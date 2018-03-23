000100000810     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      **************************************************************************
000300950118      *  Nome programma:  FNLSC2R
000400000000      *  Descrizione   :  Stampa statistica a terra
000500000000      *                   Programma esecuzione (stampa)
000600071231      *  Data creazione:  01 DIC 1993
000700000000      **************************************************************************
000800950118      *  Il pgm puo' essere lanciato dal filtro FNLSC0R oppure tramite
000900000000      *  le funzioni giornaliere, insieme ad altre stampe
001000000000      **************************************************************************
001100950210      *  ??PASSAGGIO PARAMETRI:?
001200950216      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001300950210      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001400950210      *  DS1FPG   FLAG PROGRAMMA (LASCIARE VUOTO)
001500950210      *  DS1FEL   FILIALE ELABORATORE (AREA CHE SI VUOLE STAMPARE)
001600950210      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA
001700950210      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001800950210      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
001900950210      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
002000950210      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
002100950404      *  DS1RIS   'R'=RISTAMPA, 'S'=ELABOR.+STAMPA, ' '=SOLO ELABOR.
002200950216      *  DS1FL1   STATISTICA SELEZIONATA  ( ' '=TUTTE )
002300950210      **************************************************************************
002400950210      *  N.B.: A seconda del parametro DS1RIS (stampa/ristampa) il programma
002500000000      *        segue due strade diverse per generare l'output:
002600000000      *        . se richiesta la STAMPA, ci sara' l'elaborazione dei dati con
002700950210      *          letture dai file bolle etc.; pero' le due date limite, DS1ADA
002800950210      *          e DS1AAL devono essere coincidenti;
002900000000      *        . se richiesta la RISTAMPA, non ci sara' l'elaborazione ma la
003000970211      *          semplice lettura (e stampa) del file FNSAT00F, nel quale sono
003100940505      *          gia' presenti statistiche elaborate precedentemente
003200940505      *          si puo' elaborare soltanto un giorno
003300950118      *
003400950118      *      IL FILE DI WORK SI CHIAMA:   F N W R K 0 0 F
003500950118      *      NEL CLP METTERLO IN QTEMP
003600000000      **************************************************************************
003700000000     FAZORG01L  IF   E           K DISK
003800090108     FAZCLN01L  IF   E           K DISK
003900950118     FFNBLP01L  IF   E           K DISK
004000090304     FFiart38L  IF   E           K DISK
004100020625     FFNBTP11L  IF   E           K DISK
004200020625     FFNBTT39L  IF   E           K DISK
004300020813     FFNFGV06L  IF   E           K DISK    USROPN
004400970205     FFNSAT02L  UF A E           K DISK
004500970211     FFNSAT03L  UF   E           K DISK
004600970211     F                                     RENAME(FNSAT000:FNSAT003)
004700970205     FFNSAT04L  UF   E           K DISK    USROPN
004800970205     F                                     RENAME(FNSAT000:FNSAT004)
004900950118     FFNWRK00F  UF A E           K DISK
005000000000     FTABEL00F  IF   E           K DISK
005100021016     fTntbe01l  uf   e           k Disk
005200090415     Ffista01L  uF   E           K DISK
005300970807     FFNLSC2P   O    E             PRINTER OFLIND(*IN30)
005400970807     F                                     USROPN
005500000000      *
005600000000     D L1              S              3  0 DIM(30)
005700020418     D L6              S              3  0 DIM(30)
005800950213     D LV2             S              3  0 DIM(30)
005900000616     D LIN             S              3  0 DIM(30)
006000000616     D SVLIN           S              3  0 DIM(30)
006100950210     D CMD             S              1    DIM(82) CTDATA PERRCD(80)
006200000000      * Le schiere che seguono hanno 3 elementi:
006300000000      *     1° elemento: spedizione espressa (ESPR.)
006400950627      *     2° elemento: spedizione distribuzione (DISTR)
006500950627      *     3° elemento: spedizione corriere (CORR.)
006600950627     D ENS             S              5    DIM(3) CTDATA PERRCD(3)
006700000000      * COLLI
006800970210     D CPA             S              7  0 DIM(3)
006900970210     D CBP             S              7  0 DIM(3)
007000970210     D CBR             S              7  0 DIM(3)
007100000000      * PESI
007200970210     D PPA             S              9  1 DIM(3)
007300970210     D PFI             S              9  1 DIM(3)
007400970210     D PFN             S              9  1 DIM(3)
007500970210     D PBP             S              9  1 DIM(3)
007600970210     D PBR             S              9  1 DIM(3)
007700000000      * VOLUMI
007800970210     D VPA             S              9  4 DIM(3)
007900970210     D VBP             S              9  4 DIM(3)
008000970210     D VBR             S              9  4 DIM(3)
008100000000      * SPEDIZIONI
008200970210     D SPA             S              7  0 DIM(3)
008300970210     D SBP             S              7  0 DIM(3)
008400970210     D SBR             S              7  0 DIM(3)
008500970210     D SCM             S              7  0 DIM(3)
008600970305     D FAF             S              7  0 DIM(3)
008700000000      * COLLI PER C/SERVIZIO (TOTALE AREA) -------------------------------------
008800970210AREA D C1A             S              7  0 DIM(3)
008900000000AREA D C1P             S              7  0 DIM(3)
009000000000AREA D C1R             S              7  0 DIM(3)
009100000000      * PESI PER C/SERVIZIO (TOTALE AREA)
009200970210AREA D P1A             S              9  1 DIM(3)
009300000000AREA D P1I             S              9  1 DIM(3)
009400970207AREA D P1N             S              9  1 DIM(3)
009500000000AREA D P1P             S              9  1 DIM(3)
009600000000AREA D P1R             S              9  1 DIM(3)
009700000000      * VOLUMI PER C/SERVIZIO (TOTALE AREA)
009800970210AREA D V1A             S              9  4 DIM(3)
009900000000AREA D V1P             S              9  4 DIM(3)
010000000000AREA D V1R             S              9  4 DIM(3)
010100000000      * SPEDIZIONI PER C/SERVIZIO (TOTALE AREA)
010200970210AREA D S1A             S              7  0 DIM(3)
010300000000AREA D S1P             S              7  0 DIM(3)
010400000000AREA D S1R             S              7  0 DIM(3)
010500000000AREA D S1M             S              7  0 DIM(3)
010600970305AREA D F1F             S              7  0 DIM(3)
010700000000      * COLLI PER C/SERVIZIO (PER FILIALE) -------------------------------------
010800970210FIL  D C4A             S              7  0 DIM(3)
010900000000FIL  D C4P             S              7  0 DIM(3)
011000000000FIL  D C4R             S              7  0 DIM(3)
011100000000      * PESI PER C/SERVIZIO (PER FILIALE)
011200970210FIL  D P4A             S              9  1 DIM(3)
011300000000FIL  D P4I             S              9  1 DIM(3)
011400970207FIL  D P4N             S              9  1 DIM(3)
011500000000FIL  D P4P             S              9  1 DIM(3)
011600000000FIL  D P4R             S              9  1 DIM(3)
011700000000      * VOLUMI PER C/SERVIZIO (PER FILIALE)
011800970210FIL  D V4A             S              9  4 DIM(3)
011900000000FIL  D V4P             S              9  4 DIM(3)
012000000000FIL  D V4R             S              9  4 DIM(3)
012100000000      * SPEDIZIONI PER C/SERVIZIO (PER FILIALE)
012200970210FIL  D S4A             S              7  0 DIM(3)
012300000000FIL  D S4P             S              7  0 DIM(3)
012400000000FIL  D S4R             S              7  0 DIM(3)
012500000000FIL  D S4M             S              7  0 DIM(3)
012600970305FIL  D F4F             S              7  0 DIM(3)
012700940502      *
012800940502      * COLLI PER TRANSITI (TOTALE GENERALE) -----------------------------------
012900940428GEN  D C2P             S              7  0 DIM(3)
013000000000      * PESI PER TRANSITI (TOTALE GENERALE)
013100000000GEN  D P2I             S              9  1 DIM(3)
013200940428GEN  D P2P             S              9  1 DIM(3)
013300000000      * VOLUMI PER TRANSITI (TOTALE GENERALE)
013400940428GEN  D V2P             S              9  4 DIM(3)
013500000000      * SPEDIZIONI PER TRANSITI (TOTALE GENERALE)
013600940428GEN  D S2P             S              7  0 DIM(3)
013700000000GEN  D S2M             S              7  0 DIM(3)
013800940429      * COLLI PER TOTALE AREA --------------------------------------------------
013900970211     D C3A             S              7  0 DIM(3)
014000940428     D C3P             S              7  0 DIM(3)
014100940428      * PESI PER TOTALE GENERALE
014200970211     D P3A             S              9  1 DIM(3)
014300940428     D P3I             S              9  1 DIM(3)
014400940428     D P3P             S              9  1 DIM(3)
014500940428      * VOLUMI PER TOTALE GENERALE
014600970211     D V3A             S              9  4 DIM(3)
014700940428     D V3P             S              9  4 DIM(3)
014800000331     D V3R             S              9  4 DIM(3)
014900940428      * SPEDIZIONI PER TOTALE GENERALE
015000970211     D S3A             S              7  0 DIM(3)
015100940428     D S3P             S              7  0 DIM(3)
015200940428     D S3R             S              7  0 DIM(3)
015300940428     D S3M             S              7  0 DIM(3)
015400970305     D F3F             S              7  0 DIM(3)
015500970207      * COLLI PER FILE FNSAT00F ------------------------------------------------
015600970207      * PESI PER FILE FNSAT00F
015700970207      * VOLUMI PER FILE FNSAT00F
015800970207      * SPEDIZIONI PER FILE FNSAT00F
015900000000      *
016000940504     D GME             S              1    DIM(31)
016100090108     D GMEP            S              1    DIM(31)
016200020418
016300020418     d sav_t06         S              3  0 dim(30)
016400020418
016500020418     d wc              s              3  0
016600021016
016700021016     d kTbeCod         s                   Like(TbeCod)
016800021016     d kTbeKe1         s                   Like(TbeKe1)
016900020418
017000040921     D WLBDAT          DS                  INZ
017100040921     D  G02DAT                 1      8  0
017200040921     D  G02INV                 9     16  0
017300040921     D  G02ERR                17     17
017400040921     D  G02TGI                18     22  0
017500000000      *
017600000810     D OG143         E DS
017700000810     D DS3A          E DS
017800000000     D DS3L          E DS
017900970707     D DS5P          E DS
018000000000     D KPJBA         E DS
018100000000     D CNCR80        E DS
018200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
018300950210     D* DS PER TRUL06R - CARICAMENTO £X
018400950210     D DSUL06        E DS                  EXTNAME(TRUL06DS)
018500950314     D  T06                    1     90  0
018600950314     D                                     DIM(30)
018700970303      * DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
018800970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
018900950210     D DSLS01        E DS
019000020813      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
019100020813     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
019200070522     D* DS PER LETTURA TABELLA "VPO" record DECO
019300070522     D Dvpodeco      E DS
019400021016      * Ds date statistiche
019500021016     d dSdf          e ds
019600020813      *
019700950210     D                 DS
019800950210     D  DATADA                 1      8  0
019900950210     D  ANNODA                 1      4  0
020000950210     D  MESEDA                 5      6  0
020100950210     D  $GI                    7      8  0
020200090304      * DS per controllare il "precedente" di FNart
020300090304     D $artAT          DS
020400090304     D  artLNP                 1      3  0
020500090304     D  artAAS                 4      7  0
020600090304     D  artNRS                 8      9  0
020700090304     D  artNSP                10     16  0
020800950118      * DS per controllare il "precedente" di FNBTT
020900950118     D $BTTAT          DS
021000950118     D  BTTLNP                 1      3  0
021100950118     D  BTTAAS                 4      7  0
021200950118     D  BTTNRS                 8      9  0
021300950118     D  BTTNSP                10     16  0
021400970207      * DS per i campi del file statistiche FNSAT00F
021500000000     D                 DS
021600000000     D  C0P                    1     21  0
021700970207     D                                     DIM(3)
021800970207     D  SATCDE                 1      7  0
021900970207     D  SATCDD                 8     14  0
022000970207     D  SATCDC                15     21  0
022100000000     D                 DS
022200000000     D  C0R                    1     21  0
022300970207     D                                     DIM(3)
022400970207     D  SATCNE                 1      7  0
022500970207     D  SATCND                 8     14  0
022600970207     D  SATCNC                15     21  0
022700000000     D                 DS
022800000000     D  P0I                    1     27  1
022900970207     D                                     DIM(3)
023000970207     D  SATFDE                 1      9  1
023100970207     D  SATFDD                10     18  1
023200970207     D  SATFDC                19     27  1
023300970207     D                 DS
023400970207     D  P0N                    1     27  1
023500970207     D                                     DIM(3)
023600970207     D  SATFNE                 1      9  1
023700970207     D  SATFND                10     18  1
023800970207     D  SATFNC                19     27  1
023900000000     D                 DS
024000000000     D  P0P                    1     27  1
024100970210     D                                     DIM(3)
024200970207     D  SATODE                 1      9  1
024300970207     D  SATODD                10     18  1
024400970207     D  SATODC                19     27  1
024500000000     D                 DS
024600000000     D  P0R                    1     27  1
024700970207     D                                     DIM(3)
024800970207     D  SATONE                 1      9  1
024900970207     D  SATOND                10     18  1
025000970207     D  SATONC                19     27  1
025100000000     D                 DS
025200000000     D  V0P                    1     27  4
025300970207     D                                     DIM(3)
025400970207     D  SATVDE                 1      9  4
025500970207     D  SATVDD                10     18  4
025600970207     D  SATVDC                19     27  4
025700000000     D                 DS
025800000000     D  V0R                    1     27  4
025900970207     D                                     DIM(3)
026000970207     D  SATVNE                 1      9  4
026100970207     D  SATVND                10     18  4
026200970207     D  SATVNC                19     27  4
026300000000     D                 DS
026400000000     D  S0P                    1     21  0
026500970207     D                                     DIM(3)
026600970207     D  SATSDE                 1      7  0
026700970207     D  SATSDD                 8     14  0
026800970207     D  SATSDC                15     21  0
026900000000     D                 DS
027000000000     D  S0R                    1     21  0
027100970207     D                                     DIM(3)
027200970207     D  SATSNE                 1      7  0
027300970207     D  SATSND                 8     14  0
027400970207     D  SATSNC                15     21  0
027500970305     D                 DS
027600970305     D  F0F                    1     21  0
027700970305     D                                     DIM(3)
027800970305     D  SATSFE                 1      7  0
027900970305     D  SATSFD                 8     14  0
028000970305     D  SATSFC                15     21  0
028100000000     D                 DS
028200000000     D  S0M                    1     21  0
028300000000     D                                     DIM(3)
028400000000     D  SATSCE                 1      7  0
028500970207     D  SATSCD                 8     14  0
028600970207     D  SATSCC                15     21  0
028700970210     D                 DS
028800970210     D  CDT                    1     21  0
028900970210     D                                     DIM(3)
029000970210     D  SATCPE                 1      7  0
029100970210     D  SATCPD                 8     14  0
029200970211     D  SATCPC                15     21  0
029300970210     D                 DS
029400970210     D  PDT                    1     27  1
029500970210     D                                     DIM(3)
029600970210     D  SATPPE                 1      9  1
029700970210     D  SATPPD                10     18  1
029800970210     D  SATPPC                19     27  1
029900970210     D                 DS
030000970210     D  VDT                    1     27  4
030100970210     D                                     DIM(3)
030200970210     D  SATVPE                 1      9  4
030300970210     D  SATVPD                10     18  4
030400970210     D  SATVPC                19     27  4
030500970210     D                 DS
030600970210     D  SDT                    1     21  0
030700970210     D                                     DIM(3)
030800970210     D  SATSPE                 1      7  0
030900970210     D  SATSPD                 8     14  0
031000970210     D  SATSPC                15     21  0
031100020813      *
031200020813      * Varibaili
031300020813      *
031400020813     D SAVRIS          S                   LIKE(DS1RIS) inz
031500020813     D $CDA            S              7  0              inz
031600020813     D $VDA            S              9  4              inz
031700020813     D $PDA            S              9  1              inz
031800020813     D $SDA            S              7  0              inz
031900020813     D $TFA            S                   LIKE(BLPTFA) inz
032000020813     D $AREA           S                   LIKE(DS1FEL) inz
032100020813     D $LNP            S                   LIKE(DS1LNP) inz
032200090304     D $artPR          S                   LIKE($artAT) inz
032300020813     D $FILPR          S                   LIKE(BLPLNP) inz
032400020813     D $CBLPR          S                   LIKE(BLPCBO) inz
032500020813     D $NFVPR          S                   LIKE(BLPNFV) inz
032600020813     D $DTRIT          S                   LIKE(BLPDRT) inz
032700020813     D $CDPDR          S                   LIKE(BLPPDR) inz
032800020813     D $FLAG           S                   LIKE(WRKFLG) inz
032900020813     D $FLAG2          S                   LIKE(WRKFLG) inz
033000020813     D $DFV            S                   LIKE(FGVDFV) inz
033100020813     D $FTR            S                   LIKE(SATFTR) inz
033200020813     D $DTR            S                   LIKE(SATDTR) inz
033300020813     D $TRE            S                   LIKE(SATTRE) inz
033400020813     D $COMOD          S             15  6 INZ
033500020813     D $COMO1          S             15  6 INZ
033600020813     D*
033700020813     D KCOD            S                   LIKE(TBLCOD) inz
033800020813     D KKEY            S                   LIKE(TBLKEY) inz
033900021217      *
034000021217     d $Volum          s             12  4 inz
034100021217     d $Peso           s              8  1 inz
034200030213     d*** WsiVOL          s              1    inz
034300030213     d*** WsiPES          s              1    inz
034400021217     d Wvolu           s                   like(BLPvlf) inz
034500021217     d Wpeso           s                   like(BLPpkf) inz
034600021220     d Wvolmed         s                   like($Volum) inz
034700021220     d Wpesmed         s                   like($Peso)  inz
034800090415     D ktrc            S                   LIKE(statrc) inz('T')
034900090415     D kfil            S                   LIKE(stafil) inz(000)
035000940502      **************************************************************************
035100940502      *      RIEPILOGO INDICATORI
035200940502      **************************************************************************
035300940502      * 01    - ON: LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE = 0
035400940502      *       - OFF:LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE > D.ELAB
035500940502      * 02    - ON: TOTALE GENERALE
035600940502      *       - OFF:TOTALE DI AREA
035700970226      * 03    - STAMPA DEI TOTALI TRANSITI
035800970226      * 04    - NON MEMORIZZA $ALMEN NELLA ROURINE DI TOTGEN
035900940502      * 05    - STAMPE GIORNALIERE
036000970205      * 27    - OFF: DEVO STAMPARE LA RIGA DEI TRANASITI
036100940502      * 29    - ON: IMMESSA UNA LINEA DI PARTENZA
036200940502      * 30    - ECCEDENZA PAGINA
036300940502      * 33    - LETTURA FILES
036400940502      * 34    - BOLLA IN C/SERVIZIO
036500940502      * 35    - ON: BOLLA BOLLETTATA NEL PERIODO
036600940502      *       - OFF:BOLLA BOLLETTATA NEL PERIODO PRECEDENTE
036700940502      * 36    - COLLO A TERRA
036800970211      * 37    - LETTURA SUL FILES FNSAT
036900950210      * 38    - DS1RIS= ' ' --> NON DEVO STAMPARE
037000940502      * 39    - ON: IMMESSO CODICE AREA
037100940502      * 40    - ON: STAMPA LA LILIALE CON DECODIFICA
037200940502      *       - OFF:STAMPA LA DICITURA DEL SERVIZIO BOLLE
037300940502      * 41    - ON: STAMPA SOLTANTO LA FILIALE (+ 40 ON)
037400940502      * 43    - ON: STAMPO LA PARTE RELATIVA ALLE LINEE DI PARTENZA
037500940502      *       - OFF:STAMPO LA PARTE RELATIVA AI TERMINAL DI ARRIVO
037600970211      * 45    - STAMPA LE PERCENTUALI SUL DA PARTIRE
037700950118      * 99    - DI COMODO
037800940502      **************************************************************************
037900000000     C     *ENTRY        PLIST
038000000000     C                   PARM                    KPJBA
038100950210     C                   MOVEL     KPJBU         DSLS01
038200950210      *
038300950210      * DATA FINALE AA/MM/GG (6 CARATTERI)
038400090108     C                   Z-ADD     DS1aal        DATADA
038500000000      *
038600090304     C     FNartK        KLIST
038700940428     C                   KFLD                    LIN($X)
038800940428     C                   KFLD                    $DFV
038900970205     C     FNBLPK        KLIST
039000090304     C                   KFLD                    artAAS
039100090304     C                   KFLD                    artLNP
039200090304     C                   KFLD                    artNRS
039300090304     C                   KFLD                    artNSP
039400090415     c     KSTA          Klist
039500090415     c                   Kfld                    Ktrc
039600090415     c                   Kfld                    DS1FEL
039700090415     c                   Kfld                    Kfil
039800090415     c                   Kfld                    ds1ada
039900950118     C     FNBTPK        KLIST
040000020625     c                   kfld                    bttflp
040100950118     C                   KFLD                    BTTAAS
040200950118     C                   KFLD                    BTTLNP
040300950118     C                   KFLD                    BTTNRS
040400950118     C                   KFLD                    BTTNSP
040500950118     C     AZCLNK        KLIST
040600940504     C                   KFLD                    SIMFEL
040700940504     C                   KFLD                    $TFA
040800940504     C                   KFLD                    ANNODA
040900940504     C                   KFLD                    MESEDA
041000950118     C     FNWRKK        KLIST
041100000000     C                   KFLD                    $FLAG
041200000000     C                   KFLD                    $FILPR
041300950118     C     FNWR1K        KLIST
041400000000     C                   KFLD                    $FLAG
041500000000     C                   KFLD                    WRKLNP
041600940503     C                   KFLD                    BLPAAS
041700940503     C                   KFLD                    BLPNRS
041800940503     C                   KFLD                    BLPNSP
041900000000     C                   KFLD                    WRKCSE
042000970409     C     FNWR2K        KLIST
042100970409     C                   KFLD                    $FLAG
042200970409     C                   KFLD                    WRKLNP
042300970409     C                   KFLD                    BTPAAS
042400970409     C                   KFLD                    BTPNRS
042500970409     C                   KFLD                    BTPNSP
042600970409     C                   KFLD                    WRKCSE
042700970205     C     FNSATK        KLIST
042800970205     C                   KFLD                    SIMFEL
042900970205     C                   KFLD                    LIN(XX)
043000970205     C                   KFLD                    DS1AAL
043100970205     C     FNSA1K        KLIST
043200000000     C                   KFLD                    SATFLE
043300000000     C                   KFLD                    SATTFA
043400940429     C                   KFLD                    SATLNP
043500000000     C                   KFLD                    SATDRE
043600000000     C                   KFLD                    SATTRE
043700970205     C     DTSATK        KLIST
043800940429     C                   KFLD                    $AREA
043900940429     C                   KFLD                    $TFA
044000940429     C                   KFLD                    $LNP
044100970211     C                   KFLD                    DS1ADA
044200970205     C     LPSATK        KLIST
044300940429     C                   KFLD                    $AREA
044400940429     C                   KFLD                    $TFA
044500940429     C                   KFLD                    $LNP
044600970205     C     ARSATK        KLIST
044700940429     C                   KFLD                    $AREA
044800940429     C                   KFLD                    $TFA
044900970211     C     TASATK        KLIST
045000940429     C                   KFLD                    $AREA
045100940429     C                   KFLD                    $TFA
045200970211     C                   KFLD                    DS1ADA
045300970211     C     TRSATK        KLIST
045400940503     C                   KFLD                    $AREA
045500940503     C                   KFLD                    $TFA
045600970211     C                   KFLD                    DS1ADA
045700940503     C                   KFLD                    $TRE
045800020625     c     kbtt39        klist
045900020625     c                   kfld                    simfel
046000020625     c                   kfld                    $dfv
046100021016
046200021016     c     Ktbe          Klist
046300021016     c                   Kfld                    kTbeCod
046400021016     c                   Kfld                    kTbeKe1
046500000000      *
046600021016     C                   Z-ADD     *ZERO         $COMO1           15 6
046700021016     C                   Z-ADD     1             CODUT
046800000000      **************************************************************************
046900000000      *              M A I N      L I N E
047000000000      **************************************************************************
047100940429     C                   MOVE      'RISTAMPA'    STRIP
047200940429     C*
047300950210    1C     DS1RIS        IFNE      'R'
047400940429     C                   MOVE      'STAMPA  '    STRIP
047500940502    1C                   ENDIF
047600070522     c* Carico la tabella VPO per vedere da quando cambia il calcolo
047700070522     c*  della affidabilità partenza
047800070522     C                   MOVEL     'VPO'         kTBECOD
047900070522     C                   MOVEL(P)  'DECO'        kTBEKE1
048000070522     C     KTBE          CHAIN(N)  TNTBE01L                           21
048100070522     C  N21              MOVEL     TBEUNI        DVPODECO
048200070522     C*
048300070522     c
048400000000      *
048500000000      * Se la data AL e' vuota la imposto come la data DAL
048600950210     C     DS1AAL        IFEQ      *ZERO
048700950210     C                   Z-ADD     DS1ADA        DS1AAL
048800950210     C                   Z-ADD     DS1GDA        DS1GAL
048900000000     C                   ENDIF
049000940428     C*
049100940428     C                   SETON                                        43
049200940401      *
049300950210      * Accendo un indicatore se non devo stampare il tabulato (DS1RIS=' ')
049400950210     C     DS1RIS        COMP      *BLANKS                                38
049500940429      *
049600940429      * 39 ON - Immessa l'area
049700950210    3C     DS1FEL        IFGT      0
049800940429     C                   SETON                                        39
049900940429    3C                   ENDIF
050000000000      *
050100950210      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
050200950210     C                   EXSR      COMUNE
050300950210      *
050400000000     C                   SETON                                        LR
050500020212     C*****************************************************************
050600020212     C*  PARTE COMUNE STAMPE GIORNALIERE
050700020212     C*
050800020212     C*  . REPERIMENTO 'SIMFEL'
050900020212     C*  . CARICAMENTO '£1'
051000020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
051100020212     C*  . CODA DI STAMPA ('£OUTQ')
051200020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
051300020212     C*  . MODULO PER LA STAMPA ('£MOD')
051400020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
051500020212     C*****************************************************************
051600020212     C     COMUNE        BEGSR
051700020212     C*
051800020212     C*****************************************************************
051900020212     C*       RIEPILOGO INDICATORI:
052000020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
052100020212     C* 11    -           STAMPA PER 1L
052200020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
052300020212     C*****************************************************************
052400020212     C                   Z-ADD     1             CODUT
052500020212     C*
052600020212     C* Reperimento parametri aziendali
052700020212     C                   CALL      'X§PARUT'
052800020212     C                   PARM                    UT§DSE
052900020212     C                   MOVEL     RAGUT         RSUT             20
053000020212     C                   MOVEL     REC80         CNCR80
053100020212     C*
053200020212     C* Chiave di accesso per il file tabelle
053300020212     C     KTAB1         KLIST
053400020212     C                   KFLD                    CODUT
053500020212     C                   KFLD                    KCOD
053600020212     C                   KFLD                    KKEY
053700020212     C     KTAB2         KLIST
053800020212     C                   KFLD                    CODUT
053900020212     C                   KFLD                    KCOD
054000020212     C*
054100020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
054200020212     C*
054300020212     C* DETERMINO DATA E ORA DI ESECUZIONE
054400020212     C                   TIME                    WTIME            14 0
054500020212     C                   MOVE      WTIME         WDATE             8 0
054600020212     C                   MOVEL     WTIME         UTIME             6 0
054700040921     c*
054800040921     C                   CLEAR                   WLBDAT
054900040921     C                   MOVE      Wtime         G02DAT
055000040921     C                   CALL      'XSRDA8'
055100040921     C                   PARM                    WLBDAT
055200040921     C                   Z-ADD     G02INV        dateu             8 0
055300040922     c*
055400040922     c                   clear                   $FTR
055500040922     c                   z-add     dateu         $DTR
055600020212     C*
055700020418     C* CARICO '£1'
055800020212     C                   CLEAR                   KPJBU
055900020212     C                   CLEAR                   DSUL06
056000020212     C                   MOVE      '£1'          D06COD
056100020212     C                   MOVEL     SIMFEL        D06KEY
056200050802     C                   MOVE      'S'           D06KEY
056300050802     C                   MOVEL     ds1ada        D06drf
056400020212     C                   MOVEL     DSUL06        KPJBU
056500020212     C                   CALL      'TRUL06R'
056600020212     C                   PARM                    KPJBA
056700020212     C                   MOVEL     KPJBU         DSUL06
056800020418     c                   movea     t06           sav_t06
056900020418
057000020418      * se in arrivo carico la £6
057100020418 b1  c                   if        ds1tbo = 'A'
057200020418     c                   clear                   wc
057300020418 b2  c                   do        30            wa
057400020422     c                   if        sav_t06(wa) = *zeros
057500020422     c                   leave
057600020422     c                   endif
057700020418     c                   clear                   kpjbu
057800020418     c                   clear                   dsul06
057900020418     c                   eval      d06cod = '£6'
058000020418     c                   movel     sav_t06(wa)   d06key
058100050802     C                   MOVE      'S'           D06KEY
058200020418     c                   eval      d06esc = 'S'
058300050802     c                   movel     ds1ada        d06drf
058400020419     c                   movel     dsul06        kpjbu
058500020418     c                   call      'TRUL06R'
058600020418     c                   parm                    kpjba
058700020418     c                   movel     kpjbu         dsul06
058800020418 b3  c                   do        30            wb
058900020418     c                   if        t06(wb) = *zeros
059000020418     c                   leave
059100020418     c                   endif
059200020418     c                   add       1             wc
059300020418     c                   z-add     t06(wb)       l6(wc)
059400020418 e3  c                   enddo
059500020418 e2  c                   enddo
059600020212     C*
059700020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
059800020212     C                   Z-ADD     *ZERO         WB
059900020418 b2  C                   DO        29            WA
060000020418     C     sav_T06(WA)   LOOKUP    L6                                     30
060100020212     C  N30              ADD       1             WB
060200020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
060300020418 e2  C                   ENDDO
060400020418 x1  C                   ELSE
060500020418     C                   MOVEA     sav_T06       L1
060600020418 e1  C                   ENDIF
060700020212     C*
060800020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
060900020212     C                   MOVEA     L1            LIN
061000020212     C                   CLEAR                   L1
061100020212     C                   Z-ADD     *ZERO         WB                3 0
061200020212     C     1             DO        30            WPT               3 0
061300020212     C     LIN(WPT)      IFNE      0
061400020212     C                   CLEAR                   FNLSC4
061500020212     C                   Z-ADD     LIN(WPT)      IC4LIN
061600020212     C                   MOVE      'B'           IC4FLG
061700020212     C                   MOVEL     FNLSC4        KPJBU
061800020212     C                   CALL      'FNLSC4R1'
061900020212     C                   PARM                    KPJBA
062000050802     C                   PARM                    ds1ada
062100020212     C                   MOVEL     KPJBU         FNLSC4
062200020212     C     OC4ESE        IFEQ      'S'
062300020212     C                   ADD       1             WB
062400020212     C                   MOVE      LIN(WPT)      L1(WB)
062500020212     C                   ENDIF
062600020212     C                   ENDIF
062700020212     C                   ENDDO
062800070103     C*
062900070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
063000070103     C                   Z-ADD     *ZERO         WB                3 0
063100070103     C                   DO        29            WA                3 0
063200070103     C     L1(WA)        IFNE      *ZEROS
063300070103     C     L1(WA)        ANDNE     SIMFEL
063400070103     C                   ADD       1             WB
063500070103     C                   Z-ADD     L1(WA)        LV2(WB)
063600070103     C                   ENDIF
063700070103     C                   ENDDO
063800020212     C*
063900020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
064000020212     C*
064100020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
064200020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
064300020212     C* la stampa e' lanciata da menu
064400020212     C                   MOVEL     '3L'          KCOD
064500020212     C                   MOVEL     DS1PGM        KKEY
064600020212     C                   MOVE      DS1FPG        KKEY
064700020212     C     KTAB1         CHAIN     TABEL00F                           30
064800020212     C  N30              MOVEL     TBLUNI        DS3L
064900020212     C   30              MOVEL     'F'           §3LD1L
065000020212     C   30              MOVEL     ' '           §3LD2L
065100020212     C   30              MOVEL     '*STD    '    §3LDST
065200020212     C*
065300020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
065400020212     C                   MOVEL     §3LDST        £MOD             10
065500020212     C                   CLEAR                   LIN
065600020212     C*
065700020212     C* Verifico chi ha lanciato la stampa
065800020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
065900020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
066000020212     C* DS1FGS<>0  --->  si proviene da menu
066100020212     C     DS1FGS        IFEQ      0
066200020212     C*
066300020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
066400020212     C* lancio, voglio la stampa;  invece
066500020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
066600020212     C*
066700020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
066800020212     C* creare stampa per 1°lvl. (se stessa)
066900020212     C     DS1LNP        IFEQ      SIMFEL
067000020212     C*
067100020212     C* Il flag §3LD1L ha i seguenti significati:
067200020212     C*    ' ' = tutti i dati
067300020212     C*    '1' = solo filiale di 1' livello
067400020212     C     §3LD1L        IFEQ      '1'
067500020212     C                   SETON                                        11
067600020212     C                   END
067700020212     C*
067800020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
067900020212     C* nella stampa
068000020212     C*
068100020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
068200020212     C  N11              MOVEA     L1            LIN
068300020212     C   11              MOVEA     SIMFEL        LIN(1)
068400020212     C*
068500020212     C* IMPOSTO LA STAMPANTE:
068600020212     C* STAMPANTE TABULATI
068700020212     C                   MOVEL     '5P'          KCOD
068800020212     C                   MOVEL     'T'           W004A             4
068900020212     C                   MOVE      LIN(1)        W004A
069000020212     C                   MOVEL(P)  W004A         KKEY
069100020212     C     KTAB1         CHAIN     TABEL00F                           99
069200020212     C     *IN99         IFEQ      *OFF
069300020212     C                   MOVEL     TBLUNI        DS5P
069400020212     C                   MOVEL     §5POTQ        £OUTQ            10
069500020212     C                   END
069600020212     C*
069700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
069800020212     C                   EXSR      LEGGI
069900020212     C*
070000020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
070100020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
070200020212     C                   ELSE
070300020212     C*
070400020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
070500020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
070600020212     C*
070700020212     C* Il significato del fla §3LD2L e' il seguente:
070800020212     C*    ' ' = tutti i dati
070900020212     C*    'F' = per ogni 2' livello
071000020212     C     §3LD2L        IFEQ      'F'
071100020212     C                   SETON                                        14
071200020212     C                   ENDIF
071300020212     C*
071400020212     C                   Z-ADD     1             WB
071500020212     C                   MOVEA     *ZEROS        LIN
071600020212     C  N14              MOVEA     L1            LIN
071700020212     C*
071800020212     C* Lancio per tutti i 2° lvl
071900020212     C     DS1LNP        IFEQ      0
072000020212     C*
072100020212     C* LV2 contiene tutte le filiali di 2° lvl
072200020212     C     LV2(WB)       DOWNE     0
072300020212      *
072400020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
072500020212     C                   CLEAR                   FNLSC4
072600020212     C                   Z-ADD     LV2(WB)       IC4LIN
072700020212     C                   MOVEL     FNLSC4        KPJBU
072800020212     C                   CALL      'FNLSC4R1'
072900020212     C                   PARM                    KPJBA
073000050802     C                   PARM                    ds1ada
073100020212     C                   MOVEL     KPJBU         FNLSC4
073200020212      *
073300020212     C     OC4PT1        IFEQ      'S'
073400020212     C     §3LPT         ANDEQ     'N'
073500020212     C                   GOTO      NOLEGG
073600020212     C                   ENDIF
073700020212      *
073800020212     C     OC4EXP        IFEQ      'S'
073900020212     C     §3LEXP        ANDEQ     'N'
074000020212     C                   GOTO      NOLEGG
074100020212     C                   ENDIF
074200020212      *
074300020212     C     OC4DPD        IFEQ      'S'
074400020212     C     §3LDPD        ANDEQ     'N'
074500020212     C                   GOTO      NOLEGG
074600020212     C                   ENDIF
074700020212      *
074800020212     C     OC4BAR        IFEQ      'S'
074900020212     C     §3LBAR        ANDEQ     'N'
075000020212     C                   GOTO      NOLEGG
075100020212     C                   ENDIF
075200020212      *
075300020212     C     OC4FED        IFEQ      'S'
075400020212     C     §3LFED        ANDEQ     'N'
075500020212     C                   GOTO      NOLEGG
075600020212     C                   ENDIF
075700020212      *
075800020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
075900020212     C*  dati degli altri 2° lvl
076000020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
076100020212     C   14              MOVEL     LV2(WB)       LIN(1)
076200020212     C*
076300020212     C* IMPOSTO LA STAMPANTE
076400020212     C* STAMPANTE TABULATI
076500020212     C                   MOVEL     '5P'          KCOD
076600020212     C                   MOVEL     'T'           W004A             4
076700020212     C                   MOVE      LV2(WB)       W004A
076800020212     C                   MOVEL(P)  W004A         KKEY
076900020212     C     KTAB1         CHAIN     TABEL00F                           99
077000020212     C     *IN99         IFEQ      *OFF
077100020212     C                   MOVEL     TBLUNI        DS5P
077200020212     C                   MOVEL     §5POTQ        £OUTQ            10
077300020212     C                   END
077400020212     C*
077500020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
077600020212     C                   EXSR      LEGGI
077700020212     C*
077800020212     C     NOLEGG        TAG
077900020212     C*
078000020212     C                   ADD       1             WB
078100020212     C                   ENDDO
078200020212     C*
078300020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
078400020212     C                   ELSE
078500020212     C*
078600020212     C* Lancio solo per la 2° lvl richiesta
078700020212     C   14              MOVEL     DS1LNP        LIN(1)
078800020212     C*
078900020212     C* STAMPANTE
079000020212     C* STAMPANTE TABULATI
079100020212     C                   MOVEL     '5P'          KCOD
079200020212     C                   MOVEL     'T'           W004A             4
079300020212     C                   MOVE      DS1LNP        W004A
079400020212     C                   MOVEL(P)  W004A         KKEY
079500020212     C     KTAB1         CHAIN     TABEL00F                           99
079600020212     C     *IN99         IFEQ      *OFF
079700020212     C                   MOVEL     TBLUNI        DS5P
079800020212     C                   MOVEL     §5POTQ        £OUTQ            10
079900020212     C                   END
080000020212     C*
080100020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
080200020212     C                   EXSR      LEGGI
080300020212     C*
080400020212     C                   ENDIF
080500020212     C                   ENDIF
080600020212     C*
080700020212     C                   ELSE
080800020212     C*
080900020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
081000020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
081100020212     C     DS1LNP        IFEQ      0
081200020212     C                   MOVEA     L1            LIN
081300020212     C* ...altrimenti elaboro solo se stessa
081400020212     C                   ELSE
081500020212     C                   MOVEL     DS1LNP        LIN(1)
081600020212     C                   ENDIF
081700020212     C*
081800020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
081900020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
082000020212     C* PRENDO QUELLA PRESENTE IN KPJBA
082100020212     C     DS1FGS        IFEQ      SIMFEL
082200020212     C                   MOVEL     KQOTD         £OUTQ
082300030627     C**!!!              MOVEL     KQOTD         £OUTQF
082400020212     C                   ELSE
082500020212     C* STAMPANTE TABULATI
082600020212     C                   MOVEL     '5P'          KCOD
082700020212     C                   MOVEL     'T'           W004A             4
082800020212     C                   MOVE      DS1FGS        W004A
082900020212     C                   MOVEL(P)  W004A         KKEY
083000020212     C     KTAB1         CHAIN     TABEL00F                           99
083100020212     C     *IN99         IFEQ      *OFF
083200020212     C                   MOVEL     TBLUNI        DS5P
083300020212     C                   MOVEL     §5POTQ        £OUTQ            10
083400020212     C                   END
083500020212     C                   ENDIF
083600020212     C*
083700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
083800020212     C                   EXSR      LEGGI
083900020212     C                   ENDIF
084000020212     C*
084100020212     C                   ENDSR
084200000000      **************************************************************************
084300000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
084400000000      **************************************************************************
084500000000     C     LEGGI         BEGSR
084600940502     C                   SETOFF                                       2729
084700000619     C                   SETON                                        43
084800000616     C                   CLEAR                   SAVRIS
084900970212     C*
085000970212      * Cancello dal file statistiche i record della giornata che vado a elabor.
085100970212      * Solo se richiesta elaborazione
085200970212    1C     DS1RIS        IFNE      'R'
085300000616     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
085400000616     C                   CLEAR                   KPJBU
085500000616     C                   CLEAR                   DSUL06
085600000616     C                   MOVE      '£1'          D06COD
085700000616     C                   MOVEL     SIMFEL        D06KEY
085800050802     C                   MOVE      'S'           D06KEY
085900050802     c                   movel     ds1ada        d06drf
086000000616     C                   MOVEL     DSUL06        KPJBU
086100000616     C                   CALL      'TRUL06R'
086200000616     C                   PARM                    KPJBA
086300000616     C                   MOVEL     KPJBU         DSUL06
086400000616     C**
086500000616     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
086600000616     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
086700000616     C*  CHE USO ALLA FINE
086800000616     C     DS1RIS        IFNE      ' '
086900000616     C                   MOVEA     LIN           SVLIN
087000000616     C                   MOVEL     DS1RIS        SAVRIS
087100000616     C                   CLEAR                   DS1RIS
087200000616     C                   ENDIF
087300000616     C*
087400000616     C                   MOVEA     T06           LIN
087500000616     C*
087600970212     C                   OPEN      FNSAT04L
087700970212     C                   Z-ADD     1             XX                3 0
087800970212    2C     LIN(XX)       DOWGT     *ZERO
087900970212     C*
088000970212     C     FNSATK        SETLL     FNSAT04L
088100970212     C     FNSATK        READE     FNSAT04L                               99
088200970212     C*
088300970212    3C     *IN99         DOWEQ     *OFF
088400970212     C                   DELETE    FNSAT004
088500970212     C     FNSATK        READE     FNSAT04L                               99
088600970212    3C                   ENDDO
088700970212     C                   ADD       1             XX
088800970212    2C                   ENDDO
088900970212     C*
089000970212     C                   CLOSE     FNSAT04L
089100970212     C                   END
089200940505      *
089300940505      * Come prima cosa devo verificare se giornata festiva
089400940505      *   solo per ELABORAZIONE o  per stampe giornaliere
089500950210     C     DS1FGS        IFEQ      0
089600950210     C     DS1RIS        ORNE      'R'
089700940505     C                   CLEAR                   $TFA
089800940505      *
089900950118     C     AZCLNK        CHAIN     AZCLN01L                           99
090000940505     C     *IN99         IFEQ      *OFF
090100950118     C                   MOVEA     CLNMAT        GME
090200970211     C     GME($GI)      IFEQ      'F'
090300970211     C* SE FESTIVO SCRIVO RECORD IN FNSAT CON TRE = F
090400970211     C                   CLEAR                   FNSAT000
090500970211     C                   Z-ADD     SIMFEL        SATFLE
090600970211     C                   MOVEL     'F'           SATTRE
090700970211     C                   Z-ADD     DS1ADA        SATDRE
090800040922     c                   movel     $ftr          satftr
090900040922     c                   z-add     $DTR          satdtr
091000970311     C                   WRITE     FNSAT000                             99
091100970211     C                   GOTO      ENDLEG
091200970211     C                   ENDIF
091300940505     C                   ENDIF
091400940505     C                   ENDIF
091500940429      *
091600940429     C* LANCIO DA FILTRO
091700950210    1C     DS1FGS        IFGT      0
091800940429     C* 29 ON - IMMESSA LINEA DI PARTENZA
091900950210    2C     DS1LNP        IFGT      0
092000940429     C                   SETON                                        29
092100950210    3C     DS1LNP        IFNE      DS1FEL
092200940502     C                   SETON                                        27
092300940429    3C                   ENDIF
092400940429    2C                   ENDIF
092500940429     C*
092600940429   X1C                   ELSE
092700940502     C                   SETON                                        05
092800940506     C                   CLEAR                   STRIP
092900940429     C* STAMPE GIORNALIERE
093000940429    2C     LIN(2)        IFEQ      0
093100940429     C                   SETON                                        29
093200940429    2C                   ENDIF
093300940429     C     SIMFEL        LOOKUP    LIN                                    99
093400940502     C  N99              SETON                                        27
093500940429    1C                   ENDIF
093600000912     C* SE SONO TUTTI P.O. POSTE, NON ENTRO NEMMENO NEL CICLO DI LETT
093700000912     C*   E RISTAMPA
093800000912     C                   Z-ADD     1             $X
093900000912     C                   CLEAR                   WSTAF
094000000912    1C     LIN($X)       DOWGT     0
094100000912     C     WSTAF         ANDEQ     ' '
094200000912     C                   Z-ADD     LIN($X)       $FILPR
094300000912     C* ESCLUDO I P.O. POSTE
094400020813     ***C                   CLEAR                   §OGPT
094500020813     C                   clear                   OG143
094600000912     C     $FILPR        CHAIN     AZORG01L                           82
094700000912     C  N82              MOVEL     ORGDE3        OG143
094800000912     C**
094900020813    2***C     §OGPT         IFNE      'S'
095000020813    2C                   IF        §OGntw <> 'PPT'
095100000912     C* SE ELABORATA ALMENO UNA LINEA, STAMPO ANCHE I TOTALI FINALI
095200000912     C                   MOVEL     'S'           WSTAF             1
095300000912    2C                   ENDIF
095400000912     C                   ADD       1             $X
095500000912    1C                   ENDDO
095600000000      *
095700000000      *
095800000912    1C     WSTAF         IFEQ      'S'
095900000000      * Scrivo, in stampa, se selezionata filiale o area
096000000000     C                   MOVE      *BLANKS       STRIP1
096100950210     C  N05DS1LNP        IFNE      *ZERO
096200960925     C                   MOVEL     'P.O.'        STRIP1
096300950210     C                   MOVE      DS1LNP        STRIP1
096400000000     C                   ELSE
096500940502     C                   MOVEL     'AREA'        STRIP1
096600950210     C                   MOVE      DS1FEL        STRIP1
096700000000     C                   ENDIF
096800000000      *
096900000000      * Inizializzo i campi per l'elaborazione
097000000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
097100000000     C                   Z-ADD     *ZERO         $FILPR
097200000000     C                   MOVE      *BLANKS       $CBLPR
097300000000     C                   Z-ADD     *ZERO         $NFVPR
097400940509     C                   CLEAR                   $RIGHE
097500950210      *
097600950210      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
097700950210     C                   MOVEA     £MOD          CMD(33)
097800950210     C                   MOVEA     £OUTQ         CMD(51)
097900950210      *
098000940502      * Eseguo OVRPRTF sul file di stampa
098100950210     C                   Z-ADD     82            LUNG             15 5
098200950213     C                   MOVEL     *BLANKS       COMMAN           82
098300940502     C                   MOVEA     CMD           COMMAN
098400940502     C                   CALL      'QCMDEXC'                            H1
098500940502     C                   PARM                    COMMAN
098600940502     C                   PARM                    LUNG
098700970807     C  N38              OPEN      FNLSC2P
098800950118     C  N38              WRITE     LSC2T1
098900950118     C  N38              WRITE     LSC2T3
099000940502     C  N38              SETOFF                                       30
099100000619     C**
099200000619     C* SE DS1RIS= 'S' ACCENDO PER ORA IL 38
099300000619     C     SAVRIS        IFNE      ' '
099400000619     C                   SETON                                        38
099500000619     C                   ENDIF
099600000000      *
099700950210      * L'elaborazione solo se non scelta la ristampa (DS1RIS='R')
099800950210    1C     DS1RIS        IFNE      'R'
099900000000      *
100000970205      * Leggo il file delle bolle in partenza prima quelli con data
100100970205      * F.V. = 0 poi quelli con DFV >= della data elaborazione
100200940401     C                   Z-ADD     1             $X                3 0
100300940428     C                   MOVE      *BLANK        $FLAG
100400940428     C*
100500940428    2C     LIN($X)       DOWGT     *ZERO
100600000912     C                   Z-ADD     LIN($X)       $FILPR
100700000912      * Escludo bolle LNP Poste
100800020813     ***C                   CLEAR                   §OGPT
100900020813     C                   clear                   OG143
101000000912     C     $FILPR        CHAIN     AZORG01L                           82
101100000912     C  N82              MOVEL     ORGDE3        OG143
101200000912     C**
101300020813   2A***C     §OGPT         IFNE      'S'
101400020813    2C                   IF        §OGntw <> 'PPT'
101500940428     C                   CLEAR                   $DFV
101600940428     C                   SETOFF                                           01
101700940428      *
101800940428    3C                   DO        2
101900090304     C     FNartK        SETLL     Fiart38L
102000090304     C  N01FNartK        READE     Fiart38L                               33
102100090304     C   01LIN($X)       READE     Fiart38L                               33
102200940428     C*
102300940428    4C     *IN33         DOWEQ     *OFF
102400000000      *
102500000000      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
102600000000      * come test ma mi permette di saltare delle chain)
102700090304    5C     $artAT        IFNE      $artPR
102800950118     C     FNBLPK        CHAIN     FNBLP01L                           99
102900000000     C   99              GOTO      LETTUR
103000090304     C                   MOVE      $artAT        $artPR
103100000000     C                   Z-ADD     BLPNFV        $NFVPR
103200950118     C                   Z-ADD     BLPMGS        $DATSP            8 0
103300000000     C                   MOVEL     BLPAAS        $DATSP
103400000331     C                   Z-ADD     BLPDRT        $DTRIT
103500000413     C                   Z-ADD     BLPPDR        $CDPDR
103600940428     C* $BOLLA = 1 INDICA CHE HO LETTO UNA NUOVA BOLLA
103700940428     C                   MOVEL     '1'           $BOLLA            1
103800000000      *
103900000000      * Decodifico il tipo bolla
104000950118    6C     BLPCBO        IFNE      $CBLPR
104100950210     C                   MOVE      '3A'          KCOD
104200950210     C                   MOVE      *BLANKS       KKEY
104300950210     C                   MOVEL     BLPCBO        KKEY
104400950210     C     KTAB1         CHAIN     TABEL00F                           99
104500000000     C  N99              MOVEL     TBLUNI        DS3A
104600950118     C  N99              MOVE      BLPCBO        $CBLPR
104700000000     C   99              CLEAR                   DS3A
104800940428    6C                   ENDIF
104900940427     C*
105000940428    5C                   ENDIF
105100000000      *
105200940428      * ESCLUSIONI BOLLE
105300000000     C                   EXSR      ESCLUS
105400940428    5C     $OK           IFEQ      'S'
105500000000     C                   EXSR      ELABOR
105600940428    5C                   ENDIF
105700000000      *
105800940428     C     LETTUR        TAG
105900090304     C  N01FNartK        READE     Fiart38L                               33
106000090304     C   01LIN($X)       READE     Fiart38L                               33
106100940428    4C                   ENDDO
106200940428      *
106300940428     C                   SETON                                        01
106400970205     C                   Z-ADD     DS1AAL        $DFV
106500940428    3C                   ENDDO
106600940428      *
106700940428      * cambia la filiale --> decodifico e stampo i totali
106800940428     C                   EXSR      CALSPE
106900940428     C                   EXSR      WRISAT
107000940429     C                   EXSR      WR1SAT
107100940428     C  N38              EXSR      TOTFIL
107200940428     C                   EXSR      AZZFIL
107300000912   2AC                   ENDIF
107400940428     C*
107500940401     C                   ADD       1             $X
107600940428    2C                   ENDDO
107700940429      *
107800940429     C*
107900940429     C* STAMPA DEI DATI FINALI
108000940506     C                   EXSR      STAFIN
108100000616   X C                   ENDIF
108200000616      *
108300000616     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
108400000616     C     SAVRIS        IFEQ      'S'
108500000616     C                   MOVEL     'R'           DS1RIS
108600000616     C                   MOVEA     SVLIN         LIN
108700000616     C* AZZERO LE SCHIERE DI TOTALE
108800000616     C                   EXSR      AZZSER
108900000616     C                   EXSR      AZZTRA
109000000616     C                   EXSR      AZZTOT
109100000619     C                   SETOFF                                       38
109200000616     C                   ENDIF
109300000000      *
109400970211      * Nel caso di RISTAMPA mi limito a rileggere il file statistiche (FNSAT)
109500000000      * e stampare i record che rientrano nell'intervallo dal-al
109600000616     C     DS1RIS        IFNE      ' '
109700000000     C                   EXSR      RISTAM
109800940428    1C                   ENDIF
109900000000      *
110000000000      * Stampa terminata
110100940428     C     *IN38         IFEQ      *OFF
110200940429     C*
110300940428     C     $ALMEN        IFEQ      *ZERO
110400950118     C                   WRITE     LSC2E1
110500940505     C                   ELSE
110600940429     C*
110700000000     C                   EXSR      STATER
110800940505     C                   ENDIF
110900000000      *
111000950118     C                   WRITE     LSC2E2
111100950629     C                   WRITE     LSC2E5
111200000000      *
111300000000      * Verifico se vi sono fogli viaggio aperti
111400000000     C                   EXSR      FOGVIA
111500000000      *
111600970807     C                   CLOSE     FNLSC2P
111700940428     C                   ENDIF
111800000000      *
111900000000      * Aggiorno la data ultima elaborazione
112000950210     C     DS1RIS        IFNE      'R'
112100000616     C     SAVRIS        OREQ      'S'
112200021016
112300021016     c                   Eval      kTbeCod = 'SDF'
112400021016     c                   Movel(p)  Simfel        kTbeKe1
112500021016     c     Ktbe          Chain     Tntbe01l
112600021016     c                   If        %Found(Tntbe01l)
112700021016     c                   Movel     TbeUni        dSdf
112800021016     c                   If        §SdfTerP = *Zeros or §SdfTerP > Ds1Ada
112900021016     c                   Z-Add     Ds1Ada        §SdfTerP
113000021016     c                   EndIf
113100021016     c                   If        §SdfTerU < Ds1Ada
113200021016     c                   Z-Add     Ds1Ada        §SdfTerU
113300021016     c                   EndIf
113400021016     c                   Movel     dSdf          TbeUni
113500021016     c                   Update    Tntbe000
113600021016     c                   EndIf
113700000000     C                   ENDIF
113800000912     C*
113900090415     c* Se non ho aggioranto la tabella, vedo se devo cancellare record
114000090415     c*  in FISTA
114100090415     c                   if        savris='S'
114200090415     c     ksta          delete    fista01l
114300090415     C                   ENDIF
114400090415     c
114500000912    1C                   ENDIF
114600000000      *
114700940505     C     ENDLEG        ENDSR
114800000000      **************************************************************************
114900000000      * Una volta letto il record verifico se da includere o no
115000940427      * PER QUALSIASI TIPO DI CALCOLO  (COLLI E BOLLE A TERRA E
115100940427      *  COLLI E BOLLA DA PARTIRE)
115200000000      *
115300940427      * Vi sono solo due motivi per escludere il record letto:
115400940427      * (1) se la spedizione e' stata bollettata dopo la data elaboraz
115500950210      *                Data spedizione > DS1AAL
115600000000      *
115700940427      * (2) se la data partenza del collo e' < della data elaborazione
115800090304      *                artDFV < DS1AAL
115900000000      *
116000000810      * ATTENZIONE.. DA OGGI C'E' UN MOTIVO IN PIU'.. Canale5 ! da cantare ..
116100000810      *
116200000810      * (3) se la linea di partenze è ??..... POSTE !
116300000810      *
116400940427      * Bolle di recupero / bolle legate / bolle "£" sono escluse
116500000000      **************************************************************************
116600000000     C     ESCLUS        BEGSR
116700000000      *
116800000000     C                   MOVE      'N'           $OK               1
116900000331     C                   MOVE      *BLANKS       WSOS              1
117000000810      *
117100000810      *
117200940427      * esclusioni:
117300950323      * Le bolle "legate" sono identificabili dal nr.foglio viaggio
117400950323      * = 99999 O = 99998
117500070522    1C     $NFVPR        IFEQ      99999
117600950323     C     $NFVPR        OREQ      99998
117700940427     C     $CBLPR        OREQ      '£'
117800940428     C     §3ARBL        OREQ      'R'
117900000000     C                   GOTO      ENDESC
118000070522    1C                   ENDIF
118100000413     C* Prepagati non confermati
118200070522    1C     $CDPDR        IFEQ      *ZEROS
118300000413     C                   GOTO      ENDESC
118400070522    1C                   ENDIF
118500000000      *
118600090304    1C     artDFV        IFEQ      DS1AAL
118700950629     C                   MOVEL     'S'           $OK
118800070522   x1C                   ELSE
118900070522     C
119000070522     c* Elaborazione in base A     Data BORDERÒ
119100070522   1AC                   IF        DS1AAL<§VPOnpi
119200070522     C
119300940427      * La data di spedizione deve essere <= alla data elaborazione
119400070522    2C     $DATSP        IFLE      DS1AAL
119500000000      *
119600940427      * La data partenza deve essere > = alaa data elaborazione        limiti
119700090304    3C     artDFV        IFEQ      *ZERO
119800090304     C     artDFV        ORGE      DS1AAL
119900000000     C                   MOVE      'S'           $OK
120000000000     C                   GOTO      ENDESC
120100070522    3C                   ENDIF
120200070522   X2C                   ELSE
120300000331     C* Se data spedizione > di data elaborazione
120400000411     C* e  data ritiro    <= data elaborazione    ==> BOLLA IN SOSPENS.
120500070522    3C     $DTRIT        IFLE      DS1AAL
120600000331     C                   MOVE      'S'           $OK
120700000331     C                   MOVEL     'S'           WSOS
120800070522     C                   GOTO      ENDESC
120900070522    3C                   ENDIF
121000950626     C*
121100070522    2C                   ENDIF
121200070522     c
121300070522  x1ac                   else
121400070522     c****
121500070522     c* ELAborazione in base a    data RITIRO Merce
121600070522      * La data ritiro deve essere <= alla data elaborazione
121700070522    2C     $DTRIT        IFLE      DS1AAL
121800070522      *
121900070525      * La data partenza deve essere > = alla data elaborazione        limiti
122000090304    3C     artDFV        IFEQ      *ZERO
122100090304     C     artDFV        ORGE      DS1AAL
122200070522     C                   MOVE      'S'           $OK
122300070522     C                   GOTO      ENDESC
122400070522    3C                   ENDIF
122500070522    2C                   ENDIF
122600070522   1aC                   ENDIF
122700950629     C*
122800070522    1C                   ENDIF
122900000000      *
123000000000     C     ENDESC        ENDSR
123100000000      **************************************************************************
123200000000      * Elaborazioni dei record letti, somme nei totali
123300000000      **************************************************************************
123400000000     C     ELABOR        BEGSR
123500000000      *
123600970210      * Verifico il tipo spedizione ESPR./DIST./CORR.
123700950201     C                   Z-ADD     3             $T                1 0
123800000000     C     BLPTSP        IFEQ      'E'
123900000000     C                   Z-ADD     1             $T
124000000000     C                   ENDIF
124100950201     C     BLPTSP        IFEQ      'D'
124200000619     C     BLPTSP        OREQ      'P'
124300940323     C                   Z-ADD     2             $T
124400000000     C                   ENDIF
124500000000      *
124600000000      * C/servizio oppure no ?
124700940428     C     §3ARBL        COMP      'C'                                3434
124800000000      *
124900970205     C*
125000070522    1C     WSOS          IFEQ      *BLANKS
125100970205     C* DETERMINAZIONE SE COLLO A TERRA
125200000000      * Ho due controlli da fare per sapere se il collo e' a terra:
125300090304      *    1)  artDFV = 0
125400090304      *    2)  artDFV > Data spedizione   AND   artDFV > DS1AAL
125500000000     C                   SETOFF                                       36
125600090304    2C     artDFV        IFEQ      *ZERO
125700090304     C     artDFV        ORGT      DS1AAL
125800000000     C                   SETON                                        36
125900070522    2C                   ENDIF
126000970303     C*
126100970303     C* RICHIAMO PGM PER CALCOLO AFFIDABILITA'
126200970303     C* (SOLO SE HO LETTO UNA NUOVA BOLLA - $BOLLA = '1')
126300070522    2C     $BOLLA        IFEQ      '1'
126400070522    3C     BLPDCE        IFEQ      *ZEROS
126500970303     C     BLPDUC        ORNE      DS1AAL
126600970303     C                   MOVEL     ' '           D98TLA
126700970303     C                   MOVEL     'P'           D98TBO
126800970303     C                   MOVEL     BLPAAS        D98AAS
126900970303     C                   MOVEL     BLPLNP        D98LNP
127000970303     C                   MOVEL     BLPNRS        D98NRS
127100970303     C                   MOVEL     BLPNSP        D98NSP
127200970310     C                   Z-ADD     DS1AAL        D98DRF
127300970310     C                   Z-ADD     2359          D98ORF
127400970925     C                   CALL      'TNSD99R'
127500970925     C                   PARM                    DS99
127600970303     C                   ELSE
127700970303     C                   Z-ADD     BLPNPI        D98NPI
127800070522    3C                   END
127900070522    2C                   END
128000970207     C*
128100970207     C*VERIFICO SE SI TRATTA DI "A TERRA DA PARTIRE"
128200970210     C* La considero "non da partire" se si tratta di:
128300970207     C* - spedizione non borderizzata
128400970303     C* - distribuzione a terra del giorno
128500070522     c* - dal 1° giugno 2007 il calcolo della affidabilità a terra sarà
128600070522     c*   in base alla data ritiro, per cui nel DA PARTIRE non controllo
128700070522     c*   più la data borderò ma solo se si tratta della distribuzione
128800070522     c*   del giorno
128900970205     C                   SETOFF                                       35
129000070522     c
129100070522    2C     *IN36         IFEQ      *ON
129200070522     c
129300070522    3c                   if        ds1aal<§vponpi
129400970210     C*
129500070522    4C     BLPDBR        IFGT      DS1AAL
129600070522     C     BLPDBR        OREQ      *ZEROS
129700070522     C                   SETON                                        35
129800070522   x4C                   ELSE
129900970304     C*
130000070522    5C     BLPDBR        IFLE      DS1AAL
130100070522     C     BLPDPC        ANDEQ     0
130200070522     C     D98NPI        ANDLT     0
130300070522     C*
130400070522     C     BLPDBR        ORLE      DS1AAL
130500070522     C     BLPDPC        ANDGT     DS1AAL
130600070522     C     D98NPI        ANDLT     0
130700070522     C                   SETON                                        35
130800070522    5C                   END
130900070522    4C                   END
131000070522     c
131100070522   x3c                   else
131200070522     c*
131300090108     C* SE affidabilità =0 verifico se cade in giorno festivo, per
131400090108     c*    ter-ter --> se si non do ritardo
131500090108     c                   if        d98npi=0
131600090108     C                   movel     blptfa        $TFA
131700090108      *
131800090108     C     AZCLNK        CHAIN     AZCLN01L                           99
131900090108     C                   MOVEA     CLNMAT        GME
132000090108     C                   MOVEA     CLNpom        GMEp
132100090108     C     GME($GI)      IFEQ      'F'
132200090108     C     GMEp($GI)     oreq      'F'
132300090108     c                   sub       24            d98npi
132400090108     c                   endif
132500090108     c                   endif
132600090108     c
132700070522    4c                   if        (blpdpc=0 and d98npi<0) or
132800070522     c                             (blpdpc>ds1aal and d98npi<0)
132900070522     C                   SETON                                        35
133000070522    4c                   endif
133100070522    3c                   endif
133200970210     C*
133300070522    2C                   END
133400070522    1C                   ENDIF
133500970205     C*
133600000000      *
133700970205      * Aggancio, nel file statistiche, il record per il terminal di
133800970205      * arrivo poiche' lo dovrò aggiornare
133900970205     C                   CLEAR                   FNSAT000
134000000000     C                   Z-ADD     SIMFEL        SATFLE
134100950210     C                   Z-ADD     DS1AAL        SATDRE
134200940428     C                   Z-ADD     BLPLNP        SATLNP
134300000000     C                   Z-ADD     BLPTFA        SATTFA
134400000000     C                   MOVE      *BLANK        SATTRE
134500970205     C     FNSA1K        CHAIN     FNSAT02L                           37
134600940428      *
134700970205      * NUOVA BOLLA LETTA: CONTO LE SPEDIZIONI
134800940428      *
134900940428     C     $BOLLA        IFEQ      '1'
135000940428     C                   EXSR      DATBOL
135100970205     C                   ENDIF
135200021217      *
135300021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
135400030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
135500030213      * Peso medio
135600030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
135700021220     c                   z-add     Wpesmed       $Peso
135800021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
135900030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
136000030213      * Volume medio
136100030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
136200021220     c                   z-add     Wvolmed       $Volum
136300000000      *
136400000000      * ?  CALCOLI  SUI  COLLI  ?
136500000000      *
136600000331     C     WSOS          IFEQ      *BLANKS
136700000000      * Nr.colli da partire (servira' per la percentuale)
136800970207     C     *IN35         IFEQ      *OFF
136900970210     C   34              ADD       1             CPA($T)
137000970210     C  N34              ADD       1             C4A($T)
137100970210     C                   ADD       1             CDT($T)
137200950626     C                   END
137300000000      *
137400940427     C     *IN36         IFEQ      *ON
137500970207      * A terra da partire                                             (35ON)
137600940427     C   34
137700940427     CANN35              ADD       1             CBP($T)
137800940427     C  N34
137900940427     CANN35              ADD       1             C4P($T)
138000970207     C  N35              ADD       1             C0P($T)
138100970207     C  N35              ADD       1             C3P($T)
138200970207      * A terra non da partire                                         (35ON)
138300940427     C   34
138400940427     CAN 35              ADD       1             CBR($T)
138500940427     C  N34
138600940427     CAN 35              ADD       1             C4R($T)
138700940427     C   35              ADD       1             C0R($T)
138800940427     C                   ENDIF
138900000000      *
139000000000      * ?  CALCOLI  SUL  PESO  ?
139100000000      *
139200970207     C     *IN35         IFEQ      *OFF
139300970210     C   34              ADD       $PESO         PPA($T)
139400970210     C  N34              ADD       $PESO         P4A($T)
139500970210     C                   ADD       $PESO         PDT($T)
139600950626     C                   END
139700940502      * A TERRA
139800940427     C     *IN36         IFEQ      *ON
139900940502      *
140000970206      * Fino a 35 quintali
140100021217     C     Wpeso         IFLE      3500
140200970207     C* Da partire
140300970206     C   34
140400970206     CANN35              ADD       $PESO         PFI($T)
140500970206     C  N34
140600970206     CANN35              ADD       $PESO         P4I($T)
140700970206     C  N35              ADD       $PESO         P3I($T)
140800970206     C  N35              ADD       $PESO         P0I($T)
140900970207     C* Non da partire
141000970206     C   34
141100970206     CAN 35              ADD       $PESO         PFN($T)
141200970206     C  N34
141300970206     CAN 35              ADD       $PESO         P4N($T)
141400970206     C   35              ADD       $PESO         P0N($T)
141500970206     C                   ELSE
141600970206      * Oltre 35 quintali:                                             (35ON)
141700970207     C* Da partire
141800940427     C   34
141900940427     CANN35              ADD       $PESO         PBP($T)
142000940427     C  N34
142100940427     CANN35              ADD       $PESO         P4P($T)
142200970206     C  N35              ADD       $PESO         P0P($T)
142300970206     C  N35              ADD       $PESO         P3P($T)
142400970207     C* Non da partire
142500940427     C   34
142600940427     CAN 35              ADD       $PESO         PBR($T)
142700940427     C  N34
142800940427     CAN 35              ADD       $PESO         P4R($T)
142900940427     C   35              ADD       $PESO         P0R($T)
143000940427     C                   ENDIF
143100970206     C                   ENDIF
143200000331     C*
143300000331     C                   ENDIF
143400000000      *
143500000000      * ?  CALCOLI  SUL  VOLUME  ?
143600940329      *
143700000000      *
143800021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
143900000000      *
144000000331     C     WSOS          IFEQ      *BLANKS
144100970207     C     *IN35         IFEQ      *OFF
144200970210     C   34              ADD       $VOLUM        VPA($T)
144300970210     C  N34              ADD       $VOLUM        V4A($T)
144400970210     C                   ADD       $VOLUM        VDT($T)
144500950626     C                   END
144600940505      *
144700940428     C     *IN36         IFEQ      *ON
144800970207     C* Da partire
144900940428     C   34
145000940428     CANN35              ADD       $VOLUM        VBP($T)
145100940428     C  N34
145200940428     CANN35              ADD       $VOLUM        V4P($T)
145300970206     C  N35              ADD       $VOLUM        V0P($T)
145400970206     C  N35              ADD       $VOLUM        V3P($T)
145500000331     c* Sia che sia da partire che non da partire: sommo nel totale
145600000331     C* Mc rimasti a magazzino
145700000331     C   34              ADD       $VOLUM        VBR($T)
145800000331     C  N34              ADD       $VOLUM        V4R($T)
145900000331     C                   ADD       $VOLUM        V0R($T)
146000000406     C                   ADD       $VOLUM        V3R($T)
146100000000      *
146200000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
146300000000      *
146400970205      * Visto che le spedizioni non riesco a calcolarle ora, mi limito
146500970205      * a tenere aggiornato il file di work con le bolle
146600940428     C   34              MOVE      *BLANK        WRKCSE
146700940428     C  N34              MOVE      'C'           WRKCSE
146800940503     C                   Z-ADD     BLPLNP        WRKLNP
146900950118     C     FNWR1K        CHAIN     FNWRK00F                           99
147000940428     C     *IN99         IFEQ      *OFF
147100000000     C                   ADD       1             WRKNC1
147200000000     C                   EXCEPT    AGGIO
147300000000     C                   ELSE
147400950118     C                   MOVEL     *BLANKS       WTRANS            1
147500940503     C                   EXSR      WRIWRK
147600000000     C                   ENDIF
147700940428     C                   ENDIF
147800000331     c* Se collo di bolla in sospensione sommo volume in totale mc
147900000331     c* rimasti a magazzino
148000000331     C                   ELSE
148100000331     C   34              ADD       $VOLUM        VBR($T)
148200000331     C  N34              ADD       $VOLUM        V4R($T)
148300000331     C                   ADD       $VOLUM        V0R($T)
148400000406     C                   ADD       $VOLUM        V3R($T)
148500000412     C                   ADD       $VOLUM        VOLUME            9 4
148600000331     C                   ENDIF
148700940323      *
148800040921     C                   MOVE      $FTR          SATFTR
148900040921     C                   MOVE      $DTR          SATDTR
149000940428     C     *IN37         IFEQ      *ON
149100970205     C                   WRITE     FNSAT000
149200940428     C                   ELSE
149300970205     C                   UPDATE    FNSAT000
149400940428     C                   ENDIF
149500000000      *
149600000000     C                   ENDSR
149700940428      **************************************************************************
149800940428      * Carico i dati della bolla (peso e volume)
149900940428      *  e conto le spedizioni partite il giorno di elaborazione
150000940428      **************************************************************************
150100940428     C     DATBOL        BEGSR
150200021217      *
150300021217      * Verifico i dati misurati con la macchina smistacolli
150400021217      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
150500021220      * e calcolo il VOLUME unitario medio per collo da usare se non
150600021220      * tutti i colli sono stati volumati dalla smistacolli
150700021217      *
150800030213      **** Verifico se sono stati volumati tutti i colli
150900021217     c                   clear                   Wvolu
151000050112     c                   if        BLPncr = BLPncl
151100021217     c                   z-add     BLPvlc        Wvolu
151200021217   x1c                   else
151300021217if  2c                   if        BLPvlc >= BLPvlf
151400021217     c                   z-add     BLPvlc        Wvolu
151500021217   x2c                   else
151600021217     c                   z-add     BLPvlf        Wvolu
151700021217e   2c                   endif
151800021217e   1c                   endif
151900021220      *
152000021220      * Calcolo il volume medio
152100021220     c     Wvolu         div(H)    BLPncl        Wvolmed
152200021217      *
152300021217      * Verifico i dati misurati con la macchina smistacolli
152400021217      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
152500021220      * e calcolo il PESO   unitario medio per collo da usare se non
152600021220      * tutti i colli sono stati pesati   dalla smistacolli
152700021217      *
152800030213      **** Verifico se sono stati pesati   tutti i colli
152900021217     c                   clear                   Wpeso
153000050112     c                   If        BLPncp = BLPncl
153100021217     c                   z-add     BLPpkc        Wpeso
153200021217   c1c                   else
153300021217if  2c                   if        BLPpkc >= BLPpkf
153400021217     c                   z-add     BLPpkc        Wpeso
153500021217   x2c                   else
153600021217     c                   z-add     BLPpkf        Wpeso
153700021217e   2c                   endif
153800021217e   1c                   endif
153900021220      *
154000021220      * Calcolo il peso medio
154100021220     c     Wpeso         div(H)    BLPncl        Wpesmed
154200940428     C*
154300940428     C* SOMMO SOLTANTO LE SPEDIZIONI PARTITE NEL GIORNO
154400970205     C*  LA DATA DI RIFERIMENTO E' LA DATA ULTIMO COLLO
154500940506     C     $FLAG         IFEQ      ' '
154600970205     C     BLPDUC        ANDEQ     DS1AAL
154700000331     C     WSOS          ANDEQ     *BLANKS
154800950118     C                   MOVEL     *BLANKS       WTRANS
154900940503     C                   EXSR      CTRBOL
155000970205     C                   ENDIF
155100940428     C*
155200940428     C                   MOVEL     '2'           $BOLLA
155300940428     C                   ENDSR
155400950118      **************************************************************************
155500021217      * Carico i dati della bolla (peso e volume) (TRANSITI)
155600021217     *** *  e conto le spedizioni partite il giorno di elaborazione
155700021217     *** *    (questo NON lo faceva NEANCHE prima)
155800950118      **************************************************************************
155900950118     C     DATBOT        BEGSR
156000021220      *
156100021220      * Verifico i dati misurati con la macchina smistacolli
156200021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
156300021220      * e calcolo il VOLUME unitario medio per collo da usare se non
156400021220      * tutti i colli sono stati volumati dalla smistacolli
156500021217      *
156600030213      **** Verifico se sono stati volumati tutti i colli
156700021217     c                   clear                   Wvolu
156800050112     c                   if        BTPncr = BTPncl
156900021217     c                   z-add     BTPvlc        Wvolu
157000021217   x1c                   else
157100021217if  2c                   if        BTPvlc >= BTPvlb
157200021217     c                   z-add     BTPvlc        Wvolu
157300021217   x2c                   else
157400021217     c                   z-add     BTPvlb        Wvolu
157500021217e   2c                   endif
157600021217e   1c                   endif
157700021220      *
157800021220      * Calcolo il volume medio
157900021220     c     Wvolu         div(H)    BTPncl        Wvolmed
158000021220      *
158100021220      * Verifico i dati misurati con la macchina smistacolli
158200021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
158300021220      * e calcolo il PESO   unitario medio per collo da usare se non
158400021220      * tutti i colli sono stati pesati   dalla smistacolli
158500021217      *
158600030213      **** Verifico se sono stati pesati   tutti i colli
158700021217     c                   clear                   Wpeso
158800050112     c                   if        BTPncp = BTPncl
158900021217     c                   z-add     BTPpkc        Wpeso
159000021217   x1c                   else
159100021217if  2c                   if        BTPpkc >= BTPpkb
159200021217     c                   z-add     BTPpkc        Wpeso
159300021217   x2c                   else
159400021217     c                   z-add     BTPpkb        Wpeso
159500021217e   2c                   endif
159600021217e   1c                   endif
159700021220      *
159800021220      * Calcolo il peso medio
159900071231     c     Wpeso         div(H)    BTPncl        Wpesmed
160000021217      *
160100950118     C                   MOVEL     '2'           $BOLLA
160200950118     C                   ENDSR
160300940503      **************************************************************************
160400950118      * Scrivo il file FNWRK00F
160500940503      **************************************************************************
160600940503     C     WRIWRK        BEGSR
160700950118     C*
160800940503     C                   MOVE      $FLAG         WRKFLG
160900950118     C                   Z-ADD     1             WRKNC1
161000950118     C*
161100950118     C     WTRANS        IFNE      'T'
161200940503     C                   Z-ADD     BLPAAS        WRKAAS
161300940503     C                   Z-ADD     BLPMGS        WRKMGS
161400940503     C                   Z-ADD     BLPNRS        WRKNRS
161500940503     C                   Z-ADD     BLPNSP        WRKNSP
161600940503     C                   Z-ADD     BLPLNA        WRKLNA
161700940503     C                   MOVE      BLPTSP        WRKTSP
161800940503     C                   Z-ADD     BLPNCL        WRKNCL
161900940503     C                   Z-ADD     BLPTFA        WRKTFA
162000070525     c* Se dopo la data di attivazione, memorizzo la data borderò
162100070525    3c                   if        ds1aal<§vponpi
162200970207     C                   Z-ADD     BLPDBR        WRKDBR
162300070525     c                   else
162400070525     C                   Z-ADD     BLPDrt        WRKDBR
162500070525     c                   endif
162600070525     c
162700970303     C                   Z-ADD     BLPDPC        WRKDPC
162800970303     C                   Z-ADD     BLPDUC        WRKDUC
162900970303     C                   Z-ADD     D98NPI        WRKNPI
163000950118     C*
163100950118     C                   ELSE
163200950118     C*
163300950118     C                   Z-ADD     BTPAAS        WRKAAS
163400950118     C                   Z-ADD     BTPMGS        WRKMGS
163500950118     C                   Z-ADD     BTPNRS        WRKNRS
163600950118     C                   Z-ADD     BTPNSP        WRKNSP
163700950118     C                   Z-ADD     BTPLNA        WRKLNA
163800950118     C                   MOVE      BTPTSP        WRKTSP
163900950118     C                   Z-ADD     BTPNCL        WRKNCL
164000950118     C                   Z-ADD     BTPTFA        WRKTFA
164100970207     C                   CLEAR                   WRKDBR
164200970303     C                   CLEAR                   WRKDUC
164300970303     C                   CLEAR                   WRKDPC
164400970303     C                   CLEAR                   WRKNPI
164500950118     C                   ENDIF
164600950118     C*
164700950118     C                   WRITE     FNWRK000
164800950118     C*
164900940503     C                   ENDSR
165000940503      **************************************************************************
165100940503      * Calcolo le spedizioni da partire:
165200950118      *  vedo se gia' contata prima di ricontarla(se esiste in FNWRK)
165300970207      *  Potrei averla già contata se i colli non sono sequenziali
165400940503      **************************************************************************
165500940503     C     CTRBOL        BEGSR
165600940503     C*
165700940505     C                   MOVEL     $FLAG         $FLAG2
165800940503     C                   MOVEL     'P'           $FLAG
165900940503     C                   MOVEL     BLPLNP        WRKLNP
166000940503     C                   CLEAR                   WRKCSE
166100950118     C     FNWR1K        SETLL     FNWRK00F                               99
166200940503     C* BOLLA NON TROVATA  LA SCRIVO
166300940503    1C     *IN99         IFEQ      *OFF
166400940503     C                   EXSR      WRIWRK
166500940503     C*
166600970211     C                   ADD       1             SDT($T)
166700970210     C   34              ADD       1             SPA($T)
166800940503     C* S4A --> PER C/S SPEDIZIONI DELL'AREA
166900940503     C*         PER I TRANSITI
167000970210     C  N34              ADD       1             S4A($T)
167100940503    1C                   ENDIF
167200940505     C                   MOVEL     $FLAG2        $FLAG
167300940503     C                   ENDSR
167400000000      **************************************************************************
167500000000      * Calcolo sulle spedizioni, lettura del file di work creato prima
167600000000      **************************************************************************
167700000000     C     CALSPE        BEGSR
167800940502     C                   SETOFF                                       3435
167900940428      * Verifico se e' transito
168000940502     C     $FLAG         COMP      'T'                                    35
168100000000      *
168200000000      * Leggo tutti le spedizioni della filiale da stampare dal file di work
168300970207    1C     $FLAG         IFEQ      *BLANK
168400950118     C     FNWRKK        SETLL     FNWRK00F
168500970207   X1C                   ELSE
168600950118     C     $FLAG         SETLL     FNWRK00F
168700970207    1C                   ENDIF
168800000000      *
168900970207    1C                   DO        *HIVAL
169000000000      *
169100970207    2C     $FLAG         IFEQ      *BLANK
169200950118     C     FNWRKK        READE(N)  FNWRK00F                               99
169300970207   X2C                   ELSE
169400950118     C     $FLAG         READE(N)  FNWRK00F                               99
169500970207    2C                   ENDIF
169600000000      *
169700970207    2C  N99              DO
169800000000      *
169900970210      * Verifico il tipo spedizione ESPR./DIST./CORR.
170000950201     C                   Z-ADD     3             $V                1 0
170100970207    3C     WRKTSP        IFEQ      'E'
170200000000     C                   Z-ADD     1             $V
170300970207    3C                   ENDIF
170400970207    3C     WRKTSP        IFEQ      'D'
170500940323     C                   Z-ADD     2             $V
170600970207    3C                   ENDIF
170700940428      *
170800940428      * Verifico se e' c/servizio
170900940502     C  N35WRKCSE        COMP      *BLANKS                                34
171000000000      *
171100000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
171200000000      * poiche' lo dovro aggiornare
171300970207     C                   CLEAR                   FNSAT000
171400000000     C                   Z-ADD     SIMFEL        SATFLE
171500950210     C                   Z-ADD     DS1ADA        SATDRE
171600940506     C  N35              Z-ADD     WRKLNP        SATLNP
171700940506     C   35              Z-ADD     SIMFEL        SATLNP
171800000000     C                   Z-ADD     WRKTFA        SATTFA
171900000000     C  N35              MOVE      *BLANK        SATTRE
172000000000     C   35              MOVE      'T'           SATTRE
172100970205     C     FNSA1K        CHAIN     FNSAT02L                           37
172200000000      *
172300950628     C                   MOVE      WRKMGS        $DATS1            8 0
172400950628     C                   MOVEL     WRKAAS        $DATS1
172500950628     C*
172600970207     C* A TERRA COMPLETAMENTE
172700970207      * La spedizione e' completa se il nr.colli spediti (WRKNC1) e' = uguale
172800050112      * al nr.colli della bolla (WRKNCL)
172900970207    3C     WRKNC1        IFEQ      WRKNCL
173000940506     C                   ADD       1             S0M($V)
173100970207     C* Transiti
173200970207    4C     *IN35         IFEQ      *ON
173300940506     C                   ADD       1             S2M($V)
173400970207   X4C                   ELSE
173500970207     C* Dell'area
173600940506     C   34              ADD       1             SCM($V)
173700940506     C  N34              ADD       1             S4M($V)
173800940506     C                   ADD       1             S3M($V)
173900970207     C*
174000970207    4C                   ENDIF
174100970207    3C                   ENDIF
174200970210     C* COMDAP = BLANKS => A TERRA DA PARTIRE
174300970207     C                   CLEAR                   COMDAP
174400970303     C* BORDERIZZATO, DISTRIBUZIONE A TERRA DEL GIORNO NON SPAREGGIATA
174500070525    3c                   if        ds1aal<§vponpi
174600070525     c
174700970207    3C     WRKDBR        IFGT      *ZEROS
174800970207     C     WRKDBR        ANDLE     DS1AAL
174900970303     C*
175000970303    4C     WRKDUC        IFEQ      *ZEROS
175100970303     C     WRKDPC        ANDEQ     0
175200970303     C     WRKNPI        ANDLT     0
175300970303     C*
175400970303     C     WRKDUC        OREQ      *ZEROS
175500970303     C     WRKDPC        ANDGT     DS1ADA
175600970303     C     WRKNPI        ANDLT     0
175700970303     C*
175800970303     C     WRKDUC        ORGT      DS1ADA
175900970303     C     WRKDPC        ANDGT     DS1ADA
176000970303     C     WRKNPI        ANDLT     0
176100970207     C                   MOVE      'N'           COMDAP            1
176200970207    4C                   END
176300970207   X3C                   ELSE
176400970207     C* NON BORDERIZZATO
176500970207     C                   MOVE      'N'           COMDAP
176600970207    3C                   END
176700070525     c*
176800070525     c                   else
176900070525     c
177000070525     c* calcolo per data ritiro a terra non da partire solo la distribu
177100070525     c*  zione del giorno
177200070525    4c                   if        (wrkdpc=0 and wrknpi<0) or
177300070525     c                             (wrkdpc>ds1aal and wrknpi<0)
177400070525     C                   MOVE      'N'           COMDAP
177500070525    4c                   endif
177600070525    3c                   endif
177700070525     c
177800970207     C*
177900970207    3C     COMDAP        IFEQ      *BLANKS
178000970207     C* Per i transiti memorizzo in un unico totale le spediz. a terra
178100970207     C* e non mi interessa nemmeno memorizzare il totale da partire
178200970207     C     $FLAG         OREQ      'T'
178300970207     C***
178400970207     C* TOTALE DA PARTIRE
178500970207     C***
178600970207     C* Sommo le spediz.a terra border. esclusa la distrib. del periodo
178700970210     C  N35              ADD       1             SDT($V)
178800970210     C  N35
178900970210     CAN 34              ADD       1             SPA($V)
179000970207     C* S4A --> Per c/s spedizioni dell'area
179100970210     C  N35
179200970210     CANN34              ADD       1             S4A($V)
179300970207     C***
179400970207     C* A TERRA DA PARTIRE
179500970207     C***
179600970207     C   34
179700970207     CANN35              ADD       1             SBP($V)
179800970207     C  N34
179900970207     CANN35              ADD       1             S4P($V)
180000970207     C  N35              ADD       1             S3P($V)
180100970207     C   35              ADD       1             S2P($V)
180200970207     C                   ADD       1             S0P($V)
180300970305     C***
180400970305     C* A TERRA DA PARTIRE FUORI AFFIDABILITA'
180500970305     C***
180600970305    4C     WRKNPI        IFGE      0
180700970305     C   34
180800970305     CANN35              ADD       1             FAF($V)
180900970305     C  N34
181000970305     CANN35              ADD       1             F4F($V)
181100970305     C  N35              ADD       1             F3F($V)
181200970305     C  N35              ADD       1             F0F($V)
181300970305    4C                   END
181400970207     c*
181500970207   X3C                   ELSE
181600970207     C***
181700970207     C* A TERRA NON DA PARTIRE
181800970207     C***
181900970207     C   34              ADD       1             SBR($V)
182000970207     C  N34              ADD       1             S4R($V)
182100970207     C                   ADD       1             S3R($V)
182200970207     C                   ADD       1             S0R($V)
182300970207    3C                   ENDIF
182400000000      *
182500040922     c  n37              move      $FTR          satftr
182600040922     c  n37              move      $DTR          satDTR
182700970207     C  N37              UPDATE    FNSAT000
182800000000      *
182900970207    2C                   ENDDO
183000970207    1C  N99              ENDDO
183100000000      *
183200000000     C                   ENDSR
183300000000      **************************************************************************
183400000000      * Calcolo i totali di filiale, li stampo e scrivo sul file
183500000000      **************************************************************************
183600000000     C     TOTFIL        BEGSR
183700000000      *
183800000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
183900000000      *
184000970211     C                   SETON                                        404145
184100000000     C                   Z-ADD     $FILPR        FILIAP
184200000000      *
184300000000      * Decodifico il codice filiale
184400000000     C     $FILPR        CHAIN     AZORG01L                           99
184500000000     C  N99ORGFVA        COMP      *BLANK                             9999
184600940503     C   99              CLEAR                   ORGDEP
184700000000     C  N99              MOVEL     ORGDES        ORGDEP
184800000000      *
184900000000      * ?  C  O   L   L  I  ?
185000970210     C                   XFOOT     CBP           COTOTP
185100970210     C                   XFOOT     CPA           $CDA
185200940428     C                   EXSR      PERCOL
185300000000      *
185400000000      * ?  P  E  S  O  ?
185500970210     C                   XFOOT     PFI           PEF35P
185600970210     C                   XFOOT     PBP           PEO35P
185700970210     C     PEF35P        ADD       PEO35P        PETOTP
185800970210     C                   XFOOT     PPA           $PDA
185900940428     C                   EXSR      PERPES
186000000000      *
186100000000      * ?  V  O  L  U  M E  ?
186200970210     C                   XFOOT     VBP           VOTOTP
186300970210     C                   XFOOT     VPA           $VDA
186400940428     C                   EXSR      PERVOL
186500000000      *
186600000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
186700970210     C                   XFOOT     SPA           $SDA
186800970305     C                   XFOOT     FAF           SPFAFF
186900940502     C                   EXSR      PERSPE
187000970305     C                   XFOOT     SBP           SPTOTP
187100970305     C                   XFOOT     SCM           SPCONP
187200970305     C                   XFOOT     SBR           $COMOD
187300970211     C     SPTOTP        ADD       $COMOD        SPTOTA
187400970211     C     SPTOTA        SUB       SPCONP        SPSPAP
187500000331      *
187600000331      * ? VOLUME A MAGAZZINO?
187700000331     C                   XFOOT     VBR           VOTOTM
187800000000      *
187900970210     C* STAMPO SOLO SE HO DEI DATI
188000000331     C     VOTOTM        IFGT      *ZEROS
188100000417     C* HO RIMESSO ANCHE TEST SU SPTOTA PER FAR FUNZIONARE LE RISTAMPE
188200000417     C* RELATIVE A DATE ELABORATE PRIMA CHE VENISSE FATTO IL CALCOLO
188300000417     C* DEL TOTALE VOLUME A MAGAZZINO
188400000417     C     SPTOTA        ORGT      *ZEROS
188500970210     C*
188600000000     C                   Z-ADD     $FILPR        FILIAP
188700000000     C                   EXSR      SCRIVI
188800940509     C                   ADD       1             $RIGHE            2 0
188900000000      *
189000970210      * ------------------ ESPR./DIST./CORR. --------------------------
189100000000      *
189200970210     C                   SETOFF                                       40
189300000000     C                   DO        3             $V
189400940429     C                   CLEAR                   DESCRP
189500940429      *
189600970210     C                   MOVE      ENS($V)       TIPOP
189700000000      *
189800000000      * ?  C  O   L   L  I  ?
189900970211     C                   Z-ADD     CBP($V)       COTOTP
190000970211     C                   Z-ADD     CPA($V)       $CDA
190100940428     C                   EXSR      PERCOL
190200000000      *
190300000000      * ?  P  E  S  O  ?
190400000000     C                   Z-ADD     PFI($V)       PEF35P
190500970211     C                   Z-ADD     PBP($V)       PEO35P
190600970212     C     PEF35P        ADD       PEO35P        PETOTP
190700970211     C                   Z-ADD     PPA($V)       $PDA
190800940428     C                   EXSR      PERPES
190900000000      *
191000000000      * ?  V  O  L  U  M E  ?
191100970211     C                   Z-ADD     VBP($V)       VOTOTP
191200970211     C                   Z-ADD     VPA($V)       $VDA
191300940428     C                   EXSR      PERVOL
191400000000      *
191500000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
191600970305     C                   Z-ADD     SPA($V)       $SDA
191700970305     C                   Z-ADD     FAF($V)       SPFAFF
191800940502     C                   EXSR      PERSPE
191900970305     C                   Z-ADD     SBP($V)       SPTOTP
192000970305     C                   Z-ADD     SCM($V)       SPCONP
192100970211     C     SBP($V)       ADD       SBR($V)       SPTOTA
192200970211     C     SPTOTA        SUB       SCM($V)       SPSPAP
192300000331      *
192400000331      * ?tot.volume a magazz?
192500000331     C                   Z-ADD     VBR($V)       VOTOTM
192600000000      *
192700000000     C                   EXSR      SCRIVI
192800000000      *
192900000000     C                   ENDDO
193000000000      *
193100950118     C                   WRITE     LSC2T2
193200940506     C                   ENDIF
193300940328      *
193400000417     C                   ENDSR
193500940428      **************************************************************************
193600940428      * Calcolo percentuale sui colli
193700940428      **************************************************************************
193800940428     C     PERCOL        BEGSR
193900940502     C                   Z-ADD     *ZERO         COSDPP
194000940428     C*
194100940502     C* PERCENTUALE SUL DA PARTIRE
194200970213     C  N02$CDA          IFNE      *ZERO
194300970210     C     COTOTP        DIV       $CDA          $COMOD
194400940502     C     $COMOD        MULT(H)   100           COSDPP
194500940502     C                   ENDIF
194600940428     C                   ENDSR
194700940428      **************************************************************************
194800940428      * Calcolo percentuale sul peso
194900940428      **************************************************************************
195000940428     C     PERPES        BEGSR
195100940502     C                   CLEAR                   PESDPP
195200940428     C*
195300940502     C* PERCENTUALE SUL DA PARTIRE
195400970213     C  N02$PDA          IFNE      *ZERO
195500970210     C     PETOTP        DIV       $PDA          $COMOD
195600940502     C     $COMOD        MULT(H)   100           PESDPP
195700940502     C                   ENDIF
195800940428     C                   ENDSR
195900940428      **************************************************************************
196000940428      * Calcolo percentuale sul volume
196100940428      **************************************************************************
196200940428     C     PERVOL        BEGSR
196300940502     C                   Z-ADD     *ZERO         VOSDPP
196400940502     C*
196500940502     C* PERCENTUALE SUL DA PARTIRE
196600970213     C  N02$VDA          IFNE      *ZERO
196700970210     C     VOTOTP        DIV       $VDA          $COMOD
196800940502     C     $COMOD        MULT(H)   100           VOSDPP
196900940502     C                   ENDIF
197000940428     C                   ENDSR
197100940428      **************************************************************************
197200940428      * Calcolo percentuale sulle spedizioni
197300940428      **************************************************************************
197400940428     C     PERSPE        BEGSR
197500940502     C                   Z-ADD     *ZERO         SPSDPP
197600940428     C*
197700940502     C* PERCENTUALE SUL DA PARTIRE
197800970213     C  N02$SDA          IFNE      *ZERO
197900970305     C     SPFAFF        DIV       $SDA          $COMOD
198000940502     C     $COMOD        MULT(H)   100           SPSDPP
198100940502     C                   ENDIF
198200940428     C                   ENDSR
198300000000      **************************************************************************
198400000000      * Azzeramento dei totali di filiale per nuovi conti
198500000000      **************************************************************************
198600000000     C     AZZFIL        BEGSR
198700000412     C                   CLEAR                   VOLUME
198800000000      *
198900000000      * COLLI
199000970211     C                   ADD       CPA(1)        C3A(1)
199100970211     C                   ADD       CPA(2)        C3A(2)
199200970211     C                   ADD       CPA(3)        C3A(3)
199300970211     C                   CLEAR                   CPA
199400970211     C                   CLEAR                   CBP
199500970211     C                   CLEAR                   CBR
199600970211      * PESI
199700970211     C                   ADD       PPA(1)        P3A(1)
199800970211     C                   ADD       PPA(2)        P3A(2)
199900970211     C                   ADD       PPA(3)        P3A(3)
200000970211     C                   CLEAR                   PFI
200100970211     C                   CLEAR                   PPA
200200970211     C                   CLEAR                   PBP
200300970211     C                   CLEAR                   PBR
200400970211     C                   CLEAR                   PFN
200500000000      * VOLUMI
200600970211     C                   ADD       VPA(1)        V3A(1)
200700970211     C                   ADD       VPA(2)        V3A(2)
200800970211     C                   ADD       VPA(3)        V3A(3)
200900970211     C                   CLEAR                   VPA
201000970211     C                   CLEAR                   VBP
201100970211     C                   CLEAR                   VBR
201200970211      * SPEDIZIONI
201300970211     C                   ADD       SPA(1)        S3A(1)
201400970211     C                   ADD       SPA(2)        S3A(2)
201500970211     C                   ADD       SPA(3)        S3A(3)
201600970211     C                   CLEAR                   SPA
201700970211     C                   CLEAR                   SBP
201800970211     C                   CLEAR                   SBR
201900970211     C                   CLEAR                   SCM
202000970305     C                   CLEAR                   FAF
202100000000      *
202200000000     C                   ENDSR
202300000000      **************************************************************************
202400000000      * Azzeramento dei totali di c/servizio per nuova area
202500000000      **************************************************************************
202600000000     C     AZZSER        BEGSR
202700000000      *
202800000000      * COLLI
202900940506     C                   ADD       C1A(1)        C3A(1)
203000970211     C                   ADD       C1A(2)        C3A(2)
203100970211     C                   ADD       C1A(3)        C3A(3)
203200000000     C                   CLEAR                   C1A
203300000000     C                   CLEAR                   C1P
203400000000     C                   CLEAR                   C1R
203500000000      * PESI
203600940506     C                   ADD       P1A(1)        P3A(1)
203700970211     C                   ADD       P1A(2)        P3A(2)
203800970211     C                   ADD       P1A(3)        P3A(3)
203900970211     C                   CLEAR                   P1I
204000970211     C                   CLEAR                   P1N
204100970211     C                   CLEAR                   P1P
204200970211     C                   CLEAR                   P1R
204300970211     C                   CLEAR                   P1A
204400970211      * VOLUMI
204500970211     C                   ADD       V1A(1)        V3A(1)
204600970211     C                   ADD       V1A(2)        V3A(2)
204700970211     C                   ADD       V1A(3)        V3A(3)
204800970211     C                   CLEAR                   V1A
204900970211     C                   CLEAR                   V1P
205000970211     C                   CLEAR                   V1R
205100000000      * SPEDIZIONI
205200940506     C                   ADD       S1A(1)        S3A(1)
205300970211     C                   ADD       S1A(2)        S3A(2)
205400970211     C                   ADD       S1A(3)        S3A(3)
205500970211     C                   CLEAR                   S1A
205600970211     C                   CLEAR                   S1P
205700970211     C                   CLEAR                   S1R
205800970211     C                   CLEAR                   S1M
205900970305     C                   CLEAR                   F1F
206000000000      *
206100000000     C                   ENDSR
206200000000      **************************************************************************
206300000000      * Azzeramento dei totali di c/servizio per nuova filiale
206400000000      **************************************************************************
206500000000     C     AZZSE4        BEGSR
206600000000      *
206700000000      * COLLI
206800970210     C                   CLEAR                   C4A
206900970210     C                   CLEAR                   C4P
207000970210     C                   CLEAR                   C4R
207100000000      * PESI
207200970210     C                   CLEAR                   P4A
207300970210     C                   CLEAR                   P4I
207400970210     C                   CLEAR                   P4P
207500970210     C                   CLEAR                   P4R
207600970210     C                   CLEAR                   P4N
207700000000      * VOLUMI
207800970210     C                   CLEAR                   V4A
207900970210     C                   CLEAR                   V4P
208000970210     C                   CLEAR                   V4R
208100000000      * SPEDIZIONI
208200970210     C                   CLEAR                   S4A
208300970210     C                   CLEAR                   S4P
208400970210     C                   CLEAR                   S4R
208500970210     C                   CLEAR                   S4M
208600970305     C                   CLEAR                   F4F
208700000000      *
208800000000     C                   ENDSR
208900000000      **************************************************************************
209000000000      * Azzeramento dei totali di transito per nuovi totali
209100000000      **************************************************************************
209200000000     C     AZZTRA        BEGSR
209300940506     C* SOMMO NEI TOTALI
209400940506     C                   EXSR      ADDTRA
209500000000      * COLLI
209600940428     C                   CLEAR                   C2P
209700000000      * PESI
209800000000     C                   CLEAR                   P2I
209900940428     C                   CLEAR                   P2P
210000000000      * VOLUMI
210100940428     C                   CLEAR                   V2P
210200000000      * SPEDIZIONI
210300940428     C                   CLEAR                   S2P
210400000000     C                   CLEAR                   S2M
210500000000      *
210600000000     C                   ENDSR
210700000000      **************************************************************************
210800000000      * Stampo i totali per c/servizio
210900000000      **************************************************************************
211000000000     C     TOTSER        BEGSR
211100000000      *
211200000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
211300000000      *
211400970211     C                   SETON                                        4045
211500000000     C                   SETOFF                                       41
211600000000     C                   Z-ADD     *ZERO         FILIAP
211700000000     C                   MOVE      *BLANKS       ORGDEP
211800000000     C     'C/SERVIZ'    CAT       'I':0         ORGDEP
211900000000      *
212000970211      * ?  C  O   L   L  I  ?
212100970211     C                   XFOOT     C1P           COTOTP
212200970211     C                   XFOOT     C1A           $CDA
212300970211     C                   EXSR      PERCOL
212400970211      *
212500970211      * ?  P  E  S  O  ?
212600970211     C                   XFOOT     P1I           PEF35P
212700970211     C                   XFOOT     P1P           PEO35P
212800970211     C     PEF35P        ADD       PEO35P        PETOTP
212900970211     C                   XFOOT     P1A           $PDA
213000940429     C                   EXSR      PERPES
213100000000      *
213200000000      * ?  V  O  L  U  M E  ?
213300970211     C                   XFOOT     V1P           VOTOTP
213400970211     C                   XFOOT     V1A           $VDA
213500940429     C                   EXSR      PERVOL
213600000000      *
213700000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
213800970211     C                   XFOOT     S1A           $SDA
213900970305     C                   XFOOT     F1F           SPFAFF
214000970211     C                   EXSR      PERSPE
214100970305     C                   XFOOT     S1P           SPTOTP
214200970305     C                   XFOOT     S1M           SPCONP
214300970211     C                   XFOOT     S1R           $COMO1
214400970211     C     SPTOTP        ADD       $COMO1        SPTOTA
214500970211     C     SPTOTA        SUB       SPCONP        SPSPAP
214600000331      *
214700000331      * ?tot.volume a magazz?
214800000331     C                   XFOOT     V1R           VOTOTM
214900970211      *
215000970211     C* STAMPO SOLO SE HO DEI DATI
215100000331     C     VOTOTM        IFGT      *ZEROS
215200970211     C*
215300970211     C                   EXSR      SCRIVI
215400000000      *
215500970211      * ------------------ ESPR./DIST./CORR. --------------------------
215600000000      *
215700970211     C                   SETOFF                                       40
215800000000     C                   DO        3             $V
215900940323     C                   MOVE      ENS($V)       TIPOP
216000000000      *
216100000000      * ?  C  O   L   L  I  ?
216200970211     C                   Z-ADD     C1P($V)       COTOTP
216300970211     C                   Z-ADD     C1A($V)       $CDA
216400940429     C                   EXSR      PERCOL
216500000000      *
216600000000      * ?  P  E  S  O  ?
216700000000     C                   Z-ADD     P1I($V)       PEF35P
216800970211     C                   Z-ADD     P1P($V)       PEO35P
216900970211     C     PEF35P        ADD       PEO35P        PETOTP
217000970211     C                   Z-ADD     P1A($V)       $PDA
217100940429     C                   EXSR      PERPES
217200000000      *
217300000000      * ?  V  O  L  U  M E  ?
217400970211     C                   Z-ADD     V1P($V)       VOTOTP
217500970211     C                   Z-ADD     V1A($V)       $VDA
217600940429     C                   EXSR      PERVOL
217700000000      *
217800000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
217900970211     C                   Z-ADD     S1A($V)       $SDA
218000970305     C                   Z-ADD     F1F($V)       SPFAFF
218100940502     C                   EXSR      PERSPE
218200970305     C                   Z-ADD     S1P($V)       SPTOTP
218300970305     C                   Z-ADD     S1M($V)       SPCONP
218400970211     C     S1P($V)       ADD       S1R($V)       SPTOTA
218500970211     C     SPTOTA        SUB       S1M($V)       SPSPAP
218600000331      *
218700000331      * ?tot volume a magazz?
218800000331     C                   Z-ADD     V1R($V)       VOTOTM
218900000000      *
219000000000     C                   EXSR      SCRIVI
219100000000      *
219200000000     C                   ENDDO
219300940504     C                   ENDIF
219400940429     C                   EXSR      AZZSER
219500000000      *
219600000000     C                   ENDSR
219700000000      **************************************************************************
219800000000      * Stampo i totali per transito
219900000000      **************************************************************************
220000000000     C     TOTTRA        BEGSR
220100000000      *
220200000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
220300000000      *
220400940506     C                   SETON                                        400203
220500970211     C                   SETOFF                                       45
220600000000     C                   MOVE      *BLANKS       ORGDEP
220700940502     C* stampa per linea: totale transiti
220800940502     C     *IN43         IFEQ      *ON
220900940502     C                   Z-ADD     *ZERO         FILIAP
221000940502     C                   MOVEL     'TRANSITI'    ORGDEP
221100940502     C                   SETOFF                                       41
221200940502     C                   ELSE
221300940502     c* stampa per terminal in transito
221400940502     C                   SETON                                        41
221500940502      * Decodifico il terminal di arrivo
221600940502     C                   Z-ADD     $FILPR        FILIAP
221700940502     C     $FILPR        CHAIN     AZORG01L                           99
221800940502     C  N99ORGFVA        COMP      *BLANK                             9999
221900940503     C   99              CLEAR                   ORGDEP
222000940502     C  N99              MOVEL     ORGDES        ORGDEP
222100940502     C                   ENDIF
222200000000      *
222300000000      * ?  C  O   L   L  I  ?
222400940502     C                   XFOOT     C2P           COTOTP
222500970211     C*
222600970211      * SOLO SE HO TROVATO ALEMNO UN COLLO A TERRA STAMPO
222700970211     C     COTOTP        IFGT      0
222800940504      *
222900000000      *
223000000000      * ?  P  E  S  O  ?
223100970211     C                   XFOOT     P2I           PEF35P
223200970211     C                   XFOOT     P2P           PEO35P
223300970211     C     PEO35P        ADD       PEF35P        PETOTP
223400000000      *
223500000000      * ?  V  O  L  U  M E  ?
223600940428     C                   XFOOT     V2P           VOTOTP
223700000000      *
223800000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
223900940428     C                   XFOOT     S2P           SPTOTP
224000970211     C                   XFOOT     S2P           SPTOTA
224100970211     C                   XFOOT     S2M           SPCONP
224200970211     C     SPTOTA        SUB       SPCONP        SPSPAP
224300000000      *
224400000000     C                   EXSR      SCRIVI
224500940509     C                   ADD       1             $RIGHE
224600000000      *
224700970211      * ------------------ ESPR./DIST./CORR. --------------------------
224800000000      *
224900940502     C                   SETOFF                                       4045
225000000000     C                   DO        3             $V
225100940502     C* PER TERMINAL SCRIVO CHE E' RECORD DI TRANSITO
225200940429     C  N43$V            IFEQ      1
225300940506     C                   MOVEL     'IN TRANS'    DESCRP
225400940429     C                   ELSE
225500940429     C                   CLEAR                   DESCRP
225600940429     C                   ENDIF
225700940429     C*
225800940323     C                   MOVE      ENS($V)       TIPOP
225900000000      *
226000000000      * ?  C  O   L   L  I  ?
226100940428     C                   Z-ADD     C2P($V)       COTOTP
226200000000      *
226300000000      * ?  P  E  S  O  ?
226400970211     C                   Z-ADD     P2I($V)       PEF35P
226500970211     C                   Z-ADD     P2P($V)       PEO35P
226600970211     C     PEO35P        ADD       PEF35P        PETOTP
226700000000      *
226800000000      * ?  V  O  L  U  M E  ?
226900940428     C                   Z-ADD     V2P($V)       VOTOTP
227000000000      *
227100000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
227200940428     C                   Z-ADD     S2P($V)       SPTOTP
227300970211     C                   Z-ADD     S2P($V)       SPTOTA
227400970211     C                   Z-ADD     S2M($V)       SPCONP
227500970211     C     SPTOTA        SUB       S2M($V)       SPSPAP
227600000000      *
227700000000     C                   EXSR      SCRIVI
227800000000      *
227900000000     C                   ENDDO
228000940428     C*
228100940428     C                   EXSR      AZZTRA
228200940502      *
228300950118     C  N38              WRITE     LSC2T2
228400940506     C                   ENDIF
228500940506     C                   SETOFF                                       0302
228600000000      *
228700000000     C                   ENDSR
228800000000      **************************************************************************
228900970211      * Scrittura del record sul file di trasmissione FNSAT00F
229000000000      **************************************************************************
229100000000     C     WRISAT        BEGSR
229200000000      *
229300970207     C                   CLEAR                   FNSAT000
229400000000     C                   Z-ADD     SIMFEL        SATFLE
229500000000     C                   Z-ADD     $FILPR        SATLNP
229600000000     C                   Z-ADD     *ZERO         SATTFA
229700950210     C                   Z-ADD     DS1ADA        SATDRE
229800000000     C                   MOVE      *BLANK        SATTRE
229900000000      *
230000000000      * COLLI: da partire
230100970210     C                   Z-ADD     CPA(1)        SATCPE
230200970210     C                   Z-ADD     CPA(2)        SATCPD
230300970210     C                   Z-ADD     CPA(3)        SATCPC
230400970207      * COLLI: a terra da partire
230500970207     C                   Z-ADD     CBP(1)        SATCDE
230600970207     C                   Z-ADD     CBP(2)        SATCDD
230700970207     C                   Z-ADD     CBP(3)        SATCDC
230800970207      * COLLI: a terra non da partire
230900970207     C                   Z-ADD     CBR(1)        SATCNE
231000970207     C                   Z-ADD     CBR(2)        SATCND
231100970207     C                   Z-ADD     CBR(3)        SATCNC
231200970207      * PESO: da partire
231300970210     C                   Z-ADD     PPA(1)        SATPPE
231400970210     C                   Z-ADD     PPA(2)        SATPPD
231500970210     C                   Z-ADD     PPA(3)        SATPPC
231600970207      * PESO: a terra da partire fino a 35 quintali
231700970207     C                   Z-ADD     PFI(1)        SATFDE
231800970207     C                   Z-ADD     PFI(2)        SATFDD
231900970207     C                   Z-ADD     PFI(3)        SATFDC
232000970207      * PESO: a terra non da partire fino a 35 q.
232100970207     C                   Z-ADD     PFN(1)        SATFNE
232200970207     C                   Z-ADD     PFN(2)        SATFND
232300970207     C                   Z-ADD     PFN(3)        SATFNC
232400970207     C* PESO: a terra da partire oltre 35 quintali
232500970207     C                   Z-ADD     PBP(1)        SATODE
232600970207     C                   Z-ADD     PBP(2)        SATODD
232700970207     C                   Z-ADD     PBP(3)        SATODC
232800970207      * PESO: a terra non da partire oltre 35 q.
232900970207     C                   Z-ADD     PBR(1)        SATONE
233000970207     C                   Z-ADD     PBR(2)        SATOND
233100970207     C                   Z-ADD     PBR(3)        SATONC
233200000000      * VOLUME: da partire
233300970210     C                   Z-ADD     VPA(1)        SATVPE
233400970210     C                   Z-ADD     VPA(2)        SATVPD
233500970210     C                   Z-ADD     VPA(3)        SATVPC
233600970207      * VOLUME a terra da partire
233700970207     C                   Z-ADD     VBP(1)        SATVDE
233800970207     C                   Z-ADD     VBP(2)        SATVDD
233900970207     C                   Z-ADD     VBP(3)        SATVDC
234000000331      * VOLUME rimasto a magazzino
234100000331      * (totale dei colli a terra da partire, da non partire e colli
234200000331      * di bolle in sospensione)
234300970207     C                   Z-ADD     VBR(1)        SATVNE
234400970207     C                   Z-ADD     VBR(2)        SATVND
234500970207     C                   Z-ADD     VBR(3)        SATVNC
234600940429      * SPEDIZIONI: da partire
234700970210     C                   Z-ADD     SPA(1)        SATSPE
234800970210     C                   Z-ADD     SPA(2)        SATSPD
234900970210     C                   Z-ADD     SPA(3)        SATSPC
235000970207      * SPEDIZIONI a terra da partire
235100970207     C                   Z-ADD     SBP(1)        SATSDE
235200970207     C                   Z-ADD     SBP(2)        SATSDD
235300970207     C                   Z-ADD     SBP(3)        SATSDC
235400970207      * SPEDIZIONI a terra non da partire
235500970207     C                   Z-ADD     SBR(1)        SATSNE
235600970207     C                   Z-ADD     SBR(2)        SATSND
235700970207     C                   Z-ADD     SBR(3)        SATSNC
235800970305      * SPEDIZIONI a terra da partire fuori affidabilità
235900970305     C                   Z-ADD     FAF(1)        SATSFE
236000970305     C                   Z-ADD     FAF(2)        SATSFD
236100970305     C                   Z-ADD     FAF(3)        SATSFC
236200000000      * SPEDIZIONI: complete
236300000000     C                   Z-ADD     SCM(1)        SATSCE
236400970207     C                   Z-ADD     SCM(2)        SATSCD
236500970207     C                   Z-ADD     SCM(3)        SATSCC
236600000000      *
236700970318     C* ASTERISCO LE SEGUENTI IF PERCHE' IL RECORD LO DEVO SEMPRE
236800970318     C* SCRIVERE PER EVITARE CHE NELL'ELABORAZIONE DEL REPORT DI
236900970318     C* SEDE, SE PER UN P.O. IL DA PARTIRE è = 0, VENGA ERRONEAMENTE
237000970318     C* SEGNALATO "DATI NON PERVENUTI".
237100970318     C*          SATSDE    IFGT *ZERO
237200970318     C*          SATSDD    ORGT *ZERO
237300970318     C*          SATSDC    ORGT *ZERO
237400970318     C*          SATSNE    ORGT *ZERO
237500970318     C*          SATSND    ORGT *ZERO
237600970318     C*          SATSNC    ORGT *ZERO
237700970318     C*          SATSPE    ORGT *ZEROS
237800970318     C*          SATSPD    ORGT *ZEROS
237900970318     C*          SATSPC    ORGT *ZEROS
238000940323      *
238100940428     C                   MOVE      $FTR          SATFTR
238200970207     C                   MOVE      $DTR          SATDTR
238300940323      *
238400970207     C                   WRITE     FNSAT000
238500970318     C*
238600970318     C* LE IF SOPRA ASTERISCATE LE MANTENGO PERò PER IMPOSTARE $PRIM4
238700970318     C*
238800970318     C     SATSDE        IFGT      *ZERO
238900970318     C     SATSDD        ORGT      *ZERO
239000970318     C     SATSDC        ORGT      *ZERO
239100970318     C     SATSNE        ORGT      *ZERO
239200970318     C     SATSND        ORGT      *ZERO
239300970318     C     SATSNC        ORGT      *ZERO
239400970318     C     SATSPE        ORGT      *ZEROS
239500970318     C     SATSPD        ORGT      *ZEROS
239600970318     C     SATSPC        ORGT      *ZEROS
239700000405     C     SATVNE        ORGT      *ZEROS
239800000405     C     SATVND        ORGT      *ZEROS
239900000405     C     SATVNC        ORGT      *ZEROS
240000940429     C* PER LA STAMPA DEI TOTALI DI AREA
240100940429     C                   Z-ADD     1             $PRIM4
240200970318     C                   ENDIF
240300000000      *
240400000000     C                   ENDSR
240500000000      **************************************************************************
240600970211      * Scrittura del record c/servizio sul file di trasmissione FNSAT00F
240700000000      **************************************************************************
240800000000     C     WR1SAT        BEGSR
240900000000      *
241000970207     C                   CLEAR                   FNSAT000
241100000000     C                   Z-ADD     SIMFEL        SATFLE
241200000000     C                   Z-ADD     $FILPR        SATLNP
241300000000     C                   Z-ADD     *ZERO         SATTFA
241400950210     C                   Z-ADD     DS1ADA        SATDRE
241500000000     C                   MOVE      'C'           SATTRE
241600000000      *
241700000000      * COLLI: da partire
241800970210     C                   Z-ADD     C4A(1)        SATCPE
241900970210     C                   Z-ADD     C4A(2)        SATCPD
242000970210     C                   Z-ADD     C4A(3)        SATCPC
242100970207      * COLLI: a terra da partire
242200970207     C                   Z-ADD     C4P(1)        SATCDE
242300970207     C                   Z-ADD     C4P(2)        SATCDD
242400970207     C                   Z-ADD     C4P(3)        SATCDC
242500970207      * COLLI: a terra non da partire
242600970207     C                   Z-ADD     C4R(1)        SATCNE
242700970207     C                   Z-ADD     C4R(2)        SATCND
242800970207     C                   Z-ADD     C4R(3)        SATCNC
242900970207      * PESO: da partire
243000970210     C                   Z-ADD     P4A(1)        SATPPE
243100970210     C                   Z-ADD     P4A(2)        SATPPD
243200970210     C                   Z-ADD     P4A(3)        SATPPC
243300970207      * PESO: a terra da partire fino a 35 quintali
243400970207     C                   Z-ADD     P4I(1)        SATFDE
243500970207     C                   Z-ADD     P4I(2)        SATFDD
243600970207     C                   Z-ADD     P4I(3)        SATFDC
243700970207      * PESO: a terra non da partire fino a 35 quintali
243800970207     C                   Z-ADD     P4N(1)        SATFNE
243900970207     C                   Z-ADD     P4N(2)        SATFND
244000970207     C                   Z-ADD     P4N(3)        SATFNC
244100970207      * PESO: a terra da partire oltre 35 quintali
244200970207     C                   Z-ADD     P4P(1)        SATODE
244300970207     C                   Z-ADD     P4P(2)        SATODD
244400970207     C                   Z-ADD     P4P(3)        SATODC
244500970207      * PESO: a terra non da partire oltre 35 quintali
244600970207     C                   Z-ADD     P4R(1)        SATONE
244700970207     C                   Z-ADD     P4R(2)        SATOND
244800970207     C                   Z-ADD     P4R(3)        SATONC
244900000000      * VOLUME: da partire
245000970210     C                   Z-ADD     V4A(1)        SATVPE
245100970210     C                   Z-ADD     V4A(2)        SATVPD
245200970210     C                   Z-ADD     V4A(3)        SATVPC
245300970207      * VOLUME: a terra da partire
245400970207     C                   Z-ADD     V4P(1)        SATVDE
245500970207     C                   Z-ADD     V4P(2)        SATVDD
245600970207     C                   Z-ADD     V4P(3)        SATVDC
245700000331      * VOLUME: totale rimasto a magazzino
245800000331      * (a terra da partire, da non partire, in sospensione)
245900970207     C                   Z-ADD     V4R(1)        SATVNE
246000970207     C                   Z-ADD     V4R(2)        SATVND
246100970207     C                   Z-ADD     V4R(3)        SATVNC
246200970207      * SPEDIZIONI: da partire
246300970210     C                   Z-ADD     S4A(1)        SATSPE
246400970210     C                   Z-ADD     S4A(2)        SATSPD
246500970210     C                   Z-ADD     S4A(3)        SATSPC
246600970207      * SPEDIZIONI: a terra da partire
246700970207     C                   Z-ADD     S4P(1)        SATSDE
246800970207     C                   Z-ADD     S4P(2)        SATSDD
246900970207     C                   Z-ADD     S4P(3)        SATSDC
247000970207      * SPEDIZIONI: a terra non da partire
247100970207     C                   Z-ADD     S4R(1)        SATSNE
247200970207     C                   Z-ADD     S4R(2)        SATSND
247300970207     C                   Z-ADD     S4R(3)        SATSNC
247400970305      * SPEDIZIONI: a terra da partire fuori affidabilità
247500970305     C                   Z-ADD     F4F(1)        SATSFE
247600970305     C                   Z-ADD     F4F(2)        SATSFD
247700970305     C                   Z-ADD     F4F(3)        SATSFC
247800000000      * SPEDIZIONI: complete
247900000000     C                   Z-ADD     S4M(1)        SATSCE
248000970207     C                   Z-ADD     S4M(2)        SATSCD
248100970207     C                   Z-ADD     S4M(3)        SATSCC
248200000000      *
248300970318     C     SATSDE        IFGT      *ZERO
248400970318     C     SATSDD        ORGT      *ZERO
248500970318     C     SATSDC        ORGT      *ZERO
248600970318     C     SATSNE        ORGT      *ZERO
248700970318     C     SATSND        ORGT      *ZERO
248800970318     C     SATSNC        ORGT      *ZERO
248900970318     C     SATSPE        ORGT      *ZERO
249000970318     C     SATSPD        ORGT      *ZERO
249100970318     C     SATSPC        ORGT      *ZERO
249200000405     C     SATVNE        ORGT      *ZEROS
249300000405     C     SATVND        ORGT      *ZEROS
249400000405     C     SATVNC        ORGT      *ZEROS
249500940323      *
249600940428     C                   MOVE      $FTR          SATFTR
249700970207     C                   MOVE      $DTR          SATDTR
249800940323      *
249900970207     C                   WRITE     FNSAT000
250000940429     C* PER LA STAMPA DEI C/S
250100940429     C                   Z-ADD     1             $PRIM2
250200940429     C* PER LA STAMPA DEI TOTALI DI AREA
250300940429     C                   Z-ADD     1             $PRIM4
250400000000     C                   ENDIF
250500000000      *
250600000000      * Aggiorno le schiere per i totali c/servizio per area
250700000000     C                   DO        3             $U                1 0
250800970210     C* A TERRA
250900000000     C                   ADD       C4P($U)       C1P($U)
251000000000     C                   ADD       C4R($U)       C1R($U)
251100000000     C                   ADD       P4I($U)       P1I($U)
251200970207     C                   ADD       P4N($U)       P1N($U)
251300000000     C                   ADD       P4P($U)       P1P($U)
251400000000     C                   ADD       P4R($U)       P1R($U)
251500000000     C                   ADD       V4P($U)       V1P($U)
251600000331     C                   ADD       V4R($U)       V1R($U)
251700000000     C                   ADD       S4P($U)       S1P($U)
251800000000     C                   ADD       S4R($U)       S1R($U)
251900000000     C                   ADD       S4M($U)       S1M($U)
252000970305     C                   ADD       F4F($U)       F1F($U)
252100970210     C* DA PARTIRE
252200970210     C                   ADD       C4A($U)       C1A($U)
252300970210     C                   ADD       P4A($U)       P1A($U)
252400970210     C                   ADD       V4A($U)       V1A($U)
252500970210     C                   ADD       S4A($U)       S1A($U)
252600000000     C                   ENDDO
252700000000      *
252800000000     C                   EXSR      AZZSE4
252900000000      *
253000000000     C                   ENDSR
253100000000      **************************************************************************
253200970211      * Scrittura del record transito sul file di trasmissione FNSAT00F
253300000000      **************************************************************************
253400000000     C     WR2SAT        BEGSR
253500000000      *
253600970211     C                   CLEAR                   FNSAT000
253700000000     C                   Z-ADD     SIMFEL        SATFLE
253800940502     C                   Z-ADD     SIMFEL        SATLNP
253900000000     C                   Z-ADD     *ZERO         SATTFA
254000950210     C                   Z-ADD     DS1ADA        SATDRE
254100000000     C                   MOVE      'T'           SATTRE
254200000000      *
254300000000      * COLLI: da partire
254400970211     C                   Z-ADD     0             SATCPE
254500970211     C                   Z-ADD     0             SATCPD
254600970211     C                   Z-ADD     0             SATCPC
254700970211      * COLLI:
254800970211     C                   Z-ADD     C2P(1)        SATCDE
254900970211     C                   Z-ADD     C2P(2)        SATCDD
255000970211     C                   Z-ADD     C2P(3)        SATCDC
255100970211      * PESO: da partire
255200970211     C                   Z-ADD     0             SATPPE
255300970211     C                   Z-ADD     0             SATPPD
255400970211     C                   Z-ADD     0             SATPPC
255500000000      * PESO: fino a 35 quintali
255600970211     C                   Z-ADD     P2I(1)        SATFDE
255700970211     C                   Z-ADD     P2I(2)        SATFDD
255800970211     C                   Z-ADD     P2I(3)        SATFDC
255900940428      * PESO: (non esiste il bollettato nel periodo o nel precedente)
256000970211     C                   Z-ADD     P2P(1)        SATODE
256100970211     C                   Z-ADD     P2P(2)        SATODD
256200970211     C                   Z-ADD     P2P(3)        SATODC
256300000000      * VOLUME: da partire
256400970211     C                   Z-ADD     0             SATVPE
256500970211     C                   Z-ADD     0             SATVPD
256600970211     C                   Z-ADD     0             SATVPC
256700970211      * VOLUME
256800970211     C                   Z-ADD     V2P(1)        SATVDE
256900970211     C                   Z-ADD     V2P(2)        SATVDD
257000970211     C                   Z-ADD     V2P(3)        SATVDC
257100000000      * SPEDIZIONI: da partire
257200970211     C                   Z-ADD     0             SATSPE
257300970211     C                   Z-ADD     0             SATSPD
257400970211     C                   Z-ADD     0             SATSPC
257500970211      * SPEDIZIONI
257600970211     C                   Z-ADD     S2P(1)        SATSDE
257700970211     C                   Z-ADD     S2P(2)        SATSDD
257800970211     C                   Z-ADD     S2P(3)        SATSDC
257900000000      * SPEDIZIONI: complete
258000940509     C                   Z-ADD     S2M(1)        SATSCE
258100970211     C                   Z-ADD     S2M(2)        SATSCD
258200970211     C                   Z-ADD     S2M(3)        SATSCC
258300000000      *
258400970212     C     SATSDE        IFGT      *ZERO
258500970212     C     SATSDD        ORGT      *ZERO
258600970212     C     SATSDC        ORGT      *ZERO
258700940428     C                   MOVE      $FTR          SATFTR
258800970207     C                   MOVE      $DTR          SATDTR
258900940323      *
259000970211     C                   WRITE     FNSAT000
259100940429     C* PER LA STAMPA DEI TOTALI DI AREA
259200940429     C                   Z-ADD     1             $PRIM4
259300940505     C                   Z-ADD     1             $PRIM3
259400000000     C                   ENDIF
259500000000      *
259600000000     C                   ENDSR
259700940428      **************************************************************************
259800940428      * Stampo i totali generali
259900940428      **************************************************************************
260000940428     C     TOTGEN        BEGSR
260100940428      *
260200940428      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
260300940428      *
260400940506      * stampo i totali generali finali solo se ho stampato i transiti
260500950628    1C     *IN02         IFEQ      *ON
260600940506     C     $PRIM3        ANDEQ     1
260700940506     C* I TOTALI DI AREA SEMPRE
260800940506     C     *IN02         OREQ      *OFF
260900940506     C*
261000940428     C                   SETON                                        40
261100940428     C                   SETOFF                                       41
261200970211     C  N02              SETON                                        45
261300970211     C   02              SETOFF                                       45
261400940428     C                   Z-ADD     *ZERO         FILIAP
261500940428     C                   MOVE      *BLANKS       ORGDEP
261600940428     C   02'TOT.GENE'    CAT       'RALE':0      ORGDEP
261700940428     C  N02              MOVEL     'TOTALE'      ORGDEP
261800940428     C*
261900950118     C                   WRITE     LSC2T3
262000940428      *
262100940428      * ?  C  O   L   L  I  ?
262200970211     C                   XFOOT     C3P           COTOTP
262300970211     C                   XFOOT     C3A           $CDA
262400940428     C                   EXSR      PERCOL
262500940428      *
262600940428      * ?  P  E  S  O  ?
262700970211     C                   XFOOT     P3I           PEF35P
262800970211     C                   XFOOT     P3P           PEO35P
262900970211     C     PEF35P        ADD       PEO35P        PETOTP
263000970228     C                   XFOOT     P3A           $PDA
263100970211     C                   EXSR      PERPES
263200970211      *
263300970211      * ?  V  O  L  U  M E  ?
263400970211     C                   XFOOT     V3P           VOTOTP
263500970211     C                   XFOOT     V3A           $VDA
263600940428     C                   EXSR      PERVOL
263700940428      *
263800940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
263900970211     C                   XFOOT     S3A           $SDA
264000970305     C                   XFOOT     F3F           SPFAFF
264100940502     C                   EXSR      PERSPE
264200970305     C                   XFOOT     S3P           SPTOTP
264300970305     C                   XFOOT     S3M           SPCONP
264400970211     C                   XFOOT     S3R           $COMO1
264500970211     C     SPTOTP        ADD       $COMO1        SPTOTA
264600970211     C     SPTOTA        SUB       SPCONP        SPSPAP
264700000407     C*
264800000407      * ?VOLUME A MAGAZZINO ?
264900000407     C                   XFOOT     V3R           VOTOTM
265000940428      *
265100940428     C                   EXSR      SCRIVI
265200940509     C                   ADD       1             $RIGHE
265300940428      *
265400970211      * ------------------ ESPR./DIST./CORR. --------------------------
265500940428      *
265600970211     C                   SETOFF                                       40
265700950628    2C                   DO        3             $V
265800940428     C                   MOVE      ENS($V)       TIPOP
265900940428      *
266000940428      * ?  C  O   L   L  I  ?
266100970211     C                   Z-ADD     C3P($V)       COTOTP
266200970211     C                   Z-ADD     C3A($V)       $CDA
266300940428     C                   EXSR      PERCOL
266400940428      *
266500940428      * ?  P  E  S  O  ?
266600940428     C                   Z-ADD     P3I($V)       PEF35P
266700970211     C                   Z-ADD     P3P($V)       PEO35P
266800970211     C     PEF35P        ADD       PEO35P        PETOTP
266900970211     C                   Z-ADD     P3A($V)       $PDA
267000940428     C                   EXSR      PERPES
267100940428      *
267200940428      * ?  V  O  L  U  M E  ?
267300970211     C                   Z-ADD     V3P($V)       VOTOTP
267400970211     C                   Z-ADD     V3A($V)       $VDA
267500940428     C                   EXSR      PERVOL
267600940428      *
267700940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
267800970211     C                   Z-ADD     S3A($V)       $SDA
267900970305     C                   Z-ADD     F3F($V)       SPFAFF
268000940502     C                   EXSR      PERSPE
268100970305     C                   Z-ADD     S3P($V)       SPTOTP
268200970305     C                   Z-ADD     S3M($V)       SPCONP
268300970211     C     S3P($V)       ADD       S3R($V)       SPTOTA
268400970211     C     SPTOTA        SUB       S3M($V)       SPSPAP
268500000406      * ?VOLUME A MAGAZZINO ?
268600000406     C                   Z-ADD     V3R($V)       VOTOTM
268700940428      *
268800940428     C                   EXSR      SCRIVI
268900940428      *
269000950628    2C                   ENDDO
269100940428      *
269200950118     C  N02              WRITE     LSC2T2
269300950628    1C                   ENDIF
269400940429     C*
269500950118     C   02              WRITE     LSC2T3
269600940428     C*
269700940428     C   02              EXSR      AZZTOT
269800940428      *
269900940428     C                   ENDSR
270000000000      **************************************************************************
270100000000      * Elaborazione transiti
270200940505      * Non posso leggere per data uscita dal transito perche' spesso
270300940505      * capita che ci siano bolle con data entrata un giorno e data
270400940505      * uscita il giorno prima: per questo elaboro tutte le bolle
270500950210      * che hanno la data di entrata al transito > 0 e <=a DS1AAL
270600000000      **************************************************************************
270700000000     C     ELATRA        BEGSR
270800000000      *
270900940505     C                   Z-ADD     0             $PRIM1
271000090304     C                   MOVE      *BLANK        $artPR
271100940429     C                   MOVE      *BLANK        $CBLPR
271200940428     C                   MOVE      'T'           $FLAG
271300000000      *
271400000000      * Leggo il file delle bolle in transito
271500940505     C                   Z-ADD     1             $DFV
271600020625     C     kbtt39        SETLL     FNBTT39L
271700020625     C     simfel        READe     FNBTT39L                               33
271800940428     C*
271900940428    2C     *IN33         DOWEQ     *OFF
272000950210     C     BTTDET        ANDLE     DS1AAL
272100000000      *
272200940505     C                   EXSR      ESCLU1
272300940428      *
272400940505    3C     $OK           IFEQ      'S'
272500940505     C                   Z-ADD     1             $PRIM1
272600000000      *
272700000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
272800000000      * poiche' lo dovro aggiornare
272900970211     C                   CLEAR                   FNSAT000
273000000000     C                   Z-ADD     SIMFEL        SATFLE
273100950210     C                   Z-ADD     DS1ADA        SATDRE
273200940502     C                   Z-ADD     SIMFEL        SATLNP
273300950118     C                   Z-ADD     BTPTFA        SATTFA
273400000000     C                   MOVE      'T'           SATTRE
273500970205     C     FNSA1K        CHAIN     FNSAT02L                           37
273600940428      *
273700021217      * se è la prima volta che leggo la bolla carico i dati: VOL e PKG
273800021217    4C     $BOLLA        IFEQ      '1'
273900021217     C                   EXSR      DATBOT
274000021217    4C                   ENDIF
274100021217      *
274200021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
274300030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
274400030213      * Peso medio
274500030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
274600030213if  1c***                if        Wsipes = 'S'
274700030213     c***                z-add     BTTpuc        $Peso
274800030213   x1c***                else
274900021220     c                   z-add     Wpesmed       $Peso
275000030213e   1c***                endif
275100021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
275200030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
275300030213      * Volume medio
275400030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
275500030213if  1c***                if        Wsivol = 'S'
275600030213     c***                z-add     BTTvuc        $Volum
275700030213   x1c***                else
275800021220     c                   z-add     Wvolmed       $Volum
275900030213e   1c***                endif
276000000000      *
276100970211      * Verifico il tipo spedizione ESPR./DIST./CORR.
276200950201     C                   Z-ADD     3             $T
276300950118    4C     BTPTSP        IFEQ      'E'
276400000000     C                   Z-ADD     1             $T
276500940428    4C                   ENDIF
276600950201    4C     BTPTSP        IFEQ      'D'
276700940323     C                   Z-ADD     2             $T
276800940428    4C                   ENDIF
276900000000      *
277000000000      * Ho due controlli da fare per sapere se il collo e' a terra:
277100940428      *    1)  BTTDUT = 0
277200940428      *    2)  BTTDUT > Data spedizione
277300000000     C                   SETOFF                                       36
277400940428     C     BTTDUT        IFEQ      0
277500950210     C     BTTDUT        ORGT      DS1AAL
277600000000     C                   SETON                                        36
277700000000     C                   ENDIF
277800940506      *
277900000000      * Sommo solo se il collo risulta a terra
278000940428    4C     *IN36         IFEQ      *ON
278100940506      *
278200940506      * ?  CALCOLI  SUI  COLLI  ?
278300940506      *
278400940509     C                   ADD       1             C2P($T)
278500940428     C                   ADD       1             C0P($T)
278600000000      *
278700000000      * ?  CALCOLI  SUL  PESO  ?
278800000000      *
278900000000      * Verifico il peso di 35 quintali
279000021217    5***C     BTPPKB        IFLE      3500
279100021217    5C     Wpeso         IFLE      3500
279200940509     C                   ADD       $PESO         P2I($T)
279300000000     C                   ADD       $PESO         P0I($T)
279400970211     C                   ELSE
279500970211      * Oltre 35 quintali:                                             (35ON)
279600970211     C                   ADD       $PESO         P2P($T)
279700970211     C                   ADD       $PESO         P0P($T)
279800940428    5C                   ENDIF
279900000000      *
280000000000      *
280100000000      * ?  CALCOLI  SUL  VOLUME  ?
280200000000      *
280300021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
280400021217     **5C     BTTVUC        IFEQ      *ZERO
280500021217     *** *
280600021217     ***C     BTPVLB        DIV(H)    BTPNCL        $VOLUM
280700021217     *** *
280800021217     **5C                   ELSE
280900021217     ***C                   Z-ADD     BTTVUC        $VOLUM
281000021217     **5C                   ENDIF
281100000000      *
281200940509     C                   ADD       $VOLUM        V2P($T)
281300940428     C                   ADD       $VOLUM        V0P($T)
281400000000      *
281500000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
281600000000      *
281700000000      * Visto che le spedizioni non riesco a calcolarle ora, mi limito a tenere
281800000000      * aggiornato il file di work con le bolle
281900950118     C                   Z-ADD     BTPLNP        WRKLNP
282000000000     C                   MOVE      *BLANK        WRKCSE
282100970409     C     FNWR2K        CHAIN     FNWRK00F                           99
282200940428    5C     *IN99         IFEQ      *OFF
282300000000     C                   ADD       1             WRKNC1
282400000000     C                   EXCEPT    AGGIO
282500940428   X5C                   ELSE
282600950118     C                   MOVEL     'T'           WTRANS
282700940503     C                   EXSR      WRIWRK
282800940428    5C                   ENDIF
282900940428      *
283000040921     C                   MOVE      $FTR          SATFTR
283100040921     C                   MOVE      $DTR          SATDTR
283200940428    5C     *IN37         IFEQ      *ON
283300040921     C***                MOVE      $FTR          SATFTR
283400040921     C***                MOVE      $DTR          SATDTR
283500970211     C                   WRITE     FNSAT000
283600940428   X5C                   ELSE
283700970211     C                   UPDATE    FNSAT000
283800940428    5C                   ENDIF
283900000000      *
284000940428    4C                   ENDIF
284100940428    3C                   ENDIF
284200000000      *
284300020625     C     simfel        READE     FNBTT39L                               33
284400940428    2C                   ENDDO
284500940506     C*
284600000000     C     $PRIM1        IFNE      *ZERO
284700940506     C                   EXSR      CALSPE
284800000000     C                   EXSR      WR2SAT
284900000000     C                   ENDIF
285000000000      *
285100000000     C                   ENDSR
285200940503      **************************************************************************
285300000411      * elimino i terminal che non hanno VOLUMI TOT A MAG>0
285400940503      **************************************************************************
285500940503     C     ELATER        BEGSR
285600950210     C                   Z-ADD     DS1FEL        $AREA
285700940503     C                   CLEAR                   $TFA
285800940503     C                   SETOFF                                       33
285900940503     C*
286000940503     C* ELABORO TERMINAL PER TERMINAL
286100940503     C                   EXSR      IMPTFA
286200940503     C*
286300940503    2C     *IN33         DOWEQ     *OFF
286400940503     C                   Z-ADD     *ZERO         $PRIM1
286500940503      *
286600970211     C     TASATK        READE(N)  FNSAT03L                               33
286700940503     C                   MOVEL     SATTRE        $TRE
286800940503     C*
286900940503    3C     *IN33         DOWEQ     *OFF
287000940503     C*
287100940503    4C     $TRE          IFNE      SATTRE
287200940503     C     $PRIM1        ANDEQ     0
287300970211     C     TRSATK        SETLL     FNSAT03L
287400970211     C     TRSATK        READE     FNSAT03L                               33
287500940503     C*
287600940503    5C     *IN33         DOWEQ     *OFF
287700970211     C                   DELETE    FNSAT003
287800970211     C     TRSATK        READE     FNSAT03L                               33
287900940503    5C                   ENDDO
288000940503     C                   MOVEL     SATTRE        $TRE
288100940503     C                   CLEAR                   $PRIM1
288200940503    4C                   ENDIF
288300940503     C*
288400000411     C* VEDO SE C'E' ALMENO UN VOLUME A MAGAZZINO
288500000411    4C     SATVNE        IFGT      0
288600000411     C     SATVND        ORGT      0
288700000411     C     SATVNC        ORGT      0
288800940503     C                   Z-ADD     1             $PRIM1
288900940503    4C                   ENDIF
289000970211     C     TASATK        READE(N)  FNSAT03L                               33
289100940503    3C                   ENDDO
289200940503     C* ELABORO L'ULTIMO
289300940503    3C     $PRIM1        IFEQ      0
289400970211     C     TRSATK        SETLL     FNSAT03L
289500970211     C     TRSATK        READE     FNSAT03L                               33
289600970211     C*
289700940503    4C     *IN33         DOWEQ     *OFF
289800970211     C                   DELETE    FNSAT003
289900970211     C     TRSATK        READE     FNSAT03L                               33
290000940503    4C                   ENDDO
290100940503    3C                   ENDIF
290200940503     C*
290300940503     C                   EXSR      IMPTFA
290400940503    2C                   ENDDO
290500940504     C*
290600940504     C                   ENDSR
290700000000      **************************************************************************
290800000000      * Stampo i terminal di arrivo
290900000000      **************************************************************************
291000000000     C     STATER        BEGSR
291100940429     C                   SETOFF                                       4333
291200940429     C                   SETON                                        30
291300950210     C                   Z-ADD     DS1FEL        $AREA
291400940429     C                   CLEAR                   $TFA
291500940509     C                   CLEAR                   $RIGHE
291600940429     C* ELABORO AREA PER AREA
291700940429     C  N39              EXSR      IMPARE
291800940429     C*
291900940429    2C     *IN33         DOWEQ     *OFF
292000940429     C*
292100940429     C* ELABORO TERMINAL PER TERMINAL
292200940429     C                   EXSR      IMPTFA
292300000000      *
292400940429    3C     *IN33         DOWEQ     *OFF
292500940429     C                   Z-ADD     *ZERO         $PRIM1
292600940429     C                   Z-ADD     *ZERO         $PRIM3
292700000000      *
292800970211     C     ARSATK        READE(N)  FNSAT03L                               33
292900940429     C*
293000940429    4C     *IN33         DOWEQ     *OFF
293100000000      *
293200940429     C* ELABORO SE DATA COMPRESA
293300940429     C* E, SE IMMESSA SOLTANTO UNA LINEA, SE E' LA LINEA RICHIESTA
293400970211    5C     SATDRE        IFLE      DS1AAL
293500940429     C*
293600940429     C* SE DA STAMPE GIORNALIERE ELABORO SLTANTO LE LINEE IN LIN
293700940429     C     SATLNP        LOOKUP    LIN                                    99
293800940429     C   99              EXSR      SERTRA
293900940429     C*
294000940429   X5C                   ELSE
294100940429     C                   SETON                                        33
294200940429    5C                   ENDIF
294300940429     C*
294400970211     C     ARSATK        READE(N)  FNSAT03L                               33
294500940429    4C                   ENDDO
294600940429      *
294700940429     C* Stampo le bolle dell'area
294800940429    4C     $PRIM1        IFEQ      1
294900940429     C                   EXSR      TOTFIL
295000940429     C                   EXSR      AZZFIL
295100940429    4C                   ENDIF
295200940429      *
295300940429      * Stampo le bolle in transito
295400940429    4C     $PRIM3        IFEQ      1
295500940429     C                   EXSR      TOTTRA
295600940429    4C                   ENDIF
295700940429     C*
295800940429     C* Elaboro nuovo terminal
295900940429     C                   EXSR      IMPTFA
296000940429    3C                   ENDDO
296100940429      *
296200940509     C                   Z-ADD     1             $PRIM3
296300970226     C                   SETON                                        0204
296400940429     C                   EXSR      TOTGEN
296500970226     C                   SETOFF                                       0204
296600940429     C* ELABORO NUOVA AREA
296700940429     C  N39              EXSR      IMPARE
296800940429    2C                   ENDDO
296900940429     C*
297000940429     C                   ENDSR
297100000000      **************************************************************************
297200000000      * Una volta letto il record verifico se da includere o no
297300000000      *
297400940428      * Includo se:
297500940428      * e' entrato al transito
297600940428      *  e non e' ancora uscito o e' uscito con data = > a  data elabor
297700940505      *            BTTDUT = 0
297800000000      *   oppure:
297900950210      *       AND  BTTDUT >= DS1ADAL
298000940428      *
298100940428      * escludo sempre i recuperi
298200000000      **************************************************************************
298300000000     C     ESCLU1        BEGSR
298400000000      *
298500000000     C                   MOVE      'N'           $OK
298600000000      *
298700000000     C     BTTDUT        IFEQ      *ZERO
298800950210     C     BTTDUT        ORGE      DS1AAL
298900000000     C                   MOVE      'S'           $OK
299000940428     C                   ENDIF
299100000000      *
299200000000      * Solo se superati i test ne eseguo altri
299300000000     C     $OK           IFEQ      'S'
299400940505      *
299500940505      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
299600940505      * come test ma mi permette di saltare delle chain)
299700090304    3C     $BTTAT        IFNE      $artPR
299800020625     C     FNBTPK        CHAIN     FNBTP11L                           99
299900090304     C  N99              MOVE      $BTTAT        $artPR
300000940505     C  N99              MOVEL     '1'           $BOLLA
300100940505     C   99              MOVEL     'N'           $OK
300200940505    3C                   ENDIF
300300000000      *
300400000000      * Eseguo la chain su TABEL solo se diverso dal precedente
300500950118     C  N99BTPCBO        IFNE      $CBLPR
300600950210     C                   MOVE      '3A'          KCOD
300700950210     C                   MOVE      *BLANKS       KKEY
300800950210     C                   MOVEL     BTPCBO        KKEY
300900950210     C     KTAB1         CHAIN     TABEL00F                           99
301000000000     C  N99              MOVEL     TBLUNI        DS3A
301100950118     C  N99              MOVE      BTPCBO        $CBLPR
301200000000     C   99              CLEAR                   DS3A
301300000000     C                   ENDIF
301400000000      *
301500000000      * Non considero i recuperi
301600940428     C     §3ARBL        IFEQ      'R'
301700000000     C                   MOVE      'N'           $OK
301800000000     C                   ENDIF
301900000000      *
302000000000     C                   ENDIF
302100000000      *
302200000000     C                   ENDSR
302300000000      **************************************************************************
302400970211      * RISTAMPA: lettura dal file statistiche (FNSAT00F) e output
302500000000      **************************************************************************
302600000000     C     RISTAM        BEGSR
302700000000      *
302800940502     C                   SETOFF                                       33
302900940428     C                   SETON                                        43
303000940429     C                   CLEAR                   $TFA
303100940509     C                   CLEAR                   $RIGHE
303200950210     C                   MOVEL     DS1FEL        $AREA
303300940429     C* LANCIO DA FILTRO
303400950210    1C     DS1FGS        IFGT      0
303500950210     C                   MOVEL     DS1LNP        SATLNP
303600940429     C*
303700940429     C* SE NON IMMESSA L'AREA LA CERCO
303800940429     C  N39              EXSR      IMPARE
303900940429     C*
304000940429    2C     *IN33         DOWEQ     *OFF
304100940429     C                   Z-ADD     *ZERO         $PRIM2
304200940429     C                   Z-ADD     *ZERO         $PRIM3
304300940429     C                   Z-ADD     *ZERO         $PRIM4
304400940429     C*
304500940429     C* SE NON IMMESSA LA LNP LA CERCO
304600940505     C                   EXSR      IMPLNP
304700000000      *
304800940429    3C     *IN33         DOWEQ     *OFF
304900940429     C                   Z-ADD     *ZERO         $PRIM1
305000940429     C* LEGGO IL FILE PER LNP
305100940429     C                   EXSR      LEGSAT
305200940429     C* cerco la nuova lnp
305300940429     C  N29              EXSR      IMPLNP
305400940429    3C                   ENDDO
305500940429     c*
305600940429     C* stampa finale dei C/S transiti e totale
305700940429     C                   EXSR      STAFIN
305800940429     C* cerco la nuova area
305900940429     C  N39              EXSR      IMPARE
306000940429    2C                   ENDDO
306100000000      *
306200950210   X1C                   ELSE
306300940429      *
306400940429      * Da stampe giornaliere
306500940429     C                   Z-ADD     1             $X
306600940429     C*
306700940506     C                   Z-ADD     *ZERO         $PRIM2            1 0
306800940506     C                   Z-ADD     *ZERO         $PRIM3            1 0
306900940506     C                   Z-ADD     *ZERO         $PRIM4            1 0
307000000912     C                   CLEAR                   WSTAF
307100940429    2C     LIN($X)       DOWGT     0
307200940429     C                   Z-ADD     LIN($X)       $LNP
307300940506     C                   Z-ADD     LIN($X)       $FILPR
307400940505     C                   Z-ADD     *ZERO         $PRIM1            1 0
307500000912     C* ESCLCUDO I P.O. POSTE
307600000912      * Escludo bolle LNP Poste
307700020813     ***C                   CLEAR                   §OGPT
307800020813     C                   clear                   OG143
307900000912     C     $FILPR        CHAIN     AZORG01L                           82
308000000912     C  N82              MOVEL     ORGDE3        OG143
308100000912     C**
308200020813   2A***C     §OGPT         IFNE      'S'
308300020813    2C                   IF        §OGntw <> 'PPT'
308400000912     C* SE ELABORATA ALMENO UNA LINEA, STAMPO ANCHE I TOTALI FINALI
308500000912     C                   MOVEL     'S'           WSTAF             1
308600000912     C*
308700970205     C     DTSATK        SETLL     FNSAT02L
308800940429     C* LEGGO IL FILE PER LINEA
308900940429     C                   EXSR      LEGSAT
309000000912     C                   ENDIF
309100940429     C                   ADD       1             $X
309200940429    2C                   ENDDO
309300940429     c*
309400000912     C     WSTAF         IFEQ      'S'
309500940429     C                   EXSR      STAFIN
309600940429    1C                   ENDIF
309700000912    1C                   ENDIF
309800940429     C*
309900940429     C                   ENDSR
310000940429      **************************************************************************
310100940429      * IMPOSTO L'AREA, LE CERCO MAN MANO                              i
310200940429      **************************************************************************
310300940429     C     IMPARE        BEGSR
310400970205     C     $AREA         SETGT     FNSAT02L
310500970205     C                   READ(N)   FNSAT02L                               33
310600940429     C*
310700940429     C     *IN33         IFEQ      *OFF
310800940429     C                   MOVEL     SATFLE        $AREA
310900940429     C                   ENDIF
311000940429     C                   ENDSR
311100940429      **************************************************************************
311200940429      * IMPOSTO LA LINEA DI PARTENZA, LA CERCO MAN MANO                i
311300940429      **************************************************************************
311400940429     C     IMPLNP        BEGSR
311500970205     C  N29LPSATK        SETGT     FNSAT02L
311600970205     C  N29ARSATK        READE(N)  FNSAT02L                               33
311700000619     C                   CLEAR                   WTROVA
311800940429     C*
311900000619    1C     WTROVA        DOWEQ     ' '
312000000619    2C     *IN33         IFEQ      *OFF
312100000619     C     SATLNP        LOOKUP    LIN                                    99
312200000619    3C     *IN99         IFEQ      *ON
312300940429     C                   MOVEL     SATLNP        $LNP
312400940429     C                   MOVEL     SATLNP        $FILPR
312500000619     C                   MOVEL     '1'           WTROVA            1
312600970205     C     DTSATK        SETLL     FNSAT02L
312700000619   X3C                   ELSE
312800000619     C  N29ARSATK        READE(N)  FNSAT02L                               33
312900000619    3C                   ENDIF
313000000619     C**
313100000619   X2C                   ELSE
313200000619     C                   MOVEL     '1'           WTROVA
313300000619    2C                   ENDIF
313400000619    1C                   ENDDO
313500000619     C*
313600940429     C                   ENDSR
313700940429      **************************************************************************
313800940429      * IMPOSTO IL TERMINAL, LO CERCO MAN MANO                         i
313900940429      **************************************************************************
314000940429     C     IMPTFA        BEGSR
314100970211     C     ARSATK        SETGT     FNSAT03L
314200970211     C     $AREA         READE(N)  FNSAT03L                               33
314300940429     C*
314400940429     C     *IN33         IFEQ      *OFF
314500940429     C                   MOVEL     SATTFA        $TFA
314600940429     C                   MOVEL     SATTFA        $FILPR
314700970211     C     TASATK        SETLL     FNSAT03L
314800940429     C                   ENDIF
314900940429     C                   ENDSR
315000940429      **************************************************************************
315100940429      * STAMPA FINALE DEI C/S E TRANSITI                               i
315200940429      **************************************************************************
315300940429     C     STAFIN        BEGSR
315400940429     C* stampo i totali C/S
315500940506     C     *IN38         IFEQ      *OFF
315600940429     C     $PRIM2        IFEQ      1
315700940429     C                   EXSR      TOTSER
315800940429     C                   ENDIF
315900940506     C* TOTALI DI AREA
316000970226     C                   SETON                                        04
316100940505     C                   EXSR      TOTGEN
316200970226     C                   SETOFF                                       04
316300940506     C                   ENDIF
316400940502     C*
316500940502      * Terminate le letture per filiale tocca ai transiti
316600950210     C     DS1RIS        IFNE      'R'
316700940502     C                   EXSR      ELATRA
316800940503     C*
316900940505     C* CANCELLO LE RIGHE DEI TERMINAL CHE NON HANNO COLLI A TERRA
317000940503     C                   EXSR      ELATER
317100940502     C                   ENDIF
317200940502     C*
317300940429     C* stampo i totali dei TRANSITI
317400940506     C     *IN38         IFEQ      *OFF
317500000308     C     *IN27         IFEQ      *OFF
317600000308     C     DS1RIS        OREQ      'R'
317700000308     C                   EXSR      TOTTRA
317800000308     C                   ENDIF
317900940429     c* stampo il totale di area
318000940429     C     $PRIM4        IFEQ      1
318100970226     C                   SETON                                        0204
318200940429     C                   EXSR      TOTGEN
318300970226     C                   SETOFF                                       0204
318400950118     C                   WRITE     LSC2E4
318500940429     C                   ENDIF
318600940506     C                   ENDIF
318700940429     C                   ENDSR
318800940429      **************************************************************************
318900950210      * IMPOSTO L'AREA, LE CERCO MAN MANO
319000940429      **************************************************************************
319100940429     C     LEGSAT        BEGSR
319200970205     C     LPSATK        READE(N)  FNSAT02L                               33
319300940429     C*
319400940429    4C     *IN33         DOWEQ     *OFF
319500940429      *
319600970211    5C     SATDRE        IFLE      DS1AAL
319700940429      * Aggiorno i totali
319800940429     C                   EXSR      SERTRA
319900940429   x5C                   ELSE
320000940429     C                   SETON                                        33
320100940429    5C                   ENDIF
320200940429     C*
320300970205     C  N33LPSATK        READE(N)  FNSAT02L                               33
320400940429    4C                   ENDDO
320500940429     C* totali di filiale
320600940429    4C     $PRIM1        IFEQ      1
320700940429     C                   EXSR      TOTFIL
320800940429     C                   EXSR      AZZFIL
320900940429    4C                   ENDIF
321000940429     C                   ENDSR
321100000000      **************************************************************************
321200000000      * RISTAMPA: per servizi o transiti non stampo ma aggiorno i totali
321300000000      **************************************************************************
321400000000     C     SERTRA        BEGSR
321500000000      *
321600000000     C     SATTRE        IFEQ      *BLANK
321700940429     C                   Z-ADD     1             $PRIM1
321800970211     C                   ADD       SATCPE        CPA(1)
321900970211     C                   ADD       SATCPD        CPA(2)
322000970211     C                   ADD       SATCPC        CPA(3)
322100970211     C                   ADD       SATCDE        CBP(1)
322200970211     C                   ADD       SATCDD        CBP(2)
322300970228     C                   ADD       SATCDC        CBP(3)
322400970211     C                   ADD       SATCNE        CBR(1)
322500970211     C                   ADD       SATCND        CBR(2)
322600970211     C                   ADD       SATCNC        CBR(3)
322700970211     C                   ADD       SATPPE        PPA(1)
322800970211     C                   ADD       SATPPD        PPA(2)
322900970211     C                   ADD       SATPPC        PPA(3)
323000970211     C                   ADD       SATFDE        PFI(1)
323100970211     C                   ADD       SATFDD        PFI(2)
323200970211     C                   ADD       SATFDC        PFI(3)
323300970211     C                   ADD       SATFNE        PFN(1)
323400970211     C                   ADD       SATFND        PFN(2)
323500970211     C                   ADD       SATFNC        PFN(3)
323600970211     C                   ADD       SATODE        PBP(1)
323700970211     C                   ADD       SATODD        PBP(2)
323800970211     C                   ADD       SATODC        PBP(3)
323900970211     C                   ADD       SATONE        PBR(1)
324000970211     C                   ADD       SATOND        PBR(2)
324100970211     C                   ADD       SATONC        PBR(3)
324200970211     C                   ADD       SATVPE        VPA(1)
324300970211     C                   ADD       SATVPD        VPA(2)
324400970211     C                   ADD       SATVPC        VPA(3)
324500970211     C                   ADD       SATVDE        VBP(1)
324600970211     C                   ADD       SATVDD        VBP(2)
324700970211     C                   ADD       SATVDC        VBP(3)
324800970211     C                   ADD       SATVNE        VBR(1)
324900970211     C                   ADD       SATVND        VBR(2)
325000970211     C                   ADD       SATVNC        VBR(3)
325100970211     C                   ADD       SATSPE        SPA(1)
325200970211     C                   ADD       SATSPD        SPA(2)
325300970211     C                   ADD       SATSPC        SPA(3)
325400970211     C                   ADD       SATSDE        SBP(1)
325500970211     C                   ADD       SATSDD        SBP(2)
325600970211     C                   ADD       SATSDC        SBP(3)
325700970211     C                   ADD       SATSNE        SBR(1)
325800970211     C                   ADD       SATSND        SBR(2)
325900970211     C                   ADD       SATSNC        SBR(3)
326000970305     C                   ADD       SATSFE        FAF(1)
326100970305     C                   ADD       SATSFD        FAF(2)
326200970305     C                   ADD       SATSFC        FAF(3)
326300000000     C                   ADD       SATSCE        SCM(1)
326400970211     C                   ADD       SATSCD        SCM(2)
326500970211     C                   ADD       SATSCC        SCM(3)
326600000000     C                   ENDIF
326700000000      *
326800000000     C     SATTRE        IFEQ      'C'
326900940429     C                   Z-ADD     1             $PRIM2
327000970211     C                   ADD       SATCPE        C1A(1)
327100970211     C                   ADD       SATCPD        C1A(2)
327200970211     C                   ADD       SATCPC        C1A(3)
327300970211     C                   ADD       SATCDE        C1P(1)
327400970211     C                   ADD       SATCDD        C1P(2)
327500970211     C                   ADD       SATCDC        C1P(3)
327600970211     C                   ADD       SATCNE        C1R(1)
327700970211     C                   ADD       SATCND        C1R(2)
327800970211     C                   ADD       SATCNC        C1R(3)
327900970211     C                   ADD       SATPPE        P1A(1)
328000970303     C                   ADD       SATPPD        P1A(2)
328100970303     C                   ADD       SATPPC        P1A(3)
328200970211     C                   ADD       SATFDE        P1I(3)
328300970211     C                   ADD       SATFDD        P1I(2)
328400970211     C                   ADD       SATFDC        P1I(3)
328500970211     C                   ADD       SATFNE        P1N(1)
328600970211     C                   ADD       SATFND        P1N(2)
328700970211     C                   ADD       SATFNC        P1N(3)
328800970211     C                   ADD       SATODE        P1P(1)
328900970211     C                   ADD       SATODD        P1P(2)
329000970211     C                   ADD       SATODC        P1P(3)
329100970211     C                   ADD       SATONE        P1R(1)
329200970211     C                   ADD       SATOND        P1R(2)
329300970211     C                   ADD       SATONC        P1R(3)
329400970211     C                   ADD       SATVPE        V1A(1)
329500970211     C                   ADD       SATVPD        V1A(2)
329600970211     C                   ADD       SATVPC        V1A(3)
329700970211     C                   ADD       SATVDE        V1P(1)
329800970211     C                   ADD       SATVDD        V1P(2)
329900970211     C                   ADD       SATVDC        V1P(3)
330000970211     C                   ADD       SATVNE        V1R(1)
330100970211     C                   ADD       SATVND        V1R(2)
330200970211     C                   ADD       SATVNC        V1R(3)
330300970211     C                   ADD       SATSPE        S1A(1)
330400970211     C                   ADD       SATSPD        S1A(2)
330500970211     C                   ADD       SATSPC        S1A(3)
330600970211     C                   ADD       SATSDE        S1P(1)
330700970211     C                   ADD       SATSDD        S1P(2)
330800970211     C                   ADD       SATSDC        S1P(3)
330900970211     C                   ADD       SATSNE        S1R(1)
331000970211     C                   ADD       SATSND        S1R(2)
331100970211     C                   ADD       SATSNC        S1R(3)
331200970305     C                   ADD       SATSFE        F1F(1)
331300970305     C                   ADD       SATSFD        F1F(2)
331400970305     C                   ADD       SATSFC        F1F(3)
331500000000     C                   ADD       SATSCE        S1M(1)
331600970211     C                   ADD       SATSCD        S1M(2)
331700970211     C                   ADD       SATSCC        S1M(3)
331800000000     C                   ENDIF
331900000000      *
332000940429     C     SATTRE        IFEQ      'T'
332100940429     C                   Z-ADD     1             $PRIM3
332200970211     C                   ADD       SATCDE        C2P(1)
332300970211     C                   ADD       SATCDD        C2P(2)
332400970211     C                   ADD       SATCDC        C2P(3)
332500970211     C                   ADD       SATFDE        P2I(1)
332600970211     C                   ADD       SATFDD        P2I(2)
332700970211     C                   ADD       SATFDC        P2I(3)
332800970211     C                   ADD       SATODE        P2P(1)
332900970211     C                   ADD       SATODD        P2P(2)
333000970211     C                   ADD       SATODC        P2P(3)
333100970211     C                   ADD       SATVDE        V2P(1)
333200970211     C                   ADD       SATVDD        V2P(2)
333300970211     C                   ADD       SATVDC        V2P(3)
333400970211     C                   ADD       SATSDE        S2P(1)
333500970211     C                   ADD       SATSDD        S2P(2)
333600970211     C                   ADD       SATSDC        S2P(3)
333700000000     C                   ADD       SATSCE        S2M(1)
333800970211     C                   ADD       SATSCD        S2M(2)
333900970211     C                   ADD       SATSCC        S2M(3)
334000940505     C*
334100940505     C                   ELSE
334200000000      *
334300940429      *  totali generali
334400940429     C                   Z-ADD     1             $PRIM4
334500970211     C                   ADD       SATCDE        C3P(1)
334600970211     C                   ADD       SATCDD        C3P(2)
334700970211     C                   ADD       SATCDC        C3P(3)
334800970211     C                   ADD       SATFDE        P3I(1)
334900970211     C                   ADD       SATFDD        P3I(2)
335000970211     C                   ADD       SATFDC        P3I(3)
335100970212     C                   ADD       SATODE        P3P(1)
335200970212     C                   ADD       SATODD        P3P(2)
335300970212     C                   ADD       SATODC        P3P(3)
335400970211     C                   ADD       SATVDE        V3P(1)
335500970211     C                   ADD       SATVDD        V3P(2)
335600970211     C                   ADD       SATVDC        V3P(3)
335700000406     C                   ADD       SATVNE        V3R(1)
335800000406     C                   ADD       SATVND        V3R(2)
335900000406     C                   ADD       SATVNC        V3R(3)
336000970211     C                   ADD       SATSDE        S3P(1)
336100970211     C                   ADD       SATSDD        S3P(2)
336200970211     C                   ADD       SATSDC        S3P(3)
336300970211     C                   ADD       SATSNE        S3R(1)
336400970211     C                   ADD       SATSND        S3R(2)
336500970211     C                   ADD       SATSNC        S3R(3)
336600970305     C                   ADD       SATSFE        F3F(1)
336700970305     C                   ADD       SATSFD        F3F(2)
336800970305     C                   ADD       SATSFC        F3F(3)
336900000000     C                   ADD       SATSCE        S3M(1)
337000970211     C                   ADD       SATSCD        S3M(2)
337100970211     C                   ADD       SATSCC        S3M(3)
337200940505     C                   ENDIF
337300000000      *
337400000000     C                   ENDSR
337500940505      **************************************************************************
337600940505      * RISTAMPA: addiziono i transiti al totale generale              i
337700940505      **************************************************************************
337800940505     C     ADDTRA        BEGSR
337900940505     C                   Z-ADD     1             $PRIM4
338000940505     C                   ADD       C2P           C3P
338100940505     C                   ADD       P2I           P3I
338200940505     C                   ADD       P2P           P3P
338300940505     C                   ADD       V2P           V3P
338400940505     C                   ADD       S2P           S3P
338500940505     C                   ADD       S2M           S3M
338600940505     C                   ENDSR
338700000000      **************************************************************************
338800000000      * RISTAMPA: azzera i totali generali
338900000000      **************************************************************************
339000000000     C     AZZTOT        BEGSR
339100000000      *
339200970212     C                   CLEAR                   C3A
339300970212     C                   CLEAR                   C3P
339400970211     C                   CLEAR                   P3I
339500970211     C                   CLEAR                   P3P
339600970211     C                   CLEAR                   P3A
339700970211     C                   CLEAR                   V3P
339800940502     C                   CLEAR                   V3A
339900000406     C                   CLEAR                   V3R
340000970211     C                   CLEAR                   S3A
340100970211     C                   CLEAR                   S3P
340200970211     C                   CLEAR                   S3R
340300970211     C                   CLEAR                   S3M
340400970305     C                   CLEAR                   F3F
340500000000      *
340600000000     C                   ENDSR
340700000000      **************************************************************************
340800000000      * Testa l'overflow e stampa su carta
340900000000      **************************************************************************
341000000000     C     SCRIVI        BEGSR
341100940509      * PER TOTALI GENERALI SE GIA' STAMPE PIU' DI 9 RIGHE COMUNQUE
341200940509      * VADO A PAGINA NUOVA
341300940509     C     *IN03         IFEQ      *OFF
341400940509     C     *IN02         ANDEQ     *ON
341500940509     C     $RIGHE        ANDGT     9
341600940509     C                   SETON                                        30
341700940509     C                   ENDIF
341800000000      *
341900940429     C     *IN38         IFEQ      *OFF
342000940509     C*
342100940509     C     *IN30         IFEQ      *ON
342200940509     C     *IN40         ANDEQ     *ON
342300950118     C                   WRITE     LSC2T1
342400950118     C                   WRITE     LSC2T3
342500940509     C                   CLEAR                   $RIGHE
342600940509     C                   SETOFF                                       30
342700940509     C                   ENDIF
342800950118     C                   WRITE     LSC2D1
342900000000      *
343000970226     C  N04$ALMEN        IFEQ      *ZERO
343100000000     C                   Z-ADD     1             $ALMEN
343200000000     C                   ENDIF
343300940328     C                   ENDIF
343400000000      *
343500000000     C                   ENDSR
343600000000      **************************************************************************
343700000000      * Verifica se vi sono dei fogli viaggio con data partenza compresa
343800000000      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
343900000000      **************************************************************************
344000000000     C     FOGVIA        BEGSR
344100020813     ***C                   OPEN      FNFGV04L
344200020813     C                   OPEN      FNFGV06L
344300000000      *
344400020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
344500020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
344600020813     C     SIMfel        setll     FNFGV06L                               99
344700000000     C                   DO        *HIVAL
344800020813     ***C                   READ      FNFGV04L                               99
344900020813     C     SIMfel        reade     FNFGV06L                               99
345000950210     C  N99FGVDTP        IFGE      DS1ADA
345100950210     C     FGVDTP        ANDLE     DS1AAL
345200940502     C                   SETON                                        99
345300950118     C                   WRITE     LSC2E3
345400000000     C                   ENDIF
345500940502     C  N99              ENDDO
345600000000      *
345700020813     ***C                   CLOSE     FNFGV04L
345800020813     C                   CLOSE     FNFGV06L
345900000000     C                   ENDSR
346000000000      **************************************************************************
346100000000      * Aggiornamento del numero colli esaminati, nel file di work
346200950118     OFNWRK000  E            AGGIO
346300000000     O                       WRKNC1
346400000000      **************************************************************************
346500000000      *                                4         5         6         7         8
346600950118** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
346700970807OVRPRTF FNLSC2P       FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.terra
346800950210')
346900000000**
347000950118ESPR.DISTRCORR.
