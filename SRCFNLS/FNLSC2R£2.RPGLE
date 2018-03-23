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
004000021023     FFNBLT38L  IF   E           K DISK
004100020625     FFNBTP11L  IF   E           K DISK
004200020625     FFNBTT39L  IF   E           K DISK
004300020813     ***FFNFGV04L  IF   E           K DISK    USROPN
004400020813     FFNFGV06L  IF   E           K DISK    USROPN
004500970205     FFNSAT02L  UF A E           K DISK
004600970211     FFNSAT03L  UF   E           K DISK
004700970211     F                                     RENAME(FNSAT000:FNSAT003)
004800970205     FFNSAT04L  UF   E           K DISK    USROPN
004900970205     F                                     RENAME(FNSAT000:FNSAT004)
005000950118     FFNWRK00F  UF A E           K DISK
005100000000     FTABEL00F  IF   E           K DISK
005200021016     fTntbe01l  uf   e           k Disk
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
017900040921     D*DS5C          E DS
018000970707     D DS5P          E DS
018100000000     D KPJBA         E DS
018200000000     D CNCR80        E DS
018300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
018400950210     D* DS PER TRUL06R - CARICAMENTO £X
018500950210     D DSUL06        E DS                  EXTNAME(TRUL06DS)
018600950314     D  T06                    1     90  0
018700950314     D                                     DIM(30)
018800970303      * DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
018900970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
019000950210     D DSLS01        E DS
019100020813      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
019200020813     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
019300070522     D* DS PER LETTURA TABELLA "VPO" record DECO
019400070522     D Dvpodeco      E DS
019500021016      * Ds date statistiche
019600021016     d dSdf          e ds
019700020813      *
019800950210     D                 DS
019900950210     D  DATADA                 1      8  0
020000950210     D  ANNODA                 1      4  0
020100950210     D  MESEDA                 5      6  0
020200950210     D  $GI                    7      8  0
020300950118      * DS per controllare il "precedente" di FNBLT
020400000000     D $BLTAT          DS
020500000000     D  BLTLNP                 1      3  0
020600950118     D  BLTAAS                 4      7  0
020700950118     D  BLTNRS                 8      9  0
020800950118     D  BLTNSP                10     16  0
020900950118      * DS per controllare il "precedente" di FNBTT
021000950118     D $BTTAT          DS
021100950118     D  BTTLNP                 1      3  0
021200950118     D  BTTAAS                 4      7  0
021300950118     D  BTTNRS                 8      9  0
021400950118     D  BTTNSP                10     16  0
021500970207      * DS per i campi del file statistiche FNSAT00F
021600000000     D                 DS
021700000000     D  C0P                    1     21  0
021800970207     D                                     DIM(3)
021900970207     D  SATCDE                 1      7  0
022000970207     D  SATCDD                 8     14  0
022100970207     D  SATCDC                15     21  0
022200000000     D                 DS
022300000000     D  C0R                    1     21  0
022400970207     D                                     DIM(3)
022500970207     D  SATCNE                 1      7  0
022600970207     D  SATCND                 8     14  0
022700970207     D  SATCNC                15     21  0
022800000000     D                 DS
022900000000     D  P0I                    1     27  1
023000970207     D                                     DIM(3)
023100970207     D  SATFDE                 1      9  1
023200970207     D  SATFDD                10     18  1
023300970207     D  SATFDC                19     27  1
023400970207     D                 DS
023500970207     D  P0N                    1     27  1
023600970207     D                                     DIM(3)
023700970207     D  SATFNE                 1      9  1
023800970207     D  SATFND                10     18  1
023900970207     D  SATFNC                19     27  1
024000000000     D                 DS
024100000000     D  P0P                    1     27  1
024200970210     D                                     DIM(3)
024300970207     D  SATODE                 1      9  1
024400970207     D  SATODD                10     18  1
024500970207     D  SATODC                19     27  1
024600000000     D                 DS
024700000000     D  P0R                    1     27  1
024800970207     D                                     DIM(3)
024900970207     D  SATONE                 1      9  1
025000970207     D  SATOND                10     18  1
025100970207     D  SATONC                19     27  1
025200000000     D                 DS
025300000000     D  V0P                    1     27  4
025400970207     D                                     DIM(3)
025500970207     D  SATVDE                 1      9  4
025600970207     D  SATVDD                10     18  4
025700970207     D  SATVDC                19     27  4
025800000000     D                 DS
025900000000     D  V0R                    1     27  4
026000970207     D                                     DIM(3)
026100970207     D  SATVNE                 1      9  4
026200970207     D  SATVND                10     18  4
026300970207     D  SATVNC                19     27  4
026400000000     D                 DS
026500000000     D  S0P                    1     21  0
026600970207     D                                     DIM(3)
026700970207     D  SATSDE                 1      7  0
026800970207     D  SATSDD                 8     14  0
026900970207     D  SATSDC                15     21  0
027000000000     D                 DS
027100000000     D  S0R                    1     21  0
027200970207     D                                     DIM(3)
027300970207     D  SATSNE                 1      7  0
027400970207     D  SATSND                 8     14  0
027500970207     D  SATSNC                15     21  0
027600970305     D                 DS
027700970305     D  F0F                    1     21  0
027800970305     D                                     DIM(3)
027900970305     D  SATSFE                 1      7  0
028000970305     D  SATSFD                 8     14  0
028100970305     D  SATSFC                15     21  0
028200000000     D                 DS
028300000000     D  S0M                    1     21  0
028400000000     D                                     DIM(3)
028500000000     D  SATSCE                 1      7  0
028600970207     D  SATSCD                 8     14  0
028700970207     D  SATSCC                15     21  0
028800970210     D                 DS
028900970210     D  CDT                    1     21  0
029000970210     D                                     DIM(3)
029100970210     D  SATCPE                 1      7  0
029200970210     D  SATCPD                 8     14  0
029300970211     D  SATCPC                15     21  0
029400970210     D                 DS
029500970210     D  PDT                    1     27  1
029600970210     D                                     DIM(3)
029700970210     D  SATPPE                 1      9  1
029800970210     D  SATPPD                10     18  1
029900970210     D  SATPPC                19     27  1
030000970210     D                 DS
030100970210     D  VDT                    1     27  4
030200970210     D                                     DIM(3)
030300970210     D  SATVPE                 1      9  4
030400970210     D  SATVPD                10     18  4
030500970210     D  SATVPC                19     27  4
030600970210     D                 DS
030700970210     D  SDT                    1     21  0
030800970210     D                                     DIM(3)
030900970210     D  SATSPE                 1      7  0
031000970210     D  SATSPD                 8     14  0
031100970210     D  SATSPC                15     21  0
031200020813      *
031300020813      * Varibaili
031400020813      *
031500020813     D SAVRIS          S                   LIKE(DS1RIS) inz
031600020813     D $CDA            S              7  0              inz
031700020813     D $VDA            S              9  4              inz
031800020813     D $PDA            S              9  1              inz
031900020813     D $SDA            S              7  0              inz
032000020813     D $TFA            S                   LIKE(BLPTFA) inz
032100020813     D $AREA           S                   LIKE(DS1FEL) inz
032200020813     D $LNP            S                   LIKE(DS1LNP) inz
032300020813     D $BLTPR          S                   LIKE($BLTAT) inz
032400020813     D $FILPR          S                   LIKE(BLPLNP) inz
032500020813     D $CBLPR          S                   LIKE(BLPCBO) inz
032600020813     D $NFVPR          S                   LIKE(BLPNFV) inz
032700020813     D $DTRIT          S                   LIKE(BLPDRT) inz
032800020813     D $CDPDR          S                   LIKE(BLPPDR) inz
032900020813     D $FLAG           S                   LIKE(WRKFLG) inz
033000020813     D $FLAG2          S                   LIKE(WRKFLG) inz
033100020813     D $DFV            S                   LIKE(FGVDFV) inz
033200020813     D $FTR            S                   LIKE(SATFTR) inz
033300020813     D $DTR            S                   LIKE(SATDTR) inz
033400020813     D $TRE            S                   LIKE(SATTRE) inz
033500020813     D $COMOD          S             15  6 INZ
033600020813     D $COMO1          S             15  6 INZ
033700020813     D*
033800020813     D KCOD            S                   LIKE(TBLCOD) inz
033900020813     D KKEY            S                   LIKE(TBLKEY) inz
034000021217      *
034100021217     d $Volum          s             12  4 inz
034200021217     d $Peso           s              8  1 inz
034300030213     d*** WsiVOL          s              1    inz
034400030213     d*** WsiPES          s              1    inz
034500021217     d Wvolu           s                   like(BLPvlf) inz
034600021217     d Wpeso           s                   like(BLPpkf) inz
034700021220     d Wvolmed         s                   like($Volum) inz
034800021220     d Wpesmed         s                   like($Peso)  inz
034900940502      **************************************************************************
035000940502      *      RIEPILOGO INDICATORI
035100940502      **************************************************************************
035200940502      * 01    - ON: LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE = 0
035300940502      *       - OFF:LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE > D.ELAB
035400940502      * 02    - ON: TOTALE GENERALE
035500940502      *       - OFF:TOTALE DI AREA
035600970226      * 03    - STAMPA DEI TOTALI TRANSITI
035700970226      * 04    - NON MEMORIZZA $ALMEN NELLA ROURINE DI TOTGEN
035800940502      * 05    - STAMPE GIORNALIERE
035900970205      * 27    - OFF: DEVO STAMPARE LA RIGA DEI TRANASITI
036000940502      * 29    - ON: IMMESSA UNA LINEA DI PARTENZA
036100940502      * 30    - ECCEDENZA PAGINA
036200940502      * 33    - LETTURA FILES
036300940502      * 34    - BOLLA IN C/SERVIZIO
036400940502      * 35    - ON: BOLLA BOLLETTATA NEL PERIODO
036500940502      *       - OFF:BOLLA BOLLETTATA NEL PERIODO PRECEDENTE
036600940502      * 36    - COLLO A TERRA
036700970211      * 37    - LETTURA SUL FILES FNSAT
036800950210      * 38    - DS1RIS= ' ' --> NON DEVO STAMPARE
036900940502      * 39    - ON: IMMESSO CODICE AREA
037000940502      * 40    - ON: STAMPA LA LILIALE CON DECODIFICA
037100940502      *       - OFF:STAMPA LA DICITURA DEL SERVIZIO BOLLE
037200940502      * 41    - ON: STAMPA SOLTANTO LA FILIALE (+ 40 ON)
037300940502      * 43    - ON: STAMPO LA PARTE RELATIVA ALLE LINEE DI PARTENZA
037400940502      *       - OFF:STAMPO LA PARTE RELATIVA AI TERMINAL DI ARRIVO
037500970211      * 45    - STAMPA LE PERCENTUALI SUL DA PARTIRE
037600950118      * 99    - DI COMODO
037700940502      **************************************************************************
037800000000     C     *ENTRY        PLIST
037900000000     C                   PARM                    KPJBA
038000950210     C                   MOVEL     KPJBU         DSLS01
038100950210      *
038200950210      * DATA FINALE AA/MM/GG (6 CARATTERI)
038300090108     C                   Z-ADD     DS1aal        DATADA
038400000000      *
038500950118     C     FNBLTK        KLIST
038600940428     C                   KFLD                    LIN($X)
038700940428     C                   KFLD                    $DFV
038800970205     C     FNBLPK        KLIST
038900950118     C                   KFLD                    BLTAAS
039000000000     C                   KFLD                    BLTLNP
039100000000     C                   KFLD                    BLTNRS
039200000000     C                   KFLD                    BLTNSP
039300950118     C     FNBTPK        KLIST
039400020625     c                   kfld                    bttflp
039500950118     C                   KFLD                    BTTAAS
039600950118     C                   KFLD                    BTTLNP
039700950118     C                   KFLD                    BTTNRS
039800950118     C                   KFLD                    BTTNSP
039900950118     C     AZCLNK        KLIST
040000940504     C                   KFLD                    SIMFEL
040100940504     C                   KFLD                    $TFA
040200940504     C                   KFLD                    ANNODA
040300940504     C                   KFLD                    MESEDA
040400950118     C     FNWRKK        KLIST
040500000000     C                   KFLD                    $FLAG
040600000000     C                   KFLD                    $FILPR
040700950118     C     FNWR1K        KLIST
040800000000     C                   KFLD                    $FLAG
040900000000     C                   KFLD                    WRKLNP
041000940503     C                   KFLD                    BLPAAS
041100940503     C                   KFLD                    BLPNRS
041200940503     C                   KFLD                    BLPNSP
041300000000     C                   KFLD                    WRKCSE
041400970409     C     FNWR2K        KLIST
041500970409     C                   KFLD                    $FLAG
041600970409     C                   KFLD                    WRKLNP
041700970409     C                   KFLD                    BTPAAS
041800970409     C                   KFLD                    BTPNRS
041900970409     C                   KFLD                    BTPNSP
042000970409     C                   KFLD                    WRKCSE
042100970205     C     FNSATK        KLIST
042200970205     C                   KFLD                    SIMFEL
042300970205     C                   KFLD                    LIN(XX)
042400970205     C                   KFLD                    DS1AAL
042500970205     C     FNSA1K        KLIST
042600000000     C                   KFLD                    SATFLE
042700000000     C                   KFLD                    SATTFA
042800940429     C                   KFLD                    SATLNP
042900000000     C                   KFLD                    SATDRE
043000000000     C                   KFLD                    SATTRE
043100970205     C     DTSATK        KLIST
043200940429     C                   KFLD                    $AREA
043300940429     C                   KFLD                    $TFA
043400940429     C                   KFLD                    $LNP
043500970211     C                   KFLD                    DS1ADA
043600970205     C     LPSATK        KLIST
043700940429     C                   KFLD                    $AREA
043800940429     C                   KFLD                    $TFA
043900940429     C                   KFLD                    $LNP
044000970205     C     ARSATK        KLIST
044100940429     C                   KFLD                    $AREA
044200940429     C                   KFLD                    $TFA
044300970211     C     TASATK        KLIST
044400940429     C                   KFLD                    $AREA
044500940429     C                   KFLD                    $TFA
044600970211     C                   KFLD                    DS1ADA
044700970211     C     TRSATK        KLIST
044800940503     C                   KFLD                    $AREA
044900940503     C                   KFLD                    $TFA
045000970211     C                   KFLD                    DS1ADA
045100940503     C                   KFLD                    $TRE
045200020625     c     kbtt39        klist
045300020625     c                   kfld                    simfel
045400020625     c                   kfld                    $dfv
045500021016
045600021016     c     Ktbe          Klist
045700021016     c                   Kfld                    kTbeCod
045800021016     c                   Kfld                    kTbeKe1
045900000000      *
046000020813     ***C     *LIKE         DEFINE    DS1RIS        SAVRIS
046100020813     ***C     *LIKE         DEFINE    CPA(1)        $CDA
046200020813     ***C     *LIKE         DEFINE    VPA(1)        $VDA
046300020813     ***C     *LIKE         DEFINE    PPA(1)        $PDA
046400020813     ***C     *LIKE         DEFINE    SPA(1)        $SDA
046500020813     ***C     *LIKE         DEFINE    BLPTFA        $TFA
046600020813     ***C     *LIKE         DEFINE    DS1FEL        $AREA
046700020813     ***C     *LIKE         DEFINE    DS1LNP        $LNP
046800020813     ***C     *LIKE         DEFINE    $BLTAT        $BLTPR
046900020813     ***C     *LIKE         DEFINE    BLPLNP        $FILPR
047000020813     ***C     *LIKE         DEFINE    BLPCBO        $CBLPR
047100020813     ***C     *LIKE         DEFINE    BLPNFV        $NFVPR
047200020813     ***C     *LIKE         DEFINE    BLPDRT        $DTRIT
047300020813     ***C     *LIKE         DEFINE    BLPPDR        $CDPDR
047400020813     ***C     *LIKE         DEFINE    WRKFLG        $FLAG
047500020813     ***C     *LIKE         DEFINE    WRKFLG        $FLAG2
047600020813     ***C     *LIKE         DEFINE    FGVDFV        $DFV
047700020813     ***C     *LIKE         DEFINE    SATFTR        $FTR
047800020813     ***C     *LIKE         DEFINE    SATDTR        $DTR
047900020813     ***C     *LIKE         DEFINE    SATTRE        $TRE
048000020813     ***C                   Z-ADD     *ZERO         $COMOD           15 6
048100021016     C                   Z-ADD     *ZERO         $COMO1           15 6
048200021016     C                   Z-ADD     1             CODUT
048300000000      **************************************************************************
048400000000      *              M A I N      L I N E
048500000000      **************************************************************************
048600940429     C                   MOVE      'RISTAMPA'    STRIP
048700940429     C*
048800950210    1C     DS1RIS        IFNE      'R'
048900940429     C                   MOVE      'STAMPA  '    STRIP
049000940502    1C                   ENDIF
049100070522     c* Carico la tabella VPO per vedere da quando cambia il calcolo
049200070522     c*  della affidabilità partenza
049300070522     C                   MOVEL     'VPO'         kTBECOD
049400070522     C                   MOVEL(P)  'DECO'        kTBEKE1
049500070522     C     KTBE          CHAIN(N)  TNTBE01L                           21
049600070522     C  N21              MOVEL     TBEUNI        DVPODECO
049700070522     C*
049800070522     c
049900000000      *
050000000000      * Se la data AL e' vuota la imposto come la data DAL
050100950210     C     DS1AAL        IFEQ      *ZERO
050200950210     C                   Z-ADD     DS1ADA        DS1AAL
050300950210     C                   Z-ADD     DS1GDA        DS1GAL
050400000000     C                   ENDIF
050500940428     C*
050600940428     C                   SETON                                        43
050700940401      *
050800950210      * Accendo un indicatore se non devo stampare il tabulato (DS1RIS=' ')
050900950210     C     DS1RIS        COMP      *BLANKS                                38
051000940429      *
051100940429      * 39 ON - Immessa l'area
051200950210    3C     DS1FEL        IFGT      0
051300940429     C                   SETON                                        39
051400940429    3C                   ENDIF
051500000000      *
051600950210      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
051700950210     C                   EXSR      COMUNE
051800950210      *
051900000000     C                   SETON                                        LR
052000020212     C*****************************************************************
052100020212     C*  PARTE COMUNE STAMPE GIORNALIERE
052200020212     C*
052300020212     C*  . REPERIMENTO 'SIMFEL'
052400020212     C*  . CARICAMENTO '£1'
052500020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
052600020212     C*  . CODA DI STAMPA ('£OUTQ')
052700020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
052800020212     C*  . MODULO PER LA STAMPA ('£MOD')
052900020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
053000020212     C*****************************************************************
053100020212     C     COMUNE        BEGSR
053200020212     C*
053300020212     C*****************************************************************
053400020212     C*       RIEPILOGO INDICATORI:
053500020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
053600020212     C* 11    -           STAMPA PER 1L
053700020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
053800020212     C*****************************************************************
053900020212     C                   Z-ADD     1             CODUT
054000020212     C*
054100020212     C* Reperimento parametri aziendali
054200020212     C                   CALL      'X§PARUT'
054300020212     C                   PARM                    UT§DSE
054400020212     C                   MOVEL     RAGUT         RSUT             20
054500020212     C                   MOVEL     REC80         CNCR80
054600020212     C*
054700020212     C* Chiave di accesso per il file tabelle
054800020212     C     KTAB1         KLIST
054900020212     C                   KFLD                    CODUT
055000020212     C                   KFLD                    KCOD
055100020212     C                   KFLD                    KKEY
055200020212     C     KTAB2         KLIST
055300020212     C                   KFLD                    CODUT
055400020212     C                   KFLD                    KCOD
055500020212     C*
055600020813     ***C     *LIKE         DEFINE    TBLCOD        KCOD
055700020813     ***C     *LIKE         DEFINE    TBLKEY        KKEY
055800020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
055900020212     C*
056000020212     C* DETERMINO DATA E ORA DI ESECUZIONE
056100020212     C                   TIME                    WTIME            14 0
056200020212     C                   MOVE      WTIME         WDATE             8 0
056300020212     C                   MOVEL     WTIME         UTIME             6 0
056400040921     c*
056500040921     C                   CLEAR                   WLBDAT
056600040921     C                   MOVE      Wtime         G02DAT
056700040921     C                   CALL      'XSRDA8'
056800040921     C                   PARM                    WLBDAT
056900040921     C                   Z-ADD     G02INV        dateu             8 0
057000040922     c*
057100040922     c                   clear                   $FTR
057200040922     c                   z-add     dateu         $DTR
057300020212     C*
057400020418     C* CARICO '£1'
057500020212     C                   CLEAR                   KPJBU
057600020212     C                   CLEAR                   DSUL06
057700020212     C                   MOVE      '£1'          D06COD
057800020212     C                   MOVEL     SIMFEL        D06KEY
057900050802     C                   MOVE      'S'           D06KEY
058000050802     C                   MOVEL     ds1ada        D06drf
058100020212     C                   MOVEL     DSUL06        KPJBU
058200020212     C                   CALL      'TRUL06R'
058300020212     C                   PARM                    KPJBA
058400020212     C                   MOVEL     KPJBU         DSUL06
058500020418     c                   movea     t06           sav_t06
058600020418
058700020418      * se in arrivo carico la £6
058800020418 b1  c                   if        ds1tbo = 'A'
058900020418     c                   clear                   wc
059000020418 b2  c                   do        30            wa
059100020422     c                   if        sav_t06(wa) = *zeros
059200020422     c                   leave
059300020422     c                   endif
059400020418     c                   clear                   kpjbu
059500020418     c                   clear                   dsul06
059600020418     c                   eval      d06cod = '£6'
059700020418     c                   movel     sav_t06(wa)   d06key
059800050802     C                   MOVE      'S'           D06KEY
059900020418     c                   eval      d06esc = 'S'
060000050802     c                   movel     ds1ada        d06drf
060100020419     c                   movel     dsul06        kpjbu
060200020418     c                   call      'TRUL06R'
060300020418     c                   parm                    kpjba
060400020418     c                   movel     kpjbu         dsul06
060500020418 b3  c                   do        30            wb
060600020418     c                   if        t06(wb) = *zeros
060700020418     c                   leave
060800020418     c                   endif
060900020418     c                   add       1             wc
061000020418     c                   z-add     t06(wb)       l6(wc)
061100020418 e3  c                   enddo
061200020418 e2  c                   enddo
061300020212     C*
061400020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
061500020212     C                   Z-ADD     *ZERO         WB
061600020418 b2  C                   DO        29            WA
061700020418     C     sav_T06(WA)   LOOKUP    L6                                     30
061800020212     C  N30              ADD       1             WB
061900020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
062000020418 e2  C                   ENDDO
062100020418 x1  C                   ELSE
062200020418     C                   MOVEA     sav_T06       L1
062300020418 e1  C                   ENDIF
062400020212     C*
062500020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
062600020212     C                   MOVEA     L1            LIN
062700020212     C                   CLEAR                   L1
062800020212     C                   Z-ADD     *ZERO         WB                3 0
062900020212     C     1             DO        30            WPT               3 0
063000020212     C     LIN(WPT)      IFNE      0
063100020212     C                   CLEAR                   FNLSC4
063200020212     C                   Z-ADD     LIN(WPT)      IC4LIN
063300020212     C                   MOVE      'B'           IC4FLG
063400020212     C                   MOVEL     FNLSC4        KPJBU
063500020212     C                   CALL      'FNLSC4R1'
063600020212     C                   PARM                    KPJBA
063700050802     C                   PARM                    ds1ada
063800020212     C                   MOVEL     KPJBU         FNLSC4
063900020212     C     OC4ESE        IFEQ      'S'
064000020212     C                   ADD       1             WB
064100020212     C                   MOVE      LIN(WPT)      L1(WB)
064200020212     C                   ENDIF
064300020212     C                   ENDIF
064400020212     C                   ENDDO
064500070103     C*
064600070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
064700070103     C                   Z-ADD     *ZERO         WB                3 0
064800070103     C                   DO        29            WA                3 0
064900070103     C     L1(WA)        IFNE      *ZEROS
065000070103     C     L1(WA)        ANDNE     SIMFEL
065100070103     C                   ADD       1             WB
065200070103     C                   Z-ADD     L1(WA)        LV2(WB)
065300070103     C                   ENDIF
065400070103     C                   ENDDO
065500020212     C*
065600020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
065700020212     C*
065800020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
065900020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
066000020212     C* la stampa e' lanciata da menu
066100020212     C                   MOVEL     '3L'          KCOD
066200020212     C                   MOVEL     DS1PGM        KKEY
066300020212     C                   MOVE      DS1FPG        KKEY
066400020212     C     KTAB1         CHAIN     TABEL00F                           30
066500020212     C  N30              MOVEL     TBLUNI        DS3L
066600020212     C   30              MOVEL     'F'           §3LD1L
066700020212     C   30              MOVEL     ' '           §3LD2L
066800020212     C   30              MOVEL     '*STD    '    §3LDST
066900020212     C*
067000020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
067100020212     C                   MOVEL     §3LDST        £MOD             10
067200020212     C                   CLEAR                   LIN
067300020212     C*
067400020212     C* Verifico chi ha lanciato la stampa
067500020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
067600020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
067700020212     C* DS1FGS<>0  --->  si proviene da menu
067800020212     C     DS1FGS        IFEQ      0
067900020212     C*
068000020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
068100020212     C* lancio, voglio la stampa;  invece
068200020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
068300020212     C*
068400020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
068500020212     C* creare stampa per 1°lvl. (se stessa)
068600020212     C     DS1LNP        IFEQ      SIMFEL
068700020212     C*
068800020212     C* Il flag §3LD1L ha i seguenti significati:
068900020212     C*    ' ' = tutti i dati
069000020212     C*    '1' = solo filiale di 1' livello
069100020212     C     §3LD1L        IFEQ      '1'
069200020212     C                   SETON                                        11
069300020212     C                   END
069400020212     C*
069500020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
069600020212     C* nella stampa
069700020212     C*
069800020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
069900020212     C  N11              MOVEA     L1            LIN
070000020212     C   11              MOVEA     SIMFEL        LIN(1)
070100020212     C*
070200020212     C* IMPOSTO LA STAMPANTE:
070300020212     C* STAMPANTE TABULATI
070400020212     C                   MOVEL     '5P'          KCOD
070500020212     C                   MOVEL     'T'           W004A             4
070600020212     C                   MOVE      LIN(1)        W004A
070700020212     C                   MOVEL(P)  W004A         KKEY
070800020212     C     KTAB1         CHAIN     TABEL00F                           99
070900020212     C     *IN99         IFEQ      *OFF
071000020212     C                   MOVEL     TBLUNI        DS5P
071100020212     C                   MOVEL     §5POTQ        £OUTQ            10
071200020212     C                   END
071300020212     C*
071400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
071500020212     C                   EXSR      LEGGI
071600020212     C*
071700020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
071800020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
071900020212     C                   ELSE
072000020212     C*
072100020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
072200020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
072300020212     C*
072400020212     C* Il significato del fla §3LD2L e' il seguente:
072500020212     C*    ' ' = tutti i dati
072600020212     C*    'F' = per ogni 2' livello
072700020212     C     §3LD2L        IFEQ      'F'
072800020212     C                   SETON                                        14
072900020212     C                   ENDIF
073000020212     C*
073100020212     C                   Z-ADD     1             WB
073200020212     C                   MOVEA     *ZEROS        LIN
073300020212     C  N14              MOVEA     L1            LIN
073400020212     C*
073500020212     C* Lancio per tutti i 2° lvl
073600020212     C     DS1LNP        IFEQ      0
073700020212     C*
073800020212     C* LV2 contiene tutte le filiali di 2° lvl
073900020212     C     LV2(WB)       DOWNE     0
074000020212      *
074100020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
074200020212     C                   CLEAR                   FNLSC4
074300020212     C                   Z-ADD     LV2(WB)       IC4LIN
074400020212     C                   MOVEL     FNLSC4        KPJBU
074500020212     C                   CALL      'FNLSC4R1'
074600020212     C                   PARM                    KPJBA
074700050802     C                   PARM                    ds1ada
074800020212     C                   MOVEL     KPJBU         FNLSC4
074900020212      *
075000020212     C     OC4PT1        IFEQ      'S'
075100020212     C     §3LPT         ANDEQ     'N'
075200020212     C                   GOTO      NOLEGG
075300020212     C                   ENDIF
075400020212      *
075500020212     C     OC4EXP        IFEQ      'S'
075600020212     C     §3LEXP        ANDEQ     'N'
075700020212     C                   GOTO      NOLEGG
075800020212     C                   ENDIF
075900020212      *
076000020212     C     OC4DPD        IFEQ      'S'
076100020212     C     §3LDPD        ANDEQ     'N'
076200020212     C                   GOTO      NOLEGG
076300020212     C                   ENDIF
076400020212      *
076500020212     C     OC4BAR        IFEQ      'S'
076600020212     C     §3LBAR        ANDEQ     'N'
076700020212     C                   GOTO      NOLEGG
076800020212     C                   ENDIF
076900020212      *
077000020212     C     OC4FED        IFEQ      'S'
077100020212     C     §3LFED        ANDEQ     'N'
077200020212     C                   GOTO      NOLEGG
077300020212     C                   ENDIF
077400020212      *
077500020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
077600020212     C*  dati degli altri 2° lvl
077700020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
077800020212     C   14              MOVEL     LV2(WB)       LIN(1)
077900020212     C*
078000020212     C* IMPOSTO LA STAMPANTE
078100020212     C* STAMPANTE TABULATI
078200020212     C                   MOVEL     '5P'          KCOD
078300020212     C                   MOVEL     'T'           W004A             4
078400020212     C                   MOVE      LV2(WB)       W004A
078500020212     C                   MOVEL(P)  W004A         KKEY
078600020212     C     KTAB1         CHAIN     TABEL00F                           99
078700020212     C     *IN99         IFEQ      *OFF
078800020212     C                   MOVEL     TBLUNI        DS5P
078900020212     C                   MOVEL     §5POTQ        £OUTQ            10
079000020212     C                   END
079100020212     C*
079200020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
079300020212     C                   EXSR      LEGGI
079400020212     C*
079500020212     C     NOLEGG        TAG
079600020212     C*
079700020212     C                   ADD       1             WB
079800020212     C                   ENDDO
079900020212     C*
080000020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
080100020212     C                   ELSE
080200020212     C*
080300020212     C* Lancio solo per la 2° lvl richiesta
080400020212     C   14              MOVEL     DS1LNP        LIN(1)
080500020212     C*
080600020212     C* STAMPANTE
080700020212     C* STAMPANTE TABULATI
080800020212     C                   MOVEL     '5P'          KCOD
080900020212     C                   MOVEL     'T'           W004A             4
081000020212     C                   MOVE      DS1LNP        W004A
081100020212     C                   MOVEL(P)  W004A         KKEY
081200020212     C     KTAB1         CHAIN     TABEL00F                           99
081300020212     C     *IN99         IFEQ      *OFF
081400020212     C                   MOVEL     TBLUNI        DS5P
081500020212     C                   MOVEL     §5POTQ        £OUTQ            10
081600020212     C                   END
081700020212     C*
081800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
081900020212     C                   EXSR      LEGGI
082000020212     C*
082100020212     C                   ENDIF
082200020212     C                   ENDIF
082300020212     C*
082400020212     C                   ELSE
082500020212     C*
082600020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
082700020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
082800020212     C     DS1LNP        IFEQ      0
082900020212     C                   MOVEA     L1            LIN
083000020212     C* ...altrimenti elaboro solo se stessa
083100020212     C                   ELSE
083200020212     C                   MOVEL     DS1LNP        LIN(1)
083300020212     C                   ENDIF
083400020212     C*
083500020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
083600020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
083700020212     C* PRENDO QUELLA PRESENTE IN KPJBA
083800020212     C     DS1FGS        IFEQ      SIMFEL
083900020212     C                   MOVEL     KQOTD         £OUTQ
084000030627     C**!!!              MOVEL     KQOTD         £OUTQF
084100020212     C                   ELSE
084200020212     C* STAMPANTE TABULATI
084300020212     C                   MOVEL     '5P'          KCOD
084400020212     C                   MOVEL     'T'           W004A             4
084500020212     C                   MOVE      DS1FGS        W004A
084600020212     C                   MOVEL(P)  W004A         KKEY
084700020212     C     KTAB1         CHAIN     TABEL00F                           99
084800020212     C     *IN99         IFEQ      *OFF
084900020212     C                   MOVEL     TBLUNI        DS5P
085000020212     C                   MOVEL     §5POTQ        £OUTQ            10
085100020212     C                   END
085200020212     C                   ENDIF
085300020212     C*
085400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
085500020212     C                   EXSR      LEGGI
085600020212     C                   ENDIF
085700020212     C*
085800020212     C                   ENDSR
085900000000      **************************************************************************
086000000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
086100000000      **************************************************************************
086200000000     C     LEGGI         BEGSR
086300940502     C                   SETOFF                                       2729
086400000619     C                   SETON                                        43
086500000616     C                   CLEAR                   SAVRIS
086600970212     C*
086700970212      * Cancello dal file statistiche i record della giornata che vado a elabor.
086800970212      * Solo se richiesta elaborazione
086900970212    1C     DS1RIS        IFNE      'R'
087000000616     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
087100000616     C                   CLEAR                   KPJBU
087200000616     C                   CLEAR                   DSUL06
087300000616     C                   MOVE      '£1'          D06COD
087400000616     C                   MOVEL     SIMFEL        D06KEY
087500050802     C                   MOVE      'S'           D06KEY
087600050802     c                   movel     ds1ada        d06drf
087700000616     C                   MOVEL     DSUL06        KPJBU
087800000616     C                   CALL      'TRUL06R'
087900000616     C                   PARM                    KPJBA
088000000616     C                   MOVEL     KPJBU         DSUL06
088100000616     C**
088200000616     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
088300000616     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
088400000616     C*  CHE USO ALLA FINE
088500000616     C     DS1RIS        IFNE      ' '
088600000616     C                   MOVEA     LIN           SVLIN
088700000616     C                   MOVEL     DS1RIS        SAVRIS
088800000616     C                   CLEAR                   DS1RIS
088900000616     C                   ENDIF
089000000616     C*
089100000616     C                   MOVEA     T06           LIN
089200000616     C*
089300970212     C                   OPEN      FNSAT04L
089400970212     C                   Z-ADD     1             XX                3 0
089500970212    2C     LIN(XX)       DOWGT     *ZERO
089600970212     C*
089700970212     C     FNSATK        SETLL     FNSAT04L
089800970212     C     FNSATK        READE     FNSAT04L                               99
089900970212     C*
090000970212    3C     *IN99         DOWEQ     *OFF
090100970212     C                   DELETE    FNSAT004
090200970212     C     FNSATK        READE     FNSAT04L                               99
090300970212    3C                   ENDDO
090400970212     C                   ADD       1             XX
090500970212    2C                   ENDDO
090600970212     C*
090700970212     C                   CLOSE     FNSAT04L
090800970212     C                   END
090900940505      *
091000940505      * Come prima cosa devo verificare se giornata festiva
091100940505      *   solo per ELABORAZIONE o  per stampe giornaliere
091200950210     C     DS1FGS        IFEQ      0
091300950210     C     DS1RIS        ORNE      'R'
091400940505     C                   CLEAR                   $TFA
091500940505      *
091600950118     C     AZCLNK        CHAIN     AZCLN01L                           99
091700940505     C     *IN99         IFEQ      *OFF
091800950118     C                   MOVEA     CLNMAT        GME
091900970211     C     GME($GI)      IFEQ      'F'
092000970211     C* SE FESTIVO SCRIVO RECORD IN FNSAT CON TRE = F
092100970211     C                   CLEAR                   FNSAT000
092200970211     C                   Z-ADD     SIMFEL        SATFLE
092300970211     C                   MOVEL     'F'           SATTRE
092400970211     C                   Z-ADD     DS1ADA        SATDRE
092500040922     c                   movel     $ftr          satftr
092600040922     c                   z-add     $DTR          satdtr
092700970311     C                   WRITE     FNSAT000                             99
092800970211     C                   GOTO      ENDLEG
092900970211     C                   ENDIF
093000940505     C                   ENDIF
093100940505     C                   ENDIF
093200940429      *
093300940429     C* LANCIO DA FILTRO
093400950210    1C     DS1FGS        IFGT      0
093500940429     C* 29 ON - IMMESSA LINEA DI PARTENZA
093600950210    2C     DS1LNP        IFGT      0
093700940429     C                   SETON                                        29
093800950210    3C     DS1LNP        IFNE      DS1FEL
093900940502     C                   SETON                                        27
094000940429    3C                   ENDIF
094100940429    2C                   ENDIF
094200940429     C*
094300940429   X1C                   ELSE
094400940502     C                   SETON                                        05
094500940506     C                   CLEAR                   STRIP
094600940429     C* STAMPE GIORNALIERE
094700940429    2C     LIN(2)        IFEQ      0
094800940429     C                   SETON                                        29
094900940429    2C                   ENDIF
095000940429     C     SIMFEL        LOOKUP    LIN                                    99
095100940502     C  N99              SETON                                        27
095200940429    1C                   ENDIF
095300000912     C* SE SONO TUTTI P.O. POSTE, NON ENTRO NEMMENO NEL CICLO DI LETT
095400000912     C*   E RISTAMPA
095500000912     C                   Z-ADD     1             $X
095600000912     C                   CLEAR                   WSTAF
095700000912    1C     LIN($X)       DOWGT     0
095800000912     C     WSTAF         ANDEQ     ' '
095900000912     C                   Z-ADD     LIN($X)       $FILPR
096000000912     C* ESCLUDO I P.O. POSTE
096100020813     ***C                   CLEAR                   §OGPT
096200020813     C                   clear                   OG143
096300000912     C     $FILPR        CHAIN     AZORG01L                           82
096400000912     C  N82              MOVEL     ORGDE3        OG143
096500000912     C**
096600020813    2***C     §OGPT         IFNE      'S'
096700020813    2C                   IF        §OGntw <> 'PPT'
096800000912     C* SE ELABORATA ALMENO UNA LINEA, STAMPO ANCHE I TOTALI FINALI
096900000912     C                   MOVEL     'S'           WSTAF             1
097000000912    2C                   ENDIF
097100000912     C                   ADD       1             $X
097200000912    1C                   ENDDO
097300000000      *
097400000000      *
097500000912    1C     WSTAF         IFEQ      'S'
097600000000      * Scrivo, in stampa, se selezionata filiale o area
097700000000     C                   MOVE      *BLANKS       STRIP1
097800950210     C  N05DS1LNP        IFNE      *ZERO
097900960925     C                   MOVEL     'P.O.'        STRIP1
098000950210     C                   MOVE      DS1LNP        STRIP1
098100000000     C                   ELSE
098200940502     C                   MOVEL     'AREA'        STRIP1
098300950210     C                   MOVE      DS1FEL        STRIP1
098400000000     C                   ENDIF
098500000000      *
098600000000      * Inizializzo i campi per l'elaborazione
098700000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
098800000000     C                   Z-ADD     *ZERO         $FILPR
098900000000     C                   MOVE      *BLANKS       $CBLPR
099000000000     C                   Z-ADD     *ZERO         $NFVPR
099100940509     C                   CLEAR                   $RIGHE
099200950210      *
099300950210      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
099400950210     C                   MOVEA     £MOD          CMD(33)
099500950210     C                   MOVEA     £OUTQ         CMD(51)
099600950210      *
099700940502      * Eseguo OVRPRTF sul file di stampa
099800950210     C                   Z-ADD     82            LUNG             15 5
099900950213     C                   MOVEL     *BLANKS       COMMAN           82
100000940502     C                   MOVEA     CMD           COMMAN
100100940502     C                   CALL      'QCMDEXC'                            H1
100200940502     C                   PARM                    COMMAN
100300940502     C                   PARM                    LUNG
100400970807     C  N38              OPEN      FNLSC2P
100500950118     C  N38              WRITE     LSC2T1
100600950118     C  N38              WRITE     LSC2T3
100700940502     C  N38              SETOFF                                       30
100800000619     C**
100900000619     C* SE DS1RIS= 'S' ACCENDO PER ORA IL 38
101000000619     C     SAVRIS        IFNE      ' '
101100000619     C                   SETON                                        38
101200000619     C                   ENDIF
101300000000      *
101400950210      * L'elaborazione solo se non scelta la ristampa (DS1RIS='R')
101500950210    1C     DS1RIS        IFNE      'R'
101600000000      *
101700970205      * Leggo il file delle bolle in partenza prima quelli con data
101800970205      * F.V. = 0 poi quelli con DFV >= della data elaborazione
101900940401     C                   Z-ADD     1             $X                3 0
102000940428     C                   MOVE      *BLANK        $FLAG
102100940428     C*
102200940428    2C     LIN($X)       DOWGT     *ZERO
102300000912     C                   Z-ADD     LIN($X)       $FILPR
102400000912      * Escludo bolle LNP Poste
102500020813     ***C                   CLEAR                   §OGPT
102600020813     C                   clear                   OG143
102700000912     C     $FILPR        CHAIN     AZORG01L                           82
102800000912     C  N82              MOVEL     ORGDE3        OG143
102900000912     C**
103000020813   2A***C     §OGPT         IFNE      'S'
103100020813    2C                   IF        §OGntw <> 'PPT'
103200940428     C                   CLEAR                   $DFV
103300940428     C                   SETOFF                                           01
103400940428      *
103500940428    3C                   DO        2
103600021023     C     FNBLTK        SETLL     FNBLT38L
103700021023     C  N01FNBLTK        READE     FNBLT38L                               33
103800021023     C   01LIN($X)       READE     FNBLT38L                               33
103900940428     C*
104000940428    4C     *IN33         DOWEQ     *OFF
104100000000      *
104200000000      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
104300000000      * come test ma mi permette di saltare delle chain)
104400940428    5C     $BLTAT        IFNE      $BLTPR
104500950118     C     FNBLPK        CHAIN     FNBLP01L                           99
104600000000     C   99              GOTO      LETTUR
104700000000     C                   MOVE      $BLTAT        $BLTPR
104800000000     C                   Z-ADD     BLPNFV        $NFVPR
104900950118     C                   Z-ADD     BLPMGS        $DATSP            8 0
105000000000     C                   MOVEL     BLPAAS        $DATSP
105100000331     C                   Z-ADD     BLPDRT        $DTRIT
105200000413     C                   Z-ADD     BLPPDR        $CDPDR
105300940428     C* $BOLLA = 1 INDICA CHE HO LETTO UNA NUOVA BOLLA
105400940428     C                   MOVEL     '1'           $BOLLA            1
105500000000      *
105600000000      * Decodifico il tipo bolla
105700950118    6C     BLPCBO        IFNE      $CBLPR
105800950210     C                   MOVE      '3A'          KCOD
105900950210     C                   MOVE      *BLANKS       KKEY
106000950210     C                   MOVEL     BLPCBO        KKEY
106100950210     C     KTAB1         CHAIN     TABEL00F                           99
106200000000     C  N99              MOVEL     TBLUNI        DS3A
106300950118     C  N99              MOVE      BLPCBO        $CBLPR
106400000000     C   99              CLEAR                   DS3A
106500940428    6C                   ENDIF
106600940427     C*
106700940428    5C                   ENDIF
106800000000      *
106900940428      * ESCLUSIONI BOLLE
107000000000     C                   EXSR      ESCLUS
107100940428    5C     $OK           IFEQ      'S'
107200000000     C                   EXSR      ELABOR
107300940428    5C                   ENDIF
107400000000      *
107500940428     C     LETTUR        TAG
107600021023     C  N01FNBLTK        READE     FNBLT38L                               33
107700021023     C   01LIN($X)       READE     FNBLT38L                               33
107800940428    4C                   ENDDO
107900940428      *
108000940428     C                   SETON                                        01
108100970205     C                   Z-ADD     DS1AAL        $DFV
108200940428    3C                   ENDDO
108300940428      *
108400940428      * cambia la filiale --> decodifico e stampo i totali
108500940428     C                   EXSR      CALSPE
108600940428     C                   EXSR      WRISAT
108700940429     C                   EXSR      WR1SAT
108800940428     C  N38              EXSR      TOTFIL
108900940428     C                   EXSR      AZZFIL
109000000912   2AC                   ENDIF
109100940428     C*
109200940401     C                   ADD       1             $X
109300940428    2C                   ENDDO
109400940429      *
109500940429     C*
109600940429     C* STAMPA DEI DATI FINALI
109700940506     C                   EXSR      STAFIN
109800000616   X C                   ENDIF
109900000616      *
110000000616     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
110100000616     C     SAVRIS        IFEQ      'S'
110200000616     C                   MOVEL     'R'           DS1RIS
110300000616     C                   MOVEA     SVLIN         LIN
110400000616     C* AZZERO LE SCHIERE DI TOTALE
110500000616     C                   EXSR      AZZSER
110600000616     C                   EXSR      AZZTRA
110700000616     C                   EXSR      AZZTOT
110800000619     C                   SETOFF                                       38
110900000616     C                   ENDIF
111000000000      *
111100970211      * Nel caso di RISTAMPA mi limito a rileggere il file statistiche (FNSAT)
111200000000      * e stampare i record che rientrano nell'intervallo dal-al
111300000616     C     DS1RIS        IFNE      ' '
111400000000     C                   EXSR      RISTAM
111500940428    1C                   ENDIF
111600000000      *
111700000000      * Stampa terminata
111800940428     C     *IN38         IFEQ      *OFF
111900940429     C*
112000940428     C     $ALMEN        IFEQ      *ZERO
112100950118     C                   WRITE     LSC2E1
112200940505     C                   ELSE
112300940429     C*
112400000000     C                   EXSR      STATER
112500940505     C                   ENDIF
112600000000      *
112700950118     C                   WRITE     LSC2E2
112800950629     C                   WRITE     LSC2E5
112900000000      *
113000000000      * Verifico se vi sono fogli viaggio aperti
113100000000     C                   EXSR      FOGVIA
113200000000      *
113300970807     C                   CLOSE     FNLSC2P
113400940428     C                   ENDIF
113500000000      *
113600000000      * Aggiorno la data ultima elaborazione
113700950210     C     DS1RIS        IFNE      'R'
113800000616     C     SAVRIS        OREQ      'S'
113900021016
114000021016     c                   Eval      kTbeCod = 'SDF'
114100021016     c                   Movel(p)  Simfel        kTbeKe1
114200021016     c     Ktbe          Chain     Tntbe01l
114300021016     c                   If        %Found(Tntbe01l)
114400021016     c                   Movel     TbeUni        dSdf
114500021016     c                   If        §SdfTerP = *Zeros or §SdfTerP > Ds1Ada
114600021016     c                   Z-Add     Ds1Ada        §SdfTerP
114700021016     c                   EndIf
114800021016     c                   If        §SdfTerU < Ds1Ada
114900021016     c                   Z-Add     Ds1Ada        §SdfTerU
115000021016     c                   EndIf
115100021016     c                   Movel     dSdf          TbeUni
115200021016     c                   Update    Tntbe000
115300021016     c                   EndIf
115400000000     C                   ENDIF
115500000912     C*
115600000912    1C                   ENDIF
115700000000      *
115800940505     C     ENDLEG        ENDSR
115900000000      **************************************************************************
116000000000      * Una volta letto il record verifico se da includere o no
116100940427      * PER QUALSIASI TIPO DI CALCOLO  (COLLI E BOLLE A TERRA E
116200940427      *  COLLI E BOLLA DA PARTIRE)
116300000000      *
116400940427      * Vi sono solo due motivi per escludere il record letto:
116500940427      * (1) se la spedizione e' stata bollettata dopo la data elaboraz
116600950210      *                Data spedizione > DS1AAL
116700000000      *
116800940427      * (2) se la data partenza del collo e' < della data elaborazione
116900950210      *                BLTDFV < DS1AAL
117000000000      *
117100000810      * ATTENZIONE.. DA OGGI C'E' UN MOTIVO IN PIU'.. Canale5 ! da cantare ..
117200000810      *
117300000810      * (3) se la linea di partenze è ??..... POSTE !
117400000810      *
117500940427      * Bolle di recupero / bolle legate / bolle "£" sono escluse
117600000000      **************************************************************************
117700000000     C     ESCLUS        BEGSR
117800000000      *
117900000000     C                   MOVE      'N'           $OK               1
118000000331     C                   MOVE      *BLANKS       WSOS              1
118100000810      *
118200000810      *
118300940427      * esclusioni:
118400950323      * Le bolle "legate" sono identificabili dal nr.foglio viaggio
118500950323      * = 99999 O = 99998
118600070522    1C     $NFVPR        IFEQ      99999
118700950323     C     $NFVPR        OREQ      99998
118800940427     C     $CBLPR        OREQ      '£'
118900940428     C     §3ARBL        OREQ      'R'
119000000000     C                   GOTO      ENDESC
119100070522    1C                   ENDIF
119200000413     C* Prepagati non confermati
119300070522    1C     $CDPDR        IFEQ      *ZEROS
119400000413     C                   GOTO      ENDESC
119500070522    1C                   ENDIF
119600000000      *
119700070522    1C     BLTDFV        IFEQ      DS1AAL
119800950629     C                   MOVEL     'S'           $OK
119900070522   x1C                   ELSE
120000070522     C
120100070522     c* Elaborazione in base A     Data BORDERÒ
120200070522   1AC                   IF        DS1AAL<§VPOnpi
120300070522     C
120400940427      * La data di spedizione deve essere <= alla data elaborazione
120500070522    2C     $DATSP        IFLE      DS1AAL
120600000000      *
120700940427      * La data partenza deve essere > = alaa data elaborazione        limiti
120800070522    3C     BLTDFV        IFEQ      *ZERO
120900950210     C     BLTDFV        ORGE      DS1AAL
121000000000     C                   MOVE      'S'           $OK
121100000000     C                   GOTO      ENDESC
121200070522    3C                   ENDIF
121300070522   X2C                   ELSE
121400000331     C* Se data spedizione > di data elaborazione
121500000411     C* e  data ritiro    <= data elaborazione    ==> BOLLA IN SOSPENS.
121600070522    3C     $DTRIT        IFLE      DS1AAL
121700000331     C                   MOVE      'S'           $OK
121800000331     C                   MOVEL     'S'           WSOS
121900070522     C                   GOTO      ENDESC
122000070522    3C                   ENDIF
122100950626     C*
122200070522    2C                   ENDIF
122300070522     c
122400070522  x1ac                   else
122500070522     c****
122600070522     c* ELAborazione in base a    data RITIRO Merce
122700070522      * La data ritiro deve essere <= alla data elaborazione
122800070522    2C     $DTRIT        IFLE      DS1AAL
122900070522      *
123000070525      * La data partenza deve essere > = alla data elaborazione        limiti
123100070522    3C     BLTDFV        IFEQ      *ZERO
123200070522     C     BLTDFV        ORGE      DS1AAL
123300070522     C                   MOVE      'S'           $OK
123400070522     C                   GOTO      ENDESC
123500070522    3C                   ENDIF
123600070522    2C                   ENDIF
123700070522   1aC                   ENDIF
123800950629     C*
123900070522    1C                   ENDIF
124000000000      *
124100000000     C     ENDESC        ENDSR
124200000000      **************************************************************************
124300000000      * Elaborazioni dei record letti, somme nei totali
124400000000      **************************************************************************
124500000000     C     ELABOR        BEGSR
124600000000      *
124700970210      * Verifico il tipo spedizione ESPR./DIST./CORR.
124800950201     C                   Z-ADD     3             $T                1 0
124900000000     C     BLPTSP        IFEQ      'E'
125000000000     C                   Z-ADD     1             $T
125100000000     C                   ENDIF
125200950201     C     BLPTSP        IFEQ      'D'
125300000619     C     BLPTSP        OREQ      'P'
125400940323     C                   Z-ADD     2             $T
125500000000     C                   ENDIF
125600000000      *
125700000000      * C/servizio oppure no ?
125800940428     C     §3ARBL        COMP      'C'                                3434
125900000000      *
126000970205     C*
126100070522    1C     WSOS          IFEQ      *BLANKS
126200970205     C* DETERMINAZIONE SE COLLO A TERRA
126300000000      * Ho due controlli da fare per sapere se il collo e' a terra:
126400000000      *    1)  BLTDFV = 0
126500950210      *    2)  BLTDFV > Data spedizione   AND   BLTDFV > DS1AAL
126600000000     C                   SETOFF                                       36
126700070522    2C     BLTDFV        IFEQ      *ZERO
126800950210     C     BLTDFV        ORGT      DS1AAL
126900000000     C                   SETON                                        36
127000070522    2C                   ENDIF
127100970303     C*
127200970303     C* RICHIAMO PGM PER CALCOLO AFFIDABILITA'
127300970303     C* (SOLO SE HO LETTO UNA NUOVA BOLLA - $BOLLA = '1')
127400070522    2C     $BOLLA        IFEQ      '1'
127500070522    3C     BLPDCE        IFEQ      *ZEROS
127600970303     C     BLPDUC        ORNE      DS1AAL
127700970303     C                   MOVEL     ' '           D98TLA
127800970303     C                   MOVEL     'P'           D98TBO
127900970303     C                   MOVEL     BLPAAS        D98AAS
128000970303     C                   MOVEL     BLPLNP        D98LNP
128100970303     C                   MOVEL     BLPNRS        D98NRS
128200970303     C                   MOVEL     BLPNSP        D98NSP
128300970310     C                   Z-ADD     DS1AAL        D98DRF
128400970310     C                   Z-ADD     2359          D98ORF
128500970925     C                   CALL      'TNSD99R'
128600970925     C                   PARM                    DS99
128700970303     C                   ELSE
128800970303     C                   Z-ADD     BLPNPI        D98NPI
128900070522    3C                   END
129000070522    2C                   END
129100970207     C*
129200970207     C*VERIFICO SE SI TRATTA DI "A TERRA DA PARTIRE"
129300970210     C* La considero "non da partire" se si tratta di:
129400970207     C* - spedizione non borderizzata
129500970303     C* - distribuzione a terra del giorno
129600070522     c* - dal 1° giugno 2007 il calcolo della affidabilità a terra sarà
129700070522     c*   in base alla data ritiro, per cui nel DA PARTIRE non controllo
129800070522     c*   più la data borderò ma solo se si tratta della distribuzione
129900070522     c*   del giorno
130000970205     C                   SETOFF                                       35
130100070522     c
130200070522    2C     *IN36         IFEQ      *ON
130300070522     c
130400070522    3c                   if        ds1aal<§vponpi
130500970210     C*
130600070522    4C     BLPDBR        IFGT      DS1AAL
130700070522     C     BLPDBR        OREQ      *ZEROS
130800070522     C                   SETON                                        35
130900070522   x4C                   ELSE
131000970304     C*
131100070522    5C     BLPDBR        IFLE      DS1AAL
131200070522     C     BLPDPC        ANDEQ     0
131300070522     C     D98NPI        ANDLT     0
131400070522     C*
131500070522     C     BLPDBR        ORLE      DS1AAL
131600070522     C     BLPDPC        ANDGT     DS1AAL
131700070522     C     D98NPI        ANDLT     0
131800070522     C                   SETON                                        35
131900070522    5C                   END
132000070522    4C                   END
132100070522     c
132200070522   x3c                   else
132300070522     c*
132400090108     C* SE affidabilità =0 verifico se cade in giorno festivo, per
132500090108     c*    ter-ter --> se si non do ritardo
132600090108     c                   if        d98npi=0
132700090108     C                   movel     blptfa        $TFA
132800090108      *
132900090108     C     AZCLNK        CHAIN     AZCLN01L                           99
133000090108     C                   MOVEA     CLNMAT        GME
133100090108     C                   MOVEA     CLNpom        GMEp
133200090108     C     GME($GI)      IFEQ      'F'
133300090108     C     GMEp($GI)     oreq      'F'
133400090108     c                   sub       24            d98npi
133500090108     c                   endif
133600090108     c                   endif
133700090108     c
133800070522    4c                   if        (blpdpc=0 and d98npi<0) or
133900070522     c                             (blpdpc>ds1aal and d98npi<0)
134000070522     C                   SETON                                        35
134100070522    4c                   endif
134200070522    3c                   endif
134300970210     C*
134400070522    2C                   END
134500070522    1C                   ENDIF
134600970205     C*
134700000000      *
134800970205      * Aggancio, nel file statistiche, il record per il terminal di
134900970205      * arrivo poiche' lo dovrò aggiornare
135000970205     C                   CLEAR                   FNSAT000
135100000000     C                   Z-ADD     SIMFEL        SATFLE
135200950210     C                   Z-ADD     DS1AAL        SATDRE
135300940428     C                   Z-ADD     BLPLNP        SATLNP
135400000000     C                   Z-ADD     BLPTFA        SATTFA
135500000000     C                   MOVE      *BLANK        SATTRE
135600970205     C     FNSA1K        CHAIN     FNSAT02L                           37
135700940428      *
135800970205      * NUOVA BOLLA LETTA: CONTO LE SPEDIZIONI
135900940428      *
136000940428     C     $BOLLA        IFEQ      '1'
136100940428     C                   EXSR      DATBOL
136200970205     C                   ENDIF
136300021217      *
136400021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
136500030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
136600030213      * Peso medio
136700030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
136800030213if  1c***                if        Wsipes = 'S'
136900030213     c***                z-add     BLTpuc        $Peso
137000030213   x1c***                else
137100021220     c                   z-add     Wpesmed       $Peso
137200030213e   1c***                endif
137300021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
137400030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
137500030213      * Volume medio
137600030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
137700030213if  1c***                if        Wsivol = 'S'
137800030213     c***                z-add     BLTvuc        $Volum
137900030213   x1c***                else
138000021220     c                   z-add     Wvolmed       $Volum
138100030213e   1c***                endif
138200000000      *
138300000000      * ?  CALCOLI  SUI  COLLI  ?
138400000000      *
138500000331     C     WSOS          IFEQ      *BLANKS
138600000000      * Nr.colli da partire (servira' per la percentuale)
138700970207     C     *IN35         IFEQ      *OFF
138800970210     C   34              ADD       1             CPA($T)
138900970210     C  N34              ADD       1             C4A($T)
139000970210     C                   ADD       1             CDT($T)
139100950626     C                   END
139200000000      *
139300940427     C     *IN36         IFEQ      *ON
139400970207      * A terra da partire                                             (35ON)
139500940427     C   34
139600940427     CANN35              ADD       1             CBP($T)
139700940427     C  N34
139800940427     CANN35              ADD       1             C4P($T)
139900970207     C  N35              ADD       1             C0P($T)
140000970207     C  N35              ADD       1             C3P($T)
140100970207      * A terra non da partire                                         (35ON)
140200940427     C   34
140300940427     CAN 35              ADD       1             CBR($T)
140400940427     C  N34
140500940427     CAN 35              ADD       1             C4R($T)
140600940427     C   35              ADD       1             C0R($T)
140700940427     C                   ENDIF
140800000000      *
140900000000      * ?  CALCOLI  SUL  PESO  ?
141000000000      *
141100970207     C     *IN35         IFEQ      *OFF
141200970210     C   34              ADD       $PESO         PPA($T)
141300970210     C  N34              ADD       $PESO         P4A($T)
141400970210     C                   ADD       $PESO         PDT($T)
141500950626     C                   END
141600940502      * A TERRA
141700940427     C     *IN36         IFEQ      *ON
141800940502      *
141900970206      * Fino a 35 quintali
142000021217     ***C     BLPPKF        IFLE      3500
142100021217     C     Wpeso         IFLE      3500
142200970207     C* Da partire
142300970206     C   34
142400970206     CANN35              ADD       $PESO         PFI($T)
142500970206     C  N34
142600970206     CANN35              ADD       $PESO         P4I($T)
142700970206     C  N35              ADD       $PESO         P3I($T)
142800970206     C  N35              ADD       $PESO         P0I($T)
142900970207     C* Non da partire
143000970206     C   34
143100970206     CAN 35              ADD       $PESO         PFN($T)
143200970206     C  N34
143300970206     CAN 35              ADD       $PESO         P4N($T)
143400970206     C   35              ADD       $PESO         P0N($T)
143500970206     C                   ELSE
143600970206      * Oltre 35 quintali:                                             (35ON)
143700970207     C* Da partire
143800940427     C   34
143900940427     CANN35              ADD       $PESO         PBP($T)
144000940427     C  N34
144100940427     CANN35              ADD       $PESO         P4P($T)
144200970206     C  N35              ADD       $PESO         P0P($T)
144300970206     C  N35              ADD       $PESO         P3P($T)
144400970207     C* Non da partire
144500940427     C   34
144600940427     CAN 35              ADD       $PESO         PBR($T)
144700940427     C  N34
144800940427     CAN 35              ADD       $PESO         P4R($T)
144900940427     C   35              ADD       $PESO         P0R($T)
145000940427     C                   ENDIF
145100970206     C                   ENDIF
145200000331     C*
145300000331     C                   ENDIF
145400000000      *
145500000000      * ?  CALCOLI  SUL  VOLUME  ?
145600940329      *
145700000000      *
145800021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
145900021217     ***C     BLTVUC        IFEQ      *ZERO
146000021217     *** *
146100021217     ***C     BLPVLF        DIV(H)    BLPNCL        $VOLUM           12 4
146200021217     *** *
146300021217     ***C                   ELSE
146400021217     ***C                   Z-ADD     BLTVUC        $VOLUM
146500021217     ***C                   ENDIF
146600000000      *
146700000331     C     WSOS          IFEQ      *BLANKS
146800970207     C     *IN35         IFEQ      *OFF
146900970210     C   34              ADD       $VOLUM        VPA($T)
147000970210     C  N34              ADD       $VOLUM        V4A($T)
147100970210     C                   ADD       $VOLUM        VDT($T)
147200950626     C                   END
147300940505      *
147400940428     C     *IN36         IFEQ      *ON
147500970207     C* Da partire
147600940428     C   34
147700940428     CANN35              ADD       $VOLUM        VBP($T)
147800940428     C  N34
147900940428     CANN35              ADD       $VOLUM        V4P($T)
148000970206     C  N35              ADD       $VOLUM        V0P($T)
148100970206     C  N35              ADD       $VOLUM        V3P($T)
148200000331     c* Sia che sia da partire che non da partire: sommo nel totale
148300000331     C* Mc rimasti a magazzino
148400000331     C   34              ADD       $VOLUM        VBR($T)
148500000331     C  N34              ADD       $VOLUM        V4R($T)
148600000331     C                   ADD       $VOLUM        V0R($T)
148700000406     C                   ADD       $VOLUM        V3R($T)
148800000000      *
148900000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
149000000000      *
149100970205      * Visto che le spedizioni non riesco a calcolarle ora, mi limito
149200970205      * a tenere aggiornato il file di work con le bolle
149300940428     C   34              MOVE      *BLANK        WRKCSE
149400940428     C  N34              MOVE      'C'           WRKCSE
149500940503     C                   Z-ADD     BLPLNP        WRKLNP
149600950118     C     FNWR1K        CHAIN     FNWRK00F                           99
149700940428     C     *IN99         IFEQ      *OFF
149800000000     C                   ADD       1             WRKNC1
149900000000     C                   EXCEPT    AGGIO
150000000000     C                   ELSE
150100950118     C                   MOVEL     *BLANKS       WTRANS            1
150200940503     C                   EXSR      WRIWRK
150300000000     C                   ENDIF
150400940428     C                   ENDIF
150500000331     c* Se collo di bolla in sospensione sommo volume in totale mc
150600000331     c* rimasti a magazzino
150700000331     C                   ELSE
150800000331     C   34              ADD       $VOLUM        VBR($T)
150900000331     C  N34              ADD       $VOLUM        V4R($T)
151000000331     C                   ADD       $VOLUM        V0R($T)
151100000406     C                   ADD       $VOLUM        V3R($T)
151200000412     C                   ADD       $VOLUM        VOLUME            9 4
151300000331     C                   ENDIF
151400940323      *
151500040921     C                   MOVE      $FTR          SATFTR
151600040921     C                   MOVE      $DTR          SATDTR
151700940428     C     *IN37         IFEQ      *ON
151800040921     C****               MOVE      $FTR          SATFTR
151900040921     C****               MOVE      $DTR          SATDTR
152000970205     C                   WRITE     FNSAT000
152100940428     C                   ELSE
152200970205     C                   UPDATE    FNSAT000
152300940428     C                   ENDIF
152400000000      *
152500000000     C                   ENDSR
152600940428      **************************************************************************
152700940428      * Carico i dati della bolla (peso e volume)
152800940428      *  e conto le spedizioni partite il giorno di elaborazione
152900940428      **************************************************************************
153000940428     C     DATBOL        BEGSR
153100940428      *
153200021217     *** * Calcolo il peso di un singolo collo
153300021217     ***C     BLPPKF        DIV(H)    BLPNCL        $PESO             8 1
153400021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
153500021217     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
153600021217     *** * BLPVLC=Volume rilevato)
153700050112     ***C     BLPNCR        ifeq      BLPNCL
153800021217     ***C                   Z-ADD     BLPVLC        BLPVLF
153900021217     ***C                   ELSE
154000021217     ***C     BLPVLC        IFGE      BLPVLF
154100021217     ***C                   Z-ADD     BLPVLC        BLPVLF
154200021217     ***C                   ENDIF
154300021217     ***C                   ENDIF
154400021217      *
154500021217      * Verifico i dati misurati con la macchina smistacolli
154600021217      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
154700021220      * e calcolo il VOLUME unitario medio per collo da usare se non
154800021220      * tutti i colli sono stati volumati dalla smistacolli
154900021217      *
155000030213      **** Verifico se sono stati volumati tutti i colli
155100030213     c***                clear                   Wsivol
155200021217     c                   clear                   Wvolu
155300050112     c                   if        BLPncr = BLPncl
155400021217     c                   z-add     BLPvlc        Wvolu
155500030213     c***                eval      Wsivol =  'S'
155600021217   x1c                   else
155700021217if  2c                   if        BLPvlc >= BLPvlf
155800021217     c                   z-add     BLPvlc        Wvolu
155900021217   x2c                   else
156000021217     c                   z-add     BLPvlf        Wvolu
156100021217e   2c                   endif
156200021217e   1c                   endif
156300021220      *
156400021220      * Calcolo il volume medio
156500021220     c     Wvolu         div(H)    BLPncl        Wvolmed
156600021217      *
156700021217      * Verifico i dati misurati con la macchina smistacolli
156800021217      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
156900021220      * e calcolo il PESO   unitario medio per collo da usare se non
157000021220      * tutti i colli sono stati pesati   dalla smistacolli
157100021217      *
157200030213      **** Verifico se sono stati pesati   tutti i colli
157300030213     c***                clear                   Wsipes
157400021217     c                   clear                   Wpeso
157500050112     c                   If        BLPncp = BLPncl
157600021217     c                   z-add     BLPpkc        Wpeso
157700030213     c***                eval      Wsipes =  'S'
157800021217   c1c                   else
157900021217if  2c                   if        BLPpkc >= BLPpkf
158000021217     c                   z-add     BLPpkc        Wpeso
158100021217   x2c                   else
158200021217     c                   z-add     BLPpkf        Wpeso
158300021217e   2c                   endif
158400021217e   1c                   endif
158500021220      *
158600021220      * Calcolo il peso medio
158700021220     c     Wpeso         div(H)    BLPncl        Wpesmed
158800940428     C*
158900940428     C* SOMMO SOLTANTO LE SPEDIZIONI PARTITE NEL GIORNO
159000970205     C*  LA DATA DI RIFERIMENTO E' LA DATA ULTIMO COLLO
159100940506     C     $FLAG         IFEQ      ' '
159200970205     C     BLPDUC        ANDEQ     DS1AAL
159300000331     C     WSOS          ANDEQ     *BLANKS
159400950118     C                   MOVEL     *BLANKS       WTRANS
159500940503     C                   EXSR      CTRBOL
159600970205     C                   ENDIF
159700940428     C*
159800940428     C                   MOVEL     '2'           $BOLLA
159900940428     C                   ENDSR
160000950118      **************************************************************************
160100021217      * Carico i dati della bolla (peso e volume) (TRANSITI)
160200021217     *** *  e conto le spedizioni partite il giorno di elaborazione
160300021217     *** *    (questo NON lo faceva NEANCHE prima)
160400950118      **************************************************************************
160500950118     C     DATBOT        BEGSR
160600950118      *
160700021217     *** * Calcolo il peso di un singolo collo
160800021217     ***C     BTPPKB        DIV(H)    BTPNCL        $PESO             8 1
160900021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
161000021217     *** * misurati con la macchina smistacolli (BTPNCR=Numero colli rilevati,
161100021217     *** * BTPVLC=Volume rilevato)
161200050112     ***C     BTPNCR        ifeq      BTPNCL
161300021217     ***C                   Z-ADD     BTPVLC        BTPVLB
161400021217     ***C                   ELSE
161500021217     ***C     BTPVLC        IFGE      BTPVLB
161600021217     ***C                   Z-ADD     BTPVLC        BTPVLB
161700021217     ***C                   ENDIF
161800021217     ***C                   ENDIF
161900021217     ***C*
162000021220      *
162100021220      * Verifico i dati misurati con la macchina smistacolli
162200021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
162300021220      * e calcolo il VOLUME unitario medio per collo da usare se non
162400021220      * tutti i colli sono stati volumati dalla smistacolli
162500021217      *
162600030213      **** Verifico se sono stati volumati tutti i colli
162700030213     c***                clear                   Wsivol
162800021217     c                   clear                   Wvolu
162900050112     c                   if        BTPncr = BTPncl
163000021217     c                   z-add     BTPvlc        Wvolu
163100030213     c***                eval      Wsivol =  'S'
163200021217   x1c                   else
163300021217if  2c                   if        BTPvlc >= BTPvlb
163400021217     c                   z-add     BTPvlc        Wvolu
163500021217   x2c                   else
163600021217     c                   z-add     BTPvlb        Wvolu
163700021217e   2c                   endif
163800021217e   1c                   endif
163900021220      *
164000021220      * Calcolo il volume medio
164100021220     c     Wvolu         div(H)    BTPncl        Wvolmed
164200021220      *
164300021220      * Verifico i dati misurati con la macchina smistacolli
164400021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
164500021220      * e calcolo il PESO   unitario medio per collo da usare se non
164600021220      * tutti i colli sono stati pesati   dalla smistacolli
164700021217      *
164800030213      **** Verifico se sono stati pesati   tutti i colli
164900030213     c***                clear                   Wsipes
165000021217     c                   clear                   Wpeso
165100050112     c                   if        BTPncp = BTPncl
165200021217     c                   z-add     BTPpkc        Wpeso
165300030213     c***                eval      Wsipes =  'S'
165400021217   x1c                   else
165500021217if  2c                   if        BTPpkc >= BTPpkb
165600021217     c                   z-add     BTPpkc        Wpeso
165700021217   x2c                   else
165800021217     c                   z-add     BTPpkb        Wpeso
165900021217e   2c                   endif
166000021217e   1c                   endif
166100021220      *
166200021220      * Calcolo il peso medio
166300071231     c     Wpeso         div(H)    BTPncl        Wpesmed
166400021217      *
166500950118     C                   MOVEL     '2'           $BOLLA
166600950118     C                   ENDSR
166700940503      **************************************************************************
166800950118      * Scrivo il file FNWRK00F
166900940503      **************************************************************************
167000940503     C     WRIWRK        BEGSR
167100950118     C*
167200940503     C                   MOVE      $FLAG         WRKFLG
167300950118     C                   Z-ADD     1             WRKNC1
167400950118     C*
167500950118     C     WTRANS        IFNE      'T'
167600940503     C                   Z-ADD     BLPAAS        WRKAAS
167700940503     C                   Z-ADD     BLPMGS        WRKMGS
167800940503     C                   Z-ADD     BLPNRS        WRKNRS
167900940503     C                   Z-ADD     BLPNSP        WRKNSP
168000940503     C                   Z-ADD     BLPLNA        WRKLNA
168100940503     C                   MOVE      BLPTSP        WRKTSP
168200940503     C                   Z-ADD     BLPNCL        WRKNCL
168300940503     C                   Z-ADD     BLPTFA        WRKTFA
168400070525     c* Se dopo la data di attivazione, memorizzo la data borderò
168500070525    3c                   if        ds1aal<§vponpi
168600970207     C                   Z-ADD     BLPDBR        WRKDBR
168700070525     c                   else
168800070525     C                   Z-ADD     BLPDrt        WRKDBR
168900070525     c                   endif
169000070525     c
169100970303     C                   Z-ADD     BLPDPC        WRKDPC
169200970303     C                   Z-ADD     BLPDUC        WRKDUC
169300970303     C                   Z-ADD     D98NPI        WRKNPI
169400950118     C*
169500950118     C                   ELSE
169600950118     C*
169700950118     C                   Z-ADD     BTPAAS        WRKAAS
169800950118     C                   Z-ADD     BTPMGS        WRKMGS
169900950118     C                   Z-ADD     BTPNRS        WRKNRS
170000950118     C                   Z-ADD     BTPNSP        WRKNSP
170100950118     C                   Z-ADD     BTPLNA        WRKLNA
170200950118     C                   MOVE      BTPTSP        WRKTSP
170300950118     C                   Z-ADD     BTPNCL        WRKNCL
170400950118     C                   Z-ADD     BTPTFA        WRKTFA
170500970207     C                   CLEAR                   WRKDBR
170600970303     C                   CLEAR                   WRKDUC
170700970303     C                   CLEAR                   WRKDPC
170800970303     C                   CLEAR                   WRKNPI
170900950118     C                   ENDIF
171000950118     C*
171100950118     C                   WRITE     FNWRK000
171200950118     C*
171300940503     C                   ENDSR
171400940503      **************************************************************************
171500940503      * Calcolo le spedizioni da partire:
171600950118      *  vedo se gia' contata prima di ricontarla(se esiste in FNWRK)
171700970207      *  Potrei averla già contata se i colli non sono sequenziali
171800940503      **************************************************************************
171900940503     C     CTRBOL        BEGSR
172000940503     C*
172100940505     C                   MOVEL     $FLAG         $FLAG2
172200940503     C                   MOVEL     'P'           $FLAG
172300940503     C                   MOVEL     BLPLNP        WRKLNP
172400940503     C                   CLEAR                   WRKCSE
172500950118     C     FNWR1K        SETLL     FNWRK00F                               99
172600940503     C* BOLLA NON TROVATA  LA SCRIVO
172700940503    1C     *IN99         IFEQ      *OFF
172800940503     C                   EXSR      WRIWRK
172900940503     C*
173000970211     C                   ADD       1             SDT($T)
173100970210     C   34              ADD       1             SPA($T)
173200940503     C* S4A --> PER C/S SPEDIZIONI DELL'AREA
173300940503     C*         PER I TRANSITI
173400970210     C  N34              ADD       1             S4A($T)
173500940503    1C                   ENDIF
173600940505     C                   MOVEL     $FLAG2        $FLAG
173700940503     C                   ENDSR
173800000000      **************************************************************************
173900000000      * Calcolo sulle spedizioni, lettura del file di work creato prima
174000000000      **************************************************************************
174100000000     C     CALSPE        BEGSR
174200940502     C                   SETOFF                                       3435
174300940428      * Verifico se e' transito
174400940502     C     $FLAG         COMP      'T'                                    35
174500000000      *
174600000000      * Leggo tutti le spedizioni della filiale da stampare dal file di work
174700970207    1C     $FLAG         IFEQ      *BLANK
174800950118     C     FNWRKK        SETLL     FNWRK00F
174900970207   X1C                   ELSE
175000950118     C     $FLAG         SETLL     FNWRK00F
175100970207    1C                   ENDIF
175200000000      *
175300970207    1C                   DO        *HIVAL
175400000000      *
175500970207    2C     $FLAG         IFEQ      *BLANK
175600950118     C     FNWRKK        READE(N)  FNWRK00F                               99
175700970207   X2C                   ELSE
175800950118     C     $FLAG         READE(N)  FNWRK00F                               99
175900970207    2C                   ENDIF
176000000000      *
176100970207    2C  N99              DO
176200000000      *
176300970210      * Verifico il tipo spedizione ESPR./DIST./CORR.
176400950201     C                   Z-ADD     3             $V                1 0
176500970207    3C     WRKTSP        IFEQ      'E'
176600000000     C                   Z-ADD     1             $V
176700970207    3C                   ENDIF
176800970207    3C     WRKTSP        IFEQ      'D'
176900940323     C                   Z-ADD     2             $V
177000970207    3C                   ENDIF
177100940428      *
177200940428      * Verifico se e' c/servizio
177300940502     C  N35WRKCSE        COMP      *BLANKS                                34
177400000000      *
177500000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
177600000000      * poiche' lo dovro aggiornare
177700970207     C                   CLEAR                   FNSAT000
177800000000     C                   Z-ADD     SIMFEL        SATFLE
177900950210     C                   Z-ADD     DS1ADA        SATDRE
178000940506     C  N35              Z-ADD     WRKLNP        SATLNP
178100940506     C   35              Z-ADD     SIMFEL        SATLNP
178200000000     C                   Z-ADD     WRKTFA        SATTFA
178300000000     C  N35              MOVE      *BLANK        SATTRE
178400000000     C   35              MOVE      'T'           SATTRE
178500970205     C     FNSA1K        CHAIN     FNSAT02L                           37
178600000000      *
178700950628     C                   MOVE      WRKMGS        $DATS1            8 0
178800950628     C                   MOVEL     WRKAAS        $DATS1
178900950628     C*
179000970207     C* A TERRA COMPLETAMENTE
179100970207      * La spedizione e' completa se il nr.colli spediti (WRKNC1) e' = uguale
179200050112      * al nr.colli della bolla (WRKNCL)
179300970207    3C     WRKNC1        IFEQ      WRKNCL
179400940506     C                   ADD       1             S0M($V)
179500970207     C* Transiti
179600970207    4C     *IN35         IFEQ      *ON
179700940506     C                   ADD       1             S2M($V)
179800970207   X4C                   ELSE
179900970207     C* Dell'area
180000940506     C   34              ADD       1             SCM($V)
180100940506     C  N34              ADD       1             S4M($V)
180200940506     C                   ADD       1             S3M($V)
180300970207     C*
180400970207    4C                   ENDIF
180500970207    3C                   ENDIF
180600970210     C* COMDAP = BLANKS => A TERRA DA PARTIRE
180700970207     C                   CLEAR                   COMDAP
180800970303     C* BORDERIZZATO, DISTRIBUZIONE A TERRA DEL GIORNO NON SPAREGGIATA
180900070525    3c                   if        ds1aal<§vponpi
181000070525     c
181100970207    3C     WRKDBR        IFGT      *ZEROS
181200970207     C     WRKDBR        ANDLE     DS1AAL
181300970303     C*
181400970303    4C     WRKDUC        IFEQ      *ZEROS
181500970303     C     WRKDPC        ANDEQ     0
181600970303     C     WRKNPI        ANDLT     0
181700970303     C*
181800970303     C     WRKDUC        OREQ      *ZEROS
181900970303     C     WRKDPC        ANDGT     DS1ADA
182000970303     C     WRKNPI        ANDLT     0
182100970303     C*
182200970303     C     WRKDUC        ORGT      DS1ADA
182300970303     C     WRKDPC        ANDGT     DS1ADA
182400970303     C     WRKNPI        ANDLT     0
182500970207     C                   MOVE      'N'           COMDAP            1
182600970207    4C                   END
182700970207   X3C                   ELSE
182800970207     C* NON BORDERIZZATO
182900970207     C                   MOVE      'N'           COMDAP
183000970207    3C                   END
183100070525     c*
183200070525     c                   else
183300070525     c
183400070525     c* calcolo per data ritiro a terra non da partire solo la distribu
183500070525     c*  zione del giorno
183600070525    4c                   if        (wrkdpc=0 and wrknpi<0) or
183700070525     c                             (wrkdpc>ds1aal and wrknpi<0)
183800070525     C                   MOVE      'N'           COMDAP
183900070525    4c                   endif
184000070525    3c                   endif
184100070525     c
184200970207     C*
184300970207    3C     COMDAP        IFEQ      *BLANKS
184400970207     C* Per i transiti memorizzo in un unico totale le spediz. a terra
184500970207     C* e non mi interessa nemmeno memorizzare il totale da partire
184600970207     C     $FLAG         OREQ      'T'
184700970207     C***
184800970207     C* TOTALE DA PARTIRE
184900970207     C***
185000970207     C* Sommo le spediz.a terra border. esclusa la distrib. del periodo
185100970210     C  N35              ADD       1             SDT($V)
185200970210     C  N35
185300970210     CAN 34              ADD       1             SPA($V)
185400970207     C* S4A --> Per c/s spedizioni dell'area
185500970210     C  N35
185600970210     CANN34              ADD       1             S4A($V)
185700970207     C***
185800970207     C* A TERRA DA PARTIRE
185900970207     C***
186000970207     C   34
186100970207     CANN35              ADD       1             SBP($V)
186200970207     C  N34
186300970207     CANN35              ADD       1             S4P($V)
186400970207     C  N35              ADD       1             S3P($V)
186500970207     C   35              ADD       1             S2P($V)
186600970207     C                   ADD       1             S0P($V)
186700970305     C***
186800970305     C* A TERRA DA PARTIRE FUORI AFFIDABILITA'
186900970305     C***
187000970305    4C     WRKNPI        IFGE      0
187100970305     C   34
187200970305     CANN35              ADD       1             FAF($V)
187300970305     C  N34
187400970305     CANN35              ADD       1             F4F($V)
187500970305     C  N35              ADD       1             F3F($V)
187600970305     C  N35              ADD       1             F0F($V)
187700970305    4C                   END
187800970207     c*
187900970207   X3C                   ELSE
188000970207     C***
188100970207     C* A TERRA NON DA PARTIRE
188200970207     C***
188300970207     C   34              ADD       1             SBR($V)
188400970207     C  N34              ADD       1             S4R($V)
188500970207     C                   ADD       1             S3R($V)
188600970207     C                   ADD       1             S0R($V)
188700970207    3C                   ENDIF
188800000000      *
188900040922     c  n37              move      $FTR          satftr
189000040922     c  n37              move      $DTR          satDTR
189100970207     C  N37              UPDATE    FNSAT000
189200000000      *
189300970207    2C                   ENDDO
189400970207    1C  N99              ENDDO
189500000000      *
189600000000     C                   ENDSR
189700000000      **************************************************************************
189800000000      * Calcolo i totali di filiale, li stampo e scrivo sul file
189900000000      **************************************************************************
190000000000     C     TOTFIL        BEGSR
190100000000      *
190200000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
190300000000      *
190400970211     C                   SETON                                        404145
190500000000     C                   Z-ADD     $FILPR        FILIAP
190600000000      *
190700000000      * Decodifico il codice filiale
190800000000     C     $FILPR        CHAIN     AZORG01L                           99
190900000000     C  N99ORGFVA        COMP      *BLANK                             9999
191000940503     C   99              CLEAR                   ORGDEP
191100000000     C  N99              MOVEL     ORGDES        ORGDEP
191200000000      *
191300000000      * ?  C  O   L   L  I  ?
191400970210     C                   XFOOT     CBP           COTOTP
191500970210     C                   XFOOT     CPA           $CDA
191600940428     C                   EXSR      PERCOL
191700000000      *
191800000000      * ?  P  E  S  O  ?
191900970210     C                   XFOOT     PFI           PEF35P
192000970210     C                   XFOOT     PBP           PEO35P
192100970210     C     PEF35P        ADD       PEO35P        PETOTP
192200970210     C                   XFOOT     PPA           $PDA
192300940428     C                   EXSR      PERPES
192400000000      *
192500000000      * ?  V  O  L  U  M E  ?
192600970210     C                   XFOOT     VBP           VOTOTP
192700970210     C                   XFOOT     VPA           $VDA
192800940428     C                   EXSR      PERVOL
192900000000      *
193000000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
193100970210     C                   XFOOT     SPA           $SDA
193200970305     C                   XFOOT     FAF           SPFAFF
193300940502     C                   EXSR      PERSPE
193400970305     C                   XFOOT     SBP           SPTOTP
193500970305     C                   XFOOT     SCM           SPCONP
193600970305     C                   XFOOT     SBR           $COMOD
193700970211     C     SPTOTP        ADD       $COMOD        SPTOTA
193800970211     C     SPTOTA        SUB       SPCONP        SPSPAP
193900000331      *
194000000331      * ? VOLUME A MAGAZZINO?
194100000331     C                   XFOOT     VBR           VOTOTM
194200000000      *
194300970210     C* STAMPO SOLO SE HO DEI DATI
194400000331     C     VOTOTM        IFGT      *ZEROS
194500000417     C* HO RIMESSO ANCHE TEST SU SPTOTA PER FAR FUNZIONARE LE RISTAMPE
194600000417     C* RELATIVE A DATE ELABORATE PRIMA CHE VENISSE FATTO IL CALCOLO
194700000417     C* DEL TOTALE VOLUME A MAGAZZINO
194800000417     C     SPTOTA        ORGT      *ZEROS
194900970210     C*
195000000000     C                   Z-ADD     $FILPR        FILIAP
195100000000     C                   EXSR      SCRIVI
195200940509     C                   ADD       1             $RIGHE            2 0
195300000000      *
195400970210      * ------------------ ESPR./DIST./CORR. --------------------------
195500000000      *
195600970210     C                   SETOFF                                       40
195700000000     C                   DO        3             $V
195800940429     C                   CLEAR                   DESCRP
195900940429      *
196000970210     C                   MOVE      ENS($V)       TIPOP
196100000000      *
196200000000      * ?  C  O   L   L  I  ?
196300970211     C                   Z-ADD     CBP($V)       COTOTP
196400970211     C                   Z-ADD     CPA($V)       $CDA
196500940428     C                   EXSR      PERCOL
196600000000      *
196700000000      * ?  P  E  S  O  ?
196800000000     C                   Z-ADD     PFI($V)       PEF35P
196900970211     C                   Z-ADD     PBP($V)       PEO35P
197000970212     C     PEF35P        ADD       PEO35P        PETOTP
197100970211     C                   Z-ADD     PPA($V)       $PDA
197200940428     C                   EXSR      PERPES
197300000000      *
197400000000      * ?  V  O  L  U  M E  ?
197500970211     C                   Z-ADD     VBP($V)       VOTOTP
197600970211     C                   Z-ADD     VPA($V)       $VDA
197700940428     C                   EXSR      PERVOL
197800000000      *
197900000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
198000970305     C                   Z-ADD     SPA($V)       $SDA
198100970305     C                   Z-ADD     FAF($V)       SPFAFF
198200940502     C                   EXSR      PERSPE
198300970305     C                   Z-ADD     SBP($V)       SPTOTP
198400970305     C                   Z-ADD     SCM($V)       SPCONP
198500970211     C     SBP($V)       ADD       SBR($V)       SPTOTA
198600970211     C     SPTOTA        SUB       SCM($V)       SPSPAP
198700000331      *
198800000331      * ?tot.volume a magazz?
198900000331     C                   Z-ADD     VBR($V)       VOTOTM
199000000000      *
199100000000     C                   EXSR      SCRIVI
199200000000      *
199300000000     C                   ENDDO
199400000000      *
199500950118     C                   WRITE     LSC2T2
199600940506     C                   ENDIF
199700940328      *
199800000417     C                   ENDSR
199900940428      **************************************************************************
200000940428      * Calcolo percentuale sui colli
200100940428      **************************************************************************
200200940428     C     PERCOL        BEGSR
200300940502     C                   Z-ADD     *ZERO         COSDPP
200400940428     C*
200500940502     C* PERCENTUALE SUL DA PARTIRE
200600970213     C  N02$CDA          IFNE      *ZERO
200700970210     C     COTOTP        DIV       $CDA          $COMOD
200800940502     C     $COMOD        MULT(H)   100           COSDPP
200900940502     C                   ENDIF
201000940428     C                   ENDSR
201100940428      **************************************************************************
201200940428      * Calcolo percentuale sul peso
201300940428      **************************************************************************
201400940428     C     PERPES        BEGSR
201500940502     C                   CLEAR                   PESDPP
201600940428     C*
201700940502     C* PERCENTUALE SUL DA PARTIRE
201800970213     C  N02$PDA          IFNE      *ZERO
201900970210     C     PETOTP        DIV       $PDA          $COMOD
202000940502     C     $COMOD        MULT(H)   100           PESDPP
202100940502     C                   ENDIF
202200940428     C                   ENDSR
202300940428      **************************************************************************
202400940428      * Calcolo percentuale sul volume
202500940428      **************************************************************************
202600940428     C     PERVOL        BEGSR
202700940502     C                   Z-ADD     *ZERO         VOSDPP
202800940502     C*
202900940502     C* PERCENTUALE SUL DA PARTIRE
203000970213     C  N02$VDA          IFNE      *ZERO
203100970210     C     VOTOTP        DIV       $VDA          $COMOD
203200940502     C     $COMOD        MULT(H)   100           VOSDPP
203300940502     C                   ENDIF
203400940428     C                   ENDSR
203500940428      **************************************************************************
203600940428      * Calcolo percentuale sulle spedizioni
203700940428      **************************************************************************
203800940428     C     PERSPE        BEGSR
203900940502     C                   Z-ADD     *ZERO         SPSDPP
204000940428     C*
204100940502     C* PERCENTUALE SUL DA PARTIRE
204200970213     C  N02$SDA          IFNE      *ZERO
204300970305     C     SPFAFF        DIV       $SDA          $COMOD
204400940502     C     $COMOD        MULT(H)   100           SPSDPP
204500940502     C                   ENDIF
204600940428     C                   ENDSR
204700000000      **************************************************************************
204800000000      * Azzeramento dei totali di filiale per nuovi conti
204900000000      **************************************************************************
205000000000     C     AZZFIL        BEGSR
205100000412     C                   CLEAR                   VOLUME
205200000000      *
205300000000      * COLLI
205400970211     C                   ADD       CPA(1)        C3A(1)
205500970211     C                   ADD       CPA(2)        C3A(2)
205600970211     C                   ADD       CPA(3)        C3A(3)
205700970211     C                   CLEAR                   CPA
205800970211     C                   CLEAR                   CBP
205900970211     C                   CLEAR                   CBR
206000970211      * PESI
206100970211     C                   ADD       PPA(1)        P3A(1)
206200970211     C                   ADD       PPA(2)        P3A(2)
206300970211     C                   ADD       PPA(3)        P3A(3)
206400970211     C                   CLEAR                   PFI
206500970211     C                   CLEAR                   PPA
206600970211     C                   CLEAR                   PBP
206700970211     C                   CLEAR                   PBR
206800970211     C                   CLEAR                   PFN
206900000000      * VOLUMI
207000970211     C                   ADD       VPA(1)        V3A(1)
207100970211     C                   ADD       VPA(2)        V3A(2)
207200970211     C                   ADD       VPA(3)        V3A(3)
207300970211     C                   CLEAR                   VPA
207400970211     C                   CLEAR                   VBP
207500970211     C                   CLEAR                   VBR
207600970211      * SPEDIZIONI
207700970211     C                   ADD       SPA(1)        S3A(1)
207800970211     C                   ADD       SPA(2)        S3A(2)
207900970211     C                   ADD       SPA(3)        S3A(3)
208000970211     C                   CLEAR                   SPA
208100970211     C                   CLEAR                   SBP
208200970211     C                   CLEAR                   SBR
208300970211     C                   CLEAR                   SCM
208400970305     C                   CLEAR                   FAF
208500000000      *
208600000000     C                   ENDSR
208700000000      **************************************************************************
208800000000      * Azzeramento dei totali di c/servizio per nuova area
208900000000      **************************************************************************
209000000000     C     AZZSER        BEGSR
209100000000      *
209200000000      * COLLI
209300940506     C                   ADD       C1A(1)        C3A(1)
209400970211     C                   ADD       C1A(2)        C3A(2)
209500970211     C                   ADD       C1A(3)        C3A(3)
209600000000     C                   CLEAR                   C1A
209700000000     C                   CLEAR                   C1P
209800000000     C                   CLEAR                   C1R
209900000000      * PESI
210000940506     C                   ADD       P1A(1)        P3A(1)
210100970211     C                   ADD       P1A(2)        P3A(2)
210200970211     C                   ADD       P1A(3)        P3A(3)
210300970211     C                   CLEAR                   P1I
210400970211     C                   CLEAR                   P1N
210500970211     C                   CLEAR                   P1P
210600970211     C                   CLEAR                   P1R
210700970211     C                   CLEAR                   P1A
210800970211      * VOLUMI
210900970211     C                   ADD       V1A(1)        V3A(1)
211000970211     C                   ADD       V1A(2)        V3A(2)
211100970211     C                   ADD       V1A(3)        V3A(3)
211200970211     C                   CLEAR                   V1A
211300970211     C                   CLEAR                   V1P
211400970211     C                   CLEAR                   V1R
211500000000      * SPEDIZIONI
211600940506     C                   ADD       S1A(1)        S3A(1)
211700970211     C                   ADD       S1A(2)        S3A(2)
211800970211     C                   ADD       S1A(3)        S3A(3)
211900970211     C                   CLEAR                   S1A
212000970211     C                   CLEAR                   S1P
212100970211     C                   CLEAR                   S1R
212200970211     C                   CLEAR                   S1M
212300970305     C                   CLEAR                   F1F
212400000000      *
212500000000     C                   ENDSR
212600000000      **************************************************************************
212700000000      * Azzeramento dei totali di c/servizio per nuova filiale
212800000000      **************************************************************************
212900000000     C     AZZSE4        BEGSR
213000000000      *
213100000000      * COLLI
213200970210     C                   CLEAR                   C4A
213300970210     C                   CLEAR                   C4P
213400970210     C                   CLEAR                   C4R
213500000000      * PESI
213600970210     C                   CLEAR                   P4A
213700970210     C                   CLEAR                   P4I
213800970210     C                   CLEAR                   P4P
213900970210     C                   CLEAR                   P4R
214000970210     C                   CLEAR                   P4N
214100000000      * VOLUMI
214200970210     C                   CLEAR                   V4A
214300970210     C                   CLEAR                   V4P
214400970210     C                   CLEAR                   V4R
214500000000      * SPEDIZIONI
214600970210     C                   CLEAR                   S4A
214700970210     C                   CLEAR                   S4P
214800970210     C                   CLEAR                   S4R
214900970210     C                   CLEAR                   S4M
215000970305     C                   CLEAR                   F4F
215100000000      *
215200000000     C                   ENDSR
215300000000      **************************************************************************
215400000000      * Azzeramento dei totali di transito per nuovi totali
215500000000      **************************************************************************
215600000000     C     AZZTRA        BEGSR
215700940506     C* SOMMO NEI TOTALI
215800940506     C                   EXSR      ADDTRA
215900000000      * COLLI
216000940428     C                   CLEAR                   C2P
216100000000      * PESI
216200000000     C                   CLEAR                   P2I
216300940428     C                   CLEAR                   P2P
216400000000      * VOLUMI
216500940428     C                   CLEAR                   V2P
216600000000      * SPEDIZIONI
216700940428     C                   CLEAR                   S2P
216800000000     C                   CLEAR                   S2M
216900000000      *
217000000000     C                   ENDSR
217100000000      **************************************************************************
217200000000      * Stampo i totali per c/servizio
217300000000      **************************************************************************
217400000000     C     TOTSER        BEGSR
217500000000      *
217600000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
217700000000      *
217800970211     C                   SETON                                        4045
217900000000     C                   SETOFF                                       41
218000000000     C                   Z-ADD     *ZERO         FILIAP
218100000000     C                   MOVE      *BLANKS       ORGDEP
218200000000     C     'C/SERVIZ'    CAT       'I':0         ORGDEP
218300000000      *
218400970211      * ?  C  O   L   L  I  ?
218500970211     C                   XFOOT     C1P           COTOTP
218600970211     C                   XFOOT     C1A           $CDA
218700970211     C                   EXSR      PERCOL
218800970211      *
218900970211      * ?  P  E  S  O  ?
219000970211     C                   XFOOT     P1I           PEF35P
219100970211     C                   XFOOT     P1P           PEO35P
219200970211     C     PEF35P        ADD       PEO35P        PETOTP
219300970211     C                   XFOOT     P1A           $PDA
219400940429     C                   EXSR      PERPES
219500000000      *
219600000000      * ?  V  O  L  U  M E  ?
219700970211     C                   XFOOT     V1P           VOTOTP
219800970211     C                   XFOOT     V1A           $VDA
219900940429     C                   EXSR      PERVOL
220000000000      *
220100000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
220200970211     C                   XFOOT     S1A           $SDA
220300970305     C                   XFOOT     F1F           SPFAFF
220400970211     C                   EXSR      PERSPE
220500970305     C                   XFOOT     S1P           SPTOTP
220600970305     C                   XFOOT     S1M           SPCONP
220700970211     C                   XFOOT     S1R           $COMO1
220800970211     C     SPTOTP        ADD       $COMO1        SPTOTA
220900970211     C     SPTOTA        SUB       SPCONP        SPSPAP
221000000331      *
221100000331      * ?tot.volume a magazz?
221200000331     C                   XFOOT     V1R           VOTOTM
221300970211      *
221400970211     C* STAMPO SOLO SE HO DEI DATI
221500000331     C     VOTOTM        IFGT      *ZEROS
221600970211     C*
221700970211     C                   EXSR      SCRIVI
221800000000      *
221900970211      * ------------------ ESPR./DIST./CORR. --------------------------
222000000000      *
222100970211     C                   SETOFF                                       40
222200000000     C                   DO        3             $V
222300940323     C                   MOVE      ENS($V)       TIPOP
222400000000      *
222500000000      * ?  C  O   L   L  I  ?
222600970211     C                   Z-ADD     C1P($V)       COTOTP
222700970211     C                   Z-ADD     C1A($V)       $CDA
222800940429     C                   EXSR      PERCOL
222900000000      *
223000000000      * ?  P  E  S  O  ?
223100000000     C                   Z-ADD     P1I($V)       PEF35P
223200970211     C                   Z-ADD     P1P($V)       PEO35P
223300970211     C     PEF35P        ADD       PEO35P        PETOTP
223400970211     C                   Z-ADD     P1A($V)       $PDA
223500940429     C                   EXSR      PERPES
223600000000      *
223700000000      * ?  V  O  L  U  M E  ?
223800970211     C                   Z-ADD     V1P($V)       VOTOTP
223900970211     C                   Z-ADD     V1A($V)       $VDA
224000940429     C                   EXSR      PERVOL
224100000000      *
224200000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
224300970211     C                   Z-ADD     S1A($V)       $SDA
224400970305     C                   Z-ADD     F1F($V)       SPFAFF
224500940502     C                   EXSR      PERSPE
224600970305     C                   Z-ADD     S1P($V)       SPTOTP
224700970305     C                   Z-ADD     S1M($V)       SPCONP
224800970211     C     S1P($V)       ADD       S1R($V)       SPTOTA
224900970211     C     SPTOTA        SUB       S1M($V)       SPSPAP
225000000331      *
225100000331      * ?tot volume a magazz?
225200000331     C                   Z-ADD     V1R($V)       VOTOTM
225300000000      *
225400000000     C                   EXSR      SCRIVI
225500000000      *
225600000000     C                   ENDDO
225700940504     C                   ENDIF
225800940429     C                   EXSR      AZZSER
225900000000      *
226000000000     C                   ENDSR
226100000000      **************************************************************************
226200000000      * Stampo i totali per transito
226300000000      **************************************************************************
226400000000     C     TOTTRA        BEGSR
226500000000      *
226600000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
226700000000      *
226800940506     C                   SETON                                        400203
226900970211     C                   SETOFF                                       45
227000000000     C                   MOVE      *BLANKS       ORGDEP
227100940502     C* stampa per linea: totale transiti
227200940502     C     *IN43         IFEQ      *ON
227300940502     C                   Z-ADD     *ZERO         FILIAP
227400940502     C                   MOVEL     'TRANSITI'    ORGDEP
227500940502     C                   SETOFF                                       41
227600940502     C                   ELSE
227700940502     c* stampa per terminal in transito
227800940502     C                   SETON                                        41
227900940502      * Decodifico il terminal di arrivo
228000940502     C                   Z-ADD     $FILPR        FILIAP
228100940502     C     $FILPR        CHAIN     AZORG01L                           99
228200940502     C  N99ORGFVA        COMP      *BLANK                             9999
228300940503     C   99              CLEAR                   ORGDEP
228400940502     C  N99              MOVEL     ORGDES        ORGDEP
228500940502     C                   ENDIF
228600000000      *
228700000000      * ?  C  O   L   L  I  ?
228800940502     C                   XFOOT     C2P           COTOTP
228900970211     C*
229000970211      * SOLO SE HO TROVATO ALEMNO UN COLLO A TERRA STAMPO
229100970211     C     COTOTP        IFGT      0
229200940504      *
229300000000      *
229400000000      * ?  P  E  S  O  ?
229500970211     C                   XFOOT     P2I           PEF35P
229600970211     C                   XFOOT     P2P           PEO35P
229700970211     C     PEO35P        ADD       PEF35P        PETOTP
229800000000      *
229900000000      * ?  V  O  L  U  M E  ?
230000940428     C                   XFOOT     V2P           VOTOTP
230100000000      *
230200000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
230300940428     C                   XFOOT     S2P           SPTOTP
230400970211     C                   XFOOT     S2P           SPTOTA
230500970211     C                   XFOOT     S2M           SPCONP
230600970211     C     SPTOTA        SUB       SPCONP        SPSPAP
230700000000      *
230800000000     C                   EXSR      SCRIVI
230900940509     C                   ADD       1             $RIGHE
231000000000      *
231100970211      * ------------------ ESPR./DIST./CORR. --------------------------
231200000000      *
231300940502     C                   SETOFF                                       4045
231400000000     C                   DO        3             $V
231500940502     C* PER TERMINAL SCRIVO CHE E' RECORD DI TRANSITO
231600940429     C  N43$V            IFEQ      1
231700940506     C                   MOVEL     'IN TRANS'    DESCRP
231800940429     C                   ELSE
231900940429     C                   CLEAR                   DESCRP
232000940429     C                   ENDIF
232100940429     C*
232200940323     C                   MOVE      ENS($V)       TIPOP
232300000000      *
232400000000      * ?  C  O   L   L  I  ?
232500940428     C                   Z-ADD     C2P($V)       COTOTP
232600000000      *
232700000000      * ?  P  E  S  O  ?
232800970211     C                   Z-ADD     P2I($V)       PEF35P
232900970211     C                   Z-ADD     P2P($V)       PEO35P
233000970211     C     PEO35P        ADD       PEF35P        PETOTP
233100000000      *
233200000000      * ?  V  O  L  U  M E  ?
233300940428     C                   Z-ADD     V2P($V)       VOTOTP
233400000000      *
233500000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
233600940428     C                   Z-ADD     S2P($V)       SPTOTP
233700970211     C                   Z-ADD     S2P($V)       SPTOTA
233800970211     C                   Z-ADD     S2M($V)       SPCONP
233900970211     C     SPTOTA        SUB       S2M($V)       SPSPAP
234000000000      *
234100000000     C                   EXSR      SCRIVI
234200000000      *
234300000000     C                   ENDDO
234400940428     C*
234500940428     C                   EXSR      AZZTRA
234600940502      *
234700950118     C  N38              WRITE     LSC2T2
234800940506     C                   ENDIF
234900940506     C                   SETOFF                                       0302
235000000000      *
235100000000     C                   ENDSR
235200000000      **************************************************************************
235300970211      * Scrittura del record sul file di trasmissione FNSAT00F
235400000000      **************************************************************************
235500000000     C     WRISAT        BEGSR
235600000000      *
235700970207     C                   CLEAR                   FNSAT000
235800000000     C                   Z-ADD     SIMFEL        SATFLE
235900000000     C                   Z-ADD     $FILPR        SATLNP
236000000000     C                   Z-ADD     *ZERO         SATTFA
236100950210     C                   Z-ADD     DS1ADA        SATDRE
236200000000     C                   MOVE      *BLANK        SATTRE
236300000000      *
236400000000      * COLLI: da partire
236500970210     C                   Z-ADD     CPA(1)        SATCPE
236600970210     C                   Z-ADD     CPA(2)        SATCPD
236700970210     C                   Z-ADD     CPA(3)        SATCPC
236800970207      * COLLI: a terra da partire
236900970207     C                   Z-ADD     CBP(1)        SATCDE
237000970207     C                   Z-ADD     CBP(2)        SATCDD
237100970207     C                   Z-ADD     CBP(3)        SATCDC
237200970207      * COLLI: a terra non da partire
237300970207     C                   Z-ADD     CBR(1)        SATCNE
237400970207     C                   Z-ADD     CBR(2)        SATCND
237500970207     C                   Z-ADD     CBR(3)        SATCNC
237600970207      * PESO: da partire
237700970210     C                   Z-ADD     PPA(1)        SATPPE
237800970210     C                   Z-ADD     PPA(2)        SATPPD
237900970210     C                   Z-ADD     PPA(3)        SATPPC
238000970207      * PESO: a terra da partire fino a 35 quintali
238100970207     C                   Z-ADD     PFI(1)        SATFDE
238200970207     C                   Z-ADD     PFI(2)        SATFDD
238300970207     C                   Z-ADD     PFI(3)        SATFDC
238400970207      * PESO: a terra non da partire fino a 35 q.
238500970207     C                   Z-ADD     PFN(1)        SATFNE
238600970207     C                   Z-ADD     PFN(2)        SATFND
238700970207     C                   Z-ADD     PFN(3)        SATFNC
238800970207     C* PESO: a terra da partire oltre 35 quintali
238900970207     C                   Z-ADD     PBP(1)        SATODE
239000970207     C                   Z-ADD     PBP(2)        SATODD
239100970207     C                   Z-ADD     PBP(3)        SATODC
239200970207      * PESO: a terra non da partire oltre 35 q.
239300970207     C                   Z-ADD     PBR(1)        SATONE
239400970207     C                   Z-ADD     PBR(2)        SATOND
239500970207     C                   Z-ADD     PBR(3)        SATONC
239600000000      * VOLUME: da partire
239700970210     C                   Z-ADD     VPA(1)        SATVPE
239800970210     C                   Z-ADD     VPA(2)        SATVPD
239900970210     C                   Z-ADD     VPA(3)        SATVPC
240000970207      * VOLUME a terra da partire
240100970207     C                   Z-ADD     VBP(1)        SATVDE
240200970207     C                   Z-ADD     VBP(2)        SATVDD
240300970207     C                   Z-ADD     VBP(3)        SATVDC
240400000331      * VOLUME rimasto a magazzino
240500000331      * (totale dei colli a terra da partire, da non partire e colli
240600000331      * di bolle in sospensione)
240700970207     C                   Z-ADD     VBR(1)        SATVNE
240800970207     C                   Z-ADD     VBR(2)        SATVND
240900970207     C                   Z-ADD     VBR(3)        SATVNC
241000940429      * SPEDIZIONI: da partire
241100970210     C                   Z-ADD     SPA(1)        SATSPE
241200970210     C                   Z-ADD     SPA(2)        SATSPD
241300970210     C                   Z-ADD     SPA(3)        SATSPC
241400970207      * SPEDIZIONI a terra da partire
241500970207     C                   Z-ADD     SBP(1)        SATSDE
241600970207     C                   Z-ADD     SBP(2)        SATSDD
241700970207     C                   Z-ADD     SBP(3)        SATSDC
241800970207      * SPEDIZIONI a terra non da partire
241900970207     C                   Z-ADD     SBR(1)        SATSNE
242000970207     C                   Z-ADD     SBR(2)        SATSND
242100970207     C                   Z-ADD     SBR(3)        SATSNC
242200970305      * SPEDIZIONI a terra da partire fuori affidabilità
242300970305     C                   Z-ADD     FAF(1)        SATSFE
242400970305     C                   Z-ADD     FAF(2)        SATSFD
242500970305     C                   Z-ADD     FAF(3)        SATSFC
242600000000      * SPEDIZIONI: complete
242700000000     C                   Z-ADD     SCM(1)        SATSCE
242800970207     C                   Z-ADD     SCM(2)        SATSCD
242900970207     C                   Z-ADD     SCM(3)        SATSCC
243000000000      *
243100970318     C* ASTERISCO LE SEGUENTI IF PERCHE' IL RECORD LO DEVO SEMPRE
243200970318     C* SCRIVERE PER EVITARE CHE NELL'ELABORAZIONE DEL REPORT DI
243300970318     C* SEDE, SE PER UN P.O. IL DA PARTIRE è = 0, VENGA ERRONEAMENTE
243400970318     C* SEGNALATO "DATI NON PERVENUTI".
243500970318     C*          SATSDE    IFGT *ZERO
243600970318     C*          SATSDD    ORGT *ZERO
243700970318     C*          SATSDC    ORGT *ZERO
243800970318     C*          SATSNE    ORGT *ZERO
243900970318     C*          SATSND    ORGT *ZERO
244000970318     C*          SATSNC    ORGT *ZERO
244100970318     C*          SATSPE    ORGT *ZEROS
244200970318     C*          SATSPD    ORGT *ZEROS
244300970318     C*          SATSPC    ORGT *ZEROS
244400940323      *
244500940428     C                   MOVE      $FTR          SATFTR
244600970207     C                   MOVE      $DTR          SATDTR
244700940323      *
244800970207     C                   WRITE     FNSAT000
244900970318     C*
245000970318     C* LE IF SOPRA ASTERISCATE LE MANTENGO PERò PER IMPOSTARE $PRIM4
245100970318     C*
245200970318     C     SATSDE        IFGT      *ZERO
245300970318     C     SATSDD        ORGT      *ZERO
245400970318     C     SATSDC        ORGT      *ZERO
245500970318     C     SATSNE        ORGT      *ZERO
245600970318     C     SATSND        ORGT      *ZERO
245700970318     C     SATSNC        ORGT      *ZERO
245800970318     C     SATSPE        ORGT      *ZEROS
245900970318     C     SATSPD        ORGT      *ZEROS
246000970318     C     SATSPC        ORGT      *ZEROS
246100000405     C     SATVNE        ORGT      *ZEROS
246200000405     C     SATVND        ORGT      *ZEROS
246300000405     C     SATVNC        ORGT      *ZEROS
246400940429     C* PER LA STAMPA DEI TOTALI DI AREA
246500940429     C                   Z-ADD     1             $PRIM4
246600970318     C                   ENDIF
246700000000      *
246800000000     C                   ENDSR
246900000000      **************************************************************************
247000970211      * Scrittura del record c/servizio sul file di trasmissione FNSAT00F
247100000000      **************************************************************************
247200000000     C     WR1SAT        BEGSR
247300000000      *
247400970207     C                   CLEAR                   FNSAT000
247500000000     C                   Z-ADD     SIMFEL        SATFLE
247600000000     C                   Z-ADD     $FILPR        SATLNP
247700000000     C                   Z-ADD     *ZERO         SATTFA
247800950210     C                   Z-ADD     DS1ADA        SATDRE
247900000000     C                   MOVE      'C'           SATTRE
248000000000      *
248100000000      * COLLI: da partire
248200970210     C                   Z-ADD     C4A(1)        SATCPE
248300970210     C                   Z-ADD     C4A(2)        SATCPD
248400970210     C                   Z-ADD     C4A(3)        SATCPC
248500970207      * COLLI: a terra da partire
248600970207     C                   Z-ADD     C4P(1)        SATCDE
248700970207     C                   Z-ADD     C4P(2)        SATCDD
248800970207     C                   Z-ADD     C4P(3)        SATCDC
248900970207      * COLLI: a terra non da partire
249000970207     C                   Z-ADD     C4R(1)        SATCNE
249100970207     C                   Z-ADD     C4R(2)        SATCND
249200970207     C                   Z-ADD     C4R(3)        SATCNC
249300970207      * PESO: da partire
249400970210     C                   Z-ADD     P4A(1)        SATPPE
249500970210     C                   Z-ADD     P4A(2)        SATPPD
249600970210     C                   Z-ADD     P4A(3)        SATPPC
249700970207      * PESO: a terra da partire fino a 35 quintali
249800970207     C                   Z-ADD     P4I(1)        SATFDE
249900970207     C                   Z-ADD     P4I(2)        SATFDD
250000970207     C                   Z-ADD     P4I(3)        SATFDC
250100970207      * PESO: a terra non da partire fino a 35 quintali
250200970207     C                   Z-ADD     P4N(1)        SATFNE
250300970207     C                   Z-ADD     P4N(2)        SATFND
250400970207     C                   Z-ADD     P4N(3)        SATFNC
250500970207      * PESO: a terra da partire oltre 35 quintali
250600970207     C                   Z-ADD     P4P(1)        SATODE
250700970207     C                   Z-ADD     P4P(2)        SATODD
250800970207     C                   Z-ADD     P4P(3)        SATODC
250900970207      * PESO: a terra non da partire oltre 35 quintali
251000970207     C                   Z-ADD     P4R(1)        SATONE
251100970207     C                   Z-ADD     P4R(2)        SATOND
251200970207     C                   Z-ADD     P4R(3)        SATONC
251300000000      * VOLUME: da partire
251400970210     C                   Z-ADD     V4A(1)        SATVPE
251500970210     C                   Z-ADD     V4A(2)        SATVPD
251600970210     C                   Z-ADD     V4A(3)        SATVPC
251700970207      * VOLUME: a terra da partire
251800970207     C                   Z-ADD     V4P(1)        SATVDE
251900970207     C                   Z-ADD     V4P(2)        SATVDD
252000970207     C                   Z-ADD     V4P(3)        SATVDC
252100000331      * VOLUME: totale rimasto a magazzino
252200000331      * (a terra da partire, da non partire, in sospensione)
252300970207     C                   Z-ADD     V4R(1)        SATVNE
252400970207     C                   Z-ADD     V4R(2)        SATVND
252500970207     C                   Z-ADD     V4R(3)        SATVNC
252600970207      * SPEDIZIONI: da partire
252700970210     C                   Z-ADD     S4A(1)        SATSPE
252800970210     C                   Z-ADD     S4A(2)        SATSPD
252900970210     C                   Z-ADD     S4A(3)        SATSPC
253000970207      * SPEDIZIONI: a terra da partire
253100970207     C                   Z-ADD     S4P(1)        SATSDE
253200970207     C                   Z-ADD     S4P(2)        SATSDD
253300970207     C                   Z-ADD     S4P(3)        SATSDC
253400970207      * SPEDIZIONI: a terra non da partire
253500970207     C                   Z-ADD     S4R(1)        SATSNE
253600970207     C                   Z-ADD     S4R(2)        SATSND
253700970207     C                   Z-ADD     S4R(3)        SATSNC
253800970305      * SPEDIZIONI: a terra da partire fuori affidabilità
253900970305     C                   Z-ADD     F4F(1)        SATSFE
254000970305     C                   Z-ADD     F4F(2)        SATSFD
254100970305     C                   Z-ADD     F4F(3)        SATSFC
254200000000      * SPEDIZIONI: complete
254300000000     C                   Z-ADD     S4M(1)        SATSCE
254400970207     C                   Z-ADD     S4M(2)        SATSCD
254500970207     C                   Z-ADD     S4M(3)        SATSCC
254600000000      *
254700970318     C     SATSDE        IFGT      *ZERO
254800970318     C     SATSDD        ORGT      *ZERO
254900970318     C     SATSDC        ORGT      *ZERO
255000970318     C     SATSNE        ORGT      *ZERO
255100970318     C     SATSND        ORGT      *ZERO
255200970318     C     SATSNC        ORGT      *ZERO
255300970318     C     SATSPE        ORGT      *ZERO
255400970318     C     SATSPD        ORGT      *ZERO
255500970318     C     SATSPC        ORGT      *ZERO
255600000405     C     SATVNE        ORGT      *ZEROS
255700000405     C     SATVND        ORGT      *ZEROS
255800000405     C     SATVNC        ORGT      *ZEROS
255900940323      *
256000940428     C                   MOVE      $FTR          SATFTR
256100970207     C                   MOVE      $DTR          SATDTR
256200940323      *
256300970207     C                   WRITE     FNSAT000
256400940429     C* PER LA STAMPA DEI C/S
256500940429     C                   Z-ADD     1             $PRIM2
256600940429     C* PER LA STAMPA DEI TOTALI DI AREA
256700940429     C                   Z-ADD     1             $PRIM4
256800000000     C                   ENDIF
256900000000      *
257000000000      * Aggiorno le schiere per i totali c/servizio per area
257100000000     C                   DO        3             $U                1 0
257200970210     C* A TERRA
257300000000     C                   ADD       C4P($U)       C1P($U)
257400000000     C                   ADD       C4R($U)       C1R($U)
257500000000     C                   ADD       P4I($U)       P1I($U)
257600970207     C                   ADD       P4N($U)       P1N($U)
257700000000     C                   ADD       P4P($U)       P1P($U)
257800000000     C                   ADD       P4R($U)       P1R($U)
257900000000     C                   ADD       V4P($U)       V1P($U)
258000000331     C                   ADD       V4R($U)       V1R($U)
258100000000     C                   ADD       S4P($U)       S1P($U)
258200000000     C                   ADD       S4R($U)       S1R($U)
258300000000     C                   ADD       S4M($U)       S1M($U)
258400970305     C                   ADD       F4F($U)       F1F($U)
258500970210     C* DA PARTIRE
258600970210     C                   ADD       C4A($U)       C1A($U)
258700970210     C                   ADD       P4A($U)       P1A($U)
258800970210     C                   ADD       V4A($U)       V1A($U)
258900970210     C                   ADD       S4A($U)       S1A($U)
259000000000     C                   ENDDO
259100000000      *
259200000000     C                   EXSR      AZZSE4
259300000000      *
259400000000     C                   ENDSR
259500000000      **************************************************************************
259600970211      * Scrittura del record transito sul file di trasmissione FNSAT00F
259700000000      **************************************************************************
259800000000     C     WR2SAT        BEGSR
259900000000      *
260000970211     C                   CLEAR                   FNSAT000
260100000000     C                   Z-ADD     SIMFEL        SATFLE
260200940502     C                   Z-ADD     SIMFEL        SATLNP
260300000000     C                   Z-ADD     *ZERO         SATTFA
260400950210     C                   Z-ADD     DS1ADA        SATDRE
260500000000     C                   MOVE      'T'           SATTRE
260600000000      *
260700000000      * COLLI: da partire
260800970211     C                   Z-ADD     0             SATCPE
260900970211     C                   Z-ADD     0             SATCPD
261000970211     C                   Z-ADD     0             SATCPC
261100970211      * COLLI:
261200970211     C                   Z-ADD     C2P(1)        SATCDE
261300970211     C                   Z-ADD     C2P(2)        SATCDD
261400970211     C                   Z-ADD     C2P(3)        SATCDC
261500970211      * PESO: da partire
261600970211     C                   Z-ADD     0             SATPPE
261700970211     C                   Z-ADD     0             SATPPD
261800970211     C                   Z-ADD     0             SATPPC
261900000000      * PESO: fino a 35 quintali
262000970211     C                   Z-ADD     P2I(1)        SATFDE
262100970211     C                   Z-ADD     P2I(2)        SATFDD
262200970211     C                   Z-ADD     P2I(3)        SATFDC
262300940428      * PESO: (non esiste il bollettato nel periodo o nel precedente)
262400970211     C                   Z-ADD     P2P(1)        SATODE
262500970211     C                   Z-ADD     P2P(2)        SATODD
262600970211     C                   Z-ADD     P2P(3)        SATODC
262700000000      * VOLUME: da partire
262800970211     C                   Z-ADD     0             SATVPE
262900970211     C                   Z-ADD     0             SATVPD
263000970211     C                   Z-ADD     0             SATVPC
263100970211      * VOLUME
263200970211     C                   Z-ADD     V2P(1)        SATVDE
263300970211     C                   Z-ADD     V2P(2)        SATVDD
263400970211     C                   Z-ADD     V2P(3)        SATVDC
263500000000      * SPEDIZIONI: da partire
263600970211     C                   Z-ADD     0             SATSPE
263700970211     C                   Z-ADD     0             SATSPD
263800970211     C                   Z-ADD     0             SATSPC
263900970211      * SPEDIZIONI
264000970211     C                   Z-ADD     S2P(1)        SATSDE
264100970211     C                   Z-ADD     S2P(2)        SATSDD
264200970211     C                   Z-ADD     S2P(3)        SATSDC
264300000000      * SPEDIZIONI: complete
264400940509     C                   Z-ADD     S2M(1)        SATSCE
264500970211     C                   Z-ADD     S2M(2)        SATSCD
264600970211     C                   Z-ADD     S2M(3)        SATSCC
264700000000      *
264800970212     C     SATSDE        IFGT      *ZERO
264900970212     C     SATSDD        ORGT      *ZERO
265000970212     C     SATSDC        ORGT      *ZERO
265100940428     C                   MOVE      $FTR          SATFTR
265200970207     C                   MOVE      $DTR          SATDTR
265300940323      *
265400970211     C                   WRITE     FNSAT000
265500940429     C* PER LA STAMPA DEI TOTALI DI AREA
265600940429     C                   Z-ADD     1             $PRIM4
265700940505     C                   Z-ADD     1             $PRIM3
265800000000     C                   ENDIF
265900000000      *
266000000000     C                   ENDSR
266100940428      **************************************************************************
266200940428      * Stampo i totali generali
266300940428      **************************************************************************
266400940428     C     TOTGEN        BEGSR
266500940428      *
266600940428      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
266700940428      *
266800940506      * stampo i totali generali finali solo se ho stampato i transiti
266900950628    1C     *IN02         IFEQ      *ON
267000940506     C     $PRIM3        ANDEQ     1
267100940506     C* I TOTALI DI AREA SEMPRE
267200940506     C     *IN02         OREQ      *OFF
267300940506     C*
267400940428     C                   SETON                                        40
267500940428     C                   SETOFF                                       41
267600970211     C  N02              SETON                                        45
267700970211     C   02              SETOFF                                       45
267800940428     C                   Z-ADD     *ZERO         FILIAP
267900940428     C                   MOVE      *BLANKS       ORGDEP
268000940428     C   02'TOT.GENE'    CAT       'RALE':0      ORGDEP
268100940428     C  N02              MOVEL     'TOTALE'      ORGDEP
268200940428     C*
268300950118     C                   WRITE     LSC2T3
268400940428      *
268500940428      * ?  C  O   L   L  I  ?
268600970211     C                   XFOOT     C3P           COTOTP
268700970211     C                   XFOOT     C3A           $CDA
268800940428     C                   EXSR      PERCOL
268900940428      *
269000940428      * ?  P  E  S  O  ?
269100970211     C                   XFOOT     P3I           PEF35P
269200970211     C                   XFOOT     P3P           PEO35P
269300970211     C     PEF35P        ADD       PEO35P        PETOTP
269400970228     C                   XFOOT     P3A           $PDA
269500970211     C                   EXSR      PERPES
269600970211      *
269700970211      * ?  V  O  L  U  M E  ?
269800970211     C                   XFOOT     V3P           VOTOTP
269900970211     C                   XFOOT     V3A           $VDA
270000940428     C                   EXSR      PERVOL
270100940428      *
270200940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
270300970211     C                   XFOOT     S3A           $SDA
270400970305     C                   XFOOT     F3F           SPFAFF
270500940502     C                   EXSR      PERSPE
270600970305     C                   XFOOT     S3P           SPTOTP
270700970305     C                   XFOOT     S3M           SPCONP
270800970211     C                   XFOOT     S3R           $COMO1
270900970211     C     SPTOTP        ADD       $COMO1        SPTOTA
271000970211     C     SPTOTA        SUB       SPCONP        SPSPAP
271100000407     C*
271200000407      * ?VOLUME A MAGAZZINO ?
271300000407     C                   XFOOT     V3R           VOTOTM
271400940428      *
271500940428     C                   EXSR      SCRIVI
271600940509     C                   ADD       1             $RIGHE
271700940428      *
271800970211      * ------------------ ESPR./DIST./CORR. --------------------------
271900940428      *
272000970211     C                   SETOFF                                       40
272100950628    2C                   DO        3             $V
272200940428     C                   MOVE      ENS($V)       TIPOP
272300940428      *
272400940428      * ?  C  O   L   L  I  ?
272500970211     C                   Z-ADD     C3P($V)       COTOTP
272600970211     C                   Z-ADD     C3A($V)       $CDA
272700940428     C                   EXSR      PERCOL
272800940428      *
272900940428      * ?  P  E  S  O  ?
273000940428     C                   Z-ADD     P3I($V)       PEF35P
273100970211     C                   Z-ADD     P3P($V)       PEO35P
273200970211     C     PEF35P        ADD       PEO35P        PETOTP
273300970211     C                   Z-ADD     P3A($V)       $PDA
273400940428     C                   EXSR      PERPES
273500940428      *
273600940428      * ?  V  O  L  U  M E  ?
273700970211     C                   Z-ADD     V3P($V)       VOTOTP
273800970211     C                   Z-ADD     V3A($V)       $VDA
273900940428     C                   EXSR      PERVOL
274000940428      *
274100940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
274200970211     C                   Z-ADD     S3A($V)       $SDA
274300970305     C                   Z-ADD     F3F($V)       SPFAFF
274400940502     C                   EXSR      PERSPE
274500970305     C                   Z-ADD     S3P($V)       SPTOTP
274600970305     C                   Z-ADD     S3M($V)       SPCONP
274700970211     C     S3P($V)       ADD       S3R($V)       SPTOTA
274800970211     C     SPTOTA        SUB       S3M($V)       SPSPAP
274900000406      * ?VOLUME A MAGAZZINO ?
275000000406     C                   Z-ADD     V3R($V)       VOTOTM
275100940428      *
275200940428     C                   EXSR      SCRIVI
275300940428      *
275400950628    2C                   ENDDO
275500940428      *
275600950118     C  N02              WRITE     LSC2T2
275700950628    1C                   ENDIF
275800940429     C*
275900950118     C   02              WRITE     LSC2T3
276000940428     C*
276100940428     C   02              EXSR      AZZTOT
276200940428      *
276300940428     C                   ENDSR
276400000000      **************************************************************************
276500000000      * Elaborazione transiti
276600940505      * Non posso leggere per data uscita dal transito perche' spesso
276700940505      * capita che ci siano bolle con data entrata un giorno e data
276800940505      * uscita il giorno prima: per questo elaboro tutte le bolle
276900950210      * che hanno la data di entrata al transito > 0 e <=a DS1AAL
277000000000      **************************************************************************
277100000000     C     ELATRA        BEGSR
277200000000      *
277300940505     C                   Z-ADD     0             $PRIM1
277400940428     C                   MOVE      *BLANK        $BLTPR
277500940429     C                   MOVE      *BLANK        $CBLPR
277600940428     C                   MOVE      'T'           $FLAG
277700000000      *
277800000000      * Leggo il file delle bolle in transito
277900940505     C                   Z-ADD     1             $DFV
278000020625     C     kbtt39        SETLL     FNBTT39L
278100020625     C     simfel        READe     FNBTT39L                               33
278200940428     C*
278300940428    2C     *IN33         DOWEQ     *OFF
278400950210     C     BTTDET        ANDLE     DS1AAL
278500000000      *
278600940505     C                   EXSR      ESCLU1
278700940428      *
278800940505    3C     $OK           IFEQ      'S'
278900940505     C                   Z-ADD     1             $PRIM1
279000000000      *
279100000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
279200000000      * poiche' lo dovro aggiornare
279300970211     C                   CLEAR                   FNSAT000
279400000000     C                   Z-ADD     SIMFEL        SATFLE
279500950210     C                   Z-ADD     DS1ADA        SATDRE
279600940502     C                   Z-ADD     SIMFEL        SATLNP
279700950118     C                   Z-ADD     BTPTFA        SATTFA
279800000000     C                   MOVE      'T'           SATTRE
279900970205     C     FNSA1K        CHAIN     FNSAT02L                           37
280000940428      *
280100021217      * se è la prima volta che leggo la bolla carico i dati: VOL e PKG
280200021217    4C     $BOLLA        IFEQ      '1'
280300021217     C                   EXSR      DATBOT
280400021217    4C                   ENDIF
280500021217      *
280600021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
280700030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
280800030213      * Peso medio
280900030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
281000030213if  1c***                if        Wsipes = 'S'
281100030213     c***                z-add     BTTpuc        $Peso
281200030213   x1c***                else
281300021220     c                   z-add     Wpesmed       $Peso
281400030213e   1c***                endif
281500021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
281600030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
281700030213      * Volume medio
281800030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
281900030213if  1c***                if        Wsivol = 'S'
282000030213     c***                z-add     BTTvuc        $Volum
282100030213   x1c***                else
282200021220     c                   z-add     Wvolmed       $Volum
282300030213e   1c***                endif
282400000000      *
282500970211      * Verifico il tipo spedizione ESPR./DIST./CORR.
282600950201     C                   Z-ADD     3             $T
282700950118    4C     BTPTSP        IFEQ      'E'
282800000000     C                   Z-ADD     1             $T
282900940428    4C                   ENDIF
283000950201    4C     BTPTSP        IFEQ      'D'
283100940323     C                   Z-ADD     2             $T
283200940428    4C                   ENDIF
283300000000      *
283400000000      * Ho due controlli da fare per sapere se il collo e' a terra:
283500940428      *    1)  BTTDUT = 0
283600940428      *    2)  BTTDUT > Data spedizione
283700000000     C                   SETOFF                                       36
283800940428     C     BTTDUT        IFEQ      0
283900950210     C     BTTDUT        ORGT      DS1AAL
284000000000     C                   SETON                                        36
284100000000     C                   ENDIF
284200940506      *
284300000000      * Sommo solo se il collo risulta a terra
284400940428    4C     *IN36         IFEQ      *ON
284500940506      *
284600940506      * ?  CALCOLI  SUI  COLLI  ?
284700940506      *
284800940509     C                   ADD       1             C2P($T)
284900940428     C                   ADD       1             C0P($T)
285000000000      *
285100000000      * ?  CALCOLI  SUL  PESO  ?
285200000000      *
285300000000      * Verifico il peso di 35 quintali
285400021217    5***C     BTPPKB        IFLE      3500
285500021217    5C     Wpeso         IFLE      3500
285600940509     C                   ADD       $PESO         P2I($T)
285700000000     C                   ADD       $PESO         P0I($T)
285800970211     C                   ELSE
285900970211      * Oltre 35 quintali:                                             (35ON)
286000970211     C                   ADD       $PESO         P2P($T)
286100970211     C                   ADD       $PESO         P0P($T)
286200940428    5C                   ENDIF
286300000000      *
286400000000      *
286500000000      * ?  CALCOLI  SUL  VOLUME  ?
286600000000      *
286700021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
286800021217     **5C     BTTVUC        IFEQ      *ZERO
286900021217     *** *
287000021217     ***C     BTPVLB        DIV(H)    BTPNCL        $VOLUM
287100021217     *** *
287200021217     **5C                   ELSE
287300021217     ***C                   Z-ADD     BTTVUC        $VOLUM
287400021217     **5C                   ENDIF
287500000000      *
287600940509     C                   ADD       $VOLUM        V2P($T)
287700940428     C                   ADD       $VOLUM        V0P($T)
287800000000      *
287900000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
288000000000      *
288100000000      * Visto che le spedizioni non riesco a calcolarle ora, mi limito a tenere
288200000000      * aggiornato il file di work con le bolle
288300950118     C                   Z-ADD     BTPLNP        WRKLNP
288400000000     C                   MOVE      *BLANK        WRKCSE
288500970409     C     FNWR2K        CHAIN     FNWRK00F                           99
288600940428    5C     *IN99         IFEQ      *OFF
288700000000     C                   ADD       1             WRKNC1
288800000000     C                   EXCEPT    AGGIO
288900940428   X5C                   ELSE
289000950118     C                   MOVEL     'T'           WTRANS
289100940503     C                   EXSR      WRIWRK
289200940428    5C                   ENDIF
289300940428      *
289400040921     C                   MOVE      $FTR          SATFTR
289500040921     C                   MOVE      $DTR          SATDTR
289600940428    5C     *IN37         IFEQ      *ON
289700040921     C***                MOVE      $FTR          SATFTR
289800040921     C***                MOVE      $DTR          SATDTR
289900970211     C                   WRITE     FNSAT000
290000940428   X5C                   ELSE
290100970211     C                   UPDATE    FNSAT000
290200940428    5C                   ENDIF
290300000000      *
290400940428    4C                   ENDIF
290500940428    3C                   ENDIF
290600000000      *
290700020625     C     simfel        READE     FNBTT39L                               33
290800940428    2C                   ENDDO
290900940506     C*
291000000000     C     $PRIM1        IFNE      *ZERO
291100940506     C                   EXSR      CALSPE
291200000000     C                   EXSR      WR2SAT
291300000000     C                   ENDIF
291400000000      *
291500000000     C                   ENDSR
291600940503      **************************************************************************
291700000411      * elimino i terminal che non hanno VOLUMI TOT A MAG>0
291800940503      **************************************************************************
291900940503     C     ELATER        BEGSR
292000950210     C                   Z-ADD     DS1FEL        $AREA
292100940503     C                   CLEAR                   $TFA
292200940503     C                   SETOFF                                       33
292300940503     C*
292400940503     C* ELABORO TERMINAL PER TERMINAL
292500940503     C                   EXSR      IMPTFA
292600940503     C*
292700940503    2C     *IN33         DOWEQ     *OFF
292800940503     C                   Z-ADD     *ZERO         $PRIM1
292900940503      *
293000970211     C     TASATK        READE(N)  FNSAT03L                               33
293100940503     C                   MOVEL     SATTRE        $TRE
293200940503     C*
293300940503    3C     *IN33         DOWEQ     *OFF
293400940503     C*
293500940503    4C     $TRE          IFNE      SATTRE
293600940503     C     $PRIM1        ANDEQ     0
293700970211     C     TRSATK        SETLL     FNSAT03L
293800970211     C     TRSATK        READE     FNSAT03L                               33
293900940503     C*
294000940503    5C     *IN33         DOWEQ     *OFF
294100970211     C                   DELETE    FNSAT003
294200970211     C     TRSATK        READE     FNSAT03L                               33
294300940503    5C                   ENDDO
294400940503     C                   MOVEL     SATTRE        $TRE
294500940503     C                   CLEAR                   $PRIM1
294600940503    4C                   ENDIF
294700940503     C*
294800000411     C* VEDO SE C'E' ALMENO UN VOLUME A MAGAZZINO
294900000411    4C     SATVNE        IFGT      0
295000000411     C     SATVND        ORGT      0
295100000411     C     SATVNC        ORGT      0
295200940503     C                   Z-ADD     1             $PRIM1
295300940503    4C                   ENDIF
295400970211     C     TASATK        READE(N)  FNSAT03L                               33
295500940503    3C                   ENDDO
295600940503     C* ELABORO L'ULTIMO
295700940503    3C     $PRIM1        IFEQ      0
295800970211     C     TRSATK        SETLL     FNSAT03L
295900970211     C     TRSATK        READE     FNSAT03L                               33
296000970211     C*
296100940503    4C     *IN33         DOWEQ     *OFF
296200970211     C                   DELETE    FNSAT003
296300970211     C     TRSATK        READE     FNSAT03L                               33
296400940503    4C                   ENDDO
296500940503    3C                   ENDIF
296600940503     C*
296700940503     C                   EXSR      IMPTFA
296800940503    2C                   ENDDO
296900940504     C*
297000940504     C                   ENDSR
297100000000      **************************************************************************
297200000000      * Stampo i terminal di arrivo
297300000000      **************************************************************************
297400000000     C     STATER        BEGSR
297500940429     C                   SETOFF                                       4333
297600940429     C                   SETON                                        30
297700950210     C                   Z-ADD     DS1FEL        $AREA
297800940429     C                   CLEAR                   $TFA
297900940509     C                   CLEAR                   $RIGHE
298000940429     C* ELABORO AREA PER AREA
298100940429     C  N39              EXSR      IMPARE
298200940429     C*
298300940429    2C     *IN33         DOWEQ     *OFF
298400940429     C*
298500940429     C* ELABORO TERMINAL PER TERMINAL
298600940429     C                   EXSR      IMPTFA
298700000000      *
298800940429    3C     *IN33         DOWEQ     *OFF
298900940429     C                   Z-ADD     *ZERO         $PRIM1
299000940429     C                   Z-ADD     *ZERO         $PRIM3
299100000000      *
299200970211     C     ARSATK        READE(N)  FNSAT03L                               33
299300940429     C*
299400940429    4C     *IN33         DOWEQ     *OFF
299500000000      *
299600940429     C* ELABORO SE DATA COMPRESA
299700940429     C* E, SE IMMESSA SOLTANTO UNA LINEA, SE E' LA LINEA RICHIESTA
299800970211    5C     SATDRE        IFLE      DS1AAL
299900940429     C*
300000940429     C* SE DA STAMPE GIORNALIERE ELABORO SLTANTO LE LINEE IN LIN
300100940429     C     SATLNP        LOOKUP    LIN                                    99
300200940429     C   99              EXSR      SERTRA
300300940429     C*
300400940429   X5C                   ELSE
300500940429     C                   SETON                                        33
300600940429    5C                   ENDIF
300700940429     C*
300800970211     C     ARSATK        READE(N)  FNSAT03L                               33
300900940429    4C                   ENDDO
301000940429      *
301100940429     C* Stampo le bolle dell'area
301200940429    4C     $PRIM1        IFEQ      1
301300940429     C                   EXSR      TOTFIL
301400940429     C                   EXSR      AZZFIL
301500940429    4C                   ENDIF
301600940429      *
301700940429      * Stampo le bolle in transito
301800940429    4C     $PRIM3        IFEQ      1
301900940429     C                   EXSR      TOTTRA
302000940429    4C                   ENDIF
302100940429     C*
302200940429     C* Elaboro nuovo terminal
302300940429     C                   EXSR      IMPTFA
302400940429    3C                   ENDDO
302500940429      *
302600940509     C                   Z-ADD     1             $PRIM3
302700970226     C                   SETON                                        0204
302800940429     C                   EXSR      TOTGEN
302900970226     C                   SETOFF                                       0204
303000940429     C* ELABORO NUOVA AREA
303100940429     C  N39              EXSR      IMPARE
303200940429    2C                   ENDDO
303300940429     C*
303400940429     C                   ENDSR
303500000000      **************************************************************************
303600000000      * Una volta letto il record verifico se da includere o no
303700000000      *
303800940428      * Includo se:
303900940428      * e' entrato al transito
304000940428      *  e non e' ancora uscito o e' uscito con data = > a  data elabor
304100940505      *            BTTDUT = 0
304200000000      *   oppure:
304300950210      *       AND  BTTDUT >= DS1ADAL
304400940428      *
304500940428      * escludo sempre i recuperi
304600000000      **************************************************************************
304700000000     C     ESCLU1        BEGSR
304800000000      *
304900000000     C                   MOVE      'N'           $OK
305000000000      *
305100000000     C     BTTDUT        IFEQ      *ZERO
305200950210     C     BTTDUT        ORGE      DS1AAL
305300000000     C                   MOVE      'S'           $OK
305400940428     C                   ENDIF
305500000000      *
305600000000      * Solo se superati i test ne eseguo altri
305700000000     C     $OK           IFEQ      'S'
305800940505      *
305900940505      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
306000940505      * come test ma mi permette di saltare delle chain)
306100950118    3C     $BTTAT        IFNE      $BLTPR
306200020625     C     FNBTPK        CHAIN     FNBTP11L                           99
306300950118     C  N99              MOVE      $BTTAT        $BLTPR
306400940505     C  N99              MOVEL     '1'           $BOLLA
306500940505     C   99              MOVEL     'N'           $OK
306600940505    3C                   ENDIF
306700000000      *
306800000000      * Eseguo la chain su TABEL solo se diverso dal precedente
306900950118     C  N99BTPCBO        IFNE      $CBLPR
307000950210     C                   MOVE      '3A'          KCOD
307100950210     C                   MOVE      *BLANKS       KKEY
307200950210     C                   MOVEL     BTPCBO        KKEY
307300950210     C     KTAB1         CHAIN     TABEL00F                           99
307400000000     C  N99              MOVEL     TBLUNI        DS3A
307500950118     C  N99              MOVE      BTPCBO        $CBLPR
307600000000     C   99              CLEAR                   DS3A
307700000000     C                   ENDIF
307800000000      *
307900000000      * Non considero i recuperi
308000940428     C     §3ARBL        IFEQ      'R'
308100000000     C                   MOVE      'N'           $OK
308200000000     C                   ENDIF
308300000000      *
308400000000     C                   ENDIF
308500000000      *
308600000000     C                   ENDSR
308700000000      **************************************************************************
308800970211      * RISTAMPA: lettura dal file statistiche (FNSAT00F) e output
308900000000      **************************************************************************
309000000000     C     RISTAM        BEGSR
309100000000      *
309200940502     C                   SETOFF                                       33
309300940428     C                   SETON                                        43
309400940429     C                   CLEAR                   $TFA
309500940509     C                   CLEAR                   $RIGHE
309600950210     C                   MOVEL     DS1FEL        $AREA
309700940429     C* LANCIO DA FILTRO
309800950210    1C     DS1FGS        IFGT      0
309900950210     C                   MOVEL     DS1LNP        SATLNP
310000940429     C*
310100940429     C* SE NON IMMESSA L'AREA LA CERCO
310200940429     C  N39              EXSR      IMPARE
310300940429     C*
310400940429    2C     *IN33         DOWEQ     *OFF
310500940429     C                   Z-ADD     *ZERO         $PRIM2
310600940429     C                   Z-ADD     *ZERO         $PRIM3
310700940429     C                   Z-ADD     *ZERO         $PRIM4
310800940429     C*
310900940429     C* SE NON IMMESSA LA LNP LA CERCO
311000940505     C                   EXSR      IMPLNP
311100000000      *
311200940429    3C     *IN33         DOWEQ     *OFF
311300940429     C                   Z-ADD     *ZERO         $PRIM1
311400940429     C* LEGGO IL FILE PER LNP
311500940429     C                   EXSR      LEGSAT
311600940429     C* cerco la nuova lnp
311700940429     C  N29              EXSR      IMPLNP
311800940429    3C                   ENDDO
311900940429     c*
312000940429     C* stampa finale dei C/S transiti e totale
312100940429     C                   EXSR      STAFIN
312200940429     C* cerco la nuova area
312300940429     C  N39              EXSR      IMPARE
312400940429    2C                   ENDDO
312500000000      *
312600950210   X1C                   ELSE
312700940429      *
312800940429      * Da stampe giornaliere
312900940429     C                   Z-ADD     1             $X
313000940429     C*
313100940506     C                   Z-ADD     *ZERO         $PRIM2            1 0
313200940506     C                   Z-ADD     *ZERO         $PRIM3            1 0
313300940506     C                   Z-ADD     *ZERO         $PRIM4            1 0
313400000912     C                   CLEAR                   WSTAF
313500940429    2C     LIN($X)       DOWGT     0
313600940429     C                   Z-ADD     LIN($X)       $LNP
313700940506     C                   Z-ADD     LIN($X)       $FILPR
313800940505     C                   Z-ADD     *ZERO         $PRIM1            1 0
313900000912     C* ESCLCUDO I P.O. POSTE
314000000912      * Escludo bolle LNP Poste
314100020813     ***C                   CLEAR                   §OGPT
314200020813     C                   clear                   OG143
314300000912     C     $FILPR        CHAIN     AZORG01L                           82
314400000912     C  N82              MOVEL     ORGDE3        OG143
314500000912     C**
314600020813   2A***C     §OGPT         IFNE      'S'
314700020813    2C                   IF        §OGntw <> 'PPT'
314800000912     C* SE ELABORATA ALMENO UNA LINEA, STAMPO ANCHE I TOTALI FINALI
314900000912     C                   MOVEL     'S'           WSTAF             1
315000000912     C*
315100970205     C     DTSATK        SETLL     FNSAT02L
315200940429     C* LEGGO IL FILE PER LINEA
315300940429     C                   EXSR      LEGSAT
315400000912     C                   ENDIF
315500940429     C                   ADD       1             $X
315600940429    2C                   ENDDO
315700940429     c*
315800000912     C     WSTAF         IFEQ      'S'
315900940429     C                   EXSR      STAFIN
316000940429    1C                   ENDIF
316100000912    1C                   ENDIF
316200940429     C*
316300940429     C                   ENDSR
316400940429      **************************************************************************
316500940429      * IMPOSTO L'AREA, LE CERCO MAN MANO                              i
316600940429      **************************************************************************
316700940429     C     IMPARE        BEGSR
316800970205     C     $AREA         SETGT     FNSAT02L
316900970205     C                   READ(N)   FNSAT02L                               33
317000940429     C*
317100940429     C     *IN33         IFEQ      *OFF
317200940429     C                   MOVEL     SATFLE        $AREA
317300940429     C                   ENDIF
317400940429     C                   ENDSR
317500940429      **************************************************************************
317600940429      * IMPOSTO LA LINEA DI PARTENZA, LA CERCO MAN MANO                i
317700940429      **************************************************************************
317800940429     C     IMPLNP        BEGSR
317900970205     C  N29LPSATK        SETGT     FNSAT02L
318000970205     C  N29ARSATK        READE(N)  FNSAT02L                               33
318100000619     C                   CLEAR                   WTROVA
318200940429     C*
318300000619    1C     WTROVA        DOWEQ     ' '
318400000619    2C     *IN33         IFEQ      *OFF
318500000619     C     SATLNP        LOOKUP    LIN                                    99
318600000619    3C     *IN99         IFEQ      *ON
318700940429     C                   MOVEL     SATLNP        $LNP
318800940429     C                   MOVEL     SATLNP        $FILPR
318900000619     C                   MOVEL     '1'           WTROVA            1
319000970205     C     DTSATK        SETLL     FNSAT02L
319100000619   X3C                   ELSE
319200000619     C  N29ARSATK        READE(N)  FNSAT02L                               33
319300000619    3C                   ENDIF
319400000619     C**
319500000619   X2C                   ELSE
319600000619     C                   MOVEL     '1'           WTROVA
319700000619    2C                   ENDIF
319800000619    1C                   ENDDO
319900000619     C*
320000940429     C                   ENDSR
320100940429      **************************************************************************
320200940429      * IMPOSTO IL TERMINAL, LO CERCO MAN MANO                         i
320300940429      **************************************************************************
320400940429     C     IMPTFA        BEGSR
320500970211     C     ARSATK        SETGT     FNSAT03L
320600970211     C     $AREA         READE(N)  FNSAT03L                               33
320700940429     C*
320800940429     C     *IN33         IFEQ      *OFF
320900940429     C                   MOVEL     SATTFA        $TFA
321000940429     C                   MOVEL     SATTFA        $FILPR
321100970211     C     TASATK        SETLL     FNSAT03L
321200940429     C                   ENDIF
321300940429     C                   ENDSR
321400940429      **************************************************************************
321500940429      * STAMPA FINALE DEI C/S E TRANSITI                               i
321600940429      **************************************************************************
321700940429     C     STAFIN        BEGSR
321800940429     C* stampo i totali C/S
321900940506     C     *IN38         IFEQ      *OFF
322000940429     C     $PRIM2        IFEQ      1
322100940429     C                   EXSR      TOTSER
322200940429     C                   ENDIF
322300940506     C* TOTALI DI AREA
322400970226     C                   SETON                                        04
322500940505     C                   EXSR      TOTGEN
322600970226     C                   SETOFF                                       04
322700940506     C                   ENDIF
322800940502     C*
322900940502      * Terminate le letture per filiale tocca ai transiti
323000950210     C     DS1RIS        IFNE      'R'
323100940502     C                   EXSR      ELATRA
323200940503     C*
323300940505     C* CANCELLO LE RIGHE DEI TERMINAL CHE NON HANNO COLLI A TERRA
323400940503     C                   EXSR      ELATER
323500940502     C                   ENDIF
323600940502     C*
323700940429     C* stampo i totali dei TRANSITI
323800940506     C     *IN38         IFEQ      *OFF
323900000308     C     *IN27         IFEQ      *OFF
324000000308     C     DS1RIS        OREQ      'R'
324100000308     C                   EXSR      TOTTRA
324200000308     C                   ENDIF
324300940429     c* stampo il totale di area
324400940429     C     $PRIM4        IFEQ      1
324500970226     C                   SETON                                        0204
324600940429     C                   EXSR      TOTGEN
324700970226     C                   SETOFF                                       0204
324800950118     C                   WRITE     LSC2E4
324900940429     C                   ENDIF
325000940506     C                   ENDIF
325100940429     C                   ENDSR
325200940429      **************************************************************************
325300950210      * IMPOSTO L'AREA, LE CERCO MAN MANO
325400940429      **************************************************************************
325500940429     C     LEGSAT        BEGSR
325600970205     C     LPSATK        READE(N)  FNSAT02L                               33
325700940429     C*
325800940429    4C     *IN33         DOWEQ     *OFF
325900940429      *
326000970211    5C     SATDRE        IFLE      DS1AAL
326100940429      * Aggiorno i totali
326200940429     C                   EXSR      SERTRA
326300940429   x5C                   ELSE
326400940429     C                   SETON                                        33
326500940429    5C                   ENDIF
326600940429     C*
326700970205     C  N33LPSATK        READE(N)  FNSAT02L                               33
326800940429    4C                   ENDDO
326900940429     C* totali di filiale
327000940429    4C     $PRIM1        IFEQ      1
327100940429     C                   EXSR      TOTFIL
327200940429     C                   EXSR      AZZFIL
327300940429    4C                   ENDIF
327400940429     C                   ENDSR
327500000000      **************************************************************************
327600000000      * RISTAMPA: per servizi o transiti non stampo ma aggiorno i totali
327700000000      **************************************************************************
327800000000     C     SERTRA        BEGSR
327900000000      *
328000000000     C     SATTRE        IFEQ      *BLANK
328100940429     C                   Z-ADD     1             $PRIM1
328200970211     C                   ADD       SATCPE        CPA(1)
328300970211     C                   ADD       SATCPD        CPA(2)
328400970211     C                   ADD       SATCPC        CPA(3)
328500970211     C                   ADD       SATCDE        CBP(1)
328600970211     C                   ADD       SATCDD        CBP(2)
328700970228     C                   ADD       SATCDC        CBP(3)
328800970211     C                   ADD       SATCNE        CBR(1)
328900970211     C                   ADD       SATCND        CBR(2)
329000970211     C                   ADD       SATCNC        CBR(3)
329100970211     C                   ADD       SATPPE        PPA(1)
329200970211     C                   ADD       SATPPD        PPA(2)
329300970211     C                   ADD       SATPPC        PPA(3)
329400970211     C                   ADD       SATFDE        PFI(1)
329500970211     C                   ADD       SATFDD        PFI(2)
329600970211     C                   ADD       SATFDC        PFI(3)
329700970211     C                   ADD       SATFNE        PFN(1)
329800970211     C                   ADD       SATFND        PFN(2)
329900970211     C                   ADD       SATFNC        PFN(3)
330000970211     C                   ADD       SATODE        PBP(1)
330100970211     C                   ADD       SATODD        PBP(2)
330200970211     C                   ADD       SATODC        PBP(3)
330300970211     C                   ADD       SATONE        PBR(1)
330400970211     C                   ADD       SATOND        PBR(2)
330500970211     C                   ADD       SATONC        PBR(3)
330600970211     C                   ADD       SATVPE        VPA(1)
330700970211     C                   ADD       SATVPD        VPA(2)
330800970211     C                   ADD       SATVPC        VPA(3)
330900970211     C                   ADD       SATVDE        VBP(1)
331000970211     C                   ADD       SATVDD        VBP(2)
331100970211     C                   ADD       SATVDC        VBP(3)
331200970211     C                   ADD       SATVNE        VBR(1)
331300970211     C                   ADD       SATVND        VBR(2)
331400970211     C                   ADD       SATVNC        VBR(3)
331500970211     C                   ADD       SATSPE        SPA(1)
331600970211     C                   ADD       SATSPD        SPA(2)
331700970211     C                   ADD       SATSPC        SPA(3)
331800970211     C                   ADD       SATSDE        SBP(1)
331900970211     C                   ADD       SATSDD        SBP(2)
332000970211     C                   ADD       SATSDC        SBP(3)
332100970211     C                   ADD       SATSNE        SBR(1)
332200970211     C                   ADD       SATSND        SBR(2)
332300970211     C                   ADD       SATSNC        SBR(3)
332400970305     C                   ADD       SATSFE        FAF(1)
332500970305     C                   ADD       SATSFD        FAF(2)
332600970305     C                   ADD       SATSFC        FAF(3)
332700000000     C                   ADD       SATSCE        SCM(1)
332800970211     C                   ADD       SATSCD        SCM(2)
332900970211     C                   ADD       SATSCC        SCM(3)
333000000000     C                   ENDIF
333100000000      *
333200000000     C     SATTRE        IFEQ      'C'
333300940429     C                   Z-ADD     1             $PRIM2
333400970211     C                   ADD       SATCPE        C1A(1)
333500970211     C                   ADD       SATCPD        C1A(2)
333600970211     C                   ADD       SATCPC        C1A(3)
333700970211     C                   ADD       SATCDE        C1P(1)
333800970211     C                   ADD       SATCDD        C1P(2)
333900970211     C                   ADD       SATCDC        C1P(3)
334000970211     C                   ADD       SATCNE        C1R(1)
334100970211     C                   ADD       SATCND        C1R(2)
334200970211     C                   ADD       SATCNC        C1R(3)
334300970211     C                   ADD       SATPPE        P1A(1)
334400970303     C                   ADD       SATPPD        P1A(2)
334500970303     C                   ADD       SATPPC        P1A(3)
334600970211     C                   ADD       SATFDE        P1I(3)
334700970211     C                   ADD       SATFDD        P1I(2)
334800970211     C                   ADD       SATFDC        P1I(3)
334900970211     C                   ADD       SATFNE        P1N(1)
335000970211     C                   ADD       SATFND        P1N(2)
335100970211     C                   ADD       SATFNC        P1N(3)
335200970211     C                   ADD       SATODE        P1P(1)
335300970211     C                   ADD       SATODD        P1P(2)
335400970211     C                   ADD       SATODC        P1P(3)
335500970211     C                   ADD       SATONE        P1R(1)
335600970211     C                   ADD       SATOND        P1R(2)
335700970211     C                   ADD       SATONC        P1R(3)
335800970211     C                   ADD       SATVPE        V1A(1)
335900970211     C                   ADD       SATVPD        V1A(2)
336000970211     C                   ADD       SATVPC        V1A(3)
336100970211     C                   ADD       SATVDE        V1P(1)
336200970211     C                   ADD       SATVDD        V1P(2)
336300970211     C                   ADD       SATVDC        V1P(3)
336400970211     C                   ADD       SATVNE        V1R(1)
336500970211     C                   ADD       SATVND        V1R(2)
336600970211     C                   ADD       SATVNC        V1R(3)
336700970211     C                   ADD       SATSPE        S1A(1)
336800970211     C                   ADD       SATSPD        S1A(2)
336900970211     C                   ADD       SATSPC        S1A(3)
337000970211     C                   ADD       SATSDE        S1P(1)
337100970211     C                   ADD       SATSDD        S1P(2)
337200970211     C                   ADD       SATSDC        S1P(3)
337300970211     C                   ADD       SATSNE        S1R(1)
337400970211     C                   ADD       SATSND        S1R(2)
337500970211     C                   ADD       SATSNC        S1R(3)
337600970305     C                   ADD       SATSFE        F1F(1)
337700970305     C                   ADD       SATSFD        F1F(2)
337800970305     C                   ADD       SATSFC        F1F(3)
337900000000     C                   ADD       SATSCE        S1M(1)
338000970211     C                   ADD       SATSCD        S1M(2)
338100970211     C                   ADD       SATSCC        S1M(3)
338200000000     C                   ENDIF
338300000000      *
338400940429     C     SATTRE        IFEQ      'T'
338500940429     C                   Z-ADD     1             $PRIM3
338600970211     C                   ADD       SATCDE        C2P(1)
338700970211     C                   ADD       SATCDD        C2P(2)
338800970211     C                   ADD       SATCDC        C2P(3)
338900970211     C                   ADD       SATFDE        P2I(1)
339000970211     C                   ADD       SATFDD        P2I(2)
339100970211     C                   ADD       SATFDC        P2I(3)
339200970211     C                   ADD       SATODE        P2P(1)
339300970211     C                   ADD       SATODD        P2P(2)
339400970211     C                   ADD       SATODC        P2P(3)
339500970211     C                   ADD       SATVDE        V2P(1)
339600970211     C                   ADD       SATVDD        V2P(2)
339700970211     C                   ADD       SATVDC        V2P(3)
339800970211     C                   ADD       SATSDE        S2P(1)
339900970211     C                   ADD       SATSDD        S2P(2)
340000970211     C                   ADD       SATSDC        S2P(3)
340100000000     C                   ADD       SATSCE        S2M(1)
340200970211     C                   ADD       SATSCD        S2M(2)
340300970211     C                   ADD       SATSCC        S2M(3)
340400940505     C*
340500940505     C                   ELSE
340600000000      *
340700940429      *  totali generali
340800940429     C                   Z-ADD     1             $PRIM4
340900970211     C                   ADD       SATCDE        C3P(1)
341000970211     C                   ADD       SATCDD        C3P(2)
341100970211     C                   ADD       SATCDC        C3P(3)
341200970211     C                   ADD       SATFDE        P3I(1)
341300970211     C                   ADD       SATFDD        P3I(2)
341400970211     C                   ADD       SATFDC        P3I(3)
341500970212     C                   ADD       SATODE        P3P(1)
341600970212     C                   ADD       SATODD        P3P(2)
341700970212     C                   ADD       SATODC        P3P(3)
341800970211     C                   ADD       SATVDE        V3P(1)
341900970211     C                   ADD       SATVDD        V3P(2)
342000970211     C                   ADD       SATVDC        V3P(3)
342100000406     C                   ADD       SATVNE        V3R(1)
342200000406     C                   ADD       SATVND        V3R(2)
342300000406     C                   ADD       SATVNC        V3R(3)
342400970211     C                   ADD       SATSDE        S3P(1)
342500970211     C                   ADD       SATSDD        S3P(2)
342600970211     C                   ADD       SATSDC        S3P(3)
342700970211     C                   ADD       SATSNE        S3R(1)
342800970211     C                   ADD       SATSND        S3R(2)
342900970211     C                   ADD       SATSNC        S3R(3)
343000970305     C                   ADD       SATSFE        F3F(1)
343100970305     C                   ADD       SATSFD        F3F(2)
343200970305     C                   ADD       SATSFC        F3F(3)
343300000000     C                   ADD       SATSCE        S3M(1)
343400970211     C                   ADD       SATSCD        S3M(2)
343500970211     C                   ADD       SATSCC        S3M(3)
343600940505     C                   ENDIF
343700000000      *
343800000000     C                   ENDSR
343900940505      **************************************************************************
344000940505      * RISTAMPA: addiziono i transiti al totale generale              i
344100940505      **************************************************************************
344200940505     C     ADDTRA        BEGSR
344300940505     C                   Z-ADD     1             $PRIM4
344400940505     C                   ADD       C2P           C3P
344500940505     C                   ADD       P2I           P3I
344600940505     C                   ADD       P2P           P3P
344700940505     C                   ADD       V2P           V3P
344800940505     C                   ADD       S2P           S3P
344900940505     C                   ADD       S2M           S3M
345000940505     C                   ENDSR
345100000000      **************************************************************************
345200000000      * RISTAMPA: azzera i totali generali
345300000000      **************************************************************************
345400000000     C     AZZTOT        BEGSR
345500000000      *
345600970212     C                   CLEAR                   C3A
345700970212     C                   CLEAR                   C3P
345800970211     C                   CLEAR                   P3I
345900970211     C                   CLEAR                   P3P
346000970211     C                   CLEAR                   P3A
346100970211     C                   CLEAR                   V3P
346200940502     C                   CLEAR                   V3A
346300000406     C                   CLEAR                   V3R
346400970211     C                   CLEAR                   S3A
346500970211     C                   CLEAR                   S3P
346600970211     C                   CLEAR                   S3R
346700970211     C                   CLEAR                   S3M
346800970305     C                   CLEAR                   F3F
346900000000      *
347000000000     C                   ENDSR
347100000000      **************************************************************************
347200000000      * Testa l'overflow e stampa su carta
347300000000      **************************************************************************
347400000000     C     SCRIVI        BEGSR
347500940509      * PER TOTALI GENERALI SE GIA' STAMPE PIU' DI 9 RIGHE COMUNQUE
347600940509      * VADO A PAGINA NUOVA
347700940509     C     *IN03         IFEQ      *OFF
347800940509     C     *IN02         ANDEQ     *ON
347900940509     C     $RIGHE        ANDGT     9
348000940509     C                   SETON                                        30
348100940509     C                   ENDIF
348200000000      *
348300940429     C     *IN38         IFEQ      *OFF
348400940509     C*
348500940509     C     *IN30         IFEQ      *ON
348600940509     C     *IN40         ANDEQ     *ON
348700950118     C                   WRITE     LSC2T1
348800950118     C                   WRITE     LSC2T3
348900940509     C                   CLEAR                   $RIGHE
349000940509     C                   SETOFF                                       30
349100940509     C                   ENDIF
349200950118     C                   WRITE     LSC2D1
349300000000      *
349400970226     C  N04$ALMEN        IFEQ      *ZERO
349500000000     C                   Z-ADD     1             $ALMEN
349600000000     C                   ENDIF
349700940328     C                   ENDIF
349800000000      *
349900000000     C                   ENDSR
350000000000      **************************************************************************
350100000000      * Verifica se vi sono dei fogli viaggio con data partenza compresa
350200000000      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
350300000000      **************************************************************************
350400000000     C     FOGVIA        BEGSR
350500020813     ***C                   OPEN      FNFGV04L
350600020813     C                   OPEN      FNFGV06L
350700000000      *
350800020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
350900020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
351000020813     C     SIMfel        setll     FNFGV06L                               99
351100000000     C                   DO        *HIVAL
351200020813     ***C                   READ      FNFGV04L                               99
351300020813     C     SIMfel        reade     FNFGV06L                               99
351400950210     C  N99FGVDTP        IFGE      DS1ADA
351500950210     C     FGVDTP        ANDLE     DS1AAL
351600940502     C                   SETON                                        99
351700950118     C                   WRITE     LSC2E3
351800000000     C                   ENDIF
351900940502     C  N99              ENDDO
352000000000      *
352100020813     ***C                   CLOSE     FNFGV04L
352200020813     C                   CLOSE     FNFGV06L
352300000000     C                   ENDSR
352400000000      **************************************************************************
352500000000      * Aggiornamento del numero colli esaminati, nel file di work
352600950118     OFNWRK000  E            AGGIO
352700000000     O                       WRKNC1
352800000000      **************************************************************************
352900000000      *                                4         5         6         7         8
353000950118** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
353100970807OVRPRTF FNLSC2P       FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.terra
353200950210')
353300000000**
353400950118ESPR.DISTRCORR.
