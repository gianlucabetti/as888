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
003800950118     FAZCLN01L  IF   E           K DISK    USROPN
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
016100020418
016200020418     d sav_t06         S              3  0 dim(30)
016300020418
016400020418     d wc              s              3  0
016500021016
016600021016     d kTbeCod         s                   Like(TbeCod)
016700021016     d kTbeKe1         s                   Like(TbeKe1)
016800020418
016900040921     D WLBDAT          DS                  INZ
017000040921     D  G02DAT                 1      8  0
017100040921     D  G02INV                 9     16  0
017200040921     D  G02ERR                17     17
017300040921     D  G02TGI                18     22  0
017400000000      *
017500000810     D OG143         E DS
017600000810     D DS3A          E DS
017700000000     D DS3L          E DS
017800040921     D*DS5C          E DS
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
020200950118      * DS per controllare il "precedente" di FNBLT
020300000000     D $BLTAT          DS
020400000000     D  BLTLNP                 1      3  0
020500950118     D  BLTAAS                 4      7  0
020600950118     D  BLTNRS                 8      9  0
020700950118     D  BLTNSP                10     16  0
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
032200020813     D $BLTPR          S                   LIKE($BLTAT) inz
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
034800940502      **************************************************************************
034900940502      *      RIEPILOGO INDICATORI
035000940502      **************************************************************************
035100940502      * 01    - ON: LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE = 0
035200940502      *       - OFF:LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE > D.ELAB
035300940502      * 02    - ON: TOTALE GENERALE
035400940502      *       - OFF:TOTALE DI AREA
035500970226      * 03    - STAMPA DEI TOTALI TRANSITI
035600970226      * 04    - NON MEMORIZZA $ALMEN NELLA ROURINE DI TOTGEN
035700940502      * 05    - STAMPE GIORNALIERE
035800970205      * 27    - OFF: DEVO STAMPARE LA RIGA DEI TRANASITI
035900940502      * 29    - ON: IMMESSA UNA LINEA DI PARTENZA
036000940502      * 30    - ECCEDENZA PAGINA
036100940502      * 33    - LETTURA FILES
036200940502      * 34    - BOLLA IN C/SERVIZIO
036300940502      * 35    - ON: BOLLA BOLLETTATA NEL PERIODO
036400940502      *       - OFF:BOLLA BOLLETTATA NEL PERIODO PRECEDENTE
036500940502      * 36    - COLLO A TERRA
036600970211      * 37    - LETTURA SUL FILES FNSAT
036700950210      * 38    - DS1RIS= ' ' --> NON DEVO STAMPARE
036800940502      * 39    - ON: IMMESSO CODICE AREA
036900940502      * 40    - ON: STAMPA LA LILIALE CON DECODIFICA
037000940502      *       - OFF:STAMPA LA DICITURA DEL SERVIZIO BOLLE
037100940502      * 41    - ON: STAMPA SOLTANTO LA FILIALE (+ 40 ON)
037200940502      * 43    - ON: STAMPO LA PARTE RELATIVA ALLE LINEE DI PARTENZA
037300940502      *       - OFF:STAMPO LA PARTE RELATIVA AI TERMINAL DI ARRIVO
037400970211      * 45    - STAMPA LE PERCENTUALI SUL DA PARTIRE
037500950118      * 99    - DI COMODO
037600940502      **************************************************************************
037700000000     C     *ENTRY        PLIST
037800000000     C                   PARM                    KPJBA
037900950210     C                   MOVEL     KPJBU         DSLS01
038000950210      *
038100950210      * DATA FINALE AA/MM/GG (6 CARATTERI)
038200950210     C                   Z-ADD     DS1ADA        DATADA
038300000000      *
038400950118     C     FNBLTK        KLIST
038500940428     C                   KFLD                    LIN($X)
038600940428     C                   KFLD                    $DFV
038700970205     C     FNBLPK        KLIST
038800950118     C                   KFLD                    BLTAAS
038900000000     C                   KFLD                    BLTLNP
039000000000     C                   KFLD                    BLTNRS
039100000000     C                   KFLD                    BLTNSP
039200950118     C     FNBTPK        KLIST
039300020625     c                   kfld                    bttflp
039400950118     C                   KFLD                    BTTAAS
039500950118     C                   KFLD                    BTTLNP
039600950118     C                   KFLD                    BTTNRS
039700950118     C                   KFLD                    BTTNSP
039800950118     C     AZCLNK        KLIST
039900940504     C                   KFLD                    SIMFEL
040000940504     C                   KFLD                    $TFA
040100940504     C                   KFLD                    ANNODA
040200940504     C                   KFLD                    MESEDA
040300950118     C     FNWRKK        KLIST
040400000000     C                   KFLD                    $FLAG
040500000000     C                   KFLD                    $FILPR
040600950118     C     FNWR1K        KLIST
040700000000     C                   KFLD                    $FLAG
040800000000     C                   KFLD                    WRKLNP
040900940503     C                   KFLD                    BLPAAS
041000940503     C                   KFLD                    BLPNRS
041100940503     C                   KFLD                    BLPNSP
041200000000     C                   KFLD                    WRKCSE
041300970409     C     FNWR2K        KLIST
041400970409     C                   KFLD                    $FLAG
041500970409     C                   KFLD                    WRKLNP
041600970409     C                   KFLD                    BTPAAS
041700970409     C                   KFLD                    BTPNRS
041800970409     C                   KFLD                    BTPNSP
041900970409     C                   KFLD                    WRKCSE
042000970205     C     FNSATK        KLIST
042100970205     C                   KFLD                    SIMFEL
042200970205     C                   KFLD                    LIN(XX)
042300970205     C                   KFLD                    DS1AAL
042400970205     C     FNSA1K        KLIST
042500000000     C                   KFLD                    SATFLE
042600000000     C                   KFLD                    SATTFA
042700940429     C                   KFLD                    SATLNP
042800000000     C                   KFLD                    SATDRE
042900000000     C                   KFLD                    SATTRE
043000970205     C     DTSATK        KLIST
043100940429     C                   KFLD                    $AREA
043200940429     C                   KFLD                    $TFA
043300940429     C                   KFLD                    $LNP
043400970211     C                   KFLD                    DS1ADA
043500970205     C     LPSATK        KLIST
043600940429     C                   KFLD                    $AREA
043700940429     C                   KFLD                    $TFA
043800940429     C                   KFLD                    $LNP
043900970205     C     ARSATK        KLIST
044000940429     C                   KFLD                    $AREA
044100940429     C                   KFLD                    $TFA
044200970211     C     TASATK        KLIST
044300940429     C                   KFLD                    $AREA
044400940429     C                   KFLD                    $TFA
044500970211     C                   KFLD                    DS1ADA
044600970211     C     TRSATK        KLIST
044700940503     C                   KFLD                    $AREA
044800940503     C                   KFLD                    $TFA
044900970211     C                   KFLD                    DS1ADA
045000940503     C                   KFLD                    $TRE
045100020625     c     kbtt39        klist
045200020625     c                   kfld                    simfel
045300020625     c                   kfld                    $dfv
045400021016
045500021016     c     Ktbe          Klist
045600021016     c                   Kfld                    kTbeCod
045700021016     c                   Kfld                    kTbeKe1
045800000000      *
045900020813     ***C     *LIKE         DEFINE    DS1RIS        SAVRIS
046000020813     ***C     *LIKE         DEFINE    CPA(1)        $CDA
046100020813     ***C     *LIKE         DEFINE    VPA(1)        $VDA
046200020813     ***C     *LIKE         DEFINE    PPA(1)        $PDA
046300020813     ***C     *LIKE         DEFINE    SPA(1)        $SDA
046400020813     ***C     *LIKE         DEFINE    BLPTFA        $TFA
046500020813     ***C     *LIKE         DEFINE    DS1FEL        $AREA
046600020813     ***C     *LIKE         DEFINE    DS1LNP        $LNP
046700020813     ***C     *LIKE         DEFINE    $BLTAT        $BLTPR
046800020813     ***C     *LIKE         DEFINE    BLPLNP        $FILPR
046900020813     ***C     *LIKE         DEFINE    BLPCBO        $CBLPR
047000020813     ***C     *LIKE         DEFINE    BLPNFV        $NFVPR
047100020813     ***C     *LIKE         DEFINE    BLPDRT        $DTRIT
047200020813     ***C     *LIKE         DEFINE    BLPPDR        $CDPDR
047300020813     ***C     *LIKE         DEFINE    WRKFLG        $FLAG
047400020813     ***C     *LIKE         DEFINE    WRKFLG        $FLAG2
047500020813     ***C     *LIKE         DEFINE    FGVDFV        $DFV
047600020813     ***C     *LIKE         DEFINE    SATFTR        $FTR
047700020813     ***C     *LIKE         DEFINE    SATDTR        $DTR
047800020813     ***C     *LIKE         DEFINE    SATTRE        $TRE
047900020813     ***C                   Z-ADD     *ZERO         $COMOD           15 6
048000021016     C                   Z-ADD     *ZERO         $COMO1           15 6
048100021016     C                   Z-ADD     1             CODUT
048200000000      **************************************************************************
048300000000      *              M A I N      L I N E
048400000000      **************************************************************************
048500940429     C                   MOVE      'RISTAMPA'    STRIP
048600940429     C*
048700950210    1C     DS1RIS        IFNE      'R'
048800940429     C                   MOVE      'STAMPA  '    STRIP
048900940502    1C                   ENDIF
049000070522     c* Carico la tabella VPO per vedere da quando cambia il calcolo
049100070522     c*  della affidabilità partenza
049200070522     C                   MOVEL     'VPO'         kTBECOD
049300070522     C                   MOVEL(P)  'DECO'        kTBEKE1
049400070522     C     KTBE          CHAIN(N)  TNTBE01L                           21
049500070522     C  N21              MOVEL     TBEUNI        DVPODECO
049600070522     C*
049700070522     c
049800000000      *
049900000000      * Se la data AL e' vuota la imposto come la data DAL
050000950210     C     DS1AAL        IFEQ      *ZERO
050100950210     C                   Z-ADD     DS1ADA        DS1AAL
050200950210     C                   Z-ADD     DS1GDA        DS1GAL
050300000000     C                   ENDIF
050400940428     C*
050500940428     C                   SETON                                        43
050600940401      *
050700950210      * Accendo un indicatore se non devo stampare il tabulato (DS1RIS=' ')
050800950210     C     DS1RIS        COMP      *BLANKS                                38
050900940429      *
051000940429      * 39 ON - Immessa l'area
051100950210    3C     DS1FEL        IFGT      0
051200940429     C                   SETON                                        39
051300940429    3C                   ENDIF
051400000000      *
051500950210      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
051600950210     C                   EXSR      COMUNE
051700950210      *
051800000000     C                   SETON                                        LR
051900020212     C*****************************************************************
052000020212     C*  PARTE COMUNE STAMPE GIORNALIERE
052100020212     C*
052200020212     C*  . REPERIMENTO 'SIMFEL'
052300020212     C*  . CARICAMENTO '£1'
052400020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
052500020212     C*  . CODA DI STAMPA ('£OUTQ')
052600020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
052700020212     C*  . MODULO PER LA STAMPA ('£MOD')
052800020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
052900020212     C*****************************************************************
053000020212     C     COMUNE        BEGSR
053100020212     C*
053200020212     C*****************************************************************
053300020212     C*       RIEPILOGO INDICATORI:
053400020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
053500020212     C* 11    -           STAMPA PER 1L
053600020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
053700020212     C*****************************************************************
053800020212     C                   Z-ADD     1             CODUT
053900020212     C*
054000020212     C* Reperimento parametri aziendali
054100020212     C                   CALL      'X§PARUT'
054200020212     C                   PARM                    UT§DSE
054300020212     C                   MOVEL     RAGUT         RSUT             20
054400020212     C                   MOVEL     REC80         CNCR80
054500020212     C*
054600020212     C* Chiave di accesso per il file tabelle
054700020212     C     KTAB1         KLIST
054800020212     C                   KFLD                    CODUT
054900020212     C                   KFLD                    KCOD
055000020212     C                   KFLD                    KKEY
055100020212     C     KTAB2         KLIST
055200020212     C                   KFLD                    CODUT
055300020212     C                   KFLD                    KCOD
055400020212     C*
055500020813     ***C     *LIKE         DEFINE    TBLCOD        KCOD
055600020813     ***C     *LIKE         DEFINE    TBLKEY        KKEY
055700020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
055800020212     C*
055900020212     C* DETERMINO DATA E ORA DI ESECUZIONE
056000020212     C                   TIME                    WTIME            14 0
056100020212     C                   MOVE      WTIME         WDATE             8 0
056200020212     C                   MOVEL     WTIME         UTIME             6 0
056300040921     c*
056400040921     C                   CLEAR                   WLBDAT
056500040921     C                   MOVE      Wtime         G02DAT
056600040921     C                   CALL      'XSRDA8'
056700040921     C                   PARM                    WLBDAT
056800040921     C                   Z-ADD     G02INV        dateu             8 0
056900040922     c*
057000040922     c                   clear                   $FTR
057100040922     c                   z-add     dateu         $DTR
057200020212     C*
057300020418     C* CARICO '£1'
057400020212     C                   CLEAR                   KPJBU
057500020212     C                   CLEAR                   DSUL06
057600020212     C                   MOVE      '£1'          D06COD
057700020212     C                   MOVEL     SIMFEL        D06KEY
057800050802     C                   MOVE      'S'           D06KEY
057900050802     C                   MOVEL     ds1ada        D06drf
058000020212     C                   MOVEL     DSUL06        KPJBU
058100020212     C                   CALL      'TRUL06R'
058200020212     C                   PARM                    KPJBA
058300020212     C                   MOVEL     KPJBU         DSUL06
058400020418     c                   movea     t06           sav_t06
058500020418
058600020418      * se in arrivo carico la £6
058700020418 b1  c                   if        ds1tbo = 'A'
058800020418     c                   clear                   wc
058900020418 b2  c                   do        30            wa
059000020422     c                   if        sav_t06(wa) = *zeros
059100020422     c                   leave
059200020422     c                   endif
059300020418     c                   clear                   kpjbu
059400020418     c                   clear                   dsul06
059500020418     c                   eval      d06cod = '£6'
059600020418     c                   movel     sav_t06(wa)   d06key
059700050802     C                   MOVE      'S'           D06KEY
059800020418     c                   eval      d06esc = 'S'
059900050802     c                   movel     ds1ada        d06drf
060000020419     c                   movel     dsul06        kpjbu
060100020418     c                   call      'TRUL06R'
060200020418     c                   parm                    kpjba
060300020418     c                   movel     kpjbu         dsul06
060400020418 b3  c                   do        30            wb
060500020418     c                   if        t06(wb) = *zeros
060600020418     c                   leave
060700020418     c                   endif
060800020418     c                   add       1             wc
060900020418     c                   z-add     t06(wb)       l6(wc)
061000020418 e3  c                   enddo
061100020418 e2  c                   enddo
061200020212     C*
061300020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
061400020212     C                   Z-ADD     *ZERO         WB
061500020418 b2  C                   DO        29            WA
061600020418     C     sav_T06(WA)   LOOKUP    L6                                     30
061700020212     C  N30              ADD       1             WB
061800020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
061900020418 e2  C                   ENDDO
062000020418 x1  C                   ELSE
062100020418     C                   MOVEA     sav_T06       L1
062200020418 e1  C                   ENDIF
062300020212     C*
062400020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
062500020212     C                   MOVEA     L1            LIN
062600020212     C                   CLEAR                   L1
062700020212     C                   Z-ADD     *ZERO         WB                3 0
062800020212     C     1             DO        30            WPT               3 0
062900020212     C     LIN(WPT)      IFNE      0
063000020212     C                   CLEAR                   FNLSC4
063100020212     C                   Z-ADD     LIN(WPT)      IC4LIN
063200020212     C                   MOVE      'B'           IC4FLG
063300020212     C                   MOVEL     FNLSC4        KPJBU
063400020212     C                   CALL      'FNLSC4R1'
063500020212     C                   PARM                    KPJBA
063600050802     C                   PARM                    ds1ada
063700020212     C                   MOVEL     KPJBU         FNLSC4
063800020212     C     OC4ESE        IFEQ      'S'
063900020212     C                   ADD       1             WB
064000020212     C                   MOVE      LIN(WPT)      L1(WB)
064100020212     C                   ENDIF
064200020212     C                   ENDIF
064300020212     C                   ENDDO
064400070103     C*
064500070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
064600070103     C                   Z-ADD     *ZERO         WB                3 0
064700070103     C                   DO        29            WA                3 0
064800070103     C     L1(WA)        IFNE      *ZEROS
064900070103     C     L1(WA)        ANDNE     SIMFEL
065000070103     C                   ADD       1             WB
065100070103     C                   Z-ADD     L1(WA)        LV2(WB)
065200070103     C                   ENDIF
065300070103     C                   ENDDO
065400020212     C*
065500020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
065600020212     C*
065700020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
065800020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
065900020212     C* la stampa e' lanciata da menu
066000020212     C                   MOVEL     '3L'          KCOD
066100020212     C                   MOVEL     DS1PGM        KKEY
066200020212     C                   MOVE      DS1FPG        KKEY
066300020212     C     KTAB1         CHAIN     TABEL00F                           30
066400020212     C  N30              MOVEL     TBLUNI        DS3L
066500020212     C   30              MOVEL     'F'           §3LD1L
066600020212     C   30              MOVEL     ' '           §3LD2L
066700020212     C   30              MOVEL     '*STD    '    §3LDST
066800020212     C*
066900020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
067000020212     C                   MOVEL     §3LDST        £MOD             10
067100020212     C                   CLEAR                   LIN
067200020212     C*
067300020212     C* Verifico chi ha lanciato la stampa
067400020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
067500020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
067600020212     C* DS1FGS<>0  --->  si proviene da menu
067700020212     C     DS1FGS        IFEQ      0
067800020212     C*
067900020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
068000020212     C* lancio, voglio la stampa;  invece
068100020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
068200020212     C*
068300020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
068400020212     C* creare stampa per 1°lvl. (se stessa)
068500020212     C     DS1LNP        IFEQ      SIMFEL
068600020212     C*
068700020212     C* Il flag §3LD1L ha i seguenti significati:
068800020212     C*    ' ' = tutti i dati
068900020212     C*    '1' = solo filiale di 1' livello
069000020212     C     §3LD1L        IFEQ      '1'
069100020212     C                   SETON                                        11
069200020212     C                   END
069300020212     C*
069400020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
069500020212     C* nella stampa
069600020212     C*
069700020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
069800020212     C  N11              MOVEA     L1            LIN
069900020212     C   11              MOVEA     SIMFEL        LIN(1)
070000020212     C*
070100020212     C* IMPOSTO LA STAMPANTE:
070200020212     C* STAMPANTE TABULATI
070300020212     C                   MOVEL     '5P'          KCOD
070400020212     C                   MOVEL     'T'           W004A             4
070500020212     C                   MOVE      LIN(1)        W004A
070600020212     C                   MOVEL(P)  W004A         KKEY
070700020212     C     KTAB1         CHAIN     TABEL00F                           99
070800020212     C     *IN99         IFEQ      *OFF
070900020212     C                   MOVEL     TBLUNI        DS5P
071000020212     C                   MOVEL     §5POTQ        £OUTQ            10
071100020212     C                   END
071200020212     C*
071300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
071400020212     C                   EXSR      LEGGI
071500020212     C*
071600020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
071700020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
071800020212     C                   ELSE
071900020212     C*
072000020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
072100020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
072200020212     C*
072300020212     C* Il significato del fla §3LD2L e' il seguente:
072400020212     C*    ' ' = tutti i dati
072500020212     C*    'F' = per ogni 2' livello
072600020212     C     §3LD2L        IFEQ      'F'
072700020212     C                   SETON                                        14
072800020212     C                   ENDIF
072900020212     C*
073000020212     C                   Z-ADD     1             WB
073100020212     C                   MOVEA     *ZEROS        LIN
073200020212     C  N14              MOVEA     L1            LIN
073300020212     C*
073400020212     C* Lancio per tutti i 2° lvl
073500020212     C     DS1LNP        IFEQ      0
073600020212     C*
073700020212     C* LV2 contiene tutte le filiali di 2° lvl
073800020212     C     LV2(WB)       DOWNE     0
073900020212      *
074000020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
074100020212     C                   CLEAR                   FNLSC4
074200020212     C                   Z-ADD     LV2(WB)       IC4LIN
074300020212     C                   MOVEL     FNLSC4        KPJBU
074400020212     C                   CALL      'FNLSC4R1'
074500020212     C                   PARM                    KPJBA
074600050802     C                   PARM                    ds1ada
074700020212     C                   MOVEL     KPJBU         FNLSC4
074800020212      *
074900020212     C     OC4PT1        IFEQ      'S'
075000020212     C     §3LPT         ANDEQ     'N'
075100020212     C                   GOTO      NOLEGG
075200020212     C                   ENDIF
075300020212      *
075400020212     C     OC4EXP        IFEQ      'S'
075500020212     C     §3LEXP        ANDEQ     'N'
075600020212     C                   GOTO      NOLEGG
075700020212     C                   ENDIF
075800020212      *
075900020212     C     OC4DPD        IFEQ      'S'
076000020212     C     §3LDPD        ANDEQ     'N'
076100020212     C                   GOTO      NOLEGG
076200020212     C                   ENDIF
076300020212      *
076400020212     C     OC4BAR        IFEQ      'S'
076500020212     C     §3LBAR        ANDEQ     'N'
076600020212     C                   GOTO      NOLEGG
076700020212     C                   ENDIF
076800020212      *
076900020212     C     OC4FED        IFEQ      'S'
077000020212     C     §3LFED        ANDEQ     'N'
077100020212     C                   GOTO      NOLEGG
077200020212     C                   ENDIF
077300020212      *
077400020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
077500020212     C*  dati degli altri 2° lvl
077600020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
077700020212     C   14              MOVEL     LV2(WB)       LIN(1)
077800020212     C*
077900020212     C* IMPOSTO LA STAMPANTE
078000020212     C* STAMPANTE TABULATI
078100020212     C                   MOVEL     '5P'          KCOD
078200020212     C                   MOVEL     'T'           W004A             4
078300020212     C                   MOVE      LV2(WB)       W004A
078400020212     C                   MOVEL(P)  W004A         KKEY
078500020212     C     KTAB1         CHAIN     TABEL00F                           99
078600020212     C     *IN99         IFEQ      *OFF
078700020212     C                   MOVEL     TBLUNI        DS5P
078800020212     C                   MOVEL     §5POTQ        £OUTQ            10
078900020212     C                   END
079000020212     C*
079100020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
079200020212     C                   EXSR      LEGGI
079300020212     C*
079400020212     C     NOLEGG        TAG
079500020212     C*
079600020212     C                   ADD       1             WB
079700020212     C                   ENDDO
079800020212     C*
079900020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
080000020212     C                   ELSE
080100020212     C*
080200020212     C* Lancio solo per la 2° lvl richiesta
080300020212     C   14              MOVEL     DS1LNP        LIN(1)
080400020212     C*
080500020212     C* STAMPANTE
080600020212     C* STAMPANTE TABULATI
080700020212     C                   MOVEL     '5P'          KCOD
080800020212     C                   MOVEL     'T'           W004A             4
080900020212     C                   MOVE      DS1LNP        W004A
081000020212     C                   MOVEL(P)  W004A         KKEY
081100020212     C     KTAB1         CHAIN     TABEL00F                           99
081200020212     C     *IN99         IFEQ      *OFF
081300020212     C                   MOVEL     TBLUNI        DS5P
081400020212     C                   MOVEL     §5POTQ        £OUTQ            10
081500020212     C                   END
081600020212     C*
081700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
081800020212     C                   EXSR      LEGGI
081900020212     C*
082000020212     C                   ENDIF
082100020212     C                   ENDIF
082200020212     C*
082300020212     C                   ELSE
082400020212     C*
082500020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
082600020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
082700020212     C     DS1LNP        IFEQ      0
082800020212     C                   MOVEA     L1            LIN
082900020212     C* ...altrimenti elaboro solo se stessa
083000020212     C                   ELSE
083100020212     C                   MOVEL     DS1LNP        LIN(1)
083200020212     C                   ENDIF
083300020212     C*
083400020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
083500020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
083600020212     C* PRENDO QUELLA PRESENTE IN KPJBA
083700020212     C     DS1FGS        IFEQ      SIMFEL
083800020212     C                   MOVEL     KQOTD         £OUTQ
083900030627     C**!!!              MOVEL     KQOTD         £OUTQF
084000020212     C                   ELSE
084100020212     C* STAMPANTE TABULATI
084200020212     C                   MOVEL     '5P'          KCOD
084300020212     C                   MOVEL     'T'           W004A             4
084400020212     C                   MOVE      DS1FGS        W004A
084500020212     C                   MOVEL(P)  W004A         KKEY
084600020212     C     KTAB1         CHAIN     TABEL00F                           99
084700020212     C     *IN99         IFEQ      *OFF
084800020212     C                   MOVEL     TBLUNI        DS5P
084900020212     C                   MOVEL     §5POTQ        £OUTQ            10
085000020212     C                   END
085100020212     C                   ENDIF
085200020212     C*
085300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
085400020212     C                   EXSR      LEGGI
085500020212     C                   ENDIF
085600020212     C*
085700020212     C                   ENDSR
085800000000      **************************************************************************
085900000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
086000000000      **************************************************************************
086100000000     C     LEGGI         BEGSR
086200940502     C                   SETOFF                                       2729
086300000619     C                   SETON                                        43
086400000616     C                   CLEAR                   SAVRIS
086500970212     C*
086600970212      * Cancello dal file statistiche i record della giornata che vado a elabor.
086700970212      * Solo se richiesta elaborazione
086800970212    1C     DS1RIS        IFNE      'R'
086900000616     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
087000000616     C                   CLEAR                   KPJBU
087100000616     C                   CLEAR                   DSUL06
087200000616     C                   MOVE      '£1'          D06COD
087300000616     C                   MOVEL     SIMFEL        D06KEY
087400050802     C                   MOVE      'S'           D06KEY
087500050802     c                   movel     ds1ada        d06drf
087600000616     C                   MOVEL     DSUL06        KPJBU
087700000616     C                   CALL      'TRUL06R'
087800000616     C                   PARM                    KPJBA
087900000616     C                   MOVEL     KPJBU         DSUL06
088000000616     C**
088100000616     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
088200000616     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
088300000616     C*  CHE USO ALLA FINE
088400000616     C     DS1RIS        IFNE      ' '
088500000616     C                   MOVEA     LIN           SVLIN
088600000616     C                   MOVEL     DS1RIS        SAVRIS
088700000616     C                   CLEAR                   DS1RIS
088800000616     C                   ENDIF
088900000616     C*
089000000616     C                   MOVEA     T06           LIN
089100000616     C*
089200970212     C                   OPEN      FNSAT04L
089300970212     C                   Z-ADD     1             XX                3 0
089400970212    2C     LIN(XX)       DOWGT     *ZERO
089500970212     C*
089600970212     C     FNSATK        SETLL     FNSAT04L
089700970212     C     FNSATK        READE     FNSAT04L                               99
089800970212     C*
089900970212    3C     *IN99         DOWEQ     *OFF
090000970212     C                   DELETE    FNSAT004
090100970212     C     FNSATK        READE     FNSAT04L                               99
090200970212    3C                   ENDDO
090300970212     C                   ADD       1             XX
090400970212    2C                   ENDDO
090500970212     C*
090600970212     C                   CLOSE     FNSAT04L
090700970212     C                   END
090800940505      *
090900940505      * Come prima cosa devo verificare se giornata festiva
091000940505      *   solo per ELABORAZIONE o  per stampe giornaliere
091100950210     C     DS1FGS        IFEQ      0
091200950210     C     DS1RIS        ORNE      'R'
091300950118     C                   OPEN      AZCLN01L
091400940505     C                   CLEAR                   $TFA
091500940505      *
091600950118     C     AZCLNK        CHAIN     AZCLN01L                           99
091700950118     C                   CLOSE     AZCLN01L
091800940505     C     *IN99         IFEQ      *OFF
091900950118     C                   MOVEA     CLNMAT        GME
092000970211     C     GME($GI)      IFEQ      'F'
092100970211     C* SE FESTIVO SCRIVO RECORD IN FNSAT CON TRE = F
092200970211     C                   CLEAR                   FNSAT000
092300970211     C                   Z-ADD     SIMFEL        SATFLE
092400970211     C                   MOVEL     'F'           SATTRE
092500970211     C                   Z-ADD     DS1ADA        SATDRE
092600040922     c                   movel     $ftr          satftr
092700040922     c                   z-add     $DTR          satdtr
092800970311     C                   WRITE     FNSAT000                             99
092900970211     C                   GOTO      ENDLEG
093000970211     C                   ENDIF
093100940505     C                   ENDIF
093200940505     C                   ENDIF
093300940429      *
093400940429     C* LANCIO DA FILTRO
093500950210    1C     DS1FGS        IFGT      0
093600940429     C* 29 ON - IMMESSA LINEA DI PARTENZA
093700950210    2C     DS1LNP        IFGT      0
093800940429     C                   SETON                                        29
093900950210    3C     DS1LNP        IFNE      DS1FEL
094000940502     C                   SETON                                        27
094100940429    3C                   ENDIF
094200940429    2C                   ENDIF
094300940429     C*
094400940429   X1C                   ELSE
094500940502     C                   SETON                                        05
094600940506     C                   CLEAR                   STRIP
094700940429     C* STAMPE GIORNALIERE
094800940429    2C     LIN(2)        IFEQ      0
094900940429     C                   SETON                                        29
095000940429    2C                   ENDIF
095100940429     C     SIMFEL        LOOKUP    LIN                                    99
095200940502     C  N99              SETON                                        27
095300940429    1C                   ENDIF
095400000912     C* SE SONO TUTTI P.O. POSTE, NON ENTRO NEMMENO NEL CICLO DI LETT
095500000912     C*   E RISTAMPA
095600000912     C                   Z-ADD     1             $X
095700000912     C                   CLEAR                   WSTAF
095800000912    1C     LIN($X)       DOWGT     0
095900000912     C     WSTAF         ANDEQ     ' '
096000000912     C                   Z-ADD     LIN($X)       $FILPR
096100000912     C* ESCLUDO I P.O. POSTE
096200020813     ***C                   CLEAR                   §OGPT
096300020813     C                   clear                   OG143
096400000912     C     $FILPR        CHAIN     AZORG01L                           82
096500000912     C  N82              MOVEL     ORGDE3        OG143
096600000912     C**
096700020813    2***C     §OGPT         IFNE      'S'
096800020813    2C                   IF        §OGntw <> 'PPT'
096900000912     C* SE ELABORATA ALMENO UNA LINEA, STAMPO ANCHE I TOTALI FINALI
097000000912     C                   MOVEL     'S'           WSTAF             1
097100000912    2C                   ENDIF
097200000912     C                   ADD       1             $X
097300000912    1C                   ENDDO
097400000000      *
097500000000      *
097600000912    1C     WSTAF         IFEQ      'S'
097700000000      * Scrivo, in stampa, se selezionata filiale o area
097800000000     C                   MOVE      *BLANKS       STRIP1
097900950210     C  N05DS1LNP        IFNE      *ZERO
098000960925     C                   MOVEL     'P.O.'        STRIP1
098100950210     C                   MOVE      DS1LNP        STRIP1
098200000000     C                   ELSE
098300940502     C                   MOVEL     'AREA'        STRIP1
098400950210     C                   MOVE      DS1FEL        STRIP1
098500000000     C                   ENDIF
098600000000      *
098700000000      * Inizializzo i campi per l'elaborazione
098800000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
098900000000     C                   Z-ADD     *ZERO         $FILPR
099000000000     C                   MOVE      *BLANKS       $CBLPR
099100000000     C                   Z-ADD     *ZERO         $NFVPR
099200940509     C                   CLEAR                   $RIGHE
099300950210      *
099400950210      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
099500950210     C                   MOVEA     £MOD          CMD(33)
099600950210     C                   MOVEA     £OUTQ         CMD(51)
099700950210      *
099800940502      * Eseguo OVRPRTF sul file di stampa
099900950210     C                   Z-ADD     82            LUNG             15 5
100000950213     C                   MOVEL     *BLANKS       COMMAN           82
100100940502     C                   MOVEA     CMD           COMMAN
100200940502     C                   CALL      'QCMDEXC'                            H1
100300940502     C                   PARM                    COMMAN
100400940502     C                   PARM                    LUNG
100500970807     C  N38              OPEN      FNLSC2P
100600950118     C  N38              WRITE     LSC2T1
100700950118     C  N38              WRITE     LSC2T3
100800940502     C  N38              SETOFF                                       30
100900000619     C**
101000000619     C* SE DS1RIS= 'S' ACCENDO PER ORA IL 38
101100000619     C     SAVRIS        IFNE      ' '
101200000619     C                   SETON                                        38
101300000619     C                   ENDIF
101400000000      *
101500950210      * L'elaborazione solo se non scelta la ristampa (DS1RIS='R')
101600950210    1C     DS1RIS        IFNE      'R'
101700000000      *
101800970205      * Leggo il file delle bolle in partenza prima quelli con data
101900970205      * F.V. = 0 poi quelli con DFV >= della data elaborazione
102000940401     C                   Z-ADD     1             $X                3 0
102100940428     C                   MOVE      *BLANK        $FLAG
102200940428     C*
102300940428    2C     LIN($X)       DOWGT     *ZERO
102400000912     C                   Z-ADD     LIN($X)       $FILPR
102500000912      * Escludo bolle LNP Poste
102600020813     ***C                   CLEAR                   §OGPT
102700020813     C                   clear                   OG143
102800000912     C     $FILPR        CHAIN     AZORG01L                           82
102900000912     C  N82              MOVEL     ORGDE3        OG143
103000000912     C**
103100020813   2A***C     §OGPT         IFNE      'S'
103200020813    2C                   IF        §OGntw <> 'PPT'
103300940428     C                   CLEAR                   $DFV
103400940428     C                   SETOFF                                           01
103500940428      *
103600940428    3C                   DO        2
103700021023     C     FNBLTK        SETLL     FNBLT38L
103800021023     C  N01FNBLTK        READE     FNBLT38L                               33
103900021023     C   01LIN($X)       READE     FNBLT38L                               33
104000940428     C*
104100940428    4C     *IN33         DOWEQ     *OFF
104200000000      *
104300000000      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
104400000000      * come test ma mi permette di saltare delle chain)
104500940428    5C     $BLTAT        IFNE      $BLTPR
104600950118     C     FNBLPK        CHAIN     FNBLP01L                           99
104700000000     C   99              GOTO      LETTUR
104800000000     C                   MOVE      $BLTAT        $BLTPR
104900000000     C                   Z-ADD     BLPNFV        $NFVPR
105000950118     C                   Z-ADD     BLPMGS        $DATSP            8 0
105100000000     C                   MOVEL     BLPAAS        $DATSP
105200000331     C                   Z-ADD     BLPDRT        $DTRIT
105300000413     C                   Z-ADD     BLPPDR        $CDPDR
105400940428     C* $BOLLA = 1 INDICA CHE HO LETTO UNA NUOVA BOLLA
105500940428     C                   MOVEL     '1'           $BOLLA            1
105600000000      *
105700000000      * Decodifico il tipo bolla
105800950118    6C     BLPCBO        IFNE      $CBLPR
105900950210     C                   MOVE      '3A'          KCOD
106000950210     C                   MOVE      *BLANKS       KKEY
106100950210     C                   MOVEL     BLPCBO        KKEY
106200950210     C     KTAB1         CHAIN     TABEL00F                           99
106300000000     C  N99              MOVEL     TBLUNI        DS3A
106400950118     C  N99              MOVE      BLPCBO        $CBLPR
106500000000     C   99              CLEAR                   DS3A
106600940428    6C                   ENDIF
106700940427     C*
106800940428    5C                   ENDIF
106900000000      *
107000940428      * ESCLUSIONI BOLLE
107100000000     C                   EXSR      ESCLUS
107200940428    5C     $OK           IFEQ      'S'
107300000000     C                   EXSR      ELABOR
107400940428    5C                   ENDIF
107500000000      *
107600940428     C     LETTUR        TAG
107700021023     C  N01FNBLTK        READE     FNBLT38L                               33
107800021023     C   01LIN($X)       READE     FNBLT38L                               33
107900940428    4C                   ENDDO
108000940428      *
108100940428     C                   SETON                                        01
108200970205     C                   Z-ADD     DS1AAL        $DFV
108300940428    3C                   ENDDO
108400940428      *
108500940428      * cambia la filiale --> decodifico e stampo i totali
108600940428     C                   EXSR      CALSPE
108700940428     C                   EXSR      WRISAT
108800940429     C                   EXSR      WR1SAT
108900940428     C  N38              EXSR      TOTFIL
109000940428     C                   EXSR      AZZFIL
109100000912   2AC                   ENDIF
109200940428     C*
109300940401     C                   ADD       1             $X
109400940428    2C                   ENDDO
109500940429      *
109600940429     C*
109700940429     C* STAMPA DEI DATI FINALI
109800940506     C                   EXSR      STAFIN
109900000616   X C                   ENDIF
110000000616      *
110100000616     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
110200000616     C     SAVRIS        IFEQ      'S'
110300000616     C                   MOVEL     'R'           DS1RIS
110400000616     C                   MOVEA     SVLIN         LIN
110500000616     C* AZZERO LE SCHIERE DI TOTALE
110600000616     C                   EXSR      AZZSER
110700000616     C                   EXSR      AZZTRA
110800000616     C                   EXSR      AZZTOT
110900000619     C                   SETOFF                                       38
111000000616     C                   ENDIF
111100000000      *
111200970211      * Nel caso di RISTAMPA mi limito a rileggere il file statistiche (FNSAT)
111300000000      * e stampare i record che rientrano nell'intervallo dal-al
111400000616     C     DS1RIS        IFNE      ' '
111500000000     C                   EXSR      RISTAM
111600940428    1C                   ENDIF
111700000000      *
111800000000      * Stampa terminata
111900940428     C     *IN38         IFEQ      *OFF
112000940429     C*
112100940428     C     $ALMEN        IFEQ      *ZERO
112200950118     C                   WRITE     LSC2E1
112300940505     C                   ELSE
112400940429     C*
112500000000     C                   EXSR      STATER
112600940505     C                   ENDIF
112700000000      *
112800950118     C                   WRITE     LSC2E2
112900950629     C                   WRITE     LSC2E5
113000000000      *
113100000000      * Verifico se vi sono fogli viaggio aperti
113200000000     C                   EXSR      FOGVIA
113300000000      *
113400970807     C                   CLOSE     FNLSC2P
113500940428     C                   ENDIF
113600000000      *
113700000000      * Aggiorno la data ultima elaborazione
113800950210     C     DS1RIS        IFNE      'R'
113900000616     C     SAVRIS        OREQ      'S'
114000021016
114100021016     c                   Eval      kTbeCod = 'SDF'
114200021016     c                   Movel(p)  Simfel        kTbeKe1
114300021016     c     Ktbe          Chain     Tntbe01l
114400021016     c                   If        %Found(Tntbe01l)
114500021016     c                   Movel     TbeUni        dSdf
114600021016     c                   If        §SdfTerP = *Zeros or §SdfTerP > Ds1Ada
114700021016     c                   Z-Add     Ds1Ada        §SdfTerP
114800021016     c                   EndIf
114900021016     c                   If        §SdfTerU < Ds1Ada
115000021016     c                   Z-Add     Ds1Ada        §SdfTerU
115100021016     c                   EndIf
115200021016     c                   Movel     dSdf          TbeUni
115300021016     c                   Update    Tntbe000
115400021016     c                   EndIf
115500000000     C                   ENDIF
115600000912     C*
115700000912    1C                   ENDIF
115800000000      *
115900940505     C     ENDLEG        ENDSR
116000000000      **************************************************************************
116100000000      * Una volta letto il record verifico se da includere o no
116200940427      * PER QUALSIASI TIPO DI CALCOLO  (COLLI E BOLLE A TERRA E
116300940427      *  COLLI E BOLLA DA PARTIRE)
116400000000      *
116500940427      * Vi sono solo due motivi per escludere il record letto:
116600940427      * (1) se la spedizione e' stata bollettata dopo la data elaboraz
116700950210      *                Data spedizione > DS1AAL
116800000000      *
116900940427      * (2) se la data partenza del collo e' < della data elaborazione
117000950210      *                BLTDFV < DS1AAL
117100000000      *
117200000810      * ATTENZIONE.. DA OGGI C'E' UN MOTIVO IN PIU'.. Canale5 ! da cantare ..
117300000810      *
117400000810      * (3) se la linea di partenze è ??..... POSTE !
117500000810      *
117600940427      * Bolle di recupero / bolle legate / bolle "£" sono escluse
117700000000      **************************************************************************
117800000000     C     ESCLUS        BEGSR
117900000000      *
118000000000     C                   MOVE      'N'           $OK               1
118100000331     C                   MOVE      *BLANKS       WSOS              1
118200000810      *
118300000810      *
118400940427      * esclusioni:
118500950323      * Le bolle "legate" sono identificabili dal nr.foglio viaggio
118600950323      * = 99999 O = 99998
118700070522    1C     $NFVPR        IFEQ      99999
118800950323     C     $NFVPR        OREQ      99998
118900940427     C     $CBLPR        OREQ      '£'
119000940428     C     §3ARBL        OREQ      'R'
119100000000     C                   GOTO      ENDESC
119200070522    1C                   ENDIF
119300000413     C* Prepagati non confermati
119400070522    1C     $CDPDR        IFEQ      *ZEROS
119500000413     C                   GOTO      ENDESC
119600070522    1C                   ENDIF
119700000000      *
119800070522    1C     BLTDFV        IFEQ      DS1AAL
119900950629     C                   MOVEL     'S'           $OK
120000070522   x1C                   ELSE
120100070522     C
120200070522     c* Elaborazione in base A     Data BORDERÒ
120300070522   1AC                   IF        DS1AAL<§VPOnpi
120400070522     C
120500940427      * La data di spedizione deve essere <= alla data elaborazione
120600070522    2C     $DATSP        IFLE      DS1AAL
120700000000      *
120800940427      * La data partenza deve essere > = alaa data elaborazione        limiti
120900070522    3C     BLTDFV        IFEQ      *ZERO
121000950210     C     BLTDFV        ORGE      DS1AAL
121100000000     C                   MOVE      'S'           $OK
121200000000     C                   GOTO      ENDESC
121300070522    3C                   ENDIF
121400070522   X2C                   ELSE
121500000331     C* Se data spedizione > di data elaborazione
121600000411     C* e  data ritiro    <= data elaborazione    ==> BOLLA IN SOSPENS.
121700070522    3C     $DTRIT        IFLE      DS1AAL
121800000331     C                   MOVE      'S'           $OK
121900000331     C                   MOVEL     'S'           WSOS
122000070522     C                   GOTO      ENDESC
122100070522    3C                   ENDIF
122200950626     C*
122300070522    2C                   ENDIF
122400070522     c
122500070522  x1ac                   else
122600070522     c****
122700070522     c* ELAborazione in base a    data RITIRO Merce
122800070522      * La data ritiro deve essere <= alla data elaborazione
122900070522    2C     $DTRIT        IFLE      DS1AAL
123000070522      *
123100070525      * La data partenza deve essere > = alla data elaborazione        limiti
123200070522    3C     BLTDFV        IFEQ      *ZERO
123300070522     C     BLTDFV        ORGE      DS1AAL
123400070522     C                   MOVE      'S'           $OK
123500070522     C                   GOTO      ENDESC
123600070522    3C                   ENDIF
123700070522    2C                   ENDIF
123800070522   1aC                   ENDIF
123900950629     C*
124000070522    1C                   ENDIF
124100000000      *
124200000000     C     ENDESC        ENDSR
124300000000      **************************************************************************
124400000000      * Elaborazioni dei record letti, somme nei totali
124500000000      **************************************************************************
124600000000     C     ELABOR        BEGSR
124700000000      *
124800970210      * Verifico il tipo spedizione ESPR./DIST./CORR.
124900950201     C                   Z-ADD     3             $T                1 0
125000000000     C     BLPTSP        IFEQ      'E'
125100000000     C                   Z-ADD     1             $T
125200000000     C                   ENDIF
125300950201     C     BLPTSP        IFEQ      'D'
125400000619     C     BLPTSP        OREQ      'P'
125500940323     C                   Z-ADD     2             $T
125600000000     C                   ENDIF
125700000000      *
125800000000      * C/servizio oppure no ?
125900940428     C     §3ARBL        COMP      'C'                                3434
126000000000      *
126100970205     C*
126200070522    1C     WSOS          IFEQ      *BLANKS
126300970205     C* DETERMINAZIONE SE COLLO A TERRA
126400000000      * Ho due controlli da fare per sapere se il collo e' a terra:
126500000000      *    1)  BLTDFV = 0
126600950210      *    2)  BLTDFV > Data spedizione   AND   BLTDFV > DS1AAL
126700000000     C                   SETOFF                                       36
126800070522    2C     BLTDFV        IFEQ      *ZERO
126900950210     C     BLTDFV        ORGT      DS1AAL
127000000000     C                   SETON                                        36
127100070522    2C                   ENDIF
127200970303     C*
127300970303     C* RICHIAMO PGM PER CALCOLO AFFIDABILITA'
127400970303     C* (SOLO SE HO LETTO UNA NUOVA BOLLA - $BOLLA = '1')
127500070522    2C     $BOLLA        IFEQ      '1'
127600070522    3C     BLPDCE        IFEQ      *ZEROS
127700970303     C     BLPDUC        ORNE      DS1AAL
127800970303     C                   MOVEL     ' '           D98TLA
127900970303     C                   MOVEL     'P'           D98TBO
128000970303     C                   MOVEL     BLPAAS        D98AAS
128100970303     C                   MOVEL     BLPLNP        D98LNP
128200970303     C                   MOVEL     BLPNRS        D98NRS
128300970303     C                   MOVEL     BLPNSP        D98NSP
128400970310     C                   Z-ADD     DS1AAL        D98DRF
128500970310     C                   Z-ADD     2359          D98ORF
128600970925     C                   CALL      'TNSD99R'
128700970925     C                   PARM                    DS99
128800970303     C                   ELSE
128900970303     C                   Z-ADD     BLPNPI        D98NPI
129000070522    3C                   END
129100070522    2C                   END
129200970207     C*
129300970207     C*VERIFICO SE SI TRATTA DI "A TERRA DA PARTIRE"
129400970210     C* La considero "non da partire" se si tratta di:
129500970207     C* - spedizione non borderizzata
129600970303     C* - distribuzione a terra del giorno
129700070522     c* - dal 1° giugno 2007 il calcolo della affidabilità a terra sarà
129800070522     c*   in base alla data ritiro, per cui nel DA PARTIRE non controllo
129900070522     c*   più la data borderò ma solo se si tratta della distribuzione
130000070522     c*   del giorno
130100970205     C                   SETOFF                                       35
130200070522     c
130300070522    2C     *IN36         IFEQ      *ON
130400070522     c
130500070522    3c                   if        ds1aal<§vponpi
130600970210     C*
130700070522    4C     BLPDBR        IFGT      DS1AAL
130800070522     C     BLPDBR        OREQ      *ZEROS
130900070522     C                   SETON                                        35
131000070522   x4C                   ELSE
131100970304     C*
131200070522    5C     BLPDBR        IFLE      DS1AAL
131300070522     C     BLPDPC        ANDEQ     0
131400070522     C     D98NPI        ANDLT     0
131500070522     C*
131600070522     C     BLPDBR        ORLE      DS1AAL
131700070522     C     BLPDPC        ANDGT     DS1AAL
131800070522     C     D98NPI        ANDLT     0
131900070522     C                   SETON                                        35
132000070522    5C                   END
132100070522    4C                   END
132200070522     c
132300070522   x3c                   else
132400070522     c*
132500070522    4c                   if        (blpdpc=0 and d98npi<0) or
132600070522     c                             (blpdpc>ds1aal and d98npi<0)
132700070522     C                   SETON                                        35
132800070522    4c                   endif
132900070522    3c                   endif
133000970210     C*
133100070522    2C                   END
133200070522    1C                   ENDIF
133300970205     C*
133400000000      *
133500970205      * Aggancio, nel file statistiche, il record per il terminal di
133600970205      * arrivo poiche' lo dovrò aggiornare
133700970205     C                   CLEAR                   FNSAT000
133800000000     C                   Z-ADD     SIMFEL        SATFLE
133900950210     C                   Z-ADD     DS1AAL        SATDRE
134000940428     C                   Z-ADD     BLPLNP        SATLNP
134100000000     C                   Z-ADD     BLPTFA        SATTFA
134200000000     C                   MOVE      *BLANK        SATTRE
134300970205     C     FNSA1K        CHAIN     FNSAT02L                           37
134400940428      *
134500970205      * NUOVA BOLLA LETTA: CONTO LE SPEDIZIONI
134600940428      *
134700940428     C     $BOLLA        IFEQ      '1'
134800940428     C                   EXSR      DATBOL
134900970205     C                   ENDIF
135000021217      *
135100021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
135200030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
135300030213      * Peso medio
135400030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
135500030213if  1c***                if        Wsipes = 'S'
135600030213     c***                z-add     BLTpuc        $Peso
135700030213   x1c***                else
135800021220     c                   z-add     Wpesmed       $Peso
135900030213e   1c***                endif
136000021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
136100030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
136200030213      * Volume medio
136300030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
136400030213if  1c***                if        Wsivol = 'S'
136500030213     c***                z-add     BLTvuc        $Volum
136600030213   x1c***                else
136700021220     c                   z-add     Wvolmed       $Volum
136800030213e   1c***                endif
136900000000      *
137000000000      * ?  CALCOLI  SUI  COLLI  ?
137100000000      *
137200000331     C     WSOS          IFEQ      *BLANKS
137300000000      * Nr.colli da partire (servira' per la percentuale)
137400970207     C     *IN35         IFEQ      *OFF
137500970210     C   34              ADD       1             CPA($T)
137600970210     C  N34              ADD       1             C4A($T)
137700970210     C                   ADD       1             CDT($T)
137800950626     C                   END
137900000000      *
138000940427     C     *IN36         IFEQ      *ON
138100970207      * A terra da partire                                             (35ON)
138200940427     C   34
138300940427     CANN35              ADD       1             CBP($T)
138400940427     C  N34
138500940427     CANN35              ADD       1             C4P($T)
138600970207     C  N35              ADD       1             C0P($T)
138700970207     C  N35              ADD       1             C3P($T)
138800970207      * A terra non da partire                                         (35ON)
138900940427     C   34
139000940427     CAN 35              ADD       1             CBR($T)
139100940427     C  N34
139200940427     CAN 35              ADD       1             C4R($T)
139300940427     C   35              ADD       1             C0R($T)
139400940427     C                   ENDIF
139500000000      *
139600000000      * ?  CALCOLI  SUL  PESO  ?
139700000000      *
139800970207     C     *IN35         IFEQ      *OFF
139900970210     C   34              ADD       $PESO         PPA($T)
140000970210     C  N34              ADD       $PESO         P4A($T)
140100970210     C                   ADD       $PESO         PDT($T)
140200950626     C                   END
140300940502      * A TERRA
140400940427     C     *IN36         IFEQ      *ON
140500940502      *
140600970206      * Fino a 35 quintali
140700021217     ***C     BLPPKF        IFLE      3500
140800021217     C     Wpeso         IFLE      3500
140900970207     C* Da partire
141000970206     C   34
141100970206     CANN35              ADD       $PESO         PFI($T)
141200970206     C  N34
141300970206     CANN35              ADD       $PESO         P4I($T)
141400970206     C  N35              ADD       $PESO         P3I($T)
141500970206     C  N35              ADD       $PESO         P0I($T)
141600970207     C* Non da partire
141700970206     C   34
141800970206     CAN 35              ADD       $PESO         PFN($T)
141900970206     C  N34
142000970206     CAN 35              ADD       $PESO         P4N($T)
142100970206     C   35              ADD       $PESO         P0N($T)
142200970206     C                   ELSE
142300970206      * Oltre 35 quintali:                                             (35ON)
142400970207     C* Da partire
142500940427     C   34
142600940427     CANN35              ADD       $PESO         PBP($T)
142700940427     C  N34
142800940427     CANN35              ADD       $PESO         P4P($T)
142900970206     C  N35              ADD       $PESO         P0P($T)
143000970206     C  N35              ADD       $PESO         P3P($T)
143100970207     C* Non da partire
143200940427     C   34
143300940427     CAN 35              ADD       $PESO         PBR($T)
143400940427     C  N34
143500940427     CAN 35              ADD       $PESO         P4R($T)
143600940427     C   35              ADD       $PESO         P0R($T)
143700940427     C                   ENDIF
143800970206     C                   ENDIF
143900000331     C*
144000000331     C                   ENDIF
144100000000      *
144200000000      * ?  CALCOLI  SUL  VOLUME  ?
144300940329      *
144400000000      *
144500021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
144600021217     ***C     BLTVUC        IFEQ      *ZERO
144700021217     *** *
144800021217     ***C     BLPVLF        DIV(H)    BLPNCL        $VOLUM           12 4
144900021217     *** *
145000021217     ***C                   ELSE
145100021217     ***C                   Z-ADD     BLTVUC        $VOLUM
145200021217     ***C                   ENDIF
145300000000      *
145400000331     C     WSOS          IFEQ      *BLANKS
145500970207     C     *IN35         IFEQ      *OFF
145600970210     C   34              ADD       $VOLUM        VPA($T)
145700970210     C  N34              ADD       $VOLUM        V4A($T)
145800970210     C                   ADD       $VOLUM        VDT($T)
145900950626     C                   END
146000940505      *
146100940428     C     *IN36         IFEQ      *ON
146200970207     C* Da partire
146300940428     C   34
146400940428     CANN35              ADD       $VOLUM        VBP($T)
146500940428     C  N34
146600940428     CANN35              ADD       $VOLUM        V4P($T)
146700970206     C  N35              ADD       $VOLUM        V0P($T)
146800970206     C  N35              ADD       $VOLUM        V3P($T)
146900000331     c* Sia che sia da partire che non da partire: sommo nel totale
147000000331     C* Mc rimasti a magazzino
147100000331     C   34              ADD       $VOLUM        VBR($T)
147200000331     C  N34              ADD       $VOLUM        V4R($T)
147300000331     C                   ADD       $VOLUM        V0R($T)
147400000406     C                   ADD       $VOLUM        V3R($T)
147500000000      *
147600000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
147700000000      *
147800970205      * Visto che le spedizioni non riesco a calcolarle ora, mi limito
147900970205      * a tenere aggiornato il file di work con le bolle
148000940428     C   34              MOVE      *BLANK        WRKCSE
148100940428     C  N34              MOVE      'C'           WRKCSE
148200940503     C                   Z-ADD     BLPLNP        WRKLNP
148300950118     C     FNWR1K        CHAIN     FNWRK00F                           99
148400940428     C     *IN99         IFEQ      *OFF
148500000000     C                   ADD       1             WRKNC1
148600000000     C                   EXCEPT    AGGIO
148700000000     C                   ELSE
148800950118     C                   MOVEL     *BLANKS       WTRANS            1
148900940503     C                   EXSR      WRIWRK
149000000000     C                   ENDIF
149100940428     C                   ENDIF
149200000331     c* Se collo di bolla in sospensione sommo volume in totale mc
149300000331     c* rimasti a magazzino
149400000331     C                   ELSE
149500000331     C   34              ADD       $VOLUM        VBR($T)
149600000331     C  N34              ADD       $VOLUM        V4R($T)
149700000331     C                   ADD       $VOLUM        V0R($T)
149800000406     C                   ADD       $VOLUM        V3R($T)
149900000412     C                   ADD       $VOLUM        VOLUME            9 4
150000000331     C                   ENDIF
150100940323      *
150200040921     C                   MOVE      $FTR          SATFTR
150300040921     C                   MOVE      $DTR          SATDTR
150400940428     C     *IN37         IFEQ      *ON
150500040921     C****               MOVE      $FTR          SATFTR
150600040921     C****               MOVE      $DTR          SATDTR
150700970205     C                   WRITE     FNSAT000
150800940428     C                   ELSE
150900970205     C                   UPDATE    FNSAT000
151000940428     C                   ENDIF
151100000000      *
151200000000     C                   ENDSR
151300940428      **************************************************************************
151400940428      * Carico i dati della bolla (peso e volume)
151500940428      *  e conto le spedizioni partite il giorno di elaborazione
151600940428      **************************************************************************
151700940428     C     DATBOL        BEGSR
151800940428      *
151900021217     *** * Calcolo il peso di un singolo collo
152000021217     ***C     BLPPKF        DIV(H)    BLPNCL        $PESO             8 1
152100021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
152200021217     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
152300021217     *** * BLPVLC=Volume rilevato)
152400050112     ***C     BLPNCR        ifeq      BLPNCL
152500021217     ***C                   Z-ADD     BLPVLC        BLPVLF
152600021217     ***C                   ELSE
152700021217     ***C     BLPVLC        IFGE      BLPVLF
152800021217     ***C                   Z-ADD     BLPVLC        BLPVLF
152900021217     ***C                   ENDIF
153000021217     ***C                   ENDIF
153100021217      *
153200021217      * Verifico i dati misurati con la macchina smistacolli
153300021217      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
153400021220      * e calcolo il VOLUME unitario medio per collo da usare se non
153500021220      * tutti i colli sono stati volumati dalla smistacolli
153600021217      *
153700030213      **** Verifico se sono stati volumati tutti i colli
153800030213     c***                clear                   Wsivol
153900021217     c                   clear                   Wvolu
154000050112     c                   if        BLPncr = BLPncl
154100021217     c                   z-add     BLPvlc        Wvolu
154200030213     c***                eval      Wsivol =  'S'
154300021217   x1c                   else
154400021217if  2c                   if        BLPvlc >= BLPvlf
154500021217     c                   z-add     BLPvlc        Wvolu
154600021217   x2c                   else
154700021217     c                   z-add     BLPvlf        Wvolu
154800021217e   2c                   endif
154900021217e   1c                   endif
155000021220      *
155100021220      * Calcolo il volume medio
155200021220     c     Wvolu         div(H)    BLPncl        Wvolmed
155300021217      *
155400021217      * Verifico i dati misurati con la macchina smistacolli
155500021217      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
155600021220      * e calcolo il PESO   unitario medio per collo da usare se non
155700021220      * tutti i colli sono stati pesati   dalla smistacolli
155800021217      *
155900030213      **** Verifico se sono stati pesati   tutti i colli
156000030213     c***                clear                   Wsipes
156100021217     c                   clear                   Wpeso
156200050112     c                   If        BLPncp = BLPncl
156300021217     c                   z-add     BLPpkc        Wpeso
156400030213     c***                eval      Wsipes =  'S'
156500021217   c1c                   else
156600021217if  2c                   if        BLPpkc >= BLPpkf
156700021217     c                   z-add     BLPpkc        Wpeso
156800021217   x2c                   else
156900021217     c                   z-add     BLPpkf        Wpeso
157000021217e   2c                   endif
157100021217e   1c                   endif
157200021220      *
157300021220      * Calcolo il peso medio
157400021220     c     Wpeso         div(H)    BLPncl        Wpesmed
157500940428     C*
157600940428     C* SOMMO SOLTANTO LE SPEDIZIONI PARTITE NEL GIORNO
157700970205     C*  LA DATA DI RIFERIMENTO E' LA DATA ULTIMO COLLO
157800940506     C     $FLAG         IFEQ      ' '
157900970205     C     BLPDUC        ANDEQ     DS1AAL
158000000331     C     WSOS          ANDEQ     *BLANKS
158100950118     C                   MOVEL     *BLANKS       WTRANS
158200940503     C                   EXSR      CTRBOL
158300970205     C                   ENDIF
158400940428     C*
158500940428     C                   MOVEL     '2'           $BOLLA
158600940428     C                   ENDSR
158700950118      **************************************************************************
158800021217      * Carico i dati della bolla (peso e volume) (TRANSITI)
158900021217     *** *  e conto le spedizioni partite il giorno di elaborazione
159000021217     *** *    (questo NON lo faceva NEANCHE prima)
159100950118      **************************************************************************
159200950118     C     DATBOT        BEGSR
159300950118      *
159400021217     *** * Calcolo il peso di un singolo collo
159500021217     ***C     BTPPKB        DIV(H)    BTPNCL        $PESO             8 1
159600021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
159700021217     *** * misurati con la macchina smistacolli (BTPNCR=Numero colli rilevati,
159800021217     *** * BTPVLC=Volume rilevato)
159900050112     ***C     BTPNCR        ifeq      BTPNCL
160000021217     ***C                   Z-ADD     BTPVLC        BTPVLB
160100021217     ***C                   ELSE
160200021217     ***C     BTPVLC        IFGE      BTPVLB
160300021217     ***C                   Z-ADD     BTPVLC        BTPVLB
160400021217     ***C                   ENDIF
160500021217     ***C                   ENDIF
160600021217     ***C*
160700021220      *
160800021220      * Verifico i dati misurati con la macchina smistacolli
160900021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
161000021220      * e calcolo il VOLUME unitario medio per collo da usare se non
161100021220      * tutti i colli sono stati volumati dalla smistacolli
161200021217      *
161300030213      **** Verifico se sono stati volumati tutti i colli
161400030213     c***                clear                   Wsivol
161500021217     c                   clear                   Wvolu
161600050112     c                   if        BTPncr = BTPncl
161700021217     c                   z-add     BTPvlc        Wvolu
161800030213     c***                eval      Wsivol =  'S'
161900021217   x1c                   else
162000021217if  2c                   if        BTPvlc >= BTPvlb
162100021217     c                   z-add     BTPvlc        Wvolu
162200021217   x2c                   else
162300021217     c                   z-add     BTPvlb        Wvolu
162400021217e   2c                   endif
162500021217e   1c                   endif
162600021220      *
162700021220      * Calcolo il volume medio
162800021220     c     Wvolu         div(H)    BTPncl        Wvolmed
162900021220      *
163000021220      * Verifico i dati misurati con la macchina smistacolli
163100021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
163200021220      * e calcolo il PESO   unitario medio per collo da usare se non
163300021220      * tutti i colli sono stati pesati   dalla smistacolli
163400021217      *
163500030213      **** Verifico se sono stati pesati   tutti i colli
163600030213     c***                clear                   Wsipes
163700021217     c                   clear                   Wpeso
163800050112     c                   if        BTPncp = BTPncl
163900021217     c                   z-add     BTPpkc        Wpeso
164000030213     c***                eval      Wsipes =  'S'
164100021217   x1c                   else
164200021217if  2c                   if        BTPpkc >= BTPpkb
164300021217     c                   z-add     BTPpkc        Wpeso
164400021217   x2c                   else
164500021217     c                   z-add     BTPpkb        Wpeso
164600021217e   2c                   endif
164700021217e   1c                   endif
164800021220      *
164900021220      * Calcolo il peso medio
165000071231     c     Wpeso         div(H)    BTPncl        Wpesmed
165100021217      *
165200950118     C                   MOVEL     '2'           $BOLLA
165300950118     C                   ENDSR
165400940503      **************************************************************************
165500950118      * Scrivo il file FNWRK00F
165600940503      **************************************************************************
165700940503     C     WRIWRK        BEGSR
165800950118     C*
165900940503     C                   MOVE      $FLAG         WRKFLG
166000950118     C                   Z-ADD     1             WRKNC1
166100950118     C*
166200950118     C     WTRANS        IFNE      'T'
166300940503     C                   Z-ADD     BLPAAS        WRKAAS
166400940503     C                   Z-ADD     BLPMGS        WRKMGS
166500940503     C                   Z-ADD     BLPNRS        WRKNRS
166600940503     C                   Z-ADD     BLPNSP        WRKNSP
166700940503     C                   Z-ADD     BLPLNA        WRKLNA
166800940503     C                   MOVE      BLPTSP        WRKTSP
166900940503     C                   Z-ADD     BLPNCL        WRKNCL
167000940503     C                   Z-ADD     BLPTFA        WRKTFA
167100070525     c* Se dopo la data di attivazione, memorizzo la data borderò
167200070525    3c                   if        ds1aal<§vponpi
167300970207     C                   Z-ADD     BLPDBR        WRKDBR
167400070525     c                   else
167500070525     C                   Z-ADD     BLPDrt        WRKDBR
167600070525     c                   endif
167700070525     c
167800970303     C                   Z-ADD     BLPDPC        WRKDPC
167900970303     C                   Z-ADD     BLPDUC        WRKDUC
168000970303     C                   Z-ADD     D98NPI        WRKNPI
168100950118     C*
168200950118     C                   ELSE
168300950118     C*
168400950118     C                   Z-ADD     BTPAAS        WRKAAS
168500950118     C                   Z-ADD     BTPMGS        WRKMGS
168600950118     C                   Z-ADD     BTPNRS        WRKNRS
168700950118     C                   Z-ADD     BTPNSP        WRKNSP
168800950118     C                   Z-ADD     BTPLNA        WRKLNA
168900950118     C                   MOVE      BTPTSP        WRKTSP
169000950118     C                   Z-ADD     BTPNCL        WRKNCL
169100950118     C                   Z-ADD     BTPTFA        WRKTFA
169200970207     C                   CLEAR                   WRKDBR
169300970303     C                   CLEAR                   WRKDUC
169400970303     C                   CLEAR                   WRKDPC
169500970303     C                   CLEAR                   WRKNPI
169600950118     C                   ENDIF
169700950118     C*
169800950118     C                   WRITE     FNWRK000
169900950118     C*
170000940503     C                   ENDSR
170100940503      **************************************************************************
170200940503      * Calcolo le spedizioni da partire:
170300950118      *  vedo se gia' contata prima di ricontarla(se esiste in FNWRK)
170400970207      *  Potrei averla già contata se i colli non sono sequenziali
170500940503      **************************************************************************
170600940503     C     CTRBOL        BEGSR
170700940503     C*
170800940505     C                   MOVEL     $FLAG         $FLAG2
170900940503     C                   MOVEL     'P'           $FLAG
171000940503     C                   MOVEL     BLPLNP        WRKLNP
171100940503     C                   CLEAR                   WRKCSE
171200950118     C     FNWR1K        SETLL     FNWRK00F                               99
171300940503     C* BOLLA NON TROVATA  LA SCRIVO
171400940503    1C     *IN99         IFEQ      *OFF
171500940503     C                   EXSR      WRIWRK
171600940503     C*
171700970211     C                   ADD       1             SDT($T)
171800970210     C   34              ADD       1             SPA($T)
171900940503     C* S4A --> PER C/S SPEDIZIONI DELL'AREA
172000940503     C*         PER I TRANSITI
172100970210     C  N34              ADD       1             S4A($T)
172200940503    1C                   ENDIF
172300940505     C                   MOVEL     $FLAG2        $FLAG
172400940503     C                   ENDSR
172500000000      **************************************************************************
172600000000      * Calcolo sulle spedizioni, lettura del file di work creato prima
172700000000      **************************************************************************
172800000000     C     CALSPE        BEGSR
172900940502     C                   SETOFF                                       3435
173000940428      * Verifico se e' transito
173100940502     C     $FLAG         COMP      'T'                                    35
173200000000      *
173300000000      * Leggo tutti le spedizioni della filiale da stampare dal file di work
173400970207    1C     $FLAG         IFEQ      *BLANK
173500950118     C     FNWRKK        SETLL     FNWRK00F
173600970207   X1C                   ELSE
173700950118     C     $FLAG         SETLL     FNWRK00F
173800970207    1C                   ENDIF
173900000000      *
174000970207    1C                   DO        *HIVAL
174100000000      *
174200970207    2C     $FLAG         IFEQ      *BLANK
174300950118     C     FNWRKK        READE(N)  FNWRK00F                               99
174400970207   X2C                   ELSE
174500950118     C     $FLAG         READE(N)  FNWRK00F                               99
174600970207    2C                   ENDIF
174700000000      *
174800970207    2C  N99              DO
174900000000      *
175000970210      * Verifico il tipo spedizione ESPR./DIST./CORR.
175100950201     C                   Z-ADD     3             $V                1 0
175200970207    3C     WRKTSP        IFEQ      'E'
175300000000     C                   Z-ADD     1             $V
175400970207    3C                   ENDIF
175500970207    3C     WRKTSP        IFEQ      'D'
175600940323     C                   Z-ADD     2             $V
175700970207    3C                   ENDIF
175800940428      *
175900940428      * Verifico se e' c/servizio
176000940502     C  N35WRKCSE        COMP      *BLANKS                                34
176100000000      *
176200000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
176300000000      * poiche' lo dovro aggiornare
176400970207     C                   CLEAR                   FNSAT000
176500000000     C                   Z-ADD     SIMFEL        SATFLE
176600950210     C                   Z-ADD     DS1ADA        SATDRE
176700940506     C  N35              Z-ADD     WRKLNP        SATLNP
176800940506     C   35              Z-ADD     SIMFEL        SATLNP
176900000000     C                   Z-ADD     WRKTFA        SATTFA
177000000000     C  N35              MOVE      *BLANK        SATTRE
177100000000     C   35              MOVE      'T'           SATTRE
177200970205     C     FNSA1K        CHAIN     FNSAT02L                           37
177300000000      *
177400950628     C                   MOVE      WRKMGS        $DATS1            8 0
177500950628     C                   MOVEL     WRKAAS        $DATS1
177600950628     C*
177700970207     C* A TERRA COMPLETAMENTE
177800970207      * La spedizione e' completa se il nr.colli spediti (WRKNC1) e' = uguale
177900050112      * al nr.colli della bolla (WRKNCL)
178000970207    3C     WRKNC1        IFEQ      WRKNCL
178100940506     C                   ADD       1             S0M($V)
178200970207     C* Transiti
178300970207    4C     *IN35         IFEQ      *ON
178400940506     C                   ADD       1             S2M($V)
178500970207   X4C                   ELSE
178600970207     C* Dell'area
178700940506     C   34              ADD       1             SCM($V)
178800940506     C  N34              ADD       1             S4M($V)
178900940506     C                   ADD       1             S3M($V)
179000970207     C*
179100970207    4C                   ENDIF
179200970207    3C                   ENDIF
179300970210     C* COMDAP = BLANKS => A TERRA DA PARTIRE
179400970207     C                   CLEAR                   COMDAP
179500970303     C* BORDERIZZATO, DISTRIBUZIONE A TERRA DEL GIORNO NON SPAREGGIATA
179600070525    3c                   if        ds1aal<§vponpi
179700070525     c
179800970207    3C     WRKDBR        IFGT      *ZEROS
179900970207     C     WRKDBR        ANDLE     DS1AAL
180000970303     C*
180100970303    4C     WRKDUC        IFEQ      *ZEROS
180200970303     C     WRKDPC        ANDEQ     0
180300970303     C     WRKNPI        ANDLT     0
180400970303     C*
180500970303     C     WRKDUC        OREQ      *ZEROS
180600970303     C     WRKDPC        ANDGT     DS1ADA
180700970303     C     WRKNPI        ANDLT     0
180800970303     C*
180900970303     C     WRKDUC        ORGT      DS1ADA
181000970303     C     WRKDPC        ANDGT     DS1ADA
181100970303     C     WRKNPI        ANDLT     0
181200970207     C                   MOVE      'N'           COMDAP            1
181300970207    4C                   END
181400970207   X3C                   ELSE
181500970207     C* NON BORDERIZZATO
181600970207     C                   MOVE      'N'           COMDAP
181700970207    3C                   END
181800070525     c*
181900070525     c                   else
182000070525     c
182100070525     c* calcolo per data ritiro a terra non da partire solo la distribu
182200070525     c*  zione del giorno
182300070525    4c                   if        (wrkdpc=0 and wrknpi<0) or
182400070525     c                             (wrkdpc>ds1aal and wrknpi<0)
182500070525     C                   MOVE      'N'           COMDAP
182600070525    4c                   endif
182700070525    3c                   endif
182800070525     c
182900970207     C*
183000970207    3C     COMDAP        IFEQ      *BLANKS
183100970207     C* Per i transiti memorizzo in un unico totale le spediz. a terra
183200970207     C* e non mi interessa nemmeno memorizzare il totale da partire
183300970207     C     $FLAG         OREQ      'T'
183400970207     C***
183500970207     C* TOTALE DA PARTIRE
183600970207     C***
183700970207     C* Sommo le spediz.a terra border. esclusa la distrib. del periodo
183800970210     C  N35              ADD       1             SDT($V)
183900970210     C  N35
184000970210     CAN 34              ADD       1             SPA($V)
184100970207     C* S4A --> Per c/s spedizioni dell'area
184200970210     C  N35
184300970210     CANN34              ADD       1             S4A($V)
184400970207     C***
184500970207     C* A TERRA DA PARTIRE
184600970207     C***
184700970207     C   34
184800970207     CANN35              ADD       1             SBP($V)
184900970207     C  N34
185000970207     CANN35              ADD       1             S4P($V)
185100970207     C  N35              ADD       1             S3P($V)
185200970207     C   35              ADD       1             S2P($V)
185300970207     C                   ADD       1             S0P($V)
185400970305     C***
185500970305     C* A TERRA DA PARTIRE FUORI AFFIDABILITA'
185600970305     C***
185700970305    4C     WRKNPI        IFGE      0
185800970305     C   34
185900970305     CANN35              ADD       1             FAF($V)
186000970305     C  N34
186100970305     CANN35              ADD       1             F4F($V)
186200970305     C  N35              ADD       1             F3F($V)
186300970305     C  N35              ADD       1             F0F($V)
186400970305    4C                   END
186500970207     c*
186600970207   X3C                   ELSE
186700970207     C***
186800970207     C* A TERRA NON DA PARTIRE
186900970207     C***
187000970207     C   34              ADD       1             SBR($V)
187100970207     C  N34              ADD       1             S4R($V)
187200970207     C                   ADD       1             S3R($V)
187300970207     C                   ADD       1             S0R($V)
187400970207    3C                   ENDIF
187500000000      *
187600040922     c  n37              move      $FTR          satftr
187700040922     c  n37              move      $DTR          satDTR
187800970207     C  N37              UPDATE    FNSAT000
187900000000      *
188000970207    2C                   ENDDO
188100970207    1C  N99              ENDDO
188200000000      *
188300000000     C                   ENDSR
188400000000      **************************************************************************
188500000000      * Calcolo i totali di filiale, li stampo e scrivo sul file
188600000000      **************************************************************************
188700000000     C     TOTFIL        BEGSR
188800000000      *
188900000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
189000000000      *
189100970211     C                   SETON                                        404145
189200000000     C                   Z-ADD     $FILPR        FILIAP
189300000000      *
189400000000      * Decodifico il codice filiale
189500000000     C     $FILPR        CHAIN     AZORG01L                           99
189600000000     C  N99ORGFVA        COMP      *BLANK                             9999
189700940503     C   99              CLEAR                   ORGDEP
189800000000     C  N99              MOVEL     ORGDES        ORGDEP
189900000000      *
190000000000      * ?  C  O   L   L  I  ?
190100970210     C                   XFOOT     CBP           COTOTP
190200970210     C                   XFOOT     CPA           $CDA
190300940428     C                   EXSR      PERCOL
190400000000      *
190500000000      * ?  P  E  S  O  ?
190600970210     C                   XFOOT     PFI           PEF35P
190700970210     C                   XFOOT     PBP           PEO35P
190800970210     C     PEF35P        ADD       PEO35P        PETOTP
190900970210     C                   XFOOT     PPA           $PDA
191000940428     C                   EXSR      PERPES
191100000000      *
191200000000      * ?  V  O  L  U  M E  ?
191300970210     C                   XFOOT     VBP           VOTOTP
191400970210     C                   XFOOT     VPA           $VDA
191500940428     C                   EXSR      PERVOL
191600000000      *
191700000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
191800970210     C                   XFOOT     SPA           $SDA
191900970305     C                   XFOOT     FAF           SPFAFF
192000940502     C                   EXSR      PERSPE
192100970305     C                   XFOOT     SBP           SPTOTP
192200970305     C                   XFOOT     SCM           SPCONP
192300970305     C                   XFOOT     SBR           $COMOD
192400970211     C     SPTOTP        ADD       $COMOD        SPTOTA
192500970211     C     SPTOTA        SUB       SPCONP        SPSPAP
192600000331      *
192700000331      * ? VOLUME A MAGAZZINO?
192800000331     C                   XFOOT     VBR           VOTOTM
192900000000      *
193000970210     C* STAMPO SOLO SE HO DEI DATI
193100000331     C     VOTOTM        IFGT      *ZEROS
193200000417     C* HO RIMESSO ANCHE TEST SU SPTOTA PER FAR FUNZIONARE LE RISTAMPE
193300000417     C* RELATIVE A DATE ELABORATE PRIMA CHE VENISSE FATTO IL CALCOLO
193400000417     C* DEL TOTALE VOLUME A MAGAZZINO
193500000417     C     SPTOTA        ORGT      *ZEROS
193600970210     C*
193700000000     C                   Z-ADD     $FILPR        FILIAP
193800000000     C                   EXSR      SCRIVI
193900940509     C                   ADD       1             $RIGHE            2 0
194000000000      *
194100970210      * ------------------ ESPR./DIST./CORR. --------------------------
194200000000      *
194300970210     C                   SETOFF                                       40
194400000000     C                   DO        3             $V
194500940429     C                   CLEAR                   DESCRP
194600940429      *
194700970210     C                   MOVE      ENS($V)       TIPOP
194800000000      *
194900000000      * ?  C  O   L   L  I  ?
195000970211     C                   Z-ADD     CBP($V)       COTOTP
195100970211     C                   Z-ADD     CPA($V)       $CDA
195200940428     C                   EXSR      PERCOL
195300000000      *
195400000000      * ?  P  E  S  O  ?
195500000000     C                   Z-ADD     PFI($V)       PEF35P
195600970211     C                   Z-ADD     PBP($V)       PEO35P
195700970212     C     PEF35P        ADD       PEO35P        PETOTP
195800970211     C                   Z-ADD     PPA($V)       $PDA
195900940428     C                   EXSR      PERPES
196000000000      *
196100000000      * ?  V  O  L  U  M E  ?
196200970211     C                   Z-ADD     VBP($V)       VOTOTP
196300970211     C                   Z-ADD     VPA($V)       $VDA
196400940428     C                   EXSR      PERVOL
196500000000      *
196600000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
196700970305     C                   Z-ADD     SPA($V)       $SDA
196800970305     C                   Z-ADD     FAF($V)       SPFAFF
196900940502     C                   EXSR      PERSPE
197000970305     C                   Z-ADD     SBP($V)       SPTOTP
197100970305     C                   Z-ADD     SCM($V)       SPCONP
197200970211     C     SBP($V)       ADD       SBR($V)       SPTOTA
197300970211     C     SPTOTA        SUB       SCM($V)       SPSPAP
197400000331      *
197500000331      * ?tot.volume a magazz?
197600000331     C                   Z-ADD     VBR($V)       VOTOTM
197700000000      *
197800000000     C                   EXSR      SCRIVI
197900000000      *
198000000000     C                   ENDDO
198100000000      *
198200950118     C                   WRITE     LSC2T2
198300940506     C                   ENDIF
198400940328      *
198500000417     C                   ENDSR
198600940428      **************************************************************************
198700940428      * Calcolo percentuale sui colli
198800940428      **************************************************************************
198900940428     C     PERCOL        BEGSR
199000940502     C                   Z-ADD     *ZERO         COSDPP
199100940428     C*
199200940502     C* PERCENTUALE SUL DA PARTIRE
199300970213     C  N02$CDA          IFNE      *ZERO
199400970210     C     COTOTP        DIV       $CDA          $COMOD
199500940502     C     $COMOD        MULT(H)   100           COSDPP
199600940502     C                   ENDIF
199700940428     C                   ENDSR
199800940428      **************************************************************************
199900940428      * Calcolo percentuale sul peso
200000940428      **************************************************************************
200100940428     C     PERPES        BEGSR
200200940502     C                   CLEAR                   PESDPP
200300940428     C*
200400940502     C* PERCENTUALE SUL DA PARTIRE
200500970213     C  N02$PDA          IFNE      *ZERO
200600970210     C     PETOTP        DIV       $PDA          $COMOD
200700940502     C     $COMOD        MULT(H)   100           PESDPP
200800940502     C                   ENDIF
200900940428     C                   ENDSR
201000940428      **************************************************************************
201100940428      * Calcolo percentuale sul volume
201200940428      **************************************************************************
201300940428     C     PERVOL        BEGSR
201400940502     C                   Z-ADD     *ZERO         VOSDPP
201500940502     C*
201600940502     C* PERCENTUALE SUL DA PARTIRE
201700970213     C  N02$VDA          IFNE      *ZERO
201800970210     C     VOTOTP        DIV       $VDA          $COMOD
201900940502     C     $COMOD        MULT(H)   100           VOSDPP
202000940502     C                   ENDIF
202100940428     C                   ENDSR
202200940428      **************************************************************************
202300940428      * Calcolo percentuale sulle spedizioni
202400940428      **************************************************************************
202500940428     C     PERSPE        BEGSR
202600940502     C                   Z-ADD     *ZERO         SPSDPP
202700940428     C*
202800940502     C* PERCENTUALE SUL DA PARTIRE
202900970213     C  N02$SDA          IFNE      *ZERO
203000970305     C     SPFAFF        DIV       $SDA          $COMOD
203100940502     C     $COMOD        MULT(H)   100           SPSDPP
203200940502     C                   ENDIF
203300940428     C                   ENDSR
203400000000      **************************************************************************
203500000000      * Azzeramento dei totali di filiale per nuovi conti
203600000000      **************************************************************************
203700000000     C     AZZFIL        BEGSR
203800000412     C                   CLEAR                   VOLUME
203900000000      *
204000000000      * COLLI
204100970211     C                   ADD       CPA(1)        C3A(1)
204200970211     C                   ADD       CPA(2)        C3A(2)
204300970211     C                   ADD       CPA(3)        C3A(3)
204400970211     C                   CLEAR                   CPA
204500970211     C                   CLEAR                   CBP
204600970211     C                   CLEAR                   CBR
204700970211      * PESI
204800970211     C                   ADD       PPA(1)        P3A(1)
204900970211     C                   ADD       PPA(2)        P3A(2)
205000970211     C                   ADD       PPA(3)        P3A(3)
205100970211     C                   CLEAR                   PFI
205200970211     C                   CLEAR                   PPA
205300970211     C                   CLEAR                   PBP
205400970211     C                   CLEAR                   PBR
205500970211     C                   CLEAR                   PFN
205600000000      * VOLUMI
205700970211     C                   ADD       VPA(1)        V3A(1)
205800970211     C                   ADD       VPA(2)        V3A(2)
205900970211     C                   ADD       VPA(3)        V3A(3)
206000970211     C                   CLEAR                   VPA
206100970211     C                   CLEAR                   VBP
206200970211     C                   CLEAR                   VBR
206300970211      * SPEDIZIONI
206400970211     C                   ADD       SPA(1)        S3A(1)
206500970211     C                   ADD       SPA(2)        S3A(2)
206600970211     C                   ADD       SPA(3)        S3A(3)
206700970211     C                   CLEAR                   SPA
206800970211     C                   CLEAR                   SBP
206900970211     C                   CLEAR                   SBR
207000970211     C                   CLEAR                   SCM
207100970305     C                   CLEAR                   FAF
207200000000      *
207300000000     C                   ENDSR
207400000000      **************************************************************************
207500000000      * Azzeramento dei totali di c/servizio per nuova area
207600000000      **************************************************************************
207700000000     C     AZZSER        BEGSR
207800000000      *
207900000000      * COLLI
208000940506     C                   ADD       C1A(1)        C3A(1)
208100970211     C                   ADD       C1A(2)        C3A(2)
208200970211     C                   ADD       C1A(3)        C3A(3)
208300000000     C                   CLEAR                   C1A
208400000000     C                   CLEAR                   C1P
208500000000     C                   CLEAR                   C1R
208600000000      * PESI
208700940506     C                   ADD       P1A(1)        P3A(1)
208800970211     C                   ADD       P1A(2)        P3A(2)
208900970211     C                   ADD       P1A(3)        P3A(3)
209000970211     C                   CLEAR                   P1I
209100970211     C                   CLEAR                   P1N
209200970211     C                   CLEAR                   P1P
209300970211     C                   CLEAR                   P1R
209400970211     C                   CLEAR                   P1A
209500970211      * VOLUMI
209600970211     C                   ADD       V1A(1)        V3A(1)
209700970211     C                   ADD       V1A(2)        V3A(2)
209800970211     C                   ADD       V1A(3)        V3A(3)
209900970211     C                   CLEAR                   V1A
210000970211     C                   CLEAR                   V1P
210100970211     C                   CLEAR                   V1R
210200000000      * SPEDIZIONI
210300940506     C                   ADD       S1A(1)        S3A(1)
210400970211     C                   ADD       S1A(2)        S3A(2)
210500970211     C                   ADD       S1A(3)        S3A(3)
210600970211     C                   CLEAR                   S1A
210700970211     C                   CLEAR                   S1P
210800970211     C                   CLEAR                   S1R
210900970211     C                   CLEAR                   S1M
211000970305     C                   CLEAR                   F1F
211100000000      *
211200000000     C                   ENDSR
211300000000      **************************************************************************
211400000000      * Azzeramento dei totali di c/servizio per nuova filiale
211500000000      **************************************************************************
211600000000     C     AZZSE4        BEGSR
211700000000      *
211800000000      * COLLI
211900970210     C                   CLEAR                   C4A
212000970210     C                   CLEAR                   C4P
212100970210     C                   CLEAR                   C4R
212200000000      * PESI
212300970210     C                   CLEAR                   P4A
212400970210     C                   CLEAR                   P4I
212500970210     C                   CLEAR                   P4P
212600970210     C                   CLEAR                   P4R
212700970210     C                   CLEAR                   P4N
212800000000      * VOLUMI
212900970210     C                   CLEAR                   V4A
213000970210     C                   CLEAR                   V4P
213100970210     C                   CLEAR                   V4R
213200000000      * SPEDIZIONI
213300970210     C                   CLEAR                   S4A
213400970210     C                   CLEAR                   S4P
213500970210     C                   CLEAR                   S4R
213600970210     C                   CLEAR                   S4M
213700970305     C                   CLEAR                   F4F
213800000000      *
213900000000     C                   ENDSR
214000000000      **************************************************************************
214100000000      * Azzeramento dei totali di transito per nuovi totali
214200000000      **************************************************************************
214300000000     C     AZZTRA        BEGSR
214400940506     C* SOMMO NEI TOTALI
214500940506     C                   EXSR      ADDTRA
214600000000      * COLLI
214700940428     C                   CLEAR                   C2P
214800000000      * PESI
214900000000     C                   CLEAR                   P2I
215000940428     C                   CLEAR                   P2P
215100000000      * VOLUMI
215200940428     C                   CLEAR                   V2P
215300000000      * SPEDIZIONI
215400940428     C                   CLEAR                   S2P
215500000000     C                   CLEAR                   S2M
215600000000      *
215700000000     C                   ENDSR
215800000000      **************************************************************************
215900000000      * Stampo i totali per c/servizio
216000000000      **************************************************************************
216100000000     C     TOTSER        BEGSR
216200000000      *
216300000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
216400000000      *
216500970211     C                   SETON                                        4045
216600000000     C                   SETOFF                                       41
216700000000     C                   Z-ADD     *ZERO         FILIAP
216800000000     C                   MOVE      *BLANKS       ORGDEP
216900000000     C     'C/SERVIZ'    CAT       'I':0         ORGDEP
217000000000      *
217100970211      * ?  C  O   L   L  I  ?
217200970211     C                   XFOOT     C1P           COTOTP
217300970211     C                   XFOOT     C1A           $CDA
217400970211     C                   EXSR      PERCOL
217500970211      *
217600970211      * ?  P  E  S  O  ?
217700970211     C                   XFOOT     P1I           PEF35P
217800970211     C                   XFOOT     P1P           PEO35P
217900970211     C     PEF35P        ADD       PEO35P        PETOTP
218000970211     C                   XFOOT     P1A           $PDA
218100940429     C                   EXSR      PERPES
218200000000      *
218300000000      * ?  V  O  L  U  M E  ?
218400970211     C                   XFOOT     V1P           VOTOTP
218500970211     C                   XFOOT     V1A           $VDA
218600940429     C                   EXSR      PERVOL
218700000000      *
218800000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
218900970211     C                   XFOOT     S1A           $SDA
219000970305     C                   XFOOT     F1F           SPFAFF
219100970211     C                   EXSR      PERSPE
219200970305     C                   XFOOT     S1P           SPTOTP
219300970305     C                   XFOOT     S1M           SPCONP
219400970211     C                   XFOOT     S1R           $COMO1
219500970211     C     SPTOTP        ADD       $COMO1        SPTOTA
219600970211     C     SPTOTA        SUB       SPCONP        SPSPAP
219700000331      *
219800000331      * ?tot.volume a magazz?
219900000331     C                   XFOOT     V1R           VOTOTM
220000970211      *
220100970211     C* STAMPO SOLO SE HO DEI DATI
220200000331     C     VOTOTM        IFGT      *ZEROS
220300970211     C*
220400970211     C                   EXSR      SCRIVI
220500000000      *
220600970211      * ------------------ ESPR./DIST./CORR. --------------------------
220700000000      *
220800970211     C                   SETOFF                                       40
220900000000     C                   DO        3             $V
221000940323     C                   MOVE      ENS($V)       TIPOP
221100000000      *
221200000000      * ?  C  O   L   L  I  ?
221300970211     C                   Z-ADD     C1P($V)       COTOTP
221400970211     C                   Z-ADD     C1A($V)       $CDA
221500940429     C                   EXSR      PERCOL
221600000000      *
221700000000      * ?  P  E  S  O  ?
221800000000     C                   Z-ADD     P1I($V)       PEF35P
221900970211     C                   Z-ADD     P1P($V)       PEO35P
222000970211     C     PEF35P        ADD       PEO35P        PETOTP
222100970211     C                   Z-ADD     P1A($V)       $PDA
222200940429     C                   EXSR      PERPES
222300000000      *
222400000000      * ?  V  O  L  U  M E  ?
222500970211     C                   Z-ADD     V1P($V)       VOTOTP
222600970211     C                   Z-ADD     V1A($V)       $VDA
222700940429     C                   EXSR      PERVOL
222800000000      *
222900000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
223000970211     C                   Z-ADD     S1A($V)       $SDA
223100970305     C                   Z-ADD     F1F($V)       SPFAFF
223200940502     C                   EXSR      PERSPE
223300970305     C                   Z-ADD     S1P($V)       SPTOTP
223400970305     C                   Z-ADD     S1M($V)       SPCONP
223500970211     C     S1P($V)       ADD       S1R($V)       SPTOTA
223600970211     C     SPTOTA        SUB       S1M($V)       SPSPAP
223700000331      *
223800000331      * ?tot volume a magazz?
223900000331     C                   Z-ADD     V1R($V)       VOTOTM
224000000000      *
224100000000     C                   EXSR      SCRIVI
224200000000      *
224300000000     C                   ENDDO
224400940504     C                   ENDIF
224500940429     C                   EXSR      AZZSER
224600000000      *
224700000000     C                   ENDSR
224800000000      **************************************************************************
224900000000      * Stampo i totali per transito
225000000000      **************************************************************************
225100000000     C     TOTTRA        BEGSR
225200000000      *
225300000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
225400000000      *
225500940506     C                   SETON                                        400203
225600970211     C                   SETOFF                                       45
225700000000     C                   MOVE      *BLANKS       ORGDEP
225800940502     C* stampa per linea: totale transiti
225900940502     C     *IN43         IFEQ      *ON
226000940502     C                   Z-ADD     *ZERO         FILIAP
226100940502     C                   MOVEL     'TRANSITI'    ORGDEP
226200940502     C                   SETOFF                                       41
226300940502     C                   ELSE
226400940502     c* stampa per terminal in transito
226500940502     C                   SETON                                        41
226600940502      * Decodifico il terminal di arrivo
226700940502     C                   Z-ADD     $FILPR        FILIAP
226800940502     C     $FILPR        CHAIN     AZORG01L                           99
226900940502     C  N99ORGFVA        COMP      *BLANK                             9999
227000940503     C   99              CLEAR                   ORGDEP
227100940502     C  N99              MOVEL     ORGDES        ORGDEP
227200940502     C                   ENDIF
227300000000      *
227400000000      * ?  C  O   L   L  I  ?
227500940502     C                   XFOOT     C2P           COTOTP
227600970211     C*
227700970211      * SOLO SE HO TROVATO ALEMNO UN COLLO A TERRA STAMPO
227800970211     C     COTOTP        IFGT      0
227900940504      *
228000000000      *
228100000000      * ?  P  E  S  O  ?
228200970211     C                   XFOOT     P2I           PEF35P
228300970211     C                   XFOOT     P2P           PEO35P
228400970211     C     PEO35P        ADD       PEF35P        PETOTP
228500000000      *
228600000000      * ?  V  O  L  U  M E  ?
228700940428     C                   XFOOT     V2P           VOTOTP
228800000000      *
228900000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
229000940428     C                   XFOOT     S2P           SPTOTP
229100970211     C                   XFOOT     S2P           SPTOTA
229200970211     C                   XFOOT     S2M           SPCONP
229300970211     C     SPTOTA        SUB       SPCONP        SPSPAP
229400000000      *
229500000000     C                   EXSR      SCRIVI
229600940509     C                   ADD       1             $RIGHE
229700000000      *
229800970211      * ------------------ ESPR./DIST./CORR. --------------------------
229900000000      *
230000940502     C                   SETOFF                                       4045
230100000000     C                   DO        3             $V
230200940502     C* PER TERMINAL SCRIVO CHE E' RECORD DI TRANSITO
230300940429     C  N43$V            IFEQ      1
230400940506     C                   MOVEL     'IN TRANS'    DESCRP
230500940429     C                   ELSE
230600940429     C                   CLEAR                   DESCRP
230700940429     C                   ENDIF
230800940429     C*
230900940323     C                   MOVE      ENS($V)       TIPOP
231000000000      *
231100000000      * ?  C  O   L   L  I  ?
231200940428     C                   Z-ADD     C2P($V)       COTOTP
231300000000      *
231400000000      * ?  P  E  S  O  ?
231500970211     C                   Z-ADD     P2I($V)       PEF35P
231600970211     C                   Z-ADD     P2P($V)       PEO35P
231700970211     C     PEO35P        ADD       PEF35P        PETOTP
231800000000      *
231900000000      * ?  V  O  L  U  M E  ?
232000940428     C                   Z-ADD     V2P($V)       VOTOTP
232100000000      *
232200000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
232300940428     C                   Z-ADD     S2P($V)       SPTOTP
232400970211     C                   Z-ADD     S2P($V)       SPTOTA
232500970211     C                   Z-ADD     S2M($V)       SPCONP
232600970211     C     SPTOTA        SUB       S2M($V)       SPSPAP
232700000000      *
232800000000     C                   EXSR      SCRIVI
232900000000      *
233000000000     C                   ENDDO
233100940428     C*
233200940428     C                   EXSR      AZZTRA
233300940502      *
233400950118     C  N38              WRITE     LSC2T2
233500940506     C                   ENDIF
233600940506     C                   SETOFF                                       0302
233700000000      *
233800000000     C                   ENDSR
233900000000      **************************************************************************
234000970211      * Scrittura del record sul file di trasmissione FNSAT00F
234100000000      **************************************************************************
234200000000     C     WRISAT        BEGSR
234300000000      *
234400970207     C                   CLEAR                   FNSAT000
234500000000     C                   Z-ADD     SIMFEL        SATFLE
234600000000     C                   Z-ADD     $FILPR        SATLNP
234700000000     C                   Z-ADD     *ZERO         SATTFA
234800950210     C                   Z-ADD     DS1ADA        SATDRE
234900000000     C                   MOVE      *BLANK        SATTRE
235000000000      *
235100000000      * COLLI: da partire
235200970210     C                   Z-ADD     CPA(1)        SATCPE
235300970210     C                   Z-ADD     CPA(2)        SATCPD
235400970210     C                   Z-ADD     CPA(3)        SATCPC
235500970207      * COLLI: a terra da partire
235600970207     C                   Z-ADD     CBP(1)        SATCDE
235700970207     C                   Z-ADD     CBP(2)        SATCDD
235800970207     C                   Z-ADD     CBP(3)        SATCDC
235900970207      * COLLI: a terra non da partire
236000970207     C                   Z-ADD     CBR(1)        SATCNE
236100970207     C                   Z-ADD     CBR(2)        SATCND
236200970207     C                   Z-ADD     CBR(3)        SATCNC
236300970207      * PESO: da partire
236400970210     C                   Z-ADD     PPA(1)        SATPPE
236500970210     C                   Z-ADD     PPA(2)        SATPPD
236600970210     C                   Z-ADD     PPA(3)        SATPPC
236700970207      * PESO: a terra da partire fino a 35 quintali
236800970207     C                   Z-ADD     PFI(1)        SATFDE
236900970207     C                   Z-ADD     PFI(2)        SATFDD
237000970207     C                   Z-ADD     PFI(3)        SATFDC
237100970207      * PESO: a terra non da partire fino a 35 q.
237200970207     C                   Z-ADD     PFN(1)        SATFNE
237300970207     C                   Z-ADD     PFN(2)        SATFND
237400970207     C                   Z-ADD     PFN(3)        SATFNC
237500970207     C* PESO: a terra da partire oltre 35 quintali
237600970207     C                   Z-ADD     PBP(1)        SATODE
237700970207     C                   Z-ADD     PBP(2)        SATODD
237800970207     C                   Z-ADD     PBP(3)        SATODC
237900970207      * PESO: a terra non da partire oltre 35 q.
238000970207     C                   Z-ADD     PBR(1)        SATONE
238100970207     C                   Z-ADD     PBR(2)        SATOND
238200970207     C                   Z-ADD     PBR(3)        SATONC
238300000000      * VOLUME: da partire
238400970210     C                   Z-ADD     VPA(1)        SATVPE
238500970210     C                   Z-ADD     VPA(2)        SATVPD
238600970210     C                   Z-ADD     VPA(3)        SATVPC
238700970207      * VOLUME a terra da partire
238800970207     C                   Z-ADD     VBP(1)        SATVDE
238900970207     C                   Z-ADD     VBP(2)        SATVDD
239000970207     C                   Z-ADD     VBP(3)        SATVDC
239100000331      * VOLUME rimasto a magazzino
239200000331      * (totale dei colli a terra da partire, da non partire e colli
239300000331      * di bolle in sospensione)
239400970207     C                   Z-ADD     VBR(1)        SATVNE
239500970207     C                   Z-ADD     VBR(2)        SATVND
239600970207     C                   Z-ADD     VBR(3)        SATVNC
239700940429      * SPEDIZIONI: da partire
239800970210     C                   Z-ADD     SPA(1)        SATSPE
239900970210     C                   Z-ADD     SPA(2)        SATSPD
240000970210     C                   Z-ADD     SPA(3)        SATSPC
240100970207      * SPEDIZIONI a terra da partire
240200970207     C                   Z-ADD     SBP(1)        SATSDE
240300970207     C                   Z-ADD     SBP(2)        SATSDD
240400970207     C                   Z-ADD     SBP(3)        SATSDC
240500970207      * SPEDIZIONI a terra non da partire
240600970207     C                   Z-ADD     SBR(1)        SATSNE
240700970207     C                   Z-ADD     SBR(2)        SATSND
240800970207     C                   Z-ADD     SBR(3)        SATSNC
240900970305      * SPEDIZIONI a terra da partire fuori affidabilità
241000970305     C                   Z-ADD     FAF(1)        SATSFE
241100970305     C                   Z-ADD     FAF(2)        SATSFD
241200970305     C                   Z-ADD     FAF(3)        SATSFC
241300000000      * SPEDIZIONI: complete
241400000000     C                   Z-ADD     SCM(1)        SATSCE
241500970207     C                   Z-ADD     SCM(2)        SATSCD
241600970207     C                   Z-ADD     SCM(3)        SATSCC
241700000000      *
241800970318     C* ASTERISCO LE SEGUENTI IF PERCHE' IL RECORD LO DEVO SEMPRE
241900970318     C* SCRIVERE PER EVITARE CHE NELL'ELABORAZIONE DEL REPORT DI
242000970318     C* SEDE, SE PER UN P.O. IL DA PARTIRE è = 0, VENGA ERRONEAMENTE
242100970318     C* SEGNALATO "DATI NON PERVENUTI".
242200970318     C*          SATSDE    IFGT *ZERO
242300970318     C*          SATSDD    ORGT *ZERO
242400970318     C*          SATSDC    ORGT *ZERO
242500970318     C*          SATSNE    ORGT *ZERO
242600970318     C*          SATSND    ORGT *ZERO
242700970318     C*          SATSNC    ORGT *ZERO
242800970318     C*          SATSPE    ORGT *ZEROS
242900970318     C*          SATSPD    ORGT *ZEROS
243000970318     C*          SATSPC    ORGT *ZEROS
243100940323      *
243200940428     C                   MOVE      $FTR          SATFTR
243300970207     C                   MOVE      $DTR          SATDTR
243400940323      *
243500970207     C                   WRITE     FNSAT000
243600970318     C*
243700970318     C* LE IF SOPRA ASTERISCATE LE MANTENGO PERò PER IMPOSTARE $PRIM4
243800970318     C*
243900970318     C     SATSDE        IFGT      *ZERO
244000970318     C     SATSDD        ORGT      *ZERO
244100970318     C     SATSDC        ORGT      *ZERO
244200970318     C     SATSNE        ORGT      *ZERO
244300970318     C     SATSND        ORGT      *ZERO
244400970318     C     SATSNC        ORGT      *ZERO
244500970318     C     SATSPE        ORGT      *ZEROS
244600970318     C     SATSPD        ORGT      *ZEROS
244700970318     C     SATSPC        ORGT      *ZEROS
244800000405     C     SATVNE        ORGT      *ZEROS
244900000405     C     SATVND        ORGT      *ZEROS
245000000405     C     SATVNC        ORGT      *ZEROS
245100940429     C* PER LA STAMPA DEI TOTALI DI AREA
245200940429     C                   Z-ADD     1             $PRIM4
245300970318     C                   ENDIF
245400000000      *
245500000000     C                   ENDSR
245600000000      **************************************************************************
245700970211      * Scrittura del record c/servizio sul file di trasmissione FNSAT00F
245800000000      **************************************************************************
245900000000     C     WR1SAT        BEGSR
246000000000      *
246100970207     C                   CLEAR                   FNSAT000
246200000000     C                   Z-ADD     SIMFEL        SATFLE
246300000000     C                   Z-ADD     $FILPR        SATLNP
246400000000     C                   Z-ADD     *ZERO         SATTFA
246500950210     C                   Z-ADD     DS1ADA        SATDRE
246600000000     C                   MOVE      'C'           SATTRE
246700000000      *
246800000000      * COLLI: da partire
246900970210     C                   Z-ADD     C4A(1)        SATCPE
247000970210     C                   Z-ADD     C4A(2)        SATCPD
247100970210     C                   Z-ADD     C4A(3)        SATCPC
247200970207      * COLLI: a terra da partire
247300970207     C                   Z-ADD     C4P(1)        SATCDE
247400970207     C                   Z-ADD     C4P(2)        SATCDD
247500970207     C                   Z-ADD     C4P(3)        SATCDC
247600970207      * COLLI: a terra non da partire
247700970207     C                   Z-ADD     C4R(1)        SATCNE
247800970207     C                   Z-ADD     C4R(2)        SATCND
247900970207     C                   Z-ADD     C4R(3)        SATCNC
248000970207      * PESO: da partire
248100970210     C                   Z-ADD     P4A(1)        SATPPE
248200970210     C                   Z-ADD     P4A(2)        SATPPD
248300970210     C                   Z-ADD     P4A(3)        SATPPC
248400970207      * PESO: a terra da partire fino a 35 quintali
248500970207     C                   Z-ADD     P4I(1)        SATFDE
248600970207     C                   Z-ADD     P4I(2)        SATFDD
248700970207     C                   Z-ADD     P4I(3)        SATFDC
248800970207      * PESO: a terra non da partire fino a 35 quintali
248900970207     C                   Z-ADD     P4N(1)        SATFNE
249000970207     C                   Z-ADD     P4N(2)        SATFND
249100970207     C                   Z-ADD     P4N(3)        SATFNC
249200970207      * PESO: a terra da partire oltre 35 quintali
249300970207     C                   Z-ADD     P4P(1)        SATODE
249400970207     C                   Z-ADD     P4P(2)        SATODD
249500970207     C                   Z-ADD     P4P(3)        SATODC
249600970207      * PESO: a terra non da partire oltre 35 quintali
249700970207     C                   Z-ADD     P4R(1)        SATONE
249800970207     C                   Z-ADD     P4R(2)        SATOND
249900970207     C                   Z-ADD     P4R(3)        SATONC
250000000000      * VOLUME: da partire
250100970210     C                   Z-ADD     V4A(1)        SATVPE
250200970210     C                   Z-ADD     V4A(2)        SATVPD
250300970210     C                   Z-ADD     V4A(3)        SATVPC
250400970207      * VOLUME: a terra da partire
250500970207     C                   Z-ADD     V4P(1)        SATVDE
250600970207     C                   Z-ADD     V4P(2)        SATVDD
250700970207     C                   Z-ADD     V4P(3)        SATVDC
250800000331      * VOLUME: totale rimasto a magazzino
250900000331      * (a terra da partire, da non partire, in sospensione)
251000970207     C                   Z-ADD     V4R(1)        SATVNE
251100970207     C                   Z-ADD     V4R(2)        SATVND
251200970207     C                   Z-ADD     V4R(3)        SATVNC
251300970207      * SPEDIZIONI: da partire
251400970210     C                   Z-ADD     S4A(1)        SATSPE
251500970210     C                   Z-ADD     S4A(2)        SATSPD
251600970210     C                   Z-ADD     S4A(3)        SATSPC
251700970207      * SPEDIZIONI: a terra da partire
251800970207     C                   Z-ADD     S4P(1)        SATSDE
251900970207     C                   Z-ADD     S4P(2)        SATSDD
252000970207     C                   Z-ADD     S4P(3)        SATSDC
252100970207      * SPEDIZIONI: a terra non da partire
252200970207     C                   Z-ADD     S4R(1)        SATSNE
252300970207     C                   Z-ADD     S4R(2)        SATSND
252400970207     C                   Z-ADD     S4R(3)        SATSNC
252500970305      * SPEDIZIONI: a terra da partire fuori affidabilità
252600970305     C                   Z-ADD     F4F(1)        SATSFE
252700970305     C                   Z-ADD     F4F(2)        SATSFD
252800970305     C                   Z-ADD     F4F(3)        SATSFC
252900000000      * SPEDIZIONI: complete
253000000000     C                   Z-ADD     S4M(1)        SATSCE
253100970207     C                   Z-ADD     S4M(2)        SATSCD
253200970207     C                   Z-ADD     S4M(3)        SATSCC
253300000000      *
253400970318     C     SATSDE        IFGT      *ZERO
253500970318     C     SATSDD        ORGT      *ZERO
253600970318     C     SATSDC        ORGT      *ZERO
253700970318     C     SATSNE        ORGT      *ZERO
253800970318     C     SATSND        ORGT      *ZERO
253900970318     C     SATSNC        ORGT      *ZERO
254000970318     C     SATSPE        ORGT      *ZERO
254100970318     C     SATSPD        ORGT      *ZERO
254200970318     C     SATSPC        ORGT      *ZERO
254300000405     C     SATVNE        ORGT      *ZEROS
254400000405     C     SATVND        ORGT      *ZEROS
254500000405     C     SATVNC        ORGT      *ZEROS
254600940323      *
254700940428     C                   MOVE      $FTR          SATFTR
254800970207     C                   MOVE      $DTR          SATDTR
254900940323      *
255000970207     C                   WRITE     FNSAT000
255100940429     C* PER LA STAMPA DEI C/S
255200940429     C                   Z-ADD     1             $PRIM2
255300940429     C* PER LA STAMPA DEI TOTALI DI AREA
255400940429     C                   Z-ADD     1             $PRIM4
255500000000     C                   ENDIF
255600000000      *
255700000000      * Aggiorno le schiere per i totali c/servizio per area
255800000000     C                   DO        3             $U                1 0
255900970210     C* A TERRA
256000000000     C                   ADD       C4P($U)       C1P($U)
256100000000     C                   ADD       C4R($U)       C1R($U)
256200000000     C                   ADD       P4I($U)       P1I($U)
256300970207     C                   ADD       P4N($U)       P1N($U)
256400000000     C                   ADD       P4P($U)       P1P($U)
256500000000     C                   ADD       P4R($U)       P1R($U)
256600000000     C                   ADD       V4P($U)       V1P($U)
256700000331     C                   ADD       V4R($U)       V1R($U)
256800000000     C                   ADD       S4P($U)       S1P($U)
256900000000     C                   ADD       S4R($U)       S1R($U)
257000000000     C                   ADD       S4M($U)       S1M($U)
257100970305     C                   ADD       F4F($U)       F1F($U)
257200970210     C* DA PARTIRE
257300970210     C                   ADD       C4A($U)       C1A($U)
257400970210     C                   ADD       P4A($U)       P1A($U)
257500970210     C                   ADD       V4A($U)       V1A($U)
257600970210     C                   ADD       S4A($U)       S1A($U)
257700000000     C                   ENDDO
257800000000      *
257900000000     C                   EXSR      AZZSE4
258000000000      *
258100000000     C                   ENDSR
258200000000      **************************************************************************
258300970211      * Scrittura del record transito sul file di trasmissione FNSAT00F
258400000000      **************************************************************************
258500000000     C     WR2SAT        BEGSR
258600000000      *
258700970211     C                   CLEAR                   FNSAT000
258800000000     C                   Z-ADD     SIMFEL        SATFLE
258900940502     C                   Z-ADD     SIMFEL        SATLNP
259000000000     C                   Z-ADD     *ZERO         SATTFA
259100950210     C                   Z-ADD     DS1ADA        SATDRE
259200000000     C                   MOVE      'T'           SATTRE
259300000000      *
259400000000      * COLLI: da partire
259500970211     C                   Z-ADD     0             SATCPE
259600970211     C                   Z-ADD     0             SATCPD
259700970211     C                   Z-ADD     0             SATCPC
259800970211      * COLLI:
259900970211     C                   Z-ADD     C2P(1)        SATCDE
260000970211     C                   Z-ADD     C2P(2)        SATCDD
260100970211     C                   Z-ADD     C2P(3)        SATCDC
260200970211      * PESO: da partire
260300970211     C                   Z-ADD     0             SATPPE
260400970211     C                   Z-ADD     0             SATPPD
260500970211     C                   Z-ADD     0             SATPPC
260600000000      * PESO: fino a 35 quintali
260700970211     C                   Z-ADD     P2I(1)        SATFDE
260800970211     C                   Z-ADD     P2I(2)        SATFDD
260900970211     C                   Z-ADD     P2I(3)        SATFDC
261000940428      * PESO: (non esiste il bollettato nel periodo o nel precedente)
261100970211     C                   Z-ADD     P2P(1)        SATODE
261200970211     C                   Z-ADD     P2P(2)        SATODD
261300970211     C                   Z-ADD     P2P(3)        SATODC
261400000000      * VOLUME: da partire
261500970211     C                   Z-ADD     0             SATVPE
261600970211     C                   Z-ADD     0             SATVPD
261700970211     C                   Z-ADD     0             SATVPC
261800970211      * VOLUME
261900970211     C                   Z-ADD     V2P(1)        SATVDE
262000970211     C                   Z-ADD     V2P(2)        SATVDD
262100970211     C                   Z-ADD     V2P(3)        SATVDC
262200000000      * SPEDIZIONI: da partire
262300970211     C                   Z-ADD     0             SATSPE
262400970211     C                   Z-ADD     0             SATSPD
262500970211     C                   Z-ADD     0             SATSPC
262600970211      * SPEDIZIONI
262700970211     C                   Z-ADD     S2P(1)        SATSDE
262800970211     C                   Z-ADD     S2P(2)        SATSDD
262900970211     C                   Z-ADD     S2P(3)        SATSDC
263000000000      * SPEDIZIONI: complete
263100940509     C                   Z-ADD     S2M(1)        SATSCE
263200970211     C                   Z-ADD     S2M(2)        SATSCD
263300970211     C                   Z-ADD     S2M(3)        SATSCC
263400000000      *
263500970212     C     SATSDE        IFGT      *ZERO
263600970212     C     SATSDD        ORGT      *ZERO
263700970212     C     SATSDC        ORGT      *ZERO
263800940428     C                   MOVE      $FTR          SATFTR
263900970207     C                   MOVE      $DTR          SATDTR
264000940323      *
264100970211     C                   WRITE     FNSAT000
264200940429     C* PER LA STAMPA DEI TOTALI DI AREA
264300940429     C                   Z-ADD     1             $PRIM4
264400940505     C                   Z-ADD     1             $PRIM3
264500000000     C                   ENDIF
264600000000      *
264700000000     C                   ENDSR
264800940428      **************************************************************************
264900940428      * Stampo i totali generali
265000940428      **************************************************************************
265100940428     C     TOTGEN        BEGSR
265200940428      *
265300940428      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
265400940428      *
265500940506      * stampo i totali generali finali solo se ho stampato i transiti
265600950628    1C     *IN02         IFEQ      *ON
265700940506     C     $PRIM3        ANDEQ     1
265800940506     C* I TOTALI DI AREA SEMPRE
265900940506     C     *IN02         OREQ      *OFF
266000940506     C*
266100940428     C                   SETON                                        40
266200940428     C                   SETOFF                                       41
266300970211     C  N02              SETON                                        45
266400970211     C   02              SETOFF                                       45
266500940428     C                   Z-ADD     *ZERO         FILIAP
266600940428     C                   MOVE      *BLANKS       ORGDEP
266700940428     C   02'TOT.GENE'    CAT       'RALE':0      ORGDEP
266800940428     C  N02              MOVEL     'TOTALE'      ORGDEP
266900940428     C*
267000950118     C                   WRITE     LSC2T3
267100940428      *
267200940428      * ?  C  O   L   L  I  ?
267300970211     C                   XFOOT     C3P           COTOTP
267400970211     C                   XFOOT     C3A           $CDA
267500940428     C                   EXSR      PERCOL
267600940428      *
267700940428      * ?  P  E  S  O  ?
267800970211     C                   XFOOT     P3I           PEF35P
267900970211     C                   XFOOT     P3P           PEO35P
268000970211     C     PEF35P        ADD       PEO35P        PETOTP
268100970228     C                   XFOOT     P3A           $PDA
268200970211     C                   EXSR      PERPES
268300970211      *
268400970211      * ?  V  O  L  U  M E  ?
268500970211     C                   XFOOT     V3P           VOTOTP
268600970211     C                   XFOOT     V3A           $VDA
268700940428     C                   EXSR      PERVOL
268800940428      *
268900940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
269000970211     C                   XFOOT     S3A           $SDA
269100970305     C                   XFOOT     F3F           SPFAFF
269200940502     C                   EXSR      PERSPE
269300970305     C                   XFOOT     S3P           SPTOTP
269400970305     C                   XFOOT     S3M           SPCONP
269500970211     C                   XFOOT     S3R           $COMO1
269600970211     C     SPTOTP        ADD       $COMO1        SPTOTA
269700970211     C     SPTOTA        SUB       SPCONP        SPSPAP
269800000407     C*
269900000407      * ?VOLUME A MAGAZZINO ?
270000000407     C                   XFOOT     V3R           VOTOTM
270100940428      *
270200940428     C                   EXSR      SCRIVI
270300940509     C                   ADD       1             $RIGHE
270400940428      *
270500970211      * ------------------ ESPR./DIST./CORR. --------------------------
270600940428      *
270700970211     C                   SETOFF                                       40
270800950628    2C                   DO        3             $V
270900940428     C                   MOVE      ENS($V)       TIPOP
271000940428      *
271100940428      * ?  C  O   L   L  I  ?
271200970211     C                   Z-ADD     C3P($V)       COTOTP
271300970211     C                   Z-ADD     C3A($V)       $CDA
271400940428     C                   EXSR      PERCOL
271500940428      *
271600940428      * ?  P  E  S  O  ?
271700940428     C                   Z-ADD     P3I($V)       PEF35P
271800970211     C                   Z-ADD     P3P($V)       PEO35P
271900970211     C     PEF35P        ADD       PEO35P        PETOTP
272000970211     C                   Z-ADD     P3A($V)       $PDA
272100940428     C                   EXSR      PERPES
272200940428      *
272300940428      * ?  V  O  L  U  M E  ?
272400970211     C                   Z-ADD     V3P($V)       VOTOTP
272500970211     C                   Z-ADD     V3A($V)       $VDA
272600940428     C                   EXSR      PERVOL
272700940428      *
272800940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
272900970211     C                   Z-ADD     S3A($V)       $SDA
273000970305     C                   Z-ADD     F3F($V)       SPFAFF
273100940502     C                   EXSR      PERSPE
273200970305     C                   Z-ADD     S3P($V)       SPTOTP
273300970305     C                   Z-ADD     S3M($V)       SPCONP
273400970211     C     S3P($V)       ADD       S3R($V)       SPTOTA
273500970211     C     SPTOTA        SUB       S3M($V)       SPSPAP
273600000406      * ?VOLUME A MAGAZZINO ?
273700000406     C                   Z-ADD     V3R($V)       VOTOTM
273800940428      *
273900940428     C                   EXSR      SCRIVI
274000940428      *
274100950628    2C                   ENDDO
274200940428      *
274300950118     C  N02              WRITE     LSC2T2
274400950628    1C                   ENDIF
274500940429     C*
274600950118     C   02              WRITE     LSC2T3
274700940428     C*
274800940428     C   02              EXSR      AZZTOT
274900940428      *
275000940428     C                   ENDSR
275100000000      **************************************************************************
275200000000      * Elaborazione transiti
275300940505      * Non posso leggere per data uscita dal transito perche' spesso
275400940505      * capita che ci siano bolle con data entrata un giorno e data
275500940505      * uscita il giorno prima: per questo elaboro tutte le bolle
275600950210      * che hanno la data di entrata al transito > 0 e <=a DS1AAL
275700000000      **************************************************************************
275800000000     C     ELATRA        BEGSR
275900000000      *
276000940505     C                   Z-ADD     0             $PRIM1
276100940428     C                   MOVE      *BLANK        $BLTPR
276200940429     C                   MOVE      *BLANK        $CBLPR
276300940428     C                   MOVE      'T'           $FLAG
276400000000      *
276500000000      * Leggo il file delle bolle in transito
276600940505     C                   Z-ADD     1             $DFV
276700020625     C     kbtt39        SETLL     FNBTT39L
276800020625     C     simfel        READe     FNBTT39L                               33
276900940428     C*
277000940428    2C     *IN33         DOWEQ     *OFF
277100950210     C     BTTDET        ANDLE     DS1AAL
277200000000      *
277300940505     C                   EXSR      ESCLU1
277400940428      *
277500940505    3C     $OK           IFEQ      'S'
277600940505     C                   Z-ADD     1             $PRIM1
277700000000      *
277800000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
277900000000      * poiche' lo dovro aggiornare
278000970211     C                   CLEAR                   FNSAT000
278100000000     C                   Z-ADD     SIMFEL        SATFLE
278200950210     C                   Z-ADD     DS1ADA        SATDRE
278300940502     C                   Z-ADD     SIMFEL        SATLNP
278400950118     C                   Z-ADD     BTPTFA        SATTFA
278500000000     C                   MOVE      'T'           SATTRE
278600970205     C     FNSA1K        CHAIN     FNSAT02L                           37
278700940428      *
278800021217      * se è la prima volta che leggo la bolla carico i dati: VOL e PKG
278900021217    4C     $BOLLA        IFEQ      '1'
279000021217     C                   EXSR      DATBOT
279100021217    4C                   ENDIF
279200021217      *
279300021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
279400030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
279500030213      * Peso medio
279600030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
279700030213if  1c***                if        Wsipes = 'S'
279800030213     c***                z-add     BTTpuc        $Peso
279900030213   x1c***                else
280000021220     c                   z-add     Wpesmed       $Peso
280100030213e   1c***                endif
280200021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
280300030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
280400030213      * Volume medio
280500030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
280600030213if  1c***                if        Wsivol = 'S'
280700030213     c***                z-add     BTTvuc        $Volum
280800030213   x1c***                else
280900021220     c                   z-add     Wvolmed       $Volum
281000030213e   1c***                endif
281100000000      *
281200970211      * Verifico il tipo spedizione ESPR./DIST./CORR.
281300950201     C                   Z-ADD     3             $T
281400950118    4C     BTPTSP        IFEQ      'E'
281500000000     C                   Z-ADD     1             $T
281600940428    4C                   ENDIF
281700950201    4C     BTPTSP        IFEQ      'D'
281800940323     C                   Z-ADD     2             $T
281900940428    4C                   ENDIF
282000000000      *
282100000000      * Ho due controlli da fare per sapere se il collo e' a terra:
282200940428      *    1)  BTTDUT = 0
282300940428      *    2)  BTTDUT > Data spedizione
282400000000     C                   SETOFF                                       36
282500940428     C     BTTDUT        IFEQ      0
282600950210     C     BTTDUT        ORGT      DS1AAL
282700000000     C                   SETON                                        36
282800000000     C                   ENDIF
282900940506      *
283000000000      * Sommo solo se il collo risulta a terra
283100940428    4C     *IN36         IFEQ      *ON
283200940506      *
283300940506      * ?  CALCOLI  SUI  COLLI  ?
283400940506      *
283500940509     C                   ADD       1             C2P($T)
283600940428     C                   ADD       1             C0P($T)
283700000000      *
283800000000      * ?  CALCOLI  SUL  PESO  ?
283900000000      *
284000000000      * Verifico il peso di 35 quintali
284100021217    5***C     BTPPKB        IFLE      3500
284200021217    5C     Wpeso         IFLE      3500
284300940509     C                   ADD       $PESO         P2I($T)
284400000000     C                   ADD       $PESO         P0I($T)
284500970211     C                   ELSE
284600970211      * Oltre 35 quintali:                                             (35ON)
284700970211     C                   ADD       $PESO         P2P($T)
284800970211     C                   ADD       $PESO         P0P($T)
284900940428    5C                   ENDIF
285000000000      *
285100000000      *
285200000000      * ?  CALCOLI  SUL  VOLUME  ?
285300000000      *
285400021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
285500021217     **5C     BTTVUC        IFEQ      *ZERO
285600021217     *** *
285700021217     ***C     BTPVLB        DIV(H)    BTPNCL        $VOLUM
285800021217     *** *
285900021217     **5C                   ELSE
286000021217     ***C                   Z-ADD     BTTVUC        $VOLUM
286100021217     **5C                   ENDIF
286200000000      *
286300940509     C                   ADD       $VOLUM        V2P($T)
286400940428     C                   ADD       $VOLUM        V0P($T)
286500000000      *
286600000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
286700000000      *
286800000000      * Visto che le spedizioni non riesco a calcolarle ora, mi limito a tenere
286900000000      * aggiornato il file di work con le bolle
287000950118     C                   Z-ADD     BTPLNP        WRKLNP
287100000000     C                   MOVE      *BLANK        WRKCSE
287200970409     C     FNWR2K        CHAIN     FNWRK00F                           99
287300940428    5C     *IN99         IFEQ      *OFF
287400000000     C                   ADD       1             WRKNC1
287500000000     C                   EXCEPT    AGGIO
287600940428   X5C                   ELSE
287700950118     C                   MOVEL     'T'           WTRANS
287800940503     C                   EXSR      WRIWRK
287900940428    5C                   ENDIF
288000940428      *
288100040921     C                   MOVE      $FTR          SATFTR
288200040921     C                   MOVE      $DTR          SATDTR
288300940428    5C     *IN37         IFEQ      *ON
288400040921     C***                MOVE      $FTR          SATFTR
288500040921     C***                MOVE      $DTR          SATDTR
288600970211     C                   WRITE     FNSAT000
288700940428   X5C                   ELSE
288800970211     C                   UPDATE    FNSAT000
288900940428    5C                   ENDIF
289000000000      *
289100940428    4C                   ENDIF
289200940428    3C                   ENDIF
289300000000      *
289400020625     C     simfel        READE     FNBTT39L                               33
289500940428    2C                   ENDDO
289600940506     C*
289700000000     C     $PRIM1        IFNE      *ZERO
289800940506     C                   EXSR      CALSPE
289900000000     C                   EXSR      WR2SAT
290000000000     C                   ENDIF
290100000000      *
290200000000     C                   ENDSR
290300940503      **************************************************************************
290400000411      * elimino i terminal che non hanno VOLUMI TOT A MAG>0
290500940503      **************************************************************************
290600940503     C     ELATER        BEGSR
290700950210     C                   Z-ADD     DS1FEL        $AREA
290800940503     C                   CLEAR                   $TFA
290900940503     C                   SETOFF                                       33
291000940503     C*
291100940503     C* ELABORO TERMINAL PER TERMINAL
291200940503     C                   EXSR      IMPTFA
291300940503     C*
291400940503    2C     *IN33         DOWEQ     *OFF
291500940503     C                   Z-ADD     *ZERO         $PRIM1
291600940503      *
291700970211     C     TASATK        READE(N)  FNSAT03L                               33
291800940503     C                   MOVEL     SATTRE        $TRE
291900940503     C*
292000940503    3C     *IN33         DOWEQ     *OFF
292100940503     C*
292200940503    4C     $TRE          IFNE      SATTRE
292300940503     C     $PRIM1        ANDEQ     0
292400970211     C     TRSATK        SETLL     FNSAT03L
292500970211     C     TRSATK        READE     FNSAT03L                               33
292600940503     C*
292700940503    5C     *IN33         DOWEQ     *OFF
292800970211     C                   DELETE    FNSAT003
292900970211     C     TRSATK        READE     FNSAT03L                               33
293000940503    5C                   ENDDO
293100940503     C                   MOVEL     SATTRE        $TRE
293200940503     C                   CLEAR                   $PRIM1
293300940503    4C                   ENDIF
293400940503     C*
293500000411     C* VEDO SE C'E' ALMENO UN VOLUME A MAGAZZINO
293600000411    4C     SATVNE        IFGT      0
293700000411     C     SATVND        ORGT      0
293800000411     C     SATVNC        ORGT      0
293900940503     C                   Z-ADD     1             $PRIM1
294000940503    4C                   ENDIF
294100970211     C     TASATK        READE(N)  FNSAT03L                               33
294200940503    3C                   ENDDO
294300940503     C* ELABORO L'ULTIMO
294400940503    3C     $PRIM1        IFEQ      0
294500970211     C     TRSATK        SETLL     FNSAT03L
294600970211     C     TRSATK        READE     FNSAT03L                               33
294700970211     C*
294800940503    4C     *IN33         DOWEQ     *OFF
294900970211     C                   DELETE    FNSAT003
295000970211     C     TRSATK        READE     FNSAT03L                               33
295100940503    4C                   ENDDO
295200940503    3C                   ENDIF
295300940503     C*
295400940503     C                   EXSR      IMPTFA
295500940503    2C                   ENDDO
295600940504     C*
295700940504     C                   ENDSR
295800000000      **************************************************************************
295900000000      * Stampo i terminal di arrivo
296000000000      **************************************************************************
296100000000     C     STATER        BEGSR
296200940429     C                   SETOFF                                       4333
296300940429     C                   SETON                                        30
296400950210     C                   Z-ADD     DS1FEL        $AREA
296500940429     C                   CLEAR                   $TFA
296600940509     C                   CLEAR                   $RIGHE
296700940429     C* ELABORO AREA PER AREA
296800940429     C  N39              EXSR      IMPARE
296900940429     C*
297000940429    2C     *IN33         DOWEQ     *OFF
297100940429     C*
297200940429     C* ELABORO TERMINAL PER TERMINAL
297300940429     C                   EXSR      IMPTFA
297400000000      *
297500940429    3C     *IN33         DOWEQ     *OFF
297600940429     C                   Z-ADD     *ZERO         $PRIM1
297700940429     C                   Z-ADD     *ZERO         $PRIM3
297800000000      *
297900970211     C     ARSATK        READE(N)  FNSAT03L                               33
298000940429     C*
298100940429    4C     *IN33         DOWEQ     *OFF
298200000000      *
298300940429     C* ELABORO SE DATA COMPRESA
298400940429     C* E, SE IMMESSA SOLTANTO UNA LINEA, SE E' LA LINEA RICHIESTA
298500970211    5C     SATDRE        IFLE      DS1AAL
298600940429     C*
298700940429     C* SE DA STAMPE GIORNALIERE ELABORO SLTANTO LE LINEE IN LIN
298800940429     C     SATLNP        LOOKUP    LIN                                    99
298900940429     C   99              EXSR      SERTRA
299000940429     C*
299100940429   X5C                   ELSE
299200940429     C                   SETON                                        33
299300940429    5C                   ENDIF
299400940429     C*
299500970211     C     ARSATK        READE(N)  FNSAT03L                               33
299600940429    4C                   ENDDO
299700940429      *
299800940429     C* Stampo le bolle dell'area
299900940429    4C     $PRIM1        IFEQ      1
300000940429     C                   EXSR      TOTFIL
300100940429     C                   EXSR      AZZFIL
300200940429    4C                   ENDIF
300300940429      *
300400940429      * Stampo le bolle in transito
300500940429    4C     $PRIM3        IFEQ      1
300600940429     C                   EXSR      TOTTRA
300700940429    4C                   ENDIF
300800940429     C*
300900940429     C* Elaboro nuovo terminal
301000940429     C                   EXSR      IMPTFA
301100940429    3C                   ENDDO
301200940429      *
301300940509     C                   Z-ADD     1             $PRIM3
301400970226     C                   SETON                                        0204
301500940429     C                   EXSR      TOTGEN
301600970226     C                   SETOFF                                       0204
301700940429     C* ELABORO NUOVA AREA
301800940429     C  N39              EXSR      IMPARE
301900940429    2C                   ENDDO
302000940429     C*
302100940429     C                   ENDSR
302200000000      **************************************************************************
302300000000      * Una volta letto il record verifico se da includere o no
302400000000      *
302500940428      * Includo se:
302600940428      * e' entrato al transito
302700940428      *  e non e' ancora uscito o e' uscito con data = > a  data elabor
302800940505      *            BTTDUT = 0
302900000000      *   oppure:
303000950210      *       AND  BTTDUT >= DS1ADAL
303100940428      *
303200940428      * escludo sempre i recuperi
303300000000      **************************************************************************
303400000000     C     ESCLU1        BEGSR
303500000000      *
303600000000     C                   MOVE      'N'           $OK
303700000000      *
303800000000     C     BTTDUT        IFEQ      *ZERO
303900950210     C     BTTDUT        ORGE      DS1AAL
304000000000     C                   MOVE      'S'           $OK
304100940428     C                   ENDIF
304200000000      *
304300000000      * Solo se superati i test ne eseguo altri
304400000000     C     $OK           IFEQ      'S'
304500940505      *
304600940505      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
304700940505      * come test ma mi permette di saltare delle chain)
304800950118    3C     $BTTAT        IFNE      $BLTPR
304900020625     C     FNBTPK        CHAIN     FNBTP11L                           99
305000950118     C  N99              MOVE      $BTTAT        $BLTPR
305100940505     C  N99              MOVEL     '1'           $BOLLA
305200940505     C   99              MOVEL     'N'           $OK
305300940505    3C                   ENDIF
305400000000      *
305500000000      * Eseguo la chain su TABEL solo se diverso dal precedente
305600950118     C  N99BTPCBO        IFNE      $CBLPR
305700950210     C                   MOVE      '3A'          KCOD
305800950210     C                   MOVE      *BLANKS       KKEY
305900950210     C                   MOVEL     BTPCBO        KKEY
306000950210     C     KTAB1         CHAIN     TABEL00F                           99
306100000000     C  N99              MOVEL     TBLUNI        DS3A
306200950118     C  N99              MOVE      BTPCBO        $CBLPR
306300000000     C   99              CLEAR                   DS3A
306400000000     C                   ENDIF
306500000000      *
306600000000      * Non considero i recuperi
306700940428     C     §3ARBL        IFEQ      'R'
306800000000     C                   MOVE      'N'           $OK
306900000000     C                   ENDIF
307000000000      *
307100000000     C                   ENDIF
307200000000      *
307300000000     C                   ENDSR
307400000000      **************************************************************************
307500970211      * RISTAMPA: lettura dal file statistiche (FNSAT00F) e output
307600000000      **************************************************************************
307700000000     C     RISTAM        BEGSR
307800000000      *
307900940502     C                   SETOFF                                       33
308000940428     C                   SETON                                        43
308100940429     C                   CLEAR                   $TFA
308200940509     C                   CLEAR                   $RIGHE
308300950210     C                   MOVEL     DS1FEL        $AREA
308400940429     C* LANCIO DA FILTRO
308500950210    1C     DS1FGS        IFGT      0
308600950210     C                   MOVEL     DS1LNP        SATLNP
308700940429     C*
308800940429     C* SE NON IMMESSA L'AREA LA CERCO
308900940429     C  N39              EXSR      IMPARE
309000940429     C*
309100940429    2C     *IN33         DOWEQ     *OFF
309200940429     C                   Z-ADD     *ZERO         $PRIM2
309300940429     C                   Z-ADD     *ZERO         $PRIM3
309400940429     C                   Z-ADD     *ZERO         $PRIM4
309500940429     C*
309600940429     C* SE NON IMMESSA LA LNP LA CERCO
309700940505     C                   EXSR      IMPLNP
309800000000      *
309900940429    3C     *IN33         DOWEQ     *OFF
310000940429     C                   Z-ADD     *ZERO         $PRIM1
310100940429     C* LEGGO IL FILE PER LNP
310200940429     C                   EXSR      LEGSAT
310300940429     C* cerco la nuova lnp
310400940429     C  N29              EXSR      IMPLNP
310500940429    3C                   ENDDO
310600940429     c*
310700940429     C* stampa finale dei C/S transiti e totale
310800940429     C                   EXSR      STAFIN
310900940429     C* cerco la nuova area
311000940429     C  N39              EXSR      IMPARE
311100940429    2C                   ENDDO
311200000000      *
311300950210   X1C                   ELSE
311400940429      *
311500940429      * Da stampe giornaliere
311600940429     C                   Z-ADD     1             $X
311700940429     C*
311800940506     C                   Z-ADD     *ZERO         $PRIM2            1 0
311900940506     C                   Z-ADD     *ZERO         $PRIM3            1 0
312000940506     C                   Z-ADD     *ZERO         $PRIM4            1 0
312100000912     C                   CLEAR                   WSTAF
312200940429    2C     LIN($X)       DOWGT     0
312300940429     C                   Z-ADD     LIN($X)       $LNP
312400940506     C                   Z-ADD     LIN($X)       $FILPR
312500940505     C                   Z-ADD     *ZERO         $PRIM1            1 0
312600000912     C* ESCLCUDO I P.O. POSTE
312700000912      * Escludo bolle LNP Poste
312800020813     ***C                   CLEAR                   §OGPT
312900020813     C                   clear                   OG143
313000000912     C     $FILPR        CHAIN     AZORG01L                           82
313100000912     C  N82              MOVEL     ORGDE3        OG143
313200000912     C**
313300020813   2A***C     §OGPT         IFNE      'S'
313400020813    2C                   IF        §OGntw <> 'PPT'
313500000912     C* SE ELABORATA ALMENO UNA LINEA, STAMPO ANCHE I TOTALI FINALI
313600000912     C                   MOVEL     'S'           WSTAF             1
313700000912     C*
313800970205     C     DTSATK        SETLL     FNSAT02L
313900940429     C* LEGGO IL FILE PER LINEA
314000940429     C                   EXSR      LEGSAT
314100000912     C                   ENDIF
314200940429     C                   ADD       1             $X
314300940429    2C                   ENDDO
314400940429     c*
314500000912     C     WSTAF         IFEQ      'S'
314600940429     C                   EXSR      STAFIN
314700940429    1C                   ENDIF
314800000912    1C                   ENDIF
314900940429     C*
315000940429     C                   ENDSR
315100940429      **************************************************************************
315200940429      * IMPOSTO L'AREA, LE CERCO MAN MANO                              i
315300940429      **************************************************************************
315400940429     C     IMPARE        BEGSR
315500970205     C     $AREA         SETGT     FNSAT02L
315600970205     C                   READ(N)   FNSAT02L                               33
315700940429     C*
315800940429     C     *IN33         IFEQ      *OFF
315900940429     C                   MOVEL     SATFLE        $AREA
316000940429     C                   ENDIF
316100940429     C                   ENDSR
316200940429      **************************************************************************
316300940429      * IMPOSTO LA LINEA DI PARTENZA, LA CERCO MAN MANO                i
316400940429      **************************************************************************
316500940429     C     IMPLNP        BEGSR
316600970205     C  N29LPSATK        SETGT     FNSAT02L
316700970205     C  N29ARSATK        READE(N)  FNSAT02L                               33
316800000619     C                   CLEAR                   WTROVA
316900940429     C*
317000000619    1C     WTROVA        DOWEQ     ' '
317100000619    2C     *IN33         IFEQ      *OFF
317200000619     C     SATLNP        LOOKUP    LIN                                    99
317300000619    3C     *IN99         IFEQ      *ON
317400940429     C                   MOVEL     SATLNP        $LNP
317500940429     C                   MOVEL     SATLNP        $FILPR
317600000619     C                   MOVEL     '1'           WTROVA            1
317700970205     C     DTSATK        SETLL     FNSAT02L
317800000619   X3C                   ELSE
317900000619     C  N29ARSATK        READE(N)  FNSAT02L                               33
318000000619    3C                   ENDIF
318100000619     C**
318200000619   X2C                   ELSE
318300000619     C                   MOVEL     '1'           WTROVA
318400000619    2C                   ENDIF
318500000619    1C                   ENDDO
318600000619     C*
318700940429     C                   ENDSR
318800940429      **************************************************************************
318900940429      * IMPOSTO IL TERMINAL, LO CERCO MAN MANO                         i
319000940429      **************************************************************************
319100940429     C     IMPTFA        BEGSR
319200970211     C     ARSATK        SETGT     FNSAT03L
319300970211     C     $AREA         READE(N)  FNSAT03L                               33
319400940429     C*
319500940429     C     *IN33         IFEQ      *OFF
319600940429     C                   MOVEL     SATTFA        $TFA
319700940429     C                   MOVEL     SATTFA        $FILPR
319800970211     C     TASATK        SETLL     FNSAT03L
319900940429     C                   ENDIF
320000940429     C                   ENDSR
320100940429      **************************************************************************
320200940429      * STAMPA FINALE DEI C/S E TRANSITI                               i
320300940429      **************************************************************************
320400940429     C     STAFIN        BEGSR
320500940429     C* stampo i totali C/S
320600940506     C     *IN38         IFEQ      *OFF
320700940429     C     $PRIM2        IFEQ      1
320800940429     C                   EXSR      TOTSER
320900940429     C                   ENDIF
321000940506     C* TOTALI DI AREA
321100970226     C                   SETON                                        04
321200940505     C                   EXSR      TOTGEN
321300970226     C                   SETOFF                                       04
321400940506     C                   ENDIF
321500940502     C*
321600940502      * Terminate le letture per filiale tocca ai transiti
321700950210     C     DS1RIS        IFNE      'R'
321800940502     C                   EXSR      ELATRA
321900940503     C*
322000940505     C* CANCELLO LE RIGHE DEI TERMINAL CHE NON HANNO COLLI A TERRA
322100940503     C                   EXSR      ELATER
322200940502     C                   ENDIF
322300940502     C*
322400940429     C* stampo i totali dei TRANSITI
322500940506     C     *IN38         IFEQ      *OFF
322600000308     C     *IN27         IFEQ      *OFF
322700000308     C     DS1RIS        OREQ      'R'
322800000308     C                   EXSR      TOTTRA
322900000308     C                   ENDIF
323000940429     c* stampo il totale di area
323100940429     C     $PRIM4        IFEQ      1
323200970226     C                   SETON                                        0204
323300940429     C                   EXSR      TOTGEN
323400970226     C                   SETOFF                                       0204
323500950118     C                   WRITE     LSC2E4
323600940429     C                   ENDIF
323700940506     C                   ENDIF
323800940429     C                   ENDSR
323900940429      **************************************************************************
324000950210      * IMPOSTO L'AREA, LE CERCO MAN MANO
324100940429      **************************************************************************
324200940429     C     LEGSAT        BEGSR
324300970205     C     LPSATK        READE(N)  FNSAT02L                               33
324400940429     C*
324500940429    4C     *IN33         DOWEQ     *OFF
324600940429      *
324700970211    5C     SATDRE        IFLE      DS1AAL
324800940429      * Aggiorno i totali
324900940429     C                   EXSR      SERTRA
325000940429   x5C                   ELSE
325100940429     C                   SETON                                        33
325200940429    5C                   ENDIF
325300940429     C*
325400970205     C  N33LPSATK        READE(N)  FNSAT02L                               33
325500940429    4C                   ENDDO
325600940429     C* totali di filiale
325700940429    4C     $PRIM1        IFEQ      1
325800940429     C                   EXSR      TOTFIL
325900940429     C                   EXSR      AZZFIL
326000940429    4C                   ENDIF
326100940429     C                   ENDSR
326200000000      **************************************************************************
326300000000      * RISTAMPA: per servizi o transiti non stampo ma aggiorno i totali
326400000000      **************************************************************************
326500000000     C     SERTRA        BEGSR
326600000000      *
326700000000     C     SATTRE        IFEQ      *BLANK
326800940429     C                   Z-ADD     1             $PRIM1
326900970211     C                   ADD       SATCPE        CPA(1)
327000970211     C                   ADD       SATCPD        CPA(2)
327100970211     C                   ADD       SATCPC        CPA(3)
327200970211     C                   ADD       SATCDE        CBP(1)
327300970211     C                   ADD       SATCDD        CBP(2)
327400970228     C                   ADD       SATCDC        CBP(3)
327500970211     C                   ADD       SATCNE        CBR(1)
327600970211     C                   ADD       SATCND        CBR(2)
327700970211     C                   ADD       SATCNC        CBR(3)
327800970211     C                   ADD       SATPPE        PPA(1)
327900970211     C                   ADD       SATPPD        PPA(2)
328000970211     C                   ADD       SATPPC        PPA(3)
328100970211     C                   ADD       SATFDE        PFI(1)
328200970211     C                   ADD       SATFDD        PFI(2)
328300970211     C                   ADD       SATFDC        PFI(3)
328400970211     C                   ADD       SATFNE        PFN(1)
328500970211     C                   ADD       SATFND        PFN(2)
328600970211     C                   ADD       SATFNC        PFN(3)
328700970211     C                   ADD       SATODE        PBP(1)
328800970211     C                   ADD       SATODD        PBP(2)
328900970211     C                   ADD       SATODC        PBP(3)
329000970211     C                   ADD       SATONE        PBR(1)
329100970211     C                   ADD       SATOND        PBR(2)
329200970211     C                   ADD       SATONC        PBR(3)
329300970211     C                   ADD       SATVPE        VPA(1)
329400970211     C                   ADD       SATVPD        VPA(2)
329500970211     C                   ADD       SATVPC        VPA(3)
329600970211     C                   ADD       SATVDE        VBP(1)
329700970211     C                   ADD       SATVDD        VBP(2)
329800970211     C                   ADD       SATVDC        VBP(3)
329900970211     C                   ADD       SATVNE        VBR(1)
330000970211     C                   ADD       SATVND        VBR(2)
330100970211     C                   ADD       SATVNC        VBR(3)
330200970211     C                   ADD       SATSPE        SPA(1)
330300970211     C                   ADD       SATSPD        SPA(2)
330400970211     C                   ADD       SATSPC        SPA(3)
330500970211     C                   ADD       SATSDE        SBP(1)
330600970211     C                   ADD       SATSDD        SBP(2)
330700970211     C                   ADD       SATSDC        SBP(3)
330800970211     C                   ADD       SATSNE        SBR(1)
330900970211     C                   ADD       SATSND        SBR(2)
331000970211     C                   ADD       SATSNC        SBR(3)
331100970305     C                   ADD       SATSFE        FAF(1)
331200970305     C                   ADD       SATSFD        FAF(2)
331300970305     C                   ADD       SATSFC        FAF(3)
331400000000     C                   ADD       SATSCE        SCM(1)
331500970211     C                   ADD       SATSCD        SCM(2)
331600970211     C                   ADD       SATSCC        SCM(3)
331700000000     C                   ENDIF
331800000000      *
331900000000     C     SATTRE        IFEQ      'C'
332000940429     C                   Z-ADD     1             $PRIM2
332100970211     C                   ADD       SATCPE        C1A(1)
332200970211     C                   ADD       SATCPD        C1A(2)
332300970211     C                   ADD       SATCPC        C1A(3)
332400970211     C                   ADD       SATCDE        C1P(1)
332500970211     C                   ADD       SATCDD        C1P(2)
332600970211     C                   ADD       SATCDC        C1P(3)
332700970211     C                   ADD       SATCNE        C1R(1)
332800970211     C                   ADD       SATCND        C1R(2)
332900970211     C                   ADD       SATCNC        C1R(3)
333000970211     C                   ADD       SATPPE        P1A(1)
333100970303     C                   ADD       SATPPD        P1A(2)
333200970303     C                   ADD       SATPPC        P1A(3)
333300970211     C                   ADD       SATFDE        P1I(3)
333400970211     C                   ADD       SATFDD        P1I(2)
333500970211     C                   ADD       SATFDC        P1I(3)
333600970211     C                   ADD       SATFNE        P1N(1)
333700970211     C                   ADD       SATFND        P1N(2)
333800970211     C                   ADD       SATFNC        P1N(3)
333900970211     C                   ADD       SATODE        P1P(1)
334000970211     C                   ADD       SATODD        P1P(2)
334100970211     C                   ADD       SATODC        P1P(3)
334200970211     C                   ADD       SATONE        P1R(1)
334300970211     C                   ADD       SATOND        P1R(2)
334400970211     C                   ADD       SATONC        P1R(3)
334500970211     C                   ADD       SATVPE        V1A(1)
334600970211     C                   ADD       SATVPD        V1A(2)
334700970211     C                   ADD       SATVPC        V1A(3)
334800970211     C                   ADD       SATVDE        V1P(1)
334900970211     C                   ADD       SATVDD        V1P(2)
335000970211     C                   ADD       SATVDC        V1P(3)
335100970211     C                   ADD       SATVNE        V1R(1)
335200970211     C                   ADD       SATVND        V1R(2)
335300970211     C                   ADD       SATVNC        V1R(3)
335400970211     C                   ADD       SATSPE        S1A(1)
335500970211     C                   ADD       SATSPD        S1A(2)
335600970211     C                   ADD       SATSPC        S1A(3)
335700970211     C                   ADD       SATSDE        S1P(1)
335800970211     C                   ADD       SATSDD        S1P(2)
335900970211     C                   ADD       SATSDC        S1P(3)
336000970211     C                   ADD       SATSNE        S1R(1)
336100970211     C                   ADD       SATSND        S1R(2)
336200970211     C                   ADD       SATSNC        S1R(3)
336300970305     C                   ADD       SATSFE        F1F(1)
336400970305     C                   ADD       SATSFD        F1F(2)
336500970305     C                   ADD       SATSFC        F1F(3)
336600000000     C                   ADD       SATSCE        S1M(1)
336700970211     C                   ADD       SATSCD        S1M(2)
336800970211     C                   ADD       SATSCC        S1M(3)
336900000000     C                   ENDIF
337000000000      *
337100940429     C     SATTRE        IFEQ      'T'
337200940429     C                   Z-ADD     1             $PRIM3
337300970211     C                   ADD       SATCDE        C2P(1)
337400970211     C                   ADD       SATCDD        C2P(2)
337500970211     C                   ADD       SATCDC        C2P(3)
337600970211     C                   ADD       SATFDE        P2I(1)
337700970211     C                   ADD       SATFDD        P2I(2)
337800970211     C                   ADD       SATFDC        P2I(3)
337900970211     C                   ADD       SATODE        P2P(1)
338000970211     C                   ADD       SATODD        P2P(2)
338100970211     C                   ADD       SATODC        P2P(3)
338200970211     C                   ADD       SATVDE        V2P(1)
338300970211     C                   ADD       SATVDD        V2P(2)
338400970211     C                   ADD       SATVDC        V2P(3)
338500970211     C                   ADD       SATSDE        S2P(1)
338600970211     C                   ADD       SATSDD        S2P(2)
338700970211     C                   ADD       SATSDC        S2P(3)
338800000000     C                   ADD       SATSCE        S2M(1)
338900970211     C                   ADD       SATSCD        S2M(2)
339000970211     C                   ADD       SATSCC        S2M(3)
339100940505     C*
339200940505     C                   ELSE
339300000000      *
339400940429      *  totali generali
339500940429     C                   Z-ADD     1             $PRIM4
339600970211     C                   ADD       SATCDE        C3P(1)
339700970211     C                   ADD       SATCDD        C3P(2)
339800970211     C                   ADD       SATCDC        C3P(3)
339900970211     C                   ADD       SATFDE        P3I(1)
340000970211     C                   ADD       SATFDD        P3I(2)
340100970211     C                   ADD       SATFDC        P3I(3)
340200970212     C                   ADD       SATODE        P3P(1)
340300970212     C                   ADD       SATODD        P3P(2)
340400970212     C                   ADD       SATODC        P3P(3)
340500970211     C                   ADD       SATVDE        V3P(1)
340600970211     C                   ADD       SATVDD        V3P(2)
340700970211     C                   ADD       SATVDC        V3P(3)
340800000406     C                   ADD       SATVNE        V3R(1)
340900000406     C                   ADD       SATVND        V3R(2)
341000000406     C                   ADD       SATVNC        V3R(3)
341100970211     C                   ADD       SATSDE        S3P(1)
341200970211     C                   ADD       SATSDD        S3P(2)
341300970211     C                   ADD       SATSDC        S3P(3)
341400970211     C                   ADD       SATSNE        S3R(1)
341500970211     C                   ADD       SATSND        S3R(2)
341600970211     C                   ADD       SATSNC        S3R(3)
341700970305     C                   ADD       SATSFE        F3F(1)
341800970305     C                   ADD       SATSFD        F3F(2)
341900970305     C                   ADD       SATSFC        F3F(3)
342000000000     C                   ADD       SATSCE        S3M(1)
342100970211     C                   ADD       SATSCD        S3M(2)
342200970211     C                   ADD       SATSCC        S3M(3)
342300940505     C                   ENDIF
342400000000      *
342500000000     C                   ENDSR
342600940505      **************************************************************************
342700940505      * RISTAMPA: addiziono i transiti al totale generale              i
342800940505      **************************************************************************
342900940505     C     ADDTRA        BEGSR
343000940505     C                   Z-ADD     1             $PRIM4
343100940505     C                   ADD       C2P           C3P
343200940505     C                   ADD       P2I           P3I
343300940505     C                   ADD       P2P           P3P
343400940505     C                   ADD       V2P           V3P
343500940505     C                   ADD       S2P           S3P
343600940505     C                   ADD       S2M           S3M
343700940505     C                   ENDSR
343800000000      **************************************************************************
343900000000      * RISTAMPA: azzera i totali generali
344000000000      **************************************************************************
344100000000     C     AZZTOT        BEGSR
344200000000      *
344300970212     C                   CLEAR                   C3A
344400970212     C                   CLEAR                   C3P
344500970211     C                   CLEAR                   P3I
344600970211     C                   CLEAR                   P3P
344700970211     C                   CLEAR                   P3A
344800970211     C                   CLEAR                   V3P
344900940502     C                   CLEAR                   V3A
345000000406     C                   CLEAR                   V3R
345100970211     C                   CLEAR                   S3A
345200970211     C                   CLEAR                   S3P
345300970211     C                   CLEAR                   S3R
345400970211     C                   CLEAR                   S3M
345500970305     C                   CLEAR                   F3F
345600000000      *
345700000000     C                   ENDSR
345800000000      **************************************************************************
345900000000      * Testa l'overflow e stampa su carta
346000000000      **************************************************************************
346100000000     C     SCRIVI        BEGSR
346200940509      * PER TOTALI GENERALI SE GIA' STAMPE PIU' DI 9 RIGHE COMUNQUE
346300940509      * VADO A PAGINA NUOVA
346400940509     C     *IN03         IFEQ      *OFF
346500940509     C     *IN02         ANDEQ     *ON
346600940509     C     $RIGHE        ANDGT     9
346700940509     C                   SETON                                        30
346800940509     C                   ENDIF
346900000000      *
347000940429     C     *IN38         IFEQ      *OFF
347100940509     C*
347200940509     C     *IN30         IFEQ      *ON
347300940509     C     *IN40         ANDEQ     *ON
347400950118     C                   WRITE     LSC2T1
347500950118     C                   WRITE     LSC2T3
347600940509     C                   CLEAR                   $RIGHE
347700940509     C                   SETOFF                                       30
347800940509     C                   ENDIF
347900950118     C                   WRITE     LSC2D1
348000000000      *
348100970226     C  N04$ALMEN        IFEQ      *ZERO
348200000000     C                   Z-ADD     1             $ALMEN
348300000000     C                   ENDIF
348400940328     C                   ENDIF
348500000000      *
348600000000     C                   ENDSR
348700000000      **************************************************************************
348800000000      * Verifica se vi sono dei fogli viaggio con data partenza compresa
348900000000      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
349000000000      **************************************************************************
349100000000     C     FOGVIA        BEGSR
349200020813     ***C                   OPEN      FNFGV04L
349300020813     C                   OPEN      FNFGV06L
349400000000      *
349500020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
349600020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
349700020813     C     SIMfel        setll     FNFGV06L                               99
349800000000     C                   DO        *HIVAL
349900020813     ***C                   READ      FNFGV04L                               99
350000020813     C     SIMfel        reade     FNFGV06L                               99
350100950210     C  N99FGVDTP        IFGE      DS1ADA
350200950210     C     FGVDTP        ANDLE     DS1AAL
350300940502     C                   SETON                                        99
350400950118     C                   WRITE     LSC2E3
350500000000     C                   ENDIF
350600940502     C  N99              ENDDO
350700000000      *
350800020813     ***C                   CLOSE     FNFGV04L
350900020813     C                   CLOSE     FNFGV06L
351000000000     C                   ENDSR
351100000000      **************************************************************************
351200000000      * Aggiornamento del numero colli esaminati, nel file di work
351300950118     OFNWRK000  E            AGGIO
351400000000     O                       WRKNC1
351500000000      **************************************************************************
351600000000      *                                4         5         6         7         8
351700950118** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
351800970807OVRPRTF FNLSC2P       FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.terra
351900950210')
352000000000**
352100950118ESPR.DISTRCORR.
