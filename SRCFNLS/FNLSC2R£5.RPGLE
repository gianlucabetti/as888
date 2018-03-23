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
003000090916      *          semplice lettura (e stampa) del file fisat00F, nel quale sono
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
004500090916     Ffisat02L  UF A E           K DISK
004600090916     Ffisat03L  UF   E           K DISK
004700090916     F                                     RENAME(fisat000:fisat003)
004800090916     Ffisat04L  UF   E           K DISK    USROPN
004900090916     F                                     RENAME(fisat000:fisat004)
005000950118     FFNWRK00F  UF A E           K DISK
005100000000     FTABEL00F  IF   E           K DISK
005200021016     fTntbe01l  uf   e           k Disk
005300090415     Ffista01L  uF   E           K DISK
005400970807     FFNLSC2P   O    E             PRINTER OFLIND(*IN30)
005500970807     F                                     USROPN
005600000000      *
005700000000     D L1              S              3  0 DIM(30)
005800020418     D L6              S              3  0 DIM(30)
005900950213     D LV2             S              3  0 DIM(30)
006000000616     D LIN             S              3  0 DIM(30)
006100000616     D SVLIN           S              3  0 DIM(30)
006200950210     D CMD             S              1    DIM(82) CTDATA PERRCD(80)
006300090916      * Le schiere che seguono hanno 5 elementi:
006400000000      *     1° elemento: spedizione espressa (ESPR.)
006500950627      *     2° elemento: spedizione distribuzione (DISTR)
006600950627      *     3° elemento: spedizione corriere (CORR.)
006700090916      *     4° elemento: spedizione corriere (h1030)
006800090916      *     5° elemento: spedizione corriere (usi futuri)
006900090916     D ENS             S              5    DIM(5)
007000000000      * COLLI
007100090916     D CPA             S              7  0 DIM(5)
007200090916     D CBP             S              7  0 DIM(5)
007300090916     D CBR             S              7  0 DIM(5)
007400000000      * PESI
007500090916     D PPA             S              9  1 DIM(5)
007600090916     D PFI             S              9  1 DIM(5)
007700090916     D PFN             S              9  1 DIM(5)
007800090916     D PBP             S              9  1 DIM(5)
007900090916     D PBR             S              9  1 DIM(5)
008000000000      * VOLUMI
008100090916     D VPA             S              9  4 DIM(5)
008200090916     D VBP             S              9  4 DIM(5)
008300090916     D VBR             S              9  4 DIM(5)
008400000000      * SPEDIZIONI
008500090916     D SPA             S              7  0 DIM(5)
008600090916     D SBP             S              7  0 DIM(5)
008700090916     D SBR             S              7  0 DIM(5)
008800090916     D SCM             S              7  0 DIM(5)
008900090916     D FAF             S              7  0 DIM(5)
009000000000      * COLLI PER C/SERVIZIO (TOTALE AREA) -------------------------------------
009100090916AREA D C1A             S              7  0 DIM(5)
009200090916AREA D C1P             S              7  0 DIM(5)
009300090916AREA D C1R             S              7  0 DIM(5)
009400000000      * PESI PER C/SERVIZIO (TOTALE AREA)
009500090916AREA D P1A             S              9  1 DIM(5)
009600090916AREA D P1I             S              9  1 DIM(5)
009700090916AREA D P1N             S              9  1 DIM(5)
009800090916AREA D P1P             S              9  1 DIM(5)
009900090916AREA D P1R             S              9  1 DIM(5)
010000000000      * VOLUMI PER C/SERVIZIO (TOTALE AREA)
010100090916AREA D V1A             S              9  4 DIM(5)
010200090916AREA D V1P             S              9  4 DIM(5)
010300090916AREA D V1R             S              9  4 DIM(5)
010400000000      * SPEDIZIONI PER C/SERVIZIO (TOTALE AREA)
010500090916AREA D S1A             S              7  0 DIM(5)
010600090916AREA D S1P             S              7  0 DIM(5)
010700090916AREA D S1R             S              7  0 DIM(5)
010800090916AREA D S1M             S              7  0 DIM(5)
010900090916AREA D F1F             S              7  0 DIM(5)
011000000000      * COLLI PER C/SERVIZIO (PER FILIALE) -------------------------------------
011100090916FIL  D C4A             S              7  0 DIM(5)
011200090916FIL  D C4P             S              7  0 DIM(5)
011300090916FIL  D C4R             S              7  0 DIM(5)
011400000000      * PESI PER C/SERVIZIO (PER FILIALE)
011500090916FIL  D P4A             S              9  1 DIM(5)
011600090916FIL  D P4I             S              9  1 DIM(5)
011700090916FIL  D P4N             S              9  1 DIM(5)
011800090916FIL  D P4P             S              9  1 DIM(5)
011900090916FIL  D P4R             S              9  1 DIM(5)
012000000000      * VOLUMI PER C/SERVIZIO (PER FILIALE)
012100090916FIL  D V4A             S              9  4 DIM(5)
012200090916FIL  D V4P             S              9  4 DIM(5)
012300090916FIL  D V4R             S              9  4 DIM(5)
012400000000      * SPEDIZIONI PER C/SERVIZIO (PER FILIALE)
012500090916FIL  D S4A             S              7  0 DIM(5)
012600090916FIL  D S4P             S              7  0 DIM(5)
012700090916FIL  D S4R             S              7  0 DIM(5)
012800090916FIL  D S4M             S              7  0 DIM(5)
012900090916FIL  D F4F             S              7  0 DIM(5)
013000940502      *
013100940502      * COLLI PER TRANSITI (TOTALE GENERALE) -----------------------------------
013200090916GEN  D C2P             S              7  0 DIM(5)
013300000000      * PESI PER TRANSITI (TOTALE GENERALE)
013400090916GEN  D P2I             S              9  1 DIM(5)
013500090916GEN  D P2P             S              9  1 DIM(5)
013600000000      * VOLUMI PER TRANSITI (TOTALE GENERALE)
013700090916GEN  D V2P             S              9  4 DIM(5)
013800000000      * SPEDIZIONI PER TRANSITI (TOTALE GENERALE)
013900090916GEN  D S2P             S              7  0 DIM(5)
014000090916GEN  D S2M             S              7  0 DIM(5)
014100940429      * COLLI PER TOTALE AREA --------------------------------------------------
014200090916     D C3A             S              7  0 DIM(5)
014300090916     D C3P             S              7  0 DIM(5)
014400940428      * PESI PER TOTALE GENERALE
014500090916     D P3A             S              9  1 DIM(5)
014600090916     D P3I             S              9  1 DIM(5)
014700090916     D P3P             S              9  1 DIM(5)
014800940428      * VOLUMI PER TOTALE GENERALE
014900090916     D V3A             S              9  4 DIM(5)
015000090916     D V3P             S              9  4 DIM(5)
015100090916     D V3R             S              9  4 DIM(5)
015200940428      * SPEDIZIONI PER TOTALE GENERALE
015300090916     D S3A             S              7  0 DIM(5)
015400090916     D S3P             S              7  0 DIM(5)
015500090916     D S3R             S              7  0 DIM(5)
015600090916     D S3M             S              7  0 DIM(5)
015700090916     D F3F             S              7  0 DIM(5)
015800090916      * COLLI PER FILE fisat00F ------------------------------------------------
015900090916      * PESI PER FILE fisat00F
016000090916      * VOLUMI PER FILE fisat00F
016100090916      * SPEDIZIONI PER FILE fisat00F
016200000000      *
016300940504     D GME             S              1    DIM(31)
016400090108     D GMEP            S              1    DIM(31)
016500020418
016600020418     d sav_t06         S              3  0 dim(30)
016700020418
016800020418     d wc              s              3  0
016900021016
017000021016     d kTbeCod         s                   Like(TbeCod)
017100021016     d kTbeKe1         s                   Like(TbeKe1)
017200020418
017300040921     D WLBDAT          DS                  INZ
017400040921     D  G02DAT                 1      8  0
017500040921     D  G02INV                 9     16  0
017600040921     D  G02ERR                17     17
017700040921     D  G02TGI                18     22  0
017800000000      *
017900000810     D DS3A          E DS
018000000000     D DS3L          E DS
018100970707     D DS5P          E DS
018200000000     D KPJBA         E DS
018300000000     D CNCR80        E DS
018400000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
018500950210     D* DS PER TRUL06R - CARICAMENTO £X
018600950210     D DSUL06        E DS                  EXTNAME(TRUL06DS)
018700950314     D  T06                    1     90  0
018800950314     D                                     DIM(30)
018900970303      * DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
019000970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
019100950210     D DSLS01        E DS
019200020813      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
019300020813     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
019400070522     D* DS PER LETTURA TABELLA "VPO" record DECO
019500070522     D Dvpodeco      E DS
019600021016      * Ds date statistiche
019700021016     d dSdf          e ds
019800090605
019900090605     d ds5e          e ds
020000020813      *
020100950210     D                 DS
020200950210     D  DATADA                 1      8  0
020300950210     D  ANNODA                 1      4  0
020400950210     D  MESEDA                 5      6  0
020500950210     D  $GI                    7      8  0
020600950118      * DS per controllare il "precedente" di FNBLT
020700000000     D $BLTAT          DS
020800000000     D  BLTLNP                 1      3  0
020900950118     D  BLTAAS                 4      7  0
021000950118     D  BLTNRS                 8      9  0
021100950118     D  BLTNSP                10     16  0
021200950118      * DS per controllare il "precedente" di FNBTT
021300950118     D $BTTAT          DS
021400950118     D  BTTLNP                 1      3  0
021500950118     D  BTTAAS                 4      7  0
021600950118     D  BTTNRS                 8      9  0
021700950118     D  BTTNSP                10     16  0
021800090916      * DS per i campi del file statistiche fisat00F
021900000000     D                 DS
022000090916     D  C0P                    1     35  0
022100090916     D                                     DIM(5)
022200090916     D  SATCDE                 1      7  0
022300090916     D  SATCDD                 8     14  0
022400090916     D  SATCDC                15     21  0
022500090916     D  SATCDh                22     28  0
022600090916     D  SATCDx                29     35  0
022700000000     D                 DS
022800090916     D  C0R                    1     35  0
022900090916     D                                     DIM(5)
023000970207     D  SATCNE                 1      7  0
023100970207     D  SATCND                 8     14  0
023200970207     D  SATCNC                15     21  0
023300090916     D  SATCnh                22     28  0
023400090916     D  SATCnx                29     35  0
023500000000     D                 DS
023600090916     D  P0I                    1     45  1
023700090916     D                                     DIM(5)
023800970207     D  SATFDE                 1      9  1
023900970207     D  SATFDD                10     18  1
024000970207     D  SATFDC                19     27  1
024100090916     D  SATfdh                28     36  1
024200090916     D  SATfdx                37     45  1
024300970207     D                 DS
024400090916     D  P0N                    1     45  1
024500090916     D                                     DIM(5)
024600970207     D  SATFNE                 1      9  1
024700970207     D  SATFND                10     18  1
024800970207     D  SATFNC                19     27  1
024900090916     D  SATfnh                28     36  1
025000090916     D  SATfnx                37     45  1
025100000000     D                 DS
025200090916     D  P0P                    1     45  1
025300090916     D                                     DIM(5)
025400970207     D  SATODE                 1      9  1
025500970207     D  SATODD                10     18  1
025600970207     D  SATODC                19     27  1
025700090916     D  SATOdh                28     36  1
025800090916     D  SATOdx                37     45  1
025900000000     D                 DS
026000090916     D  P0R                    1     45  1
026100090916     D                                     DIM(5)
026200970207     D  SATONE                 1      9  1
026300970207     D  SATOND                10     18  1
026400970207     D  SATONC                19     27  1
026500090916     D  SATonh                28     36  1
026600090916     D  SATonx                37     45  1
026700000000     D                 DS
026800090916     D  V0P                    1     45  4
026900090916     D                                     DIM(5)
027000970207     D  SATVDE                 1      9  4
027100970207     D  SATVDD                10     18  4
027200970207     D  SATVDC                19     27  4
027300090916     D  SATvdh                28     36  4
027400090916     D  SATvdx                37     45  4
027500000000     D                 DS
027600090916     D  V0R                    1     45  4
027700090916     D                                     DIM(5)
027800970207     D  SATVNE                 1      9  4
027900970207     D  SATVND                10     18  4
028000970207     D  SATVNC                19     27  4
028100090916     D  SATvnh                28     36  4
028200090916     D  SATvnx                37     45  4
028300000000     D                 DS
028400090916     D  S0P                    1     35  0
028500090916     D                                     DIM(5)
028600970207     D  SATSDE                 1      7  0
028700970207     D  SATSDD                 8     14  0
028800970207     D  SATSDC                15     21  0
028900090916     D  SATsdh                22     28  0
029000090916     D  SATsdx                29     35  0
029100000000     D                 DS
029200090916     D  S0R                    1     35  0
029300090916     D                                     DIM(5)
029400970207     D  SATSNE                 1      7  0
029500970207     D  SATSND                 8     14  0
029600970207     D  SATSNC                15     21  0
029700090916     D  SATsnh                22     28  0
029800090916     D  SATsnx                29     35  0
029900970305     D                 DS
030000090916     D  F0F                    1     35  0
030100090916     D                                     DIM(5)
030200970305     D  SATSFE                 1      7  0
030300970305     D  SATSFD                 8     14  0
030400970305     D  SATSFC                15     21  0
030500090916     D  SATsfh                22     28  0
030600090916     D  SATsfx                29     35  0
030700000000     D                 DS
030800090916     D  S0M                    1     35  0
030900090916     D                                     DIM(5)
031000000000     D  SATSCE                 1      7  0
031100970207     D  SATSCD                 8     14  0
031200970207     D  SATSCC                15     21  0
031300090916     D  SATsch                22     28  0
031400090916     D  SATscx                29     35  0
031500970210     D                 DS
031600090916     D  CDT                    1     35  0
031700090916     D                                     DIM(5)
031800970210     D  SATCPE                 1      7  0
031900970210     D  SATCPD                 8     14  0
032000970211     D  SATCPC                15     21  0
032100090916     D  SATcph                22     28  0
032200090916     D  SATcpx                29     35  0
032300970210     D                 DS
032400090916     D  PDT                    1     45  1
032500090916     D                                     DIM(5)
032600970210     D  SATPPE                 1      9  1
032700970210     D  SATPPD                10     18  1
032800970210     D  SATPPC                19     27  1
032900090916     D  SATpph                28     36  1
033000090916     D  SATppx                37     45  1
033100970210     D                 DS
033200090916     D  VDT                    1     45  4
033300090916     D                                     DIM(5)
033400970210     D  SATVPE                 1      9  4
033500970210     D  SATVPD                10     18  4
033600970210     D  SATVPC                19     27  4
033700090916     D  SATvph                28     36  4
033800090916     D  SATvpx                37     45  4
033900970210     D                 DS
034000090916     D  SDT                    1     35  0
034100090916     D                                     DIM(5)
034200970210     D  SATSPE                 1      7  0
034300970210     D  SATSPD                 8     14  0
034400970210     D  SATSPC                15     21  0
034500090916     D  SATsph                22     28  0
034600090916     D  SATspx                29     35  0
034700020813      *
034800090916      * Variabili
034900020813      *
035000020813     D SAVRIS          S                   LIKE(DS1RIS) inz
035100020813     D $CDA            S              7  0              inz
035200020813     D $VDA            S              9  4              inz
035300020813     D $PDA            S              9  1              inz
035400020813     D $SDA            S              7  0              inz
035500020813     D $TFA            S                   LIKE(BLPTFA) inz
035600020813     D $AREA           S                   LIKE(DS1FEL) inz
035700020813     D $LNP            S                   LIKE(DS1LNP) inz
035800020813     D $BLTPR          S                   LIKE($BLTAT) inz
035900020813     D $FILPR          S                   LIKE(BLPLNP) inz
036000020813     D $CBLPR          S                   LIKE(BLPCBO) inz
036100020813     D $NFVPR          S                   LIKE(BLPNFV) inz
036200020813     D $DTRIT          S                   LIKE(BLPDRT) inz
036300020813     D $CDPDR          S                   LIKE(BLPPDR) inz
036400020813     D $FLAG           S                   LIKE(WRKFLG) inz
036500020813     D $FLAG2          S                   LIKE(WRKFLG) inz
036600020813     D $DFV            S                   LIKE(FGVDFV) inz
036700020813     D $FTR            S                   LIKE(SATFTR) inz
036800020813     D $DTR            S                   LIKE(SATDTR) inz
036900020813     D $TRE            S                   LIKE(SATTRE) inz
037000020813     D $COMOD          S             15  6 INZ
037100020813     D $COMO1          S             15  6 INZ
037200020813     D*
037300020813     D KCOD            S                   LIKE(TBLCOD) inz
037400020813     D KKEY            S                   LIKE(TBLKEY) inz
037500021217      *
037600021217     d $Volum          s             12  4 inz
037700021217     d $Peso           s              8  1 inz
037800030213     d*** WsiVOL          s              1    inz
037900030213     d*** WsiPES          s              1    inz
038000021217     d Wvolu           s                   like(BLPvlf) inz
038100021217     d Wpeso           s                   like(BLPpkf) inz
038200021220     d Wvolmed         s                   like($Volum) inz
038300021220     d Wpesmed         s                   like($Peso)  inz
038400090415     D ktrc            S                   LIKE(statrc) inz('T')
038500090415     D kfil            S                   LIKE(stafil) inz(000)
038600940502      **************************************************************************
038700940502      *      RIEPILOGO INDICATORI
038800940502      **************************************************************************
038900940502      * 01    - ON: LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE = 0
039000940502      *       - OFF:LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE > D.ELAB
039100940502      * 02    - ON: TOTALE GENERALE
039200940502      *       - OFF:TOTALE DI AREA
039300970226      * 03    - STAMPA DEI TOTALI TRANSITI
039400970226      * 04    - NON MEMORIZZA $ALMEN NELLA ROURINE DI TOTGEN
039500940502      * 05    - STAMPE GIORNALIERE
039600970205      * 27    - OFF: DEVO STAMPARE LA RIGA DEI TRANASITI
039700940502      * 29    - ON: IMMESSA UNA LINEA DI PARTENZA
039800940502      * 30    - ECCEDENZA PAGINA
039900940502      * 33    - LETTURA FILES
040000940502      * 34    - BOLLA IN C/SERVIZIO
040100940502      * 35    - ON: BOLLA BOLLETTATA NEL PERIODO
040200940502      *       - OFF:BOLLA BOLLETTATA NEL PERIODO PRECEDENTE
040300940502      * 36    - COLLO A TERRA
040400090916      * 37    - LETTURA SUL FILES fisat
040500950210      * 38    - DS1RIS= ' ' --> NON DEVO STAMPARE
040600940502      * 39    - ON: IMMESSO CODICE AREA
040700940502      * 40    - ON: STAMPA LA LILIALE CON DECODIFICA
040800940502      *       - OFF:STAMPA LA DICITURA DEL SERVIZIO BOLLE
040900940502      * 41    - ON: STAMPA SOLTANTO LA FILIALE (+ 40 ON)
041000940502      * 43    - ON: STAMPO LA PARTE RELATIVA ALLE LINEE DI PARTENZA
041100940502      *       - OFF:STAMPO LA PARTE RELATIVA AI TERMINAL DI ARRIVO
041200970211      * 45    - STAMPA LE PERCENTUALI SUL DA PARTIRE
041300950118      * 99    - DI COMODO
041400940502      **************************************************************************
041500000000     C     *ENTRY        PLIST
041600000000     C                   PARM                    KPJBA
041700950210     C                   MOVEL     KPJBU         DSLS01
041800950210      *
041900950210      * DATA FINALE AA/MM/GG (6 CARATTERI)
042000090108     C                   Z-ADD     DS1aal        DATADA
042100000000      *
042200090415     c     KSTA          Klist
042300090415     c                   Kfld                    Ktrc
042400090415     c                   Kfld                    DS1FEL
042500090415     c                   Kfld                    Kfil
042600090415     c                   Kfld                    ds1ada
042700950118     C     FNBLTK        KLIST
042800940428     C                   KFLD                    LIN($X)
042900940428     C                   KFLD                    $DFV
043000970205     C     FNBLPK        KLIST
043100950118     C                   KFLD                    BLTAAS
043200000000     C                   KFLD                    BLTLNP
043300000000     C                   KFLD                    BLTNRS
043400000000     C                   KFLD                    BLTNSP
043500950118     C     FNBTPK        KLIST
043600020625     c                   kfld                    bttflp
043700950118     C                   KFLD                    BTTAAS
043800950118     C                   KFLD                    BTTLNP
043900950118     C                   KFLD                    BTTNRS
044000950118     C                   KFLD                    BTTNSP
044100950118     C     AZCLNK        KLIST
044200940504     C                   KFLD                    SIMFEL
044300940504     C                   KFLD                    $TFA
044400940504     C                   KFLD                    ANNODA
044500940504     C                   KFLD                    MESEDA
044600950118     C     FNWRKK        KLIST
044700000000     C                   KFLD                    $FLAG
044800000000     C                   KFLD                    $FILPR
044900950118     C     FNWR1K        KLIST
045000000000     C                   KFLD                    $FLAG
045100000000     C                   KFLD                    WRKLNP
045200940503     C                   KFLD                    BLPAAS
045300940503     C                   KFLD                    BLPNRS
045400940503     C                   KFLD                    BLPNSP
045500000000     C                   KFLD                    WRKCSE
045600970409     C     FNWR2K        KLIST
045700970409     C                   KFLD                    $FLAG
045800970409     C                   KFLD                    WRKLNP
045900970409     C                   KFLD                    BTPAAS
046000970409     C                   KFLD                    BTPNRS
046100970409     C                   KFLD                    BTPNSP
046200970409     C                   KFLD                    WRKCSE
046300090916     C     fisatK        KLIST
046400970205     C                   KFLD                    SIMFEL
046500970205     C                   KFLD                    LIN(XX)
046600970205     C                   KFLD                    DS1AAL
046700970205     C     FNSA1K        KLIST
046800000000     C                   KFLD                    SATFLE
046900000000     C                   KFLD                    SATTFA
047000940429     C                   KFLD                    SATLNP
047100000000     C                   KFLD                    SATDRE
047200000000     C                   KFLD                    SATTRE
047300970205     C     DTSATK        KLIST
047400940429     C                   KFLD                    $AREA
047500940429     C                   KFLD                    $TFA
047600940429     C                   KFLD                    $LNP
047700970211     C                   KFLD                    DS1ADA
047800970205     C     LPSATK        KLIST
047900940429     C                   KFLD                    $AREA
048000940429     C                   KFLD                    $TFA
048100940429     C                   KFLD                    $LNP
048200970205     C     ARSATK        KLIST
048300940429     C                   KFLD                    $AREA
048400940429     C                   KFLD                    $TFA
048500970211     C     TASATK        KLIST
048600940429     C                   KFLD                    $AREA
048700940429     C                   KFLD                    $TFA
048800970211     C                   KFLD                    DS1ADA
048900970211     C     TRSATK        KLIST
049000940503     C                   KFLD                    $AREA
049100940503     C                   KFLD                    $TFA
049200970211     C                   KFLD                    DS1ADA
049300940503     C                   KFLD                    $TRE
049400020625     c     kbtt39        klist
049500020625     c                   kfld                    simfel
049600020625     c                   kfld                    $dfv
049700021016
049800021016     c     Ktbe          Klist
049900021016     c                   Kfld                    kTbeCod
050000021016     c                   Kfld                    kTbeKe1
050100000000      *
050200021016     C                   Z-ADD     *ZERO         $COMO1           15 6
050300021016     C                   Z-ADD     1             CODUT
050400000000      **************************************************************************
050500000000      *              M A I N      L I N E
050600000000      **************************************************************************
050700940429     C                   MOVE      'RISTAMPA'    STRIP
050800940429     C*
050900950210    1C     DS1RIS        IFNE      'R'
051000940429     C                   MOVE      'STAMPA  '    STRIP
051100940502    1C                   ENDIF
051200070522     c* Carico la tabella VPO per vedere da quando cambia il calcolo
051300070522     c*  della affidabilità partenza
051400070522     C                   MOVEL     'VPO'         kTBECOD
051500070522     C                   MOVEL(P)  'DECO'        kTBEKE1
051600070522     C     KTBE          CHAIN(N)  TNTBE01L                           21
051700070522     C  N21              MOVEL     TBEUNI        DVPODECO
051800070522     C*
051900070522     c
052000090605      * carico le descrizioni fisse dei tipi servizio
052100090605      * chiodo l'indice di schiera per ogni tipo servizio
052200090605      * 1=E - 2=D - 3=C
052300090605     c                   clear                   ds5e
052400090605     c                   clear                   ens
052500090605     c                   eval      kcod = '5E'
052600090605     c     ktab2         setll     tabel00f
052700090605     c                   do        *hival
052800090605     c     ktab2         reade(n)  tabel00f
052900090605     c                   if        %eof(tabel00f)
053000090605     c                   leave
053100090605     c                   endif
053200090605     c                   eval      ds5e = tbluni
053300090605     c                   if        tblkey = 'C'
053400090605     c                   eval      ens(03) = §5ed05
053500090605     c                   endif
053600090605     c                   if        tblkey = 'D'
053700090605     c                   eval      ens(02) = §5ed05
053800090605     c                   endif
053900090605     c                   if        tblkey = 'E'
054000090605     c                   eval      ens(01) = §5ed05
054100090605     c                   endif
054200090916     c                   if        tblkey = 'H'
054300090916     c                   eval      ens(04) = §5ed05
054400090916     c                   endif
054500090605     c                   enddo
054600090605
054700000000      *
054800000000      * Se la data AL e' vuota la imposto come la data DAL
054900950210     C     DS1AAL        IFEQ      *ZERO
055000950210     C                   Z-ADD     DS1ADA        DS1AAL
055100950210     C                   Z-ADD     DS1GDA        DS1GAL
055200000000     C                   ENDIF
055300940428     C*
055400940428     C                   SETON                                        43
055500940401      *
055600950210      * Accendo un indicatore se non devo stampare il tabulato (DS1RIS=' ')
055700950210     C     DS1RIS        COMP      *BLANKS                                38
055800940429      *
055900940429      * 39 ON - Immessa l'area
056000950210    3C     DS1FEL        IFGT      0
056100940429     C                   SETON                                        39
056200940429    3C                   ENDIF
056300000000      *
056400950210      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
056500950210     C                   EXSR      COMUNE
056600950210      *
056700000000     C                   SETON                                        LR
056800020212     C*****************************************************************
056900020212     C*  PARTE COMUNE STAMPE GIORNALIERE
057000020212     C*
057100020212     C*  . REPERIMENTO 'SIMFEL'
057200020212     C*  . CARICAMENTO '£1'
057300020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
057400020212     C*  . CODA DI STAMPA ('£OUTQ')
057500020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
057600020212     C*  . MODULO PER LA STAMPA ('£MOD')
057700020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
057800020212     C*****************************************************************
057900020212     C     COMUNE        BEGSR
058000020212     C*
058100020212     C*****************************************************************
058200020212     C*       RIEPILOGO INDICATORI:
058300020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
058400020212     C* 11    -           STAMPA PER 1L
058500020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
058600020212     C*****************************************************************
058700020212     C                   Z-ADD     1             CODUT
058800020212     C*
058900020212     C* Reperimento parametri aziendali
059000020212     C                   CALL      'X§PARUT'
059100020212     C                   PARM                    UT§DSE
059200020212     C                   MOVEL     RAGUT         RSUT             20
059300020212     C                   MOVEL     REC80         CNCR80
059400020212     C*
059500020212     C* Chiave di accesso per il file tabelle
059600020212     C     KTAB1         KLIST
059700020212     C                   KFLD                    CODUT
059800020212     C                   KFLD                    KCOD
059900020212     C                   KFLD                    KKEY
060000020212     C     KTAB2         KLIST
060100020212     C                   KFLD                    CODUT
060200020212     C                   KFLD                    KCOD
060300020212     C*
060400020813     ***C     *LIKE         DEFINE    TBLCOD        KCOD
060500020813     ***C     *LIKE         DEFINE    TBLKEY        KKEY
060600020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
060700020212     C*
060800020212     C* DETERMINO DATA E ORA DI ESECUZIONE
060900020212     C                   TIME                    WTIME            14 0
061000020212     C                   MOVE      WTIME         WDATE             8 0
061100020212     C                   MOVEL     WTIME         UTIME             6 0
061200040921     c*
061300040921     C                   CLEAR                   WLBDAT
061400040921     C                   MOVE      Wtime         G02DAT
061500040921     C                   CALL      'XSRDA8'
061600040921     C                   PARM                    WLBDAT
061700040921     C                   Z-ADD     G02INV        dateu             8 0
061800090916     c* Accendo indicatore 10 de devo stampare anche tipo servizio H
061900090916     c                   if        ds1ada>=§vpotsph
062000090916     c                   seton                                        10
062100090916     c                   endif
062200090916
062300090916     c* Se c'e' già il tipo servizio  H ciclo 4 volte,
062400090916     c*  altrimenti 3
062500090916     c  n10              z-add     3             ciclo             2 0
062600090916     c   10              z-add     4             ciclo             2 0
062700040922     c*
062800040922     c                   clear                   $FTR
062900040922     c                   z-add     dateu         $DTR
063000020212     C*
063100020418     C* CARICO '£1'
063200020212     C                   CLEAR                   KPJBU
063300020212     C                   CLEAR                   DSUL06
063400020212     C                   MOVE      '£1'          D06COD
063500020212     C                   MOVEL     SIMFEL        D06KEY
063600050802     C                   MOVE      'S'           D06KEY
063700050802     C                   MOVEL     ds1ada        D06drf
063800020212     C                   MOVEL     DSUL06        KPJBU
063900020212     C                   CALL      'TRUL06R'
064000020212     C                   PARM                    KPJBA
064100020212     C                   MOVEL     KPJBU         DSUL06
064200020418     c                   movea     t06           sav_t06
064300020418
064400020418      * se in arrivo carico la £6
064500020418 b1  c                   if        ds1tbo = 'A'
064600020418     c                   clear                   wc
064700020418 b2  c                   do        30            wa
064800020422     c                   if        sav_t06(wa) = *zeros
064900020422     c                   leave
065000020422     c                   endif
065100020418     c                   clear                   kpjbu
065200020418     c                   clear                   dsul06
065300020418     c                   eval      d06cod = '£6'
065400020418     c                   movel     sav_t06(wa)   d06key
065500050802     C                   MOVE      'S'           D06KEY
065600020418     c                   eval      d06esc = 'S'
065700050802     c                   movel     ds1ada        d06drf
065800020419     c                   movel     dsul06        kpjbu
065900020418     c                   call      'TRUL06R'
066000020418     c                   parm                    kpjba
066100020418     c                   movel     kpjbu         dsul06
066200020418 b3  c                   do        30            wb
066300020418     c                   if        t06(wb) = *zeros
066400020418     c                   leave
066500020418     c                   endif
066600020418     c                   add       1             wc
066700020418     c                   z-add     t06(wb)       l6(wc)
066800020418 e3  c                   enddo
066900020418 e2  c                   enddo
067000020212     C*
067100020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
067200020212     C                   Z-ADD     *ZERO         WB
067300020418 b2  C                   DO        29            WA
067400020418     C     sav_T06(WA)   LOOKUP    L6                                     30
067500020212     C  N30              ADD       1             WB
067600020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
067700020418 e2  C                   ENDDO
067800020418 x1  C                   ELSE
067900020418     C                   MOVEA     sav_T06       L1
068000020418 e1  C                   ENDIF
068100020212     C*
068200020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
068300020212     C                   MOVEA     L1            LIN
068400020212     C                   CLEAR                   L1
068500020212     C                   Z-ADD     *ZERO         WB                3 0
068600020212     C     1             DO        30            WPT               3 0
068700020212     C     LIN(WPT)      IFNE      0
068800020212     C                   CLEAR                   FNLSC4
068900020212     C                   Z-ADD     LIN(WPT)      IC4LIN
069000020212     C                   MOVE      'B'           IC4FLG
069100020212     C                   MOVEL     FNLSC4        KPJBU
069200020212     C                   CALL      'FNLSC4R1'
069300020212     C                   PARM                    KPJBA
069400050802     C                   PARM                    ds1ada
069500020212     C                   MOVEL     KPJBU         FNLSC4
069600020212     C     OC4ESE        IFEQ      'S'
069700020212     C                   ADD       1             WB
069800020212     C                   MOVE      LIN(WPT)      L1(WB)
069900020212     C                   ENDIF
070000020212     C                   ENDIF
070100020212     C                   ENDDO
070200070103     C*
070300070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
070400070103     C                   Z-ADD     *ZERO         WB                3 0
070500070103     C                   DO        29            WA                3 0
070600070103     C     L1(WA)        IFNE      *ZEROS
070700070103     C     L1(WA)        ANDNE     SIMFEL
070800070103     C                   ADD       1             WB
070900070103     C                   Z-ADD     L1(WA)        LV2(WB)
071000070103     C                   ENDIF
071100070103     C                   ENDDO
071200020212     C*
071300020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
071400020212     C*
071500020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
071600020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
071700020212     C* la stampa e' lanciata da menu
071800020212     C                   MOVEL     '3L'          KCOD
071900020212     C                   MOVEL     DS1PGM        KKEY
072000020212     C                   MOVE      DS1FPG        KKEY
072100020212     C     KTAB1         CHAIN     TABEL00F                           30
072200020212     C  N30              MOVEL     TBLUNI        DS3L
072300020212     C   30              MOVEL     'F'           §3LD1L
072400020212     C   30              MOVEL     ' '           §3LD2L
072500020212     C   30              MOVEL     '*STD    '    §3LDST
072600020212     C*
072700020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
072800020212     C                   MOVEL     §3LDST        £MOD             10
072900020212     C                   CLEAR                   LIN
073000020212     C*
073100020212     C* Verifico chi ha lanciato la stampa
073200020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
073300020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
073400020212     C* DS1FGS<>0  --->  si proviene da menu
073500020212     C     DS1FGS        IFEQ      0
073600020212     C*
073700020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
073800020212     C* lancio, voglio la stampa;  invece
073900020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
074000020212     C*
074100020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
074200020212     C* creare stampa per 1°lvl. (se stessa)
074300020212     C     DS1LNP        IFEQ      SIMFEL
074400020212     C*
074500020212     C* Il flag §3LD1L ha i seguenti significati:
074600020212     C*    ' ' = tutti i dati
074700020212     C*    '1' = solo filiale di 1' livello
074800020212     C     §3LD1L        IFEQ      '1'
074900020212     C                   SETON                                        11
075000020212     C                   END
075100020212     C*
075200020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
075300020212     C* nella stampa
075400020212     C*
075500020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
075600020212     C  N11              MOVEA     L1            LIN
075700020212     C   11              MOVEA     SIMFEL        LIN(1)
075800020212     C*
075900020212     C* IMPOSTO LA STAMPANTE:
076000020212     C* STAMPANTE TABULATI
076100020212     C                   MOVEL     '5P'          KCOD
076200020212     C                   MOVEL     'T'           W004A             4
076300020212     C                   MOVE      LIN(1)        W004A
076400020212     C                   MOVEL(P)  W004A         KKEY
076500020212     C     KTAB1         CHAIN     TABEL00F                           99
076600020212     C     *IN99         IFEQ      *OFF
076700020212     C                   MOVEL     TBLUNI        DS5P
076800020212     C                   MOVEL     §5POTQ        £OUTQ            10
076900020212     C                   END
077000020212     C*
077100020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
077200020212     C                   EXSR      LEGGI
077300020212     C*
077400020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
077500020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
077600020212     C                   ELSE
077700020212     C*
077800020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
077900020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
078000020212     C*
078100020212     C* Il significato del fla §3LD2L e' il seguente:
078200020212     C*    ' ' = tutti i dati
078300020212     C*    'F' = per ogni 2' livello
078400020212     C     §3LD2L        IFEQ      'F'
078500020212     C                   SETON                                        14
078600020212     C                   ENDIF
078700020212     C*
078800020212     C                   Z-ADD     1             WB
078900020212     C                   MOVEA     *ZEROS        LIN
079000020212     C  N14              MOVEA     L1            LIN
079100020212     C*
079200020212     C* Lancio per tutti i 2° lvl
079300020212     C     DS1LNP        IFEQ      0
079400020212     C*
079500020212     C* LV2 contiene tutte le filiali di 2° lvl
079600020212     C     LV2(WB)       DOWNE     0
079700020212      *
079800020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
079900020212     C                   CLEAR                   FNLSC4
080000020212     C                   Z-ADD     LV2(WB)       IC4LIN
080100020212     C                   MOVEL     FNLSC4        KPJBU
080200020212     C                   CALL      'FNLSC4R1'
080300020212     C                   PARM                    KPJBA
080400050802     C                   PARM                    ds1ada
080500020212     C                   MOVEL     KPJBU         FNLSC4
080600020212      *
080700020212     C     OC4PT1        IFEQ      'S'
080800020212     C     §3LPT         ANDEQ     'N'
080900020212     C                   GOTO      NOLEGG
081000020212     C                   ENDIF
081100020212      *
081200020212     C     OC4EXP        IFEQ      'S'
081300020212     C     §3LEXP        ANDEQ     'N'
081400020212     C                   GOTO      NOLEGG
081500020212     C                   ENDIF
081600020212      *
081700020212     C     OC4DPD        IFEQ      'S'
081800020212     C     §3LDPD        ANDEQ     'N'
081900020212     C                   GOTO      NOLEGG
082000020212     C                   ENDIF
082100020212      *
082200020212     C     OC4BAR        IFEQ      'S'
082300020212     C     §3LBAR        ANDEQ     'N'
082400020212     C                   GOTO      NOLEGG
082500020212     C                   ENDIF
082600020212      *
082700020212     C     OC4FED        IFEQ      'S'
082800020212     C     §3LFED        ANDEQ     'N'
082900020212     C                   GOTO      NOLEGG
083000020212     C                   ENDIF
083100020212      *
083200020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
083300020212     C*  dati degli altri 2° lvl
083400020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
083500020212     C   14              MOVEL     LV2(WB)       LIN(1)
083600020212     C*
083700020212     C* IMPOSTO LA STAMPANTE
083800020212     C* STAMPANTE TABULATI
083900020212     C                   MOVEL     '5P'          KCOD
084000020212     C                   MOVEL     'T'           W004A             4
084100020212     C                   MOVE      LV2(WB)       W004A
084200020212     C                   MOVEL(P)  W004A         KKEY
084300020212     C     KTAB1         CHAIN     TABEL00F                           99
084400020212     C     *IN99         IFEQ      *OFF
084500020212     C                   MOVEL     TBLUNI        DS5P
084600020212     C                   MOVEL     §5POTQ        £OUTQ            10
084700020212     C                   END
084800020212     C*
084900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
085000020212     C                   EXSR      LEGGI
085100020212     C*
085200020212     C     NOLEGG        TAG
085300020212     C*
085400020212     C                   ADD       1             WB
085500020212     C                   ENDDO
085600020212     C*
085700020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
085800020212     C                   ELSE
085900020212     C*
086000020212     C* Lancio solo per la 2° lvl richiesta
086100020212     C   14              MOVEL     DS1LNP        LIN(1)
086200020212     C*
086300020212     C* STAMPANTE
086400020212     C* STAMPANTE TABULATI
086500020212     C                   MOVEL     '5P'          KCOD
086600020212     C                   MOVEL     'T'           W004A             4
086700020212     C                   MOVE      DS1LNP        W004A
086800020212     C                   MOVEL(P)  W004A         KKEY
086900020212     C     KTAB1         CHAIN     TABEL00F                           99
087000020212     C     *IN99         IFEQ      *OFF
087100020212     C                   MOVEL     TBLUNI        DS5P
087200020212     C                   MOVEL     §5POTQ        £OUTQ            10
087300020212     C                   END
087400020212     C*
087500020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
087600020212     C                   EXSR      LEGGI
087700020212     C*
087800020212     C                   ENDIF
087900020212     C                   ENDIF
088000020212     C*
088100020212     C                   ELSE
088200020212     C*
088300020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
088400020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
088500020212     C     DS1LNP        IFEQ      0
088600020212     C                   MOVEA     L1            LIN
088700020212     C* ...altrimenti elaboro solo se stessa
088800020212     C                   ELSE
088900020212     C                   MOVEL     DS1LNP        LIN(1)
089000020212     C                   ENDIF
089100020212     C*
089200020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
089300020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
089400020212     C* PRENDO QUELLA PRESENTE IN KPJBA
089500020212     C     DS1FGS        IFEQ      SIMFEL
089600020212     C                   MOVEL     KQOTD         £OUTQ
089700030627     C**!!!              MOVEL     KQOTD         £OUTQF
089800020212     C                   ELSE
089900020212     C* STAMPANTE TABULATI
090000020212     C                   MOVEL     '5P'          KCOD
090100020212     C                   MOVEL     'T'           W004A             4
090200020212     C                   MOVE      DS1FGS        W004A
090300020212     C                   MOVEL(P)  W004A         KKEY
090400020212     C     KTAB1         CHAIN     TABEL00F                           99
090500020212     C     *IN99         IFEQ      *OFF
090600020212     C                   MOVEL     TBLUNI        DS5P
090700020212     C                   MOVEL     §5POTQ        £OUTQ            10
090800020212     C                   END
090900020212     C                   ENDIF
091000020212     C*
091100020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
091200020212     C                   EXSR      LEGGI
091300020212     C                   ENDIF
091400020212     C*
091500020212     C                   ENDSR
091600000000      **************************************************************************
091700000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
091800000000      **************************************************************************
091900000000     C     LEGGI         BEGSR
092000940502     C                   SETOFF                                       2729
092100000619     C                   SETON                                        43
092200000616     C                   CLEAR                   SAVRIS
092300970212     C*
092400970212      * Cancello dal file statistiche i record della giornata che vado a elabor.
092500970212      * Solo se richiesta elaborazione
092600970212    1C     DS1RIS        IFNE      'R'
092700000616     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
092800000616     C                   CLEAR                   KPJBU
092900000616     C                   CLEAR                   DSUL06
093000000616     C                   MOVE      '£1'          D06COD
093100000616     C                   MOVEL     SIMFEL        D06KEY
093200050802     C                   MOVE      'S'           D06KEY
093300050802     c                   movel     ds1ada        d06drf
093400000616     C                   MOVEL     DSUL06        KPJBU
093500000616     C                   CALL      'TRUL06R'
093600000616     C                   PARM                    KPJBA
093700000616     C                   MOVEL     KPJBU         DSUL06
093800000616     C**
093900000616     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
094000000616     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
094100000616     C*  CHE USO ALLA FINE
094200090916    2C     DS1RIS        IFNE      ' '
094300000616     C                   MOVEA     LIN           SVLIN
094400000616     C                   MOVEL     DS1RIS        SAVRIS
094500000616     C                   CLEAR                   DS1RIS
094600090916    2C                   ENDIF
094700000616     C*
094800000616     C                   MOVEA     T06           LIN
094900000616     C*
095000090916     C                   OPEN      fisat04L
095100970212     C                   Z-ADD     1             XX                3 0
095200970212    2C     LIN(XX)       DOWGT     *ZERO
095300970212     C*
095400090916     C     fisatK        SETLL     fisat04L
095500090916     C     fisatK        READE     fisat04L                               99
095600970212     C*
095700970212    3C     *IN99         DOWEQ     *OFF
095800090916     C                   DELETE    fisat004
095900090916     C     fisatK        READE     fisat04L                               99
096000970212    3C                   ENDDO
096100970212     C                   ADD       1             XX
096200970212    2C                   ENDDO
096300970212     C*
096400090916     C                   CLOSE     fisat04L
096500090916    1C                   END
096600940505      *
096700940505      * Come prima cosa devo verificare se giornata festiva
096800940505      *   solo per ELABORAZIONE o  per stampe giornaliere
096900090916    1C     DS1FGS        IFEQ      0
097000950210     C     DS1RIS        ORNE      'R'
097100940505     C                   CLEAR                   $TFA
097200940505      *
097300950118     C     AZCLNK        CHAIN     AZCLN01L                           99
097400090916    2C     *IN99         IFEQ      *OFF
097500950118     C                   MOVEA     CLNMAT        GME
097600090916    3C     GME($GI)      IFEQ      'F'
097700090916     C* SE FESTIVO SCRIVO RECORD IN fisat CON TRE = F
097800090916     C                   CLEAR                   fisat000
097900970211     C                   Z-ADD     SIMFEL        SATFLE
098000970211     C                   MOVEL     'F'           SATTRE
098100970211     C                   Z-ADD     DS1ADA        SATDRE
098200040922     c                   movel     $ftr          satftr
098300040922     c                   z-add     $DTR          satdtr
098400090916     C                   WRITE     fisat000                             99
098500970211     C                   GOTO      ENDLEG
098600090916    3C                   ENDIF
098700090916    2C                   ENDIF
098800090916    1C                   ENDIF
098900940429      *
099000940429     C* LANCIO DA FILTRO
099100950210    1C     DS1FGS        IFGT      0
099200940429     C* 29 ON - IMMESSA LINEA DI PARTENZA
099300950210    2C     DS1LNP        IFGT      0
099400940429     C                   SETON                                        29
099500950210    3C     DS1LNP        IFNE      DS1FEL
099600940502     C                   SETON                                        27
099700940429    3C                   ENDIF
099800940429    2C                   ENDIF
099900940429     C*
100000940429   X1C                   ELSE
100100940502     C                   SETON                                        05
100200940506     C                   CLEAR                   STRIP
100300940429     C* STAMPE GIORNALIERE
100400940429    2C     LIN(2)        IFEQ      0
100500940429     C                   SETON                                        29
100600940429    2C                   ENDIF
100700940429     C     SIMFEL        LOOKUP    LIN                                    99
100800940502     C  N99              SETON                                        27
100900940429    1C                   ENDIF
101000000000      *
101100000000      * Scrivo, in stampa, se selezionata filiale o area
101200000000     C                   MOVE      *BLANKS       STRIP1
101300090916    1C  N05DS1LNP        IFNE      *ZERO
101400960925     C                   MOVEL     'P.O.'        STRIP1
101500950210     C                   MOVE      DS1LNP        STRIP1
101600000000     C                   ELSE
101700940502     C                   MOVEL     'AREA'        STRIP1
101800950210     C                   MOVE      DS1FEL        STRIP1
101900090916    1C                   ENDIF
102000000000      *
102100000000      * Inizializzo i campi per l'elaborazione
102200000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
102300000000     C                   Z-ADD     *ZERO         $FILPR
102400000000     C                   MOVE      *BLANKS       $CBLPR
102500000000     C                   Z-ADD     *ZERO         $NFVPR
102600940509     C                   CLEAR                   $RIGHE
102700950210      *
102800950210      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
102900950210     C                   MOVEA     £MOD          CMD(33)
103000950210     C                   MOVEA     £OUTQ         CMD(51)
103100950210      *
103200940502      * Eseguo OVRPRTF sul file di stampa
103300950210     C                   Z-ADD     82            LUNG             15 5
103400950213     C                   MOVEL     *BLANKS       COMMAN           82
103500940502     C                   MOVEA     CMD           COMMAN
103600940502     C                   CALL      'QCMDEXC'                            H1
103700940502     C                   PARM                    COMMAN
103800940502     C                   PARM                    LUNG
103900970807     C  N38              OPEN      FNLSC2P
104000950118     C  N38              WRITE     LSC2T1
104100950118     C  N38              WRITE     LSC2T3
104200940502     C  N38              SETOFF                                       30
104300000619     C**
104400000619     C* SE DS1RIS= 'S' ACCENDO PER ORA IL 38
104500090916    1C     SAVRIS        IFNE      ' '
104600000619     C                   SETON                                        38
104700090916    1C                   ENDIF
104800000000      *
104900950210      * L'elaborazione solo se non scelta la ristampa (DS1RIS='R')
105000950210    1C     DS1RIS        IFNE      'R'
105100000000      *
105200970205      * Leggo il file delle bolle in partenza prima quelli con data
105300970205      * F.V. = 0 poi quelli con DFV >= della data elaborazione
105400940401     C                   Z-ADD     1             $X                3 0
105500940428     C                   MOVE      *BLANK        $FLAG
105600940428     C*
105700940428    2C     LIN($X)       DOWGT     *ZERO
105800000912     C                   Z-ADD     LIN($X)       $FILPR
105900940428     C                   CLEAR                   $DFV
106000940428     C                   SETOFF                                           01
106100940428      *
106200940428    3C                   DO        2
106300021023     C     FNBLTK        SETLL     FNBLT38L
106400021023     C  N01FNBLTK        READE     FNBLT38L                               33
106500021023     C   01LIN($X)       READE     FNBLT38L                               33
106600940428     C*
106700940428    4C     *IN33         DOWEQ     *OFF
106800000000      *
106900000000      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
107000000000      * come test ma mi permette di saltare delle chain)
107100940428    5C     $BLTAT        IFNE      $BLTPR
107200950118     C     FNBLPK        CHAIN     FNBLP01L                           99
107300000000     C   99              GOTO      LETTUR
107400000000     C                   MOVE      $BLTAT        $BLTPR
107500000000     C                   Z-ADD     BLPNFV        $NFVPR
107600950118     C                   Z-ADD     BLPMGS        $DATSP            8 0
107700000000     C                   MOVEL     BLPAAS        $DATSP
107800000331     C                   Z-ADD     BLPDRT        $DTRIT
107900000413     C                   Z-ADD     BLPPDR        $CDPDR
108000940428     C* $BOLLA = 1 INDICA CHE HO LETTO UNA NUOVA BOLLA
108100940428     C                   MOVEL     '1'           $BOLLA            1
108200000000      *
108300000000      * Decodifico il tipo bolla
108400950118    6C     BLPCBO        IFNE      $CBLPR
108500950210     C                   MOVE      '3A'          KCOD
108600950210     C                   MOVE      *BLANKS       KKEY
108700950210     C                   MOVEL     BLPCBO        KKEY
108800950210     C     KTAB1         CHAIN     TABEL00F                           99
108900000000     C  N99              MOVEL     TBLUNI        DS3A
109000950118     C  N99              MOVE      BLPCBO        $CBLPR
109100000000     C   99              CLEAR                   DS3A
109200940428    6C                   ENDIF
109300940427     C*
109400940428    5C                   ENDIF
109500000000      *
109600940428      * ESCLUSIONI BOLLE
109700000000     C                   EXSR      ESCLUS
109800940428    5C     $OK           IFEQ      'S'
109900000000     C                   EXSR      ELABOR
110000940428    5C                   ENDIF
110100000000      *
110200940428     C     LETTUR        TAG
110300021023     C  N01FNBLTK        READE     FNBLT38L                               33
110400021023     C   01LIN($X)       READE     FNBLT38L                               33
110500940428    4C                   ENDDO
110600940428      *
110700940428     C                   SETON                                        01
110800970205     C                   Z-ADD     DS1AAL        $DFV
110900940428    3C                   ENDDO
111000940428      *
111100940428      * cambia la filiale --> decodifico e stampo i totali
111200940428     C                   EXSR      CALSPE
111300940428     C                   EXSR      WRISAT
111400940429     C                   EXSR      WR1SAT
111500940428     C  N38              EXSR      TOTFIL
111600940428     C                   EXSR      AZZFIL
111700940428     C*
111800940401     C                   ADD       1             $X
111900090916    2C                   ENDDO
112000940429      *
112100940429     C*
112200940429     C* STAMPA DEI DATI FINALI
112300940506     C                   EXSR      STAFIN
112400090916    1C                   ENDIF
112500000616      *
112600000616     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
112700090916    1C     SAVRIS        IFEQ      'S'
112800000616     C                   MOVEL     'R'           DS1RIS
112900000616     C                   MOVEA     SVLIN         LIN
113000000616     C* AZZERO LE SCHIERE DI TOTALE
113100000616     C                   EXSR      AZZSER
113200000616     C                   EXSR      AZZTRA
113300000616     C                   EXSR      AZZTOT
113400000619     C                   SETOFF                                       38
113500090916    1C                   ENDIF
113600000000      *
113700090916      * Nel caso di RISTAMPA mi limito a rileggere il file statistiche (fisat)
113800000000      * e stampare i record che rientrano nell'intervallo dal-al
113900090916    1C     DS1RIS        IFNE      ' '
114000000000     C                   EXSR      RISTAM
114100940428    1C                   ENDIF
114200000000      *
114300000000      * Stampa terminata
114400090916    1C     *IN38         IFEQ      *OFF
114500940429     C*
114600090916    2C     $ALMEN        IFEQ      *ZERO
114700950118     C                   WRITE     LSC2E1
114800940505     C                   ELSE
114900940429     C*
115000000000     C                   EXSR      STATER
115100090916    2C                   ENDIF
115200000000      *
115300950118     C                   WRITE     LSC2E2
115400950629     C                   WRITE     LSC2E5
115500000000      *
115600000000      * Verifico se vi sono fogli viaggio aperti
115700000000     C                   EXSR      FOGVIA
115800000000      *
115900970807     C                   CLOSE     FNLSC2P
116000090916    1C                   ENDIF
116100000000      *
116200000000      * Aggiorno la data ultima elaborazione
116300090916    1C     DS1RIS        IFNE      'R'
116400000616     C     SAVRIS        OREQ      'S'
116500021016
116600021016     c                   Eval      kTbeCod = 'SDF'
116700021016     c                   Movel(p)  Simfel        kTbeKe1
116800021016     c     Ktbe          Chain     Tntbe01l
116900090916    2c                   If        %Found(Tntbe01l)
117000021016     c                   Movel     TbeUni        dSdf
117100021016     c                   If        §SdfTerP = *Zeros or §SdfTerP > Ds1Ada
117200021016     c                   Z-Add     Ds1Ada        §SdfTerP
117300021016     c                   EndIf
117400021016     c                   If        §SdfTerU < Ds1Ada
117500021016     c                   Z-Add     Ds1Ada        §SdfTerU
117600021016     c                   EndIf
117700021016     c                   Movel     dSdf          TbeUni
117800021016     c                   Update    Tntbe000
117900090916    2c                   EndIf
118000090916    1C                   ENDIF
118100090415     c
118200090415     c* Se non ho aggioranto la tabella, vedo se devo cancellare record
118300090415     c*  in FISTA
118400090916    1c                   if        savris='S'
118500090415     c     ksta          delete    fista01l
118600090916    1C                   ENDIF
118700000912     C*
118800000000      *
118900940505     C     ENDLEG        ENDSR
119000000000      **************************************************************************
119100000000      * Una volta letto il record verifico se da includere o no
119200940427      * PER QUALSIASI TIPO DI CALCOLO  (COLLI E BOLLE A TERRA E
119300940427      *  COLLI E BOLLA DA PARTIRE)
119400000000      *
119500940427      * Vi sono solo due motivi per escludere il record letto:
119600940427      * (1) se la spedizione e' stata bollettata dopo la data elaboraz
119700950210      *                Data spedizione > DS1AAL
119800000000      *
119900940427      * (2) se la data partenza del collo e' < della data elaborazione
120000950210      *                BLTDFV < DS1AAL
120100000000      *
120200000810      * ATTENZIONE.. DA OGGI C'E' UN MOTIVO IN PIU'.. Canale5 ! da cantare ..
120300000810      *
120400000810      * (3) se la linea di partenze è ??..... POSTE !
120500000810      *
120600940427      * Bolle di recupero / bolle legate / bolle "£" sono escluse
120700000000      **************************************************************************
120800000000     C     ESCLUS        BEGSR
120900000000      *
121000000000     C                   MOVE      'N'           $OK               1
121100000331     C                   MOVE      *BLANKS       WSOS              1
121200000810      *
121300000810      *
121400940427      * esclusioni:
121500950323      * Le bolle "legate" sono identificabili dal nr.foglio viaggio
121600950323      * = 99999 O = 99998
121700070522    1C     $NFVPR        IFEQ      99999
121800950323     C     $NFVPR        OREQ      99998
121900940427     C     $CBLPR        OREQ      '£'
122000940428     C     §3ARBL        OREQ      'R'
122100000000     C                   GOTO      ENDESC
122200070522    1C                   ENDIF
122300000413     C* Prepagati non confermati
122400070522    1C     $CDPDR        IFEQ      *ZEROS
122500000413     C                   GOTO      ENDESC
122600070522    1C                   ENDIF
122700000000      *
122800070522    1C     BLTDFV        IFEQ      DS1AAL
122900950629     C                   MOVEL     'S'           $OK
123000070522   x1C                   ELSE
123100070522     C
123200070522     c* Elaborazione in base A     Data BORDERÒ
123300070522   1AC                   IF        DS1AAL<§VPOnpi
123400070522     C
123500940427      * La data di spedizione deve essere <= alla data elaborazione
123600070522    2C     $DATSP        IFLE      DS1AAL
123700000000      *
123800940427      * La data partenza deve essere > = alaa data elaborazione        limiti
123900070522    3C     BLTDFV        IFEQ      *ZERO
124000950210     C     BLTDFV        ORGE      DS1AAL
124100000000     C                   MOVE      'S'           $OK
124200000000     C                   GOTO      ENDESC
124300070522    3C                   ENDIF
124400070522   X2C                   ELSE
124500000331     C* Se data spedizione > di data elaborazione
124600000411     C* e  data ritiro    <= data elaborazione    ==> BOLLA IN SOSPENS.
124700070522    3C     $DTRIT        IFLE      DS1AAL
124800000331     C                   MOVE      'S'           $OK
124900000331     C                   MOVEL     'S'           WSOS
125000070522     C                   GOTO      ENDESC
125100070522    3C                   ENDIF
125200950626     C*
125300070522    2C                   ENDIF
125400070522     c
125500070522  x1ac                   else
125600070522     c****
125700070522     c* ELAborazione in base a    data RITIRO Merce
125800070522      * La data ritiro deve essere <= alla data elaborazione
125900070522    2C     $DTRIT        IFLE      DS1AAL
126000070522      *
126100070525      * La data partenza deve essere > = alla data elaborazione        limiti
126200070522    3C     BLTDFV        IFEQ      *ZERO
126300070522     C     BLTDFV        ORGE      DS1AAL
126400070522     C                   MOVE      'S'           $OK
126500070522     C                   GOTO      ENDESC
126600070522    3C                   ENDIF
126700070522    2C                   ENDIF
126800070522   1aC                   ENDIF
126900950629     C*
127000070522    1C                   ENDIF
127100000000      *
127200000000     C     ENDESC        ENDSR
127300000000      **************************************************************************
127400000000      * Elaborazioni dei record letti, somme nei totali
127500000000      **************************************************************************
127600000000     C     ELABOR        BEGSR
127700000000      *
127800090916      * Verifico il tipo servizio
127900090916     C                   Z-ADD     5             $T                1 0
128000090916     c                   select
128100090916     C     BLPTSP        wheneq    'E'
128200000000     C                   Z-ADD     1             $T
128300090916     C
128400090916     C     BLPTSP        wheneq    'D'
128500940323     C                   Z-ADD     2             $T
128600090916
128700090916     C     BLPTSP        wheneq    'C'
128800090916     C                   Z-ADD     3             $T
128900090916
129000090916     C     BLPTSP        wheneq    'H'
129100090916     C                   Z-ADD     4             $T
129200090916     c                   endsl
129300000000      *
129400000000      * C/servizio oppure no ?
129500940428     C     §3ARBL        COMP      'C'                                3434
129600000000      *
129700970205     C*
129800070522    1C     WSOS          IFEQ      *BLANKS
129900970205     C* DETERMINAZIONE SE COLLO A TERRA
130000000000      * Ho due controlli da fare per sapere se il collo e' a terra:
130100000000      *    1)  BLTDFV = 0
130200950210      *    2)  BLTDFV > Data spedizione   AND   BLTDFV > DS1AAL
130300000000     C                   SETOFF                                       36
130400070522    2C     BLTDFV        IFEQ      *ZERO
130500950210     C     BLTDFV        ORGT      DS1AAL
130600000000     C                   SETON                                        36
130700070522    2C                   ENDIF
130800970303     C*
130900970303     C* RICHIAMO PGM PER CALCOLO AFFIDABILITA'
131000970303     C* (SOLO SE HO LETTO UNA NUOVA BOLLA - $BOLLA = '1')
131100070522    2C     $BOLLA        IFEQ      '1'
131200070522    3C     BLPDCE        IFEQ      *ZEROS
131300970303     C     BLPDUC        ORNE      DS1AAL
131400970303     C                   MOVEL     ' '           D98TLA
131500970303     C                   MOVEL     'P'           D98TBO
131600970303     C                   MOVEL     BLPAAS        D98AAS
131700970303     C                   MOVEL     BLPLNP        D98LNP
131800970303     C                   MOVEL     BLPNRS        D98NRS
131900970303     C                   MOVEL     BLPNSP        D98NSP
132000970310     C                   Z-ADD     DS1AAL        D98DRF
132100970310     C                   Z-ADD     2359          D98ORF
132200970925     C                   CALL      'TNSD99R'
132300970925     C                   PARM                    DS99
132400970303     C                   ELSE
132500970303     C                   Z-ADD     BLPNPI        D98NPI
132600070522    3C                   END
132700070522    2C                   END
132800970207     C*
132900970207     C*VERIFICO SE SI TRATTA DI "A TERRA DA PARTIRE"
133000970210     C* La considero "non da partire" se si tratta di:
133100970207     C* - spedizione non borderizzata
133200970303     C* - distribuzione a terra del giorno
133300070522     c* - dal 1° giugno 2007 il calcolo della affidabilità a terra sarà
133400070522     c*   in base alla data ritiro, per cui nel DA PARTIRE non controllo
133500070522     c*   più la data borderò ma solo se si tratta della distribuzione
133600070522     c*   del giorno
133700970205     C                   SETOFF                                       35
133800070522     c
133900070522    2C     *IN36         IFEQ      *ON
134000070522     c
134100070522    3c                   if        ds1aal<§vponpi
134200970210     C*
134300070522    4C     BLPDBR        IFGT      DS1AAL
134400070522     C     BLPDBR        OREQ      *ZEROS
134500070522     C                   SETON                                        35
134600070522   x4C                   ELSE
134700970304     C*
134800070522    5C     BLPDBR        IFLE      DS1AAL
134900070522     C     BLPDPC        ANDEQ     0
135000070522     C     D98NPI        ANDLT     0
135100070522     C*
135200070522     C     BLPDBR        ORLE      DS1AAL
135300070522     C     BLPDPC        ANDGT     DS1AAL
135400070522     C     D98NPI        ANDLT     0
135500070522     C                   SETON                                        35
135600070522    5C                   END
135700070522    4C                   END
135800070522     c
135900070522   x3c                   else
136000070522     c*
136100090108     C* SE affidabilità =0 verifico se cade in giorno festivo, per
136200090108     c*    ter-ter --> se si non do ritardo
136300090108     c                   if        d98npi=0
136400090108     C                   movel     blptfa        $TFA
136500090108      *
136600090108     C     AZCLNK        CHAIN     AZCLN01L                           99
136700090108     C                   MOVEA     CLNMAT        GME
136800090108     C                   MOVEA     CLNpom        GMEp
136900090108     C     GME($GI)      IFEQ      'F'
137000090108     C     GMEp($GI)     oreq      'F'
137100090108     c                   sub       24            d98npi
137200090108     c                   endif
137300090108     c                   endif
137400090108     c
137500070522    4c                   if        (blpdpc=0 and d98npi<0) or
137600070522     c                             (blpdpc>ds1aal and d98npi<0)
137700070522     C                   SETON                                        35
137800070522    4c                   endif
137900070522    3c                   endif
138000970210     C*
138100070522    2C                   END
138200070522    1C                   ENDIF
138300970205     C*
138400000000      *
138500970205      * Aggancio, nel file statistiche, il record per il terminal di
138600970205      * arrivo poiche' lo dovrò aggiornare
138700090916     C                   CLEAR                   fisat000
138800000000     C                   Z-ADD     SIMFEL        SATFLE
138900950210     C                   Z-ADD     DS1AAL        SATDRE
139000940428     C                   Z-ADD     BLPLNP        SATLNP
139100000000     C                   Z-ADD     BLPTFA        SATTFA
139200000000     C                   MOVE      *BLANK        SATTRE
139300090916     C     FNSA1K        CHAIN     fisat02L                           37
139400940428      *
139500970205      * NUOVA BOLLA LETTA: CONTO LE SPEDIZIONI
139600940428      *
139700940428     C     $BOLLA        IFEQ      '1'
139800940428     C                   EXSR      DATBOL
139900970205     C                   ENDIF
140000021217      *
140100021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
140200030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
140300030213      * Peso medio
140400030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
140500030213if  1c***                if        Wsipes = 'S'
140600030213     c***                z-add     BLTpuc        $Peso
140700030213   x1c***                else
140800021220     c                   z-add     Wpesmed       $Peso
140900030213e   1c***                endif
141000021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
141100030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
141200030213      * Volume medio
141300030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
141400030213if  1c***                if        Wsivol = 'S'
141500030213     c***                z-add     BLTvuc        $Volum
141600030213   x1c***                else
141700021220     c                   z-add     Wvolmed       $Volum
141800030213e   1c***                endif
141900000000      *
142000000000      * ?  CALCOLI  SUI  COLLI  ?
142100000000      *
142200000331     C     WSOS          IFEQ      *BLANKS
142300000000      * Nr.colli da partire (servira' per la percentuale)
142400970207     C     *IN35         IFEQ      *OFF
142500970210     C   34              ADD       1             CPA($T)
142600970210     C  N34              ADD       1             C4A($T)
142700970210     C                   ADD       1             CDT($T)
142800950626     C                   END
142900000000      *
143000940427     C     *IN36         IFEQ      *ON
143100970207      * A terra da partire                                             (35ON)
143200940427     C   34
143300940427     CANN35              ADD       1             CBP($T)
143400940427     C  N34
143500940427     CANN35              ADD       1             C4P($T)
143600970207     C  N35              ADD       1             C0P($T)
143700970207     C  N35              ADD       1             C3P($T)
143800970207      * A terra non da partire                                         (35ON)
143900940427     C   34
144000940427     CAN 35              ADD       1             CBR($T)
144100940427     C  N34
144200940427     CAN 35              ADD       1             C4R($T)
144300940427     C   35              ADD       1             C0R($T)
144400940427     C                   ENDIF
144500000000      *
144600000000      * ?  CALCOLI  SUL  PESO  ?
144700000000      *
144800970207     C     *IN35         IFEQ      *OFF
144900970210     C   34              ADD       $PESO         PPA($T)
145000970210     C  N34              ADD       $PESO         P4A($T)
145100970210     C                   ADD       $PESO         PDT($T)
145200950626     C                   END
145300940502      * A TERRA
145400940427     C     *IN36         IFEQ      *ON
145500940502      *
145600970206      * Fino a 35 quintali
145700021217     ***C     BLPPKF        IFLE      3500
145800021217     C     Wpeso         IFLE      3500
145900970207     C* Da partire
146000970206     C   34
146100970206     CANN35              ADD       $PESO         PFI($T)
146200970206     C  N34
146300970206     CANN35              ADD       $PESO         P4I($T)
146400970206     C  N35              ADD       $PESO         P3I($T)
146500970206     C  N35              ADD       $PESO         P0I($T)
146600970207     C* Non da partire
146700970206     C   34
146800970206     CAN 35              ADD       $PESO         PFN($T)
146900970206     C  N34
147000970206     CAN 35              ADD       $PESO         P4N($T)
147100970206     C   35              ADD       $PESO         P0N($T)
147200970206     C                   ELSE
147300970206      * Oltre 35 quintali:                                             (35ON)
147400970207     C* Da partire
147500940427     C   34
147600940427     CANN35              ADD       $PESO         PBP($T)
147700940427     C  N34
147800940427     CANN35              ADD       $PESO         P4P($T)
147900970206     C  N35              ADD       $PESO         P0P($T)
148000970206     C  N35              ADD       $PESO         P3P($T)
148100970207     C* Non da partire
148200940427     C   34
148300940427     CAN 35              ADD       $PESO         PBR($T)
148400940427     C  N34
148500940427     CAN 35              ADD       $PESO         P4R($T)
148600940427     C   35              ADD       $PESO         P0R($T)
148700940427     C                   ENDIF
148800970206     C                   ENDIF
148900000331     C*
149000000331     C                   ENDIF
149100000000      *
149200000000      * ?  CALCOLI  SUL  VOLUME  ?
149300940329      *
149400000000      *
149500021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
149600021217     ***C     BLTVUC        IFEQ      *ZERO
149700021217     *** *
149800021217     ***C     BLPVLF        DIV(H)    BLPNCL        $VOLUM           12 4
149900021217     *** *
150000021217     ***C                   ELSE
150100021217     ***C                   Z-ADD     BLTVUC        $VOLUM
150200021217     ***C                   ENDIF
150300000000      *
150400000331     C     WSOS          IFEQ      *BLANKS
150500970207     C     *IN35         IFEQ      *OFF
150600970210     C   34              ADD       $VOLUM        VPA($T)
150700970210     C  N34              ADD       $VOLUM        V4A($T)
150800970210     C                   ADD       $VOLUM        VDT($T)
150900950626     C                   END
151000940505      *
151100940428     C     *IN36         IFEQ      *ON
151200970207     C* Da partire
151300940428     C   34
151400940428     CANN35              ADD       $VOLUM        VBP($T)
151500940428     C  N34
151600940428     CANN35              ADD       $VOLUM        V4P($T)
151700970206     C  N35              ADD       $VOLUM        V0P($T)
151800970206     C  N35              ADD       $VOLUM        V3P($T)
151900000331     c* Sia che sia da partire che non da partire: sommo nel totale
152000000331     C* Mc rimasti a magazzino
152100000331     C   34              ADD       $VOLUM        VBR($T)
152200000331     C  N34              ADD       $VOLUM        V4R($T)
152300000331     C                   ADD       $VOLUM        V0R($T)
152400000406     C                   ADD       $VOLUM        V3R($T)
152500000000      *
152600000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
152700000000      *
152800970205      * Visto che le spedizioni non riesco a calcolarle ora, mi limito
152900970205      * a tenere aggiornato il file di work con le bolle
153000940428     C   34              MOVE      *BLANK        WRKCSE
153100940428     C  N34              MOVE      'C'           WRKCSE
153200940503     C                   Z-ADD     BLPLNP        WRKLNP
153300950118     C     FNWR1K        CHAIN     FNWRK00F                           99
153400940428     C     *IN99         IFEQ      *OFF
153500000000     C                   ADD       1             WRKNC1
153600000000     C                   EXCEPT    AGGIO
153700000000     C                   ELSE
153800950118     C                   MOVEL     *BLANKS       WTRANS            1
153900940503     C                   EXSR      WRIWRK
154000000000     C                   ENDIF
154100940428     C                   ENDIF
154200000331     c* Se collo di bolla in sospensione sommo volume in totale mc
154300000331     c* rimasti a magazzino
154400000331     C                   ELSE
154500000331     C   34              ADD       $VOLUM        VBR($T)
154600000331     C  N34              ADD       $VOLUM        V4R($T)
154700000331     C                   ADD       $VOLUM        V0R($T)
154800000406     C                   ADD       $VOLUM        V3R($T)
154900000412     C                   ADD       $VOLUM        VOLUME            9 4
155000000331     C                   ENDIF
155100940323      *
155200040921     C                   MOVE      $FTR          SATFTR
155300040921     C                   MOVE      $DTR          SATDTR
155400940428     C     *IN37         IFEQ      *ON
155500040921     C****               MOVE      $FTR          SATFTR
155600040921     C****               MOVE      $DTR          SATDTR
155700090916     C                   WRITE     fisat000
155800940428     C                   ELSE
155900090916     C                   UPDATE    fisat000
156000940428     C                   ENDIF
156100000000      *
156200000000     C                   ENDSR
156300940428      **************************************************************************
156400940428      * Carico i dati della bolla (peso e volume)
156500940428      *  e conto le spedizioni partite il giorno di elaborazione
156600940428      **************************************************************************
156700940428     C     DATBOL        BEGSR
156800940428      *
156900021217     *** * Calcolo il peso di un singolo collo
157000021217     ***C     BLPPKF        DIV(H)    BLPNCL        $PESO             8 1
157100021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
157200021217     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
157300021217     *** * BLPVLC=Volume rilevato)
157400050112     ***C     BLPNCR        ifeq      BLPNCL
157500021217     ***C                   Z-ADD     BLPVLC        BLPVLF
157600021217     ***C                   ELSE
157700021217     ***C     BLPVLC        IFGE      BLPVLF
157800021217     ***C                   Z-ADD     BLPVLC        BLPVLF
157900021217     ***C                   ENDIF
158000021217     ***C                   ENDIF
158100021217      *
158200021217      * Verifico i dati misurati con la macchina smistacolli
158300021217      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
158400021220      * e calcolo il VOLUME unitario medio per collo da usare se non
158500021220      * tutti i colli sono stati volumati dalla smistacolli
158600021217      *
158700030213      **** Verifico se sono stati volumati tutti i colli
158800030213     c***                clear                   Wsivol
158900021217     c                   clear                   Wvolu
159000050112     c                   if        BLPncr = BLPncl
159100021217     c                   z-add     BLPvlc        Wvolu
159200030213     c***                eval      Wsivol =  'S'
159300021217   x1c                   else
159400021217if  2c                   if        BLPvlc >= BLPvlf
159500021217     c                   z-add     BLPvlc        Wvolu
159600021217   x2c                   else
159700021217     c                   z-add     BLPvlf        Wvolu
159800021217e   2c                   endif
159900021217e   1c                   endif
160000021220      *
160100021220      * Calcolo il volume medio
160200021220     c     Wvolu         div(H)    BLPncl        Wvolmed
160300021217      *
160400021217      * Verifico i dati misurati con la macchina smistacolli
160500021217      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
160600021220      * e calcolo il PESO   unitario medio per collo da usare se non
160700021220      * tutti i colli sono stati pesati   dalla smistacolli
160800021217      *
160900030213      **** Verifico se sono stati pesati   tutti i colli
161000030213     c***                clear                   Wsipes
161100021217     c                   clear                   Wpeso
161200050112     c                   If        BLPncp = BLPncl
161300021217     c                   z-add     BLPpkc        Wpeso
161400030213     c***                eval      Wsipes =  'S'
161500021217   c1c                   else
161600021217if  2c                   if        BLPpkc >= BLPpkf
161700021217     c                   z-add     BLPpkc        Wpeso
161800021217   x2c                   else
161900021217     c                   z-add     BLPpkf        Wpeso
162000021217e   2c                   endif
162100021217e   1c                   endif
162200021220      *
162300021220      * Calcolo il peso medio
162400021220     c     Wpeso         div(H)    BLPncl        Wpesmed
162500940428     C*
162600940428     C* SOMMO SOLTANTO LE SPEDIZIONI PARTITE NEL GIORNO
162700970205     C*  LA DATA DI RIFERIMENTO E' LA DATA ULTIMO COLLO
162800940506     C     $FLAG         IFEQ      ' '
162900970205     C     BLPDUC        ANDEQ     DS1AAL
163000000331     C     WSOS          ANDEQ     *BLANKS
163100950118     C                   MOVEL     *BLANKS       WTRANS
163200940503     C                   EXSR      CTRBOL
163300970205     C                   ENDIF
163400940428     C*
163500940428     C                   MOVEL     '2'           $BOLLA
163600940428     C                   ENDSR
163700950118      **************************************************************************
163800021217      * Carico i dati della bolla (peso e volume) (TRANSITI)
163900021217     *** *  e conto le spedizioni partite il giorno di elaborazione
164000021217     *** *    (questo NON lo faceva NEANCHE prima)
164100950118      **************************************************************************
164200950118     C     DATBOT        BEGSR
164300950118      *
164400021217     *** * Calcolo il peso di un singolo collo
164500021217     ***C     BTPPKB        DIV(H)    BTPNCL        $PESO             8 1
164600021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
164700021217     *** * misurati con la macchina smistacolli (BTPNCR=Numero colli rilevati,
164800021217     *** * BTPVLC=Volume rilevato)
164900050112     ***C     BTPNCR        ifeq      BTPNCL
165000021217     ***C                   Z-ADD     BTPVLC        BTPVLB
165100021217     ***C                   ELSE
165200021217     ***C     BTPVLC        IFGE      BTPVLB
165300021217     ***C                   Z-ADD     BTPVLC        BTPVLB
165400021217     ***C                   ENDIF
165500021217     ***C                   ENDIF
165600021217     ***C*
165700021220      *
165800021220      * Verifico i dati misurati con la macchina smistacolli
165900021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
166000021220      * e calcolo il VOLUME unitario medio per collo da usare se non
166100021220      * tutti i colli sono stati volumati dalla smistacolli
166200021217      *
166300030213      **** Verifico se sono stati volumati tutti i colli
166400030213     c***                clear                   Wsivol
166500021217     c                   clear                   Wvolu
166600050112     c                   if        BTPncr = BTPncl
166700021217     c                   z-add     BTPvlc        Wvolu
166800030213     c***                eval      Wsivol =  'S'
166900021217   x1c                   else
167000021217if  2c                   if        BTPvlc >= BTPvlb
167100021217     c                   z-add     BTPvlc        Wvolu
167200021217   x2c                   else
167300021217     c                   z-add     BTPvlb        Wvolu
167400021217e   2c                   endif
167500021217e   1c                   endif
167600021220      *
167700021220      * Calcolo il volume medio
167800021220     c     Wvolu         div(H)    BTPncl        Wvolmed
167900021220      *
168000021220      * Verifico i dati misurati con la macchina smistacolli
168100021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
168200021220      * e calcolo il PESO   unitario medio per collo da usare se non
168300021220      * tutti i colli sono stati pesati   dalla smistacolli
168400021217      *
168500030213      **** Verifico se sono stati pesati   tutti i colli
168600030213     c***                clear                   Wsipes
168700021217     c                   clear                   Wpeso
168800050112     c                   if        BTPncp = BTPncl
168900021217     c                   z-add     BTPpkc        Wpeso
169000030213     c***                eval      Wsipes =  'S'
169100021217   x1c                   else
169200021217if  2c                   if        BTPpkc >= BTPpkb
169300021217     c                   z-add     BTPpkc        Wpeso
169400021217   x2c                   else
169500021217     c                   z-add     BTPpkb        Wpeso
169600021217e   2c                   endif
169700021217e   1c                   endif
169800021220      *
169900021220      * Calcolo il peso medio
170000071231     c     Wpeso         div(H)    BTPncl        Wpesmed
170100021217      *
170200950118     C                   MOVEL     '2'           $BOLLA
170300950118     C                   ENDSR
170400940503      **************************************************************************
170500950118      * Scrivo il file FNWRK00F
170600940503      **************************************************************************
170700940503     C     WRIWRK        BEGSR
170800950118     C*
170900940503     C                   MOVE      $FLAG         WRKFLG
171000950118     C                   Z-ADD     1             WRKNC1
171100950118     C*
171200950118     C     WTRANS        IFNE      'T'
171300940503     C                   Z-ADD     BLPAAS        WRKAAS
171400940503     C                   Z-ADD     BLPMGS        WRKMGS
171500940503     C                   Z-ADD     BLPNRS        WRKNRS
171600940503     C                   Z-ADD     BLPNSP        WRKNSP
171700940503     C                   Z-ADD     BLPLNA        WRKLNA
171800940503     C                   MOVE      BLPTSP        WRKTSP
171900940503     C                   Z-ADD     BLPNCL        WRKNCL
172000940503     C                   Z-ADD     BLPTFA        WRKTFA
172100070525     c* Se dopo la data di attivazione, memorizzo la data borderò
172200070525    3c                   if        ds1aal<§vponpi
172300970207     C                   Z-ADD     BLPDBR        WRKDBR
172400070525     c                   else
172500070525     C                   Z-ADD     BLPDrt        WRKDBR
172600070525     c                   endif
172700070525     c
172800970303     C                   Z-ADD     BLPDPC        WRKDPC
172900970303     C                   Z-ADD     BLPDUC        WRKDUC
173000970303     C                   Z-ADD     D98NPI        WRKNPI
173100950118     C*
173200950118     C                   ELSE
173300950118     C*
173400950118     C                   Z-ADD     BTPAAS        WRKAAS
173500950118     C                   Z-ADD     BTPMGS        WRKMGS
173600950118     C                   Z-ADD     BTPNRS        WRKNRS
173700950118     C                   Z-ADD     BTPNSP        WRKNSP
173800950118     C                   Z-ADD     BTPLNA        WRKLNA
173900950118     C                   MOVE      BTPTSP        WRKTSP
174000950118     C                   Z-ADD     BTPNCL        WRKNCL
174100950118     C                   Z-ADD     BTPTFA        WRKTFA
174200970207     C                   CLEAR                   WRKDBR
174300970303     C                   CLEAR                   WRKDUC
174400970303     C                   CLEAR                   WRKDPC
174500970303     C                   CLEAR                   WRKNPI
174600950118     C                   ENDIF
174700950118     C*
174800950118     C                   WRITE     FNWRK000
174900950118     C*
175000940503     C                   ENDSR
175100940503      **************************************************************************
175200940503      * Calcolo le spedizioni da partire:
175300950118      *  vedo se gia' contata prima di ricontarla(se esiste in FNWRK)
175400970207      *  Potrei averla già contata se i colli non sono sequenziali
175500940503      **************************************************************************
175600940503     C     CTRBOL        BEGSR
175700940503     C*
175800940505     C                   MOVEL     $FLAG         $FLAG2
175900940503     C                   MOVEL     'P'           $FLAG
176000940503     C                   MOVEL     BLPLNP        WRKLNP
176100940503     C                   CLEAR                   WRKCSE
176200950118     C     FNWR1K        SETLL     FNWRK00F                               99
176300940503     C* BOLLA NON TROVATA  LA SCRIVO
176400940503    1C     *IN99         IFEQ      *OFF
176500940503     C                   EXSR      WRIWRK
176600940503     C*
176700970211     C                   ADD       1             SDT($T)
176800970210     C   34              ADD       1             SPA($T)
176900940503     C* S4A --> PER C/S SPEDIZIONI DELL'AREA
177000940503     C*         PER I TRANSITI
177100970210     C  N34              ADD       1             S4A($T)
177200940503    1C                   ENDIF
177300940505     C                   MOVEL     $FLAG2        $FLAG
177400940503     C                   ENDSR
177500000000      **************************************************************************
177600000000      * Calcolo sulle spedizioni, lettura del file di work creato prima
177700000000      **************************************************************************
177800000000     C     CALSPE        BEGSR
177900940502     C                   SETOFF                                       3435
178000940428      * Verifico se e' transito
178100940502     C     $FLAG         COMP      'T'                                    35
178200000000      *
178300000000      * Leggo tutti le spedizioni della filiale da stampare dal file di work
178400970207    1C     $FLAG         IFEQ      *BLANK
178500950118     C     FNWRKK        SETLL     FNWRK00F
178600970207   X1C                   ELSE
178700950118     C     $FLAG         SETLL     FNWRK00F
178800970207    1C                   ENDIF
178900000000      *
179000970207    1C                   DO        *HIVAL
179100000000      *
179200970207    2C     $FLAG         IFEQ      *BLANK
179300950118     C     FNWRKK        READE(N)  FNWRK00F                               99
179400970207   X2C                   ELSE
179500950118     C     $FLAG         READE(N)  FNWRK00F                               99
179600970207    2C                   ENDIF
179700000000      *
179800970207    2C  N99              DO
179900000000      *
180000090916      * Verifico il tipo servizio
180100090916     C                   Z-ADD     5             $V                1 0
180200090916     C                   SELECT
180300090916     C     wrktsp        wheneq    'E'
180400000000     C                   Z-ADD     1             $V
180500090916     C     wrktsp        wheneq    'D'
180600940323     C                   Z-ADD     2             $V
180700090916     C     wrktsp        wheneq    'C'
180800090916     C                   Z-ADD     3             $V
180900090916     C     wrktsp        wheneq    'H'
181000090916     C                   Z-ADD     4             $V
181100090916    3C                   ENDsl
181200940428      *
181300940428      * Verifico se e' c/servizio
181400940502     C  N35WRKCSE        COMP      *BLANKS                                34
181500000000      *
181600000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
181700000000      * poiche' lo dovro aggiornare
181800090916     C                   CLEAR                   fisat000
181900000000     C                   Z-ADD     SIMFEL        SATFLE
182000950210     C                   Z-ADD     DS1ADA        SATDRE
182100940506     C  N35              Z-ADD     WRKLNP        SATLNP
182200940506     C   35              Z-ADD     SIMFEL        SATLNP
182300000000     C                   Z-ADD     WRKTFA        SATTFA
182400000000     C  N35              MOVE      *BLANK        SATTRE
182500000000     C   35              MOVE      'T'           SATTRE
182600090916     C     FNSA1K        CHAIN     fisat02L                           37
182700000000      *
182800950628     C                   MOVE      WRKMGS        $DATS1            8 0
182900950628     C                   MOVEL     WRKAAS        $DATS1
183000950628     C*
183100970207     C* A TERRA COMPLETAMENTE
183200970207      * La spedizione e' completa se il nr.colli spediti (WRKNC1) e' = uguale
183300050112      * al nr.colli della bolla (WRKNCL)
183400970207    3C     WRKNC1        IFEQ      WRKNCL
183500940506     C                   ADD       1             S0M($V)
183600970207     C* Transiti
183700970207    4C     *IN35         IFEQ      *ON
183800940506     C                   ADD       1             S2M($V)
183900970207   X4C                   ELSE
184000970207     C* Dell'area
184100940506     C   34              ADD       1             SCM($V)
184200940506     C  N34              ADD       1             S4M($V)
184300940506     C                   ADD       1             S3M($V)
184400970207     C*
184500970207    4C                   ENDIF
184600970207    3C                   ENDIF
184700970210     C* COMDAP = BLANKS => A TERRA DA PARTIRE
184800970207     C                   CLEAR                   COMDAP
184900970303     C* BORDERIZZATO, DISTRIBUZIONE A TERRA DEL GIORNO NON SPAREGGIATA
185000070525    3c                   if        ds1aal<§vponpi
185100070525     c
185200970207    3C     WRKDBR        IFGT      *ZEROS
185300970207     C     WRKDBR        ANDLE     DS1AAL
185400970303     C*
185500970303    4C     WRKDUC        IFEQ      *ZEROS
185600970303     C     WRKDPC        ANDEQ     0
185700970303     C     WRKNPI        ANDLT     0
185800970303     C*
185900970303     C     WRKDUC        OREQ      *ZEROS
186000970303     C     WRKDPC        ANDGT     DS1ADA
186100970303     C     WRKNPI        ANDLT     0
186200970303     C*
186300970303     C     WRKDUC        ORGT      DS1ADA
186400970303     C     WRKDPC        ANDGT     DS1ADA
186500970303     C     WRKNPI        ANDLT     0
186600970207     C                   MOVE      'N'           COMDAP            1
186700970207    4C                   END
186800970207   X3C                   ELSE
186900970207     C* NON BORDERIZZATO
187000970207     C                   MOVE      'N'           COMDAP
187100970207    3C                   END
187200070525     c*
187300070525     c                   else
187400070525     c
187500070525     c* calcolo per data ritiro a terra non da partire solo la distribu
187600070525     c*  zione del giorno
187700070525    4c                   if        (wrkdpc=0 and wrknpi<0) or
187800070525     c                             (wrkdpc>ds1aal and wrknpi<0)
187900070525     C                   MOVE      'N'           COMDAP
188000070525    4c                   endif
188100070525    3c                   endif
188200070525     c
188300970207     C*
188400970207    3C     COMDAP        IFEQ      *BLANKS
188500970207     C* Per i transiti memorizzo in un unico totale le spediz. a terra
188600970207     C* e non mi interessa nemmeno memorizzare il totale da partire
188700970207     C     $FLAG         OREQ      'T'
188800970207     C***
188900970207     C* TOTALE DA PARTIRE
189000970207     C***
189100970207     C* Sommo le spediz.a terra border. esclusa la distrib. del periodo
189200970210     C  N35              ADD       1             SDT($V)
189300970210     C  N35
189400970210     CAN 34              ADD       1             SPA($V)
189500970207     C* S4A --> Per c/s spedizioni dell'area
189600970210     C  N35
189700970210     CANN34              ADD       1             S4A($V)
189800970207     C***
189900970207     C* A TERRA DA PARTIRE
190000970207     C***
190100970207     C   34
190200970207     CANN35              ADD       1             SBP($V)
190300970207     C  N34
190400970207     CANN35              ADD       1             S4P($V)
190500970207     C  N35              ADD       1             S3P($V)
190600970207     C   35              ADD       1             S2P($V)
190700970207     C                   ADD       1             S0P($V)
190800970305     C***
190900970305     C* A TERRA DA PARTIRE FUORI AFFIDABILITA'
191000970305     C***
191100970305    4C     WRKNPI        IFGE      0
191200970305     C   34
191300970305     CANN35              ADD       1             FAF($V)
191400970305     C  N34
191500970305     CANN35              ADD       1             F4F($V)
191600970305     C  N35              ADD       1             F3F($V)
191700970305     C  N35              ADD       1             F0F($V)
191800970305    4C                   END
191900970207     c*
192000970207   X3C                   ELSE
192100970207     C***
192200970207     C* A TERRA NON DA PARTIRE
192300970207     C***
192400970207     C   34              ADD       1             SBR($V)
192500970207     C  N34              ADD       1             S4R($V)
192600970207     C                   ADD       1             S3R($V)
192700970207     C                   ADD       1             S0R($V)
192800970207    3C                   ENDIF
192900000000      *
193000040922     c  n37              move      $FTR          satftr
193100040922     c  n37              move      $DTR          satDTR
193200090916     C  N37              UPDATE    fisat000
193300000000      *
193400970207    2C                   ENDDO
193500970207    1C  N99              ENDDO
193600000000      *
193700000000     C                   ENDSR
193800000000      **************************************************************************
193900000000      * Calcolo i totali di filiale, li stampo e scrivo sul file
194000000000      **************************************************************************
194100000000     C     TOTFIL        BEGSR
194200000000      *
194300000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
194400000000      *
194500970211     C                   SETON                                        404145
194600000000     C                   Z-ADD     $FILPR        FILIAP
194700000000      *
194800000000      * Decodifico il codice filiale
194900000000     C     $FILPR        CHAIN     AZORG01L                           99
195000000000     C  N99ORGFVA        COMP      *BLANK                             9999
195100940503     C   99              CLEAR                   ORGDEP
195200000000     C  N99              MOVEL     ORGDES        ORGDEP
195300000000      *
195400000000      * ?  C  O   L   L  I  ?
195500970210     C                   XFOOT     CBP           COTOTP
195600970210     C                   XFOOT     CPA           $CDA
195700940428     C                   EXSR      PERCOL
195800000000      *
195900000000      * ?  P  E  S  O  ?
196000970210     C                   XFOOT     PFI           PEF35P
196100970210     C                   XFOOT     PBP           PEO35P
196200970210     C     PEF35P        ADD       PEO35P        PETOTP
196300970210     C                   XFOOT     PPA           $PDA
196400940428     C                   EXSR      PERPES
196500000000      *
196600000000      * ?  V  O  L  U  M E  ?
196700970210     C                   XFOOT     VBP           VOTOTP
196800970210     C                   XFOOT     VPA           $VDA
196900940428     C                   EXSR      PERVOL
197000000000      *
197100000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
197200970210     C                   XFOOT     SPA           $SDA
197300970305     C                   XFOOT     FAF           SPFAFF
197400940502     C                   EXSR      PERSPE
197500970305     C                   XFOOT     SBP           SPTOTP
197600970305     C                   XFOOT     SCM           SPCONP
197700970305     C                   XFOOT     SBR           $COMOD
197800970211     C     SPTOTP        ADD       $COMOD        SPTOTA
197900970211     C     SPTOTA        SUB       SPCONP        SPSPAP
198000000331      *
198100000331      * ? VOLUME A MAGAZZINO?
198200000331     C                   XFOOT     VBR           VOTOTM
198300000000      *
198400970210     C* STAMPO SOLO SE HO DEI DATI
198500000331     C     VOTOTM        IFGT      *ZEROS
198600000417     C* HO RIMESSO ANCHE TEST SU SPTOTA PER FAR FUNZIONARE LE RISTAMPE
198700000417     C* RELATIVE A DATE ELABORATE PRIMA CHE VENISSE FATTO IL CALCOLO
198800000417     C* DEL TOTALE VOLUME A MAGAZZINO
198900000417     C     SPTOTA        ORGT      *ZEROS
199000970210     C*
199100000000     C                   Z-ADD     $FILPR        FILIAP
199200000000     C                   EXSR      SCRIVI
199300940509     C                   ADD       1             $RIGHE            2 0
199400000000      *
199500090916      * ------------------ ESPR./DIST./CORR./H1030 --------------------
199600000000      *
199700970210     C                   SETOFF                                       40
199800090916
199900090916     C                   DO        ciclo         $V
200000940429     C                   CLEAR                   DESCRP
200100940429      *
200200970210     C                   MOVE      ENS($V)       TIPOP
200300000000      *
200400000000      * ?  C  O   L   L  I  ?
200500970211     C                   Z-ADD     CBP($V)       COTOTP
200600970211     C                   Z-ADD     CPA($V)       $CDA
200700940428     C                   EXSR      PERCOL
200800000000      *
200900000000      * ?  P  E  S  O  ?
201000000000     C                   Z-ADD     PFI($V)       PEF35P
201100970211     C                   Z-ADD     PBP($V)       PEO35P
201200970212     C     PEF35P        ADD       PEO35P        PETOTP
201300970211     C                   Z-ADD     PPA($V)       $PDA
201400940428     C                   EXSR      PERPES
201500000000      *
201600000000      * ?  V  O  L  U  M E  ?
201700970211     C                   Z-ADD     VBP($V)       VOTOTP
201800970211     C                   Z-ADD     VPA($V)       $VDA
201900940428     C                   EXSR      PERVOL
202000000000      *
202100000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
202200970305     C                   Z-ADD     SPA($V)       $SDA
202300970305     C                   Z-ADD     FAF($V)       SPFAFF
202400940502     C                   EXSR      PERSPE
202500970305     C                   Z-ADD     SBP($V)       SPTOTP
202600970305     C                   Z-ADD     SCM($V)       SPCONP
202700970211     C     SBP($V)       ADD       SBR($V)       SPTOTA
202800970211     C     SPTOTA        SUB       SCM($V)       SPSPAP
202900000331      *
203000000331      * ?tot.volume a magazz?
203100000331     C                   Z-ADD     VBR($V)       VOTOTM
203200000000      *
203300000000     C                   EXSR      SCRIVI
203400000000      *
203500000000     C                   ENDDO
203600000000      *
203700950118     C                   WRITE     LSC2T2
203800940506     C                   ENDIF
203900940328      *
204000000417     C                   ENDSR
204100940428      **************************************************************************
204200940428      * Calcolo percentuale sui colli
204300940428      **************************************************************************
204400940428     C     PERCOL        BEGSR
204500940502     C                   Z-ADD     *ZERO         COSDPP
204600940428     C*
204700940502     C* PERCENTUALE SUL DA PARTIRE
204800970213     C  N02$CDA          IFNE      *ZERO
204900970210     C     COTOTP        DIV       $CDA          $COMOD
205000940502     C     $COMOD        MULT(H)   100           COSDPP
205100940502     C                   ENDIF
205200940428     C                   ENDSR
205300940428      **************************************************************************
205400940428      * Calcolo percentuale sul peso
205500940428      **************************************************************************
205600940428     C     PERPES        BEGSR
205700940502     C                   CLEAR                   PESDPP
205800940428     C*
205900940502     C* PERCENTUALE SUL DA PARTIRE
206000970213     C  N02$PDA          IFNE      *ZERO
206100970210     C     PETOTP        DIV       $PDA          $COMOD
206200940502     C     $COMOD        MULT(H)   100           PESDPP
206300940502     C                   ENDIF
206400940428     C                   ENDSR
206500940428      **************************************************************************
206600940428      * Calcolo percentuale sul volume
206700940428      **************************************************************************
206800940428     C     PERVOL        BEGSR
206900940502     C                   Z-ADD     *ZERO         VOSDPP
207000940502     C*
207100940502     C* PERCENTUALE SUL DA PARTIRE
207200970213     C  N02$VDA          IFNE      *ZERO
207300970210     C     VOTOTP        DIV       $VDA          $COMOD
207400940502     C     $COMOD        MULT(H)   100           VOSDPP
207500940502     C                   ENDIF
207600940428     C                   ENDSR
207700940428      **************************************************************************
207800940428      * Calcolo percentuale sulle spedizioni
207900940428      **************************************************************************
208000940428     C     PERSPE        BEGSR
208100940502     C                   Z-ADD     *ZERO         SPSDPP
208200940428     C*
208300940502     C* PERCENTUALE SUL DA PARTIRE
208400970213     C  N02$SDA          IFNE      *ZERO
208500970305     C     SPFAFF        DIV       $SDA          $COMOD
208600940502     C     $COMOD        MULT(H)   100           SPSDPP
208700940502     C                   ENDIF
208800940428     C                   ENDSR
208900000000      **************************************************************************
209000000000      * Azzeramento dei totali di filiale per nuovi conti
209100000000      **************************************************************************
209200000000     C     AZZFIL        BEGSR
209300000412     C                   CLEAR                   VOLUME
209400000000      *
209500000000      * COLLI
209600970211     C                   ADD       CPA(1)        C3A(1)
209700970211     C                   ADD       CPA(2)        C3A(2)
209800970211     C                   ADD       CPA(3)        C3A(3)
209900090916     C                   ADD       CPA(4)        C3A(4)
210000090916     C                   ADD       CPA(5)        C3A(5)
210100970211     C                   CLEAR                   CPA
210200970211     C                   CLEAR                   CBP
210300970211     C                   CLEAR                   CBR
210400970211      * PESI
210500970211     C                   ADD       PPA(1)        P3A(1)
210600970211     C                   ADD       PPA(2)        P3A(2)
210700970211     C                   ADD       PPA(3)        P3A(3)
210800090916     C                   ADD       PPA(4)        P3A(4)
210900090916     C                   ADD       PPA(5)        P3A(5)
211000970211     C                   CLEAR                   PFI
211100970211     C                   CLEAR                   PPA
211200970211     C                   CLEAR                   PBP
211300970211     C                   CLEAR                   PBR
211400970211     C                   CLEAR                   PFN
211500000000      * VOLUMI
211600970211     C                   ADD       VPA(1)        V3A(1)
211700970211     C                   ADD       VPA(2)        V3A(2)
211800970211     C                   ADD       VPA(3)        V3A(3)
211900090916     C                   ADD       VPA(4)        V3A(4)
212000090916     C                   ADD       VPA(5)        V3A(5)
212100970211     C                   CLEAR                   VPA
212200970211     C                   CLEAR                   VBP
212300970211     C                   CLEAR                   VBR
212400970211      * SPEDIZIONI
212500970211     C                   ADD       SPA(1)        S3A(1)
212600970211     C                   ADD       SPA(2)        S3A(2)
212700970211     C                   ADD       SPA(3)        S3A(3)
212800090916     C                   ADD       SPA(4)        S3A(4)
212900090916     C                   ADD       SPA(5)        S3A(5)
213000970211     C                   CLEAR                   SPA
213100970211     C                   CLEAR                   SBP
213200970211     C                   CLEAR                   SBR
213300970211     C                   CLEAR                   SCM
213400970305     C                   CLEAR                   FAF
213500000000      *
213600000000     C                   ENDSR
213700000000      **************************************************************************
213800000000      * Azzeramento dei totali di c/servizio per nuova area
213900000000      **************************************************************************
214000000000     C     AZZSER        BEGSR
214100000000      *
214200000000      * COLLI
214300940506     C                   ADD       C1A(1)        C3A(1)
214400970211     C                   ADD       C1A(2)        C3A(2)
214500970211     C                   ADD       C1A(3)        C3A(3)
214600090916     C                   ADD       C1A(4)        C3A(4)
214700090916     C                   ADD       C1A(5)        C3A(5)
214800000000     C                   CLEAR                   C1A
214900000000     C                   CLEAR                   C1P
215000000000     C                   CLEAR                   C1R
215100000000      * PESI
215200940506     C                   ADD       P1A(1)        P3A(1)
215300970211     C                   ADD       P1A(2)        P3A(2)
215400970211     C                   ADD       P1A(3)        P3A(3)
215500090916     C                   ADD       P1A(4)        P3A(4)
215600090916     C                   ADD       P1A(5)        P3A(5)
215700970211     C                   CLEAR                   P1I
215800970211     C                   CLEAR                   P1N
215900970211     C                   CLEAR                   P1P
216000970211     C                   CLEAR                   P1R
216100970211     C                   CLEAR                   P1A
216200970211      * VOLUMI
216300970211     C                   ADD       V1A(1)        V3A(1)
216400970211     C                   ADD       V1A(2)        V3A(2)
216500970211     C                   ADD       V1A(3)        V3A(3)
216600090916     C                   ADD       V1A(4)        V3A(4)
216700090916     C                   ADD       V1A(5)        V3A(5)
216800970211     C                   CLEAR                   V1A
216900970211     C                   CLEAR                   V1P
217000970211     C                   CLEAR                   V1R
217100000000      * SPEDIZIONI
217200940506     C                   ADD       S1A(1)        S3A(1)
217300970211     C                   ADD       S1A(2)        S3A(2)
217400970211     C                   ADD       S1A(3)        S3A(3)
217500090916     C                   ADD       S1A(4)        S3A(4)
217600090916     C                   ADD       S1A(5)        S3A(5)
217700970211     C                   CLEAR                   S1A
217800970211     C                   CLEAR                   S1P
217900970211     C                   CLEAR                   S1R
218000970211     C                   CLEAR                   S1M
218100970305     C                   CLEAR                   F1F
218200000000      *
218300000000     C                   ENDSR
218400000000      **************************************************************************
218500000000      * Azzeramento dei totali di c/servizio per nuova filiale
218600000000      **************************************************************************
218700000000     C     AZZSE4        BEGSR
218800000000      *
218900000000      * COLLI
219000970210     C                   CLEAR                   C4A
219100970210     C                   CLEAR                   C4P
219200970210     C                   CLEAR                   C4R
219300000000      * PESI
219400970210     C                   CLEAR                   P4A
219500970210     C                   CLEAR                   P4I
219600970210     C                   CLEAR                   P4P
219700970210     C                   CLEAR                   P4R
219800970210     C                   CLEAR                   P4N
219900000000      * VOLUMI
220000970210     C                   CLEAR                   V4A
220100970210     C                   CLEAR                   V4P
220200970210     C                   CLEAR                   V4R
220300000000      * SPEDIZIONI
220400970210     C                   CLEAR                   S4A
220500970210     C                   CLEAR                   S4P
220600970210     C                   CLEAR                   S4R
220700970210     C                   CLEAR                   S4M
220800970305     C                   CLEAR                   F4F
220900000000      *
221000000000     C                   ENDSR
221100000000      **************************************************************************
221200000000      * Azzeramento dei totali di transito per nuovi totali
221300000000      **************************************************************************
221400000000     C     AZZTRA        BEGSR
221500940506     C* SOMMO NEI TOTALI
221600940506     C                   EXSR      ADDTRA
221700000000      * COLLI
221800940428     C                   CLEAR                   C2P
221900000000      * PESI
222000000000     C                   CLEAR                   P2I
222100940428     C                   CLEAR                   P2P
222200000000      * VOLUMI
222300940428     C                   CLEAR                   V2P
222400000000      * SPEDIZIONI
222500940428     C                   CLEAR                   S2P
222600000000     C                   CLEAR                   S2M
222700000000      *
222800000000     C                   ENDSR
222900000000      **************************************************************************
223000000000      * Stampo i totali per c/servizio
223100000000      **************************************************************************
223200000000     C     TOTSER        BEGSR
223300000000      *
223400000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
223500000000      *
223600970211     C                   SETON                                        4045
223700000000     C                   SETOFF                                       41
223800000000     C                   Z-ADD     *ZERO         FILIAP
223900000000     C                   MOVE      *BLANKS       ORGDEP
224000000000     C     'C/SERVIZ'    CAT       'I':0         ORGDEP
224100000000      *
224200970211      * ?  C  O   L   L  I  ?
224300970211     C                   XFOOT     C1P           COTOTP
224400970211     C                   XFOOT     C1A           $CDA
224500970211     C                   EXSR      PERCOL
224600970211      *
224700970211      * ?  P  E  S  O  ?
224800970211     C                   XFOOT     P1I           PEF35P
224900970211     C                   XFOOT     P1P           PEO35P
225000970211     C     PEF35P        ADD       PEO35P        PETOTP
225100970211     C                   XFOOT     P1A           $PDA
225200940429     C                   EXSR      PERPES
225300000000      *
225400000000      * ?  V  O  L  U  M E  ?
225500970211     C                   XFOOT     V1P           VOTOTP
225600970211     C                   XFOOT     V1A           $VDA
225700940429     C                   EXSR      PERVOL
225800000000      *
225900000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
226000970211     C                   XFOOT     S1A           $SDA
226100970305     C                   XFOOT     F1F           SPFAFF
226200970211     C                   EXSR      PERSPE
226300970305     C                   XFOOT     S1P           SPTOTP
226400970305     C                   XFOOT     S1M           SPCONP
226500970211     C                   XFOOT     S1R           $COMO1
226600970211     C     SPTOTP        ADD       $COMO1        SPTOTA
226700970211     C     SPTOTA        SUB       SPCONP        SPSPAP
226800000331      *
226900000331      * ?tot.volume a magazz?
227000000331     C                   XFOOT     V1R           VOTOTM
227100970211      *
227200970211     C* STAMPO SOLO SE HO DEI DATI
227300000331     C     VOTOTM        IFGT      *ZEROS
227400970211     C*
227500970211     C                   EXSR      SCRIVI
227600000000      *
227700090916      * ------------------ ESPR./DIST./CORR./H1030 --------------------
227800000000      *
227900970211     C                   SETOFF                                       40
228000090916     C                   DO        ciclo         $V
228100940323     C                   MOVE      ENS($V)       TIPOP
228200000000      *
228300000000      * ?  C  O   L   L  I  ?
228400970211     C                   Z-ADD     C1P($V)       COTOTP
228500970211     C                   Z-ADD     C1A($V)       $CDA
228600940429     C                   EXSR      PERCOL
228700000000      *
228800000000      * ?  P  E  S  O  ?
228900000000     C                   Z-ADD     P1I($V)       PEF35P
229000970211     C                   Z-ADD     P1P($V)       PEO35P
229100970211     C     PEF35P        ADD       PEO35P        PETOTP
229200970211     C                   Z-ADD     P1A($V)       $PDA
229300940429     C                   EXSR      PERPES
229400000000      *
229500000000      * ?  V  O  L  U  M E  ?
229600970211     C                   Z-ADD     V1P($V)       VOTOTP
229700970211     C                   Z-ADD     V1A($V)       $VDA
229800940429     C                   EXSR      PERVOL
229900000000      *
230000000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
230100970211     C                   Z-ADD     S1A($V)       $SDA
230200970305     C                   Z-ADD     F1F($V)       SPFAFF
230300940502     C                   EXSR      PERSPE
230400970305     C                   Z-ADD     S1P($V)       SPTOTP
230500970305     C                   Z-ADD     S1M($V)       SPCONP
230600970211     C     S1P($V)       ADD       S1R($V)       SPTOTA
230700970211     C     SPTOTA        SUB       S1M($V)       SPSPAP
230800000331      *
230900000331      * ?tot volume a magazz?
231000000331     C                   Z-ADD     V1R($V)       VOTOTM
231100000000      *
231200000000     C                   EXSR      SCRIVI
231300000000      *
231400000000     C                   ENDDO
231500940504     C                   ENDIF
231600940429     C                   EXSR      AZZSER
231700000000      *
231800000000     C                   ENDSR
231900000000      **************************************************************************
232000000000      * Stampo i totali per transito
232100000000      **************************************************************************
232200000000     C     TOTTRA        BEGSR
232300000000      *
232400000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
232500000000      *
232600940506     C                   SETON                                        400203
232700970211     C                   SETOFF                                       45
232800000000     C                   MOVE      *BLANKS       ORGDEP
232900940502     C* stampa per linea: totale transiti
233000940502     C     *IN43         IFEQ      *ON
233100940502     C                   Z-ADD     *ZERO         FILIAP
233200940502     C                   MOVEL     'TRANSITI'    ORGDEP
233300940502     C                   SETOFF                                       41
233400940502     C                   ELSE
233500940502     c* stampa per terminal in transito
233600940502     C                   SETON                                        41
233700940502      * Decodifico il terminal di arrivo
233800940502     C                   Z-ADD     $FILPR        FILIAP
233900940502     C     $FILPR        CHAIN     AZORG01L                           99
234000940502     C  N99ORGFVA        COMP      *BLANK                             9999
234100940503     C   99              CLEAR                   ORGDEP
234200940502     C  N99              MOVEL     ORGDES        ORGDEP
234300940502     C                   ENDIF
234400000000      *
234500000000      * ?  C  O   L   L  I  ?
234600940502     C                   XFOOT     C2P           COTOTP
234700970211     C*
234800970211      * SOLO SE HO TROVATO ALEMNO UN COLLO A TERRA STAMPO
234900970211     C     COTOTP        IFGT      0
235000940504      *
235100000000      *
235200000000      * ?  P  E  S  O  ?
235300970211     C                   XFOOT     P2I           PEF35P
235400970211     C                   XFOOT     P2P           PEO35P
235500970211     C     PEO35P        ADD       PEF35P        PETOTP
235600000000      *
235700000000      * ?  V  O  L  U  M E  ?
235800940428     C                   XFOOT     V2P           VOTOTP
235900000000      *
236000000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
236100940428     C                   XFOOT     S2P           SPTOTP
236200970211     C                   XFOOT     S2P           SPTOTA
236300970211     C                   XFOOT     S2M           SPCONP
236400970211     C     SPTOTA        SUB       SPCONP        SPSPAP
236500000000      *
236600000000     C                   EXSR      SCRIVI
236700940509     C                   ADD       1             $RIGHE
236800000000      *
236900090916      * ------------------ ESPR./DIST./CORR./H1030 --------------------
237000000000      *
237100940502     C                   SETOFF                                       4045
237200090916     C                   DO        CICLO         $V
237300940502     C* PER TERMINAL SCRIVO CHE E' RECORD DI TRANSITO
237400940429     C  N43$V            IFEQ      1
237500940506     C                   MOVEL     'IN TRANS'    DESCRP
237600940429     C                   ELSE
237700940429     C                   CLEAR                   DESCRP
237800940429     C                   ENDIF
237900940429     C*
238000940323     C                   MOVE      ENS($V)       TIPOP
238100000000      *
238200000000      * ?  C  O   L   L  I  ?
238300940428     C                   Z-ADD     C2P($V)       COTOTP
238400000000      *
238500000000      * ?  P  E  S  O  ?
238600970211     C                   Z-ADD     P2I($V)       PEF35P
238700970211     C                   Z-ADD     P2P($V)       PEO35P
238800970211     C     PEO35P        ADD       PEF35P        PETOTP
238900000000      *
239000000000      * ?  V  O  L  U  M E  ?
239100940428     C                   Z-ADD     V2P($V)       VOTOTP
239200000000      *
239300000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
239400940428     C                   Z-ADD     S2P($V)       SPTOTP
239500970211     C                   Z-ADD     S2P($V)       SPTOTA
239600970211     C                   Z-ADD     S2M($V)       SPCONP
239700970211     C     SPTOTA        SUB       S2M($V)       SPSPAP
239800000000      *
239900000000     C                   EXSR      SCRIVI
240000000000      *
240100000000     C                   ENDDO
240200940428     C*
240300940428     C                   EXSR      AZZTRA
240400940502      *
240500950118     C  N38              WRITE     LSC2T2
240600940506     C                   ENDIF
240700940506     C                   SETOFF                                       0302
240800000000      *
240900000000     C                   ENDSR
241000000000      **************************************************************************
241100090916      * Scrittura del record sul file di trasmissione fisat00F
241200000000      **************************************************************************
241300000000     C     WRISAT        BEGSR
241400000000      *
241500090916     C                   CLEAR                   fisat000
241600000000     C                   Z-ADD     SIMFEL        SATFLE
241700000000     C                   Z-ADD     $FILPR        SATLNP
241800000000     C                   Z-ADD     *ZERO         SATTFA
241900950210     C                   Z-ADD     DS1ADA        SATDRE
242000000000     C                   MOVE      *BLANK        SATTRE
242100000000      *
242200000000      * COLLI: da partire
242300970210     C                   Z-ADD     CPA(1)        SATCPE
242400970210     C                   Z-ADD     CPA(2)        SATCPD
242500970210     C                   Z-ADD     CPA(3)        SATCPC
242600090916     c                   Z-ADD     CPA(4)        SATCPh
242700090916     c                   Z-ADD     CPA(5)        SATCpx
242800970207      * COLLI: a terra da partire
242900970207     C                   Z-ADD     CBP(1)        SATCDE
243000970207     C                   Z-ADD     CBP(2)        SATCDD
243100970207     C                   Z-ADD     CBP(3)        SATCDC
243200090916     C                   Z-ADD     CBP(4)        SATCDh
243300090916     C                   Z-ADD     CBP(5)        SATCDx
243400970207      * COLLI: a terra non da partire
243500970207     C                   Z-ADD     CBR(1)        SATCNE
243600970207     C                   Z-ADD     CBR(2)        SATCND
243700970207     C                   Z-ADD     CBR(3)        SATCNC
243800090916     C                   Z-ADD     CBR(4)        SATCNh
243900090916     C                   Z-ADD     CBR(5)        SATCNx
244000970207      * PESO: da partire
244100970210     C                   Z-ADD     PPA(1)        SATPPE
244200970210     C                   Z-ADD     PPA(2)        SATPPD
244300970210     C                   Z-ADD     PPA(3)        SATPPC
244400090916     C                   Z-ADD     PPA(4)        SATPPh
244500090916     C                   Z-ADD     PPA(5)        SATPPx
244600970207      * PESO: a terra da partire fino a 35 quintali
244700970207     C                   Z-ADD     PFI(1)        SATFDE
244800970207     C                   Z-ADD     PFI(2)        SATFDD
244900970207     C                   Z-ADD     PFI(3)        SATFDC
245000090916     C                   Z-ADD     PFI(4)        SATFDh
245100090916     C                   Z-ADD     PFI(5)        SATFDx
245200970207      * PESO: a terra non da partire fino a 35 q.
245300970207     C                   Z-ADD     PFN(1)        SATFNE
245400970207     C                   Z-ADD     PFN(2)        SATFND
245500970207     C                   Z-ADD     PFN(3)        SATFNC
245600090916     C                   Z-ADD     PFN(4)        SATFNh
245700090916     C                   Z-ADD     PFN(5)        SATFNx
245800970207     C* PESO: a terra da partire oltre 35 quintali
245900970207     C                   Z-ADD     PBP(1)        SATODE
246000970207     C                   Z-ADD     PBP(2)        SATODD
246100970207     C                   Z-ADD     PBP(3)        SATODC
246200090916     C                   Z-ADD     PBP(4)        SATODh
246300090916     C                   Z-ADD     PBP(5)        SATODx
246400970207      * PESO: a terra non da partire oltre 35 q.
246500970207     C                   Z-ADD     PBR(1)        SATONE
246600970207     C                   Z-ADD     PBR(2)        SATOND
246700970207     C                   Z-ADD     PBR(3)        SATONC
246800090916     c                   Z-ADD     PBR(4)        SATONh
246900090916     C                   Z-ADD     PBR(5)        SATONx
247000000000      * VOLUME: da partire
247100970210     C                   Z-ADD     VPA(1)        SATVPE
247200970210     C                   Z-ADD     VPA(2)        SATVPD
247300970210     C                   Z-ADD     VPA(3)        SATVPC
247400090916     C                   Z-ADD     VPA(4)        SATVPh
247500090916     C                   Z-ADD     VPA(5)        SATVPx
247600970207      * VOLUME a terra da partire
247700970207     C                   Z-ADD     VBP(1)        SATVDE
247800970207     C                   Z-ADD     VBP(2)        SATVDD
247900970207     C                   Z-ADD     VBP(3)        SATVDC
248000090916     C                   Z-ADD     VBP(4)        SATVDh
248100090916     C                   Z-ADD     VBP(5)        SATVDx
248200000331      * VOLUME rimasto a magazzino
248300000331      * (totale dei colli a terra da partire, da non partire e colli
248400000331      * di bolle in sospensione)
248500970207     C                   Z-ADD     VBR(1)        SATVNE
248600970207     C                   Z-ADD     VBR(2)        SATVND
248700970207     C                   Z-ADD     VBR(3)        SATVNC
248800090916     C                   Z-ADD     VBR(4)        SATVNh
248900090916     C                   Z-ADD     VBR(5)        SATVNx
249000940429      * SPEDIZIONI: da partire
249100970210     C                   Z-ADD     SPA(1)        SATSPE
249200970210     C                   Z-ADD     SPA(2)        SATSPD
249300970210     C                   Z-ADD     SPA(3)        SATSPC
249400090916     C                   Z-ADD     SPA(4)        SATSPh
249500090916     C                   Z-ADD     SPA(5)        SATSPx
249600970207      * SPEDIZIONI a terra da partire
249700970207     C                   Z-ADD     SBP(1)        SATSDE
249800970207     C                   Z-ADD     SBP(2)        SATSDD
249900970207     C                   Z-ADD     SBP(3)        SATSDC
250000090916     C                   Z-ADD     SBP(4)        SATSDh
250100090916     C                   Z-ADD     SBP(5)        SATSDx
250200090916      * SPEDIZIONI a terra non da partire
250300970207     C                   Z-ADD     SBR(1)        SATSNE
250400970207     C                   Z-ADD     SBR(2)        SATSND
250500970207     C                   Z-ADD     SBR(3)        SATSNC
250600090916     C                   Z-ADD     SBR(4)        SATSNh
250700090916     C                   Z-ADD     SBR(5)        SATSNx
250800970305      * SPEDIZIONI a terra da partire fuori affidabilità
250900970305     C                   Z-ADD     FAF(1)        SATSFE
251000970305     C                   Z-ADD     FAF(2)        SATSFD
251100970305     C                   Z-ADD     FAF(3)        SATSFC
251200090916     C                   Z-ADD     FAF(4)        SATSFh
251300090916     C                   Z-ADD     FAF(5)        SATSFx
251400000000      * SPEDIZIONI: complete
251500000000     C                   Z-ADD     SCM(1)        SATSCE
251600970207     C                   Z-ADD     SCM(2)        SATSCD
251700970207     C                   Z-ADD     SCM(3)        SATSCC
251800090916     C                   Z-ADD     SCM(4)        SATSCh
251900090916     C                   Z-ADD     SCM(5)        SATSCx
252000000000      *
252100970318     C* ASTERISCO LE SEGUENTI IF PERCHE' IL RECORD LO DEVO SEMPRE
252200970318     C* SCRIVERE PER EVITARE CHE NELL'ELABORAZIONE DEL REPORT DI
252300970318     C* SEDE, SE PER UN P.O. IL DA PARTIRE è = 0, VENGA ERRONEAMENTE
252400970318     C* SEGNALATO "DATI NON PERVENUTI".
252500970318     C*          SATSDE    IFGT *ZERO
252600970318     C*          SATSDD    ORGT *ZERO
252700970318     C*          SATSDC    ORGT *ZERO
252800970318     C*          SATSNE    ORGT *ZERO
252900970318     C*          SATSND    ORGT *ZERO
253000970318     C*          SATSNC    ORGT *ZERO
253100970318     C*          SATSPE    ORGT *ZEROS
253200970318     C*          SATSPD    ORGT *ZEROS
253300970318     C*          SATSPC    ORGT *ZEROS
253400940323      *
253500940428     C                   MOVE      $FTR          SATFTR
253600970207     C                   MOVE      $DTR          SATDTR
253700940323      *
253800090916     C                   WRITE     fisat000
253900970318     C*
254000970318     C* LE IF SOPRA ASTERISCATE LE MANTENGO PERò PER IMPOSTARE $PRIM4
254100970318     C*
254200970318     C     SATSDE        IFGT      *ZERO
254300970318     C     SATSDD        ORGT      *ZERO
254400970318     C     SATSDC        ORGT      *ZERO
254500090916     C     SATSDh        ORGT      *ZERO
254600090916     C     SATSDx        ORGT      *ZERO
254700970318     C     SATSNE        ORGT      *ZERO
254800970318     C     SATSND        ORGT      *ZERO
254900970318     C     SATSNC        ORGT      *ZERO
255000090916     C     SATSNh        ORGT      *ZERO
255100090916     C     SATSNx        ORGT      *ZERO
255200970318     C     SATSPE        ORGT      *ZEROS
255300970318     C     SATSPD        ORGT      *ZEROS
255400970318     C     SATSPC        ORGT      *ZEROS
255500090916     C     SATSPh        ORGT      *ZEROS
255600090916     C     SATSPx        ORGT      *ZEROS
255700000405     C     SATVNE        ORGT      *ZEROS
255800000405     C     SATVND        ORGT      *ZEROS
255900000405     C     SATVNC        ORGT      *ZEROS
256000090916     C     SATVNh        ORGT      *ZEROS
256100090916     C     SATVNx        ORGT      *ZEROS
256200940429     C* PER LA STAMPA DEI TOTALI DI AREA
256300940429     C                   Z-ADD     1             $PRIM4
256400970318     C                   ENDIF
256500000000      *
256600000000     C                   ENDSR
256700000000      **************************************************************************
256800090916      * Scrittura del record c/servizio sul file di trasmissione fisat00F
256900000000      **************************************************************************
257000000000     C     WR1SAT        BEGSR
257100000000      *
257200090916     C                   CLEAR                   fisat000
257300000000     C                   Z-ADD     SIMFEL        SATFLE
257400000000     C                   Z-ADD     $FILPR        SATLNP
257500000000     C                   Z-ADD     *ZERO         SATTFA
257600950210     C                   Z-ADD     DS1ADA        SATDRE
257700000000     C                   MOVE      'C'           SATTRE
257800000000      *
257900000000      * COLLI: da partire
258000970210     C                   Z-ADD     C4A(1)        SATCPE
258100970210     C                   Z-ADD     C4A(2)        SATCPD
258200970210     C                   Z-ADD     C4A(3)        SATCPC
258300090916     C                   Z-ADD     C4A(4)        SATCPh
258400090916     C                   Z-ADD     C4A(5)        SATCPx
258500970207      * COLLI: a terra da partire
258600970207     C                   Z-ADD     C4P(1)        SATCDE
258700970207     C                   Z-ADD     C4P(2)        SATCDD
258800970207     C                   Z-ADD     C4P(3)        SATCDC
258900090916     C                   Z-ADD     C4P(4)        SATCDh
259000090916     C                   Z-ADD     C4P(5)        SATCDx
259100970207      * COLLI: a terra non da partire
259200970207     C                   Z-ADD     C4R(1)        SATCNE
259300970207     C                   Z-ADD     C4R(2)        SATCND
259400970207     C                   Z-ADD     C4R(3)        SATCNC
259500090916     C                   Z-ADD     C4R(4)        SATCNh
259600090916     C                   Z-ADD     C4R(5)        SATCNx
259700970207      * PESO: da partire
259800970210     C                   Z-ADD     P4A(1)        SATPPE
259900970210     C                   Z-ADD     P4A(2)        SATPPD
260000970210     C                   Z-ADD     P4A(3)        SATPPC
260100090916     C                   Z-ADD     P4A(4)        SATPPh
260200090916     C                   Z-ADD     P4A(5)        SATPPx
260300970207      * PESO: a terra da partire fino a 35 quintali
260400970207     C                   Z-ADD     P4I(1)        SATFDE
260500970207     C                   Z-ADD     P4I(2)        SATFDD
260600970207     C                   Z-ADD     P4I(3)        SATFDC
260700090916     C                   Z-ADD     P4I(4)        SATFDh
260800090916     C                   Z-ADD     P4I(5)        SATFDx
260900970207      * PESO: a terra non da partire fino a 35 quintali
261000970207     C                   Z-ADD     P4N(1)        SATFNE
261100970207     C                   Z-ADD     P4N(2)        SATFND
261200970207     C                   Z-ADD     P4N(3)        SATFNC
261300090916     C                   Z-ADD     P4N(4)        SATFNh
261400090916     C                   Z-ADD     P4N(5)        SATFNx
261500970207      * PESO: a terra da partire oltre 35 quintali
261600970207     C                   Z-ADD     P4P(1)        SATODE
261700970207     C                   Z-ADD     P4P(2)        SATODD
261800970207     C                   Z-ADD     P4P(3)        SATODC
261900090916     C                   Z-ADD     P4P(4)        SATODh
262000090916     C                   Z-ADD     P4P(5)        SATODx
262100970207      * PESO: a terra non da partire oltre 35 quintali
262200970207     C                   Z-ADD     P4R(1)        SATONE
262300970207     C                   Z-ADD     P4R(2)        SATOND
262400970207     C                   Z-ADD     P4R(3)        SATONC
262500090916     C                   Z-ADD     P4R(4)        SATONh
262600090916     C                   Z-ADD     P4R(5)        SATONx
262700000000      * VOLUME: da partire
262800970210     C                   Z-ADD     V4A(1)        SATVPE
262900970210     C                   Z-ADD     V4A(2)        SATVPD
263000970210     C                   Z-ADD     V4A(3)        SATVPC
263100090916     C                   Z-ADD     V4A(4)        SATVPh
263200090916     C                   Z-ADD     V4A(5)        SATVPx
263300970207      * VOLUME: a terra da partire
263400970207     C                   Z-ADD     V4P(1)        SATVDE
263500970207     C                   Z-ADD     V4P(2)        SATVDD
263600970207     C                   Z-ADD     V4P(3)        SATVDC
263700090916     C                   Z-ADD     V4P(4)        SATVDh
263800090916     C                   Z-ADD     V4P(5)        SATVDx
263900000331      * VOLUME: totale rimasto a magazzino
264000000331      * (a terra da partire, da non partire, in sospensione)
264100970207     C                   Z-ADD     V4R(1)        SATVNE
264200970207     C                   Z-ADD     V4R(2)        SATVND
264300970207     C                   Z-ADD     V4R(3)        SATVNC
264400090916     C                   Z-ADD     V4R(4)        SATVNh
264500090916     C                   Z-ADD     V4R(5)        SATVNx
264600970207      * SPEDIZIONI: da partire
264700970210     C                   Z-ADD     S4A(1)        SATSPE
264800970210     C                   Z-ADD     S4A(2)        SATSPD
264900970210     C                   Z-ADD     S4A(3)        SATSPC
265000090916     C                   Z-ADD     S4A(4)        SATSPh
265100090916     C                   Z-ADD     S4A(5)        SATSPx
265200970207      * SPEDIZIONI: a terra da partire
265300970207     C                   Z-ADD     S4P(1)        SATSDE
265400970207     C                   Z-ADD     S4P(2)        SATSDD
265500970207     C                   Z-ADD     S4P(3)        SATSDC
265600090916     C                   Z-ADD     S4P(4)        SATSDh
265700090916     C                   Z-ADD     S4P(5)        SATSDx
265800970207      * SPEDIZIONI: a terra non da partire
265900970207     C                   Z-ADD     S4R(1)        SATSNE
266000970207     C                   Z-ADD     S4R(2)        SATSND
266100970207     C                   Z-ADD     S4R(3)        SATSNC
266200090916     C                   Z-ADD     S4R(4)        SATSNh
266300090916     C                   Z-ADD     S4R(5)        SATSNx
266400970305      * SPEDIZIONI: a terra da partire fuori affidabilità
266500970305     C                   Z-ADD     F4F(1)        SATSFE
266600970305     C                   Z-ADD     F4F(2)        SATSFD
266700970305     C                   Z-ADD     F4F(3)        SATSFC
266800090916     C                   Z-ADD     F4F(4)        SATSFh
266900090916     C                   Z-ADD     F4F(5)        SATSFx
267000000000      * SPEDIZIONI: complete
267100000000     C                   Z-ADD     S4M(1)        SATSCE
267200970207     C                   Z-ADD     S4M(2)        SATSCD
267300970207     C                   Z-ADD     S4M(3)        SATSCC
267400090916     C                   Z-ADD     S4M(4)        SATSCh
267500090916     C                   Z-ADD     S4M(5)        SATSCx
267600000000      *
267700970318     C     SATSDE        IFGT      *ZERO
267800970318     C     SATSDD        ORGT      *ZERO
267900970318     C     SATSDC        ORGT      *ZERO
268000090916     C     SATSDh        ORGT      *ZERO
268100090916     C     SATSDx        ORGT      *ZERO
268200970318     C     SATSNE        ORGT      *ZERO
268300970318     C     SATSND        ORGT      *ZERO
268400970318     C     SATSNC        ORGT      *ZERO
268500090916     C     SATSNh        ORGT      *ZERO
268600090916     C     SATSNx        ORGT      *ZERO
268700970318     C     SATSPE        ORGT      *ZERO
268800970318     C     SATSPD        ORGT      *ZERO
268900970318     C     SATSPC        ORGT      *ZERO
269000090916     C     SATSPh        ORGT      *ZERO
269100090916     C     SATSPx        ORGT      *ZERO
269200000405     C     SATVNE        ORGT      *ZEROS
269300000405     C     SATVND        ORGT      *ZEROS
269400000405     C     SATVNC        ORGT      *ZEROS
269500090916     C     SATVNh        ORGT      *ZEROS
269600090916     C     SATVNx        ORGT      *ZEROS
269700940323      *
269800940428     C                   MOVE      $FTR          SATFTR
269900970207     C                   MOVE      $DTR          SATDTR
270000940323      *
270100090916     C                   WRITE     fisat000
270200940429     C* PER LA STAMPA DEI C/S
270300940429     C                   Z-ADD     1             $PRIM2
270400940429     C* PER LA STAMPA DEI TOTALI DI AREA
270500940429     C                   Z-ADD     1             $PRIM4
270600000000     C                   ENDIF
270700000000      *
270800000000      * Aggiorno le schiere per i totali c/servizio per area
270900090916     C                   DO        5             $U                1 0
271000970210     C* A TERRA
271100000000     C                   ADD       C4P($U)       C1P($U)
271200000000     C                   ADD       C4R($U)       C1R($U)
271300000000     C                   ADD       P4I($U)       P1I($U)
271400970207     C                   ADD       P4N($U)       P1N($U)
271500000000     C                   ADD       P4P($U)       P1P($U)
271600000000     C                   ADD       P4R($U)       P1R($U)
271700000000     C                   ADD       V4P($U)       V1P($U)
271800000331     C                   ADD       V4R($U)       V1R($U)
271900000000     C                   ADD       S4P($U)       S1P($U)
272000000000     C                   ADD       S4R($U)       S1R($U)
272100000000     C                   ADD       S4M($U)       S1M($U)
272200970305     C                   ADD       F4F($U)       F1F($U)
272300970210     C* DA PARTIRE
272400970210     C                   ADD       C4A($U)       C1A($U)
272500970210     C                   ADD       P4A($U)       P1A($U)
272600970210     C                   ADD       V4A($U)       V1A($U)
272700970210     C                   ADD       S4A($U)       S1A($U)
272800000000     C                   ENDDO
272900000000      *
273000000000     C                   EXSR      AZZSE4
273100000000      *
273200000000     C                   ENDSR
273300000000      **************************************************************************
273400090916      * Scrittura del record transito sul file di trasmissione fisat00F
273500000000      **************************************************************************
273600000000     C     WR2SAT        BEGSR
273700000000      *
273800090916     C                   CLEAR                   fisat000
273900000000     C                   Z-ADD     SIMFEL        SATFLE
274000940502     C                   Z-ADD     SIMFEL        SATLNP
274100000000     C                   Z-ADD     *ZERO         SATTFA
274200950210     C                   Z-ADD     DS1ADA        SATDRE
274300000000     C                   MOVE      'T'           SATTRE
274400000000      *
274500000000      * COLLI: da partire
274600970211     C                   Z-ADD     0             SATCPE
274700970211     C                   Z-ADD     0             SATCPD
274800970211     C                   Z-ADD     0             SATCPC
274900090916     C                   Z-ADD     0             SATCPh
275000090916     C                   Z-ADD     0             SATCPx
275100970211      * COLLI:
275200970211     C                   Z-ADD     C2P(1)        SATCDE
275300970211     C                   Z-ADD     C2P(2)        SATCDD
275400970211     C                   Z-ADD     C2P(3)        SATCDC
275500090916     C                   Z-ADD     C2P(4)        SATCDh
275600090916     C                   Z-ADD     C2P(5)        SATCDx
275700970211      * PESO: da partire
275800970211     C                   Z-ADD     0             SATPPE
275900970211     C                   Z-ADD     0             SATPPD
276000970211     C                   Z-ADD     0             SATPPC
276100090916     C                   Z-ADD     0             SATPPh
276200090916     C                   Z-ADD     0             SATPPx
276300000000      * PESO: fino a 35 quintali
276400970211     C                   Z-ADD     P2I(1)        SATFDE
276500970211     C                   Z-ADD     P2I(2)        SATFDD
276600970211     C                   Z-ADD     P2I(3)        SATFDC
276700090916     C                   Z-ADD     P2I(4)        SATFDh
276800090916     C                   Z-ADD     P2I(5)        SATFDx
276900940428      * PESO: (non esiste il bollettato nel periodo o nel precedente)
277000970211     C                   Z-ADD     P2P(1)        SATODE
277100970211     C                   Z-ADD     P2P(2)        SATODD
277200970211     C                   Z-ADD     P2P(3)        SATODC
277300090916     C                   Z-ADD     P2P(4)        SATODh
277400090916     C                   Z-ADD     P2P(5)        SATODx
277500000000      * VOLUME: da partire
277600970211     C                   Z-ADD     0             SATVPE
277700970211     C                   Z-ADD     0             SATVPD
277800970211     C                   Z-ADD     0             SATVPC
277900090916     C                   Z-ADD     0             SATVPh
278000090916     C                   Z-ADD     0             SATVPx
278100970211      * VOLUME
278200970211     C                   Z-ADD     V2P(1)        SATVDE
278300970211     C                   Z-ADD     V2P(2)        SATVDD
278400970211     C                   Z-ADD     V2P(3)        SATVDC
278500090916     C                   Z-ADD     V2P(4)        SATVDh
278600090916     C                   Z-ADD     V2P(5)        SATVDx
278700000000      * SPEDIZIONI: da partire
278800970211     C                   Z-ADD     0             SATSPE
278900970211     C                   Z-ADD     0             SATSPD
279000970211     C                   Z-ADD     0             SATSPC
279100090916     C                   Z-ADD     0             SATSPh
279200090916     C                   Z-ADD     0             SATSPx
279300970211      * SPEDIZIONI
279400970211     C                   Z-ADD     S2P(1)        SATSDE
279500970211     C                   Z-ADD     S2P(2)        SATSDD
279600970211     C                   Z-ADD     S2P(3)        SATSDC
279700090916     C                   Z-ADD     S2P(4)        SATSDh
279800090916     C                   Z-ADD     S2P(5)        SATSDx
279900000000      * SPEDIZIONI: complete
280000940509     C                   Z-ADD     S2M(1)        SATSCE
280100970211     C                   Z-ADD     S2M(2)        SATSCD
280200970211     C                   Z-ADD     S2M(3)        SATSCC
280300090916     C                   Z-ADD     S2M(4)        SATSCh
280400090916     C                   Z-ADD     S2M(5)        SATSCx
280500000000      *
280600970212     C     SATSDE        IFGT      *ZERO
280700970212     C     SATSDD        ORGT      *ZERO
280800970212     C     SATSDC        ORGT      *ZERO
280900090916     C     SATSDH        ORGT      *ZERO
281000090916     C     SATSDX        ORGT      *ZERO
281100940428     C                   MOVE      $FTR          SATFTR
281200970207     C                   MOVE      $DTR          SATDTR
281300940323      *
281400090916     C                   WRITE     fisat000
281500940429     C* PER LA STAMPA DEI TOTALI DI AREA
281600940429     C                   Z-ADD     1             $PRIM4
281700940505     C                   Z-ADD     1             $PRIM3
281800000000     C                   ENDIF
281900000000      *
282000000000     C                   ENDSR
282100940428      **************************************************************************
282200940428      * Stampo i totali generali
282300940428      **************************************************************************
282400940428     C     TOTGEN        BEGSR
282500940428      *
282600940428      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
282700940428      *
282800940506      * stampo i totali generali finali solo se ho stampato i transiti
282900950628    1C     *IN02         IFEQ      *ON
283000940506     C     $PRIM3        ANDEQ     1
283100940506     C* I TOTALI DI AREA SEMPRE
283200940506     C     *IN02         OREQ      *OFF
283300940506     C*
283400940428     C                   SETON                                        40
283500940428     C                   SETOFF                                       41
283600970211     C  N02              SETON                                        45
283700970211     C   02              SETOFF                                       45
283800940428     C                   Z-ADD     *ZERO         FILIAP
283900940428     C                   MOVE      *BLANKS       ORGDEP
284000940428     C   02'TOT.GENE'    CAT       'RALE':0      ORGDEP
284100940428     C  N02              MOVEL     'TOTALE'      ORGDEP
284200940428     C*
284300950118     C                   WRITE     LSC2T3
284400940428      *
284500940428      * ?  C  O   L   L  I  ?
284600970211     C                   XFOOT     C3P           COTOTP
284700970211     C                   XFOOT     C3A           $CDA
284800940428     C                   EXSR      PERCOL
284900940428      *
285000940428      * ?  P  E  S  O  ?
285100970211     C                   XFOOT     P3I           PEF35P
285200970211     C                   XFOOT     P3P           PEO35P
285300970211     C     PEF35P        ADD       PEO35P        PETOTP
285400970228     C                   XFOOT     P3A           $PDA
285500970211     C                   EXSR      PERPES
285600970211      *
285700970211      * ?  V  O  L  U  M E  ?
285800970211     C                   XFOOT     V3P           VOTOTP
285900970211     C                   XFOOT     V3A           $VDA
286000940428     C                   EXSR      PERVOL
286100940428      *
286200940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
286300970211     C                   XFOOT     S3A           $SDA
286400970305     C                   XFOOT     F3F           SPFAFF
286500940502     C                   EXSR      PERSPE
286600970305     C                   XFOOT     S3P           SPTOTP
286700970305     C                   XFOOT     S3M           SPCONP
286800970211     C                   XFOOT     S3R           $COMO1
286900970211     C     SPTOTP        ADD       $COMO1        SPTOTA
287000970211     C     SPTOTA        SUB       SPCONP        SPSPAP
287100000407     C*
287200000407      * ?VOLUME A MAGAZZINO ?
287300000407     C                   XFOOT     V3R           VOTOTM
287400940428      *
287500940428     C                   EXSR      SCRIVI
287600940509     C                   ADD       1             $RIGHE
287700940428      *
287800090916      * ------------------ ESPR./DIST./CORR./1030 ---------------------
287900940428      *
288000970211     C                   SETOFF                                       40
288100090916    2C                   DO        CICLO         $V
288200940428     C                   MOVE      ENS($V)       TIPOP
288300940428      *
288400940428      * ?  C  O   L   L  I  ?
288500970211     C                   Z-ADD     C3P($V)       COTOTP
288600970211     C                   Z-ADD     C3A($V)       $CDA
288700940428     C                   EXSR      PERCOL
288800940428      *
288900940428      * ?  P  E  S  O  ?
289000940428     C                   Z-ADD     P3I($V)       PEF35P
289100970211     C                   Z-ADD     P3P($V)       PEO35P
289200970211     C     PEF35P        ADD       PEO35P        PETOTP
289300970211     C                   Z-ADD     P3A($V)       $PDA
289400940428     C                   EXSR      PERPES
289500940428      *
289600940428      * ?  V  O  L  U  M E  ?
289700970211     C                   Z-ADD     V3P($V)       VOTOTP
289800970211     C                   Z-ADD     V3A($V)       $VDA
289900940428     C                   EXSR      PERVOL
290000940428      *
290100940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
290200970211     C                   Z-ADD     S3A($V)       $SDA
290300970305     C                   Z-ADD     F3F($V)       SPFAFF
290400940502     C                   EXSR      PERSPE
290500970305     C                   Z-ADD     S3P($V)       SPTOTP
290600970305     C                   Z-ADD     S3M($V)       SPCONP
290700970211     C     S3P($V)       ADD       S3R($V)       SPTOTA
290800970211     C     SPTOTA        SUB       S3M($V)       SPSPAP
290900000406      * ?VOLUME A MAGAZZINO ?
291000000406     C                   Z-ADD     V3R($V)       VOTOTM
291100940428      *
291200940428     C                   EXSR      SCRIVI
291300940428      *
291400950628    2C                   ENDDO
291500940428      *
291600950118     C  N02              WRITE     LSC2T2
291700950628    1C                   ENDIF
291800940429     C*
291900950118     C   02              WRITE     LSC2T3
292000940428     C*
292100940428     C   02              EXSR      AZZTOT
292200940428      *
292300940428     C                   ENDSR
292400000000      **************************************************************************
292500000000      * Elaborazione transiti
292600940505      * Non posso leggere per data uscita dal transito perche' spesso
292700940505      * capita che ci siano bolle con data entrata un giorno e data
292800940505      * uscita il giorno prima: per questo elaboro tutte le bolle
292900950210      * che hanno la data di entrata al transito > 0 e <=a DS1AAL
293000000000      **************************************************************************
293100000000     C     ELATRA        BEGSR
293200000000      *
293300940505     C                   Z-ADD     0             $PRIM1
293400940428     C                   MOVE      *BLANK        $BLTPR
293500940429     C                   MOVE      *BLANK        $CBLPR
293600940428     C                   MOVE      'T'           $FLAG
293700000000      *
293800000000      * Leggo il file delle bolle in transito
293900940505     C                   Z-ADD     1             $DFV
294000020625     C     kbtt39        SETLL     FNBTT39L
294100020625     C     simfel        READe     FNBTT39L                               33
294200940428     C*
294300940428    2C     *IN33         DOWEQ     *OFF
294400950210     C     BTTDET        ANDLE     DS1AAL
294500000000      *
294600940505     C                   EXSR      ESCLU1
294700940428      *
294800940505    3C     $OK           IFEQ      'S'
294900940505     C                   Z-ADD     1             $PRIM1
295000000000      *
295100000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
295200000000      * poiche' lo dovro aggiornare
295300090916     C                   CLEAR                   fisat000
295400000000     C                   Z-ADD     SIMFEL        SATFLE
295500950210     C                   Z-ADD     DS1ADA        SATDRE
295600940502     C                   Z-ADD     SIMFEL        SATLNP
295700950118     C                   Z-ADD     BTPTFA        SATTFA
295800000000     C                   MOVE      'T'           SATTRE
295900090916     C     FNSA1K        CHAIN     fisat02L                           37
296000940428      *
296100021217      * se è la prima volta che leggo la bolla carico i dati: VOL e PKG
296200021217    4C     $BOLLA        IFEQ      '1'
296300021217     C                   EXSR      DATBOT
296400021217    4C                   ENDIF
296500021217      *
296600021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
296700030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
296800030213      * Peso medio
296900030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
297000030213if  1c***                if        Wsipes = 'S'
297100030213     c***                z-add     BTTpuc        $Peso
297200030213   x1c***                else
297300021220     c                   z-add     Wpesmed       $Peso
297400030213e   1c***                endif
297500021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
297600030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
297700030213      * Volume medio
297800030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
297900030213if  1c***                if        Wsivol = 'S'
298000030213     c***                z-add     BTTvuc        $Volum
298100030213   x1c***                else
298200021220     c                   z-add     Wvolmed       $Volum
298300030213e   1c***                endif
298400000000      *
298500970211      * Verifico il tipo spedizione ESPR./DIST./CORR.
298600090916     C                   Z-ADD     5             $T
298700090916     c                   select
298800090916    4C     BTPTSP        wheneq    'E'
298900000000     C                   Z-ADD     1             $T
299000090916    4C     BTPTSP        wheneq    'D'
299100940323     C                   Z-ADD     2             $T
299200090916    4C     BTPTSP        wheneq    'C'
299300090916     C                   Z-ADD     3             $T
299400090916    4C     BTPTSP        wheneq    'H'
299500090916     C                   Z-ADD     4             $T
299600090916    4C                   ENDSL
299700000000      *
299800000000      * Ho due controlli da fare per sapere se il collo e' a terra:
299900940428      *    1)  BTTDUT = 0
300000940428      *    2)  BTTDUT > Data spedizione
300100000000     C                   SETOFF                                       36
300200940428     C     BTTDUT        IFEQ      0
300300950210     C     BTTDUT        ORGT      DS1AAL
300400000000     C                   SETON                                        36
300500000000     C                   ENDIF
300600940506      *
300700000000      * Sommo solo se il collo risulta a terra
300800940428    4C     *IN36         IFEQ      *ON
300900940506      *
301000940506      * ?  CALCOLI  SUI  COLLI  ?
301100940506      *
301200940509     C                   ADD       1             C2P($T)
301300940428     C                   ADD       1             C0P($T)
301400000000      *
301500000000      * ?  CALCOLI  SUL  PESO  ?
301600000000      *
301700000000      * Verifico il peso di 35 quintali
301800021217    5***C     BTPPKB        IFLE      3500
301900021217    5C     Wpeso         IFLE      3500
302000940509     C                   ADD       $PESO         P2I($T)
302100000000     C                   ADD       $PESO         P0I($T)
302200970211     C                   ELSE
302300970211      * Oltre 35 quintali:                                             (35ON)
302400970211     C                   ADD       $PESO         P2P($T)
302500970211     C                   ADD       $PESO         P0P($T)
302600940428    5C                   ENDIF
302700000000      *
302800000000      *
302900000000      * ?  CALCOLI  SUL  VOLUME  ?
303000000000      *
303100021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
303200021217     **5C     BTTVUC        IFEQ      *ZERO
303300021217     *** *
303400021217     ***C     BTPVLB        DIV(H)    BTPNCL        $VOLUM
303500021217     *** *
303600021217     **5C                   ELSE
303700021217     ***C                   Z-ADD     BTTVUC        $VOLUM
303800021217     **5C                   ENDIF
303900000000      *
304000940509     C                   ADD       $VOLUM        V2P($T)
304100940428     C                   ADD       $VOLUM        V0P($T)
304200000000      *
304300000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
304400000000      *
304500000000      * Visto che le spedizioni non riesco a calcolarle ora, mi limito a tenere
304600000000      * aggiornato il file di work con le bolle
304700950118     C                   Z-ADD     BTPLNP        WRKLNP
304800000000     C                   MOVE      *BLANK        WRKCSE
304900970409     C     FNWR2K        CHAIN     FNWRK00F                           99
305000940428    5C     *IN99         IFEQ      *OFF
305100000000     C                   ADD       1             WRKNC1
305200000000     C                   EXCEPT    AGGIO
305300940428   X5C                   ELSE
305400950118     C                   MOVEL     'T'           WTRANS
305500940503     C                   EXSR      WRIWRK
305600940428    5C                   ENDIF
305700940428      *
305800040921     C                   MOVE      $FTR          SATFTR
305900040921     C                   MOVE      $DTR          SATDTR
306000940428    5C     *IN37         IFEQ      *ON
306100040921     C***                MOVE      $FTR          SATFTR
306200040921     C***                MOVE      $DTR          SATDTR
306300090916     C                   WRITE     fisat000
306400940428   X5C                   ELSE
306500090916     C                   UPDATE    fisat000
306600940428    5C                   ENDIF
306700000000      *
306800940428    4C                   ENDIF
306900940428    3C                   ENDIF
307000000000      *
307100020625     C     simfel        READE     FNBTT39L                               33
307200940428    2C                   ENDDO
307300940506     C*
307400000000     C     $PRIM1        IFNE      *ZERO
307500940506     C                   EXSR      CALSPE
307600000000     C                   EXSR      WR2SAT
307700000000     C                   ENDIF
307800000000      *
307900000000     C                   ENDSR
308000940503      **************************************************************************
308100000411      * elimino i terminal che non hanno VOLUMI TOT A MAG>0
308200940503      **************************************************************************
308300940503     C     ELATER        BEGSR
308400950210     C                   Z-ADD     DS1FEL        $AREA
308500940503     C                   CLEAR                   $TFA
308600940503     C                   SETOFF                                       33
308700940503     C*
308800940503     C* ELABORO TERMINAL PER TERMINAL
308900940503     C                   EXSR      IMPTFA
309000940503     C*
309100940503    2C     *IN33         DOWEQ     *OFF
309200940503     C                   Z-ADD     *ZERO         $PRIM1
309300940503      *
309400090916     C     TASATK        READE(N)  fisat03L                               33
309500940503     C                   MOVEL     SATTRE        $TRE
309600940503     C*
309700940503    3C     *IN33         DOWEQ     *OFF
309800940503     C*
309900940503    4C     $TRE          IFNE      SATTRE
310000940503     C     $PRIM1        ANDEQ     0
310100090916     C     TRSATK        SETLL     fisat03L
310200090916     C     TRSATK        READE     fisat03L                               33
310300940503     C*
310400940503    5C     *IN33         DOWEQ     *OFF
310500090916     C                   DELETE    fisat003
310600090916     C     TRSATK        READE     fisat03L                               33
310700940503    5C                   ENDDO
310800940503     C                   MOVEL     SATTRE        $TRE
310900940503     C                   CLEAR                   $PRIM1
311000940503    4C                   ENDIF
311100940503     C*
311200000411     C* VEDO SE C'E' ALMENO UN VOLUME A MAGAZZINO
311300000411    4C     SATVNE        IFGT      0
311400000411     C     SATVND        ORGT      0
311500000411     C     SATVNC        ORGT      0
311600940503     C                   Z-ADD     1             $PRIM1
311700940503    4C                   ENDIF
311800090916     C     TASATK        READE(N)  fisat03L                               33
311900940503    3C                   ENDDO
312000940503     C* ELABORO L'ULTIMO
312100940503    3C     $PRIM1        IFEQ      0
312200090916     C     TRSATK        SETLL     fisat03L
312300090916     C     TRSATK        READE     fisat03L                               33
312400970211     C*
312500940503    4C     *IN33         DOWEQ     *OFF
312600090916     C                   DELETE    fisat003
312700090916     C     TRSATK        READE     fisat03L                               33
312800940503    4C                   ENDDO
312900940503    3C                   ENDIF
313000940503     C*
313100940503     C                   EXSR      IMPTFA
313200940503    2C                   ENDDO
313300940504     C*
313400940504     C                   ENDSR
313500000000      **************************************************************************
313600000000      * Stampo i terminal di arrivo
313700000000      **************************************************************************
313800000000     C     STATER        BEGSR
313900940429     C                   SETOFF                                       4333
314000940429     C                   SETON                                        30
314100950210     C                   Z-ADD     DS1FEL        $AREA
314200940429     C                   CLEAR                   $TFA
314300940509     C                   CLEAR                   $RIGHE
314400940429     C* ELABORO AREA PER AREA
314500940429     C  N39              EXSR      IMPARE
314600940429     C*
314700940429    2C     *IN33         DOWEQ     *OFF
314800940429     C*
314900940429     C* ELABORO TERMINAL PER TERMINAL
315000940429     C                   EXSR      IMPTFA
315100000000      *
315200940429    3C     *IN33         DOWEQ     *OFF
315300940429     C                   Z-ADD     *ZERO         $PRIM1
315400940429     C                   Z-ADD     *ZERO         $PRIM3
315500000000      *
315600090916     C     ARSATK        READE(N)  fisat03L                               33
315700940429     C*
315800940429    4C     *IN33         DOWEQ     *OFF
315900000000      *
316000940429     C* ELABORO SE DATA COMPRESA
316100940429     C* E, SE IMMESSA SOLTANTO UNA LINEA, SE E' LA LINEA RICHIESTA
316200970211    5C     SATDRE        IFLE      DS1AAL
316300940429     C*
316400940429     C* SE DA STAMPE GIORNALIERE ELABORO SLTANTO LE LINEE IN LIN
316500940429     C     SATLNP        LOOKUP    LIN                                    99
316600940429     C   99              EXSR      SERTRA
316700940429     C*
316800940429   X5C                   ELSE
316900940429     C                   SETON                                        33
317000940429    5C                   ENDIF
317100940429     C*
317200090916     C     ARSATK        READE(N)  fisat03L                               33
317300940429    4C                   ENDDO
317400940429      *
317500940429     C* Stampo le bolle dell'area
317600940429    4C     $PRIM1        IFEQ      1
317700940429     C                   EXSR      TOTFIL
317800940429     C                   EXSR      AZZFIL
317900940429    4C                   ENDIF
318000940429      *
318100940429      * Stampo le bolle in transito
318200940429    4C     $PRIM3        IFEQ      1
318300940429     C                   EXSR      TOTTRA
318400940429    4C                   ENDIF
318500940429     C*
318600940429     C* Elaboro nuovo terminal
318700940429     C                   EXSR      IMPTFA
318800940429    3C                   ENDDO
318900940429      *
319000940509     C                   Z-ADD     1             $PRIM3
319100970226     C                   SETON                                        0204
319200940429     C                   EXSR      TOTGEN
319300970226     C                   SETOFF                                       0204
319400940429     C* ELABORO NUOVA AREA
319500940429     C  N39              EXSR      IMPARE
319600940429    2C                   ENDDO
319700940429     C*
319800940429     C                   ENDSR
319900000000      **************************************************************************
320000000000      * Una volta letto il record verifico se da includere o no
320100000000      *
320200940428      * Includo se:
320300940428      * e' entrato al transito
320400940428      *  e non e' ancora uscito o e' uscito con data = > a  data elabor
320500940505      *            BTTDUT = 0
320600000000      *   oppure:
320700950210      *       AND  BTTDUT >= DS1ADAL
320800940428      *
320900940428      * escludo sempre i recuperi
321000000000      **************************************************************************
321100000000     C     ESCLU1        BEGSR
321200000000      *
321300000000     C                   MOVE      'N'           $OK
321400000000      *
321500000000     C     BTTDUT        IFEQ      *ZERO
321600950210     C     BTTDUT        ORGE      DS1AAL
321700000000     C                   MOVE      'S'           $OK
321800940428     C                   ENDIF
321900000000      *
322000000000      * Solo se superati i test ne eseguo altri
322100000000     C     $OK           IFEQ      'S'
322200940505      *
322300940505      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
322400940505      * come test ma mi permette di saltare delle chain)
322500950118    3C     $BTTAT        IFNE      $BLTPR
322600020625     C     FNBTPK        CHAIN     FNBTP11L                           99
322700950118     C  N99              MOVE      $BTTAT        $BLTPR
322800940505     C  N99              MOVEL     '1'           $BOLLA
322900940505     C   99              MOVEL     'N'           $OK
323000940505    3C                   ENDIF
323100000000      *
323200000000      * Eseguo la chain su TABEL solo se diverso dal precedente
323300950118     C  N99BTPCBO        IFNE      $CBLPR
323400950210     C                   MOVE      '3A'          KCOD
323500950210     C                   MOVE      *BLANKS       KKEY
323600950210     C                   MOVEL     BTPCBO        KKEY
323700950210     C     KTAB1         CHAIN     TABEL00F                           99
323800000000     C  N99              MOVEL     TBLUNI        DS3A
323900950118     C  N99              MOVE      BTPCBO        $CBLPR
324000000000     C   99              CLEAR                   DS3A
324100000000     C                   ENDIF
324200000000      *
324300000000      * Non considero i recuperi
324400940428     C     §3ARBL        IFEQ      'R'
324500000000     C                   MOVE      'N'           $OK
324600000000     C                   ENDIF
324700000000      *
324800000000     C                   ENDIF
324900000000      *
325000000000     C                   ENDSR
325100000000      **************************************************************************
325200090916      * RISTAMPA: lettura dal file statistiche (fisat00F) e output
325300000000      **************************************************************************
325400000000     C     RISTAM        BEGSR
325500000000      *
325600940502     C                   SETOFF                                       33
325700940428     C                   SETON                                        43
325800940429     C                   CLEAR                   $TFA
325900940509     C                   CLEAR                   $RIGHE
326000950210     C                   MOVEL     DS1FEL        $AREA
326100940429     C* LANCIO DA FILTRO
326200950210    1C     DS1FGS        IFGT      0
326300950210     C                   MOVEL     DS1LNP        SATLNP
326400940429     C*
326500940429     C* SE NON IMMESSA L'AREA LA CERCO
326600940429     C  N39              EXSR      IMPARE
326700940429     C*
326800940429    2C     *IN33         DOWEQ     *OFF
326900940429     C                   Z-ADD     *ZERO         $PRIM2
327000940429     C                   Z-ADD     *ZERO         $PRIM3
327100940429     C                   Z-ADD     *ZERO         $PRIM4
327200940429     C*
327300940429     C* SE NON IMMESSA LA LNP LA CERCO
327400940505     C                   EXSR      IMPLNP
327500000000      *
327600940429    3C     *IN33         DOWEQ     *OFF
327700940429     C                   Z-ADD     *ZERO         $PRIM1
327800940429     C* LEGGO IL FILE PER LNP
327900940429     C                   EXSR      LEGSAT
328000940429     C* cerco la nuova lnp
328100940429     C  N29              EXSR      IMPLNP
328200940429    3C                   ENDDO
328300940429     c*
328400940429     C* stampa finale dei C/S transiti e totale
328500940429     C                   EXSR      STAFIN
328600940429     C* cerco la nuova area
328700940429     C  N39              EXSR      IMPARE
328800940429    2C                   ENDDO
328900000000      *
329000950210   X1C                   ELSE
329100940429      *
329200940429      * Da stampe giornaliere
329300940429     C                   Z-ADD     1             $X
329400940429     C*
329500940506     C                   Z-ADD     *ZERO         $PRIM2            1 0
329600940506     C                   Z-ADD     *ZERO         $PRIM3            1 0
329700940506     C                   Z-ADD     *ZERO         $PRIM4            1 0
329800940429    2C     LIN($X)       DOWGT     0
329900940429     C                   Z-ADD     LIN($X)       $LNP
330000940506     C                   Z-ADD     LIN($X)       $FILPR
330100940505     C                   Z-ADD     *ZERO         $PRIM1            1 0
330200000912     C*
330300090916     C     DTSATK        SETLL     fisat02L
330400940429     C* LEGGO IL FILE PER LINEA
330500940429     C                   EXSR      LEGSAT
330600940429     C                   ADD       1             $X
330700940429    2C                   ENDDO
330800940429     c*
330900940429     C                   EXSR      STAFIN
331000000912    1C                   ENDIF
331100940429     C*
331200940429     C                   ENDSR
331300940429      **************************************************************************
331400940429      * IMPOSTO L'AREA, LE CERCO MAN MANO                              i
331500940429      **************************************************************************
331600940429     C     IMPARE        BEGSR
331700090916     C     $AREA         SETGT     fisat02L
331800090916     C                   READ(N)   fisat02L                               33
331900940429     C*
332000940429     C     *IN33         IFEQ      *OFF
332100940429     C                   MOVEL     SATFLE        $AREA
332200940429     C                   ENDIF
332300940429     C                   ENDSR
332400940429      **************************************************************************
332500940429      * IMPOSTO LA LINEA DI PARTENZA, LA CERCO MAN MANO                i
332600940429      **************************************************************************
332700940429     C     IMPLNP        BEGSR
332800090916     C  N29LPSATK        SETGT     fisat02L
332900090916     C  N29ARSATK        READE(N)  fisat02L                               33
333000000619     C                   CLEAR                   WTROVA
333100940429     C*
333200000619    1C     WTROVA        DOWEQ     ' '
333300000619    2C     *IN33         IFEQ      *OFF
333400000619     C     SATLNP        LOOKUP    LIN                                    99
333500000619    3C     *IN99         IFEQ      *ON
333600940429     C                   MOVEL     SATLNP        $LNP
333700940429     C                   MOVEL     SATLNP        $FILPR
333800000619     C                   MOVEL     '1'           WTROVA            1
333900090916     C     DTSATK        SETLL     fisat02L
334000000619   X3C                   ELSE
334100090916     C  N29ARSATK        READE(N)  fisat02L                               33
334200000619    3C                   ENDIF
334300000619     C**
334400000619   X2C                   ELSE
334500000619     C                   MOVEL     '1'           WTROVA
334600000619    2C                   ENDIF
334700000619    1C                   ENDDO
334800000619     C*
334900940429     C                   ENDSR
335000940429      **************************************************************************
335100940429      * IMPOSTO IL TERMINAL, LO CERCO MAN MANO                         i
335200940429      **************************************************************************
335300940429     C     IMPTFA        BEGSR
335400090916     C     ARSATK        SETGT     fisat03L
335500090916     C     $AREA         READE(N)  fisat03L                               33
335600940429     C*
335700940429     C     *IN33         IFEQ      *OFF
335800940429     C                   MOVEL     SATTFA        $TFA
335900940429     C                   MOVEL     SATTFA        $FILPR
336000090916     C     TASATK        SETLL     fisat03L
336100940429     C                   ENDIF
336200940429     C                   ENDSR
336300940429      **************************************************************************
336400940429      * STAMPA FINALE DEI C/S E TRANSITI                               i
336500940429      **************************************************************************
336600940429     C     STAFIN        BEGSR
336700940429     C* stampo i totali C/S
336800940506     C     *IN38         IFEQ      *OFF
336900940429     C     $PRIM2        IFEQ      1
337000940429     C                   EXSR      TOTSER
337100940429     C                   ENDIF
337200940506     C* TOTALI DI AREA
337300970226     C                   SETON                                        04
337400940505     C                   EXSR      TOTGEN
337500970226     C                   SETOFF                                       04
337600940506     C                   ENDIF
337700940502     C*
337800940502      * Terminate le letture per filiale tocca ai transiti
337900950210     C     DS1RIS        IFNE      'R'
338000940502     C                   EXSR      ELATRA
338100940503     C*
338200940505     C* CANCELLO LE RIGHE DEI TERMINAL CHE NON HANNO COLLI A TERRA
338300940503     C                   EXSR      ELATER
338400940502     C                   ENDIF
338500940502     C*
338600940429     C* stampo i totali dei TRANSITI
338700940506     C     *IN38         IFEQ      *OFF
338800000308     C     *IN27         IFEQ      *OFF
338900000308     C     DS1RIS        OREQ      'R'
339000000308     C                   EXSR      TOTTRA
339100000308     C                   ENDIF
339200940429     c* stampo il totale di area
339300940429     C     $PRIM4        IFEQ      1
339400970226     C                   SETON                                        0204
339500940429     C                   EXSR      TOTGEN
339600970226     C                   SETOFF                                       0204
339700950118     C                   WRITE     LSC2E4
339800940429     C                   ENDIF
339900940506     C                   ENDIF
340000940429     C                   ENDSR
340100940429      **************************************************************************
340200950210      * IMPOSTO L'AREA, LE CERCO MAN MANO
340300940429      **************************************************************************
340400940429     C     LEGSAT        BEGSR
340500090916     C     LPSATK        READE(N)  fisat02L                               33
340600940429     C*
340700940429    4C     *IN33         DOWEQ     *OFF
340800940429      *
340900970211    5C     SATDRE        IFLE      DS1AAL
341000940429      * Aggiorno i totali
341100940429     C                   EXSR      SERTRA
341200940429   x5C                   ELSE
341300940429     C                   SETON                                        33
341400940429    5C                   ENDIF
341500940429     C*
341600090916     C  N33LPSATK        READE(N)  fisat02L                               33
341700940429    4C                   ENDDO
341800940429     C* totali di filiale
341900940429    4C     $PRIM1        IFEQ      1
342000940429     C                   EXSR      TOTFIL
342100940429     C                   EXSR      AZZFIL
342200940429    4C                   ENDIF
342300940429     C                   ENDSR
342400000000      **************************************************************************
342500000000      * RISTAMPA: per servizi o transiti non stampo ma aggiorno i totali
342600000000      **************************************************************************
342700000000     C     SERTRA        BEGSR
342800000000      *
342900000000     C     SATTRE        IFEQ      *BLANK
343000940429     C                   Z-ADD     1             $PRIM1
343100970211     C                   ADD       SATCPE        CPA(1)
343200970211     C                   ADD       SATCPD        CPA(2)
343300970211     C                   ADD       SATCPC        CPA(3)
343400090916     C                   ADD       SATCPh        CPA(4)
343500090916     C                   ADD       SATCPx        CPA(5)
343600090916
343700970211     C                   ADD       SATCDE        CBP(1)
343800970211     C                   ADD       SATCDD        CBP(2)
343900970228     C                   ADD       SATCDC        CBP(3)
344000090916     C                   ADD       SATCDh        CBP(4)
344100090916     C                   ADD       SATCDx        CBP(5)
344200090916
344300970211     C                   ADD       SATCNE        CBR(1)
344400970211     C                   ADD       SATCND        CBR(2)
344500970211     C                   ADD       SATCNC        CBR(3)
344600090916     C                   ADD       SATCNh        CBR(4)
344700090916     C                   ADD       SATCNx        CBR(5)
344800090916
344900970211     C                   ADD       SATPPE        PPA(1)
345000970211     C                   ADD       SATPPD        PPA(2)
345100970211     C                   ADD       SATPPC        PPA(3)
345200090916     C                   ADD       SATPPh        PPA(4)
345300090916     C                   ADD       SATPPx        PPA(5)
345400090916
345500970211     C                   ADD       SATFDE        PFI(1)
345600970211     C                   ADD       SATFDD        PFI(2)
345700970211     C                   ADD       SATFDC        PFI(3)
345800090916     C                   ADD       SATFDh        PFI(4)
345900090916     C                   ADD       SATFDx        PFI(5)
346000090916
346100970211     C                   ADD       SATFNE        PFN(1)
346200970211     C                   ADD       SATFND        PFN(2)
346300970211     C                   ADD       SATFNC        PFN(3)
346400090916     C                   ADD       SATFNh        PFN(4)
346500090916     C                   ADD       SATFNx        PFN(5)
346600090916
346700970211     C                   ADD       SATODE        PBP(1)
346800970211     C                   ADD       SATODD        PBP(2)
346900970211     C                   ADD       SATODC        PBP(3)
347000090916     C                   ADD       SATODh        PBP(4)
347100090916     C                   ADD       SATODx        PBP(5)
347200090916
347300970211     C                   ADD       SATONE        PBR(1)
347400970211     C                   ADD       SATOND        PBR(2)
347500970211     C                   ADD       SATONC        PBR(3)
347600090916     C                   ADD       SATONh        PBR(4)
347700090916     C                   ADD       SATONx        PBR(5)
347800090916
347900970211     C                   ADD       SATVPE        VPA(1)
348000970211     C                   ADD       SATVPD        VPA(2)
348100970211     C                   ADD       SATVPC        VPA(3)
348200090916     C                   ADD       SATVPh        VPA(4)
348300090916     C                   ADD       SATVPx        VPA(5)
348400090916
348500970211     C                   ADD       SATVDE        VBP(1)
348600970211     C                   ADD       SATVDD        VBP(2)
348700970211     C                   ADD       SATVDC        VBP(3)
348800090916     C                   ADD       SATVDh        VBP(4)
348900090916     C                   ADD       SATVDx        VBP(5)
349000090916
349100970211     C                   ADD       SATVNE        VBR(1)
349200970211     C                   ADD       SATVND        VBR(2)
349300970211     C                   ADD       SATVNC        VBR(3)
349400090916     C                   ADD       SATVNh        VBR(4)
349500090916     C                   ADD       SATVNx        VBR(5)
349600090916
349700970211     C                   ADD       SATSPE        SPA(1)
349800970211     C                   ADD       SATSPD        SPA(2)
349900970211     C                   ADD       SATSPC        SPA(3)
350000090916     C                   ADD       SATSPh        SPA(4)
350100090916     C                   ADD       SATSPx        SPA(5)
350200090916
350300970211     C                   ADD       SATSDE        SBP(1)
350400970211     C                   ADD       SATSDD        SBP(2)
350500970211     C                   ADD       SATSDC        SBP(3)
350600090916     C                   ADD       SATSDh        SBP(4)
350700090916     C                   ADD       SATSDx        SBP(5)
350800090916
350900970211     C                   ADD       SATSNE        SBR(1)
351000970211     C                   ADD       SATSND        SBR(2)
351100970211     C                   ADD       SATSNC        SBR(3)
351200090916     C                   ADD       SATSNh        SBR(4)
351300090916     C                   ADD       SATSNx        SBR(5)
351400090916
351500970305     C                   ADD       SATSFE        FAF(1)
351600970305     C                   ADD       SATSFD        FAF(2)
351700970305     C                   ADD       SATSFC        FAF(3)
351800090916     C                   ADD       SATSFh        FAF(4)
351900090916     C                   ADD       SATSFx        FAF(5)
352000090916
352100000000     C                   ADD       SATSCE        SCM(1)
352200970211     C                   ADD       SATSCD        SCM(2)
352300970211     C                   ADD       SATSCC        SCM(3)
352400090916     C                   ADD       SATSCh        SCM(4)
352500090916     C                   ADD       SATSCx        SCM(5)
352600000000     C                   ENDIF
352700000000      *
352800000000     C     SATTRE        IFEQ      'C'
352900940429     C                   Z-ADD     1             $PRIM2
353000970211     C                   ADD       SATCPE        C1A(1)
353100970211     C                   ADD       SATCPD        C1A(2)
353200970211     C                   ADD       SATCPC        C1A(3)
353300090916     C                   ADD       SATCPh        C1A(4)
353400090916     C                   ADD       SATCPx        C1A(5)
353500090916
353600970211     C                   ADD       SATCDE        C1P(1)
353700970211     C                   ADD       SATCDD        C1P(2)
353800970211     C                   ADD       SATCDC        C1P(3)
353900090916     C                   ADD       SATCDh        C1P(4)
354000090916     C                   ADD       SATCDx        C1P(5)
354100090916
354200970211     C                   ADD       SATCNE        C1R(1)
354300970211     C                   ADD       SATCND        C1R(2)
354400970211     C                   ADD       SATCNC        C1R(3)
354500090916     C                   ADD       SATCNh        C1R(4)
354600090916     C                   ADD       SATCNx        C1R(5)
354700090916
354800970211     C                   ADD       SATPPE        P1A(1)
354900970303     C                   ADD       SATPPD        P1A(2)
355000970303     C                   ADD       SATPPC        P1A(3)
355100090916     C                   ADD       SATPPh        P1A(4)
355200090916     C                   ADD       SATPPx        P1A(5)
355300090916
355400970211     C                   ADD       SATFDE        P1I(3)
355500970211     C                   ADD       SATFDD        P1I(2)
355600970211     C                   ADD       SATFDC        P1I(3)
355700090916     C                   ADD       SATFDh        P1I(4)
355800090916     C                   ADD       SATFDx        P1I(5)
355900090916
356000970211     C                   ADD       SATFNE        P1N(1)
356100970211     C                   ADD       SATFND        P1N(2)
356200970211     C                   ADD       SATFNC        P1N(3)
356300090916     C                   ADD       SATFNh        P1N(4)
356400090916     C                   ADD       SATFNx        P1N(5)
356500090916
356600970211     C                   ADD       SATODE        P1P(1)
356700970211     C                   ADD       SATODD        P1P(2)
356800970211     C                   ADD       SATODC        P1P(3)
356900090916     C                   ADD       SATODh        P1P(4)
357000090916     C                   ADD       SATODx        P1P(5)
357100090916
357200970211     C                   ADD       SATONE        P1R(1)
357300970211     C                   ADD       SATOND        P1R(2)
357400970211     C                   ADD       SATONC        P1R(3)
357500090916     C                   ADD       SATONh        P1R(4)
357600090916     C                   ADD       SATONx        P1R(5)
357700090916
357800970211     C                   ADD       SATVPE        V1A(1)
357900970211     C                   ADD       SATVPD        V1A(2)
358000970211     C                   ADD       SATVPC        V1A(3)
358100090916     C                   ADD       SATVPh        V1A(4)
358200090916     C                   ADD       SATVPx        V1A(5)
358300090916
358400970211     C                   ADD       SATVDE        V1P(1)
358500970211     C                   ADD       SATVDD        V1P(2)
358600970211     C                   ADD       SATVDC        V1P(3)
358700090916     C                   ADD       SATVDh        V1P(4)
358800090916     C                   ADD       SATVDx        V1P(5)
358900090916
359000970211     C                   ADD       SATVNE        V1R(1)
359100970211     C                   ADD       SATVND        V1R(2)
359200970211     C                   ADD       SATVNC        V1R(3)
359300090916     C                   ADD       SATVNh        V1R(4)
359400090916     C                   ADD       SATVNx        V1R(5)
359500090916
359600970211     C                   ADD       SATSPE        S1A(1)
359700970211     C                   ADD       SATSPD        S1A(2)
359800970211     C                   ADD       SATSPC        S1A(3)
359900090916     C                   ADD       SATSPh        S1A(4)
360000090916     C                   ADD       SATSPx        S1A(5)
360100090916
360200970211     C                   ADD       SATSDE        S1P(1)
360300970211     C                   ADD       SATSDD        S1P(2)
360400970211     C                   ADD       SATSDC        S1P(3)
360500090916     C                   ADD       SATSDh        S1P(4)
360600090916     C                   ADD       SATSDx        S1P(5)
360700090916
360800970211     C                   ADD       SATSNE        S1R(1)
360900970211     C                   ADD       SATSND        S1R(2)
361000970211     C                   ADD       SATSNC        S1R(3)
361100090916     C                   ADD       SATSNh        S1R(4)
361200090916     C                   ADD       SATSNx        S1R(5)
361300090916
361400970305     C                   ADD       SATSFE        F1F(1)
361500970305     C                   ADD       SATSFD        F1F(2)
361600970305     C                   ADD       SATSFC        F1F(3)
361700090916     C                   ADD       SATSFh        F1F(4)
361800090916     C                   ADD       SATSFx        F1F(5)
361900090916
362000000000     C                   ADD       SATSCE        S1M(1)
362100970211     C                   ADD       SATSCD        S1M(2)
362200970211     C                   ADD       SATSCC        S1M(3)
362300090916     C                   ADD       SATSCh        S1M(4)
362400090916     C                   ADD       SATSCx        S1M(5)
362500000000     C                   ENDIF
362600000000      *
362700940429     C     SATTRE        IFEQ      'T'
362800940429     C                   Z-ADD     1             $PRIM3
362900970211     C                   ADD       SATCDE        C2P(1)
363000970211     C                   ADD       SATCDD        C2P(2)
363100970211     C                   ADD       SATCDC        C2P(3)
363200090916     C                   ADD       SATCDh        C2P(4)
363300090916     C                   ADD       SATCDx        C2P(5)
363400090916
363500970211     C                   ADD       SATFDE        P2I(1)
363600970211     C                   ADD       SATFDD        P2I(2)
363700970211     C                   ADD       SATFDC        P2I(3)
363800090916     C                   ADD       SATFDh        P2I(4)
363900090916     C                   ADD       SATFDx        P2I(5)
364000090916
364100970211     C                   ADD       SATODE        P2P(1)
364200970211     C                   ADD       SATODD        P2P(2)
364300970211     C                   ADD       SATODC        P2P(3)
364400090916     C                   ADD       SATODh        P2P(4)
364500090916     C                   ADD       SATODx        P2P(5)
364600090916
364700970211     C                   ADD       SATVDE        V2P(1)
364800970211     C                   ADD       SATVDD        V2P(2)
364900970211     C                   ADD       SATVDC        V2P(3)
365000090916     C                   ADD       SATVDh        V2P(4)
365100090916     C                   ADD       SATVDx        V2P(5)
365200090916
365300970211     C                   ADD       SATSDE        S2P(1)
365400970211     C                   ADD       SATSDD        S2P(2)
365500970211     C                   ADD       SATSDC        S2P(3)
365600090916     C                   ADD       SATSDh        S2P(4)
365700090916     C                   ADD       SATSDx        S2P(5)
365800090916
365900000000     C                   ADD       SATSCE        S2M(1)
366000970211     C                   ADD       SATSCD        S2M(2)
366100970211     C                   ADD       SATSCC        S2M(3)
366200090916     C                   ADD       SATSCh        S2M(4)
366300090916     C                   ADD       SATSCx        S2M(5)
366400940505     C*
366500940505     C                   ELSE
366600000000      *
366700940429      *  totali generali
366800940429     C                   Z-ADD     1             $PRIM4
366900970211     C                   ADD       SATCDE        C3P(1)
367000970211     C                   ADD       SATCDD        C3P(2)
367100970211     C                   ADD       SATCDC        C3P(3)
367200090916     C                   ADD       SATCDh        C3P(4)
367300090916     C                   ADD       SATCDx        C3P(5)
367400090916
367500970211     C                   ADD       SATFDE        P3I(1)
367600970211     C                   ADD       SATFDD        P3I(2)
367700970211     C                   ADD       SATFDC        P3I(3)
367800090916     C                   ADD       SATFDh        P3I(4)
367900090916     C                   ADD       SATFDx        P3I(5)
368000090916
368100970212     C                   ADD       SATODE        P3P(1)
368200970212     C                   ADD       SATODD        P3P(2)
368300970212     C                   ADD       SATODC        P3P(3)
368400090916     C                   ADD       SATODh        P3P(4)
368500090916     C                   ADD       SATODx        P3P(5)
368600090916
368700970211     C                   ADD       SATVDE        V3P(1)
368800970211     C                   ADD       SATVDD        V3P(2)
368900970211     C                   ADD       SATVDC        V3P(3)
369000090916     C                   ADD       SATVDh        V3P(4)
369100090916     C                   ADD       SATVDx        V3P(5)
369200090916
369300000406     C                   ADD       SATVNE        V3R(1)
369400000406     C                   ADD       SATVND        V3R(2)
369500000406     C                   ADD       SATVNC        V3R(3)
369600090916     C                   ADD       SATVNh        V3R(4)
369700090916     C                   ADD       SATVNx        V3R(5)
369800090916
369900970211     C                   ADD       SATSDE        S3P(1)
370000970211     C                   ADD       SATSDD        S3P(2)
370100970211     C                   ADD       SATSDC        S3P(3)
370200090916     C                   ADD       SATSDh        S3P(4)
370300090916     C                   ADD       SATSDx        S3P(5)
370400090916
370500970211     C                   ADD       SATSNE        S3R(1)
370600970211     C                   ADD       SATSND        S3R(2)
370700970211     C                   ADD       SATSNC        S3R(3)
370800090916     C                   ADD       SATSNh        S3R(4)
370900090916     C                   ADD       SATSNx        S3R(5)
371000090916
371100970305     C                   ADD       SATSFE        F3F(1)
371200970305     C                   ADD       SATSFD        F3F(2)
371300970305     C                   ADD       SATSFC        F3F(3)
371400090916     C                   ADD       SATSFh        F3F(4)
371500090916     C                   ADD       SATSFx        F3F(5)
371600090916
371700000000     C                   ADD       SATSCE        S3M(1)
371800970211     C                   ADD       SATSCD        S3M(2)
371900970211     C                   ADD       SATSCC        S3M(3)
372000090916     C                   ADD       SATSCh        S3M(4)
372100090916     C                   ADD       SATSCx        S3M(5)
372200940505     C                   ENDIF
372300000000      *
372400000000     C                   ENDSR
372500940505      **************************************************************************
372600940505      * RISTAMPA: addiziono i transiti al totale generale              i
372700940505      **************************************************************************
372800940505     C     ADDTRA        BEGSR
372900940505     C                   Z-ADD     1             $PRIM4
373000940505     C                   ADD       C2P           C3P
373100940505     C                   ADD       P2I           P3I
373200940505     C                   ADD       P2P           P3P
373300940505     C                   ADD       V2P           V3P
373400940505     C                   ADD       S2P           S3P
373500940505     C                   ADD       S2M           S3M
373600940505     C                   ENDSR
373700000000      **************************************************************************
373800000000      * RISTAMPA: azzera i totali generali
373900000000      **************************************************************************
374000000000     C     AZZTOT        BEGSR
374100000000      *
374200970212     C                   CLEAR                   C3A
374300970212     C                   CLEAR                   C3P
374400970211     C                   CLEAR                   P3I
374500970211     C                   CLEAR                   P3P
374600970211     C                   CLEAR                   P3A
374700970211     C                   CLEAR                   V3P
374800940502     C                   CLEAR                   V3A
374900000406     C                   CLEAR                   V3R
375000970211     C                   CLEAR                   S3A
375100970211     C                   CLEAR                   S3P
375200970211     C                   CLEAR                   S3R
375300970211     C                   CLEAR                   S3M
375400970305     C                   CLEAR                   F3F
375500000000      *
375600000000     C                   ENDSR
375700000000      **************************************************************************
375800000000      * Testa l'overflow e stampa su carta
375900000000      **************************************************************************
376000000000     C     SCRIVI        BEGSR
376100940509      * PER TOTALI GENERALI SE GIA' STAMPE PIU' DI 9 RIGHE COMUNQUE
376200940509      * VADO A PAGINA NUOVA
376300940509     C     *IN03         IFEQ      *OFF
376400940509     C     *IN02         ANDEQ     *ON
376500940509     C     $RIGHE        ANDGT     9
376600940509     C                   SETON                                        30
376700940509     C                   ENDIF
376800000000      *
376900940429     C     *IN38         IFEQ      *OFF
377000940509     C*
377100940509     C     *IN30         IFEQ      *ON
377200940509     C     *IN40         ANDEQ     *ON
377300950118     C                   WRITE     LSC2T1
377400950118     C                   WRITE     LSC2T3
377500940509     C                   CLEAR                   $RIGHE
377600940509     C                   SETOFF                                       30
377700940509     C                   ENDIF
377800950118     C                   WRITE     LSC2D1
377900000000      *
378000970226     C  N04$ALMEN        IFEQ      *ZERO
378100000000     C                   Z-ADD     1             $ALMEN
378200000000     C                   ENDIF
378300940328     C                   ENDIF
378400000000      *
378500000000     C                   ENDSR
378600000000      **************************************************************************
378700000000      * Verifica se vi sono dei fogli viaggio con data partenza compresa
378800000000      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
378900000000      **************************************************************************
379000000000     C     FOGVIA        BEGSR
379100020813     ***C                   OPEN      FNFGV04L
379200020813     C                   OPEN      FNFGV06L
379300000000      *
379400020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
379500020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
379600020813     C     SIMfel        setll     FNFGV06L                               99
379700000000     C                   DO        *HIVAL
379800020813     ***C                   READ      FNFGV04L                               99
379900020813     C     SIMfel        reade     FNFGV06L                               99
380000950210     C  N99FGVDTP        IFGE      DS1ADA
380100950210     C     FGVDTP        ANDLE     DS1AAL
380200940502     C                   SETON                                        99
380300950118     C                   WRITE     LSC2E3
380400000000     C                   ENDIF
380500940502     C  N99              ENDDO
380600000000      *
380700020813     ***C                   CLOSE     FNFGV04L
380800020813     C                   CLOSE     FNFGV06L
380900000000     C                   ENDSR
381000000000      **************************************************************************
381100000000      * Aggiornamento del numero colli esaminati, nel file di work
381200950118     OFNWRK000  E            AGGIO
381300000000     O                       WRKNC1
381400000000      **************************************************************************
381500000000      *                                4         5         6         7         8
381600950118** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
381700970807OVRPRTF FNLSC2P       FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.terra
381800950210')
