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
004300020813     FFNFGV06L  IF   E           K DISK    USROPN
004400090916     Ffisat02L  UF A E           K DISK
004500090916     Ffisat03L  UF   E           K DISK
004600090916     F                                     RENAME(fisat000:fisat003)
004700090916     Ffisat04L  UF   E           K DISK    USROPN
004800090916     F                                     RENAME(fisat000:fisat004)
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
006200090916      * Le schiere che seguono hanno 5 elementi:
006300000000      *     1° elemento: spedizione espressa (ESPR.)
006400950627      *     2° elemento: spedizione distribuzione (DISTR)
006500950627      *     3° elemento: spedizione corriere (CORR.)
006600090916      *     4° elemento: spedizione corriere (h1030)
006700090916      *     5° elemento: spedizione corriere (usi futuri)
006800090916     D ENS             S              5    DIM(5)
006900000000      * COLLI
007000090916     D CPA             S              7  0 DIM(5)
007100090916     D CBP             S              7  0 DIM(5)
007200090916     D CBR             S              7  0 DIM(5)
007300000000      * PESI
007400090916     D PPA             S              9  1 DIM(5)
007500090916     D PFI             S              9  1 DIM(5)
007600090916     D PFN             S              9  1 DIM(5)
007700090916     D PBP             S              9  1 DIM(5)
007800090916     D PBR             S              9  1 DIM(5)
007900000000      * VOLUMI
008000090916     D VPA             S              9  4 DIM(5)
008100090916     D VBP             S              9  4 DIM(5)
008200090916     D VBR             S              9  4 DIM(5)
008300000000      * SPEDIZIONI
008400090916     D SPA             S              7  0 DIM(5)
008500090916     D SBP             S              7  0 DIM(5)
008600090916     D SBR             S              7  0 DIM(5)
008700090916     D SCM             S              7  0 DIM(5)
008800090916     D FAF             S              7  0 DIM(5)
008900000000      * COLLI PER C/SERVIZIO (TOTALE AREA) -------------------------------------
009000090916AREA D C1A             S              7  0 DIM(5)
009100090916AREA D C1P             S              7  0 DIM(5)
009200090916AREA D C1R             S              7  0 DIM(5)
009300000000      * PESI PER C/SERVIZIO (TOTALE AREA)
009400090916AREA D P1A             S              9  1 DIM(5)
009500090916AREA D P1I             S              9  1 DIM(5)
009600090916AREA D P1N             S              9  1 DIM(5)
009700090916AREA D P1P             S              9  1 DIM(5)
009800090916AREA D P1R             S              9  1 DIM(5)
009900000000      * VOLUMI PER C/SERVIZIO (TOTALE AREA)
010000090916AREA D V1A             S              9  4 DIM(5)
010100090916AREA D V1P             S              9  4 DIM(5)
010200090916AREA D V1R             S              9  4 DIM(5)
010300000000      * SPEDIZIONI PER C/SERVIZIO (TOTALE AREA)
010400090916AREA D S1A             S              7  0 DIM(5)
010500090916AREA D S1P             S              7  0 DIM(5)
010600090916AREA D S1R             S              7  0 DIM(5)
010700090916AREA D S1M             S              7  0 DIM(5)
010800090916AREA D F1F             S              7  0 DIM(5)
010900000000      * COLLI PER C/SERVIZIO (PER FILIALE) -------------------------------------
011000090916FIL  D C4A             S              7  0 DIM(5)
011100090916FIL  D C4P             S              7  0 DIM(5)
011200090916FIL  D C4R             S              7  0 DIM(5)
011300000000      * PESI PER C/SERVIZIO (PER FILIALE)
011400090916FIL  D P4A             S              9  1 DIM(5)
011500090916FIL  D P4I             S              9  1 DIM(5)
011600090916FIL  D P4N             S              9  1 DIM(5)
011700090916FIL  D P4P             S              9  1 DIM(5)
011800090916FIL  D P4R             S              9  1 DIM(5)
011900000000      * VOLUMI PER C/SERVIZIO (PER FILIALE)
012000090916FIL  D V4A             S              9  4 DIM(5)
012100090916FIL  D V4P             S              9  4 DIM(5)
012200090916FIL  D V4R             S              9  4 DIM(5)
012300000000      * SPEDIZIONI PER C/SERVIZIO (PER FILIALE)
012400090916FIL  D S4A             S              7  0 DIM(5)
012500090916FIL  D S4P             S              7  0 DIM(5)
012600090916FIL  D S4R             S              7  0 DIM(5)
012700090916FIL  D S4M             S              7  0 DIM(5)
012800090916FIL  D F4F             S              7  0 DIM(5)
012900940502      *
013000940502      * COLLI PER TRANSITI (TOTALE GENERALE) -----------------------------------
013100090916GEN  D C2P             S              7  0 DIM(5)
013200000000      * PESI PER TRANSITI (TOTALE GENERALE)
013300090916GEN  D P2I             S              9  1 DIM(5)
013400090916GEN  D P2P             S              9  1 DIM(5)
013500000000      * VOLUMI PER TRANSITI (TOTALE GENERALE)
013600090916GEN  D V2P             S              9  4 DIM(5)
013700000000      * SPEDIZIONI PER TRANSITI (TOTALE GENERALE)
013800090916GEN  D S2P             S              7  0 DIM(5)
013900090916GEN  D S2M             S              7  0 DIM(5)
014000940429      * COLLI PER TOTALE AREA --------------------------------------------------
014100090916     D C3A             S              7  0 DIM(5)
014200090916     D C3P             S              7  0 DIM(5)
014300940428      * PESI PER TOTALE GENERALE
014400090916     D P3A             S              9  1 DIM(5)
014500090916     D P3I             S              9  1 DIM(5)
014600090916     D P3P             S              9  1 DIM(5)
014700940428      * VOLUMI PER TOTALE GENERALE
014800090916     D V3A             S              9  4 DIM(5)
014900090916     D V3P             S              9  4 DIM(5)
015000090916     D V3R             S              9  4 DIM(5)
015100940428      * SPEDIZIONI PER TOTALE GENERALE
015200090916     D S3A             S              7  0 DIM(5)
015300090916     D S3P             S              7  0 DIM(5)
015400090916     D S3R             S              7  0 DIM(5)
015500090916     D S3M             S              7  0 DIM(5)
015600090916     D F3F             S              7  0 DIM(5)
015700090916      * COLLI PER FILE fisat00F ------------------------------------------------
015800090916      * PESI PER FILE fisat00F
015900090916      * VOLUMI PER FILE fisat00F
016000090916      * SPEDIZIONI PER FILE fisat00F
016100000000      *
016200940504     D GME             S              1    DIM(31)
016300090108     D GMEP            S              1    DIM(31)
016400020418
016500020418     d sav_t06         S              3  0 dim(30)
016600020418
016700020418     d wc              s              3  0
016800021016
016900021016     d kTbeCod         s                   Like(TbeCod)
017000021016     d kTbeKe1         s                   Like(TbeKe1)
017100020418
017200040921     D WLBDAT          DS                  INZ
017300040921     D  G02DAT                 1      8  0
017400040921     D  G02INV                 9     16  0
017500040921     D  G02ERR                17     17
017600040921     D  G02TGI                18     22  0
017700000000      *
017800000810     D DS3A          E DS
017900000000     D DS3L          E DS
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
019300021016      * Ds date statistiche
019400021016     d dSdf          e ds
019500090605
019600090605     d ds5e          e ds
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
021500090916      * DS per i campi del file statistiche fisat00F
021600000000     D                 DS
021700090916     D  C0P                    1     35  0
021800090916     D                                     DIM(5)
021900090916     D  SATCDE                 1      7  0
022000090916     D  SATCDD                 8     14  0
022100090916     D  SATCDC                15     21  0
022200090916     D  SATCDh                22     28  0
022300090916     D  SATCDx                29     35  0
022400000000     D                 DS
022500090916     D  C0R                    1     35  0
022600090916     D                                     DIM(5)
022700970207     D  SATCNE                 1      7  0
022800970207     D  SATCND                 8     14  0
022900970207     D  SATCNC                15     21  0
023000090916     D  SATCnh                22     28  0
023100090916     D  SATCnx                29     35  0
023200000000     D                 DS
023300090916     D  P0I                    1     45  1
023400090916     D                                     DIM(5)
023500970207     D  SATFDE                 1      9  1
023600970207     D  SATFDD                10     18  1
023700970207     D  SATFDC                19     27  1
023800090916     D  SATfdh                28     36  1
023900090916     D  SATfdx                37     45  1
024000970207     D                 DS
024100090916     D  P0N                    1     45  1
024200090916     D                                     DIM(5)
024300970207     D  SATFNE                 1      9  1
024400970207     D  SATFND                10     18  1
024500970207     D  SATFNC                19     27  1
024600090916     D  SATfnh                28     36  1
024700090916     D  SATfnx                37     45  1
024800000000     D                 DS
024900090916     D  P0P                    1     45  1
025000090916     D                                     DIM(5)
025100970207     D  SATODE                 1      9  1
025200970207     D  SATODD                10     18  1
025300970207     D  SATODC                19     27  1
025400090916     D  SATOdh                28     36  1
025500090916     D  SATOdx                37     45  1
025600000000     D                 DS
025700090916     D  P0R                    1     45  1
025800090916     D                                     DIM(5)
025900970207     D  SATONE                 1      9  1
026000970207     D  SATOND                10     18  1
026100970207     D  SATONC                19     27  1
026200090916     D  SATonh                28     36  1
026300090916     D  SATonx                37     45  1
026400000000     D                 DS
026500090916     D  V0P                    1     45  4
026600090916     D                                     DIM(5)
026700970207     D  SATVDE                 1      9  4
026800970207     D  SATVDD                10     18  4
026900970207     D  SATVDC                19     27  4
027000090916     D  SATvdh                28     36  4
027100090916     D  SATvdx                37     45  4
027200000000     D                 DS
027300090916     D  V0R                    1     45  4
027400090916     D                                     DIM(5)
027500970207     D  SATVNE                 1      9  4
027600970207     D  SATVND                10     18  4
027700970207     D  SATVNC                19     27  4
027800090916     D  SATvnh                28     36  4
027900090916     D  SATvnx                37     45  4
028000000000     D                 DS
028100090916     D  S0P                    1     35  0
028200090916     D                                     DIM(5)
028300970207     D  SATSDE                 1      7  0
028400970207     D  SATSDD                 8     14  0
028500970207     D  SATSDC                15     21  0
028600090916     D  SATsdh                22     28  0
028700090916     D  SATsdx                29     35  0
028800000000     D                 DS
028900090916     D  S0R                    1     35  0
029000090916     D                                     DIM(5)
029100970207     D  SATSNE                 1      7  0
029200970207     D  SATSND                 8     14  0
029300970207     D  SATSNC                15     21  0
029400090916     D  SATsnh                22     28  0
029500090916     D  SATsnx                29     35  0
029600970305     D                 DS
029700090916     D  F0F                    1     35  0
029800090916     D                                     DIM(5)
029900970305     D  SATSFE                 1      7  0
030000970305     D  SATSFD                 8     14  0
030100970305     D  SATSFC                15     21  0
030200090916     D  SATsfh                22     28  0
030300090916     D  SATsfx                29     35  0
030400000000     D                 DS
030500090916     D  S0M                    1     35  0
030600090916     D                                     DIM(5)
030700000000     D  SATSCE                 1      7  0
030800970207     D  SATSCD                 8     14  0
030900970207     D  SATSCC                15     21  0
031000090916     D  SATsch                22     28  0
031100090916     D  SATscx                29     35  0
031200970210     D                 DS
031300090916     D  CDT                    1     35  0
031400090916     D                                     DIM(5)
031500970210     D  SATCPE                 1      7  0
031600970210     D  SATCPD                 8     14  0
031700970211     D  SATCPC                15     21  0
031800090916     D  SATcph                22     28  0
031900090916     D  SATcpx                29     35  0
032000970210     D                 DS
032100090916     D  PDT                    1     45  1
032200090916     D                                     DIM(5)
032300970210     D  SATPPE                 1      9  1
032400970210     D  SATPPD                10     18  1
032500970210     D  SATPPC                19     27  1
032600090916     D  SATpph                28     36  1
032700090916     D  SATppx                37     45  1
032800970210     D                 DS
032900090916     D  VDT                    1     45  4
033000090916     D                                     DIM(5)
033100970210     D  SATVPE                 1      9  4
033200970210     D  SATVPD                10     18  4
033300970210     D  SATVPC                19     27  4
033400090916     D  SATvph                28     36  4
033500090916     D  SATvpx                37     45  4
033600970210     D                 DS
033700090916     D  SDT                    1     35  0
033800090916     D                                     DIM(5)
033900970210     D  SATSPE                 1      7  0
034000970210     D  SATSPD                 8     14  0
034100970210     D  SATSPC                15     21  0
034200090916     D  SATsph                22     28  0
034300090916     D  SATspx                29     35  0
034400020813      *
034500090916      * Variabili
034600020813      *
034700020813     D SAVRIS          S                   LIKE(DS1RIS) inz
034800020813     D $CDA            S              7  0              inz
034900020813     D $VDA            S              9  4              inz
035000020813     D $PDA            S              9  1              inz
035100020813     D $SDA            S              7  0              inz
035200020813     D $TFA            S                   LIKE(BLPTFA) inz
035300020813     D $AREA           S                   LIKE(DS1FEL) inz
035400020813     D $LNP            S                   LIKE(DS1LNP) inz
035500020813     D $BLTPR          S                   LIKE($BLTAT) inz
035600020813     D $FILPR          S                   LIKE(BLPLNP) inz
035700020813     D $CBLPR          S                   LIKE(BLPCBO) inz
035800020813     D $NFVPR          S                   LIKE(BLPNFV) inz
035900020813     D $DTRIT          S                   LIKE(BLPDRT) inz
036000020813     D $CDPDR          S                   LIKE(BLPPDR) inz
036100020813     D $FLAG           S                   LIKE(WRKFLG) inz
036200020813     D $FLAG2          S                   LIKE(WRKFLG) inz
036300020813     D $DFV            S                   LIKE(FGVDFV) inz
036400020813     D $FTR            S                   LIKE(SATFTR) inz
036500020813     D $DTR            S                   LIKE(SATDTR) inz
036600020813     D $TRE            S                   LIKE(SATTRE) inz
036700020813     D $COMOD          S             15  6 INZ
036800020813     D $COMO1          S             15  6 INZ
036900020813     D*
037000020813     D KCOD            S                   LIKE(TBLCOD) inz
037100020813     D KKEY            S                   LIKE(TBLKEY) inz
037200021217      *
037300021217     d $Volum          s             12  4 inz
037400021217     d $Peso           s              8  1 inz
037500030213     d*** WsiVOL          s              1    inz
037600030213     d*** WsiPES          s              1    inz
037700021217     d Wvolu           s                   like(BLPvlf) inz
037800021217     d Wpeso           s                   like(BLPpkf) inz
037900021220     d Wvolmed         s                   like($Volum) inz
038000021220     d Wpesmed         s                   like($Peso)  inz
038100090415     D ktrc            S                   LIKE(statrc) inz('T')
038200090415     D kfil            S                   LIKE(stafil) inz(000)
038300940502      **************************************************************************
038400940502      *      RIEPILOGO INDICATORI
038500940502      **************************************************************************
038600940502      * 01    - ON: LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE = 0
038700940502      *       - OFF:LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE > D.ELAB
038800940502      * 02    - ON: TOTALE GENERALE
038900940502      *       - OFF:TOTALE DI AREA
039000970226      * 03    - STAMPA DEI TOTALI TRANSITI
039100970226      * 04    - NON MEMORIZZA $ALMEN NELLA ROURINE DI TOTGEN
039200940502      * 05    - STAMPE GIORNALIERE
039300970205      * 27    - OFF: DEVO STAMPARE LA RIGA DEI TRANASITI
039400940502      * 29    - ON: IMMESSA UNA LINEA DI PARTENZA
039500940502      * 30    - ECCEDENZA PAGINA
039600940502      * 33    - LETTURA FILES
039700940502      * 34    - BOLLA IN C/SERVIZIO
039800940502      * 35    - ON: BOLLA BOLLETTATA NEL PERIODO
039900940502      *       - OFF:BOLLA BOLLETTATA NEL PERIODO PRECEDENTE
040000940502      * 36    - COLLO A TERRA
040100090916      * 37    - LETTURA SUL FILES fisat
040200950210      * 38    - DS1RIS= ' ' --> NON DEVO STAMPARE
040300940502      * 39    - ON: IMMESSO CODICE AREA
040400940502      * 40    - ON: STAMPA LA LILIALE CON DECODIFICA
040500940502      *       - OFF:STAMPA LA DICITURA DEL SERVIZIO BOLLE
040600940502      * 41    - ON: STAMPA SOLTANTO LA FILIALE (+ 40 ON)
040700940502      * 43    - ON: STAMPO LA PARTE RELATIVA ALLE LINEE DI PARTENZA
040800940502      *       - OFF:STAMPO LA PARTE RELATIVA AI TERMINAL DI ARRIVO
040900970211      * 45    - STAMPA LE PERCENTUALI SUL DA PARTIRE
041000950118      * 99    - DI COMODO
041100940502      **************************************************************************
041200000000     C     *ENTRY        PLIST
041300000000     C                   PARM                    KPJBA
041400950210     C                   MOVEL     KPJBU         DSLS01
041500950210      *
041600950210      * DATA FINALE AA/MM/GG (6 CARATTERI)
041700090108     C                   Z-ADD     DS1aal        DATADA
041800000000      *
041900090415     c     KSTA          Klist
042000090415     c                   Kfld                    Ktrc
042100090415     c                   Kfld                    DS1FEL
042200090415     c                   Kfld                    Kfil
042300090415     c                   Kfld                    ds1ada
042400950118     C     FNBLTK        KLIST
042500940428     C                   KFLD                    LIN($X)
042600940428     C                   KFLD                    $DFV
042700970205     C     FNBLPK        KLIST
042800950118     C                   KFLD                    BLTAAS
042900000000     C                   KFLD                    BLTLNP
043000000000     C                   KFLD                    BLTNRS
043100000000     C                   KFLD                    BLTNSP
043200950118     C     FNBTPK        KLIST
043300020625     c                   kfld                    bttflp
043400950118     C                   KFLD                    BTTAAS
043500950118     C                   KFLD                    BTTLNP
043600950118     C                   KFLD                    BTTNRS
043700950118     C                   KFLD                    BTTNSP
043800950118     C     AZCLNK        KLIST
043900940504     C                   KFLD                    SIMFEL
044000940504     C                   KFLD                    $TFA
044100940504     C                   KFLD                    ANNODA
044200940504     C                   KFLD                    MESEDA
044300950118     C     FNWRKK        KLIST
044400000000     C                   KFLD                    $FLAG
044500000000     C                   KFLD                    $FILPR
044600950118     C     FNWR1K        KLIST
044700000000     C                   KFLD                    $FLAG
044800000000     C                   KFLD                    WRKLNP
044900940503     C                   KFLD                    BLPAAS
045000940503     C                   KFLD                    BLPNRS
045100940503     C                   KFLD                    BLPNSP
045200000000     C                   KFLD                    WRKCSE
045300970409     C     FNWR2K        KLIST
045400970409     C                   KFLD                    $FLAG
045500970409     C                   KFLD                    WRKLNP
045600970409     C                   KFLD                    BTPAAS
045700970409     C                   KFLD                    BTPNRS
045800970409     C                   KFLD                    BTPNSP
045900970409     C                   KFLD                    WRKCSE
046000090916     C     fisatK        KLIST
046100970205     C                   KFLD                    SIMFEL
046200970205     C                   KFLD                    LIN(XX)
046300970205     C                   KFLD                    DS1AAL
046400970205     C     FNSA1K        KLIST
046500000000     C                   KFLD                    SATFLE
046600000000     C                   KFLD                    SATTFA
046700940429     C                   KFLD                    SATLNP
046800000000     C                   KFLD                    SATDRE
046900000000     C                   KFLD                    SATTRE
047000970205     C     DTSATK        KLIST
047100940429     C                   KFLD                    $AREA
047200940429     C                   KFLD                    $TFA
047300940429     C                   KFLD                    $LNP
047400970211     C                   KFLD                    DS1ADA
047500970205     C     LPSATK        KLIST
047600940429     C                   KFLD                    $AREA
047700940429     C                   KFLD                    $TFA
047800940429     C                   KFLD                    $LNP
047900970205     C     ARSATK        KLIST
048000940429     C                   KFLD                    $AREA
048100940429     C                   KFLD                    $TFA
048200970211     C     TASATK        KLIST
048300940429     C                   KFLD                    $AREA
048400940429     C                   KFLD                    $TFA
048500970211     C                   KFLD                    DS1ADA
048600970211     C     TRSATK        KLIST
048700940503     C                   KFLD                    $AREA
048800940503     C                   KFLD                    $TFA
048900970211     C                   KFLD                    DS1ADA
049000940503     C                   KFLD                    $TRE
049100020625     c     kbtt39        klist
049200020625     c                   kfld                    simfel
049300020625     c                   kfld                    $dfv
049400021016
049500021016     c     Ktbe          Klist
049600021016     c                   Kfld                    kTbeCod
049700021016     c                   Kfld                    kTbeKe1
049800000000      *
049900021016     C                   Z-ADD     *ZERO         $COMO1           15 6
050000021016     C                   Z-ADD     1             CODUT
050100000000      **************************************************************************
050200000000      *              M A I N      L I N E
050300000000      **************************************************************************
050400940429     C                   MOVE      'RISTAMPA'    STRIP
050500940429     C*
050600950210    1C     DS1RIS        IFNE      'R'
050700940429     C                   MOVE      'STAMPA  '    STRIP
050800940502    1C                   ENDIF
050900070522     C*
051000090605      * carico le descrizioni fisse dei tipi servizio
051100090605      * chiodo l'indice di schiera per ogni tipo servizio
051200090605      * 1=E - 2=D - 3=C
051300090605     c                   clear                   ds5e
051400090605     c                   clear                   ens
051500090605     c                   eval      kcod = '5E'
051600090605     c     ktab2         setll     tabel00f
051700090605     c                   do        *hival
051800090605     c     ktab2         reade(n)  tabel00f
051900090605     c                   if        %eof(tabel00f)
052000090605     c                   leave
052100090605     c                   endif
052200090605     c                   eval      ds5e = tbluni
052300090605     c                   if        tblkey = 'C'
052400090605     c                   eval      ens(03) = §5ed05
052500090605     c                   endif
052600090605     c                   if        tblkey = 'D'
052700090605     c                   eval      ens(02) = §5ed05
052800090605     c                   endif
052900090605     c                   if        tblkey = 'E'
053000090605     c                   eval      ens(01) = §5ed05
053100090605     c                   endif
053200090916     c                   if        tblkey = 'H'
053300090916     c                   eval      ens(04) = §5ed05
053400090916     c                   endif
053500090605     c                   enddo
053600090605
053700000000      *
053800000000      * Se la data AL e' vuota la imposto come la data DAL
053900950210     C     DS1AAL        IFEQ      *ZERO
054000950210     C                   Z-ADD     DS1ADA        DS1AAL
054100950210     C                   Z-ADD     DS1GDA        DS1GAL
054200000000     C                   ENDIF
054300940428     C*
054400940428     C                   SETON                                        43
054500940401      *
054600950210      * Accendo un indicatore se non devo stampare il tabulato (DS1RIS=' ')
054700950210     C     DS1RIS        COMP      *BLANKS                                38
054800940429      *
054900940429      * 39 ON - Immessa l'area
055000950210    3C     DS1FEL        IFGT      0
055100940429     C                   SETON                                        39
055200940429    3C                   ENDIF
055300000000      *
055400950210      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
055500950210     C                   EXSR      COMUNE
055600950210      *
055700000000     C                   SETON                                        LR
055800020212     C*****************************************************************
055900020212     C*  PARTE COMUNE STAMPE GIORNALIERE
056000020212     C*
056100020212     C*  . REPERIMENTO 'SIMFEL'
056200020212     C*  . CARICAMENTO '£1'
056300020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
056400020212     C*  . CODA DI STAMPA ('£OUTQ')
056500020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
056600020212     C*  . MODULO PER LA STAMPA ('£MOD')
056700020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
056800020212     C*****************************************************************
056900020212     C     COMUNE        BEGSR
057000020212     C*
057100020212     C*****************************************************************
057200020212     C*       RIEPILOGO INDICATORI:
057300020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
057400020212     C* 11    -           STAMPA PER 1L
057500020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
057600020212     C*****************************************************************
057700020212     C                   Z-ADD     1             CODUT
057800020212     C*
057900020212     C* Reperimento parametri aziendali
058000020212     C                   CALL      'X§PARUT'
058100020212     C                   PARM                    UT§DSE
058200020212     C                   MOVEL     RAGUT         RSUT             20
058300020212     C                   MOVEL     REC80         CNCR80
058400020212     C*
058500020212     C* Chiave di accesso per il file tabelle
058600020212     C     KTAB1         KLIST
058700020212     C                   KFLD                    CODUT
058800020212     C                   KFLD                    KCOD
058900020212     C                   KFLD                    KKEY
059000020212     C     KTAB2         KLIST
059100020212     C                   KFLD                    CODUT
059200020212     C                   KFLD                    KCOD
059300020212     C*
059400020813     ***C     *LIKE         DEFINE    TBLCOD        KCOD
059500020813     ***C     *LIKE         DEFINE    TBLKEY        KKEY
059600020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
059700020212     C*
059800020212     C* DETERMINO DATA E ORA DI ESECUZIONE
059900020212     C                   TIME                    WTIME            14 0
060000020212     C                   MOVE      WTIME         WDATE             8 0
060100020212     C                   MOVEL     WTIME         UTIME             6 0
060200040921     c*
060300040921     C                   CLEAR                   WLBDAT
060400040921     C                   MOVE      Wtime         G02DAT
060500040921     C                   CALL      'XSRDA8'
060600040921     C                   PARM                    WLBDAT
060700040921     C                   Z-ADD     G02INV        dateu             8 0
060800090916     c* Accendo indicatore 10 de devo stampare anche tipo servizio H
060900090916     c                   seton                                        10
061000090916
061100090916     c* Se c'e' già il tipo servizio  H ciclo 4 volte,
061200090916     c*  altrimenti 3
061300090916     c  n10              z-add     3             ciclo             2 0
061400090916     c   10              z-add     4             ciclo             2 0
061500040922     c*
061600040922     c                   clear                   $FTR
061700040922     c                   z-add     dateu         $DTR
061800020212     C*
061900020418     C* CARICO '£1'
062000020212     C                   CLEAR                   KPJBU
062100020212     C                   CLEAR                   DSUL06
062200020212     C                   MOVE      '£1'          D06COD
062300020212     C                   MOVEL     SIMFEL        D06KEY
062400050802     C                   MOVE      'S'           D06KEY
062500050802     C                   MOVEL     ds1ada        D06drf
062600020212     C                   MOVEL     DSUL06        KPJBU
062700020212     C                   CALL      'TRUL06R'
062800020212     C                   PARM                    KPJBA
062900020212     C                   MOVEL     KPJBU         DSUL06
063000020418     c                   movea     t06           sav_t06
063100020418
063200020418      * se in arrivo carico la £6
063300020418 b1  c                   if        ds1tbo = 'A'
063400020418     c                   clear                   wc
063500020418 b2  c                   do        30            wa
063600020422     c                   if        sav_t06(wa) = *zeros
063700020422     c                   leave
063800020422     c                   endif
063900020418     c                   clear                   kpjbu
064000020418     c                   clear                   dsul06
064100020418     c                   eval      d06cod = '£6'
064200020418     c                   movel     sav_t06(wa)   d06key
064300050802     C                   MOVE      'S'           D06KEY
064400020418     c                   eval      d06esc = 'S'
064500050802     c                   movel     ds1ada        d06drf
064600020419     c                   movel     dsul06        kpjbu
064700020418     c                   call      'TRUL06R'
064800020418     c                   parm                    kpjba
064900020418     c                   movel     kpjbu         dsul06
065000020418 b3  c                   do        30            wb
065100020418     c                   if        t06(wb) = *zeros
065200020418     c                   leave
065300020418     c                   endif
065400020418     c                   add       1             wc
065500020418     c                   z-add     t06(wb)       l6(wc)
065600020418 e3  c                   enddo
065700020418 e2  c                   enddo
065800020212     C*
065900020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
066000020212     C                   Z-ADD     *ZERO         WB
066100020418 b2  C                   DO        29            WA
066200020418     C     sav_T06(WA)   LOOKUP    L6                                     30
066300020212     C  N30              ADD       1             WB
066400020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
066500020418 e2  C                   ENDDO
066600020418 x1  C                   ELSE
066700020418     C                   MOVEA     sav_T06       L1
066800020418 e1  C                   ENDIF
066900020212     C*
067000020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
067100020212     C                   MOVEA     L1            LIN
067200020212     C                   CLEAR                   L1
067300020212     C                   Z-ADD     *ZERO         WB                3 0
067400020212     C     1             DO        30            WPT               3 0
067500020212     C     LIN(WPT)      IFNE      0
067600020212     C                   CLEAR                   FNLSC4
067700020212     C                   Z-ADD     LIN(WPT)      IC4LIN
067800020212     C                   MOVE      'B'           IC4FLG
067900020212     C                   MOVEL     FNLSC4        KPJBU
068000020212     C                   CALL      'FNLSC4R1'
068100020212     C                   PARM                    KPJBA
068200050802     C                   PARM                    ds1ada
068300020212     C                   MOVEL     KPJBU         FNLSC4
068400020212     C     OC4ESE        IFEQ      'S'
068500020212     C                   ADD       1             WB
068600020212     C                   MOVE      LIN(WPT)      L1(WB)
068700020212     C                   ENDIF
068800020212     C                   ENDIF
068900020212     C                   ENDDO
069000070103     C*
069100070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
069200070103     C                   Z-ADD     *ZERO         WB                3 0
069300070103     C                   DO        29            WA                3 0
069400070103     C     L1(WA)        IFNE      *ZEROS
069500070103     C     L1(WA)        ANDNE     SIMFEL
069600070103     C                   ADD       1             WB
069700070103     C                   Z-ADD     L1(WA)        LV2(WB)
069800070103     C                   ENDIF
069900070103     C                   ENDDO
070000020212     C*
070100020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
070200020212     C*
070300020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
070400020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
070500020212     C* la stampa e' lanciata da menu
070600020212     C                   MOVEL     '3L'          KCOD
070700020212     C                   MOVEL     DS1PGM        KKEY
070800020212     C                   MOVE      DS1FPG        KKEY
070900020212     C     KTAB1         CHAIN     TABEL00F                           30
071000020212     C  N30              MOVEL     TBLUNI        DS3L
071100020212     C   30              MOVEL     'F'           §3LD1L
071200020212     C   30              MOVEL     ' '           §3LD2L
071300020212     C   30              MOVEL     '*STD    '    §3LDST
071400020212     C*
071500020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
071600020212     C                   MOVEL     §3LDST        £MOD             10
071700020212     C                   CLEAR                   LIN
071800020212     C*
071900020212     C* Verifico chi ha lanciato la stampa
072000020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
072100020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
072200020212     C* DS1FGS<>0  --->  si proviene da menu
072300020212     C     DS1FGS        IFEQ      0
072400020212     C*
072500020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
072600020212     C* lancio, voglio la stampa;  invece
072700020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
072800020212     C*
072900020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
073000020212     C* creare stampa per 1°lvl. (se stessa)
073100020212     C     DS1LNP        IFEQ      SIMFEL
073200020212     C*
073300020212     C* Il flag §3LD1L ha i seguenti significati:
073400020212     C*    ' ' = tutti i dati
073500020212     C*    '1' = solo filiale di 1' livello
073600020212     C     §3LD1L        IFEQ      '1'
073700020212     C                   SETON                                        11
073800020212     C                   END
073900020212     C*
074000020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
074100020212     C* nella stampa
074200020212     C*
074300020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
074400020212     C  N11              MOVEA     L1            LIN
074500020212     C   11              MOVEA     SIMFEL        LIN(1)
074600020212     C*
074700020212     C* IMPOSTO LA STAMPANTE:
074800020212     C* STAMPANTE TABULATI
074900020212     C                   MOVEL     '5P'          KCOD
075000020212     C                   MOVEL     'T'           W004A             4
075100020212     C                   MOVE      LIN(1)        W004A
075200020212     C                   MOVEL(P)  W004A         KKEY
075300020212     C     KTAB1         CHAIN     TABEL00F                           99
075400020212     C     *IN99         IFEQ      *OFF
075500020212     C                   MOVEL     TBLUNI        DS5P
075600020212     C                   MOVEL     §5POTQ        £OUTQ            10
075700020212     C                   END
075800020212     C*
075900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
076000020212     C                   EXSR      LEGGI
076100020212     C*
076200020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
076300020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
076400020212     C                   ELSE
076500020212     C*
076600020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
076700020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
076800020212     C*
076900020212     C* Il significato del fla §3LD2L e' il seguente:
077000020212     C*    ' ' = tutti i dati
077100020212     C*    'F' = per ogni 2' livello
077200020212     C     §3LD2L        IFEQ      'F'
077300020212     C                   SETON                                        14
077400020212     C                   ENDIF
077500020212     C*
077600020212     C                   Z-ADD     1             WB
077700020212     C                   MOVEA     *ZEROS        LIN
077800020212     C  N14              MOVEA     L1            LIN
077900020212     C*
078000020212     C* Lancio per tutti i 2° lvl
078100020212     C     DS1LNP        IFEQ      0
078200020212     C*
078300020212     C* LV2 contiene tutte le filiali di 2° lvl
078400020212     C     LV2(WB)       DOWNE     0
078500020212      *
078600020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
078700020212     C                   CLEAR                   FNLSC4
078800020212     C                   Z-ADD     LV2(WB)       IC4LIN
078900020212     C                   MOVEL     FNLSC4        KPJBU
079000020212     C                   CALL      'FNLSC4R1'
079100020212     C                   PARM                    KPJBA
079200050802     C                   PARM                    ds1ada
079300020212     C                   MOVEL     KPJBU         FNLSC4
079400020212      *
079500020212     C     OC4PT1        IFEQ      'S'
079600020212     C     §3LPT         ANDEQ     'N'
079700020212     C                   GOTO      NOLEGG
079800020212     C                   ENDIF
079900020212      *
080000020212     C     OC4EXP        IFEQ      'S'
080100020212     C     §3LEXP        ANDEQ     'N'
080200020212     C                   GOTO      NOLEGG
080300020212     C                   ENDIF
080400020212      *
080500020212     C     OC4DPD        IFEQ      'S'
080600020212     C     §3LDPD        ANDEQ     'N'
080700020212     C                   GOTO      NOLEGG
080800020212     C                   ENDIF
080900020212      *
081000020212     C     OC4BAR        IFEQ      'S'
081100020212     C     §3LBAR        ANDEQ     'N'
081200020212     C                   GOTO      NOLEGG
081300020212     C                   ENDIF
081400020212      *
081500020212     C     OC4FED        IFEQ      'S'
081600020212     C     §3LFED        ANDEQ     'N'
081700020212     C                   GOTO      NOLEGG
081800020212     C                   ENDIF
081900020212      *
082000020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
082100020212     C*  dati degli altri 2° lvl
082200020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
082300020212     C   14              MOVEL     LV2(WB)       LIN(1)
082400020212     C*
082500020212     C* IMPOSTO LA STAMPANTE
082600020212     C* STAMPANTE TABULATI
082700020212     C                   MOVEL     '5P'          KCOD
082800020212     C                   MOVEL     'T'           W004A             4
082900020212     C                   MOVE      LV2(WB)       W004A
083000020212     C                   MOVEL(P)  W004A         KKEY
083100020212     C     KTAB1         CHAIN     TABEL00F                           99
083200020212     C     *IN99         IFEQ      *OFF
083300020212     C                   MOVEL     TBLUNI        DS5P
083400020212     C                   MOVEL     §5POTQ        £OUTQ            10
083500020212     C                   END
083600020212     C*
083700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
083800020212     C                   EXSR      LEGGI
083900020212     C*
084000020212     C     NOLEGG        TAG
084100020212     C*
084200020212     C                   ADD       1             WB
084300020212     C                   ENDDO
084400020212     C*
084500020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
084600020212     C                   ELSE
084700020212     C*
084800020212     C* Lancio solo per la 2° lvl richiesta
084900020212     C   14              MOVEL     DS1LNP        LIN(1)
085000020212     C*
085100020212     C* STAMPANTE
085200020212     C* STAMPANTE TABULATI
085300020212     C                   MOVEL     '5P'          KCOD
085400020212     C                   MOVEL     'T'           W004A             4
085500020212     C                   MOVE      DS1LNP        W004A
085600020212     C                   MOVEL(P)  W004A         KKEY
085700020212     C     KTAB1         CHAIN     TABEL00F                           99
085800020212     C     *IN99         IFEQ      *OFF
085900020212     C                   MOVEL     TBLUNI        DS5P
086000020212     C                   MOVEL     §5POTQ        £OUTQ            10
086100020212     C                   END
086200020212     C*
086300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
086400020212     C                   EXSR      LEGGI
086500020212     C*
086600020212     C                   ENDIF
086700020212     C                   ENDIF
086800020212     C*
086900020212     C                   ELSE
087000020212     C*
087100020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
087200020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
087300020212     C     DS1LNP        IFEQ      0
087400020212     C                   MOVEA     L1            LIN
087500020212     C* ...altrimenti elaboro solo se stessa
087600020212     C                   ELSE
087700020212     C                   MOVEL     DS1LNP        LIN(1)
087800020212     C                   ENDIF
087900020212     C*
088000020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
088100020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
088200020212     C* PRENDO QUELLA PRESENTE IN KPJBA
088300020212     C     DS1FGS        IFEQ      SIMFEL
088400020212     C                   MOVEL     KQOTD         £OUTQ
088500030627     C**!!!              MOVEL     KQOTD         £OUTQF
088600020212     C                   ELSE
088700020212     C* STAMPANTE TABULATI
088800020212     C                   MOVEL     '5P'          KCOD
088900020212     C                   MOVEL     'T'           W004A             4
089000020212     C                   MOVE      DS1FGS        W004A
089100020212     C                   MOVEL(P)  W004A         KKEY
089200020212     C     KTAB1         CHAIN     TABEL00F                           99
089300020212     C     *IN99         IFEQ      *OFF
089400020212     C                   MOVEL     TBLUNI        DS5P
089500170302     C****               MOVEL     §5POTQ        £OUTQ            10
089600020212     C                   END
089700020212     C                   ENDIF
089800020212     C*
089900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
090000020212     C                   EXSR      LEGGI
090100020212     C                   ENDIF
090200020212     C*
090300020212     C                   ENDSR
090400000000      **************************************************************************
090500000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
090600000000      **************************************************************************
090700000000     C     LEGGI         BEGSR
090800940502     C                   SETOFF                                       2729
090900000619     C                   SETON                                        43
091000000616     C                   CLEAR                   SAVRIS
091100970212     C*
091200970212      * Cancello dal file statistiche i record della giornata che vado a elabor.
091300970212      * Solo se richiesta elaborazione
091400970212    1C     DS1RIS        IFNE      'R'
091500000616     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
091600000616     C                   CLEAR                   KPJBU
091700000616     C                   CLEAR                   DSUL06
091800000616     C                   MOVE      '£1'          D06COD
091900000616     C                   MOVEL     SIMFEL        D06KEY
092000050802     C                   MOVE      'S'           D06KEY
092100050802     c                   movel     ds1ada        d06drf
092200000616     C                   MOVEL     DSUL06        KPJBU
092300000616     C                   CALL      'TRUL06R'
092400000616     C                   PARM                    KPJBA
092500000616     C                   MOVEL     KPJBU         DSUL06
092600000616     C**
092700000616     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
092800000616     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
092900000616     C*  CHE USO ALLA FINE
093000090916    2C     DS1RIS        IFNE      ' '
093100000616     C                   MOVEA     LIN           SVLIN
093200000616     C                   MOVEL     DS1RIS        SAVRIS
093300000616     C                   CLEAR                   DS1RIS
093400090916    2C                   ENDIF
093500000616     C*
093600000616     C                   MOVEA     T06           LIN
093700000616     C*
093800090916     C                   OPEN      fisat04L
093900970212     C                   Z-ADD     1             XX                3 0
094000970212    2C     LIN(XX)       DOWGT     *ZERO
094100970212     C*
094200090916     C     fisatK        SETLL     fisat04L
094300090916     C     fisatK        READE     fisat04L                               99
094400970212     C*
094500970212    3C     *IN99         DOWEQ     *OFF
094600090916     C                   DELETE    fisat004
094700090916     C     fisatK        READE     fisat04L                               99
094800970212    3C                   ENDDO
094900970212     C                   ADD       1             XX
095000970212    2C                   ENDDO
095100970212     C*
095200090916     C                   CLOSE     fisat04L
095300090916    1C                   END
095400940505      *
095500940505      * Come prima cosa devo verificare se giornata festiva
095600940505      *   solo per ELABORAZIONE o  per stampe giornaliere
095700090916    1C     DS1FGS        IFEQ      0
095800950210     C     DS1RIS        ORNE      'R'
095900940505     C                   CLEAR                   $TFA
096000940505      *
096100950118     C     AZCLNK        CHAIN     AZCLN01L                           99
096200090916    2C     *IN99         IFEQ      *OFF
096300950118     C                   MOVEA     CLNMAT        GME
096400090916    3C     GME($GI)      IFEQ      'F'
096500090916     C* SE FESTIVO SCRIVO RECORD IN fisat CON TRE = F
096600090916     C                   CLEAR                   fisat000
096700970211     C                   Z-ADD     SIMFEL        SATFLE
096800970211     C                   MOVEL     'F'           SATTRE
096900970211     C                   Z-ADD     DS1ADA        SATDRE
097000040922     c                   movel     $ftr          satftr
097100040922     c                   z-add     $DTR          satdtr
097200090916     C                   WRITE     fisat000                             99
097300970211     C                   GOTO      ENDLEG
097400090916    3C                   ENDIF
097500090916    2C                   ENDIF
097600090916    1C                   ENDIF
097700940429      *
097800940429     C* LANCIO DA FILTRO
097900950210    1C     DS1FGS        IFGT      0
098000940429     C* 29 ON - IMMESSA LINEA DI PARTENZA
098100950210    2C     DS1LNP        IFGT      0
098200940429     C                   SETON                                        29
098300950210    3C     DS1LNP        IFNE      DS1FEL
098400940502     C                   SETON                                        27
098500940429    3C                   ENDIF
098600940429    2C                   ENDIF
098700940429     C*
098800940429   X1C                   ELSE
098900940502     C                   SETON                                        05
099000940506     C                   CLEAR                   STRIP
099100940429     C* STAMPE GIORNALIERE
099200940429    2C     LIN(2)        IFEQ      0
099300940429     C                   SETON                                        29
099400940429    2C                   ENDIF
099500940429     C     SIMFEL        LOOKUP    LIN                                    99
099600940502     C  N99              SETON                                        27
099700940429    1C                   ENDIF
099800000000      *
099900000000      * Scrivo, in stampa, se selezionata filiale o area
100000000000     C                   MOVE      *BLANKS       STRIP1
100100090916    1C  N05DS1LNP        IFNE      *ZERO
100200960925     C                   MOVEL     'P.O.'        STRIP1
100300950210     C                   MOVE      DS1LNP        STRIP1
100400000000     C                   ELSE
100500940502     C                   MOVEL     'AREA'        STRIP1
100600950210     C                   MOVE      DS1FEL        STRIP1
100700090916    1C                   ENDIF
100800000000      *
100900000000      * Inizializzo i campi per l'elaborazione
101000000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
101100000000     C                   Z-ADD     *ZERO         $FILPR
101200000000     C                   MOVE      *BLANKS       $CBLPR
101300000000     C                   Z-ADD     *ZERO         $NFVPR
101400940509     C                   CLEAR                   $RIGHE
101500950210      *
101600950210      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
101700950210     C                   MOVEA     £MOD          CMD(33)
101701170302     c                   if        £outq=*blanks
101702170302     c                   eval      £outq='*JOB     '
101703170302     c                   endif
101800950210     C                   MOVEA     £OUTQ         CMD(51)
101900950210      *
102000940502      * Eseguo OVRPRTF sul file di stampa
102100950210     C                   Z-ADD     82            LUNG             15 5
102200950213     C                   MOVEL     *BLANKS       COMMAN           82
102300940502     C                   MOVEA     CMD           COMMAN
102400940502     C                   CALL      'QCMDEXC'                            H1
102500940502     C                   PARM                    COMMAN
102600940502     C                   PARM                    LUNG
102700970807     C  N38              OPEN      FNLSC2P
102800950118     C  N38              WRITE     LSC2T1
102900950118     C  N38              WRITE     LSC2T3
103000940502     C  N38              SETOFF                                       30
103100000619     C**
103200000619     C* SE DS1RIS= 'S' ACCENDO PER ORA IL 38
103300090916    1C     SAVRIS        IFNE      ' '
103400000619     C                   SETON                                        38
103500090916    1C                   ENDIF
103600000000      *
103700950210      * L'elaborazione solo se non scelta la ristampa (DS1RIS='R')
103800950210    1C     DS1RIS        IFNE      'R'
103900000000      *
104000970205      * Leggo il file delle bolle in partenza prima quelli con data
104100970205      * F.V. = 0 poi quelli con DFV >= della data elaborazione
104200940401     C                   Z-ADD     1             $X                3 0
104300940428     C                   MOVE      *BLANK        $FLAG
104400940428     C*
104500940428    2C     LIN($X)       DOWGT     *ZERO
104600000912     C                   Z-ADD     LIN($X)       $FILPR
104700940428     C                   CLEAR                   $DFV
104800940428     C                   SETOFF                                           01
104900940428      *
105000940428    3C                   DO        2
105100021023     C     FNBLTK        SETLL     FNBLT38L
105200021023     C  N01FNBLTK        READE     FNBLT38L                               33
105300021023     C   01LIN($X)       READE     FNBLT38L                               33
105400940428     C*
105500940428    4C     *IN33         DOWEQ     *OFF
105600000000      *
105700000000      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
105800000000      * come test ma mi permette di saltare delle chain)
105900940428    5C     $BLTAT        IFNE      $BLTPR
106000950118     C     FNBLPK        CHAIN     FNBLP01L                           99
106100000000     C   99              GOTO      LETTUR
106200000000     C                   MOVE      $BLTAT        $BLTPR
106300000000     C                   Z-ADD     BLPNFV        $NFVPR
106400950118     C                   Z-ADD     BLPMGS        $DATSP            8 0
106500000000     C                   MOVEL     BLPAAS        $DATSP
106600000331     C                   Z-ADD     BLPDRT        $DTRIT
106700000413     C                   Z-ADD     BLPPDR        $CDPDR
106800940428     C* $BOLLA = 1 INDICA CHE HO LETTO UNA NUOVA BOLLA
106900940428     C                   MOVEL     '1'           $BOLLA            1
107000000000      *
107100000000      * Decodifico il tipo bolla
107200950118    6C     BLPCBO        IFNE      $CBLPR
107300950210     C                   MOVE      '3A'          KCOD
107400950210     C                   MOVE      *BLANKS       KKEY
107500950210     C                   MOVEL     BLPCBO        KKEY
107600950210     C     KTAB1         CHAIN     TABEL00F                           99
107700000000     C  N99              MOVEL     TBLUNI        DS3A
107800950118     C  N99              MOVE      BLPCBO        $CBLPR
107900000000     C   99              CLEAR                   DS3A
108000940428    6C                   ENDIF
108100940427     C*
108200940428    5C                   ENDIF
108300000000      *
108400940428      * ESCLUSIONI BOLLE
108500000000     C                   EXSR      ESCLUS
108600940428    5C     $OK           IFEQ      'S'
108700000000     C                   EXSR      ELABOR
108800940428    5C                   ENDIF
108900000000      *
109000940428     C     LETTUR        TAG
109100021023     C  N01FNBLTK        READE     FNBLT38L                               33
109200021023     C   01LIN($X)       READE     FNBLT38L                               33
109300940428    4C                   ENDDO
109400940428      *
109500940428     C                   SETON                                        01
109600970205     C                   Z-ADD     DS1AAL        $DFV
109700940428    3C                   ENDDO
109800940428      *
109900940428      * cambia la filiale --> decodifico e stampo i totali
110000940428     C                   EXSR      CALSPE
110100940428     C                   EXSR      WRISAT
110200940429     C                   EXSR      WR1SAT
110300940428     C  N38              EXSR      TOTFIL
110400940428     C                   EXSR      AZZFIL
110500940428     C*
110600940401     C                   ADD       1             $X
110700090916    2C                   ENDDO
110800940429      *
110900940429     C*
111000940429     C* STAMPA DEI DATI FINALI
111100940506     C                   EXSR      STAFIN
111200090916    1C                   ENDIF
111300000616      *
111400000616     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
111500090916    1C     SAVRIS        IFEQ      'S'
111600000616     C                   MOVEL     'R'           DS1RIS
111700000616     C                   MOVEA     SVLIN         LIN
111800000616     C* AZZERO LE SCHIERE DI TOTALE
111900000616     C                   EXSR      AZZSER
112000000616     C                   EXSR      AZZTRA
112100000616     C                   EXSR      AZZTOT
112200000619     C                   SETOFF                                       38
112300090916    1C                   ENDIF
112400000000      *
112500090916      * Nel caso di RISTAMPA mi limito a rileggere il file statistiche (fisat)
112600000000      * e stampare i record che rientrano nell'intervallo dal-al
112700090916    1C     DS1RIS        IFNE      ' '
112800000000     C                   EXSR      RISTAM
112900940428    1C                   ENDIF
113000000000      *
113100000000      * Stampa terminata
113200090916    1C     *IN38         IFEQ      *OFF
113300940429     C*
113400090916    2C     $ALMEN        IFEQ      *ZERO
113500950118     C                   WRITE     LSC2E1
113600940505     C                   ELSE
113700940429     C*
113800000000     C                   EXSR      STATER
113900090916    2C                   ENDIF
114000000000      *
114100950118     C                   WRITE     LSC2E2
114200950629     C                   WRITE     LSC2E5
114300000000      *
114400000000      * Verifico se vi sono fogli viaggio aperti
114500000000     C                   EXSR      FOGVIA
114600000000      *
114700970807     C                   CLOSE     FNLSC2P
114800090916    1C                   ENDIF
114900000000      *
115000000000      * Aggiorno la data ultima elaborazione
115100090916    1C     DS1RIS        IFNE      'R'
115200000616     C     SAVRIS        OREQ      'S'
115300021016
115400021016     c                   Eval      kTbeCod = 'SDF'
115500021016     c                   Movel(p)  Simfel        kTbeKe1
115600021016     c     Ktbe          Chain     Tntbe01l
115700090916    2c                   If        %Found(Tntbe01l)
115800021016     c                   Movel     TbeUni        dSdf
115900021016     c                   If        §SdfTerP = *Zeros or §SdfTerP > Ds1Ada
116000021016     c                   Z-Add     Ds1Ada        §SdfTerP
116100021016     c                   EndIf
116200021016     c                   If        §SdfTerU < Ds1Ada
116300021016     c                   Z-Add     Ds1Ada        §SdfTerU
116400021016     c                   EndIf
116500021016     c                   Movel     dSdf          TbeUni
116600021016     c                   Update    Tntbe000
116700090916    2c                   EndIf
116800090916    1C                   ENDIF
116900090415     c
117000090415     c* Se non ho aggioranto la tabella, vedo se devo cancellare record
117100090415     c*  in FISTA
117200090916    1c                   if        savris='S'
117300090415     c     ksta          delete    fista01l
117400090916    1C                   ENDIF
117500000912     C*
117600000000      *
117700940505     C     ENDLEG        ENDSR
117800000000      **************************************************************************
117900000000      * Una volta letto il record verifico se da includere o no
118000940427      * PER QUALSIASI TIPO DI CALCOLO  (COLLI E BOLLE A TERRA E
118100940427      *  COLLI E BOLLA DA PARTIRE)
118200000000      *
118300940427      * Vi sono solo due motivi per escludere il record letto:
118400940427      * (1) se la spedizione e' stata bollettata dopo la data elaboraz
118500950210      *                Data spedizione > DS1AAL
118600000000      *
118700940427      * (2) se la data partenza del collo e' < della data elaborazione
118800950210      *                BLTDFV < DS1AAL
118900000000      *
119000000810      * ATTENZIONE.. DA OGGI C'E' UN MOTIVO IN PIU'.. Canale5 ! da cantare ..
119100000810      *
119200000810      * (3) se la linea di partenze è ??..... POSTE !
119300000810      *
119400940427      * Bolle di recupero / bolle legate / bolle "£" sono escluse
119500000000      **************************************************************************
119600000000     C     ESCLUS        BEGSR
119700000000      *
119800000000     C                   MOVE      'N'           $OK               1
119900000331     C                   MOVE      *BLANKS       WSOS              1
120000000810      *
120100000810      *
120200940427      * esclusioni:
120300950323      * Le bolle "legate" sono identificabili dal nr.foglio viaggio
120400950323      * = 99999 O = 99998
120500070522    1C     $NFVPR        IFEQ      99999
120600950323     C     $NFVPR        OREQ      99998
120700940427     C     $CBLPR        OREQ      '£'
120800940428     C     §3ARBL        OREQ      'R'
120900000000     C                   GOTO      ENDESC
121000070522    1C                   ENDIF
121100000413     C* Prepagati non confermati
121200070522    1C     $CDPDR        IFEQ      *ZEROS
121300000413     C                   GOTO      ENDESC
121400070522    1C                   ENDIF
121500000000      *
121600070522    1C     BLTDFV        IFEQ      DS1AAL
121700950629     C                   MOVEL     'S'           $OK
121800070522   x1C                   ELSE
121900070522     C
122000070522     c* ELAborazione in base a    data RITIRO Merce
122100070522      * La data ritiro deve essere <= alla data elaborazione
122200070522    2C     $DTRIT        IFLE      DS1AAL
122300070522      *
122400070525      * La data partenza deve essere > = alla data elaborazione        limiti
122500070522    3C     BLTDFV        IFEQ      *ZERO
122600070522     C     BLTDFV        ORGE      DS1AAL
122700070522     C                   MOVE      'S'           $OK
122800070522     C                   GOTO      ENDESC
122900070522    3C                   ENDIF
123000070522    2C                   ENDIF
123100950629     C*
123200070522    1C                   ENDIF
123300000000      *
123400000000     C     ENDESC        ENDSR
123500000000      **************************************************************************
123600000000      * Elaborazioni dei record letti, somme nei totali
123700000000      **************************************************************************
123800000000     C     ELABOR        BEGSR
123900000000      *
124000090916      * Verifico il tipo servizio
124100090916     C                   Z-ADD     5             $T                1 0
124200090916     c                   select
124300090916     C     BLPTSP        wheneq    'E'
124400000000     C                   Z-ADD     1             $T
124500090916     C
124600090916     C     BLPTSP        wheneq    'D'
124700940323     C                   Z-ADD     2             $T
124800090916
124900090916     C     BLPTSP        wheneq    'C'
125000090916     C                   Z-ADD     3             $T
125100090916
125200090916     C     BLPTSP        wheneq    'H'
125300090916     C                   Z-ADD     4             $T
125400090916     c                   endsl
125500000000      *
125600000000      * C/servizio oppure no ?
125700940428     C     §3ARBL        COMP      'C'                                3434
125800000000      *
125900970205     C*
126000070522    1C     WSOS          IFEQ      *BLANKS
126100970205     C* DETERMINAZIONE SE COLLO A TERRA
126200000000      * Ho due controlli da fare per sapere se il collo e' a terra:
126300000000      *    1)  BLTDFV = 0
126400950210      *    2)  BLTDFV > Data spedizione   AND   BLTDFV > DS1AAL
126500000000     C                   SETOFF                                       36
126600070522    2C     BLTDFV        IFEQ      *ZERO
126700950210     C     BLTDFV        ORGT      DS1AAL
126800000000     C                   SETON                                        36
126900070522    2C                   ENDIF
127000970303     C*
127100970303     C* RICHIAMO PGM PER CALCOLO AFFIDABILITA'
127200970303     C* (SOLO SE HO LETTO UNA NUOVA BOLLA - $BOLLA = '1')
127300070522    2C     $BOLLA        IFEQ      '1'
127400070522    3C     BLPDCE        IFEQ      *ZEROS
127500970303     C     BLPDUC        ORNE      DS1AAL
127600970303     C                   MOVEL     ' '           D98TLA
127700970303     C                   MOVEL     'P'           D98TBO
127800970303     C                   MOVEL     BLPAAS        D98AAS
127900970303     C                   MOVEL     BLPLNP        D98LNP
128000970303     C                   MOVEL     BLPNRS        D98NRS
128100970303     C                   MOVEL     BLPNSP        D98NSP
128200970310     C                   Z-ADD     DS1AAL        D98DRF
128300970310     C                   Z-ADD     2359          D98ORF
128400970925     C                   CALL      'TNSD99R'
128500970925     C                   PARM                    DS99
128600970303     C                   ELSE
128700970303     C                   Z-ADD     BLPNPI        D98NPI
128800070522    3C                   END
128900070522    2C                   END
129000970207     C*
129100970207     C*VERIFICO SE SI TRATTA DI "A TERRA DA PARTIRE"
129200970210     C* La considero "non da partire" se si tratta di:
129300970207     C* - spedizione non borderizzata
129400970303     C* - distribuzione a terra del giorno
129500070522     c* - dal 1° giugno 2007 il calcolo della affidabilità a terra sarà
129600070522     c*   in base alla data ritiro, per cui nel DA PARTIRE non controllo
129700070522     c*   più la data borderò ma solo se si tratta della distribuzione
129800070522     c*   del giorno
129900970205     C                   SETOFF                                       35
130000070522     c
130100070522    2C     *IN36         IFEQ      *ON
130200070522     c*
130300090108     C* SE affidabilità =0 verifico se cade in giorno festivo, per
130400090108     c*    ter-ter --> se si non do ritardo
130500090108     c                   if        d98npi=0
130600090108     C                   movel     blptfa        $TFA
130700090108      *
130800090108     C     AZCLNK        CHAIN     AZCLN01L                           99
130900090108     C                   MOVEA     CLNMAT        GME
131000090108     C                   MOVEA     CLNpom        GMEp
131100090108     C     GME($GI)      IFEQ      'F'
131200090108     C     GMEp($GI)     oreq      'F'
131300090108     c                   sub       24            d98npi
131400090108     c                   endif
131500090108     c                   endif
131600090108     c
131700070522    4c                   if        (blpdpc=0 and d98npi<0) or
131800070522     c                             (blpdpc>ds1aal and d98npi<0)
131900070522     C                   SETON                                        35
132000070522    4c                   endif
132100970210     C*
132200070522    2C                   END
132300070522    1C                   ENDIF
132400970205     C*
132500000000      *
132600970205      * Aggancio, nel file statistiche, il record per il terminal di
132700970205      * arrivo poiche' lo dovrò aggiornare
132800090916     C                   CLEAR                   fisat000
132900000000     C                   Z-ADD     SIMFEL        SATFLE
133000950210     C                   Z-ADD     DS1AAL        SATDRE
133100940428     C                   Z-ADD     BLPLNP        SATLNP
133200000000     C                   Z-ADD     BLPTFA        SATTFA
133300000000     C                   MOVE      *BLANK        SATTRE
133400090916     C     FNSA1K        CHAIN     fisat02L                           37
133500940428      *
133600970205      * NUOVA BOLLA LETTA: CONTO LE SPEDIZIONI
133700940428      *
133800940428     C     $BOLLA        IFEQ      '1'
133900940428     C                   EXSR      DATBOL
134000970205     C                   ENDIF
134100021217      *
134200021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
134300030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
134400030213      * Peso medio
134500030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
134600030213if  1c***                if        Wsipes = 'S'
134700030213     c***                z-add     BLTpuc        $Peso
134800030213   x1c***                else
134900021220     c                   z-add     Wpesmed       $Peso
135000030213e   1c***                endif
135100021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
135200030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
135300030213      * Volume medio
135400030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
135500030213if  1c***                if        Wsivol = 'S'
135600030213     c***                z-add     BLTvuc        $Volum
135700030213   x1c***                else
135800021220     c                   z-add     Wvolmed       $Volum
135900030213e   1c***                endif
136000000000      *
136100000000      * ?  CALCOLI  SUI  COLLI  ?
136200000000      *
136300000331     C     WSOS          IFEQ      *BLANKS
136400000000      * Nr.colli da partire (servira' per la percentuale)
136500970207     C     *IN35         IFEQ      *OFF
136600970210     C   34              ADD       1             CPA($T)
136700970210     C  N34              ADD       1             C4A($T)
136800970210     C                   ADD       1             CDT($T)
136900950626     C                   END
137000000000      *
137100940427     C     *IN36         IFEQ      *ON
137200970207      * A terra da partire                                             (35ON)
137300940427     C   34
137400940427     CANN35              ADD       1             CBP($T)
137500940427     C  N34
137600940427     CANN35              ADD       1             C4P($T)
137700970207     C  N35              ADD       1             C0P($T)
137800970207     C  N35              ADD       1             C3P($T)
137900970207      * A terra non da partire                                         (35ON)
138000940427     C   34
138100940427     CAN 35              ADD       1             CBR($T)
138200940427     C  N34
138300940427     CAN 35              ADD       1             C4R($T)
138400940427     C   35              ADD       1             C0R($T)
138500940427     C                   ENDIF
138600000000      *
138700000000      * ?  CALCOLI  SUL  PESO  ?
138800000000      *
138900970207     C     *IN35         IFEQ      *OFF
139000970210     C   34              ADD       $PESO         PPA($T)
139100970210     C  N34              ADD       $PESO         P4A($T)
139200970210     C                   ADD       $PESO         PDT($T)
139300950626     C                   END
139400940502      * A TERRA
139500940427     C     *IN36         IFEQ      *ON
139600940502      *
139700970206      * Fino a 35 quintali
139800021217     ***C     BLPPKF        IFLE      3500
139900021217     C     Wpeso         IFLE      3500
140000970207     C* Da partire
140100970206     C   34
140200970206     CANN35              ADD       $PESO         PFI($T)
140300970206     C  N34
140400970206     CANN35              ADD       $PESO         P4I($T)
140500970206     C  N35              ADD       $PESO         P3I($T)
140600970206     C  N35              ADD       $PESO         P0I($T)
140700970207     C* Non da partire
140800970206     C   34
140900970206     CAN 35              ADD       $PESO         PFN($T)
141000970206     C  N34
141100970206     CAN 35              ADD       $PESO         P4N($T)
141200970206     C   35              ADD       $PESO         P0N($T)
141300970206     C                   ELSE
141400970206      * Oltre 35 quintali:                                             (35ON)
141500970207     C* Da partire
141600940427     C   34
141700940427     CANN35              ADD       $PESO         PBP($T)
141800940427     C  N34
141900940427     CANN35              ADD       $PESO         P4P($T)
142000970206     C  N35              ADD       $PESO         P0P($T)
142100970206     C  N35              ADD       $PESO         P3P($T)
142200970207     C* Non da partire
142300940427     C   34
142400940427     CAN 35              ADD       $PESO         PBR($T)
142500940427     C  N34
142600940427     CAN 35              ADD       $PESO         P4R($T)
142700940427     C   35              ADD       $PESO         P0R($T)
142800940427     C                   ENDIF
142900970206     C                   ENDIF
143000000331     C*
143100000331     C                   ENDIF
143200000000      *
143300000000      * ?  CALCOLI  SUL  VOLUME  ?
143400940329      *
143500000000      *
143600021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
143700021217     ***C     BLTVUC        IFEQ      *ZERO
143800021217     *** *
143900021217     ***C     BLPVLF        DIV(H)    BLPNCL        $VOLUM           12 4
144000021217     *** *
144100021217     ***C                   ELSE
144200021217     ***C                   Z-ADD     BLTVUC        $VOLUM
144300021217     ***C                   ENDIF
144400000000      *
144500000331     C     WSOS          IFEQ      *BLANKS
144600970207     C     *IN35         IFEQ      *OFF
144700970210     C   34              ADD       $VOLUM        VPA($T)
144800970210     C  N34              ADD       $VOLUM        V4A($T)
144900970210     C                   ADD       $VOLUM        VDT($T)
145000950626     C                   END
145100940505      *
145200940428     C     *IN36         IFEQ      *ON
145300970207     C* Da partire
145400940428     C   34
145500940428     CANN35              ADD       $VOLUM        VBP($T)
145600940428     C  N34
145700940428     CANN35              ADD       $VOLUM        V4P($T)
145800970206     C  N35              ADD       $VOLUM        V0P($T)
145900970206     C  N35              ADD       $VOLUM        V3P($T)
146000000331     c* Sia che sia da partire che non da partire: sommo nel totale
146100000331     C* Mc rimasti a magazzino
146200000331     C   34              ADD       $VOLUM        VBR($T)
146300000331     C  N34              ADD       $VOLUM        V4R($T)
146400000331     C                   ADD       $VOLUM        V0R($T)
146500000406     C                   ADD       $VOLUM        V3R($T)
146600000000      *
146700000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
146800000000      *
146900970205      * Visto che le spedizioni non riesco a calcolarle ora, mi limito
147000970205      * a tenere aggiornato il file di work con le bolle
147100940428     C   34              MOVE      *BLANK        WRKCSE
147200940428     C  N34              MOVE      'C'           WRKCSE
147300940503     C                   Z-ADD     BLPLNP        WRKLNP
147400950118     C     FNWR1K        CHAIN     FNWRK00F                           99
147500940428     C     *IN99         IFEQ      *OFF
147600000000     C                   ADD       1             WRKNC1
147700000000     C                   EXCEPT    AGGIO
147800000000     C                   ELSE
147900950118     C                   MOVEL     *BLANKS       WTRANS            1
148000940503     C                   EXSR      WRIWRK
148100000000     C                   ENDIF
148200940428     C                   ENDIF
148300000331     c* Se collo di bolla in sospensione sommo volume in totale mc
148400000331     c* rimasti a magazzino
148500000331     C                   ELSE
148600000331     C   34              ADD       $VOLUM        VBR($T)
148700000331     C  N34              ADD       $VOLUM        V4R($T)
148800000331     C                   ADD       $VOLUM        V0R($T)
148900000406     C                   ADD       $VOLUM        V3R($T)
149000000412     C                   ADD       $VOLUM        VOLUME            9 4
149100000331     C                   ENDIF
149200940323      *
149300040921     C                   MOVE      $FTR          SATFTR
149400040921     C                   MOVE      $DTR          SATDTR
149500940428     C     *IN37         IFEQ      *ON
149600040921     C****               MOVE      $FTR          SATFTR
149700040921     C****               MOVE      $DTR          SATDTR
149800090916     C                   WRITE     fisat000
149900940428     C                   ELSE
150000090916     C                   UPDATE    fisat000
150100940428     C                   ENDIF
150200000000      *
150300000000     C                   ENDSR
150400940428      **************************************************************************
150500940428      * Carico i dati della bolla (peso e volume)
150600940428      *  e conto le spedizioni partite il giorno di elaborazione
150700940428      **************************************************************************
150800940428     C     DATBOL        BEGSR
150900940428      *
151000021217     *** * Calcolo il peso di un singolo collo
151100021217     ***C     BLPPKF        DIV(H)    BLPNCL        $PESO             8 1
151200021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
151300021217     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
151400021217     *** * BLPVLC=Volume rilevato)
151500050112     ***C     BLPNCR        ifeq      BLPNCL
151600021217     ***C                   Z-ADD     BLPVLC        BLPVLF
151700021217     ***C                   ELSE
151800021217     ***C     BLPVLC        IFGE      BLPVLF
151900021217     ***C                   Z-ADD     BLPVLC        BLPVLF
152000021217     ***C                   ENDIF
152100021217     ***C                   ENDIF
152200021217      *
152300021217      * Verifico i dati misurati con la macchina smistacolli
152400021217      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
152500021220      * e calcolo il VOLUME unitario medio per collo da usare se non
152600021220      * tutti i colli sono stati volumati dalla smistacolli
152700021217      *
152800030213      **** Verifico se sono stati volumati tutti i colli
152900030213     c***                clear                   Wsivol
153000021217     c                   clear                   Wvolu
153100050112     c                   if        BLPncr = BLPncl
153200021217     c                   z-add     BLPvlc        Wvolu
153300030213     c***                eval      Wsivol =  'S'
153400021217   x1c                   else
153500021217if  2c                   if        BLPvlc >= BLPvlf
153600021217     c                   z-add     BLPvlc        Wvolu
153700021217   x2c                   else
153800021217     c                   z-add     BLPvlf        Wvolu
153900021217e   2c                   endif
154000021217e   1c                   endif
154100021220      *
154200021220      * Calcolo il volume medio
154300021220     c     Wvolu         div(H)    BLPncl        Wvolmed
154400021217      *
154500021217      * Verifico i dati misurati con la macchina smistacolli
154600021217      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
154700021220      * e calcolo il PESO   unitario medio per collo da usare se non
154800021220      * tutti i colli sono stati pesati   dalla smistacolli
154900021217      *
155000030213      **** Verifico se sono stati pesati   tutti i colli
155100030213     c***                clear                   Wsipes
155200021217     c                   clear                   Wpeso
155300050112     c                   If        BLPncp = BLPncl
155400021217     c                   z-add     BLPpkc        Wpeso
155500030213     c***                eval      Wsipes =  'S'
155600021217   c1c                   else
155700021217if  2c                   if        BLPpkc >= BLPpkf
155800021217     c                   z-add     BLPpkc        Wpeso
155900021217   x2c                   else
156000021217     c                   z-add     BLPpkf        Wpeso
156100021217e   2c                   endif
156200021217e   1c                   endif
156300021220      *
156400021220      * Calcolo il peso medio
156500021220     c     Wpeso         div(H)    BLPncl        Wpesmed
156600940428     C*
156700940428     C* SOMMO SOLTANTO LE SPEDIZIONI PARTITE NEL GIORNO
156800970205     C*  LA DATA DI RIFERIMENTO E' LA DATA ULTIMO COLLO
156900940506     C     $FLAG         IFEQ      ' '
157000970205     C     BLPDUC        ANDEQ     DS1AAL
157100000331     C     WSOS          ANDEQ     *BLANKS
157200950118     C                   MOVEL     *BLANKS       WTRANS
157300940503     C                   EXSR      CTRBOL
157400970205     C                   ENDIF
157500940428     C*
157600940428     C                   MOVEL     '2'           $BOLLA
157700940428     C                   ENDSR
157800950118      **************************************************************************
157900021217      * Carico i dati della bolla (peso e volume) (TRANSITI)
158000021217     *** *  e conto le spedizioni partite il giorno di elaborazione
158100021217     *** *    (questo NON lo faceva NEANCHE prima)
158200950118      **************************************************************************
158300950118     C     DATBOT        BEGSR
158400950118      *
158500021217     *** * Calcolo il peso di un singolo collo
158600021217     ***C     BTPPKB        DIV(H)    BTPNCL        $PESO             8 1
158700021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
158800021217     *** * misurati con la macchina smistacolli (BTPNCR=Numero colli rilevati,
158900021217     *** * BTPVLC=Volume rilevato)
159000050112     ***C     BTPNCR        ifeq      BTPNCL
159100021217     ***C                   Z-ADD     BTPVLC        BTPVLB
159200021217     ***C                   ELSE
159300021217     ***C     BTPVLC        IFGE      BTPVLB
159400021217     ***C                   Z-ADD     BTPVLC        BTPVLB
159500021217     ***C                   ENDIF
159600021217     ***C                   ENDIF
159700021217     ***C*
159800021220      *
159900021220      * Verifico i dati misurati con la macchina smistacolli
160000021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
160100021220      * e calcolo il VOLUME unitario medio per collo da usare se non
160200021220      * tutti i colli sono stati volumati dalla smistacolli
160300021217      *
160400030213      **** Verifico se sono stati volumati tutti i colli
160500030213     c***                clear                   Wsivol
160600021217     c                   clear                   Wvolu
160700050112     c                   if        BTPncr = BTPncl
160800021217     c                   z-add     BTPvlc        Wvolu
160900030213     c***                eval      Wsivol =  'S'
161000021217   x1c                   else
161100021217if  2c                   if        BTPvlc >= BTPvlb
161200021217     c                   z-add     BTPvlc        Wvolu
161300021217   x2c                   else
161400021217     c                   z-add     BTPvlb        Wvolu
161500021217e   2c                   endif
161600021217e   1c                   endif
161700021220      *
161800021220      * Calcolo il volume medio
161900021220     c     Wvolu         div(H)    BTPncl        Wvolmed
162000021220      *
162100021220      * Verifico i dati misurati con la macchina smistacolli
162200021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
162300021220      * e calcolo il PESO   unitario medio per collo da usare se non
162400021220      * tutti i colli sono stati pesati   dalla smistacolli
162500021217      *
162600030213      **** Verifico se sono stati pesati   tutti i colli
162700030213     c***                clear                   Wsipes
162800021217     c                   clear                   Wpeso
162900050112     c                   if        BTPncp = BTPncl
163000021217     c                   z-add     BTPpkc        Wpeso
163100030213     c***                eval      Wsipes =  'S'
163200021217   x1c                   else
163300021217if  2c                   if        BTPpkc >= BTPpkb
163400021217     c                   z-add     BTPpkc        Wpeso
163500021217   x2c                   else
163600021217     c                   z-add     BTPpkb        Wpeso
163700021217e   2c                   endif
163800021217e   1c                   endif
163900021220      *
164000021220      * Calcolo il peso medio
164100071231     c     Wpeso         div(H)    BTPncl        Wpesmed
164200021217      *
164300950118     C                   MOVEL     '2'           $BOLLA
164400950118     C                   ENDSR
164500940503      **************************************************************************
164600950118      * Scrivo il file FNWRK00F
164700940503      **************************************************************************
164800940503     C     WRIWRK        BEGSR
164900950118     C*
165000940503     C                   MOVE      $FLAG         WRKFLG
165100950118     C                   Z-ADD     1             WRKNC1
165200950118     C*
165300950118     C     WTRANS        IFNE      'T'
165400940503     C                   Z-ADD     BLPAAS        WRKAAS
165500940503     C                   Z-ADD     BLPMGS        WRKMGS
165600940503     C                   Z-ADD     BLPNRS        WRKNRS
165700940503     C                   Z-ADD     BLPNSP        WRKNSP
165800940503     C                   Z-ADD     BLPLNA        WRKLNA
165900940503     C                   MOVE      BLPTSP        WRKTSP
166000940503     C                   Z-ADD     BLPNCL        WRKNCL
166100940503     C                   Z-ADD     BLPTFA        WRKTFA
166200101126     c*
166300070525     C                   Z-ADD     BLPDrt        WRKDBR
166400070525     c
166500970303     C                   Z-ADD     BLPDPC        WRKDPC
166600970303     C                   Z-ADD     BLPDUC        WRKDUC
166700970303     C                   Z-ADD     D98NPI        WRKNPI
166800950118     C*
166900950118     C                   ELSE
167000950118     C*
167100950118     C                   Z-ADD     BTPAAS        WRKAAS
167200950118     C                   Z-ADD     BTPMGS        WRKMGS
167300950118     C                   Z-ADD     BTPNRS        WRKNRS
167400950118     C                   Z-ADD     BTPNSP        WRKNSP
167500950118     C                   Z-ADD     BTPLNA        WRKLNA
167600950118     C                   MOVE      BTPTSP        WRKTSP
167700950118     C                   Z-ADD     BTPNCL        WRKNCL
167800950118     C                   Z-ADD     BTPTFA        WRKTFA
167900970207     C                   CLEAR                   WRKDBR
168000970303     C                   CLEAR                   WRKDUC
168100970303     C                   CLEAR                   WRKDPC
168200970303     C                   CLEAR                   WRKNPI
168300950118     C                   ENDIF
168400950118     C*
168500950118     C                   WRITE     FNWRK000
168600950118     C*
168700940503     C                   ENDSR
168800940503      **************************************************************************
168900940503      * Calcolo le spedizioni da partire:
169000950118      *  vedo se gia' contata prima di ricontarla(se esiste in FNWRK)
169100970207      *  Potrei averla già contata se i colli non sono sequenziali
169200940503      **************************************************************************
169300940503     C     CTRBOL        BEGSR
169400940503     C*
169500940505     C                   MOVEL     $FLAG         $FLAG2
169600940503     C                   MOVEL     'P'           $FLAG
169700940503     C                   MOVEL     BLPLNP        WRKLNP
169800940503     C                   CLEAR                   WRKCSE
169900950118     C     FNWR1K        SETLL     FNWRK00F                               99
170000940503     C* BOLLA NON TROVATA  LA SCRIVO
170100940503    1C     *IN99         IFEQ      *OFF
170200940503     C                   EXSR      WRIWRK
170300940503     C*
170400970211     C                   ADD       1             SDT($T)
170500970210     C   34              ADD       1             SPA($T)
170600940503     C* S4A --> PER C/S SPEDIZIONI DELL'AREA
170700940503     C*         PER I TRANSITI
170800970210     C  N34              ADD       1             S4A($T)
170900940503    1C                   ENDIF
171000940505     C                   MOVEL     $FLAG2        $FLAG
171100940503     C                   ENDSR
171200000000      **************************************************************************
171300000000      * Calcolo sulle spedizioni, lettura del file di work creato prima
171400000000      **************************************************************************
171500000000     C     CALSPE        BEGSR
171600940502     C                   SETOFF                                       3435
171700940428      * Verifico se e' transito
171800940502     C     $FLAG         COMP      'T'                                    35
171900000000      *
172000000000      * Leggo tutti le spedizioni della filiale da stampare dal file di work
172100970207    1C     $FLAG         IFEQ      *BLANK
172200950118     C     FNWRKK        SETLL     FNWRK00F
172300970207   X1C                   ELSE
172400950118     C     $FLAG         SETLL     FNWRK00F
172500970207    1C                   ENDIF
172600000000      *
172700970207    1C                   DO        *HIVAL
172800000000      *
172900970207    2C     $FLAG         IFEQ      *BLANK
173000950118     C     FNWRKK        READE(N)  FNWRK00F                               99
173100970207   X2C                   ELSE
173200950118     C     $FLAG         READE(N)  FNWRK00F                               99
173300970207    2C                   ENDIF
173400000000      *
173500970207    2C  N99              DO
173600000000      *
173700090916      * Verifico il tipo servizio
173800090916     C                   Z-ADD     5             $V                1 0
173900090916     C                   SELECT
174000090916     C     wrktsp        wheneq    'E'
174100000000     C                   Z-ADD     1             $V
174200090916     C     wrktsp        wheneq    'D'
174300940323     C                   Z-ADD     2             $V
174400090916     C     wrktsp        wheneq    'C'
174500090916     C                   Z-ADD     3             $V
174600090916     C     wrktsp        wheneq    'H'
174700090916     C                   Z-ADD     4             $V
174800090916    3C                   ENDsl
174900940428      *
175000940428      * Verifico se e' c/servizio
175100940502     C  N35WRKCSE        COMP      *BLANKS                                34
175200000000      *
175300000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
175400000000      * poiche' lo dovro aggiornare
175500090916     C                   CLEAR                   fisat000
175600000000     C                   Z-ADD     SIMFEL        SATFLE
175700950210     C                   Z-ADD     DS1ADA        SATDRE
175800940506     C  N35              Z-ADD     WRKLNP        SATLNP
175900940506     C   35              Z-ADD     SIMFEL        SATLNP
176000000000     C                   Z-ADD     WRKTFA        SATTFA
176100000000     C  N35              MOVE      *BLANK        SATTRE
176200000000     C   35              MOVE      'T'           SATTRE
176300090916     C     FNSA1K        CHAIN     fisat02L                           37
176400000000      *
176500950628     C                   MOVE      WRKMGS        $DATS1            8 0
176600950628     C                   MOVEL     WRKAAS        $DATS1
176700950628     C*
176800970207     C* A TERRA COMPLETAMENTE
176900970207      * La spedizione e' completa se il nr.colli spediti (WRKNC1) e' = uguale
177000050112      * al nr.colli della bolla (WRKNCL)
177100970207    3C     WRKNC1        IFEQ      WRKNCL
177200940506     C                   ADD       1             S0M($V)
177300970207     C* Transiti
177400970207    4C     *IN35         IFEQ      *ON
177500940506     C                   ADD       1             S2M($V)
177600970207   X4C                   ELSE
177700970207     C* Dell'area
177800940506     C   34              ADD       1             SCM($V)
177900940506     C  N34              ADD       1             S4M($V)
178000940506     C                   ADD       1             S3M($V)
178100970207     C*
178200970207    4C                   ENDIF
178300970207    3C                   ENDIF
178400970210     C* COMDAP = BLANKS => A TERRA DA PARTIRE
178500101126     C                   CLEAR                   COMDAP            1
178600070525     c
178700070525     c* calcolo per data ritiro a terra non da partire solo la distribu
178800070525     c*  zione del giorno
178900070525    4c                   if        (wrkdpc=0 and wrknpi<0) or
179000070525     c                             (wrkdpc>ds1aal and wrknpi<0)
179100070525     C                   MOVE      'N'           COMDAP
179200070525    4c                   endif
179300070525     c
179400970207     C*
179500970207    3C     COMDAP        IFEQ      *BLANKS
179600970207     C* Per i transiti memorizzo in un unico totale le spediz. a terra
179700970207     C* e non mi interessa nemmeno memorizzare il totale da partire
179800970207     C     $FLAG         OREQ      'T'
179900970207     C***
180000970207     C* TOTALE DA PARTIRE
180100970207     C***
180200970207     C* Sommo le spediz.a terra border. esclusa la distrib. del periodo
180300970210     C  N35              ADD       1             SDT($V)
180400970210     C  N35
180500970210     CAN 34              ADD       1             SPA($V)
180600970207     C* S4A --> Per c/s spedizioni dell'area
180700970210     C  N35
180800970210     CANN34              ADD       1             S4A($V)
180900970207     C***
181000970207     C* A TERRA DA PARTIRE
181100970207     C***
181200970207     C   34
181300970207     CANN35              ADD       1             SBP($V)
181400970207     C  N34
181500970207     CANN35              ADD       1             S4P($V)
181600970207     C  N35              ADD       1             S3P($V)
181700970207     C   35              ADD       1             S2P($V)
181800970207     C                   ADD       1             S0P($V)
181900970305     C***
182000970305     C* A TERRA DA PARTIRE FUORI AFFIDABILITA'
182100970305     C***
182200970305    4C     WRKNPI        IFGE      0
182300970305     C   34
182400970305     CANN35              ADD       1             FAF($V)
182500970305     C  N34
182600970305     CANN35              ADD       1             F4F($V)
182700970305     C  N35              ADD       1             F3F($V)
182800970305     C  N35              ADD       1             F0F($V)
182900970305    4C                   END
183000970207     c*
183100970207   X3C                   ELSE
183200970207     C***
183300970207     C* A TERRA NON DA PARTIRE
183400970207     C***
183500970207     C   34              ADD       1             SBR($V)
183600970207     C  N34              ADD       1             S4R($V)
183700970207     C                   ADD       1             S3R($V)
183800970207     C                   ADD       1             S0R($V)
183900970207    3C                   ENDIF
184000000000      *
184100040922     c  n37              move      $FTR          satftr
184200040922     c  n37              move      $DTR          satDTR
184300090916     C  N37              UPDATE    fisat000
184400000000      *
184500970207    2C                   ENDDO
184600970207    1C  N99              ENDDO
184700000000      *
184800000000     C                   ENDSR
184900000000      **************************************************************************
185000000000      * Calcolo i totali di filiale, li stampo e scrivo sul file
185100000000      **************************************************************************
185200000000     C     TOTFIL        BEGSR
185300000000      *
185400000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
185500000000      *
185600970211     C                   SETON                                        404145
185700000000     C                   Z-ADD     $FILPR        FILIAP
185800000000      *
185900000000      * Decodifico il codice filiale
186000000000     C     $FILPR        CHAIN     AZORG01L                           99
186100000000     C  N99ORGFVA        COMP      *BLANK                             9999
186200940503     C   99              CLEAR                   ORGDEP
186300000000     C  N99              MOVEL     ORGDES        ORGDEP
186400000000      *
186500000000      * ?  C  O   L   L  I  ?
186600970210     C                   XFOOT     CBP           COTOTP
186700970210     C                   XFOOT     CPA           $CDA
186800940428     C                   EXSR      PERCOL
186900000000      *
187000000000      * ?  P  E  S  O  ?
187100970210     C                   XFOOT     PFI           PEF35P
187200970210     C                   XFOOT     PBP           PEO35P
187300970210     C     PEF35P        ADD       PEO35P        PETOTP
187400970210     C                   XFOOT     PPA           $PDA
187500940428     C                   EXSR      PERPES
187600000000      *
187700000000      * ?  V  O  L  U  M E  ?
187800970210     C                   XFOOT     VBP           VOTOTP
187900970210     C                   XFOOT     VPA           $VDA
188000940428     C                   EXSR      PERVOL
188100000000      *
188200000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
188300970210     C                   XFOOT     SPA           $SDA
188400970305     C                   XFOOT     FAF           SPFAFF
188500940502     C                   EXSR      PERSPE
188600970305     C                   XFOOT     SBP           SPTOTP
188700970305     C                   XFOOT     SCM           SPCONP
188800970305     C                   XFOOT     SBR           $COMOD
188900970211     C     SPTOTP        ADD       $COMOD        SPTOTA
189000970211     C     SPTOTA        SUB       SPCONP        SPSPAP
189100000331      *
189200000331      * ? VOLUME A MAGAZZINO?
189300000331     C                   XFOOT     VBR           VOTOTM
189400000000      *
189500970210     C* STAMPO SOLO SE HO DEI DATI
189600000331     C     VOTOTM        IFGT      *ZEROS
189700000417     C* HO RIMESSO ANCHE TEST SU SPTOTA PER FAR FUNZIONARE LE RISTAMPE
189800000417     C* RELATIVE A DATE ELABORATE PRIMA CHE VENISSE FATTO IL CALCOLO
189900000417     C* DEL TOTALE VOLUME A MAGAZZINO
190000000417     C     SPTOTA        ORGT      *ZEROS
190100970210     C*
190200000000     C                   Z-ADD     $FILPR        FILIAP
190300000000     C                   EXSR      SCRIVI
190400940509     C                   ADD       1             $RIGHE            2 0
190500000000      *
190600090916      * ------------------ ESPR./DIST./CORR./H1030 --------------------
190700000000      *
190800970210     C                   SETOFF                                       40
190900090916
191000090916     C                   DO        ciclo         $V
191100940429     C                   CLEAR                   DESCRP
191200940429      *
191300970210     C                   MOVE      ENS($V)       TIPOP
191400000000      *
191500000000      * ?  C  O   L   L  I  ?
191600970211     C                   Z-ADD     CBP($V)       COTOTP
191700970211     C                   Z-ADD     CPA($V)       $CDA
191800940428     C                   EXSR      PERCOL
191900000000      *
192000000000      * ?  P  E  S  O  ?
192100000000     C                   Z-ADD     PFI($V)       PEF35P
192200970211     C                   Z-ADD     PBP($V)       PEO35P
192300970212     C     PEF35P        ADD       PEO35P        PETOTP
192400970211     C                   Z-ADD     PPA($V)       $PDA
192500940428     C                   EXSR      PERPES
192600000000      *
192700000000      * ?  V  O  L  U  M E  ?
192800970211     C                   Z-ADD     VBP($V)       VOTOTP
192900970211     C                   Z-ADD     VPA($V)       $VDA
193000940428     C                   EXSR      PERVOL
193100000000      *
193200000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
193300970305     C                   Z-ADD     SPA($V)       $SDA
193400970305     C                   Z-ADD     FAF($V)       SPFAFF
193500940502     C                   EXSR      PERSPE
193600970305     C                   Z-ADD     SBP($V)       SPTOTP
193700970305     C                   Z-ADD     SCM($V)       SPCONP
193800970211     C     SBP($V)       ADD       SBR($V)       SPTOTA
193900970211     C     SPTOTA        SUB       SCM($V)       SPSPAP
194000000331      *
194100000331      * ?tot.volume a magazz?
194200000331     C                   Z-ADD     VBR($V)       VOTOTM
194300000000      *
194400000000     C                   EXSR      SCRIVI
194500000000      *
194600000000     C                   ENDDO
194700000000      *
194800950118     C                   WRITE     LSC2T2
194900940506     C                   ENDIF
195000940328      *
195100000417     C                   ENDSR
195200940428      **************************************************************************
195300940428      * Calcolo percentuale sui colli
195400940428      **************************************************************************
195500940428     C     PERCOL        BEGSR
195600940502     C                   Z-ADD     *ZERO         COSDPP
195700940428     C*
195800940502     C* PERCENTUALE SUL DA PARTIRE
195900970213     C  N02$CDA          IFNE      *ZERO
196000970210     C     COTOTP        DIV       $CDA          $COMOD
196100940502     C     $COMOD        MULT(H)   100           COSDPP
196200940502     C                   ENDIF
196300940428     C                   ENDSR
196400940428      **************************************************************************
196500940428      * Calcolo percentuale sul peso
196600940428      **************************************************************************
196700940428     C     PERPES        BEGSR
196800940502     C                   CLEAR                   PESDPP
196900940428     C*
197000940502     C* PERCENTUALE SUL DA PARTIRE
197100970213     C  N02$PDA          IFNE      *ZERO
197200970210     C     PETOTP        DIV       $PDA          $COMOD
197300940502     C     $COMOD        MULT(H)   100           PESDPP
197400940502     C                   ENDIF
197500940428     C                   ENDSR
197600940428      **************************************************************************
197700940428      * Calcolo percentuale sul volume
197800940428      **************************************************************************
197900940428     C     PERVOL        BEGSR
198000940502     C                   Z-ADD     *ZERO         VOSDPP
198100940502     C*
198200940502     C* PERCENTUALE SUL DA PARTIRE
198300970213     C  N02$VDA          IFNE      *ZERO
198400970210     C     VOTOTP        DIV       $VDA          $COMOD
198500940502     C     $COMOD        MULT(H)   100           VOSDPP
198600940502     C                   ENDIF
198700940428     C                   ENDSR
198800940428      **************************************************************************
198900940428      * Calcolo percentuale sulle spedizioni
199000940428      **************************************************************************
199100940428     C     PERSPE        BEGSR
199200940502     C                   Z-ADD     *ZERO         SPSDPP
199300940428     C*
199400940502     C* PERCENTUALE SUL DA PARTIRE
199500970213     C  N02$SDA          IFNE      *ZERO
199600970305     C     SPFAFF        DIV       $SDA          $COMOD
199700940502     C     $COMOD        MULT(H)   100           SPSDPP
199800940502     C                   ENDIF
199900940428     C                   ENDSR
200000000000      **************************************************************************
200100000000      * Azzeramento dei totali di filiale per nuovi conti
200200000000      **************************************************************************
200300000000     C     AZZFIL        BEGSR
200400000412     C                   CLEAR                   VOLUME
200500000000      *
200600000000      * COLLI
200700970211     C                   ADD       CPA(1)        C3A(1)
200800970211     C                   ADD       CPA(2)        C3A(2)
200900970211     C                   ADD       CPA(3)        C3A(3)
201000090916     C                   ADD       CPA(4)        C3A(4)
201100090916     C                   ADD       CPA(5)        C3A(5)
201200970211     C                   CLEAR                   CPA
201300970211     C                   CLEAR                   CBP
201400970211     C                   CLEAR                   CBR
201500970211      * PESI
201600970211     C                   ADD       PPA(1)        P3A(1)
201700970211     C                   ADD       PPA(2)        P3A(2)
201800970211     C                   ADD       PPA(3)        P3A(3)
201900090916     C                   ADD       PPA(4)        P3A(4)
202000090916     C                   ADD       PPA(5)        P3A(5)
202100970211     C                   CLEAR                   PFI
202200970211     C                   CLEAR                   PPA
202300970211     C                   CLEAR                   PBP
202400970211     C                   CLEAR                   PBR
202500970211     C                   CLEAR                   PFN
202600000000      * VOLUMI
202700970211     C                   ADD       VPA(1)        V3A(1)
202800970211     C                   ADD       VPA(2)        V3A(2)
202900970211     C                   ADD       VPA(3)        V3A(3)
203000090916     C                   ADD       VPA(4)        V3A(4)
203100090916     C                   ADD       VPA(5)        V3A(5)
203200970211     C                   CLEAR                   VPA
203300970211     C                   CLEAR                   VBP
203400970211     C                   CLEAR                   VBR
203500970211      * SPEDIZIONI
203600970211     C                   ADD       SPA(1)        S3A(1)
203700970211     C                   ADD       SPA(2)        S3A(2)
203800970211     C                   ADD       SPA(3)        S3A(3)
203900090916     C                   ADD       SPA(4)        S3A(4)
204000090916     C                   ADD       SPA(5)        S3A(5)
204100970211     C                   CLEAR                   SPA
204200970211     C                   CLEAR                   SBP
204300970211     C                   CLEAR                   SBR
204400970211     C                   CLEAR                   SCM
204500970305     C                   CLEAR                   FAF
204600000000      *
204700000000     C                   ENDSR
204800000000      **************************************************************************
204900000000      * Azzeramento dei totali di c/servizio per nuova area
205000000000      **************************************************************************
205100000000     C     AZZSER        BEGSR
205200000000      *
205300000000      * COLLI
205400940506     C                   ADD       C1A(1)        C3A(1)
205500970211     C                   ADD       C1A(2)        C3A(2)
205600970211     C                   ADD       C1A(3)        C3A(3)
205700090916     C                   ADD       C1A(4)        C3A(4)
205800090916     C                   ADD       C1A(5)        C3A(5)
205900000000     C                   CLEAR                   C1A
206000000000     C                   CLEAR                   C1P
206100000000     C                   CLEAR                   C1R
206200000000      * PESI
206300940506     C                   ADD       P1A(1)        P3A(1)
206400970211     C                   ADD       P1A(2)        P3A(2)
206500970211     C                   ADD       P1A(3)        P3A(3)
206600090916     C                   ADD       P1A(4)        P3A(4)
206700090916     C                   ADD       P1A(5)        P3A(5)
206800970211     C                   CLEAR                   P1I
206900970211     C                   CLEAR                   P1N
207000970211     C                   CLEAR                   P1P
207100970211     C                   CLEAR                   P1R
207200970211     C                   CLEAR                   P1A
207300970211      * VOLUMI
207400970211     C                   ADD       V1A(1)        V3A(1)
207500970211     C                   ADD       V1A(2)        V3A(2)
207600970211     C                   ADD       V1A(3)        V3A(3)
207700090916     C                   ADD       V1A(4)        V3A(4)
207800090916     C                   ADD       V1A(5)        V3A(5)
207900970211     C                   CLEAR                   V1A
208000970211     C                   CLEAR                   V1P
208100970211     C                   CLEAR                   V1R
208200000000      * SPEDIZIONI
208300940506     C                   ADD       S1A(1)        S3A(1)
208400970211     C                   ADD       S1A(2)        S3A(2)
208500970211     C                   ADD       S1A(3)        S3A(3)
208600090916     C                   ADD       S1A(4)        S3A(4)
208700090916     C                   ADD       S1A(5)        S3A(5)
208800970211     C                   CLEAR                   S1A
208900970211     C                   CLEAR                   S1P
209000970211     C                   CLEAR                   S1R
209100970211     C                   CLEAR                   S1M
209200970305     C                   CLEAR                   F1F
209300000000      *
209400000000     C                   ENDSR
209500000000      **************************************************************************
209600000000      * Azzeramento dei totali di c/servizio per nuova filiale
209700000000      **************************************************************************
209800000000     C     AZZSE4        BEGSR
209900000000      *
210000000000      * COLLI
210100970210     C                   CLEAR                   C4A
210200970210     C                   CLEAR                   C4P
210300970210     C                   CLEAR                   C4R
210400000000      * PESI
210500970210     C                   CLEAR                   P4A
210600970210     C                   CLEAR                   P4I
210700970210     C                   CLEAR                   P4P
210800970210     C                   CLEAR                   P4R
210900970210     C                   CLEAR                   P4N
211000000000      * VOLUMI
211100970210     C                   CLEAR                   V4A
211200970210     C                   CLEAR                   V4P
211300970210     C                   CLEAR                   V4R
211400000000      * SPEDIZIONI
211500970210     C                   CLEAR                   S4A
211600970210     C                   CLEAR                   S4P
211700970210     C                   CLEAR                   S4R
211800970210     C                   CLEAR                   S4M
211900970305     C                   CLEAR                   F4F
212000000000      *
212100000000     C                   ENDSR
212200000000      **************************************************************************
212300000000      * Azzeramento dei totali di transito per nuovi totali
212400000000      **************************************************************************
212500000000     C     AZZTRA        BEGSR
212600940506     C* SOMMO NEI TOTALI
212700940506     C                   EXSR      ADDTRA
212800000000      * COLLI
212900940428     C                   CLEAR                   C2P
213000000000      * PESI
213100000000     C                   CLEAR                   P2I
213200940428     C                   CLEAR                   P2P
213300000000      * VOLUMI
213400940428     C                   CLEAR                   V2P
213500000000      * SPEDIZIONI
213600940428     C                   CLEAR                   S2P
213700000000     C                   CLEAR                   S2M
213800000000      *
213900000000     C                   ENDSR
214000000000      **************************************************************************
214100000000      * Stampo i totali per c/servizio
214200000000      **************************************************************************
214300000000     C     TOTSER        BEGSR
214400000000      *
214500000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
214600000000      *
214700970211     C                   SETON                                        4045
214800000000     C                   SETOFF                                       41
214900000000     C                   Z-ADD     *ZERO         FILIAP
215000000000     C                   MOVE      *BLANKS       ORGDEP
215100000000     C     'C/SERVIZ'    CAT       'I':0         ORGDEP
215200000000      *
215300970211      * ?  C  O   L   L  I  ?
215400970211     C                   XFOOT     C1P           COTOTP
215500970211     C                   XFOOT     C1A           $CDA
215600970211     C                   EXSR      PERCOL
215700970211      *
215800970211      * ?  P  E  S  O  ?
215900970211     C                   XFOOT     P1I           PEF35P
216000970211     C                   XFOOT     P1P           PEO35P
216100970211     C     PEF35P        ADD       PEO35P        PETOTP
216200970211     C                   XFOOT     P1A           $PDA
216300940429     C                   EXSR      PERPES
216400000000      *
216500000000      * ?  V  O  L  U  M E  ?
216600970211     C                   XFOOT     V1P           VOTOTP
216700970211     C                   XFOOT     V1A           $VDA
216800940429     C                   EXSR      PERVOL
216900000000      *
217000000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
217100970211     C                   XFOOT     S1A           $SDA
217200970305     C                   XFOOT     F1F           SPFAFF
217300970211     C                   EXSR      PERSPE
217400970305     C                   XFOOT     S1P           SPTOTP
217500970305     C                   XFOOT     S1M           SPCONP
217600970211     C                   XFOOT     S1R           $COMO1
217700970211     C     SPTOTP        ADD       $COMO1        SPTOTA
217800970211     C     SPTOTA        SUB       SPCONP        SPSPAP
217900000331      *
218000000331      * ?tot.volume a magazz?
218100000331     C                   XFOOT     V1R           VOTOTM
218200970211      *
218300970211     C* STAMPO SOLO SE HO DEI DATI
218400000331     C     VOTOTM        IFGT      *ZEROS
218500970211     C*
218600970211     C                   EXSR      SCRIVI
218700000000      *
218800090916      * ------------------ ESPR./DIST./CORR./H1030 --------------------
218900000000      *
219000970211     C                   SETOFF                                       40
219100090916     C                   DO        ciclo         $V
219200940323     C                   MOVE      ENS($V)       TIPOP
219300000000      *
219400000000      * ?  C  O   L   L  I  ?
219500970211     C                   Z-ADD     C1P($V)       COTOTP
219600970211     C                   Z-ADD     C1A($V)       $CDA
219700940429     C                   EXSR      PERCOL
219800000000      *
219900000000      * ?  P  E  S  O  ?
220000000000     C                   Z-ADD     P1I($V)       PEF35P
220100970211     C                   Z-ADD     P1P($V)       PEO35P
220200970211     C     PEF35P        ADD       PEO35P        PETOTP
220300970211     C                   Z-ADD     P1A($V)       $PDA
220400940429     C                   EXSR      PERPES
220500000000      *
220600000000      * ?  V  O  L  U  M E  ?
220700970211     C                   Z-ADD     V1P($V)       VOTOTP
220800970211     C                   Z-ADD     V1A($V)       $VDA
220900940429     C                   EXSR      PERVOL
221000000000      *
221100000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
221200970211     C                   Z-ADD     S1A($V)       $SDA
221300970305     C                   Z-ADD     F1F($V)       SPFAFF
221400940502     C                   EXSR      PERSPE
221500970305     C                   Z-ADD     S1P($V)       SPTOTP
221600970305     C                   Z-ADD     S1M($V)       SPCONP
221700970211     C     S1P($V)       ADD       S1R($V)       SPTOTA
221800970211     C     SPTOTA        SUB       S1M($V)       SPSPAP
221900000331      *
222000000331      * ?tot volume a magazz?
222100000331     C                   Z-ADD     V1R($V)       VOTOTM
222200000000      *
222300000000     C                   EXSR      SCRIVI
222400000000      *
222500000000     C                   ENDDO
222600940504     C                   ENDIF
222700940429     C                   EXSR      AZZSER
222800000000      *
222900000000     C                   ENDSR
223000000000      **************************************************************************
223100000000      * Stampo i totali per transito
223200000000      **************************************************************************
223300000000     C     TOTTRA        BEGSR
223400000000      *
223500000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
223600000000      *
223700940506     C                   SETON                                        400203
223800970211     C                   SETOFF                                       45
223900000000     C                   MOVE      *BLANKS       ORGDEP
224000940502     C* stampa per linea: totale transiti
224100940502     C     *IN43         IFEQ      *ON
224200940502     C                   Z-ADD     *ZERO         FILIAP
224300940502     C                   MOVEL     'TRANSITI'    ORGDEP
224400940502     C                   SETOFF                                       41
224500940502     C                   ELSE
224600940502     c* stampa per terminal in transito
224700940502     C                   SETON                                        41
224800940502      * Decodifico il terminal di arrivo
224900940502     C                   Z-ADD     $FILPR        FILIAP
225000940502     C     $FILPR        CHAIN     AZORG01L                           99
225100940502     C  N99ORGFVA        COMP      *BLANK                             9999
225200940503     C   99              CLEAR                   ORGDEP
225300940502     C  N99              MOVEL     ORGDES        ORGDEP
225400940502     C                   ENDIF
225500000000      *
225600000000      * ?  C  O   L   L  I  ?
225700940502     C                   XFOOT     C2P           COTOTP
225800970211     C*
225900970211      * SOLO SE HO TROVATO ALEMNO UN COLLO A TERRA STAMPO
226000970211     C     COTOTP        IFGT      0
226100940504      *
226200000000      *
226300000000      * ?  P  E  S  O  ?
226400970211     C                   XFOOT     P2I           PEF35P
226500970211     C                   XFOOT     P2P           PEO35P
226600970211     C     PEO35P        ADD       PEF35P        PETOTP
226700000000      *
226800000000      * ?  V  O  L  U  M E  ?
226900940428     C                   XFOOT     V2P           VOTOTP
227000000000      *
227100000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
227200940428     C                   XFOOT     S2P           SPTOTP
227300970211     C                   XFOOT     S2P           SPTOTA
227400970211     C                   XFOOT     S2M           SPCONP
227500970211     C     SPTOTA        SUB       SPCONP        SPSPAP
227600000000      *
227700000000     C                   EXSR      SCRIVI
227800940509     C                   ADD       1             $RIGHE
227900000000      *
228000090916      * ------------------ ESPR./DIST./CORR./H1030 --------------------
228100000000      *
228200940502     C                   SETOFF                                       4045
228300090916     C                   DO        CICLO         $V
228400940502     C* PER TERMINAL SCRIVO CHE E' RECORD DI TRANSITO
228500940429     C  N43$V            IFEQ      1
228600940506     C                   MOVEL     'IN TRANS'    DESCRP
228700940429     C                   ELSE
228800940429     C                   CLEAR                   DESCRP
228900940429     C                   ENDIF
229000940429     C*
229100940323     C                   MOVE      ENS($V)       TIPOP
229200000000      *
229300000000      * ?  C  O   L   L  I  ?
229400940428     C                   Z-ADD     C2P($V)       COTOTP
229500000000      *
229600000000      * ?  P  E  S  O  ?
229700970211     C                   Z-ADD     P2I($V)       PEF35P
229800970211     C                   Z-ADD     P2P($V)       PEO35P
229900970211     C     PEO35P        ADD       PEF35P        PETOTP
230000000000      *
230100000000      * ?  V  O  L  U  M E  ?
230200940428     C                   Z-ADD     V2P($V)       VOTOTP
230300000000      *
230400000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
230500940428     C                   Z-ADD     S2P($V)       SPTOTP
230600970211     C                   Z-ADD     S2P($V)       SPTOTA
230700970211     C                   Z-ADD     S2M($V)       SPCONP
230800970211     C     SPTOTA        SUB       S2M($V)       SPSPAP
230900000000      *
231000000000     C                   EXSR      SCRIVI
231100000000      *
231200000000     C                   ENDDO
231300940428     C*
231400940428     C                   EXSR      AZZTRA
231500940502      *
231600950118     C  N38              WRITE     LSC2T2
231700940506     C                   ENDIF
231800940506     C                   SETOFF                                       0302
231900000000      *
232000000000     C                   ENDSR
232100000000      **************************************************************************
232200090916      * Scrittura del record sul file di trasmissione fisat00F
232300000000      **************************************************************************
232400000000     C     WRISAT        BEGSR
232500000000      *
232600090916     C                   CLEAR                   fisat000
232700000000     C                   Z-ADD     SIMFEL        SATFLE
232800000000     C                   Z-ADD     $FILPR        SATLNP
232900000000     C                   Z-ADD     *ZERO         SATTFA
233000950210     C                   Z-ADD     DS1ADA        SATDRE
233100000000     C                   MOVE      *BLANK        SATTRE
233200000000      *
233300000000      * COLLI: da partire
233400970210     C                   Z-ADD     CPA(1)        SATCPE
233500970210     C                   Z-ADD     CPA(2)        SATCPD
233600970210     C                   Z-ADD     CPA(3)        SATCPC
233700090916     c                   Z-ADD     CPA(4)        SATCPh
233800090916     c                   Z-ADD     CPA(5)        SATCpx
233900970207      * COLLI: a terra da partire
234000970207     C                   Z-ADD     CBP(1)        SATCDE
234100970207     C                   Z-ADD     CBP(2)        SATCDD
234200970207     C                   Z-ADD     CBP(3)        SATCDC
234300090916     C                   Z-ADD     CBP(4)        SATCDh
234400090916     C                   Z-ADD     CBP(5)        SATCDx
234500970207      * COLLI: a terra non da partire
234600970207     C                   Z-ADD     CBR(1)        SATCNE
234700970207     C                   Z-ADD     CBR(2)        SATCND
234800970207     C                   Z-ADD     CBR(3)        SATCNC
234900090916     C                   Z-ADD     CBR(4)        SATCNh
235000090916     C                   Z-ADD     CBR(5)        SATCNx
235100970207      * PESO: da partire
235200970210     C                   Z-ADD     PPA(1)        SATPPE
235300970210     C                   Z-ADD     PPA(2)        SATPPD
235400970210     C                   Z-ADD     PPA(3)        SATPPC
235500090916     C                   Z-ADD     PPA(4)        SATPPh
235600090916     C                   Z-ADD     PPA(5)        SATPPx
235700970207      * PESO: a terra da partire fino a 35 quintali
235800970207     C                   Z-ADD     PFI(1)        SATFDE
235900970207     C                   Z-ADD     PFI(2)        SATFDD
236000970207     C                   Z-ADD     PFI(3)        SATFDC
236100090916     C                   Z-ADD     PFI(4)        SATFDh
236200090916     C                   Z-ADD     PFI(5)        SATFDx
236300970207      * PESO: a terra non da partire fino a 35 q.
236400970207     C                   Z-ADD     PFN(1)        SATFNE
236500970207     C                   Z-ADD     PFN(2)        SATFND
236600970207     C                   Z-ADD     PFN(3)        SATFNC
236700090916     C                   Z-ADD     PFN(4)        SATFNh
236800090916     C                   Z-ADD     PFN(5)        SATFNx
236900970207     C* PESO: a terra da partire oltre 35 quintali
237000970207     C                   Z-ADD     PBP(1)        SATODE
237100970207     C                   Z-ADD     PBP(2)        SATODD
237200970207     C                   Z-ADD     PBP(3)        SATODC
237300090916     C                   Z-ADD     PBP(4)        SATODh
237400090916     C                   Z-ADD     PBP(5)        SATODx
237500970207      * PESO: a terra non da partire oltre 35 q.
237600970207     C                   Z-ADD     PBR(1)        SATONE
237700970207     C                   Z-ADD     PBR(2)        SATOND
237800970207     C                   Z-ADD     PBR(3)        SATONC
237900090916     c                   Z-ADD     PBR(4)        SATONh
238000090916     C                   Z-ADD     PBR(5)        SATONx
238100000000      * VOLUME: da partire
238200970210     C                   Z-ADD     VPA(1)        SATVPE
238300970210     C                   Z-ADD     VPA(2)        SATVPD
238400970210     C                   Z-ADD     VPA(3)        SATVPC
238500090916     C                   Z-ADD     VPA(4)        SATVPh
238600090916     C                   Z-ADD     VPA(5)        SATVPx
238700970207      * VOLUME a terra da partire
238800970207     C                   Z-ADD     VBP(1)        SATVDE
238900970207     C                   Z-ADD     VBP(2)        SATVDD
239000970207     C                   Z-ADD     VBP(3)        SATVDC
239100090916     C                   Z-ADD     VBP(4)        SATVDh
239200090916     C                   Z-ADD     VBP(5)        SATVDx
239300000331      * VOLUME rimasto a magazzino
239400000331      * (totale dei colli a terra da partire, da non partire e colli
239500000331      * di bolle in sospensione)
239600970207     C                   Z-ADD     VBR(1)        SATVNE
239700970207     C                   Z-ADD     VBR(2)        SATVND
239800970207     C                   Z-ADD     VBR(3)        SATVNC
239900090916     C                   Z-ADD     VBR(4)        SATVNh
240000090916     C                   Z-ADD     VBR(5)        SATVNx
240100940429      * SPEDIZIONI: da partire
240200970210     C                   Z-ADD     SPA(1)        SATSPE
240300970210     C                   Z-ADD     SPA(2)        SATSPD
240400970210     C                   Z-ADD     SPA(3)        SATSPC
240500090916     C                   Z-ADD     SPA(4)        SATSPh
240600090916     C                   Z-ADD     SPA(5)        SATSPx
240700970207      * SPEDIZIONI a terra da partire
240800970207     C                   Z-ADD     SBP(1)        SATSDE
240900970207     C                   Z-ADD     SBP(2)        SATSDD
241000970207     C                   Z-ADD     SBP(3)        SATSDC
241100090916     C                   Z-ADD     SBP(4)        SATSDh
241200090916     C                   Z-ADD     SBP(5)        SATSDx
241300090916      * SPEDIZIONI a terra non da partire
241400970207     C                   Z-ADD     SBR(1)        SATSNE
241500970207     C                   Z-ADD     SBR(2)        SATSND
241600970207     C                   Z-ADD     SBR(3)        SATSNC
241700090916     C                   Z-ADD     SBR(4)        SATSNh
241800090916     C                   Z-ADD     SBR(5)        SATSNx
241900970305      * SPEDIZIONI a terra da partire fuori affidabilità
242000970305     C                   Z-ADD     FAF(1)        SATSFE
242100970305     C                   Z-ADD     FAF(2)        SATSFD
242200970305     C                   Z-ADD     FAF(3)        SATSFC
242300090916     C                   Z-ADD     FAF(4)        SATSFh
242400090916     C                   Z-ADD     FAF(5)        SATSFx
242500000000      * SPEDIZIONI: complete
242600000000     C                   Z-ADD     SCM(1)        SATSCE
242700970207     C                   Z-ADD     SCM(2)        SATSCD
242800970207     C                   Z-ADD     SCM(3)        SATSCC
242900090916     C                   Z-ADD     SCM(4)        SATSCh
243000090916     C                   Z-ADD     SCM(5)        SATSCx
243100000000      *
243200970318     C* ASTERISCO LE SEGUENTI IF PERCHE' IL RECORD LO DEVO SEMPRE
243300970318     C* SCRIVERE PER EVITARE CHE NELL'ELABORAZIONE DEL REPORT DI
243400970318     C* SEDE, SE PER UN P.O. IL DA PARTIRE è = 0, VENGA ERRONEAMENTE
243500970318     C* SEGNALATO "DATI NON PERVENUTI".
243600970318     C*          SATSDE    IFGT *ZERO
243700970318     C*          SATSDD    ORGT *ZERO
243800970318     C*          SATSDC    ORGT *ZERO
243900970318     C*          SATSNE    ORGT *ZERO
244000970318     C*          SATSND    ORGT *ZERO
244100970318     C*          SATSNC    ORGT *ZERO
244200970318     C*          SATSPE    ORGT *ZEROS
244300970318     C*          SATSPD    ORGT *ZEROS
244400970318     C*          SATSPC    ORGT *ZEROS
244500940323      *
244600940428     C                   MOVE      $FTR          SATFTR
244700970207     C                   MOVE      $DTR          SATDTR
244800940323      *
244900090916     C                   WRITE     fisat000
245000970318     C*
245100970318     C* LE IF SOPRA ASTERISCATE LE MANTENGO PERò PER IMPOSTARE $PRIM4
245200970318     C*
245300970318     C     SATSDE        IFGT      *ZERO
245400970318     C     SATSDD        ORGT      *ZERO
245500970318     C     SATSDC        ORGT      *ZERO
245600090916     C     SATSDh        ORGT      *ZERO
245700090916     C     SATSDx        ORGT      *ZERO
245800970318     C     SATSNE        ORGT      *ZERO
245900970318     C     SATSND        ORGT      *ZERO
246000970318     C     SATSNC        ORGT      *ZERO
246100090916     C     SATSNh        ORGT      *ZERO
246200090916     C     SATSNx        ORGT      *ZERO
246300970318     C     SATSPE        ORGT      *ZEROS
246400970318     C     SATSPD        ORGT      *ZEROS
246500970318     C     SATSPC        ORGT      *ZEROS
246600090916     C     SATSPh        ORGT      *ZEROS
246700090916     C     SATSPx        ORGT      *ZEROS
246800000405     C     SATVNE        ORGT      *ZEROS
246900000405     C     SATVND        ORGT      *ZEROS
247000000405     C     SATVNC        ORGT      *ZEROS
247100090916     C     SATVNh        ORGT      *ZEROS
247200090916     C     SATVNx        ORGT      *ZEROS
247300940429     C* PER LA STAMPA DEI TOTALI DI AREA
247400940429     C                   Z-ADD     1             $PRIM4
247500970318     C                   ENDIF
247600000000      *
247700000000     C                   ENDSR
247800000000      **************************************************************************
247900090916      * Scrittura del record c/servizio sul file di trasmissione fisat00F
248000000000      **************************************************************************
248100000000     C     WR1SAT        BEGSR
248200000000      *
248300090916     C                   CLEAR                   fisat000
248400000000     C                   Z-ADD     SIMFEL        SATFLE
248500000000     C                   Z-ADD     $FILPR        SATLNP
248600000000     C                   Z-ADD     *ZERO         SATTFA
248700950210     C                   Z-ADD     DS1ADA        SATDRE
248800000000     C                   MOVE      'C'           SATTRE
248900000000      *
249000000000      * COLLI: da partire
249100970210     C                   Z-ADD     C4A(1)        SATCPE
249200970210     C                   Z-ADD     C4A(2)        SATCPD
249300970210     C                   Z-ADD     C4A(3)        SATCPC
249400090916     C                   Z-ADD     C4A(4)        SATCPh
249500090916     C                   Z-ADD     C4A(5)        SATCPx
249600970207      * COLLI: a terra da partire
249700970207     C                   Z-ADD     C4P(1)        SATCDE
249800970207     C                   Z-ADD     C4P(2)        SATCDD
249900970207     C                   Z-ADD     C4P(3)        SATCDC
250000090916     C                   Z-ADD     C4P(4)        SATCDh
250100090916     C                   Z-ADD     C4P(5)        SATCDx
250200970207      * COLLI: a terra non da partire
250300970207     C                   Z-ADD     C4R(1)        SATCNE
250400970207     C                   Z-ADD     C4R(2)        SATCND
250500970207     C                   Z-ADD     C4R(3)        SATCNC
250600090916     C                   Z-ADD     C4R(4)        SATCNh
250700090916     C                   Z-ADD     C4R(5)        SATCNx
250800970207      * PESO: da partire
250900970210     C                   Z-ADD     P4A(1)        SATPPE
251000970210     C                   Z-ADD     P4A(2)        SATPPD
251100970210     C                   Z-ADD     P4A(3)        SATPPC
251200090916     C                   Z-ADD     P4A(4)        SATPPh
251300090916     C                   Z-ADD     P4A(5)        SATPPx
251400970207      * PESO: a terra da partire fino a 35 quintali
251500970207     C                   Z-ADD     P4I(1)        SATFDE
251600970207     C                   Z-ADD     P4I(2)        SATFDD
251700970207     C                   Z-ADD     P4I(3)        SATFDC
251800090916     C                   Z-ADD     P4I(4)        SATFDh
251900090916     C                   Z-ADD     P4I(5)        SATFDx
252000970207      * PESO: a terra non da partire fino a 35 quintali
252100970207     C                   Z-ADD     P4N(1)        SATFNE
252200970207     C                   Z-ADD     P4N(2)        SATFND
252300970207     C                   Z-ADD     P4N(3)        SATFNC
252400090916     C                   Z-ADD     P4N(4)        SATFNh
252500090916     C                   Z-ADD     P4N(5)        SATFNx
252600970207      * PESO: a terra da partire oltre 35 quintali
252700970207     C                   Z-ADD     P4P(1)        SATODE
252800970207     C                   Z-ADD     P4P(2)        SATODD
252900970207     C                   Z-ADD     P4P(3)        SATODC
253000090916     C                   Z-ADD     P4P(4)        SATODh
253100090916     C                   Z-ADD     P4P(5)        SATODx
253200970207      * PESO: a terra non da partire oltre 35 quintali
253300970207     C                   Z-ADD     P4R(1)        SATONE
253400970207     C                   Z-ADD     P4R(2)        SATOND
253500970207     C                   Z-ADD     P4R(3)        SATONC
253600090916     C                   Z-ADD     P4R(4)        SATONh
253700090916     C                   Z-ADD     P4R(5)        SATONx
253800000000      * VOLUME: da partire
253900970210     C                   Z-ADD     V4A(1)        SATVPE
254000970210     C                   Z-ADD     V4A(2)        SATVPD
254100970210     C                   Z-ADD     V4A(3)        SATVPC
254200090916     C                   Z-ADD     V4A(4)        SATVPh
254300090916     C                   Z-ADD     V4A(5)        SATVPx
254400970207      * VOLUME: a terra da partire
254500970207     C                   Z-ADD     V4P(1)        SATVDE
254600970207     C                   Z-ADD     V4P(2)        SATVDD
254700970207     C                   Z-ADD     V4P(3)        SATVDC
254800090916     C                   Z-ADD     V4P(4)        SATVDh
254900090916     C                   Z-ADD     V4P(5)        SATVDx
255000000331      * VOLUME: totale rimasto a magazzino
255100000331      * (a terra da partire, da non partire, in sospensione)
255200970207     C                   Z-ADD     V4R(1)        SATVNE
255300970207     C                   Z-ADD     V4R(2)        SATVND
255400970207     C                   Z-ADD     V4R(3)        SATVNC
255500090916     C                   Z-ADD     V4R(4)        SATVNh
255600090916     C                   Z-ADD     V4R(5)        SATVNx
255700970207      * SPEDIZIONI: da partire
255800970210     C                   Z-ADD     S4A(1)        SATSPE
255900970210     C                   Z-ADD     S4A(2)        SATSPD
256000970210     C                   Z-ADD     S4A(3)        SATSPC
256100090916     C                   Z-ADD     S4A(4)        SATSPh
256200090916     C                   Z-ADD     S4A(5)        SATSPx
256300970207      * SPEDIZIONI: a terra da partire
256400970207     C                   Z-ADD     S4P(1)        SATSDE
256500970207     C                   Z-ADD     S4P(2)        SATSDD
256600970207     C                   Z-ADD     S4P(3)        SATSDC
256700090916     C                   Z-ADD     S4P(4)        SATSDh
256800090916     C                   Z-ADD     S4P(5)        SATSDx
256900970207      * SPEDIZIONI: a terra non da partire
257000970207     C                   Z-ADD     S4R(1)        SATSNE
257100970207     C                   Z-ADD     S4R(2)        SATSND
257200970207     C                   Z-ADD     S4R(3)        SATSNC
257300090916     C                   Z-ADD     S4R(4)        SATSNh
257400090916     C                   Z-ADD     S4R(5)        SATSNx
257500970305      * SPEDIZIONI: a terra da partire fuori affidabilità
257600970305     C                   Z-ADD     F4F(1)        SATSFE
257700970305     C                   Z-ADD     F4F(2)        SATSFD
257800970305     C                   Z-ADD     F4F(3)        SATSFC
257900090916     C                   Z-ADD     F4F(4)        SATSFh
258000090916     C                   Z-ADD     F4F(5)        SATSFx
258100000000      * SPEDIZIONI: complete
258200000000     C                   Z-ADD     S4M(1)        SATSCE
258300970207     C                   Z-ADD     S4M(2)        SATSCD
258400970207     C                   Z-ADD     S4M(3)        SATSCC
258500090916     C                   Z-ADD     S4M(4)        SATSCh
258600090916     C                   Z-ADD     S4M(5)        SATSCx
258700000000      *
258800970318     C     SATSDE        IFGT      *ZERO
258900970318     C     SATSDD        ORGT      *ZERO
259000970318     C     SATSDC        ORGT      *ZERO
259100090916     C     SATSDh        ORGT      *ZERO
259200090916     C     SATSDx        ORGT      *ZERO
259300970318     C     SATSNE        ORGT      *ZERO
259400970318     C     SATSND        ORGT      *ZERO
259500970318     C     SATSNC        ORGT      *ZERO
259600090916     C     SATSNh        ORGT      *ZERO
259700090916     C     SATSNx        ORGT      *ZERO
259800970318     C     SATSPE        ORGT      *ZERO
259900970318     C     SATSPD        ORGT      *ZERO
260000970318     C     SATSPC        ORGT      *ZERO
260100090916     C     SATSPh        ORGT      *ZERO
260200090916     C     SATSPx        ORGT      *ZERO
260300000405     C     SATVNE        ORGT      *ZEROS
260400000405     C     SATVND        ORGT      *ZEROS
260500000405     C     SATVNC        ORGT      *ZEROS
260600090916     C     SATVNh        ORGT      *ZEROS
260700090916     C     SATVNx        ORGT      *ZEROS
260800940323      *
260900940428     C                   MOVE      $FTR          SATFTR
261000970207     C                   MOVE      $DTR          SATDTR
261100940323      *
261200090916     C                   WRITE     fisat000
261300940429     C* PER LA STAMPA DEI C/S
261400940429     C                   Z-ADD     1             $PRIM2
261500940429     C* PER LA STAMPA DEI TOTALI DI AREA
261600940429     C                   Z-ADD     1             $PRIM4
261700000000     C                   ENDIF
261800000000      *
261900000000      * Aggiorno le schiere per i totali c/servizio per area
262000090916     C                   DO        5             $U                1 0
262100970210     C* A TERRA
262200000000     C                   ADD       C4P($U)       C1P($U)
262300000000     C                   ADD       C4R($U)       C1R($U)
262400000000     C                   ADD       P4I($U)       P1I($U)
262500970207     C                   ADD       P4N($U)       P1N($U)
262600000000     C                   ADD       P4P($U)       P1P($U)
262700000000     C                   ADD       P4R($U)       P1R($U)
262800000000     C                   ADD       V4P($U)       V1P($U)
262900000331     C                   ADD       V4R($U)       V1R($U)
263000000000     C                   ADD       S4P($U)       S1P($U)
263100000000     C                   ADD       S4R($U)       S1R($U)
263200000000     C                   ADD       S4M($U)       S1M($U)
263300970305     C                   ADD       F4F($U)       F1F($U)
263400970210     C* DA PARTIRE
263500970210     C                   ADD       C4A($U)       C1A($U)
263600970210     C                   ADD       P4A($U)       P1A($U)
263700970210     C                   ADD       V4A($U)       V1A($U)
263800970210     C                   ADD       S4A($U)       S1A($U)
263900000000     C                   ENDDO
264000000000      *
264100000000     C                   EXSR      AZZSE4
264200000000      *
264300000000     C                   ENDSR
264400000000      **************************************************************************
264500090916      * Scrittura del record transito sul file di trasmissione fisat00F
264600000000      **************************************************************************
264700000000     C     WR2SAT        BEGSR
264800000000      *
264900090916     C                   CLEAR                   fisat000
265000000000     C                   Z-ADD     SIMFEL        SATFLE
265100940502     C                   Z-ADD     SIMFEL        SATLNP
265200000000     C                   Z-ADD     *ZERO         SATTFA
265300950210     C                   Z-ADD     DS1ADA        SATDRE
265400000000     C                   MOVE      'T'           SATTRE
265500000000      *
265600000000      * COLLI: da partire
265700970211     C                   Z-ADD     0             SATCPE
265800970211     C                   Z-ADD     0             SATCPD
265900970211     C                   Z-ADD     0             SATCPC
266000090916     C                   Z-ADD     0             SATCPh
266100090916     C                   Z-ADD     0             SATCPx
266200970211      * COLLI:
266300970211     C                   Z-ADD     C2P(1)        SATCDE
266400970211     C                   Z-ADD     C2P(2)        SATCDD
266500970211     C                   Z-ADD     C2P(3)        SATCDC
266600090916     C                   Z-ADD     C2P(4)        SATCDh
266700090916     C                   Z-ADD     C2P(5)        SATCDx
266800970211      * PESO: da partire
266900970211     C                   Z-ADD     0             SATPPE
267000970211     C                   Z-ADD     0             SATPPD
267100970211     C                   Z-ADD     0             SATPPC
267200090916     C                   Z-ADD     0             SATPPh
267300090916     C                   Z-ADD     0             SATPPx
267400000000      * PESO: fino a 35 quintali
267500970211     C                   Z-ADD     P2I(1)        SATFDE
267600970211     C                   Z-ADD     P2I(2)        SATFDD
267700970211     C                   Z-ADD     P2I(3)        SATFDC
267800090916     C                   Z-ADD     P2I(4)        SATFDh
267900090916     C                   Z-ADD     P2I(5)        SATFDx
268000940428      * PESO: (non esiste il bollettato nel periodo o nel precedente)
268100970211     C                   Z-ADD     P2P(1)        SATODE
268200970211     C                   Z-ADD     P2P(2)        SATODD
268300970211     C                   Z-ADD     P2P(3)        SATODC
268400090916     C                   Z-ADD     P2P(4)        SATODh
268500090916     C                   Z-ADD     P2P(5)        SATODx
268600000000      * VOLUME: da partire
268700970211     C                   Z-ADD     0             SATVPE
268800970211     C                   Z-ADD     0             SATVPD
268900970211     C                   Z-ADD     0             SATVPC
269000090916     C                   Z-ADD     0             SATVPh
269100090916     C                   Z-ADD     0             SATVPx
269200970211      * VOLUME
269300970211     C                   Z-ADD     V2P(1)        SATVDE
269400970211     C                   Z-ADD     V2P(2)        SATVDD
269500970211     C                   Z-ADD     V2P(3)        SATVDC
269600090916     C                   Z-ADD     V2P(4)        SATVDh
269700090916     C                   Z-ADD     V2P(5)        SATVDx
269800000000      * SPEDIZIONI: da partire
269900970211     C                   Z-ADD     0             SATSPE
270000970211     C                   Z-ADD     0             SATSPD
270100970211     C                   Z-ADD     0             SATSPC
270200090916     C                   Z-ADD     0             SATSPh
270300090916     C                   Z-ADD     0             SATSPx
270400970211      * SPEDIZIONI
270500970211     C                   Z-ADD     S2P(1)        SATSDE
270600970211     C                   Z-ADD     S2P(2)        SATSDD
270700970211     C                   Z-ADD     S2P(3)        SATSDC
270800090916     C                   Z-ADD     S2P(4)        SATSDh
270900090916     C                   Z-ADD     S2P(5)        SATSDx
271000000000      * SPEDIZIONI: complete
271100940509     C                   Z-ADD     S2M(1)        SATSCE
271200970211     C                   Z-ADD     S2M(2)        SATSCD
271300970211     C                   Z-ADD     S2M(3)        SATSCC
271400090916     C                   Z-ADD     S2M(4)        SATSCh
271500090916     C                   Z-ADD     S2M(5)        SATSCx
271600000000      *
271700970212     C     SATSDE        IFGT      *ZERO
271800970212     C     SATSDD        ORGT      *ZERO
271900970212     C     SATSDC        ORGT      *ZERO
272000090916     C     SATSDH        ORGT      *ZERO
272100090916     C     SATSDX        ORGT      *ZERO
272200940428     C                   MOVE      $FTR          SATFTR
272300970207     C                   MOVE      $DTR          SATDTR
272400940323      *
272500090916     C                   WRITE     fisat000
272600940429     C* PER LA STAMPA DEI TOTALI DI AREA
272700940429     C                   Z-ADD     1             $PRIM4
272800940505     C                   Z-ADD     1             $PRIM3
272900000000     C                   ENDIF
273000000000      *
273100000000     C                   ENDSR
273200940428      **************************************************************************
273300940428      * Stampo i totali generali
273400940428      **************************************************************************
273500940428     C     TOTGEN        BEGSR
273600940428      *
273700940428      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
273800940428      *
273900940506      * stampo i totali generali finali solo se ho stampato i transiti
274000950628    1C     *IN02         IFEQ      *ON
274100940506     C     $PRIM3        ANDEQ     1
274200940506     C* I TOTALI DI AREA SEMPRE
274300940506     C     *IN02         OREQ      *OFF
274400940506     C*
274500940428     C                   SETON                                        40
274600940428     C                   SETOFF                                       41
274700970211     C  N02              SETON                                        45
274800970211     C   02              SETOFF                                       45
274900940428     C                   Z-ADD     *ZERO         FILIAP
275000940428     C                   MOVE      *BLANKS       ORGDEP
275100940428     C   02'TOT.GENE'    CAT       'RALE':0      ORGDEP
275200940428     C  N02              MOVEL     'TOTALE'      ORGDEP
275300940428     C*
275400950118     C                   WRITE     LSC2T3
275500940428      *
275600940428      * ?  C  O   L   L  I  ?
275700970211     C                   XFOOT     C3P           COTOTP
275800970211     C                   XFOOT     C3A           $CDA
275900940428     C                   EXSR      PERCOL
276000940428      *
276100940428      * ?  P  E  S  O  ?
276200970211     C                   XFOOT     P3I           PEF35P
276300970211     C                   XFOOT     P3P           PEO35P
276400970211     C     PEF35P        ADD       PEO35P        PETOTP
276500970228     C                   XFOOT     P3A           $PDA
276600970211     C                   EXSR      PERPES
276700970211      *
276800970211      * ?  V  O  L  U  M E  ?
276900970211     C                   XFOOT     V3P           VOTOTP
277000970211     C                   XFOOT     V3A           $VDA
277100940428     C                   EXSR      PERVOL
277200940428      *
277300940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
277400970211     C                   XFOOT     S3A           $SDA
277500970305     C                   XFOOT     F3F           SPFAFF
277600940502     C                   EXSR      PERSPE
277700970305     C                   XFOOT     S3P           SPTOTP
277800970305     C                   XFOOT     S3M           SPCONP
277900970211     C                   XFOOT     S3R           $COMO1
278000970211     C     SPTOTP        ADD       $COMO1        SPTOTA
278100970211     C     SPTOTA        SUB       SPCONP        SPSPAP
278200000407     C*
278300000407      * ?VOLUME A MAGAZZINO ?
278400000407     C                   XFOOT     V3R           VOTOTM
278500940428      *
278600940428     C                   EXSR      SCRIVI
278700940509     C                   ADD       1             $RIGHE
278800940428      *
278900090916      * ------------------ ESPR./DIST./CORR./1030 ---------------------
279000940428      *
279100970211     C                   SETOFF                                       40
279200090916    2C                   DO        CICLO         $V
279300940428     C                   MOVE      ENS($V)       TIPOP
279400940428      *
279500940428      * ?  C  O   L   L  I  ?
279600970211     C                   Z-ADD     C3P($V)       COTOTP
279700970211     C                   Z-ADD     C3A($V)       $CDA
279800940428     C                   EXSR      PERCOL
279900940428      *
280000940428      * ?  P  E  S  O  ?
280100940428     C                   Z-ADD     P3I($V)       PEF35P
280200970211     C                   Z-ADD     P3P($V)       PEO35P
280300970211     C     PEF35P        ADD       PEO35P        PETOTP
280400970211     C                   Z-ADD     P3A($V)       $PDA
280500940428     C                   EXSR      PERPES
280600940428      *
280700940428      * ?  V  O  L  U  M E  ?
280800970211     C                   Z-ADD     V3P($V)       VOTOTP
280900970211     C                   Z-ADD     V3A($V)       $VDA
281000940428     C                   EXSR      PERVOL
281100940428      *
281200940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
281300970211     C                   Z-ADD     S3A($V)       $SDA
281400970305     C                   Z-ADD     F3F($V)       SPFAFF
281500940502     C                   EXSR      PERSPE
281600970305     C                   Z-ADD     S3P($V)       SPTOTP
281700970305     C                   Z-ADD     S3M($V)       SPCONP
281800970211     C     S3P($V)       ADD       S3R($V)       SPTOTA
281900970211     C     SPTOTA        SUB       S3M($V)       SPSPAP
282000000406      * ?VOLUME A MAGAZZINO ?
282100000406     C                   Z-ADD     V3R($V)       VOTOTM
282200940428      *
282300940428     C                   EXSR      SCRIVI
282400940428      *
282500950628    2C                   ENDDO
282600940428      *
282700950118     C  N02              WRITE     LSC2T2
282800950628    1C                   ENDIF
282900940429     C*
283000950118     C   02              WRITE     LSC2T3
283100940428     C*
283200940428     C   02              EXSR      AZZTOT
283300940428      *
283400940428     C                   ENDSR
283500000000      **************************************************************************
283600000000      * Elaborazione transiti
283700940505      * Non posso leggere per data uscita dal transito perche' spesso
283800940505      * capita che ci siano bolle con data entrata un giorno e data
283900940505      * uscita il giorno prima: per questo elaboro tutte le bolle
284000950210      * che hanno la data di entrata al transito > 0 e <=a DS1AAL
284100000000      **************************************************************************
284200000000     C     ELATRA        BEGSR
284300000000      *
284400940505     C                   Z-ADD     0             $PRIM1
284500940428     C                   MOVE      *BLANK        $BLTPR
284600940429     C                   MOVE      *BLANK        $CBLPR
284700940428     C                   MOVE      'T'           $FLAG
284800000000      *
284900000000      * Leggo il file delle bolle in transito
285000940505     C                   Z-ADD     1             $DFV
285100020625     C     kbtt39        SETLL     FNBTT39L
285200020625     C     simfel        READe     FNBTT39L                               33
285300940428     C*
285400940428    2C     *IN33         DOWEQ     *OFF
285500950210     C     BTTDET        ANDLE     DS1AAL
285600000000      *
285700940505     C                   EXSR      ESCLU1
285800940428      *
285900940505    3C     $OK           IFEQ      'S'
286000940505     C                   Z-ADD     1             $PRIM1
286100000000      *
286200000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
286300000000      * poiche' lo dovro aggiornare
286400090916     C                   CLEAR                   fisat000
286500000000     C                   Z-ADD     SIMFEL        SATFLE
286600950210     C                   Z-ADD     DS1ADA        SATDRE
286700940502     C                   Z-ADD     SIMFEL        SATLNP
286800950118     C                   Z-ADD     BTPTFA        SATTFA
286900000000     C                   MOVE      'T'           SATTRE
287000090916     C     FNSA1K        CHAIN     fisat02L                           37
287100940428      *
287200021217      * se è la prima volta che leggo la bolla carico i dati: VOL e PKG
287300021217    4C     $BOLLA        IFEQ      '1'
287400021217     C                   EXSR      DATBOT
287500021217    4C                   ENDIF
287600021217      *
287700021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
287800030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
287900030213      * Peso medio
288000030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
288100030213if  1c***                if        Wsipes = 'S'
288200030213     c***                z-add     BTTpuc        $Peso
288300030213   x1c***                else
288400021220     c                   z-add     Wpesmed       $Peso
288500030213e   1c***                endif
288600021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
288700030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
288800030213      * Volume medio
288900030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
289000030213if  1c***                if        Wsivol = 'S'
289100030213     c***                z-add     BTTvuc        $Volum
289200030213   x1c***                else
289300021220     c                   z-add     Wvolmed       $Volum
289400030213e   1c***                endif
289500000000      *
289600970211      * Verifico il tipo spedizione ESPR./DIST./CORR.
289700090916     C                   Z-ADD     5             $T
289800090916     c                   select
289900090916    4C     BTPTSP        wheneq    'E'
290000000000     C                   Z-ADD     1             $T
290100090916    4C     BTPTSP        wheneq    'D'
290200940323     C                   Z-ADD     2             $T
290300090916    4C     BTPTSP        wheneq    'C'
290400090916     C                   Z-ADD     3             $T
290500090916    4C     BTPTSP        wheneq    'H'
290600090916     C                   Z-ADD     4             $T
290700090916    4C                   ENDSL
290800000000      *
290900000000      * Ho due controlli da fare per sapere se il collo e' a terra:
291000940428      *    1)  BTTDUT = 0
291100940428      *    2)  BTTDUT > Data spedizione
291200000000     C                   SETOFF                                       36
291300940428     C     BTTDUT        IFEQ      0
291400950210     C     BTTDUT        ORGT      DS1AAL
291500000000     C                   SETON                                        36
291600000000     C                   ENDIF
291700940506      *
291800000000      * Sommo solo se il collo risulta a terra
291900940428    4C     *IN36         IFEQ      *ON
292000940506      *
292100940506      * ?  CALCOLI  SUI  COLLI  ?
292200940506      *
292300940509     C                   ADD       1             C2P($T)
292400940428     C                   ADD       1             C0P($T)
292500000000      *
292600000000      * ?  CALCOLI  SUL  PESO  ?
292700000000      *
292800000000      * Verifico il peso di 35 quintali
292900021217    5***C     BTPPKB        IFLE      3500
293000021217    5C     Wpeso         IFLE      3500
293100940509     C                   ADD       $PESO         P2I($T)
293200000000     C                   ADD       $PESO         P0I($T)
293300970211     C                   ELSE
293400970211      * Oltre 35 quintali:                                             (35ON)
293500970211     C                   ADD       $PESO         P2P($T)
293600970211     C                   ADD       $PESO         P0P($T)
293700940428    5C                   ENDIF
293800000000      *
293900000000      *
294000000000      * ?  CALCOLI  SUL  VOLUME  ?
294100000000      *
294200021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
294300021217     **5C     BTTVUC        IFEQ      *ZERO
294400021217     *** *
294500021217     ***C     BTPVLB        DIV(H)    BTPNCL        $VOLUM
294600021217     *** *
294700021217     **5C                   ELSE
294800021217     ***C                   Z-ADD     BTTVUC        $VOLUM
294900021217     **5C                   ENDIF
295000000000      *
295100940509     C                   ADD       $VOLUM        V2P($T)
295200940428     C                   ADD       $VOLUM        V0P($T)
295300000000      *
295400000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
295500000000      *
295600000000      * Visto che le spedizioni non riesco a calcolarle ora, mi limito a tenere
295700000000      * aggiornato il file di work con le bolle
295800950118     C                   Z-ADD     BTPLNP        WRKLNP
295900000000     C                   MOVE      *BLANK        WRKCSE
296000970409     C     FNWR2K        CHAIN     FNWRK00F                           99
296100940428    5C     *IN99         IFEQ      *OFF
296200000000     C                   ADD       1             WRKNC1
296300000000     C                   EXCEPT    AGGIO
296400940428   X5C                   ELSE
296500950118     C                   MOVEL     'T'           WTRANS
296600940503     C                   EXSR      WRIWRK
296700940428    5C                   ENDIF
296800940428      *
296900040921     C                   MOVE      $FTR          SATFTR
297000040921     C                   MOVE      $DTR          SATDTR
297100940428    5C     *IN37         IFEQ      *ON
297200040921     C***                MOVE      $FTR          SATFTR
297300040921     C***                MOVE      $DTR          SATDTR
297400090916     C                   WRITE     fisat000
297500940428   X5C                   ELSE
297600090916     C                   UPDATE    fisat000
297700940428    5C                   ENDIF
297800000000      *
297900940428    4C                   ENDIF
298000940428    3C                   ENDIF
298100000000      *
298200020625     C     simfel        READE     FNBTT39L                               33
298300940428    2C                   ENDDO
298400940506     C*
298500000000     C     $PRIM1        IFNE      *ZERO
298600940506     C                   EXSR      CALSPE
298700000000     C                   EXSR      WR2SAT
298800000000     C                   ENDIF
298900000000      *
299000000000     C                   ENDSR
299100940503      **************************************************************************
299200000411      * elimino i terminal che non hanno VOLUMI TOT A MAG>0
299300940503      **************************************************************************
299400940503     C     ELATER        BEGSR
299500950210     C                   Z-ADD     DS1FEL        $AREA
299600940503     C                   CLEAR                   $TFA
299700940503     C                   SETOFF                                       33
299800940503     C*
299900940503     C* ELABORO TERMINAL PER TERMINAL
300000940503     C                   EXSR      IMPTFA
300100940503     C*
300200940503    2C     *IN33         DOWEQ     *OFF
300300940503     C                   Z-ADD     *ZERO         $PRIM1
300400940503      *
300500090916     C     TASATK        READE(N)  fisat03L                               33
300600940503     C                   MOVEL     SATTRE        $TRE
300700940503     C*
300800940503    3C     *IN33         DOWEQ     *OFF
300900940503     C*
301000940503    4C     $TRE          IFNE      SATTRE
301100940503     C     $PRIM1        ANDEQ     0
301200090916     C     TRSATK        SETLL     fisat03L
301300090916     C     TRSATK        READE     fisat03L                               33
301400940503     C*
301500940503    5C     *IN33         DOWEQ     *OFF
301600090916     C                   DELETE    fisat003
301700090916     C     TRSATK        READE     fisat03L                               33
301800940503    5C                   ENDDO
301900940503     C                   MOVEL     SATTRE        $TRE
302000940503     C                   CLEAR                   $PRIM1
302100940503    4C                   ENDIF
302200940503     C*
302300000411     C* VEDO SE C'E' ALMENO UN VOLUME A MAGAZZINO
302400000411    4C     SATVNE        IFGT      0
302500000411     C     SATVND        ORGT      0
302600000411     C     SATVNC        ORGT      0
302700940503     C                   Z-ADD     1             $PRIM1
302800940503    4C                   ENDIF
302900090916     C     TASATK        READE(N)  fisat03L                               33
303000940503    3C                   ENDDO
303100940503     C* ELABORO L'ULTIMO
303200940503    3C     $PRIM1        IFEQ      0
303300090916     C     TRSATK        SETLL     fisat03L
303400090916     C     TRSATK        READE     fisat03L                               33
303500970211     C*
303600940503    4C     *IN33         DOWEQ     *OFF
303700090916     C                   DELETE    fisat003
303800090916     C     TRSATK        READE     fisat03L                               33
303900940503    4C                   ENDDO
304000940503    3C                   ENDIF
304100940503     C*
304200940503     C                   EXSR      IMPTFA
304300940503    2C                   ENDDO
304400940504     C*
304500940504     C                   ENDSR
304600000000      **************************************************************************
304700000000      * Stampo i terminal di arrivo
304800000000      **************************************************************************
304900000000     C     STATER        BEGSR
305000940429     C                   SETOFF                                       4333
305100940429     C                   SETON                                        30
305200950210     C                   Z-ADD     DS1FEL        $AREA
305300940429     C                   CLEAR                   $TFA
305400940509     C                   CLEAR                   $RIGHE
305500940429     C* ELABORO AREA PER AREA
305600940429     C  N39              EXSR      IMPARE
305700940429     C*
305800940429    2C     *IN33         DOWEQ     *OFF
305900940429     C*
306000940429     C* ELABORO TERMINAL PER TERMINAL
306100940429     C                   EXSR      IMPTFA
306200000000      *
306300940429    3C     *IN33         DOWEQ     *OFF
306400940429     C                   Z-ADD     *ZERO         $PRIM1
306500940429     C                   Z-ADD     *ZERO         $PRIM3
306600000000      *
306700090916     C     ARSATK        READE(N)  fisat03L                               33
306800940429     C*
306900940429    4C     *IN33         DOWEQ     *OFF
307000000000      *
307100940429     C* ELABORO SE DATA COMPRESA
307200940429     C* E, SE IMMESSA SOLTANTO UNA LINEA, SE E' LA LINEA RICHIESTA
307300970211    5C     SATDRE        IFLE      DS1AAL
307400940429     C*
307500940429     C* SE DA STAMPE GIORNALIERE ELABORO SLTANTO LE LINEE IN LIN
307600940429     C     SATLNP        LOOKUP    LIN                                    99
307700940429     C   99              EXSR      SERTRA
307800940429     C*
307900940429   X5C                   ELSE
308000940429     C                   SETON                                        33
308100940429    5C                   ENDIF
308200940429     C*
308300090916     C     ARSATK        READE(N)  fisat03L                               33
308400940429    4C                   ENDDO
308500940429      *
308600940429     C* Stampo le bolle dell'area
308700940429    4C     $PRIM1        IFEQ      1
308800940429     C                   EXSR      TOTFIL
308900940429     C                   EXSR      AZZFIL
309000940429    4C                   ENDIF
309100940429      *
309200940429      * Stampo le bolle in transito
309300940429    4C     $PRIM3        IFEQ      1
309400940429     C                   EXSR      TOTTRA
309500940429    4C                   ENDIF
309600940429     C*
309700940429     C* Elaboro nuovo terminal
309800940429     C                   EXSR      IMPTFA
309900940429    3C                   ENDDO
310000940429      *
310100940509     C                   Z-ADD     1             $PRIM3
310200970226     C                   SETON                                        0204
310300940429     C                   EXSR      TOTGEN
310400970226     C                   SETOFF                                       0204
310500940429     C* ELABORO NUOVA AREA
310600940429     C  N39              EXSR      IMPARE
310700940429    2C                   ENDDO
310800940429     C*
310900940429     C                   ENDSR
311000000000      **************************************************************************
311100000000      * Una volta letto il record verifico se da includere o no
311200000000      *
311300940428      * Includo se:
311400940428      * e' entrato al transito
311500940428      *  e non e' ancora uscito o e' uscito con data = > a  data elabor
311600940505      *            BTTDUT = 0
311700000000      *   oppure:
311800950210      *       AND  BTTDUT >= DS1ADAL
311900940428      *
312000940428      * escludo sempre i recuperi
312100000000      **************************************************************************
312200000000     C     ESCLU1        BEGSR
312300000000      *
312400000000     C                   MOVE      'N'           $OK
312500000000      *
312600000000     C     BTTDUT        IFEQ      *ZERO
312700950210     C     BTTDUT        ORGE      DS1AAL
312800000000     C                   MOVE      'S'           $OK
312900940428     C                   ENDIF
313000000000      *
313100000000      * Solo se superati i test ne eseguo altri
313200000000     C     $OK           IFEQ      'S'
313300940505      *
313400940505      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
313500940505      * come test ma mi permette di saltare delle chain)
313600950118    3C     $BTTAT        IFNE      $BLTPR
313700020625     C     FNBTPK        CHAIN     FNBTP11L                           99
313800950118     C  N99              MOVE      $BTTAT        $BLTPR
313900940505     C  N99              MOVEL     '1'           $BOLLA
314000940505     C   99              MOVEL     'N'           $OK
314100940505    3C                   ENDIF
314200000000      *
314300000000      * Eseguo la chain su TABEL solo se diverso dal precedente
314400950118     C  N99BTPCBO        IFNE      $CBLPR
314500950210     C                   MOVE      '3A'          KCOD
314600950210     C                   MOVE      *BLANKS       KKEY
314700950210     C                   MOVEL     BTPCBO        KKEY
314800950210     C     KTAB1         CHAIN     TABEL00F                           99
314900000000     C  N99              MOVEL     TBLUNI        DS3A
315000950118     C  N99              MOVE      BTPCBO        $CBLPR
315100000000     C   99              CLEAR                   DS3A
315200000000     C                   ENDIF
315300000000      *
315400000000      * Non considero i recuperi
315500940428     C     §3ARBL        IFEQ      'R'
315600000000     C                   MOVE      'N'           $OK
315700000000     C                   ENDIF
315800000000      *
315900000000     C                   ENDIF
316000000000      *
316100000000     C                   ENDSR
316200000000      **************************************************************************
316300090916      * RISTAMPA: lettura dal file statistiche (fisat00F) e output
316400000000      **************************************************************************
316500000000     C     RISTAM        BEGSR
316600000000      *
316700940502     C                   SETOFF                                       33
316800940428     C                   SETON                                        43
316900940429     C                   CLEAR                   $TFA
317000940509     C                   CLEAR                   $RIGHE
317100950210     C                   MOVEL     DS1FEL        $AREA
317200940429     C* LANCIO DA FILTRO
317300950210    1C     DS1FGS        IFGT      0
317400950210     C                   MOVEL     DS1LNP        SATLNP
317500940429     C*
317600940429     C* SE NON IMMESSA L'AREA LA CERCO
317700940429     C  N39              EXSR      IMPARE
317800940429     C*
317900940429    2C     *IN33         DOWEQ     *OFF
318000940429     C                   Z-ADD     *ZERO         $PRIM2
318100940429     C                   Z-ADD     *ZERO         $PRIM3
318200940429     C                   Z-ADD     *ZERO         $PRIM4
318300940429     C*
318400940429     C* SE NON IMMESSA LA LNP LA CERCO
318500940505     C                   EXSR      IMPLNP
318600000000      *
318700940429    3C     *IN33         DOWEQ     *OFF
318800940429     C                   Z-ADD     *ZERO         $PRIM1
318900940429     C* LEGGO IL FILE PER LNP
319000940429     C                   EXSR      LEGSAT
319100940429     C* cerco la nuova lnp
319200940429     C  N29              EXSR      IMPLNP
319300940429    3C                   ENDDO
319400940429     c*
319500940429     C* stampa finale dei C/S transiti e totale
319600940429     C                   EXSR      STAFIN
319700940429     C* cerco la nuova area
319800940429     C  N39              EXSR      IMPARE
319900940429    2C                   ENDDO
320000000000      *
320100950210   X1C                   ELSE
320200940429      *
320300940429      * Da stampe giornaliere
320400940429     C                   Z-ADD     1             $X
320500940429     C*
320600940506     C                   Z-ADD     *ZERO         $PRIM2            1 0
320700940506     C                   Z-ADD     *ZERO         $PRIM3            1 0
320800940506     C                   Z-ADD     *ZERO         $PRIM4            1 0
320900940429    2C     LIN($X)       DOWGT     0
321000940429     C                   Z-ADD     LIN($X)       $LNP
321100940506     C                   Z-ADD     LIN($X)       $FILPR
321200940505     C                   Z-ADD     *ZERO         $PRIM1            1 0
321300000912     C*
321400090916     C     DTSATK        SETLL     fisat02L
321500940429     C* LEGGO IL FILE PER LINEA
321600940429     C                   EXSR      LEGSAT
321700940429     C                   ADD       1             $X
321800940429    2C                   ENDDO
321900940429     c*
322000940429     C                   EXSR      STAFIN
322100000912    1C                   ENDIF
322200940429     C*
322300940429     C                   ENDSR
322400940429      **************************************************************************
322500940429      * IMPOSTO L'AREA, LE CERCO MAN MANO                              i
322600940429      **************************************************************************
322700940429     C     IMPARE        BEGSR
322800090916     C     $AREA         SETGT     fisat02L
322900090916     C                   READ(N)   fisat02L                               33
323000940429     C*
323100940429     C     *IN33         IFEQ      *OFF
323200940429     C                   MOVEL     SATFLE        $AREA
323300940429     C                   ENDIF
323400940429     C                   ENDSR
323500940429      **************************************************************************
323600940429      * IMPOSTO LA LINEA DI PARTENZA, LA CERCO MAN MANO                i
323700940429      **************************************************************************
323800940429     C     IMPLNP        BEGSR
323900090916     C  N29LPSATK        SETGT     fisat02L
324000090916     C  N29ARSATK        READE(N)  fisat02L                               33
324100000619     C                   CLEAR                   WTROVA
324200940429     C*
324300000619    1C     WTROVA        DOWEQ     ' '
324400000619    2C     *IN33         IFEQ      *OFF
324500000619     C     SATLNP        LOOKUP    LIN                                    99
324600000619    3C     *IN99         IFEQ      *ON
324700940429     C                   MOVEL     SATLNP        $LNP
324800940429     C                   MOVEL     SATLNP        $FILPR
324900000619     C                   MOVEL     '1'           WTROVA            1
325000090916     C     DTSATK        SETLL     fisat02L
325100000619   X3C                   ELSE
325200090916     C  N29ARSATK        READE(N)  fisat02L                               33
325300000619    3C                   ENDIF
325400000619     C**
325500000619   X2C                   ELSE
325600000619     C                   MOVEL     '1'           WTROVA
325700000619    2C                   ENDIF
325800000619    1C                   ENDDO
325900000619     C*
326000940429     C                   ENDSR
326100940429      **************************************************************************
326200940429      * IMPOSTO IL TERMINAL, LO CERCO MAN MANO                         i
326300940429      **************************************************************************
326400940429     C     IMPTFA        BEGSR
326500090916     C     ARSATK        SETGT     fisat03L
326600090916     C     $AREA         READE(N)  fisat03L                               33
326700940429     C*
326800940429     C     *IN33         IFEQ      *OFF
326900940429     C                   MOVEL     SATTFA        $TFA
327000940429     C                   MOVEL     SATTFA        $FILPR
327100090916     C     TASATK        SETLL     fisat03L
327200940429     C                   ENDIF
327300940429     C                   ENDSR
327400940429      **************************************************************************
327500940429      * STAMPA FINALE DEI C/S E TRANSITI                               i
327600940429      **************************************************************************
327700940429     C     STAFIN        BEGSR
327800940429     C* stampo i totali C/S
327900940506     C     *IN38         IFEQ      *OFF
328000940429     C     $PRIM2        IFEQ      1
328100940429     C                   EXSR      TOTSER
328200940429     C                   ENDIF
328300940506     C* TOTALI DI AREA
328400970226     C                   SETON                                        04
328500940505     C                   EXSR      TOTGEN
328600970226     C                   SETOFF                                       04
328700940506     C                   ENDIF
328800940502     C*
328900940502      * Terminate le letture per filiale tocca ai transiti
329000950210     C     DS1RIS        IFNE      'R'
329100940502     C                   EXSR      ELATRA
329200940503     C*
329300940505     C* CANCELLO LE RIGHE DEI TERMINAL CHE NON HANNO COLLI A TERRA
329400940503     C                   EXSR      ELATER
329500940502     C                   ENDIF
329600940502     C*
329700940429     C* stampo i totali dei TRANSITI
329800940506     C     *IN38         IFEQ      *OFF
329900000308     C     *IN27         IFEQ      *OFF
330000000308     C     DS1RIS        OREQ      'R'
330100000308     C                   EXSR      TOTTRA
330200000308     C                   ENDIF
330300940429     c* stampo il totale di area
330400940429     C     $PRIM4        IFEQ      1
330500970226     C                   SETON                                        0204
330600940429     C                   EXSR      TOTGEN
330700970226     C                   SETOFF                                       0204
330800950118     C                   WRITE     LSC2E4
330900940429     C                   ENDIF
331000940506     C                   ENDIF
331100940429     C                   ENDSR
331200940429      **************************************************************************
331300950210      * IMPOSTO L'AREA, LE CERCO MAN MANO
331400940429      **************************************************************************
331500940429     C     LEGSAT        BEGSR
331600090916     C     LPSATK        READE(N)  fisat02L                               33
331700940429     C*
331800940429    4C     *IN33         DOWEQ     *OFF
331900940429      *
332000970211    5C     SATDRE        IFLE      DS1AAL
332100940429      * Aggiorno i totali
332200940429     C                   EXSR      SERTRA
332300940429   x5C                   ELSE
332400940429     C                   SETON                                        33
332500940429    5C                   ENDIF
332600940429     C*
332700090916     C  N33LPSATK        READE(N)  fisat02L                               33
332800940429    4C                   ENDDO
332900940429     C* totali di filiale
333000940429    4C     $PRIM1        IFEQ      1
333100940429     C                   EXSR      TOTFIL
333200940429     C                   EXSR      AZZFIL
333300940429    4C                   ENDIF
333400940429     C                   ENDSR
333500000000      **************************************************************************
333600000000      * RISTAMPA: per servizi o transiti non stampo ma aggiorno i totali
333700000000      **************************************************************************
333800000000     C     SERTRA        BEGSR
333900000000      *
334000000000     C     SATTRE        IFEQ      *BLANK
334100940429     C                   Z-ADD     1             $PRIM1
334200970211     C                   ADD       SATCPE        CPA(1)
334300970211     C                   ADD       SATCPD        CPA(2)
334400970211     C                   ADD       SATCPC        CPA(3)
334500090916     C                   ADD       SATCPh        CPA(4)
334600090916     C                   ADD       SATCPx        CPA(5)
334700090916
334800970211     C                   ADD       SATCDE        CBP(1)
334900970211     C                   ADD       SATCDD        CBP(2)
335000970228     C                   ADD       SATCDC        CBP(3)
335100090916     C                   ADD       SATCDh        CBP(4)
335200090916     C                   ADD       SATCDx        CBP(5)
335300090916
335400970211     C                   ADD       SATCNE        CBR(1)
335500970211     C                   ADD       SATCND        CBR(2)
335600970211     C                   ADD       SATCNC        CBR(3)
335700090916     C                   ADD       SATCNh        CBR(4)
335800090916     C                   ADD       SATCNx        CBR(5)
335900090916
336000970211     C                   ADD       SATPPE        PPA(1)
336100970211     C                   ADD       SATPPD        PPA(2)
336200970211     C                   ADD       SATPPC        PPA(3)
336300090916     C                   ADD       SATPPh        PPA(4)
336400090916     C                   ADD       SATPPx        PPA(5)
336500090916
336600970211     C                   ADD       SATFDE        PFI(1)
336700970211     C                   ADD       SATFDD        PFI(2)
336800970211     C                   ADD       SATFDC        PFI(3)
336900090916     C                   ADD       SATFDh        PFI(4)
337000090916     C                   ADD       SATFDx        PFI(5)
337100090916
337200970211     C                   ADD       SATFNE        PFN(1)
337300970211     C                   ADD       SATFND        PFN(2)
337400970211     C                   ADD       SATFNC        PFN(3)
337500090916     C                   ADD       SATFNh        PFN(4)
337600090916     C                   ADD       SATFNx        PFN(5)
337700090916
337800970211     C                   ADD       SATODE        PBP(1)
337900970211     C                   ADD       SATODD        PBP(2)
338000970211     C                   ADD       SATODC        PBP(3)
338100090916     C                   ADD       SATODh        PBP(4)
338200090916     C                   ADD       SATODx        PBP(5)
338300090916
338400970211     C                   ADD       SATONE        PBR(1)
338500970211     C                   ADD       SATOND        PBR(2)
338600970211     C                   ADD       SATONC        PBR(3)
338700090916     C                   ADD       SATONh        PBR(4)
338800090916     C                   ADD       SATONx        PBR(5)
338900090916
339000970211     C                   ADD       SATVPE        VPA(1)
339100970211     C                   ADD       SATVPD        VPA(2)
339200970211     C                   ADD       SATVPC        VPA(3)
339300090916     C                   ADD       SATVPh        VPA(4)
339400090916     C                   ADD       SATVPx        VPA(5)
339500090916
339600970211     C                   ADD       SATVDE        VBP(1)
339700970211     C                   ADD       SATVDD        VBP(2)
339800970211     C                   ADD       SATVDC        VBP(3)
339900090916     C                   ADD       SATVDh        VBP(4)
340000090916     C                   ADD       SATVDx        VBP(5)
340100090916
340200970211     C                   ADD       SATVNE        VBR(1)
340300970211     C                   ADD       SATVND        VBR(2)
340400970211     C                   ADD       SATVNC        VBR(3)
340500090916     C                   ADD       SATVNh        VBR(4)
340600090916     C                   ADD       SATVNx        VBR(5)
340700090916
340800970211     C                   ADD       SATSPE        SPA(1)
340900970211     C                   ADD       SATSPD        SPA(2)
341000970211     C                   ADD       SATSPC        SPA(3)
341100090916     C                   ADD       SATSPh        SPA(4)
341200090916     C                   ADD       SATSPx        SPA(5)
341300090916
341400970211     C                   ADD       SATSDE        SBP(1)
341500970211     C                   ADD       SATSDD        SBP(2)
341600970211     C                   ADD       SATSDC        SBP(3)
341700090916     C                   ADD       SATSDh        SBP(4)
341800090916     C                   ADD       SATSDx        SBP(5)
341900090916
342000970211     C                   ADD       SATSNE        SBR(1)
342100970211     C                   ADD       SATSND        SBR(2)
342200970211     C                   ADD       SATSNC        SBR(3)
342300090916     C                   ADD       SATSNh        SBR(4)
342400090916     C                   ADD       SATSNx        SBR(5)
342500090916
342600970305     C                   ADD       SATSFE        FAF(1)
342700970305     C                   ADD       SATSFD        FAF(2)
342800970305     C                   ADD       SATSFC        FAF(3)
342900090916     C                   ADD       SATSFh        FAF(4)
343000090916     C                   ADD       SATSFx        FAF(5)
343100090916
343200000000     C                   ADD       SATSCE        SCM(1)
343300970211     C                   ADD       SATSCD        SCM(2)
343400970211     C                   ADD       SATSCC        SCM(3)
343500090916     C                   ADD       SATSCh        SCM(4)
343600090916     C                   ADD       SATSCx        SCM(5)
343700000000     C                   ENDIF
343800000000      *
343900000000     C     SATTRE        IFEQ      'C'
344000940429     C                   Z-ADD     1             $PRIM2
344100970211     C                   ADD       SATCPE        C1A(1)
344200970211     C                   ADD       SATCPD        C1A(2)
344300970211     C                   ADD       SATCPC        C1A(3)
344400090916     C                   ADD       SATCPh        C1A(4)
344500090916     C                   ADD       SATCPx        C1A(5)
344600090916
344700970211     C                   ADD       SATCDE        C1P(1)
344800970211     C                   ADD       SATCDD        C1P(2)
344900970211     C                   ADD       SATCDC        C1P(3)
345000090916     C                   ADD       SATCDh        C1P(4)
345100090916     C                   ADD       SATCDx        C1P(5)
345200090916
345300970211     C                   ADD       SATCNE        C1R(1)
345400970211     C                   ADD       SATCND        C1R(2)
345500970211     C                   ADD       SATCNC        C1R(3)
345600090916     C                   ADD       SATCNh        C1R(4)
345700090916     C                   ADD       SATCNx        C1R(5)
345800090916
345900970211     C                   ADD       SATPPE        P1A(1)
346000970303     C                   ADD       SATPPD        P1A(2)
346100970303     C                   ADD       SATPPC        P1A(3)
346200090916     C                   ADD       SATPPh        P1A(4)
346300090916     C                   ADD       SATPPx        P1A(5)
346400090916
346500970211     C                   ADD       SATFDE        P1I(3)
346600970211     C                   ADD       SATFDD        P1I(2)
346700970211     C                   ADD       SATFDC        P1I(3)
346800090916     C                   ADD       SATFDh        P1I(4)
346900090916     C                   ADD       SATFDx        P1I(5)
347000090916
347100970211     C                   ADD       SATFNE        P1N(1)
347200970211     C                   ADD       SATFND        P1N(2)
347300970211     C                   ADD       SATFNC        P1N(3)
347400090916     C                   ADD       SATFNh        P1N(4)
347500090916     C                   ADD       SATFNx        P1N(5)
347600090916
347700970211     C                   ADD       SATODE        P1P(1)
347800970211     C                   ADD       SATODD        P1P(2)
347900970211     C                   ADD       SATODC        P1P(3)
348000090916     C                   ADD       SATODh        P1P(4)
348100090916     C                   ADD       SATODx        P1P(5)
348200090916
348300970211     C                   ADD       SATONE        P1R(1)
348400970211     C                   ADD       SATOND        P1R(2)
348500970211     C                   ADD       SATONC        P1R(3)
348600090916     C                   ADD       SATONh        P1R(4)
348700090916     C                   ADD       SATONx        P1R(5)
348800090916
348900970211     C                   ADD       SATVPE        V1A(1)
349000970211     C                   ADD       SATVPD        V1A(2)
349100970211     C                   ADD       SATVPC        V1A(3)
349200090916     C                   ADD       SATVPh        V1A(4)
349300090916     C                   ADD       SATVPx        V1A(5)
349400090916
349500970211     C                   ADD       SATVDE        V1P(1)
349600970211     C                   ADD       SATVDD        V1P(2)
349700970211     C                   ADD       SATVDC        V1P(3)
349800090916     C                   ADD       SATVDh        V1P(4)
349900090916     C                   ADD       SATVDx        V1P(5)
350000090916
350100970211     C                   ADD       SATVNE        V1R(1)
350200970211     C                   ADD       SATVND        V1R(2)
350300970211     C                   ADD       SATVNC        V1R(3)
350400090916     C                   ADD       SATVNh        V1R(4)
350500090916     C                   ADD       SATVNx        V1R(5)
350600090916
350700970211     C                   ADD       SATSPE        S1A(1)
350800970211     C                   ADD       SATSPD        S1A(2)
350900970211     C                   ADD       SATSPC        S1A(3)
351000090916     C                   ADD       SATSPh        S1A(4)
351100090916     C                   ADD       SATSPx        S1A(5)
351200090916
351300970211     C                   ADD       SATSDE        S1P(1)
351400970211     C                   ADD       SATSDD        S1P(2)
351500970211     C                   ADD       SATSDC        S1P(3)
351600090916     C                   ADD       SATSDh        S1P(4)
351700090916     C                   ADD       SATSDx        S1P(5)
351800090916
351900970211     C                   ADD       SATSNE        S1R(1)
352000970211     C                   ADD       SATSND        S1R(2)
352100970211     C                   ADD       SATSNC        S1R(3)
352200090916     C                   ADD       SATSNh        S1R(4)
352300090916     C                   ADD       SATSNx        S1R(5)
352400090916
352500970305     C                   ADD       SATSFE        F1F(1)
352600970305     C                   ADD       SATSFD        F1F(2)
352700970305     C                   ADD       SATSFC        F1F(3)
352800090916     C                   ADD       SATSFh        F1F(4)
352900090916     C                   ADD       SATSFx        F1F(5)
353000090916
353100000000     C                   ADD       SATSCE        S1M(1)
353200970211     C                   ADD       SATSCD        S1M(2)
353300970211     C                   ADD       SATSCC        S1M(3)
353400090916     C                   ADD       SATSCh        S1M(4)
353500090916     C                   ADD       SATSCx        S1M(5)
353600000000     C                   ENDIF
353700000000      *
353800940429     C     SATTRE        IFEQ      'T'
353900940429     C                   Z-ADD     1             $PRIM3
354000970211     C                   ADD       SATCDE        C2P(1)
354100970211     C                   ADD       SATCDD        C2P(2)
354200970211     C                   ADD       SATCDC        C2P(3)
354300090916     C                   ADD       SATCDh        C2P(4)
354400090916     C                   ADD       SATCDx        C2P(5)
354500090916
354600970211     C                   ADD       SATFDE        P2I(1)
354700970211     C                   ADD       SATFDD        P2I(2)
354800970211     C                   ADD       SATFDC        P2I(3)
354900090916     C                   ADD       SATFDh        P2I(4)
355000090916     C                   ADD       SATFDx        P2I(5)
355100090916
355200970211     C                   ADD       SATODE        P2P(1)
355300970211     C                   ADD       SATODD        P2P(2)
355400970211     C                   ADD       SATODC        P2P(3)
355500090916     C                   ADD       SATODh        P2P(4)
355600090916     C                   ADD       SATODx        P2P(5)
355700090916
355800970211     C                   ADD       SATVDE        V2P(1)
355900970211     C                   ADD       SATVDD        V2P(2)
356000970211     C                   ADD       SATVDC        V2P(3)
356100090916     C                   ADD       SATVDh        V2P(4)
356200090916     C                   ADD       SATVDx        V2P(5)
356300090916
356400970211     C                   ADD       SATSDE        S2P(1)
356500970211     C                   ADD       SATSDD        S2P(2)
356600970211     C                   ADD       SATSDC        S2P(3)
356700090916     C                   ADD       SATSDh        S2P(4)
356800090916     C                   ADD       SATSDx        S2P(5)
356900090916
357000000000     C                   ADD       SATSCE        S2M(1)
357100970211     C                   ADD       SATSCD        S2M(2)
357200970211     C                   ADD       SATSCC        S2M(3)
357300090916     C                   ADD       SATSCh        S2M(4)
357400090916     C                   ADD       SATSCx        S2M(5)
357500940505     C*
357600940505     C                   ELSE
357700000000      *
357800940429      *  totali generali
357900940429     C                   Z-ADD     1             $PRIM4
358000970211     C                   ADD       SATCDE        C3P(1)
358100970211     C                   ADD       SATCDD        C3P(2)
358200970211     C                   ADD       SATCDC        C3P(3)
358300090916     C                   ADD       SATCDh        C3P(4)
358400090916     C                   ADD       SATCDx        C3P(5)
358500090916
358600970211     C                   ADD       SATFDE        P3I(1)
358700970211     C                   ADD       SATFDD        P3I(2)
358800970211     C                   ADD       SATFDC        P3I(3)
358900090916     C                   ADD       SATFDh        P3I(4)
359000090916     C                   ADD       SATFDx        P3I(5)
359100090916
359200970212     C                   ADD       SATODE        P3P(1)
359300970212     C                   ADD       SATODD        P3P(2)
359400970212     C                   ADD       SATODC        P3P(3)
359500090916     C                   ADD       SATODh        P3P(4)
359600090916     C                   ADD       SATODx        P3P(5)
359700090916
359800970211     C                   ADD       SATVDE        V3P(1)
359900970211     C                   ADD       SATVDD        V3P(2)
360000970211     C                   ADD       SATVDC        V3P(3)
360100090916     C                   ADD       SATVDh        V3P(4)
360200090916     C                   ADD       SATVDx        V3P(5)
360300090916
360400000406     C                   ADD       SATVNE        V3R(1)
360500000406     C                   ADD       SATVND        V3R(2)
360600000406     C                   ADD       SATVNC        V3R(3)
360700090916     C                   ADD       SATVNh        V3R(4)
360800090916     C                   ADD       SATVNx        V3R(5)
360900090916
361000970211     C                   ADD       SATSDE        S3P(1)
361100970211     C                   ADD       SATSDD        S3P(2)
361200970211     C                   ADD       SATSDC        S3P(3)
361300090916     C                   ADD       SATSDh        S3P(4)
361400090916     C                   ADD       SATSDx        S3P(5)
361500090916
361600970211     C                   ADD       SATSNE        S3R(1)
361700970211     C                   ADD       SATSND        S3R(2)
361800970211     C                   ADD       SATSNC        S3R(3)
361900090916     C                   ADD       SATSNh        S3R(4)
362000090916     C                   ADD       SATSNx        S3R(5)
362100090916
362200970305     C                   ADD       SATSFE        F3F(1)
362300970305     C                   ADD       SATSFD        F3F(2)
362400970305     C                   ADD       SATSFC        F3F(3)
362500090916     C                   ADD       SATSFh        F3F(4)
362600090916     C                   ADD       SATSFx        F3F(5)
362700090916
362800000000     C                   ADD       SATSCE        S3M(1)
362900970211     C                   ADD       SATSCD        S3M(2)
363000970211     C                   ADD       SATSCC        S3M(3)
363100090916     C                   ADD       SATSCh        S3M(4)
363200090916     C                   ADD       SATSCx        S3M(5)
363300940505     C                   ENDIF
363400000000      *
363500000000     C                   ENDSR
363600940505      **************************************************************************
363700940505      * RISTAMPA: addiziono i transiti al totale generale              i
363800940505      **************************************************************************
363900940505     C     ADDTRA        BEGSR
364000940505     C                   Z-ADD     1             $PRIM4
364100940505     C                   ADD       C2P           C3P
364200940505     C                   ADD       P2I           P3I
364300940505     C                   ADD       P2P           P3P
364400940505     C                   ADD       V2P           V3P
364500940505     C                   ADD       S2P           S3P
364600940505     C                   ADD       S2M           S3M
364700940505     C                   ENDSR
364800000000      **************************************************************************
364900000000      * RISTAMPA: azzera i totali generali
365000000000      **************************************************************************
365100000000     C     AZZTOT        BEGSR
365200000000      *
365300970212     C                   CLEAR                   C3A
365400970212     C                   CLEAR                   C3P
365500970211     C                   CLEAR                   P3I
365600970211     C                   CLEAR                   P3P
365700970211     C                   CLEAR                   P3A
365800970211     C                   CLEAR                   V3P
365900940502     C                   CLEAR                   V3A
366000000406     C                   CLEAR                   V3R
366100970211     C                   CLEAR                   S3A
366200970211     C                   CLEAR                   S3P
366300970211     C                   CLEAR                   S3R
366400970211     C                   CLEAR                   S3M
366500970305     C                   CLEAR                   F3F
366600000000      *
366700000000     C                   ENDSR
366800000000      **************************************************************************
366900000000      * Testa l'overflow e stampa su carta
367000000000      **************************************************************************
367100000000     C     SCRIVI        BEGSR
367200940509      * PER TOTALI GENERALI SE GIA' STAMPE PIU' DI 9 RIGHE COMUNQUE
367300940509      * VADO A PAGINA NUOVA
367400940509     C     *IN03         IFEQ      *OFF
367500940509     C     *IN02         ANDEQ     *ON
367600940509     C     $RIGHE        ANDGT     9
367700940509     C                   SETON                                        30
367800940509     C                   ENDIF
367900000000      *
368000940429     C     *IN38         IFEQ      *OFF
368100940509     C*
368200940509     C     *IN30         IFEQ      *ON
368300940509     C     *IN40         ANDEQ     *ON
368400950118     C                   WRITE     LSC2T1
368500950118     C                   WRITE     LSC2T3
368600940509     C                   CLEAR                   $RIGHE
368700940509     C                   SETOFF                                       30
368800940509     C                   ENDIF
368900950118     C                   WRITE     LSC2D1
369000000000      *
369100970226     C  N04$ALMEN        IFEQ      *ZERO
369200000000     C                   Z-ADD     1             $ALMEN
369300000000     C                   ENDIF
369400940328     C                   ENDIF
369500000000      *
369600000000     C                   ENDSR
369700000000      **************************************************************************
369800000000      * Verifica se vi sono dei fogli viaggio con data partenza compresa
369900000000      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
370000000000      **************************************************************************
370100000000     C     FOGVIA        BEGSR
370200020813     ***C                   OPEN      FNFGV04L
370300020813     C                   OPEN      FNFGV06L
370400000000      *
370500020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
370600020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
370700020813     C     SIMfel        setll     FNFGV06L                               99
370800000000     C                   DO        *HIVAL
370900020813     ***C                   READ      FNFGV04L                               99
371000020813     C     SIMfel        reade     FNFGV06L                               99
371100950210     C  N99FGVDTP        IFGE      DS1ADA
371200950210     C     FGVDTP        ANDLE     DS1AAL
371300940502     C                   SETON                                        99
371400950118     C                   WRITE     LSC2E3
371500000000     C                   ENDIF
371600940502     C  N99              ENDDO
371700000000      *
371800020813     ***C                   CLOSE     FNFGV04L
371900020813     C                   CLOSE     FNFGV06L
372000000000     C                   ENDSR
372100000000      **************************************************************************
372200000000      * Aggiornamento del numero colli esaminati, nel file di work
372300950118     OFNWRK000  E            AGGIO
372400000000     O                       WRKNC1
372500000000      **************************************************************************
372600000000      *                                4         5         6         7         8
372700950118** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
372800970807OVRPRTF FNLSC2P       FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.terra
372900950210')
