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
006300000000      * Le schiere che seguono hanno 3 elementi:
006400000000      *     1° elemento: spedizione espressa (ESPR.)
006500950627      *     2° elemento: spedizione distribuzione (DISTR)
006600950627      *     3° elemento: spedizione corriere (CORR.)
006700090605     D**!!! ENS             S              5    DIM(3) CTDATA PERRCD(3)
006800090605     D ENS             S              5    DIM(3)
006900000000      * COLLI
007000970210     D CPA             S              7  0 DIM(3)
007100970210     D CBP             S              7  0 DIM(3)
007200970210     D CBR             S              7  0 DIM(3)
007300000000      * PESI
007400970210     D PPA             S              9  1 DIM(3)
007500970210     D PFI             S              9  1 DIM(3)
007600970210     D PFN             S              9  1 DIM(3)
007700970210     D PBP             S              9  1 DIM(3)
007800970210     D PBR             S              9  1 DIM(3)
007900000000      * VOLUMI
008000970210     D VPA             S              9  4 DIM(3)
008100970210     D VBP             S              9  4 DIM(3)
008200970210     D VBR             S              9  4 DIM(3)
008300000000      * SPEDIZIONI
008400970210     D SPA             S              7  0 DIM(3)
008500970210     D SBP             S              7  0 DIM(3)
008600970210     D SBR             S              7  0 DIM(3)
008700970210     D SCM             S              7  0 DIM(3)
008800970305     D FAF             S              7  0 DIM(3)
008900000000      * COLLI PER C/SERVIZIO (TOTALE AREA) -------------------------------------
009000970210AREA D C1A             S              7  0 DIM(3)
009100000000AREA D C1P             S              7  0 DIM(3)
009200000000AREA D C1R             S              7  0 DIM(3)
009300000000      * PESI PER C/SERVIZIO (TOTALE AREA)
009400970210AREA D P1A             S              9  1 DIM(3)
009500000000AREA D P1I             S              9  1 DIM(3)
009600970207AREA D P1N             S              9  1 DIM(3)
009700000000AREA D P1P             S              9  1 DIM(3)
009800000000AREA D P1R             S              9  1 DIM(3)
009900000000      * VOLUMI PER C/SERVIZIO (TOTALE AREA)
010000970210AREA D V1A             S              9  4 DIM(3)
010100000000AREA D V1P             S              9  4 DIM(3)
010200000000AREA D V1R             S              9  4 DIM(3)
010300000000      * SPEDIZIONI PER C/SERVIZIO (TOTALE AREA)
010400970210AREA D S1A             S              7  0 DIM(3)
010500000000AREA D S1P             S              7  0 DIM(3)
010600000000AREA D S1R             S              7  0 DIM(3)
010700000000AREA D S1M             S              7  0 DIM(3)
010800970305AREA D F1F             S              7  0 DIM(3)
010900000000      * COLLI PER C/SERVIZIO (PER FILIALE) -------------------------------------
011000970210FIL  D C4A             S              7  0 DIM(3)
011100000000FIL  D C4P             S              7  0 DIM(3)
011200000000FIL  D C4R             S              7  0 DIM(3)
011300000000      * PESI PER C/SERVIZIO (PER FILIALE)
011400970210FIL  D P4A             S              9  1 DIM(3)
011500000000FIL  D P4I             S              9  1 DIM(3)
011600970207FIL  D P4N             S              9  1 DIM(3)
011700000000FIL  D P4P             S              9  1 DIM(3)
011800000000FIL  D P4R             S              9  1 DIM(3)
011900000000      * VOLUMI PER C/SERVIZIO (PER FILIALE)
012000970210FIL  D V4A             S              9  4 DIM(3)
012100000000FIL  D V4P             S              9  4 DIM(3)
012200000000FIL  D V4R             S              9  4 DIM(3)
012300000000      * SPEDIZIONI PER C/SERVIZIO (PER FILIALE)
012400970210FIL  D S4A             S              7  0 DIM(3)
012500000000FIL  D S4P             S              7  0 DIM(3)
012600000000FIL  D S4R             S              7  0 DIM(3)
012700000000FIL  D S4M             S              7  0 DIM(3)
012800970305FIL  D F4F             S              7  0 DIM(3)
012900940502      *
013000940502      * COLLI PER TRANSITI (TOTALE GENERALE) -----------------------------------
013100940428GEN  D C2P             S              7  0 DIM(3)
013200000000      * PESI PER TRANSITI (TOTALE GENERALE)
013300000000GEN  D P2I             S              9  1 DIM(3)
013400940428GEN  D P2P             S              9  1 DIM(3)
013500000000      * VOLUMI PER TRANSITI (TOTALE GENERALE)
013600940428GEN  D V2P             S              9  4 DIM(3)
013700000000      * SPEDIZIONI PER TRANSITI (TOTALE GENERALE)
013800940428GEN  D S2P             S              7  0 DIM(3)
013900000000GEN  D S2M             S              7  0 DIM(3)
014000940429      * COLLI PER TOTALE AREA --------------------------------------------------
014100970211     D C3A             S              7  0 DIM(3)
014200940428     D C3P             S              7  0 DIM(3)
014300940428      * PESI PER TOTALE GENERALE
014400970211     D P3A             S              9  1 DIM(3)
014500940428     D P3I             S              9  1 DIM(3)
014600940428     D P3P             S              9  1 DIM(3)
014700940428      * VOLUMI PER TOTALE GENERALE
014800970211     D V3A             S              9  4 DIM(3)
014900940428     D V3P             S              9  4 DIM(3)
015000000331     D V3R             S              9  4 DIM(3)
015100940428      * SPEDIZIONI PER TOTALE GENERALE
015200970211     D S3A             S              7  0 DIM(3)
015300940428     D S3P             S              7  0 DIM(3)
015400940428     D S3R             S              7  0 DIM(3)
015500940428     D S3M             S              7  0 DIM(3)
015600970305     D F3F             S              7  0 DIM(3)
015700970207      * COLLI PER FILE FNSAT00F ------------------------------------------------
015800970207      * PESI PER FILE FNSAT00F
015900970207      * VOLUMI PER FILE FNSAT00F
016000970207      * SPEDIZIONI PER FILE FNSAT00F
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
017800000810     D OG143         E DS
017900000810     D DS3A          E DS
018000000000     D DS3L          E DS
018100040921     D*DS5C          E DS
018200970707     D DS5P          E DS
018300000000     D KPJBA         E DS
018400000000     D CNCR80        E DS
018500000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
018600950210     D* DS PER TRUL06R - CARICAMENTO £X
018700950210     D DSUL06        E DS                  EXTNAME(TRUL06DS)
018800950314     D  T06                    1     90  0
018900950314     D                                     DIM(30)
019000970303      * DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
019100970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
019200950210     D DSLS01        E DS
019300020813      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
019400020813     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
019500070522     D* DS PER LETTURA TABELLA "VPO" record DECO
019600070522     D Dvpodeco      E DS
019700021016      * Ds date statistiche
019800021016     d dSdf          e ds
019900090605
020000090605     d ds5e          e ds
020100020813      *
020200950210     D                 DS
020300950210     D  DATADA                 1      8  0
020400950210     D  ANNODA                 1      4  0
020500950210     D  MESEDA                 5      6  0
020600950210     D  $GI                    7      8  0
020700950118      * DS per controllare il "precedente" di FNBLT
020800000000     D $BLTAT          DS
020900000000     D  BLTLNP                 1      3  0
021000950118     D  BLTAAS                 4      7  0
021100950118     D  BLTNRS                 8      9  0
021200950118     D  BLTNSP                10     16  0
021300950118      * DS per controllare il "precedente" di FNBTT
021400950118     D $BTTAT          DS
021500950118     D  BTTLNP                 1      3  0
021600950118     D  BTTAAS                 4      7  0
021700950118     D  BTTNRS                 8      9  0
021800950118     D  BTTNSP                10     16  0
021900970207      * DS per i campi del file statistiche FNSAT00F
022000000000     D                 DS
022100000000     D  C0P                    1     21  0
022200970207     D                                     DIM(3)
022300970207     D  SATCDE                 1      7  0
022400970207     D  SATCDD                 8     14  0
022500970207     D  SATCDC                15     21  0
022600000000     D                 DS
022700000000     D  C0R                    1     21  0
022800970207     D                                     DIM(3)
022900970207     D  SATCNE                 1      7  0
023000970207     D  SATCND                 8     14  0
023100970207     D  SATCNC                15     21  0
023200000000     D                 DS
023300000000     D  P0I                    1     27  1
023400970207     D                                     DIM(3)
023500970207     D  SATFDE                 1      9  1
023600970207     D  SATFDD                10     18  1
023700970207     D  SATFDC                19     27  1
023800970207     D                 DS
023900970207     D  P0N                    1     27  1
024000970207     D                                     DIM(3)
024100970207     D  SATFNE                 1      9  1
024200970207     D  SATFND                10     18  1
024300970207     D  SATFNC                19     27  1
024400000000     D                 DS
024500000000     D  P0P                    1     27  1
024600970210     D                                     DIM(3)
024700970207     D  SATODE                 1      9  1
024800970207     D  SATODD                10     18  1
024900970207     D  SATODC                19     27  1
025000000000     D                 DS
025100000000     D  P0R                    1     27  1
025200970207     D                                     DIM(3)
025300970207     D  SATONE                 1      9  1
025400970207     D  SATOND                10     18  1
025500970207     D  SATONC                19     27  1
025600000000     D                 DS
025700000000     D  V0P                    1     27  4
025800970207     D                                     DIM(3)
025900970207     D  SATVDE                 1      9  4
026000970207     D  SATVDD                10     18  4
026100970207     D  SATVDC                19     27  4
026200000000     D                 DS
026300000000     D  V0R                    1     27  4
026400970207     D                                     DIM(3)
026500970207     D  SATVNE                 1      9  4
026600970207     D  SATVND                10     18  4
026700970207     D  SATVNC                19     27  4
026800000000     D                 DS
026900000000     D  S0P                    1     21  0
027000970207     D                                     DIM(3)
027100970207     D  SATSDE                 1      7  0
027200970207     D  SATSDD                 8     14  0
027300970207     D  SATSDC                15     21  0
027400000000     D                 DS
027500000000     D  S0R                    1     21  0
027600970207     D                                     DIM(3)
027700970207     D  SATSNE                 1      7  0
027800970207     D  SATSND                 8     14  0
027900970207     D  SATSNC                15     21  0
028000970305     D                 DS
028100970305     D  F0F                    1     21  0
028200970305     D                                     DIM(3)
028300970305     D  SATSFE                 1      7  0
028400970305     D  SATSFD                 8     14  0
028500970305     D  SATSFC                15     21  0
028600000000     D                 DS
028700000000     D  S0M                    1     21  0
028800000000     D                                     DIM(3)
028900000000     D  SATSCE                 1      7  0
029000970207     D  SATSCD                 8     14  0
029100970207     D  SATSCC                15     21  0
029200970210     D                 DS
029300970210     D  CDT                    1     21  0
029400970210     D                                     DIM(3)
029500970210     D  SATCPE                 1      7  0
029600970210     D  SATCPD                 8     14  0
029700970211     D  SATCPC                15     21  0
029800970210     D                 DS
029900970210     D  PDT                    1     27  1
030000970210     D                                     DIM(3)
030100970210     D  SATPPE                 1      9  1
030200970210     D  SATPPD                10     18  1
030300970210     D  SATPPC                19     27  1
030400970210     D                 DS
030500970210     D  VDT                    1     27  4
030600970210     D                                     DIM(3)
030700970210     D  SATVPE                 1      9  4
030800970210     D  SATVPD                10     18  4
030900970210     D  SATVPC                19     27  4
031000970210     D                 DS
031100970210     D  SDT                    1     21  0
031200970210     D                                     DIM(3)
031300970210     D  SATSPE                 1      7  0
031400970210     D  SATSPD                 8     14  0
031500970210     D  SATSPC                15     21  0
031600020813      *
031700020813      * Varibaili
031800020813      *
031900020813     D SAVRIS          S                   LIKE(DS1RIS) inz
032000020813     D $CDA            S              7  0              inz
032100020813     D $VDA            S              9  4              inz
032200020813     D $PDA            S              9  1              inz
032300020813     D $SDA            S              7  0              inz
032400020813     D $TFA            S                   LIKE(BLPTFA) inz
032500020813     D $AREA           S                   LIKE(DS1FEL) inz
032600020813     D $LNP            S                   LIKE(DS1LNP) inz
032700020813     D $BLTPR          S                   LIKE($BLTAT) inz
032800020813     D $FILPR          S                   LIKE(BLPLNP) inz
032900020813     D $CBLPR          S                   LIKE(BLPCBO) inz
033000020813     D $NFVPR          S                   LIKE(BLPNFV) inz
033100020813     D $DTRIT          S                   LIKE(BLPDRT) inz
033200020813     D $CDPDR          S                   LIKE(BLPPDR) inz
033300020813     D $FLAG           S                   LIKE(WRKFLG) inz
033400020813     D $FLAG2          S                   LIKE(WRKFLG) inz
033500020813     D $DFV            S                   LIKE(FGVDFV) inz
033600020813     D $FTR            S                   LIKE(SATFTR) inz
033700020813     D $DTR            S                   LIKE(SATDTR) inz
033800020813     D $TRE            S                   LIKE(SATTRE) inz
033900020813     D $COMOD          S             15  6 INZ
034000020813     D $COMO1          S             15  6 INZ
034100020813     D*
034200020813     D KCOD            S                   LIKE(TBLCOD) inz
034300020813     D KKEY            S                   LIKE(TBLKEY) inz
034400021217      *
034500021217     d $Volum          s             12  4 inz
034600021217     d $Peso           s              8  1 inz
034700030213     d*** WsiVOL          s              1    inz
034800030213     d*** WsiPES          s              1    inz
034900021217     d Wvolu           s                   like(BLPvlf) inz
035000021217     d Wpeso           s                   like(BLPpkf) inz
035100021220     d Wvolmed         s                   like($Volum) inz
035200021220     d Wpesmed         s                   like($Peso)  inz
035300090415     D ktrc            S                   LIKE(statrc) inz('T')
035400090415     D kfil            S                   LIKE(stafil) inz(000)
035500940502      **************************************************************************
035600940502      *      RIEPILOGO INDICATORI
035700940502      **************************************************************************
035800940502      * 01    - ON: LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE = 0
035900940502      *       - OFF:LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE > D.ELAB
036000940502      * 02    - ON: TOTALE GENERALE
036100940502      *       - OFF:TOTALE DI AREA
036200970226      * 03    - STAMPA DEI TOTALI TRANSITI
036300970226      * 04    - NON MEMORIZZA $ALMEN NELLA ROURINE DI TOTGEN
036400940502      * 05    - STAMPE GIORNALIERE
036500970205      * 27    - OFF: DEVO STAMPARE LA RIGA DEI TRANASITI
036600940502      * 29    - ON: IMMESSA UNA LINEA DI PARTENZA
036700940502      * 30    - ECCEDENZA PAGINA
036800940502      * 33    - LETTURA FILES
036900940502      * 34    - BOLLA IN C/SERVIZIO
037000940502      * 35    - ON: BOLLA BOLLETTATA NEL PERIODO
037100940502      *       - OFF:BOLLA BOLLETTATA NEL PERIODO PRECEDENTE
037200940502      * 36    - COLLO A TERRA
037300970211      * 37    - LETTURA SUL FILES FNSAT
037400950210      * 38    - DS1RIS= ' ' --> NON DEVO STAMPARE
037500940502      * 39    - ON: IMMESSO CODICE AREA
037600940502      * 40    - ON: STAMPA LA LILIALE CON DECODIFICA
037700940502      *       - OFF:STAMPA LA DICITURA DEL SERVIZIO BOLLE
037800940502      * 41    - ON: STAMPA SOLTANTO LA FILIALE (+ 40 ON)
037900940502      * 43    - ON: STAMPO LA PARTE RELATIVA ALLE LINEE DI PARTENZA
038000940502      *       - OFF:STAMPO LA PARTE RELATIVA AI TERMINAL DI ARRIVO
038100970211      * 45    - STAMPA LE PERCENTUALI SUL DA PARTIRE
038200950118      * 99    - DI COMODO
038300940502      **************************************************************************
038400000000     C     *ENTRY        PLIST
038500000000     C                   PARM                    KPJBA
038600950210     C                   MOVEL     KPJBU         DSLS01
038700950210      *
038800950210      * DATA FINALE AA/MM/GG (6 CARATTERI)
038900090108     C                   Z-ADD     DS1aal        DATADA
039000000000      *
039100090415     c     KSTA          Klist
039200090415     c                   Kfld                    Ktrc
039300090415     c                   Kfld                    DS1FEL
039400090415     c                   Kfld                    Kfil
039500090415     c                   Kfld                    ds1ada
039600950118     C     FNBLTK        KLIST
039700940428     C                   KFLD                    LIN($X)
039800940428     C                   KFLD                    $DFV
039900970205     C     FNBLPK        KLIST
040000950118     C                   KFLD                    BLTAAS
040100000000     C                   KFLD                    BLTLNP
040200000000     C                   KFLD                    BLTNRS
040300000000     C                   KFLD                    BLTNSP
040400950118     C     FNBTPK        KLIST
040500020625     c                   kfld                    bttflp
040600950118     C                   KFLD                    BTTAAS
040700950118     C                   KFLD                    BTTLNP
040800950118     C                   KFLD                    BTTNRS
040900950118     C                   KFLD                    BTTNSP
041000950118     C     AZCLNK        KLIST
041100940504     C                   KFLD                    SIMFEL
041200940504     C                   KFLD                    $TFA
041300940504     C                   KFLD                    ANNODA
041400940504     C                   KFLD                    MESEDA
041500950118     C     FNWRKK        KLIST
041600000000     C                   KFLD                    $FLAG
041700000000     C                   KFLD                    $FILPR
041800950118     C     FNWR1K        KLIST
041900000000     C                   KFLD                    $FLAG
042000000000     C                   KFLD                    WRKLNP
042100940503     C                   KFLD                    BLPAAS
042200940503     C                   KFLD                    BLPNRS
042300940503     C                   KFLD                    BLPNSP
042400000000     C                   KFLD                    WRKCSE
042500970409     C     FNWR2K        KLIST
042600970409     C                   KFLD                    $FLAG
042700970409     C                   KFLD                    WRKLNP
042800970409     C                   KFLD                    BTPAAS
042900970409     C                   KFLD                    BTPNRS
043000970409     C                   KFLD                    BTPNSP
043100970409     C                   KFLD                    WRKCSE
043200970205     C     FNSATK        KLIST
043300970205     C                   KFLD                    SIMFEL
043400970205     C                   KFLD                    LIN(XX)
043500970205     C                   KFLD                    DS1AAL
043600970205     C     FNSA1K        KLIST
043700000000     C                   KFLD                    SATFLE
043800000000     C                   KFLD                    SATTFA
043900940429     C                   KFLD                    SATLNP
044000000000     C                   KFLD                    SATDRE
044100000000     C                   KFLD                    SATTRE
044200970205     C     DTSATK        KLIST
044300940429     C                   KFLD                    $AREA
044400940429     C                   KFLD                    $TFA
044500940429     C                   KFLD                    $LNP
044600970211     C                   KFLD                    DS1ADA
044700970205     C     LPSATK        KLIST
044800940429     C                   KFLD                    $AREA
044900940429     C                   KFLD                    $TFA
045000940429     C                   KFLD                    $LNP
045100970205     C     ARSATK        KLIST
045200940429     C                   KFLD                    $AREA
045300940429     C                   KFLD                    $TFA
045400970211     C     TASATK        KLIST
045500940429     C                   KFLD                    $AREA
045600940429     C                   KFLD                    $TFA
045700970211     C                   KFLD                    DS1ADA
045800970211     C     TRSATK        KLIST
045900940503     C                   KFLD                    $AREA
046000940503     C                   KFLD                    $TFA
046100970211     C                   KFLD                    DS1ADA
046200940503     C                   KFLD                    $TRE
046300020625     c     kbtt39        klist
046400020625     c                   kfld                    simfel
046500020625     c                   kfld                    $dfv
046600021016
046700021016     c     Ktbe          Klist
046800021016     c                   Kfld                    kTbeCod
046900021016     c                   Kfld                    kTbeKe1
047000000000      *
047100020813     ***C     *LIKE         DEFINE    DS1RIS        SAVRIS
047200020813     ***C     *LIKE         DEFINE    CPA(1)        $CDA
047300020813     ***C     *LIKE         DEFINE    VPA(1)        $VDA
047400020813     ***C     *LIKE         DEFINE    PPA(1)        $PDA
047500020813     ***C     *LIKE         DEFINE    SPA(1)        $SDA
047600020813     ***C     *LIKE         DEFINE    BLPTFA        $TFA
047700020813     ***C     *LIKE         DEFINE    DS1FEL        $AREA
047800020813     ***C     *LIKE         DEFINE    DS1LNP        $LNP
047900020813     ***C     *LIKE         DEFINE    $BLTAT        $BLTPR
048000020813     ***C     *LIKE         DEFINE    BLPLNP        $FILPR
048100020813     ***C     *LIKE         DEFINE    BLPCBO        $CBLPR
048200020813     ***C     *LIKE         DEFINE    BLPNFV        $NFVPR
048300020813     ***C     *LIKE         DEFINE    BLPDRT        $DTRIT
048400020813     ***C     *LIKE         DEFINE    BLPPDR        $CDPDR
048500020813     ***C     *LIKE         DEFINE    WRKFLG        $FLAG
048600020813     ***C     *LIKE         DEFINE    WRKFLG        $FLAG2
048700020813     ***C     *LIKE         DEFINE    FGVDFV        $DFV
048800020813     ***C     *LIKE         DEFINE    SATFTR        $FTR
048900020813     ***C     *LIKE         DEFINE    SATDTR        $DTR
049000020813     ***C     *LIKE         DEFINE    SATTRE        $TRE
049100020813     ***C                   Z-ADD     *ZERO         $COMOD           15 6
049200021016     C                   Z-ADD     *ZERO         $COMO1           15 6
049300021016     C                   Z-ADD     1             CODUT
049400000000      **************************************************************************
049500000000      *              M A I N      L I N E
049600000000      **************************************************************************
049700940429     C                   MOVE      'RISTAMPA'    STRIP
049800940429     C*
049900950210    1C     DS1RIS        IFNE      'R'
050000940429     C                   MOVE      'STAMPA  '    STRIP
050100940502    1C                   ENDIF
050200070522     c* Carico la tabella VPO per vedere da quando cambia il calcolo
050300070522     c*  della affidabilità partenza
050400070522     C                   MOVEL     'VPO'         kTBECOD
050500070522     C                   MOVEL(P)  'DECO'        kTBEKE1
050600070522     C     KTBE          CHAIN(N)  TNTBE01L                           21
050700070522     C  N21              MOVEL     TBEUNI        DVPODECO
050800070522     C*
050900070522     c
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
053200090605     c                   enddo
053300090605
053400000000      *
053500000000      * Se la data AL e' vuota la imposto come la data DAL
053600950210     C     DS1AAL        IFEQ      *ZERO
053700950210     C                   Z-ADD     DS1ADA        DS1AAL
053800950210     C                   Z-ADD     DS1GDA        DS1GAL
053900000000     C                   ENDIF
054000940428     C*
054100940428     C                   SETON                                        43
054200940401      *
054300950210      * Accendo un indicatore se non devo stampare il tabulato (DS1RIS=' ')
054400950210     C     DS1RIS        COMP      *BLANKS                                38
054500940429      *
054600940429      * 39 ON - Immessa l'area
054700950210    3C     DS1FEL        IFGT      0
054800940429     C                   SETON                                        39
054900940429    3C                   ENDIF
055000000000      *
055100950210      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
055200950210     C                   EXSR      COMUNE
055300950210      *
055400000000     C                   SETON                                        LR
055500020212     C*****************************************************************
055600020212     C*  PARTE COMUNE STAMPE GIORNALIERE
055700020212     C*
055800020212     C*  . REPERIMENTO 'SIMFEL'
055900020212     C*  . CARICAMENTO '£1'
056000020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
056100020212     C*  . CODA DI STAMPA ('£OUTQ')
056200020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
056300020212     C*  . MODULO PER LA STAMPA ('£MOD')
056400020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
056500020212     C*****************************************************************
056600020212     C     COMUNE        BEGSR
056700020212     C*
056800020212     C*****************************************************************
056900020212     C*       RIEPILOGO INDICATORI:
057000020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
057100020212     C* 11    -           STAMPA PER 1L
057200020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
057300020212     C*****************************************************************
057400020212     C                   Z-ADD     1             CODUT
057500020212     C*
057600020212     C* Reperimento parametri aziendali
057700020212     C                   CALL      'X§PARUT'
057800020212     C                   PARM                    UT§DSE
057900020212     C                   MOVEL     RAGUT         RSUT             20
058000020212     C                   MOVEL     REC80         CNCR80
058100020212     C*
058200020212     C* Chiave di accesso per il file tabelle
058300020212     C     KTAB1         KLIST
058400020212     C                   KFLD                    CODUT
058500020212     C                   KFLD                    KCOD
058600020212     C                   KFLD                    KKEY
058700020212     C     KTAB2         KLIST
058800020212     C                   KFLD                    CODUT
058900020212     C                   KFLD                    KCOD
059000020212     C*
059100020813     ***C     *LIKE         DEFINE    TBLCOD        KCOD
059200020813     ***C     *LIKE         DEFINE    TBLKEY        KKEY
059300020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
059400020212     C*
059500020212     C* DETERMINO DATA E ORA DI ESECUZIONE
059600020212     C                   TIME                    WTIME            14 0
059700020212     C                   MOVE      WTIME         WDATE             8 0
059800020212     C                   MOVEL     WTIME         UTIME             6 0
059900040921     c*
060000040921     C                   CLEAR                   WLBDAT
060100040921     C                   MOVE      Wtime         G02DAT
060200040921     C                   CALL      'XSRDA8'
060300040921     C                   PARM                    WLBDAT
060400040921     C                   Z-ADD     G02INV        dateu             8 0
060500040922     c*
060600040922     c                   clear                   $FTR
060700040922     c                   z-add     dateu         $DTR
060800020212     C*
060900020418     C* CARICO '£1'
061000020212     C                   CLEAR                   KPJBU
061100020212     C                   CLEAR                   DSUL06
061200020212     C                   MOVE      '£1'          D06COD
061300020212     C                   MOVEL     SIMFEL        D06KEY
061400050802     C                   MOVE      'S'           D06KEY
061500050802     C                   MOVEL     ds1ada        D06drf
061600020212     C                   MOVEL     DSUL06        KPJBU
061700020212     C                   CALL      'TRUL06R'
061800020212     C                   PARM                    KPJBA
061900020212     C                   MOVEL     KPJBU         DSUL06
062000020418     c                   movea     t06           sav_t06
062100020418
062200020418      * se in arrivo carico la £6
062300020418 b1  c                   if        ds1tbo = 'A'
062400020418     c                   clear                   wc
062500020418 b2  c                   do        30            wa
062600020422     c                   if        sav_t06(wa) = *zeros
062700020422     c                   leave
062800020422     c                   endif
062900020418     c                   clear                   kpjbu
063000020418     c                   clear                   dsul06
063100020418     c                   eval      d06cod = '£6'
063200020418     c                   movel     sav_t06(wa)   d06key
063300050802     C                   MOVE      'S'           D06KEY
063400020418     c                   eval      d06esc = 'S'
063500050802     c                   movel     ds1ada        d06drf
063600020419     c                   movel     dsul06        kpjbu
063700020418     c                   call      'TRUL06R'
063800020418     c                   parm                    kpjba
063900020418     c                   movel     kpjbu         dsul06
064000020418 b3  c                   do        30            wb
064100020418     c                   if        t06(wb) = *zeros
064200020418     c                   leave
064300020418     c                   endif
064400020418     c                   add       1             wc
064500020418     c                   z-add     t06(wb)       l6(wc)
064600020418 e3  c                   enddo
064700020418 e2  c                   enddo
064800020212     C*
064900020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
065000020212     C                   Z-ADD     *ZERO         WB
065100020418 b2  C                   DO        29            WA
065200020418     C     sav_T06(WA)   LOOKUP    L6                                     30
065300020212     C  N30              ADD       1             WB
065400020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
065500020418 e2  C                   ENDDO
065600020418 x1  C                   ELSE
065700020418     C                   MOVEA     sav_T06       L1
065800020418 e1  C                   ENDIF
065900020212     C*
066000020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
066100020212     C                   MOVEA     L1            LIN
066200020212     C                   CLEAR                   L1
066300020212     C                   Z-ADD     *ZERO         WB                3 0
066400020212     C     1             DO        30            WPT               3 0
066500020212     C     LIN(WPT)      IFNE      0
066600020212     C                   CLEAR                   FNLSC4
066700020212     C                   Z-ADD     LIN(WPT)      IC4LIN
066800020212     C                   MOVE      'B'           IC4FLG
066900020212     C                   MOVEL     FNLSC4        KPJBU
067000020212     C                   CALL      'FNLSC4R1'
067100020212     C                   PARM                    KPJBA
067200050802     C                   PARM                    ds1ada
067300020212     C                   MOVEL     KPJBU         FNLSC4
067400020212     C     OC4ESE        IFEQ      'S'
067500020212     C                   ADD       1             WB
067600020212     C                   MOVE      LIN(WPT)      L1(WB)
067700020212     C                   ENDIF
067800020212     C                   ENDIF
067900020212     C                   ENDDO
068000070103     C*
068100070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
068200070103     C                   Z-ADD     *ZERO         WB                3 0
068300070103     C                   DO        29            WA                3 0
068400070103     C     L1(WA)        IFNE      *ZEROS
068500070103     C     L1(WA)        ANDNE     SIMFEL
068600070103     C                   ADD       1             WB
068700070103     C                   Z-ADD     L1(WA)        LV2(WB)
068800070103     C                   ENDIF
068900070103     C                   ENDDO
069000020212     C*
069100020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
069200020212     C*
069300020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
069400020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
069500020212     C* la stampa e' lanciata da menu
069600020212     C                   MOVEL     '3L'          KCOD
069700020212     C                   MOVEL     DS1PGM        KKEY
069800020212     C                   MOVE      DS1FPG        KKEY
069900020212     C     KTAB1         CHAIN     TABEL00F                           30
070000020212     C  N30              MOVEL     TBLUNI        DS3L
070100020212     C   30              MOVEL     'F'           §3LD1L
070200020212     C   30              MOVEL     ' '           §3LD2L
070300020212     C   30              MOVEL     '*STD    '    §3LDST
070400020212     C*
070500020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
070600020212     C                   MOVEL     §3LDST        £MOD             10
070700020212     C                   CLEAR                   LIN
070800020212     C*
070900020212     C* Verifico chi ha lanciato la stampa
071000020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
071100020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
071200020212     C* DS1FGS<>0  --->  si proviene da menu
071300020212     C     DS1FGS        IFEQ      0
071400020212     C*
071500020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
071600020212     C* lancio, voglio la stampa;  invece
071700020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
071800020212     C*
071900020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
072000020212     C* creare stampa per 1°lvl. (se stessa)
072100020212     C     DS1LNP        IFEQ      SIMFEL
072200020212     C*
072300020212     C* Il flag §3LD1L ha i seguenti significati:
072400020212     C*    ' ' = tutti i dati
072500020212     C*    '1' = solo filiale di 1' livello
072600020212     C     §3LD1L        IFEQ      '1'
072700020212     C                   SETON                                        11
072800020212     C                   END
072900020212     C*
073000020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
073100020212     C* nella stampa
073200020212     C*
073300020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
073400020212     C  N11              MOVEA     L1            LIN
073500020212     C   11              MOVEA     SIMFEL        LIN(1)
073600020212     C*
073700020212     C* IMPOSTO LA STAMPANTE:
073800020212     C* STAMPANTE TABULATI
073900020212     C                   MOVEL     '5P'          KCOD
074000020212     C                   MOVEL     'T'           W004A             4
074100020212     C                   MOVE      LIN(1)        W004A
074200020212     C                   MOVEL(P)  W004A         KKEY
074300020212     C     KTAB1         CHAIN     TABEL00F                           99
074400020212     C     *IN99         IFEQ      *OFF
074500020212     C                   MOVEL     TBLUNI        DS5P
074600020212     C                   MOVEL     §5POTQ        £OUTQ            10
074700020212     C                   END
074800020212     C*
074900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
075000020212     C                   EXSR      LEGGI
075100020212     C*
075200020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
075300020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
075400020212     C                   ELSE
075500020212     C*
075600020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
075700020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
075800020212     C*
075900020212     C* Il significato del fla §3LD2L e' il seguente:
076000020212     C*    ' ' = tutti i dati
076100020212     C*    'F' = per ogni 2' livello
076200020212     C     §3LD2L        IFEQ      'F'
076300020212     C                   SETON                                        14
076400020212     C                   ENDIF
076500020212     C*
076600020212     C                   Z-ADD     1             WB
076700020212     C                   MOVEA     *ZEROS        LIN
076800020212     C  N14              MOVEA     L1            LIN
076900020212     C*
077000020212     C* Lancio per tutti i 2° lvl
077100020212     C     DS1LNP        IFEQ      0
077200020212     C*
077300020212     C* LV2 contiene tutte le filiali di 2° lvl
077400020212     C     LV2(WB)       DOWNE     0
077500020212      *
077600020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
077700020212     C                   CLEAR                   FNLSC4
077800020212     C                   Z-ADD     LV2(WB)       IC4LIN
077900020212     C                   MOVEL     FNLSC4        KPJBU
078000020212     C                   CALL      'FNLSC4R1'
078100020212     C                   PARM                    KPJBA
078200050802     C                   PARM                    ds1ada
078300020212     C                   MOVEL     KPJBU         FNLSC4
078400020212      *
078500020212     C     OC4PT1        IFEQ      'S'
078600020212     C     §3LPT         ANDEQ     'N'
078700020212     C                   GOTO      NOLEGG
078800020212     C                   ENDIF
078900020212      *
079000020212     C     OC4EXP        IFEQ      'S'
079100020212     C     §3LEXP        ANDEQ     'N'
079200020212     C                   GOTO      NOLEGG
079300020212     C                   ENDIF
079400020212      *
079500020212     C     OC4DPD        IFEQ      'S'
079600020212     C     §3LDPD        ANDEQ     'N'
079700020212     C                   GOTO      NOLEGG
079800020212     C                   ENDIF
079900020212      *
080000020212     C     OC4BAR        IFEQ      'S'
080100020212     C     §3LBAR        ANDEQ     'N'
080200020212     C                   GOTO      NOLEGG
080300020212     C                   ENDIF
080400020212      *
080500020212     C     OC4FED        IFEQ      'S'
080600020212     C     §3LFED        ANDEQ     'N'
080700020212     C                   GOTO      NOLEGG
080800020212     C                   ENDIF
080900020212      *
081000020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
081100020212     C*  dati degli altri 2° lvl
081200020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
081300020212     C   14              MOVEL     LV2(WB)       LIN(1)
081400020212     C*
081500020212     C* IMPOSTO LA STAMPANTE
081600020212     C* STAMPANTE TABULATI
081700020212     C                   MOVEL     '5P'          KCOD
081800020212     C                   MOVEL     'T'           W004A             4
081900020212     C                   MOVE      LV2(WB)       W004A
082000020212     C                   MOVEL(P)  W004A         KKEY
082100020212     C     KTAB1         CHAIN     TABEL00F                           99
082200020212     C     *IN99         IFEQ      *OFF
082300020212     C                   MOVEL     TBLUNI        DS5P
082400020212     C                   MOVEL     §5POTQ        £OUTQ            10
082500020212     C                   END
082600020212     C*
082700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
082800020212     C                   EXSR      LEGGI
082900020212     C*
083000020212     C     NOLEGG        TAG
083100020212     C*
083200020212     C                   ADD       1             WB
083300020212     C                   ENDDO
083400020212     C*
083500020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
083600020212     C                   ELSE
083700020212     C*
083800020212     C* Lancio solo per la 2° lvl richiesta
083900020212     C   14              MOVEL     DS1LNP        LIN(1)
084000020212     C*
084100020212     C* STAMPANTE
084200020212     C* STAMPANTE TABULATI
084300020212     C                   MOVEL     '5P'          KCOD
084400020212     C                   MOVEL     'T'           W004A             4
084500020212     C                   MOVE      DS1LNP        W004A
084600020212     C                   MOVEL(P)  W004A         KKEY
084700020212     C     KTAB1         CHAIN     TABEL00F                           99
084800020212     C     *IN99         IFEQ      *OFF
084900020212     C                   MOVEL     TBLUNI        DS5P
085000020212     C                   MOVEL     §5POTQ        £OUTQ            10
085100020212     C                   END
085200020212     C*
085300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
085400020212     C                   EXSR      LEGGI
085500020212     C*
085600020212     C                   ENDIF
085700020212     C                   ENDIF
085800020212     C*
085900020212     C                   ELSE
086000020212     C*
086100020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
086200020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
086300020212     C     DS1LNP        IFEQ      0
086400020212     C                   MOVEA     L1            LIN
086500020212     C* ...altrimenti elaboro solo se stessa
086600020212     C                   ELSE
086700020212     C                   MOVEL     DS1LNP        LIN(1)
086800020212     C                   ENDIF
086900020212     C*
087000020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
087100020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
087200020212     C* PRENDO QUELLA PRESENTE IN KPJBA
087300020212     C     DS1FGS        IFEQ      SIMFEL
087400020212     C                   MOVEL     KQOTD         £OUTQ
087500030627     C**!!!              MOVEL     KQOTD         £OUTQF
087600020212     C                   ELSE
087700020212     C* STAMPANTE TABULATI
087800020212     C                   MOVEL     '5P'          KCOD
087900020212     C                   MOVEL     'T'           W004A             4
088000020212     C                   MOVE      DS1FGS        W004A
088100020212     C                   MOVEL(P)  W004A         KKEY
088200020212     C     KTAB1         CHAIN     TABEL00F                           99
088300020212     C     *IN99         IFEQ      *OFF
088400020212     C                   MOVEL     TBLUNI        DS5P
088500020212     C                   MOVEL     §5POTQ        £OUTQ            10
088600020212     C                   END
088700020212     C                   ENDIF
088800020212     C*
088900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
089000020212     C                   EXSR      LEGGI
089100020212     C                   ENDIF
089200020212     C*
089300020212     C                   ENDSR
089400000000      **************************************************************************
089500000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
089600000000      **************************************************************************
089700000000     C     LEGGI         BEGSR
089800940502     C                   SETOFF                                       2729
089900000619     C                   SETON                                        43
090000000616     C                   CLEAR                   SAVRIS
090100970212     C*
090200970212      * Cancello dal file statistiche i record della giornata che vado a elabor.
090300970212      * Solo se richiesta elaborazione
090400970212    1C     DS1RIS        IFNE      'R'
090500000616     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
090600000616     C                   CLEAR                   KPJBU
090700000616     C                   CLEAR                   DSUL06
090800000616     C                   MOVE      '£1'          D06COD
090900000616     C                   MOVEL     SIMFEL        D06KEY
091000050802     C                   MOVE      'S'           D06KEY
091100050802     c                   movel     ds1ada        d06drf
091200000616     C                   MOVEL     DSUL06        KPJBU
091300000616     C                   CALL      'TRUL06R'
091400000616     C                   PARM                    KPJBA
091500000616     C                   MOVEL     KPJBU         DSUL06
091600000616     C**
091700000616     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
091800000616     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
091900000616     C*  CHE USO ALLA FINE
092000000616     C     DS1RIS        IFNE      ' '
092100000616     C                   MOVEA     LIN           SVLIN
092200000616     C                   MOVEL     DS1RIS        SAVRIS
092300000616     C                   CLEAR                   DS1RIS
092400000616     C                   ENDIF
092500000616     C*
092600000616     C                   MOVEA     T06           LIN
092700000616     C*
092800970212     C                   OPEN      FNSAT04L
092900970212     C                   Z-ADD     1             XX                3 0
093000970212    2C     LIN(XX)       DOWGT     *ZERO
093100970212     C*
093200970212     C     FNSATK        SETLL     FNSAT04L
093300970212     C     FNSATK        READE     FNSAT04L                               99
093400970212     C*
093500970212    3C     *IN99         DOWEQ     *OFF
093600970212     C                   DELETE    FNSAT004
093700970212     C     FNSATK        READE     FNSAT04L                               99
093800970212    3C                   ENDDO
093900970212     C                   ADD       1             XX
094000970212    2C                   ENDDO
094100970212     C*
094200970212     C                   CLOSE     FNSAT04L
094300970212     C                   END
094400940505      *
094500940505      * Come prima cosa devo verificare se giornata festiva
094600940505      *   solo per ELABORAZIONE o  per stampe giornaliere
094700950210     C     DS1FGS        IFEQ      0
094800950210     C     DS1RIS        ORNE      'R'
094900940505     C                   CLEAR                   $TFA
095000940505      *
095100950118     C     AZCLNK        CHAIN     AZCLN01L                           99
095200940505     C     *IN99         IFEQ      *OFF
095300950118     C                   MOVEA     CLNMAT        GME
095400970211     C     GME($GI)      IFEQ      'F'
095500970211     C* SE FESTIVO SCRIVO RECORD IN FNSAT CON TRE = F
095600970211     C                   CLEAR                   FNSAT000
095700970211     C                   Z-ADD     SIMFEL        SATFLE
095800970211     C                   MOVEL     'F'           SATTRE
095900970211     C                   Z-ADD     DS1ADA        SATDRE
096000040922     c                   movel     $ftr          satftr
096100040922     c                   z-add     $DTR          satdtr
096200970311     C                   WRITE     FNSAT000                             99
096300970211     C                   GOTO      ENDLEG
096400970211     C                   ENDIF
096500940505     C                   ENDIF
096600940505     C                   ENDIF
096700940429      *
096800940429     C* LANCIO DA FILTRO
096900950210    1C     DS1FGS        IFGT      0
097000940429     C* 29 ON - IMMESSA LINEA DI PARTENZA
097100950210    2C     DS1LNP        IFGT      0
097200940429     C                   SETON                                        29
097300950210    3C     DS1LNP        IFNE      DS1FEL
097400940502     C                   SETON                                        27
097500940429    3C                   ENDIF
097600940429    2C                   ENDIF
097700940429     C*
097800940429   X1C                   ELSE
097900940502     C                   SETON                                        05
098000940506     C                   CLEAR                   STRIP
098100940429     C* STAMPE GIORNALIERE
098200940429    2C     LIN(2)        IFEQ      0
098300940429     C                   SETON                                        29
098400940429    2C                   ENDIF
098500940429     C     SIMFEL        LOOKUP    LIN                                    99
098600940502     C  N99              SETON                                        27
098700940429    1C                   ENDIF
098800000912     C* SE SONO TUTTI P.O. POSTE, NON ENTRO NEMMENO NEL CICLO DI LETT
098900000912     C*   E RISTAMPA
099000000912     C                   Z-ADD     1             $X
099100000912     C                   CLEAR                   WSTAF
099200000912    1C     LIN($X)       DOWGT     0
099300000912     C     WSTAF         ANDEQ     ' '
099400000912     C                   Z-ADD     LIN($X)       $FILPR
099500000912     C* ESCLUDO I P.O. POSTE
099600020813     ***C                   CLEAR                   §OGPT
099700020813     C                   clear                   OG143
099800000912     C     $FILPR        CHAIN     AZORG01L                           82
099900000912     C  N82              MOVEL     ORGDE3        OG143
100000000912     C**
100100020813    2***C     §OGPT         IFNE      'S'
100200020813    2C                   IF        §OGntw <> 'PPT'
100300000912     C* SE ELABORATA ALMENO UNA LINEA, STAMPO ANCHE I TOTALI FINALI
100400000912     C                   MOVEL     'S'           WSTAF             1
100500000912    2C                   ENDIF
100600000912     C                   ADD       1             $X
100700000912    1C                   ENDDO
100800000000      *
100900000000      *
101000000912    1C     WSTAF         IFEQ      'S'
101100000000      * Scrivo, in stampa, se selezionata filiale o area
101200000000     C                   MOVE      *BLANKS       STRIP1
101300950210     C  N05DS1LNP        IFNE      *ZERO
101400960925     C                   MOVEL     'P.O.'        STRIP1
101500950210     C                   MOVE      DS1LNP        STRIP1
101600000000     C                   ELSE
101700940502     C                   MOVEL     'AREA'        STRIP1
101800950210     C                   MOVE      DS1FEL        STRIP1
101900000000     C                   ENDIF
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
104500000619     C     SAVRIS        IFNE      ' '
104600000619     C                   SETON                                        38
104700000619     C                   ENDIF
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
105900000912      * Escludo bolle LNP Poste
106000020813     ***C                   CLEAR                   §OGPT
106100020813     C                   clear                   OG143
106200000912     C     $FILPR        CHAIN     AZORG01L                           82
106300000912     C  N82              MOVEL     ORGDE3        OG143
106400000912     C**
106500020813   2A***C     §OGPT         IFNE      'S'
106600020813    2C                   IF        §OGntw <> 'PPT'
106700940428     C                   CLEAR                   $DFV
106800940428     C                   SETOFF                                           01
106900940428      *
107000940428    3C                   DO        2
107100021023     C     FNBLTK        SETLL     FNBLT38L
107200021023     C  N01FNBLTK        READE     FNBLT38L                               33
107300021023     C   01LIN($X)       READE     FNBLT38L                               33
107400940428     C*
107500940428    4C     *IN33         DOWEQ     *OFF
107600000000      *
107700000000      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
107800000000      * come test ma mi permette di saltare delle chain)
107900940428    5C     $BLTAT        IFNE      $BLTPR
108000950118     C     FNBLPK        CHAIN     FNBLP01L                           99
108100000000     C   99              GOTO      LETTUR
108200000000     C                   MOVE      $BLTAT        $BLTPR
108300000000     C                   Z-ADD     BLPNFV        $NFVPR
108400950118     C                   Z-ADD     BLPMGS        $DATSP            8 0
108500000000     C                   MOVEL     BLPAAS        $DATSP
108600000331     C                   Z-ADD     BLPDRT        $DTRIT
108700000413     C                   Z-ADD     BLPPDR        $CDPDR
108800940428     C* $BOLLA = 1 INDICA CHE HO LETTO UNA NUOVA BOLLA
108900940428     C                   MOVEL     '1'           $BOLLA            1
109000000000      *
109100000000      * Decodifico il tipo bolla
109200950118    6C     BLPCBO        IFNE      $CBLPR
109300950210     C                   MOVE      '3A'          KCOD
109400950210     C                   MOVE      *BLANKS       KKEY
109500950210     C                   MOVEL     BLPCBO        KKEY
109600950210     C     KTAB1         CHAIN     TABEL00F                           99
109700000000     C  N99              MOVEL     TBLUNI        DS3A
109800950118     C  N99              MOVE      BLPCBO        $CBLPR
109900000000     C   99              CLEAR                   DS3A
110000940428    6C                   ENDIF
110100940427     C*
110200940428    5C                   ENDIF
110300000000      *
110400940428      * ESCLUSIONI BOLLE
110500000000     C                   EXSR      ESCLUS
110600940428    5C     $OK           IFEQ      'S'
110700000000     C                   EXSR      ELABOR
110800940428    5C                   ENDIF
110900000000      *
111000940428     C     LETTUR        TAG
111100021023     C  N01FNBLTK        READE     FNBLT38L                               33
111200021023     C   01LIN($X)       READE     FNBLT38L                               33
111300940428    4C                   ENDDO
111400940428      *
111500940428     C                   SETON                                        01
111600970205     C                   Z-ADD     DS1AAL        $DFV
111700940428    3C                   ENDDO
111800940428      *
111900940428      * cambia la filiale --> decodifico e stampo i totali
112000940428     C                   EXSR      CALSPE
112100940428     C                   EXSR      WRISAT
112200940429     C                   EXSR      WR1SAT
112300940428     C  N38              EXSR      TOTFIL
112400940428     C                   EXSR      AZZFIL
112500000912   2AC                   ENDIF
112600940428     C*
112700940401     C                   ADD       1             $X
112800940428    2C                   ENDDO
112900940429      *
113000940429     C*
113100940429     C* STAMPA DEI DATI FINALI
113200940506     C                   EXSR      STAFIN
113300000616   X C                   ENDIF
113400000616      *
113500000616     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
113600000616     C     SAVRIS        IFEQ      'S'
113700000616     C                   MOVEL     'R'           DS1RIS
113800000616     C                   MOVEA     SVLIN         LIN
113900000616     C* AZZERO LE SCHIERE DI TOTALE
114000000616     C                   EXSR      AZZSER
114100000616     C                   EXSR      AZZTRA
114200000616     C                   EXSR      AZZTOT
114300000619     C                   SETOFF                                       38
114400000616     C                   ENDIF
114500000000      *
114600970211      * Nel caso di RISTAMPA mi limito a rileggere il file statistiche (FNSAT)
114700000000      * e stampare i record che rientrano nell'intervallo dal-al
114800000616     C     DS1RIS        IFNE      ' '
114900000000     C                   EXSR      RISTAM
115000940428    1C                   ENDIF
115100000000      *
115200000000      * Stampa terminata
115300940428     C     *IN38         IFEQ      *OFF
115400940429     C*
115500940428     C     $ALMEN        IFEQ      *ZERO
115600950118     C                   WRITE     LSC2E1
115700940505     C                   ELSE
115800940429     C*
115900000000     C                   EXSR      STATER
116000940505     C                   ENDIF
116100000000      *
116200950118     C                   WRITE     LSC2E2
116300950629     C                   WRITE     LSC2E5
116400000000      *
116500000000      * Verifico se vi sono fogli viaggio aperti
116600000000     C                   EXSR      FOGVIA
116700000000      *
116800970807     C                   CLOSE     FNLSC2P
116900940428     C                   ENDIF
117000000000      *
117100000000      * Aggiorno la data ultima elaborazione
117200950210     C     DS1RIS        IFNE      'R'
117300000616     C     SAVRIS        OREQ      'S'
117400021016
117500021016     c                   Eval      kTbeCod = 'SDF'
117600021016     c                   Movel(p)  Simfel        kTbeKe1
117700021016     c     Ktbe          Chain     Tntbe01l
117800021016     c                   If        %Found(Tntbe01l)
117900021016     c                   Movel     TbeUni        dSdf
118000021016     c                   If        §SdfTerP = *Zeros or §SdfTerP > Ds1Ada
118100021016     c                   Z-Add     Ds1Ada        §SdfTerP
118200021016     c                   EndIf
118300021016     c                   If        §SdfTerU < Ds1Ada
118400021016     c                   Z-Add     Ds1Ada        §SdfTerU
118500021016     c                   EndIf
118600021016     c                   Movel     dSdf          TbeUni
118700021016     c                   Update    Tntbe000
118800021016     c                   EndIf
118900000000     C                   ENDIF
119000090415     c
119100090415     c* Se non ho aggioranto la tabella, vedo se devo cancellare record
119200090415     c*  in FISTA
119300090415     c                   if        savris='S'
119400090415     c     ksta          delete    fista01l
119500090415     C                   ENDIF
119600000912     C*
119700000912    1C                   ENDIF
119800000000      *
119900940505     C     ENDLEG        ENDSR
120000000000      **************************************************************************
120100000000      * Una volta letto il record verifico se da includere o no
120200940427      * PER QUALSIASI TIPO DI CALCOLO  (COLLI E BOLLE A TERRA E
120300940427      *  COLLI E BOLLA DA PARTIRE)
120400000000      *
120500940427      * Vi sono solo due motivi per escludere il record letto:
120600940427      * (1) se la spedizione e' stata bollettata dopo la data elaboraz
120700950210      *                Data spedizione > DS1AAL
120800000000      *
120900940427      * (2) se la data partenza del collo e' < della data elaborazione
121000950210      *                BLTDFV < DS1AAL
121100000000      *
121200000810      * ATTENZIONE.. DA OGGI C'E' UN MOTIVO IN PIU'.. Canale5 ! da cantare ..
121300000810      *
121400000810      * (3) se la linea di partenze è ??..... POSTE !
121500000810      *
121600940427      * Bolle di recupero / bolle legate / bolle "£" sono escluse
121700000000      **************************************************************************
121800000000     C     ESCLUS        BEGSR
121900000000      *
122000000000     C                   MOVE      'N'           $OK               1
122100000331     C                   MOVE      *BLANKS       WSOS              1
122200000810      *
122300000810      *
122400940427      * esclusioni:
122500950323      * Le bolle "legate" sono identificabili dal nr.foglio viaggio
122600950323      * = 99999 O = 99998
122700070522    1C     $NFVPR        IFEQ      99999
122800950323     C     $NFVPR        OREQ      99998
122900940427     C     $CBLPR        OREQ      '£'
123000940428     C     §3ARBL        OREQ      'R'
123100000000     C                   GOTO      ENDESC
123200070522    1C                   ENDIF
123300000413     C* Prepagati non confermati
123400070522    1C     $CDPDR        IFEQ      *ZEROS
123500000413     C                   GOTO      ENDESC
123600070522    1C                   ENDIF
123700000000      *
123800070522    1C     BLTDFV        IFEQ      DS1AAL
123900950629     C                   MOVEL     'S'           $OK
124000070522   x1C                   ELSE
124100070522     C
124200070522     c* Elaborazione in base A     Data BORDERÒ
124300070522   1AC                   IF        DS1AAL<§VPOnpi
124400070522     C
124500940427      * La data di spedizione deve essere <= alla data elaborazione
124600070522    2C     $DATSP        IFLE      DS1AAL
124700000000      *
124800940427      * La data partenza deve essere > = alaa data elaborazione        limiti
124900070522    3C     BLTDFV        IFEQ      *ZERO
125000950210     C     BLTDFV        ORGE      DS1AAL
125100000000     C                   MOVE      'S'           $OK
125200000000     C                   GOTO      ENDESC
125300070522    3C                   ENDIF
125400070522   X2C                   ELSE
125500000331     C* Se data spedizione > di data elaborazione
125600000411     C* e  data ritiro    <= data elaborazione    ==> BOLLA IN SOSPENS.
125700070522    3C     $DTRIT        IFLE      DS1AAL
125800000331     C                   MOVE      'S'           $OK
125900000331     C                   MOVEL     'S'           WSOS
126000070522     C                   GOTO      ENDESC
126100070522    3C                   ENDIF
126200950626     C*
126300070522    2C                   ENDIF
126400070522     c
126500070522  x1ac                   else
126600070522     c****
126700070522     c* ELAborazione in base a    data RITIRO Merce
126800070522      * La data ritiro deve essere <= alla data elaborazione
126900070522    2C     $DTRIT        IFLE      DS1AAL
127000070522      *
127100070525      * La data partenza deve essere > = alla data elaborazione        limiti
127200070522    3C     BLTDFV        IFEQ      *ZERO
127300070522     C     BLTDFV        ORGE      DS1AAL
127400070522     C                   MOVE      'S'           $OK
127500070522     C                   GOTO      ENDESC
127600070522    3C                   ENDIF
127700070522    2C                   ENDIF
127800070522   1aC                   ENDIF
127900950629     C*
128000070522    1C                   ENDIF
128100000000      *
128200000000     C     ENDESC        ENDSR
128300000000      **************************************************************************
128400000000      * Elaborazioni dei record letti, somme nei totali
128500000000      **************************************************************************
128600000000     C     ELABOR        BEGSR
128700000000      *
128800970210      * Verifico il tipo spedizione ESPR./DIST./CORR.
128900950201     C                   Z-ADD     3             $T                1 0
129000000000     C     BLPTSP        IFEQ      'E'
129100000000     C                   Z-ADD     1             $T
129200000000     C                   ENDIF
129300950201     C     BLPTSP        IFEQ      'D'
129400000619     C     BLPTSP        OREQ      'P'
129500940323     C                   Z-ADD     2             $T
129600000000     C                   ENDIF
129700000000      *
129800000000      * C/servizio oppure no ?
129900940428     C     §3ARBL        COMP      'C'                                3434
130000000000      *
130100970205     C*
130200070522    1C     WSOS          IFEQ      *BLANKS
130300970205     C* DETERMINAZIONE SE COLLO A TERRA
130400000000      * Ho due controlli da fare per sapere se il collo e' a terra:
130500000000      *    1)  BLTDFV = 0
130600950210      *    2)  BLTDFV > Data spedizione   AND   BLTDFV > DS1AAL
130700000000     C                   SETOFF                                       36
130800070522    2C     BLTDFV        IFEQ      *ZERO
130900950210     C     BLTDFV        ORGT      DS1AAL
131000000000     C                   SETON                                        36
131100070522    2C                   ENDIF
131200970303     C*
131300970303     C* RICHIAMO PGM PER CALCOLO AFFIDABILITA'
131400970303     C* (SOLO SE HO LETTO UNA NUOVA BOLLA - $BOLLA = '1')
131500070522    2C     $BOLLA        IFEQ      '1'
131600070522    3C     BLPDCE        IFEQ      *ZEROS
131700970303     C     BLPDUC        ORNE      DS1AAL
131800970303     C                   MOVEL     ' '           D98TLA
131900970303     C                   MOVEL     'P'           D98TBO
132000970303     C                   MOVEL     BLPAAS        D98AAS
132100970303     C                   MOVEL     BLPLNP        D98LNP
132200970303     C                   MOVEL     BLPNRS        D98NRS
132300970303     C                   MOVEL     BLPNSP        D98NSP
132400970310     C                   Z-ADD     DS1AAL        D98DRF
132500970310     C                   Z-ADD     2359          D98ORF
132600970925     C                   CALL      'TNSD99R'
132700970925     C                   PARM                    DS99
132800970303     C                   ELSE
132900970303     C                   Z-ADD     BLPNPI        D98NPI
133000070522    3C                   END
133100070522    2C                   END
133200970207     C*
133300970207     C*VERIFICO SE SI TRATTA DI "A TERRA DA PARTIRE"
133400970210     C* La considero "non da partire" se si tratta di:
133500970207     C* - spedizione non borderizzata
133600970303     C* - distribuzione a terra del giorno
133700070522     c* - dal 1° giugno 2007 il calcolo della affidabilità a terra sarà
133800070522     c*   in base alla data ritiro, per cui nel DA PARTIRE non controllo
133900070522     c*   più la data borderò ma solo se si tratta della distribuzione
134000070522     c*   del giorno
134100970205     C                   SETOFF                                       35
134200070522     c
134300070522    2C     *IN36         IFEQ      *ON
134400070522     c
134500070522    3c                   if        ds1aal<§vponpi
134600970210     C*
134700070522    4C     BLPDBR        IFGT      DS1AAL
134800070522     C     BLPDBR        OREQ      *ZEROS
134900070522     C                   SETON                                        35
135000070522   x4C                   ELSE
135100970304     C*
135200070522    5C     BLPDBR        IFLE      DS1AAL
135300070522     C     BLPDPC        ANDEQ     0
135400070522     C     D98NPI        ANDLT     0
135500070522     C*
135600070522     C     BLPDBR        ORLE      DS1AAL
135700070522     C     BLPDPC        ANDGT     DS1AAL
135800070522     C     D98NPI        ANDLT     0
135900070522     C                   SETON                                        35
136000070522    5C                   END
136100070522    4C                   END
136200070522     c
136300070522   x3c                   else
136400070522     c*
136500090108     C* SE affidabilità =0 verifico se cade in giorno festivo, per
136600090108     c*    ter-ter --> se si non do ritardo
136700090108     c                   if        d98npi=0
136800090108     C                   movel     blptfa        $TFA
136900090108      *
137000090108     C     AZCLNK        CHAIN     AZCLN01L                           99
137100090108     C                   MOVEA     CLNMAT        GME
137200090108     C                   MOVEA     CLNpom        GMEp
137300090108     C     GME($GI)      IFEQ      'F'
137400090108     C     GMEp($GI)     oreq      'F'
137500090108     c                   sub       24            d98npi
137600090108     c                   endif
137700090108     c                   endif
137800090108     c
137900070522    4c                   if        (blpdpc=0 and d98npi<0) or
138000070522     c                             (blpdpc>ds1aal and d98npi<0)
138100070522     C                   SETON                                        35
138200070522    4c                   endif
138300070522    3c                   endif
138400970210     C*
138500070522    2C                   END
138600070522    1C                   ENDIF
138700970205     C*
138800000000      *
138900970205      * Aggancio, nel file statistiche, il record per il terminal di
139000970205      * arrivo poiche' lo dovrò aggiornare
139100970205     C                   CLEAR                   FNSAT000
139200000000     C                   Z-ADD     SIMFEL        SATFLE
139300950210     C                   Z-ADD     DS1AAL        SATDRE
139400940428     C                   Z-ADD     BLPLNP        SATLNP
139500000000     C                   Z-ADD     BLPTFA        SATTFA
139600000000     C                   MOVE      *BLANK        SATTRE
139700970205     C     FNSA1K        CHAIN     FNSAT02L                           37
139800940428      *
139900970205      * NUOVA BOLLA LETTA: CONTO LE SPEDIZIONI
140000940428      *
140100940428     C     $BOLLA        IFEQ      '1'
140200940428     C                   EXSR      DATBOL
140300970205     C                   ENDIF
140400021217      *
140500021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
140600030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
140700030213      * Peso medio
140800030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
140900030213if  1c***                if        Wsipes = 'S'
141000030213     c***                z-add     BLTpuc        $Peso
141100030213   x1c***                else
141200021220     c                   z-add     Wpesmed       $Peso
141300030213e   1c***                endif
141400021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
141500030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
141600030213      * Volume medio
141700030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
141800030213if  1c***                if        Wsivol = 'S'
141900030213     c***                z-add     BLTvuc        $Volum
142000030213   x1c***                else
142100021220     c                   z-add     Wvolmed       $Volum
142200030213e   1c***                endif
142300000000      *
142400000000      * ?  CALCOLI  SUI  COLLI  ?
142500000000      *
142600000331     C     WSOS          IFEQ      *BLANKS
142700000000      * Nr.colli da partire (servira' per la percentuale)
142800970207     C     *IN35         IFEQ      *OFF
142900970210     C   34              ADD       1             CPA($T)
143000970210     C  N34              ADD       1             C4A($T)
143100970210     C                   ADD       1             CDT($T)
143200950626     C                   END
143300000000      *
143400940427     C     *IN36         IFEQ      *ON
143500970207      * A terra da partire                                             (35ON)
143600940427     C   34
143700940427     CANN35              ADD       1             CBP($T)
143800940427     C  N34
143900940427     CANN35              ADD       1             C4P($T)
144000970207     C  N35              ADD       1             C0P($T)
144100970207     C  N35              ADD       1             C3P($T)
144200970207      * A terra non da partire                                         (35ON)
144300940427     C   34
144400940427     CAN 35              ADD       1             CBR($T)
144500940427     C  N34
144600940427     CAN 35              ADD       1             C4R($T)
144700940427     C   35              ADD       1             C0R($T)
144800940427     C                   ENDIF
144900000000      *
145000000000      * ?  CALCOLI  SUL  PESO  ?
145100000000      *
145200970207     C     *IN35         IFEQ      *OFF
145300970210     C   34              ADD       $PESO         PPA($T)
145400970210     C  N34              ADD       $PESO         P4A($T)
145500970210     C                   ADD       $PESO         PDT($T)
145600950626     C                   END
145700940502      * A TERRA
145800940427     C     *IN36         IFEQ      *ON
145900940502      *
146000970206      * Fino a 35 quintali
146100021217     ***C     BLPPKF        IFLE      3500
146200021217     C     Wpeso         IFLE      3500
146300970207     C* Da partire
146400970206     C   34
146500970206     CANN35              ADD       $PESO         PFI($T)
146600970206     C  N34
146700970206     CANN35              ADD       $PESO         P4I($T)
146800970206     C  N35              ADD       $PESO         P3I($T)
146900970206     C  N35              ADD       $PESO         P0I($T)
147000970207     C* Non da partire
147100970206     C   34
147200970206     CAN 35              ADD       $PESO         PFN($T)
147300970206     C  N34
147400970206     CAN 35              ADD       $PESO         P4N($T)
147500970206     C   35              ADD       $PESO         P0N($T)
147600970206     C                   ELSE
147700970206      * Oltre 35 quintali:                                             (35ON)
147800970207     C* Da partire
147900940427     C   34
148000940427     CANN35              ADD       $PESO         PBP($T)
148100940427     C  N34
148200940427     CANN35              ADD       $PESO         P4P($T)
148300970206     C  N35              ADD       $PESO         P0P($T)
148400970206     C  N35              ADD       $PESO         P3P($T)
148500970207     C* Non da partire
148600940427     C   34
148700940427     CAN 35              ADD       $PESO         PBR($T)
148800940427     C  N34
148900940427     CAN 35              ADD       $PESO         P4R($T)
149000940427     C   35              ADD       $PESO         P0R($T)
149100940427     C                   ENDIF
149200970206     C                   ENDIF
149300000331     C*
149400000331     C                   ENDIF
149500000000      *
149600000000      * ?  CALCOLI  SUL  VOLUME  ?
149700940329      *
149800000000      *
149900021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
150000021217     ***C     BLTVUC        IFEQ      *ZERO
150100021217     *** *
150200021217     ***C     BLPVLF        DIV(H)    BLPNCL        $VOLUM           12 4
150300021217     *** *
150400021217     ***C                   ELSE
150500021217     ***C                   Z-ADD     BLTVUC        $VOLUM
150600021217     ***C                   ENDIF
150700000000      *
150800000331     C     WSOS          IFEQ      *BLANKS
150900970207     C     *IN35         IFEQ      *OFF
151000970210     C   34              ADD       $VOLUM        VPA($T)
151100970210     C  N34              ADD       $VOLUM        V4A($T)
151200970210     C                   ADD       $VOLUM        VDT($T)
151300950626     C                   END
151400940505      *
151500940428     C     *IN36         IFEQ      *ON
151600970207     C* Da partire
151700940428     C   34
151800940428     CANN35              ADD       $VOLUM        VBP($T)
151900940428     C  N34
152000940428     CANN35              ADD       $VOLUM        V4P($T)
152100970206     C  N35              ADD       $VOLUM        V0P($T)
152200970206     C  N35              ADD       $VOLUM        V3P($T)
152300000331     c* Sia che sia da partire che non da partire: sommo nel totale
152400000331     C* Mc rimasti a magazzino
152500000331     C   34              ADD       $VOLUM        VBR($T)
152600000331     C  N34              ADD       $VOLUM        V4R($T)
152700000331     C                   ADD       $VOLUM        V0R($T)
152800000406     C                   ADD       $VOLUM        V3R($T)
152900000000      *
153000000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
153100000000      *
153200970205      * Visto che le spedizioni non riesco a calcolarle ora, mi limito
153300970205      * a tenere aggiornato il file di work con le bolle
153400940428     C   34              MOVE      *BLANK        WRKCSE
153500940428     C  N34              MOVE      'C'           WRKCSE
153600940503     C                   Z-ADD     BLPLNP        WRKLNP
153700950118     C     FNWR1K        CHAIN     FNWRK00F                           99
153800940428     C     *IN99         IFEQ      *OFF
153900000000     C                   ADD       1             WRKNC1
154000000000     C                   EXCEPT    AGGIO
154100000000     C                   ELSE
154200950118     C                   MOVEL     *BLANKS       WTRANS            1
154300940503     C                   EXSR      WRIWRK
154400000000     C                   ENDIF
154500940428     C                   ENDIF
154600000331     c* Se collo di bolla in sospensione sommo volume in totale mc
154700000331     c* rimasti a magazzino
154800000331     C                   ELSE
154900000331     C   34              ADD       $VOLUM        VBR($T)
155000000331     C  N34              ADD       $VOLUM        V4R($T)
155100000331     C                   ADD       $VOLUM        V0R($T)
155200000406     C                   ADD       $VOLUM        V3R($T)
155300000412     C                   ADD       $VOLUM        VOLUME            9 4
155400000331     C                   ENDIF
155500940323      *
155600040921     C                   MOVE      $FTR          SATFTR
155700040921     C                   MOVE      $DTR          SATDTR
155800940428     C     *IN37         IFEQ      *ON
155900040921     C****               MOVE      $FTR          SATFTR
156000040921     C****               MOVE      $DTR          SATDTR
156100970205     C                   WRITE     FNSAT000
156200940428     C                   ELSE
156300970205     C                   UPDATE    FNSAT000
156400940428     C                   ENDIF
156500000000      *
156600000000     C                   ENDSR
156700940428      **************************************************************************
156800940428      * Carico i dati della bolla (peso e volume)
156900940428      *  e conto le spedizioni partite il giorno di elaborazione
157000940428      **************************************************************************
157100940428     C     DATBOL        BEGSR
157200940428      *
157300021217     *** * Calcolo il peso di un singolo collo
157400021217     ***C     BLPPKF        DIV(H)    BLPNCL        $PESO             8 1
157500021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
157600021217     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
157700021217     *** * BLPVLC=Volume rilevato)
157800050112     ***C     BLPNCR        ifeq      BLPNCL
157900021217     ***C                   Z-ADD     BLPVLC        BLPVLF
158000021217     ***C                   ELSE
158100021217     ***C     BLPVLC        IFGE      BLPVLF
158200021217     ***C                   Z-ADD     BLPVLC        BLPVLF
158300021217     ***C                   ENDIF
158400021217     ***C                   ENDIF
158500021217      *
158600021217      * Verifico i dati misurati con la macchina smistacolli
158700021217      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
158800021220      * e calcolo il VOLUME unitario medio per collo da usare se non
158900021220      * tutti i colli sono stati volumati dalla smistacolli
159000021217      *
159100030213      **** Verifico se sono stati volumati tutti i colli
159200030213     c***                clear                   Wsivol
159300021217     c                   clear                   Wvolu
159400050112     c                   if        BLPncr = BLPncl
159500021217     c                   z-add     BLPvlc        Wvolu
159600030213     c***                eval      Wsivol =  'S'
159700021217   x1c                   else
159800021217if  2c                   if        BLPvlc >= BLPvlf
159900021217     c                   z-add     BLPvlc        Wvolu
160000021217   x2c                   else
160100021217     c                   z-add     BLPvlf        Wvolu
160200021217e   2c                   endif
160300021217e   1c                   endif
160400021220      *
160500021220      * Calcolo il volume medio
160600021220     c     Wvolu         div(H)    BLPncl        Wvolmed
160700021217      *
160800021217      * Verifico i dati misurati con la macchina smistacolli
160900021217      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
161000021220      * e calcolo il PESO   unitario medio per collo da usare se non
161100021220      * tutti i colli sono stati pesati   dalla smistacolli
161200021217      *
161300030213      **** Verifico se sono stati pesati   tutti i colli
161400030213     c***                clear                   Wsipes
161500021217     c                   clear                   Wpeso
161600050112     c                   If        BLPncp = BLPncl
161700021217     c                   z-add     BLPpkc        Wpeso
161800030213     c***                eval      Wsipes =  'S'
161900021217   c1c                   else
162000021217if  2c                   if        BLPpkc >= BLPpkf
162100021217     c                   z-add     BLPpkc        Wpeso
162200021217   x2c                   else
162300021217     c                   z-add     BLPpkf        Wpeso
162400021217e   2c                   endif
162500021217e   1c                   endif
162600021220      *
162700021220      * Calcolo il peso medio
162800021220     c     Wpeso         div(H)    BLPncl        Wpesmed
162900940428     C*
163000940428     C* SOMMO SOLTANTO LE SPEDIZIONI PARTITE NEL GIORNO
163100970205     C*  LA DATA DI RIFERIMENTO E' LA DATA ULTIMO COLLO
163200940506     C     $FLAG         IFEQ      ' '
163300970205     C     BLPDUC        ANDEQ     DS1AAL
163400000331     C     WSOS          ANDEQ     *BLANKS
163500950118     C                   MOVEL     *BLANKS       WTRANS
163600940503     C                   EXSR      CTRBOL
163700970205     C                   ENDIF
163800940428     C*
163900940428     C                   MOVEL     '2'           $BOLLA
164000940428     C                   ENDSR
164100950118      **************************************************************************
164200021217      * Carico i dati della bolla (peso e volume) (TRANSITI)
164300021217     *** *  e conto le spedizioni partite il giorno di elaborazione
164400021217     *** *    (questo NON lo faceva NEANCHE prima)
164500950118      **************************************************************************
164600950118     C     DATBOT        BEGSR
164700950118      *
164800021217     *** * Calcolo il peso di un singolo collo
164900021217     ***C     BTPPKB        DIV(H)    BTPNCL        $PESO             8 1
165000021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
165100021217     *** * misurati con la macchina smistacolli (BTPNCR=Numero colli rilevati,
165200021217     *** * BTPVLC=Volume rilevato)
165300050112     ***C     BTPNCR        ifeq      BTPNCL
165400021217     ***C                   Z-ADD     BTPVLC        BTPVLB
165500021217     ***C                   ELSE
165600021217     ***C     BTPVLC        IFGE      BTPVLB
165700021217     ***C                   Z-ADD     BTPVLC        BTPVLB
165800021217     ***C                   ENDIF
165900021217     ***C                   ENDIF
166000021217     ***C*
166100021220      *
166200021220      * Verifico i dati misurati con la macchina smistacolli
166300021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
166400021220      * e calcolo il VOLUME unitario medio per collo da usare se non
166500021220      * tutti i colli sono stati volumati dalla smistacolli
166600021217      *
166700030213      **** Verifico se sono stati volumati tutti i colli
166800030213     c***                clear                   Wsivol
166900021217     c                   clear                   Wvolu
167000050112     c                   if        BTPncr = BTPncl
167100021217     c                   z-add     BTPvlc        Wvolu
167200030213     c***                eval      Wsivol =  'S'
167300021217   x1c                   else
167400021217if  2c                   if        BTPvlc >= BTPvlb
167500021217     c                   z-add     BTPvlc        Wvolu
167600021217   x2c                   else
167700021217     c                   z-add     BTPvlb        Wvolu
167800021217e   2c                   endif
167900021217e   1c                   endif
168000021220      *
168100021220      * Calcolo il volume medio
168200021220     c     Wvolu         div(H)    BTPncl        Wvolmed
168300021220      *
168400021220      * Verifico i dati misurati con la macchina smistacolli
168500021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
168600021220      * e calcolo il PESO   unitario medio per collo da usare se non
168700021220      * tutti i colli sono stati pesati   dalla smistacolli
168800021217      *
168900030213      **** Verifico se sono stati pesati   tutti i colli
169000030213     c***                clear                   Wsipes
169100021217     c                   clear                   Wpeso
169200050112     c                   if        BTPncp = BTPncl
169300021217     c                   z-add     BTPpkc        Wpeso
169400030213     c***                eval      Wsipes =  'S'
169500021217   x1c                   else
169600021217if  2c                   if        BTPpkc >= BTPpkb
169700021217     c                   z-add     BTPpkc        Wpeso
169800021217   x2c                   else
169900021217     c                   z-add     BTPpkb        Wpeso
170000021217e   2c                   endif
170100021217e   1c                   endif
170200021220      *
170300021220      * Calcolo il peso medio
170400071231     c     Wpeso         div(H)    BTPncl        Wpesmed
170500021217      *
170600950118     C                   MOVEL     '2'           $BOLLA
170700950118     C                   ENDSR
170800940503      **************************************************************************
170900950118      * Scrivo il file FNWRK00F
171000940503      **************************************************************************
171100940503     C     WRIWRK        BEGSR
171200950118     C*
171300940503     C                   MOVE      $FLAG         WRKFLG
171400950118     C                   Z-ADD     1             WRKNC1
171500950118     C*
171600950118     C     WTRANS        IFNE      'T'
171700940503     C                   Z-ADD     BLPAAS        WRKAAS
171800940503     C                   Z-ADD     BLPMGS        WRKMGS
171900940503     C                   Z-ADD     BLPNRS        WRKNRS
172000940503     C                   Z-ADD     BLPNSP        WRKNSP
172100940503     C                   Z-ADD     BLPLNA        WRKLNA
172200940503     C                   MOVE      BLPTSP        WRKTSP
172300940503     C                   Z-ADD     BLPNCL        WRKNCL
172400940503     C                   Z-ADD     BLPTFA        WRKTFA
172500070525     c* Se dopo la data di attivazione, memorizzo la data borderò
172600070525    3c                   if        ds1aal<§vponpi
172700970207     C                   Z-ADD     BLPDBR        WRKDBR
172800070525     c                   else
172900070525     C                   Z-ADD     BLPDrt        WRKDBR
173000070525     c                   endif
173100070525     c
173200970303     C                   Z-ADD     BLPDPC        WRKDPC
173300970303     C                   Z-ADD     BLPDUC        WRKDUC
173400970303     C                   Z-ADD     D98NPI        WRKNPI
173500950118     C*
173600950118     C                   ELSE
173700950118     C*
173800950118     C                   Z-ADD     BTPAAS        WRKAAS
173900950118     C                   Z-ADD     BTPMGS        WRKMGS
174000950118     C                   Z-ADD     BTPNRS        WRKNRS
174100950118     C                   Z-ADD     BTPNSP        WRKNSP
174200950118     C                   Z-ADD     BTPLNA        WRKLNA
174300950118     C                   MOVE      BTPTSP        WRKTSP
174400950118     C                   Z-ADD     BTPNCL        WRKNCL
174500950118     C                   Z-ADD     BTPTFA        WRKTFA
174600970207     C                   CLEAR                   WRKDBR
174700970303     C                   CLEAR                   WRKDUC
174800970303     C                   CLEAR                   WRKDPC
174900970303     C                   CLEAR                   WRKNPI
175000950118     C                   ENDIF
175100950118     C*
175200950118     C                   WRITE     FNWRK000
175300950118     C*
175400940503     C                   ENDSR
175500940503      **************************************************************************
175600940503      * Calcolo le spedizioni da partire:
175700950118      *  vedo se gia' contata prima di ricontarla(se esiste in FNWRK)
175800970207      *  Potrei averla già contata se i colli non sono sequenziali
175900940503      **************************************************************************
176000940503     C     CTRBOL        BEGSR
176100940503     C*
176200940505     C                   MOVEL     $FLAG         $FLAG2
176300940503     C                   MOVEL     'P'           $FLAG
176400940503     C                   MOVEL     BLPLNP        WRKLNP
176500940503     C                   CLEAR                   WRKCSE
176600950118     C     FNWR1K        SETLL     FNWRK00F                               99
176700940503     C* BOLLA NON TROVATA  LA SCRIVO
176800940503    1C     *IN99         IFEQ      *OFF
176900940503     C                   EXSR      WRIWRK
177000940503     C*
177100970211     C                   ADD       1             SDT($T)
177200970210     C   34              ADD       1             SPA($T)
177300940503     C* S4A --> PER C/S SPEDIZIONI DELL'AREA
177400940503     C*         PER I TRANSITI
177500970210     C  N34              ADD       1             S4A($T)
177600940503    1C                   ENDIF
177700940505     C                   MOVEL     $FLAG2        $FLAG
177800940503     C                   ENDSR
177900000000      **************************************************************************
178000000000      * Calcolo sulle spedizioni, lettura del file di work creato prima
178100000000      **************************************************************************
178200000000     C     CALSPE        BEGSR
178300940502     C                   SETOFF                                       3435
178400940428      * Verifico se e' transito
178500940502     C     $FLAG         COMP      'T'                                    35
178600000000      *
178700000000      * Leggo tutti le spedizioni della filiale da stampare dal file di work
178800970207    1C     $FLAG         IFEQ      *BLANK
178900950118     C     FNWRKK        SETLL     FNWRK00F
179000970207   X1C                   ELSE
179100950118     C     $FLAG         SETLL     FNWRK00F
179200970207    1C                   ENDIF
179300000000      *
179400970207    1C                   DO        *HIVAL
179500000000      *
179600970207    2C     $FLAG         IFEQ      *BLANK
179700950118     C     FNWRKK        READE(N)  FNWRK00F                               99
179800970207   X2C                   ELSE
179900950118     C     $FLAG         READE(N)  FNWRK00F                               99
180000970207    2C                   ENDIF
180100000000      *
180200970207    2C  N99              DO
180300000000      *
180400970210      * Verifico il tipo spedizione ESPR./DIST./CORR.
180500950201     C                   Z-ADD     3             $V                1 0
180600970207    3C     WRKTSP        IFEQ      'E'
180700000000     C                   Z-ADD     1             $V
180800970207    3C                   ENDIF
180900970207    3C     WRKTSP        IFEQ      'D'
181000940323     C                   Z-ADD     2             $V
181100970207    3C                   ENDIF
181200940428      *
181300940428      * Verifico se e' c/servizio
181400940502     C  N35WRKCSE        COMP      *BLANKS                                34
181500000000      *
181600000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
181700000000      * poiche' lo dovro aggiornare
181800970207     C                   CLEAR                   FNSAT000
181900000000     C                   Z-ADD     SIMFEL        SATFLE
182000950210     C                   Z-ADD     DS1ADA        SATDRE
182100940506     C  N35              Z-ADD     WRKLNP        SATLNP
182200940506     C   35              Z-ADD     SIMFEL        SATLNP
182300000000     C                   Z-ADD     WRKTFA        SATTFA
182400000000     C  N35              MOVE      *BLANK        SATTRE
182500000000     C   35              MOVE      'T'           SATTRE
182600970205     C     FNSA1K        CHAIN     FNSAT02L                           37
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
193200970207     C  N37              UPDATE    FNSAT000
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
199500970210      * ------------------ ESPR./DIST./CORR. --------------------------
199600000000      *
199700970210     C                   SETOFF                                       40
199800000000     C                   DO        3             $V
199900940429     C                   CLEAR                   DESCRP
200000940429      *
200100970210     C                   MOVE      ENS($V)       TIPOP
200200000000      *
200300000000      * ?  C  O   L   L  I  ?
200400970211     C                   Z-ADD     CBP($V)       COTOTP
200500970211     C                   Z-ADD     CPA($V)       $CDA
200600940428     C                   EXSR      PERCOL
200700000000      *
200800000000      * ?  P  E  S  O  ?
200900000000     C                   Z-ADD     PFI($V)       PEF35P
201000970211     C                   Z-ADD     PBP($V)       PEO35P
201100970212     C     PEF35P        ADD       PEO35P        PETOTP
201200970211     C                   Z-ADD     PPA($V)       $PDA
201300940428     C                   EXSR      PERPES
201400000000      *
201500000000      * ?  V  O  L  U  M E  ?
201600970211     C                   Z-ADD     VBP($V)       VOTOTP
201700970211     C                   Z-ADD     VPA($V)       $VDA
201800940428     C                   EXSR      PERVOL
201900000000      *
202000000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
202100970305     C                   Z-ADD     SPA($V)       $SDA
202200970305     C                   Z-ADD     FAF($V)       SPFAFF
202300940502     C                   EXSR      PERSPE
202400970305     C                   Z-ADD     SBP($V)       SPTOTP
202500970305     C                   Z-ADD     SCM($V)       SPCONP
202600970211     C     SBP($V)       ADD       SBR($V)       SPTOTA
202700970211     C     SPTOTA        SUB       SCM($V)       SPSPAP
202800000331      *
202900000331      * ?tot.volume a magazz?
203000000331     C                   Z-ADD     VBR($V)       VOTOTM
203100000000      *
203200000000     C                   EXSR      SCRIVI
203300000000      *
203400000000     C                   ENDDO
203500000000      *
203600950118     C                   WRITE     LSC2T2
203700940506     C                   ENDIF
203800940328      *
203900000417     C                   ENDSR
204000940428      **************************************************************************
204100940428      * Calcolo percentuale sui colli
204200940428      **************************************************************************
204300940428     C     PERCOL        BEGSR
204400940502     C                   Z-ADD     *ZERO         COSDPP
204500940428     C*
204600940502     C* PERCENTUALE SUL DA PARTIRE
204700970213     C  N02$CDA          IFNE      *ZERO
204800970210     C     COTOTP        DIV       $CDA          $COMOD
204900940502     C     $COMOD        MULT(H)   100           COSDPP
205000940502     C                   ENDIF
205100940428     C                   ENDSR
205200940428      **************************************************************************
205300940428      * Calcolo percentuale sul peso
205400940428      **************************************************************************
205500940428     C     PERPES        BEGSR
205600940502     C                   CLEAR                   PESDPP
205700940428     C*
205800940502     C* PERCENTUALE SUL DA PARTIRE
205900970213     C  N02$PDA          IFNE      *ZERO
206000970210     C     PETOTP        DIV       $PDA          $COMOD
206100940502     C     $COMOD        MULT(H)   100           PESDPP
206200940502     C                   ENDIF
206300940428     C                   ENDSR
206400940428      **************************************************************************
206500940428      * Calcolo percentuale sul volume
206600940428      **************************************************************************
206700940428     C     PERVOL        BEGSR
206800940502     C                   Z-ADD     *ZERO         VOSDPP
206900940502     C*
207000940502     C* PERCENTUALE SUL DA PARTIRE
207100970213     C  N02$VDA          IFNE      *ZERO
207200970210     C     VOTOTP        DIV       $VDA          $COMOD
207300940502     C     $COMOD        MULT(H)   100           VOSDPP
207400940502     C                   ENDIF
207500940428     C                   ENDSR
207600940428      **************************************************************************
207700940428      * Calcolo percentuale sulle spedizioni
207800940428      **************************************************************************
207900940428     C     PERSPE        BEGSR
208000940502     C                   Z-ADD     *ZERO         SPSDPP
208100940428     C*
208200940502     C* PERCENTUALE SUL DA PARTIRE
208300970213     C  N02$SDA          IFNE      *ZERO
208400970305     C     SPFAFF        DIV       $SDA          $COMOD
208500940502     C     $COMOD        MULT(H)   100           SPSDPP
208600940502     C                   ENDIF
208700940428     C                   ENDSR
208800000000      **************************************************************************
208900000000      * Azzeramento dei totali di filiale per nuovi conti
209000000000      **************************************************************************
209100000000     C     AZZFIL        BEGSR
209200000412     C                   CLEAR                   VOLUME
209300000000      *
209400000000      * COLLI
209500970211     C                   ADD       CPA(1)        C3A(1)
209600970211     C                   ADD       CPA(2)        C3A(2)
209700970211     C                   ADD       CPA(3)        C3A(3)
209800970211     C                   CLEAR                   CPA
209900970211     C                   CLEAR                   CBP
210000970211     C                   CLEAR                   CBR
210100970211      * PESI
210200970211     C                   ADD       PPA(1)        P3A(1)
210300970211     C                   ADD       PPA(2)        P3A(2)
210400970211     C                   ADD       PPA(3)        P3A(3)
210500970211     C                   CLEAR                   PFI
210600970211     C                   CLEAR                   PPA
210700970211     C                   CLEAR                   PBP
210800970211     C                   CLEAR                   PBR
210900970211     C                   CLEAR                   PFN
211000000000      * VOLUMI
211100970211     C                   ADD       VPA(1)        V3A(1)
211200970211     C                   ADD       VPA(2)        V3A(2)
211300970211     C                   ADD       VPA(3)        V3A(3)
211400970211     C                   CLEAR                   VPA
211500970211     C                   CLEAR                   VBP
211600970211     C                   CLEAR                   VBR
211700970211      * SPEDIZIONI
211800970211     C                   ADD       SPA(1)        S3A(1)
211900970211     C                   ADD       SPA(2)        S3A(2)
212000970211     C                   ADD       SPA(3)        S3A(3)
212100970211     C                   CLEAR                   SPA
212200970211     C                   CLEAR                   SBP
212300970211     C                   CLEAR                   SBR
212400970211     C                   CLEAR                   SCM
212500970305     C                   CLEAR                   FAF
212600000000      *
212700000000     C                   ENDSR
212800000000      **************************************************************************
212900000000      * Azzeramento dei totali di c/servizio per nuova area
213000000000      **************************************************************************
213100000000     C     AZZSER        BEGSR
213200000000      *
213300000000      * COLLI
213400940506     C                   ADD       C1A(1)        C3A(1)
213500970211     C                   ADD       C1A(2)        C3A(2)
213600970211     C                   ADD       C1A(3)        C3A(3)
213700000000     C                   CLEAR                   C1A
213800000000     C                   CLEAR                   C1P
213900000000     C                   CLEAR                   C1R
214000000000      * PESI
214100940506     C                   ADD       P1A(1)        P3A(1)
214200970211     C                   ADD       P1A(2)        P3A(2)
214300970211     C                   ADD       P1A(3)        P3A(3)
214400970211     C                   CLEAR                   P1I
214500970211     C                   CLEAR                   P1N
214600970211     C                   CLEAR                   P1P
214700970211     C                   CLEAR                   P1R
214800970211     C                   CLEAR                   P1A
214900970211      * VOLUMI
215000970211     C                   ADD       V1A(1)        V3A(1)
215100970211     C                   ADD       V1A(2)        V3A(2)
215200970211     C                   ADD       V1A(3)        V3A(3)
215300970211     C                   CLEAR                   V1A
215400970211     C                   CLEAR                   V1P
215500970211     C                   CLEAR                   V1R
215600000000      * SPEDIZIONI
215700940506     C                   ADD       S1A(1)        S3A(1)
215800970211     C                   ADD       S1A(2)        S3A(2)
215900970211     C                   ADD       S1A(3)        S3A(3)
216000970211     C                   CLEAR                   S1A
216100970211     C                   CLEAR                   S1P
216200970211     C                   CLEAR                   S1R
216300970211     C                   CLEAR                   S1M
216400970305     C                   CLEAR                   F1F
216500000000      *
216600000000     C                   ENDSR
216700000000      **************************************************************************
216800000000      * Azzeramento dei totali di c/servizio per nuova filiale
216900000000      **************************************************************************
217000000000     C     AZZSE4        BEGSR
217100000000      *
217200000000      * COLLI
217300970210     C                   CLEAR                   C4A
217400970210     C                   CLEAR                   C4P
217500970210     C                   CLEAR                   C4R
217600000000      * PESI
217700970210     C                   CLEAR                   P4A
217800970210     C                   CLEAR                   P4I
217900970210     C                   CLEAR                   P4P
218000970210     C                   CLEAR                   P4R
218100970210     C                   CLEAR                   P4N
218200000000      * VOLUMI
218300970210     C                   CLEAR                   V4A
218400970210     C                   CLEAR                   V4P
218500970210     C                   CLEAR                   V4R
218600000000      * SPEDIZIONI
218700970210     C                   CLEAR                   S4A
218800970210     C                   CLEAR                   S4P
218900970210     C                   CLEAR                   S4R
219000970210     C                   CLEAR                   S4M
219100970305     C                   CLEAR                   F4F
219200000000      *
219300000000     C                   ENDSR
219400000000      **************************************************************************
219500000000      * Azzeramento dei totali di transito per nuovi totali
219600000000      **************************************************************************
219700000000     C     AZZTRA        BEGSR
219800940506     C* SOMMO NEI TOTALI
219900940506     C                   EXSR      ADDTRA
220000000000      * COLLI
220100940428     C                   CLEAR                   C2P
220200000000      * PESI
220300000000     C                   CLEAR                   P2I
220400940428     C                   CLEAR                   P2P
220500000000      * VOLUMI
220600940428     C                   CLEAR                   V2P
220700000000      * SPEDIZIONI
220800940428     C                   CLEAR                   S2P
220900000000     C                   CLEAR                   S2M
221000000000      *
221100000000     C                   ENDSR
221200000000      **************************************************************************
221300000000      * Stampo i totali per c/servizio
221400000000      **************************************************************************
221500000000     C     TOTSER        BEGSR
221600000000      *
221700000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
221800000000      *
221900970211     C                   SETON                                        4045
222000000000     C                   SETOFF                                       41
222100000000     C                   Z-ADD     *ZERO         FILIAP
222200000000     C                   MOVE      *BLANKS       ORGDEP
222300000000     C     'C/SERVIZ'    CAT       'I':0         ORGDEP
222400000000      *
222500970211      * ?  C  O   L   L  I  ?
222600970211     C                   XFOOT     C1P           COTOTP
222700970211     C                   XFOOT     C1A           $CDA
222800970211     C                   EXSR      PERCOL
222900970211      *
223000970211      * ?  P  E  S  O  ?
223100970211     C                   XFOOT     P1I           PEF35P
223200970211     C                   XFOOT     P1P           PEO35P
223300970211     C     PEF35P        ADD       PEO35P        PETOTP
223400970211     C                   XFOOT     P1A           $PDA
223500940429     C                   EXSR      PERPES
223600000000      *
223700000000      * ?  V  O  L  U  M E  ?
223800970211     C                   XFOOT     V1P           VOTOTP
223900970211     C                   XFOOT     V1A           $VDA
224000940429     C                   EXSR      PERVOL
224100000000      *
224200000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
224300970211     C                   XFOOT     S1A           $SDA
224400970305     C                   XFOOT     F1F           SPFAFF
224500970211     C                   EXSR      PERSPE
224600970305     C                   XFOOT     S1P           SPTOTP
224700970305     C                   XFOOT     S1M           SPCONP
224800970211     C                   XFOOT     S1R           $COMO1
224900970211     C     SPTOTP        ADD       $COMO1        SPTOTA
225000970211     C     SPTOTA        SUB       SPCONP        SPSPAP
225100000331      *
225200000331      * ?tot.volume a magazz?
225300000331     C                   XFOOT     V1R           VOTOTM
225400970211      *
225500970211     C* STAMPO SOLO SE HO DEI DATI
225600000331     C     VOTOTM        IFGT      *ZEROS
225700970211     C*
225800970211     C                   EXSR      SCRIVI
225900000000      *
226000970211      * ------------------ ESPR./DIST./CORR. --------------------------
226100000000      *
226200970211     C                   SETOFF                                       40
226300000000     C                   DO        3             $V
226400940323     C                   MOVE      ENS($V)       TIPOP
226500000000      *
226600000000      * ?  C  O   L   L  I  ?
226700970211     C                   Z-ADD     C1P($V)       COTOTP
226800970211     C                   Z-ADD     C1A($V)       $CDA
226900940429     C                   EXSR      PERCOL
227000000000      *
227100000000      * ?  P  E  S  O  ?
227200000000     C                   Z-ADD     P1I($V)       PEF35P
227300970211     C                   Z-ADD     P1P($V)       PEO35P
227400970211     C     PEF35P        ADD       PEO35P        PETOTP
227500970211     C                   Z-ADD     P1A($V)       $PDA
227600940429     C                   EXSR      PERPES
227700000000      *
227800000000      * ?  V  O  L  U  M E  ?
227900970211     C                   Z-ADD     V1P($V)       VOTOTP
228000970211     C                   Z-ADD     V1A($V)       $VDA
228100940429     C                   EXSR      PERVOL
228200000000      *
228300000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
228400970211     C                   Z-ADD     S1A($V)       $SDA
228500970305     C                   Z-ADD     F1F($V)       SPFAFF
228600940502     C                   EXSR      PERSPE
228700970305     C                   Z-ADD     S1P($V)       SPTOTP
228800970305     C                   Z-ADD     S1M($V)       SPCONP
228900970211     C     S1P($V)       ADD       S1R($V)       SPTOTA
229000970211     C     SPTOTA        SUB       S1M($V)       SPSPAP
229100000331      *
229200000331      * ?tot volume a magazz?
229300000331     C                   Z-ADD     V1R($V)       VOTOTM
229400000000      *
229500000000     C                   EXSR      SCRIVI
229600000000      *
229700000000     C                   ENDDO
229800940504     C                   ENDIF
229900940429     C                   EXSR      AZZSER
230000000000      *
230100000000     C                   ENDSR
230200000000      **************************************************************************
230300000000      * Stampo i totali per transito
230400000000      **************************************************************************
230500000000     C     TOTTRA        BEGSR
230600000000      *
230700000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
230800000000      *
230900940506     C                   SETON                                        400203
231000970211     C                   SETOFF                                       45
231100000000     C                   MOVE      *BLANKS       ORGDEP
231200940502     C* stampa per linea: totale transiti
231300940502     C     *IN43         IFEQ      *ON
231400940502     C                   Z-ADD     *ZERO         FILIAP
231500940502     C                   MOVEL     'TRANSITI'    ORGDEP
231600940502     C                   SETOFF                                       41
231700940502     C                   ELSE
231800940502     c* stampa per terminal in transito
231900940502     C                   SETON                                        41
232000940502      * Decodifico il terminal di arrivo
232100940502     C                   Z-ADD     $FILPR        FILIAP
232200940502     C     $FILPR        CHAIN     AZORG01L                           99
232300940502     C  N99ORGFVA        COMP      *BLANK                             9999
232400940503     C   99              CLEAR                   ORGDEP
232500940502     C  N99              MOVEL     ORGDES        ORGDEP
232600940502     C                   ENDIF
232700000000      *
232800000000      * ?  C  O   L   L  I  ?
232900940502     C                   XFOOT     C2P           COTOTP
233000970211     C*
233100970211      * SOLO SE HO TROVATO ALEMNO UN COLLO A TERRA STAMPO
233200970211     C     COTOTP        IFGT      0
233300940504      *
233400000000      *
233500000000      * ?  P  E  S  O  ?
233600970211     C                   XFOOT     P2I           PEF35P
233700970211     C                   XFOOT     P2P           PEO35P
233800970211     C     PEO35P        ADD       PEF35P        PETOTP
233900000000      *
234000000000      * ?  V  O  L  U  M E  ?
234100940428     C                   XFOOT     V2P           VOTOTP
234200000000      *
234300000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
234400940428     C                   XFOOT     S2P           SPTOTP
234500970211     C                   XFOOT     S2P           SPTOTA
234600970211     C                   XFOOT     S2M           SPCONP
234700970211     C     SPTOTA        SUB       SPCONP        SPSPAP
234800000000      *
234900000000     C                   EXSR      SCRIVI
235000940509     C                   ADD       1             $RIGHE
235100000000      *
235200970211      * ------------------ ESPR./DIST./CORR. --------------------------
235300000000      *
235400940502     C                   SETOFF                                       4045
235500000000     C                   DO        3             $V
235600940502     C* PER TERMINAL SCRIVO CHE E' RECORD DI TRANSITO
235700940429     C  N43$V            IFEQ      1
235800940506     C                   MOVEL     'IN TRANS'    DESCRP
235900940429     C                   ELSE
236000940429     C                   CLEAR                   DESCRP
236100940429     C                   ENDIF
236200940429     C*
236300940323     C                   MOVE      ENS($V)       TIPOP
236400000000      *
236500000000      * ?  C  O   L   L  I  ?
236600940428     C                   Z-ADD     C2P($V)       COTOTP
236700000000      *
236800000000      * ?  P  E  S  O  ?
236900970211     C                   Z-ADD     P2I($V)       PEF35P
237000970211     C                   Z-ADD     P2P($V)       PEO35P
237100970211     C     PEO35P        ADD       PEF35P        PETOTP
237200000000      *
237300000000      * ?  V  O  L  U  M E  ?
237400940428     C                   Z-ADD     V2P($V)       VOTOTP
237500000000      *
237600000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
237700940428     C                   Z-ADD     S2P($V)       SPTOTP
237800970211     C                   Z-ADD     S2P($V)       SPTOTA
237900970211     C                   Z-ADD     S2M($V)       SPCONP
238000970211     C     SPTOTA        SUB       S2M($V)       SPSPAP
238100000000      *
238200000000     C                   EXSR      SCRIVI
238300000000      *
238400000000     C                   ENDDO
238500940428     C*
238600940428     C                   EXSR      AZZTRA
238700940502      *
238800950118     C  N38              WRITE     LSC2T2
238900940506     C                   ENDIF
239000940506     C                   SETOFF                                       0302
239100000000      *
239200000000     C                   ENDSR
239300000000      **************************************************************************
239400970211      * Scrittura del record sul file di trasmissione FNSAT00F
239500000000      **************************************************************************
239600000000     C     WRISAT        BEGSR
239700000000      *
239800970207     C                   CLEAR                   FNSAT000
239900000000     C                   Z-ADD     SIMFEL        SATFLE
240000000000     C                   Z-ADD     $FILPR        SATLNP
240100000000     C                   Z-ADD     *ZERO         SATTFA
240200950210     C                   Z-ADD     DS1ADA        SATDRE
240300000000     C                   MOVE      *BLANK        SATTRE
240400000000      *
240500000000      * COLLI: da partire
240600970210     C                   Z-ADD     CPA(1)        SATCPE
240700970210     C                   Z-ADD     CPA(2)        SATCPD
240800970210     C                   Z-ADD     CPA(3)        SATCPC
240900970207      * COLLI: a terra da partire
241000970207     C                   Z-ADD     CBP(1)        SATCDE
241100970207     C                   Z-ADD     CBP(2)        SATCDD
241200970207     C                   Z-ADD     CBP(3)        SATCDC
241300970207      * COLLI: a terra non da partire
241400970207     C                   Z-ADD     CBR(1)        SATCNE
241500970207     C                   Z-ADD     CBR(2)        SATCND
241600970207     C                   Z-ADD     CBR(3)        SATCNC
241700970207      * PESO: da partire
241800970210     C                   Z-ADD     PPA(1)        SATPPE
241900970210     C                   Z-ADD     PPA(2)        SATPPD
242000970210     C                   Z-ADD     PPA(3)        SATPPC
242100970207      * PESO: a terra da partire fino a 35 quintali
242200970207     C                   Z-ADD     PFI(1)        SATFDE
242300970207     C                   Z-ADD     PFI(2)        SATFDD
242400970207     C                   Z-ADD     PFI(3)        SATFDC
242500970207      * PESO: a terra non da partire fino a 35 q.
242600970207     C                   Z-ADD     PFN(1)        SATFNE
242700970207     C                   Z-ADD     PFN(2)        SATFND
242800970207     C                   Z-ADD     PFN(3)        SATFNC
242900970207     C* PESO: a terra da partire oltre 35 quintali
243000970207     C                   Z-ADD     PBP(1)        SATODE
243100970207     C                   Z-ADD     PBP(2)        SATODD
243200970207     C                   Z-ADD     PBP(3)        SATODC
243300970207      * PESO: a terra non da partire oltre 35 q.
243400970207     C                   Z-ADD     PBR(1)        SATONE
243500970207     C                   Z-ADD     PBR(2)        SATOND
243600970207     C                   Z-ADD     PBR(3)        SATONC
243700000000      * VOLUME: da partire
243800970210     C                   Z-ADD     VPA(1)        SATVPE
243900970210     C                   Z-ADD     VPA(2)        SATVPD
244000970210     C                   Z-ADD     VPA(3)        SATVPC
244100970207      * VOLUME a terra da partire
244200970207     C                   Z-ADD     VBP(1)        SATVDE
244300970207     C                   Z-ADD     VBP(2)        SATVDD
244400970207     C                   Z-ADD     VBP(3)        SATVDC
244500000331      * VOLUME rimasto a magazzino
244600000331      * (totale dei colli a terra da partire, da non partire e colli
244700000331      * di bolle in sospensione)
244800970207     C                   Z-ADD     VBR(1)        SATVNE
244900970207     C                   Z-ADD     VBR(2)        SATVND
245000970207     C                   Z-ADD     VBR(3)        SATVNC
245100940429      * SPEDIZIONI: da partire
245200970210     C                   Z-ADD     SPA(1)        SATSPE
245300970210     C                   Z-ADD     SPA(2)        SATSPD
245400970210     C                   Z-ADD     SPA(3)        SATSPC
245500970207      * SPEDIZIONI a terra da partire
245600970207     C                   Z-ADD     SBP(1)        SATSDE
245700970207     C                   Z-ADD     SBP(2)        SATSDD
245800970207     C                   Z-ADD     SBP(3)        SATSDC
245900970207      * SPEDIZIONI a terra non da partire
246000970207     C                   Z-ADD     SBR(1)        SATSNE
246100970207     C                   Z-ADD     SBR(2)        SATSND
246200970207     C                   Z-ADD     SBR(3)        SATSNC
246300970305      * SPEDIZIONI a terra da partire fuori affidabilità
246400970305     C                   Z-ADD     FAF(1)        SATSFE
246500970305     C                   Z-ADD     FAF(2)        SATSFD
246600970305     C                   Z-ADD     FAF(3)        SATSFC
246700000000      * SPEDIZIONI: complete
246800000000     C                   Z-ADD     SCM(1)        SATSCE
246900970207     C                   Z-ADD     SCM(2)        SATSCD
247000970207     C                   Z-ADD     SCM(3)        SATSCC
247100000000      *
247200970318     C* ASTERISCO LE SEGUENTI IF PERCHE' IL RECORD LO DEVO SEMPRE
247300970318     C* SCRIVERE PER EVITARE CHE NELL'ELABORAZIONE DEL REPORT DI
247400970318     C* SEDE, SE PER UN P.O. IL DA PARTIRE è = 0, VENGA ERRONEAMENTE
247500970318     C* SEGNALATO "DATI NON PERVENUTI".
247600970318     C*          SATSDE    IFGT *ZERO
247700970318     C*          SATSDD    ORGT *ZERO
247800970318     C*          SATSDC    ORGT *ZERO
247900970318     C*          SATSNE    ORGT *ZERO
248000970318     C*          SATSND    ORGT *ZERO
248100970318     C*          SATSNC    ORGT *ZERO
248200970318     C*          SATSPE    ORGT *ZEROS
248300970318     C*          SATSPD    ORGT *ZEROS
248400970318     C*          SATSPC    ORGT *ZEROS
248500940323      *
248600940428     C                   MOVE      $FTR          SATFTR
248700970207     C                   MOVE      $DTR          SATDTR
248800940323      *
248900970207     C                   WRITE     FNSAT000
249000970318     C*
249100970318     C* LE IF SOPRA ASTERISCATE LE MANTENGO PERò PER IMPOSTARE $PRIM4
249200970318     C*
249300970318     C     SATSDE        IFGT      *ZERO
249400970318     C     SATSDD        ORGT      *ZERO
249500970318     C     SATSDC        ORGT      *ZERO
249600970318     C     SATSNE        ORGT      *ZERO
249700970318     C     SATSND        ORGT      *ZERO
249800970318     C     SATSNC        ORGT      *ZERO
249900970318     C     SATSPE        ORGT      *ZEROS
250000970318     C     SATSPD        ORGT      *ZEROS
250100970318     C     SATSPC        ORGT      *ZEROS
250200000405     C     SATVNE        ORGT      *ZEROS
250300000405     C     SATVND        ORGT      *ZEROS
250400000405     C     SATVNC        ORGT      *ZEROS
250500940429     C* PER LA STAMPA DEI TOTALI DI AREA
250600940429     C                   Z-ADD     1             $PRIM4
250700970318     C                   ENDIF
250800000000      *
250900000000     C                   ENDSR
251000000000      **************************************************************************
251100970211      * Scrittura del record c/servizio sul file di trasmissione FNSAT00F
251200000000      **************************************************************************
251300000000     C     WR1SAT        BEGSR
251400000000      *
251500970207     C                   CLEAR                   FNSAT000
251600000000     C                   Z-ADD     SIMFEL        SATFLE
251700000000     C                   Z-ADD     $FILPR        SATLNP
251800000000     C                   Z-ADD     *ZERO         SATTFA
251900950210     C                   Z-ADD     DS1ADA        SATDRE
252000000000     C                   MOVE      'C'           SATTRE
252100000000      *
252200000000      * COLLI: da partire
252300970210     C                   Z-ADD     C4A(1)        SATCPE
252400970210     C                   Z-ADD     C4A(2)        SATCPD
252500970210     C                   Z-ADD     C4A(3)        SATCPC
252600970207      * COLLI: a terra da partire
252700970207     C                   Z-ADD     C4P(1)        SATCDE
252800970207     C                   Z-ADD     C4P(2)        SATCDD
252900970207     C                   Z-ADD     C4P(3)        SATCDC
253000970207      * COLLI: a terra non da partire
253100970207     C                   Z-ADD     C4R(1)        SATCNE
253200970207     C                   Z-ADD     C4R(2)        SATCND
253300970207     C                   Z-ADD     C4R(3)        SATCNC
253400970207      * PESO: da partire
253500970210     C                   Z-ADD     P4A(1)        SATPPE
253600970210     C                   Z-ADD     P4A(2)        SATPPD
253700970210     C                   Z-ADD     P4A(3)        SATPPC
253800970207      * PESO: a terra da partire fino a 35 quintali
253900970207     C                   Z-ADD     P4I(1)        SATFDE
254000970207     C                   Z-ADD     P4I(2)        SATFDD
254100970207     C                   Z-ADD     P4I(3)        SATFDC
254200970207      * PESO: a terra non da partire fino a 35 quintali
254300970207     C                   Z-ADD     P4N(1)        SATFNE
254400970207     C                   Z-ADD     P4N(2)        SATFND
254500970207     C                   Z-ADD     P4N(3)        SATFNC
254600970207      * PESO: a terra da partire oltre 35 quintali
254700970207     C                   Z-ADD     P4P(1)        SATODE
254800970207     C                   Z-ADD     P4P(2)        SATODD
254900970207     C                   Z-ADD     P4P(3)        SATODC
255000970207      * PESO: a terra non da partire oltre 35 quintali
255100970207     C                   Z-ADD     P4R(1)        SATONE
255200970207     C                   Z-ADD     P4R(2)        SATOND
255300970207     C                   Z-ADD     P4R(3)        SATONC
255400000000      * VOLUME: da partire
255500970210     C                   Z-ADD     V4A(1)        SATVPE
255600970210     C                   Z-ADD     V4A(2)        SATVPD
255700970210     C                   Z-ADD     V4A(3)        SATVPC
255800970207      * VOLUME: a terra da partire
255900970207     C                   Z-ADD     V4P(1)        SATVDE
256000970207     C                   Z-ADD     V4P(2)        SATVDD
256100970207     C                   Z-ADD     V4P(3)        SATVDC
256200000331      * VOLUME: totale rimasto a magazzino
256300000331      * (a terra da partire, da non partire, in sospensione)
256400970207     C                   Z-ADD     V4R(1)        SATVNE
256500970207     C                   Z-ADD     V4R(2)        SATVND
256600970207     C                   Z-ADD     V4R(3)        SATVNC
256700970207      * SPEDIZIONI: da partire
256800970210     C                   Z-ADD     S4A(1)        SATSPE
256900970210     C                   Z-ADD     S4A(2)        SATSPD
257000970210     C                   Z-ADD     S4A(3)        SATSPC
257100970207      * SPEDIZIONI: a terra da partire
257200970207     C                   Z-ADD     S4P(1)        SATSDE
257300970207     C                   Z-ADD     S4P(2)        SATSDD
257400970207     C                   Z-ADD     S4P(3)        SATSDC
257500970207      * SPEDIZIONI: a terra non da partire
257600970207     C                   Z-ADD     S4R(1)        SATSNE
257700970207     C                   Z-ADD     S4R(2)        SATSND
257800970207     C                   Z-ADD     S4R(3)        SATSNC
257900970305      * SPEDIZIONI: a terra da partire fuori affidabilità
258000970305     C                   Z-ADD     F4F(1)        SATSFE
258100970305     C                   Z-ADD     F4F(2)        SATSFD
258200970305     C                   Z-ADD     F4F(3)        SATSFC
258300000000      * SPEDIZIONI: complete
258400000000     C                   Z-ADD     S4M(1)        SATSCE
258500970207     C                   Z-ADD     S4M(2)        SATSCD
258600970207     C                   Z-ADD     S4M(3)        SATSCC
258700000000      *
258800970318     C     SATSDE        IFGT      *ZERO
258900970318     C     SATSDD        ORGT      *ZERO
259000970318     C     SATSDC        ORGT      *ZERO
259100970318     C     SATSNE        ORGT      *ZERO
259200970318     C     SATSND        ORGT      *ZERO
259300970318     C     SATSNC        ORGT      *ZERO
259400970318     C     SATSPE        ORGT      *ZERO
259500970318     C     SATSPD        ORGT      *ZERO
259600970318     C     SATSPC        ORGT      *ZERO
259700000405     C     SATVNE        ORGT      *ZEROS
259800000405     C     SATVND        ORGT      *ZEROS
259900000405     C     SATVNC        ORGT      *ZEROS
260000940323      *
260100940428     C                   MOVE      $FTR          SATFTR
260200970207     C                   MOVE      $DTR          SATDTR
260300940323      *
260400970207     C                   WRITE     FNSAT000
260500940429     C* PER LA STAMPA DEI C/S
260600940429     C                   Z-ADD     1             $PRIM2
260700940429     C* PER LA STAMPA DEI TOTALI DI AREA
260800940429     C                   Z-ADD     1             $PRIM4
260900000000     C                   ENDIF
261000000000      *
261100000000      * Aggiorno le schiere per i totali c/servizio per area
261200000000     C                   DO        3             $U                1 0
261300970210     C* A TERRA
261400000000     C                   ADD       C4P($U)       C1P($U)
261500000000     C                   ADD       C4R($U)       C1R($U)
261600000000     C                   ADD       P4I($U)       P1I($U)
261700970207     C                   ADD       P4N($U)       P1N($U)
261800000000     C                   ADD       P4P($U)       P1P($U)
261900000000     C                   ADD       P4R($U)       P1R($U)
262000000000     C                   ADD       V4P($U)       V1P($U)
262100000331     C                   ADD       V4R($U)       V1R($U)
262200000000     C                   ADD       S4P($U)       S1P($U)
262300000000     C                   ADD       S4R($U)       S1R($U)
262400000000     C                   ADD       S4M($U)       S1M($U)
262500970305     C                   ADD       F4F($U)       F1F($U)
262600970210     C* DA PARTIRE
262700970210     C                   ADD       C4A($U)       C1A($U)
262800970210     C                   ADD       P4A($U)       P1A($U)
262900970210     C                   ADD       V4A($U)       V1A($U)
263000970210     C                   ADD       S4A($U)       S1A($U)
263100000000     C                   ENDDO
263200000000      *
263300000000     C                   EXSR      AZZSE4
263400000000      *
263500000000     C                   ENDSR
263600000000      **************************************************************************
263700970211      * Scrittura del record transito sul file di trasmissione FNSAT00F
263800000000      **************************************************************************
263900000000     C     WR2SAT        BEGSR
264000000000      *
264100970211     C                   CLEAR                   FNSAT000
264200000000     C                   Z-ADD     SIMFEL        SATFLE
264300940502     C                   Z-ADD     SIMFEL        SATLNP
264400000000     C                   Z-ADD     *ZERO         SATTFA
264500950210     C                   Z-ADD     DS1ADA        SATDRE
264600000000     C                   MOVE      'T'           SATTRE
264700000000      *
264800000000      * COLLI: da partire
264900970211     C                   Z-ADD     0             SATCPE
265000970211     C                   Z-ADD     0             SATCPD
265100970211     C                   Z-ADD     0             SATCPC
265200970211      * COLLI:
265300970211     C                   Z-ADD     C2P(1)        SATCDE
265400970211     C                   Z-ADD     C2P(2)        SATCDD
265500970211     C                   Z-ADD     C2P(3)        SATCDC
265600970211      * PESO: da partire
265700970211     C                   Z-ADD     0             SATPPE
265800970211     C                   Z-ADD     0             SATPPD
265900970211     C                   Z-ADD     0             SATPPC
266000000000      * PESO: fino a 35 quintali
266100970211     C                   Z-ADD     P2I(1)        SATFDE
266200970211     C                   Z-ADD     P2I(2)        SATFDD
266300970211     C                   Z-ADD     P2I(3)        SATFDC
266400940428      * PESO: (non esiste il bollettato nel periodo o nel precedente)
266500970211     C                   Z-ADD     P2P(1)        SATODE
266600970211     C                   Z-ADD     P2P(2)        SATODD
266700970211     C                   Z-ADD     P2P(3)        SATODC
266800000000      * VOLUME: da partire
266900970211     C                   Z-ADD     0             SATVPE
267000970211     C                   Z-ADD     0             SATVPD
267100970211     C                   Z-ADD     0             SATVPC
267200970211      * VOLUME
267300970211     C                   Z-ADD     V2P(1)        SATVDE
267400970211     C                   Z-ADD     V2P(2)        SATVDD
267500970211     C                   Z-ADD     V2P(3)        SATVDC
267600000000      * SPEDIZIONI: da partire
267700970211     C                   Z-ADD     0             SATSPE
267800970211     C                   Z-ADD     0             SATSPD
267900970211     C                   Z-ADD     0             SATSPC
268000970211      * SPEDIZIONI
268100970211     C                   Z-ADD     S2P(1)        SATSDE
268200970211     C                   Z-ADD     S2P(2)        SATSDD
268300970211     C                   Z-ADD     S2P(3)        SATSDC
268400000000      * SPEDIZIONI: complete
268500940509     C                   Z-ADD     S2M(1)        SATSCE
268600970211     C                   Z-ADD     S2M(2)        SATSCD
268700970211     C                   Z-ADD     S2M(3)        SATSCC
268800000000      *
268900970212     C     SATSDE        IFGT      *ZERO
269000970212     C     SATSDD        ORGT      *ZERO
269100970212     C     SATSDC        ORGT      *ZERO
269200940428     C                   MOVE      $FTR          SATFTR
269300970207     C                   MOVE      $DTR          SATDTR
269400940323      *
269500970211     C                   WRITE     FNSAT000
269600940429     C* PER LA STAMPA DEI TOTALI DI AREA
269700940429     C                   Z-ADD     1             $PRIM4
269800940505     C                   Z-ADD     1             $PRIM3
269900000000     C                   ENDIF
270000000000      *
270100000000     C                   ENDSR
270200940428      **************************************************************************
270300940428      * Stampo i totali generali
270400940428      **************************************************************************
270500940428     C     TOTGEN        BEGSR
270600940428      *
270700940428      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
270800940428      *
270900940506      * stampo i totali generali finali solo se ho stampato i transiti
271000950628    1C     *IN02         IFEQ      *ON
271100940506     C     $PRIM3        ANDEQ     1
271200940506     C* I TOTALI DI AREA SEMPRE
271300940506     C     *IN02         OREQ      *OFF
271400940506     C*
271500940428     C                   SETON                                        40
271600940428     C                   SETOFF                                       41
271700970211     C  N02              SETON                                        45
271800970211     C   02              SETOFF                                       45
271900940428     C                   Z-ADD     *ZERO         FILIAP
272000940428     C                   MOVE      *BLANKS       ORGDEP
272100940428     C   02'TOT.GENE'    CAT       'RALE':0      ORGDEP
272200940428     C  N02              MOVEL     'TOTALE'      ORGDEP
272300940428     C*
272400950118     C                   WRITE     LSC2T3
272500940428      *
272600940428      * ?  C  O   L   L  I  ?
272700970211     C                   XFOOT     C3P           COTOTP
272800970211     C                   XFOOT     C3A           $CDA
272900940428     C                   EXSR      PERCOL
273000940428      *
273100940428      * ?  P  E  S  O  ?
273200970211     C                   XFOOT     P3I           PEF35P
273300970211     C                   XFOOT     P3P           PEO35P
273400970211     C     PEF35P        ADD       PEO35P        PETOTP
273500970228     C                   XFOOT     P3A           $PDA
273600970211     C                   EXSR      PERPES
273700970211      *
273800970211      * ?  V  O  L  U  M E  ?
273900970211     C                   XFOOT     V3P           VOTOTP
274000970211     C                   XFOOT     V3A           $VDA
274100940428     C                   EXSR      PERVOL
274200940428      *
274300940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
274400970211     C                   XFOOT     S3A           $SDA
274500970305     C                   XFOOT     F3F           SPFAFF
274600940502     C                   EXSR      PERSPE
274700970305     C                   XFOOT     S3P           SPTOTP
274800970305     C                   XFOOT     S3M           SPCONP
274900970211     C                   XFOOT     S3R           $COMO1
275000970211     C     SPTOTP        ADD       $COMO1        SPTOTA
275100970211     C     SPTOTA        SUB       SPCONP        SPSPAP
275200000407     C*
275300000407      * ?VOLUME A MAGAZZINO ?
275400000407     C                   XFOOT     V3R           VOTOTM
275500940428      *
275600940428     C                   EXSR      SCRIVI
275700940509     C                   ADD       1             $RIGHE
275800940428      *
275900970211      * ------------------ ESPR./DIST./CORR. --------------------------
276000940428      *
276100970211     C                   SETOFF                                       40
276200950628    2C                   DO        3             $V
276300940428     C                   MOVE      ENS($V)       TIPOP
276400940428      *
276500940428      * ?  C  O   L   L  I  ?
276600970211     C                   Z-ADD     C3P($V)       COTOTP
276700970211     C                   Z-ADD     C3A($V)       $CDA
276800940428     C                   EXSR      PERCOL
276900940428      *
277000940428      * ?  P  E  S  O  ?
277100940428     C                   Z-ADD     P3I($V)       PEF35P
277200970211     C                   Z-ADD     P3P($V)       PEO35P
277300970211     C     PEF35P        ADD       PEO35P        PETOTP
277400970211     C                   Z-ADD     P3A($V)       $PDA
277500940428     C                   EXSR      PERPES
277600940428      *
277700940428      * ?  V  O  L  U  M E  ?
277800970211     C                   Z-ADD     V3P($V)       VOTOTP
277900970211     C                   Z-ADD     V3A($V)       $VDA
278000940428     C                   EXSR      PERVOL
278100940428      *
278200940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
278300970211     C                   Z-ADD     S3A($V)       $SDA
278400970305     C                   Z-ADD     F3F($V)       SPFAFF
278500940502     C                   EXSR      PERSPE
278600970305     C                   Z-ADD     S3P($V)       SPTOTP
278700970305     C                   Z-ADD     S3M($V)       SPCONP
278800970211     C     S3P($V)       ADD       S3R($V)       SPTOTA
278900970211     C     SPTOTA        SUB       S3M($V)       SPSPAP
279000000406      * ?VOLUME A MAGAZZINO ?
279100000406     C                   Z-ADD     V3R($V)       VOTOTM
279200940428      *
279300940428     C                   EXSR      SCRIVI
279400940428      *
279500950628    2C                   ENDDO
279600940428      *
279700950118     C  N02              WRITE     LSC2T2
279800950628    1C                   ENDIF
279900940429     C*
280000950118     C   02              WRITE     LSC2T3
280100940428     C*
280200940428     C   02              EXSR      AZZTOT
280300940428      *
280400940428     C                   ENDSR
280500000000      **************************************************************************
280600000000      * Elaborazione transiti
280700940505      * Non posso leggere per data uscita dal transito perche' spesso
280800940505      * capita che ci siano bolle con data entrata un giorno e data
280900940505      * uscita il giorno prima: per questo elaboro tutte le bolle
281000950210      * che hanno la data di entrata al transito > 0 e <=a DS1AAL
281100000000      **************************************************************************
281200000000     C     ELATRA        BEGSR
281300000000      *
281400940505     C                   Z-ADD     0             $PRIM1
281500940428     C                   MOVE      *BLANK        $BLTPR
281600940429     C                   MOVE      *BLANK        $CBLPR
281700940428     C                   MOVE      'T'           $FLAG
281800000000      *
281900000000      * Leggo il file delle bolle in transito
282000940505     C                   Z-ADD     1             $DFV
282100020625     C     kbtt39        SETLL     FNBTT39L
282200020625     C     simfel        READe     FNBTT39L                               33
282300940428     C*
282400940428    2C     *IN33         DOWEQ     *OFF
282500950210     C     BTTDET        ANDLE     DS1AAL
282600000000      *
282700940505     C                   EXSR      ESCLU1
282800940428      *
282900940505    3C     $OK           IFEQ      'S'
283000940505     C                   Z-ADD     1             $PRIM1
283100000000      *
283200000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
283300000000      * poiche' lo dovro aggiornare
283400970211     C                   CLEAR                   FNSAT000
283500000000     C                   Z-ADD     SIMFEL        SATFLE
283600950210     C                   Z-ADD     DS1ADA        SATDRE
283700940502     C                   Z-ADD     SIMFEL        SATLNP
283800950118     C                   Z-ADD     BTPTFA        SATTFA
283900000000     C                   MOVE      'T'           SATTRE
284000970205     C     FNSA1K        CHAIN     FNSAT02L                           37
284100940428      *
284200021217      * se è la prima volta che leggo la bolla carico i dati: VOL e PKG
284300021217    4C     $BOLLA        IFEQ      '1'
284400021217     C                   EXSR      DATBOT
284500021217    4C                   ENDIF
284600021217      *
284700021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
284800030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
284900030213      * Peso medio
285000030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
285100030213if  1c***                if        Wsipes = 'S'
285200030213     c***                z-add     BTTpuc        $Peso
285300030213   x1c***                else
285400021220     c                   z-add     Wpesmed       $Peso
285500030213e   1c***                endif
285600021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
285700030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
285800030213      * Volume medio
285900030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
286000030213if  1c***                if        Wsivol = 'S'
286100030213     c***                z-add     BTTvuc        $Volum
286200030213   x1c***                else
286300021220     c                   z-add     Wvolmed       $Volum
286400030213e   1c***                endif
286500000000      *
286600970211      * Verifico il tipo spedizione ESPR./DIST./CORR.
286700950201     C                   Z-ADD     3             $T
286800950118    4C     BTPTSP        IFEQ      'E'
286900000000     C                   Z-ADD     1             $T
287000940428    4C                   ENDIF
287100950201    4C     BTPTSP        IFEQ      'D'
287200940323     C                   Z-ADD     2             $T
287300940428    4C                   ENDIF
287400000000      *
287500000000      * Ho due controlli da fare per sapere se il collo e' a terra:
287600940428      *    1)  BTTDUT = 0
287700940428      *    2)  BTTDUT > Data spedizione
287800000000     C                   SETOFF                                       36
287900940428     C     BTTDUT        IFEQ      0
288000950210     C     BTTDUT        ORGT      DS1AAL
288100000000     C                   SETON                                        36
288200000000     C                   ENDIF
288300940506      *
288400000000      * Sommo solo se il collo risulta a terra
288500940428    4C     *IN36         IFEQ      *ON
288600940506      *
288700940506      * ?  CALCOLI  SUI  COLLI  ?
288800940506      *
288900940509     C                   ADD       1             C2P($T)
289000940428     C                   ADD       1             C0P($T)
289100000000      *
289200000000      * ?  CALCOLI  SUL  PESO  ?
289300000000      *
289400000000      * Verifico il peso di 35 quintali
289500021217    5***C     BTPPKB        IFLE      3500
289600021217    5C     Wpeso         IFLE      3500
289700940509     C                   ADD       $PESO         P2I($T)
289800000000     C                   ADD       $PESO         P0I($T)
289900970211     C                   ELSE
290000970211      * Oltre 35 quintali:                                             (35ON)
290100970211     C                   ADD       $PESO         P2P($T)
290200970211     C                   ADD       $PESO         P0P($T)
290300940428    5C                   ENDIF
290400000000      *
290500000000      *
290600000000      * ?  CALCOLI  SUL  VOLUME  ?
290700000000      *
290800021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
290900021217     **5C     BTTVUC        IFEQ      *ZERO
291000021217     *** *
291100021217     ***C     BTPVLB        DIV(H)    BTPNCL        $VOLUM
291200021217     *** *
291300021217     **5C                   ELSE
291400021217     ***C                   Z-ADD     BTTVUC        $VOLUM
291500021217     **5C                   ENDIF
291600000000      *
291700940509     C                   ADD       $VOLUM        V2P($T)
291800940428     C                   ADD       $VOLUM        V0P($T)
291900000000      *
292000000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
292100000000      *
292200000000      * Visto che le spedizioni non riesco a calcolarle ora, mi limito a tenere
292300000000      * aggiornato il file di work con le bolle
292400950118     C                   Z-ADD     BTPLNP        WRKLNP
292500000000     C                   MOVE      *BLANK        WRKCSE
292600970409     C     FNWR2K        CHAIN     FNWRK00F                           99
292700940428    5C     *IN99         IFEQ      *OFF
292800000000     C                   ADD       1             WRKNC1
292900000000     C                   EXCEPT    AGGIO
293000940428   X5C                   ELSE
293100950118     C                   MOVEL     'T'           WTRANS
293200940503     C                   EXSR      WRIWRK
293300940428    5C                   ENDIF
293400940428      *
293500040921     C                   MOVE      $FTR          SATFTR
293600040921     C                   MOVE      $DTR          SATDTR
293700940428    5C     *IN37         IFEQ      *ON
293800040921     C***                MOVE      $FTR          SATFTR
293900040921     C***                MOVE      $DTR          SATDTR
294000970211     C                   WRITE     FNSAT000
294100940428   X5C                   ELSE
294200970211     C                   UPDATE    FNSAT000
294300940428    5C                   ENDIF
294400000000      *
294500940428    4C                   ENDIF
294600940428    3C                   ENDIF
294700000000      *
294800020625     C     simfel        READE     FNBTT39L                               33
294900940428    2C                   ENDDO
295000940506     C*
295100000000     C     $PRIM1        IFNE      *ZERO
295200940506     C                   EXSR      CALSPE
295300000000     C                   EXSR      WR2SAT
295400000000     C                   ENDIF
295500000000      *
295600000000     C                   ENDSR
295700940503      **************************************************************************
295800000411      * elimino i terminal che non hanno VOLUMI TOT A MAG>0
295900940503      **************************************************************************
296000940503     C     ELATER        BEGSR
296100950210     C                   Z-ADD     DS1FEL        $AREA
296200940503     C                   CLEAR                   $TFA
296300940503     C                   SETOFF                                       33
296400940503     C*
296500940503     C* ELABORO TERMINAL PER TERMINAL
296600940503     C                   EXSR      IMPTFA
296700940503     C*
296800940503    2C     *IN33         DOWEQ     *OFF
296900940503     C                   Z-ADD     *ZERO         $PRIM1
297000940503      *
297100970211     C     TASATK        READE(N)  FNSAT03L                               33
297200940503     C                   MOVEL     SATTRE        $TRE
297300940503     C*
297400940503    3C     *IN33         DOWEQ     *OFF
297500940503     C*
297600940503    4C     $TRE          IFNE      SATTRE
297700940503     C     $PRIM1        ANDEQ     0
297800970211     C     TRSATK        SETLL     FNSAT03L
297900970211     C     TRSATK        READE     FNSAT03L                               33
298000940503     C*
298100940503    5C     *IN33         DOWEQ     *OFF
298200970211     C                   DELETE    FNSAT003
298300970211     C     TRSATK        READE     FNSAT03L                               33
298400940503    5C                   ENDDO
298500940503     C                   MOVEL     SATTRE        $TRE
298600940503     C                   CLEAR                   $PRIM1
298700940503    4C                   ENDIF
298800940503     C*
298900000411     C* VEDO SE C'E' ALMENO UN VOLUME A MAGAZZINO
299000000411    4C     SATVNE        IFGT      0
299100000411     C     SATVND        ORGT      0
299200000411     C     SATVNC        ORGT      0
299300940503     C                   Z-ADD     1             $PRIM1
299400940503    4C                   ENDIF
299500970211     C     TASATK        READE(N)  FNSAT03L                               33
299600940503    3C                   ENDDO
299700940503     C* ELABORO L'ULTIMO
299800940503    3C     $PRIM1        IFEQ      0
299900970211     C     TRSATK        SETLL     FNSAT03L
300000970211     C     TRSATK        READE     FNSAT03L                               33
300100970211     C*
300200940503    4C     *IN33         DOWEQ     *OFF
300300970211     C                   DELETE    FNSAT003
300400970211     C     TRSATK        READE     FNSAT03L                               33
300500940503    4C                   ENDDO
300600940503    3C                   ENDIF
300700940503     C*
300800940503     C                   EXSR      IMPTFA
300900940503    2C                   ENDDO
301000940504     C*
301100940504     C                   ENDSR
301200000000      **************************************************************************
301300000000      * Stampo i terminal di arrivo
301400000000      **************************************************************************
301500000000     C     STATER        BEGSR
301600940429     C                   SETOFF                                       4333
301700940429     C                   SETON                                        30
301800950210     C                   Z-ADD     DS1FEL        $AREA
301900940429     C                   CLEAR                   $TFA
302000940509     C                   CLEAR                   $RIGHE
302100940429     C* ELABORO AREA PER AREA
302200940429     C  N39              EXSR      IMPARE
302300940429     C*
302400940429    2C     *IN33         DOWEQ     *OFF
302500940429     C*
302600940429     C* ELABORO TERMINAL PER TERMINAL
302700940429     C                   EXSR      IMPTFA
302800000000      *
302900940429    3C     *IN33         DOWEQ     *OFF
303000940429     C                   Z-ADD     *ZERO         $PRIM1
303100940429     C                   Z-ADD     *ZERO         $PRIM3
303200000000      *
303300970211     C     ARSATK        READE(N)  FNSAT03L                               33
303400940429     C*
303500940429    4C     *IN33         DOWEQ     *OFF
303600000000      *
303700940429     C* ELABORO SE DATA COMPRESA
303800940429     C* E, SE IMMESSA SOLTANTO UNA LINEA, SE E' LA LINEA RICHIESTA
303900970211    5C     SATDRE        IFLE      DS1AAL
304000940429     C*
304100940429     C* SE DA STAMPE GIORNALIERE ELABORO SLTANTO LE LINEE IN LIN
304200940429     C     SATLNP        LOOKUP    LIN                                    99
304300940429     C   99              EXSR      SERTRA
304400940429     C*
304500940429   X5C                   ELSE
304600940429     C                   SETON                                        33
304700940429    5C                   ENDIF
304800940429     C*
304900970211     C     ARSATK        READE(N)  FNSAT03L                               33
305000940429    4C                   ENDDO
305100940429      *
305200940429     C* Stampo le bolle dell'area
305300940429    4C     $PRIM1        IFEQ      1
305400940429     C                   EXSR      TOTFIL
305500940429     C                   EXSR      AZZFIL
305600940429    4C                   ENDIF
305700940429      *
305800940429      * Stampo le bolle in transito
305900940429    4C     $PRIM3        IFEQ      1
306000940429     C                   EXSR      TOTTRA
306100940429    4C                   ENDIF
306200940429     C*
306300940429     C* Elaboro nuovo terminal
306400940429     C                   EXSR      IMPTFA
306500940429    3C                   ENDDO
306600940429      *
306700940509     C                   Z-ADD     1             $PRIM3
306800970226     C                   SETON                                        0204
306900940429     C                   EXSR      TOTGEN
307000970226     C                   SETOFF                                       0204
307100940429     C* ELABORO NUOVA AREA
307200940429     C  N39              EXSR      IMPARE
307300940429    2C                   ENDDO
307400940429     C*
307500940429     C                   ENDSR
307600000000      **************************************************************************
307700000000      * Una volta letto il record verifico se da includere o no
307800000000      *
307900940428      * Includo se:
308000940428      * e' entrato al transito
308100940428      *  e non e' ancora uscito o e' uscito con data = > a  data elabor
308200940505      *            BTTDUT = 0
308300000000      *   oppure:
308400950210      *       AND  BTTDUT >= DS1ADAL
308500940428      *
308600940428      * escludo sempre i recuperi
308700000000      **************************************************************************
308800000000     C     ESCLU1        BEGSR
308900000000      *
309000000000     C                   MOVE      'N'           $OK
309100000000      *
309200000000     C     BTTDUT        IFEQ      *ZERO
309300950210     C     BTTDUT        ORGE      DS1AAL
309400000000     C                   MOVE      'S'           $OK
309500940428     C                   ENDIF
309600000000      *
309700000000      * Solo se superati i test ne eseguo altri
309800000000     C     $OK           IFEQ      'S'
309900940505      *
310000940505      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
310100940505      * come test ma mi permette di saltare delle chain)
310200950118    3C     $BTTAT        IFNE      $BLTPR
310300020625     C     FNBTPK        CHAIN     FNBTP11L                           99
310400950118     C  N99              MOVE      $BTTAT        $BLTPR
310500940505     C  N99              MOVEL     '1'           $BOLLA
310600940505     C   99              MOVEL     'N'           $OK
310700940505    3C                   ENDIF
310800000000      *
310900000000      * Eseguo la chain su TABEL solo se diverso dal precedente
311000950118     C  N99BTPCBO        IFNE      $CBLPR
311100950210     C                   MOVE      '3A'          KCOD
311200950210     C                   MOVE      *BLANKS       KKEY
311300950210     C                   MOVEL     BTPCBO        KKEY
311400950210     C     KTAB1         CHAIN     TABEL00F                           99
311500000000     C  N99              MOVEL     TBLUNI        DS3A
311600950118     C  N99              MOVE      BTPCBO        $CBLPR
311700000000     C   99              CLEAR                   DS3A
311800000000     C                   ENDIF
311900000000      *
312000000000      * Non considero i recuperi
312100940428     C     §3ARBL        IFEQ      'R'
312200000000     C                   MOVE      'N'           $OK
312300000000     C                   ENDIF
312400000000      *
312500000000     C                   ENDIF
312600000000      *
312700000000     C                   ENDSR
312800000000      **************************************************************************
312900970211      * RISTAMPA: lettura dal file statistiche (FNSAT00F) e output
313000000000      **************************************************************************
313100000000     C     RISTAM        BEGSR
313200000000      *
313300940502     C                   SETOFF                                       33
313400940428     C                   SETON                                        43
313500940429     C                   CLEAR                   $TFA
313600940509     C                   CLEAR                   $RIGHE
313700950210     C                   MOVEL     DS1FEL        $AREA
313800940429     C* LANCIO DA FILTRO
313900950210    1C     DS1FGS        IFGT      0
314000950210     C                   MOVEL     DS1LNP        SATLNP
314100940429     C*
314200940429     C* SE NON IMMESSA L'AREA LA CERCO
314300940429     C  N39              EXSR      IMPARE
314400940429     C*
314500940429    2C     *IN33         DOWEQ     *OFF
314600940429     C                   Z-ADD     *ZERO         $PRIM2
314700940429     C                   Z-ADD     *ZERO         $PRIM3
314800940429     C                   Z-ADD     *ZERO         $PRIM4
314900940429     C*
315000940429     C* SE NON IMMESSA LA LNP LA CERCO
315100940505     C                   EXSR      IMPLNP
315200000000      *
315300940429    3C     *IN33         DOWEQ     *OFF
315400940429     C                   Z-ADD     *ZERO         $PRIM1
315500940429     C* LEGGO IL FILE PER LNP
315600940429     C                   EXSR      LEGSAT
315700940429     C* cerco la nuova lnp
315800940429     C  N29              EXSR      IMPLNP
315900940429    3C                   ENDDO
316000940429     c*
316100940429     C* stampa finale dei C/S transiti e totale
316200940429     C                   EXSR      STAFIN
316300940429     C* cerco la nuova area
316400940429     C  N39              EXSR      IMPARE
316500940429    2C                   ENDDO
316600000000      *
316700950210   X1C                   ELSE
316800940429      *
316900940429      * Da stampe giornaliere
317000940429     C                   Z-ADD     1             $X
317100940429     C*
317200940506     C                   Z-ADD     *ZERO         $PRIM2            1 0
317300940506     C                   Z-ADD     *ZERO         $PRIM3            1 0
317400940506     C                   Z-ADD     *ZERO         $PRIM4            1 0
317500000912     C                   CLEAR                   WSTAF
317600940429    2C     LIN($X)       DOWGT     0
317700940429     C                   Z-ADD     LIN($X)       $LNP
317800940506     C                   Z-ADD     LIN($X)       $FILPR
317900940505     C                   Z-ADD     *ZERO         $PRIM1            1 0
318000000912     C* ESCLCUDO I P.O. POSTE
318100000912      * Escludo bolle LNP Poste
318200020813     ***C                   CLEAR                   §OGPT
318300020813     C                   clear                   OG143
318400000912     C     $FILPR        CHAIN     AZORG01L                           82
318500000912     C  N82              MOVEL     ORGDE3        OG143
318600000912     C**
318700020813   2A***C     §OGPT         IFNE      'S'
318800020813    2C                   IF        §OGntw <> 'PPT'
318900000912     C* SE ELABORATA ALMENO UNA LINEA, STAMPO ANCHE I TOTALI FINALI
319000000912     C                   MOVEL     'S'           WSTAF             1
319100000912     C*
319200970205     C     DTSATK        SETLL     FNSAT02L
319300940429     C* LEGGO IL FILE PER LINEA
319400940429     C                   EXSR      LEGSAT
319500000912     C                   ENDIF
319600940429     C                   ADD       1             $X
319700940429    2C                   ENDDO
319800940429     c*
319900000912     C     WSTAF         IFEQ      'S'
320000940429     C                   EXSR      STAFIN
320100940429    1C                   ENDIF
320200000912    1C                   ENDIF
320300940429     C*
320400940429     C                   ENDSR
320500940429      **************************************************************************
320600940429      * IMPOSTO L'AREA, LE CERCO MAN MANO                              i
320700940429      **************************************************************************
320800940429     C     IMPARE        BEGSR
320900970205     C     $AREA         SETGT     FNSAT02L
321000970205     C                   READ(N)   FNSAT02L                               33
321100940429     C*
321200940429     C     *IN33         IFEQ      *OFF
321300940429     C                   MOVEL     SATFLE        $AREA
321400940429     C                   ENDIF
321500940429     C                   ENDSR
321600940429      **************************************************************************
321700940429      * IMPOSTO LA LINEA DI PARTENZA, LA CERCO MAN MANO                i
321800940429      **************************************************************************
321900940429     C     IMPLNP        BEGSR
322000970205     C  N29LPSATK        SETGT     FNSAT02L
322100970205     C  N29ARSATK        READE(N)  FNSAT02L                               33
322200000619     C                   CLEAR                   WTROVA
322300940429     C*
322400000619    1C     WTROVA        DOWEQ     ' '
322500000619    2C     *IN33         IFEQ      *OFF
322600000619     C     SATLNP        LOOKUP    LIN                                    99
322700000619    3C     *IN99         IFEQ      *ON
322800940429     C                   MOVEL     SATLNP        $LNP
322900940429     C                   MOVEL     SATLNP        $FILPR
323000000619     C                   MOVEL     '1'           WTROVA            1
323100970205     C     DTSATK        SETLL     FNSAT02L
323200000619   X3C                   ELSE
323300000619     C  N29ARSATK        READE(N)  FNSAT02L                               33
323400000619    3C                   ENDIF
323500000619     C**
323600000619   X2C                   ELSE
323700000619     C                   MOVEL     '1'           WTROVA
323800000619    2C                   ENDIF
323900000619    1C                   ENDDO
324000000619     C*
324100940429     C                   ENDSR
324200940429      **************************************************************************
324300940429      * IMPOSTO IL TERMINAL, LO CERCO MAN MANO                         i
324400940429      **************************************************************************
324500940429     C     IMPTFA        BEGSR
324600970211     C     ARSATK        SETGT     FNSAT03L
324700970211     C     $AREA         READE(N)  FNSAT03L                               33
324800940429     C*
324900940429     C     *IN33         IFEQ      *OFF
325000940429     C                   MOVEL     SATTFA        $TFA
325100940429     C                   MOVEL     SATTFA        $FILPR
325200970211     C     TASATK        SETLL     FNSAT03L
325300940429     C                   ENDIF
325400940429     C                   ENDSR
325500940429      **************************************************************************
325600940429      * STAMPA FINALE DEI C/S E TRANSITI                               i
325700940429      **************************************************************************
325800940429     C     STAFIN        BEGSR
325900940429     C* stampo i totali C/S
326000940506     C     *IN38         IFEQ      *OFF
326100940429     C     $PRIM2        IFEQ      1
326200940429     C                   EXSR      TOTSER
326300940429     C                   ENDIF
326400940506     C* TOTALI DI AREA
326500970226     C                   SETON                                        04
326600940505     C                   EXSR      TOTGEN
326700970226     C                   SETOFF                                       04
326800940506     C                   ENDIF
326900940502     C*
327000940502      * Terminate le letture per filiale tocca ai transiti
327100950210     C     DS1RIS        IFNE      'R'
327200940502     C                   EXSR      ELATRA
327300940503     C*
327400940505     C* CANCELLO LE RIGHE DEI TERMINAL CHE NON HANNO COLLI A TERRA
327500940503     C                   EXSR      ELATER
327600940502     C                   ENDIF
327700940502     C*
327800940429     C* stampo i totali dei TRANSITI
327900940506     C     *IN38         IFEQ      *OFF
328000000308     C     *IN27         IFEQ      *OFF
328100000308     C     DS1RIS        OREQ      'R'
328200000308     C                   EXSR      TOTTRA
328300000308     C                   ENDIF
328400940429     c* stampo il totale di area
328500940429     C     $PRIM4        IFEQ      1
328600970226     C                   SETON                                        0204
328700940429     C                   EXSR      TOTGEN
328800970226     C                   SETOFF                                       0204
328900950118     C                   WRITE     LSC2E4
329000940429     C                   ENDIF
329100940506     C                   ENDIF
329200940429     C                   ENDSR
329300940429      **************************************************************************
329400950210      * IMPOSTO L'AREA, LE CERCO MAN MANO
329500940429      **************************************************************************
329600940429     C     LEGSAT        BEGSR
329700970205     C     LPSATK        READE(N)  FNSAT02L                               33
329800940429     C*
329900940429    4C     *IN33         DOWEQ     *OFF
330000940429      *
330100970211    5C     SATDRE        IFLE      DS1AAL
330200940429      * Aggiorno i totali
330300940429     C                   EXSR      SERTRA
330400940429   x5C                   ELSE
330500940429     C                   SETON                                        33
330600940429    5C                   ENDIF
330700940429     C*
330800970205     C  N33LPSATK        READE(N)  FNSAT02L                               33
330900940429    4C                   ENDDO
331000940429     C* totali di filiale
331100940429    4C     $PRIM1        IFEQ      1
331200940429     C                   EXSR      TOTFIL
331300940429     C                   EXSR      AZZFIL
331400940429    4C                   ENDIF
331500940429     C                   ENDSR
331600000000      **************************************************************************
331700000000      * RISTAMPA: per servizi o transiti non stampo ma aggiorno i totali
331800000000      **************************************************************************
331900000000     C     SERTRA        BEGSR
332000000000      *
332100000000     C     SATTRE        IFEQ      *BLANK
332200940429     C                   Z-ADD     1             $PRIM1
332300970211     C                   ADD       SATCPE        CPA(1)
332400970211     C                   ADD       SATCPD        CPA(2)
332500970211     C                   ADD       SATCPC        CPA(3)
332600970211     C                   ADD       SATCDE        CBP(1)
332700970211     C                   ADD       SATCDD        CBP(2)
332800970228     C                   ADD       SATCDC        CBP(3)
332900970211     C                   ADD       SATCNE        CBR(1)
333000970211     C                   ADD       SATCND        CBR(2)
333100970211     C                   ADD       SATCNC        CBR(3)
333200970211     C                   ADD       SATPPE        PPA(1)
333300970211     C                   ADD       SATPPD        PPA(2)
333400970211     C                   ADD       SATPPC        PPA(3)
333500970211     C                   ADD       SATFDE        PFI(1)
333600970211     C                   ADD       SATFDD        PFI(2)
333700970211     C                   ADD       SATFDC        PFI(3)
333800970211     C                   ADD       SATFNE        PFN(1)
333900970211     C                   ADD       SATFND        PFN(2)
334000970211     C                   ADD       SATFNC        PFN(3)
334100970211     C                   ADD       SATODE        PBP(1)
334200970211     C                   ADD       SATODD        PBP(2)
334300970211     C                   ADD       SATODC        PBP(3)
334400970211     C                   ADD       SATONE        PBR(1)
334500970211     C                   ADD       SATOND        PBR(2)
334600970211     C                   ADD       SATONC        PBR(3)
334700970211     C                   ADD       SATVPE        VPA(1)
334800970211     C                   ADD       SATVPD        VPA(2)
334900970211     C                   ADD       SATVPC        VPA(3)
335000970211     C                   ADD       SATVDE        VBP(1)
335100970211     C                   ADD       SATVDD        VBP(2)
335200970211     C                   ADD       SATVDC        VBP(3)
335300970211     C                   ADD       SATVNE        VBR(1)
335400970211     C                   ADD       SATVND        VBR(2)
335500970211     C                   ADD       SATVNC        VBR(3)
335600970211     C                   ADD       SATSPE        SPA(1)
335700970211     C                   ADD       SATSPD        SPA(2)
335800970211     C                   ADD       SATSPC        SPA(3)
335900970211     C                   ADD       SATSDE        SBP(1)
336000970211     C                   ADD       SATSDD        SBP(2)
336100970211     C                   ADD       SATSDC        SBP(3)
336200970211     C                   ADD       SATSNE        SBR(1)
336300970211     C                   ADD       SATSND        SBR(2)
336400970211     C                   ADD       SATSNC        SBR(3)
336500970305     C                   ADD       SATSFE        FAF(1)
336600970305     C                   ADD       SATSFD        FAF(2)
336700970305     C                   ADD       SATSFC        FAF(3)
336800000000     C                   ADD       SATSCE        SCM(1)
336900970211     C                   ADD       SATSCD        SCM(2)
337000970211     C                   ADD       SATSCC        SCM(3)
337100000000     C                   ENDIF
337200000000      *
337300000000     C     SATTRE        IFEQ      'C'
337400940429     C                   Z-ADD     1             $PRIM2
337500970211     C                   ADD       SATCPE        C1A(1)
337600970211     C                   ADD       SATCPD        C1A(2)
337700970211     C                   ADD       SATCPC        C1A(3)
337800970211     C                   ADD       SATCDE        C1P(1)
337900970211     C                   ADD       SATCDD        C1P(2)
338000970211     C                   ADD       SATCDC        C1P(3)
338100970211     C                   ADD       SATCNE        C1R(1)
338200970211     C                   ADD       SATCND        C1R(2)
338300970211     C                   ADD       SATCNC        C1R(3)
338400970211     C                   ADD       SATPPE        P1A(1)
338500970303     C                   ADD       SATPPD        P1A(2)
338600970303     C                   ADD       SATPPC        P1A(3)
338700970211     C                   ADD       SATFDE        P1I(3)
338800970211     C                   ADD       SATFDD        P1I(2)
338900970211     C                   ADD       SATFDC        P1I(3)
339000970211     C                   ADD       SATFNE        P1N(1)
339100970211     C                   ADD       SATFND        P1N(2)
339200970211     C                   ADD       SATFNC        P1N(3)
339300970211     C                   ADD       SATODE        P1P(1)
339400970211     C                   ADD       SATODD        P1P(2)
339500970211     C                   ADD       SATODC        P1P(3)
339600970211     C                   ADD       SATONE        P1R(1)
339700970211     C                   ADD       SATOND        P1R(2)
339800970211     C                   ADD       SATONC        P1R(3)
339900970211     C                   ADD       SATVPE        V1A(1)
340000970211     C                   ADD       SATVPD        V1A(2)
340100970211     C                   ADD       SATVPC        V1A(3)
340200970211     C                   ADD       SATVDE        V1P(1)
340300970211     C                   ADD       SATVDD        V1P(2)
340400970211     C                   ADD       SATVDC        V1P(3)
340500970211     C                   ADD       SATVNE        V1R(1)
340600970211     C                   ADD       SATVND        V1R(2)
340700970211     C                   ADD       SATVNC        V1R(3)
340800970211     C                   ADD       SATSPE        S1A(1)
340900970211     C                   ADD       SATSPD        S1A(2)
341000970211     C                   ADD       SATSPC        S1A(3)
341100970211     C                   ADD       SATSDE        S1P(1)
341200970211     C                   ADD       SATSDD        S1P(2)
341300970211     C                   ADD       SATSDC        S1P(3)
341400970211     C                   ADD       SATSNE        S1R(1)
341500970211     C                   ADD       SATSND        S1R(2)
341600970211     C                   ADD       SATSNC        S1R(3)
341700970305     C                   ADD       SATSFE        F1F(1)
341800970305     C                   ADD       SATSFD        F1F(2)
341900970305     C                   ADD       SATSFC        F1F(3)
342000000000     C                   ADD       SATSCE        S1M(1)
342100970211     C                   ADD       SATSCD        S1M(2)
342200970211     C                   ADD       SATSCC        S1M(3)
342300000000     C                   ENDIF
342400000000      *
342500940429     C     SATTRE        IFEQ      'T'
342600940429     C                   Z-ADD     1             $PRIM3
342700970211     C                   ADD       SATCDE        C2P(1)
342800970211     C                   ADD       SATCDD        C2P(2)
342900970211     C                   ADD       SATCDC        C2P(3)
343000970211     C                   ADD       SATFDE        P2I(1)
343100970211     C                   ADD       SATFDD        P2I(2)
343200970211     C                   ADD       SATFDC        P2I(3)
343300970211     C                   ADD       SATODE        P2P(1)
343400970211     C                   ADD       SATODD        P2P(2)
343500970211     C                   ADD       SATODC        P2P(3)
343600970211     C                   ADD       SATVDE        V2P(1)
343700970211     C                   ADD       SATVDD        V2P(2)
343800970211     C                   ADD       SATVDC        V2P(3)
343900970211     C                   ADD       SATSDE        S2P(1)
344000970211     C                   ADD       SATSDD        S2P(2)
344100970211     C                   ADD       SATSDC        S2P(3)
344200000000     C                   ADD       SATSCE        S2M(1)
344300970211     C                   ADD       SATSCD        S2M(2)
344400970211     C                   ADD       SATSCC        S2M(3)
344500940505     C*
344600940505     C                   ELSE
344700000000      *
344800940429      *  totali generali
344900940429     C                   Z-ADD     1             $PRIM4
345000970211     C                   ADD       SATCDE        C3P(1)
345100970211     C                   ADD       SATCDD        C3P(2)
345200970211     C                   ADD       SATCDC        C3P(3)
345300970211     C                   ADD       SATFDE        P3I(1)
345400970211     C                   ADD       SATFDD        P3I(2)
345500970211     C                   ADD       SATFDC        P3I(3)
345600970212     C                   ADD       SATODE        P3P(1)
345700970212     C                   ADD       SATODD        P3P(2)
345800970212     C                   ADD       SATODC        P3P(3)
345900970211     C                   ADD       SATVDE        V3P(1)
346000970211     C                   ADD       SATVDD        V3P(2)
346100970211     C                   ADD       SATVDC        V3P(3)
346200000406     C                   ADD       SATVNE        V3R(1)
346300000406     C                   ADD       SATVND        V3R(2)
346400000406     C                   ADD       SATVNC        V3R(3)
346500970211     C                   ADD       SATSDE        S3P(1)
346600970211     C                   ADD       SATSDD        S3P(2)
346700970211     C                   ADD       SATSDC        S3P(3)
346800970211     C                   ADD       SATSNE        S3R(1)
346900970211     C                   ADD       SATSND        S3R(2)
347000970211     C                   ADD       SATSNC        S3R(3)
347100970305     C                   ADD       SATSFE        F3F(1)
347200970305     C                   ADD       SATSFD        F3F(2)
347300970305     C                   ADD       SATSFC        F3F(3)
347400000000     C                   ADD       SATSCE        S3M(1)
347500970211     C                   ADD       SATSCD        S3M(2)
347600970211     C                   ADD       SATSCC        S3M(3)
347700940505     C                   ENDIF
347800000000      *
347900000000     C                   ENDSR
348000940505      **************************************************************************
348100940505      * RISTAMPA: addiziono i transiti al totale generale              i
348200940505      **************************************************************************
348300940505     C     ADDTRA        BEGSR
348400940505     C                   Z-ADD     1             $PRIM4
348500940505     C                   ADD       C2P           C3P
348600940505     C                   ADD       P2I           P3I
348700940505     C                   ADD       P2P           P3P
348800940505     C                   ADD       V2P           V3P
348900940505     C                   ADD       S2P           S3P
349000940505     C                   ADD       S2M           S3M
349100940505     C                   ENDSR
349200000000      **************************************************************************
349300000000      * RISTAMPA: azzera i totali generali
349400000000      **************************************************************************
349500000000     C     AZZTOT        BEGSR
349600000000      *
349700970212     C                   CLEAR                   C3A
349800970212     C                   CLEAR                   C3P
349900970211     C                   CLEAR                   P3I
350000970211     C                   CLEAR                   P3P
350100970211     C                   CLEAR                   P3A
350200970211     C                   CLEAR                   V3P
350300940502     C                   CLEAR                   V3A
350400000406     C                   CLEAR                   V3R
350500970211     C                   CLEAR                   S3A
350600970211     C                   CLEAR                   S3P
350700970211     C                   CLEAR                   S3R
350800970211     C                   CLEAR                   S3M
350900970305     C                   CLEAR                   F3F
351000000000      *
351100000000     C                   ENDSR
351200000000      **************************************************************************
351300000000      * Testa l'overflow e stampa su carta
351400000000      **************************************************************************
351500000000     C     SCRIVI        BEGSR
351600940509      * PER TOTALI GENERALI SE GIA' STAMPE PIU' DI 9 RIGHE COMUNQUE
351700940509      * VADO A PAGINA NUOVA
351800940509     C     *IN03         IFEQ      *OFF
351900940509     C     *IN02         ANDEQ     *ON
352000940509     C     $RIGHE        ANDGT     9
352100940509     C                   SETON                                        30
352200940509     C                   ENDIF
352300000000      *
352400940429     C     *IN38         IFEQ      *OFF
352500940509     C*
352600940509     C     *IN30         IFEQ      *ON
352700940509     C     *IN40         ANDEQ     *ON
352800950118     C                   WRITE     LSC2T1
352900950118     C                   WRITE     LSC2T3
353000940509     C                   CLEAR                   $RIGHE
353100940509     C                   SETOFF                                       30
353200940509     C                   ENDIF
353300950118     C                   WRITE     LSC2D1
353400000000      *
353500970226     C  N04$ALMEN        IFEQ      *ZERO
353600000000     C                   Z-ADD     1             $ALMEN
353700000000     C                   ENDIF
353800940328     C                   ENDIF
353900000000      *
354000000000     C                   ENDSR
354100000000      **************************************************************************
354200000000      * Verifica se vi sono dei fogli viaggio con data partenza compresa
354300000000      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
354400000000      **************************************************************************
354500000000     C     FOGVIA        BEGSR
354600020813     ***C                   OPEN      FNFGV04L
354700020813     C                   OPEN      FNFGV06L
354800000000      *
354900020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
355000020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
355100020813     C     SIMfel        setll     FNFGV06L                               99
355200000000     C                   DO        *HIVAL
355300020813     ***C                   READ      FNFGV04L                               99
355400020813     C     SIMfel        reade     FNFGV06L                               99
355500950210     C  N99FGVDTP        IFGE      DS1ADA
355600950210     C     FGVDTP        ANDLE     DS1AAL
355700940502     C                   SETON                                        99
355800950118     C                   WRITE     LSC2E3
355900000000     C                   ENDIF
356000940502     C  N99              ENDDO
356100000000      *
356200020813     ***C                   CLOSE     FNFGV04L
356300020813     C                   CLOSE     FNFGV06L
356400000000     C                   ENDSR
356500000000      **************************************************************************
356600000000      * Aggiornamento del numero colli esaminati, nel file di work
356700950118     OFNWRK000  E            AGGIO
356800000000     O                       WRKNC1
356900000000      **************************************************************************
357000000000      *                                4         5         6         7         8
357100950118** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
357200970807OVRPRTF FNLSC2P       FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.terra
357300950210')
