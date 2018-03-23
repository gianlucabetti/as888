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
006700950627     D ENS             S              5    DIM(3) CTDATA PERRCD(3)
006800000000      * COLLI
006900970210     D CPA             S              7  0 DIM(3)
007000970210     D CBP             S              7  0 DIM(3)
007100970210     D CBR             S              7  0 DIM(3)
007200000000      * PESI
007300970210     D PPA             S              9  1 DIM(3)
007400970210     D PFI             S              9  1 DIM(3)
007500970210     D PFN             S              9  1 DIM(3)
007600970210     D PBP             S              9  1 DIM(3)
007700970210     D PBR             S              9  1 DIM(3)
007800000000      * VOLUMI
007900970210     D VPA             S              9  4 DIM(3)
008000970210     D VBP             S              9  4 DIM(3)
008100970210     D VBR             S              9  4 DIM(3)
008200000000      * SPEDIZIONI
008300970210     D SPA             S              7  0 DIM(3)
008400970210     D SBP             S              7  0 DIM(3)
008500970210     D SBR             S              7  0 DIM(3)
008600970210     D SCM             S              7  0 DIM(3)
008700970305     D FAF             S              7  0 DIM(3)
008800000000      * COLLI PER C/SERVIZIO (TOTALE AREA) -------------------------------------
008900970210AREA D C1A             S              7  0 DIM(3)
009000000000AREA D C1P             S              7  0 DIM(3)
009100000000AREA D C1R             S              7  0 DIM(3)
009200000000      * PESI PER C/SERVIZIO (TOTALE AREA)
009300970210AREA D P1A             S              9  1 DIM(3)
009400000000AREA D P1I             S              9  1 DIM(3)
009500970207AREA D P1N             S              9  1 DIM(3)
009600000000AREA D P1P             S              9  1 DIM(3)
009700000000AREA D P1R             S              9  1 DIM(3)
009800000000      * VOLUMI PER C/SERVIZIO (TOTALE AREA)
009900970210AREA D V1A             S              9  4 DIM(3)
010000000000AREA D V1P             S              9  4 DIM(3)
010100000000AREA D V1R             S              9  4 DIM(3)
010200000000      * SPEDIZIONI PER C/SERVIZIO (TOTALE AREA)
010300970210AREA D S1A             S              7  0 DIM(3)
010400000000AREA D S1P             S              7  0 DIM(3)
010500000000AREA D S1R             S              7  0 DIM(3)
010600000000AREA D S1M             S              7  0 DIM(3)
010700970305AREA D F1F             S              7  0 DIM(3)
010800000000      * COLLI PER C/SERVIZIO (PER FILIALE) -------------------------------------
010900970210FIL  D C4A             S              7  0 DIM(3)
011000000000FIL  D C4P             S              7  0 DIM(3)
011100000000FIL  D C4R             S              7  0 DIM(3)
011200000000      * PESI PER C/SERVIZIO (PER FILIALE)
011300970210FIL  D P4A             S              9  1 DIM(3)
011400000000FIL  D P4I             S              9  1 DIM(3)
011500970207FIL  D P4N             S              9  1 DIM(3)
011600000000FIL  D P4P             S              9  1 DIM(3)
011700000000FIL  D P4R             S              9  1 DIM(3)
011800000000      * VOLUMI PER C/SERVIZIO (PER FILIALE)
011900970210FIL  D V4A             S              9  4 DIM(3)
012000000000FIL  D V4P             S              9  4 DIM(3)
012100000000FIL  D V4R             S              9  4 DIM(3)
012200000000      * SPEDIZIONI PER C/SERVIZIO (PER FILIALE)
012300970210FIL  D S4A             S              7  0 DIM(3)
012400000000FIL  D S4P             S              7  0 DIM(3)
012500000000FIL  D S4R             S              7  0 DIM(3)
012600000000FIL  D S4M             S              7  0 DIM(3)
012700970305FIL  D F4F             S              7  0 DIM(3)
012800940502      *
012900940502      * COLLI PER TRANSITI (TOTALE GENERALE) -----------------------------------
013000940428GEN  D C2P             S              7  0 DIM(3)
013100000000      * PESI PER TRANSITI (TOTALE GENERALE)
013200000000GEN  D P2I             S              9  1 DIM(3)
013300940428GEN  D P2P             S              9  1 DIM(3)
013400000000      * VOLUMI PER TRANSITI (TOTALE GENERALE)
013500940428GEN  D V2P             S              9  4 DIM(3)
013600000000      * SPEDIZIONI PER TRANSITI (TOTALE GENERALE)
013700940428GEN  D S2P             S              7  0 DIM(3)
013800000000GEN  D S2M             S              7  0 DIM(3)
013900940429      * COLLI PER TOTALE AREA --------------------------------------------------
014000970211     D C3A             S              7  0 DIM(3)
014100940428     D C3P             S              7  0 DIM(3)
014200940428      * PESI PER TOTALE GENERALE
014300970211     D P3A             S              9  1 DIM(3)
014400940428     D P3I             S              9  1 DIM(3)
014500940428     D P3P             S              9  1 DIM(3)
014600940428      * VOLUMI PER TOTALE GENERALE
014700970211     D V3A             S              9  4 DIM(3)
014800940428     D V3P             S              9  4 DIM(3)
014900000331     D V3R             S              9  4 DIM(3)
015000940428      * SPEDIZIONI PER TOTALE GENERALE
015100970211     D S3A             S              7  0 DIM(3)
015200940428     D S3P             S              7  0 DIM(3)
015300940428     D S3R             S              7  0 DIM(3)
015400940428     D S3M             S              7  0 DIM(3)
015500970305     D F3F             S              7  0 DIM(3)
015600970207      * COLLI PER FILE FNSAT00F ------------------------------------------------
015700970207      * PESI PER FILE FNSAT00F
015800970207      * VOLUMI PER FILE FNSAT00F
015900970207      * SPEDIZIONI PER FILE FNSAT00F
016000000000      *
016100940504     D GME             S              1    DIM(31)
016200090108     D GMEP            S              1    DIM(31)
016300020418
016400020418     d sav_t06         S              3  0 dim(30)
016500020418
016600020418     d wc              s              3  0
016700021016
016800021016     d kTbeCod         s                   Like(TbeCod)
016900021016     d kTbeKe1         s                   Like(TbeKe1)
017000020418
017100040921     D WLBDAT          DS                  INZ
017200040921     D  G02DAT                 1      8  0
017300040921     D  G02INV                 9     16  0
017400040921     D  G02ERR                17     17
017500040921     D  G02TGI                18     22  0
017600000000      *
017700000810     D OG143         E DS
017800000810     D DS3A          E DS
017900000000     D DS3L          E DS
018000040921     D*DS5C          E DS
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
019800020813      *
019900950210     D                 DS
020000950210     D  DATADA                 1      8  0
020100950210     D  ANNODA                 1      4  0
020200950210     D  MESEDA                 5      6  0
020300950210     D  $GI                    7      8  0
020400950118      * DS per controllare il "precedente" di FNBLT
020500000000     D $BLTAT          DS
020600000000     D  BLTLNP                 1      3  0
020700950118     D  BLTAAS                 4      7  0
020800950118     D  BLTNRS                 8      9  0
020900950118     D  BLTNSP                10     16  0
021000950118      * DS per controllare il "precedente" di FNBTT
021100950118     D $BTTAT          DS
021200950118     D  BTTLNP                 1      3  0
021300950118     D  BTTAAS                 4      7  0
021400950118     D  BTTNRS                 8      9  0
021500950118     D  BTTNSP                10     16  0
021600970207      * DS per i campi del file statistiche FNSAT00F
021700000000     D                 DS
021800000000     D  C0P                    1     21  0
021900970207     D                                     DIM(3)
022000970207     D  SATCDE                 1      7  0
022100970207     D  SATCDD                 8     14  0
022200970207     D  SATCDC                15     21  0
022300000000     D                 DS
022400000000     D  C0R                    1     21  0
022500970207     D                                     DIM(3)
022600970207     D  SATCNE                 1      7  0
022700970207     D  SATCND                 8     14  0
022800970207     D  SATCNC                15     21  0
022900000000     D                 DS
023000000000     D  P0I                    1     27  1
023100970207     D                                     DIM(3)
023200970207     D  SATFDE                 1      9  1
023300970207     D  SATFDD                10     18  1
023400970207     D  SATFDC                19     27  1
023500970207     D                 DS
023600970207     D  P0N                    1     27  1
023700970207     D                                     DIM(3)
023800970207     D  SATFNE                 1      9  1
023900970207     D  SATFND                10     18  1
024000970207     D  SATFNC                19     27  1
024100000000     D                 DS
024200000000     D  P0P                    1     27  1
024300970210     D                                     DIM(3)
024400970207     D  SATODE                 1      9  1
024500970207     D  SATODD                10     18  1
024600970207     D  SATODC                19     27  1
024700000000     D                 DS
024800000000     D  P0R                    1     27  1
024900970207     D                                     DIM(3)
025000970207     D  SATONE                 1      9  1
025100970207     D  SATOND                10     18  1
025200970207     D  SATONC                19     27  1
025300000000     D                 DS
025400000000     D  V0P                    1     27  4
025500970207     D                                     DIM(3)
025600970207     D  SATVDE                 1      9  4
025700970207     D  SATVDD                10     18  4
025800970207     D  SATVDC                19     27  4
025900000000     D                 DS
026000000000     D  V0R                    1     27  4
026100970207     D                                     DIM(3)
026200970207     D  SATVNE                 1      9  4
026300970207     D  SATVND                10     18  4
026400970207     D  SATVNC                19     27  4
026500000000     D                 DS
026600000000     D  S0P                    1     21  0
026700970207     D                                     DIM(3)
026800970207     D  SATSDE                 1      7  0
026900970207     D  SATSDD                 8     14  0
027000970207     D  SATSDC                15     21  0
027100000000     D                 DS
027200000000     D  S0R                    1     21  0
027300970207     D                                     DIM(3)
027400970207     D  SATSNE                 1      7  0
027500970207     D  SATSND                 8     14  0
027600970207     D  SATSNC                15     21  0
027700970305     D                 DS
027800970305     D  F0F                    1     21  0
027900970305     D                                     DIM(3)
028000970305     D  SATSFE                 1      7  0
028100970305     D  SATSFD                 8     14  0
028200970305     D  SATSFC                15     21  0
028300000000     D                 DS
028400000000     D  S0M                    1     21  0
028500000000     D                                     DIM(3)
028600000000     D  SATSCE                 1      7  0
028700970207     D  SATSCD                 8     14  0
028800970207     D  SATSCC                15     21  0
028900970210     D                 DS
029000970210     D  CDT                    1     21  0
029100970210     D                                     DIM(3)
029200970210     D  SATCPE                 1      7  0
029300970210     D  SATCPD                 8     14  0
029400970211     D  SATCPC                15     21  0
029500970210     D                 DS
029600970210     D  PDT                    1     27  1
029700970210     D                                     DIM(3)
029800970210     D  SATPPE                 1      9  1
029900970210     D  SATPPD                10     18  1
030000970210     D  SATPPC                19     27  1
030100970210     D                 DS
030200970210     D  VDT                    1     27  4
030300970210     D                                     DIM(3)
030400970210     D  SATVPE                 1      9  4
030500970210     D  SATVPD                10     18  4
030600970210     D  SATVPC                19     27  4
030700970210     D                 DS
030800970210     D  SDT                    1     21  0
030900970210     D                                     DIM(3)
031000970210     D  SATSPE                 1      7  0
031100970210     D  SATSPD                 8     14  0
031200970210     D  SATSPC                15     21  0
031300020813      *
031400020813      * Varibaili
031500020813      *
031600020813     D SAVRIS          S                   LIKE(DS1RIS) inz
031700020813     D $CDA            S              7  0              inz
031800020813     D $VDA            S              9  4              inz
031900020813     D $PDA            S              9  1              inz
032000020813     D $SDA            S              7  0              inz
032100020813     D $TFA            S                   LIKE(BLPTFA) inz
032200020813     D $AREA           S                   LIKE(DS1FEL) inz
032300020813     D $LNP            S                   LIKE(DS1LNP) inz
032400020813     D $BLTPR          S                   LIKE($BLTAT) inz
032500020813     D $FILPR          S                   LIKE(BLPLNP) inz
032600020813     D $CBLPR          S                   LIKE(BLPCBO) inz
032700020813     D $NFVPR          S                   LIKE(BLPNFV) inz
032800020813     D $DTRIT          S                   LIKE(BLPDRT) inz
032900020813     D $CDPDR          S                   LIKE(BLPPDR) inz
033000020813     D $FLAG           S                   LIKE(WRKFLG) inz
033100020813     D $FLAG2          S                   LIKE(WRKFLG) inz
033200020813     D $DFV            S                   LIKE(FGVDFV) inz
033300020813     D $FTR            S                   LIKE(SATFTR) inz
033400020813     D $DTR            S                   LIKE(SATDTR) inz
033500020813     D $TRE            S                   LIKE(SATTRE) inz
033600020813     D $COMOD          S             15  6 INZ
033700020813     D $COMO1          S             15  6 INZ
033800020813     D*
033900020813     D KCOD            S                   LIKE(TBLCOD) inz
034000020813     D KKEY            S                   LIKE(TBLKEY) inz
034100021217      *
034200021217     d $Volum          s             12  4 inz
034300021217     d $Peso           s              8  1 inz
034400030213     d*** WsiVOL          s              1    inz
034500030213     d*** WsiPES          s              1    inz
034600021217     d Wvolu           s                   like(BLPvlf) inz
034700021217     d Wpeso           s                   like(BLPpkf) inz
034800021220     d Wvolmed         s                   like($Volum) inz
034900021220     d Wpesmed         s                   like($Peso)  inz
035000090415     D ktrc            S                   LIKE(statrc) inz('T')
035100090415     D kfil            S                   LIKE(stafil) inz(000)
035200940502      **************************************************************************
035300940502      *      RIEPILOGO INDICATORI
035400940502      **************************************************************************
035500940502      * 01    - ON: LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE = 0
035600940502      *       - OFF:LEGGO IL DETTAGLIO CON DATA PARTENZA MERCE > D.ELAB
035700940502      * 02    - ON: TOTALE GENERALE
035800940502      *       - OFF:TOTALE DI AREA
035900970226      * 03    - STAMPA DEI TOTALI TRANSITI
036000970226      * 04    - NON MEMORIZZA $ALMEN NELLA ROURINE DI TOTGEN
036100940502      * 05    - STAMPE GIORNALIERE
036200970205      * 27    - OFF: DEVO STAMPARE LA RIGA DEI TRANASITI
036300940502      * 29    - ON: IMMESSA UNA LINEA DI PARTENZA
036400940502      * 30    - ECCEDENZA PAGINA
036500940502      * 33    - LETTURA FILES
036600940502      * 34    - BOLLA IN C/SERVIZIO
036700940502      * 35    - ON: BOLLA BOLLETTATA NEL PERIODO
036800940502      *       - OFF:BOLLA BOLLETTATA NEL PERIODO PRECEDENTE
036900940502      * 36    - COLLO A TERRA
037000970211      * 37    - LETTURA SUL FILES FNSAT
037100950210      * 38    - DS1RIS= ' ' --> NON DEVO STAMPARE
037200940502      * 39    - ON: IMMESSO CODICE AREA
037300940502      * 40    - ON: STAMPA LA LILIALE CON DECODIFICA
037400940502      *       - OFF:STAMPA LA DICITURA DEL SERVIZIO BOLLE
037500940502      * 41    - ON: STAMPA SOLTANTO LA FILIALE (+ 40 ON)
037600940502      * 43    - ON: STAMPO LA PARTE RELATIVA ALLE LINEE DI PARTENZA
037700940502      *       - OFF:STAMPO LA PARTE RELATIVA AI TERMINAL DI ARRIVO
037800970211      * 45    - STAMPA LE PERCENTUALI SUL DA PARTIRE
037900950118      * 99    - DI COMODO
038000940502      **************************************************************************
038100000000     C     *ENTRY        PLIST
038200000000     C                   PARM                    KPJBA
038300950210     C                   MOVEL     KPJBU         DSLS01
038400950210      *
038500950210      * DATA FINALE AA/MM/GG (6 CARATTERI)
038600090108     C                   Z-ADD     DS1aal        DATADA
038700000000      *
038800090415     c     KSTA          Klist
038900090415     c                   Kfld                    Ktrc
039000090415     c                   Kfld                    DS1FEL
039100090415     c                   Kfld                    Kfil
039200090415     c                   Kfld                    ds1ada
039300950118     C     FNBLTK        KLIST
039400940428     C                   KFLD                    LIN($X)
039500940428     C                   KFLD                    $DFV
039600970205     C     FNBLPK        KLIST
039700950118     C                   KFLD                    BLTAAS
039800000000     C                   KFLD                    BLTLNP
039900000000     C                   KFLD                    BLTNRS
040000000000     C                   KFLD                    BLTNSP
040100950118     C     FNBTPK        KLIST
040200020625     c                   kfld                    bttflp
040300950118     C                   KFLD                    BTTAAS
040400950118     C                   KFLD                    BTTLNP
040500950118     C                   KFLD                    BTTNRS
040600950118     C                   KFLD                    BTTNSP
040700950118     C     AZCLNK        KLIST
040800940504     C                   KFLD                    SIMFEL
040900940504     C                   KFLD                    $TFA
041000940504     C                   KFLD                    ANNODA
041100940504     C                   KFLD                    MESEDA
041200950118     C     FNWRKK        KLIST
041300000000     C                   KFLD                    $FLAG
041400000000     C                   KFLD                    $FILPR
041500950118     C     FNWR1K        KLIST
041600000000     C                   KFLD                    $FLAG
041700000000     C                   KFLD                    WRKLNP
041800940503     C                   KFLD                    BLPAAS
041900940503     C                   KFLD                    BLPNRS
042000940503     C                   KFLD                    BLPNSP
042100000000     C                   KFLD                    WRKCSE
042200970409     C     FNWR2K        KLIST
042300970409     C                   KFLD                    $FLAG
042400970409     C                   KFLD                    WRKLNP
042500970409     C                   KFLD                    BTPAAS
042600970409     C                   KFLD                    BTPNRS
042700970409     C                   KFLD                    BTPNSP
042800970409     C                   KFLD                    WRKCSE
042900970205     C     FNSATK        KLIST
043000970205     C                   KFLD                    SIMFEL
043100970205     C                   KFLD                    LIN(XX)
043200970205     C                   KFLD                    DS1AAL
043300970205     C     FNSA1K        KLIST
043400000000     C                   KFLD                    SATFLE
043500000000     C                   KFLD                    SATTFA
043600940429     C                   KFLD                    SATLNP
043700000000     C                   KFLD                    SATDRE
043800000000     C                   KFLD                    SATTRE
043900970205     C     DTSATK        KLIST
044000940429     C                   KFLD                    $AREA
044100940429     C                   KFLD                    $TFA
044200940429     C                   KFLD                    $LNP
044300970211     C                   KFLD                    DS1ADA
044400970205     C     LPSATK        KLIST
044500940429     C                   KFLD                    $AREA
044600940429     C                   KFLD                    $TFA
044700940429     C                   KFLD                    $LNP
044800970205     C     ARSATK        KLIST
044900940429     C                   KFLD                    $AREA
045000940429     C                   KFLD                    $TFA
045100970211     C     TASATK        KLIST
045200940429     C                   KFLD                    $AREA
045300940429     C                   KFLD                    $TFA
045400970211     C                   KFLD                    DS1ADA
045500970211     C     TRSATK        KLIST
045600940503     C                   KFLD                    $AREA
045700940503     C                   KFLD                    $TFA
045800970211     C                   KFLD                    DS1ADA
045900940503     C                   KFLD                    $TRE
046000020625     c     kbtt39        klist
046100020625     c                   kfld                    simfel
046200020625     c                   kfld                    $dfv
046300021016
046400021016     c     Ktbe          Klist
046500021016     c                   Kfld                    kTbeCod
046600021016     c                   Kfld                    kTbeKe1
046700000000      *
046800020813     ***C     *LIKE         DEFINE    DS1RIS        SAVRIS
046900020813     ***C     *LIKE         DEFINE    CPA(1)        $CDA
047000020813     ***C     *LIKE         DEFINE    VPA(1)        $VDA
047100020813     ***C     *LIKE         DEFINE    PPA(1)        $PDA
047200020813     ***C     *LIKE         DEFINE    SPA(1)        $SDA
047300020813     ***C     *LIKE         DEFINE    BLPTFA        $TFA
047400020813     ***C     *LIKE         DEFINE    DS1FEL        $AREA
047500020813     ***C     *LIKE         DEFINE    DS1LNP        $LNP
047600020813     ***C     *LIKE         DEFINE    $BLTAT        $BLTPR
047700020813     ***C     *LIKE         DEFINE    BLPLNP        $FILPR
047800020813     ***C     *LIKE         DEFINE    BLPCBO        $CBLPR
047900020813     ***C     *LIKE         DEFINE    BLPNFV        $NFVPR
048000020813     ***C     *LIKE         DEFINE    BLPDRT        $DTRIT
048100020813     ***C     *LIKE         DEFINE    BLPPDR        $CDPDR
048200020813     ***C     *LIKE         DEFINE    WRKFLG        $FLAG
048300020813     ***C     *LIKE         DEFINE    WRKFLG        $FLAG2
048400020813     ***C     *LIKE         DEFINE    FGVDFV        $DFV
048500020813     ***C     *LIKE         DEFINE    SATFTR        $FTR
048600020813     ***C     *LIKE         DEFINE    SATDTR        $DTR
048700020813     ***C     *LIKE         DEFINE    SATTRE        $TRE
048800020813     ***C                   Z-ADD     *ZERO         $COMOD           15 6
048900021016     C                   Z-ADD     *ZERO         $COMO1           15 6
049000021016     C                   Z-ADD     1             CODUT
049100000000      **************************************************************************
049200000000      *              M A I N      L I N E
049300000000      **************************************************************************
049400940429     C                   MOVE      'RISTAMPA'    STRIP
049500940429     C*
049600950210    1C     DS1RIS        IFNE      'R'
049700940429     C                   MOVE      'STAMPA  '    STRIP
049800940502    1C                   ENDIF
049900070522     c* Carico la tabella VPO per vedere da quando cambia il calcolo
050000070522     c*  della affidabilità partenza
050100070522     C                   MOVEL     'VPO'         kTBECOD
050200070522     C                   MOVEL(P)  'DECO'        kTBEKE1
050300070522     C     KTBE          CHAIN(N)  TNTBE01L                           21
050400070522     C  N21              MOVEL     TBEUNI        DVPODECO
050500070522     C*
050600070522     c
050700000000      *
050800000000      * Se la data AL e' vuota la imposto come la data DAL
050900950210     C     DS1AAL        IFEQ      *ZERO
051000950210     C                   Z-ADD     DS1ADA        DS1AAL
051100950210     C                   Z-ADD     DS1GDA        DS1GAL
051200000000     C                   ENDIF
051300940428     C*
051400940428     C                   SETON                                        43
051500940401      *
051600950210      * Accendo un indicatore se non devo stampare il tabulato (DS1RIS=' ')
051700950210     C     DS1RIS        COMP      *BLANKS                                38
051800940429      *
051900940429      * 39 ON - Immessa l'area
052000950210    3C     DS1FEL        IFGT      0
052100940429     C                   SETON                                        39
052200940429    3C                   ENDIF
052300000000      *
052400950210      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
052500950210     C                   EXSR      COMUNE
052600950210      *
052700000000     C                   SETON                                        LR
052800020212     C*****************************************************************
052900020212     C*  PARTE COMUNE STAMPE GIORNALIERE
053000020212     C*
053100020212     C*  . REPERIMENTO 'SIMFEL'
053200020212     C*  . CARICAMENTO '£1'
053300020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
053400020212     C*  . CODA DI STAMPA ('£OUTQ')
053500020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
053600020212     C*  . MODULO PER LA STAMPA ('£MOD')
053700020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
053800020212     C*****************************************************************
053900020212     C     COMUNE        BEGSR
054000020212     C*
054100020212     C*****************************************************************
054200020212     C*       RIEPILOGO INDICATORI:
054300020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
054400020212     C* 11    -           STAMPA PER 1L
054500020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
054600020212     C*****************************************************************
054700020212     C                   Z-ADD     1             CODUT
054800020212     C*
054900020212     C* Reperimento parametri aziendali
055000020212     C                   CALL      'X§PARUT'
055100020212     C                   PARM                    UT§DSE
055200020212     C                   MOVEL     RAGUT         RSUT             20
055300020212     C                   MOVEL     REC80         CNCR80
055400020212     C*
055500020212     C* Chiave di accesso per il file tabelle
055600020212     C     KTAB1         KLIST
055700020212     C                   KFLD                    CODUT
055800020212     C                   KFLD                    KCOD
055900020212     C                   KFLD                    KKEY
056000020212     C     KTAB2         KLIST
056100020212     C                   KFLD                    CODUT
056200020212     C                   KFLD                    KCOD
056300020212     C*
056400020813     ***C     *LIKE         DEFINE    TBLCOD        KCOD
056500020813     ***C     *LIKE         DEFINE    TBLKEY        KKEY
056600020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
056700020212     C*
056800020212     C* DETERMINO DATA E ORA DI ESECUZIONE
056900020212     C                   TIME                    WTIME            14 0
057000020212     C                   MOVE      WTIME         WDATE             8 0
057100020212     C                   MOVEL     WTIME         UTIME             6 0
057200040921     c*
057300040921     C                   CLEAR                   WLBDAT
057400040921     C                   MOVE      Wtime         G02DAT
057500040921     C                   CALL      'XSRDA8'
057600040921     C                   PARM                    WLBDAT
057700040921     C                   Z-ADD     G02INV        dateu             8 0
057800040922     c*
057900040922     c                   clear                   $FTR
058000040922     c                   z-add     dateu         $DTR
058100020212     C*
058200020418     C* CARICO '£1'
058300020212     C                   CLEAR                   KPJBU
058400020212     C                   CLEAR                   DSUL06
058500020212     C                   MOVE      '£1'          D06COD
058600020212     C                   MOVEL     SIMFEL        D06KEY
058700050802     C                   MOVE      'S'           D06KEY
058800050802     C                   MOVEL     ds1ada        D06drf
058900020212     C                   MOVEL     DSUL06        KPJBU
059000020212     C                   CALL      'TRUL06R'
059100020212     C                   PARM                    KPJBA
059200020212     C                   MOVEL     KPJBU         DSUL06
059300020418     c                   movea     t06           sav_t06
059400020418
059500020418      * se in arrivo carico la £6
059600020418 b1  c                   if        ds1tbo = 'A'
059700020418     c                   clear                   wc
059800020418 b2  c                   do        30            wa
059900020422     c                   if        sav_t06(wa) = *zeros
060000020422     c                   leave
060100020422     c                   endif
060200020418     c                   clear                   kpjbu
060300020418     c                   clear                   dsul06
060400020418     c                   eval      d06cod = '£6'
060500020418     c                   movel     sav_t06(wa)   d06key
060600050802     C                   MOVE      'S'           D06KEY
060700020418     c                   eval      d06esc = 'S'
060800050802     c                   movel     ds1ada        d06drf
060900020419     c                   movel     dsul06        kpjbu
061000020418     c                   call      'TRUL06R'
061100020418     c                   parm                    kpjba
061200020418     c                   movel     kpjbu         dsul06
061300020418 b3  c                   do        30            wb
061400020418     c                   if        t06(wb) = *zeros
061500020418     c                   leave
061600020418     c                   endif
061700020418     c                   add       1             wc
061800020418     c                   z-add     t06(wb)       l6(wc)
061900020418 e3  c                   enddo
062000020418 e2  c                   enddo
062100020212     C*
062200020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
062300020212     C                   Z-ADD     *ZERO         WB
062400020418 b2  C                   DO        29            WA
062500020418     C     sav_T06(WA)   LOOKUP    L6                                     30
062600020212     C  N30              ADD       1             WB
062700020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
062800020418 e2  C                   ENDDO
062900020418 x1  C                   ELSE
063000020418     C                   MOVEA     sav_T06       L1
063100020418 e1  C                   ENDIF
063200020212     C*
063300020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
063400020212     C                   MOVEA     L1            LIN
063500020212     C                   CLEAR                   L1
063600020212     C                   Z-ADD     *ZERO         WB                3 0
063700020212     C     1             DO        30            WPT               3 0
063800020212     C     LIN(WPT)      IFNE      0
063900020212     C                   CLEAR                   FNLSC4
064000020212     C                   Z-ADD     LIN(WPT)      IC4LIN
064100020212     C                   MOVE      'B'           IC4FLG
064200020212     C                   MOVEL     FNLSC4        KPJBU
064300020212     C                   CALL      'FNLSC4R1'
064400020212     C                   PARM                    KPJBA
064500050802     C                   PARM                    ds1ada
064600020212     C                   MOVEL     KPJBU         FNLSC4
064700020212     C     OC4ESE        IFEQ      'S'
064800020212     C                   ADD       1             WB
064900020212     C                   MOVE      LIN(WPT)      L1(WB)
065000020212     C                   ENDIF
065100020212     C                   ENDIF
065200020212     C                   ENDDO
065300070103     C*
065400070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
065500070103     C                   Z-ADD     *ZERO         WB                3 0
065600070103     C                   DO        29            WA                3 0
065700070103     C     L1(WA)        IFNE      *ZEROS
065800070103     C     L1(WA)        ANDNE     SIMFEL
065900070103     C                   ADD       1             WB
066000070103     C                   Z-ADD     L1(WA)        LV2(WB)
066100070103     C                   ENDIF
066200070103     C                   ENDDO
066300020212     C*
066400020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
066500020212     C*
066600020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
066700020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
066800020212     C* la stampa e' lanciata da menu
066900020212     C                   MOVEL     '3L'          KCOD
067000020212     C                   MOVEL     DS1PGM        KKEY
067100020212     C                   MOVE      DS1FPG        KKEY
067200020212     C     KTAB1         CHAIN     TABEL00F                           30
067300020212     C  N30              MOVEL     TBLUNI        DS3L
067400020212     C   30              MOVEL     'F'           §3LD1L
067500020212     C   30              MOVEL     ' '           §3LD2L
067600020212     C   30              MOVEL     '*STD    '    §3LDST
067700020212     C*
067800020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
067900020212     C                   MOVEL     §3LDST        £MOD             10
068000020212     C                   CLEAR                   LIN
068100020212     C*
068200020212     C* Verifico chi ha lanciato la stampa
068300020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
068400020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
068500020212     C* DS1FGS<>0  --->  si proviene da menu
068600020212     C     DS1FGS        IFEQ      0
068700020212     C*
068800020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
068900020212     C* lancio, voglio la stampa;  invece
069000020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
069100020212     C*
069200020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
069300020212     C* creare stampa per 1°lvl. (se stessa)
069400020212     C     DS1LNP        IFEQ      SIMFEL
069500020212     C*
069600020212     C* Il flag §3LD1L ha i seguenti significati:
069700020212     C*    ' ' = tutti i dati
069800020212     C*    '1' = solo filiale di 1' livello
069900020212     C     §3LD1L        IFEQ      '1'
070000020212     C                   SETON                                        11
070100020212     C                   END
070200020212     C*
070300020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
070400020212     C* nella stampa
070500020212     C*
070600020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
070700020212     C  N11              MOVEA     L1            LIN
070800020212     C   11              MOVEA     SIMFEL        LIN(1)
070900020212     C*
071000020212     C* IMPOSTO LA STAMPANTE:
071100020212     C* STAMPANTE TABULATI
071200020212     C                   MOVEL     '5P'          KCOD
071300020212     C                   MOVEL     'T'           W004A             4
071400020212     C                   MOVE      LIN(1)        W004A
071500020212     C                   MOVEL(P)  W004A         KKEY
071600020212     C     KTAB1         CHAIN     TABEL00F                           99
071700020212     C     *IN99         IFEQ      *OFF
071800020212     C                   MOVEL     TBLUNI        DS5P
071900020212     C                   MOVEL     §5POTQ        £OUTQ            10
072000020212     C                   END
072100020212     C*
072200020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
072300020212     C                   EXSR      LEGGI
072400020212     C*
072500020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
072600020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
072700020212     C                   ELSE
072800020212     C*
072900020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
073000020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
073100020212     C*
073200020212     C* Il significato del fla §3LD2L e' il seguente:
073300020212     C*    ' ' = tutti i dati
073400020212     C*    'F' = per ogni 2' livello
073500020212     C     §3LD2L        IFEQ      'F'
073600020212     C                   SETON                                        14
073700020212     C                   ENDIF
073800020212     C*
073900020212     C                   Z-ADD     1             WB
074000020212     C                   MOVEA     *ZEROS        LIN
074100020212     C  N14              MOVEA     L1            LIN
074200020212     C*
074300020212     C* Lancio per tutti i 2° lvl
074400020212     C     DS1LNP        IFEQ      0
074500020212     C*
074600020212     C* LV2 contiene tutte le filiali di 2° lvl
074700020212     C     LV2(WB)       DOWNE     0
074800020212      *
074900020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
075000020212     C                   CLEAR                   FNLSC4
075100020212     C                   Z-ADD     LV2(WB)       IC4LIN
075200020212     C                   MOVEL     FNLSC4        KPJBU
075300020212     C                   CALL      'FNLSC4R1'
075400020212     C                   PARM                    KPJBA
075500050802     C                   PARM                    ds1ada
075600020212     C                   MOVEL     KPJBU         FNLSC4
075700020212      *
075800020212     C     OC4PT1        IFEQ      'S'
075900020212     C     §3LPT         ANDEQ     'N'
076000020212     C                   GOTO      NOLEGG
076100020212     C                   ENDIF
076200020212      *
076300020212     C     OC4EXP        IFEQ      'S'
076400020212     C     §3LEXP        ANDEQ     'N'
076500020212     C                   GOTO      NOLEGG
076600020212     C                   ENDIF
076700020212      *
076800020212     C     OC4DPD        IFEQ      'S'
076900020212     C     §3LDPD        ANDEQ     'N'
077000020212     C                   GOTO      NOLEGG
077100020212     C                   ENDIF
077200020212      *
077300020212     C     OC4BAR        IFEQ      'S'
077400020212     C     §3LBAR        ANDEQ     'N'
077500020212     C                   GOTO      NOLEGG
077600020212     C                   ENDIF
077700020212      *
077800020212     C     OC4FED        IFEQ      'S'
077900020212     C     §3LFED        ANDEQ     'N'
078000020212     C                   GOTO      NOLEGG
078100020212     C                   ENDIF
078200020212      *
078300020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
078400020212     C*  dati degli altri 2° lvl
078500020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
078600020212     C   14              MOVEL     LV2(WB)       LIN(1)
078700020212     C*
078800020212     C* IMPOSTO LA STAMPANTE
078900020212     C* STAMPANTE TABULATI
079000020212     C                   MOVEL     '5P'          KCOD
079100020212     C                   MOVEL     'T'           W004A             4
079200020212     C                   MOVE      LV2(WB)       W004A
079300020212     C                   MOVEL(P)  W004A         KKEY
079400020212     C     KTAB1         CHAIN     TABEL00F                           99
079500020212     C     *IN99         IFEQ      *OFF
079600020212     C                   MOVEL     TBLUNI        DS5P
079700020212     C                   MOVEL     §5POTQ        £OUTQ            10
079800020212     C                   END
079900020212     C*
080000020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
080100020212     C                   EXSR      LEGGI
080200020212     C*
080300020212     C     NOLEGG        TAG
080400020212     C*
080500020212     C                   ADD       1             WB
080600020212     C                   ENDDO
080700020212     C*
080800020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
080900020212     C                   ELSE
081000020212     C*
081100020212     C* Lancio solo per la 2° lvl richiesta
081200020212     C   14              MOVEL     DS1LNP        LIN(1)
081300020212     C*
081400020212     C* STAMPANTE
081500020212     C* STAMPANTE TABULATI
081600020212     C                   MOVEL     '5P'          KCOD
081700020212     C                   MOVEL     'T'           W004A             4
081800020212     C                   MOVE      DS1LNP        W004A
081900020212     C                   MOVEL(P)  W004A         KKEY
082000020212     C     KTAB1         CHAIN     TABEL00F                           99
082100020212     C     *IN99         IFEQ      *OFF
082200020212     C                   MOVEL     TBLUNI        DS5P
082300020212     C                   MOVEL     §5POTQ        £OUTQ            10
082400020212     C                   END
082500020212     C*
082600020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
082700020212     C                   EXSR      LEGGI
082800020212     C*
082900020212     C                   ENDIF
083000020212     C                   ENDIF
083100020212     C*
083200020212     C                   ELSE
083300020212     C*
083400020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
083500020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
083600020212     C     DS1LNP        IFEQ      0
083700020212     C                   MOVEA     L1            LIN
083800020212     C* ...altrimenti elaboro solo se stessa
083900020212     C                   ELSE
084000020212     C                   MOVEL     DS1LNP        LIN(1)
084100020212     C                   ENDIF
084200020212     C*
084300020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
084400020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
084500020212     C* PRENDO QUELLA PRESENTE IN KPJBA
084600020212     C     DS1FGS        IFEQ      SIMFEL
084700020212     C                   MOVEL     KQOTD         £OUTQ
084800030627     C**!!!              MOVEL     KQOTD         £OUTQF
084900020212     C                   ELSE
085000020212     C* STAMPANTE TABULATI
085100020212     C                   MOVEL     '5P'          KCOD
085200020212     C                   MOVEL     'T'           W004A             4
085300020212     C                   MOVE      DS1FGS        W004A
085400020212     C                   MOVEL(P)  W004A         KKEY
085500020212     C     KTAB1         CHAIN     TABEL00F                           99
085600020212     C     *IN99         IFEQ      *OFF
085700020212     C                   MOVEL     TBLUNI        DS5P
085800020212     C                   MOVEL     §5POTQ        £OUTQ            10
085900020212     C                   END
086000020212     C                   ENDIF
086100020212     C*
086200020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
086300020212     C                   EXSR      LEGGI
086400020212     C                   ENDIF
086500020212     C*
086600020212     C                   ENDSR
086700000000      **************************************************************************
086800000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
086900000000      **************************************************************************
087000000000     C     LEGGI         BEGSR
087100940502     C                   SETOFF                                       2729
087200000619     C                   SETON                                        43
087300000616     C                   CLEAR                   SAVRIS
087400970212     C*
087500970212      * Cancello dal file statistiche i record della giornata che vado a elabor.
087600970212      * Solo se richiesta elaborazione
087700970212    1C     DS1RIS        IFNE      'R'
087800000616     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
087900000616     C                   CLEAR                   KPJBU
088000000616     C                   CLEAR                   DSUL06
088100000616     C                   MOVE      '£1'          D06COD
088200000616     C                   MOVEL     SIMFEL        D06KEY
088300050802     C                   MOVE      'S'           D06KEY
088400050802     c                   movel     ds1ada        d06drf
088500000616     C                   MOVEL     DSUL06        KPJBU
088600000616     C                   CALL      'TRUL06R'
088700000616     C                   PARM                    KPJBA
088800000616     C                   MOVEL     KPJBU         DSUL06
088900000616     C**
089000000616     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
089100000616     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
089200000616     C*  CHE USO ALLA FINE
089300000616     C     DS1RIS        IFNE      ' '
089400000616     C                   MOVEA     LIN           SVLIN
089500000616     C                   MOVEL     DS1RIS        SAVRIS
089600000616     C                   CLEAR                   DS1RIS
089700000616     C                   ENDIF
089800000616     C*
089900000616     C                   MOVEA     T06           LIN
090000000616     C*
090100970212     C                   OPEN      FNSAT04L
090200970212     C                   Z-ADD     1             XX                3 0
090300970212    2C     LIN(XX)       DOWGT     *ZERO
090400970212     C*
090500970212     C     FNSATK        SETLL     FNSAT04L
090600970212     C     FNSATK        READE     FNSAT04L                               99
090700970212     C*
090800970212    3C     *IN99         DOWEQ     *OFF
090900970212     C                   DELETE    FNSAT004
091000970212     C     FNSATK        READE     FNSAT04L                               99
091100970212    3C                   ENDDO
091200970212     C                   ADD       1             XX
091300970212    2C                   ENDDO
091400970212     C*
091500970212     C                   CLOSE     FNSAT04L
091600970212     C                   END
091700940505      *
091800940505      * Come prima cosa devo verificare se giornata festiva
091900940505      *   solo per ELABORAZIONE o  per stampe giornaliere
092000950210     C     DS1FGS        IFEQ      0
092100950210     C     DS1RIS        ORNE      'R'
092200940505     C                   CLEAR                   $TFA
092300940505      *
092400950118     C     AZCLNK        CHAIN     AZCLN01L                           99
092500940505     C     *IN99         IFEQ      *OFF
092600950118     C                   MOVEA     CLNMAT        GME
092700970211     C     GME($GI)      IFEQ      'F'
092800970211     C* SE FESTIVO SCRIVO RECORD IN FNSAT CON TRE = F
092900970211     C                   CLEAR                   FNSAT000
093000970211     C                   Z-ADD     SIMFEL        SATFLE
093100970211     C                   MOVEL     'F'           SATTRE
093200970211     C                   Z-ADD     DS1ADA        SATDRE
093300040922     c                   movel     $ftr          satftr
093400040922     c                   z-add     $DTR          satdtr
093500970311     C                   WRITE     FNSAT000                             99
093600970211     C                   GOTO      ENDLEG
093700970211     C                   ENDIF
093800940505     C                   ENDIF
093900940505     C                   ENDIF
094000940429      *
094100940429     C* LANCIO DA FILTRO
094200950210    1C     DS1FGS        IFGT      0
094300940429     C* 29 ON - IMMESSA LINEA DI PARTENZA
094400950210    2C     DS1LNP        IFGT      0
094500940429     C                   SETON                                        29
094600950210    3C     DS1LNP        IFNE      DS1FEL
094700940502     C                   SETON                                        27
094800940429    3C                   ENDIF
094900940429    2C                   ENDIF
095000940429     C*
095100940429   X1C                   ELSE
095200940502     C                   SETON                                        05
095300940506     C                   CLEAR                   STRIP
095400940429     C* STAMPE GIORNALIERE
095500940429    2C     LIN(2)        IFEQ      0
095600940429     C                   SETON                                        29
095700940429    2C                   ENDIF
095800940429     C     SIMFEL        LOOKUP    LIN                                    99
095900940502     C  N99              SETON                                        27
096000940429    1C                   ENDIF
096100000912     C* SE SONO TUTTI P.O. POSTE, NON ENTRO NEMMENO NEL CICLO DI LETT
096200000912     C*   E RISTAMPA
096300000912     C                   Z-ADD     1             $X
096400000912     C                   CLEAR                   WSTAF
096500000912    1C     LIN($X)       DOWGT     0
096600000912     C     WSTAF         ANDEQ     ' '
096700000912     C                   Z-ADD     LIN($X)       $FILPR
096800000912     C* ESCLUDO I P.O. POSTE
096900020813     ***C                   CLEAR                   §OGPT
097000020813     C                   clear                   OG143
097100000912     C     $FILPR        CHAIN     AZORG01L                           82
097200000912     C  N82              MOVEL     ORGDE3        OG143
097300000912     C**
097400020813    2***C     §OGPT         IFNE      'S'
097500020813    2C                   IF        §OGntw <> 'PPT'
097600000912     C* SE ELABORATA ALMENO UNA LINEA, STAMPO ANCHE I TOTALI FINALI
097700000912     C                   MOVEL     'S'           WSTAF             1
097800000912    2C                   ENDIF
097900000912     C                   ADD       1             $X
098000000912    1C                   ENDDO
098100000000      *
098200000000      *
098300000912    1C     WSTAF         IFEQ      'S'
098400000000      * Scrivo, in stampa, se selezionata filiale o area
098500000000     C                   MOVE      *BLANKS       STRIP1
098600950210     C  N05DS1LNP        IFNE      *ZERO
098700960925     C                   MOVEL     'P.O.'        STRIP1
098800950210     C                   MOVE      DS1LNP        STRIP1
098900000000     C                   ELSE
099000940502     C                   MOVEL     'AREA'        STRIP1
099100950210     C                   MOVE      DS1FEL        STRIP1
099200000000     C                   ENDIF
099300000000      *
099400000000      * Inizializzo i campi per l'elaborazione
099500000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
099600000000     C                   Z-ADD     *ZERO         $FILPR
099700000000     C                   MOVE      *BLANKS       $CBLPR
099800000000     C                   Z-ADD     *ZERO         $NFVPR
099900940509     C                   CLEAR                   $RIGHE
100000950210      *
100100950210      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
100200950210     C                   MOVEA     £MOD          CMD(33)
100300950210     C                   MOVEA     £OUTQ         CMD(51)
100400950210      *
100500940502      * Eseguo OVRPRTF sul file di stampa
100600950210     C                   Z-ADD     82            LUNG             15 5
100700950213     C                   MOVEL     *BLANKS       COMMAN           82
100800940502     C                   MOVEA     CMD           COMMAN
100900940502     C                   CALL      'QCMDEXC'                            H1
101000940502     C                   PARM                    COMMAN
101100940502     C                   PARM                    LUNG
101200970807     C  N38              OPEN      FNLSC2P
101300950118     C  N38              WRITE     LSC2T1
101400950118     C  N38              WRITE     LSC2T3
101500940502     C  N38              SETOFF                                       30
101600000619     C**
101700000619     C* SE DS1RIS= 'S' ACCENDO PER ORA IL 38
101800000619     C     SAVRIS        IFNE      ' '
101900000619     C                   SETON                                        38
102000000619     C                   ENDIF
102100000000      *
102200950210      * L'elaborazione solo se non scelta la ristampa (DS1RIS='R')
102300950210    1C     DS1RIS        IFNE      'R'
102400000000      *
102500970205      * Leggo il file delle bolle in partenza prima quelli con data
102600970205      * F.V. = 0 poi quelli con DFV >= della data elaborazione
102700940401     C                   Z-ADD     1             $X                3 0
102800940428     C                   MOVE      *BLANK        $FLAG
102900940428     C*
103000940428    2C     LIN($X)       DOWGT     *ZERO
103100000912     C                   Z-ADD     LIN($X)       $FILPR
103200000912      * Escludo bolle LNP Poste
103300020813     ***C                   CLEAR                   §OGPT
103400020813     C                   clear                   OG143
103500000912     C     $FILPR        CHAIN     AZORG01L                           82
103600000912     C  N82              MOVEL     ORGDE3        OG143
103700000912     C**
103800020813   2A***C     §OGPT         IFNE      'S'
103900020813    2C                   IF        §OGntw <> 'PPT'
104000940428     C                   CLEAR                   $DFV
104100940428     C                   SETOFF                                           01
104200940428      *
104300940428    3C                   DO        2
104400021023     C     FNBLTK        SETLL     FNBLT38L
104500021023     C  N01FNBLTK        READE     FNBLT38L                               33
104600021023     C   01LIN($X)       READE     FNBLT38L                               33
104700940428     C*
104800940428    4C     *IN33         DOWEQ     *OFF
104900000000      *
105000000000      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
105100000000      * come test ma mi permette di saltare delle chain)
105200940428    5C     $BLTAT        IFNE      $BLTPR
105300950118     C     FNBLPK        CHAIN     FNBLP01L                           99
105400000000     C   99              GOTO      LETTUR
105500000000     C                   MOVE      $BLTAT        $BLTPR
105600000000     C                   Z-ADD     BLPNFV        $NFVPR
105700950118     C                   Z-ADD     BLPMGS        $DATSP            8 0
105800000000     C                   MOVEL     BLPAAS        $DATSP
105900000331     C                   Z-ADD     BLPDRT        $DTRIT
106000000413     C                   Z-ADD     BLPPDR        $CDPDR
106100940428     C* $BOLLA = 1 INDICA CHE HO LETTO UNA NUOVA BOLLA
106200940428     C                   MOVEL     '1'           $BOLLA            1
106300000000      *
106400000000      * Decodifico il tipo bolla
106500950118    6C     BLPCBO        IFNE      $CBLPR
106600950210     C                   MOVE      '3A'          KCOD
106700950210     C                   MOVE      *BLANKS       KKEY
106800950210     C                   MOVEL     BLPCBO        KKEY
106900950210     C     KTAB1         CHAIN     TABEL00F                           99
107000000000     C  N99              MOVEL     TBLUNI        DS3A
107100950118     C  N99              MOVE      BLPCBO        $CBLPR
107200000000     C   99              CLEAR                   DS3A
107300940428    6C                   ENDIF
107400940427     C*
107500940428    5C                   ENDIF
107600000000      *
107700940428      * ESCLUSIONI BOLLE
107800000000     C                   EXSR      ESCLUS
107900940428    5C     $OK           IFEQ      'S'
108000000000     C                   EXSR      ELABOR
108100940428    5C                   ENDIF
108200000000      *
108300940428     C     LETTUR        TAG
108400021023     C  N01FNBLTK        READE     FNBLT38L                               33
108500021023     C   01LIN($X)       READE     FNBLT38L                               33
108600940428    4C                   ENDDO
108700940428      *
108800940428     C                   SETON                                        01
108900970205     C                   Z-ADD     DS1AAL        $DFV
109000940428    3C                   ENDDO
109100940428      *
109200940428      * cambia la filiale --> decodifico e stampo i totali
109300940428     C                   EXSR      CALSPE
109400940428     C                   EXSR      WRISAT
109500940429     C                   EXSR      WR1SAT
109600940428     C  N38              EXSR      TOTFIL
109700940428     C                   EXSR      AZZFIL
109800000912   2AC                   ENDIF
109900940428     C*
110000940401     C                   ADD       1             $X
110100940428    2C                   ENDDO
110200940429      *
110300940429     C*
110400940429     C* STAMPA DEI DATI FINALI
110500940506     C                   EXSR      STAFIN
110600000616   X C                   ENDIF
110700000616      *
110800000616     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
110900000616     C     SAVRIS        IFEQ      'S'
111000000616     C                   MOVEL     'R'           DS1RIS
111100000616     C                   MOVEA     SVLIN         LIN
111200000616     C* AZZERO LE SCHIERE DI TOTALE
111300000616     C                   EXSR      AZZSER
111400000616     C                   EXSR      AZZTRA
111500000616     C                   EXSR      AZZTOT
111600000619     C                   SETOFF                                       38
111700000616     C                   ENDIF
111800000000      *
111900970211      * Nel caso di RISTAMPA mi limito a rileggere il file statistiche (FNSAT)
112000000000      * e stampare i record che rientrano nell'intervallo dal-al
112100000616     C     DS1RIS        IFNE      ' '
112200000000     C                   EXSR      RISTAM
112300940428    1C                   ENDIF
112400000000      *
112500000000      * Stampa terminata
112600940428     C     *IN38         IFEQ      *OFF
112700940429     C*
112800940428     C     $ALMEN        IFEQ      *ZERO
112900950118     C                   WRITE     LSC2E1
113000940505     C                   ELSE
113100940429     C*
113200000000     C                   EXSR      STATER
113300940505     C                   ENDIF
113400000000      *
113500950118     C                   WRITE     LSC2E2
113600950629     C                   WRITE     LSC2E5
113700000000      *
113800000000      * Verifico se vi sono fogli viaggio aperti
113900000000     C                   EXSR      FOGVIA
114000000000      *
114100970807     C                   CLOSE     FNLSC2P
114200940428     C                   ENDIF
114300000000      *
114400000000      * Aggiorno la data ultima elaborazione
114500950210     C     DS1RIS        IFNE      'R'
114600000616     C     SAVRIS        OREQ      'S'
114700021016
114800021016     c                   Eval      kTbeCod = 'SDF'
114900021016     c                   Movel(p)  Simfel        kTbeKe1
115000021016     c     Ktbe          Chain     Tntbe01l
115100021016     c                   If        %Found(Tntbe01l)
115200021016     c                   Movel     TbeUni        dSdf
115300021016     c                   If        §SdfTerP = *Zeros or §SdfTerP > Ds1Ada
115400021016     c                   Z-Add     Ds1Ada        §SdfTerP
115500021016     c                   EndIf
115600021016     c                   If        §SdfTerU < Ds1Ada
115700021016     c                   Z-Add     Ds1Ada        §SdfTerU
115800021016     c                   EndIf
115900021016     c                   Movel     dSdf          TbeUni
116000021016     c                   Update    Tntbe000
116100021016     c                   EndIf
116200000000     C                   ENDIF
116300090415     c
116400090415     c* Se non ho aggioranto la tabella, vedo se devo cancellare record
116500090415     c*  in FISTA
116600090415     c                   if        savris='S'
116700090415     c     ksta          delete    fista01l
116800090415     C                   ENDIF
116900000912     C*
117000000912    1C                   ENDIF
117100000000      *
117200940505     C     ENDLEG        ENDSR
117300000000      **************************************************************************
117400000000      * Una volta letto il record verifico se da includere o no
117500940427      * PER QUALSIASI TIPO DI CALCOLO  (COLLI E BOLLE A TERRA E
117600940427      *  COLLI E BOLLA DA PARTIRE)
117700000000      *
117800940427      * Vi sono solo due motivi per escludere il record letto:
117900940427      * (1) se la spedizione e' stata bollettata dopo la data elaboraz
118000950210      *                Data spedizione > DS1AAL
118100000000      *
118200940427      * (2) se la data partenza del collo e' < della data elaborazione
118300950210      *                BLTDFV < DS1AAL
118400000000      *
118500000810      * ATTENZIONE.. DA OGGI C'E' UN MOTIVO IN PIU'.. Canale5 ! da cantare ..
118600000810      *
118700000810      * (3) se la linea di partenze è ??..... POSTE !
118800000810      *
118900940427      * Bolle di recupero / bolle legate / bolle "£" sono escluse
119000000000      **************************************************************************
119100000000     C     ESCLUS        BEGSR
119200000000      *
119300000000     C                   MOVE      'N'           $OK               1
119400000331     C                   MOVE      *BLANKS       WSOS              1
119500000810      *
119600000810      *
119700940427      * esclusioni:
119800950323      * Le bolle "legate" sono identificabili dal nr.foglio viaggio
119900950323      * = 99999 O = 99998
120000070522    1C     $NFVPR        IFEQ      99999
120100950323     C     $NFVPR        OREQ      99998
120200940427     C     $CBLPR        OREQ      '£'
120300940428     C     §3ARBL        OREQ      'R'
120400000000     C                   GOTO      ENDESC
120500070522    1C                   ENDIF
120600000413     C* Prepagati non confermati
120700070522    1C     $CDPDR        IFEQ      *ZEROS
120800000413     C                   GOTO      ENDESC
120900070522    1C                   ENDIF
121000000000      *
121100070522    1C     BLTDFV        IFEQ      DS1AAL
121200950629     C                   MOVEL     'S'           $OK
121300070522   x1C                   ELSE
121400070522     C
121500070522     c* Elaborazione in base A     Data BORDERÒ
121600070522   1AC                   IF        DS1AAL<§VPOnpi
121700070522     C
121800940427      * La data di spedizione deve essere <= alla data elaborazione
121900070522    2C     $DATSP        IFLE      DS1AAL
122000000000      *
122100940427      * La data partenza deve essere > = alaa data elaborazione        limiti
122200070522    3C     BLTDFV        IFEQ      *ZERO
122300950210     C     BLTDFV        ORGE      DS1AAL
122400000000     C                   MOVE      'S'           $OK
122500000000     C                   GOTO      ENDESC
122600070522    3C                   ENDIF
122700070522   X2C                   ELSE
122800000331     C* Se data spedizione > di data elaborazione
122900000411     C* e  data ritiro    <= data elaborazione    ==> BOLLA IN SOSPENS.
123000070522    3C     $DTRIT        IFLE      DS1AAL
123100000331     C                   MOVE      'S'           $OK
123200000331     C                   MOVEL     'S'           WSOS
123300070522     C                   GOTO      ENDESC
123400070522    3C                   ENDIF
123500950626     C*
123600070522    2C                   ENDIF
123700070522     c
123800070522  x1ac                   else
123900070522     c****
124000070522     c* ELAborazione in base a    data RITIRO Merce
124100070522      * La data ritiro deve essere <= alla data elaborazione
124200070522    2C     $DTRIT        IFLE      DS1AAL
124300070522      *
124400070525      * La data partenza deve essere > = alla data elaborazione        limiti
124500070522    3C     BLTDFV        IFEQ      *ZERO
124600070522     C     BLTDFV        ORGE      DS1AAL
124700070522     C                   MOVE      'S'           $OK
124800070522     C                   GOTO      ENDESC
124900070522    3C                   ENDIF
125000070522    2C                   ENDIF
125100070522   1aC                   ENDIF
125200950629     C*
125300070522    1C                   ENDIF
125400000000      *
125500000000     C     ENDESC        ENDSR
125600000000      **************************************************************************
125700000000      * Elaborazioni dei record letti, somme nei totali
125800000000      **************************************************************************
125900000000     C     ELABOR        BEGSR
126000000000      *
126100970210      * Verifico il tipo spedizione ESPR./DIST./CORR.
126200950201     C                   Z-ADD     3             $T                1 0
126300000000     C     BLPTSP        IFEQ      'E'
126400000000     C                   Z-ADD     1             $T
126500000000     C                   ENDIF
126600950201     C     BLPTSP        IFEQ      'D'
126700000619     C     BLPTSP        OREQ      'P'
126800940323     C                   Z-ADD     2             $T
126900000000     C                   ENDIF
127000000000      *
127100000000      * C/servizio oppure no ?
127200940428     C     §3ARBL        COMP      'C'                                3434
127300000000      *
127400970205     C*
127500070522    1C     WSOS          IFEQ      *BLANKS
127600970205     C* DETERMINAZIONE SE COLLO A TERRA
127700000000      * Ho due controlli da fare per sapere se il collo e' a terra:
127800000000      *    1)  BLTDFV = 0
127900950210      *    2)  BLTDFV > Data spedizione   AND   BLTDFV > DS1AAL
128000000000     C                   SETOFF                                       36
128100070522    2C     BLTDFV        IFEQ      *ZERO
128200950210     C     BLTDFV        ORGT      DS1AAL
128300000000     C                   SETON                                        36
128400070522    2C                   ENDIF
128500970303     C*
128600970303     C* RICHIAMO PGM PER CALCOLO AFFIDABILITA'
128700970303     C* (SOLO SE HO LETTO UNA NUOVA BOLLA - $BOLLA = '1')
128800070522    2C     $BOLLA        IFEQ      '1'
128900070522    3C     BLPDCE        IFEQ      *ZEROS
129000970303     C     BLPDUC        ORNE      DS1AAL
129100970303     C                   MOVEL     ' '           D98TLA
129200970303     C                   MOVEL     'P'           D98TBO
129300970303     C                   MOVEL     BLPAAS        D98AAS
129400970303     C                   MOVEL     BLPLNP        D98LNP
129500970303     C                   MOVEL     BLPNRS        D98NRS
129600970303     C                   MOVEL     BLPNSP        D98NSP
129700970310     C                   Z-ADD     DS1AAL        D98DRF
129800970310     C                   Z-ADD     2359          D98ORF
129900970925     C                   CALL      'TNSD99R'
130000970925     C                   PARM                    DS99
130100970303     C                   ELSE
130200970303     C                   Z-ADD     BLPNPI        D98NPI
130300070522    3C                   END
130400070522    2C                   END
130500970207     C*
130600970207     C*VERIFICO SE SI TRATTA DI "A TERRA DA PARTIRE"
130700970210     C* La considero "non da partire" se si tratta di:
130800970207     C* - spedizione non borderizzata
130900970303     C* - distribuzione a terra del giorno
131000070522     c* - dal 1° giugno 2007 il calcolo della affidabilità a terra sarà
131100070522     c*   in base alla data ritiro, per cui nel DA PARTIRE non controllo
131200070522     c*   più la data borderò ma solo se si tratta della distribuzione
131300070522     c*   del giorno
131400970205     C                   SETOFF                                       35
131500070522     c
131600070522    2C     *IN36         IFEQ      *ON
131700070522     c
131800070522    3c                   if        ds1aal<§vponpi
131900970210     C*
132000070522    4C     BLPDBR        IFGT      DS1AAL
132100070522     C     BLPDBR        OREQ      *ZEROS
132200070522     C                   SETON                                        35
132300070522   x4C                   ELSE
132400970304     C*
132500070522    5C     BLPDBR        IFLE      DS1AAL
132600070522     C     BLPDPC        ANDEQ     0
132700070522     C     D98NPI        ANDLT     0
132800070522     C*
132900070522     C     BLPDBR        ORLE      DS1AAL
133000070522     C     BLPDPC        ANDGT     DS1AAL
133100070522     C     D98NPI        ANDLT     0
133200070522     C                   SETON                                        35
133300070522    5C                   END
133400070522    4C                   END
133500070522     c
133600070522   x3c                   else
133700070522     c*
133800090108     C* SE affidabilità =0 verifico se cade in giorno festivo, per
133900090108     c*    ter-ter --> se si non do ritardo
134000090108     c                   if        d98npi=0
134100090108     C                   movel     blptfa        $TFA
134200090108      *
134300090108     C     AZCLNK        CHAIN     AZCLN01L                           99
134400090108     C                   MOVEA     CLNMAT        GME
134500090108     C                   MOVEA     CLNpom        GMEp
134600090108     C     GME($GI)      IFEQ      'F'
134700090108     C     GMEp($GI)     oreq      'F'
134800090108     c                   sub       24            d98npi
134900090108     c                   endif
135000090108     c                   endif
135100090108     c
135200070522    4c                   if        (blpdpc=0 and d98npi<0) or
135300070522     c                             (blpdpc>ds1aal and d98npi<0)
135400070522     C                   SETON                                        35
135500070522    4c                   endif
135600070522    3c                   endif
135700970210     C*
135800070522    2C                   END
135900070522    1C                   ENDIF
136000970205     C*
136100000000      *
136200970205      * Aggancio, nel file statistiche, il record per il terminal di
136300970205      * arrivo poiche' lo dovrò aggiornare
136400970205     C                   CLEAR                   FNSAT000
136500000000     C                   Z-ADD     SIMFEL        SATFLE
136600950210     C                   Z-ADD     DS1AAL        SATDRE
136700940428     C                   Z-ADD     BLPLNP        SATLNP
136800000000     C                   Z-ADD     BLPTFA        SATTFA
136900000000     C                   MOVE      *BLANK        SATTRE
137000970205     C     FNSA1K        CHAIN     FNSAT02L                           37
137100940428      *
137200970205      * NUOVA BOLLA LETTA: CONTO LE SPEDIZIONI
137300940428      *
137400940428     C     $BOLLA        IFEQ      '1'
137500940428     C                   EXSR      DATBOL
137600970205     C                   ENDIF
137700021217      *
137800021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
137900030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
138000030213      * Peso medio
138100030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
138200030213if  1c***                if        Wsipes = 'S'
138300030213     c***                z-add     BLTpuc        $Peso
138400030213   x1c***                else
138500021220     c                   z-add     Wpesmed       $Peso
138600030213e   1c***                endif
138700021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
138800030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
138900030213      * Volume medio
139000030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
139100030213if  1c***                if        Wsivol = 'S'
139200030213     c***                z-add     BLTvuc        $Volum
139300030213   x1c***                else
139400021220     c                   z-add     Wvolmed       $Volum
139500030213e   1c***                endif
139600000000      *
139700000000      * ?  CALCOLI  SUI  COLLI  ?
139800000000      *
139900000331     C     WSOS          IFEQ      *BLANKS
140000000000      * Nr.colli da partire (servira' per la percentuale)
140100970207     C     *IN35         IFEQ      *OFF
140200970210     C   34              ADD       1             CPA($T)
140300970210     C  N34              ADD       1             C4A($T)
140400970210     C                   ADD       1             CDT($T)
140500950626     C                   END
140600000000      *
140700940427     C     *IN36         IFEQ      *ON
140800970207      * A terra da partire                                             (35ON)
140900940427     C   34
141000940427     CANN35              ADD       1             CBP($T)
141100940427     C  N34
141200940427     CANN35              ADD       1             C4P($T)
141300970207     C  N35              ADD       1             C0P($T)
141400970207     C  N35              ADD       1             C3P($T)
141500970207      * A terra non da partire                                         (35ON)
141600940427     C   34
141700940427     CAN 35              ADD       1             CBR($T)
141800940427     C  N34
141900940427     CAN 35              ADD       1             C4R($T)
142000940427     C   35              ADD       1             C0R($T)
142100940427     C                   ENDIF
142200000000      *
142300000000      * ?  CALCOLI  SUL  PESO  ?
142400000000      *
142500970207     C     *IN35         IFEQ      *OFF
142600970210     C   34              ADD       $PESO         PPA($T)
142700970210     C  N34              ADD       $PESO         P4A($T)
142800970210     C                   ADD       $PESO         PDT($T)
142900950626     C                   END
143000940502      * A TERRA
143100940427     C     *IN36         IFEQ      *ON
143200940502      *
143300970206      * Fino a 35 quintali
143400021217     ***C     BLPPKF        IFLE      3500
143500021217     C     Wpeso         IFLE      3500
143600970207     C* Da partire
143700970206     C   34
143800970206     CANN35              ADD       $PESO         PFI($T)
143900970206     C  N34
144000970206     CANN35              ADD       $PESO         P4I($T)
144100970206     C  N35              ADD       $PESO         P3I($T)
144200970206     C  N35              ADD       $PESO         P0I($T)
144300970207     C* Non da partire
144400970206     C   34
144500970206     CAN 35              ADD       $PESO         PFN($T)
144600970206     C  N34
144700970206     CAN 35              ADD       $PESO         P4N($T)
144800970206     C   35              ADD       $PESO         P0N($T)
144900970206     C                   ELSE
145000970206      * Oltre 35 quintali:                                             (35ON)
145100970207     C* Da partire
145200940427     C   34
145300940427     CANN35              ADD       $PESO         PBP($T)
145400940427     C  N34
145500940427     CANN35              ADD       $PESO         P4P($T)
145600970206     C  N35              ADD       $PESO         P0P($T)
145700970206     C  N35              ADD       $PESO         P3P($T)
145800970207     C* Non da partire
145900940427     C   34
146000940427     CAN 35              ADD       $PESO         PBR($T)
146100940427     C  N34
146200940427     CAN 35              ADD       $PESO         P4R($T)
146300940427     C   35              ADD       $PESO         P0R($T)
146400940427     C                   ENDIF
146500970206     C                   ENDIF
146600000331     C*
146700000331     C                   ENDIF
146800000000      *
146900000000      * ?  CALCOLI  SUL  VOLUME  ?
147000940329      *
147100000000      *
147200021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
147300021217     ***C     BLTVUC        IFEQ      *ZERO
147400021217     *** *
147500021217     ***C     BLPVLF        DIV(H)    BLPNCL        $VOLUM           12 4
147600021217     *** *
147700021217     ***C                   ELSE
147800021217     ***C                   Z-ADD     BLTVUC        $VOLUM
147900021217     ***C                   ENDIF
148000000000      *
148100000331     C     WSOS          IFEQ      *BLANKS
148200970207     C     *IN35         IFEQ      *OFF
148300970210     C   34              ADD       $VOLUM        VPA($T)
148400970210     C  N34              ADD       $VOLUM        V4A($T)
148500970210     C                   ADD       $VOLUM        VDT($T)
148600950626     C                   END
148700940505      *
148800940428     C     *IN36         IFEQ      *ON
148900970207     C* Da partire
149000940428     C   34
149100940428     CANN35              ADD       $VOLUM        VBP($T)
149200940428     C  N34
149300940428     CANN35              ADD       $VOLUM        V4P($T)
149400970206     C  N35              ADD       $VOLUM        V0P($T)
149500970206     C  N35              ADD       $VOLUM        V3P($T)
149600000331     c* Sia che sia da partire che non da partire: sommo nel totale
149700000331     C* Mc rimasti a magazzino
149800000331     C   34              ADD       $VOLUM        VBR($T)
149900000331     C  N34              ADD       $VOLUM        V4R($T)
150000000331     C                   ADD       $VOLUM        V0R($T)
150100000406     C                   ADD       $VOLUM        V3R($T)
150200000000      *
150300000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
150400000000      *
150500970205      * Visto che le spedizioni non riesco a calcolarle ora, mi limito
150600970205      * a tenere aggiornato il file di work con le bolle
150700940428     C   34              MOVE      *BLANK        WRKCSE
150800940428     C  N34              MOVE      'C'           WRKCSE
150900940503     C                   Z-ADD     BLPLNP        WRKLNP
151000950118     C     FNWR1K        CHAIN     FNWRK00F                           99
151100940428     C     *IN99         IFEQ      *OFF
151200000000     C                   ADD       1             WRKNC1
151300000000     C                   EXCEPT    AGGIO
151400000000     C                   ELSE
151500950118     C                   MOVEL     *BLANKS       WTRANS            1
151600940503     C                   EXSR      WRIWRK
151700000000     C                   ENDIF
151800940428     C                   ENDIF
151900000331     c* Se collo di bolla in sospensione sommo volume in totale mc
152000000331     c* rimasti a magazzino
152100000331     C                   ELSE
152200000331     C   34              ADD       $VOLUM        VBR($T)
152300000331     C  N34              ADD       $VOLUM        V4R($T)
152400000331     C                   ADD       $VOLUM        V0R($T)
152500000406     C                   ADD       $VOLUM        V3R($T)
152600000412     C                   ADD       $VOLUM        VOLUME            9 4
152700000331     C                   ENDIF
152800940323      *
152900040921     C                   MOVE      $FTR          SATFTR
153000040921     C                   MOVE      $DTR          SATDTR
153100940428     C     *IN37         IFEQ      *ON
153200040921     C****               MOVE      $FTR          SATFTR
153300040921     C****               MOVE      $DTR          SATDTR
153400970205     C                   WRITE     FNSAT000
153500940428     C                   ELSE
153600970205     C                   UPDATE    FNSAT000
153700940428     C                   ENDIF
153800000000      *
153900000000     C                   ENDSR
154000940428      **************************************************************************
154100940428      * Carico i dati della bolla (peso e volume)
154200940428      *  e conto le spedizioni partite il giorno di elaborazione
154300940428      **************************************************************************
154400940428     C     DATBOL        BEGSR
154500940428      *
154600021217     *** * Calcolo il peso di un singolo collo
154700021217     ***C     BLPPKF        DIV(H)    BLPNCL        $PESO             8 1
154800021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
154900021217     *** * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
155000021217     *** * BLPVLC=Volume rilevato)
155100050112     ***C     BLPNCR        ifeq      BLPNCL
155200021217     ***C                   Z-ADD     BLPVLC        BLPVLF
155300021217     ***C                   ELSE
155400021217     ***C     BLPVLC        IFGE      BLPVLF
155500021217     ***C                   Z-ADD     BLPVLC        BLPVLF
155600021217     ***C                   ENDIF
155700021217     ***C                   ENDIF
155800021217      *
155900021217      * Verifico i dati misurati con la macchina smistacolli
156000021217      * (BLPNCR=Num. colli rilevati x volume, BLPVLC=Volume rilevato)
156100021220      * e calcolo il VOLUME unitario medio per collo da usare se non
156200021220      * tutti i colli sono stati volumati dalla smistacolli
156300021217      *
156400030213      **** Verifico se sono stati volumati tutti i colli
156500030213     c***                clear                   Wsivol
156600021217     c                   clear                   Wvolu
156700050112     c                   if        BLPncr = BLPncl
156800021217     c                   z-add     BLPvlc        Wvolu
156900030213     c***                eval      Wsivol =  'S'
157000021217   x1c                   else
157100021217if  2c                   if        BLPvlc >= BLPvlf
157200021217     c                   z-add     BLPvlc        Wvolu
157300021217   x2c                   else
157400021217     c                   z-add     BLPvlf        Wvolu
157500021217e   2c                   endif
157600021217e   1c                   endif
157700021220      *
157800021220      * Calcolo il volume medio
157900021220     c     Wvolu         div(H)    BLPncl        Wvolmed
158000021217      *
158100021217      * Verifico i dati misurati con la macchina smistacolli
158200021217      * (BLPNCP=Num. colli rilevati x peso, BLPPKC=Peso rilevato)
158300021220      * e calcolo il PESO   unitario medio per collo da usare se non
158400021220      * tutti i colli sono stati pesati   dalla smistacolli
158500021217      *
158600030213      **** Verifico se sono stati pesati   tutti i colli
158700030213     c***                clear                   Wsipes
158800021217     c                   clear                   Wpeso
158900050112     c                   If        BLPncp = BLPncl
159000021217     c                   z-add     BLPpkc        Wpeso
159100030213     c***                eval      Wsipes =  'S'
159200021217   c1c                   else
159300021217if  2c                   if        BLPpkc >= BLPpkf
159400021217     c                   z-add     BLPpkc        Wpeso
159500021217   x2c                   else
159600021217     c                   z-add     BLPpkf        Wpeso
159700021217e   2c                   endif
159800021217e   1c                   endif
159900021220      *
160000021220      * Calcolo il peso medio
160100021220     c     Wpeso         div(H)    BLPncl        Wpesmed
160200940428     C*
160300940428     C* SOMMO SOLTANTO LE SPEDIZIONI PARTITE NEL GIORNO
160400970205     C*  LA DATA DI RIFERIMENTO E' LA DATA ULTIMO COLLO
160500940506     C     $FLAG         IFEQ      ' '
160600970205     C     BLPDUC        ANDEQ     DS1AAL
160700000331     C     WSOS          ANDEQ     *BLANKS
160800950118     C                   MOVEL     *BLANKS       WTRANS
160900940503     C                   EXSR      CTRBOL
161000970205     C                   ENDIF
161100940428     C*
161200940428     C                   MOVEL     '2'           $BOLLA
161300940428     C                   ENDSR
161400950118      **************************************************************************
161500021217      * Carico i dati della bolla (peso e volume) (TRANSITI)
161600021217     *** *  e conto le spedizioni partite il giorno di elaborazione
161700021217     *** *    (questo NON lo faceva NEANCHE prima)
161800950118      **************************************************************************
161900950118     C     DATBOT        BEGSR
162000950118      *
162100021217     *** * Calcolo il peso di un singolo collo
162200021217     ***C     BTPPKB        DIV(H)    BTPNCL        $PESO             8 1
162300021217     *** * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
162400021217     *** * misurati con la macchina smistacolli (BTPNCR=Numero colli rilevati,
162500021217     *** * BTPVLC=Volume rilevato)
162600050112     ***C     BTPNCR        ifeq      BTPNCL
162700021217     ***C                   Z-ADD     BTPVLC        BTPVLB
162800021217     ***C                   ELSE
162900021217     ***C     BTPVLC        IFGE      BTPVLB
163000021217     ***C                   Z-ADD     BTPVLC        BTPVLB
163100021217     ***C                   ENDIF
163200021217     ***C                   ENDIF
163300021217     ***C*
163400021220      *
163500021220      * Verifico i dati misurati con la macchina smistacolli
163600021220      * (BTPNCR=Num. colli rilevati x volume, BTPVLC=Volume rilevato)
163700021220      * e calcolo il VOLUME unitario medio per collo da usare se non
163800021220      * tutti i colli sono stati volumati dalla smistacolli
163900021217      *
164000030213      **** Verifico se sono stati volumati tutti i colli
164100030213     c***                clear                   Wsivol
164200021217     c                   clear                   Wvolu
164300050112     c                   if        BTPncr = BTPncl
164400021217     c                   z-add     BTPvlc        Wvolu
164500030213     c***                eval      Wsivol =  'S'
164600021217   x1c                   else
164700021217if  2c                   if        BTPvlc >= BTPvlb
164800021217     c                   z-add     BTPvlc        Wvolu
164900021217   x2c                   else
165000021217     c                   z-add     BTPvlb        Wvolu
165100021217e   2c                   endif
165200021217e   1c                   endif
165300021220      *
165400021220      * Calcolo il volume medio
165500021220     c     Wvolu         div(H)    BTPncl        Wvolmed
165600021220      *
165700021220      * Verifico i dati misurati con la macchina smistacolli
165800021220      * (BTPNCP=Num. colli rilevati x peso, BTPPKC=Peso rilevato)
165900021220      * e calcolo il PESO   unitario medio per collo da usare se non
166000021220      * tutti i colli sono stati pesati   dalla smistacolli
166100021217      *
166200030213      **** Verifico se sono stati pesati   tutti i colli
166300030213     c***                clear                   Wsipes
166400021217     c                   clear                   Wpeso
166500050112     c                   if        BTPncp = BTPncl
166600021217     c                   z-add     BTPpkc        Wpeso
166700030213     c***                eval      Wsipes =  'S'
166800021217   x1c                   else
166900021217if  2c                   if        BTPpkc >= BTPpkb
167000021217     c                   z-add     BTPpkc        Wpeso
167100021217   x2c                   else
167200021217     c                   z-add     BTPpkb        Wpeso
167300021217e   2c                   endif
167400021217e   1c                   endif
167500021220      *
167600021220      * Calcolo il peso medio
167700071231     c     Wpeso         div(H)    BTPncl        Wpesmed
167800021217      *
167900950118     C                   MOVEL     '2'           $BOLLA
168000950118     C                   ENDSR
168100940503      **************************************************************************
168200950118      * Scrivo il file FNWRK00F
168300940503      **************************************************************************
168400940503     C     WRIWRK        BEGSR
168500950118     C*
168600940503     C                   MOVE      $FLAG         WRKFLG
168700950118     C                   Z-ADD     1             WRKNC1
168800950118     C*
168900950118     C     WTRANS        IFNE      'T'
169000940503     C                   Z-ADD     BLPAAS        WRKAAS
169100940503     C                   Z-ADD     BLPMGS        WRKMGS
169200940503     C                   Z-ADD     BLPNRS        WRKNRS
169300940503     C                   Z-ADD     BLPNSP        WRKNSP
169400940503     C                   Z-ADD     BLPLNA        WRKLNA
169500940503     C                   MOVE      BLPTSP        WRKTSP
169600940503     C                   Z-ADD     BLPNCL        WRKNCL
169700940503     C                   Z-ADD     BLPTFA        WRKTFA
169800070525     c* Se dopo la data di attivazione, memorizzo la data borderò
169900070525    3c                   if        ds1aal<§vponpi
170000970207     C                   Z-ADD     BLPDBR        WRKDBR
170100070525     c                   else
170200070525     C                   Z-ADD     BLPDrt        WRKDBR
170300070525     c                   endif
170400070525     c
170500970303     C                   Z-ADD     BLPDPC        WRKDPC
170600970303     C                   Z-ADD     BLPDUC        WRKDUC
170700970303     C                   Z-ADD     D98NPI        WRKNPI
170800950118     C*
170900950118     C                   ELSE
171000950118     C*
171100950118     C                   Z-ADD     BTPAAS        WRKAAS
171200950118     C                   Z-ADD     BTPMGS        WRKMGS
171300950118     C                   Z-ADD     BTPNRS        WRKNRS
171400950118     C                   Z-ADD     BTPNSP        WRKNSP
171500950118     C                   Z-ADD     BTPLNA        WRKLNA
171600950118     C                   MOVE      BTPTSP        WRKTSP
171700950118     C                   Z-ADD     BTPNCL        WRKNCL
171800950118     C                   Z-ADD     BTPTFA        WRKTFA
171900970207     C                   CLEAR                   WRKDBR
172000970303     C                   CLEAR                   WRKDUC
172100970303     C                   CLEAR                   WRKDPC
172200970303     C                   CLEAR                   WRKNPI
172300950118     C                   ENDIF
172400950118     C*
172500950118     C                   WRITE     FNWRK000
172600950118     C*
172700940503     C                   ENDSR
172800940503      **************************************************************************
172900940503      * Calcolo le spedizioni da partire:
173000950118      *  vedo se gia' contata prima di ricontarla(se esiste in FNWRK)
173100970207      *  Potrei averla già contata se i colli non sono sequenziali
173200940503      **************************************************************************
173300940503     C     CTRBOL        BEGSR
173400940503     C*
173500940505     C                   MOVEL     $FLAG         $FLAG2
173600940503     C                   MOVEL     'P'           $FLAG
173700940503     C                   MOVEL     BLPLNP        WRKLNP
173800940503     C                   CLEAR                   WRKCSE
173900950118     C     FNWR1K        SETLL     FNWRK00F                               99
174000940503     C* BOLLA NON TROVATA  LA SCRIVO
174100940503    1C     *IN99         IFEQ      *OFF
174200940503     C                   EXSR      WRIWRK
174300940503     C*
174400970211     C                   ADD       1             SDT($T)
174500970210     C   34              ADD       1             SPA($T)
174600940503     C* S4A --> PER C/S SPEDIZIONI DELL'AREA
174700940503     C*         PER I TRANSITI
174800970210     C  N34              ADD       1             S4A($T)
174900940503    1C                   ENDIF
175000940505     C                   MOVEL     $FLAG2        $FLAG
175100940503     C                   ENDSR
175200000000      **************************************************************************
175300000000      * Calcolo sulle spedizioni, lettura del file di work creato prima
175400000000      **************************************************************************
175500000000     C     CALSPE        BEGSR
175600940502     C                   SETOFF                                       3435
175700940428      * Verifico se e' transito
175800940502     C     $FLAG         COMP      'T'                                    35
175900000000      *
176000000000      * Leggo tutti le spedizioni della filiale da stampare dal file di work
176100970207    1C     $FLAG         IFEQ      *BLANK
176200950118     C     FNWRKK        SETLL     FNWRK00F
176300970207   X1C                   ELSE
176400950118     C     $FLAG         SETLL     FNWRK00F
176500970207    1C                   ENDIF
176600000000      *
176700970207    1C                   DO        *HIVAL
176800000000      *
176900970207    2C     $FLAG         IFEQ      *BLANK
177000950118     C     FNWRKK        READE(N)  FNWRK00F                               99
177100970207   X2C                   ELSE
177200950118     C     $FLAG         READE(N)  FNWRK00F                               99
177300970207    2C                   ENDIF
177400000000      *
177500970207    2C  N99              DO
177600000000      *
177700970210      * Verifico il tipo spedizione ESPR./DIST./CORR.
177800950201     C                   Z-ADD     3             $V                1 0
177900970207    3C     WRKTSP        IFEQ      'E'
178000000000     C                   Z-ADD     1             $V
178100970207    3C                   ENDIF
178200970207    3C     WRKTSP        IFEQ      'D'
178300940323     C                   Z-ADD     2             $V
178400970207    3C                   ENDIF
178500940428      *
178600940428      * Verifico se e' c/servizio
178700940502     C  N35WRKCSE        COMP      *BLANKS                                34
178800000000      *
178900000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
179000000000      * poiche' lo dovro aggiornare
179100970207     C                   CLEAR                   FNSAT000
179200000000     C                   Z-ADD     SIMFEL        SATFLE
179300950210     C                   Z-ADD     DS1ADA        SATDRE
179400940506     C  N35              Z-ADD     WRKLNP        SATLNP
179500940506     C   35              Z-ADD     SIMFEL        SATLNP
179600000000     C                   Z-ADD     WRKTFA        SATTFA
179700000000     C  N35              MOVE      *BLANK        SATTRE
179800000000     C   35              MOVE      'T'           SATTRE
179900970205     C     FNSA1K        CHAIN     FNSAT02L                           37
180000000000      *
180100950628     C                   MOVE      WRKMGS        $DATS1            8 0
180200950628     C                   MOVEL     WRKAAS        $DATS1
180300950628     C*
180400970207     C* A TERRA COMPLETAMENTE
180500970207      * La spedizione e' completa se il nr.colli spediti (WRKNC1) e' = uguale
180600050112      * al nr.colli della bolla (WRKNCL)
180700970207    3C     WRKNC1        IFEQ      WRKNCL
180800940506     C                   ADD       1             S0M($V)
180900970207     C* Transiti
181000970207    4C     *IN35         IFEQ      *ON
181100940506     C                   ADD       1             S2M($V)
181200970207   X4C                   ELSE
181300970207     C* Dell'area
181400940506     C   34              ADD       1             SCM($V)
181500940506     C  N34              ADD       1             S4M($V)
181600940506     C                   ADD       1             S3M($V)
181700970207     C*
181800970207    4C                   ENDIF
181900970207    3C                   ENDIF
182000970210     C* COMDAP = BLANKS => A TERRA DA PARTIRE
182100970207     C                   CLEAR                   COMDAP
182200970303     C* BORDERIZZATO, DISTRIBUZIONE A TERRA DEL GIORNO NON SPAREGGIATA
182300070525    3c                   if        ds1aal<§vponpi
182400070525     c
182500970207    3C     WRKDBR        IFGT      *ZEROS
182600970207     C     WRKDBR        ANDLE     DS1AAL
182700970303     C*
182800970303    4C     WRKDUC        IFEQ      *ZEROS
182900970303     C     WRKDPC        ANDEQ     0
183000970303     C     WRKNPI        ANDLT     0
183100970303     C*
183200970303     C     WRKDUC        OREQ      *ZEROS
183300970303     C     WRKDPC        ANDGT     DS1ADA
183400970303     C     WRKNPI        ANDLT     0
183500970303     C*
183600970303     C     WRKDUC        ORGT      DS1ADA
183700970303     C     WRKDPC        ANDGT     DS1ADA
183800970303     C     WRKNPI        ANDLT     0
183900970207     C                   MOVE      'N'           COMDAP            1
184000970207    4C                   END
184100970207   X3C                   ELSE
184200970207     C* NON BORDERIZZATO
184300970207     C                   MOVE      'N'           COMDAP
184400970207    3C                   END
184500070525     c*
184600070525     c                   else
184700070525     c
184800070525     c* calcolo per data ritiro a terra non da partire solo la distribu
184900070525     c*  zione del giorno
185000070525    4c                   if        (wrkdpc=0 and wrknpi<0) or
185100070525     c                             (wrkdpc>ds1aal and wrknpi<0)
185200070525     C                   MOVE      'N'           COMDAP
185300070525    4c                   endif
185400070525    3c                   endif
185500070525     c
185600970207     C*
185700970207    3C     COMDAP        IFEQ      *BLANKS
185800970207     C* Per i transiti memorizzo in un unico totale le spediz. a terra
185900970207     C* e non mi interessa nemmeno memorizzare il totale da partire
186000970207     C     $FLAG         OREQ      'T'
186100970207     C***
186200970207     C* TOTALE DA PARTIRE
186300970207     C***
186400970207     C* Sommo le spediz.a terra border. esclusa la distrib. del periodo
186500970210     C  N35              ADD       1             SDT($V)
186600970210     C  N35
186700970210     CAN 34              ADD       1             SPA($V)
186800970207     C* S4A --> Per c/s spedizioni dell'area
186900970210     C  N35
187000970210     CANN34              ADD       1             S4A($V)
187100970207     C***
187200970207     C* A TERRA DA PARTIRE
187300970207     C***
187400970207     C   34
187500970207     CANN35              ADD       1             SBP($V)
187600970207     C  N34
187700970207     CANN35              ADD       1             S4P($V)
187800970207     C  N35              ADD       1             S3P($V)
187900970207     C   35              ADD       1             S2P($V)
188000970207     C                   ADD       1             S0P($V)
188100970305     C***
188200970305     C* A TERRA DA PARTIRE FUORI AFFIDABILITA'
188300970305     C***
188400970305    4C     WRKNPI        IFGE      0
188500970305     C   34
188600970305     CANN35              ADD       1             FAF($V)
188700970305     C  N34
188800970305     CANN35              ADD       1             F4F($V)
188900970305     C  N35              ADD       1             F3F($V)
189000970305     C  N35              ADD       1             F0F($V)
189100970305    4C                   END
189200970207     c*
189300970207   X3C                   ELSE
189400970207     C***
189500970207     C* A TERRA NON DA PARTIRE
189600970207     C***
189700970207     C   34              ADD       1             SBR($V)
189800970207     C  N34              ADD       1             S4R($V)
189900970207     C                   ADD       1             S3R($V)
190000970207     C                   ADD       1             S0R($V)
190100970207    3C                   ENDIF
190200000000      *
190300040922     c  n37              move      $FTR          satftr
190400040922     c  n37              move      $DTR          satDTR
190500970207     C  N37              UPDATE    FNSAT000
190600000000      *
190700970207    2C                   ENDDO
190800970207    1C  N99              ENDDO
190900000000      *
191000000000     C                   ENDSR
191100000000      **************************************************************************
191200000000      * Calcolo i totali di filiale, li stampo e scrivo sul file
191300000000      **************************************************************************
191400000000     C     TOTFIL        BEGSR
191500000000      *
191600000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
191700000000      *
191800970211     C                   SETON                                        404145
191900000000     C                   Z-ADD     $FILPR        FILIAP
192000000000      *
192100000000      * Decodifico il codice filiale
192200000000     C     $FILPR        CHAIN     AZORG01L                           99
192300000000     C  N99ORGFVA        COMP      *BLANK                             9999
192400940503     C   99              CLEAR                   ORGDEP
192500000000     C  N99              MOVEL     ORGDES        ORGDEP
192600000000      *
192700000000      * ?  C  O   L   L  I  ?
192800970210     C                   XFOOT     CBP           COTOTP
192900970210     C                   XFOOT     CPA           $CDA
193000940428     C                   EXSR      PERCOL
193100000000      *
193200000000      * ?  P  E  S  O  ?
193300970210     C                   XFOOT     PFI           PEF35P
193400970210     C                   XFOOT     PBP           PEO35P
193500970210     C     PEF35P        ADD       PEO35P        PETOTP
193600970210     C                   XFOOT     PPA           $PDA
193700940428     C                   EXSR      PERPES
193800000000      *
193900000000      * ?  V  O  L  U  M E  ?
194000970210     C                   XFOOT     VBP           VOTOTP
194100970210     C                   XFOOT     VPA           $VDA
194200940428     C                   EXSR      PERVOL
194300000000      *
194400000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
194500970210     C                   XFOOT     SPA           $SDA
194600970305     C                   XFOOT     FAF           SPFAFF
194700940502     C                   EXSR      PERSPE
194800970305     C                   XFOOT     SBP           SPTOTP
194900970305     C                   XFOOT     SCM           SPCONP
195000970305     C                   XFOOT     SBR           $COMOD
195100970211     C     SPTOTP        ADD       $COMOD        SPTOTA
195200970211     C     SPTOTA        SUB       SPCONP        SPSPAP
195300000331      *
195400000331      * ? VOLUME A MAGAZZINO?
195500000331     C                   XFOOT     VBR           VOTOTM
195600000000      *
195700970210     C* STAMPO SOLO SE HO DEI DATI
195800000331     C     VOTOTM        IFGT      *ZEROS
195900000417     C* HO RIMESSO ANCHE TEST SU SPTOTA PER FAR FUNZIONARE LE RISTAMPE
196000000417     C* RELATIVE A DATE ELABORATE PRIMA CHE VENISSE FATTO IL CALCOLO
196100000417     C* DEL TOTALE VOLUME A MAGAZZINO
196200000417     C     SPTOTA        ORGT      *ZEROS
196300970210     C*
196400000000     C                   Z-ADD     $FILPR        FILIAP
196500000000     C                   EXSR      SCRIVI
196600940509     C                   ADD       1             $RIGHE            2 0
196700000000      *
196800970210      * ------------------ ESPR./DIST./CORR. --------------------------
196900000000      *
197000970210     C                   SETOFF                                       40
197100000000     C                   DO        3             $V
197200940429     C                   CLEAR                   DESCRP
197300940429      *
197400970210     C                   MOVE      ENS($V)       TIPOP
197500000000      *
197600000000      * ?  C  O   L   L  I  ?
197700970211     C                   Z-ADD     CBP($V)       COTOTP
197800970211     C                   Z-ADD     CPA($V)       $CDA
197900940428     C                   EXSR      PERCOL
198000000000      *
198100000000      * ?  P  E  S  O  ?
198200000000     C                   Z-ADD     PFI($V)       PEF35P
198300970211     C                   Z-ADD     PBP($V)       PEO35P
198400970212     C     PEF35P        ADD       PEO35P        PETOTP
198500970211     C                   Z-ADD     PPA($V)       $PDA
198600940428     C                   EXSR      PERPES
198700000000      *
198800000000      * ?  V  O  L  U  M E  ?
198900970211     C                   Z-ADD     VBP($V)       VOTOTP
199000970211     C                   Z-ADD     VPA($V)       $VDA
199100940428     C                   EXSR      PERVOL
199200000000      *
199300000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
199400970305     C                   Z-ADD     SPA($V)       $SDA
199500970305     C                   Z-ADD     FAF($V)       SPFAFF
199600940502     C                   EXSR      PERSPE
199700970305     C                   Z-ADD     SBP($V)       SPTOTP
199800970305     C                   Z-ADD     SCM($V)       SPCONP
199900970211     C     SBP($V)       ADD       SBR($V)       SPTOTA
200000970211     C     SPTOTA        SUB       SCM($V)       SPSPAP
200100000331      *
200200000331      * ?tot.volume a magazz?
200300000331     C                   Z-ADD     VBR($V)       VOTOTM
200400000000      *
200500000000     C                   EXSR      SCRIVI
200600000000      *
200700000000     C                   ENDDO
200800000000      *
200900950118     C                   WRITE     LSC2T2
201000940506     C                   ENDIF
201100940328      *
201200000417     C                   ENDSR
201300940428      **************************************************************************
201400940428      * Calcolo percentuale sui colli
201500940428      **************************************************************************
201600940428     C     PERCOL        BEGSR
201700940502     C                   Z-ADD     *ZERO         COSDPP
201800940428     C*
201900940502     C* PERCENTUALE SUL DA PARTIRE
202000970213     C  N02$CDA          IFNE      *ZERO
202100970210     C     COTOTP        DIV       $CDA          $COMOD
202200940502     C     $COMOD        MULT(H)   100           COSDPP
202300940502     C                   ENDIF
202400940428     C                   ENDSR
202500940428      **************************************************************************
202600940428      * Calcolo percentuale sul peso
202700940428      **************************************************************************
202800940428     C     PERPES        BEGSR
202900940502     C                   CLEAR                   PESDPP
203000940428     C*
203100940502     C* PERCENTUALE SUL DA PARTIRE
203200970213     C  N02$PDA          IFNE      *ZERO
203300970210     C     PETOTP        DIV       $PDA          $COMOD
203400940502     C     $COMOD        MULT(H)   100           PESDPP
203500940502     C                   ENDIF
203600940428     C                   ENDSR
203700940428      **************************************************************************
203800940428      * Calcolo percentuale sul volume
203900940428      **************************************************************************
204000940428     C     PERVOL        BEGSR
204100940502     C                   Z-ADD     *ZERO         VOSDPP
204200940502     C*
204300940502     C* PERCENTUALE SUL DA PARTIRE
204400970213     C  N02$VDA          IFNE      *ZERO
204500970210     C     VOTOTP        DIV       $VDA          $COMOD
204600940502     C     $COMOD        MULT(H)   100           VOSDPP
204700940502     C                   ENDIF
204800940428     C                   ENDSR
204900940428      **************************************************************************
205000940428      * Calcolo percentuale sulle spedizioni
205100940428      **************************************************************************
205200940428     C     PERSPE        BEGSR
205300940502     C                   Z-ADD     *ZERO         SPSDPP
205400940428     C*
205500940502     C* PERCENTUALE SUL DA PARTIRE
205600970213     C  N02$SDA          IFNE      *ZERO
205700970305     C     SPFAFF        DIV       $SDA          $COMOD
205800940502     C     $COMOD        MULT(H)   100           SPSDPP
205900940502     C                   ENDIF
206000940428     C                   ENDSR
206100000000      **************************************************************************
206200000000      * Azzeramento dei totali di filiale per nuovi conti
206300000000      **************************************************************************
206400000000     C     AZZFIL        BEGSR
206500000412     C                   CLEAR                   VOLUME
206600000000      *
206700000000      * COLLI
206800970211     C                   ADD       CPA(1)        C3A(1)
206900970211     C                   ADD       CPA(2)        C3A(2)
207000970211     C                   ADD       CPA(3)        C3A(3)
207100970211     C                   CLEAR                   CPA
207200970211     C                   CLEAR                   CBP
207300970211     C                   CLEAR                   CBR
207400970211      * PESI
207500970211     C                   ADD       PPA(1)        P3A(1)
207600970211     C                   ADD       PPA(2)        P3A(2)
207700970211     C                   ADD       PPA(3)        P3A(3)
207800970211     C                   CLEAR                   PFI
207900970211     C                   CLEAR                   PPA
208000970211     C                   CLEAR                   PBP
208100970211     C                   CLEAR                   PBR
208200970211     C                   CLEAR                   PFN
208300000000      * VOLUMI
208400970211     C                   ADD       VPA(1)        V3A(1)
208500970211     C                   ADD       VPA(2)        V3A(2)
208600970211     C                   ADD       VPA(3)        V3A(3)
208700970211     C                   CLEAR                   VPA
208800970211     C                   CLEAR                   VBP
208900970211     C                   CLEAR                   VBR
209000970211      * SPEDIZIONI
209100970211     C                   ADD       SPA(1)        S3A(1)
209200970211     C                   ADD       SPA(2)        S3A(2)
209300970211     C                   ADD       SPA(3)        S3A(3)
209400970211     C                   CLEAR                   SPA
209500970211     C                   CLEAR                   SBP
209600970211     C                   CLEAR                   SBR
209700970211     C                   CLEAR                   SCM
209800970305     C                   CLEAR                   FAF
209900000000      *
210000000000     C                   ENDSR
210100000000      **************************************************************************
210200000000      * Azzeramento dei totali di c/servizio per nuova area
210300000000      **************************************************************************
210400000000     C     AZZSER        BEGSR
210500000000      *
210600000000      * COLLI
210700940506     C                   ADD       C1A(1)        C3A(1)
210800970211     C                   ADD       C1A(2)        C3A(2)
210900970211     C                   ADD       C1A(3)        C3A(3)
211000000000     C                   CLEAR                   C1A
211100000000     C                   CLEAR                   C1P
211200000000     C                   CLEAR                   C1R
211300000000      * PESI
211400940506     C                   ADD       P1A(1)        P3A(1)
211500970211     C                   ADD       P1A(2)        P3A(2)
211600970211     C                   ADD       P1A(3)        P3A(3)
211700970211     C                   CLEAR                   P1I
211800970211     C                   CLEAR                   P1N
211900970211     C                   CLEAR                   P1P
212000970211     C                   CLEAR                   P1R
212100970211     C                   CLEAR                   P1A
212200970211      * VOLUMI
212300970211     C                   ADD       V1A(1)        V3A(1)
212400970211     C                   ADD       V1A(2)        V3A(2)
212500970211     C                   ADD       V1A(3)        V3A(3)
212600970211     C                   CLEAR                   V1A
212700970211     C                   CLEAR                   V1P
212800970211     C                   CLEAR                   V1R
212900000000      * SPEDIZIONI
213000940506     C                   ADD       S1A(1)        S3A(1)
213100970211     C                   ADD       S1A(2)        S3A(2)
213200970211     C                   ADD       S1A(3)        S3A(3)
213300970211     C                   CLEAR                   S1A
213400970211     C                   CLEAR                   S1P
213500970211     C                   CLEAR                   S1R
213600970211     C                   CLEAR                   S1M
213700970305     C                   CLEAR                   F1F
213800000000      *
213900000000     C                   ENDSR
214000000000      **************************************************************************
214100000000      * Azzeramento dei totali di c/servizio per nuova filiale
214200000000      **************************************************************************
214300000000     C     AZZSE4        BEGSR
214400000000      *
214500000000      * COLLI
214600970210     C                   CLEAR                   C4A
214700970210     C                   CLEAR                   C4P
214800970210     C                   CLEAR                   C4R
214900000000      * PESI
215000970210     C                   CLEAR                   P4A
215100970210     C                   CLEAR                   P4I
215200970210     C                   CLEAR                   P4P
215300970210     C                   CLEAR                   P4R
215400970210     C                   CLEAR                   P4N
215500000000      * VOLUMI
215600970210     C                   CLEAR                   V4A
215700970210     C                   CLEAR                   V4P
215800970210     C                   CLEAR                   V4R
215900000000      * SPEDIZIONI
216000970210     C                   CLEAR                   S4A
216100970210     C                   CLEAR                   S4P
216200970210     C                   CLEAR                   S4R
216300970210     C                   CLEAR                   S4M
216400970305     C                   CLEAR                   F4F
216500000000      *
216600000000     C                   ENDSR
216700000000      **************************************************************************
216800000000      * Azzeramento dei totali di transito per nuovi totali
216900000000      **************************************************************************
217000000000     C     AZZTRA        BEGSR
217100940506     C* SOMMO NEI TOTALI
217200940506     C                   EXSR      ADDTRA
217300000000      * COLLI
217400940428     C                   CLEAR                   C2P
217500000000      * PESI
217600000000     C                   CLEAR                   P2I
217700940428     C                   CLEAR                   P2P
217800000000      * VOLUMI
217900940428     C                   CLEAR                   V2P
218000000000      * SPEDIZIONI
218100940428     C                   CLEAR                   S2P
218200000000     C                   CLEAR                   S2M
218300000000      *
218400000000     C                   ENDSR
218500000000      **************************************************************************
218600000000      * Stampo i totali per c/servizio
218700000000      **************************************************************************
218800000000     C     TOTSER        BEGSR
218900000000      *
219000000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
219100000000      *
219200970211     C                   SETON                                        4045
219300000000     C                   SETOFF                                       41
219400000000     C                   Z-ADD     *ZERO         FILIAP
219500000000     C                   MOVE      *BLANKS       ORGDEP
219600000000     C     'C/SERVIZ'    CAT       'I':0         ORGDEP
219700000000      *
219800970211      * ?  C  O   L   L  I  ?
219900970211     C                   XFOOT     C1P           COTOTP
220000970211     C                   XFOOT     C1A           $CDA
220100970211     C                   EXSR      PERCOL
220200970211      *
220300970211      * ?  P  E  S  O  ?
220400970211     C                   XFOOT     P1I           PEF35P
220500970211     C                   XFOOT     P1P           PEO35P
220600970211     C     PEF35P        ADD       PEO35P        PETOTP
220700970211     C                   XFOOT     P1A           $PDA
220800940429     C                   EXSR      PERPES
220900000000      *
221000000000      * ?  V  O  L  U  M E  ?
221100970211     C                   XFOOT     V1P           VOTOTP
221200970211     C                   XFOOT     V1A           $VDA
221300940429     C                   EXSR      PERVOL
221400000000      *
221500000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
221600970211     C                   XFOOT     S1A           $SDA
221700970305     C                   XFOOT     F1F           SPFAFF
221800970211     C                   EXSR      PERSPE
221900970305     C                   XFOOT     S1P           SPTOTP
222000970305     C                   XFOOT     S1M           SPCONP
222100970211     C                   XFOOT     S1R           $COMO1
222200970211     C     SPTOTP        ADD       $COMO1        SPTOTA
222300970211     C     SPTOTA        SUB       SPCONP        SPSPAP
222400000331      *
222500000331      * ?tot.volume a magazz?
222600000331     C                   XFOOT     V1R           VOTOTM
222700970211      *
222800970211     C* STAMPO SOLO SE HO DEI DATI
222900000331     C     VOTOTM        IFGT      *ZEROS
223000970211     C*
223100970211     C                   EXSR      SCRIVI
223200000000      *
223300970211      * ------------------ ESPR./DIST./CORR. --------------------------
223400000000      *
223500970211     C                   SETOFF                                       40
223600000000     C                   DO        3             $V
223700940323     C                   MOVE      ENS($V)       TIPOP
223800000000      *
223900000000      * ?  C  O   L   L  I  ?
224000970211     C                   Z-ADD     C1P($V)       COTOTP
224100970211     C                   Z-ADD     C1A($V)       $CDA
224200940429     C                   EXSR      PERCOL
224300000000      *
224400000000      * ?  P  E  S  O  ?
224500000000     C                   Z-ADD     P1I($V)       PEF35P
224600970211     C                   Z-ADD     P1P($V)       PEO35P
224700970211     C     PEF35P        ADD       PEO35P        PETOTP
224800970211     C                   Z-ADD     P1A($V)       $PDA
224900940429     C                   EXSR      PERPES
225000000000      *
225100000000      * ?  V  O  L  U  M E  ?
225200970211     C                   Z-ADD     V1P($V)       VOTOTP
225300970211     C                   Z-ADD     V1A($V)       $VDA
225400940429     C                   EXSR      PERVOL
225500000000      *
225600000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
225700970211     C                   Z-ADD     S1A($V)       $SDA
225800970305     C                   Z-ADD     F1F($V)       SPFAFF
225900940502     C                   EXSR      PERSPE
226000970305     C                   Z-ADD     S1P($V)       SPTOTP
226100970305     C                   Z-ADD     S1M($V)       SPCONP
226200970211     C     S1P($V)       ADD       S1R($V)       SPTOTA
226300970211     C     SPTOTA        SUB       S1M($V)       SPSPAP
226400000331      *
226500000331      * ?tot volume a magazz?
226600000331     C                   Z-ADD     V1R($V)       VOTOTM
226700000000      *
226800000000     C                   EXSR      SCRIVI
226900000000      *
227000000000     C                   ENDDO
227100940504     C                   ENDIF
227200940429     C                   EXSR      AZZSER
227300000000      *
227400000000     C                   ENDSR
227500000000      **************************************************************************
227600000000      * Stampo i totali per transito
227700000000      **************************************************************************
227800000000     C     TOTTRA        BEGSR
227900000000      *
228000000000      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
228100000000      *
228200940506     C                   SETON                                        400203
228300970211     C                   SETOFF                                       45
228400000000     C                   MOVE      *BLANKS       ORGDEP
228500940502     C* stampa per linea: totale transiti
228600940502     C     *IN43         IFEQ      *ON
228700940502     C                   Z-ADD     *ZERO         FILIAP
228800940502     C                   MOVEL     'TRANSITI'    ORGDEP
228900940502     C                   SETOFF                                       41
229000940502     C                   ELSE
229100940502     c* stampa per terminal in transito
229200940502     C                   SETON                                        41
229300940502      * Decodifico il terminal di arrivo
229400940502     C                   Z-ADD     $FILPR        FILIAP
229500940502     C     $FILPR        CHAIN     AZORG01L                           99
229600940502     C  N99ORGFVA        COMP      *BLANK                             9999
229700940503     C   99              CLEAR                   ORGDEP
229800940502     C  N99              MOVEL     ORGDES        ORGDEP
229900940502     C                   ENDIF
230000000000      *
230100000000      * ?  C  O   L   L  I  ?
230200940502     C                   XFOOT     C2P           COTOTP
230300970211     C*
230400970211      * SOLO SE HO TROVATO ALEMNO UN COLLO A TERRA STAMPO
230500970211     C     COTOTP        IFGT      0
230600940504      *
230700000000      *
230800000000      * ?  P  E  S  O  ?
230900970211     C                   XFOOT     P2I           PEF35P
231000970211     C                   XFOOT     P2P           PEO35P
231100970211     C     PEO35P        ADD       PEF35P        PETOTP
231200000000      *
231300000000      * ?  V  O  L  U  M E  ?
231400940428     C                   XFOOT     V2P           VOTOTP
231500000000      *
231600000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
231700940428     C                   XFOOT     S2P           SPTOTP
231800970211     C                   XFOOT     S2P           SPTOTA
231900970211     C                   XFOOT     S2M           SPCONP
232000970211     C     SPTOTA        SUB       SPCONP        SPSPAP
232100000000      *
232200000000     C                   EXSR      SCRIVI
232300940509     C                   ADD       1             $RIGHE
232400000000      *
232500970211      * ------------------ ESPR./DIST./CORR. --------------------------
232600000000      *
232700940502     C                   SETOFF                                       4045
232800000000     C                   DO        3             $V
232900940502     C* PER TERMINAL SCRIVO CHE E' RECORD DI TRANSITO
233000940429     C  N43$V            IFEQ      1
233100940506     C                   MOVEL     'IN TRANS'    DESCRP
233200940429     C                   ELSE
233300940429     C                   CLEAR                   DESCRP
233400940429     C                   ENDIF
233500940429     C*
233600940323     C                   MOVE      ENS($V)       TIPOP
233700000000      *
233800000000      * ?  C  O   L   L  I  ?
233900940428     C                   Z-ADD     C2P($V)       COTOTP
234000000000      *
234100000000      * ?  P  E  S  O  ?
234200970211     C                   Z-ADD     P2I($V)       PEF35P
234300970211     C                   Z-ADD     P2P($V)       PEO35P
234400970211     C     PEO35P        ADD       PEF35P        PETOTP
234500000000      *
234600000000      * ?  V  O  L  U  M E  ?
234700940428     C                   Z-ADD     V2P($V)       VOTOTP
234800000000      *
234900000000      * ?  S  P  E  D  I  Z  I  O  N  I  ?
235000940428     C                   Z-ADD     S2P($V)       SPTOTP
235100970211     C                   Z-ADD     S2P($V)       SPTOTA
235200970211     C                   Z-ADD     S2M($V)       SPCONP
235300970211     C     SPTOTA        SUB       S2M($V)       SPSPAP
235400000000      *
235500000000     C                   EXSR      SCRIVI
235600000000      *
235700000000     C                   ENDDO
235800940428     C*
235900940428     C                   EXSR      AZZTRA
236000940502      *
236100950118     C  N38              WRITE     LSC2T2
236200940506     C                   ENDIF
236300940506     C                   SETOFF                                       0302
236400000000      *
236500000000     C                   ENDSR
236600000000      **************************************************************************
236700970211      * Scrittura del record sul file di trasmissione FNSAT00F
236800000000      **************************************************************************
236900000000     C     WRISAT        BEGSR
237000000000      *
237100970207     C                   CLEAR                   FNSAT000
237200000000     C                   Z-ADD     SIMFEL        SATFLE
237300000000     C                   Z-ADD     $FILPR        SATLNP
237400000000     C                   Z-ADD     *ZERO         SATTFA
237500950210     C                   Z-ADD     DS1ADA        SATDRE
237600000000     C                   MOVE      *BLANK        SATTRE
237700000000      *
237800000000      * COLLI: da partire
237900970210     C                   Z-ADD     CPA(1)        SATCPE
238000970210     C                   Z-ADD     CPA(2)        SATCPD
238100970210     C                   Z-ADD     CPA(3)        SATCPC
238200970207      * COLLI: a terra da partire
238300970207     C                   Z-ADD     CBP(1)        SATCDE
238400970207     C                   Z-ADD     CBP(2)        SATCDD
238500970207     C                   Z-ADD     CBP(3)        SATCDC
238600970207      * COLLI: a terra non da partire
238700970207     C                   Z-ADD     CBR(1)        SATCNE
238800970207     C                   Z-ADD     CBR(2)        SATCND
238900970207     C                   Z-ADD     CBR(3)        SATCNC
239000970207      * PESO: da partire
239100970210     C                   Z-ADD     PPA(1)        SATPPE
239200970210     C                   Z-ADD     PPA(2)        SATPPD
239300970210     C                   Z-ADD     PPA(3)        SATPPC
239400970207      * PESO: a terra da partire fino a 35 quintali
239500970207     C                   Z-ADD     PFI(1)        SATFDE
239600970207     C                   Z-ADD     PFI(2)        SATFDD
239700970207     C                   Z-ADD     PFI(3)        SATFDC
239800970207      * PESO: a terra non da partire fino a 35 q.
239900970207     C                   Z-ADD     PFN(1)        SATFNE
240000970207     C                   Z-ADD     PFN(2)        SATFND
240100970207     C                   Z-ADD     PFN(3)        SATFNC
240200970207     C* PESO: a terra da partire oltre 35 quintali
240300970207     C                   Z-ADD     PBP(1)        SATODE
240400970207     C                   Z-ADD     PBP(2)        SATODD
240500970207     C                   Z-ADD     PBP(3)        SATODC
240600970207      * PESO: a terra non da partire oltre 35 q.
240700970207     C                   Z-ADD     PBR(1)        SATONE
240800970207     C                   Z-ADD     PBR(2)        SATOND
240900970207     C                   Z-ADD     PBR(3)        SATONC
241000000000      * VOLUME: da partire
241100970210     C                   Z-ADD     VPA(1)        SATVPE
241200970210     C                   Z-ADD     VPA(2)        SATVPD
241300970210     C                   Z-ADD     VPA(3)        SATVPC
241400970207      * VOLUME a terra da partire
241500970207     C                   Z-ADD     VBP(1)        SATVDE
241600970207     C                   Z-ADD     VBP(2)        SATVDD
241700970207     C                   Z-ADD     VBP(3)        SATVDC
241800000331      * VOLUME rimasto a magazzino
241900000331      * (totale dei colli a terra da partire, da non partire e colli
242000000331      * di bolle in sospensione)
242100970207     C                   Z-ADD     VBR(1)        SATVNE
242200970207     C                   Z-ADD     VBR(2)        SATVND
242300970207     C                   Z-ADD     VBR(3)        SATVNC
242400940429      * SPEDIZIONI: da partire
242500970210     C                   Z-ADD     SPA(1)        SATSPE
242600970210     C                   Z-ADD     SPA(2)        SATSPD
242700970210     C                   Z-ADD     SPA(3)        SATSPC
242800970207      * SPEDIZIONI a terra da partire
242900970207     C                   Z-ADD     SBP(1)        SATSDE
243000970207     C                   Z-ADD     SBP(2)        SATSDD
243100970207     C                   Z-ADD     SBP(3)        SATSDC
243200970207      * SPEDIZIONI a terra non da partire
243300970207     C                   Z-ADD     SBR(1)        SATSNE
243400970207     C                   Z-ADD     SBR(2)        SATSND
243500970207     C                   Z-ADD     SBR(3)        SATSNC
243600970305      * SPEDIZIONI a terra da partire fuori affidabilità
243700970305     C                   Z-ADD     FAF(1)        SATSFE
243800970305     C                   Z-ADD     FAF(2)        SATSFD
243900970305     C                   Z-ADD     FAF(3)        SATSFC
244000000000      * SPEDIZIONI: complete
244100000000     C                   Z-ADD     SCM(1)        SATSCE
244200970207     C                   Z-ADD     SCM(2)        SATSCD
244300970207     C                   Z-ADD     SCM(3)        SATSCC
244400000000      *
244500970318     C* ASTERISCO LE SEGUENTI IF PERCHE' IL RECORD LO DEVO SEMPRE
244600970318     C* SCRIVERE PER EVITARE CHE NELL'ELABORAZIONE DEL REPORT DI
244700970318     C* SEDE, SE PER UN P.O. IL DA PARTIRE è = 0, VENGA ERRONEAMENTE
244800970318     C* SEGNALATO "DATI NON PERVENUTI".
244900970318     C*          SATSDE    IFGT *ZERO
245000970318     C*          SATSDD    ORGT *ZERO
245100970318     C*          SATSDC    ORGT *ZERO
245200970318     C*          SATSNE    ORGT *ZERO
245300970318     C*          SATSND    ORGT *ZERO
245400970318     C*          SATSNC    ORGT *ZERO
245500970318     C*          SATSPE    ORGT *ZEROS
245600970318     C*          SATSPD    ORGT *ZEROS
245700970318     C*          SATSPC    ORGT *ZEROS
245800940323      *
245900940428     C                   MOVE      $FTR          SATFTR
246000970207     C                   MOVE      $DTR          SATDTR
246100940323      *
246200970207     C                   WRITE     FNSAT000
246300970318     C*
246400970318     C* LE IF SOPRA ASTERISCATE LE MANTENGO PERò PER IMPOSTARE $PRIM4
246500970318     C*
246600970318     C     SATSDE        IFGT      *ZERO
246700970318     C     SATSDD        ORGT      *ZERO
246800970318     C     SATSDC        ORGT      *ZERO
246900970318     C     SATSNE        ORGT      *ZERO
247000970318     C     SATSND        ORGT      *ZERO
247100970318     C     SATSNC        ORGT      *ZERO
247200970318     C     SATSPE        ORGT      *ZEROS
247300970318     C     SATSPD        ORGT      *ZEROS
247400970318     C     SATSPC        ORGT      *ZEROS
247500000405     C     SATVNE        ORGT      *ZEROS
247600000405     C     SATVND        ORGT      *ZEROS
247700000405     C     SATVNC        ORGT      *ZEROS
247800940429     C* PER LA STAMPA DEI TOTALI DI AREA
247900940429     C                   Z-ADD     1             $PRIM4
248000970318     C                   ENDIF
248100000000      *
248200000000     C                   ENDSR
248300000000      **************************************************************************
248400970211      * Scrittura del record c/servizio sul file di trasmissione FNSAT00F
248500000000      **************************************************************************
248600000000     C     WR1SAT        BEGSR
248700000000      *
248800970207     C                   CLEAR                   FNSAT000
248900000000     C                   Z-ADD     SIMFEL        SATFLE
249000000000     C                   Z-ADD     $FILPR        SATLNP
249100000000     C                   Z-ADD     *ZERO         SATTFA
249200950210     C                   Z-ADD     DS1ADA        SATDRE
249300000000     C                   MOVE      'C'           SATTRE
249400000000      *
249500000000      * COLLI: da partire
249600970210     C                   Z-ADD     C4A(1)        SATCPE
249700970210     C                   Z-ADD     C4A(2)        SATCPD
249800970210     C                   Z-ADD     C4A(3)        SATCPC
249900970207      * COLLI: a terra da partire
250000970207     C                   Z-ADD     C4P(1)        SATCDE
250100970207     C                   Z-ADD     C4P(2)        SATCDD
250200970207     C                   Z-ADD     C4P(3)        SATCDC
250300970207      * COLLI: a terra non da partire
250400970207     C                   Z-ADD     C4R(1)        SATCNE
250500970207     C                   Z-ADD     C4R(2)        SATCND
250600970207     C                   Z-ADD     C4R(3)        SATCNC
250700970207      * PESO: da partire
250800970210     C                   Z-ADD     P4A(1)        SATPPE
250900970210     C                   Z-ADD     P4A(2)        SATPPD
251000970210     C                   Z-ADD     P4A(3)        SATPPC
251100970207      * PESO: a terra da partire fino a 35 quintali
251200970207     C                   Z-ADD     P4I(1)        SATFDE
251300970207     C                   Z-ADD     P4I(2)        SATFDD
251400970207     C                   Z-ADD     P4I(3)        SATFDC
251500970207      * PESO: a terra non da partire fino a 35 quintali
251600970207     C                   Z-ADD     P4N(1)        SATFNE
251700970207     C                   Z-ADD     P4N(2)        SATFND
251800970207     C                   Z-ADD     P4N(3)        SATFNC
251900970207      * PESO: a terra da partire oltre 35 quintali
252000970207     C                   Z-ADD     P4P(1)        SATODE
252100970207     C                   Z-ADD     P4P(2)        SATODD
252200970207     C                   Z-ADD     P4P(3)        SATODC
252300970207      * PESO: a terra non da partire oltre 35 quintali
252400970207     C                   Z-ADD     P4R(1)        SATONE
252500970207     C                   Z-ADD     P4R(2)        SATOND
252600970207     C                   Z-ADD     P4R(3)        SATONC
252700000000      * VOLUME: da partire
252800970210     C                   Z-ADD     V4A(1)        SATVPE
252900970210     C                   Z-ADD     V4A(2)        SATVPD
253000970210     C                   Z-ADD     V4A(3)        SATVPC
253100970207      * VOLUME: a terra da partire
253200970207     C                   Z-ADD     V4P(1)        SATVDE
253300970207     C                   Z-ADD     V4P(2)        SATVDD
253400970207     C                   Z-ADD     V4P(3)        SATVDC
253500000331      * VOLUME: totale rimasto a magazzino
253600000331      * (a terra da partire, da non partire, in sospensione)
253700970207     C                   Z-ADD     V4R(1)        SATVNE
253800970207     C                   Z-ADD     V4R(2)        SATVND
253900970207     C                   Z-ADD     V4R(3)        SATVNC
254000970207      * SPEDIZIONI: da partire
254100970210     C                   Z-ADD     S4A(1)        SATSPE
254200970210     C                   Z-ADD     S4A(2)        SATSPD
254300970210     C                   Z-ADD     S4A(3)        SATSPC
254400970207      * SPEDIZIONI: a terra da partire
254500970207     C                   Z-ADD     S4P(1)        SATSDE
254600970207     C                   Z-ADD     S4P(2)        SATSDD
254700970207     C                   Z-ADD     S4P(3)        SATSDC
254800970207      * SPEDIZIONI: a terra non da partire
254900970207     C                   Z-ADD     S4R(1)        SATSNE
255000970207     C                   Z-ADD     S4R(2)        SATSND
255100970207     C                   Z-ADD     S4R(3)        SATSNC
255200970305      * SPEDIZIONI: a terra da partire fuori affidabilità
255300970305     C                   Z-ADD     F4F(1)        SATSFE
255400970305     C                   Z-ADD     F4F(2)        SATSFD
255500970305     C                   Z-ADD     F4F(3)        SATSFC
255600000000      * SPEDIZIONI: complete
255700000000     C                   Z-ADD     S4M(1)        SATSCE
255800970207     C                   Z-ADD     S4M(2)        SATSCD
255900970207     C                   Z-ADD     S4M(3)        SATSCC
256000000000      *
256100970318     C     SATSDE        IFGT      *ZERO
256200970318     C     SATSDD        ORGT      *ZERO
256300970318     C     SATSDC        ORGT      *ZERO
256400970318     C     SATSNE        ORGT      *ZERO
256500970318     C     SATSND        ORGT      *ZERO
256600970318     C     SATSNC        ORGT      *ZERO
256700970318     C     SATSPE        ORGT      *ZERO
256800970318     C     SATSPD        ORGT      *ZERO
256900970318     C     SATSPC        ORGT      *ZERO
257000000405     C     SATVNE        ORGT      *ZEROS
257100000405     C     SATVND        ORGT      *ZEROS
257200000405     C     SATVNC        ORGT      *ZEROS
257300940323      *
257400940428     C                   MOVE      $FTR          SATFTR
257500970207     C                   MOVE      $DTR          SATDTR
257600940323      *
257700970207     C                   WRITE     FNSAT000
257800940429     C* PER LA STAMPA DEI C/S
257900940429     C                   Z-ADD     1             $PRIM2
258000940429     C* PER LA STAMPA DEI TOTALI DI AREA
258100940429     C                   Z-ADD     1             $PRIM4
258200000000     C                   ENDIF
258300000000      *
258400000000      * Aggiorno le schiere per i totali c/servizio per area
258500000000     C                   DO        3             $U                1 0
258600970210     C* A TERRA
258700000000     C                   ADD       C4P($U)       C1P($U)
258800000000     C                   ADD       C4R($U)       C1R($U)
258900000000     C                   ADD       P4I($U)       P1I($U)
259000970207     C                   ADD       P4N($U)       P1N($U)
259100000000     C                   ADD       P4P($U)       P1P($U)
259200000000     C                   ADD       P4R($U)       P1R($U)
259300000000     C                   ADD       V4P($U)       V1P($U)
259400000331     C                   ADD       V4R($U)       V1R($U)
259500000000     C                   ADD       S4P($U)       S1P($U)
259600000000     C                   ADD       S4R($U)       S1R($U)
259700000000     C                   ADD       S4M($U)       S1M($U)
259800970305     C                   ADD       F4F($U)       F1F($U)
259900970210     C* DA PARTIRE
260000970210     C                   ADD       C4A($U)       C1A($U)
260100970210     C                   ADD       P4A($U)       P1A($U)
260200970210     C                   ADD       V4A($U)       V1A($U)
260300970210     C                   ADD       S4A($U)       S1A($U)
260400000000     C                   ENDDO
260500000000      *
260600000000     C                   EXSR      AZZSE4
260700000000      *
260800000000     C                   ENDSR
260900000000      **************************************************************************
261000970211      * Scrittura del record transito sul file di trasmissione FNSAT00F
261100000000      **************************************************************************
261200000000     C     WR2SAT        BEGSR
261300000000      *
261400970211     C                   CLEAR                   FNSAT000
261500000000     C                   Z-ADD     SIMFEL        SATFLE
261600940502     C                   Z-ADD     SIMFEL        SATLNP
261700000000     C                   Z-ADD     *ZERO         SATTFA
261800950210     C                   Z-ADD     DS1ADA        SATDRE
261900000000     C                   MOVE      'T'           SATTRE
262000000000      *
262100000000      * COLLI: da partire
262200970211     C                   Z-ADD     0             SATCPE
262300970211     C                   Z-ADD     0             SATCPD
262400970211     C                   Z-ADD     0             SATCPC
262500970211      * COLLI:
262600970211     C                   Z-ADD     C2P(1)        SATCDE
262700970211     C                   Z-ADD     C2P(2)        SATCDD
262800970211     C                   Z-ADD     C2P(3)        SATCDC
262900970211      * PESO: da partire
263000970211     C                   Z-ADD     0             SATPPE
263100970211     C                   Z-ADD     0             SATPPD
263200970211     C                   Z-ADD     0             SATPPC
263300000000      * PESO: fino a 35 quintali
263400970211     C                   Z-ADD     P2I(1)        SATFDE
263500970211     C                   Z-ADD     P2I(2)        SATFDD
263600970211     C                   Z-ADD     P2I(3)        SATFDC
263700940428      * PESO: (non esiste il bollettato nel periodo o nel precedente)
263800970211     C                   Z-ADD     P2P(1)        SATODE
263900970211     C                   Z-ADD     P2P(2)        SATODD
264000970211     C                   Z-ADD     P2P(3)        SATODC
264100000000      * VOLUME: da partire
264200970211     C                   Z-ADD     0             SATVPE
264300970211     C                   Z-ADD     0             SATVPD
264400970211     C                   Z-ADD     0             SATVPC
264500970211      * VOLUME
264600970211     C                   Z-ADD     V2P(1)        SATVDE
264700970211     C                   Z-ADD     V2P(2)        SATVDD
264800970211     C                   Z-ADD     V2P(3)        SATVDC
264900000000      * SPEDIZIONI: da partire
265000970211     C                   Z-ADD     0             SATSPE
265100970211     C                   Z-ADD     0             SATSPD
265200970211     C                   Z-ADD     0             SATSPC
265300970211      * SPEDIZIONI
265400970211     C                   Z-ADD     S2P(1)        SATSDE
265500970211     C                   Z-ADD     S2P(2)        SATSDD
265600970211     C                   Z-ADD     S2P(3)        SATSDC
265700000000      * SPEDIZIONI: complete
265800940509     C                   Z-ADD     S2M(1)        SATSCE
265900970211     C                   Z-ADD     S2M(2)        SATSCD
266000970211     C                   Z-ADD     S2M(3)        SATSCC
266100000000      *
266200970212     C     SATSDE        IFGT      *ZERO
266300970212     C     SATSDD        ORGT      *ZERO
266400970212     C     SATSDC        ORGT      *ZERO
266500940428     C                   MOVE      $FTR          SATFTR
266600970207     C                   MOVE      $DTR          SATDTR
266700940323      *
266800970211     C                   WRITE     FNSAT000
266900940429     C* PER LA STAMPA DEI TOTALI DI AREA
267000940429     C                   Z-ADD     1             $PRIM4
267100940505     C                   Z-ADD     1             $PRIM3
267200000000     C                   ENDIF
267300000000      *
267400000000     C                   ENDSR
267500940428      **************************************************************************
267600940428      * Stampo i totali generali
267700940428      **************************************************************************
267800940428     C     TOTGEN        BEGSR
267900940428      *
268000940428      * ------------------ PRIMA RIGA, TOTALI DI FILIALE --------------
268100940428      *
268200940506      * stampo i totali generali finali solo se ho stampato i transiti
268300950628    1C     *IN02         IFEQ      *ON
268400940506     C     $PRIM3        ANDEQ     1
268500940506     C* I TOTALI DI AREA SEMPRE
268600940506     C     *IN02         OREQ      *OFF
268700940506     C*
268800940428     C                   SETON                                        40
268900940428     C                   SETOFF                                       41
269000970211     C  N02              SETON                                        45
269100970211     C   02              SETOFF                                       45
269200940428     C                   Z-ADD     *ZERO         FILIAP
269300940428     C                   MOVE      *BLANKS       ORGDEP
269400940428     C   02'TOT.GENE'    CAT       'RALE':0      ORGDEP
269500940428     C  N02              MOVEL     'TOTALE'      ORGDEP
269600940428     C*
269700950118     C                   WRITE     LSC2T3
269800940428      *
269900940428      * ?  C  O   L   L  I  ?
270000970211     C                   XFOOT     C3P           COTOTP
270100970211     C                   XFOOT     C3A           $CDA
270200940428     C                   EXSR      PERCOL
270300940428      *
270400940428      * ?  P  E  S  O  ?
270500970211     C                   XFOOT     P3I           PEF35P
270600970211     C                   XFOOT     P3P           PEO35P
270700970211     C     PEF35P        ADD       PEO35P        PETOTP
270800970228     C                   XFOOT     P3A           $PDA
270900970211     C                   EXSR      PERPES
271000970211      *
271100970211      * ?  V  O  L  U  M E  ?
271200970211     C                   XFOOT     V3P           VOTOTP
271300970211     C                   XFOOT     V3A           $VDA
271400940428     C                   EXSR      PERVOL
271500940428      *
271600940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
271700970211     C                   XFOOT     S3A           $SDA
271800970305     C                   XFOOT     F3F           SPFAFF
271900940502     C                   EXSR      PERSPE
272000970305     C                   XFOOT     S3P           SPTOTP
272100970305     C                   XFOOT     S3M           SPCONP
272200970211     C                   XFOOT     S3R           $COMO1
272300970211     C     SPTOTP        ADD       $COMO1        SPTOTA
272400970211     C     SPTOTA        SUB       SPCONP        SPSPAP
272500000407     C*
272600000407      * ?VOLUME A MAGAZZINO ?
272700000407     C                   XFOOT     V3R           VOTOTM
272800940428      *
272900940428     C                   EXSR      SCRIVI
273000940509     C                   ADD       1             $RIGHE
273100940428      *
273200970211      * ------------------ ESPR./DIST./CORR. --------------------------
273300940428      *
273400970211     C                   SETOFF                                       40
273500950628    2C                   DO        3             $V
273600940428     C                   MOVE      ENS($V)       TIPOP
273700940428      *
273800940428      * ?  C  O   L   L  I  ?
273900970211     C                   Z-ADD     C3P($V)       COTOTP
274000970211     C                   Z-ADD     C3A($V)       $CDA
274100940428     C                   EXSR      PERCOL
274200940428      *
274300940428      * ?  P  E  S  O  ?
274400940428     C                   Z-ADD     P3I($V)       PEF35P
274500970211     C                   Z-ADD     P3P($V)       PEO35P
274600970211     C     PEF35P        ADD       PEO35P        PETOTP
274700970211     C                   Z-ADD     P3A($V)       $PDA
274800940428     C                   EXSR      PERPES
274900940428      *
275000940428      * ?  V  O  L  U  M E  ?
275100970211     C                   Z-ADD     V3P($V)       VOTOTP
275200970211     C                   Z-ADD     V3A($V)       $VDA
275300940428     C                   EXSR      PERVOL
275400940428      *
275500940428      * ?  S  P  E  D  I  Z  I  O  N  I  ?
275600970211     C                   Z-ADD     S3A($V)       $SDA
275700970305     C                   Z-ADD     F3F($V)       SPFAFF
275800940502     C                   EXSR      PERSPE
275900970305     C                   Z-ADD     S3P($V)       SPTOTP
276000970305     C                   Z-ADD     S3M($V)       SPCONP
276100970211     C     S3P($V)       ADD       S3R($V)       SPTOTA
276200970211     C     SPTOTA        SUB       S3M($V)       SPSPAP
276300000406      * ?VOLUME A MAGAZZINO ?
276400000406     C                   Z-ADD     V3R($V)       VOTOTM
276500940428      *
276600940428     C                   EXSR      SCRIVI
276700940428      *
276800950628    2C                   ENDDO
276900940428      *
277000950118     C  N02              WRITE     LSC2T2
277100950628    1C                   ENDIF
277200940429     C*
277300950118     C   02              WRITE     LSC2T3
277400940428     C*
277500940428     C   02              EXSR      AZZTOT
277600940428      *
277700940428     C                   ENDSR
277800000000      **************************************************************************
277900000000      * Elaborazione transiti
278000940505      * Non posso leggere per data uscita dal transito perche' spesso
278100940505      * capita che ci siano bolle con data entrata un giorno e data
278200940505      * uscita il giorno prima: per questo elaboro tutte le bolle
278300950210      * che hanno la data di entrata al transito > 0 e <=a DS1AAL
278400000000      **************************************************************************
278500000000     C     ELATRA        BEGSR
278600000000      *
278700940505     C                   Z-ADD     0             $PRIM1
278800940428     C                   MOVE      *BLANK        $BLTPR
278900940429     C                   MOVE      *BLANK        $CBLPR
279000940428     C                   MOVE      'T'           $FLAG
279100000000      *
279200000000      * Leggo il file delle bolle in transito
279300940505     C                   Z-ADD     1             $DFV
279400020625     C     kbtt39        SETLL     FNBTT39L
279500020625     C     simfel        READe     FNBTT39L                               33
279600940428     C*
279700940428    2C     *IN33         DOWEQ     *OFF
279800950210     C     BTTDET        ANDLE     DS1AAL
279900000000      *
280000940505     C                   EXSR      ESCLU1
280100940428      *
280200940505    3C     $OK           IFEQ      'S'
280300940505     C                   Z-ADD     1             $PRIM1
280400000000      *
280500000000      * Aggancio, nel file statistiche, il record per il terminal di arrivo
280600000000      * poiche' lo dovro aggiornare
280700970211     C                   CLEAR                   FNSAT000
280800000000     C                   Z-ADD     SIMFEL        SATFLE
280900950210     C                   Z-ADD     DS1ADA        SATDRE
281000940502     C                   Z-ADD     SIMFEL        SATLNP
281100950118     C                   Z-ADD     BTPTFA        SATTFA
281200000000     C                   MOVE      'T'           SATTRE
281300970205     C     FNSA1K        CHAIN     FNSAT02L                           37
281400940428      *
281500021217      * se è la prima volta che leggo la bolla carico i dati: VOL e PKG
281600021217    4C     $BOLLA        IFEQ      '1'
281700021217     C                   EXSR      DATBOT
281800021217    4C                   ENDIF
281900021217      *
282000021220      * $PESO = PESO DI UN COLLO, DA USARE PER SOMME.
282100030213      **** Peso unitario se disponibile per ogni collo, medio altrimenti
282200030213      * Peso medio
282300030213      * ("metti" che qualche collo sia stato pesato in arrivo...)
282400030213if  1c***                if        Wsipes = 'S'
282500030213     c***                z-add     BTTpuc        $Peso
282600030213   x1c***                else
282700021220     c                   z-add     Wpesmed       $Peso
282800030213e   1c***                endif
282900021220      * $VOLUM= VOLUME DI UN COLLO DA USARE PER SOMME.
283000030213      **** Volume unitario se disponibile per ogni collo, medio altrimenti
283100030213      * Volume medio
283200030213      * ("metti" che qualche collo sia stato volumato in arrivo...)
283300030213if  1c***                if        Wsivol = 'S'
283400030213     c***                z-add     BTTvuc        $Volum
283500030213   x1c***                else
283600021220     c                   z-add     Wvolmed       $Volum
283700030213e   1c***                endif
283800000000      *
283900970211      * Verifico il tipo spedizione ESPR./DIST./CORR.
284000950201     C                   Z-ADD     3             $T
284100950118    4C     BTPTSP        IFEQ      'E'
284200000000     C                   Z-ADD     1             $T
284300940428    4C                   ENDIF
284400950201    4C     BTPTSP        IFEQ      'D'
284500940323     C                   Z-ADD     2             $T
284600940428    4C                   ENDIF
284700000000      *
284800000000      * Ho due controlli da fare per sapere se il collo e' a terra:
284900940428      *    1)  BTTDUT = 0
285000940428      *    2)  BTTDUT > Data spedizione
285100000000     C                   SETOFF                                       36
285200940428     C     BTTDUT        IFEQ      0
285300950210     C     BTTDUT        ORGT      DS1AAL
285400000000     C                   SETON                                        36
285500000000     C                   ENDIF
285600940506      *
285700000000      * Sommo solo se il collo risulta a terra
285800940428    4C     *IN36         IFEQ      *ON
285900940506      *
286000940506      * ?  CALCOLI  SUI  COLLI  ?
286100940506      *
286200940509     C                   ADD       1             C2P($T)
286300940428     C                   ADD       1             C0P($T)
286400000000      *
286500000000      * ?  CALCOLI  SUL  PESO  ?
286600000000      *
286700000000      * Verifico il peso di 35 quintali
286800021217    5***C     BTPPKB        IFLE      3500
286900021217    5C     Wpeso         IFLE      3500
287000940509     C                   ADD       $PESO         P2I($T)
287100000000     C                   ADD       $PESO         P0I($T)
287200970211     C                   ELSE
287300970211      * Oltre 35 quintali:                                             (35ON)
287400970211     C                   ADD       $PESO         P2P($T)
287500970211     C                   ADD       $PESO         P0P($T)
287600940428    5C                   ENDIF
287700000000      *
287800000000      *
287900000000      * ?  CALCOLI  SUL  VOLUME  ?
288000000000      *
288100021217     *** * Calcolo volume di un collo, ($VOLUM) da usare per somme
288200021217     **5C     BTTVUC        IFEQ      *ZERO
288300021217     *** *
288400021217     ***C     BTPVLB        DIV(H)    BTPNCL        $VOLUM
288500021217     *** *
288600021217     **5C                   ELSE
288700021217     ***C                   Z-ADD     BTTVUC        $VOLUM
288800021217     **5C                   ENDIF
288900000000      *
289000940509     C                   ADD       $VOLUM        V2P($T)
289100940428     C                   ADD       $VOLUM        V0P($T)
289200000000      *
289300000000      * ?  CALCOLI  SULLE  SPEDIZIONI  ?
289400000000      *
289500000000      * Visto che le spedizioni non riesco a calcolarle ora, mi limito a tenere
289600000000      * aggiornato il file di work con le bolle
289700950118     C                   Z-ADD     BTPLNP        WRKLNP
289800000000     C                   MOVE      *BLANK        WRKCSE
289900970409     C     FNWR2K        CHAIN     FNWRK00F                           99
290000940428    5C     *IN99         IFEQ      *OFF
290100000000     C                   ADD       1             WRKNC1
290200000000     C                   EXCEPT    AGGIO
290300940428   X5C                   ELSE
290400950118     C                   MOVEL     'T'           WTRANS
290500940503     C                   EXSR      WRIWRK
290600940428    5C                   ENDIF
290700940428      *
290800040921     C                   MOVE      $FTR          SATFTR
290900040921     C                   MOVE      $DTR          SATDTR
291000940428    5C     *IN37         IFEQ      *ON
291100040921     C***                MOVE      $FTR          SATFTR
291200040921     C***                MOVE      $DTR          SATDTR
291300970211     C                   WRITE     FNSAT000
291400940428   X5C                   ELSE
291500970211     C                   UPDATE    FNSAT000
291600940428    5C                   ENDIF
291700000000      *
291800940428    4C                   ENDIF
291900940428    3C                   ENDIF
292000000000      *
292100020625     C     simfel        READE     FNBTT39L                               33
292200940428    2C                   ENDDO
292300940506     C*
292400000000     C     $PRIM1        IFNE      *ZERO
292500940506     C                   EXSR      CALSPE
292600000000     C                   EXSR      WR2SAT
292700000000     C                   ENDIF
292800000000      *
292900000000     C                   ENDSR
293000940503      **************************************************************************
293100000411      * elimino i terminal che non hanno VOLUMI TOT A MAG>0
293200940503      **************************************************************************
293300940503     C     ELATER        BEGSR
293400950210     C                   Z-ADD     DS1FEL        $AREA
293500940503     C                   CLEAR                   $TFA
293600940503     C                   SETOFF                                       33
293700940503     C*
293800940503     C* ELABORO TERMINAL PER TERMINAL
293900940503     C                   EXSR      IMPTFA
294000940503     C*
294100940503    2C     *IN33         DOWEQ     *OFF
294200940503     C                   Z-ADD     *ZERO         $PRIM1
294300940503      *
294400970211     C     TASATK        READE(N)  FNSAT03L                               33
294500940503     C                   MOVEL     SATTRE        $TRE
294600940503     C*
294700940503    3C     *IN33         DOWEQ     *OFF
294800940503     C*
294900940503    4C     $TRE          IFNE      SATTRE
295000940503     C     $PRIM1        ANDEQ     0
295100970211     C     TRSATK        SETLL     FNSAT03L
295200970211     C     TRSATK        READE     FNSAT03L                               33
295300940503     C*
295400940503    5C     *IN33         DOWEQ     *OFF
295500970211     C                   DELETE    FNSAT003
295600970211     C     TRSATK        READE     FNSAT03L                               33
295700940503    5C                   ENDDO
295800940503     C                   MOVEL     SATTRE        $TRE
295900940503     C                   CLEAR                   $PRIM1
296000940503    4C                   ENDIF
296100940503     C*
296200000411     C* VEDO SE C'E' ALMENO UN VOLUME A MAGAZZINO
296300000411    4C     SATVNE        IFGT      0
296400000411     C     SATVND        ORGT      0
296500000411     C     SATVNC        ORGT      0
296600940503     C                   Z-ADD     1             $PRIM1
296700940503    4C                   ENDIF
296800970211     C     TASATK        READE(N)  FNSAT03L                               33
296900940503    3C                   ENDDO
297000940503     C* ELABORO L'ULTIMO
297100940503    3C     $PRIM1        IFEQ      0
297200970211     C     TRSATK        SETLL     FNSAT03L
297300970211     C     TRSATK        READE     FNSAT03L                               33
297400970211     C*
297500940503    4C     *IN33         DOWEQ     *OFF
297600970211     C                   DELETE    FNSAT003
297700970211     C     TRSATK        READE     FNSAT03L                               33
297800940503    4C                   ENDDO
297900940503    3C                   ENDIF
298000940503     C*
298100940503     C                   EXSR      IMPTFA
298200940503    2C                   ENDDO
298300940504     C*
298400940504     C                   ENDSR
298500000000      **************************************************************************
298600000000      * Stampo i terminal di arrivo
298700000000      **************************************************************************
298800000000     C     STATER        BEGSR
298900940429     C                   SETOFF                                       4333
299000940429     C                   SETON                                        30
299100950210     C                   Z-ADD     DS1FEL        $AREA
299200940429     C                   CLEAR                   $TFA
299300940509     C                   CLEAR                   $RIGHE
299400940429     C* ELABORO AREA PER AREA
299500940429     C  N39              EXSR      IMPARE
299600940429     C*
299700940429    2C     *IN33         DOWEQ     *OFF
299800940429     C*
299900940429     C* ELABORO TERMINAL PER TERMINAL
300000940429     C                   EXSR      IMPTFA
300100000000      *
300200940429    3C     *IN33         DOWEQ     *OFF
300300940429     C                   Z-ADD     *ZERO         $PRIM1
300400940429     C                   Z-ADD     *ZERO         $PRIM3
300500000000      *
300600970211     C     ARSATK        READE(N)  FNSAT03L                               33
300700940429     C*
300800940429    4C     *IN33         DOWEQ     *OFF
300900000000      *
301000940429     C* ELABORO SE DATA COMPRESA
301100940429     C* E, SE IMMESSA SOLTANTO UNA LINEA, SE E' LA LINEA RICHIESTA
301200970211    5C     SATDRE        IFLE      DS1AAL
301300940429     C*
301400940429     C* SE DA STAMPE GIORNALIERE ELABORO SLTANTO LE LINEE IN LIN
301500940429     C     SATLNP        LOOKUP    LIN                                    99
301600940429     C   99              EXSR      SERTRA
301700940429     C*
301800940429   X5C                   ELSE
301900940429     C                   SETON                                        33
302000940429    5C                   ENDIF
302100940429     C*
302200970211     C     ARSATK        READE(N)  FNSAT03L                               33
302300940429    4C                   ENDDO
302400940429      *
302500940429     C* Stampo le bolle dell'area
302600940429    4C     $PRIM1        IFEQ      1
302700940429     C                   EXSR      TOTFIL
302800940429     C                   EXSR      AZZFIL
302900940429    4C                   ENDIF
303000940429      *
303100940429      * Stampo le bolle in transito
303200940429    4C     $PRIM3        IFEQ      1
303300940429     C                   EXSR      TOTTRA
303400940429    4C                   ENDIF
303500940429     C*
303600940429     C* Elaboro nuovo terminal
303700940429     C                   EXSR      IMPTFA
303800940429    3C                   ENDDO
303900940429      *
304000940509     C                   Z-ADD     1             $PRIM3
304100970226     C                   SETON                                        0204
304200940429     C                   EXSR      TOTGEN
304300970226     C                   SETOFF                                       0204
304400940429     C* ELABORO NUOVA AREA
304500940429     C  N39              EXSR      IMPARE
304600940429    2C                   ENDDO
304700940429     C*
304800940429     C                   ENDSR
304900000000      **************************************************************************
305000000000      * Una volta letto il record verifico se da includere o no
305100000000      *
305200940428      * Includo se:
305300940428      * e' entrato al transito
305400940428      *  e non e' ancora uscito o e' uscito con data = > a  data elabor
305500940505      *            BTTDUT = 0
305600000000      *   oppure:
305700950210      *       AND  BTTDUT >= DS1ADAL
305800940428      *
305900940428      * escludo sempre i recuperi
306000000000      **************************************************************************
306100000000     C     ESCLU1        BEGSR
306200000000      *
306300000000     C                   MOVE      'N'           $OK
306400000000      *
306500000000     C     BTTDUT        IFEQ      *ZERO
306600950210     C     BTTDUT        ORGE      DS1AAL
306700000000     C                   MOVE      'S'           $OK
306800940428     C                   ENDIF
306900000000      *
307000000000      * Solo se superati i test ne eseguo altri
307100000000     C     $OK           IFEQ      'S'
307200940505      *
307300940505      * Aggancio la bolla, se diversa dalla precedente (non e' significativo
307400940505      * come test ma mi permette di saltare delle chain)
307500950118    3C     $BTTAT        IFNE      $BLTPR
307600020625     C     FNBTPK        CHAIN     FNBTP11L                           99
307700950118     C  N99              MOVE      $BTTAT        $BLTPR
307800940505     C  N99              MOVEL     '1'           $BOLLA
307900940505     C   99              MOVEL     'N'           $OK
308000940505    3C                   ENDIF
308100000000      *
308200000000      * Eseguo la chain su TABEL solo se diverso dal precedente
308300950118     C  N99BTPCBO        IFNE      $CBLPR
308400950210     C                   MOVE      '3A'          KCOD
308500950210     C                   MOVE      *BLANKS       KKEY
308600950210     C                   MOVEL     BTPCBO        KKEY
308700950210     C     KTAB1         CHAIN     TABEL00F                           99
308800000000     C  N99              MOVEL     TBLUNI        DS3A
308900950118     C  N99              MOVE      BTPCBO        $CBLPR
309000000000     C   99              CLEAR                   DS3A
309100000000     C                   ENDIF
309200000000      *
309300000000      * Non considero i recuperi
309400940428     C     §3ARBL        IFEQ      'R'
309500000000     C                   MOVE      'N'           $OK
309600000000     C                   ENDIF
309700000000      *
309800000000     C                   ENDIF
309900000000      *
310000000000     C                   ENDSR
310100000000      **************************************************************************
310200970211      * RISTAMPA: lettura dal file statistiche (FNSAT00F) e output
310300000000      **************************************************************************
310400000000     C     RISTAM        BEGSR
310500000000      *
310600940502     C                   SETOFF                                       33
310700940428     C                   SETON                                        43
310800940429     C                   CLEAR                   $TFA
310900940509     C                   CLEAR                   $RIGHE
311000950210     C                   MOVEL     DS1FEL        $AREA
311100940429     C* LANCIO DA FILTRO
311200950210    1C     DS1FGS        IFGT      0
311300950210     C                   MOVEL     DS1LNP        SATLNP
311400940429     C*
311500940429     C* SE NON IMMESSA L'AREA LA CERCO
311600940429     C  N39              EXSR      IMPARE
311700940429     C*
311800940429    2C     *IN33         DOWEQ     *OFF
311900940429     C                   Z-ADD     *ZERO         $PRIM2
312000940429     C                   Z-ADD     *ZERO         $PRIM3
312100940429     C                   Z-ADD     *ZERO         $PRIM4
312200940429     C*
312300940429     C* SE NON IMMESSA LA LNP LA CERCO
312400940505     C                   EXSR      IMPLNP
312500000000      *
312600940429    3C     *IN33         DOWEQ     *OFF
312700940429     C                   Z-ADD     *ZERO         $PRIM1
312800940429     C* LEGGO IL FILE PER LNP
312900940429     C                   EXSR      LEGSAT
313000940429     C* cerco la nuova lnp
313100940429     C  N29              EXSR      IMPLNP
313200940429    3C                   ENDDO
313300940429     c*
313400940429     C* stampa finale dei C/S transiti e totale
313500940429     C                   EXSR      STAFIN
313600940429     C* cerco la nuova area
313700940429     C  N39              EXSR      IMPARE
313800940429    2C                   ENDDO
313900000000      *
314000950210   X1C                   ELSE
314100940429      *
314200940429      * Da stampe giornaliere
314300940429     C                   Z-ADD     1             $X
314400940429     C*
314500940506     C                   Z-ADD     *ZERO         $PRIM2            1 0
314600940506     C                   Z-ADD     *ZERO         $PRIM3            1 0
314700940506     C                   Z-ADD     *ZERO         $PRIM4            1 0
314800000912     C                   CLEAR                   WSTAF
314900940429    2C     LIN($X)       DOWGT     0
315000940429     C                   Z-ADD     LIN($X)       $LNP
315100940506     C                   Z-ADD     LIN($X)       $FILPR
315200940505     C                   Z-ADD     *ZERO         $PRIM1            1 0
315300000912     C* ESCLCUDO I P.O. POSTE
315400000912      * Escludo bolle LNP Poste
315500020813     ***C                   CLEAR                   §OGPT
315600020813     C                   clear                   OG143
315700000912     C     $FILPR        CHAIN     AZORG01L                           82
315800000912     C  N82              MOVEL     ORGDE3        OG143
315900000912     C**
316000020813   2A***C     §OGPT         IFNE      'S'
316100020813    2C                   IF        §OGntw <> 'PPT'
316200000912     C* SE ELABORATA ALMENO UNA LINEA, STAMPO ANCHE I TOTALI FINALI
316300000912     C                   MOVEL     'S'           WSTAF             1
316400000912     C*
316500970205     C     DTSATK        SETLL     FNSAT02L
316600940429     C* LEGGO IL FILE PER LINEA
316700940429     C                   EXSR      LEGSAT
316800000912     C                   ENDIF
316900940429     C                   ADD       1             $X
317000940429    2C                   ENDDO
317100940429     c*
317200000912     C     WSTAF         IFEQ      'S'
317300940429     C                   EXSR      STAFIN
317400940429    1C                   ENDIF
317500000912    1C                   ENDIF
317600940429     C*
317700940429     C                   ENDSR
317800940429      **************************************************************************
317900940429      * IMPOSTO L'AREA, LE CERCO MAN MANO                              i
318000940429      **************************************************************************
318100940429     C     IMPARE        BEGSR
318200970205     C     $AREA         SETGT     FNSAT02L
318300970205     C                   READ(N)   FNSAT02L                               33
318400940429     C*
318500940429     C     *IN33         IFEQ      *OFF
318600940429     C                   MOVEL     SATFLE        $AREA
318700940429     C                   ENDIF
318800940429     C                   ENDSR
318900940429      **************************************************************************
319000940429      * IMPOSTO LA LINEA DI PARTENZA, LA CERCO MAN MANO                i
319100940429      **************************************************************************
319200940429     C     IMPLNP        BEGSR
319300970205     C  N29LPSATK        SETGT     FNSAT02L
319400970205     C  N29ARSATK        READE(N)  FNSAT02L                               33
319500000619     C                   CLEAR                   WTROVA
319600940429     C*
319700000619    1C     WTROVA        DOWEQ     ' '
319800000619    2C     *IN33         IFEQ      *OFF
319900000619     C     SATLNP        LOOKUP    LIN                                    99
320000000619    3C     *IN99         IFEQ      *ON
320100940429     C                   MOVEL     SATLNP        $LNP
320200940429     C                   MOVEL     SATLNP        $FILPR
320300000619     C                   MOVEL     '1'           WTROVA            1
320400970205     C     DTSATK        SETLL     FNSAT02L
320500000619   X3C                   ELSE
320600000619     C  N29ARSATK        READE(N)  FNSAT02L                               33
320700000619    3C                   ENDIF
320800000619     C**
320900000619   X2C                   ELSE
321000000619     C                   MOVEL     '1'           WTROVA
321100000619    2C                   ENDIF
321200000619    1C                   ENDDO
321300000619     C*
321400940429     C                   ENDSR
321500940429      **************************************************************************
321600940429      * IMPOSTO IL TERMINAL, LO CERCO MAN MANO                         i
321700940429      **************************************************************************
321800940429     C     IMPTFA        BEGSR
321900970211     C     ARSATK        SETGT     FNSAT03L
322000970211     C     $AREA         READE(N)  FNSAT03L                               33
322100940429     C*
322200940429     C     *IN33         IFEQ      *OFF
322300940429     C                   MOVEL     SATTFA        $TFA
322400940429     C                   MOVEL     SATTFA        $FILPR
322500970211     C     TASATK        SETLL     FNSAT03L
322600940429     C                   ENDIF
322700940429     C                   ENDSR
322800940429      **************************************************************************
322900940429      * STAMPA FINALE DEI C/S E TRANSITI                               i
323000940429      **************************************************************************
323100940429     C     STAFIN        BEGSR
323200940429     C* stampo i totali C/S
323300940506     C     *IN38         IFEQ      *OFF
323400940429     C     $PRIM2        IFEQ      1
323500940429     C                   EXSR      TOTSER
323600940429     C                   ENDIF
323700940506     C* TOTALI DI AREA
323800970226     C                   SETON                                        04
323900940505     C                   EXSR      TOTGEN
324000970226     C                   SETOFF                                       04
324100940506     C                   ENDIF
324200940502     C*
324300940502      * Terminate le letture per filiale tocca ai transiti
324400950210     C     DS1RIS        IFNE      'R'
324500940502     C                   EXSR      ELATRA
324600940503     C*
324700940505     C* CANCELLO LE RIGHE DEI TERMINAL CHE NON HANNO COLLI A TERRA
324800940503     C                   EXSR      ELATER
324900940502     C                   ENDIF
325000940502     C*
325100940429     C* stampo i totali dei TRANSITI
325200940506     C     *IN38         IFEQ      *OFF
325300000308     C     *IN27         IFEQ      *OFF
325400000308     C     DS1RIS        OREQ      'R'
325500000308     C                   EXSR      TOTTRA
325600000308     C                   ENDIF
325700940429     c* stampo il totale di area
325800940429     C     $PRIM4        IFEQ      1
325900970226     C                   SETON                                        0204
326000940429     C                   EXSR      TOTGEN
326100970226     C                   SETOFF                                       0204
326200950118     C                   WRITE     LSC2E4
326300940429     C                   ENDIF
326400940506     C                   ENDIF
326500940429     C                   ENDSR
326600940429      **************************************************************************
326700950210      * IMPOSTO L'AREA, LE CERCO MAN MANO
326800940429      **************************************************************************
326900940429     C     LEGSAT        BEGSR
327000970205     C     LPSATK        READE(N)  FNSAT02L                               33
327100940429     C*
327200940429    4C     *IN33         DOWEQ     *OFF
327300940429      *
327400970211    5C     SATDRE        IFLE      DS1AAL
327500940429      * Aggiorno i totali
327600940429     C                   EXSR      SERTRA
327700940429   x5C                   ELSE
327800940429     C                   SETON                                        33
327900940429    5C                   ENDIF
328000940429     C*
328100970205     C  N33LPSATK        READE(N)  FNSAT02L                               33
328200940429    4C                   ENDDO
328300940429     C* totali di filiale
328400940429    4C     $PRIM1        IFEQ      1
328500940429     C                   EXSR      TOTFIL
328600940429     C                   EXSR      AZZFIL
328700940429    4C                   ENDIF
328800940429     C                   ENDSR
328900000000      **************************************************************************
329000000000      * RISTAMPA: per servizi o transiti non stampo ma aggiorno i totali
329100000000      **************************************************************************
329200000000     C     SERTRA        BEGSR
329300000000      *
329400000000     C     SATTRE        IFEQ      *BLANK
329500940429     C                   Z-ADD     1             $PRIM1
329600970211     C                   ADD       SATCPE        CPA(1)
329700970211     C                   ADD       SATCPD        CPA(2)
329800970211     C                   ADD       SATCPC        CPA(3)
329900970211     C                   ADD       SATCDE        CBP(1)
330000970211     C                   ADD       SATCDD        CBP(2)
330100970228     C                   ADD       SATCDC        CBP(3)
330200970211     C                   ADD       SATCNE        CBR(1)
330300970211     C                   ADD       SATCND        CBR(2)
330400970211     C                   ADD       SATCNC        CBR(3)
330500970211     C                   ADD       SATPPE        PPA(1)
330600970211     C                   ADD       SATPPD        PPA(2)
330700970211     C                   ADD       SATPPC        PPA(3)
330800970211     C                   ADD       SATFDE        PFI(1)
330900970211     C                   ADD       SATFDD        PFI(2)
331000970211     C                   ADD       SATFDC        PFI(3)
331100970211     C                   ADD       SATFNE        PFN(1)
331200970211     C                   ADD       SATFND        PFN(2)
331300970211     C                   ADD       SATFNC        PFN(3)
331400970211     C                   ADD       SATODE        PBP(1)
331500970211     C                   ADD       SATODD        PBP(2)
331600970211     C                   ADD       SATODC        PBP(3)
331700970211     C                   ADD       SATONE        PBR(1)
331800970211     C                   ADD       SATOND        PBR(2)
331900970211     C                   ADD       SATONC        PBR(3)
332000970211     C                   ADD       SATVPE        VPA(1)
332100970211     C                   ADD       SATVPD        VPA(2)
332200970211     C                   ADD       SATVPC        VPA(3)
332300970211     C                   ADD       SATVDE        VBP(1)
332400970211     C                   ADD       SATVDD        VBP(2)
332500970211     C                   ADD       SATVDC        VBP(3)
332600970211     C                   ADD       SATVNE        VBR(1)
332700970211     C                   ADD       SATVND        VBR(2)
332800970211     C                   ADD       SATVNC        VBR(3)
332900970211     C                   ADD       SATSPE        SPA(1)
333000970211     C                   ADD       SATSPD        SPA(2)
333100970211     C                   ADD       SATSPC        SPA(3)
333200970211     C                   ADD       SATSDE        SBP(1)
333300970211     C                   ADD       SATSDD        SBP(2)
333400970211     C                   ADD       SATSDC        SBP(3)
333500970211     C                   ADD       SATSNE        SBR(1)
333600970211     C                   ADD       SATSND        SBR(2)
333700970211     C                   ADD       SATSNC        SBR(3)
333800970305     C                   ADD       SATSFE        FAF(1)
333900970305     C                   ADD       SATSFD        FAF(2)
334000970305     C                   ADD       SATSFC        FAF(3)
334100000000     C                   ADD       SATSCE        SCM(1)
334200970211     C                   ADD       SATSCD        SCM(2)
334300970211     C                   ADD       SATSCC        SCM(3)
334400000000     C                   ENDIF
334500000000      *
334600000000     C     SATTRE        IFEQ      'C'
334700940429     C                   Z-ADD     1             $PRIM2
334800970211     C                   ADD       SATCPE        C1A(1)
334900970211     C                   ADD       SATCPD        C1A(2)
335000970211     C                   ADD       SATCPC        C1A(3)
335100970211     C                   ADD       SATCDE        C1P(1)
335200970211     C                   ADD       SATCDD        C1P(2)
335300970211     C                   ADD       SATCDC        C1P(3)
335400970211     C                   ADD       SATCNE        C1R(1)
335500970211     C                   ADD       SATCND        C1R(2)
335600970211     C                   ADD       SATCNC        C1R(3)
335700970211     C                   ADD       SATPPE        P1A(1)
335800970303     C                   ADD       SATPPD        P1A(2)
335900970303     C                   ADD       SATPPC        P1A(3)
336000970211     C                   ADD       SATFDE        P1I(3)
336100970211     C                   ADD       SATFDD        P1I(2)
336200970211     C                   ADD       SATFDC        P1I(3)
336300970211     C                   ADD       SATFNE        P1N(1)
336400970211     C                   ADD       SATFND        P1N(2)
336500970211     C                   ADD       SATFNC        P1N(3)
336600970211     C                   ADD       SATODE        P1P(1)
336700970211     C                   ADD       SATODD        P1P(2)
336800970211     C                   ADD       SATODC        P1P(3)
336900970211     C                   ADD       SATONE        P1R(1)
337000970211     C                   ADD       SATOND        P1R(2)
337100970211     C                   ADD       SATONC        P1R(3)
337200970211     C                   ADD       SATVPE        V1A(1)
337300970211     C                   ADD       SATVPD        V1A(2)
337400970211     C                   ADD       SATVPC        V1A(3)
337500970211     C                   ADD       SATVDE        V1P(1)
337600970211     C                   ADD       SATVDD        V1P(2)
337700970211     C                   ADD       SATVDC        V1P(3)
337800970211     C                   ADD       SATVNE        V1R(1)
337900970211     C                   ADD       SATVND        V1R(2)
338000970211     C                   ADD       SATVNC        V1R(3)
338100970211     C                   ADD       SATSPE        S1A(1)
338200970211     C                   ADD       SATSPD        S1A(2)
338300970211     C                   ADD       SATSPC        S1A(3)
338400970211     C                   ADD       SATSDE        S1P(1)
338500970211     C                   ADD       SATSDD        S1P(2)
338600970211     C                   ADD       SATSDC        S1P(3)
338700970211     C                   ADD       SATSNE        S1R(1)
338800970211     C                   ADD       SATSND        S1R(2)
338900970211     C                   ADD       SATSNC        S1R(3)
339000970305     C                   ADD       SATSFE        F1F(1)
339100970305     C                   ADD       SATSFD        F1F(2)
339200970305     C                   ADD       SATSFC        F1F(3)
339300000000     C                   ADD       SATSCE        S1M(1)
339400970211     C                   ADD       SATSCD        S1M(2)
339500970211     C                   ADD       SATSCC        S1M(3)
339600000000     C                   ENDIF
339700000000      *
339800940429     C     SATTRE        IFEQ      'T'
339900940429     C                   Z-ADD     1             $PRIM3
340000970211     C                   ADD       SATCDE        C2P(1)
340100970211     C                   ADD       SATCDD        C2P(2)
340200970211     C                   ADD       SATCDC        C2P(3)
340300970211     C                   ADD       SATFDE        P2I(1)
340400970211     C                   ADD       SATFDD        P2I(2)
340500970211     C                   ADD       SATFDC        P2I(3)
340600970211     C                   ADD       SATODE        P2P(1)
340700970211     C                   ADD       SATODD        P2P(2)
340800970211     C                   ADD       SATODC        P2P(3)
340900970211     C                   ADD       SATVDE        V2P(1)
341000970211     C                   ADD       SATVDD        V2P(2)
341100970211     C                   ADD       SATVDC        V2P(3)
341200970211     C                   ADD       SATSDE        S2P(1)
341300970211     C                   ADD       SATSDD        S2P(2)
341400970211     C                   ADD       SATSDC        S2P(3)
341500000000     C                   ADD       SATSCE        S2M(1)
341600970211     C                   ADD       SATSCD        S2M(2)
341700970211     C                   ADD       SATSCC        S2M(3)
341800940505     C*
341900940505     C                   ELSE
342000000000      *
342100940429      *  totali generali
342200940429     C                   Z-ADD     1             $PRIM4
342300970211     C                   ADD       SATCDE        C3P(1)
342400970211     C                   ADD       SATCDD        C3P(2)
342500970211     C                   ADD       SATCDC        C3P(3)
342600970211     C                   ADD       SATFDE        P3I(1)
342700970211     C                   ADD       SATFDD        P3I(2)
342800970211     C                   ADD       SATFDC        P3I(3)
342900970212     C                   ADD       SATODE        P3P(1)
343000970212     C                   ADD       SATODD        P3P(2)
343100970212     C                   ADD       SATODC        P3P(3)
343200970211     C                   ADD       SATVDE        V3P(1)
343300970211     C                   ADD       SATVDD        V3P(2)
343400970211     C                   ADD       SATVDC        V3P(3)
343500000406     C                   ADD       SATVNE        V3R(1)
343600000406     C                   ADD       SATVND        V3R(2)
343700000406     C                   ADD       SATVNC        V3R(3)
343800970211     C                   ADD       SATSDE        S3P(1)
343900970211     C                   ADD       SATSDD        S3P(2)
344000970211     C                   ADD       SATSDC        S3P(3)
344100970211     C                   ADD       SATSNE        S3R(1)
344200970211     C                   ADD       SATSND        S3R(2)
344300970211     C                   ADD       SATSNC        S3R(3)
344400970305     C                   ADD       SATSFE        F3F(1)
344500970305     C                   ADD       SATSFD        F3F(2)
344600970305     C                   ADD       SATSFC        F3F(3)
344700000000     C                   ADD       SATSCE        S3M(1)
344800970211     C                   ADD       SATSCD        S3M(2)
344900970211     C                   ADD       SATSCC        S3M(3)
345000940505     C                   ENDIF
345100000000      *
345200000000     C                   ENDSR
345300940505      **************************************************************************
345400940505      * RISTAMPA: addiziono i transiti al totale generale              i
345500940505      **************************************************************************
345600940505     C     ADDTRA        BEGSR
345700940505     C                   Z-ADD     1             $PRIM4
345800940505     C                   ADD       C2P           C3P
345900940505     C                   ADD       P2I           P3I
346000940505     C                   ADD       P2P           P3P
346100940505     C                   ADD       V2P           V3P
346200940505     C                   ADD       S2P           S3P
346300940505     C                   ADD       S2M           S3M
346400940505     C                   ENDSR
346500000000      **************************************************************************
346600000000      * RISTAMPA: azzera i totali generali
346700000000      **************************************************************************
346800000000     C     AZZTOT        BEGSR
346900000000      *
347000970212     C                   CLEAR                   C3A
347100970212     C                   CLEAR                   C3P
347200970211     C                   CLEAR                   P3I
347300970211     C                   CLEAR                   P3P
347400970211     C                   CLEAR                   P3A
347500970211     C                   CLEAR                   V3P
347600940502     C                   CLEAR                   V3A
347700000406     C                   CLEAR                   V3R
347800970211     C                   CLEAR                   S3A
347900970211     C                   CLEAR                   S3P
348000970211     C                   CLEAR                   S3R
348100970211     C                   CLEAR                   S3M
348200970305     C                   CLEAR                   F3F
348300000000      *
348400000000     C                   ENDSR
348500000000      **************************************************************************
348600000000      * Testa l'overflow e stampa su carta
348700000000      **************************************************************************
348800000000     C     SCRIVI        BEGSR
348900940509      * PER TOTALI GENERALI SE GIA' STAMPE PIU' DI 9 RIGHE COMUNQUE
349000940509      * VADO A PAGINA NUOVA
349100940509     C     *IN03         IFEQ      *OFF
349200940509     C     *IN02         ANDEQ     *ON
349300940509     C     $RIGHE        ANDGT     9
349400940509     C                   SETON                                        30
349500940509     C                   ENDIF
349600000000      *
349700940429     C     *IN38         IFEQ      *OFF
349800940509     C*
349900940509     C     *IN30         IFEQ      *ON
350000940509     C     *IN40         ANDEQ     *ON
350100950118     C                   WRITE     LSC2T1
350200950118     C                   WRITE     LSC2T3
350300940509     C                   CLEAR                   $RIGHE
350400940509     C                   SETOFF                                       30
350500940509     C                   ENDIF
350600950118     C                   WRITE     LSC2D1
350700000000      *
350800970226     C  N04$ALMEN        IFEQ      *ZERO
350900000000     C                   Z-ADD     1             $ALMEN
351000000000     C                   ENDIF
351100940328     C                   ENDIF
351200000000      *
351300000000     C                   ENDSR
351400000000      **************************************************************************
351500000000      * Verifica se vi sono dei fogli viaggio con data partenza compresa
351600000000      * nel periodo dell'elaborazione (solo se richiesta la STAMPA)
351700000000      **************************************************************************
351800000000     C     FOGVIA        BEGSR
351900020813     ***C                   OPEN      FNFGV04L
352000020813     C                   OPEN      FNFGV06L
352100000000      *
352200020813     *** * Leggo la vista logica comprendente solo i fogli viaggio aperti (-04L)
352300020813      * Leggo la vista logica comprendente solo i fogli viaggio aperti (-06L)
352400020813     C     SIMfel        setll     FNFGV06L                               99
352500000000     C                   DO        *HIVAL
352600020813     ***C                   READ      FNFGV04L                               99
352700020813     C     SIMfel        reade     FNFGV06L                               99
352800950210     C  N99FGVDTP        IFGE      DS1ADA
352900950210     C     FGVDTP        ANDLE     DS1AAL
353000940502     C                   SETON                                        99
353100950118     C                   WRITE     LSC2E3
353200000000     C                   ENDIF
353300940502     C  N99              ENDDO
353400000000      *
353500020813     ***C                   CLOSE     FNFGV04L
353600020813     C                   CLOSE     FNFGV06L
353700000000     C                   ENDSR
353800000000      **************************************************************************
353900000000      * Aggiornamento del numero colli esaminati, nel file di work
354000950118     OFNWRK000  E            AGGIO
354100000000     O                       WRKNC1
354200000000      **************************************************************************
354300000000      *                                4         5         6         7         8
354400950118** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
354500970807OVRPRTF FNLSC2P       FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.terra
354600950210')
354700000000**
354800950118ESPR.DISTRCORR.
