000100950130     H* FNLSB2R *-----------------------------------------------------*
000200930113     H*      PROGRAMMA STAMPA QUADRATURA SPUNTE I.M.P. (BATCH)        *
000300930113     H*---------------------------------------------------------------*
000400930113     H DECEDIT('0,') DATEDIT(*DMY.)
000500950131     FFNFVV01L  IF   E           K DISK
000600950131     FFNFGV01L  IF   E           K DISK
000700070116     FFNBRV01L  IF   E           K DISK
000800070116     FFNBRV09L  IF   E           K DISK
000900070116     F                                     RENAME(FNBRV000:FNBRV003)
001000930113     FTABEL00F  IF   E           K DISK
001100930115     FAZORG01L  IF   E           K DISK
001200021203     F*FNAPD01L  IF   E           K DISK
001300021203     FFIAPD01L  IF   E           K DISK
001400950131     FFNBLP01L  IF   E           K DISK
001500090305     FFiART27L  IF   E           K DISK
001600930119     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001700930114     D*
001800930114     D* SCHIERA STAMPA TESTATA
001900930115     D STA             S             66    DIM(7) CTDATA PERRCD(1)
002000930113     D*
002100930113     D* SCHIERA CONTENENTE LE CATEGORIE DEI FOGLI
002200930113     D CAT             S              1    DIM(30)
002300930113     D*
002400020813     D KPJBA         E DS
002500020813     D UT§DSE0F      E DS
002600950131     D CNCR80        E DS
002700020813     D DS7N          E DS
002800950320     D*
002900930113     D PARAM           DS
003000930115     D* NUMERO FOGLIO
003100950131     D  PARNFI                 1      5
003200930114     D* DATA FOGLIO G/M/A
003300950131     D  PARDF2                 6     13  0
003400930114     D* DATA PRESENZA COLLI (G/M/A)
003500950131     D  PARDPC                14     21  0
003600930115     D* RAGIONE SOCIALE
003700950131     D  RSUT                  22     41
003800930115     D* FILIALE ELABORATORE
003900950131     D  PARFLE                42     44  0
004000950320     D* FILIALE GESTIONE FOGLIO
004100950320     D  PARFGS                45     47  0
004200950320     D* DATA FOGLIO AAMMGG
004300950320     D  PARDFV                48     55  0
004400930122     D*
004500930122     D* DS PER CONFRONTO
004600930310     D                 DS
004700930122     D  STAAAS                 1      2  0
004800930122     D  STALNP                 3      5  0
004900930122     D  STANRS                 6      7  0
005000930122     D  STANSP                 8     14  0
005100930122     D  STACON                 1     14  0
005200950131     D WLBDAT          DS
005300950131     D  G02DAT                 1      8  0
005400950131     D  G02INV                 9     16  0
005500950131     D  G02ERR                17     17
005600950131     D  G02TGI                18     22  0
005700021029      * TRUL06DS serve per call a TRUL06R
005800021029     d Trul06Ds      e ds
005900021029     d  L1                     1     90  0 Dim(30)
006000021203
006100021203     D kpdpdr          S                   LIKE(APDpdr)
006200021203     D kpdtip          S                   LIKE(APDtip)
006300930113     C*---------------------------------------------------------------*
006400930113     C* INDICATORI USATI                                              *
006500930113     C*---------------------------------------------------------------*
006600930119     C* 01 - OFF STAMPA 'NESSUN COLLO MANCANTE'
006700930120     C* 02 - ON STAMPA SERIE (+ TRATTINO) SOLO SE >< DA ZERO
006800930119     C* 10 - OFF STAMPA 'COLLO SPUNTATO SU F. N° ...'
006900931126     C* 10 - ON  STAMPA 'LETTORE BARCOD N.RO: ...'
007000930119     C* 11 - OFF STAMPA 'COLLI NON SEQUENZIALI'; ON 'COLLO DAL/ALL'
007100930122     C* 12 - ON STAMPA DETTAGLIO PER SPEDIZIONI >< (AAS+LNP+NRS+NSP)
007200930119     C* 30 - LETTURA  FILE TABEL00F (CARICAMENTO CATEGORIE)
007300070116     C* 31 - LETTURA  FILE FNBRV01L
007400070116     C* 32 - AGGANCIO FILE FNBRV09L
007500950131     C* 33 - DI COMODO
007600950131     C* 35 - AGGANCIO FILE FNFGV01L
007700930115     C* 36 - AGGANCIO FILE AZORG01L
007800090305     C* 37 - AGGANCIO FILE FIART27L E FNBLP01L
007900930119     C* 38 - LETTURA  FILE TABEL00F (DECODIFICA PADRONCINO)
008000930119     C* 39 - LETTURA  FILE TABEL00F (DECODIFICA CATEGORIA FOGLIO)
008100021029      * 40 - Lookup
008200930113     C*---------------------------------------------------------------*
008300930113     C*                    MAIN                                       *
008400930113     C* PROGRAMMA DI STAMPA QUADRATURA SPUNTE I.M.P.                  *
008500930113     C*---------------------------------------------------------------*
008600930113     C*
008700930113     C* DEFINIZIONE VARIABILI E KLIST
008800930113     C                   EXSR      DEFVAR
008900930113     C*
009000930113     C* CARICAMENTO TABELLA
009100930113     C                   EXSR      CARTAB
009200930113     C*
009300930115     C* DECODIFICA CAMPI TESTATA
009400930115     C                   EXSR      DECTES
009500930118     C* STAMPA TESTATA
009600930301     C                   EXCEPT    TES
009700930122     C*
009800930122     C                   Z-ADD     *ZEROS        CONFRO           14 0
009900930118     C*
010000930113     C* LETTURA COLLI FOGLIO IMP RICHIESTO
010100930113     C                   Z-ADD     3             KNPG
010200000204     C                   MOVE      *ZEROS        KNFV
010300000204     C                   MOVE      PARNFI        KNFV
010400950320     C                   MOVEL     PARFGS        KFGS3
010500070116     C     KBRV1         SETLL     FNBRV01L
010600070116     C     KBRV1         READE     FNBRV01L                               31
010700930118     C*
010800930118     C* PER NESSUN COLLO TROVATO STAMPA MESSAGGIO
010900930120IF  1C     *IN31         IFEQ      '1'
011000930120     C                   EXCEPT    NUL
011100930120     C                   SETON                                            01
011200930120E   1C                   ENDIF
011300930118     C*
011400930113DO  1C     *IN31         DOWEQ     '0'
011500930114     C*
011600930122     C* STAMPA TESTATA PER SALTO PAGINA
011700930122IF  2C     *INOF         IFEQ      '1'
011800930122     C                   EXCEPT    TES
011900930122     C                   SETOFF                                           OF
012000930122     C*
012100930301     C* STAMPA INTESTAZIONE DETTAGLIO ANCHE PER ROTTURA DI CODICE
012200930122     C                   Z-ADD     *ZEROS        CONFRO
012300930122     C*
012400930122E   2C                   ENDIF
012500930115     C*
012600930114     C                   SETOFF                                           10
012700950131     C                   Z-ADD     BRVLNP        SAVLNP
012800950131     C                   Z-ADD     BRVLNA        SAVLNA
012900950131     C                   Z-ADD     BRVNRS        SAVNRS
013000950131     C                   Z-ADD     BRVNSC        SAVNSC
013100930113     C*
013200930113     C* CERCO LE SPUNTE FRA TUTTE LE CATEGORIE CARICATE
013300930113     C                   Z-ADD     *ZEROS        FLAG
013400930113     C                   Z-ADD     1             I
013500930114     C*
013600930114     C                   MOVEL     *BLANKS       SAVCAT            1
013700020813     C                   movel     *blanks       SAVpog            3 0
013800000204     C                   Z-ADD     *ZEROS        SAVNUM            6 0
013900950320     C                   Z-ADD     *ZEROS        SAVDAT            8 0
014000930114     C*
014100930114     C* ESCO PER FINE CATEGORIE O PER SPUNTA TROVATA
014200930113DO  2C     I             DOWLE     VOLTE
014300930113     C     FLAG          ANDEQ     *ZEROS
014400950131     C*
014500950131     C* DEVO CERCARE LA PRIMA SPUNTA DELLA MIA AREA
014600950131     C                   MOVEL     CAT(I)        KNPG3
014700070116     C     KBRV3         SETLL     FNBRV09L
014800070116     C     KBRV3         READE     FNBRV09L                               32
014900950320    3C     *IN32         DOWEQ     *OFF
015000950320     C     FLAG          ANDEQ     *ZEROS
015100930113     C*
015200070116     C* SPUNTA PRESENTE IN FNBRV09L
015300950320IF  4C     BRVNPG        IFGT      1
015400930113     C*
015500930113     C* SE LA SPUNTA ESISTE NELLA CATEGORIA LETTA CONFRONTO LE DATE
015600930114     C                   MOVEL     CAT(I)        KCAT
015700930114     C                   Z-ADD     BRVNFV        KNUM
015800950131     C                   Z-ADD     BRVFGS        KFGS
015900950131     C     KFVV1         CHAIN     FNFVV01L                           33
016000930113     C   33              Z-ADD     *ZEROS        FVVDFV
016100930113     C*
016200950320     C* DATA SPUNTA = DATA CONTROLLO  --> TUTTO OK
016300930119IF  5C     FVVDFV        IFEQ      DATCOL
016400930113     C                   Z-ADD     1             FLAG              1 0
016500930113     C*
016600930113X   5C                   ELSE
016700930114     C*
016800930113     C* DATE DIVERSE MA SPUNTA TROVATA
016900950320IF  6C     FVVDFV        IFGT      SAVDAT
017000950320     C* IGNORO SE SI TRATTA DI SPUNTA CATEGORIA 3 CON DATA=DATA F.IMP
017100950320    7C     CAT(I)        IFNE      '3'
017200950320     C     CAT(I)        OREQ      '3'
017300950320     C     FVVDFV        ANDNE     PARDFV
017400930114     C                   MOVEL     CAT(I)        SAVCAT
017500020813     C                   z-add     BRVfgs        SAVpog
017600930114     C                   Z-ADD     BRVNFV        SAVNUM
017700930114     C                   Z-ADD     FVVDFV        SAVDAT
017800950320E   7C                   ENDIF
017900950320E   6C                   ENDIF
018000930114E   5C                   ENDIF
018100930114     C*
018200950320X   4C                   ELSE
018300930114     C*
018400930114     C* SE LA SPUNTA ESISTE NELLA CATEGORIA LETTA CONFRONTO LE DATE
018500930114     C*
018800020813     C                   z-add     BRVnfv        FGVnfv
018900020813     C                   z-add     BRVfgs        FGVlnp
019000020813     C     K02FGV01      chain     FNFGV01L                           35
019100930114     C   35              Z-ADD     *ZEROS        FGVDFV
019200930114     C*
019300950320     C* DATA SPUNTA = DATA CONTROLLO  --> TUTTO OK
019400930119IF  5C     FGVDFV        IFEQ      DATCOL
019500950131     C                   Z-ADD     1             FLAG
019600930114     C*
019700930114X   5C                   ELSE
019800930114     C*
019900930114     C* DATE DIVERSE MA SPUNTA TROVATA
020000930114IF  6C     FGVDFV        IFGE      SAVDAT
020100930114     C                   MOVEL     CAT(I)        SAVCAT
020200020813     C                   z-add     BRVfgs        SAVpog
020300950131     C                   Z-ADD     BRVNFV        SAVNUM
020400930114     C                   Z-ADD     FGVDFV        SAVDAT
020500930114E   6C                   ENDIF
020600930114E   5C                   ENDIF
020700950320E   4C                   ENDIF
020800930114     C*
020900070116     C     KBRV3         READE     FNBRV09L                               32
021000950320E   3C                   ENDDO
021100950320     C*
021200950320     C                   ADD       1             I
021300930114E   2C                   ENDDO
021400930114     C*
021500930114     C* SPUNTA NON TROVATA IN NESSUNA CATEGORIA: 'COLLO MANCANTE'
021600950320IF  2C     FLAG          IFLT      1
021700950320IF  3C     SAVCAT        IFEQ      *BLANKS
021800020813     C     SAVpog        andeq     *zeros
021900930114     C     SAVNUM        ANDEQ     *ZEROS
022000930114     C     SAVDAT        ANDEQ     *ZEROS
022100930119     C                   SETON                                            10
022200930114     C                   EXSR      STAMPA
022300950320X   3C                   ELSE
022400930114     C*
022500930114     C* SPUNTA TROVATA IN DATA DIVERSA: 'COLLO SPUNTATO SU FOGLIO...'
022600930119     C                   SETOFF                                           10
022700930114     C                   EXSR      STAMPA
022800950320E   3C                   ENDIF
022900950320E   2C                   ENDIF
023000930114     C*
023100930114     C* LEGGO UN ALTRO COLLO DELL'I.M.P.
023200070116     C     KBRV1         READE     FNBRV01L                               31
023300930114E   1C                   ENDDO
023400930113     C*
023500930119     C* SE NON SONO PASSATA DA 'STAMPA' VUOL DIRE CHE E TUTTO OK
023600930119IF  1C     *IN01         IFEQ      '0'
023700930119     C                   EXCEPT    OK
023800930119E   1C                   ENDIF
023900930113     C*
024000930113     C                   SETON                                            LR
024100930113     C*---------------------------------------------------------------*
024200930113     C*                    CARTAB                                     *
024300930113     C* ROUTINE DI CARICAMENTO TABELLA CATEGORIE FOGLI                *
024400930113     C*---------------------------------------------------------------*
024500930113     C     CARTAB        BEGSR
024600930113     C*
024700930113     C                   MOVEL     '7N'          COD
024800930113     C                   Z-ADD     *ZEROS        VOLTE             2 0
024900930113     C                   Z-ADD     *ZEROS        I                 2 0
025000930113     C*
025100930114     C     KT7N          SETLL     TABEL00F
025200930114     C     KT7N          READE     TABEL00F                               30
025300930113DO  1C     *IN30         DOWEQ     '0'
025400930113     C*
025500930113     C* ESCLUDO CATEGORIE ANNULLATE
025600930113IF  2C     TBLFLG        IFEQ      *BLANKS
025700930113     C                   ADD       1             I
025800930113     C                   MOVEL     TBLKEY        CAT(I)
025900930113E   2C                   ENDIF
026000950320     C*
026100930114     C     KT7N          READE     TABEL00F                               30
026200930113E   1C                   ENDDO
026300930113     C*
026400930113     C                   Z-ADD     I             VOLTE
026500930113     C*
026600930113     C                   ENDSR
026700930115     C*---------------------------------------------------------------*
026800930115     C*                    DECTES                                     *
026900930115     C* ROUTINE DI DECODIFICA VARIABILI IN STAMPA  - TESTATA -        *
027000930115     C*---------------------------------------------------------------*
027100930115     C     DECTES        BEGSR
027200930115     C*
027300950131     C                   TIME                    WTIME            14 0
027400950131     C                   MOVE      WTIME         WDATE             8 0
027500950131     C                   MOVEL     WTIME         UTIME             6 0
027600930115     C*
027700930115     C* DECODIFICA FILIALE IN GESTIONE
027800950320     C     PARFGS        CHAIN     AZORG01L                           36
027900930115     C  N36              MOVEL     ORGDES        DESFIL           20
028000930301     C   36              MOVEL     *BLANKS       DESFIL
028100930115     C*
028200930115     C                   ENDSR
028300930114     C*---------------------------------------------------------------*
028400930114     C*                    STAMPA                                     *
028500930115     C* ROUTINE DI DECODIFICA E STAMPA RIGHE DI DETTAGLIO             *
028600930114     C*---------------------------------------------------------------*
028700930114     C     STAMPA        BEGSR
028800930115     C*
028900930119     C                   SETON                                        01
029000930122     C                   SETOFF                                       021112
029100930114     C*
029200930115     C* LETTURA DETTAGLIO BOLLE
029300090305     C     KART27        CHAIN     FIART27L                           37
029400930115     C*
029500930115IF  1C     *IN37         IFEQ      '0'
029600090305     c     ARTLnp        Lookup    L1                                     40
029700021029   1ac                   If        *In40
029800950131     C     KBLP25        CHAIN     FNBLP01L                           37
029900930115     C*
030000930115IF  2C     *IN37         IFEQ      '0'
030100930122     C*
030200930122     C* STAMPO RIGA DETTAGLIO SOLO PER ROTTURA DI CODICE
030201090305     C                   Z-ADD     BLPAAS        BL§AAS            2 0
030300930122     C                   Z-ADD     BLPAAS        STAAAS
030400930122     C                   Z-ADD     BLPLNP        STALNP
030500930122     C                   Z-ADD     BLPNRS        STANRS
030600930122     C                   Z-ADD     BLPNSP        STANSP
030700930122IF  3C     CONFRO        IFNE      STACON
030800930122     C                   SETON                                        12
030900930122     C* SALVO NUOVI VALORI
031000930122     C                   MOVEL     STACON        CONFRO
031100930122X   3C                   ELSE
031200930122     C                   SETOFF                                       12
031300930122E   3C                   ENDIF
031400930120     C*
031500930120     C* STAMPO IL TRATTINO SOLO SE LA SERIE E' >< DA ZERO
031600930122IF  3C     BLPNRS        IFNE      *ZEROS
031700930120     C                   SETON                                        02
031800930122E   3C                   ENDIF
031900930120     C*
032000930120     C* GIRO DATA COLLO IN G/M
032100930120     C                   MOVEL     BLPMGS        STMMGS            4 0
032200930120     C                   MOVEL     STMMGS        MM
032300950131     C                   MOVE      STMMGS        GG                2 0
032400930120     C                   MOVE      MM            STMMGS
032500930120     C                   MOVEL     GG            STMMGS
032600930118     C*
032700930118     C* ACCORCIO RAGIONE SOCIALE MITTENTE
032800930121     C                   MOVEL     BLPRSM        DESMIT           18
032900930115     C*
033000930115     C* SEGNACOLLI SEQUENZIALI
033100930122IF  3C     BLPFNS        IFNE      'S'
033200930115     C* STAMPO 'DAL/AL'
033300930115     C                   SETON                                        11
033400930122X   3C                   ELSE
033500930115     C* STAMPO 'SEGNACOLLI NON SEQUENZIALI'
033600930115     C                   SETOFF                                       11
033700930122E   3C                   ENDIF
033800930115     C*
033900930115     C* DECODIFICA PADRONCINO
034000021203     C*    BLPPDR        CHAIN     FNAPD01L                           38
034100021205     C                   move      blppdr        kpdpdr
034200021203     C                   move      'A'           kpdtip
034300021203     C     kfapd         chain     fiapd01L                           38
034400950131     C  N38              MOVEL     APDRSC        DESPAD           10
034500930115     C   38              MOVEL     *BLANKS       DESPAD
034600930118     C*
034700930118     C* GIRO DATA RITIRO MERCE IN G/M/A
034800950131     C                   Z-ADD     BLPDRT        G02INV
034900950131     C                   Z-ADD     *ZERO         G02DAT
035000950131     C                   MOVEL     '3'           G02ERR
035100950131     C                   CALL      'XSRDA8'
035200950131     C                   PARM                    WLBDAT
035300950131     C                   MOVEL     G02DAT        SAVDRT            6 0
035400950131     C                   MOVE      G02DAT        AA                2 0
035500950131     C                   MOVE      AA            SAVDRT
035600930122     C*
035700930121E   2C                   ENDIF
035800021029   1ac                   Else
035900021029     c                   Eval      *In37 = *On
036000021029   1ac                   EndIf
036100930121E   1C                   ENDIF
036200930115     C*
036300950329     C* STAMPO LA SERIE SOLO SE DIVERSA DA 0
036400950329     C     *IN37         IFEQ      *ON
036500950329     C     SAVNRS        COMP      *ZEROS                             0202
036600950329     C                   END
036700930115     C* DECODIFICA CATEGORIA FOGLIO SPUNTATO
036800930121IF  1C     *IN10         IFEQ      '0'
036900930115     C                   MOVEL     '7N'          COD
037000930119     C                   MOVEL     *BLANKS       KEY
037100930115     C                   MOVEL     SAVCAT        KEY
037200930115     C     KT3D          CHAIN     TABEL00F                           39
037300930115     C  N39              MOVEL     TBLUNI        DS7N
037400930115     C  N39              MOVEL     §7NDES        DESCAT           10
037500930115     C   39              MOVEL     *BLANKS       DESCAT
037600930115     C*
037700930115     C* GIRO DATA DA PORTARE IN STAMPA
037800930115     C                   MOVE      SAVDAT        STMDAT            4 0
037900930120     C                   MOVEL     STMDAT        MM                2 0
038000930120     C                   MOVE      STMDAT        GG
038100930120     C                   MOVE      MM            STMDAT
038200930120     C                   MOVEL     GG            STMDAT
038300930120     C*
038400930121E   1C                   ENDIF
038500950131     C*
038700930115     C*
038800930115     C* STAMPA RIGA DI DETTAGLIO
038900930115     C                   EXCEPT    DET
039000930115     C*
039100930114     C                   ENDSR
039200930113     C*---------------------------------------------------------------*
039300930113     C*                    DEFVAR                                     *
039400930113     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
039500930113     C*---------------------------------------------------------------*
039600930113     C     DEFVAR        BEGSR
039700930113     C*
039800930113     C     *ENTRY        PLIST
039900930113     C                   PARM                    KPJBA
040000930113     C                   MOVEL     KPJBU         PARAM
040100930113     C*
040200930113     C                   Z-ADD     1             CODUT
040300950131     C                   CALL      'X§PARUT'
040400020813     C                   PARM                    UT§DSE0F
040500950131     C                   MOVEL     REC80         CNCR80
040600930113     C*
040700930113     C* LETTURA DI TABEL00F
040800930113     C     KT7N          KLIST
040900950131     C                   KFLD                    CODUT
041000950131     C                   KFLD                    COD
041100930115     C     KT3D          KLIST
041200930115     C                   KFLD                    CODUT
041300930115     C                   KFLD                    COD
041400950131     C                   KFLD                    KEY
041500070116     C* LETTURA DI FNBRV01L
041600930113     C     KBRV1         KLIST
041700950131     C                   KFLD                    KNPG
041800950131     C                   KFLD                    KNFV
041900950320     C                   KFLD                    KFGS3
042000950131     C* LETTURA DI FNFVV01L
042100930114     C     KFVV1         KLIST
042200950131     C                   KFLD                    KCAT
042300950131     C                   KFLD                    KNUM
042400950131     C                   KFLD                    KFGS
042500070116     C* LETTURA DI FNBRV09L
042600930114     C     KBRV3         KLIST
042700950131     C                   KFLD                    KNPG3
042800950131     C                   KFLD                    SAVLNP
042900950131     C                   KFLD                    SAVLNA
043000950131     C                   KFLD                    SAVNRS
043100950131     C                   KFLD                    SAVNSC
043200950320     C                   KFLD                    PARFLE
043300090305     C* LETTURA DI FIART27L
043400090305     C     KART27        KLIST
043500930115     C                   KFLD                    SAVLNP
043600930115     C                   KFLD                    SAVLNA
043700930115     C                   KFLD                    SAVNRS
043800930115     C                   KFLD                    SAVNSC
043900950131     C* LETTURA DI FNBLP01L
044000930115     C     KBLP25        KLIST
044100090305     C                   KFLD                    ARTAAS
044200090305     C                   KFLD                    ARTLNP
044300090305     C                   KFLD                    ARTNRS
044400090305     C                   KFLD                    ARTNSP
044500020813     C* Lettura di FNFGV01L
044600020813     C     K02FGV01      klist
044700020813     C                   kfld                    FGVnfv
044800020813     C                   kfld                    FGVlnp
044900021203
045000021203     C     Kfapd         klist
045100021203     C                   kfld                    kpdtip
045200021203     C                   kfld                    kpdpdr
045300930119     C*
045400930119     C* GIRO DATA PRESENZA COLLI IN A/M/G
045500950131     C                   Z-ADD     PARDPC        G02DAT
045600950131     C                   Z-ADD     *ZERO         G02INV
045700950131     C                   MOVEL     *BLANKS       G02ERR
045800950131     C                   CALL      'XSRDA8'
045900950131     C                   PARM                    WLBDAT
046000950131     C                   Z-ADD     G02INV        DATCOL
046100950131     C*
046200950131     C     *LIKE         DEFINE    PARDPC        DATCOL
046300950131     C     *LIKE         DEFINE    TBLCOD        COD
046400950131     C     *LIKE         DEFINE    TBLKEY        KEY
046500950131     C     *LIKE         DEFINE    BRVNPG        KNPG
046600950131     C     *LIKE         DEFINE    BRVNFV        KNFV
046700950131     C     *LIKE         DEFINE    FVVNPG        KCAT
046800950131     C     *LIKE         DEFINE    FVVNFV        KNUM
046900950131     C     *LIKE         DEFINE    FVVFGS        KFGS
047000950320     C     *LIKE         DEFINE    FVVFGS        KFGS3
047100950131     C     *LIKE         DEFINE    BRVNPG        KNPG3
047200950131     C     *LIKE         DEFINE    BRVLNP        SAVLNP
047300950131     C     *LIKE         DEFINE    BRVLNA        SAVLNA
047400950131     C     *LIKE         DEFINE    BRVNRS        SAVNRS
047500950131     C     *LIKE         DEFINE    BRVNSC        SAVNSC
047600021029
047700021029      * Carico i p.o. del Terminal
047800021029     c                   Clear                   Trul06Ds
047900021029     c                   Eval      D06Cod = '£1'
048000021029     c                   Movel     Simfel        D06Key
048100021029     c                   Eval      D06Tla = 'L'
048200021029     c                   Movel(p)  Trul06Ds      Kpjbu
048300021029     c                   Call      'TRUL06R'
048400021029     c                   Parm                    Kpjba
048500021029     c                   Movel     Kpjbu         Trul06Ds
048600930115     C*
048700930113     C                   ENDSR
048800930114     C*---------------------------------------------------------------*
048900930114     OQSYSPRT   E            TES              02
049000930114     O                       STA(1)              66
049100930114     O                       STA(2)             132
049200930114     O                       RSUT                20
049400950131     O                       PARDPC              93 '  /  /    '
049500950131     O                       WDATE              120 '  /  /    '
049600930115     O                       PAGE          Z    132
049700930115     O*---------------------------------------------------------------*
049800930114     O          E            TES         1
049900950131     O                       KNSIF               10
050000950131     O                       KNMUS               21
050100950320     O                       UTIME              119 ' 0:  :  '
050200930115     O*---------------------------------------------------------------*
050300930114     O          E            TES         1
050400930115     O                       STA(3)              66
050500950320     O                       PARFGS              22
050600930114     O                       DESFIL              43
050700950131     O                       PARNFI              60
050800950131     O                       PARDF2              77 '  /  /    '
050900930115     O*---------------------------------------------------------------*
051000930115     O          E            TES         2
051100930115     O                       STA(4)              66
051200930115     O                       STA(5)             132
051300930115     O*---------------------------------------------------------------*
051400930118     O          E            TES         1  2
051500930115     O                       STA(6)              66
051600930115     O                       STA(7)             132
051700930119     O*---------------------------------------------------------------*
051800930115     O          E            DET            1
051900950131     O              N37 12   BL§AAS               2
052000930122     O              N37 12   BLPLNP               6
052100930121     O               37      SAVLNP               6
052200950320     O              N37 02 12BLPNRS               9
052300950329     O               37 02   SAVNRS               9
052400950320     O              N37 02 12                    10 '-'
052500950320     O              N37 12   BLPNSP        Z     17
052600950320     O              N37 12   STMMGS              23 '  /  '
052700950320     O              N37 12   BLPLNA              27
052800950320     O               37      SAVLNA              27
052900950320     O              N37 12   DESMIT              46
053000950320     O              N37 11 12BLPNCD        Z     54
053100950320     O              N37 11 12                    55 '/'
053200950320     O              N37 11 12BLPNCA        Z     62
053300950320     O              N37N11 12                    62 'NON SEQUENZIALI'
053400950320     O              N37 12   BLPPDR        Z     69
053500950320     O              N37 12                       70 '-'
053600950320     O              N37 12   DESPAD              80
053700950320     O              N37 12   SAVDRT              89 '  /  /  '
053800950320     O                       SAVNSC        Z     97
053900950131     O               10                         127 'LETTORE BARCOD N.RO:'
054000950131     O               10      BRVNPS             130
054100950320     O              N10                         109 'SPUNT. F.'
054200000204     O              N10      DESCAT             119
054300000204     O              N10                         120 ' N.'
054400950131     O              N10      SAVNUM        Z    126
054500950131     O              N10                         127 '-'
054600950131     O              N10      STMDAT             132 '  /  '
054700930115     O*---------------------------------------------------------------*
054800930115     O          E            OK          2  2
054900930121     O                                           56 'N E S S U N   C O L L O'
055000930121     O                                           74 '   M A N C A N T E'
055100930118     O*---------------------------------------------------------------*
055200930118     O          E            NUL         2  2
055300930118     O                                           45 'N O N  E S I S T O N O  '
055400930118     O                                           69 'S P U N T E  A  F R O N '
055500930118     O                                           93 'T E  D I  Q U E S T O  F'
055600930118     O                                          111 ' O G L I O  I.M.P.'
055700930114     O*---------------------------------------------------------------*
055800930115     O* TESTATA ELENCO SEGNACOLLI MANCANTI                            *
055900930114     O*---------------------------------------------------------------*
056000950320**
056100930118XXXXXXXXXXXXXXXXXXXX/XXX         ** SEGNACOLLI SPUNTATI IN I.M.P.     1
056200950131MANCANTI IN DATA XX/XX/XXXX **   FNLSB2R    XX/XX/XXXX    PAG.XXXX    2
056300930118FOGLIO I.M.P.: DI  XXX-XXXXXXXXXXXXXXXXXXXX       N.RO XXXXX  DEL     3
056400950131   S P E D I Z I O N E  LIN MITTENTE             SEGNACOLLI      P    4
056500950320ADRONCINO        DATA  SEGNACOLLO                                     5
056600950320AN LNP SR  NUMERO  DATA ARR                        DAL/AL
056700950320                RITIRO  MANCANTE                                      7
