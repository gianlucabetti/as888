000100950130     H* FNLSB2R *-----------------------------------------------------*
000200930113     H*      PROGRAMMA STAMPA QUADRATURA SPUNTE I.M.P. (BATCH)        *
000300930113     H*---------------------------------------------------------------*
000400930113     H DECEDIT('0,') DATEDIT(*DMY.)
000500950131     FFNFVV01L  IF   E           K DISK
000600950131     FFNFGV01L  IF   E           K DISK
000700000204     FFIBRV01L  IF   E           K DISK
000800000204     FFIBRV09L  IF   E           K DISK
000900000204     F                                     RENAME(FIBRV000:FIBRV003)
001000930113     FTABEL00F  IF   E           K DISK
001100930115     FAZORG01L  IF   E           K DISK
001200021203     F*FNAPD01L  IF   E           K DISK
001201021203     FFIAPD01L  IF   E           K DISK
001300950131     FFNBLP01L  IF   E           K DISK
001400950131     FFNBLT27L  IF   E           K DISK
001500930119     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001600930114     D*
001700930114     D* SCHIERA STAMPA TESTATA
001800930115     D STA             S             66    DIM(7) CTDATA PERRCD(1)
001900930113     D*
002000930113     D* SCHIERA CONTENENTE LE CATEGORIE DEI FOGLI
002100930113     D CAT             S              1    DIM(30)
002200930113     D*
002300020813     D KPJBA         E DS
002400020813     D UT§DSE0F      E DS
002500950131     D CNCR80        E DS
002600020813     D DS7N          E DS
002700950320     D*
002800930113     D PARAM           DS
002900930115     D* NUMERO FOGLIO
003000950131     D  PARNFI                 1      5
003100930114     D* DATA FOGLIO G/M/A
003200950131     D  PARDF2                 6     13  0
003300930114     D* DATA PRESENZA COLLI (G/M/A)
003400950131     D  PARDPC                14     21  0
003500930115     D* RAGIONE SOCIALE
003600950131     D  RSUT                  22     41
003700930115     D* FILIALE ELABORATORE
003800950131     D  PARFLE                42     44  0
003900950320     D* FILIALE GESTIONE FOGLIO
004000950320     D  PARFGS                45     47  0
004100950320     D* DATA FOGLIO AAMMGG
004200950320     D  PARDFV                48     55  0
004300930122     D*
004400930122     D* DS PER CONFRONTO
004500930310     D                 DS
004600930122     D  STAAAS                 1      2  0
004700930122     D  STALNP                 3      5  0
004800930122     D  STANRS                 6      7  0
004900930122     D  STANSP                 8     14  0
005000930122     D  STACON                 1     14  0
005100950131     D WLBDAT          DS
005200950131     D  G02DAT                 1      8  0
005300950131     D  G02INV                 9     16  0
005400950131     D  G02ERR                17     17
005500950131     D  G02TGI                18     22  0
005600021029      * TRUL06DS serve per call a TRUL06R
005700021029     d Trul06Ds      e ds
005800021029     d  L1                     1     90  0 Dim(30)
005801021203
005802021203     D kpdpdr          S                   LIKE(APDpdr)
005803021203     D kpdtip          S                   LIKE(APDtip)
005900930113     C*---------------------------------------------------------------*
006000930113     C* INDICATORI USATI                                              *
006100930113     C*---------------------------------------------------------------*
006200930119     C* 01 - OFF STAMPA 'NESSUN COLLO MANCANTE'
006300930120     C* 02 - ON STAMPA SERIE (+ TRATTINO) SOLO SE >< DA ZERO
006400930119     C* 10 - OFF STAMPA 'COLLO SPUNTATO SU F. N° ...'
006500931126     C* 10 - ON  STAMPA 'LETTORE BARCOD N.RO: ...'
006600930119     C* 11 - OFF STAMPA 'COLLI NON SEQUENZIALI'; ON 'COLLO DAL/ALL'
006700930122     C* 12 - ON STAMPA DETTAGLIO PER SPEDIZIONI >< (AAS+LNP+NRS+NSP)
006800930119     C* 30 - LETTURA  FILE TABEL00F (CARICAMENTO CATEGORIE)
006900000204     C* 31 - LETTURA  FILE FIBRV01L
007000000204     C* 32 - AGGANCIO FILE FIBRV09L
007100950131     C* 33 - DI COMODO
007200950131     C* 35 - AGGANCIO FILE FNFGV01L
007300930115     C* 36 - AGGANCIO FILE AZORG01L
007400950131     C* 37 - AGGANCIO FILE FNBLT27L E FNBLP01L
007500930119     C* 38 - LETTURA  FILE TABEL00F (DECODIFICA PADRONCINO)
007600930119     C* 39 - LETTURA  FILE TABEL00F (DECODIFICA CATEGORIA FOGLIO)
007700021029      * 40 - Lookup
007800930113     C*---------------------------------------------------------------*
007900930113     C*                    MAIN                                       *
008000930113     C* PROGRAMMA DI STAMPA QUADRATURA SPUNTE I.M.P.                  *
008100930113     C*---------------------------------------------------------------*
008200930113     C*
008300930113     C* DEFINIZIONE VARIABILI E KLIST
008400930113     C                   EXSR      DEFVAR
008500930113     C*
008600930113     C* CARICAMENTO TABELLA
008700930113     C                   EXSR      CARTAB
008800930113     C*
008900930115     C* DECODIFICA CAMPI TESTATA
009000930115     C                   EXSR      DECTES
009100930118     C* STAMPA TESTATA
009200930301     C                   EXCEPT    TES
009300930122     C*
009400930122     C                   Z-ADD     *ZEROS        CONFRO           14 0
009500930118     C*
009600930113     C* LETTURA COLLI FOGLIO IMP RICHIESTO
009700930113     C                   Z-ADD     3             KNPG
009800000204     C                   MOVE      *ZEROS        KNFV
009900000204     C                   MOVE      PARNFI        KNFV
010000950320     C                   MOVEL     PARFGS        KFGS3
010100000204     C     KBRV1         SETLL     FIBRV01L
010200000204     C     KBRV1         READE     FIBRV01L                               31
010300930118     C*
010400930118     C* PER NESSUN COLLO TROVATO STAMPA MESSAGGIO
010500930120IF  1C     *IN31         IFEQ      '1'
010600930120     C                   EXCEPT    NUL
010700930120     C                   SETON                                            01
010800930120E   1C                   ENDIF
010900930118     C*
011000930113DO  1C     *IN31         DOWEQ     '0'
011100930114     C*
011200930122     C* STAMPA TESTATA PER SALTO PAGINA
011300930122IF  2C     *INOF         IFEQ      '1'
011400930122     C                   EXCEPT    TES
011500930122     C                   SETOFF                                           OF
011600930122     C*
011700930301     C* STAMPA INTESTAZIONE DETTAGLIO ANCHE PER ROTTURA DI CODICE
011800930122     C                   Z-ADD     *ZEROS        CONFRO
011900930122     C*
012000930122E   2C                   ENDIF
012100930115     C*
012200930114     C                   SETOFF                                           10
012300950131     C                   Z-ADD     BRVLNP        SAVLNP
012400950131     C                   Z-ADD     BRVLNA        SAVLNA
012500950131     C                   Z-ADD     BRVNRS        SAVNRS
012600950131     C                   Z-ADD     BRVNSC        SAVNSC
012700930113     C*
012800930113     C* CERCO LE SPUNTE FRA TUTTE LE CATEGORIE CARICATE
012900930113     C                   Z-ADD     *ZEROS        FLAG
013000930113     C                   Z-ADD     1             I
013100930114     C*
013200930114     C                   MOVEL     *BLANKS       SAVCAT            1
013300020813     C                   movel     *blanks       SAVpog            3 0
013400000204     C                   Z-ADD     *ZEROS        SAVNUM            6 0
013500950320     C                   Z-ADD     *ZEROS        SAVDAT            8 0
013600930114     C*
013700930114     C* ESCO PER FINE CATEGORIE O PER SPUNTA TROVATA
013800930113DO  2C     I             DOWLE     VOLTE
013900930113     C     FLAG          ANDEQ     *ZEROS
014000950131     C*
014100950131     C* DEVO CERCARE LA PRIMA SPUNTA DELLA MIA AREA
014200950131     C                   MOVEL     CAT(I)        KNPG3
014300000204     C     KBRV3         SETLL     FIBRV09L
014400000204     C     KBRV3         READE     FIBRV09L                               32
014500950320    3C     *IN32         DOWEQ     *OFF
014600950320     C     FLAG          ANDEQ     *ZEROS
014700930113     C*
014800000204     C* SPUNTA PRESENTE IN FIBRV09L
014900950320IF  4C     BRVNPG        IFGT      1
015000930113     C*
015100930113     C* SE LA SPUNTA ESISTE NELLA CATEGORIA LETTA CONFRONTO LE DATE
015200930114     C                   MOVEL     CAT(I)        KCAT
015300930114     C                   Z-ADD     BRVNFV        KNUM
015400950131     C                   Z-ADD     BRVFGS        KFGS
015500950131     C     KFVV1         CHAIN     FNFVV01L                           33
015600930113     C   33              Z-ADD     *ZEROS        FVVDFV
015700930113     C*
015800950320     C* DATA SPUNTA = DATA CONTROLLO  --> TUTTO OK
015900930119IF  5C     FVVDFV        IFEQ      DATCOL
016000930113     C                   Z-ADD     1             FLAG              1 0
016100930113     C*
016200930113X   5C                   ELSE
016300930114     C*
016400930113     C* DATE DIVERSE MA SPUNTA TROVATA
016500950320IF  6C     FVVDFV        IFGT      SAVDAT
016600950320     C* IGNORO SE SI TRATTA DI SPUNTA CATEGORIA 3 CON DATA=DATA F.IMP
016700950320    7C     CAT(I)        IFNE      '3'
016800950320     C     CAT(I)        OREQ      '3'
016900950320     C     FVVDFV        ANDNE     PARDFV
017000930114     C                   MOVEL     CAT(I)        SAVCAT
017100020813     C                   z-add     BRVfgs        SAVpog
017200930114     C                   Z-ADD     BRVNFV        SAVNUM
017300930114     C                   Z-ADD     FVVDFV        SAVDAT
017400950320E   7C                   ENDIF
017500950320E   6C                   ENDIF
017600930114E   5C                   ENDIF
017700930114     C*
017800950320X   4C                   ELSE
017900930114     C*
018000930114     C* SE LA SPUNTA ESISTE NELLA CATEGORIA LETTA CONFRONTO LE DATE
018100930114     C*
018200020813     ***C                   Z-ADD     BRVNFV        W0050             5 0
018300020813     ***C     W0050         CHAIN     FNFGV01L                           35
018400020813     C                   z-add     BRVnfv        FGVnfv
018500020813     C                   z-add     BRVfgs        FGVlnp
018600020813     C     K02FGV01      chain     FNFGV01L                           35
018700930114     C   35              Z-ADD     *ZEROS        FGVDFV
018800930114     C*
018900950320     C* DATA SPUNTA = DATA CONTROLLO  --> TUTTO OK
019000930119IF  5C     FGVDFV        IFEQ      DATCOL
019100950131     C                   Z-ADD     1             FLAG
019200930114     C*
019300930114X   5C                   ELSE
019400930114     C*
019500930114     C* DATE DIVERSE MA SPUNTA TROVATA
019600930114IF  6C     FGVDFV        IFGE      SAVDAT
019700930114     C                   MOVEL     CAT(I)        SAVCAT
019800020813     C                   z-add     BRVfgs        SAVpog
019900950131     C                   Z-ADD     BRVNFV        SAVNUM
020000930114     C                   Z-ADD     FGVDFV        SAVDAT
020100930114E   6C                   ENDIF
020200930114E   5C                   ENDIF
020300950320E   4C                   ENDIF
020400930114     C*
020500000204     C     KBRV3         READE     FIBRV09L                               32
020600950320E   3C                   ENDDO
020700950320     C*
020800950320     C                   ADD       1             I
020900930114E   2C                   ENDDO
021000930114     C*
021100930114     C* SPUNTA NON TROVATA IN NESSUNA CATEGORIA: 'COLLO MANCANTE'
021200950320IF  2C     FLAG          IFLT      1
021300950320IF  3C     SAVCAT        IFEQ      *BLANKS
021400020813     C     SAVpog        andeq     *zeros
021500930114     C     SAVNUM        ANDEQ     *ZEROS
021600930114     C     SAVDAT        ANDEQ     *ZEROS
021700930119     C                   SETON                                            10
021800930114     C                   EXSR      STAMPA
021900950320X   3C                   ELSE
022000930114     C*
022100930114     C* SPUNTA TROVATA IN DATA DIVERSA: 'COLLO SPUNTATO SU FOGLIO...'
022200930119     C                   SETOFF                                           10
022300930114     C                   EXSR      STAMPA
022400950320E   3C                   ENDIF
022500950320E   2C                   ENDIF
022600930114     C*
022700930114     C* LEGGO UN ALTRO COLLO DELL'I.M.P.
022800000204     C     KBRV1         READE     FIBRV01L                               31
022900930114E   1C                   ENDDO
023000930113     C*
023100930119     C* SE NON SONO PASSATA DA 'STAMPA' VUOL DIRE CHE E TUTTO OK
023200930119IF  1C     *IN01         IFEQ      '0'
023300930119     C                   EXCEPT    OK
023400930119E   1C                   ENDIF
023500930113     C*
023600930113     C                   SETON                                            LR
023700930113     C*---------------------------------------------------------------*
023800930113     C*                    CARTAB                                     *
023900930113     C* ROUTINE DI CARICAMENTO TABELLA CATEGORIE FOGLI                *
024000930113     C*---------------------------------------------------------------*
024100930113     C     CARTAB        BEGSR
024200930113     C*
024300930113     C                   MOVEL     '7N'          COD
024400930113     C                   Z-ADD     *ZEROS        VOLTE             2 0
024500930113     C                   Z-ADD     *ZEROS        I                 2 0
024600930113     C*
024700930114     C     KT7N          SETLL     TABEL00F
024800930114     C     KT7N          READE     TABEL00F                               30
024900930113DO  1C     *IN30         DOWEQ     '0'
025000930113     C*
025100930113     C* ESCLUDO CATEGORIE ANNULLATE
025200930113IF  2C     TBLFLG        IFEQ      *BLANKS
025300930113     C                   ADD       1             I
025400930113     C                   MOVEL     TBLKEY        CAT(I)
025500930113E   2C                   ENDIF
025600950320     C*
025700930114     C     KT7N          READE     TABEL00F                               30
025800930113E   1C                   ENDDO
025900930113     C*
026000930113     C                   Z-ADD     I             VOLTE
026100930113     C*
026200930113     C                   ENDSR
026300930115     C*---------------------------------------------------------------*
026400930115     C*                    DECTES                                     *
026500930115     C* ROUTINE DI DECODIFICA VARIABILI IN STAMPA  - TESTATA -        *
026600930115     C*---------------------------------------------------------------*
026700930115     C     DECTES        BEGSR
026800930115     C*
026900950131     C                   TIME                    WTIME            14 0
027000950131     C                   MOVE      WTIME         WDATE             8 0
027100950131     C                   MOVEL     WTIME         UTIME             6 0
027200930115     C*
027300930115     C* DECODIFICA FILIALE IN GESTIONE
027400950320     C     PARFGS        CHAIN     AZORG01L                           36
027500930115     C  N36              MOVEL     ORGDES        DESFIL           20
027600930301     C   36              MOVEL     *BLANKS       DESFIL
027700930115     C*
027800930115     C                   ENDSR
027900930114     C*---------------------------------------------------------------*
028000930114     C*                    STAMPA                                     *
028100930115     C* ROUTINE DI DECODIFICA E STAMPA RIGHE DI DETTAGLIO             *
028200930114     C*---------------------------------------------------------------*
028300930114     C     STAMPA        BEGSR
028400930115     C*
028500930119     C                   SETON                                        01
028600930122     C                   SETOFF                                       021112
028700930114     C*
028800930115     C* LETTURA DETTAGLIO BOLLE
028900950131     C     KBLT27        CHAIN     FNBLT27L                           37
029000930115     C*
029100930115IF  1C     *IN37         IFEQ      '0'
029200021029     c     BltLnp        Lookup    L1                                     40
029300021029   1ac                   If        *In40
029400950131     C     KBLP25        CHAIN     FNBLP01L                           37
029500930115     C*
029600930115IF  2C     *IN37         IFEQ      '0'
029700930122     C*
029800930122     C* STAMPO RIGA DETTAGLIO SOLO PER ROTTURA DI CODICE
029900930122     C                   Z-ADD     BLPAAS        STAAAS
030000930122     C                   Z-ADD     BLPLNP        STALNP
030100930122     C                   Z-ADD     BLPNRS        STANRS
030200930122     C                   Z-ADD     BLPNSP        STANSP
030300930122IF  3C     CONFRO        IFNE      STACON
030400930122     C                   SETON                                        12
030500930122     C* SALVO NUOVI VALORI
030600930122     C                   MOVEL     STACON        CONFRO
030700930122X   3C                   ELSE
030800930122     C                   SETOFF                                       12
030900930122E   3C                   ENDIF
031000930120     C*
031100930120     C* STAMPO IL TRATTINO SOLO SE LA SERIE E' >< DA ZERO
031200930122IF  3C     BLPNRS        IFNE      *ZEROS
031300930120     C                   SETON                                        02
031400930122E   3C                   ENDIF
031500930120     C*
031600930120     C* GIRO DATA COLLO IN G/M
031700930120     C                   MOVEL     BLPMGS        STMMGS            4 0
031800930120     C                   MOVEL     STMMGS        MM
031900950131     C                   MOVE      STMMGS        GG                2 0
032000930120     C                   MOVE      MM            STMMGS
032100930120     C                   MOVEL     GG            STMMGS
032200930118     C*
032300930118     C* ACCORCIO RAGIONE SOCIALE MITTENTE
032400930121     C                   MOVEL     BLPRSM        DESMIT           18
032500930115     C*
032600930115     C* SEGNACOLLI SEQUENZIALI
032700930122IF  3C     BLPFNS        IFNE      'S'
032800930115     C* STAMPO 'DAL/AL'
032900930115     C                   SETON                                        11
033000930122X   3C                   ELSE
033100930115     C* STAMPO 'SEGNACOLLI NON SEQUENZIALI'
033200930115     C                   SETOFF                                       11
033300930122E   3C                   ENDIF
033400930115     C*
033500930115     C* DECODIFICA PADRONCINO
033600021203     C*    BLPPDR        CHAIN     FNAPD01L                           38
033601021205     C                   move      blppdr        kpdpdr
033602021203     C                   move      'A'           kpdtip
033603021203     C     kfapd         chain     fiapd01L                           38
033700950131     C  N38              MOVEL     APDRSC        DESPAD           10
033800930115     C   38              MOVEL     *BLANKS       DESPAD
033900930118     C*
034000930118     C* GIRO DATA RITIRO MERCE IN G/M/A
034100950131     C                   Z-ADD     BLPDRT        G02INV
034200950131     C                   Z-ADD     *ZERO         G02DAT
034300950131     C                   MOVEL     '3'           G02ERR
034400950131     C                   CALL      'XSRDA8'
034500950131     C                   PARM                    WLBDAT
034600950131     C                   MOVEL     G02DAT        SAVDRT            6 0
034700950131     C                   MOVE      G02DAT        AA                2 0
034800950131     C                   MOVE      AA            SAVDRT
034900930122     C*
035000930121E   2C                   ENDIF
035100021029   1ac                   Else
035200021029     c                   Eval      *In37 = *On
035300021029   1ac                   EndIf
035400930121E   1C                   ENDIF
035500930115     C*
035600950329     C* STAMPO LA SERIE SOLO SE DIVERSA DA 0
035700950329     C     *IN37         IFEQ      *ON
035800950329     C     SAVNRS        COMP      *ZEROS                             0202
035900950329     C                   END
036000930115     C* DECODIFICA CATEGORIA FOGLIO SPUNTATO
036100930121IF  1C     *IN10         IFEQ      '0'
036200930115     C                   MOVEL     '7N'          COD
036300930119     C                   MOVEL     *BLANKS       KEY
036400930115     C                   MOVEL     SAVCAT        KEY
036500930115     C     KT3D          CHAIN     TABEL00F                           39
036600930115     C  N39              MOVEL     TBLUNI        DS7N
036700930115     C  N39              MOVEL     §7NDES        DESCAT           10
036800930115     C   39              MOVEL     *BLANKS       DESCAT
036900930115     C*
037000930115     C* GIRO DATA DA PORTARE IN STAMPA
037100930115     C                   MOVE      SAVDAT        STMDAT            4 0
037200930120     C                   MOVEL     STMDAT        MM                2 0
037300930120     C                   MOVE      STMDAT        GG
037400930120     C                   MOVE      MM            STMDAT
037500930120     C                   MOVEL     GG            STMDAT
037600930120     C*
037700930121E   1C                   ENDIF
037800950131     C*
037900950131     C                   Z-ADD     BLPAAS        BL§AAS            2 0
038000930115     C*
038100930115     C* STAMPA RIGA DI DETTAGLIO
038200930115     C                   EXCEPT    DET
038300930115     C*
038400930114     C                   ENDSR
038500930113     C*---------------------------------------------------------------*
038600930113     C*                    DEFVAR                                     *
038700930113     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
038800930113     C*---------------------------------------------------------------*
038900930113     C     DEFVAR        BEGSR
039000930113     C*
039100930113     C     *ENTRY        PLIST
039200930113     C                   PARM                    KPJBA
039300930113     C                   MOVEL     KPJBU         PARAM
039400930113     C*
039500930113     C                   Z-ADD     1             CODUT
039600950131     C                   CALL      'X§PARUT'
039700020813     C                   PARM                    UT§DSE0F
039800950131     C                   MOVEL     REC80         CNCR80
039900930113     C*
040000930113     C* LETTURA DI TABEL00F
040100930113     C     KT7N          KLIST
040200950131     C                   KFLD                    CODUT
040300950131     C                   KFLD                    COD
040400930115     C     KT3D          KLIST
040500930115     C                   KFLD                    CODUT
040600930115     C                   KFLD                    COD
040700950131     C                   KFLD                    KEY
040800000204     C* LETTURA DI FIBRV01L
040900930113     C     KBRV1         KLIST
041000950131     C                   KFLD                    KNPG
041100950131     C                   KFLD                    KNFV
041200950320     C                   KFLD                    KFGS3
041300950131     C* LETTURA DI FNFVV01L
041400930114     C     KFVV1         KLIST
041500950131     C                   KFLD                    KCAT
041600950131     C                   KFLD                    KNUM
041700950131     C                   KFLD                    KFGS
041800000204     C* LETTURA DI FIBRV09L
041900930114     C     KBRV3         KLIST
042000950131     C                   KFLD                    KNPG3
042100950131     C                   KFLD                    SAVLNP
042200950131     C                   KFLD                    SAVLNA
042300950131     C                   KFLD                    SAVNRS
042400950131     C                   KFLD                    SAVNSC
042500950320     C                   KFLD                    PARFLE
042600950131     C* LETTURA DI FNBLT27L
042700930115     C     KBLT27        KLIST
042800930115     C                   KFLD                    SAVLNP
042900930115     C                   KFLD                    SAVLNA
043000930115     C                   KFLD                    SAVNRS
043100930115     C                   KFLD                    SAVNSC
043200950131     C* LETTURA DI FNBLP01L
043300930115     C     KBLP25        KLIST
043400930115     C                   KFLD                    BLTAAS
043500930115     C                   KFLD                    BLTLNP
043600930115     C                   KFLD                    BLTNRS
043700930115     C                   KFLD                    BLTNSP
043800020813     C* Lettura di FNFGV01L
043900020813     C     K02FGV01      klist
044000020813     C                   kfld                    FGVnfv
044100020813     C                   kfld                    FGVlnp
044101021203
044102021203     C     Kfapd         klist
044103021203     C                   kfld                    kpdtip
044104021203     C                   kfld                    kpdpdr
044200930119     C*
044300930119     C* GIRO DATA PRESENZA COLLI IN A/M/G
044400950131     C                   Z-ADD     PARDPC        G02DAT
044500950131     C                   Z-ADD     *ZERO         G02INV
044600950131     C                   MOVEL     *BLANKS       G02ERR
044700950131     C                   CALL      'XSRDA8'
044800950131     C                   PARM                    WLBDAT
044900950131     C                   Z-ADD     G02INV        DATCOL
045000950131     C*
045100950131     C     *LIKE         DEFINE    PARDPC        DATCOL
045200950131     C     *LIKE         DEFINE    TBLCOD        COD
045300950131     C     *LIKE         DEFINE    TBLKEY        KEY
045400950131     C     *LIKE         DEFINE    BRVNPG        KNPG
045500950131     C     *LIKE         DEFINE    BRVNFV        KNFV
045600950131     C     *LIKE         DEFINE    FVVNPG        KCAT
045700950131     C     *LIKE         DEFINE    FVVNFV        KNUM
045800950131     C     *LIKE         DEFINE    FVVFGS        KFGS
045900950320     C     *LIKE         DEFINE    FVVFGS        KFGS3
046000950131     C     *LIKE         DEFINE    BRVNPG        KNPG3
046100950131     C     *LIKE         DEFINE    BRVLNP        SAVLNP
046200950131     C     *LIKE         DEFINE    BRVLNA        SAVLNA
046300950131     C     *LIKE         DEFINE    BRVNRS        SAVNRS
046400950131     C     *LIKE         DEFINE    BRVNSC        SAVNSC
046500021029
046600021029      * Carico i p.o. del Terminal
046700021029     c                   Clear                   Trul06Ds
046800021029     c                   Eval      D06Cod = '£1'
046900021029     c                   Movel     Simfel        D06Key
047000021029     c                   Eval      D06Tla = 'L'
047100021029     c                   Movel(p)  Trul06Ds      Kpjbu
047200021029     c                   Call      'TRUL06R'
047300021029     c                   Parm                    Kpjba
047400021029     c                   Movel     Kpjbu         Trul06Ds
047500930115     C*
047600930113     C                   ENDSR
047700930114     C*---------------------------------------------------------------*
047800930114     OQSYSPRT   E            TES              02
047900930114     O                       STA(1)              66
048000930114     O                       STA(2)             132
048100930114     O                       RSUT                20
048200950131     O                       PARFLE              24
048300950131     O                       PARDPC              93 '  /  /    '
048400950131     O                       WDATE              120 '  /  /    '
048500930115     O                       PAGE          Z    132
048600930115     O*---------------------------------------------------------------*
048700930114     O          E            TES         1
048800950131     O                       KNSIF               10
048900950131     O                       KNMUS               21
049000950320     O                       UTIME              119 ' 0:  :  '
049100930115     O*---------------------------------------------------------------*
049200930114     O          E            TES         1
049300930115     O                       STA(3)              66
049400950320     O                       PARFGS              22
049500930114     O                       DESFIL              43
049600950131     O                       PARNFI              60
049700950131     O                       PARDF2              77 '  /  /    '
049800930115     O*---------------------------------------------------------------*
049900930115     O          E            TES         2
050000930115     O                       STA(4)              66
050100930115     O                       STA(5)             132
050200930115     O*---------------------------------------------------------------*
050300930118     O          E            TES         1  2
050400930115     O                       STA(6)              66
050500930115     O                       STA(7)             132
050600930119     O*---------------------------------------------------------------*
050700930115     O          E            DET            1
050800950131     O              N37 12   BL§AAS               2
050900930122     O              N37 12   BLPLNP               6
051000930121     O               37      SAVLNP               6
051100950320     O              N37 02 12BLPNRS               9
051200950329     O               37 02   SAVNRS               9
051300950320     O              N37 02 12                    10 '-'
051400950320     O              N37 12   BLPNSP        Z     17
051500950320     O              N37 12   STMMGS              23 '  /  '
051600950320     O              N37 12   BLPLNA              27
051700950320     O               37      SAVLNA              27
051800950320     O              N37 12   DESMIT              46
051900950320     O              N37 11 12BLPNCD        Z     54
052000950320     O              N37 11 12                    55 '/'
052100950320     O              N37 11 12BLPNCA        Z     62
052200950320     O              N37N11 12                    62 'NON SEQUENZIALI'
052300950320     O              N37 12   BLPPDR        Z     69
052400950320     O              N37 12                       70 '-'
052500950320     O              N37 12   DESPAD              80
052600950320     O              N37 12   SAVDRT              89 '  /  /  '
052700950320     O                       SAVNSC        Z     97
052800950131     O               10                         127 'LETTORE BARCOD N.RO:'
052900950131     O               10      BRVNPS             130
053000950320     O              N10                         109 'SPUNT. F.'
053100000204     O              N10      DESCAT             119
053200000204     O              N10                         120 ' N.'
053300950131     O              N10      SAVNUM        Z    126
053400950131     O              N10                         127 '-'
053500950131     O              N10      STMDAT             132 '  /  '
053600930115     O*---------------------------------------------------------------*
053700930115     O          E            OK          2  2
053800930121     O                                           56 'N E S S U N   C O L L O'
053900930121     O                                           74 '   M A N C A N T E'
054000930118     O*---------------------------------------------------------------*
054100930118     O          E            NUL         2  2
054200930118     O                                           45 'N O N  E S I S T O N O  '
054300930118     O                                           69 'S P U N T E  A  F R O N '
054400930118     O                                           93 'T E  D I  Q U E S T O  F'
054500930118     O                                          111 ' O G L I O  I.M.P.'
054600930114     O*---------------------------------------------------------------*
054700930115     O* TESTATA ELENCO SEGNACOLLI MANCANTI                            *
054800930114     O*---------------------------------------------------------------*
054900950320**
055000930118XXXXXXXXXXXXXXXXXXXX/XXX         ** SEGNACOLLI SPUNTATI IN I.M.P.     1
055100950131MANCANTI IN DATA XX/XX/XXXX **   FNLSB2R    XX/XX/XXXX    PAG.XXXX    2
055200930118FOGLIO I.M.P.: DI  XXX-XXXXXXXXXXXXXXXXXXXX       N.RO XXXXX  DEL     3
055300950131   S P E D I Z I O N E  LIN MITTENTE             SEGNACOLLI      P    4
055400950320ADRONCINO        DATA  SEGNACOLLO                                     5
055500950320AN LNP SR  NUMERO  DATA ARR                        DAL/AL
055600950320                RITIRO  MANCANTE                                      7
