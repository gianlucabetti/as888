000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000300931209     F*  KL        RITORNO                                           *
000400940928     F*  KC        FINE LAVORO                                       *
000500891030     F*--------------------------------------------------------------*
000600940927     FTRTB69D   CF   E             WORKSTN
000700940929     F                                     SFILE(TB69S02:NRR)
000800940418     F*----------
000900940928     FTABEL00F  IF   E           K DISK
001000940418     D*--------------------------------------------------------------*
001100940418     D*   SCHIERE                                                    *
001200940701     D*--------------------------------------------------------------*
001300940930     D ERR             S             70    DIM(1) CTDATA PERRCD(1)
001400940928     D COST            S             16    DIM(1) CTDATA PERRCD(1)
001500960108     D CODP            S              2    DIM(30)
001600960108     D C7Q             S              2    DIM(100)
001700960108     D D7Q             S             89    DIM(100)
001800940928     D*--------------------------------------------------------------*
001900940928     D*  DS                                                          *
002000940928     D*--------------------------------------------------------------*
002100940929     D DSTB69        E DS                  EXTNAME(DS7PQRS)
002200080227     D DS7V          E DS
002300050519     D DS7U          E DS
002400940928     D DS7Q          E DS
002500940928     D  TIP                   78     82
002600940928     D  DSP                   83     87
002700900608      *
002800891030     D KPJBA         E DS
002900911118      *
003000971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
003100911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003200960108      *  Codice x posizionamento su tabella 7V
003300960108     D                 DS
003400960108     D  WTPPAR                 1      1
003500960108     D  WCDPAR                 2      3
003600960109     D  WTPELE                 4      4
003700960109     D  WKEY01                 1      4
003800960109     D  WPROGG                 5      8  0
003900960110     D  WKEY02                 1      8
004000960109      *  SComposizione KEY di TABEL x 7V
004100960109     D                 DS
004200960109     D  WSVKEY                 1      4
004300960109     D  TBLKEY                 1      8
004400940228      *
004500940228     D                SDS
004600940927     D  V1CPGM                 1     10
004700931209     C*--------------------------------------------------------------*
004800931209     C*    C I C L O    P R I N C I P A L E                          *
004900931209     C*--------------------------------------------------------------*
005000931209     C*  Loop gestione varie videate fino a rich. fine elaboraz.
005100940927     C     WFINE         DOWNE     'S'
005200940927     C*
005300940928     C     WTPVID        CASEQ     '1'           GESD01
005400940928     C     WTPVID        CASEQ     '2'           GESD02
005500940928     C     WTPVID        CASEQ     '3'           GESD03
005600931209     C                   END
005700931209     C*
005800931209     C                   END
005900931209     C* Fine programma
006000931209     C     FINE          TAG
006100940928     C                   MOVEL     DSTB69        KPJBU
006200931209     C                   SETON                                        LR
006300940928     C*---------------------------------------------------*
006400940928     C*  GESD01: Gestione prima videata                   *
006500940928     C*---------------------------------------------------*
006600940928     C     GESD01        BEGSR
006700940928     C*---------------------------------------------------*
006800931209     C*  Emissione videata
006900940929     C                   MOVEL     *BLANKS       V1DFUN
007000940929     C                   WRITE     TB69T01
007100940928     C                   EXFMT     TB69D01
007200940928     C                   SETOFF                                       4028
007300940418     C                   MOVEL     *BLANKS       $MSG
007400940928     C*---------------------------------------------------*
007500931209     C*  Fine elaborazione
007600931209     C     *INKC         IFEQ      '1'
007700940929     C                   MOVEL     'S'           WFINE
007800931209     C                   GOTO      FINVD1
007900931209     C                   END
008000940928     C*---------------------------------------------------*
008100940928     C*   Se richiesta ricerca richiamo subfile
008200940928     C     '?'           SCAN      V1CCOD                                 32
008300940928     C     *IN32         IFEQ      '1'
008400940929     C                   EXSR      INZ02
008500940929     C                   GOTO      FINVD1
008600940928     C                   END
008700940928     C*---------------------------------------------------*
008800940928     C*   Se immesso codice eseguo caricamento dati
008900940928     C     V1CCOD        IFNE      *BLANKS
009000940929     C                   MOVEL     V1CCOD        DS7COD
009100940929     C                   MOVEL     '1'           DS7OPZ
009200940928     C                   MOVEL     DSTB69        KPJBU
009300960109     C                   CALL      'TRTB69R0'
009400960110     C                   PARM                    KPJBA
009500960109     C                   MOVEL     KPJBU         DSTB69
009600940929     C     DS7F03        IFEQ      '1'
009700940929     C                   MOVEL     'S'           WFINE
009800940929     C                   ELSE
009900960111     C                   EXSR      CAR7Q
010000940929     C                   EXSR      INZ01
010100940929     C                   END
010200940928     C                   ELSE
010300940928     C                   SETON                                        2840
010400940928     C                   MOVEL     ERR(1)        $MSG
010500940928     C                   END
010600931209     C*--------------------------------------------------------------*
010700940928     C     FINVD1        ENDSR
010800940928     C*--------------------------------------------------------------*
010900940928     C*  GESD02: Gestione prima videata                              *
011000940928     C*--------------------------------------------------------------*
011100940928     C     GESD02        BEGSR
011200940928     C*----------------------------------------------------*
011300940928     C*  Emissione videata
011400940929     C  N28              Z-ADD     1             DSPNRR
011500940928     C                   SETON                                        20
011600940928     C                   WRITE     TB69Z02
011700940928     C                   EXFMT     TB69C02
011800940928     C                   SETOFF                                       2840
011900940928     C                   MOVEL     *BLANKS       $MSG
012000940928     C*----------------------------------------------------*
012100940928     C*  Fine elaborazione
012200940928     C     *INKC         IFEQ      '1'
012300940929     C                   MOVEL     'S'           WFINE
012400940929     C                   MOVEL     '1'           DS7F03
012500940928     C                   GOTO      FINVD2
012600940928     C                   END
012700940928     C*----------------------------------------------------*
012800940928     C*  Ritorno prima videata
012900940928     C     *INKL         IFEQ      '1'
013000960112     C     DS7RIC        IFEQ      ' '
013100940928     C                   EXSR      INZ01
013200960112     C                   ELSE
013300960112     C                   MOVEL     'S'           WFINE
013400960112     C                   END
013500940928     C                   GOTO      FINVD2
013600940928     C                   END
013700940928     C*----------------------------------------------------*
013800940928     C*  controlli
013900940928     C                   EXSR      CTR02
014000940929     C*----------------------------------------------------*
014100940928     C     FINVD2        ENDSR
014200940928     C*----------------------------------------------------*
014300940928     C*  GESD03: Gestione videata subfile vuoto            *
014400940928     C*----------------------------------------------------*
014500940928     C     GESD03        BEGSR
014600940928     C*----------------------------------------------------*
014700940928     C*  Emissione videata
014800940929     C                   SETOFF                                       20
014900940928     C                   WRITE     TB69D02
015000940928     C                   EXFMT     TB69C02
015100940928     C                   SETOFF                                       2840
015200940928     C*----------------------------------------------------*
015300940928     C*  Fine elaborazione
015400940928     C     *INKC         IFEQ      '1'
015500940929     C                   MOVEL     '1'           DS7F03
015600940929     C                   MOVEL     'S'           WFINE
015700940928     C                   GOTO      FINVD3
015800940928     C                   END
015900940928     C*----------------------------------------------------*
016000940929     C     DS7RIC        IFEQ      ' '
016100940928     C                   EXSR      INZ01
016200940929     C                   ELSE
016300940929     C                   MOVEL     'S'           WFINE
016400940929     C                   END
016500940928     C*----------------------------------------------------*
016600940928     C     FINVD3        ENDSR
016700940928     C*----------------------------------------------------*
016800940928     C*  INZ01   Gestione inizializzazione prima videata   *
016900940928     C*----------------------------------------------------*
017000940927     C     INZ01         BEGSR
017100940419     C*  Inizializzo prima videata impostando rich. ricerca tabella
017200940929     C                   MOVEL     '1'           WTPVID
017300940928     C                   MOVEL     '? '          V1CCOD
017400940419     C                   MOVEL     *BLANKS       $MSG
017500940928     C                   SETOFF                                       2840
017600940419     C*
017700940419     C                   ENDSR
017800940928     C*----------------------------------------------------*
017900940928     C*  INZ02 : Gestione inizializz. Seconda videata      *
018000940928     C*----------------------------------------------------*
018100940928     C     INZ02         BEGSR
018200940701     C*----------------------------------------------------*
018300940930     C                   SETOFF                                       28
018400940418     C                   MOVEL     *BLANKS       $MSG
018500940928     C*----------------------------------------------------*
018600940929     C     DS7GES        IFEQ      'V'
018700940929     C                   MOVEL     COST(1)       V1DFUN
018800940928     C                   ELSE
018900940929     C                   MOVEL     *BLANKS       V1DFUN
019000940928     C                   END
019100940927     C*----------------------------------------------------*
019200940927     C*  Inizializzazione subfile
019300940927     C                   SETOFF                                       2021
019400940927     C                   WRITE     TB69C02
019500940929     C                   Z-ADD     0             NRR               4 0
019600940927     C                   SETON                                        21
019700940927     C*----------------------------------------------------*
019800940927     C*  Caricamento subfile
019900960109     C                   Z-ADD     1             X
020000960109     C                   MOVEL     '7V'          KCOD
020100940929     C                   MOVEL     *BLANKS       V2CSEL
020200960109     C                   DO        Z             X
020300960109      *  Costruisco chiave per posizionamento su tabel
020400960109     C                   MOVEL     'Q'           WTPPAR
020500960109     C                   MOVEL     C7Q(X)        WCDPAR
020600960109     C                   MOVEL     'V'           WTPELE
020700960109     C                   Z-ADD     0             WPROGG
020800960110     C                   MOVEL     WKEY02        KKEY
020900960109     C                   SETOFF                                       01
021000141201     C                   SETOFF                                       03
021100960109     C     KTAB2         SETLL     TABEL00F
021200960109     C     KTAB1         READE     TABEL00F                               31
021300960109     C     *IN31         IFEQ      '0'
021400960109     C     WSVKEY        ANDEQ     WKEY01
021500960109     C*----------------------------------------------------*
021600960109     C*  Se immesso codice cliente prima di caricare particol.
021700960109     C*  controllo se il cliente è abilitato
021800960109     C                   SETON                                        32
021900960109     C                   MOVEL     D7Q(X)        DS7Q
022000960109     C     DS7KSC        IFNE      0
022100960109     C     §7QABC        ANDEQ     'S'
022200960110     C     C7Q(X)        LOOKUP    CODP                                   32
022300960109     C                   END
022400960109     C*----------------------------------------------------*
022500960109     C     *IN32         IFEQ      '1'
022600960110     C                   MOVEL     C7Q(X)        V2CCOD
022700960109     C                   MOVEL     §7QELI        V2CELI
022800960109     C                   MOVEL     §7QINS        V2CINS
022900940927     C     *IN31         DOWEQ     '0'
023000960109     C     WSVKEY        ANDEQ     WKEY01
023100960109     C     TBLUNI        IFNE      *BLANKS
023200960109     C                   MOVEL     TBLUNI        V2DCOD
023300940928     C                   ADD       1             NRR
023400141201     C                   SETON                                        03
023500940928     C                   WRITE     TB69S02
023600960109     C                   END
023700960109     C     KTAB1         READE     TABEL00F                               31
023800141201     C   03              SETON                                        01
023900960109     C                   END
024000940928     C*----------------------------------------------------*
024100960109     C                   END
024200940418     C*----------------------------------------------------*
024300960109     C                   END
024400960109     C*----------------------------------------------------*
024500960109     C                   END
024600960109     C*----------------------------------------------------*
024700940928     C*  Controllo se devo emettere subfile vuoto
024800940928     C     NRR           IFEQ      0
024900940928     C                   MOVEL     '3'           WTPVID
025000940928     C                   ELSE
025100940929     C                   MOVEL     '2'           WTPVID            1
025200940928     C                   END
025300940928     C*----------------------------------------------------*
025400940928     C                   ENDSR
025500940928     C*----------------------------------------------------*
025600940928     C*  CTR02 : Gestione inizial. seconda videata         *
025700940928     C*----------------------------------------------------*
025800940928     C     CTR02         BEGSR
025900940928     C*----------------------------------------------------*
026000940929     C                   MOVEL     'N'           WSELEZ            1
026100960111     C                   SETOFF                                       01
026200940928     C                   READC     TB69S02                                31
026300940928     C     *IN31         DOWEQ     '0'
026400940928     C     *IN28         ANDEQ     '0'
026500940928     C                   SETOFF                                       40
026600940928     C     V2CSEL        IFEQ      '5'
026700960108     C                   MOVEL     V2CSEL        DS7OPZ
026800960108     C                   MOVEL     V2CCOD        DS7COD
026900960108     C                   MOVEL     DSTB69        KPJBU
027000960109     C                   MOVEL     'S'           WSELEZ
027100960108     C     DS7GES        IFNE      'C'
027200960108     C                   CALL      'TRTB69R0'
027300960108     C                   PARM                    KPJBA
027400960109     C                   MOVEL     KPJBU         DSTB69
027500960109     C                   END
027600940929     C     DS7F03        IFEQ      '1'
027700940929     C                   MOVEL     'S'           WFINE
027800940929     C                   GOTO      FINCT2
027900940929     C                   END
028000940928     C                   END
028100960111     C                   EXSR      CAR7Q
028200940928     C     V2CSEL        IFEQ      '1'
028300941107     C                   MOVEL     V2CSEL        DS7OPZ
028400940929     C     DS7GES        IFEQ      'V'
028500940930     C                   MOVEL     V2CCOD        DS7COD
028600941107     C                   MOVEL     V2CELI        DS7ELI
028700941107     C                   MOVEL     V2CINS        DS7INS
028800940930     C                   MOVEL     'S'           WFINE
028900940928     C                   ELSE
029000940929     C                   MOVEL     V2CCOD        DS7COD
029100940928     C                   MOVEL     DSTB69        KPJBU
029200960109     C                   MOVEL     'S'           WSELEZ
029300960108     C                   CALL      'TRTB69R0'
029400960108     C                   PARM                    KPJBA
029500960109     C                   MOVEL     KPJBU         DSTB69
029600940929     C     DS7F03        IFEQ      '1'
029700940929     C                   MOVEL     'S'           WFINE
029800940929     C                   GOTO      FINCT2
029900940929     C                   END
030000960111     C                   EXSR      CAR7Q
030100940928     C                   END
030200940929     C                   END
030300940928     C*  Aggiorno Sufile
030400940928     C                   SETON                                        22
030500940928     C     *IN40         IFEQ      '0'
030600940929     C                   MOVEL     ' '           V2CSEL
030700940928     C                   END
030800940928     C                   UPDATE    TB69S02
030900940928     C                   SETOFF                                       22
031000940928     C  N40              READC     TB69S02                                31
031100940928     C                   END
031200940928     C*
031300940929     C     WSELEZ        IFEQ      'S'
031400941003     C     DS7RIC        IFEQ      ' '
031500940929     C                   EXSR      INZ01
031600941003     C                   ELSE
031700941107     C     DS7OPZ        IFNE      '1'
031800941104     C                   MOVEL     *BLANKS       DS7COD
031900940929     C                   END
032000941104     C                   END
032100941003     C                   END
032200940929     C*
032300940929     C     FINCT2        ENDSR
032400960111     C*--------------------------------------------------------------*
032500960111     C*  CAR7Q:  Caricamento tabella 7Q                              *
032600960111     C*--------------------------------------------------------------*
032700960111     C     CAR7Q         BEGSR
032800960111     C*
032900960111     C                   MOVEL     '7Q'          KCOD
033000960111     C                   Z-ADD     0             X                 3 0
033100960111     C                   MOVEA     *BLANKS       C7Q
033200960111     C                   MOVEA     *BLANKS       D7Q
033300960111     C     KTAB1         CHAIN     TABEL00F                           31
033400960111     C     *IN31         DOWEQ     '0'
033500960111     C     TBLFLG        IFEQ      ' '
033600960111     C                   ADD       1             X
033700960111     C                   MOVEL     TBLKEY        C7Q(X)
033800960111     C                   MOVEL     TBLUNI        D7Q(X)
033900960111     C                   END
034000960111     C     KTAB1         READE     TABEL00F                               31
034100960111     C                   END
034200960111     C                   Z-ADD     X             Z                 3 0
034300960111     C                   ENDSR
034400931209     C*--------------------------------------------------------------*
034500931209     C*  *INZSR: Operazioni iniziali                                 *
034600931209     C*--------------------------------------------------------------*
034700931209     C     *INZSR        BEGSR
034800931209     C*
034900931209     C     *ENTRY        PLIST
035000931209     C                   PARM                    KPJBA
035100940928     C                   MOVEL     KPJBU         DSTB69
035200960112     C     DSTB69        IFEQ      *BLANKS
035300960112     C                   CLEAR                   DSTB69
035400960112     C                   END
035500931209     C*----------------------------------------------------*
035600931209     C*  Richiamo XPARUT
035700931209     C                   Z-ADD     1             CODUT
035800931209     C                   CALL      'XPARUT'
035900931209     C                   PARM                    UTEDSE
036000931209     C     CODUT         CABEQ     -1            FINE
036100940927     C                   MOVEL     RAGUT         V1CRSU
036200931209     C*----------------------------------------------------*
036300931209     C* Definizione chiavi                                 *
036400940927     C     KTAB1         KLIST
036500940927     C                   KFLD                    KKUT
036600940927     C                   KFLD                    KCOD
036700940928     C*
036800940928     C     KTAB2         KLIST
036900940928     C                   KFLD                    KKUT
037000940928     C                   KFLD                    KCOD
037100940928     C                   KFLD                    KKEY
037200931209     C*----------------------------------------------------*
037300931209     C* Definizione variabili                              *
037400940927     C     *LIKE         DEFINE    TBLKUT        KKUT
037500940927     C     *LIKE         DEFINE    TBLCOD        KCOD
037600940928     C     *LIKE         DEFINE    TBLKEY        KKEY
037700971210     C******************************************************
037800971210     C* Richiamo pgm per controllo modificabilità tabella
037900021023     C                   MOVEL     KNSIF         KNSIF7            7
038000021024     C                   MOVE      KNSIF7        PROVA             1
038100980225     C     DS7GES        IFEQ      *BLANKS
038200021024     C     PROVA         ANDNE     'P'
038300971210     C                   CLEAR                   TRUL12
038400971210     C                   MOVEL     'L'           I12TLA
038500971210     C                   MOVEL     KNSIF         I12NSI
038600971210     C                   MOVEL     '7Q'          I12COD
038700971210     C                   CALL      'TRUL12R'
038800971210     C                   PARM                    TRUL12
038900971210     C     O12ERR        IFEQ      '1'
039000971210     C                   GOTO      FINE
039100971210     C                   END
039200980225     C                   END
039300971210     C******************************************************
039400940927     C*  Inizializzazione variabili
039500940928     C                   Z-ADD     1             KKUT
039600940927     C                   MOVEL     'N'           WFINE             1
039700940927     C                   MOVEL     *BLANKS       V1DFUN
039800940928     C                   SETOFF                                       90
039900960112     C*----------------------------------------------------*
040000960112     C*  carico tabella particolarità giacenza in schiera
040100960112     C                   EXSR      CAR7Q
040200960112     C*----------------------------------------------------*
040300960112     C     DS7KSC        IFNE      0
040400960112     C                   MOVEL     '7U'          KCOD
040500960112     C                   MOVEL     DS7KSC        KKEY
040600960112     C                   MOVE      'Q'           KKEY
040700960112     C                   MOVEA     *BLANKS       CODP
040800960112     C     KTAB2         CHAIN     TABEL00F                           31
040900960112     C     *IN31         IFEQ      '0'
041000960112     C     TBLFLG        ANDEQ     ' '
041100050509     c                   movel     tbluni        ds7u
041200050509     c                   if        §7ucpa <> ' '
041300960112     C                   MOVEA     TBLUNI        CODP
041400960112     C                   MOVEL     TBLUNI        DS7CDP
041500960112     C                   END
041600050509     C                   END
041700960112     C                   END
041800940927     C*----------------------------------------------------*
041900940928     C*  Se il programma non è richiamato inizializzo DS
042000940928     C*  parametri ed emetto prima videata
042100940929     C     DS7RIC        IFEQ      ' '
042200940928     C                   CLEAR                   DSTB69
042300940927     C                   EXSR      INZ01
042400940928     C*----------------------------------------------------*
042500940927     C                   ELSE
042600940928     C*----------------------------------------------------*
042700940929     C     DS7GES        COMP      'V'                                    90
042800940929     C                   MOVEL     ' '           DS7F03
042900940928     C*  Se il programma è richiamato e ho il codice particolarità
043000940928     C*  impostato richiamo pgm. successivo
043100940929     C     DS7COD        IFNE      *BLANKS
043200960108     C                   MOVEL     DSTB69        KPJBU
043300960108     C                   CALL      'TRTB69R0'
043400960108     C                   PARM                    KPJBA
043500960109     C                   MOVEL     KPJBU         DSTB69
043600960111     C                   EXSR      CAR7Q
043700940929     C                   MOVEL     'S'           WFINE
043800940928     C*----------------------------------------------------*
043900940928     C* ... altrimenti controllo se ho impostato il codice cliente
044000940928     C* e se si provvedo al caricamento delle particolarità giac.
044100940928     C* a cui è abilitato
044200940928     C                   ELSE
044300940928     C*----------------------------------------------------*
044400940928     C*  ... emetto subfile selezione particolarità.
044500940928     C                   EXSR      INZ02
044600940928     C                   END
044700940928     C                   END
044800940228     C*----------------------------------------------------*
044900931209     C                   ENDSR
045000931209     O*-----------------------------------------------------*
045100940927**
045200940927Immettere codice particolarità giacenza                               01
045300940419**
045400940927INTERROGAZIONE
