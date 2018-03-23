000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000300931209     F*  KL        RITORNO                                           *
000400940928     F*  KC        FINE LAVORO                                       *
000500891030     F*--------------------------------------------------------------*
000600960115     FTRTB73D   CF   E             WORKSTN
000700960115     F                                     SFILE(TB73S02:NRR)
000800940418     F*----------
000900940928     FTABEL00F  IF   E           K DISK
001000940418     D*--------------------------------------------------------------*
001100940418     D*   SCHIERE                                                    *
001200940701     D*--------------------------------------------------------------*
001300960115     D ERR             S             70    DIM(1) CTDATA PERRCD(1)
001400940928     D COST            S             16    DIM(1) CTDATA PERRCD(1)
001500960115     D CODP            S              2    DIM(30)
001600960115     D C7S             S              2    DIM(100)
001700960115     D D7S             S             89    DIM(100)
001800940928     D*--------------------------------------------------------------*
001900940928     D*  DS                                                          *
002000940928     D*--------------------------------------------------------------*
002100960115     D DSTB73        E DS                  EXTNAME(DS7PQRS)
002200960115     D DS7S          E DS
002300080227     D DS7V          E DS
002400900608      *
002500891030     D KPJBA         E DS
002600911118      *
002700971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
002800911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002900960108      *  Codice x posizionamento su tabella 7V
003000960108     D                 DS
003100960108     D  WTPPAR                 1      1
003200960108     D  WCDPAR                 2      3
003300960109     D  WTPELE                 4      4
003400960109     D  WKEY01                 1      4
003500960109     D  WPROGG                 5      8  0
003600960110     D  WKEY02                 1      8
003700960109      *  SComposizione KEY di TABEL x 7V
003800960109     D                 DS
003900960109     D  WSVKEY                 1      4
004000960109     D  TBLKEY                 1      8
004100940228      *
004200940228     D                SDS
004300940927     D  V1CPGM                 1     10
004400931209     C*--------------------------------------------------------------*
004500931209     C*    C I C L O    P R I N C I P A L E                          *
004600931209     C*--------------------------------------------------------------*
004700931209     C*  Loop gestione varie videate fino a rich. fine elaboraz.
004800940927     C     WFINE         DOWNE     'S'
004900940927     C*
005000940928     C     WTPVID        CASEQ     '1'           GESD01
005100940928     C     WTPVID        CASEQ     '2'           GESD02
005200940928     C     WTPVID        CASEQ     '3'           GESD03
005300931209     C                   END
005400931209     C*
005500931209     C                   END
005600931209     C* Fine programma
005700931209     C     FINE          TAG
005800960115     C                   MOVEL     DSTB73        KPJBU
005900931209     C                   SETON                                        LR
006000940928     C*---------------------------------------------------*
006100940928     C*  GESD01: Gestione prima videata                   *
006200940928     C*---------------------------------------------------*
006300940928     C     GESD01        BEGSR
006400940928     C*---------------------------------------------------*
006500931209     C*  Emissione videata
006600940929     C                   MOVEL     *BLANKS       V1DFUN
006700960115     C                   WRITE     TB73T01
006800960115     C                   EXFMT     TB73D01
006900940928     C                   SETOFF                                       4028
007000940418     C                   MOVEL     *BLANKS       $MSG
007100940928     C*---------------------------------------------------*
007200931209     C*  Fine elaborazione
007300931209     C     *INKC         IFEQ      '1'
007400940929     C                   MOVEL     'S'           WFINE
007500931209     C                   GOTO      FINVD1
007600931209     C                   END
007700940928     C*---------------------------------------------------*
007800940928     C*   Se richiesta ricerca richiamo subfile
007900940928     C     '?'           SCAN      V1CCOD                                 32
008000940928     C     *IN32         IFEQ      '1'
008100940929     C                   EXSR      INZ02
008200940929     C                   GOTO      FINVD1
008300940928     C                   END
008400940928     C*---------------------------------------------------*
008500940928     C*   Se immesso codice eseguo caricamento dati
008600940928     C     V1CCOD        IFNE      *BLANKS
008700940929     C                   MOVEL     V1CCOD        DS7COD
008800940929     C                   MOVEL     '1'           DS7OPZ
008900960115     C                   MOVEL     DSTB73        KPJBU
009000960115     C                   CALL      'TRTB73R0'
009100960110     C                   PARM                    KPJBA
009200960115     C                   MOVEL     KPJBU         DSTB73
009300940929     C     DS7F03        IFEQ      '1'
009400940929     C                   MOVEL     'S'           WFINE
009500940929     C                   ELSE
009600960115     C                   EXSR      CAR7S
009700940929     C                   EXSR      INZ01
009800940929     C                   END
009900940928     C                   ELSE
010000940928     C                   SETON                                        2840
010100940928     C                   MOVEL     ERR(1)        $MSG
010200940928     C                   END
010300931209     C*--------------------------------------------------------------*
010400940928     C     FINVD1        ENDSR
010500940928     C*--------------------------------------------------------------*
010600940928     C*  GESD02: Gestione prima videata                              *
010700940928     C*--------------------------------------------------------------*
010800940928     C     GESD02        BEGSR
010900940928     C*----------------------------------------------------*
011000940928     C*  Emissione videata
011100940929     C  N28              Z-ADD     1             DSPNRR
011200940928     C                   SETON                                        20
011300960115     C                   WRITE     TB73Z02
011400960115     C                   EXFMT     TB73C02
011500940928     C                   SETOFF                                       2840
011600940928     C                   MOVEL     *BLANKS       $MSG
011700940928     C*----------------------------------------------------*
011800940928     C*  Fine elaborazione
011900940928     C     *INKC         IFEQ      '1'
012000940929     C                   MOVEL     'S'           WFINE
012100940929     C                   MOVEL     '1'           DS7F03
012200940928     C                   GOTO      FINVD2
012300940928     C                   END
012400940928     C*----------------------------------------------------*
012500940928     C*  Ritorno prima videata
012600960112     C     *INKL         IFEQ      '1'
012700960115     C     DS7RIC        IFEQ      ' '
012800960112     C                   EXSR      INZ01
012900960112     C                   ELSE
013000960112     C                   MOVEL     'S'           WFINE
013100960112     C                   END
013200940928     C                   GOTO      FINVD2
013300940928     C                   END
013400940928     C*----------------------------------------------------*
013500940928     C*  controlli
013600940928     C                   EXSR      CTR02
013700940929     C*----------------------------------------------------*
013800940928     C     FINVD2        ENDSR
013900940928     C*----------------------------------------------------*
014000940928     C*  GESD03: Gestione videata subfile vuoto            *
014100940928     C*----------------------------------------------------*
014200940928     C     GESD03        BEGSR
014300940928     C*----------------------------------------------------*
014400940928     C*  Emissione videata
014500940929     C                   SETOFF                                       20
014600960115     C                   WRITE     TB73D02
014700960115     C                   EXFMT     TB73C02
014800940928     C                   SETOFF                                       2840
014900940928     C*----------------------------------------------------*
015000940928     C*  Fine elaborazione
015100940928     C     *INKC         IFEQ      '1'
015200940929     C                   MOVEL     '1'           DS7F03
015300940929     C                   MOVEL     'S'           WFINE
015400940928     C                   GOTO      FINVD3
015500940928     C                   END
015600940928     C*----------------------------------------------------*
015700960115     C     DS7RIC        IFEQ      ' '
015800940928     C                   EXSR      INZ01
015900940929     C                   ELSE
016000940929     C                   MOVEL     'S'           WFINE
016100940929     C                   END
016200940928     C*----------------------------------------------------*
016300940928     C     FINVD3        ENDSR
016400940928     C*----------------------------------------------------*
016500940928     C*  INZ01   Gestione inizializzazione prima videata   *
016600940928     C*----------------------------------------------------*
016700940927     C     INZ01         BEGSR
016800940419     C*  Inizializzo prima videata impostando rich. ricerca tabella
016900940929     C                   MOVEL     '1'           WTPVID
017000940928     C                   MOVEL     '? '          V1CCOD
017100940419     C                   MOVEL     *BLANKS       $MSG
017200940928     C                   SETOFF                                       2840
017300940419     C*
017400940419     C                   ENDSR
017500940928     C*----------------------------------------------------*
017600940928     C*  INZ02 : Gestione inizializz. Seconda videata      *
017700940928     C*----------------------------------------------------*
017800940928     C     INZ02         BEGSR
017900940701     C*----------------------------------------------------*
018000940930     C                   SETOFF                                       28
018100940418     C                   MOVEL     *BLANKS       $MSG
018200940928     C*----------------------------------------------------*
018300940929     C     DS7GES        IFEQ      'V'
018400940929     C                   MOVEL     COST(1)       V1DFUN
018500940928     C                   ELSE
018600940929     C                   MOVEL     *BLANKS       V1DFUN
018700940928     C                   END
018800940927     C*----------------------------------------------------*
018900940927     C*  Inizializzazione subfile
019000940927     C                   SETOFF                                       2021
019100960115     C                   WRITE     TB73C02
019200940929     C                   Z-ADD     0             NRR               4 0
019300940927     C                   SETON                                        21
019400940927     C*----------------------------------------------------*
019500940927     C*  Caricamento subfile
019600960109     C                   Z-ADD     1             X
019700960109     C                   MOVEL     '7V'          KCOD
019800940929     C                   MOVEL     *BLANKS       V2CSEL
019900960109     C                   DO        Z             X
020000960109      *  Costruisco chiave per posizionamento su tabel
020100960115     C                   MOVEL     'S'           WTPPAR
020200960115     C                   MOVEL     C7S(X)        WCDPAR
020300960109     C                   MOVEL     'V'           WTPELE
020400960109     C                   Z-ADD     0             WPROGG
020500960110     C                   MOVEL     WKEY02        KKEY
020600960109     C                   SETOFF                                       01
020700960109     C     KTAB2         SETLL     TABEL00F
020800960109     C     KTAB1         READE     TABEL00F                               31
020900960109     C     *IN31         IFEQ      '0'
021000960109     C     WSVKEY        ANDEQ     WKEY01
021100960109     C*----------------------------------------------------*
021200960109     C*  Se immesso codice cliente prima di caricare particol.
021300960109     C*  controllo se il cliente è abilitato
021400960109     C                   SETON                                        32
021500960115     C                   MOVEL     D7S(X)        DS7S
021600960109     C     DS7KSC        IFNE      0
021700960115     C     §7SABC        ANDEQ     'S'
021800960115     C     C7S(X)        LOOKUP    CODP                                   32
021900960109     C                   END
022000960109     C*----------------------------------------------------*
022100960109     C     *IN32         IFEQ      '1'
022200960115     C                   MOVEL     C7S(X)        V2CCOD
022300960115     C                   MOVEL     §7SELI        V2CELI
022400960115     C                   MOVEL     §7SINS        V2CINS
022500940927     C     *IN31         DOWEQ     '0'
022600960109     C     WSVKEY        ANDEQ     WKEY01
022700960109     C     TBLUNI        IFNE      *BLANKS
022800960109     C                   MOVEL     TBLUNI        V2DCOD
022900940928     C                   ADD       1             NRR
023000960115     C                   WRITE     TB73S02
023100960109     C                   END
023200960109     C     KTAB1         READE     TABEL00F                               31
023300960109     C                   SETON                                        01
023400960109     C                   END
023500940928     C*----------------------------------------------------*
023600960109     C                   END
023700940418     C*----------------------------------------------------*
023800960109     C                   END
023900960109     C*----------------------------------------------------*
024000960109     C                   END
024100960109     C*----------------------------------------------------*
024200940928     C*  Controllo se devo emettere subfile vuoto
024300940928     C     NRR           IFEQ      0
024400940928     C                   MOVEL     '3'           WTPVID
024500940928     C                   ELSE
024600940929     C                   MOVEL     '2'           WTPVID            1
024700940928     C                   END
024800940928     C*----------------------------------------------------*
024900940928     C                   ENDSR
025000940928     C*----------------------------------------------------*
025100940928     C*  CTR02 : Gestione inizial. seconda videata         *
025200940928     C*----------------------------------------------------*
025300940928     C     CTR02         BEGSR
025400940928     C*----------------------------------------------------*
025500940929     C                   MOVEL     'N'           WSELEZ            1
025600960111     C                   SETOFF                                       01
025700960115     C                   READC     TB73S02                                31
025800940928     C     *IN31         DOWEQ     '0'
025900940928     C     *IN28         ANDEQ     '0'
026000940928     C                   SETOFF                                       40
026100940928     C     V2CSEL        IFEQ      '5'
026200960108     C                   MOVEL     V2CSEL        DS7OPZ
026300960108     C                   MOVEL     V2CCOD        DS7COD
026400960115     C                   MOVEL     DSTB73        KPJBU
026500960109     C                   MOVEL     'S'           WSELEZ
026600960115     C                   CALL      'TRTB73R0'
026700960108     C                   PARM                    KPJBA
026800960115     C                   MOVEL     KPJBU         DSTB73
026900940929     C     DS7F03        IFEQ      '1'
027000940929     C                   MOVEL     'S'           WFINE
027100940929     C                   GOTO      FINCT2
027200940929     C                   END
027300940928     C                   END
027400960115     C                   EXSR      CAR7S
027500940928     C     V2CSEL        IFEQ      '1'
027600941107     C                   MOVEL     V2CSEL        DS7OPZ
027700940929     C     DS7GES        IFEQ      'V'
027800940930     C                   MOVEL     V2CCOD        DS7COD
027900941107     C                   MOVEL     V2CELI        DS7ELI
028000941107     C                   MOVEL     V2CINS        DS7INS
028100940930     C                   MOVEL     'S'           WFINE
028200940928     C                   ELSE
028300940929     C                   MOVEL     V2CCOD        DS7COD
028400960115     C                   MOVEL     DSTB73        KPJBU
028500960109     C                   MOVEL     'S'           WSELEZ
028600960115     C                   CALL      'TRTB73R0'
028700960108     C                   PARM                    KPJBA
028800960115     C                   MOVEL     KPJBU         DSTB73
028900940929     C     DS7F03        IFEQ      '1'
029000940929     C                   MOVEL     'S'           WFINE
029100940929     C                   GOTO      FINCT2
029200940929     C                   END
029300960115     C                   EXSR      CAR7S
029400940928     C                   END
029500940929     C                   END
029600940928     C*  Aggiorno Sufile
029700940928     C                   SETON                                        22
029800940928     C     *IN40         IFEQ      '0'
029900940929     C                   MOVEL     ' '           V2CSEL
030000940928     C                   END
030100960115     C                   UPDATE    TB73S02
030200940928     C                   SETOFF                                       22
030300960115     C  N40              READC     TB73S02                                31
030400940928     C                   END
030500940928     C*
030600940929     C     WSELEZ        IFEQ      'S'
030700960115     C     DS7RIC        IFEQ      ' '
030800940929     C                   EXSR      INZ01
030900941003     C                   ELSE
031000941107     C     DS7OPZ        IFNE      '1'
031100941104     C                   MOVEL     *BLANKS       DS7COD
031200940929     C                   END
031300941104     C                   END
031400941003     C                   END
031500940929     C*
031600940929     C     FINCT2        ENDSR
031700960111     C*--------------------------------------------------------------*
031800960115     C*  CAR7S:  Caricamento tabella 7S                              *
031900960111     C*--------------------------------------------------------------*
032000960115     C     CAR7S         BEGSR
032100960111     C*
032200960115     C                   MOVEL     '7S'          KCOD
032300960111     C                   Z-ADD     0             X                 3 0
032400960115     C                   MOVEA     *BLANKS       C7S
032500960115     C                   MOVEA     *BLANKS       D7S
032600960111     C     KTAB1         CHAIN     TABEL00F                           31
032700960111     C     *IN31         DOWEQ     '0'
032800960111     C     TBLFLG        IFEQ      ' '
032900960111     C                   ADD       1             X
033000960115     C                   MOVEL     TBLKEY        C7S(X)
033100960115     C                   MOVEL     TBLUNI        D7S(X)
033200960111     C                   END
033300960111     C     KTAB1         READE     TABEL00F                               31
033400960111     C                   END
033500960111     C                   Z-ADD     X             Z                 3 0
033600960111     C                   ENDSR
033700931209     C*--------------------------------------------------------------*
033800931209     C*  *INZSR: Operazioni iniziali                                 *
033900931209     C*--------------------------------------------------------------*
034000931209     C     *INZSR        BEGSR
034100931209     C*
034200931209     C     *ENTRY        PLIST
034300931209     C                   PARM                    KPJBA
034400960115     C                   MOVEL     KPJBU         DSTB73
034500960115     C     DSTB73        IFEQ      *BLANKS
034600960115     C                   CLEAR                   DSTB73
034700960112     C                   END
034800931209     C*----------------------------------------------------*
034900931209     C*  Richiamo XPARUT
035000931209     C                   Z-ADD     1             CODUT
035100931209     C                   CALL      'XPARUT'
035200931209     C                   PARM                    UTEDSE
035300931209     C     CODUT         CABEQ     -1            FINE
035400940927     C                   MOVEL     RAGUT         V1CRSU
035500931209     C*----------------------------------------------------*
035600931209     C* Definizione chiavi                                 *
035700940927     C     KTAB1         KLIST
035800940927     C                   KFLD                    KKUT
035900940927     C                   KFLD                    KCOD
036000940928     C*
036100940928     C     KTAB2         KLIST
036200940928     C                   KFLD                    KKUT
036300940928     C                   KFLD                    KCOD
036400940928     C                   KFLD                    KKEY
036500931209     C*----------------------------------------------------*
036600931209     C* Definizione variabili                              *
036700940927     C     *LIKE         DEFINE    TBLKUT        KKUT
036800940927     C     *LIKE         DEFINE    TBLCOD        KCOD
036900940928     C     *LIKE         DEFINE    TBLKEY        KKEY
037000971210     C******************************************************
037100971210     C* Richiamo pgm per controllo modificabilità tabella
037200980225     C     DS7GES        IFEQ      *BLANKS
037300971210     C                   CLEAR                   TRUL12
037400971210     C                   MOVEL     'L'           I12TLA
037500971210     C                   MOVEL     KNSIF         I12NSI
037600971210     C                   MOVEL     '7S'          I12COD
037700971210     C                   CALL      'TRUL12R'
037800971210     C                   PARM                    TRUL12
037900971210     C     O12ERR        IFEQ      '1'
038000971210     C                   GOTO      FINE
038100971210     C                   END
038200980225     C                   END
038300940725     C*----------------------------------------------------*
038400940927     C*  Inizializzazione variabili
038500940928     C                   Z-ADD     1             KKUT
038600940927     C                   MOVEL     'N'           WFINE             1
038700940927     C                   MOVEL     *BLANKS       V1DFUN
038800940928     C                   SETOFF                                       90
038900960111     C*----------------------------------------------------*
039000960115     C*  carico tabella particolarità varie in schiera
039100960115     C                   EXSR      CAR7S
039200960112     C*----------------------------------------------------*
039300960112     C     DS7KSC        IFNE      0
039400960112     C                   MOVEL     '7U'          KCOD
039500960112     C                   MOVEL     DS7KSC        KKEY
039600960115     C                   MOVE      'S'           KKEY
039700960112     C                   MOVEA     *BLANKS       CODP
039800960112     C     KTAB2         CHAIN     TABEL00F                           31
039900960112     C     *IN31         IFEQ      '0'
040000960112     C     TBLFLG        ANDEQ     ' '
040100960112     C                   MOVEA     TBLUNI        CODP
040200960112     C                   MOVEL     TBLUNI        DS7CDP
040300960112     C                   END
040400960112     C                   END
040500940927     C*----------------------------------------------------*
040600940928     C*  Se il programma non è richiamato inizializzo DS
040700940928     C*  parametri ed emetto prima videata
040800960115     C     DS7RIC        IFEQ      ' '
040900960115     C                   CLEAR                   DSTB73
041000940927     C                   EXSR      INZ01
041100940928     C*----------------------------------------------------*
041200940927     C                   ELSE
041300940928     C*----------------------------------------------------*
041400940929     C     DS7GES        COMP      'V'                                    90
041500940929     C                   MOVEL     ' '           DS7F03
041600940928     C*  Se il programma è richiamato e ho il codice particolarità
041700940928     C*  impostato richiamo pgm. successivo
041800940929     C     DS7COD        IFNE      *BLANKS
041900960115     C                   MOVEL     DSTB73        KPJBU
042000960115     C                   CALL      'TRTB73R0'
042100960108     C                   PARM                    KPJBA
042200960115     C                   MOVEL     KPJBU         DSTB73
042300960115     C                   EXSR      CAR7S
042400940929     C                   MOVEL     'S'           WFINE
042500940928     C*----------------------------------------------------*
042600940928     C* ... altrimenti controllo se ho impostato il codice cliente
042700960115     C* e se si provvedo al caricamento delle particolarità varie
042800940928     C* a cui è abilitato
042900940928     C                   ELSE
043000940928     C*----------------------------------------------------*
043100940928     C*  ... emetto subfile selezione particolarità.
043200940928     C                   EXSR      INZ02
043300940928     C                   END
043400940928     C                   END
043500940228     C*----------------------------------------------------*
043600931209     C                   ENDSR
043700931209     O*-----------------------------------------------------*
043800940927**
043900960115Immettere codice particolarità varie                                  01
044000940419**
044100940927INTERROGAZIONE
