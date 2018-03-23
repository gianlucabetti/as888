000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000300931209     F*  KL        RITORNO                                           *
000400940928     F*  KC        FINE LAVORO                                       *
000500891030     F*--------------------------------------------------------------*
000600960111     FTRTB70D   CF   E             WORKSTN
000700960111     F                                     SFILE(TB70S02:NRR)
000800940418     F*----------
000900940928     FTABEL00F  IF   E           K DISK
001000940418     D*--------------------------------------------------------------*
001100940418     D*   SCHIERE                                                    *
001200940701     D*--------------------------------------------------------------*
001300940930     D ERR             S             70    DIM(1) CTDATA PERRCD(1)
001400940928     D COST            S             16    DIM(1) CTDATA PERRCD(1)
001500960111     D CODP            S              2    DIM(30)
001600960111     D C7R             S              2    DIM(100)
001700960111     D D7R             S             89    DIM(100)
001800940928     D*--------------------------------------------------------------*
001900940928     D*  DS                                                          *
002000940928     D*--------------------------------------------------------------*
002100960111     D DSTB70        E DS                  EXTNAME(DS7PQRS)
002200080227     D DS7V          E DS
002300960111     D DS7R          E DS
002400940928     D  TIP                   78     82
002500940928     D  DSP                   83     87
002600900608      *
002700891030     D KPJBA         E DS
002800911118      *
002900971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
003000911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003100960108      *  Codice x posizionamento su tabella 7V
003200960108     D                 DS
003300960108     D  WTPPAR                 1      1
003400960108     D  WCDPAR                 2      3
003500960109     D  WTPELE                 4      4
003600960109     D  WKEY01                 1      4
003700960109     D  WPROGG                 5      8  0
003800960110     D  WKEY02                 1      8
003900960109      *  SComposizione KEY di TABEL x 7V
004000960109     D                 DS
004100960109     D  WSVKEY                 1      4
004200960109     D  TBLKEY                 1      8
004300940228      *
004400940228     D                SDS
004500940927     D  V1CPGM                 1     10
004600931209     C*--------------------------------------------------------------*
004700931209     C*    C I C L O    P R I N C I P A L E                          *
004800931209     C*--------------------------------------------------------------*
004900931209     C*  Loop gestione varie videate fino a rich. fine elaboraz.
005000940927     C     WFINE         DOWNE     'S'
005100940927     C*
005200940928     C     WTPVID        CASEQ     '1'           GESD01
005300940928     C     WTPVID        CASEQ     '2'           GESD02
005400940928     C     WTPVID        CASEQ     '3'           GESD03
005500931209     C                   END
005600931209     C*
005700931209     C                   END
005800931209     C* Fine programma
005900931209     C     FINE          TAG
006000960111     C                   MOVEL     DSTB70        KPJBU
006100931209     C                   SETON                                        LR
006200940928     C*---------------------------------------------------*
006300940928     C*  GESD01: Gestione prima videata                   *
006400940928     C*---------------------------------------------------*
006500940928     C     GESD01        BEGSR
006600940928     C*---------------------------------------------------*
006700931209     C*  Emissione videata
006800940929     C                   MOVEL     *BLANKS       V1DFUN
006900960111     C                   WRITE     TB70T01
007000960111     C                   EXFMT     TB70D01
007100940928     C                   SETOFF                                       4028
007200940418     C                   MOVEL     *BLANKS       $MSG
007300940928     C*---------------------------------------------------*
007400931209     C*  Fine elaborazione
007500931209     C     *INKC         IFEQ      '1'
007600940929     C                   MOVEL     'S'           WFINE
007700931209     C                   GOTO      FINVD1
007800931209     C                   END
007900940928     C*---------------------------------------------------*
008000940928     C*   Se richiesta ricerca richiamo subfile
008100940928     C     '?'           SCAN      V1CCOD                                 32
008200940928     C     *IN32         IFEQ      '1'
008300940929     C                   EXSR      INZ02
008400940929     C                   GOTO      FINVD1
008500940928     C                   END
008600940928     C*---------------------------------------------------*
008700940928     C*   Se immesso codice eseguo caricamento dati
008800940928     C     V1CCOD        IFNE      *BLANKS
008900940929     C                   MOVEL     V1CCOD        DS7COD
009000940929     C                   MOVEL     '1'           DS7OPZ
009100960111     C                   MOVEL     DSTB70        KPJBU
009200960111     C                   CALL      'TRTB70R0'
009300960110     C                   PARM                    KPJBA
009400960111     C                   MOVEL     KPJBU         DSTB70
009500940929     C     DS7F03        IFEQ      '1'
009600940929     C                   MOVEL     'S'           WFINE
009700940929     C                   ELSE
009800960111     C                   EXSR      CAR7R
009900940929     C                   EXSR      INZ01
010000940929     C                   END
010100940928     C                   ELSE
010200940928     C                   SETON                                        2840
010300940928     C                   MOVEL     ERR(1)        $MSG
010400940928     C                   END
010500931209     C*--------------------------------------------------------------*
010600940928     C     FINVD1        ENDSR
010700940928     C*--------------------------------------------------------------*
010800940928     C*  GESD02: Gestione prima videata                              *
010900940928     C*--------------------------------------------------------------*
011000940928     C     GESD02        BEGSR
011100940928     C*----------------------------------------------------*
011200940928     C*  Emissione videata
011300940929     C  N28              Z-ADD     1             DSPNRR
011400940928     C                   SETON                                        20
011500960111     C                   WRITE     TB70Z02
011600960111     C                   EXFMT     TB70C02
011700940928     C                   SETOFF                                       2840
011800940928     C                   MOVEL     *BLANKS       $MSG
011900940928     C*----------------------------------------------------*
012000940928     C*  Fine elaborazione
012100940928     C     *INKC         IFEQ      '1'
012200940929     C                   MOVEL     'S'           WFINE
012300940929     C                   MOVEL     '1'           DS7F03
012400940928     C                   GOTO      FINVD2
012500940928     C                   END
012600940928     C*----------------------------------------------------*
012700940928     C*  Ritorno prima videata
012800960112     C     *INKL         IFEQ      '1'
012900960112     C     DS7RIC        IFEQ      ' '
013000960112     C                   EXSR      INZ01
013100960112     C                   ELSE
013200960112     C                   MOVEL     'S'           WFINE
013300960112     C                   END
013400940928     C                   GOTO      FINVD2
013500940928     C                   END
013600940928     C*----------------------------------------------------*
013700940928     C*  controlli
013800940928     C                   EXSR      CTR02
013900940929     C*----------------------------------------------------*
014000940928     C     FINVD2        ENDSR
014100940928     C*----------------------------------------------------*
014200940928     C*  GESD03: Gestione videata subfile vuoto            *
014300940928     C*----------------------------------------------------*
014400940928     C     GESD03        BEGSR
014500940928     C*----------------------------------------------------*
014600940928     C*  Emissione videata
014700940929     C                   SETOFF                                       20
014800960111     C                   WRITE     TB70D02
014900960111     C                   EXFMT     TB70C02
015000940928     C                   SETOFF                                       2840
015100940928     C*----------------------------------------------------*
015200940928     C*  Fine elaborazione
015300940928     C     *INKC         IFEQ      '1'
015400940929     C                   MOVEL     '1'           DS7F03
015500940929     C                   MOVEL     'S'           WFINE
015600940928     C                   GOTO      FINVD3
015700940928     C                   END
015800940928     C*----------------------------------------------------*
015900940929     C     DS7RIC        IFEQ      ' '
016000940928     C                   EXSR      INZ01
016100940929     C                   ELSE
016200940929     C                   MOVEL     'S'           WFINE
016300940929     C                   END
016400940928     C*----------------------------------------------------*
016500940928     C     FINVD3        ENDSR
016600940928     C*----------------------------------------------------*
016700940928     C*  INZ01   Gestione inizializzazione prima videata   *
016800940928     C*----------------------------------------------------*
016900940927     C     INZ01         BEGSR
017000940419     C*  Inizializzo prima videata impostando rich. ricerca tabella
017100940929     C                   MOVEL     '1'           WTPVID
017200940928     C                   MOVEL     '? '          V1CCOD
017300940419     C                   MOVEL     *BLANKS       $MSG
017400940928     C                   SETOFF                                       2840
017500940419     C*
017600940419     C                   ENDSR
017700940928     C*----------------------------------------------------*
017800940928     C*  INZ02 : Gestione inizializz. Seconda videata      *
017900940928     C*----------------------------------------------------*
018000940928     C     INZ02         BEGSR
018100940701     C*----------------------------------------------------*
018200940930     C                   SETOFF                                       28
018300940418     C                   MOVEL     *BLANKS       $MSG
018400940928     C*----------------------------------------------------*
018500940929     C     DS7GES        IFEQ      'V'
018600940929     C                   MOVEL     COST(1)       V1DFUN
018700940928     C                   ELSE
018800940929     C                   MOVEL     *BLANKS       V1DFUN
018900940928     C                   END
019000940927     C*----------------------------------------------------*
019100940927     C*  Inizializzazione subfile
019200940927     C                   SETOFF                                       2021
019300960111     C                   WRITE     TB70C02
019400940929     C                   Z-ADD     0             NRR               4 0
019500940927     C                   SETON                                        21
019600940927     C*----------------------------------------------------*
019700940927     C*  Caricamento subfile
019800960109     C                   Z-ADD     1             X
019900960109     C                   MOVEL     '7V'          KCOD
020000940929     C                   MOVEL     *BLANKS       V2CSEL
020100960109     C                   DO        Z             X
020200960109      *  Costruisco chiave per posizionamento su tabel
020300960111     C                   MOVEL     'R'           WTPPAR
020400960111     C                   MOVEL     C7R(X)        WCDPAR
020500960109     C                   MOVEL     'V'           WTPELE
020600960109     C                   Z-ADD     0             WPROGG
020700960110     C                   MOVEL     WKEY02        KKEY
020800960109     C                   SETOFF                                       01
020900960109     C     KTAB2         SETLL     TABEL00F
021000960109     C     KTAB1         READE     TABEL00F                               31
021100960109     C     *IN31         IFEQ      '0'
021200960109     C     WSVKEY        ANDEQ     WKEY01
021300960109     C*----------------------------------------------------*
021400960109     C*  Se immesso codice cliente prima di caricare particol.
021500960109     C*  controllo se il cliente è abilitato
021600960109     C                   SETON                                        32
021700960111     C                   MOVEL     D7R(X)        DS7R
021800960109     C     DS7KSC        IFNE      0
021900960111     C     §7RABC        ANDEQ     'S'
022000960111     C     C7R(X)        LOOKUP    CODP                                   32
022100960109     C                   END
022200960109     C*----------------------------------------------------*
022300960109     C     *IN32         IFEQ      '1'
022400960111     C                   MOVEL     C7R(X)        V2CCOD
022500960111     C                   MOVEL     §7RELI        V2CELI
022600960111     C                   MOVEL     §7RINS        V2CINS
022700940927     C     *IN31         DOWEQ     '0'
022800960109     C     WSVKEY        ANDEQ     WKEY01
022900140507      * se la prima riga è stata sblankata non permetterebbe di poter
023000140507      * selezionare la tabella anche se vi fossero successive righe cariche
023100140507     c                   clear                   scritto_riga      1
023200960109     C     TBLUNI        IFNE      *BLANKS
023300960109     C                   MOVEL     TBLUNI        V2DCOD
023400940928     C                   ADD       1             NRR
023500960111     C                   WRITE     TB70S02
023600140507     c                   movel     'S'           scritto_riga
023700140507     c                   else
023800960109     C                   END
023900960109     C     KTAB1         READE     TABEL00F                               31
024000140507     c                   if           scritto_riga = 'S'
024100960109     C                   SETON                                        01
024200140507     c                   end
024300960109     C                   END
024400940928     C*----------------------------------------------------*
024500960109     C                   END
024600940418     C*----------------------------------------------------*
024700960109     C                   END
024800960109     C*----------------------------------------------------*
024900960109     C                   END
025000960109     C*----------------------------------------------------*
025100940928     C*  Controllo se devo emettere subfile vuoto
025200940928     C     NRR           IFEQ      0
025300940928     C                   MOVEL     '3'           WTPVID
025400940928     C                   ELSE
025500940929     C                   MOVEL     '2'           WTPVID            1
025600940928     C                   END
025700940928     C*----------------------------------------------------*
025800940928     C                   ENDSR
025900940928     C*----------------------------------------------------*
026000940928     C*  CTR02 : Gestione inizial. seconda videata         *
026100940928     C*----------------------------------------------------*
026200940928     C     CTR02         BEGSR
026300940928     C*----------------------------------------------------*
026400940929     C                   MOVEL     'N'           WSELEZ            1
026500960111     C                   SETOFF                                       01
026600960111     C                   READC     TB70S02                                31
026700940928     C     *IN31         DOWEQ     '0'
026800940928     C     *IN28         ANDEQ     '0'
026900940928     C                   SETOFF                                       40
027000940928     C     V2CSEL        IFEQ      '5'
027100960108     C                   MOVEL     V2CSEL        DS7OPZ
027200960108     C                   MOVEL     V2CCOD        DS7COD
027300960111     C                   MOVEL     DSTB70        KPJBU
027400960109     C                   MOVEL     'S'           WSELEZ
027500960111     C                   CALL      'TRTB70R0'
027600960108     C                   PARM                    KPJBA
027700960111     C                   MOVEL     KPJBU         DSTB70
027800940929     C     DS7F03        IFEQ      '1'
027900940929     C                   MOVEL     'S'           WFINE
028000940929     C                   GOTO      FINCT2
028100940929     C                   END
028200940928     C                   END
028300960111     C                   EXSR      CAR7R
028400940928     C     V2CSEL        IFEQ      '1'
028500941107     C                   MOVEL     V2CSEL        DS7OPZ
028600940929     C     DS7GES        IFEQ      'V'
028700940930     C                   MOVEL     V2CCOD        DS7COD
028800941107     C                   MOVEL     V2CELI        DS7ELI
028900941107     C                   MOVEL     V2CINS        DS7INS
029000940930     C                   MOVEL     'S'           WFINE
029100940928     C                   ELSE
029200940929     C                   MOVEL     V2CCOD        DS7COD
029300960111     C                   MOVEL     DSTB70        KPJBU
029400960109     C                   MOVEL     'S'           WSELEZ
029500960111     C                   CALL      'TRTB70R0'
029600960108     C                   PARM                    KPJBA
029700960111     C                   MOVEL     KPJBU         DSTB70
029800940929     C     DS7F03        IFEQ      '1'
029900940929     C                   MOVEL     'S'           WFINE
030000940929     C                   GOTO      FINCT2
030100940929     C                   END
030200960111     C                   EXSR      CAR7R
030300940928     C                   END
030400940929     C                   END
030500940928     C*  Aggiorno Sufile
030600940928     C                   SETON                                        22
030700940928     C     *IN40         IFEQ      '0'
030800940929     C                   MOVEL     ' '           V2CSEL
030900940928     C                   END
031000960111     C                   UPDATE    TB70S02
031100940928     C                   SETOFF                                       22
031200960111     C  N40              READC     TB70S02                                31
031300940928     C                   END
031400940928     C*
031500940929     C     WSELEZ        IFEQ      'S'
031600941003     C     DS7RIC        IFEQ      ' '
031700940929     C                   EXSR      INZ01
031800941003     C                   ELSE
031900941107     C     DS7OPZ        IFNE      '1'
032000941104     C                   MOVEL     *BLANKS       DS7COD
032100940929     C                   END
032200941104     C                   END
032300941003     C                   END
032400940929     C*
032500940929     C     FINCT2        ENDSR
032600960111     C*--------------------------------------------------------------*
032700960111     C*  CAR7R:  Caricamento tabella 7R                              *
032800960111     C*--------------------------------------------------------------*
032900960111     C     CAR7R         BEGSR
033000960111     C*
033100960111     C                   MOVEL     '7R'          KCOD
033200960111     C                   Z-ADD     0             X                 3 0
033300960111     C                   MOVEA     *BLANKS       C7R
033400960111     C                   MOVEA     *BLANKS       D7R
033500960111     C     KTAB1         CHAIN     TABEL00F                           31
033600960111     C     *IN31         DOWEQ     '0'
033700960111     C     TBLFLG        IFEQ      ' '
033800960111     C                   ADD       1             X
033900960111     C                   MOVEL     TBLKEY        C7R(X)
034000960111     C                   MOVEL     TBLUNI        D7R(X)
034100960111     C                   END
034200960111     C     KTAB1         READE     TABEL00F                               31
034300960111     C                   END
034400960111     C                   Z-ADD     X             Z                 3 0
034500960111     C                   ENDSR
034600931209     C*--------------------------------------------------------------*
034700931209     C*  *INZSR: Operazioni iniziali                                 *
034800931209     C*--------------------------------------------------------------*
034900931209     C     *INZSR        BEGSR
035000931209     C*
035100931209     C     *ENTRY        PLIST
035200931209     C                   PARM                    KPJBA
035300960111     C                   MOVEL     KPJBU         DSTB70
035400960112     C     DSTB70        IFEQ      *BLANKS
035500960112     C                   CLEAR                   DSTB70
035600960112     C                   END
035700931209     C*----------------------------------------------------*
035800931209     C*  Richiamo XPARUT
035900931209     C                   Z-ADD     1             CODUT
036000931209     C                   CALL      'XPARUT'
036100931209     C                   PARM                    UTEDSE
036200940927     C                   MOVEL     RAGUT         V1CRSU
036300931209     C*----------------------------------------------------*
036400931209     C* Definizione chiavi                                 *
036500940927     C     KTAB1         KLIST
036600940927     C                   KFLD                    KKUT
036700940927     C                   KFLD                    KCOD
036800940928     C*
036900940928     C     KTAB2         KLIST
037000940928     C                   KFLD                    KKUT
037100940928     C                   KFLD                    KCOD
037200940928     C                   KFLD                    KKEY
037300931209     C*----------------------------------------------------*
037400931209     C* Definizione variabili                              *
037500940927     C     *LIKE         DEFINE    TBLKUT        KKUT
037600940927     C     *LIKE         DEFINE    TBLCOD        KCOD
037700940928     C     *LIKE         DEFINE    TBLKEY        KKEY
037800971210     C******************************************************
037900050503     C* Richiamo pgm per controllo modificabilità tabella SE NON EDP
038000050503     c                   if        %subst(knmus:1:3) <> 'EDP'
038100980225     C     DS7GES        IFEQ      *BLANKS
038200971210     C                   CLEAR                   TRUL12
038300971210     C                   MOVEL     'L'           I12TLA
038400971210     C                   MOVEL     KNSIF         I12NSI
038500971210     C                   MOVEL     '7R'          I12COD
038600971210     C                   CALL      'TRUL12R'
038700971210     C                   PARM                    TRUL12
038800971210     C     O12ERR        IFEQ      '1'
038900971210     C                   GOTO      FINE
039000971210     C                   END
039100980225     C                   END
039200050503     c                   endif
039300971210     C******************************************************
039400940927     C*  Inizializzazione variabili
039500940928     C                   Z-ADD     1             KKUT
039600940927     C                   MOVEL     'N'           WFINE             1
039700940927     C                   MOVEL     *BLANKS       V1DFUN
039800940928     C                   SETOFF                                       90
039900960111     C*----------------------------------------------------*
040000960111     C*  carico tabella particolarità consegne in schiera
040100960111     C                   EXSR      CAR7R
040200960112     C*----------------------------------------------------*
040300960112     C     DS7KSC        IFNE      0
040400960112     C                   MOVEL     '7U'          KCOD
040500960112     C                   MOVEL     DS7KSC        KKEY
040600960112     C                   MOVE      'R'           KKEY
040700960112     C                   MOVEA     *BLANKS       CODP
040800960112     C     KTAB2         CHAIN     TABEL00F                           31
040900960112     C     *IN31         IFEQ      '0'
041000960112     C     TBLFLG        ANDEQ     ' '
041100960112     C                   MOVEA     TBLUNI        CODP
041200960112     C                   MOVEL     TBLUNI        DS7CDP
041300960112     C                   END
041400960112     C                   END
041500940927     C*----------------------------------------------------*
041600940928     C*  Se il programma non è richiamato inizializzo DS
041700940928     C*  parametri ed emetto prima videata
041800940929     C     DS7RIC        IFEQ      ' '
041900960111     C                   CLEAR                   DSTB70
042000940927     C                   EXSR      INZ01
042100940928     C*----------------------------------------------------*
042200940927     C                   ELSE
042300940928     C*----------------------------------------------------*
042400940929     C     DS7GES        COMP      'V'                                    90
042500940929     C                   MOVEL     ' '           DS7F03
042600940928     C*  Se il programma è richiamato e ho il codice particolarità
042700940928     C*  impostato richiamo pgm. successivo
042800940929     C     DS7COD        IFNE      *BLANKS
042900960111     C                   MOVEL     DSTB70        KPJBU
043000960111     C                   CALL      'TRTB70R0'
043100960108     C                   PARM                    KPJBA
043200960111     C                   MOVEL     KPJBU         DSTB70
043300960111     C                   EXSR      CAR7R
043400940929     C                   MOVEL     'S'           WFINE
043500940928     C*----------------------------------------------------*
043600940928     C* ... altrimenti controllo se ho impostato il codice cliente
043700960111     C* e se si provvedo al caricamento delle particolarità cons.
043800940928     C* a cui è abilitato
043900940928     C                   ELSE
044000940928     C*----------------------------------------------------*
044100940928     C*  ... emetto subfile selezione particolarità.
044200080826     c*  solo se non devo restituire la skiera dei codici a cui abilitato
044300080826     c*  il cliente e basta
044400080826     c                   if        ds7ges<>'O'
044500940928     C                   EXSR      INZ02
044600080826     c                   else
044700080826     C                   MOVEL     'S'           WFINE
044800080826     C                   endif
044900080826     C                   END
045000940928     C                   END
045100940228     C*----------------------------------------------------*
045200931209     C                   ENDSR
045300931209     O*-----------------------------------------------------*
045400940927**
045500960111Immettere codice particolarità consegne                               01
045600940419**
045700940927INTERROGAZIONE
