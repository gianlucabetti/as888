000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000300931209     F*  KL        RITORNO                                           *
000400940928     F*  KC        FINE LAVORO                                       *
000500891030     F*--------------------------------------------------------------*
000600960111     FTRTB70D   CF   E             WORKSTN
000700960111     F                                     SFILE(TB70S02:NRR)
000800940418     F*----------
000900141001     FTABEL00F  IF   E           K DISK
001000940418     D*--------------------------------------------------------------*
001100940418     D*   SCHIERE                                                    *
001200940701     D*--------------------------------------------------------------*
001300940930     D ERR             S             70    DIM(1) CTDATA PERRCD(1)
001400940928     D COST            S             16    DIM(1) CTDATA PERRCD(1)
001500960111     D CODP            S              2    DIM(30)
001600141118     D A7R             S              1    DIM(100)
001700960111     D C7R             S              2    DIM(100)
001800960111     D D7R             S             89    DIM(100)
001900940928     D*--------------------------------------------------------------*
002000940928     D*  DS                                                          *
002100940928     D*--------------------------------------------------------------*
002200960111     D DSTB70        E DS                  EXTNAME(DS7PQRS)
002300080227     D DS7V          E DS
002400960111     D DS7R          E DS
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
006100960111     C                   MOVEL     DSTB70        KPJBU
006200931209     C                   SETON                                        LR
006300940928     C*---------------------------------------------------*
006400940928     C*  GESD01: Gestione prima videata                   *
006500940928     C*---------------------------------------------------*
006600940928     C     GESD01        BEGSR
006700940928     C*---------------------------------------------------*
006800931209     C*  Emissione videata
006900940929     C                   MOVEL     *BLANKS       V1DFUN
007000960111     C                   WRITE     TB70T01
007100960111     C                   EXFMT     TB70D01
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
009200960111     C                   MOVEL     DSTB70        KPJBU
009300960111     C                   CALL      'TRTB70R0'
009400960110     C                   PARM                    KPJBA
009500960111     C                   MOVEL     KPJBU         DSTB70
009600940929     C     DS7F03        IFEQ      '1'
009700940929     C                   MOVEL     'S'           WFINE
009800940929     C                   ELSE
009900960111     C                   EXSR      CAR7R
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
011600960111     C                   WRITE     TB70Z02
011700960111     C                   EXFMT     TB70C02
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
012900960112     C     *INKL         IFEQ      '1'
013000960112     C     DS7RIC        IFEQ      ' '
013100960112     C                   EXSR      INZ01
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
014900960111     C                   WRITE     TB70D02
015000960111     C                   EXFMT     TB70C02
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
019400960111     C                   WRITE     TB70C02
019500940929     C                   Z-ADD     0             NRR               4 0
019600940927     C                   SETON                                        21
019700940927     C*----------------------------------------------------*
019800940927     C*  Caricamento subfile
019900960109     C                   Z-ADD     1             X
020000960109     C                   MOVEL     '7V'          KCOD
020100940929     C                   MOVEL     *BLANKS       V2CSEL
020200960109     C                   DO        Z             X
020300141118     c                   if        A7R(x) = *blank
020400960109      *  Costruisco chiave per posizionamento su tabel
020500960111     C                   MOVEL     'R'           WTPPAR
020600960111     C                   MOVEL     C7R(X)        WCDPAR
020700960109     C                   MOVEL     'V'           WTPELE
020800960109     C                   Z-ADD     0             WPROGG
020900960110     C                   MOVEL     WKEY02        KKEY
021000960109     C                   SETOFF                                       01
021100960109     C     KTAB2         SETLL     TABEL00F
021200960109     C     KTAB1         READE     TABEL00F                               31
021300960109     C     *IN31         IFEQ      '0'
021400960109     C     WSVKEY        ANDEQ     WKEY01
021500960109     C*----------------------------------------------------*
021600960109     C*  Se immesso codice cliente prima di caricare particol.
021700960109     C*  controllo se il cliente è abilitato
021800960109     C                   SETON                                        32
021900960111     C                   MOVEL     D7R(X)        DS7R
022000960109     C     DS7KSC        IFNE      0
022100960111     C     §7RABC        ANDEQ     'S'
022200960111     C     C7R(X)        LOOKUP    CODP                                   32
022300960109     C                   END
022400960109     C*----------------------------------------------------*
022500960109     C     *IN32         IFEQ      '1'
022600960111     C                   MOVEL     C7R(X)        V2CCOD
022700960111     C                   MOVEL     §7RELI        V2CELI
022800960111     C                   MOVEL     §7RINS        V2CINS
022900940927     C     *IN31         DOWEQ     '0'
023000960109     C     WSVKEY        ANDEQ     WKEY01
023100140507      * se la prima riga è stata sblankata non permetterebbe di poter
023200140507      * selezionare la tabella anche se vi fossero successive righe cariche
023300140507     c                   clear                   scritto_riga      1
023400960109     C     TBLUNI        IFNE      *BLANKS
023500960109     C                   MOVEL     TBLUNI        V2DCOD
023600940928     C                   ADD       1             NRR
023700960111     C                   WRITE     TB70S02
023800140507     c                   movel     'S'           scritto_riga
023900140507     c                   else
024000960109     C                   END
024100960109     C     KTAB1         READE     TABEL00F                               31
024200140507     c                   if           scritto_riga = 'S'
024300960109     C                   SETON                                        01
024400140507     c                   end
024500960109     C                   END
024600940928     C*----------------------------------------------------*
024700960109     C                   END
024800940418     C*----------------------------------------------------*
024900960109     C                   END
025000960109     C*----------------------------------------------------*
025100141118     c                   endif
025200960109     C                   END
025300960109     C*----------------------------------------------------*
025400940928     C*  Controllo se devo emettere subfile vuoto
025500940928     C     NRR           IFEQ      0
025600940928     C                   MOVEL     '3'           WTPVID
025700940928     C                   ELSE
025800940929     C                   MOVEL     '2'           WTPVID            1
025900940928     C                   END
026000940928     C*----------------------------------------------------*
026100940928     C                   ENDSR
026200940928     C*----------------------------------------------------*
026300940928     C*  CTR02 : Gestione inizial. seconda videata         *
026400940928     C*----------------------------------------------------*
026500940928     C     CTR02         BEGSR
026600940928     C*----------------------------------------------------*
026700940929     C                   MOVEL     'N'           WSELEZ            1
026800141118     C                   clear                   Fatta_Copia       1
026900960111     C                   SETOFF                                       01
027000960111     C                   READC     TB70S02                                31
027100940928     C     *IN31         DOWEQ     '0'
027200940928     C     *IN28         ANDEQ     '0'
027300940928     C                   SETOFF                                       40
027400141001      *
027500141001     C     V2CSEL        IFEQ      '3'
027600141118     C                   movel     'S'           Fatta_Copia
027700141001     c                   eval      cpyDAcod = v2cCOD
027800141003     c                   eval      cpyDAtrs = 'S'
027900141001     C                   ExsR      Wind_COPIA
028000141001     C                   EXSR      CAR7R
028100141001     C                   END
028200141001      *
028300940928     C     V2CSEL        IFEQ      '5'
028400960108     C                   MOVEL     V2CSEL        DS7OPZ
028500960108     C                   MOVEL     V2CCOD        DS7COD
028600960111     C                   MOVEL     DSTB70        KPJBU
028700960109     C                   MOVEL     'S'           WSELEZ
028800960111     C                   CALL      'TRTB70R0'
028900960108     C                   PARM                    KPJBA
029000960111     C                   MOVEL     KPJBU         DSTB70
029100940929     C     DS7F03        IFEQ      '1'
029200940929     C                   MOVEL     'S'           WFINE
029300940929     C                   GOTO      FINCT2
029400940929     C                   END
029500940928     C                   END
029600960111     C                   EXSR      CAR7R
029700141001      *
029800940928     C     V2CSEL        IFEQ      '1'
029900941107     C                   MOVEL     V2CSEL        DS7OPZ
030000940929     C     DS7GES        IFEQ      'V'
030100940930     C                   MOVEL     V2CCOD        DS7COD
030200941107     C                   MOVEL     V2CELI        DS7ELI
030300941107     C                   MOVEL     V2CINS        DS7INS
030400940930     C                   MOVEL     'S'           WFINE
030500940928     C                   ELSE
030600940929     C                   MOVEL     V2CCOD        DS7COD
030700960111     C                   MOVEL     DSTB70        KPJBU
030800960109     C                   MOVEL     'S'           WSELEZ
030900960111     C                   CALL      'TRTB70R0'
031000960108     C                   PARM                    KPJBA
031100960111     C                   MOVEL     KPJBU         DSTB70
031200940929     C     DS7F03        IFEQ      '1'
031300940929     C                   MOVEL     'S'           WFINE
031400940929     C                   GOTO      FINCT2
031500940929     C                   END
031600960111     C                   EXSR      CAR7R
031700940928     C                   END
031800940929     C                   END
031900940928     C*  Aggiorno Sufile
032000940928     C                   SETON                                        22
032100940928     C     *IN40         IFEQ      '0'
032200940929     C                   MOVEL     ' '           V2CSEL
032300940928     C                   END
032400960111     C                   UPDATE    TB70S02
032500940928     C                   SETOFF                                       22
032600960111     C  N40              READC     TB70S02                                31
032700940928     C                   END
032800940928     C*
032900141118     C* se si è fatta una copia ricarica il SFL di selezione
033000141118     C     Fatta_Copia   IFEQ      'S'
033100141118     C                   EXSR      inz02
033200141118     C                   END
033300141118     C*
033400940929     C     WSELEZ        IFEQ      'S'
033500941003     C     DS7RIC        IFEQ      ' '
033600940929     C                   EXSR      INZ01
033700941003     C                   ELSE
033800941107     C     DS7OPZ        IFNE      '1'
033900941104     C                   MOVEL     *BLANKS       DS7COD
034000940929     C                   END
034100941104     C                   END
034200941003     C                   END
034300940929     C*
034400940929     C     FINCT2        ENDSR
034500960111     C*--------------------------------------------------------------*
034600141001     C*  Funzione di copia tabelle 7R,7V e PXA                       *
034700960111     C*--------------------------------------------------------------*
034800141001     C     Wind_COPIA    BEGSR
034900141001     C*
035000141001     c     Video_CPY     tag
035100141001     c                   exfmt     TB70cpy
035200141001      *-- F12
035300141001     c   KL              goto      end_Copy
035400141001      * Unico Controllo:
035500141001      *   Il nuovo codice NON deve esistere o non deve essere *blk
035600141001     c                   z-add     1             nr                3 0
035700141001     c     cpyAcod       lookup    c7R(nr)                                77
035800141001      *
035900141001     c  nKF
036000141001     cOR 77              goto      Video_CPY
036100141001     C*
036200141001     c                   exsr      Copia_Tabelle
036300141001     C*
036400141001     C     end_copy      ENDSR
036500141001     C*--------------------------------------------------------------*
036600141001     C*  Copia le Tabelle - esegue la scrittura                      *
036700141001     C*--------------------------------------------------------------*
036800141001     C     Copia_Tabelle BEGSR
036900141001      *
037000141001      * un pgm esterno duplica le tabelle 7R, 7V e PXA
037100141001     c                   eval      daCodice = cpyDAcod
037200141001     c                   eval       aCodice =  cpyAcod
037300141003     c                   eval      daTrasm  = cpyDAtrs
037400141001     c                   call      'TRTB70R2'
037500141001     c                   parm                    daCodice          2
037600141001     c                   parm                    aCodice           2
037700141003     c                   parm                    daTrasm           1
037800141001      *
037900141001     C     end_wrt       ENDSR
038000141001     C*--------------------------------------------------------------*
038100141001     C*  CAR7R:  Caricamento tabella 7R                              *
038200141001     C*--------------------------------------------------------------*
038300141001     C     CAR7R         BEGSR
038400141001     C*
038500960111     C                   MOVEL     '7R'          KCOD
038600960111     C                   Z-ADD     0             X                 3 0
038700960111     C                   MOVEA     *BLANKS       C7R
038800960111     C                   MOVEA     *BLANKS       D7R
038900960111     C     KTAB1         CHAIN     TABEL00F                           31
039000960111     C     *IN31         DOWEQ     '0'
039100141118     C**** TBLFLG        IFEQ      ' '
039200960111     C                   ADD       1             X
039300141118     C                   MOVEL     TBLFLG        A7R(X)
039400960111     C                   MOVEL     TBLKEY        C7R(X)
039500960111     C                   MOVEL     TBLUNI        D7R(X)
039600141118     C*****              END
039700960111     C     KTAB1         READE     TABEL00F                               31
039800960111     C                   END
039900960111     C                   Z-ADD     X             Z                 3 0
040000960111     C                   ENDSR
040100931209     C*--------------------------------------------------------------*
040200931209     C*  *INZSR: Operazioni iniziali                                 *
040300931209     C*--------------------------------------------------------------*
040400931209     C     *INZSR        BEGSR
040500931209     C*
040600931209     C     *ENTRY        PLIST
040700931209     C                   PARM                    KPJBA
040800960111     C                   MOVEL     KPJBU         DSTB70
040900960112     C     DSTB70        IFEQ      *BLANKS
041000960112     C                   CLEAR                   DSTB70
041100960112     C                   END
041200931209     C*----------------------------------------------------*
041300931209     C*  Richiamo XPARUT
041400931209     C                   Z-ADD     1             CODUT
041500931209     C                   CALL      'XPARUT'
041600931209     C                   PARM                    UTEDSE
041700940927     C                   MOVEL     RAGUT         V1CRSU
041800931209     C*----------------------------------------------------*
041900931209     C* Definizione chiavi                                 *
042000940927     C     KTAB1         KLIST
042100940927     C                   KFLD                    KKUT
042200940927     C                   KFLD                    KCOD
042300940928     C*
042400940928     C     KTAB2         KLIST
042500940928     C                   KFLD                    KKUT
042600940928     C                   KFLD                    KCOD
042700940928     C                   KFLD                    KKEY
042800931209     C*----------------------------------------------------*
042900931209     C* Definizione variabili                              *
043000940927     C     *LIKE         DEFINE    TBLKUT        KKUT
043100940927     C     *LIKE         DEFINE    TBLCOD        KCOD
043200940928     C     *LIKE         DEFINE    TBLKEY        KKEY
043300971210     C******************************************************
043400050503     C* Richiamo pgm per controllo modificabilità tabella SE NON EDP
043500050503     c                   if        %subst(knmus:1:3) <> 'EDP'
043600980225     C     DS7GES        IFEQ      *BLANKS
043700971210     C                   CLEAR                   TRUL12
043800971210     C                   MOVEL     'L'           I12TLA
043900971210     C                   MOVEL     KNSIF         I12NSI
044000971210     C                   MOVEL     '7R'          I12COD
044100971210     C                   CALL      'TRUL12R'
044200971210     C                   PARM                    TRUL12
044300971210     C     O12ERR        IFEQ      '1'
044400971210     C                   GOTO      FINE
044500971210     C                   END
044600980225     C                   END
044700050503     c                   endif
044800971210     C******************************************************
044900940927     C*  Inizializzazione variabili
045000940928     C                   Z-ADD     1             KKUT
045100940927     C                   MOVEL     'N'           WFINE             1
045200940927     C                   MOVEL     *BLANKS       V1DFUN
045300940928     C                   SETOFF                                       90
045400960111     C*----------------------------------------------------*
045500960111     C*  carico tabella particolarità consegne in schiera
045600960111     C                   EXSR      CAR7R
045700960112     C*----------------------------------------------------*
045800960112     C     DS7KSC        IFNE      0
045900960112     C                   MOVEL     '7U'          KCOD
046000960112     C                   MOVEL     DS7KSC        KKEY
046100960112     C                   MOVE      'R'           KKEY
046200960112     C                   MOVEA     *BLANKS       CODP
046300960112     C     KTAB2         CHAIN     TABEL00F                           31
046400960112     C     *IN31         IFEQ      '0'
046500960112     C     TBLFLG        ANDEQ     ' '
046600960112     C                   MOVEA     TBLUNI        CODP
046700960112     C                   MOVEL     TBLUNI        DS7CDP
046800960112     C                   END
046900960112     C                   END
047000940927     C*----------------------------------------------------*
047100940928     C*  Se il programma non è richiamato inizializzo DS
047200940928     C*  parametri ed emetto prima videata
047300940929     C     DS7RIC        IFEQ      ' '
047400960111     C                   CLEAR                   DSTB70
047500940927     C                   EXSR      INZ01
047600940928     C*----------------------------------------------------*
047700940927     C                   ELSE
047800940928     C*----------------------------------------------------*
047900940929     C     DS7GES        COMP      'V'                                    90
048000940929     C                   MOVEL     ' '           DS7F03
048100940928     C*  Se il programma è richiamato e ho il codice particolarità
048200940928     C*  impostato richiamo pgm. successivo
048300940929     C     DS7COD        IFNE      *BLANKS
048400960111     C                   MOVEL     DSTB70        KPJBU
048500960111     C                   CALL      'TRTB70R0'
048600960108     C                   PARM                    KPJBA
048700960111     C                   MOVEL     KPJBU         DSTB70
048800960111     C                   EXSR      CAR7R
048900940929     C                   MOVEL     'S'           WFINE
049000940928     C*----------------------------------------------------*
049100940928     C* ... altrimenti controllo se ho impostato il codice cliente
049200960111     C* e se si provvedo al caricamento delle particolarità cons.
049300940928     C* a cui è abilitato
049400940928     C                   ELSE
049500940928     C*----------------------------------------------------*
049600940928     C*  ... emetto subfile selezione particolarità.
049700080826     c*  solo se non devo restituire la skiera dei codici a cui abilitato
049800080826     c*  il cliente e basta
049900080826     c                   if        ds7ges<>'O'
050000940928     C                   EXSR      INZ02
050100080826     c                   else
050200080826     C                   MOVEL     'S'           WFINE
050300080826     C                   endif
050400080826     C                   END
050500940928     C                   END
050600940228     C*----------------------------------------------------*
050700931209     C                   ENDSR
050800931209     O*-----------------------------------------------------*
050900940927**
051000960111Immettere codice particolarità consegne                               01
051100940419**
051200940927INTERROGAZIONE
