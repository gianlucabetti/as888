000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000300960109     F*  KC        FINE LAVORO                                       *
000400960109     F*  KF        CONFERMA                                          *
000500960109     F*  KG        DATI STAMPA SU BOLLA                              *
000600960109     F*  KL        RITORNO                                           *
000700891030     F*--------------------------------------------------------------*
000800940927     FTRTB69D   CF   E             WORKSTN
000900960109     F                                     SFILE(TB69S05:NRR5)
001000960109     F                                     SFILE(TB69S06:NRR6)
001100940418     F*----------
001200960110     FTABEL00F  UF A E           K DISK
001300940418     D*--------------------------------------------------------------*
001400940418     D*   SCHIERE                                                    *
001500940701     D*--------------------------------------------------------------*
001600960110     D ERR             S             70    DIM(4) CTDATA PERRCD(1)
001700960109     D COST            S             16    DIM(3) CTDATA PERRCD(1)
001800960109     D CODP            S              2    DIM(30)
001900960109     D C7Q             S              2    DIM(100)
002000960109     D D7Q             S             89    DIM(100)
002100940928     D*--------------------------------------------------------------*
002200940928     D*  DS                                                          *
002300940928     D*--------------------------------------------------------------*
002400080227     D DS7V          E DS
002500960109     D DS7Q          E DS
002600960109     D DSTB69        E DS                  EXTNAME(DS7PQRS)
002700900608      *
002800891030     D KPJBA         E DS
002900911118      *
003000911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003100960108      *  Codice x posizionamento su tabella 7V
003200960108     D                 DS
003300960108     D  WTPPAR                 1      1
003400960108     D  WCDPAR                 2      3
003500960109     D  WKEY00                 1      3
003600960109     D  WTPELE                 4      4
003700960109     D  WKEY01                 1      4
003800960109     D  WPROGG                 5      8  0
003900960109     D  WKEY02                 1      8
004000960109      *  SComposizione KEY di TABEL x 7V
004100960109     D                 DS
004200960109     D  WANKEY                 1      3
004300960109     D  WSVKEY                 1      4
004400960110     D  TBPROG                 5      8  0
004500960109     D  TBLKEY                 1      8
004600940228      *
004700940228     D                SDS
004800960109     D  VIDPGM                 1     10
004900931209     C*--------------------------------------------------------------*
005000931209     C*    C I C L O    P R I N C I P A L E                          *
005100931209     C*--------------------------------------------------------------*
005200960109     C     *ENTRY        PLIST
005300960109     C                   PARM                    KPJBA
005400960109     C                   MOVEL     KPJBU         DSTB69
005500960109     C                   MOVEL     ' '           DS7ERR
005600960109     C                   MOVEL     ' '           DS7F03
005700960109     C                   MOVEL     *BLANKS       DS7MSG
005800960110     C                   MOVEL     'N'           WFINE
005900960110     C                   MOVEL     'N'           WPGMIN            1
006000960109     C*  Prima volta
006100960109     C     WPRIMA        IFNE      'N'
006200960109     C                   MOVEL     'N'           WPRIMA            1
006300960109     C                   EXSR      DEFCAM
006400960109     C                   END
006500960109     C*  Eseguo controllo del codice
006600960109     C                   EXSR      CTRCOD
006700960109     C*  Eseguo caricamento note descrittive
006800960109     C     WFINE         IFNE      'S'
006900960112     C     DS7GES        ANDNE     'C'
007000960109     C                   EXSR      INZ01
007100960110     C*  Loop gestione varie videate fino a rich. fine elaboraz.
007200960110     C     WPGMIN        DOWEQ     'N'
007300931209     C*  Loop gestione varie videate fino a rich. fine elaboraz.
007400940927     C     WFINE         DOWNE     'S'
007500940927     C*
007600960109     C     WTPVID        CASEQ     '1'           GESD01
007700960109     C     WTPVID        CASEQ     '2'           GESD02
007800960109     C     WTPVID        CASEQ     '3'           GESD03
007900931209     C                   END
008000960110     C                   END
008100960110     C*  Se no fine programma richiamo successive videate
008200960110     C     DS7F03        IFEQ      ' '
008300960110     C                   EXSR      CALL69
008400960110     C                   END
008500960110     C*
008600960110     C                   END
008700960110     C*
008800960109     C                   END
008900931209     C* Fine programma
009000931209     C     FINE          TAG
009100940928     C                   MOVEL     DSTB69        KPJBU
009200960110     C* Libero tabel
009300960110     C     *LOVAL        SETLL     TABEL00F
009400960109     C* Se il programma è richiamato da altre funzioni chiudo in LR
009500960109     C* altrimenti chiudo in RT
009600960109     C     DS7RIC        IFEQ      'S'
009700960109     C                   SETON                                        LR
009800960109     C                   ELSE
009900960109     C                   SETON                                        RT
010000960109     C                   END
010100940928     C*---------------------------------------------------*
010200960109     C*  GESD01: Gestione videata descrizioni             *
010300940928     C*---------------------------------------------------*
010400940928     C     GESD01        BEGSR
010500940928     C*---------------------------------------------------*
010600931209     C*  Emissione videata
010700960109     C                   SETON                                        20
010800960109     C                   WRITE     TB69Z05
010900960109     C                   EXFMT     TB69C05
011000960109     C                   SETOFF                                       28
011100940418     C                   MOVEL     *BLANKS       $MSG
011200940928     C*---------------------------------------------------*
011300931209     C*  Fine elaborazione
011400931209     C     *INKC         IFEQ      '1'
011500940929     C                   MOVEL     'S'           WFINE
011600960109     C                   MOVEL     '1'           DS7F03
011700960110     C                   MOVEL     'S'           WPGMIN            1
011800931209     C                   GOTO      FINVD1
011900931209     C                   END
012000960109     C*---------------------------------------------------*
012100960109     C*  Ritorno videata precedente
012200960109     C     *INKL         IFEQ      '1'
012300960109     C                   MOVEL     'S'           WFINE
012400960110     C                   MOVEL     '2'           DS7F03
012500960110     C                   MOVEL     'S'           WPGMIN            1
012600960109     C                   GOTO      FINVD1
012700960109     C                   END
012800960109     C*---------------------------------------------------*
012900960109     C*  Richiamo videata gestione dati stampa su bolla
013000960109     C     *INKG         IFEQ      '1'
013100960109     C                   EXSR      INZ02
013200960109     C                   GOTO      FINVD1
013300960109     C                   END
013400960109     C*---------------------------------------------------*
013500960112     C*---------------------------------------------------*
013600960112     C*  X visualiz.con invio passo alla videata successiva
013700960112     C     DS7GES        IFEQ      'V'
013800960112     C     DS7OPZ        OREQ      '5'
013900021010     C                   CLEAR                   DS7F03
014000960112     C                   MOVEL     'S'           WFINE
014100960112     C                   END
014200960112     C*---------------------------------------------------*
014300960109     C*  Richiamo videata gestione dati stampa su bolla
014400960109     C     *INKF         IFEQ      '1'
014500960109     C                   EXSR      WRTDES
014600960109     C                   END
014700931209     C*--------------------------------------------------------------*
014800940928     C     FINVD1        ENDSR
014900940928     C*--------------------------------------------------------------*
015000960109     C*  GESD02: Gestione subfile dati stampa bolla                  *
015100940928     C*--------------------------------------------------------------*
015200940928     C     GESD02        BEGSR
015300940928     C*----------------------------------------------------*
015400940928     C*  Emissione videata
015500960109     C                   SETON                                        20
015600960109     C                   WRITE     TB69Z06
015700960109     C                   EXFMT     TB69C06
015800940928     C                   SETOFF                                       2840
015900940928     C                   MOVEL     *BLANKS       $MSG
016000940928     C*----------------------------------------------------*
016100960109     C*  Fine elaborazione
016200960109     C     *INKC         IFEQ      '1'
016300960110     C                   MOVEL     'S'           WPGMIN            1
016400960109     C                   MOVEL     'S'           WFINE
016500960109     C                   MOVEL     '1'           DS7F03
016600960109     C                   GOTO      FINVD2
016700960109     C                   END
016800960109     C*---------------------------------------------------*
016900960109     C*  Ritorno videata precedente
017000960109     C     *INKL         IFEQ      '1'
017100960110     C                   MOVEL     '1'           WTPVID            1
017200960110     C                   GOTO      FINVD2
017300960109     C                   END
017400960109     C*---------------------------------------------------*
017500960109     C*  Richiamo videata gestione dati stampa su bolla
017600960109     C     *INKF         IFEQ      '1'
017700960109     C                   EXSR      WRTTES
017800960109     C                   MOVEL     '1'           WTPVID
017900960109     C                   END
018000940929     C*----------------------------------------------------*
018100940928     C     FINVD2        ENDSR
018200960109     C*--------------------------------------------------------------*
018300960109     C*  GESD03: Gestione subfile vuoto dati stampa bolla            *
018400960109     C*--------------------------------------------------------------*
018500960109     C     GESD03        BEGSR
018600960109     C*----------------------------------------------------*
018700960109     C*  Emissione videata
018800960110     C                   SETOFF                                       20
018900960109     C                   WRITE     TB69D06
019000960109     C                   EXFMT     TB69C06
019100960109     C                   SETOFF                                       2840
019200960110     C                   MOVEL     '1'           WTPVID
019300960109     C*----------------------------------------------------*
019400960109     C*  Fine elaborazione
019500960109     C     *INKC         IFEQ      '1'
019600960110     C                   MOVEL     'S'           WPGMIN            1
019700960110     C                   MOVEL     '1'           DS7F03
019800960109     C                   MOVEL     'S'           WFINE
019900960109     C                   END
020000960109     C*---------------------------------------------------*
020100960109     C                   ENDSR
020200960109     C*----------------------------------------------------*
020300960109     C*  CTRCOD: Controllo codice particolarità giacenza   *
020400960109     C*----------------------------------------------------*
020500960109     C     CTRCOD        BEGSR
020600960109     C*
020700960109     C                   MOVEL     *BLANKS       DS7INS
020800960109     C                   MOVEL     *BLANKS       DS7ELI
020900960109     C                   Z-ADD     1             KKUT
021000960109     C                   MOVEL     '7Q'          KCOD
021100960109     C                   MOVEL     *BLANKS       KKEY
021200960109     C                   MOVEL     DS7COD        KKEY
021300960209     C                   MOVEL     DS7COD        V5CCOD
021400960209     C                   MOVEL     DS7COD        V6CCOD
021500960109     C                   MOVEL     ' '           DS7ERR
021600960109     C                   CLEAR                   DS7Q
021700960109     C     KTAB2         CHAIN     TABEL00F                           31
021800960109     C*----------------------------------------------------*
021900960110     C     DS7RIC        IFEQ      'S'
022000960109     C*----------------------------------------------------*
022100960109     C     *IN31         IFEQ      '1'
022200960110     C                   MOVEL     ERR(2)        DS7MSG
022300960109     C                   MOVEL     '1'           DS7ERR
022400960109     C                   MOVEL     'S'           WFINE
022500960109     C                   GOTO      FINCTR
022600960109     C                   END
022700960109     C*  Se pgm richiamato x controllo verifico se cod.annull.
022800960109     C     *IN31         IFEQ      '0'
022900960109     C     TBLFLG        ANDNE     ' '
023000960109     C     DS7GES        ANDEQ     'C'
023100960109     C                   MOVEL     'S'           WFINE
023200960110     C                   MOVEL     ERR(3)        DS7MSG
023300960109     C                   MOVEL     '1'           DS7ERR
023400960109     C                   GOTO      FINCTR
023500960109     C                   END
023600960109     C*----------------------------------------------------*
023700960109     C*  Se devo controllare abilitazione di un cod.cliente
023800960109     C*  carico i dati dalla 7U
023900960109     C                   MOVEL     TBLUNI        DS7Q
024000960109     C                   MOVEL     §7QELI        DS7ELI
024100960109     C                   MOVEL     §7QINS        DS7INS
024200960109     C     DS7KSC        IFNE      0
024300960109     C     §7QABC        ANDEQ     'S'
024400960109     C                   MOVEA     DS7CDP        CODP
024500960109     C     V5CCOD        LOOKUP    CODP                                   32
024600960109     C     *IN32         IFEQ      '0'
024700960109     C                   MOVEL     'S'           WFINE
024800960110     C                   MOVEL     ERR(4)        DS7MSG
024900960109     C                   MOVEL     '1'           DS7ERR
025000960109     C                   END
025100960109     C                   END
025200960109     C*----------------------------------------------------*
025300960109     C                   ELSE
025400960109     C*  Se non è richiamato controllo se sono in immissione
025500960109     C     *IN31         IFEQ      '1'
025600960110     C                   MOVEL     COST(1)       V5DFUN
025700960109     C                   ELSE
025800960110     C                   MOVEL     COST(2)       V5DFUN
025900960109     C                   END
026000960109     C*----------------------------------------------------*
026100960109     C                   END
026200960109     C*----------------------------------------------------*
026300960109     C     FINCTR        ENDSR
026400940928     C*----------------------------------------------------*
026500940928     C*  INZ01   Gestione inizializzazione prima videata   *
026600940928     C*----------------------------------------------------*
026700940927     C     INZ01         BEGSR
026800960109     C*  Inizializzo videata subfile note descrittive
026900940929     C                   MOVEL     '1'           WTPVID
027000960109     C                   SETOFF                                       2890
027100940418     C                   MOVEL     *BLANKS       $MSG
027200940928     C*----------------------------------------------------*
027300960109     C*  Controllo se gestisco visulizzazione
027400940929     C     DS7GES        IFEQ      'V'
027500960109     C     DS7OPZ        OREQ      '5'
027600960109     C                   SETON                                        90
027700960109     C                   MOVEL     COST(3)       V5DFUN
027800940928     C                   END
027900940927     C*----------------------------------------------------*
028000940927     C*  Inizializzazione subfile
028100940927     C                   SETOFF                                       2021
028200960109     C                   WRITE     TB69C05
028300960109     C                   Z-ADD     0             NRR5              4 0
028400940927     C                   SETON                                        21
028500940927     C*----------------------------------------------------*
028600940927     C*  Caricamento subfile
028700960109     C                   MOVEL     '7V'          KCOD
028800960109      *  Controllo se esitono note da stampare x bolla
028900960109     C                   MOVEL     'Q'           WTPPAR
029000960109     C                   MOVEL     DS7COD        WCDPAR
029100960109     C                   MOVEL     'S'           WTPELE
029200960109     C                   Z-ADD     0             WPROGG
029300960109     C                   MOVEL     WKEY02        KKEY
029400960109     C                   SETOFF                                       94
029500960109     C     KTAB2         SETLL     TABEL00F
029600960109     C     KTAB1         READE     TABEL00F                               31
029700960109     C     *IN31         IFEQ      '0'
029800960109     C     WSVKEY        ANDEQ     WKEY01
029900960109     C                   SETON                                        94
030000960109     C                   END
030100960109      *  Costruisco chiave per posizionamento su tabel
030200960109     C                   MOVEL     'V'           WTPELE
030300960111     C                   Z-ADD     0             WNRDES
030400960111     C                   MOVEL     WKEY02        KKEY
030500960109     C                   SETOFF                                       01
030600960109     C     KTAB2         SETLL     TABEL00F
030700960109     C     KTAB1         READE     TABEL00F                               31
030800960109     C     *IN31         IFEQ      '0'
030900960109     C     WSVKEY        ANDEQ     WKEY01
031000960109     C*----------------------------------------------------*
031100940927     C     *IN31         DOWEQ     '0'
031200960109     C     WSVKEY        ANDEQ     WKEY01
031300960109     C     TBLUNI        IFNE      *BLANK
031400080227     C*******            MOVEL     TBLUNI        V5DCOD
031500080227     C                   MOVEL     TBLUNI        ds7V
031600080227     C                   MOVEL     §7Vnot        V5dCOD
031700960109     C                   MOVEL     TBLKEY        V5CKEY
031800960109     C                   ADD       1             NRR5
031900960109     C     TBLFLG        COMP      ' '                                929293
032000960109     C                   WRITE     TB69S05
032100960109     C                   END
032200960111     C                   Z-ADD     TBPROG        WNRDES            4 0
032300960109     C     KTAB1         READE     TABEL00F                               31
032400960109     C                   SETON                                        01
032500960109     C                   END
032600940928     C*----------------------------------------------------*
032700960109     C                   END
032800960109     C*----------------------------------------------------*
032900960109     C*  Se il programma è richiamato in aggiornamento
033000960109     C*  carico una pagina di righe vuote
033100960109     C     DS7GES        IFEQ      ' '
033200960110     C     DS7OPZ        ANDNE     '5'
033300960109     C                   EXSR      ROLDES
033400960109     C                   END
033500960109     C*----------------------------------------------------*
033600940928     C                   ENDSR
033700960109     C*----------------------------------------------------*
033800960109     C*  INZ02   Gestione inizializzazione seconda videata *
033900960109     C*----------------------------------------------------*
034000960109     C     INZ02         BEGSR
034100960109     C*  Inizializzo videata subfile note descrittive
034200960110     C                   MOVEL     '2'           WTPVID
034300960109     C                   SETOFF                                       2890
034400960109     C                   MOVEL     *BLANKS       $MSG
034500960112     C*----------------------------------------------------*
034600960112     C*  Controllo se gestisco visulizzazione
034700960112     C     DS7GES        IFEQ      'V'
034800960112     C     DS7OPZ        OREQ      '5'
034900960112     C                   SETON                                        90
035000960112     C                   MOVEL     COST(3)       V5DFUN
035100960112     C                   END
035200960109     C*----------------------------------------------------*
035300960109     C*  Inizializzazione subfile
035400960109     C                   SETOFF                                       2021
035500960109     C                   WRITE     TB69C06
035600960109     C                   Z-ADD     0             NRR6              4 0
035700960109     C                   SETON                                        21
035800960109     C                   MOVEL     V5DFUN        V6DFUN
035900960109     C*----------------------------------------------------*
036000960109     C*  Caricamento subfile
036100960109     C                   MOVEL     '7V'          KCOD
036200960109      *  Costruisco chiave per posizionamento su tabel
036300960109     C                   MOVEL     'Q'           WTPPAR
036400960109     C                   MOVEL     DS7COD        WCDPAR
036500960109     C                   MOVEL     'S'           WTPELE
036600960109     C                   Z-ADD     0             WPROGG
036700960109     C                   MOVEL     WKEY02        KKEY
036800960111     C                   Z-ADD     0             WNRTES
036900960109     C                   SETOFF                                       01
037000960109     C     KTAB2         SETLL     TABEL00F
037100960109     C     KTAB1         READE     TABEL00F                               31
037200960109     C     *IN31         IFEQ      '0'
037300960109     C     WSVKEY        ANDEQ     WKEY01
037400960109     C*----------------------------------------------------*
037500960109     C     *IN31         DOWEQ     '0'
037600960109     C     WSVKEY        ANDEQ     WKEY01
037700960109     C     TBLUNI        IFNE      *BLANK
037800960109     C                   MOVEL     TBLKEY        V6CKEY
037900080228     C                   MOVEL     TBLUNI        ds7V
038000080228     C                   MOVEL     §7vNOT        V6DSTP
038100080228     C                   MOVEL     §7Vpda        V6PDA
038200960109     C                   ADD       1             NRR6
038300960109     C     TBLFLG        COMP      ' '                                929293
038400960109     C                   WRITE     TB69S06
038500960109     C                   END
038600960111     C                   Z-ADD     TBPROG        WNRTES            4 0
038700960109     C     KTAB1         READE     TABEL00F                               31
038800960109     C                   SETON                                        01
038900960109     C                   END
039000960109     C*----------------------------------------------------*
039100960109     C                   END
039200960109     C*----------------------------------------------------*
039300960109     C*  Se il programma è richiamato in aggiornamento
039400960109     C*  carico una pagina di righe vuote
039500960109     C     DS7GES        IFEQ      ' '
039600960109     C     DS7OPZ        ANDEQ     '1'
039700960115     C     NRR6          ANDEQ     0
039800960115     C                   EXSR      ROLTES
039900960109     C                   END
040000960110     C*----------------------------------------------------*
040100960110     C     NRR6          IFEQ      0
040200960110     C                   MOVEL     '3'           WTPVID
040300960110     C                   END
040400960109     C*----------------------------------------------------*
040500960109     C                   ENDSR
040600960109     C*----------------------------------------------------*
040700960109     C*  ROLDES: Carico riga di dettaglio                  *
040800960109     C*----------------------------------------------------*
040900960109     C     ROLDES        BEGSR
041000960109     C*
041100960109     C                   DO        17
041200960116     C                   SETOFF                                       92
041300960109     C                   MOVEL     *BLANKS       V5CKEY
041400960109     C                   MOVEL     *BLANKS       V5DCOD
041500960109     C                   ADD       1             NRR5
041600960109     C                   WRITE     TB69S05
041700960109     C                   END
041800960109     C*
041900960109     C                   ENDSR
042000960109     C*----------------------------------------------------*
042100960109     C*  ROLTES: Carico riga di dettaglio                  *
042200960109     C*----------------------------------------------------*
042300960109     C     ROLTES        BEGSR
042400960109     C*
042500960109     C                   MOVEL     *BLANKS       V6CKEY
042600960109     C                   MOVEL     *BLANKS       V6DSTP
042700080228     C                   MOVEL     *BLANKS       V6pda
042800960109     C                   ADD       1             NRR6
042900960116     C                   SETOFF                                       92
043000960109     C                   WRITE     TB69S06
043100960109     C*
043200960109     C                   ENDSR
043300960109     C*----------------------------------------------------*
043400960109     C*  WRTDES: Gestione scrittura videata descrizioni    *
043500940928     C*----------------------------------------------------*
043600960109     C     WRTDES        BEGSR
043700940928     C*----------------------------------------------------*
043800940929     C                   MOVEL     'N'           WSELEZ            1
043900960110     C                   MOVEL     ' '           DS7F03
044000960109     C                   Z-ADD     1             NRR5
044100960109     C     NRR5          CHAIN     TB69S05                            31
044200940928     C     *IN31         DOWEQ     '0'
044300940928     C     *IN28         ANDEQ     '0'
044400940928     C                   SETOFF                                       40
044500960109     C*  Variazione
044600960109     C     V5CKEY        IFNE      *BLANKS
044700960109     C                   MOVEL     'S'           WSELEZ
044800960109     C                   MOVEL     V5CKEY        KKEY
044900960110     C     KTAB2         CHAIN     TABEL00F                           32
045000960110     C                   MOVEL     '1'           TBLFTT
045100960110     C                   MOVEL     *BLANKS       TBLUNI
045200080227     C****               MOVEL     V5DCOD        TBLUNI
045300080227     C                   MOVEL     V5DCOD        §7Vnot
045400080227     C                   MOVEL     ds7V          TBLUNI
045500960109     C                   MOVEL     *BLANKS       TBLFTR
045600960109     C                   MOVEL     *ZEROS        TBLDTR
045700960109     C     V5DCOD        IFNE      *BLANKS
045800960109     C  N32              UPDATE    TABEL
045900960109     C                   ELSE
046000960109     C*  Se ho abblencato riga gestisco l'annullamento
046100960111     C                   MOVEL     '*'           TBLFLG
046200960110     C                   MOVEL     '1'           TBLFTT
046300960109     C                   MOVEL     *BLANKS       TBLFTR
046400960109     C                   MOVEL     *ZEROS        TBLDTR
046500960109     C  N32              UPDATE    TABEL
046600960109     C                   END
046700960109     C                   ELSE
046800960109     C*  Nuova riga
046900960110     C     V5DCOD        IFNE      *BLANKS
047000141128     c                   clear                   TBLflg
047100960109     C                   MOVEL     'S'           WSELEZ
047200960109     C                   ADD       1             WNRDES
047300960109     C                   Z-ADD     WNRDES        WPROGG
047400960109     C                   MOVEL     'Q'           WTPPAR
047500960109     C                   MOVEL     DS7COD        WCDPAR
047600960110     C                   MOVEL     'V'           WTPELE
047700960109     C                   Z-ADD     1             TBLKUT
047800960109     C                   MOVEL     WKEY02        TBLKEY
047900960109     C                   MOVEL     '7V'          TBLCOD
048000960109     C                   MOVEL     '1'           TBLFTT
048100960110     C                   MOVEL     *BLANKS       TBLUNI
048200080227     C******             MOVEL     V5DCOD        TBLUNI
048300080227     C                   MOVEL     V5DCOD        §7Vnot
048400080227     C                   MOVEL     ds7V          TBLUNI
048500960110     C                   WRITE     TABEL
048600960109     C                   END
048700960109     C                   END
048800960109     C*  Lettura successiva
048900960109     C                   ADD       1             NRR5
049000960109     C     NRR5          CHAIN     TB69S05                            31
049100960109     C                   END
049200960109     C*  Se non ho scritto nessuna nota do errore
049300960109     C     WSELEZ        IFEQ      'N'
049400960109     C                   SETON                                        28
049500960109     C                   MOVEL     ERR(1)        $MSG
049600960109     C                   ELSE
049700960109     C                   MOVEL     'S'           WFINE
049800960109     C                   END
049900940929     C*
050000960109     C                   ENDSR
050100960109     C*----------------------------------------------------*
050200960109     C*  WRTTES: Gestione scrittura videata descrizioni    *
050300960109     C*----------------------------------------------------*
050400960109     C     WRTTES        BEGSR
050500960109     C*----------------------------------------------------*
050600960109     C                   Z-ADD     1             NRR6
050700960109     C     NRR6          CHAIN     TB69S06                            31
050800960109     C     *IN31         DOWEQ     '0'
050900960109     C     *IN28         ANDEQ     '0'
051000960109     C                   SETOFF                                       40
051100960109     C*  Variazione
051200960109     C     V6CKEY        IFNE      *BLANKS
051300960109     C                   MOVEL     V6CKEY        KKEY
051400960110     C     KTAB2         CHAIN     TABEL00F                           32
051500960111     C                   MOVEL     *BLANKS       TBLUNI
051600080228     C*****              MOVEL     V6DSTP        TBLUNI
051700080228     C                   MOVEL     V6DSTP        §7vNOT
051800080228     C                   MOVEL     V6PDA         §7vPDA
051900080228     C                   MOVEL     ds7V          TBLUNI
052000960109     C                   MOVEL     *BLANKS       TBLFTR
052100960109     C                   MOVEL     *ZEROS        TBLDTR
052200960110     C     V6DSTP        IFNE      *BLANKS
052300960109     C  N32              UPDATE    TABEL
052400960109     C                   ELSE
052500960109     C*  Se ho abblencato riga gestisco l'annullamento
052600960111     C                   MOVEL     '*'           TBLFLG
052700960109     C                   MOVEL     *BLANKS       TBLFTR
052800960109     C                   MOVEL     *ZEROS        TBLDTR
052900960109     C  N32              UPDATE    TABEL
053000960109     C                   END
053100960109     C                   ELSE
053200960109     C*  Nuova riga
053300960110     C     V6DSTP        IFNE      *BLANKS
053400960111     C                   ADD       1             WNRTES
053500960111     C                   Z-ADD     WNRTES        WPROGG
053600960109     C                   MOVEL     'Q'           WTPPAR
053700960109     C                   MOVEL     DS7COD        WCDPAR
053800960109     C                   MOVEL     'S'           WTPELE
053900960109     C                   Z-ADD     1             TBLKUT
054000960109     C                   MOVEL     WKEY02        TBLKEY
054100960109     C                   MOVEL     '7V'          TBLCOD
054200960109     C                   MOVEL     '1'           TBLFTT
054300960111     C                   MOVEL     *BLANKS       TBLUNI
054400080228     C*****              MOVEL     V6DSTP        TBLUNI
054500080228     C                   MOVEL     V6DSTP        §7vNOT
054600080228     C                   MOVEL     V6pda         §7vPDA
054700080228     C                   MOVEL     ds7V          TBLUNI
054800960110     C                   WRITE     TABEL
054900960109     C                   END
055000960109     C                   END
055100960109     C*  Lettura successiva
055200960111     C                   ADD       1             NRR6
055300960109     C     NRR6          CHAIN     TB69S06                            31
055400960109     C                   END
055500960109     C*
055600960109     C                   ENDSR
055700960109     C*--------------------------------------------------------------*
055800960109     C*  CALL69: Richiamo pgm trtb69r1                               *
055900960109     C*--------------------------------------------------------------*
056000960109     C     CALL69        BEGSR
056100960109     C*
056200960116     C                   MOVEL     *BLANKS       DS7RIP
056300960116     C                   MOVEL     *BLANKS       DS7TRA
056400960109     C                   CALL      'TRTB69R1'
056500960109     C                   PARM                    KPJBA
056600960110     C                   MOVEL     KPJBU         DSTB69
056700960110     C                   MOVEL     'S'           WPGMIN
056800960110     C*  Controllo se devo aggiornare il fle descrizioni
056900960110     C     DS7F03        IFEQ      ' '
057000960109     C*  Se annullato codice annullo anche le descrizioni
057100960109     C     DS7RIP        IFEQ      'A'
057200960109     C                   EXSR      ANNUL
057300960109     C                   END
057400960109     C*  Se ripristinato codice riprisitno anche le descrizioni
057500960109     C     DS7RIP        IFEQ      'R'
057600960109     C                   EXSR      RIPRI
057700960109     C                   END
057800960109     C*  Se no trasmissione annullo trasmiss.
057900960109     C     DS7TRA        IFEQ      'N'
058000960109     C                   EXSR      NOTRA
058100960109     C                   END
058200960110     C                   ELSE
058300960110     C     DS7F03        IFEQ      '2'
058400960110     C                   MOVEL     '1'           WTPVID
058500960110     C                   MOVEL     'N'           WPGMIN
058600960110     C                   MOVEL     'N'           WFINE
058700960110     C                   END
058800960110     C                   END
058900960109     C*
059000960109     C                   ENDSR
059100960109     C*--------------------------------------------------------------*
059200960109     C*  ANNUL: Annullo codici tabella descrizioni                   *
059300960109     C*--------------------------------------------------------------*
059400960109     C     ANNUL         BEGSR
059500960109     C*
059600960109     C                   MOVEL     *BLANKS       KKEY
059700960109     C                   MOVEL     '7V'          KCOD
059800960109      *  Costruisco chiave per posizionamento su tabel
059900960109     C                   MOVEL     'Q'           WTPPAR
060000960109     C                   MOVEL     DS7COD        WCDPAR
060100960109     C                   MOVEL     WKEY00        KKEY
060200960109     C                   SETOFF                                       01
060300960109     C     KTAB2         SETLL     TABEL00F
060400960109     C     KTAB1         READE     TABEL00F                               31
060500960109     C     *IN31         DOWEQ     '0'
060600960109     C     WANKEY        ANDEQ     WKEY00
060700960111     C                   MOVEL     '*'           TBLFLG
060800960109     C                   EXCEPT    ANNTBL
060900960109     C     KTAB1         READE     TABEL00F                               31
061000960109     C                   END
061100960109     C*
061200960109     C                   ENDSR
061300960109     C*--------------------------------------------------------------*
061400960109     C*  RIPRI: Riprisitino codici tabella descrizioni               *
061500960109     C*--------------------------------------------------------------*
061600960109     C     RIPRI         BEGSR
061700960109     C*
061800960109     C                   MOVEL     *BLANKS       KKEY
061900960109     C                   MOVEL     '7V'          KCOD
062000960109      *  Costruisco chiave per posizionamento su tabel
062100960109     C                   MOVEL     'Q'           WTPPAR
062200960109     C                   MOVEL     DS7COD        WCDPAR
062300960109     C                   MOVEL     WKEY00        KKEY
062400960109     C                   SETOFF                                       01
062500960109     C     KTAB2         SETLL     TABEL00F
062600960109     C     KTAB1         READE     TABEL00F                               31
062700960109     C     *IN31         DOWEQ     '0'
062800960109     C     WANKEY        ANDEQ     WKEY00
062900960109     C                   MOVEL     ' '           TBLFLG
063000960109     C                   EXCEPT    ANNTBL
063100960109     C     KTAB1         READE     TABEL00F                               31
063200960109     C                   END
063300960109     C*
063400960109     C                   ENDSR
063500960109     C*--------------------------------------------------------------*
063600960109     C*  NOTRA: Tabella non in trasmissione                          *
063700960109     C*--------------------------------------------------------------*
063800960109     C     NOTRA         BEGSR
063900960109     C*
064000960109     C                   MOVEL     *BLANKS       KKEY
064100960109     C                   MOVEL     '7V'          KCOD
064200960109      *  Costruisco chiave per posizionamento su tabel
064300960109     C                   MOVEL     'Q'           WTPPAR
064400960109     C                   MOVEL     DS7COD        WCDPAR
064500960109     C                   MOVEL     WKEY00        KKEY
064600960109     C                   SETOFF                                       01
064700960109     C     KTAB2         SETLL     TABEL00F
064800960109     C     KTAB1         READE     TABEL00F                               31
064900960109     C     *IN31         DOWEQ     '0'
065000960109     C     WANKEY        ANDEQ     WKEY00
065100960109     C                   MOVEL     ' '           TBLFTT
065200960109     C                   EXCEPT    TRATBL
065300960109     C     KTAB1         READE     TABEL00F                               31
065400960109     C                   END
065500960109     C*
065600960109     C                   ENDSR
065700931209     C*--------------------------------------------------------------*
065800960109     C*  DEFCAM: Operazioni iniziali                                 *
065900931209     C*--------------------------------------------------------------*
066000960109     C     DEFCAM        BEGSR
066100931209     C*
066200931209     C*  Richiamo XPARUT
066300931209     C                   Z-ADD     1             CODUT
066400931209     C                   CALL      'XPARUT'
066500931209     C                   PARM                    UTEDSE
066600931209     C     CODUT         CABEQ     -1            FINE
066700960109     C                   MOVEL     RAGUT         V5CRSU
066800931209     C*----------------------------------------------------*
066900931209     C* Definizione chiavi                                 *
067000940927     C     KTAB1         KLIST
067100940927     C                   KFLD                    KKUT
067200940927     C                   KFLD                    KCOD
067300940928     C*
067400940928     C     KTAB2         KLIST
067500940928     C                   KFLD                    KKUT
067600940928     C                   KFLD                    KCOD
067700940928     C                   KFLD                    KKEY
067800931209     C*----------------------------------------------------*
067900931209     C* Definizione variabili                              *
068000940927     C     *LIKE         DEFINE    TBLKUT        KKUT
068100940927     C     *LIKE         DEFINE    TBLCOD        KCOD
068200940928     C     *LIKE         DEFINE    TBLKEY        KKEY
068300940725     C*----------------------------------------------------*
068400940927     C*  Inizializzazione variabili
068500940928     C                   Z-ADD     1             KKUT
068600940927     C                   MOVEL     'N'           WFINE             1
068700960109     C                   MOVEL     *BLANKS       V5DFUN
068800940928     C                   SETOFF                                       90
068900940228     C*----------------------------------------------------*
069000931209     C                   ENDSR
069100931209     O*-----------------------------------------------------*
069200960109     OTABEL     E            ANNTBL
069300960109     O                       TBLFLG
069400960109     OTABEL     E            TRATBL
069500960109     O                       TBLFTT
069600940927**
069700960109Immettere almeno una riga di descrizione                              01
069800960109Codice particolarità giacenza inesistente                             02
069900960110Codice particolarità giacenza annullato                               03
070000960110Cliente non abilitato alla gestione della particolarità giacenza      04
070100940419**
070200960109IMMISSIONE
070300960109VARIAZIONE
070400940927INTERROGAZIONE
