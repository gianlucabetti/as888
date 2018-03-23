000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000300960109     F*  KC        FINE LAVORO                                       *
000400960109     F*  KF        CONFERMA                                          *
000500960109     F*  KG        DATI STAMPA SU BOLLA                              *
000600960109     F*  KL        RITORNO                                           *
000700891030     F*--------------------------------------------------------------*
000800960115     FTRTB73D   CF   E             WORKSTN
000900960115     F                                     SFILE(TB73S04:NRR4)
001000960115     F                                     SFILE(TB73S05:NRR5)
001100940418     F*----------
001200960110     FTABEL00F  UF A E           K DISK
001300940418     D*--------------------------------------------------------------*
001400940418     D*   SCHIERE                                                    *
001500940701     D*--------------------------------------------------------------*
001600960110     D ERR             S             70    DIM(4) CTDATA PERRCD(1)
001700960109     D COST            S             16    DIM(3) CTDATA PERRCD(1)
001800960115     D CODP            S              2    DIM(30)
001900960115     D C7S             S              2    DIM(100)
002000960115     D D7S             S             89    DIM(100)
002100940928     D*--------------------------------------------------------------*
002200940928     D*  DS                                                          *
002300940928     D*--------------------------------------------------------------*
002400080227     D DS7V          E DS
002500960115     D DS7S          E DS
002600960115     D DSTB73        E DS                  EXTNAME(DS7PQRS)
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
005400960115     C                   MOVEL     KPJBU         DSTB73
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
008300960115     C                   EXSR      CALL73
008400960110     C                   END
008500960110     C*
008600960110     C                   END
008700960110     C*
008800960109     C                   END
008900931209     C* Fine programma
009000931209     C     FINE          TAG
009100960115     C                   MOVEL     DSTB73        KPJBU
009200960110     C* Libero tabel
009300960110     C     *LOVAL        SETLL     TABEL00F
009400960109     C* Se il programma è richiamato da altre funzioni chiudo in LR
009500960109     C* altrimenti chiudo in RT
009600960115     C     DS7RIC        IFEQ      'S'
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
010800960115     C                   WRITE     TB73Z04
010900960115     C                   EXFMT     TB73C04
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
013400960111     C*---------------------------------------------------*
013500960111     C*  X visualiz.con invio passo alla videata successiva
013600960111     C     DS7GES        IFEQ      'V'
013700960111     C     DS7OPZ        OREQ      '5'
013800960111     C                   MOVEL     'S'           WFINE
013900960111     C                   END
014000960109     C*---------------------------------------------------*
014100960109     C*  Richiamo videata gestione dati stampa su bolla
014200960109     C     *INKF         IFEQ      '1'
014300960109     C                   EXSR      WRTDES
014400960109     C                   END
014500931209     C*--------------------------------------------------------------*
014600940928     C     FINVD1        ENDSR
014700940928     C*--------------------------------------------------------------*
014800960109     C*  GESD02: Gestione subfile dati stampa bolla                  *
014900940928     C*--------------------------------------------------------------*
015000940928     C     GESD02        BEGSR
015100940928     C*----------------------------------------------------*
015200940928     C*  Emissione videata
015300960109     C                   SETON                                        20
015400960115     C                   WRITE     TB73Z05
015500960115     C                   EXFMT     TB73C05
015600940928     C                   SETOFF                                       2840
015700940928     C                   MOVEL     *BLANKS       $MSG
015800940928     C*----------------------------------------------------*
015900960109     C*  Fine elaborazione
016000960109     C     *INKC         IFEQ      '1'
016100960110     C                   MOVEL     'S'           WPGMIN            1
016200960109     C                   MOVEL     'S'           WFINE
016300960109     C                   MOVEL     '1'           DS7F03
016400960109     C                   GOTO      FINVD2
016500960109     C                   END
016600960109     C*---------------------------------------------------*
016700960109     C*  Ritorno videata precedente
016800960109     C     *INKL         IFEQ      '1'
016900960110     C                   MOVEL     '1'           WTPVID            1
017000960110     C                   GOTO      FINVD2
017100960109     C                   END
017200960109     C*---------------------------------------------------*
017300960109     C*  Richiamo videata gestione dati stampa su bolla
017400960109     C     *INKF         IFEQ      '1'
017500960109     C                   EXSR      WRTTES
017600960109     C                   MOVEL     '1'           WTPVID
017700960109     C                   END
017800940929     C*----------------------------------------------------*
017900940928     C     FINVD2        ENDSR
018000960109     C*--------------------------------------------------------------*
018100960109     C*  GESD03: Gestione subfile vuoto dati stampa bolla            *
018200960109     C*--------------------------------------------------------------*
018300960109     C     GESD03        BEGSR
018400960109     C*----------------------------------------------------*
018500960109     C*  Emissione videata
018600960110     C                   SETOFF                                       20
018700960115     C                   WRITE     TB73D05
018800960115     C                   EXFMT     TB73C05
018900960109     C                   SETOFF                                       2840
019000960110     C                   MOVEL     '1'           WTPVID
019100960109     C*----------------------------------------------------*
019200960109     C*  Fine elaborazione
019300960109     C     *INKC         IFEQ      '1'
019400960110     C                   MOVEL     'S'           WPGMIN            1
019500960110     C                   MOVEL     '1'           DS7F03
019600960109     C                   MOVEL     'S'           WFINE
019700960109     C                   END
019800960109     C*---------------------------------------------------*
019900960109     C                   ENDSR
020000960109     C*----------------------------------------------------*
020100960111     C*  CTRCOD: Controllo codice particolarità consegne   *
020200960109     C*----------------------------------------------------*
020300960109     C     CTRCOD        BEGSR
020400960109     C*
020500960109     C                   MOVEL     *BLANKS       DS7INS
020600960109     C                   MOVEL     *BLANKS       DS7ELI
020700960109     C                   Z-ADD     1             KKUT
020800960115     C                   MOVEL     '7S'          KCOD
020900960109     C                   MOVEL     *BLANKS       KKEY
021000960109     C                   MOVEL     DS7COD        KKEY
021100960209     C                   MOVEL     DS7COD        V4CCOD
021200960209     C                   MOVEL     DS7COD        V5CCOD
021300960109     C                   MOVEL     ' '           DS7ERR
021400960115     C                   CLEAR                   DS7S
021500960109     C     KTAB2         CHAIN     TABEL00F                           31
021600960109     C*----------------------------------------------------*
021700960110     C     DS7RIC        IFEQ      'S'
021800960109     C*----------------------------------------------------*
021900960109     C     *IN31         IFEQ      '1'
022000960110     C                   MOVEL     ERR(2)        DS7MSG
022100960109     C                   MOVEL     '1'           DS7ERR
022200960109     C                   MOVEL     'S'           WFINE
022300960109     C                   GOTO      FINCTR
022400960109     C                   END
022500960109     C*  Se pgm richiamato x controllo verifico se cod.annull.
022600960109     C     *IN31         IFEQ      '0'
022700960109     C     TBLFLG        ANDNE     ' '
022800960109     C     DS7GES        ANDEQ     'C'
022900960109     C                   MOVEL     'S'           WFINE
023000960110     C                   MOVEL     ERR(3)        DS7MSG
023100960109     C                   MOVEL     '1'           DS7ERR
023200960109     C                   GOTO      FINCTR
023300960109     C                   END
023400960109     C*----------------------------------------------------*
023500960109     C*  Se devo controllare abilitazione di un cod.cliente
023600960109     C*  carico i dati dalla 7U
023700960115     C                   MOVEL     TBLUNI        DS7S
023800960115     C                   MOVEL     §7SELI        DS7ELI
023900960115     C                   MOVEL     §7SINS        DS7INS
024000960109     C     DS7KSC        IFNE      0
024100960115     C     §7SABC        ANDEQ     'S'
024200960109     C                   MOVEA     DS7CDP        CODP
024300960115     C     V4CCOD        LOOKUP    CODP                                   32
024400960109     C     *IN32         IFEQ      '0'
024500960109     C                   MOVEL     'S'           WFINE
024600960110     C                   MOVEL     ERR(4)        DS7MSG
024700960109     C                   MOVEL     '1'           DS7ERR
024800960109     C                   END
024900960109     C                   END
025000960109     C*----------------------------------------------------*
025100960109     C                   ELSE
025200960109     C*  Se non è richiamato controllo se sono in immissione
025300960109     C     *IN31         IFEQ      '1'
025400960115     C                   MOVEL     COST(1)       V4DFUN
025500960109     C                   ELSE
025600960115     C                   MOVEL     COST(2)       V4DFUN
025700960109     C                   END
025800960109     C*----------------------------------------------------*
025900960109     C                   END
026000960109     C*----------------------------------------------------*
026100960109     C     FINCTR        ENDSR
026200940928     C*----------------------------------------------------*
026300940928     C*  INZ01   Gestione inizializzazione prima videata   *
026400940928     C*----------------------------------------------------*
026500940927     C     INZ01         BEGSR
026600960109     C*  Inizializzo videata subfile note descrittive
026700940929     C                   MOVEL     '1'           WTPVID
026800960109     C                   SETOFF                                       2890
026900940418     C                   MOVEL     *BLANKS       $MSG
027000940928     C*----------------------------------------------------*
027100960109     C*  Controllo se gestisco visulizzazione
027200940929     C     DS7GES        IFEQ      'V'
027300960109     C     DS7OPZ        OREQ      '5'
027400960109     C                   SETON                                        90
027500960115     C                   MOVEL     COST(3)       V4DFUN
027600940928     C                   END
027700940927     C*----------------------------------------------------*
027800940927     C*  Inizializzazione subfile
027900940927     C                   SETOFF                                       2021
028000960115     C                   WRITE     TB73C04
028100960115     C                   Z-ADD     0             NRR4              4 0
028200940927     C                   SETON                                        21
028300940927     C*----------------------------------------------------*
028400940927     C*  Caricamento subfile
028500960109     C                   MOVEL     '7V'          KCOD
028600960109      *  Controllo se esitono note da stampare x bolla
028700960115     C                   MOVEL     'S'           WTPPAR
028800960109     C                   MOVEL     DS7COD        WCDPAR
028900960109     C                   MOVEL     'S'           WTPELE
029000960109     C                   Z-ADD     0             WPROGG
029100960109     C                   MOVEL     WKEY02        KKEY
029200960109     C                   SETOFF                                       94
029300960109     C     KTAB2         SETLL     TABEL00F
029400960109     C     KTAB1         READE     TABEL00F                               31
029500960109     C     *IN31         IFEQ      '0'
029600960109     C     WSVKEY        ANDEQ     WKEY01
029700960109     C                   SETON                                        94
029800960109     C                   END
029900960109      *  Costruisco chiave per posizionamento su tabel
030000960109     C                   MOVEL     'V'           WTPELE
030100960111     C                   Z-ADD     0             WNRDES
030200960111     C                   MOVEL     WKEY02        KKEY
030300960109     C                   SETOFF                                       01
030400960109     C     KTAB2         SETLL     TABEL00F
030500960109     C     KTAB1         READE     TABEL00F                               31
030600960109     C     *IN31         IFEQ      '0'
030700960109     C     WSVKEY        ANDEQ     WKEY01
030800960109     C*----------------------------------------------------*
030900940927     C     *IN31         DOWEQ     '0'
031000960109     C     WSVKEY        ANDEQ     WKEY01
031100960109     C     TBLUNI        IFNE      *BLANK
031200080227     C*****              MOVEL     TBLUNI        V4DCOD
031300080227     C                   MOVEL     TBLUNI        ds7V
031400080227     C                   MOVEL     §7Vnot        V4dCOD
031500960115     C                   MOVEL     TBLKEY        V4CKEY
031600960115     C                   ADD       1             NRR4
031700960109     C     TBLFLG        COMP      ' '                                929293
031800960115     C                   WRITE     TB73S04
031900960109     C                   END
032000960111     C                   Z-ADD     TBPROG        WNRDES            4 0
032100960109     C     KTAB1         READE     TABEL00F                               31
032200960109     C                   SETON                                        01
032300960109     C                   END
032400940928     C*----------------------------------------------------*
032500960109     C                   END
032600960109     C*----------------------------------------------------*
032700960109     C*  Se il programma è richiamato in aggiornamento
032800960109     C*  carico una pagina di righe vuote
032900960109     C     DS7GES        IFEQ      ' '
033000960110     C     DS7OPZ        ANDNE     '5'
033100960109     C                   EXSR      ROLDES
033200960109     C                   END
033300960109     C*----------------------------------------------------*
033400940928     C                   ENDSR
033500960109     C*----------------------------------------------------*
033600960109     C*  INZ02   Gestione inizializzazione seconda videata *
033700960109     C*----------------------------------------------------*
033800960109     C     INZ02         BEGSR
033900960109     C*  Inizializzo videata subfile note descrittive
034000960110     C                   MOVEL     '2'           WTPVID
034100960109     C                   SETOFF                                       2890
034200960109     C                   MOVEL     *BLANKS       $MSG
034300960109     C*----------------------------------------------------*
034400960109     C*  Inizializzazione subfile
034500960109     C                   SETOFF                                       2021
034600960115     C                   WRITE     TB73C05
034700960115     C                   Z-ADD     0             NRR5              4 0
034800960109     C                   SETON                                        21
034900960115     C                   MOVEL     V4DFUN        V5DFUN
035000960109     C*----------------------------------------------------*
035100960109     C*  Caricamento subfile
035200960109     C                   MOVEL     '7V'          KCOD
035300960109      *  Costruisco chiave per posizionamento su tabel
035400960115     C                   MOVEL     'S'           WTPPAR
035500960109     C                   MOVEL     DS7COD        WCDPAR
035600960109     C                   MOVEL     'S'           WTPELE
035700960109     C                   Z-ADD     0             WPROGG
035800960109     C                   MOVEL     WKEY02        KKEY
035900960111     C                   Z-ADD     0             WNRTES
036000960109     C                   SETOFF                                       01
036100960109     C     KTAB2         SETLL     TABEL00F
036200960109     C     KTAB1         READE     TABEL00F                               31
036300960109     C     *IN31         IFEQ      '0'
036400960109     C     WSVKEY        ANDEQ     WKEY01
036500960109     C*----------------------------------------------------*
036600960109     C     *IN31         DOWEQ     '0'
036700960109     C     WSVKEY        ANDEQ     WKEY01
036800960109     C     TBLUNI        IFNE      *BLANK
036900960115     C                   MOVEL     TBLKEY        V5CKEY
037000080228     C****               MOVEL     TBLUNI        V5DSTP
037100080228     C                   MOVEL     TBLUNI        ds7V
037200080228     C                   MOVEL     §7vNOT        V5DSTP
037300080228     C                   MOVEL     §7vPDA        V5pda
037400960115     C                   ADD       1             NRR5
037500960109     C     TBLFLG        COMP      ' '                                929293
037600960115     C                   WRITE     TB73S05
037700960109     C                   END
037800960111     C                   Z-ADD     TBPROG        WNRTES            4 0
037900960109     C     KTAB1         READE     TABEL00F                               31
038000960109     C                   SETON                                        01
038100960109     C                   END
038200960109     C*----------------------------------------------------*
038300960109     C                   END
038400960109     C*----------------------------------------------------*
038500960109     C*  Se il programma è richiamato in aggiornamento
038600960109     C*  carico una pagina di righe vuote
038700960109     C     DS7GES        IFEQ      ' '
038800960109     C     DS7OPZ        ANDEQ     '1'
038900960115     C     NRR5          ANDEQ     0
039000960109     C                   EXSR      ROLTES
039100960109     C                   END
039200960110     C*----------------------------------------------------*
039300960115     C     NRR5          IFEQ      0
039400960110     C                   MOVEL     '3'           WTPVID
039500960110     C                   END
039600960109     C*----------------------------------------------------*
039700960109     C                   ENDSR
039800960109     C*----------------------------------------------------*
039900960109     C*  ROLDES: Carico riga di dettaglio                  *
040000960109     C*----------------------------------------------------*
040100960109     C     ROLDES        BEGSR
040200960109     C*
040300960109     C                   DO        17
040400960115     C                   MOVEL     *BLANKS       V4CKEY
040500960115     C                   MOVEL     *BLANKS       V4DCOD
040600960115     C                   ADD       1             NRR4
040700960109     C     TBLFLG        COMP      ' '                                929293
040800960115     C                   WRITE     TB73S04
040900960109     C                   END
041000960109     C*
041100960109     C                   ENDSR
041200960109     C*----------------------------------------------------*
041300960109     C*  ROLTES: Carico riga di dettaglio                  *
041400960109     C*----------------------------------------------------*
041500960109     C     ROLTES        BEGSR
041600960109     C*
041700960115     C                   MOVEL     *BLANKS       V5CKEY
041800960115     C                   MOVEL     *BLANKS       V5DSTP
041900080228     C                   MOVEL     *BLANKS       V5pda
042000960115     C                   ADD       1             NRR5
042100960109     C     TBLFLG        COMP      ' '                                929293
042200960115     C                   WRITE     TB73S05
042300960109     C*
042400960109     C                   ENDSR
042500960109     C*----------------------------------------------------*
042600960109     C*  WRTDES: Gestione scrittura videata descrizioni    *
042700940928     C*----------------------------------------------------*
042800960109     C     WRTDES        BEGSR
042900940928     C*----------------------------------------------------*
043000940929     C                   MOVEL     'N'           WSELEZ            1
043100960110     C                   MOVEL     ' '           DS7F03
043200960109     C                   Z-ADD     1             NRR5
043300960115     C     NRR5          CHAIN     TB73S04                            31
043400940928     C     *IN31         DOWEQ     '0'
043500940928     C     *IN28         ANDEQ     '0'
043600940928     C                   SETOFF                                       40
043700960109     C*  Variazione
043800960115     C     V4CKEY        IFNE      *BLANKS
043900960109     C                   MOVEL     'S'           WSELEZ
044000960115     C                   MOVEL     V4CKEY        KKEY
044100960110     C     KTAB2         CHAIN     TABEL00F                           32
044200960110     C                   MOVEL     '1'           TBLFTT
044300960110     C                   MOVEL     *BLANKS       TBLUNI
044400080227     C*****              MOVEL     V4DCOD        TBLUNI
044500080227     C                   MOVEL     V4DCOD        §7Vnot
044600080227     C                   MOVEL     ds7V          TBLUNI
044700960109     C                   MOVEL     *BLANKS       TBLFTR
044800960109     C                   MOVEL     *ZEROS        TBLDTR
044900960115     C     V4DCOD        IFNE      *BLANKS
045000960109     C  N32              UPDATE    TABEL
045100960109     C                   ELSE
045200960109     C*  Se ho abblencato riga gestisco l'annullamento
045300960111     C                   MOVEL     '*'           TBLFLG
045400960110     C                   MOVEL     '1'           TBLFTT
045500960109     C                   MOVEL     *BLANKS       TBLFTR
045600960109     C                   MOVEL     *ZEROS        TBLDTR
045700960109     C  N32              UPDATE    TABEL
045800960109     C                   END
045900960109     C                   ELSE
046000960109     C*  Nuova riga
046100960115     C     V4DCOD        IFNE      *BLANKS
046200960109     C                   MOVEL     'S'           WSELEZ
046300960109     C                   ADD       1             WNRDES
046400960109     C                   Z-ADD     WNRDES        WPROGG
046500960115     C                   MOVEL     'S'           WTPPAR
046600960109     C                   MOVEL     DS7COD        WCDPAR
046700960110     C                   MOVEL     'V'           WTPELE
046800960109     C                   Z-ADD     1             TBLKUT
046900960109     C                   MOVEL     WKEY02        TBLKEY
047000960109     C                   MOVEL     '7V'          TBLCOD
047100960109     C                   MOVEL     '1'           TBLFTT
047200960110     C                   MOVEL     *BLANKS       TBLUNI
047300080227     C*****              MOVEL     V4DCOD        TBLUNI
047400080227     C                   MOVEL     V4DCOD        §7Vnot
047500080227     C                   MOVEL     ds7V          TBLUNI
047600960110     C                   WRITE     TABEL
047700960109     C                   END
047800960109     C                   END
047900960109     C*  Lettura successiva
048000960115     C                   ADD       1             NRR4
048100960115     C     NRR4          CHAIN     TB73S04                            31
048200960109     C                   END
048300960109     C*  Se non ho scritto nessuna nota do errore
048400960109     C     WSELEZ        IFEQ      'N'
048500960109     C                   SETON                                        28
048600960109     C                   MOVEL     ERR(1)        $MSG
048700960109     C                   ELSE
048800960109     C                   MOVEL     'S'           WFINE
048900960109     C                   END
049000940929     C*
049100960109     C                   ENDSR
049200960109     C*----------------------------------------------------*
049300960109     C*  WRTTES: Gestione scrittura videata descrizioni    *
049400960109     C*----------------------------------------------------*
049500960109     C     WRTTES        BEGSR
049600960109     C*----------------------------------------------------*
049700960115     C                   Z-ADD     1             NRR5
049800960115     C     NRR5          CHAIN     TB73S05                            31
049900960109     C     *IN31         DOWEQ     '0'
050000960109     C     *IN28         ANDEQ     '0'
050100960109     C                   SETOFF                                       40
050200960109     C*  Variazione
050300960115     C     V5CKEY        IFNE      *BLANKS
050400960115     C                   MOVEL     V5CKEY        KKEY
050500960110     C     KTAB2         CHAIN     TABEL00F                           32
050600960111     C                   MOVEL     *BLANKS       TBLUNI
050700080228     C****               MOVEL     V5DSTP        TBLUNI
050800080228     C                   MOVEL     V5DSTP        §7vNOT
050900080228     C                   MOVEL     V5pda         §7vPDA
051000080228     C                   MOVEL     ds7V          TBLUNI
051100960109     C                   MOVEL     *BLANKS       TBLFTR
051200960109     C                   MOVEL     *ZEROS        TBLDTR
051300960115     C     V5DSTP        IFNE      *BLANKS
051400960109     C  N32              UPDATE    TABEL
051500960109     C                   ELSE
051600960109     C*  Se ho abblencato riga gestisco l'annullamento
051700960111     C                   MOVEL     '*'           TBLFLG
051800960109     C                   MOVEL     *BLANKS       TBLFTR
051900960109     C                   MOVEL     *ZEROS        TBLDTR
052000960109     C  N32              UPDATE    TABEL
052100960109     C                   END
052200960109     C                   ELSE
052300960109     C*  Nuova riga
052400960115     C     V5DSTP        IFNE      *BLANKS
052500960111     C                   ADD       1             WNRTES
052600960111     C                   Z-ADD     WNRTES        WPROGG
052700960115     C                   MOVEL     'S'           WTPPAR
052800960109     C                   MOVEL     DS7COD        WCDPAR
052900960109     C                   MOVEL     'S'           WTPELE
053000960109     C                   Z-ADD     1             TBLKUT
053100960109     C                   MOVEL     WKEY02        TBLKEY
053200960109     C                   MOVEL     '7V'          TBLCOD
053300960109     C                   MOVEL     '1'           TBLFTT
053400960111     C                   MOVEL     *BLANKS       TBLUNI
053500080228     C*****              MOVEL     V5DSTP        TBLUNI
053600080228     C                   MOVEL     V5DSTP        §7vNOT
053700080228     C                   MOVEL     V5pda         §7vPDA
053800080228     C                   MOVEL     ds7V          TBLUNI
053900960110     C                   WRITE     TABEL
054000960109     C                   END
054100960109     C                   END
054200960109     C*  Lettura successiva
054300960115     C                   ADD       1             NRR5
054400960115     C     NRR5          CHAIN     TB73S05                            31
054500960109     C                   END
054600960109     C*
054700960109     C                   ENDSR
054800960109     C*--------------------------------------------------------------*
054900960115     C*  CALL73: Richiamo pgm trtb73r1                               *
055000960109     C*--------------------------------------------------------------*
055100960115     C     CALL73        BEGSR
055200960109     C*
055300960115     C                   CALL      'TRTB73R1'
055400960109     C                   PARM                    KPJBA
055500960115     C                   MOVEL     KPJBU         DSTB73
055600960110     C                   MOVEL     'S'           WPGMIN
055700960110     C*  Controllo se devo aggiornare il fle descrizioni
055800960110     C     DS7F03        IFEQ      ' '
055900960109     C*  Se annullato codice annullo anche le descrizioni
056000960109     C     DS7RIP        IFEQ      'A'
056100960109     C                   EXSR      ANNUL
056200960109     C                   END
056300960109     C*  Se ripristinato codice riprisitno anche le descrizioni
056400960109     C     DS7RIP        IFEQ      'R'
056500960109     C                   EXSR      RIPRI
056600960109     C                   END
056700960109     C*  Se no trasmissione annullo trasmiss.
056800960109     C     DS7TRA        IFEQ      'N'
056900960109     C                   EXSR      NOTRA
057000960109     C                   END
057100960110     C                   ELSE
057200960110     C     DS7F03        IFEQ      '2'
057300960110     C                   MOVEL     '1'           WTPVID
057400960110     C                   MOVEL     'N'           WPGMIN
057500960110     C                   MOVEL     'N'           WFINE
057600960110     C                   END
057700960110     C                   END
057800960109     C*
057900960109     C                   ENDSR
058000960109     C*--------------------------------------------------------------*
058100960109     C*  ANNUL: Annullo codici tabella descrizioni                   *
058200960109     C*--------------------------------------------------------------*
058300960109     C     ANNUL         BEGSR
058400960109     C*
058500960109     C                   MOVEL     *BLANKS       KKEY
058600960109     C                   MOVEL     '7V'          KCOD
058700960109      *  Costruisco chiave per posizionamento su tabel
058800960111     C                   MOVEL     'R'           WTPPAR
058900960109     C                   MOVEL     DS7COD        WCDPAR
059000960109     C                   MOVEL     WKEY00        KKEY
059100960109     C                   SETOFF                                       01
059200960109     C     KTAB2         SETLL     TABEL00F
059300960109     C     KTAB1         READE     TABEL00F                               31
059400960109     C     *IN31         DOWEQ     '0'
059500960109     C     WANKEY        ANDEQ     WKEY00
059600960111     C                   MOVEL     '*'           TBLFLG
059700960109     C                   EXCEPT    ANNTBL
059800960109     C     KTAB1         READE     TABEL00F                               31
059900960109     C                   END
060000960109     C*
060100960109     C                   ENDSR
060200960109     C*--------------------------------------------------------------*
060300960109     C*  RIPRI: Riprisitino codici tabella descrizioni               *
060400960109     C*--------------------------------------------------------------*
060500960109     C     RIPRI         BEGSR
060600960109     C*
060700960109     C                   MOVEL     *BLANKS       KKEY
060800960109     C                   MOVEL     '7V'          KCOD
060900960109      *  Costruisco chiave per posizionamento su tabel
061000960115     C                   MOVEL     'S'           WTPPAR
061100960109     C                   MOVEL     DS7COD        WCDPAR
061200960109     C                   MOVEL     WKEY00        KKEY
061300960109     C                   SETOFF                                       01
061400960109     C     KTAB2         SETLL     TABEL00F
061500960109     C     KTAB1         READE     TABEL00F                               31
061600960109     C     *IN31         DOWEQ     '0'
061700960109     C     WANKEY        ANDEQ     WKEY00
061800960109     C                   MOVEL     ' '           TBLFLG
061900960109     C                   EXCEPT    ANNTBL
062000960109     C     KTAB1         READE     TABEL00F                               31
062100960109     C                   END
062200960109     C*
062300960109     C                   ENDSR
062400960109     C*--------------------------------------------------------------*
062500960109     C*  NOTRA: Tabella non in trasmissione                          *
062600960109     C*--------------------------------------------------------------*
062700960109     C     NOTRA         BEGSR
062800960109     C*
062900960109     C                   MOVEL     *BLANKS       KKEY
063000960109     C                   MOVEL     '7V'          KCOD
063100960109      *  Costruisco chiave per posizionamento su tabel
063200960115     C                   MOVEL     'S'           WTPPAR
063300960109     C                   MOVEL     DS7COD        WCDPAR
063400960109     C                   MOVEL     WKEY00        KKEY
063500960109     C                   SETOFF                                       01
063600960109     C     KTAB2         SETLL     TABEL00F
063700960109     C     KTAB1         READE     TABEL00F                               31
063800960109     C     *IN31         DOWEQ     '0'
063900960109     C     WANKEY        ANDEQ     WKEY00
064000960109     C                   MOVEL     ' '           TBLFTT
064100960109     C                   EXCEPT    TRATBL
064200960109     C     KTAB1         READE     TABEL00F                               31
064300960109     C                   END
064400960109     C*
064500960109     C                   ENDSR
064600931209     C*--------------------------------------------------------------*
064700960109     C*  DEFCAM: Operazioni iniziali                                 *
064800931209     C*--------------------------------------------------------------*
064900960109     C     DEFCAM        BEGSR
065000931209     C*
065100931209     C*  Richiamo XPARUT
065200931209     C                   Z-ADD     1             CODUT
065300931209     C                   CALL      'XPARUT'
065400931209     C                   PARM                    UTEDSE
065500931209     C     CODUT         CABEQ     -1            FINE
065600960115     C                   MOVEL     RAGUT         V4CRSU
065700931209     C*----------------------------------------------------*
065800931209     C* Definizione chiavi                                 *
065900940927     C     KTAB1         KLIST
066000940927     C                   KFLD                    KKUT
066100940927     C                   KFLD                    KCOD
066200940928     C*
066300940928     C     KTAB2         KLIST
066400940928     C                   KFLD                    KKUT
066500940928     C                   KFLD                    KCOD
066600940928     C                   KFLD                    KKEY
066700931209     C*----------------------------------------------------*
066800931209     C* Definizione variabili                              *
066900940927     C     *LIKE         DEFINE    TBLKUT        KKUT
067000940927     C     *LIKE         DEFINE    TBLCOD        KCOD
067100940928     C     *LIKE         DEFINE    TBLKEY        KKEY
067200940725     C*----------------------------------------------------*
067300940927     C*  Inizializzazione variabili
067400940928     C                   Z-ADD     1             KKUT
067500940927     C                   MOVEL     'N'           WFINE             1
067600960115     C                   MOVEL     *BLANKS       V4DFUN
067700940928     C                   SETOFF                                       90
067800940228     C*----------------------------------------------------*
067900931209     C                   ENDSR
068000931209     O*-----------------------------------------------------*
068100960109     OTABEL     E            ANNTBL
068200960109     O                       TBLFLG
068300960109     OTABEL     E            TRATBL
068400960109     O                       TBLFTT
068500940927**
068600960109Immettere almeno una riga di descrizione                              01
068700960209Codice particolarità varie inesistente                                02
068800960209Codice particolarità varie annullato                                  03
068900960209Cliente non abilitato alla gestione della particolarità varie         04
069000940419**
069100960109IMMISSIONE
069200960109VARIAZIONE
069300940927INTERROGAZIONE
