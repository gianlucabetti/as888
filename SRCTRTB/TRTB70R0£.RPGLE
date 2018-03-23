000100110608     /*END
000200891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000300891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000400960109     F*  KC        FINE LAVORO                                       *
000500960109     F*  KF        CONFERMA                                          *
000600960109     F*  KG        DATI STAMPA SU BOLLA                              *
000700960109     F*  KL        RITORNO                                           *
000800891030     F*--------------------------------------------------------------*
000900960111     FTRTB70D   CF   E             WORKSTN
001000960111     F                                     SFILE(TB70S05:NRR5)
001100960111     F                                     SFILE(TB70S06:NRR6)
001200940418     F*----------
001300960110     FTABEL00F  UF A E           K DISK
001400940418     D*--------------------------------------------------------------*
001500940418     D*   SCHIERE                                                    *
001600940701     D*--------------------------------------------------------------*
001700960110     D ERR             S             70    DIM(4) CTDATA PERRCD(1)
001800960109     D COST            S             16    DIM(3) CTDATA PERRCD(1)
001900960111     D CODP            S              2    DIM(30)
002000960111     D C7R             S              2    DIM(100)
002100960111     D D7R             S             89    DIM(100)
002200940928     D*--------------------------------------------------------------*
002300940928     D*  DS                                                          *
002400940928     D*--------------------------------------------------------------*
002500080227     D DS7V          E DS
002600960111     D DS7R          E DS
002700960111     D DSTB70        E DS                  EXTNAME(DS7PQRS)
002800900608      *
002900891030     D KPJBA         E DS
003000911118      *
003100911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003200960108      *  Codice x posizionamento su tabella 7V
003300960108     D                 DS
003400960108     D  WTPPAR                 1      1
003500960108     D  WCDPAR                 2      3
003600960109     D  WKEY00                 1      3
003700960109     D  WTPELE                 4      4
003800960109     D  WKEY01                 1      4
003900960109     D  WPROGG                 5      8  0
004000960109     D  WKEY02                 1      8
004100960109      *  SComposizione KEY di TABEL x 7V
004200960109     D                 DS
004300960109     D  WANKEY                 1      3
004400960109     D  WSVKEY                 1      4
004500960110     D  TBPROG                 5      8  0
004600960109     D  TBLKEY                 1      8
004700940228      *
004800940228     D                SDS
004900110412     D  V5CPGM                 1     10
005000931209     C*--------------------------------------------------------------*
005100931209     C*    C I C L O    P R I N C I P A L E                          *
005200931209     C*--------------------------------------------------------------*
005300960109     C     *ENTRY        PLIST
005400960109     C                   PARM                    KPJBA
005500960111     C                   MOVEL     KPJBU         DSTB70
005600960109     C                   MOVEL     ' '           DS7ERR
005700960109     C                   MOVEL     ' '           DS7F03
005800960109     C                   MOVEL     *BLANKS       DS7MSG
005900960110     C                   MOVEL     'N'           WFINE
006000960110     C                   MOVEL     'N'           WPGMIN            1
006100960109     C*  Prima volta
006200960109     C     WPRIMA        IFNE      'N'
006300960109     C                   MOVEL     'N'           WPRIMA            1
006400960109     C                   EXSR      DEFCAM
006500960109     C                   END
006600960109     C*  Eseguo controllo del codice
006700960109     C                   EXSR      CTRCOD
006800960109     C*  Eseguo caricamento note descrittive
006900960109     C     WFINE         IFNE      'S'
007000960112     C     DS7GES        ANDNE     'C'
007100960109     C                   EXSR      INZ01
007200960110     C*  Loop gestione varie videate fino a rich. fine elaboraz.
007300960110     C     WPGMIN        DOWEQ     'N'
007400931209     C*  Loop gestione varie videate fino a rich. fine elaboraz.
007500940927     C     WFINE         DOWNE     'S'
007600940927     C*
007700960109     C     WTPVID        CASEQ     '1'           GESD01
007800960109     C     WTPVID        CASEQ     '2'           GESD02
007900960109     C     WTPVID        CASEQ     '3'           GESD03
008000931209     C                   END
008100960110     C                   END
008200960110     C*  Se no fine programma richiamo successive videate
008300960110     C     DS7F03        IFEQ      ' '
008400960111     C                   EXSR      CALL70
008500960110     C                   END
008600960110     C*
008700960110     C                   END
008800960110     C*
008900960109     C                   END
009000931209     C* Fine programma
009100931209     C     FINE          TAG
009200960111     C                   MOVEL     DSTB70        KPJBU
009300960110     C* Libero tabel
009400960110     C     *LOVAL        SETLL     TABEL00F
009500960109     C* Se il programma è richiamato da altre funzioni chiudo in LR
009600960109     C* altrimenti chiudo in RT
009700960109     C     DS7RIC        IFEQ      'S'
009800960109     C                   SETON                                        LR
009900960109     C                   ELSE
010000960109     C                   SETON                                        RT
010100960109     C                   END
010200940928     C*---------------------------------------------------*
010300960109     C*  GESD01: Gestione videata descrizioni             *
010400940928     C*---------------------------------------------------*
010500940928     C     GESD01        BEGSR
010600940928     C*---------------------------------------------------*
010700931209     C*  Emissione videata
010800960109     C                   SETON                                        20
010900960111     C                   WRITE     TB70Z05
011000960111     C                   EXFMT     TB70C05
011100960109     C                   SETOFF                                       28
011200940418     C                   MOVEL     *BLANKS       $MSG
011300940928     C*---------------------------------------------------*
011400931209     C*  Fine elaborazione
011500931209     C     *INKC         IFEQ      '1'
011600940929     C                   MOVEL     'S'           WFINE
011700960109     C                   MOVEL     '1'           DS7F03
011800960110     C                   MOVEL     'S'           WPGMIN            1
011900931209     C                   GOTO      FINVD1
012000931209     C                   END
012100960109     C*---------------------------------------------------*
012200960109     C*  Ritorno videata precedente
012300960109     C     *INKL         IFEQ      '1'
012400960109     C                   MOVEL     'S'           WFINE
012500960110     C                   MOVEL     '2'           DS7F03
012600960110     C                   MOVEL     'S'           WPGMIN            1
012700960109     C                   GOTO      FINVD1
012800960109     C                   END
012900960109     C*---------------------------------------------------*
013000960109     C*  Richiamo videata gestione dati stampa su bolla
013100960109     C     *INKG         IFEQ      '1'
013200960109     C                   EXSR      INZ02
013300960109     C                   GOTO      FINVD1
013400960109     C                   END
013500960111     C*---------------------------------------------------*
013600960111     C*  X visualiz.con invio passo alla videata successiva
013700960111     C     DS7GES        IFEQ      'V'
013800960111     C     DS7OPZ        OREQ      '5'
013900960111     C                   MOVEL     'S'           WFINE
014000960111     C                   END
014100960109     C*---------------------------------------------------*
014200960109     C*  Richiamo videata gestione dati stampa su bolla
014300960109     C     *INKF         IFEQ      '1'
014400960109     C                   EXSR      WRTDES
014500960109     C                   END
014600931209     C*--------------------------------------------------------------*
014700940928     C     FINVD1        ENDSR
014800940928     C*--------------------------------------------------------------*
014900960109     C*  GESD02: Gestione subfile dati stampa bolla                  *
015000940928     C*--------------------------------------------------------------*
015100940928     C     GESD02        BEGSR
015200940928     C*----------------------------------------------------*
015300940928     C*  Emissione videata
015400960109     C                   SETON                                        20
015500960111     C                   WRITE     TB70Z06
015600960111     C                   EXFMT     TB70C06
015700940928     C                   SETOFF                                       2840
015800940928     C                   MOVEL     *BLANKS       $MSG
015900940928     C*----------------------------------------------------*
016000960109     C*  Fine elaborazione
016100960109     C     *INKC         IFEQ      '1'
016200960110     C                   MOVEL     'S'           WPGMIN            1
016300960109     C                   MOVEL     'S'           WFINE
016400960109     C                   MOVEL     '1'           DS7F03
016500960109     C                   GOTO      FINVD2
016600960109     C                   END
016700960109     C*---------------------------------------------------*
016800960109     C*  Ritorno videata precedente
016900960109     C     *INKL         IFEQ      '1'
017000960110     C                   MOVEL     '1'           WTPVID            1
017100960110     C                   GOTO      FINVD2
017200960109     C                   END
017300960109     C*---------------------------------------------------*
017400960109     C*  Richiamo videata gestione dati stampa su bolla
017500960109     C     *INKF         IFEQ      '1'
017600960109     C                   EXSR      WRTTES
017700960109     C                   MOVEL     '1'           WTPVID
017800960109     C                   END
017900940929     C*----------------------------------------------------*
018000940928     C     FINVD2        ENDSR
018100960109     C*--------------------------------------------------------------*
018200960109     C*  GESD03: Gestione subfile vuoto dati stampa bolla            *
018300960109     C*--------------------------------------------------------------*
018400960109     C     GESD03        BEGSR
018500960109     C*----------------------------------------------------*
018600960109     C*  Emissione videata
018700960110     C                   SETOFF                                       20
018800960111     C                   WRITE     TB70D06
018900960111     C                   EXFMT     TB70C06
019000960109     C                   SETOFF                                       2840
019100960110     C                   MOVEL     '1'           WTPVID
019200960109     C*----------------------------------------------------*
019300960109     C*  Fine elaborazione
019400960109     C     *INKC         IFEQ      '1'
019500960110     C                   MOVEL     'S'           WPGMIN            1
019600960110     C                   MOVEL     '1'           DS7F03
019700960109     C                   MOVEL     'S'           WFINE
019800960109     C                   END
019900960109     C*---------------------------------------------------*
020000960109     C                   ENDSR
020100960109     C*----------------------------------------------------*
020200960111     C*  CTRCOD: Controllo codice particolarità consegne   *
020300960109     C*----------------------------------------------------*
020400960109     C     CTRCOD        BEGSR
020500960109     C*
020600960109     C                   MOVEL     *BLANKS       DS7INS
020700960109     C                   MOVEL     *BLANKS       DS7ELI
020800960109     C                   Z-ADD     1             KKUT
020900960111     C                   MOVEL     '7R'          KCOD
021000960109     C                   MOVEL     *BLANKS       KKEY
021100960109     C                   MOVEL     DS7COD        KKEY
021200960209     C                   MOVEL     DS7COD        V5CCOD
021300960209     C                   MOVEL     DS7COD        V6CCOD
021400960109     C                   MOVEL     ' '           DS7ERR
021500960111     C                   CLEAR                   DS7R
021600960109     C     KTAB2         CHAIN     TABEL00F                           31
021700960109     C*----------------------------------------------------*
021800960110     C     DS7RIC        IFEQ      'S'
021900960109     C*----------------------------------------------------*
022000960109     C     *IN31         IFEQ      '1'
022100960110     C                   MOVEL     ERR(2)        DS7MSG
022200960109     C                   MOVEL     '1'           DS7ERR
022300960109     C                   MOVEL     'S'           WFINE
022400960109     C                   GOTO      FINCTR
022500960109     C                   END
022600960109     C*  Se pgm richiamato x controllo verifico se cod.annull.
022700960109     C     *IN31         IFEQ      '0'
022800960109     C     TBLFLG        ANDNE     ' '
022900960109     C     DS7GES        ANDEQ     'C'
023000960109     C                   MOVEL     'S'           WFINE
023100960110     C                   MOVEL     ERR(3)        DS7MSG
023200960109     C                   MOVEL     '1'           DS7ERR
023300960109     C                   GOTO      FINCTR
023400960109     C                   END
023500960109     C*----------------------------------------------------*
023600960109     C*  Se devo controllare abilitazione di un cod.cliente
023700960109     C*  carico i dati dalla 7U
023800960111     C                   MOVEL     TBLUNI        DS7R
023900960111     C                   MOVEL     §7RELI        DS7ELI
024000960111     C                   MOVEL     §7RINS        DS7INS
024100960109     C     DS7KSC        IFNE      0
024200960111     C     §7RABC        ANDEQ     'S'
024300960109     C                   MOVEA     DS7CDP        CODP
024400960109     C     V5CCOD        LOOKUP    CODP                                   32
024500960109     C     *IN32         IFEQ      '0'
024600960109     C                   MOVEL     'S'           WFINE
024700960110     C                   MOVEL     ERR(4)        DS7MSG
024800960109     C                   MOVEL     '1'           DS7ERR
024900960109     C                   END
025000960109     C                   END
025100960109     C*----------------------------------------------------*
025200960109     C                   ELSE
025300960109     C*  Se non è richiamato controllo se sono in immissione
025400960109     C     *IN31         IFEQ      '1'
025500960110     C                   MOVEL     COST(1)       V5DFUN
025600960109     C                   ELSE
025700960110     C                   MOVEL     COST(2)       V5DFUN
025800960109     C                   END
025900960109     C*----------------------------------------------------*
026000960109     C                   END
026100960109     C*----------------------------------------------------*
026200960109     C     FINCTR        ENDSR
026300940928     C*----------------------------------------------------*
026400940928     C*  INZ01   Gestione inizializzazione prima videata   *
026500940928     C*----------------------------------------------------*
026600940927     C     INZ01         BEGSR
026700960109     C*  Inizializzo videata subfile note descrittive
026800940929     C                   MOVEL     '1'           WTPVID
026900960109     C                   SETOFF                                       2890
027000940418     C                   MOVEL     *BLANKS       $MSG
027100940928     C*----------------------------------------------------*
027200960109     C*  Controllo se gestisco visulizzazione
027300940929     C     DS7GES        IFEQ      'V'
027400960109     C     DS7OPZ        OREQ      '5'
027500960109     C                   SETON                                        90
027600960109     C                   MOVEL     COST(3)       V5DFUN
027700940928     C                   END
027800940927     C*----------------------------------------------------*
027900940927     C*  Inizializzazione subfile
028000940927     C                   SETOFF                                       2021
028100960111     C                   WRITE     TB70C05
028200960109     C                   Z-ADD     0             NRR5              4 0
028300940927     C                   SETON                                        21
028400940927     C*----------------------------------------------------*
028500940927     C*  Caricamento subfile
028600960109     C                   MOVEL     '7V'          KCOD
028700960109      *  Controllo se esitono note da stampare x bolla
028800960111     C                   MOVEL     'R'           WTPPAR
028900960109     C                   MOVEL     DS7COD        WCDPAR
029000960109     C                   MOVEL     'S'           WTPELE
029100960109     C                   Z-ADD     0             WPROGG
029200960109     C                   MOVEL     WKEY02        KKEY
029300960109     C                   SETOFF                                       94
029400960109     C     KTAB2         SETLL     TABEL00F
029500960109     C     KTAB1         READE     TABEL00F                               31
029600960109     C     *IN31         IFEQ      '0'
029700960109     C     WSVKEY        ANDEQ     WKEY01
029800960109     C                   SETON                                        94
029900960109     C                   END
030000960109      *  Costruisco chiave per posizionamento su tabel
030100960109     C                   MOVEL     'V'           WTPELE
030200960111     C                   Z-ADD     0             WNRDES
030300960111     C                   MOVEL     WKEY02        KKEY
030400960109     C                   SETOFF                                       01
030500960109     C     KTAB2         SETLL     TABEL00F
030600960109     C     KTAB1         READE     TABEL00F                               31
030700960109     C     *IN31         IFEQ      '0'
030800960109     C     WSVKEY        ANDEQ     WKEY01
030900960109     C*----------------------------------------------------*
031000940927     C     *IN31         DOWEQ     '0'
031100960109     C     WSVKEY        ANDEQ     WKEY01
031200960109     C     TBLUNI        IFNE      *BLANK
031300080227     C*******            MOVEL     TBLUNI        V5DCOD
031400080227     C                   MOVEL     TBLUNI        ds7V
031500080227     C                   MOVEL     §7Vnot        V5dCOD
031600960109     C                   MOVEL     TBLKEY        V5CKEY
031700960109     C                   ADD       1             NRR5
031800960109     C     TBLFLG        COMP      ' '                                929293
031900960111     C                   WRITE     TB70S05
032000960109     C                   END
032100960111     C                   Z-ADD     TBPROG        WNRDES            4 0
032200960109     C     KTAB1         READE     TABEL00F                               31
032300960109     C                   SETON                                        01
032400960109     C                   END
032500940928     C*----------------------------------------------------*
032600960109     C                   END
032700960109     C*----------------------------------------------------*
032800960109     C*  Se il programma è richiamato in aggiornamento
032900960109     C*  carico una pagina di righe vuote
033000960109     C     DS7GES        IFEQ      ' '
033100960110     C     DS7OPZ        ANDNE     '5'
033200960109     C                   EXSR      ROLDES
033300960109     C                   END
033400960109     C*----------------------------------------------------*
033500940928     C                   ENDSR
033600960109     C*----------------------------------------------------*
033700960109     C*  INZ02   Gestione inizializzazione seconda videata *
033800960109     C*----------------------------------------------------*
033900960109     C     INZ02         BEGSR
034000960109     C*  Inizializzo videata subfile note descrittive
034100960110     C                   MOVEL     '2'           WTPVID
034200960109     C                   SETOFF                                       2890
034300960109     C                   MOVEL     *BLANKS       $MSG
034400960111     C*----------------------------------------------------*
034500960111     C*  Controllo se gestisco visulizzazione
034600960111     C     DS7GES        IFEQ      'V'
034700960111     C     DS7OPZ        OREQ      '5'
034800960111     C                   SETON                                        90
034900960111     C                   MOVEL     COST(3)       V5DFUN
035000960111     C                   END
035100960109     C*----------------------------------------------------*
035200960109     C*  Inizializzazione subfile
035300960109     C                   SETOFF                                       2021
035400960111     C                   WRITE     TB70C06
035500960109     C                   Z-ADD     0             NRR6              4 0
035600960109     C                   SETON                                        21
035700110412     C                   movel     V5Cpgm        V6Cpgm
035800110412     C                   movel     V5Crsu        V6Crsu
035900960109     C                   MOVEL     V5DFUN        V6DFUN
036000960109     C*----------------------------------------------------*
036100960109     C*  Caricamento subfile
036200960109     C                   MOVEL     '7V'          KCOD
036300960109      *  Costruisco chiave per posizionamento su tabel
036400960111     C                   MOVEL     'R'           WTPPAR
036500960109     C                   MOVEL     DS7COD        WCDPAR
036600960109     C                   MOVEL     'S'           WTPELE
036700960109     C                   Z-ADD     0             WPROGG
036800960109     C                   MOVEL     WKEY02        KKEY
036900960111     C                   Z-ADD     0             WNRTES
037000960109     C                   SETOFF                                       01
037100960109     C     KTAB2         SETLL     TABEL00F
037200960109     C     KTAB1         READE     TABEL00F                               31
037300960109     C     *IN31         IFEQ      '0'
037400960109     C     WSVKEY        ANDEQ     WKEY01
037500960109     C*----------------------------------------------------*
037600960109     C     *IN31         DOWEQ     '0'
037700960109     C     WSVKEY        ANDEQ     WKEY01
037800960109     C     TBLUNI        IFNE      *BLANK
037900960109     C                   MOVEL     TBLKEY        V6CKEY
038000080228     C****               MOVEL     TBLUNI        V6DSTP
038100080228     C                   MOVEL     TBLUNI        ds7V
038200080228     C                   MOVEL     §7vNOT        V6DSTP
038300080228     C                   MOVEL     §7vPDA        V6pda
038400960109     C                   ADD       1             NRR6
038500960109     C     TBLFLG        COMP      ' '                                929293
038600960111     C                   WRITE     TB70S06
038700960109     C                   END
038800960111     C                   Z-ADD     TBPROG        WNRTES            4 0
038900960109     C     KTAB1         READE     TABEL00F                               31
039000960109     C                   SETON                                        01
039100960109     C                   END
039200960109     C*----------------------------------------------------*
039300960109     C                   END
039400960109     C*----------------------------------------------------*
039500960109     C*  Se il programma è richiamato in aggiornamento
039600960109     C*  carico una pagina di righe vuote
039700960109     C     DS7GES        IFEQ      ' '
039800960109     C     DS7OPZ        ANDEQ     '1'
039900960115     C     NRR6          ANDEQ     0
040000960109     C                   EXSR      ROLTES
040100960109     C                   END
040200960110     C*----------------------------------------------------*
040300960110     C     NRR6          IFEQ      0
040400960110     C                   MOVEL     '3'           WTPVID
040500960110     C                   END
040600960109     C*----------------------------------------------------*
040700960109     C                   ENDSR
040800960109     C*----------------------------------------------------*
040900960109     C*  ROLDES: Carico riga di dettaglio                  *
041000960109     C*----------------------------------------------------*
041100960109     C     ROLDES        BEGSR
041200960109     C*
041300960109     C                   DO        17
041400960117     C                   SETOFF                                       92
041500960109     C                   MOVEL     *BLANKS       V5CKEY
041600960109     C                   MOVEL     *BLANKS       V5DCOD
041700960109     C                   ADD       1             NRR5
041800960111     C                   WRITE     TB70S05
041900960109     C                   END
042000960109     C*
042100960109     C                   ENDSR
042200960109     C*----------------------------------------------------*
042300960109     C*  ROLTES: Carico riga di dettaglio                  *
042400960109     C*----------------------------------------------------*
042500960109     C     ROLTES        BEGSR
042600960109     C*
042700960109     C                   MOVEL     *BLANKS       V6CKEY
042800960109     C                   MOVEL     *BLANKS       V6DSTP
042900080228     C                   MOVEL     *BLANKS       V6pda
043000960109     C                   ADD       1             NRR6
043100960117     C                   SETOFF                                       92
043200960111     C                   WRITE     TB70S06
043300960109     C*
043400960109     C                   ENDSR
043500960109     C*----------------------------------------------------*
043600960109     C*  WRTDES: Gestione scrittura videata descrizioni    *
043700940928     C*----------------------------------------------------*
043800960109     C     WRTDES        BEGSR
043900940928     C*----------------------------------------------------*
044000940929     C                   MOVEL     'N'           WSELEZ            1
044100960110     C                   MOVEL     ' '           DS7F03
044200960109     C                   Z-ADD     1             NRR5
044300960111     C     NRR5          CHAIN     TB70S05                            31
044400940928     C     *IN31         DOWEQ     '0'
044500940928     C     *IN28         ANDEQ     '0'
044600940928     C                   SETOFF                                       40
044700960109     C*  Variazione
044800960109     C     V5CKEY        IFNE      *BLANKS
044900960109     C                   MOVEL     'S'           WSELEZ
045000960109     C                   MOVEL     V5CKEY        KKEY
045100960110     C     KTAB2         CHAIN     TABEL00F                           32
045200960110     C                   MOVEL     '1'           TBLFTT
045300960110     C                   MOVEL     *BLANKS       TBLUNI
045400080227     C****               MOVEL     V5DCOD        TBLUNI
045500080227     C                   MOVEL     V5DCOD        §7Vnot
045600080227     C                   MOVEL     ds7V          TBLUNI
045700960109     C                   MOVEL     *BLANKS       TBLFTR
045800960109     C                   MOVEL     *ZEROS        TBLDTR
045900960109     C     V5DCOD        IFNE      *BLANKS
046000960109     C  N32              UPDATE    TABEL
046100960109     C                   ELSE
046200960109     C*  Se ho abblencato riga gestisco l'annullamento
046300960111     C                   MOVEL     '*'           TBLFLG
046400960110     C                   MOVEL     '1'           TBLFTT
046500960109     C                   MOVEL     *BLANKS       TBLFTR
046600960109     C                   MOVEL     *ZEROS        TBLDTR
046700960109     C  N32              UPDATE    TABEL
046800960109     C                   END
046900960109     C                   ELSE
047000960109     C*  Nuova riga
047100960110     C     V5DCOD        IFNE      *BLANKS
047200960109     C                   MOVEL     'S'           WSELEZ
047300960109     C                   ADD       1             WNRDES
047400960109     C                   Z-ADD     WNRDES        WPROGG
047500960111     C                   MOVEL     'R'           WTPPAR
047600960109     C                   MOVEL     DS7COD        WCDPAR
047700960110     C                   MOVEL     'V'           WTPELE
047800960109     C                   Z-ADD     1             TBLKUT
047900960109     C                   MOVEL     WKEY02        TBLKEY
048000960109     C                   MOVEL     '7V'          TBLCOD
048100960109     C                   MOVEL     '1'           TBLFTT
048200960110     C                   MOVEL     *BLANKS       TBLUNI
048300080227     C******             MOVEL     V5DCOD        TBLUNI
048400080227     C                   MOVEL     V5DCOD        §7Vnot
048500080227     C                   MOVEL     ds7V          TBLUNI
048600960110     C                   WRITE     TABEL
048700140507      * Si memorizza come se fosse caricato da file x un eventuale UPDATE
048800140507      * successivo
048900140507     C                   MOVEL     TBLKEY        V5CKEY
049000140507     c                   update    TB70S05
049100140507      ********
049200960109     C                   END
049300960109     C                   END
049400960109     C*  Lettura successiva
049500960109     C                   ADD       1             NRR5
049600960111     C     NRR5          CHAIN     TB70S05                            31
049700960109     C                   END
049800960109     C*  Se non ho scritto nessuna nota do errore
049900960109     C     WSELEZ        IFEQ      'N'
050000960109     C                   SETON                                        28
050100960109     C                   MOVEL     ERR(1)        $MSG
050200960109     C                   ELSE
050300960109     C                   MOVEL     'S'           WFINE
050400960109     C                   END
050500940929     C*
050600960109     C                   ENDSR
050700960109     C*----------------------------------------------------*
050800960109     C*  WRTTES: Gestione scrittura videata descrizioni    *
050900960109     C*----------------------------------------------------*
051000960109     C     WRTTES        BEGSR
051100960109     C*----------------------------------------------------*
051200960109     C                   Z-ADD     1             NRR6
051300960111     C     NRR6          CHAIN     TB70S06                            31
051400960109     C     *IN31         DOWEQ     '0'
051500960109     C     *IN28         ANDEQ     '0'
051600960109     C                   SETOFF                                       40
051700960109     C*  Variazione
051800960109     C     V6CKEY        IFNE      *BLANKS
051900960109     C                   MOVEL     V6CKEY        KKEY
052000960110     C     KTAB2         CHAIN     TABEL00F                           32
052100960111     C                   MOVEL     *BLANKS       TBLUNI
052200080228     C****               MOVEL     V6DSTP        TBLUNI
052300080228     C                   MOVEL     V6DSTP        §7vNOT
052400080228     C                   MOVEL     V6pda         §7vPDA
052500080228     C                   MOVEL     ds7V          TBLUNI
052600960109     C                   MOVEL     *BLANKS       TBLFTR
052700960109     C                   MOVEL     *ZEROS        TBLDTR
052800960110     C     V6DSTP        IFNE      *BLANKS
052900960109     C  N32              UPDATE    TABEL
053000960109     C                   ELSE
053100960109     C*  Se ho abblencato riga gestisco l'annullamento
053200960111     C                   MOVEL     '*'           TBLFLG
053300960109     C                   MOVEL     *BLANKS       TBLFTR
053400960109     C                   MOVEL     *ZEROS        TBLDTR
053500960109     C  N32              UPDATE    TABEL
053600960109     C                   END
053700960109     C                   ELSE
053800960109     C*  Nuova riga
053900960110     C     V6DSTP        IFNE      *BLANKS
054000960111     C                   ADD       1             WNRTES
054100960111     C                   Z-ADD     WNRTES        WPROGG
054200960111     C                   MOVEL     'R'           WTPPAR
054300960109     C                   MOVEL     DS7COD        WCDPAR
054400960109     C                   MOVEL     'S'           WTPELE
054500960109     C                   Z-ADD     1             TBLKUT
054600960109     C                   MOVEL     WKEY02        TBLKEY
054700960109     C                   MOVEL     '7V'          TBLCOD
054800960109     C                   MOVEL     '1'           TBLFTT
054900960111     C                   MOVEL     *BLANKS       TBLUNI
055000080228     C*****              MOVEL     V6DSTP        TBLUNI
055100080228     C                   MOVEL     V6DSTP        §7vNOT
055200080228     C                   MOVEL     V6pda         §7vPDA
055300080228     C                   MOVEL     ds7V          TBLUNI
055400960110     C                   WRITE     TABEL
055500960109     C                   END
055600960109     C                   END
055700960109     C*  Lettura successiva
055800960111     C                   ADD       1             NRR6
055900960111     C     NRR6          CHAIN     TB70S06                            31
056000960109     C                   END
056100960109     C*
056200960109     C                   ENDSR
056300960109     C*--------------------------------------------------------------*
056400960111     C*  CALL70: Richiamo pgm trtb70r1                               *
056500960109     C*--------------------------------------------------------------*
056600960111     C     CALL70        BEGSR
056700960109     C*
056800960117     C                   MOVEL     *BLANKS       DS7RIP
056900960117     C                   MOVEL     *BLANKS       DS7TRA
057000960111     C                   CALL      'TRTB70R1'
057100960109     C                   PARM                    KPJBA
057200960111     C                   MOVEL     KPJBU         DSTB70
057300960110     C                   MOVEL     'S'           WPGMIN
057400960110     C*  Controllo se devo aggiornare il fle descrizioni
057500960110     C     DS7F03        IFEQ      ' '
057600960109     C*  Se annullato codice annullo anche le descrizioni
057700960109     C     DS7RIP        IFEQ      'A'
057800960109     C                   EXSR      ANNUL
057900960109     C                   END
058000960109     C*  Se ripristinato codice riprisitno anche le descrizioni
058100960109     C     DS7RIP        IFEQ      'R'
058200960109     C                   EXSR      RIPRI
058300960109     C                   END
058400960109     C*  Se no trasmissione annullo trasmiss.
058500960109     C     DS7TRA        IFEQ      'N'
058600960109     C                   EXSR      NOTRA
058700960109     C                   END
058800960110     C                   ELSE
058900960110     C     DS7F03        IFEQ      '2'
059000960110     C                   MOVEL     '1'           WTPVID
059100960110     C                   MOVEL     'N'           WPGMIN
059200960110     C                   MOVEL     'N'           WFINE
059300960110     C                   END
059400960110     C                   END
059500110608     C*
059600110608     C* -Ripulisco il flag DS7F03 (1=F3, 2=F12) perchè altrimenti?
059700110608     C*  diventa IMPOSSIBILE proseguire (va in loop il *pgm):?
059800110608     C*  risultano ammesio solo F3 e F12 (non più Invio).?
059900110608     C                   clear                   DS7F03
060000960109     C*
060100960109     C                   ENDSR
060200960109     C*--------------------------------------------------------------*
060300960109     C*  ANNUL: Annullo codici tabella descrizioni                   *
060400960109     C*--------------------------------------------------------------*
060500960109     C     ANNUL         BEGSR
060600960109     C*
060700960109     C                   MOVEL     *BLANKS       KKEY
060800960109     C                   MOVEL     '7V'          KCOD
060900960109      *  Costruisco chiave per posizionamento su tabel
061000960111     C                   MOVEL     'R'           WTPPAR
061100960109     C                   MOVEL     DS7COD        WCDPAR
061200960109     C                   MOVEL     WKEY00        KKEY
061300960109     C                   SETOFF                                       01
061400960109     C     KTAB2         SETLL     TABEL00F
061500960109     C     KTAB1         READE     TABEL00F                               31
061600960109     C     *IN31         DOWEQ     '0'
061700960109     C     WANKEY        ANDEQ     WKEY00
061800960111     C                   MOVEL     '*'           TBLFLG
061900960109     C                   EXCEPT    ANNTBL
062000960109     C     KTAB1         READE     TABEL00F                               31
062100960109     C                   END
062200960109     C*
062300960109     C                   ENDSR
062400960109     C*--------------------------------------------------------------*
062500960109     C*  RIPRI: Riprisitino codici tabella descrizioni               *
062600960109     C*--------------------------------------------------------------*
062700960109     C     RIPRI         BEGSR
062800960109     C*
062900960109     C                   MOVEL     *BLANKS       KKEY
063000960109     C                   MOVEL     '7V'          KCOD
063100960109      *  Costruisco chiave per posizionamento su tabel
063200960111     C                   MOVEL     'R'           WTPPAR
063300960109     C                   MOVEL     DS7COD        WCDPAR
063400960109     C                   MOVEL     WKEY00        KKEY
063500960109     C                   SETOFF                                       01
063600960109     C     KTAB2         SETLL     TABEL00F
063700960109     C     KTAB1         READE     TABEL00F                               31
063800960109     C     *IN31         DOWEQ     '0'
063900960109     C     WANKEY        ANDEQ     WKEY00
064000960109     C                   MOVEL     ' '           TBLFLG
064100960109     C                   EXCEPT    ANNTBL
064200960109     C     KTAB1         READE     TABEL00F                               31
064300960109     C                   END
064400960109     C*
064500960109     C                   ENDSR
064600960109     C*--------------------------------------------------------------*
064700960109     C*  NOTRA: Tabella non in trasmissione                          *
064800960109     C*--------------------------------------------------------------*
064900960109     C     NOTRA         BEGSR
065000960109     C*
065100960109     C                   MOVEL     *BLANKS       KKEY
065200960109     C                   MOVEL     '7V'          KCOD
065300960109      *  Costruisco chiave per posizionamento su tabel
065400960111     C                   MOVEL     'R'           WTPPAR
065500960109     C                   MOVEL     DS7COD        WCDPAR
065600960109     C                   MOVEL     WKEY00        KKEY
065700960109     C                   SETOFF                                       01
065800960109     C     KTAB2         SETLL     TABEL00F
065900960109     C     KTAB1         READE     TABEL00F                               31
066000960109     C     *IN31         DOWEQ     '0'
066100960109     C     WANKEY        ANDEQ     WKEY00
066200960109     C                   MOVEL     ' '           TBLFTT
066300960109     C                   EXCEPT    TRATBL
066400960109     C     KTAB1         READE     TABEL00F                               31
066500960109     C                   END
066600960109     C*
066700960109     C                   ENDSR
066800931209     C*--------------------------------------------------------------*
066900960109     C*  DEFCAM: Operazioni iniziali                                 *
067000931209     C*--------------------------------------------------------------*
067100960109     C     DEFCAM        BEGSR
067200931209     C*
067300931209     C*  Richiamo XPARUT
067400931209     C                   Z-ADD     1             CODUT
067500931209     C                   CALL      'XPARUT'
067600931209     C                   PARM                    UTEDSE
067700931209     C     CODUT         CABEQ     -1            FINE
067800960109     C                   MOVEL     RAGUT         V5CRSU
067900931209     C*----------------------------------------------------*
068000931209     C* Definizione chiavi                                 *
068100940927     C     KTAB1         KLIST
068200940927     C                   KFLD                    KKUT
068300940927     C                   KFLD                    KCOD
068400940928     C*
068500940928     C     KTAB2         KLIST
068600940928     C                   KFLD                    KKUT
068700940928     C                   KFLD                    KCOD
068800940928     C                   KFLD                    KKEY
068900931209     C*----------------------------------------------------*
069000931209     C* Definizione variabili                              *
069100940927     C     *LIKE         DEFINE    TBLKUT        KKUT
069200940927     C     *LIKE         DEFINE    TBLCOD        KCOD
069300940928     C     *LIKE         DEFINE    TBLKEY        KKEY
069400940725     C*----------------------------------------------------*
069500940927     C*  Inizializzazione variabili
069600940928     C                   Z-ADD     1             KKUT
069700940927     C                   MOVEL     'N'           WFINE             1
069800960109     C                   MOVEL     *BLANKS       V5DFUN
069900940928     C                   SETOFF                                       90
070000940228     C*----------------------------------------------------*
070100931209     C                   ENDSR
070200931209     O*-----------------------------------------------------*
070300960109     OTABEL     E            ANNTBL
070400960109     O                       TBLFLG
070500960109     OTABEL     E            TRATBL
070600960109     O                       TBLFTT
070700940927**
070800960109Immettere almeno una riga di descrizione                              01
070900960111Codice particolarità consegne inesistente                             02
071000960111Codice particolarità consegne annullato                               03
071100960111Cliente non abilitato alla gestione della particolarità consegne      04
071200940419**
071300110412   IMMISSIONE
071400110412   VARIAZIONE
071500110412 INTERROGAZIONE
