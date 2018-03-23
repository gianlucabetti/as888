000100160308     /*END
000200891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000300891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000400960109     F*  KC        FINE LAVORO                                       *
000500960109     F*  KF        CONFERMA                                          *
000600960109     F*  KG        DATI STAMPA SU BOLLA                              *
000700960109     F*  KL        RITORNO                                           *
000800891030     F*--------------------------------------------------------------*
000900940927     FTRTB69D   CF   E             WORKSTN
001000960109     F                                     SFILE(TB69S05:NRR5)
001100960109     F                                     SFILE(TB69S06:NRR6)
001200940418     F*----------
001300960110     FTABEL00F  UF A E           K DISK
001400940418     D*--------------------------------------------------------------*
001500940418     D*   SCHIERE                                                    *
001600940701     D*--------------------------------------------------------------*
001700960110     D ERR             S             70    DIM(4) CTDATA PERRCD(1)
001800960109     D COST            S             16    DIM(3) CTDATA PERRCD(1)
001900960109     D CODP            S              2    DIM(30)
002000960109     D C7Q             S              2    DIM(100)
002100960109     D D7Q             S             89    DIM(100)
002200940928     D*--------------------------------------------------------------*
002300940928     D*  DS                                                          *
002400940928     D*--------------------------------------------------------------*
002500080227     D DS7V          E DS
002600960109     D DS7Q          E DS
002700960109     D DSTB69        E DS                  EXTNAME(DS7PQRS)
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
004900160308     D  V5CPGM                 1     10
005000931209     C*--------------------------------------------------------------*
005100931209     C*    C I C L O    P R I N C I P A L E                          *
005200931209     C*--------------------------------------------------------------*
005300960109     C     *ENTRY        PLIST
005400960109     C                   PARM                    KPJBA
005500960109     C                   MOVEL     KPJBU         DSTB69
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
008400960110     C                   EXSR      CALL69
008500960110     C                   END
008600960110     C*
008700960110     C                   END
008800960110     C*
008900960109     C                   END
009000931209     C* Fine programma
009100931209     C     FINE          TAG
009200940928     C                   MOVEL     DSTB69        KPJBU
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
010900960109     C                   WRITE     TB69Z05
011000960109     C                   EXFMT     TB69C05
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
013500960109     C*---------------------------------------------------*
013600960112     C*---------------------------------------------------*
013700960112     C*  X visualiz.con invio passo alla videata successiva
013800960112     C     DS7GES        IFEQ      'V'
013900960112     C     DS7OPZ        OREQ      '5'
014000021010     C                   CLEAR                   DS7F03
014100960112     C                   MOVEL     'S'           WFINE
014200960112     C                   END
014300960112     C*---------------------------------------------------*
014400960109     C*  Richiamo videata gestione dati stampa su bolla
014500960109     C     *INKF         IFEQ      '1'
014600960109     C                   EXSR      WRTDES
014700960109     C                   END
014800931209     C*--------------------------------------------------------------*
014900940928     C     FINVD1        ENDSR
015000940928     C*--------------------------------------------------------------*
015100960109     C*  GESD02: Gestione subfile dati stampa bolla                  *
015200940928     C*--------------------------------------------------------------*
015300940928     C     GESD02        BEGSR
015400940928     C*----------------------------------------------------*
015500940928     C*  Emissione videata
015600960109     C                   SETON                                        20
015700960109     C                   WRITE     TB69Z06
015800960109     C                   EXFMT     TB69C06
015900940928     C                   SETOFF                                       2840
016000940928     C                   MOVEL     *BLANKS       $MSG
016100940928     C*----------------------------------------------------*
016200960109     C*  Fine elaborazione
016300960109     C     *INKC         IFEQ      '1'
016400960110     C                   MOVEL     'S'           WPGMIN            1
016500960109     C                   MOVEL     'S'           WFINE
016600960109     C                   MOVEL     '1'           DS7F03
016700960109     C                   GOTO      FINVD2
016800960109     C                   END
016900960109     C*---------------------------------------------------*
017000960109     C*  Ritorno videata precedente
017100960109     C     *INKL         IFEQ      '1'
017200960110     C                   MOVEL     '1'           WTPVID            1
017300960110     C                   GOTO      FINVD2
017400960109     C                   END
017500960109     C*---------------------------------------------------*
017600960109     C*  Richiamo videata gestione dati stampa su bolla
017700960109     C     *INKF         IFEQ      '1'
017800960109     C                   EXSR      WRTTES
017900960109     C                   MOVEL     '1'           WTPVID
018000960109     C                   END
018100940929     C*----------------------------------------------------*
018200940928     C     FINVD2        ENDSR
018300960109     C*--------------------------------------------------------------*
018400960109     C*  GESD03: Gestione subfile vuoto dati stampa bolla            *
018500960109     C*--------------------------------------------------------------*
018600960109     C     GESD03        BEGSR
018700960109     C*----------------------------------------------------*
018800960109     C*  Emissione videata
018900960110     C                   SETOFF                                       20
019000960109     C                   WRITE     TB69D06
019100960109     C                   EXFMT     TB69C06
019200960109     C                   SETOFF                                       2840
019300960110     C                   MOVEL     '1'           WTPVID
019400960109     C*----------------------------------------------------*
019500960109     C*  Fine elaborazione
019600960109     C     *INKC         IFEQ      '1'
019700960110     C                   MOVEL     'S'           WPGMIN            1
019800960110     C                   MOVEL     '1'           DS7F03
019900960109     C                   MOVEL     'S'           WFINE
020000960109     C                   END
020100960109     C*---------------------------------------------------*
020200960109     C                   ENDSR
020300960109     C*----------------------------------------------------*
020400960109     C*  CTRCOD: Controllo codice particolarità giacenza   *
020500960109     C*----------------------------------------------------*
020600960109     C     CTRCOD        BEGSR
020700960109     C*
020800960109     C                   MOVEL     *BLANKS       DS7INS
020900960109     C                   MOVEL     *BLANKS       DS7ELI
021000960109     C                   Z-ADD     1             KKUT
021100960109     C                   MOVEL     '7Q'          KCOD
021200960109     C                   MOVEL     *BLANKS       KKEY
021300960109     C                   MOVEL     DS7COD        KKEY
021400960209     C                   MOVEL     DS7COD        V5CCOD
021500960209     C                   MOVEL     DS7COD        V6CCOD
021600960109     C                   MOVEL     ' '           DS7ERR
021700960109     C                   CLEAR                   DS7Q
021800160308     c                   If        DS7GES = 'V'  or
021900160308     c                             DS7OPZ = '5'
022000160308     c     Ktab2         chain(N)  TABEL                              31
022100160308     c                   Else
022200960109     C     KTAB2         CHAIN     TABEL00F                           31
022300160308     c                   EndIf
022400960109     C*----------------------------------------------------*
022500960110     C     DS7RIC        IFEQ      'S'
022600960109     C*----------------------------------------------------*
022700960109     C     *IN31         IFEQ      '1'
022800960110     C                   MOVEL     ERR(2)        DS7MSG
022900960109     C                   MOVEL     '1'           DS7ERR
023000960109     C                   MOVEL     'S'           WFINE
023100960109     C                   GOTO      FINCTR
023200960109     C                   END
023300960109     C*  Se pgm richiamato x controllo verifico se cod.annull.
023400960109     C     *IN31         IFEQ      '0'
023500960109     C     TBLFLG        ANDNE     ' '
023600960109     C     DS7GES        ANDEQ     'C'
023700960109     C                   MOVEL     'S'           WFINE
023800960110     C                   MOVEL     ERR(3)        DS7MSG
023900960109     C                   MOVEL     '1'           DS7ERR
024000960109     C                   GOTO      FINCTR
024100960109     C                   END
024200960109     C*----------------------------------------------------*
024300960109     C*  Se devo controllare abilitazione di un cod.cliente
024400960109     C*  carico i dati dalla 7U
024500960109     C                   MOVEL     TBLUNI        DS7Q
024600960109     C                   MOVEL     §7QELI        DS7ELI
024700960109     C                   MOVEL     §7QINS        DS7INS
024800960109     C     DS7KSC        IFNE      0
024900960109     C     §7QABC        ANDEQ     'S'
025000960109     C                   MOVEA     DS7CDP        CODP
025100960109     C     V5CCOD        LOOKUP    CODP                                   32
025200960109     C     *IN32         IFEQ      '0'
025300960109     C                   MOVEL     'S'           WFINE
025400960110     C                   MOVEL     ERR(4)        DS7MSG
025500960109     C                   MOVEL     '1'           DS7ERR
025600960109     C                   END
025700960109     C                   END
025800960109     C*----------------------------------------------------*
025900960109     C                   ELSE
026000960109     C*  Se non è richiamato controllo se sono in immissione
026100960109     C     *IN31         IFEQ      '1'
026200960110     C                   MOVEL     COST(1)       V5DFUN
026300960109     C                   ELSE
026400960110     C                   MOVEL     COST(2)       V5DFUN
026500960109     C                   END
026600960109     C*----------------------------------------------------*
026700960109     C                   END
026800960109     C*----------------------------------------------------*
026900960109     C     FINCTR        ENDSR
027000940928     C*----------------------------------------------------*
027100940928     C*  INZ01   Gestione inizializzazione prima videata   *
027200940928     C*----------------------------------------------------*
027300940927     C     INZ01         BEGSR
027400960109     C*  Inizializzo videata subfile note descrittive
027500940929     C                   MOVEL     '1'           WTPVID
027600960109     C                   SETOFF                                       2890
027700940418     C                   MOVEL     *BLANKS       $MSG
027800940928     C*----------------------------------------------------*
027900960109     C*  Controllo se gestisco visulizzazione
028000940929     C     DS7GES        IFEQ      'V'
028100960109     C     DS7OPZ        OREQ      '5'
028200960109     C                   SETON                                        90
028300960109     C                   MOVEL     COST(3)       V5DFUN
028400940928     C                   END
028500940927     C*----------------------------------------------------*
028600940927     C*  Inizializzazione subfile
028700940927     C                   SETOFF                                       2021
028800960109     C                   WRITE     TB69C05
028900960109     C                   Z-ADD     0             NRR5              4 0
029000940927     C                   SETON                                        21
029100940927     C*----------------------------------------------------*
029200940927     C*  Caricamento subfile
029300960109     C                   MOVEL     '7V'          KCOD
029400960109      *  Controllo se esitono note da stampare x bolla
029500960109     C                   MOVEL     'Q'           WTPPAR
029600960109     C                   MOVEL     DS7COD        WCDPAR
029700960109     C                   MOVEL     'S'           WTPELE
029800960109     C                   Z-ADD     0             WPROGG
029900960109     C                   MOVEL     WKEY02        KKEY
030000960109     C                   SETOFF                                       94
030100960109     C     KTAB2         SETLL     TABEL00F
030200160308     c                   If        DS7GES = 'V'  or
030300160308     c                             DS7OPZ = '5'
030400160308     c     Ktab1         reade(N)  TABEL                                  31
030500160308     c                   Else
030600960109     C     KTAB1         READE     TABEL00F                               31
030700160308     c                   EndIf
030800960109     C     *IN31         IFEQ      '0'
030900960109     C     WSVKEY        ANDEQ     WKEY01
031000960109     C                   SETON                                        94
031100960109     C                   END
031200960109      *  Costruisco chiave per posizionamento su tabel
031300960109     C                   MOVEL     'V'           WTPELE
031400960111     C                   Z-ADD     0             WNRDES
031500960111     C                   MOVEL     WKEY02        KKEY
031600960109     C                   SETOFF                                       01
031700960109     C     KTAB2         SETLL     TABEL00F
031800160308     c                   If        DS7GES = 'V'  or
031900160308     c                             DS7OPZ = '5'
032000160308     c     Ktab1         reade(N)  TABEL                                  31
032100160308     c                   Else
032200960109     C     KTAB1         READE     TABEL00F                               31
032300160308     c                   EndIf
032400960109     C     *IN31         IFEQ      '0'
032500960109     C     WSVKEY        ANDEQ     WKEY01
032600960109     C*----------------------------------------------------*
032700940927     C     *IN31         DOWEQ     '0'
032800960109     C     WSVKEY        ANDEQ     WKEY01
032900960109     C     TBLUNI        IFNE      *BLANK
033000080227     C*******            MOVEL     TBLUNI        V5DCOD
033100080227     C                   MOVEL     TBLUNI        ds7V
033200080227     C                   MOVEL     §7Vnot        V5dCOD
033300960109     C                   MOVEL     TBLKEY        V5CKEY
033400960109     C                   ADD       1             NRR5
033500960109     C     TBLFLG        COMP      ' '                                929293
033600960109     C                   WRITE     TB69S05
033700960109     C                   END
033800960111     C                   Z-ADD     TBPROG        WNRDES            4 0
033900160308     c                   If        DS7GES = 'V'  or
034000160308     c                             DS7OPZ = '5'
034100160308     c     Ktab1         reade(N)  TABEL                                  31
034200160308     c                   Else
034300960109     C     KTAB1         READE     TABEL00F                               31
034400160308     c                   EndIf
034500960109     C                   SETON                                        01
034600960109     C                   END
034700940928     C*----------------------------------------------------*
034800960109     C                   END
034900960109     C*----------------------------------------------------*
035000960109     C*  Se il programma è richiamato in aggiornamento
035100960109     C*  carico una pagina di righe vuote
035200960109     C     DS7GES        IFEQ      ' '
035300960110     C     DS7OPZ        ANDNE     '5'
035400960109     C                   EXSR      ROLDES
035500960109     C                   END
035600960109     C*----------------------------------------------------*
035700940928     C                   ENDSR
035800960109     C*----------------------------------------------------*
035900960109     C*  INZ02   Gestione inizializzazione seconda videata *
036000960109     C*----------------------------------------------------*
036100960109     C     INZ02         BEGSR
036200960109     C*  Inizializzo videata subfile note descrittive
036300960110     C                   MOVEL     '2'           WTPVID
036400960109     C                   SETOFF                                       2890
036500960109     C                   MOVEL     *BLANKS       $MSG
036600960112     C*----------------------------------------------------*
036700960112     C*  Controllo se gestisco visulizzazione
036800960112     C     DS7GES        IFEQ      'V'
036900960112     C     DS7OPZ        OREQ      '5'
037000960112     C                   SETON                                        90
037100960112     C                   MOVEL     COST(3)       V5DFUN
037200960112     C                   END
037300960109     C*----------------------------------------------------*
037400960109     C*  Inizializzazione subfile
037500960109     C                   SETOFF                                       2021
037600960109     C                   WRITE     TB69C06
037700960109     C                   Z-ADD     0             NRR6              4 0
037800960109     C                   SETON                                        21
037900160308     C                   movel     V5Cpgm        V6Cpgm
038000160308     C                   movel     V5Crsu        V6Crsu
038100960109     C                   MOVEL     V5DFUN        V6DFUN
038200960109     C*----------------------------------------------------*
038300960109     C*  Caricamento subfile
038400960109     C                   MOVEL     '7V'          KCOD
038500960109      *  Costruisco chiave per posizionamento su tabel
038600960109     C                   MOVEL     'Q'           WTPPAR
038700960109     C                   MOVEL     DS7COD        WCDPAR
038800960109     C                   MOVEL     'S'           WTPELE
038900960109     C                   Z-ADD     0             WPROGG
039000960109     C                   MOVEL     WKEY02        KKEY
039100960111     C                   Z-ADD     0             WNRTES
039200960109     C                   SETOFF                                       01
039300960109     C     KTAB2         SETLL     TABEL00F
039400160308     c                   If        DS7GES = 'V'  or
039500160308     c                             DS7OPZ = '5'
039600160308     c     Ktab1         reade(N)  TABEL                                  31
039700160308     c                   Else
039800960109     C     KTAB1         READE     TABEL00F                               31
039900160308     c                   EndIf
040000960109     C     *IN31         IFEQ      '0'
040100960109     C     WSVKEY        ANDEQ     WKEY01
040200960109     C*----------------------------------------------------*
040300960109     C     *IN31         DOWEQ     '0'
040400960109     C     WSVKEY        ANDEQ     WKEY01
040500960109     C     TBLUNI        IFNE      *BLANK
040600960109     C                   MOVEL     TBLKEY        V6CKEY
040700080228     C                   MOVEL     TBLUNI        ds7V
040800080228     C                   MOVEL     §7vNOT        V6DSTP
040900080228     C                   MOVEL     §7Vpda        V6PDA
041000960109     C                   ADD       1             NRR6
041100960109     C     TBLFLG        COMP      ' '                                929293
041200960109     C                   WRITE     TB69S06
041300960109     C                   END
041400960111     C                   Z-ADD     TBPROG        WNRTES            4 0
041500160308     c                   If        DS7GES = 'V'  or
041600160308     c                             DS7OPZ = '5'
041700160308     c     Ktab1         reade(N)  TABEL                                  31
041800160308     c                   Else
041900960109     C     KTAB1         READE     TABEL00F                               31
042000160308     c                   EndIf
042100960109     C                   SETON                                        01
042200960109     C                   END
042300960109     C*----------------------------------------------------*
042400960109     C                   END
042500960109     C*----------------------------------------------------*
042600960109     C*  Se il programma è richiamato in aggiornamento
042700960109     C*  carico una pagina di righe vuote
042800960109     C     DS7GES        IFEQ      ' '
042900960109     C     DS7OPZ        ANDEQ     '1'
043000960115     C     NRR6          ANDEQ     0
043100960115     C                   EXSR      ROLTES
043200960109     C                   END
043300960110     C*----------------------------------------------------*
043400960110     C     NRR6          IFEQ      0
043500960110     C                   MOVEL     '3'           WTPVID
043600960110     C                   END
043700960109     C*----------------------------------------------------*
043800960109     C                   ENDSR
043900960109     C*----------------------------------------------------*
044000960109     C*  ROLDES: Carico riga di dettaglio                  *
044100960109     C*----------------------------------------------------*
044200960109     C     ROLDES        BEGSR
044300960109     C*
044400960109     C                   DO        17
044500960116     C                   SETOFF                                       92
044600960109     C                   MOVEL     *BLANKS       V5CKEY
044700960109     C                   MOVEL     *BLANKS       V5DCOD
044800960109     C                   ADD       1             NRR5
044900960109     C                   WRITE     TB69S05
045000960109     C                   END
045100960109     C*
045200960109     C                   ENDSR
045300960109     C*----------------------------------------------------*
045400960109     C*  ROLTES: Carico riga di dettaglio                  *
045500960109     C*----------------------------------------------------*
045600960109     C     ROLTES        BEGSR
045700960109     C*
045800960109     C                   MOVEL     *BLANKS       V6CKEY
045900960109     C                   MOVEL     *BLANKS       V6DSTP
046000080228     C                   MOVEL     *BLANKS       V6pda
046100960109     C                   ADD       1             NRR6
046200960116     C                   SETOFF                                       92
046300960109     C                   WRITE     TB69S06
046400960109     C*
046500960109     C                   ENDSR
046600960109     C*----------------------------------------------------*
046700960109     C*  WRTDES: Gestione scrittura videata descrizioni    *
046800940928     C*----------------------------------------------------*
046900960109     C     WRTDES        BEGSR
047000940928     C*----------------------------------------------------*
047100940929     C                   MOVEL     'N'           WSELEZ            1
047200960110     C                   MOVEL     ' '           DS7F03
047300960109     C                   Z-ADD     1             NRR5
047400960109     C     NRR5          CHAIN     TB69S05                            31
047500940928     C     *IN31         DOWEQ     '0'
047600940928     C     *IN28         ANDEQ     '0'
047700940928     C                   SETOFF                                       40
047800960109     C*  Variazione
047900960109     C     V5CKEY        IFNE      *BLANKS
048000960109     C                   MOVEL     'S'           WSELEZ
048100960109     C                   MOVEL     V5CKEY        KKEY
048200960110     C     KTAB2         CHAIN     TABEL00F                           32
048300960110     C                   MOVEL     '1'           TBLFTT
048400960110     C                   MOVEL     *BLANKS       TBLUNI
048500080227     C****               MOVEL     V5DCOD        TBLUNI
048600080227     C                   MOVEL     V5DCOD        §7Vnot
048700080227     C                   MOVEL     ds7V          TBLUNI
048800960109     C                   MOVEL     *BLANKS       TBLFTR
048900960109     C                   MOVEL     *ZEROS        TBLDTR
049000960109     C     V5DCOD        IFNE      *BLANKS
049100960109     C  N32              UPDATE    TABEL
049200960109     C                   ELSE
049300960109     C*  Se ho abblencato riga gestisco l'annullamento
049400960111     C                   MOVEL     '*'           TBLFLG
049500960110     C                   MOVEL     '1'           TBLFTT
049600960109     C                   MOVEL     *BLANKS       TBLFTR
049700960109     C                   MOVEL     *ZEROS        TBLDTR
049800960109     C  N32              UPDATE    TABEL
049900960109     C                   END
050000960109     C                   ELSE
050100960109     C*  Nuova riga
050200960110     C     V5DCOD        IFNE      *BLANKS
050300141128     c                   clear                   TBLflg
050400960109     C                   MOVEL     'S'           WSELEZ
050500960109     C                   ADD       1             WNRDES
050600960109     C                   Z-ADD     WNRDES        WPROGG
050700960109     C                   MOVEL     'Q'           WTPPAR
050800960109     C                   MOVEL     DS7COD        WCDPAR
050900960110     C                   MOVEL     'V'           WTPELE
051000960109     C                   Z-ADD     1             TBLKUT
051100960109     C                   MOVEL     WKEY02        TBLKEY
051200960109     C                   MOVEL     '7V'          TBLCOD
051300960109     C                   MOVEL     '1'           TBLFTT
051400960110     C                   MOVEL     *BLANKS       TBLUNI
051500080227     C******             MOVEL     V5DCOD        TBLUNI
051600080227     C                   MOVEL     V5DCOD        §7Vnot
051700080227     C                   MOVEL     ds7V          TBLUNI
051800960110     C                   WRITE     TABEL
051900960109     C                   END
052000960109     C                   END
052100960109     C*  Lettura successiva
052200960109     C                   ADD       1             NRR5
052300960109     C     NRR5          CHAIN     TB69S05                            31
052400960109     C                   END
052500960109     C*  Se non ho scritto nessuna nota do errore
052600960109     C     WSELEZ        IFEQ      'N'
052700960109     C                   SETON                                        28
052800960109     C                   MOVEL     ERR(1)        $MSG
052900960109     C                   ELSE
053000960109     C                   MOVEL     'S'           WFINE
053100960109     C                   END
053200940929     C*
053300960109     C                   ENDSR
053400960109     C*----------------------------------------------------*
053500960109     C*  WRTTES: Gestione scrittura videata descrizioni    *
053600960109     C*----------------------------------------------------*
053700960109     C     WRTTES        BEGSR
053800960109     C*----------------------------------------------------*
053900960109     C                   Z-ADD     1             NRR6
054000960109     C     NRR6          CHAIN     TB69S06                            31
054100960109     C     *IN31         DOWEQ     '0'
054200960109     C     *IN28         ANDEQ     '0'
054300960109     C                   SETOFF                                       40
054400960109     C*  Variazione
054500960109     C     V6CKEY        IFNE      *BLANKS
054600960109     C                   MOVEL     V6CKEY        KKEY
054700960110     C     KTAB2         CHAIN     TABEL00F                           32
054800960111     C                   MOVEL     *BLANKS       TBLUNI
054900080228     C*****              MOVEL     V6DSTP        TBLUNI
055000080228     C                   MOVEL     V6DSTP        §7vNOT
055100080228     C                   MOVEL     V6PDA         §7vPDA
055200080228     C                   MOVEL     ds7V          TBLUNI
055300960109     C                   MOVEL     *BLANKS       TBLFTR
055400960109     C                   MOVEL     *ZEROS        TBLDTR
055500960110     C     V6DSTP        IFNE      *BLANKS
055600960109     C  N32              UPDATE    TABEL
055700960109     C                   ELSE
055800960109     C*  Se ho abblencato riga gestisco l'annullamento
055900960111     C                   MOVEL     '*'           TBLFLG
056000960109     C                   MOVEL     *BLANKS       TBLFTR
056100960109     C                   MOVEL     *ZEROS        TBLDTR
056200960109     C  N32              UPDATE    TABEL
056300960109     C                   END
056400960109     C                   ELSE
056500960109     C*  Nuova riga
056600960110     C     V6DSTP        IFNE      *BLANKS
056700960111     C                   ADD       1             WNRTES
056800960111     C                   Z-ADD     WNRTES        WPROGG
056900960109     C                   MOVEL     'Q'           WTPPAR
057000960109     C                   MOVEL     DS7COD        WCDPAR
057100960109     C                   MOVEL     'S'           WTPELE
057200960109     C                   Z-ADD     1             TBLKUT
057300960109     C                   MOVEL     WKEY02        TBLKEY
057400960109     C                   MOVEL     '7V'          TBLCOD
057500960109     C                   MOVEL     '1'           TBLFTT
057600960111     C                   MOVEL     *BLANKS       TBLUNI
057700080228     C*****              MOVEL     V6DSTP        TBLUNI
057800080228     C                   MOVEL     V6DSTP        §7vNOT
057900080228     C                   MOVEL     V6pda         §7vPDA
058000080228     C                   MOVEL     ds7V          TBLUNI
058100960110     C                   WRITE     TABEL
058200960109     C                   END
058300960109     C                   END
058400960109     C*  Lettura successiva
058500960111     C                   ADD       1             NRR6
058600960109     C     NRR6          CHAIN     TB69S06                            31
058700960109     C                   END
058800960109     C*
058900960109     C                   ENDSR
059000960109     C*--------------------------------------------------------------*
059100960109     C*  CALL69: Richiamo pgm trtb69r1                               *
059200960109     C*--------------------------------------------------------------*
059300960109     C     CALL69        BEGSR
059400960109     C*
059500960116     C                   MOVEL     *BLANKS       DS7RIP
059600960116     C                   MOVEL     *BLANKS       DS7TRA
059700960109     C                   CALL      'TRTB69R1'
059800960109     C                   PARM                    KPJBA
059900960110     C                   MOVEL     KPJBU         DSTB69
060000960110     C                   MOVEL     'S'           WPGMIN
060100960110     C*  Controllo se devo aggiornare il fle descrizioni
060200960110     C     DS7F03        IFEQ      ' '
060300960109     C*  Se annullato codice annullo anche le descrizioni
060400960109     C     DS7RIP        IFEQ      'A'
060500960109     C                   EXSR      ANNUL
060600960109     C                   END
060700960109     C*  Se ripristinato codice riprisitno anche le descrizioni
060800960109     C     DS7RIP        IFEQ      'R'
060900960109     C                   EXSR      RIPRI
061000960109     C                   END
061100960109     C*  Se no trasmissione annullo trasmiss.
061200960109     C     DS7TRA        IFEQ      'N'
061300960109     C                   EXSR      NOTRA
061400960109     C                   END
061500960110     C                   ELSE
061600960110     C     DS7F03        IFEQ      '2'
061700960110     C                   MOVEL     '1'           WTPVID
061800960110     C                   MOVEL     'N'           WPGMIN
061900960110     C                   MOVEL     'N'           WFINE
062000960110     C                   END
062100960110     C                   END
062200960109     C*
062300960109     C                   ENDSR
062400960109     C*--------------------------------------------------------------*
062500960109     C*  ANNUL: Annullo codici tabella descrizioni                   *
062600960109     C*--------------------------------------------------------------*
062700960109     C     ANNUL         BEGSR
062800960109     C*
062900960109     C                   MOVEL     *BLANKS       KKEY
063000960109     C                   MOVEL     '7V'          KCOD
063100960109      *  Costruisco chiave per posizionamento su tabel
063200960109     C                   MOVEL     'Q'           WTPPAR
063300960109     C                   MOVEL     DS7COD        WCDPAR
063400960109     C                   MOVEL     WKEY00        KKEY
063500960109     C                   SETOFF                                       01
063600960109     C     KTAB2         SETLL     TABEL00F
063700960109     C     KTAB1         READE     TABEL00F                               31
063800960109     C     *IN31         DOWEQ     '0'
063900960109     C     WANKEY        ANDEQ     WKEY00
064000960111     C                   MOVEL     '*'           TBLFLG
064100960109     C                   EXCEPT    ANNTBL
064200960109     C     KTAB1         READE     TABEL00F                               31
064300960109     C                   END
064400960109     C*
064500960109     C                   ENDSR
064600960109     C*--------------------------------------------------------------*
064700960109     C*  RIPRI: Riprisitino codici tabella descrizioni               *
064800960109     C*--------------------------------------------------------------*
064900960109     C     RIPRI         BEGSR
065000960109     C*
065100960109     C                   MOVEL     *BLANKS       KKEY
065200960109     C                   MOVEL     '7V'          KCOD
065300960109      *  Costruisco chiave per posizionamento su tabel
065400960109     C                   MOVEL     'Q'           WTPPAR
065500960109     C                   MOVEL     DS7COD        WCDPAR
065600960109     C                   MOVEL     WKEY00        KKEY
065700960109     C                   SETOFF                                       01
065800960109     C     KTAB2         SETLL     TABEL00F
065900960109     C     KTAB1         READE     TABEL00F                               31
066000960109     C     *IN31         DOWEQ     '0'
066100960109     C     WANKEY        ANDEQ     WKEY00
066200960109     C                   MOVEL     ' '           TBLFLG
066300960109     C                   EXCEPT    ANNTBL
066400960109     C     KTAB1         READE     TABEL00F                               31
066500960109     C                   END
066600960109     C*
066700960109     C                   ENDSR
066800960109     C*--------------------------------------------------------------*
066900960109     C*  NOTRA: Tabella non in trasmissione                          *
067000960109     C*--------------------------------------------------------------*
067100960109     C     NOTRA         BEGSR
067200960109     C*
067300960109     C                   MOVEL     *BLANKS       KKEY
067400960109     C                   MOVEL     '7V'          KCOD
067500960109      *  Costruisco chiave per posizionamento su tabel
067600960109     C                   MOVEL     'Q'           WTPPAR
067700960109     C                   MOVEL     DS7COD        WCDPAR
067800960109     C                   MOVEL     WKEY00        KKEY
067900960109     C                   SETOFF                                       01
068000960109     C     KTAB2         SETLL     TABEL00F
068100960109     C     KTAB1         READE     TABEL00F                               31
068200960109     C     *IN31         DOWEQ     '0'
068300960109     C     WANKEY        ANDEQ     WKEY00
068400960109     C                   MOVEL     ' '           TBLFTT
068500960109     C                   EXCEPT    TRATBL
068600960109     C     KTAB1         READE     TABEL00F                               31
068700960109     C                   END
068800960109     C*
068900960109     C                   ENDSR
069000931209     C*--------------------------------------------------------------*
069100960109     C*  DEFCAM: Operazioni iniziali                                 *
069200931209     C*--------------------------------------------------------------*
069300960109     C     DEFCAM        BEGSR
069400931209     C*
069500931209     C*  Richiamo XPARUT
069600931209     C                   Z-ADD     1             CODUT
069700931209     C                   CALL      'XPARUT'
069800931209     C                   PARM                    UTEDSE
069900931209     C     CODUT         CABEQ     -1            FINE
070000960109     C                   MOVEL     RAGUT         V5CRSU
070100931209     C*----------------------------------------------------*
070200931209     C* Definizione chiavi                                 *
070300940927     C     KTAB1         KLIST
070400940927     C                   KFLD                    KKUT
070500940927     C                   KFLD                    KCOD
070600940928     C*
070700940928     C     KTAB2         KLIST
070800940928     C                   KFLD                    KKUT
070900940928     C                   KFLD                    KCOD
071000940928     C                   KFLD                    KKEY
071100931209     C*----------------------------------------------------*
071200931209     C* Definizione variabili                              *
071300940927     C     *LIKE         DEFINE    TBLKUT        KKUT
071400940927     C     *LIKE         DEFINE    TBLCOD        KCOD
071500940928     C     *LIKE         DEFINE    TBLKEY        KKEY
071600940725     C*----------------------------------------------------*
071700940927     C*  Inizializzazione variabili
071800940928     C                   Z-ADD     1             KKUT
071900940927     C                   MOVEL     'N'           WFINE             1
072000960109     C                   MOVEL     *BLANKS       V5DFUN
072100940928     C                   SETOFF                                       90
072200940228     C*----------------------------------------------------*
072300931209     C                   ENDSR
072400931209     O*-----------------------------------------------------*
072500960109     OTABEL     E            ANNTBL
072600960109     O                       TBLFLG
072700960109     OTABEL     E            TRATBL
072800960109     O                       TBLFTT
072900940927**
073000960109Immettere almeno una riga di descrizione                              01
073100960109Codice particolarità giacenza inesistente                             02
073200960110Codice particolarità giacenza annullato                               03
073300960110Cliente non abilitato alla gestione della particolarità giacenza      04
073400940419**
073500960109IMMISSIONE
073600960109VARIAZIONE
073700940927INTERROGAZIONE
