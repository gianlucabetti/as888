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
021600160308     c                   If        DS7GES = 'V'  or
021700160308     c                             DS7OPZ = '5'
021800160308     c     Ktab2         chain(N)  TABEL                              31
021900160308     c                   Else
022000960109     C     KTAB2         CHAIN     TABEL00F                           31
022100160308     c                   EndIf
022200960109     C*----------------------------------------------------*
022300960110     C     DS7RIC        IFEQ      'S'
022400960109     C*----------------------------------------------------*
022500960109     C     *IN31         IFEQ      '1'
022600960110     C                   MOVEL     ERR(2)        DS7MSG
022700960109     C                   MOVEL     '1'           DS7ERR
022800960109     C                   MOVEL     'S'           WFINE
022900960109     C                   GOTO      FINCTR
023000960109     C                   END
023100960109     C*  Se pgm richiamato x controllo verifico se cod.annull.
023200960109     C     *IN31         IFEQ      '0'
023300960109     C     TBLFLG        ANDNE     ' '
023400960109     C     DS7GES        ANDEQ     'C'
023500960109     C                   MOVEL     'S'           WFINE
023600960110     C                   MOVEL     ERR(3)        DS7MSG
023700960109     C                   MOVEL     '1'           DS7ERR
023800960109     C                   GOTO      FINCTR
023900960109     C                   END
024000960109     C*----------------------------------------------------*
024100960109     C*  Se devo controllare abilitazione di un cod.cliente
024200960109     C*  carico i dati dalla 7U
024300960111     C                   MOVEL     TBLUNI        DS7R
024400960111     C                   MOVEL     §7RELI        DS7ELI
024500960111     C                   MOVEL     §7RINS        DS7INS
024600960109     C     DS7KSC        IFNE      0
024700960111     C     §7RABC        ANDEQ     'S'
024800960109     C                   MOVEA     DS7CDP        CODP
024900960109     C     V5CCOD        LOOKUP    CODP                                   32
025000960109     C     *IN32         IFEQ      '0'
025100960109     C                   MOVEL     'S'           WFINE
025200960110     C                   MOVEL     ERR(4)        DS7MSG
025300960109     C                   MOVEL     '1'           DS7ERR
025400960109     C                   END
025500960109     C                   END
025600960109     C*----------------------------------------------------*
025700960109     C                   ELSE
025800960109     C*  Se non è richiamato controllo se sono in immissione
025900960109     C     *IN31         IFEQ      '1'
026000960110     C                   MOVEL     COST(1)       V5DFUN
026100960109     C                   ELSE
026200960110     C                   MOVEL     COST(2)       V5DFUN
026300960109     C                   END
026400960109     C*----------------------------------------------------*
026500960109     C                   END
026600960109     C*----------------------------------------------------*
026700960109     C     FINCTR        ENDSR
026800940928     C*----------------------------------------------------*
026900940928     C*  INZ01   Gestione inizializzazione prima videata   *
027000940928     C*----------------------------------------------------*
027100940927     C     INZ01         BEGSR
027200960109     C*  Inizializzo videata subfile note descrittive
027300940929     C                   MOVEL     '1'           WTPVID
027400960109     C                   SETOFF                                       2890
027500940418     C                   MOVEL     *BLANKS       $MSG
027600940928     C*----------------------------------------------------*
027700960109     C*  Controllo se gestisco visulizzazione
027800940929     C     DS7GES        IFEQ      'V'
027900960109     C     DS7OPZ        OREQ      '5'
028000960109     C                   SETON                                        90
028100960109     C                   MOVEL     COST(3)       V5DFUN
028200940928     C                   END
028300940927     C*----------------------------------------------------*
028400940927     C*  Inizializzazione subfile
028500940927     C                   SETOFF                                       2021
028600960111     C                   WRITE     TB70C05
028700960109     C                   Z-ADD     0             NRR5              4 0
028800940927     C                   SETON                                        21
028900940927     C*----------------------------------------------------*
029000940927     C*  Caricamento subfile
029100960109     C                   MOVEL     '7V'          KCOD
029200960109      *  Controllo se esitono note da stampare x bolla
029300960111     C                   MOVEL     'R'           WTPPAR
029400960109     C                   MOVEL     DS7COD        WCDPAR
029500960109     C                   MOVEL     'S'           WTPELE
029600960109     C                   Z-ADD     0             WPROGG
029700960109     C                   MOVEL     WKEY02        KKEY
029800960109     C                   SETOFF                                       94
029900960109     C     KTAB2         SETLL     TABEL00F
030000160308     c                   If        DS7GES = 'V'  or
030100160308     c                             DS7OPZ = '5'
030200160308     c     Ktab1         reade(N)  TABEL                                  31
030300160308     c                   Else
030400960109     C     KTAB1         READE     TABEL00F                               31
030500160308     c                   EndIf
030600960109     C     *IN31         IFEQ      '0'
030700960109     C     WSVKEY        ANDEQ     WKEY01
030800960109     C                   SETON                                        94
030900960109     C                   END
031000960109      *  Costruisco chiave per posizionamento su tabel
031100960109     C                   MOVEL     'V'           WTPELE
031200960111     C                   Z-ADD     0             WNRDES
031300960111     C                   MOVEL     WKEY02        KKEY
031400960109     C                   SETOFF                                       01
031500960109     C     KTAB2         SETLL     TABEL00F
031600160308     c                   If        DS7GES = 'V'  or
031700160308     c                             DS7OPZ = '5'
031800160308     c     Ktab1         reade(N)  TABEL                                  31
031900160308     c                   Else
032000960109     C     KTAB1         READE     TABEL00F                               31
032100160308     c                   EndIf
032200960109     C     *IN31         IFEQ      '0'
032300960109     C     WSVKEY        ANDEQ     WKEY01
032400960109     C*----------------------------------------------------*
032500940927     C     *IN31         DOWEQ     '0'
032600960109     C     WSVKEY        ANDEQ     WKEY01
032700960109     C     TBLUNI        IFNE      *BLANK
032800080227     C*******            MOVEL     TBLUNI        V5DCOD
032900080227     C                   MOVEL     TBLUNI        ds7V
033000080227     C                   MOVEL     §7Vnot        V5dCOD
033100960109     C                   MOVEL     TBLKEY        V5CKEY
033200960109     C                   ADD       1             NRR5
033300960109     C     TBLFLG        COMP      ' '                                929293
033400960111     C                   WRITE     TB70S05
033500960109     C                   END
033600960111     C                   Z-ADD     TBPROG        WNRDES            4 0
033700160308     c                   If        DS7GES = 'V'  or
033800160308     c                             DS7OPZ = '5'
033900160308     c     Ktab1         reade(N)  TABEL                                  31
034000160308     c                   Else
034100960109     C     KTAB1         READE     TABEL00F                               31
034200160308     c                   EndIf
034300960109     C                   SETON                                        01
034400960109     C                   END
034500940928     C*----------------------------------------------------*
034600960109     C                   END
034700960109     C*----------------------------------------------------*
034800960109     C*  Se il programma è richiamato in aggiornamento
034900960109     C*  carico una pagina di righe vuote
035000960109     C     DS7GES        IFEQ      ' '
035100960110     C     DS7OPZ        ANDNE     '5'
035200960109     C                   EXSR      ROLDES
035300960109     C                   END
035400960109     C*----------------------------------------------------*
035500940928     C                   ENDSR
035600960109     C*----------------------------------------------------*
035700960109     C*  INZ02   Gestione inizializzazione seconda videata *
035800960109     C*----------------------------------------------------*
035900960109     C     INZ02         BEGSR
036000960109     C*  Inizializzo videata subfile note descrittive
036100960110     C                   MOVEL     '2'           WTPVID
036200960109     C                   SETOFF                                       2890
036300960109     C                   MOVEL     *BLANKS       $MSG
036400960111     C*----------------------------------------------------*
036500960111     C*  Controllo se gestisco visulizzazione
036600960111     C     DS7GES        IFEQ      'V'
036700960111     C     DS7OPZ        OREQ      '5'
036800960111     C                   SETON                                        90
036900960111     C                   MOVEL     COST(3)       V5DFUN
037000960111     C                   END
037100960109     C*----------------------------------------------------*
037200960109     C*  Inizializzazione subfile
037300960109     C                   SETOFF                                       2021
037400960111     C                   WRITE     TB70C06
037500960109     C                   Z-ADD     0             NRR6              4 0
037600960109     C                   SETON                                        21
037700110412     C                   movel     V5Cpgm        V6Cpgm
037800110412     C                   movel     V5Crsu        V6Crsu
037900960109     C                   MOVEL     V5DFUN        V6DFUN
038000960109     C*----------------------------------------------------*
038100960109     C*  Caricamento subfile
038200960109     C                   MOVEL     '7V'          KCOD
038300960109      *  Costruisco chiave per posizionamento su tabel
038400960111     C                   MOVEL     'R'           WTPPAR
038500960109     C                   MOVEL     DS7COD        WCDPAR
038600960109     C                   MOVEL     'S'           WTPELE
038700960109     C                   Z-ADD     0             WPROGG
038800960109     C                   MOVEL     WKEY02        KKEY
038900960111     C                   Z-ADD     0             WNRTES
039000960109     C                   SETOFF                                       01
039100960109     C     KTAB2         SETLL     TABEL00F
039200160308     c                   If        DS7GES = 'V'  or
039300160308     c                             DS7OPZ = '5'
039400160308     c     Ktab1         reade(N)  TABEL                                  31
039500160308     c                   Else
039600960109     C     KTAB1         READE     TABEL00F                               31
039700160308     c                   EndIf
039800960109     C     *IN31         IFEQ      '0'
039900960109     C     WSVKEY        ANDEQ     WKEY01
040000960109     C*----------------------------------------------------*
040100960109     C     *IN31         DOWEQ     '0'
040200960109     C     WSVKEY        ANDEQ     WKEY01
040300960109     C     TBLUNI        IFNE      *BLANK
040400960109     C                   MOVEL     TBLKEY        V6CKEY
040500080228     C****               MOVEL     TBLUNI        V6DSTP
040600080228     C                   MOVEL     TBLUNI        ds7V
040700080228     C                   MOVEL     §7vNOT        V6DSTP
040800080228     C                   MOVEL     §7vPDA        V6pda
040900960109     C                   ADD       1             NRR6
041000960109     C     TBLFLG        COMP      ' '                                929293
041100960111     C                   WRITE     TB70S06
041200960109     C                   END
041300960111     C                   Z-ADD     TBPROG        WNRTES            4 0
041400160308     c                   If        DS7GES = 'V'  or
041500160308     c                             DS7OPZ = '5'
041600160308     c     Ktab1         reade(N)  TABEL                                  31
041700160308     c                   Else
041800960109     C     KTAB1         READE     TABEL00F                               31
041900160308     c                   EndIf
042000960109     C                   SETON                                        01
042100960109     C                   END
042200960109     C*----------------------------------------------------*
042300960109     C                   END
042400960109     C*----------------------------------------------------*
042500960109     C*  Se il programma è richiamato in aggiornamento
042600960109     C*  carico una pagina di righe vuote
042700960109     C     DS7GES        IFEQ      ' '
042800960109     C     DS7OPZ        ANDEQ     '1'
042900960115     C     NRR6          ANDEQ     0
043000960109     C                   EXSR      ROLTES
043100960109     C                   END
043200960110     C*----------------------------------------------------*
043300960110     C     NRR6          IFEQ      0
043400960110     C                   MOVEL     '3'           WTPVID
043500960110     C                   END
043600960109     C*----------------------------------------------------*
043700960109     C                   ENDSR
043800960109     C*----------------------------------------------------*
043900960109     C*  ROLDES: Carico riga di dettaglio                  *
044000960109     C*----------------------------------------------------*
044100960109     C     ROLDES        BEGSR
044200960109     C*
044300960109     C                   DO        17
044400960117     C                   SETOFF                                       92
044500960109     C                   MOVEL     *BLANKS       V5CKEY
044600960109     C                   MOVEL     *BLANKS       V5DCOD
044700960109     C                   ADD       1             NRR5
044800960111     C                   WRITE     TB70S05
044900960109     C                   END
045000960109     C*
045100960109     C                   ENDSR
045200960109     C*----------------------------------------------------*
045300960109     C*  ROLTES: Carico riga di dettaglio                  *
045400960109     C*----------------------------------------------------*
045500960109     C     ROLTES        BEGSR
045600960109     C*
045700960109     C                   MOVEL     *BLANKS       V6CKEY
045800960109     C                   MOVEL     *BLANKS       V6DSTP
045900080228     C                   MOVEL     *BLANKS       V6pda
046000960109     C                   ADD       1             NRR6
046100960117     C                   SETOFF                                       92
046200960111     C                   WRITE     TB70S06
046300960109     C*
046400960109     C                   ENDSR
046500960109     C*----------------------------------------------------*
046600960109     C*  WRTDES: Gestione scrittura videata descrizioni    *
046700940928     C*----------------------------------------------------*
046800960109     C     WRTDES        BEGSR
046900940928     C*----------------------------------------------------*
047000940929     C                   MOVEL     'N'           WSELEZ            1
047100960110     C                   MOVEL     ' '           DS7F03
047200960109     C                   Z-ADD     1             NRR5
047300960111     C     NRR5          CHAIN     TB70S05                            31
047400940928     C     *IN31         DOWEQ     '0'
047500940928     C     *IN28         ANDEQ     '0'
047600940928     C                   SETOFF                                       40
047700960109     C*  Variazione
047800960109     C     V5CKEY        IFNE      *BLANKS
047900960109     C                   MOVEL     'S'           WSELEZ
048000960109     C                   MOVEL     V5CKEY        KKEY
048100960110     C     KTAB2         CHAIN     TABEL00F                           32
048200960110     C                   MOVEL     '1'           TBLFTT
048300960110     C                   MOVEL     *BLANKS       TBLUNI
048400080227     C****               MOVEL     V5DCOD        TBLUNI
048500080227     C                   MOVEL     V5DCOD        §7Vnot
048600080227     C                   MOVEL     ds7V          TBLUNI
048700960109     C                   MOVEL     *BLANKS       TBLFTR
048800960109     C                   MOVEL     *ZEROS        TBLDTR
048900960109     C     V5DCOD        IFNE      *BLANKS
049000960109     C  N32              UPDATE    TABEL
049100960109     C                   ELSE
049200960109     C*  Se ho abblencato riga gestisco l'annullamento
049300960111     C                   MOVEL     '*'           TBLFLG
049400960110     C                   MOVEL     '1'           TBLFTT
049500960109     C                   MOVEL     *BLANKS       TBLFTR
049600960109     C                   MOVEL     *ZEROS        TBLDTR
049700960109     C  N32              UPDATE    TABEL
049800960109     C                   END
049900960109     C                   ELSE
050000960109     C*  Nuova riga
050100960110     C     V5DCOD        IFNE      *BLANKS
050200960109     C                   MOVEL     'S'           WSELEZ
050300960109     C                   ADD       1             WNRDES
050400960109     C                   Z-ADD     WNRDES        WPROGG
050500960111     C                   MOVEL     'R'           WTPPAR
050600960109     C                   MOVEL     DS7COD        WCDPAR
050700960110     C                   MOVEL     'V'           WTPELE
050800960109     C                   Z-ADD     1             TBLKUT
050900960109     C                   MOVEL     WKEY02        TBLKEY
051000960109     C                   MOVEL     '7V'          TBLCOD
051100960109     C                   MOVEL     '1'           TBLFTT
051200960110     C                   MOVEL     *BLANKS       TBLUNI
051300080227     C******             MOVEL     V5DCOD        TBLUNI
051400080227     C                   MOVEL     V5DCOD        §7Vnot
051500080227     C                   MOVEL     ds7V          TBLUNI
051600960110     C                   WRITE     TABEL
051700140507      * Si memorizza come se fosse caricato da file x un eventuale UPDATE
051800140507      * successivo
051900140507     C                   MOVEL     TBLKEY        V5CKEY
052000140507     c                   update    TB70S05
052100140507      ********
052200960109     C                   END
052300960109     C                   END
052400960109     C*  Lettura successiva
052500960109     C                   ADD       1             NRR5
052600960111     C     NRR5          CHAIN     TB70S05                            31
052700960109     C                   END
052800960109     C*  Se non ho scritto nessuna nota do errore
052900960109     C     WSELEZ        IFEQ      'N'
053000960109     C                   SETON                                        28
053100960109     C                   MOVEL     ERR(1)        $MSG
053200960109     C                   ELSE
053300960109     C                   MOVEL     'S'           WFINE
053400960109     C                   END
053500940929     C*
053600960109     C                   ENDSR
053700960109     C*----------------------------------------------------*
053800960109     C*  WRTTES: Gestione scrittura videata descrizioni    *
053900960109     C*----------------------------------------------------*
054000960109     C     WRTTES        BEGSR
054100960109     C*----------------------------------------------------*
054200960109     C                   Z-ADD     1             NRR6
054300960111     C     NRR6          CHAIN     TB70S06                            31
054400960109     C     *IN31         DOWEQ     '0'
054500960109     C     *IN28         ANDEQ     '0'
054600960109     C                   SETOFF                                       40
054700960109     C*  Variazione
054800960109     C     V6CKEY        IFNE      *BLANKS
054900960109     C                   MOVEL     V6CKEY        KKEY
055000960110     C     KTAB2         CHAIN     TABEL00F                           32
055100960111     C                   MOVEL     *BLANKS       TBLUNI
055200080228     C****               MOVEL     V6DSTP        TBLUNI
055300080228     C                   MOVEL     V6DSTP        §7vNOT
055400080228     C                   MOVEL     V6pda         §7vPDA
055500080228     C                   MOVEL     ds7V          TBLUNI
055600960109     C                   MOVEL     *BLANKS       TBLFTR
055700960109     C                   MOVEL     *ZEROS        TBLDTR
055800960110     C     V6DSTP        IFNE      *BLANKS
055900960109     C  N32              UPDATE    TABEL
056000960109     C                   ELSE
056100960109     C*  Se ho abblencato riga gestisco l'annullamento
056200960111     C                   MOVEL     '*'           TBLFLG
056300960109     C                   MOVEL     *BLANKS       TBLFTR
056400960109     C                   MOVEL     *ZEROS        TBLDTR
056500960109     C  N32              UPDATE    TABEL
056600960109     C                   END
056700960109     C                   ELSE
056800960109     C*  Nuova riga
056900960110     C     V6DSTP        IFNE      *BLANKS
057000960111     C                   ADD       1             WNRTES
057100960111     C                   Z-ADD     WNRTES        WPROGG
057200960111     C                   MOVEL     'R'           WTPPAR
057300960109     C                   MOVEL     DS7COD        WCDPAR
057400960109     C                   MOVEL     'S'           WTPELE
057500960109     C                   Z-ADD     1             TBLKUT
057600960109     C                   MOVEL     WKEY02        TBLKEY
057700960109     C                   MOVEL     '7V'          TBLCOD
057800960109     C                   MOVEL     '1'           TBLFTT
057900960111     C                   MOVEL     *BLANKS       TBLUNI
058000080228     C*****              MOVEL     V6DSTP        TBLUNI
058100080228     C                   MOVEL     V6DSTP        §7vNOT
058200080228     C                   MOVEL     V6pda         §7vPDA
058300080228     C                   MOVEL     ds7V          TBLUNI
058400960110     C                   WRITE     TABEL
058500960109     C                   END
058600960109     C                   END
058700960109     C*  Lettura successiva
058800960111     C                   ADD       1             NRR6
058900960111     C     NRR6          CHAIN     TB70S06                            31
059000960109     C                   END
059100960109     C*
059200960109     C                   ENDSR
059300960109     C*--------------------------------------------------------------*
059400960111     C*  CALL70: Richiamo pgm trtb70r1                               *
059500960109     C*--------------------------------------------------------------*
059600960111     C     CALL70        BEGSR
059700960109     C*
059800960117     C                   MOVEL     *BLANKS       DS7RIP
059900960117     C                   MOVEL     *BLANKS       DS7TRA
060000960111     C                   CALL      'TRTB70R1'
060100960109     C                   PARM                    KPJBA
060200960111     C                   MOVEL     KPJBU         DSTB70
060300960110     C                   MOVEL     'S'           WPGMIN
060400960110     C*  Controllo se devo aggiornare il fle descrizioni
060500960110     C     DS7F03        IFEQ      ' '
060600960109     C*  Se annullato codice annullo anche le descrizioni
060700960109     C     DS7RIP        IFEQ      'A'
060800960109     C                   EXSR      ANNUL
060900960109     C                   END
061000960109     C*  Se ripristinato codice riprisitno anche le descrizioni
061100960109     C     DS7RIP        IFEQ      'R'
061200960109     C                   EXSR      RIPRI
061300960109     C                   END
061400960109     C*  Se no trasmissione annullo trasmiss.
061500960109     C     DS7TRA        IFEQ      'N'
061600960109     C                   EXSR      NOTRA
061700960109     C                   END
061800960110     C                   ELSE
061900960110     C     DS7F03        IFEQ      '2'
062000960110     C                   MOVEL     '1'           WTPVID
062100960110     C                   MOVEL     'N'           WPGMIN
062200960110     C                   MOVEL     'N'           WFINE
062300960110     C                   END
062400960110     C                   END
062500110608     C*
062600110608     C* -Ripulisco il flag DS7F03 (1=F3, 2=F12) perchè altrimenti?
062700110608     C*  diventa IMPOSSIBILE proseguire (va in loop il *pgm):?
062800110608     C*  risultano ammesio solo F3 e F12 (non più Invio).?
062900110608     C                   clear                   DS7F03
063000960109     C*
063100960109     C                   ENDSR
063200960109     C*--------------------------------------------------------------*
063300960109     C*  ANNUL: Annullo codici tabella descrizioni                   *
063400960109     C*--------------------------------------------------------------*
063500960109     C     ANNUL         BEGSR
063600960109     C*
063700960109     C                   MOVEL     *BLANKS       KKEY
063800960109     C                   MOVEL     '7V'          KCOD
063900960109      *  Costruisco chiave per posizionamento su tabel
064000960111     C                   MOVEL     'R'           WTPPAR
064100960109     C                   MOVEL     DS7COD        WCDPAR
064200960109     C                   MOVEL     WKEY00        KKEY
064300960109     C                   SETOFF                                       01
064400960109     C     KTAB2         SETLL     TABEL00F
064500960109     C     KTAB1         READE     TABEL00F                               31
064600960109     C     *IN31         DOWEQ     '0'
064700960109     C     WANKEY        ANDEQ     WKEY00
064800960111     C                   MOVEL     '*'           TBLFLG
064900960109     C                   EXCEPT    ANNTBL
065000960109     C     KTAB1         READE     TABEL00F                               31
065100960109     C                   END
065200960109     C*
065300960109     C                   ENDSR
065400960109     C*--------------------------------------------------------------*
065500960109     C*  RIPRI: Riprisitino codici tabella descrizioni               *
065600960109     C*--------------------------------------------------------------*
065700960109     C     RIPRI         BEGSR
065800960109     C*
065900960109     C                   MOVEL     *BLANKS       KKEY
066000960109     C                   MOVEL     '7V'          KCOD
066100960109      *  Costruisco chiave per posizionamento su tabel
066200960111     C                   MOVEL     'R'           WTPPAR
066300960109     C                   MOVEL     DS7COD        WCDPAR
066400960109     C                   MOVEL     WKEY00        KKEY
066500960109     C                   SETOFF                                       01
066600960109     C     KTAB2         SETLL     TABEL00F
066700960109     C     KTAB1         READE     TABEL00F                               31
066800960109     C     *IN31         DOWEQ     '0'
066900960109     C     WANKEY        ANDEQ     WKEY00
067000960109     C                   MOVEL     ' '           TBLFLG
067100960109     C                   EXCEPT    ANNTBL
067200960109     C     KTAB1         READE     TABEL00F                               31
067300960109     C                   END
067400960109     C*
067500960109     C                   ENDSR
067600960109     C*--------------------------------------------------------------*
067700960109     C*  NOTRA: Tabella non in trasmissione                          *
067800960109     C*--------------------------------------------------------------*
067900960109     C     NOTRA         BEGSR
068000960109     C*
068100960109     C                   MOVEL     *BLANKS       KKEY
068200960109     C                   MOVEL     '7V'          KCOD
068300960109      *  Costruisco chiave per posizionamento su tabel
068400960111     C                   MOVEL     'R'           WTPPAR
068500960109     C                   MOVEL     DS7COD        WCDPAR
068600960109     C                   MOVEL     WKEY00        KKEY
068700960109     C                   SETOFF                                       01
068800960109     C     KTAB2         SETLL     TABEL00F
068900960109     C     KTAB1         READE     TABEL00F                               31
069000960109     C     *IN31         DOWEQ     '0'
069100960109     C     WANKEY        ANDEQ     WKEY00
069200960109     C                   MOVEL     ' '           TBLFTT
069300960109     C                   EXCEPT    TRATBL
069400960109     C     KTAB1         READE     TABEL00F                               31
069500960109     C                   END
069600960109     C*
069700960109     C                   ENDSR
069800931209     C*--------------------------------------------------------------*
069900960109     C*  DEFCAM: Operazioni iniziali                                 *
070000931209     C*--------------------------------------------------------------*
070100960109     C     DEFCAM        BEGSR
070200931209     C*
070300931209     C*  Richiamo XPARUT
070400931209     C                   Z-ADD     1             CODUT
070500931209     C                   CALL      'XPARUT'
070600931209     C                   PARM                    UTEDSE
070700931209     C     CODUT         CABEQ     -1            FINE
070800960109     C                   MOVEL     RAGUT         V5CRSU
070900931209     C*----------------------------------------------------*
071000931209     C* Definizione chiavi                                 *
071100940927     C     KTAB1         KLIST
071200940927     C                   KFLD                    KKUT
071300940927     C                   KFLD                    KCOD
071400940928     C*
071500940928     C     KTAB2         KLIST
071600940928     C                   KFLD                    KKUT
071700940928     C                   KFLD                    KCOD
071800940928     C                   KFLD                    KKEY
071900931209     C*----------------------------------------------------*
072000931209     C* Definizione variabili                              *
072100940927     C     *LIKE         DEFINE    TBLKUT        KKUT
072200940927     C     *LIKE         DEFINE    TBLCOD        KCOD
072300940928     C     *LIKE         DEFINE    TBLKEY        KKEY
072400940725     C*----------------------------------------------------*
072500940927     C*  Inizializzazione variabili
072600940928     C                   Z-ADD     1             KKUT
072700940927     C                   MOVEL     'N'           WFINE             1
072800960109     C                   MOVEL     *BLANKS       V5DFUN
072900940928     C                   SETOFF                                       90
073000940228     C*----------------------------------------------------*
073100931209     C                   ENDSR
073200931209     O*-----------------------------------------------------*
073300960109     OTABEL     E            ANNTBL
073400960109     O                       TBLFLG
073500960109     OTABEL     E            TRATBL
073600960109     O                       TBLFTT
073700940927**
073800960109Immettere almeno una riga di descrizione                              01
073900960111Codice particolarità consegne inesistente                             02
074000960111Codice particolarità consegne annullato                               03
074100960111Cliente non abilitato alla gestione della particolarità consegne      04
074200940419**
074300110412   IMMISSIONE
074400110412   VARIAZIONE
074500110412 INTERROGAZIONE
