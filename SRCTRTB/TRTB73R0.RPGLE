000100160308     /*END
000200891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000300891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000400960109     F*  KC        FINE LAVORO                                       *
000500960109     F*  KF        CONFERMA                                          *
000600960109     F*  KG        DATI STAMPA SU BOLLA                              *
000700960109     F*  KL        RITORNO                                           *
000800891030     F*--------------------------------------------------------------*
000900960115     FTRTB73D   CF   E             WORKSTN
001000960115     F                                     SFILE(TB73S04:NRR4)
001100960115     F                                     SFILE(TB73S05:NRR5)
001200940418     F*----------
001300960110     FTABEL00F  UF A E           K DISK
001400940418     D*--------------------------------------------------------------*
001500940418     D*   SCHIERE                                                    *
001600940701     D*--------------------------------------------------------------*
001700960110     D ERR             S             70    DIM(4) CTDATA PERRCD(1)
001800960109     D COST            S             16    DIM(3) CTDATA PERRCD(1)
001900960115     D CODP            S              2    DIM(30)
002000960115     D C7S             S              2    DIM(100)
002100960115     D D7S             S             89    DIM(100)
002200940928     D*--------------------------------------------------------------*
002300940928     D*  DS                                                          *
002400940928     D*--------------------------------------------------------------*
002500080227     D DS7V          E DS
002600960115     D DS7S          E DS
002700960115     D DSTB73        E DS                  EXTNAME(DS7PQRS)
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
004900160308     D  V4CPGM                 1     10
005000931209     C*--------------------------------------------------------------*
005100931209     C*    C I C L O    P R I N C I P A L E                          *
005200931209     C*--------------------------------------------------------------*
005300960109     C     *ENTRY        PLIST
005400960109     C                   PARM                    KPJBA
005500960115     C                   MOVEL     KPJBU         DSTB73
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
008400960115     C                   EXSR      CALL73
008500960110     C                   END
008600960110     C*
008700960110     C                   END
008800960110     C*
008900960109     C                   END
009000931209     C* Fine programma
009100931209     C     FINE          TAG
009200960115     C                   MOVEL     DSTB73        KPJBU
009300960110     C* Libero tabel
009400960110     C     *LOVAL        SETLL     TABEL00F
009500960109     C* Se il programma è richiamato da altre funzioni chiudo in LR
009600960109     C* altrimenti chiudo in RT
009700960115     C     DS7RIC        IFEQ      'S'
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
010900960115     C                   WRITE     TB73Z04
011000960115     C                   EXFMT     TB73C04
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
015500960115     C                   WRITE     TB73Z05
015600960115     C                   EXFMT     TB73C05
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
018800960115     C                   WRITE     TB73D05
018900960115     C                   EXFMT     TB73C05
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
020900960115     C                   MOVEL     '7S'          KCOD
021000960109     C                   MOVEL     *BLANKS       KKEY
021100960109     C                   MOVEL     DS7COD        KKEY
021200960209     C                   MOVEL     DS7COD        V4CCOD
021300960209     C                   MOVEL     DS7COD        V5CCOD
021400960109     C                   MOVEL     ' '           DS7ERR
021500960115     C                   CLEAR                   DS7S
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
024300960115     C                   MOVEL     TBLUNI        DS7S
024400960115     C                   MOVEL     §7SELI        DS7ELI
024500960115     C                   MOVEL     §7SINS        DS7INS
024600960109     C     DS7KSC        IFNE      0
024700960115     C     §7SABC        ANDEQ     'S'
024800960109     C                   MOVEA     DS7CDP        CODP
024900960115     C     V4CCOD        LOOKUP    CODP                                   32
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
026000960115     C                   MOVEL     COST(1)       V4DFUN
026100960109     C                   ELSE
026200960115     C                   MOVEL     COST(2)       V4DFUN
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
028100960115     C                   MOVEL     COST(3)       V4DFUN
028200940928     C                   END
028300940927     C*----------------------------------------------------*
028400940927     C*  Inizializzazione subfile
028500940927     C                   SETOFF                                       2021
028600960115     C                   WRITE     TB73C04
028700960115     C                   Z-ADD     0             NRR4              4 0
028800940927     C                   SETON                                        21
028900940927     C*----------------------------------------------------*
029000940927     C*  Caricamento subfile
029100960109     C                   MOVEL     '7V'          KCOD
029200960109      *  Controllo se esitono note da stampare x bolla
029300960115     C                   MOVEL     'S'           WTPPAR
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
032800080227     C*****              MOVEL     TBLUNI        V4DCOD
032900080227     C                   MOVEL     TBLUNI        ds7V
033000080227     C                   MOVEL     §7Vnot        V4dCOD
033100960115     C                   MOVEL     TBLKEY        V4CKEY
033200960115     C                   ADD       1             NRR4
033300960109     C     TBLFLG        COMP      ' '                                929293
033400960115     C                   WRITE     TB73S04
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
036400960109     C*----------------------------------------------------*
036500960109     C*  Inizializzazione subfile
036600960109     C                   SETOFF                                       2021
036700960115     C                   WRITE     TB73C05
036800960115     C                   Z-ADD     0             NRR5              4 0
036900960109     C                   SETON                                        21
037000160308     C                   movel     V4Cpgm        V5Cpgm
037100160308     C                   movel     V4Crsu        V5Crsu
037200960115     C                   MOVEL     V4DFUN        V5DFUN
037300960109     C*----------------------------------------------------*
037400960109     C*  Caricamento subfile
037500960109     C                   MOVEL     '7V'          KCOD
037600960109      *  Costruisco chiave per posizionamento su tabel
037700960115     C                   MOVEL     'S'           WTPPAR
037800960109     C                   MOVEL     DS7COD        WCDPAR
037900960109     C                   MOVEL     'S'           WTPELE
038000960109     C                   Z-ADD     0             WPROGG
038100960109     C                   MOVEL     WKEY02        KKEY
038200960111     C                   Z-ADD     0             WNRTES
038300960109     C                   SETOFF                                       01
038400960109     C     KTAB2         SETLL     TABEL00F
038500160308     c                   If        DS7GES = 'V'  or
038600160308     c                             DS7OPZ = '5'
038700160308     c     Ktab1         reade(N)  TABEL                                  31
038800160308     c                   Else
038900960109     C     KTAB1         READE     TABEL00F                               31
039000160308     c                   EndIf
039100960109     C     *IN31         IFEQ      '0'
039200960109     C     WSVKEY        ANDEQ     WKEY01
039300960109     C*----------------------------------------------------*
039400960109     C     *IN31         DOWEQ     '0'
039500960109     C     WSVKEY        ANDEQ     WKEY01
039600960109     C     TBLUNI        IFNE      *BLANK
039700960115     C                   MOVEL     TBLKEY        V5CKEY
039800080228     C****               MOVEL     TBLUNI        V5DSTP
039900080228     C                   MOVEL     TBLUNI        ds7V
040000080228     C                   MOVEL     §7vNOT        V5DSTP
040100080228     C                   MOVEL     §7vPDA        V5pda
040200960115     C                   ADD       1             NRR5
040300960109     C     TBLFLG        COMP      ' '                                929293
040400960115     C                   WRITE     TB73S05
040500960109     C                   END
040600960111     C                   Z-ADD     TBPROG        WNRTES            4 0
040700160308     c                   If        DS7GES = 'V'  or
040800160308     c                             DS7OPZ = '5'
040900160308     c     Ktab1         reade(N)  TABEL                                  31
041000160308     c                   Else
041100960109     C     KTAB1         READE     TABEL00F                               31
041200160308     c                   EndIf
041300960109     C                   SETON                                        01
041400960109     C                   END
041500960109     C*----------------------------------------------------*
041600960109     C                   END
041700960109     C*----------------------------------------------------*
041800960109     C*  Se il programma è richiamato in aggiornamento
041900960109     C*  carico una pagina di righe vuote
042000960109     C     DS7GES        IFEQ      ' '
042100960109     C     DS7OPZ        ANDEQ     '1'
042200960115     C     NRR5          ANDEQ     0
042300960109     C                   EXSR      ROLTES
042400960109     C                   END
042500960110     C*----------------------------------------------------*
042600960115     C     NRR5          IFEQ      0
042700960110     C                   MOVEL     '3'           WTPVID
042800960110     C                   END
042900960109     C*----------------------------------------------------*
043000960109     C                   ENDSR
043100960109     C*----------------------------------------------------*
043200960109     C*  ROLDES: Carico riga di dettaglio                  *
043300960109     C*----------------------------------------------------*
043400960109     C     ROLDES        BEGSR
043500960109     C*
043600960109     C                   DO        17
043700960115     C                   MOVEL     *BLANKS       V4CKEY
043800960115     C                   MOVEL     *BLANKS       V4DCOD
043900960115     C                   ADD       1             NRR4
044000960109     C     TBLFLG        COMP      ' '                                929293
044100960115     C                   WRITE     TB73S04
044200960109     C                   END
044300960109     C*
044400960109     C                   ENDSR
044500960109     C*----------------------------------------------------*
044600960109     C*  ROLTES: Carico riga di dettaglio                  *
044700960109     C*----------------------------------------------------*
044800960109     C     ROLTES        BEGSR
044900960109     C*
045000960115     C                   MOVEL     *BLANKS       V5CKEY
045100960115     C                   MOVEL     *BLANKS       V5DSTP
045200080228     C                   MOVEL     *BLANKS       V5pda
045300960115     C                   ADD       1             NRR5
045400960109     C     TBLFLG        COMP      ' '                                929293
045500960115     C                   WRITE     TB73S05
045600960109     C*
045700960109     C                   ENDSR
045800960109     C*----------------------------------------------------*
045900960109     C*  WRTDES: Gestione scrittura videata descrizioni    *
046000940928     C*----------------------------------------------------*
046100960109     C     WRTDES        BEGSR
046200940928     C*----------------------------------------------------*
046300940929     C                   MOVEL     'N'           WSELEZ            1
046400960110     C                   MOVEL     ' '           DS7F03
046500960109     C                   Z-ADD     1             NRR5
046600960115     C     NRR5          CHAIN     TB73S04                            31
046700940928     C     *IN31         DOWEQ     '0'
046800940928     C     *IN28         ANDEQ     '0'
046900940928     C                   SETOFF                                       40
047000960109     C*  Variazione
047100960115     C     V4CKEY        IFNE      *BLANKS
047200960109     C                   MOVEL     'S'           WSELEZ
047300960115     C                   MOVEL     V4CKEY        KKEY
047400960110     C     KTAB2         CHAIN     TABEL00F                           32
047500960110     C                   MOVEL     '1'           TBLFTT
047600960110     C                   MOVEL     *BLANKS       TBLUNI
047700080227     C*****              MOVEL     V4DCOD        TBLUNI
047800080227     C                   MOVEL     V4DCOD        §7Vnot
047900080227     C                   MOVEL     ds7V          TBLUNI
048000960109     C                   MOVEL     *BLANKS       TBLFTR
048100960109     C                   MOVEL     *ZEROS        TBLDTR
048200960115     C     V4DCOD        IFNE      *BLANKS
048300960109     C  N32              UPDATE    TABEL
048400960109     C                   ELSE
048500960109     C*  Se ho abblencato riga gestisco l'annullamento
048600960111     C                   MOVEL     '*'           TBLFLG
048700960110     C                   MOVEL     '1'           TBLFTT
048800960109     C                   MOVEL     *BLANKS       TBLFTR
048900960109     C                   MOVEL     *ZEROS        TBLDTR
049000960109     C  N32              UPDATE    TABEL
049100960109     C                   END
049200960109     C                   ELSE
049300960109     C*  Nuova riga
049400960115     C     V4DCOD        IFNE      *BLANKS
049500960109     C                   MOVEL     'S'           WSELEZ
049600960109     C                   ADD       1             WNRDES
049700960109     C                   Z-ADD     WNRDES        WPROGG
049800960115     C                   MOVEL     'S'           WTPPAR
049900960109     C                   MOVEL     DS7COD        WCDPAR
050000960110     C                   MOVEL     'V'           WTPELE
050100960109     C                   Z-ADD     1             TBLKUT
050200960109     C                   MOVEL     WKEY02        TBLKEY
050300960109     C                   MOVEL     '7V'          TBLCOD
050400960109     C                   MOVEL     '1'           TBLFTT
050500960110     C                   MOVEL     *BLANKS       TBLUNI
050600080227     C*****              MOVEL     V4DCOD        TBLUNI
050700080227     C                   MOVEL     V4DCOD        §7Vnot
050800080227     C                   MOVEL     ds7V          TBLUNI
050900960110     C                   WRITE     TABEL
051000960109     C                   END
051100960109     C                   END
051200960109     C*  Lettura successiva
051300960115     C                   ADD       1             NRR4
051400960115     C     NRR4          CHAIN     TB73S04                            31
051500960109     C                   END
051600960109     C*  Se non ho scritto nessuna nota do errore
051700960109     C     WSELEZ        IFEQ      'N'
051800960109     C                   SETON                                        28
051900960109     C                   MOVEL     ERR(1)        $MSG
052000960109     C                   ELSE
052100960109     C                   MOVEL     'S'           WFINE
052200960109     C                   END
052300940929     C*
052400960109     C                   ENDSR
052500960109     C*----------------------------------------------------*
052600960109     C*  WRTTES: Gestione scrittura videata descrizioni    *
052700960109     C*----------------------------------------------------*
052800960109     C     WRTTES        BEGSR
052900960109     C*----------------------------------------------------*
053000960115     C                   Z-ADD     1             NRR5
053100960115     C     NRR5          CHAIN     TB73S05                            31
053200960109     C     *IN31         DOWEQ     '0'
053300960109     C     *IN28         ANDEQ     '0'
053400960109     C                   SETOFF                                       40
053500960109     C*  Variazione
053600960115     C     V5CKEY        IFNE      *BLANKS
053700960115     C                   MOVEL     V5CKEY        KKEY
053800960110     C     KTAB2         CHAIN     TABEL00F                           32
053900960111     C                   MOVEL     *BLANKS       TBLUNI
054000080228     C****               MOVEL     V5DSTP        TBLUNI
054100080228     C                   MOVEL     V5DSTP        §7vNOT
054200080228     C                   MOVEL     V5pda         §7vPDA
054300080228     C                   MOVEL     ds7V          TBLUNI
054400960109     C                   MOVEL     *BLANKS       TBLFTR
054500960109     C                   MOVEL     *ZEROS        TBLDTR
054600960115     C     V5DSTP        IFNE      *BLANKS
054700960109     C  N32              UPDATE    TABEL
054800960109     C                   ELSE
054900960109     C*  Se ho abblencato riga gestisco l'annullamento
055000960111     C                   MOVEL     '*'           TBLFLG
055100960109     C                   MOVEL     *BLANKS       TBLFTR
055200960109     C                   MOVEL     *ZEROS        TBLDTR
055300960109     C  N32              UPDATE    TABEL
055400960109     C                   END
055500960109     C                   ELSE
055600960109     C*  Nuova riga
055700960115     C     V5DSTP        IFNE      *BLANKS
055800960111     C                   ADD       1             WNRTES
055900960111     C                   Z-ADD     WNRTES        WPROGG
056000960115     C                   MOVEL     'S'           WTPPAR
056100960109     C                   MOVEL     DS7COD        WCDPAR
056200960109     C                   MOVEL     'S'           WTPELE
056300960109     C                   Z-ADD     1             TBLKUT
056400960109     C                   MOVEL     WKEY02        TBLKEY
056500960109     C                   MOVEL     '7V'          TBLCOD
056600960109     C                   MOVEL     '1'           TBLFTT
056700960111     C                   MOVEL     *BLANKS       TBLUNI
056800080228     C*****              MOVEL     V5DSTP        TBLUNI
056900080228     C                   MOVEL     V5DSTP        §7vNOT
057000080228     C                   MOVEL     V5pda         §7vPDA
057100080228     C                   MOVEL     ds7V          TBLUNI
057200960110     C                   WRITE     TABEL
057300960109     C                   END
057400960109     C                   END
057500960109     C*  Lettura successiva
057600960115     C                   ADD       1             NRR5
057700960115     C     NRR5          CHAIN     TB73S05                            31
057800960109     C                   END
057900960109     C*
058000960109     C                   ENDSR
058100960109     C*--------------------------------------------------------------*
058200960115     C*  CALL73: Richiamo pgm trtb73r1                               *
058300960109     C*--------------------------------------------------------------*
058400960115     C     CALL73        BEGSR
058500960109     C*
058600960115     C                   CALL      'TRTB73R1'
058700960109     C                   PARM                    KPJBA
058800960115     C                   MOVEL     KPJBU         DSTB73
058900960110     C                   MOVEL     'S'           WPGMIN
059000960110     C*  Controllo se devo aggiornare il fle descrizioni
059100960110     C     DS7F03        IFEQ      ' '
059200960109     C*  Se annullato codice annullo anche le descrizioni
059300960109     C     DS7RIP        IFEQ      'A'
059400960109     C                   EXSR      ANNUL
059500960109     C                   END
059600960109     C*  Se ripristinato codice riprisitno anche le descrizioni
059700960109     C     DS7RIP        IFEQ      'R'
059800960109     C                   EXSR      RIPRI
059900960109     C                   END
060000960109     C*  Se no trasmissione annullo trasmiss.
060100960109     C     DS7TRA        IFEQ      'N'
060200960109     C                   EXSR      NOTRA
060300960109     C                   END
060400960110     C                   ELSE
060500960110     C     DS7F03        IFEQ      '2'
060600960110     C                   MOVEL     '1'           WTPVID
060700960110     C                   MOVEL     'N'           WPGMIN
060800960110     C                   MOVEL     'N'           WFINE
060900960110     C                   END
061000960110     C                   END
061100960109     C*
061200960109     C                   ENDSR
061300960109     C*--------------------------------------------------------------*
061400960109     C*  ANNUL: Annullo codici tabella descrizioni                   *
061500960109     C*--------------------------------------------------------------*
061600960109     C     ANNUL         BEGSR
061700960109     C*
061800960109     C                   MOVEL     *BLANKS       KKEY
061900960109     C                   MOVEL     '7V'          KCOD
062000960109      *  Costruisco chiave per posizionamento su tabel
062100960111     C                   MOVEL     'R'           WTPPAR
062200960109     C                   MOVEL     DS7COD        WCDPAR
062300960109     C                   MOVEL     WKEY00        KKEY
062400960109     C                   SETOFF                                       01
062500960109     C     KTAB2         SETLL     TABEL00F
062600960109     C     KTAB1         READE     TABEL00F                               31
062700960109     C     *IN31         DOWEQ     '0'
062800960109     C     WANKEY        ANDEQ     WKEY00
062900960111     C                   MOVEL     '*'           TBLFLG
063000960109     C                   EXCEPT    ANNTBL
063100960109     C     KTAB1         READE     TABEL00F                               31
063200960109     C                   END
063300960109     C*
063400960109     C                   ENDSR
063500960109     C*--------------------------------------------------------------*
063600960109     C*  RIPRI: Riprisitino codici tabella descrizioni               *
063700960109     C*--------------------------------------------------------------*
063800960109     C     RIPRI         BEGSR
063900960109     C*
064000960109     C                   MOVEL     *BLANKS       KKEY
064100960109     C                   MOVEL     '7V'          KCOD
064200960109      *  Costruisco chiave per posizionamento su tabel
064300960115     C                   MOVEL     'S'           WTPPAR
064400960109     C                   MOVEL     DS7COD        WCDPAR
064500960109     C                   MOVEL     WKEY00        KKEY
064600960109     C                   SETOFF                                       01
064700960109     C     KTAB2         SETLL     TABEL00F
064800960109     C     KTAB1         READE     TABEL00F                               31
064900960109     C     *IN31         DOWEQ     '0'
065000960109     C     WANKEY        ANDEQ     WKEY00
065100960109     C                   MOVEL     ' '           TBLFLG
065200960109     C                   EXCEPT    ANNTBL
065300960109     C     KTAB1         READE     TABEL00F                               31
065400960109     C                   END
065500960109     C*
065600960109     C                   ENDSR
065700960109     C*--------------------------------------------------------------*
065800960109     C*  NOTRA: Tabella non in trasmissione                          *
065900960109     C*--------------------------------------------------------------*
066000960109     C     NOTRA         BEGSR
066100960109     C*
066200960109     C                   MOVEL     *BLANKS       KKEY
066300960109     C                   MOVEL     '7V'          KCOD
066400960109      *  Costruisco chiave per posizionamento su tabel
066500960115     C                   MOVEL     'S'           WTPPAR
066600960109     C                   MOVEL     DS7COD        WCDPAR
066700960109     C                   MOVEL     WKEY00        KKEY
066800960109     C                   SETOFF                                       01
066900960109     C     KTAB2         SETLL     TABEL00F
067000960109     C     KTAB1         READE     TABEL00F                               31
067100960109     C     *IN31         DOWEQ     '0'
067200960109     C     WANKEY        ANDEQ     WKEY00
067300960109     C                   MOVEL     ' '           TBLFTT
067400960109     C                   EXCEPT    TRATBL
067500960109     C     KTAB1         READE     TABEL00F                               31
067600960109     C                   END
067700960109     C*
067800960109     C                   ENDSR
067900931209     C*--------------------------------------------------------------*
068000960109     C*  DEFCAM: Operazioni iniziali                                 *
068100931209     C*--------------------------------------------------------------*
068200960109     C     DEFCAM        BEGSR
068300931209     C*
068400931209     C*  Richiamo XPARUT
068500931209     C                   Z-ADD     1             CODUT
068600931209     C                   CALL      'XPARUT'
068700931209     C                   PARM                    UTEDSE
068800931209     C     CODUT         CABEQ     -1            FINE
068900960115     C                   MOVEL     RAGUT         V4CRSU
069000931209     C*----------------------------------------------------*
069100931209     C* Definizione chiavi                                 *
069200940927     C     KTAB1         KLIST
069300940927     C                   KFLD                    KKUT
069400940927     C                   KFLD                    KCOD
069500940928     C*
069600940928     C     KTAB2         KLIST
069700940928     C                   KFLD                    KKUT
069800940928     C                   KFLD                    KCOD
069900940928     C                   KFLD                    KKEY
070000931209     C*----------------------------------------------------*
070100931209     C* Definizione variabili                              *
070200940927     C     *LIKE         DEFINE    TBLKUT        KKUT
070300940927     C     *LIKE         DEFINE    TBLCOD        KCOD
070400940928     C     *LIKE         DEFINE    TBLKEY        KKEY
070500940725     C*----------------------------------------------------*
070600940927     C*  Inizializzazione variabili
070700940928     C                   Z-ADD     1             KKUT
070800940927     C                   MOVEL     'N'           WFINE             1
070900960115     C                   MOVEL     *BLANKS       V4DFUN
071000940928     C                   SETOFF                                       90
071100940228     C*----------------------------------------------------*
071200931209     C                   ENDSR
071300931209     O*-----------------------------------------------------*
071400960109     OTABEL     E            ANNTBL
071500960109     O                       TBLFLG
071600960109     OTABEL     E            TRATBL
071700960109     O                       TBLFTT
071800940927**
071900960109Immettere almeno una riga di descrizione                              01
072000960209Codice particolarità varie inesistente                                02
072100960209Codice particolarità varie annullato                                  03
072200960209Cliente non abilitato alla gestione della particolarità varie         04
072300940419**
072400960109IMMISSIONE
072500960109VARIAZIONE
072600940927INTERROGAZIONE
