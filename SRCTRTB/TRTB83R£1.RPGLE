000100101119     /*END
000200940111     H*------------------------------------------------------------------------*
000300940114     H* GESTIONE TABELLA TEMPI CALCOLO DELIVERY - SUBFILE
000400940111     H*------------------------------------------------------------------------*
000500940119     H DECEDIT('0,') DATEDIT(*DMY.)
000600940111     F*------------------------------------------------------------------------*
000700940111     F* DATA BASE
000800940111     F*------------------------------------------------------------------------*
000900000725     FTRTB83D   CF   E             WORKSTN INFDS(DEVDS1)
001000000725     F                                     SFILE(TB83DVS:SFLULT)
001100000725     F                                     SFILE(TB83DVM:MSGULT)
001200930310     FTABEL00F  IF   E           K DISK
001300000711     FCNACO00F  IF   E           K DISK
001400940111     D*------------------------------------------------------------------------*
001500940111     D* SCHIERE
001600940111     D*------------------------------------------------------------------------*
001700940105     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
001800930802     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001900930802     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
002000930802     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
002100930802     D FUN             S             40    DIM(31)
002200930802     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002300930803     D FFU             S              6    DIM(31)
002400930803     D FDE             S             19    DIM(31)
002500930803     D FAT             S              1    DIM(31)
002600930802     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
002700930803     D OPD             S              2    DIM(24)
002800930803     D OPS             S              2    DIM(24)
002900930803     D ODE             S             19    DIM(24)
003000930803     D OAT             S              1    DIM(24)
003100930802     D* TABELLA DESCRIZIONI A VIDEO
003200930802     D FUV             S             19    DIM(8)
003300930802     D OPV             S             19    DIM(8)
003400930802     D* TABELLA TASTI FUNZIONALI PREMUTI
003500930802     D KEYS            S              1    DIM(40)
003600940111     D*------------------------------------------------------------------------*
003700940111     D* INPUT
003800940111     D*------------------------------------------------------------------------*
003900940111     D*---------------------
004000930723     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
004100940111     D*---------------------
004200930723     D DSOPZ         E DS
004300930803     D  OPZRIG                 1     40
004400940111     D*---------------------
004500930723     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004600940111     D*---------------------
004700930723     D DSFUN         E DS
004800930803     D  FUNRIG                 1     40
004900940111     D*---------------------
005000930127     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005100940111     D*---------------------
005200930127     D DSMSG         E DS
005300940111     D*---------------------
005400020724     D CNCR80        E DS
005500020724     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005600940111     D*---------------------
005700000000     D* DS PER GESTIONE TASTI FUNZIONALI
005800940111     D*---------------------
005900930802     D DSKEYS        E DS
006000930803     D  KEYRIG                 1     40
006100940111     D*---------------------
006200930806     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006300940111     D*---------------------
006400930806     D                SDS
006500940111     D  USER                 254    263
006600940111     D*---------------------
006700930720     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006800940111     D*---------------------
006900000000     D DEVDS1          DS
007000000000     D  KEY                  369    369
007100930721     D  CURSOR               370    371
007200930720     D  FRTSFL               378    379B 0
007300940111     D*---------------------
007400940111     D* DS TEMPI FISSI CALCOLO DELIVERY
007500940111     D*---------------------
007600000725     D DS3Q          E DS
007700940111     D*---------------------
007800940111     D* DS ESTERNA DI PROCEDURA
007900940111     D*---------------------
008000000725     D DS83          E DS                  EXTNAME(TRTB83DS)
008100980130     D*------------------
008200980130     D* DS CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
008300980130     D*------------------
008400980130     D UL12DS        E DS                  EXTNAME(TRUL12DS)
008500940111     D*---------------------
008600940111     D* ARCHITETTURA
008700940111     D*---------------------
008800940111     D KPJBA         E DS
008900940111     C*------------------------------------------------------------------------*
009000940111     C* MAIN LINE
009100940111     C*------------------------------------------------------------------------*
009200000000     C*
009300000000     C* CICLO FINO A CHE:
009400940111DO  1C     $FINE         DOUEQ     '1'
009500000000     C*
009600930805     C                   MOVEL     '0'           $ERR
009700000000     C*
009800930721     C* CONTROLLA GESTIONE VIDEO
009900940111IF  2C     $GES          IFEQ      'VS'
010000930806     C                   EXSR      GESVS
010100940111E   2C                   ENDIF
010200000000     C*
010300940111E   1C                   ENDDO
010400940111     C* FINE PGM
010500000000     C                   SETON                                        LR
010600940111     C*------------------------------------------------------------------------*
010700930806     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
010800940111     C*------------------------------------------------------------------------*
010900930806     C     CARVS         BEGSR
011000930713     C*
011100930806     C                   MOVEL     'VS'          $GES
011200930713     C*
011300930804     C                   EXSR      CAROPZ
011400930804     C                   EXSR      OUTOPZ
011500930804     C                   MOVEA     FUN2          FUN
011600930804     C                   EXSR      CARFUN
011700930804     C                   EXSR      OUTFUN
011800930713     C*
011900000725     C                   WRITE     TB83DVT
012000000725     C                   WRITE     TB83DVO
012100000725     C                   WRITE     TB83DVF
012200930713     C*
012300930806     C                   EXSR      POSSFL
012400930713     C*
012500930713     C                   ENDSR
012600940111     C*------------------------------------------------------------------------*
012700930806     C* GESVS  - GESTIONE SUBFILE
012800940111     C*------------------------------------------------------------------------*
012900930806     C     GESVS         BEGSR
013000930714     C*
013100940111IF  1C     $TESVD        IFEQ      '1'
013200000725     C                   WRITE     TB83DVT
013300000725     C                   WRITE     TB83DVO
013400000725     C                   WRITE     TB83DVF
013500940111E   1C                   ENDIF
013600930713     C*
013700930805     C                   EXSR      OUTSFL
013800930713     C*
013900930805     C                   EXSR      CONFUN
014000930805     C*
014100940111IF  1C     $ERR          IFEQ      '0'
014200940111SL  2C                   SELECT
014300930804     C     KEY           WHENEQ    F6
014400930804     C                   EXSR      F06GES
014500940111     C                   OTHER
014600930804     C                   EXSR      FUNSFL
014700940111E   2C                   ENDSL
014800940111E   1C                   ENDIF
014900930713     C*
015000930713     C                   ENDSR
015100940111     C*------------------------------------------------------------------------*
015200930804     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
015300940111     C*------------------------------------------------------------------------*
015400930126     C     F06GES        BEGSR
015500930127     C*
015600940111     C* LANCIO L'INSERIMENTO
015700090518     C                   MOVEL     *BLANK        WCLI              7
015800940114     C                   MOVEL     'GES'         WOP0              3
015900940114     C                   MOVEL     'F06'         WOP1              3
016000000725     C                   MOVEL     'TRTB84R'     WPGM              8
016100930805     C                   EXSR      LANPGM
016200930128     C*
016300930805     C                   EXSR      POSSFL
016400930127     C*
016500000000     C                   ENDSR
016600940111     C*------------------------------------------------------------------------*
016700930803     C*  GESOPZ - GESTIONE OPZIONI
016800940111     C*------------------------------------------------------------------------*
016900930803     C     GESOPZ        BEGSR
017000930804     C*
017100940111SL  1C                   SELECT
017200930806     C     VSCOPZ        WHENEQ    ' 2'
017300930804     C                   EXSR      O02GES
017400000711     C     VSCOPZ        WHENEQ    ' 3'
017500000711     C                   EXSR      O03GES
017600930806     C     VSCOPZ        WHENEQ    ' 4'
017700930804     C                   EXSR      O04GES
017800940121     C     VSCOPZ        WHENEQ    ' 5'
017900940121     C                   EXSR      O05GES
018000930806     C     VSCOPZ        WHENEQ    ' 7'
018100930804     C                   EXSR      O07GES
018200930804     C                   OTHER
018300930804     C                   MOVEL     MSG(4)        $MSG
018400930805     C                   EXSR      CARMSG
018500940111E   1C                   ENDSL
018600930804     C*
018700000000     C                   ENDSR
018800940111     C*------------------------------------------------------------------------*
018900000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
019000940111     C*------------------------------------------------------------------------*
019100000000     C     O02GES        BEGSR
019200930128     C*
019300000801IF  1C     VSCATB        IFEQ      'A'
019400000801     C                   MOVEL     '1'           $ERR
019500000801     C                   MOVEL     MSG(12)       $MSG
019600000801     C                   EXSR      CARMSG
019700000801E   1C                   ENDIF
019800931217     C*
019900940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
020000000711IF  1C     VIDCLI        IFEQ      *BLANKS
020100940117     C                   MOVEL     USER          WEDP              3
020200940117IF  2C     WEDP          IFNE      'EDP'
020300940117     C                   MOVEL     '1'           $ERR
020400940117     C                   MOVEL     MSG(11)       $MSG
020500940117     C                   EXSR      CARMSG
020600940117E   2C                   ENDIF
020700940117E   1C                   ENDIF
020800940117     C*
020900940111IF  1C     $ERR          IFEQ      '0'
021000000000     C*
021100940111     C* LANCIO LA MODIFICA
021200000711     C                   MOVEL     VIDCLI        WCLI
021300000711     C                   Z-ADD     VIDCKS        WCKS              7 0
021400000711     C                   MOVE      VIDCSD        WCSD              2
021500000711     C                   MOVE      VIDCDM        WCDM             10
021600000711     C                   MOVE      VIDPRF        WPRF             10
021700000711     C                   MOVE      VIDNHL        WNHL              2
021800940114     C                   MOVEL     'GES'         WOP0
021900940114     C                   MOVEL     'O02'         WOP1
022000000725     C                   MOVEL     'TRTB84R'     WPGM
022100930805     C                   EXSR      LANPGM
022200000000     C*
022300930805     C                   Z-ADD     1             SWMODI
022400930128     C*
022500940111E   1C                   ENDIF
022600000000     C*
022700000000     C                   ENDSR
022800000711     C*------------------------------------------------------------------------*
022900000711     C*  O03GES - GESTIONE OPZIONE 3=COPIA
023000000711     C*------------------------------------------------------------------------*
023100000711     C     O03GES        BEGSR
023200000711     C*
023300000711     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
023400000801IF  1C     VSCATB        IFEQ      'A'
023500000801     C                   MOVEL     '1'           $ERR
023600000801     C                   MOVEL     MSG(12)       $MSG
023700000801     C                   EXSR      CARMSG
023800000801E   1C                   ENDIF
023900000711     C*
024000000711     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
024100000711IF  1C     VIDCLI        IFEQ      *BLANKS
024200000711     C                   MOVEL     USER          WEDP              3
024300000711IF  2C     WEDP          IFNE      'EDP'
024400000711     C                   MOVEL     '1'           $ERR
024500000711     C                   MOVEL     MSG(11)       $MSG
024600000711     C                   EXSR      CARMSG
024700000711E   2C                   ENDIF
024800000711E   1C                   ENDIF
024900000711     C*
025000000711IF  1C     $ERR          IFEQ      '0'
025100000711     C*
025200000711     C* LANCIO LA COPIA
025300000711     C                   MOVEL     VIDCLI        WCLI
025400000711     C                   Z-ADD     VIDCKS        WCKS
025500000711     C                   MOVE      VIDCSD        WCSD
025600000711     C                   MOVE      VIDCDM        WCDM
025700000711     C                   MOVE      VIDPRF        WPRF
025800000711     C                   MOVE      VIDNHL        WNHL
025900000711     C                   MOVEL     'GES'         WOP0
026000000711     C                   MOVEL     'O03'         WOP1
026100000725     C                   MOVEL     'TRTB84R'     WPGM
026200000711     C                   EXSR      LANPGM
026300000711     C*
026400000711     C                   Z-ADD     1             SWMODI
026500000711     C*
026600000711E   1C                   ENDIF
026700000711     C*
026800000711     C                   ENDSR
026900940111     C*------------------------------------------------------------------------*
027000000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
027100940111     C*------------------------------------------------------------------------*
027200000000     C     O04GES        BEGSR
027300930128     C*
027400930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
027500000801IF  1C     VSCATB        IFEQ      'A'
027600000801     C                   MOVEL     '1'           $ERR
027700000801     C                   MOVEL     MSG(12)       $MSG
027800000801     C                   EXSR      CARMSG
027900000801E   1C                   ENDIF
028000940117     C*
028100940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
028200000711IF  1C     VIDCLI        IFEQ      *BLANKS
028300940117     C                   MOVEL     USER          WEDP              3
028400940117IF  2C     WEDP          IFNE      'EDP'
028500940117     C                   MOVEL     '1'           $ERR
028600940117     C                   MOVEL     MSG(11)       $MSG
028700940117     C                   EXSR      CARMSG
028800940117E   2C                   ENDIF
028900940117E   1C                   ENDIF
029000931217     C*
029100940111IF  1C     $ERR          IFEQ      '0'
029200940111     C*
029300940111     C* LANCIO LA CANCELLAZIONE
029400000711     C                   MOVEL     VIDCLI        WCLI
029500000711     C                   Z-ADD     VIDCKS        WCKS
029600000711     C                   MOVE      VIDCSD        WCSD
029700000711     C                   MOVE      VIDCDM        WCDM
029800000711     C                   MOVE      VIDPRF        WPRF
029900000711     C                   MOVE      VIDNHL        WNHL
030000940114     C                   MOVEL     'GES'         WOP0
030100940114     C                   MOVEL     'O04'         WOP1
030200000725     C                   MOVEL     'TRTB84R'     WPGM
030300930201     C                   EXSR      LANPGM
030400000000     C*
030500930805     C                   Z-ADD     1             SWMODI
030600940111     C*
030700940111E   1C                   ENDIF
030800000000     C*
030900000000     C                   ENDSR
031000940121     C*------------------------------------------------------------------------*
031100000711     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZA
031200940121     C*------------------------------------------------------------------------*
031300940121     C     O05GES        BEGSR
031400940121     C*
031500940121     C* LANCIO LA VISUALIZZAZIONE
031600000711     C                   MOVEL     VIDCLI        WCLI
031700000711     C                   Z-ADD     VIDCKS        WCKS
031800000711     C                   MOVE      VIDCSD        WCSD
031900000711     C                   MOVE      VIDCDM        WCDM
032000000711     C                   MOVE      VIDPRF        WPRF
032100000711     C                   MOVE      VIDNHL        WNHL
032200940121     C                   MOVEL     'GES'         WOP0
032300940121     C                   MOVEL     'O05'         WOP1
032400000725     C                   MOVEL     'TRTB84R'     WPGM
032500940121     C                   EXSR      LANPGM
032600940121     C*
032700940121     C                   ENDSR
032800940111     C*------------------------------------------------------------------------*
032900930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
033000940111     C*------------------------------------------------------------------------*
033100930128     C     O07GES        BEGSR
033200930128     C*
033300930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
033400000801IF  1C     VSCATB        IFNE      'A'
033500000801     C                   MOVEL     '1'           $ERR
033600000801     C                   MOVEL     MSG(13)       $MSG
033700000801     C                   EXSR      CARMSG
033800000801E   1C                   ENDIF
033900940117     C*
034000940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
034100000711IF  1C     VIDCLI        IFEQ      *BLANKS
034200940117     C                   MOVEL     USER          WEDP              3
034300940117IF  2C     WEDP          IFNE      'EDP'
034400940117     C                   MOVEL     '1'           $ERR
034500940117     C                   MOVEL     MSG(11)       $MSG
034600940117     C                   EXSR      CARMSG
034700940117E   2C                   ENDIF
034800940117E   1C                   ENDIF
034900930215     C*
035000940111IF  1C     $ERR          IFEQ      '0'
035100930128     C*
035200940111     C* LANCIO IL RIPRISTINO
035300000711     C                   MOVEL     VIDCLI        WCLI
035400000711     C                   Z-ADD     VIDCKS        WCKS
035500000711     C                   MOVE      VIDCSD        WCSD
035600000711     C                   MOVE      VIDCDM        WCDM
035700000711     C                   MOVE      VIDPRF        WPRF
035800000711     C                   MOVE      VIDNHL        WNHL
035900940114     C                   MOVEL     'GES'         WOP0
036000940114     C                   MOVEL     'O07'         WOP1
036100000725     C                   MOVEL     'TRTB84R'     WPGM
036200930201     C                   EXSR      LANPGM
036300930128     C*
036400930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
036500930128     C                   Z-ADD     1             SWMODI
036600930128     C*
036700940111E   1C                   ENDIF
036800930128     C*
036900930128     C                   ENDSR
037000940111     C*------------------------------------------------------------------------*
037100930205     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
037200940111     C*------------------------------------------------------------------------*
037300930205     C     NEWPOS        BEGSR
037400930205     C*
037500940111     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
037600940111     C*        1 = RICHIESTO POSIZIONAMENTO
037700930205     C                   Z-ADD     0             SWPOSI            7 0
037800930205     C*
037900940111     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
038000940111     C*            NON OCCORRE RICARICARE IL SUBFILE
038100940111     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
038200930205     C                   Z-ADD     0             SWMODI            1 0
038300930205     C*
038400930205     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
038500000719IF  1C     VCCCLI        IFNE      DEPCLI
038600101119IF  1C     VCCuni        orne      DEPuni
038700101119IF  1C     VCCsun        orne      DEPsun
038800930205     C*
038900930205     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
039000930205     C                   EXSR      CHKPAR
039100930205     C*
039200930205     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
039300940111IF  2C     $ERR          IFEQ      '0'
039400940111     C                   Z-ADD     1             SWPOSI
039500000711     C                   MOVEL     VCCCLI        DEPCLI
039600101119     C                   Z-ADD     VCCuni        DEPuni
039700101119     C                   movel     VCCsun        DEPsun
039800940111E   2C                   ENDIF
039900000719E   1C                   ENDIF
040000930205     C*
040100930205     C                   ENDSR
040200940111     C*------------------------------------------------------------------------*
040300940111     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
040400940111     C*------------------------------------------------------------------------*
040500940111     C     CHKPAR        BEGSR
040600940111     C*
040700940111     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
040800940111     C                   SETOFF                                       80
040900940111     C*
041000000718     C* CONTROLLO ESISTENZA
041100090518     C                   MOVEL     VCCCLI        WCLI              7
041200000711     C                   MOVEL     *BLANKS       WDCLI            22
041300000711     C                   EXSR      CHKCLI
041400940113IF  2C     $ERR          IFEQ      '1'
041500940111     C                   MOVEL     '1'           $ERR
041600940111     C                   MOVEL     MSG(9)        $MSG
041700940111     C                   EXSR      CARMSG
041800940111     C                   SETON                                        80
041900940113E   2C                   ENDIF
042000000719     C                   MOVEL     WDCLI         VIDDCL
042100940111     C*
042200940111     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
042300940111     C* CURSORE SUL CAMPO DEL CONTROL
042400940111IF  1C     $ERR          IFEQ      '1'
042500940111     C                   MOVEL     '0'           *IN43
042600940111X   1C                   ELSE
042700940111     C                   MOVEL     '1'           *IN43
042800940111E   1C                   ENDIF
042900940111     C*
043000940111     C                   ENDSR
043100940111     C*------------------------------------------------------------------------*
043200000711     C*  CHKCLI - CONTROLLA LA FILIALE
043300940111     C*------------------------------------------------------------------------*
043400000711     C     CHKCLI        BEGSR
043500940111     C*
043600940114     C*
043700940114     C* CONTROLLO ESISTENZA NELL'ORGANIGRAMMA
043800000718     C                   MOVE      VIDCLI        ULTIMO            1
043900000711     C     ULTIMO        IFEQ      *BLANK
044000000719     C     VIDCLI        ANDNE     *BLANK
044100000718     C                   MOVEL     VIDCLI        KORCLI
044200000711     C     KEYCLI        CHAIN     CNACO00F                           99
044300940114IF  2C     *IN99         IFEQ      '1'
044400940114     C                   MOVEL     '1'           $ERR
044500000711     C                   MOVEL     *ALL'*'       WDCLI
044600940114X   2C                   ELSE
044700000711     C                   MOVEL     ACORAG        WDCLI
044800940114E   2C                   ENDIF
044900000711     C                   ELSE
045000000711     C                   MOVEL     *BLANK        WDCLI
045100000711     C                   END
045200940113     C*
045300940114     C                   ENDSR
045400940112     C*------------------------------------------------------------------------*
045500000711     C* SETCLI - POSIZIONAMENTO E PRIMA LETTURA RECORD TABELLE
045600940112     C*------------------------------------------------------------------------*
045700000711     C     SETCLI        BEGSR
045800940112     C*
045900000719     C* POSIZIONAMENTO RECORD TABELLA
046000940112     C*
046100020724     C                   Z-ADD     1             KTBKUT
046200000725     C                   MOVEL     '3Q'          KTBCOD
046300940112     C                   MOVEL     *BLANKS       KTBKEY
046400000711     C                   MOVEL     WCLI          KTBKEY
046500940112     C*
046600940113     C     KEYTAB        SETLL     TABEL00F                           99
046700940112     C*
046800000711     C                   EXSR      LETCLI
046900940112     C*
047000940112     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
047100940112IF  1C     *IN99         IFEQ      '1'
047200940112     C                   Z-ADD     1             SFLEOF
047300940112E   1C                   ENDIF
047400940112     C*
047500940112     C                   ENDSR
047600940112     C*------------------------------------------------------------------------*
047700000711     C* LETCLI - LEGGE RECORD SUCCESSIVO
047800940112     C*------------------------------------------------------------------------*
047900000711     C     LETCLI        BEGSR
048000940112     C*
048100940113     C* FINO A QUANDO NON TROVI UN RECORD VALIDO (RECOK = '1') STAI DENTRO
048200940113     C                   MOVEL     '0'           RECOK             1
048300940113DO  1C     RECOK         DOWEQ     '0'
048400940112     C*
048500000711     C* LETTURA RECORD TABELLA
048600940113     C                   READ      TABEL00F                               99
048700101119     C                   MOVEL     '1'           RECOK
048800940112     C*
048900940113     C* FINE FILE  O TABELLA DIVERSA
049000940112IF  2C     *IN99         IFEQ      '1'
049100940113     C     TBLKUT        ORNE      1
049200000725     C     TBLCOD        ORNE      '3Q'
049300940112     C                   Z-ADD     1             SFLEOF
049400000718     C                   ELSE
049500101119     c                   movel     tbluni        ds3Q
049600101119     c                   movel     §3Qcks        w007a             7
049700101119if  3c                   if        (VCCuni > *zero and VCCuni <> §3Qcks)
049800101119     c                             or
049900101119     c                             (VCCsun = 'S' and %subst(TBLkey:1:7)<>w007a)
050000101119     C                   MOVEL     '0'           RECOK
050100101119e   3c                   endif
050200940112E   2C                   ENDIF
050300940112E   1C                   ENDDO
050400940112     C*
050500940112     C                   ENDSR
050600940111     C*------------------------------------------------------------------------*
050700000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
050800940111     C*------------------------------------------------------------------------*
050900000000     C     IRESFL        BEGSR
051000940112     C*
051100000719     C* RIEMPIMENTO CAMPI
051200000801IF  1C     TBLFLG        IFNE      *BLANKS
051300000801     C                   MOVEL     'A'           VSCATB
051400000801X   1C                   ELSE
051500000801     C                   MOVEL     ' '           VSCATB
051600000801E   1C                   ENDIF
051700000711     C                   MOVEL     TBLKEY        VIDCLI
051800000725     C                   MOVEL     TBLUNI        DS3Q
051900000725     C                   MOVEL     §3QCSD        VIDCSD
052000000725     C                   MOVEL     §3QCDM        VIDCDM
052100000725     C                   MOVEL     §3QCKS        VIDCKS
052200000725     C                   MOVEL     §3QPRF        VIDPRF
052300000725     C                   MOVEL     §3QNHL        VIDNHL
052400000718     C                   EXSR      CHKCLI
052500000718     C                   MOVEL     WDCLI         VIDDCL
052600940112     C*
052700000000     C                   ENDSR
052800940111     C*------------------------------------------------------------------------*
052900930201     C* LANPGM - LANCIA UN PROGRAMMA
053000940111     C*------------------------------------------------------------------------*
053100930201     C     LANPGM        BEGSR
053200940114     C*
053300940114     C*--------------------
053400940114     C* PARAMETRI INFORMATIVI
053500940114     C*--------------------
053600000725     C                   MOVEL     WOP0          D83OP0
053700000725     C                   MOVEL     WOP1          D83OP1
053800000725     C                   MOVEL     '0'           D83F03
053900000725     C                   MOVEL     '0'           D83ERR
054000000725     C                   MOVEL     *BLANKS       D83MSG
054100940114     C*--------------------
054200940114     C* PARAMETRI
054300940114     C*--------------------
054400000725     C                   MOVEL     WCLI          D83CLI
054500000725     C                   Z-ADD     WCKS          D83CKS
054600000725     C                   MOVE      WCSD          D83CSD
054700000725     C                   MOVE      WCDM          D83CDM
054800000725     C                   MOVE      WPRF          D83PRF
054900000725     C                   MOVE      WNHL          D83NHL
055000940114     C* LANCIO
055100940114     C                   CALL      WPGM
055200000725     C                   PARM                    DS83
055300940114     C*
055400930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
055500000725IF  1C     D83F03        IFEQ      '1'
055600930721     C                   MOVEL     '1'           $F03
055700930721     C                   MOVEL     '1'           $ERR
055800940105     C                   MOVEL     '1'           $FINE
055900940114     C*
056000930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
056100940114X   1C                   ELSE
056200000725     C                   MOVEL     D83MSG        $MSG
056300930721     C                   EXSR      CARMSG
056400940114E   1C                   ENDIF
056500930201     C*
056600930201     C                   ENDSR
056700940114     C*------------------------------------------------------------------------*
056800000725     C*  CAR3Q  - CARICAMENTO DATI DI 3Q
056900940114     C*------------------------------------------------------------------------*
057000000725     C     CAR3Q         BEGSR
057100940114     C*
057200000719     C* CARICO I DATI DI
057300940114     C*
057400020724     C                   Z-ADD     1             KTBKUT
057500000725     C                   MOVEL     '3Q'          KTBCOD
057600940114     C                   MOVEL     *BLANKS       KTBKEY
057700000719     C                   MOVEL     VCCCLI        KTBKEY
057800940114     C*
057900000719     C     KEYTAB        CHAIN     TABEL00F                           99
058000940114IF  1C     *IN99         IFEQ      *OFF
058100000725     C                   MOVEL     TBLUNI        DS3Q
058200940114X   1C                   ELSE
058300000725     C                   CLEAR                   DS3Q
058400940114E   1C                   ENDIF
058500940114     C*
058600000725     C* MUOVO I CAMPI DI '3Q' IN QUELLI DELLA DS DI PROCEDURA PER IL PASSAGGIO
058700000725     C                   MOVEL     §3QCSD        D83CSD
058800000725     C                   Z-ADD     §3QCKS        D83CKS
058900000725     C                   MOVEL     §3QCDM        D83CDM
059000000725     C                   MOVEL     §3QPRF        D83PRF
059100000725     C                   MOVEL     §3QNHL        D83NHL
059200000719     C                   EXSR      CHKPAR
059300940114     C*
059400940114     C                   ENDSR
059500940111     C*------------------------------------------------------------------------*
059600930713     C*  *INZSR - ROUTINE INIZIALE
059700940111     C*------------------------------------------------------------------------*
059800930713     C     *INZSR        BEGSR
059900000000     C*
060000000000     C* RICEVIMENTO PARAMETRI
060100000000     C     *ENTRY        PLIST
060200940105     C                   PARM                    KPJBA
060300000719     C                   MOVEL     KPJBU         VCCCLI
060400940111     C*
060500940111     C* DEFINISCE LE VARIABILI DI PROGRAMMA
060600000711     C     *LIKE         DEFINE    VCCCLI        DEPCLI
060700101119     C     *LIKE         DEFINE    VCCuni        DEPuni
060800101119     C     *LIKE         DEFINE    VCCsun        DEPsun
060900940111     C     *LIKE         DEFINE    TBLKUT        KTBKUT
061000940111     C     *LIKE         DEFINE    TBLCOD        KTBCOD
061100940111     C     *LIKE         DEFINE    TBLKEY        KTBKEY
061200000711     C     *LIKE         DEFINE    ACOKSC        KORCLI
061300930310     C*
061400930310     C* CHIAVE LETTURA TABEL00F - COMPLETA
061500940112     C     KEYTAB        KLIST
061600940111     C                   KFLD                    KTBKUT
061700940111     C                   KFLD                    KTBCOD
061800940111     C                   KFLD                    KTBKEY
061900940112     C*
062000940114     C* CHIAVE LETTURA TABEL00F - PARZIALE
062100940112     C     KEYTA2        KLIST
062200940112     C                   KFLD                    KTBKUT
062300940112     C                   KFLD                    KTBCOD
062400940114     C*
062500940114     C* CHIAVE LETTURA AZORG01L - COMPLETA
062600000711     C     KEYCLI        KLIST
062700000711     C                   KFLD                    KTBKUT
062800000711     C                   KFLD                    KORKCC            4 0
062900000711     C                   KFLD                    KORCLI
063000000711     C                   Z-ADD     151           KORKCC
063100930714     C*
063200930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
063300930803     C                   EXSR      INZSTD
063400930803     C*
063500930803     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
063600930803     C                   EXSR      INZSFL
063700020724     C                   MOVEL     RAGUT         VTDRSU
063800980130     C*
063900980130     C* CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
064000980130     C                   CLEAR                   UL12DS
064100980130     C                   MOVEL     'L'           I12TLA
064200000725     C                   MOVEL     '3Q'          I12COD
064300980130     C                   MOVEL     KNSIF         I12NSI
064400980130     C                   CALL      'TRUL12R'
064500980130     C                   PARM                    UL12DS
064600980130     C     O12ERR        IFEQ      '1'
064700980130     C                   MOVEL     '1'           $FINE
064800980130     C                   ENDIF
064900940104     C*
065000000719     C* CARICA I DATI
065100000725     C                   EXSR      CAR3Q
065200940114     C*
065300940104     C* EMETTO VIDEATA SUBFILE
065400940114     C                   EXSR      CARVS
065500940114     C*
065600000000     C                   ENDSR
065700940111     C*------------------------------------------------------------------------*
065800940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
065900940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066000940111     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
066100940111     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
066200940111     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
066300940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066400940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066500940111     C*------------------------------------------------------------------------*
066600930803     C* INZSTD - ROUTINE INIZIALE STANDARD
066700940111     C*------------------------------------------------------------------------*
066800930803     C     INZSTD        BEGSR
066900930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
067000930802     C                   CALL      'XKEYS'
067100930802     C                   PARM                    DSKEYS
067200930805     C                   MOVEA     KEYRIG        KEYS
067300930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
067400930714     C     XMSGPA        PLIST
067500930714     C                   PARM                    DSMSMS
067600930714     C                   PARM                    DSMSPG
067700930714     C                   PARM                    DSMSKE
067800930714     C                   PARM                    DSMSER
067900930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
068000000725     C                   MOVEL     'TRTB83R'     DSMSPG
068100000725     C                   MOVEL     'TRTB83R'     VMCQUE
068200930806     C                   MOVEL     *BLANKS       VMCKEY
068300930714     C                   MOVEL     *BLANKS       DSMSMS
068400930805     C* INIZIALIZZO FLAG CONTROLLO PGM
068500930721     C                   MOVEL     '0'           $ERR              1
068600930721     C                   MOVEL     '0'           $F03              1
068700930806     C                   MOVEL     *BLANKS       $GES              2
068800930721     C                   MOVEL     *BLANKS       $MSG             80
068900930721     C                   MOVEL     '0'           $FINE             1
069000930721     C                   MOVEL     '0'           $TESVD            1
069100940112     C                   MOVEL     'G'           $TGES             1
069200020724     C*
069300020724     C                   CALL      'X§PARUT'
069400020724     C                   PARM                    UT§DSE
069500020724     C                   MOVEL     REC80         CNCR80
069600930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
069700930721     C                   BITOFF    '01234567'    POSMI1            1
069800930721     C                   BITOFF    '01234567'    POSMI2            1
069900930721     C                   BITOFF    '01234567'    POSMA1            1
070000930721     C                   BITOFF    '01234567'    POSMA2            1
070100930721     C                   BITON     '34'          POSMI1            1
070200930721     C                   BITON     '7'           POSMI2            1
070300930721     C                   BITON     '34'          POSMA1            1
070400930721     C                   BITON     '0'           POSMA2            1
070500930721     C                   MOVE      POSMI2        POSMIN            2
070600930721     C                   MOVEL     POSMI1        POSMIN
070700930721     C                   MOVE      POSMA2        POSMAX            2
070800930721     C                   MOVEL     POSMA1        POSMAX
070900930714     C                   ENDSR
071000940111     C*------------------------------------------------------------------------*
071100930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
071200940111     C*------------------------------------------------------------------------*
071300930804     C     FUNSTD        BEGSR
071400930804     C                   SELECT
071500930804     C     KEY           WHENEQ    F3
071600930804     C                   EXSR      F03STD
071700930804     C     KEY           WHENEQ    F12
071800930804     C                   EXSR      F12STD
071900930804     C     KEY           WHENEQ    F24
072000930804     C                   EXSR      F24STD
072100930804     C                   OTHER
072200930804     C                   MOVEL     MSG(2)        $MSG
072300930804     C                   EXSR      CARMSG
072400930804     C                   ENDSL
072500930804     C                   ENDSR
072600940111     C*------------------------------------------------------------------------*
072700930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
072800940111     C*------------------------------------------------------------------------*
072900930804     C     F03STD        BEGSR
073000930804     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
073100930923     C                   MOVEL     '1'           $F03
073200930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
073300930804     C                   MOVEL     '1'           $FINE
073400930804     C                   ENDSR
073500940111     C*------------------------------------------------------------------------*
073600930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
073700940111     C*------------------------------------------------------------------------*
073800930804     C     F12STD        BEGSR
073900930805     C* IMPOSTO FLAG DI FIN PGM
074000930804     C                   MOVEL     '1'           $FINE
074100930804     C                   ENDSR
074200940111     C*------------------------------------------------------------------------*
074300930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
074400940111     C*------------------------------------------------------------------------*
074500930804     C     F24STD        BEGSR
074600930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
074700930804     C                   EXSR      OUTFUN
074800930804     C                   ENDSR
074900940111     C*------------------------------------------------------------------------*
075000930804     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
075100940111     C*------------------------------------------------------------------------*
075200930804     C     INZSFL        BEGSR
075300930804     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
075400930806     C     *LIKE         DEFINE    VCCNBR        SFLPAG
075500930806     C     *LIKE         DEFINE    VCCNBR        SFLEOF
075600930806     C     *LIKE         DEFINE    VCCNBR        SFLERR
075700930806     C     *LIKE         DEFINE    VCCNBR        SFLULT
075800930806     C     *LIKE         DEFINE    VCCNBR        WFLULT
075900930806     C     *LIKE         DEFINE    VCCNBR        SFLRIG
076000930804     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
076100930804     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
076200101119     C                   Z-ADD     11            SFLPAG
076300930804     C                   ENDSR
076400940111     C*------------------------------------------------------------------------*
076500930804     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
076600940111     C*------------------------------------------------------------------------*
076700930804     C     FUNSFL        BEGSR
076800930804     C                   SELECT
076900930804     C     KEY           WHENEQ    ENTER
077000930804     C                   EXSR      ENTSFL
077100930805     C     KEY           WHENEQ    F3
077200930805     C                   EXSR      F03STD
077300930804     C     KEY           WHENEQ    F5
077400930804     C                   EXSR      F05STD
077500930805     C     KEY           WHENEQ    F12
077600930805     C                   EXSR      F12STD
077700930804     C     KEY           WHENEQ    F17
077800930804     C                   EXSR      F17STD
077900930804     C     KEY           WHENEQ    F18
078000930804     C                   EXSR      F18STD
078100930804     C     KEY           WHENEQ    F23
078200930804     C                   EXSR      F23STD
078300930805     C     KEY           WHENEQ    F24
078400930805     C                   EXSR      F24STD
078500930804     C     KEY           WHENEQ    ROLLUP
078600930804     C                   EXSR      RUPSTD
078700930804     C     KEY           WHENEQ    ROLLDN
078800930804     C                   EXSR      RDNSTD
078900930804     C                   OTHER
079000930804     C                   MOVEL     MSG(2)        $MSG
079100930804     C                   EXSR      CARMSG
079200930804     C                   ENDSL
079300930804     C                   ENDSR
079400940111     C*------------------------------------------------------------------------*
079500930804     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
079600940111     C*------------------------------------------------------------------------*
079700930804     C     F05STD        BEGSR
079800930804     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
079900930804     C                   EXSR      POSSFL
080000930804     C                   ENDSR
080100940111     C*------------------------------------------------------------------------*
080200930804     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
080300940111     C*------------------------------------------------------------------------*
080400930804     C     F17STD        BEGSR
080500930804     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
080600930806     C                   Z-ADD     1             VCCNBR
080700930804     C                   ENDSR
080800940111     C*------------------------------------------------------------------------*
080900930804     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
081000940111     C*------------------------------------------------------------------------*
081100930804     C     F18STD        BEGSR
081200930804     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
081300930806     C                   Z-ADD     SFLULT        VCCNBR
081400930804     C                   ENDSR
081500940111     C*------------------------------------------------------------------------*
081600930804     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
081700940111     C*------------------------------------------------------------------------*
081800930804     C     F23STD        BEGSR
081900930804     C* GESTIONE VISUALIZZAZIONE OPZIONI
082000930804     C                   EXSR      OUTOPZ
082100930804     C                   ENDSR
082200940111     C*------------------------------------------------------------------------*
082300930804     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
082400940111     C*------------------------------------------------------------------------*
082500930804     C     RUPSTD        BEGSR
082600930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
082700930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
082800930805     C     CURSOR        IFGE      POSMIN
082900930805     C     CURSOR        ANDLE     POSMAX
083000930804     C                   ELSE
083100930805     C                   EXSR      CARSFL
083200930805     C                   ENDIF
083300930804     C                   ENDSR
083400940111     C*------------------------------------------------------------------------*
083500930804     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
083600940111     C*------------------------------------------------------------------------*
083700930804     C     RDNSTD        BEGSR
083800930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
083900930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
084000930804     C     CURSOR        IFGE      POSMIN
084100930804     C     CURSOR        ANDLE     POSMAX
084200930804     C                   ELSE
084300930804     C* INVIO MESSAGGIO DI INIZIO ELENCO
084400930804     C                   MOVEL     '1'           $ERR
084500930804     C                   MOVEL     MSG(5)        $MSG
084600930804     C                   EXSR      CARMSG
084700930805     C                   ENDIF
084800930804     C                   ENDSR
084900940111     C*------------------------------------------------------------------------*
085000930804     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
085100940111     C*------------------------------------------------------------------------*
085200930804     C     ENTSFL        BEGSR
085300930804     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
085400930804     C                   EXSR      NEWPOS
085500930804     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
085600930804     C     $ERR          IFEQ      '0'
085700930804     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
085800930804     C     SWPOSI        IFNE      0
085900930804     C                   EXSR      POSSFL
086000930804     C                   ELSE
086100930804     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
086200930804     C     SFLULT        IFNE      0
086300930804     C                   EXSR      OPZSFL
086400930804     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
086500930804     C     SWMODI        IFNE      0
086600930804     C                   EXSR      POSSFL
086700930805     C                   ENDIF
086800930805     C                   ENDIF
086900930805     C                   ENDIF
087000930805     C                   ENDIF
087100930804     C                   ENDSR
087200940111     C*------------------------------------------------------------------------*
087300940111     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
087400940111     C*------------------------------------------------------------------------*
087500930804     C     POSSFL        BEGSR
087600930804     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
087700930804     C                   EXSR      CLRSFL
087800930804     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
087900000711     C                   EXSR      SETCLI
088000930804     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
088100930804     C     SFLEOF        IFEQ      0
088200930804     C                   EXSR      CARSFL
088300930805     C                   ENDIF
088400930804     C                   ENDSR
088500940111     C*------------------------------------------------------------------------*
088600930804     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
088700940111     C*------------------------------------------------------------------------*
088800930804     C     OPZSFL        BEGSR
088900930802     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
089000930802     C                   MOVEL     'S'           OPZOK             1
089100000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
089200000000     C                   Z-ADD     0             SFLVAR            5 0
089300000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
089400000000     C                   Z-ADD     0             SFLERR
089500000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
089600000000     C                   Z-ADD     SFLULT        WFLULT
089700000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
089800000725     C                   READC     TB83DVS                                49
089900930806     C     *IN49         DOWEQ     '0'
090000000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
090100000000     C                   ADD       1             SFLVAR
090200930802     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
090300930805     C     OPZOK         IFEQ      'S'
090400930802     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
090500930806     C     VSCOPZ        IFNE      *BLANKS
090600930802     C                   EXSR      CONOPZ
090700930802     C     OPZOK         IFEQ      'S'
090800930802     C                   EXSR      GESOPZ
090900930805     C                   ENDIF
091000930805     C                   ENDIF
091100000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
091200000000     C*            2. PULIZIA SCELTA IN SFL
091300930805     C     OPZOK         IFEQ      'S'
091400000000     C                   MOVEL     '0'           *IN41
091500930806     C                   MOVEL     *BLANKS       VSCOPZ
091600930805     C                   ELSE
091700000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
091800000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
091900000000     C                   MOVEL     '1'           *IN41
092000000000     C                   Z-ADD     SFLULT        SFLERR
092100930805     C                   ENDIF
092200930805     C                   ELSE
092300000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
092400000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
092500000000     C                   MOVEL     '1'           *IN41
092600930805     C                   ENDIF
092700000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
092800000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
092900000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
093000930803     C     OPZOK         IFNE      'S'
093100930806     C                   Z-ADD     SFLERR        VCCNBR
093200000000     C                   ELSE
093300930806     C                   Z-ADD     SFLULT        VCCNBR
093400930805     C                   ENDIF
093500000000     C* AGGIORNO RECORD SFL
093600000725     C                   UPDATE    TB83DVS
093700000725     C                   READC     TB83DVS                                49
093800930805     C                   ENDDO
093900000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
094000000000     C                   Z-ADD     WFLULT        SFLULT
094100000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
094200000719     C*          SFLVAR    IFEQ 0
094300000719     C*                    MOVEL'1'       $FINE
094400000719     C*                    ENDIF
094500000000     C                   ENDSR
094600940111     C*------------------------------------------------------------------------*
094700930804     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
094800940111     C*------------------------------------------------------------------------*
094900000000     C     CLRSFL        BEGSR
095000000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
095100000000     C* (0=NON FINE FILE  1=FINE FILE)
095200000000     C                   Z-ADD     0             SFLEOF
095300000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
095400000000     C                   Z-ADD     0             SFLULT
095500000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
095600000000     C                   MOVEL     '1'           *IN40
095700000725     C                   WRITE     TB83DVC
095800000000     C                   MOVEL     '0'           *IN40
095900000000     C                   ENDSR
096000940111     C*------------------------------------------------------------------------*
096100000000     C* CARSFL - CARICAMENTO SFL
096200940111     C*------------------------------------------------------------------------*
096300000000     C     CARSFL        BEGSR
096400000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
096500000000     C     SFLULT        IFEQ      0
096600000711     C                   EXSR      SETCLI
096700930805     C                   ENDIF
096800000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
096900000000     C                   Z-ADD     SFLULT        WFLULT
097000000000     C* CICLO:
097100000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
097200000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
097300000000     C                   Z-ADD     1             SFLRIG
097400930805     C     SFLRIG        DOWLE     SFLPAG
097500000000     C     SFLEOF        ANDEQ     0
097600000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
097700930806     C                   MOVEL     *BLANKS       VSCOPZ
097800000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
097900000000     C                   EXSR      IRESFL
098000000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
098100000000     C                   ADD       1             SFLULT
098200000000     C* SCRITTURA RECORD SFL
098300000725     C                   WRITE     TB83DVS
098400000000     C                   ADD       1             SFLRIG
098500000000     C* LETTURA FILE DA CARICARE
098600000711     C                   EXSR      LETCLI
098700930805     C                   ENDDO
098800000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
098900000000     C     WFLULT        IFNE      SFLULT
099000930806     C                   Z-ADD     WFLULT        VCCNBR
099100930806     C                   ADD       1             VCCNBR
099200930805     C                   ENDIF
099300930720     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
099400930805     C     SFLEOF        IFEQ      1
099500930721     C     SFLULT        ANDGT     *ZEROS
099600930721     C                   MOVEL     '1'           $ERR
099700930804     C                   MOVEL     MSG(6)        $MSG
099800930721     C                   EXSR      CARMSG
099900930804     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
100000000725     C     SFLULT        CHAIN     TB83DVS                            99
100100930804     C     *IN99         IFEQ      '0'
100200930804     C                   SETON                                        45
100300000725     C                   UPDATE    TB83DVS
100400930804     C                   SETOFF                                       45
100500930805     C                   ENDIF
100600930805     C                   ENDIF
100700000000     C                   ENDSR
100800940111     C*------------------------------------------------------------------------*
100900930804     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
101000940111     C*------------------------------------------------------------------------*
101100000000     C     OUTSFL        BEGSR
101200930805     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
101300930721     C     MSGULT        IFNE      *ZEROS
101400930721     C                   EXSR      OUTMSG
101500930805     C                   ENDIF
101600000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
101700000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
101800000000     C     SFLEOF        IFEQ      0
101900000000     C                   MOVEL     '0'           *IN42
102000000000     C                   ELSE
102100000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
102200000000     C                   MOVEL     '1'           *IN42
102300930805     C                   ENDIF
102400930720     C* EMISSIONE SUBFILE
102500000000     C     SFLULT        IFLE      0
102600930720     C*  --> CASO DI SFL VUOTO
102700930720     C                   MOVEL     '0'           *IN43
102800000725     C                   WRITE     TB83DVV
102900000725     C                   WRITE     TB83DVC
103000000725     C                   READ      TB83DVC                                99
103100000000     C                   ELSE
103200930720     C*  --> CASO DI SFL CON RECORDS
103300000725     C                   WRITE     TB83DVC
103400000725     C                   READ      TB83DVC                                99
103500930805     C                   ENDIF
103600930720     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
103700930720     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
103800930806     C                   Z-ADD     FRTSFL        VCCNBR
103900000000     C                   ENDSR
104000940111     C*------------------------------------------------------------------------*
104100930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
104200930721     C*          CODA MESSAGGI DEL PGM
104300940111     C*------------------------------------------------------------------------*
104400930721     C     CARMSG        BEGSR
104500930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
104600930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
104700930721     C     MSGULT        IFEQ      0
104800930721     C                   MOVEL     '1'           *IN40
104900000725     C                   WRITE     TB83DVK
105000930721     C                   MOVEL     '0'           *IN40
105100930805     C                   ENDIF
105200930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
105300930721     C                   MOVEL     $MSG          DSMSMS
105400930721     C                   CALL      'XMSG'        XMSGPA
105500930721     C     DSMSER        IFEQ      0
105600930806     C                   MOVEL     DSMSKE        VMCKEY
105700930805     C                   ENDIF
105800930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
105900931015     C                   ADD       1             MSGULT            4 0
106000000725     C                   WRITE     TB83DVM
106100930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
106200930721     C                   MOVEL     *BLANKS       $MSG
106300930721     C                   ENDSR
106400940111     C*------------------------------------------------------------------------*
106500930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
106600940111     C*------------------------------------------------------------------------*
106700930721     C     OUTMSG        BEGSR
106800930721     C* VISUALIZZAZIONE MESSAGGIO
106900930721     C                   MOVEL     '1'           *IN44
107000000725     C                   WRITE     TB83DVK
107100930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
107200930721     C                   Z-ADD     *ZEROS        MSGULT
107300930721     C                   ENDSR
107400940111     C*------------------------------------------------------------------------*
107500930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
107600930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
107700930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
107800930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
107900930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
108000940111     C*------------------------------------------------------------------------*
108100930803     C     CARFUN        BEGSR
108200930803     C                   Z-ADD     *ZEROS        FUNULT            5 0
108300931015     C     1             DO        31            I                 5 0
108400930803     C                   MOVEL     FUN(I)        FUNRIG
108500930805     C                   MOVEL     FUNFUN        FFU(I)
108600930805     C                   MOVEL     FUNDES        FDE(I)
108700020724     C     SIMTPP        IFEQ      'S'
108800930923     C     $TGES         IFEQ      'G'
108900930805     C                   MOVEL     FUNASG        FAT(I)
109000930803     C                   ELSE
109100930803     C                   MOVEL     FUNASI        FAT(I)
109200930803     C                   ENDIF
109300930803     C                   ELSE
109400930923     C     $TGES         IFEQ      'G'
109500930803     C                   MOVEL     FUNAFG        FAT(I)
109600930803     C                   ELSE
109700930803     C                   MOVEL     FUNAFI        FAT(I)
109800930803     C                   ENDIF
109900930803     C                   ENDIF
110000930806     C     FAT(I)        IFEQ      'E'
110100930806     C                   MOVEL     USER          WEDP              3
110200930806     C     WEDP          IFEQ      'EDP'
110300930806     C                   MOVEL     'S'           FAT(I)
110400930806     C                   ELSE
110500930806     C                   MOVEL     'N'           FAT(I)
110600930806     C                   ENDIF
110700930806     C                   ENDIF
110800930803     C                   ENDDO
110900930803     C                   ENDSR
111000940111     C*------------------------------------------------------------------------*
111100930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
111200930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
111300930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
111400930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
111500940111     C*------------------------------------------------------------------------*
111600930803     C     OUTFUN        BEGSR
111700930805     C     1             DO        8             I
111800930805     C                   MOVEL     *BLANKS       FUV(I)
111900930803     C                   ENDDO
112000930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
112100930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0
112200930803     C     1             DO        31            I
112300930803     C     FAT(I)        IFEQ      'S'
112400930803     C     FDE(I)        ANDNE     *BLANKS
112500930803     C                   ADD       1             FUNNAT
112600930803     C                   ENDIF
112700930803     C                   ENDDO
112800930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
112900930803     C     FUNNAT        IFGT      8
113000930803     C                   MOVEL     'S'           FAT(24)
113100930803     C                   ENDIF
113200930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
113300930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
113400930803     C                   Z-ADD     8             FUNVIS            5 0
113500930803     C     FAT(23)       IFEQ      'S'
113600930803     C                   SUB       1             FUNVIS
113700930803     C                   ENDIF
113800930803     C     FAT(24)       IFEQ      'S'
113900930803     C                   SUB       1             FUNVIS
114000930803     C                   ENDIF
114100930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
114200931015     C                   Z-ADD     *ZEROS        W                 5 0
114300930803     C                   ADD       1             FUNULT
114400930803     C     FUNULT        IFGT      31
114500930803     C                   Z-ADD     1             FUNULT
114600930805     C                   ENDIF
114700930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
114800930803     C     FUNULT        DO        31            I
114900930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
115000930803     C     W             IFLT      FUNVIS
115100930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
115200930803     C     FAT(I)        IFEQ      'S'
115300930803     C     FDE(I)        ANDNE     *BLANKS
115400930805     C     I             IFEQ      23
115500930805     C     I             OREQ      24
115600930803     C                   ELSE
115700930803     C                   ADD       1             W
115800930803     C                   MOVEL     FDE(I)        FUV(W)
115900930803     C                   ENDIF
116000930803     C                   ENDIF
116100930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
116200930803     C                   Z-ADD     I             FUNULT
116300930803     C                   ENDIF
116400930803     C                   ENDDO
116500930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
116600930806     C                   MOVEL     FUV(1)        VFD001
116700930806     C                   MOVEL     FUV(2)        VFD002
116800930806     C                   MOVEL     FUV(3)        VFD003
116900930806     C                   MOVEL     FUV(4)        VFD004
117000930806     C                   MOVEL     FUV(5)        VFD005
117100930806     C                   MOVEL     FUV(6)        VFD006
117200930803     C     FAT(23)       IFEQ      'N'
117300930803     C     FAT(24)       ANDEQ     'N'
117400930806     C                   MOVEL     FUV(7)        VFD007
117500930806     C                   MOVEL     FUV(8)        VFD008
117600930803     C                   ELSE
117700930803     C     FAT(23)       IFEQ      'S'
117800930803     C     FAT(24)       ANDEQ     'S'
117900930806     C                   MOVEL     FDE(23)       VFD007
118000930806     C                   MOVEL     FDE(24)       VFD008
118100930803     C                   ELSE
118200930806     C                   MOVEL     FUV(7)        VFD007
118300930803     C     FAT(23)       IFEQ      'S'
118400930806     C                   MOVEL     FDE(23)       VFD008
118500930803     C                   ELSE
118600930806     C                   MOVEL     FDE(24)       VFD008
118700930805     C                   ENDIF
118800930805     C                   ENDIF
118900930805     C                   ENDIF
119000930803     C                   MOVEL     '1'           $TESVD
119100930803     C                   ENDSR
119200940111     C*------------------------------------------------------------------------*
119300930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
119400930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
119500930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
119600930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
119700940111     C*------------------------------------------------------------------------*
119800930803     C     CONFUN        BEGSR
119900930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
120000930803     C     1             DO        31            I
120100930803     C     KEY           IFEQ      KEYS(I)
120200930805     C* CONTROLLO SE IL TASTO E' ATTIVO
120300930803     C     FAT(I)        IFNE      'S'
120400930804     C                   MOVEL     MSG(1)        $MSG
120500930803     C                   EXSR      CARMSG
120600930803     C                   MOVEL     '1'           $ERR
120700930803     C                   ENDIF
120800930803     C                   ENDIF
120900930803     C                   ENDDO
121000930803     C                   ENDSR
121100940111     C*------------------------------------------------------------------------*
121200930802     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
121300930802     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
121400930803     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
121500930803     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
121600930803     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
121700930803     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
121800940111     C*------------------------------------------------------------------------*
121900930802     C     CAROPZ        BEGSR
122000930803     C                   Z-ADD     *ZEROS        OPZULT            5 0
122100930802     C     1             DO        24            I
122200930803     C                   MOVEL     OPZ(I)        OPZRIG
122300930805     C                   MOVEL     OPZOPD        OPD(I)
122400930805     C                   MOVEL     OPZOPS        OPS(I)
122500930805     C                   MOVEL     OPZDES        ODE(I)
122600020724     C     SIMTPP        IFEQ      'S'
122700930923     C     $TGES         IFEQ      'G'
122800930805     C                   MOVEL     OPZASG        OAT(I)
122900930802     C                   ELSE
123000930805     C                   MOVEL     OPZASI        OAT(I)
123100930802     C                   ENDIF
123200930802     C                   ELSE
123300930923     C     $TGES         IFEQ      'G'
123400930805     C                   MOVEL     OPZAFG        OAT(I)
123500930802     C                   ELSE
123600930805     C                   MOVEL     OPZAFI        OAT(I)
123700930802     C                   ENDIF
123800930802     C                   ENDIF
123900930806     C     OAT(I)        IFEQ      'E'
124000930806     C                   MOVEL     USER          WEDP              3
124100930806     C     WEDP          IFEQ      'EDP'
124200930806     C                   MOVEL     'S'           OAT(I)
124300930806     C                   ELSE
124400930806     C                   MOVEL     'N'           OAT(I)
124500930806     C                   ENDIF
124600930806     C                   ENDIF
124700930802     C                   ENDDO
124800930802     C                   ENDSR
124900940111     C*------------------------------------------------------------------------*
125000930802     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
125100930802     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
125200930802     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
125300930806     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
125400930802     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
125500940111     C*------------------------------------------------------------------------*
125600930802     C     OUTOPZ        BEGSR
125700930805     C     1             DO        8             I
125800930805     C                   MOVEL     *BLANKS       OPV(I)
125900930802     C                   ENDDO
126000930802     C                   Z-ADD     *ZEROS        OPZNAT            2 0
126100930805     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
126200930803     C                   ADD       1             OPZULT
126300930803     C     OPZULT        IFGT      24
126400930803     C                   Z-ADD     1             OPZULT
126500930803     C                   END
126600930805     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
126700930802     C     1             DO        24            I
126800930805     C     OAT(I)        IFEQ      'S'
126900930805     C     ODE(I)        ANDNE     *BLANKS
127000930802     C                   ADD       1             OPZNAT
127100930802     C                   ENDIF
127200930802     C                   ENDDO
127300930802     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
127400931015     C                   Z-ADD     0             W
127500930802     C     OPZULT        DO        24            I
127600930802     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
127700930802     C     W             IFLT      8
127800930802     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
127900930805     C     OAT(I)        IFEQ      'S'
128000930803     C     ODE(I)        ANDNE     *BLANKS
128100930802     C                   ADD       1             W
128200930803     C                   MOVEL     ODE(I)        OPV(W)
128300930805     C                   ENDIF
128400930805     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
128500930802     C                   Z-ADD     I             OPZULT
128600930802     C                   ENDIF
128700930802     C                   ENDDO
128800930802     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
128900930806     C                   MOVEL     OPV(1)        VOD001
129000930806     C                   MOVEL     OPV(2)        VOD002
129100930806     C                   MOVEL     OPV(3)        VOD003
129200930806     C                   MOVEL     OPV(4)        VOD004
129300930806     C                   MOVEL     OPV(5)        VOD005
129400930806     C                   MOVEL     OPV(6)        VOD006
129500930806     C                   MOVEL     OPV(7)        VOD007
129600930806     C                   MOVEL     OPV(8)        VOD008
129700930802     C                   MOVEL     '1'           $TESVD
129800930923     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
129900930923     C     OPZNAT        IFGT      8
130000930923     C                   MOVEL     'S'           FAT(23)
130100930923     C                   ENDIF
130200930802     C                   ENDSR
130300940111     C*------------------------------------------------------------------------*
130400930802     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
130500930806     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
130600930802     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
130700930802     C*                   - ERRORE SE OPZIONE NON ATTIVA
130800940111     C*------------------------------------------------------------------------*
130900930802     C     CONOPZ        BEGSR
131000000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
131100930803     C                   MOVEL     *BLANKS       OPZOK             1
131200930805     C     1             DO        24            I                 5 0
131300930806     C     VSCOPZ        IFEQ      OPD(I)
131400930806     C     VSCOPZ        OREQ      OPS(I)
131500930805     C     OAT(I)        IFEQ      'S'
131600930806     C                   MOVEL     OPD(I)        VSCOPZ
131700940105     C                   MOVEL     'S'           OPZOK
131800930805     C                   ELSE
131900930802     C                   MOVEL     'N'           OPZOK             1
132000930805     C                   ENDIF
132100930802     C                   ENDIF
132200930802     C                   ENDDO
132300930805     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
132400930803     C     OPZOK         IFNE      'S'
132500930804     C                   MOVEL     MSG(3)        $MSG
132600930803     C                   EXSR      CARMSG
132700930803     C                   MOVEL     '1'           $ERR
132800930803     C                   MOVEL     'N'           OPZOK             1
132900930803     C                   ENDIF
133000000000     C                   ENDSR
133100930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
133200000725TRTB83R- Tasto funzionale non valido.                                          1
133300000725TRTB83R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
133400000725TRTB83R- Opzione non valida.                                                   3
133500000725TRTB83R- Opzione abilitata ma non gestita. Avvertire CED.                      4
133600000725TRTB83R- Inizio elenco.                                                        5
133700000725TRTB83R- Fine elenco.                                                          6
133800000725TRTB83R- 7                                                                     7
133900000725TRTB83R- 8                                                                     8
134000000725TRTB83R- 9                                                                     9
134100000725TRTB83R- 10                                                                   10
134200000725TRTB83R- 11                                                                   11
134300000725TRTB83R- 12                                                                   12
134400000725TRTB83R- 13                                                                   13
134500000725TRTB83R- 14                                                                   14
134600000725TRTB83R- 15                                                                   15
134700000725TRTB83R- 16                                                                   16
134800000725TRTB83R- 17                                                                   17
134900000725TRTB83R- 18                                                                   18
135000000725TRTB83R- 19                                                                   19
135100000725TRTB83R- 20                                                                   20
135200930729** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
135300930803- 1-1 -                   -    NN     NN
135400940112- 2-2 -2=Modifica         -    SS     SS
135500000711- 3-3 -3=COPIA            -    SS     SS
135600940112- 4-4 -4=Cancellazione    -    SS     SS
135700940121- 5-5 -5=Visualizzazione  -    SS     SS
135800930803- 6-6 -                   -    NN     NN
135900940112- 7-7 -7=Ripristino       -    SS     SS
136000940104- 8-8 -                   -    NN     NN
136100940111- 9-9 -                   -    NN     NN
136200930806-10-  -                   -    NN     NN
136300930806-11-  -                   -    NN     NN
136400930806-12-  -                   -    NN     NN
136500930803-13-  -                   -    NN     NN
136600930803-14-  -                   -    NN     NN
136700930803-15-  -                   -    NN     NN
136800930803-16-  -                   -    NN     NN
136900930803-17-  -                   -    NN     NN
137000930803-18-  -                   -    NN     NN
137100930803-19-  -                   -    NN     NN
137200930803-20-  -                   -    NN     NN
137300930803-21-  -                   -    NN     NN
137400930803-22-  -                   -    NN     NN
137500930803-23-  -                   -    NN     NN
137600930803-24-  -                   -    NN     NN
137700930730** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
137800930723F1    -                   -    NN     NN
137900930723F2    -                   -    NN     NN
138000930802F3    -F3=Fine            -    SS     SS
138100930723F4    -                   -    NN     NN
138200940112F5    -F5=Rivisualizzaz.  -    SS     SS
138300940112F6    -F6=Inserimento     -    SS     SS
138400930723F7    -                   -    NN     NN
138500930723F8    -                   -    NN     NN
138600930723F9    -                   -    NN     NN
138700930723F10   -                   -    NN     NN
138800930723F11   -                   -    NN     NN
138900930802F12   -F12=Ritorno        -    SS     SS
139000930806F13   -                   -    NN     NN
139100930806F14   -                   -    NN     NN
139200930723F15   -                   -    NN     NN
139300930723F16   -                   -    NN     NN
139400930802F17   -F17=Inizio dati    -    SS     SS
139500930802F18   -F18=Fine dati      -    SS     SS
139600930723F19   -                   -    NN     NN
139700930723F20   -                   -    NN     NN
139800940111F21   -                   -    NN     NN
139900940111F22   -                   -    NN     NN
140000930803F23   -F23=Altre opzioni  -    NN     NN
140100930803F24   -F24=Altri tasti    -    NN     NN
140200930802ENTER -                   -    SS     SS
140300930802ROLLUP-                   -    SS     SS
140400930802ROLLDN-                   -    SS     SS
140500930802HELP  -                   -    NN     NN
140600930802PRINT -                   -    NN     NN
140700930802BSPACE-                   -    NN     NN
140800930802CLEAR -                   -    NN     NN
