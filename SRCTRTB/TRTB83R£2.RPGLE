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
001400120216
001500120216       //--------------------------------------------------------------
001600120216       //?Definizione costanti.                                        ?
001700120216       //--------------------------------------------------------------
001800120216
001900120216       // -?Costante per controllo "caratteri solo numerici"?
002000120216     d c_Digits        c                   const('0123456789')
002100120216
002200940111     D*------------------------------------------------------------------------*
002300940111     D* SCHIERE
002400940111     D*------------------------------------------------------------------------*
002500940105     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002600930802     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002700930802     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
002800930802     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
002900930802     D FUN             S             40    DIM(31)
003000930802     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003100930803     D FFU             S              6    DIM(31)
003200930803     D FDE             S             19    DIM(31)
003300930803     D FAT             S              1    DIM(31)
003400930802     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003500930803     D OPD             S              2    DIM(24)
003600930803     D OPS             S              2    DIM(24)
003700930803     D ODE             S             19    DIM(24)
003800930803     D OAT             S              1    DIM(24)
003900930802     D* TABELLA DESCRIZIONI A VIDEO
004000930802     D FUV             S             19    DIM(8)
004100930802     D OPV             S             19    DIM(8)
004200930802     D* TABELLA TASTI FUNZIONALI PREMUTI
004300930802     D KEYS            S              1    DIM(40)
004400940111     D*------------------------------------------------------------------------*
004500940111     D* INPUT
004600940111     D*------------------------------------------------------------------------*
004700940111     D*---------------------
004800930723     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
004900940111     D*---------------------
005000930723     D DSOPZ         E DS
005100930803     D  OPZRIG                 1     40
005200940111     D*---------------------
005300930723     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
005400940111     D*---------------------
005500930723     D DSFUN         E DS
005600930803     D  FUNRIG                 1     40
005700940111     D*---------------------
005800930127     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005900940111     D*---------------------
006000930127     D DSMSG         E DS
006100940111     D*---------------------
006200020724     D CNCR80        E DS
006300020724     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006400940111     D*---------------------
006500000000     D* DS PER GESTIONE TASTI FUNZIONALI
006600940111     D*---------------------
006700930802     D DSKEYS        E DS
006800930803     D  KEYRIG                 1     40
006900940111     D*---------------------
007000930806     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007100940111     D*---------------------
007200930806     D                SDS
007300940111     D  USER                 254    263
007400940111     D*---------------------
007500930720     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007600940111     D*---------------------
007700000000     D DEVDS1          DS
007800000000     D  KEY                  369    369
007900930721     D  CURSOR               370    371
008000930720     D  FRTSFL               378    379B 0
008100940111     D*---------------------
008200940111     D* DS TEMPI FISSI CALCOLO DELIVERY
008300940111     D*---------------------
008400000725     D DS3Q          E DS
008500940111     D*---------------------
008600940111     D* DS ESTERNA DI PROCEDURA
008700940111     D*---------------------
008800000725     D DS83          E DS                  EXTNAME(TRTB83DS)
008900980130     D*------------------
009000980130     D* DS CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
009100980130     D*------------------
009200980130     D UL12DS        E DS                  EXTNAME(TRUL12DS)
009300940111     D*---------------------
009400940111     D* ARCHITETTURA
009500940111     D*---------------------
009600940111     D KPJBA         E DS
009700940111     C*------------------------------------------------------------------------*
009800940111     C* MAIN LINE
009900940111     C*------------------------------------------------------------------------*
010000000000     C*
010100000000     C* CICLO FINO A CHE:
010200940111DO  1C     $FINE         DOUEQ     '1'
010300000000     C*
010400930805     C                   MOVEL     '0'           $ERR
010500000000     C*
010600930721     C* CONTROLLA GESTIONE VIDEO
010700940111IF  2C     $GES          IFEQ      'VS'
010800930806     C                   EXSR      GESVS
010900940111E   2C                   ENDIF
011000000000     C*
011100940111E   1C                   ENDDO
011200940111     C* FINE PGM
011300000000     C                   SETON                                        LR
011400940111     C*------------------------------------------------------------------------*
011500930806     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
011600940111     C*------------------------------------------------------------------------*
011700930806     C     CARVS         BEGSR
011800930713     C*
011900930806     C                   MOVEL     'VS'          $GES
012000930713     C*
012100930804     C                   EXSR      CAROPZ
012200930804     C                   EXSR      OUTOPZ
012300930804     C                   MOVEA     FUN2          FUN
012400930804     C                   EXSR      CARFUN
012500930804     C                   EXSR      OUTFUN
012600930713     C*
012700000725     C                   WRITE     TB83DVT
012800000725     C                   WRITE     TB83DVO
012900000725     C                   WRITE     TB83DVF
013000930713     C*
013100930806     C                   EXSR      POSSFL
013200930713     C*
013300930713     C                   ENDSR
013400940111     C*------------------------------------------------------------------------*
013500930806     C* GESVS  - GESTIONE SUBFILE
013600940111     C*------------------------------------------------------------------------*
013700930806     C     GESVS         BEGSR
013800930714     C*
013900940111IF  1C     $TESVD        IFEQ      '1'
014000000725     C                   WRITE     TB83DVT
014100000725     C                   WRITE     TB83DVO
014200000725     C                   WRITE     TB83DVF
014300940111E   1C                   ENDIF
014400930713     C*
014500930805     C                   EXSR      OUTSFL
014600930713     C*
014700930805     C                   EXSR      CONFUN
014800930805     C*
014900940111IF  1C     $ERR          IFEQ      '0'
015000940111SL  2C                   SELECT
015100930804     C     KEY           WHENEQ    F6
015200930804     C                   EXSR      F06GES
015300940111     C                   OTHER
015400930804     C                   EXSR      FUNSFL
015500940111E   2C                   ENDSL
015600940111E   1C                   ENDIF
015700930713     C*
015800930713     C                   ENDSR
015900940111     C*------------------------------------------------------------------------*
016000930804     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
016100940111     C*------------------------------------------------------------------------*
016200930126     C     F06GES        BEGSR
016300930127     C*
016400940111     C* LANCIO L'INSERIMENTO
016500090518     C                   MOVEL     *BLANK        WCLI              7
016600940114     C                   MOVEL     'GES'         WOP0              3
016700940114     C                   MOVEL     'F06'         WOP1              3
016800000725     C                   MOVEL     'TRTB84R'     WPGM              8
016900930805     C                   EXSR      LANPGM
017000930128     C*
017100930805     C                   EXSR      POSSFL
017200930127     C*
017300000000     C                   ENDSR
017400940111     C*------------------------------------------------------------------------*
017500930803     C*  GESOPZ - GESTIONE OPZIONI
017600940111     C*------------------------------------------------------------------------*
017700930803     C     GESOPZ        BEGSR
017800930804     C*
017900940111SL  1C                   SELECT
018000930806     C     VSCOPZ        WHENEQ    ' 2'
018100930804     C                   EXSR      O02GES
018200000711     C     VSCOPZ        WHENEQ    ' 3'
018300000711     C                   EXSR      O03GES
018400930806     C     VSCOPZ        WHENEQ    ' 4'
018500930804     C                   EXSR      O04GES
018600940121     C     VSCOPZ        WHENEQ    ' 5'
018700940121     C                   EXSR      O05GES
018800930806     C     VSCOPZ        WHENEQ    ' 7'
018900930804     C                   EXSR      O07GES
019000930804     C                   OTHER
019100930804     C                   MOVEL     MSG(4)        $MSG
019200930805     C                   EXSR      CARMSG
019300940111E   1C                   ENDSL
019400930804     C*
019500000000     C                   ENDSR
019600940111     C*------------------------------------------------------------------------*
019700000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
019800940111     C*------------------------------------------------------------------------*
019900000000     C     O02GES        BEGSR
020000930128     C*
020100000801IF  1C     VSCATB        IFEQ      'A'
020200000801     C                   MOVEL     '1'           $ERR
020300000801     C                   MOVEL     MSG(12)       $MSG
020400000801     C                   EXSR      CARMSG
020500000801E   1C                   ENDIF
020600931217     C*
020700940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
020800000711IF  1C     VIDCLI        IFEQ      *BLANKS
020900940117     C                   MOVEL     USER          WEDP              3
021000940117IF  2C     WEDP          IFNE      'EDP'
021100940117     C                   MOVEL     '1'           $ERR
021200940117     C                   MOVEL     MSG(11)       $MSG
021300940117     C                   EXSR      CARMSG
021400940117E   2C                   ENDIF
021500940117E   1C                   ENDIF
021600940117     C*
021700940111IF  1C     $ERR          IFEQ      '0'
021800000000     C*
021900940111     C* LANCIO LA MODIFICA
022000000711     C                   MOVEL     VIDCLI        WCLI
022100000711     C                   Z-ADD     VIDCKS        WCKS              7 0
022200000711     C                   MOVE      VIDCSD        WCSD              2
022300000711     C                   MOVE      VIDCDM        WCDM             10
022400000711     C                   MOVE      VIDPRF        WPRF             10
022500000711     C                   MOVE      VIDNHL        WNHL              2
022600940114     C                   MOVEL     'GES'         WOP0
022700940114     C                   MOVEL     'O02'         WOP1
022800000725     C                   MOVEL     'TRTB84R'     WPGM
022900930805     C                   EXSR      LANPGM
023000000000     C*
023100930805     C                   Z-ADD     1             SWMODI
023200930128     C*
023300940111E   1C                   ENDIF
023400000000     C*
023500000000     C                   ENDSR
023600000711     C*------------------------------------------------------------------------*
023700000711     C*  O03GES - GESTIONE OPZIONE 3=COPIA
023800000711     C*------------------------------------------------------------------------*
023900000711     C     O03GES        BEGSR
024000000711     C*
024100000711     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
024200000801IF  1C     VSCATB        IFEQ      'A'
024300000801     C                   MOVEL     '1'           $ERR
024400000801     C                   MOVEL     MSG(12)       $MSG
024500000801     C                   EXSR      CARMSG
024600000801E   1C                   ENDIF
024700000711     C*
024800000711     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
024900000711IF  1C     VIDCLI        IFEQ      *BLANKS
025000000711     C                   MOVEL     USER          WEDP              3
025100000711IF  2C     WEDP          IFNE      'EDP'
025200000711     C                   MOVEL     '1'           $ERR
025300000711     C                   MOVEL     MSG(11)       $MSG
025400000711     C                   EXSR      CARMSG
025500000711E   2C                   ENDIF
025600000711E   1C                   ENDIF
025700000711     C*
025800000711IF  1C     $ERR          IFEQ      '0'
025900000711     C*
026000000711     C* LANCIO LA COPIA
026100000711     C                   MOVEL     VIDCLI        WCLI
026200000711     C                   Z-ADD     VIDCKS        WCKS
026300000711     C                   MOVE      VIDCSD        WCSD
026400000711     C                   MOVE      VIDCDM        WCDM
026500000711     C                   MOVE      VIDPRF        WPRF
026600000711     C                   MOVE      VIDNHL        WNHL
026700000711     C                   MOVEL     'GES'         WOP0
026800000711     C                   MOVEL     'O03'         WOP1
026900000725     C                   MOVEL     'TRTB84R'     WPGM
027000000711     C                   EXSR      LANPGM
027100000711     C*
027200000711     C                   Z-ADD     1             SWMODI
027300000711     C*
027400000711E   1C                   ENDIF
027500000711     C*
027600000711     C                   ENDSR
027700940111     C*------------------------------------------------------------------------*
027800000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
027900940111     C*------------------------------------------------------------------------*
028000000000     C     O04GES        BEGSR
028100930128     C*
028200930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
028300000801IF  1C     VSCATB        IFEQ      'A'
028400000801     C                   MOVEL     '1'           $ERR
028500000801     C                   MOVEL     MSG(12)       $MSG
028600000801     C                   EXSR      CARMSG
028700000801E   1C                   ENDIF
028800940117     C*
028900940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
029000000711IF  1C     VIDCLI        IFEQ      *BLANKS
029100940117     C                   MOVEL     USER          WEDP              3
029200940117IF  2C     WEDP          IFNE      'EDP'
029300940117     C                   MOVEL     '1'           $ERR
029400940117     C                   MOVEL     MSG(11)       $MSG
029500940117     C                   EXSR      CARMSG
029600940117E   2C                   ENDIF
029700940117E   1C                   ENDIF
029800931217     C*
029900940111IF  1C     $ERR          IFEQ      '0'
030000940111     C*
030100940111     C* LANCIO LA CANCELLAZIONE
030200000711     C                   MOVEL     VIDCLI        WCLI
030300000711     C                   Z-ADD     VIDCKS        WCKS
030400000711     C                   MOVE      VIDCSD        WCSD
030500000711     C                   MOVE      VIDCDM        WCDM
030600000711     C                   MOVE      VIDPRF        WPRF
030700000711     C                   MOVE      VIDNHL        WNHL
030800940114     C                   MOVEL     'GES'         WOP0
030900940114     C                   MOVEL     'O04'         WOP1
031000000725     C                   MOVEL     'TRTB84R'     WPGM
031100930201     C                   EXSR      LANPGM
031200000000     C*
031300930805     C                   Z-ADD     1             SWMODI
031400940111     C*
031500940111E   1C                   ENDIF
031600000000     C*
031700000000     C                   ENDSR
031800940121     C*------------------------------------------------------------------------*
031900000711     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZA
032000940121     C*------------------------------------------------------------------------*
032100940121     C     O05GES        BEGSR
032200940121     C*
032300940121     C* LANCIO LA VISUALIZZAZIONE
032400000711     C                   MOVEL     VIDCLI        WCLI
032500000711     C                   Z-ADD     VIDCKS        WCKS
032600000711     C                   MOVE      VIDCSD        WCSD
032700000711     C                   MOVE      VIDCDM        WCDM
032800000711     C                   MOVE      VIDPRF        WPRF
032900000711     C                   MOVE      VIDNHL        WNHL
033000940121     C                   MOVEL     'GES'         WOP0
033100940121     C                   MOVEL     'O05'         WOP1
033200000725     C                   MOVEL     'TRTB84R'     WPGM
033300940121     C                   EXSR      LANPGM
033400940121     C*
033500940121     C                   ENDSR
033600940111     C*------------------------------------------------------------------------*
033700930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
033800940111     C*------------------------------------------------------------------------*
033900930128     C     O07GES        BEGSR
034000930128     C*
034100930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
034200000801IF  1C     VSCATB        IFNE      'A'
034300000801     C                   MOVEL     '1'           $ERR
034400000801     C                   MOVEL     MSG(13)       $MSG
034500000801     C                   EXSR      CARMSG
034600000801E   1C                   ENDIF
034700940117     C*
034800940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
034900000711IF  1C     VIDCLI        IFEQ      *BLANKS
035000940117     C                   MOVEL     USER          WEDP              3
035100940117IF  2C     WEDP          IFNE      'EDP'
035200940117     C                   MOVEL     '1'           $ERR
035300940117     C                   MOVEL     MSG(11)       $MSG
035400940117     C                   EXSR      CARMSG
035500940117E   2C                   ENDIF
035600940117E   1C                   ENDIF
035700930215     C*
035800940111IF  1C     $ERR          IFEQ      '0'
035900930128     C*
036000940111     C* LANCIO IL RIPRISTINO
036100000711     C                   MOVEL     VIDCLI        WCLI
036200000711     C                   Z-ADD     VIDCKS        WCKS
036300000711     C                   MOVE      VIDCSD        WCSD
036400000711     C                   MOVE      VIDCDM        WCDM
036500000711     C                   MOVE      VIDPRF        WPRF
036600000711     C                   MOVE      VIDNHL        WNHL
036700940114     C                   MOVEL     'GES'         WOP0
036800940114     C                   MOVEL     'O07'         WOP1
036900000725     C                   MOVEL     'TRTB84R'     WPGM
037000930201     C                   EXSR      LANPGM
037100930128     C*
037200930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
037300930128     C                   Z-ADD     1             SWMODI
037400930128     C*
037500940111E   1C                   ENDIF
037600930128     C*
037700930128     C                   ENDSR
037800940111     C*------------------------------------------------------------------------*
037900930205     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
038000940111     C*------------------------------------------------------------------------*
038100930205     C     NEWPOS        BEGSR
038200930205     C*
038300940111     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
038400940111     C*        1 = RICHIESTO POSIZIONAMENTO
038500930205     C                   Z-ADD     0             SWPOSI            7 0
038600930205     C*
038700940111     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
038800940111     C*            NON OCCORRE RICARICARE IL SUBFILE
038900940111     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
039000930205     C                   Z-ADD     0             SWMODI            1 0
039100930205     C*
039200930205     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
039300000719IF  1C     VCCCLI        IFNE      DEPCLI
039400101119IF  1C     VCCuni        orne      DEPuni
039500101119IF  1C     VCCsun        orne      DEPsun
039600930205     C*
039700930205     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
039800930205     C                   EXSR      CHKPAR
039900930205     C*
040000930205     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
040100940111IF  2C     $ERR          IFEQ      '0'
040200940111     C                   Z-ADD     1             SWPOSI
040300000711     C                   MOVEL     VCCCLI        DEPCLI
040400101119     C                   Z-ADD     VCCuni        DEPuni
040500101119     C                   movel     VCCsun        DEPsun
040600940111E   2C                   ENDIF
040700000719E   1C                   ENDIF
040800930205     C*
040900930205     C                   ENDSR
041000940111     C*------------------------------------------------------------------------*
041100940111     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
041200940111     C*------------------------------------------------------------------------*
041300940111     C     CHKPAR        BEGSR
041400940111     C*
041500940111     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
041600940111     C                   SETOFF                                       80
041700940111     C*
041800000718     C* CONTROLLO ESISTENZA
041900090518     C                   MOVEL     VCCCLI        WCLI              7
042000000711     C                   MOVEL     *BLANKS       WDCLI            22
042100000711     C                   EXSR      CHKCLI
042200940113IF  2C     $ERR          IFEQ      '1'
042300940111     C                   MOVEL     '1'           $ERR
042400940111     C                   MOVEL     MSG(9)        $MSG
042500940111     C                   EXSR      CARMSG
042600940111     C                   SETON                                        80
042700940113E   2C                   ENDIF
042800000719     C                   MOVEL     WDCLI         VIDDCL
042900940111     C*
043000940111     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
043100940111     C* CURSORE SUL CAMPO DEL CONTROL
043200940111IF  1C     $ERR          IFEQ      '1'
043300940111     C                   MOVEL     '0'           *IN43
043400940111X   1C                   ELSE
043500940111     C                   MOVEL     '1'           *IN43
043600940111E   1C                   ENDIF
043700940111     C*
043800940111     C                   ENDSR
043900940111     C*------------------------------------------------------------------------*
044000000711     C*  CHKCLI - CONTROLLA LA FILIALE
044100940111     C*------------------------------------------------------------------------*
044200000711     C     CHKCLI        BEGSR
044300940111     C*
044400940114     C* CONTROLLO ESISTENZA NELL'ORGANIGRAMMA
044500120216     C*//                MOVE      VIDCLI        ULTIMO            1
044600120216     C*//  ULTIMO        IFEQ      *BLANK
044700120216     C*//  VIDCLI        ANDNE     *BLANK
044800120216     c                   if        %check( c_Digits : VIDcli ) = *zero
044900000718     C                   MOVEL     VIDCLI        KORCLI
045000000711     C     KEYCLI        CHAIN     CNACO00F                           99
045100940114IF  2C     *IN99         IFEQ      '1'
045200940114     C                   MOVEL     '1'           $ERR
045300000711     C                   MOVEL     *ALL'*'       WDCLI
045400940114X   2C                   ELSE
045500000711     C                   MOVEL     ACORAG        WDCLI
045600940114E   2C                   ENDIF
045700000711     C                   ELSE
045800000711     C                   MOVEL     *BLANK        WDCLI
045900000711     C                   END
046000940113     C*
046100940114     C                   ENDSR
046200940112     C*------------------------------------------------------------------------*
046300000711     C* SETCLI - POSIZIONAMENTO E PRIMA LETTURA RECORD TABELLE
046400940112     C*------------------------------------------------------------------------*
046500000711     C     SETCLI        BEGSR
046600940112     C*
046700000719     C* POSIZIONAMENTO RECORD TABELLA
046800940112     C*
046900020724     C                   Z-ADD     1             KTBKUT
047000000725     C                   MOVEL     '3Q'          KTBCOD
047100940112     C                   MOVEL     *BLANKS       KTBKEY
047200000711     C                   MOVEL     WCLI          KTBKEY
047300940112     C*
047400940113     C     KEYTAB        SETLL     TABEL00F                           99
047500940112     C*
047600000711     C                   EXSR      LETCLI
047700940112     C*
047800940112     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
047900940112IF  1C     *IN99         IFEQ      '1'
048000940112     C                   Z-ADD     1             SFLEOF
048100940112E   1C                   ENDIF
048200940112     C*
048300940112     C                   ENDSR
048400940112     C*------------------------------------------------------------------------*
048500000711     C* LETCLI - LEGGE RECORD SUCCESSIVO
048600940112     C*------------------------------------------------------------------------*
048700000711     C     LETCLI        BEGSR
048800940112     C*
048900940113     C* FINO A QUANDO NON TROVI UN RECORD VALIDO (RECOK = '1') STAI DENTRO
049000940113     C                   MOVEL     '0'           RECOK             1
049100940113DO  1C     RECOK         DOWEQ     '0'
049200940112     C*
049300000711     C* LETTURA RECORD TABELLA
049400940113     C                   READ      TABEL00F                               99
049500101119     C                   MOVEL     '1'           RECOK
049600940112     C*
049700940113     C* FINE FILE  O TABELLA DIVERSA
049800940112IF  2C     *IN99         IFEQ      '1'
049900940113     C     TBLKUT        ORNE      1
050000000725     C     TBLCOD        ORNE      '3Q'
050100940112     C                   Z-ADD     1             SFLEOF
050200000718     C                   ELSE
050300101119     c                   movel     tbluni        ds3Q
050400101119     c                   movel     §3Qcks        w007a             7
050500101119if  3c                   if        (VCCuni > *zero and VCCuni <> §3Qcks)
050600101119     c                             or
050700101119     c                             (VCCsun = 'S' and %subst(TBLkey:1:7)<>w007a)
050800101119     C                   MOVEL     '0'           RECOK
050900101119e   3c                   endif
051000940112E   2C                   ENDIF
051100940112E   1C                   ENDDO
051200940112     C*
051300940112     C                   ENDSR
051400940111     C*------------------------------------------------------------------------*
051500000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
051600940111     C*------------------------------------------------------------------------*
051700000000     C     IRESFL        BEGSR
051800940112     C*
051900000719     C* RIEMPIMENTO CAMPI
052000000801IF  1C     TBLFLG        IFNE      *BLANKS
052100000801     C                   MOVEL     'A'           VSCATB
052200000801X   1C                   ELSE
052300000801     C                   MOVEL     ' '           VSCATB
052400000801E   1C                   ENDIF
052500000711     C                   MOVEL     TBLKEY        VIDCLI
052600000725     C                   MOVEL     TBLUNI        DS3Q
052700000725     C                   MOVEL     §3QCSD        VIDCSD
052800000725     C                   MOVEL     §3QCDM        VIDCDM
052900000725     C                   MOVEL     §3QCKS        VIDCKS
053000000725     C                   MOVEL     §3QPRF        VIDPRF
053100000725     C                   MOVEL     §3QNHL        VIDNHL
053200000718     C                   EXSR      CHKCLI
053300000718     C                   MOVEL     WDCLI         VIDDCL
053400940112     C*
053500000000     C                   ENDSR
053600940111     C*------------------------------------------------------------------------*
053700930201     C* LANPGM - LANCIA UN PROGRAMMA
053800940111     C*------------------------------------------------------------------------*
053900930201     C     LANPGM        BEGSR
054000940114     C*
054100940114     C*--------------------
054200940114     C* PARAMETRI INFORMATIVI
054300940114     C*--------------------
054400000725     C                   MOVEL     WOP0          D83OP0
054500000725     C                   MOVEL     WOP1          D83OP1
054600000725     C                   MOVEL     '0'           D83F03
054700000725     C                   MOVEL     '0'           D83ERR
054800000725     C                   MOVEL     *BLANKS       D83MSG
054900940114     C*--------------------
055000940114     C* PARAMETRI
055100940114     C*--------------------
055200000725     C                   MOVEL     WCLI          D83CLI
055300000725     C                   Z-ADD     WCKS          D83CKS
055400000725     C                   MOVE      WCSD          D83CSD
055500000725     C                   MOVE      WCDM          D83CDM
055600000725     C                   MOVE      WPRF          D83PRF
055700000725     C                   MOVE      WNHL          D83NHL
055800940114     C* LANCIO
055900940114     C                   CALL      WPGM
056000000725     C                   PARM                    DS83
056100940114     C*
056200930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
056300000725IF  1C     D83F03        IFEQ      '1'
056400930721     C                   MOVEL     '1'           $F03
056500930721     C                   MOVEL     '1'           $ERR
056600940105     C                   MOVEL     '1'           $FINE
056700940114     C*
056800930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
056900940114X   1C                   ELSE
057000000725     C                   MOVEL     D83MSG        $MSG
057100930721     C                   EXSR      CARMSG
057200940114E   1C                   ENDIF
057300930201     C*
057400930201     C                   ENDSR
057500940114     C*------------------------------------------------------------------------*
057600000725     C*  CAR3Q  - CARICAMENTO DATI DI 3Q
057700940114     C*------------------------------------------------------------------------*
057800000725     C     CAR3Q         BEGSR
057900940114     C*
058000000719     C* CARICO I DATI DI
058100940114     C*
058200020724     C                   Z-ADD     1             KTBKUT
058300000725     C                   MOVEL     '3Q'          KTBCOD
058400940114     C                   MOVEL     *BLANKS       KTBKEY
058500000719     C                   MOVEL     VCCCLI        KTBKEY
058600940114     C*
058700000719     C     KEYTAB        CHAIN     TABEL00F                           99
058800940114IF  1C     *IN99         IFEQ      *OFF
058900000725     C                   MOVEL     TBLUNI        DS3Q
059000940114X   1C                   ELSE
059100000725     C                   CLEAR                   DS3Q
059200940114E   1C                   ENDIF
059300940114     C*
059400000725     C* MUOVO I CAMPI DI '3Q' IN QUELLI DELLA DS DI PROCEDURA PER IL PASSAGGIO
059500000725     C                   MOVEL     §3QCSD        D83CSD
059600000725     C                   Z-ADD     §3QCKS        D83CKS
059700000725     C                   MOVEL     §3QCDM        D83CDM
059800000725     C                   MOVEL     §3QPRF        D83PRF
059900000725     C                   MOVEL     §3QNHL        D83NHL
060000000719     C                   EXSR      CHKPAR
060100940114     C*
060200940114     C                   ENDSR
060300940111     C*------------------------------------------------------------------------*
060400930713     C*  *INZSR - ROUTINE INIZIALE
060500940111     C*------------------------------------------------------------------------*
060600930713     C     *INZSR        BEGSR
060700000000     C*
060800000000     C* RICEVIMENTO PARAMETRI
060900000000     C     *ENTRY        PLIST
061000940105     C                   PARM                    KPJBA
061100000719     C                   MOVEL     KPJBU         VCCCLI
061200940111     C*
061300940111     C* DEFINISCE LE VARIABILI DI PROGRAMMA
061400000711     C     *LIKE         DEFINE    VCCCLI        DEPCLI
061500101119     C     *LIKE         DEFINE    VCCuni        DEPuni
061600101119     C     *LIKE         DEFINE    VCCsun        DEPsun
061700940111     C     *LIKE         DEFINE    TBLKUT        KTBKUT
061800940111     C     *LIKE         DEFINE    TBLCOD        KTBCOD
061900940111     C     *LIKE         DEFINE    TBLKEY        KTBKEY
062000000711     C     *LIKE         DEFINE    ACOKSC        KORCLI
062100930310     C*
062200930310     C* CHIAVE LETTURA TABEL00F - COMPLETA
062300940112     C     KEYTAB        KLIST
062400940111     C                   KFLD                    KTBKUT
062500940111     C                   KFLD                    KTBCOD
062600940111     C                   KFLD                    KTBKEY
062700940112     C*
062800940114     C* CHIAVE LETTURA TABEL00F - PARZIALE
062900940112     C     KEYTA2        KLIST
063000940112     C                   KFLD                    KTBKUT
063100940112     C                   KFLD                    KTBCOD
063200940114     C*
063300940114     C* CHIAVE LETTURA AZORG01L - COMPLETA
063400000711     C     KEYCLI        KLIST
063500000711     C                   KFLD                    KTBKUT
063600000711     C                   KFLD                    KORKCC            4 0
063700000711     C                   KFLD                    KORCLI
063800000711     C                   Z-ADD     151           KORKCC
063900930714     C*
064000930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
064100930803     C                   EXSR      INZSTD
064200930803     C*
064300930803     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
064400930803     C                   EXSR      INZSFL
064500020724     C                   MOVEL     RAGUT         VTDRSU
064600980130     C*
064700980130     C* CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
064800980130     C                   CLEAR                   UL12DS
064900980130     C                   MOVEL     'L'           I12TLA
065000000725     C                   MOVEL     '3Q'          I12COD
065100980130     C                   MOVEL     KNSIF         I12NSI
065200980130     C                   CALL      'TRUL12R'
065300980130     C                   PARM                    UL12DS
065400980130     C     O12ERR        IFEQ      '1'
065500980130     C                   MOVEL     '1'           $FINE
065600980130     C                   ENDIF
065700940104     C*
065800000719     C* CARICA I DATI
065900000725     C                   EXSR      CAR3Q
066000940114     C*
066100940104     C* EMETTO VIDEATA SUBFILE
066200940114     C                   EXSR      CARVS
066300940114     C*
066400000000     C                   ENDSR
066500940111     C*------------------------------------------------------------------------*
066600940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066700940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066800940111     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
066900940111     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
067000940111     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
067100940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067200940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067300940111     C*------------------------------------------------------------------------*
067400930803     C* INZSTD - ROUTINE INIZIALE STANDARD
067500940111     C*------------------------------------------------------------------------*
067600930803     C     INZSTD        BEGSR
067700930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
067800930802     C                   CALL      'XKEYS'
067900930802     C                   PARM                    DSKEYS
068000930805     C                   MOVEA     KEYRIG        KEYS
068100930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
068200930714     C     XMSGPA        PLIST
068300930714     C                   PARM                    DSMSMS
068400930714     C                   PARM                    DSMSPG
068500930714     C                   PARM                    DSMSKE
068600930714     C                   PARM                    DSMSER
068700930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
068800000725     C                   MOVEL     'TRTB83R'     DSMSPG
068900000725     C                   MOVEL     'TRTB83R'     VMCQUE
069000930806     C                   MOVEL     *BLANKS       VMCKEY
069100930714     C                   MOVEL     *BLANKS       DSMSMS
069200930805     C* INIZIALIZZO FLAG CONTROLLO PGM
069300930721     C                   MOVEL     '0'           $ERR              1
069400930721     C                   MOVEL     '0'           $F03              1
069500930806     C                   MOVEL     *BLANKS       $GES              2
069600930721     C                   MOVEL     *BLANKS       $MSG             80
069700930721     C                   MOVEL     '0'           $FINE             1
069800930721     C                   MOVEL     '0'           $TESVD            1
069900940112     C                   MOVEL     'G'           $TGES             1
070000020724     C*
070100020724     C                   CALL      'X§PARUT'
070200020724     C                   PARM                    UT§DSE
070300020724     C                   MOVEL     REC80         CNCR80
070400930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
070500930721     C                   BITOFF    '01234567'    POSMI1            1
070600930721     C                   BITOFF    '01234567'    POSMI2            1
070700930721     C                   BITOFF    '01234567'    POSMA1            1
070800930721     C                   BITOFF    '01234567'    POSMA2            1
070900930721     C                   BITON     '34'          POSMI1            1
071000930721     C                   BITON     '7'           POSMI2            1
071100930721     C                   BITON     '34'          POSMA1            1
071200930721     C                   BITON     '0'           POSMA2            1
071300930721     C                   MOVE      POSMI2        POSMIN            2
071400930721     C                   MOVEL     POSMI1        POSMIN
071500930721     C                   MOVE      POSMA2        POSMAX            2
071600930721     C                   MOVEL     POSMA1        POSMAX
071700930714     C                   ENDSR
071800940111     C*------------------------------------------------------------------------*
071900930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
072000940111     C*------------------------------------------------------------------------*
072100930804     C     FUNSTD        BEGSR
072200930804     C                   SELECT
072300930804     C     KEY           WHENEQ    F3
072400930804     C                   EXSR      F03STD
072500930804     C     KEY           WHENEQ    F12
072600930804     C                   EXSR      F12STD
072700930804     C     KEY           WHENEQ    F24
072800930804     C                   EXSR      F24STD
072900930804     C                   OTHER
073000930804     C                   MOVEL     MSG(2)        $MSG
073100930804     C                   EXSR      CARMSG
073200930804     C                   ENDSL
073300930804     C                   ENDSR
073400940111     C*------------------------------------------------------------------------*
073500930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
073600940111     C*------------------------------------------------------------------------*
073700930804     C     F03STD        BEGSR
073800930804     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
073900930923     C                   MOVEL     '1'           $F03
074000930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
074100930804     C                   MOVEL     '1'           $FINE
074200930804     C                   ENDSR
074300940111     C*------------------------------------------------------------------------*
074400930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
074500940111     C*------------------------------------------------------------------------*
074600930804     C     F12STD        BEGSR
074700930805     C* IMPOSTO FLAG DI FIN PGM
074800930804     C                   MOVEL     '1'           $FINE
074900930804     C                   ENDSR
075000940111     C*------------------------------------------------------------------------*
075100930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
075200940111     C*------------------------------------------------------------------------*
075300930804     C     F24STD        BEGSR
075400930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
075500930804     C                   EXSR      OUTFUN
075600930804     C                   ENDSR
075700940111     C*------------------------------------------------------------------------*
075800930804     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
075900940111     C*------------------------------------------------------------------------*
076000930804     C     INZSFL        BEGSR
076100930804     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
076200930806     C     *LIKE         DEFINE    VCCNBR        SFLPAG
076300930806     C     *LIKE         DEFINE    VCCNBR        SFLEOF
076400930806     C     *LIKE         DEFINE    VCCNBR        SFLERR
076500930806     C     *LIKE         DEFINE    VCCNBR        SFLULT
076600930806     C     *LIKE         DEFINE    VCCNBR        WFLULT
076700930806     C     *LIKE         DEFINE    VCCNBR        SFLRIG
076800930804     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
076900930804     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
077000101119     C                   Z-ADD     11            SFLPAG
077100930804     C                   ENDSR
077200940111     C*------------------------------------------------------------------------*
077300930804     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
077400940111     C*------------------------------------------------------------------------*
077500930804     C     FUNSFL        BEGSR
077600930804     C                   SELECT
077700930804     C     KEY           WHENEQ    ENTER
077800930804     C                   EXSR      ENTSFL
077900930805     C     KEY           WHENEQ    F3
078000930805     C                   EXSR      F03STD
078100930804     C     KEY           WHENEQ    F5
078200930804     C                   EXSR      F05STD
078300930805     C     KEY           WHENEQ    F12
078400930805     C                   EXSR      F12STD
078500930804     C     KEY           WHENEQ    F17
078600930804     C                   EXSR      F17STD
078700930804     C     KEY           WHENEQ    F18
078800930804     C                   EXSR      F18STD
078900930804     C     KEY           WHENEQ    F23
079000930804     C                   EXSR      F23STD
079100930805     C     KEY           WHENEQ    F24
079200930805     C                   EXSR      F24STD
079300930804     C     KEY           WHENEQ    ROLLUP
079400930804     C                   EXSR      RUPSTD
079500930804     C     KEY           WHENEQ    ROLLDN
079600930804     C                   EXSR      RDNSTD
079700930804     C                   OTHER
079800930804     C                   MOVEL     MSG(2)        $MSG
079900930804     C                   EXSR      CARMSG
080000930804     C                   ENDSL
080100930804     C                   ENDSR
080200940111     C*------------------------------------------------------------------------*
080300930804     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
080400940111     C*------------------------------------------------------------------------*
080500930804     C     F05STD        BEGSR
080600930804     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
080700930804     C                   EXSR      POSSFL
080800930804     C                   ENDSR
080900940111     C*------------------------------------------------------------------------*
081000930804     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
081100940111     C*------------------------------------------------------------------------*
081200930804     C     F17STD        BEGSR
081300930804     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
081400930806     C                   Z-ADD     1             VCCNBR
081500930804     C                   ENDSR
081600940111     C*------------------------------------------------------------------------*
081700930804     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
081800940111     C*------------------------------------------------------------------------*
081900930804     C     F18STD        BEGSR
082000930804     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
082100930806     C                   Z-ADD     SFLULT        VCCNBR
082200930804     C                   ENDSR
082300940111     C*------------------------------------------------------------------------*
082400930804     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
082500940111     C*------------------------------------------------------------------------*
082600930804     C     F23STD        BEGSR
082700930804     C* GESTIONE VISUALIZZAZIONE OPZIONI
082800930804     C                   EXSR      OUTOPZ
082900930804     C                   ENDSR
083000940111     C*------------------------------------------------------------------------*
083100930804     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
083200940111     C*------------------------------------------------------------------------*
083300930804     C     RUPSTD        BEGSR
083400930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
083500930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
083600930805     C     CURSOR        IFGE      POSMIN
083700930805     C     CURSOR        ANDLE     POSMAX
083800930804     C                   ELSE
083900930805     C                   EXSR      CARSFL
084000930805     C                   ENDIF
084100930804     C                   ENDSR
084200940111     C*------------------------------------------------------------------------*
084300930804     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
084400940111     C*------------------------------------------------------------------------*
084500930804     C     RDNSTD        BEGSR
084600930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
084700930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
084800930804     C     CURSOR        IFGE      POSMIN
084900930804     C     CURSOR        ANDLE     POSMAX
085000930804     C                   ELSE
085100930804     C* INVIO MESSAGGIO DI INIZIO ELENCO
085200930804     C                   MOVEL     '1'           $ERR
085300930804     C                   MOVEL     MSG(5)        $MSG
085400930804     C                   EXSR      CARMSG
085500930805     C                   ENDIF
085600930804     C                   ENDSR
085700940111     C*------------------------------------------------------------------------*
085800930804     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
085900940111     C*------------------------------------------------------------------------*
086000930804     C     ENTSFL        BEGSR
086100930804     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
086200930804     C                   EXSR      NEWPOS
086300930804     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
086400930804     C     $ERR          IFEQ      '0'
086500930804     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
086600930804     C     SWPOSI        IFNE      0
086700930804     C                   EXSR      POSSFL
086800930804     C                   ELSE
086900930804     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
087000930804     C     SFLULT        IFNE      0
087100930804     C                   EXSR      OPZSFL
087200930804     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
087300930804     C     SWMODI        IFNE      0
087400930804     C                   EXSR      POSSFL
087500930805     C                   ENDIF
087600930805     C                   ENDIF
087700930805     C                   ENDIF
087800930805     C                   ENDIF
087900930804     C                   ENDSR
088000940111     C*------------------------------------------------------------------------*
088100940111     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
088200940111     C*------------------------------------------------------------------------*
088300930804     C     POSSFL        BEGSR
088400930804     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
088500930804     C                   EXSR      CLRSFL
088600930804     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
088700000711     C                   EXSR      SETCLI
088800930804     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
088900930804     C     SFLEOF        IFEQ      0
089000930804     C                   EXSR      CARSFL
089100930805     C                   ENDIF
089200930804     C                   ENDSR
089300940111     C*------------------------------------------------------------------------*
089400930804     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
089500940111     C*------------------------------------------------------------------------*
089600930804     C     OPZSFL        BEGSR
089700930802     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
089800930802     C                   MOVEL     'S'           OPZOK             1
089900000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
090000000000     C                   Z-ADD     0             SFLVAR            5 0
090100000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
090200000000     C                   Z-ADD     0             SFLERR
090300000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
090400000000     C                   Z-ADD     SFLULT        WFLULT
090500000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
090600000725     C                   READC     TB83DVS                                49
090700930806     C     *IN49         DOWEQ     '0'
090800000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
090900000000     C                   ADD       1             SFLVAR
091000930802     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
091100930805     C     OPZOK         IFEQ      'S'
091200930802     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
091300930806     C     VSCOPZ        IFNE      *BLANKS
091400930802     C                   EXSR      CONOPZ
091500930802     C     OPZOK         IFEQ      'S'
091600930802     C                   EXSR      GESOPZ
091700930805     C                   ENDIF
091800930805     C                   ENDIF
091900000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
092000000000     C*            2. PULIZIA SCELTA IN SFL
092100930805     C     OPZOK         IFEQ      'S'
092200000000     C                   MOVEL     '0'           *IN41
092300930806     C                   MOVEL     *BLANKS       VSCOPZ
092400930805     C                   ELSE
092500000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
092600000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
092700000000     C                   MOVEL     '1'           *IN41
092800000000     C                   Z-ADD     SFLULT        SFLERR
092900930805     C                   ENDIF
093000930805     C                   ELSE
093100000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
093200000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
093300000000     C                   MOVEL     '1'           *IN41
093400930805     C                   ENDIF
093500000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
093600000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
093700000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
093800930803     C     OPZOK         IFNE      'S'
093900930806     C                   Z-ADD     SFLERR        VCCNBR
094000000000     C                   ELSE
094100930806     C                   Z-ADD     SFLULT        VCCNBR
094200930805     C                   ENDIF
094300000000     C* AGGIORNO RECORD SFL
094400000725     C                   UPDATE    TB83DVS
094500000725     C                   READC     TB83DVS                                49
094600930805     C                   ENDDO
094700000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
094800000000     C                   Z-ADD     WFLULT        SFLULT
094900000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
095000000719     C*          SFLVAR    IFEQ 0
095100000719     C*                    MOVEL'1'       $FINE
095200000719     C*                    ENDIF
095300000000     C                   ENDSR
095400940111     C*------------------------------------------------------------------------*
095500930804     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
095600940111     C*------------------------------------------------------------------------*
095700000000     C     CLRSFL        BEGSR
095800000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
095900000000     C* (0=NON FINE FILE  1=FINE FILE)
096000000000     C                   Z-ADD     0             SFLEOF
096100000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
096200000000     C                   Z-ADD     0             SFLULT
096300000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
096400000000     C                   MOVEL     '1'           *IN40
096500000725     C                   WRITE     TB83DVC
096600000000     C                   MOVEL     '0'           *IN40
096700000000     C                   ENDSR
096800940111     C*------------------------------------------------------------------------*
096900000000     C* CARSFL - CARICAMENTO SFL
097000940111     C*------------------------------------------------------------------------*
097100000000     C     CARSFL        BEGSR
097200000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
097300000000     C     SFLULT        IFEQ      0
097400000711     C                   EXSR      SETCLI
097500930805     C                   ENDIF
097600000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
097700000000     C                   Z-ADD     SFLULT        WFLULT
097800000000     C* CICLO:
097900000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
098000000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
098100000000     C                   Z-ADD     1             SFLRIG
098200930805     C     SFLRIG        DOWLE     SFLPAG
098300000000     C     SFLEOF        ANDEQ     0
098400000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
098500930806     C                   MOVEL     *BLANKS       VSCOPZ
098600000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
098700000000     C                   EXSR      IRESFL
098800000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
098900000000     C                   ADD       1             SFLULT
099000000000     C* SCRITTURA RECORD SFL
099100000725     C                   WRITE     TB83DVS
099200000000     C                   ADD       1             SFLRIG
099300000000     C* LETTURA FILE DA CARICARE
099400000711     C                   EXSR      LETCLI
099500930805     C                   ENDDO
099600000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
099700000000     C     WFLULT        IFNE      SFLULT
099800930806     C                   Z-ADD     WFLULT        VCCNBR
099900930806     C                   ADD       1             VCCNBR
100000930805     C                   ENDIF
100100930720     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
100200930805     C     SFLEOF        IFEQ      1
100300930721     C     SFLULT        ANDGT     *ZEROS
100400930721     C                   MOVEL     '1'           $ERR
100500930804     C                   MOVEL     MSG(6)        $MSG
100600930721     C                   EXSR      CARMSG
100700930804     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
100800000725     C     SFLULT        CHAIN     TB83DVS                            99
100900930804     C     *IN99         IFEQ      '0'
101000930804     C                   SETON                                        45
101100000725     C                   UPDATE    TB83DVS
101200930804     C                   SETOFF                                       45
101300930805     C                   ENDIF
101400930805     C                   ENDIF
101500000000     C                   ENDSR
101600940111     C*------------------------------------------------------------------------*
101700930804     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
101800940111     C*------------------------------------------------------------------------*
101900000000     C     OUTSFL        BEGSR
102000930805     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
102100930721     C     MSGULT        IFNE      *ZEROS
102200930721     C                   EXSR      OUTMSG
102300930805     C                   ENDIF
102400000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
102500000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
102600000000     C     SFLEOF        IFEQ      0
102700000000     C                   MOVEL     '0'           *IN42
102800000000     C                   ELSE
102900000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
103000000000     C                   MOVEL     '1'           *IN42
103100930805     C                   ENDIF
103200930720     C* EMISSIONE SUBFILE
103300000000     C     SFLULT        IFLE      0
103400930720     C*  --> CASO DI SFL VUOTO
103500930720     C                   MOVEL     '0'           *IN43
103600000725     C                   WRITE     TB83DVV
103700000725     C                   WRITE     TB83DVC
103800000725     C                   READ      TB83DVC                                99
103900000000     C                   ELSE
104000930720     C*  --> CASO DI SFL CON RECORDS
104100000725     C                   WRITE     TB83DVC
104200000725     C                   READ      TB83DVC                                99
104300930805     C                   ENDIF
104400930720     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
104500930720     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
104600930806     C                   Z-ADD     FRTSFL        VCCNBR
104700000000     C                   ENDSR
104800940111     C*------------------------------------------------------------------------*
104900930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
105000930721     C*          CODA MESSAGGI DEL PGM
105100940111     C*------------------------------------------------------------------------*
105200930721     C     CARMSG        BEGSR
105300930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
105400930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
105500930721     C     MSGULT        IFEQ      0
105600930721     C                   MOVEL     '1'           *IN40
105700000725     C                   WRITE     TB83DVK
105800930721     C                   MOVEL     '0'           *IN40
105900930805     C                   ENDIF
106000930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
106100930721     C                   MOVEL     $MSG          DSMSMS
106200930721     C                   CALL      'XMSG'        XMSGPA
106300930721     C     DSMSER        IFEQ      0
106400930806     C                   MOVEL     DSMSKE        VMCKEY
106500930805     C                   ENDIF
106600930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
106700931015     C                   ADD       1             MSGULT            4 0
106800000725     C                   WRITE     TB83DVM
106900930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
107000930721     C                   MOVEL     *BLANKS       $MSG
107100930721     C                   ENDSR
107200940111     C*------------------------------------------------------------------------*
107300930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
107400940111     C*------------------------------------------------------------------------*
107500930721     C     OUTMSG        BEGSR
107600930721     C* VISUALIZZAZIONE MESSAGGIO
107700930721     C                   MOVEL     '1'           *IN44
107800000725     C                   WRITE     TB83DVK
107900930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
108000930721     C                   Z-ADD     *ZEROS        MSGULT
108100930721     C                   ENDSR
108200940111     C*------------------------------------------------------------------------*
108300930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
108400930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
108500930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
108600930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
108700930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
108800940111     C*------------------------------------------------------------------------*
108900930803     C     CARFUN        BEGSR
109000930803     C                   Z-ADD     *ZEROS        FUNULT            5 0
109100931015     C     1             DO        31            I                 5 0
109200930803     C                   MOVEL     FUN(I)        FUNRIG
109300930805     C                   MOVEL     FUNFUN        FFU(I)
109400930805     C                   MOVEL     FUNDES        FDE(I)
109500020724     C     SIMTPP        IFEQ      'S'
109600930923     C     $TGES         IFEQ      'G'
109700930805     C                   MOVEL     FUNASG        FAT(I)
109800930803     C                   ELSE
109900930803     C                   MOVEL     FUNASI        FAT(I)
110000930803     C                   ENDIF
110100930803     C                   ELSE
110200930923     C     $TGES         IFEQ      'G'
110300930803     C                   MOVEL     FUNAFG        FAT(I)
110400930803     C                   ELSE
110500930803     C                   MOVEL     FUNAFI        FAT(I)
110600930803     C                   ENDIF
110700930803     C                   ENDIF
110800930806     C     FAT(I)        IFEQ      'E'
110900930806     C                   MOVEL     USER          WEDP              3
111000930806     C     WEDP          IFEQ      'EDP'
111100930806     C                   MOVEL     'S'           FAT(I)
111200930806     C                   ELSE
111300930806     C                   MOVEL     'N'           FAT(I)
111400930806     C                   ENDIF
111500930806     C                   ENDIF
111600930803     C                   ENDDO
111700930803     C                   ENDSR
111800940111     C*------------------------------------------------------------------------*
111900930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
112000930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
112100930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
112200930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
112300940111     C*------------------------------------------------------------------------*
112400930803     C     OUTFUN        BEGSR
112500930805     C     1             DO        8             I
112600930805     C                   MOVEL     *BLANKS       FUV(I)
112700930803     C                   ENDDO
112800930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
112900930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0
113000930803     C     1             DO        31            I
113100930803     C     FAT(I)        IFEQ      'S'
113200930803     C     FDE(I)        ANDNE     *BLANKS
113300930803     C                   ADD       1             FUNNAT
113400930803     C                   ENDIF
113500930803     C                   ENDDO
113600930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
113700930803     C     FUNNAT        IFGT      8
113800930803     C                   MOVEL     'S'           FAT(24)
113900930803     C                   ENDIF
114000930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
114100930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
114200930803     C                   Z-ADD     8             FUNVIS            5 0
114300930803     C     FAT(23)       IFEQ      'S'
114400930803     C                   SUB       1             FUNVIS
114500930803     C                   ENDIF
114600930803     C     FAT(24)       IFEQ      'S'
114700930803     C                   SUB       1             FUNVIS
114800930803     C                   ENDIF
114900930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
115000931015     C                   Z-ADD     *ZEROS        W                 5 0
115100930803     C                   ADD       1             FUNULT
115200930803     C     FUNULT        IFGT      31
115300930803     C                   Z-ADD     1             FUNULT
115400930805     C                   ENDIF
115500930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
115600930803     C     FUNULT        DO        31            I
115700930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
115800930803     C     W             IFLT      FUNVIS
115900930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
116000930803     C     FAT(I)        IFEQ      'S'
116100930803     C     FDE(I)        ANDNE     *BLANKS
116200930805     C     I             IFEQ      23
116300930805     C     I             OREQ      24
116400930803     C                   ELSE
116500930803     C                   ADD       1             W
116600930803     C                   MOVEL     FDE(I)        FUV(W)
116700930803     C                   ENDIF
116800930803     C                   ENDIF
116900930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
117000930803     C                   Z-ADD     I             FUNULT
117100930803     C                   ENDIF
117200930803     C                   ENDDO
117300930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
117400930806     C                   MOVEL     FUV(1)        VFD001
117500930806     C                   MOVEL     FUV(2)        VFD002
117600930806     C                   MOVEL     FUV(3)        VFD003
117700930806     C                   MOVEL     FUV(4)        VFD004
117800930806     C                   MOVEL     FUV(5)        VFD005
117900930806     C                   MOVEL     FUV(6)        VFD006
118000930803     C     FAT(23)       IFEQ      'N'
118100930803     C     FAT(24)       ANDEQ     'N'
118200930806     C                   MOVEL     FUV(7)        VFD007
118300930806     C                   MOVEL     FUV(8)        VFD008
118400930803     C                   ELSE
118500930803     C     FAT(23)       IFEQ      'S'
118600930803     C     FAT(24)       ANDEQ     'S'
118700930806     C                   MOVEL     FDE(23)       VFD007
118800930806     C                   MOVEL     FDE(24)       VFD008
118900930803     C                   ELSE
119000930806     C                   MOVEL     FUV(7)        VFD007
119100930803     C     FAT(23)       IFEQ      'S'
119200930806     C                   MOVEL     FDE(23)       VFD008
119300930803     C                   ELSE
119400930806     C                   MOVEL     FDE(24)       VFD008
119500930805     C                   ENDIF
119600930805     C                   ENDIF
119700930805     C                   ENDIF
119800930803     C                   MOVEL     '1'           $TESVD
119900930803     C                   ENDSR
120000940111     C*------------------------------------------------------------------------*
120100930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
120200930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
120300930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
120400930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
120500940111     C*------------------------------------------------------------------------*
120600930803     C     CONFUN        BEGSR
120700930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
120800930803     C     1             DO        31            I
120900930803     C     KEY           IFEQ      KEYS(I)
121000930805     C* CONTROLLO SE IL TASTO E' ATTIVO
121100930803     C     FAT(I)        IFNE      'S'
121200930804     C                   MOVEL     MSG(1)        $MSG
121300930803     C                   EXSR      CARMSG
121400930803     C                   MOVEL     '1'           $ERR
121500930803     C                   ENDIF
121600930803     C                   ENDIF
121700930803     C                   ENDDO
121800930803     C                   ENDSR
121900940111     C*------------------------------------------------------------------------*
122000930802     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
122100930802     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
122200930803     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
122300930803     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
122400930803     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
122500930803     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
122600940111     C*------------------------------------------------------------------------*
122700930802     C     CAROPZ        BEGSR
122800930803     C                   Z-ADD     *ZEROS        OPZULT            5 0
122900930802     C     1             DO        24            I
123000930803     C                   MOVEL     OPZ(I)        OPZRIG
123100930805     C                   MOVEL     OPZOPD        OPD(I)
123200930805     C                   MOVEL     OPZOPS        OPS(I)
123300930805     C                   MOVEL     OPZDES        ODE(I)
123400020724     C     SIMTPP        IFEQ      'S'
123500930923     C     $TGES         IFEQ      'G'
123600930805     C                   MOVEL     OPZASG        OAT(I)
123700930802     C                   ELSE
123800930805     C                   MOVEL     OPZASI        OAT(I)
123900930802     C                   ENDIF
124000930802     C                   ELSE
124100930923     C     $TGES         IFEQ      'G'
124200930805     C                   MOVEL     OPZAFG        OAT(I)
124300930802     C                   ELSE
124400930805     C                   MOVEL     OPZAFI        OAT(I)
124500930802     C                   ENDIF
124600930802     C                   ENDIF
124700930806     C     OAT(I)        IFEQ      'E'
124800930806     C                   MOVEL     USER          WEDP              3
124900930806     C     WEDP          IFEQ      'EDP'
125000930806     C                   MOVEL     'S'           OAT(I)
125100930806     C                   ELSE
125200930806     C                   MOVEL     'N'           OAT(I)
125300930806     C                   ENDIF
125400930806     C                   ENDIF
125500930802     C                   ENDDO
125600930802     C                   ENDSR
125700940111     C*------------------------------------------------------------------------*
125800930802     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
125900930802     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
126000930802     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
126100930806     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
126200930802     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
126300940111     C*------------------------------------------------------------------------*
126400930802     C     OUTOPZ        BEGSR
126500930805     C     1             DO        8             I
126600930805     C                   MOVEL     *BLANKS       OPV(I)
126700930802     C                   ENDDO
126800930802     C                   Z-ADD     *ZEROS        OPZNAT            2 0
126900930805     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
127000930803     C                   ADD       1             OPZULT
127100930803     C     OPZULT        IFGT      24
127200930803     C                   Z-ADD     1             OPZULT
127300930803     C                   END
127400930805     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
127500930802     C     1             DO        24            I
127600930805     C     OAT(I)        IFEQ      'S'
127700930805     C     ODE(I)        ANDNE     *BLANKS
127800930802     C                   ADD       1             OPZNAT
127900930802     C                   ENDIF
128000930802     C                   ENDDO
128100930802     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
128200931015     C                   Z-ADD     0             W
128300930802     C     OPZULT        DO        24            I
128400930802     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
128500930802     C     W             IFLT      8
128600930802     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
128700930805     C     OAT(I)        IFEQ      'S'
128800930803     C     ODE(I)        ANDNE     *BLANKS
128900930802     C                   ADD       1             W
129000930803     C                   MOVEL     ODE(I)        OPV(W)
129100930805     C                   ENDIF
129200930805     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
129300930802     C                   Z-ADD     I             OPZULT
129400930802     C                   ENDIF
129500930802     C                   ENDDO
129600930802     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
129700930806     C                   MOVEL     OPV(1)        VOD001
129800930806     C                   MOVEL     OPV(2)        VOD002
129900930806     C                   MOVEL     OPV(3)        VOD003
130000930806     C                   MOVEL     OPV(4)        VOD004
130100930806     C                   MOVEL     OPV(5)        VOD005
130200930806     C                   MOVEL     OPV(6)        VOD006
130300930806     C                   MOVEL     OPV(7)        VOD007
130400930806     C                   MOVEL     OPV(8)        VOD008
130500930802     C                   MOVEL     '1'           $TESVD
130600930923     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
130700930923     C     OPZNAT        IFGT      8
130800930923     C                   MOVEL     'S'           FAT(23)
130900930923     C                   ENDIF
131000930802     C                   ENDSR
131100940111     C*------------------------------------------------------------------------*
131200930802     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
131300930806     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
131400930802     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
131500930802     C*                   - ERRORE SE OPZIONE NON ATTIVA
131600940111     C*------------------------------------------------------------------------*
131700930802     C     CONOPZ        BEGSR
131800000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
131900930803     C                   MOVEL     *BLANKS       OPZOK             1
132000930805     C     1             DO        24            I                 5 0
132100930806     C     VSCOPZ        IFEQ      OPD(I)
132200930806     C     VSCOPZ        OREQ      OPS(I)
132300930805     C     OAT(I)        IFEQ      'S'
132400930806     C                   MOVEL     OPD(I)        VSCOPZ
132500940105     C                   MOVEL     'S'           OPZOK
132600930805     C                   ELSE
132700930802     C                   MOVEL     'N'           OPZOK             1
132800930805     C                   ENDIF
132900930802     C                   ENDIF
133000930802     C                   ENDDO
133100930805     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
133200930803     C     OPZOK         IFNE      'S'
133300930804     C                   MOVEL     MSG(3)        $MSG
133400930803     C                   EXSR      CARMSG
133500930803     C                   MOVEL     '1'           $ERR
133600930803     C                   MOVEL     'N'           OPZOK             1
133700930803     C                   ENDIF
133800000000     C                   ENDSR
133900930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
134000000725TRTB83R- Tasto funzionale non valido.                                          1
134100000725TRTB83R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
134200000725TRTB83R- Opzione non valida.                                                   3
134300000725TRTB83R- Opzione abilitata ma non gestita. Avvertire CED.                      4
134400000725TRTB83R- Inizio elenco.                                                        5
134500000725TRTB83R- Fine elenco.                                                          6
134600000725TRTB83R- 7                                                                     7
134700000725TRTB83R- 8                                                                     8
134800000725TRTB83R- 9                                                                     9
134900000725TRTB83R- 10                                                                   10
135000000725TRTB83R- 11                                                                   11
135100000725TRTB83R- 12                                                                   12
135200000725TRTB83R- 13                                                                   13
135300000725TRTB83R- 14                                                                   14
135400000725TRTB83R- 15                                                                   15
135500000725TRTB83R- 16                                                                   16
135600000725TRTB83R- 17                                                                   17
135700000725TRTB83R- 18                                                                   18
135800000725TRTB83R- 19                                                                   19
135900000725TRTB83R- 20                                                                   20
136000930729** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
136100930803- 1-1 -                   -    NN     NN
136200940112- 2-2 -2=Modifica         -    SS     SS
136300000711- 3-3 -3=COPIA            -    SS     SS
136400940112- 4-4 -4=Cancellazione    -    SS     SS
136500940121- 5-5 -5=Visualizzazione  -    SS     SS
136600930803- 6-6 -                   -    NN     NN
136700940112- 7-7 -7=Ripristino       -    SS     SS
136800940104- 8-8 -                   -    NN     NN
136900940111- 9-9 -                   -    NN     NN
137000930806-10-  -                   -    NN     NN
137100930806-11-  -                   -    NN     NN
137200930806-12-  -                   -    NN     NN
137300930803-13-  -                   -    NN     NN
137400930803-14-  -                   -    NN     NN
137500930803-15-  -                   -    NN     NN
137600930803-16-  -                   -    NN     NN
137700930803-17-  -                   -    NN     NN
137800930803-18-  -                   -    NN     NN
137900930803-19-  -                   -    NN     NN
138000930803-20-  -                   -    NN     NN
138100930803-21-  -                   -    NN     NN
138200930803-22-  -                   -    NN     NN
138300930803-23-  -                   -    NN     NN
138400930803-24-  -                   -    NN     NN
138500930730** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
138600930723F1    -                   -    NN     NN
138700930723F2    -                   -    NN     NN
138800930802F3    -F3=Fine            -    SS     SS
138900930723F4    -                   -    NN     NN
139000940112F5    -F5=Rivisualizzaz.  -    SS     SS
139100940112F6    -F6=Inserimento     -    SS     SS
139200930723F7    -                   -    NN     NN
139300930723F8    -                   -    NN     NN
139400930723F9    -                   -    NN     NN
139500930723F10   -                   -    NN     NN
139600930723F11   -                   -    NN     NN
139700930802F12   -F12=Ritorno        -    SS     SS
139800930806F13   -                   -    NN     NN
139900930806F14   -                   -    NN     NN
140000930723F15   -                   -    NN     NN
140100930723F16   -                   -    NN     NN
140200930802F17   -F17=Inizio dati    -    SS     SS
140300930802F18   -F18=Fine dati      -    SS     SS
140400930723F19   -                   -    NN     NN
140500930723F20   -                   -    NN     NN
140600940111F21   -                   -    NN     NN
140700940111F22   -                   -    NN     NN
140800930803F23   -F23=Altre opzioni  -    NN     NN
140900930803F24   -F24=Altri tasti    -    NN     NN
141000930802ENTER -                   -    SS     SS
141100930802ROLLUP-                   -    SS     SS
141200930802ROLLDN-                   -    SS     SS
141300930802HELP  -                   -    NN     NN
141400930802PRINT -                   -    NN     NN
141500930802BSPACE-                   -    NN     NN
141600930802CLEAR -                   -    NN     NN
