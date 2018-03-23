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
061200120808     C                   MOVEL     VCCCLI        DEPCLI
061300120808     C                   Z-ADD     VCCuni        DEPuni
061400120808     C                   movel     VCCsun        DEPsun
061500940111     C*
061600940111     C* DEFINISCE LE VARIABILI DI PROGRAMMA
061700000711     C     *LIKE         DEFINE    VCCCLI        DEPCLI
061800101119     C     *LIKE         DEFINE    VCCuni        DEPuni
061900101119     C     *LIKE         DEFINE    VCCsun        DEPsun
062000940111     C     *LIKE         DEFINE    TBLKUT        KTBKUT
062100940111     C     *LIKE         DEFINE    TBLCOD        KTBCOD
062200940111     C     *LIKE         DEFINE    TBLKEY        KTBKEY
062300000711     C     *LIKE         DEFINE    ACOKSC        KORCLI
062400930310     C*
062500930310     C* CHIAVE LETTURA TABEL00F - COMPLETA
062600940112     C     KEYTAB        KLIST
062700940111     C                   KFLD                    KTBKUT
062800940111     C                   KFLD                    KTBCOD
062900940111     C                   KFLD                    KTBKEY
063000940112     C*
063100940114     C* CHIAVE LETTURA TABEL00F - PARZIALE
063200940112     C     KEYTA2        KLIST
063300940112     C                   KFLD                    KTBKUT
063400940112     C                   KFLD                    KTBCOD
063500940114     C*
063600940114     C* CHIAVE LETTURA AZORG01L - COMPLETA
063700000711     C     KEYCLI        KLIST
063800000711     C                   KFLD                    KTBKUT
063900000711     C                   KFLD                    KORKCC            4 0
064000000711     C                   KFLD                    KORCLI
064100000711     C                   Z-ADD     151           KORKCC
064200930714     C*
064300930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
064400930803     C                   EXSR      INZSTD
064500930803     C*
064600930803     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
064700930803     C                   EXSR      INZSFL
064800020724     C                   MOVEL     RAGUT         VTDRSU
064900980130     C*
065000980130     C* CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
065100980130     C                   CLEAR                   UL12DS
065200980130     C                   MOVEL     'L'           I12TLA
065300000725     C                   MOVEL     '3Q'          I12COD
065400980130     C                   MOVEL     KNSIF         I12NSI
065500980130     C                   CALL      'TRUL12R'
065600980130     C                   PARM                    UL12DS
065700980130     C     O12ERR        IFEQ      '1'
065800980130     C                   MOVEL     '1'           $FINE
065900980130     C                   ENDIF
066000940104     C*
066100000719     C* CARICA I DATI
066200000725     C                   EXSR      CAR3Q
066300940114     C*
066400940104     C* EMETTO VIDEATA SUBFILE
066500940114     C                   EXSR      CARVS
066600940114     C*
066700000000     C                   ENDSR
066800940111     C*------------------------------------------------------------------------*
066900940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067000940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067100940111     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
067200940111     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
067300940111     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
067400940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067500940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067600940111     C*------------------------------------------------------------------------*
067700930803     C* INZSTD - ROUTINE INIZIALE STANDARD
067800940111     C*------------------------------------------------------------------------*
067900930803     C     INZSTD        BEGSR
068000930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
068100930802     C                   CALL      'XKEYS'
068200930802     C                   PARM                    DSKEYS
068300930805     C                   MOVEA     KEYRIG        KEYS
068400930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
068500930714     C     XMSGPA        PLIST
068600930714     C                   PARM                    DSMSMS
068700930714     C                   PARM                    DSMSPG
068800930714     C                   PARM                    DSMSKE
068900930714     C                   PARM                    DSMSER
069000930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
069100000725     C                   MOVEL     'TRTB83R'     DSMSPG
069200000725     C                   MOVEL     'TRTB83R'     VMCQUE
069300930806     C                   MOVEL     *BLANKS       VMCKEY
069400930714     C                   MOVEL     *BLANKS       DSMSMS
069500930805     C* INIZIALIZZO FLAG CONTROLLO PGM
069600930721     C                   MOVEL     '0'           $ERR              1
069700930721     C                   MOVEL     '0'           $F03              1
069800930806     C                   MOVEL     *BLANKS       $GES              2
069900930721     C                   MOVEL     *BLANKS       $MSG             80
070000930721     C                   MOVEL     '0'           $FINE             1
070100930721     C                   MOVEL     '0'           $TESVD            1
070200940112     C                   MOVEL     'G'           $TGES             1
070300020724     C*
070400020724     C                   CALL      'X§PARUT'
070500020724     C                   PARM                    UT§DSE
070600020724     C                   MOVEL     REC80         CNCR80
070700930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
070800930721     C                   BITOFF    '01234567'    POSMI1            1
070900930721     C                   BITOFF    '01234567'    POSMI2            1
071000930721     C                   BITOFF    '01234567'    POSMA1            1
071100930721     C                   BITOFF    '01234567'    POSMA2            1
071200930721     C                   BITON     '34'          POSMI1            1
071300930721     C                   BITON     '7'           POSMI2            1
071400930721     C                   BITON     '34'          POSMA1            1
071500930721     C                   BITON     '0'           POSMA2            1
071600930721     C                   MOVE      POSMI2        POSMIN            2
071700930721     C                   MOVEL     POSMI1        POSMIN
071800930721     C                   MOVE      POSMA2        POSMAX            2
071900930721     C                   MOVEL     POSMA1        POSMAX
072000930714     C                   ENDSR
072100940111     C*------------------------------------------------------------------------*
072200930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
072300940111     C*------------------------------------------------------------------------*
072400930804     C     FUNSTD        BEGSR
072500930804     C                   SELECT
072600930804     C     KEY           WHENEQ    F3
072700930804     C                   EXSR      F03STD
072800930804     C     KEY           WHENEQ    F12
072900930804     C                   EXSR      F12STD
073000930804     C     KEY           WHENEQ    F24
073100930804     C                   EXSR      F24STD
073200930804     C                   OTHER
073300930804     C                   MOVEL     MSG(2)        $MSG
073400930804     C                   EXSR      CARMSG
073500930804     C                   ENDSL
073600930804     C                   ENDSR
073700940111     C*------------------------------------------------------------------------*
073800930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
073900940111     C*------------------------------------------------------------------------*
074000930804     C     F03STD        BEGSR
074100930804     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
074200930923     C                   MOVEL     '1'           $F03
074300930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
074400930804     C                   MOVEL     '1'           $FINE
074500930804     C                   ENDSR
074600940111     C*------------------------------------------------------------------------*
074700930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
074800940111     C*------------------------------------------------------------------------*
074900930804     C     F12STD        BEGSR
075000930805     C* IMPOSTO FLAG DI FIN PGM
075100930804     C                   MOVEL     '1'           $FINE
075200930804     C                   ENDSR
075300940111     C*------------------------------------------------------------------------*
075400930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
075500940111     C*------------------------------------------------------------------------*
075600930804     C     F24STD        BEGSR
075700930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
075800930804     C                   EXSR      OUTFUN
075900930804     C                   ENDSR
076000940111     C*------------------------------------------------------------------------*
076100930804     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
076200940111     C*------------------------------------------------------------------------*
076300930804     C     INZSFL        BEGSR
076400930804     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
076500930806     C     *LIKE         DEFINE    VCCNBR        SFLPAG
076600930806     C     *LIKE         DEFINE    VCCNBR        SFLEOF
076700930806     C     *LIKE         DEFINE    VCCNBR        SFLERR
076800930806     C     *LIKE         DEFINE    VCCNBR        SFLULT
076900930806     C     *LIKE         DEFINE    VCCNBR        WFLULT
077000930806     C     *LIKE         DEFINE    VCCNBR        SFLRIG
077100930804     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
077200930804     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
077300101119     C                   Z-ADD     11            SFLPAG
077400930804     C                   ENDSR
077500940111     C*------------------------------------------------------------------------*
077600930804     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
077700940111     C*------------------------------------------------------------------------*
077800930804     C     FUNSFL        BEGSR
077900930804     C                   SELECT
078000930804     C     KEY           WHENEQ    ENTER
078100930804     C                   EXSR      ENTSFL
078200930805     C     KEY           WHENEQ    F3
078300930805     C                   EXSR      F03STD
078400930804     C     KEY           WHENEQ    F5
078500930804     C                   EXSR      F05STD
078600930805     C     KEY           WHENEQ    F12
078700930805     C                   EXSR      F12STD
078800930804     C     KEY           WHENEQ    F17
078900930804     C                   EXSR      F17STD
079000930804     C     KEY           WHENEQ    F18
079100930804     C                   EXSR      F18STD
079200930804     C     KEY           WHENEQ    F23
079300930804     C                   EXSR      F23STD
079400930805     C     KEY           WHENEQ    F24
079500930805     C                   EXSR      F24STD
079600930804     C     KEY           WHENEQ    ROLLUP
079700930804     C                   EXSR      RUPSTD
079800930804     C     KEY           WHENEQ    ROLLDN
079900930804     C                   EXSR      RDNSTD
080000930804     C                   OTHER
080100930804     C                   MOVEL     MSG(2)        $MSG
080200930804     C                   EXSR      CARMSG
080300930804     C                   ENDSL
080400930804     C                   ENDSR
080500940111     C*------------------------------------------------------------------------*
080600930804     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
080700940111     C*------------------------------------------------------------------------*
080800930804     C     F05STD        BEGSR
080900930804     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
081000930804     C                   EXSR      POSSFL
081100930804     C                   ENDSR
081200940111     C*------------------------------------------------------------------------*
081300930804     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
081400940111     C*------------------------------------------------------------------------*
081500930804     C     F17STD        BEGSR
081600930804     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
081700930806     C                   Z-ADD     1             VCCNBR
081800930804     C                   ENDSR
081900940111     C*------------------------------------------------------------------------*
082000930804     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
082100940111     C*------------------------------------------------------------------------*
082200930804     C     F18STD        BEGSR
082300930804     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
082400930806     C                   Z-ADD     SFLULT        VCCNBR
082500930804     C                   ENDSR
082600940111     C*------------------------------------------------------------------------*
082700930804     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
082800940111     C*------------------------------------------------------------------------*
082900930804     C     F23STD        BEGSR
083000930804     C* GESTIONE VISUALIZZAZIONE OPZIONI
083100930804     C                   EXSR      OUTOPZ
083200930804     C                   ENDSR
083300940111     C*------------------------------------------------------------------------*
083400930804     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
083500940111     C*------------------------------------------------------------------------*
083600930804     C     RUPSTD        BEGSR
083700930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
083800930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
083900930805     C     CURSOR        IFGE      POSMIN
084000930805     C     CURSOR        ANDLE     POSMAX
084100930804     C                   ELSE
084200930805     C                   EXSR      CARSFL
084300930805     C                   ENDIF
084400930804     C                   ENDSR
084500940111     C*------------------------------------------------------------------------*
084600930804     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
084700940111     C*------------------------------------------------------------------------*
084800930804     C     RDNSTD        BEGSR
084900930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
085000930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
085100930804     C     CURSOR        IFGE      POSMIN
085200930804     C     CURSOR        ANDLE     POSMAX
085300930804     C                   ELSE
085400930804     C* INVIO MESSAGGIO DI INIZIO ELENCO
085500930804     C                   MOVEL     '1'           $ERR
085600930804     C                   MOVEL     MSG(5)        $MSG
085700930804     C                   EXSR      CARMSG
085800930805     C                   ENDIF
085900930804     C                   ENDSR
086000940111     C*------------------------------------------------------------------------*
086100930804     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
086200940111     C*------------------------------------------------------------------------*
086300930804     C     ENTSFL        BEGSR
086400930804     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
086500930804     C                   EXSR      NEWPOS
086600930804     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
086700930804     C     $ERR          IFEQ      '0'
086800930804     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
086900930804     C     SWPOSI        IFNE      0
087000930804     C                   EXSR      POSSFL
087100930804     C                   ELSE
087200930804     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
087300930804     C     SFLULT        IFNE      0
087400930804     C                   EXSR      OPZSFL
087500930804     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
087600930804     C     SWMODI        IFNE      0
087700930804     C                   EXSR      POSSFL
087800930805     C                   ENDIF
087900930805     C                   ENDIF
088000930805     C                   ENDIF
088100930805     C                   ENDIF
088200930804     C                   ENDSR
088300940111     C*------------------------------------------------------------------------*
088400940111     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
088500940111     C*------------------------------------------------------------------------*
088600930804     C     POSSFL        BEGSR
088700930804     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
088800930804     C                   EXSR      CLRSFL
088900930804     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
089000000711     C                   EXSR      SETCLI
089100930804     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
089200930804     C     SFLEOF        IFEQ      0
089300930804     C                   EXSR      CARSFL
089400930805     C                   ENDIF
089500930804     C                   ENDSR
089600940111     C*------------------------------------------------------------------------*
089700930804     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
089800940111     C*------------------------------------------------------------------------*
089900930804     C     OPZSFL        BEGSR
090000930802     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
090100930802     C                   MOVEL     'S'           OPZOK             1
090200000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
090300000000     C                   Z-ADD     0             SFLVAR            5 0
090400000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
090500000000     C                   Z-ADD     0             SFLERR
090600000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
090700000000     C                   Z-ADD     SFLULT        WFLULT
090800000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
090900000725     C                   READC     TB83DVS                                49
091000930806     C     *IN49         DOWEQ     '0'
091100000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
091200000000     C                   ADD       1             SFLVAR
091300930802     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
091400930805     C     OPZOK         IFEQ      'S'
091500930802     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
091600930806     C     VSCOPZ        IFNE      *BLANKS
091700930802     C                   EXSR      CONOPZ
091800930802     C     OPZOK         IFEQ      'S'
091900930802     C                   EXSR      GESOPZ
092000930805     C                   ENDIF
092100930805     C                   ENDIF
092200000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
092300000000     C*            2. PULIZIA SCELTA IN SFL
092400930805     C     OPZOK         IFEQ      'S'
092500000000     C                   MOVEL     '0'           *IN41
092600930806     C                   MOVEL     *BLANKS       VSCOPZ
092700930805     C                   ELSE
092800000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
092900000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
093000000000     C                   MOVEL     '1'           *IN41
093100000000     C                   Z-ADD     SFLULT        SFLERR
093200930805     C                   ENDIF
093300930805     C                   ELSE
093400000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
093500000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
093600000000     C                   MOVEL     '1'           *IN41
093700930805     C                   ENDIF
093800000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
093900000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
094000000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
094100930803     C     OPZOK         IFNE      'S'
094200930806     C                   Z-ADD     SFLERR        VCCNBR
094300000000     C                   ELSE
094400930806     C                   Z-ADD     SFLULT        VCCNBR
094500930805     C                   ENDIF
094600000000     C* AGGIORNO RECORD SFL
094700000725     C                   UPDATE    TB83DVS
094800000725     C                   READC     TB83DVS                                49
094900930805     C                   ENDDO
095000000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
095100000000     C                   Z-ADD     WFLULT        SFLULT
095200000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
095300000719     C*          SFLVAR    IFEQ 0
095400000719     C*                    MOVEL'1'       $FINE
095500000719     C*                    ENDIF
095600000000     C                   ENDSR
095700940111     C*------------------------------------------------------------------------*
095800930804     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
095900940111     C*------------------------------------------------------------------------*
096000000000     C     CLRSFL        BEGSR
096100000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
096200000000     C* (0=NON FINE FILE  1=FINE FILE)
096300000000     C                   Z-ADD     0             SFLEOF
096400000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
096500000000     C                   Z-ADD     0             SFLULT
096600000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
096700000000     C                   MOVEL     '1'           *IN40
096800000725     C                   WRITE     TB83DVC
096900000000     C                   MOVEL     '0'           *IN40
097000000000     C                   ENDSR
097100940111     C*------------------------------------------------------------------------*
097200000000     C* CARSFL - CARICAMENTO SFL
097300940111     C*------------------------------------------------------------------------*
097400000000     C     CARSFL        BEGSR
097500000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
097600000000     C     SFLULT        IFEQ      0
097700000711     C                   EXSR      SETCLI
097800930805     C                   ENDIF
097900000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
098000000000     C                   Z-ADD     SFLULT        WFLULT
098100000000     C* CICLO:
098200000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
098300000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
098400000000     C                   Z-ADD     1             SFLRIG
098500930805     C     SFLRIG        DOWLE     SFLPAG
098600000000     C     SFLEOF        ANDEQ     0
098700000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
098800930806     C                   MOVEL     *BLANKS       VSCOPZ
098900000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
099000000000     C                   EXSR      IRESFL
099100000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
099200000000     C                   ADD       1             SFLULT
099300000000     C* SCRITTURA RECORD SFL
099400000725     C                   WRITE     TB83DVS
099500000000     C                   ADD       1             SFLRIG
099600000000     C* LETTURA FILE DA CARICARE
099700000711     C                   EXSR      LETCLI
099800930805     C                   ENDDO
099900000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
100000000000     C     WFLULT        IFNE      SFLULT
100100930806     C                   Z-ADD     WFLULT        VCCNBR
100200930806     C                   ADD       1             VCCNBR
100300930805     C                   ENDIF
100400930720     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
100500930805     C     SFLEOF        IFEQ      1
100600930721     C     SFLULT        ANDGT     *ZEROS
100700930721     C                   MOVEL     '1'           $ERR
100800930804     C                   MOVEL     MSG(6)        $MSG
100900930721     C                   EXSR      CARMSG
101000930804     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
101100000725     C     SFLULT        CHAIN     TB83DVS                            99
101200930804     C     *IN99         IFEQ      '0'
101300930804     C                   SETON                                        45
101400000725     C                   UPDATE    TB83DVS
101500930804     C                   SETOFF                                       45
101600930805     C                   ENDIF
101700930805     C                   ENDIF
101800000000     C                   ENDSR
101900940111     C*------------------------------------------------------------------------*
102000930804     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
102100940111     C*------------------------------------------------------------------------*
102200000000     C     OUTSFL        BEGSR
102300930805     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
102400930721     C     MSGULT        IFNE      *ZEROS
102500930721     C                   EXSR      OUTMSG
102600930805     C                   ENDIF
102700000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
102800000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
102900000000     C     SFLEOF        IFEQ      0
103000000000     C                   MOVEL     '0'           *IN42
103100000000     C                   ELSE
103200000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
103300000000     C                   MOVEL     '1'           *IN42
103400930805     C                   ENDIF
103500930720     C* EMISSIONE SUBFILE
103600000000     C     SFLULT        IFLE      0
103700930720     C*  --> CASO DI SFL VUOTO
103800930720     C                   MOVEL     '0'           *IN43
103900000725     C                   WRITE     TB83DVV
104000000725     C                   WRITE     TB83DVC
104100000725     C                   READ      TB83DVC                                99
104200000000     C                   ELSE
104300930720     C*  --> CASO DI SFL CON RECORDS
104400000725     C                   WRITE     TB83DVC
104500000725     C                   READ      TB83DVC                                99
104600930805     C                   ENDIF
104700930720     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
104800930720     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
104900930806     C                   Z-ADD     FRTSFL        VCCNBR
105000000000     C                   ENDSR
105100940111     C*------------------------------------------------------------------------*
105200930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
105300930721     C*          CODA MESSAGGI DEL PGM
105400940111     C*------------------------------------------------------------------------*
105500930721     C     CARMSG        BEGSR
105600930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
105700930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
105800930721     C     MSGULT        IFEQ      0
105900930721     C                   MOVEL     '1'           *IN40
106000000725     C                   WRITE     TB83DVK
106100930721     C                   MOVEL     '0'           *IN40
106200930805     C                   ENDIF
106300930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
106400930721     C                   MOVEL     $MSG          DSMSMS
106500930721     C                   CALL      'XMSG'        XMSGPA
106600930721     C     DSMSER        IFEQ      0
106700930806     C                   MOVEL     DSMSKE        VMCKEY
106800930805     C                   ENDIF
106900930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
107000931015     C                   ADD       1             MSGULT            4 0
107100000725     C                   WRITE     TB83DVM
107200930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
107300930721     C                   MOVEL     *BLANKS       $MSG
107400930721     C                   ENDSR
107500940111     C*------------------------------------------------------------------------*
107600930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
107700940111     C*------------------------------------------------------------------------*
107800930721     C     OUTMSG        BEGSR
107900930721     C* VISUALIZZAZIONE MESSAGGIO
108000930721     C                   MOVEL     '1'           *IN44
108100000725     C                   WRITE     TB83DVK
108200930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
108300930721     C                   Z-ADD     *ZEROS        MSGULT
108400930721     C                   ENDSR
108500940111     C*------------------------------------------------------------------------*
108600930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
108700930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
108800930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
108900930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
109000930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
109100940111     C*------------------------------------------------------------------------*
109200930803     C     CARFUN        BEGSR
109300930803     C                   Z-ADD     *ZEROS        FUNULT            5 0
109400931015     C     1             DO        31            I                 5 0
109500930803     C                   MOVEL     FUN(I)        FUNRIG
109600930805     C                   MOVEL     FUNFUN        FFU(I)
109700930805     C                   MOVEL     FUNDES        FDE(I)
109800020724     C     SIMTPP        IFEQ      'S'
109900930923     C     $TGES         IFEQ      'G'
110000930805     C                   MOVEL     FUNASG        FAT(I)
110100930803     C                   ELSE
110200930803     C                   MOVEL     FUNASI        FAT(I)
110300930803     C                   ENDIF
110400930803     C                   ELSE
110500930923     C     $TGES         IFEQ      'G'
110600930803     C                   MOVEL     FUNAFG        FAT(I)
110700930803     C                   ELSE
110800930803     C                   MOVEL     FUNAFI        FAT(I)
110900930803     C                   ENDIF
111000930803     C                   ENDIF
111100930806     C     FAT(I)        IFEQ      'E'
111200930806     C                   MOVEL     USER          WEDP              3
111300930806     C     WEDP          IFEQ      'EDP'
111400930806     C                   MOVEL     'S'           FAT(I)
111500930806     C                   ELSE
111600930806     C                   MOVEL     'N'           FAT(I)
111700930806     C                   ENDIF
111800930806     C                   ENDIF
111900930803     C                   ENDDO
112000930803     C                   ENDSR
112100940111     C*------------------------------------------------------------------------*
112200930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
112300930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
112400930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
112500930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
112600940111     C*------------------------------------------------------------------------*
112700930803     C     OUTFUN        BEGSR
112800930805     C     1             DO        8             I
112900930805     C                   MOVEL     *BLANKS       FUV(I)
113000930803     C                   ENDDO
113100930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
113200930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0
113300930803     C     1             DO        31            I
113400930803     C     FAT(I)        IFEQ      'S'
113500930803     C     FDE(I)        ANDNE     *BLANKS
113600930803     C                   ADD       1             FUNNAT
113700930803     C                   ENDIF
113800930803     C                   ENDDO
113900930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
114000930803     C     FUNNAT        IFGT      8
114100930803     C                   MOVEL     'S'           FAT(24)
114200930803     C                   ENDIF
114300930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
114400930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
114500930803     C                   Z-ADD     8             FUNVIS            5 0
114600930803     C     FAT(23)       IFEQ      'S'
114700930803     C                   SUB       1             FUNVIS
114800930803     C                   ENDIF
114900930803     C     FAT(24)       IFEQ      'S'
115000930803     C                   SUB       1             FUNVIS
115100930803     C                   ENDIF
115200930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
115300931015     C                   Z-ADD     *ZEROS        W                 5 0
115400930803     C                   ADD       1             FUNULT
115500930803     C     FUNULT        IFGT      31
115600930803     C                   Z-ADD     1             FUNULT
115700930805     C                   ENDIF
115800930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
115900930803     C     FUNULT        DO        31            I
116000930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
116100930803     C     W             IFLT      FUNVIS
116200930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
116300930803     C     FAT(I)        IFEQ      'S'
116400930803     C     FDE(I)        ANDNE     *BLANKS
116500930805     C     I             IFEQ      23
116600930805     C     I             OREQ      24
116700930803     C                   ELSE
116800930803     C                   ADD       1             W
116900930803     C                   MOVEL     FDE(I)        FUV(W)
117000930803     C                   ENDIF
117100930803     C                   ENDIF
117200930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
117300930803     C                   Z-ADD     I             FUNULT
117400930803     C                   ENDIF
117500930803     C                   ENDDO
117600930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
117700930806     C                   MOVEL     FUV(1)        VFD001
117800930806     C                   MOVEL     FUV(2)        VFD002
117900930806     C                   MOVEL     FUV(3)        VFD003
118000930806     C                   MOVEL     FUV(4)        VFD004
118100930806     C                   MOVEL     FUV(5)        VFD005
118200930806     C                   MOVEL     FUV(6)        VFD006
118300930803     C     FAT(23)       IFEQ      'N'
118400930803     C     FAT(24)       ANDEQ     'N'
118500930806     C                   MOVEL     FUV(7)        VFD007
118600930806     C                   MOVEL     FUV(8)        VFD008
118700930803     C                   ELSE
118800930803     C     FAT(23)       IFEQ      'S'
118900930803     C     FAT(24)       ANDEQ     'S'
119000930806     C                   MOVEL     FDE(23)       VFD007
119100930806     C                   MOVEL     FDE(24)       VFD008
119200930803     C                   ELSE
119300930806     C                   MOVEL     FUV(7)        VFD007
119400930803     C     FAT(23)       IFEQ      'S'
119500930806     C                   MOVEL     FDE(23)       VFD008
119600930803     C                   ELSE
119700930806     C                   MOVEL     FDE(24)       VFD008
119800930805     C                   ENDIF
119900930805     C                   ENDIF
120000930805     C                   ENDIF
120100930803     C                   MOVEL     '1'           $TESVD
120200930803     C                   ENDSR
120300940111     C*------------------------------------------------------------------------*
120400930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
120500930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
120600930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
120700930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
120800940111     C*------------------------------------------------------------------------*
120900930803     C     CONFUN        BEGSR
121000930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
121100930803     C     1             DO        31            I
121200930803     C     KEY           IFEQ      KEYS(I)
121300930805     C* CONTROLLO SE IL TASTO E' ATTIVO
121400930803     C     FAT(I)        IFNE      'S'
121500930804     C                   MOVEL     MSG(1)        $MSG
121600930803     C                   EXSR      CARMSG
121700930803     C                   MOVEL     '1'           $ERR
121800930803     C                   ENDIF
121900930803     C                   ENDIF
122000930803     C                   ENDDO
122100930803     C                   ENDSR
122200940111     C*------------------------------------------------------------------------*
122300930802     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
122400930802     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
122500930803     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
122600930803     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
122700930803     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
122800930803     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
122900940111     C*------------------------------------------------------------------------*
123000930802     C     CAROPZ        BEGSR
123100930803     C                   Z-ADD     *ZEROS        OPZULT            5 0
123200930802     C     1             DO        24            I
123300930803     C                   MOVEL     OPZ(I)        OPZRIG
123400930805     C                   MOVEL     OPZOPD        OPD(I)
123500930805     C                   MOVEL     OPZOPS        OPS(I)
123600930805     C                   MOVEL     OPZDES        ODE(I)
123700020724     C     SIMTPP        IFEQ      'S'
123800930923     C     $TGES         IFEQ      'G'
123900930805     C                   MOVEL     OPZASG        OAT(I)
124000930802     C                   ELSE
124100930805     C                   MOVEL     OPZASI        OAT(I)
124200930802     C                   ENDIF
124300930802     C                   ELSE
124400930923     C     $TGES         IFEQ      'G'
124500930805     C                   MOVEL     OPZAFG        OAT(I)
124600930802     C                   ELSE
124700930805     C                   MOVEL     OPZAFI        OAT(I)
124800930802     C                   ENDIF
124900930802     C                   ENDIF
125000930806     C     OAT(I)        IFEQ      'E'
125100930806     C                   MOVEL     USER          WEDP              3
125200930806     C     WEDP          IFEQ      'EDP'
125300930806     C                   MOVEL     'S'           OAT(I)
125400930806     C                   ELSE
125500930806     C                   MOVEL     'N'           OAT(I)
125600930806     C                   ENDIF
125700930806     C                   ENDIF
125800930802     C                   ENDDO
125900930802     C                   ENDSR
126000940111     C*------------------------------------------------------------------------*
126100930802     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
126200930802     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
126300930802     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
126400930806     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
126500930802     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
126600940111     C*------------------------------------------------------------------------*
126700930802     C     OUTOPZ        BEGSR
126800930805     C     1             DO        8             I
126900930805     C                   MOVEL     *BLANKS       OPV(I)
127000930802     C                   ENDDO
127100930802     C                   Z-ADD     *ZEROS        OPZNAT            2 0
127200930805     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
127300930803     C                   ADD       1             OPZULT
127400930803     C     OPZULT        IFGT      24
127500930803     C                   Z-ADD     1             OPZULT
127600930803     C                   END
127700930805     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
127800930802     C     1             DO        24            I
127900930805     C     OAT(I)        IFEQ      'S'
128000930805     C     ODE(I)        ANDNE     *BLANKS
128100930802     C                   ADD       1             OPZNAT
128200930802     C                   ENDIF
128300930802     C                   ENDDO
128400930802     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
128500931015     C                   Z-ADD     0             W
128600930802     C     OPZULT        DO        24            I
128700930802     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
128800930802     C     W             IFLT      8
128900930802     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
129000930805     C     OAT(I)        IFEQ      'S'
129100930803     C     ODE(I)        ANDNE     *BLANKS
129200930802     C                   ADD       1             W
129300930803     C                   MOVEL     ODE(I)        OPV(W)
129400930805     C                   ENDIF
129500930805     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
129600930802     C                   Z-ADD     I             OPZULT
129700930802     C                   ENDIF
129800930802     C                   ENDDO
129900930802     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
130000930806     C                   MOVEL     OPV(1)        VOD001
130100930806     C                   MOVEL     OPV(2)        VOD002
130200930806     C                   MOVEL     OPV(3)        VOD003
130300930806     C                   MOVEL     OPV(4)        VOD004
130400930806     C                   MOVEL     OPV(5)        VOD005
130500930806     C                   MOVEL     OPV(6)        VOD006
130600930806     C                   MOVEL     OPV(7)        VOD007
130700930806     C                   MOVEL     OPV(8)        VOD008
130800930802     C                   MOVEL     '1'           $TESVD
130900930923     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
131000930923     C     OPZNAT        IFGT      8
131100930923     C                   MOVEL     'S'           FAT(23)
131200930923     C                   ENDIF
131300930802     C                   ENDSR
131400940111     C*------------------------------------------------------------------------*
131500930802     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
131600930806     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
131700930802     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
131800930802     C*                   - ERRORE SE OPZIONE NON ATTIVA
131900940111     C*------------------------------------------------------------------------*
132000930802     C     CONOPZ        BEGSR
132100000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
132200930803     C                   MOVEL     *BLANKS       OPZOK             1
132300930805     C     1             DO        24            I                 5 0
132400930806     C     VSCOPZ        IFEQ      OPD(I)
132500930806     C     VSCOPZ        OREQ      OPS(I)
132600930805     C     OAT(I)        IFEQ      'S'
132700930806     C                   MOVEL     OPD(I)        VSCOPZ
132800940105     C                   MOVEL     'S'           OPZOK
132900930805     C                   ELSE
133000930802     C                   MOVEL     'N'           OPZOK             1
133100930805     C                   ENDIF
133200930802     C                   ENDIF
133300930802     C                   ENDDO
133400930805     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
133500930803     C     OPZOK         IFNE      'S'
133600930804     C                   MOVEL     MSG(3)        $MSG
133700930803     C                   EXSR      CARMSG
133800930803     C                   MOVEL     '1'           $ERR
133900930803     C                   MOVEL     'N'           OPZOK             1
134000930803     C                   ENDIF
134100000000     C                   ENDSR
134200930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
134300000725TRTB83R- Tasto funzionale non valido.                                          1
134400000725TRTB83R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
134500000725TRTB83R- Opzione non valida.                                                   3
134600000725TRTB83R- Opzione abilitata ma non gestita. Avvertire CED.                      4
134700000725TRTB83R- Inizio elenco.                                                        5
134800000725TRTB83R- Fine elenco.                                                          6
134900000725TRTB83R- 7                                                                     7
135000000725TRTB83R- 8                                                                     8
135100000725TRTB83R- 9                                                                     9
135200000725TRTB83R- 10                                                                   10
135300000725TRTB83R- 11                                                                   11
135400000725TRTB83R- 12                                                                   12
135500000725TRTB83R- 13                                                                   13
135600000725TRTB83R- 14                                                                   14
135700000725TRTB83R- 15                                                                   15
135800000725TRTB83R- 16                                                                   16
135900000725TRTB83R- 17                                                                   17
136000000725TRTB83R- 18                                                                   18
136100000725TRTB83R- 19                                                                   19
136200000725TRTB83R- 20                                                                   20
136300930729** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
136400930803- 1-1 -                   -    NN     NN
136500940112- 2-2 -2=Modifica         -    SS     SS
136600000711- 3-3 -3=COPIA            -    SS     SS
136700940112- 4-4 -4=Cancellazione    -    SS     SS
136800940121- 5-5 -5=Visualizzazione  -    SS     SS
136900930803- 6-6 -                   -    NN     NN
137000940112- 7-7 -7=Ripristino       -    SS     SS
137100940104- 8-8 -                   -    NN     NN
137200940111- 9-9 -                   -    NN     NN
137300930806-10-  -                   -    NN     NN
137400930806-11-  -                   -    NN     NN
137500930806-12-  -                   -    NN     NN
137600930803-13-  -                   -    NN     NN
137700930803-14-  -                   -    NN     NN
137800930803-15-  -                   -    NN     NN
137900930803-16-  -                   -    NN     NN
138000930803-17-  -                   -    NN     NN
138100930803-18-  -                   -    NN     NN
138200930803-19-  -                   -    NN     NN
138300930803-20-  -                   -    NN     NN
138400930803-21-  -                   -    NN     NN
138500930803-22-  -                   -    NN     NN
138600930803-23-  -                   -    NN     NN
138700930803-24-  -                   -    NN     NN
138800930730** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
138900930723F1    -                   -    NN     NN
139000930723F2    -                   -    NN     NN
139100930802F3    -F3=Fine            -    SS     SS
139200930723F4    -                   -    NN     NN
139300940112F5    -F5=Rivisualizzaz.  -    SS     SS
139400940112F6    -F6=Inserimento     -    SS     SS
139500930723F7    -                   -    NN     NN
139600930723F8    -                   -    NN     NN
139700930723F9    -                   -    NN     NN
139800930723F10   -                   -    NN     NN
139900930723F11   -                   -    NN     NN
140000930802F12   -F12=Ritorno        -    SS     SS
140100930806F13   -                   -    NN     NN
140200930806F14   -                   -    NN     NN
140300930723F15   -                   -    NN     NN
140400930723F16   -                   -    NN     NN
140500930802F17   -F17=Inizio dati    -    SS     SS
140600930802F18   -F18=Fine dati      -    SS     SS
140700930723F19   -                   -    NN     NN
140800930723F20   -                   -    NN     NN
140900940111F21   -                   -    NN     NN
141000940111F22   -                   -    NN     NN
141100930803F23   -F23=Altre opzioni  -    NN     NN
141200930803F24   -F24=Altri tasti    -    NN     NN
141300930802ENTER -                   -    SS     SS
141400930802ROLLUP-                   -    SS     SS
141500930802ROLLDN-                   -    SS     SS
141600930802HELP  -                   -    NN     NN
141700930802PRINT -                   -    NN     NN
141800930802BSPACE-                   -    NN     NN
141900930802CLEAR -                   -    NN     NN
