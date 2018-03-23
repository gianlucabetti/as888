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
022500140915     C                   MOVE      VIDADB        WADB              1
022600140915     C                   MOVE      VIDADS        WADS              1
022700940114     C                   MOVEL     'GES'         WOP0
022800940114     C                   MOVEL     'O02'         WOP1
022900000725     C                   MOVEL     'TRTB84R'     WPGM
023000930805     C                   EXSR      LANPGM
023100000000     C*
023200930805     C                   Z-ADD     1             SWMODI
023300930128     C*
023400940111E   1C                   ENDIF
023500000000     C*
023600000000     C                   ENDSR
023700000711     C*------------------------------------------------------------------------*
023800000711     C*  O03GES - GESTIONE OPZIONE 3=COPIA
023900000711     C*------------------------------------------------------------------------*
024000000711     C     O03GES        BEGSR
024100000711     C*
024200000711     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
024300000801IF  1C     VSCATB        IFEQ      'A'
024400000801     C                   MOVEL     '1'           $ERR
024500000801     C                   MOVEL     MSG(12)       $MSG
024600000801     C                   EXSR      CARMSG
024700000801E   1C                   ENDIF
024800000711     C*
024900000711     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
025000000711IF  1C     VIDCLI        IFEQ      *BLANKS
025100000711     C                   MOVEL     USER          WEDP              3
025200000711IF  2C     WEDP          IFNE      'EDP'
025300000711     C                   MOVEL     '1'           $ERR
025400000711     C                   MOVEL     MSG(11)       $MSG
025500000711     C                   EXSR      CARMSG
025600000711E   2C                   ENDIF
025700000711E   1C                   ENDIF
025800000711     C*
025900000711IF  1C     $ERR          IFEQ      '0'
026000000711     C*
026100000711     C* LANCIO LA COPIA
026200000711     C                   MOVEL     VIDCLI        WCLI
026300000711     C                   Z-ADD     VIDCKS        WCKS
026400000711     C                   MOVE      VIDCSD        WCSD
026500000711     C                   MOVE      VIDCDM        WCDM
026600000711     C                   MOVE      VIDPRF        WPRF
026700140915     C                   MOVE      VIDADB        WADB
026800140915     C                   MOVE      VIDADS        WADS
026900000711     C                   MOVEL     'GES'         WOP0
027000000711     C                   MOVEL     'O03'         WOP1
027100000725     C                   MOVEL     'TRTB84R'     WPGM
027200000711     C                   EXSR      LANPGM
027300000711     C*
027400000711     C                   Z-ADD     1             SWMODI
027500000711     C*
027600000711E   1C                   ENDIF
027700000711     C*
027800000711     C                   ENDSR
027900940111     C*------------------------------------------------------------------------*
028000000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
028100940111     C*------------------------------------------------------------------------*
028200000000     C     O04GES        BEGSR
028300930128     C*
028400930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
028500000801IF  1C     VSCATB        IFEQ      'A'
028600000801     C                   MOVEL     '1'           $ERR
028700000801     C                   MOVEL     MSG(12)       $MSG
028800000801     C                   EXSR      CARMSG
028900000801E   1C                   ENDIF
029000940117     C*
029100940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
029200000711IF  1C     VIDCLI        IFEQ      *BLANKS
029300940117     C                   MOVEL     USER          WEDP              3
029400940117IF  2C     WEDP          IFNE      'EDP'
029500940117     C                   MOVEL     '1'           $ERR
029600940117     C                   MOVEL     MSG(11)       $MSG
029700940117     C                   EXSR      CARMSG
029800940117E   2C                   ENDIF
029900940117E   1C                   ENDIF
030000931217     C*
030100940111IF  1C     $ERR          IFEQ      '0'
030200940111     C*
030300940111     C* LANCIO LA CANCELLAZIONE
030400000711     C                   MOVEL     VIDCLI        WCLI
030500000711     C                   Z-ADD     VIDCKS        WCKS
030600000711     C                   MOVE      VIDCSD        WCSD
030700000711     C                   MOVE      VIDCDM        WCDM
030800000711     C                   MOVE      VIDPRF        WPRF
030900140915     C                   MOVE      VIDADB        WADB
031000140915     C                   MOVE      VIDADS        WADS
031100940114     C                   MOVEL     'GES'         WOP0
031200940114     C                   MOVEL     'O04'         WOP1
031300000725     C                   MOVEL     'TRTB84R'     WPGM
031400930201     C                   EXSR      LANPGM
031500000000     C*
031600930805     C                   Z-ADD     1             SWMODI
031700940111     C*
031800940111E   1C                   ENDIF
031900000000     C*
032000000000     C                   ENDSR
032100940121     C*------------------------------------------------------------------------*
032200000711     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZA
032300940121     C*------------------------------------------------------------------------*
032400940121     C     O05GES        BEGSR
032500940121     C*
032600940121     C* LANCIO LA VISUALIZZAZIONE
032700000711     C                   MOVEL     VIDCLI        WCLI
032800000711     C                   Z-ADD     VIDCKS        WCKS
032900000711     C                   MOVE      VIDCSD        WCSD
033000000711     C                   MOVE      VIDCDM        WCDM
033100000711     C                   MOVE      VIDPRF        WPRF
033200140915     C                   MOVE      VIDADB        WADB
033300140915     C                   MOVE      VIDADS        WADS
033400940121     C                   MOVEL     'GES'         WOP0
033500940121     C                   MOVEL     'O05'         WOP1
033600000725     C                   MOVEL     'TRTB84R'     WPGM
033700940121     C                   EXSR      LANPGM
033800940121     C*
033900940121     C                   ENDSR
034000940111     C*------------------------------------------------------------------------*
034100930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
034200940111     C*------------------------------------------------------------------------*
034300930128     C     O07GES        BEGSR
034400930128     C*
034500930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
034600000801IF  1C     VSCATB        IFNE      'A'
034700000801     C                   MOVEL     '1'           $ERR
034800000801     C                   MOVEL     MSG(13)       $MSG
034900000801     C                   EXSR      CARMSG
035000000801E   1C                   ENDIF
035100940117     C*
035200940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
035300000711IF  1C     VIDCLI        IFEQ      *BLANKS
035400940117     C                   MOVEL     USER          WEDP              3
035500940117IF  2C     WEDP          IFNE      'EDP'
035600940117     C                   MOVEL     '1'           $ERR
035700940117     C                   MOVEL     MSG(11)       $MSG
035800940117     C                   EXSR      CARMSG
035900940117E   2C                   ENDIF
036000940117E   1C                   ENDIF
036100930215     C*
036200940111IF  1C     $ERR          IFEQ      '0'
036300930128     C*
036400940111     C* LANCIO IL RIPRISTINO
036500000711     C                   MOVEL     VIDCLI        WCLI
036600000711     C                   Z-ADD     VIDCKS        WCKS
036700000711     C                   MOVE      VIDCSD        WCSD
036800000711     C                   MOVE      VIDCDM        WCDM
036900000711     C                   MOVE      VIDPRF        WPRF
037000140915     C                   MOVE      VIDADB        WADB
037100140915     C                   MOVE      VIDADS        WADS
037200940114     C                   MOVEL     'GES'         WOP0
037300940114     C                   MOVEL     'O07'         WOP1
037400000725     C                   MOVEL     'TRTB84R'     WPGM
037500930201     C                   EXSR      LANPGM
037600930128     C*
037700930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
037800930128     C                   Z-ADD     1             SWMODI
037900930128     C*
038000940111E   1C                   ENDIF
038100930128     C*
038200930128     C                   ENDSR
038300940111     C*------------------------------------------------------------------------*
038400930205     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
038500940111     C*------------------------------------------------------------------------*
038600930205     C     NEWPOS        BEGSR
038700930205     C*
038800940111     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
038900940111     C*        1 = RICHIESTO POSIZIONAMENTO
039000930205     C                   Z-ADD     0             SWPOSI            7 0
039100930205     C*
039200940111     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
039300940111     C*            NON OCCORRE RICARICARE IL SUBFILE
039400940111     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
039500930205     C                   Z-ADD     0             SWMODI            1 0
039600930205     C*
039700930205     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
039800000719IF  1C     VCCCLI        IFNE      DEPCLI
039900101119IF  1C     VCCuni        orne      DEPuni
040000101119IF  1C     VCCsun        orne      DEPsun
040100930205     C*
040200930205     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
040300930205     C                   EXSR      CHKPAR
040400930205     C*
040500930205     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
040600940111IF  2C     $ERR          IFEQ      '0'
040700940111     C                   Z-ADD     1             SWPOSI
040800000711     C                   MOVEL     VCCCLI        DEPCLI
040900101119     C                   Z-ADD     VCCuni        DEPuni
041000101119     C                   movel     VCCsun        DEPsun
041100940111E   2C                   ENDIF
041200000719E   1C                   ENDIF
041300930205     C*
041400930205     C                   ENDSR
041500940111     C*------------------------------------------------------------------------*
041600940111     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
041700940111     C*------------------------------------------------------------------------*
041800940111     C     CHKPAR        BEGSR
041900940111     C*
042000940111     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
042100940111     C                   SETOFF                                       80
042200940111     C*
042300000718     C* CONTROLLO ESISTENZA
042400090518     C                   MOVEL     VCCCLI        WCLI              7
042500000711     C                   MOVEL     *BLANKS       WDCLI            22
042600000711     C                   EXSR      CHKCLI
042700940113IF  2C     $ERR          IFEQ      '1'
042800940111     C                   MOVEL     '1'           $ERR
042900940111     C                   MOVEL     MSG(9)        $MSG
043000940111     C                   EXSR      CARMSG
043100940111     C                   SETON                                        80
043200940113E   2C                   ENDIF
043300000719     C                   MOVEL     WDCLI         VIDDCL
043400940111     C*
043500940111     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
043600940111     C* CURSORE SUL CAMPO DEL CONTROL
043700940111IF  1C     $ERR          IFEQ      '1'
043800940111     C                   MOVEL     '0'           *IN43
043900940111X   1C                   ELSE
044000940111     C                   MOVEL     '1'           *IN43
044100940111E   1C                   ENDIF
044200940111     C*
044300940111     C                   ENDSR
044400940111     C*------------------------------------------------------------------------*
044500000711     C*  CHKCLI - CONTROLLA LA FILIALE
044600940111     C*------------------------------------------------------------------------*
044700000711     C     CHKCLI        BEGSR
044800940111     C*
044900940114     C* CONTROLLO ESISTENZA NELL'ORGANIGRAMMA
045000120216     C*//                MOVE      VIDCLI        ULTIMO            1
045100120216     C*//  ULTIMO        IFEQ      *BLANK
045200120216     C*//  VIDCLI        ANDNE     *BLANK
045300120216     c                   if        %check( c_Digits : VIDcli ) = *zero
045400000718     C                   MOVEL     VIDCLI        KORCLI
045500000711     C     KEYCLI        CHAIN     CNACO00F                           99
045600940114IF  2C     *IN99         IFEQ      '1'
045700940114     C                   MOVEL     '1'           $ERR
045800000711     C                   MOVEL     *ALL'*'       WDCLI
045900940114X   2C                   ELSE
046000000711     C                   MOVEL     ACORAG        WDCLI
046100940114E   2C                   ENDIF
046200000711     C                   ELSE
046300000711     C                   MOVEL     *BLANK        WDCLI
046400000711     C                   END
046500940113     C*
046600940114     C                   ENDSR
046700940112     C*------------------------------------------------------------------------*
046800000711     C* SETCLI - POSIZIONAMENTO E PRIMA LETTURA RECORD TABELLE
046900940112     C*------------------------------------------------------------------------*
047000000711     C     SETCLI        BEGSR
047100940112     C*
047200000719     C* POSIZIONAMENTO RECORD TABELLA
047300940112     C*
047400020724     C                   Z-ADD     1             KTBKUT
047500000725     C                   MOVEL     '3Q'          KTBCOD
047600940112     C                   MOVEL     *BLANKS       KTBKEY
047700000711     C                   MOVEL     WCLI          KTBKEY
047800940112     C*
047900940113     C     KEYTAB        SETLL     TABEL00F                           99
048000940112     C*
048100000711     C                   EXSR      LETCLI
048200940112     C*
048300940112     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
048400940112IF  1C     *IN99         IFEQ      '1'
048500940112     C                   Z-ADD     1             SFLEOF
048600940112E   1C                   ENDIF
048700940112     C*
048800940112     C                   ENDSR
048900940112     C*------------------------------------------------------------------------*
049000000711     C* LETCLI - LEGGE RECORD SUCCESSIVO
049100940112     C*------------------------------------------------------------------------*
049200000711     C     LETCLI        BEGSR
049300940112     C*
049400940113     C* FINO A QUANDO NON TROVI UN RECORD VALIDO (RECOK = '1') STAI DENTRO
049500940113     C                   MOVEL     '0'           RECOK             1
049600940113DO  1C     RECOK         DOWEQ     '0'
049700940112     C*
049800000711     C* LETTURA RECORD TABELLA
049900940113     C                   READ      TABEL00F                               99
050000101119     C                   MOVEL     '1'           RECOK
050100940112     C*
050200940113     C* FINE FILE  O TABELLA DIVERSA
050300940112IF  2C     *IN99         IFEQ      '1'
050400940113     C     TBLKUT        ORNE      1
050500000725     C     TBLCOD        ORNE      '3Q'
050600940112     C                   Z-ADD     1             SFLEOF
050700000718     C                   ELSE
050800101119     c                   movel     tbluni        ds3Q
050900101119     c                   movel     §3Qcks        w007a             7
051000101119if  3c                   if        (VCCuni > *zero and VCCuni <> §3Qcks)
051100101119     c                             or
051200101119     c                             (VCCsun = 'S' and %subst(TBLkey:1:7)<>w007a)
051300101119     C                   MOVEL     '0'           RECOK
051400101119e   3c                   endif
051500940112E   2C                   ENDIF
051600940112E   1C                   ENDDO
051700940112     C*
051800940112     C                   ENDSR
051900940111     C*------------------------------------------------------------------------*
052000000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
052100940111     C*------------------------------------------------------------------------*
052200000000     C     IRESFL        BEGSR
052300940112     C*
052400000719     C* RIEMPIMENTO CAMPI
052500000801IF  1C     TBLFLG        IFNE      *BLANKS
052600000801     C                   MOVEL     'A'           VSCATB
052700000801X   1C                   ELSE
052800000801     C                   MOVEL     ' '           VSCATB
052900000801E   1C                   ENDIF
053000000711     C                   MOVEL     TBLKEY        VIDCLI
053100000725     C                   MOVEL     TBLUNI        DS3Q
053200000725     C                   MOVEL     §3QCSD        VIDCSD
053300000725     C                   MOVEL     §3QCDM        VIDCDM
053400000725     C                   MOVEL     §3QCKS        VIDCKS
053500000725     C                   MOVEL     §3QPRF        VIDPRF
053600140915     C                   MOVEL     §3QADB        VIDADB
053700140915     C                   MOVEL     §3QADS        VIDADS
053800000718     C                   EXSR      CHKCLI
053900000718     C                   MOVEL     WDCLI         VIDDCL
054000940112     C*
054100000000     C                   ENDSR
054200940111     C*------------------------------------------------------------------------*
054300930201     C* LANPGM - LANCIA UN PROGRAMMA
054400940111     C*------------------------------------------------------------------------*
054500930201     C     LANPGM        BEGSR
054600940114     C*
054700940114     C*--------------------
054800940114     C* PARAMETRI INFORMATIVI
054900940114     C*--------------------
055000000725     C                   MOVEL     WOP0          D83OP0
055100000725     C                   MOVEL     WOP1          D83OP1
055200000725     C                   MOVEL     '0'           D83F03
055300000725     C                   MOVEL     '0'           D83ERR
055400000725     C                   MOVEL     *BLANKS       D83MSG
055500940114     C*--------------------
055600940114     C* PARAMETRI
055700940114     C*--------------------
055800000725     C                   MOVEL     WCLI          D83CLI
055900000725     C                   Z-ADD     WCKS          D83CKS
056000000725     C                   MOVE      WCSD          D83CSD
056100000725     C                   MOVE      WCDM          D83CDM
056200000725     C                   MOVE      WPRF          D83PRF
056300140915     C                   MOVE      WADB          D83ADB
056400140915     C                   MOVE      WADS          D83ADS
056500940114     C* LANCIO
056600940114     C                   CALL      WPGM
056700000725     C                   PARM                    DS83
056800940114     C*
056900930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
057000000725IF  1C     D83F03        IFEQ      '1'
057100930721     C                   MOVEL     '1'           $F03
057200930721     C                   MOVEL     '1'           $ERR
057300940105     C                   MOVEL     '1'           $FINE
057400940114     C*
057500930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
057600940114X   1C                   ELSE
057700000725     C                   MOVEL     D83MSG        $MSG
057800930721     C                   EXSR      CARMSG
057900940114E   1C                   ENDIF
058000930201     C*
058100930201     C                   ENDSR
058200940114     C*------------------------------------------------------------------------*
058300000725     C*  CAR3Q  - CARICAMENTO DATI DI 3Q
058400940114     C*------------------------------------------------------------------------*
058500000725     C     CAR3Q         BEGSR
058600940114     C*
058700000719     C* CARICO I DATI DI
058800940114     C*
058900020724     C                   Z-ADD     1             KTBKUT
059000000725     C                   MOVEL     '3Q'          KTBCOD
059100940114     C                   MOVEL     *BLANKS       KTBKEY
059200000719     C                   MOVEL     VCCCLI        KTBKEY
059300940114     C*
059400000719     C     KEYTAB        CHAIN     TABEL00F                           99
059500940114IF  1C     *IN99         IFEQ      *OFF
059600000725     C                   MOVEL     TBLUNI        DS3Q
059700940114X   1C                   ELSE
059800000725     C                   CLEAR                   DS3Q
059900940114E   1C                   ENDIF
060000940114     C*
060100000725     C* MUOVO I CAMPI DI '3Q' IN QUELLI DELLA DS DI PROCEDURA PER IL PASSAGGIO
060200000725     C                   MOVEL     §3QCSD        D83CSD
060300000725     C                   Z-ADD     §3QCKS        D83CKS
060400000725     C                   MOVEL     §3QCDM        D83CDM
060500000725     C                   MOVEL     §3QPRF        D83PRF
060600140915     C                   MOVEL     §3QADB        D83ADB
060700140915     C                   MOVEL     §3QADS        D83ADS
060800000719     C                   EXSR      CHKPAR
060900940114     C*
061000940114     C                   ENDSR
061100940111     C*------------------------------------------------------------------------*
061200930713     C*  *INZSR - ROUTINE INIZIALE
061300940111     C*------------------------------------------------------------------------*
061400930713     C     *INZSR        BEGSR
061500000000     C*
061600000000     C* RICEVIMENTO PARAMETRI
061700000000     C     *ENTRY        PLIST
061800940105     C                   PARM                    KPJBA
061900000719     C                   MOVEL     KPJBU         VCCCLI
062000120808     C                   MOVEL     VCCCLI        DEPCLI
062100120808     C                   Z-ADD     VCCuni        DEPuni
062200120808     C                   movel     VCCsun        DEPsun
062300940111     C*
062400940111     C* DEFINISCE LE VARIABILI DI PROGRAMMA
062500000711     C     *LIKE         DEFINE    VCCCLI        DEPCLI
062600101119     C     *LIKE         DEFINE    VCCuni        DEPuni
062700101119     C     *LIKE         DEFINE    VCCsun        DEPsun
062800940111     C     *LIKE         DEFINE    TBLKUT        KTBKUT
062900940111     C     *LIKE         DEFINE    TBLCOD        KTBCOD
063000940111     C     *LIKE         DEFINE    TBLKEY        KTBKEY
063100000711     C     *LIKE         DEFINE    ACOKSC        KORCLI
063200930310     C*
063300930310     C* CHIAVE LETTURA TABEL00F - COMPLETA
063400940112     C     KEYTAB        KLIST
063500940111     C                   KFLD                    KTBKUT
063600940111     C                   KFLD                    KTBCOD
063700940111     C                   KFLD                    KTBKEY
063800940112     C*
063900940114     C* CHIAVE LETTURA TABEL00F - PARZIALE
064000940112     C     KEYTA2        KLIST
064100940112     C                   KFLD                    KTBKUT
064200940112     C                   KFLD                    KTBCOD
064300940114     C*
064400940114     C* CHIAVE LETTURA AZORG01L - COMPLETA
064500000711     C     KEYCLI        KLIST
064600000711     C                   KFLD                    KTBKUT
064700000711     C                   KFLD                    KORKCC            4 0
064800000711     C                   KFLD                    KORCLI
064900000711     C                   Z-ADD     151           KORKCC
065000930714     C*
065100930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
065200930803     C                   EXSR      INZSTD
065300930803     C*
065400930803     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
065500930803     C                   EXSR      INZSFL
065600020724     C                   MOVEL     RAGUT         VTDRSU
065700980130     C*
065800980130     C* CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
065900180219     C                   CLEAR                   UL12DS
066000180219     C                   MOVEL     'L'           I12TLA
066100000725     C                   MOVEL     '3Q'          I12COD
066200980130     C                   MOVEL     KNSIF         I12NSI
066300980130     C                   CALL      'TRUL12R'
066400980130     C                   PARM                    UL12DS
066500180219     C**   O12ERR        IFEQ      '1'
066600180219     C**                 MOVEL     '1'           $FINE
066700180219     C**                 ENDIF
066800940104     C*
066900000719     C* CARICA I DATI
067000000725     C                   EXSR      CAR3Q
067100940114     C*
067200940104     C* EMETTO VIDEATA SUBFILE
067300940114     C                   EXSR      CARVS
067400940114     C*
067500000000     C                   ENDSR
067600940111     C*------------------------------------------------------------------------*
067700940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067800940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067900940111     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
068000940111     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
068100940111     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
068200940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
068300940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
068400940111     C*------------------------------------------------------------------------*
068500930803     C* INZSTD - ROUTINE INIZIALE STANDARD
068600940111     C*------------------------------------------------------------------------*
068700930803     C     INZSTD        BEGSR
068800930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
068900930802     C                   CALL      'XKEYS'
069000930802     C                   PARM                    DSKEYS
069100930805     C                   MOVEA     KEYRIG        KEYS
069200930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
069300930714     C     XMSGPA        PLIST
069400930714     C                   PARM                    DSMSMS
069500930714     C                   PARM                    DSMSPG
069600930714     C                   PARM                    DSMSKE
069700930714     C                   PARM                    DSMSER
069800930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
069900000725     C                   MOVEL     'TRTB83R'     DSMSPG
070000000725     C                   MOVEL     'TRTB83R'     VMCQUE
070100930806     C                   MOVEL     *BLANKS       VMCKEY
070200930714     C                   MOVEL     *BLANKS       DSMSMS
070300930805     C* INIZIALIZZO FLAG CONTROLLO PGM
070400930721     C                   MOVEL     '0'           $ERR              1
070500930721     C                   MOVEL     '0'           $F03              1
070600930806     C                   MOVEL     *BLANKS       $GES              2
070700930721     C                   MOVEL     *BLANKS       $MSG             80
070800930721     C                   MOVEL     '0'           $FINE             1
070900930721     C                   MOVEL     '0'           $TESVD            1
071000940112     C                   MOVEL     'G'           $TGES             1
071100020724     C*
071200020724     C                   CALL      'X§PARUT'
071300020724     C                   PARM                    UT§DSE
071400020724     C                   MOVEL     REC80         CNCR80
071500930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
071600930721     C                   BITOFF    '01234567'    POSMI1            1
071700930721     C                   BITOFF    '01234567'    POSMI2            1
071800930721     C                   BITOFF    '01234567'    POSMA1            1
071900930721     C                   BITOFF    '01234567'    POSMA2            1
072000930721     C                   BITON     '34'          POSMI1            1
072100930721     C                   BITON     '7'           POSMI2            1
072200930721     C                   BITON     '34'          POSMA1            1
072300930721     C                   BITON     '0'           POSMA2            1
072400930721     C                   MOVE      POSMI2        POSMIN            2
072500930721     C                   MOVEL     POSMI1        POSMIN
072600930721     C                   MOVE      POSMA2        POSMAX            2
072700930721     C                   MOVEL     POSMA1        POSMAX
072800930714     C                   ENDSR
072900940111     C*------------------------------------------------------------------------*
073000930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
073100940111     C*------------------------------------------------------------------------*
073200930804     C     FUNSTD        BEGSR
073300930804     C                   SELECT
073400930804     C     KEY           WHENEQ    F3
073500930804     C                   EXSR      F03STD
073600930804     C     KEY           WHENEQ    F12
073700930804     C                   EXSR      F12STD
073800930804     C     KEY           WHENEQ    F24
073900930804     C                   EXSR      F24STD
074000930804     C                   OTHER
074100930804     C                   MOVEL     MSG(2)        $MSG
074200930804     C                   EXSR      CARMSG
074300930804     C                   ENDSL
074400930804     C                   ENDSR
074500940111     C*------------------------------------------------------------------------*
074600930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
074700940111     C*------------------------------------------------------------------------*
074800930804     C     F03STD        BEGSR
074900930804     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
075000930923     C                   MOVEL     '1'           $F03
075100930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
075200930804     C                   MOVEL     '1'           $FINE
075300930804     C                   ENDSR
075400940111     C*------------------------------------------------------------------------*
075500930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
075600940111     C*------------------------------------------------------------------------*
075700930804     C     F12STD        BEGSR
075800930805     C* IMPOSTO FLAG DI FIN PGM
075900930804     C                   MOVEL     '1'           $FINE
076000930804     C                   ENDSR
076100940111     C*------------------------------------------------------------------------*
076200930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
076300940111     C*------------------------------------------------------------------------*
076400930804     C     F24STD        BEGSR
076500930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
076600930804     C                   EXSR      OUTFUN
076700930804     C                   ENDSR
076800940111     C*------------------------------------------------------------------------*
076900930804     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
077000940111     C*------------------------------------------------------------------------*
077100930804     C     INZSFL        BEGSR
077200930804     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
077300930806     C     *LIKE         DEFINE    VCCNBR        SFLPAG
077400930806     C     *LIKE         DEFINE    VCCNBR        SFLEOF
077500930806     C     *LIKE         DEFINE    VCCNBR        SFLERR
077600930806     C     *LIKE         DEFINE    VCCNBR        SFLULT
077700930806     C     *LIKE         DEFINE    VCCNBR        WFLULT
077800930806     C     *LIKE         DEFINE    VCCNBR        SFLRIG
077900930804     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
078000930804     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
078100101119     C                   Z-ADD     11            SFLPAG
078200930804     C                   ENDSR
078300940111     C*------------------------------------------------------------------------*
078400930804     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
078500940111     C*------------------------------------------------------------------------*
078600930804     C     FUNSFL        BEGSR
078700930804     C                   SELECT
078800930804     C     KEY           WHENEQ    ENTER
078900930804     C                   EXSR      ENTSFL
079000930805     C     KEY           WHENEQ    F3
079100930805     C                   EXSR      F03STD
079200930804     C     KEY           WHENEQ    F5
079300930804     C                   EXSR      F05STD
079400930805     C     KEY           WHENEQ    F12
079500930805     C                   EXSR      F12STD
079600930804     C     KEY           WHENEQ    F17
079700930804     C                   EXSR      F17STD
079800930804     C     KEY           WHENEQ    F18
079900930804     C                   EXSR      F18STD
080000930804     C     KEY           WHENEQ    F23
080100930804     C                   EXSR      F23STD
080200930805     C     KEY           WHENEQ    F24
080300930805     C                   EXSR      F24STD
080400930804     C     KEY           WHENEQ    ROLLUP
080500930804     C                   EXSR      RUPSTD
080600930804     C     KEY           WHENEQ    ROLLDN
080700930804     C                   EXSR      RDNSTD
080800930804     C                   OTHER
080900930804     C                   MOVEL     MSG(2)        $MSG
081000930804     C                   EXSR      CARMSG
081100930804     C                   ENDSL
081200930804     C                   ENDSR
081300940111     C*------------------------------------------------------------------------*
081400930804     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
081500940111     C*------------------------------------------------------------------------*
081600930804     C     F05STD        BEGSR
081700930804     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
081800930804     C                   EXSR      POSSFL
081900930804     C                   ENDSR
082000940111     C*------------------------------------------------------------------------*
082100930804     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
082200940111     C*------------------------------------------------------------------------*
082300930804     C     F17STD        BEGSR
082400930804     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
082500930806     C                   Z-ADD     1             VCCNBR
082600930804     C                   ENDSR
082700940111     C*------------------------------------------------------------------------*
082800930804     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
082900940111     C*------------------------------------------------------------------------*
083000930804     C     F18STD        BEGSR
083100930804     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
083200930806     C                   Z-ADD     SFLULT        VCCNBR
083300930804     C                   ENDSR
083400940111     C*------------------------------------------------------------------------*
083500930804     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
083600940111     C*------------------------------------------------------------------------*
083700930804     C     F23STD        BEGSR
083800930804     C* GESTIONE VISUALIZZAZIONE OPZIONI
083900930804     C                   EXSR      OUTOPZ
084000930804     C                   ENDSR
084100940111     C*------------------------------------------------------------------------*
084200930804     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
084300940111     C*------------------------------------------------------------------------*
084400930804     C     RUPSTD        BEGSR
084500930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
084600930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
084700930805     C     CURSOR        IFGE      POSMIN
084800930805     C     CURSOR        ANDLE     POSMAX
084900930804     C                   ELSE
085000930805     C                   EXSR      CARSFL
085100930805     C                   ENDIF
085200930804     C                   ENDSR
085300940111     C*------------------------------------------------------------------------*
085400930804     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
085500940111     C*------------------------------------------------------------------------*
085600930804     C     RDNSTD        BEGSR
085700930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
085800930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
085900930804     C     CURSOR        IFGE      POSMIN
086000930804     C     CURSOR        ANDLE     POSMAX
086100930804     C                   ELSE
086200930804     C* INVIO MESSAGGIO DI INIZIO ELENCO
086300930804     C                   MOVEL     '1'           $ERR
086400930804     C                   MOVEL     MSG(5)        $MSG
086500930804     C                   EXSR      CARMSG
086600930805     C                   ENDIF
086700930804     C                   ENDSR
086800940111     C*------------------------------------------------------------------------*
086900930804     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
087000940111     C*------------------------------------------------------------------------*
087100930804     C     ENTSFL        BEGSR
087200930804     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
087300930804     C                   EXSR      NEWPOS
087400930804     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
087500930804     C     $ERR          IFEQ      '0'
087600930804     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
087700930804     C     SWPOSI        IFNE      0
087800930804     C                   EXSR      POSSFL
087900930804     C                   ELSE
088000930804     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
088100930804     C     SFLULT        IFNE      0
088200930804     C                   EXSR      OPZSFL
088300930804     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
088400930804     C     SWMODI        IFNE      0
088500930804     C                   EXSR      POSSFL
088600930805     C                   ENDIF
088700930805     C                   ENDIF
088800930805     C                   ENDIF
088900930805     C                   ENDIF
089000930804     C                   ENDSR
089100940111     C*------------------------------------------------------------------------*
089200940111     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
089300940111     C*------------------------------------------------------------------------*
089400930804     C     POSSFL        BEGSR
089500930804     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
089600930804     C                   EXSR      CLRSFL
089700930804     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
089800000711     C                   EXSR      SETCLI
089900930804     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
090000930804     C     SFLEOF        IFEQ      0
090100930804     C                   EXSR      CARSFL
090200930805     C                   ENDIF
090300930804     C                   ENDSR
090400940111     C*------------------------------------------------------------------------*
090500930804     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
090600940111     C*------------------------------------------------------------------------*
090700930804     C     OPZSFL        BEGSR
090800930802     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
090900930802     C                   MOVEL     'S'           OPZOK             1
091000000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
091100000000     C                   Z-ADD     0             SFLVAR            5 0
091200000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
091300000000     C                   Z-ADD     0             SFLERR
091400000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
091500000000     C                   Z-ADD     SFLULT        WFLULT
091600000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
091700000725     C                   READC     TB83DVS                                49
091800930806     C     *IN49         DOWEQ     '0'
091900000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
092000000000     C                   ADD       1             SFLVAR
092100930802     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
092200930805     C     OPZOK         IFEQ      'S'
092300930802     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
092400930806     C     VSCOPZ        IFNE      *BLANKS
092500930802     C                   EXSR      CONOPZ
092600930802     C     OPZOK         IFEQ      'S'
092700930802     C                   EXSR      GESOPZ
092800930805     C                   ENDIF
092900930805     C                   ENDIF
093000000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
093100000000     C*            2. PULIZIA SCELTA IN SFL
093200930805     C     OPZOK         IFEQ      'S'
093300000000     C                   MOVEL     '0'           *IN41
093400930806     C                   MOVEL     *BLANKS       VSCOPZ
093500930805     C                   ELSE
093600000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
093700000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
093800000000     C                   MOVEL     '1'           *IN41
093900000000     C                   Z-ADD     SFLULT        SFLERR
094000930805     C                   ENDIF
094100930805     C                   ELSE
094200000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
094300000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
094400000000     C                   MOVEL     '1'           *IN41
094500930805     C                   ENDIF
094600000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
094700000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
094800000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
094900930803     C     OPZOK         IFNE      'S'
095000930806     C                   Z-ADD     SFLERR        VCCNBR
095100000000     C                   ELSE
095200930806     C                   Z-ADD     SFLULT        VCCNBR
095300930805     C                   ENDIF
095400000000     C* AGGIORNO RECORD SFL
095500000725     C                   UPDATE    TB83DVS
095600000725     C                   READC     TB83DVS                                49
095700930805     C                   ENDDO
095800000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
095900000000     C                   Z-ADD     WFLULT        SFLULT
096000000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
096100000719     C*          SFLVAR    IFEQ 0
096200000719     C*                    MOVEL'1'       $FINE
096300000719     C*                    ENDIF
096400000000     C                   ENDSR
096500940111     C*------------------------------------------------------------------------*
096600930804     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
096700940111     C*------------------------------------------------------------------------*
096800000000     C     CLRSFL        BEGSR
096900000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
097000000000     C* (0=NON FINE FILE  1=FINE FILE)
097100000000     C                   Z-ADD     0             SFLEOF
097200000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
097300000000     C                   Z-ADD     0             SFLULT
097400000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
097500000000     C                   MOVEL     '1'           *IN40
097600000725     C                   WRITE     TB83DVC
097700000000     C                   MOVEL     '0'           *IN40
097800000000     C                   ENDSR
097900940111     C*------------------------------------------------------------------------*
098000000000     C* CARSFL - CARICAMENTO SFL
098100940111     C*------------------------------------------------------------------------*
098200000000     C     CARSFL        BEGSR
098300000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
098400000000     C     SFLULT        IFEQ      0
098500000711     C                   EXSR      SETCLI
098600930805     C                   ENDIF
098700000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
098800000000     C                   Z-ADD     SFLULT        WFLULT
098900000000     C* CICLO:
099000000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
099100000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
099200000000     C                   Z-ADD     1             SFLRIG
099300930805     C     SFLRIG        DOWLE     SFLPAG
099400000000     C     SFLEOF        ANDEQ     0
099500000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
099600930806     C                   MOVEL     *BLANKS       VSCOPZ
099700000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
099800000000     C                   EXSR      IRESFL
099900000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
100000000000     C                   ADD       1             SFLULT
100100000000     C* SCRITTURA RECORD SFL
100200000725     C                   WRITE     TB83DVS
100300000000     C                   ADD       1             SFLRIG
100400000000     C* LETTURA FILE DA CARICARE
100500000711     C                   EXSR      LETCLI
100600930805     C                   ENDDO
100700000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
100800000000     C     WFLULT        IFNE      SFLULT
100900930806     C                   Z-ADD     WFLULT        VCCNBR
101000930806     C                   ADD       1             VCCNBR
101100930805     C                   ENDIF
101200930720     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
101300930805     C     SFLEOF        IFEQ      1
101400930721     C     SFLULT        ANDGT     *ZEROS
101500930721     C                   MOVEL     '1'           $ERR
101600930804     C                   MOVEL     MSG(6)        $MSG
101700930721     C                   EXSR      CARMSG
101800930804     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
101900000725     C     SFLULT        CHAIN     TB83DVS                            99
102000930804     C     *IN99         IFEQ      '0'
102100930804     C                   SETON                                        45
102200000725     C                   UPDATE    TB83DVS
102300930804     C                   SETOFF                                       45
102400930805     C                   ENDIF
102500930805     C                   ENDIF
102600000000     C                   ENDSR
102700940111     C*------------------------------------------------------------------------*
102800930804     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
102900940111     C*------------------------------------------------------------------------*
103000000000     C     OUTSFL        BEGSR
103100930805     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
103200930721     C     MSGULT        IFNE      *ZEROS
103300930721     C                   EXSR      OUTMSG
103400930805     C                   ENDIF
103500000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
103600000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
103700000000     C     SFLEOF        IFEQ      0
103800000000     C                   MOVEL     '0'           *IN42
103900000000     C                   ELSE
104000000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
104100000000     C                   MOVEL     '1'           *IN42
104200930805     C                   ENDIF
104300930720     C* EMISSIONE SUBFILE
104400000000     C     SFLULT        IFLE      0
104500930720     C*  --> CASO DI SFL VUOTO
104600930720     C                   MOVEL     '0'           *IN43
104700000725     C                   WRITE     TB83DVV
104800000725     C                   WRITE     TB83DVC
104900000725     C                   READ      TB83DVC                                99
105000000000     C                   ELSE
105100930720     C*  --> CASO DI SFL CON RECORDS
105200000725     C                   WRITE     TB83DVC
105300000725     C                   READ      TB83DVC                                99
105400930805     C                   ENDIF
105500930720     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
105600930720     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
105700930806     C                   Z-ADD     FRTSFL        VCCNBR
105800000000     C                   ENDSR
105900940111     C*------------------------------------------------------------------------*
106000930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
106100930721     C*          CODA MESSAGGI DEL PGM
106200940111     C*------------------------------------------------------------------------*
106300930721     C     CARMSG        BEGSR
106400930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
106500930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
106600930721     C     MSGULT        IFEQ      0
106700930721     C                   MOVEL     '1'           *IN40
106800000725     C                   WRITE     TB83DVK
106900930721     C                   MOVEL     '0'           *IN40
107000930805     C                   ENDIF
107100930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
107200930721     C                   MOVEL     $MSG          DSMSMS
107300930721     C                   CALL      'XMSG'        XMSGPA
107400930721     C     DSMSER        IFEQ      0
107500930806     C                   MOVEL     DSMSKE        VMCKEY
107600930805     C                   ENDIF
107700930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
107800931015     C                   ADD       1             MSGULT            4 0
107900000725     C                   WRITE     TB83DVM
108000930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
108100930721     C                   MOVEL     *BLANKS       $MSG
108200930721     C                   ENDSR
108300940111     C*------------------------------------------------------------------------*
108400930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
108500940111     C*------------------------------------------------------------------------*
108600930721     C     OUTMSG        BEGSR
108700930721     C* VISUALIZZAZIONE MESSAGGIO
108800930721     C                   MOVEL     '1'           *IN44
108900000725     C                   WRITE     TB83DVK
109000930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
109100930721     C                   Z-ADD     *ZEROS        MSGULT
109200930721     C                   ENDSR
109300940111     C*------------------------------------------------------------------------*
109400930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
109500930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
109600930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
109700930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
109800930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
109900940111     C*------------------------------------------------------------------------*
110000930803     C     CARFUN        BEGSR
110100930803     C                   Z-ADD     *ZEROS        FUNULT            5 0
110200931015     C     1             DO        31            I                 5 0
110300930803     C                   MOVEL     FUN(I)        FUNRIG
110400930805     C                   MOVEL     FUNFUN        FFU(I)
110500930805     C                   MOVEL     FUNDES        FDE(I)
110600020724     C     SIMTPP        IFEQ      'S'
110700930923     C     $TGES         IFEQ      'G'
110800930805     C                   MOVEL     FUNASG        FAT(I)
110900930803     C                   ELSE
111000930803     C                   MOVEL     FUNASI        FAT(I)
111100930803     C                   ENDIF
111200930803     C                   ELSE
111300930923     C     $TGES         IFEQ      'G'
111400930803     C                   MOVEL     FUNAFG        FAT(I)
111500930803     C                   ELSE
111600930803     C                   MOVEL     FUNAFI        FAT(I)
111700930803     C                   ENDIF
111800930803     C                   ENDIF
111900930806     C     FAT(I)        IFEQ      'E'
112000930806     C                   MOVEL     USER          WEDP              3
112100930806     C     WEDP          IFEQ      'EDP'
112200930806     C                   MOVEL     'S'           FAT(I)
112300930806     C                   ELSE
112400930806     C                   MOVEL     'N'           FAT(I)
112500930806     C                   ENDIF
112600930806     C                   ENDIF
112700930803     C                   ENDDO
112800930803     C                   ENDSR
112900940111     C*------------------------------------------------------------------------*
113000930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
113100930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
113200930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
113300930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
113400940111     C*------------------------------------------------------------------------*
113500930803     C     OUTFUN        BEGSR
113600930805     C     1             DO        8             I
113700930805     C                   MOVEL     *BLANKS       FUV(I)
113800930803     C                   ENDDO
113900930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
114000930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0
114100930803     C     1             DO        31            I
114200930803     C     FAT(I)        IFEQ      'S'
114300930803     C     FDE(I)        ANDNE     *BLANKS
114400930803     C                   ADD       1             FUNNAT
114500930803     C                   ENDIF
114600930803     C                   ENDDO
114700930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
114800930803     C     FUNNAT        IFGT      8
114900930803     C                   MOVEL     'S'           FAT(24)
115000930803     C                   ENDIF
115100930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
115200930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
115300930803     C                   Z-ADD     8             FUNVIS            5 0
115400930803     C     FAT(23)       IFEQ      'S'
115500930803     C                   SUB       1             FUNVIS
115600930803     C                   ENDIF
115700930803     C     FAT(24)       IFEQ      'S'
115800930803     C                   SUB       1             FUNVIS
115900930803     C                   ENDIF
116000930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
116100931015     C                   Z-ADD     *ZEROS        W                 5 0
116200930803     C                   ADD       1             FUNULT
116300930803     C     FUNULT        IFGT      31
116400930803     C                   Z-ADD     1             FUNULT
116500930805     C                   ENDIF
116600930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
116700930803     C     FUNULT        DO        31            I
116800930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
116900930803     C     W             IFLT      FUNVIS
117000930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
117100930803     C     FAT(I)        IFEQ      'S'
117200930803     C     FDE(I)        ANDNE     *BLANKS
117300930805     C     I             IFEQ      23
117400930805     C     I             OREQ      24
117500930803     C                   ELSE
117600930803     C                   ADD       1             W
117700930803     C                   MOVEL     FDE(I)        FUV(W)
117800930803     C                   ENDIF
117900930803     C                   ENDIF
118000930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
118100930803     C                   Z-ADD     I             FUNULT
118200930803     C                   ENDIF
118300930803     C                   ENDDO
118400930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
118500930806     C                   MOVEL     FUV(1)        VFD001
118600930806     C                   MOVEL     FUV(2)        VFD002
118700930806     C                   MOVEL     FUV(3)        VFD003
118800930806     C                   MOVEL     FUV(4)        VFD004
118900930806     C                   MOVEL     FUV(5)        VFD005
119000930806     C                   MOVEL     FUV(6)        VFD006
119100930803     C     FAT(23)       IFEQ      'N'
119200930803     C     FAT(24)       ANDEQ     'N'
119300930806     C                   MOVEL     FUV(7)        VFD007
119400930806     C                   MOVEL     FUV(8)        VFD008
119500930803     C                   ELSE
119600930803     C     FAT(23)       IFEQ      'S'
119700930803     C     FAT(24)       ANDEQ     'S'
119800930806     C                   MOVEL     FDE(23)       VFD007
119900930806     C                   MOVEL     FDE(24)       VFD008
120000930803     C                   ELSE
120100930806     C                   MOVEL     FUV(7)        VFD007
120200930803     C     FAT(23)       IFEQ      'S'
120300930806     C                   MOVEL     FDE(23)       VFD008
120400930803     C                   ELSE
120500930806     C                   MOVEL     FDE(24)       VFD008
120600930805     C                   ENDIF
120700930805     C                   ENDIF
120800930805     C                   ENDIF
120900930803     C                   MOVEL     '1'           $TESVD
121000930803     C                   ENDSR
121100940111     C*------------------------------------------------------------------------*
121200930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
121300930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
121400930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
121500930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
121600940111     C*------------------------------------------------------------------------*
121700930803     C     CONFUN        BEGSR
121800930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
121900930803     C     1             DO        31            I
122000930803     C     KEY           IFEQ      KEYS(I)
122100930805     C* CONTROLLO SE IL TASTO E' ATTIVO
122200930803     C     FAT(I)        IFNE      'S'
122300930804     C                   MOVEL     MSG(1)        $MSG
122400930803     C                   EXSR      CARMSG
122500930803     C                   MOVEL     '1'           $ERR
122600930803     C                   ENDIF
122700930803     C                   ENDIF
122800930803     C                   ENDDO
122900930803     C                   ENDSR
123000940111     C*------------------------------------------------------------------------*
123100930802     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
123200930802     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
123300930803     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
123400930803     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
123500930803     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
123600930803     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
123700940111     C*------------------------------------------------------------------------*
123800930802     C     CAROPZ        BEGSR
123900930803     C                   Z-ADD     *ZEROS        OPZULT            5 0
124000930802     C     1             DO        24            I
124100930803     C                   MOVEL     OPZ(I)        OPZRIG
124200930805     C                   MOVEL     OPZOPD        OPD(I)
124300930805     C                   MOVEL     OPZOPS        OPS(I)
124400930805     C                   MOVEL     OPZDES        ODE(I)
124500020724     C     SIMTPP        IFEQ      'S'
124600930923     C     $TGES         IFEQ      'G'
124700930805     C                   MOVEL     OPZASG        OAT(I)
124800930802     C                   ELSE
124900930805     C                   MOVEL     OPZASI        OAT(I)
125000930802     C                   ENDIF
125100930802     C                   ELSE
125200930923     C     $TGES         IFEQ      'G'
125300930805     C                   MOVEL     OPZAFG        OAT(I)
125400930802     C                   ELSE
125500930805     C                   MOVEL     OPZAFI        OAT(I)
125600930802     C                   ENDIF
125700930802     C                   ENDIF
125800930806     C     OAT(I)        IFEQ      'E'
125900930806     C                   MOVEL     USER          WEDP              3
126000930806     C     WEDP          IFEQ      'EDP'
126100930806     C                   MOVEL     'S'           OAT(I)
126200930806     C                   ELSE
126300930806     C                   MOVEL     'N'           OAT(I)
126400930806     C                   ENDIF
126500930806     C                   ENDIF
126600930802     C                   ENDDO
126700930802     C                   ENDSR
126800940111     C*------------------------------------------------------------------------*
126900930802     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
127000930802     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
127100930802     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
127200930806     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
127300930802     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
127400940111     C*------------------------------------------------------------------------*
127500930802     C     OUTOPZ        BEGSR
127600930805     C     1             DO        8             I
127700930805     C                   MOVEL     *BLANKS       OPV(I)
127800930802     C                   ENDDO
127900930802     C                   Z-ADD     *ZEROS        OPZNAT            2 0
128000930805     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
128100930803     C                   ADD       1             OPZULT
128200930803     C     OPZULT        IFGT      24
128300930803     C                   Z-ADD     1             OPZULT
128400930803     C                   END
128500930805     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
128600930802     C     1             DO        24            I
128700930805     C     OAT(I)        IFEQ      'S'
128800930805     C     ODE(I)        ANDNE     *BLANKS
128900930802     C                   ADD       1             OPZNAT
129000930802     C                   ENDIF
129100930802     C                   ENDDO
129200930802     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
129300931015     C                   Z-ADD     0             W
129400930802     C     OPZULT        DO        24            I
129500930802     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
129600930802     C     W             IFLT      8
129700930802     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
129800930805     C     OAT(I)        IFEQ      'S'
129900930803     C     ODE(I)        ANDNE     *BLANKS
130000930802     C                   ADD       1             W
130100930803     C                   MOVEL     ODE(I)        OPV(W)
130200930805     C                   ENDIF
130300930805     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
130400930802     C                   Z-ADD     I             OPZULT
130500930802     C                   ENDIF
130600930802     C                   ENDDO
130700930802     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
130800930806     C                   MOVEL     OPV(1)        VOD001
130900930806     C                   MOVEL     OPV(2)        VOD002
131000930806     C                   MOVEL     OPV(3)        VOD003
131100930806     C                   MOVEL     OPV(4)        VOD004
131200930806     C                   MOVEL     OPV(5)        VOD005
131300930806     C                   MOVEL     OPV(6)        VOD006
131400930806     C                   MOVEL     OPV(7)        VOD007
131500930806     C                   MOVEL     OPV(8)        VOD008
131600930802     C                   MOVEL     '1'           $TESVD
131700930923     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
131800930923     C     OPZNAT        IFGT      8
131900930923     C                   MOVEL     'S'           FAT(23)
132000930923     C                   ENDIF
132100930802     C                   ENDSR
132200940111     C*------------------------------------------------------------------------*
132300930802     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
132400930806     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
132500930802     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
132600930802     C*                   - ERRORE SE OPZIONE NON ATTIVA
132700940111     C*------------------------------------------------------------------------*
132800930802     C     CONOPZ        BEGSR
132900000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
133000930803     C                   MOVEL     *BLANKS       OPZOK             1
133100930805     C     1             DO        24            I                 5 0
133200930806     C     VSCOPZ        IFEQ      OPD(I)
133300930806     C     VSCOPZ        OREQ      OPS(I)
133400930805     C     OAT(I)        IFEQ      'S'
133500930806     C                   MOVEL     OPD(I)        VSCOPZ
133600940105     C                   MOVEL     'S'           OPZOK
133700930805     C                   ELSE
133800930802     C                   MOVEL     'N'           OPZOK             1
133900930805     C                   ENDIF
134000930802     C                   ENDIF
134100930802     C                   ENDDO
134200930805     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
134300930803     C     OPZOK         IFNE      'S'
134400930804     C                   MOVEL     MSG(3)        $MSG
134500930803     C                   EXSR      CARMSG
134600930803     C                   MOVEL     '1'           $ERR
134700930803     C                   MOVEL     'N'           OPZOK             1
134800930803     C                   ENDIF
134900000000     C                   ENDSR
135000930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
135100000725TRTB83R- Tasto funzionale non valido.                                          1
135200000725TRTB83R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
135300000725TRTB83R- Opzione non valida.                                                   3
135400000725TRTB83R- Opzione abilitata ma non gestita. Avvertire CED.                      4
135500000725TRTB83R- Inizio elenco.                                                        5
135600000725TRTB83R- Fine elenco.                                                          6
135700000725TRTB83R- 7                                                                     7
135800000725TRTB83R- 8                                                                     8
135900000725TRTB83R- 9                                                                     9
136000000725TRTB83R- 10                                                                   10
136100000725TRTB83R- 11                                                                   11
136200000725TRTB83R- 12                                                                   12
136300000725TRTB83R- 13                                                                   13
136400000725TRTB83R- 14                                                                   14
136500000725TRTB83R- 15                                                                   15
136600000725TRTB83R- 16                                                                   16
136700000725TRTB83R- 17                                                                   17
136800000725TRTB83R- 18                                                                   18
136900000725TRTB83R- 19                                                                   19
137000000725TRTB83R- 20                                                                   20
137100930729** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
137200930803- 1-1 -                   -    NN     NN
137300940112- 2-2 -2=Modifica         -    SS     SS
137400000711- 3-3 -3=COPIA            -    SS     SS
137500940112- 4-4 -4=Cancellazione    -    SS     SS
137600940121- 5-5 -5=Visualizzazione  -    SS     SS
137700930803- 6-6 -                   -    NN     NN
137800940112- 7-7 -7=Ripristino       -    SS     SS
137900940104- 8-8 -                   -    NN     NN
138000940111- 9-9 -                   -    NN     NN
138100930806-10-  -                   -    NN     NN
138200930806-11-  -                   -    NN     NN
138300930806-12-  -                   -    NN     NN
138400930803-13-  -                   -    NN     NN
138500930803-14-  -                   -    NN     NN
138600930803-15-  -                   -    NN     NN
138700930803-16-  -                   -    NN     NN
138800930803-17-  -                   -    NN     NN
138900930803-18-  -                   -    NN     NN
139000930803-19-  -                   -    NN     NN
139100930803-20-  -                   -    NN     NN
139200930803-21-  -                   -    NN     NN
139300930803-22-  -                   -    NN     NN
139400930803-23-  -                   -    NN     NN
139500930803-24-  -                   -    NN     NN
139600930730** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
139700930723F1    -                   -    NN     NN
139800930723F2    -                   -    NN     NN
139900930802F3    -F3=Fine            -    SS     SS
140000930723F4    -                   -    NN     NN
140100940112F5    -F5=Rivisualizzaz.  -    SS     SS
140200940112F6    -F6=Inserimento     -    SS     SS
140300930723F7    -                   -    NN     NN
140400930723F8    -                   -    NN     NN
140500930723F9    -                   -    NN     NN
140600930723F10   -                   -    NN     NN
140700930723F11   -                   -    NN     NN
140800930802F12   -F12=Ritorno        -    SS     SS
140900930806F13   -                   -    NN     NN
141000930806F14   -                   -    NN     NN
141100930723F15   -                   -    NN     NN
141200930723F16   -                   -    NN     NN
141300930802F17   -F17=Inizio dati    -    SS     SS
141400930802F18   -F18=Fine dati      -    SS     SS
141500930723F19   -                   -    NN     NN
141600930723F20   -                   -    NN     NN
141700940111F21   -                   -    NN     NN
141800940111F22   -                   -    NN     NN
141900930803F23   -F23=Altre opzioni  -    NN     NN
142000930803F24   -F24=Altri tasti    -    NN     NN
142100930802ENTER -                   -    SS     SS
142200930802ROLLUP-                   -    SS     SS
142300930802ROLLDN-                   -    SS     SS
142400930802HELP  -                   -    NN     NN
142500930802PRINT -                   -    NN     NN
142600930802BSPACE-                   -    NN     NN
142700930802CLEAR -                   -    NN     NN
