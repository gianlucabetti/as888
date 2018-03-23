000100940111     H*------------------------------------------------------------------------*
000200940114     H* GESTIONE TABELLA TEMPI CALCOLO DELIVERY - SUBFILE
000300940111     H*------------------------------------------------------------------------*
000400940119     H DECEDIT('0,') DATEDIT(*DMY.)
000500940111     F*------------------------------------------------------------------------*
000600940111     F* DATA BASE
000700940111     F*------------------------------------------------------------------------*
000800940111     FTRTB50D   CF   E             WORKSTN INFDS(DEVDS1)
000900940111     F                                     SFILE(TB50DVS:SFLULT)
001000940111     F                                     SFILE(TB50DVM:MSGULT)
001100930310     FTABEL00F  IF   E           K DISK
001200061220     Ftntbe01l  IF   E           K DISK
001300940114     FAZORG01L  IF   E           K DISK
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
005400020923     D* DS REPERIMENTO DATI UTENTE
005500020923     D*-------------------
005600061218     D TIBS34DS      E DS
005700020923     D DDATIUTE      E DS
005800020923     D AZUTEDS       E DS                  extname(AZUTE00F)
005900940111     D*---------------------
006000000000     D* DS PER GESTIONE TASTI FUNZIONALI
006100940111     D*---------------------
006200930802     D DSKEYS        E DS
006300930803     D  KEYRIG                 1     40
006400940111     D*---------------------
006500930806     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006600940111     D*---------------------
006700930806     D                SDS
006800940111     D  USER                 254    263
006900940111     D*---------------------
007000930720     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007100940111     D*---------------------
007200000000     D DEVDS1          DS
007300000000     D  KEY                  369    369
007400930721     D  CURSOR               370    371
007500930720     D  FRTSFL               378    379B 0
007600940111     D*---------------------
007700940111     D* DS TEMPI FISSI CALCOLO DELIVERY
007800940111     D*---------------------
007900940111     D DS1X          E DS
008000940111     D*---------------------
008100940111     D* DS ESTERNA DI PROCEDURA
008200940111     D*---------------------
008300940111     D DS50          E DS                  EXTNAME(TRTB50DS)
008400061218     D* DS PER TIBS02R - GESTIONE TNTBE00F
008500061218     D DSBS02        E DS                  EXTNAME(TIBS02DS)
008600061218     d
008700061218     D trul09ds      E DS
008800980130     D*------------------
008900980130     D* DS CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
009000980130     D*------------------
009100980130     D UL12DS        E DS                  EXTNAME(TRUL12DS)
009200940111     D*---------------------
009300940111     D* ARCHITETTURA
009400940111     D*---------------------
009500940111     D KPJBA         E DS
009600061220     d
009700061220     dkcod             s                   like(tbecod) inz('SOT')
009800061220     dkke1             s                   like(tbeke1)
009900061220     dkke2             s                   like(tbeke2) inz('        ')
009901130520     dwost             s                   like(§1xhho)
010000940111     C*------------------------------------------------------------------------*
010100940111     C* MAIN LINE
010200940111     C*------------------------------------------------------------------------*
010300000000     C*
010400000000     C* CICLO FINO A CHE:
010500940111DO  1C     $FINE         DOUEQ     '1'
010600000000     C*
010700930805     C                   MOVEL     '0'           $ERR
010800000000     C*
010900930721     C* CONTROLLA GESTIONE VIDEO
011000940111IF  2C     $GES          IFEQ      'VS'
011100930806     C                   EXSR      GESVS
011200940111E   2C                   ENDIF
011300000000     C*
011400940111E   1C                   ENDDO
011500940111     C* FINE PGM
011600000000     C                   SETON                                        LR
011700940111     C*------------------------------------------------------------------------*
011800930806     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
011900940111     C*------------------------------------------------------------------------*
012000930806     C     CARVS         BEGSR
012100930713     C*
012200930806     C                   MOVEL     'VS'          $GES
012300930713     C*
012400930804     C                   EXSR      CAROPZ
012500930804     C                   EXSR      OUTOPZ
012600930804     C                   MOVEA     FUN2          FUN
012700930804     C                   EXSR      CARFUN
012800930804     C                   EXSR      OUTFUN
012900930713     C*
013000940111     C                   WRITE     TB50DVT
013100940111     C                   WRITE     TB50DVO
013200940111     C                   WRITE     TB50DVF
013300930713     C*
013400930806     C                   EXSR      POSSFL
013500930713     C*
013600930713     C                   ENDSR
013700940111     C*------------------------------------------------------------------------*
013800930806     C* GESVS  - GESTIONE SUBFILE
013900940111     C*------------------------------------------------------------------------*
014000930806     C     GESVS         BEGSR
014100930714     C*
014200940111IF  1C     $TESVD        IFEQ      '1'
014300940111     C                   WRITE     TB50DVT
014400940111     C                   WRITE     TB50DVO
014500940111     C                   WRITE     TB50DVF
014600940111E   1C                   ENDIF
014700930713     C*
014800930805     C                   EXSR      OUTSFL
014900930713     C*
015000930805     C                   EXSR      CONFUN
015100930805     C*
015200940111IF  1C     $ERR          IFEQ      '0'
015300940111SL  2C                   SELECT
015400930804     C     KEY           WHENEQ    F6
015500930804     C                   EXSR      F06GES
015600940111     C                   OTHER
015700930804     C                   EXSR      FUNSFL
015800940111E   2C                   ENDSL
015900940111E   1C                   ENDIF
016000930713     C*
016100930713     C                   ENDSR
016200940111     C*------------------------------------------------------------------------*
016300930804     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
016400940111     C*------------------------------------------------------------------------*
016500930126     C     F06GES        BEGSR
016600930127     C*
016700940111     C* LANCIO L'INSERIMENTO
016800940114     C                   MOVEL     *ZEROS        WLNP              3
016900940121     C                   Z-ADD     DEPHHO        WHH1              4 0
017000940121     C                   Z-ADD     DE1HHO        WHH2              4 0
017100061220     C                   Z-ADD     DEpsot        Wsot              4 0
017101130520     C                   Z-ADD     DEthho        Wost
017200061220     c                   clear                   watb              1
017300940114     C                   MOVEL     'GES'         WOP0              3
017400940114     C                   MOVEL     'F06'         WOP1              3
017500940111     C                   MOVEL     'TRTB51R'     WPGM              8
017600930805     C                   EXSR      LANPGM
017700930128     C*
017800930805     C                   EXSR      POSSFL
017900930127     C*
018000000000     C                   ENDSR
018100940111     C*------------------------------------------------------------------------*
018200930803     C*  GESOPZ - GESTIONE OPZIONI
018300940111     C*------------------------------------------------------------------------*
018400930803     C     GESOPZ        BEGSR
018500930804     C*
018600940111SL  1C                   SELECT
018700930806     C     VSCOPZ        WHENEQ    ' 2'
018800930804     C                   EXSR      O02GES
018900930806     C     VSCOPZ        WHENEQ    ' 4'
019000930804     C                   EXSR      O04GES
019100940121     C     VSCOPZ        WHENEQ    ' 5'
019200940121     C                   EXSR      O05GES
019300930806     C     VSCOPZ        WHENEQ    ' 7'
019400930804     C                   EXSR      O07GES
019500930804     C                   OTHER
019600930804     C                   MOVEL     MSG(4)        $MSG
019700930805     C                   EXSR      CARMSG
019800940111E   1C                   ENDSL
019900930804     C*
020000000000     C                   ENDSR
020100940111     C*------------------------------------------------------------------------*
020200000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
020300940111     C*------------------------------------------------------------------------*
020400000000     C     O02GES        BEGSR
020500930128     C*
020600940111IF  1C     VSCATB        IFEQ      'A'
020700940117     C                   MOVEL     '1'           $ERR
020800940117     C                   MOVEL     MSG(12)       $MSG
020900930721     C                   EXSR      CARMSG
021000940111E   1C                   ENDIF
021100931217     C*
021200940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
021300940117IF  1C     VSCFIL        IFEQ      *BLANKS
021400940117     C                   MOVEL     USER          WEDP              3
021500940117IF  2C     WEDP          IFNE      'EDP'
021600940117     C                   MOVEL     '1'           $ERR
021700940117     C                   MOVEL     MSG(11)       $MSG
021800940117     C                   EXSR      CARMSG
021900940117E   2C                   ENDIF
022000940117E   1C                   ENDIF
022100940117     C*
022200940111IF  1C     $ERR          IFEQ      '0'
022300000000     C*
022400940111     C* LANCIO LA MODIFICA
022500940114     C                   MOVEL     VSCFIL        WLNP
022600940117     C                   Z-ADD     VSCHH1        WHH1
022700940117     C                   Z-ADD     VSCHH2        WHH2
022800061220     C                   Z-ADD     VSCsot        Wsot
022801130520     C                   Z-ADD     VSCost        Wost
022900061220     c                   movel     vscatb        watb
023000940114     C                   MOVEL     'GES'         WOP0
023100940114     C                   MOVEL     'O02'         WOP1
023200940111     C                   MOVEL     'TRTB51R'     WPGM
023300930805     C                   EXSR      LANPGM
023400000000     C*
023500930805     C                   Z-ADD     1             SWMODI
023600930128     C*
023700940111E   1C                   ENDIF
023800000000     C*
023900000000     C                   ENDSR
024000940111     C*------------------------------------------------------------------------*
024100000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
024200940111     C*------------------------------------------------------------------------*
024300000000     C     O04GES        BEGSR
024400930128     C*
024500930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
024600940111IF  1C     VSCATB        IFEQ      'A'
024700930721     C                   MOVEL     '1'           $ERR
024800930805     C                   MOVEL     MSG(12)       $MSG
024900930721     C                   EXSR      CARMSG
025000940111E   1C                   ENDIF
025100940117     C*
025200940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
025300940117IF  1C     VSCFIL        IFEQ      *BLANKS
025400940117     C                   MOVEL     USER          WEDP              3
025500940117IF  2C     WEDP          IFNE      'EDP'
025600940117     C                   MOVEL     '1'           $ERR
025700940117     C                   MOVEL     MSG(11)       $MSG
025800940117     C                   EXSR      CARMSG
025900940117E   2C                   ENDIF
026000940117E   1C                   ENDIF
026100931217     C*
026200940111IF  1C     $ERR          IFEQ      '0'
026300940111     C*
026400940111     C* LANCIO LA CANCELLAZIONE
026500940114     C                   MOVEL     VSCFIL        WLNP
026600940117     C                   Z-ADD     VSCHH1        WHH1
026700940117     C                   Z-ADD     VSCHH2        WHH2
026800061220     C                   Z-ADD     VSCsot        Wsot
026801130520     C                   Z-ADD     VSCost        Wost
026900061220     c                   movel     vscatb        watb
027000940114     C                   MOVEL     'GES'         WOP0
027100940114     C                   MOVEL     'O04'         WOP1
027200940111     C                   MOVEL     'TRTB51R'     WPGM
027300930201     C                   EXSR      LANPGM
027400000000     C*
027500930805     C                   Z-ADD     1             SWMODI
027600940111     C*
027700940111E   1C                   ENDIF
027800000000     C*
027900000000     C                   ENDSR
028000940121     C*------------------------------------------------------------------------*
028100940121     C*  O05GES - GESTIONE OPZIONE 2=MODIFICA
028200940121     C*------------------------------------------------------------------------*
028300940121     C     O05GES        BEGSR
028400940121     C*
028500940121     C* LANCIO LA VISUALIZZAZIONE
028600940121     C                   MOVEL     VSCFIL        WLNP
028700940121     C                   Z-ADD     VSCHH1        WHH1
028800940121     C                   Z-ADD     VSCHH2        WHH2
028900061220     C                   Z-ADD     VSCsot        Wsot
028901130520     C                   Z-ADD     VSCost        Wost
029000061220     c                   movel     vscatb        watb
029100940121     C                   MOVEL     'GES'         WOP0
029200940121     C                   MOVEL     'O05'         WOP1
029300940121     C                   MOVEL     'TRTB51R'     WPGM
029400940121     C                   EXSR      LANPGM
029500940121     C*
029600940121     C                   ENDSR
029700940111     C*------------------------------------------------------------------------*
029800930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
029900940111     C*------------------------------------------------------------------------*
030000930128     C     O07GES        BEGSR
030100930128     C*
030200930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
030300940111IF  1C     VSCATB        IFNE      'A'
030400930721     C                   MOVEL     '1'           $ERR
030500930721     C                   MOVEL     MSG(13)       $MSG
030600930721     C                   EXSR      CARMSG
030700940111E   1C                   ENDIF
030800940117     C*
030900940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
031000940117IF  1C     VSCFIL        IFEQ      *BLANKS
031100940117     C                   MOVEL     USER          WEDP              3
031200940117IF  2C     WEDP          IFNE      'EDP'
031300940117     C                   MOVEL     '1'           $ERR
031400940117     C                   MOVEL     MSG(11)       $MSG
031500940117     C                   EXSR      CARMSG
031600940117E   2C                   ENDIF
031700940117E   1C                   ENDIF
031800930215     C*
031900940111IF  1C     $ERR          IFEQ      '0'
032000930128     C*
032100940111     C* LANCIO IL RIPRISTINO
032200940114     C                   MOVEL     VSCFIL        WLNP
032300940117     C                   Z-ADD     VSCHH1        WHH1
032400940117     C                   Z-ADD     VSCHH2        WHH2
032500061220     C                   Z-ADD     VSCsot        Wsot
032501130520     C                   Z-ADD     VSCost        Wost
032600061220     c                   movel     vscatb        watb
032700940114     C                   MOVEL     'GES'         WOP0
032800940114     C                   MOVEL     'O07'         WOP1
032900940111     C                   MOVEL     'TRTB51R'     WPGM
033000930201     C                   EXSR      LANPGM
033100930128     C*
033200930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
033300930128     C                   Z-ADD     1             SWMODI
033400930128     C*
033500940111E   1C                   ENDIF
033600930128     C*
033700930128     C                   ENDSR
033800940111     C*------------------------------------------------------------------------*
033900930205     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
034000940111     C*------------------------------------------------------------------------*
034100930205     C     NEWPOS        BEGSR
034200930205     C*
034300940111     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
034400940111     C*        1 = RICHIESTO POSIZIONAMENTO
034500930205     C                   Z-ADD     0             SWPOSI            7 0
034600930205     C*
034700940111     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
034800940111     C*            NON OCCORRE RICARICARE IL SUBFILE
034900940111     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
035000930205     C                   Z-ADD     0             SWMODI            1 0
035100930205     C*
035200930205     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
035300940111IF  1C     VCCFIL        IFNE      DEPFIL
035400930205     C*
035500930205     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
035600930205     C                   EXSR      CHKPAR
035700930205     C*
035800930205     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
035900940111IF  2C     $ERR          IFEQ      '0'
036000940111     C                   Z-ADD     1             SWPOSI
036100940111     C                   MOVEL     VCCFIL        DEPFIL
036200940111E   2C                   ENDIF
036300940111E   1C                   ENDIF
036400930205     C*
036500930205     C                   ENDSR
036600940111     C*------------------------------------------------------------------------*
036700940111     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
036800940111     C*------------------------------------------------------------------------*
036900940111     C     CHKPAR        BEGSR
037000940111     C*
037100940111     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
037200940111     C                   SETOFF                                       80
037300940111     C*
037400940114     C* CONTROLLO ESISTENZA FILIALE, SOLO SE DIVERSA DA BLANKS (VALIDA X IL PGM)
037500940112     C                   MOVEL     VCCFIL        WFIL              3
037600940112     C                   MOVEL     *BLANKS       WDFIL            20
037700940113IF  1C     VCCFIL        IFNE      *BLANKS
037800940114     C                   EXSR      CHKFIL
037900940113IF  2C     $ERR          IFEQ      '1'
038000940111     C                   MOVEL     '1'           $ERR
038100940111     C                   MOVEL     MSG(9)        $MSG
038200940111     C                   EXSR      CARMSG
038300940111     C                   SETON                                        80
038400940113E   2C                   ENDIF
038500940113E   1C                   ENDIF
038600940113     C                   MOVEL     WDFIL         VCDFIL
038700940111     C*
038800940111     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
038900940111     C* CURSORE SUL CAMPO DEL CONTROL
039000940111IF  1C     $ERR          IFEQ      '1'
039100940111     C                   MOVEL     '0'           *IN43
039200940111X   1C                   ELSE
039300940111     C                   MOVEL     '1'           *IN43
039400940111E   1C                   ENDIF
039500940111     C*
039600940111     C                   ENDSR
039700940111     C*------------------------------------------------------------------------*
039800940111     C*  CHKFIL - CONTROLLA LA FILIALE
039900940111     C*------------------------------------------------------------------------*
040000940114     C     CHKFIL        BEGSR
040100940111     C*
040200940114     C* INSERIRE CARATTERI NON NUMERICI, ERRORE
040300940114IF  1C     WFIL          IFLT      *ZEROS
040400940114     C                   MOVEL     '1'           $ERR
040500940114X   1C                   ELSE
040600940114     C*
040700940114     C* CONTROLLO ESISTENZA NELL'ORGANIGRAMMA
040800940114     C                   MOVEL     WFIL          KORFIL
040900940114     C     KEYORG        CHAIN     AZORG01L                           99
041000940114IF  2C     *IN99         IFEQ      '1'
041100940114     C     ORGFVA        ORNE      *BLANKS
041200940114     C                   MOVEL     '1'           $ERR
041300940114     C                   MOVEL     *ALL'*'       WDFIL
041400940114X   2C                   ELSE
041500940114     C                   MOVEL     ORGDES        WDFIL
041600940114E   2C                   ENDIF
041700940114E   1C                   ENDIF
041800940113     C*
041900940114     C                   ENDSR
042000940112     C*------------------------------------------------------------------------*
042100940112     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD TABELLE
042200940112     C*------------------------------------------------------------------------*
042300940112     C     SETFIL        BEGSR
042400940112     C*
042500940112     C* POSIZIONAMENTO RECORD TABELLA 'OSA'
042600940114     C                   MOVEL     WFIL          OSA               6
042700940112     C                   MOVE      'OSA'         OSA
042800940112     C*
042900020923     C                   MOVEL     1             KTBKUT
043000940112     C                   MOVEL     '1X'          KTBCOD
043100940112     C                   MOVEL     *BLANKS       KTBKEY
043200940112     C                   MOVEL     OSA           KTBKEY
043300940112     C*
043400940113     C     KEYTAB        SETLL     TABEL00F                           99
043500940112     C*
043600960418     C                   EXSR      LETFIL
043700940112     C*
043800940112     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
043900940112IF  1C     *IN99         IFEQ      '1'
044000940112     C                   Z-ADD     1             SFLEOF
044100940112E   1C                   ENDIF
044200940112     C*
044300940112     C                   ENDSR
044400940112     C*------------------------------------------------------------------------*
044500971013     C* LETFIL - LEGGE RECORD SUCCESSIVO
044600940112     C*------------------------------------------------------------------------*
044700940112     C     LETFIL        BEGSR
044800940112     C*
044900940113     C* FINO A QUANDO NON TROVI UN RECORD VALIDO (RECOK = '1') STAI DENTRO
045000940113     C                   MOVEL     '0'           RECOK             1
045100940113DO  1C     RECOK         DOWEQ     '0'
045200940112     C*
045300940112     C* LETTURA RECORD TABELLA 'OSA'
045400940113     C                   READ      TABEL00F                               99
045500940112     C*
045600940113     C* FINE FILE  O TABELLA DIVERSA
045700940112IF  2C     *IN99         IFEQ      '1'
045800940113     C     TBLKUT        ORNE      1
045900940113     C     TBLCOD        ORNE      '1X'
046000940112     C                   Z-ADD     1             SFLEOF
046100940113     C                   MOVEL     '1'           RECOK
046200940112X   2C                   ELSE
046300940112     C*
046400940112     C* CONTROLLO RECORD VALIDO
046500940113     C                   MOVE      TBLKEY        TIPKEY            5
046600940113IF  3C     TIPKEY        IFEQ      'OSA  '
046700940113     C                   MOVEL     '1'           RECOK
046800940113E   3C                   ENDIF
046900940112E   2C                   ENDIF
047000940112E   1C                   ENDDO
047100940112     C*
047200940112     C                   ENDSR
047300940111     C*------------------------------------------------------------------------*
047400000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
047500940111     C*------------------------------------------------------------------------*
047600000000     C     IRESFL        BEGSR
047700940112     C*
047800940112     C* RIEMPIMENTO CAMPI DI "OSA"
047900940112IF  1C     TBLFLG        IFNE      *BLANKS
048000940112     C                   MOVEL     'A'           VSCATB
048100940112X   1C                   ELSE
048200940112     C                   MOVEL     ' '           VSCATB
048300940113E   1C                   ENDIF
048400940112     C                   MOVEL     TBLKEY        VSCFIL
048500940112     C                   MOVEL     TBLUNI        DS1X
048600940112     C                   Z-ADD     §1XHHO        VSCHH1
048700061220     c* Decodifica filiale
048800061220     c                   movel     vscfil        w0030             3 0
048801130520     c                   if        w0030=0
048802130520     c                   movel(p)  'STANDARD'    vsdfil
048803130520     c                   else
048900061220     c     w0030         chain     azorg01l
049000061220     c                   if        %found(azorg01l)
049100061220     c                   movel     orgdes        vsdfil
049200061220     c                   else
049300061220     c                   movel     *all'?'       vsdfil
049400061220     c                   endif
049401130520     c                   endif
049500940112     C*
049600940112     C* REPERISCO IL CORRISPONDENTE RECORD OSA1
049700940112     C                   MOVEL     *BLANKS       ALF7              7
049800940112     C                   MOVEL     TBLKEY        ALF7
049900940112     C                   MOVE      '1'           ALF7
050000940113     C                   MOVEL     *BLANKS       KTBKEY
050100940113     C                   MOVEL     ALF7          KTBKEY
050200940112     C     KEYTAB        CHAIN     TABEL00F                           98
050300940112     C*
050400940112     C* RIEMPIMENTO CAMPI DI "OSA1"
050500940112IF  1C     *IN98         IFEQ      '0'
050600940113     C                   MOVEL     TBLUNI        DS1X
050700940112     C                   Z-ADD     §1XHHO        VSCHH2
050800940112X   1C                   ELSE
050900940112     C                   Z-ADD     9999          VSCHH2
051000940112E   1C                   ENDIF
051001130520
051002130520     c                   clear                   trul09ds
051003130520     c                   movel     vscfil        i09tfa
051004130520     c                   movel     'TFA'         i09mod
051005130520     c                   call      'TRUL09R'
051006130520     c                   parm                    trul09ds
051007130520
051008130520     c* Se 1X OSA annullata, carico anche OST annullata, altrimenti ignoro
051009130520     c                   clear                   vscost
051010130520
051011130520     C                   MOVEL(p)  TBLKEY        ALF7
051012130520     C                   MOVE      'T '          ALF7
051013130520     C                   MOVEL(p)  ALF7          KTBKEY
051014130520     c     keytab        chain     tabel00f
051015130520     c*
051016130520     c                   if        (not %found(tabel00f) or
051017130520     c                             (tblflg<>' ' and vscatb=' '))
051018130520     c                             and o09err=' '
051019130520     c                   movel     dethho        vscost
051020130520     c                   endif
051021130520     c
051022130520     c                   if        %found(tabel00f) and(tblflg=' ' or
051023130520     c                             tblflg<>' ' and vscatb<>' ')
051024130520     c                   movel     tbluni        ds1x
051025130520     C                   Z-ADD     §1XHHO        vscost
051026130520     c                   endif
051100061218     c
051200061218     c* Verifico se presente tabella SOT, solo se si tratta di terminal
051300061218     c*  di arrivo
051900061218     c
052000061220     c                   clear                   vscsot
052100061220     c* Se 1X annullata, carico anche sot annullata, altrimenti ignoro
052200061220     c                   movel     vscfil        kke1
052300061220     c     ktbe          chain     tntbe01l
052400061220     c*
052500061220     c                   if        (not %found(tntbe01l) or
052600061220     c                             (tbeatb<>' ' and vscatb=' '))
052700061220     c                             and o09err=' '
052800061220     c                   movel     depsot        vscsot
052900061220     c                   endif
053000061220     c
053100061220     c                   if        %found(tntbe01l) and(tbeatb=' ' or
053200061220     c                             tbeatb<>' ' and vscatb<>' ')
053300061220     c                   movel     tbeuni        vscsot
053400061220     c                   endif
053500061220     c
053600061218     c
053700000000     C                   ENDSR
053800940111     C*------------------------------------------------------------------------*
053900930201     C* LANPGM - LANCIA UN PROGRAMMA
054000940111     C*------------------------------------------------------------------------*
054100930201     C     LANPGM        BEGSR
054200940114     C*
054300940114     C*--------------------
054400940114     C* PARAMETRI INFORMATIVI
054500940114     C*--------------------
054600940114     C                   MOVEL     WOP0          D50OP0
054700940114     C                   MOVEL     WOP1          D50OP1
054800940114     C                   MOVEL     '0'           D50F03
054900940114     C                   MOVEL     '0'           D50ERR
055000940114     C                   MOVEL     *BLANKS       D50MSG
055100940114     C*--------------------
055200940114     C* PARAMETRI
055300940114     C*--------------------
055400940114     C                   MOVEL     WLNP          D50FIL
055500940117     C                   Z-ADD     WHH1          OSAHHO
055600940117     C                   Z-ADD     WHH2          OS1HHO
055700061220     C                   Z-ADD     Wsot          d50sot
055701130520     C                   Z-ADD     Wost          osthho
055800061220     c                   movel     watb          d50atb
055900940114     C* LANCIO
056000940114     C                   CALL      WPGM
056100940114     C                   PARM                    DS50
056200940114     C*
056300930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
056400940114IF  1C     D50F03        IFEQ      '1'
056500930721     C                   MOVEL     '1'           $F03
056600930721     C                   MOVEL     '1'           $ERR
056700940105     C                   MOVEL     '1'           $FINE
056800940114     C*
056900930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
057000940114X   1C                   ELSE
057100940112     C                   MOVEL     D50MSG        $MSG
057200930721     C                   EXSR      CARMSG
057300940114E   1C                   ENDIF
057400930201     C*
057500930201     C                   ENDSR
057600940114     C*------------------------------------------------------------------------*
057700940114     C*  CAROSA - CARICAMENTO DATI DI OSA E OSA1 CHE NON CHIEDO IN INSERIMENTO
057800940114     C*------------------------------------------------------------------------*
057900940114     C     CAROSA        BEGSR
058000940114     C*
058100940114     C* CARICO I DATI DI 'OSA'
058200940114     C*
058300020923     C                   MOVEL     1             KTBKUT
058400940114     C                   MOVEL     '1X'          KTBCOD
058500940114     C                   MOVEL     *BLANKS       KTBKEY
058600940119     C                   MOVEL     *BLANKS       OSA               6
058700940119     C                   MOVE      'OSA'         OSA
058800940119     C                   MOVEL     OSA           KTBKEY
058900940114     C*
059000940114     C     KEYTAB        CHAIN     TABEL00F                           99
059100940114IF  1C     *IN99         IFEQ      *OFF
059200940114     C                   MOVEL     TBLUNI        DS1X
059300940114X   1C                   ELSE
059400940119     C                   CLEAR                   DS1X
059500940114E   1C                   ENDIF
059600940114     C*
059700940114     C* MUOVO I CAMPI DI 'OSA' IN QUELLI DELLA DS DI PROCEDURA PER IL PASSAGGIO
059800940114     C                   MOVEL     §1XDES        OSADES
059900940114     C                   MOVEL     §1XFI1        OSAFI1
060000940114     C                   Z-ADD     §1XHHG        OSAHHG
060100940114     C                   MOVEL     §1XFI2        OSAFI2
060200940121     C                   Z-ADD     §1XHHO        OSAHHO
060300940114     C                   MOVEL     §1XFI3        OSAFI3
060400940114     C                   Z-ADD     §1XHHM        OSAHHM
060500940114     C                   MOVEL     §1XFI4        OSAFI4
060600940114     C                   Z-ADD     §1XHHP        OSAHHP
060700940114     C                   MOVEL     §1XFI5        OSAFI5
060800940114     C                   Z-ADD     §1XHHI        OSAHHI
060900940114     C                   MOVEL     §1XFI6        OSAFI6
061000940114     C                   Z-ADD     §1XHHS        OSAHHS
061100940121     C                   Z-ADD     §1XHHO        DEPHHO
061200940114     C*
061300940114     C* CARICO I DATI DI 'OSA1'
061400940114     C                   MOVEL     *BLANKS       OSA1              7
061500940114     C                   MOVE      'OSA1'        OSA1
061600940114     C*
061700020923     C                   MOVEL     1             KTBKUT
061800940114     C                   MOVEL     '1X'          KTBCOD
061900940114     C                   MOVEL     *BLANKS       KTBKEY
062000940114     C                   MOVEL     OSA1          KTBKEY
062100940114     C*
062200940114     C     KEYTAB        CHAIN     TABEL00F                           99
062300940114IF  1C     *IN99         IFEQ      *OFF
062400940114     C                   MOVEL     TBLUNI        DS1X
062500940114X   1C                   ELSE
062600940114     C                   CLEAR                   DS1X
062700940114E   1C                   ENDIF
062800940114     C*
062900940114     C* MUOVO I CAMPI DI 'OSA1' IN QUELLI DELLA DS DI PROCEDURA PER IL PASSAGGIO
063000940114     C                   MOVEL     §1XDES        OS1DES
063100940114     C                   MOVEL     §1XFI1        OS1FI1
063200940114     C                   Z-ADD     §1XHHG        OS1HHG
063300940114     C                   MOVEL     §1XFI2        OS1FI2
063400940121     C                   Z-ADD     §1XHHO        OS1HHO
063500940114     C                   MOVEL     §1XFI3        OS1FI3
063600940114     C                   Z-ADD     §1XHHM        OS1HHM
063700940114     C                   MOVEL     §1XFI4        OS1FI4
063800940114     C                   Z-ADD     §1XHHP        OS1HHP
063900940114     C                   MOVEL     §1XFI5        OS1FI5
064000940114     C                   Z-ADD     §1XHHI        OS1HHI
064100940114     C                   MOVEL     §1XFI6        OS1FI6
064200940114     C                   Z-ADD     §1XHHS        OS1HHS
064300940121     C                   Z-ADD     §1XHHO        DE1HHO
064400940114     C*
064500061220     c* Carico il dato GEN della tabella SOT
064600061220     C                   CLEAR                   DSBS02
064700061220     C                   MOVEL     'C'           T02MOD
064800061220     C                   MOVEL     KNSIF         T02SIF
064900061220     C                   MOVEL     'SOT'         T02COD
065000061220     C                   MOVEL     'GEN'         T02KE1
065100061220     C                   CALL      'TIBS02R'
065200061220     C                   PARM                    KPJBA
065300061220     C                   PARM                    DSBS02
065400061220     c                   movel     t02uni        depsot
065401130517
065402130517     C* CARICO I DATI DI 'OST'
065403130517     C*
065404130517     C                   MOVEL     1             KTBKUT
065405130517     C                   MOVEL     '1X'          KTBCOD
065406130517     C                   MOVEL     *BLANKS       KTBKEY
065407130517     C                   MOVEL     *BLANKS       OSA               6
065408130517     C                   MOVE      'OST'         OSA
065409130517     C                   MOVEL     OSA           KTBKEY
065410130517     C*
065411130517     C     KEYTAB        CHAIN     TABEL00F                           99
065412130517IF  1C     *IN99         IFEQ      *OFF
065413130517     C                   MOVEL     TBLUNI        DS1X
065414130517X   1C                   ELSE
065415130517     C                   CLEAR                   DS1X
065416130517E   1C                   ENDIF
065417130517     C*
065418130517     C* MUOVO I CAMPI DI 'OST' IN QUELLI DELLA DS DI PROCEDURA PER IL PASSAGGIO
065419130517     C                   MOVEL     §1XDES        OSTDES
065420130517     C                   MOVEL     §1XFI1        OSTFI1
065421130517     C                   Z-ADD     §1XHHG        OSTHHG
065422130517     C                   MOVEL     §1XFI2        OSTFI2
065423130517     C                   Z-ADD     §1XHHO        OSTHHO
065424130517     C                   MOVEL     §1XFI3        OSTFI3
065425130517     C                   Z-ADD     §1XHHM        OSTHHM
065426130517     C                   MOVEL     §1XFI4        OSTFI4
065427130517     C                   Z-ADD     §1XHHP        OSTHHP
065428130517     C                   MOVEL     §1XFI5        OSTFI5
065429130517     C                   Z-ADD     §1XHHI        OSTHHI
065430130517     C                   MOVEL     §1XFI6        OSTFI6
065431130517     C                   Z-ADD     §1XHHS        OSTHHS
065432130517     C                   Z-ADD     §1XHHO        DETHHO
065500061220     c
065600940114     C                   ENDSR
065700020923     C*-------------------------------------------------------------------------
065800020923     C* REPERISCE I DATI UTENTE
065900020923     C*-------------------------------------------------------------------------
066000020923     C     REPDATIUTE    BEGSR
066100020923     C*
066200020923     C* INIZIALIZZA VARIABILI DI WRK
066300020923     C                   CLEAR                   TIBS34DS
066400020923     C                   CLEAR                   AZUTEDS
066500020923     C                   CLEAR                   DDATIUTE
066600020923     C*
066700020923     C     *DTAARA       DEFINE    §azute        azuteds
066800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
066900020923     C                   IN(E)     *DTAARA
067000020923if  1C                   IF        %Error
067100020923     c                   EVAL      I34Tla = 'L'
067200020923     C                   CALL      'TIBS34R'
067300020923     C                   PARM                    Tibs34Ds
067400020923     C                   IN        *DTAARA
067500020923e   1C                   ENDIF
067600020923     C*
067700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
067800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
067900020923     C*
068000020923     C                   ENDSR
068100940111     C*------------------------------------------------------------------------*
068200930713     C*  *INZSR - ROUTINE INIZIALE
068300940111     C*------------------------------------------------------------------------*
068400930713     C     *INZSR        BEGSR
068500000000     C*
068600000000     C* RICEVIMENTO PARAMETRI
068700000000     C     *ENTRY        PLIST
068800940105     C                   PARM                    KPJBA
068900940111     C*
069000940111     C* DEFINISCE LE VARIABILI DI PROGRAMMA
069100940111     C     *LIKE         DEFINE    VCCFIL        DEPFIL
069200940111     C     *LIKE         DEFINE    TBLKUT        KTBKUT
069300940111     C     *LIKE         DEFINE    TBLCOD        KTBCOD
069400940111     C     *LIKE         DEFINE    TBLKEY        KTBKEY
069500940121     C     *LIKE         DEFINE    ORGFIL        KORFIL
069600940121     C     *LIKE         DEFINE    OSAHHO        DEPHHO
069700940121     C     *LIKE         DEFINE    OS1HHO        DE1HHO
069800061220     C     *LIKE         DEFINE    OS1HHO        DEpsot
069801130520     C     *LIKE         DEFINE    OSTHHO        DETHHO
069900930310     C*
070000930310     C* CHIAVE LETTURA TABEL00F - COMPLETA
070100940112     C     KEYTAB        KLIST
070200940111     C                   KFLD                    KTBKUT
070300940111     C                   KFLD                    KTBCOD
070400940111     C                   KFLD                    KTBKEY
070500061220     C     Ktbe          KLIST
070600061220     C                   KFLD                    KCOD
070700061220     C                   KFLD                    KKE1
070800061220     C                   KFLD                    KKE2
070900061220     C*
071000940112     C*
071100940114     C* CHIAVE LETTURA TABEL00F - PARZIALE
071200940112     C     KEYTA2        KLIST
071300940112     C                   KFLD                    KTBKUT
071400940112     C                   KFLD                    KTBCOD
071500940114     C*
071600940114     C* CHIAVE LETTURA AZORG01L - COMPLETA
071700940114     C     KEYORG        KLIST
071800940114     C                   KFLD                    KORFIL
071900930714     C*
072000930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
072100930803     C                   EXSR      INZSTD
072200930803     C*
072300930803     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
072400930803     C                   EXSR      INZSFL
072500930714     C*
072600930806     C* IMPOSTO CAMPI TESTATA VIDEO
072700940114     C                   MOVEL     DSFIRS        VTDRSU
072800980130     C*
072900980130     C* CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
073000061220     C**                 CLEAR                   UL12DS
073100061220     C**                 MOVEL     'L'           I12TLA
073200061220     C**                 MOVEL     '1X'          I12COD
073300061220     C**                 MOVEL     KNSIF         I12NSI
073400061220     C**                 CALL      'TRUL12R'
073500061220     C**                 PARM                    UL12DS
073600061220     C**   O12ERR        IFEQ      '1'
073700061220     C**                 MOVEL     '1'           $FINE
073800061220     C**                 ENDIF
073900940104     C*
074000940114     C* CARICA I DATI DI OSA E OSA1
074100940114     C                   EXSR      CAROSA
074200940114     C*
074300940114     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
074400960418     C                   MOVEL     '***'         DEPFIL
074500940114     C*
074600940104     C* EMETTO VIDEATA SUBFILE
074700940114     C                   EXSR      CARVS
074800940114     C*
074900000000     C                   ENDSR
075000940111     C*------------------------------------------------------------------------*
075100940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075200940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075300940111     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
075400940111     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
075500940111     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
075600940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075700940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075800940111     C*------------------------------------------------------------------------*
075900930803     C* INZSTD - ROUTINE INIZIALE STANDARD
076000940111     C*------------------------------------------------------------------------*
076100930803     C     INZSTD        BEGSR
076200930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
076300930802     C                   CALL      'XKEYS'
076400930802     C                   PARM                    DSKEYS
076500930805     C                   MOVEA     KEYRIG        KEYS
076600930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
076700930714     C     XMSGPA        PLIST
076800930714     C                   PARM                    DSMSMS
076900930714     C                   PARM                    DSMSPG
077000930714     C                   PARM                    DSMSKE
077100930714     C                   PARM                    DSMSER
077200930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
077300940111     C                   MOVEL     'TRTB50R'     DSMSPG
077400940111     C                   MOVEL     'TRTB50R'     VMCQUE
077500930806     C                   MOVEL     *BLANKS       VMCKEY
077600930714     C                   MOVEL     *BLANKS       DSMSMS
077700930805     C* INIZIALIZZO FLAG CONTROLLO PGM
077800930721     C                   MOVEL     '0'           $ERR              1
077900930721     C                   MOVEL     '0'           $F03              1
078000930806     C                   MOVEL     *BLANKS       $GES              2
078100930721     C                   MOVEL     *BLANKS       $MSG             80
078200930721     C                   MOVEL     '0'           $FINE             1
078300930721     C                   MOVEL     '0'           $TESVD            1
078400940112     C                   MOVEL     'G'           $TGES             1
078500930714     C* LANCIO ROUTINE PER VERIFICARE SE
078600930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
078700930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
078800020923     C                   EXSR      REPDATIUTE
078900930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
079000930721     C                   BITOFF    '01234567'    POSMI1            1
079100930721     C                   BITOFF    '01234567'    POSMI2            1
079200930721     C                   BITOFF    '01234567'    POSMA1            1
079300930721     C                   BITOFF    '01234567'    POSMA2            1
079400930721     C                   BITON     '34'          POSMI1            1
079500930721     C                   BITON     '7'           POSMI2            1
079600930721     C                   BITON     '34'          POSMA1            1
079700930721     C                   BITON     '0'           POSMA2            1
079800930721     C                   MOVE      POSMI2        POSMIN            2
079900930721     C                   MOVEL     POSMI1        POSMIN
080000930721     C                   MOVE      POSMA2        POSMAX            2
080100930721     C                   MOVEL     POSMA1        POSMAX
080200930714     C                   ENDSR
080300940111     C*------------------------------------------------------------------------*
080400930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
080500940111     C*------------------------------------------------------------------------*
080600930804     C     FUNSTD        BEGSR
080700930804     C                   SELECT
080800930804     C     KEY           WHENEQ    F3
080900930804     C                   EXSR      F03STD
081000930804     C     KEY           WHENEQ    F12
081100930804     C                   EXSR      F12STD
081200930804     C     KEY           WHENEQ    F24
081300930804     C                   EXSR      F24STD
081400930804     C                   OTHER
081500930804     C                   MOVEL     MSG(2)        $MSG
081600930804     C                   EXSR      CARMSG
081700930804     C                   ENDSL
081800930804     C                   ENDSR
081900940111     C*------------------------------------------------------------------------*
082000930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
082100940111     C*------------------------------------------------------------------------*
082200930804     C     F03STD        BEGSR
082300930804     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
082400930923     C                   MOVEL     '1'           $F03
082500930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
082600930804     C                   MOVEL     '1'           $FINE
082700930804     C                   ENDSR
082800940111     C*------------------------------------------------------------------------*
082900930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
083000940111     C*------------------------------------------------------------------------*
083100930804     C     F12STD        BEGSR
083200930805     C* IMPOSTO FLAG DI FIN PGM
083300930804     C                   MOVEL     '1'           $FINE
083400930804     C                   ENDSR
083500940111     C*------------------------------------------------------------------------*
083600930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
083700940111     C*------------------------------------------------------------------------*
083800930804     C     F24STD        BEGSR
083900930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
084000930804     C                   EXSR      OUTFUN
084100930804     C                   ENDSR
084200940111     C*------------------------------------------------------------------------*
084300930804     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
084400940111     C*------------------------------------------------------------------------*
084500930804     C     INZSFL        BEGSR
084600930804     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
084700930806     C     *LIKE         DEFINE    VCCNBR        SFLPAG
084800930806     C     *LIKE         DEFINE    VCCNBR        SFLEOF
084900930806     C     *LIKE         DEFINE    VCCNBR        SFLERR
085000930806     C     *LIKE         DEFINE    VCCNBR        SFLULT
085100930806     C     *LIKE         DEFINE    VCCNBR        WFLULT
085200930806     C     *LIKE         DEFINE    VCCNBR        SFLRIG
085300930804     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
085400930804     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
085500930804     C                   Z-ADD     12            SFLPAG
085600930804     C                   ENDSR
085700940111     C*------------------------------------------------------------------------*
085800930804     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
085900940111     C*------------------------------------------------------------------------*
086000930804     C     FUNSFL        BEGSR
086100930804     C                   SELECT
086200930804     C     KEY           WHENEQ    ENTER
086300930804     C                   EXSR      ENTSFL
086400930805     C     KEY           WHENEQ    F3
086500930805     C                   EXSR      F03STD
086600930804     C     KEY           WHENEQ    F5
086700930804     C                   EXSR      F05STD
086800930805     C     KEY           WHENEQ    F12
086900930805     C                   EXSR      F12STD
087000930804     C     KEY           WHENEQ    F17
087100930804     C                   EXSR      F17STD
087200930804     C     KEY           WHENEQ    F18
087300930804     C                   EXSR      F18STD
087400930804     C     KEY           WHENEQ    F23
087500930804     C                   EXSR      F23STD
087600930805     C     KEY           WHENEQ    F24
087700930805     C                   EXSR      F24STD
087800930804     C     KEY           WHENEQ    ROLLUP
087900930804     C                   EXSR      RUPSTD
088000930804     C     KEY           WHENEQ    ROLLDN
088100930804     C                   EXSR      RDNSTD
088200930804     C                   OTHER
088300930804     C                   MOVEL     MSG(2)        $MSG
088400930804     C                   EXSR      CARMSG
088500930804     C                   ENDSL
088600930804     C                   ENDSR
088700940111     C*------------------------------------------------------------------------*
088800930804     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
088900940111     C*------------------------------------------------------------------------*
089000930804     C     F05STD        BEGSR
089100930804     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
089200930804     C                   EXSR      POSSFL
089300930804     C                   ENDSR
089400940111     C*------------------------------------------------------------------------*
089500930804     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
089600940111     C*------------------------------------------------------------------------*
089700930804     C     F17STD        BEGSR
089800930804     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
089900930806     C                   Z-ADD     1             VCCNBR
090000930804     C                   ENDSR
090100940111     C*------------------------------------------------------------------------*
090200930804     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
090300940111     C*------------------------------------------------------------------------*
090400930804     C     F18STD        BEGSR
090500930804     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
090600930806     C                   Z-ADD     SFLULT        VCCNBR
090700930804     C                   ENDSR
090800940111     C*------------------------------------------------------------------------*
090900930804     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
091000940111     C*------------------------------------------------------------------------*
091100930804     C     F23STD        BEGSR
091200930804     C* GESTIONE VISUALIZZAZIONE OPZIONI
091300930804     C                   EXSR      OUTOPZ
091400930804     C                   ENDSR
091500940111     C*------------------------------------------------------------------------*
091600930804     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
091700940111     C*------------------------------------------------------------------------*
091800930804     C     RUPSTD        BEGSR
091900930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
092000930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
092100930805     C     CURSOR        IFGE      POSMIN
092200930805     C     CURSOR        ANDLE     POSMAX
092300930804     C                   ELSE
092400930805     C                   EXSR      CARSFL
092500930805     C                   ENDIF
092600930804     C                   ENDSR
092700940111     C*------------------------------------------------------------------------*
092800930804     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
092900940111     C*------------------------------------------------------------------------*
093000930804     C     RDNSTD        BEGSR
093100930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
093200930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
093300930804     C     CURSOR        IFGE      POSMIN
093400930804     C     CURSOR        ANDLE     POSMAX
093500930804     C                   ELSE
093600930804     C* INVIO MESSAGGIO DI INIZIO ELENCO
093700930804     C                   MOVEL     '1'           $ERR
093800930804     C                   MOVEL     MSG(5)        $MSG
093900930804     C                   EXSR      CARMSG
094000930805     C                   ENDIF
094100930804     C                   ENDSR
094200940111     C*------------------------------------------------------------------------*
094300930804     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
094400940111     C*------------------------------------------------------------------------*
094500930804     C     ENTSFL        BEGSR
094600930804     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
094700930804     C                   EXSR      NEWPOS
094800930804     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
094900930804     C     $ERR          IFEQ      '0'
095000930804     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
095100930804     C     SWPOSI        IFNE      0
095200930804     C                   EXSR      POSSFL
095300930804     C                   ELSE
095400930804     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
095500930804     C     SFLULT        IFNE      0
095600930804     C                   EXSR      OPZSFL
095700930804     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
095800930804     C     SWMODI        IFNE      0
095900930804     C                   EXSR      POSSFL
096000930805     C                   ENDIF
096100930805     C                   ENDIF
096200930805     C                   ENDIF
096300930805     C                   ENDIF
096400930804     C                   ENDSR
096500940111     C*------------------------------------------------------------------------*
096600940111     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
096700940111     C*------------------------------------------------------------------------*
096800930804     C     POSSFL        BEGSR
096900930804     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
097000930804     C                   EXSR      CLRSFL
097100930804     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
097200930804     C                   EXSR      SETFIL
097300930804     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
097400930804     C     SFLEOF        IFEQ      0
097500930804     C                   EXSR      CARSFL
097600930805     C                   ENDIF
097700930804     C                   ENDSR
097800940111     C*------------------------------------------------------------------------*
097900930804     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
098000940111     C*------------------------------------------------------------------------*
098100930804     C     OPZSFL        BEGSR
098200930802     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
098300930802     C                   MOVEL     'S'           OPZOK             1
098400000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
098500000000     C                   Z-ADD     0             SFLVAR            5 0
098600000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
098700000000     C                   Z-ADD     0             SFLERR
098800000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
098900000000     C                   Z-ADD     SFLULT        WFLULT
099000000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
099100940111     C                   READC     TB50DVS                                49
099200930806     C     *IN49         DOWEQ     '0'
099300000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
099400000000     C                   ADD       1             SFLVAR
099500930802     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
099600930805     C     OPZOK         IFEQ      'S'
099700930802     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
099800930806     C     VSCOPZ        IFNE      *BLANKS
099900930802     C                   EXSR      CONOPZ
100000930802     C     OPZOK         IFEQ      'S'
100100930802     C                   EXSR      GESOPZ
100200930805     C                   ENDIF
100300930805     C                   ENDIF
100400000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
100500000000     C*            2. PULIZIA SCELTA IN SFL
100600930805     C     OPZOK         IFEQ      'S'
100700000000     C                   MOVEL     '0'           *IN41
100800930806     C                   MOVEL     *BLANKS       VSCOPZ
100900930805     C                   ELSE
101000000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
101100000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
101200000000     C                   MOVEL     '1'           *IN41
101300000000     C                   Z-ADD     SFLULT        SFLERR
101400930805     C                   ENDIF
101500930805     C                   ELSE
101600000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
101700000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
101800000000     C                   MOVEL     '1'           *IN41
101900930805     C                   ENDIF
102000000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
102100000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
102200000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
102300930803     C     OPZOK         IFNE      'S'
102400930806     C                   Z-ADD     SFLERR        VCCNBR
102500000000     C                   ELSE
102600930806     C                   Z-ADD     SFLULT        VCCNBR
102700930805     C                   ENDIF
102800000000     C* AGGIORNO RECORD SFL
102900940111     C                   UPDATE    TB50DVS
103000940111     C                   READC     TB50DVS                                49
103100930805     C                   ENDDO
103200000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
103300000000     C                   Z-ADD     WFLULT        SFLULT
103400000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
103500000000     C     SFLVAR        IFEQ      0
103600930721     C                   MOVEL     '1'           $FINE
103700930805     C                   ENDIF
103800000000     C                   ENDSR
103900940111     C*------------------------------------------------------------------------*
104000930804     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
104100940111     C*------------------------------------------------------------------------*
104200000000     C     CLRSFL        BEGSR
104300000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
104400000000     C* (0=NON FINE FILE  1=FINE FILE)
104500000000     C                   Z-ADD     0             SFLEOF
104600000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
104700000000     C                   Z-ADD     0             SFLULT
104800000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
104900000000     C                   MOVEL     '1'           *IN40
105000940111     C                   WRITE     TB50DVC
105100000000     C                   MOVEL     '0'           *IN40
105200000000     C                   ENDSR
105300940111     C*------------------------------------------------------------------------*
105400000000     C* CARSFL - CARICAMENTO SFL
105500940111     C*------------------------------------------------------------------------*
105600000000     C     CARSFL        BEGSR
105700000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
105800000000     C     SFLULT        IFEQ      0
105900000000     C                   EXSR      SETFIL
106000930805     C                   ENDIF
106100000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
106200000000     C                   Z-ADD     SFLULT        WFLULT
106300000000     C* CICLO:
106400000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
106500000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
106600000000     C                   Z-ADD     1             SFLRIG
106700930805     C     SFLRIG        DOWLE     SFLPAG
106800000000     C     SFLEOF        ANDEQ     0
106900000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
107000930806     C                   MOVEL     *BLANKS       VSCOPZ
107100000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
107200000000     C                   EXSR      IRESFL
107300000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
107400000000     C                   ADD       1             SFLULT
107500000000     C* SCRITTURA RECORD SFL
107600940111     C                   WRITE     TB50DVS
107700000000     C                   ADD       1             SFLRIG
107800000000     C* LETTURA FILE DA CARICARE
107900940112     C                   EXSR      LETFIL
108000930805     C                   ENDDO
108100000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
108200000000     C     WFLULT        IFNE      SFLULT
108300930806     C                   Z-ADD     WFLULT        VCCNBR
108400930806     C                   ADD       1             VCCNBR
108500930805     C                   ENDIF
108600930720     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
108700930805     C     SFLEOF        IFEQ      1
108800930721     C     SFLULT        ANDGT     *ZEROS
108900930721     C                   MOVEL     '1'           $ERR
109000930804     C                   MOVEL     MSG(6)        $MSG
109100930721     C                   EXSR      CARMSG
109200930804     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
109300940111     C     SFLULT        CHAIN     TB50DVS                            99
109400930804     C     *IN99         IFEQ      '0'
109500930804     C                   SETON                                        45
109600940111     C                   UPDATE    TB50DVS
109700930804     C                   SETOFF                                       45
109800930805     C                   ENDIF
109900930805     C                   ENDIF
110000000000     C                   ENDSR
110100940111     C*------------------------------------------------------------------------*
110200930804     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
110300940111     C*------------------------------------------------------------------------*
110400000000     C     OUTSFL        BEGSR
110500930805     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
110600930721     C     MSGULT        IFNE      *ZEROS
110700930721     C                   EXSR      OUTMSG
110800930805     C                   ENDIF
110900000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
111000000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
111100000000     C     SFLEOF        IFEQ      0
111200000000     C                   MOVEL     '0'           *IN42
111300000000     C                   ELSE
111400000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
111500000000     C                   MOVEL     '1'           *IN42
111600930805     C                   ENDIF
111700930720     C* EMISSIONE SUBFILE
111800000000     C     SFLULT        IFLE      0
111900930720     C*  --> CASO DI SFL VUOTO
112000930720     C                   MOVEL     '0'           *IN43
112100940111     C                   WRITE     TB50DVV
112200940111     C                   WRITE     TB50DVC
112300940111     C                   READ      TB50DVC                                99
112400000000     C                   ELSE
112500930720     C*  --> CASO DI SFL CON RECORDS
112600940111     C                   WRITE     TB50DVC
112700940111     C                   READ      TB50DVC                                99
112800930805     C                   ENDIF
112900930720     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
113000930720     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
113100930806     C                   Z-ADD     FRTSFL        VCCNBR
113200000000     C                   ENDSR
113300940111     C*------------------------------------------------------------------------*
113400930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
113500930721     C*          CODA MESSAGGI DEL PGM
113600940111     C*------------------------------------------------------------------------*
113700930721     C     CARMSG        BEGSR
113800930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
113900930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
114000930721     C     MSGULT        IFEQ      0
114100930721     C                   MOVEL     '1'           *IN40
114200940111     C                   WRITE     TB50DVK
114300930721     C                   MOVEL     '0'           *IN40
114400930805     C                   ENDIF
114500930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
114600930721     C                   MOVEL     $MSG          DSMSMS
114700930721     C                   CALL      'XMSG'        XMSGPA
114800930721     C     DSMSER        IFEQ      0
114900930806     C                   MOVEL     DSMSKE        VMCKEY
115000930805     C                   ENDIF
115100930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
115200931015     C                   ADD       1             MSGULT            4 0
115300940111     C                   WRITE     TB50DVM
115400930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
115500930721     C                   MOVEL     *BLANKS       $MSG
115600930721     C                   ENDSR
115700940111     C*------------------------------------------------------------------------*
115800930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
115900940111     C*------------------------------------------------------------------------*
116000930721     C     OUTMSG        BEGSR
116100930721     C* VISUALIZZAZIONE MESSAGGIO
116200930721     C                   MOVEL     '1'           *IN44
116300940111     C                   WRITE     TB50DVK
116400930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
116500930721     C                   Z-ADD     *ZEROS        MSGULT
116600930721     C                   ENDSR
116700940111     C*------------------------------------------------------------------------*
116800930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
116900930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
117000930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
117100930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
117200930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
117300940111     C*------------------------------------------------------------------------*
117400930803     C     CARFUN        BEGSR
117500930803     C                   Z-ADD     *ZEROS        FUNULT            5 0
117600931015     C     1             DO        31            I                 5 0
117700930803     C                   MOVEL     FUN(I)        FUNRIG
117800930805     C                   MOVEL     FUNFUN        FFU(I)
117900930805     C                   MOVEL     FUNDES        FDE(I)
118000020923     C     DUTLPO        IFEQ      'S'
118100930923     C     $TGES         IFEQ      'G'
118200930805     C                   MOVEL     FUNASG        FAT(I)
118300930803     C                   ELSE
118400930803     C                   MOVEL     FUNASI        FAT(I)
118500930803     C                   ENDIF
118600930803     C                   ELSE
118700930923     C     $TGES         IFEQ      'G'
118800930803     C                   MOVEL     FUNAFG        FAT(I)
118900930803     C                   ELSE
119000930803     C                   MOVEL     FUNAFI        FAT(I)
119100930803     C                   ENDIF
119200930803     C                   ENDIF
119300930806     C     FAT(I)        IFEQ      'E'
119400930806     C                   MOVEL     USER          WEDP              3
119500930806     C     WEDP          IFEQ      'EDP'
119600930806     C                   MOVEL     'S'           FAT(I)
119700930806     C                   ELSE
119800930806     C                   MOVEL     'N'           FAT(I)
119900930806     C                   ENDIF
120000930806     C                   ENDIF
120100930803     C                   ENDDO
120200930803     C                   ENDSR
120300940111     C*------------------------------------------------------------------------*
120400930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
120500930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
120600930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
120700930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
120800940111     C*------------------------------------------------------------------------*
120900930803     C     OUTFUN        BEGSR
121000930805     C     1             DO        8             I
121100930805     C                   MOVEL     *BLANKS       FUV(I)
121200930803     C                   ENDDO
121300930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
121400930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0
121500930803     C     1             DO        31            I
121600930803     C     FAT(I)        IFEQ      'S'
121700930803     C     FDE(I)        ANDNE     *BLANKS
121800930803     C                   ADD       1             FUNNAT
121900930803     C                   ENDIF
122000930803     C                   ENDDO
122100930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
122200930803     C     FUNNAT        IFGT      8
122300930803     C                   MOVEL     'S'           FAT(24)
122400930803     C                   ENDIF
122500930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
122600930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
122700930803     C                   Z-ADD     8             FUNVIS            5 0
122800930803     C     FAT(23)       IFEQ      'S'
122900930803     C                   SUB       1             FUNVIS
123000930803     C                   ENDIF
123100930803     C     FAT(24)       IFEQ      'S'
123200930803     C                   SUB       1             FUNVIS
123300930803     C                   ENDIF
123400930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
123500931015     C                   Z-ADD     *ZEROS        W                 5 0
123600930803     C                   ADD       1             FUNULT
123700930803     C     FUNULT        IFGT      31
123800930803     C                   Z-ADD     1             FUNULT
123900930805     C                   ENDIF
124000930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
124100930803     C     FUNULT        DO        31            I
124200930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
124300930803     C     W             IFLT      FUNVIS
124400930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
124500930803     C     FAT(I)        IFEQ      'S'
124600930803     C     FDE(I)        ANDNE     *BLANKS
124700930805     C     I             IFEQ      23
124800930805     C     I             OREQ      24
124900930803     C                   ELSE
125000930803     C                   ADD       1             W
125100930803     C                   MOVEL     FDE(I)        FUV(W)
125200930803     C                   ENDIF
125300930803     C                   ENDIF
125400930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
125500930803     C                   Z-ADD     I             FUNULT
125600930803     C                   ENDIF
125700930803     C                   ENDDO
125800930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
125900930806     C                   MOVEL     FUV(1)        VFD001
126000930806     C                   MOVEL     FUV(2)        VFD002
126100930806     C                   MOVEL     FUV(3)        VFD003
126200930806     C                   MOVEL     FUV(4)        VFD004
126300930806     C                   MOVEL     FUV(5)        VFD005
126400930806     C                   MOVEL     FUV(6)        VFD006
126500930803     C     FAT(23)       IFEQ      'N'
126600930803     C     FAT(24)       ANDEQ     'N'
126700930806     C                   MOVEL     FUV(7)        VFD007
126800930806     C                   MOVEL     FUV(8)        VFD008
126900930803     C                   ELSE
127000930803     C     FAT(23)       IFEQ      'S'
127100930803     C     FAT(24)       ANDEQ     'S'
127200930806     C                   MOVEL     FDE(23)       VFD007
127300930806     C                   MOVEL     FDE(24)       VFD008
127400930803     C                   ELSE
127500930806     C                   MOVEL     FUV(7)        VFD007
127600930803     C     FAT(23)       IFEQ      'S'
127700930806     C                   MOVEL     FDE(23)       VFD008
127800930803     C                   ELSE
127900930806     C                   MOVEL     FDE(24)       VFD008
128000930805     C                   ENDIF
128100930805     C                   ENDIF
128200930805     C                   ENDIF
128300930803     C                   MOVEL     '1'           $TESVD
128400930803     C                   ENDSR
128500940111     C*------------------------------------------------------------------------*
128600930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
128700930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
128800930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
128900930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
129000940111     C*------------------------------------------------------------------------*
129100930803     C     CONFUN        BEGSR
129200930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
129300930803     C     1             DO        31            I
129400930803     C     KEY           IFEQ      KEYS(I)
129500930805     C* CONTROLLO SE IL TASTO E' ATTIVO
129600930803     C     FAT(I)        IFNE      'S'
129700930804     C                   MOVEL     MSG(1)        $MSG
129800930803     C                   EXSR      CARMSG
129900930803     C                   MOVEL     '1'           $ERR
130000930803     C                   ENDIF
130100930803     C                   ENDIF
130200930803     C                   ENDDO
130300930803     C                   ENDSR
130400940111     C*------------------------------------------------------------------------*
130500930802     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
130600930802     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
130700930803     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
130800930803     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
130900930803     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
131000930803     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
131100940111     C*------------------------------------------------------------------------*
131200930802     C     CAROPZ        BEGSR
131300930803     C                   Z-ADD     *ZEROS        OPZULT            5 0
131400930802     C     1             DO        24            I
131500930803     C                   MOVEL     OPZ(I)        OPZRIG
131600930805     C                   MOVEL     OPZOPD        OPD(I)
131700930805     C                   MOVEL     OPZOPS        OPS(I)
131800930805     C                   MOVEL     OPZDES        ODE(I)
131900020923     C     DUTLPO        IFEQ      'S'
132000930923     C     $TGES         IFEQ      'G'
132100930805     C                   MOVEL     OPZASG        OAT(I)
132200930802     C                   ELSE
132300930805     C                   MOVEL     OPZASI        OAT(I)
132400930802     C                   ENDIF
132500930802     C                   ELSE
132600930923     C     $TGES         IFEQ      'G'
132700930805     C                   MOVEL     OPZAFG        OAT(I)
132800930802     C                   ELSE
132900930805     C                   MOVEL     OPZAFI        OAT(I)
133000930802     C                   ENDIF
133100930802     C                   ENDIF
133200930806     C     OAT(I)        IFEQ      'E'
133300930806     C                   MOVEL     USER          WEDP              3
133400930806     C     WEDP          IFEQ      'EDP'
133500930806     C                   MOVEL     'S'           OAT(I)
133600930806     C                   ELSE
133700930806     C                   MOVEL     'N'           OAT(I)
133800930806     C                   ENDIF
133900930806     C                   ENDIF
134000930802     C                   ENDDO
134100930802     C                   ENDSR
134200940111     C*------------------------------------------------------------------------*
134300930802     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
134400930802     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
134500930802     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
134600930806     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
134700930802     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
134800940111     C*------------------------------------------------------------------------*
134900930802     C     OUTOPZ        BEGSR
135000930805     C     1             DO        8             I
135100930805     C                   MOVEL     *BLANKS       OPV(I)
135200930802     C                   ENDDO
135300930802     C                   Z-ADD     *ZEROS        OPZNAT            2 0
135400930805     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
135500930803     C                   ADD       1             OPZULT
135600930803     C     OPZULT        IFGT      24
135700930803     C                   Z-ADD     1             OPZULT
135800930803     C                   END
135900930805     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
136000930802     C     1             DO        24            I
136100930805     C     OAT(I)        IFEQ      'S'
136200930805     C     ODE(I)        ANDNE     *BLANKS
136300930802     C                   ADD       1             OPZNAT
136400930802     C                   ENDIF
136500930802     C                   ENDDO
136600930802     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
136700931015     C                   Z-ADD     0             W
136800930802     C     OPZULT        DO        24            I
136900930802     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
137000930802     C     W             IFLT      8
137100930802     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
137200930805     C     OAT(I)        IFEQ      'S'
137300930803     C     ODE(I)        ANDNE     *BLANKS
137400930802     C                   ADD       1             W
137500930803     C                   MOVEL     ODE(I)        OPV(W)
137600930805     C                   ENDIF
137700930805     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
137800930802     C                   Z-ADD     I             OPZULT
137900930802     C                   ENDIF
138000930802     C                   ENDDO
138100930802     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
138200930806     C                   MOVEL     OPV(1)        VOD001
138300930806     C                   MOVEL     OPV(2)        VOD002
138400930806     C                   MOVEL     OPV(3)        VOD003
138500930806     C                   MOVEL     OPV(4)        VOD004
138600930806     C                   MOVEL     OPV(5)        VOD005
138700930806     C                   MOVEL     OPV(6)        VOD006
138800930806     C                   MOVEL     OPV(7)        VOD007
138900930806     C                   MOVEL     OPV(8)        VOD008
139000930802     C                   MOVEL     '1'           $TESVD
139100930923     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
139200930923     C     OPZNAT        IFGT      8
139300930923     C                   MOVEL     'S'           FAT(23)
139400930923     C                   ENDIF
139500930802     C                   ENDSR
139600940111     C*------------------------------------------------------------------------*
139700930802     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
139800930806     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
139900930802     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
140000930802     C*                   - ERRORE SE OPZIONE NON ATTIVA
140100940111     C*------------------------------------------------------------------------*
140200930802     C     CONOPZ        BEGSR
140300000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
140400930803     C                   MOVEL     *BLANKS       OPZOK             1
140500930805     C     1             DO        24            I                 5 0
140600930806     C     VSCOPZ        IFEQ      OPD(I)
140700930806     C     VSCOPZ        OREQ      OPS(I)
140800930805     C     OAT(I)        IFEQ      'S'
140900930806     C                   MOVEL     OPD(I)        VSCOPZ
141000940105     C                   MOVEL     'S'           OPZOK
141100930805     C                   ELSE
141200930802     C                   MOVEL     'N'           OPZOK             1
141300930805     C                   ENDIF
141400930802     C                   ENDIF
141500930802     C                   ENDDO
141600930805     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
141700930803     C     OPZOK         IFNE      'S'
141800930804     C                   MOVEL     MSG(3)        $MSG
141900930803     C                   EXSR      CARMSG
142000930803     C                   MOVEL     '1'           $ERR
142100930803     C                   MOVEL     'N'           OPZOK             1
142200930803     C                   ENDIF
142300000000     C                   ENDSR
142400930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
142500940111TRTB50R- Tasto funzionale non valido.                                          1
142600940111TRTB50R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
142700940111TRTB50R- Opzione non valida.                                                   3
142800940111TRTB50R- Opzione abilitata ma non gestita. Avvertire CED.                      4
142900940111TRTB50R- Inizio elenco.                                                        5
143000940111TRTB50R- Fine elenco.                                                          6
143100940112TRTB50R- 7                                                                     7
143200940112TRTB50R- 8                                                                     8
143300940112TRTB50R- Codice filiale errata                                                 9
143400940112TRTB50R- 10                                                                   10
143500940117TRTB50R- Impossibile manutenzionare questo tempo se non si ha il profilo EDP  11
143600940112TRTB50R- Tempo annullato, consentito solo il ripristino                       12
143700940112TRTB50R- Tempo non annullato, impossibile ripristinare                        13
143800940112TRTB50R- 14                                                                   14
143900940112TRTB50R- 15                                                                   15
144000940112TRTB50R- 16                                                                   16
144100940111TRTB50R- 17                                                                   17
144200940111TRTB50R- 18                                                                   18
144300940111TRTB50R- 19                                                                   19
144400940111TRTB50R- 20                                                                   20
144500930729** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
144600930803- 1-1 -                   -    NN     NN
144700940112- 2-2 -2=Modifica         -    SS     SS
144800930803- 3-3 -                   -    NN     NN
144900940112- 4-4 -4=Cancellazione    -    SS     SS
145000940121- 5-5 -5=Visualizzazione  -    SS     SS
145100930803- 6-6 -                   -    NN     NN
145200940112- 7-7 -7=Ripristino       -    SS     SS
145300940104- 8-8 -                   -    NN     NN
145400940111- 9-9 -                   -    NN     NN
145500930806-10-  -                   -    NN     NN
145600930806-11-  -                   -    NN     NN
145700930806-12-  -                   -    NN     NN
145800930803-13-  -                   -    NN     NN
145900930803-14-  -                   -    NN     NN
146000930803-15-  -                   -    NN     NN
146100930803-16-  -                   -    NN     NN
146200930803-17-  -                   -    NN     NN
146300930803-18-  -                   -    NN     NN
146400930803-19-  -                   -    NN     NN
146500930803-20-  -                   -    NN     NN
146600930803-21-  -                   -    NN     NN
146700930803-22-  -                   -    NN     NN
146800930803-23-  -                   -    NN     NN
146900930803-24-  -                   -    NN     NN
147000930730** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
147100930723F1    -                   -    NN     NN
147200930723F2    -                   -    NN     NN
147300930802F3    -F3=Fine            -    SS     SS
147400930723F4    -                   -    NN     NN
147500940112F5    -F5=Rivisualizzaz.  -    SS     SS
147600940112F6    -F6=Inserimento     -    SS     SS
147700930723F7    -                   -    NN     NN
147800930723F8    -                   -    NN     NN
147900930723F9    -                   -    NN     NN
148000930723F10   -                   -    NN     NN
148100930723F11   -                   -    NN     NN
148200930802F12   -F12=Ritorno        -    SS     SS
148300930806F13   -                   -    NN     NN
148400930806F14   -                   -    NN     NN
148500930723F15   -                   -    NN     NN
148600930723F16   -                   -    NN     NN
148700930802F17   -F17=Inizio dati    -    SS     SS
148800930802F18   -F18=Fine dati      -    SS     SS
148900930723F19   -                   -    NN     NN
149000930723F20   -                   -    NN     NN
149100940111F21   -                   -    NN     NN
149200940111F22   -                   -    NN     NN
149300930803F23   -F23=Altre opzioni  -    NN     NN
149400930803F24   -F24=Altri tasti    -    NN     NN
149500930802ENTER -                   -    SS     SS
149600930802ROLLUP-                   -    SS     SS
149700930802ROLLDN-                   -    SS     SS
149800930802HELP  -                   -    NN     NN
149900930802PRINT -                   -    NN     NN
150000930802BSPACE-                   -    NN     NN
150100930802CLEAR -                   -    NN     NN
