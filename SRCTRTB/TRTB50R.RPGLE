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
010000130520     dwost             s                   like(§1xhho)
010100940111     C*------------------------------------------------------------------------*
010200940111     C* MAIN LINE
010300940111     C*------------------------------------------------------------------------*
010400000000     C*
010500000000     C* CICLO FINO A CHE:
010600940111DO  1C     $FINE         DOUEQ     '1'
010700000000     C*
010800930805     C                   MOVEL     '0'           $ERR
010900000000     C*
011000930721     C* CONTROLLA GESTIONE VIDEO
011100940111IF  2C     $GES          IFEQ      'VS'
011200930806     C                   EXSR      GESVS
011300940111E   2C                   ENDIF
011400000000     C*
011500940111E   1C                   ENDDO
011600940111     C* FINE PGM
011700000000     C                   SETON                                        LR
011800940111     C*------------------------------------------------------------------------*
011900930806     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
012000940111     C*------------------------------------------------------------------------*
012100930806     C     CARVS         BEGSR
012200930713     C*
012300930806     C                   MOVEL     'VS'          $GES
012400930713     C*
012500930804     C                   EXSR      CAROPZ
012600930804     C                   EXSR      OUTOPZ
012700930804     C                   MOVEA     FUN2          FUN
012800930804     C                   EXSR      CARFUN
012900930804     C                   EXSR      OUTFUN
013000930713     C*
013100940111     C                   WRITE     TB50DVT
013200940111     C                   WRITE     TB50DVO
013300940111     C                   WRITE     TB50DVF
013400930713     C*
013500930806     C                   EXSR      POSSFL
013600930713     C*
013700930713     C                   ENDSR
013800940111     C*------------------------------------------------------------------------*
013900930806     C* GESVS  - GESTIONE SUBFILE
014000940111     C*------------------------------------------------------------------------*
014100930806     C     GESVS         BEGSR
014200930714     C*
014300940111IF  1C     $TESVD        IFEQ      '1'
014400940111     C                   WRITE     TB50DVT
014500940111     C                   WRITE     TB50DVO
014600940111     C                   WRITE     TB50DVF
014700940111E   1C                   ENDIF
014800930713     C*
014900930805     C                   EXSR      OUTSFL
015000930713     C*
015100930805     C                   EXSR      CONFUN
015200930805     C*
015300940111IF  1C     $ERR          IFEQ      '0'
015400940111SL  2C                   SELECT
015500930804     C     KEY           WHENEQ    F6
015600930804     C                   EXSR      F06GES
015700940111     C                   OTHER
015800930804     C                   EXSR      FUNSFL
015900940111E   2C                   ENDSL
016000940111E   1C                   ENDIF
016100930713     C*
016200930713     C                   ENDSR
016300940111     C*------------------------------------------------------------------------*
016400930804     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
016500940111     C*------------------------------------------------------------------------*
016600930126     C     F06GES        BEGSR
016700930127     C*
016800940111     C* LANCIO L'INSERIMENTO
016900940114     C                   MOVEL     *ZEROS        WLNP              3
017000940121     C                   Z-ADD     DEPHHO        WHH1              4 0
017100940121     C                   Z-ADD     DE1HHO        WHH2              4 0
017200061220     C                   Z-ADD     DEpsot        Wsot              4 0
017300130520     C                   Z-ADD     DEthho        Wost
017400061220     c                   clear                   watb              1
017500940114     C                   MOVEL     'GES'         WOP0              3
017600940114     C                   MOVEL     'F06'         WOP1              3
017700940111     C                   MOVEL     'TRTB51R'     WPGM              8
017800930805     C                   EXSR      LANPGM
017900930128     C*
018000930805     C                   EXSR      POSSFL
018100930127     C*
018200000000     C                   ENDSR
018300940111     C*------------------------------------------------------------------------*
018400930803     C*  GESOPZ - GESTIONE OPZIONI
018500940111     C*------------------------------------------------------------------------*
018600930803     C     GESOPZ        BEGSR
018700930804     C*
018800940111SL  1C                   SELECT
018900930806     C     VSCOPZ        WHENEQ    ' 2'
019000930804     C                   EXSR      O02GES
019100930806     C     VSCOPZ        WHENEQ    ' 4'
019200930804     C                   EXSR      O04GES
019300940121     C     VSCOPZ        WHENEQ    ' 5'
019400940121     C                   EXSR      O05GES
019500930806     C     VSCOPZ        WHENEQ    ' 7'
019600930804     C                   EXSR      O07GES
019700930804     C                   OTHER
019800930804     C                   MOVEL     MSG(4)        $MSG
019900930805     C                   EXSR      CARMSG
020000940111E   1C                   ENDSL
020100930804     C*
020200000000     C                   ENDSR
020300940111     C*------------------------------------------------------------------------*
020400000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
020500940111     C*------------------------------------------------------------------------*
020600000000     C     O02GES        BEGSR
020700930128     C*
020800940111IF  1C     VSCATB        IFEQ      'A'
020900940117     C                   MOVEL     '1'           $ERR
021000940117     C                   MOVEL     MSG(12)       $MSG
021100930721     C                   EXSR      CARMSG
021200940111E   1C                   ENDIF
021300931217     C*
021400940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
021500940117IF  1C     VSCFIL        IFEQ      *BLANKS
021600940117     C                   MOVEL     USER          WEDP              3
021700940117IF  2C     WEDP          IFNE      'EDP'
021800940117     C                   MOVEL     '1'           $ERR
021900940117     C                   MOVEL     MSG(11)       $MSG
022000940117     C                   EXSR      CARMSG
022100940117E   2C                   ENDIF
022200940117E   1C                   ENDIF
022300940117     C*
022400940111IF  1C     $ERR          IFEQ      '0'
022500000000     C*
022600940111     C* LANCIO LA MODIFICA
022700940114     C                   MOVEL     VSCFIL        WLNP
022800940117     C                   Z-ADD     VSCHH1        WHH1
022900940117     C                   Z-ADD     VSCHH2        WHH2
023000061220     C                   Z-ADD     VSCsot        Wsot
023100130520     C                   Z-ADD     VSCost        Wost
023200061220     c                   movel     vscatb        watb
023300940114     C                   MOVEL     'GES'         WOP0
023400940114     C                   MOVEL     'O02'         WOP1
023500940111     C                   MOVEL     'TRTB51R'     WPGM
023600930805     C                   EXSR      LANPGM
023700000000     C*
023800930805     C                   Z-ADD     1             SWMODI
023900150821     c* Se ho modoficato il record generico, ricarico i dati generali
024000150821     c                   if        vscfil<=*zeros
024100150821     c                   exsr      CAROSA
024200150821     c                   endif
024300930128     C*
024400940111E   1C                   ENDIF
024500000000     C*
024600000000     C                   ENDSR
024700940111     C*------------------------------------------------------------------------*
024800000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
024900940111     C*------------------------------------------------------------------------*
025000000000     C     O04GES        BEGSR
025100930128     C*
025200930128     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
025300940111IF  1C     VSCATB        IFEQ      'A'
025400930721     C                   MOVEL     '1'           $ERR
025500930805     C                   MOVEL     MSG(12)       $MSG
025600930721     C                   EXSR      CARMSG
025700940111E   1C                   ENDIF
025800940117     C*
025900940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
026000940117IF  1C     VSCFIL        IFEQ      *BLANKS
026100940117     C                   MOVEL     USER          WEDP              3
026200940117IF  2C     WEDP          IFNE      'EDP'
026300940117     C                   MOVEL     '1'           $ERR
026400940117     C                   MOVEL     MSG(11)       $MSG
026500940117     C                   EXSR      CARMSG
026600940117E   2C                   ENDIF
026700940117E   1C                   ENDIF
026800931217     C*
026900940111IF  1C     $ERR          IFEQ      '0'
027000940111     C*
027100940111     C* LANCIO LA CANCELLAZIONE
027200940114     C                   MOVEL     VSCFIL        WLNP
027300940117     C                   Z-ADD     VSCHH1        WHH1
027400940117     C                   Z-ADD     VSCHH2        WHH2
027500061220     C                   Z-ADD     VSCsot        Wsot
027600130520     C                   Z-ADD     VSCost        Wost
027700061220     c                   movel     vscatb        watb
027800940114     C                   MOVEL     'GES'         WOP0
027900940114     C                   MOVEL     'O04'         WOP1
028000940111     C                   MOVEL     'TRTB51R'     WPGM
028100930201     C                   EXSR      LANPGM
028200000000     C*
028300930805     C                   Z-ADD     1             SWMODI
028400940111     C*
028500940111E   1C                   ENDIF
028600000000     C*
028700000000     C                   ENDSR
028800940121     C*------------------------------------------------------------------------*
028900940121     C*  O05GES - GESTIONE OPZIONE 2=MODIFICA
029000940121     C*------------------------------------------------------------------------*
029100940121     C     O05GES        BEGSR
029200940121     C*
029300940121     C* LANCIO LA VISUALIZZAZIONE
029400940121     C                   MOVEL     VSCFIL        WLNP
029500940121     C                   Z-ADD     VSCHH1        WHH1
029600940121     C                   Z-ADD     VSCHH2        WHH2
029700061220     C                   Z-ADD     VSCsot        Wsot
029800130520     C                   Z-ADD     VSCost        Wost
029900061220     c                   movel     vscatb        watb
030000940121     C                   MOVEL     'GES'         WOP0
030100940121     C                   MOVEL     'O05'         WOP1
030200940121     C                   MOVEL     'TRTB51R'     WPGM
030300940121     C                   EXSR      LANPGM
030400940121     C*
030500940121     C                   ENDSR
030600940111     C*------------------------------------------------------------------------*
030700930128     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
030800940111     C*------------------------------------------------------------------------*
030900930128     C     O07GES        BEGSR
031000930128     C*
031100930128     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
031200940111IF  1C     VSCATB        IFNE      'A'
031300930721     C                   MOVEL     '1'           $ERR
031400930721     C                   MOVEL     MSG(13)       $MSG
031500930721     C                   EXSR      CARMSG
031600940111E   1C                   ENDIF
031700940117     C*
031800940117     C* SE NON SONO EDP NON POSSONO MANUTENZIONARE IL TEMPO 'BBB' (GENERALE)
031900940117IF  1C     VSCFIL        IFEQ      *BLANKS
032000940117     C                   MOVEL     USER          WEDP              3
032100940117IF  2C     WEDP          IFNE      'EDP'
032200940117     C                   MOVEL     '1'           $ERR
032300940117     C                   MOVEL     MSG(11)       $MSG
032400940117     C                   EXSR      CARMSG
032500940117E   2C                   ENDIF
032600940117E   1C                   ENDIF
032700930215     C*
032800940111IF  1C     $ERR          IFEQ      '0'
032900930128     C*
033000940111     C* LANCIO IL RIPRISTINO
033100940114     C                   MOVEL     VSCFIL        WLNP
033200940117     C                   Z-ADD     VSCHH1        WHH1
033300940117     C                   Z-ADD     VSCHH2        WHH2
033400061220     C                   Z-ADD     VSCsot        Wsot
033500130520     C                   Z-ADD     VSCost        Wost
033600061220     c                   movel     vscatb        watb
033700940114     C                   MOVEL     'GES'         WOP0
033800940114     C                   MOVEL     'O07'         WOP1
033900940111     C                   MOVEL     'TRTB51R'     WPGM
034000930201     C                   EXSR      LANPGM
034100930128     C*
034200930128     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
034300930128     C                   Z-ADD     1             SWMODI
034400930128     C*
034500940111E   1C                   ENDIF
034600930128     C*
034700930128     C                   ENDSR
034800940111     C*------------------------------------------------------------------------*
034900930205     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
035000940111     C*------------------------------------------------------------------------*
035100930205     C     NEWPOS        BEGSR
035200930205     C*
035300940111     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
035400940111     C*        1 = RICHIESTO POSIZIONAMENTO
035500930205     C                   Z-ADD     0             SWPOSI            7 0
035600930205     C*
035700940111     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
035800940111     C*            NON OCCORRE RICARICARE IL SUBFILE
035900940111     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
036000930205     C                   Z-ADD     0             SWMODI            1 0
036100930205     C*
036200930205     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
036300940111IF  1C     VCCFIL        IFNE      DEPFIL
036400930205     C*
036500930205     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
036600930205     C                   EXSR      CHKPAR
036700930205     C*
036800930205     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
036900940111IF  2C     $ERR          IFEQ      '0'
037000940111     C                   Z-ADD     1             SWPOSI
037100940111     C                   MOVEL     VCCFIL        DEPFIL
037200940111E   2C                   ENDIF
037300940111E   1C                   ENDIF
037400930205     C*
037500930205     C                   ENDSR
037600940111     C*------------------------------------------------------------------------*
037700940111     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
037800940111     C*------------------------------------------------------------------------*
037900940111     C     CHKPAR        BEGSR
038000940111     C*
038100940111     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
038200940111     C                   SETOFF                                       80
038300940111     C*
038400940114     C* CONTROLLO ESISTENZA FILIALE, SOLO SE DIVERSA DA BLANKS (VALIDA X IL PGM)
038500940112     C                   MOVEL     VCCFIL        WFIL              3
038600940112     C                   MOVEL     *BLANKS       WDFIL            20
038700940113IF  1C     VCCFIL        IFNE      *BLANKS
038800940114     C                   EXSR      CHKFIL
038900940113IF  2C     $ERR          IFEQ      '1'
039000940111     C                   MOVEL     '1'           $ERR
039100940111     C                   MOVEL     MSG(9)        $MSG
039200940111     C                   EXSR      CARMSG
039300940111     C                   SETON                                        80
039400940113E   2C                   ENDIF
039500940113E   1C                   ENDIF
039600940113     C                   MOVEL     WDFIL         VCDFIL
039700940111     C*
039800940111     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
039900940111     C* CURSORE SUL CAMPO DEL CONTROL
040000940111IF  1C     $ERR          IFEQ      '1'
040100940111     C                   MOVEL     '0'           *IN43
040200940111X   1C                   ELSE
040300940111     C                   MOVEL     '1'           *IN43
040400940111E   1C                   ENDIF
040500940111     C*
040600940111     C                   ENDSR
040700940111     C*------------------------------------------------------------------------*
040800940111     C*  CHKFIL - CONTROLLA LA FILIALE
040900940111     C*------------------------------------------------------------------------*
041000940114     C     CHKFIL        BEGSR
041100940111     C*
041200940114     C* INSERIRE CARATTERI NON NUMERICI, ERRORE
041300940114IF  1C     WFIL          IFLT      *ZEROS
041400940114     C                   MOVEL     '1'           $ERR
041500940114X   1C                   ELSE
041600940114     C*
041700940114     C* CONTROLLO ESISTENZA NELL'ORGANIGRAMMA
041800940114     C                   MOVEL     WFIL          KORFIL
041900940114     C     KEYORG        CHAIN     AZORG01L                           99
042000940114IF  2C     *IN99         IFEQ      '1'
042100940114     C     ORGFVA        ORNE      *BLANKS
042200940114     C                   MOVEL     '1'           $ERR
042300940114     C                   MOVEL     *ALL'*'       WDFIL
042400940114X   2C                   ELSE
042500940114     C                   MOVEL     ORGDES        WDFIL
042600940114E   2C                   ENDIF
042700940114E   1C                   ENDIF
042800940113     C*
042900940114     C                   ENDSR
043000940112     C*------------------------------------------------------------------------*
043100940112     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD TABELLE
043200940112     C*------------------------------------------------------------------------*
043300940112     C     SETFIL        BEGSR
043400940112     C*
043500940112     C* POSIZIONAMENTO RECORD TABELLA 'OSA'
043600940114     C                   MOVEL     WFIL          OSA               6
043700940112     C                   MOVE      'OSA'         OSA
043800940112     C*
043900020923     C                   MOVEL     1             KTBKUT
044000940112     C                   MOVEL     '1X'          KTBCOD
044100940112     C                   MOVEL     *BLANKS       KTBKEY
044200940112     C                   MOVEL     OSA           KTBKEY
044300940112     C*
044400940113     C     KEYTAB        SETLL     TABEL00F                           99
044500940112     C*
044600960418     C                   EXSR      LETFIL
044700940112     C*
044800940112     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
044900940112IF  1C     *IN99         IFEQ      '1'
045000940112     C                   Z-ADD     1             SFLEOF
045100940112E   1C                   ENDIF
045200940112     C*
045300940112     C                   ENDSR
045400940112     C*------------------------------------------------------------------------*
045500971013     C* LETFIL - LEGGE RECORD SUCCESSIVO
045600940112     C*------------------------------------------------------------------------*
045700940112     C     LETFIL        BEGSR
045800940112     C*
045900940113     C* FINO A QUANDO NON TROVI UN RECORD VALIDO (RECOK = '1') STAI DENTRO
046000940113     C                   MOVEL     '0'           RECOK             1
046100940113DO  1C     RECOK         DOWEQ     '0'
046200940112     C*
046300940112     C* LETTURA RECORD TABELLA 'OSA'
046400940113     C                   READ      TABEL00F                               99
046500940112     C*
046600940113     C* FINE FILE  O TABELLA DIVERSA
046700940112IF  2C     *IN99         IFEQ      '1'
046800940113     C     TBLKUT        ORNE      1
046900940113     C     TBLCOD        ORNE      '1X'
047000940112     C                   Z-ADD     1             SFLEOF
047100940113     C                   MOVEL     '1'           RECOK
047200940112X   2C                   ELSE
047300940112     C*
047400940112     C* CONTROLLO RECORD VALIDO
047500940113     C                   MOVE      TBLKEY        TIPKEY            5
047600940113IF  3C     TIPKEY        IFEQ      'OSA  '
047700940113     C                   MOVEL     '1'           RECOK
047800940113E   3C                   ENDIF
047900940112E   2C                   ENDIF
048000940112E   1C                   ENDDO
048100940112     C*
048200940112     C                   ENDSR
048300940111     C*------------------------------------------------------------------------*
048400000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
048500940111     C*------------------------------------------------------------------------*
048600000000     C     IRESFL        BEGSR
048700940112     C*
048800940112     C* RIEMPIMENTO CAMPI DI "OSA"
048900940112IF  1C     TBLFLG        IFNE      *BLANKS
049000940112     C                   MOVEL     'A'           VSCATB
049100940112X   1C                   ELSE
049200940112     C                   MOVEL     ' '           VSCATB
049300940113E   1C                   ENDIF
049400940112     C                   MOVEL     TBLKEY        VSCFIL
049500940112     C                   MOVEL     TBLUNI        DS1X
049600940112     C                   Z-ADD     §1XHHO        VSCHH1
049700061220     c* Decodifica filiale
049800061220     c                   movel     vscfil        w0030             3 0
049900130520     c                   if        w0030=0
050000130520     c                   movel(p)  'STANDARD'    vsdfil
050100130520     c                   else
050200061220     c     w0030         chain     azorg01l
050300061220     c                   if        %found(azorg01l)
050400061220     c                   movel     orgdes        vsdfil
050500061220     c                   else
050600061220     c                   movel     *all'?'       vsdfil
050700061220     c                   endif
050800130520     c                   endif
050900940112     C*
051000940112     C* REPERISCO IL CORRISPONDENTE RECORD OSA1
051100940112     C                   MOVEL     *BLANKS       ALF7              7
051200940112     C                   MOVEL     TBLKEY        ALF7
051300940112     C                   MOVE      '1'           ALF7
051400940113     C                   MOVEL     *BLANKS       KTBKEY
051500940113     C                   MOVEL     ALF7          KTBKEY
051600940112     C     KEYTAB        CHAIN     TABEL00F                           98
051700940112     C*
051800940112     C* RIEMPIMENTO CAMPI DI "OSA1"
051900940112IF  1C     *IN98         IFEQ      '0'
052000940113     C                   MOVEL     TBLUNI        DS1X
052100940112     C                   Z-ADD     §1XHHO        VSCHH2
052200940112X   1C                   ELSE
052300940112     C                   Z-ADD     9999          VSCHH2
052400940112E   1C                   ENDIF
052500130520
052600130520     c                   clear                   trul09ds
052700130520     c                   movel     vscfil        i09tfa
052800130520     c                   movel     'TFA'         i09mod
052900130520     c                   call      'TRUL09R'
053000130520     c                   parm                    trul09ds
053100130520
053200130520     c* Se 1X OSA annullata, carico anche OST annullata, altrimenti ignoro
053300130520     c                   clear                   vscost
053400130520
053500130520     C                   MOVEL(p)  TBLKEY        ALF7
053600130520     C                   MOVE      'T '          ALF7
053700130520     C                   MOVEL(p)  ALF7          KTBKEY
053800130520     c     keytab        chain     tabel00f
053900130520     c*
054000130520     c                   if        (not %found(tabel00f) or
054100130520     c                             (tblflg<>' ' and vscatb=' '))
054200130520     c                             and o09err=' '
054300130520     c                   movel     dethho        vscost
054400130520     c                   endif
054500130520     c
054600130520     c                   if        %found(tabel00f) and(tblflg=' ' or
054700130520     c                             tblflg<>' ' and vscatb<>' ')
054800130520     c                   movel     tbluni        ds1x
054900130520     C                   Z-ADD     §1XHHO        vscost
055000130520     c                   endif
055100061218     c
055200061218     c* Verifico se presente tabella SOT, solo se si tratta di terminal
055300061218     c*  di arrivo
055400061218     c
055500061220     c                   clear                   vscsot
055600061220     c* Se 1X annullata, carico anche sot annullata, altrimenti ignoro
055700150821     c                   if        vscfil<=*zeros
055800150821     c                   movel     'GEN'         kke1
055900150821     c                   else
056000061220     c                   movel     vscfil        kke1
056100150821     c                   endif
056200061220     c     ktbe          chain     tntbe01l
056300061220     c*
056400061220     c                   if        (not %found(tntbe01l) or
056500061220     c                             (tbeatb<>' ' and vscatb=' '))
056600061220     c                             and o09err=' '
056700061220     c                   movel     depsot        vscsot
056800061220     c                   endif
056900061220     c
057000061220     c                   if        %found(tntbe01l) and(tbeatb=' ' or
057100061220     c                             tbeatb<>' ' and vscatb<>' ')
057200061220     c                   movel     tbeuni        vscsot
057300061220     c                   endif
057400061220     c
057500061218     c
057600000000     C                   ENDSR
057700940111     C*------------------------------------------------------------------------*
057800930201     C* LANPGM - LANCIA UN PROGRAMMA
057900940111     C*------------------------------------------------------------------------*
058000930201     C     LANPGM        BEGSR
058100940114     C*
058200940114     C*--------------------
058300940114     C* PARAMETRI INFORMATIVI
058400940114     C*--------------------
058500940114     C                   MOVEL     WOP0          D50OP0
058600940114     C                   MOVEL     WOP1          D50OP1
058700940114     C                   MOVEL     '0'           D50F03
058800940114     C                   MOVEL     '0'           D50ERR
058900940114     C                   MOVEL     *BLANKS       D50MSG
059000940114     C*--------------------
059100940114     C* PARAMETRI
059200940114     C*--------------------
059300940114     C                   MOVEL     WLNP          D50FIL
059400940117     C                   Z-ADD     WHH1          OSAHHO
059500940117     C                   Z-ADD     WHH2          OS1HHO
059600061220     C                   Z-ADD     Wsot          d50sot
059700130520     C                   Z-ADD     Wost          osthho
059800061220     c                   movel     watb          d50atb
059900940114     C* LANCIO
060000940114     C                   CALL      WPGM
060100940114     C                   PARM                    DS50
060200940114     C*
060300930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
060400940114IF  1C     D50F03        IFEQ      '1'
060500930721     C                   MOVEL     '1'           $F03
060600930721     C                   MOVEL     '1'           $ERR
060700940105     C                   MOVEL     '1'           $FINE
060800940114     C*
060900930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
061000940114X   1C                   ELSE
061100940112     C                   MOVEL     D50MSG        $MSG
061200930721     C                   EXSR      CARMSG
061300940114E   1C                   ENDIF
061400930201     C*
061500930201     C                   ENDSR
061600940114     C*------------------------------------------------------------------------*
061700940114     C*  CAROSA - CARICAMENTO DATI DI OSA E OSA1 CHE NON CHIEDO IN INSERIMENTO
061800940114     C*------------------------------------------------------------------------*
061900940114     C     CAROSA        BEGSR
062000940114     C*
062100940114     C* CARICO I DATI DI 'OSA'
062200940114     C*
062300020923     C                   MOVEL     1             KTBKUT
062400940114     C                   MOVEL     '1X'          KTBCOD
062500940114     C                   MOVEL     *BLANKS       KTBKEY
062600940119     C                   MOVEL     *BLANKS       OSA               6
062700940119     C                   MOVE      'OSA'         OSA
062800940119     C                   MOVEL     OSA           KTBKEY
062900940114     C*
063000940114     C     KEYTAB        CHAIN     TABEL00F                           99
063100940114IF  1C     *IN99         IFEQ      *OFF
063200940114     C                   MOVEL     TBLUNI        DS1X
063300940114X   1C                   ELSE
063400940119     C                   CLEAR                   DS1X
063500940114E   1C                   ENDIF
063600940114     C*
063700940114     C* MUOVO I CAMPI DI 'OSA' IN QUELLI DELLA DS DI PROCEDURA PER IL PASSAGGIO
063800940114     C                   MOVEL     §1XDES        OSADES
063900940114     C                   MOVEL     §1XFI1        OSAFI1
064000940114     C                   Z-ADD     §1XHHG        OSAHHG
064100940114     C                   MOVEL     §1XFI2        OSAFI2
064200940121     C                   Z-ADD     §1XHHO        OSAHHO
064300940114     C                   MOVEL     §1XFI3        OSAFI3
064400940114     C                   Z-ADD     §1XHHM        OSAHHM
064500940114     C                   MOVEL     §1XFI4        OSAFI4
064600940114     C                   Z-ADD     §1XHHP        OSAHHP
064700940114     C                   MOVEL     §1XFI5        OSAFI5
064800940114     C                   Z-ADD     §1XHHI        OSAHHI
064900940114     C                   MOVEL     §1XFI6        OSAFI6
065000940114     C                   Z-ADD     §1XHHS        OSAHHS
065100940121     C                   Z-ADD     §1XHHO        DEPHHO
065200940114     C*
065300940114     C* CARICO I DATI DI 'OSA1'
065400940114     C                   MOVEL     *BLANKS       OSA1              7
065500940114     C                   MOVE      'OSA1'        OSA1
065600940114     C*
065700020923     C                   MOVEL     1             KTBKUT
065800940114     C                   MOVEL     '1X'          KTBCOD
065900940114     C                   MOVEL     *BLANKS       KTBKEY
066000940114     C                   MOVEL     OSA1          KTBKEY
066100940114     C*
066200940114     C     KEYTAB        CHAIN     TABEL00F                           99
066300940114IF  1C     *IN99         IFEQ      *OFF
066400940114     C                   MOVEL     TBLUNI        DS1X
066500940114X   1C                   ELSE
066600940114     C                   CLEAR                   DS1X
066700940114E   1C                   ENDIF
066800940114     C*
066900940114     C* MUOVO I CAMPI DI 'OSA1' IN QUELLI DELLA DS DI PROCEDURA PER IL PASSAGGIO
067000940114     C                   MOVEL     §1XDES        OS1DES
067100940114     C                   MOVEL     §1XFI1        OS1FI1
067200940114     C                   Z-ADD     §1XHHG        OS1HHG
067300940114     C                   MOVEL     §1XFI2        OS1FI2
067400940121     C                   Z-ADD     §1XHHO        OS1HHO
067500940114     C                   MOVEL     §1XFI3        OS1FI3
067600940114     C                   Z-ADD     §1XHHM        OS1HHM
067700940114     C                   MOVEL     §1XFI4        OS1FI4
067800940114     C                   Z-ADD     §1XHHP        OS1HHP
067900940114     C                   MOVEL     §1XFI5        OS1FI5
068000940114     C                   Z-ADD     §1XHHI        OS1HHI
068100940114     C                   MOVEL     §1XFI6        OS1FI6
068200940114     C                   Z-ADD     §1XHHS        OS1HHS
068300940121     C                   Z-ADD     §1XHHO        DE1HHO
068400940114     C*
068500061220     c* Carico il dato GEN della tabella SOT
068600061220     C                   CLEAR                   DSBS02
068700061220     C                   MOVEL     'C'           T02MOD
068800061220     C                   MOVEL     KNSIF         T02SIF
068900061220     C                   MOVEL     'SOT'         T02COD
069000061220     C                   MOVEL     'GEN'         T02KE1
069100061220     C                   CALL      'TIBS02R'
069200061220     C                   PARM                    KPJBA
069300061220     C                   PARM                    DSBS02
069400061220     c                   movel     t02uni        depsot
069500130517
069600130517     C* CARICO I DATI DI 'OST'
069700130517     C*
069800130517     C                   MOVEL     1             KTBKUT
069900130517     C                   MOVEL     '1X'          KTBCOD
070000130517     C                   MOVEL     *BLANKS       KTBKEY
070100130517     C                   MOVEL     *BLANKS       OSA               6
070200130517     C                   MOVE      'OST'         OSA
070300130517     C                   MOVEL     OSA           KTBKEY
070400130517     C*
070500130517     C     KEYTAB        CHAIN     TABEL00F                           99
070600130517IF  1C     *IN99         IFEQ      *OFF
070700130517     C                   MOVEL     TBLUNI        DS1X
070800130517X   1C                   ELSE
070900130517     C                   CLEAR                   DS1X
071000130517E   1C                   ENDIF
071100130517     C*
071200130517     C* MUOVO I CAMPI DI 'OST' IN QUELLI DELLA DS DI PROCEDURA PER IL PASSAGGIO
071300130517     C                   MOVEL     §1XDES        OSTDES
071400130517     C                   MOVEL     §1XFI1        OSTFI1
071500130517     C                   Z-ADD     §1XHHG        OSTHHG
071600130517     C                   MOVEL     §1XFI2        OSTFI2
071700130517     C                   Z-ADD     §1XHHO        OSTHHO
071800130517     C                   MOVEL     §1XFI3        OSTFI3
071900130517     C                   Z-ADD     §1XHHM        OSTHHM
072000130517     C                   MOVEL     §1XFI4        OSTFI4
072100130517     C                   Z-ADD     §1XHHP        OSTHHP
072200130517     C                   MOVEL     §1XFI5        OSTFI5
072300130517     C                   Z-ADD     §1XHHI        OSTHHI
072400130517     C                   MOVEL     §1XFI6        OSTFI6
072500130517     C                   Z-ADD     §1XHHS        OSTHHS
072600130517     C                   Z-ADD     §1XHHO        DETHHO
072700061220     c
072800940114     C                   ENDSR
072900020923     C*-------------------------------------------------------------------------
073000020923     C* REPERISCE I DATI UTENTE
073100020923     C*-------------------------------------------------------------------------
073200020923     C     REPDATIUTE    BEGSR
073300020923     C*
073400020923     C* INIZIALIZZA VARIABILI DI WRK
073500020923     C                   CLEAR                   TIBS34DS
073600020923     C                   CLEAR                   AZUTEDS
073700020923     C                   CLEAR                   DDATIUTE
073800020923     C*
073900020923     C     *DTAARA       DEFINE    §azute        azuteds
074000020923     C     *DTAARA       DEFINE    §datiute      ddatiute
074100020923     C                   IN(E)     *DTAARA
074200020923if  1C                   IF        %Error
074300020923     c                   EVAL      I34Tla = 'L'
074400020923     C                   CALL      'TIBS34R'
074500020923     C                   PARM                    Tibs34Ds
074600020923     C                   IN        *DTAARA
074700020923e   1C                   ENDIF
074800020923     C*
074900020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
075000020923     C                   MOVEL(P)  RSUT          DSFIRS           20
075100020923     C*
075200020923     C                   ENDSR
075300940111     C*------------------------------------------------------------------------*
075400930713     C*  *INZSR - ROUTINE INIZIALE
075500940111     C*------------------------------------------------------------------------*
075600930713     C     *INZSR        BEGSR
075700000000     C*
075800000000     C* RICEVIMENTO PARAMETRI
075900000000     C     *ENTRY        PLIST
076000940105     C                   PARM                    KPJBA
076100940111     C*
076200940111     C* DEFINISCE LE VARIABILI DI PROGRAMMA
076300940111     C     *LIKE         DEFINE    VCCFIL        DEPFIL
076400940111     C     *LIKE         DEFINE    TBLKUT        KTBKUT
076500940111     C     *LIKE         DEFINE    TBLCOD        KTBCOD
076600940111     C     *LIKE         DEFINE    TBLKEY        KTBKEY
076700940121     C     *LIKE         DEFINE    ORGFIL        KORFIL
076800940121     C     *LIKE         DEFINE    OSAHHO        DEPHHO
076900940121     C     *LIKE         DEFINE    OS1HHO        DE1HHO
077000061220     C     *LIKE         DEFINE    OS1HHO        DEpsot
077100130520     C     *LIKE         DEFINE    OSTHHO        DETHHO
077200930310     C*
077300930310     C* CHIAVE LETTURA TABEL00F - COMPLETA
077400940112     C     KEYTAB        KLIST
077500940111     C                   KFLD                    KTBKUT
077600940111     C                   KFLD                    KTBCOD
077700940111     C                   KFLD                    KTBKEY
077800061220     C     Ktbe          KLIST
077900061220     C                   KFLD                    KCOD
078000061220     C                   KFLD                    KKE1
078100061220     C                   KFLD                    KKE2
078200061220     C*
078300940112     C*
078400940114     C* CHIAVE LETTURA TABEL00F - PARZIALE
078500940112     C     KEYTA2        KLIST
078600940112     C                   KFLD                    KTBKUT
078700940112     C                   KFLD                    KTBCOD
078800940114     C*
078900940114     C* CHIAVE LETTURA AZORG01L - COMPLETA
079000940114     C     KEYORG        KLIST
079100940114     C                   KFLD                    KORFIL
079200930714     C*
079300930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
079400930803     C                   EXSR      INZSTD
079500930803     C*
079600930803     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
079700930803     C                   EXSR      INZSFL
079800930714     C*
079900930806     C* IMPOSTO CAMPI TESTATA VIDEO
080000940114     C                   MOVEL     DSFIRS        VTDRSU
080100980130     C*
080200980130     C* CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
080300061220     C**                 CLEAR                   UL12DS
080400061220     C**                 MOVEL     'L'           I12TLA
080500061220     C**                 MOVEL     '1X'          I12COD
080600061220     C**                 MOVEL     KNSIF         I12NSI
080700061220     C**                 CALL      'TRUL12R'
080800061220     C**                 PARM                    UL12DS
080900061220     C**   O12ERR        IFEQ      '1'
081000061220     C**                 MOVEL     '1'           $FINE
081100061220     C**                 ENDIF
081200940104     C*
081300940114     C* CARICA I DATI DI OSA E OSA1
081400940114     C                   EXSR      CAROSA
081500940114     C*
081600940114     C* IMPOSTO I DEPOSITI PER CONFRONTI SUCCESSIVI
081700960418     C                   MOVEL     '***'         DEPFIL
081800940114     C*
081900940104     C* EMETTO VIDEATA SUBFILE
082000940114     C                   EXSR      CARVS
082100940114     C*
082200000000     C                   ENDSR
082300940111     C*------------------------------------------------------------------------*
082400940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
082500940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
082600940111     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
082700940111     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
082800940111     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
082900940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083000940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083100940111     C*------------------------------------------------------------------------*
083200930803     C* INZSTD - ROUTINE INIZIALE STANDARD
083300940111     C*------------------------------------------------------------------------*
083400930803     C     INZSTD        BEGSR
083500930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
083600930802     C                   CALL      'XKEYS'
083700930802     C                   PARM                    DSKEYS
083800930805     C                   MOVEA     KEYRIG        KEYS
083900930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
084000930714     C     XMSGPA        PLIST
084100930714     C                   PARM                    DSMSMS
084200930714     C                   PARM                    DSMSPG
084300930714     C                   PARM                    DSMSKE
084400930714     C                   PARM                    DSMSER
084500930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
084600940111     C                   MOVEL     'TRTB50R'     DSMSPG
084700940111     C                   MOVEL     'TRTB50R'     VMCQUE
084800930806     C                   MOVEL     *BLANKS       VMCKEY
084900930714     C                   MOVEL     *BLANKS       DSMSMS
085000930805     C* INIZIALIZZO FLAG CONTROLLO PGM
085100930721     C                   MOVEL     '0'           $ERR              1
085200930721     C                   MOVEL     '0'           $F03              1
085300930806     C                   MOVEL     *BLANKS       $GES              2
085400930721     C                   MOVEL     *BLANKS       $MSG             80
085500930721     C                   MOVEL     '0'           $FINE             1
085600930721     C                   MOVEL     '0'           $TESVD            1
085700940112     C                   MOVEL     'G'           $TGES             1
085800930714     C* LANCIO ROUTINE PER VERIFICARE SE
085900930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
086000930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
086100020923     C                   EXSR      REPDATIUTE
086200930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
086300930721     C                   BITOFF    '01234567'    POSMI1            1
086400930721     C                   BITOFF    '01234567'    POSMI2            1
086500930721     C                   BITOFF    '01234567'    POSMA1            1
086600930721     C                   BITOFF    '01234567'    POSMA2            1
086700930721     C                   BITON     '34'          POSMI1            1
086800930721     C                   BITON     '7'           POSMI2            1
086900930721     C                   BITON     '34'          POSMA1            1
087000930721     C                   BITON     '0'           POSMA2            1
087100930721     C                   MOVE      POSMI2        POSMIN            2
087200930721     C                   MOVEL     POSMI1        POSMIN
087300930721     C                   MOVE      POSMA2        POSMAX            2
087400930721     C                   MOVEL     POSMA1        POSMAX
087500930714     C                   ENDSR
087600940111     C*------------------------------------------------------------------------*
087700930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
087800940111     C*------------------------------------------------------------------------*
087900930804     C     FUNSTD        BEGSR
088000930804     C                   SELECT
088100930804     C     KEY           WHENEQ    F3
088200930804     C                   EXSR      F03STD
088300930804     C     KEY           WHENEQ    F12
088400930804     C                   EXSR      F12STD
088500930804     C     KEY           WHENEQ    F24
088600930804     C                   EXSR      F24STD
088700930804     C                   OTHER
088800930804     C                   MOVEL     MSG(2)        $MSG
088900930804     C                   EXSR      CARMSG
089000930804     C                   ENDSL
089100930804     C                   ENDSR
089200940111     C*------------------------------------------------------------------------*
089300930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
089400940111     C*------------------------------------------------------------------------*
089500930804     C     F03STD        BEGSR
089600930804     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
089700930923     C                   MOVEL     '1'           $F03
089800930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
089900930804     C                   MOVEL     '1'           $FINE
090000930804     C                   ENDSR
090100940111     C*------------------------------------------------------------------------*
090200930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
090300940111     C*------------------------------------------------------------------------*
090400930804     C     F12STD        BEGSR
090500930805     C* IMPOSTO FLAG DI FIN PGM
090600930804     C                   MOVEL     '1'           $FINE
090700930804     C                   ENDSR
090800940111     C*------------------------------------------------------------------------*
090900930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
091000940111     C*------------------------------------------------------------------------*
091100930804     C     F24STD        BEGSR
091200930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
091300930804     C                   EXSR      OUTFUN
091400930804     C                   ENDSR
091500940111     C*------------------------------------------------------------------------*
091600930804     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
091700940111     C*------------------------------------------------------------------------*
091800930804     C     INZSFL        BEGSR
091900930804     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
092000930806     C     *LIKE         DEFINE    VCCNBR        SFLPAG
092100930806     C     *LIKE         DEFINE    VCCNBR        SFLEOF
092200930806     C     *LIKE         DEFINE    VCCNBR        SFLERR
092300930806     C     *LIKE         DEFINE    VCCNBR        SFLULT
092400930806     C     *LIKE         DEFINE    VCCNBR        WFLULT
092500930806     C     *LIKE         DEFINE    VCCNBR        SFLRIG
092600930804     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
092700930804     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
092800930804     C                   Z-ADD     12            SFLPAG
092900930804     C                   ENDSR
093000940111     C*------------------------------------------------------------------------*
093100930804     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
093200940111     C*------------------------------------------------------------------------*
093300930804     C     FUNSFL        BEGSR
093400930804     C                   SELECT
093500930804     C     KEY           WHENEQ    ENTER
093600930804     C                   EXSR      ENTSFL
093700930805     C     KEY           WHENEQ    F3
093800930805     C                   EXSR      F03STD
093900930804     C     KEY           WHENEQ    F5
094000930804     C                   EXSR      F05STD
094100930805     C     KEY           WHENEQ    F12
094200930805     C                   EXSR      F12STD
094300930804     C     KEY           WHENEQ    F17
094400930804     C                   EXSR      F17STD
094500930804     C     KEY           WHENEQ    F18
094600930804     C                   EXSR      F18STD
094700930804     C     KEY           WHENEQ    F23
094800930804     C                   EXSR      F23STD
094900930805     C     KEY           WHENEQ    F24
095000930805     C                   EXSR      F24STD
095100930804     C     KEY           WHENEQ    ROLLUP
095200930804     C                   EXSR      RUPSTD
095300930804     C     KEY           WHENEQ    ROLLDN
095400930804     C                   EXSR      RDNSTD
095500930804     C                   OTHER
095600930804     C                   MOVEL     MSG(2)        $MSG
095700930804     C                   EXSR      CARMSG
095800930804     C                   ENDSL
095900930804     C                   ENDSR
096000940111     C*------------------------------------------------------------------------*
096100930804     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
096200940111     C*------------------------------------------------------------------------*
096300930804     C     F05STD        BEGSR
096400930804     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
096500930804     C                   EXSR      POSSFL
096600930804     C                   ENDSR
096700940111     C*------------------------------------------------------------------------*
096800930804     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
096900940111     C*------------------------------------------------------------------------*
097000930804     C     F17STD        BEGSR
097100930804     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
097200930806     C                   Z-ADD     1             VCCNBR
097300930804     C                   ENDSR
097400940111     C*------------------------------------------------------------------------*
097500930804     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
097600940111     C*------------------------------------------------------------------------*
097700930804     C     F18STD        BEGSR
097800930804     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
097900930806     C                   Z-ADD     SFLULT        VCCNBR
098000930804     C                   ENDSR
098100940111     C*------------------------------------------------------------------------*
098200930804     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
098300940111     C*------------------------------------------------------------------------*
098400930804     C     F23STD        BEGSR
098500930804     C* GESTIONE VISUALIZZAZIONE OPZIONI
098600930804     C                   EXSR      OUTOPZ
098700930804     C                   ENDSR
098800940111     C*------------------------------------------------------------------------*
098900930804     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
099000940111     C*------------------------------------------------------------------------*
099100930804     C     RUPSTD        BEGSR
099200930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
099300930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
099400930805     C     CURSOR        IFGE      POSMIN
099500930805     C     CURSOR        ANDLE     POSMAX
099600930804     C                   ELSE
099700930805     C                   EXSR      CARSFL
099800930805     C                   ENDIF
099900930804     C                   ENDSR
100000940111     C*------------------------------------------------------------------------*
100100930804     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
100200940111     C*------------------------------------------------------------------------*
100300930804     C     RDNSTD        BEGSR
100400930805     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
100500930805     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
100600930804     C     CURSOR        IFGE      POSMIN
100700930804     C     CURSOR        ANDLE     POSMAX
100800930804     C                   ELSE
100900930804     C* INVIO MESSAGGIO DI INIZIO ELENCO
101000930804     C                   MOVEL     '1'           $ERR
101100930804     C                   MOVEL     MSG(5)        $MSG
101200930804     C                   EXSR      CARMSG
101300930805     C                   ENDIF
101400930804     C                   ENDSR
101500940111     C*------------------------------------------------------------------------*
101600930804     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
101700940111     C*------------------------------------------------------------------------*
101800930804     C     ENTSFL        BEGSR
101900930804     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
102000930804     C                   EXSR      NEWPOS
102100930804     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
102200930804     C     $ERR          IFEQ      '0'
102300930804     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
102400930804     C     SWPOSI        IFNE      0
102500930804     C                   EXSR      POSSFL
102600930804     C                   ELSE
102700930804     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
102800930804     C     SFLULT        IFNE      0
102900930804     C                   EXSR      OPZSFL
103000930804     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
103100930804     C     SWMODI        IFNE      0
103200930804     C                   EXSR      POSSFL
103300930805     C                   ENDIF
103400930805     C                   ENDIF
103500930805     C                   ENDIF
103600930805     C                   ENDIF
103700930804     C                   ENDSR
103800940111     C*------------------------------------------------------------------------*
103900940111     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
104000940111     C*------------------------------------------------------------------------*
104100930804     C     POSSFL        BEGSR
104200930804     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
104300930804     C                   EXSR      CLRSFL
104400930804     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
104500930804     C                   EXSR      SETFIL
104600930804     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
104700930804     C     SFLEOF        IFEQ      0
104800930804     C                   EXSR      CARSFL
104900930805     C                   ENDIF
105000930804     C                   ENDSR
105100940111     C*------------------------------------------------------------------------*
105200930804     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
105300940111     C*------------------------------------------------------------------------*
105400930804     C     OPZSFL        BEGSR
105500930802     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
105600930802     C                   MOVEL     'S'           OPZOK             1
105700000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
105800000000     C                   Z-ADD     0             SFLVAR            5 0
105900000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
106000000000     C                   Z-ADD     0             SFLERR
106100000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
106200000000     C                   Z-ADD     SFLULT        WFLULT
106300000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
106400940111     C                   READC     TB50DVS                                49
106500930806     C     *IN49         DOWEQ     '0'
106600000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
106700000000     C                   ADD       1             SFLVAR
106800930802     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
106900930805     C     OPZOK         IFEQ      'S'
107000930802     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
107100930806     C     VSCOPZ        IFNE      *BLANKS
107200930802     C                   EXSR      CONOPZ
107300930802     C     OPZOK         IFEQ      'S'
107400930802     C                   EXSR      GESOPZ
107500930805     C                   ENDIF
107600930805     C                   ENDIF
107700000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
107800000000     C*            2. PULIZIA SCELTA IN SFL
107900930805     C     OPZOK         IFEQ      'S'
108000000000     C                   MOVEL     '0'           *IN41
108100930806     C                   MOVEL     *BLANKS       VSCOPZ
108200930805     C                   ELSE
108300000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
108400000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
108500000000     C                   MOVEL     '1'           *IN41
108600000000     C                   Z-ADD     SFLULT        SFLERR
108700930805     C                   ENDIF
108800930805     C                   ELSE
108900000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
109000000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
109100000000     C                   MOVEL     '1'           *IN41
109200930805     C                   ENDIF
109300000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
109400000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
109500000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
109600930803     C     OPZOK         IFNE      'S'
109700930806     C                   Z-ADD     SFLERR        VCCNBR
109800000000     C                   ELSE
109900930806     C                   Z-ADD     SFLULT        VCCNBR
110000930805     C                   ENDIF
110100000000     C* AGGIORNO RECORD SFL
110200940111     C                   UPDATE    TB50DVS
110300940111     C                   READC     TB50DVS                                49
110400930805     C                   ENDDO
110500000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
110600000000     C                   Z-ADD     WFLULT        SFLULT
110700000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
110800000000     C     SFLVAR        IFEQ      0
110900150821     C*****              MOVEL     '1'           $FINE
111000930805     C                   ENDIF
111100000000     C                   ENDSR
111200940111     C*------------------------------------------------------------------------*
111300930804     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
111400940111     C*------------------------------------------------------------------------*
111500000000     C     CLRSFL        BEGSR
111600000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
111700000000     C* (0=NON FINE FILE  1=FINE FILE)
111800000000     C                   Z-ADD     0             SFLEOF
111900000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
112000000000     C                   Z-ADD     0             SFLULT
112100000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
112200000000     C                   MOVEL     '1'           *IN40
112300940111     C                   WRITE     TB50DVC
112400000000     C                   MOVEL     '0'           *IN40
112500000000     C                   ENDSR
112600940111     C*------------------------------------------------------------------------*
112700000000     C* CARSFL - CARICAMENTO SFL
112800940111     C*------------------------------------------------------------------------*
112900000000     C     CARSFL        BEGSR
113000000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
113100000000     C     SFLULT        IFEQ      0
113200000000     C                   EXSR      SETFIL
113300930805     C                   ENDIF
113400000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
113500000000     C                   Z-ADD     SFLULT        WFLULT
113600000000     C* CICLO:
113700000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
113800000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
113900000000     C                   Z-ADD     1             SFLRIG
114000930805     C     SFLRIG        DOWLE     SFLPAG
114100000000     C     SFLEOF        ANDEQ     0
114200000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
114300930806     C                   MOVEL     *BLANKS       VSCOPZ
114400000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
114500000000     C                   EXSR      IRESFL
114600000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
114700000000     C                   ADD       1             SFLULT
114800000000     C* SCRITTURA RECORD SFL
114900940111     C                   WRITE     TB50DVS
115000000000     C                   ADD       1             SFLRIG
115100000000     C* LETTURA FILE DA CARICARE
115200940112     C                   EXSR      LETFIL
115300930805     C                   ENDDO
115400000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
115500000000     C     WFLULT        IFNE      SFLULT
115600930806     C                   Z-ADD     WFLULT        VCCNBR
115700930806     C                   ADD       1             VCCNBR
115800930805     C                   ENDIF
115900930720     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
116000930805     C     SFLEOF        IFEQ      1
116100930721     C     SFLULT        ANDGT     *ZEROS
116200930721     C                   MOVEL     '1'           $ERR
116300930804     C                   MOVEL     MSG(6)        $MSG
116400930721     C                   EXSR      CARMSG
116500930804     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
116600940111     C     SFLULT        CHAIN     TB50DVS                            99
116700930804     C     *IN99         IFEQ      '0'
116800930804     C                   SETON                                        45
116900940111     C                   UPDATE    TB50DVS
117000930804     C                   SETOFF                                       45
117100930805     C                   ENDIF
117200930805     C                   ENDIF
117300000000     C                   ENDSR
117400940111     C*------------------------------------------------------------------------*
117500930804     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
117600940111     C*------------------------------------------------------------------------*
117700000000     C     OUTSFL        BEGSR
117800930805     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
117900930721     C     MSGULT        IFNE      *ZEROS
118000930721     C                   EXSR      OUTMSG
118100930805     C                   ENDIF
118200000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
118300000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
118400000000     C     SFLEOF        IFEQ      0
118500000000     C                   MOVEL     '0'           *IN42
118600000000     C                   ELSE
118700000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
118800000000     C                   MOVEL     '1'           *IN42
118900930805     C                   ENDIF
119000930720     C* EMISSIONE SUBFILE
119100000000     C     SFLULT        IFLE      0
119200930720     C*  --> CASO DI SFL VUOTO
119300930720     C                   MOVEL     '0'           *IN43
119400940111     C                   WRITE     TB50DVV
119500940111     C                   WRITE     TB50DVC
119600940111     C                   READ      TB50DVC                                99
119700000000     C                   ELSE
119800930720     C*  --> CASO DI SFL CON RECORDS
119900940111     C                   WRITE     TB50DVC
120000940111     C                   READ      TB50DVC                                99
120100930805     C                   ENDIF
120200930720     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
120300930720     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
120400930806     C                   Z-ADD     FRTSFL        VCCNBR
120500000000     C                   ENDSR
120600940111     C*------------------------------------------------------------------------*
120700930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
120800930721     C*          CODA MESSAGGI DEL PGM
120900940111     C*------------------------------------------------------------------------*
121000930721     C     CARMSG        BEGSR
121100930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
121200930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
121300930721     C     MSGULT        IFEQ      0
121400930721     C                   MOVEL     '1'           *IN40
121500940111     C                   WRITE     TB50DVK
121600930721     C                   MOVEL     '0'           *IN40
121700930805     C                   ENDIF
121800930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
121900930721     C                   MOVEL     $MSG          DSMSMS
122000930721     C                   CALL      'XMSG'        XMSGPA
122100930721     C     DSMSER        IFEQ      0
122200930806     C                   MOVEL     DSMSKE        VMCKEY
122300930805     C                   ENDIF
122400930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
122500931015     C                   ADD       1             MSGULT            4 0
122600940111     C                   WRITE     TB50DVM
122700930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
122800930721     C                   MOVEL     *BLANKS       $MSG
122900930721     C                   ENDSR
123000940111     C*------------------------------------------------------------------------*
123100930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
123200940111     C*------------------------------------------------------------------------*
123300930721     C     OUTMSG        BEGSR
123400930721     C* VISUALIZZAZIONE MESSAGGIO
123500930721     C                   MOVEL     '1'           *IN44
123600940111     C                   WRITE     TB50DVK
123700930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
123800930721     C                   Z-ADD     *ZEROS        MSGULT
123900930721     C                   ENDSR
124000940111     C*------------------------------------------------------------------------*
124100930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
124200930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
124300930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
124400930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
124500930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
124600940111     C*------------------------------------------------------------------------*
124700930803     C     CARFUN        BEGSR
124800930803     C                   Z-ADD     *ZEROS        FUNULT            5 0
124900931015     C     1             DO        31            I                 5 0
125000930803     C                   MOVEL     FUN(I)        FUNRIG
125100930805     C                   MOVEL     FUNFUN        FFU(I)
125200930805     C                   MOVEL     FUNDES        FDE(I)
125300020923     C     DUTLPO        IFEQ      'S'
125400930923     C     $TGES         IFEQ      'G'
125500930805     C                   MOVEL     FUNASG        FAT(I)
125600930803     C                   ELSE
125700930803     C                   MOVEL     FUNASI        FAT(I)
125800930803     C                   ENDIF
125900930803     C                   ELSE
126000930923     C     $TGES         IFEQ      'G'
126100930803     C                   MOVEL     FUNAFG        FAT(I)
126200930803     C                   ELSE
126300930803     C                   MOVEL     FUNAFI        FAT(I)
126400930803     C                   ENDIF
126500930803     C                   ENDIF
126600930806     C     FAT(I)        IFEQ      'E'
126700930806     C                   MOVEL     USER          WEDP              3
126800930806     C     WEDP          IFEQ      'EDP'
126900930806     C                   MOVEL     'S'           FAT(I)
127000930806     C                   ELSE
127100930806     C                   MOVEL     'N'           FAT(I)
127200930806     C                   ENDIF
127300930806     C                   ENDIF
127400930803     C                   ENDDO
127500930803     C                   ENDSR
127600940111     C*------------------------------------------------------------------------*
127700930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
127800930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
127900930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
128000930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
128100940111     C*------------------------------------------------------------------------*
128200930803     C     OUTFUN        BEGSR
128300930805     C     1             DO        8             I
128400930805     C                   MOVEL     *BLANKS       FUV(I)
128500930803     C                   ENDDO
128600930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
128700930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0
128800930803     C     1             DO        31            I
128900930803     C     FAT(I)        IFEQ      'S'
129000930803     C     FDE(I)        ANDNE     *BLANKS
129100930803     C                   ADD       1             FUNNAT
129200930803     C                   ENDIF
129300930803     C                   ENDDO
129400930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
129500930803     C     FUNNAT        IFGT      8
129600930803     C                   MOVEL     'S'           FAT(24)
129700930803     C                   ENDIF
129800930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
129900930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
130000930803     C                   Z-ADD     8             FUNVIS            5 0
130100930803     C     FAT(23)       IFEQ      'S'
130200930803     C                   SUB       1             FUNVIS
130300930803     C                   ENDIF
130400930803     C     FAT(24)       IFEQ      'S'
130500930803     C                   SUB       1             FUNVIS
130600930803     C                   ENDIF
130700930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
130800931015     C                   Z-ADD     *ZEROS        W                 5 0
130900930803     C                   ADD       1             FUNULT
131000930803     C     FUNULT        IFGT      31
131100930803     C                   Z-ADD     1             FUNULT
131200930805     C                   ENDIF
131300930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
131400930803     C     FUNULT        DO        31            I
131500930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
131600930803     C     W             IFLT      FUNVIS
131700930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
131800930803     C     FAT(I)        IFEQ      'S'
131900930803     C     FDE(I)        ANDNE     *BLANKS
132000930805     C     I             IFEQ      23
132100930805     C     I             OREQ      24
132200930803     C                   ELSE
132300930803     C                   ADD       1             W
132400930803     C                   MOVEL     FDE(I)        FUV(W)
132500930803     C                   ENDIF
132600930803     C                   ENDIF
132700930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
132800930803     C                   Z-ADD     I             FUNULT
132900930803     C                   ENDIF
133000930803     C                   ENDDO
133100930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
133200930806     C                   MOVEL     FUV(1)        VFD001
133300930806     C                   MOVEL     FUV(2)        VFD002
133400930806     C                   MOVEL     FUV(3)        VFD003
133500930806     C                   MOVEL     FUV(4)        VFD004
133600930806     C                   MOVEL     FUV(5)        VFD005
133700930806     C                   MOVEL     FUV(6)        VFD006
133800930803     C     FAT(23)       IFEQ      'N'
133900930803     C     FAT(24)       ANDEQ     'N'
134000930806     C                   MOVEL     FUV(7)        VFD007
134100930806     C                   MOVEL     FUV(8)        VFD008
134200930803     C                   ELSE
134300930803     C     FAT(23)       IFEQ      'S'
134400930803     C     FAT(24)       ANDEQ     'S'
134500930806     C                   MOVEL     FDE(23)       VFD007
134600930806     C                   MOVEL     FDE(24)       VFD008
134700930803     C                   ELSE
134800930806     C                   MOVEL     FUV(7)        VFD007
134900930803     C     FAT(23)       IFEQ      'S'
135000930806     C                   MOVEL     FDE(23)       VFD008
135100930803     C                   ELSE
135200930806     C                   MOVEL     FDE(24)       VFD008
135300930805     C                   ENDIF
135400930805     C                   ENDIF
135500930805     C                   ENDIF
135600930803     C                   MOVEL     '1'           $TESVD
135700930803     C                   ENDSR
135800940111     C*------------------------------------------------------------------------*
135900930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
136000930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
136100930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
136200930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
136300940111     C*------------------------------------------------------------------------*
136400930803     C     CONFUN        BEGSR
136500930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
136600930803     C     1             DO        31            I
136700930803     C     KEY           IFEQ      KEYS(I)
136800930805     C* CONTROLLO SE IL TASTO E' ATTIVO
136900930803     C     FAT(I)        IFNE      'S'
137000930804     C                   MOVEL     MSG(1)        $MSG
137100930803     C                   EXSR      CARMSG
137200930803     C                   MOVEL     '1'           $ERR
137300930803     C                   ENDIF
137400930803     C                   ENDIF
137500930803     C                   ENDDO
137600930803     C                   ENDSR
137700940111     C*------------------------------------------------------------------------*
137800930802     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
137900930802     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
138000930803     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
138100930803     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
138200930803     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
138300930803     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
138400940111     C*------------------------------------------------------------------------*
138500930802     C     CAROPZ        BEGSR
138600930803     C                   Z-ADD     *ZEROS        OPZULT            5 0
138700930802     C     1             DO        24            I
138800930803     C                   MOVEL     OPZ(I)        OPZRIG
138900930805     C                   MOVEL     OPZOPD        OPD(I)
139000930805     C                   MOVEL     OPZOPS        OPS(I)
139100930805     C                   MOVEL     OPZDES        ODE(I)
139200020923     C     DUTLPO        IFEQ      'S'
139300930923     C     $TGES         IFEQ      'G'
139400930805     C                   MOVEL     OPZASG        OAT(I)
139500930802     C                   ELSE
139600930805     C                   MOVEL     OPZASI        OAT(I)
139700930802     C                   ENDIF
139800930802     C                   ELSE
139900930923     C     $TGES         IFEQ      'G'
140000930805     C                   MOVEL     OPZAFG        OAT(I)
140100930802     C                   ELSE
140200930805     C                   MOVEL     OPZAFI        OAT(I)
140300930802     C                   ENDIF
140400930802     C                   ENDIF
140500930806     C     OAT(I)        IFEQ      'E'
140600930806     C                   MOVEL     USER          WEDP              3
140700930806     C     WEDP          IFEQ      'EDP'
140800930806     C                   MOVEL     'S'           OAT(I)
140900930806     C                   ELSE
141000930806     C                   MOVEL     'N'           OAT(I)
141100930806     C                   ENDIF
141200930806     C                   ENDIF
141300930802     C                   ENDDO
141400930802     C                   ENDSR
141500940111     C*------------------------------------------------------------------------*
141600930802     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
141700930802     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
141800930802     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
141900930806     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
142000930802     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
142100940111     C*------------------------------------------------------------------------*
142200930802     C     OUTOPZ        BEGSR
142300930805     C     1             DO        8             I
142400930805     C                   MOVEL     *BLANKS       OPV(I)
142500930802     C                   ENDDO
142600930802     C                   Z-ADD     *ZEROS        OPZNAT            2 0
142700930805     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
142800930803     C                   ADD       1             OPZULT
142900930803     C     OPZULT        IFGT      24
143000930803     C                   Z-ADD     1             OPZULT
143100930803     C                   END
143200930805     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
143300930802     C     1             DO        24            I
143400930805     C     OAT(I)        IFEQ      'S'
143500930805     C     ODE(I)        ANDNE     *BLANKS
143600930802     C                   ADD       1             OPZNAT
143700930802     C                   ENDIF
143800930802     C                   ENDDO
143900930802     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
144000931015     C                   Z-ADD     0             W
144100930802     C     OPZULT        DO        24            I
144200930802     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
144300930802     C     W             IFLT      8
144400930802     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
144500930805     C     OAT(I)        IFEQ      'S'
144600930803     C     ODE(I)        ANDNE     *BLANKS
144700930802     C                   ADD       1             W
144800930803     C                   MOVEL     ODE(I)        OPV(W)
144900930805     C                   ENDIF
145000930805     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
145100930802     C                   Z-ADD     I             OPZULT
145200930802     C                   ENDIF
145300930802     C                   ENDDO
145400930802     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
145500930806     C                   MOVEL     OPV(1)        VOD001
145600930806     C                   MOVEL     OPV(2)        VOD002
145700930806     C                   MOVEL     OPV(3)        VOD003
145800930806     C                   MOVEL     OPV(4)        VOD004
145900930806     C                   MOVEL     OPV(5)        VOD005
146000930806     C                   MOVEL     OPV(6)        VOD006
146100930806     C                   MOVEL     OPV(7)        VOD007
146200930806     C                   MOVEL     OPV(8)        VOD008
146300930802     C                   MOVEL     '1'           $TESVD
146400930923     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
146500930923     C     OPZNAT        IFGT      8
146600930923     C                   MOVEL     'S'           FAT(23)
146700930923     C                   ENDIF
146800930802     C                   ENDSR
146900940111     C*------------------------------------------------------------------------*
147000930802     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
147100930806     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
147200930802     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
147300930802     C*                   - ERRORE SE OPZIONE NON ATTIVA
147400940111     C*------------------------------------------------------------------------*
147500930802     C     CONOPZ        BEGSR
147600000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
147700930803     C                   MOVEL     *BLANKS       OPZOK             1
147800930805     C     1             DO        24            I                 5 0
147900930806     C     VSCOPZ        IFEQ      OPD(I)
148000930806     C     VSCOPZ        OREQ      OPS(I)
148100930805     C     OAT(I)        IFEQ      'S'
148200930806     C                   MOVEL     OPD(I)        VSCOPZ
148300940105     C                   MOVEL     'S'           OPZOK
148400930805     C                   ELSE
148500930802     C                   MOVEL     'N'           OPZOK             1
148600930805     C                   ENDIF
148700930802     C                   ENDIF
148800930802     C                   ENDDO
148900930805     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
149000930803     C     OPZOK         IFNE      'S'
149100930804     C                   MOVEL     MSG(3)        $MSG
149200930803     C                   EXSR      CARMSG
149300930803     C                   MOVEL     '1'           $ERR
149400930803     C                   MOVEL     'N'           OPZOK             1
149500930803     C                   ENDIF
149600000000     C                   ENDSR
149700930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
149800940111TRTB50R- Tasto funzionale non valido.                                          1
149900940111TRTB50R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
150000940111TRTB50R- Opzione non valida.                                                   3
150100940111TRTB50R- Opzione abilitata ma non gestita. Avvertire CED.                      4
150200940111TRTB50R- Inizio elenco.                                                        5
150300940111TRTB50R- Fine elenco.                                                          6
150400940112TRTB50R- 7                                                                     7
150500940112TRTB50R- 8                                                                     8
150600940112TRTB50R- Codice filiale errata                                                 9
150700940112TRTB50R- 10                                                                   10
150800940117TRTB50R- Impossibile manutenzionare questo tempo se non si ha il profilo EDP  11
150900940112TRTB50R- Tempo annullato, consentito solo il ripristino                       12
151000940112TRTB50R- Tempo non annullato, impossibile ripristinare                        13
151100940112TRTB50R- 14                                                                   14
151200940112TRTB50R- 15                                                                   15
151300940112TRTB50R- 16                                                                   16
151400940111TRTB50R- 17                                                                   17
151500940111TRTB50R- 18                                                                   18
151600940111TRTB50R- 19                                                                   19
151700940111TRTB50R- 20                                                                   20
151800930729** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
151900930803- 1-1 -                   -    NN     NN
152000940112- 2-2 -2=Modifica         -    SS     SS
152100930803- 3-3 -                   -    NN     NN
152200940112- 4-4 -4=Cancellazione    -    SS     SS
152300940121- 5-5 -5=Visualizzazione  -    SS     SS
152400930803- 6-6 -                   -    NN     NN
152500940112- 7-7 -7=Ripristino       -    SS     SS
152600940104- 8-8 -                   -    NN     NN
152700940111- 9-9 -                   -    NN     NN
152800930806-10-  -                   -    NN     NN
152900930806-11-  -                   -    NN     NN
153000930806-12-  -                   -    NN     NN
153100930803-13-  -                   -    NN     NN
153200930803-14-  -                   -    NN     NN
153300930803-15-  -                   -    NN     NN
153400930803-16-  -                   -    NN     NN
153500930803-17-  -                   -    NN     NN
153600930803-18-  -                   -    NN     NN
153700930803-19-  -                   -    NN     NN
153800930803-20-  -                   -    NN     NN
153900930803-21-  -                   -    NN     NN
154000930803-22-  -                   -    NN     NN
154100930803-23-  -                   -    NN     NN
154200930803-24-  -                   -    NN     NN
154300930730** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
154400930723F1    -                   -    NN     NN
154500930723F2    -                   -    NN     NN
154600930802F3    -F3=Fine            -    SS     SS
154700930723F4    -                   -    NN     NN
154800940112F5    -F5=Rivisualizzaz.  -    SS     SS
154900940112F6    -F6=Inserimento     -    SS     SS
155000930723F7    -                   -    NN     NN
155100930723F8    -                   -    NN     NN
155200930723F9    -                   -    NN     NN
155300930723F10   -                   -    NN     NN
155400930723F11   -                   -    NN     NN
155500930802F12   -F12=Ritorno        -    SS     SS
155600930806F13   -                   -    NN     NN
155700930806F14   -                   -    NN     NN
155800930723F15   -                   -    NN     NN
155900930723F16   -                   -    NN     NN
156000930802F17   -F17=Inizio dati    -    SS     SS
156100930802F18   -F18=Fine dati      -    SS     SS
156200930723F19   -                   -    NN     NN
156300930723F20   -                   -    NN     NN
156400940111F21   -                   -    NN     NN
156500940111F22   -                   -    NN     NN
156600930803F23   -F23=Altre opzioni  -    NN     NN
156700930803F24   -F24=Altri tasti    -    NN     NN
156800930802ENTER -                   -    SS     SS
156900930802ROLLUP-                   -    SS     SS
157000930802ROLLDN-                   -    SS     SS
157100930802HELP  -                   -    NN     NN
157200930802PRINT -                   -    NN     NN
157300930802BSPACE-                   -    NN     NN
157400930802CLEAR -                   -    NN     NN
