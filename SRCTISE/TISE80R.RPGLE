000100141127     H*
000200141127     H*
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *
000500940307      *  21           GENERICO OPERAZIONI I/O
000600940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000700940224      *  30           SFLDSP
000800940224      * N31           SFLCLR
000900940224      *  31           SFLDSPCTL
001000940224      *  32           SFLNXTCHG
001100940224      *  33           SFLEND
001200940224      *  39           OF PRTF
001300940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001400940608      *  Specificare l'uso dei singoli indicatori
001500940224      *  50 <---> 98  ERRORI SU VIDEO
001600940608      *  Specificare l'uso dei singoli indicatori
001700940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001800940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
001900940224      *  99           INDIC. GENERALE DI ERRORE
002000940128     F*----------------------------------------------------*
002100050511     FTISE800F  IF   E           K DISK
002200050509     FTISE80D   CF   E             WORKSTN
002300940607     F                                     SFILE(S1:S1NRR)
002400940201     F                                     INFDS(DSFMT)
002500940128     D*----------------------------------------------------*
002600940211     D* Passaggio Parametri
002700940211     D KPJBA         E DS
002800030113      *-------------
002900940325     D* Parametri in ricezione
003000030113     D  TABDS          DS
003100030729     D  XTAOPZ                 1      1
003200030729     D  XTARET                 2      2
003300030729     D  XTAOPR                 3      3
003400030729     D  XTAERR                 4      4
003500050509     D  XTACOD                 5     11  0
003600940211     D*-------------
003700940211     D DSFMT           DS           512
003800940506     D  $TASTO               369    369
003900940211     D  NRG                  370    370
004000940211     D  NCL                  371    371
004100940211     D  SFLNRR               378    379B 0
004200030728      *
004300030728     Ddataiso          s               d   datfmt(*iso)
004400030728     Ddataeur          s               d   datfmt(*eur)
004500940207     D*-------------
004600940207     D* Nome PGM a video
004700940207     D                 DS
004800940207     D  PROGR                  1     10
004900940207     D  ASTER1                 1      1    INZ('*')
005000940207     D  SIGLA                  2      9
005100940207     D  ASTER2                10     10    INZ('*')
005200940127     D*-------------
005300940127     D* Reperimento nome PGM
005400940127     D STATUS         SDS           333
005500940127     D  DSPGM            *PROC
005600030113     D*-------------
005700030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005800030113$003 D S1NRR           S                   Like(C1rcd)
005900030113$003 D WSfl            S                   Like(C1nrr)
006000030113$003 D Wmax            S                   Like(C1rcd)
006100030113$003 D Wpag            S                   Like(C1rcd)
006200030113$003 D Winzs1          S                   Like($inzs1)
006300940207     D*-------------
006400940211     D* COSTANTI
006500940211     D*-------------
006600030113     D SFLPAG          C                   CONST(11)
006700940314     D* dimensione della schiera $MS1
006800940506     D*
006900940506     D* Tasti di funzione
007000940506     D F01             C                   CONST(X'31')
007100940506     D F02             C                   CONST(X'32')
007200940506     D F03             C                   CONST(X'33')
007300940506     D F04             C                   CONST(X'34')
007400940506     D F05             C                   CONST(X'35')
007500940506     D F06             C                   CONST(X'36')
007600940506     D F07             C                   CONST(X'37')
007700940506     D F08             C                   CONST(X'38')
007800940506     D F09             C                   CONST(X'39')
007900940506     D F10             C                   CONST(X'3A')
008000940506     D F11             C                   CONST(X'3B')
008100940506     D F12             C                   CONST(X'3C')
008200940506     D F13             C                   CONST(X'B1')
008300940506     D F14             C                   CONST(X'B2')
008400940506     D F15             C                   CONST(X'B3')
008500940506     D F16             C                   CONST(X'B4')
008600940506     D F17             C                   CONST(X'B5')
008700940506     D F18             C                   CONST(X'B6')
008800940506     D F19             C                   CONST(X'B7')
008900940506     D F20             C                   CONST(X'B8')
009000940506     D F21             C                   CONST(X'B9')
009100940506     D F22             C                   CONST(X'BA')
009200940506     D F23             C                   CONST(X'BB')
009300940506     D F24             C                   CONST(X'BC')
009400940506     D ENTER           C                   CONST(X'F1')
009500940506     D ROLDWN          C                   CONST(X'F4')
009600940506     D ROLLUP          C                   CONST(X'F5')
009700940127     C*----------------------------------------------------*
009800940127     C*                MAIN LINE PROGRAM
009900940127     C*----------------------------------------------------*
010000940223     C* inizializzazione variabili
010100940223     C                   EXSR      INZVAR
010200940223     C*
010300940223     C     $FINE         DOWEQ     *OFF
010400940131     C     $GEST         CASEQ     'S1'          GESS1
010500940117     C                   END
010600940117     C                   END
010700940325     C* fine programma
010800940325     C                   SETON                                            LR
010900030113     C************************************************************
011000030113     C* INIZIALIZZAZIONE VARIABILI
011100030113     C************************************************************
011200030113     C     INZVAR        BEGSR
011300030113     C*
011400030113     C* Pulizia campi e indicatori
011500030113     C                   MOVE      *ALL'0'       IN4049           10
011600030113     C                   MOVEA     IN4049        *IN(40)
011700030113     C                   CLEAR                   S1OPZ
011800030113     C* Variabili per gestione videate
011900030113     C*
012000030113     C                   MOVE      *OFF          $FINE
012100030113     C                   MOVE      *OFF          $INZS1
012200030113     C                   MOVE      *OFF          $EFILE
012300030113     C                   MOVE      *OFF          $ESCI
012400030113     C                   MOVE      *OFF          $RCDOK
012500030113     C                   Z-ADD     0             $ULKS1            3 0
012600030113     C*
012700030113     C                   MOVE      *ON           $INZS1
012800030113     C                   MOVE      'S1'          $GEST
012900030113     C*
013000030113     C* Variabili appoggio
013100030114     C                   Z-ADD     1             WPAG
013200030113     c*
013300030113     C                   ENDSR
013400940127     C************************************************************
013500940131     C* GESTIONE LISTA
013600940127     C************************************************************
013700940127     C     GESS1         BEGSR
013800030113     C*
013900940223     C* inizializzazione videata
014000940223     C     $INZS1        IFEQ      *ON
014100940127     C                   EXSR      INZS1
014200940223     C                   MOVE      *OFF          $INZS1
014300940127     C                   ENDIF
014400030113     C*
014500030113     C* emissione piede videata
014600030113     C                   WRITE     Z1
014700030113     C* Non ci sono records
014800940223     C     WMAX          IFEQ      0
014900940607     C                   WRITE     D1
015000030114     C                   Else
015100030114     C     Wsfl          IFgt      0
015200050511     C     Wsfl          andle     Wmax
015300030114     C                   Z-ADD     wsfl          C1RCD
015400030114     C                   Else
015500050517     C     Wsfl          ifgt      Wmax
015600050517     C                   Z-ADD     wmax          C1RCD
015700050517     c                   else
015800030114     C     Wpag          IFgt      0
015900040115     C     pagine        andgt     0
016000030114     C                   Z-ADD     wpag          C1RCD
016100050511     C                   Else
016200050511     C                   Z-ADD     1             C1RCD
016300030114     C                   EndIF
016400050517     C                   EndIF
016500030114     C                   EndIF
016600030114     C                   ENDIF
016700940127     C*
016800030113     C*              *------------------*
016900940607     C                   EXFMT     C1
017000030113     C*              *------------------*
017100030113     C*
017200940204     C     C1NRR         IFNE      0
017300940204     C                   Z-ADD     C1NRR         WSFL
017400940204     C                   ENDIF
017500940127     C                   Z-ADD     SFLNRR        C1RCD
017600030113     C* Selezioni
0177009401271    C                   SELECT
017800940127     C* F3=Fine
017900940506     C     $TASTO        WHENEQ    F03
018000940309     C                   EXSR      F03S1
018100940131     C* F10=Immissione
018200940506     C     $TASTO        WHENEQ    F10
018300940309     C                   EXSR      F10S1
0184009401271O   C                   OTHER
018500940127     C* CONTROLLO DATI
018600940131     C                   EXSR      CTRC1
018700940201     C     *IN99         IFEQ      *OFF
018800940131     C                   EXSR      CTRS1
018900940131     C                   END
0190009401271-   C                   ENDSL
019100940127     C*
019200940127     C                   ENDSR
019300940224     C/EJECT
019400940127     C************************************************************
019500940131     C* INIZIALIZZAZIONE LISTA
019600940127     C************************************************************
019700940127     C     INZS1         BEGSR
019800940302     C* pulizia SFL
019900940128     C                   SETOFF                                         3031
020000940607     C                   WRITE     C1
020100940128     C                   SETON                                          31
020200940128     C*
020300030113     C* CARICAMENTO SFL totale
020400940201     C                   Z-ADD     0             S1NRR
020500030113     C                   Z-ADD     1             C1RCD
020600940128     C                   Z-ADD     0             WMAX
020700940224     C*
020800940224     C* Posizionamento su file pilota
020900050511     c     *loval        setll     Tise800F
021000940608    >C                   EXSR      REDANA
021100030113     C* Carico il SFL
021200940127     C                   EXSR      ROLS1
021300030113     C*
021400030114     c                   if        xtaopr <> '1'
021500030114     C                   Z-ADD     1             WPAG
021600030114     c                   end
021700940127     C*
021800940127     C                   ENDSR
021900940127     C************************************************************
022000940131     C* CARICAMENTO PAGINA LISTA
022100940127     C************************************************************
022200940127     C     ROLS1         BEGSR
022300940127     C*
022400940128     C                   SETOFF                                       32
022500940223     C                   Z-ADD     0             Y
022600940127     C                   Z-ADD     WMAX          S1NRR
022700940127     C*
022800940127     C* Leggo dal file anagrafico per caricare la lista
0229009401311    C     $EFILE        DOWEQ     *OFF
023000940127     C*
023100030113     c                   clear                   s1opz
023200050509     c                   movel     se8COD        s1cod
023300050509     c                   movel     se8UNI        s1uni
023400050509     c                   movel     se8DES        s1DES
023500050511     c                   movel     se8CLV        s1CLV
023600940127     C*
023700940127     C                   ADD       1             S1NRR
023800940127     C                   ADD       1             Y
023900940127     C*
024000940607     C                   WRITE     S1
024100940131     C*
024200940316    >C                   EXSR      REDANA
024300940128     C*
0244009401271-   C                   ENDDO
024500940127     C*
024600940223     C                   Z-ADD     S1NRR         WMAX                 30
024700940127     C*
024800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
024900940127     C*
025000030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
025100940127     C                   MVR                     RESTO             3 0
025200030114     C     PAGINE        MULT      SFLPAG        C1RCD
0253000301141    C     RESTO         IFGT      0
025400030114     C                   ADD       1             C1RCD
0255000301141E   C                   ELSE
025600030114     C                   SUB       SFLPAG        C1RCD
025700030114     C                   ADD       1             C1RCD
0258000301141-   C                   ENDIF
025900940128     C*
026000940127     C                   ENDSR
026100940128     C************************************************************
026200940131     C* LETTURA RCD ARCHIVIO PILOTA
026300940128     C************************************************************
026400940607     C     REDANA        BEGSR
026500940128     C*
026600940131     C                   MOVEL     *OFF          $EFILE
026700940131     C                   MOVEL     *OFF          $RCDOK
026800940131     C*
0269009401311    C     $EFILE        DOUEQ     *ON
027000940131     C     $RCDOK        OREQ      *ON
027100030728     C*
027200050511     c                   Read      Tise800F
027300050511     c                   if        %eof(Tise800F)
027400030113     C                   MOVEL     *ON           $EFILE
027500030113     C                   MOVE      $EFILE        *IN33
027600030113     c                   else
027700030113     C                   MOVE      *ON           $RCDOK
027800030113     c                   end
027900940131     C*
0280009401311-   C                   ENDDO
028100940131     C*
028200940131     C                   ENDSR
028300940224     C************************************************************
028400940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
028500940224     C************************************************************
028600940224     C     CLCPAG        BEGSR
028700940224     C* Input :
028800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
028900940224     C*          cursore
029000940224     C* - SFLPAG = numero rcd per pagina sfl
029100940224     C* Output :
029200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
029300940224     C*
029400940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
029500940224     C                   MVR                     RESTO             3 0
029600940224     C     RESTO         IFGT      0
029700940224     C                   ADD       1             PAGINE
029800940224     C                   ENDIF
029900940224     C     PAGINE        MULT      SFLPAG        WPAG
030000940224     C*
030100940224     C                   ENDSR
030200940309     C************************************************************
030300940309     C* GESTIONE F03 VIDEO S1
030400940309     C************************************************************
030500940309     C     F03S1         BEGSR
030600940309     C*
030700940309     C                   MOVE      *ON           $FINE
030800940325     C* fine programma
030900940309     C                   ENDSR
031000940309     C/EJECT
031100940309     C************************************************************
031200940309     C* GESTIONE F10 VIDEO S1
031300940314     c* AGGIUNTA RECORD
031400940309     C************************************************************
031500940309     C     F10S1         BEGSR
031600940309     C*
031700030113     C                   RESET                   tabds
031800030729     C                   MOVEL     '1'           xtaopz
031900030113     C                   MOVE      *ZERO         xtaret
032000030113     C                   MOVE      *ZERO         xtaopr
032100050509     C                   MOVEl     *Zero         xtacod
032200030113     C                   MOVE      *BLANKS       KPJBU
032300030113     C                   MOVEL     tabds         KPJBU
032400050509$004 C                   CALL      'TISE80R1'
032500030113     C                   PARM                    KPJBA
032600030114     C                   MOVEL     KPJBU         tabds
032700030113      *
032800940309     C* ritorno da PGM gestione
032900940309     C                   EXSR      GESRET
033000940309     C     WINZS1        IFEQ      *ON
033100940309     C                   MOVE      *ON           $INZS1
033200940309     C* se esistevano già righe sul sfl
033300940309     C* calcolo pagina a cui deve posizionarsi
033400940309     C     WSFL          IFGT      0
033500940309     C                   EXSR      CLCPAG
033600940309     C* altrimenti carico solo la 1a pagina
033700940309     C                   ELSE
033800940309     C                   Z-ADD     SFLPAG        WPAG
033900940309     C                   ENDIF
034000940309     C                   ENDIF
034100940309     C*
034200940309     C                   ENDSR
034300940128     C************************************************************
034400940131     C* CONTROLLO TESTATA LISTA
034500940128     C************************************************************
034600940131     C     CTRC1         BEGSR
034700940128     C*
034800940201     C                   MOVE      *OFF          *IN99
034900940131     C*
035000940202     C                   ENDSR
035100940131     C************************************************************
035200940131     C* CONTROLLO OPZIONI LISTA
035300940131     C************************************************************
035400940131     C     CTRS1         BEGSR
035500940131     C*
035600940202     C                   MOVEL     *OFF          $ESCI
035700940201     C                   SETOFF                                       99
035800940131     C                   Z-ADD     0             S1OPZ
035900940131     C*
036000940127     C* Leggo il sfl solo se ci sono rcd
0361009401311    C     WMAX          IFGT      0
036200940607     C                   READC     S1                                     21
036300940127     C*
036400940131     C* esce se fine sfl o errore che richiede l'uscita
0365009401312    C     *IN21         DOWEQ     *OFF
036600940131     C     $ESCI         ANDEQ     *OFF
036700940201     C                   Z-ADD     S1NRR         C1RCD
036800940131     C* ctrl su riga
036900940131     C                   EXSR      RECS1
037000940131     C* gestione opzioni
0371009401313    C     S1OPZ         IFNE      0
037200940201     C     *IN99         ANDEQ     *OFF
037300940131     C                   EXSR      OPZS1
0374009401273-   C                   ENDIF
037500940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0376009402013    C     *IN99         IFEQ      *ON
037700940201     C     $ESCI         OREQ      *ON
037800940131     C                   MOVE      *ON           *IN32
037900940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038000940204     C* la ripristinerò a conclusione del ciclo di READC
038100940223     C                   MOVE      *OFF          $INZS1
0382009402233-   C                   ENDIF
038300940223     C*
038400940131     C                   Z-ADD     0             S1OPZ
038500940223     C*
038600940607     C                   UPDATE    S1
038700940223     C*
038800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0389009401313    C     $ESCI         IFEQ      *OFF
039000940607     C                   READC     S1                                     21
039100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0392009402014    C     *IN21         IFEQ      *ON
039300940201     C                   MOVE      WINZS1        $INZS1
039400940204     C* calcolo pagina a cui deve posizionarsi
039500940224     C                   EXSR      CLCPAG
0396009402014-   C                   ENDIF
0397009402013-   C                   ENDIF
039800940131     C*
0399009401272-   C                   ENDDO
040000940127     C*
0401009401311-   C                   ENDIF
040200940131     C*
040300940127     C                   ENDSR
040400940127     C/EJECT
040500940127     C************************************************************
040600940131     C* CONTROLLO CAMPI I/O RIGA LISTA
040700940127     C************************************************************
040800940131     C     RECS1         BEGSR
040900940131     C*
041000940201     C* reset indicatori DSPATR
041100940201     C                   MOVE      *ALL'0'       IN4049           10
041200940201     C                   MOVEA     IN4049        *IN(40)
041300940201     C*
041400940131     C                   ENDSR
041500940131     C************************************************************
041600940131     C* GESTIONE OPZIONI LISTA
041700940131     C************************************************************
041800940131     C     OPZS1         BEGSR
041900940201     C*
042000030113     C                   RESET                   tabds
042100030113     C                   MOVEL     S1OPZ         xtaopz
042200030113     C                   MOVE      *ZERO         xtaret
042300030113     C                   MOVE      *ZERO         xtaopr
042400030729     C                   MOVEl     s1cod         xtacod
042500940715     C                   MOVE      *BLANKS       KPJBU
042600030113     C                   MOVEL     tabds         KPJBU
042700050509$004 C                   CALL      'TISE80R1'
042800940607     C                   PARM                    KPJBA
042900030114     C                   MOVEL     KPJBU         tabds
043000940201     C*
043100940223     C* ritorno da PGM gestione
043200940223     C                   EXSR      GESRET
043300940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0434009402252    C     *IN99         IFEQ      *ON
043500940223     C                   SETON                                        40
0436009402252-   C                   ENDIF
043700940225     C*
043800940131     C                   ENDSR
043900940223     C/EJECT
044000940223     C************************************************************
044100940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
044200940223     C************************************************************
044300940223     C     GESRET        BEGSR
044400940223     C*
044500940223     C* modo di ritorno
044600940223     C*
0447009402231    C                   SELECT
044800940314    >C* << questi modi di utilizzo dei valori di ritorno dal
044900940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
045000940314    >C*    proposte, normalmente sempre valide, ma modificabili
045100940314    >C*    per situazioni particolari >>
045200940223     C* 1 = F3
045300030113    >C     xtaret        WHENEQ    '1'
045400940224     C                   MOVE      *ON           $ESCI
045500940223     C                   MOVE      *ON           $FINE
045600940223     C* 2 = F12
045700030113    >C     xtaret        WHENEQ    '2'
045800940223     C                   MOVE      *ON           $ESCI
045900940223     C*
0460009402231-   C                   ENDSL
046100940223     C*
046200940223     C* operazione eseguite dal pgm chiamato
046300940223     C*
0464009402231    C                   SELECT
046500940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
046600030113    >C     xtaopr        WHENEQ    '1'
046700940223     C                   MOVE      *ON           WINZS1
046800940223     C*
0469009402231-   C                   ENDSL
047000940223     C*
047100940223     C* funzione non eseguibile per errore :
047200940223     C*
0473009402231    C                   SELECT
047400940223     C* 1 = funzione richiamata chiusa in errore
047500940316    >C*  eventualmente gestire altri codici di errore
047600030113    >C     xtaerr        WHENEQ    '1'
047700940223     C                   MOVE      *ON           $ESCI
047800940223     C                   SETON                                        5299
047900940223     C*
0480009402231-   C                   ENDSL
048100940223     C*
048200940223     C                   ENDSR
048300940223     C/EJECT
048400940131     C************************************************************
048500940131     C* OPERAZIONI INIZIALI
048600940131     C************************************************************
048700940131     C     *INZSR        BEGSR
048800030113     C*
048900030113     C* Reperimento parametri
049000030113     C     *ENTRY        PLIST
049100030113     C                   PARM                    KPJBA
049200030113     C*
049300030113     C* Variabili per gestione videate
049400030113     C                   MOVE      *BLANK        $GEST             2
049500030113     C                   MOVE      *BLANK        $FINE             1
049600030113     C                   MOVE      *BLANK        $INZS1            1
049700030113     C                   MOVE      *BLANK        $EFILE            1
049800030113     C                   MOVE      *BLANK        $ESCI             1
049900030113     C                   MOVE      *BLANK        $RCDOK            1
050000030113     C* Indici
050100030113     C                   Z-ADD     0             X                 3 0
050200030113     C                   Z-ADD     0             Y                 3 0
050300940127     C*
050400940117     C                   ENDSR
050500030113     C************************************************************
