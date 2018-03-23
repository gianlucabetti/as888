000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
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
005600940607     D* descrizione tabella
005700030113$003 D EXTDS         E DS                  EXTNAME(DS8A)
005800030113     D*-------------
005900030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006000030113$003 D S1NRR           S                   Like(C1rcd)
006100030113$003 D WSfl            S                   Like(C1nrr)
006200030113$003 D Wmax            S                   Like(C1rcd)
006300030113$003 D Wpag            S                   Like(C1rcd)
006400030113$003 D Winzs1          S                   Like($inzs1)
006500940207     D*-------------
006600940211     D* COSTANTI
006700940211     D*-------------
006800030113     D SFLPAG          C                   CONST(11)
006900940314     D* dimensione della schiera $MS1
007000940506     D*
007100940506     D* Tasti di funzione
007200940506     D F01             C                   CONST(X'31')
007300940506     D F02             C                   CONST(X'32')
007400940506     D F03             C                   CONST(X'33')
007500940506     D F04             C                   CONST(X'34')
007600940506     D F05             C                   CONST(X'35')
007700940506     D F06             C                   CONST(X'36')
007800940506     D F07             C                   CONST(X'37')
007900940506     D F08             C                   CONST(X'38')
008000940506     D F09             C                   CONST(X'39')
008100940506     D F10             C                   CONST(X'3A')
008200940506     D F11             C                   CONST(X'3B')
008300940506     D F12             C                   CONST(X'3C')
008400940506     D F13             C                   CONST(X'B1')
008500940506     D F14             C                   CONST(X'B2')
008600940506     D F15             C                   CONST(X'B3')
008700940506     D F16             C                   CONST(X'B4')
008800940506     D F17             C                   CONST(X'B5')
008900940506     D F18             C                   CONST(X'B6')
009000940506     D F19             C                   CONST(X'B7')
009100940506     D F20             C                   CONST(X'B8')
009200940506     D F21             C                   CONST(X'B9')
009300940506     D F22             C                   CONST(X'BA')
009400940506     D F23             C                   CONST(X'BB')
009500940506     D F24             C                   CONST(X'BC')
009600940506     D ENTER           C                   CONST(X'F1')
009700940506     D ROLDWN          C                   CONST(X'F4')
009800940506     D ROLLUP          C                   CONST(X'F5')
009900940127     C*----------------------------------------------------*
010000940127     C*                MAIN LINE PROGRAM
010100940127     C*----------------------------------------------------*
010200940223     C* inizializzazione variabili
010300940223     C                   EXSR      INZVAR
010400940223     C*
010500940223     C     $FINE         DOWEQ     *OFF
010600940131     C     $GEST         CASEQ     'S1'          GESS1
010700940117     C                   END
010800940117     C                   END
010900940325     C* fine programma
011000940325     C                   SETON                                            LR
011100030113     C************************************************************
011200030113     C* INIZIALIZZAZIONE VARIABILI
011300030113     C************************************************************
011400030113     C     INZVAR        BEGSR
011500030113     C*
011600030113     C* Pulizia campi e indicatori
011700030113     C                   MOVE      *ALL'0'       IN4049           10
011800030113     C                   MOVEA     IN4049        *IN(40)
011900030113     C                   CLEAR                   S1OPZ
012000030113     C* Variabili per gestione videate
012100030113     C*
012200030113     C                   MOVE      *OFF          $FINE
012300030113     C                   MOVE      *OFF          $INZS1
012400030113     C                   MOVE      *OFF          $EFILE
012500030113     C                   MOVE      *OFF          $ESCI
012600030113     C                   MOVE      *OFF          $RCDOK
012700030113     C                   Z-ADD     0             $ULKS1            3 0
012800030113     C*
012900030113     C                   MOVE      *ON           $INZS1
013000030113     C                   MOVE      'S1'          $GEST
013100030113     C*
013200030113     C* Variabili appoggio
013300030114     C                   Z-ADD     1             WPAG
013400030113     c*
013500030113     C                   ENDSR
013600940127     C************************************************************
013700940131     C* GESTIONE LISTA
013800940127     C************************************************************
013900940127     C     GESS1         BEGSR
014000030113     C*
014100940223     C* inizializzazione videata
014200940223     C     $INZS1        IFEQ      *ON
014300940127     C                   EXSR      INZS1
014400940223     C                   MOVE      *OFF          $INZS1
014500940127     C                   ENDIF
014600030113     C*
014700030113     C* emissione piede videata
014800030113     C                   WRITE     Z1
014900030113     C* Non ci sono records
015000940223     C     WMAX          IFEQ      0
015100940607     C                   WRITE     D1
015200030114     C                   Else
015300030114     C     Wsfl          IFgt      0
015400050511     C     Wsfl          andle     Wmax
015500030114     C                   Z-ADD     wsfl          C1RCD
015600030114     C                   Else
015700030114     C     Wpag          IFgt      0
015800040115     C     pagine        andgt     0
015900030114     C                   Z-ADD     wpag          C1RCD
016000050511     C                   Else
016100050511     C                   Z-ADD     1             C1RCD
016200030114     C                   EndIF
016300030114     C                   EndIF
016400030114     C                   ENDIF
016500940127     C*
016600030113     C*              *------------------*
016700940607     C                   EXFMT     C1
016800030113     C*              *------------------*
016900030113     C*
017000940204     C     C1NRR         IFNE      0
017100940204     C                   Z-ADD     C1NRR         WSFL
017200940204     C                   ENDIF
017300940127     C                   Z-ADD     SFLNRR        C1RCD
017400030113     C* Selezioni
0175009401271    C                   SELECT
017600940127     C* F3=Fine
017700940506     C     $TASTO        WHENEQ    F03
017800940309     C                   EXSR      F03S1
017900940131     C* F10=Immissione
018000940506     C     $TASTO        WHENEQ    F10
018100940309     C                   EXSR      F10S1
0182009401271O   C                   OTHER
018300940127     C* CONTROLLO DATI
018400940131     C                   EXSR      CTRC1
018500940201     C     *IN99         IFEQ      *OFF
018600940131     C                   EXSR      CTRS1
018700940131     C                   END
0188009401271-   C                   ENDSL
018900940127     C*
019000940127     C                   ENDSR
019100940224     C/EJECT
019200940127     C************************************************************
019300940131     C* INIZIALIZZAZIONE LISTA
019400940127     C************************************************************
019500940127     C     INZS1         BEGSR
019600940302     C* pulizia SFL
019700940128     C                   SETOFF                                         3031
019800940607     C                   WRITE     C1
019900940128     C                   SETON                                          31
020000940128     C*
020100030113     C* CARICAMENTO SFL totale
020200940201     C                   Z-ADD     0             S1NRR
020300030113     C                   Z-ADD     1             C1RCD
020400940128     C                   Z-ADD     0             WMAX
020500940224     C*
020600940224     C* Posizionamento su file pilota
020700050511     c     *loval        setll     Tise800F
020800940608    >C                   EXSR      REDANA
020900030113     C* Carico il SFL
021000940127     C                   EXSR      ROLS1
021100030113     C*
021200030114     c                   if        xtaopr <> '1'
021300030114     C                   Z-ADD     1             WPAG
021400030114     c                   end
021500940127     C*
021600940127     C                   ENDSR
021700940127     C************************************************************
021800940131     C* CARICAMENTO PAGINA LISTA
021900940127     C************************************************************
022000940127     C     ROLS1         BEGSR
022100940127     C*
022200940128     C                   SETOFF                                       32
022300940223     C                   Z-ADD     0             Y
022400940127     C                   Z-ADD     WMAX          S1NRR
022500940127     C*
022600940127     C* Leggo dal file anagrafico per caricare la lista
0227009401311    C     $EFILE        DOWEQ     *OFF
022800940127     C*
022900030113     c                   clear                   s1opz
023000050509     c                   movel     se8COD        s1cod
023100050509     c                   movel     se8UNI        s1uni
023200050509     c                   movel     se8DES        s1DES
023300050511     c                   movel     se8CLV        s1CLV
023400940127     C*
023500940127     C                   ADD       1             S1NRR
023600940127     C                   ADD       1             Y
023700940127     C*
023800940607     C                   WRITE     S1
023900940131     C*
024000940316    >C                   EXSR      REDANA
024100940128     C*
0242009401271-   C                   ENDDO
024300940127     C*
024400940223     C                   Z-ADD     S1NRR         WMAX                 30
024500940127     C*
024600940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
024700940127     C*
024800030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
024900940127     C                   MVR                     RESTO             3 0
025000030114     C     PAGINE        MULT      SFLPAG        C1RCD
0251000301141    C     RESTO         IFGT      0
025200030114     C                   ADD       1             C1RCD
0253000301141E   C                   ELSE
025400030114     C                   SUB       SFLPAG        C1RCD
025500030114     C                   ADD       1             C1RCD
0256000301141-   C                   ENDIF
025700940128     C*
025800940127     C                   ENDSR
025900940128     C************************************************************
026000940131     C* LETTURA RCD ARCHIVIO PILOTA
026100940128     C************************************************************
026200940607     C     REDANA        BEGSR
026300940128     C*
026400940131     C                   MOVEL     *OFF          $EFILE
026500940131     C                   MOVEL     *OFF          $RCDOK
026600940131     C*
0267009401311    C     $EFILE        DOUEQ     *ON
026800940131     C     $RCDOK        OREQ      *ON
026900030728     C*
027000050511     c                   Read      Tise800F
027100050511     c                   if        %eof(Tise800F)
027200030113     C                   MOVEL     *ON           $EFILE
027300030113     C                   MOVE      $EFILE        *IN33
027400030113     c                   else
027500030113     C                   MOVE      *ON           $RCDOK
027600030113     c                   end
027700940131     C*
0278009401311-   C                   ENDDO
027900940131     C*
028000940131     C                   ENDSR
028100940224     C************************************************************
028200940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
028300940224     C************************************************************
028400940224     C     CLCPAG        BEGSR
028500940224     C* Input :
028600940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
028700940224     C*          cursore
028800940224     C* - SFLPAG = numero rcd per pagina sfl
028900940224     C* Output :
029000940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
029100940224     C*
029200940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
029300940224     C                   MVR                     RESTO             3 0
029400940224     C     RESTO         IFGT      0
029500940224     C                   ADD       1             PAGINE
029600940224     C                   ENDIF
029700940224     C     PAGINE        MULT      SFLPAG        WPAG
029800940224     C*
029900940224     C                   ENDSR
030000940309     C************************************************************
030100940309     C* GESTIONE F03 VIDEO S1
030200940309     C************************************************************
030300940309     C     F03S1         BEGSR
030400940309     C*
030500940309     C                   MOVE      *ON           $FINE
030600940325     C* fine programma
030700940309     C                   ENDSR
030800940309     C/EJECT
030900940309     C************************************************************
031000940309     C* GESTIONE F10 VIDEO S1
031100940314     c* AGGIUNTA RECORD
031200940309     C************************************************************
031300940309     C     F10S1         BEGSR
031400940309     C*
031500030113     C                   RESET                   tabds
031600030729     C                   MOVEL     '1'           xtaopz
031700030113     C                   MOVE      *ZERO         xtaret
031800030113     C                   MOVE      *ZERO         xtaopr
031900050509     C                   MOVEl     *Zero         xtacod
032000030113     C                   MOVE      *BLANKS       KPJBU
032100030113     C                   MOVEL     tabds         KPJBU
032200050509$004 C                   CALL      'TISE80R1'
032300030113     C                   PARM                    KPJBA
032400030114     C                   MOVEL     KPJBU         tabds
032500030113      *
032600940309     C* ritorno da PGM gestione
032700940309     C                   EXSR      GESRET
032800940309     C     WINZS1        IFEQ      *ON
032900940309     C                   MOVE      *ON           $INZS1
033000940309     C* se esistevano già righe sul sfl
033100940309     C* calcolo pagina a cui deve posizionarsi
033200940309     C     WSFL          IFGT      0
033300940309     C                   EXSR      CLCPAG
033400940309     C* altrimenti carico solo la 1a pagina
033500940309     C                   ELSE
033600940309     C                   Z-ADD     SFLPAG        WPAG
033700940309     C                   ENDIF
033800940309     C                   ENDIF
033900940309     C*
034000940309     C                   ENDSR
034100940128     C************************************************************
034200940131     C* CONTROLLO TESTATA LISTA
034300940128     C************************************************************
034400940131     C     CTRC1         BEGSR
034500940128     C*
034600940201     C                   MOVE      *OFF          *IN99
034700940131     C*
034800940202     C                   ENDSR
034900940131     C************************************************************
035000940131     C* CONTROLLO OPZIONI LISTA
035100940131     C************************************************************
035200940131     C     CTRS1         BEGSR
035300940131     C*
035400940202     C                   MOVEL     *OFF          $ESCI
035500940201     C                   SETOFF                                       99
035600940131     C                   Z-ADD     0             S1OPZ
035700940131     C*
035800940127     C* Leggo il sfl solo se ci sono rcd
0359009401311    C     WMAX          IFGT      0
036000940607     C                   READC     S1                                     21
036100940127     C*
036200940131     C* esce se fine sfl o errore che richiede l'uscita
0363009401312    C     *IN21         DOWEQ     *OFF
036400940131     C     $ESCI         ANDEQ     *OFF
036500940201     C                   Z-ADD     S1NRR         C1RCD
036600940131     C* ctrl su riga
036700940131     C                   EXSR      RECS1
036800940131     C* gestione opzioni
0369009401313    C     S1OPZ         IFNE      0
037000940201     C     *IN99         ANDEQ     *OFF
037100940131     C                   EXSR      OPZS1
0372009401273-   C                   ENDIF
037300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0374009402013    C     *IN99         IFEQ      *ON
037500940201     C     $ESCI         OREQ      *ON
037600940131     C                   MOVE      *ON           *IN32
037700940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
037800940204     C* la ripristinerò a conclusione del ciclo di READC
037900940223     C                   MOVE      *OFF          $INZS1
0380009402233-   C                   ENDIF
038100940223     C*
038200940131     C                   Z-ADD     0             S1OPZ
038300940223     C*
038400940607     C                   UPDATE    S1
038500940223     C*
038600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0387009401313    C     $ESCI         IFEQ      *OFF
038800940607     C                   READC     S1                                     21
038900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0390009402014    C     *IN21         IFEQ      *ON
039100940201     C                   MOVE      WINZS1        $INZS1
039200940204     C* calcolo pagina a cui deve posizionarsi
039300940224     C                   EXSR      CLCPAG
0394009402014-   C                   ENDIF
0395009402013-   C                   ENDIF
039600940131     C*
0397009401272-   C                   ENDDO
039800940127     C*
0399009401311-   C                   ENDIF
040000940131     C*
040100940127     C                   ENDSR
040200940127     C/EJECT
040300940127     C************************************************************
040400940131     C* CONTROLLO CAMPI I/O RIGA LISTA
040500940127     C************************************************************
040600940131     C     RECS1         BEGSR
040700940131     C*
040800940201     C* reset indicatori DSPATR
040900940201     C                   MOVE      *ALL'0'       IN4049           10
041000940201     C                   MOVEA     IN4049        *IN(40)
041100940201     C*
041200940131     C                   ENDSR
041300940131     C************************************************************
041400940131     C* GESTIONE OPZIONI LISTA
041500940131     C************************************************************
041600940131     C     OPZS1         BEGSR
041700940201     C*
041800030113     C                   RESET                   tabds
041900030113     C                   MOVEL     S1OPZ         xtaopz
042000030113     C                   MOVE      *ZERO         xtaret
042100030113     C                   MOVE      *ZERO         xtaopr
042200030729     C                   MOVEl     s1cod         xtacod
042300940715     C                   MOVE      *BLANKS       KPJBU
042400030113     C                   MOVEL     tabds         KPJBU
042500050509$004 C                   CALL      'TISE80R1'
042600940607     C                   PARM                    KPJBA
042700030114     C                   MOVEL     KPJBU         tabds
042800940201     C*
042900940223     C* ritorno da PGM gestione
043000940223     C                   EXSR      GESRET
043100940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0432009402252    C     *IN99         IFEQ      *ON
043300940223     C                   SETON                                        40
0434009402252-   C                   ENDIF
043500940225     C*
043600940131     C                   ENDSR
043700940223     C/EJECT
043800940223     C************************************************************
043900940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
044000940223     C************************************************************
044100940223     C     GESRET        BEGSR
044200940223     C*
044300940223     C* modo di ritorno
044400940223     C*
0445009402231    C                   SELECT
044600940314    >C* << questi modi di utilizzo dei valori di ritorno dal
044700940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
044800940314    >C*    proposte, normalmente sempre valide, ma modificabili
044900940314    >C*    per situazioni particolari >>
045000940223     C* 1 = F3
045100030113    >C     xtaret        WHENEQ    '1'
045200940224     C                   MOVE      *ON           $ESCI
045300940223     C                   MOVE      *ON           $FINE
045400940223     C* 2 = F12
045500030113    >C     xtaret        WHENEQ    '2'
045600940223     C                   MOVE      *ON           $ESCI
045700940223     C*
0458009402231-   C                   ENDSL
045900940223     C*
046000940223     C* operazione eseguite dal pgm chiamato
046100940223     C*
0462009402231    C                   SELECT
046300940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
046400030113    >C     xtaopr        WHENEQ    '1'
046500940223     C                   MOVE      *ON           WINZS1
046600940223     C*
0467009402231-   C                   ENDSL
046800940223     C*
046900940223     C* funzione non eseguibile per errore :
047000940223     C*
0471009402231    C                   SELECT
047200940223     C* 1 = funzione richiamata chiusa in errore
047300940316    >C*  eventualmente gestire altri codici di errore
047400030113    >C     xtaerr        WHENEQ    '1'
047500940223     C                   MOVE      *ON           $ESCI
047600940223     C                   SETON                                        5299
047700940223     C*
0478009402231-   C                   ENDSL
047900940223     C*
048000940223     C                   ENDSR
048100940223     C/EJECT
048200940131     C************************************************************
048300940131     C* OPERAZIONI INIZIALI
048400940131     C************************************************************
048500940131     C     *INZSR        BEGSR
048600030113     C*
048700030113     C* Reperimento parametri
048800030113     C     *ENTRY        PLIST
048900030113     C                   PARM                    KPJBA
049000030113     C*
049100030113     C* Variabili per gestione videate
049200030113     C                   MOVE      *BLANK        $GEST             2
049300030113     C                   MOVE      *BLANK        $FINE             1
049400030113     C                   MOVE      *BLANK        $INZS1            1
049500030113     C                   MOVE      *BLANK        $EFILE            1
049600030113     C                   MOVE      *BLANK        $ESCI             1
049700030113     C                   MOVE      *BLANK        $RCDOK            1
049800030113     C* Indici
049900030113     C                   Z-ADD     0             X                 3 0
050000030113     C                   Z-ADD     0             Y                 3 0
050100940127     C*
050200940117     C                   ENDSR
050300030113     C************************************************************
