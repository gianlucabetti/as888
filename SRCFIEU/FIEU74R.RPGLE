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
002100040115     FTNcfp02L  IF   E           K DISK
002200040115     FFIEU74D   CF   E             WORKSTN
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
003500040115     D  XTACOD                 5      7
003600040115     D  XTANTW                 8     10
003700040115     D  XTAFIL                11     13  0
003800040115     D  XTADDA                14     21  0
003900040115     D  XTAADA                22     29  0
004000940211     D*-------------
004100940211     D DSFMT           DS           512
004200940506     D  $TASTO               369    369
004300940211     D  NRG                  370    370
004400940211     D  NCL                  371    371
004500940211     D  SFLNRR               378    379B 0
004600030728      *
004700030728     Ddataiso          s               d   datfmt(*iso)
004800030728     Ddataeur          s               d   datfmt(*eur)
004900940207     D*-------------
005000940207     D* Nome PGM a video
005100940207     D                 DS
005200940207     D  PROGR                  1     10
005300940207     D  ASTER1                 1      1    INZ('*')
005400940207     D  SIGLA                  2      9
005500940207     D  ASTER2                10     10    INZ('*')
005600940127     D*-------------
005700940127     D* Reperimento nome PGM
005800940127     D STATUS         SDS           333
005900940127     D  DSPGM            *PROC
006000030113     D*-------------
006100030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006200030113$003 D S1NRR           S                   Like(C1rcd)
006300030113$003 D WSfl            S                   Like(C1nrr)
006400030113$003 D Wmax            S                   Like(C1rcd)
006500030113$003 D Wpag            S                   Like(C1rcd)
006600030113$003 D Winzs1          S                   Like($inzs1)
006700940207     D*-------------
006800940211     D* COSTANTI
006900940211     D*-------------
007000030113     D SFLPAG          C                   CONST(11)
007100940314     D* dimensione della schiera $MS1
007200940506     D*
007300940506     D* Tasti di funzione
007400940506     D F01             C                   CONST(X'31')
007500940506     D F02             C                   CONST(X'32')
007600940506     D F03             C                   CONST(X'33')
007700940506     D F04             C                   CONST(X'34')
007800940506     D F05             C                   CONST(X'35')
007900940506     D F06             C                   CONST(X'36')
008000940506     D F07             C                   CONST(X'37')
008100940506     D F08             C                   CONST(X'38')
008200940506     D F09             C                   CONST(X'39')
008300940506     D F10             C                   CONST(X'3A')
008400940506     D F11             C                   CONST(X'3B')
008500940506     D F12             C                   CONST(X'3C')
008600940506     D F13             C                   CONST(X'B1')
008700940506     D F14             C                   CONST(X'B2')
008800940506     D F15             C                   CONST(X'B3')
008900940506     D F16             C                   CONST(X'B4')
009000940506     D F17             C                   CONST(X'B5')
009100940506     D F18             C                   CONST(X'B6')
009200940506     D F19             C                   CONST(X'B7')
009300940506     D F20             C                   CONST(X'B8')
009400940506     D F21             C                   CONST(X'B9')
009500940506     D F22             C                   CONST(X'BA')
009600940506     D F23             C                   CONST(X'BB')
009700940506     D F24             C                   CONST(X'BC')
009800940506     D ENTER           C                   CONST(X'F1')
009900940506     D ROLDWN          C                   CONST(X'F4')
010000940506     D ROLLUP          C                   CONST(X'F5')
010100940127     C*----------------------------------------------------*
010200940127     C*                MAIN LINE PROGRAM
010300940127     C*----------------------------------------------------*
010400940223     C* inizializzazione variabili
010500940223     C                   EXSR      INZVAR
010600940223     C*
010700940223     C     $FINE         DOWEQ     *OFF
010800940131     C     $GEST         CASEQ     'S1'          GESS1
010900940117     C                   END
011000940117     C                   END
011100940325     C* fine programma
011200940325     C                   SETON                                            LR
011300030113     C************************************************************
011400030113     C* INIZIALIZZAZIONE VARIABILI
011500030113     C************************************************************
011600030113     C     INZVAR        BEGSR
011700030113     C*
011800030113     C* Pulizia campi e indicatori
011900030113     C                   MOVE      *ALL'0'       IN4049           10
012000030113     C                   MOVEA     IN4049        *IN(40)
012100030113     C                   CLEAR                   S1OPZ
012200030113     C* Variabili per gestione videate
012300030113     C*
012400030113     C                   MOVE      *OFF          $FINE
012500030113     C                   MOVE      *OFF          $INZS1
012600030113     C                   MOVE      *OFF          $EFILE
012700030113     C                   MOVE      *OFF          $ESCI
012800030113     C                   MOVE      *OFF          $RCDOK
012900030113     C                   Z-ADD     0             $ULKS1            3 0
013000030113     C*
013100030113     C                   MOVE      *ON           $INZS1
013200030113     C                   MOVE      'S1'          $GEST
013300030113     C*
013400030113     C* Variabili appoggio
013500030114     C                   Z-ADD     1             WPAG
013600030113     c*
013700030113     C                   ENDSR
013800940127     C************************************************************
013900940131     C* GESTIONE LISTA
014000940127     C************************************************************
014100940127     C     GESS1         BEGSR
014200030113     C*
014300940223     C* inizializzazione videata
014400940223     C     $INZS1        IFEQ      *ON
014500940127     C                   EXSR      INZS1
014600940223     C                   MOVE      *OFF          $INZS1
014700940127     C                   ENDIF
014800030113     C*
014900030113     C* emissione piede videata
015000030113     C                   WRITE     Z1
015100030113     C* Non ci sono records
015200940223     C     WMAX          IFEQ      0
015300940607     C                   WRITE     D1
015400030114     C                   Else
015500030114     C     Wsfl          IFgt      0
015600030114     C                   Z-ADD     wsfl          C1RCD
015700030114     C                   Else
015800030114     C     Wpag          IFgt      0
015900040115     C     Pagine        andgt     0
016000030114     C                   Z-ADD     wpag          C1RCD
016100030114     C                   EndIF
016200030114     C                   EndIF
016300030114     C                   ENDIF
016400940127     C*
016500030113     C*              *------------------*
016600940607     C                   EXFMT     C1
016700030113     C*              *------------------*
016800030113     C*
016900940204     C     C1NRR         IFNE      0
017000940204     C                   Z-ADD     C1NRR         WSFL
017100940204     C                   ENDIF
017200940127     C                   Z-ADD     SFLNRR        C1RCD
017300030113     C* Selezioni
0174009401271    C                   SELECT
017500940127     C* F3=Fine
017600940506     C     $TASTO        WHENEQ    F03
017700940309     C                   EXSR      F03S1
017800940131     C* F10=Immissione
017900940506     C     $TASTO        WHENEQ    F10
018000940309     C                   EXSR      F10S1
0181009401271O   C                   OTHER
018200940127     C* CONTROLLO DATI
018300940131     C                   EXSR      CTRC1
018400940201     C     *IN99         IFEQ      *OFF
018500940131     C                   EXSR      CTRS1
018600940131     C                   END
0187009401271-   C                   ENDSL
018800940127     C*
018900940127     C                   ENDSR
019000940224     C/EJECT
019100940127     C************************************************************
019200940131     C* INIZIALIZZAZIONE LISTA
019300940127     C************************************************************
019400940127     C     INZS1         BEGSR
019500940302     C* pulizia SFL
019600940128     C                   SETOFF                                         3031
019700940607     C                   WRITE     C1
019800940128     C                   SETON                                          31
019900940128     C*
020000030113     C* CARICAMENTO SFL totale
020100940201     C                   Z-ADD     0             S1NRR
020200030113     C                   Z-ADD     1             C1RCD
020300940128     C                   Z-ADD     0             WMAX
020400940224     C*
020500940224     C* Posizionamento su file pilota
020600040115     c     *loval        setll     Tncfp02L
020700940608    >C                   EXSR      REDANA
020800030113     C* Carico il SFL
020900940127     C                   EXSR      ROLS1
021000030113     C*
021100030114     c                   if        xtaopr <> '1'
021200030114     C                   Z-ADD     1             WPAG
021300030114     c                   end
021400940127     C*
021500940127     C                   ENDSR
021600940127     C************************************************************
021700940131     C* CARICAMENTO PAGINA LISTA
021800940127     C************************************************************
021900940127     C     ROLS1         BEGSR
022000940127     C*
022100940128     C                   SETOFF                                       32
022200940223     C                   Z-ADD     0             Y
022300940127     C                   Z-ADD     WMAX          S1NRR
022400940127     C*
022500940127     C* Leggo dal file anagrafico per caricare la lista
0226009401311    C     $EFILE        DOWEQ     *OFF
022700940127     C*
022800030113     c                   clear                   s1opz
022900040115     c                   movel     cfpCAU        s1cod
023000040115     c                   movel     cfpNTW        s1ntw
023100040115     c                   move      cfpFIL        s1fil
023200040115     c                   move      cfpdda        h1dda
023300040115     c                   move      cfpdda        dataiso
023400030728     c                   move      dataiso       dataeur
023500030728     c                   move      dataeur       s1dda
023600040115     c                   move      cfpada        h1ada
023700040115     c                   move      cfpada        dataiso
023800030728     c                   move      dataiso       dataeur
023900030728     c                   move      dataeur       s1ada
024000040115     c                   movel     cfpGRP        s1grp
024100040115     c                   movel     cfpDES        s1DES
024200040115     c                   movel     cfpANN        s1ANN
024300940127     C*
024400940127     C                   ADD       1             S1NRR
024500940127     C                   ADD       1             Y
024600940127     C*
024700940607     C                   WRITE     S1
024800940131     C*
024900940316    >C                   EXSR      REDANA
025000940128     C*
0251009401271-   C                   ENDDO
025200940127     C*
025300940223     C                   Z-ADD     S1NRR         WMAX                 30
025400940127     C*
025500940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
025600940127     C*
025700030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
025800940127     C                   MVR                     RESTO             3 0
025900030114     C     PAGINE        MULT      SFLPAG        C1RCD
0260000301141    C     RESTO         IFGT      0
026100030114     C                   ADD       1             C1RCD
0262000301141E   C                   ELSE
026300030114     C                   SUB       SFLPAG        C1RCD
026400030114     C                   ADD       1             C1RCD
0265000301141-   C                   ENDIF
026600940128     C*
026700940127     C                   ENDSR
026800940128     C************************************************************
026900940131     C* LETTURA RCD ARCHIVIO PILOTA
027000940128     C************************************************************
027100940607     C     REDANA        BEGSR
027200940128     C*
027300940131     C                   MOVEL     *OFF          $EFILE
027400940131     C                   MOVEL     *OFF          $RCDOK
027500940131     C*
0276009401311    C     $EFILE        DOUEQ     *ON
027700940131     C     $RCDOK        OREQ      *ON
027800030728     C*
027900040115     c                   Read      Tncfp02L
028000040115     c                   if        %eof(Tncfp02L)
028100030113     C                   MOVEL     *ON           $EFILE
028200030113     C                   MOVE      $EFILE        *IN33
028300030113     c                   else
028400030113     C                   MOVE      *ON           $RCDOK
028500030113     c                   end
028600940131     C*
0287009401311-   C                   ENDDO
028800940131     C*
028900940131     C                   ENDSR
029000940224     C************************************************************
029100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
029200940224     C************************************************************
029300940224     C     CLCPAG        BEGSR
029400940224     C* Input :
029500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
029600940224     C*          cursore
029700940224     C* - SFLPAG = numero rcd per pagina sfl
029800940224     C* Output :
029900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
030000940224     C*
030100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
030200940224     C                   MVR                     RESTO             3 0
030300940224     C     RESTO         IFGT      0
030400940224     C                   ADD       1             PAGINE
030500940224     C                   ENDIF
030600940224     C     PAGINE        MULT      SFLPAG        WPAG
030700940224     C*
030800940224     C                   ENDSR
030900940309     C************************************************************
031000940309     C* GESTIONE F03 VIDEO S1
031100940309     C************************************************************
031200940309     C     F03S1         BEGSR
031300940309     C*
031400940309     C                   MOVE      *ON           $FINE
031500940325     C* fine programma
031600940309     C                   ENDSR
031700940309     C/EJECT
031800940309     C************************************************************
031900940309     C* GESTIONE F10 VIDEO S1
032000940314     c* AGGIUNTA RECORD
032100940309     C************************************************************
032200940309     C     F10S1         BEGSR
032300940309     C*
032400030113     C                   RESET                   tabds
032500030729     C                   MOVEL     '1'           xtaopz
032600030113     C                   MOVE      *ZERO         xtaret
032700030113     C                   MOVE      *ZERO         xtaopr
032800030729     C                   MOVEl     *blank        xtacod
032900030729     C                   MOVEl     *blank        xtantw
033000030729     C                   MOVEl     *zero         xtafil
033100030729     C                   MOVEl     *zero         xtadda
033200030729     C                   MOVEl     *zero         xtaada
033300030113     C                   MOVE      *BLANKS       KPJBU
033400030113     C                   MOVEL     tabds         KPJBU
033500040115$004 C                   CALL      'FIEU74R1'
033600030113     C                   PARM                    KPJBA
033700030114     C                   MOVEL     KPJBU         tabds
033800030113      *
033900940309     C* ritorno da PGM gestione
034000940309     C                   EXSR      GESRET
034100940309     C     WINZS1        IFEQ      *ON
034200940309     C                   MOVE      *ON           $INZS1
034300940309     C* se esistevano già righe sul sfl
034400940309     C* calcolo pagina a cui deve posizionarsi
034500940309     C     WSFL          IFGT      0
034600940309     C                   EXSR      CLCPAG
034700940309     C* altrimenti carico solo la 1a pagina
034800940309     C                   ELSE
034900940309     C                   Z-ADD     SFLPAG        WPAG
035000940309     C                   ENDIF
035100940309     C                   ENDIF
035200940309     C*
035300940309     C                   ENDSR
035400940128     C************************************************************
035500940131     C* CONTROLLO TESTATA LISTA
035600940128     C************************************************************
035700940131     C     CTRC1         BEGSR
035800940128     C*
035900940201     C                   MOVE      *OFF          *IN99
036000940131     C*
036100940202     C                   ENDSR
036200940131     C************************************************************
036300940131     C* CONTROLLO OPZIONI LISTA
036400940131     C************************************************************
036500940131     C     CTRS1         BEGSR
036600940131     C*
036700940202     C                   MOVEL     *OFF          $ESCI
036800940201     C                   SETOFF                                       99
036900940131     C                   Z-ADD     0             S1OPZ
037000940131     C*
037100940127     C* Leggo il sfl solo se ci sono rcd
0372009401311    C     WMAX          IFGT      0
037300940607     C                   READC     S1                                     21
037400940127     C*
037500940131     C* esce se fine sfl o errore che richiede l'uscita
0376009401312    C     *IN21         DOWEQ     *OFF
037700940131     C     $ESCI         ANDEQ     *OFF
037800940201     C                   Z-ADD     S1NRR         C1RCD
037900940131     C* ctrl su riga
038000940131     C                   EXSR      RECS1
038100940131     C* gestione opzioni
0382009401313    C     S1OPZ         IFNE      0
038300940201     C     *IN99         ANDEQ     *OFF
038400940131     C                   EXSR      OPZS1
0385009401273-   C                   ENDIF
038600940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0387009402013    C     *IN99         IFEQ      *ON
038800940201     C     $ESCI         OREQ      *ON
038900940131     C                   MOVE      *ON           *IN32
039000940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
039100940204     C* la ripristinerò a conclusione del ciclo di READC
039200940223     C                   MOVE      *OFF          $INZS1
0393009402233-   C                   ENDIF
039400940223     C*
039500940131     C                   Z-ADD     0             S1OPZ
039600940223     C*
039700940607     C                   UPDATE    S1
039800940223     C*
039900940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0400009401313    C     $ESCI         IFEQ      *OFF
040100940607     C                   READC     S1                                     21
040200940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0403009402014    C     *IN21         IFEQ      *ON
040400940201     C                   MOVE      WINZS1        $INZS1
040500940204     C* calcolo pagina a cui deve posizionarsi
040600940224     C                   EXSR      CLCPAG
0407009402014-   C                   ENDIF
0408009402013-   C                   ENDIF
040900940131     C*
0410009401272-   C                   ENDDO
041100940127     C*
0412009401311-   C                   ENDIF
041300940131     C*
041400940127     C                   ENDSR
041500940127     C/EJECT
041600940127     C************************************************************
041700940131     C* CONTROLLO CAMPI I/O RIGA LISTA
041800940127     C************************************************************
041900940131     C     RECS1         BEGSR
042000940131     C*
042100940201     C* reset indicatori DSPATR
042200940201     C                   MOVE      *ALL'0'       IN4049           10
042300940201     C                   MOVEA     IN4049        *IN(40)
042400940201     C*
042500940131     C                   ENDSR
042600940131     C************************************************************
042700940131     C* GESTIONE OPZIONI LISTA
042800940131     C************************************************************
042900940131     C     OPZS1         BEGSR
043000940201     C*
043100030113     C                   RESET                   tabds
043200030113     C                   MOVEL     S1OPZ         xtaopz
043300030113     C                   MOVE      *ZERO         xtaret
043400030113     C                   MOVE      *ZERO         xtaopr
043500030729     C                   MOVEl     s1cod         xtacod
043600030729     C                   MOVEl     s1ntw         xtantw
043700030729     C                   MOVEl     s1fil         xtafil
043800030729     C                   MOVEl     h1dda         xtadda
043900030729     C                   MOVEl     h1ada         xtaada
044000940715     C                   MOVE      *BLANKS       KPJBU
044100030113     C                   MOVEL     tabds         KPJBU
044200040115$004 C                   CALL      'FIEU74R1'
044300940607     C                   PARM                    KPJBA
044400030114     C                   MOVEL     KPJBU         tabds
044500940201     C*
044600940223     C* ritorno da PGM gestione
044700940223     C                   EXSR      GESRET
044800940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0449009402252    C     *IN99         IFEQ      *ON
045000940223     C                   SETON                                        40
0451009402252-   C                   ENDIF
045200940225     C*
045300940131     C                   ENDSR
045400940223     C/EJECT
045500940223     C************************************************************
045600940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
045700940223     C************************************************************
045800940223     C     GESRET        BEGSR
045900940223     C*
046000940223     C* modo di ritorno
046100940223     C*
0462009402231    C                   SELECT
046300940314    >C* << questi modi di utilizzo dei valori di ritorno dal
046400940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
046500940314    >C*    proposte, normalmente sempre valide, ma modificabili
046600940314    >C*    per situazioni particolari >>
046700940223     C* 1 = F3
046800030113    >C     xtaret        WHENEQ    '1'
046900940224     C                   MOVE      *ON           $ESCI
047000940223     C                   MOVE      *ON           $FINE
047100940223     C* 2 = F12
047200030113    >C     xtaret        WHENEQ    '2'
047300940223     C                   MOVE      *ON           $ESCI
047400940223     C*
0475009402231-   C                   ENDSL
047600940223     C*
047700940223     C* operazione eseguite dal pgm chiamato
047800940223     C*
0479009402231    C                   SELECT
048000940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
048100030113    >C     xtaopr        WHENEQ    '1'
048200940223     C                   MOVE      *ON           WINZS1
048300940223     C*
0484009402231-   C                   ENDSL
048500940223     C*
048600940223     C* funzione non eseguibile per errore :
048700940223     C*
0488009402231    C                   SELECT
048900940223     C* 1 = funzione richiamata chiusa in errore
049000940316    >C*  eventualmente gestire altri codici di errore
049100030113    >C     xtaerr        WHENEQ    '1'
049200940223     C                   MOVE      *ON           $ESCI
049300940223     C                   SETON                                        5299
049400940223     C*
0495009402231-   C                   ENDSL
049600940223     C*
049700940223     C                   ENDSR
049800940223     C/EJECT
049900940131     C************************************************************
050000940131     C* OPERAZIONI INIZIALI
050100940131     C************************************************************
050200940131     C     *INZSR        BEGSR
050300030113     C*
050400030113     C* Reperimento parametri
050500030113     C     *ENTRY        PLIST
050600030113     C                   PARM                    KPJBA
050700030113     C*
050800030113     C* Variabili per gestione videate
050900030113     C                   MOVE      *BLANK        $GEST             2
051000030113     C                   MOVE      *BLANK        $FINE             1
051100030113     C                   MOVE      *BLANK        $INZS1            1
051200030113     C                   MOVE      *BLANK        $EFILE            1
051300030113     C                   MOVE      *BLANK        $ESCI             1
051400030113     C                   MOVE      *BLANK        $RCDOK            1
051500030113     C* Indici
051600030113     C                   Z-ADD     0             X                 3 0
051700030113     C                   Z-ADD     0             Y                 3 0
051800940127     C*
051900940117     C                   ENDSR
052000030113     C************************************************************
