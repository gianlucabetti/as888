000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *
000500040930      *  11           x selezione di un codice da ripassare al pgm chiamante
000600940307      *  21           GENERICO OPERAZIONI I/O
000700940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000800940224      *  30           SFLDSP
000900940224      * N31           SFLCLR
001000940224      *  31           SFLDSPCTL
001100940224      *  32           SFLNXTCHG
001200940224      *  33           SFLEND
001300940224      *  39           OF PRTF
001400940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001500940608      *  Specificare l'uso dei singoli indicatori
001600940224      *  50 <---> 98  ERRORI SU VIDEO
001700940608      *  Specificare l'uso dei singoli indicatori
001800940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001900940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002000940224      *  99           INDIC. GENERALE DI ERRORE
002100940128     F*----------------------------------------------------*
002200090225     Fedstbl02L IF   E           K DISK
002300121224     Fedmap01l  IF   E           K DISK
002400090225     FTRTctb1D  CF   E             WORKSTN
002500940607     F                                     SFILE(S1:S1NRR)
002600090225     F                                     SFILE(S0:S0NRR)
002700940201     F                                     INFDS(DSFMT)
002800940128     D*----------------------------------------------------*
002900940211     D* Passaggio Parametri
003000940211     D KPJBA         E DS
003100030113      *-------------
003200940325     D* Parametri in ricezione
003300030113     D  TABDS          DS
003400030113     D  XTAOPZ                 1      2
003500030113     D  XTARET                 3      3
003600030113     D  XTAOPR                 4      4
003700030113     D  XTAERR                 5      5
003800030113     D  XTAKEY                 6     13
003900940211     D*-------------
004000940211     D DSFMT           DS           512
004100940506     D  $TASTO               369    369
004200940211     D  NRG                  370    370
004300940211     D  NCL                  371    371
004400940211     D  SFLNRR               378    379B 0
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
005800090225$003 D S0NRR           S                   Like(C1rcd)
005900090225$003 D S1NRR           S                   Like(C1rcd)
006000030113$003 D WSfl            S                   Like(C1nrr)
006100030113$003 D Wmax            S                   Like(C1rcd)
006200030113$003 D Wpag            S                   Like(C1rcd)
006300090225$003 D Winzs0          S                   Like($inzs0)
006400090225$003 D Winzs1          S                   Like($inzs1)
006500040930$003 D x1cod           S                   Like(s1cod)
006600090225$003 D x0cod           S                   Like(s0cod)
006700090225     D*-------------
006800090225$003 D segmento        S                   Like(etbSGM)
006900940207     D*-------------
007000940211     D* COSTANTI
007100940211     D*-------------
007200090225     D SFLPAG0         C                   CONST(11)
007300090225     D SFLPAG1         C                   CONST(06)
007400940314     D* dimensione della schiera $MS1
007500940506     D*
007600940506     D* Tasti di funzione
007700940506     D F01             C                   CONST(X'31')
007800940506     D F02             C                   CONST(X'32')
007900940506     D F03             C                   CONST(X'33')
008000940506     D F04             C                   CONST(X'34')
008100940506     D F05             C                   CONST(X'35')
008200940506     D F06             C                   CONST(X'36')
008300940506     D F07             C                   CONST(X'37')
008400940506     D F08             C                   CONST(X'38')
008500940506     D F09             C                   CONST(X'39')
008600940506     D F10             C                   CONST(X'3A')
008700940506     D F11             C                   CONST(X'3B')
008800940506     D F12             C                   CONST(X'3C')
008900940506     D F13             C                   CONST(X'B1')
009000940506     D F14             C                   CONST(X'B2')
009100940506     D F15             C                   CONST(X'B3')
009200940506     D F16             C                   CONST(X'B4')
009300940506     D F17             C                   CONST(X'B5')
009400940506     D F18             C                   CONST(X'B6')
009500940506     D F19             C                   CONST(X'B7')
009600940506     D F20             C                   CONST(X'B8')
009700940506     D F21             C                   CONST(X'B9')
009800940506     D F22             C                   CONST(X'BA')
009900940506     D F23             C                   CONST(X'BB')
010000940506     D F24             C                   CONST(X'BC')
010100940506     D ENTER           C                   CONST(X'F1')
010200940506     D ROLDWN          C                   CONST(X'F4')
010300940506     D ROLLUP          C                   CONST(X'F5')
010400940127     C*----------------------------------------------------*
010500940127     C*                MAIN LINE PROGRAM
010600940127     C*----------------------------------------------------*
010700940223     C* inizializzazione variabili
010800940223     C                   EXSR      INZVAR
010900940223     C*
011000940223     C     $FINE         DOWEQ     *OFF
011100090225     C     $GEST         CASEQ     'S0'          GESS0
011200940131     C     $GEST         CASEQ     'S1'          GESS1
011300940117     C                   END
011400940117     C                   END
011500940325     C* fine programma
011600940325     C                   SETON                                            LR
011700030113     C************************************************************
011800030113     C* INIZIALIZZAZIONE VARIABILI
011900030113     C************************************************************
012000030113     C     INZVAR        BEGSR
012100030113     C*
012200030113     C* Pulizia campi e indicatori
012300030113     C                   MOVE      *ALL'0'       IN4049           10
012400030113     C                   MOVEA     IN4049        *IN(40)
012500030113     C                   CLEAR                   S1OPZ
012600090225     C                   CLEAR                   S0OPZ
012700030113     C*
012800090225     C* Variabili per gestione videate
012900030113     C                   MOVE      *OFF          $FINE
013000030113     C                   MOVE      *OFF          $EFILE
013100030113     C                   MOVE      *OFF          $ESCI
013200030113     C                   MOVE      *OFF          $RCDOK
013300030113     C                   Z-ADD     0             $ULKS1            3 0
013400030113     C*
013500030113     C                   MOVE      *ON           $INZS1
013600090225     C                   MOVE      *ON           $INZS0
013700090225     C                   MOVE      'S0'          $GEST
013800030113     C*
013900030113     C* Variabili appoggio
014000030114     C                   Z-ADD     1             WPAG
014100030113     c*
014200030113     C                   ENDSR
014300090225     C************************************************************
014400090225     C* GESTIONE LISTA
014500090225     C************************************************************
014600090225     C     GESS0         BEGSR
014700090225     C*
014800090225     C* inizializzazione videata
014900090225     C     $INZS0        IFEQ      *ON
015000090225     C                   EXSR      INZS0
015100090225     C                   MOVE      *OFF          $INZS0
015200090225     C                   ENDIF
015300090225     C*
015400090225     C* emissione piede videata
015500090225     C                   WRITE     Z0
015600090225     C* Non ci sono records
015700090225     C     WMAX          IFEQ      0
015800090225     C                   WRITE     D0
015900090225     C                   Else
016000090225     C     Wsfl          IFgt      0
016100090225     C                   Z-ADD     wsfl          C0RCD
016200090225     C                   Else
016300090225     C     Wpag          IFgt      0
016400090225     C                   Z-ADD     wpag          C0RCD
016500090225     C                   EndIF
016600090225     C                   EndIF
016700090225     C                   ENDIF
016800090225     C*
016900090225     C*              *------------------*
017000090225     C                   EXFMT     C0
017100090225     C*              *------------------*
017200090225     C*
017300090225     C     C0NRR         IFNE      0
017400090225     C                   Z-ADD     C0NRR         WSFL
017500090225     C                   ENDIF
017600090225     C                   Z-ADD     SFLNRR        C0RCD
017700090225     C* Selezioni
0178000902251    C                   SELECT
017900090225     C* F3=Fine
018000090225     C     $TASTO        WHENEQ    F03
018100090225     C                   EXSR      F03S1
0182000902251O   C                   OTHER
018300090225     C* CONTROLLO DATI
018400090225     C                   EXSR      CTRC0
018500090225     C     *IN99         IFEQ      *OFF
018600090225     C                   EXSR      CTRS0
018700090225     C                   END
0188000902251-   C                   ENDSL
018900090225     C*
019000090225     C                   ENDSR
019100090225     C/EJECT
019200090225     C************************************************************
019300090225     C* INIZIALIZZAZIONE LISTA
019400090225     C************************************************************
019500090225     C     INZS0         BEGSR
019600090225     C* pulizia SFL
019700090225     C                   SETOFF                                         3031
019800090225     C                   WRITE     C0
019900090225     C                   SETON                                          31
020000090225     C*
020100090225     C* CARICAMENTO SFL totale
020200090225     C                   Z-ADD     0             S0NRR
020300090225     C                   Z-ADD     1             C0RCD
020400090225     C                   Z-ADD     0             WMAX
020500090225     C                   Z-ADD     0             wsfl
020600090225     C*
020700090225     C*  Posizionamento all'inizio
020800121224     c     *Loval        setll     edmap01l
020900090225    >C                   EXSR      REDSEGM
021000090225     C* Carico il SFL
021100090225     C                   EXSR      ROLS0
021200090225     C*
021300090225     c                   if        xtaopr <> '1'
021400090225     C                   Z-ADD     1             WPAG
021500090225     c                   end
021600090225     C*
021700090225     C                   ENDSR
021800090225     C************************************************************
021900090225     C* CARICAMENTO PAGINA LISTA
022000090225     C************************************************************
022100090225     C     ROLS0         BEGSR
022200090225     C*
022300090225     C                   SETOFF                                       32
022400090225     C                   Z-ADD     0             Y
022500090225     C                   Z-ADD     WMAX          S0NRR
022600090225     C*
022700090225     C* Leggo dal file i segmenti presenti nell'archivio
0228000902251    C     $EFILE        DOWEQ     *OFF
022900090225     C*
023000090225     c                   clear                   s0opz
023100090225     c*
023200090225     c* Prendo tipo segmento generale
023300121224     c                   if        mapSEQUEN = 0 and mapPROGR = 0
023400121224     c                   eval      s0cod =  mapCODICE
023500121224     c                   eval      s0des =  mapDESCR
023600090225     C                   ADD       1             S0NRR
023700090225     C                   ADD       1             Y
023800090225     C                   WRITE     S0
023900090225     c                   end
024000090225     C*
024100090225    >C                   EXSR      REDsegm
024200090225     C*
0243000902251-   C                   ENDDO
024400090225     C*
024500090225     C                   Z-ADD     S0NRR         WMAX                 30
024600090225     C*
024700090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
024800090225     C*
024900090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
025000090225     C                   MVR                     RESTO             3 0
025100090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0252000902251    C     RESTO         IFGT      0
025300090225     C                   ADD       1             C0RCD
0254000902251E   C                   ELSE
025500090225     C                   SUB       SFLPAG0       C0RCD
025600090225     C                   ADD       1             C0RCD
0257000902251-   C                   ENDIF
025800090225     C*
025900090225     C                   ENDSR
026000090225     C************************************************************
026100090225     C* LETTURA RCD
026200090225     C************************************************************
026300090225     C     REDsegm       BEGSR
026400090225     C*
026500090225     C                   MOVEL     *OFF          $EFILE
026600090225     C                   MOVEL     *OFF          $RCDOK
026700090225     C*
0268000902251    C     $EFILE        DOUEQ     *ON
026900090225     C     $RCDOK        OREQ      *ON
027000090225     C*
027100121224     c                   Read      edmap01L
027200121224     c                   if        %eof(edmap01L)
027300090225     C                   MOVEL     *ON           $EFILE
027400090225     C                   MOVE      $EFILE        *IN33
027500090225     c                   else
027600090225     C                   MOVE      *ON           $RCDOK
027700090225     c                   end
027800090225     C*
0279000902251-   C                   ENDDO
028000090225     C*
028100090225     C                   ENDSR
028200090225     C************************************************************
028300090225     C* CONTROLLO TESTATA LISTA
028400090225     C************************************************************
028500090225     C     CTRC0         BEGSR
028600090225     C*
028700090225     C                   MOVE      *OFF          *IN99
028800090225     C*
028900090225     C                   ENDSR
029000090225     C************************************************************
029100090225     C* CONTROLLO OPZIONI LISTA
029200090225     C************************************************************
029300090225     C     CTRS0         BEGSR
029400090225     C*
029500090225     C                   MOVEL     *OFF          $ESCI
029600090225     C                   SETOFF                                       99
029700090225     C                   clear                   S0OPZ
029800090225     c                   clear                   x0cod
029900090225     C*
030000090225     C* Leggo il sfl solo se ci sono rcd
0301000902251    C     WMAX          IFGT      0
030200090225     C                   READC     S0                                     21
030300090225     C*
030400090225     C* esce se fine sfl o errore che richiede l'uscita
0305000902252    C     *IN21         DOWEQ     *OFF
030600090225     C     $ESCI         ANDEQ     *OFF
030700090225     C                   Z-ADD     S0NRR         C0RCD
030800090225     C* ctrl su riga
030900090225     C                   EXSR      RECS0
031000090225     C* gestione opzioni
0311000902253    C     S0OPZ         IFNE      *blank
031200090225     C     *IN99         ANDEQ     *OFF
031300090225     C                   EXSR      OPZS0
0314000902253-   C                   ENDIF
031500090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0316000902253    C     *IN99         IFEQ      *ON
031700090225     C     $ESCI         OREQ      *ON
031800090225     C                   MOVE      *ON           *IN32
031900090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
032000090225     C* la ripristinerò a conclusione del ciclo di READC
032100090225     C                   MOVE      *OFF          $INZS0
0322000902253-   C                   ENDIF
032300090225     C*
032400090225     C                   clear                   S0OPZ
032500090225     C*
032600090225     C                   UPDATE    S0
032700090225     C*
032800090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0329000902253    C     $ESCI         IFEQ      *OFF
033000090225     C                   READC     S0                                     21
033100090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0332000902254    C     *IN21         IFEQ      *ON
033300090225     C                   MOVE      WINZS0        $INZS0
033400090225     C* calcolo pagina a cui deve posizionarsi
033500090225     C                   EXSR      CLCPAG0
0336000902254-   C                   ENDIF
0337000902253-   C                   ENDIF
033800090225     C*
0339000902252-   C                   ENDDO
034000090225     C*
0341000902251-   C                   ENDIF
034200090225     C*
034300090225     C                   ENDSR
034400090225     C/EJECT
034500090225     C************************************************************
034600090225     C* CONTROLLO CAMPI I/O RIGA LISTA
034700090225     C************************************************************
034800090225     C     RECS0         BEGSR
034900090225     C*
035000090225     C* reset indicatori DSPATR
035100090225     C                   MOVE      *ALL'0'       IN4049           10
035200090225     C                   MOVEA     IN4049        *IN(40)
035300090225     C*
035400090225     C                   ENDSR
035500090225     C************************************************************
035600090225     C* GESTIONE OPZIONI LISTA
035700090225     C************************************************************
035800090225     C     OPZS0         BEGSR
035900090225     C*
036000090225      * selezionato un codice da restituire al chiamante
0361000902253    C     S0OPZ         IFeq      '1'
036200090225     c                   move      s0cod         segmento
036300090225     C                   MOVE      'S1'          $GEST
036400090225     C                   MOVEL     *ON           $ESCI
036500090225     C                   END
036600090225     C*
036700090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0368000902252    C     *IN99         IFEQ      *ON
036900090225     C                   SETON                                        40
0370000902252-   C                   ENDIF
037100090225     C*
037200090225     C                   ENDSR
037300090225     C/EJECT
037400940127     C************************************************************
037500940131     C* GESTIONE LISTA
037600940127     C************************************************************
037700940127     C     GESS1         BEGSR
037800030113     C*
037900940223     C* inizializzazione videata
038000940223     C     $INZS1        IFEQ      *ON
038100940127     C                   EXSR      INZS1
038200940223     C                   MOVE      *OFF          $INZS1
038300940127     C                   ENDIF
038400030113     C*
038500030113     C* emissione piede videata
038600030113     C                   WRITE     Z1
038700030113     C* Non ci sono records
038800940223     C     WMAX          IFEQ      0
038900940607     C                   WRITE     D1
039000030114     C                   Else
039100030114     C     Wsfl          IFgt      0
039200030114     C                   Z-ADD     wsfl          C1RCD
039300030114     C                   Else
039400030114     C     Wpag          IFgt      0
039500030114     C                   Z-ADD     wpag          C1RCD
039600030114     C                   EndIF
039700030114     C                   EndIF
039800030114     C                   ENDIF
039900940127     C*
040000030113     C*              *------------------*
040100940607     C                   EXFMT     C1
040200030113     C*              *------------------*
040300030113     C*
040400940204     C     C1NRR         IFNE      0
040500940204     C                   Z-ADD     C1NRR         WSFL
040600940204     C                   ENDIF
040700940127     C                   Z-ADD     SFLNRR        C1RCD
040800030113     C* Selezioni
0409009401271    C                   SELECT
041000940127     C* F3=Fine
041100940506     C     $TASTO        WHENEQ    F03
041200940309     C                   EXSR      F03S1
041300940131     C* F10=Immissione
041400940506     C     $TASTO        WHENEQ    F10
041500940309     C                   EXSR      F10S1
041600090225     C* F12=Ritorno
041700090225     C     $TASTO        WHENEQ    F12
041800090225     C                   eval      $Gest='S0'
041900090225     C                   MOVE      *ON           $INZS1
042000090225     C                   MOVE      *ON           $INZS0
0421009401271O   C                   OTHER
042200940127     C* CONTROLLO DATI
042300940131     C                   EXSR      CTRC1
042400940201     C     *IN99         IFEQ      *OFF
042500940131     C                   EXSR      CTRS1
042600940131     C                   END
0427009401271-   C                   ENDSL
042800940127     C*
042900940127     C                   ENDSR
043000940224     C/EJECT
043100940127     C************************************************************
043200940131     C* INIZIALIZZAZIONE LISTA
043300940127     C************************************************************
043400940127     C     INZS1         BEGSR
043500940302     C* pulizia SFL
043600940128     C                   SETOFF                                         3031
043700940607     C                   WRITE     C1
043800940128     C                   SETON                                          31
043900940128     C*
044000030113     C* CARICAMENTO SFL totale
044100940201     C                   Z-ADD     0             S1NRR
044200030113     C                   Z-ADD     1             C1RCD
044300940128     C                   Z-ADD     0             WMAX
044400090225     C                   Z-ADD     0             wsfl
044500090225     C                   move      segmento      c1SEG
044600940224     C*
044700940224     C* Posizionamento su file pilota
044800090225     c     kTBL          setll     edstbl02L
044900940608    >C                   EXSR      REDANA
045000030113     C* Carico il SFL
045100940127     C                   EXSR      ROLS1
045200030113     C*
045300030114     c                   if        xtaopr <> '1'
045400030114     C                   Z-ADD     1             WPAG
045500030114     c                   end
045600940127     C*
045700940127     C                   ENDSR
045800940127     C************************************************************
045900940131     C* CARICAMENTO PAGINA LISTA
046000940127     C************************************************************
046100940127     C     ROLS1         BEGSR
046200940127     C*
046300940128     C                   SETOFF                                       32
046400940223     C                   Z-ADD     0             Y
046500940127     C                   Z-ADD     WMAX          S1NRR
046600940127     C*
046700940127     C* Leggo dal file anagrafico per caricare la lista
0468009401311    C     $EFILE        DOWEQ     *OFF
046900940127     C*
047000030113     c                   clear                   s1opz
047100090225     c                   eval      s1cod = etbvalsgm
047200090225     c                   eval      s1unb = etbunb
047300090225     c                   eval      h1unb = etbunb
047400090225     c                   movel     etbdescri     s1des
047500090225     c                   movel     etbdati       s1dati
047600940127     C*
047700940127     C                   ADD       1             S1NRR
047800940127     C                   ADD       1             Y
047900940127     C*
048000940607     C                   WRITE     S1
048100940131     C*
048200940316    >C                   EXSR      REDANA
048300940128     C*
0484009401271-   C                   ENDDO
048500940127     C*
048600940223     C                   Z-ADD     S1NRR         WMAX                 30
048700940127     C*
048800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
048900940127     C*
049000090225     C     S1NRR         DIV       SFLPAG1       PAGINE            4 0
049100940127     C                   MVR                     RESTO             3 0
049200090225     C     PAGINE        MULT      SFLPAG1       C1RCD
0493000301141    C     RESTO         IFGT      0
049400030114     C                   ADD       1             C1RCD
0495000301141E   C                   ELSE
049600090225     C                   SUB       SFLPAG1       C1RCD
049700030114     C                   ADD       1             C1RCD
0498000301141-   C                   ENDIF
049900940128     C*
050000940127     C                   ENDSR
050100940128     C************************************************************
050200940131     C* LETTURA RCD ARCHIVIO PILOTA
050300940128     C************************************************************
050400940607     C     REDANA        BEGSR
050500940128     C*
050600940131     C                   MOVEL     *OFF          $EFILE
050700940131     C                   MOVEL     *OFF          $RCDOK
050800940131     C*
0509009401311    C     $EFILE        DOUEQ     *ON
051000940131     C     $RCDOK        OREQ      *ON
051100940131     C*
051200090225     c     ktbl          Reade     edstbl02L
051300090225     c                   if        %eof(edstbl02L)
051400030113     C                   MOVEL     *ON           $EFILE
051500030113     C                   MOVE      $EFILE        *IN33
051600030113     c                   else
051700030113     C                   MOVE      *ON           $RCDOK
051800030113     c                   end
051900940131     C*
0520009401311-   C                   ENDDO
052100940131     C*
052200940131     C                   ENDSR
052300940224     C************************************************************
052400940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
052500940224     C************************************************************
052600090225     C     CLCPAG0       BEGSR
052700940224     C* Input :
052800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
052900940224     C*          cursore
053000940224     C* - SFLPAG = numero rcd per pagina sfl
053100940224     C* Output :
053200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
053300940224     C*
053400090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
053500940224     C                   MVR                     RESTO             3 0
053600940224     C     RESTO         IFGT      0
053700940224     C                   ADD       1             PAGINE
053800940224     C                   ENDIF
053900090226     c                   if        pagine > 1
054000090225     C     PAGINE        MULT      SFLPAG0       WPAG
054100090226     C                   END
054200940224     C*
054300940224     C                   ENDSR
054400090225     C************************************************************
054500090225     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
054600090225     C************************************************************
054700090225     C     CLCPAG1       BEGSR
054800090225     C* Input :
054900090225     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
055000090225     C*          cursore
055100090225     C* - SFLPAG = numero rcd per pagina sfl
055200090225     C* Output :
055300090225     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
055400090225     C*
055500090225     C     WSFL          DIV       SFLPAG1       PAGINE            4 0
055600090225     C                   MVR                     RESTO             3 0
055700090225     C     RESTO         IFGT      0
055800090225     C                   ADD       1             PAGINE
055900090225     C                   ENDIF
056000090226     c                   if        pagine > 1
056100090225     C     PAGINE        MULT      SFLPAG1       WPAG
056200090226     C                   ENDIF
056300090225     C*
056400090225     C                   ENDSR
056500940309     C************************************************************
056600940309     C* GESTIONE F03 VIDEO S1
056700940309     C************************************************************
056800940309     C     F03S1         BEGSR
056900940309     C*
057000940309     C                   MOVE      *ON           $FINE
057100940325     C* fine programma
057200940309     C                   ENDSR
057300940309     C/EJECT
057400940309     C************************************************************
057500940309     C* GESTIONE F10 VIDEO S1
057600940314     c* AGGIUNTA RECORD
057700940309     C************************************************************
057800940309     C     F10S1         BEGSR
057900940309     C*
058000030113     C                   RESET                   tabds
058100030113     C                   MOVEL     '01'          xtaopz
058200030113     C                   MOVE      *ZERO         xtaret
058300030113     C                   MOVE      *ZERO         xtaopr
058400030114     C                   MOVEl     *blank        xtakey
058500030113     C                   MOVE      *BLANKS       KPJBU
058600030113     C                   MOVEL     tabds         KPJBU
058700090225     C                   move      c1SEG         KeySegmento       3
058800090225     C                   clear                   KeyTabella       35
058900090225     C                   clear                   KeyCliente       35
059000090225$004 C                   CALL      'TRTCTB1R2'
059100030113     C                   PARM                    KPJBA
059200090225     C                   PARM                    KeySegmento       3
059300090225     C                   PARM                    KeyTabella       35
059400090225     C                   PARM                    KeyCliente       35
059500090225      *
059600030114     C                   MOVEL     KPJBU         tabds
059700030113      *
059800940309     C* ritorno da PGM gestione
059900940309     C                   EXSR      GESRET
060000090226     C                   MOVE      'S1'          $GEST
060100090226      *
060200940309     C     WINZS1        IFEQ      *ON
060300940309     C                   MOVE      *ON           $INZS1
060400090226     C* carico sempre la 1a pagina
060500090226     C                   Z-ADD     1             WPAG
060600940309     C                   ENDIF
060700940309     C*
060800940309     C                   ENDSR
060900940128     C************************************************************
061000940131     C* CONTROLLO TESTATA LISTA
061100940128     C************************************************************
061200940131     C     CTRC1         BEGSR
061300940128     C*
061400940201     C                   MOVE      *OFF          *IN99
061500940131     C*
061600940202     C                   ENDSR
061700940131     C************************************************************
061800940131     C* CONTROLLO OPZIONI LISTA
061900940131     C************************************************************
062000940131     C     CTRS1         BEGSR
062100940131     C*
062200940202     C                   MOVEL     *OFF          $ESCI
062300940201     C                   SETOFF                                       99
062400940131     C                   Z-ADD     0             S1OPZ
062500040930     c                   clear                   x1cod
062600940131     C*
062700940127     C* Leggo il sfl solo se ci sono rcd
0628009401311    C     WMAX          IFGT      0
062900940607     C                   READC     S1                                     21
063000940127     C*
063100940131     C* esce se fine sfl o errore che richiede l'uscita
0632009401312    C     *IN21         DOWEQ     *OFF
063300940131     C     $ESCI         ANDEQ     *OFF
063400940201     C                   Z-ADD     S1NRR         C1RCD
063500940131     C* ctrl su riga
063600940131     C                   EXSR      RECS1
063700940131     C* gestione opzioni
0638009401313    C     S1OPZ         IFNE      0
063900940201     C     *IN99         ANDEQ     *OFF
064000940131     C                   EXSR      OPZS1
0641009401273-   C                   ENDIF
064200940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0643009402013    C     *IN99         IFEQ      *ON
064400940201     C     $ESCI         OREQ      *ON
064500940131     C                   MOVE      *ON           *IN32
064600940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
064700940204     C* la ripristinerò a conclusione del ciclo di READC
064800940223     C                   MOVE      *OFF          $INZS1
0649009402233-   C                   ENDIF
065000940223     C*
065100940131     C                   Z-ADD     0             S1OPZ
065200940223     C*
065300940607     C                   UPDATE    S1
065400940223     C*
065500940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0656009401313    C     $ESCI         IFEQ      *OFF
065700940607     C                   READC     S1                                     21
065800940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0659009402014    C     *IN21         IFEQ      *ON
066000940201     C                   MOVE      WINZS1        $INZS1
066100940204     C* calcolo pagina a cui deve posizionarsi
066200090225     C                   EXSR      CLCPAG1
0663009402014-   C                   ENDIF
0664009402013-   C                   ENDIF
066500940131     C*
0666009401272-   C                   ENDDO
066700940127     C*
0668009401311-   C                   ENDIF
066900940131     C*
067000940127     C                   ENDSR
067100940127     C/EJECT
067200940127     C************************************************************
067300940131     C* CONTROLLO CAMPI I/O RIGA LISTA
067400940127     C************************************************************
067500940131     C     RECS1         BEGSR
067600940131     C*
067700940201     C* reset indicatori DSPATR
067800940201     C                   MOVE      *ALL'0'       IN4049           10
067900940201     C                   MOVEA     IN4049        *IN(40)
068000940201     C*
068100040930     C*  se richiamato x selezionare un codice 11 = *on
068200040930     C*   non deve utilizzare le opzioni di modifica
068300040930     C*  e viceversa
068400040930     c     *in11         ifeq      *on
0685000409303    C     S1OPZ         IFeq      2
0686000409303    C     S1OPZ         OReq      3
0687000409303    C     S1OPZ         OReq      4
068800040930     c                   seton                                        41  99
068900040930     C                   END
069000040930      * selezionato un codice da restituire al chiamante
0691000409303    C     S1OPZ         IFeq      1
069200040930     c                   move      s1cod         x1cod
069300040930     c                   clear                   kpjbu
069400040930     c                   movel     x1cod         kpjbu
069500040930     C                   MOVEL     *ON           $ESCI
069600040930     C                   MOVEL     *ON           $fine
069700040930     C                   END
069800040930     C                   END
069900040930     C*
070000040930     c     *in11         ifeq      *off
0701000409303    C     S1OPZ         ANDeq     1
070200040930     c                   seton                                        41  99
070300040930     C                   END
070400040930     C*
070500940131     C                   ENDSR
070600940131     C************************************************************
070700940131     C* GESTIONE OPZIONI LISTA
070800940131     C************************************************************
070900940131     C     OPZS1         BEGSR
071000940201     C*
0711000409302    C     *IN11         IFEQ      *Off
0712000409302    C     *IN11         oreq      *On
0713000409302    C     s1opz         andeq     05
071400040930     C*
071500030113     C                   RESET                   tabds
071600030113     C                   MOVEL     S1OPZ         xtaopz
071700030113     C                   MOVE      *ZERO         xtaret
071800030113     C                   MOVE      *ZERO         xtaopr
071900030113     C                   MOVEl     S1COD         xtakey
072000940715     C                   MOVE      *BLANKS       KPJBU
072100030113     C                   MOVEL     tabds         KPJBU
072200090225     C                   move      c1SEG         KeySegmento       3
072300090225     C                   movel     s1cod         KeyTabella       35
072400090225     C                   movel     h1UNB         KeyCliente       35
072500090225$004 C                   CALL      'TRTCTB1R2'
072600940607     C                   PARM                    KPJBA
072700090225     C                   PARM                    KeySegmento       3
072800090225     C                   PARM                    KeyTabella       35
072900090225     C                   PARM                    KeyCliente       35
073000090225      *
073100030114     C                   MOVEL     KPJBU         tabds
073200940201     C*
073300940223     C* ritorno da PGM gestione
073400940223     C                   EXSR      GESRET
073500040930     C*
073600940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0737009402252    C     *IN99         IFEQ      *ON
073800940223     C                   SETON                                        40
0739009402252-   C                   ENDIF
074000940225     C*
0741000409302-   C                   ENDIF
074200040930     C*
074300940131     C                   ENDSR
074400940223     C/EJECT
074500940223     C************************************************************
074600940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
074700940223     C************************************************************
074800940223     C     GESRET        BEGSR
074900940223     C*
075000940223     C* modo di ritorno
075100940223     C*
0752009402231    C                   SELECT
075300940314    >C* << questi modi di utilizzo dei valori di ritorno dal
075400940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
075500940314    >C*    proposte, normalmente sempre valide, ma modificabili
075600940314    >C*    per situazioni particolari >>
075700940223     C* 1 = F3
075800030113    >C     xtaret        WHENEQ    '1'
075900940224     C                   MOVE      *ON           $ESCI
076000940223     C                   MOVE      *ON           $FINE
076100940223     C* 2 = F12
076200030113    >C     xtaret        WHENEQ    '2'
076300940223     C                   MOVE      *ON           $ESCI
076400940223     C*
0765009402231-   C                   ENDSL
076600940223     C*
076700940223     C* operazione eseguite dal pgm chiamato
076800940223     C*
0769009402231    C                   SELECT
077000940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
077100030113    >C     xtaopr        WHENEQ    '1'
077200940223     C                   MOVE      *ON           WINZS1
077300940223     C*
0774009402231-   C                   ENDSL
077500940223     C*
077600940223     C* funzione non eseguibile per errore :
077700940223     C*
0778009402231    C                   SELECT
077900940223     C* 1 = funzione richiamata chiusa in errore
078000940316    >C*  eventualmente gestire altri codici di errore
078100030113    >C     xtaerr        WHENEQ    '1'
078200940223     C                   MOVE      *ON           $ESCI
078300940223     C                   SETON                                        5299
078400940223     C*
0785009402231-   C                   ENDSL
078600940223     C*
078700940223     C                   ENDSR
078800940223     C/EJECT
078900940131     C************************************************************
079000940131     C* OPERAZIONI INIZIALI
079100940131     C************************************************************
079200940131     C     *INZSR        BEGSR
079300030113     C*
079400030113     C* Reperimento parametri
079500030113     C     *ENTRY        PLIST
079600030113     C                   PARM                    KPJBA
079700040930     C                   movel     kpjbu         Selez             1
079800040930     C*
079900040930     C* Se chiamato x selezionare un codice
080000040930     c                   clear                   x1cod
080100040930     c                   if        selez = '?'
080200040930     c                   seton                                        11
080300040930     c                   end
080400030113     C*
080500030113     C* Variabili per gestione videate
080600030113     C                   MOVE      *BLANK        $GEST             2
080700030113     C                   MOVE      *BLANK        $FINE             1
080800030113     C                   MOVE      *BLANK        $INZS1            1
080900090225     C                   MOVE      *BLANK        $INZS0            1
081000030113     C                   MOVE      *BLANK        $EFILE            1
081100030113     C                   MOVE      *BLANK        $ESCI             1
081200030113     C                   MOVE      *BLANK        $RCDOK            1
081300030113     C*
081400030113     C     KTBL          KLIST
081500090225     C                   KFLD                    SEGMENTO          3
081600030113     C* Indici
081700030113     C                   Z-ADD     0             X                 3 0
081800030113     C                   Z-ADD     0             Y                 3 0
081900940506     C*
082000940506     C* Reperimento tasti di funzione
082100940127     C*
082200940117     C                   ENDSR
082300030113     C************************************************************
