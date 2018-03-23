000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200940224      *
000300040930      *  11           x selezione di un codice da ripassare al pgm chiamante
000400940307      *  21           GENERICO OPERAZIONI I/O
000500940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000600940224      *  30           SFLDSP
000700940224      * N31           SFLCLR
000800940224      *  31           SFLDSPCTL
000900940224      *  32           SFLNXTCHG
001000940224      *  33           SFLEND
001100940224      *  39           OF PRTF
001200940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001300940608      *  Specificare l'uso dei singoli indicatori
001400940224      *  50 <---> 98  ERRORI SU VIDEO
001500940608      *  Specificare l'uso dei singoli indicatori
001600940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001700940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
001800940224      *  99           INDIC. GENERALE DI ERRORE
001900940128     F*----------------------------------------------------*
002000090225     Fedstbl02L IF   E           K DISK
002100121224     Fedmap01l  IF   E           K DISK
002200090225     FTRTctb1D  CF   E             WORKSTN
002300940607     F                                     SFILE(S1:S1NRR)
002400090225     F                                     SFILE(S0:S0NRR)
002500940201     F                                     INFDS(DSFMT)
002600940128     D*----------------------------------------------------*
002700940211     D* Passaggio Parametri
002800940211     D KPJBA         E DS
002900030113      *-------------
003000940325     D* Parametri in ricezione
003100030113     D  TABDS          DS
003200030113     D  XTAOPZ                 1      2
003300030113     D  XTARET                 3      3
003400030113     D  XTAOPR                 4      4
003500030113     D  XTAERR                 5      5
003600030113     D  XTAKEY                 6     13
003700940211     D*-------------
003800940211     D DSFMT           DS           512
003900940506     D  $TASTO               369    369
004000940211     D  NRG                  370    370
004100940211     D  NCL                  371    371
004200940211     D  SFLNRR               378    379B 0
004300940207     D*-------------
004400940207     D* Nome PGM a video
004500940207     D                 DS
004600940207     D  PROGR                  1     10
004700940207     D  ASTER1                 1      1    INZ('*')
004800940207     D  SIGLA                  2      9
004900940207     D  ASTER2                10     10    INZ('*')
005000940127     D*-------------
005100940127     D* Reperimento nome PGM
005200940127     D STATUS         SDS           333
005300940127     D  DSPGM            *PROC
005400030113     D*-------------
005500030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005600090225$003 D S0NRR           S                   Like(C1rcd)
005700090225$003 D S1NRR           S                   Like(C1rcd)
005800030113$003 D WSfl            S                   Like(C1nrr)
005900030113$003 D Wmax            S                   Like(C1rcd)
006000030113$003 D Wpag            S                   Like(C1rcd)
006100090225$003 D Winzs0          S                   Like($inzs0)
006200090225$003 D Winzs1          S                   Like($inzs1)
006300040930$003 D x1cod           S                   Like(s1cod)
006400090225$003 D x0cod           S                   Like(s0cod)
006500090225     D*-------------
006600121224$003 D segmento        S                   Like(etbsgm)
006700940207     D*-------------
006800940211     D* COSTANTI
006900940211     D*-------------
007000090225     D SFLPAG0         C                   CONST(11)
007100090225     D SFLPAG1         C                   CONST(06)
007200940314     D* dimensione della schiera $MS1
007300940506     D*
007400940506     D* Tasti di funzione
007500940506     D F01             C                   CONST(X'31')
007600940506     D F02             C                   CONST(X'32')
007700940506     D F03             C                   CONST(X'33')
007800940506     D F04             C                   CONST(X'34')
007900940506     D F05             C                   CONST(X'35')
008000940506     D F06             C                   CONST(X'36')
008100940506     D F07             C                   CONST(X'37')
008200940506     D F08             C                   CONST(X'38')
008300940506     D F09             C                   CONST(X'39')
008400940506     D F10             C                   CONST(X'3A')
008500940506     D F11             C                   CONST(X'3B')
008600940506     D F12             C                   CONST(X'3C')
008700940506     D F13             C                   CONST(X'B1')
008800940506     D F14             C                   CONST(X'B2')
008900940506     D F15             C                   CONST(X'B3')
009000940506     D F16             C                   CONST(X'B4')
009100940506     D F17             C                   CONST(X'B5')
009200940506     D F18             C                   CONST(X'B6')
009300940506     D F19             C                   CONST(X'B7')
009400940506     D F20             C                   CONST(X'B8')
009500940506     D F21             C                   CONST(X'B9')
009600940506     D F22             C                   CONST(X'BA')
009700940506     D F23             C                   CONST(X'BB')
009800940506     D F24             C                   CONST(X'BC')
009900940506     D ENTER           C                   CONST(X'F1')
010000940506     D ROLDWN          C                   CONST(X'F4')
010100940506     D ROLLUP          C                   CONST(X'F5')
010200940127     C*----------------------------------------------------*
010300940127     C*                MAIN LINE PROGRAM
010400940127     C*----------------------------------------------------*
010500940223     C* inizializzazione variabili
010600940223     C                   EXSR      INZVAR
010700940223     C*
010800940223     C     $FINE         DOWEQ     *OFF
010900090225     C     $GEST         CASEQ     'S0'          GESS0
011000940131     C     $GEST         CASEQ     'S1'          GESS1
011100940117     C                   END
011200940117     C                   END
011300940325     C* fine programma
011400940325     C                   SETON                                            LR
011500030113     C************************************************************
011600030113     C* INIZIALIZZAZIONE VARIABILI
011700030113     C************************************************************
011800030113     C     INZVAR        BEGSR
011900030113     C*
012000030113     C* Pulizia campi e indicatori
012100030113     C                   MOVE      *ALL'0'       IN4049           10
012200030113     C                   MOVEA     IN4049        *IN(40)
012300030113     C                   CLEAR                   S1OPZ
012400090225     C                   CLEAR                   S0OPZ
012500030113     C*
012600090225     C* Variabili per gestione videate
012700030113     C                   MOVE      *OFF          $FINE
012800030113     C                   MOVE      *OFF          $EFILE
012900030113     C                   MOVE      *OFF          $ESCI
013000030113     C                   MOVE      *OFF          $RCDOK
013100030113     C                   Z-ADD     0             $ULKS1            3 0
013200030113     C*
013300030113     C                   MOVE      *ON           $INZS1
013400090225     C                   MOVE      *ON           $INZS0
013500090225     C                   MOVE      'S0'          $GEST
013600030113     C*
013700030113     C* Variabili appoggio
013800030114     C                   Z-ADD     1             WPAG
013900030113     c*
014000030113     C                   ENDSR
014100090225     C************************************************************
014200090225     C* GESTIONE LISTA
014300090225     C************************************************************
014400090225     C     GESS0         BEGSR
014500090225     C*
014600090225     C* inizializzazione videata
014700090225     C     $INZS0        IFEQ      *ON
014800090225     C                   EXSR      INZS0
014900090225     C                   MOVE      *OFF          $INZS0
015000090225     C                   ENDIF
015100090225     C*
015200090225     C* emissione piede videata
015300090225     C                   WRITE     Z0
015400090225     C* Non ci sono records
015500090225     C     WMAX          IFEQ      0
015600090225     C                   WRITE     D0
015700090225     C                   Else
015800090225     C     Wsfl          IFgt      0
015900090225     C                   Z-ADD     wsfl          C0RCD
016000090225     C                   Else
016100090225     C     Wpag          IFgt      0
016200090225     C                   Z-ADD     wpag          C0RCD
016300090225     C                   EndIF
016400090225     C                   EndIF
016500090225     C                   ENDIF
016600090225     C*
016700090225     C*              *------------------*
016800090225     C                   EXFMT     C0
016900090225     C*              *------------------*
017000090225     C*
017100090225     C     C0NRR         IFNE      0
017200090225     C                   Z-ADD     C0NRR         WSFL
017300090225     C                   ENDIF
017400090225     C                   Z-ADD     SFLNRR        C0RCD
017500090225     C* Selezioni
0176000902251    C                   SELECT
017700090225     C* F3=Fine
017800090225     C     $TASTO        WHENEQ    F03
017900090225     C                   EXSR      F03S1
0180000902251O   C                   OTHER
018100090225     C* CONTROLLO DATI
018200090225     C                   EXSR      CTRC0
018300090225     C     *IN99         IFEQ      *OFF
018400090225     C                   EXSR      CTRS0
018500090225     C                   END
0186000902251-   C                   ENDSL
018700090225     C*
018800090225     C                   ENDSR
018900090225     C/EJECT
019000090225     C************************************************************
019100090225     C* INIZIALIZZAZIONE LISTA
019200090225     C************************************************************
019300090225     C     INZS0         BEGSR
019400090225     C* pulizia SFL
019500090225     C                   SETOFF                                         3031
019600090225     C                   WRITE     C0
019700090225     C                   SETON                                          31
019800090225     C*
019900090225     C* CARICAMENTO SFL totale
020000090225     C                   Z-ADD     0             S0NRR
020100090225     C                   Z-ADD     1             C0RCD
020200090225     C                   Z-ADD     0             WMAX
020300090225     C                   Z-ADD     0             wsfl
020400090225     C*
020500090225     C*  Posizionamento all'inizio
020600121224     c     *Loval        setll     edMAP01l
020700090225    >C                   EXSR      REDSEGM
020800090225     C* Carico il SFL
020900090225     C                   EXSR      ROLS0
021000090225     C*
021100090225     c                   if        xtaopr <> '1'
021200090225     C                   Z-ADD     1             WPAG
021300090225     c                   end
021400090225     C*
021500090225     C                   ENDSR
021600090225     C************************************************************
021700090225     C* CARICAMENTO PAGINA LISTA
021800090225     C************************************************************
021900090225     C     ROLS0         BEGSR
022000090225     C*
022100090225     C                   SETOFF                                       32
022200090225     C                   Z-ADD     0             Y
022300090225     C                   Z-ADD     WMAX          S0NRR
022400090225     C*
022500090225     C* Leggo dal file i segmenti presenti nell'archivio
0226000902251    C     $EFILE        DOWEQ     *OFF
022700090225     C*
022800090225     c                   clear                   s0opz
022900090225     c*
023000090225     c* Prendo tipo segmento generale
023100121224     c                   if        mapSEQUEN = 0 and mapPROGR = 0
023200121224     c                   eval      s0cod =  mapCODICE
023300121224     c                   eval      s0des =  mapDESCR
023400090225     C                   ADD       1             S0NRR
023500090225     C                   ADD       1             Y
023600090225     C                   WRITE     S0
023700090225     c                   end
023800090225     C*
023900090225    >C                   EXSR      REDsegm
024000090225     C*
0241000902251-   C                   ENDDO
024200090225     C*
024300090225     C                   Z-ADD     S0NRR         WMAX                 30
024400090225     C*
024500090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
024600090225     C*
024700090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
024800090225     C                   MVR                     RESTO             3 0
024900090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0250000902251    C     RESTO         IFGT      0
025100090225     C                   ADD       1             C0RCD
0252000902251E   C                   ELSE
025300090225     C                   SUB       SFLPAG0       C0RCD
025400090225     C                   ADD       1             C0RCD
0255000902251-   C                   ENDIF
025600090225     C*
025700090225     C                   ENDSR
025800090225     C************************************************************
025900090225     C* LETTURA RCD
026000090225     C************************************************************
026100090225     C     REDsegm       BEGSR
026200090225     C*
026300090225     C                   MOVEL     *OFF          $EFILE
026400090225     C                   MOVEL     *OFF          $RCDOK
026500090225     C*
0266000902251    C     $EFILE        DOUEQ     *ON
026700090225     C     $RCDOK        OREQ      *ON
026800090225     C*
026900121224     c                   Read      edmap01L
027000121224     c                   if        %eof(edmap01L)
027100090225     C                   MOVEL     *ON           $EFILE
027200090225     C                   MOVE      $EFILE        *IN33
027300090225     c                   else
027400090225     C                   MOVE      *ON           $RCDOK
027500090225     c                   end
027600090225     C*
0277000902251-   C                   ENDDO
027800090225     C*
027900090225     C                   ENDSR
028000090225     C************************************************************
028100090225     C* CONTROLLO TESTATA LISTA
028200090225     C************************************************************
028300090225     C     CTRC0         BEGSR
028400090225     C*
028500090225     C                   MOVE      *OFF          *IN99
028600090225     C*
028700090225     C                   ENDSR
028800090225     C************************************************************
028900090225     C* CONTROLLO OPZIONI LISTA
029000090225     C************************************************************
029100090225     C     CTRS0         BEGSR
029200090225     C*
029300090225     C                   MOVEL     *OFF          $ESCI
029400090225     C                   SETOFF                                       99
029500090225     C                   clear                   S0OPZ
029600090225     c                   clear                   x0cod
029700090225     C*
029800090225     C* Leggo il sfl solo se ci sono rcd
0299000902251    C     WMAX          IFGT      0
030000090225     C                   READC     S0                                     21
030100090225     C*
030200090225     C* esce se fine sfl o errore che richiede l'uscita
0303000902252    C     *IN21         DOWEQ     *OFF
030400090225     C     $ESCI         ANDEQ     *OFF
030500090225     C                   Z-ADD     S0NRR         C0RCD
030600090225     C* ctrl su riga
030700090225     C                   EXSR      RECS0
030800090225     C* gestione opzioni
0309000902253    C     S0OPZ         IFNE      *blank
031000090225     C     *IN99         ANDEQ     *OFF
031100090225     C                   EXSR      OPZS0
0312000902253-   C                   ENDIF
031300090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0314000902253    C     *IN99         IFEQ      *ON
031500090225     C     $ESCI         OREQ      *ON
031600090225     C                   MOVE      *ON           *IN32
031700090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
031800090225     C* la ripristinerò a conclusione del ciclo di READC
031900090225     C                   MOVE      *OFF          $INZS0
0320000902253-   C                   ENDIF
032100090225     C*
032200090225     C                   clear                   S0OPZ
032300090225     C*
032400090225     C                   UPDATE    S0
032500090225     C*
032600090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0327000902253    C     $ESCI         IFEQ      *OFF
032800090225     C                   READC     S0                                     21
032900090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0330000902254    C     *IN21         IFEQ      *ON
033100090225     C                   MOVE      WINZS0        $INZS0
033200090225     C* calcolo pagina a cui deve posizionarsi
033300090225     C                   EXSR      CLCPAG0
0334000902254-   C                   ENDIF
0335000902253-   C                   ENDIF
033600090225     C*
0337000902252-   C                   ENDDO
033800090225     C*
0339000902251-   C                   ENDIF
034000090225     C*
034100090225     C                   ENDSR
034200090225     C/EJECT
034300090225     C************************************************************
034400090225     C* CONTROLLO CAMPI I/O RIGA LISTA
034500090225     C************************************************************
034600090225     C     RECS0         BEGSR
034700090225     C*
034800090225     C* reset indicatori DSPATR
034900090225     C                   MOVE      *ALL'0'       IN4049           10
035000090225     C                   MOVEA     IN4049        *IN(40)
035100090225     C*
035200090225     C                   ENDSR
035300090225     C************************************************************
035400090225     C* GESTIONE OPZIONI LISTA
035500090225     C************************************************************
035600090225     C     OPZS0         BEGSR
035700090225     C*
035800090225      * selezionato un codice da restituire al chiamante
0359000902253    C     S0OPZ         IFeq      '1'
036000090225     c                   move      s0cod         segmento
036100090225     C                   MOVE      'S1'          $GEST
036200090225     C                   MOVEL     *ON           $ESCI
036300090225     C                   END
036400090225     C*
036500090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0366000902252    C     *IN99         IFEQ      *ON
036700090225     C                   SETON                                        40
0368000902252-   C                   ENDIF
036900090225     C*
037000090225     C                   ENDSR
037100090225     C/EJECT
037200940127     C************************************************************
037300940131     C* GESTIONE LISTA
037400940127     C************************************************************
037500940127     C     GESS1         BEGSR
037600030113     C*
037700940223     C* inizializzazione videata
037800940223     C     $INZS1        IFEQ      *ON
037900940127     C                   EXSR      INZS1
038000940223     C                   MOVE      *OFF          $INZS1
038100940127     C                   ENDIF
038200030113     C*
038300030113     C* emissione piede videata
038400030113     C                   WRITE     Z1
038500030113     C* Non ci sono records
038600940223     C     WMAX          IFEQ      0
038700940607     C                   WRITE     D1
038800030114     C                   Else
038900030114     C     Wsfl          IFgt      0
039000030114     C                   Z-ADD     wsfl          C1RCD
039100030114     C                   Else
039200030114     C     Wpag          IFgt      0
039300030114     C                   Z-ADD     wpag          C1RCD
039400030114     C                   EndIF
039500030114     C                   EndIF
039600030114     C                   ENDIF
039700940127     C*
039800030113     C*              *------------------*
039900940607     C                   EXFMT     C1
040000030113     C*              *------------------*
040100030113     C*
040200940204     C     C1NRR         IFNE      0
040300940204     C                   Z-ADD     C1NRR         WSFL
040400940204     C                   ENDIF
040500940127     C                   Z-ADD     SFLNRR        C1RCD
040600030113     C* Selezioni
0407009401271    C                   SELECT
040800940127     C* F3=Fine
040900940506     C     $TASTO        WHENEQ    F03
041000940309     C                   EXSR      F03S1
041100940131     C* F10=Immissione
041200940506     C     $TASTO        WHENEQ    F10
041300940309     C                   EXSR      F10S1
041400090225     C* F12=Ritorno
041500090225     C     $TASTO        WHENEQ    F12
041600090225     C                   eval      $Gest='S0'
041700090225     C                   MOVE      *ON           $INZS1
041800090225     C                   MOVE      *ON           $INZS0
0419009401271O   C                   OTHER
042000940127     C* CONTROLLO DATI
042100940131     C                   EXSR      CTRC1
042200940201     C     *IN99         IFEQ      *OFF
042300940131     C                   EXSR      CTRS1
042400940131     C                   END
0425009401271-   C                   ENDSL
042600940127     C*
042700940127     C                   ENDSR
042800940224     C/EJECT
042900940127     C************************************************************
043000940131     C* INIZIALIZZAZIONE LISTA
043100940127     C************************************************************
043200940127     C     INZS1         BEGSR
043300940302     C* pulizia SFL
043400940128     C                   SETOFF                                         3031
043500940607     C                   WRITE     C1
043600940128     C                   SETON                                          31
043700940128     C*
043800030113     C* CARICAMENTO SFL totale
043900940201     C                   Z-ADD     0             S1NRR
044000030113     C                   Z-ADD     1             C1RCD
044100940128     C                   Z-ADD     0             WMAX
044200090225     C                   Z-ADD     0             wsfl
044300090225     C                   move      segmento      c1SEG
044400940224     C*
044500940224     C* Posizionamento su file pilota
044600090225     c     kTBL          setll     edstbl02L
044700940608    >C                   EXSR      REDANA
044800030113     C* Carico il SFL
044900940127     C                   EXSR      ROLS1
045000030113     C*
045100030114     c                   if        xtaopr <> '1'
045200030114     C                   Z-ADD     1             WPAG
045300030114     c                   end
045400940127     C*
045500940127     C                   ENDSR
045600940127     C************************************************************
045700940131     C* CARICAMENTO PAGINA LISTA
045800940127     C************************************************************
045900940127     C     ROLS1         BEGSR
046000940127     C*
046100940128     C                   SETOFF                                       32
046200940223     C                   Z-ADD     0             Y
046300940127     C                   Z-ADD     WMAX          S1NRR
046400940127     C*
046500940127     C* Leggo dal file anagrafico per caricare la lista
0466009401311    C     $EFILE        DOWEQ     *OFF
046700940127     C*
046800030113     c                   clear                   s1opz
046900121224     c                   eval      s1cod = etbvalsgm
047000121224     c                   eval      h1cod = etbvalsgm
047100090225     c                   eval      s1unb = etbunb
047200090225     c                   eval      h1unb = etbunb
047300090225     c                   movel     etbdescri     s1des
047400100715     c                   move(p)   etbdescri     campo10          10
047500100715     c                   move      campo10       s1cod
047600090225     c                   movel     etbdati       s1dati
047700940127     C*
047800940127     C                   ADD       1             S1NRR
047900940127     C                   ADD       1             Y
048000940127     C*
048100940607     C                   WRITE     S1
048200940131     C*
048300940316    >C                   EXSR      REDANA
048400940128     C*
0485009401271-   C                   ENDDO
048600940127     C*
048700940223     C                   Z-ADD     S1NRR         WMAX                 30
048800940127     C*
048900940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
049000940127     C*
049100090225     C     S1NRR         DIV       SFLPAG1       PAGINE            4 0
049200940127     C                   MVR                     RESTO             3 0
049300090225     C     PAGINE        MULT      SFLPAG1       C1RCD
0494000301141    C     RESTO         IFGT      0
049500030114     C                   ADD       1             C1RCD
0496000301141E   C                   ELSE
049700090225     C                   SUB       SFLPAG1       C1RCD
049800030114     C                   ADD       1             C1RCD
0499000301141-   C                   ENDIF
050000940128     C*
050100940127     C                   ENDSR
050200940128     C************************************************************
050300940131     C* LETTURA RCD ARCHIVIO PILOTA
050400940128     C************************************************************
050500940607     C     REDANA        BEGSR
050600940128     C*
050700940131     C                   MOVEL     *OFF          $EFILE
050800940131     C                   MOVEL     *OFF          $RCDOK
050900940131     C*
0510009401311    C     $EFILE        DOUEQ     *ON
051100940131     C     $RCDOK        OREQ      *ON
051200940131     C*
051300090225     c     ktbl          Reade     edstbl02L
051400090225     c                   if        %eof(edstbl02L)
051500030113     C                   MOVEL     *ON           $EFILE
051600030113     C                   MOVE      $EFILE        *IN33
051700030113     c                   else
051800030113     C                   MOVE      *ON           $RCDOK
051900030113     c                   end
052000940131     C*
0521009401311-   C                   ENDDO
052200940131     C*
052300940131     C                   ENDSR
052400940224     C************************************************************
052500940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
052600940224     C************************************************************
052700090225     C     CLCPAG0       BEGSR
052800940224     C* Input :
052900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
053000940224     C*          cursore
053100940224     C* - SFLPAG = numero rcd per pagina sfl
053200940224     C* Output :
053300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
053400940224     C*
053500090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
053600940224     C                   MVR                     RESTO             3 0
053700940224     C     RESTO         IFGT      0
053800940224     C                   ADD       1             PAGINE
053900940224     C                   ENDIF
054000090226     c                   if        pagine > 1
054100090225     C     PAGINE        MULT      SFLPAG0       WPAG
054200090226     C                   END
054300940224     C*
054400940224     C                   ENDSR
054500090225     C************************************************************
054600090225     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
054700090225     C************************************************************
054800090225     C     CLCPAG1       BEGSR
054900090225     C* Input :
055000090225     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
055100090225     C*          cursore
055200090225     C* - SFLPAG = numero rcd per pagina sfl
055300090225     C* Output :
055400090225     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
055500090225     C*
055600090225     C     WSFL          DIV       SFLPAG1       PAGINE            4 0
055700090225     C                   MVR                     RESTO             3 0
055800090225     C     RESTO         IFGT      0
055900090225     C                   ADD       1             PAGINE
056000090225     C                   ENDIF
056100090226     c                   if        pagine > 1
056200090225     C     PAGINE        MULT      SFLPAG1       WPAG
056300090226     C                   ENDIF
056400090225     C*
056500090225     C                   ENDSR
056600940309     C************************************************************
056700940309     C* GESTIONE F03 VIDEO S1
056800940309     C************************************************************
056900940309     C     F03S1         BEGSR
057000940309     C*
057100940309     C                   MOVE      *ON           $FINE
057200940325     C* fine programma
057300940309     C                   ENDSR
057400940309     C/EJECT
057500940309     C************************************************************
057600940309     C* GESTIONE F10 VIDEO S1
057700940314     c* AGGIUNTA RECORD
057800940309     C************************************************************
057900940309     C     F10S1         BEGSR
058000940309     C*
058100030113     C                   RESET                   tabds
058200030113     C                   MOVEL     '01'          xtaopz
058300030113     C                   MOVE      *ZERO         xtaret
058400030113     C                   MOVE      *ZERO         xtaopr
058500030114     C                   MOVEl     *blank        xtakey
058600030113     C                   MOVE      *BLANKS       KPJBU
058700030113     C                   MOVEL     tabds         KPJBU
058800090225     C                   move      c1SEG         KeySegmento       3
058900090225     C                   clear                   KeyTabella       35
059000090225     C                   clear                   KeyCliente       35
059100090225$004 C                   CALL      'TRTCTB1R2'
059200030113     C                   PARM                    KPJBA
059300090225     C                   PARM                    KeySegmento       3
059400090225     C                   PARM                    KeyTabella       35
059500090225     C                   PARM                    KeyCliente       35
059600090225      *
059700030114     C                   MOVEL     KPJBU         tabds
059800030113      *
059900940309     C* ritorno da PGM gestione
060000940309     C                   EXSR      GESRET
060100090226     C                   MOVE      'S1'          $GEST
060200090226      *
060300940309     C     WINZS1        IFEQ      *ON
060400940309     C                   MOVE      *ON           $INZS1
060500090226     C* carico sempre la 1a pagina
060600090226     C                   Z-ADD     1             WPAG
060700940309     C                   ENDIF
060800940309     C*
060900940309     C                   ENDSR
061000940128     C************************************************************
061100940131     C* CONTROLLO TESTATA LISTA
061200940128     C************************************************************
061300940131     C     CTRC1         BEGSR
061400940128     C*
061500940201     C                   MOVE      *OFF          *IN99
061600940131     C*
061700940202     C                   ENDSR
061800940131     C************************************************************
061900940131     C* CONTROLLO OPZIONI LISTA
062000940131     C************************************************************
062100940131     C     CTRS1         BEGSR
062200940131     C*
062300940202     C                   MOVEL     *OFF          $ESCI
062400940201     C                   SETOFF                                       99
062500940131     C                   Z-ADD     0             S1OPZ
062600040930     c                   clear                   x1cod
062700940131     C*
062800940127     C* Leggo il sfl solo se ci sono rcd
0629009401311    C     WMAX          IFGT      0
063000940607     C                   READC     S1                                     21
063100940127     C*
063200940131     C* esce se fine sfl o errore che richiede l'uscita
0633009401312    C     *IN21         DOWEQ     *OFF
063400940131     C     $ESCI         ANDEQ     *OFF
063500940201     C                   Z-ADD     S1NRR         C1RCD
063600940131     C* ctrl su riga
063700940131     C                   EXSR      RECS1
063800940131     C* gestione opzioni
0639009401313    C     S1OPZ         IFNE      0
064000940201     C     *IN99         ANDEQ     *OFF
064100940131     C                   EXSR      OPZS1
0642009401273-   C                   ENDIF
064300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0644009402013    C     *IN99         IFEQ      *ON
064500940201     C     $ESCI         OREQ      *ON
064600940131     C                   MOVE      *ON           *IN32
064700940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
064800940204     C* la ripristinerò a conclusione del ciclo di READC
064900940223     C                   MOVE      *OFF          $INZS1
0650009402233-   C                   ENDIF
065100940223     C*
065200940131     C                   Z-ADD     0             S1OPZ
065300940223     C*
065400940607     C                   UPDATE    S1
065500940223     C*
065600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0657009401313    C     $ESCI         IFEQ      *OFF
065800940607     C                   READC     S1                                     21
065900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0660009402014    C     *IN21         IFEQ      *ON
066100940201     C                   MOVE      WINZS1        $INZS1
066200940204     C* calcolo pagina a cui deve posizionarsi
066300090225     C                   EXSR      CLCPAG1
0664009402014-   C                   ENDIF
0665009402013-   C                   ENDIF
066600940131     C*
0667009401272-   C                   ENDDO
066800940127     C*
0669009401311-   C                   ENDIF
067000940131     C*
067100940127     C                   ENDSR
067200940127     C/EJECT
067300940127     C************************************************************
067400940131     C* CONTROLLO CAMPI I/O RIGA LISTA
067500940127     C************************************************************
067600940131     C     RECS1         BEGSR
067700940131     C*
067800940201     C* reset indicatori DSPATR
067900940201     C                   MOVE      *ALL'0'       IN4049           10
068000940201     C                   MOVEA     IN4049        *IN(40)
068100940201     C*
068200040930     C*  se richiamato x selezionare un codice 11 = *on
068300040930     C*   non deve utilizzare le opzioni di modifica
068400040930     C*  e viceversa
068500040930     c     *in11         ifeq      *on
0686000409303    C     S1OPZ         IFeq      2
0687000409303    C     S1OPZ         OReq      3
0688000409303    C     S1OPZ         OReq      4
068900040930     c                   seton                                        41  99
069000040930     C                   END
069100040930      * selezionato un codice da restituire al chiamante
0692000409303    C     S1OPZ         IFeq      1
069300100715     c                   move      h1cod         x1cod
069400040930     c                   clear                   kpjbu
069500040930     c                   movel     x1cod         kpjbu
069600040930     C                   MOVEL     *ON           $ESCI
069700040930     C                   MOVEL     *ON           $fine
069800040930     C                   END
069900040930     C                   END
070000040930     C*
070100040930     c     *in11         ifeq      *off
0702000409303    C     S1OPZ         ANDeq     1
070300040930     c                   seton                                        41  99
070400040930     C                   END
070500040930     C*
070600940131     C                   ENDSR
070700940131     C************************************************************
070800940131     C* GESTIONE OPZIONI LISTA
070900940131     C************************************************************
071000940131     C     OPZS1         BEGSR
071100940201     C*
0712000409302    C     *IN11         IFEQ      *Off
0713000409302    C     *IN11         oreq      *On
0714000409302    C     s1opz         andeq     05
071500040930     C*
071600030113     C                   RESET                   tabds
071700030113     C                   MOVEL     S1OPZ         xtaopz
071800030113     C                   MOVE      *ZERO         xtaret
071900030113     C                   MOVE      *ZERO         xtaopr
072000100715     C                   MOVEl     h1COD         xtakey
072100940715     C                   MOVE      *BLANKS       KPJBU
072200030113     C                   MOVEL     tabds         KPJBU
072300090225     C                   move      c1SEG         KeySegmento       3
072400100715     C                   movel     h1cod         KeyTabella       35
072500090225     C                   movel     h1UNB         KeyCliente       35
072600090225$004 C                   CALL      'TRTCTB1R2'
072700940607     C                   PARM                    KPJBA
072800090225     C                   PARM                    KeySegmento       3
072900090225     C                   PARM                    KeyTabella       35
073000090225     C                   PARM                    KeyCliente       35
073100090225      *
073200030114     C                   MOVEL     KPJBU         tabds
073300940201     C*
073400940223     C* ritorno da PGM gestione
073500940223     C                   EXSR      GESRET
073600040930     C*
073700940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0738009402252    C     *IN99         IFEQ      *ON
073900940223     C                   SETON                                        40
0740009402252-   C                   ENDIF
074100940225     C*
0742000409302-   C                   ENDIF
074300040930     C*
074400940131     C                   ENDSR
074500940223     C/EJECT
074600940223     C************************************************************
074700940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
074800940223     C************************************************************
074900940223     C     GESRET        BEGSR
075000940223     C*
075100940223     C* modo di ritorno
075200940223     C*
0753009402231    C                   SELECT
075400940314    >C* << questi modi di utilizzo dei valori di ritorno dal
075500940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
075600940314    >C*    proposte, normalmente sempre valide, ma modificabili
075700940314    >C*    per situazioni particolari >>
075800940223     C* 1 = F3
075900030113    >C     xtaret        WHENEQ    '1'
076000940224     C                   MOVE      *ON           $ESCI
076100940223     C                   MOVE      *ON           $FINE
076200940223     C* 2 = F12
076300030113    >C     xtaret        WHENEQ    '2'
076400940223     C                   MOVE      *ON           $ESCI
076500940223     C*
0766009402231-   C                   ENDSL
076700940223     C*
076800940223     C* operazione eseguite dal pgm chiamato
076900940223     C*
0770009402231    C                   SELECT
077100940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
077200030113    >C     xtaopr        WHENEQ    '1'
077300940223     C                   MOVE      *ON           WINZS1
077400940223     C*
0775009402231-   C                   ENDSL
077600940223     C*
077700940223     C* funzione non eseguibile per errore :
077800940223     C*
0779009402231    C                   SELECT
078000940223     C* 1 = funzione richiamata chiusa in errore
078100940316    >C*  eventualmente gestire altri codici di errore
078200030113    >C     xtaerr        WHENEQ    '1'
078300940223     C                   MOVE      *ON           $ESCI
078400940223     C                   SETON                                        5299
078500940223     C*
0786009402231-   C                   ENDSL
078700940223     C*
078800940223     C                   ENDSR
078900940223     C/EJECT
079000940131     C************************************************************
079100940131     C* OPERAZIONI INIZIALI
079200940131     C************************************************************
079300940131     C     *INZSR        BEGSR
079400030113     C*
079500030113     C* Reperimento parametri
079600030113     C     *ENTRY        PLIST
079700030113     C                   PARM                    KPJBA
079800040930     C                   movel     kpjbu         Selez             1
079900040930     C*
080000040930     C* Se chiamato x selezionare un codice
080100040930     c                   clear                   x1cod
080200040930     c                   if        selez = '?'
080300040930     c                   seton                                        11
080400040930     c                   end
080500030113     C*
080600030113     C* Variabili per gestione videate
080700030113     C                   MOVE      *BLANK        $GEST             2
080800030113     C                   MOVE      *BLANK        $FINE             1
080900030113     C                   MOVE      *BLANK        $INZS1            1
081000090225     C                   MOVE      *BLANK        $INZS0            1
081100030113     C                   MOVE      *BLANK        $EFILE            1
081200030113     C                   MOVE      *BLANK        $ESCI             1
081300030113     C                   MOVE      *BLANK        $RCDOK            1
081400030113     C*
081500030113     C     KTBL          KLIST
081600090225     C                   KFLD                    SEGMENTO          3
081700030113     C* Indici
081800030113     C                   Z-ADD     0             X                 3 0
081900030113     C                   Z-ADD     0             Y                 3 0
082000940506     C*
082100940506     C* Reperimento tasti di funzione
082200940127     C*
082300940117     C                   ENDSR
082400030113     C************************************************************
