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
002200110901     Fantab01L  IF   E           K DISK
002300110901     FANT156D   CF   E             WORKSTN
002400940607     F                                     SFILE(S1:S1NRR)
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
003600110902     D  XTAKEY                 6     10
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
005400940607     D* descrizione tabella
005500110902$003 D EXTDS         E DS                  EXTNAME(antab156)
005600030113     D*-------------
005700030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005800030113$003 D S1NRR           S                   Like(C1rcd)
005900030113$003 D WSfl            S                   Like(C1nrr)
006000030113$003 D Wmax            S                   Like(C1rcd)
006100030113$003 D Wpag            S                   Like(C1rcd)
006200030113$003 D Winzs1          S                   Like($inzs1)
006300040930$003 D x1cod           S                   Like(s1cod)
006400940207     D*-------------
006500940211     D* COSTANTI
006600940211     D*-------------
006700030113     D SFLPAG          C                   CONST(11)
006800940314     D* dimensione della schiera $MS1
006900940506     D*
007000940506     D* Tasti di funzione
007100940506     D F01             C                   CONST(X'31')
007200940506     D F02             C                   CONST(X'32')
007300940506     D F03             C                   CONST(X'33')
007400940506     D F04             C                   CONST(X'34')
007500940506     D F05             C                   CONST(X'35')
007600940506     D F06             C                   CONST(X'36')
007700940506     D F07             C                   CONST(X'37')
007800940506     D F08             C                   CONST(X'38')
007900940506     D F09             C                   CONST(X'39')
008000940506     D F10             C                   CONST(X'3A')
008100940506     D F11             C                   CONST(X'3B')
008200940506     D F12             C                   CONST(X'3C')
008300940506     D F13             C                   CONST(X'B1')
008400940506     D F14             C                   CONST(X'B2')
008500940506     D F15             C                   CONST(X'B3')
008600940506     D F16             C                   CONST(X'B4')
008700940506     D F17             C                   CONST(X'B5')
008800940506     D F18             C                   CONST(X'B6')
008900940506     D F19             C                   CONST(X'B7')
009000940506     D F20             C                   CONST(X'B8')
009100940506     D F21             C                   CONST(X'B9')
009200940506     D F22             C                   CONST(X'BA')
009300940506     D F23             C                   CONST(X'BB')
009400940506     D F24             C                   CONST(X'BC')
009500940506     D ENTER           C                   CONST(X'F1')
009600940506     D ROLDWN          C                   CONST(X'F4')
009700940506     D ROLLUP          C                   CONST(X'F5')
009800940207     I*-------------
009900940607     IS1
010000110901$015 I              S1DES                       DES156
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
015900030114     C                   Z-ADD     wpag          C1RCD
016000030114     C                   EndIF
016100030114     C                   EndIF
016200030114     C                   ENDIF
016300940127     C*
016400030113     C*              *------------------*
016500940607     C                   EXFMT     C1
016600030113     C*              *------------------*
016700030113     C*
016800940204     C     C1NRR         IFNE      0
016900940204     C                   Z-ADD     C1NRR         WSFL
017000940204     C                   ENDIF
017100940127     C                   Z-ADD     SFLNRR        C1RCD
017200030113     C* Selezioni
0173009401271    C                   SELECT
017400940127     C* F3=Fine
017500940506     C     $TASTO        WHENEQ    F03
017600940309     C                   EXSR      F03S1
017700940131     C* F10=Immissione
017800940506     C     $TASTO        WHENEQ    F10
017900940309     C                   EXSR      F10S1
0180009401271O   C                   OTHER
018100940127     C* CONTROLLO DATI
018200940131     C                   EXSR      CTRC1
018300940201     C     *IN99         IFEQ      *OFF
018400940131     C                   EXSR      CTRS1
018500940131     C                   END
0186009401271-   C                   ENDSL
018700940127     C*
018800940127     C                   ENDSR
018900940224     C/EJECT
019000940127     C************************************************************
019100940131     C* INIZIALIZZAZIONE LISTA
019200940127     C************************************************************
019300940127     C     INZS1         BEGSR
019400940302     C* pulizia SFL
019500940128     C                   SETOFF                                         3031
019600940607     C                   WRITE     C1
019700940128     C                   SETON                                          31
019800940128     C*
019900030113     C* CARICAMENTO SFL totale
020000940201     C                   Z-ADD     0             S1NRR
020100030113     C                   Z-ADD     1             C1RCD
020200940128     C                   Z-ADD     0             WMAX
020300940224     C*
020400940224     C* Posizionamento su file pilota
020500110901     c     kTBL          setll     antab01l
020600940608    >C                   EXSR      REDANA
020700030113     C* Carico il SFL
020800940127     C                   EXSR      ROLS1
020900030113     C*
021000030114     c                   if        xtaopr <> '1'
021100030114     C                   Z-ADD     1             WPAG
021200030114     c                   end
021300940127     C*
021400940127     C                   ENDSR
021500940127     C************************************************************
021600940131     C* CARICAMENTO PAGINA LISTA
021700940127     C************************************************************
021800940127     C     ROLS1         BEGSR
021900940127     C*
022000940128     C                   SETOFF                                       32
022100940223     C                   Z-ADD     0             Y
022200940127     C                   Z-ADD     WMAX          S1NRR
022300940127     C*
022400940127     C* Leggo dal file anagrafico per caricare la lista
0225009401311    C     $EFILE        DOWEQ     *OFF
022600940127     C*
022700030113     c                   clear                   s1opz
022800110901     c                   move      *all'0'       s1cod
022900110901     c                   move      tabcod        s1cod
023000110901     c                   movel     tabdes        extds
023100030113     C*
023200030115     c                   clear                   S1Ann
023300940127     C*
023400940127     C                   ADD       1             S1NRR
023500940127     C                   ADD       1             Y
023600940127     C*
023700940607     C                   WRITE     S1
023800940131     C*
023900940316    >C                   EXSR      REDANA
024000940128     C*
0241009401271-   C                   ENDDO
024200940127     C*
024300940223     C                   Z-ADD     S1NRR         WMAX                 30
024400940127     C*
024500940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
024600940127     C*
024700030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
024800940127     C                   MVR                     RESTO             3 0
024900030114     C     PAGINE        MULT      SFLPAG        C1RCD
0250000301141    C     RESTO         IFGT      0
025100030114     C                   ADD       1             C1RCD
0252000301141E   C                   ELSE
025300030114     C                   SUB       SFLPAG        C1RCD
025400030114     C                   ADD       1             C1RCD
0255000301141-   C                   ENDIF
025600940128     C*
025700940127     C                   ENDSR
025800940128     C************************************************************
025900940131     C* LETTURA RCD ARCHIVIO PILOTA
026000940128     C************************************************************
026100940607     C     REDANA        BEGSR
026200940128     C*
026300940131     C                   MOVEL     *OFF          $EFILE
026400940131     C                   MOVEL     *OFF          $RCDOK
026500940131     C*
0266009401311    C     $EFILE        DOUEQ     *ON
026700940131     C     $RCDOK        OREQ      *ON
026800940131     C*
026900110901     c     kTBL          Reade     antab01l
027000110901     c                   if        %eof(antab01l)
027100030113     C                   MOVEL     *ON           $EFILE
027200030113     C                   MOVE      $EFILE        *IN33
027300030113     c                   else
027400030113     C                   MOVE      *ON           $RCDOK
027500030113     c                   end
027600940131     C*
0277009401311-   C                   ENDDO
027800940131     C*
027900940131     C                   ENDSR
028000940224     C************************************************************
028100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
028200940224     C************************************************************
028300940224     C     CLCPAG        BEGSR
028400940224     C* Input :
028500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
028600940224     C*          cursore
028700940224     C* - SFLPAG = numero rcd per pagina sfl
028800940224     C* Output :
028900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
029000940224     C*
029100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
029200940224     C                   MVR                     RESTO             3 0
029300940224     C     RESTO         IFGT      0
029400940224     C                   ADD       1             PAGINE
029500940224     C                   ENDIF
029600940224     C     PAGINE        MULT      SFLPAG        WPAG
029700940224     C*
029800940224     C                   ENDSR
029900940309     C************************************************************
030000940309     C* GESTIONE F03 VIDEO S1
030100940309     C************************************************************
030200940309     C     F03S1         BEGSR
030300940309     C*
030400940309     C                   MOVE      *ON           $FINE
030500940325     C* fine programma
030600940309     C                   ENDSR
030700940309     C/EJECT
030800940309     C************************************************************
030900940309     C* GESTIONE F10 VIDEO S1
031000940314     c* AGGIUNTA RECORD
031100940309     C************************************************************
031200940309     C     F10S1         BEGSR
031300940309     C*
031400030113     C                   RESET                   tabds
031500030113     C                   MOVEL     '01'          xtaopz
031600030113     C                   MOVE      *ZERO         xtaret
031700030113     C                   MOVE      *ZERO         xtaopr
031800030114     C                   MOVEl     *blank        xtakey
031900030113     C                   MOVE      *BLANKS       KPJBU
032000030113     C                   MOVEL     tabds         KPJBU
032100110901$004 C                   CALL      'ANT156R2'
032200030113     C                   PARM                    KPJBA
032300030114     C                   MOVEL     KPJBU         tabds
032400030113      *
032500940309     C* ritorno da PGM gestione
032600940309     C                   EXSR      GESRET
032700940309     C     WINZS1        IFEQ      *ON
032800940309     C                   MOVE      *ON           $INZS1
032900940309     C* se esistevano già righe sul sfl
033000940309     C* calcolo pagina a cui deve posizionarsi
033100940309     C     WSFL          IFGT      0
033200940309     C                   EXSR      CLCPAG
033300940309     C* altrimenti carico solo la 1a pagina
033400940309     C                   ELSE
033500940309     C                   Z-ADD     SFLPAG        WPAG
033600940309     C                   ENDIF
033700940309     C                   ENDIF
033800940309     C*
033900940309     C                   ENDSR
034000940128     C************************************************************
034100940131     C* CONTROLLO TESTATA LISTA
034200940128     C************************************************************
034300940131     C     CTRC1         BEGSR
034400940128     C*
034500940201     C                   MOVE      *OFF          *IN99
034600940131     C*
034700940202     C                   ENDSR
034800940131     C************************************************************
034900940131     C* CONTROLLO OPZIONI LISTA
035000940131     C************************************************************
035100940131     C     CTRS1         BEGSR
035200940131     C*
035300940202     C                   MOVEL     *OFF          $ESCI
035400940201     C                   SETOFF                                       99
035500940131     C                   Z-ADD     0             S1OPZ
035600040930     c                   clear                   x1cod
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
041200040930     C*  se richiamato x selezionare un codice 11 = *on
041300040930     C*   non deve utilizzare le opzioni di modifica
041400040930     C*  e viceversa
041500040930     c     *in11         ifeq      *on
0416000409303    C     S1OPZ         IFeq      2
0417000409303    C     S1OPZ         OReq      3
0418000409303    C     S1OPZ         OReq      4
041900040930     c                   seton                                        41  99
042000040930     C                   END
042100040930      * selezionato un codice da restituire al chiamante
0422000409303    C     S1OPZ         IFeq      1
042300110901     c                   move      *all'0'       x1cod
042400110901     c                   move      s1cod         x1cod
042500040930     c                   clear                   kpjbu
042600040930     c                   movel     x1cod         kpjbu
042700040930     C                   MOVEL     *ON           $ESCI
042800040930     C                   MOVEL     *ON           $fine
042900040930     C                   END
043000040930     C                   END
043100040930     C*
043200040930     c     *in11         ifeq      *off
0433000409303    C     S1OPZ         ANDeq     1
043400040930     c                   seton                                        41  99
043500040930     C                   END
043600040930     C*
043700940131     C                   ENDSR
043800940131     C************************************************************
043900940131     C* GESTIONE OPZIONI LISTA
044000940131     C************************************************************
044100940131     C     OPZS1         BEGSR
044200940201     C*
0443000409302    C     *IN11         IFEQ      *Off
0444000409302    C     *IN11         oreq      *On
0445000409302    C     s1opz         andeq     05
044600040930     C*
044700030113     C                   RESET                   tabds
044800110902     c                   MOVE      *all'0'       xtaopz
044900110902     C                   MOVE      S1OPZ         xtaopz
045000030113     C                   MOVE      *ZERO         xtaret
045100030113     C                   MOVE      *ZERO         xtaopr
045200110901     C                   MOVE      *all'0'       xtakey
045300110901     C                   MOVE      S1COD         xtakey
045400940715     C                   MOVE      *BLANKS       KPJBU
045500030113     C                   MOVEL     tabds         KPJBU
045600110901$004 C                   CALL      'ANT156R2'
045700940607     C                   PARM                    KPJBA
045800030114     C                   MOVEL     KPJBU         tabds
045900940201     C*
046000940223     C* ritorno da PGM gestione
046100940223     C                   EXSR      GESRET
046200040930     C*
046300940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0464009402252    C     *IN99         IFEQ      *ON
046500940223     C                   SETON                                        40
0466009402252-   C                   ENDIF
046700940225     C*
0468000409302-   C                   ENDIF
046900040930     C*
047000940131     C                   ENDSR
047100940223     C/EJECT
047200940223     C************************************************************
047300940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
047400940223     C************************************************************
047500940223     C     GESRET        BEGSR
047600940223     C*
047700940223     C* modo di ritorno
047800940223     C*
0479009402231    C                   SELECT
048000940314    >C* << questi modi di utilizzo dei valori di ritorno dal
048100940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
048200940314    >C*    proposte, normalmente sempre valide, ma modificabili
048300940314    >C*    per situazioni particolari >>
048400940223     C* 1 = F3
048500030113    >C     xtaret        WHENEQ    '1'
048600940224     C                   MOVE      *ON           $ESCI
048700940223     C                   MOVE      *ON           $FINE
048800940223     C* 2 = F12
048900030113    >C     xtaret        WHENEQ    '2'
049000940223     C                   MOVE      *ON           $ESCI
049100940223     C*
0492009402231-   C                   ENDSL
049300940223     C*
049400940223     C* operazione eseguite dal pgm chiamato
049500940223     C*
0496009402231    C                   SELECT
049700940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
049800030113    >C     xtaopr        WHENEQ    '1'
049900940223     C                   MOVE      *ON           WINZS1
050000940223     C*
0501009402231-   C                   ENDSL
050200940223     C*
050300940223     C* funzione non eseguibile per errore :
050400940223     C*
0505009402231    C                   SELECT
050600940223     C* 1 = funzione richiamata chiusa in errore
050700940316    >C*  eventualmente gestire altri codici di errore
050800030113    >C     xtaerr        WHENEQ    '1'
050900940223     C                   MOVE      *ON           $ESCI
051000940223     C                   SETON                                        5299
051100940223     C*
0512009402231-   C                   ENDSL
051300940223     C*
051400940223     C                   ENDSR
051500940223     C/EJECT
051600940131     C************************************************************
051700940131     C* OPERAZIONI INIZIALI
051800940131     C************************************************************
051900940131     C     *INZSR        BEGSR
052000030113     C*
052100030113     C* Reperimento parametri
052200030113     C     *ENTRY        PLIST
052300030113     C                   PARM                    KPJBA
052400040930     C                   movel     kpjbu         Selez             1
052500040930     C*
052600040930     C* Se chiamato x selezionare un codice
052700040930     c                   clear                   x1cod
052800040930     c                   if        selez = '?'
052900040930     c                   seton                                        11
053000040930     c                   end
053100030113     C*
053200030113     C* Variabili per gestione videate
053300030113     C                   MOVE      *BLANK        $GEST             2
053400030113     C                   MOVE      *BLANK        $FINE             1
053500030113     C                   MOVE      *BLANK        $INZS1            1
053600030113     C                   MOVE      *BLANK        $EFILE            1
053700030113     C                   MOVE      *BLANK        $ESCI             1
053800030113     C                   MOVE      *BLANK        $RCDOK            1
053900030113     C*
054000030113     C     KTBL          KLIST
054100110901     C                   KFLD                    Tabgru
054200030113     C* Indici
054300030113     C                   Z-ADD     0             X                 3 0
054400030113     C                   Z-ADD     0             Y                 3 0
054500940506     C*
054600110901     C                   MOVE      '156'         tabGRU
054700940127     C*
054800940117     C                   ENDSR
054900030113     C************************************************************
