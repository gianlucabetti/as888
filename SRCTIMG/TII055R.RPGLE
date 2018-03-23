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
002000110901     Fantab01L  IF   E           K DISK
002100110921     FTII055D   CF   E             WORKSTN
002200940607     F                                     SFILE(S1:S1NRR)
002300940201     F                                     INFDS(DSFMT)
002400940128     D*----------------------------------------------------*
002500940211     D* Passaggio Parametri
002600940211     D KPJBA         E DS
002700030113      *-------------
002800940325     D* Parametri in ricezione
002900030113     D  TABDS          DS
003000030113     D  XTAOPZ                 1      2
003100030113     D  XTARET                 3      3
003200030113     D  XTAOPR                 4      4
003300030113     D  XTAERR                 5      5
003400110921     D  XTAKEY                 6      6
003500940211     D*-------------
003600940211     D DSFMT           DS           512
003700940506     D  $TASTO               369    369
003800940211     D  NRG                  370    370
003900940211     D  NCL                  371    371
004000940211     D  SFLNRR               378    379B 0
004100940207     D*-------------
004200940207     D* Nome PGM a video
004300940207     D                 DS
004400940207     D  PROGR                  1     10
004500940207     D  ASTER1                 1      1    INZ('*')
004600940207     D  SIGLA                  2      9
004700940207     D  ASTER2                10     10    INZ('*')
004800940127     D*-------------
004900940127     D* Reperimento nome PGM
005000940127     D STATUS         SDS           333
005100940127     D  DSPGM            *PROC
005200940607     D* descrizione tabella
005300110921$003 D EXTDS         E DS                  EXTNAME(ANTAB055)
005400030113     D*-------------
005500030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005600030113$003 D S1NRR           S                   Like(C1rcd)
005700030113$003 D WSfl            S                   Like(C1nrr)
005800030113$003 D Wmax            S                   Like(C1rcd)
005900030113$003 D Wpag            S                   Like(C1rcd)
006000030113$003 D Winzs1          S                   Like($inzs1)
006100040930$003 D x1cod           S                   Like(s1cod)
006200940207     D*-------------
006300940211     D* COSTANTI
006400940211     D*-------------
006500030113     D SFLPAG          C                   CONST(11)
006600940314     D* dimensione della schiera $MS1
006700940506     D*
006800940506     D* Tasti di funzione
006900940506     D F01             C                   CONST(X'31')
007000940506     D F02             C                   CONST(X'32')
007100940506     D F03             C                   CONST(X'33')
007200940506     D F04             C                   CONST(X'34')
007300940506     D F05             C                   CONST(X'35')
007400940506     D F06             C                   CONST(X'36')
007500940506     D F07             C                   CONST(X'37')
007600940506     D F08             C                   CONST(X'38')
007700940506     D F09             C                   CONST(X'39')
007800940506     D F10             C                   CONST(X'3A')
007900940506     D F11             C                   CONST(X'3B')
008000940506     D F12             C                   CONST(X'3C')
008100940506     D F13             C                   CONST(X'B1')
008200940506     D F14             C                   CONST(X'B2')
008300940506     D F15             C                   CONST(X'B3')
008400940506     D F16             C                   CONST(X'B4')
008500940506     D F17             C                   CONST(X'B5')
008600940506     D F18             C                   CONST(X'B6')
008700940506     D F19             C                   CONST(X'B7')
008800940506     D F20             C                   CONST(X'B8')
008900940506     D F21             C                   CONST(X'B9')
009000940506     D F22             C                   CONST(X'BA')
009100940506     D F23             C                   CONST(X'BB')
009200940506     D F24             C                   CONST(X'BC')
009300940506     D ENTER           C                   CONST(X'F1')
009400940506     D ROLDWN          C                   CONST(X'F4')
009500940506     D ROLLUP          C                   CONST(X'F5')
009600940207     I*-------------
009700940607     IS1
009800110921$015 I              S1DES                       DES055
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
015400030114     C                   Z-ADD     wsfl          C1RCD
015500030114     C                   Else
015600030114     C     Wpag          IFgt      0
015700030114     C                   Z-ADD     wpag          C1RCD
015800030114     C                   EndIF
015900030114     C                   EndIF
016000030114     C                   ENDIF
016100940127     C*
016200030113     C*              *------------------*
016300940607     C                   EXFMT     C1
016400030113     C*              *------------------*
016500030113     C*
016600940204     C     C1NRR         IFNE      0
016700940204     C                   Z-ADD     C1NRR         WSFL
016800940204     C                   ENDIF
016900940127     C                   Z-ADD     SFLNRR        C1RCD
017000030113     C* Selezioni
0171009401271    C                   SELECT
017200940127     C* F3=Fine
017300940506     C     $TASTO        WHENEQ    F03
017400940309     C                   EXSR      F03S1
017500940131     C* F10=Immissione
017600940506     C     $TASTO        WHENEQ    F10
017700940309     C                   EXSR      F10S1
0178009401271O   C                   OTHER
017900940127     C* CONTROLLO DATI
018000940131     C                   EXSR      CTRC1
018100940201     C     *IN99         IFEQ      *OFF
018200940131     C                   EXSR      CTRS1
018300940131     C                   END
0184009401271-   C                   ENDSL
018500940127     C*
018600940127     C                   ENDSR
018700940224     C/EJECT
018800940127     C************************************************************
018900940131     C* INIZIALIZZAZIONE LISTA
019000940127     C************************************************************
019100940127     C     INZS1         BEGSR
019200940302     C* pulizia SFL
019300940128     C                   SETOFF                                         3031
019400940607     C                   WRITE     C1
019500940128     C                   SETON                                          31
019600940128     C*
019700030113     C* CARICAMENTO SFL totale
019800940201     C                   Z-ADD     0             S1NRR
019900030113     C                   Z-ADD     1             C1RCD
020000940128     C                   Z-ADD     0             WMAX
020100940224     C*
020200940224     C* Posizionamento su file pilota
020300110901     c     kTBL          setll     antab01l
020400940608    >C                   EXSR      REDANA
020500030113     C* Carico il SFL
020600940127     C                   EXSR      ROLS1
020700030113     C*
020800030114     c                   if        xtaopr <> '1'
020900030114     C                   Z-ADD     1             WPAG
021000030114     c                   end
021100940127     C*
021200940127     C                   ENDSR
021300940127     C************************************************************
021400940131     C* CARICAMENTO PAGINA LISTA
021500940127     C************************************************************
021600940127     C     ROLS1         BEGSR
021700940127     C*
021800940128     C                   SETOFF                                       32
021900940223     C                   Z-ADD     0             Y
022000940127     C                   Z-ADD     WMAX          S1NRR
022100940127     C*
022200940127     C* Leggo dal file anagrafico per caricare la lista
0223009401311    C     $EFILE        DOWEQ     *OFF
022400940127     C*
022500030113     c                   clear                   s1opz
022600110901     c                   move      *all'0'       s1cod
022700110901     c                   move      tabcod        s1cod
022800110901     c                   movel     tabdes        extds
022900030113     C*
023000030115     c                   clear                   S1Ann
023100940127     C*
023200940127     C                   ADD       1             S1NRR
023300940127     C                   ADD       1             Y
023400940127     C*
023500940607     C                   WRITE     S1
023600940131     C*
023700940316    >C                   EXSR      REDANA
023800940128     C*
0239009401271-   C                   ENDDO
024000940127     C*
024100940223     C                   Z-ADD     S1NRR         WMAX                 30
024200940127     C*
024300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
024400940127     C*
024500030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
024600940127     C                   MVR                     RESTO             3 0
024700030114     C     PAGINE        MULT      SFLPAG        C1RCD
0248000301141    C     RESTO         IFGT      0
024900030114     C                   ADD       1             C1RCD
0250000301141E   C                   ELSE
025100030114     C                   SUB       SFLPAG        C1RCD
025200030114     C                   ADD       1             C1RCD
0253000301141-   C                   ENDIF
025400940128     C*
025500940127     C                   ENDSR
025600940128     C************************************************************
025700940131     C* LETTURA RCD ARCHIVIO PILOTA
025800940128     C************************************************************
025900940607     C     REDANA        BEGSR
026000940128     C*
026100940131     C                   MOVEL     *OFF          $EFILE
026200940131     C                   MOVEL     *OFF          $RCDOK
026300940131     C*
0264009401311    C     $EFILE        DOUEQ     *ON
026500940131     C     $RCDOK        OREQ      *ON
026600940131     C*
026700110901     c     kTBL          Reade     antab01l
026800110901     c                   if        %eof(antab01l)
026900030113     C                   MOVEL     *ON           $EFILE
027000030113     C                   MOVE      $EFILE        *IN33
027100030113     c                   else
027200030113     C                   MOVE      *ON           $RCDOK
027300030113     c                   end
027400940131     C*
0275009401311-   C                   ENDDO
027600940131     C*
027700940131     C                   ENDSR
027800940224     C************************************************************
027900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
028000940224     C************************************************************
028100940224     C     CLCPAG        BEGSR
028200940224     C* Input :
028300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
028400940224     C*          cursore
028500940224     C* - SFLPAG = numero rcd per pagina sfl
028600940224     C* Output :
028700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
028800940224     C*
028900940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
029000940224     C                   MVR                     RESTO             3 0
029100940224     C     RESTO         IFGT      0
029200940224     C                   ADD       1             PAGINE
029300940224     C                   ENDIF
029400940224     C     PAGINE        MULT      SFLPAG        WPAG
029500940224     C*
029600940224     C                   ENDSR
029700940309     C************************************************************
029800940309     C* GESTIONE F03 VIDEO S1
029900940309     C************************************************************
030000940309     C     F03S1         BEGSR
030100940309     C*
030200940309     C                   MOVE      *ON           $FINE
030300940325     C* fine programma
030400940309     C                   ENDSR
030500940309     C/EJECT
030600940309     C************************************************************
030700940309     C* GESTIONE F10 VIDEO S1
030800940314     c* AGGIUNTA RECORD
030900940309     C************************************************************
031000940309     C     F10S1         BEGSR
031100940309     C*
031200030113     C                   RESET                   tabds
031300030113     C                   MOVEL     '01'          xtaopz
031400030113     C                   MOVE      *ZERO         xtaret
031500030113     C                   MOVE      *ZERO         xtaopr
031600030114     C                   MOVEl     *blank        xtakey
031700030113     C                   MOVE      *BLANKS       KPJBU
031800030113     C                   MOVEL     tabds         KPJBU
031900110921$004 C                   CALL      'TII055R2'
032000030113     C                   PARM                    KPJBA
032100030114     C                   MOVEL     KPJBU         tabds
032200030113      *
032300940309     C* ritorno da PGM gestione
032400940309     C                   EXSR      GESRET
032500940309     C     WINZS1        IFEQ      *ON
032600940309     C                   MOVE      *ON           $INZS1
032700940309     C* se esistevano già righe sul sfl
032800940309     C* calcolo pagina a cui deve posizionarsi
032900940309     C     WSFL          IFGT      0
033000940309     C                   EXSR      CLCPAG
033100940309     C* altrimenti carico solo la 1a pagina
033200940309     C                   ELSE
033300940309     C                   Z-ADD     SFLPAG        WPAG
033400940309     C                   ENDIF
033500940309     C                   ENDIF
033600940309     C*
033700940309     C                   ENDSR
033800940128     C************************************************************
033900940131     C* CONTROLLO TESTATA LISTA
034000940128     C************************************************************
034100940131     C     CTRC1         BEGSR
034200940128     C*
034300940201     C                   MOVE      *OFF          *IN99
034400940131     C*
034500940202     C                   ENDSR
034600940131     C************************************************************
034700940131     C* CONTROLLO OPZIONI LISTA
034800940131     C************************************************************
034900940131     C     CTRS1         BEGSR
035000940131     C*
035100940202     C                   MOVEL     *OFF          $ESCI
035200940201     C                   SETOFF                                       99
035300940131     C                   Z-ADD     0             S1OPZ
035400040930     c                   clear                   x1cod
035500940131     C*
035600940127     C* Leggo il sfl solo se ci sono rcd
0357009401311    C     WMAX          IFGT      0
035800940607     C                   READC     S1                                     21
035900940127     C*
036000940131     C* esce se fine sfl o errore che richiede l'uscita
0361009401312    C     *IN21         DOWEQ     *OFF
036200940131     C     $ESCI         ANDEQ     *OFF
036300940201     C                   Z-ADD     S1NRR         C1RCD
036400940131     C* ctrl su riga
036500940131     C                   EXSR      RECS1
036600940131     C* gestione opzioni
0367009401313    C     S1OPZ         IFNE      0
036800940201     C     *IN99         ANDEQ     *OFF
036900940131     C                   EXSR      OPZS1
0370009401273-   C                   ENDIF
037100940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0372009402013    C     *IN99         IFEQ      *ON
037300940201     C     $ESCI         OREQ      *ON
037400940131     C                   MOVE      *ON           *IN32
037500940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
037600940204     C* la ripristinerò a conclusione del ciclo di READC
037700940223     C                   MOVE      *OFF          $INZS1
0378009402233-   C                   ENDIF
037900940223     C*
038000940131     C                   Z-ADD     0             S1OPZ
038100940223     C*
038200940607     C                   UPDATE    S1
038300940223     C*
038400940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0385009401313    C     $ESCI         IFEQ      *OFF
038600940607     C                   READC     S1                                     21
038700940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0388009402014    C     *IN21         IFEQ      *ON
038900940201     C                   MOVE      WINZS1        $INZS1
039000940204     C* calcolo pagina a cui deve posizionarsi
039100940224     C                   EXSR      CLCPAG
0392009402014-   C                   ENDIF
0393009402013-   C                   ENDIF
039400940131     C*
0395009401272-   C                   ENDDO
039600940127     C*
0397009401311-   C                   ENDIF
039800940131     C*
039900940127     C                   ENDSR
040000940127     C/EJECT
040100940127     C************************************************************
040200940131     C* CONTROLLO CAMPI I/O RIGA LISTA
040300940127     C************************************************************
040400940131     C     RECS1         BEGSR
040500940131     C*
040600940201     C* reset indicatori DSPATR
040700940201     C                   MOVE      *ALL'0'       IN4049           10
040800940201     C                   MOVEA     IN4049        *IN(40)
040900940201     C*
041000040930     C*  se richiamato x selezionare un codice 11 = *on
041100040930     C*   non deve utilizzare le opzioni di modifica
041200040930     C*  e viceversa
041300040930     c     *in11         ifeq      *on
0414000409303    C     S1OPZ         IFeq      2
0415000409303    C     S1OPZ         OReq      3
0416000409303    C     S1OPZ         OReq      4
041700040930     c                   seton                                        41  99
041800040930     C                   END
041900040930      * selezionato un codice da restituire al chiamante
0420000409303    C     S1OPZ         IFeq      1
042100040930     c                   move      s1cod         x1cod
042200040930     c                   clear                   kpjbu
042300040930     c                   movel     x1cod         kpjbu
042400040930     C                   MOVEL     *ON           $ESCI
042500040930     C                   MOVEL     *ON           $fine
042600040930     C                   END
042700040930     C                   END
042800040930     C*
042900040930     c     *in11         ifeq      *off
0430000409303    C     S1OPZ         ANDeq     1
043100040930     c                   seton                                        41  99
043200040930     C                   END
043300040930     C*
043400940131     C                   ENDSR
043500940131     C************************************************************
043600940131     C* GESTIONE OPZIONI LISTA
043700940131     C************************************************************
043800940131     C     OPZS1         BEGSR
043900940201     C*
0440000409302    C     *IN11         IFEQ      *Off
0441000409302    C     *IN11         oreq      *On
0442000409302    C     s1opz         andeq     05
044300040930     C*
044400030113     C                   RESET                   tabds
044500110902     C                   MOVE      *all'0'       xtaopz
044600110902     C                   MOVE      S1OPZ         xtaopz
044700030113     C                   MOVE      *ZERO         xtaret
044800030113     C                   MOVE      *ZERO         xtaopr
044900110902     C                   MOVEl     S1COD         xtakey
045000940715     C                   MOVE      *BLANKS       KPJBU
045100030113     C                   MOVEL     tabds         KPJBU
045200110921$004 C                   CALL      'TII055R2'
045300940607     C                   PARM                    KPJBA
045400030114     C                   MOVEL     KPJBU         tabds
045500940201     C*
045600940223     C* ritorno da PGM gestione
045700940223     C                   EXSR      GESRET
045800040930     C*
045900940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0460009402252    C     *IN99         IFEQ      *ON
046100940223     C                   SETON                                        40
0462009402252-   C                   ENDIF
046300940225     C*
0464000409302-   C                   ENDIF
046500040930     C*
046600940131     C                   ENDSR
046700940223     C/EJECT
046800940223     C************************************************************
046900940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
047000940223     C************************************************************
047100940223     C     GESRET        BEGSR
047200940223     C*
047300940223     C* modo di ritorno
047400940223     C*
0475009402231    C                   SELECT
047600940314    >C* << questi modi di utilizzo dei valori di ritorno dal
047700940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
047800940314    >C*    proposte, normalmente sempre valide, ma modificabili
047900940314    >C*    per situazioni particolari >>
048000940223     C* 1 = F3
048100030113    >C     xtaret        WHENEQ    '1'
048200940224     C                   MOVE      *ON           $ESCI
048300940223     C                   MOVE      *ON           $FINE
048400940223     C* 2 = F12
048500030113    >C     xtaret        WHENEQ    '2'
048600940223     C                   MOVE      *ON           $ESCI
048700940223     C*
0488009402231-   C                   ENDSL
048900940223     C*
049000940223     C* operazione eseguite dal pgm chiamato
049100940223     C*
0492009402231    C                   SELECT
049300940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
049400030113    >C     xtaopr        WHENEQ    '1'
049500940223     C                   MOVE      *ON           WINZS1
049600940223     C*
0497009402231-   C                   ENDSL
049800940223     C*
049900940223     C* funzione non eseguibile per errore :
050000940223     C*
0501009402231    C                   SELECT
050200940223     C* 1 = funzione richiamata chiusa in errore
050300940316    >C*  eventualmente gestire altri codici di errore
050400030113    >C     xtaerr        WHENEQ    '1'
050500940223     C                   MOVE      *ON           $ESCI
050600940223     C                   SETON                                        5299
050700940223     C*
0508009402231-   C                   ENDSL
050900940223     C*
051000940223     C                   ENDSR
051100940223     C/EJECT
051200940131     C************************************************************
051300940131     C* OPERAZIONI INIZIALI
051400940131     C************************************************************
051500940131     C     *INZSR        BEGSR
051600030113     C*
051700030113     C* Reperimento parametri
051800030113     C     *ENTRY        PLIST
051900030113     C                   PARM                    KPJBA
052000040930     C                   movel     kpjbu         Selez             1
052100040930     C*
052200040930     C* Se chiamato x selezionare un codice
052300040930     c                   clear                   x1cod
052400040930     c                   if        selez = '?'
052500040930     c                   seton                                        11
052600040930     c                   end
052700030113     C*
052800030113     C* Variabili per gestione videate
052900030113     C                   MOVE      *BLANK        $GEST             2
053000030113     C                   MOVE      *BLANK        $FINE             1
053100030113     C                   MOVE      *BLANK        $INZS1            1
053200030113     C                   MOVE      *BLANK        $EFILE            1
053300030113     C                   MOVE      *BLANK        $ESCI             1
053400030113     C                   MOVE      *BLANK        $RCDOK            1
053500030113     C*
053600030113     C     KTBL          KLIST
053700110901     C                   KFLD                    Tabgru
053800030113     C* Indici
053900030113     C                   Z-ADD     0             X                 3 0
054000030113     C                   Z-ADD     0             Y                 3 0
054100940506     C*
054200110921     C                   MOVE      '055'         tabGRU
054300940127     C*
054400940117     C                   ENDSR
054500030113     C************************************************************
