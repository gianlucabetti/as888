000100141127     H*
000200141127     H*
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
002200080701     FTNTBE01L  IF   E           K DISK
002300080701     FTNTB68D   CF   E             WORKSTN
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
005400940607     D* descrizione tabella
005500080701$003 D EXTDS         E DS                  EXTNAME(DDSF)
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
010000080701$015 I              S1DES                       D§DSFDES
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
020500080701     c     kTBL          setll     TnTBe01L
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
022800080701     c                   movel     tbeke1        s1cod
022900080701     c                   movel     tbeuni        extds
023000030113     C*
023100030115     c                   clear                   S1Ann
023200080701     c                   if        tbeATB ='*'
023300030115     c                   move      'A'           S1Ann
023400030115     c                   end
023500940127     C*
023600940127     C                   ADD       1             S1NRR
023700940127     C                   ADD       1             Y
023800940127     C*
023900940607     C                   WRITE     S1
024000940131     C*
024100940316    >C                   EXSR      REDANA
024200940128     C*
0243009401271-   C                   ENDDO
024400940127     C*
024500940223     C                   Z-ADD     S1NRR         WMAX                 30
024600940127     C*
024700940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
024800940127     C*
024900030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
025000940127     C                   MVR                     RESTO             3 0
025100030114     C     PAGINE        MULT      SFLPAG        C1RCD
0252000301141    C     RESTO         IFGT      0
025300030114     C                   ADD       1             C1RCD
0254000301141E   C                   ELSE
025500030114     C                   SUB       SFLPAG        C1RCD
025600030114     C                   ADD       1             C1RCD
0257000301141-   C                   ENDIF
025800940128     C*
025900940127     C                   ENDSR
026000940128     C************************************************************
026100940131     C* LETTURA RCD ARCHIVIO PILOTA
026200940128     C************************************************************
026300940607     C     REDANA        BEGSR
026400940128     C*
026500940131     C                   MOVEL     *OFF          $EFILE
026600940131     C                   MOVEL     *OFF          $RCDOK
026700940131     C*
0268009401311    C     $EFILE        DOUEQ     *ON
026900940131     C     $RCDOK        OREQ      *ON
027000940131     C*
027100080701     c     kTBL          Reade     TnTBe01L
027200080701     c                   if        %eof(TnTBe01L)
027300030113     C                   MOVEL     *ON           $EFILE
027400030113     C                   MOVE      $EFILE        *IN33
027500030113     c                   else
027600030113     C                   MOVE      *ON           $RCDOK
027700030113     c                   end
027800940131     C*
0279009401311-   C                   ENDDO
028000940131     C*
028100940131     C                   ENDSR
028200940224     C************************************************************
028300940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
028400940224     C************************************************************
028500940224     C     CLCPAG        BEGSR
028600940224     C* Input :
028700940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
028800940224     C*          cursore
028900940224     C* - SFLPAG = numero rcd per pagina sfl
029000940224     C* Output :
029100940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
029200940224     C*
029300940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
029400940224     C                   MVR                     RESTO             3 0
029500940224     C     RESTO         IFGT      0
029600940224     C                   ADD       1             PAGINE
029700940224     C                   ENDIF
029800940224     C     PAGINE        MULT      SFLPAG        WPAG
029900940224     C*
030000940224     C                   ENDSR
030100940309     C************************************************************
030200940309     C* GESTIONE F03 VIDEO S1
030300940309     C************************************************************
030400940309     C     F03S1         BEGSR
030500940309     C*
030600940309     C                   MOVE      *ON           $FINE
030700940325     C* fine programma
030800940309     C                   ENDSR
030900940309     C/EJECT
031000940309     C************************************************************
031100940309     C* GESTIONE F10 VIDEO S1
031200940314     c* AGGIUNTA RECORD
031300940309     C************************************************************
031400940309     C     F10S1         BEGSR
031500940309     C*
031600030113     C                   RESET                   tabds
031700030113     C                   MOVEL     '01'          xtaopz
031800030113     C                   MOVE      *ZERO         xtaret
031900030113     C                   MOVE      *ZERO         xtaopr
032000030114     C                   MOVEl     *blank        xtakey
032100030113     C                   MOVE      *BLANKS       KPJBU
032200030113     C                   MOVEL     tabds         KPJBU
032300080701$004 C                   CALL      'TNTB68R2'
032400030113     C                   PARM                    KPJBA
032500030114     C                   MOVEL     KPJBU         tabds
032600030113      *
032700940309     C* ritorno da PGM gestione
032800940309     C                   EXSR      GESRET
032900940309     C     WINZS1        IFEQ      *ON
033000940309     C                   MOVE      *ON           $INZS1
033100940309     C* se esistevano già righe sul sfl
033200940309     C* calcolo pagina a cui deve posizionarsi
033300940309     C     WSFL          IFGT      0
033400940309     C                   EXSR      CLCPAG
033500940309     C* altrimenti carico solo la 1a pagina
033600940309     C                   ELSE
033700940309     C                   Z-ADD     SFLPAG        WPAG
033800940309     C                   ENDIF
033900940309     C                   ENDIF
034000940309     C*
034100940309     C                   ENDSR
034200940128     C************************************************************
034300940131     C* CONTROLLO TESTATA LISTA
034400940128     C************************************************************
034500940131     C     CTRC1         BEGSR
034600940128     C*
034700940201     C                   MOVE      *OFF          *IN99
034800940131     C*
034900940202     C                   ENDSR
035000940131     C************************************************************
035100940131     C* CONTROLLO OPZIONI LISTA
035200940131     C************************************************************
035300940131     C     CTRS1         BEGSR
035400940131     C*
035500940202     C                   MOVEL     *OFF          $ESCI
035600940201     C                   SETOFF                                       99
035700940131     C                   Z-ADD     0             S1OPZ
035800040930     c                   clear                   x1cod
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
041400040930     C*  se richiamato x selezionare un codice 11 = *on
041500040930     C*   non deve utilizzare le opzioni di modifica
041600040930     C*  e viceversa
041700040930     c     *in11         ifeq      *on
0418000409303    C     S1OPZ         IFeq      2
0419000409303    C     S1OPZ         OReq      3
0420000409303    C     S1OPZ         OReq      4
042100040930     c                   seton                                        41  99
042200040930     C                   END
042300040930      * selezionato un codice da restituire al chiamante
0424000409303    C     S1OPZ         IFeq      1
042500040930     c                   move      s1cod         x1cod
042600040930     c                   clear                   kpjbu
042700040930     c                   movel     x1cod         kpjbu
042800040930     C                   MOVEL     *ON           $ESCI
042900040930     C                   MOVEL     *ON           $fine
043000040930     C                   END
043100040930     C                   END
043200040930     C*
043300040930     c     *in11         ifeq      *off
0434000409303    C     S1OPZ         ANDeq     1
043500040930     c                   seton                                        41  99
043600040930     C                   END
043700040930     C*
043800940131     C                   ENDSR
043900940131     C************************************************************
044000940131     C* GESTIONE OPZIONI LISTA
044100940131     C************************************************************
044200940131     C     OPZS1         BEGSR
044300940201     C*
0444000409302    C     *IN11         IFEQ      *Off
0445000409302    C     *IN11         oreq      *On
0446000409302    C     s1opz         andeq     05
044700040930     C*
044800030113     C                   RESET                   tabds
044900030113     C                   MOVEL     S1OPZ         xtaopz
045000030113     C                   MOVE      *ZERO         xtaret
045100030113     C                   MOVE      *ZERO         xtaopr
045200030113     C                   MOVEl     S1COD         xtakey
045300940715     C                   MOVE      *BLANKS       KPJBU
045400030113     C                   MOVEL     tabds         KPJBU
045500080701$004 C                   CALL      'TNTB68R2'
045600940607     C                   PARM                    KPJBA
045700030114     C                   MOVEL     KPJBU         tabds
045800940201     C*
045900940223     C* ritorno da PGM gestione
046000940223     C                   EXSR      GESRET
046100040930     C*
046200940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0463009402252    C     *IN99         IFEQ      *ON
046400940223     C                   SETON                                        40
0465009402252-   C                   ENDIF
046600940225     C*
0467000409302-   C                   ENDIF
046800040930     C*
046900940131     C                   ENDSR
047000940223     C/EJECT
047100940223     C************************************************************
047200940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
047300940223     C************************************************************
047400940223     C     GESRET        BEGSR
047500940223     C*
047600940223     C* modo di ritorno
047700940223     C*
0478009402231    C                   SELECT
047900940314    >C* << questi modi di utilizzo dei valori di ritorno dal
048000940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
048100940314    >C*    proposte, normalmente sempre valide, ma modificabili
048200940314    >C*    per situazioni particolari >>
048300940223     C* 1 = F3
048400030113    >C     xtaret        WHENEQ    '1'
048500940224     C                   MOVE      *ON           $ESCI
048600940223     C                   MOVE      *ON           $FINE
048700940223     C* 2 = F12
048800030113    >C     xtaret        WHENEQ    '2'
048900940223     C                   MOVE      *ON           $ESCI
049000940223     C*
0491009402231-   C                   ENDSL
049200940223     C*
049300940223     C* operazione eseguite dal pgm chiamato
049400940223     C*
0495009402231    C                   SELECT
049600940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
049700030113    >C     xtaopr        WHENEQ    '1'
049800940223     C                   MOVE      *ON           WINZS1
049900940223     C*
0500009402231-   C                   ENDSL
050100940223     C*
050200940223     C* funzione non eseguibile per errore :
050300940223     C*
0504009402231    C                   SELECT
050500940223     C* 1 = funzione richiamata chiusa in errore
050600940316    >C*  eventualmente gestire altri codici di errore
050700030113    >C     xtaerr        WHENEQ    '1'
050800940223     C                   MOVE      *ON           $ESCI
050900940223     C                   SETON                                        5299
051000940223     C*
0511009402231-   C                   ENDSL
051200940223     C*
051300940223     C                   ENDSR
051400940223     C/EJECT
051500940131     C************************************************************
051600940131     C* OPERAZIONI INIZIALI
051700940131     C************************************************************
051800940131     C     *INZSR        BEGSR
051900030113     C*
052000030113     C* Reperimento parametri
052100030113     C     *ENTRY        PLIST
052200030113     C                   PARM                    KPJBA
052300040930     C                   movel     kpjbu         Selez             1
052400040930     C*
052500040930     C* Se chiamato x selezionare un codice
052600040930     c                   clear                   x1cod
052700040930     c                   if        selez = '?'
052800040930     c                   seton                                        11
052900040930     c                   end
053000030113     C*
053100030113     C* Variabili per gestione videate
053200030113     C                   MOVE      *BLANK        $GEST             2
053300030113     C                   MOVE      *BLANK        $FINE             1
053400030113     C                   MOVE      *BLANK        $INZS1            1
053500030113     C                   MOVE      *BLANK        $EFILE            1
053600030113     C                   MOVE      *BLANK        $ESCI             1
053700030113     C                   MOVE      *BLANK        $RCDOK            1
053800030113     C*
053900030113     C     KTBL          KLIST
054000080701     C                   KFLD                    TBECOD
054100030113     C* Indici
054200030113     C                   Z-ADD     0             X                 3 0
054300030113     C                   Z-ADD     0             Y                 3 0
054400940506     C*
054500080701     C                   MOVE      'DSF'         tbecod
054600940127     C*
054700940117     C                   ENDSR
054800030113     C************************************************************
