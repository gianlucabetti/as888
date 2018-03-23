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
002000110926     Ftntbe01L  IF   E           K DISK
002100110926     Fazorg01L  IF   E           K DISK
002200110927     FTntb90D   CF   E             WORKSTN
002300940607     F                                     SFILE(S1:S1NRR)
002400940201     F                                     INFDS(DSFMT)
002500940128     D*----------------------------------------------------*
002600940211     D* Passaggio Parametri
002700940211     D KPJBA         E DS
002800030113      *-------------
002900940325     D* Parametri in ricezione
003000030113     D  TABDS          DS
003100030113     D  XTAOPZ                 1      2
003200030113     D  XTARET                 3      3
003300030113     D  XTAOPR                 4      4
003400030113     D  XTAERR                 5      5
003500110926     D  XTAKEY                 6      8
003600940211     D*-------------
003700940211     D DSFMT           DS           512
003800940506     D  $TASTO               369    369
003900940211     D  NRG                  370    370
004000940211     D  NCL                  371    371
004100940211     D  SFLNRR               378    379B 0
004200940207     D*-------------
004300940207     D* Nome PGM a video
004400940207     D                 DS
004500940207     D  PROGR                  1     10
004600940207     D  ASTER1                 1      1    INZ('*')
004700940207     D  SIGLA                  2      9
004800940207     D  ASTER2                10     10    INZ('*')
004900940127     D*-------------
005000940127     D* Reperimento nome PGM
005100940127     D STATUS         SDS           333
005200940127     D  DSPGM            *PROC
005300940607     D* descrizione tabella
005400110926$003 D EXTDS         E DS                  EXTNAME(tntb90ds)
005500030113     D*-------------
005600030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005700030113$003 D S1NRR           S                   Like(C1rcd)
005800030113$003 D WSfl            S                   Like(C1nrr)
005900030113$003 D Wmax            S                   Like(C1rcd)
006000030113$003 D Wpag            S                   Like(C1rcd)
006100030113$003 D Winzs1          S                   Like($inzs1)
006200040930$003 D x1cod           S                   Like(s1cod)
006300110926     D KCOD            S                   LIKE(TBECOD) inz('WEK')
006400110926     D KKE1            S                   LIKE(TBEKE1)
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
009900940207     I*-------------
010000940607     IS1
010100110926$015 I              S1DES                       DESFIL
010200940127     C*----------------------------------------------------*
010300940127     C*                MAIN LINE PROGRAM
010400940127     C*----------------------------------------------------*
010500940223     C* inizializzazione variabili
010600940223     C                   EXSR      INZVAR
010700940223     C*
010800940223     C     $FINE         DOWEQ     *OFF
010900940131     C     $GEST         CASEQ     'S1'          GESS1
011000940117     C                   END
011100940117     C                   END
011200940325     C* fine programma
011300940325     C                   SETON                                            LR
011400030113     C************************************************************
011500030113     C* INIZIALIZZAZIONE VARIABILI
011600030113     C************************************************************
011700030113     C     INZVAR        BEGSR
011800030113     C*
011900030113     C* Pulizia campi e indicatori
012000030113     C                   MOVE      *ALL'0'       IN4049           10
012100030113     C                   MOVEA     IN4049        *IN(40)
012200030113     C                   CLEAR                   S1OPZ
012300030113     C* Variabili per gestione videate
012400030113     C*
012500030113     C                   MOVE      *OFF          $FINE
012600030113     C                   MOVE      *OFF          $INZS1
012700030113     C                   MOVE      *OFF          $EFILE
012800030113     C                   MOVE      *OFF          $ESCI
012900030113     C                   MOVE      *OFF          $RCDOK
013000030113     C                   Z-ADD     0             $ULKS1            3 0
013100030113     C*
013200030113     C                   MOVE      *ON           $INZS1
013300030113     C                   MOVE      'S1'          $GEST
013400030113     C*
013500030113     C* Variabili appoggio
013600030114     C                   Z-ADD     1             WPAG
013700030113     c*
013800030113     C                   ENDSR
013900940127     C************************************************************
014000940131     C* GESTIONE LISTA
014100940127     C************************************************************
014200940127     C     GESS1         BEGSR
014300030113     C*
014400940223     C* inizializzazione videata
014500940223     C     $INZS1        IFEQ      *ON
014600940127     C                   EXSR      INZS1
014700940223     C                   MOVE      *OFF          $INZS1
014800940127     C                   ENDIF
014900030113     C*
015000030113     C* emissione piede videata
015100030113     C                   WRITE     Z1
015200030113     C* Non ci sono records
015300940223     C     WMAX          IFEQ      0
015400940607     C                   WRITE     D1
015500030114     C                   Else
015600030114     C     Wsfl          IFgt      0
015700030114     C                   Z-ADD     wsfl          C1RCD
015800030114     C                   Else
015900030114     C     Wpag          IFgt      0
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
020600110926     c     kTBL          setll     tntbe01l
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
022900110926     c                   clear                   DESfil
023000110901     c                   move      *all'0'       s1cod
023100110926     c                   movel     tbeke1        s1cod
023200110926     c                   move      s1cod         d1fil             3 0
023300110926      **
023400110926     c     d1FIL         chain     azorg01l
023500110926     c                   if        %Found(azorg01l)
023600110926     c                   movel     orgdes        DESfil
023700110926     c                   end
023800110926     c                   movel     tbeuni        extds
023900030113     C*
024000030115     c                   clear                   S1Ann
024100940127     C*
024200940127     C                   ADD       1             S1NRR
024300940127     C                   ADD       1             Y
024400940127     C*
024500940607     C                   WRITE     S1
024600940131     C*
024700940316    >C                   EXSR      REDANA
024800940128     C*
0249009401271-   C                   ENDDO
025000940127     C*
025100940223     C                   Z-ADD     S1NRR         WMAX                 30
025200940127     C*
025300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
025400940127     C*
025500030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
025600940127     C                   MVR                     RESTO             3 0
025700030114     C     PAGINE        MULT      SFLPAG        C1RCD
0258000301141    C     RESTO         IFGT      0
025900030114     C                   ADD       1             C1RCD
0260000301141E   C                   ELSE
026100030114     C                   SUB       SFLPAG        C1RCD
026200030114     C                   ADD       1             C1RCD
0263000301141-   C                   ENDIF
026400940128     C*
026500940127     C                   ENDSR
026600940128     C************************************************************
026700940131     C* LETTURA RCD ARCHIVIO PILOTA
026800940128     C************************************************************
026900940607     C     REDANA        BEGSR
027000940128     C*
027100940131     C                   MOVEL     *OFF          $EFILE
027200940131     C                   MOVEL     *OFF          $RCDOK
027300940131     C*
0274009401311    C     $EFILE        DOUEQ     *ON
027500940131     C     $RCDOK        OREQ      *ON
027600940131     C*
027700110926     c     kTBL          Reade     tntbe01l
027800110926     c                   if        %eof(tntbe01l)
027900030113     C                   MOVEL     *ON           $EFILE
028000030113     C                   MOVE      $EFILE        *IN33
028100030113     c                   else
028200030113     C                   MOVE      *ON           $RCDOK
028300030113     c                   end
028400940131     C*
0285009401311-   C                   ENDDO
028600940131     C*
028700940131     C                   ENDSR
028800940224     C************************************************************
028900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
029000940224     C************************************************************
029100940224     C     CLCPAG        BEGSR
029200940224     C* Input :
029300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
029400940224     C*          cursore
029500940224     C* - SFLPAG = numero rcd per pagina sfl
029600940224     C* Output :
029700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
029800940224     C*
029900940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
030000940224     C                   MVR                     RESTO             3 0
030100940224     C     RESTO         IFGT      0
030200940224     C                   ADD       1             PAGINE
030300940224     C                   ENDIF
030400940224     C     PAGINE        MULT      SFLPAG        WPAG
030500940224     C*
030600940224     C                   ENDSR
030700940309     C************************************************************
030800940309     C* GESTIONE F03 VIDEO S1
030900940309     C************************************************************
031000940309     C     F03S1         BEGSR
031100940309     C*
031200940309     C                   MOVE      *ON           $FINE
031300940325     C* fine programma
031400940309     C                   ENDSR
031500940309     C/EJECT
031600940309     C************************************************************
031700940309     C* GESTIONE F10 VIDEO S1
031800940314     c* AGGIUNTA RECORD
031900940309     C************************************************************
032000940309     C     F10S1         BEGSR
032100940309     C*
032200030113     C                   RESET                   tabds
032300030113     C                   MOVEL     '01'          xtaopz
032400030113     C                   MOVE      *ZERO         xtaret
032500030113     C                   MOVE      *ZERO         xtaopr
032600030114     C                   MOVEl     *blank        xtakey
032700030113     C                   MOVE      *BLANKS       KPJBU
032800030113     C                   MOVEL     tabds         KPJBU
032900110927$004 C                   CALL      'TNTB90R2'
033000030113     C                   PARM                    KPJBA
033100030114     C                   MOVEL     KPJBU         tabds
033200030113      *
033300940309     C* ritorno da PGM gestione
033400940309     C                   EXSR      GESRET
033500940309     C     WINZS1        IFEQ      *ON
033600940309     C                   MOVE      *ON           $INZS1
033700940309     C* se esistevano già righe sul sfl
033800940309     C* calcolo pagina a cui deve posizionarsi
033900940309     C     WSFL          IFGT      0
034000940309     C                   EXSR      CLCPAG
034100940309     C* altrimenti carico solo la 1a pagina
034200940309     C                   ELSE
034300940309     C                   Z-ADD     SFLPAG        WPAG
034400940309     C                   ENDIF
034500940309     C                   ENDIF
034600940309     C*
034700940309     C                   ENDSR
034800940128     C************************************************************
034900940131     C* CONTROLLO TESTATA LISTA
035000940128     C************************************************************
035100940131     C     CTRC1         BEGSR
035200940128     C*
035300940201     C                   MOVE      *OFF          *IN99
035400940131     C*
035500940202     C                   ENDSR
035600940131     C************************************************************
035700940131     C* CONTROLLO OPZIONI LISTA
035800940131     C************************************************************
035900940131     C     CTRS1         BEGSR
036000940131     C*
036100940202     C                   MOVEL     *OFF          $ESCI
036200940201     C                   SETOFF                                       99
036300940131     C                   Z-ADD     0             S1OPZ
036400040930     c                   clear                   x1cod
036500940131     C*
036600940127     C* Leggo il sfl solo se ci sono rcd
0367009401311    C     WMAX          IFGT      0
036800940607     C                   READC     S1                                     21
036900940127     C*
037000940131     C* esce se fine sfl o errore che richiede l'uscita
0371009401312    C     *IN21         DOWEQ     *OFF
037200940131     C     $ESCI         ANDEQ     *OFF
037300940201     C                   Z-ADD     S1NRR         C1RCD
037400940131     C* ctrl su riga
037500940131     C                   EXSR      RECS1
037600940131     C* gestione opzioni
0377009401313    C     S1OPZ         IFNE      0
037800940201     C     *IN99         ANDEQ     *OFF
037900940131     C                   EXSR      OPZS1
0380009401273-   C                   ENDIF
038100940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0382009402013    C     *IN99         IFEQ      *ON
038300940201     C     $ESCI         OREQ      *ON
038400940131     C                   MOVE      *ON           *IN32
038500940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038600940204     C* la ripristinerò a conclusione del ciclo di READC
038700940223     C                   MOVE      *OFF          $INZS1
0388009402233-   C                   ENDIF
038900940223     C*
039000940131     C                   Z-ADD     0             S1OPZ
039100940223     C*
039200940607     C                   UPDATE    S1
039300940223     C*
039400940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0395009401313    C     $ESCI         IFEQ      *OFF
039600940607     C                   READC     S1                                     21
039700940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0398009402014    C     *IN21         IFEQ      *ON
039900940201     C                   MOVE      WINZS1        $INZS1
040000940204     C* calcolo pagina a cui deve posizionarsi
040100940224     C                   EXSR      CLCPAG
0402009402014-   C                   ENDIF
0403009402013-   C                   ENDIF
040400940131     C*
0405009401272-   C                   ENDDO
040600940127     C*
0407009401311-   C                   ENDIF
040800940131     C*
040900940127     C                   ENDSR
041000940127     C/EJECT
041100940127     C************************************************************
041200940131     C* CONTROLLO CAMPI I/O RIGA LISTA
041300940127     C************************************************************
041400940131     C     RECS1         BEGSR
041500940131     C*
041600940201     C* reset indicatori DSPATR
041700940201     C                   MOVE      *ALL'0'       IN4049           10
041800940201     C                   MOVEA     IN4049        *IN(40)
041900940201     C*
042000040930     C*  se richiamato x selezionare un codice 11 = *on
042100040930     C*   non deve utilizzare le opzioni di modifica
042200040930     C*  e viceversa
042300040930     c     *in11         ifeq      *on
0424000409303    C     S1OPZ         IFeq      2
0425000409303    C     S1OPZ         OReq      3
0426000409303    C     S1OPZ         OReq      4
042700040930     c                   seton                                        41  99
042800040930     C                   END
042900040930      * selezionato un codice da restituire al chiamante
0430000409303    C     S1OPZ         IFeq      1
043100040930     c                   move      s1cod         x1cod
043200040930     c                   clear                   kpjbu
043300040930     c                   movel     x1cod         kpjbu
043400040930     C                   MOVEL     *ON           $ESCI
043500040930     C                   MOVEL     *ON           $fine
043600040930     C                   END
043700040930     C                   END
043800040930     C*
043900040930     c     *in11         ifeq      *off
0440000409303    C     S1OPZ         ANDeq     1
044100040930     c                   seton                                        41  99
044200040930     C                   END
044300040930     C*
044400940131     C                   ENDSR
044500940131     C************************************************************
044600940131     C* GESTIONE OPZIONI LISTA
044700940131     C************************************************************
044800940131     C     OPZS1         BEGSR
044900940201     C*
0450000409302    C     *IN11         IFEQ      *Off
0451000409302    C     *IN11         oreq      *On
0452000409302    C     s1opz         andeq     05
045300040930     C*
045400030113     C                   RESET                   tabds
045500110902     C                   MOVE      *all'0'       xtaopz
045600110902     C                   MOVE      S1OPZ         xtaopz
045700030113     C                   MOVE      *ZERO         xtaret
045800030113     C                   MOVE      *ZERO         xtaopr
045900110902     C                   MOVEl     S1COD         xtakey
046000940715     C                   MOVE      *BLANKS       KPJBU
046100030113     C                   MOVEL     tabds         KPJBU
046200110927$004 C                   CALL      'TNTB90R2'
046300940607     C                   PARM                    KPJBA
046400030114     C                   MOVEL     KPJBU         tabds
046500940201     C*
046600940223     C* ritorno da PGM gestione
046700940223     C                   EXSR      GESRET
046800040930     C*
046900940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0470009402252    C     *IN99         IFEQ      *ON
047100940223     C                   SETON                                        40
0472009402252-   C                   ENDIF
047300940225     C*
0474000409302-   C                   ENDIF
047500040930     C*
047600940131     C                   ENDSR
047700940223     C/EJECT
047800940223     C************************************************************
047900940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
048000940223     C************************************************************
048100940223     C     GESRET        BEGSR
048200940223     C*
048300940223     C* modo di ritorno
048400940223     C*
0485009402231    C                   SELECT
048600940314    >C* << questi modi di utilizzo dei valori di ritorno dal
048700940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
048800940314    >C*    proposte, normalmente sempre valide, ma modificabili
048900940314    >C*    per situazioni particolari >>
049000940223     C* 1 = F3
049100030113    >C     xtaret        WHENEQ    '1'
049200940224     C                   MOVE      *ON           $ESCI
049300940223     C                   MOVE      *ON           $FINE
049400940223     C* 2 = F12
049500030113    >C     xtaret        WHENEQ    '2'
049600940223     C                   MOVE      *ON           $ESCI
049700940223     C*
0498009402231-   C                   ENDSL
049900940223     C*
050000940223     C* operazione eseguite dal pgm chiamato
050100940223     C*
0502009402231    C                   SELECT
050300940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
050400030113    >C     xtaopr        WHENEQ    '1'
050500940223     C                   MOVE      *ON           WINZS1
050600940223     C*
0507009402231-   C                   ENDSL
050800940223     C*
050900940223     C* funzione non eseguibile per errore :
051000940223     C*
0511009402231    C                   SELECT
051200940223     C* 1 = funzione richiamata chiusa in errore
051300940316    >C*  eventualmente gestire altri codici di errore
051400030113    >C     xtaerr        WHENEQ    '1'
051500940223     C                   MOVE      *ON           $ESCI
051600940223     C                   SETON                                        5299
051700940223     C*
0518009402231-   C                   ENDSL
051900940223     C*
052000940223     C                   ENDSR
052100940223     C/EJECT
052200940131     C************************************************************
052300940131     C* OPERAZIONI INIZIALI
052400940131     C************************************************************
052500940131     C     *INZSR        BEGSR
052600030113     C*
052700030113     C* Reperimento parametri
052800030113     C     *ENTRY        PLIST
052900030113     C                   PARM                    KPJBA
053000040930     C                   movel     kpjbu         Selez             1
053100040930     C*
053200040930     C* Se chiamato x selezionare un codice
053300040930     c                   clear                   x1cod
053400040930     c                   if        selez = '?'
053500040930     c                   seton                                        11
053600040930     c                   end
053700030113     C*
053800030113     C* Variabili per gestione videate
053900030113     C                   MOVE      *BLANK        $GEST             2
054000030113     C                   MOVE      *BLANK        $FINE             1
054100030113     C                   MOVE      *BLANK        $INZS1            1
054200030113     C                   MOVE      *BLANK        $EFILE            1
054300030113     C                   MOVE      *BLANK        $ESCI             1
054400030113     C                   MOVE      *BLANK        $RCDOK            1
054500030113     C*
054600030113     C     KTBL          KLIST
054700110926     C                   KFLD                    KCOD
054800030113     C* Indici
054900030113     C                   Z-ADD     0             X                 3 0
055000030113     C                   Z-ADD     0             Y                 3 0
055100940506     C*
055200110926     C                   move      'WEK'         KCOD
055300940127     C*
055400940117     C                   ENDSR
055500030113     C************************************************************
