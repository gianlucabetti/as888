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
002100060927     FTntbe01L  IF   E           K DISK
002200060927     Ffieu56D   CF   E             WORKSTN
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
003500030113     D  XTAKEY                 6     13
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
005300060927     D* descrizione tabella
005400060927$003 D***** EXTDS         E DS                  EXTNAME(DSOD)
005500030113     D*-------------
005600030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005700030113$003 D S1NRR           S                   Like(C1rcd)
005800030113$003 D WSfl            S                   Like(C1nrr)
005900030113$003 D Wmax            S                   Like(C1rcd)
006000030113$003 D Wpag            S                   Like(C1rcd)
006100030113$003 D Winzs1          S                   Like($inzs1)
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
009600940127     C*----------------------------------------------------*
009700940127     C*                MAIN LINE PROGRAM
009800940127     C*----------------------------------------------------*
009900940223     C* inizializzazione variabili
010000940223     C                   EXSR      INZVAR
010100940223     C*
010200940223     C     $FINE         DOWEQ     *OFF
010300940131     C     $GEST         CASEQ     'S1'          GESS1
010400940117     C                   END
010500940117     C                   END
010600940325     C* fine programma
010700940325     C                   SETON                                            LR
010800030113     C************************************************************
010900030113     C* INIZIALIZZAZIONE VARIABILI
011000030113     C************************************************************
011100030113     C     INZVAR        BEGSR
011200030113     C*
011300030113     C* Pulizia campi e indicatori
011400030113     C                   MOVE      *ALL'0'       IN4049           10
011500030113     C                   MOVEA     IN4049        *IN(40)
011600030113     C                   CLEAR                   S1OPZ
011700030113     C* Variabili per gestione videate
011800030113     C*
011900030113     C                   MOVE      *OFF          $FINE
012000030113     C                   MOVE      *OFF          $INZS1
012100030113     C                   MOVE      *OFF          $EFILE
012200030113     C                   MOVE      *OFF          $ESCI
012300030113     C                   MOVE      *OFF          $RCDOK
012400030113     C                   Z-ADD     0             $ULKS1            3 0
012500030113     C*
012600030113     C                   MOVE      *ON           $INZS1
012700030113     C                   MOVE      'S1'          $GEST
012800030113     C*
012900030113     C* Variabili appoggio
013000030114     C                   Z-ADD     1             WPAG
013100030113     c*
013200030113     C                   ENDSR
013300940127     C************************************************************
013400940131     C* GESTIONE LISTA
013500940127     C************************************************************
013600940127     C     GESS1         BEGSR
013700030113     C*
013800940223     C* inizializzazione videata
013900940223     C     $INZS1        IFEQ      *ON
014000940127     C                   EXSR      INZS1
014100940223     C                   MOVE      *OFF          $INZS1
014200940127     C                   ENDIF
014300030113     C*
014400030113     C* emissione piede videata
014500030113     C                   WRITE     Z1
014600030113     C* Non ci sono records
014700940223     C     WMAX          IFEQ      0
014800940607     C                   WRITE     D1
014900030114     C                   Else
015000030114     C     Wsfl          IFgt      0
015100030114     C                   Z-ADD     wsfl          C1RCD
015200030114     C                   Else
015300030114     C     Wpag          IFgt      0
015400030114     C                   Z-ADD     wpag          C1RCD
015500030114     C                   EndIF
015600030114     C                   EndIF
015700030114     C                   ENDIF
015800940127     C*
015900030113     C*              *------------------*
016000940607     C                   EXFMT     C1
016100030113     C*              *------------------*
016200030113     C*
016300940204     C     C1NRR         IFNE      0
016400940204     C                   Z-ADD     C1NRR         WSFL
016500940204     C                   ENDIF
016600940127     C                   Z-ADD     SFLNRR        C1RCD
016700030113     C* Selezioni
0168009401271    C                   SELECT
016900940127     C* F3=Fine
017000940506     C     $TASTO        WHENEQ    F03
017100940309     C                   EXSR      F03S1
017200940131     C* F10=Immissione
017300940506     C     $TASTO        WHENEQ    F10
017400940309     C                   EXSR      F10S1
0175009401271O   C                   OTHER
017600940127     C* CONTROLLO DATI
017700940131     C                   EXSR      CTRC1
017800940201     C     *IN99         IFEQ      *OFF
017900940131     C                   EXSR      CTRS1
018000940131     C                   END
0181009401271-   C                   ENDSL
018200940127     C*
018300940127     C                   ENDSR
018400940224     C/EJECT
018500940127     C************************************************************
018600940131     C* INIZIALIZZAZIONE LISTA
018700940127     C************************************************************
018800940127     C     INZS1         BEGSR
018900940302     C* pulizia SFL
019000940128     C                   SETOFF                                         3031
019100940607     C                   WRITE     C1
019200940128     C                   SETON                                          31
019300940128     C*
019400030113     C* CARICAMENTO SFL totale
019500940201     C                   Z-ADD     0             S1NRR
019600030113     C                   Z-ADD     1             C1RCD
019700940128     C                   Z-ADD     0             WMAX
019800940224     C*
019900940224     C* Posizionamento su file pilota
020000060927     c     kTBE          setll     Tntbe01l
020100940608    >C                   EXSR      REDANA
020200030113     C* Carico il SFL
020300940127     C                   EXSR      ROLS1
020400030113     C*
020500030114     c                   if        xtaopr <> '1'
020600030114     C                   Z-ADD     1             WPAG
020700030114     c                   end
020800940127     C*
020900940127     C                   ENDSR
021000940127     C************************************************************
021100940131     C* CARICAMENTO PAGINA LISTA
021200940127     C************************************************************
021300940127     C     ROLS1         BEGSR
021400940127     C*
021500940128     C                   SETOFF                                       32
021600940223     C                   Z-ADD     0             Y
021700940127     C                   Z-ADD     WMAX          S1NRR
021800940127     C*
021900940127     C* Leggo dal file anagrafico per caricare la lista
0220009401311    C     $EFILE        DOWEQ     *OFF
022100940127     C*
022200030113     c                   clear                   s1opz
022300060927     c                   movel     tbeke1        s1cod
022400060927     c                   movel     tbeuni        s1des
022500030113     C*
022600030115     c                   clear                   S1Ann
022700060927     c                   if        tbeatb <>*blank
022800030115     c                   move      'A'           S1Ann
022900030115     c                   end
023000940127     C*
023100940127     C                   ADD       1             S1NRR
023200940127     C                   ADD       1             Y
023300940127     C*
023400940607     C                   WRITE     S1
023500940131     C*
023600940316    >C                   EXSR      REDANA
023700940128     C*
0238009401271-   C                   ENDDO
023900940127     C*
024000940223     C                   Z-ADD     S1NRR         WMAX                 30
024100940127     C*
024200940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
024300940127     C*
024400030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
024500940127     C                   MVR                     RESTO             3 0
024600030114     C     PAGINE        MULT      SFLPAG        C1RCD
0247000301141    C     RESTO         IFGT      0
024800030114     C                   ADD       1             C1RCD
0249000301141E   C                   ELSE
025000030114     C                   SUB       SFLPAG        C1RCD
025100030114     C                   ADD       1             C1RCD
0252000301141-   C                   ENDIF
025300940128     C*
025400940127     C                   ENDSR
025500940128     C************************************************************
025600940131     C* LETTURA RCD ARCHIVIO PILOTA
025700940128     C************************************************************
025800940607     C     REDANA        BEGSR
025900940128     C*
026000940131     C                   MOVEL     *OFF          $EFILE
026100940131     C                   MOVEL     *OFF          $RCDOK
026200940131     C*
0263009401311    C     $EFILE        DOUEQ     *ON
026400940131     C     $RCDOK        OREQ      *ON
026500940131     C*
026600060927     c     kTBE          Reade     Tntbe01l
026700060927     c                   if        %eof(Tntbe01l)
026800030113     C                   MOVEL     *ON           $EFILE
026900030113     C                   MOVE      $EFILE        *IN33
027000030113     c                   else
027100030113     C                   MOVE      *ON           $RCDOK
027200030113     c                   end
027300940131     C*
0274009401311-   C                   ENDDO
027500940131     C*
027600940131     C                   ENDSR
027700940224     C************************************************************
027800940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
027900940224     C************************************************************
028000940224     C     CLCPAG        BEGSR
028100940224     C* Input :
028200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
028300940224     C*          cursore
028400940224     C* - SFLPAG = numero rcd per pagina sfl
028500940224     C* Output :
028600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
028700940224     C*
028800940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
028900940224     C                   MVR                     RESTO             3 0
029000940224     C     RESTO         IFGT      0
029100940224     C                   ADD       1             PAGINE
029200940224     C                   ENDIF
029300940224     C     PAGINE        MULT      SFLPAG        WPAG
029400940224     C*
029500940224     C                   ENDSR
029600940309     C************************************************************
029700940309     C* GESTIONE F03 VIDEO S1
029800940309     C************************************************************
029900940309     C     F03S1         BEGSR
030000940309     C*
030100940309     C                   MOVE      *ON           $FINE
030200940325     C* fine programma
030300940309     C                   ENDSR
030400940309     C/EJECT
030500940309     C************************************************************
030600940309     C* GESTIONE F10 VIDEO S1
030700940314     c* AGGIUNTA RECORD
030800940309     C************************************************************
030900940309     C     F10S1         BEGSR
031000060927     C******
031100030113     C                   RESET                   tabds
031200060927     C******             MOVEL     '01'          xtaopz
031300060927     C******             MOVE      *ZERO         xtaret
031400060927     C******             MOVE      *ZERO         xtaopr
031500060927     C******             MOVEl     *blank        xtakey
031600060927     C******             MOVE      *BLANKS       KPJBU
031700060927     C******             MOVEL     tabds         KPJBU
031800060927$004 C******             CALL      'TRTB88R2'
031900060927     C******             PARM                    KPJBA
032000060927     C******             MOVEL     KPJBU         tabds
032100030113      *
032200940309     C* ritorno da PGM gestione
032300940309     C                   EXSR      GESRET
032400940309     C     WINZS1        IFEQ      *ON
032500940309     C                   MOVE      *ON           $INZS1
032600940309     C* se esistevano già righe sul sfl
032700940309     C* calcolo pagina a cui deve posizionarsi
032800940309     C     WSFL          IFGT      0
032900940309     C                   EXSR      CLCPAG
033000940309     C* altrimenti carico solo la 1a pagina
033100940309     C                   ELSE
033200940309     C                   Z-ADD     SFLPAG        WPAG
033300940309     C                   ENDIF
033400940309     C                   ENDIF
033500940309     C*
033600940309     C                   ENDSR
033700940128     C************************************************************
033800940131     C* CONTROLLO TESTATA LISTA
033900940128     C************************************************************
034000940131     C     CTRC1         BEGSR
034100940128     C*
034200940201     C                   MOVE      *OFF          *IN99
034300940131     C*
034400940202     C                   ENDSR
034500940131     C************************************************************
034600940131     C* CONTROLLO OPZIONI LISTA
034700940131     C************************************************************
034800940131     C     CTRS1         BEGSR
034900940131     C*
035000940202     C                   MOVEL     *OFF          $ESCI
035100940201     C                   SETOFF                                       99
035200940131     C                   Z-ADD     0             S1OPZ
035300940131     C*
035400940127     C* Leggo il sfl solo se ci sono rcd
0355009401311    C     WMAX          IFGT      0
035600940607     C                   READC     S1                                     21
035700940127     C*
035800940131     C* esce se fine sfl o errore che richiede l'uscita
0359009401312    C     *IN21         DOWEQ     *OFF
036000940131     C     $ESCI         ANDEQ     *OFF
036100940201     C                   Z-ADD     S1NRR         C1RCD
036200940131     C* ctrl su riga
036300940131     C                   EXSR      RECS1
036400940131     C* gestione opzioni
0365009401313    C     S1OPZ         IFNE      0
036600940201     C     *IN99         ANDEQ     *OFF
036700940131     C                   EXSR      OPZS1
0368009401273-   C                   ENDIF
036900940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0370009402013    C     *IN99         IFEQ      *ON
037100940201     C     $ESCI         OREQ      *ON
037200940131     C                   MOVE      *ON           *IN32
037300940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
037400940204     C* la ripristinerò a conclusione del ciclo di READC
037500940223     C                   MOVE      *OFF          $INZS1
0376009402233-   C                   ENDIF
037700940223     C*
037800940131     C                   Z-ADD     0             S1OPZ
037900940223     C*
038000940607     C                   UPDATE    S1
038100940223     C*
038200940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0383009401313    C     $ESCI         IFEQ      *OFF
038400940607     C                   READC     S1                                     21
038500940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0386009402014    C     *IN21         IFEQ      *ON
038700940201     C                   MOVE      WINZS1        $INZS1
038800940204     C* calcolo pagina a cui deve posizionarsi
038900940224     C                   EXSR      CLCPAG
0390009402014-   C                   ENDIF
0391009402013-   C                   ENDIF
039200940131     C*
0393009401272-   C                   ENDDO
039400940127     C*
0395009401311-   C                   ENDIF
039600940131     C*
039700940127     C                   ENDSR
039800940127     C/EJECT
039900940127     C************************************************************
040000940131     C* CONTROLLO CAMPI I/O RIGA LISTA
040100940127     C************************************************************
040200940131     C     RECS1         BEGSR
040300940131     C*
040400940201     C* reset indicatori DSPATR
040500940201     C                   MOVE      *ALL'0'       IN4049           10
040600940201     C                   MOVEA     IN4049        *IN(40)
040700940201     C*
040800940131     C                   ENDSR
040900940131     C************************************************************
041000940131     C* GESTIONE OPZIONI LISTA
041100940131     C************************************************************
041200940131     C     OPZS1         BEGSR
041300060927     C*******
041400060927     C*******            RESET                   tabds
041500060927     C*******            MOVEL     S1OPZ         xtaopz
041600060927     C*******            MOVE      *ZERO         xtaret
041700060927     C*******            MOVE      *ZERO         xtaopr
041800060927     C*******            MOVEl     S1COD         xtakey
041900060927     C*******            MOVE      *BLANKS       KPJBU
042000060927     C*******            MOVEL     tabds         KPJBU
042100060927$004 C*******            CALL      'TRTB80R2'
042200060927     C*******            PARM                    KPJBA
042300060927     C*******            MOVEL     KPJBU         tabds
042400060927     C*******
042500060927     C* ritorno da PGM gestione
042600060927     C*******            EXSR      GESRET
042700060927     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0428000609272    C******IN99         IFEQ      *ON
042900060927     C*******            SETON                                        40
0430000609272-   C*******            ENDIF
043100060927     C*
043200060927     C                   if        S1OPZ = 1
043300060927     c                   movel     s1cod         kpjbu
043400060927     C                   MOVE      *ON           $ESCI
043500060927     C                   MOVE      *ON           $FINE
043600060927     c                   end
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
050000030113     C*
050100060927     C     KTBE          KLIST
050200060927     C                   KFLD                    TBECOD
050300030113     C* Indici
050400030113     C                   Z-ADD     0             X                 3 0
050500030113     C                   Z-ADD     0             Y                 3 0
050600940506     C*
050700940506     C* Reperimento tasti di funzione
050800060927     C                   MOVE      'SOD'         tbecod
050900940127     C*
051000940117     C                   ENDSR
051100030113     C************************************************************
