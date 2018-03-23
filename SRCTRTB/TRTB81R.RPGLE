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
002100030113     FTABEL00F  IF   E           K DISK
002200030122     FTRTB81D   CF   E             WORKSTN
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
005300940607     D* descrizione tabella
005400030122$003 D EXTDS         E DS                  EXTNAME(DS8D)
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
009600940207     I*-------------
009700940607     IS1
009800030122$015 I              S1DES                       §8DDES
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
020300030113     c     kTBL          setll     TABEL00F
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
022600030113     c                   movel     tblkey        s1cod
022700030113     c                   movel     tbluni        extds
022800030113     C*
022900030113     C* Se Tariffa per Cooperativa
023000030122     c                   move      §8dPA         S1PA
023100030115     c                   clear                   S1Ann
023200030115     c                   if        tblflg ='*'
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
027100030113     c     kTBL          Reade     TABEL00F
027200030113     c                   if        %eof(Tabel00F)
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
032300030122$004 C                   CALL      'TRTB81R2'
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
035800940131     C*
035900940127     C* Leggo il sfl solo se ci sono rcd
0360009401311    C     WMAX          IFGT      0
036100940607     C                   READC     S1                                     21
036200940127     C*
036300940131     C* esce se fine sfl o errore che richiede l'uscita
0364009401312    C     *IN21         DOWEQ     *OFF
036500940131     C     $ESCI         ANDEQ     *OFF
036600940201     C                   Z-ADD     S1NRR         C1RCD
036700940131     C* ctrl su riga
036800940131     C                   EXSR      RECS1
036900940131     C* gestione opzioni
0370009401313    C     S1OPZ         IFNE      0
037100940201     C     *IN99         ANDEQ     *OFF
037200940131     C                   EXSR      OPZS1
0373009401273-   C                   ENDIF
037400940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0375009402013    C     *IN99         IFEQ      *ON
037600940201     C     $ESCI         OREQ      *ON
037700940131     C                   MOVE      *ON           *IN32
037800940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
037900940204     C* la ripristinerò a conclusione del ciclo di READC
038000940223     C                   MOVE      *OFF          $INZS1
0381009402233-   C                   ENDIF
038200940223     C*
038300940131     C                   Z-ADD     0             S1OPZ
038400940223     C*
038500940607     C                   UPDATE    S1
038600940223     C*
038700940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0388009401313    C     $ESCI         IFEQ      *OFF
038900940607     C                   READC     S1                                     21
039000940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0391009402014    C     *IN21         IFEQ      *ON
039200940201     C                   MOVE      WINZS1        $INZS1
039300940204     C* calcolo pagina a cui deve posizionarsi
039400940224     C                   EXSR      CLCPAG
0395009402014-   C                   ENDIF
0396009402013-   C                   ENDIF
039700940131     C*
0398009401272-   C                   ENDDO
039900940127     C*
0400009401311-   C                   ENDIF
040100940131     C*
040200940127     C                   ENDSR
040300940127     C/EJECT
040400940127     C************************************************************
040500940131     C* CONTROLLO CAMPI I/O RIGA LISTA
040600940127     C************************************************************
040700940131     C     RECS1         BEGSR
040800940131     C*
040900940201     C* reset indicatori DSPATR
041000940201     C                   MOVE      *ALL'0'       IN4049           10
041100940201     C                   MOVEA     IN4049        *IN(40)
041200940201     C*
041300940131     C                   ENDSR
041400940131     C************************************************************
041500940131     C* GESTIONE OPZIONI LISTA
041600940131     C************************************************************
041700940131     C     OPZS1         BEGSR
041800940201     C*
041900030113     C                   RESET                   tabds
042000030113     C                   MOVEL     S1OPZ         xtaopz
042100030113     C                   MOVE      *ZERO         xtaret
042200030113     C                   MOVE      *ZERO         xtaopr
042300030113     C                   MOVEl     S1COD         xtakey
042400940715     C                   MOVE      *BLANKS       KPJBU
042500030113     C                   MOVEL     tabds         KPJBU
042600030122$004 C                   CALL      'TRTB81R2'
042700940607     C                   PARM                    KPJBA
042800030114     C                   MOVEL     KPJBU         tabds
042900940201     C*
043000940223     C* ritorno da PGM gestione
043100940223     C                   EXSR      GESRET
043200940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0433009402252    C     *IN99         IFEQ      *ON
043400940223     C                   SETON                                        40
0435009402252-   C                   ENDIF
043600940225     C*
043700940131     C                   ENDSR
043800940223     C/EJECT
043900940223     C************************************************************
044000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
044100940223     C************************************************************
044200940223     C     GESRET        BEGSR
044300940223     C*
044400940223     C* modo di ritorno
044500940223     C*
0446009402231    C                   SELECT
044700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
044800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
044900940314    >C*    proposte, normalmente sempre valide, ma modificabili
045000940314    >C*    per situazioni particolari >>
045100940223     C* 1 = F3
045200030113    >C     xtaret        WHENEQ    '1'
045300940224     C                   MOVE      *ON           $ESCI
045400940223     C                   MOVE      *ON           $FINE
045500940223     C* 2 = F12
045600030113    >C     xtaret        WHENEQ    '2'
045700940223     C                   MOVE      *ON           $ESCI
045800940223     C*
0459009402231-   C                   ENDSL
046000940223     C*
046100940223     C* operazione eseguite dal pgm chiamato
046200940223     C*
0463009402231    C                   SELECT
046400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
046500030113    >C     xtaopr        WHENEQ    '1'
046600940223     C                   MOVE      *ON           WINZS1
046700940223     C*
0468009402231-   C                   ENDSL
046900940223     C*
047000940223     C* funzione non eseguibile per errore :
047100940223     C*
0472009402231    C                   SELECT
047300940223     C* 1 = funzione richiamata chiusa in errore
047400940316    >C*  eventualmente gestire altri codici di errore
047500030113    >C     xtaerr        WHENEQ    '1'
047600940223     C                   MOVE      *ON           $ESCI
047700940223     C                   SETON                                        5299
047800940223     C*
0479009402231-   C                   ENDSL
048000940223     C*
048100940223     C                   ENDSR
048200940223     C/EJECT
048300940131     C************************************************************
048400940131     C* OPERAZIONI INIZIALI
048500940131     C************************************************************
048600940131     C     *INZSR        BEGSR
048700030113     C*
048800030113     C* Reperimento parametri
048900030113     C     *ENTRY        PLIST
049000030113     C                   PARM                    KPJBA
049100030113     C*
049200030113     C* Variabili per gestione videate
049300030113     C                   MOVE      *BLANK        $GEST             2
049400030113     C                   MOVE      *BLANK        $FINE             1
049500030113     C                   MOVE      *BLANK        $INZS1            1
049600030113     C                   MOVE      *BLANK        $EFILE            1
049700030113     C                   MOVE      *BLANK        $ESCI             1
049800030113     C                   MOVE      *BLANK        $RCDOK            1
049900030113     C*
050000030113     C     KTBL          KLIST
050100030113     C                   KFLD                    TBLKUT
050200030113     C                   KFLD                    TBLCOD
050300030113     C* Indici
050400030113     C                   Z-ADD     0             X                 3 0
050500030113     C                   Z-ADD     0             Y                 3 0
050600940506     C*
050700940506     C* Reperimento tasti di funzione
050800030113     C                   MOVE      1             tblkut
050900030122     C                   MOVE      '8D'          tblcod
051000940127     C*
051100940117     C                   ENDSR
051200030113     C************************************************************
