000100141127      * tabella x CTL.FATTURE PTN
000200141127      *
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
002100170705     FTFNLVP5L  IF   E           K DISK
002200170705     Fazorg01L  IF   E           K DISK
002300170705     FFNLVP18D  CF   E             WORKSTN
002400940607     F                                     SFILE(S1:S1NRR)
002500940201     F                                     INFDS(DSFMT)
002600940128     D*----------------------------------------------------*
002700940211     D* Passaggio Parametri
002800940211     D KPJBA         E DS
002900030113      *-------------
003000940325     D* Parametri in ricezione
003100030113     D  TABDS          DS
003200030729     D  XTAOPZ                 1      1
003300030729     D  XTARET                 2      2
003400030729     D  XTAOPR                 3      3
003500030729     D  XTAERR                 4      4
003600030729     D  XTAFIL                10     12  0
003700030729     D  XTADDA                13     20  0
003800030729     D  XTAADA                21     28  0
003900940211     D*-------------
004000940211     D DSFMT           DS           512
004100940506     D  $TASTO               369    369
004200940211     D  NRG                  370    370
004300940211     D  NCL                  371    371
004400940211     D  SFLNRR               378    379B 0
004500030728      *
004600030728     Ddataiso          s               d   datfmt(*iso)
004700030728     Ddataeur          s               d   datfmt(*eur)
004800940207     D*-------------
004900940207     D* Nome PGM a video
005000940207     D                 DS
005100940207     D  PROGR                  1     10
005200940207     D  ASTER1                 1      1    INZ('*')
005300940207     D  SIGLA                  2      9
005400940207     D  ASTER2                10     10    INZ('*')
005500940127     D*-------------
005600940127     D* Reperimento nome PGM
005700940127     D STATUS         SDS           333
005800940127     D  DSPGM            *PROC
005900030113     D*-------------
006000030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006100030113$003 D S1NRR           S                   Like(C1rcd)
006200030113$003 D WSfl            S                   Like(C1nrr)
006300030113$003 D Wmax            S                   Like(C1rcd)
006400030113$003 D Wpag            S                   Like(C1rcd)
006500030113$003 D Winzs1          S                   Like($inzs1)
006600940207     D*-------------
006700940211     D* COSTANTI
006800940211     D*-------------
006900030113     D SFLPAG          C                   CONST(11)
007000940314     D* dimensione della schiera $MS1
007100940506     D*
007200940506     D* Tasti di funzione
007300940506     D F01             C                   CONST(X'31')
007400940506     D F02             C                   CONST(X'32')
007500940506     D F03             C                   CONST(X'33')
007600940506     D F04             C                   CONST(X'34')
007700940506     D F05             C                   CONST(X'35')
007800940506     D F06             C                   CONST(X'36')
007900940506     D F07             C                   CONST(X'37')
008000940506     D F08             C                   CONST(X'38')
008100940506     D F09             C                   CONST(X'39')
008200940506     D F10             C                   CONST(X'3A')
008300940506     D F11             C                   CONST(X'3B')
008400940506     D F12             C                   CONST(X'3C')
008500940506     D F13             C                   CONST(X'B1')
008600940506     D F14             C                   CONST(X'B2')
008700940506     D F15             C                   CONST(X'B3')
008800940506     D F16             C                   CONST(X'B4')
008900940506     D F17             C                   CONST(X'B5')
009000940506     D F18             C                   CONST(X'B6')
009100940506     D F19             C                   CONST(X'B7')
009200940506     D F20             C                   CONST(X'B8')
009300940506     D F21             C                   CONST(X'B9')
009400940506     D F22             C                   CONST(X'BA')
009500940506     D F23             C                   CONST(X'BB')
009600940506     D F24             C                   CONST(X'BC')
009700940506     D ENTER           C                   CONST(X'F1')
009800940506     D ROLDWN          C                   CONST(X'F4')
009900940506     D ROLLUP          C                   CONST(X'F5')
010000940127     C*----------------------------------------------------*
010100940127     C*                MAIN LINE PROGRAM
010200940127     C*----------------------------------------------------*
010300940223     C* inizializzazione variabili
010400940223     C                   EXSR      INZVAR
010500940223     C*
010600940223     C     $FINE         DOWEQ     *OFF
010700940131     C     $GEST         CASEQ     'S1'          GESS1
010800940117     C                   END
010900940117     C                   END
011000940325     C* fine programma
011100940325     C                   SETON                                            LR
011200030113     C************************************************************
011300030113     C* INIZIALIZZAZIONE VARIABILI
011400030113     C************************************************************
011500030113     C     INZVAR        BEGSR
011600030113     C*
011700030113     C* Pulizia campi e indicatori
011800030113     C                   MOVE      *ALL'0'       IN4049           10
011900030113     C                   MOVEA     IN4049        *IN(40)
012000030113     C                   CLEAR                   S1OPZ
012100030113     C* Variabili per gestione videate
012200030113     C*
012300030113     C                   MOVE      *OFF          $FINE
012400030113     C                   MOVE      *OFF          $INZS1
012500030113     C                   MOVE      *OFF          $EFILE
012600030113     C                   MOVE      *OFF          $ESCI
012700030113     C                   MOVE      *OFF          $RCDOK
012800030113     C                   Z-ADD     0             $ULKS1            3 0
012900030113     C*
013000030113     C                   MOVE      *ON           $INZS1
013100030113     C                   MOVE      'S1'          $GEST
013200030113     C*
013300030113     C* Variabili appoggio
013400030114     C                   Z-ADD     1             WPAG
013500030113     c*
013600030113     C                   ENDSR
013700940127     C************************************************************
013800940131     C* GESTIONE LISTA
013900940127     C************************************************************
014000940127     C     GESS1         BEGSR
014100030113     C*
014200940223     C* inizializzazione videata
014300940223     C     $INZS1        IFEQ      *ON
014400940127     C                   EXSR      INZS1
014500940223     C                   MOVE      *OFF          $INZS1
014600940127     C                   ENDIF
014700030113     C*
014800030113     C* emissione piede videata
014900030113     C                   WRITE     Z1
015000030113     C* Non ci sono records
015100940223     C     WMAX          IFEQ      0
015200940607     C                   WRITE     D1
015300030114     C                   Else
015400030114     C     Wsfl          IFgt      0
015500030114     C                   Z-ADD     wsfl          C1RCD
015600030114     C                   Else
015700030114     C     Wpag          IFgt      0
015800040115     C     pagine        andgt     0
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
020500170705     c     *loval        setll     TFNLVP5L
020600940608    >C                   EXSR      REDANA
020700030113     C* Carico il SFL
020800940127     C                   EXSR      ROLS1
020900030113     C*
021000170718     c                   if        xtaopr <> '1'
021100030114     C                   Z-ADD     1             WPAG
021200170718     c                   end
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
022800170705     c                   move      VP5FGS        s1fil
022900170705     c                   move      VP5DIMM       h1dda
023000170705     c                   move      VP5DIMM       dataiso
023100030728     c                   move      dataiso       dataeur
023200030728     c                   move      dataeur       s1dda
023300170705     c     VP5FGS        chain     azorg01l
023400170705     c                   if        %found(azorg01l)
023500170705     c                   movel     orgDES        s1DES
023600170705     c                   end
023700940127     C*
023800940127     C                   ADD       1             S1NRR
023900940127     C                   ADD       1             Y
024000940127     C*
024100940607     C                   WRITE     S1
024200940131     C*
024300940316    >C                   EXSR      REDANA
024400940128     C*
0245009401271-   C                   ENDDO
024600940127     C*
024700940223     C                   Z-ADD     S1NRR         WMAX                 30
024800940127     C*
024900940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
025000940127     C*
025100030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
025200940127     C                   MVR                     RESTO             3 0
025300030114     C     PAGINE        MULT      SFLPAG        C1RCD
0254000301141    C     RESTO         IFGT      0
025500030114     C                   ADD       1             C1RCD
0256000301141E   C                   ELSE
025700030114     C                   SUB       SFLPAG        C1RCD
025800030114     C                   ADD       1             C1RCD
0259000301141-   C                   ENDIF
026000940128     C*
026100940127     C                   ENDSR
026200940128     C************************************************************
026300940131     C* LETTURA RCD ARCHIVIO PILOTA
026400940128     C************************************************************
026500940607     C     REDANA        BEGSR
026600940128     C*
026700940131     C                   MOVEL     *OFF          $EFILE
026800940131     C                   MOVEL     *OFF          $RCDOK
026900940131     C*
0270009401311    C     $EFILE        DOUEQ     *ON
027100940131     C     $RCDOK        OREQ      *ON
027200030728     C*
027300170705     c                   Read      TFNLVP5L
027400170705     c                   if        %eof(TFNLVP5L)
027500030113     C                   MOVEL     *ON           $EFILE
027600030113     C                   MOVE      $EFILE        *IN33
027700030113     c                   else
027800030113     C                   MOVE      *ON           $RCDOK
027900030113     c                   end
028000940131     C*
0281009401311-   C                   ENDDO
028200940131     C*
028300940131     C                   ENDSR
028400940224     C************************************************************
028500940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
028600940224     C************************************************************
028700940224     C     CLCPAG        BEGSR
028800940224     C* Input :
028900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
029000940224     C*          cursore
029100940224     C* - SFLPAG = numero rcd per pagina sfl
029200940224     C* Output :
029300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
029400940224     C*
029500940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
029600940224     C                   MVR                     RESTO             3 0
029700940224     C     RESTO         IFGT      0
029800940224     C                   ADD       1             PAGINE
029900940224     C                   ENDIF
030000940224     C     PAGINE        MULT      SFLPAG        WPAG
030100940224     C*
030200940224     C                   ENDSR
030300940309     C************************************************************
030400940309     C* GESTIONE F03 VIDEO S1
030500940309     C************************************************************
030600940309     C     F03S1         BEGSR
030700940309     C*
030800940309     C                   MOVE      *ON           $FINE
030900940325     C* fine programma
031000940309     C                   ENDSR
031100940309     C/EJECT
031200940309     C************************************************************
031300940309     C* GESTIONE F10 VIDEO S1
031400940314     c* AGGIUNTA RECORD
031500940309     C************************************************************
031600940309     C     F10S1         BEGSR
031700940309     C*
031800030113     C                   RESET                   tabds
031900030729     C                   MOVEL     '1'           xtaopz
032000030113     C                   MOVE      *ZERO         xtaret
032100030113     C                   MOVE      *ZERO         xtaopr
032200030729     C                   MOVEl     *zero         xtafil
032300030729     C                   MOVEl     *zero         xtadda
032400030729     C                   MOVEl     *zero         xtaada
032500030113     C                   MOVE      *BLANKS       KPJBU
032600030113     C                   MOVEL     tabds         KPJBU
032700170705$004 C                   CALL      'FNLVP18R1'
032800030113     C                   PARM                    KPJBA
032900030114     C                   MOVEL     KPJBU         tabds
033000030113      *
033100940309     C* ritorno da PGM gestione
033200940309     C                   EXSR      GESRET
033300940309     C     WINZS1        IFEQ      *ON
033400940309     C                   MOVE      *ON           $INZS1
033500940309     C* se esistevano già righe sul sfl
033600940309     C* calcolo pagina a cui deve posizionarsi
033700940309     C     WSFL          IFGT      0
033800940309     C                   EXSR      CLCPAG
033900940309     C* altrimenti carico solo la 1a pagina
034000940309     C                   ELSE
034100940309     C                   Z-ADD     SFLPAG        WPAG
034200940309     C                   ENDIF
034300940309     C                   ENDIF
034400940309     C*
034500940309     C                   ENDSR
034600940128     C************************************************************
034700940131     C* CONTROLLO TESTATA LISTA
034800940128     C************************************************************
034900940131     C     CTRC1         BEGSR
035000940128     C*
035100940201     C                   MOVE      *OFF          *IN99
035200940131     C*
035300940202     C                   ENDSR
035400940131     C************************************************************
035500940131     C* CONTROLLO OPZIONI LISTA
035600940131     C************************************************************
035700940131     C     CTRS1         BEGSR
035800940131     C*
035900940202     C                   MOVEL     *OFF          $ESCI
036000940201     C                   SETOFF                                       99
036100940131     C                   Z-ADD     0             S1OPZ
036200940131     C*
036300940127     C* Leggo il sfl solo se ci sono rcd
0364009401311    C     WMAX          IFGT      0
036500940607     C                   READC     S1                                     21
036600940127     C*
036700940131     C* esce se fine sfl o errore che richiede l'uscita
0368009401312    C     *IN21         DOWEQ     *OFF
036900940131     C     $ESCI         ANDEQ     *OFF
037000940201     C                   Z-ADD     S1NRR         C1RCD
037100940131     C* ctrl su riga
037200940131     C                   EXSR      RECS1
037300940131     C* gestione opzioni
0374009401313    C     S1OPZ         IFNE      0
037500940201     C     *IN99         ANDEQ     *OFF
037600940131     C                   EXSR      OPZS1
0377009401273-   C                   ENDIF
037800940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0379009402013    C     *IN99         IFEQ      *ON
038000940201     C     $ESCI         OREQ      *ON
038100940131     C                   MOVE      *ON           *IN32
038200940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038300940204     C* la ripristinerò a conclusione del ciclo di READC
038400940223     C                   MOVE      *OFF          $INZS1
0385009402233-   C                   ENDIF
038600940223     C*
038700940131     C                   Z-ADD     0             S1OPZ
038800940223     C*
038900940607     C                   UPDATE    S1
039000940223     C*
039100940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0392009401313    C     $ESCI         IFEQ      *OFF
039300940607     C                   READC     S1                                     21
039400940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0395009402014    C     *IN21         IFEQ      *ON
039600940201     C                   MOVE      WINZS1        $INZS1
039700940204     C* calcolo pagina a cui deve posizionarsi
039800940224     C                   EXSR      CLCPAG
0399009402014-   C                   ENDIF
0400009402013-   C                   ENDIF
040100940131     C*
0402009401272-   C                   ENDDO
040300940127     C*
0404009401311-   C                   ENDIF
040500940131     C*
040600940127     C                   ENDSR
040700940127     C/EJECT
040800940127     C************************************************************
040900940131     C* CONTROLLO CAMPI I/O RIGA LISTA
041000940127     C************************************************************
041100940131     C     RECS1         BEGSR
041200940131     C*
041300940201     C* reset indicatori DSPATR
041400940201     C                   MOVE      *ALL'0'       IN4049           10
041500940201     C                   MOVEA     IN4049        *IN(40)
041600940201     C*
041700940131     C                   ENDSR
041800940131     C************************************************************
041900940131     C* GESTIONE OPZIONI LISTA
042000940131     C************************************************************
042100940131     C     OPZS1         BEGSR
042200940201     C*
042300030113     C                   RESET                   tabds
042400030113     C                   MOVEL     S1OPZ         xtaopz
042500030113     C                   MOVE      *ZERO         xtaret
042600030113     C                   MOVE      *ZERO         xtaopr
042700030729     C                   MOVEl     s1fil         xtafil
042800030729     C                   MOVEl     h1dda         xtadda
042900940715     C                   MOVE      *BLANKS       KPJBU
043000030113     C                   MOVEL     tabds         KPJBU
043100170705$004 C                   CALL      'FNLVP18R1'
043200940607     C                   PARM                    KPJBA
043300030114     C                   MOVEL     KPJBU         tabds
043400940201     C*
043500940223     C* ritorno da PGM gestione
043600940223     C                   EXSR      GESRET
043700940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0438009402252    C     *IN99         IFEQ      *ON
043900940223     C                   SETON                                        40
0440009402252-   C                   ENDIF
044100940225     C*
044200940131     C                   ENDSR
044300940223     C/EJECT
044400940223     C************************************************************
044500940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
044600940223     C************************************************************
044700940223     C     GESRET        BEGSR
044800940223     C*
044900940223     C* modo di ritorno
045000940223     C*
0451009402231    C                   SELECT
045200940314    >C* << questi modi di utilizzo dei valori di ritorno dal
045300940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
045400940314    >C*    proposte, normalmente sempre valide, ma modificabili
045500940314    >C*    per situazioni particolari >>
045600940223     C* 1 = F3
045700030113    >C     xtaret        WHENEQ    '1'
045800940224     C                   MOVE      *ON           $ESCI
045900940223     C                   MOVE      *ON           $FINE
046000940223     C* 2 = F12
046100030113    >C     xtaret        WHENEQ    '2'
046200940223     C                   MOVE      *ON           $ESCI
046300940223     C*
0464009402231-   C                   ENDSL
046500940223     C*
046600940223     C* operazione eseguite dal pgm chiamato
046700940223     C*
0468009402231    C                   SELECT
046900940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
047000030113    >C     xtaopr        WHENEQ    '1'
047100940223     C                   MOVE      *ON           WINZS1
047200170718     C                   clear                   wsfl
0473009402231-   C                   ENDSL
047400940223     C*
047500940223     C* funzione non eseguibile per errore :
047600940223     C*
0477009402231    C                   SELECT
047800940223     C* 1 = funzione richiamata chiusa in errore
047900940316    >C*  eventualmente gestire altri codici di errore
048000030113    >C     xtaerr        WHENEQ    '1'
048100940223     C                   MOVE      *ON           $ESCI
048200940223     C                   SETON                                        5299
048300940223     C*
0484009402231-   C                   ENDSL
048500940223     C*
048600940223     C                   ENDSR
048700940223     C/EJECT
048800940131     C************************************************************
048900940131     C* OPERAZIONI INIZIALI
049000940131     C************************************************************
049100940131     C     *INZSR        BEGSR
049200030113     C*
049300030113     C* Reperimento parametri
049400030113     C     *ENTRY        PLIST
049500030113     C                   PARM                    KPJBA
049600030113     C*
049700030113     C* Variabili per gestione videate
049800030113     C                   MOVE      *BLANK        $GEST             2
049900030113     C                   MOVE      *BLANK        $FINE             1
050000030113     C                   MOVE      *BLANK        $INZS1            1
050100030113     C                   MOVE      *BLANK        $EFILE            1
050200030113     C                   MOVE      *BLANK        $ESCI             1
050300030113     C                   MOVE      *BLANK        $RCDOK            1
050400030113     C* Indici
050500030113     C                   Z-ADD     0             X                 3 0
050600030113     C                   Z-ADD     0             Y                 3 0
050700940127     C*
050800940117     C                   ENDSR
050900030113     C************************************************************
