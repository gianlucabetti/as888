000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200940224      *
000300940307      *  21           GENERICO OPERAZIONI I/O
000400940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000500940224      *  30           SFLDSP
000600940224      * N31           SFLCLR
000700940224      *  31           SFLDSPCTL
000800940224      *  32           SFLNXTCHG
000900940224      *  33           SFLEND
001000940224      *  39           OF PRTF
001100940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001200940608      *  Specificare l'uso dei singoli indicatori
001300940224      *  50 <---> 98  ERRORI SU VIDEO
001400940608      *  Specificare l'uso dei singoli indicatori
001500940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001600940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
001700940224      *  99           INDIC. GENERALE DI ERRORE
001800940128     F*----------------------------------------------------*
001900150428     FTNcld00F  IF   E           K DISK
002000150428     FFIEU66D   CF   E             WORKSTN
002100940607     F                                     SFILE(S1:S1NRR)
002200940201     F                                     INFDS(DSFMT)
002300940128     D*----------------------------------------------------*
002400940211     D* Passaggio Parametri
002500940211     D KPJBA         E DS
002600030113      *-------------
002700940325     D* Parametri in ricezione
002800030113     D  TABDS          DS
002900030729     D  XTAOPZ                 1      1
003000030729     D  XTARET                 2      2
003100030729     D  XTAOPR                 3      3
003200030729     D  XTAERR                 4      4
003300150428     D  XTACOD                 5     14
003400150428     D  XTADEP                15     18  0
003500150428     D  XTADDA                19     26  0
003600150428     D  XTAADA                27     34  0
003700940211     D*-------------
003800940211     D DSFMT           DS           512
003900940506     D  $TASTO               369    369
004000940211     D  NRG                  370    370
004100940211     D  NCL                  371    371
004200940211     D  SFLNRR               378    379B 0
004300030728      *
004400030728     Ddataiso          s               d   datfmt(*iso)
004500030728     Ddataeur          s               d   datfmt(*eur)
004600940207     D*-------------
004700940207     D* Nome PGM a video
004800940207     D                 DS
004900940207     D  PROGR                  1     10
005000940207     D  ASTER1                 1      1    INZ('*')
005100940207     D  SIGLA                  2      9
005200940207     D  ASTER2                10     10    INZ('*')
005300940127     D*-------------
005400940127     D* Reperimento nome PGM
005500940127     D STATUS         SDS           333
005600940127     D  DSPGM            *PROC
005700030113     D*-------------
005800030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005900030113$003 D S1NRR           S                   Like(C1rcd)
006000030113$003 D WSfl            S                   Like(C1nrr)
006100030113$003 D Wmax            S                   Like(C1rcd)
006200030113$003 D Wpag            S                   Like(C1rcd)
006300030113$003 D Winzs1          S                   Like($inzs1)
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
009800940127     C*----------------------------------------------------*
009900940127     C*                MAIN LINE PROGRAM
010000940127     C*----------------------------------------------------*
010100940223     C* inizializzazione variabili
010200940223     C                   EXSR      INZVAR
010300940223     C*
010400940223     C     $FINE         DOWEQ     *OFF
010500940131     C     $GEST         CASEQ     'S1'          GESS1
010600940117     C                   END
010700940117     C                   END
010800940325     C* fine programma
010900940325     C                   SETON                                            LR
011000030113     C************************************************************
011100030113     C* INIZIALIZZAZIONE VARIABILI
011200030113     C************************************************************
011300030113     C     INZVAR        BEGSR
011400030113     C*
011500030113     C* Pulizia campi e indicatori
011600030113     C                   MOVE      *ALL'0'       IN4049           10
011700030113     C                   MOVEA     IN4049        *IN(40)
011800030113     C                   CLEAR                   S1OPZ
011900030113     C* Variabili per gestione videate
012000030113     C*
012100030113     C                   MOVE      *OFF          $FINE
012200030113     C                   MOVE      *OFF          $INZS1
012300030113     C                   MOVE      *OFF          $EFILE
012400030113     C                   MOVE      *OFF          $ESCI
012500030113     C                   MOVE      *OFF          $RCDOK
012600030113     C                   Z-ADD     0             $ULKS1            3 0
012700030113     C*
012800030113     C                   MOVE      *ON           $INZS1
012900030113     C                   MOVE      'S1'          $GEST
013000030113     C*
013100030113     C* Variabili appoggio
013200030114     C                   Z-ADD     1             WPAG
013300030113     c*
013400030113     C                   ENDSR
013500940127     C************************************************************
013600940131     C* GESTIONE LISTA
013700940127     C************************************************************
013800940127     C     GESS1         BEGSR
013900030113     C*
014000940223     C* inizializzazione videata
014100940223     C     $INZS1        IFEQ      *ON
014200940127     C                   EXSR      INZS1
014300940223     C                   MOVE      *OFF          $INZS1
014400940127     C                   ENDIF
014500030113     C*
014600030113     C* emissione piede videata
014700030113     C                   WRITE     Z1
014800030113     C* Non ci sono records
014900940223     C     WMAX          IFEQ      0
015000940607     C                   WRITE     D1
015100030114     C                   Else
015200030114     C     Wsfl          IFgt      0
015300030114     C                   Z-ADD     wsfl          C1RCD
015400030114     C                   Else
015500030114     C     Wpag          IFgt      0
015600040115     C     pagine        andgt     0
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
020300150428     c     *loval        setll     TnCLD00f
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
022600150428     c                   movel     cldCOD        s1cod
022700150428     c                   move      cldDEP        s1dep
022800150428     c                   move      clddda        h1dda
022900150428     c                   move      clddda        dataiso
023000150428     c                   move      dataiso       dataeur
023100150428     c                   move      dataeur       s1dda
023200150428     c                   move      cldada        h1ada
023300150428     c                   move      cldada        dataiso
023400150428     c                   move      dataiso       dataeur
023500150428     c                   move      dataeur       s1ada
023600150428     c                   movel     cldANN        s1ANN
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
027300150428     c                   Read      TnCLD00f
027400150428     c                   if        %eof(TnCLD00f)
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
032200150428     C                   MOVEl     *blank        xtacod
032300150428     C                   MOVEl     *zero         xtadep
032400030729     C                   MOVEl     *zero         xtadda
032500030729     C                   MOVEl     *zero         xtaada
032600030113     C                   MOVE      *BLANKS       KPJBU
032700030113     C                   MOVEL     tabds         KPJBU
032800150428$004 C                   CALL      'FIEU66R1'
032900030113     C                   PARM                    KPJBA
033000030114     C                   MOVEL     KPJBU         tabds
033100030113      *
033200940309     C* ritorno da PGM gestione
033300940309     C                   EXSR      GESRET
033400940309     C     WINZS1        IFEQ      *ON
033500940309     C                   MOVE      *ON           $INZS1
033600940309     C* se esistevano già righe sul sfl
033700940309     C* calcolo pagina a cui deve posizionarsi
033800940309     C     WSFL          IFGT      0
033900940309     C                   EXSR      CLCPAG
034000940309     C* altrimenti carico solo la 1a pagina
034100940309     C                   ELSE
034200940309     C                   Z-ADD     SFLPAG        WPAG
034300940309     C                   ENDIF
034400940309     C                   ENDIF
034500940309     C*
034600940309     C                   ENDSR
034700940128     C************************************************************
034800940131     C* CONTROLLO TESTATA LISTA
034900940128     C************************************************************
035000940131     C     CTRC1         BEGSR
035100940128     C*
035200940201     C                   MOVE      *OFF          *IN99
035300940131     C*
035400940202     C                   ENDSR
035500940131     C************************************************************
035600940131     C* CONTROLLO OPZIONI LISTA
035700940131     C************************************************************
035800940131     C     CTRS1         BEGSR
035900940131     C*
036000940202     C                   MOVEL     *OFF          $ESCI
036100940201     C                   SETOFF                                       99
036200940131     C                   Z-ADD     0             S1OPZ
036300940131     C*
036400940127     C* Leggo il sfl solo se ci sono rcd
0365009401311    C     WMAX          IFGT      0
036600940607     C                   READC     S1                                     21
036700940127     C*
036800940131     C* esce se fine sfl o errore che richiede l'uscita
0369009401312    C     *IN21         DOWEQ     *OFF
037000940131     C     $ESCI         ANDEQ     *OFF
037100940201     C                   Z-ADD     S1NRR         C1RCD
037200940131     C* ctrl su riga
037300940131     C                   EXSR      RECS1
037400940131     C* gestione opzioni
0375009401313    C     S1OPZ         IFNE      0
037600940201     C     *IN99         ANDEQ     *OFF
037700940131     C                   EXSR      OPZS1
0378009401273-   C                   ENDIF
037900940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0380009402013    C     *IN99         IFEQ      *ON
038100940201     C     $ESCI         OREQ      *ON
038200940131     C                   MOVE      *ON           *IN32
038300940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038400940204     C* la ripristinerò a conclusione del ciclo di READC
038500940223     C                   MOVE      *OFF          $INZS1
0386009402233-   C                   ENDIF
038700940223     C*
038800940131     C                   Z-ADD     0             S1OPZ
038900940223     C*
039000940607     C                   UPDATE    S1
039100940223     C*
039200940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0393009401313    C     $ESCI         IFEQ      *OFF
039400940607     C                   READC     S1                                     21
039500940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0396009402014    C     *IN21         IFEQ      *ON
039700940201     C                   MOVE      WINZS1        $INZS1
039800940204     C* calcolo pagina a cui deve posizionarsi
039900940224     C                   EXSR      CLCPAG
0400009402014-   C                   ENDIF
0401009402013-   C                   ENDIF
040200940131     C*
0403009401272-   C                   ENDDO
040400940127     C*
0405009401311-   C                   ENDIF
040600940131     C*
040700940127     C                   ENDSR
040800940127     C/EJECT
040900940127     C************************************************************
041000940131     C* CONTROLLO CAMPI I/O RIGA LISTA
041100940127     C************************************************************
041200940131     C     RECS1         BEGSR
041300940131     C*
041400940201     C* reset indicatori DSPATR
041500940201     C                   MOVE      *ALL'0'       IN4049           10
041600940201     C                   MOVEA     IN4049        *IN(40)
041700940201     C*
041800940131     C                   ENDSR
041900940131     C************************************************************
042000940131     C* GESTIONE OPZIONI LISTA
042100940131     C************************************************************
042200940131     C     OPZS1         BEGSR
042300940201     C*
042400030113     C                   RESET                   tabds
042500030113     C                   MOVEL     S1OPZ         xtaopz
042600030113     C                   MOVE      *ZERO         xtaret
042700030113     C                   MOVE      *ZERO         xtaopr
042800030729     C                   MOVEl     s1cod         xtacod
042900150428     C                   MOVEl     s1dep         xtadep
043000030729     C                   MOVEl     h1dda         xtadda
043100030729     C                   MOVEl     h1ada         xtaada
043200940715     C                   MOVE      *BLANKS       KPJBU
043300030113     C                   MOVEL     tabds         KPJBU
043400150428$004 C                   CALL      'FIEU66R1'
043500940607     C                   PARM                    KPJBA
043600030114     C                   MOVEL     KPJBU         tabds
043700940201     C*
043800940223     C* ritorno da PGM gestione
043900940223     C                   EXSR      GESRET
044000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0441009402252    C     *IN99         IFEQ      *ON
044200940223     C                   SETON                                        40
0443009402252-   C                   ENDIF
044400940225     C*
044500940131     C                   ENDSR
044600940223     C/EJECT
044700940223     C************************************************************
044800940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
044900940223     C************************************************************
045000940223     C     GESRET        BEGSR
045100940223     C*
045200940223     C* modo di ritorno
045300940223     C*
0454009402231    C                   SELECT
045500940314    >C* << questi modi di utilizzo dei valori di ritorno dal
045600940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
045700940314    >C*    proposte, normalmente sempre valide, ma modificabili
045800940314    >C*    per situazioni particolari >>
045900940223     C* 1 = F3
046000030113    >C     xtaret        WHENEQ    '1'
046100940224     C                   MOVE      *ON           $ESCI
046200940223     C                   MOVE      *ON           $FINE
046300940223     C* 2 = F12
046400030113    >C     xtaret        WHENEQ    '2'
046500940223     C                   MOVE      *ON           $ESCI
046600940223     C*
0467009402231-   C                   ENDSL
046800940223     C*
046900940223     C* operazione eseguite dal pgm chiamato
047000940223     C*
0471009402231    C                   SELECT
047200940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
047300030113    >C     xtaopr        WHENEQ    '1'
047400940223     C                   MOVE      *ON           WINZS1
047500940223     C*
0476009402231-   C                   ENDSL
047700940223     C*
047800940223     C* funzione non eseguibile per errore :
047900940223     C*
0480009402231    C                   SELECT
048100940223     C* 1 = funzione richiamata chiusa in errore
048200940316    >C*  eventualmente gestire altri codici di errore
048300030113    >C     xtaerr        WHENEQ    '1'
048400940223     C                   MOVE      *ON           $ESCI
048500940223     C                   SETON                                        5299
048600940223     C*
0487009402231-   C                   ENDSL
048800940223     C*
048900940223     C                   ENDSR
049000940223     C/EJECT
049100940131     C************************************************************
049200940131     C* OPERAZIONI INIZIALI
049300940131     C************************************************************
049400940131     C     *INZSR        BEGSR
049500030113     C*
049600030113     C* Reperimento parametri
049700030113     C     *ENTRY        PLIST
049800030113     C                   PARM                    KPJBA
049900030113     C*
050000030113     C* Variabili per gestione videate
050100030113     C                   MOVE      *BLANK        $GEST             2
050200030113     C                   MOVE      *BLANK        $FINE             1
050300030113     C                   MOVE      *BLANK        $INZS1            1
050400030113     C                   MOVE      *BLANK        $EFILE            1
050500030113     C                   MOVE      *BLANK        $ESCI             1
050600030113     C                   MOVE      *BLANK        $RCDOK            1
050700030113     C* Indici
050800030113     C                   Z-ADD     0             X                 3 0
050900030113     C                   Z-ADD     0             Y                 3 0
051000940127     C*
051100940117     C                   ENDSR
051200030113     C************************************************************
