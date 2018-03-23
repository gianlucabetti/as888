000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
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
002100050511     FTISE800F  IF   E           K DISK
002200050509     FTISE80D   CF   E             WORKSTN
002300940607     F                                     SFILE(S1:S1NRR)
002400940201     F                                     INFDS(DSFMT)
002500940128     D*----------------------------------------------------*
002600940211     D* Passaggio Parametri
002700940211     D KPJBA         E DS
002800030113      *-------------
002900940325     D* Parametri in ricezione
003000030113     D  TABDS          DS
003100030729     D  XTAOPZ                 1      1
003200030729     D  XTARET                 2      2
003300030729     D  XTAOPR                 3      3
003400030729     D  XTAERR                 4      4
003500050509     D  XTACOD                 5     11  0
003600940211     D*-------------
003700940211     D DSFMT           DS           512
003800940506     D  $TASTO               369    369
003900940211     D  NRG                  370    370
004000940211     D  NCL                  371    371
004100940211     D  SFLNRR               378    379B 0
004200030728      *
004300030728     Ddataiso          s               d   datfmt(*iso)
004400030728     Ddataeur          s               d   datfmt(*eur)
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
005600940607     D* descrizione tabella
005700030113$003 D EXTDS         E DS                  EXTNAME(DS8A)
005800030113     D*-------------
005900030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006000030113$003 D S1NRR           S                   Like(C1rcd)
006100030113$003 D WSfl            S                   Like(C1nrr)
006200030113$003 D Wmax            S                   Like(C1rcd)
006300030113$003 D Wpag            S                   Like(C1rcd)
006400030113$003 D Winzs1          S                   Like($inzs1)
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
015400050511     C     Wsfl          andle     Wmax
015500030114     C                   Z-ADD     wsfl          C1RCD
015600030114     C                   Else
015700050517     C     Wsfl          ifgt      Wmax
015800050517     C                   Z-ADD     wmax          C1RCD
015900050517     c                   else
016000030114     C     Wpag          IFgt      0
016100040115     C     pagine        andgt     0
016200030114     C                   Z-ADD     wpag          C1RCD
016300050511     C                   Else
016400050511     C                   Z-ADD     1             C1RCD
016500030114     C                   EndIF
016600050517     C                   EndIF
016700030114     C                   EndIF
016800030114     C                   ENDIF
016900940127     C*
017000030113     C*              *------------------*
017100940607     C                   EXFMT     C1
017200030113     C*              *------------------*
017300030113     C*
017400940204     C     C1NRR         IFNE      0
017500940204     C                   Z-ADD     C1NRR         WSFL
017600940204     C                   ENDIF
017700940127     C                   Z-ADD     SFLNRR        C1RCD
017800030113     C* Selezioni
0179009401271    C                   SELECT
018000940127     C* F3=Fine
018100940506     C     $TASTO        WHENEQ    F03
018200940309     C                   EXSR      F03S1
018300940131     C* F10=Immissione
018400940506     C     $TASTO        WHENEQ    F10
018500940309     C                   EXSR      F10S1
0186009401271O   C                   OTHER
018700940127     C* CONTROLLO DATI
018800940131     C                   EXSR      CTRC1
018900940201     C     *IN99         IFEQ      *OFF
019000940131     C                   EXSR      CTRS1
019100940131     C                   END
0192009401271-   C                   ENDSL
019300940127     C*
019400940127     C                   ENDSR
019500940224     C/EJECT
019600940127     C************************************************************
019700940131     C* INIZIALIZZAZIONE LISTA
019800940127     C************************************************************
019900940127     C     INZS1         BEGSR
020000940302     C* pulizia SFL
020100940128     C                   SETOFF                                         3031
020200940607     C                   WRITE     C1
020300940128     C                   SETON                                          31
020400940128     C*
020500030113     C* CARICAMENTO SFL totale
020600940201     C                   Z-ADD     0             S1NRR
020700030113     C                   Z-ADD     1             C1RCD
020800940128     C                   Z-ADD     0             WMAX
020900940224     C*
021000940224     C* Posizionamento su file pilota
021100050511     c     *loval        setll     Tise800F
021200940608    >C                   EXSR      REDANA
021300030113     C* Carico il SFL
021400940127     C                   EXSR      ROLS1
021500030113     C*
021600030114     c                   if        xtaopr <> '1'
021700030114     C                   Z-ADD     1             WPAG
021800030114     c                   end
021900940127     C*
022000940127     C                   ENDSR
022100940127     C************************************************************
022200940131     C* CARICAMENTO PAGINA LISTA
022300940127     C************************************************************
022400940127     C     ROLS1         BEGSR
022500940127     C*
022600940128     C                   SETOFF                                       32
022700940223     C                   Z-ADD     0             Y
022800940127     C                   Z-ADD     WMAX          S1NRR
022900940127     C*
023000940127     C* Leggo dal file anagrafico per caricare la lista
0231009401311    C     $EFILE        DOWEQ     *OFF
023200940127     C*
023300030113     c                   clear                   s1opz
023400050509     c                   movel     se8COD        s1cod
023500050509     c                   movel     se8UNI        s1uni
023600050509     c                   movel     se8DES        s1DES
023700050511     c                   movel     se8CLV        s1CLV
023800940127     C*
023900940127     C                   ADD       1             S1NRR
024000940127     C                   ADD       1             Y
024100940127     C*
024200940607     C                   WRITE     S1
024300940131     C*
024400940316    >C                   EXSR      REDANA
024500940128     C*
0246009401271-   C                   ENDDO
024700940127     C*
024800940223     C                   Z-ADD     S1NRR         WMAX                 30
024900940127     C*
025000940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
025100940127     C*
025200030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
025300940127     C                   MVR                     RESTO             3 0
025400030114     C     PAGINE        MULT      SFLPAG        C1RCD
0255000301141    C     RESTO         IFGT      0
025600030114     C                   ADD       1             C1RCD
0257000301141E   C                   ELSE
025800030114     C                   SUB       SFLPAG        C1RCD
025900030114     C                   ADD       1             C1RCD
0260000301141-   C                   ENDIF
026100940128     C*
026200940127     C                   ENDSR
026300940128     C************************************************************
026400940131     C* LETTURA RCD ARCHIVIO PILOTA
026500940128     C************************************************************
026600940607     C     REDANA        BEGSR
026700940128     C*
026800940131     C                   MOVEL     *OFF          $EFILE
026900940131     C                   MOVEL     *OFF          $RCDOK
027000940131     C*
0271009401311    C     $EFILE        DOUEQ     *ON
027200940131     C     $RCDOK        OREQ      *ON
027300030728     C*
027400050511     c                   Read      Tise800F
027500050511     c                   if        %eof(Tise800F)
027600030113     C                   MOVEL     *ON           $EFILE
027700030113     C                   MOVE      $EFILE        *IN33
027800030113     c                   else
027900030113     C                   MOVE      *ON           $RCDOK
028000030113     c                   end
028100940131     C*
0282009401311-   C                   ENDDO
028300940131     C*
028400940131     C                   ENDSR
028500940224     C************************************************************
028600940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
028700940224     C************************************************************
028800940224     C     CLCPAG        BEGSR
028900940224     C* Input :
029000940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
029100940224     C*          cursore
029200940224     C* - SFLPAG = numero rcd per pagina sfl
029300940224     C* Output :
029400940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
029500940224     C*
029600940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
029700940224     C                   MVR                     RESTO             3 0
029800940224     C     RESTO         IFGT      0
029900940224     C                   ADD       1             PAGINE
030000940224     C                   ENDIF
030100940224     C     PAGINE        MULT      SFLPAG        WPAG
030200940224     C*
030300940224     C                   ENDSR
030400940309     C************************************************************
030500940309     C* GESTIONE F03 VIDEO S1
030600940309     C************************************************************
030700940309     C     F03S1         BEGSR
030800940309     C*
030900940309     C                   MOVE      *ON           $FINE
031000940325     C* fine programma
031100940309     C                   ENDSR
031200940309     C/EJECT
031300940309     C************************************************************
031400940309     C* GESTIONE F10 VIDEO S1
031500940314     c* AGGIUNTA RECORD
031600940309     C************************************************************
031700940309     C     F10S1         BEGSR
031800940309     C*
031900030113     C                   RESET                   tabds
032000030729     C                   MOVEL     '1'           xtaopz
032100030113     C                   MOVE      *ZERO         xtaret
032200030113     C                   MOVE      *ZERO         xtaopr
032300050509     C                   MOVEl     *Zero         xtacod
032400030113     C                   MOVE      *BLANKS       KPJBU
032500030113     C                   MOVEL     tabds         KPJBU
032600050509$004 C                   CALL      'TISE80R1'
032700030113     C                   PARM                    KPJBA
032800030114     C                   MOVEL     KPJBU         tabds
032900030113      *
033000940309     C* ritorno da PGM gestione
033100940309     C                   EXSR      GESRET
033200940309     C     WINZS1        IFEQ      *ON
033300940309     C                   MOVE      *ON           $INZS1
033400940309     C* se esistevano già righe sul sfl
033500940309     C* calcolo pagina a cui deve posizionarsi
033600940309     C     WSFL          IFGT      0
033700940309     C                   EXSR      CLCPAG
033800940309     C* altrimenti carico solo la 1a pagina
033900940309     C                   ELSE
034000940309     C                   Z-ADD     SFLPAG        WPAG
034100940309     C                   ENDIF
034200940309     C                   ENDIF
034300940309     C*
034400940309     C                   ENDSR
034500940128     C************************************************************
034600940131     C* CONTROLLO TESTATA LISTA
034700940128     C************************************************************
034800940131     C     CTRC1         BEGSR
034900940128     C*
035000940201     C                   MOVE      *OFF          *IN99
035100940131     C*
035200940202     C                   ENDSR
035300940131     C************************************************************
035400940131     C* CONTROLLO OPZIONI LISTA
035500940131     C************************************************************
035600940131     C     CTRS1         BEGSR
035700940131     C*
035800940202     C                   MOVEL     *OFF          $ESCI
035900940201     C                   SETOFF                                       99
036000940131     C                   Z-ADD     0             S1OPZ
036100940131     C*
036200940127     C* Leggo il sfl solo se ci sono rcd
0363009401311    C     WMAX          IFGT      0
036400940607     C                   READC     S1                                     21
036500940127     C*
036600940131     C* esce se fine sfl o errore che richiede l'uscita
0367009401312    C     *IN21         DOWEQ     *OFF
036800940131     C     $ESCI         ANDEQ     *OFF
036900940201     C                   Z-ADD     S1NRR         C1RCD
037000940131     C* ctrl su riga
037100940131     C                   EXSR      RECS1
037200940131     C* gestione opzioni
0373009401313    C     S1OPZ         IFNE      0
037400940201     C     *IN99         ANDEQ     *OFF
037500940131     C                   EXSR      OPZS1
0376009401273-   C                   ENDIF
037700940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0378009402013    C     *IN99         IFEQ      *ON
037900940201     C     $ESCI         OREQ      *ON
038000940131     C                   MOVE      *ON           *IN32
038100940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038200940204     C* la ripristinerò a conclusione del ciclo di READC
038300940223     C                   MOVE      *OFF          $INZS1
0384009402233-   C                   ENDIF
038500940223     C*
038600940131     C                   Z-ADD     0             S1OPZ
038700940223     C*
038800940607     C                   UPDATE    S1
038900940223     C*
039000940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0391009401313    C     $ESCI         IFEQ      *OFF
039200940607     C                   READC     S1                                     21
039300940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0394009402014    C     *IN21         IFEQ      *ON
039500940201     C                   MOVE      WINZS1        $INZS1
039600940204     C* calcolo pagina a cui deve posizionarsi
039700940224     C                   EXSR      CLCPAG
0398009402014-   C                   ENDIF
0399009402013-   C                   ENDIF
040000940131     C*
0401009401272-   C                   ENDDO
040200940127     C*
0403009401311-   C                   ENDIF
040400940131     C*
040500940127     C                   ENDSR
040600940127     C/EJECT
040700940127     C************************************************************
040800940131     C* CONTROLLO CAMPI I/O RIGA LISTA
040900940127     C************************************************************
041000940131     C     RECS1         BEGSR
041100940131     C*
041200940201     C* reset indicatori DSPATR
041300940201     C                   MOVE      *ALL'0'       IN4049           10
041400940201     C                   MOVEA     IN4049        *IN(40)
041500940201     C*
041600940131     C                   ENDSR
041700940131     C************************************************************
041800940131     C* GESTIONE OPZIONI LISTA
041900940131     C************************************************************
042000940131     C     OPZS1         BEGSR
042100940201     C*
042200030113     C                   RESET                   tabds
042300030113     C                   MOVEL     S1OPZ         xtaopz
042400030113     C                   MOVE      *ZERO         xtaret
042500030113     C                   MOVE      *ZERO         xtaopr
042600030729     C                   MOVEl     s1cod         xtacod
042700940715     C                   MOVE      *BLANKS       KPJBU
042800030113     C                   MOVEL     tabds         KPJBU
042900050509$004 C                   CALL      'TISE80R1'
043000940607     C                   PARM                    KPJBA
043100030114     C                   MOVEL     KPJBU         tabds
043200940201     C*
043300940223     C* ritorno da PGM gestione
043400940223     C                   EXSR      GESRET
043500940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0436009402252    C     *IN99         IFEQ      *ON
043700940223     C                   SETON                                        40
0438009402252-   C                   ENDIF
043900940225     C*
044000940131     C                   ENDSR
044100940223     C/EJECT
044200940223     C************************************************************
044300940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
044400940223     C************************************************************
044500940223     C     GESRET        BEGSR
044600940223     C*
044700940223     C* modo di ritorno
044800940223     C*
0449009402231    C                   SELECT
045000940314    >C* << questi modi di utilizzo dei valori di ritorno dal
045100940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
045200940314    >C*    proposte, normalmente sempre valide, ma modificabili
045300940314    >C*    per situazioni particolari >>
045400940223     C* 1 = F3
045500030113    >C     xtaret        WHENEQ    '1'
045600940224     C                   MOVE      *ON           $ESCI
045700940223     C                   MOVE      *ON           $FINE
045800940223     C* 2 = F12
045900030113    >C     xtaret        WHENEQ    '2'
046000940223     C                   MOVE      *ON           $ESCI
046100940223     C*
0462009402231-   C                   ENDSL
046300940223     C*
046400940223     C* operazione eseguite dal pgm chiamato
046500940223     C*
0466009402231    C                   SELECT
046700940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
046800030113    >C     xtaopr        WHENEQ    '1'
046900940223     C                   MOVE      *ON           WINZS1
047000940223     C*
0471009402231-   C                   ENDSL
047200940223     C*
047300940223     C* funzione non eseguibile per errore :
047400940223     C*
0475009402231    C                   SELECT
047600940223     C* 1 = funzione richiamata chiusa in errore
047700940316    >C*  eventualmente gestire altri codici di errore
047800030113    >C     xtaerr        WHENEQ    '1'
047900940223     C                   MOVE      *ON           $ESCI
048000940223     C                   SETON                                        5299
048100940223     C*
0482009402231-   C                   ENDSL
048300940223     C*
048400940223     C                   ENDSR
048500940223     C/EJECT
048600940131     C************************************************************
048700940131     C* OPERAZIONI INIZIALI
048800940131     C************************************************************
048900940131     C     *INZSR        BEGSR
049000030113     C*
049100030113     C* Reperimento parametri
049200030113     C     *ENTRY        PLIST
049300030113     C                   PARM                    KPJBA
049400030113     C*
049500030113     C* Variabili per gestione videate
049600030113     C                   MOVE      *BLANK        $GEST             2
049700030113     C                   MOVE      *BLANK        $FINE             1
049800030113     C                   MOVE      *BLANK        $INZS1            1
049900030113     C                   MOVE      *BLANK        $EFILE            1
050000030113     C                   MOVE      *BLANK        $ESCI             1
050100030113     C                   MOVE      *BLANK        $RCDOK            1
050200030113     C* Indici
050300030113     C                   Z-ADD     0             X                 3 0
050400030113     C                   Z-ADD     0             Y                 3 0
050500940127     C*
050600940117     C                   ENDSR
050700030113     C************************************************************
