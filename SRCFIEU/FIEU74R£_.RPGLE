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
002100040115     FTNcfp02L  IF   E           K DISK
002200040115     FFIEU74D   CF   E             WORKSTN
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
003500040115     D  XTACOD                 5      7
003600040115     D  XTANTW                 8     10
003700040115     D  XTAFIL                11     13  0
003800040115     D  XTADDA                14     21  0
003900040115     D  XTAADA                22     29  0
004000940211     D*-------------
004100940211     D DSFMT           DS           512
004200940506     D  $TASTO               369    369
004300940211     D  NRG                  370    370
004400940211     D  NCL                  371    371
004500940211     D  SFLNRR               378    379B 0
004600030728      *
004700030728     Ddataiso          s               d   datfmt(*iso)
004800030728     Ddataeur          s               d   datfmt(*eur)
004900940207     D*-------------
005000940207     D* Nome PGM a video
005100940207     D                 DS
005200940207     D  PROGR                  1     10
005300940207     D  ASTER1                 1      1    INZ('*')
005400940207     D  SIGLA                  2      9
005500940207     D  ASTER2                10     10    INZ('*')
005600940127     D*-------------
005700940127     D* Reperimento nome PGM
005800940127     D STATUS         SDS           333
005900940127     D  DSPGM            *PROC
006000940607     D* descrizione tabella
006100030113$003 D EXTDS         E DS                  EXTNAME(DS8A)
006200030113     D*-------------
006300030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006400030113$003 D S1NRR           S                   Like(C1rcd)
006500030113$003 D WSfl            S                   Like(C1nrr)
006600030113$003 D Wmax            S                   Like(C1rcd)
006700030113$003 D Wpag            S                   Like(C1rcd)
006800030113$003 D Winzs1          S                   Like($inzs1)
006900940207     D*-------------
007000940211     D* COSTANTI
007100940211     D*-------------
007200030113     D SFLPAG          C                   CONST(11)
007300940314     D* dimensione della schiera $MS1
007400940506     D*
007500940506     D* Tasti di funzione
007600940506     D F01             C                   CONST(X'31')
007700940506     D F02             C                   CONST(X'32')
007800940506     D F03             C                   CONST(X'33')
007900940506     D F04             C                   CONST(X'34')
008000940506     D F05             C                   CONST(X'35')
008100940506     D F06             C                   CONST(X'36')
008200940506     D F07             C                   CONST(X'37')
008300940506     D F08             C                   CONST(X'38')
008400940506     D F09             C                   CONST(X'39')
008500940506     D F10             C                   CONST(X'3A')
008600940506     D F11             C                   CONST(X'3B')
008700940506     D F12             C                   CONST(X'3C')
008800940506     D F13             C                   CONST(X'B1')
008900940506     D F14             C                   CONST(X'B2')
009000940506     D F15             C                   CONST(X'B3')
009100940506     D F16             C                   CONST(X'B4')
009200940506     D F17             C                   CONST(X'B5')
009300940506     D F18             C                   CONST(X'B6')
009400940506     D F19             C                   CONST(X'B7')
009500940506     D F20             C                   CONST(X'B8')
009600940506     D F21             C                   CONST(X'B9')
009700940506     D F22             C                   CONST(X'BA')
009800940506     D F23             C                   CONST(X'BB')
009900940506     D F24             C                   CONST(X'BC')
010000940506     D ENTER           C                   CONST(X'F1')
010100940506     D ROLDWN          C                   CONST(X'F4')
010200940506     D ROLLUP          C                   CONST(X'F5')
010300940127     C*----------------------------------------------------*
010400940127     C*                MAIN LINE PROGRAM
010500940127     C*----------------------------------------------------*
010600940223     C* inizializzazione variabili
010700940223     C                   EXSR      INZVAR
010800940223     C*
010900940223     C     $FINE         DOWEQ     *OFF
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
012400030113     C* Variabili per gestione videate
012500030113     C*
012600030113     C                   MOVE      *OFF          $FINE
012700030113     C                   MOVE      *OFF          $INZS1
012800030113     C                   MOVE      *OFF          $EFILE
012900030113     C                   MOVE      *OFF          $ESCI
013000030113     C                   MOVE      *OFF          $RCDOK
013100030113     C                   Z-ADD     0             $ULKS1            3 0
013200030113     C*
013300030113     C                   MOVE      *ON           $INZS1
013400030113     C                   MOVE      'S1'          $GEST
013500030113     C*
013600030113     C* Variabili appoggio
013700030114     C                   Z-ADD     1             WPAG
013800030113     c*
013900030113     C                   ENDSR
014000940127     C************************************************************
014100940131     C* GESTIONE LISTA
014200940127     C************************************************************
014300940127     C     GESS1         BEGSR
014400030113     C*
014500940223     C* inizializzazione videata
014600940223     C     $INZS1        IFEQ      *ON
014700940127     C                   EXSR      INZS1
014800940223     C                   MOVE      *OFF          $INZS1
014900940127     C                   ENDIF
015000030113     C*
015100030113     C* emissione piede videata
015200030113     C                   WRITE     Z1
015300030113     C* Non ci sono records
015400940223     C     WMAX          IFEQ      0
015500940607     C                   WRITE     D1
015600030114     C                   Else
015700030114     C     Wsfl          IFgt      0
015800030114     C                   Z-ADD     wsfl          C1RCD
015900030114     C                   Else
016000030114     C     Wpag          IFgt      0
016100040115     C     Pagine        andgt     0
016200030114     C                   Z-ADD     wpag          C1RCD
016300030114     C                   EndIF
016400030114     C                   EndIF
016500030114     C                   ENDIF
016600940127     C*
016700030113     C*              *------------------*
016800940607     C                   EXFMT     C1
016900030113     C*              *------------------*
017000030113     C*
017100940204     C     C1NRR         IFNE      0
017200940204     C                   Z-ADD     C1NRR         WSFL
017300940204     C                   ENDIF
017400940127     C                   Z-ADD     SFLNRR        C1RCD
017500030113     C* Selezioni
0176009401271    C                   SELECT
017700940127     C* F3=Fine
017800940506     C     $TASTO        WHENEQ    F03
017900940309     C                   EXSR      F03S1
018000940131     C* F10=Immissione
018100940506     C     $TASTO        WHENEQ    F10
018200940309     C                   EXSR      F10S1
0183009401271O   C                   OTHER
018400940127     C* CONTROLLO DATI
018500940131     C                   EXSR      CTRC1
018600940201     C     *IN99         IFEQ      *OFF
018700940131     C                   EXSR      CTRS1
018800940131     C                   END
0189009401271-   C                   ENDSL
019000940127     C*
019100940127     C                   ENDSR
019200940224     C/EJECT
019300940127     C************************************************************
019400940131     C* INIZIALIZZAZIONE LISTA
019500940127     C************************************************************
019600940127     C     INZS1         BEGSR
019700940302     C* pulizia SFL
019800940128     C                   SETOFF                                         3031
019900940607     C                   WRITE     C1
020000940128     C                   SETON                                          31
020100940128     C*
020200030113     C* CARICAMENTO SFL totale
020300940201     C                   Z-ADD     0             S1NRR
020400030113     C                   Z-ADD     1             C1RCD
020500940128     C                   Z-ADD     0             WMAX
020600940224     C*
020700940224     C* Posizionamento su file pilota
020800040115     c     *loval        setll     Tncfp02L
020900940608    >C                   EXSR      REDANA
021000030113     C* Carico il SFL
021100940127     C                   EXSR      ROLS1
021200030113     C*
021300030114     c                   if        xtaopr <> '1'
021400030114     C                   Z-ADD     1             WPAG
021500030114     c                   end
021600940127     C*
021700940127     C                   ENDSR
021800940127     C************************************************************
021900940131     C* CARICAMENTO PAGINA LISTA
022000940127     C************************************************************
022100940127     C     ROLS1         BEGSR
022200940127     C*
022300940128     C                   SETOFF                                       32
022400940223     C                   Z-ADD     0             Y
022500940127     C                   Z-ADD     WMAX          S1NRR
022600940127     C*
022700940127     C* Leggo dal file anagrafico per caricare la lista
0228009401311    C     $EFILE        DOWEQ     *OFF
022900940127     C*
023000030113     c                   clear                   s1opz
023100040115     c                   movel     cfpCAU        s1cod
023200040115     c                   movel     cfpNTW        s1ntw
023300040115     c                   move      cfpFIL        s1fil
023400040115     c                   move      cfpdda        h1dda
023500040115     c                   move      cfpdda        dataiso
023600030728     c                   move      dataiso       dataeur
023700030728     c                   move      dataeur       s1dda
023800040115     c                   move      cfpada        h1ada
023900040115     c                   move      cfpada        dataiso
024000030728     c                   move      dataiso       dataeur
024100030728     c                   move      dataeur       s1ada
024200040115     c                   movel     cfpGRP        s1grp
024300040115     c                   movel     cfpDES        s1DES
024400040115     c                   movel     cfpANN        s1ANN
024500940127     C*
024600940127     C                   ADD       1             S1NRR
024700940127     C                   ADD       1             Y
024800940127     C*
024900940607     C                   WRITE     S1
025000940131     C*
025100940316    >C                   EXSR      REDANA
025200940128     C*
0253009401271-   C                   ENDDO
025400940127     C*
025500940223     C                   Z-ADD     S1NRR         WMAX                 30
025600940127     C*
025700940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
025800940127     C*
025900030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
026000940127     C                   MVR                     RESTO             3 0
026100030114     C     PAGINE        MULT      SFLPAG        C1RCD
0262000301141    C     RESTO         IFGT      0
026300030114     C                   ADD       1             C1RCD
0264000301141E   C                   ELSE
026500030114     C                   SUB       SFLPAG        C1RCD
026600030114     C                   ADD       1             C1RCD
0267000301141-   C                   ENDIF
026800940128     C*
026900940127     C                   ENDSR
027000940128     C************************************************************
027100940131     C* LETTURA RCD ARCHIVIO PILOTA
027200940128     C************************************************************
027300940607     C     REDANA        BEGSR
027400940128     C*
027500940131     C                   MOVEL     *OFF          $EFILE
027600940131     C                   MOVEL     *OFF          $RCDOK
027700940131     C*
0278009401311    C     $EFILE        DOUEQ     *ON
027900940131     C     $RCDOK        OREQ      *ON
028000030728     C*
028100040115     c                   Read      Tncfp02L
028200040115     c                   if        %eof(Tncfp02L)
028300030113     C                   MOVEL     *ON           $EFILE
028400030113     C                   MOVE      $EFILE        *IN33
028500030113     c                   else
028600030113     C                   MOVE      *ON           $RCDOK
028700030113     c                   end
028800940131     C*
0289009401311-   C                   ENDDO
029000940131     C*
029100940131     C                   ENDSR
029200940224     C************************************************************
029300940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
029400940224     C************************************************************
029500940224     C     CLCPAG        BEGSR
029600940224     C* Input :
029700940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
029800940224     C*          cursore
029900940224     C* - SFLPAG = numero rcd per pagina sfl
030000940224     C* Output :
030100940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
030200940224     C*
030300940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
030400940224     C                   MVR                     RESTO             3 0
030500940224     C     RESTO         IFGT      0
030600940224     C                   ADD       1             PAGINE
030700940224     C                   ENDIF
030800940224     C     PAGINE        MULT      SFLPAG        WPAG
030900940224     C*
031000940224     C                   ENDSR
031100940309     C************************************************************
031200940309     C* GESTIONE F03 VIDEO S1
031300940309     C************************************************************
031400940309     C     F03S1         BEGSR
031500940309     C*
031600940309     C                   MOVE      *ON           $FINE
031700940325     C* fine programma
031800940309     C                   ENDSR
031900940309     C/EJECT
032000940309     C************************************************************
032100940309     C* GESTIONE F10 VIDEO S1
032200940314     c* AGGIUNTA RECORD
032300940309     C************************************************************
032400940309     C     F10S1         BEGSR
032500940309     C*
032600030113     C                   RESET                   tabds
032700030729     C                   MOVEL     '1'           xtaopz
032800030113     C                   MOVE      *ZERO         xtaret
032900030113     C                   MOVE      *ZERO         xtaopr
033000030729     C                   MOVEl     *blank        xtacod
033100030729     C                   MOVEl     *blank        xtantw
033200030729     C                   MOVEl     *zero         xtafil
033300030729     C                   MOVEl     *zero         xtadda
033400030729     C                   MOVEl     *zero         xtaada
033500030113     C                   MOVE      *BLANKS       KPJBU
033600030113     C                   MOVEL     tabds         KPJBU
033700040115$004 C                   CALL      'FIEU74R1'
033800030113     C                   PARM                    KPJBA
033900030114     C                   MOVEL     KPJBU         tabds
034000030113      *
034100940309     C* ritorno da PGM gestione
034200940309     C                   EXSR      GESRET
034300940309     C     WINZS1        IFEQ      *ON
034400940309     C                   MOVE      *ON           $INZS1
034500940309     C* se esistevano già righe sul sfl
034600940309     C* calcolo pagina a cui deve posizionarsi
034700940309     C     WSFL          IFGT      0
034800940309     C                   EXSR      CLCPAG
034900940309     C* altrimenti carico solo la 1a pagina
035000940309     C                   ELSE
035100940309     C                   Z-ADD     SFLPAG        WPAG
035200940309     C                   ENDIF
035300940309     C                   ENDIF
035400940309     C*
035500940309     C                   ENDSR
035600940128     C************************************************************
035700940131     C* CONTROLLO TESTATA LISTA
035800940128     C************************************************************
035900940131     C     CTRC1         BEGSR
036000940128     C*
036100940201     C                   MOVE      *OFF          *IN99
036200940131     C*
036300940202     C                   ENDSR
036400940131     C************************************************************
036500940131     C* CONTROLLO OPZIONI LISTA
036600940131     C************************************************************
036700940131     C     CTRS1         BEGSR
036800940131     C*
036900940202     C                   MOVEL     *OFF          $ESCI
037000940201     C                   SETOFF                                       99
037100940131     C                   Z-ADD     0             S1OPZ
037200940131     C*
037300940127     C* Leggo il sfl solo se ci sono rcd
0374009401311    C     WMAX          IFGT      0
037500940607     C                   READC     S1                                     21
037600940127     C*
037700940131     C* esce se fine sfl o errore che richiede l'uscita
0378009401312    C     *IN21         DOWEQ     *OFF
037900940131     C     $ESCI         ANDEQ     *OFF
038000940201     C                   Z-ADD     S1NRR         C1RCD
038100940131     C* ctrl su riga
038200940131     C                   EXSR      RECS1
038300940131     C* gestione opzioni
0384009401313    C     S1OPZ         IFNE      0
038500940201     C     *IN99         ANDEQ     *OFF
038600940131     C                   EXSR      OPZS1
0387009401273-   C                   ENDIF
038800940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0389009402013    C     *IN99         IFEQ      *ON
039000940201     C     $ESCI         OREQ      *ON
039100940131     C                   MOVE      *ON           *IN32
039200940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
039300940204     C* la ripristinerò a conclusione del ciclo di READC
039400940223     C                   MOVE      *OFF          $INZS1
0395009402233-   C                   ENDIF
039600940223     C*
039700940131     C                   Z-ADD     0             S1OPZ
039800940223     C*
039900940607     C                   UPDATE    S1
040000940223     C*
040100940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0402009401313    C     $ESCI         IFEQ      *OFF
040300940607     C                   READC     S1                                     21
040400940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0405009402014    C     *IN21         IFEQ      *ON
040600940201     C                   MOVE      WINZS1        $INZS1
040700940204     C* calcolo pagina a cui deve posizionarsi
040800940224     C                   EXSR      CLCPAG
0409009402014-   C                   ENDIF
0410009402013-   C                   ENDIF
041100940131     C*
0412009401272-   C                   ENDDO
041300940127     C*
0414009401311-   C                   ENDIF
041500940131     C*
041600940127     C                   ENDSR
041700940127     C/EJECT
041800940127     C************************************************************
041900940131     C* CONTROLLO CAMPI I/O RIGA LISTA
042000940127     C************************************************************
042100940131     C     RECS1         BEGSR
042200940131     C*
042300940201     C* reset indicatori DSPATR
042400940201     C                   MOVE      *ALL'0'       IN4049           10
042500940201     C                   MOVEA     IN4049        *IN(40)
042600940201     C*
042700940131     C                   ENDSR
042800940131     C************************************************************
042900940131     C* GESTIONE OPZIONI LISTA
043000940131     C************************************************************
043100940131     C     OPZS1         BEGSR
043200940201     C*
043300030113     C                   RESET                   tabds
043400030113     C                   MOVEL     S1OPZ         xtaopz
043500030113     C                   MOVE      *ZERO         xtaret
043600030113     C                   MOVE      *ZERO         xtaopr
043700030729     C                   MOVEl     s1cod         xtacod
043800030729     C                   MOVEl     s1ntw         xtantw
043900030729     C                   MOVEl     s1fil         xtafil
044000030729     C                   MOVEl     h1dda         xtadda
044100030729     C                   MOVEl     h1ada         xtaada
044200940715     C                   MOVE      *BLANKS       KPJBU
044300030113     C                   MOVEL     tabds         KPJBU
044400040115$004 C                   CALL      'FIEU74R1'
044500940607     C                   PARM                    KPJBA
044600030114     C                   MOVEL     KPJBU         tabds
044700940201     C*
044800940223     C* ritorno da PGM gestione
044900940223     C                   EXSR      GESRET
045000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0451009402252    C     *IN99         IFEQ      *ON
045200940223     C                   SETON                                        40
0453009402252-   C                   ENDIF
045400940225     C*
045500940131     C                   ENDSR
045600940223     C/EJECT
045700940223     C************************************************************
045800940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
045900940223     C************************************************************
046000940223     C     GESRET        BEGSR
046100940223     C*
046200940223     C* modo di ritorno
046300940223     C*
0464009402231    C                   SELECT
046500940314    >C* << questi modi di utilizzo dei valori di ritorno dal
046600940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
046700940314    >C*    proposte, normalmente sempre valide, ma modificabili
046800940314    >C*    per situazioni particolari >>
046900940223     C* 1 = F3
047000030113    >C     xtaret        WHENEQ    '1'
047100940224     C                   MOVE      *ON           $ESCI
047200940223     C                   MOVE      *ON           $FINE
047300940223     C* 2 = F12
047400030113    >C     xtaret        WHENEQ    '2'
047500940223     C                   MOVE      *ON           $ESCI
047600940223     C*
0477009402231-   C                   ENDSL
047800940223     C*
047900940223     C* operazione eseguite dal pgm chiamato
048000940223     C*
0481009402231    C                   SELECT
048200940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
048300030113    >C     xtaopr        WHENEQ    '1'
048400940223     C                   MOVE      *ON           WINZS1
048500940223     C*
0486009402231-   C                   ENDSL
048700940223     C*
048800940223     C* funzione non eseguibile per errore :
048900940223     C*
0490009402231    C                   SELECT
049100940223     C* 1 = funzione richiamata chiusa in errore
049200940316    >C*  eventualmente gestire altri codici di errore
049300030113    >C     xtaerr        WHENEQ    '1'
049400940223     C                   MOVE      *ON           $ESCI
049500940223     C                   SETON                                        5299
049600940223     C*
0497009402231-   C                   ENDSL
049800940223     C*
049900940223     C                   ENDSR
050000940223     C/EJECT
050100940131     C************************************************************
050200940131     C* OPERAZIONI INIZIALI
050300940131     C************************************************************
050400940131     C     *INZSR        BEGSR
050500030113     C*
050600030113     C* Reperimento parametri
050700030113     C     *ENTRY        PLIST
050800030113     C                   PARM                    KPJBA
050900030113     C*
051000030113     C* Variabili per gestione videate
051100030113     C                   MOVE      *BLANK        $GEST             2
051200030113     C                   MOVE      *BLANK        $FINE             1
051300030113     C                   MOVE      *BLANK        $INZS1            1
051400030113     C                   MOVE      *BLANK        $EFILE            1
051500030113     C                   MOVE      *BLANK        $ESCI             1
051600030113     C                   MOVE      *BLANK        $RCDOK            1
051700030113     C* Indici
051800030113     C                   Z-ADD     0             X                 3 0
051900030113     C                   Z-ADD     0             Y                 3 0
052000940127     C*
052100940117     C                   ENDSR
052200030113     C************************************************************
