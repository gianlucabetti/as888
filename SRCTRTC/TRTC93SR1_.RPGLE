000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200940211     H DECEDIT('0,') DATEDIT(*DMY.)
000300940224      *
000400040930      *  11           x selezione di un codice da ripassare al pgm chiamante
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
002100111220     FEDsts01L  IF   E           K DISK
002200111220     FTRTC93SD1 CF   E             WORKSTN
002300940607     F                                     SFILE(S1:S1NRR)
002400090225     F                                     SFILE(S0:S0NRR)
002500940201     F                                     INFDS(DSFMT)
002600940128     D*----------------------------------------------------*
002700940211     D* Passaggio Parametri
002800940211     D KPJBA         E DS
002900111220     D UTEDSE0F      E DS
003000100617      *
003100030113      *-------------
003200940325     D* Parametri in ricezione
003300030113     D  TABDS          DS
003400030113     D  XTAOPZ                 1      2
003500030113     D  XTARET                 3      3
003600030113     D  XTAOPR                 4      4
003700030113     D  XTAERR                 5      5
003800111220     D  XTAKEY1                6      8
003900111220     D  XTAKEY2                9     11
004000111220     D  XTACOD1               12     14
004100111220     D  XTACOD2               15     17
004200111220     D  XTAUNBcli             18     52
004300940211     D*-------------
004400940211     D DSFMT           DS           512
004500940506     D  $TASTO               369    369
004600940211     D  NRG                  370    370
004700940211     D  NCL                  371    371
004800940211     D  SFLNRR               378    379B 0
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
006000030113     D*-------------
006100111220     C* Variabili appoggio sempre presenti
006200090225$003 D S0NRR           S                   Like(C1rcd)
006300090225$003 D S1NRR           S                   Like(C1rcd)
006400100319$003 D S2NRR           S                   Like(C1rcd)
006500030113$003 D WSfl            S                   Like(C1nrr)
006600030113$003 D Wmax            S                   Like(C1rcd)
006700030113$003 D Wpag            S                   Like(C1rcd)
006800090225$003 D Winzs0          S                   Like($inzs0)
006900090225$003 D Winzs1          S                   Like($inzs1)
007000100319$003 D Winzs2          S                   Like($inzs2)
007100100324$003 D x1cod           S                   Like(h1cod)
007200111220$003 D x1cod1          S                   Like(h1cod1)
007300090225$003 D x0cod           S                   Like(s0cod)
007400111220     D*-------------
007500100326$003 D savC0pos        S                   Like(C0POS)
007600111220$003 D savC0pos1       S                   Like(C0POS1)
007700090225     D*-------------
007800100119     d dataiso         s               d   datfmt(*iso)
007900100119     d dataeur         s               d   datfmt(*eur)
008000111220     D*-------------
008100111220     d  savUNBCLI      s                   Like(stsUNBCLI)
008200111220     d  savCODSTS      s                   Like(stsCODSTS)
008300111220     d  savSUBSTS      s                   Like(stsSUBSTS)
008400940207     D*-------------
008500940211     D* COSTANTI
008600940211     D*-------------
008700090225     D SFLPAG0         C                   CONST(11)
008800100617     D SFLPAG1         C                   CONST(12)
008900111220     D*-------------
009000940506     D* Tasti di funzione
009100940506     D F01             C                   CONST(X'31')
009200940506     D F02             C                   CONST(X'32')
009300940506     D F03             C                   CONST(X'33')
009400940506     D F04             C                   CONST(X'34')
009500940506     D F05             C                   CONST(X'35')
009600940506     D F06             C                   CONST(X'36')
009700940506     D F07             C                   CONST(X'37')
009800940506     D F08             C                   CONST(X'38')
009900940506     D F09             C                   CONST(X'39')
010000940506     D F10             C                   CONST(X'3A')
010100940506     D F11             C                   CONST(X'3B')
010200940506     D F12             C                   CONST(X'3C')
010300940506     D F13             C                   CONST(X'B1')
010400940506     D F14             C                   CONST(X'B2')
010500940506     D F15             C                   CONST(X'B3')
010600940506     D F16             C                   CONST(X'B4')
010700940506     D F17             C                   CONST(X'B5')
010800940506     D F18             C                   CONST(X'B6')
010900940506     D F19             C                   CONST(X'B7')
011000940506     D F20             C                   CONST(X'B8')
011100940506     D F21             C                   CONST(X'B9')
011200940506     D F22             C                   CONST(X'BA')
011300940506     D F23             C                   CONST(X'BB')
011400940506     D F24             C                   CONST(X'BC')
011500940506     D ENTER           C                   CONST(X'F1')
011600940506     D ROLDWN          C                   CONST(X'F4')
011700940506     D ROLLUP          C                   CONST(X'F5')
011800940127     C*----------------------------------------------------*
011900940127     C*                MAIN LINE PROGRAM
012000940127     C*----------------------------------------------------*
012100940223     C* inizializzazione variabili
012200940223     C                   EXSR      INZVAR
012300940223     C*
012400940223     C     $FINE         DOWEQ     *OFF
012500090225     C     $GEST         CASEQ     'S0'          GESS0
012600940131     C     $GEST         CASEQ     'S1'          GESS1
012700940117     C                   END
012800940117     C                   END
012900940325     C* fine programma
013000940325     C                   SETON                                            LR
013100030113     C************************************************************
013200030113     C* INIZIALIZZAZIONE VARIABILI
013300030113     C************************************************************
013400030113     C     INZVAR        BEGSR
013500030113     C*
013600030113     C* Pulizia campi e indicatori
013700030113     C                   MOVE      *ALL'0'       IN4049           10
013800030113     C                   MOVEA     IN4049        *IN(40)
013900030113     C                   CLEAR                   S1OPZ
014000090225     C                   CLEAR                   S0OPZ
014100030113     C*
014200090225     C* Variabili per gestione videate
014300030113     C                   MOVE      *OFF          $FINE
014400030113     C                   MOVE      *OFF          $EFILE
014500030113     C                   MOVE      *OFF          $ESCI
014600030113     C                   MOVE      *OFF          $RCDOK
014700030113     C                   Z-ADD     0             $ULKS1            3 0
014800030113     C*
014900100319     C                   MOVE      *ON           $INZS2
015000030113     C                   MOVE      *ON           $INZS1
015100090225     C                   MOVE      *ON           $INZS0
015200030113     C* Variabili appoggio
015300030114     C                   Z-ADD     1             WPAG
015400030113     c*
015500100407     c* Video da cui partire
015600111220     C                   MOVE      'S0'          $GEST
015700100407     c*
015800030113     C                   ENDSR
015900090225     C************************************************************
016000090225     C* GESTIONE LISTA
016100090225     C************************************************************
016200090225     C     GESS0         BEGSR
016300090225     C*
016400090225     C* inizializzazione videata
016500090225     C     $INZS0        IFEQ      *ON
016600090225     C                   EXSR      INZS0
016700090225     C                   MOVE      *OFF          $INZS0
016800090225     C                   ENDIF
016900100325     C*
017000090225     C* emissione piede videata
017100090225     C                   WRITE     Z0
017200090225     C* Non ci sono records
017300090225     C     WMAX          IFEQ      0
017400090225     C                   WRITE     D0
017500090225     C                   Else
017600090225     C     Wsfl          IFgt      0
017700090225     C                   Z-ADD     wsfl          C0RCD
017800090225     C                   Else
017900090225     C     Wpag          IFgt      0
018000090225     C                   Z-ADD     wpag          C0RCD
018100090225     C                   EndIF
018200090225     C                   EndIF
018300090225     C                   ENDIF
018400090225     C*
018500090225     C*              *------------------*
018600090225     C                   EXFMT     C0
018700090225     C*              *------------------*
018800090225     C*
018900090225     C     C0NRR         IFNE      0
019000090225     C                   Z-ADD     C0NRR         WSFL
019100090225     C                   ENDIF
019200090225     C                   Z-ADD     SFLNRR        C0RCD
019300090225     C* Selezioni
0194000902251    C                   SELECT
019500090225     C* F3=Fine
019600090225     C     $TASTO        WHENEQ    F03
019700090225     C                   EXSR      F03S1
019800100121     C* F10=Immissione
019900100121     C     $TASTO        WHENEQ    F10
020000100121     C                   EXSR      F10S1
020100100325     C                   MOVE      'S0'          $GEST
0202000902251O   C                   OTHER
020300090225     C* CONTROLLO DATI
020400090225     C                   EXSR      CTRC0
020500090225     C     *IN99         IFEQ      *OFF
020600090225     C                   EXSR      CTRS0
020700090225     C                   END
0208000902251-   C                   ENDSL
020900090225     C*
021000090225     C                   ENDSR
021100090225     C/EJECT
021200090225     C************************************************************
021300090225     C* INIZIALIZZAZIONE LISTA
021400090225     C************************************************************
021500090225     C     INZS0         BEGSR
021600100407     C*
021700090225     C* pulizia SFL
021800090225     C                   SETOFF                                         3031
021900090225     C                   WRITE     C0
022000090225     C                   SETON                                          31
022100090225     C*
022200090225     C* CARICAMENTO SFL totale
022300090225     C                   Z-ADD     0             S0NRR
022400090225     C                   Z-ADD     1             C0RCD
022500090225     C                   Z-ADD     0             WMAX
022600090225     C                   Z-ADD     0             wsfl
022700090225     C*
022800090225     C*  Posizionamento all'inizio
022900111220     c                   movel(p)  savUNBCLI     stsUNBCLI
023000111220     c                   movel(p)  savUNBCLI     c0UNB
023100111220     c                   movel(p)  c0pos         stsCODSTS
023200111220     c                   movel(p)  c0pos1        stsSUBSTS
023300111220     C*
023400100326     C                   IF        Posiziona = 'S'
023500111220     c     Key_STS       setll     edSTS01L
023600100326     C                   eval      Posiziona = *blank
023700100326     c                   else
023800111220     c     savUNBCLI     setll     edSTS01L
023900100326     c                   end
024000111220     C*
024100111220    >C                   EXSR      READ_STS
024200090225     C* Carico il SFL
024300090225     C                   EXSR      ROLS0
024400090225     C*
024500090225     c                   if        xtaopr <> '1'
024600090225     C                   Z-ADD     1             WPAG
024700090225     c                   end
024800090225     C*
024900090225     C                   ENDSR
025000090225     C************************************************************
025100090225     C* CARICAMENTO PAGINA LISTA
025200090225     C************************************************************
025300090225     C     ROLS0         BEGSR
025400090225     C*
025500090225     C                   SETOFF                                       32
025600090225     C                   Z-ADD     0             Y
025700090225     C                   Z-ADD     WMAX          S0NRR
025800100119     C*
025900090225     C* Leggo dal file i segmenti presenti nell'archivio
0260000902251    C     $EFILE        DOWEQ     *OFF
026100100617     C*
026200111220     c                   clear                   s0opz
026300111220     c                   movel     stsCODSTS     s0cod
026400111220     c                   movel     stsSUBSTS     s0cod1
026500100617     C*
026600111220     c                   movel     stsCODSTSb    s0brt
026700111220     c                   movel     stsSUBSTSb    s0brt1
026800100617     c*
026900111220     c                   eval      s0des =  stsDEScri
027000090225     C*
027100090225     C                   ADD       1             S0NRR
027200090225     C                   ADD       1             Y
027300090225     C                   WRITE     S0
027400100119     C*
027500111220    >C                   EXSR      READ_STS
0276000902251-   C                   ENDDO
027700090225     C*
027800090225     C                   Z-ADD     S0NRR         WMAX                 30
027900090225     C*
028000090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
028100090225     C*
028200090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
028300090225     C                   MVR                     RESTO             3 0
028400090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0285000902251    C     RESTO         IFGT      0
028600090225     C                   ADD       1             C0RCD
0287000902251E   C                   ELSE
028800090225     C                   SUB       SFLPAG0       C0RCD
028900090225     C                   ADD       1             C0RCD
0290000902251-   C                   ENDIF
029100090225     C*
029200090225     C                   ENDSR
029300090225     C************************************************************
029400090225     C* LETTURA RCD
029500090225     C************************************************************
029600111220     C     READ_STS      BEGSR
029700090225     C*
029800090225     C                   MOVEL     *OFF          $EFILE
029900090225     C                   MOVEL     *OFF          $RCDOK
030000090225     C*
0301000902251    C     $EFILE        DOUEQ     *ON
030200090225     C     $RCDOK        OREQ      *ON
030300090225     C*
030400111220     c     stsUNBCLI     Reade     edSTS01L
030500111220     c                   if        %eof(edSTS01L)
030600090225     C                   MOVEL     *ON           $EFILE
030700090225     C                   MOVE      $EFILE        *IN33
030800090225     c                   else
030900100322      **
031000100322     c                   eXSr      Selez_RECord
031100100119     c                   end
031200090225     C*
0313000902251-   C                   ENDDO
031400090225     C*
031500090225     C                   ENDSR
031600090225     C************************************************************
031700090225     C* CONTROLLO TESTATA LISTA
031800090225     C************************************************************
031900100322     C     Selez_RECord  BEGSR
032000100407      *
032100100618     C                   MOVEL     *ON           $RCDOK
032200100322     C*
032300100322     C     End_filtro    ENDSR
032400100322     C************************************************************
032500100322     C* CONTROLLO TESTATA LISTA
032600100322     C************************************************************
032700100322     C     CTRC0         BEGSR
032800100322     C*
032900100326     C                   MOVE      *OFF          *IN99
033000100326     C*
033100100326     C                   clear                   posiziona         1
033200111220     c                   if        c0pos <> SAVc0pos or
033300111220     c                             c0pos1<> SAVc0pos1
033400100326     C                   move      'S'           posiziona         1
033500100326     c                   eval      SAVc0pos = c0pos
033600111220     c                   eval      SAVc0pos1= c0pos1
033700100326     c                   move      *ON           $INZS0
033800100326     c                   end
033900090225     C*
034000090225     C                   ENDSR
034100090225     C************************************************************
034200090225     C* CONTROLLO OPZIONI LISTA
034300090225     C************************************************************
034400090225     C     CTRS0         BEGSR
034500090225     C*
034600090225     C                   MOVEL     *OFF          $ESCI
034700090225     C                   SETOFF                                       99
034800090225     C                   clear                   S0OPZ
034900090225     c                   clear                   x0cod
035000090225     C*
035100090225     C* Leggo il sfl solo se ci sono rcd
0352000902251    C     WMAX          IFGT      0
035300090225     C                   READC     S0                                     21
035400090225     C*
035500090225     C* esce se fine sfl o errore che richiede l'uscita
0356000902252    C     *IN21         DOWEQ     *OFF
035700090225     C     $ESCI         ANDEQ     *OFF
035800090225     C                   Z-ADD     S0NRR         C0RCD
035900090225     C* ctrl su riga
036000090225     C                   EXSR      RECS0
036100090225     C* gestione opzioni
0362000902253    C     S0OPZ         IFNE      *blank
036300090225     C     *IN99         ANDEQ     *OFF
036400090225     C                   EXSR      OPZS0
0365000902253-   C                   ENDIF
036600111220      *
036700090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0368000902253    C     *IN99         IFEQ      *ON
036900090225     C     $ESCI         OREQ      *ON
037000090225     C                   MOVE      *ON           *IN32
037100090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
037200090225     C* la ripristinerò a conclusione del ciclo di READC
037300090225     C                   MOVE      *OFF          $INZS0
0374000902253-   C                   ENDIF
037500090225     C*
037600090225     C                   clear                   S0OPZ
037700090225     C                   UPDATE    S0
037800090225     C*
037900090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0380000902253    C     $ESCI         IFEQ      *OFF
038100090225     C                   READC     S0                                     21
038200111220     C*
038300090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0384000902254    C     *IN21         IFEQ      *ON
038500090225     C                   MOVE      WINZS0        $INZS0
038600090225     C* calcolo pagina a cui deve posizionarsi
038700090225     C                   EXSR      CLCPAG0
0388000902254-   C                   ENDIF
038900111220     C*
0390000902253-   C                   ENDIF
039100090225     C*
0392000902252-   C                   ENDDO
039300090225     C*
0394000902251-   C                   ENDIF
039500090225     C*
039600090225     C                   ENDSR
039700090225     C/EJECT
039800090225     C************************************************************
039900090225     C* CONTROLLO CAMPI I/O RIGA LISTA
040000090225     C************************************************************
040100090225     C     RECS0         BEGSR
040200090225     C*
040300090225     C* reset indicatori DSPATR
040400090225     C                   MOVE      *ALL'0'       IN4049           10
040500090225     C                   MOVEA     IN4049        *IN(40)
040600090225     C*
040700090225     C                   ENDSR
040800090225     C************************************************************
040900090225     C* GESTIONE OPZIONI LISTA
041000090225     C************************************************************
041100090225     C     OPZS0         BEGSR
041200090225     C*
041300090225      * selezionato un codice da restituire al chiamante
0414000902253    C     S0OPZ         IFeq      '1'
041500111220     c                   move      s0cod         codSTS            3
041600111220     c                   move      s0cod1        codSUB            3
041700090225     C                   MOVE      'S1'          $GEST
041800090225     C                   MOVEL     *ON           $ESCI
041900090225     C                   END
042000090225     C*
042100090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0422000902252    C     *IN99         IFEQ      *ON
042300090225     C                   SETON                                        40
0424000902252-   C                   ENDIF
042500090225     C*
042600090225     C                   ENDSR
042700090225     C/EJECT
042800940127     C************************************************************
042900940131     C* GESTIONE LISTA
043000940127     C************************************************************
043100940127     C     GESS1         BEGSR
043200030113     C*
043300940223     C* inizializzazione videata
043400940223     C     $INZS1        IFEQ      *ON
043500940127     C                   EXSR      INZS1
043600940223     C                   MOVE      *OFF          $INZS1
043700940127     C                   ENDIF
043800030113     C*
043900030113     C* emissione piede videata
044000030113     C                   WRITE     Z1
044100030113     C* Non ci sono records
044200940223     C     WMAX          IFEQ      0
044300940607     C                   WRITE     D1
044400030114     C                   Else
044500030114     C     Wsfl          IFgt      0
044600030114     C                   Z-ADD     wsfl          C1RCD
044700030114     C                   Else
044800030114     C     Wpag          IFgt      0
044900030114     C                   Z-ADD     wpag          C1RCD
045000030114     C                   EndIF
045100030114     C                   EndIF
045200030114     C                   ENDIF
045300940127     C*
045400030113     C*              *------------------*
045500940607     C                   EXFMT     C1
045600030113     C*              *------------------*
045700030113     C*
045800940204     C     C1NRR         IFNE      0
045900940204     C                   Z-ADD     C1NRR         WSFL
046000940204     C                   ENDIF
046100940127     C                   Z-ADD     SFLNRR        C1RCD
046200030113     C* Selezioni
0463009401271    C                   SELECT
046400940127     C* F3=Fine
046500940506     C     $TASTO        WHENEQ    F03
046600940309     C                   EXSR      F03S1
046700940131     C* F10=Immissione
046800940506     C     $TASTO        WHENEQ    F10
046900940309     C                   EXSR      F10S1
047000100325     C                   MOVE      'S1'          $GEST
047100090225     C* F12=Ritorno
047200090225     C     $TASTO        WHENEQ    F12
047300090225     C                   eval      $Gest='S0'
047400090225     C                   MOVE      *ON           $INZS1
047500090225     C                   MOVE      *ON           $INZS0
047600100407      *
047700100326     C* se c'era un posizionamento
047800111220     c                   if        c0POS <> *blank or
047900111220     c                             c0POS1<> *blank
048000100326     c                   eval      SAVc0pos = c0pos
048100111220     c                   eval      SAVc0pos1= c0pos1
048200100326     c                   eval      posiziona = 'S'
048300100326     c                   end
048400100407      *
0485009401271O   C                   OTHER
048600940127     C* CONTROLLO DATI
048700940131     C                   EXSR      CTRC1
048800940201     C     *IN99         IFEQ      *OFF
048900940131     C                   EXSR      CTRS1
049000940131     C                   END
0491009401271-   C                   ENDSL
049200940127     C*
049300940127     C                   ENDSR
049400940224     C/EJECT
049500940127     C************************************************************
049600940131     C* INIZIALIZZAZIONE LISTA
049700940127     C************************************************************
049800940127     C     INZS1         BEGSR
049900100322     C*
050000940302     C* pulizia SFL
050100100121     C                   SETOFF                                       423031
050200940607     C                   WRITE     C1
050300940128     C                   SETON                                          31
050400940128     C*
050500030113     C* CARICAMENTO SFL totale
050600940201     C                   Z-ADD     0             S1NRR
050700030113     C                   Z-ADD     1             C1RCD
050800940128     C                   Z-ADD     0             WMAX
050900090225     C                   Z-ADD     0             wsfl
051000111220     c                   movel(p)  savUNBCLI     c1UNB
051100940224     C*
051200940224     C* Posizionamento su file pilota
051300111220     c                   if        Posiziona = 'S'
051400111220     c     key_STS       setll     edSTS01L
051500100618     c                   else
051600111220     c     savUNBCLI     setll     edSTS01L
051700100618     c                   end
051800100618     C*
051900111220    >C                   EXSR      READ_STS
052000030113     C* Carico il SFL
052100940127     C                   EXSR      ROLS1
052200030113     C*
052300030114     c                   if        xtaopr <> '1'
052400030114     C                   Z-ADD     1             WPAG
052500030114     c                   end
052600940127     C*
052700940127     C                   ENDSR
052800940127     C************************************************************
052900940131     C* CARICAMENTO PAGINA LISTA
053000940127     C************************************************************
053100940127     C     ROLS1         BEGSR
053200940127     C*
053300940128     C                   SETOFF                                       32
053400940223     C                   Z-ADD     0             Y
053500940127     C                   Z-ADD     WMAX          S1NRR
053600940127     C*
053700940127     C* Leggo dal file anagrafico per caricare la lista
0538009401311    C     $EFILE        DOWEQ     *OFF
053900111220     C*
054000111220     c                   clear                   s1opz
054100111220     c                   clear                   s1cod
054200111220     c                   clear                   s1brt
054300111220     C*
054400111220     c                   movel     stsCODSTS     s1cod
054500111220     c                   move      stsSUBSTS     s1cod
054600111220     C*
054700111220     c                   movel     stsCODSTSb    s1brt
054800111220     c                   move      stsSUBSTSb    s1brt
054900111220     c*
055000111220     c                   eval      s1des =  stsDEScri
055100111220     C*
055200940127     C                   ADD       1             S1NRR
055300940127     C                   ADD       1             Y
055400940607     C                   WRITE     S1
055500940131     C*
055600111220    >C                   EXSR      READ_STS
0557009401271-   C                   ENDDO
055800940127     C*
055900940223     C                   Z-ADD     S1NRR         WMAX                 30
056000940127     C*
056100940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
056200090225     C     S1NRR         DIV       SFLPAG1       PAGINE            4 0
056300940127     C                   MVR                     RESTO             3 0
056400090225     C     PAGINE        MULT      SFLPAG1       C1RCD
0565000301141    C     RESTO         IFGT      0
056600030114     C                   ADD       1             C1RCD
0567000301141E   C                   ELSE
056800090225     C                   SUB       SFLPAG1       C1RCD
056900030114     C                   ADD       1             C1RCD
0570000301141-   C                   ENDIF
057100940128     C*
057200940127     C                   ENDSR
057300940224     C************************************************************
057400940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
057500940224     C************************************************************
057600090225     C     CLCPAG0       BEGSR
057700940224     C* Input :
057800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
057900940224     C*          cursore
058000940224     C* - SFLPAG = numero rcd per pagina sfl
058100940224     C* Output :
058200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
058300940224     C*
058400090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
058500940224     C                   MVR                     RESTO             3 0
058600940224     C     RESTO         IFGT      0
058700940224     C                   ADD       1             PAGINE
058800940224     C                   ENDIF
058900090226     c                   if        pagine > 1
059000090225     C     PAGINE        MULT      SFLPAG0       WPAG
059100090226     C                   END
059200940224     C*
059300940224     C                   ENDSR
059400090225     C************************************************************
059500090225     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
059600090225     C************************************************************
059700090225     C     CLCPAG1       BEGSR
059800090225     C* Input :
059900090225     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
060000090225     C*          cursore
060100090225     C* - SFLPAG = numero rcd per pagina sfl
060200090225     C* Output :
060300090225     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
060400090225     C*
060500090225     C     WSFL          DIV       SFLPAG1       PAGINE            4 0
060600090225     C                   MVR                     RESTO             3 0
060700090225     C     RESTO         IFGT      0
060800090225     C                   ADD       1             PAGINE
060900090225     C                   ENDIF
061000090226     c                   if        pagine > 1
061100090225     C     PAGINE        MULT      SFLPAG1       WPAG
061200090226     C                   ENDIF
061300090225     C*
061400090225     C                   ENDSR
061500940309     C************************************************************
061600940309     C* GESTIONE F03 VIDEO S1
061700940309     C************************************************************
061800940309     C     F03S1         BEGSR
061900940309     C*
062000940309     C                   MOVE      *ON           $FINE
062100940325     C* fine programma
062200940309     C                   ENDSR
062300940309     C/EJECT
062400940309     C************************************************************
062500940309     C* GESTIONE F10 VIDEO S1
062600940314     c* AGGIUNTA RECORD
062700940309     C************************************************************
062800940309     C     F10S1         BEGSR
062900940309     C*
063000030113     C                   RESET                   tabds
063100030113     C                   MOVEL     '01'          xtaopz
063200030113     C                   MOVE      *ZERO         xtaret
063300030113     C                   MOVE      *ZERO         xtaopr
063400100409     C                   MOVEl     *blank        xtakey1
063500100409     C                   MOVEl     *blank        xtakey2
063600111220     C                   MOVEl     *blank        xtacod1
063700111220     C                   MOVEl     *blank        xtacod2
063800111220     C                   MOVEl     savUNBcli     xtaUNBcli
063900030113     C                   MOVE      *BLANKS       KPJBU
064000030113     C                   MOVEL     tabds         KPJBU
064100111220$004 C                   CALL      'TRTC93SR2'
064200030113     C                   PARM                    KPJBA
064300090225      *
064400030114     C                   MOVEL     KPJBU         tabds
064500030113      *
064600940309     C* ritorno da PGM gestione
064700940309     C                   EXSR      GESRET
064800090226      *
064900940309     C     WINZS1        IFEQ      *ON
065000940309     C                   MOVE      *ON           $INZS1
065100090226     C* carico sempre la 1a pagina
065200090226     C                   Z-ADD     1             WPAG
065300940309     C                   ENDIF
065400940309     C*
065500940309     C                   ENDSR
065600100322     C************************************************************
065700100322     C* CONTROLLO TESTATA LISTA
065800100322     C************************************************************
065900100322     C     CTRC1         BEGSR
066000100322     C*
066100940201     C                   MOVE      *OFF          *IN99
066200940131     C*
066300940202     C                   ENDSR
066400940131     C************************************************************
066500940131     C* CONTROLLO OPZIONI LISTA
066600940131     C************************************************************
066700940131     C     CTRS1         BEGSR
066800940131     C*
066900940202     C                   MOVEL     *OFF          $ESCI
067000940201     C                   SETOFF                                       99
067100940131     C                   Z-ADD     0             S1OPZ
067200040930     c                   clear                   x1cod
067300111220     c                   clear                   x1cod1
067400940131     C*
067500940127     C* Leggo il sfl solo se ci sono rcd
0676009401311    C     WMAX          IFGT      0
067700940607     C                   READC     S1                                     21
067800940127     C*
067900940131     C* esce se fine sfl o errore che richiede l'uscita
0680009401312    C     *IN21         DOWEQ     *OFF
068100940131     C     $ESCI         ANDEQ     *OFF
068200940201     C                   Z-ADD     S1NRR         C1RCD
068300940131     C* ctrl su riga
068400940131     C                   EXSR      RECS1
068500940131     C* gestione opzioni
0686009401313    C     S1OPZ         IFNE      0
068700940201     C     *IN99         ANDEQ     *OFF
068800940131     C                   EXSR      OPZS1
0689009401273-   C                   ENDIF
069000940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0691009402013    C     *IN99         IFEQ      *ON
069200940201     C     $ESCI         OREQ      *ON
069300940131     C                   MOVE      *ON           *IN32
069400940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
069500940204     C* la ripristinerò a conclusione del ciclo di READC
069600940223     C                   MOVE      *OFF          $INZS1
0697009402233-   C                   ENDIF
069800940223     C*
069900100323     C  n99              Z-ADD     0             S1OPZ
070000100121      ****
070100100323     c     h1annulla     comp      *on                                    42
070200940223     C*
070300940607     C                   UPDATE    S1
070400940223     C*
070500940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0706009401313    C     $ESCI         IFEQ      *OFF
070700940607     C                   READC     S1                                     21
070800940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0709009402014    C     *IN21         IFEQ      *ON
071000940201     C                   MOVE      WINZS1        $INZS1
071100940204     C* calcolo pagina a cui deve posizionarsi
071200090225     C                   EXSR      CLCPAG1
0713009402014-   C                   ENDIF
0714009402013-   C                   ENDIF
071500940131     C*
0716009401272-   C                   ENDDO
071700940127     C*
0718009401311-   C                   ENDIF
071900940131     C*
072000940127     C                   ENDSR
072100940127     C/EJECT
072200940127     C************************************************************
072300940131     C* CONTROLLO CAMPI I/O RIGA LISTA
072400940127     C************************************************************
072500940131     C     RECS1         BEGSR
072600940131     C*
072700940201     C* reset indicatori DSPATR
072800940201     C                   MOVE      *ALL'0'       IN4049           10
072900940201     C                   MOVEA     IN4049        *IN(40)
073000100323     C*
073100100323     C*  se richiamato x selezionare un codice, --> 11 = *on
073200100323     C*   non deve utilizzare le opzioni di modifica e viceversa
073300040930     c     *in11         ifeq      *on
0734000409303    C     S1OPZ         IFeq      2
0735000409303    C     S1OPZ         OReq      3
0736000409303    C     S1OPZ         OReq      4
073700040930     c                   seton                                        41  99
073800040930     C                   END
073900100323     C*
074000040930      * selezionato un codice da restituire al chiamante
0741000409303    C     S1OPZ         IFeq      1
074200111220     c                   movel(p)  h1cod         x1cod
074300111220     c                   movel(p)  h1cod1        x1cod1
074400040930     c                   clear                   kpjbu
074500111220     c                   eval      kpjbu = x1cod + x1cod1
074600040930     C                   MOVEL     *ON           $ESCI
074700040930     C                   MOVEL     *ON           $fine
074800040930     C                   END
074900040930     C                   END
075000040930     C*
075100040930     c     *in11         ifeq      *off
0752000409303    C     S1OPZ         ANDeq     1
075300040930     c                   seton                                        41  99
075400040930     C                   END
075500040930     C*
075600940131     C                   ENDSR
075700940131     C************************************************************
075800940131     C* GESTIONE OPZIONI LISTA
075900940131     C************************************************************
076000940131     C     OPZS1         BEGSR
076100940201     C*
0762000409302    C     *IN11         IFEQ      *Off
0763000409302    C     *IN11         oreq      *On
0764000409302    C     s1opz         andeq     05
076500040930     C*
076600030113     C                   RESET                   tabds
076700100323     c                   move      *zero         xtaopz
076800100323     C                   MOVE      S1OPZ         xtaopz
076900030113     C                   MOVE      *ZERO         xtaret
077000030113     C                   MOVE      *ZERO         xtaopr
077100100409     C                   MOVEl(p)  h1COD         xtakey1
077200111220     C                   MOVEl(p)  h1COD1        xtakey2
077300111220     C                   MOVEl     *blank        xtacod1
077400111220     C                   MOVEl     *blank        xtacod2
077500111220     C                   MOVEl     savUNBcli     xtaUNBcli
077600940715     C                   MOVE      *BLANKS       KPJBU
077700030113     C                   MOVEL     tabds         KPJBU
077800100622$004 C                   CALL      'TNTBQEMR2'
077900940607     C                   PARM                    KPJBA
078000090225      *
078100030114     C                   MOVEL     KPJBU         tabds
078200940201     C*
078300940223     C* ritorno da PGM gestione
078400940223     C                   EXSR      GESRET
078500040930     C*
078600940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0787009402252    C     *IN99         IFEQ      *ON
078800940223     C                   SETON                                        40
0789009402252-   C                   ENDIF
079000940225     C*
0791000409302-   C                   ENDIF
079200040930     C*
079300940131     C                   ENDSR
079400940223     C/EJECT
079500940223     C************************************************************
079600940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
079700940223     C************************************************************
079800940223     C     GESRET        BEGSR
079900940223     C*
080000940223     C* modo di ritorno
080100940223     C*
0802009402231    C                   SELECT
080300940314    >C* << questi modi di utilizzo dei valori di ritorno dal
080400940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
080500940314    >C*    proposte, normalmente sempre valide, ma modificabili
080600940314    >C*    per situazioni particolari >>
080700940223     C* 1 = F3
080800030113    >C     xtaret        WHENEQ    '1'
080900940224     C                   MOVE      *ON           $ESCI
081000940223     C                   MOVE      *ON           $FINE
081100940223     C* 2 = F12
081200030113    >C     xtaret        WHENEQ    '2'
081300940223     C                   MOVE      *ON           $ESCI
081400940223     C*
0815009402231-   C                   ENDSL
081600940223     C*
081700940223     C* operazione eseguite dal pgm chiamato
081800940223     C*
0819009402231    C                   SELECT
082000940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
082100030113    >C     xtaopr        WHENEQ    '1'
082200940223     C                   MOVE      *ON           WINZS1
082300940223     C*
0824009402231-   C                   ENDSL
082500940223     C*
082600940223     C* funzione non eseguibile per errore :
082700940223     C*
0828009402231    C                   SELECT
082900940223     C* 1 = funzione richiamata chiusa in errore
083000940316    >C*  eventualmente gestire altri codici di errore
083100030113    >C     xtaerr        WHENEQ    '1'
083200940223     C                   MOVE      *ON           $ESCI
083300940223     C                   SETON                                        5299
083400940223     C*
0835009402231-   C                   ENDSL
083600940223     C*
083700940223     C                   ENDSR
083800940223     C/EJECT
083900940131     C************************************************************
084000940131     C* OPERAZIONI INIZIALI
084100940131     C************************************************************
084200940131     C     *INZSR        BEGSR
084300030113     C*
084400030113     C* Reperimento parametri
084500030113     C     *ENTRY        PLIST
084600030113     C                   PARM                    KPJBA
084700111220     C*
084800111220     C                   eval        TABDS = KPJBU
084900111220     c                   eval      savUNBCLI = xtaUNBCLI
085000040930     C*
085100111220     c     key_STS       Klist
085200111220     c                   kfld                    stsUNBCLI
085300111220     c                   kfld                    stsCODSTS
085400111220     c                   kfld                    stsSUBSTS
085500100617      *
085600100617     C* RECUPERO DATI DELL'UTENTE
085700100617     C                   Z-ADD     1             CODUT             1 0
085800100617     C                   CALL      'XPARUT'
085900100617     C                   PARM                    UTEDSE0F
086000100617     C                   MOVEL     RAGUT         RSUT             20
086100100326     C*
086200040930     C* Se chiamato x selezionare un codice
086300040930     c                   clear                   x1cod
086400111220     c                   clear                   x1cod1
086500030113     C*
086600030113     C* Variabili per gestione videate
086700030113     C                   MOVE      *BLANK        $GEST             2
086800030113     C                   MOVE      *BLANK        $FINE             1
086900100319     C                   MOVE      *BLANK        $INZS2            1
087000030113     C                   MOVE      *BLANK        $INZS1            1
087100090225     C                   MOVE      *BLANK        $INZS0            1
087200030113     C                   MOVE      *BLANK        $EFILE            1
087300030113     C                   MOVE      *BLANK        $ESCI             1
087400030113     C                   MOVE      *BLANK        $RCDOK            1
087500030113     C* Indici
087600030113     C                   Z-ADD     0             X                 3 0
087700030113     C                   Z-ADD     0             Y                 3 0
087800940506     C*
087900940506     C* Reperimento tasti di funzione
088000100326     C*
088100100326     C                   clear                   c0pos
088200111220     C                   clear                   c0pos1
088300100326     C                   clear                   SAVc0pos
088400111220     C                   clear                   SAVc0pos1
088500940127     C*
088600940117     C                   ENDSR
088700100319     C************************************************************
