000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
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
002200100119     Ftntbe01L  IF   E           K DISK
002300100617     FCNACO00F  IF   E           K DISK
002400100617     FAZORG01L  IF   E           K DISK
002500100622     FTNTBQEMD  CF   E             WORKSTN
002600940607     F                                     SFILE(S1:S1NRR)
002700090225     F                                     SFILE(S0:S0NRR)
002800940201     F                                     INFDS(DSFMT)
002900940128     D*----------------------------------------------------*
003000940211     D* Passaggio Parametri
003100940211     D KPJBA         E DS
003200100622     D tbeUNI_dQEM   E DS                  EXTNAME(DQEM)
003300100322     D                                     PREFIX(tbeUNI_)
003400100622     D DQEM          E DS
003500100617     D UTEDSE0F      E DS
003600100617     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
003700100617     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
003800100617      * Ds scomposzione tipo capoconti
003900100617     D TCUDS           DS
004000100617     D  F34                    3      4
004100100617     D  F56                    5      6
004200100617      *
004300030113      *-------------
004400940325     D* Parametri in ricezione
004500030113     D  TABDS          DS
004600030113     D  XTAOPZ                 1      2
004700030113     D  XTARET                 3      3
004800030113     D  XTAOPR                 4      4
004900030113     D  XTAERR                 5      5
005000100409     D  XTAKEY1                6     20
005100100409     D  XTAKEY2               21     30
005200100409     D  XTACOD                31     33
005300940211     D*-------------
005400940211     D DSFMT           DS           512
005500940506     D  $TASTO               369    369
005600940211     D  NRG                  370    370
005700940211     D  NCL                  371    371
005800940211     D  SFLNRR               378    379B 0
005900940207     D*-------------
006000940207     D* Nome PGM a video
006100940207     D                 DS
006200940207     D  PROGR                  1     10
006300940207     D  ASTER1                 1      1    INZ('*')
006400940207     D  SIGLA                  2      9
006500940207     D  ASTER2                10     10    INZ('*')
006600940127     D*-------------
006700940127     D* Reperimento nome PGM
006800940127     D STATUS         SDS           333
006900940127     D  DSPGM            *PROC
007000030113     D*-------------
007100100618$003 D KKCC            S                   Like(acokcc)
007200100618$003 D KKut            S                   Like(acokut)
007300100618$003 D KKSC            S                   Like(acoksc)
007400100618     D*-------------
007500030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
007600090225$003 D S0NRR           S                   Like(C1rcd)
007700090225$003 D S1NRR           S                   Like(C1rcd)
007800100319$003 D S2NRR           S                   Like(C1rcd)
007900030113$003 D WSfl            S                   Like(C1nrr)
008000030113$003 D Wmax            S                   Like(C1rcd)
008100030113$003 D Wpag            S                   Like(C1rcd)
008200090225$003 D Winzs0          S                   Like($inzs0)
008300090225$003 D Winzs1          S                   Like($inzs1)
008400100319$003 D Winzs2          S                   Like($inzs2)
008500100324$003 D x1cod           S                   Like(h1cod)
008600090225$003 D x0cod           S                   Like(s0cod)
008700100326$003 D savC0pos        S                   Like(C0POS)
008800090225     D*-------------
008900100119     d dataiso         s               d   datfmt(*iso)
009000100119     d dataeur         s               d   datfmt(*eur)
009100100622     d sav_cod_QEM     s              7
009200100617     d sav_cod_FIL     s              3
009300940207     D*-------------
009400940211     D* COSTANTI
009500940211     D*-------------
009600090225     D SFLPAG0         C                   CONST(11)
009700100617     D SFLPAG1         C                   CONST(12)
009800940314     D* dimensione della schiera $MS1
009900940506     D*
010000940506     D* Tasti di funzione
010100940506     D F01             C                   CONST(X'31')
010200940506     D F02             C                   CONST(X'32')
010300940506     D F03             C                   CONST(X'33')
010400940506     D F04             C                   CONST(X'34')
010500940506     D F05             C                   CONST(X'35')
010600940506     D F06             C                   CONST(X'36')
010700940506     D F07             C                   CONST(X'37')
010800940506     D F08             C                   CONST(X'38')
010900940506     D F09             C                   CONST(X'39')
011000940506     D F10             C                   CONST(X'3A')
011100940506     D F11             C                   CONST(X'3B')
011200940506     D F12             C                   CONST(X'3C')
011300940506     D F13             C                   CONST(X'B1')
011400940506     D F14             C                   CONST(X'B2')
011500940506     D F15             C                   CONST(X'B3')
011600940506     D F16             C                   CONST(X'B4')
011700940506     D F17             C                   CONST(X'B5')
011800940506     D F18             C                   CONST(X'B6')
011900940506     D F19             C                   CONST(X'B7')
012000940506     D F20             C                   CONST(X'B8')
012100940506     D F21             C                   CONST(X'B9')
012200940506     D F22             C                   CONST(X'BA')
012300940506     D F23             C                   CONST(X'BB')
012400940506     D F24             C                   CONST(X'BC')
012500940506     D ENTER           C                   CONST(X'F1')
012600940506     D ROLDWN          C                   CONST(X'F4')
012700940506     D ROLLUP          C                   CONST(X'F5')
012800940127     C*----------------------------------------------------*
012900940127     C*                MAIN LINE PROGRAM
013000940127     C*----------------------------------------------------*
013100940223     C* inizializzazione variabili
013200940223     C                   EXSR      INZVAR
013300940223     C*
013400940223     C     $FINE         DOWEQ     *OFF
013500090225     C     $GEST         CASEQ     'S0'          GESS0
013600940131     C     $GEST         CASEQ     'S1'          GESS1
013700940117     C                   END
013800940117     C                   END
013900940325     C* fine programma
014000940325     C                   SETON                                            LR
014100030113     C************************************************************
014200030113     C* INIZIALIZZAZIONE VARIABILI
014300030113     C************************************************************
014400030113     C     INZVAR        BEGSR
014500030113     C*
014600030113     C* Pulizia campi e indicatori
014700030113     C                   MOVE      *ALL'0'       IN4049           10
014800030113     C                   MOVEA     IN4049        *IN(40)
014900030113     C                   CLEAR                   S1OPZ
015000090225     C                   CLEAR                   S0OPZ
015100030113     C*
015200090225     C* Variabili per gestione videate
015300030113     C                   MOVE      *OFF          $FINE
015400030113     C                   MOVE      *OFF          $EFILE
015500030113     C                   MOVE      *OFF          $ESCI
015600030113     C                   MOVE      *OFF          $RCDOK
015700030113     C                   Z-ADD     0             $ULKS1            3 0
015800030113     C*
015900100319     C                   MOVE      *ON           $INZS2
016000030113     C                   MOVE      *ON           $INZS1
016100090225     C                   MOVE      *ON           $INZS0
016200030113     C* Variabili appoggio
016300030114     C                   Z-ADD     1             WPAG
016400030113     c*
016500100407     c* Video da cui partire
016600100622     c                   movel(p)  'QEM'         tbe_QEM           3
016700100407     C                   MOVE      'S0'          $GEST
016800100407     c*
016900030113     C                   ENDSR
017000090225     C************************************************************
017100090225     C* GESTIONE LISTA
017200090225     C************************************************************
017300090225     C     GESS0         BEGSR
017400090225     C*
017500090225     C* inizializzazione videata
017600090225     C     $INZS0        IFEQ      *ON
017700090225     C                   EXSR      INZS0
017800090225     C                   MOVE      *OFF          $INZS0
017900090225     C                   ENDIF
018000100325     C*
018100090225     C* emissione piede videata
018200090225     C                   WRITE     Z0
018300090225     C* Non ci sono records
018400090225     C     WMAX          IFEQ      0
018500090225     C                   WRITE     D0
018600090225     C                   Else
018700090225     C     Wsfl          IFgt      0
018800090225     C                   Z-ADD     wsfl          C0RCD
018900090225     C                   Else
019000090225     C     Wpag          IFgt      0
019100090225     C                   Z-ADD     wpag          C0RCD
019200090225     C                   EndIF
019300090225     C                   EndIF
019400090225     C                   ENDIF
019500090225     C*
019600090225     C*              *------------------*
019700090225     C                   EXFMT     C0
019800090225     C*              *------------------*
019900090225     C*
020000090225     C     C0NRR         IFNE      0
020100090225     C                   Z-ADD     C0NRR         WSFL
020200090225     C                   ENDIF
020300090225     C                   Z-ADD     SFLNRR        C0RCD
020400090225     C* Selezioni
0205000902251    C                   SELECT
020600090225     C* F3=Fine
020700090225     C     $TASTO        WHENEQ    F03
020800090225     C                   EXSR      F03S1
020900100121     C* F10=Immissione
021000100121     C     $TASTO        WHENEQ    F10
021100100121     C                   EXSR      F10S1
021200100325     C                   MOVE      'S0'          $GEST
0213000902251O   C                   OTHER
021400090225     C* CONTROLLO DATI
021500090225     C                   EXSR      CTRC0
021600090225     C     *IN99         IFEQ      *OFF
021700090225     C                   EXSR      CTRS0
021800090225     C                   END
0219000902251-   C                   ENDSL
022000090225     C*
022100090225     C                   ENDSR
022200090225     C/EJECT
022300090225     C************************************************************
022400090225     C* INIZIALIZZAZIONE LISTA
022500090225     C************************************************************
022600090225     C     INZS0         BEGSR
022700100407     C*
022800090225     C* pulizia SFL
022900090225     C                   SETOFF                                         3031
023000090225     C                   WRITE     C0
023100090225     C                   SETON                                          31
023200090225     C*
023300090225     C* CARICAMENTO SFL totale
023400090225     C                   Z-ADD     0             S0NRR
023500090225     C                   Z-ADD     1             C0RCD
023600090225     C                   Z-ADD     0             WMAX
023700090225     C                   Z-ADD     0             wsfl
023800090225     C*
023900090225     C*  Posizionamento all'inizio
024000100617     c                   movel(p)  c0pos         tbe_COD          15
024100100326     C                   IF        Posiziona = 'S'
024200100622     c     tbe_cod_QEM   setll     tnTBE01L
024300100326     C                   eval      Posiziona = *blank
024400100326     c                   else
024500100622     c     tbe_QEM       setll     tnTBE01L
024600100326     c                   end
024700100119    >C                   EXSR      READ_TBE
024800090225     C* Carico il SFL
024900090225     C                   EXSR      ROLS0
025000090225     C*
025100090225     c                   if        xtaopr <> '1'
025200090225     C                   Z-ADD     1             WPAG
025300090225     c                   end
025400090225     C*
025500090225     C                   ENDSR
025600090225     C************************************************************
025700090225     C* CARICAMENTO PAGINA LISTA
025800090225     C************************************************************
025900090225     C     ROLS0         BEGSR
026000090225     C*
026100090225     C                   SETOFF                                       32
026200090225     C                   Z-ADD     0             Y
026300090225     C                   Z-ADD     WMAX          S0NRR
026400090225     C*
026500100119     C*  Selezionarle Tutte
026600100617     c                   eval      s0cod =  '999'
026700100617     c                   eval      s0des =  'T U T T I'
026800100119     C                   ADD       1             S0NRR
026900100119     C                   ADD       1             Y
027000100119     C                   WRITE     S0
027100100119     C*
027200090225     C* Leggo dal file i segmenti presenti nell'archivio
0273000902251    C     $EFILE        DOWEQ     *OFF
027400100617     C*
027500100617     c                   movel     tbeKe1        cod_FIL           3
027600100617     C*
027700100617     c                   if        cod_FIL <> sav_cod_FIL
027800100617     c*
027900100617     c                   clear                   s0opz
028000100617     c* Prendo tipo segmento generale
028100100617     c                   eval      s0cod =  cod_FIL
028200100617     c                   movel     cod_FIL       num_FIL           3 0
028300100617     c     num_FIL       chain     azorg01l
028400100617     c                   if        %Found(azorg01l)
028500100617     c                   eval      s0des =  orgDES
028600100617     c                   end
028700090225     C*
028800090225     C                   ADD       1             S0NRR
028900090225     C                   ADD       1             Y
029000090225     C                   WRITE     S0
029100100119     C*
029200100617     c                   eval       sav_cod_FIL = cod_FIL
029300100119     c                   end
029400100119     C*
029500100119    >C                   EXSR      READ_TBE
029600090225     C*
0297000902251-   C                   ENDDO
029800090225     C*
029900090225     C                   Z-ADD     S0NRR         WMAX                 30
030000090225     C*
030100090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
030200090225     C*
030300090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
030400090225     C                   MVR                     RESTO             3 0
030500090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0306000902251    C     RESTO         IFGT      0
030700090225     C                   ADD       1             C0RCD
0308000902251E   C                   ELSE
030900090225     C                   SUB       SFLPAG0       C0RCD
031000090225     C                   ADD       1             C0RCD
0311000902251-   C                   ENDIF
031200090225     C*
031300090225     C                   ENDSR
031400090225     C************************************************************
031500090225     C* LETTURA RCD
031600090225     C************************************************************
031700100119     C     READ_TBE      BEGSR
031800090225     C*
031900090225     C                   MOVEL     *OFF          $EFILE
032000090225     C                   MOVEL     *OFF          $RCDOK
032100090225     C*
0322000902251    C     $EFILE        DOUEQ     *ON
032300090225     C     $RCDOK        OREQ      *ON
032400090225     C*
032500100622     c     tbe_QEM       Reade     tntbe01L
032600100119     c                   if        %eof(tntbe01L)
032700090225     C                   MOVEL     *ON           $EFILE
032800090225     C                   MOVE      $EFILE        *IN33
032900090225     c                   else
033000100322      **
033100100322     c                   eXSr      Selez_RECord
033200100119     c                   end
033300090225     C*
0334000902251-   C                   ENDDO
033500090225     C*
033600090225     C                   ENDSR
033700090225     C************************************************************
033800090225     C* CONTROLLO TESTATA LISTA
033900090225     C************************************************************
034000100322     C     Selez_RECord  BEGSR
034100100407      *
034200100322     C*  imposta i flags appena letti nella DS
034300100622     c                   movel     tbeUNI        tbeUNI_dQEM
034400100618     C                   MOVEL     *ON           $RCDOK
034500100322     C*
034600100322     C     End_filtro    ENDSR
034700100322     C************************************************************
034800100322     C* CONTROLLO TESTATA LISTA
034900100322     C************************************************************
035000100322     C     CTRC0         BEGSR
035100100322     C*
035200100326     C                   MOVE      *OFF          *IN99
035300100326     C*
035400100326     C                   clear                   posiziona         1
035500100326     c                   if        c0pos <> SAVc0pos
035600100326     C                   move      'S'           posiziona         1
035700100326     c                   eval      SAVc0pos = c0pos
035800100326     c                   move      *ON           $INZS0
035900100326     c                   end
036000090225     C*
036100090225     C                   ENDSR
036200090225     C************************************************************
036300090225     C* CONTROLLO OPZIONI LISTA
036400090225     C************************************************************
036500090225     C     CTRS0         BEGSR
036600090225     C*
036700090225     C                   MOVEL     *OFF          $ESCI
036800090225     C                   SETOFF                                       99
036900090225     C                   clear                   S0OPZ
037000090225     c                   clear                   x0cod
037100090225     C*
037200090225     C* Leggo il sfl solo se ci sono rcd
0373000902251    C     WMAX          IFGT      0
037400090225     C                   READC     S0                                     21
037500090225     C*
037600090225     C* esce se fine sfl o errore che richiede l'uscita
0377000902252    C     *IN21         DOWEQ     *OFF
037800090225     C     $ESCI         ANDEQ     *OFF
037900090225     C                   Z-ADD     S0NRR         C0RCD
038000090225     C* ctrl su riga
038100090225     C                   EXSR      RECS0
038200090225     C* gestione opzioni
0383000902253    C     S0OPZ         IFNE      *blank
038400090225     C     *IN99         ANDEQ     *OFF
038500090225     C                   EXSR      OPZS0
0386000902253-   C                   ENDIF
038700090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0388000902253    C     *IN99         IFEQ      *ON
038900090225     C     $ESCI         OREQ      *ON
039000090225     C                   MOVE      *ON           *IN32
039100090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
039200090225     C* la ripristinerò a conclusione del ciclo di READC
039300090225     C                   MOVE      *OFF          $INZS0
0394000902253-   C                   ENDIF
039500090225     C*
039600090225     C                   clear                   S0OPZ
039700090225     C*
039800090225     C                   UPDATE    S0
039900090225     C*
040000090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0401000902253    C     $ESCI         IFEQ      *OFF
040200090225     C                   READC     S0                                     21
040300090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0404000902254    C     *IN21         IFEQ      *ON
040500090225     C                   MOVE      WINZS0        $INZS0
040600090225     C* calcolo pagina a cui deve posizionarsi
040700090225     C                   EXSR      CLCPAG0
0408000902254-   C                   ENDIF
0409000902253-   C                   ENDIF
041000090225     C*
0411000902252-   C                   ENDDO
041200090225     C*
0413000902251-   C                   ENDIF
041400090225     C*
041500090225     C                   ENDSR
041600090225     C/EJECT
041700090225     C************************************************************
041800090225     C* CONTROLLO CAMPI I/O RIGA LISTA
041900090225     C************************************************************
042000090225     C     RECS0         BEGSR
042100090225     C*
042200090225     C* reset indicatori DSPATR
042300090225     C                   MOVE      *ALL'0'       IN4049           10
042400090225     C                   MOVEA     IN4049        *IN(40)
042500090225     C*
042600090225     C                   ENDSR
042700090225     C************************************************************
042800090225     C* GESTIONE OPZIONI LISTA
042900090225     C************************************************************
043000090225     C     OPZS0         BEGSR
043100090225     C*
043200090225      * selezionato un codice da restituire al chiamante
0433000902253    C     S0OPZ         IFeq      '1'
043400100618     c                   move      s0cod         codiceFIL         3
043500090225     C                   MOVE      'S1'          $GEST
043600090225     C                   MOVEL     *ON           $ESCI
043700090225     C                   END
043800090225     C*
043900090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0440000902252    C     *IN99         IFEQ      *ON
044100090225     C                   SETON                                        40
0442000902252-   C                   ENDIF
044300090225     C*
044400090225     C                   ENDSR
044500090225     C/EJECT
044600940127     C************************************************************
044700940131     C* GESTIONE LISTA
044800940127     C************************************************************
044900940127     C     GESS1         BEGSR
045000030113     C*
045100940223     C* inizializzazione videata
045200940223     C     $INZS1        IFEQ      *ON
045300940127     C                   EXSR      INZS1
045400940223     C                   MOVE      *OFF          $INZS1
045500940127     C                   ENDIF
045600030113     C*
045700030113     C* emissione piede videata
045800030113     C                   WRITE     Z1
045900030113     C* Non ci sono records
046000940223     C     WMAX          IFEQ      0
046100940607     C                   WRITE     D1
046200030114     C                   Else
046300030114     C     Wsfl          IFgt      0
046400030114     C                   Z-ADD     wsfl          C1RCD
046500030114     C                   Else
046600030114     C     Wpag          IFgt      0
046700030114     C                   Z-ADD     wpag          C1RCD
046800030114     C                   EndIF
046900030114     C                   EndIF
047000030114     C                   ENDIF
047100940127     C*
047200030113     C*              *------------------*
047300940607     C                   EXFMT     C1
047400030113     C*              *------------------*
047500030113     C*
047600940204     C     C1NRR         IFNE      0
047700940204     C                   Z-ADD     C1NRR         WSFL
047800940204     C                   ENDIF
047900940127     C                   Z-ADD     SFLNRR        C1RCD
048000030113     C* Selezioni
0481009401271    C                   SELECT
048200940127     C* F3=Fine
048300940506     C     $TASTO        WHENEQ    F03
048400940309     C                   EXSR      F03S1
048500940131     C* F10=Immissione
048600940506     C     $TASTO        WHENEQ    F10
048700940309     C                   EXSR      F10S1
048800100325     C                   MOVE      'S1'          $GEST
048900090225     C* F12=Ritorno
049000090225     C     $TASTO        WHENEQ    F12
049100090225     C                   eval      $Gest='S0'
049200090225     C                   MOVE      *ON           $INZS1
049300090225     C                   MOVE      *ON           $INZS0
049400100407      *
049500100326     C* se c'era un posizionamento
049600100326     c                   if        c0POS <> *blank
049700100326     c                   eval      SAVc0pos = c0pos
049800100326     c                   eval      posiziona = 'S'
049900100326     c                   end
050000100407      *
0501009401271O   C                   OTHER
050200940127     C* CONTROLLO DATI
050300940131     C                   EXSR      CTRC1
050400940201     C     *IN99         IFEQ      *OFF
050500940131     C                   EXSR      CTRS1
050600940131     C                   END
0507009401271-   C                   ENDSL
050800940127     C*
050900940127     C                   ENDSR
051000940224     C/EJECT
051100940127     C************************************************************
051200940131     C* INIZIALIZZAZIONE LISTA
051300940127     C************************************************************
051400940127     C     INZS1         BEGSR
051500100322     C*
051600940302     C* pulizia SFL
051700100121     C                   SETOFF                                       423031
051800940607     C                   WRITE     C1
051900940128     C                   SETON                                          31
052000940128     C*
052100030113     C* CARICAMENTO SFL totale
052200940201     C                   Z-ADD     0             S1NRR
052300030113     C                   Z-ADD     1             C1RCD
052400940128     C                   Z-ADD     0             WMAX
052500090225     C                   Z-ADD     0             wsfl
052600100618     c                   if        codiceFIL = *blank
052700100618     c                   eval      codiceFIL = '999'
052800100121     c                   end
052900100618     C                   move      codiceFIL     c1FIL             3
053000100618     c                   movel(p)  codiceFIL     tbe_COD
053100940224     C*
053200940224     C* Posizionamento su file pilota
053300100618     c                   if        codiceFIL = '999'
053400100622     c     tbe_QEM       setll     tntbe01L
053500100618     c                   else
053600100622     c     tbe_cod_QEM   setll     tntbe01L
053700100618     c                   end
053800100618     C*
053900100119    >C                   EXSR      READ_tbe
054000030113     C* Carico il SFL
054100940127     C                   EXSR      ROLS1
054200030113     C*
054300030114     c                   if        xtaopr <> '1'
054400030114     C                   Z-ADD     1             WPAG
054500030114     c                   end
054600940127     C*
054700940127     C                   ENDSR
054800940127     C************************************************************
054900940131     C* CARICAMENTO PAGINA LISTA
055000940127     C************************************************************
055100940127     C     ROLS1         BEGSR
055200940127     C*
055300940128     C                   SETOFF                                       32
055400940223     C                   Z-ADD     0             Y
055500940127     C                   Z-ADD     WMAX          S1NRR
055600940127     C*
055700940127     C* Leggo dal file anagrafico per caricare la lista
0558009401311    C     $EFILE        DOWEQ     *OFF
055900100618     c                   movel     tbeKe1        cod_FIL           3
056000100119     C*
056100100618     c                   if        cod_FIL = c1FIL or c1FIL = '999'
056200100317     c*
056300030113     c                   clear                   s1opz
056400100324     c                   eval      h1cod = tbeKE1
056500100324     c                   movel(p)  tbeke1        s1cod
056600100617     c                   clear                   s1DES
056700100617     c                   clear                   s1TRS
056800100617     c                   clear                   s1DET
056900100617     c                   clear                   s1ECC
057000100617      *
057100100617      * Decodifico partner
057200100617     C                   Z-ADD     1             KKUT
057300100617     C                   Z-ADD     KCI           KKCC
057400100617     C                   MOVE      *ZEROS        KKSC
057500100617     C                   MOVE      S1COD         KKSC
057600100618     C     KACO          CHAIN     CNACO00F                           21
057700100617     c                   if        %Found(CNACO00F)
057800100617     c                   eval      s1des =  ACORAG
057900100119     c                   end
058000100622     c                   eval      dQEM   = tbeUNI
058100100622     c                   MOVEL     §QEMDTB       s1TRS
058200100622     c                   MOVEL     §QEMSGD       s1DET
058300100622     c                   MOVEL     §QEMECC       s1ECC
058400940127     C*
058500100323     c                   eval      h1ANNulla = *off
058600100121     c                   if        tbeATB <> *blank
058700100323     c                   move(p)   'ANNULLATO'   s1des
058800100323     c                   eval      h1ANNulla = *on
058900100121     c                   seton                                        42
059000100121     c                   end
059100100121     C*
059200940127     C                   ADD       1             S1NRR
059300940127     C                   ADD       1             Y
059400940607     C                   WRITE     S1
059500100121     c                   setoff                                       42
059600100119     c                   end
059700940131     C*
059800100119    >C                   EXSR      READ_tbe
059900100618     c                   if        codiceFIL <>'999' and
060000100618     c                             codiceFIL <> %subst(tbeKe1:1:3)
0601001006181    C                   eval      $EFILE = *ON
060200100618     c                   end
0603009401271-   C                   ENDDO
060400940127     C*
060500940223     C                   Z-ADD     S1NRR         WMAX                 30
060600940127     C*
060700940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
060800090225     C     S1NRR         DIV       SFLPAG1       PAGINE            4 0
060900940127     C                   MVR                     RESTO             3 0
061000090225     C     PAGINE        MULT      SFLPAG1       C1RCD
0611000301141    C     RESTO         IFGT      0
061200030114     C                   ADD       1             C1RCD
0613000301141E   C                   ELSE
061400090225     C                   SUB       SFLPAG1       C1RCD
061500030114     C                   ADD       1             C1RCD
0616000301141-   C                   ENDIF
061700940128     C*
061800940127     C                   ENDSR
061900940224     C************************************************************
062000940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
062100940224     C************************************************************
062200090225     C     CLCPAG0       BEGSR
062300940224     C* Input :
062400940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
062500940224     C*          cursore
062600940224     C* - SFLPAG = numero rcd per pagina sfl
062700940224     C* Output :
062800940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
062900940224     C*
063000090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
063100940224     C                   MVR                     RESTO             3 0
063200940224     C     RESTO         IFGT      0
063300940224     C                   ADD       1             PAGINE
063400940224     C                   ENDIF
063500090226     c                   if        pagine > 1
063600090225     C     PAGINE        MULT      SFLPAG0       WPAG
063700090226     C                   END
063800940224     C*
063900940224     C                   ENDSR
064000090225     C************************************************************
064100090225     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
064200090225     C************************************************************
064300090225     C     CLCPAG1       BEGSR
064400090225     C* Input :
064500090225     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
064600090225     C*          cursore
064700090225     C* - SFLPAG = numero rcd per pagina sfl
064800090225     C* Output :
064900090225     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
065000090225     C*
065100090225     C     WSFL          DIV       SFLPAG1       PAGINE            4 0
065200090225     C                   MVR                     RESTO             3 0
065300090225     C     RESTO         IFGT      0
065400090225     C                   ADD       1             PAGINE
065500090225     C                   ENDIF
065600090226     c                   if        pagine > 1
065700090225     C     PAGINE        MULT      SFLPAG1       WPAG
065800090226     C                   ENDIF
065900090225     C*
066000090225     C                   ENDSR
066100940309     C************************************************************
066200940309     C* GESTIONE F03 VIDEO S1
066300940309     C************************************************************
066400940309     C     F03S1         BEGSR
066500940309     C*
066600940309     C                   MOVE      *ON           $FINE
066700940325     C* fine programma
066800940309     C                   ENDSR
066900940309     C/EJECT
067000940309     C************************************************************
067100940309     C* GESTIONE F10 VIDEO S1
067200940314     c* AGGIUNTA RECORD
067300940309     C************************************************************
067400940309     C     F10S1         BEGSR
067500940309     C*
067600030113     C                   RESET                   tabds
067700030113     C                   MOVEL     '01'          xtaopz
067800030113     C                   MOVE      *ZERO         xtaret
067900030113     C                   MOVE      *ZERO         xtaopr
068000100409     C                   MOVEl     *blank        xtakey1
068100100409     C                   MOVEl     *blank        xtakey2
068200100622     C                   MOVEl     'QEM'         xtacod
068300030113     C                   MOVE      *BLANKS       KPJBU
068400030113     C                   MOVEL     tabds         KPJBU
068500100622$004 C                   CALL      'TNTBQEMR2'
068600030113     C                   PARM                    KPJBA
068700090225      *
068800030114     C                   MOVEL     KPJBU         tabds
068900030113      *
069000940309     C* ritorno da PGM gestione
069100940309     C                   EXSR      GESRET
069200090226      *
069300940309     C     WINZS1        IFEQ      *ON
069400940309     C                   MOVE      *ON           $INZS1
069500090226     C* carico sempre la 1a pagina
069600090226     C                   Z-ADD     1             WPAG
069700940309     C                   ENDIF
069800940309     C*
069900940309     C                   ENDSR
070000100322     C************************************************************
070100100322     C* CONTROLLO TESTATA LISTA
070200100322     C************************************************************
070300100322     C     CTRC1         BEGSR
070400100322     C*
070500940201     C                   MOVE      *OFF          *IN99
070600940131     C*
070700940202     C                   ENDSR
070800940131     C************************************************************
070900940131     C* CONTROLLO OPZIONI LISTA
071000940131     C************************************************************
071100940131     C     CTRS1         BEGSR
071200940131     C*
071300940202     C                   MOVEL     *OFF          $ESCI
071400940201     C                   SETOFF                                       99
071500940131     C                   Z-ADD     0             S1OPZ
071600040930     c                   clear                   x1cod
071700940131     C*
071800940127     C* Leggo il sfl solo se ci sono rcd
0719009401311    C     WMAX          IFGT      0
072000940607     C                   READC     S1                                     21
072100940127     C*
072200940131     C* esce se fine sfl o errore che richiede l'uscita
0723009401312    C     *IN21         DOWEQ     *OFF
072400940131     C     $ESCI         ANDEQ     *OFF
072500940201     C                   Z-ADD     S1NRR         C1RCD
072600940131     C* ctrl su riga
072700940131     C                   EXSR      RECS1
072800940131     C* gestione opzioni
0729009401313    C     S1OPZ         IFNE      0
073000940201     C     *IN99         ANDEQ     *OFF
073100940131     C                   EXSR      OPZS1
0732009401273-   C                   ENDIF
073300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0734009402013    C     *IN99         IFEQ      *ON
073500940201     C     $ESCI         OREQ      *ON
073600940131     C                   MOVE      *ON           *IN32
073700940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
073800940204     C* la ripristinerò a conclusione del ciclo di READC
073900940223     C                   MOVE      *OFF          $INZS1
0740009402233-   C                   ENDIF
074100940223     C*
074200100323     C  n99              Z-ADD     0             S1OPZ
074300100121      ****
074400100323     c     h1annulla     comp      *on                                    42
074500940223     C*
074600940607     C                   UPDATE    S1
074700940223     C*
074800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0749009401313    C     $ESCI         IFEQ      *OFF
075000940607     C                   READC     S1                                     21
075100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0752009402014    C     *IN21         IFEQ      *ON
075300940201     C                   MOVE      WINZS1        $INZS1
075400940204     C* calcolo pagina a cui deve posizionarsi
075500090225     C                   EXSR      CLCPAG1
0756009402014-   C                   ENDIF
0757009402013-   C                   ENDIF
075800940131     C*
0759009401272-   C                   ENDDO
076000940127     C*
0761009401311-   C                   ENDIF
076200940131     C*
076300940127     C                   ENDSR
076400940127     C/EJECT
076500940127     C************************************************************
076600940131     C* CONTROLLO CAMPI I/O RIGA LISTA
076700940127     C************************************************************
076800940131     C     RECS1         BEGSR
076900940131     C*
077000940201     C* reset indicatori DSPATR
077100940201     C                   MOVE      *ALL'0'       IN4049           10
077200940201     C                   MOVEA     IN4049        *IN(40)
077300100323     C*
077400100323     C*  se si è selezionato di ripristinare un record NON annullato
0775001003233    C     S1OPZ         IFeq      8
0776001003233    C     H1ANNulla     andeq     *off
077700100323     c                   seton                                        41  99
077800100323     C                   END
077900100323     C*
078000100323     C*  se invece il record era annullato si può solo vedere o ripristinare
0781001003233    C     H1ANNulla     ifeq      *on
0782001003233    C     S1OPZ         andne     8
0783001003233    C     S1OPZ         andne     5
078400100323     c                   seton                                        41  99
078500100323     C                   END
078600100323     C*
078700100323     C*  se richiamato x selezionare un codice, --> 11 = *on
078800100323     C*   non deve utilizzare le opzioni di modifica e viceversa
078900040930     c     *in11         ifeq      *on
0790000409303    C     S1OPZ         IFeq      2
0791000409303    C     S1OPZ         OReq      3
0792000409303    C     S1OPZ         OReq      4
0793001003233    C     S1OPZ         OReq      8
079400040930     c                   seton                                        41  99
079500040930     C                   END
079600100323     C*
079700040930      * selezionato un codice da restituire al chiamante
0798000409303    C     S1OPZ         IFeq      1
079900100324     c                   move      h1cod         x1cod
080000040930     c                   clear                   kpjbu
080100040930     c                   movel     x1cod         kpjbu
080200040930     C                   MOVEL     *ON           $ESCI
080300040930     C                   MOVEL     *ON           $fine
080400040930     C                   END
080500040930     C                   END
080600040930     C*
080700040930     c     *in11         ifeq      *off
0808000409303    C     S1OPZ         ANDeq     1
080900040930     c                   seton                                        41  99
081000040930     C                   END
081100040930     C*
081200940131     C                   ENDSR
081300940131     C************************************************************
081400940131     C* GESTIONE OPZIONI LISTA
081500940131     C************************************************************
081600940131     C     OPZS1         BEGSR
081700940201     C*
0818000409302    C     *IN11         IFEQ      *Off
0819000409302    C     *IN11         oreq      *On
0820000409302    C     s1opz         andeq     05
082100040930     C*
082200030113     C                   RESET                   tabds
082300100323     c                   move      *zero         xtaopz
082400100323     C                   MOVE      S1OPZ         xtaopz
082500030113     C                   MOVE      *ZERO         xtaret
082600030113     C                   MOVE      *ZERO         xtaopr
082700100409     C                   MOVEl(p)  h1COD         xtakey1
082800100617     C                   MOVEl(p)  *blank        xtakey2
082900100622     C                   MOVEl     'QEM'         xtacod
083000940715     C                   MOVE      *BLANKS       KPJBU
083100030113     C                   MOVEL     tabds         KPJBU
083200100622$004 C                   CALL      'TNTBQEMR2'
083300940607     C                   PARM                    KPJBA
083400090225      *
083500030114     C                   MOVEL     KPJBU         tabds
083600940201     C*
083700940223     C* ritorno da PGM gestione
083800940223     C                   EXSR      GESRET
083900040930     C*
084000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0841009402252    C     *IN99         IFEQ      *ON
084200940223     C                   SETON                                        40
0843009402252-   C                   ENDIF
084400940225     C*
0845000409302-   C                   ENDIF
084600040930     C*
084700940131     C                   ENDSR
084800940223     C/EJECT
084900940223     C************************************************************
085000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
085100940223     C************************************************************
085200940223     C     GESRET        BEGSR
085300940223     C*
085400940223     C* modo di ritorno
085500940223     C*
0856009402231    C                   SELECT
085700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
085800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
085900940314    >C*    proposte, normalmente sempre valide, ma modificabili
086000940314    >C*    per situazioni particolari >>
086100940223     C* 1 = F3
086200030113    >C     xtaret        WHENEQ    '1'
086300940224     C                   MOVE      *ON           $ESCI
086400940223     C                   MOVE      *ON           $FINE
086500940223     C* 2 = F12
086600030113    >C     xtaret        WHENEQ    '2'
086700940223     C                   MOVE      *ON           $ESCI
086800940223     C*
0869009402231-   C                   ENDSL
087000940223     C*
087100940223     C* operazione eseguite dal pgm chiamato
087200940223     C*
0873009402231    C                   SELECT
087400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
087500030113    >C     xtaopr        WHENEQ    '1'
087600940223     C                   MOVE      *ON           WINZS1
087700940223     C*
0878009402231-   C                   ENDSL
087900940223     C*
088000940223     C* funzione non eseguibile per errore :
088100940223     C*
0882009402231    C                   SELECT
088300940223     C* 1 = funzione richiamata chiusa in errore
088400940316    >C*  eventualmente gestire altri codici di errore
088500030113    >C     xtaerr        WHENEQ    '1'
088600940223     C                   MOVE      *ON           $ESCI
088700940223     C                   SETON                                        5299
088800940223     C*
0889009402231-   C                   ENDSL
089000940223     C*
089100940223     C                   ENDSR
089200940223     C/EJECT
089300940131     C************************************************************
089400940131     C* OPERAZIONI INIZIALI
089500940131     C************************************************************
089600940131     C     *INZSR        BEGSR
089700030113     C*
089800030113     C* Reperimento parametri
089900030113     C     *ENTRY        PLIST
090000030113     C                   PARM                    KPJBA
090100040930     C*
090200100622     c     tbe_cod_QEM   Klist
090300100622     c                   kfld                    tbe_QEM
090400100617     c                   kfld                    tbe_COD          15
090500100617      *
090600100617     C     KACO          KLIST
090700100617     C                   KFLD                    KKUT
090800100617     C                   KFLD                    KKCC
090900100617     C                   KFLD                    KKSC
091000100617      *
091100100617     C* RECUPERO DATI DELL'UTENTE
091200100617     C                   Z-ADD     1             CODUT             1 0
091300100617     C                   CALL      'XPARUT'
091400100617     C                   PARM                    UTEDSE0F
091500100617     C                   MOVEL     RAGUT         RSUT             20
091600100617     C*
091700100617     C* RICERCA CAPOCONTI
091800100617     C                   DO        50            X
091900100617     C                   MOVE      TCU(X)        TCUDS
092000100617     C     F56           IFEQ      'CG'
092100100617     C     F34           ANDEQ     '01'
092200100617     C                   Z-ADD     KCU(X)        KCI               4 0
092300100617     C                   END
092400100617     C                   END
092500100617     C*
092600100326     C*
092700040930     C* Se chiamato x selezionare un codice
092800040930     c                   clear                   x1cod
092900030113     C*
093000030113     C* Variabili per gestione videate
093100030113     C                   MOVE      *BLANK        $GEST             2
093200030113     C                   MOVE      *BLANK        $FINE             1
093300100319     C                   MOVE      *BLANK        $INZS2            1
093400030113     C                   MOVE      *BLANK        $INZS1            1
093500090225     C                   MOVE      *BLANK        $INZS0            1
093600030113     C                   MOVE      *BLANK        $EFILE            1
093700030113     C                   MOVE      *BLANK        $ESCI             1
093800030113     C                   MOVE      *BLANK        $RCDOK            1
093900030113     C* Indici
094000030113     C                   Z-ADD     0             X                 3 0
094100030113     C                   Z-ADD     0             Y                 3 0
094200940506     C*
094300940506     C* Reperimento tasti di funzione
094400100326     C*
094500100326     C                   clear                   c0pos
094600100326     C                   clear                   SAVc0pos
094700940127     C*
094800940117     C                   ENDSR
094900100319     C************************************************************
