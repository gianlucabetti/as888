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
002000100119     Ftntbe01L  IF   E           K DISK
002100100617     FCNACO00F  IF   E           K DISK
002200100617     FAZORG01L  IF   E           K DISK
002300100622     FTNTBQEMD  CF   E             WORKSTN
002400940607     F                                     SFILE(S1:S1NRR)
002500090225     F                                     SFILE(S0:S0NRR)
002600940201     F                                     INFDS(DSFMT)
002700940128     D*----------------------------------------------------*
002800940211     D* Passaggio Parametri
002900940211     D KPJBA         E DS
003000100622     D tbeUNI_dQEM   E DS                  EXTNAME(DQEM)
003100100322     D                                     PREFIX(tbeUNI_)
003200100622     D DQEM          E DS
003300100617     D UTEDSE0F      E DS
003400100617     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
003500100617     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
003600100617      * Ds scomposzione tipo capoconti
003700100617     D TCUDS           DS
003800100617     D  F34                    3      4
003900100617     D  F56                    5      6
004000100617      *
004100030113      *-------------
004200940325     D* Parametri in ricezione
004300030113     D  TABDS          DS
004400030113     D  XTAOPZ                 1      2
004500030113     D  XTARET                 3      3
004600030113     D  XTAOPR                 4      4
004700030113     D  XTAERR                 5      5
004800100409     D  XTAKEY1                6     20
004900100409     D  XTAKEY2               21     30
005000100409     D  XTACOD                31     33
005100940211     D*-------------
005200940211     D DSFMT           DS           512
005300940506     D  $TASTO               369    369
005400940211     D  NRG                  370    370
005500940211     D  NCL                  371    371
005600940211     D  SFLNRR               378    379B 0
005700940207     D*-------------
005800940207     D* Nome PGM a video
005900940207     D                 DS
006000940207     D  PROGR                  1     10
006100940207     D  ASTER1                 1      1    INZ('*')
006200940207     D  SIGLA                  2      9
006300940207     D  ASTER2                10     10    INZ('*')
006400940127     D*-------------
006500940127     D* Reperimento nome PGM
006600940127     D STATUS         SDS           333
006700940127     D  DSPGM            *PROC
006800030113     D*-------------
006900100618$003 D KKCC            S                   Like(acokcc)
007000100618$003 D KKut            S                   Like(acokut)
007100100618$003 D KKSC            S                   Like(acoksc)
007200100618     D*-------------
007300030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
007400090225$003 D S0NRR           S                   Like(C1rcd)
007500090225$003 D S1NRR           S                   Like(C1rcd)
007600100319$003 D S2NRR           S                   Like(C1rcd)
007700030113$003 D WSfl            S                   Like(C1nrr)
007800030113$003 D Wmax            S                   Like(C1rcd)
007900030113$003 D Wpag            S                   Like(C1rcd)
008000090225$003 D Winzs0          S                   Like($inzs0)
008100090225$003 D Winzs1          S                   Like($inzs1)
008200100319$003 D Winzs2          S                   Like($inzs2)
008300100324$003 D x1cod           S                   Like(h1cod)
008400090225$003 D x0cod           S                   Like(s0cod)
008500100326$003 D savC0pos        S                   Like(C0POS)
008600090225     D*-------------
008700100119     d dataiso         s               d   datfmt(*iso)
008800100119     d dataeur         s               d   datfmt(*eur)
008900100622     d sav_cod_QEM     s              7
009000100617     d sav_cod_FIL     s              3
009100940207     D*-------------
009200940211     D* COSTANTI
009300940211     D*-------------
009400090225     D SFLPAG0         C                   CONST(11)
009500100617     D SFLPAG1         C                   CONST(12)
009600940314     D* dimensione della schiera $MS1
009700940506     D*
009800940506     D* Tasti di funzione
009900940506     D F01             C                   CONST(X'31')
010000940506     D F02             C                   CONST(X'32')
010100940506     D F03             C                   CONST(X'33')
010200940506     D F04             C                   CONST(X'34')
010300940506     D F05             C                   CONST(X'35')
010400940506     D F06             C                   CONST(X'36')
010500940506     D F07             C                   CONST(X'37')
010600940506     D F08             C                   CONST(X'38')
010700940506     D F09             C                   CONST(X'39')
010800940506     D F10             C                   CONST(X'3A')
010900940506     D F11             C                   CONST(X'3B')
011000940506     D F12             C                   CONST(X'3C')
011100940506     D F13             C                   CONST(X'B1')
011200940506     D F14             C                   CONST(X'B2')
011300940506     D F15             C                   CONST(X'B3')
011400940506     D F16             C                   CONST(X'B4')
011500940506     D F17             C                   CONST(X'B5')
011600940506     D F18             C                   CONST(X'B6')
011700940506     D F19             C                   CONST(X'B7')
011800940506     D F20             C                   CONST(X'B8')
011900940506     D F21             C                   CONST(X'B9')
012000940506     D F22             C                   CONST(X'BA')
012100940506     D F23             C                   CONST(X'BB')
012200940506     D F24             C                   CONST(X'BC')
012300940506     D ENTER           C                   CONST(X'F1')
012400940506     D ROLDWN          C                   CONST(X'F4')
012500940506     D ROLLUP          C                   CONST(X'F5')
012600940127     C*----------------------------------------------------*
012700940127     C*                MAIN LINE PROGRAM
012800940127     C*----------------------------------------------------*
012900940223     C* inizializzazione variabili
013000940223     C                   EXSR      INZVAR
013100940223     C*
013200940223     C     $FINE         DOWEQ     *OFF
013300090225     C     $GEST         CASEQ     'S0'          GESS0
013400940131     C     $GEST         CASEQ     'S1'          GESS1
013500940117     C                   END
013600940117     C                   END
013700940325     C* fine programma
013800940325     C                   SETON                                            LR
013900030113     C************************************************************
014000030113     C* INIZIALIZZAZIONE VARIABILI
014100030113     C************************************************************
014200030113     C     INZVAR        BEGSR
014300030113     C*
014400030113     C* Pulizia campi e indicatori
014500030113     C                   MOVE      *ALL'0'       IN4049           10
014600030113     C                   MOVEA     IN4049        *IN(40)
014700030113     C                   CLEAR                   S1OPZ
014800090225     C                   CLEAR                   S0OPZ
014900030113     C*
015000090225     C* Variabili per gestione videate
015100030113     C                   MOVE      *OFF          $FINE
015200030113     C                   MOVE      *OFF          $EFILE
015300030113     C                   MOVE      *OFF          $ESCI
015400030113     C                   MOVE      *OFF          $RCDOK
015500030113     C                   Z-ADD     0             $ULKS1            3 0
015600030113     C*
015700100319     C                   MOVE      *ON           $INZS2
015800030113     C                   MOVE      *ON           $INZS1
015900090225     C                   MOVE      *ON           $INZS0
016000030113     C* Variabili appoggio
016100030114     C                   Z-ADD     1             WPAG
016200030113     c*
016300100407     c* Video da cui partire
016400100622     c                   movel(p)  'QEM'         tbe_QEM           3
016500100407     C                   MOVE      'S0'          $GEST
016600100407     c*
016700030113     C                   ENDSR
016800090225     C************************************************************
016900090225     C* GESTIONE LISTA
017000090225     C************************************************************
017100090225     C     GESS0         BEGSR
017200090225     C*
017300090225     C* inizializzazione videata
017400090225     C     $INZS0        IFEQ      *ON
017500090225     C                   EXSR      INZS0
017600090225     C                   MOVE      *OFF          $INZS0
017700090225     C                   ENDIF
017800100325     C*
017900090225     C* emissione piede videata
018000090225     C                   WRITE     Z0
018100090225     C* Non ci sono records
018200090225     C     WMAX          IFEQ      0
018300090225     C                   WRITE     D0
018400090225     C                   Else
018500090225     C     Wsfl          IFgt      0
018600090225     C                   Z-ADD     wsfl          C0RCD
018700090225     C                   Else
018800090225     C     Wpag          IFgt      0
018900090225     C                   Z-ADD     wpag          C0RCD
019000090225     C                   EndIF
019100090225     C                   EndIF
019200090225     C                   ENDIF
019300090225     C*
019400090225     C*              *------------------*
019500090225     C                   EXFMT     C0
019600090225     C*              *------------------*
019700090225     C*
019800090225     C     C0NRR         IFNE      0
019900090225     C                   Z-ADD     C0NRR         WSFL
020000090225     C                   ENDIF
020100090225     C                   Z-ADD     SFLNRR        C0RCD
020200090225     C* Selezioni
0203000902251    C                   SELECT
020400090225     C* F3=Fine
020500090225     C     $TASTO        WHENEQ    F03
020600090225     C                   EXSR      F03S1
020700100121     C* F10=Immissione
020800100121     C     $TASTO        WHENEQ    F10
020900100121     C                   EXSR      F10S1
021000100325     C                   MOVE      'S0'          $GEST
0211000902251O   C                   OTHER
021200090225     C* CONTROLLO DATI
021300090225     C                   EXSR      CTRC0
021400090225     C     *IN99         IFEQ      *OFF
021500090225     C                   EXSR      CTRS0
021600090225     C                   END
0217000902251-   C                   ENDSL
021800090225     C*
021900090225     C                   ENDSR
022000090225     C/EJECT
022100090225     C************************************************************
022200090225     C* INIZIALIZZAZIONE LISTA
022300090225     C************************************************************
022400090225     C     INZS0         BEGSR
022500100407     C*
022600090225     C* pulizia SFL
022700090225     C                   SETOFF                                         3031
022800090225     C                   WRITE     C0
022900090225     C                   SETON                                          31
023000090225     C*
023100090225     C* CARICAMENTO SFL totale
023200090225     C                   Z-ADD     0             S0NRR
023300090225     C                   Z-ADD     1             C0RCD
023400090225     C                   Z-ADD     0             WMAX
023500090225     C                   Z-ADD     0             wsfl
023600090225     C*
023700090225     C*  Posizionamento all'inizio
023800100617     c                   movel(p)  c0pos         tbe_COD          15
023900100326     C                   IF        Posiziona = 'S'
024000100622     c     tbe_cod_QEM   setll     tnTBE01L
024100100326     C                   eval      Posiziona = *blank
024200100326     c                   else
024300100622     c     tbe_QEM       setll     tnTBE01L
024400100326     c                   end
024500100119    >C                   EXSR      READ_TBE
024600090225     C* Carico il SFL
024700090225     C                   EXSR      ROLS0
024800090225     C*
024900090225     c                   if        xtaopr <> '1'
025000090225     C                   Z-ADD     1             WPAG
025100090225     c                   end
025200090225     C*
025300090225     C                   ENDSR
025400090225     C************************************************************
025500090225     C* CARICAMENTO PAGINA LISTA
025600090225     C************************************************************
025700090225     C     ROLS0         BEGSR
025800090225     C*
025900090225     C                   SETOFF                                       32
026000090225     C                   Z-ADD     0             Y
026100090225     C                   Z-ADD     WMAX          S0NRR
026200090225     C*
026300100119     C*  Selezionarle Tutte
026400100617     c                   eval      s0cod =  '999'
026500100617     c                   eval      s0des =  'T U T T I'
026600100119     C                   ADD       1             S0NRR
026700100119     C                   ADD       1             Y
026800100119     C                   WRITE     S0
026900100119     C*
027000090225     C* Leggo dal file i segmenti presenti nell'archivio
0271000902251    C     $EFILE        DOWEQ     *OFF
027200100617     C*
027300100617     c                   movel     tbeKe1        cod_FIL           3
027400100617     C*
027500100617     c                   if        cod_FIL <> sav_cod_FIL
027600100617     c*
027700100617     c                   clear                   s0opz
027800100617     c* Prendo tipo segmento generale
027900100617     c                   eval      s0cod =  cod_FIL
028000100617     c                   movel     cod_FIL       num_FIL           3 0
028100100617     c     num_FIL       chain     azorg01l
028200100617     c                   if        %Found(azorg01l)
028300100617     c                   eval      s0des =  orgDES
028400100617     c                   end
028500090225     C*
028600090225     C                   ADD       1             S0NRR
028700090225     C                   ADD       1             Y
028800090225     C                   WRITE     S0
028900100119     C*
029000100617     c                   eval       sav_cod_FIL = cod_FIL
029100100119     c                   end
029200100119     C*
029300100119    >C                   EXSR      READ_TBE
029400090225     C*
0295000902251-   C                   ENDDO
029600090225     C*
029700090225     C                   Z-ADD     S0NRR         WMAX                 30
029800090225     C*
029900090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
030000090225     C*
030100090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
030200090225     C                   MVR                     RESTO             3 0
030300090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0304000902251    C     RESTO         IFGT      0
030500090225     C                   ADD       1             C0RCD
0306000902251E   C                   ELSE
030700090225     C                   SUB       SFLPAG0       C0RCD
030800090225     C                   ADD       1             C0RCD
0309000902251-   C                   ENDIF
031000090225     C*
031100090225     C                   ENDSR
031200090225     C************************************************************
031300090225     C* LETTURA RCD
031400090225     C************************************************************
031500100119     C     READ_TBE      BEGSR
031600090225     C*
031700090225     C                   MOVEL     *OFF          $EFILE
031800090225     C                   MOVEL     *OFF          $RCDOK
031900090225     C*
0320000902251    C     $EFILE        DOUEQ     *ON
032100090225     C     $RCDOK        OREQ      *ON
032200090225     C*
032300100622     c     tbe_QEM       Reade     tntbe01L
032400100119     c                   if        %eof(tntbe01L)
032500090225     C                   MOVEL     *ON           $EFILE
032600090225     C                   MOVE      $EFILE        *IN33
032700090225     c                   else
032800100322      **
032900100322     c                   eXSr      Selez_RECord
033000100119     c                   end
033100090225     C*
0332000902251-   C                   ENDDO
033300090225     C*
033400090225     C                   ENDSR
033500090225     C************************************************************
033600090225     C* CONTROLLO TESTATA LISTA
033700090225     C************************************************************
033800100322     C     Selez_RECord  BEGSR
033900100407      *
034000100322     C*  imposta i flags appena letti nella DS
034100100622     c                   movel     tbeUNI        tbeUNI_dQEM
034200100618     C                   MOVEL     *ON           $RCDOK
034300100322     C*
034400100322     C     End_filtro    ENDSR
034500100322     C************************************************************
034600100322     C* CONTROLLO TESTATA LISTA
034700100322     C************************************************************
034800100322     C     CTRC0         BEGSR
034900100322     C*
035000100326     C                   MOVE      *OFF          *IN99
035100100326     C*
035200100326     C                   clear                   posiziona         1
035300100326     c                   if        c0pos <> SAVc0pos
035400100326     C                   move      'S'           posiziona         1
035500100326     c                   eval      SAVc0pos = c0pos
035600100326     c                   move      *ON           $INZS0
035700100326     c                   end
035800090225     C*
035900090225     C                   ENDSR
036000090225     C************************************************************
036100090225     C* CONTROLLO OPZIONI LISTA
036200090225     C************************************************************
036300090225     C     CTRS0         BEGSR
036400090225     C*
036500090225     C                   MOVEL     *OFF          $ESCI
036600090225     C                   SETOFF                                       99
036700090225     C                   clear                   S0OPZ
036800090225     c                   clear                   x0cod
036900090225     C*
037000090225     C* Leggo il sfl solo se ci sono rcd
0371000902251    C     WMAX          IFGT      0
037200090225     C                   READC     S0                                     21
037300090225     C*
037400090225     C* esce se fine sfl o errore che richiede l'uscita
0375000902252    C     *IN21         DOWEQ     *OFF
037600090225     C     $ESCI         ANDEQ     *OFF
037700090225     C                   Z-ADD     S0NRR         C0RCD
037800090225     C* ctrl su riga
037900090225     C                   EXSR      RECS0
038000090225     C* gestione opzioni
0381000902253    C     S0OPZ         IFNE      *blank
038200090225     C     *IN99         ANDEQ     *OFF
038300090225     C                   EXSR      OPZS0
0384000902253-   C                   ENDIF
038500090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0386000902253    C     *IN99         IFEQ      *ON
038700090225     C     $ESCI         OREQ      *ON
038800090225     C                   MOVE      *ON           *IN32
038900090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
039000090225     C* la ripristinerò a conclusione del ciclo di READC
039100090225     C                   MOVE      *OFF          $INZS0
0392000902253-   C                   ENDIF
039300090225     C*
039400090225     C                   clear                   S0OPZ
039500090225     C*
039600090225     C                   UPDATE    S0
039700090225     C*
039800090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0399000902253    C     $ESCI         IFEQ      *OFF
040000090225     C                   READC     S0                                     21
040100090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0402000902254    C     *IN21         IFEQ      *ON
040300090225     C                   MOVE      WINZS0        $INZS0
040400090225     C* calcolo pagina a cui deve posizionarsi
040500090225     C                   EXSR      CLCPAG0
0406000902254-   C                   ENDIF
0407000902253-   C                   ENDIF
040800090225     C*
0409000902252-   C                   ENDDO
041000090225     C*
0411000902251-   C                   ENDIF
041200090225     C*
041300090225     C                   ENDSR
041400090225     C/EJECT
041500090225     C************************************************************
041600090225     C* CONTROLLO CAMPI I/O RIGA LISTA
041700090225     C************************************************************
041800090225     C     RECS0         BEGSR
041900090225     C*
042000090225     C* reset indicatori DSPATR
042100090225     C                   MOVE      *ALL'0'       IN4049           10
042200090225     C                   MOVEA     IN4049        *IN(40)
042300090225     C*
042400090225     C                   ENDSR
042500090225     C************************************************************
042600090225     C* GESTIONE OPZIONI LISTA
042700090225     C************************************************************
042800090225     C     OPZS0         BEGSR
042900090225     C*
043000090225      * selezionato un codice da restituire al chiamante
0431000902253    C     S0OPZ         IFeq      '1'
043200100618     c                   move      s0cod         codiceFIL         3
043300090225     C                   MOVE      'S1'          $GEST
043400090225     C                   MOVEL     *ON           $ESCI
043500090225     C                   END
043600090225     C*
043700090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0438000902252    C     *IN99         IFEQ      *ON
043900090225     C                   SETON                                        40
0440000902252-   C                   ENDIF
044100090225     C*
044200090225     C                   ENDSR
044300090225     C/EJECT
044400940127     C************************************************************
044500940131     C* GESTIONE LISTA
044600940127     C************************************************************
044700940127     C     GESS1         BEGSR
044800030113     C*
044900940223     C* inizializzazione videata
045000940223     C     $INZS1        IFEQ      *ON
045100940127     C                   EXSR      INZS1
045200940223     C                   MOVE      *OFF          $INZS1
045300940127     C                   ENDIF
045400030113     C*
045500030113     C* emissione piede videata
045600030113     C                   WRITE     Z1
045700030113     C* Non ci sono records
045800940223     C     WMAX          IFEQ      0
045900940607     C                   WRITE     D1
046000030114     C                   Else
046100030114     C     Wsfl          IFgt      0
046200030114     C                   Z-ADD     wsfl          C1RCD
046300030114     C                   Else
046400030114     C     Wpag          IFgt      0
046500030114     C                   Z-ADD     wpag          C1RCD
046600030114     C                   EndIF
046700030114     C                   EndIF
046800030114     C                   ENDIF
046900940127     C*
047000030113     C*              *------------------*
047100940607     C                   EXFMT     C1
047200030113     C*              *------------------*
047300030113     C*
047400940204     C     C1NRR         IFNE      0
047500940204     C                   Z-ADD     C1NRR         WSFL
047600940204     C                   ENDIF
047700940127     C                   Z-ADD     SFLNRR        C1RCD
047800030113     C* Selezioni
0479009401271    C                   SELECT
048000940127     C* F3=Fine
048100940506     C     $TASTO        WHENEQ    F03
048200940309     C                   EXSR      F03S1
048300940131     C* F10=Immissione
048400940506     C     $TASTO        WHENEQ    F10
048500940309     C                   EXSR      F10S1
048600100325     C                   MOVE      'S1'          $GEST
048700090225     C* F12=Ritorno
048800090225     C     $TASTO        WHENEQ    F12
048900090225     C                   eval      $Gest='S0'
049000090225     C                   MOVE      *ON           $INZS1
049100090225     C                   MOVE      *ON           $INZS0
049200100407      *
049300100326     C* se c'era un posizionamento
049400100326     c                   if        c0POS <> *blank
049500100326     c                   eval      SAVc0pos = c0pos
049600100326     c                   eval      posiziona = 'S'
049700100326     c                   end
049800100407      *
0499009401271O   C                   OTHER
050000940127     C* CONTROLLO DATI
050100940131     C                   EXSR      CTRC1
050200940201     C     *IN99         IFEQ      *OFF
050300940131     C                   EXSR      CTRS1
050400940131     C                   END
0505009401271-   C                   ENDSL
050600940127     C*
050700940127     C                   ENDSR
050800940224     C/EJECT
050900940127     C************************************************************
051000940131     C* INIZIALIZZAZIONE LISTA
051100940127     C************************************************************
051200940127     C     INZS1         BEGSR
051300100322     C*
051400940302     C* pulizia SFL
051500100121     C                   SETOFF                                       423031
051600940607     C                   WRITE     C1
051700940128     C                   SETON                                          31
051800940128     C*
051900030113     C* CARICAMENTO SFL totale
052000940201     C                   Z-ADD     0             S1NRR
052100030113     C                   Z-ADD     1             C1RCD
052200940128     C                   Z-ADD     0             WMAX
052300090225     C                   Z-ADD     0             wsfl
052400100618     c                   if        codiceFIL = *blank
052500100618     c                   eval      codiceFIL = '999'
052600100121     c                   end
052700100618     C                   move      codiceFIL     c1FIL             3
052800100618     c                   movel(p)  codiceFIL     tbe_COD
052900940224     C*
053000940224     C* Posizionamento su file pilota
053100100618     c                   if        codiceFIL = '999'
053200100622     c     tbe_QEM       setll     tntbe01L
053300100618     c                   else
053400100622     c     tbe_cod_QEM   setll     tntbe01L
053500100618     c                   end
053600100618     C*
053700100119    >C                   EXSR      READ_tbe
053800030113     C* Carico il SFL
053900940127     C                   EXSR      ROLS1
054000030113     C*
054100030114     c                   if        xtaopr <> '1'
054200030114     C                   Z-ADD     1             WPAG
054300030114     c                   end
054400940127     C*
054500940127     C                   ENDSR
054600940127     C************************************************************
054700940131     C* CARICAMENTO PAGINA LISTA
054800940127     C************************************************************
054900940127     C     ROLS1         BEGSR
055000940127     C*
055100940128     C                   SETOFF                                       32
055200940223     C                   Z-ADD     0             Y
055300940127     C                   Z-ADD     WMAX          S1NRR
055400940127     C*
055500940127     C* Leggo dal file anagrafico per caricare la lista
0556009401311    C     $EFILE        DOWEQ     *OFF
055700100618     c                   movel     tbeKe1        cod_FIL           3
055800100119     C*
055900100618     c                   if        cod_FIL = c1FIL or c1FIL = '999'
056000100317     c*
056100030113     c                   clear                   s1opz
056200100324     c                   eval      h1cod = tbeKE1
056300100324     c                   movel(p)  tbeke1        s1cod
056400100617     c                   clear                   s1DES
056500100617     c                   clear                   s1TRS
056600100617     c                   clear                   s1DET
056700100617     c                   clear                   s1ECC
056800100617      *
056900100617      * Decodifico partner
057000100617     C                   Z-ADD     1             KKUT
057100100617     C                   Z-ADD     KCI           KKCC
057200100617     C                   MOVE      *ZEROS        KKSC
057300100617     C                   MOVE      S1COD         KKSC
057400100618     C     KACO          CHAIN     CNACO00F                           21
057500100617     c                   if        %Found(CNACO00F)
057600100617     c                   eval      s1des =  ACORAG
057700100119     c                   end
057800100622     c                   eval      dQEM   = tbeUNI
057900100622     c                   MOVEL     §QEMDTB       s1TRS
058000100622     c                   MOVEL     §QEMSGD       s1DET
058100100622     c                   MOVEL     §QEMECC       s1ECC
058200940127     C*
058300100323     c                   eval      h1ANNulla = *off
058400100121     c                   if        tbeATB <> *blank
058500100323     c                   move(p)   'ANNULLATO'   s1des
058600100323     c                   eval      h1ANNulla = *on
058700100121     c                   seton                                        42
058800100121     c                   end
058900100121     C*
059000940127     C                   ADD       1             S1NRR
059100940127     C                   ADD       1             Y
059200940607     C                   WRITE     S1
059300100121     c                   setoff                                       42
059400100119     c                   end
059500940131     C*
059600100119    >C                   EXSR      READ_tbe
059700100618     c                   if        codiceFIL <>'999' and
059800100618     c                             codiceFIL <> %subst(tbeKe1:1:3)
0599001006181    C                   eval      $EFILE = *ON
060000100618     c                   end
0601009401271-   C                   ENDDO
060200940127     C*
060300940223     C                   Z-ADD     S1NRR         WMAX                 30
060400940127     C*
060500940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
060600090225     C     S1NRR         DIV       SFLPAG1       PAGINE            4 0
060700940127     C                   MVR                     RESTO             3 0
060800090225     C     PAGINE        MULT      SFLPAG1       C1RCD
0609000301141    C     RESTO         IFGT      0
061000030114     C                   ADD       1             C1RCD
0611000301141E   C                   ELSE
061200090225     C                   SUB       SFLPAG1       C1RCD
061300030114     C                   ADD       1             C1RCD
0614000301141-   C                   ENDIF
061500940128     C*
061600940127     C                   ENDSR
061700940224     C************************************************************
061800940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
061900940224     C************************************************************
062000090225     C     CLCPAG0       BEGSR
062100940224     C* Input :
062200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
062300940224     C*          cursore
062400940224     C* - SFLPAG = numero rcd per pagina sfl
062500940224     C* Output :
062600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
062700940224     C*
062800090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
062900940224     C                   MVR                     RESTO             3 0
063000940224     C     RESTO         IFGT      0
063100940224     C                   ADD       1             PAGINE
063200940224     C                   ENDIF
063300090226     c                   if        pagine > 1
063400090225     C     PAGINE        MULT      SFLPAG0       WPAG
063500090226     C                   END
063600940224     C*
063700940224     C                   ENDSR
063800090225     C************************************************************
063900090225     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
064000090225     C************************************************************
064100090225     C     CLCPAG1       BEGSR
064200090225     C* Input :
064300090225     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
064400090225     C*          cursore
064500090225     C* - SFLPAG = numero rcd per pagina sfl
064600090225     C* Output :
064700090225     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
064800090225     C*
064900090225     C     WSFL          DIV       SFLPAG1       PAGINE            4 0
065000090225     C                   MVR                     RESTO             3 0
065100090225     C     RESTO         IFGT      0
065200090225     C                   ADD       1             PAGINE
065300090225     C                   ENDIF
065400090226     c                   if        pagine > 1
065500090225     C     PAGINE        MULT      SFLPAG1       WPAG
065600090226     C                   ENDIF
065700090225     C*
065800090225     C                   ENDSR
065900940309     C************************************************************
066000940309     C* GESTIONE F03 VIDEO S1
066100940309     C************************************************************
066200940309     C     F03S1         BEGSR
066300940309     C*
066400940309     C                   MOVE      *ON           $FINE
066500940325     C* fine programma
066600940309     C                   ENDSR
066700940309     C/EJECT
066800940309     C************************************************************
066900940309     C* GESTIONE F10 VIDEO S1
067000940314     c* AGGIUNTA RECORD
067100940309     C************************************************************
067200940309     C     F10S1         BEGSR
067300940309     C*
067400030113     C                   RESET                   tabds
067500030113     C                   MOVEL     '01'          xtaopz
067600030113     C                   MOVE      *ZERO         xtaret
067700030113     C                   MOVE      *ZERO         xtaopr
067800100409     C                   MOVEl     *blank        xtakey1
067900100409     C                   MOVEl     *blank        xtakey2
068000100622     C                   MOVEl     'QEM'         xtacod
068100030113     C                   MOVE      *BLANKS       KPJBU
068200030113     C                   MOVEL     tabds         KPJBU
068300100622$004 C                   CALL      'TNTBQEMR2'
068400030113     C                   PARM                    KPJBA
068500090225      *
068600030114     C                   MOVEL     KPJBU         tabds
068700030113      *
068800940309     C* ritorno da PGM gestione
068900940309     C                   EXSR      GESRET
069000090226      *
069100940309     C     WINZS1        IFEQ      *ON
069200940309     C                   MOVE      *ON           $INZS1
069300090226     C* carico sempre la 1a pagina
069400090226     C                   Z-ADD     1             WPAG
069500940309     C                   ENDIF
069600940309     C*
069700940309     C                   ENDSR
069800100322     C************************************************************
069900100322     C* CONTROLLO TESTATA LISTA
070000100322     C************************************************************
070100100322     C     CTRC1         BEGSR
070200100322     C*
070300940201     C                   MOVE      *OFF          *IN99
070400940131     C*
070500940202     C                   ENDSR
070600940131     C************************************************************
070700940131     C* CONTROLLO OPZIONI LISTA
070800940131     C************************************************************
070900940131     C     CTRS1         BEGSR
071000940131     C*
071100940202     C                   MOVEL     *OFF          $ESCI
071200940201     C                   SETOFF                                       99
071300940131     C                   Z-ADD     0             S1OPZ
071400040930     c                   clear                   x1cod
071500940131     C*
071600940127     C* Leggo il sfl solo se ci sono rcd
0717009401311    C     WMAX          IFGT      0
071800940607     C                   READC     S1                                     21
071900940127     C*
072000940131     C* esce se fine sfl o errore che richiede l'uscita
0721009401312    C     *IN21         DOWEQ     *OFF
072200940131     C     $ESCI         ANDEQ     *OFF
072300940201     C                   Z-ADD     S1NRR         C1RCD
072400940131     C* ctrl su riga
072500940131     C                   EXSR      RECS1
072600940131     C* gestione opzioni
0727009401313    C     S1OPZ         IFNE      0
072800940201     C     *IN99         ANDEQ     *OFF
072900940131     C                   EXSR      OPZS1
0730009401273-   C                   ENDIF
073100940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0732009402013    C     *IN99         IFEQ      *ON
073300940201     C     $ESCI         OREQ      *ON
073400940131     C                   MOVE      *ON           *IN32
073500940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
073600940204     C* la ripristinerò a conclusione del ciclo di READC
073700940223     C                   MOVE      *OFF          $INZS1
0738009402233-   C                   ENDIF
073900940223     C*
074000100323     C  n99              Z-ADD     0             S1OPZ
074100100121      ****
074200100323     c     h1annulla     comp      *on                                    42
074300940223     C*
074400940607     C                   UPDATE    S1
074500940223     C*
074600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0747009401313    C     $ESCI         IFEQ      *OFF
074800940607     C                   READC     S1                                     21
074900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0750009402014    C     *IN21         IFEQ      *ON
075100940201     C                   MOVE      WINZS1        $INZS1
075200940204     C* calcolo pagina a cui deve posizionarsi
075300090225     C                   EXSR      CLCPAG1
0754009402014-   C                   ENDIF
0755009402013-   C                   ENDIF
075600940131     C*
0757009401272-   C                   ENDDO
075800940127     C*
0759009401311-   C                   ENDIF
076000940131     C*
076100940127     C                   ENDSR
076200940127     C/EJECT
076300940127     C************************************************************
076400940131     C* CONTROLLO CAMPI I/O RIGA LISTA
076500940127     C************************************************************
076600940131     C     RECS1         BEGSR
076700940131     C*
076800940201     C* reset indicatori DSPATR
076900940201     C                   MOVE      *ALL'0'       IN4049           10
077000940201     C                   MOVEA     IN4049        *IN(40)
077100100323     C*
077200100323     C*  se si è selezionato di ripristinare un record NON annullato
0773001003233    C     S1OPZ         IFeq      8
0774001003233    C     H1ANNulla     andeq     *off
077500100323     c                   seton                                        41  99
077600100323     C                   END
077700100323     C*
077800100323     C*  se invece il record era annullato si può solo vedere o ripristinare
0779001003233    C     H1ANNulla     ifeq      *on
0780001003233    C     S1OPZ         andne     8
0781001003233    C     S1OPZ         andne     5
078200100323     c                   seton                                        41  99
078300100323     C                   END
078400100323     C*
078500100323     C*  se richiamato x selezionare un codice, --> 11 = *on
078600100323     C*   non deve utilizzare le opzioni di modifica e viceversa
078700040930     c     *in11         ifeq      *on
0788000409303    C     S1OPZ         IFeq      2
0789000409303    C     S1OPZ         OReq      3
0790000409303    C     S1OPZ         OReq      4
0791001003233    C     S1OPZ         OReq      8
079200040930     c                   seton                                        41  99
079300040930     C                   END
079400100323     C*
079500040930      * selezionato un codice da restituire al chiamante
0796000409303    C     S1OPZ         IFeq      1
079700100324     c                   move      h1cod         x1cod
079800040930     c                   clear                   kpjbu
079900040930     c                   movel     x1cod         kpjbu
080000040930     C                   MOVEL     *ON           $ESCI
080100040930     C                   MOVEL     *ON           $fine
080200040930     C                   END
080300040930     C                   END
080400040930     C*
080500040930     c     *in11         ifeq      *off
0806000409303    C     S1OPZ         ANDeq     1
080700040930     c                   seton                                        41  99
080800040930     C                   END
080900040930     C*
081000940131     C                   ENDSR
081100940131     C************************************************************
081200940131     C* GESTIONE OPZIONI LISTA
081300940131     C************************************************************
081400940131     C     OPZS1         BEGSR
081500940201     C*
0816000409302    C     *IN11         IFEQ      *Off
0817000409302    C     *IN11         oreq      *On
0818000409302    C     s1opz         andeq     05
081900040930     C*
082000030113     C                   RESET                   tabds
082100100323     c                   move      *zero         xtaopz
082200100323     C                   MOVE      S1OPZ         xtaopz
082300030113     C                   MOVE      *ZERO         xtaret
082400030113     C                   MOVE      *ZERO         xtaopr
082500100409     C                   MOVEl(p)  h1COD         xtakey1
082600100617     C                   MOVEl(p)  *blank        xtakey2
082700100622     C                   MOVEl     'QEM'         xtacod
082800940715     C                   MOVE      *BLANKS       KPJBU
082900030113     C                   MOVEL     tabds         KPJBU
083000100622$004 C                   CALL      'TNTBQEMR2'
083100940607     C                   PARM                    KPJBA
083200090225      *
083300030114     C                   MOVEL     KPJBU         tabds
083400940201     C*
083500940223     C* ritorno da PGM gestione
083600940223     C                   EXSR      GESRET
083700040930     C*
083800940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0839009402252    C     *IN99         IFEQ      *ON
084000940223     C                   SETON                                        40
0841009402252-   C                   ENDIF
084200940225     C*
0843000409302-   C                   ENDIF
084400040930     C*
084500940131     C                   ENDSR
084600940223     C/EJECT
084700940223     C************************************************************
084800940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
084900940223     C************************************************************
085000940223     C     GESRET        BEGSR
085100940223     C*
085200940223     C* modo di ritorno
085300940223     C*
0854009402231    C                   SELECT
085500940314    >C* << questi modi di utilizzo dei valori di ritorno dal
085600940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
085700940314    >C*    proposte, normalmente sempre valide, ma modificabili
085800940314    >C*    per situazioni particolari >>
085900940223     C* 1 = F3
086000030113    >C     xtaret        WHENEQ    '1'
086100940224     C                   MOVE      *ON           $ESCI
086200940223     C                   MOVE      *ON           $FINE
086300940223     C* 2 = F12
086400030113    >C     xtaret        WHENEQ    '2'
086500940223     C                   MOVE      *ON           $ESCI
086600940223     C*
0867009402231-   C                   ENDSL
086800940223     C*
086900940223     C* operazione eseguite dal pgm chiamato
087000940223     C*
0871009402231    C                   SELECT
087200940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
087300030113    >C     xtaopr        WHENEQ    '1'
087400940223     C                   MOVE      *ON           WINZS1
087500940223     C*
0876009402231-   C                   ENDSL
087700940223     C*
087800940223     C* funzione non eseguibile per errore :
087900940223     C*
0880009402231    C                   SELECT
088100940223     C* 1 = funzione richiamata chiusa in errore
088200940316    >C*  eventualmente gestire altri codici di errore
088300030113    >C     xtaerr        WHENEQ    '1'
088400940223     C                   MOVE      *ON           $ESCI
088500940223     C                   SETON                                        5299
088600940223     C*
0887009402231-   C                   ENDSL
088800940223     C*
088900940223     C                   ENDSR
089000940223     C/EJECT
089100940131     C************************************************************
089200940131     C* OPERAZIONI INIZIALI
089300940131     C************************************************************
089400940131     C     *INZSR        BEGSR
089500030113     C*
089600030113     C* Reperimento parametri
089700030113     C     *ENTRY        PLIST
089800030113     C                   PARM                    KPJBA
089900040930     C*
090000100622     c     tbe_cod_QEM   Klist
090100100622     c                   kfld                    tbe_QEM
090200100617     c                   kfld                    tbe_COD          15
090300100617      *
090400100617     C     KACO          KLIST
090500100617     C                   KFLD                    KKUT
090600100617     C                   KFLD                    KKCC
090700100617     C                   KFLD                    KKSC
090800100617      *
090900100617     C* RECUPERO DATI DELL'UTENTE
091000100617     C                   Z-ADD     1             CODUT             1 0
091100100617     C                   CALL      'XPARUT'
091200100617     C                   PARM                    UTEDSE0F
091300100617     C                   MOVEL     RAGUT         RSUT             20
091400100617     C*
091500100617     C* RICERCA CAPOCONTI
091600100617     C                   DO        50            X
091700100617     C                   MOVE      TCU(X)        TCUDS
091800100617     C     F56           IFEQ      'CG'
091900100617     C     F34           ANDEQ     '01'
092000100617     C                   Z-ADD     KCU(X)        KCI               4 0
092100100617     C                   END
092200100617     C                   END
092300100617     C*
092400100326     C*
092500040930     C* Se chiamato x selezionare un codice
092600040930     c                   clear                   x1cod
092700030113     C*
092800030113     C* Variabili per gestione videate
092900030113     C                   MOVE      *BLANK        $GEST             2
093000030113     C                   MOVE      *BLANK        $FINE             1
093100100319     C                   MOVE      *BLANK        $INZS2            1
093200030113     C                   MOVE      *BLANK        $INZS1            1
093300090225     C                   MOVE      *BLANK        $INZS0            1
093400030113     C                   MOVE      *BLANK        $EFILE            1
093500030113     C                   MOVE      *BLANK        $ESCI             1
093600030113     C                   MOVE      *BLANK        $RCDOK            1
093700030113     C* Indici
093800030113     C                   Z-ADD     0             X                 3 0
093900030113     C                   Z-ADD     0             Y                 3 0
094000940506     C*
094100940506     C* Reperimento tasti di funzione
094200100326     C*
094300100326     C                   clear                   c0pos
094400100326     C                   clear                   SAVc0pos
094500940127     C*
094600940117     C                   ENDSR
094700100319     C************************************************************
