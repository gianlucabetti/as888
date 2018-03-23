000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200131015      *-----------------------------------------------------------------------
000300131015      *   IL PRIMO PGM deve solo permettere di SCEGLIERE  con (1) o chiedere
000400131015      *    di inserire F10
000500131015      *-----------------------------------------------------------------------
000600131015      *
000700940307      *  21           GENERICO OPERAZIONI I/O
000800940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000900940224      *  30           SFLDSP
001000940224      * N31           SFLCLR
001100940224      *  31           SFLDSPCTL
001200940224      *  32           SFLNXTCHG
001300940224      *  33           SFLEND
001400940224      *  39           OF PRTF
001500940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001600940608      *  Specificare l'uso dei singoli indicatori
001700940224      *  50 <---> 98  ERRORI SU VIDEO
001800940608      *  Specificare l'uso dei singoli indicatori
001900940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002000940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002100940224      *  99           INDIC. GENERALE DI ERRORE
002200940128     F*----------------------------------------------------*
002300100802     Fedmap01l  IF   E           K DISK
002400100802     FTRTctb0D  CF   E             WORKSTN
002500940607     F                                     SFILE(S1:S1NRR)
002600940201     F                                     INFDS(DSFMT)
002700940128     D*----------------------------------------------------*
002800940211     D* Passaggio Parametri
002900940211     D KPJBA         E DS
003000030113      *-------------
003100940325     D* Parametri in ricezione
003200030113     D  TABDS          DS
003300030113     D  XTAOPZ                 1      2
003400030113     D  XTARET                 3      3
003500030113     D  XTAOPR                 4      4
003600030113     D  XTAERR                 5      5
003700100805     D  XTAKEY                 6     20
003800100805$003 D Kcodice                             Like(MAPcodice)
003900100805$003 D Kunbcli                             Like(MAPUNBCLI)
004000100803$003 D Ktmsg                               Like(MAPTIPOMSG)
004100100803$003 D Kvers                               Like(MAPVERSION)
004200100803$003 D Krels                               Like(MAPRELEASE)
004300100803$003 D Kagenz                              Like(MAPAGENCY)
004400100803$003 D Kassoc                              Like(MAPASSOCIA)
004500100804$003 D KObblig                             Like(MAPOBBLIG)
004600100804$003 D KDescr                              Like(MAPDESCR)
004700100804$003 D KDesci                              Like(MAPDESCI)
004800100805$003 D OLDCodice                           Like(MAPcodice)
004900100805$003 D OLDunbcli                           Like(MAPUNBCLI)
005000100805$003 D OLDtmsg                             Like(MAPTIPOMSG)
005100100805$003 D OLDvers                             Like(MAPVERSION)
005200100805$003 D OLDrels                             Like(MAPRELEASE)
005300100805$003 D OLDagenz                            Like(MAPAGENCY)
005400100805$003 D OLDassoc                            Like(MAPASSOCIA)
005500940211     D*-------------
005600940211     D DSFMT           DS           512
005700940506     D  $TASTO               369    369
005800940211     D  NRG                  370    370
005900940211     D  NCL                  371    371
006000940211     D  SFLNRR               378    379B 0
006100940207     D*-------------
006200940207     D* Nome PGM a video
006300940207     D                 DS
006400940207     D  PROGR                  1     10
006500940207     D  ASTER1                 1      1    INZ('*')
006600940207     D  SIGLA                  2      9
006700940207     D  ASTER2                10     10    INZ('*')
006800940127     D*-------------
006900940127     D* Reperimento nome PGM
007000940127     D STATUS         SDS           333
007100940127     D  DSPGM            *PROC
007200030113     D*-------------
007300030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
007400090225$003 D S1NRR           S                   Like(C1rcd)
007500030113$003 D WSfl            S                   Like(C1nrr)
007600030113$003 D Wmax            S                   Like(C1rcd)
007700030113$003 D Wpag            S                   Like(C1rcd)
007800090225$003 D Winzs1          S                   Like($inzs1)
007900040930$003 D x1cod           S                   Like(s1cod)
008000090225     D*-------------
008100100802$003 D segmento        S                   Like(mapCODICE)
008200940207     D*-------------
008300090225     D SFLPAG1         C                   CONST(06)
008400100802     D*-------------
008500940314     D* dimensione della schiera $MS1
008600940506     D*
008700940506     D* Tasti di funzione
008800940506     D F01             C                   CONST(X'31')
008900940506     D F02             C                   CONST(X'32')
009000940506     D F03             C                   CONST(X'33')
009100940506     D F04             C                   CONST(X'34')
009200940506     D F05             C                   CONST(X'35')
009300940506     D F06             C                   CONST(X'36')
009400940506     D F07             C                   CONST(X'37')
009500940506     D F08             C                   CONST(X'38')
009600940506     D F09             C                   CONST(X'39')
009700940506     D F10             C                   CONST(X'3A')
009800940506     D F11             C                   CONST(X'3B')
009900940506     D F12             C                   CONST(X'3C')
010000940506     D F13             C                   CONST(X'B1')
010100940506     D F14             C                   CONST(X'B2')
010200940506     D F15             C                   CONST(X'B3')
010300940506     D F16             C                   CONST(X'B4')
010400940506     D F17             C                   CONST(X'B5')
010500940506     D F18             C                   CONST(X'B6')
010600940506     D F19             C                   CONST(X'B7')
010700940506     D F20             C                   CONST(X'B8')
010800940506     D F21             C                   CONST(X'B9')
010900940506     D F22             C                   CONST(X'BA')
011000940506     D F23             C                   CONST(X'BB')
011100940506     D F24             C                   CONST(X'BC')
011200940506     D ENTER           C                   CONST(X'F1')
011300940506     D ROLDWN          C                   CONST(X'F4')
011400940506     D ROLLUP          C                   CONST(X'F5')
011500940127     C*----------------------------------------------------*
011600940127     C*                MAIN LINE PROGRAM
011700940127     C*----------------------------------------------------*
011800940223     C* inizializzazione variabili
011900940223     C                   EXSR      INZVAR
012000940223     C*
012100940223     C     $FINE         DOWEQ     *OFF
012200940131     C     $GEST         CASEQ     'S1'          GESS1
012300940117     C                   END
012400940117     C                   END
012500940325     C* fine programma
012600940325     C                   SETON                                            LR
012700030113     C************************************************************
012800030113     C* INIZIALIZZAZIONE VARIABILI
012900030113     C************************************************************
013000030113     C     INZVAR        BEGSR
013100030113     C*
013200030113     C* Pulizia campi e indicatori
013300030113     C                   MOVE      *ALL'0'       IN4049           10
013400030113     C                   MOVEA     IN4049        *IN(40)
013500030113     C                   CLEAR                   S1OPZ
013600030113     C*
013700090225     C* Variabili per gestione videate
013800030113     C                   MOVE      *OFF          $FINE
013900030113     C                   MOVE      *OFF          $EFILE
014000030113     C                   MOVE      *OFF          $ESCI
014100030113     C                   MOVE      *OFF          $RCDOK
014200030113     C                   Z-ADD     0             $ULKS1            3 0
014300030113     C*
014400030113     C                   MOVE      *ON           $INZS1
014500100802     C                   MOVE      'S1'          $GEST
014600030113     C* Variabili appoggio
014700030114     C                   Z-ADD     1             WPAG
014800030113     c*
014900030113     C                   ENDSR
015000940127     C************************************************************
015100940131     C* GESTIONE LISTA
015200940127     C************************************************************
015300940127     C     GESS1         BEGSR
015400030113     C*
015500940223     C* inizializzazione videata
015600940223     C     $INZS1        IFEQ      *ON
015700940127     C                   EXSR      INZS1
015800940223     C                   MOVE      *OFF          $INZS1
015900940127     C                   ENDIF
016000030113     C*
016100030113     C* emissione piede videata
016200030113     C                   WRITE     Z1
016300030113     C* Non ci sono records
016400940223     C     WMAX          IFEQ      0
016500940607     C                   WRITE     D1
016600030114     C                   Else
016700030114     C     Wsfl          IFgt      0
016800030114     C                   Z-ADD     wsfl          C1RCD
016900030114     C                   Else
017000030114     C     Wpag          IFgt      0
017100030114     C                   Z-ADD     wpag          C1RCD
017200030114     C                   EndIF
017300030114     C                   EndIF
017400030114     C                   ENDIF
017500940127     C*
017600100809     C     WMAX          IFLT      C1RCD
017700100809     C                   Z-ADD     1             C1RCD
017800100809     C                   ENDIF
017900030113     C*              *------------------*
018000940607     C                   EXFMT     C1
018100030113     C*              *------------------*
018200030113     C*
018300940204     C     C1NRR         IFNE      0
018400940204     C                   Z-ADD     C1NRR         WSFL
018500940204     C                   ENDIF
018600940127     C                   Z-ADD     SFLNRR        C1RCD
018700030113     C* Selezioni
0188009401271    C                   SELECT
018900940127     C* F3=Fine
019000940506     C     $TASTO        WHENEQ    F03
019100940309     C                   EXSR      F03S1
019200940131     C* F10=Immissione
019300940506     C     $TASTO        WHENEQ    F10
019400940309     C                   EXSR      F10S1
019500090225     C* F12=Ritorno
019600090225     C     $TASTO        WHENEQ    F12
019700100802     C                   eval      $Gest='S1'
019800090225     C                   MOVE      *ON           $INZS1
0199009401271O   C                   OTHER
020000940127     C* CONTROLLO DATI
020100940131     C                   EXSR      CTRC1
020200940201     C     *IN99         IFEQ      *OFF
020300940131     C                   EXSR      CTRS1
020400940131     C                   END
0205009401271-   C                   ENDSL
020600940127     C*
020700940127     C                   ENDSR
020800940224     C/EJECT
020900940127     C************************************************************
021000940131     C* INIZIALIZZAZIONE LISTA
021100940127     C************************************************************
021200940127     C     INZS1         BEGSR
021300940302     C* pulizia SFL
021400940128     C                   SETOFF                                         3031
021500940607     C                   WRITE     C1
021600940128     C                   SETON                                          31
021700940128     C*
021800030113     C* CARICAMENTO SFL totale
021900940201     C                   Z-ADD     0             S1NRR
022000030113     C                   Z-ADD     1             C1RCD
022100940128     C                   Z-ADD     0             WMAX
022200090225     C                   Z-ADD     0             wsfl
022300940224     C*
022400940224     C* Posizionamento su file pilota
022500100802     c     *loval        setll     edmap01L
022600940608    >C                   EXSR      REDANA
022700030113     C* Carico il SFL
022800940127     C                   EXSR      ROLS1
022900030113     C*
023000030114     c                   if        xtaopr <> '1'
023100030114     C                   Z-ADD     1             WPAG
023200030114     c                   end
023300940127     C*
023400940127     C                   ENDSR
023500940127     C************************************************************
023600940131     C* CARICAMENTO PAGINA LISTA
023700940127     C************************************************************
023800940127     C     ROLS1         BEGSR
023900940127     C*
024000940128     C                   SETOFF                                       32
024100940223     C                   Z-ADD     0             Y
024200940127     C                   Z-ADD     WMAX          S1NRR
024300940127     C*
024400940127     C* Leggo dal file anagrafico per caricare la lista
0245009401311    C     $EFILE        DOWEQ     *OFF
024600940127     C*
024700030113     c                   clear                   s1opz
024800100802     c                   eval      s1cod = mapCODICE
024900100802     c                   eval      s1des = mapDESCR
025000100802     c                   eval      s1unb = mapUNBCLI
025100100802     c                   eval      s1TMSG= mapTIPOMSG
025200100802     c                   eval      s1VERS= mapVERSION
025300100802     c                   eval      s1REL = mapRELEASE
025400100802     c                   eval      s1AGEN= mapAGENCY
025500100802     c                   eval      s1ASSO= mapASSOCIA
025600100804     c                   eval      h1obbl= mapOBBLIG
025700100804     c                   eval      h1descr= mapDESCR
025800100804     c                   eval      h1desci= mapDESCI
025900100802     C*
026000940127     C                   ADD       1             S1NRR
026100940127     C                   ADD       1             Y
026200940127     C*
026300940607     C                   WRITE     S1
026400940131     C*
026500940316    >C                   EXSR      REDANA
0266009401271-   C                   ENDDO
026700940127     C*
026800940223     C                   Z-ADD     S1NRR         WMAX                 30
026900940127     C*
027000940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
027100940127     C*
027200090225     C     S1NRR         DIV       SFLPAG1       PAGINE            4 0
027300940127     C                   MVR                     RESTO             3 0
027400090225     C     PAGINE        MULT      SFLPAG1       C1RCD
0275000301141    C     RESTO         IFGT      0
027600030114     C                   ADD       1             C1RCD
0277000301141E   C                   ELSE
027800090225     C                   SUB       SFLPAG1       C1RCD
027900030114     C                   ADD       1             C1RCD
0280000301141-   C                   ENDIF
028100940128     C*
028200940127     C                   ENDSR
028300940128     C************************************************************
028400940131     C* LETTURA RCD ARCHIVIO PILOTA
028500940128     C************************************************************
028600940607     C     REDANA        BEGSR
028700940128     C*
028800940131     C                   MOVEL     *OFF          $EFILE
028900940131     C                   MOVEL     *OFF          $RCDOK
029000940131     C*
0291009401311    C     $EFILE        DOUEQ     *ON
029200940131     C     $RCDOK        OREQ      *ON
029300940131     C*
029400100802     c                   Read      edMAP01L
029500100802     c                   if        %eof(edmap01L)
029600030113     C                   MOVEL     *ON           $EFILE
029700030113     C                   MOVE      $EFILE        *IN33
029800030113     c                   else
029900100802     c                   if          MAPSEQUEN = 0  and
030000100802     c                               MAPPROGR  = 0  and
030100100802     c                               MAPTIPOD  = *blank  and
030200100802     c                               MAPCATEGO = *blank  and
030300100802     c                               MAPNCAMPO = *blank
030400030113     C                   MOVE      *ON           $RCDOK
030500030113     c                   end
030600100802     c                   end
030700940131     C*
0308009401311-   C                   ENDDO
030900940131     C*
031000940131     C                   ENDSR
031100090225     C************************************************************
031200090225     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
031300090225     C************************************************************
031400090225     C     CLCPAG1       BEGSR
031500090225     C* Input :
031600090225     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
031700090225     C*          cursore
031800090225     C* - SFLPAG = numero rcd per pagina sfl
031900090225     C* Output :
032000090225     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
032100090225     C*
032200090225     C     WSFL          DIV       SFLPAG1       PAGINE            4 0
032300090225     C                   MVR                     RESTO             3 0
032400090225     C     RESTO         IFGT      0
032500090225     C                   ADD       1             PAGINE
032600090225     C                   ENDIF
032700090226     c                   if        pagine > 1
032800090225     C     PAGINE        MULT      SFLPAG1       WPAG
032900090226     C                   ENDIF
033000090225     C*
033100090225     C                   ENDSR
033200940309     C************************************************************
033300940309     C* GESTIONE F03 VIDEO S1
033400940309     C************************************************************
033500940309     C     F03S1         BEGSR
033600940309     C*
033700940309     C                   MOVE      *ON           $FINE
033800940325     C* fine programma
033900940309     C                   ENDSR
034000940309     C/EJECT
034100940309     C************************************************************
034200940309     C* GESTIONE F10 VIDEO S1
034300940314     c* AGGIUNTA RECORD
034400940309     C************************************************************
034500940309     C     F10S1         BEGSR
034600940309     C*
034700030113     C                   RESET                   tabds
034800131015     C                   MOVEL     '10'          xtaopz
034900030113     C                   MOVE      *ZERO         xtaret
035000030113     C                   MOVE      *ZERO         xtaopr
035100030114     C                   MOVEl     *blank        xtakey
035200100805     C                   clear                   Kcodice
035300100805     C                   clear                   Kunbcli
035400100803     C                   clear                   Ktmsg
035500100803     C                   clear                   Kvers
035600100803     C                   clear                   Krels
035700100803     C                   clear                   Kagenz
035800100803     C                   clear                   Kassoc
035900100805     C                   clear                   Kdescr
036000100805     C                   clear                   Kdesci
036100100805     C                   clear                   oldcodice
036200100805     C                   clear                   oldunbcli
036300100805     C                   clear                   oldtmsg
036400100805     C                   clear                   oldvers
036500100805     C                   clear                   oldrels
036600100805     C                   clear                   oldagenz
036700100805     C                   clear                   oldassoc
036800030113     C                   MOVE      *BLANKS       KPJBU
036900030113     C                   MOVEL     tabds         KPJBU
037000131015$004 C                   CALL      'TRTCTB0R0'                          99
037100030113     C                   PARM                    KPJBA
037200100804     c   99              exfmt     ALERTWIND
037300090225      *
037400030114     C                   MOVEL     KPJBU         tabds
037500030113      *
037600940309     C* ritorno da PGM gestione
037700940309     C                   EXSR      GESRET
037800090226     C                   MOVE      'S1'          $GEST
037900090226      *
038000940309     C     WINZS1        IFEQ      *ON
038100940309     C                   MOVE      *ON           $INZS1
038200090226     C* carico sempre la 1a pagina
038300090226     C                   Z-ADD     1             WPAG
038400940309     C                   ENDIF
038500940309     C*
038600940309     C                   ENDSR
038700940128     C************************************************************
038800940131     C* CONTROLLO TESTATA LISTA
038900940128     C************************************************************
039000940131     C     CTRC1         BEGSR
039100940128     C*
039200940201     C                   MOVE      *OFF          *IN99
039300940131     C*
039400940202     C                   ENDSR
039500940131     C************************************************************
039600940131     C* CONTROLLO OPZIONI LISTA
039700940131     C************************************************************
039800940131     C     CTRS1         BEGSR
039900940131     C*
040000940202     C                   MOVEL     *OFF          $ESCI
040100940201     C                   SETOFF                                       99
040200940131     C                   Z-ADD     0             S1OPZ
040300040930     c                   clear                   x1cod
040400940131     C*
040500940127     C* Leggo il sfl solo se ci sono rcd
0406009401311    C     WMAX          IFGT      0
040700940607     C                   READC     S1                                     21
040800940127     C*
040900940131     C* esce se fine sfl o errore che richiede l'uscita
0410009401312    C     *IN21         DOWEQ     *OFF
041100940131     C     $ESCI         ANDEQ     *OFF
041200940201     C                   Z-ADD     S1NRR         C1RCD
041300940131     C* ctrl su riga
041400940131     C                   EXSR      RECS1
041500940131     C* gestione opzioni
0416009401313    C     S1OPZ         IFNE      0
041700940201     C     *IN99         ANDEQ     *OFF
041800940131     C                   EXSR      OPZS1
0419009401273-   C                   ENDIF
042000940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0421009402013    C     *IN99         IFEQ      *ON
042200940201     C     $ESCI         OREQ      *ON
042300940131     C                   MOVE      *ON           *IN32
042400940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
042500940204     C* la ripristinerò a conclusione del ciclo di READC
042600940223     C                   MOVE      *OFF          $INZS1
0427009402233-   C                   ENDIF
042800940223     C*
042900940131     C                   Z-ADD     0             S1OPZ
043000940223     C*
043100940607     C                   UPDATE    S1
043200940223     C*
043300940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0434009401313    C     $ESCI         IFEQ      *OFF
043500940607     C                   READC     S1                                     21
043600940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0437009402014    C     *IN21         IFEQ      *ON
043800940201     C                   MOVE      WINZS1        $INZS1
043900940204     C* calcolo pagina a cui deve posizionarsi
044000090225     C                   EXSR      CLCPAG1
0441009402014-   C                   ENDIF
0442009402013-   C                   ENDIF
044300940131     C*
0444009401272-   C                   ENDDO
044500940127     C*
0446009401311-   C                   ENDIF
044700940131     C*
044800940127     C                   ENDSR
044900940127     C/EJECT
045000940127     C************************************************************
045100940131     C* CONTROLLO CAMPI I/O RIGA LISTA
045200940127     C************************************************************
045300940131     C     RECS1         BEGSR
045400940131     C*
045500940201     C* reset indicatori DSPATR
045600940201     C                   MOVE      *ALL'0'       IN4049           10
045700940201     C                   MOVEA     IN4049        *IN(40)
045800940201     C*
045900040930      * selezionato un codice da restituire al chiamante
0460000409303    C     S1OPZ         IFeq      1
046100040930     c                   move      s1cod         x1cod
046200040930     c                   clear                   kpjbu
046300040930     c                   movel     x1cod         kpjbu
046400131015     C******             MOVEL     *ON           $ESCI
046500131015     C******             MOVEL     *ON           $fine
046600040930     C                   END
046700040930     C*
046800940131     C                   ENDSR
046900940131     C************************************************************
047000940131     C* GESTIONE OPZIONI LISTA
047100940131     C************************************************************
047200940131     C     OPZS1         BEGSR
047300940201     C*
047400131015     C*  Solo in Visualizzazione
047500030113     C                   RESET                   tabds
047600100802     C                   MOVE      *all'0'       xtaopz
047700131015     C                   MOVE      '05'          xtaopz
047800030113     C                   MOVE      *ZERO         xtaret
047900030113     C                   MOVE      *ZERO         xtaopr
048000100805     C                   MOVEl     *blank        xtakey
048100100805     c                   eval      Kcodice  = s1cod
048200100805     c                   eval      Kunbcli  = s1unb
048300100803     c                   eval      Ktmsg    = s1TMSG
048400100803     c                   eval      Kvers    = s1VERS
048500100803     c                   eval      Krels    = s1REL
048600100803     c                   eval      Kagenz   = s1AGEN
048700100803     c                   eval      Kassoc   = s1ASSO
048800100804     c                   eval      Kobblig  = h1OBBL
048900100804     c                   eval      Kdescr   = h1descr
049000100804     c                   eval      Kdesci   = h1desci
049100100805     c                   eval      oldcodice  = s1cod
049200100805     c                   eval      oldunbcli  = s1unb
049300100805     c                   eval      oldtmsg    = s1TMSG
049400100805     c                   eval      oldvers    = s1VERS
049500100805     c                   eval      oldrels    = s1REL
049600100805     c                   eval      oldagenz   = s1AGEN
049700100805     c                   eval      oldassoc   = s1ASSO
049800940715     C                   MOVE      *BLANKS       KPJBU
049900030113     C                   MOVEL     tabds         KPJBU
050000131015$004 C                   CALL      'TRTCTB0R2'                          99
050100940607     C                   PARM                    KPJBA
050200100804     c   99              exfmt     ALERTWIND
050300090225      *
050400030114     C                   MOVEL     KPJBU         tabds
050500940201     C*
050600940223     C* ritorno da PGM gestione
050700940223     C                   EXSR      GESRET
050800040930     C*
050900940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0510009402252    C     *IN99         IFEQ      *ON
051100940223     C                   SETON                                        40
0512009402252-   C                   ENDIF
051300040930     C*
051400940131     C                   ENDSR
051500940223     C/EJECT
051600940223     C************************************************************
051700940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
051800940223     C************************************************************
051900940223     C     GESRET        BEGSR
052000940223     C*
052100940223     C* modo di ritorno
052200940223     C*
0523009402231    C                   SELECT
052400940314    >C* << questi modi di utilizzo dei valori di ritorno dal
052500940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
052600940314    >C*    proposte, normalmente sempre valide, ma modificabili
052700940314    >C*    per situazioni particolari >>
052800940223     C* 1 = F3
052900030113    >C     xtaret        WHENEQ    '1'
053000940224     C                   MOVE      *ON           $ESCI
053100940223     C                   MOVE      *ON           $FINE
053200940223     C* 2 = F12
053300030113    >C     xtaret        WHENEQ    '2'
053400940223     C                   MOVE      *ON           $ESCI
053500940223     C*
0536009402231-   C                   ENDSL
053700940223     C*
053800940223     C* operazione eseguite dal pgm chiamato
053900940223     C*
0540009402231    C                   SELECT
054100940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
054200030113    >C     xtaopr        WHENEQ    '1'
054300940223     C                   MOVE      *ON           WINZS1
054400940223     C*
0545009402231-   C                   ENDSL
054600940223     C*
054700940223     C* funzione non eseguibile per errore :
054800940223     C*
0549009402231    C                   SELECT
055000940223     C* 1 = funzione richiamata chiusa in errore
055100940316    >C*  eventualmente gestire altri codici di errore
055200030113    >C     xtaerr        WHENEQ    '1'
055300940223     C                   MOVE      *ON           $ESCI
055400940223     C                   SETON                                        5299
055500940223     C*
0556009402231-   C                   ENDSL
055700940223     C*
055800940223     C                   ENDSR
055900940223     C/EJECT
056000940131     C************************************************************
056100940131     C* OPERAZIONI INIZIALI
056200940131     C************************************************************
056300940131     C     *INZSR        BEGSR
056400030113     C*
056500030113     C* Reperimento parametri
056600030113     C     *ENTRY        PLIST
056700030113     C                   PARM                    KPJBA
056800040930     C*
056900040930     C* Se chiamato x selezionare un codice
057000040930     c                   clear                   x1cod
057100030113     C*
057200030113     C* Variabili per gestione videate
057300030113     C                   MOVE      *BLANK        $GEST             2
057400030113     C                   MOVE      *BLANK        $FINE             1
057500030113     C                   MOVE      *BLANK        $INZS1            1
057600030113     C                   MOVE      *BLANK        $EFILE            1
057700030113     C                   MOVE      *BLANK        $ESCI             1
057800030113     C                   MOVE      *BLANK        $RCDOK            1
057900030113     C* Indici
058000030113     C                   Z-ADD     0             X                 3 0
058100030113     C                   Z-ADD     0             Y                 3 0
058200940506     C*
058300940506     C* Reperimento tasti di funzione
058400940127     C*
058500940117     C                   ENDSR
058600030113     C************************************************************
