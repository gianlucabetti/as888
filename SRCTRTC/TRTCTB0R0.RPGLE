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
001900100802     Fedmap01l  IF   E           K DISK
002000131015     FTRTctb0D0 CF   E             WORKSTN
002100940607     F                                     SFILE(S1:S1NRR)
002200940201     F                                     INFDS(DSFMT)
002300940128     D*----------------------------------------------------*
002400940211     D* Passaggio Parametri
002500940211     D KPJBA         E DS
002600030113      *-------------
002700940325     D* Parametri in ricezione
002800030113     D  TABDS          DS
002900030113     D  XTAOPZ                 1      2
003000030113     D  XTARET                 3      3
003100030113     D  XTAOPR                 4      4
003200030113     D  XTAERR                 5      5
003300100805     D  XTAKEY                 6     20
003400100805$003 D Kcodice                             Like(MAPcodice)
003500100805$003 D Kunbcli                             Like(MAPUNBCLI)
003600100803$003 D Ktmsg                               Like(MAPTIPOMSG)
003700100803$003 D Kvers                               Like(MAPVERSION)
003800100803$003 D Krels                               Like(MAPRELEASE)
003900100803$003 D Kagenz                              Like(MAPAGENCY)
004000100803$003 D Kassoc                              Like(MAPASSOCIA)
004100100804$003 D KObblig                             Like(MAPOBBLIG)
004200100804$003 D KDescr                              Like(MAPDESCR)
004300100804$003 D KDesci                              Like(MAPDESCI)
004400100805$003 D OLDCodice                           Like(MAPcodice)
004500100805$003 D OLDunbcli                           Like(MAPUNBCLI)
004600100805$003 D OLDtmsg                             Like(MAPTIPOMSG)
004700100805$003 D OLDvers                             Like(MAPVERSION)
004800100805$003 D OLDrels                             Like(MAPRELEASE)
004900100805$003 D OLDagenz                            Like(MAPAGENCY)
005000100805$003 D OLDassoc                            Like(MAPASSOCIA)
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
006900030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
007000090225$003 D S1NRR           S                   Like(C1rcd)
007100030113$003 D WSfl            S                   Like(C1nrr)
007200030113$003 D Wmax            S                   Like(C1rcd)
007300030113$003 D Wpag            S                   Like(C1rcd)
007400090225$003 D Winzs1          S                   Like($inzs1)
007500131015$003 D x1cod           S                   Like(c1cod)
007600090225     D*-------------
007700100802$003 D segmento        S                   Like(mapCODICE)
007800940207     D*-------------
007900090225     D SFLPAG1         C                   CONST(06)
008000100802     D*-------------
008100940314     D* dimensione della schiera $MS1
008200940506     D*
008300940506     D* Tasti di funzione
008400940506     D F01             C                   CONST(X'31')
008500940506     D F02             C                   CONST(X'32')
008600940506     D F03             C                   CONST(X'33')
008700940506     D F04             C                   CONST(X'34')
008800940506     D F05             C                   CONST(X'35')
008900940506     D F06             C                   CONST(X'36')
009000940506     D F07             C                   CONST(X'37')
009100940506     D F08             C                   CONST(X'38')
009200940506     D F09             C                   CONST(X'39')
009300940506     D F10             C                   CONST(X'3A')
009400940506     D F11             C                   CONST(X'3B')
009500940506     D F12             C                   CONST(X'3C')
009600940506     D F13             C                   CONST(X'B1')
009700940506     D F14             C                   CONST(X'B2')
009800940506     D F15             C                   CONST(X'B3')
009900940506     D F16             C                   CONST(X'B4')
010000940506     D F17             C                   CONST(X'B5')
010100940506     D F18             C                   CONST(X'B6')
010200940506     D F19             C                   CONST(X'B7')
010300940506     D F20             C                   CONST(X'B8')
010400940506     D F21             C                   CONST(X'B9')
010500940506     D F22             C                   CONST(X'BA')
010600940506     D F23             C                   CONST(X'BB')
010700940506     D F24             C                   CONST(X'BC')
010800940506     D ENTER           C                   CONST(X'F1')
010900940506     D ROLDWN          C                   CONST(X'F4')
011000940506     D ROLLUP          C                   CONST(X'F5')
011100940127     C*----------------------------------------------------*
011200940127     C*                MAIN LINE PROGRAM
011300940127     C*----------------------------------------------------*
011400940223     C* inizializzazione variabili
011500940223     C                   EXSR      INZVAR
011600940223     C*
011700940223     C     $FINE         DOWEQ     *OFF
011800940131     C     $GEST         CASEQ     'S1'          GESS1
011900940117     C                   END
012000940117     C                   END
012100940325     C* fine programma
012200131015     C                   eval        KPJBU  =  tabds
012300940325     C                   SETON                                            LR
012400030113     C************************************************************
012500030113     C* INIZIALIZZAZIONE VARIABILI
012600030113     C************************************************************
012700030113     C     INZVAR        BEGSR
012800131015     C*
012900131015     C                   eval      tabds = KPJBU
013000030113     C*
013100030113     C* Pulizia campi e indicatori
013200030113     C                   MOVE      *ALL'0'       IN4049           10
013300030113     C                   MOVEA     IN4049        *IN(40)
013400030113     C                   CLEAR                   S1OPZ
013500030113     C*
013600090225     C* Variabili per gestione videate
013700030113     C                   MOVE      *OFF          $FINE
013800030113     C                   MOVE      *OFF          $EFILE
013900030113     C                   MOVE      *OFF          $ESCI
014000030113     C                   MOVE      *OFF          $RCDOK
014100030113     C                   Z-ADD     0             $ULKS1            3 0
014200030113     C*
014300030113     C                   MOVE      *ON           $INZS1
014400100802     C                   MOVE      'S1'          $GEST
014500030113     C* Variabili appoggio
014600030114     C                   Z-ADD     1             WPAG
014700030113     c*
014800030113     C                   ENDSR
014900940127     C************************************************************
015000940131     C* GESTIONE LISTA
015100940127     C************************************************************
015200940127     C     GESS1         BEGSR
015300030113     C*
015400131015     C*  Se è stato richiamato in INSERIMENTO
015500131015     c                   if        xtaopz ='10'
015600131015     C                   EXSR      F10S1
015700131015     c                   else
015800131015     C*
015900940223     C* inizializzazione videata
016000940223     C     $INZS1        IFEQ      *ON
016100940127     C                   EXSR      INZS1
016200940223     C                   MOVE      *OFF          $INZS1
016300940127     C                   ENDIF
016400030113     C*
016500030113     C* emissione piede videata
016600030113     C                   WRITE     Z1
016700030113     C* Non ci sono records
016800940223     C     WMAX          IFEQ      0
016900940607     C                   WRITE     D1
017000030114     C                   Else
017100030114     C     Wsfl          IFgt      0
017200030114     C                   Z-ADD     wsfl          C1RCD
017300030114     C                   Else
017400030114     C     Wpag          IFgt      0
017500030114     C                   Z-ADD     wpag          C1RCD
017600030114     C                   EndIF
017700030114     C                   EndIF
017800030114     C                   ENDIF
017900940127     C*
018000100809     C     WMAX          IFLT      C1RCD
018100100809     C                   Z-ADD     1             C1RCD
018200100809     C                   ENDIF
018300030113     C*              *------------------*
018400940607     C                   EXFMT     C1
018500030113     C*              *------------------*
018600030113     C*
018700940204     C     C1NRR         IFNE      0
018800940204     C                   Z-ADD     C1NRR         WSFL
018900940204     C                   ENDIF
019000940127     C                   Z-ADD     SFLNRR        C1RCD
019100030113     C* Selezioni
0192009401271    C                   SELECT
019300940127     C* F3=Fine
019400940506     C     $TASTO        WHENEQ    F03
019500940309     C                   EXSR      F03S1
019600940131     C* F10=Immissione
019700940506     C     $TASTO        WHENEQ    F10
019800940309     C                   EXSR      F10S1
019900090225     C* F12=Ritorno
020000090225     C     $TASTO        WHENEQ    F12
020100131015     C                   EXSR      F12S1
0202009401271O   C                   OTHER
020300940127     C* CONTROLLO DATI
020400940131     C                   EXSR      CTRC1
020500940201     C     *IN99         IFEQ      *OFF
020600940131     C                   EXSR      CTRS1
020700940131     C                   END
0208009401271-   C                   ENDSL
020900940127     C*
021000131015     c                   end
021100131015     C*
021200940127     C                   ENDSR
021300940224     C/EJECT
021400940127     C************************************************************
021500940131     C* INIZIALIZZAZIONE LISTA
021600940127     C************************************************************
021700940127     C     INZS1         BEGSR
021800940302     C* pulizia SFL
021900940128     C                   SETOFF                                         3031
022000940607     C                   WRITE     C1
022100940128     C                   SETON                                          31
022200940128     C*
022300030113     C* CARICAMENTO SFL totale
022400940201     C                   Z-ADD     0             S1NRR
022500030113     C                   Z-ADD     1             C1RCD
022600940128     C                   Z-ADD     0             WMAX
022700090225     C                   Z-ADD     0             wsfl
022800940224     C*
022900131015     c                   eval         c1UNB   =    Kunbcli
023000131015     c                   eval         c1TMSG  =    Ktmsg
023100131015     c                   eval         c1VERS  =    Kvers
023200131015     c                   eval         c1REL   =    Krels
023300131015     c                   eval         c1AGEN  =    Kagenz
023400131015     c                   eval         c1ASSO  =    Kassoc
023500131015     c                   eval         c1COD   =    Kcodice
023600131015     c                   eval         c1DES   =    Kdescr
023700131015     C*
023800940224     C* Posizionamento su file pilota
023900131015     c                   eval         MAPUNBCLI   =    Kunbcli
024000131015     c                   eval         MAPTIPOMSG  =    Ktmsg
024100131015     c                   eval         MAPVERSION  =    Kvers
024200131015     c                   eval         MAPRELEASE  =    Krels
024300131015     c                   eval         MAPAGENCY   =    Kagenz
024400131015     c                   eval         MAPASSOCIA  =    Kassoc
024500131015     c                   eval         MAPCODICE   =    Kcodice
024600131015     C*
024700131015     c     KeyReade      setll     edmap01L
024800940608    >C                   EXSR      REDANA
024900030113     C* Carico il SFL
025000940127     C                   EXSR      ROLS1
025100030113     C*
025200030114     c                   if        xtaopr <> '1'
025300030114     C                   Z-ADD     1             WPAG
025400030114     c                   end
025500940127     C*
025600940127     C                   ENDSR
025700940127     C************************************************************
025800940131     C* CARICAMENTO PAGINA LISTA
025900940127     C************************************************************
026000940127     C     ROLS1         BEGSR
026100940127     C*
026200940128     C                   SETOFF                                       32
026300940223     C                   Z-ADD     0             Y
026400940127     C                   Z-ADD     WMAX          S1NRR
026500940127     C*
026600940127     C* Leggo dal file anagrafico per caricare la lista
0267009401311    C     $EFILE        DOWEQ     *OFF
026800940127     C*
026900030113     c                   clear                   s1opz
027000131015     c                   eval      s1SEQUEN   = mapSEQUEN
027100131015     c                   eval      s1PROGR    = mapPROGR
027200131015     c                   eval      s1CATEGO   = mapCATEGO
027300131015     c                   eval      s1NCAMPO   = mapNCAMPO
027400131015     c                   eval      s1OBBLIG   = mapOBBLIG
027500131015     c                   eval      s1TIPOD    = mapTIPOD
027600131015     c                   eval      s1LUNGH    = mapLUNGH
027700131015     c                   eval      s1DECIM    = mapDECIM
027800131015     c                   eval      s1DESCR    = mapDESCR
027900131015     c                   eval      s1DESCI    = mapDESCI
028000131015     C*
028100940127     C                   ADD       1             S1NRR
028200940127     C                   ADD       1             Y
028300940127     C*
028400940607     C                   WRITE     S1
028500940131     C*
028600940316    >C                   EXSR      REDANA
0287009401271-   C                   ENDDO
028800940127     C*
028900940223     C                   Z-ADD     S1NRR         WMAX                 30
029000940127     C*
029100940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
029200940127     C*
029300090225     C     S1NRR         DIV       SFLPAG1       PAGINE            4 0
029400940127     C                   MVR                     RESTO             3 0
029500090225     C     PAGINE        MULT      SFLPAG1       C1RCD
0296000301141    C     RESTO         IFGT      0
029700030114     C                   ADD       1             C1RCD
0298000301141E   C                   ELSE
029900090225     C                   SUB       SFLPAG1       C1RCD
030000030114     C                   ADD       1             C1RCD
0301000301141-   C                   ENDIF
030200940128     C*
030300940127     C                   ENDSR
030400940128     C************************************************************
030500940131     C* LETTURA RCD ARCHIVIO PILOTA
030600940128     C************************************************************
030700940607     C     REDANA        BEGSR
030800940128     C*
030900940131     C                   MOVEL     *OFF          $EFILE
031000940131     C                   MOVEL     *OFF          $RCDOK
031100940131     C*
0312009401311    C     $EFILE        DOUEQ     *ON
031300940131     C     $RCDOK        OREQ      *ON
031400940131     C*
031500131015     c     KeyReade      Reade     edMAP01L
031600100802     c                   if        %eof(edmap01L)
031700030113     C                   MOVEL     *ON           $EFILE
031800030113     C                   MOVE      $EFILE        *IN33
031900030113     c                   else
032000030113     C                   MOVE      *ON           $RCDOK
032100131015     c                   if          MAPTIPOD  = *blank   or
032200131015     c                               MAPTIPOD  = 'D'
032300131015     C                   MOVE      *OFF          $RCDOK
032400131015     c                   end
032500030113     c                   end
032600940131     C*
0327009401311-   C                   ENDDO
032800940131     C*
032900940131     C                   ENDSR
033000090225     C************************************************************
033100090225     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
033200090225     C************************************************************
033300090225     C     CLCPAG1       BEGSR
033400090225     C* Input :
033500090225     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
033600090225     C*          cursore
033700090225     C* - SFLPAG = numero rcd per pagina sfl
033800090225     C* Output :
033900090225     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
034000090225     C*
034100090225     C     WSFL          DIV       SFLPAG1       PAGINE            4 0
034200090225     C                   MVR                     RESTO             3 0
034300090225     C     RESTO         IFGT      0
034400090225     C                   ADD       1             PAGINE
034500090225     C                   ENDIF
034600090226     c                   if        pagine > 1
034700090225     C     PAGINE        MULT      SFLPAG1       WPAG
034800090226     C                   ENDIF
034900090225     C*
035000090225     C                   ENDSR
035100940309     C************************************************************
035200940309     C* GESTIONE F03 VIDEO S1
035300940309     C************************************************************
035400940309     C     F03S1         BEGSR
035500940309     C*
035600131015     c                   eval      xtaRET ='1'
035700940309     C                   MOVE      *ON           $FINE
035800940325     C* fine programma
035900940309     C                   ENDSR
036000940309     C/EJECT
036100131015     C************************************************************
036200131015     C* GESTIONE F12 VIDEO S0
036300131015     C************************************************************
036400131015     C     F12S1         BEGSR
036500131015     C*
036600131015     c                   eval      xtaRET ='2'
036700131015     C                   MOVE      *ON           $FINE
036800131015     C* fine programma
036900131015     C                   ENDSR
037000131015     C/EJECT
037100940309     C************************************************************
037200940309     C* GESTIONE F10 VIDEO S1
037300940314     c* AGGIUNTA RECORD
037400940309     C************************************************************
037500940309     C     F10S1         BEGSR
037600940309     C*
037700030113     C                   RESET                   tabds
037800030113     C                   MOVEL     '01'          xtaopz
037900030113     C                   MOVE      *ZERO         xtaret
038000030113     C                   MOVE      *ZERO         xtaopr
038100030114     C                   MOVEl     *blank        xtakey
038200100805     C                   clear                   Kcodice
038300100805     C                   clear                   Kunbcli
038400100803     C                   clear                   Ktmsg
038500100803     C                   clear                   Kvers
038600100803     C                   clear                   Krels
038700100803     C                   clear                   Kagenz
038800100803     C                   clear                   Kassoc
038900100805     C                   clear                   Kdescr
039000100805     C                   clear                   Kdesci
039100100805     C                   clear                   oldcodice
039200100805     C                   clear                   oldunbcli
039300100805     C                   clear                   oldtmsg
039400100805     C                   clear                   oldvers
039500100805     C                   clear                   oldrels
039600100805     C                   clear                   oldagenz
039700100805     C                   clear                   oldassoc
039800030113     C                   MOVE      *BLANKS       KPJBU
039900030113     C                   MOVEL     tabds         KPJBU
040000100804$004 C                   CALL      'TRTCTB0R1'                          99
040100030113     C                   PARM                    KPJBA
040200100804     c   99              exfmt     ALERTWIND
040300090225      *
040400030114     C                   MOVEL     KPJBU         tabds
040500030113      *
040600940309     C* ritorno da PGM gestione
040700940309     C                   EXSR      GESRET
040800090226     C                   MOVE      'S1'          $GEST
040900090226      *
041000940309     C     WINZS1        IFEQ      *ON
041100940309     C                   MOVE      *ON           $INZS1
041200090226     C* carico sempre la 1a pagina
041300090226     C                   Z-ADD     1             WPAG
041400940309     C                   ENDIF
041500940309     C*
041600940309     C                   ENDSR
041700940128     C************************************************************
041800940131     C* CONTROLLO TESTATA LISTA
041900940128     C************************************************************
042000940131     C     CTRC1         BEGSR
042100940128     C*
042200940201     C                   MOVE      *OFF          *IN99
042300940131     C*
042400940202     C                   ENDSR
042500940131     C************************************************************
042600940131     C* CONTROLLO OPZIONI LISTA
042700940131     C************************************************************
042800940131     C     CTRS1         BEGSR
042900940131     C*
043000940202     C                   MOVEL     *OFF          $ESCI
043100940201     C                   SETOFF                                       99
043200940131     C                   Z-ADD     0             S1OPZ
043300040930     c                   clear                   x1cod
043400940131     C*
043500940127     C* Leggo il sfl solo se ci sono rcd
0436009401311    C     WMAX          IFGT      0
043700940607     C                   READC     S1                                     21
043800940127     C*
043900940131     C* esce se fine sfl o errore che richiede l'uscita
0440009401312    C     *IN21         DOWEQ     *OFF
044100940131     C     $ESCI         ANDEQ     *OFF
044200940201     C                   Z-ADD     S1NRR         C1RCD
044300940131     C* ctrl su riga
044400940131     C                   EXSR      RECS1
044500940131     C* gestione opzioni
0446009401313    C     S1OPZ         IFNE      0
044700940201     C     *IN99         ANDEQ     *OFF
044800940131     C                   EXSR      OPZS1
0449009401273-   C                   ENDIF
045000940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0451009402013    C     *IN99         IFEQ      *ON
045200940201     C     $ESCI         OREQ      *ON
045300940131     C                   MOVE      *ON           *IN32
045400940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
045500940204     C* la ripristinerò a conclusione del ciclo di READC
045600940223     C                   MOVE      *OFF          $INZS1
0457009402233-   C                   ENDIF
045800940223     C*
045900940131     C                   Z-ADD     0             S1OPZ
046000940223     C*
046100940607     C                   UPDATE    S1
046200940223     C*
046300940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0464009401313    C     $ESCI         IFEQ      *OFF
046500940607     C                   READC     S1                                     21
046600940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0467009402014    C     *IN21         IFEQ      *ON
046800940201     C                   MOVE      WINZS1        $INZS1
046900940204     C* calcolo pagina a cui deve posizionarsi
047000090225     C                   EXSR      CLCPAG1
0471009402014-   C                   ENDIF
0472009402013-   C                   ENDIF
047300940131     C*
0474009401272-   C                   ENDDO
047500940127     C*
0476009401311-   C                   ENDIF
047700940131     C*
047800940127     C                   ENDSR
047900940127     C/EJECT
048000940127     C************************************************************
048100940131     C* CONTROLLO CAMPI I/O RIGA LISTA
048200940127     C************************************************************
048300940131     C     RECS1         BEGSR
048400940131     C*
048500940201     C* reset indicatori DSPATR
048600940201     C                   MOVE      *ALL'0'       IN4049           10
048700940201     C                   MOVEA     IN4049        *IN(40)
048800940201     C*
048900040930      * selezionato un codice da restituire al chiamante
0490000409303    C     S1OPZ         IFeq      1
049100131015     c                   move      c1cod         x1cod
049200040930     c                   clear                   kpjbu
049300040930     c                   movel     x1cod         kpjbu
049400040930     C                   MOVEL     *ON           $ESCI
049500040930     C                   MOVEL     *ON           $fine
049600040930     C                   END
049700040930     C*
049800940131     C                   ENDSR
049900940131     C************************************************************
050000940131     C* GESTIONE OPZIONI LISTA
050100940131     C************************************************************
050200940131     C     OPZS1         BEGSR
050300940201     C*
050400030113     C                   RESET                   tabds
050500100802     C                   MOVE      *all'0'       xtaopz
050600100802     C                   MOVE      S1OPZ         xtaopz
050700030113     C                   MOVE      *ZERO         xtaret
050800030113     C                   MOVE      *ZERO         xtaopr
050900100805     C                   MOVEl     *blank        xtakey
051000131015     c                   eval      Kcodice  = c1cod
051100131015     c                   eval      Kunbcli  = c1unb
051200131015     c                   eval      Ktmsg    = c1TMSG
051300131015     c                   eval      Kvers    = c1VERS
051400131015     c                   eval      Krels    = c1REL
051500131015     c                   eval      Kagenz   = c1AGEN
051600131015     c                   eval      Kassoc   = c1ASSO
051700131015     c                   eval      Kobblig  = *blank
051800131015     c                   eval      Kdescr   = *blank
051900131015     c                   eval      Kdesci   = *blank
052000131015     c                   eval      oldcodice  = c1cod
052100131015     c                   eval      oldunbcli  = c1unb
052200131015     c                   eval      oldtmsg    = c1TMSG
052300131015     c                   eval      oldvers    = c1VERS
052400131015     c                   eval      oldrels    = c1REL
052500131015     c                   eval      oldagenz   = c1AGEN
052600131015     c                   eval      oldassoc   = c1ASSO
052700940715     C                   MOVE      *BLANKS       KPJBU
052800030113     C                   MOVEL     tabds         KPJBU
052900100804$004 C                   CALL      'TRTCTB0R1'                          99
053000940607     C                   PARM                    KPJBA
053100100804     c   99              exfmt     ALERTWIND
053200090225      *
053300030114     C                   MOVEL     KPJBU         tabds
053400940201     C*
053500940223     C* ritorno da PGM gestione
053600940223     C                   EXSR      GESRET
053700040930     C*
053800940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0539009402252    C     *IN99         IFEQ      *ON
054000940223     C                   SETON                                        40
0541009402252-   C                   ENDIF
054200040930     C*
054300940131     C                   ENDSR
054400940223     C/EJECT
054500940223     C************************************************************
054600940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
054700940223     C************************************************************
054800940223     C     GESRET        BEGSR
054900940223     C*
055000940223     C* modo di ritorno
055100940223     C*
0552009402231    C                   SELECT
055300940314    >C* << questi modi di utilizzo dei valori di ritorno dal
055400940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
055500940314    >C*    proposte, normalmente sempre valide, ma modificabili
055600940314    >C*    per situazioni particolari >>
055700940223     C* 1 = F3
055800030113    >C     xtaret        WHENEQ    '1'
055900940224     C                   MOVE      *ON           $ESCI
056000940223     C                   MOVE      *ON           $FINE
056100940223     C* 2 = F12
056200030113    >C     xtaret        WHENEQ    '2'
056300131015     C                   MOVE      *ON           $FINE
056400940223     C                   MOVE      *ON           $ESCI
056500940223     C*
0566009402231-   C                   ENDSL
056700940223     C*
056800940223     C* operazione eseguite dal pgm chiamato
056900940223     C*
0570009402231    C                   SELECT
057100940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
057200030113    >C     xtaopr        WHENEQ    '1'
057300940223     C                   MOVE      *ON           WINZS1
057400940223     C*
0575009402231-   C                   ENDSL
057600940223     C*
057700940223     C* funzione non eseguibile per errore :
057800940223     C*
0579009402231    C                   SELECT
058000940223     C* 1 = funzione richiamata chiusa in errore
058100940316    >C*  eventualmente gestire altri codici di errore
058200030113    >C     xtaerr        WHENEQ    '1'
058300940223     C                   MOVE      *ON           $ESCI
058400940223     C                   SETON                                        5299
058500940223     C*
0586009402231-   C                   ENDSL
058700940223     C*
058800940223     C                   ENDSR
058900940223     C/EJECT
059000940131     C************************************************************
059100940131     C* OPERAZIONI INIZIALI
059200940131     C************************************************************
059300940131     C     *INZSR        BEGSR
059400030113     C*
059500030113     C* Reperimento parametri
059600030113     C     *ENTRY        PLIST
059700030113     C                   PARM                    KPJBA
059800040930     C*
059900040930     C* Se chiamato x selezionare un codice
060000040930     c                   clear                   x1cod
060100030113     C*
060200030113     C* Variabili per gestione videate
060300030113     C                   MOVE      *BLANK        $GEST             2
060400030113     C                   MOVE      *BLANK        $FINE             1
060500030113     C                   MOVE      *BLANK        $INZS1            1
060600030113     C                   MOVE      *BLANK        $EFILE            1
060700030113     C                   MOVE      *BLANK        $ESCI             1
060800030113     C                   MOVE      *BLANK        $RCDOK            1
060900030113     C* Indici
061000030113     C                   Z-ADD     0             X                 3 0
061100030113     C                   Z-ADD     0             Y                 3 0
061200940506     C*
061300940506     C* Reperimento tasti di funzione
061400131015     c     KeyReade      klist
061500131015     c                   kfld                    MAPUNBCLI
061600131015     c                   kfld                    MAPTIPOMSG
061700131015     c                   kfld                    MAPVERSION
061800131015     c                   kfld                    MAPRELEASE
061900131015     c                   kfld                    MAPAGENCY
062000131015     c                   kfld                    MAPASSOCIA
062100131015     c                   kfld                    MAPCODICE
062200940127     C*
062300940117     C                   ENDSR
062400030113     C************************************************************
