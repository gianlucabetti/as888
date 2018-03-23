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
002100100802     Fedmap01l  IF   E           K DISK
002200100802     FTRTctb0D  CF   E             WORKSTN
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
003500100805     D  XTAKEY                 6     20
003600100805$003 D Kcodice                             Like(MAPcodice)
003700100805$003 D Kunbcli                             Like(MAPUNBCLI)
003800100803$003 D Ktmsg                               Like(MAPTIPOMSG)
003900100803$003 D Kvers                               Like(MAPVERSION)
004000100803$003 D Krels                               Like(MAPRELEASE)
004100100803$003 D Kagenz                              Like(MAPAGENCY)
004200100803$003 D Kassoc                              Like(MAPASSOCIA)
004300100804$003 D KObblig                             Like(MAPOBBLIG)
004400100804$003 D KDescr                              Like(MAPDESCR)
004500100804$003 D KDesci                              Like(MAPDESCI)
004600100805$003 D OLDCodice                           Like(MAPcodice)
004700100805$003 D OLDunbcli                           Like(MAPUNBCLI)
004800100805$003 D OLDtmsg                             Like(MAPTIPOMSG)
004900100805$003 D OLDvers                             Like(MAPVERSION)
005000100805$003 D OLDrels                             Like(MAPRELEASE)
005100100805$003 D OLDagenz                            Like(MAPAGENCY)
005200100805$003 D OLDassoc                            Like(MAPASSOCIA)
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
007100030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
007200090225$003 D S1NRR           S                   Like(C1rcd)
007300030113$003 D WSfl            S                   Like(C1nrr)
007400030113$003 D Wmax            S                   Like(C1rcd)
007500030113$003 D Wpag            S                   Like(C1rcd)
007600090225$003 D Winzs1          S                   Like($inzs1)
007700040930$003 D x1cod           S                   Like(s1cod)
007800090225     D*-------------
007900100802$003 D segmento        S                   Like(mapCODICE)
008000940207     D*-------------
008100090225     D SFLPAG1         C                   CONST(06)
008200100802     D*-------------
008300940314     D* dimensione della schiera $MS1
008400940506     D*
008500940506     D* Tasti di funzione
008600940506     D F01             C                   CONST(X'31')
008700940506     D F02             C                   CONST(X'32')
008800940506     D F03             C                   CONST(X'33')
008900940506     D F04             C                   CONST(X'34')
009000940506     D F05             C                   CONST(X'35')
009100940506     D F06             C                   CONST(X'36')
009200940506     D F07             C                   CONST(X'37')
009300940506     D F08             C                   CONST(X'38')
009400940506     D F09             C                   CONST(X'39')
009500940506     D F10             C                   CONST(X'3A')
009600940506     D F11             C                   CONST(X'3B')
009700940506     D F12             C                   CONST(X'3C')
009800940506     D F13             C                   CONST(X'B1')
009900940506     D F14             C                   CONST(X'B2')
010000940506     D F15             C                   CONST(X'B3')
010100940506     D F16             C                   CONST(X'B4')
010200940506     D F17             C                   CONST(X'B5')
010300940506     D F18             C                   CONST(X'B6')
010400940506     D F19             C                   CONST(X'B7')
010500940506     D F20             C                   CONST(X'B8')
010600940506     D F21             C                   CONST(X'B9')
010700940506     D F22             C                   CONST(X'BA')
010800940506     D F23             C                   CONST(X'BB')
010900940506     D F24             C                   CONST(X'BC')
011000940506     D ENTER           C                   CONST(X'F1')
011100940506     D ROLDWN          C                   CONST(X'F4')
011200940506     D ROLLUP          C                   CONST(X'F5')
011300940127     C*----------------------------------------------------*
011400940127     C*                MAIN LINE PROGRAM
011500940127     C*----------------------------------------------------*
011600940223     C* inizializzazione variabili
011700940223     C                   EXSR      INZVAR
011800940223     C*
011900940223     C     $FINE         DOWEQ     *OFF
012000940131     C     $GEST         CASEQ     'S1'          GESS1
012100940117     C                   END
012200940117     C                   END
012300940325     C* fine programma
012400940325     C                   SETON                                            LR
012500030113     C************************************************************
012600030113     C* INIZIALIZZAZIONE VARIABILI
012700030113     C************************************************************
012800030113     C     INZVAR        BEGSR
012900030113     C*
013000030113     C* Pulizia campi e indicatori
013100030113     C                   MOVE      *ALL'0'       IN4049           10
013200030113     C                   MOVEA     IN4049        *IN(40)
013300030113     C                   CLEAR                   S1OPZ
013400030113     C*
013500090225     C* Variabili per gestione videate
013600030113     C                   MOVE      *OFF          $FINE
013700030113     C                   MOVE      *OFF          $EFILE
013800030113     C                   MOVE      *OFF          $ESCI
013900030113     C                   MOVE      *OFF          $RCDOK
014000030113     C                   Z-ADD     0             $ULKS1            3 0
014100030113     C*
014200030113     C                   MOVE      *ON           $INZS1
014300100802     C                   MOVE      'S1'          $GEST
014400030113     C* Variabili appoggio
014500030114     C                   Z-ADD     1             WPAG
014600030113     c*
014700030113     C                   ENDSR
014800940127     C************************************************************
014900940131     C* GESTIONE LISTA
015000940127     C************************************************************
015100940127     C     GESS1         BEGSR
015200030113     C*
015300940223     C* inizializzazione videata
015400940223     C     $INZS1        IFEQ      *ON
015500940127     C                   EXSR      INZS1
015600940223     C                   MOVE      *OFF          $INZS1
015700940127     C                   ENDIF
015800030113     C*
015900030113     C* emissione piede videata
016000030113     C                   WRITE     Z1
016100030113     C* Non ci sono records
016200940223     C     WMAX          IFEQ      0
016300940607     C                   WRITE     D1
016400030114     C                   Else
016500030114     C     Wsfl          IFgt      0
016600030114     C                   Z-ADD     wsfl          C1RCD
016700030114     C                   Else
016800030114     C     Wpag          IFgt      0
016900030114     C                   Z-ADD     wpag          C1RCD
017000030114     C                   EndIF
017100030114     C                   EndIF
017200030114     C                   ENDIF
017300940127     C*
017400100809     C     WMAX          IFLT      C1RCD
017500100809     C                   Z-ADD     1             C1RCD
017600100809     C                   ENDIF
017700030113     C*              *------------------*
017800940607     C                   EXFMT     C1
017900030113     C*              *------------------*
018000030113     C*
018100940204     C     C1NRR         IFNE      0
018200940204     C                   Z-ADD     C1NRR         WSFL
018300940204     C                   ENDIF
018400940127     C                   Z-ADD     SFLNRR        C1RCD
018500030113     C* Selezioni
0186009401271    C                   SELECT
018700940127     C* F3=Fine
018800940506     C     $TASTO        WHENEQ    F03
018900940309     C                   EXSR      F03S1
019000940131     C* F10=Immissione
019100940506     C     $TASTO        WHENEQ    F10
019200940309     C                   EXSR      F10S1
019300090225     C* F12=Ritorno
019400090225     C     $TASTO        WHENEQ    F12
019500100802     C                   eval      $Gest='S1'
019600090225     C                   MOVE      *ON           $INZS1
0197009401271O   C                   OTHER
019800940127     C* CONTROLLO DATI
019900940131     C                   EXSR      CTRC1
020000940201     C     *IN99         IFEQ      *OFF
020100940131     C                   EXSR      CTRS1
020200940131     C                   END
0203009401271-   C                   ENDSL
020400940127     C*
020500940127     C                   ENDSR
020600940224     C/EJECT
020700940127     C************************************************************
020800940131     C* INIZIALIZZAZIONE LISTA
020900940127     C************************************************************
021000940127     C     INZS1         BEGSR
021100940302     C* pulizia SFL
021200940128     C                   SETOFF                                         3031
021300940607     C                   WRITE     C1
021400940128     C                   SETON                                          31
021500940128     C*
021600030113     C* CARICAMENTO SFL totale
021700940201     C                   Z-ADD     0             S1NRR
021800030113     C                   Z-ADD     1             C1RCD
021900940128     C                   Z-ADD     0             WMAX
022000090225     C                   Z-ADD     0             wsfl
022100940224     C*
022200940224     C* Posizionamento su file pilota
022300100802     c     *loval        setll     edmap01L
022400940608    >C                   EXSR      REDANA
022500030113     C* Carico il SFL
022600940127     C                   EXSR      ROLS1
022700030113     C*
022800030114     c                   if        xtaopr <> '1'
022900030114     C                   Z-ADD     1             WPAG
023000030114     c                   end
023100940127     C*
023200940127     C                   ENDSR
023300940127     C************************************************************
023400940131     C* CARICAMENTO PAGINA LISTA
023500940127     C************************************************************
023600940127     C     ROLS1         BEGSR
023700940127     C*
023800940128     C                   SETOFF                                       32
023900940223     C                   Z-ADD     0             Y
024000940127     C                   Z-ADD     WMAX          S1NRR
024100940127     C*
024200940127     C* Leggo dal file anagrafico per caricare la lista
0243009401311    C     $EFILE        DOWEQ     *OFF
024400940127     C*
024500030113     c                   clear                   s1opz
024600100802     c                   eval      s1cod = mapCODICE
024700100802     c                   eval      s1des = mapDESCR
024800100802     c                   eval      s1unb = mapUNBCLI
024900100802     c                   eval      s1TMSG= mapTIPOMSG
025000100802     c                   eval      s1VERS= mapVERSION
025100100802     c                   eval      s1REL = mapRELEASE
025200100802     c                   eval      s1AGEN= mapAGENCY
025300100802     c                   eval      s1ASSO= mapASSOCIA
025400100804     c                   eval      h1obbl= mapOBBLIG
025500100804     c                   eval      h1descr= mapDESCR
025600100804     c                   eval      h1desci= mapDESCI
025700100802     C*
025800940127     C                   ADD       1             S1NRR
025900940127     C                   ADD       1             Y
026000940127     C*
026100940607     C                   WRITE     S1
026200940131     C*
026300940316    >C                   EXSR      REDANA
0264009401271-   C                   ENDDO
026500940127     C*
026600940223     C                   Z-ADD     S1NRR         WMAX                 30
026700940127     C*
026800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
026900940127     C*
027000090225     C     S1NRR         DIV       SFLPAG1       PAGINE            4 0
027100940127     C                   MVR                     RESTO             3 0
027200090225     C     PAGINE        MULT      SFLPAG1       C1RCD
0273000301141    C     RESTO         IFGT      0
027400030114     C                   ADD       1             C1RCD
0275000301141E   C                   ELSE
027600090225     C                   SUB       SFLPAG1       C1RCD
027700030114     C                   ADD       1             C1RCD
0278000301141-   C                   ENDIF
027900940128     C*
028000940127     C                   ENDSR
028100940128     C************************************************************
028200940131     C* LETTURA RCD ARCHIVIO PILOTA
028300940128     C************************************************************
028400940607     C     REDANA        BEGSR
028500940128     C*
028600940131     C                   MOVEL     *OFF          $EFILE
028700940131     C                   MOVEL     *OFF          $RCDOK
028800940131     C*
0289009401311    C     $EFILE        DOUEQ     *ON
029000940131     C     $RCDOK        OREQ      *ON
029100940131     C*
029200100802     c                   Read      edMAP01L
029300100802     c                   if        %eof(edmap01L)
029400030113     C                   MOVEL     *ON           $EFILE
029500030113     C                   MOVE      $EFILE        *IN33
029600030113     c                   else
029700100802     c                   if          MAPSEQUEN = 0  and
029800100802     c                               MAPPROGR  = 0  and
029900100802     c                               MAPTIPOD  = *blank  and
030000100802     c                               MAPCATEGO = *blank  and
030100100802     c                               MAPNCAMPO = *blank
030200030113     C                   MOVE      *ON           $RCDOK
030300030113     c                   end
030400100802     c                   end
030500940131     C*
0306009401311-   C                   ENDDO
030700940131     C*
030800940131     C                   ENDSR
030900090225     C************************************************************
031000090225     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
031100090225     C************************************************************
031200090225     C     CLCPAG1       BEGSR
031300090225     C* Input :
031400090225     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
031500090225     C*          cursore
031600090225     C* - SFLPAG = numero rcd per pagina sfl
031700090225     C* Output :
031800090225     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
031900090225     C*
032000090225     C     WSFL          DIV       SFLPAG1       PAGINE            4 0
032100090225     C                   MVR                     RESTO             3 0
032200090225     C     RESTO         IFGT      0
032300090225     C                   ADD       1             PAGINE
032400090225     C                   ENDIF
032500090226     c                   if        pagine > 1
032600090225     C     PAGINE        MULT      SFLPAG1       WPAG
032700090226     C                   ENDIF
032800090225     C*
032900090225     C                   ENDSR
033000940309     C************************************************************
033100940309     C* GESTIONE F03 VIDEO S1
033200940309     C************************************************************
033300940309     C     F03S1         BEGSR
033400940309     C*
033500940309     C                   MOVE      *ON           $FINE
033600940325     C* fine programma
033700940309     C                   ENDSR
033800940309     C/EJECT
033900940309     C************************************************************
034000940309     C* GESTIONE F10 VIDEO S1
034100940314     c* AGGIUNTA RECORD
034200940309     C************************************************************
034300940309     C     F10S1         BEGSR
034400940309     C*
034500030113     C                   RESET                   tabds
034600030113     C                   MOVEL     '01'          xtaopz
034700030113     C                   MOVE      *ZERO         xtaret
034800030113     C                   MOVE      *ZERO         xtaopr
034900030114     C                   MOVEl     *blank        xtakey
035000100805     C                   clear                   Kcodice
035100100805     C                   clear                   Kunbcli
035200100803     C                   clear                   Ktmsg
035300100803     C                   clear                   Kvers
035400100803     C                   clear                   Krels
035500100803     C                   clear                   Kagenz
035600100803     C                   clear                   Kassoc
035700100805     C                   clear                   Kdescr
035800100805     C                   clear                   Kdesci
035900100805     C                   clear                   oldcodice
036000100805     C                   clear                   oldunbcli
036100100805     C                   clear                   oldtmsg
036200100805     C                   clear                   oldvers
036300100805     C                   clear                   oldrels
036400100805     C                   clear                   oldagenz
036500100805     C                   clear                   oldassoc
036600030113     C                   MOVE      *BLANKS       KPJBU
036700030113     C                   MOVEL     tabds         KPJBU
036800100804$004 C                   CALL      'TRTCTB0R1'                          99
036900030113     C                   PARM                    KPJBA
037000100804     c   99              exfmt     ALERTWIND
037100090225      *
037200030114     C                   MOVEL     KPJBU         tabds
037300030113      *
037400940309     C* ritorno da PGM gestione
037500940309     C                   EXSR      GESRET
037600090226     C                   MOVE      'S1'          $GEST
037700090226      *
037800940309     C     WINZS1        IFEQ      *ON
037900940309     C                   MOVE      *ON           $INZS1
038000090226     C* carico sempre la 1a pagina
038100090226     C                   Z-ADD     1             WPAG
038200940309     C                   ENDIF
038300940309     C*
038400940309     C                   ENDSR
038500940128     C************************************************************
038600940131     C* CONTROLLO TESTATA LISTA
038700940128     C************************************************************
038800940131     C     CTRC1         BEGSR
038900940128     C*
039000940201     C                   MOVE      *OFF          *IN99
039100940131     C*
039200940202     C                   ENDSR
039300940131     C************************************************************
039400940131     C* CONTROLLO OPZIONI LISTA
039500940131     C************************************************************
039600940131     C     CTRS1         BEGSR
039700940131     C*
039800940202     C                   MOVEL     *OFF          $ESCI
039900940201     C                   SETOFF                                       99
040000940131     C                   Z-ADD     0             S1OPZ
040100040930     c                   clear                   x1cod
040200940131     C*
040300940127     C* Leggo il sfl solo se ci sono rcd
0404009401311    C     WMAX          IFGT      0
040500940607     C                   READC     S1                                     21
040600940127     C*
040700940131     C* esce se fine sfl o errore che richiede l'uscita
0408009401312    C     *IN21         DOWEQ     *OFF
040900940131     C     $ESCI         ANDEQ     *OFF
041000940201     C                   Z-ADD     S1NRR         C1RCD
041100940131     C* ctrl su riga
041200940131     C                   EXSR      RECS1
041300940131     C* gestione opzioni
0414009401313    C     S1OPZ         IFNE      0
041500940201     C     *IN99         ANDEQ     *OFF
041600940131     C                   EXSR      OPZS1
0417009401273-   C                   ENDIF
041800940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0419009402013    C     *IN99         IFEQ      *ON
042000940201     C     $ESCI         OREQ      *ON
042100940131     C                   MOVE      *ON           *IN32
042200940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
042300940204     C* la ripristinerò a conclusione del ciclo di READC
042400940223     C                   MOVE      *OFF          $INZS1
0425009402233-   C                   ENDIF
042600940223     C*
042700940131     C                   Z-ADD     0             S1OPZ
042800940223     C*
042900940607     C                   UPDATE    S1
043000940223     C*
043100940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0432009401313    C     $ESCI         IFEQ      *OFF
043300940607     C                   READC     S1                                     21
043400940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0435009402014    C     *IN21         IFEQ      *ON
043600940201     C                   MOVE      WINZS1        $INZS1
043700940204     C* calcolo pagina a cui deve posizionarsi
043800090225     C                   EXSR      CLCPAG1
0439009402014-   C                   ENDIF
0440009402013-   C                   ENDIF
044100940131     C*
0442009401272-   C                   ENDDO
044300940127     C*
0444009401311-   C                   ENDIF
044500940131     C*
044600940127     C                   ENDSR
044700940127     C/EJECT
044800940127     C************************************************************
044900940131     C* CONTROLLO CAMPI I/O RIGA LISTA
045000940127     C************************************************************
045100940131     C     RECS1         BEGSR
045200940131     C*
045300940201     C* reset indicatori DSPATR
045400940201     C                   MOVE      *ALL'0'       IN4049           10
045500940201     C                   MOVEA     IN4049        *IN(40)
045600940201     C*
045700040930      * selezionato un codice da restituire al chiamante
0458000409303    C     S1OPZ         IFeq      1
045900040930     c                   move      s1cod         x1cod
046000040930     c                   clear                   kpjbu
046100040930     c                   movel     x1cod         kpjbu
046200040930     C                   MOVEL     *ON           $ESCI
046300040930     C                   MOVEL     *ON           $fine
046400040930     C                   END
046500040930     C*
046600940131     C                   ENDSR
046700940131     C************************************************************
046800940131     C* GESTIONE OPZIONI LISTA
046900940131     C************************************************************
047000940131     C     OPZS1         BEGSR
047100940201     C*
047200030113     C                   RESET                   tabds
047300100802     C                   MOVE      *all'0'       xtaopz
047400100802     C                   MOVE      S1OPZ         xtaopz
047500030113     C                   MOVE      *ZERO         xtaret
047600030113     C                   MOVE      *ZERO         xtaopr
047700100805     C                   MOVEl     *blank        xtakey
047800100805     c                   eval      Kcodice  = s1cod
047900100805     c                   eval      Kunbcli  = s1unb
048000100803     c                   eval      Ktmsg    = s1TMSG
048100100803     c                   eval      Kvers    = s1VERS
048200100803     c                   eval      Krels    = s1REL
048300100803     c                   eval      Kagenz   = s1AGEN
048400100803     c                   eval      Kassoc   = s1ASSO
048500100804     c                   eval      Kobblig  = h1OBBL
048600100804     c                   eval      Kdescr   = h1descr
048700100804     c                   eval      Kdesci   = h1desci
048800100805     c                   eval      oldcodice  = s1cod
048900100805     c                   eval      oldunbcli  = s1unb
049000100805     c                   eval      oldtmsg    = s1TMSG
049100100805     c                   eval      oldvers    = s1VERS
049200100805     c                   eval      oldrels    = s1REL
049300100805     c                   eval      oldagenz   = s1AGEN
049400100805     c                   eval      oldassoc   = s1ASSO
049500940715     C                   MOVE      *BLANKS       KPJBU
049600030113     C                   MOVEL     tabds         KPJBU
049700100804$004 C                   CALL      'TRTCTB0R1'                          99
049800940607     C                   PARM                    KPJBA
049900100804     c   99              exfmt     ALERTWIND
050000090225      *
050100030114     C                   MOVEL     KPJBU         tabds
050200940201     C*
050300940223     C* ritorno da PGM gestione
050400940223     C                   EXSR      GESRET
050500040930     C*
050600940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0507009402252    C     *IN99         IFEQ      *ON
050800940223     C                   SETON                                        40
0509009402252-   C                   ENDIF
051000040930     C*
051100940131     C                   ENDSR
051200940223     C/EJECT
051300940223     C************************************************************
051400940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
051500940223     C************************************************************
051600940223     C     GESRET        BEGSR
051700940223     C*
051800940223     C* modo di ritorno
051900940223     C*
0520009402231    C                   SELECT
052100940314    >C* << questi modi di utilizzo dei valori di ritorno dal
052200940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
052300940314    >C*    proposte, normalmente sempre valide, ma modificabili
052400940314    >C*    per situazioni particolari >>
052500940223     C* 1 = F3
052600030113    >C     xtaret        WHENEQ    '1'
052700940224     C                   MOVE      *ON           $ESCI
052800940223     C                   MOVE      *ON           $FINE
052900940223     C* 2 = F12
053000030113    >C     xtaret        WHENEQ    '2'
053100940223     C                   MOVE      *ON           $ESCI
053200940223     C*
0533009402231-   C                   ENDSL
053400940223     C*
053500940223     C* operazione eseguite dal pgm chiamato
053600940223     C*
0537009402231    C                   SELECT
053800940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
053900030113    >C     xtaopr        WHENEQ    '1'
054000940223     C                   MOVE      *ON           WINZS1
054100940223     C*
0542009402231-   C                   ENDSL
054300940223     C*
054400940223     C* funzione non eseguibile per errore :
054500940223     C*
0546009402231    C                   SELECT
054700940223     C* 1 = funzione richiamata chiusa in errore
054800940316    >C*  eventualmente gestire altri codici di errore
054900030113    >C     xtaerr        WHENEQ    '1'
055000940223     C                   MOVE      *ON           $ESCI
055100940223     C                   SETON                                        5299
055200940223     C*
0553009402231-   C                   ENDSL
055400940223     C*
055500940223     C                   ENDSR
055600940223     C/EJECT
055700940131     C************************************************************
055800940131     C* OPERAZIONI INIZIALI
055900940131     C************************************************************
056000940131     C     *INZSR        BEGSR
056100030113     C*
056200030113     C* Reperimento parametri
056300030113     C     *ENTRY        PLIST
056400030113     C                   PARM                    KPJBA
056500040930     C*
056600040930     C* Se chiamato x selezionare un codice
056700040930     c                   clear                   x1cod
056800030113     C*
056900030113     C* Variabili per gestione videate
057000030113     C                   MOVE      *BLANK        $GEST             2
057100030113     C                   MOVE      *BLANK        $FINE             1
057200030113     C                   MOVE      *BLANK        $INZS1            1
057300030113     C                   MOVE      *BLANK        $EFILE            1
057400030113     C                   MOVE      *BLANK        $ESCI             1
057500030113     C                   MOVE      *BLANK        $RCDOK            1
057600030113     C* Indici
057700030113     C                   Z-ADD     0             X                 3 0
057800030113     C                   Z-ADD     0             Y                 3 0
057900940506     C*
058000940506     C* Reperimento tasti di funzione
058100940127     C*
058200940117     C                   ENDSR
058300030113     C************************************************************
