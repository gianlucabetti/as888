000100141105     H DECEDIT('0,') DATEDIT(*YMD.)
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
002000141017     FEDPFL01L  IF   E           K DISK
002100141017     F                                     INFDS(PFLDS)
002200141021     FEDTAB01L  IF   E           K DISK
002300141017     FTRTC9PLD1 CF   E             WORKSTN
002400090225     F                                     SFILE(S0:S0NRR)
002500940201     F                                     INFDS(DSFMT)
002600940128     D*----------------------------------------------------*
002700940211     D* Passaggio Parametri
002800940211     D KPJBA         E DS
002900111220     D UTEDSE0F      E DS
003000141021     D EDIDSPT       E DS
003100030113      *-------------
003200141017     D* Parametri in ricezione
003300141017     D  TABDS        E DS                  EXTNAME(TRTC9PLDS)
003400141017     C*----------------------------------------------------*
003500141017     d PFLds           ds
003600141017     d  PFL_nrr              397    400b 0
003700940211     D*-------------
003800940211     D DSFMT           DS           512
003900940506     D  $TASTO               369    369
004000940211     D  NRG                  370    370
004100940211     D  NCL                  371    371
004200940211     D  SFLNRR               378    379B 0
004300940207     D*-------------
004400940207     D* Nome PGM a video
004500940207     D                 DS
004600940207     D  PROGR                  1     10
004700940207     D  ASTER1                 1      1    INZ('*')
004800940207     D  SIGLA                  2      9
004900940207     D  ASTER2                10     10    INZ('*')
005000940127     D*-------------
005100940127     D* Reperimento nome PGM
005200940127     D STATUS         SDS           333
005300940127     D  DSPGM            *PROC
005400030113     D*-------------
005500111220     C* Variabili appoggio sempre presenti
005600111221$003 D S0NRR           S                   Like(C0rcd)
005700111221$003 D WSfl            S                   Like(C0nrr)
005800111221$003 D Wmax            S                   Like(C0rcd)
005900111221$003 D Wpag            S                   Like(C0rcd)
006000090225$003 D Winzs0          S                   Like($inzs0)
006100111220     D*-------------
006200141021$003 D savLna          S                   Like(XTAKLNA)
006300141021$003 D savLuogo        S                   Like(XTAKLUO)
006400141021$003 D savData         S                   Like(XTAKDAT)
006500141021     D*-------------
006600100326$003 D savC0pos        S                   Like(C0POS)
006700090225     D*-------------
006800100119     d dataiso         s               d   datfmt(*iso)
006900100119     d dataeur         s               d   datfmt(*eur)
007000940207     D*-------------
007100940211     D* COSTANTI
007200940211     D*-------------
007300090225     D SFLPAG0         C                   CONST(11)
007400100617     D SFLPAG1         C                   CONST(12)
007500111220     D*-------------
007600940506     D* Tasti di funzione
007700940506     D F01             C                   CONST(X'31')
007800940506     D F02             C                   CONST(X'32')
007900940506     D F03             C                   CONST(X'33')
008000940506     D F04             C                   CONST(X'34')
008100940506     D F05             C                   CONST(X'35')
008200940506     D F06             C                   CONST(X'36')
008300940506     D F07             C                   CONST(X'37')
008400940506     D F08             C                   CONST(X'38')
008500940506     D F09             C                   CONST(X'39')
008600940506     D F10             C                   CONST(X'3A')
008700940506     D F11             C                   CONST(X'3B')
008800940506     D F12             C                   CONST(X'3C')
008900940506     D F13             C                   CONST(X'B1')
009000940506     D F14             C                   CONST(X'B2')
009100940506     D F15             C                   CONST(X'B3')
009200940506     D F16             C                   CONST(X'B4')
009300940506     D F17             C                   CONST(X'B5')
009400940506     D F18             C                   CONST(X'B6')
009500940506     D F19             C                   CONST(X'B7')
009600940506     D F20             C                   CONST(X'B8')
009700940506     D F21             C                   CONST(X'B9')
009800940506     D F22             C                   CONST(X'BA')
009900940506     D F23             C                   CONST(X'BB')
010000940506     D F24             C                   CONST(X'BC')
010100940506     D ENTER           C                   CONST(X'F1')
010200940506     D ROLDWN          C                   CONST(X'F4')
010300940506     D ROLLUP          C                   CONST(X'F5')
010400940127     C*----------------------------------------------------*
010500940127     C*                MAIN LINE PROGRAM
010600940127     C*----------------------------------------------------*
010700940223     C* inizializzazione variabili
010800940223     C                   EXSR      INZVAR
010900940223     C*
011000940223     C     $FINE         DOWEQ     *OFF
011100090225     C     $GEST         CASEQ     'S0'          GESS0
011200940117     C                   END
011300940117     C                   END
011400940325     C* fine programma
011500940325     C                   SETON                                            LR
011600030113     C************************************************************
011700030113     C* INIZIALIZZAZIONE VARIABILI
011800030113     C************************************************************
011900030113     C     INZVAR        BEGSR
012000030113     C*
012100030113     C* Pulizia campi e indicatori
012200030113     C                   MOVE      *ALL'0'       IN4049           10
012300030113     C                   MOVEA     IN4049        *IN(40)
012400090225     C                   CLEAR                   S0OPZ
012500030113     C*
012600090225     C* Variabili per gestione videate
012700030113     C                   MOVE      *OFF          $FINE
012800030113     C                   MOVE      *OFF          $EFILE
012900030113     C                   MOVE      *OFF          $ESCI
013000030113     C                   MOVE      *OFF          $RCDOK
013100030113     C                   Z-ADD     0             $ULKS1            3 0
013200030113     C*
013300090225     C                   MOVE      *ON           $INZS0
013400030113     C* Variabili appoggio
013500030114     C                   Z-ADD     1             WPAG
013600100407     c* Video da cui partire
013700111220     C                   MOVE      'S0'          $GEST
013800100407     c*
013900030113     C                   ENDSR
014000090225     C************************************************************
014100090225     C* GESTIONE LISTA
014200090225     C************************************************************
014300090225     C     GESS0         BEGSR
014400090225     C*
014500090225     C* inizializzazione videata
014600090225     C     $INZS0        IFEQ      *ON
014700090225     C                   EXSR      INZS0
014800090225     C                   MOVE      *OFF          $INZS0
014900090225     C                   ENDIF
015000100325     C*
015100090225     C* emissione piede videata
015200090225     C                   WRITE     Z0
015300090225     C* Non ci sono records
015400090225     C     WMAX          IFEQ      0
015500090225     C                   WRITE     D0
015600090225     C                   Else
015700090225     C     Wsfl          IFgt      0
015800090225     C                   Z-ADD     wsfl          C0RCD
015900090225     C                   Else
016000090225     C     Wpag          IFgt      0
016100090225     C                   Z-ADD     wpag          C0RCD
016200090225     C                   EndIF
016300090225     C                   EndIF
016400090225     C                   ENDIF
016500090225     C*
016600090225     C*              *------------------*
016700090225     C                   EXFMT     C0
016800090225     C*              *------------------*
016900090225     C*
017000090225     C     C0NRR         IFNE      0
017100090225     C                   Z-ADD     C0NRR         WSFL
017200090225     C                   ENDIF
017300090225     C                   Z-ADD     SFLNRR        C0RCD
017400090225     C* Selezioni
0175000902251    C                   SELECT
017600090225     C* F3=Fine
017700090225     C     $TASTO        WHENEQ    F03
017800090225     C                   EXSR      F03S1
017900141105     C* F13=tutte le PList o quelle attive ad oggi
018000141105     C     $TASTO        WHENEQ    F13
018100141105     C                   EXSR      F13S1
018200100121     C* F10=Immissione
018300141031     C     $TASTO        WHENEQ    F10
018400141031     c     *in11         andeq     *Off
018500100121     C                   EXSR      F10S1
018600100325     C                   MOVE      'S0'          $GEST
0187000902251O   C                   OTHER
018800090225     C* CONTROLLO DATI
018900090225     C                   EXSR      CTRC0
019000090225     C     *IN99         IFEQ      *OFF
019100090225     C                   EXSR      CTRS0
019200090225     C                   END
0193000902251-   C                   ENDSL
019400090225     C*
019500090225     C                   ENDSR
019600090225     C/EJECT
019700090225     C************************************************************
019800090225     C* INIZIALIZZAZIONE LISTA
019900090225     C************************************************************
020000090225     C     INZS0         BEGSR
020100100407     C*
020200090225     C* pulizia SFL
020300090225     C                   SETOFF                                         3031
020400090225     C                   WRITE     C0
020500090225     C                   SETON                                          31
020600090225     C*
020700090225     C* CARICAMENTO SFL totale
020800090225     C                   Z-ADD     0             S0NRR
020900090225     C                   Z-ADD     1             C0RCD
021000090225     C                   Z-ADD     0             WMAX
021100131011     C                   Z-ADD     1             wsfl
021200090225     C*
021300141021     C* decodifica la Linea del PARTNER
021400141021     c                   movel     'PT'          tab_PT            2
021500141021     c     tab_PT        setll     edtab01l
021600141021     c     tab_PT        reade     edtab01l
021700141021     c                   dow       not %Eof(edtab01l)
021800141021     c                   movel     tabUNI        edidspt
021900141021     c                   if        §PTLNP = XTAKLNA
022000141021     c                   eval      c0PARTNER = tabKEY
022100141021     c                   leave
022200141021     c                   end
022300141021     c     tab_PT        reade     edtab01l
022400141021     c                   end
022500141021     C*
022600090225     C*  Posizionamento all'inizio
022700141021     c                   z-add     savLna        pflLineaA
022800141021     c                   z-add     savLna        c0LNA
022900141021     c                   movel(p)  savLuogo      c0Luogo
023000141021     c                   movel(p)  c0pos         pflLUOGO
023100111220     C*
023200100326     C                   IF        Posiziona = 'S'
023300141020     c     Key_PFL       setll     edPFL01L
023400100326     C                   eval      Posiziona = *blank
023500100326     c                   else
023600141021     c     savLNA        setll     edPFL01L
023700100326     c                   end
023800111220     C*
023900141021    >C                   EXSR      READ_PFL
024000090225     C* Carico il SFL
024100090225     C                   EXSR      ROLS0
024200090225     C*
024300090225     c                   if        xtaopr <> '1'
024400090225     C                   Z-ADD     1             WPAG
024500090225     c                   end
024600090225     C*
024700090225     C                   ENDSR
024800090225     C************************************************************
024900090225     C* CARICAMENTO PAGINA LISTA
025000090225     C************************************************************
025100090225     C     ROLS0         BEGSR
025200090225     C*
025300090225     C                   SETOFF                                       32
025400090225     C                   Z-ADD     0             Y
025500090225     C                   Z-ADD     WMAX          S0NRR
025600100119     C*
025700090225     C* Leggo dal file i segmenti presenti nell'archivio
0258000902251    C     $EFILE        DOWEQ     *OFF
025900100617     C*
026000111220     c                   clear                   s0opz
026100141021     c                   movel     pflLINEAA     h0Lna
026200141021     c                   movel     pflLUOGO      s0Luogo
026300141021     c                   movel     pflDESCR      s0Des
026400141021     c                   movel     pflCODPFL     s0Plst
026500141021     c                   movel     pflDTDEC      h0DtDec
026600141021     c                   movel     pflDTSCA      h0DtSca
026700141021     c                   move      pflDTDEC      dataiso
026800141021     c                   move      dataiso       dataeur
026900141021     c                   move      dataeur       s0DtDec
027000141021     c                   move      pflDTSCA      dataiso
027100141021     c                   move      dataiso       dataeur
027200141021     c                   move      dataeur       s0DtSca
027300141021     C*
027400141017     c                   z-add     PFL_nrr       h0Nrec
027500090225     C*
027600090225     C                   ADD       1             S0NRR
027700090225     C                   ADD       1             Y
027800090225     C                   WRITE     S0
027900100119     C*
028000141021    >C                   EXSR      READ_PFL
0281000902251-   C                   ENDDO
028200090225     C*
028300090225     C                   Z-ADD     S0NRR         WMAX                 30
028400090225     C*
028500090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
028600090225     C*
028700090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
028800090225     C                   MVR                     RESTO             3 0
028900090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0290000902251    C     RESTO         IFGT      0
029100090225     C                   ADD       1             C0RCD
0292000902251E   C                   ELSE
029300090225     C                   SUB       SFLPAG0       C0RCD
029400090225     C                   ADD       1             C0RCD
0295000902251-   C                   ENDIF
029600090225     C*
029700090225     C                   ENDSR
029800090225     C************************************************************
029900090225     C* LETTURA RCD
030000090225     C************************************************************
030100141021     C     READ_PFL      BEGSR
030200090225     C*
030300090225     C                   MOVEL     *OFF          $EFILE
030400090225     C                   MOVEL     *OFF          $RCDOK
030500090225     C*
0306000902251    C     $EFILE        DOUEQ     *ON
030700090225     C     $RCDOK        OREQ      *ON
030800090225     C*
030900141021     c     PFLLINEAA     Reade     edPFL01L
031000141021     c                   if        %eof(edPFL01L)
031100090225     C                   MOVEL     *ON           $EFILE
031200090225     C                   MOVE      $EFILE        *IN33
031300090225     c                   else
031400100322      **
031500100322     c                   eXSr      Selez_RECord
031600100119     c                   end
031700090225     C*
0318000902251-   C                   ENDDO
031900090225     C*
032000090225     C                   ENDSR
032100090225     C************************************************************
032200090225     C* CONTROLLO TESTATA LISTA
032300090225     C************************************************************
032400100322     C     Selez_RECord  BEGSR
032500100407      *
032600100618     C                   MOVEL     *ON           $RCDOK
032700100322     C*
032800141021     C* Controlli sul record
032900141021     C*      dei records in vigore alla data
033000141021     c                   if        savDATA > 0
033100141021     C*
033200141021     c                   if        PFLDTDEC > savDATA or
033300141021     c                             PFLDTSCA < savDATA
033400141021     C                   MOVEL     *OFF          $RCDOK
033500141021     c                   end
033600141021     C*
033700141021     c                   end
033800141021     C*
033900100322     C     End_filtro    ENDSR
034000100322     C************************************************************
034100100322     C* CONTROLLO TESTATA LISTA
034200100322     C************************************************************
034300100322     C     CTRC0         BEGSR
034400100322     C*
034500100326     C                   MOVE      *OFF          *IN99
034600100326     C*
034700100326     C                   clear                   posiziona         1
034800141021     c                   if        c0pos <> SAVc0pos
034900100326     C                   move      'S'           posiziona         1
035000100326     c                   eval      SAVc0pos = c0pos
035100100326     c                   move      *ON           $INZS0
035200100326     c                   end
035300090225     C*
035400090225     C                   ENDSR
035500090225     C************************************************************
035600090225     C* CONTROLLO OPZIONI LISTA
035700090225     C************************************************************
035800090225     C     CTRS0         BEGSR
035900090225     C*
036000090225     C                   MOVEL     *OFF          $ESCI
036100090225     C                   SETOFF                                       99
036200090225     C                   clear                   S0OPZ
036300090225     C*
036400090225     C* Leggo il sfl solo se ci sono rcd
0365000902251    C     WMAX          IFGT      0
036600090225     C                   READC     S0                                     21
036700090225     C*
036800090225     C* esce se fine sfl o errore che richiede l'uscita
0369000902252    C     *IN21         DOWEQ     *OFF
037000090225     C     $ESCI         ANDEQ     *OFF
037100090225     C                   Z-ADD     S0NRR         C0RCD
037200090225     C* ctrl su riga
037300090225     C                   EXSR      RECS0
037400090225     C* gestione opzioni
0375001112203    C     S0OPZ         IFNE      *blank
037600090225     C     *IN99         ANDEQ     *OFF
037700090225     C                   EXSR      OPZS0
0378000902253-   C                   ENDIF
037900111220      *
038000090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0381000902253    C     *IN99         IFEQ      *ON
038200090225     C     $ESCI         OREQ      *ON
038300090225     C                   MOVE      *ON           *IN32
038400090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038500090225     C* la ripristinerò a conclusione del ciclo di READC
038600090225     C                   MOVE      *OFF          $INZS0
0387000902253-   C                   ENDIF
038800090225     C*
038900090225     C                   clear                   S0OPZ
039000090225     C                   UPDATE    S0
039100090225     C*
039200090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0393000902253    C     $ESCI         IFEQ      *OFF
039400090225     C                   READC     S0                                     21
039500111220     C*
039600090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0397000902254    C     *IN21         IFEQ      *ON
039800090225     C                   MOVE      WINZS0        $INZS0
039900090225     C* calcolo pagina a cui deve posizionarsi
040000090225     C                   EXSR      CLCPAG0
0401000902254-   C                   ENDIF
040200111220     C*
0403000902253-   C                   ENDIF
040400090225     C*
0405000902252-   C                   ENDDO
040600090225     C*
0407000902251-   C                   ENDIF
040800090225     C*
040900090225     C                   ENDSR
041000090225     C/EJECT
041100090225     C************************************************************
041200090225     C* CONTROLLO CAMPI I/O RIGA LISTA
041300090225     C************************************************************
041400090225     C     RECS0         BEGSR
041500090225     C*
041600090225     C* reset indicatori DSPATR
041700090225     C                   MOVE      *ALL'0'       IN4049           10
041800090225     C                   MOVEA     IN4049        *IN(40)
041900111220     C*
042000111220     C*  se richiamato x selezionare un codice, --> 11 = *on
042100111220     C*   non deve utilizzare le opzioni di modifica e viceversa
042200111220     c     *in11         ifeq      *on
0423001112203    C     S0OPZ         IFeq      '2'
0424001112203    C     S0OPZ         OReq      '3'
0425001112203    C     S0OPZ         OReq      '4'
042600111220     c                   seton                                        41  99
042700111220     C                   END
042800141021     C                   END
042900111220     C*
043000111220     c     *in11         ifeq      *off
0431001112203    C     S0OPZ         ANDeq     '1'
043200111220     c                   seton                                        41  99
043300111220     C                   END
043400090225     C*
043500090225     C                   ENDSR
043600090225     C************************************************************
043700090225     C* GESTIONE OPZIONI LISTA
043800090225     C************************************************************
043900090225     C     OPZS0         BEGSR
044000090225     C*
044100141021      * selezionato un codice da restituire al chiamante
0442001410213    C     S0OPZ         IFeq      '1'
044300141021     c                   clear                   kpjbu
044400141021     c                   eval      XTAKLNA = h0Lna
044500141021     c                   eval      XTAKLUO = s0Luogo
044600141021     c                   eval      XTAKPFL = s0Plst
044700141021     c                   eval      XTAKDAT = h0DtDec
044800141021     c                   eval      XTANREC = h0nREC
044900141021     c                   eval      XTADESCR= s0des
045000141021     c                   eval      kpjbu = TABDS
045100141021     C                   MOVE      'S1'          $GEST
045200141021     C                   MOVEL     *ON           $ESCI
045300141021     C                   MOVEL     *ON           $fine
045400141021     C                   END
045500111220     C*
0456001112202    C     *IN11         IFEQ      *Off
0457001112202    C     *IN11         oreq      *On
0458001112202    C     s0opz         andeq     '5'
045900111220     C*
046000111220     C                   RESET                   tabds
046100111220     c                   move      *zero         xtaopz
046200111220     C                   MOVE      S0OPZ         xtaopz
046300111220     C                   MOVE      *ZERO         xtaret
046400111220     C                   MOVE      *ZERO         xtaopr
046500141021     C                   z-add     h0Lna         xtaKLNA
046600141021     C                   MOVEl(p)  s0Luogo       xtaKLUO
046700141021     C                   MOVEl(p)  s0PLST        xtaKPFL
046800141021     c                   eval      XTADESCR= s0des
046900141021     C                   z-add     h0DtDec       xtaKDAT
047000131011     C                   MOVEl     h0Nrec        xtanrec
047100111220     C                   MOVE      *BLANKS       KPJBU
047200111220     C                   MOVEL     tabds         KPJBU
047300141021$004 C                   CALL      'TRTC9PLR2'
047400111220     C                   PARM                    KPJBA
047500111220      *
047600111220     C                   MOVEL     KPJBU         tabds
047700111220     C*
047800111220     C* ritorno da PGM gestione
047900111220     C                   EXSR      GESRET
048000090225     C*
048100090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0482000902252    C     *IN99         IFEQ      *ON
048300090225     C                   SETON                                        40
0484000902252-   C                   ENDIF
048500111220     C*
0486001112202-   C                   ENDIF
048700090225     C*
048800090225     C                   ENDSR
048900111220     C*
049000090225     C/EJECT
049100940224     C************************************************************
049200940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
049300940224     C************************************************************
049400090225     C     CLCPAG0       BEGSR
049500940224     C* Input :
049600940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
049700940224     C*          cursore
049800940224     C* - SFLPAG = numero rcd per pagina sfl
049900940224     C* Output :
050000940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
050100940224     C*
050200090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
050300940224     C                   MVR                     RESTO             3 0
050400940224     C     RESTO         IFGT      0
050500940224     C                   ADD       1             PAGINE
050600940224     C                   ENDIF
050700090226     c                   if        pagine > 1
050800090225     C     PAGINE        MULT      SFLPAG0       WPAG
050900090226     C                   END
051000940224     C*
051100940224     C                   ENDSR
051200940309     C************************************************************
051300940309     C* GESTIONE F03 VIDEO S1
051400940309     C************************************************************
051500940309     C     F03S1         BEGSR
051600940309     C*
051700940309     C                   MOVE      *ON           $FINE
051800940325     C* fine programma
051900940309     C                   ENDSR
052000940309     C/EJECT
052100141105     C************************************************************
052200141105     C* GESTIONE F13 VIDEO S1
052300141105     C************************************************************
052400141105     C     F13S1         BEGSR
052500141105     C*
052600141105     c                   move      *ON           $INZS0
052700141105     C                   clear                   c0pos
052800141105     C                   clear                   SAVc0pos
052900141105     c                   if        savDATA > 0
053000141105     c                   seton                                        13
053100141105     c                   z-add     0             savDATA
053200141105     c                   else
053300141105     c                   setoff                                       13
053400141105     c                   move      *date         savDATA
053500141105     c                   end
053600141105     C* fine programma
053700141105     C                   ENDSR
053800141105     C/EJECT
053900940309     C************************************************************
054000940309     C* GESTIONE F10 VIDEO S1
054100940314     c* AGGIUNTA RECORD
054200940309     C************************************************************
054300940309     C     F10S1         BEGSR
054400940309     C*
054500030113     C                   RESET                   tabds
054600141021     C                   MOVEL     '10'          xtaopz
054700030113     C                   MOVE      *ZERO         xtaret
054800030113     C                   MOVE      *ZERO         xtaopr
054900141021     C                   z-add     0             XTAKDAT
055000141021     C                   MOVEl     *blank        XTAKPFL
055100141021     C                   MOVEl     savLNA        XTAKLNA
055200141021     C                   MOVEl     *blank        XTAKLUO
055300141021     C                   MOVEl     *blank        XTAdescr
055400131011     C                   z-add     0             xtaNREC
055500030113     C                   MOVE      *BLANKS       KPJBU
055600030113     C                   MOVEL     tabds         KPJBU
055700141021$004 C                   CALL      'TRTC9PLR2'
055800030113     C                   PARM                    KPJBA
055900090225      *
056000030114     C                   MOVEL     KPJBU         tabds
056100030113      *
056200940309     C* ritorno da PGM gestione
056300940309     C                   EXSR      GESRET
056400090226      *
056500111221     C     WINZS0        IFEQ      *ON
056600111221     C                   MOVE      *ON           $INZS0
056700090226     C* carico sempre la 1a pagina
056800090226     C                   Z-ADD     1             WPAG
056900940309     C                   ENDIF
057000940309     C*
057100940309     C                   ENDSR
057200940223     C************************************************************
057300940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
057400940223     C************************************************************
057500940223     C     GESRET        BEGSR
057600940223     C*
057700940223     C* modo di ritorno
057800940223     C*
0579009402231    C                   SELECT
058000940314    >C* << questi modi di utilizzo dei valori di ritorno dal
058100940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
058200940314    >C*    proposte, normalmente sempre valide, ma modificabili
058300940314    >C*    per situazioni particolari >>
058400940223     C* 1 = F3
058500030113    >C     xtaret        WHENEQ    '1'
058600940224     C                   MOVE      *ON           $ESCI
058700940223     C                   MOVE      *ON           $FINE
058800940223     C* 2 = F12
058900030113    >C     xtaret        WHENEQ    '2'
059000940223     C                   MOVE      *ON           $ESCI
059100940223     C*
0592009402231-   C                   ENDSL
059300940223     C*
059400940223     C* operazione eseguite dal pgm chiamato
059500940223     C*
0596009402231    C                   SELECT
059700940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
059800030113    >C     xtaopr        WHENEQ    '1'
059900111221     C                   MOVE      *ON           WINZS0
060000940223     C*
0601009402231-   C                   ENDSL
060200940223     C*
060300940223     C* funzione non eseguibile per errore :
060400940223     C*
0605009402231    C                   SELECT
060600940223     C* 1 = funzione richiamata chiusa in errore
060700940316    >C*  eventualmente gestire altri codici di errore
060800030113    >C     xtaerr        WHENEQ    '1'
060900940223     C                   MOVE      *ON           $ESCI
061000940223     C                   SETON                                        5299
061100940223     C*
0612009402231-   C                   ENDSL
061300940223     C*
061400940223     C                   ENDSR
061500940223     C/EJECT
061600940131     C************************************************************
061700940131     C* OPERAZIONI INIZIALI
061800940131     C************************************************************
061900940131     C     *INZSR        BEGSR
062000030113     C*
062100030113     C* Reperimento parametri
062200030113     C     *ENTRY        PLIST
062300030113     C                   PARM                    KPJBA
062400111220     C*
062500111220     C                   eval        TABDS = KPJBU
062600141021     c                   eval      savLNA    = XTAKLNA
062700141021     c                   eval      savLUOGO  = XTAKLUO
062800141021     c                   eval      savDATA   = XTAKDAT
062900141105     c     savDATA       comp      0                                      13
063000141105     c     xtaopZ        comp      '01'                                   11
063100040930     C*
063200141020     c     key_PFL       Klist
063300141020     c                   kfld                    PFLLINEAA
063400141020     c                   kfld                    PFLLUOGO
063500100617      *
063600100617     C* RECUPERO DATI DELL'UTENTE
063700100617     C                   Z-ADD     1             CODUT             1 0
063800100617     C                   CALL      'XPARUT'
063900100617     C                   PARM                    UTEDSE0F
064000100617     C                   MOVEL     RAGUT         RSUT             20
064100030113     C*
064200030113     C* Variabili per gestione videate
064300030113     C                   MOVE      *BLANK        $GEST             2
064400030113     C                   MOVE      *BLANK        $FINE             1
064500090225     C                   MOVE      *BLANK        $INZS0            1
064600030113     C                   MOVE      *BLANK        $EFILE            1
064700030113     C                   MOVE      *BLANK        $ESCI             1
064800030113     C                   MOVE      *BLANK        $RCDOK            1
064900030113     C* Indici
065000030113     C                   Z-ADD     0             X                 3 0
065100030113     C                   Z-ADD     0             Y                 3 0
065200940506     C*
065300940506     C* Reperimento tasti di funzione
065400100326     C*
065500100326     C                   clear                   c0pos
065600100326     C                   clear                   SAVc0pos
065700940127     C*
065800940117     C                   ENDSR
065900100319     C************************************************************
