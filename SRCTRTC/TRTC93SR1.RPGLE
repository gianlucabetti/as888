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
002000111220     FEDsts01L  IF   E           K DISK
002100131011     F                                     INFDS(STSDS)
002200111220     FTRTC93SD1 CF   E             WORKSTN
002300090225     F                                     SFILE(S0:S0NRR)
002400940201     F                                     INFDS(DSFMT)
002500940128     D*----------------------------------------------------*
002600940211     D* Passaggio Parametri
002700940211     D KPJBA         E DS
002800111220     D UTEDSE0F      E DS
002900100617      *
003000030113      *-------------
003100940325     D* Parametri in ricezione
003200030113     D  TABDS          DS
003300030113     D  XTAOPZ                 1      2
003400030113     D  XTARET                 3      3
003500030113     D  XTAOPR                 4      4
003600030113     D  XTAERR                 5      5
003700111220     D  XTAKEY1                6      8
003800111220     D  XTAKEY2                9     11
003900111220     D  XTACOD1               12     14
004000111220     D  XTACOD2               15     17
004100111220     D  XTAUNBcli             18     52
004200111221     D  XTARAGs               53     84
004300131011     D  XTANREC               85     93  0
004400131011     D*-------------
004500131011     d stsds           ds
004600131011     d  sts_nrr              397    400b 0
004700940211     D*-------------
004800940211     D DSFMT           DS           512
004900940506     D  $TASTO               369    369
005000940211     D  NRG                  370    370
005100940211     D  NCL                  371    371
005200940211     D  SFLNRR               378    379B 0
005300940207     D*-------------
005400940207     D* Nome PGM a video
005500940207     D                 DS
005600940207     D  PROGR                  1     10
005700940207     D  ASTER1                 1      1    INZ('*')
005800940207     D  SIGLA                  2      9
005900940207     D  ASTER2                10     10    INZ('*')
006000940127     D*-------------
006100940127     D* Reperimento nome PGM
006200940127     D STATUS         SDS           333
006300940127     D  DSPGM            *PROC
006400030113     D*-------------
006500111220     C* Variabili appoggio sempre presenti
006600111221$003 D S0NRR           S                   Like(C0rcd)
006700111221$003 D WSfl            S                   Like(C0nrr)
006800111221$003 D Wmax            S                   Like(C0rcd)
006900111221$003 D Wpag            S                   Like(C0rcd)
007000090225$003 D Winzs0          S                   Like($inzs0)
007100090225$003 D x0cod           S                   Like(s0cod)
007200111220$003 D x0cod1          S                   Like(s0cod1)
007300131011$003 D x0nrec          S                   Like(h0nrec)
007400111220     D*-------------
007500100326$003 D savC0pos        S                   Like(C0POS)
007600111220$003 D savC0pos1       S                   Like(C0POS1)
007700090225     D*-------------
007800100119     d dataiso         s               d   datfmt(*iso)
007900100119     d dataeur         s               d   datfmt(*eur)
008000111220     D*-------------
008100111221     d  savRAGS        s             35
008200111221     d  savUNBCLI      s                   Like(stsUNBCLI)
008300111220     d  savCODSTS      s                   Like(stsCODSTS)
008400111220     d  savSUBSTS      s                   Like(stsSUBSTS)
008500940207     D*-------------
008600940211     D* COSTANTI
008700940211     D*-------------
008800090225     D SFLPAG0         C                   CONST(11)
008900100617     D SFLPAG1         C                   CONST(12)
009000111220     D*-------------
009100940506     D* Tasti di funzione
009200940506     D F01             C                   CONST(X'31')
009300940506     D F02             C                   CONST(X'32')
009400940506     D F03             C                   CONST(X'33')
009500940506     D F04             C                   CONST(X'34')
009600940506     D F05             C                   CONST(X'35')
009700940506     D F06             C                   CONST(X'36')
009800940506     D F07             C                   CONST(X'37')
009900940506     D F08             C                   CONST(X'38')
010000940506     D F09             C                   CONST(X'39')
010100940506     D F10             C                   CONST(X'3A')
010200940506     D F11             C                   CONST(X'3B')
010300940506     D F12             C                   CONST(X'3C')
010400940506     D F13             C                   CONST(X'B1')
010500940506     D F14             C                   CONST(X'B2')
010600940506     D F15             C                   CONST(X'B3')
010700940506     D F16             C                   CONST(X'B4')
010800940506     D F17             C                   CONST(X'B5')
010900940506     D F18             C                   CONST(X'B6')
011000940506     D F19             C                   CONST(X'B7')
011100940506     D F20             C                   CONST(X'B8')
011200940506     D F21             C                   CONST(X'B9')
011300940506     D F22             C                   CONST(X'BA')
011400940506     D F23             C                   CONST(X'BB')
011500940506     D F24             C                   CONST(X'BC')
011600940506     D ENTER           C                   CONST(X'F1')
011700940506     D ROLDWN          C                   CONST(X'F4')
011800940506     D ROLLUP          C                   CONST(X'F5')
011900940127     C*----------------------------------------------------*
012000940127     C*                MAIN LINE PROGRAM
012100940127     C*----------------------------------------------------*
012200940223     C* inizializzazione variabili
012300940223     C                   EXSR      INZVAR
012400940223     C*
012500940223     C     $FINE         DOWEQ     *OFF
012600090225     C     $GEST         CASEQ     'S0'          GESS0
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
013900090225     C                   CLEAR                   S0OPZ
014000030113     C*
014100090225     C* Variabili per gestione videate
014200030113     C                   MOVE      *OFF          $FINE
014300030113     C                   MOVE      *OFF          $EFILE
014400030113     C                   MOVE      *OFF          $ESCI
014500030113     C                   MOVE      *OFF          $RCDOK
014600030113     C                   Z-ADD     0             $ULKS1            3 0
014700030113     C*
014800090225     C                   MOVE      *ON           $INZS0
014900030113     C* Variabili appoggio
015000030114     C                   Z-ADD     1             WPAG
015100030113     c*
015200100407     c* Video da cui partire
015300111220     C                   MOVE      'S0'          $GEST
015400100407     c*
015500030113     C                   ENDSR
015600090225     C************************************************************
015700090225     C* GESTIONE LISTA
015800090225     C************************************************************
015900090225     C     GESS0         BEGSR
016000090225     C*
016100090225     C* inizializzazione videata
016200090225     C     $INZS0        IFEQ      *ON
016300090225     C                   EXSR      INZS0
016400090225     C                   MOVE      *OFF          $INZS0
016500090225     C                   ENDIF
016600100325     C*
016700090225     C* emissione piede videata
016800090225     C                   WRITE     Z0
016900090225     C* Non ci sono records
017000090225     C     WMAX          IFEQ      0
017100090225     C                   WRITE     D0
017200090225     C                   Else
017300090225     C     Wsfl          IFgt      0
017400090225     C                   Z-ADD     wsfl          C0RCD
017500090225     C                   Else
017600090225     C     Wpag          IFgt      0
017700090225     C                   Z-ADD     wpag          C0RCD
017800090225     C                   EndIF
017900090225     C                   EndIF
018000090225     C                   ENDIF
018100090225     C*
018200090225     C*              *------------------*
018300090225     C                   EXFMT     C0
018400090225     C*              *------------------*
018500090225     C*
018600090225     C     C0NRR         IFNE      0
018700090225     C                   Z-ADD     C0NRR         WSFL
018800090225     C                   ENDIF
018900090225     C                   Z-ADD     SFLNRR        C0RCD
019000090225     C* Selezioni
0191000902251    C                   SELECT
019200090225     C* F3=Fine
019300090225     C     $TASTO        WHENEQ    F03
019400090225     C                   EXSR      F03S1
019500100121     C* F10=Immissione
019600100121     C     $TASTO        WHENEQ    F10
019700100121     C                   EXSR      F10S1
019800100325     C                   MOVE      'S0'          $GEST
0199000902251O   C                   OTHER
020000090225     C* CONTROLLO DATI
020100090225     C                   EXSR      CTRC0
020200090225     C     *IN99         IFEQ      *OFF
020300090225     C                   EXSR      CTRS0
020400090225     C                   END
0205000902251-   C                   ENDSL
020600090225     C*
020700090225     C                   ENDSR
020800090225     C/EJECT
020900090225     C************************************************************
021000090225     C* INIZIALIZZAZIONE LISTA
021100090225     C************************************************************
021200090225     C     INZS0         BEGSR
021300100407     C*
021400090225     C* pulizia SFL
021500090225     C                   SETOFF                                         3031
021600090225     C                   WRITE     C0
021700090225     C                   SETON                                          31
021800090225     C*
021900090225     C* CARICAMENTO SFL totale
022000090225     C                   Z-ADD     0             S0NRR
022100090225     C                   Z-ADD     1             C0RCD
022200090225     C                   Z-ADD     0             WMAX
022300131011     C                   Z-ADD     1             wsfl
022400090225     C*
022500090225     C*  Posizionamento all'inizio
022600111220     c                   movel(p)  savUNBCLI     stsUNBCLI
022700111220     c                   movel(p)  savUNBCLI     c0UNB
022800111221     c                   movel(p)  savRAGS       c0RAGS
022900111220     c                   movel(p)  c0pos         stsCODSTS
023000111220     c                   movel(p)  c0pos1        stsSUBSTS
023100111220     C*
023200100326     C                   IF        Posiziona = 'S'
023300111220     c     Key_STS       setll     edSTS01L
023400100326     C                   eval      Posiziona = *blank
023500100326     c                   else
023600111220     c     savUNBCLI     setll     edSTS01L
023700100326     c                   end
023800111220     C*
023900111220    >C                   EXSR      READ_STS
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
026100111220     c                   movel     stsCODSTS     s0cod
026200111220     c                   movel     stsSUBSTS     s0cod1
026300131011     c                   z-add     sts_nrr       h0Nrec
026400100617     C*
026500111220     c                   movel     stsCODSTSb    s0brt
026600111220     c                   movel     stsSUBSTSb    s0brt1
026700100617     c*
026800111220     c                   eval      s0des =  stsDEScri
026900090225     C*
027000090225     C                   ADD       1             S0NRR
027100090225     C                   ADD       1             Y
027200090225     C                   WRITE     S0
027300100119     C*
027400111220    >C                   EXSR      READ_STS
0275000902251-   C                   ENDDO
027600090225     C*
027700090225     C                   Z-ADD     S0NRR         WMAX                 30
027800090225     C*
027900090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
028000090225     C*
028100090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
028200090225     C                   MVR                     RESTO             3 0
028300090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0284000902251    C     RESTO         IFGT      0
028500090225     C                   ADD       1             C0RCD
0286000902251E   C                   ELSE
028700090225     C                   SUB       SFLPAG0       C0RCD
028800090225     C                   ADD       1             C0RCD
0289000902251-   C                   ENDIF
029000090225     C*
029100090225     C                   ENDSR
029200090225     C************************************************************
029300090225     C* LETTURA RCD
029400090225     C************************************************************
029500111220     C     READ_STS      BEGSR
029600090225     C*
029700090225     C                   MOVEL     *OFF          $EFILE
029800090225     C                   MOVEL     *OFF          $RCDOK
029900090225     C*
0300000902251    C     $EFILE        DOUEQ     *ON
030100090225     C     $RCDOK        OREQ      *ON
030200090225     C*
030300111220     c     stsUNBCLI     Reade     edSTS01L
030400111220     c                   if        %eof(edSTS01L)
030500090225     C                   MOVEL     *ON           $EFILE
030600090225     C                   MOVE      $EFILE        *IN33
030700090225     c                   else
030800100322      **
030900100322     c                   eXSr      Selez_RECord
031000100119     c                   end
031100090225     C*
0312000902251-   C                   ENDDO
031300090225     C*
031400090225     C                   ENDSR
031500090225     C************************************************************
031600090225     C* CONTROLLO TESTATA LISTA
031700090225     C************************************************************
031800100322     C     Selez_RECord  BEGSR
031900100407      *
032000100618     C                   MOVEL     *ON           $RCDOK
032100100322     C*
032200100322     C     End_filtro    ENDSR
032300100322     C************************************************************
032400100322     C* CONTROLLO TESTATA LISTA
032500100322     C************************************************************
032600100322     C     CTRC0         BEGSR
032700100322     C*
032800100326     C                   MOVE      *OFF          *IN99
032900100326     C*
033000100326     C                   clear                   posiziona         1
033100111220     c                   if        c0pos <> SAVc0pos or
033200111220     c                             c0pos1<> SAVc0pos1
033300100326     C                   move      'S'           posiziona         1
033400100326     c                   eval      SAVc0pos = c0pos
033500111220     c                   eval      SAVc0pos1= c0pos1
033600100326     c                   move      *ON           $INZS0
033700100326     c                   end
033800090225     C*
033900090225     C                   ENDSR
034000090225     C************************************************************
034100090225     C* CONTROLLO OPZIONI LISTA
034200090225     C************************************************************
034300090225     C     CTRS0         BEGSR
034400090225     C*
034500090225     C                   MOVEL     *OFF          $ESCI
034600090225     C                   SETOFF                                       99
034700090225     C                   clear                   S0OPZ
034800090225     c                   clear                   x0cod
034900111220     c                   clear                   x0cod1
035000131011     c                   clear                   x0nrec
035100090225     C*
035200090225     C* Leggo il sfl solo se ci sono rcd
0353000902251    C     WMAX          IFGT      0
035400090225     C                   READC     S0                                     21
035500090225     C*
035600090225     C* esce se fine sfl o errore che richiede l'uscita
0357000902252    C     *IN21         DOWEQ     *OFF
035800090225     C     $ESCI         ANDEQ     *OFF
035900090225     C                   Z-ADD     S0NRR         C0RCD
036000090225     C* ctrl su riga
036100090225     C                   EXSR      RECS0
036200090225     C* gestione opzioni
0363001112203    C     S0OPZ         IFNE      *blank
036400090225     C     *IN99         ANDEQ     *OFF
036500090225     C                   EXSR      OPZS0
0366000902253-   C                   ENDIF
036700111220      *
036800090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0369000902253    C     *IN99         IFEQ      *ON
037000090225     C     $ESCI         OREQ      *ON
037100090225     C                   MOVE      *ON           *IN32
037200090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
037300090225     C* la ripristinerò a conclusione del ciclo di READC
037400090225     C                   MOVE      *OFF          $INZS0
0375000902253-   C                   ENDIF
037600090225     C*
037700090225     C                   clear                   S0OPZ
037800090225     C                   UPDATE    S0
037900090225     C*
038000090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0381000902253    C     $ESCI         IFEQ      *OFF
038200090225     C                   READC     S0                                     21
038300111220     C*
038400090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0385000902254    C     *IN21         IFEQ      *ON
038600090225     C                   MOVE      WINZS0        $INZS0
038700090225     C* calcolo pagina a cui deve posizionarsi
038800090225     C                   EXSR      CLCPAG0
0389000902254-   C                   ENDIF
039000111220     C*
0391000902253-   C                   ENDIF
039200090225     C*
0393000902252-   C                   ENDDO
039400090225     C*
0395000902251-   C                   ENDIF
039600090225     C*
039700090225     C                   ENDSR
039800090225     C/EJECT
039900090225     C************************************************************
040000090225     C* CONTROLLO CAMPI I/O RIGA LISTA
040100090225     C************************************************************
040200090225     C     RECS0         BEGSR
040300090225     C*
040400090225     C* reset indicatori DSPATR
040500090225     C                   MOVE      *ALL'0'       IN4049           10
040600090225     C                   MOVEA     IN4049        *IN(40)
040700111220     C*
040800111220     C*  se richiamato x selezionare un codice, --> 11 = *on
040900111220     C*   non deve utilizzare le opzioni di modifica e viceversa
041000111220     c     *in11         ifeq      *on
0411001112203    C     S0OPZ         IFeq      '2'
0412001112203    C     S0OPZ         OReq      '3'
0413001112203    C     S0OPZ         OReq      '4'
041400111220     c                   seton                                        41  99
041500111220     C                   END
041600111220     C*
041700111220      * selezionato un codice da restituire al chiamante
0418001112203    C     S0OPZ         IFeq      '1'
041900111220     c                   movel(p)  s0cod         x0cod
042000111220     c                   movel(p)  s0cod1        x0cod1
042100131011     c                   z-add     h0nREC        x0nrec
042200111220     c                   clear                   kpjbu
042300111220     c                   eval      kpjbu = x0cod + x0cod1
042400111220     C                   MOVEL     *ON           $ESCI
042500111220     C                   MOVEL     *ON           $fine
042600111220     C                   END
042700111220     C                   END
042800111220     C*
042900111220     c     *in11         ifeq      *off
0430001112203    C     S0OPZ         ANDeq     '1'
043100111220     c                   seton                                        41  99
043200111220     C                   END
043300090225     C*
043400090225     C                   ENDSR
043500090225     C************************************************************
043600090225     C* GESTIONE OPZIONI LISTA
043700090225     C************************************************************
043800090225     C     OPZS0         BEGSR
043900090225     C*
044000090225      * selezionato un codice da restituire al chiamante
0441000902253    C     S0OPZ         IFeq      '1'
044200111220     c                   move      s0cod         codSTS            3
044300111220     c                   move      s0cod1        codSUB            3
044400090225     C                   MOVE      'S1'          $GEST
044500090225     C                   MOVEL     *ON           $ESCI
044600090225     C                   END
044700111220     C*
0448001112202    C     *IN11         IFEQ      *Off
0449001112202    C     *IN11         oreq      *On
0450001112202    C     s0opz         andeq     '5'
045100111220     C*
045200111220     C                   RESET                   tabds
045300111220     c                   move      *zero         xtaopz
045400111220     C                   MOVE      S0OPZ         xtaopz
045500111220     C                   MOVE      *ZERO         xtaret
045600111220     C                   MOVE      *ZERO         xtaopr
045700111220     C                   MOVEl(p)  s0COD         xtakey1
045800111220     C                   MOVEl(p)  s0COD1        xtakey2
045900111220     C                   MOVEl     *blank        xtacod1
046000111220     C                   MOVEl     *blank        xtacod2
046100111220     C                   MOVEl     savUNBcli     xtaUNBcli
046200111221     C                   MOVEl     savRAGs       xtaRAGs
046300131011     C                   MOVEl     h0Nrec        xtanrec
046400111220     C                   MOVE      *BLANKS       KPJBU
046500111220     C                   MOVEL     tabds         KPJBU
046600111220$004 C                   CALL      'TRTC93SR2'
046700111220     C                   PARM                    KPJBA
046800111220      *
046900111220     C                   MOVEL     KPJBU         tabds
047000111220     C*
047100111220     C* ritorno da PGM gestione
047200111220     C                   EXSR      GESRET
047300090225     C*
047400090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0475000902252    C     *IN99         IFEQ      *ON
047600090225     C                   SETON                                        40
0477000902252-   C                   ENDIF
047800111220     C*
0479001112202-   C                   ENDIF
048000090225     C*
048100090225     C                   ENDSR
048200111220     C*
048300090225     C/EJECT
048400940224     C************************************************************
048500940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
048600940224     C************************************************************
048700090225     C     CLCPAG0       BEGSR
048800940224     C* Input :
048900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
049000940224     C*          cursore
049100940224     C* - SFLPAG = numero rcd per pagina sfl
049200940224     C* Output :
049300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
049400940224     C*
049500090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
049600940224     C                   MVR                     RESTO             3 0
049700940224     C     RESTO         IFGT      0
049800940224     C                   ADD       1             PAGINE
049900940224     C                   ENDIF
050000090226     c                   if        pagine > 1
050100090225     C     PAGINE        MULT      SFLPAG0       WPAG
050200090226     C                   END
050300940224     C*
050400940224     C                   ENDSR
050500940309     C************************************************************
050600940309     C* GESTIONE F03 VIDEO S1
050700940309     C************************************************************
050800940309     C     F03S1         BEGSR
050900940309     C*
051000940309     C                   MOVE      *ON           $FINE
051100940325     C* fine programma
051200940309     C                   ENDSR
051300940309     C/EJECT
051400940309     C************************************************************
051500940309     C* GESTIONE F10 VIDEO S1
051600940314     c* AGGIUNTA RECORD
051700940309     C************************************************************
051800940309     C     F10S1         BEGSR
051900940309     C*
052000030113     C                   RESET                   tabds
052100030113     C                   MOVEL     '01'          xtaopz
052200030113     C                   MOVE      *ZERO         xtaret
052300030113     C                   MOVE      *ZERO         xtaopr
052400100409     C                   MOVEl     *blank        xtakey1
052500100409     C                   MOVEl     *blank        xtakey2
052600111220     C                   MOVEl     *blank        xtacod1
052700111220     C                   MOVEl     *blank        xtacod2
052800111220     C                   MOVEl     savUNBcli     xtaUNBcli
052900111221     C                   MOVEl     savRAGs       xtaRAGs
053000131011     C                   z-add     0             xtaNREC
053100030113     C                   MOVE      *BLANKS       KPJBU
053200030113     C                   MOVEL     tabds         KPJBU
053300111220$004 C                   CALL      'TRTC93SR2'
053400030113     C                   PARM                    KPJBA
053500090225      *
053600030114     C                   MOVEL     KPJBU         tabds
053700030113      *
053800940309     C* ritorno da PGM gestione
053900940309     C                   EXSR      GESRET
054000090226      *
054100111221     C     WINZS0        IFEQ      *ON
054200111221     C                   MOVE      *ON           $INZS0
054300090226     C* carico sempre la 1a pagina
054400090226     C                   Z-ADD     1             WPAG
054500940309     C                   ENDIF
054600940309     C*
054700940309     C                   ENDSR
054800940223     C************************************************************
054900940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
055000940223     C************************************************************
055100940223     C     GESRET        BEGSR
055200940223     C*
055300940223     C* modo di ritorno
055400940223     C*
0555009402231    C                   SELECT
055600940314    >C* << questi modi di utilizzo dei valori di ritorno dal
055700940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
055800940314    >C*    proposte, normalmente sempre valide, ma modificabili
055900940314    >C*    per situazioni particolari >>
056000940223     C* 1 = F3
056100030113    >C     xtaret        WHENEQ    '1'
056200940224     C                   MOVE      *ON           $ESCI
056300940223     C                   MOVE      *ON           $FINE
056400940223     C* 2 = F12
056500030113    >C     xtaret        WHENEQ    '2'
056600940223     C                   MOVE      *ON           $ESCI
056700940223     C*
0568009402231-   C                   ENDSL
056900940223     C*
057000940223     C* operazione eseguite dal pgm chiamato
057100940223     C*
0572009402231    C                   SELECT
057300940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
057400030113    >C     xtaopr        WHENEQ    '1'
057500111221     C                   MOVE      *ON           WINZS0
057600940223     C*
0577009402231-   C                   ENDSL
057800940223     C*
057900940223     C* funzione non eseguibile per errore :
058000940223     C*
0581009402231    C                   SELECT
058200940223     C* 1 = funzione richiamata chiusa in errore
058300940316    >C*  eventualmente gestire altri codici di errore
058400030113    >C     xtaerr        WHENEQ    '1'
058500940223     C                   MOVE      *ON           $ESCI
058600940223     C                   SETON                                        5299
058700940223     C*
0588009402231-   C                   ENDSL
058900940223     C*
059000940223     C                   ENDSR
059100940223     C/EJECT
059200940131     C************************************************************
059300940131     C* OPERAZIONI INIZIALI
059400940131     C************************************************************
059500940131     C     *INZSR        BEGSR
059600030113     C*
059700030113     C* Reperimento parametri
059800030113     C     *ENTRY        PLIST
059900030113     C                   PARM                    KPJBA
060000111220     C*
060100111220     C                   eval        TABDS = KPJBU
060200111220     c                   eval      savUNBCLI = xtaUNBCLI
060300111221     c                   eval      savRAGS   = xtaRAGS
060400111221     c     xtaopr        comp      '01'                                   11
060500040930     C*
060600111220     c     key_STS       Klist
060700111220     c                   kfld                    stsUNBCLI
060800111220     c                   kfld                    stsCODSTS
060900111220     c                   kfld                    stsSUBSTS
061000100617      *
061100100617     C* RECUPERO DATI DELL'UTENTE
061200100617     C                   Z-ADD     1             CODUT             1 0
061300100617     C                   CALL      'XPARUT'
061400100617     C                   PARM                    UTEDSE0F
061500100617     C                   MOVEL     RAGUT         RSUT             20
061600100326     C*
061700040930     C* Se chiamato x selezionare un codice
061800111221     c                   clear                   x0cod
061900111221     c                   clear                   x0cod1
062000131011     c                   clear                   x0nrec
062100030113     C*
062200030113     C* Variabili per gestione videate
062300030113     C                   MOVE      *BLANK        $GEST             2
062400030113     C                   MOVE      *BLANK        $FINE             1
062500090225     C                   MOVE      *BLANK        $INZS0            1
062600030113     C                   MOVE      *BLANK        $EFILE            1
062700030113     C                   MOVE      *BLANK        $ESCI             1
062800030113     C                   MOVE      *BLANK        $RCDOK            1
062900030113     C* Indici
063000030113     C                   Z-ADD     0             X                 3 0
063100030113     C                   Z-ADD     0             Y                 3 0
063200940506     C*
063300940506     C* Reperimento tasti di funzione
063400100326     C*
063500100326     C                   clear                   c0pos
063600111220     C                   clear                   c0pos1
063700100326     C                   clear                   SAVc0pos
063800111220     C                   clear                   SAVc0pos1
063900940127     C*
064000940117     C                   ENDSR
064100100319     C************************************************************
