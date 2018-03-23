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
006200111221$003 D S0NRR           S                   Like(C0rcd)
006300111221$003 D WSfl            S                   Like(C0nrr)
006400111221$003 D Wmax            S                   Like(C0rcd)
006500111221$003 D Wpag            S                   Like(C0rcd)
006600090225$003 D Winzs0          S                   Like($inzs0)
006700090225$003 D x0cod           S                   Like(s0cod)
006800111220$003 D x0cod1          S                   Like(s0cod1)
006900111220     D*-------------
007000100326$003 D savC0pos        S                   Like(C0POS)
007100111220$003 D savC0pos1       S                   Like(C0POS1)
007200090225     D*-------------
007300100119     d dataiso         s               d   datfmt(*iso)
007400100119     d dataeur         s               d   datfmt(*eur)
007500111220     D*-------------
007600111221     d  savRAGS        s             35
007700111221     d  savUNBCLI      s                   Like(stsUNBCLI)
007800111220     d  savCODSTS      s                   Like(stsCODSTS)
007900111220     d  savSUBSTS      s                   Like(stsSUBSTS)
008000940207     D*-------------
008100940211     D* COSTANTI
008200940211     D*-------------
008300090225     D SFLPAG0         C                   CONST(11)
008400100617     D SFLPAG1         C                   CONST(12)
008500111220     D*-------------
008600940506     D* Tasti di funzione
008700940506     D F01             C                   CONST(X'31')
008800940506     D F02             C                   CONST(X'32')
008900940506     D F03             C                   CONST(X'33')
009000940506     D F04             C                   CONST(X'34')
009100940506     D F05             C                   CONST(X'35')
009200940506     D F06             C                   CONST(X'36')
009300940506     D F07             C                   CONST(X'37')
009400940506     D F08             C                   CONST(X'38')
009500940506     D F09             C                   CONST(X'39')
009600940506     D F10             C                   CONST(X'3A')
009700940506     D F11             C                   CONST(X'3B')
009800940506     D F12             C                   CONST(X'3C')
009900940506     D F13             C                   CONST(X'B1')
010000940506     D F14             C                   CONST(X'B2')
010100940506     D F15             C                   CONST(X'B3')
010200940506     D F16             C                   CONST(X'B4')
010300940506     D F17             C                   CONST(X'B5')
010400940506     D F18             C                   CONST(X'B6')
010500940506     D F19             C                   CONST(X'B7')
010600940506     D F20             C                   CONST(X'B8')
010700940506     D F21             C                   CONST(X'B9')
010800940506     D F22             C                   CONST(X'BA')
010900940506     D F23             C                   CONST(X'BB')
011000940506     D F24             C                   CONST(X'BC')
011100940506     D ENTER           C                   CONST(X'F1')
011200940506     D ROLDWN          C                   CONST(X'F4')
011300940506     D ROLLUP          C                   CONST(X'F5')
011400940127     C*----------------------------------------------------*
011500940127     C*                MAIN LINE PROGRAM
011600940127     C*----------------------------------------------------*
011700940223     C* inizializzazione variabili
011800940223     C                   EXSR      INZVAR
011900940223     C*
012000940223     C     $FINE         DOWEQ     *OFF
012100090225     C     $GEST         CASEQ     'S0'          GESS0
012200940117     C                   END
012300940117     C                   END
012400940325     C* fine programma
012500940325     C                   SETON                                            LR
012600030113     C************************************************************
012700030113     C* INIZIALIZZAZIONE VARIABILI
012800030113     C************************************************************
012900030113     C     INZVAR        BEGSR
013000030113     C*
013100030113     C* Pulizia campi e indicatori
013200030113     C                   MOVE      *ALL'0'       IN4049           10
013300030113     C                   MOVEA     IN4049        *IN(40)
013400090225     C                   CLEAR                   S0OPZ
013500030113     C*
013600090225     C* Variabili per gestione videate
013700030113     C                   MOVE      *OFF          $FINE
013800030113     C                   MOVE      *OFF          $EFILE
013900030113     C                   MOVE      *OFF          $ESCI
014000030113     C                   MOVE      *OFF          $RCDOK
014100030113     C                   Z-ADD     0             $ULKS1            3 0
014200030113     C*
014300090225     C                   MOVE      *ON           $INZS0
014400030113     C* Variabili appoggio
014500030114     C                   Z-ADD     1             WPAG
014600030113     c*
014700100407     c* Video da cui partire
014800111220     C                   MOVE      'S0'          $GEST
014900100407     c*
015000030113     C                   ENDSR
015100090225     C************************************************************
015200090225     C* GESTIONE LISTA
015300090225     C************************************************************
015400090225     C     GESS0         BEGSR
015500090225     C*
015600090225     C* inizializzazione videata
015700090225     C     $INZS0        IFEQ      *ON
015800090225     C                   EXSR      INZS0
015900090225     C                   MOVE      *OFF          $INZS0
016000090225     C                   ENDIF
016100100325     C*
016200090225     C* emissione piede videata
016300090225     C                   WRITE     Z0
016400090225     C* Non ci sono records
016500090225     C     WMAX          IFEQ      0
016600090225     C                   WRITE     D0
016700090225     C                   Else
016800090225     C     Wsfl          IFgt      0
016900090225     C                   Z-ADD     wsfl          C0RCD
017000090225     C                   Else
017100090225     C     Wpag          IFgt      0
017200090225     C                   Z-ADD     wpag          C0RCD
017300090225     C                   EndIF
017400090225     C                   EndIF
017500090225     C                   ENDIF
017600090225     C*
017700090225     C*              *------------------*
017800090225     C                   EXFMT     C0
017900090225     C*              *------------------*
018000090225     C*
018100090225     C     C0NRR         IFNE      0
018200090225     C                   Z-ADD     C0NRR         WSFL
018300090225     C                   ENDIF
018400090225     C                   Z-ADD     SFLNRR        C0RCD
018500090225     C* Selezioni
0186000902251    C                   SELECT
018700090225     C* F3=Fine
018800090225     C     $TASTO        WHENEQ    F03
018900090225     C                   EXSR      F03S1
019000100121     C* F10=Immissione
019100100121     C     $TASTO        WHENEQ    F10
019200100121     C                   EXSR      F10S1
019300100325     C                   MOVE      'S0'          $GEST
0194000902251O   C                   OTHER
019500090225     C* CONTROLLO DATI
019600090225     C                   EXSR      CTRC0
019700090225     C     *IN99         IFEQ      *OFF
019800090225     C                   EXSR      CTRS0
019900090225     C                   END
0200000902251-   C                   ENDSL
020100090225     C*
020200090225     C                   ENDSR
020300090225     C/EJECT
020400090225     C************************************************************
020500090225     C* INIZIALIZZAZIONE LISTA
020600090225     C************************************************************
020700090225     C     INZS0         BEGSR
020800100407     C*
020900090225     C* pulizia SFL
021000090225     C                   SETOFF                                         3031
021100090225     C                   WRITE     C0
021200090225     C                   SETON                                          31
021300090225     C*
021400090225     C* CARICAMENTO SFL totale
021500090225     C                   Z-ADD     0             S0NRR
021600090225     C                   Z-ADD     1             C0RCD
021700090225     C                   Z-ADD     0             WMAX
021800090225     C                   Z-ADD     0             wsfl
021900090225     C*
022000090225     C*  Posizionamento all'inizio
022100111220     c                   movel(p)  savUNBCLI     stsUNBCLI
022200111220     c                   movel(p)  savUNBCLI     c0UNB
022300111221     c                   movel(p)  savRAGS       c0RAGS
022400111220     c                   movel(p)  c0pos         stsCODSTS
022500111220     c                   movel(p)  c0pos1        stsSUBSTS
022600111220     C*
022700100326     C                   IF        Posiziona = 'S'
022800111220     c     Key_STS       setll     edSTS01L
022900100326     C                   eval      Posiziona = *blank
023000100326     c                   else
023100111220     c     savUNBCLI     setll     edSTS01L
023200100326     c                   end
023300111220     C*
023400111220    >C                   EXSR      READ_STS
023500090225     C* Carico il SFL
023600090225     C                   EXSR      ROLS0
023700090225     C*
023800090225     c                   if        xtaopr <> '1'
023900090225     C                   Z-ADD     1             WPAG
024000090225     c                   end
024100090225     C*
024200090225     C                   ENDSR
024300090225     C************************************************************
024400090225     C* CARICAMENTO PAGINA LISTA
024500090225     C************************************************************
024600090225     C     ROLS0         BEGSR
024700090225     C*
024800090225     C                   SETOFF                                       32
024900090225     C                   Z-ADD     0             Y
025000090225     C                   Z-ADD     WMAX          S0NRR
025100100119     C*
025200090225     C* Leggo dal file i segmenti presenti nell'archivio
0253000902251    C     $EFILE        DOWEQ     *OFF
025400100617     C*
025500111220     c                   clear                   s0opz
025600111220     c                   movel     stsCODSTS     s0cod
025700111220     c                   movel     stsSUBSTS     s0cod1
025800100617     C*
025900111220     c                   movel     stsCODSTSb    s0brt
026000111220     c                   movel     stsSUBSTSb    s0brt1
026100100617     c*
026200111220     c                   eval      s0des =  stsDEScri
026300090225     C*
026400090225     C                   ADD       1             S0NRR
026500090225     C                   ADD       1             Y
026600090225     C                   WRITE     S0
026700100119     C*
026800111220    >C                   EXSR      READ_STS
0269000902251-   C                   ENDDO
027000090225     C*
027100090225     C                   Z-ADD     S0NRR         WMAX                 30
027200090225     C*
027300090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
027400090225     C*
027500090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
027600090225     C                   MVR                     RESTO             3 0
027700090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0278000902251    C     RESTO         IFGT      0
027900090225     C                   ADD       1             C0RCD
0280000902251E   C                   ELSE
028100090225     C                   SUB       SFLPAG0       C0RCD
028200090225     C                   ADD       1             C0RCD
0283000902251-   C                   ENDIF
028400090225     C*
028500090225     C                   ENDSR
028600090225     C************************************************************
028700090225     C* LETTURA RCD
028800090225     C************************************************************
028900111220     C     READ_STS      BEGSR
029000090225     C*
029100090225     C                   MOVEL     *OFF          $EFILE
029200090225     C                   MOVEL     *OFF          $RCDOK
029300090225     C*
0294000902251    C     $EFILE        DOUEQ     *ON
029500090225     C     $RCDOK        OREQ      *ON
029600090225     C*
029700111220     c     stsUNBCLI     Reade     edSTS01L
029800111220     c                   if        %eof(edSTS01L)
029900090225     C                   MOVEL     *ON           $EFILE
030000090225     C                   MOVE      $EFILE        *IN33
030100090225     c                   else
030200100322      **
030300100322     c                   eXSr      Selez_RECord
030400100119     c                   end
030500090225     C*
0306000902251-   C                   ENDDO
030700090225     C*
030800090225     C                   ENDSR
030900090225     C************************************************************
031000090225     C* CONTROLLO TESTATA LISTA
031100090225     C************************************************************
031200100322     C     Selez_RECord  BEGSR
031300100407      *
031400100618     C                   MOVEL     *ON           $RCDOK
031500100322     C*
031600100322     C     End_filtro    ENDSR
031700100322     C************************************************************
031800100322     C* CONTROLLO TESTATA LISTA
031900100322     C************************************************************
032000100322     C     CTRC0         BEGSR
032100100322     C*
032200100326     C                   MOVE      *OFF          *IN99
032300100326     C*
032400100326     C                   clear                   posiziona         1
032500111220     c                   if        c0pos <> SAVc0pos or
032600111220     c                             c0pos1<> SAVc0pos1
032700100326     C                   move      'S'           posiziona         1
032800100326     c                   eval      SAVc0pos = c0pos
032900111220     c                   eval      SAVc0pos1= c0pos1
033000100326     c                   move      *ON           $INZS0
033100100326     c                   end
033200090225     C*
033300090225     C                   ENDSR
033400090225     C************************************************************
033500090225     C* CONTROLLO OPZIONI LISTA
033600090225     C************************************************************
033700090225     C     CTRS0         BEGSR
033800090225     C*
033900090225     C                   MOVEL     *OFF          $ESCI
034000090225     C                   SETOFF                                       99
034100090225     C                   clear                   S0OPZ
034200090225     c                   clear                   x0cod
034300111220     c                   clear                   x0cod1
034400090225     C*
034500090225     C* Leggo il sfl solo se ci sono rcd
0346000902251    C     WMAX          IFGT      0
034700090225     C                   READC     S0                                     21
034800090225     C*
034900090225     C* esce se fine sfl o errore che richiede l'uscita
0350000902252    C     *IN21         DOWEQ     *OFF
035100090225     C     $ESCI         ANDEQ     *OFF
035200090225     C                   Z-ADD     S0NRR         C0RCD
035300090225     C* ctrl su riga
035400090225     C                   EXSR      RECS0
035500090225     C* gestione opzioni
0356001112203    C     S0OPZ         IFNE      *blank
035700090225     C     *IN99         ANDEQ     *OFF
035800090225     C                   EXSR      OPZS0
0359000902253-   C                   ENDIF
036000111220      *
036100090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0362000902253    C     *IN99         IFEQ      *ON
036300090225     C     $ESCI         OREQ      *ON
036400090225     C                   MOVE      *ON           *IN32
036500090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
036600090225     C* la ripristinerò a conclusione del ciclo di READC
036700090225     C                   MOVE      *OFF          $INZS0
0368000902253-   C                   ENDIF
036900090225     C*
037000090225     C                   clear                   S0OPZ
037100090225     C                   UPDATE    S0
037200090225     C*
037300090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0374000902253    C     $ESCI         IFEQ      *OFF
037500090225     C                   READC     S0                                     21
037600111220     C*
037700090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0378000902254    C     *IN21         IFEQ      *ON
037900090225     C                   MOVE      WINZS0        $INZS0
038000090225     C* calcolo pagina a cui deve posizionarsi
038100090225     C                   EXSR      CLCPAG0
0382000902254-   C                   ENDIF
038300111220     C*
0384000902253-   C                   ENDIF
038500090225     C*
0386000902252-   C                   ENDDO
038700090225     C*
0388000902251-   C                   ENDIF
038900090225     C*
039000090225     C                   ENDSR
039100090225     C/EJECT
039200090225     C************************************************************
039300090225     C* CONTROLLO CAMPI I/O RIGA LISTA
039400090225     C************************************************************
039500090225     C     RECS0         BEGSR
039600090225     C*
039700090225     C* reset indicatori DSPATR
039800090225     C                   MOVE      *ALL'0'       IN4049           10
039900090225     C                   MOVEA     IN4049        *IN(40)
040000111220     C*
040100111220     C*  se richiamato x selezionare un codice, --> 11 = *on
040200111220     C*   non deve utilizzare le opzioni di modifica e viceversa
040300111220     c     *in11         ifeq      *on
0404001112203    C     S0OPZ         IFeq      '2'
0405001112203    C     S0OPZ         OReq      '3'
0406001112203    C     S0OPZ         OReq      '4'
040700111220     c                   seton                                        41  99
040800111220     C                   END
040900111220     C*
041000111220      * selezionato un codice da restituire al chiamante
0411001112203    C     S0OPZ         IFeq      '1'
041200111220     c                   movel(p)  s0cod         x0cod
041300111220     c                   movel(p)  s0cod1        x0cod1
041400111220     c                   clear                   kpjbu
041500111220     c                   eval      kpjbu = x0cod + x0cod1
041600111220     C                   MOVEL     *ON           $ESCI
041700111220     C                   MOVEL     *ON           $fine
041800111220     C                   END
041900111220     C                   END
042000111220     C*
042100111220     c     *in11         ifeq      *off
0422001112203    C     S0OPZ         ANDeq     '1'
042300111220     c                   seton                                        41  99
042400111220     C                   END
042500090225     C*
042600090225     C                   ENDSR
042700090225     C************************************************************
042800090225     C* GESTIONE OPZIONI LISTA
042900090225     C************************************************************
043000090225     C     OPZS0         BEGSR
043100090225     C*
043200090225      * selezionato un codice da restituire al chiamante
0433000902253    C     S0OPZ         IFeq      '1'
043400111220     c                   move      s0cod         codSTS            3
043500111220     c                   move      s0cod1        codSUB            3
043600090225     C                   MOVE      'S1'          $GEST
043700090225     C                   MOVEL     *ON           $ESCI
043800090225     C                   END
043900111220     C*
0440001112202    C     *IN11         IFEQ      *Off
0441001112202    C     *IN11         oreq      *On
0442001112202    C     s0opz         andeq     '5'
044300111220     C*
044400111220     C                   RESET                   tabds
044500111220     c                   move      *zero         xtaopz
044600111220     C                   MOVE      S0OPZ         xtaopz
044700111220     C                   MOVE      *ZERO         xtaret
044800111220     C                   MOVE      *ZERO         xtaopr
044900111220     C                   MOVEl(p)  s0COD         xtakey1
045000111220     C                   MOVEl(p)  s0COD1        xtakey2
045100111220     C                   MOVEl     *blank        xtacod1
045200111220     C                   MOVEl     *blank        xtacod2
045300111220     C                   MOVEl     savUNBcli     xtaUNBcli
045400111220     C                   MOVE      *BLANKS       KPJBU
045500111220     C                   MOVEL     tabds         KPJBU
045600111220$004 C                   CALL      'TRTC93SR2'
045700111220     C                   PARM                    KPJBA
045800111220      *
045900111220     C                   MOVEL     KPJBU         tabds
046000111220     C*
046100111220     C* ritorno da PGM gestione
046200111220     C                   EXSR      GESRET
046300090225     C*
046400090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0465000902252    C     *IN99         IFEQ      *ON
046600090225     C                   SETON                                        40
0467000902252-   C                   ENDIF
046800111220     C*
0469001112202-   C                   ENDIF
047000090225     C*
047100090225     C                   ENDSR
047200111220     C*
047300090225     C/EJECT
047400940224     C************************************************************
047500940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
047600940224     C************************************************************
047700090225     C     CLCPAG0       BEGSR
047800940224     C* Input :
047900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
048000940224     C*          cursore
048100940224     C* - SFLPAG = numero rcd per pagina sfl
048200940224     C* Output :
048300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
048400940224     C*
048500090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
048600940224     C                   MVR                     RESTO             3 0
048700940224     C     RESTO         IFGT      0
048800940224     C                   ADD       1             PAGINE
048900940224     C                   ENDIF
049000090226     c                   if        pagine > 1
049100090225     C     PAGINE        MULT      SFLPAG0       WPAG
049200090226     C                   END
049300940224     C*
049400940224     C                   ENDSR
049500940309     C************************************************************
049600940309     C* GESTIONE F03 VIDEO S1
049700940309     C************************************************************
049800940309     C     F03S1         BEGSR
049900940309     C*
050000940309     C                   MOVE      *ON           $FINE
050100940325     C* fine programma
050200940309     C                   ENDSR
050300940309     C/EJECT
050400940309     C************************************************************
050500940309     C* GESTIONE F10 VIDEO S1
050600940314     c* AGGIUNTA RECORD
050700940309     C************************************************************
050800940309     C     F10S1         BEGSR
050900940309     C*
051000030113     C                   RESET                   tabds
051100030113     C                   MOVEL     '01'          xtaopz
051200030113     C                   MOVE      *ZERO         xtaret
051300030113     C                   MOVE      *ZERO         xtaopr
051400100409     C                   MOVEl     *blank        xtakey1
051500100409     C                   MOVEl     *blank        xtakey2
051600111220     C                   MOVEl     *blank        xtacod1
051700111220     C                   MOVEl     *blank        xtacod2
051800111220     C                   MOVEl     savUNBcli     xtaUNBcli
051900030113     C                   MOVE      *BLANKS       KPJBU
052000030113     C                   MOVEL     tabds         KPJBU
052100111220$004 C                   CALL      'TRTC93SR2'
052200030113     C                   PARM                    KPJBA
052300090225      *
052400030114     C                   MOVEL     KPJBU         tabds
052500030113      *
052600940309     C* ritorno da PGM gestione
052700940309     C                   EXSR      GESRET
052800090226      *
052900111221     C     WINZS0        IFEQ      *ON
053000111221     C                   MOVE      *ON           $INZS0
053100090226     C* carico sempre la 1a pagina
053200090226     C                   Z-ADD     1             WPAG
053300940309     C                   ENDIF
053400940309     C*
053500940309     C                   ENDSR
053600940223     C************************************************************
053700940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
053800940223     C************************************************************
053900940223     C     GESRET        BEGSR
054000940223     C*
054100940223     C* modo di ritorno
054200940223     C*
0543009402231    C                   SELECT
054400940314    >C* << questi modi di utilizzo dei valori di ritorno dal
054500940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
054600940314    >C*    proposte, normalmente sempre valide, ma modificabili
054700940314    >C*    per situazioni particolari >>
054800940223     C* 1 = F3
054900030113    >C     xtaret        WHENEQ    '1'
055000940224     C                   MOVE      *ON           $ESCI
055100940223     C                   MOVE      *ON           $FINE
055200940223     C* 2 = F12
055300030113    >C     xtaret        WHENEQ    '2'
055400940223     C                   MOVE      *ON           $ESCI
055500940223     C*
0556009402231-   C                   ENDSL
055700940223     C*
055800940223     C* operazione eseguite dal pgm chiamato
055900940223     C*
0560009402231    C                   SELECT
056100940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
056200030113    >C     xtaopr        WHENEQ    '1'
056300111221     C                   MOVE      *ON           WINZS0
056400940223     C*
0565009402231-   C                   ENDSL
056600940223     C*
056700940223     C* funzione non eseguibile per errore :
056800940223     C*
0569009402231    C                   SELECT
057000940223     C* 1 = funzione richiamata chiusa in errore
057100940316    >C*  eventualmente gestire altri codici di errore
057200030113    >C     xtaerr        WHENEQ    '1'
057300940223     C                   MOVE      *ON           $ESCI
057400940223     C                   SETON                                        5299
057500940223     C*
0576009402231-   C                   ENDSL
057700940223     C*
057800940223     C                   ENDSR
057900940223     C/EJECT
058000940131     C************************************************************
058100940131     C* OPERAZIONI INIZIALI
058200940131     C************************************************************
058300940131     C     *INZSR        BEGSR
058400030113     C*
058500030113     C* Reperimento parametri
058600030113     C     *ENTRY        PLIST
058700030113     C                   PARM                    KPJBA
058800111220     C*
058900111220     C                   eval        TABDS = KPJBU
059000111220     c                   eval      savUNBCLI = xtaUNBCLI
059100111221     c                   eval      savRAGS   = xtaRAGS
059200111221     c     xtaopr        comp      '01'                                   11
059300040930     C*
059400111220     c     key_STS       Klist
059500111220     c                   kfld                    stsUNBCLI
059600111220     c                   kfld                    stsCODSTS
059700111220     c                   kfld                    stsSUBSTS
059800100617      *
059900100617     C* RECUPERO DATI DELL'UTENTE
060000100617     C                   Z-ADD     1             CODUT             1 0
060100100617     C                   CALL      'XPARUT'
060200100617     C                   PARM                    UTEDSE0F
060300100617     C                   MOVEL     RAGUT         RSUT             20
060400100326     C*
060500040930     C* Se chiamato x selezionare un codice
060600111221     c                   clear                   x0cod
060700111221     c                   clear                   x0cod1
060800030113     C*
060900030113     C* Variabili per gestione videate
061000030113     C                   MOVE      *BLANK        $GEST             2
061100030113     C                   MOVE      *BLANK        $FINE             1
061200090225     C                   MOVE      *BLANK        $INZS0            1
061300030113     C                   MOVE      *BLANK        $EFILE            1
061400030113     C                   MOVE      *BLANK        $ESCI             1
061500030113     C                   MOVE      *BLANK        $RCDOK            1
061600030113     C* Indici
061700030113     C                   Z-ADD     0             X                 3 0
061800030113     C                   Z-ADD     0             Y                 3 0
061900940506     C*
062000940506     C* Reperimento tasti di funzione
062100100326     C*
062200100326     C                   clear                   c0pos
062300111220     C                   clear                   c0pos1
062400100326     C                   clear                   SAVc0pos
062500111220     C                   clear                   SAVc0pos1
062600940127     C*
062700940117     C                   ENDSR
062800100319     C************************************************************
