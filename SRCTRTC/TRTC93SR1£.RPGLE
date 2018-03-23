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
002200131011     F                                     INFDS(STSDS)
002300111220     FTRTC93SD1 CF   E             WORKSTN
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
004300111221     D  XTARAGs               53     84
004400131011     D  XTANREC               85     93  0
004500131011     D*-------------
004600131011     d stsds           ds
004700131011     d  sts_nrr              397    400b 0
004800940211     D*-------------
004900940211     D DSFMT           DS           512
005000940506     D  $TASTO               369    369
005100940211     D  NRG                  370    370
005200940211     D  NCL                  371    371
005300940211     D  SFLNRR               378    379B 0
005400940207     D*-------------
005500940207     D* Nome PGM a video
005600940207     D                 DS
005700940207     D  PROGR                  1     10
005800940207     D  ASTER1                 1      1    INZ('*')
005900940207     D  SIGLA                  2      9
006000940207     D  ASTER2                10     10    INZ('*')
006100940127     D*-------------
006200940127     D* Reperimento nome PGM
006300940127     D STATUS         SDS           333
006400940127     D  DSPGM            *PROC
006500030113     D*-------------
006600111220     C* Variabili appoggio sempre presenti
006700111221$003 D S0NRR           S                   Like(C0rcd)
006800111221$003 D WSfl            S                   Like(C0nrr)
006900111221$003 D Wmax            S                   Like(C0rcd)
007000111221$003 D Wpag            S                   Like(C0rcd)
007100090225$003 D Winzs0          S                   Like($inzs0)
007200090225$003 D x0cod           S                   Like(s0cod)
007300111220$003 D x0cod1          S                   Like(s0cod1)
007400131011$003 D x0nrec          S                   Like(h0nrec)
007500111220     D*-------------
007600100326$003 D savC0pos        S                   Like(C0POS)
007700111220$003 D savC0pos1       S                   Like(C0POS1)
007800090225     D*-------------
007900100119     d dataiso         s               d   datfmt(*iso)
008000100119     d dataeur         s               d   datfmt(*eur)
008100111220     D*-------------
008200111221     d  savRAGS        s             35
008300111221     d  savUNBCLI      s                   Like(stsUNBCLI)
008400111220     d  savCODSTS      s                   Like(stsCODSTS)
008500111220     d  savSUBSTS      s                   Like(stsSUBSTS)
008600940207     D*-------------
008700940211     D* COSTANTI
008800940211     D*-------------
008900090225     D SFLPAG0         C                   CONST(11)
009000100617     D SFLPAG1         C                   CONST(12)
009100111220     D*-------------
009200940506     D* Tasti di funzione
009300940506     D F01             C                   CONST(X'31')
009400940506     D F02             C                   CONST(X'32')
009500940506     D F03             C                   CONST(X'33')
009600940506     D F04             C                   CONST(X'34')
009700940506     D F05             C                   CONST(X'35')
009800940506     D F06             C                   CONST(X'36')
009900940506     D F07             C                   CONST(X'37')
010000940506     D F08             C                   CONST(X'38')
010100940506     D F09             C                   CONST(X'39')
010200940506     D F10             C                   CONST(X'3A')
010300940506     D F11             C                   CONST(X'3B')
010400940506     D F12             C                   CONST(X'3C')
010500940506     D F13             C                   CONST(X'B1')
010600940506     D F14             C                   CONST(X'B2')
010700940506     D F15             C                   CONST(X'B3')
010800940506     D F16             C                   CONST(X'B4')
010900940506     D F17             C                   CONST(X'B5')
011000940506     D F18             C                   CONST(X'B6')
011100940506     D F19             C                   CONST(X'B7')
011200940506     D F20             C                   CONST(X'B8')
011300940506     D F21             C                   CONST(X'B9')
011400940506     D F22             C                   CONST(X'BA')
011500940506     D F23             C                   CONST(X'BB')
011600940506     D F24             C                   CONST(X'BC')
011700940506     D ENTER           C                   CONST(X'F1')
011800940506     D ROLDWN          C                   CONST(X'F4')
011900940506     D ROLLUP          C                   CONST(X'F5')
012000940127     C*----------------------------------------------------*
012100940127     C*                MAIN LINE PROGRAM
012200940127     C*----------------------------------------------------*
012300940223     C* inizializzazione variabili
012400940223     C                   EXSR      INZVAR
012500940223     C*
012600940223     C     $FINE         DOWEQ     *OFF
012700090225     C     $GEST         CASEQ     'S0'          GESS0
012800940117     C                   END
012900940117     C                   END
013000940325     C* fine programma
013100940325     C                   SETON                                            LR
013200030113     C************************************************************
013300030113     C* INIZIALIZZAZIONE VARIABILI
013400030113     C************************************************************
013500030113     C     INZVAR        BEGSR
013600030113     C*
013700030113     C* Pulizia campi e indicatori
013800030113     C                   MOVE      *ALL'0'       IN4049           10
013900030113     C                   MOVEA     IN4049        *IN(40)
014000090225     C                   CLEAR                   S0OPZ
014100030113     C*
014200090225     C* Variabili per gestione videate
014300030113     C                   MOVE      *OFF          $FINE
014400030113     C                   MOVE      *OFF          $EFILE
014500030113     C                   MOVE      *OFF          $ESCI
014600030113     C                   MOVE      *OFF          $RCDOK
014700030113     C                   Z-ADD     0             $ULKS1            3 0
014800030113     C*
014900090225     C                   MOVE      *ON           $INZS0
015000030113     C* Variabili appoggio
015100030114     C                   Z-ADD     1             WPAG
015200030113     c*
015300100407     c* Video da cui partire
015400111220     C                   MOVE      'S0'          $GEST
015500100407     c*
015600030113     C                   ENDSR
015700090225     C************************************************************
015800090225     C* GESTIONE LISTA
015900090225     C************************************************************
016000090225     C     GESS0         BEGSR
016100090225     C*
016200090225     C* inizializzazione videata
016300090225     C     $INZS0        IFEQ      *ON
016400090225     C                   EXSR      INZS0
016500090225     C                   MOVE      *OFF          $INZS0
016600090225     C                   ENDIF
016700100325     C*
016800090225     C* emissione piede videata
016900090225     C                   WRITE     Z0
017000090225     C* Non ci sono records
017100090225     C     WMAX          IFEQ      0
017200090225     C                   WRITE     D0
017300090225     C                   Else
017400090225     C     Wsfl          IFgt      0
017500090225     C                   Z-ADD     wsfl          C0RCD
017600090225     C                   Else
017700090225     C     Wpag          IFgt      0
017800090225     C                   Z-ADD     wpag          C0RCD
017900090225     C                   EndIF
018000090225     C                   EndIF
018100090225     C                   ENDIF
018200090225     C*
018300090225     C*              *------------------*
018400090225     C                   EXFMT     C0
018500090225     C*              *------------------*
018600090225     C*
018700090225     C     C0NRR         IFNE      0
018800090225     C                   Z-ADD     C0NRR         WSFL
018900090225     C                   ENDIF
019000090225     C                   Z-ADD     SFLNRR        C0RCD
019100090225     C* Selezioni
0192000902251    C                   SELECT
019300090225     C* F3=Fine
019400090225     C     $TASTO        WHENEQ    F03
019500090225     C                   EXSR      F03S1
019600100121     C* F10=Immissione
019700100121     C     $TASTO        WHENEQ    F10
019800100121     C                   EXSR      F10S1
019900100325     C                   MOVE      'S0'          $GEST
0200000902251O   C                   OTHER
020100090225     C* CONTROLLO DATI
020200090225     C                   EXSR      CTRC0
020300090225     C     *IN99         IFEQ      *OFF
020400090225     C                   EXSR      CTRS0
020500090225     C                   END
0206000902251-   C                   ENDSL
020700090225     C*
020800090225     C                   ENDSR
020900090225     C/EJECT
021000090225     C************************************************************
021100090225     C* INIZIALIZZAZIONE LISTA
021200090225     C************************************************************
021300090225     C     INZS0         BEGSR
021400100407     C*
021500090225     C* pulizia SFL
021600090225     C                   SETOFF                                         3031
021700090225     C                   WRITE     C0
021800090225     C                   SETON                                          31
021900090225     C*
022000090225     C* CARICAMENTO SFL totale
022100090225     C                   Z-ADD     0             S0NRR
022200090225     C                   Z-ADD     1             C0RCD
022300090225     C                   Z-ADD     0             WMAX
022400131011     C                   Z-ADD     1             wsfl
022500090225     C*
022600090225     C*  Posizionamento all'inizio
022700111220     c                   movel(p)  savUNBCLI     stsUNBCLI
022800111220     c                   movel(p)  savUNBCLI     c0UNB
022900111221     c                   movel(p)  savRAGS       c0RAGS
023000111220     c                   movel(p)  c0pos         stsCODSTS
023100111220     c                   movel(p)  c0pos1        stsSUBSTS
023200111220     C*
023300100326     C                   IF        Posiziona = 'S'
023400111220     c     Key_STS       setll     edSTS01L
023500100326     C                   eval      Posiziona = *blank
023600100326     c                   else
023700111220     c     savUNBCLI     setll     edSTS01L
023800100326     c                   end
023900111220     C*
024000111220    >C                   EXSR      READ_STS
024100090225     C* Carico il SFL
024200090225     C                   EXSR      ROLS0
024300090225     C*
024400090225     c                   if        xtaopr <> '1'
024500090225     C                   Z-ADD     1             WPAG
024600090225     c                   end
024700090225     C*
024800090225     C                   ENDSR
024900090225     C************************************************************
025000090225     C* CARICAMENTO PAGINA LISTA
025100090225     C************************************************************
025200090225     C     ROLS0         BEGSR
025300090225     C*
025400090225     C                   SETOFF                                       32
025500090225     C                   Z-ADD     0             Y
025600090225     C                   Z-ADD     WMAX          S0NRR
025700100119     C*
025800090225     C* Leggo dal file i segmenti presenti nell'archivio
0259000902251    C     $EFILE        DOWEQ     *OFF
026000100617     C*
026100111220     c                   clear                   s0opz
026200111220     c                   movel     stsCODSTS     s0cod
026300111220     c                   movel     stsSUBSTS     s0cod1
026400131011     c                   z-add     sts_nrr       h0Nrec
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
035000111220     c                   clear                   x0cod1
035100131011     c                   clear                   x0nrec
035200090225     C*
035300090225     C* Leggo il sfl solo se ci sono rcd
0354000902251    C     WMAX          IFGT      0
035500090225     C                   READC     S0                                     21
035600090225     C*
035700090225     C* esce se fine sfl o errore che richiede l'uscita
0358000902252    C     *IN21         DOWEQ     *OFF
035900090225     C     $ESCI         ANDEQ     *OFF
036000090225     C                   Z-ADD     S0NRR         C0RCD
036100090225     C* ctrl su riga
036200090225     C                   EXSR      RECS0
036300090225     C* gestione opzioni
0364001112203    C     S0OPZ         IFNE      *blank
036500090225     C     *IN99         ANDEQ     *OFF
036600090225     C                   EXSR      OPZS0
0367000902253-   C                   ENDIF
036800111220      *
036900090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0370000902253    C     *IN99         IFEQ      *ON
037100090225     C     $ESCI         OREQ      *ON
037200090225     C                   MOVE      *ON           *IN32
037300090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
037400090225     C* la ripristinerò a conclusione del ciclo di READC
037500090225     C                   MOVE      *OFF          $INZS0
0376000902253-   C                   ENDIF
037700090225     C*
037800090225     C                   clear                   S0OPZ
037900090225     C                   UPDATE    S0
038000090225     C*
038100090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0382000902253    C     $ESCI         IFEQ      *OFF
038300090225     C                   READC     S0                                     21
038400111220     C*
038500090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0386000902254    C     *IN21         IFEQ      *ON
038700090225     C                   MOVE      WINZS0        $INZS0
038800090225     C* calcolo pagina a cui deve posizionarsi
038900090225     C                   EXSR      CLCPAG0
0390000902254-   C                   ENDIF
039100111220     C*
0392000902253-   C                   ENDIF
039300090225     C*
0394000902252-   C                   ENDDO
039500090225     C*
0396000902251-   C                   ENDIF
039700090225     C*
039800090225     C                   ENDSR
039900090225     C/EJECT
040000090225     C************************************************************
040100090225     C* CONTROLLO CAMPI I/O RIGA LISTA
040200090225     C************************************************************
040300090225     C     RECS0         BEGSR
040400090225     C*
040500090225     C* reset indicatori DSPATR
040600090225     C                   MOVE      *ALL'0'       IN4049           10
040700090225     C                   MOVEA     IN4049        *IN(40)
040800111220     C*
040900111220     C*  se richiamato x selezionare un codice, --> 11 = *on
041000111220     C*   non deve utilizzare le opzioni di modifica e viceversa
041100111220     c     *in11         ifeq      *on
0412001112203    C     S0OPZ         IFeq      '2'
0413001112203    C     S0OPZ         OReq      '3'
0414001112203    C     S0OPZ         OReq      '4'
041500111220     c                   seton                                        41  99
041600111220     C                   END
041700111220     C*
041800111220      * selezionato un codice da restituire al chiamante
0419001112203    C     S0OPZ         IFeq      '1'
042000111220     c                   movel(p)  s0cod         x0cod
042100111220     c                   movel(p)  s0cod1        x0cod1
042200131011     c                   z-add     h0nREC        x0nrec
042300111220     c                   clear                   kpjbu
042400111220     c                   eval      kpjbu = x0cod + x0cod1
042500111220     C                   MOVEL     *ON           $ESCI
042600111220     C                   MOVEL     *ON           $fine
042700111220     C                   END
042800111220     C                   END
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
044100090225      * selezionato un codice da restituire al chiamante
0442000902253    C     S0OPZ         IFeq      '1'
044300111220     c                   move      s0cod         codSTS            3
044400111220     c                   move      s0cod1        codSUB            3
044500090225     C                   MOVE      'S1'          $GEST
044600090225     C                   MOVEL     *ON           $ESCI
044700090225     C                   END
044800111220     C*
0449001112202    C     *IN11         IFEQ      *Off
0450001112202    C     *IN11         oreq      *On
0451001112202    C     s0opz         andeq     '5'
045200111220     C*
045300111220     C                   RESET                   tabds
045400111220     c                   move      *zero         xtaopz
045500111220     C                   MOVE      S0OPZ         xtaopz
045600111220     C                   MOVE      *ZERO         xtaret
045700111220     C                   MOVE      *ZERO         xtaopr
045800111220     C                   MOVEl(p)  s0COD         xtakey1
045900111220     C                   MOVEl(p)  s0COD1        xtakey2
046000111220     C                   MOVEl     *blank        xtacod1
046100111220     C                   MOVEl     *blank        xtacod2
046200111220     C                   MOVEl     savUNBcli     xtaUNBcli
046300111221     C                   MOVEl     savRAGs       xtaRAGs
046400131011     C                   MOVEl     h0Nrec        xtanrec
046500111220     C                   MOVE      *BLANKS       KPJBU
046600111220     C                   MOVEL     tabds         KPJBU
046700111220$004 C                   CALL      'TRTC93SR2'
046800111220     C                   PARM                    KPJBA
046900111220      *
047000111220     C                   MOVEL     KPJBU         tabds
047100111220     C*
047200111220     C* ritorno da PGM gestione
047300111220     C                   EXSR      GESRET
047400090225     C*
047500090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0476000902252    C     *IN99         IFEQ      *ON
047700090225     C                   SETON                                        40
0478000902252-   C                   ENDIF
047900111220     C*
0480001112202-   C                   ENDIF
048100090225     C*
048200090225     C                   ENDSR
048300111220     C*
048400090225     C/EJECT
048500940224     C************************************************************
048600940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
048700940224     C************************************************************
048800090225     C     CLCPAG0       BEGSR
048900940224     C* Input :
049000940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
049100940224     C*          cursore
049200940224     C* - SFLPAG = numero rcd per pagina sfl
049300940224     C* Output :
049400940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
049500940224     C*
049600090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
049700940224     C                   MVR                     RESTO             3 0
049800940224     C     RESTO         IFGT      0
049900940224     C                   ADD       1             PAGINE
050000940224     C                   ENDIF
050100090226     c                   if        pagine > 1
050200090225     C     PAGINE        MULT      SFLPAG0       WPAG
050300090226     C                   END
050400940224     C*
050500940224     C                   ENDSR
050600940309     C************************************************************
050700940309     C* GESTIONE F03 VIDEO S1
050800940309     C************************************************************
050900940309     C     F03S1         BEGSR
051000940309     C*
051100940309     C                   MOVE      *ON           $FINE
051200940325     C* fine programma
051300940309     C                   ENDSR
051400940309     C/EJECT
051500940309     C************************************************************
051600940309     C* GESTIONE F10 VIDEO S1
051700940314     c* AGGIUNTA RECORD
051800940309     C************************************************************
051900940309     C     F10S1         BEGSR
052000940309     C*
052100030113     C                   RESET                   tabds
052200030113     C                   MOVEL     '01'          xtaopz
052300030113     C                   MOVE      *ZERO         xtaret
052400030113     C                   MOVE      *ZERO         xtaopr
052500100409     C                   MOVEl     *blank        xtakey1
052600100409     C                   MOVEl     *blank        xtakey2
052700111220     C                   MOVEl     *blank        xtacod1
052800111220     C                   MOVEl     *blank        xtacod2
052900111220     C                   MOVEl     savUNBcli     xtaUNBcli
053000111221     C                   MOVEl     savRAGs       xtaRAGs
053100131011     C                   z-add     0             xtaNREC
053200030113     C                   MOVE      *BLANKS       KPJBU
053300030113     C                   MOVEL     tabds         KPJBU
053400111220$004 C                   CALL      'TRTC93SR2'
053500030113     C                   PARM                    KPJBA
053600090225      *
053700030114     C                   MOVEL     KPJBU         tabds
053800030113      *
053900940309     C* ritorno da PGM gestione
054000940309     C                   EXSR      GESRET
054100090226      *
054200111221     C     WINZS0        IFEQ      *ON
054300111221     C                   MOVE      *ON           $INZS0
054400090226     C* carico sempre la 1a pagina
054500090226     C                   Z-ADD     1             WPAG
054600940309     C                   ENDIF
054700940309     C*
054800940309     C                   ENDSR
054900940223     C************************************************************
055000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
055100940223     C************************************************************
055200940223     C     GESRET        BEGSR
055300940223     C*
055400940223     C* modo di ritorno
055500940223     C*
0556009402231    C                   SELECT
055700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
055800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
055900940314    >C*    proposte, normalmente sempre valide, ma modificabili
056000940314    >C*    per situazioni particolari >>
056100940223     C* 1 = F3
056200030113    >C     xtaret        WHENEQ    '1'
056300940224     C                   MOVE      *ON           $ESCI
056400940223     C                   MOVE      *ON           $FINE
056500940223     C* 2 = F12
056600030113    >C     xtaret        WHENEQ    '2'
056700940223     C                   MOVE      *ON           $ESCI
056800940223     C*
0569009402231-   C                   ENDSL
057000940223     C*
057100940223     C* operazione eseguite dal pgm chiamato
057200940223     C*
0573009402231    C                   SELECT
057400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
057500030113    >C     xtaopr        WHENEQ    '1'
057600111221     C                   MOVE      *ON           WINZS0
057700940223     C*
0578009402231-   C                   ENDSL
057900940223     C*
058000940223     C* funzione non eseguibile per errore :
058100940223     C*
0582009402231    C                   SELECT
058300940223     C* 1 = funzione richiamata chiusa in errore
058400940316    >C*  eventualmente gestire altri codici di errore
058500030113    >C     xtaerr        WHENEQ    '1'
058600940223     C                   MOVE      *ON           $ESCI
058700940223     C                   SETON                                        5299
058800940223     C*
0589009402231-   C                   ENDSL
059000940223     C*
059100940223     C                   ENDSR
059200940223     C/EJECT
059300940131     C************************************************************
059400940131     C* OPERAZIONI INIZIALI
059500940131     C************************************************************
059600940131     C     *INZSR        BEGSR
059700030113     C*
059800030113     C* Reperimento parametri
059900030113     C     *ENTRY        PLIST
060000030113     C                   PARM                    KPJBA
060100111220     C*
060200111220     C                   eval        TABDS = KPJBU
060300111220     c                   eval      savUNBCLI = xtaUNBCLI
060400111221     c                   eval      savRAGS   = xtaRAGS
060500111221     c     xtaopr        comp      '01'                                   11
060600040930     C*
060700111220     c     key_STS       Klist
060800111220     c                   kfld                    stsUNBCLI
060900111220     c                   kfld                    stsCODSTS
061000111220     c                   kfld                    stsSUBSTS
061100100617      *
061200100617     C* RECUPERO DATI DELL'UTENTE
061300100617     C                   Z-ADD     1             CODUT             1 0
061400100617     C                   CALL      'XPARUT'
061500100617     C                   PARM                    UTEDSE0F
061600100617     C                   MOVEL     RAGUT         RSUT             20
061700100326     C*
061800040930     C* Se chiamato x selezionare un codice
061900111221     c                   clear                   x0cod
062000111221     c                   clear                   x0cod1
062100131011     c                   clear                   x0nrec
062200030113     C*
062300030113     C* Variabili per gestione videate
062400030113     C                   MOVE      *BLANK        $GEST             2
062500030113     C                   MOVE      *BLANK        $FINE             1
062600090225     C                   MOVE      *BLANK        $INZS0            1
062700030113     C                   MOVE      *BLANK        $EFILE            1
062800030113     C                   MOVE      *BLANK        $ESCI             1
062900030113     C                   MOVE      *BLANK        $RCDOK            1
063000030113     C* Indici
063100030113     C                   Z-ADD     0             X                 3 0
063200030113     C                   Z-ADD     0             Y                 3 0
063300940506     C*
063400940506     C* Reperimento tasti di funzione
063500100326     C*
063600100326     C                   clear                   c0pos
063700111220     C                   clear                   c0pos1
063800100326     C                   clear                   SAVc0pos
063900111220     C                   clear                   SAVc0pos1
064000940127     C*
064100940117     C                   ENDSR
064200100319     C************************************************************
