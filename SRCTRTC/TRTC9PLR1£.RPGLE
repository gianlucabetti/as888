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
002100141017     FEDPFL01L  IF   E           K DISK
002200141017     F                                     INFDS(PFLDS)
002300141021     FEDTAB01L  IF   E           K DISK
002400141017     FTRTC9PLD1 CF   E             WORKSTN
002500090225     F                                     SFILE(S0:S0NRR)
002600940201     F                                     INFDS(DSFMT)
002700940128     D*----------------------------------------------------*
002800940211     D* Passaggio Parametri
002900940211     D KPJBA         E DS
003000111220     D UTEDSE0F      E DS
003100141021     D EDIDSPT       E DS
003200030113      *-------------
003300141017     D* Parametri in ricezione
003400141017     D  TABDS        E DS                  EXTNAME(TRTC9PLDS)
003500141017     C*----------------------------------------------------*
003600141017     d PFLds           ds
003700141017     d  PFL_nrr              397    400b 0
003800940211     D*-------------
003900940211     D DSFMT           DS           512
004000940506     D  $TASTO               369    369
004100940211     D  NRG                  370    370
004200940211     D  NCL                  371    371
004300940211     D  SFLNRR               378    379B 0
004400940207     D*-------------
004500940207     D* Nome PGM a video
004600940207     D                 DS
004700940207     D  PROGR                  1     10
004800940207     D  ASTER1                 1      1    INZ('*')
004900940207     D  SIGLA                  2      9
005000940207     D  ASTER2                10     10    INZ('*')
005100940127     D*-------------
005200940127     D* Reperimento nome PGM
005300940127     D STATUS         SDS           333
005400940127     D  DSPGM            *PROC
005500030113     D*-------------
005600111220     C* Variabili appoggio sempre presenti
005700111221$003 D S0NRR           S                   Like(C0rcd)
005800111221$003 D WSfl            S                   Like(C0nrr)
005900111221$003 D Wmax            S                   Like(C0rcd)
006000111221$003 D Wpag            S                   Like(C0rcd)
006100090225$003 D Winzs0          S                   Like($inzs0)
006200111220     D*-------------
006300141021$003 D savLna          S                   Like(XTAKLNA)
006400141021$003 D savLuogo        S                   Like(XTAKLUO)
006500141021$003 D savData         S                   Like(XTAKDAT)
006600141021     D*-------------
006700100326$003 D savC0pos        S                   Like(C0POS)
006800090225     D*-------------
006900100119     d dataiso         s               d   datfmt(*iso)
007000100119     d dataeur         s               d   datfmt(*eur)
007100940207     D*-------------
007200940211     D* COSTANTI
007300940211     D*-------------
007400090225     D SFLPAG0         C                   CONST(11)
007500100617     D SFLPAG1         C                   CONST(12)
007600111220     D*-------------
007700940506     D* Tasti di funzione
007800940506     D F01             C                   CONST(X'31')
007900940506     D F02             C                   CONST(X'32')
008000940506     D F03             C                   CONST(X'33')
008100940506     D F04             C                   CONST(X'34')
008200940506     D F05             C                   CONST(X'35')
008300940506     D F06             C                   CONST(X'36')
008400940506     D F07             C                   CONST(X'37')
008500940506     D F08             C                   CONST(X'38')
008600940506     D F09             C                   CONST(X'39')
008700940506     D F10             C                   CONST(X'3A')
008800940506     D F11             C                   CONST(X'3B')
008900940506     D F12             C                   CONST(X'3C')
009000940506     D F13             C                   CONST(X'B1')
009100940506     D F14             C                   CONST(X'B2')
009200940506     D F15             C                   CONST(X'B3')
009300940506     D F16             C                   CONST(X'B4')
009400940506     D F17             C                   CONST(X'B5')
009500940506     D F18             C                   CONST(X'B6')
009600940506     D F19             C                   CONST(X'B7')
009700940506     D F20             C                   CONST(X'B8')
009800940506     D F21             C                   CONST(X'B9')
009900940506     D F22             C                   CONST(X'BA')
010000940506     D F23             C                   CONST(X'BB')
010100940506     D F24             C                   CONST(X'BC')
010200940506     D ENTER           C                   CONST(X'F1')
010300940506     D ROLDWN          C                   CONST(X'F4')
010400940506     D ROLLUP          C                   CONST(X'F5')
010500940127     C*----------------------------------------------------*
010600940127     C*                MAIN LINE PROGRAM
010700940127     C*----------------------------------------------------*
010800940223     C* inizializzazione variabili
010900940223     C                   EXSR      INZVAR
011000940223     C*
011100940223     C     $FINE         DOWEQ     *OFF
011200090225     C     $GEST         CASEQ     'S0'          GESS0
011300940117     C                   END
011400940117     C                   END
011500940325     C* fine programma
011600940325     C                   SETON                                            LR
011700030113     C************************************************************
011800030113     C* INIZIALIZZAZIONE VARIABILI
011900030113     C************************************************************
012000030113     C     INZVAR        BEGSR
012100030113     C*
012200030113     C* Pulizia campi e indicatori
012300030113     C                   MOVE      *ALL'0'       IN4049           10
012400030113     C                   MOVEA     IN4049        *IN(40)
012500090225     C                   CLEAR                   S0OPZ
012600030113     C*
012700090225     C* Variabili per gestione videate
012800030113     C                   MOVE      *OFF          $FINE
012900030113     C                   MOVE      *OFF          $EFILE
013000030113     C                   MOVE      *OFF          $ESCI
013100030113     C                   MOVE      *OFF          $RCDOK
013200030113     C                   Z-ADD     0             $ULKS1            3 0
013300030113     C*
013400090225     C                   MOVE      *ON           $INZS0
013500030113     C* Variabili appoggio
013600030114     C                   Z-ADD     1             WPAG
013700100407     c* Video da cui partire
013800111220     C                   MOVE      'S0'          $GEST
013900100407     c*
014000030113     C                   ENDSR
014100090225     C************************************************************
014200090225     C* GESTIONE LISTA
014300090225     C************************************************************
014400090225     C     GESS0         BEGSR
014500090225     C*
014600090225     C* inizializzazione videata
014700090225     C     $INZS0        IFEQ      *ON
014800090225     C                   EXSR      INZS0
014900090225     C                   MOVE      *OFF          $INZS0
015000090225     C                   ENDIF
015100100325     C*
015200090225     C* emissione piede videata
015300090225     C                   WRITE     Z0
015400090225     C* Non ci sono records
015500090225     C     WMAX          IFEQ      0
015600090225     C                   WRITE     D0
015700090225     C                   Else
015800090225     C     Wsfl          IFgt      0
015900090225     C                   Z-ADD     wsfl          C0RCD
016000090225     C                   Else
016100090225     C     Wpag          IFgt      0
016200090225     C                   Z-ADD     wpag          C0RCD
016300090225     C                   EndIF
016400090225     C                   EndIF
016500090225     C                   ENDIF
016600090225     C*
016700090225     C*              *------------------*
016800090225     C                   EXFMT     C0
016900090225     C*              *------------------*
017000090225     C*
017100090225     C     C0NRR         IFNE      0
017200090225     C                   Z-ADD     C0NRR         WSFL
017300090225     C                   ENDIF
017400090225     C                   Z-ADD     SFLNRR        C0RCD
017500090225     C* Selezioni
0176000902251    C                   SELECT
017700090225     C* F3=Fine
017800090225     C     $TASTO        WHENEQ    F03
017900090225     C                   EXSR      F03S1
018000100121     C* F10=Immissione
018100100121     C     $TASTO        WHENEQ    F10
018200100121     C                   EXSR      F10S1
018300100325     C                   MOVE      'S0'          $GEST
0184000902251O   C                   OTHER
018500090225     C* CONTROLLO DATI
018600090225     C                   EXSR      CTRC0
018700090225     C     *IN99         IFEQ      *OFF
018800090225     C                   EXSR      CTRS0
018900090225     C                   END
0190000902251-   C                   ENDSL
019100090225     C*
019200090225     C                   ENDSR
019300090225     C/EJECT
019400090225     C************************************************************
019500090225     C* INIZIALIZZAZIONE LISTA
019600090225     C************************************************************
019700090225     C     INZS0         BEGSR
019800100407     C*
019900090225     C* pulizia SFL
020000090225     C                   SETOFF                                         3031
020100090225     C                   WRITE     C0
020200090225     C                   SETON                                          31
020300090225     C*
020400090225     C* CARICAMENTO SFL totale
020500090225     C                   Z-ADD     0             S0NRR
020600090225     C                   Z-ADD     1             C0RCD
020700090225     C                   Z-ADD     0             WMAX
020800131011     C                   Z-ADD     1             wsfl
020900090225     C*
021000141021     C* decodifica la Linea del PARTNER
021100141021     c                   movel     'PT'          tab_PT            2
021200141021     c     tab_PT        setll     edtab01l
021300141021     c     tab_PT        reade     edtab01l
021400141021     c                   dow       not %Eof(edtab01l)
021500141021     c                   movel     tabUNI        edidspt
021600141021     c                   if        §PTLNP = XTAKLNA
021700141021     c                   eval      c0PARTNER = tabKEY
021800141021     c                   leave
021900141021     c                   end
022000141021     c     tab_PT        reade     edtab01l
022100141021     c                   end
022200141021     C*
022300090225     C*  Posizionamento all'inizio
022400141021     c                   z-add     savLna        pflLineaA
022500141021     c                   z-add     savLna        c0LNA
022600141021     c                   movel(p)  savLuogo      c0Luogo
022700141021     c                   movel(p)  c0pos         pflLUOGO
022800111220     C*
022900100326     C                   IF        Posiziona = 'S'
023000141020     c     Key_PFL       setll     edPFL01L
023100100326     C                   eval      Posiziona = *blank
023200100326     c                   else
023300141021     c     savLNA        setll     edPFL01L
023400100326     c                   end
023500111220     C*
023600141021    >C                   EXSR      READ_PFL
023700090225     C* Carico il SFL
023800090225     C                   EXSR      ROLS0
023900090225     C*
024000090225     c                   if        xtaopr <> '1'
024100090225     C                   Z-ADD     1             WPAG
024200090225     c                   end
024300090225     C*
024400090225     C                   ENDSR
024500090225     C************************************************************
024600090225     C* CARICAMENTO PAGINA LISTA
024700090225     C************************************************************
024800090225     C     ROLS0         BEGSR
024900090225     C*
025000090225     C                   SETOFF                                       32
025100090225     C                   Z-ADD     0             Y
025200090225     C                   Z-ADD     WMAX          S0NRR
025300100119     C*
025400090225     C* Leggo dal file i segmenti presenti nell'archivio
0255000902251    C     $EFILE        DOWEQ     *OFF
025600100617     C*
025700111220     c                   clear                   s0opz
025800141021     c                   movel     pflLINEAA     h0Lna
025900141021     c                   movel     pflLUOGO      s0Luogo
026000141021     c                   movel     pflDESCR      s0Des
026100141021     c                   movel     pflCODPFL     s0Plst
026200141021     c                   movel     pflDTDEC      h0DtDec
026300141021     c                   movel     pflDTSCA      h0DtSca
026400141021     c                   move      pflDTDEC      dataiso
026500141021     c                   move      dataiso       dataeur
026600141021     c                   move      dataeur       s0DtDec
026700141021     c                   move      pflDTSCA      dataiso
026800141021     c                   move      dataiso       dataeur
026900141021     c                   move      dataeur       s0DtSca
027000141021     C*
027100141017     c                   z-add     PFL_nrr       h0Nrec
027200090225     C*
027300090225     C                   ADD       1             S0NRR
027400090225     C                   ADD       1             Y
027500090225     C                   WRITE     S0
027600100119     C*
027700141021    >C                   EXSR      READ_PFL
0278000902251-   C                   ENDDO
027900090225     C*
028000090225     C                   Z-ADD     S0NRR         WMAX                 30
028100090225     C*
028200090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
028300090225     C*
028400090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
028500090225     C                   MVR                     RESTO             3 0
028600090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0287000902251    C     RESTO         IFGT      0
028800090225     C                   ADD       1             C0RCD
0289000902251E   C                   ELSE
029000090225     C                   SUB       SFLPAG0       C0RCD
029100090225     C                   ADD       1             C0RCD
0292000902251-   C                   ENDIF
029300090225     C*
029400090225     C                   ENDSR
029500090225     C************************************************************
029600090225     C* LETTURA RCD
029700090225     C************************************************************
029800141021     C     READ_PFL      BEGSR
029900090225     C*
030000090225     C                   MOVEL     *OFF          $EFILE
030100090225     C                   MOVEL     *OFF          $RCDOK
030200090225     C*
0303000902251    C     $EFILE        DOUEQ     *ON
030400090225     C     $RCDOK        OREQ      *ON
030500090225     C*
030600141021     c     PFLLINEAA     Reade     edPFL01L
030700141021     c                   if        %eof(edPFL01L)
030800090225     C                   MOVEL     *ON           $EFILE
030900090225     C                   MOVE      $EFILE        *IN33
031000090225     c                   else
031100100322      **
031200100322     c                   eXSr      Selez_RECord
031300100119     c                   end
031400090225     C*
0315000902251-   C                   ENDDO
031600090225     C*
031700090225     C                   ENDSR
031800090225     C************************************************************
031900090225     C* CONTROLLO TESTATA LISTA
032000090225     C************************************************************
032100100322     C     Selez_RECord  BEGSR
032200100407      *
032300100618     C                   MOVEL     *ON           $RCDOK
032400100322     C*
032500141021     C* Controlli sul record
032600141021     C*      dei records in vigore alla data
032700141021     c                   if        savDATA > 0
032800141021     C*
032900141021     c                   if        PFLDTDEC > savDATA or
033000141021     c                             PFLDTSCA < savDATA
033100141021     C                   MOVEL     *OFF          $RCDOK
033200141021     c                   end
033300141021     C*
033400141021     c                   end
033500141021     C*
033600100322     C     End_filtro    ENDSR
033700100322     C************************************************************
033800100322     C* CONTROLLO TESTATA LISTA
033900100322     C************************************************************
034000100322     C     CTRC0         BEGSR
034100100322     C*
034200100326     C                   MOVE      *OFF          *IN99
034300100326     C*
034400100326     C                   clear                   posiziona         1
034500141021     c                   if        c0pos <> SAVc0pos
034600100326     C                   move      'S'           posiziona         1
034700100326     c                   eval      SAVc0pos = c0pos
034800100326     c                   move      *ON           $INZS0
034900100326     c                   end
035000090225     C*
035100090225     C                   ENDSR
035200090225     C************************************************************
035300090225     C* CONTROLLO OPZIONI LISTA
035400090225     C************************************************************
035500090225     C     CTRS0         BEGSR
035600090225     C*
035700090225     C                   MOVEL     *OFF          $ESCI
035800090225     C                   SETOFF                                       99
035900090225     C                   clear                   S0OPZ
036000090225     C*
036100090225     C* Leggo il sfl solo se ci sono rcd
0362000902251    C     WMAX          IFGT      0
036300090225     C                   READC     S0                                     21
036400090225     C*
036500090225     C* esce se fine sfl o errore che richiede l'uscita
0366000902252    C     *IN21         DOWEQ     *OFF
036700090225     C     $ESCI         ANDEQ     *OFF
036800090225     C                   Z-ADD     S0NRR         C0RCD
036900090225     C* ctrl su riga
037000090225     C                   EXSR      RECS0
037100090225     C* gestione opzioni
0372001112203    C     S0OPZ         IFNE      *blank
037300090225     C     *IN99         ANDEQ     *OFF
037400090225     C                   EXSR      OPZS0
0375000902253-   C                   ENDIF
037600111220      *
037700090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0378000902253    C     *IN99         IFEQ      *ON
037900090225     C     $ESCI         OREQ      *ON
038000090225     C                   MOVE      *ON           *IN32
038100090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038200090225     C* la ripristinerò a conclusione del ciclo di READC
038300090225     C                   MOVE      *OFF          $INZS0
0384000902253-   C                   ENDIF
038500090225     C*
038600090225     C                   clear                   S0OPZ
038700090225     C                   UPDATE    S0
038800090225     C*
038900090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0390000902253    C     $ESCI         IFEQ      *OFF
039100090225     C                   READC     S0                                     21
039200111220     C*
039300090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0394000902254    C     *IN21         IFEQ      *ON
039500090225     C                   MOVE      WINZS0        $INZS0
039600090225     C* calcolo pagina a cui deve posizionarsi
039700090225     C                   EXSR      CLCPAG0
0398000902254-   C                   ENDIF
039900111220     C*
0400000902253-   C                   ENDIF
040100090225     C*
0402000902252-   C                   ENDDO
040300090225     C*
0404000902251-   C                   ENDIF
040500090225     C*
040600090225     C                   ENDSR
040700090225     C/EJECT
040800090225     C************************************************************
040900090225     C* CONTROLLO CAMPI I/O RIGA LISTA
041000090225     C************************************************************
041100090225     C     RECS0         BEGSR
041200090225     C*
041300090225     C* reset indicatori DSPATR
041400090225     C                   MOVE      *ALL'0'       IN4049           10
041500090225     C                   MOVEA     IN4049        *IN(40)
041600111220     C*
041700111220     C*  se richiamato x selezionare un codice, --> 11 = *on
041800111220     C*   non deve utilizzare le opzioni di modifica e viceversa
041900111220     c     *in11         ifeq      *on
0420001112203    C     S0OPZ         IFeq      '2'
0421001112203    C     S0OPZ         OReq      '3'
0422001112203    C     S0OPZ         OReq      '4'
042300111220     c                   seton                                        41  99
042400111220     C                   END
042500141021     C                   END
042600111220     C*
042700111220     c     *in11         ifeq      *off
0428001112203    C     S0OPZ         ANDeq     '1'
042900111220     c                   seton                                        41  99
043000111220     C                   END
043100090225     C*
043200090225     C                   ENDSR
043300090225     C************************************************************
043400090225     C* GESTIONE OPZIONI LISTA
043500090225     C************************************************************
043600090225     C     OPZS0         BEGSR
043700090225     C*
043800141021      * selezionato un codice da restituire al chiamante
0439001410213    C     S0OPZ         IFeq      '1'
044000141021     c                   clear                   kpjbu
044100141021     c                   eval      XTAKLNA = h0Lna
044200141021     c                   eval      XTAKLUO = s0Luogo
044300141021     c                   eval      XTAKPFL = s0Plst
044400141021     c                   eval      XTAKDAT = h0DtDec
044500141021     c                   eval      XTANREC = h0nREC
044600141021     c                   eval      XTADESCR= s0des
044700141021     c                   eval      kpjbu = TABDS
044800141021     C                   MOVE      'S1'          $GEST
044900141021     C                   MOVEL     *ON           $ESCI
045000141021     C                   MOVEL     *ON           $fine
045100141021     C                   END
045200111220     C*
0453001112202    C     *IN11         IFEQ      *Off
0454001112202    C     *IN11         oreq      *On
0455001112202    C     s0opz         andeq     '5'
045600111220     C*
045700111220     C                   RESET                   tabds
045800111220     c                   move      *zero         xtaopz
045900111220     C                   MOVE      S0OPZ         xtaopz
046000111220     C                   MOVE      *ZERO         xtaret
046100111220     C                   MOVE      *ZERO         xtaopr
046200141021     C                   z-add     h0Lna         xtaKLNA
046300141021     C                   MOVEl(p)  s0Luogo       xtaKLUO
046400141021     C                   MOVEl(p)  s0PLST        xtaKPFL
046500141021     c                   eval      XTADESCR= s0des
046600141021     C                   z-add     h0DtDec       xtaKDAT
046700131011     C                   MOVEl     h0Nrec        xtanrec
046800111220     C                   MOVE      *BLANKS       KPJBU
046900111220     C                   MOVEL     tabds         KPJBU
047000141021$004 C                   CALL      'TRTC9PLR2'
047100111220     C                   PARM                    KPJBA
047200111220      *
047300111220     C                   MOVEL     KPJBU         tabds
047400111220     C*
047500111220     C* ritorno da PGM gestione
047600111220     C                   EXSR      GESRET
047700090225     C*
047800090225     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0479000902252    C     *IN99         IFEQ      *ON
048000090225     C                   SETON                                        40
0481000902252-   C                   ENDIF
048200111220     C*
0483001112202-   C                   ENDIF
048400090225     C*
048500090225     C                   ENDSR
048600111220     C*
048700090225     C/EJECT
048800940224     C************************************************************
048900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
049000940224     C************************************************************
049100090225     C     CLCPAG0       BEGSR
049200940224     C* Input :
049300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
049400940224     C*          cursore
049500940224     C* - SFLPAG = numero rcd per pagina sfl
049600940224     C* Output :
049700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
049800940224     C*
049900090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
050000940224     C                   MVR                     RESTO             3 0
050100940224     C     RESTO         IFGT      0
050200940224     C                   ADD       1             PAGINE
050300940224     C                   ENDIF
050400090226     c                   if        pagine > 1
050500090225     C     PAGINE        MULT      SFLPAG0       WPAG
050600090226     C                   END
050700940224     C*
050800940224     C                   ENDSR
050900940309     C************************************************************
051000940309     C* GESTIONE F03 VIDEO S1
051100940309     C************************************************************
051200940309     C     F03S1         BEGSR
051300940309     C*
051400940309     C                   MOVE      *ON           $FINE
051500940325     C* fine programma
051600940309     C                   ENDSR
051700940309     C/EJECT
051800940309     C************************************************************
051900940309     C* GESTIONE F10 VIDEO S1
052000940314     c* AGGIUNTA RECORD
052100940309     C************************************************************
052200940309     C     F10S1         BEGSR
052300940309     C*
052400030113     C                   RESET                   tabds
052500141021     C                   MOVEL     '10'          xtaopz
052600030113     C                   MOVE      *ZERO         xtaret
052700030113     C                   MOVE      *ZERO         xtaopr
052800141021     C                   z-add     0             XTAKDAT
052900141021     C                   MOVEl     *blank        XTAKPFL
053000141021     C                   MOVEl     savLNA        XTAKLNA
053100141021     C                   MOVEl     *blank        XTAKLUO
053200141021     C                   MOVEl     *blank        XTAdescr
053300131011     C                   z-add     0             xtaNREC
053400030113     C                   MOVE      *BLANKS       KPJBU
053500030113     C                   MOVEL     tabds         KPJBU
053600141021$004 C                   CALL      'TRTC9PLR2'
053700030113     C                   PARM                    KPJBA
053800090225      *
053900030114     C                   MOVEL     KPJBU         tabds
054000030113      *
054100940309     C* ritorno da PGM gestione
054200940309     C                   EXSR      GESRET
054300090226      *
054400111221     C     WINZS0        IFEQ      *ON
054500111221     C                   MOVE      *ON           $INZS0
054600090226     C* carico sempre la 1a pagina
054700090226     C                   Z-ADD     1             WPAG
054800940309     C                   ENDIF
054900940309     C*
055000940309     C                   ENDSR
055100940223     C************************************************************
055200940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
055300940223     C************************************************************
055400940223     C     GESRET        BEGSR
055500940223     C*
055600940223     C* modo di ritorno
055700940223     C*
0558009402231    C                   SELECT
055900940314    >C* << questi modi di utilizzo dei valori di ritorno dal
056000940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
056100940314    >C*    proposte, normalmente sempre valide, ma modificabili
056200940314    >C*    per situazioni particolari >>
056300940223     C* 1 = F3
056400030113    >C     xtaret        WHENEQ    '1'
056500940224     C                   MOVE      *ON           $ESCI
056600940223     C                   MOVE      *ON           $FINE
056700940223     C* 2 = F12
056800030113    >C     xtaret        WHENEQ    '2'
056900940223     C                   MOVE      *ON           $ESCI
057000940223     C*
0571009402231-   C                   ENDSL
057200940223     C*
057300940223     C* operazione eseguite dal pgm chiamato
057400940223     C*
0575009402231    C                   SELECT
057600940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
057700030113    >C     xtaopr        WHENEQ    '1'
057800111221     C                   MOVE      *ON           WINZS0
057900940223     C*
0580009402231-   C                   ENDSL
058100940223     C*
058200940223     C* funzione non eseguibile per errore :
058300940223     C*
0584009402231    C                   SELECT
058500940223     C* 1 = funzione richiamata chiusa in errore
058600940316    >C*  eventualmente gestire altri codici di errore
058700030113    >C     xtaerr        WHENEQ    '1'
058800940223     C                   MOVE      *ON           $ESCI
058900940223     C                   SETON                                        5299
059000940223     C*
0591009402231-   C                   ENDSL
059200940223     C*
059300940223     C                   ENDSR
059400940223     C/EJECT
059500940131     C************************************************************
059600940131     C* OPERAZIONI INIZIALI
059700940131     C************************************************************
059800940131     C     *INZSR        BEGSR
059900030113     C*
060000030113     C* Reperimento parametri
060100030113     C     *ENTRY        PLIST
060200030113     C                   PARM                    KPJBA
060300111220     C*
060400111220     C                   eval        TABDS = KPJBU
060500141021     c                   eval      savLNA    = XTAKLNA
060600141021     c                   eval      savLUOGO  = XTAKLUO
060700141021     c                   eval      savDATA   = XTAKDAT
060800141017     c     xtaopZ        comp      '01'                                   11
060900040930     C*
061000141020     c     key_PFL       Klist
061100141020     c                   kfld                    PFLLINEAA
061200141020     c                   kfld                    PFLLUOGO
061300100617      *
061400100617     C* RECUPERO DATI DELL'UTENTE
061500100617     C                   Z-ADD     1             CODUT             1 0
061600100617     C                   CALL      'XPARUT'
061700100617     C                   PARM                    UTEDSE0F
061800100617     C                   MOVEL     RAGUT         RSUT             20
061900030113     C*
062000030113     C* Variabili per gestione videate
062100030113     C                   MOVE      *BLANK        $GEST             2
062200030113     C                   MOVE      *BLANK        $FINE             1
062300090225     C                   MOVE      *BLANK        $INZS0            1
062400030113     C                   MOVE      *BLANK        $EFILE            1
062500030113     C                   MOVE      *BLANK        $ESCI             1
062600030113     C                   MOVE      *BLANK        $RCDOK            1
062700030113     C* Indici
062800030113     C                   Z-ADD     0             X                 3 0
062900030113     C                   Z-ADD     0             Y                 3 0
063000940506     C*
063100940506     C* Reperimento tasti di funzione
063200100326     C*
063300100326     C                   clear                   c0pos
063400100326     C                   clear                   SAVc0pos
063500940127     C*
063600940117     C                   ENDSR
063700100319     C************************************************************
