000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200940211     H DECEDIT('0,') DATEDIT(*DMY.)
000300141023      *-----------------------------------------------------------------------
000400141023      *  ELENCO DI RICERCA : si seleziona un  codice LUOGO già inserito
000500141023      *            per evitare di scriverlo diversamente
000600141023      *            OPPURE si deciderà VOLONTARIAMENTE di scriverlo diverso.
000700141023      *-----------------------------------------------------------------------
000800040930      *  11           x selezione di un codice da ripassare al pgm chiamante
000900940307      *  21           GENERICO OPERAZIONI I/O
001000940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001100940224      *  30           SFLDSP
001200940224      * N31           SFLCLR
001300940224      *  31           SFLDSPCTL
001400940224      *  32           SFLNXTCHG
001500940224      *  33           SFLEND
001600940224      *  39           OF PRTF
001700940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001800940608      *  Specificare l'uso dei singoli indicatori
001900940224      *  50 <---> 98  ERRORI SU VIDEO
002000940608      *  Specificare l'uso dei singoli indicatori
002100940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002200940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002300940224      *  99           INDIC. GENERALE DI ERRORE
002400940128     F*----------------------------------------------------*
002500141023     FEDPFL01L  IF   E           K DISK    usropn
002600141017     F                                     INFDS(PFLDS)
002700141023     FTRTC9PLD3 CF   E             WORKSTN
002800090225     F                                     SFILE(S0:S0NRR)
002900940201     F                                     INFDS(DSFMT)
003000141017     C*----------------------------------------------------*
003100141017     d PFLds           ds
003200141017     d  PFL_nrr              397    400b 0
003300940211     D*-------------
003400940211     D DSFMT           DS           512
003500940506     D  $TASTO               369    369
003600940211     D  NRG                  370    370
003700940211     D  NCL                  371    371
003800940211     D  SFLNRR               378    379B 0
003900940207     D*-------------
004000940207     D* Nome PGM a video
004100940207     D                 DS
004200940207     D  PROGR                  1     10
004300940207     D  ASTER1                 1      1    INZ('*')
004400940207     D  SIGLA                  2      9
004500940207     D  ASTER2                10     10    INZ('*')
004600940127     D*-------------
004700940127     D* Reperimento nome PGM
004800940127     D STATUS         SDS           333
004900940127     D  DSPGM            *PROC
005000030113     D*-------------
005100111220     C* Variabili appoggio sempre presenti
005200111221$003 D S0NRR           S                   Like(C0rcd)
005300111221$003 D WSfl            S                   Like(C0nrr)
005400111221$003 D Wmax            S                   Like(C0rcd)
005500111221$003 D Wpag            S                   Like(C0rcd)
005600090225$003 D Winzs0          S                   Like($inzs0)
005700111220     D*-------------
005800100326$003 D savC0pos        S                   Like(C0POS)
005900090225     D*-------------
006000100119     d dataiso         s               d   datfmt(*iso)
006100100119     d dataeur         s               d   datfmt(*eur)
006200141023     D*-------------
006300141023     D LUOGO           s             20    DIM(100)
006400141023     D wrkLUOGO        s             20
006500940207     D*-------------
006600940211     D* COSTANTI
006700940211     D*-------------
006800090225     D SFLPAG0         C                   CONST(11)
006900100617     D SFLPAG1         C                   CONST(12)
007000111220     D*-------------
007100940506     D* Tasti di funzione
007200940506     D F01             C                   CONST(X'31')
007300940506     D F02             C                   CONST(X'32')
007400940506     D F03             C                   CONST(X'33')
007500940506     D F04             C                   CONST(X'34')
007600940506     D F05             C                   CONST(X'35')
007700940506     D F06             C                   CONST(X'36')
007800940506     D F07             C                   CONST(X'37')
007900940506     D F08             C                   CONST(X'38')
008000940506     D F09             C                   CONST(X'39')
008100940506     D F10             C                   CONST(X'3A')
008200940506     D F11             C                   CONST(X'3B')
008300940506     D F12             C                   CONST(X'3C')
008400940506     D F13             C                   CONST(X'B1')
008500940506     D F14             C                   CONST(X'B2')
008600940506     D F15             C                   CONST(X'B3')
008700940506     D F16             C                   CONST(X'B4')
008800940506     D F17             C                   CONST(X'B5')
008900940506     D F18             C                   CONST(X'B6')
009000940506     D F19             C                   CONST(X'B7')
009100940506     D F20             C                   CONST(X'B8')
009200940506     D F21             C                   CONST(X'B9')
009300940506     D F22             C                   CONST(X'BA')
009400940506     D F23             C                   CONST(X'BB')
009500940506     D F24             C                   CONST(X'BC')
009600940506     D ENTER           C                   CONST(X'F1')
009700940506     D ROLDWN          C                   CONST(X'F4')
009800940506     D ROLLUP          C                   CONST(X'F5')
009900940127     C*----------------------------------------------------*
010000940127     C*                MAIN LINE PROGRAM
010100940127     C*----------------------------------------------------*
010200940223     C* inizializzazione variabili
010300940223     C                   EXSR      INZVAR
010400940223     C*
010500940223     C     $FINE         DOWEQ     *OFF
010600090225     C     $GEST         CASEQ     'S0'          GESS0
010700940117     C                   END
010800940117     C                   END
010900940325     C* fine programma
011000940325     C                   SETON                                            LR
011100030113     C************************************************************
011200030113     C* INIZIALIZZAZIONE VARIABILI
011300030113     C************************************************************
011400030113     C     INZVAR        BEGSR
011500030113     C*
011600030113     C* Pulizia campi e indicatori
011700030113     C                   MOVE      *ALL'0'       IN4049           10
011800030113     C                   MOVEA     IN4049        *IN(40)
011900090225     C                   CLEAR                   S0OPZ
012000030113     C*
012100090225     C* Variabili per gestione videate
012200030113     C                   MOVE      *OFF          $FINE
012300030113     C                   MOVE      *OFF          $EFILE
012400030113     C                   MOVE      *OFF          $ESCI
012500030113     C                   MOVE      *OFF          $RCDOK
012600030113     C                   Z-ADD     0             $ULKS1            3 0
012700030113     C*
012800090225     C                   MOVE      *ON           $INZS0
012900030113     C* Variabili appoggio
013000030114     C                   Z-ADD     1             WPAG
013100100407     c* Video da cui partire
013200111220     C                   MOVE      'S0'          $GEST
013300100407     c*
013400030113     C                   ENDSR
013500090225     C************************************************************
013600090225     C* GESTIONE LISTA
013700090225     C************************************************************
013800090225     C     GESS0         BEGSR
013900090225     C*
014000090225     C* inizializzazione videata
014100090225     C     $INZS0        IFEQ      *ON
014200090225     C                   EXSR      INZS0
014300090225     C                   MOVE      *OFF          $INZS0
014400090225     C                   ENDIF
014500100325     C*
014600090225     C* emissione piede videata
014700090225     C                   WRITE     Z0
014800090225     C* Non ci sono records
014900090225     C     WMAX          IFEQ      0
015000090225     C                   WRITE     D0
015100090225     C                   Else
015200090225     C     Wsfl          IFgt      0
015300090225     C                   Z-ADD     wsfl          C0RCD
015400090225     C                   Else
015500090225     C     Wpag          IFgt      0
015600090225     C                   Z-ADD     wpag          C0RCD
015700090225     C                   EndIF
015800090225     C                   EndIF
015900090225     C                   ENDIF
016000090225     C*
016100090225     C*              *------------------*
016200090225     C                   EXFMT     C0
016300090225     C*              *------------------*
016400090225     C*
016500090225     C     C0NRR         IFNE      0
016600090225     C                   Z-ADD     C0NRR         WSFL
016700090225     C                   ENDIF
016800090225     C                   Z-ADD     SFLNRR        C0RCD
016900090225     C* Selezioni
0170000902251    C                   SELECT
017100141023     C* F12=Ritorno
017200141023     C     $TASTO        WHENEQ    F12
017300141023     C                   EXSR      F12S1
0174000902251O   C                   OTHER
017500090225     C* CONTROLLO DATI
017600090225     C                   EXSR      CTRC0
017700090225     C     *IN99         IFEQ      *OFF
017800090225     C                   EXSR      CTRS0
017900090225     C                   END
0180000902251-   C                   ENDSL
018100090225     C*
018200090225     C                   ENDSR
018300090225     C/EJECT
018400090225     C************************************************************
018500090225     C* INIZIALIZZAZIONE LISTA
018600090225     C************************************************************
018700090225     C     INZS0         BEGSR
018800100407     C*
018900090225     C* pulizia SFL
019000090225     C                   SETOFF                                         3031
019100090225     C                   WRITE     C0
019200090225     C                   SETON                                          31
019300090225     C*
019400090225     C* CARICAMENTO SFL totale
019500090225     C                   Z-ADD     0             S0NRR
019600090225     C                   Z-ADD     1             C0RCD
019700090225     C                   Z-ADD     0             WMAX
019800131011     C                   Z-ADD     1             wsfl
019900090225     C*
020000141023     C                   Z-ADD     0             piuuno            3 0
020100141023     c                   clear                   LUOGO
020200141023      *
020300141023     C* imposta in schiera tutti i LUOGHI presenti
020400141023     c                   open      edPFL01l
020500141023     c                   read      edPFL01l
020600141023     c                   dow       not %Eof(edPFL01l)
020700141023     c                   z-add     1             luo               3 0
020800141023     c                   if        pflLUOGO >= c0pos
020900141023     c     pflLUOGO      lookup    LUOGO(luo)                             21
021000141023     c                   if        not %Equal
021100141023     C                   eval        piuuno = piuuno + 1
021200141023     c                   eval      LUOGO(piuuno) =  pflLUOGO
021300141021     c                   end
021400141023     c                   endIF
021500141023     c                   read      edPFL01l
021600141023     c                   endDO
021700141023     c                   close     edPFL01l
021800141023     C*
021900141023     C* Dopo aver caricato tutti i luoghi
022000141023     C*   RIORDINA IN ORDINE ALFABETICO TUTTI I LUOGHI PRESENTI
022100141023     c                   sorta     LUOGO
022200141023     C*
022300090225     C*  Posizionamento all'inizio
022400141023     c                   movel(p)  c0pos         wrkLUOGO
022500111220     C*
022600100326     C                   IF        Posiziona = 'S'
022700141023     c                   z-add     1             luo               3 0
022800141023     c     wrkLUOGO      lookup    LUOGO(luo)                             21
022900141023     c                   if            %Equal
023000141023     c                   z-add     luo           zz                3 0
023100100326     C                   eval      Posiziona = *blank
023200100326     c                   else
023300141023     c                   z-add     1             zz                3 0
023400100326     c                   end
023500141023     c                   else
023600141023     c                   z-add     1             zz                3 0
023700141023     c                   endIF
023800111220     C*
023900141023    >C                   EXSR      READ_LUO
024000090225     C* Carico il SFL
024100090225     C                   EXSR      ROLS0
024200090225     C*
024300090225     C                   ENDSR
024400090225     C************************************************************
024500090225     C* CARICAMENTO PAGINA LISTA
024600090225     C************************************************************
024700090225     C     ROLS0         BEGSR
024800090225     C*
024900090225     C                   SETOFF                                       32
025000090225     C                   Z-ADD     0             Y
025100090225     C                   Z-ADD     WMAX          S0NRR
025200100119     C*
025300090225     C* Leggo dal file i segmenti presenti nell'archivio
0254000902251    C     $EFILE        DOWEQ     *OFF
025500100617     C*
025600111220     c                   clear                   s0opz
025700141023     C*
025800141023     c                   eval      s0LUOGO = wrkLUOGO
025900090225     C*
026000090225     C                   ADD       1             S0NRR
026100090225     C                   ADD       1             Y
026200090225     C                   WRITE     S0
026300100119     C*
026400141023    >C                   EXSR      READ_LUO
0265000902251-   C                   ENDDO
026600090225     C*
026700090225     C                   Z-ADD     S0NRR         WMAX                 30
026800090225     C*
026900090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
027000090225     C*
027100090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
027200090225     C                   MVR                     RESTO             3 0
027300090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0274000902251    C     RESTO         IFGT      0
027500090225     C                   ADD       1             C0RCD
0276000902251E   C                   ELSE
027700090225     C                   SUB       SFLPAG0       C0RCD
027800090225     C                   ADD       1             C0RCD
0279000902251-   C                   ENDIF
028000090225     C*
028100090225     C                   ENDSR
028200090225     C************************************************************
028300090225     C* LETTURA RCD
028400090225     C************************************************************
028500141023     C     READ_LUO      BEGSR
028600090225     C*
028700090225     C                   MOVEL     *OFF          $EFILE
028800090225     C                   MOVEL     *OFF          $RCDOK
028900090225     C*
0290000902251    C     $EFILE        DOUEQ     *ON
029100090225     C     $RCDOK        OREQ      *ON
029200090225     C*
029300141023     c                   if        zz > %Elem(LUOGO)
029400090225     C                   MOVEL     *ON           $EFILE
029500090225     C                   MOVE      $EFILE        *IN33
029600090225     c                   else
029700100322      **
029800141023     c                   eval      wrkLUOGO = LUOGO(zz)
029900141023      **
030000100322     c                   eXSr      Selez_RECord
030100141023      ** incrementa
030200141023     C                   eval      zz = zz + 1
030300100119     c                   end
030400090225     C*
0305000902251-   C                   ENDDO
030600090225     C*
030700090225     C                   ENDSR
030800090225     C************************************************************
030900090225     C* CONTROLLO TESTATA LISTA
031000090225     C************************************************************
031100100322     C     Selez_RECord  BEGSR
031200100407      *
031300100618     C                   MOVEL     *ON           $RCDOK
031400141023      *
031500141023     c                   if        wrkLUOGO = *blank
031600141023     C                   MOVEL     *OFF          $RCDOK
031700141023     c                   end
031800141021     C*
031900100322     C     End_filtro    ENDSR
032000100322     C************************************************************
032100100322     C* CONTROLLO TESTATA LISTA
032200100322     C************************************************************
032300100322     C     CTRC0         BEGSR
032400100322     C*
032500100326     C                   MOVE      *OFF          *IN99
032600100326     C*
032700100326     C                   clear                   posiziona         1
032800141021     c                   if        c0pos <> SAVc0pos
032900100326     C                   move      'S'           posiziona         1
033000100326     c                   eval      SAVc0pos = c0pos
033100100326     c                   move      *ON           $INZS0
033200100326     c                   end
033300090225     C*
033400090225     C                   ENDSR
033500090225     C************************************************************
033600090225     C* CONTROLLO OPZIONI LISTA
033700090225     C************************************************************
033800090225     C     CTRS0         BEGSR
033900090225     C*
034000090225     C                   MOVEL     *OFF          $ESCI
034100090225     C                   SETOFF                                       99
034200090225     C                   clear                   S0OPZ
034300090225     C*
034400090225     C* Leggo il sfl solo se ci sono rcd
0345000902251    C     WMAX          IFGT      0
034600090225     C                   READC     S0                                     21
034700090225     C*
034800090225     C* esce se fine sfl o errore che richiede l'uscita
0349000902252    C     *IN21         DOWEQ     *OFF
035000090225     C     $ESCI         ANDEQ     *OFF
035100090225     C                   Z-ADD     S0NRR         C0RCD
035200090225     C* ctrl su riga
035300090225     C                   EXSR      RECS0
035400090225     C* gestione opzioni
0355001112203    C     S0OPZ         IFNE      *blank
035600090225     C     *IN99         ANDEQ     *OFF
035700090225     C                   EXSR      OPZS0
0358000902253-   C                   ENDIF
035900111220      *
036000090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0361000902253    C     *IN99         IFEQ      *ON
036200090225     C     $ESCI         OREQ      *ON
036300090225     C                   MOVE      *ON           *IN32
036400090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
036500090225     C* la ripristinerò a conclusione del ciclo di READC
036600090225     C                   MOVE      *OFF          $INZS0
0367000902253-   C                   ENDIF
036800090225     C*
036900090225     C                   clear                   S0OPZ
037000090225     C                   UPDATE    S0
037100090225     C*
037200090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0373000902253    C     $ESCI         IFEQ      *OFF
037400090225     C                   READC     S0                                     21
037500111220     C*
037600090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0377000902254    C     *IN21         IFEQ      *ON
037800090225     C                   MOVE      WINZS0        $INZS0
037900090225     C* calcolo pagina a cui deve posizionarsi
038000090225     C                   EXSR      CLCPAG0
0381000902254-   C                   ENDIF
038200111220     C*
0383000902253-   C                   ENDIF
038400090225     C*
0385000902252-   C                   ENDDO
038600090225     C*
0387000902251-   C                   ENDIF
038800090225     C*
038900090225     C                   ENDSR
039000090225     C/EJECT
039100090225     C************************************************************
039200090225     C* CONTROLLO CAMPI I/O RIGA LISTA
039300090225     C************************************************************
039400090225     C     RECS0         BEGSR
039500090225     C*
039600090225     C* reset indicatori DSPATR
039700090225     C                   MOVE      *ALL'0'       IN4049           10
039800090225     C                   MOVEA     IN4049        *IN(40)
039900090225     C*
040000090225     C                   ENDSR
040100090225     C************************************************************
040200090225     C* GESTIONE OPZIONI LISTA
040300090225     C************************************************************
040400090225     C     OPZS0         BEGSR
040500090225     C*
040600141024     c                   clear                   parLuogo
040700141021      * selezionato un codice da restituire al chiamante
0408001410213    C     S0OPZ         IFeq      '1'
040900141024     c                   eval      parLuogo = s0luogo
041000141021     C                   MOVE      'S1'          $GEST
041100141021     C                   MOVEL     *ON           $ESCI
041200141021     C                   MOVEL     *ON           $fine
041300141021     C                   END
041400090225     C*
041500090225     C                   ENDSR
041600090225     C/EJECT
041700940224     C************************************************************
041800940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
041900940224     C************************************************************
042000090225     C     CLCPAG0       BEGSR
042100940224     C* Input :
042200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
042300940224     C*          cursore
042400940224     C* - SFLPAG = numero rcd per pagina sfl
042500940224     C* Output :
042600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
042700940224     C*
042800090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
042900940224     C                   MVR                     RESTO             3 0
043000940224     C     RESTO         IFGT      0
043100940224     C                   ADD       1             PAGINE
043200940224     C                   ENDIF
043300090226     c                   if        pagine > 1
043400090225     C     PAGINE        MULT      SFLPAG0       WPAG
043500090226     C                   END
043600940224     C*
043700940224     C                   ENDSR
043800940309     C************************************************************
043900141023     C* GESTIONE F12 VIDEO S1
044000940309     C************************************************************
044100141023     C     F12S1         BEGSR
044200940309     C*
044300940309     C                   MOVE      *ON           $FINE
044400141023     C                   MOVE      *ON           $ESCI
044500141024     c                   clear                   parLuogo
044600940325     C* fine programma
044700940309     C                   ENDSR
044800940309     C/EJECT
044900940131     C************************************************************
045000940131     C* OPERAZIONI INIZIALI
045100940131     C************************************************************
045200940131     C     *INZSR        BEGSR
045300030113     C*
045400030113     C* Reperimento parametri
045500030113     C     *ENTRY        PLIST
045600141024     C                   PARM                    ParLUOGO         20
045700141024     C*
045800030113     C* Variabili per gestione videate
045900030113     C                   MOVE      *BLANK        $GEST             2
046000030113     C                   MOVE      *BLANK        $FINE             1
046100090225     C                   MOVE      *BLANK        $INZS0            1
046200030113     C                   MOVE      *BLANK        $EFILE            1
046300030113     C                   MOVE      *BLANK        $ESCI             1
046400030113     C                   MOVE      *BLANK        $RCDOK            1
046500030113     C* Indici
046600030113     C                   Z-ADD     0             X                 3 0
046700030113     C                   Z-ADD     0             Y                 3 0
046800940506     C*
046900940506     C* Reperimento tasti di funzione
047000100326     C                   clear                   c0pos
047100100326     C                   clear                   SAVc0pos
047200940127     C*
047300940117     C                   ENDSR
047400100319     C************************************************************
