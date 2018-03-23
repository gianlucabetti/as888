000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200141023      *-----------------------------------------------------------------------
000300141023      *  ELENCO DI RICERCA : si seleziona un  codice LUOGO già inserito
000400141023      *            per evitare di scriverlo diversamente
000500141023      *            OPPURE si deciderà VOLONTARIAMENTE di scriverlo diverso.
000600141023      *-----------------------------------------------------------------------
000700040930      *  11           x selezione di un codice da ripassare al pgm chiamante
000800940307      *  21           GENERICO OPERAZIONI I/O
000900940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001000940224      *  30           SFLDSP
001100940224      * N31           SFLCLR
001200940224      *  31           SFLDSPCTL
001300940224      *  32           SFLNXTCHG
001400940224      *  33           SFLEND
001500940224      *  39           OF PRTF
001600940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001700940608      *  Specificare l'uso dei singoli indicatori
001800940224      *  50 <---> 98  ERRORI SU VIDEO
001900940608      *  Specificare l'uso dei singoli indicatori
002000940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002100940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002200940224      *  99           INDIC. GENERALE DI ERRORE
002300940128     F*----------------------------------------------------*
002400141023     FEDPFL01L  IF   E           K DISK    usropn
002500141017     F                                     INFDS(PFLDS)
002600141023     FTRTC9PLD3 CF   E             WORKSTN
002700090225     F                                     SFILE(S0:S0NRR)
002800940201     F                                     INFDS(DSFMT)
002900141017     C*----------------------------------------------------*
003000141017     d PFLds           ds
003100141017     d  PFL_nrr              397    400b 0
003200940211     D*-------------
003300940211     D DSFMT           DS           512
003400940506     D  $TASTO               369    369
003500940211     D  NRG                  370    370
003600940211     D  NCL                  371    371
003700940211     D  SFLNRR               378    379B 0
003800940207     D*-------------
003900940207     D* Nome PGM a video
004000940207     D                 DS
004100940207     D  PROGR                  1     10
004200940207     D  ASTER1                 1      1    INZ('*')
004300940207     D  SIGLA                  2      9
004400940207     D  ASTER2                10     10    INZ('*')
004500940127     D*-------------
004600940127     D* Reperimento nome PGM
004700940127     D STATUS         SDS           333
004800940127     D  DSPGM            *PROC
004900030113     D*-------------
005000111220     C* Variabili appoggio sempre presenti
005100111221$003 D S0NRR           S                   Like(C0rcd)
005200111221$003 D WSfl            S                   Like(C0nrr)
005300111221$003 D Wmax            S                   Like(C0rcd)
005400111221$003 D Wpag            S                   Like(C0rcd)
005500090225$003 D Winzs0          S                   Like($inzs0)
005600111220     D*-------------
005700100326$003 D savC0pos        S                   Like(C0POS)
005800090225     D*-------------
005900100119     d dataiso         s               d   datfmt(*iso)
006000100119     d dataeur         s               d   datfmt(*eur)
006100141023     D*-------------
006200141023     D LUOGO           s             20    DIM(100)
006300141023     D wrkLUOGO        s             20
006400940207     D*-------------
006500940211     D* COSTANTI
006600940211     D*-------------
006700090225     D SFLPAG0         C                   CONST(11)
006800100617     D SFLPAG1         C                   CONST(12)
006900111220     D*-------------
007000940506     D* Tasti di funzione
007100940506     D F01             C                   CONST(X'31')
007200940506     D F02             C                   CONST(X'32')
007300940506     D F03             C                   CONST(X'33')
007400940506     D F04             C                   CONST(X'34')
007500940506     D F05             C                   CONST(X'35')
007600940506     D F06             C                   CONST(X'36')
007700940506     D F07             C                   CONST(X'37')
007800940506     D F08             C                   CONST(X'38')
007900940506     D F09             C                   CONST(X'39')
008000940506     D F10             C                   CONST(X'3A')
008100940506     D F11             C                   CONST(X'3B')
008200940506     D F12             C                   CONST(X'3C')
008300940506     D F13             C                   CONST(X'B1')
008400940506     D F14             C                   CONST(X'B2')
008500940506     D F15             C                   CONST(X'B3')
008600940506     D F16             C                   CONST(X'B4')
008700940506     D F17             C                   CONST(X'B5')
008800940506     D F18             C                   CONST(X'B6')
008900940506     D F19             C                   CONST(X'B7')
009000940506     D F20             C                   CONST(X'B8')
009100940506     D F21             C                   CONST(X'B9')
009200940506     D F22             C                   CONST(X'BA')
009300940506     D F23             C                   CONST(X'BB')
009400940506     D F24             C                   CONST(X'BC')
009500940506     D ENTER           C                   CONST(X'F1')
009600940506     D ROLDWN          C                   CONST(X'F4')
009700940506     D ROLLUP          C                   CONST(X'F5')
009800940127     C*----------------------------------------------------*
009900940127     C*                MAIN LINE PROGRAM
010000940127     C*----------------------------------------------------*
010100940223     C* inizializzazione variabili
010200940223     C                   EXSR      INZVAR
010300940223     C*
010400940223     C     $FINE         DOWEQ     *OFF
010500090225     C     $GEST         CASEQ     'S0'          GESS0
010600940117     C                   END
010700940117     C                   END
010800940325     C* fine programma
010900940325     C                   SETON                                            LR
011000030113     C************************************************************
011100030113     C* INIZIALIZZAZIONE VARIABILI
011200030113     C************************************************************
011300030113     C     INZVAR        BEGSR
011400030113     C*
011500030113     C* Pulizia campi e indicatori
011600030113     C                   MOVE      *ALL'0'       IN4049           10
011700030113     C                   MOVEA     IN4049        *IN(40)
011800090225     C                   CLEAR                   S0OPZ
011900030113     C*
012000090225     C* Variabili per gestione videate
012100030113     C                   MOVE      *OFF          $FINE
012200030113     C                   MOVE      *OFF          $EFILE
012300030113     C                   MOVE      *OFF          $ESCI
012400030113     C                   MOVE      *OFF          $RCDOK
012500030113     C                   Z-ADD     0             $ULKS1            3 0
012600030113     C*
012700090225     C                   MOVE      *ON           $INZS0
012800030113     C* Variabili appoggio
012900030114     C                   Z-ADD     1             WPAG
013000100407     c* Video da cui partire
013100111220     C                   MOVE      'S0'          $GEST
013200100407     c*
013300030113     C                   ENDSR
013400090225     C************************************************************
013500090225     C* GESTIONE LISTA
013600090225     C************************************************************
013700090225     C     GESS0         BEGSR
013800090225     C*
013900090225     C* inizializzazione videata
014000090225     C     $INZS0        IFEQ      *ON
014100090225     C                   EXSR      INZS0
014200090225     C                   MOVE      *OFF          $INZS0
014300090225     C                   ENDIF
014400100325     C*
014500090225     C* emissione piede videata
014600090225     C                   WRITE     Z0
014700090225     C* Non ci sono records
014800090225     C     WMAX          IFEQ      0
014900090225     C                   WRITE     D0
015000090225     C                   Else
015100090225     C     Wsfl          IFgt      0
015200090225     C                   Z-ADD     wsfl          C0RCD
015300090225     C                   Else
015400090225     C     Wpag          IFgt      0
015500090225     C                   Z-ADD     wpag          C0RCD
015600090225     C                   EndIF
015700090225     C                   EndIF
015800090225     C                   ENDIF
015900090225     C*
016000090225     C*              *------------------*
016100090225     C                   EXFMT     C0
016200090225     C*              *------------------*
016300090225     C*
016400090225     C     C0NRR         IFNE      0
016500090225     C                   Z-ADD     C0NRR         WSFL
016600090225     C                   ENDIF
016700090225     C                   Z-ADD     SFLNRR        C0RCD
016800090225     C* Selezioni
0169000902251    C                   SELECT
017000141023     C* F12=Ritorno
017100141023     C     $TASTO        WHENEQ    F12
017200141023     C                   EXSR      F12S1
0173000902251O   C                   OTHER
017400090225     C* CONTROLLO DATI
017500090225     C                   EXSR      CTRC0
017600090225     C     *IN99         IFEQ      *OFF
017700090225     C                   EXSR      CTRS0
017800090225     C                   END
0179000902251-   C                   ENDSL
018000090225     C*
018100090225     C                   ENDSR
018200090225     C/EJECT
018300090225     C************************************************************
018400090225     C* INIZIALIZZAZIONE LISTA
018500090225     C************************************************************
018600090225     C     INZS0         BEGSR
018700100407     C*
018800090225     C* pulizia SFL
018900090225     C                   SETOFF                                         3031
019000090225     C                   WRITE     C0
019100090225     C                   SETON                                          31
019200090225     C*
019300090225     C* CARICAMENTO SFL totale
019400090225     C                   Z-ADD     0             S0NRR
019500090225     C                   Z-ADD     1             C0RCD
019600090225     C                   Z-ADD     0             WMAX
019700131011     C                   Z-ADD     1             wsfl
019800090225     C*
019900141023     C                   Z-ADD     0             piuuno            3 0
020000141023     c                   clear                   LUOGO
020100141023      *
020200141023     C* imposta in schiera tutti i LUOGHI presenti
020300141023     c                   open      edPFL01l
020400141023     c                   read      edPFL01l
020500141023     c                   dow       not %Eof(edPFL01l)
020600141023     c                   z-add     1             luo               3 0
020700141023     c                   if        pflLUOGO >= c0pos
020800141023     c     pflLUOGO      lookup    LUOGO(luo)                             21
020900141023     c                   if        not %Equal
021000141023     C                   eval        piuuno = piuuno + 1
021100141023     c                   eval      LUOGO(piuuno) =  pflLUOGO
021200141021     c                   end
021300141023     c                   endIF
021400141023     c                   read      edPFL01l
021500141023     c                   endDO
021600141023     c                   close     edPFL01l
021700141023     C*
021800141023     C* Dopo aver caricato tutti i luoghi
021900141023     C*   RIORDINA IN ORDINE ALFABETICO TUTTI I LUOGHI PRESENTI
022000141023     c                   sorta     LUOGO
022100141023     C*
022200090225     C*  Posizionamento all'inizio
022300141023     c                   movel(p)  c0pos         wrkLUOGO
022400111220     C*
022500100326     C                   IF        Posiziona = 'S'
022600141023     c                   z-add     1             luo               3 0
022700141023     c     wrkLUOGO      lookup    LUOGO(luo)                             21
022800141023     c                   if            %Equal
022900141023     c                   z-add     luo           zz                3 0
023000100326     C                   eval      Posiziona = *blank
023100100326     c                   else
023200141023     c                   z-add     1             zz                3 0
023300100326     c                   end
023400141023     c                   else
023500141023     c                   z-add     1             zz                3 0
023600141023     c                   endIF
023700111220     C*
023800141023    >C                   EXSR      READ_LUO
023900090225     C* Carico il SFL
024000090225     C                   EXSR      ROLS0
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
025600141023     C*
025700141023     c                   eval      s0LUOGO = wrkLUOGO
025800090225     C*
025900090225     C                   ADD       1             S0NRR
026000090225     C                   ADD       1             Y
026100090225     C                   WRITE     S0
026200100119     C*
026300141023    >C                   EXSR      READ_LUO
0264000902251-   C                   ENDDO
026500090225     C*
026600090225     C                   Z-ADD     S0NRR         WMAX                 30
026700090225     C*
026800090225     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
026900090225     C*
027000090225     C     S0NRR         DIV       SFLPAG0       PAGINE            4 0
027100090225     C                   MVR                     RESTO             3 0
027200090225     C     PAGINE        MULT      SFLPAG0       C0RCD
0273000902251    C     RESTO         IFGT      0
027400090225     C                   ADD       1             C0RCD
0275000902251E   C                   ELSE
027600090225     C                   SUB       SFLPAG0       C0RCD
027700090225     C                   ADD       1             C0RCD
0278000902251-   C                   ENDIF
027900090225     C*
028000090225     C                   ENDSR
028100090225     C************************************************************
028200090225     C* LETTURA RCD
028300090225     C************************************************************
028400141023     C     READ_LUO      BEGSR
028500090225     C*
028600090225     C                   MOVEL     *OFF          $EFILE
028700090225     C                   MOVEL     *OFF          $RCDOK
028800090225     C*
0289000902251    C     $EFILE        DOUEQ     *ON
029000090225     C     $RCDOK        OREQ      *ON
029100090225     C*
029200141023     c                   if        zz > %Elem(LUOGO)
029300090225     C                   MOVEL     *ON           $EFILE
029400090225     C                   MOVE      $EFILE        *IN33
029500090225     c                   else
029600100322      **
029700141023     c                   eval      wrkLUOGO = LUOGO(zz)
029800141023      **
029900100322     c                   eXSr      Selez_RECord
030000141023      ** incrementa
030100141023     C                   eval      zz = zz + 1
030200100119     c                   end
030300090225     C*
0304000902251-   C                   ENDDO
030500090225     C*
030600090225     C                   ENDSR
030700090225     C************************************************************
030800090225     C* CONTROLLO TESTATA LISTA
030900090225     C************************************************************
031000100322     C     Selez_RECord  BEGSR
031100100407      *
031200100618     C                   MOVEL     *ON           $RCDOK
031300141023      *
031400141023     c                   if        wrkLUOGO = *blank
031500141023     C                   MOVEL     *OFF          $RCDOK
031600141023     c                   end
031700141021     C*
031800100322     C     End_filtro    ENDSR
031900100322     C************************************************************
032000100322     C* CONTROLLO TESTATA LISTA
032100100322     C************************************************************
032200100322     C     CTRC0         BEGSR
032300100322     C*
032400100326     C                   MOVE      *OFF          *IN99
032500100326     C*
032600100326     C                   clear                   posiziona         1
032700141021     c                   if        c0pos <> SAVc0pos
032800100326     C                   move      'S'           posiziona         1
032900100326     c                   eval      SAVc0pos = c0pos
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
034200090225     C*
034300090225     C* Leggo il sfl solo se ci sono rcd
0344000902251    C     WMAX          IFGT      0
034500090225     C                   READC     S0                                     21
034600090225     C*
034700090225     C* esce se fine sfl o errore che richiede l'uscita
0348000902252    C     *IN21         DOWEQ     *OFF
034900090225     C     $ESCI         ANDEQ     *OFF
035000090225     C                   Z-ADD     S0NRR         C0RCD
035100090225     C* ctrl su riga
035200090225     C                   EXSR      RECS0
035300090225     C* gestione opzioni
0354001112203    C     S0OPZ         IFNE      *blank
035500090225     C     *IN99         ANDEQ     *OFF
035600090225     C                   EXSR      OPZS0
0357000902253-   C                   ENDIF
035800111220      *
035900090225     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0360000902253    C     *IN99         IFEQ      *ON
036100090225     C     $ESCI         OREQ      *ON
036200090225     C                   MOVE      *ON           *IN32
036300090225     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
036400090225     C* la ripristinerò a conclusione del ciclo di READC
036500090225     C                   MOVE      *OFF          $INZS0
0366000902253-   C                   ENDIF
036700090225     C*
036800090225     C                   clear                   S0OPZ
036900090225     C                   UPDATE    S0
037000090225     C*
037100090225     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0372000902253    C     $ESCI         IFEQ      *OFF
037300090225     C                   READC     S0                                     21
037400111220     C*
037500090225     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0376000902254    C     *IN21         IFEQ      *ON
037700090225     C                   MOVE      WINZS0        $INZS0
037800090225     C* calcolo pagina a cui deve posizionarsi
037900090225     C                   EXSR      CLCPAG0
0380000902254-   C                   ENDIF
038100111220     C*
0382000902253-   C                   ENDIF
038300090225     C*
0384000902252-   C                   ENDDO
038500090225     C*
0386000902251-   C                   ENDIF
038700090225     C*
038800090225     C                   ENDSR
038900090225     C/EJECT
039000090225     C************************************************************
039100090225     C* CONTROLLO CAMPI I/O RIGA LISTA
039200090225     C************************************************************
039300090225     C     RECS0         BEGSR
039400090225     C*
039500090225     C* reset indicatori DSPATR
039600090225     C                   MOVE      *ALL'0'       IN4049           10
039700090225     C                   MOVEA     IN4049        *IN(40)
039800090225     C*
039900090225     C                   ENDSR
040000090225     C************************************************************
040100090225     C* GESTIONE OPZIONI LISTA
040200090225     C************************************************************
040300090225     C     OPZS0         BEGSR
040400090225     C*
040500141024     c                   clear                   parLuogo
040600141021      * selezionato un codice da restituire al chiamante
0407001410213    C     S0OPZ         IFeq      '1'
040800141024     c                   eval      parLuogo = s0luogo
040900141021     C                   MOVE      'S1'          $GEST
041000141021     C                   MOVEL     *ON           $ESCI
041100141021     C                   MOVEL     *ON           $fine
041200141021     C                   END
041300090225     C*
041400090225     C                   ENDSR
041500090225     C/EJECT
041600940224     C************************************************************
041700940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
041800940224     C************************************************************
041900090225     C     CLCPAG0       BEGSR
042000940224     C* Input :
042100940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
042200940224     C*          cursore
042300940224     C* - SFLPAG = numero rcd per pagina sfl
042400940224     C* Output :
042500940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
042600940224     C*
042700090225     C     WSFL          DIV       SFLPAG0       PAGINE            4 0
042800940224     C                   MVR                     RESTO             3 0
042900940224     C     RESTO         IFGT      0
043000940224     C                   ADD       1             PAGINE
043100940224     C                   ENDIF
043200090226     c                   if        pagine > 1
043300090225     C     PAGINE        MULT      SFLPAG0       WPAG
043400090226     C                   END
043500940224     C*
043600940224     C                   ENDSR
043700940309     C************************************************************
043800141023     C* GESTIONE F12 VIDEO S1
043900940309     C************************************************************
044000141023     C     F12S1         BEGSR
044100940309     C*
044200940309     C                   MOVE      *ON           $FINE
044300141023     C                   MOVE      *ON           $ESCI
044400141024     c                   clear                   parLuogo
044500940325     C* fine programma
044600940309     C                   ENDSR
044700940309     C/EJECT
044800940131     C************************************************************
044900940131     C* OPERAZIONI INIZIALI
045000940131     C************************************************************
045100940131     C     *INZSR        BEGSR
045200030113     C*
045300030113     C* Reperimento parametri
045400030113     C     *ENTRY        PLIST
045500141024     C                   PARM                    ParLUOGO         20
045600141024     C*
045700030113     C* Variabili per gestione videate
045800030113     C                   MOVE      *BLANK        $GEST             2
045900030113     C                   MOVE      *BLANK        $FINE             1
046000090225     C                   MOVE      *BLANK        $INZS0            1
046100030113     C                   MOVE      *BLANK        $EFILE            1
046200030113     C                   MOVE      *BLANK        $ESCI             1
046300030113     C                   MOVE      *BLANK        $RCDOK            1
046400030113     C* Indici
046500030113     C                   Z-ADD     0             X                 3 0
046600030113     C                   Z-ADD     0             Y                 3 0
046700940506     C*
046800940506     C* Reperimento tasti di funzione
046900100326     C                   clear                   c0pos
047000100326     C                   clear                   SAVc0pos
047100940127     C*
047200940117     C                   ENDSR
047300100319     C************************************************************
