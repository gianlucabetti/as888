000100141127     H*
000200141127     H*
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *
000500040930      *  11           x selezione di un codice da ripassare al pgm chiamante
000600940307      *  21           GENERICO OPERAZIONI I/O
000700940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000800940224      *  30           SFLDSP
000900940224      * N31           SFLCLR
001000940224      *  31           SFLDSPCTL
001100940224      *  32           SFLNXTCHG
001200940224      *  33           SFLEND
001300940224      *  39           OF PRTF
001400940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001500940608      *  Specificare l'uso dei singoli indicatori
001600940224      *  50 <---> 98  ERRORI SU VIDEO
001700940608      *  Specificare l'uso dei singoli indicatori
001800940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001900940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002000940224      *  99           INDIC. GENERALE DI ERRORE
002100940128     F*----------------------------------------------------*
002200110926     Ftntbe01L  IF   E           K DISK
002300110926     Fazorg01L  IF   E           K DISK
002400160412     FTntbSCSd  CF   E             WORKSTN
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
003700160412     D  XTAKEY                 6      7
003800160412     D  XTAKE2                 8     22
003900940211     D*-------------
004000940211     D DSFMT           DS           512
004100940506     D  $TASTO               369    369
004200940211     D  NRG                  370    370
004300940211     D  NCL                  371    371
004400940211     D  SFLNRR               378    379B 0
004500940207     D*-------------
004600940207     D* Nome PGM a video
004700940207     D                 DS
004800940207     D  PROGR                  1     10
004900940207     D  ASTER1                 1      1    INZ('*')
005000940207     D  SIGLA                  2      9
005100940207     D  ASTER2                10     10    INZ('*')
005200940127     D*-------------
005300940127     D* Reperimento nome PGM
005400940127     D STATUS         SDS           333
005500940127     D  DSPGM            *PROC
005600940607     D* descrizione tabella
005700160412$003 D EXTDS         E DS                  EXTNAME(tntbSCSds)
005800030113     D*-------------
005900030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006000030113$003 D S1NRR           S                   Like(C1rcd)
006100030113$003 D WSfl            S                   Like(C1nrr)
006200030113$003 D Wmax            S                   Like(C1rcd)
006300030113$003 D Wpag            S                   Like(C1rcd)
006400030113$003 D Winzs1          S                   Like($inzs1)
006500040930$003 D x1cod           S                   Like(s1cod)
006600160412$003 D x1cod2          S                   Like(h1cod2)
006700160412     D KCOD            S                   LIKE(TBECOD) inz('SCS')
006800110926     D KKE1            S                   LIKE(TBEKE1)
006900160412     D KKE2            S                   LIKE(TBEKE2)
007000940207     D*-------------
007100940211     D* COSTANTI
007200940211     D*-------------
007300030113     D SFLPAG          C                   CONST(11)
007400940314     D* dimensione della schiera $MS1
007500940506     D*
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
010400940207     I*-------------
010500940607     IS1
010600160412$015 I              S1DES                       desSTATUS
010700940127     C*----------------------------------------------------*
010800940127     C*                MAIN LINE PROGRAM
010900940127     C*----------------------------------------------------*
011000940223     C* inizializzazione variabili
011100940223     C                   EXSR      INZVAR
011200940223     C*
011300940223     C     $FINE         DOWEQ     *OFF
011400940131     C     $GEST         CASEQ     'S1'          GESS1
011500940117     C                   END
011600940117     C                   END
011700940325     C* fine programma
011800940325     C                   SETON                                            LR
011900030113     C************************************************************
012000030113     C* INIZIALIZZAZIONE VARIABILI
012100030113     C************************************************************
012200030113     C     INZVAR        BEGSR
012300030113     C*
012400030113     C* Pulizia campi e indicatori
012500030113     C                   MOVE      *ALL'0'       IN4049           10
012600030113     C                   MOVEA     IN4049        *IN(40)
012700030113     C                   CLEAR                   S1OPZ
012800030113     C* Variabili per gestione videate
012900030113     C*
013000030113     C                   MOVE      *OFF          $FINE
013100030113     C                   MOVE      *OFF          $INZS1
013200030113     C                   MOVE      *OFF          $EFILE
013300030113     C                   MOVE      *OFF          $ESCI
013400030113     C                   MOVE      *OFF          $RCDOK
013500030113     C                   Z-ADD     0             $ULKS1            3 0
013600030113     C*
013700030113     C                   MOVE      *ON           $INZS1
013800030113     C                   MOVE      'S1'          $GEST
013900030113     C*
014000030113     C* Variabili appoggio
014100030114     C                   Z-ADD     1             WPAG
014200030113     c*
014300030113     C                   ENDSR
014400940127     C************************************************************
014500940131     C* GESTIONE LISTA
014600940127     C************************************************************
014700940127     C     GESS1         BEGSR
014800030113     C*
014900940223     C* inizializzazione videata
015000940223     C     $INZS1        IFEQ      *ON
015100940127     C                   EXSR      INZS1
015200940223     C                   MOVE      *OFF          $INZS1
015300940127     C                   ENDIF
015400030113     C*
015500030113     C* emissione piede videata
015600030113     C                   WRITE     Z1
015700030113     C* Non ci sono records
015800940223     C     WMAX          IFEQ      0
015900940607     C                   WRITE     D1
016000030114     C                   Else
016100120412     C     Wsfl          IFgt      Wmax
016200120412     c                   z-add     wmax          Wsfl
016300120412     c                   end
016400030114     C     Wsfl          IFgt      0
016500030114     C                   Z-ADD     wsfl          C1RCD
016600030114     C                   Else
016700030114     C     Wpag          IFgt      0
016800030114     C                   Z-ADD     wpag          C1RCD
016900030114     C                   EndIF
017000030114     C                   EndIF
017100030114     C                   ENDIF
017200940127     C*
017300030113     C*              *------------------*
017400940607     C                   EXFMT     C1
017500030113     C*              *------------------*
017600030113     C*
017700940204     C     C1NRR         IFNE      0
017800940204     C                   Z-ADD     C1NRR         WSFL
017900940204     C                   ENDIF
018000940127     C                   Z-ADD     SFLNRR        C1RCD
018100030113     C* Selezioni
0182009401271    C                   SELECT
018300940127     C* F3=Fine
018400940506     C     $TASTO        WHENEQ    F03
018500940309     C                   EXSR      F03S1
018600940131     C* F10=Immissione
018700940506     C     $TASTO        WHENEQ    F10
018800940309     C                   EXSR      F10S1
0189009401271O   C                   OTHER
019000940127     C* CONTROLLO DATI
019100940131     C                   EXSR      CTRC1
019200940201     C     *IN99         IFEQ      *OFF
019300940131     C                   EXSR      CTRS1
019400940131     C                   END
0195009401271-   C                   ENDSL
019600940127     C*
019700940127     C                   ENDSR
019800940224     C/EJECT
019900940127     C************************************************************
020000940131     C* INIZIALIZZAZIONE LISTA
020100940127     C************************************************************
020200940127     C     INZS1         BEGSR
020300940302     C* pulizia SFL
020400940128     C                   SETOFF                                         3031
020500940607     C                   WRITE     C1
020600940128     C                   SETON                                          31
020700940128     C*
020800030113     C* CARICAMENTO SFL totale
020900940201     C                   Z-ADD     0             S1NRR
021000030113     C                   Z-ADD     1             C1RCD
021100940128     C                   Z-ADD     0             WMAX
021200120412     C                   Z-ADD     1             WPAG
021300940224     C*
021400940224     C* Posizionamento su file pilota
021500110926     c     kTBL          setll     tntbe01l
021600940608    >C                   EXSR      REDANA
021700030113     C* Carico il SFL
021800940127     C                   EXSR      ROLS1
021900030113     C*
022000030114     c                   if        xtaopr <> '1'
022100030114     C                   Z-ADD     1             WPAG
022200030114     c                   end
022300940127     C*
022400940127     C                   ENDSR
022500940127     C************************************************************
022600940131     C* CARICAMENTO PAGINA LISTA
022700940127     C************************************************************
022800940127     C     ROLS1         BEGSR
022900940127     C*
023000940128     C                   SETOFF                                       32
023100940223     C                   Z-ADD     0             Y
023200940127     C                   Z-ADD     WMAX          S1NRR
023300940127     C*
023400940127     C* Leggo dal file anagrafico per caricare la lista
0235009401311    C     $EFILE        DOWEQ     *OFF
023600940127     C*
023700030113     c                   clear                   s1opz
023800160412     c                   clear                   desSTATUS
023900120412     c                   clear                   Obbiettivo        3
024000160412     c                   clear                   s1cod
024100160412     c                   clear                   h1cod2
024200160412     c                   clear                   desSTATUS
024300160413     c                   movel     tbeke1        s1cod
024400160412     c                   movel     tbeke2        h1cod2
024500120412      **
024600110926     c                   movel     tbeuni        extds
024700160412     c                   eval      desSTATUS = %trim(tbeke2) + ' - ' + SCSDESC
024800160413     c                   eval      s1SCANS   = scsSOST
024900030113     C*
025000030115     c                   clear                   S1Ann
025100940127     C*
025200940127     C                   ADD       1             S1NRR
025300940127     C                   ADD       1             Y
025400940127     C*
025500940607     C                   WRITE     S1
025600940131     C*
025700940316    >C                   EXSR      REDANA
025800940128     C*
0259009401271-   C                   ENDDO
026000940127     C*
026100940223     C                   Z-ADD     S1NRR         WMAX                 30
026200940127     C*
026300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
026400940127     C*
026500030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
026600940127     C                   MVR                     RESTO             3 0
026700030114     C     PAGINE        MULT      SFLPAG        C1RCD
0268000301141    C     RESTO         IFGT      0
026900030114     C                   ADD       1             C1RCD
0270000301141E   C                   ELSE
027100030114     C                   SUB       SFLPAG        C1RCD
027200030114     C                   ADD       1             C1RCD
0273000301141-   C                   ENDIF
027400940128     C*
027500940127     C                   ENDSR
027600940128     C************************************************************
027700940131     C* LETTURA RCD ARCHIVIO PILOTA
027800940128     C************************************************************
027900940607     C     REDANA        BEGSR
028000940128     C*
028100940131     C                   MOVEL     *OFF          $EFILE
028200940131     C                   MOVEL     *OFF          $RCDOK
028300940131     C*
0284009401311    C     $EFILE        DOUEQ     *ON
028500940131     C     $RCDOK        OREQ      *ON
028600940131     C*
028700110926     c     kTBL          Reade     tntbe01l
028800110926     c                   if        %eof(tntbe01l)
028900030113     C                   MOVEL     *ON           $EFILE
029000030113     C                   MOVE      $EFILE        *IN33
029100030113     c                   else
029200030113     C                   MOVE      *ON           $RCDOK
029300030113     c                   end
029400940131     C*
0295009401311-   C                   ENDDO
029600940131     C*
029700940131     C                   ENDSR
029800940224     C************************************************************
029900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
030000940224     C************************************************************
030100940224     C     CLCPAG        BEGSR
030200940224     C* Input :
030300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
030400940224     C*          cursore
030500940224     C* - SFLPAG = numero rcd per pagina sfl
030600940224     C* Output :
030700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
030800940224     C*
030900120412     c                   if        WSFL >0
031000940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
031100940224     C                   MVR                     RESTO             3 0
031200940224     C     RESTO         IFGT      0
031300940224     C                   ADD       1             PAGINE
031400940224     C                   ENDIF
031500940224     C     PAGINE        MULT      SFLPAG        WPAG
031600120412     c                   else
031700120412     C                   Z-ADD     1             WPAG
031800120412     c                   end
031900940224     C*
032000940224     C                   ENDSR
032100940309     C************************************************************
032200940309     C* GESTIONE F03 VIDEO S1
032300940309     C************************************************************
032400940309     C     F03S1         BEGSR
032500940309     C*
032600940309     C                   MOVE      *ON           $FINE
032700940325     C* fine programma
032800940309     C                   ENDSR
032900940309     C/EJECT
033000940309     C************************************************************
033100940309     C* GESTIONE F10 VIDEO S1
033200940314     c* AGGIUNTA RECORD
033300940309     C************************************************************
033400940309     C     F10S1         BEGSR
033500940309     C*
033600030113     C                   RESET                   tabds
033700030113     C                   MOVEL     '01'          xtaopz
033800030113     C                   MOVE      *ZERO         xtaret
033900030113     C                   MOVE      *ZERO         xtaopr
034000030114     C                   MOVEl     *blank        xtakey
034100160412     C                   MOVEl     *blank        xtake2
034200030113     C                   MOVE      *BLANKS       KPJBU
034300030113     C                   MOVEL     tabds         KPJBU
034400160412$004 C                   CALL      'TNTBSCSR2'
034500030113     C                   PARM                    KPJBA
034600030114     C                   MOVEL     KPJBU         tabds
034700030113      *
034800940309     C* ritorno da PGM gestione
034900940309     C                   EXSR      GESRET
035000940309     C     WINZS1        IFEQ      *ON
035100940309     C                   MOVE      *ON           $INZS1
035200940309     C* se esistevano già righe sul sfl
035300940309     C* calcolo pagina a cui deve posizionarsi
035400940309     C     WSFL          IFGT      0
035500940309     C                   EXSR      CLCPAG
035600940309     C* altrimenti carico solo la 1a pagina
035700940309     C                   ELSE
035800940309     C                   Z-ADD     SFLPAG        WPAG
035900940309     C                   ENDIF
036000940309     C                   ENDIF
036100940309     C*
036200940309     C                   ENDSR
036300940128     C************************************************************
036400940131     C* CONTROLLO TESTATA LISTA
036500940128     C************************************************************
036600940131     C     CTRC1         BEGSR
036700940128     C*
036800940201     C                   MOVE      *OFF          *IN99
036900940131     C*
037000940202     C                   ENDSR
037100940131     C************************************************************
037200940131     C* CONTROLLO OPZIONI LISTA
037300940131     C************************************************************
037400940131     C     CTRS1         BEGSR
037500940131     C*
037600940202     C                   MOVEL     *OFF          $ESCI
037700940201     C                   SETOFF                                       99
037800940131     C                   Z-ADD     0             S1OPZ
037900040930     c                   clear                   x1cod
038000160412     c                   clear                   h1cod2
038100940131     C*
038200940127     C* Leggo il sfl solo se ci sono rcd
0383009401311    C     WMAX          IFGT      0
038400940607     C                   READC     S1                                     21
038500940127     C*
038600940131     C* esce se fine sfl o errore che richiede l'uscita
0387009401312    C     *IN21         DOWEQ     *OFF
038800940131     C     $ESCI         ANDEQ     *OFF
038900940201     C                   Z-ADD     S1NRR         C1RCD
039000940131     C* ctrl su riga
039100940131     C                   EXSR      RECS1
039200940131     C* gestione opzioni
0393009401313    C     S1OPZ         IFNE      0
039400940201     C     *IN99         ANDEQ     *OFF
039500940131     C                   EXSR      OPZS1
0396009401273-   C                   ENDIF
039700940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0398009402013    C     *IN99         IFEQ      *ON
039900940201     C     $ESCI         OREQ      *ON
040000940131     C                   MOVE      *ON           *IN32
040100940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
040200940204     C* la ripristinerò a conclusione del ciclo di READC
040300940223     C                   MOVE      *OFF          $INZS1
0404009402233-   C                   ENDIF
040500940223     C*
040600940131     C                   Z-ADD     0             S1OPZ
040700940223     C*
040800940607     C                   UPDATE    S1
040900940223     C*
041000940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0411009401313    C     $ESCI         IFEQ      *OFF
041200940607     C                   READC     S1                                     21
041300940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0414009402014    C     *IN21         IFEQ      *ON
041500940201     C                   MOVE      WINZS1        $INZS1
041600940204     C* calcolo pagina a cui deve posizionarsi
041700940224     C                   EXSR      CLCPAG
0418009402014-   C                   ENDIF
0419009402013-   C                   ENDIF
042000940131     C*
0421009401272-   C                   ENDDO
042200940127     C*
0423009401311-   C                   ENDIF
042400940131     C*
042500940127     C                   ENDSR
042600940127     C/EJECT
042700940127     C************************************************************
042800940131     C* CONTROLLO CAMPI I/O RIGA LISTA
042900940127     C************************************************************
043000940131     C     RECS1         BEGSR
043100940131     C*
043200940201     C* reset indicatori DSPATR
043300940201     C                   MOVE      *ALL'0'       IN4049           10
043400940201     C                   MOVEA     IN4049        *IN(40)
043500940201     C*
043600040930     C*  se richiamato x selezionare un codice 11 = *on
043700040930     C*   non deve utilizzare le opzioni di modifica
043800040930     C*  e viceversa
043900040930     c     *in11         ifeq      *on
0440000409303    C     S1OPZ         IFeq      2
0441000409303    C     S1OPZ         OReq      3
0442000409303    C     S1OPZ         OReq      4
044300040930     c                   seton                                        41  99
044400040930     C                   END
044500040930      * selezionato un codice da restituire al chiamante
0446000409303    C     S1OPZ         IFeq      1
044700160412     c                   movel     s1cod         x1cod
044800160412     c                   movel     h1cod2        x1cod2
044900040930     c                   clear                   kpjbu
045000160412     c                   eval      kpjbu = x1cod + x1cod2
045100040930     C                   MOVEL     *ON           $ESCI
045200040930     C                   MOVEL     *ON           $fine
045300040930     C                   END
045400040930     C                   END
045500040930     C*
045600040930     c     *in11         ifeq      *off
0457000409303    C     S1OPZ         ANDeq     1
045800040930     c                   seton                                        41  99
045900040930     C                   END
046000040930     C*
046100940131     C                   ENDSR
046200940131     C************************************************************
046300940131     C* GESTIONE OPZIONI LISTA
046400940131     C************************************************************
046500940131     C     OPZS1         BEGSR
046600940201     C*
0467000409302    C     *IN11         IFEQ      *Off
0468000409302    C     *IN11         oreq      *On
0469000409302    C     s1opz         andeq     05
047000040930     C*
047100030113     C                   RESET                   tabds
047200110902     C                   MOVE      *all'0'       xtaopz
047300110902     C                   MOVE      S1OPZ         xtaopz
047400030113     C                   MOVE      *ZERO         xtaret
047500030113     C                   MOVE      *ZERO         xtaopr
047600110902     C                   MOVEl     S1COD         xtakey
047700160412     C                   MOVEl     H1COD2        xtake2
047800940715     C                   MOVE      *BLANKS       KPJBU
047900030113     C                   MOVEL     tabds         KPJBU
048000160412$004 C                   CALL      'TNTBSCSR2'
048100940607     C                   PARM                    KPJBA
048200030114     C                   MOVEL     KPJBU         tabds
048300940201     C*
048400940223     C* ritorno da PGM gestione
048500940223     C                   EXSR      GESRET
048600040930     C*
048700940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0488009402252    C     *IN99         IFEQ      *ON
048900940223     C                   SETON                                        40
0490009402252-   C                   ENDIF
049100940225     C*
0492000409302-   C                   ENDIF
049300040930     C*
049400940131     C                   ENDSR
049500940223     C/EJECT
049600940223     C************************************************************
049700940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
049800940223     C************************************************************
049900940223     C     GESRET        BEGSR
050000940223     C*
050100940223     C* modo di ritorno
050200940223     C*
0503009402231    C                   SELECT
050400940314    >C* << questi modi di utilizzo dei valori di ritorno dal
050500940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
050600940314    >C*    proposte, normalmente sempre valide, ma modificabili
050700940314    >C*    per situazioni particolari >>
050800940223     C* 1 = F3
050900030113    >C     xtaret        WHENEQ    '1'
051000940224     C                   MOVE      *ON           $ESCI
051100940223     C                   MOVE      *ON           $FINE
051200940223     C* 2 = F12
051300030113    >C     xtaret        WHENEQ    '2'
051400940223     C                   MOVE      *ON           $ESCI
051500940223     C*
0516009402231-   C                   ENDSL
051700940223     C*
051800940223     C* operazione eseguite dal pgm chiamato
051900940223     C*
0520009402231    C                   SELECT
052100940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
052200030113    >C     xtaopr        WHENEQ    '1'
052300940223     C                   MOVE      *ON           WINZS1
052400940223     C*
0525009402231-   C                   ENDSL
052600940223     C*
052700940223     C* funzione non eseguibile per errore :
052800940223     C*
0529009402231    C                   SELECT
053000940223     C* 1 = funzione richiamata chiusa in errore
053100940316    >C*  eventualmente gestire altri codici di errore
053200030113    >C     xtaerr        WHENEQ    '1'
053300940223     C                   MOVE      *ON           $ESCI
053400940223     C                   SETON                                        5299
053500940223     C*
0536009402231-   C                   ENDSL
053700940223     C*
053800940223     C                   ENDSR
053900940223     C/EJECT
054000940131     C************************************************************
054100940131     C* OPERAZIONI INIZIALI
054200940131     C************************************************************
054300940131     C     *INZSR        BEGSR
054400030113     C*
054500030113     C* Reperimento parametri
054600030113     C     *ENTRY        PLIST
054700030113     C                   PARM                    KPJBA
054800040930     C                   movel     kpjbu         Selez             1
054900040930     C*
055000040930     C* Se chiamato x selezionare un codice
055100040930     c                   clear                   x1cod
055200160412     c                   clear                   x1cod2
055300040930     c                   if        selez = '?'
055400040930     c                   seton                                        11
055500040930     c                   end
055600030113     C*
055700030113     C* Variabili per gestione videate
055800030113     C                   MOVE      *BLANK        $GEST             2
055900030113     C                   MOVE      *BLANK        $FINE             1
056000030113     C                   MOVE      *BLANK        $INZS1            1
056100030113     C                   MOVE      *BLANK        $EFILE            1
056200030113     C                   MOVE      *BLANK        $ESCI             1
056300030113     C                   MOVE      *BLANK        $RCDOK            1
056400030113     C*
056500030113     C     KTBL          KLIST
056600110926     C                   KFLD                    KCOD
056700030113     C* Indici
056800030113     C                   Z-ADD     0             X                 3 0
056900030113     C                   Z-ADD     0             Y                 3 0
057000940506     C*
057100160412     C                   move      'SCS'         KCOD
057200940127     C*
057300940117     C                   ENDSR
057400030113     C************************************************************
