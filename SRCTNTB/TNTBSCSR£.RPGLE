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
003700110926     D  XTAKEY                 6      8
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
005500940607     D* descrizione tabella
005600120412$003 D EXTDS         E DS                  EXTNAME(tntb93ds)
005700030113     D*-------------
005800030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005900030113$003 D S1NRR           S                   Like(C1rcd)
006000030113$003 D WSfl            S                   Like(C1nrr)
006100030113$003 D Wmax            S                   Like(C1rcd)
006200030113$003 D Wpag            S                   Like(C1rcd)
006300030113$003 D Winzs1          S                   Like($inzs1)
006400040930$003 D x1cod           S                   Like(s1cod)
006500160412     D KCOD            S                   LIKE(TBECOD) inz('SCS')
006600110926     D KKE1            S                   LIKE(TBEKE1)
006700160412     D KKE2            S                   LIKE(TBEKE2)
006800940207     D*-------------
006900940211     D* COSTANTI
007000940211     D*-------------
007100030113     D SFLPAG          C                   CONST(11)
007200940314     D* dimensione della schiera $MS1
007300940506     D*
007400940506     D* Tasti di funzione
007500940506     D F01             C                   CONST(X'31')
007600940506     D F02             C                   CONST(X'32')
007700940506     D F03             C                   CONST(X'33')
007800940506     D F04             C                   CONST(X'34')
007900940506     D F05             C                   CONST(X'35')
008000940506     D F06             C                   CONST(X'36')
008100940506     D F07             C                   CONST(X'37')
008200940506     D F08             C                   CONST(X'38')
008300940506     D F09             C                   CONST(X'39')
008400940506     D F10             C                   CONST(X'3A')
008500940506     D F11             C                   CONST(X'3B')
008600940506     D F12             C                   CONST(X'3C')
008700940506     D F13             C                   CONST(X'B1')
008800940506     D F14             C                   CONST(X'B2')
008900940506     D F15             C                   CONST(X'B3')
009000940506     D F16             C                   CONST(X'B4')
009100940506     D F17             C                   CONST(X'B5')
009200940506     D F18             C                   CONST(X'B6')
009300940506     D F19             C                   CONST(X'B7')
009400940506     D F20             C                   CONST(X'B8')
009500940506     D F21             C                   CONST(X'B9')
009600940506     D F22             C                   CONST(X'BA')
009700940506     D F23             C                   CONST(X'BB')
009800940506     D F24             C                   CONST(X'BC')
009900940506     D ENTER           C                   CONST(X'F1')
010000940506     D ROLDWN          C                   CONST(X'F4')
010100940506     D ROLLUP          C                   CONST(X'F5')
010200940207     I*-------------
010300940607     IS1
010400160412$015 I              S1DES                       desSTATUS
010500940127     C*----------------------------------------------------*
010600940127     C*                MAIN LINE PROGRAM
010700940127     C*----------------------------------------------------*
010800940223     C* inizializzazione variabili
010900940223     C                   EXSR      INZVAR
011000940223     C*
011100940223     C     $FINE         DOWEQ     *OFF
011200940131     C     $GEST         CASEQ     'S1'          GESS1
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
012500030113     C                   CLEAR                   S1OPZ
012600030113     C* Variabili per gestione videate
012700030113     C*
012800030113     C                   MOVE      *OFF          $FINE
012900030113     C                   MOVE      *OFF          $INZS1
013000030113     C                   MOVE      *OFF          $EFILE
013100030113     C                   MOVE      *OFF          $ESCI
013200030113     C                   MOVE      *OFF          $RCDOK
013300030113     C                   Z-ADD     0             $ULKS1            3 0
013400030113     C*
013500030113     C                   MOVE      *ON           $INZS1
013600030113     C                   MOVE      'S1'          $GEST
013700030113     C*
013800030113     C* Variabili appoggio
013900030114     C                   Z-ADD     1             WPAG
014000030113     c*
014100030113     C                   ENDSR
014200940127     C************************************************************
014300940131     C* GESTIONE LISTA
014400940127     C************************************************************
014500940127     C     GESS1         BEGSR
014600030113     C*
014700940223     C* inizializzazione videata
014800940223     C     $INZS1        IFEQ      *ON
014900940127     C                   EXSR      INZS1
015000940223     C                   MOVE      *OFF          $INZS1
015100940127     C                   ENDIF
015200030113     C*
015300030113     C* emissione piede videata
015400030113     C                   WRITE     Z1
015500030113     C* Non ci sono records
015600940223     C     WMAX          IFEQ      0
015700940607     C                   WRITE     D1
015800030114     C                   Else
015900120412     C     Wsfl          IFgt      Wmax
016000120412     c                   z-add     wmax          Wsfl
016100120412     c                   end
016200030114     C     Wsfl          IFgt      0
016300030114     C                   Z-ADD     wsfl          C1RCD
016400030114     C                   Else
016500030114     C     Wpag          IFgt      0
016600030114     C                   Z-ADD     wpag          C1RCD
016700030114     C                   EndIF
016800030114     C                   EndIF
016900030114     C                   ENDIF
017000940127     C*
017100030113     C*              *------------------*
017200940607     C                   EXFMT     C1
017300030113     C*              *------------------*
017400030113     C*
017500940204     C     C1NRR         IFNE      0
017600940204     C                   Z-ADD     C1NRR         WSFL
017700940204     C                   ENDIF
017800940127     C                   Z-ADD     SFLNRR        C1RCD
017900030113     C* Selezioni
0180009401271    C                   SELECT
018100940127     C* F3=Fine
018200940506     C     $TASTO        WHENEQ    F03
018300940309     C                   EXSR      F03S1
018400940131     C* F10=Immissione
018500940506     C     $TASTO        WHENEQ    F10
018600940309     C                   EXSR      F10S1
0187009401271O   C                   OTHER
018800940127     C* CONTROLLO DATI
018900940131     C                   EXSR      CTRC1
019000940201     C     *IN99         IFEQ      *OFF
019100940131     C                   EXSR      CTRS1
019200940131     C                   END
0193009401271-   C                   ENDSL
019400940127     C*
019500940127     C                   ENDSR
019600940224     C/EJECT
019700940127     C************************************************************
019800940131     C* INIZIALIZZAZIONE LISTA
019900940127     C************************************************************
020000940127     C     INZS1         BEGSR
020100940302     C* pulizia SFL
020200940128     C                   SETOFF                                         3031
020300940607     C                   WRITE     C1
020400940128     C                   SETON                                          31
020500940128     C*
020600030113     C* CARICAMENTO SFL totale
020700940201     C                   Z-ADD     0             S1NRR
020800030113     C                   Z-ADD     1             C1RCD
020900940128     C                   Z-ADD     0             WMAX
021000120412     C                   Z-ADD     1             WPAG
021100940224     C*
021200940224     C* Posizionamento su file pilota
021300110926     c     kTBL          setll     tntbe01l
021400940608    >C                   EXSR      REDANA
021500030113     C* Carico il SFL
021600940127     C                   EXSR      ROLS1
021700030113     C*
021800030114     c                   if        xtaopr <> '1'
021900030114     C                   Z-ADD     1             WPAG
022000030114     c                   end
022100940127     C*
022200940127     C                   ENDSR
022300940127     C************************************************************
022400940131     C* CARICAMENTO PAGINA LISTA
022500940127     C************************************************************
022600940127     C     ROLS1         BEGSR
022700940127     C*
022800940128     C                   SETOFF                                       32
022900940223     C                   Z-ADD     0             Y
023000940127     C                   Z-ADD     WMAX          S1NRR
023100940127     C*
023200940127     C* Leggo dal file anagrafico per caricare la lista
0233009401311    C     $EFILE        DOWEQ     *OFF
023400940127     C*
023500030113     c                   clear                   s1opz
023600160412     c                   clear                   desSTATUS
023700120412     c                   clear                   Obbiettivo        3
023800160412     c                   clear                   s1cod
023900160412     c                   clear                   s1des
024000110926     c                   movel     tbeke1        s1cod
024100160412     c                   movel     tbeke2        s1des
024200110926      **
024300120412      **
024400110926     c                   movel     tbeuni        extds
024500160412     c                   eval      s1des = %trim(s1des) + ' - ' + SCSDESC
024600030113     C*
024700030115     c                   clear                   S1Ann
024800940127     C*
024900940127     C                   ADD       1             S1NRR
025000940127     C                   ADD       1             Y
025100940127     C*
025200940607     C                   WRITE     S1
025300940131     C*
025400940316    >C                   EXSR      REDANA
025500940128     C*
0256009401271-   C                   ENDDO
025700940127     C*
025800940223     C                   Z-ADD     S1NRR         WMAX                 30
025900940127     C*
026000940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
026100940127     C*
026200030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
026300940127     C                   MVR                     RESTO             3 0
026400030114     C     PAGINE        MULT      SFLPAG        C1RCD
0265000301141    C     RESTO         IFGT      0
026600030114     C                   ADD       1             C1RCD
0267000301141E   C                   ELSE
026800030114     C                   SUB       SFLPAG        C1RCD
026900030114     C                   ADD       1             C1RCD
0270000301141-   C                   ENDIF
027100940128     C*
027200940127     C                   ENDSR
027300940128     C************************************************************
027400940131     C* LETTURA RCD ARCHIVIO PILOTA
027500940128     C************************************************************
027600940607     C     REDANA        BEGSR
027700940128     C*
027800940131     C                   MOVEL     *OFF          $EFILE
027900940131     C                   MOVEL     *OFF          $RCDOK
028000940131     C*
0281009401311    C     $EFILE        DOUEQ     *ON
028200940131     C     $RCDOK        OREQ      *ON
028300940131     C*
028400110926     c     kTBL          Reade     tntbe01l
028500110926     c                   if        %eof(tntbe01l)
028600030113     C                   MOVEL     *ON           $EFILE
028700030113     C                   MOVE      $EFILE        *IN33
028800030113     c                   else
028900030113     C                   MOVE      *ON           $RCDOK
029000030113     c                   end
029100940131     C*
0292009401311-   C                   ENDDO
029300940131     C*
029400940131     C                   ENDSR
029500940224     C************************************************************
029600940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
029700940224     C************************************************************
029800940224     C     CLCPAG        BEGSR
029900940224     C* Input :
030000940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
030100940224     C*          cursore
030200940224     C* - SFLPAG = numero rcd per pagina sfl
030300940224     C* Output :
030400940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
030500940224     C*
030600120412     c                   if        WSFL >0
030700940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
030800940224     C                   MVR                     RESTO             3 0
030900940224     C     RESTO         IFGT      0
031000940224     C                   ADD       1             PAGINE
031100940224     C                   ENDIF
031200940224     C     PAGINE        MULT      SFLPAG        WPAG
031300120412     c                   else
031400120412     C                   Z-ADD     1             WPAG
031500120412     c                   end
031600940224     C*
031700940224     C                   ENDSR
031800940309     C************************************************************
031900940309     C* GESTIONE F03 VIDEO S1
032000940309     C************************************************************
032100940309     C     F03S1         BEGSR
032200940309     C*
032300940309     C                   MOVE      *ON           $FINE
032400940325     C* fine programma
032500940309     C                   ENDSR
032600940309     C/EJECT
032700940309     C************************************************************
032800940309     C* GESTIONE F10 VIDEO S1
032900940314     c* AGGIUNTA RECORD
033000940309     C************************************************************
033100940309     C     F10S1         BEGSR
033200940309     C*
033300030113     C                   RESET                   tabds
033400030113     C                   MOVEL     '01'          xtaopz
033500030113     C                   MOVE      *ZERO         xtaret
033600030113     C                   MOVE      *ZERO         xtaopr
033700030114     C                   MOVEl     *blank        xtakey
033800030113     C                   MOVE      *BLANKS       KPJBU
033900030113     C                   MOVEL     tabds         KPJBU
034000160412$004 C                   CALL      'TNTBSCSR2'
034100030113     C                   PARM                    KPJBA
034200030114     C                   MOVEL     KPJBU         tabds
034300030113      *
034400940309     C* ritorno da PGM gestione
034500940309     C                   EXSR      GESRET
034600940309     C     WINZS1        IFEQ      *ON
034700940309     C                   MOVE      *ON           $INZS1
034800940309     C* se esistevano già righe sul sfl
034900940309     C* calcolo pagina a cui deve posizionarsi
035000940309     C     WSFL          IFGT      0
035100940309     C                   EXSR      CLCPAG
035200940309     C* altrimenti carico solo la 1a pagina
035300940309     C                   ELSE
035400940309     C                   Z-ADD     SFLPAG        WPAG
035500940309     C                   ENDIF
035600940309     C                   ENDIF
035700940309     C*
035800940309     C                   ENDSR
035900940128     C************************************************************
036000940131     C* CONTROLLO TESTATA LISTA
036100940128     C************************************************************
036200940131     C     CTRC1         BEGSR
036300940128     C*
036400940201     C                   MOVE      *OFF          *IN99
036500940131     C*
036600940202     C                   ENDSR
036700940131     C************************************************************
036800940131     C* CONTROLLO OPZIONI LISTA
036900940131     C************************************************************
037000940131     C     CTRS1         BEGSR
037100940131     C*
037200940202     C                   MOVEL     *OFF          $ESCI
037300940201     C                   SETOFF                                       99
037400940131     C                   Z-ADD     0             S1OPZ
037500040930     c                   clear                   x1cod
037600940131     C*
037700940127     C* Leggo il sfl solo se ci sono rcd
0378009401311    C     WMAX          IFGT      0
037900940607     C                   READC     S1                                     21
038000940127     C*
038100940131     C* esce se fine sfl o errore che richiede l'uscita
0382009401312    C     *IN21         DOWEQ     *OFF
038300940131     C     $ESCI         ANDEQ     *OFF
038400940201     C                   Z-ADD     S1NRR         C1RCD
038500940131     C* ctrl su riga
038600940131     C                   EXSR      RECS1
038700940131     C* gestione opzioni
0388009401313    C     S1OPZ         IFNE      0
038900940201     C     *IN99         ANDEQ     *OFF
039000940131     C                   EXSR      OPZS1
0391009401273-   C                   ENDIF
039200940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0393009402013    C     *IN99         IFEQ      *ON
039400940201     C     $ESCI         OREQ      *ON
039500940131     C                   MOVE      *ON           *IN32
039600940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
039700940204     C* la ripristinerò a conclusione del ciclo di READC
039800940223     C                   MOVE      *OFF          $INZS1
0399009402233-   C                   ENDIF
040000940223     C*
040100940131     C                   Z-ADD     0             S1OPZ
040200940223     C*
040300940607     C                   UPDATE    S1
040400940223     C*
040500940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0406009401313    C     $ESCI         IFEQ      *OFF
040700940607     C                   READC     S1                                     21
040800940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0409009402014    C     *IN21         IFEQ      *ON
041000940201     C                   MOVE      WINZS1        $INZS1
041100940204     C* calcolo pagina a cui deve posizionarsi
041200940224     C                   EXSR      CLCPAG
0413009402014-   C                   ENDIF
0414009402013-   C                   ENDIF
041500940131     C*
0416009401272-   C                   ENDDO
041700940127     C*
0418009401311-   C                   ENDIF
041900940131     C*
042000940127     C                   ENDSR
042100940127     C/EJECT
042200940127     C************************************************************
042300940131     C* CONTROLLO CAMPI I/O RIGA LISTA
042400940127     C************************************************************
042500940131     C     RECS1         BEGSR
042600940131     C*
042700940201     C* reset indicatori DSPATR
042800940201     C                   MOVE      *ALL'0'       IN4049           10
042900940201     C                   MOVEA     IN4049        *IN(40)
043000940201     C*
043100040930     C*  se richiamato x selezionare un codice 11 = *on
043200040930     C*   non deve utilizzare le opzioni di modifica
043300040930     C*  e viceversa
043400040930     c     *in11         ifeq      *on
0435000409303    C     S1OPZ         IFeq      2
0436000409303    C     S1OPZ         OReq      3
0437000409303    C     S1OPZ         OReq      4
043800040930     c                   seton                                        41  99
043900040930     C                   END
044000040930      * selezionato un codice da restituire al chiamante
0441000409303    C     S1OPZ         IFeq      1
044200040930     c                   move      s1cod         x1cod
044300040930     c                   clear                   kpjbu
044400040930     c                   movel     x1cod         kpjbu
044500040930     C                   MOVEL     *ON           $ESCI
044600040930     C                   MOVEL     *ON           $fine
044700040930     C                   END
044800040930     C                   END
044900040930     C*
045000040930     c     *in11         ifeq      *off
0451000409303    C     S1OPZ         ANDeq     1
045200040930     c                   seton                                        41  99
045300040930     C                   END
045400040930     C*
045500940131     C                   ENDSR
045600940131     C************************************************************
045700940131     C* GESTIONE OPZIONI LISTA
045800940131     C************************************************************
045900940131     C     OPZS1         BEGSR
046000940201     C*
0461000409302    C     *IN11         IFEQ      *Off
0462000409302    C     *IN11         oreq      *On
0463000409302    C     s1opz         andeq     05
046400040930     C*
046500030113     C                   RESET                   tabds
046600110902     C                   MOVE      *all'0'       xtaopz
046700110902     C                   MOVE      S1OPZ         xtaopz
046800030113     C                   MOVE      *ZERO         xtaret
046900030113     C                   MOVE      *ZERO         xtaopr
047000110902     C                   MOVEl     S1COD         xtakey
047100940715     C                   MOVE      *BLANKS       KPJBU
047200030113     C                   MOVEL     tabds         KPJBU
047300160412$004 C                   CALL      'TNTBSCSR2'
047400940607     C                   PARM                    KPJBA
047500030114     C                   MOVEL     KPJBU         tabds
047600940201     C*
047700940223     C* ritorno da PGM gestione
047800940223     C                   EXSR      GESRET
047900040930     C*
048000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0481009402252    C     *IN99         IFEQ      *ON
048200940223     C                   SETON                                        40
0483009402252-   C                   ENDIF
048400940225     C*
0485000409302-   C                   ENDIF
048600040930     C*
048700940131     C                   ENDSR
048800940223     C/EJECT
048900940223     C************************************************************
049000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
049100940223     C************************************************************
049200940223     C     GESRET        BEGSR
049300940223     C*
049400940223     C* modo di ritorno
049500940223     C*
0496009402231    C                   SELECT
049700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
049800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
049900940314    >C*    proposte, normalmente sempre valide, ma modificabili
050000940314    >C*    per situazioni particolari >>
050100940223     C* 1 = F3
050200030113    >C     xtaret        WHENEQ    '1'
050300940224     C                   MOVE      *ON           $ESCI
050400940223     C                   MOVE      *ON           $FINE
050500940223     C* 2 = F12
050600030113    >C     xtaret        WHENEQ    '2'
050700940223     C                   MOVE      *ON           $ESCI
050800940223     C*
0509009402231-   C                   ENDSL
051000940223     C*
051100940223     C* operazione eseguite dal pgm chiamato
051200940223     C*
0513009402231    C                   SELECT
051400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
051500030113    >C     xtaopr        WHENEQ    '1'
051600940223     C                   MOVE      *ON           WINZS1
051700940223     C*
0518009402231-   C                   ENDSL
051900940223     C*
052000940223     C* funzione non eseguibile per errore :
052100940223     C*
0522009402231    C                   SELECT
052300940223     C* 1 = funzione richiamata chiusa in errore
052400940316    >C*  eventualmente gestire altri codici di errore
052500030113    >C     xtaerr        WHENEQ    '1'
052600940223     C                   MOVE      *ON           $ESCI
052700940223     C                   SETON                                        5299
052800940223     C*
0529009402231-   C                   ENDSL
053000940223     C*
053100940223     C                   ENDSR
053200940223     C/EJECT
053300940131     C************************************************************
053400940131     C* OPERAZIONI INIZIALI
053500940131     C************************************************************
053600940131     C     *INZSR        BEGSR
053700030113     C*
053800030113     C* Reperimento parametri
053900030113     C     *ENTRY        PLIST
054000030113     C                   PARM                    KPJBA
054100040930     C                   movel     kpjbu         Selez             1
054200040930     C*
054300040930     C* Se chiamato x selezionare un codice
054400040930     c                   clear                   x1cod
054500040930     c                   if        selez = '?'
054600040930     c                   seton                                        11
054700040930     c                   end
054800030113     C*
054900030113     C* Variabili per gestione videate
055000030113     C                   MOVE      *BLANK        $GEST             2
055100030113     C                   MOVE      *BLANK        $FINE             1
055200030113     C                   MOVE      *BLANK        $INZS1            1
055300030113     C                   MOVE      *BLANK        $EFILE            1
055400030113     C                   MOVE      *BLANK        $ESCI             1
055500030113     C                   MOVE      *BLANK        $RCDOK            1
055600030113     C*
055700030113     C     KTBL          KLIST
055800110926     C                   KFLD                    KCOD
055900030113     C* Indici
056000030113     C                   Z-ADD     0             X                 3 0
056100030113     C                   Z-ADD     0             Y                 3 0
056200940506     C*
056300160412     C                   move      'SCS'         KCOD
056400940127     C*
056500940117     C                   ENDSR
056600030113     C************************************************************
