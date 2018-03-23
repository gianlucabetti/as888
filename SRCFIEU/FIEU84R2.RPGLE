000100141127     H*
000200141127     H*
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
002100040826     Ffieu84f   IF   E           K DISK
002200040826     FFIEU84D2  CF   E             WORKSTN
002300940607     F                                     SFILE(S1:S1NRR)
002400940201     F                                     INFDS(DSFMT)
002500940128     D*----------------------------------------------------*
002600940211     D* Passaggio Parametri
002700940211     D KPJBA         E DS
002800030113      *-------------
002900940325     D* Parametri in ricezione
003000030113     D  TABDS          DS
003100030729     D  XTAOPZ                 1      1
003200030729     D  XTARET                 2      2
003300030729     D  XTAOPR                 3      3
003400030729     D  XTAERR                 4      4
003500040826     D  XTACOD                 5      7
003600940211     D*-------------
003700940211     D DSFMT           DS           512
003800940506     D  $TASTO               369    369
003900940211     D  NRG                  370    370
004000940211     D  NCL                  371    371
004100940211     D  SFLNRR               378    379B 0
004200030728      *
004300030728     Ddataiso          s               d   datfmt(*iso)
004400030728     Ddataeur          s               d   datfmt(*eur)
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
005600030113     D*-------------
005700030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005800030113$003 D S1NRR           S                   Like(C1rcd)
005900030113$003 D WSfl            S                   Like(C1nrr)
006000030113$003 D Wmax            S                   Like(C1rcd)
006100030113$003 D Wpag            S                   Like(C1rcd)
006200030113$003 D Winzs1          S                   Like($inzs1)
006300041008$003 D x1cod           S                   Like(s1cod)
006400940207     D*-------------
006500940211     D* COSTANTI
006600940211     D*-------------
006700030113     D SFLPAG          C                   CONST(11)
006800940314     D* dimensione della schiera $MS1
006900940506     D*
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
010500940131     C     $GEST         CASEQ     'S1'          GESS1
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
011800030113     C                   CLEAR                   S1OPZ
011900030113     C* Variabili per gestione videate
012000030113     C*
012100030113     C                   MOVE      *OFF          $FINE
012200030113     C                   MOVE      *OFF          $INZS1
012300030113     C                   MOVE      *OFF          $EFILE
012400030113     C                   MOVE      *OFF          $ESCI
012500030113     C                   MOVE      *OFF          $RCDOK
012600030113     C                   Z-ADD     0             $ULKS1            3 0
012700030113     C*
012800030113     C                   MOVE      *ON           $INZS1
012900030113     C                   MOVE      'S1'          $GEST
013000030113     C*
013100030113     C* Variabili appoggio
013200030114     C                   Z-ADD     1             WPAG
013300030113     c*
013400030113     C                   ENDSR
013500940127     C************************************************************
013600940131     C* GESTIONE LISTA
013700940127     C************************************************************
013800940127     C     GESS1         BEGSR
013900030113     C*
014000940223     C* inizializzazione videata
014100940223     C     $INZS1        IFEQ      *ON
014200940127     C                   EXSR      INZS1
014300940223     C                   MOVE      *OFF          $INZS1
014400940127     C                   ENDIF
014500030113     C*
014600030113     C* emissione piede videata
014700030113     C                   WRITE     Z1
014800030113     C* Non ci sono records
014900940223     C     WMAX          IFEQ      0
015000940607     C                   WRITE     D1
015100030114     C                   Else
015200030114     C     Wsfl          IFgt      0
015300030114     C                   Z-ADD     wsfl          C1RCD
015400030114     C                   Else
015500030114     C     Wpag          IFgt      0
015600040115     C     pagine        andgt     0
015700030114     C                   Z-ADD     wpag          C1RCD
015800030114     C                   EndIF
015900030114     C                   EndIF
016000030114     C                   ENDIF
016100940127     C*
016200030113     C*              *------------------*
016300940607     C                   EXFMT     C1
016400030113     C*              *------------------*
016500030113     C*
016600940204     C     C1NRR         IFNE      0
016700940204     C                   Z-ADD     C1NRR         WSFL
016800940204     C                   ENDIF
016900940127     C                   Z-ADD     SFLNRR        C1RCD
017000030113     C* Selezioni
0171009401271    C                   SELECT
017200940127     C* F3=Fine
017300940506     C     $TASTO        WHENEQ    F03
017400940309     C                   EXSR      F03S1
017500940131     C* F10=Immissione
017600940506     C     $TASTO        WHENEQ    F10
017700940309     C                   EXSR      F10S1
0178009401271O   C                   OTHER
017900940127     C* CONTROLLO DATI
018000940131     C                   EXSR      CTRC1
018100940201     C     *IN99         IFEQ      *OFF
018200940131     C                   EXSR      CTRS1
018300940131     C                   END
0184009401271-   C                   ENDSL
018500940127     C*
018600940127     C                   ENDSR
018700940224     C/EJECT
018800940127     C************************************************************
018900940131     C* INIZIALIZZAZIONE LISTA
019000940127     C************************************************************
019100940127     C     INZS1         BEGSR
019200940302     C* pulizia SFL
019300940128     C                   SETOFF                                         3031
019400940607     C                   WRITE     C1
019500940128     C                   SETON                                          31
019600940128     C*
019700030113     C* CARICAMENTO SFL totale
019800940201     C                   Z-ADD     0             S1NRR
019900030113     C                   Z-ADD     1             C1RCD
020000940128     C                   Z-ADD     0             WMAX
020100940224     C*
020200940224     C* Posizionamento su file pilota
020300040826     c     *loval        setll     fieu84f
020400940608    >C                   EXSR      REDANA
020500030113     C* Carico il SFL
020600940127     C                   EXSR      ROLS1
020700030113     C*
020800030114     c                   if        xtaopr <> '1'
020900030114     C                   Z-ADD     1             WPAG
021000030114     c                   end
021100940127     C*
021200940127     C                   ENDSR
021300940127     C************************************************************
021400940131     C* CARICAMENTO PAGINA LISTA
021500940127     C************************************************************
021600940127     C     ROLS1         BEGSR
021700940127     C*
021800940128     C                   SETOFF                                       32
021900940223     C                   Z-ADD     0             Y
022000940127     C                   Z-ADD     WMAX          S1NRR
022100940127     C*
022200940127     C* Leggo dal file anagrafico per caricare la lista
0223009401311    C     $EFILE        DOWEQ     *OFF
022400940127     C*
022500030113     c                   clear                   s1opz
022600040826     c                   movel     t84NTW        s1cod
022700040826     c                   movel     t84des        s1des
022800040826     c                   movel     t84ifc        s1ifc
022900040826     c                   movel     t84ifr        s1ifr
023000040826     c                   movel     t84efc        s1efc
023100040826     c                   movel     t84efr        s1efr
023200040826     c                   movel     t84iac        s1iac
023300040826     c                   movel     t84iar        s1iar
023400040826     c                   movel     t84eac        s1eac
023500040826     c                   movel     t84ear        s1ear
023600940127     C*
023700940127     C                   ADD       1             S1NRR
023800940127     C                   ADD       1             Y
023900940127     C*
024000940607     C                   WRITE     S1
024100940131     C*
024200940316    >C                   EXSR      REDANA
024300940128     C*
0244009401271-   C                   ENDDO
024500940127     C*
024600940223     C                   Z-ADD     S1NRR         WMAX                 30
024700940127     C*
024800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
024900940127     C*
025000030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
025100940127     C                   MVR                     RESTO             3 0
025200030114     C     PAGINE        MULT      SFLPAG        C1RCD
0253000301141    C     RESTO         IFGT      0
025400030114     C                   ADD       1             C1RCD
0255000301141E   C                   ELSE
025600030114     C                   SUB       SFLPAG        C1RCD
025700030114     C                   ADD       1             C1RCD
0258000301141-   C                   ENDIF
025900940128     C*
026000940127     C                   ENDSR
026100940128     C************************************************************
026200940131     C* LETTURA RCD ARCHIVIO PILOTA
026300940128     C************************************************************
026400940607     C     REDANA        BEGSR
026500940128     C*
026600940131     C                   MOVEL     *OFF          $EFILE
026700940131     C                   MOVEL     *OFF          $RCDOK
026800940131     C*
0269009401311    C     $EFILE        DOUEQ     *ON
027000940131     C     $RCDOK        OREQ      *ON
027100030728     C*
027200040826     c                   Read      fieu84f
027300040826     c                   if        %eof(fieu84f)
027400030113     C                   MOVEL     *ON           $EFILE
027500030113     C                   MOVE      $EFILE        *IN33
027600030113     c                   else
027700030113     C                   MOVE      *ON           $RCDOK
027800030113     c                   end
027900940131     C*
0280009401311-   C                   ENDDO
028100940131     C*
028200940131     C                   ENDSR
028300940224     C************************************************************
028400940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
028500940224     C************************************************************
028600940224     C     CLCPAG        BEGSR
028700940224     C* Input :
028800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
028900940224     C*          cursore
029000940224     C* - SFLPAG = numero rcd per pagina sfl
029100940224     C* Output :
029200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
029300940224     C*
029400940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
029500940224     C                   MVR                     RESTO             3 0
029600940224     C     RESTO         IFGT      0
029700940224     C                   ADD       1             PAGINE
029800940224     C                   ENDIF
029900940224     C     PAGINE        MULT      SFLPAG        WPAG
030000940224     C*
030100940224     C                   ENDSR
030200940309     C************************************************************
030300940309     C* GESTIONE F03 VIDEO S1
030400940309     C************************************************************
030500940309     C     F03S1         BEGSR
030600940309     C*
030700940309     C                   MOVE      *ON           $FINE
030800940325     C* fine programma
030900940309     C                   ENDSR
031000940309     C/EJECT
031100940309     C************************************************************
031200940309     C* GESTIONE F10 VIDEO S1
031300940314     c* AGGIUNTA RECORD
031400940309     C************************************************************
031500940309     C     F10S1         BEGSR
031600940309     C*
031700030113     C                   RESET                   tabds
031800030729     C                   MOVEL     '1'           xtaopz
031900030113     C                   MOVE      *ZERO         xtaret
032000030113     C                   MOVE      *ZERO         xtaopr
032100030729     C                   MOVEl     *blank        xtacod
032200030113     C                   MOVE      *BLANKS       KPJBU
032300030113     C                   MOVEL     tabds         KPJBU
032400040826$004 C                   CALL      'FIEU84R1'
032500030113     C                   PARM                    KPJBA
032600030114     C                   MOVEL     KPJBU         tabds
032700030113      *
032800940309     C* ritorno da PGM gestione
032900940309     C                   EXSR      GESRET
033000940309     C     WINZS1        IFEQ      *ON
033100940309     C                   MOVE      *ON           $INZS1
033200940309     C* se esistevano già righe sul sfl
033300940309     C* calcolo pagina a cui deve posizionarsi
033400940309     C     WSFL          IFGT      0
033500940309     C                   EXSR      CLCPAG
033600940309     C* altrimenti carico solo la 1a pagina
033700940309     C                   ELSE
033800940309     C                   Z-ADD     SFLPAG        WPAG
033900940309     C                   ENDIF
034000940309     C                   ENDIF
034100940309     C*
034200940309     C                   ENDSR
034300940128     C************************************************************
034400940131     C* CONTROLLO TESTATA LISTA
034500940128     C************************************************************
034600940131     C     CTRC1         BEGSR
034700940128     C*
034800940201     C                   MOVE      *OFF          *IN99
034900940131     C*
035000940202     C                   ENDSR
035100940131     C************************************************************
035200940131     C* CONTROLLO OPZIONI LISTA
035300940131     C************************************************************
035400940131     C     CTRS1         BEGSR
035500940131     C*
035600940202     C                   MOVEL     *OFF          $ESCI
035700940201     C                   SETOFF                                       99
035800940131     C                   Z-ADD     0             S1OPZ
035900940131     C*
036000940127     C* Leggo il sfl solo se ci sono rcd
0361009401311    C     WMAX          IFGT      0
036200940607     C                   READC     S1                                     21
036300940127     C*
036400940131     C* esce se fine sfl o errore che richiede l'uscita
0365009401312    C     *IN21         DOWEQ     *OFF
036600940131     C     $ESCI         ANDEQ     *OFF
036700940201     C                   Z-ADD     S1NRR         C1RCD
036800940131     C* ctrl su riga
036900940131     C                   EXSR      RECS1
037000940131     C* gestione opzioni
0371009401313    C     S1OPZ         IFNE      0
037200940201     C     *IN99         ANDEQ     *OFF
037300940131     C                   EXSR      OPZS1
0374009401273-   C                   ENDIF
037500940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0376009402013    C     *IN99         IFEQ      *ON
037700940201     C     $ESCI         OREQ      *ON
037800940131     C                   MOVE      *ON           *IN32
037900940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038000940204     C* la ripristinerò a conclusione del ciclo di READC
038100940223     C                   MOVE      *OFF          $INZS1
0382009402233-   C                   ENDIF
038300940223     C*
038400940131     C                   Z-ADD     0             S1OPZ
038500940223     C*
038600940607     C                   UPDATE    S1
038700940223     C*
038800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0389009401313    C     $ESCI         IFEQ      *OFF
039000940607     C                   READC     S1                                     21
039100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0392009402014    C     *IN21         IFEQ      *ON
039300940201     C                   MOVE      WINZS1        $INZS1
039400940204     C* calcolo pagina a cui deve posizionarsi
039500940224     C                   EXSR      CLCPAG
0396009402014-   C                   ENDIF
0397009402013-   C                   ENDIF
039800940131     C*
0399009401272-   C                   ENDDO
040000940127     C*
0401009401311-   C                   ENDIF
040200940131     C*
040300940127     C                   ENDSR
040400940127     C/EJECT
040500940127     C************************************************************
040600940131     C* CONTROLLO CAMPI I/O RIGA LISTA
040700940127     C************************************************************
040800940131     C     RECS1         BEGSR
040900940131     C*
041000940201     C* reset indicatori DSPATR
041100940201     C                   MOVE      *ALL'0'       IN4049           10
041200940201     C                   MOVEA     IN4049        *IN(40)
041300940201     C*
041400041008     C*  se richiamato x selezionare un codice 11 = *on
041500041008     C*   non deve utilizzare le opzioni di modifica
041600041008     C*  e viceversa
041700041008     c     *in11         ifeq      *on
0418000410083    C     S1OPZ         IFeq      2
0419000410083    C     S1OPZ         OReq      3
0420000410083    C     S1OPZ         OReq      4
042100041008     c                   seton                                        41  99
042200041008     C                   END
042300041008      * selezionato un codice da restituire al chiamante
0424000410083    C     S1OPZ         IFeq      1
042500041008     c                   move      s1cod         x1cod
042600041008     c                   clear                   kpjbu
042700041008     c                   movel     x1cod         kpjbu
042800041008     C                   MOVEL     *ON           $ESCI
042900041008     C                   MOVEL     *ON           $fine
043000041008     C                   END
043100041008     C                   END
043200041008     C*
043300041008     c     *in11         ifeq      *off
0434000410083    C     S1OPZ         ANDeq     1
043500041008     c                   seton                                        41  99
043600041008     C                   END
043700041008     C*
043800940131     C                   ENDSR
043900940131     C************************************************************
044000940131     C* GESTIONE OPZIONI LISTA
044100940131     C************************************************************
044200940131     C     OPZS1         BEGSR
044300041008     C*
0444000410082    C     *IN11         IFEQ      *Off
0445000410082    C     *IN11         oreq      *On
0446000410082    C     s1opz         andeq     05
044700940201     C*
044800030113     C                   RESET                   tabds
044900030113     C                   MOVEL     S1OPZ         xtaopz
045000030113     C                   MOVE      *ZERO         xtaret
045100030113     C                   MOVE      *ZERO         xtaopr
045200030729     C                   MOVEl     s1cod         xtacod
045300940715     C                   MOVE      *BLANKS       KPJBU
045400030113     C                   MOVEL     tabds         KPJBU
045500040826$004 C                   CALL      'FIEU84R1'
045600940607     C                   PARM                    KPJBA
045700030114     C                   MOVEL     KPJBU         tabds
045800940201     C*
045900940223     C* ritorno da PGM gestione
046000940223     C                   EXSR      GESRET
046100940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0462009402252    C     *IN99         IFEQ      *ON
046300940223     C                   SETON                                        40
0464009402252-   C                   ENDIF
046500940225     C*
0466000410082-   C                   ENDIF
046700041008     C*
046800940131     C                   ENDSR
046900940223     C/EJECT
047000940223     C************************************************************
047100940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
047200940223     C************************************************************
047300940223     C     GESRET        BEGSR
047400940223     C*
047500940223     C* modo di ritorno
047600940223     C*
0477009402231    C                   SELECT
047800940314    >C* << questi modi di utilizzo dei valori di ritorno dal
047900940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
048000940314    >C*    proposte, normalmente sempre valide, ma modificabili
048100940314    >C*    per situazioni particolari >>
048200940223     C* 1 = F3
048300030113    >C     xtaret        WHENEQ    '1'
048400940224     C                   MOVE      *ON           $ESCI
048500940223     C                   MOVE      *ON           $FINE
048600940223     C* 2 = F12
048700030113    >C     xtaret        WHENEQ    '2'
048800940223     C                   MOVE      *ON           $ESCI
048900940223     C*
0490009402231-   C                   ENDSL
049100940223     C*
049200940223     C* operazione eseguite dal pgm chiamato
049300940223     C*
0494009402231    C                   SELECT
049500940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
049600030113    >C     xtaopr        WHENEQ    '1'
049700940223     C                   MOVE      *ON           WINZS1
049800940223     C*
0499009402231-   C                   ENDSL
050000940223     C*
050100940223     C* funzione non eseguibile per errore :
050200940223     C*
0503009402231    C                   SELECT
050400940223     C* 1 = funzione richiamata chiusa in errore
050500940316    >C*  eventualmente gestire altri codici di errore
050600030113    >C     xtaerr        WHENEQ    '1'
050700940223     C                   MOVE      *ON           $ESCI
050800940223     C                   SETON                                        5299
050900940223     C*
0510009402231-   C                   ENDSL
051100940223     C*
051200940223     C                   ENDSR
051300940223     C/EJECT
051400940131     C************************************************************
051500940131     C* OPERAZIONI INIZIALI
051600940131     C************************************************************
051700940131     C     *INZSR        BEGSR
051800030113     C*
051900030113     C* Reperimento parametri
052000030113     C     *ENTRY        PLIST
052100030113     C                   PARM                    KPJBA
052200041008     C                   movel     kpjbu         Selez             1
052300041008     C*
052400041008     c                   if        selez = 'V'
052500041008     c                   seton                                        11
052600041008     c                   end
052700030113     C*
052800030113     C* Variabili per gestione videate
052900030113     C                   MOVE      *BLANK        $GEST             2
053000030113     C                   MOVE      *BLANK        $FINE             1
053100030113     C                   MOVE      *BLANK        $INZS1            1
053200030113     C                   MOVE      *BLANK        $EFILE            1
053300030113     C                   MOVE      *BLANK        $ESCI             1
053400030113     C                   MOVE      *BLANK        $RCDOK            1
053500030113     C* Indici
053600030113     C                   Z-ADD     0             X                 3 0
053700030113     C                   Z-ADD     0             Y                 3 0
053800940127     C*
053900940117     C                   ENDSR
054000030113     C************************************************************
