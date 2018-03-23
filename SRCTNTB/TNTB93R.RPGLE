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
002400120412     FTntb93D   CF   E             WORKSTN
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
006500120412     D KCOD            S                   LIKE(TBECOD) inz('MIC')
006600110926     D KKE1            S                   LIKE(TBEKE1)
006700940207     D*-------------
006800940211     D* COSTANTI
006900940211     D*-------------
007000030113     D SFLPAG          C                   CONST(11)
007100940314     D* dimensione della schiera $MS1
007200940506     D*
007300940506     D* Tasti di funzione
007400940506     D F01             C                   CONST(X'31')
007500940506     D F02             C                   CONST(X'32')
007600940506     D F03             C                   CONST(X'33')
007700940506     D F04             C                   CONST(X'34')
007800940506     D F05             C                   CONST(X'35')
007900940506     D F06             C                   CONST(X'36')
008000940506     D F07             C                   CONST(X'37')
008100940506     D F08             C                   CONST(X'38')
008200940506     D F09             C                   CONST(X'39')
008300940506     D F10             C                   CONST(X'3A')
008400940506     D F11             C                   CONST(X'3B')
008500940506     D F12             C                   CONST(X'3C')
008600940506     D F13             C                   CONST(X'B1')
008700940506     D F14             C                   CONST(X'B2')
008800940506     D F15             C                   CONST(X'B3')
008900940506     D F16             C                   CONST(X'B4')
009000940506     D F17             C                   CONST(X'B5')
009100940506     D F18             C                   CONST(X'B6')
009200940506     D F19             C                   CONST(X'B7')
009300940506     D F20             C                   CONST(X'B8')
009400940506     D F21             C                   CONST(X'B9')
009500940506     D F22             C                   CONST(X'BA')
009600940506     D F23             C                   CONST(X'BB')
009700940506     D F24             C                   CONST(X'BC')
009800940506     D ENTER           C                   CONST(X'F1')
009900940506     D ROLDWN          C                   CONST(X'F4')
010000940506     D ROLLUP          C                   CONST(X'F5')
010100940207     I*-------------
010200940607     IS1
010300120412$015 I              S1DES                       DESFILOBB
010400940127     C*----------------------------------------------------*
010500940127     C*                MAIN LINE PROGRAM
010600940127     C*----------------------------------------------------*
010700940223     C* inizializzazione variabili
010800940223     C                   EXSR      INZVAR
010900940223     C*
011000940223     C     $FINE         DOWEQ     *OFF
011100940131     C     $GEST         CASEQ     'S1'          GESS1
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
012400030113     C                   CLEAR                   S1OPZ
012500030113     C* Variabili per gestione videate
012600030113     C*
012700030113     C                   MOVE      *OFF          $FINE
012800030113     C                   MOVE      *OFF          $INZS1
012900030113     C                   MOVE      *OFF          $EFILE
013000030113     C                   MOVE      *OFF          $ESCI
013100030113     C                   MOVE      *OFF          $RCDOK
013200030113     C                   Z-ADD     0             $ULKS1            3 0
013300030113     C*
013400030113     C                   MOVE      *ON           $INZS1
013500030113     C                   MOVE      'S1'          $GEST
013600030113     C*
013700030113     C* Variabili appoggio
013800030114     C                   Z-ADD     1             WPAG
013900030113     c*
014000030113     C                   ENDSR
014100940127     C************************************************************
014200940131     C* GESTIONE LISTA
014300940127     C************************************************************
014400940127     C     GESS1         BEGSR
014500030113     C*
014600940223     C* inizializzazione videata
014700940223     C     $INZS1        IFEQ      *ON
014800940127     C                   EXSR      INZS1
014900940223     C                   MOVE      *OFF          $INZS1
015000940127     C                   ENDIF
015100030113     C*
015200030113     C* emissione piede videata
015300030113     C                   WRITE     Z1
015400030113     C* Non ci sono records
015500940223     C     WMAX          IFEQ      0
015600940607     C                   WRITE     D1
015700030114     C                   Else
015800120412     C     Wsfl          IFgt      Wmax
015900120412     c                   z-add     wmax          Wsfl
016000120412     c                   end
016100030114     C     Wsfl          IFgt      0
016200030114     C                   Z-ADD     wsfl          C1RCD
016300030114     C                   Else
016400030114     C     Wpag          IFgt      0
016500030114     C                   Z-ADD     wpag          C1RCD
016600030114     C                   EndIF
016700030114     C                   EndIF
016800030114     C                   ENDIF
016900940127     C*
017000030113     C*              *------------------*
017100940607     C                   EXFMT     C1
017200030113     C*              *------------------*
017300030113     C*
017400940204     C     C1NRR         IFNE      0
017500940204     C                   Z-ADD     C1NRR         WSFL
017600940204     C                   ENDIF
017700940127     C                   Z-ADD     SFLNRR        C1RCD
017800030113     C* Selezioni
0179009401271    C                   SELECT
018000940127     C* F3=Fine
018100940506     C     $TASTO        WHENEQ    F03
018200940309     C                   EXSR      F03S1
018300940131     C* F10=Immissione
018400940506     C     $TASTO        WHENEQ    F10
018500940309     C                   EXSR      F10S1
0186009401271O   C                   OTHER
018700940127     C* CONTROLLO DATI
018800940131     C                   EXSR      CTRC1
018900940201     C     *IN99         IFEQ      *OFF
019000940131     C                   EXSR      CTRS1
019100940131     C                   END
0192009401271-   C                   ENDSL
019300940127     C*
019400940127     C                   ENDSR
019500940224     C/EJECT
019600940127     C************************************************************
019700940131     C* INIZIALIZZAZIONE LISTA
019800940127     C************************************************************
019900940127     C     INZS1         BEGSR
020000940302     C* pulizia SFL
020100940128     C                   SETOFF                                         3031
020200940607     C                   WRITE     C1
020300940128     C                   SETON                                          31
020400940128     C*
020500030113     C* CARICAMENTO SFL totale
020600940201     C                   Z-ADD     0             S1NRR
020700030113     C                   Z-ADD     1             C1RCD
020800940128     C                   Z-ADD     0             WMAX
020900120412     C                   Z-ADD     1             WPAG
021000940224     C*
021100940224     C* Posizionamento su file pilota
021200110926     c     kTBL          setll     tntbe01l
021300940608    >C                   EXSR      REDANA
021400030113     C* Carico il SFL
021500940127     C                   EXSR      ROLS1
021600030113     C*
021700030114     c                   if        xtaopr <> '1'
021800030114     C                   Z-ADD     1             WPAG
021900030114     c                   end
022000940127     C*
022100940127     C                   ENDSR
022200940127     C************************************************************
022300940131     C* CARICAMENTO PAGINA LISTA
022400940127     C************************************************************
022500940127     C     ROLS1         BEGSR
022600940127     C*
022700940128     C                   SETOFF                                       32
022800940223     C                   Z-ADD     0             Y
022900940127     C                   Z-ADD     WMAX          S1NRR
023000940127     C*
023100940127     C* Leggo dal file anagrafico per caricare la lista
0232009401311    C     $EFILE        DOWEQ     *OFF
023300940127     C*
023400030113     c                   clear                   s1opz
023500120412     c                   clear                   DESfilOBB
023600120412     c                   clear                   DESfil
023700120412     c                   clear                   Obbiettivo        3
023800110901     c                   move      *all'0'       s1cod
023900110926     c                   movel     tbeke1        s1cod
024000110926     c                   move      s1cod         d1fil             3 0
024100110926      **
024200110926     c     d1FIL         chain     azorg01l
024300110926     c                   if        %Found(azorg01l)
024400120412     c                   movel     orgdes        DESfil           25
024500110926     c                   end
024600120412      **
024700110926     c                   movel     tbeuni        extds
024800120412     c                   if        B93MMIC  <> *blank
024900120412     c                   eval      obbiettivo = B93MMIC
025000120412     c                   else
025100120412     c                   eval      obbiettivo = '000'
025200120412     c                   end
025300120412     c                   move      obbiettivo    obbiet            3 0
025400030113     C*
025500120412     c                   eval      DESfilOBB = Desfil + '  ' +
025600120412     c                             %editc(Obbiet:'4')
025700120412     C*
025800030115     c                   clear                   S1Ann
025900940127     C*
026000940127     C                   ADD       1             S1NRR
026100940127     C                   ADD       1             Y
026200940127     C*
026300940607     C                   WRITE     S1
026400940131     C*
026500940316    >C                   EXSR      REDANA
026600940128     C*
0267009401271-   C                   ENDDO
026800940127     C*
026900940223     C                   Z-ADD     S1NRR         WMAX                 30
027000940127     C*
027100940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
027200940127     C*
027300030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
027400940127     C                   MVR                     RESTO             3 0
027500030114     C     PAGINE        MULT      SFLPAG        C1RCD
0276000301141    C     RESTO         IFGT      0
027700030114     C                   ADD       1             C1RCD
0278000301141E   C                   ELSE
027900030114     C                   SUB       SFLPAG        C1RCD
028000030114     C                   ADD       1             C1RCD
0281000301141-   C                   ENDIF
028200940128     C*
028300940127     C                   ENDSR
028400940128     C************************************************************
028500940131     C* LETTURA RCD ARCHIVIO PILOTA
028600940128     C************************************************************
028700940607     C     REDANA        BEGSR
028800940128     C*
028900940131     C                   MOVEL     *OFF          $EFILE
029000940131     C                   MOVEL     *OFF          $RCDOK
029100940131     C*
0292009401311    C     $EFILE        DOUEQ     *ON
029300940131     C     $RCDOK        OREQ      *ON
029400940131     C*
029500110926     c     kTBL          Reade     tntbe01l
029600110926     c                   if        %eof(tntbe01l)
029700030113     C                   MOVEL     *ON           $EFILE
029800030113     C                   MOVE      $EFILE        *IN33
029900030113     c                   else
030000030113     C                   MOVE      *ON           $RCDOK
030100030113     c                   end
030200940131     C*
0303009401311-   C                   ENDDO
030400940131     C*
030500940131     C                   ENDSR
030600940224     C************************************************************
030700940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
030800940224     C************************************************************
030900940224     C     CLCPAG        BEGSR
031000940224     C* Input :
031100940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
031200940224     C*          cursore
031300940224     C* - SFLPAG = numero rcd per pagina sfl
031400940224     C* Output :
031500940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
031600940224     C*
031700120412     c                   if        WSFL >0
031800940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
031900940224     C                   MVR                     RESTO             3 0
032000940224     C     RESTO         IFGT      0
032100940224     C                   ADD       1             PAGINE
032200940224     C                   ENDIF
032300940224     C     PAGINE        MULT      SFLPAG        WPAG
032400120412     c                   else
032500120412     C                   Z-ADD     1             WPAG
032600120412     c                   end
032700940224     C*
032800940224     C                   ENDSR
032900940309     C************************************************************
033000940309     C* GESTIONE F03 VIDEO S1
033100940309     C************************************************************
033200940309     C     F03S1         BEGSR
033300940309     C*
033400940309     C                   MOVE      *ON           $FINE
033500940325     C* fine programma
033600940309     C                   ENDSR
033700940309     C/EJECT
033800940309     C************************************************************
033900940309     C* GESTIONE F10 VIDEO S1
034000940314     c* AGGIUNTA RECORD
034100940309     C************************************************************
034200940309     C     F10S1         BEGSR
034300940309     C*
034400030113     C                   RESET                   tabds
034500030113     C                   MOVEL     '01'          xtaopz
034600030113     C                   MOVE      *ZERO         xtaret
034700030113     C                   MOVE      *ZERO         xtaopr
034800030114     C                   MOVEl     *blank        xtakey
034900030113     C                   MOVE      *BLANKS       KPJBU
035000030113     C                   MOVEL     tabds         KPJBU
035100120412$004 C                   CALL      'TNTB93R2'
035200030113     C                   PARM                    KPJBA
035300030114     C                   MOVEL     KPJBU         tabds
035400030113      *
035500940309     C* ritorno da PGM gestione
035600940309     C                   EXSR      GESRET
035700940309     C     WINZS1        IFEQ      *ON
035800940309     C                   MOVE      *ON           $INZS1
035900940309     C* se esistevano già righe sul sfl
036000940309     C* calcolo pagina a cui deve posizionarsi
036100940309     C     WSFL          IFGT      0
036200940309     C                   EXSR      CLCPAG
036300940309     C* altrimenti carico solo la 1a pagina
036400940309     C                   ELSE
036500940309     C                   Z-ADD     SFLPAG        WPAG
036600940309     C                   ENDIF
036700940309     C                   ENDIF
036800940309     C*
036900940309     C                   ENDSR
037000940128     C************************************************************
037100940131     C* CONTROLLO TESTATA LISTA
037200940128     C************************************************************
037300940131     C     CTRC1         BEGSR
037400940128     C*
037500940201     C                   MOVE      *OFF          *IN99
037600940131     C*
037700940202     C                   ENDSR
037800940131     C************************************************************
037900940131     C* CONTROLLO OPZIONI LISTA
038000940131     C************************************************************
038100940131     C     CTRS1         BEGSR
038200940131     C*
038300940202     C                   MOVEL     *OFF          $ESCI
038400940201     C                   SETOFF                                       99
038500940131     C                   Z-ADD     0             S1OPZ
038600040930     c                   clear                   x1cod
038700940131     C*
038800940127     C* Leggo il sfl solo se ci sono rcd
0389009401311    C     WMAX          IFGT      0
039000940607     C                   READC     S1                                     21
039100940127     C*
039200940131     C* esce se fine sfl o errore che richiede l'uscita
0393009401312    C     *IN21         DOWEQ     *OFF
039400940131     C     $ESCI         ANDEQ     *OFF
039500940201     C                   Z-ADD     S1NRR         C1RCD
039600940131     C* ctrl su riga
039700940131     C                   EXSR      RECS1
039800940131     C* gestione opzioni
0399009401313    C     S1OPZ         IFNE      0
040000940201     C     *IN99         ANDEQ     *OFF
040100940131     C                   EXSR      OPZS1
0402009401273-   C                   ENDIF
040300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0404009402013    C     *IN99         IFEQ      *ON
040500940201     C     $ESCI         OREQ      *ON
040600940131     C                   MOVE      *ON           *IN32
040700940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
040800940204     C* la ripristinerò a conclusione del ciclo di READC
040900940223     C                   MOVE      *OFF          $INZS1
0410009402233-   C                   ENDIF
041100940223     C*
041200940131     C                   Z-ADD     0             S1OPZ
041300940223     C*
041400940607     C                   UPDATE    S1
041500940223     C*
041600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0417009401313    C     $ESCI         IFEQ      *OFF
041800940607     C                   READC     S1                                     21
041900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0420009402014    C     *IN21         IFEQ      *ON
042100940201     C                   MOVE      WINZS1        $INZS1
042200940204     C* calcolo pagina a cui deve posizionarsi
042300940224     C                   EXSR      CLCPAG
0424009402014-   C                   ENDIF
0425009402013-   C                   ENDIF
042600940131     C*
0427009401272-   C                   ENDDO
042800940127     C*
0429009401311-   C                   ENDIF
043000940131     C*
043100940127     C                   ENDSR
043200940127     C/EJECT
043300940127     C************************************************************
043400940131     C* CONTROLLO CAMPI I/O RIGA LISTA
043500940127     C************************************************************
043600940131     C     RECS1         BEGSR
043700940131     C*
043800940201     C* reset indicatori DSPATR
043900940201     C                   MOVE      *ALL'0'       IN4049           10
044000940201     C                   MOVEA     IN4049        *IN(40)
044100940201     C*
044200040930     C*  se richiamato x selezionare un codice 11 = *on
044300040930     C*   non deve utilizzare le opzioni di modifica
044400040930     C*  e viceversa
044500040930     c     *in11         ifeq      *on
0446000409303    C     S1OPZ         IFeq      2
0447000409303    C     S1OPZ         OReq      3
0448000409303    C     S1OPZ         OReq      4
044900040930     c                   seton                                        41  99
045000040930     C                   END
045100040930      * selezionato un codice da restituire al chiamante
0452000409303    C     S1OPZ         IFeq      1
045300040930     c                   move      s1cod         x1cod
045400040930     c                   clear                   kpjbu
045500040930     c                   movel     x1cod         kpjbu
045600040930     C                   MOVEL     *ON           $ESCI
045700040930     C                   MOVEL     *ON           $fine
045800040930     C                   END
045900040930     C                   END
046000040930     C*
046100040930     c     *in11         ifeq      *off
0462000409303    C     S1OPZ         ANDeq     1
046300040930     c                   seton                                        41  99
046400040930     C                   END
046500040930     C*
046600940131     C                   ENDSR
046700940131     C************************************************************
046800940131     C* GESTIONE OPZIONI LISTA
046900940131     C************************************************************
047000940131     C     OPZS1         BEGSR
047100940201     C*
0472000409302    C     *IN11         IFEQ      *Off
0473000409302    C     *IN11         oreq      *On
0474000409302    C     s1opz         andeq     05
047500040930     C*
047600030113     C                   RESET                   tabds
047700110902     C                   MOVE      *all'0'       xtaopz
047800110902     C                   MOVE      S1OPZ         xtaopz
047900030113     C                   MOVE      *ZERO         xtaret
048000030113     C                   MOVE      *ZERO         xtaopr
048100110902     C                   MOVEl     S1COD         xtakey
048200940715     C                   MOVE      *BLANKS       KPJBU
048300030113     C                   MOVEL     tabds         KPJBU
048400120412$004 C                   CALL      'TNTB93R2'
048500940607     C                   PARM                    KPJBA
048600030114     C                   MOVEL     KPJBU         tabds
048700940201     C*
048800940223     C* ritorno da PGM gestione
048900940223     C                   EXSR      GESRET
049000040930     C*
049100940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0492009402252    C     *IN99         IFEQ      *ON
049300940223     C                   SETON                                        40
0494009402252-   C                   ENDIF
049500940225     C*
0496000409302-   C                   ENDIF
049700040930     C*
049800940131     C                   ENDSR
049900940223     C/EJECT
050000940223     C************************************************************
050100940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
050200940223     C************************************************************
050300940223     C     GESRET        BEGSR
050400940223     C*
050500940223     C* modo di ritorno
050600940223     C*
0507009402231    C                   SELECT
050800940314    >C* << questi modi di utilizzo dei valori di ritorno dal
050900940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
051000940314    >C*    proposte, normalmente sempre valide, ma modificabili
051100940314    >C*    per situazioni particolari >>
051200940223     C* 1 = F3
051300030113    >C     xtaret        WHENEQ    '1'
051400940224     C                   MOVE      *ON           $ESCI
051500940223     C                   MOVE      *ON           $FINE
051600940223     C* 2 = F12
051700030113    >C     xtaret        WHENEQ    '2'
051800940223     C                   MOVE      *ON           $ESCI
051900940223     C*
0520009402231-   C                   ENDSL
052100940223     C*
052200940223     C* operazione eseguite dal pgm chiamato
052300940223     C*
0524009402231    C                   SELECT
052500940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
052600030113    >C     xtaopr        WHENEQ    '1'
052700940223     C                   MOVE      *ON           WINZS1
052800940223     C*
0529009402231-   C                   ENDSL
053000940223     C*
053100940223     C* funzione non eseguibile per errore :
053200940223     C*
0533009402231    C                   SELECT
053400940223     C* 1 = funzione richiamata chiusa in errore
053500940316    >C*  eventualmente gestire altri codici di errore
053600030113    >C     xtaerr        WHENEQ    '1'
053700940223     C                   MOVE      *ON           $ESCI
053800940223     C                   SETON                                        5299
053900940223     C*
0540009402231-   C                   ENDSL
054100940223     C*
054200940223     C                   ENDSR
054300940223     C/EJECT
054400940131     C************************************************************
054500940131     C* OPERAZIONI INIZIALI
054600940131     C************************************************************
054700940131     C     *INZSR        BEGSR
054800030113     C*
054900030113     C* Reperimento parametri
055000030113     C     *ENTRY        PLIST
055100030113     C                   PARM                    KPJBA
055200040930     C                   movel     kpjbu         Selez             1
055300040930     C*
055400040930     C* Se chiamato x selezionare un codice
055500040930     c                   clear                   x1cod
055600040930     c                   if        selez = '?'
055700040930     c                   seton                                        11
055800040930     c                   end
055900030113     C*
056000030113     C* Variabili per gestione videate
056100030113     C                   MOVE      *BLANK        $GEST             2
056200030113     C                   MOVE      *BLANK        $FINE             1
056300030113     C                   MOVE      *BLANK        $INZS1            1
056400030113     C                   MOVE      *BLANK        $EFILE            1
056500030113     C                   MOVE      *BLANK        $ESCI             1
056600030113     C                   MOVE      *BLANK        $RCDOK            1
056700030113     C*
056800030113     C     KTBL          KLIST
056900110926     C                   KFLD                    KCOD
057000030113     C* Indici
057100030113     C                   Z-ADD     0             X                 3 0
057200030113     C                   Z-ADD     0             Y                 3 0
057300940506     C*
057400120412     C                   move      'MIC'         KCOD
057500940127     C*
057600940117     C                   ENDSR
057700030113     C************************************************************
