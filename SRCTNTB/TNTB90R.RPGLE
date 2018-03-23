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
002000110926     Ftntbe01L  IF   E           K DISK
002100110926     Fazorg01L  IF   E           K DISK
002200110927     FTntb90D   CF   E             WORKSTN
002300940607     F                                     SFILE(S1:S1NRR)
002400940201     F                                     INFDS(DSFMT)
002500940128     D*----------------------------------------------------*
002600940211     D* Passaggio Parametri
002700940211     D KPJBA         E DS
002800030113      *-------------
002900940325     D* Parametri in ricezione
003000030113     D  TABDS          DS
003100030113     D  XTAOPZ                 1      2
003200030113     D  XTARET                 3      3
003300030113     D  XTAOPR                 4      4
003400030113     D  XTAERR                 5      5
003500110926     D  XTAKEY                 6      8
003600940211     D*-------------
003700940211     D DSFMT           DS           512
003800940506     D  $TASTO               369    369
003900940211     D  NRG                  370    370
004000940211     D  NCL                  371    371
004100940211     D  SFLNRR               378    379B 0
004200940207     D*-------------
004300940207     D* Nome PGM a video
004400940207     D                 DS
004500940207     D  PROGR                  1     10
004600940207     D  ASTER1                 1      1    INZ('*')
004700940207     D  SIGLA                  2      9
004800940207     D  ASTER2                10     10    INZ('*')
004900940127     D*-------------
005000940127     D* Reperimento nome PGM
005100940127     D STATUS         SDS           333
005200940127     D  DSPGM            *PROC
005300940607     D* descrizione tabella
005400110926$003 D EXTDS         E DS                  EXTNAME(tntb90ds)
005500030113     D*-------------
005600030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005700030113$003 D S1NRR           S                   Like(C1rcd)
005800030113$003 D WSfl            S                   Like(C1nrr)
005900030113$003 D Wmax            S                   Like(C1rcd)
006000030113$003 D Wpag            S                   Like(C1rcd)
006100030113$003 D Winzs1          S                   Like($inzs1)
006200040930$003 D x1cod           S                   Like(s1cod)
006300110926     D KCOD            S                   LIKE(TBECOD) inz('WEK')
006400110926     D KKE1            S                   LIKE(TBEKE1)
006500940207     D*-------------
006600940211     D* COSTANTI
006700940211     D*-------------
006800030113     D SFLPAG          C                   CONST(11)
006900940314     D* dimensione della schiera $MS1
007000940506     D*
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
009900940207     I*-------------
010000940607     IS1
010100110926$015 I              S1DES                       DESFIL
010200940127     C*----------------------------------------------------*
010300940127     C*                MAIN LINE PROGRAM
010400940127     C*----------------------------------------------------*
010500940223     C* inizializzazione variabili
010600940223     C                   EXSR      INZVAR
010700940223     C*
010800940223     C     $FINE         DOWEQ     *OFF
010900940131     C     $GEST         CASEQ     'S1'          GESS1
011000940117     C                   END
011100940117     C                   END
011200940325     C* fine programma
011300940325     C                   SETON                                            LR
011400030113     C************************************************************
011500030113     C* INIZIALIZZAZIONE VARIABILI
011600030113     C************************************************************
011700030113     C     INZVAR        BEGSR
011800030113     C*
011900030113     C* Pulizia campi e indicatori
012000030113     C                   MOVE      *ALL'0'       IN4049           10
012100030113     C                   MOVEA     IN4049        *IN(40)
012200030113     C                   CLEAR                   S1OPZ
012300030113     C* Variabili per gestione videate
012400030113     C*
012500030113     C                   MOVE      *OFF          $FINE
012600030113     C                   MOVE      *OFF          $INZS1
012700030113     C                   MOVE      *OFF          $EFILE
012800030113     C                   MOVE      *OFF          $ESCI
012900030113     C                   MOVE      *OFF          $RCDOK
013000030113     C                   Z-ADD     0             $ULKS1            3 0
013100030113     C*
013200030113     C                   MOVE      *ON           $INZS1
013300030113     C                   MOVE      'S1'          $GEST
013400030113     C*
013500030113     C* Variabili appoggio
013600030114     C                   Z-ADD     1             WPAG
013700030113     c*
013800030113     C                   ENDSR
013900940127     C************************************************************
014000940131     C* GESTIONE LISTA
014100940127     C************************************************************
014200940127     C     GESS1         BEGSR
014300030113     C*
014400940223     C* inizializzazione videata
014500940223     C     $INZS1        IFEQ      *ON
014600940127     C                   EXSR      INZS1
014700940223     C                   MOVE      *OFF          $INZS1
014800940127     C                   ENDIF
014900030113     C*
015000030113     C* emissione piede videata
015100030113     C                   WRITE     Z1
015200030113     C* Non ci sono records
015300940223     C     WMAX          IFEQ      0
015400940607     C                   WRITE     D1
015500030114     C                   Else
015600120412     C     Wsfl          IFgt      Wmax
015700120412     c                   z-add     wmax          Wsfl
015800120412     c                   end
015900030114     C     Wsfl          IFgt      0
016000030114     C                   Z-ADD     wsfl          C1RCD
016100030114     C                   Else
016200030114     C     Wpag          IFgt      0
016300030114     C                   Z-ADD     wpag          C1RCD
016400030114     C                   EndIF
016500030114     C                   EndIF
016600030114     C                   ENDIF
016700940127     C*
016800030113     C*              *------------------*
016900940607     C                   EXFMT     C1
017000030113     C*              *------------------*
017100030113     C*
017200940204     C     C1NRR         IFNE      0
017300940204     C                   Z-ADD     C1NRR         WSFL
017400940204     C                   ENDIF
017500940127     C                   Z-ADD     SFLNRR        C1RCD
017600030113     C* Selezioni
0177009401271    C                   SELECT
017800940127     C* F3=Fine
017900940506     C     $TASTO        WHENEQ    F03
018000940309     C                   EXSR      F03S1
018100940131     C* F10=Immissione
018200940506     C     $TASTO        WHENEQ    F10
018300940309     C                   EXSR      F10S1
0184009401271O   C                   OTHER
018500940127     C* CONTROLLO DATI
018600940131     C                   EXSR      CTRC1
018700940201     C     *IN99         IFEQ      *OFF
018800940131     C                   EXSR      CTRS1
018900940131     C                   END
0190009401271-   C                   ENDSL
019100940127     C*
019200940127     C                   ENDSR
019300940224     C/EJECT
019400940127     C************************************************************
019500940131     C* INIZIALIZZAZIONE LISTA
019600940127     C************************************************************
019700940127     C     INZS1         BEGSR
019800940302     C* pulizia SFL
019900940128     C                   SETOFF                                         3031
020000940607     C                   WRITE     C1
020100940128     C                   SETON                                          31
020200940128     C*
020300030113     C* CARICAMENTO SFL totale
020400940201     C                   Z-ADD     0             S1NRR
020500030113     C                   Z-ADD     1             C1RCD
020600940128     C                   Z-ADD     0             WMAX
020700120412     C                   Z-ADD     1             WPAG
020800940224     C*
020900940224     C* Posizionamento su file pilota
021000110926     c     kTBL          setll     tntbe01l
021100940608    >C                   EXSR      REDANA
021200030113     C* Carico il SFL
021300940127     C                   EXSR      ROLS1
021400030113     C*
021500030114     c                   if        xtaopr <> '1'
021600030114     C                   Z-ADD     1             WPAG
021700030114     c                   end
021800940127     C*
021900940127     C                   ENDSR
022000940127     C************************************************************
022100940131     C* CARICAMENTO PAGINA LISTA
022200940127     C************************************************************
022300940127     C     ROLS1         BEGSR
022400940127     C*
022500940128     C                   SETOFF                                       32
022600940223     C                   Z-ADD     0             Y
022700940127     C                   Z-ADD     WMAX          S1NRR
022800940127     C*
022900940127     C* Leggo dal file anagrafico per caricare la lista
0230009401311    C     $EFILE        DOWEQ     *OFF
023100940127     C*
023200030113     c                   clear                   s1opz
023300110926     c                   clear                   DESfil
023400110901     c                   move      *all'0'       s1cod
023500110926     c                   movel     tbeke1        s1cod
023600110926     c                   move      s1cod         d1fil             3 0
023700110926      **
023800110926     c     d1FIL         chain     azorg01l
023900110926     c                   if        %Found(azorg01l)
024000110926     c                   movel     orgdes        DESfil
024100110926     c                   end
024200110926     c                   movel     tbeuni        extds
024300030113     C*
024400030115     c                   clear                   S1Ann
024500940127     C*
024600940127     C                   ADD       1             S1NRR
024700940127     C                   ADD       1             Y
024800940127     C*
024900940607     C                   WRITE     S1
025000940131     C*
025100940316    >C                   EXSR      REDANA
025200940128     C*
0253009401271-   C                   ENDDO
025400940127     C*
025500940223     C                   Z-ADD     S1NRR         WMAX                 30
025600940127     C*
025700940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
025800940127     C*
025900030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
026000940127     C                   MVR                     RESTO             3 0
026100030114     C     PAGINE        MULT      SFLPAG        C1RCD
0262000301141    C     RESTO         IFGT      0
026300030114     C                   ADD       1             C1RCD
0264000301141E   C                   ELSE
026500030114     C                   SUB       SFLPAG        C1RCD
026600030114     C                   ADD       1             C1RCD
0267000301141-   C                   ENDIF
026800940128     C*
026900940127     C                   ENDSR
027000940128     C************************************************************
027100940131     C* LETTURA RCD ARCHIVIO PILOTA
027200940128     C************************************************************
027300940607     C     REDANA        BEGSR
027400940128     C*
027500940131     C                   MOVEL     *OFF          $EFILE
027600940131     C                   MOVEL     *OFF          $RCDOK
027700940131     C*
0278009401311    C     $EFILE        DOUEQ     *ON
027900940131     C     $RCDOK        OREQ      *ON
028000940131     C*
028100110926     c     kTBL          Reade     tntbe01l
028200110926     c                   if        %eof(tntbe01l)
028300030113     C                   MOVEL     *ON           $EFILE
028400030113     C                   MOVE      $EFILE        *IN33
028500030113     c                   else
028600030113     C                   MOVE      *ON           $RCDOK
028700030113     c                   end
028800940131     C*
0289009401311-   C                   ENDDO
029000940131     C*
029100940131     C                   ENDSR
029200940224     C************************************************************
029300940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
029400940224     C************************************************************
029500940224     C     CLCPAG        BEGSR
029600940224     C* Input :
029700940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
029800940224     C*          cursore
029900940224     C* - SFLPAG = numero rcd per pagina sfl
030000940224     C* Output :
030100940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
030200940224     C*
030300120412     c                   if        WSFL >0
030400940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
030500940224     C                   MVR                     RESTO             3 0
030600940224     C     RESTO         IFGT      0
030700940224     C                   ADD       1             PAGINE
030800940224     C                   ENDIF
030900940224     C     PAGINE        MULT      SFLPAG        WPAG
031000120412     c                   else
031100120412     C                   Z-ADD     1             WPAG
031200120412     c                   end
031300940224     C*
031400940224     C                   ENDSR
031500940309     C************************************************************
031600940309     C* GESTIONE F03 VIDEO S1
031700940309     C************************************************************
031800940309     C     F03S1         BEGSR
031900940309     C*
032000940309     C                   MOVE      *ON           $FINE
032100940325     C* fine programma
032200940309     C                   ENDSR
032300940309     C/EJECT
032400940309     C************************************************************
032500940309     C* GESTIONE F10 VIDEO S1
032600940314     c* AGGIUNTA RECORD
032700940309     C************************************************************
032800940309     C     F10S1         BEGSR
032900940309     C*
033000030113     C                   RESET                   tabds
033100030113     C                   MOVEL     '01'          xtaopz
033200030113     C                   MOVE      *ZERO         xtaret
033300030113     C                   MOVE      *ZERO         xtaopr
033400030114     C                   MOVEl     *blank        xtakey
033500030113     C                   MOVE      *BLANKS       KPJBU
033600030113     C                   MOVEL     tabds         KPJBU
033700110927$004 C                   CALL      'TNTB90R2'
033800030113     C                   PARM                    KPJBA
033900030114     C                   MOVEL     KPJBU         tabds
034000030113      *
034100940309     C* ritorno da PGM gestione
034200940309     C                   EXSR      GESRET
034300940309     C     WINZS1        IFEQ      *ON
034400940309     C                   MOVE      *ON           $INZS1
034500940309     C* se esistevano già righe sul sfl
034600940309     C* calcolo pagina a cui deve posizionarsi
034700940309     C     WSFL          IFGT      0
034800940309     C                   EXSR      CLCPAG
034900940309     C* altrimenti carico solo la 1a pagina
035000940309     C                   ELSE
035100940309     C                   Z-ADD     SFLPAG        WPAG
035200940309     C                   ENDIF
035300940309     C                   ENDIF
035400940309     C*
035500940309     C                   ENDSR
035600940128     C************************************************************
035700940131     C* CONTROLLO TESTATA LISTA
035800940128     C************************************************************
035900940131     C     CTRC1         BEGSR
036000940128     C*
036100940201     C                   MOVE      *OFF          *IN99
036200940131     C*
036300940202     C                   ENDSR
036400940131     C************************************************************
036500940131     C* CONTROLLO OPZIONI LISTA
036600940131     C************************************************************
036700940131     C     CTRS1         BEGSR
036800940131     C*
036900940202     C                   MOVEL     *OFF          $ESCI
037000940201     C                   SETOFF                                       99
037100940131     C                   Z-ADD     0             S1OPZ
037200040930     c                   clear                   x1cod
037300940131     C*
037400940127     C* Leggo il sfl solo se ci sono rcd
0375009401311    C     WMAX          IFGT      0
037600940607     C                   READC     S1                                     21
037700940127     C*
037800940131     C* esce se fine sfl o errore che richiede l'uscita
0379009401312    C     *IN21         DOWEQ     *OFF
038000940131     C     $ESCI         ANDEQ     *OFF
038100940201     C                   Z-ADD     S1NRR         C1RCD
038200940131     C* ctrl su riga
038300940131     C                   EXSR      RECS1
038400940131     C* gestione opzioni
0385009401313    C     S1OPZ         IFNE      0
038600940201     C     *IN99         ANDEQ     *OFF
038700940131     C                   EXSR      OPZS1
0388009401273-   C                   ENDIF
038900940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0390009402013    C     *IN99         IFEQ      *ON
039100940201     C     $ESCI         OREQ      *ON
039200940131     C                   MOVE      *ON           *IN32
039300940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
039400940204     C* la ripristinerò a conclusione del ciclo di READC
039500940223     C                   MOVE      *OFF          $INZS1
0396009402233-   C                   ENDIF
039700940223     C*
039800940131     C                   Z-ADD     0             S1OPZ
039900940223     C*
040000940607     C                   UPDATE    S1
040100940223     C*
040200940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0403009401313    C     $ESCI         IFEQ      *OFF
040400940607     C                   READC     S1                                     21
040500940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0406009402014    C     *IN21         IFEQ      *ON
040700940201     C                   MOVE      WINZS1        $INZS1
040800940204     C* calcolo pagina a cui deve posizionarsi
040900940224     C                   EXSR      CLCPAG
0410009402014-   C                   ENDIF
0411009402013-   C                   ENDIF
041200940131     C*
0413009401272-   C                   ENDDO
041400940127     C*
0415009401311-   C                   ENDIF
041600940131     C*
041700940127     C                   ENDSR
041800940127     C/EJECT
041900940127     C************************************************************
042000940131     C* CONTROLLO CAMPI I/O RIGA LISTA
042100940127     C************************************************************
042200940131     C     RECS1         BEGSR
042300940131     C*
042400940201     C* reset indicatori DSPATR
042500940201     C                   MOVE      *ALL'0'       IN4049           10
042600940201     C                   MOVEA     IN4049        *IN(40)
042700940201     C*
042800040930     C*  se richiamato x selezionare un codice 11 = *on
042900040930     C*   non deve utilizzare le opzioni di modifica
043000040930     C*  e viceversa
043100040930     c     *in11         ifeq      *on
0432000409303    C     S1OPZ         IFeq      2
0433000409303    C     S1OPZ         OReq      3
0434000409303    C     S1OPZ         OReq      4
043500040930     c                   seton                                        41  99
043600040930     C                   END
043700040930      * selezionato un codice da restituire al chiamante
0438000409303    C     S1OPZ         IFeq      1
043900040930     c                   move      s1cod         x1cod
044000040930     c                   clear                   kpjbu
044100040930     c                   movel     x1cod         kpjbu
044200040930     C                   MOVEL     *ON           $ESCI
044300040930     C                   MOVEL     *ON           $fine
044400040930     C                   END
044500040930     C                   END
044600040930     C*
044700040930     c     *in11         ifeq      *off
0448000409303    C     S1OPZ         ANDeq     1
044900040930     c                   seton                                        41  99
045000040930     C                   END
045100040930     C*
045200940131     C                   ENDSR
045300940131     C************************************************************
045400940131     C* GESTIONE OPZIONI LISTA
045500940131     C************************************************************
045600940131     C     OPZS1         BEGSR
045700940201     C*
0458000409302    C     *IN11         IFEQ      *Off
0459000409302    C     *IN11         oreq      *On
0460000409302    C     s1opz         andeq     05
046100040930     C*
046200030113     C                   RESET                   tabds
046300110902     C                   MOVE      *all'0'       xtaopz
046400110902     C                   MOVE      S1OPZ         xtaopz
046500030113     C                   MOVE      *ZERO         xtaret
046600030113     C                   MOVE      *ZERO         xtaopr
046700110902     C                   MOVEl     S1COD         xtakey
046800940715     C                   MOVE      *BLANKS       KPJBU
046900030113     C                   MOVEL     tabds         KPJBU
047000110927$004 C                   CALL      'TNTB90R2'
047100940607     C                   PARM                    KPJBA
047200030114     C                   MOVEL     KPJBU         tabds
047300940201     C*
047400940223     C* ritorno da PGM gestione
047500940223     C                   EXSR      GESRET
047600040930     C*
047700940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0478009402252    C     *IN99         IFEQ      *ON
047900940223     C                   SETON                                        40
0480009402252-   C                   ENDIF
048100940225     C*
0482000409302-   C                   ENDIF
048300040930     C*
048400940131     C                   ENDSR
048500940223     C/EJECT
048600940223     C************************************************************
048700940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
048800940223     C************************************************************
048900940223     C     GESRET        BEGSR
049000940223     C*
049100940223     C* modo di ritorno
049200940223     C*
0493009402231    C                   SELECT
049400940314    >C* << questi modi di utilizzo dei valori di ritorno dal
049500940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
049600940314    >C*    proposte, normalmente sempre valide, ma modificabili
049700940314    >C*    per situazioni particolari >>
049800940223     C* 1 = F3
049900030113    >C     xtaret        WHENEQ    '1'
050000940224     C                   MOVE      *ON           $ESCI
050100940223     C                   MOVE      *ON           $FINE
050200940223     C* 2 = F12
050300030113    >C     xtaret        WHENEQ    '2'
050400940223     C                   MOVE      *ON           $ESCI
050500940223     C*
0506009402231-   C                   ENDSL
050700940223     C*
050800940223     C* operazione eseguite dal pgm chiamato
050900940223     C*
0510009402231    C                   SELECT
051100940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
051200030113    >C     xtaopr        WHENEQ    '1'
051300940223     C                   MOVE      *ON           WINZS1
051400940223     C*
0515009402231-   C                   ENDSL
051600940223     C*
051700940223     C* funzione non eseguibile per errore :
051800940223     C*
0519009402231    C                   SELECT
052000940223     C* 1 = funzione richiamata chiusa in errore
052100940316    >C*  eventualmente gestire altri codici di errore
052200030113    >C     xtaerr        WHENEQ    '1'
052300940223     C                   MOVE      *ON           $ESCI
052400940223     C                   SETON                                        5299
052500940223     C*
0526009402231-   C                   ENDSL
052700940223     C*
052800940223     C                   ENDSR
052900940223     C/EJECT
053000940131     C************************************************************
053100940131     C* OPERAZIONI INIZIALI
053200940131     C************************************************************
053300940131     C     *INZSR        BEGSR
053400030113     C*
053500030113     C* Reperimento parametri
053600030113     C     *ENTRY        PLIST
053700030113     C                   PARM                    KPJBA
053800040930     C                   movel     kpjbu         Selez             1
053900040930     C*
054000040930     C* Se chiamato x selezionare un codice
054100040930     c                   clear                   x1cod
054200040930     c                   if        selez = '?'
054300040930     c                   seton                                        11
054400040930     c                   end
054500030113     C*
054600030113     C* Variabili per gestione videate
054700030113     C                   MOVE      *BLANK        $GEST             2
054800030113     C                   MOVE      *BLANK        $FINE             1
054900030113     C                   MOVE      *BLANK        $INZS1            1
055000030113     C                   MOVE      *BLANK        $EFILE            1
055100030113     C                   MOVE      *BLANK        $ESCI             1
055200030113     C                   MOVE      *BLANK        $RCDOK            1
055300030113     C*
055400030113     C     KTBL          KLIST
055500110926     C                   KFLD                    KCOD
055600030113     C* Indici
055700030113     C                   Z-ADD     0             X                 3 0
055800030113     C                   Z-ADD     0             Y                 3 0
055900940506     C*
056000110926     C                   move      'WEK'         KCOD
056100940127     C*
056200940117     C                   ENDSR
056300030113     C************************************************************
