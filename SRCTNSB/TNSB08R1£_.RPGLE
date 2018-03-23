000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
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
015702120412     C     Wsfl          IFgt      Wmax
015703120412     c                   z-add     wmax          Wsfl
015704120412     c                   end
015800030114     C     Wsfl          IFgt      0
015900030114     C                   Z-ADD     wsfl          C1RCD
016000030114     C                   Else
016100030114     C     Wpag          IFgt      0
016200030114     C                   Z-ADD     wpag          C1RCD
016300030114     C                   EndIF
016400030114     C                   EndIF
016500030114     C                   ENDIF
016600940127     C*
016700030113     C*              *------------------*
016800940607     C                   EXFMT     C1
016900030113     C*              *------------------*
017000030113     C*
017100940204     C     C1NRR         IFNE      0
017200940204     C                   Z-ADD     C1NRR         WSFL
017300940204     C                   ENDIF
017400940127     C                   Z-ADD     SFLNRR        C1RCD
017500030113     C* Selezioni
0176009401271    C                   SELECT
017700940127     C* F3=Fine
017800940506     C     $TASTO        WHENEQ    F03
017900940309     C                   EXSR      F03S1
018000940131     C* F10=Immissione
018100940506     C     $TASTO        WHENEQ    F10
018200940309     C                   EXSR      F10S1
0183009401271O   C                   OTHER
018400940127     C* CONTROLLO DATI
018500940131     C                   EXSR      CTRC1
018600940201     C     *IN99         IFEQ      *OFF
018700940131     C                   EXSR      CTRS1
018800940131     C                   END
0189009401271-   C                   ENDSL
019000940127     C*
019100940127     C                   ENDSR
019200940224     C/EJECT
019300940127     C************************************************************
019400940131     C* INIZIALIZZAZIONE LISTA
019500940127     C************************************************************
019600940127     C     INZS1         BEGSR
019700940302     C* pulizia SFL
019800940128     C                   SETOFF                                         3031
019900940607     C                   WRITE     C1
020000940128     C                   SETON                                          31
020100940128     C*
020200030113     C* CARICAMENTO SFL totale
020300940201     C                   Z-ADD     0             S1NRR
020400030113     C                   Z-ADD     1             C1RCD
020500940128     C                   Z-ADD     0             WMAX
020501120412     C                   Z-ADD     1             WPAG
020600940224     C*
020700940224     C* Posizionamento su file pilota
020800110926     c     kTBL          setll     tntbe01l
020900940608    >C                   EXSR      REDANA
021000030113     C* Carico il SFL
021100940127     C                   EXSR      ROLS1
021200030113     C*
021300030114     c                   if        xtaopr <> '1'
021400030114     C                   Z-ADD     1             WPAG
021500030114     c                   end
021600940127     C*
021700940127     C                   ENDSR
021800940127     C************************************************************
021900940131     C* CARICAMENTO PAGINA LISTA
022000940127     C************************************************************
022100940127     C     ROLS1         BEGSR
022200940127     C*
022300940128     C                   SETOFF                                       32
022400940223     C                   Z-ADD     0             Y
022500940127     C                   Z-ADD     WMAX          S1NRR
022600940127     C*
022700940127     C* Leggo dal file anagrafico per caricare la lista
0228009401311    C     $EFILE        DOWEQ     *OFF
022900940127     C*
023000030113     c                   clear                   s1opz
023100120412     c                   clear                   DESfilOBB
023101120412     c                   clear                   DESfil
023102120412     c                   clear                   Obbiettivo        3
023200110901     c                   move      *all'0'       s1cod
023300110926     c                   movel     tbeke1        s1cod
023400110926     c                   move      s1cod         d1fil             3 0
023500110926      **
023600110926     c     d1FIL         chain     azorg01l
023700110926     c                   if        %Found(azorg01l)
023800120412     c                   movel     orgdes        DESfil           25
023900110926     c                   end
023901120412      **
024000110926     c                   movel     tbeuni        extds
024001120412     c                   if        B93MMIC  <> *blank
024002120412     c                   eval      obbiettivo = B93MMIC
024003120412     c                   else
024004120412     c                   eval      obbiettivo = '000'
024005120412     c                   end
024006120412     c                   move      obbiettivo    obbiet            3 0
024100030113     C*
024101120412     c                   eval      DESfilOBB = Desfil + '  ' +
024102120412     c                             %editc(Obbiet:'4')
024103120412     C*
024200030115     c                   clear                   S1Ann
024300940127     C*
024400940127     C                   ADD       1             S1NRR
024500940127     C                   ADD       1             Y
024600940127     C*
024700940607     C                   WRITE     S1
024800940131     C*
024900940316    >C                   EXSR      REDANA
025000940128     C*
0251009401271-   C                   ENDDO
025200940127     C*
025300940223     C                   Z-ADD     S1NRR         WMAX                 30
025400940127     C*
025500940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
025600940127     C*
025700030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
025800940127     C                   MVR                     RESTO             3 0
025900030114     C     PAGINE        MULT      SFLPAG        C1RCD
0260000301141    C     RESTO         IFGT      0
026100030114     C                   ADD       1             C1RCD
0262000301141E   C                   ELSE
026300030114     C                   SUB       SFLPAG        C1RCD
026400030114     C                   ADD       1             C1RCD
0265000301141-   C                   ENDIF
026600940128     C*
026700940127     C                   ENDSR
026800940128     C************************************************************
026900940131     C* LETTURA RCD ARCHIVIO PILOTA
027000940128     C************************************************************
027100940607     C     REDANA        BEGSR
027200940128     C*
027300940131     C                   MOVEL     *OFF          $EFILE
027400940131     C                   MOVEL     *OFF          $RCDOK
027500940131     C*
0276009401311    C     $EFILE        DOUEQ     *ON
027700940131     C     $RCDOK        OREQ      *ON
027800940131     C*
027900110926     c     kTBL          Reade     tntbe01l
028000110926     c                   if        %eof(tntbe01l)
028100030113     C                   MOVEL     *ON           $EFILE
028200030113     C                   MOVE      $EFILE        *IN33
028300030113     c                   else
028400030113     C                   MOVE      *ON           $RCDOK
028500030113     c                   end
028600940131     C*
0287009401311-   C                   ENDDO
028800940131     C*
028900940131     C                   ENDSR
029000940224     C************************************************************
029100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
029200940224     C************************************************************
029300940224     C     CLCPAG        BEGSR
029400940224     C* Input :
029500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
029600940224     C*          cursore
029700940224     C* - SFLPAG = numero rcd per pagina sfl
029800940224     C* Output :
029900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
030000940224     C*
030001120412     c                   if        WSFL >0
030100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
030200940224     C                   MVR                     RESTO             3 0
030300940224     C     RESTO         IFGT      0
030400940224     C                   ADD       1             PAGINE
030500940224     C                   ENDIF
030600940224     C     PAGINE        MULT      SFLPAG        WPAG
030601120412     c                   else
030602120412     C                   Z-ADD     1             WPAG
030603120412     c                   end
030700940224     C*
030800940224     C                   ENDSR
030900940309     C************************************************************
031000940309     C* GESTIONE F03 VIDEO S1
031100940309     C************************************************************
031200940309     C     F03S1         BEGSR
031300940309     C*
031400940309     C                   MOVE      *ON           $FINE
031500940325     C* fine programma
031600940309     C                   ENDSR
031700940309     C/EJECT
031800940309     C************************************************************
031900940309     C* GESTIONE F10 VIDEO S1
032000940314     c* AGGIUNTA RECORD
032100940309     C************************************************************
032200940309     C     F10S1         BEGSR
032300940309     C*
032400030113     C                   RESET                   tabds
032500030113     C                   MOVEL     '01'          xtaopz
032600030113     C                   MOVE      *ZERO         xtaret
032700030113     C                   MOVE      *ZERO         xtaopr
032800030114     C                   MOVEl     *blank        xtakey
032900030113     C                   MOVE      *BLANKS       KPJBU
033000030113     C                   MOVEL     tabds         KPJBU
033100120412$004 C                   CALL      'TNTB93R2'
033200030113     C                   PARM                    KPJBA
033300030114     C                   MOVEL     KPJBU         tabds
033400030113      *
033500940309     C* ritorno da PGM gestione
033600940309     C                   EXSR      GESRET
033700940309     C     WINZS1        IFEQ      *ON
033800940309     C                   MOVE      *ON           $INZS1
033900940309     C* se esistevano già righe sul sfl
034000940309     C* calcolo pagina a cui deve posizionarsi
034100940309     C     WSFL          IFGT      0
034200940309     C                   EXSR      CLCPAG
034300940309     C* altrimenti carico solo la 1a pagina
034400940309     C                   ELSE
034500940309     C                   Z-ADD     SFLPAG        WPAG
034600940309     C                   ENDIF
034700940309     C                   ENDIF
034800940309     C*
034900940309     C                   ENDSR
035000940128     C************************************************************
035100940131     C* CONTROLLO TESTATA LISTA
035200940128     C************************************************************
035300940131     C     CTRC1         BEGSR
035400940128     C*
035500940201     C                   MOVE      *OFF          *IN99
035600940131     C*
035700940202     C                   ENDSR
035800940131     C************************************************************
035900940131     C* CONTROLLO OPZIONI LISTA
036000940131     C************************************************************
036100940131     C     CTRS1         BEGSR
036200940131     C*
036300940202     C                   MOVEL     *OFF          $ESCI
036400940201     C                   SETOFF                                       99
036500940131     C                   Z-ADD     0             S1OPZ
036600040930     c                   clear                   x1cod
036700940131     C*
036800940127     C* Leggo il sfl solo se ci sono rcd
0369009401311    C     WMAX          IFGT      0
037000940607     C                   READC     S1                                     21
037100940127     C*
037200940131     C* esce se fine sfl o errore che richiede l'uscita
0373009401312    C     *IN21         DOWEQ     *OFF
037400940131     C     $ESCI         ANDEQ     *OFF
037500940201     C                   Z-ADD     S1NRR         C1RCD
037600940131     C* ctrl su riga
037700940131     C                   EXSR      RECS1
037800940131     C* gestione opzioni
0379009401313    C     S1OPZ         IFNE      0
038000940201     C     *IN99         ANDEQ     *OFF
038100940131     C                   EXSR      OPZS1
0382009401273-   C                   ENDIF
038300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0384009402013    C     *IN99         IFEQ      *ON
038500940201     C     $ESCI         OREQ      *ON
038600940131     C                   MOVE      *ON           *IN32
038700940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038800940204     C* la ripristinerò a conclusione del ciclo di READC
038900940223     C                   MOVE      *OFF          $INZS1
0390009402233-   C                   ENDIF
039100940223     C*
039200940131     C                   Z-ADD     0             S1OPZ
039300940223     C*
039400940607     C                   UPDATE    S1
039500940223     C*
039600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0397009401313    C     $ESCI         IFEQ      *OFF
039800940607     C                   READC     S1                                     21
039900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0400009402014    C     *IN21         IFEQ      *ON
040100940201     C                   MOVE      WINZS1        $INZS1
040200940204     C* calcolo pagina a cui deve posizionarsi
040300940224     C                   EXSR      CLCPAG
0404009402014-   C                   ENDIF
0405009402013-   C                   ENDIF
040600940131     C*
0407009401272-   C                   ENDDO
040800940127     C*
0409009401311-   C                   ENDIF
041000940131     C*
041100940127     C                   ENDSR
041200940127     C/EJECT
041300940127     C************************************************************
041400940131     C* CONTROLLO CAMPI I/O RIGA LISTA
041500940127     C************************************************************
041600940131     C     RECS1         BEGSR
041700940131     C*
041800940201     C* reset indicatori DSPATR
041900940201     C                   MOVE      *ALL'0'       IN4049           10
042000940201     C                   MOVEA     IN4049        *IN(40)
042100940201     C*
042200040930     C*  se richiamato x selezionare un codice 11 = *on
042300040930     C*   non deve utilizzare le opzioni di modifica
042400040930     C*  e viceversa
042500040930     c     *in11         ifeq      *on
0426000409303    C     S1OPZ         IFeq      2
0427000409303    C     S1OPZ         OReq      3
0428000409303    C     S1OPZ         OReq      4
042900040930     c                   seton                                        41  99
043000040930     C                   END
043100040930      * selezionato un codice da restituire al chiamante
0432000409303    C     S1OPZ         IFeq      1
043300040930     c                   move      s1cod         x1cod
043400040930     c                   clear                   kpjbu
043500040930     c                   movel     x1cod         kpjbu
043600040930     C                   MOVEL     *ON           $ESCI
043700040930     C                   MOVEL     *ON           $fine
043800040930     C                   END
043900040930     C                   END
044000040930     C*
044100040930     c     *in11         ifeq      *off
0442000409303    C     S1OPZ         ANDeq     1
044300040930     c                   seton                                        41  99
044400040930     C                   END
044500040930     C*
044600940131     C                   ENDSR
044700940131     C************************************************************
044800940131     C* GESTIONE OPZIONI LISTA
044900940131     C************************************************************
045000940131     C     OPZS1         BEGSR
045100940201     C*
0452000409302    C     *IN11         IFEQ      *Off
0453000409302    C     *IN11         oreq      *On
0454000409302    C     s1opz         andeq     05
045500040930     C*
045600030113     C                   RESET                   tabds
045700110902     C                   MOVE      *all'0'       xtaopz
045800110902     C                   MOVE      S1OPZ         xtaopz
045900030113     C                   MOVE      *ZERO         xtaret
046000030113     C                   MOVE      *ZERO         xtaopr
046100110902     C                   MOVEl     S1COD         xtakey
046200940715     C                   MOVE      *BLANKS       KPJBU
046300030113     C                   MOVEL     tabds         KPJBU
046400120412$004 C                   CALL      'TNTB93R2'
046500940607     C                   PARM                    KPJBA
046600030114     C                   MOVEL     KPJBU         tabds
046700940201     C*
046800940223     C* ritorno da PGM gestione
046900940223     C                   EXSR      GESRET
047000040930     C*
047100940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0472009402252    C     *IN99         IFEQ      *ON
047300940223     C                   SETON                                        40
0474009402252-   C                   ENDIF
047500940225     C*
0476000409302-   C                   ENDIF
047700040930     C*
047800940131     C                   ENDSR
047900940223     C/EJECT
048000940223     C************************************************************
048100940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
048200940223     C************************************************************
048300940223     C     GESRET        BEGSR
048400940223     C*
048500940223     C* modo di ritorno
048600940223     C*
0487009402231    C                   SELECT
048800940314    >C* << questi modi di utilizzo dei valori di ritorno dal
048900940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
049000940314    >C*    proposte, normalmente sempre valide, ma modificabili
049100940314    >C*    per situazioni particolari >>
049200940223     C* 1 = F3
049300030113    >C     xtaret        WHENEQ    '1'
049400940224     C                   MOVE      *ON           $ESCI
049500940223     C                   MOVE      *ON           $FINE
049600940223     C* 2 = F12
049700030113    >C     xtaret        WHENEQ    '2'
049800940223     C                   MOVE      *ON           $ESCI
049900940223     C*
0500009402231-   C                   ENDSL
050100940223     C*
050200940223     C* operazione eseguite dal pgm chiamato
050300940223     C*
0504009402231    C                   SELECT
050500940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
050600030113    >C     xtaopr        WHENEQ    '1'
050700940223     C                   MOVE      *ON           WINZS1
050800940223     C*
0509009402231-   C                   ENDSL
051000940223     C*
051100940223     C* funzione non eseguibile per errore :
051200940223     C*
0513009402231    C                   SELECT
051400940223     C* 1 = funzione richiamata chiusa in errore
051500940316    >C*  eventualmente gestire altri codici di errore
051600030113    >C     xtaerr        WHENEQ    '1'
051700940223     C                   MOVE      *ON           $ESCI
051800940223     C                   SETON                                        5299
051900940223     C*
0520009402231-   C                   ENDSL
052100940223     C*
052200940223     C                   ENDSR
052300940223     C/EJECT
052400940131     C************************************************************
052500940131     C* OPERAZIONI INIZIALI
052600940131     C************************************************************
052700940131     C     *INZSR        BEGSR
052800030113     C*
052900030113     C* Reperimento parametri
053000030113     C     *ENTRY        PLIST
053100030113     C                   PARM                    KPJBA
053200040930     C                   movel     kpjbu         Selez             1
053300040930     C*
053400040930     C* Se chiamato x selezionare un codice
053500040930     c                   clear                   x1cod
053600040930     c                   if        selez = '?'
053700040930     c                   seton                                        11
053800040930     c                   end
053900030113     C*
054000030113     C* Variabili per gestione videate
054100030113     C                   MOVE      *BLANK        $GEST             2
054200030113     C                   MOVE      *BLANK        $FINE             1
054300030113     C                   MOVE      *BLANK        $INZS1            1
054400030113     C                   MOVE      *BLANK        $EFILE            1
054500030113     C                   MOVE      *BLANK        $ESCI             1
054600030113     C                   MOVE      *BLANK        $RCDOK            1
054700030113     C*
054800030113     C     KTBL          KLIST
054900110926     C                   KFLD                    KCOD
055000030113     C* Indici
055100030113     C                   Z-ADD     0             X                 3 0
055200030113     C                   Z-ADD     0             Y                 3 0
055300940506     C*
055400120412     C                   move      'MIC'         KCOD
055500940127     C*
055600940117     C                   ENDSR
055700030113     C************************************************************
