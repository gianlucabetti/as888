000100130115     H  DECEDIT('0,') DATEDIT(*DMY.) OPTION(*NOXREF) CVTOPT(*DATETIME)
000200130115     H  DFTACTGRP(*NO)
000300130115     H*--------------------------------------------------------------*
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
002100110926     Ftntbe01L  IF   E           K DISK
002200110926     Fazorg01L  IF   E           K DISK
002300130115     FTntbZ79D  CF   E             WORKSTN
002400940607     F                                     SFILE(S1:S1NRR)
002500940201     F                                     INFDS(DSFMT)
002600940128     D*----------------------------------------------------*
002700940211     D* Passaggio Parametri
002800940211     D KPJBA         E DS
002900030113      *-------------
003000940325     D* Parametri in ricezione
003100030113     D  TABDS          DS
003200030113     D  XTAOPZ                 1      2
003300030113     D  XTARET                 3      3
003400030113     D  XTAOPR                 4      4
003500030113     D  XTAERR                 5      5
003600110926     D  XTAKEY                 6      8
003700130115     D  XTAKEY2                9     11
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
005600130115$003 D EXTDS         E DS                  EXTNAME(tntbZ79ds)
005700030113     D*-------------
005800030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005900030113$003 D S1NRR           S                   Like(C1rcd)
006000030113$003 D WSfl            S                   Like(C1nrr)
006100030113$003 D Wmax            S                   Like(C1rcd)
006200030113$003 D Wpag            S                   Like(C1rcd)
006300030113$003 D Winzs1          S                   Like($inzs1)
006400040930$003 D x1cod           S                   Like(s1cod)
006500130115$003 D x1cod2          S                   Like(s1cod2)
006600130115     D KCOD            S                   LIKE(TBECOD) inz('Z79')
006700110926     D KKE1            S                   LIKE(TBEKE1)
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
010400130115$015 I              S1DES                       NEWDATA
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
023600130115     c                   clear                   newDATA
023700130115     c                   clear                   DESfil           30
023800110901     c                   move      *all'0'       s1cod
023900110926     c                   movel     tbeke1        s1cod
024000130115     c                   movel     tbeke2        s1cod2
024100110926     c                   move      s1cod         d1fil             3 0
024200110926      **
024300110926     c     d1FIL         chain     azorg01l
024400110926     c                   if        %Found(azorg01l)
024500110926     c                   movel     orgdes        DESfil
024600110926     c                   end
024700130115      **
024800110926     c                   movel     tbeuni        extds
024900130115     c                   movel     Z79NEWSOC     newDATA
025000130115     c                   move      Z79DADATA     newDATA
025100030113     C*
025200030115     c                   clear                   S1Ann
025300940127     C*
025400940127     C                   ADD       1             S1NRR
025500940127     C                   ADD       1             Y
025600940127     C*
025700940607     C                   WRITE     S1
025800940131     C*
025900940316    >C                   EXSR      REDANA
026000940128     C*
0261009401271-   C                   ENDDO
026200940127     C*
026300940223     C                   Z-ADD     S1NRR         WMAX                 30
026400940127     C*
026500940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
026600940127     C*
026700030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
026800940127     C                   MVR                     RESTO             3 0
026900030114     C     PAGINE        MULT      SFLPAG        C1RCD
0270000301141    C     RESTO         IFGT      0
027100030114     C                   ADD       1             C1RCD
0272000301141E   C                   ELSE
027300030114     C                   SUB       SFLPAG        C1RCD
027400030114     C                   ADD       1             C1RCD
0275000301141-   C                   ENDIF
027600940128     C*
027700940127     C                   ENDSR
027800940128     C************************************************************
027900940131     C* LETTURA RCD ARCHIVIO PILOTA
028000940128     C************************************************************
028100940607     C     REDANA        BEGSR
028200940128     C*
028300940131     C                   MOVEL     *OFF          $EFILE
028400940131     C                   MOVEL     *OFF          $RCDOK
028500940131     C*
0286009401311    C     $EFILE        DOUEQ     *ON
028700940131     C     $RCDOK        OREQ      *ON
028800940131     C*
028900110926     c     kTBL          Reade     tntbe01l
029000110926     c                   if        %eof(tntbe01l)
029100030113     C                   MOVEL     *ON           $EFILE
029200030113     C                   MOVE      $EFILE        *IN33
029300030113     c                   else
029400030113     C                   MOVE      *ON           $RCDOK
029500030113     c                   end
029600940131     C*
0297009401311-   C                   ENDDO
029800940131     C*
029900940131     C                   ENDSR
030000940224     C************************************************************
030100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
030200940224     C************************************************************
030300940224     C     CLCPAG        BEGSR
030400940224     C* Input :
030500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
030600940224     C*          cursore
030700940224     C* - SFLPAG = numero rcd per pagina sfl
030800940224     C* Output :
030900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
031000940224     C*
031100120412     c                   if        WSFL >0
031200940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
031300940224     C                   MVR                     RESTO             3 0
031400940224     C     RESTO         IFGT      0
031500940224     C                   ADD       1             PAGINE
031600940224     C                   ENDIF
031700940224     C     PAGINE        MULT      SFLPAG        WPAG
031800120412     c                   else
031900120412     C                   Z-ADD     1             WPAG
032000120412     c                   end
032100940224     C*
032200940224     C                   ENDSR
032300940309     C************************************************************
032400940309     C* GESTIONE F03 VIDEO S1
032500940309     C************************************************************
032600940309     C     F03S1         BEGSR
032700940309     C*
032800940309     C                   MOVE      *ON           $FINE
032900940325     C* fine programma
033000940309     C                   ENDSR
033100940309     C/EJECT
033200940309     C************************************************************
033300940309     C* GESTIONE F10 VIDEO S1
033400940314     c* AGGIUNTA RECORD
033500940309     C************************************************************
033600940309     C     F10S1         BEGSR
033700940309     C*
033800030113     C                   RESET                   tabds
033900030113     C                   MOVEL     '01'          xtaopz
034000030113     C                   MOVE      *ZERO         xtaret
034100030113     C                   MOVE      *ZERO         xtaopr
034200030114     C                   MOVEl     *blank        xtakey
034300130115     C                   MOVEl     *blank        xtakey2
034400030113     C                   MOVE      *BLANKS       KPJBU
034500030113     C                   MOVEL     tabds         KPJBU
034600110927$004 C                   CALL      'TNTB90R2'
034700030113     C                   PARM                    KPJBA
034800030114     C                   MOVEL     KPJBU         tabds
034900030113      *
035000940309     C* ritorno da PGM gestione
035100940309     C                   EXSR      GESRET
035200940309     C     WINZS1        IFEQ      *ON
035300940309     C                   MOVE      *ON           $INZS1
035400940309     C* se esistevano già righe sul sfl
035500940309     C* calcolo pagina a cui deve posizionarsi
035600940309     C     WSFL          IFGT      0
035700940309     C                   EXSR      CLCPAG
035800940309     C* altrimenti carico solo la 1a pagina
035900940309     C                   ELSE
036000940309     C                   Z-ADD     SFLPAG        WPAG
036100940309     C                   ENDIF
036200940309     C                   ENDIF
036300940309     C*
036400940309     C                   ENDSR
036500940128     C************************************************************
036600940131     C* CONTROLLO TESTATA LISTA
036700940128     C************************************************************
036800940131     C     CTRC1         BEGSR
036900940128     C*
037000940201     C                   MOVE      *OFF          *IN99
037100940131     C*
037200940202     C                   ENDSR
037300940131     C************************************************************
037400940131     C* CONTROLLO OPZIONI LISTA
037500940131     C************************************************************
037600940131     C     CTRS1         BEGSR
037700940131     C*
037800940202     C                   MOVEL     *OFF          $ESCI
037900940201     C                   SETOFF                                       99
038000940131     C                   Z-ADD     0             S1OPZ
038100040930     c                   clear                   x1cod
038200130115     c                   clear                   x1cod2
038300940131     C*
038400940127     C* Leggo il sfl solo se ci sono rcd
0385009401311    C     WMAX          IFGT      0
038600940607     C                   READC     S1                                     21
038700940127     C*
038800940131     C* esce se fine sfl o errore che richiede l'uscita
0389009401312    C     *IN21         DOWEQ     *OFF
039000940131     C     $ESCI         ANDEQ     *OFF
039100940201     C                   Z-ADD     S1NRR         C1RCD
039200940131     C* ctrl su riga
039300940131     C                   EXSR      RECS1
039400940131     C* gestione opzioni
0395009401313    C     S1OPZ         IFNE      0
039600940201     C     *IN99         ANDEQ     *OFF
039700940131     C                   EXSR      OPZS1
0398009401273-   C                   ENDIF
039900940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0400009402013    C     *IN99         IFEQ      *ON
040100940201     C     $ESCI         OREQ      *ON
040200940131     C                   MOVE      *ON           *IN32
040300940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
040400940204     C* la ripristinerò a conclusione del ciclo di READC
040500940223     C                   MOVE      *OFF          $INZS1
0406009402233-   C                   ENDIF
040700940223     C*
040800940131     C                   Z-ADD     0             S1OPZ
040900940223     C*
041000940607     C                   UPDATE    S1
041100940223     C*
041200940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0413009401313    C     $ESCI         IFEQ      *OFF
041400940607     C                   READC     S1                                     21
041500940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0416009402014    C     *IN21         IFEQ      *ON
041700940201     C                   MOVE      WINZS1        $INZS1
041800940204     C* calcolo pagina a cui deve posizionarsi
041900940224     C                   EXSR      CLCPAG
0420009402014-   C                   ENDIF
0421009402013-   C                   ENDIF
042200940131     C*
0423009401272-   C                   ENDDO
042400940127     C*
0425009401311-   C                   ENDIF
042600940131     C*
042700940127     C                   ENDSR
042800940127     C/EJECT
042900940127     C************************************************************
043000940131     C* CONTROLLO CAMPI I/O RIGA LISTA
043100940127     C************************************************************
043200940131     C     RECS1         BEGSR
043300940131     C*
043400940201     C* reset indicatori DSPATR
043500940201     C                   MOVE      *ALL'0'       IN4049           10
043600940201     C                   MOVEA     IN4049        *IN(40)
043700940201     C*
043800040930     C*  se richiamato x selezionare un codice 11 = *on
043900040930     C*   non deve utilizzare le opzioni di modifica
044000040930     C*  e viceversa
044100040930     c     *in11         ifeq      *on
0442000409303    C     S1OPZ         IFeq      2
0443000409303    C     S1OPZ         OReq      3
0444000409303    C     S1OPZ         OReq      4
044500040930     c                   seton                                        41  99
044600040930     C                   END
044700130115     C*
044800040930      * selezionato un codice da restituire al chiamante
0449000409303    C     S1OPZ         IFeq      1
045000040930     c                   move      s1cod         x1cod
045100130115     c                   move      s1cod2        x1cod2
045200040930     c                   clear                   kpjbu
045300130115     c                   eval      kpjbu = x1cod + x1cod2
045400040930     C                   MOVEL     *ON           $ESCI
045500040930     C                   MOVEL     *ON           $fine
045600040930     C                   END
045700040930     C                   END
045800040930     C*
045900040930     c     *in11         ifeq      *off
0460000409303    C     S1OPZ         ANDeq     1
046100040930     c                   seton                                        41  99
046200040930     C                   END
046300040930     C*
046400940131     C                   ENDSR
046500940131     C************************************************************
046600940131     C* GESTIONE OPZIONI LISTA
046700940131     C************************************************************
046800940131     C     OPZS1         BEGSR
046900940201     C*
0470000409302    C     *IN11         IFEQ      *Off
0471000409302    C     *IN11         oreq      *On
0472000409302    C     s1opz         andeq     05
047300040930     C*
047400030113     C                   RESET                   tabds
047500110902     C                   MOVE      *all'0'       xtaopz
047600110902     C                   MOVE      S1OPZ         xtaopz
047700030113     C                   MOVE      *ZERO         xtaret
047800030113     C                   MOVE      *ZERO         xtaopr
047900110902     C                   MOVEl     S1COD         xtakey
048000130115     C                   MOVEl     S1COD2        xtakey2
048100940715     C                   MOVE      *BLANKS       KPJBU
048200030113     C                   MOVEL     tabds         KPJBU
048300110927$004 C                   CALL      'TNTB90R2'
048400940607     C                   PARM                    KPJBA
048500030114     C                   MOVEL     KPJBU         tabds
048600940201     C*
048700940223     C* ritorno da PGM gestione
048800940223     C                   EXSR      GESRET
048900040930     C*
049000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0491009402252    C     *IN99         IFEQ      *ON
049200940223     C                   SETON                                        40
0493009402252-   C                   ENDIF
049400940225     C*
0495000409302-   C                   ENDIF
049600040930     C*
049700940131     C                   ENDSR
049800940223     C/EJECT
049900940223     C************************************************************
050000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
050100940223     C************************************************************
050200940223     C     GESRET        BEGSR
050300940223     C*
050400940223     C* modo di ritorno
050500940223     C*
0506009402231    C                   SELECT
050700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
050800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
050900940314    >C*    proposte, normalmente sempre valide, ma modificabili
051000940314    >C*    per situazioni particolari >>
051100940223     C* 1 = F3
051200030113    >C     xtaret        WHENEQ    '1'
051300940224     C                   MOVE      *ON           $ESCI
051400940223     C                   MOVE      *ON           $FINE
051500940223     C* 2 = F12
051600030113    >C     xtaret        WHENEQ    '2'
051700940223     C                   MOVE      *ON           $ESCI
051800940223     C*
0519009402231-   C                   ENDSL
052000940223     C*
052100940223     C* operazione eseguite dal pgm chiamato
052200940223     C*
0523009402231    C                   SELECT
052400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
052500030113    >C     xtaopr        WHENEQ    '1'
052600940223     C                   MOVE      *ON           WINZS1
052700940223     C*
0528009402231-   C                   ENDSL
052900940223     C*
053000940223     C* funzione non eseguibile per errore :
053100940223     C*
0532009402231    C                   SELECT
053300940223     C* 1 = funzione richiamata chiusa in errore
053400940316    >C*  eventualmente gestire altri codici di errore
053500030113    >C     xtaerr        WHENEQ    '1'
053600940223     C                   MOVE      *ON           $ESCI
053700940223     C                   SETON                                        5299
053800940223     C*
0539009402231-   C                   ENDSL
054000940223     C*
054100940223     C                   ENDSR
054200940223     C/EJECT
054300940131     C************************************************************
054400940131     C* OPERAZIONI INIZIALI
054500940131     C************************************************************
054600940131     C     *INZSR        BEGSR
054700030113     C*
054800030113     C* Reperimento parametri
054900030113     C     *ENTRY        PLIST
055000030113     C                   PARM                    KPJBA
055100040930     C                   movel     kpjbu         Selez             1
055200040930     C*
055300040930     C* Se chiamato x selezionare un codice
055400040930     c                   clear                   x1cod
055500130115     c                   clear                   x1cod2
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
057400130115     C                   move      'Z79'         KCOD
057500940127     C*
057600940117     C                   ENDSR
057700030113     C************************************************************
