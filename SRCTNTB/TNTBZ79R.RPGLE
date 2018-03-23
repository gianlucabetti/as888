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
003800130116     D*-------------
003900130116     d dataiso         s               d   datfmt(*iso)
004000130116     d udateiso        s               d   datfmt(*iso)
004100130116     d dataeur         s               d   datfmt(*eur)
004200940211     D*-------------
004300940211     D DSFMT           DS           512
004400940506     D  $TASTO               369    369
004500940211     D  NRG                  370    370
004600940211     D  NCL                  371    371
004700940211     D  SFLNRR               378    379B 0
004800940207     D*-------------
004900940207     D* Nome PGM a video
005000940207     D                 DS
005100940207     D  PROGR                  1     10
005200940207     D  ASTER1                 1      1    INZ('*')
005300940207     D  SIGLA                  2      9
005400940207     D  ASTER2                10     10    INZ('*')
005500940127     D*-------------
005600940127     D* Reperimento nome PGM
005700940127     D STATUS         SDS           333
005800940127     D  DSPGM            *PROC
005900940607     D* descrizione tabella
006000130115$003 D EXTDS         E DS                  EXTNAME(tntbZ79ds)
006100030113     D*-------------
006200030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006300030113$003 D S1NRR           S                   Like(C1rcd)
006400030113$003 D WSfl            S                   Like(C1nrr)
006500030113$003 D Wmax            S                   Like(C1rcd)
006600030113$003 D Wpag            S                   Like(C1rcd)
006700030113$003 D Winzs1          S                   Like($inzs1)
006800040930$003 D x1cod           S                   Like(s1cod)
006900130115$003 D x1cod2          S                   Like(s1cod2)
007000130115     D KCOD            S                   LIKE(TBECOD) inz('Z79')
007100110926     D KKE1            S                   LIKE(TBEKE1)
007200940207     D*-------------
007300940211     D* COSTANTI
007400940211     D*-------------
007500030113     D SFLPAG          C                   CONST(11)
007600940314     D* dimensione della schiera $MS1
007700940506     D*
007800940506     D* Tasti di funzione
007900940506     D F01             C                   CONST(X'31')
008000940506     D F02             C                   CONST(X'32')
008100940506     D F03             C                   CONST(X'33')
008200940506     D F04             C                   CONST(X'34')
008300940506     D F05             C                   CONST(X'35')
008400940506     D F06             C                   CONST(X'36')
008500940506     D F07             C                   CONST(X'37')
008600940506     D F08             C                   CONST(X'38')
008700940506     D F09             C                   CONST(X'39')
008800940506     D F10             C                   CONST(X'3A')
008900940506     D F11             C                   CONST(X'3B')
009000940506     D F12             C                   CONST(X'3C')
009100940506     D F13             C                   CONST(X'B1')
009200940506     D F14             C                   CONST(X'B2')
009300940506     D F15             C                   CONST(X'B3')
009400940506     D F16             C                   CONST(X'B4')
009500940506     D F17             C                   CONST(X'B5')
009600940506     D F18             C                   CONST(X'B6')
009700940506     D F19             C                   CONST(X'B7')
009800940506     D F20             C                   CONST(X'B8')
009900940506     D F21             C                   CONST(X'B9')
010000940506     D F22             C                   CONST(X'BA')
010100940506     D F23             C                   CONST(X'BB')
010200940506     D F24             C                   CONST(X'BC')
010300940506     D ENTER           C                   CONST(X'F1')
010400940506     D ROLDWN          C                   CONST(X'F4')
010500940506     D ROLLUP          C                   CONST(X'F5')
010600940207     I*-------------
010700940607     IS1
010800130115$015 I              S1DES                       NEWDATA
010900940127     C*----------------------------------------------------*
011000940127     C*                MAIN LINE PROGRAM
011100940127     C*----------------------------------------------------*
011200940223     C* inizializzazione variabili
011300940223     C                   EXSR      INZVAR
011400940223     C*
011500940223     C     $FINE         DOWEQ     *OFF
011600940131     C     $GEST         CASEQ     'S1'          GESS1
011700940117     C                   END
011800940117     C                   END
011900940325     C* fine programma
012000940325     C                   SETON                                            LR
012100030113     C************************************************************
012200030113     C* INIZIALIZZAZIONE VARIABILI
012300030113     C************************************************************
012400030113     C     INZVAR        BEGSR
012500030113     C*
012600030113     C* Pulizia campi e indicatori
012700030113     C                   MOVE      *ALL'0'       IN4049           10
012800030113     C                   MOVEA     IN4049        *IN(40)
012900030113     C                   CLEAR                   S1OPZ
013000030113     C* Variabili per gestione videate
013100030113     C*
013200030113     C                   MOVE      *OFF          $FINE
013300030113     C                   MOVE      *OFF          $INZS1
013400030113     C                   MOVE      *OFF          $EFILE
013500030113     C                   MOVE      *OFF          $ESCI
013600030113     C                   MOVE      *OFF          $RCDOK
013700030113     C                   Z-ADD     0             $ULKS1            3 0
013800030113     C*
013900030113     C                   MOVE      *ON           $INZS1
014000030113     C                   MOVE      'S1'          $GEST
014100030113     C*
014200030113     C* Variabili appoggio
014300030114     C                   Z-ADD     1             WPAG
014400030113     c*
014500030113     C                   ENDSR
014600940127     C************************************************************
014700940131     C* GESTIONE LISTA
014800940127     C************************************************************
014900940127     C     GESS1         BEGSR
015000030113     C*
015100940223     C* inizializzazione videata
015200940223     C     $INZS1        IFEQ      *ON
015300940127     C                   EXSR      INZS1
015400940223     C                   MOVE      *OFF          $INZS1
015500940127     C                   ENDIF
015600030113     C*
015700030113     C* emissione piede videata
015800030113     C                   WRITE     Z1
015900030113     C* Non ci sono records
016000940223     C     WMAX          IFEQ      0
016100940607     C                   WRITE     D1
016200030114     C                   Else
016300120412     C     Wsfl          IFgt      Wmax
016400120412     c                   z-add     wmax          Wsfl
016500120412     c                   end
016600030114     C     Wsfl          IFgt      0
016700030114     C                   Z-ADD     wsfl          C1RCD
016800030114     C                   Else
016900030114     C     Wpag          IFgt      0
017000030114     C                   Z-ADD     wpag          C1RCD
017100030114     C                   EndIF
017200030114     C                   EndIF
017300030114     C                   ENDIF
017400940127     C*
017500030113     C*              *------------------*
017600940607     C                   EXFMT     C1
017700030113     C*              *------------------*
017800030113     C*
017900940204     C     C1NRR         IFNE      0
018000940204     C                   Z-ADD     C1NRR         WSFL
018100940204     C                   ENDIF
018200940127     C                   Z-ADD     SFLNRR        C1RCD
018300030113     C* Selezioni
0184009401271    C                   SELECT
018500940127     C* F3=Fine
018600940506     C     $TASTO        WHENEQ    F03
018700940309     C                   EXSR      F03S1
018800940131     C* F10=Immissione
018900940506     C     $TASTO        WHENEQ    F10
019000940309     C                   EXSR      F10S1
0191009401271O   C                   OTHER
019200940127     C* CONTROLLO DATI
019300940131     C                   EXSR      CTRC1
019400940201     C     *IN99         IFEQ      *OFF
019500940131     C                   EXSR      CTRS1
019600940131     C                   END
0197009401271-   C                   ENDSL
019800940127     C*
019900940127     C                   ENDSR
020000940224     C/EJECT
020100940127     C************************************************************
020200940131     C* INIZIALIZZAZIONE LISTA
020300940127     C************************************************************
020400940127     C     INZS1         BEGSR
020500940302     C* pulizia SFL
020600940128     C                   SETOFF                                         3031
020700940607     C                   WRITE     C1
020800940128     C                   SETON                                          31
020900940128     C*
021000030113     C* CARICAMENTO SFL totale
021100940201     C                   Z-ADD     0             S1NRR
021200030113     C                   Z-ADD     1             C1RCD
021300940128     C                   Z-ADD     0             WMAX
021400120412     C                   Z-ADD     1             WPAG
021500940224     C*
021600940224     C* Posizionamento su file pilota
021700110926     c     kTBL          setll     tntbe01l
021800940608    >C                   EXSR      REDANA
021900030113     C* Carico il SFL
022000940127     C                   EXSR      ROLS1
022100030113     C*
022200030114     c                   if        xtaopr <> '1'
022300030114     C                   Z-ADD     1             WPAG
022400030114     c                   end
022500940127     C*
022600940127     C                   ENDSR
022700940127     C************************************************************
022800940131     C* CARICAMENTO PAGINA LISTA
022900940127     C************************************************************
023000940127     C     ROLS1         BEGSR
023100940127     C*
023200940128     C                   SETOFF                                       32
023300940223     C                   Z-ADD     0             Y
023400940127     C                   Z-ADD     WMAX          S1NRR
023500940127     C*
023600940127     C* Leggo dal file anagrafico per caricare la lista
0237009401311    C     $EFILE        DOWEQ     *OFF
023800940127     C*
023900030113     c                   clear                   s1opz
024000130115     c                   clear                   newDATA
024100130115     c                   clear                   DESfil           30
024200110901     c                   move      *all'0'       s1cod
024300110926     c                   movel     tbeke1        s1cod
024400130115     c                   movel     tbeke2        s1cod2
024500110926     c                   move      s1cod         d1fil             3 0
024600110926      **
024700110926     c     d1FIL         chain     azorg01l
024800110926     c                   if        %Found(azorg01l)
024900110926     c                   movel     orgdes        DESfil
025000110926     c                   end
025100130115      **
025200110926     c                   movel     tbeuni        extds
025300130115     c                   movel     Z79NEWSOC     newDATA
025400130116     c                   movel     Z79daDATA     anno_4            4
025500130116     c                   move      Z79daDATA     mmgg_4            4
025600130116     c                   move      anno_4        data_10          10
025700130116     c                   eval      %subst(data_10:1:2) =
025800130116     c                             %subst(mmgg_4:3:2)
025900130116     c                   eval      %subst(data_10:4:2) =
026000130116     c                             %subst(mmgg_4:1:2)
026100130116     c                   eval      %subst(data_10:3:1) = '/'
026200130116     c                   eval      %subst(data_10:6:1) = '/'
026300130116     c                   move      data_10       newDATA
026400030113     C*
026500030115     c                   clear                   S1Ann
026600940127     C*
026700940127     C                   ADD       1             S1NRR
026800940127     C                   ADD       1             Y
026900940127     C*
027000940607     C                   WRITE     S1
027100940131     C*
027200940316    >C                   EXSR      REDANA
027300940128     C*
0274009401271-   C                   ENDDO
027500940127     C*
027600940223     C                   Z-ADD     S1NRR         WMAX                 30
027700940127     C*
027800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
027900940127     C*
028000030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
028100940127     C                   MVR                     RESTO             3 0
028200030114     C     PAGINE        MULT      SFLPAG        C1RCD
0283000301141    C     RESTO         IFGT      0
028400030114     C                   ADD       1             C1RCD
0285000301141E   C                   ELSE
028600030114     C                   SUB       SFLPAG        C1RCD
028700030114     C                   ADD       1             C1RCD
0288000301141-   C                   ENDIF
028900940128     C*
029000940127     C                   ENDSR
029100940128     C************************************************************
029200940131     C* LETTURA RCD ARCHIVIO PILOTA
029300940128     C************************************************************
029400940607     C     REDANA        BEGSR
029500940128     C*
029600940131     C                   MOVEL     *OFF          $EFILE
029700940131     C                   MOVEL     *OFF          $RCDOK
029800940131     C*
0299009401311    C     $EFILE        DOUEQ     *ON
030000940131     C     $RCDOK        OREQ      *ON
030100940131     C*
030200110926     c     kTBL          Reade     tntbe01l
030300110926     c                   if        %eof(tntbe01l)
030400030113     C                   MOVEL     *ON           $EFILE
030500030113     C                   MOVE      $EFILE        *IN33
030600030113     c                   else
030700030113     C                   MOVE      *ON           $RCDOK
030800030113     c                   end
030900940131     C*
0310009401311-   C                   ENDDO
031100940131     C*
031200940131     C                   ENDSR
031300940224     C************************************************************
031400940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
031500940224     C************************************************************
031600940224     C     CLCPAG        BEGSR
031700940224     C* Input :
031800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
031900940224     C*          cursore
032000940224     C* - SFLPAG = numero rcd per pagina sfl
032100940224     C* Output :
032200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
032300940224     C*
032400120412     c                   if        WSFL >0
032500940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
032600940224     C                   MVR                     RESTO             3 0
032700940224     C     RESTO         IFGT      0
032800940224     C                   ADD       1             PAGINE
032900940224     C                   ENDIF
033000940224     C     PAGINE        MULT      SFLPAG        WPAG
033100120412     c                   else
033200120412     C                   Z-ADD     1             WPAG
033300120412     c                   end
033400940224     C*
033500940224     C                   ENDSR
033600940309     C************************************************************
033700940309     C* GESTIONE F03 VIDEO S1
033800940309     C************************************************************
033900940309     C     F03S1         BEGSR
034000940309     C*
034100940309     C                   MOVE      *ON           $FINE
034200940325     C* fine programma
034300940309     C                   ENDSR
034400940309     C/EJECT
034500940309     C************************************************************
034600940309     C* GESTIONE F10 VIDEO S1
034700940314     c* AGGIUNTA RECORD
034800940309     C************************************************************
034900940309     C     F10S1         BEGSR
035000940309     C*
035100030113     C                   RESET                   tabds
035200030113     C                   MOVEL     '01'          xtaopz
035300030113     C                   MOVE      *ZERO         xtaret
035400030113     C                   MOVE      *ZERO         xtaopr
035500030114     C                   MOVEl     *blank        xtakey
035600130115     C                   MOVEl     *blank        xtakey2
035700030113     C                   MOVE      *BLANKS       KPJBU
035800030113     C                   MOVEL     tabds         KPJBU
035900130115$004 C                   CALL      'TNTBZ79R2'
036000030113     C                   PARM                    KPJBA
036100030114     C                   MOVEL     KPJBU         tabds
036200030113      *
036300940309     C* ritorno da PGM gestione
036400940309     C                   EXSR      GESRET
036500940309     C     WINZS1        IFEQ      *ON
036600940309     C                   MOVE      *ON           $INZS1
036700940309     C* se esistevano già righe sul sfl
036800940309     C* calcolo pagina a cui deve posizionarsi
036900940309     C     WSFL          IFGT      0
037000940309     C                   EXSR      CLCPAG
037100940309     C* altrimenti carico solo la 1a pagina
037200940309     C                   ELSE
037300940309     C                   Z-ADD     SFLPAG        WPAG
037400940309     C                   ENDIF
037500940309     C                   ENDIF
037600940309     C*
037700940309     C                   ENDSR
037800940128     C************************************************************
037900940131     C* CONTROLLO TESTATA LISTA
038000940128     C************************************************************
038100940131     C     CTRC1         BEGSR
038200940128     C*
038300940201     C                   MOVE      *OFF          *IN99
038400940131     C*
038500940202     C                   ENDSR
038600940131     C************************************************************
038700940131     C* CONTROLLO OPZIONI LISTA
038800940131     C************************************************************
038900940131     C     CTRS1         BEGSR
039000940131     C*
039100940202     C                   MOVEL     *OFF          $ESCI
039200940201     C                   SETOFF                                       99
039300940131     C                   Z-ADD     0             S1OPZ
039400040930     c                   clear                   x1cod
039500130115     c                   clear                   x1cod2
039600940131     C*
039700940127     C* Leggo il sfl solo se ci sono rcd
0398009401311    C     WMAX          IFGT      0
039900940607     C                   READC     S1                                     21
040000940127     C*
040100940131     C* esce se fine sfl o errore che richiede l'uscita
0402009401312    C     *IN21         DOWEQ     *OFF
040300940131     C     $ESCI         ANDEQ     *OFF
040400940201     C                   Z-ADD     S1NRR         C1RCD
040500940131     C* ctrl su riga
040600940131     C                   EXSR      RECS1
040700940131     C* gestione opzioni
0408009401313    C     S1OPZ         IFNE      0
040900940201     C     *IN99         ANDEQ     *OFF
041000940131     C                   EXSR      OPZS1
0411009401273-   C                   ENDIF
041200940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0413009402013    C     *IN99         IFEQ      *ON
041400940201     C     $ESCI         OREQ      *ON
041500940131     C                   MOVE      *ON           *IN32
041600940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
041700940204     C* la ripristinerò a conclusione del ciclo di READC
041800940223     C                   MOVE      *OFF          $INZS1
0419009402233-   C                   ENDIF
042000940223     C*
042100940131     C                   Z-ADD     0             S1OPZ
042200940223     C*
042300940607     C                   UPDATE    S1
042400940223     C*
042500940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0426009401313    C     $ESCI         IFEQ      *OFF
042700940607     C                   READC     S1                                     21
042800940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0429009402014    C     *IN21         IFEQ      *ON
043000940201     C                   MOVE      WINZS1        $INZS1
043100940204     C* calcolo pagina a cui deve posizionarsi
043200940224     C                   EXSR      CLCPAG
0433009402014-   C                   ENDIF
0434009402013-   C                   ENDIF
043500940131     C*
0436009401272-   C                   ENDDO
043700940127     C*
0438009401311-   C                   ENDIF
043900940131     C*
044000940127     C                   ENDSR
044100940127     C/EJECT
044200940127     C************************************************************
044300940131     C* CONTROLLO CAMPI I/O RIGA LISTA
044400940127     C************************************************************
044500940131     C     RECS1         BEGSR
044600940131     C*
044700940201     C* reset indicatori DSPATR
044800940201     C                   MOVE      *ALL'0'       IN4049           10
044900940201     C                   MOVEA     IN4049        *IN(40)
045000940201     C*
045100040930     C*  se richiamato x selezionare un codice 11 = *on
045200040930     C*   non deve utilizzare le opzioni di modifica
045300040930     C*  e viceversa
045400040930     c     *in11         ifeq      *on
0455000409303    C     S1OPZ         IFeq      2
0456000409303    C     S1OPZ         OReq      3
0457000409303    C     S1OPZ         OReq      4
045800040930     c                   seton                                        41  99
045900040930     C                   END
046000130115     C*
046100040930      * selezionato un codice da restituire al chiamante
0462000409303    C     S1OPZ         IFeq      1
046300040930     c                   move      s1cod         x1cod
046400130115     c                   move      s1cod2        x1cod2
046500040930     c                   clear                   kpjbu
046600130115     c                   eval      kpjbu = x1cod + x1cod2
046700040930     C                   MOVEL     *ON           $ESCI
046800040930     C                   MOVEL     *ON           $fine
046900040930     C                   END
047000040930     C                   END
047100040930     C*
047200040930     c     *in11         ifeq      *off
0473000409303    C     S1OPZ         ANDeq     1
047400040930     c                   seton                                        41  99
047500040930     C                   END
047600040930     C*
047700940131     C                   ENDSR
047800940131     C************************************************************
047900940131     C* GESTIONE OPZIONI LISTA
048000940131     C************************************************************
048100940131     C     OPZS1         BEGSR
048200940201     C*
0483000409302    C     *IN11         IFEQ      *Off
0484000409302    C     *IN11         oreq      *On
0485000409302    C     s1opz         andeq     05
048600040930     C*
048700030113     C                   RESET                   tabds
048800110902     C                   MOVE      *all'0'       xtaopz
048900110902     C                   MOVE      S1OPZ         xtaopz
049000030113     C                   MOVE      *ZERO         xtaret
049100030113     C                   MOVE      *ZERO         xtaopr
049200110902     C                   MOVEl     S1COD         xtakey
049300130115     C                   MOVEl     S1COD2        xtakey2
049400940715     C                   MOVE      *BLANKS       KPJBU
049500030113     C                   MOVEL     tabds         KPJBU
049600130115$004 C                   CALL      'TNTBZ79R2'
049700940607     C                   PARM                    KPJBA
049800030114     C                   MOVEL     KPJBU         tabds
049900940201     C*
050000940223     C* ritorno da PGM gestione
050100940223     C                   EXSR      GESRET
050200040930     C*
050300940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0504009402252    C     *IN99         IFEQ      *ON
050500940223     C                   SETON                                        40
0506009402252-   C                   ENDIF
050700940225     C*
0508000409302-   C                   ENDIF
050900040930     C*
051000940131     C                   ENDSR
051100940223     C/EJECT
051200940223     C************************************************************
051300940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
051400940223     C************************************************************
051500940223     C     GESRET        BEGSR
051600940223     C*
051700940223     C* modo di ritorno
051800940223     C*
0519009402231    C                   SELECT
052000940314    >C* << questi modi di utilizzo dei valori di ritorno dal
052100940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
052200940314    >C*    proposte, normalmente sempre valide, ma modificabili
052300940314    >C*    per situazioni particolari >>
052400940223     C* 1 = F3
052500030113    >C     xtaret        WHENEQ    '1'
052600940224     C                   MOVE      *ON           $ESCI
052700940223     C                   MOVE      *ON           $FINE
052800940223     C* 2 = F12
052900030113    >C     xtaret        WHENEQ    '2'
053000940223     C                   MOVE      *ON           $ESCI
053100940223     C*
0532009402231-   C                   ENDSL
053300940223     C*
053400940223     C* operazione eseguite dal pgm chiamato
053500940223     C*
0536009402231    C                   SELECT
053700940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
053800030113    >C     xtaopr        WHENEQ    '1'
053900940223     C                   MOVE      *ON           WINZS1
054000940223     C*
0541009402231-   C                   ENDSL
054200940223     C*
054300940223     C* funzione non eseguibile per errore :
054400940223     C*
0545009402231    C                   SELECT
054600940223     C* 1 = funzione richiamata chiusa in errore
054700940316    >C*  eventualmente gestire altri codici di errore
054800030113    >C     xtaerr        WHENEQ    '1'
054900940223     C                   MOVE      *ON           $ESCI
055000940223     C                   SETON                                        5299
055100940223     C*
0552009402231-   C                   ENDSL
055300940223     C*
055400940223     C                   ENDSR
055500940223     C/EJECT
055600940131     C************************************************************
055700940131     C* OPERAZIONI INIZIALI
055800940131     C************************************************************
055900940131     C     *INZSR        BEGSR
056000030113     C*
056100030113     C* Reperimento parametri
056200030113     C     *ENTRY        PLIST
056300030113     C                   PARM                    KPJBA
056400040930     C                   movel     kpjbu         Selez             1
056500040930     C*
056600040930     C* Se chiamato x selezionare un codice
056700040930     c                   clear                   x1cod
056800130115     c                   clear                   x1cod2
056900040930     c                   if        selez = '?'
057000040930     c                   seton                                        11
057100040930     c                   end
057200030113     C*
057300030113     C* Variabili per gestione videate
057400030113     C                   MOVE      *BLANK        $GEST             2
057500030113     C                   MOVE      *BLANK        $FINE             1
057600030113     C                   MOVE      *BLANK        $INZS1            1
057700030113     C                   MOVE      *BLANK        $EFILE            1
057800030113     C                   MOVE      *BLANK        $ESCI             1
057900030113     C                   MOVE      *BLANK        $RCDOK            1
058000030113     C*
058100030113     C     KTBL          KLIST
058200110926     C                   KFLD                    KCOD
058300030113     C* Indici
058400030113     C                   Z-ADD     0             X                 3 0
058500030113     C                   Z-ADD     0             Y                 3 0
058600940506     C*
058700130115     C                   move      'Z79'         KCOD
058800940127     C*
058900940117     C                   ENDSR
059000030113     C************************************************************
