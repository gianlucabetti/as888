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
002200110906     FTMORP03L  IF   E           K DISK
002300110902     FTII502D   CF   E             WORKSTN
002400940607     F                                     SFILE(S1:S1NRR)
002500940201     F                                     INFDS(DSFMT)
002600940128     D*----------------------------------------------------*
002700940211     D* Passaggio Parametri
002800940211     D KPJBA         E DS
002900110909     D tii502ds      E DS
003000110909     D*----------------------------------------------------*
003100110902      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
003200110902     D WLBDAT          DS                  INZ
003300110902     D  G02DAT                 1      8  0
003400110902     D  G02INV                 9     16  0
003500110902     D  G02ERR                17     17
003600110902     D  G02TGI                18     22  0
003700110902      *
003800110902     D DATPAR          DS
003900110902     D  GIODAT                 1      8  0
004000110902     D  GIOINV                 9     16  0
004100110902     D  GIOTGI                17     21  0
004200110902      *
004300030113      *-------------
004400940325     D* Parametri in ricezione
004500030113     D  TABDS          DS
004600030113     D  XTAOPZ                 1      2
004700030113     D  XTARET                 3      3
004800030113     D  XTAOPR                 4      4
004900030113     D  XTAERR                 5      5
005000110902     D  XTAKEY                 6     10
005100940211     D*-------------
005200940211     D DSFMT           DS           512
005300940506     D  $TASTO               369    369
005400940211     D  NRG                  370    370
005500940211     D  NCL                  371    371
005600940211     D  SFLNRR               378    379B 0
005700940207     D*-------------
005800940207     D* Nome PGM a video
005900940207     D                 DS
006000940207     D  PROGR                  1     10
006100940207     D  ASTER1                 1      1    INZ('*')
006200940207     D  SIGLA                  2      9
006300940207     D  ASTER2                10     10    INZ('*')
006400940127     D*-------------
006500940127     D* Reperimento nome PGM
006600940127     D STATUS         SDS           333
006700940127     D  DSPGM            *PROC
006800940607     D* descrizione tabella
006900110906$003 D EXTDS         E DS                  EXTNAME(TMORP00F) prefix(DS)
007000030113     D*-------------
007100030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
007200030113$003 D S1NRR           S                   Like(C1rcd)
007300030113$003 D WSfl            S                   Like(C1nrr)
007400030113$003 D Wmax            S                   Like(C1rcd)
007500030113$003 D Wpag            S                   Like(C1rcd)
007600030113$003 D Winzs1          S                   Like($inzs1)
007700040930$003 D x1cod           S                   Like(s1cod)
007800940207     D*-------------
007900940211     D* COSTANTI
008000940211     D*-------------
008100030113     D SFLPAG          C                   CONST(11)
008200940314     D* dimensione della schiera $MS1
008300940506     D*
008400940506     D* Tasti di funzione
008500940506     D F01             C                   CONST(X'31')
008600940506     D F02             C                   CONST(X'32')
008700940506     D F03             C                   CONST(X'33')
008800940506     D F04             C                   CONST(X'34')
008900940506     D F05             C                   CONST(X'35')
009000940506     D F06             C                   CONST(X'36')
009100940506     D F07             C                   CONST(X'37')
009200940506     D F08             C                   CONST(X'38')
009300940506     D F09             C                   CONST(X'39')
009400940506     D F10             C                   CONST(X'3A')
009500940506     D F11             C                   CONST(X'3B')
009600940506     D F12             C                   CONST(X'3C')
009700940506     D F13             C                   CONST(X'B1')
009800940506     D F14             C                   CONST(X'B2')
009900940506     D F15             C                   CONST(X'B3')
010000940506     D F16             C                   CONST(X'B4')
010100940506     D F17             C                   CONST(X'B5')
010200940506     D F18             C                   CONST(X'B6')
010300940506     D F19             C                   CONST(X'B7')
010400940506     D F20             C                   CONST(X'B8')
010500940506     D F21             C                   CONST(X'B9')
010600940506     D F22             C                   CONST(X'BA')
010700940506     D F23             C                   CONST(X'BB')
010800940506     D F24             C                   CONST(X'BC')
010900940506     D ENTER           C                   CONST(X'F1')
011000940506     D ROLDWN          C                   CONST(X'F4')
011100940506     D ROLLUP          C                   CONST(X'F5')
011200940207     I*-------------
011300110907     D Pianificato     C                   CONST('P')
011400110907     D aConsuntivo     C                   CONST('C')
011500110916     D Tutto           C                   CONST('T')
011600940127     C*----------------------------------------------------*
011700940127     C*                MAIN LINE PROGRAM
011800940127     C*----------------------------------------------------*
011900940223     C* inizializzazione variabili
012000940223     C                   EXSR      INZVAR
012100940223     C*
012200940223     C     $FINE         DOWEQ     *OFF
012300940131     C     $GEST         CASEQ     'S1'          GESS1
012400940117     C                   END
012500940117     C                   END
012600940325     C* fine programma
012700940325     C                   SETON                                            LR
012800030113     C************************************************************
012900030113     C* INIZIALIZZAZIONE VARIABILI
013000030113     C************************************************************
013100030113     C     INZVAR        BEGSR
013200030113     C*
013300030113     C* Pulizia campi e indicatori
013400030113     C                   MOVE      *ALL'0'       IN4049           10
013500030113     C                   MOVEA     IN4049        *IN(40)
013600030113     C                   CLEAR                   S1OPZ
013700030113     C* Variabili per gestione videate
013800030113     C*
013900030113     C                   MOVE      *OFF          $FINE
014000030113     C                   MOVE      *OFF          $INZS1
014100030113     C                   MOVE      *OFF          $EFILE
014200030113     C                   MOVE      *OFF          $ESCI
014300030113     C                   MOVE      *OFF          $RCDOK
014400030113     C                   Z-ADD     0             $ULKS1            3 0
014500030113     C*
014600030113     C                   MOVE      *ON           $INZS1
014700030113     C                   MOVE      'S1'          $GEST
014800030113     C*
014900030113     C* Variabili appoggio
015000030114     C                   Z-ADD     1             WPAG
015100030113     c*
015200030113     C                   ENDSR
015300940127     C************************************************************
015400940131     C* GESTIONE LISTA
015500940127     C************************************************************
015600940127     C     GESS1         BEGSR
015700030113     C*
015800940223     C* inizializzazione videata
015900940223     C     $INZS1        IFEQ      *ON
016000940127     C                   EXSR      INZS1
016100940223     C                   MOVE      *OFF          $INZS1
016200940127     C                   ENDIF
016300030113     C*
016400030113     C* emissione piede videata
016500030113     C                   WRITE     Z1
016600030113     C* Non ci sono records
016700940223     C     WMAX          IFEQ      0
016800940607     C                   WRITE     D1
016900030114     C                   Else
017000030114     C     Wsfl          IFgt      0
017100030114     C                   Z-ADD     wsfl          C1RCD
017200030114     C                   Else
017300030114     C     Wpag          IFgt      0
017400030114     C                   Z-ADD     wpag          C1RCD
017500030114     C                   EndIF
017600030114     C                   EndIF
017700030114     C                   ENDIF
017800940127     C*
017900030113     C*              *------------------*
018000940607     C                   EXFMT     C1
018100030113     C*              *------------------*
018200030113     C*
018300940204     C     C1NRR         IFNE      0
018400940204     C                   Z-ADD     C1NRR         WSFL
018500940204     C                   ENDIF
018600940127     C                   Z-ADD     SFLNRR        C1RCD
018700030113     C* Selezioni
0188009401271    C                   SELECT
018900940127     C* F3=Fine
019000940506     C     $TASTO        WHENEQ    F03
019100940309     C                   EXSR      F03S1
0192009401271O   C                   OTHER
019300940127     C* CONTROLLO DATI
019400940131     C                   EXSR      CTRC1
019500940201     C     *IN99         IFEQ      *OFF
019600940131     C                   EXSR      CTRS1
019700940131     C                   END
0198009401271-   C                   ENDSL
019900940127     C*
020000940127     C                   ENDSR
020100940224     C/EJECT
020200940127     C************************************************************
020300940131     C* INIZIALIZZAZIONE LISTA
020400940127     C************************************************************
020500940127     C     INZS1         BEGSR
020600940302     C* pulizia SFL
020700940128     C                   SETOFF                                         3031
020800940607     C                   WRITE     C1
020900940128     C                   SETON                                          31
021000940128     C*
021100030113     C* CARICAMENTO SFL totale
021200940201     C                   Z-ADD     0             S1NRR
021300030113     C                   Z-ADD     1             C1RCD
021400940128     C                   Z-ADD     0             WMAX
021500940224     C*
021600940224     C* Posizionamento su file pilota
021700110916     C                   eval        ORPfil  = I502FILIAL
021800110916     C                   eval        ORPCONF = I502CONFER
021900110916     c     KORP3_1       setll     TMORP03l
022000940608    >C                   EXSR      REDANA
022100030113     C* Carico il SFL
022200940127     C                   EXSR      ROLS1
022300030113     C*
022400030114     c                   if        xtaopr <> '1'
022500030114     C                   Z-ADD     1             WPAG
022600030114     c                   end
022700940127     C*
022800940127     C                   ENDSR
022900940127     C************************************************************
023000940131     C* CARICAMENTO PAGINA LISTA
023100940127     C************************************************************
023200940127     C     ROLS1         BEGSR
023300940127     C*
023400940128     C                   SETOFF                                       32
023500940223     C                   Z-ADD     0             Y
023600940127     C                   Z-ADD     WMAX          S1NRR
023700940127     C*
023800940127     C* Leggo dal file anagrafico per caricare la lista
0239009401311    C     $EFILE        DOWEQ     *OFF
024000940127     C*
024100030113     c                   clear                   s1opz
024200110901     c                   move      *all'0'       s1cod
024300110902     c                   move      orpNOR        s1cod
024400111019     c                   move      orpFIL        s1fil
024500111020     C*
024600111020     c                   clear                   s1tord
024700111020     c                   if        orpPIAN = 'C'
024800111020     c                   movel     'aCons'       s1tord
024900111020     c                   elseif    orpPIAN = 'P'
025000111020     c                   move      'Pian'        s1tord
025100111020     c                   end
025200111020     C*
025300111020     c                   move(p)   'no'          s1conf
025400111020     c                   if        orpCONF = '1'
025500111020     c                   movel(p)  'Fil'         s1conf
025600111020     c                   elseIf    orpCONF = 'C'
025700111020     c                   movel(p)  'Sed'         s1conf
025800111020     c                   end
025900111020     C*
026000110907     c                   setoff                                       54
026100110907     c                   if        s1tord = Pianificato
026200110907     c                   setoN                                        54
026300110907     c                   end
026400110902     C*
026500110902     C                   Z-ADD     orpDOR        G02INV                         *AAAA/MM/GG
026600110902     C                   MOVEL     '3'           G02ERR
026700110902     C                   CALL      'XSRDA8'
026800110902     C                   PARM                    WLBDAT
026900110902     c                   Z-ADD     G02DAT        s1data
027000030113     C*
027100940127     C*
027200940127     C                   ADD       1             S1NRR
027300940127     C                   ADD       1             Y
027400940127     C*
027500940607     C                   WRITE     S1
027600940131     C*
027700940316    >C                   EXSR      REDANA
027800940128     C*
0279009401271-   C                   ENDDO
028000940127     C*
028100940223     C                   Z-ADD     S1NRR         WMAX                 30
028200940127     C*
028300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
028400940127     C*
028500030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
028600940127     C                   MVR                     RESTO             3 0
028700030114     C     PAGINE        MULT      SFLPAG        C1RCD
0288000301141    C     RESTO         IFGT      0
028900030114     C                   ADD       1             C1RCD
0290000301141E   C                   ELSE
029100030114     C                   SUB       SFLPAG        C1RCD
029200030114     C                   ADD       1             C1RCD
0293000301141-   C                   ENDIF
029400940128     C*
029500940127     C                   ENDSR
029600940128     C************************************************************
029700940131     C* LETTURA RCD ARCHIVIO PILOTA
029800940128     C************************************************************
029900940607     C     REDANA        BEGSR
030000940128     C*
030100111019     c     rifai         tag
030200111019     C*
030300940131     C                   MOVEL     *OFF          $EFILE
030400940131     C                   MOVEL     *OFF          $RCDOK
030500940131     C*
0306009401311    C     $EFILE        DOUEQ     *ON
030700940131     C     $RCDOK        OREQ      *ON
030800940131     C*
030900111020     c                   if        i502filial > 000
031000110916     c     KORP3_1       Reade     TMORP03l
031100111020     c                   else
031200111020     c     orpconf       Reade     TMORP03l
031300111020     c                   end
031400111020     C*
031500110906     c                   if        %eof(TMORP03l)
031600030113     C                   MOVEL     *ON           $EFILE
031700030113     C                   MOVE      $EFILE        *IN33
031800030113     c                   else
031900030113     C                   MOVE      *ON           $RCDOK
032000110909     c**
032100110916     c                   if        I502TIPORD <> orpPIAN and
032200110916     c                             I502TIPORD <> Tutto
032300110916     c                               or
032400110916     c                             I502UFFCOM <> orpUFF and
032500110916     c                             I502UFFCOM <> Tutto
032600111019     c                               or
032700111019     c                             I502adata  < orpDOR  or
032800111019     c                             I502dadata > orpDOR
032900110909     C                   MOVE      *Off          $RCDOK
033000110909     c                   end
033100110909      *
033200110906     C     KORP3         SETGT     TMORP03L
033300030113     c                   end
033400940131     C*
0335009401311-   C                   ENDDO
033600940131     C*
033700111019     C* se non richiesti quelli confermati da sede
033800111019     C* dopo il primo giro con il flag *blank deve trovare quelli
033900111019     C* con il flag a '1' ossia confermati in filiale
034000111020     C                   if         i502confer   = ' ' and
034100111020     c                                   $EFILE  = *on
034200111020      *
034300111019     c                   eval         i502confer = '1'
034400111019     C                   eval         ORPfil  = I502FILIAL
034500111019     C                   eval         ORPCONF = I502CONFER
034600111019     c     KORP3_1       setll     TMORP03l
034700111019     c                   goto      rifai
034800111020      *
034900111019     c                   end
035000111019     C*
035100940131     C                   ENDSR
035200940224     C************************************************************
035300940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
035400940224     C************************************************************
035500940224     C     CLCPAG        BEGSR
035600940224     C* Input :
035700940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
035800940224     C*          cursore
035900940224     C* - SFLPAG = numero rcd per pagina sfl
036000940224     C* Output :
036100940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
036200940224     C*
036300940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
036400940224     C                   MVR                     RESTO             3 0
036500940224     C     RESTO         IFGT      0
036600940224     C                   ADD       1             PAGINE
036700940224     C                   ENDIF
036800940224     C     PAGINE        MULT      SFLPAG        WPAG
036900940224     C*
037000940224     C                   ENDSR
037100940309     C************************************************************
037200940309     C* GESTIONE F03 VIDEO S1
037300940309     C************************************************************
037400940309     C     F03S1         BEGSR
037500940309     C*
037600940309     C                   MOVE      *ON           $FINE
037700940325     C* fine programma
037800940309     C                   ENDSR
037900940309     C/EJECT
038000940128     C************************************************************
038100940131     C* CONTROLLO TESTATA LISTA
038200940128     C************************************************************
038300940131     C     CTRC1         BEGSR
038400940128     C*
038500940201     C                   MOVE      *OFF          *IN99
038600940131     C*
038700940202     C                   ENDSR
038800940131     C************************************************************
038900940131     C* CONTROLLO OPZIONI LISTA
039000940131     C************************************************************
039100940131     C     CTRS1         BEGSR
039200940131     C*
039300940202     C                   MOVEL     *OFF          $ESCI
039400940201     C                   SETOFF                                       99
039500940131     C                   Z-ADD     0             S1OPZ
039600040930     c                   clear                   x1cod
039700940131     C*
039800940127     C* Leggo il sfl solo se ci sono rcd
0399009401311    C     WMAX          IFGT      0
040000940607     C                   READC     S1                                     21
040100940127     C*
040200940131     C* esce se fine sfl o errore che richiede l'uscita
0403009401312    C     *IN21         DOWEQ     *OFF
040400940131     C     $ESCI         ANDEQ     *OFF
040500940201     C                   Z-ADD     S1NRR         C1RCD
040600940131     C* ctrl su riga
040700940131     C                   EXSR      RECS1
040800940131     C* gestione opzioni
0409009401313    C     S1OPZ         IFNE      0
041000940201     C     *IN99         ANDEQ     *OFF
041100940131     C                   EXSR      OPZS1
0412009401273-   C                   ENDIF
041300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0414009402013    C     *IN99         IFEQ      *ON
041500940201     C     $ESCI         OREQ      *ON
041600940131     C                   MOVE      *ON           *IN32
041700940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
041800940204     C* la ripristinerò a conclusione del ciclo di READC
041900940223     C                   MOVE      *OFF          $INZS1
0420009402233-   C                   ENDIF
042100940223     C*
042200940131     C                   Z-ADD     0             S1OPZ
042300940223     C*
042400940607     C                   UPDATE    S1
042500940223     C*
042600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0427009401313    C     $ESCI         IFEQ      *OFF
042800940607     C                   READC     S1                                     21
042900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0430009402014    C     *IN21         IFEQ      *ON
043100940201     C                   MOVE      WINZS1        $INZS1
043200940204     C* calcolo pagina a cui deve posizionarsi
043300940224     C                   EXSR      CLCPAG
0434009402014-   C                   ENDIF
0435009402013-   C                   ENDIF
043600940131     C*
0437009401272-   C                   ENDDO
043800940127     C*
0439009401311-   C                   ENDIF
044000940131     C*
044100940127     C                   ENDSR
044200940127     C/EJECT
044300940127     C************************************************************
044400940131     C* CONTROLLO CAMPI I/O RIGA LISTA
044500940127     C************************************************************
044600940131     C     RECS1         BEGSR
044700940131     C*
044800940201     C* reset indicatori DSPATR
044900940201     C                   MOVE      *ALL'0'       IN4049           10
045000940201     C                   MOVEA     IN4049        *IN(40)
045100940201     C*
045200040930     C*  se richiamato x selezionare un codice 11 = *on
045300040930     C*   non deve utilizzare le opzioni di modifica
045400040930     C*  e viceversa
045500040930     c     *in11         ifeq      *on
0456000409303    C     S1OPZ         IFeq      2
0457000409303    C     S1OPZ         OReq      3
0458000409303    C     S1OPZ         OReq      4
045900040930     c                   seton                                        41  99
046000040930     C                   END
046100040930      * selezionato un codice da restituire al chiamante
0462000409303    C     S1OPZ         IFeq      1
046300040930     c                   move      s1cod         x1cod
046400110909      *
046500110909     c                   move      s1cod         O502NUMORD
046600110909     c                   eval       kpjbu = tii502ds
046700110909      *
046800040930     C                   MOVEL     *ON           $ESCI
046900040930     C                   MOVEL     *ON           $fine
047000040930     C                   END
047100040930     C                   END
047200040930     C*
047300040930     c     *in11         ifeq      *off
0474000409303    C     S1OPZ         ANDeq     1
047500040930     c                   seton                                        41  99
047600040930     C                   END
047700110907     C*  Rev.Image
047800110907     c                   setoff                                       54
047900110907     c                   if        s1tord = Pianificato
048000110907     c                   setoN                                        54
048100110907     c                   end
048200110907     C*
048300940131     C                   ENDSR
048400940131     C************************************************************
048500940131     C* GESTIONE OPZIONI LISTA
048600940131     C************************************************************
048700940131     C     OPZS1         BEGSR
048800940201     C*
048900940201     C*
049000940223     C* ritorno da PGM gestione
049100940223     C                   EXSR      GESRET
049200040930     C*
049300940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0494009402252    C     *IN99         IFEQ      *ON
049500940223     C                   SETON                                        40
0496009402252-   C                   ENDIF
049700940225     C*
049800040930     C*
049900940131     C                   ENDSR
050000940223     C/EJECT
050100940223     C************************************************************
050200940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
050300940223     C************************************************************
050400940223     C     GESRET        BEGSR
050500940223     C*
050600940223     C* modo di ritorno
050700940223     C*
0508009402231    C                   SELECT
050900940314    >C* << questi modi di utilizzo dei valori di ritorno dal
051000940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
051100940314    >C*    proposte, normalmente sempre valide, ma modificabili
051200940314    >C*    per situazioni particolari >>
051300940223     C* 1 = F3
051400030113    >C     xtaret        WHENEQ    '1'
051500940224     C                   MOVE      *ON           $ESCI
051600940223     C                   MOVE      *ON           $FINE
051700940223     C* 2 = F12
051800030113    >C     xtaret        WHENEQ    '2'
051900940223     C                   MOVE      *ON           $ESCI
052000940223     C*
0521009402231-   C                   ENDSL
052200940223     C*
052300940223     C* operazione eseguite dal pgm chiamato
052400940223     C*
0525009402231    C                   SELECT
052600940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
052700030113    >C     xtaopr        WHENEQ    '1'
052800940223     C                   MOVE      *ON           WINZS1
052900940223     C*
0530009402231-   C                   ENDSL
053100940223     C*
053200940223     C* funzione non eseguibile per errore :
053300940223     C*
0534009402231    C                   SELECT
053500940223     C* 1 = funzione richiamata chiusa in errore
053600940316    >C*  eventualmente gestire altri codici di errore
053700030113    >C     xtaerr        WHENEQ    '1'
053800940223     C                   MOVE      *ON           $ESCI
053900940223     C                   SETON                                        5299
054000940223     C*
0541009402231-   C                   ENDSL
054200940223     C*
054300940223     C                   ENDSR
054400940223     C/EJECT
054500940131     C************************************************************
054600940131     C* OPERAZIONI INIZIALI
054700940131     C************************************************************
054800940131     C     *INZSR        BEGSR
054900030113     C*
055000030113     C* Reperimento parametri
055100030113     C     *ENTRY        PLIST
055200030113     C                   PARM                    KPJBA
055300110909     C                   movel     kpjbu         tii502DS
055400110916     C*
055500110916     C* se deve ricercare ordini confermati oppure ancora da confermare
055600111019     C                   if        i502confer = 'C'
055700110916     C                   move      i502confer    ORPCONF
055800111019     C                   move      *on           *in06
055900111019     c                   end
056000040930     C*
056100040930     C* Se chiamato x selezionare un codice
056200040930     c                   clear                   x1cod
056300111020     c****               if        I502FILIAL > 0
056400040930     c                   seton                                        11
056500111020     c****               end
056600030113     C*
056700030113     C* Variabili per gestione videate
056800030113     C                   MOVE      *BLANK        $GEST             2
056900030113     C                   MOVE      *BLANK        $FINE             1
057000030113     C                   MOVE      *BLANK        $INZS1            1
057100030113     C                   MOVE      *BLANK        $EFILE            1
057200030113     C                   MOVE      *BLANK        $ESCI             1
057300030113     C                   MOVE      *BLANK        $RCDOK            1
057400030113     C* Indici
057500030113     C                   Z-ADD     0             X                 3 0
057600030113     C                   Z-ADD     0             Y                 3 0
057700940506     C*
057800110902     C     KORP3         KLIST
057900110916     C                   KFLD                    ORPCONF
058000110916     C                   KFLD                    ORPFIL
058100110902     C                   KFLD                    ORPNOR
058200110916     C*
058300110916     C     KORP3_1       KLIST
058400110916     C                   KFLD                    ORPCONF
058500110916     C                   KFLD                    ORPFIL
058600940127     C*
058700940117     C                   ENDSR
058800030113     C************************************************************
