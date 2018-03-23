000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
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
002100080915     FFifgtL1L  IF   E           K DISK
002200080915     FTnvr8abD  CF   E             WORKSTN
002300940607     F                                     SFILE(S1:S1NRR)
002400940201     F                                     INFDS(DSFMT)
002500940128     D*----------------------------------------------------*
002600940211     D* Passaggio Parametri
002700940211     D KPJBA         E DS
002800080915     D Tibs36ds      E DS
002900080915     D CNCR80        E DS
003000080915     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003100940211     D*-------------
003200940211     D DSFMT           DS           512
003300940506     D  $TASTO               369    369
003400940211     D  NRG                  370    370
003500940211     D  NCL                  371    371
003600940211     D  SFLNRR               378    379B 0
003700940207     D*-------------
003800940207     D* Nome PGM a video
003900940207     D                 DS
004000940207     D  PROGR                  1     10
004100940207     D  ASTER1                 1      1    INZ('*')
004200940207     D  SIGLA                  2      9
004300940207     D  ASTER2                10     10    INZ('*')
004400940127     D*-------------
004500940127     D* Reperimento nome PGM
004600940127     D STATUS         SDS           333
004700940127     D  DSPGM            *PROC
004800030113     D*-------------
004900030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005000030113$003 D S1NRR           S                   Like(C1rcd)
005100030113$003 D WSfl            S                   Like(C1nrr)
005200030113$003 D Wmax            S                   Like(C1rcd)
005300030113$003 D Wpag            S                   Like(C1rcd)
005400030113$003 D Winzs1          S                   Like($inzs1)
005500940207     D*-------------
005600940211     D* COSTANTI
005700940211     D*-------------
005800030113     D SFLPAG          C                   CONST(11)
005900940314     D* dimensione della schiera $MS1
006000940506     D*
006100940506     D* Tasti di funzione
006200940506     D F01             C                   CONST(X'31')
006300940506     D F02             C                   CONST(X'32')
006400940506     D F03             C                   CONST(X'33')
006500940506     D F04             C                   CONST(X'34')
006600940506     D F05             C                   CONST(X'35')
006700940506     D F06             C                   CONST(X'36')
006800940506     D F07             C                   CONST(X'37')
006900940506     D F08             C                   CONST(X'38')
007000940506     D F09             C                   CONST(X'39')
007100940506     D F10             C                   CONST(X'3A')
007200940506     D F11             C                   CONST(X'3B')
007300940506     D F12             C                   CONST(X'3C')
007400940506     D F13             C                   CONST(X'B1')
007500940506     D F14             C                   CONST(X'B2')
007600940506     D F15             C                   CONST(X'B3')
007700940506     D F16             C                   CONST(X'B4')
007800940506     D F17             C                   CONST(X'B5')
007900940506     D F18             C                   CONST(X'B6')
008000940506     D F19             C                   CONST(X'B7')
008100940506     D F20             C                   CONST(X'B8')
008200940506     D F21             C                   CONST(X'B9')
008300940506     D F22             C                   CONST(X'BA')
008400940506     D F23             C                   CONST(X'BB')
008500940506     D F24             C                   CONST(X'BC')
008600940506     D ENTER           C                   CONST(X'F1')
008700940506     D ROLDWN          C                   CONST(X'F4')
008800940506     D ROLLUP          C                   CONST(X'F5')
008900940127     C*----------------------------------------------------*
009000940127     C*                MAIN LINE PROGRAM
009100940127     C*----------------------------------------------------*
009200940223     C* inizializzazione variabili
009300940223     C                   EXSR      INZVAR
009400940223     C*
009500940223     C     $FINE         DOWEQ     *OFF
009600080915     C     $GEST         CASEQ     'D0'          GESD0
009700940131     C     $GEST         CASEQ     'S1'          GESS1
009800940117     C                   END
009900940117     C                   END
010000940325     C* fine programma
010100940325     C                   SETON                                            LR
010200030113     C************************************************************
010300030113     C* INIZIALIZZAZIONE VARIABILI
010400030113     C************************************************************
010500030113     C     INZVAR        BEGSR
010600030113     C*
010700030113     C* Pulizia campi e indicatori
010800030113     C                   MOVE      *ALL'0'       IN4049           10
010900030113     C                   MOVEA     IN4049        *IN(40)
011000030113     C                   CLEAR                   S1OPZ
011100030113     C* Variabili per gestione videate
011200030113     C                   MOVE      *OFF          $FINE
011300030113     C                   MOVE      *OFF          $INZS1
011400030113     C                   MOVE      *OFF          $EFILE
011500030113     C                   MOVE      *OFF          $ESCI
011600030113     C                   MOVE      *OFF          $RCDOK
011700030113     C*
011800080915     C                   MOVE      *ON           $INZD0
011900080915     C                   MOVE      'D0'          $GEST
012000030113     C*
012100030113     C* Variabili appoggio
012200030114     C                   Z-ADD     1             WPAG
012300030113     c*
012400030113     C                   ENDSR
012500940127     C************************************************************
012600080915     C* Primo Video Introduttivo con Filtro iniziale sulla Filiale
012700940127     C************************************************************
012800080915     C     GESD0         BEGSR
012900030113     C*
013000940223     C* inizializzazione videata
013100080915     C     $INZD0        IFEQ      *ON
013200080915     C                   EXSR      INZD0
013300080915     C                   MOVE      *OFF          $INZD0
013400940127     C                   ENDIF
013500940127     C*
013600030113     C*              *------------------*
013700080915     C                   EXFMT     D0
013800030113     C*              *------------------*
013900030113     C* Selezioni
0140009401271    C                   SELECT
014100940127     C* F3=Fine
014200940506     C     $TASTO        WHENEQ    F03
014300080915     C                   EXSR      F03Fine
014400080915     C* F18=cambio Filiale
014500080915     C     $TASTO        WHENEQ    F18
014600080915     C                   EXSR      F18Filiale
0147009401271O   C                   OTHER
014800940127     C* CONTROLLO DATI
014900080915     C                   EXSR      CTRD0
015000080915      *
015100940201     C     *IN99         IFEQ      *OFF
015200080915     C                   MOVE      'S1'          $GEST
015300940131     C                   END
015400080915     C*
0155009401271-   C                   ENDSL
015600080915     C*
015700940127     C                   ENDSR
015800080915     C/EJECT
015900080915     C************************************************************
016000080915     C* INIZIALIZZAZIONE primo video di Filtro
016100080915     C************************************************************
016200080915     C     INZD0         BEGSR
016300080915     C*
016400080915     C                   MOVEL     o36POU        d1PO
016500080915      *
016600080915     C                   ENDSR
016700940224     C/EJECT
016800940127     C************************************************************
016900080915     C* Controlla i campi del video di Filtro
017000940127     C************************************************************
017100080915     C     CTRD0         BEGSR
017200080915     C*
017300080915     c                   setoff                                       99
017400080915      *
017500080915      *   Controllo Filiale in Gestione - Filiale Amministrativa
017600080915      *
017700940127     C                   ENDSR
017800080915     C/EJECT
017900080915     C************************************************************
018000080915     C* Cambio di Filiale del video di Filtro
018100080915     C************************************************************
018200080915     C     F18Filiale    BEGSR
018300080915     C*
018400080915     c                   eval      *in01 = *on
018500080915      *
018600080915     C                   ENDSR
018700080915     C************************************************************
018800080915     C* GESTIONE LISTA
018900080915     C************************************************************
019000080915     C     GESS1         BEGSR
019100080915     C*
019200080915     C* inizializzazione videata
019300080915     C     $INZS1        IFEQ      *ON
019400080915     C                   EXSR      INZS1
019500080915     C                   MOVE      *OFF          $INZS1
019600080915     C                   ENDIF
019700080915     C*
019800080915     C* emissione piede videata
019900080915     C                   WRITE     Z1
020000080915     C*
020100080915     C* Non ci sono records
020200080915     C     WMAX          IFEQ      0
020300080915     C                   WRITE     D1
020400080915     C                   Else
020500080915     C     Wsfl          IFgt      0
020600080915     C                   Z-ADD     wsfl          C1RCD
020700080915     C                   Else
020800080915     C     Wpag          IFgt      0
020900080915     C                   Z-ADD     wpag          C1RCD
021000080915     C                   EndIF
021100080915     C                   EndIF
021200080915     C                   ENDIF
021300080915     C*
021400080915     C*              *------------------*
021500080915     C                   EXFMT     C1
021600080915     C*              *------------------*
021700080915     C*
021800080915     C     C1NRR         IFNE      0
021900080915     C                   Z-ADD     C1NRR         WSFL
022000080915     C                   ENDIF
022100080915     C                   Z-ADD     SFLNRR        C1RCD
022200080915     C* Selezioni
0223000809151    C                   SELECT
022400080915     C* F3=Fine
022500080915     C     $TASTO        WHENEQ    F03
022600080915     C                   EXSR      F03Fine
0227000809151O   C                   OTHER
022800080915     C* CONTROLLO DATI
022900080915     C                   EXSR      CTRC1
023000080915     C     *IN99         IFEQ      *OFF
023100080915     C                   EXSR      CTRS1
023200080915     C                   END
0233000809151-   C                   ENDSL
023400080915     C*
023500080915     C* F6=Aggiorna
023600080915     C     $TASTO        ifEQ      F06
023700080915     C                   Exsr      F06Aggiorna
023800080915     C                   END
023900080915     C*
024000080915     C                   ENDSR
024100080915     C/EJECT
024200080915     C************************************************************
024300080915     C* INIZIALIZZAZIONE LISTA
024400080915     C************************************************************
024500080915     C     INZS1         BEGSR
024600080915     C* pulizia SFL
024700080915     C                   SETOFF                                         3031
024800080915     C                   WRITE     C1
024900080915     C                   SETON                                          31
025000080915     C*
025100080915     C* CARICAMENTO SFL totale
025200080915     C                   Z-ADD     0             S1NRR
025300080915     C                   Z-ADD     1             C1RCD
025400080915     C                   Z-ADD     0             WMAX
025500080915     C*
025600080915     C* Posizionamento su file pilota
025700080915     c     *loval        setll     FIFGTl1L
025800080915    >C                   EXSR      REDANA
025900080915     C* Carico il SFL
026000080915     C                   EXSR      ROLS1
026100080915     C*
026200080915     C                   Z-ADD     1             WPAG
026300080915     C*
026400080915     C                   ENDSR
026500940127     C************************************************************
026600940131     C* CARICAMENTO PAGINA LISTA
026700940127     C************************************************************
026800940127     C     ROLS1         BEGSR
026900940127     C*
027000940128     C                   SETOFF                                       32
027100940223     C                   Z-ADD     0             Y
027200940127     C                   Z-ADD     WMAX          S1NRR
027300940127     C*
027400940127     C* Leggo dal file anagrafico per caricare la lista
0275009401311    C     $EFILE        DOWEQ     *OFF
027600940127     C*
027700030113     c                   clear                   s1opz
027800080915     c                   z-add     FGTLFGS       s1FGS
027900080915     c                   z-add     FGTLPDR       s1PDR
028000080915     c                   z-add     FGTLWTR       s1WTR
028100080915     c                   z-add     FGTLDDT       s1DDT
028200080915     c                   z-add     FGTLDCN       s1DCN
028300030113     C*
028400940127     C                   ADD       1             S1NRR
028500940127     C                   ADD       1             Y
028600940127     C*
028700940607     C                   WRITE     S1
028800940131     C*
028900940316    >C                   EXSR      REDANA
029000940128     C*
0291009401271-   C                   ENDDO
029200940127     C*
029300940223     C                   Z-ADD     S1NRR         WMAX                 30
029400940127     C*
029500940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
029600940127     C*
029700030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
029800940127     C                   MVR                     RESTO             3 0
029900030114     C     PAGINE        MULT      SFLPAG        C1RCD
0300000301141    C     RESTO         IFGT      0
030100030114     C                   ADD       1             C1RCD
0302000301141E   C                   ELSE
030300030114     C                   SUB       SFLPAG        C1RCD
030400030114     C                   ADD       1             C1RCD
0305000301141-   C                   ENDIF
030600940128     C*
030700940127     C                   ENDSR
030800940128     C************************************************************
030900940131     C* LETTURA RCD ARCHIVIO PILOTA
031000940128     C************************************************************
031100940607     C     REDANA        BEGSR
031200940128     C*
031300940131     C                   MOVEL     *OFF          $EFILE
031400940131     C                   MOVEL     *OFF          $RCDOK
031500940131     C*
0316009401311    C     $EFILE        DOUEQ     *ON
031700940131     C     $RCDOK        OREQ      *ON
031800940131     C*
031900080915     c                   Read      FIFGTL1L
032000080915     c                   if        %eof(FIFGTL1L)
032100030113     C                   MOVEL     *ON           $EFILE
032200030113     C                   MOVE      $EFILE        *IN33
032300030113     c                   else
032400030113     C                   MOVE      *ON           $RCDOK
032500030113     c                   end
032600940131     C*
032700080915     C*  se non rientra nei dati di filtro imposta OFF - $RCDOK
032800080915     C*
032900080915     C*
0330009401311-   C                   ENDDO
033100940131     C*
033200940131     C                   ENDSR
033300940224     C************************************************************
033400940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
033500940224     C************************************************************
033600940224     C     CLCPAG        BEGSR
033700940224     C* Input :
033800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
033900940224     C*          cursore
034000940224     C* - SFLPAG = numero rcd per pagina sfl
034100940224     C* Output :
034200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
034300940224     C*
034400940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
034500940224     C                   MVR                     RESTO             3 0
034600940224     C     RESTO         IFGT      0
034700940224     C                   ADD       1             PAGINE
034800940224     C                   ENDIF
034900940224     C     PAGINE        MULT      SFLPAG        WPAG
035000940224     C*
035100940224     C                   ENDSR
035200940309     C************************************************************
035300940309     C* GESTIONE F03 VIDEO S1
035400940309     C************************************************************
035500080915     C     F03Fine       BEGSR
035600940309     C*
035700940309     C                   MOVE      *ON           $FINE
035800940325     C* fine programma
035900940309     C                   ENDSR
036000940309     C/EJECT
036100940128     C************************************************************
036200940131     C* CONTROLLO TESTATA LISTA
036300940128     C************************************************************
036400940131     C     CTRC1         BEGSR
036500940128     C*
036600940201     C                   MOVE      *OFF          *IN99
036700940131     C*
036800940202     C                   ENDSR
036900940131     C************************************************************
037000940131     C* CONTROLLO OPZIONI LISTA
037100940131     C************************************************************
037200940131     C     CTRS1         BEGSR
037300940131     C*
037400940202     C                   MOVEL     *OFF          $ESCI
037500940201     C                   SETOFF                                       99
037600940131     C                   Z-ADD     0             S1OPZ
037700940131     C*
037800940127     C* Leggo il sfl solo se ci sono rcd
0379009401311    C     WMAX          IFGT      0
038000940607     C                   READC     S1                                     21
038100940127     C*
038200940131     C* esce se fine sfl o errore che richiede l'uscita
0383009401312    C     *IN21         DOWEQ     *OFF
038400940131     C     $ESCI         ANDEQ     *OFF
038500940201     C                   Z-ADD     S1NRR         C1RCD
038600940131     C* ctrl su riga
038700940131     C                   EXSR      RECS1
038800940131     C* gestione opzioni
0389009401313    C     S1OPZ         IFNE      0
039000940201     C     *IN99         ANDEQ     *OFF
039100940131     C                   EXSR      OPZS1
0392009401273-   C                   ENDIF
039300080915      *
039400940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0395009402013    C     *IN99         IFEQ      *ON
039600940201     C     $ESCI         OREQ      *ON
039700940131     C                   MOVE      *ON           *IN32
039800940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
039900940204     C* la ripristinerò a conclusione del ciclo di READC
040000940223     C                   MOVE      *OFF          $INZS1
0401009402233-   C                   ENDIF
040200940223     C*
040300940131     C                   Z-ADD     0             S1OPZ
040400940223     C*
040500940607     C                   UPDATE    S1
040600940223     C*
040700940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0408009401313    C     $ESCI         IFEQ      *OFF
040900940607     C                   READC     S1                                     21
041000940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0411009402014    C     *IN21         IFEQ      *ON
041200940201     C                   MOVE      WINZS1        $INZS1
041300940204     C* calcolo pagina a cui deve posizionarsi
041400940224     C                   EXSR      CLCPAG
0415009402014-   C                   ENDIF
0416009402013-   C                   ENDIF
041700940131     C*
0418009401272-   C                   ENDDO
041900940127     C*
0420009401311-   C                   ENDIF
042100940131     C*
042200940127     C                   ENDSR
042300940127     C/EJECT
042400940127     C************************************************************
042500940131     C* CONTROLLO CAMPI I/O RIGA LISTA
042600940127     C************************************************************
042700940131     C     RECS1         BEGSR
042800940131     C*
042900940201     C* reset indicatori DSPATR
043000940201     C                   MOVE      *ALL'0'       IN4049           10
043100940201     C                   MOVEA     IN4049        *IN(40)
043200940201     C*
043300940131     C                   ENDSR
043400940131     C************************************************************
043500940131     C* GESTIONE OPZIONI LISTA
043600940131     C************************************************************
043700940131     C     OPZS1         BEGSR
043800940201     C*
043900940201     C*
044000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0441009402252    C     *IN99         IFEQ      *ON
044200940223     C                   SETON                                        40
0443009402252-   C                   ENDIF
044400940225     C*
044500940131     C                   ENDSR
044600940223     C/EJECT
044700080915     C************************************************************
044800080915     C* Aggiorna la Data di Convalida Tariffa
044900080915     C************************************************************
045000080915     C     F06Aggiorna   BEGSR
045100080915     C*
045200080915     C*
045300080915     C*
045400080915     C                   ENDSR
045500080915     C/EJECT
045600940131     C************************************************************
045700940131     C* OPERAZIONI INIZIALI
045800940131     C************************************************************
045900940131     C     *INZSR        BEGSR
046000030113     C*
046100030113     C* Reperimento parametri
046200030113     C     *ENTRY        PLIST
046300030113     C                   PARM                    KPJBA
046400030113     C*
046500030113     C* Variabili per gestione videate
046600030113     C                   MOVE      *BLANK        $GEST             2
046700030113     C                   MOVE      *BLANK        $FINE             1
046800030113     C                   MOVE      *BLANK        $INZS1            1
046900080915     C                   MOVE      *BLANK        $INZD0            1
047000030113     C                   MOVE      *BLANK        $EFILE            1
047100030113     C                   MOVE      *BLANK        $ESCI             1
047200030113     C                   MOVE      *BLANK        $RCDOK            1
047300030113     C*
047400030113     C* Indici
047500030113     C                   Z-ADD     0             X                 3 0
047600030113     C                   Z-ADD     0             Y                 3 0
047700940506     C*
047800080915     C* Imposta la UDATE  da TIME
047900080915     C                   TIME                    W0140            14 0
048000080915     C                   MOVE      W0140         UDATE8            8 0
048100080915     C*
048200080915      *  In base all'utente prende il P.O.
048300080915     C                   CLEAR                   Tibs36Ds
048400080915     c                   EVAL      I36ute = Knmus
048500080915     c                   EVAL      I36Tla = 'L'
048600080915     C                   CALL      'TIBS36R'
048700080915     C                   PARM                    Tibs36Ds
048800080915     c                   movel     KNMUS         ALFA3             3
048900080915     C*
049000080915     C                   Z-ADD     1             CODUT
049100080915     C                   CALL      'X§PARUT'
049200080915     C                   PARM                    UT§DSE
049300080915     C                   MOVEL     RAGUT         RSUT             20
049400080915     C                   MOVEL     RAGUT         RSUT2            20
049500080915     C                   MOVEL     REC80         CNCR80
049600080915      *
049700940117     C                   ENDSR
049800030113     C************************************************************
