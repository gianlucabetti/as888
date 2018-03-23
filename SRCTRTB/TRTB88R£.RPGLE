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
002100030113     FTABEL00F  IF   E           K DISK
002200070625     FTRTB88D   CF   E             WORKSTN
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
003500030113     D  XTAKEY                 6     13
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
005400070627$003 D EXTDSC        E DS                  EXTNAME(DSCC)
005500070627$003 D EXTDSB        E DS                  EXTNAME(DSCB)
005600070627$003 D EXTDSA        E DS                  EXTNAME(DSCA)
005700070627$003 D SavKut          S                   Like(TblKut)
005800070627$003 D SavCod          S                   Like(TblCod)
005900070627$003 D SavKey          S                   Like(TblKey)
006000030113     D*-------------
006100030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006200030113$003 D S1NRR           S                   Like(C1rcd)
006300030113$003 D WSfl            S                   Like(C1nrr)
006400030113$003 D Wmax            S                   Like(C1rcd)
006500030113$003 D Wpag            S                   Like(C1rcd)
006600030113$003 D Winzs1          S                   Like($inzs1)
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
010300070625$015 I              S1DES                       §CCDES
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
018201070628     C* F8=Richiama gestione solo tabella CA
018202070628     C     $TASTO        WHENEQ    F08
018203070628     C                   call      'TRTB89R'
018204070628     c                   parm                    kpjba
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
020600940224     C*
020700940224     C* Posizionamento su file pilota
020800030113     c     kTBL          setll     TABEL00F
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
023100030113     c                   movel     tblkey        s1cod
023200070627     c                   movel     tbluni        extdsC
023300070625     C* Se Annullato
023400030115     c                   clear                   S1Ann
023500030115     c                   if        tblflg ='*'
023600030115     c                   move      'A'           S1Ann
023700030115     c                   end
023800070627     C                   eval      SavKEy = TBLKEY
023900070627      *
024000070627     C*   Descrizione Fedex
024100070627     c                   clear                   s1def
024200070627     c                   movel     'CB'          TBLCOD
024300070627     c     ktbl3         chain     tabel00f
024400070627     c                   if        %Found(tabel00f)
024500070627     c                   movel     tbluni        extdsB
024600070627     c                   movel     §CBDSF        s1def
024700070627     C                   End
024800070627      *
024900070627     C*   Ordinamento su Statistica Clienti
025000070627     c                   clear                   s1ord
025100070627     c                   movel     'CA'          TBLCOD
025200070627     c     ktbl3         chain     tabel00f
025300070627     c                   if        %Found(tabel00f)
025400070627     c                   movel     tbluni        extdsA
025500070627     c                   z-add     §CAORD        s1ord
025600070627     C                   End
025700070627      *
025800070627      * riposizionamento per ciclo principale di lettura
025900070627     c                   movel     'CC'          TBLCOD
026000070627     C                   eval      TblKEy = SavKEY
026100070627     c     ktbl3         setgt     tabel00f
026200940127     C*
026300940127     C                   ADD       1             S1NRR
026400940127     C                   ADD       1             Y
026500940127     C*
026600940607     C                   WRITE     S1
026700940131     C*
026800940316    >C                   EXSR      REDANA
026900940128     C*
0270009401271-   C                   ENDDO
027100940127     C*
027200940223     C                   Z-ADD     S1NRR         WMAX                 30
027300940127     C*
027400940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
027500940127     C*
027600030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
027700940127     C                   MVR                     RESTO             3 0
027800030114     C     PAGINE        MULT      SFLPAG        C1RCD
0279000301141    C     RESTO         IFGT      0
028000030114     C                   ADD       1             C1RCD
0281000301141E   C                   ELSE
028200030114     C                   SUB       SFLPAG        C1RCD
028300030114     C                   ADD       1             C1RCD
0284000301141-   C                   ENDIF
028500940128     C*
028600940127     C                   ENDSR
028700940128     C************************************************************
028800940131     C* LETTURA RCD ARCHIVIO PILOTA
028900940128     C************************************************************
029000940607     C     REDANA        BEGSR
029100940128     C*
029200940131     C                   MOVEL     *OFF          $EFILE
029300940131     C                   MOVEL     *OFF          $RCDOK
029400940131     C*
0295009401311    C     $EFILE        DOUEQ     *ON
029600940131     C     $RCDOK        OREQ      *ON
029700940131     C*
029800030113     c     kTBL          Reade     TABEL00F
029900030113     c                   if        %eof(Tabel00F)
030000030113     C                   MOVEL     *ON           $EFILE
030100030113     C                   MOVE      $EFILE        *IN33
030200030113     c                   else
030300030113     C                   MOVE      *ON           $RCDOK
030400030113     c                   end
030500940131     C*
0306009401311-   C                   ENDDO
030700940131     C*
030800940131     C                   ENDSR
030900940224     C************************************************************
031000940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
031100940224     C************************************************************
031200940224     C     CLCPAG        BEGSR
031300940224     C* Input :
031400940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
031500940224     C*          cursore
031600940224     C* - SFLPAG = numero rcd per pagina sfl
031700940224     C* Output :
031800940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
031900940224     C*
032000940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
032100940224     C                   MVR                     RESTO             3 0
032200940224     C     RESTO         IFGT      0
032300940224     C                   ADD       1             PAGINE
032400940224     C                   ENDIF
032500940224     C     PAGINE        MULT      SFLPAG        WPAG
032600940224     C*
032700940224     C                   ENDSR
032800940309     C************************************************************
032900940309     C* GESTIONE F03 VIDEO S1
033000940309     C************************************************************
033100940309     C     F03S1         BEGSR
033200940309     C*
033300940309     C                   MOVE      *ON           $FINE
033400940325     C* fine programma
033500940309     C                   ENDSR
033600940309     C/EJECT
033700940309     C************************************************************
033800940309     C* GESTIONE F10 VIDEO S1
033900940314     c* AGGIUNTA RECORD
034000940309     C************************************************************
034100940309     C     F10S1         BEGSR
034200940309     C*
034300030113     C                   RESET                   tabds
034400030113     C                   MOVEL     '01'          xtaopz
034500030113     C                   MOVE      *ZERO         xtaret
034600030113     C                   MOVE      *ZERO         xtaopr
034700030114     C                   MOVEl     *blank        xtakey
034800030113     C                   MOVE      *BLANKS       KPJBU
034900030113     C                   MOVEL     tabds         KPJBU
035000070625$004 C                   CALL      'TRTB88R2'
035100030113     C                   PARM                    KPJBA
035200030114     C                   MOVEL     KPJBU         tabds
035300030113      *
035400940309     C* ritorno da PGM gestione
035500940309     C                   EXSR      GESRET
035600940309     C     WINZS1        IFEQ      *ON
035700940309     C                   MOVE      *ON           $INZS1
035800940309     C* se esistevano già righe sul sfl
035900940309     C* calcolo pagina a cui deve posizionarsi
036000940309     C     WSFL          IFGT      0
036100940309     C                   EXSR      CLCPAG
036200940309     C* altrimenti carico solo la 1a pagina
036300940309     C                   ELSE
036400940309     C                   Z-ADD     SFLPAG        WPAG
036500940309     C                   ENDIF
036600940309     C                   ENDIF
036700940309     C*
036800940309     C                   ENDSR
036900940128     C************************************************************
037000940131     C* CONTROLLO TESTATA LISTA
037100940128     C************************************************************
037200940131     C     CTRC1         BEGSR
037300940128     C*
037400940201     C                   MOVE      *OFF          *IN99
037500940131     C*
037600940202     C                   ENDSR
037700940131     C************************************************************
037800940131     C* CONTROLLO OPZIONI LISTA
037900940131     C************************************************************
038000940131     C     CTRS1         BEGSR
038100940131     C*
038200940202     C                   MOVEL     *OFF          $ESCI
038300940201     C                   SETOFF                                       99
038400940131     C                   Z-ADD     0             S1OPZ
038500940131     C*
038600940127     C* Leggo il sfl solo se ci sono rcd
0387009401311    C     WMAX          IFGT      0
038800940607     C                   READC     S1                                     21
038900940127     C*
039000940131     C* esce se fine sfl o errore che richiede l'uscita
0391009401312    C     *IN21         DOWEQ     *OFF
039200940131     C     $ESCI         ANDEQ     *OFF
039300940201     C                   Z-ADD     S1NRR         C1RCD
039400940131     C* ctrl su riga
039500940131     C                   EXSR      RECS1
039600940131     C* gestione opzioni
0397009401313    C     S1OPZ         IFNE      0
039800940201     C     *IN99         ANDEQ     *OFF
039900940131     C                   EXSR      OPZS1
0400009401273-   C                   ENDIF
040100940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0402009402013    C     *IN99         IFEQ      *ON
040300940201     C     $ESCI         OREQ      *ON
040400940131     C                   MOVE      *ON           *IN32
040500940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
040600940204     C* la ripristinerò a conclusione del ciclo di READC
040700940223     C                   MOVE      *OFF          $INZS1
0408009402233-   C                   ENDIF
040900940223     C*
041000940131     C                   Z-ADD     0             S1OPZ
041100940223     C*
041200940607     C                   UPDATE    S1
041300940223     C*
041400940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0415009401313    C     $ESCI         IFEQ      *OFF
041600940607     C                   READC     S1                                     21
041700940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0418009402014    C     *IN21         IFEQ      *ON
041900940201     C                   MOVE      WINZS1        $INZS1
042000940204     C* calcolo pagina a cui deve posizionarsi
042100940224     C                   EXSR      CLCPAG
0422009402014-   C                   ENDIF
0423009402013-   C                   ENDIF
042400940131     C*
0425009401272-   C                   ENDDO
042600940127     C*
0427009401311-   C                   ENDIF
042800940131     C*
042900940127     C                   ENDSR
043000940127     C/EJECT
043100940127     C************************************************************
043200940131     C* CONTROLLO CAMPI I/O RIGA LISTA
043300940127     C************************************************************
043400940131     C     RECS1         BEGSR
043500940131     C*
043600940201     C* reset indicatori DSPATR
043700940201     C                   MOVE      *ALL'0'       IN4049           10
043800940201     C                   MOVEA     IN4049        *IN(40)
043900940201     C*
044000940131     C                   ENDSR
044100940131     C************************************************************
044200940131     C* GESTIONE OPZIONI LISTA
044300940131     C************************************************************
044400940131     C     OPZS1         BEGSR
044500940201     C*
044600030113     C                   RESET                   tabds
044700030113     C                   MOVEL     S1OPZ         xtaopz
044800030113     C                   MOVE      *ZERO         xtaret
044900030113     C                   MOVE      *ZERO         xtaopr
045000030113     C                   MOVEl     S1COD         xtakey
045100940715     C                   MOVE      *BLANKS       KPJBU
045200030113     C                   MOVEL     tabds         KPJBU
045300070625$004 C                   CALL      'TRTB88R2'
045400940607     C                   PARM                    KPJBA
045500030114     C                   MOVEL     KPJBU         tabds
045600940201     C*
045700940223     C* ritorno da PGM gestione
045800940223     C                   EXSR      GESRET
045900940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0460009402252    C     *IN99         IFEQ      *ON
046100940223     C                   SETON                                        40
0462009402252-   C                   ENDIF
046300940225     C*
046400940131     C                   ENDSR
046500940223     C/EJECT
046600940223     C************************************************************
046700940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
046800940223     C************************************************************
046900940223     C     GESRET        BEGSR
047000940223     C*
047100940223     C* modo di ritorno
047200940223     C*
0473009402231    C                   SELECT
047400940314    >C* << questi modi di utilizzo dei valori di ritorno dal
047500940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
047600940314    >C*    proposte, normalmente sempre valide, ma modificabili
047700940314    >C*    per situazioni particolari >>
047800940223     C* 1 = F3
047900030113    >C     xtaret        WHENEQ    '1'
048000940224     C                   MOVE      *ON           $ESCI
048100940223     C                   MOVE      *ON           $FINE
048200940223     C* 2 = F12
048300030113    >C     xtaret        WHENEQ    '2'
048400940223     C                   MOVE      *ON           $ESCI
048500940223     C*
0486009402231-   C                   ENDSL
048700940223     C*
048800940223     C* operazione eseguite dal pgm chiamato
048900940223     C*
0490009402231    C                   SELECT
049100940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
049200030113    >C     xtaopr        WHENEQ    '1'
049300940223     C                   MOVE      *ON           WINZS1
049400940223     C*
0495009402231-   C                   ENDSL
049600940223     C*
049700940223     C* funzione non eseguibile per errore :
049800940223     C*
0499009402231    C                   SELECT
050000940223     C* 1 = funzione richiamata chiusa in errore
050100940316    >C*  eventualmente gestire altri codici di errore
050200030113    >C     xtaerr        WHENEQ    '1'
050300940223     C                   MOVE      *ON           $ESCI
050400940223     C                   SETON                                        5299
050500940223     C*
0506009402231-   C                   ENDSL
050700940223     C*
050800940223     C                   ENDSR
050900940223     C/EJECT
051000940131     C************************************************************
051100940131     C* OPERAZIONI INIZIALI
051200940131     C************************************************************
051300940131     C     *INZSR        BEGSR
051400030113     C*
051500030113     C* Reperimento parametri
051600030113     C     *ENTRY        PLIST
051700030113     C                   PARM                    KPJBA
051800030113     C*
051900030113     C* Variabili per gestione videate
052000030113     C                   MOVE      *BLANK        $GEST             2
052100030113     C                   MOVE      *BLANK        $FINE             1
052200030113     C                   MOVE      *BLANK        $INZS1            1
052300030113     C                   MOVE      *BLANK        $EFILE            1
052400030113     C                   MOVE      *BLANK        $ESCI             1
052500030113     C                   MOVE      *BLANK        $RCDOK            1
052600030113     C*
052700030113     C     KTBL          KLIST
052800030113     C                   KFLD                    TBLKUT
052900030113     C                   KFLD                    TBLCOD
053000070627     C*
053100070627     C     KTBL3         KLIST
053200070627     C                   KFLD                    TBLKUT
053300070627     C                   KFLD                    TBLCOD
053400070627     C                   KFLD                    TBLKEY
053500030113     C* Indici
053600030113     C                   Z-ADD     0             X                 3 0
053700030113     C                   Z-ADD     0             Y                 3 0
053800940506     C*
053900940506     C* Reperimento tasti di funzione
054000030113     C                   MOVE      1             tblkut
054100070625     C                   MOVE      'CC'          tblcod
054200940127     C*
054300940117     C                   ENDSR
054400030113     C************************************************************
