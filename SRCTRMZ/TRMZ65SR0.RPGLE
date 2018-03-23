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
002100090320     FFiapd01L  iF   E           K DISK
002200090320     Faitra01l  IF   E           K DISK
002300090320     F                                     infds(dsaitra)
002400090827     FTRMZ65SD0 CF   E             WORKSTN
002500940607     F                                     SFILE(S1:S1NRR)
002600940201     F                                     INFDS(DSFMT)
002700940128     D*----------------------------------------------------*
002800090320     D dsaitra         DS
002900090320     D  rrnaitra             397    400b 0
003000090320      *-------------
003100940211     D* Passaggio Parametri
003200940211     D KPJBA         E DS
003300030113      *-------------
003400940325     D* Parametri in ricezione
003500090320     D xTABDS          DS
003600030113     D  XTAOPZ                 1      2
003700030113     D  XTARET                 3      3
003800030113     D  XTAOPR                 4      4
003900030113     D  XTAERR                 5      5
004000090320     D  XTArecord              6     14  0
004100090320     D  XTAKEY                15     21  0
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
005900030113     D*-------------
006000030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006100030113$003 D S1NRR           S                   Like(C1rcd)
006200030113$003 D WSfl            S                   Like(C1nrr)
006300030113$003 D Wmax            S                   Like(C1rcd)
006400030113$003 D Wpag            S                   Like(C1rcd)
006500030113$003 D Winzs1          S                   Like($inzs1)
006600120123     d dataiso         s               d   datfmt(*iso)
006700120123     d dataeur         s               d   datfmt(*eur)
006800120123     d datagma         s               d   datfmt(*dmy)
006900940207     D*-------------
007000940211     D* COSTANTI
007100940211     D*-------------
007200030113     D SFLPAG          C                   CONST(11)
007300940314     D* dimensione della schiera $MS1
007400940506     D*
007500940506     D* Tasti di funzione
007600940506     D F01             C                   CONST(X'31')
007700940506     D F02             C                   CONST(X'32')
007800940506     D F03             C                   CONST(X'33')
007900940506     D F04             C                   CONST(X'34')
008000940506     D F05             C                   CONST(X'35')
008100940506     D F06             C                   CONST(X'36')
008200940506     D F07             C                   CONST(X'37')
008300940506     D F08             C                   CONST(X'38')
008400940506     D F09             C                   CONST(X'39')
008500940506     D F10             C                   CONST(X'3A')
008600940506     D F11             C                   CONST(X'3B')
008700940506     D F12             C                   CONST(X'3C')
008800940506     D F13             C                   CONST(X'B1')
008900940506     D F14             C                   CONST(X'B2')
009000940506     D F15             C                   CONST(X'B3')
009100940506     D F16             C                   CONST(X'B4')
009200940506     D F17             C                   CONST(X'B5')
009300940506     D F18             C                   CONST(X'B6')
009400940506     D F19             C                   CONST(X'B7')
009500940506     D F20             C                   CONST(X'B8')
009600940506     D F21             C                   CONST(X'B9')
009700940506     D F22             C                   CONST(X'BA')
009800940506     D F23             C                   CONST(X'BB')
009900940506     D F24             C                   CONST(X'BC')
010000940506     D ENTER           C                   CONST(X'F1')
010100940506     D ROLDWN          C                   CONST(X'F4')
010200940506     D ROLLUP          C                   CONST(X'F5')
010300940127     C*----------------------------------------------------*
010400940127     C*                MAIN LINE PROGRAM
010500940127     C*----------------------------------------------------*
010600940223     C* inizializzazione variabili
010700940223     C                   EXSR      INZVAR
010800940223     C*
010900940223     C     $FINE         DOWEQ     *OFF
011000940131     C     $GEST         CASEQ     'S1'          GESS1
011100940117     C                   END
011200940117     C                   END
011300940325     C* fine programma
011400940325     C                   SETON                                            LR
011500030113     C************************************************************
011600030113     C* INIZIALIZZAZIONE VARIABILI
011700030113     C************************************************************
011800030113     C     INZVAR        BEGSR
011900030113     C*
012000030113     C* Pulizia campi e indicatori
012100030113     C                   MOVE      *ALL'0'       IN4049           10
012200030113     C                   MOVEA     IN4049        *IN(40)
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
014900090320      *
015000090827     C* Se alla fine non ha nemmeno un record
015100090320      *  Restituisco direttamente senza emettere il SFL di selezione
015200090827     C     WMAX          Ifeq      0
015300090320     C*
015400090320     c                   exsr      imposta_record
015500090320     C*
015600090320     c                   else
015700030113     C*
015800030113     C* emissione piede videata
015900030113     C                   WRITE     Z1
016000030113     C* Non ci sono records
016100940223     C     WMAX          IFEQ      0
016200940607     C                   WRITE     D1
016300090320     c                   setoff                                       30
016400030114     C                   Else
016500090320     c                   seton                                        30
016600090320      *
016700030114     C     Wsfl          IFgt      0
016800030114     C                   Z-ADD     wsfl          C1RCD
016900030114     C                   Else
017000030114     C     Wpag          IFgt      0
017100030114     C                   Z-ADD     wpag          C1RCD
017200030114     C                   EndIF
017300030114     C                   EndIF
017400030114     C                   ENDIF
017500940127     C*
017600030113     C*              *------------------*
017700940607     C                   EXFMT     C1
017800030113     C*              *------------------*
017900030113     C*
018000940204     C     C1NRR         IFNE      0
018100940204     C                   Z-ADD     C1NRR         WSFL
018200940204     C                   ENDIF
018300940127     C                   Z-ADD     SFLNRR        C1RCD
018400030113     C* Selezioni
0185009401271    C                   SELECT
018600940127     C* F3=Fine
018700940506     C     $TASTO        WHENEQ    F03
018800940309     C                   EXSR      F03S1
018900090320     C* F12=Ritorno
019000090320     C     $TASTO        WHENEQ    F12
019100090320     C                   EXSR      F12S1
0192009401271O   C                   OTHER
019300940127     C* CONTROLLO DATI
019400940131     C                   EXSR      CTRC1
019500940201     C     *IN99         IFEQ      *OFF
019600940131     C                   EXSR      CTRS1
019700940131     C                   END
0198009401271-   C                   ENDSL
019900940127     C*
020000090320     C                   EndIF
020100090320     C*
020200940127     C                   ENDSR
020300940224     C/EJECT
020400940127     C************************************************************
020500940131     C* INIZIALIZZAZIONE LISTA
020600940127     C************************************************************
020700940127     C     INZS1         BEGSR
020800940302     C* pulizia SFL
020900940128     C                   SETOFF                                         3031
021000940607     C                   WRITE     C1
021100940128     C                   SETON                                          31
021200940128     C*
021300030113     C* CARICAMENTO SFL totale
021400940201     C                   Z-ADD     0             S1NRR
021500030113     C                   Z-ADD     1             C1RCD
021600940128     C                   Z-ADD     0             WMAX
021700090827     C                   eval      c1pgm = DSPGM
021800940224     C*
021900940224     C* Posizionamento su file pilota
022000090320     c                   eval      traNRC = xtaKEY
022100090320     C                   Z-ADD     traNRC        C1NRC
022200090320     c     ktra01        setll     aitra01l
022300940608    >C                   EXSR      REDANA
022400030113     C* Carico il SFL
022500940127     C                   EXSR      ROLS1
022600030113     C*
022700030114     c                   if        xtaopr <> '1'
022800030114     C                   Z-ADD     1             WPAG
022900030114     c                   end
023000940127     C*
023100940127     C                   ENDSR
023200940127     C************************************************************
023300940131     C* CARICAMENTO PAGINA LISTA
023400940127     C************************************************************
023500940127     C     ROLS1         BEGSR
023600940127     C*
023700940128     C                   SETOFF                                       32
023800940223     C                   Z-ADD     0             Y
023900940127     C                   Z-ADD     WMAX          S1NRR
024000940127     C*
024100940127     C* Leggo dal file anagrafico per caricare la lista
0242009401311    C     $EFILE        DOWEQ     *OFF
024300940127     C*
024400090320     c                   z-add     trakau        s1cod
024500090320     c                   z-add     trabpt        s1cod2
024600090507     c                   clear                   s1des
024700090507     c                   clear                   s1des2
024800090320     c                   eval      tipo = 'A'
024900090320     c                   eval      autista = trakau
025000090320     c     kAUT          chain     fiapd01l
025100090320     c                   if        %Found(fiapd01l)
025200090420     c                   eval      s1des   = apdrsf
025300090320     C                   End
025400090320     c                   z-add     rrnaitra      h1record
025500090320     c                   eval      tipo = 'D'
025600090320     c                   eval      autista = trabpt
025700090320     c     kAUT          chain     fiapd01l
025800090320     c                   if        %Found(fiapd01l)
025900090420     c                   eval      s1des2  = apdrsf
026000090320     C                   End
026100120123     C*
026200120123     C* Data Disaccreditamento aggiunta Gen.2012
026300120123     c                   setoff                                       88
026400120123     C                   clear                   S1DISAC
026500120123     c                   if        traDFI >0
026600120123     C                   move      tradfi        campo06           6
026700120123     C                   move      campo06       campo02           2
026800120123     C                   movel     campo06       S1DISAC
026900120123     C                   movel     campo02       S1DISAC
027000120123     C                   movel     campo06       campo02
027100120123     C                   move      campo02       S1DISAC
027200120123     c                   seton                                        88
027300120123     c                   end
027400120123     C*
027500030113     C*
027600090320     C*  Decodifica Rag.Sociale
027700090410     c                   if        s1cod > 0 or  s1cod2 > 0
027800940127     C                   ADD       1             S1NRR
027900940127     C                   ADD       1             Y
028000940127     C*
028100940607     C                   WRITE     S1
028200090410     c                   end
028300940131     C*
028400940316    >C                   EXSR      REDANA
028500940128     C*
0286009401271-   C                   ENDDO
028700940127     C*
028800940223     C                   Z-ADD     S1NRR         WMAX                 30
028900940127     C*
029000940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
029100940127     C*
029200030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
029300940127     C                   MVR                     RESTO             3 0
029400030114     C     PAGINE        MULT      SFLPAG        C1RCD
0295000301141    C     RESTO         IFGT      0
029600030114     C                   ADD       1             C1RCD
0297000301141E   C                   ELSE
029800030114     C                   SUB       SFLPAG        C1RCD
029900030114     C                   ADD       1             C1RCD
0300000301141-   C                   ENDIF
030100940128     C*
030200940127     C                   ENDSR
030300940128     C************************************************************
030400940131     C* LETTURA RCD ARCHIVIO PILOTA
030500940128     C************************************************************
030600940607     C     REDANA        BEGSR
030700940128     C*
030800940131     C                   MOVEL     *OFF          $EFILE
030900940131     C                   MOVEL     *OFF          $RCDOK
031000940131     C*
0311009401311    C     $EFILE        DOUEQ     *ON
031200940131     C     $RCDOK        OREQ      *ON
031300940131     C*
031400090320     c     ktra01        ReadE     aitra01l
031500090320     c                   if        %eof(aitra01l)
031600030113     C                   MOVEL     *ON           $EFILE
031700030113     C                   MOVE      $EFILE        *IN33
031800030113     c                   else
031900090320      *
032000120124     C                   MOVE      *ON           $RCDOK
032100120124      * Se Disaccreditato e richiesto Dettaglio dei soli accreditati
032200120123     c*************  Richiesta modifica di volerli vedere tutti (Schettini Gen.2012)
032300120124     c                   if        traDFI > 0 and xtaOPZ ='D'
032400120124     C                   MOVE      *OFF          $RCDOK
032500120124     c                   end
032600090320      *
032700030113     c                   end
032800940131     C*
0329009401311-   C                   ENDDO
033000940131     C*
033100940131     C                   ENDSR
033200940224     C************************************************************
033300940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
033400940224     C************************************************************
033500940224     C     CLCPAG        BEGSR
033600940224     C* Input :
033700940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
033800940224     C*          cursore
033900940224     C* - SFLPAG = numero rcd per pagina sfl
034000940224     C* Output :
034100940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
034200940224     C*
034300940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
034400940224     C                   MVR                     RESTO             3 0
034500940224     C     RESTO         IFGT      0
034600940224     C                   ADD       1             PAGINE
034700940224     C                   ENDIF
034800940224     C     PAGINE        MULT      SFLPAG        WPAG
034900940224     C*
035000940224     C                   ENDSR
035100940309     C************************************************************
035200940309     C* GESTIONE F03 VIDEO S1
035300940309     C************************************************************
035400940309     C     F03S1         BEGSR
035500940309     C*
035600090327     c                   eval      xtaRET = '1'
035700940309     C                   MOVE      *ON           $FINE
035800940325     C* fine programma
035900940309     C                   ENDSR
036000940309     C/EJECT
036100090320     C************************************************************
036200090320     C* GESTIONE F12 VIDEO S1
036300090320     C************************************************************
036400090320     C     F12S1         BEGSR
036500090320     C*
036600090327     c                   eval      xtaRET = '2'
036700090320     C                   MOVE      *ON           $FINE
036800090320     C* fine programma
036900090320     C                   ENDSR
037000090320     C/EJECT
037100940128     C************************************************************
037200940131     C* CONTROLLO TESTATA LISTA
037300940128     C************************************************************
037400940131     C     CTRC1         BEGSR
037500940128     C*
037600940201     C                   MOVE      *OFF          *IN99
037700940131     C*
037800940202     C                   ENDSR
037900940131     C************************************************************
038000940131     C* CONTROLLO OPZIONI LISTA
038100940131     C************************************************************
038200940131     C     CTRS1         BEGSR
038300940131     C*
038400940202     C                   MOVEL     *OFF          $ESCI
038500940201     C                   SETOFF                                       99
038600940131     C*
038700940127     C* Leggo il sfl solo se ci sono rcd
0388009401311    C     WMAX          IFGT      0
038900940607     C                   READC     S1                                     21
039000940127     C*
039100940131     C* esce se fine sfl o errore che richiede l'uscita
0392009401312    C     *IN21         DOWEQ     *OFF
039300940131     C     $ESCI         ANDEQ     *OFF
039400940201     C                   Z-ADD     S1NRR         C1RCD
039500940131     C* ctrl su riga
039600940131     C                   EXSR      RECS1
039700940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0398009402013    C     *IN99         IFEQ      *ON
039900940201     C     $ESCI         OREQ      *ON
040000940131     C                   MOVE      *ON           *IN32
040100940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
040200940204     C* la ripristinerò a conclusione del ciclo di READC
040300940223     C                   MOVE      *OFF          $INZS1
0404009402233-   C                   ENDIF
040500940223     C*
040600940607     C                   UPDATE    S1
040700940223     C*
040800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0409009401313    C     $ESCI         IFEQ      *OFF
041000940607     C                   READC     S1                                     21
041100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0412009402014    C     *IN21         IFEQ      *ON
041300940201     C                   MOVE      WINZS1        $INZS1
041400940204     C* calcolo pagina a cui deve posizionarsi
041500940224     C                   EXSR      CLCPAG
0416009402014-   C                   ENDIF
0417009402013-   C                   ENDIF
041800940131     C*
0419009401272-   C                   ENDDO
042000940127     C*
0421009401311-   C                   ENDIF
042200940131     C*
042300940127     C                   ENDSR
042400940127     C/EJECT
042500940127     C************************************************************
042600940131     C* CONTROLLO CAMPI I/O RIGA LISTA
042700940127     C************************************************************
042800940131     C     RECS1         BEGSR
042900940131     C*
043000940201     C* reset indicatori DSPATR
043100940201     C                   MOVE      *ALL'0'       IN4049           10
043200940201     C                   MOVEA     IN4049        *IN(40)
043300940201     C*
043400940131     C                   ENDSR
043500940223     C/EJECT
043600940131     C************************************************************
043700940131     C* OPERAZIONI INIZIALI
043800940131     C************************************************************
043900940131     C     *INZSR        BEGSR
044000030113     C*
044100030113     C* Reperimento parametri
044200030113     C     *ENTRY        PLIST
044300030113     C                   PARM                    KPJBA
044400090320     C                   eval      xtabds = KPJBU
044500030113     C*
044600030113     C* Variabili per gestione videate
044700030113     C                   MOVE      *BLANK        $GEST             2
044800030113     C                   MOVE      *BLANK        $FINE             1
044900030113     C                   MOVE      *BLANK        $INZS1            1
045000030113     C                   MOVE      *BLANK        $EFILE            1
045100030113     C                   MOVE      *BLANK        $ESCI             1
045200030113     C                   MOVE      *BLANK        $RCDOK            1
045300030113     C*
045400090320     C     Ktra01        KLIST
045500090320     C                   KFLD                    TraNRC
045600090320     C*
045700090320     c     Kaut          klist
045800090320     c                   kfld                    tipo              1
045900090320     c                   kfld                    autista           7 0
046000030113     C* Indici
046100030113     C                   Z-ADD     0             X                 3 0
046200030113     C                   Z-ADD     0             Y                 3 0
046300090320     C*
046400090320     C                   ENDSR
046500090320     C/EJECT
046600090320     C************************************************************
046700090320     C* Imposta il record   da restituire
046800090320     C************************************************************
046900090320     C     Imposta_RecordBEGSR
047000090320     C*
047100090320     C                   RESET                   xtabds
047200090320     C                   MOVEL     '1'           xtaopz
047300090320     C                   MOVE      *ZERO         xtaret
047400090320     C                   MOVE      *ZERO         xtaopr
047500090320     C                   MOVEl     H1record      xtarecord
047600090320     C                   MOVE      *BLANKS       KPJBU
047700090320     C                   MOVEL     xtabds        KPJBU
047800090320     C*
047900090320     C                   MOVE      *ON           $ESCI
048000090323     C                   MOVE      *ON           $FINE
048100940127     C*
048200940117     C                   ENDSR
048300030113     C************************************************************
