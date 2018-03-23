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
002400090320     FTRMZ70SD0 CF   E             WORKSTN
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
006600940207     D*-------------
006700940211     D* COSTANTI
006800940211     D*-------------
006900030113     D SFLPAG          C                   CONST(11)
007000940314     D* dimensione della schiera $MS1
007100940506     D*
007200940506     D* Tasti di funzione
007300940506     D F01             C                   CONST(X'31')
007400940506     D F02             C                   CONST(X'32')
007500940506     D F03             C                   CONST(X'33')
007600940506     D F04             C                   CONST(X'34')
007700940506     D F05             C                   CONST(X'35')
007800940506     D F06             C                   CONST(X'36')
007900940506     D F07             C                   CONST(X'37')
008000940506     D F08             C                   CONST(X'38')
008100940506     D F09             C                   CONST(X'39')
008200940506     D F10             C                   CONST(X'3A')
008300940506     D F11             C                   CONST(X'3B')
008400940506     D F12             C                   CONST(X'3C')
008500940506     D F13             C                   CONST(X'B1')
008600940506     D F14             C                   CONST(X'B2')
008700940506     D F15             C                   CONST(X'B3')
008800940506     D F16             C                   CONST(X'B4')
008900940506     D F17             C                   CONST(X'B5')
009000940506     D F18             C                   CONST(X'B6')
009100940506     D F19             C                   CONST(X'B7')
009200940506     D F20             C                   CONST(X'B8')
009300940506     D F21             C                   CONST(X'B9')
009400940506     D F22             C                   CONST(X'BA')
009500940506     D F23             C                   CONST(X'BB')
009600940506     D F24             C                   CONST(X'BC')
009700940506     D ENTER           C                   CONST(X'F1')
009800940506     D ROLDWN          C                   CONST(X'F4')
009900940506     D ROLLUP          C                   CONST(X'F5')
010000940127     C*----------------------------------------------------*
010100940127     C*                MAIN LINE PROGRAM
010200940127     C*----------------------------------------------------*
010300940223     C* inizializzazione variabili
010400940223     C                   EXSR      INZVAR
010500940223     C*
010600940223     C     $FINE         DOWEQ     *OFF
010700940131     C     $GEST         CASEQ     'S1'          GESS1
010800940117     C                   END
010900940117     C                   END
011000940325     C* fine programma
011100940325     C                   SETON                                            LR
011200030113     C************************************************************
011300030113     C* INIZIALIZZAZIONE VARIABILI
011400030113     C************************************************************
011500030113     C     INZVAR        BEGSR
011600030113     C*
011700030113     C* Pulizia campi e indicatori
011800030113     C                   MOVE      *ALL'0'       IN4049           10
011900030113     C                   MOVEA     IN4049        *IN(40)
012000030113     C                   CLEAR                   S1OPZ
012100030113     C* Variabili per gestione videate
012200030113     C*
012300030113     C                   MOVE      *OFF          $FINE
012400030113     C                   MOVE      *OFF          $INZS1
012500030113     C                   MOVE      *OFF          $EFILE
012600030113     C                   MOVE      *OFF          $ESCI
012700030113     C                   MOVE      *OFF          $RCDOK
012800030113     C                   Z-ADD     0             $ULKS1            3 0
012900030113     C*
013000030113     C                   MOVE      *ON           $INZS1
013100030113     C                   MOVE      'S1'          $GEST
013200030113     C*
013300030113     C* Variabili appoggio
013400030114     C                   Z-ADD     1             WPAG
013500030113     c*
013600030113     C                   ENDSR
013700940127     C************************************************************
013800940131     C* GESTIONE LISTA
013900940127     C************************************************************
014000940127     C     GESS1         BEGSR
014100030113     C*
014200940223     C* inizializzazione videata
014300940223     C     $INZS1        IFEQ      *ON
014400940127     C                   EXSR      INZS1
014500940223     C                   MOVE      *OFF          $INZS1
014600940127     C                   ENDIF
014700090320      *
014800090320     C* Se alla fine è un record solo
014900090320      *  Restituisco direttamente senza emettere il SFL di selezione
015000090320     C     WMAX          IFEQ      1
015100090323     C     WMAX          oreq      0
015200090320     C*
015300090320     c                   exsr      imposta_record
015400090320     C*
015500090320     c                   else
015600030113     C*
015700030113     C* emissione piede videata
015800030113     C                   WRITE     Z1
015900030113     C* Non ci sono records
016000940223     C     WMAX          IFEQ      0
016100940607     C                   WRITE     D1
016200090320     c                   setoff                                       30
016300030114     C                   Else
016400090320     c                   seton                                        30
016500090320      *
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
018800090320     C* F12=Ritorno
018900090320     C     $TASTO        WHENEQ    F12
019000090320     C                   EXSR      F12S1
0191009401271O   C                   OTHER
019200940127     C* CONTROLLO DATI
019300940131     C                   EXSR      CTRC1
019400940201     C     *IN99         IFEQ      *OFF
019500940131     C                   EXSR      CTRS1
019600940131     C                   END
0197009401271-   C                   ENDSL
019800940127     C*
019900090320     C                   EndIF
020000090320     C*
020100940127     C                   ENDSR
020200940224     C/EJECT
020300940127     C************************************************************
020400940131     C* INIZIALIZZAZIONE LISTA
020500940127     C************************************************************
020600940127     C     INZS1         BEGSR
020700940302     C* pulizia SFL
020800940128     C                   SETOFF                                         3031
020900940607     C                   WRITE     C1
021000940128     C                   SETON                                          31
021100940128     C*
021200030113     C* CARICAMENTO SFL totale
021300940201     C                   Z-ADD     0             S1NRR
021400030113     C                   Z-ADD     1             C1RCD
021500940128     C                   Z-ADD     0             WMAX
021600940224     C*
021700940224     C* Posizionamento su file pilota
021800090320     c                   eval      traNRC = xtaKEY
021900090320     C                   Z-ADD     traNRC        C1NRC
022000090320     c     ktra01        setll     aitra01l
022100940608    >C                   EXSR      REDANA
022200030113     C* Carico il SFL
022300940127     C                   EXSR      ROLS1
022400030113     C*
022500030114     c                   if        xtaopr <> '1'
022600030114     C                   Z-ADD     1             WPAG
022700030114     c                   end
022800940127     C*
022900940127     C                   ENDSR
023000940127     C************************************************************
023100940131     C* CARICAMENTO PAGINA LISTA
023200940127     C************************************************************
023300940127     C     ROLS1         BEGSR
023400940127     C*
023500940128     C                   SETOFF                                       32
023600940223     C                   Z-ADD     0             Y
023700940127     C                   Z-ADD     WMAX          S1NRR
023800940127     C*
023900940127     C* Leggo dal file anagrafico per caricare la lista
0240009401311    C     $EFILE        DOWEQ     *OFF
024100940127     C*
024200030113     c                   clear                   s1opz
024300090320     c                   z-add     trakau        s1cod
024400090320     c                   z-add     trabpt        s1cod2
024500090507     c                   clear                   s1des
024600090507     c                   clear                   s1des2
024700090320     c                   eval      tipo = 'A'
024800090320     c                   eval      autista = trakau
024900090320     c     kAUT          chain     fiapd01l
025000090320     c                   if        %Found(fiapd01l)
025100090420     c                   eval      s1des   = apdrsf
025200090320     C                   End
025300090320     c                   z-add     rrnaitra      h1record
025400090320     c                   eval      tipo = 'D'
025500090320     c                   eval      autista = trabpt
025600090320     c     kAUT          chain     fiapd01l
025700090320     c                   if        %Found(fiapd01l)
025800090420     c                   eval      s1des2  = apdrsf
025900090320     C                   End
026000030113     C*
026100090320     C*  Decodifica Rag.Sociale
026200090410     c                   if        s1cod > 0 or  s1cod2 > 0
026300940127     C                   ADD       1             S1NRR
026400940127     C                   ADD       1             Y
026500940127     C*
026600940607     C                   WRITE     S1
026700090410     c                   end
026800940131     C*
026900940316    >C                   EXSR      REDANA
027000940128     C*
0271009401271-   C                   ENDDO
027200940127     C*
027300940223     C                   Z-ADD     S1NRR         WMAX                 30
027400940127     C*
027500940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
027600940127     C*
027700030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
027800940127     C                   MVR                     RESTO             3 0
027900030114     C     PAGINE        MULT      SFLPAG        C1RCD
0280000301141    C     RESTO         IFGT      0
028100030114     C                   ADD       1             C1RCD
0282000301141E   C                   ELSE
028300030114     C                   SUB       SFLPAG        C1RCD
028400030114     C                   ADD       1             C1RCD
0285000301141-   C                   ENDIF
028600940128     C*
028700940127     C                   ENDSR
028800940128     C************************************************************
028900940131     C* LETTURA RCD ARCHIVIO PILOTA
029000940128     C************************************************************
029100940607     C     REDANA        BEGSR
029200940128     C*
029300940131     C                   MOVEL     *OFF          $EFILE
029400940131     C                   MOVEL     *OFF          $RCDOK
029500940131     C*
0296009401311    C     $EFILE        DOUEQ     *ON
029700940131     C     $RCDOK        OREQ      *ON
029800940131     C*
029900090320     c     ktra01        ReadE     aitra01l
030000090320     c                   if        %eof(aitra01l)
030100030113     C                   MOVEL     *ON           $EFILE
030200030113     C                   MOVE      $EFILE        *IN33
030300030113     c                   else
030400090320      *
030500090320      * Non Disaccreditato
030600090320     c                   if        traDFI = 0
030700030113     C                   MOVE      *ON           $RCDOK
030800090320     c                   end
030900090320      *
031000030113     c                   end
031100940131     C*
0312009401311-   C                   ENDDO
031300940131     C*
031400940131     C                   ENDSR
031500940224     C************************************************************
031600940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
031700940224     C************************************************************
031800940224     C     CLCPAG        BEGSR
031900940224     C* Input :
032000940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
032100940224     C*          cursore
032200940224     C* - SFLPAG = numero rcd per pagina sfl
032300940224     C* Output :
032400940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
032500940224     C*
032600940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
032700940224     C                   MVR                     RESTO             3 0
032800940224     C     RESTO         IFGT      0
032900940224     C                   ADD       1             PAGINE
033000940224     C                   ENDIF
033100940224     C     PAGINE        MULT      SFLPAG        WPAG
033200940224     C*
033300940224     C                   ENDSR
033400940309     C************************************************************
033500940309     C* GESTIONE F03 VIDEO S1
033600940309     C************************************************************
033700940309     C     F03S1         BEGSR
033800940309     C*
033900090327     c                   eval      xtaRET = '1'
034000940309     C                   MOVE      *ON           $FINE
034100940325     C* fine programma
034200940309     C                   ENDSR
034300940309     C/EJECT
034400090320     C************************************************************
034500090320     C* GESTIONE F12 VIDEO S1
034600090320     C************************************************************
034700090320     C     F12S1         BEGSR
034800090320     C*
034900090327     c                   eval      xtaRET = '2'
035000090320     C                   MOVE      *ON           $FINE
035100090320     C* fine programma
035200090320     C                   ENDSR
035300090320     C/EJECT
035400940128     C************************************************************
035500940131     C* CONTROLLO TESTATA LISTA
035600940128     C************************************************************
035700940131     C     CTRC1         BEGSR
035800940128     C*
035900940201     C                   MOVE      *OFF          *IN99
036000940131     C*
036100940202     C                   ENDSR
036200940131     C************************************************************
036300940131     C* CONTROLLO OPZIONI LISTA
036400940131     C************************************************************
036500940131     C     CTRS1         BEGSR
036600940131     C*
036700940202     C                   MOVEL     *OFF          $ESCI
036800940201     C                   SETOFF                                       99
036900940131     C                   Z-ADD     0             S1OPZ
037000940131     C*
037100940127     C* Leggo il sfl solo se ci sono rcd
0372009401311    C     WMAX          IFGT      0
037300940607     C                   READC     S1                                     21
037400940127     C*
037500940131     C* esce se fine sfl o errore che richiede l'uscita
0376009401312    C     *IN21         DOWEQ     *OFF
037700940131     C     $ESCI         ANDEQ     *OFF
037800940201     C                   Z-ADD     S1NRR         C1RCD
037900940131     C* ctrl su riga
038000940131     C                   EXSR      RECS1
038100940131     C* gestione opzioni
0382009401313    C     S1OPZ         IFNE      0
038300940201     C     *IN99         ANDEQ     *OFF
038400940131     C                   EXSR      OPZS1
0385009401273-   C                   ENDIF
038600940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0387009402013    C     *IN99         IFEQ      *ON
038800940201     C     $ESCI         OREQ      *ON
038900940131     C                   MOVE      *ON           *IN32
039000940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
039100940204     C* la ripristinerò a conclusione del ciclo di READC
039200940223     C                   MOVE      *OFF          $INZS1
0393009402233-   C                   ENDIF
039400940223     C*
039500940131     C                   Z-ADD     0             S1OPZ
039600940223     C*
039700940607     C                   UPDATE    S1
039800940223     C*
039900940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0400009401313    C     $ESCI         IFEQ      *OFF
040100940607     C                   READC     S1                                     21
040200940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0403009402014    C     *IN21         IFEQ      *ON
040400940201     C                   MOVE      WINZS1        $INZS1
040500940204     C* calcolo pagina a cui deve posizionarsi
040600940224     C                   EXSR      CLCPAG
0407009402014-   C                   ENDIF
0408009402013-   C                   ENDIF
040900940131     C*
0410009401272-   C                   ENDDO
041100940127     C*
0412009401311-   C                   ENDIF
041300940131     C*
041400940127     C                   ENDSR
041500940127     C/EJECT
041600940127     C************************************************************
041700940131     C* CONTROLLO CAMPI I/O RIGA LISTA
041800940127     C************************************************************
041900940131     C     RECS1         BEGSR
042000940131     C*
042100940201     C* reset indicatori DSPATR
042200940201     C                   MOVE      *ALL'0'       IN4049           10
042300940201     C                   MOVEA     IN4049        *IN(40)
042400940201     C*
042500940131     C                   ENDSR
042600940131     C************************************************************
042700940131     C* GESTIONE OPZIONI LISTA
042800940131     C************************************************************
042900940131     C     OPZS1         BEGSR
043000940201     C*
043100090320     C                   RESET                   xtabds
043200030113     C                   MOVEL     S1OPZ         xtaopz
043300030113     C                   MOVE      *ZERO         xtaret
043400030113     C                   MOVE      *ZERO         xtaopr
043500090320     C                   MOVEl     H1record      xtarecord
043600940715     C                   MOVE      *BLANKS       KPJBU
043700090320     C                   MOVEL     xtabds        KPJBU
043800090320     C                   MOVEL     *ON           $ESCI
043900090320     C                   MOVE      *ON           $FINE
044000940201     C*
044100940223     C* ritorno da PGM gestione
044200940223     C                   EXSR      GESRET
044300940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0444009402252    C     *IN99         IFEQ      *ON
044500940223     C                   SETON                                        40
0446009402252-   C                   ENDIF
044700940225     C*
044800940131     C                   ENDSR
044900940223     C/EJECT
045000940223     C************************************************************
045100940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
045200940223     C************************************************************
045300940223     C     GESRET        BEGSR
045400940223     C*
045500940223     C* modo di ritorno
0456009402231    C                   SELECT
045700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
045800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
045900940314    >C*    proposte, normalmente sempre valide, ma modificabili
046000940314    >C*    per situazioni particolari >>
046100940223     C* 1 = F3
046200030113    >C     xtaret        WHENEQ    '1'
046300940224     C                   MOVE      *ON           $ESCI
046400940223     C                   MOVE      *ON           $FINE
046500940223     C* 2 = F12
046600030113    >C     xtaret        WHENEQ    '2'
046700940223     C                   MOVE      *ON           $ESCI
046800940223     C*
0469009402231-   C                   ENDSL
047000940223     C*
047100940223     C* operazione eseguite dal pgm chiamato
047200940223     C*
0473009402231    C                   SELECT
047400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
047500030113    >C     xtaopr        WHENEQ    '1'
047600940223     C                   MOVE      *ON           WINZS1
047700940223     C*
0478009402231-   C                   ENDSL
047900940223     C*
048000940223     C* funzione non eseguibile per errore :
048100940223     C*
0482009402231    C                   SELECT
048300940223     C* 1 = funzione richiamata chiusa in errore
048400940316    >C*  eventualmente gestire altri codici di errore
048500030113    >C     xtaerr        WHENEQ    '1'
048600940223     C                   MOVE      *ON           $ESCI
048700940223     C                   SETON                                        5299
048800940223     C*
0489009402231-   C                   ENDSL
049000940223     C*
049100940223     C                   ENDSR
049200940223     C/EJECT
049300940131     C************************************************************
049400940131     C* OPERAZIONI INIZIALI
049500940131     C************************************************************
049600940131     C     *INZSR        BEGSR
049700030113     C*
049800030113     C* Reperimento parametri
049900030113     C     *ENTRY        PLIST
050000030113     C                   PARM                    KPJBA
050100090320     C                   eval      xtabds = KPJBU
050200030113     C*
050300030113     C* Variabili per gestione videate
050400030113     C                   MOVE      *BLANK        $GEST             2
050500030113     C                   MOVE      *BLANK        $FINE             1
050600030113     C                   MOVE      *BLANK        $INZS1            1
050700030113     C                   MOVE      *BLANK        $EFILE            1
050800030113     C                   MOVE      *BLANK        $ESCI             1
050900030113     C                   MOVE      *BLANK        $RCDOK            1
051000030113     C*
051100090320     C     Ktra01        KLIST
051200090320     C                   KFLD                    TraNRC
051300090320     C*
051400090320     c     Kaut          klist
051500090320     c                   kfld                    tipo              1
051600090320     c                   kfld                    autista           7 0
051700030113     C* Indici
051800030113     C                   Z-ADD     0             X                 3 0
051900030113     C                   Z-ADD     0             Y                 3 0
052000090320     C*
052100090320     C                   ENDSR
052200090320     C/EJECT
052300090320     C************************************************************
052400090320     C* Imposta il record   da restituire
052500090320     C************************************************************
052600090320     C     Imposta_RecordBEGSR
052700090320     C*
052800090320     C                   RESET                   xtabds
052900090320     C                   MOVEL     '1'           xtaopz
053000090320     C                   MOVE      *ZERO         xtaret
053100090320     C                   MOVE      *ZERO         xtaopr
053200090320     C                   MOVEl     H1record      xtarecord
053300090320     C                   MOVE      *BLANKS       KPJBU
053400090320     C                   MOVEL     xtabds        KPJBU
053500090320     C*
053600090320     C                   MOVE      *ON           $ESCI
053700090323     C                   MOVE      *ON           $FINE
053800940127     C*
053900940117     C                   ENDSR
054000030113     C************************************************************
