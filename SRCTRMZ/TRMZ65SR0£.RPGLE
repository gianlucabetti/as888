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
006501120123     d dataiso         s               d   datfmt(*iso)
006502120123     d dataeur         s               d   datfmt(*eur)
006503120123     d datagma         s               d   datfmt(*dmy)
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
012000030113     C* Variabili per gestione videate
012100030113     C*
012200030113     C                   MOVE      *OFF          $FINE
012300030113     C                   MOVE      *OFF          $INZS1
012400030113     C                   MOVE      *OFF          $EFILE
012500030113     C                   MOVE      *OFF          $ESCI
012600030113     C                   MOVE      *OFF          $RCDOK
012700030113     C                   Z-ADD     0             $ULKS1            3 0
012800030113     C*
012900030113     C                   MOVE      *ON           $INZS1
013000030113     C                   MOVE      'S1'          $GEST
013100030113     C*
013200030113     C* Variabili appoggio
013300030114     C                   Z-ADD     1             WPAG
013400030113     c*
013500030113     C                   ENDSR
013600940127     C************************************************************
013700940131     C* GESTIONE LISTA
013800940127     C************************************************************
013900940127     C     GESS1         BEGSR
014000030113     C*
014100940223     C* inizializzazione videata
014200940223     C     $INZS1        IFEQ      *ON
014300940127     C                   EXSR      INZS1
014400940223     C                   MOVE      *OFF          $INZS1
014500940127     C                   ENDIF
014600090320      *
014700090827     C* Se alla fine non ha nemmeno un record
014800090320      *  Restituisco direttamente senza emettere il SFL di selezione
014900090827     C     WMAX          Ifeq      0
015000090320     C*
015100090320     c                   exsr      imposta_record
015200090320     C*
015300090320     c                   else
015400030113     C*
015500030113     C* emissione piede videata
015600030113     C                   WRITE     Z1
015700030113     C* Non ci sono records
015800940223     C     WMAX          IFEQ      0
015900940607     C                   WRITE     D1
016000090320     c                   setoff                                       30
016100030114     C                   Else
016200090320     c                   seton                                        30
016300090320      *
016400030114     C     Wsfl          IFgt      0
016500030114     C                   Z-ADD     wsfl          C1RCD
016600030114     C                   Else
016700030114     C     Wpag          IFgt      0
016800030114     C                   Z-ADD     wpag          C1RCD
016900030114     C                   EndIF
017000030114     C                   EndIF
017100030114     C                   ENDIF
017200940127     C*
017300030113     C*              *------------------*
017400940607     C                   EXFMT     C1
017500030113     C*              *------------------*
017600030113     C*
017700940204     C     C1NRR         IFNE      0
017800940204     C                   Z-ADD     C1NRR         WSFL
017900940204     C                   ENDIF
018000940127     C                   Z-ADD     SFLNRR        C1RCD
018100030113     C* Selezioni
0182009401271    C                   SELECT
018300940127     C* F3=Fine
018400940506     C     $TASTO        WHENEQ    F03
018500940309     C                   EXSR      F03S1
018600090320     C* F12=Ritorno
018700090320     C     $TASTO        WHENEQ    F12
018800090320     C                   EXSR      F12S1
0189009401271O   C                   OTHER
019000940127     C* CONTROLLO DATI
019100940131     C                   EXSR      CTRC1
019200940201     C     *IN99         IFEQ      *OFF
019300940131     C                   EXSR      CTRS1
019400940131     C                   END
0195009401271-   C                   ENDSL
019600940127     C*
019700090320     C                   EndIF
019800090320     C*
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
021400090827     C                   eval      c1pgm = DSPGM
021500940224     C*
021600940224     C* Posizionamento su file pilota
021700090320     c                   eval      traNRC = xtaKEY
021800090320     C                   Z-ADD     traNRC        C1NRC
021900090320     c     ktra01        setll     aitra01l
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
024100090320     c                   z-add     trakau        s1cod
024200090320     c                   z-add     trabpt        s1cod2
024300090507     c                   clear                   s1des
024400090507     c                   clear                   s1des2
024500090320     c                   eval      tipo = 'A'
024600090320     c                   eval      autista = trakau
024700090320     c     kAUT          chain     fiapd01l
024800090320     c                   if        %Found(fiapd01l)
024900090420     c                   eval      s1des   = apdrsf
025000090320     C                   End
025100090320     c                   z-add     rrnaitra      h1record
025200090320     c                   eval      tipo = 'D'
025300090320     c                   eval      autista = trabpt
025400090320     c     kAUT          chain     fiapd01l
025500090320     c                   if        %Found(fiapd01l)
025600090420     c                   eval      s1des2  = apdrsf
025700090320     C                   End
025701120123     C*
025702120123     C* Data Disaccreditamento aggiunta Gen.2012
025703120123     c                   setoff                                       88
025704120123     C                   clear                   S1DISAC
025705120123     c                   if        traDFI >0
025706120123     C                   move      tradfi        campo06           6
025707120123     C                   move      campo06       campo02           2
025708120123     C                   movel     campo06       S1DISAC
025709120123     C                   movel     campo02       S1DISAC
025710120123     C                   movel     campo06       campo02
025711120123     C                   move      campo02       S1DISAC
025712120123     c                   seton                                        88
025713120123     c                   end
025714120123     C*
025800030113     C*
025900090320     C*  Decodifica Rag.Sociale
026000090410     c                   if        s1cod > 0 or  s1cod2 > 0
026100940127     C                   ADD       1             S1NRR
026200940127     C                   ADD       1             Y
026300940127     C*
026400940607     C                   WRITE     S1
026500090410     c                   end
026600940131     C*
026700940316    >C                   EXSR      REDANA
026800940128     C*
0269009401271-   C                   ENDDO
027000940127     C*
027100940223     C                   Z-ADD     S1NRR         WMAX                 30
027200940127     C*
027300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
027400940127     C*
027500030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
027600940127     C                   MVR                     RESTO             3 0
027700030114     C     PAGINE        MULT      SFLPAG        C1RCD
0278000301141    C     RESTO         IFGT      0
027900030114     C                   ADD       1             C1RCD
0280000301141E   C                   ELSE
028100030114     C                   SUB       SFLPAG        C1RCD
028200030114     C                   ADD       1             C1RCD
0283000301141-   C                   ENDIF
028400940128     C*
028500940127     C                   ENDSR
028600940128     C************************************************************
028700940131     C* LETTURA RCD ARCHIVIO PILOTA
028800940128     C************************************************************
028900940607     C     REDANA        BEGSR
029000940128     C*
029100940131     C                   MOVEL     *OFF          $EFILE
029200940131     C                   MOVEL     *OFF          $RCDOK
029300940131     C*
0294009401311    C     $EFILE        DOUEQ     *ON
029500940131     C     $RCDOK        OREQ      *ON
029600940131     C*
029700090320     c     ktra01        ReadE     aitra01l
029800090320     c                   if        %eof(aitra01l)
029900030113     C                   MOVEL     *ON           $EFILE
030000030113     C                   MOVE      $EFILE        *IN33
030100030113     c                   else
030200090320      *
030201120124     C                   MOVE      *ON           $RCDOK
030300120124      * Se Disaccreditato e richiesto Dettaglio dei soli accreditati
030301120123     c*************  Richiesta modifica di volerli vedere tutti (Schettini Gen.2012)
030302120124     c                   if        traDFI > 0 and xtaOPZ ='D'
030500120124     C                   MOVE      *OFF          $RCDOK
030600120124     c                   end
030700090320      *
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
032400940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
032500940224     C                   MVR                     RESTO             3 0
032600940224     C     RESTO         IFGT      0
032700940224     C                   ADD       1             PAGINE
032800940224     C                   ENDIF
032900940224     C     PAGINE        MULT      SFLPAG        WPAG
033000940224     C*
033100940224     C                   ENDSR
033200940309     C************************************************************
033300940309     C* GESTIONE F03 VIDEO S1
033400940309     C************************************************************
033500940309     C     F03S1         BEGSR
033600940309     C*
033700090327     c                   eval      xtaRET = '1'
033800940309     C                   MOVE      *ON           $FINE
033900940325     C* fine programma
034000940309     C                   ENDSR
034100940309     C/EJECT
034200090320     C************************************************************
034300090320     C* GESTIONE F12 VIDEO S1
034400090320     C************************************************************
034500090320     C     F12S1         BEGSR
034600090320     C*
034700090327     c                   eval      xtaRET = '2'
034800090320     C                   MOVE      *ON           $FINE
034900090320     C* fine programma
035000090320     C                   ENDSR
035100090320     C/EJECT
035200940128     C************************************************************
035300940131     C* CONTROLLO TESTATA LISTA
035400940128     C************************************************************
035500940131     C     CTRC1         BEGSR
035600940128     C*
035700940201     C                   MOVE      *OFF          *IN99
035800940131     C*
035900940202     C                   ENDSR
036000940131     C************************************************************
036100940131     C* CONTROLLO OPZIONI LISTA
036200940131     C************************************************************
036300940131     C     CTRS1         BEGSR
036400940131     C*
036500940202     C                   MOVEL     *OFF          $ESCI
036600940201     C                   SETOFF                                       99
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
037800940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0379009402013    C     *IN99         IFEQ      *ON
038000940201     C     $ESCI         OREQ      *ON
038100940131     C                   MOVE      *ON           *IN32
038200940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
038300940204     C* la ripristinerò a conclusione del ciclo di READC
038400940223     C                   MOVE      *OFF          $INZS1
0385009402233-   C                   ENDIF
038600940223     C*
038700940607     C                   UPDATE    S1
038800940223     C*
038900940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0390009401313    C     $ESCI         IFEQ      *OFF
039100940607     C                   READC     S1                                     21
039200940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0393009402014    C     *IN21         IFEQ      *ON
039400940201     C                   MOVE      WINZS1        $INZS1
039500940204     C* calcolo pagina a cui deve posizionarsi
039600940224     C                   EXSR      CLCPAG
0397009402014-   C                   ENDIF
0398009402013-   C                   ENDIF
039900940131     C*
0400009401272-   C                   ENDDO
040100940127     C*
0402009401311-   C                   ENDIF
040300940131     C*
040400940127     C                   ENDSR
040500940127     C/EJECT
040600940127     C************************************************************
040700940131     C* CONTROLLO CAMPI I/O RIGA LISTA
040800940127     C************************************************************
040900940131     C     RECS1         BEGSR
041000940131     C*
041100940201     C* reset indicatori DSPATR
041200940201     C                   MOVE      *ALL'0'       IN4049           10
041300940201     C                   MOVEA     IN4049        *IN(40)
041400940201     C*
041500940131     C                   ENDSR
041600940223     C/EJECT
041700940131     C************************************************************
041800940131     C* OPERAZIONI INIZIALI
041900940131     C************************************************************
042000940131     C     *INZSR        BEGSR
042100030113     C*
042200030113     C* Reperimento parametri
042300030113     C     *ENTRY        PLIST
042400030113     C                   PARM                    KPJBA
042500090320     C                   eval      xtabds = KPJBU
042600030113     C*
042700030113     C* Variabili per gestione videate
042800030113     C                   MOVE      *BLANK        $GEST             2
042900030113     C                   MOVE      *BLANK        $FINE             1
043000030113     C                   MOVE      *BLANK        $INZS1            1
043100030113     C                   MOVE      *BLANK        $EFILE            1
043200030113     C                   MOVE      *BLANK        $ESCI             1
043300030113     C                   MOVE      *BLANK        $RCDOK            1
043400030113     C*
043500090320     C     Ktra01        KLIST
043600090320     C                   KFLD                    TraNRC
043700090320     C*
043800090320     c     Kaut          klist
043900090320     c                   kfld                    tipo              1
044000090320     c                   kfld                    autista           7 0
044100030113     C* Indici
044200030113     C                   Z-ADD     0             X                 3 0
044300030113     C                   Z-ADD     0             Y                 3 0
044400090320     C*
044500090320     C                   ENDSR
044600090320     C/EJECT
044700090320     C************************************************************
044800090320     C* Imposta il record   da restituire
044900090320     C************************************************************
045000090320     C     Imposta_RecordBEGSR
045100090320     C*
045200090320     C                   RESET                   xtabds
045300090320     C                   MOVEL     '1'           xtaopz
045400090320     C                   MOVE      *ZERO         xtaret
045500090320     C                   MOVE      *ZERO         xtaopr
045600090320     C                   MOVEl     H1record      xtarecord
045700090320     C                   MOVE      *BLANKS       KPJBU
045800090320     C                   MOVEL     xtabds        KPJBU
045900090320     C*
046000090320     C                   MOVE      *ON           $ESCI
046100090323     C                   MOVE      *ON           $FINE
046200940127     C*
046300940117     C                   ENDSR
046400030113     C************************************************************
