000100970214     H DECEDIT('0,') DATEDIT(*DMY.)
000200030729      *
000300940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000400940620      *  03           PROTECT TUTTI I CAMPI
000500030123      *  09           PROTECT dei campi voce C/E
000600940223      *  21           GENERICO OPERAZIONI I/O
000700940223      *  22           GENERICO ERRORE OPERAZIONI I/O
000800940223      *  30           SFLDSP
000900940223      * N31           SFLCLR
001000940128      *  31           SFLDSPCTL
001100940128      *  32           SFLNXTCHG
001200940128      *  33           SFLEND
001300940128      *  39           OF PRTF
001400940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001500940317    > *  Specificare l'uso dei singoli indicatori
001600940315      *  50 <---> 98  ERRORI SU VIDEO
001700940317    > *  Specificare l'uso dei singoli indicatori
001800940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
001900940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002000940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002100940128      *  99           INDIC. GENERALE DI ERRORE
002200940128     F*----------------------------------------------------*
002300161010$001 FFIEU63D   CF   E             WORKSTN
002400940201     F                                     INFDS(DSFMT)
002500161011     Ftitas30c  iF   E           k DISK
002600940128     D*----------------------------------------------------*
002700940127     D* Passaggio Parametri
002800940127     D KPJBA         E DS
002900940201     D*-------------
003000030204     D  S_TASTO        S                   like($Tasto)
003100940201     D DSFMT           DS
003200940506     D  $TASTO               369    369
003300940201     D  NRG                  370    370
003400940201     D  NCL                  371    371
003500940201     D*-------------
003600940201     D* posizione cursore
003700940201     D CURSOR          DS
003800940223     D  RIRI                   1      2B 0 INZ
003900940201     D  R$$                    2      2
004000940223     D  COCO                   3      4B 0 INZ
004100940201     D  C$$                    4      4
004200161010     D*
004300161010     D fieu18r1DS    e DS
004400161010     D*-------------
004500161011     D dataeur         s               d   datfmt(*eur)
004600161011     D dataiso         s               d   datfmt(*iso)
004700161011     D udate3m         s               d   datfmt(*iso)                         3 mesi fa
004800940207     D*-------------
004900940207     D* Reperimento nome PGM
005000940207     D STATUS         SDS           333
005100940207     D  DSPGM            *PROC
005200940225     D*-------------
005300940225     D* COSTANTI
005400940225     D*-------------
005500940506     D* Tasti di funzione
005600940506     D F01             C                   CONST(X'31')
005700940506     D F02             C                   CONST(X'32')
005800940506     D F03             C                   CONST(X'33')
005900940506     D F04             C                   CONST(X'34')
006000940506     D F05             C                   CONST(X'35')
006100940506     D F06             C                   CONST(X'36')
006200940506     D F07             C                   CONST(X'37')
006300940506     D F08             C                   CONST(X'38')
006400940506     D F09             C                   CONST(X'39')
006500940506     D F10             C                   CONST(X'3A')
006600940506     D F11             C                   CONST(X'3B')
006700940506     D F12             C                   CONST(X'3C')
006800940506     D F13             C                   CONST(X'B1')
006900940506     D F14             C                   CONST(X'B2')
007000940506     D F15             C                   CONST(X'B3')
007100940506     D F16             C                   CONST(X'B4')
007200940506     D F17             C                   CONST(X'B5')
007300940506     D F18             C                   CONST(X'B6')
007400940506     D F19             C                   CONST(X'B7')
007500940506     D F20             C                   CONST(X'B8')
007600940506     D F21             C                   CONST(X'B9')
007700940506     D F22             C                   CONST(X'BA')
007800940506     D F23             C                   CONST(X'BB')
007900940506     D F24             C                   CONST(X'BC')
008000940506     D ENTER           C                   CONST(X'F1')
008100940506     D ROLDWN          C                   CONST(X'F4')
008200940506     D ROLLUP          C                   CONST(X'F5')
008300940506     D*-------------
008400941108     D DATA            C                   CONST('0001-01-01')
008500030114     D OPz_Imm         C                   CONST('IMMISSIONE')
008600940127     C*----------------------------------------------------*
008700940127     C*                MAIN LINE PROGRAM
008800940127     C*----------------------------------------------------*
008900940223     C     $FINE         DOWEQ     *OFF
009000940202     C     $GEST         CASEQ     'D1'          GESD1
009100940117     C                   END
009200940117     C                   END
009300940325     C* fine programma
009400030113     C                   Seton                                        LR
009500940131     C************************************************************
009600940510     C* GESTIONE VIDEO RECORD D1
009700940131     C************************************************************
009800940127     C     GESD1         BEGSR
009900030113      *
010000940225     C* inizializzazione videata
010100940223     C     $INZD1        IFEQ      *ON
010200940127     C                   EXSR      INZD1
010300940223     C                   MOVE      *OFF          $INZD1
010400940117     C                   END
010500030113      *
010600940225     C* emissione piede videata se proveniente da altra
010700940225     C* salvataggio valore formato di provenienza
010800940207     C     $LASTV        IFNE      'D1'
010900940614     C                   WRITE     T1
011000940202     C                   MOVE      $LASTV        $LASTG
011100940202     C                   MOVE      'D1'          $LASTV
011200940202     C                   END
011300940131     C*
011400030728     c     video1        tag
011500030114     c                   SETOFF                                         99
011600940620     C* Immissione/Modifica/Copia
011700030113     C*               *----------------*
011800940620     C                   EXFMT     D1
011900030113     C*               *----------------*
012000030114     C     *IN99         IFEQ      *OFF
012100030114     C*
0122000301141    C                   SELECT
012300030114     C* F3=Fine
012400030114     C     $TASTO        WHENEQ    F03
012500030114     C                   EXSR      F03D1
012600030114     C*
0127000301141O   C                   OTHER
012800030114     C*
012900030114     C                   EXSR      CTRD1
013000030728     c   99              goto      video1
013100030113     C*
013200940616     C     *IN99         IFEQ      *OFF
013300940616     C* F6
0134000302052    C     $TASTO        ifeq      F06
013500940224     C* eseguo aggiornamento
013600161010    >C                   EXSR      ESEGUE
013700030205     c                   end
013800030114     C*
013900030114     C                   ENDIF
014000030114     C*
0141000301142-   C                   ENDSL
014200030114     C*
0143000301142-   C                   ENDIF
014400940224     C*
014500940117     C                   ENDSR
014600940117      ************************************************************
014700940207      * INIZIALIZZAZIONE VIDEATA DATI
014800940117      ************************************************************
014900940127     C     INZD1         BEGSR
015000940131     C*
015100940615     C                   CLEAR                   D1
015200940224     C*
015300940117     C                   ENDSR
015400940309     C************************************************************
015500940309     C* GESTIONE F03 VIDEO D1
015600940309     C************************************************************
015700940309     C     F03D1         BEGSR
015800940309     C*
015900940309     C                   MOVE      *ON           $FINE
016000940325     C*
016100940325     C                   ENDSR
016200940131     C************************************************************
016300940207     C* CONTROLLO VIDEATA
016400940131     C************************************************************
016500940127     C     CTRD1         BEGSR
016600940131     C*
016700940127     C                   SETOFF                                       99
016800030123      *
016900940127     C* non imposto pos. del cursore
017000940127     C                   Z-ADD     0             H1RIGA
017100940127     C                   Z-ADD     0             H1COLO
017200030728     c*
017300030114     C* controllo codice vuoto
017400161010$017 C     vidprc        IFEQ      *zeros
017500161010     C                   SETON                                        5199
017600161010     C                   goto      err1
017700161010     C                   Else
017800161010      **
017900161010     c                   clear                   trovato           1
018000161011     c                   clear                   viddat
018100161010     c                   movel     vidprc        alfprc           14
018200161010     C/EXEC SQL
018300171108     C+   SELECT 'S', PNDaas, PNDlnp, PNDnrs, PNDnsp
018400161011     C+   INTO :trovato, :vidaas, :vidlnp, :vidnrs, :vidnsp
018500171108     C+     FROM fipnd00F join TITAS00F
018600171108     C+    ON pndAAS = TASaas and
018700171108     C+       pndLNP = TASlnp and
018800171108     C+       pndNRS = TASnrs and
018900171108     C+       pndNSP = TASnsp
019000171108     C+    WHERE
019100171108     C+   PNDipn =:alfprc
019200161010     c+ FETCH FIRST 1 ROW ONLY
019300161010     C/END-EXEC
019400161010      *
019500161010     c                   if        trovato = *blank
019600171108     C/EXEC SQL
019700171108     C+   SELECT 'S', PNDaas, PNDlnp, PNDnrs, PNDnsp
019800171108     C+   INTO :trovato, :vidaas, :vidlnp, :vidnrs, :vidnsp
019900171108     C+     FROM fipnd00F join TITAS10F
020000171108     C+    ON pndAAS = TASaas and
020100171108     C+       pndLNP = TASlnp and
020200171108     C+       pndNRS = TASnrs and
020300171108     C+       pndNSP = TASnsp
020400171108     C+    WHERE
020500171108     C+   PNDipn =:alfprc
020600171108     c+ FETCH FIRST 1 ROW ONLY
020700171108     C/END-EXEC
020800171108     c                   if        trovato = *blank
020900171108     C/EXEC SQL
021000171108     C+   SELECT 'S', PNDaas, PNDlnp, PNDnrs, PNDnsp
021100171108     C+   INTO :trovato, :vidaas, :vidlnp, :vidnrs, :vidnsp
021200171108     C+     FROM fipnd00F join TITASP0F
021300171108     C+    ON pndAAS = TASaas and
021400171108     C+       pndLNP = TASlnp and
021500171108     C+       pndNRS = TASnrs and
021600171108     C+       pndNSP = TASnsp
021700171108     C+    WHERE
021800171108     C+   PNDipn =:alfprc
021900171108     c+ FETCH FIRST 1 ROW ONLY
022000171108     C/END-EXEC
022100161011     C                   End
022200171108     C                   EndIF
022300161011      *
022400161011     c                   if        trovato = 'S'
022500161011     C     Ktas          chain     titas30c
022600161011     c                   if        %Found(titas30c)
022700161011     c                   movel     tasaas        datatas           8 0
022800161011     c                   move      tasmgs        datatas
022900161011     c                   move      datatas       dataiso
023000161011     c                   move      dataiso       dataeur
023100161011     c                   move      dataeur       viddat
023200161011      *** non può essere più vecchia di 3 mesi
023300161011     c                   if        udate3m > dataiso
023400161011     C                   SETON                                        5399
023500161011     C                   goto      err1
023600161010     C                   End
023700170411      ** controlla che non vi sia già un fuori misura inserito sulla bolla
023800170411     c                   if        tasftc ='F' or tastc2='F'
023900170411     C                   SETON                                        5599
024000170411     C                   goto      err1
024100170411     C                   End
024200161011     C                   End
024300161011      *
024400161011     C                   End
024500161010      *
024600161010     C                   Endif
024700030114      *
024800161010$017 C     vidnsp        IFEQ      *zeros
024900161010     C                   SETON                                        5299
025000161010     C                   goto      err1
025100161010     C                   End
025200170117      *
025300170117      *  x NON codificato dare errore
025400170117     c                   if        %subst(%editc(tasKSC:'X'):4:4) = '8888' or
025500170117     c                             %subst(%editc(tasKSC:'X'):4:4) = '9999'
025600170117     C                   SETON                                        5499
025700170117     C                   goto      err1
025800170117     C                   End
025900161010      *
026000161010     c     err1          tag
026100940131     C*
026200940302     C     *IN99         IFEQ      *ON
026300940510     C     $GEST         ANDEQ     'D1'
026400940117     C                   MOVEA     *IN           WIN
026500940128     C                   MOVE      *ALL'0'       IN5098           49
026600940128     C                   MOVEA     IN5098        *IN(50)
026700940614     C                   WRITE     D1
026800940117     C                   MOVEA     WIN           *IN
026900940302     C                   ENDIF
027000940127     C*
027100940117     C                   ENDSR
027200940207     C/EJECT
027300940224     C************************************************************
027400161010    >C*  conferma esecuzione
027500940224     C************************************************************
027600161010    >C     ESEGUE        BEGSR
027700161010
027800161010      ** Fuori Misura
027900161010     c                   clear                   fieu18r1DS
028000161011     c                   eval         I18AAS     = vidaas
028100161010     c                   eval         I18LNP     = vidlnp
028200161010     c                   eval         I18NRS     = vidnrs
028300161010     c                   eval         I18NSP     = vidnsp
028400161010     c                   eval         I18PARCEL  = alfprc
028500161010     c                   eval         I18WEIGHT  = 0
028600161010     c                   eval         I18FCLR    = 'F'
028700170523     c                   eval         I18FOVR    = 'O'
028800161010      **
028900161010     c                   call      'FIEU18R1'
029000161010     c                   parm                    kpjba
029100161010     c                   parm                    fieu18r1ds
029200161010      *
029300170410      *   esito OK ricarica la videata
029400170410     c                   if        O18ESITO = '0'
029500161010     C                   MOVE      *ON           $INZD1
029600170410     c                   elseif    O18ESITO = '9'
029700170410     c                   eval      D1ERR1 = 'Attenzione: aggiornamento non riu-
029800170410     c                             scito.'
029900170410     c                   eval      D1ERR2 = ' sped.EEX girata su DPD... non agg-
030000170410     c                             iornabile'
030100170410     c                   else
030200170410      *
030300170410      *  altrimenti emissione errore
030400170410     C                   MOVE      *OFF          $INZD1
030500170410     c                   eval      D1ERR1 = 'Attenzione: aggiornamento non riu-
030600170410     c                             scito.'
030700170410     c                   eval      D1ERR2 = ' Controllare la spedizione.'
030800170410      *
030900170410     c                   end
031000161010      *
031100940207     C                   ENDSR
031200940131     C************************************************************
031300940131     C* OPERAZIONI INIZIALI
031400940131     C************************************************************
031500940131     C     *INZSR        BEGSR
031600940131     C*
031700940127     C* Reperimento parametri
031800940117     C     *ENTRY        PLIST
031900940117     C                   PARM                    KPJBA
032000030115     C*
032100940223     C* Variabili per gestione videate
032200940223     C                   MOVE      'D1'          $GEST
032300940223     C                   MOVE      *OFF          $FINE
032400940223     C                   MOVE      *ON           $INZD1
032500940224     C                   MOVE      *BLANK        $LASTG
032600940224     C                   MOVE      *BLANK        $LASTV
032700940506     C                   Z-ADD     0             $ULKD1            3 0
032800940127     C*
032900940223     C* Variabili appoggio
033000940224     C                   Z-ADD     0             CURR
033100940224     C                   Z-ADD     0             CURC
033200940207     C*
033300940207     C* Valorizzazione campi univoci testate
033400940614     C                   CLEAR                   T1
033500940614     C                   MOVEL     KNSIF         NOMSIF
033600941027     C                   MOVEL     DSPGM         NOMPGM
033700940224     C*
033800940224     C* Inizializzazione/Decodifica prima videata
033900940224     C                   EXSR      INZD1
034000940224     C                   MOVE      *OFF          $INZD1
034100161011      *
034200161011     C* Imposta la UDATE  da TIME
034300161011     C                   TIME                    W0140            14 0
034400161011     C                   MOVE      W0140         datagma           8 0
034500161011     C                   MOVE      datagma       dataeur
034600161011     C                   MOVE      dataeur       dataiso
034700161011     C*
034800161011     C*  Toglie 3 mesi ad oggi.
034900161011     C     dataiso       subdur    3:*m          udate3m
035000940127     C*
035100940117     C                   ENDSR
035200940128     C************************************************************
035300940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
035400940223     C************************************************************
035500940223     C     DEFCAM        BEGSR
035600940223     C*
035700161011     C* klist
035800161011     C     Ktas          KLIST
035900161011     C                   KFLD                    vidaas
036000161011     C                   KFLD                    vidlnp
036100161011     C                   KFLD                    vidnrs
036200161011     C                   KFLD                    vidnsp
036300030728      *
036400940223     C* Variabili per gestione videate
036500940223     C                   MOVE      *BLANK        $GEST             2
036600940223     C                   MOVE      *BLANK        $FINE             1
036700940223     C                   MOVE      *BLANK        $INZD1            1
036800940223     C                   MOVE      *BLANK        $LASTG            2
036900940223     C                   MOVE      *BLANK        $LASTV            2
037000940223     C*
037100940223     C* Variabili appoggio
037200940223     C                   Z-ADD     0             CURR              2 0
037300940223     C                   Z-ADD     0             CURC              2 0
037400940223     C                   MOVE      *ZEROS        WIN              99
037500940224     C                   MOVE      *BLANK        WTBERR            1
037600940223     C*
037700940223     C* Indici
037800940223     C                   Z-ADD     0             X                 3 0
037900940223     C                   Z-ADD     0             Y                 3 0
038000940127     C*
038100940117     C                   ENDSR
038200940128     C************************************************************
038300161011     C*
038400161011     C************************************************************
038500161011     C     *PSSR         BEGSR
038600161011     C*
038700161011     c                   exfmt     NOTinfo
038800161011     C*
038900161011     C                   ENDSR
039000161011     C************************************************************
