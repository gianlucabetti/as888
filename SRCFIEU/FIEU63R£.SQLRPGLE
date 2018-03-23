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
002900161010     D tita400f      E DS
003000940201     D*-------------
003100030204     D  S_TASTO        S                   like($Tasto)
003200940201     D DSFMT           DS
003300940506     D  $TASTO               369    369
003400940201     D  NRG                  370    370
003500940201     D  NCL                  371    371
003600940201     D*-------------
003700940201     D* posizione cursore
003800940201     D CURSOR          DS
003900940223     D  RIRI                   1      2B 0 INZ
004000940201     D  R$$                    2      2
004100940223     D  COCO                   3      4B 0 INZ
004200940201     D  C$$                    4      4
004300161010     D*
004400161010     D fieu18r1DS    e DS
004500161010     D*-------------
004600161011     D dataeur         s               d   datfmt(*eur)
004700161011     D dataiso         s               d   datfmt(*iso)
004800161011     D udate3m         s               d   datfmt(*iso)                         3 mesi fa
004900940207     D*-------------
005000940207     D* Reperimento nome PGM
005100940207     D STATUS         SDS           333
005200940207     D  DSPGM            *PROC
005300940225     D*-------------
005400940225     D* COSTANTI
005500940225     D*-------------
005600940506     D* Tasti di funzione
005700940506     D F01             C                   CONST(X'31')
005800940506     D F02             C                   CONST(X'32')
005900940506     D F03             C                   CONST(X'33')
006000940506     D F04             C                   CONST(X'34')
006100940506     D F05             C                   CONST(X'35')
006200940506     D F06             C                   CONST(X'36')
006300940506     D F07             C                   CONST(X'37')
006400940506     D F08             C                   CONST(X'38')
006500940506     D F09             C                   CONST(X'39')
006600940506     D F10             C                   CONST(X'3A')
006700940506     D F11             C                   CONST(X'3B')
006800940506     D F12             C                   CONST(X'3C')
006900940506     D F13             C                   CONST(X'B1')
007000940506     D F14             C                   CONST(X'B2')
007100940506     D F15             C                   CONST(X'B3')
007200940506     D F16             C                   CONST(X'B4')
007300940506     D F17             C                   CONST(X'B5')
007400940506     D F18             C                   CONST(X'B6')
007500940506     D F19             C                   CONST(X'B7')
007600940506     D F20             C                   CONST(X'B8')
007700940506     D F21             C                   CONST(X'B9')
007800940506     D F22             C                   CONST(X'BA')
007900940506     D F23             C                   CONST(X'BB')
008000940506     D F24             C                   CONST(X'BC')
008100940506     D ENTER           C                   CONST(X'F1')
008200940506     D ROLDWN          C                   CONST(X'F4')
008300940506     D ROLLUP          C                   CONST(X'F5')
008400940506     D*-------------
008500941108     D DATA            C                   CONST('0001-01-01')
008600030114     D OPz_Imm         C                   CONST('IMMISSIONE')
008700940127     C*----------------------------------------------------*
008800940127     C*                MAIN LINE PROGRAM
008900940127     C*----------------------------------------------------*
009000940223     C     $FINE         DOWEQ     *OFF
009100940202     C     $GEST         CASEQ     'D1'          GESD1
009200940117     C                   END
009300940117     C                   END
009400940325     C* fine programma
009500030113     C                   Seton                                        LR
009600940131     C************************************************************
009700940510     C* GESTIONE VIDEO RECORD D1
009800940131     C************************************************************
009900940127     C     GESD1         BEGSR
010000030113      *
010100940225     C* inizializzazione videata
010200940223     C     $INZD1        IFEQ      *ON
010300940127     C                   EXSR      INZD1
010400940223     C                   MOVE      *OFF          $INZD1
010500940117     C                   END
010600030113      *
010700940225     C* emissione piede videata se proveniente da altra
010800940225     C* salvataggio valore formato di provenienza
010900940207     C     $LASTV        IFNE      'D1'
011000940614     C                   WRITE     T1
011100940202     C                   MOVE      $LASTV        $LASTG
011200940202     C                   MOVE      'D1'          $LASTV
011300940202     C                   END
011400940131     C*
011500030728     c     video1        tag
011600030114     c                   SETOFF                                         99
011700940620     C* Immissione/Modifica/Copia
011800030113     C*               *----------------*
011900940620     C                   EXFMT     D1
012000030113     C*               *----------------*
012100030114     C     *IN99         IFEQ      *OFF
012200030114     C*
0123000301141    C                   SELECT
012400030114     C* F3=Fine
012500030114     C     $TASTO        WHENEQ    F03
012600030114     C                   EXSR      F03D1
012700030114     C*
0128000301141O   C                   OTHER
012900030114     C*
013000030114     C                   EXSR      CTRD1
013100030728     c   99              goto      video1
013200030113     C*
013300940616     C     *IN99         IFEQ      *OFF
013400940616     C* F6
0135000302052    C     $TASTO        ifeq      F06
013600940224     C* eseguo aggiornamento
013700161010    >C                   EXSR      ESEGUE
013800030205     c                   end
013900030114     C*
014000030114     C                   ENDIF
014100030114     C*
0142000301142-   C                   ENDSL
014300030114     C*
0144000301142-   C                   ENDIF
014500940224     C*
014600940117     C                   ENDSR
014700940117      ************************************************************
014800940207      * INIZIALIZZAZIONE VIDEATA DATI
014900940117      ************************************************************
015000940127     C     INZD1         BEGSR
015100940131     C*
015200940615     C                   CLEAR                   D1
015300940224     C*
015400940117     C                   ENDSR
015500940309     C************************************************************
015600940309     C* GESTIONE F03 VIDEO D1
015700940309     C************************************************************
015800940309     C     F03D1         BEGSR
015900940309     C*
016000940309     C                   MOVE      *ON           $FINE
016100940325     C*
016200940325     C                   ENDSR
016300940131     C************************************************************
016400940207     C* CONTROLLO VIDEATA
016500940131     C************************************************************
016600940127     C     CTRD1         BEGSR
016700940131     C*
016800940127     C                   SETOFF                                       99
016900030123      *
017000940127     C* non imposto pos. del cursore
017100940127     C                   Z-ADD     0             H1RIGA
017200940127     C                   Z-ADD     0             H1COLO
017300030728     c*
017400030114     C* controllo codice vuoto
017500161010$017 C     vidprc        IFEQ      *zeros
017600161010     C                   SETON                                        5199
017700161010     C                   goto      err1
017800161010     C                   Else
017900161010      **
018000161010     c                   clear                   trovato           1
018100161011     c                   clear                   viddat
018200161010     c                   movel     vidprc        alfprc           14
018300161010      *
018400161010     C/EXEC SQL
018500161010     C+   SELECT 'S', ta4aas, ta4lnp, ta4nrs, ta4nsp
018600161011     C+   INTO :trovato, :vidaas, :vidlnp, :vidnrs, :vidnsp
018700161010     C+     FROM tita400F WHERE
018800161011     C+   TA4trc='I' and substr(ta4not, 1, 14)=:alfprc
018900161010     c+ FETCH FIRST 1 ROW ONLY
019000161010     C/END-EXEC
019100161010      *
019200161010     c                   if        trovato = *blank
019300161010     C/EXEC SQL
019400161010     C+   SELECT 'S', ta4aas, ta4lnp, ta4nrs, ta4nsp
019500161011     C+   INTO :trovato, :vidaas, :vidlnp, :vidnrs, :vidnsp
019600161010     C+     FROM tita4p0F WHERE
019700161011     C+   TA4trc='I' and substr(ta4not, 1, 14)=:alfprc
019800161010     c+ FETCH FIRST 1 ROW ONLY
019900161010     C/END-EXEC
020000161011     C                   End
020100161011      *
020200161011     c                   if        trovato = 'S'
020300161011     C     Ktas          chain     titas30c
020400161011     c                   if        %Found(titas30c)
020500161011     c                   movel     tasaas        datatas           8 0
020600161011     c                   move      tasmgs        datatas
020700161011     c                   move      datatas       dataiso
020800161011     c                   move      dataiso       dataeur
020900161011     c                   move      dataeur       viddat
021000161011      *** non può essere più vecchia di 3 mesi
021100161011     c                   if        udate3m > dataiso
021200161011     C                   SETON                                        5399
021300161011     C                   goto      err1
021400161010     C                   End
021500170411      ** controlla che non vi sia già un fuori misura inserito sulla bolla
021600170411     c                   if        tasftc ='F' or tastc2='F'
021700170411     C                   SETON                                        5599
021800170411     C                   goto      err1
021900170411     C                   End
022000161011     C                   End
022100161011      *
022200161011     C                   End
022300161010      *
022400161010     C                   Endif
022500030114      *
022600161010$017 C     vidnsp        IFEQ      *zeros
022700161010     C                   SETON                                        5299
022800161010     C                   goto      err1
022900161010     C                   End
023000170117      *
023100170117      *  x NON codificato dare errore
023200170117     c                   if        %subst(%editc(tasKSC:'X'):4:4) = '8888' or
023300170117     c                             %subst(%editc(tasKSC:'X'):4:4) = '9999'
023400170117     C                   SETON                                        5499
023500170117     C                   goto      err1
023600170117     C                   End
023700161010      *
023800161010     c     err1          tag
023900940131     C*
024000940302     C     *IN99         IFEQ      *ON
024100940510     C     $GEST         ANDEQ     'D1'
024200940117     C                   MOVEA     *IN           WIN
024300940128     C                   MOVE      *ALL'0'       IN5098           49
024400940128     C                   MOVEA     IN5098        *IN(50)
024500940614     C                   WRITE     D1
024600940117     C                   MOVEA     WIN           *IN
024700940302     C                   ENDIF
024800940127     C*
024900940117     C                   ENDSR
025000940207     C/EJECT
025100940224     C************************************************************
025200161010    >C*  conferma esecuzione
025300940224     C************************************************************
025400161010    >C     ESEGUE        BEGSR
025500161010
025600161010      ** Fuori Misura
025700161010     c                   clear                   fieu18r1DS
025800161011     c                   eval         I18AAS     = vidaas
025900161010     c                   eval         I18LNP     = vidlnp
026000161010     c                   eval         I18NRS     = vidnrs
026100161010     c                   eval         I18NSP     = vidnsp
026200161010     c                   eval         I18PARCEL  = alfprc
026300161010     c                   eval         I18WEIGHT  = 0
026400161010     c                   eval         I18FCLR    = 'F'
026401170523     c                   eval         I18FOVR    = 'O'
026500161010      **
026600161010     c                   call      'FIEU18R1'
026700161010     c                   parm                    kpjba
026800161010     c                   parm                    fieu18r1ds
026900161010      *
027000170410      *   esito OK ricarica la videata
027100170410     c                   if        O18ESITO = '0'
027200161010     C                   MOVE      *ON           $INZD1
027300170410     c                   elseif    O18ESITO = '9'
027400170410     c                   eval      D1ERR1 = 'Attenzione: aggiornamento non riu-
027500170410     c                             scito.'
027600170410     c                   eval      D1ERR2 = ' sped.EEX girata su DPD... non agg-
027700170410     c                             iornabile'
027800170410     c                   else
027900170410      *
028000170410      *  altrimenti emissione errore
028100170410     C                   MOVE      *OFF          $INZD1
028200170410     c                   eval      D1ERR1 = 'Attenzione: aggiornamento non riu-
028300170410     c                             scito.'
028400170410     c                   eval      D1ERR2 = ' Controllare la spedizione.'
028500170410      *
028600170410     c                   end
028700161010      *
028800940207     C                   ENDSR
028900940131     C************************************************************
029000940131     C* OPERAZIONI INIZIALI
029100940131     C************************************************************
029200940131     C     *INZSR        BEGSR
029300940131     C*
029400940127     C* Reperimento parametri
029500940117     C     *ENTRY        PLIST
029600940117     C                   PARM                    KPJBA
029700030115     C*
029800940223     C* Variabili per gestione videate
029900940223     C                   MOVE      'D1'          $GEST
030000940223     C                   MOVE      *OFF          $FINE
030100940223     C                   MOVE      *ON           $INZD1
030200940224     C                   MOVE      *BLANK        $LASTG
030300940224     C                   MOVE      *BLANK        $LASTV
030400940506     C                   Z-ADD     0             $ULKD1            3 0
030500940127     C*
030600940223     C* Variabili appoggio
030700940224     C                   Z-ADD     0             CURR
030800940224     C                   Z-ADD     0             CURC
030900940207     C*
031000940207     C* Valorizzazione campi univoci testate
031100940614     C                   CLEAR                   T1
031200940614     C                   MOVEL     KNSIF         NOMSIF
031300941027     C                   MOVEL     DSPGM         NOMPGM
031400940224     C*
031500940224     C* Inizializzazione/Decodifica prima videata
031600940224     C                   EXSR      INZD1
031700940224     C                   MOVE      *OFF          $INZD1
031800161011      *
031900161011     C* Imposta la UDATE  da TIME
032000161011     C                   TIME                    W0140            14 0
032100161011     C                   MOVE      W0140         datagma           8 0
032200161011     C                   MOVE      datagma       dataeur
032300161011     C                   MOVE      dataeur       dataiso
032400161011     C*
032500161011     C*  Toglie 3 mesi ad oggi.
032600161011     C     dataiso       subdur    3:*m          udate3m
032700940127     C*
032800940117     C                   ENDSR
032900940128     C************************************************************
033000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
033100940223     C************************************************************
033200940223     C     DEFCAM        BEGSR
033300940223     C*
033400161011     C* klist
033500161011     C     Ktas          KLIST
033600161011     C                   KFLD                    vidaas
033700161011     C                   KFLD                    vidlnp
033800161011     C                   KFLD                    vidnrs
033900161011     C                   KFLD                    vidnsp
034000030728      *
034100940223     C* Variabili per gestione videate
034200940223     C                   MOVE      *BLANK        $GEST             2
034300940223     C                   MOVE      *BLANK        $FINE             1
034400940223     C                   MOVE      *BLANK        $INZD1            1
034500940223     C                   MOVE      *BLANK        $LASTG            2
034600940223     C                   MOVE      *BLANK        $LASTV            2
034700940223     C*
034800940223     C* Variabili appoggio
034900940223     C                   Z-ADD     0             CURR              2 0
035000940223     C                   Z-ADD     0             CURC              2 0
035100940223     C                   MOVE      *ZEROS        WIN              99
035200940224     C                   MOVE      *BLANK        WTBERR            1
035300940223     C*
035400940223     C* Indici
035500940223     C                   Z-ADD     0             X                 3 0
035600940223     C                   Z-ADD     0             Y                 3 0
035700940127     C*
035800940117     C                   ENDSR
035900940128     C************************************************************
036000161011     C*
036100161011     C************************************************************
036200161011     C     *PSSR         BEGSR
036300161011     C*
036400161011     c                   exfmt     NOTinfo
036500161011     C*
036600161011     C                   ENDSR
036700161011     C************************************************************
