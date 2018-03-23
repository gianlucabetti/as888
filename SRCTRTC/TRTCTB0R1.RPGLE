000100970214     H DECEDIT('0,') DATEDIT(*DMY.)
000200940223      *
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
002300100803$001 FTRTCTB0D1 CF   E             WORKSTN
002400940201     F                                     INFDS(DSFMT)
002500100803     Fedmap01l  iF   E           K DISK
002600100803     Fedtab01l  iF   E           K DISK
002700940128     D*----------------------------------------------------*
002800940127     D* Passaggio Parametri
002900940127     D KPJBA         E DS
003000030113      *-------------
003100030113     D* Parametri in ricezione
003200030113     D  TABDS          DS
003300030113     D  XTAOPZ                 1      2
003400030113     D  XTARET                 3      3
003500030113     D  XTAOPR                 4      4
003600030113     D  XTAERR                 5      5
003700100805     D  XTAKEY                 6     20
003800100805$003 D Kcodice                             Like(MAPcodice)
003900100805$003 D Kunbcli                             Like(MAPUNBCLI)
004000100805$003 D Ktmsg                               Like(MAPTIPOMSG)
004100100805$003 D Kvers                               Like(MAPVERSION)
004200100805$003 D Krels                               Like(MAPRELEASE)
004300100805$003 D Kagenz                              Like(MAPAGENCY)
004400100805$003 D Kassoc                              Like(MAPASSOCIA)
004500100805$003 D KObblig                             Like(MAPOBBLIG)
004600100805$003 D KDescr                              Like(MAPDESCR)
004700100805$003 D KDesci                              Like(MAPDESCI)
004800100805$003 D OLDCodice                           Like(MAPcodice)
004900100805$003 D OLDunbcli                           Like(MAPUNBCLI)
005000100805$003 D OLDtmsg                             Like(MAPTIPOMSG)
005100100805$003 D OLDvers                             Like(MAPVERSION)
005200100805$003 D OLDrels                             Like(MAPRELEASE)
005300100805$003 D OLDagenz                            Like(MAPAGENCY)
005400100805$003 D OLDassoc                            Like(MAPASSOCIA)
005500940201     D*-------------
005600030204     D  S_TASTO        S                   like($Tasto)
005700940201     D DSFMT           DS
005800940506     D  $TASTO               369    369
005900940201     D  NRG                  370    370
006000940201     D  NCL                  371    371
006100940201     D*-------------
006200940201     D* posizione cursore
006300940201     D CURSOR          DS
006400940223     D  RIRI                   1      2B 0 INZ
006500940201     D  R$$                    2      2
006600940223     D  COCO                   3      4B 0 INZ
006700940201     D  C$$                    4      4
006800940207     D*-------------
006900940207     D* Reperimento nome PGM
007000940207     D STATUS         SDS           333
007100940207     D  DSPGM            *PROC
007200940225     D*-------------
007300940225     D* COSTANTI
007400940225     D*-------------
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
010300940506     D*-------------
010400941108     D DATA            C                   CONST('0001-01-01')
010500030114     D Annull          C                   CONST('  ANNULLATO  ')
010600030114     D OPz_Imm         C                   CONST('IMMISSIONE')
010700030114     D OPz_Var         C                   CONST('VARIAZIONE')
010800030114     D OPz_Del         C                   CONST(' ANNULLA  ')
010900030114     D OPz_Vis         C                   CONST('VISUALIZZA')
011000940127     C*----------------------------------------------------*
011100940127     C*                MAIN LINE PROGRAM
011200940127     C*----------------------------------------------------*
011300940223     C*
011400940223     C     $FINE         DOWEQ     *OFF
011500940202     C     $GEST         CASEQ     'D1'          GESD1
011600100803     C     $GEST         CASEQ     'S1'          GESS1
011700940117     C                   END
011800940117     C                   END
011900940325     C* fine programma
012000030113     c                   movel     tabds         kpjbu
012100030113     C                   Seton                                        LR
012200100803     C************************************************************
012300100803     C* OPERAZIONI INIZIALI
012400100803     C************************************************************
012500100803     C     *INZSR        BEGSR
012600100803     C*
012700100803     C* Reperimento parametri
012800100803     C     *ENTRY        PLIST
012900100803     C                   PARM                    KPJBA
013000100803     C                   movel     kpjbu         tabds
013100100803      *
013200100803     C* Variabili per gestione videate
013300100806     c                   if        XTAOPZ ='01' or XTAOPZ ='02' or XTAOPZ ='03'
013400100803     C                   MOVE      'D1'          $GEST
013500100803     c                   else
013600100803     C                   MOVE      'S1'          $GEST
013700100803     c                   end
013800100803      *
013900100803     C                   MOVE      *OFF          $FINE
014000100803     C                   MOVE      *ON           $INZD1
014100100803     C                   MOVE      *BLANK        $LASTG
014200100803     C                   MOVE      *BLANK        $LASTV
014300100803     C*
014400100803     C* Variabili appoggio
014500100803     C                   Z-ADD     0             CURR
014600100803     C                   Z-ADD     0             CURC
014700100803     C*
014800100803     C                   ENDSR
014900100803     C************************************************************
015000100803     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
015100100803     C************************************************************
015200100803     C     DEFCAM        BEGSR
015300100803      *
015400100803     C     k_map01       kLIST
015500100803     C                   kfld                    Kunbcli
015600100803     C                   kfld                    Ktmsg
015700100803     C                   kfld                    Kvers
015800100803     C                   kfld                    Krels
015900100803     C                   kfld                    Kagenz
016000100803     C                   kfld                    Kassoc
016100100803     C                   kfld                    Kcodice
016200100803     C*
016300100803     C     k_edtab       kLIST
016400100803     C                   kfld                    tabCOD
016500100803     C                   kfld                    tabKEY
016600100803     C*
016700100803     C* Variabili per gestione videate
016800100803     C                   MOVE      *BLANK        $GEST             2
016900100803     C                   MOVE      *BLANK        $FINE             1
017000100803     C                   MOVE      *BLANK        $INZD1            1
017100100803     C                   MOVE      *BLANK        $LASTG            2
017200100803     C                   MOVE      *BLANK        $LASTV            2
017300100803     C*
017400100803     C* Variabili appoggio
017500100803     C                   Z-ADD     0             CURR              2 0
017600100803     C                   Z-ADD     0             CURC              2 0
017700100803     C                   MOVE      *ZEROS        WIN              99
017800100803     C* Indici
017900100803     C                   Z-ADD     0             X                 3 0
018000100803     C                   Z-ADD     0             Y                 3 0
018100100803     C*
018200100803     C                   ENDSR
018300940131     C************************************************************
018400940510     C* GESTIONE VIDEO RECORD D1
018500940131     C************************************************************
018600940127     C     GESD1         BEGSR
018700030113      *
018800940225     C* inizializzazione videata
018900940223     C     $INZD1        IFEQ      *ON
019000940127     C                   EXSR      INZD1
019100940223     C                   MOVE      *OFF          $INZD1
019200940117     C                   END
019300030113      *
019400940225     C* salvataggio valore formato di provenienza
019500940207     C     $LASTV        IFNE      'D1'
019600940202     C                   MOVE      'D1'          $LASTV
019700940202     C                   END
019800940131     C*
019900030114     c                   movel     Opz_Imm       D1Fun
020000030114     c                   SETOFF                                         99
020100940620     C* Immissione/Modifica/Copia
020200030113     C*               *----------------*
020300940620     C                   EXFMT     D1
020400030113     C*               *----------------*
020500030114     C*
0206000301141    C                   SELECT
020700030114     C* F3=Fine
020800030114     C     $TASTO        WHENEQ    F03
020900030114     C                   EXSR      F03D1
021000030114     C* F12=Ritorno
021100030114     C     $TASTO        WHENEQ    F12
021200030114     C                   EXSR      F12D1
021300030114     C*
0214000301141O   C                   OTHER
021500030114     C*
021600030114     C                   EXSR      CTRD1
021700030113     C*
021800940616     C     *IN99         IFEQ      *OFF
021900100803     C                   MOVE      'S1'          $GEST
022000030114     C                   ENDIF
022100030114     C*
0222000301142-   C                   ENDSL
022300030114     C*
022400940117     C                   ENDSR
022500940117      ************************************************************
022600940207      * INIZIALIZZAZIONE VIDEATA DATI
022700940117      ************************************************************
022800940127     C     INZD1         BEGSR
022900940131     C*
023000940615     C                   CLEAR                   D1
023100100805     C*
023200100805     C* Valorizzazione campi univoci testate
023300100805     C                   MOVEL     KNSIF         NOMSIF
023400100805     C                   MOVEL     DSPGM         NOMPGM
023500100805     C                   MOVEL     KNMUS         NOMDIT
023600940207     C*
023700940224     C* valorizzo la chiave univoca per reperire i dati
023800940224     C* se non immissione
0239001008061   >C     xtaopz        IFEQ      '02'
0240001008061   >C     xtaopz        orEQ      '03'
024100030204      *
024200940315    >C*-------------------------------------------------------------
024300940315    >C* impostare i campi chiave ricevuti
024400940315    >C*-------------------------------------------------------------
024500100803      *
024600100803     C     K_map01       chain(N)  edMAP01l
024700100803     C* riempio la videata
024800100803     C                   IF        %Found(edMAP01l)
024900940224     C                   EXSR      RIED1
0250001008032-   C                   ENDIF
025100100803     C*
025200100803     C* richiamo routine dei ctrl per verificare
025300100803     C*  che non esista già la chiave da inserire o da copiare
025400940614    >C                   EXSR      CTRD1
025500030113      *
025600940224     C                   MOVE      *ALL'0'       IN5098
025700940224     C                   MOVEA     IN5098        *IN(50)
025800030113      *
0259009402241-   C                   ENDIF
026000940224     C*
026100940117     C                   ENDSR
026200940207     C/EJECT
026300100803     C************************************************************
026400100803     C* RIEMPIMENTO VIDEATA  D1
026500100803     C************************************************************
026600100803     C     RIED1         BEGSR
026700100803     C*
026800100803     C                   MOVEL     mapCODICE     D1sgm
026900100803     C                   MOVEL     mapDESCR      D1des
027000100803     C                   MOVEL     mapDESCI      D1desi
027100100804     C                   MOVEL     'N'           D1obbl
027200100804     C                   If        mapOBBLIG = 'M'
027300100804     C                   MOVEL     'S'           D1obbl
027400100804     c                   end
027500100803     C                   MOVEL     mapTIPOMSG    D1TMSG
027600100803     C                   MOVEL     mapVERSION    D1VERS
027700100803     C                   MOVEL     mapRELEASE    D1REL
027800100803     C                   MOVEL     mapAGENCY     D1AGEN
027900100803     C                   MOVEL     mapASSOCIA    D1ASSOC
028000100803     C                   MOVEL     mapUNBCLI     D1unb
028100100803     C*
028200100803     C                   ENDSR
028300100803     C************************************************************
028400100803     C* CONTROLLO VIDEATA
028500100803     C************************************************************
028600100803     C     CTRD1         BEGSR
028700100803     C*
028800100803     C                   SETOFF                                       99
028900100803      *
029000100803     C* non imposto pos. del cursore
029100100803     C                   Z-ADD     0             H1RIGA
029200100803     C                   Z-ADD     0             H1COLO
029300100803     C*
029400100803     C* controllo codice vuoto
029500100803$017 C     D1sgm         IFEQ      *blank
029600100803     C                   SETON                                        5199
029700100803     C                   END
029800100803     C*
029900100803     C* controllo descrizione
030000100803$017 C     D1des         IFEQ      *blank
030100100803     C                   SETON                                        5299
030200100803     C                   END
030300100803     C*
030400100803$017 C     D1desi        IFEQ      *blank
030500100803     C                   SETON                                        5399
030600100803     C                   END
030700100803     C*
030800100803$017 C     D1obbl        IFEQ      *blank
030900100803     C                   SETON                                        5499
031000100803     C                   END
031100100803     C*
031200100803     C*** se già presente
031300100803     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
031400100803     C                   movel     D1UNB         Kunbcli
031500100803     C                   movel     D1TMSG        Ktmsg
031600100803     C                   movel     D1VERS        Kvers
031700100803     C                   movel     D1REL         Krels
031800100803     C                   movel     D1AGEN        Kagenz
031900100803     C                   movel     D1ASSOC       Kassoc
032000100804     C                   movel     D1DES         Kdescr
032100100804     C                   movel     D1DESI        Kdesci
032200100803     C                   movel     d1sgm         Kcodice
032300100804     c                   if        d1obbl = 'S'
032400100804     C                   movel     'M'           Kobblig
032500100804     c                   else
032600100804     C                   movel     'C'           Kobblig
032700100804     c                   end
032800100803     C     K_map01       chain(N)  edMAP01L
032900100806     c                   if        %Found(edMAP01L) and XTAOPZ ='03'
033000100803     C                   SETON                                        5899
033100100803     C                   EndIF
033200100803     C* controllo
033300100803     C**   se sbagliato il codice UNB
033400100803$017 C     D1unb         IFne      *blank
033500100803     c                   eval      tabcod = 'PT'
033600100803     c                   eval      tabkey = d1unb
033700100803     C     k_edtab       chain     edtab01l
033800100803     c                   if        not %Found(edtab01l)
033900100803     C                   SETON                                        5799
034000100803     c                   end
034100100803     C                   ENDIF
034200100803     C*
034300100803     C     *IN99         IFEQ      *ON
034400100803     C                   MOVEA     *IN           WIN
034500100803     C                   MOVE      *ALL'0'       IN5098           49
034600100803     C                   MOVEA     IN5098        *IN(50)
034700100803     C                   WRITE     D1
034800100803     C                   MOVEA     WIN           *IN
034900100803     C                   ENDIF
035000100803     C*
035100100803     C                   ENDSR
035200100803     C/EJECT
035300940309     C************************************************************
035400940309     C* GESTIONE F03 VIDEO D1
035500940309     C************************************************************
035600940309     C     F03D1         BEGSR
035700940309     C*
035800940309     C                   MOVE      *ON           $FINE
035900030113    >C                   MOVE      '1'           xtaret
036000940325     C*
036100940325     C                   ENDSR
036200940309     C************************************************************
036300940309     C* GESTIONE F12 VIDEO D1
036400940309     C************************************************************
036500940309     C     F12D1         BEGSR
036600940309     C*
036700940309     C                   MOVE      *ON           $FINE
036800940309     C*
036900940309     C                   ENDSR
037000100803     C************************************************************
037100100803     C* GESTIONE VIDEO SFL    S1
037200100803     C************************************************************
037300100803     C     GESS1         BEGSR
037400100803      *
037500100804     C                   MOVEL     tabds         KPJBU
037600100804$004 C                   CALL      'TRTCTB0R2'                          99
037700100804     C                   PARM                    KPJBA
037800100805     C                   eval      tabds = KPJBU
037900100804      *  se errore chiude il programma
038000100804     c                   if        *in99
038100100804     C                   MOVE      *ON           $FINE
038200100804     c                   movel     Opz_Imm       DWFun
038300100804     C                   exfmt     alertWIND
038400100804     C                   else
038500100804      *
038600100804     C*  solo se richiesto F12 di ritorno altrimenti si torna sempre al
038700100804      *    chiamante x scegliere
038800100804     c                   if        (XTAOPZ = '01' or XTAOPZ = '03') and
038900100805     c                              XTARET = '2'
039000100804     C                   MOVE      'D1'          $GEST
039100100805     C                   MOVE      *OFF          $FINE
039200100804     C                   else
039300100804     C                   MOVE      *ON           $FINE
039400100804     C                   end
039500100804     C*
039600100804     C                   end
039700100804     C*
039800100803     C                   ENDSR
039900940128     C************************************************************
