000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400940223      *
000500940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000600940620      *  03           PROTECT TUTTI I CAMPI
000700030123      *  09           PROTECT dei campi voce C/E
000800940223      *  21           GENERICO OPERAZIONI I/O
000900940223      *  22           GENERICO ERRORE OPERAZIONI I/O
001000940223      *  30           SFLDSP
001100940223      * N31           SFLCLR
001200940128      *  31           SFLDSPCTL
001300940128      *  32           SFLNXTCHG
001400940128      *  33           SFLEND
001500940128      *  39           OF PRTF
001600940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001700940317    > *  Specificare l'uso dei singoli indicatori
001800940315      *  50 <---> 98  ERRORI SU VIDEO
001900940317    > *  Specificare l'uso dei singoli indicatori
002000940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002100940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002200940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002300940128      *  99           INDIC. GENERALE DI ERRORE
002400940128     F*----------------------------------------------------*
002500100803$001 FTRTCTB0D1 CF   E             WORKSTN
002600940201     F                                     INFDS(DSFMT)
002700100803     Fedmap01l  iF   E           K DISK
002800100803     Fedtab01l  iF   E           K DISK
002900940128     D*----------------------------------------------------*
003000940127     D* Passaggio Parametri
003100940127     D KPJBA         E DS
003200030113      *-------------
003300030113     D* Parametri in ricezione
003400030113     D  TABDS          DS
003500030113     D  XTAOPZ                 1      2
003600030113     D  XTARET                 3      3
003700030113     D  XTAOPR                 4      4
003800030113     D  XTAERR                 5      5
003900100805     D  XTAKEY                 6     20
004000100805$003 D Kcodice                             Like(MAPcodice)
004100100805$003 D Kunbcli                             Like(MAPUNBCLI)
004200100805$003 D Ktmsg                               Like(MAPTIPOMSG)
004300100805$003 D Kvers                               Like(MAPVERSION)
004400100805$003 D Krels                               Like(MAPRELEASE)
004500100805$003 D Kagenz                              Like(MAPAGENCY)
004600100805$003 D Kassoc                              Like(MAPASSOCIA)
004700100805$003 D KObblig                             Like(MAPOBBLIG)
004800100805$003 D KDescr                              Like(MAPDESCR)
004900100805$003 D KDesci                              Like(MAPDESCI)
005000100805$003 D OLDCodice                           Like(MAPcodice)
005100100805$003 D OLDunbcli                           Like(MAPUNBCLI)
005200100805$003 D OLDtmsg                             Like(MAPTIPOMSG)
005300100805$003 D OLDvers                             Like(MAPVERSION)
005400100805$003 D OLDrels                             Like(MAPRELEASE)
005500100805$003 D OLDagenz                            Like(MAPAGENCY)
005600100805$003 D OLDassoc                            Like(MAPASSOCIA)
005700940201     D*-------------
005800030204     D  S_TASTO        S                   like($Tasto)
005900940201     D DSFMT           DS
006000940506     D  $TASTO               369    369
006100940201     D  NRG                  370    370
006200940201     D  NCL                  371    371
006300940201     D*-------------
006400940201     D* posizione cursore
006500940201     D CURSOR          DS
006600940223     D  RIRI                   1      2B 0 INZ
006700940201     D  R$$                    2      2
006800940223     D  COCO                   3      4B 0 INZ
006900940201     D  C$$                    4      4
007000940207     D*-------------
007100940207     D* Reperimento nome PGM
007200940207     D STATUS         SDS           333
007300940207     D  DSPGM            *PROC
007400940225     D*-------------
007500940225     D* COSTANTI
007600940225     D*-------------
007700940506     D* Tasti di funzione
007800940506     D F01             C                   CONST(X'31')
007900940506     D F02             C                   CONST(X'32')
008000940506     D F03             C                   CONST(X'33')
008100940506     D F04             C                   CONST(X'34')
008200940506     D F05             C                   CONST(X'35')
008300940506     D F06             C                   CONST(X'36')
008400940506     D F07             C                   CONST(X'37')
008500940506     D F08             C                   CONST(X'38')
008600940506     D F09             C                   CONST(X'39')
008700940506     D F10             C                   CONST(X'3A')
008800940506     D F11             C                   CONST(X'3B')
008900940506     D F12             C                   CONST(X'3C')
009000940506     D F13             C                   CONST(X'B1')
009100940506     D F14             C                   CONST(X'B2')
009200940506     D F15             C                   CONST(X'B3')
009300940506     D F16             C                   CONST(X'B4')
009400940506     D F17             C                   CONST(X'B5')
009500940506     D F18             C                   CONST(X'B6')
009600940506     D F19             C                   CONST(X'B7')
009700940506     D F20             C                   CONST(X'B8')
009800940506     D F21             C                   CONST(X'B9')
009900940506     D F22             C                   CONST(X'BA')
010000940506     D F23             C                   CONST(X'BB')
010100940506     D F24             C                   CONST(X'BC')
010200940506     D ENTER           C                   CONST(X'F1')
010300940506     D ROLDWN          C                   CONST(X'F4')
010400940506     D ROLLUP          C                   CONST(X'F5')
010500940506     D*-------------
010600941108     D DATA            C                   CONST('0001-01-01')
010700030114     D Annull          C                   CONST('  ANNULLATO  ')
010800030114     D OPz_Imm         C                   CONST('IMMISSIONE')
010900030114     D OPz_Var         C                   CONST('VARIAZIONE')
011000030114     D OPz_Del         C                   CONST(' ANNULLA  ')
011100030114     D OPz_Vis         C                   CONST('VISUALIZZA')
011200940127     C*----------------------------------------------------*
011300940127     C*                MAIN LINE PROGRAM
011400940127     C*----------------------------------------------------*
011500940223     C*
011600940223     C     $FINE         DOWEQ     *OFF
011700940202     C     $GEST         CASEQ     'D1'          GESD1
011800100803     C     $GEST         CASEQ     'S1'          GESS1
011900940117     C                   END
012000940117     C                   END
012100940325     C* fine programma
012200030113     c                   movel     tabds         kpjbu
012300030113     C                   Seton                                        LR
012400100803     C************************************************************
012500100803     C* OPERAZIONI INIZIALI
012600100803     C************************************************************
012700100803     C     *INZSR        BEGSR
012800100803     C*
012900100803     C* Reperimento parametri
013000100803     C     *ENTRY        PLIST
013100100803     C                   PARM                    KPJBA
013200100803     C                   movel     kpjbu         tabds
013300100803      *
013400100803     C* Variabili per gestione videate
013500100806     c                   if        XTAOPZ ='01' or XTAOPZ ='02' or XTAOPZ ='03'
013600100803     C                   MOVE      'D1'          $GEST
013700100803     c                   else
013800100803     C                   MOVE      'S1'          $GEST
013900100803     c                   end
014000100803      *
014100100803     C                   MOVE      *OFF          $FINE
014200100803     C                   MOVE      *ON           $INZD1
014300100803     C                   MOVE      *BLANK        $LASTG
014400100803     C                   MOVE      *BLANK        $LASTV
014500100803     C*
014600100803     C* Variabili appoggio
014700100803     C                   Z-ADD     0             CURR
014800100803     C                   Z-ADD     0             CURC
014900100803     C*
015000100803     C                   ENDSR
015100100803     C************************************************************
015200100803     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
015300100803     C************************************************************
015400100803     C     DEFCAM        BEGSR
015500100803      *
015600100803     C     k_map01       kLIST
015700100803     C                   kfld                    Kunbcli
015800100803     C                   kfld                    Ktmsg
015900100803     C                   kfld                    Kvers
016000100803     C                   kfld                    Krels
016100100803     C                   kfld                    Kagenz
016200100803     C                   kfld                    Kassoc
016300100803     C                   kfld                    Kcodice
016400100803     C*
016500100803     C     k_edtab       kLIST
016600100803     C                   kfld                    tabCOD
016700100803     C                   kfld                    tabKEY
016800100803     C*
016900100803     C* Variabili per gestione videate
017000100803     C                   MOVE      *BLANK        $GEST             2
017100100803     C                   MOVE      *BLANK        $FINE             1
017200100803     C                   MOVE      *BLANK        $INZD1            1
017300100803     C                   MOVE      *BLANK        $LASTG            2
017400100803     C                   MOVE      *BLANK        $LASTV            2
017500100803     C*
017600100803     C* Variabili appoggio
017700100803     C                   Z-ADD     0             CURR              2 0
017800100803     C                   Z-ADD     0             CURC              2 0
017900100803     C                   MOVE      *ZEROS        WIN              99
018000100803     C* Indici
018100100803     C                   Z-ADD     0             X                 3 0
018200100803     C                   Z-ADD     0             Y                 3 0
018300100803     C*
018400100803     C                   ENDSR
018500940131     C************************************************************
018600940510     C* GESTIONE VIDEO RECORD D1
018700940131     C************************************************************
018800940127     C     GESD1         BEGSR
018900030113      *
019000940225     C* inizializzazione videata
019100940223     C     $INZD1        IFEQ      *ON
019200940127     C                   EXSR      INZD1
019300940223     C                   MOVE      *OFF          $INZD1
019400940117     C                   END
019500030113      *
019600940225     C* salvataggio valore formato di provenienza
019700940207     C     $LASTV        IFNE      'D1'
019800940202     C                   MOVE      'D1'          $LASTV
019900940202     C                   END
020000940131     C*
020100030114     c                   movel     Opz_Imm       D1Fun
020200030114     c                   SETOFF                                         99
020300940620     C* Immissione/Modifica/Copia
020400030113     C*               *----------------*
020500940620     C                   EXFMT     D1
020600030113     C*               *----------------*
020700030114     C*
0208000301141    C                   SELECT
020900030114     C* F3=Fine
021000030114     C     $TASTO        WHENEQ    F03
021100030114     C                   EXSR      F03D1
021200030114     C* F12=Ritorno
021300030114     C     $TASTO        WHENEQ    F12
021400030114     C                   EXSR      F12D1
021500030114     C*
0216000301141O   C                   OTHER
021700030114     C*
021800030114     C                   EXSR      CTRD1
021900030113     C*
022000940616     C     *IN99         IFEQ      *OFF
022100100803     C                   MOVE      'S1'          $GEST
022200030114     C                   ENDIF
022300030114     C*
0224000301142-   C                   ENDSL
022500030114     C*
022600940117     C                   ENDSR
022700940117      ************************************************************
022800940207      * INIZIALIZZAZIONE VIDEATA DATI
022900940117      ************************************************************
023000940127     C     INZD1         BEGSR
023100940131     C*
023200940615     C                   CLEAR                   D1
023300100805     C*
023400100805     C* Valorizzazione campi univoci testate
023500100805     C                   MOVEL     KNSIF         NOMSIF
023600100805     C                   MOVEL     DSPGM         NOMPGM
023700100805     C                   MOVEL     KNMUS         NOMDIT
023800940207     C*
023900940224     C* valorizzo la chiave univoca per reperire i dati
024000940224     C* se non immissione
0241001008061   >C     xtaopz        IFEQ      '02'
0242001008061   >C     xtaopz        orEQ      '03'
024300030204      *
024400940315    >C*-------------------------------------------------------------
024500940315    >C* impostare i campi chiave ricevuti
024600940315    >C*-------------------------------------------------------------
024700100803      *
024800100803     C     K_map01       chain(N)  edMAP01l
024900100803     C* riempio la videata
025000100803     C                   IF        %Found(edMAP01l)
025100940224     C                   EXSR      RIED1
0252001008032-   C                   ENDIF
025300100803     C*
025400100803     C* richiamo routine dei ctrl per verificare
025500100803     C*  che non esista già la chiave da inserire o da copiare
025600940614    >C                   EXSR      CTRD1
025700030113      *
025800940224     C                   MOVE      *ALL'0'       IN5098
025900940224     C                   MOVEA     IN5098        *IN(50)
026000030113      *
0261009402241-   C                   ENDIF
026200940224     C*
026300940117     C                   ENDSR
026400940207     C/EJECT
026500100803     C************************************************************
026600100803     C* RIEMPIMENTO VIDEATA  D1
026700100803     C************************************************************
026800100803     C     RIED1         BEGSR
026900100803     C*
027000100803     C                   MOVEL     mapCODICE     D1sgm
027100100803     C                   MOVEL     mapDESCR      D1des
027200100803     C                   MOVEL     mapDESCI      D1desi
027300100804     C                   MOVEL     'N'           D1obbl
027400100804     C                   If        mapOBBLIG = 'M'
027500100804     C                   MOVEL     'S'           D1obbl
027600100804     c                   end
027700100803     C                   MOVEL     mapTIPOMSG    D1TMSG
027800100803     C                   MOVEL     mapVERSION    D1VERS
027900100803     C                   MOVEL     mapRELEASE    D1REL
028000100803     C                   MOVEL     mapAGENCY     D1AGEN
028100100803     C                   MOVEL     mapASSOCIA    D1ASSOC
028200100803     C                   MOVEL     mapUNBCLI     D1unb
028300100803     C*
028400100803     C                   ENDSR
028500100803     C************************************************************
028600100803     C* CONTROLLO VIDEATA
028700100803     C************************************************************
028800100803     C     CTRD1         BEGSR
028900100803     C*
029000100803     C                   SETOFF                                       99
029100100803      *
029200100803     C* non imposto pos. del cursore
029300100803     C                   Z-ADD     0             H1RIGA
029400100803     C                   Z-ADD     0             H1COLO
029500100803     C*
029600100803     C* controllo codice vuoto
029700100803$017 C     D1sgm         IFEQ      *blank
029800100803     C                   SETON                                        5199
029900100803     C                   END
030000100803     C*
030100100803     C* controllo descrizione
030200100803$017 C     D1des         IFEQ      *blank
030300100803     C                   SETON                                        5299
030400100803     C                   END
030500100803     C*
030600100803$017 C     D1desi        IFEQ      *blank
030700100803     C                   SETON                                        5399
030800100803     C                   END
030900100803     C*
031000100803$017 C     D1obbl        IFEQ      *blank
031100100803     C                   SETON                                        5499
031200100803     C                   END
031300100803     C*
031400100803     C*** se già presente
031500100803     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
031600100803     C                   movel     D1UNB         Kunbcli
031700100803     C                   movel     D1TMSG        Ktmsg
031800100803     C                   movel     D1VERS        Kvers
031900100803     C                   movel     D1REL         Krels
032000100803     C                   movel     D1AGEN        Kagenz
032100100803     C                   movel     D1ASSOC       Kassoc
032200100804     C                   movel     D1DES         Kdescr
032300100804     C                   movel     D1DESI        Kdesci
032400100803     C                   movel     d1sgm         Kcodice
032500100804     c                   if        d1obbl = 'S'
032600100804     C                   movel     'M'           Kobblig
032700100804     c                   else
032800100804     C                   movel     'C'           Kobblig
032900100804     c                   end
033000100803     C     K_map01       chain(N)  edMAP01L
033100100806     c                   if        %Found(edMAP01L) and XTAOPZ ='03'
033200100803     C                   SETON                                        5899
033300100803     C                   EndIF
033400100803     C* controllo
033500100803     C**   se sbagliato il codice UNB
033600100803$017 C     D1unb         IFne      *blank
033700100803     c                   eval      tabcod = 'PT'
033800100803     c                   eval      tabkey = d1unb
033900100803     C     k_edtab       chain     edtab01l
034000100803     c                   if        not %Found(edtab01l)
034100100803     C                   SETON                                        5799
034200100803     c                   end
034300100803     C                   ENDIF
034400100803     C*
034500100803     C     *IN99         IFEQ      *ON
034600100803     C                   MOVEA     *IN           WIN
034700100803     C                   MOVE      *ALL'0'       IN5098           49
034800100803     C                   MOVEA     IN5098        *IN(50)
034900100803     C                   WRITE     D1
035000100803     C                   MOVEA     WIN           *IN
035100100803     C                   ENDIF
035200100803     C*
035300100803     C                   ENDSR
035400100803     C/EJECT
035500940309     C************************************************************
035600940309     C* GESTIONE F03 VIDEO D1
035700940309     C************************************************************
035800940309     C     F03D1         BEGSR
035900940309     C*
036000940309     C                   MOVE      *ON           $FINE
036100030113    >C                   MOVE      '1'           xtaret
036200940325     C*
036300940325     C                   ENDSR
036400940309     C************************************************************
036500940309     C* GESTIONE F12 VIDEO D1
036600940309     C************************************************************
036700940309     C     F12D1         BEGSR
036800940309     C*
036900940309     C                   MOVE      *ON           $FINE
037000940309     C*
037100940309     C                   ENDSR
037200100803     C************************************************************
037300100803     C* GESTIONE VIDEO SFL    S1
037400100803     C************************************************************
037500100803     C     GESS1         BEGSR
037600100803      *
037700100804     C                   MOVEL     tabds         KPJBU
037800100804$004 C                   CALL      'TRTCTB0R2'                          99
037900100804     C                   PARM                    KPJBA
038000100805     C                   eval      tabds = KPJBU
038100100804      *  se errore chiude il programma
038200100804     c                   if        *in99
038300100804     C                   MOVE      *ON           $FINE
038400100804     c                   movel     Opz_Imm       DWFun
038500100804     C                   exfmt     alertWIND
038600100804     C                   else
038700100804      *
038800100804     C*  solo se richiesto F12 di ritorno altrimenti si torna sempre al
038900100804      *    chiamante x scegliere
039000100804     c                   if        (XTAOPZ = '01' or XTAOPZ = '03') and
039100100805     c                              XTARET = '2'
039200100804     C                   MOVE      'D1'          $GEST
039300100805     C                   MOVE      *OFF          $FINE
039400100804     C                   else
039500100804     C                   MOVE      *ON           $FINE
039600100804     C                   end
039700100804     C*
039800100804     C                   end
039900100804     C*
040000100803     C                   ENDSR
040100940128     C************************************************************
