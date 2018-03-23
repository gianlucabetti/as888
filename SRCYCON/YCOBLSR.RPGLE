000100010402     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) BNDDIR(PJXBND)
000200010611     H*PARMS ACTGRP(QILE) CVTOPT(*NONE)
000300010322      *****************************************************************
000400140916     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO)
000401140916     H ACTGRP('QILE') BNDDIR('PJXBND')
000500010322      *****************************************************************
000600010322      *
000700140916      *            AUTORIZZAZIONI
000800010322      *                   Messaggi precodificati
000900010330      *                   Work flow
001000010322      *                   Legami msg/lista distribuz.
001100010322      *
001200010322      *****************************************************************
001300010322      * DOCUMENTI MICROANALISI :
001400010322      *
001500010322      * NOTE TECNICHE :
001600010322      *
001700010322      *
001800010322      * PERSONALIZZAZIONI
001900010322      *
002000010322      *
002100010322      *
002200010322      *               MAPPA INDICATORI STD
002300010322      *
002400010322      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002500010322      *  21           GENERICO OPERAZIONI I/O
002600010322      *  22           GENERICO ERRORE OPERAZIONI I/O
002700010322      *  30           SFLDSP
002800010322      * N31           SFLCLR
002900010322      *  31           SFLDSPCTL
003000010322      *  32           SFLNXTCHG
003100010322      *  33           SFLEND
003200010322      *  39           OF PRTF
003300010322      *  40 <---> 49  DSPATR ERRORI SU SFL
003400010322      *  Specificare l'uso dei singoli indicatori
003500010322      *  50 <---> 98  ERRORI SU VIDEO
003600010322      *  Specificare l'uso dei singoli indicatori
003700010322      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003800010322      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003900010322      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
004000010322      *  99           INDIC. GENERALE DI ERRORE
004100010322      *****************************************************************
004200140916     FycoblsD   CF   E             WORKSTN
004300140916     F                                     RENAME(ycoblsD1:FMTD1)
004400140916     F                                     RENAME(ycoblsT1:FMTT1)
004500140916     F                                     RENAME(ycoblsZ1:FMTZ1)
004600010322     F* eventuali schermi supplementari
004700140916     F                                     RENAME(ycoblsD2:FMTD2)
004800140916     F                                     RENAME(ycoblsZ2:FMTZ2)
004900010322     F                                     INFDS(DSFMT)
005000010322      *****************************************************************
005100010322     D* Schiere per la gestione dei tasti di funzione
005200010322     D $FM             S             27    DIM(24)
005300010322     D $FX             S             50    DIM(24)
005400010322     D $FC             S              1    DIM(24)
005500010322     D $FR             S              1    DIM(24)
005600010322     D* Video D1
005700010322     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005800010322     D $FX1            S             50    DIM(24) ALT($FM1)
005900010322     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006000010322     D $FR1            S              1    DIM(24) ALT($FC1)
006100010322     D*----------------------------------------------------*
006200010322     D* Passaggio Parametri
006300010322     D KPJBA         E DS
006400010322     D*-------------
006500010322     D* DS Esterna gestione messaggi per COMIT
006600010322     D XMSGDS        E DS
006700010322     D  STS          E                     EXTFLD(MSGSTS)
006800010322     D                                     DIM(333)
006900010322     D  JBA          E                     EXTFLD(MSGJBA)
007000010322     D                                     DIM(502)
007100010322     D*-------------
007200010322     D* Passaggio Parametri al PGM di manutenzione
007300140916     D ycoblsDS      E DS
007400010322     D*-------------
007500010322     D* Parametri
007600010322     D PAR755        E DS                  EXTNAME(ANG755DS)
007700010322     D  RET755       E                     EXTFLD(FNN755)
007800010322     D*-------------
007900010322     D DSFMT           DS
008000010322     D  $TASTO               369    369
008100010322     D  NRG                  370    370
008200010322     D  NCL                  371    371
008300010322     D*-------------
008400010322     D* posizione cursore
008500010322     D CURSOR          DS
008600010322     D  RIRI                   1      2B 0 INZ
008700010322     D  R$$                    2      2
008800010322     D  COCO                   3      4B 0 INZ
008900010322     D  C$$                    4      4
009000010322     D*-------------
009100010322     D* Reperimento nome PGM
009200010322     D STATUS         SDS           333
009300010322     D  DSPGM            *PROC
009400010322     D*-------------
009500010322     D* Costanti
009600010322     D* data a 0
009700010322     D $DT0            C                   CONST('0001-01-01')
009800010322     D*-------------
009900010322     D* Tasti di funzione
010000010322     D F01             C                   CONST(X'31')
010100010322     D F02             C                   CONST(X'32')
010200010322     D F03             C                   CONST(X'33')
010300010322     D F04             C                   CONST(X'34')
010400010322     D F05             C                   CONST(X'35')
010500010322     D F06             C                   CONST(X'36')
010600010322     D F07             C                   CONST(X'37')
010700010322     D F08             C                   CONST(X'38')
010800010322     D F09             C                   CONST(X'39')
010900010322     D F10             C                   CONST(X'3A')
011000010322     D F11             C                   CONST(X'3B')
011100010322     D F12             C                   CONST(X'3C')
011200010322     D F13             C                   CONST(X'B1')
011300010322     D F14             C                   CONST(X'B2')
011400010322     D F15             C                   CONST(X'B3')
011500010322     D F16             C                   CONST(X'B4')
011600010322     D F17             C                   CONST(X'B5')
011700010322     D F18             C                   CONST(X'B6')
011800010322     D F19             C                   CONST(X'B7')
011900010322     D F20             C                   CONST(X'B8')
012000010322     D F21             C                   CONST(X'B9')
012100010322     D F22             C                   CONST(X'BA')
012200010322     D F23             C                   CONST(X'BB')
012300010322     D F24             C                   CONST(X'BC')
012400010322     D ENTER           C                   CONST(X'F1')
012500010322     D ROLDWN          C                   CONST(X'F4')
012600010322     D ROLLUP          C                   CONST(X'F5')
012700010322     D*-------------
012800010322     D* Dati di ambiente ottenuti da XSOC
012900010322     D SOC001        E DS                  EXTNAME(XSOC001DS)
013000010322     D*-------------
013100010322     D* DS Interna per dati di output di XSOC
013200010322     D XSOCDS          DS          1000
013300010322      *****************************************************************
013400010322      *                MAIN LINE PROGRAM
013500010322      *****************************************************************
013600010322     C*
013700010322     C* inizializzazione variabili
013800010322     C                   EXSR      INZVAR
013900010322     C*
014000010322     C     $FINE         DOWEQ     *OFF
014100010322     C     $GEST         CASEQ     'D1'          GESD1
014200010322     C*    $GEST         CASEQ     'D2'          GESD2
014300010322     C                   ENDCS
014400010322     C                   ENDDO
014500010322     C*
014600010322     C* fine programma
014700010322     C                   EXSR      ENDPGM
014800010322     C************************************************************
014900010322     C* RICERCHE
015000010322     C************************************************************
015100010322     C     ENDPGM        BEGSR
015200010322     C*
015300010322     C* passaggio dati al pgm chiamante
015400140916     C                   MOVEL     ycoblsDS      DDS755
015500010322     C*
015600010322     C                   SETON                                            RT
015700010322     C                   RETURN
015800010322     C*
015900010322     C                   ENDSR
016000010322     C************************************************************
016100010322     C* RICERCHE
016200010322     C************************************************************
016300010322     C     SEARCH        BEGSR
016400010322     C*
016500010322     C* determino Riga/Colonna del cursore
016600010322     C                   MOVE      NRG           R$$
016700010322     C                   MOVE      NCL           C$$
016800010322     C                   Z-ADD     RIRI          CURR
016900010322     C                   Z-ADD     COCO          CURC
017000010322     C*
017100010322     C* I campi che al momento dell'F4 risultano protetti non devono
017200010322     C* essere abilitati alla funzione di ricerca
017300010322     C*
017400010322     C                   SELECT
017500010322     C*
017600010322     C     1             WHENNE    1
017700010322     C*
017800010322     C* ricerca non abilitata in questa posizione
017900010322     C                   OTHER
018000010322     C* imposto pos. del cursore
018100010322     C                   Z-ADD     CURR          H1RIGA
018200010322     C                   Z-ADD     CURC          H1COLO
018300010322     C                   SETON                                        9899
018400010322     C*
018500010322     C                   ENDSL
018600010322     C*
018700010322     C                   MOVE      *HIVAL        $LASTV
018800010322     C                   ENDSR
018900010322     C************************************************************
019000010322     C* GESTIONE EMISSIONE TASTI FUNZIONE
019100010322     C************************************************************
019200010322     C     GESPIE        BEGSR
019300010322     C*
019400010322     C                   SELECT
019500010322     C*
019600010322     C     $GEST         WHENEQ    'D1'
019700010322     C     $ULKD1        IFEQ      0
019800010322     C* riempimento opzioni
019900010322     C                   EXSR      F24D1
020000010322     C                   ELSE
020100010322     C                   WRITE     FMTZ1
020200010322     C                   ENDIF
020300010322     C*
020400010322     C     $GEST         WHENEQ    'D2'
020500010322     C     $ULKD1        IFEQ      0
020600010322     C* riempimento opzioni
020700010322     C                   EXSR      F24D2
020800010322     C                   ELSE
020900010322     C                   WRITE     FMTZ1
021000010322     C                   ENDIF
021100010322     C                   ENDSL
021200010322     C*
021300010322     C                   ENDSR
021400010322     C************************************************************
021500010322     C* GESTIONE VIDEO RECORD D1
021600010322     C************************************************************
021700010322     C     GESD1         BEGSR
021800010322     C* inizializzazione videata
021900010322     C     $INZD1        IFEQ      *ON
022000010322     C                   EXSR      INZD1
022100010322     C                   MOVE      *OFF          $INZD1
022200010322     C                   ENDIF
022300010322     C* emissione piede videata se proveniente da altra
022400010322     C* salvataggio valore formato di provenienza
022500010322     C     $LASTV        IFNE      'D1'
022600010322     C                   WRITE     FMTT1
022700010322     C                   EXSR      GESPIE
022800010322     C                   MOVE      $LASTV        $LASTG
022900010322     C                   MOVE      'D1'          $LASTV
023000010322     C                   ENDIF
023100010322     C*
023200010322     C* se esistono errori sulla videata
023300010322     C* emetto la write del formato a indicatori spenti per vedere
023400010322     C* le eventuali decodifiche
023500010322     C     *IN99         IFEQ      *ON
023600010322     C     $GEST         ANDEQ     'D1'
023700010322     C                   MOVEA     *IN           WIN
023800010322     C                   MOVE      *ALL'0'       IN5098           49
023900010322     C                   MOVEA     IN5098        *IN(50)
024000010322     C                   WRITE     FMTD1
024100010322     C                   MOVEA     WIN           *IN
024200010322     C                   ENDIF
024300010322     C*
024400010322     C                   SELECT
024500010322     C* Visualizzazione
024600010322     C     OPZ755        WHENEQ    5
024700010322     C                   WRITE     FMTD1
024800010322     C                   EXFMT     PROTECT
024900010322     C* Immissione/Modifica/Copia
025000010322     C                   OTHER
025100010322     C                   EXFMT     FMTD1
025200010322     C                   ENDSL
025300010322     C* controllo abilitazione tasti funzione
025400010322     C*
025500010322     C                   EXSR      CTRKD1
025600010322     C*
025700010322     C     *IN99         IFEQ      *OFF
025800010322     C*
025900010322     C                   SELECT
026000010322     C* ROLLUP
026100010322    >C*          $TASTO    WHEQ ROLLUP
026200010322    >C*                    MOVE 'D2'      $GEST
026300010322     C* F2=Decodifica
026400010322     C     $TASTO        WHENEQ    F02
026500010322     C                   EXSR      F02D1
026600010322     C* F3=Fine
026700010322     C     $TASTO        WHENEQ    F03
026800010322     C                   EXSR      F03D1
026900010322     C* F4=Ricerca
027000010322     C     $TASTO        WHENEQ    F04
027100010322     C                   EXSR      SEARCH
027200010322     C* F5=Refresh
027300010322     C     $TASTO        WHENEQ    F05
027400010322     C                   EXSR      F05D1
027500010322     C* F8=Successivo
027600010322     C     $TASTO        WHENEQ    F08
027700010322     C                   EXSR      F08D1
027800010322     C* F12=Ritorno
027900010322     C     $TASTO        WHENEQ    F12
028000010322     C                   EXSR      F12D1
028100010322     C* F24=Altri tasti
028200010322     C     $TASTO        WHENEQ    F24
028300010322     C                   EXSR      F24D1
028400010322     C*
028500010322   2XC                   OTHER
028600010322     C                   EXSR      CTRD1
028700010322     C* aggiungere le chiamate ai controlli delle videate supplementari
028800010322    >C***        *IN99     IFEQ *OFF
028900010322    >C***                  EXSR CTRD2
029000010322    >C***        *IN99     IFEQ *ON
029100010322    >C***                  SETON                         96
029200010322    >C***                  ENDIF
029300010322    >C***                  ENDIF
029400010322     C*
029500010322   3BC     $TASTO        IFEQ      F06
029600010322     C     *IN99         ANDEQ     *OFF
029700010322     C* ripasso la stringa contenente i dati modificati al pgm
029800010322     C* chiamante che sarà lui a salvarla nel file
029900010322     C                   EXSR      F06D1
030000010322     C                   ENDIF
030100010322     C*
030200010322     C                   ENDSL
030300010322     C*
030400010322     C                   ENDIF
030500010322     C*
030600010322     C     XGESD1        ENDSR
030700010322     C************************************************************
030800010322     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
030900010322     C************************************************************
031000010322     C     CTRKD1        BEGSR
031100010322     C                   SETOFF                                       99
031200010322     C                   MOVEA     $FC1          $FC
031300010322     C                   EXSR      CTRKEY
031400010322     C     *IN99         IFEQ      *ON
031500010322     C                   SETON                                        97
031600010322     C                   ENDIF
031700010322     C     XCTRKD1       ENDSR
031800010322      ************************************************************
031900010322      * INIZIALIZZAZIONE VIDEATA DATI
032000010322      ************************************************************
032100010322     C     INZD1         BEGSR
032200010322     C*
032300010322     C                   CLEAR                   FMTD1
032400010322     C                   CLEAR                   FMTD2
032500010322     C*
032600010322     C                   Z-ADD     1             $PIED1
032700010322     C*
032800010322     C* se non immissione
032900010322   B1C     OPZ755        IFNE      10
033000010322     C*
033100010322     C* reperisco i valori videata dalla maschera dati
033200010322     C* se non immissione
033300140916     C                   MOVEL     DDS755        ycoblsDS
033400010322     C* riempio la videata
033500010322     C                   EXSR      RIED1
033600010322     C* Aggiungere le routine di riempimento dei record succcessivi
033700010322    >C***                EXSR      RIED2
033800010322     C* richiamo routine dei ctrl per decodificarla
033900010322     C                   EXSR      CTRD1
034000010322     C* Aggiungere le routine di controllo dei record succcessivi
034100010322    >C***                EXSR      CTRD2
034200010322     C                   MOVE      *ALL'0'       IN5098
034300010322     C                   MOVEA     IN5098        *IN(50)
034400010322     C*
034500010322     C* riempio la videata con i valori di default
034600010322   X1C                   ELSE
034700010322     C*
034800010322      * Messaggi precodificati
034900140916     C                   MOVEL     SI            D1DRGMOR
034901140916     C                   MOVEL     SI            D1DRGBLS
035100140916     C                   MOVEL     SI            D1AUTMOR
035200140916     C                   MOVEL     SI            D1AUTDG
035300140916     C                   MOVEL     SI            D1ANNPRP
035500140916     C                   MOVEL     SI            D1EXCPRP
035600140916     C                   MOVEL     SI            D1STAMOR
035700140916     C                   MOVEL     SI            D1EXCBLS
035701141106     C                   MOVEL     SI            D1INSMAN
035702141106     C                   MOVEL     SI            D1MORAMAN
035703141106     C                   MOVEL     SI            D1BLOCMAN
035704150114     C                   MOVEL     SI            D1undo
035705150114     C                   MOVEL     SI            D1revmora
035706150114     C                   MOVEL     SI            D1revbls
035707150114     C*
036400010322   E1C                   ENDIF
036500010322     C*
036600010322     C     XINZD1        ENDSR
036700010322     C************************************************************
036800010322     C* RIEMPIMENTO VIDEATA  D1
036900010322     C************************************************************
037000010322     C     RIED1         BEGSR
037100140916      *
037200140916     C                   if        DRGMOR = *On
037400140916     C                   movel     SI            D1DRGMOR
037500010322     C                   else
037600140916     C                   movel     NO            D1DRGMOR
037700140916     C                   endif
037701140916      *
037702140916     C                   if        DRGBLS = *On
037705140916     C                   movel     SI            D1DRGBLS
037706140916     C                   else
037707140916     C                   movel     NO            D1DRGBLS
037708140916     C                   endif
037709140916      *
037710140916     C                   if        AUTMOR = *On
037711140916     C                   movel     SI            D1AUTMOR
037712140916     C                   else
037713140916     C                   movel     NO            D1AUTMOR
037714140916     C                   endif
037715140916      *
037716140916     C                   if        AUTDG = *On
037717140916     C                   movel     SI            D1AUTDG
037718140916     C                   else
037719140916     C                   movel     NO            D1AUTDG
037720140916     C                   endif
037721140916      *
037722140916     C                   if        ANNPRP = *On
037723140916     C                   movel     SI            D1ANNPRP
037724140916     C                   else
037725140916     C                   movel     NO            D1ANNPRP
037726140916     C                   endif
037727140916      *
037728140916     C                   if        EXCPRP = *On
037729140916     C                   movel     SI            D1EXCPRP
037730140916     C                   else
037731140916     C                   movel     NO            D1EXCPRP
037732140916     C                   endif
037733140916      *
037734140916     C                   if        STAMOR = *On
037735140916     C                   movel     SI            D1STAMOR
037736140916     C                   else
037737140916     C                   movel     NO            D1STAMOR
037738140916     C                   endif
037739140916      *
037740140916     C                   if        EXCBLS = *On
037741140916     C                   movel     SI            D1EXCBLS
037742140916     C                   else
037743140916     C                   movel     NO            D1EXCBLS
037744140916     C                   endif
037745141106      *
037746141106     C                   if        INSMAN = *ON
037747141106     C                   movel     SI            D1INSMAN
037748141106     C                   else
037749141106     C                   movel     NO            D1INSMAN
037750141106     C                   endif
037751141106      *
037752141106     C                   if        MORAMAN = *ON
037753141106     C                   movel     SI            D1MORAMAN
037754141106     C                   else
037755141106     C                   movel     NO            D1MORAMAN
037756141106     C                   endif
037757141106      *
037758141106     C                   if        BLOCMAN = *ON
037759141106     C                   movel     SI            D1BLOCMAN
037760141106     C                   else
037761141106     C                   movel     NO            D1BLOCMAN
037762141106     C                   endif
037763150114      *
037764150114     C                   if        undo = *ON
037765150114     C                   movel     SI            D1undo
037766150114     C                   else
037767150114     C                   movel     NO            D1undo
037768150114     C                   endif
037769150114      *
037770150114     C                   if        revmora = *ON
037771150114     C                   movel     SI            D1revmora
037772150114     C                   else
037773150114     C                   movel     NO            D1revmora
037774150114     C                   endif
037775150114      *
037776150114     C                   if        revbls = *ON
037777150114     C                   movel     SI            D1revbls
037778150114     C                   else
037779150114     C                   movel     NO            D1revbls
037780150114     C                   endif
037781150114     C*
043600010322     C     XRieD1        ENDSR
043700010322     C************************************************************
043800010322     C* GESTIONE F02 VIDEO D1
043900010322     C************************************************************
044000010322     C     F02D1         BEGSR
044100010322     C*
044200010322     C* per decodificare la videata richiamo la routine dei ctrl
044300010322     C* e poi disattivo gli eventuali messaggi di errore
044400010322     C                   EXSR      CTRD1
044500010322     C*
044600010322     C                   MOVE      *ALL'0'       IN5098
044700010322     C                   MOVEA     IN5098        *IN(50)
044800010322     C*
044900010322     C     XF02D1        ENDSR
045000010322     C************************************************************
045100010322     C* GESTIONE F03 VIDEO D1
045200010322     C************************************************************
045300010322     C     F03D1         BEGSR
045400010322     C*
045500010322     C                   MOVE      *ON           $FINE
045600010322     C                   MOVE      'F03'         RET755
045700010322     C* fine programma
045800010322     C                   EXSR      ENDPGM
045900010322     C*
046000010322     C     XF03D1        ENDSR
046100010322     C************************************************************
046200010322     C* GESTIONE F05 VIDEO D1
046300010322     C************************************************************
046400010322     C     F05D1         BEGSR
046500010322     C*
046600010322     C                   MOVE      *ON           $INZD1
046700010322     C*
046800010322     C     XF05D1        ENDSR
046900010322     C************************************************************
047000010322     C* GESTIONE F06 VIDEO D1
047100010322     C************************************************************
047200010322     C     F06D1         BEGSR
047300140916     C*
047301140916     C                   if        D1DRGMOR = SI
047302140916     C                   movel     *On           DRGMOR
047303140916     C                   else
047304140916     C                   movel     *Off          DRGMOR
047305140916     C                   endif
047306140916     C*
047307140916     C                   if        D1DRGBLS = SI
047308140916     C                   movel     *On           DRGBLS
047309140916     C                   else
047310140916     C                   movel     *Off          DRGBLS
047311140916     C                   endif
047312140916     C*
047313140916     C                   if        D1AUTMOR = SI
047314140916     C                   movel     *On           AUTMOR
047315140916     C                   else
047316140916     C                   movel     *Off          AUTMOR
047317140916     C                   endif
047318140916     C*
047319140916     C                   if        D1AUTDG = SI
047320140916     C                   movel     *On           AUTDG
047321140916     C                   else
047322140916     C                   movel     *Off          AUTDG
047323140916     C                   endif
047324140916     C*
047325140916     C                   if        D1ANNPRP = SI
047326140916     C                   movel     *On           ANNPRP
047327140916     C                   else
047328140916     C                   movel     *Off          ANNPRP
047329140916     C                   endif
047330140916     C*
047331140916     C                   if        D1EXCPRP = SI
047332140916     C                   movel     *On           EXCPRP
047333140916     C                   else
047334140916     C                   movel     *Off          EXCPRP
047335140916     C                   endif
047336140916     C*
047337140916     C                   if        D1STAMOR = SI
047338140916     C                   movel     *On           STAMOR
047339140916     C                   else
047340140916     C                   movel     *Off          STAMOR
047341140916     C                   endif
047342140916     C*
047343140916     C                   if        D1EXCBLS = SI
047344140916     C                   movel     *On           EXCBLS
047345140916     C                   else
047346140916     C                   movel     *Off          EXCBLS
047347140916     C                   endif
047348141106     C*
047349141106     C                   if        D1INSMAN = SI
047350141106     C                   movel     *On           INSMAN
047351141106     C                   else
047352141106     C                   movel     *Off          INSMAN
047353141106     C                   endif
047354141106     C*
047355141106     C                   if        D1MORAMAN = SI
047356141106     C                   movel     *On           MORAMAN
047357141106     C                   else
047358141106     C                   movel     *Off          MORAMAN
047359141106     C                   endif
047360141106     C*
047361141106     C                   if        D1BLOCMAN = SI
047362141106     C                   movel     *On           BLOCMAN
047363141106     C                   else
047364141106     C                   movel     *Off          BLOCMAN
047365141106     C                   endif
047366150114     C*
047367150114     C                   if        D1undo = SI
047368150114     C                   movel     *On           undo
047369150114     C                   else
047370150114     C                   movel     *Off          undo
047371150114     C                   endif
047372150114     C*
047373150114     C                   if        D1revmora = SI
047374150114     C                   movel     *On           revmora
047375150114     C                   else
047376150114     C                   movel     *Off          revmora
047377150114     C                   endif
047378150114     C*
047379150114     C                   if        D1revbls = SI
047380150114     C                   movel     *On           revbls
047381150114     C                   else
047382150114     C                   movel     *Off          revbls
047383150114     C                   endif
047384150114     C*
053800010323     C                   MOVE      'F06'         RET755
053900010323     C                   MOVE      *ON           $FINE
054000010322     C*
054100010322     C     XF06D1        ENDSR
054200010322     C************************************************************
054300010322     C* GESTIONE F08 VIDEO D1
054400010322     C************************************************************
054500010322     C     F08D1         BEGSR
054600010322     C*
054700010322     C                   MOVE      *ON           $FINE
054800010322     C                   MOVE      *BLANK        RET755
054900010322     C*
055000010322     C                   ENDSR
055100010322     C************************************************************
055200010322     C* GESTIONE F12 VIDEO D1
055300010322     C************************************************************
055400010322     C     F12D1         BEGSR
055500010322     C*
055600010322     C                   MOVE      *ON           $FINE
055700010322     C                   MOVE      'F12'         RET755
055800010322     C*
055900010322     C                   ENDSR
056000010322     C************************************************************
056100010322     C* GESTIONE F24 VIDEO D1
056200010322     C************************************************************
056300010322     C     F24D1         BEGSR
056400010322     C*
056500010322     C*
056600010322     C                   MOVEA     $FX1          $FX
056700010322     C                   MOVEA     $FC1          $FC
056800010322     C                   MOVEA     $FR1          $FR
056900010322     C                   Z-ADD     $ULKD1        $ULK
057000010322     C                   EXSR      RIEKEY
057100010322     C                   Z-ADD     $ULK          $ULKD1            3 0
057200010322     C                   MOVEA     $FC           $FC1
057300010322     C                   MOVEL     $KEY1         Z1KE1
057400060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
057500010322     C                   MOVEL     $KEY2         Z1KE2
057600010322     C                   WRITE     FMTZ1
057700010322     C*
057800010322     C                   ENDSR
057900010322     C************************************************************
058000010322     C* CONTROLLO VIDEATA
058100010322     C************************************************************
058200010322     C     CTRD1         BEGSR
058300010322     C*
058400010323     C                   eval      *In99 = *Off
058500010322     C* non imposto pos. del cursore
058600010322     C                   Z-ADD     0             H1RIGA
058700010322     C                   Z-ADD     0             H1COLO
058800010323      *
058900010323      * Ctrl validità di tutti i flag
059000010323      *
059100010323      * Messaggi precodificati
059200010323     C                   exsr      CtrlJCMSG
059300140916      *
060000010323     C     XCtrD1        ENDSR
060100010323      ************************************************************
060200010323      * Controllo flag a video Messaggi precodificati
060300010323      ************************************************************
060400010323    >C     CtrlJCMSG     BEGSR
060500010323      *
060600010323      * Messaggi precodificati
060700140916     C*                   if        D1ImmJCMSG<>SI  and  D1ImmJCMSG<>NO
060800140916     C*                   eval      *In51 = *On
060900140916     C*                   eval      *In99 = *On
061000140916     C*                   endif
062300010323      *
062400010323     C     XCtrJCMSG     ENDSR
062500140916     C************************************************************
067400010322     C* GESTIONE VIDEO RECORD  D2
067500010322     C************************************************************
067600010322    >C     GESD2         BEGSR
067700010322     C* emissione piede videata se proveniente da altra
067800010322     C* salvataggio valore formato di provenienza
067900010322     C     $LASTV        IFNE      'D2'
068000010322     C                   WRITE     FMTT1
068100010322     C                   EXSR      GESPIE
068200010322     C                   MOVE      $LASTV        $LASTG
068300010322     C                   MOVE      'D2'          $LASTV
068400010322     C                   ENDIF
068500010322     C*
068600010322     C* se esistono errori sulla videata
068700010322     C* emetto la write del formato a indicatori spenti per vedere
068800010322     C* le eventuali decodifiche
068900010322     C     *IN99         IFEQ      *ON
069000010322     C     $GEST         ANDEQ     'D2'
069100010322     C                   MOVEA     *IN           WIN
069200010322     C                   MOVE      *ALL'0'       IN5098           49
069300010322     C                   MOVEA     IN5098        *IN(50)
069400010322     C                   WRITE     FMTD2
069500010322     C                   MOVEA     WIN           *IN
069600010322     C                   ENDIF
069700010322     C*
069800010322     C                   SELECT
069900010322     C* Visualizzazione
070000010322     C     OPZ755        WHENEQ    5
070100010322     C                   WRITE     FMTD2
070200010322     C                   EXFMT     PROTECT
070300010322     C* Immissione/Modifica/Copia
070400010322     C                   OTHER
070500010322     C                   EXFMT     FMTD2
070600010322     C                   ENDSL
070700010322     C* controllo abilitazione tasti funzione
070800010322     C*
070900010322     C                   EXSR      CTRKD1
071000010322     C*
071100010322     C     *IN99         IFEQ      *OFF
071200010322     C*
071300010322     C                   SELECT
071400010322     C* ROLDOWN
071500010322     C     $TASTO        WHENEQ    ROLDWN
071600010322     C                   MOVE      'D1'          $GEST
071700010322     C* F3=Fine
071800010322     C     $TASTO        WHENEQ    F03
071900010322     C                   EXSR      F03D1
072000010322     C* F4=Ricerca
072100010322     C     $TASTO        WHENEQ    F04
072200010322     C                   EXSR      SEARCH
072300010322     C* F5=Refresh
072400010322     C     $TASTO        WHENEQ    F05
072500010322     C                   EXSR      F05D2
072600010322     C* F8=Successivo
072700010322     C     $TASTO        WHENEQ    F08
072800010322     C                   EXSR      F08D1
072900010322     C* F12=Ritorno
073000010322     C     $TASTO        WHENEQ    F12
073100010322     C                   EXSR      F12D1
073200010322     C* F24=Altri tasti
073300010322     C     $TASTO        WHENEQ    F24
073400010322     C                   EXSR      F24D2
073500010322     C*
073600010322     C                   OTHER
073700010322     C                   EXSR      CTRD2
073800010322     C     *IN99         IFEQ      *OFF
073900010322     C                   EXSR      CTRD1
074000010322     C     *IN99         IFEQ      *ON
074100010322     C                   SETON                                            96
074200010322     C                   ENDIF
074300010322     C                   ENDIF
074400010322     C*
074500010322     C*
074600010322     C     $TASTO        IFEQ      F06
074700010322     C     *IN99         ANDEQ     *OFF
074800010322     C* ripasso la stringa contenente i dati modificati al pgm
074900010322     C* chiamante che sarà lui a salvarla nel file
075000010322     C                   EXSR      F06D1
075100010322     C                   ENDIF
075200010322     C*
075300010322     C                   ENDSL
075400010322     C*
075500010322     C                   ENDIF
075600010322     C*
075700010322    >C                   ENDSR
075800010322     C/EJECT
075900010322     C************************************************************
076000010322     C* RIEMPIMENTO VIDEATA D2
076100010322     C************************************************************
076200010322    >C     RIED2         BEGSR
076300010322     C*
076400010322     C*
076500010322    >C                   ENDSR
076600010322     C************************************************************
076700010322     C* GESTIONE F05 VIDEO D2
076800010322     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
076900010322     C* e il riposizionamento alla prima
077000010322     C************************************************************
077100010322    >C     F05D2         BEGSR
077200010322     C*
077300010322     C                   MOVE      *ON           $INZD1
077400010322     C                   MOVE      'D1'          $GEST
077500010322     C*
077600010322    >C                   ENDSR
077700010322     C************************************************************
077800010322     C* GESTIONE F24 VIDEO D2
077900010322     C************************************************************
078000010322    >C     F24D2         BEGSR
078100010322     C*
078200010322     C                   MOVEA     $FX1          $FX
078300010322     C                   MOVEA     $FC1          $FC
078400010322     C                   MOVEA     $FR1          $FR
078500010322     C                   Z-ADD     $ULKD1        $ULK
078600010322     C                   EXSR      RIEKEY
078700010322     C                   Z-ADD     $ULK          $ULKD1            3 0
078800010322     C                   MOVEA     $FC           $FC1
078900010322     C                   MOVEL     $KEY1         Z1KE1
079000060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
079100010322     C                   MOVEL     $KEY2         Z1KE2
079200010322     C                   WRITE     FMTZ1
079300010322     C*
079400010322    >C                   ENDSR
079500010322     C************************************************************
079600010322     C* CONTROLLO VIDEATA  D2
079700010322     C************************************************************
079800010322    >C     CTRD2         BEGSR
079900010322     C*
080000010322     C                   SETOFF                                       99
080100010322     C* non imposto pos. del cursore
080200010322     C                   Z-ADD     0             H1RIGA
080300010322     C                   Z-ADD     0             H1COLO
080400010322     C*
080500010322     C                   ENDSR
080600010322     C************************************************************
080700010322     C* OPERAZIONI INIZIALI
080800010322     C************************************************************
080900010322     C     *INZSR        BEGSR
081000010322     C*
081100010322     C* Reperimento parametri
081200010322     C*
081300010322     C     *ENTRY        PLIST
081400010323     C                   PARM                    KPJBA
081500010322     C                   PARM                    PAR755
081600010322     C*
081700010322     C* Reperimento tasti di funzione
081800010322     C*
081900010322     C                   EXSR      REPKEY
082000010322     C*
082100010322     C                   ENDSR
082200010322     C************************************************************
082300010322     C* REPERIMENTO TASTI DI FUNZIONE
082400010322     C************************************************************
082500010322     C     REPKEY        BEGSR
082600010322     C                   MOVEA     $FM1          $FM
082700010322     C                   MOVEA     $FC1          $FC
082800010322     C                   EXSR      REPKKK
082900010322     C                   MOVEA     $FX           $FX1
083000010322     C*
083100010322     C                   ENDSR
083200010322     C************************************************************
083300010322     C* REPERIMENTO TASTI DI FUNZIONE
083400010322     C************************************************************
083500010322     C     REPKKK        BEGSR
083600010322     C*
083700010322     C                   CLEAR                   $FX
083800010322     C     1             DO        24            X
083900010322     C*
084000010322     C     $FM(X)        IFNE      *BLANK
084100010322     C     $FC(X)        ANDNE     '0'
084200010322     C*
084300010322     C                   MOVEL     $FM(X)        IDMSG
084400010322     C                   CALLB     'XRTVM'
084500010322     C                   PARM                    IDMSG            27
084600010322     C                   PARM                    TXTMSG          100
084700010322     C                   PARM                    ERRMSG            1
084800010322     C*
084900010322     C     ERRMSG        IFNE      *ON
085000010322     C                   MOVEL     TXTMSG        $FX(X)
085100010322     C                   ELSE
085200010322     C                   MOVEL     *ALL' '       $FX(X)
085300010322     C                   ENDIF
085400010322     C*
085500010322     C                   ENDIF
085600010322     C*
085700010322     C                   ENDDO
085800010322     C*
085900010322     C                   ENDSR
086000010322     C************************************************************
086100010322     C* INIZIALIZZAZIONE VARIABILI
086200010322     C************************************************************
086300010322     C     INZVAR        BEGSR
086400010322     C*
086500010322     C* Reperimento dati società
086600010322     C*
086700010322    >C                   MOVEL     'SOC001'      TIPXSC
086800010322     C                   MOVEL     *BLANK        SOCXSC
086900010322     C                   EXSR      REPSOC
087000010322     C     RTNXSC        IFNE      '1'
087100010322     C                   MOVEL     XSOCDS        SOC001
087200010322     C                   ELSE
087300010322     C                   RETURN
087400010322     C                   ENDIF
087500010322     C*
087600010322     C* Dati da PGM guida
087700010322     C*
087800140916    >C                   MOVEL     KPJBU         ycoblsDS
087900010322     C*
088000010322     C* Definizione dei tasti funzione utilizzabili a pgm
088100010322     C*
088200010322     C                   EXSR      DEFOPZ
088300010322     C*
088400010322     C* Variabili per gestione videate
088500010322     C*
088600010322     C                   MOVE      'D1'          $GEST                          fmt in gestione
088700010322     C                   MOVE      *OFF          $FINE                          fine pgm
088800010322     C                   MOVE      *ON           $INZD1                         inizializzione D1
088900010322     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
089000010322     C                   MOVE      *BLANK        $LASTV                         ultima emissione
089100010322     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
089200010322     C*
089300010322     C* Variabili appoggio
089400010322     C*
089500010322     C                   Z-ADD     0             CURR
089600010322     C                   Z-ADD     0             CURC
089700010322     C                   MOVE      XSCON         SI
089800010322     C                   MOVE      XSCOFF        NO
089900010322     C*
090000010322     C* Valorizzazione campi univoci testate
090100010322     C*
090200010322     C                   MOVEL     KNSIF         NOMSIF
090300010322     C                   MOVEL     XSCDSI        NOMDIT
090400010322     C                   MOVEL     DSPGM         NOMPGM
090500010322     C*
090600010322     C* Centratura decodifica funzione
090700010322     C*
090800010322     C                   Eval      XLenStr = %Size(XFn755)
090900010322     C                   Eval      XLenTot = %Size(XFn755)
091000010322     C                   Call      'X71CENTRA'
091100010322     C                   Parm                    XFn755
091200010322     C                   Parm                    XLenStr           3 0
091300010322     C                   Parm                    XLenTot           3 0
091400010322     C*
091500010322     C* Inizializzazione/Decodifica prima videata
091600010322     C*
091700010322     C                   EXSR      INZD1
091800010322     C                   MOVE      *OFF          $INZD1
091900010322     C*
092000010322     C                   ENDSR
092100010322     C************************************************************
092200010322     C* REPERIMENTO DATI SOCIETA'
092300010322     C************************************************************
092400010322     C     REPSOC        BEGSR
092500010322     C*
092600010322     C                   CALLB     'XSOC'
092700010322     C                   PARM                    TIPXSC            6
092800010322     C                   PARM                    SOCXSC            3
092900010322     C                   PARM                    CDSXSC            9 0
093000010322     C                   PARM                    MODXSC            3
093100010322     C                   PARM      *BLANKS       RTNXSC            1
093200010322     C                   PARM                    XSOCDS
093300010322     C                   PARM                    KPJBA
093400010322     C*
093500010322     C                   ENDSR
093600010322     C************************************************************
093700010322     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
093800010322     C************************************************************
093900010322     C     DEFOPZ        BEGSR
094000010322     C*
094100010322     C* Questa routine deve valorizzare la schiera $FC1/$FR1
094200010322     C* con S o N a seconda della utilizzabilità o meno
094300010322     C* del tasto funzione correlato
094400010322     C*
094500010322     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
094600010322     C* di compilazione
094700010322     C                   RESET                   $FC1
094800010322     C                   RESET                   $FR1
094900010322     C*
095000010322     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
095100010322     C* invalido alcune opzioni
095200010322     C                   SELECT
095300010322     C* Se pgm richiamato in interrogazione disattivo :
095400010322     C     OPZ755        WHENEQ    5
095500010322     C* F4=Ricerca
095600010322     C                   Z-ADD     4             X
095700010322     C                   MOVE      '0'           $FC1(X)
095800010322     C* F5=Ripristino
095900010322     C                   Z-ADD     5             X
096000010322     C                   MOVE      '0'           $FC1(X)
096100010322     C* F6=Conferma
096200010322     C                   Z-ADD     6             X
096300010322     C                   MOVE      '0'           $FC1(X)
096400010322     C*
096500010322     C                   OTHER
096600010322     C*
096700010322     C                   ENDSL
096800010322     C*
096900010322     C                   ENDSR
097000010322     C************************************************************
097100010322     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
097200010322     C************************************************************
097300010322     C     DEFCAM        BEGSR
097400010322     C*
097500010322     C* klist
097600010322     C*
097700010322     C*
097800010322     C* Variabili per gestione videate
097900010322     C*
098000010322     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
098100010322     C                   MOVE      *BLANK        $FINE             1            fine pgm
098200010322     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
098300010322     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
098400010322     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
098500010322     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
098600010322     C*
098700010322     C* Variabili appoggio
098800010322     C*
098900010322     C                   Z-ADD     0             CURR              2 0
099000010322     C                   Z-ADD     0             CURC              2 0
099100010322     C                   MOVE      *ZEROS        WIN              99
099200010322     C                   MOVE      *BLANK        SI                1
099300010322     C                   MOVE      *BLANK        NO                1
099400010322     C*
099500010322     C* Indici
099600010322     C*
099700010322     C                   Z-ADD     0             X                 3 0
099800010322     C                   Z-ADD     0             Y                 3 0
099900010322     C*
100000010322     C                   ENDSR
100100010322     C************************************************************
100200010322     C/EJECT
100300010322     C************************************************************
100400010322     C* CONTROLLO ABILITAZIONE TASTI
100500010322     C************************************************************
100600010322     C     CTRKEY        BEGSR
100700010322     C                   SELECT
100800010322     C     $TASTO        WHENEQ    F01
100900010322     C     $FC(1)        IFEQ      '0'
101000010322     C                   SETON                                        99
101100010322     C                   ENDIF
101200010322     C     $TASTO        WHENEQ    F02
101300010322     C     $FC(2)        IFEQ      '0'
101400010322     C                   SETON                                        99
101500010322     C                   ENDIF
101600010322     C     $TASTO        WHENEQ    F03
101700010322     C     $FC(3)        IFEQ      '0'
101800010322     C                   SETON                                        99
101900010322     C                   ENDIF
102000010322     C     $TASTO        WHENEQ    F04
102100010322     C     $FC(4)        IFEQ      '0'
102200010322     C                   SETON                                        99
102300010322     C                   ENDIF
102400010322     C     $TASTO        WHENEQ    F05
102500010322     C     $FC(5)        IFEQ      '0'
102600010322     C                   SETON                                        99
102700010322     C                   ENDIF
102800010322     C     $TASTO        WHENEQ    F06
102900010322     C     $FC(6)        IFEQ      '0'
103000010322     C                   SETON                                        99
103100010322     C                   ENDIF
103200010322     C     $TASTO        WHENEQ    F07
103300010322     C     $FC(7)        IFEQ      '0'
103400010322     C                   SETON                                        99
103500010322     C                   ENDIF
103600010322     C     $TASTO        WHENEQ    F08
103700010322     C     $FC(8)        IFEQ      '0'
103800010322     C                   SETON                                        99
103900010322     C                   ENDIF
104000010322     C     $TASTO        WHENEQ    F09
104100010322     C     $FC(09)       IFEQ      '0'
104200010322     C                   SETON                                        99
104300010322     C                   ENDIF
104400010322     C     $TASTO        WHENEQ    F10
104500010322     C     $FC(10)       IFEQ      '0'
104600010322     C                   SETON                                        99
104700010322     C                   ENDIF
104800010322     C     $TASTO        WHENEQ    F11
104900010322     C     $FC(11)       IFEQ      '0'
105000010322     C                   SETON                                        99
105100010322     C                   ENDIF
105200010322     C     $TASTO        WHENEQ    F12
105300010322     C     $FC(12)       IFEQ      '0'
105400010322     C                   SETON                                        99
105500010322     C                   ENDIF
105600010322     C     $TASTO        WHENEQ    F13
105700010322     C     $FC(13)       IFEQ      '0'
105800010322     C                   SETON                                        99
105900010322     C                   ENDIF
106000010322     C     $TASTO        WHENEQ    F14
106100010322     C     $FC(14)       IFEQ      '0'
106200010322     C                   SETON                                        99
106300010322     C                   ENDIF
106400010322     C     $TASTO        WHENEQ    F15
106500010322     C     $FC(15)       IFEQ      '0'
106600010322     C                   SETON                                        99
106700010322     C                   ENDIF
106800010322     C     $TASTO        WHENEQ    F16
106900010322     C     $FC(16)       IFEQ      '0'
107000010322     C                   SETON                                        99
107100010322     C                   ENDIF
107200010322     C     $TASTO        WHENEQ    F17
107300010322     C     $FC(17)       IFEQ      '0'
107400010322     C                   SETON                                        99
107500010322     C                   ENDIF
107600010322     C     $TASTO        WHENEQ    F18
107700010322     C     $FC(18)       IFEQ      '0'
107800010322     C                   SETON                                        99
107900010322     C                   ENDIF
108000010322     C     $TASTO        WHENEQ    F19
108100010322     C     $FC(19)       IFEQ      '0'
108200010322     C                   SETON                                        99
108300010322     C                   ENDIF
108400010322     C     $TASTO        WHENEQ    F20
108500010322     C     $FC(20)       IFEQ      '0'
108600010322     C                   SETON                                        99
108700010322     C                   ENDIF
108800010322     C     $TASTO        WHENEQ    F21
108900010322     C     $FC(21)       IFEQ      '0'
109000010322     C                   SETON                                        99
109100010322     C                   ENDIF
109200010322     C     $TASTO        WHENEQ    F22
109300010322     C     $FC(22)       IFEQ      '0'
109400010322     C                   SETON                                        99
109500010322     C                   ENDIF
109600010322     C     $TASTO        WHENEQ    F23
109700010322     C     $FC(23)       IFEQ      '0'
109800010322     C                   SETON                                        99
109900010322     C                   ENDIF
110000010322     C     $TASTO        WHENEQ    F24
110100010322     C     $FC(24)       IFEQ      '0'
110200010322     C                   SETON                                        99
110300010322     C                   ENDIF
110400010322     C                   ENDSL
110500010322     C                   ENDSR
110600010322     C************************************************************
110700010322     C* RIEMPIMENTO TASTI FUNZIONE
110800010322     C************************************************************
110900010322     C     RIEKEY        BEGSR
111000010322     C*
111100010322     C* Input :
111200010322     C* - $FX = schiera con tutti i testi disponibili
111300010322     C* - $FC = schiere con i flag di validità associati ai testi
111400010322     C* - $FR = schiere con i flag di REVERSE IMAGE
111500010322     C* - $ULK = ultimo testo caricato
111600010322     C* - $L01 = Lunghezza prima riga
111700010322     C* - $L02 = Lunghezza seconda riga
111800010322     C* Output :
111900010322     C* - $ULK
112000010322     C* - $KEY1= 1° riga di tasti funzione
112100010322     C* - $KEY2= 2° riga di tasti funzione
112200010322     C*
112300010322     C                   CALLB     'XKEYMSG'
112400010322     C* input
112500010322     C                   PARM                    $FX
112600010322     C                   PARM                    $FC
112700010322     C                   PARM                    $FR
112800010322     C                   PARM                    $ULK              3 0
112900010322     C* output
113000010322     C                   PARM                    $KEY1            79
113100010322     C                   PARM                    $KEY2            79
113200010322    >C* input
113300010322    >C                   PARM      79            $L01              3 0
113400010322    >C                   PARM      79            $L02              3 0
113500010322     C*
113600060224A1148C                   PARM                    $ALLFUNCT       480
113700060224A1148C*
113800010322     C                   ENDSR
113900010322     C*
114000010323     C************************************************************
114100010322** TASTI DI FUNZIONE  D1
114200010322PROMSG    *LIBL     KEY0001  01
114300010322PROMSG    *LIBL     KEY0002  02
114400010322PROMSG    *LIBL     KEY0003  03
114500010322PROMSG    *LIBL     KEY0004  04
114600010322PROMSG    *LIBL     KEY0005  05
114700010322PROMSG    *LIBL     KEY0006  06
114800010322PROMSG    *LIBL     KEY0007  07
114900010322PROMSG    *LIBL     KEY0008  08
115000010322PROMSG    *LIBL     KEY0009  09
115100010322PROMSG    *LIBL     KEY0010  10
115200010322PROMSG    *LIBL     KEY0011  11
115300010322PROMSG    *LIBL     KEY0012  12
115400010322PROMSG    *LIBL     KEY0013  13
115500010322PROMSG    *LIBL     KEY0014  14
115600010322PROMSG    *LIBL     KEY0015  15
115700010322PROMSG    *LIBL     KEY0016  16
115800010322PROMSG    *LIBL     KEY0017  17
115900010322PROMSG    *LIBL     KEY0018  18
116000010322PROMSG    *LIBL     KEY0019  19
116100010322PROMSG    *LIBL     KEY0020  20
116200010322PROMSG    *LIBL     KEY0021  21
116300010322PROMSG    *LIBL     KEY0022  22
116400010322PROMSG    *LIBL     KEY0023  23
116500010322PROMSG    *LIBL     KEY0024  24
116600010322**  ABILITAZIONE D1
1167000103220N  01
1168000103220N  02
1169000103221N  03
1170000103221N  04
1171000103221N  05
1172000103221N  06
1173000103220N  07
1174000103221N  08
1175000103220N  09
1176000103220N  10
1177000103220N  11
1178000103221N  12
1179000103220N  13
1180000103220N  14
1181000103220N  15
1182000103220N  16
1183000103220N  17
1184000103220N  18
1185000103220N  19
1186000103220N  20
1187000103220N  21
1188000103220N  22
1189000103220N  23   Impostare a '1' se si usano le opzioni
1190000103221N  24   Lasciare sempre impostata a '1' la colonna $FC
