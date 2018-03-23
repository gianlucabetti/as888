000100010402     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) BNDDIR(PJXBND)
000200010611     H*PARMS ACTGRP(QILE) CVTOPT(*NONE)
000300010322      *****************************************************************
000400010322     H DECEDIT('0,') DATEDIT(*DMY.)
000500010322      *****************************************************************
000600010322      *
000700010322      *            AUTORIZZAZIONI JCOM/POSTA ELETTRONICA:
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
004200010322     FANGJCPD   CF   E             WORKSTN
004300010322     F                                     RENAME(GJCPD1:FMTD1)
004400010322     F                                     RENAME(GJCPT1:FMTT1)
004500010322     F                                     RENAME(GJCPZ1:FMTZ1)
004600010322     F* eventuali schermi supplementari
004700010322     F                                     RENAME(GJCPD2:FMTD2)
004800010322     F                                     RENAME(GJCPZ2:FMTZ2)
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
007300010322     D ANGJCPDS      E DS
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
015400010322     C                   MOVEL     ANGJCPDS      DDS755
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
033300010322     C                   MOVEL     DDS755        ANGJCPDS
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
034900010323     C                   MOVEL     SI            D1IMMJCMSG
035000010323     C                   MOVEL     SI            D1MANJCMSG
035100010323     C                   MOVEL     SI            D1ANNJCMSG
035200010323     C                   MOVEL     SI            D1VISJCMSG
035300010330      * Work flow
035400010330     C                   MOVEL     SI            D1IMMJcwft
035500010330     C                   MOVEL     SI            D1MANJcwft
035600010330     C                   MOVEL     SI            D1ANNJcwft
035700010330     C                   MOVEL     SI            D1VISJcwft
035800010322      * Legami msg/lista distribuz.
035900010323     C                   MOVEL     SI            D1IMMJCLMC
036000010323     C                   MOVEL     SI            D1MANJCLMC
036100010323     C                   MOVEL     SI            D1ANNJCLMC
036200010323     C                   MOVEL     SI            D1VISJCLMC
036300010322     C*
036400010322   E1C                   ENDIF
036500010322     C*
036600010322     C     XINZD1        ENDSR
036700010322     C************************************************************
036800010322     C* RIEMPIMENTO VIDEATA  D1
036900010322     C************************************************************
037000010322     C     RIED1         BEGSR
037100010322     C*
037200010322      * Messaggi precodificati
037300010323     C                   if        ImmJCMSG   = *On
037400010323     C                   movel     SI            D1ImmJCMSG
037500010322     C                   else
037600010323     C                   movel     NO            D1ImmJCMSG
037700010322     C                   endif
037800010323     C                   if        ManJCMSG   = *On
037900010323     C                   movel     SI            D1ManJCMSG
038000010322     C                   else
038100010323     C                   movel     NO            D1ManJCMSG
038200010322     C                   endif
038300010323     C                   if        AnnJCMSG   = *On
038400010323     C                   movel     SI            D1AnnJCMSG
038500010322     C                   else
038600010323     C                   movel     NO            D1AnnJCMSG
038700010322     C                   endif
038800010323     C                   if        VisJCMSG   = *On
038900010323     C                   movel     SI            D1VisJCMSG
039000010322     C                   else
039100010323     C                   movel     NO            D1VisJCMSG
039200010322     C                   endif
039300010330      * Work flow
039400010330     C                   if        ImmJCWFT   = *On
039500010330     C                   movel     SI            D1ImmJCWFT
039600010322     C                   else
039700010330     C                   movel     NO            D1ImmJCWFT
039800010322     C                   endif
039900010330     C                   if        ManJCWFT   = *On
040000010330     C                   movel     SI            D1ManJCWFT
040100010322     C                   else
040200010330     C                   movel     NO            D1ManJCWFT
040300010322     C                   endif
040400010330     C                   if        AnnJCWFT   = *On
040500010330     C                   movel     SI            D1AnnJCWFT
040600010322     C                   else
040700010330     C                   movel     NO            D1AnnJCWFT
040800010322     C                   endif
040900010330     C                   if        VisJCWFT   = *On
041000010330     C                   movel     SI            D1VisJCWFT
041100010322     C                   else
041200010330     C                   movel     NO            D1VisJCWFT
041300010322     C                   endif
041400010322      * Legami msg/lista distribuz.
041500010323     C                   if        ImmJCLMC   = *On
041600010323     C                   movel     SI            D1ImmJCLMC
041700010322     C                   else
041800010323     C                   movel     NO            D1ImmJCLMC
041900010322     C                   endif
042000010323     C                   if        ManJCLMC   = *On
042100010323     C                   movel     SI            D1ManJCLMC
042200010322     C                   else
042300010323     C                   movel     NO            D1ManJCLMC
042400010322     C                   endif
042500010323     C                   if        AnnJCLMC   = *On
042600010323     C                   movel     SI            D1AnnJCLMC
042700010322     C                   else
042800010323     C                   movel     NO            D1AnnJCLMC
042900010322     C                   endif
043000010323     C                   if        VisJCLMC   = *On
043100010323     C                   movel     SI            D1VisJCLMC
043200010322     C                   else
043300010323     C                   movel     NO            D1VisJCLMC
043400010322     C                   endif
043500010322     C*
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
047300010322     C*
047400010322      * Messaggi precodificati
047500010323     C                   if        D1ImmJCMSG  = SI
047600010323     C                   movel     *On           ImmJCMSG
047700010322     C                   else
047800010323     C                   movel     *Off          ImmJCMSG
047900010322     C                   endif
048000010323     C                   if        D1ManJCMSG  = SI
048100010323     C                   movel     *On           ManJCMSG
048200010322     C                   else
048300010323     C                   movel     *Off          ManJCMSG
048400010322     C                   endif
048500010323     C                   if        D1AnnJCMSG  = SI
048600010323     C                   movel     *On           AnnJCMSG
048700010322     C                   else
048800010323     C                   movel     *Off          AnnJCMSG
048900010322     C                   endif
049000010323     C                   if        D1VisJCMSG  = SI
049100010323     C                   movel     *On           VisJCMSG
049200010322     C                   else
049300010323     C                   movel     *Off          VisJCMSG
049400010322     C                   endif
049500010330      * Work flow
049600010330     C                   if        D1ImmJCWFT  = SI
049700010330     C                   movel     *On           ImmJCWFT
049800010322     C                   else
049900010330     C                   movel     *Off          ImmJCWFT
050000010322     C                   endif
050100010330     C                   if        D1ManJCWFT  = SI
050200010330     C                   movel     *On           ManJCWFT
050300010322     C                   else
050400010330     C                   movel     *Off          ManJCWFT
050500010322     C                   endif
050600010330     C                   if        D1AnnJCWFT  = SI
050700010330     C                   movel     *On           AnnJCWFT
050800010322     C                   else
050900010330     C                   movel     *Off          AnnJCWFT
051000010322     C                   endif
051100010330     C                   if        D1VisJCWFT  = SI
051200010330     C                   movel     *On           VisJCWFT
051300010322     C                   else
051400010330     C                   movel     *Off          VisJCWFT
051500010322     C                   endif
051600010322      * Legami msg/lista distribuz.
051700010323     C                   if        D1ImmJCLMC  = SI
051800010323     C                   movel     *On           ImmJCLMC
051900010322     C                   else
052000010323     C                   movel     *Off          ImmJCLMC
052100010322     C                   endif
052200010323     C                   if        D1ManJCLMC  = SI
052300010323     C                   movel     *On           ManJCLMC
052400010322     C                   else
052500010323     C                   movel     *Off          ManJCLMC
052600010322     C                   endif
052700010323     C                   if        D1AnnJCLMC  = SI
052800010323     C                   movel     *On           AnnJCLMC
052900010322     C                   else
053000010323     C                   movel     *Off          AnnJCLMC
053100010322     C                   endif
053200010323     C                   if        D1VisJCLMC  = SI
053300010323     C                   movel     *On           VisJCLMC
053400010322     C                   else
053500010323     C                   movel     *Off          VisJCLMC
053600010322     C                   endif
053700010323     C*
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
000000060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
057400010322     C                   MOVEL     $KEY2         Z1KE2
057500010322     C                   WRITE     FMTZ1
057600010322     C*
057700010322     C                   ENDSR
057800010322     C************************************************************
057900010322     C* CONTROLLO VIDEATA
058000010322     C************************************************************
058100010322     C     CTRD1         BEGSR
058200010322     C*
058300010323     C                   eval      *In99 = *Off
058400010322     C* non imposto pos. del cursore
058500010322     C                   Z-ADD     0             H1RIGA
058600010322     C                   Z-ADD     0             H1COLO
058700010323      *
058800010323      * Ctrl validità di tutti i flag
058900010323      *
059000010323      * Messaggi precodificati
059100010323     C                   exsr      CtrlJCMSG
059200010323      *
059300010330      * Work flow
059400010330     C                   exsr      CtrlJCWFT
059500010323      *
059600010323      * Legami msg/lista distribuz.
059700010323     C                   exsr      CtrlJCLMC
059800010323      *
059900010323     C     XCtrD1        ENDSR
060000010323      ************************************************************
060100010323      * Controllo flag a video Messaggi precodificati
060200010323      ************************************************************
060300010323    >C     CtrlJCMSG     BEGSR
060400010323      *
060500010323      * Messaggi precodificati
060600010323     C                   if        D1ImmJCMSG<>SI  and  D1ImmJCMSG<>NO
060700010323     C                   eval      *In51 = *On
060800010323     C                   eval      *In99 = *On
060900010323     C                   endif
061000010323     C                   if        D1ManJCMSG<>SI  and  D1ManJCMSG<>NO
061100010323     C                   eval      *In52 = *On
061200010323     C                   eval      *In99 = *On
061300010323     C                   endif
061400010323     C                   if        D1AnnJCMSG<>SI  and  D1AnnJCMSG<>NO
061500010323     C                   eval      *In53 = *On
061600010323     C                   eval      *In99 = *On
061700010323     C                   endif
061800010323     C                   if        D1VisJCMSG<>SI  and  D1VisJCMSG<>NO
061900010323     C                   eval      *In54 = *On
062000010323     C                   eval      *In99 = *On
062100010323     C                   endif
062200010323      *
062300010323     C     XCtrJCMSG     ENDSR
062400010323      ************************************************************
062500010330      * Controllo flag a video Work flow
062600010323      ************************************************************
062700010330    >C     CtrlJCWFT     BEGSR
062800010323      *
062900010330      * Work flow
063000010330     C                   if        D1ImmJCWFT <>SI  and  D1ImmJCWFT <>NO
063100010323     C                   eval      *In55 = *On
063200010323     C                   eval      *In99 = *On
063300010323     C                   endif
063400010330     C                   if        D1ManJCWFT <>SI  and  D1ManJCWFT <>NO
063500010323     C                   eval      *In56 = *On
063600010323     C                   eval      *In99 = *On
063700010323     C                   endif
063800010330     C                   if        D1AnnJCWFT <>SI  and  D1AnnJCWFT <>NO
063900010323     C                   eval      *In57 = *On
064000010323     C                   eval      *In99 = *On
064100010323     C                   endif
064200010330     C                   if        D1VisJCWFT <>SI  and  D1VisJCWFT <>NO
064300010323     C                   eval      *In58 = *On
064400010323     C                   eval      *In99 = *On
064500010323     C                   endif
064600010323      *
064700010330     C     XCtrJCWFT     ENDSR
064800010323      ************************************************************
064900010323      * Controllo flag a video Legami msg/lista distribuz.
065000010323      ************************************************************
065100010323    >C     CtrlJCLMC     BEGSR
065200010323      *
065300010323      * Legami msg/lista distribuz.
065400010323     C                   if        D1ImmJCLMC<>SI  and  D1ImmJCLMC<>NO
065500010323     C                   eval      *In59 = *On
065600010323     C                   eval      *In99 = *On
065700010323     C                   endif
065800010323     C                   if        D1ManJCLMC<>SI  and  D1ManJCLMC<>NO
065900010323     C                   eval      *In60 = *On
066000010323     C                   eval      *In99 = *On
066100010323     C                   endif
066200010323     C                   if        D1AnnJCLMC<>SI  and  D1AnnJCLMC<>NO
066300010323     C                   eval      *In61 = *On
066400010323     C                   eval      *In99 = *On
066500010323     C                   endif
066600010323     C                   if        D1VisJCLMC<>SI  and  D1VisJCLMC<>NO
066700010323     C                   eval      *In62 = *On
066800010323     C                   eval      *In99 = *On
066900010323     C                   endif
067000010323      *
067100010323     C     XCtrJCLMC     ENDSR
067200010322     C************************************************************
067300010322     C* GESTIONE VIDEO RECORD  D2
067400010322     C************************************************************
067500010322    >C     GESD2         BEGSR
067600010322     C* emissione piede videata se proveniente da altra
067700010322     C* salvataggio valore formato di provenienza
067800010322     C     $LASTV        IFNE      'D2'
067900010322     C                   WRITE     FMTT1
068000010322     C                   EXSR      GESPIE
068100010322     C                   MOVE      $LASTV        $LASTG
068200010322     C                   MOVE      'D2'          $LASTV
068300010322     C                   ENDIF
068400010322     C*
068500010322     C* se esistono errori sulla videata
068600010322     C* emetto la write del formato a indicatori spenti per vedere
068700010322     C* le eventuali decodifiche
068800010322     C     *IN99         IFEQ      *ON
068900010322     C     $GEST         ANDEQ     'D2'
069000010322     C                   MOVEA     *IN           WIN
069100010322     C                   MOVE      *ALL'0'       IN5098           49
069200010322     C                   MOVEA     IN5098        *IN(50)
069300010322     C                   WRITE     FMTD2
069400010322     C                   MOVEA     WIN           *IN
069500010322     C                   ENDIF
069600010322     C*
069700010322     C                   SELECT
069800010322     C* Visualizzazione
069900010322     C     OPZ755        WHENEQ    5
070000010322     C                   WRITE     FMTD2
070100010322     C                   EXFMT     PROTECT
070200010322     C* Immissione/Modifica/Copia
070300010322     C                   OTHER
070400010322     C                   EXFMT     FMTD2
070500010322     C                   ENDSL
070600010322     C* controllo abilitazione tasti funzione
070700010322     C*
070800010322     C                   EXSR      CTRKD1
070900010322     C*
071000010322     C     *IN99         IFEQ      *OFF
071100010322     C*
071200010322     C                   SELECT
071300010322     C* ROLDOWN
071400010322     C     $TASTO        WHENEQ    ROLDWN
071500010322     C                   MOVE      'D1'          $GEST
071600010322     C* F3=Fine
071700010322     C     $TASTO        WHENEQ    F03
071800010322     C                   EXSR      F03D1
071900010322     C* F4=Ricerca
072000010322     C     $TASTO        WHENEQ    F04
072100010322     C                   EXSR      SEARCH
072200010322     C* F5=Refresh
072300010322     C     $TASTO        WHENEQ    F05
072400010322     C                   EXSR      F05D2
072500010322     C* F8=Successivo
072600010322     C     $TASTO        WHENEQ    F08
072700010322     C                   EXSR      F08D1
072800010322     C* F12=Ritorno
072900010322     C     $TASTO        WHENEQ    F12
073000010322     C                   EXSR      F12D1
073100010322     C* F24=Altri tasti
073200010322     C     $TASTO        WHENEQ    F24
073300010322     C                   EXSR      F24D2
073400010322     C*
073500010322     C                   OTHER
073600010322     C                   EXSR      CTRD2
073700010322     C     *IN99         IFEQ      *OFF
073800010322     C                   EXSR      CTRD1
073900010322     C     *IN99         IFEQ      *ON
074000010322     C                   SETON                                            96
074100010322     C                   ENDIF
074200010322     C                   ENDIF
074300010322     C*
074400010322     C*
074500010322     C     $TASTO        IFEQ      F06
074600010322     C     *IN99         ANDEQ     *OFF
074700010322     C* ripasso la stringa contenente i dati modificati al pgm
074800010322     C* chiamante che sarà lui a salvarla nel file
074900010322     C                   EXSR      F06D1
075000010322     C                   ENDIF
075100010322     C*
075200010322     C                   ENDSL
075300010322     C*
075400010322     C                   ENDIF
075500010322     C*
075600010322    >C                   ENDSR
075700010322     C/EJECT
075800010322     C************************************************************
075900010322     C* RIEMPIMENTO VIDEATA D2
076000010322     C************************************************************
076100010322    >C     RIED2         BEGSR
076200010322     C*
076300010322     C*
076400010322    >C                   ENDSR
076500010322     C************************************************************
076600010322     C* GESTIONE F05 VIDEO D2
076700010322     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
076800010322     C* e il riposizionamento alla prima
076900010322     C************************************************************
077000010322    >C     F05D2         BEGSR
077100010322     C*
077200010322     C                   MOVE      *ON           $INZD1
077300010322     C                   MOVE      'D1'          $GEST
077400010322     C*
077500010322    >C                   ENDSR
077600010322     C************************************************************
077700010322     C* GESTIONE F24 VIDEO D2
077800010322     C************************************************************
077900010322    >C     F24D2         BEGSR
078000010322     C*
078100010322     C                   MOVEA     $FX1          $FX
078200010322     C                   MOVEA     $FC1          $FC
078300010322     C                   MOVEA     $FR1          $FR
078400010322     C                   Z-ADD     $ULKD1        $ULK
078500010322     C                   EXSR      RIEKEY
078600010322     C                   Z-ADD     $ULK          $ULKD1            3 0
078700010322     C                   MOVEA     $FC           $FC1
078800010322     C                   MOVEL     $KEY1         Z1KE1
000000060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
078900010322     C                   MOVEL     $KEY2         Z1KE2
079000010322     C                   WRITE     FMTZ1
079100010322     C*
079200010322    >C                   ENDSR
079300010322     C************************************************************
079400010322     C* CONTROLLO VIDEATA  D2
079500010322     C************************************************************
079600010322    >C     CTRD2         BEGSR
079700010322     C*
079800010322     C                   SETOFF                                       99
079900010322     C* non imposto pos. del cursore
080000010322     C                   Z-ADD     0             H1RIGA
080100010322     C                   Z-ADD     0             H1COLO
080200010322     C*
080300010322     C                   ENDSR
080400010322     C************************************************************
080500010322     C* OPERAZIONI INIZIALI
080600010322     C************************************************************
080700010322     C     *INZSR        BEGSR
080800010322     C*
080900010322     C* Reperimento parametri
081000010322     C*
081100010322     C     *ENTRY        PLIST
081200010323     C                   PARM                    KPJBA
081300010322     C                   PARM                    PAR755
081400010322     C*
081500010322     C* Reperimento tasti di funzione
081600010322     C*
081700010322     C                   EXSR      REPKEY
081800010322     C*
081900010322     C                   ENDSR
082000010322     C************************************************************
082100010322     C* REPERIMENTO TASTI DI FUNZIONE
082200010322     C************************************************************
082300010322     C     REPKEY        BEGSR
082400010322     C                   MOVEA     $FM1          $FM
082500010322     C                   MOVEA     $FC1          $FC
082600010322     C                   EXSR      REPKKK
082700010322     C                   MOVEA     $FX           $FX1
082800010322     C*
082900010322     C                   ENDSR
083000010322     C************************************************************
083100010322     C* REPERIMENTO TASTI DI FUNZIONE
083200010322     C************************************************************
083300010322     C     REPKKK        BEGSR
083400010322     C*
083500010322     C                   CLEAR                   $FX
083600010322     C     1             DO        24            X
083700010322     C*
083800010322     C     $FM(X)        IFNE      *BLANK
083900010322     C     $FC(X)        ANDNE     '0'
084000010322     C*
084100010322     C                   MOVEL     $FM(X)        IDMSG
084200010322     C                   CALLB     'XRTVM'
084300010322     C                   PARM                    IDMSG            27
084400010322     C                   PARM                    TXTMSG          100
084500010322     C                   PARM                    ERRMSG            1
084600010322     C*
084700010322     C     ERRMSG        IFNE      *ON
084800010322     C                   MOVEL     TXTMSG        $FX(X)
084900010322     C                   ELSE
085000010322     C                   MOVEL     *ALL' '       $FX(X)
085100010322     C                   ENDIF
085200010322     C*
085300010322     C                   ENDIF
085400010322     C*
085500010322     C                   ENDDO
085600010322     C*
085700010322     C                   ENDSR
085800010322     C************************************************************
085900010322     C* INIZIALIZZAZIONE VARIABILI
086000010322     C************************************************************
086100010322     C     INZVAR        BEGSR
086200010322     C*
086300010322     C* Reperimento dati società
086400010322     C*
086500010322    >C                   MOVEL     'SOC001'      TIPXSC
086600010322     C                   MOVEL     *BLANK        SOCXSC
086700010322     C                   EXSR      REPSOC
086800010322     C     RTNXSC        IFNE      '1'
086900010322     C                   MOVEL     XSOCDS        SOC001
087000010322     C                   ELSE
087100010322     C                   RETURN
087200010322     C                   ENDIF
087300010322     C*
087400010322     C* Dati da PGM guida
087500010322     C*
087600010323    >C                   MOVEL     KPJBU         ANGJCPDS
087700010322     C*
087800010322     C* Definizione dei tasti funzione utilizzabili a pgm
087900010322     C*
088000010322     C                   EXSR      DEFOPZ
088100010322     C*
088200010322     C* Variabili per gestione videate
088300010322     C*
088400010322     C                   MOVE      'D1'          $GEST                          fmt in gestione
088500010322     C                   MOVE      *OFF          $FINE                          fine pgm
088600010322     C                   MOVE      *ON           $INZD1                         inizializzione D1
088700010322     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
088800010322     C                   MOVE      *BLANK        $LASTV                         ultima emissione
088900010322     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
089000010322     C*
089100010322     C* Variabili appoggio
089200010322     C*
089300010322     C                   Z-ADD     0             CURR
089400010322     C                   Z-ADD     0             CURC
089500010322     C                   MOVE      XSCON         SI
089600010322     C                   MOVE      XSCOFF        NO
089700010322     C*
089800010322     C* Valorizzazione campi univoci testate
089900010322     C*
090000010322     C                   MOVEL     KNSIF         NOMSIF
090100010322     C                   MOVEL     XSCDSI        NOMDIT
090200010322     C                   MOVEL     DSPGM         NOMPGM
090300010322     C*
090400010322     C* Centratura decodifica funzione
090500010322     C*
090600010322     C                   Eval      XLenStr = %Size(XFn755)
090700010322     C                   Eval      XLenTot = %Size(XFn755)
090800010322     C                   Call      'X71CENTRA'
090900010322     C                   Parm                    XFn755
091000010322     C                   Parm                    XLenStr           3 0
091100010322     C                   Parm                    XLenTot           3 0
091200010322     C*
091300010322     C* Inizializzazione/Decodifica prima videata
091400010322     C*
091500010322     C                   EXSR      INZD1
091600010322     C                   MOVE      *OFF          $INZD1
091700010322     C*
091800010322     C                   ENDSR
091900010322     C************************************************************
092000010322     C* REPERIMENTO DATI SOCIETA'
092100010322     C************************************************************
092200010322     C     REPSOC        BEGSR
092300010322     C*
092400010322     C                   CALLB     'XSOC'
092500010322     C                   PARM                    TIPXSC            6
092600010322     C                   PARM                    SOCXSC            3
092700010322     C                   PARM                    CDSXSC            9 0
092800010322     C                   PARM                    MODXSC            3
092900010322     C                   PARM      *BLANKS       RTNXSC            1
093000010322     C                   PARM                    XSOCDS
093100010322     C                   PARM                    KPJBA
093200010322     C*
093300010322     C                   ENDSR
093400010322     C************************************************************
093500010322     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
093600010322     C************************************************************
093700010322     C     DEFOPZ        BEGSR
093800010322     C*
093900010322     C* Questa routine deve valorizzare la schiera $FC1/$FR1
094000010322     C* con S o N a seconda della utilizzabilità o meno
094100010322     C* del tasto funzione correlato
094200010322     C*
094300010322     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
094400010322     C* di compilazione
094500010322     C                   RESET                   $FC1
094600010322     C                   RESET                   $FR1
094700010322     C*
094800010322     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
094900010322     C* invalido alcune opzioni
095000010322     C                   SELECT
095100010322     C* Se pgm richiamato in interrogazione disattivo :
095200010322     C     OPZ755        WHENEQ    5
095300010322     C* F4=Ricerca
095400010322     C                   Z-ADD     4             X
095500010322     C                   MOVE      '0'           $FC1(X)
095600010322     C* F5=Ripristino
095700010322     C                   Z-ADD     5             X
095800010322     C                   MOVE      '0'           $FC1(X)
095900010322     C* F6=Conferma
096000010322     C                   Z-ADD     6             X
096100010322     C                   MOVE      '0'           $FC1(X)
096200010322     C*
096300010322     C                   OTHER
096400010322     C*
096500010322     C                   ENDSL
096600010322     C*
096700010322     C                   ENDSR
096800010322     C************************************************************
096900010322     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
097000010322     C************************************************************
097100010322     C     DEFCAM        BEGSR
097200010322     C*
097300010322     C* klist
097400010322     C*
097500010322     C*
097600010322     C* Variabili per gestione videate
097700010322     C*
097800010322     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
097900010322     C                   MOVE      *BLANK        $FINE             1            fine pgm
098000010322     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
098100010322     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
098200010322     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
098300010322     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
098400010322     C*
098500010322     C* Variabili appoggio
098600010322     C*
098700010322     C                   Z-ADD     0             CURR              2 0
098800010322     C                   Z-ADD     0             CURC              2 0
098900010322     C                   MOVE      *ZEROS        WIN              99
099000010322     C                   MOVE      *BLANK        SI                1
099100010322     C                   MOVE      *BLANK        NO                1
099200010322     C*
099300010322     C* Indici
099400010322     C*
099500010322     C                   Z-ADD     0             X                 3 0
099600010322     C                   Z-ADD     0             Y                 3 0
099700010322     C*
099800010322     C                   ENDSR
099900010322     C************************************************************
100000010322     C/EJECT
100100010322     C************************************************************
100200010322     C* CONTROLLO ABILITAZIONE TASTI
100300010322     C************************************************************
100400010322     C     CTRKEY        BEGSR
100500010322     C                   SELECT
100600010322     C     $TASTO        WHENEQ    F01
100700010322     C     $FC(1)        IFEQ      '0'
100800010322     C                   SETON                                        99
100900010322     C                   ENDIF
101000010322     C     $TASTO        WHENEQ    F02
101100010322     C     $FC(2)        IFEQ      '0'
101200010322     C                   SETON                                        99
101300010322     C                   ENDIF
101400010322     C     $TASTO        WHENEQ    F03
101500010322     C     $FC(3)        IFEQ      '0'
101600010322     C                   SETON                                        99
101700010322     C                   ENDIF
101800010322     C     $TASTO        WHENEQ    F04
101900010322     C     $FC(4)        IFEQ      '0'
102000010322     C                   SETON                                        99
102100010322     C                   ENDIF
102200010322     C     $TASTO        WHENEQ    F05
102300010322     C     $FC(5)        IFEQ      '0'
102400010322     C                   SETON                                        99
102500010322     C                   ENDIF
102600010322     C     $TASTO        WHENEQ    F06
102700010322     C     $FC(6)        IFEQ      '0'
102800010322     C                   SETON                                        99
102900010322     C                   ENDIF
103000010322     C     $TASTO        WHENEQ    F07
103100010322     C     $FC(7)        IFEQ      '0'
103200010322     C                   SETON                                        99
103300010322     C                   ENDIF
103400010322     C     $TASTO        WHENEQ    F08
103500010322     C     $FC(8)        IFEQ      '0'
103600010322     C                   SETON                                        99
103700010322     C                   ENDIF
103800010322     C     $TASTO        WHENEQ    F09
103900010322     C     $FC(09)       IFEQ      '0'
104000010322     C                   SETON                                        99
104100010322     C                   ENDIF
104200010322     C     $TASTO        WHENEQ    F10
104300010322     C     $FC(10)       IFEQ      '0'
104400010322     C                   SETON                                        99
104500010322     C                   ENDIF
104600010322     C     $TASTO        WHENEQ    F11
104700010322     C     $FC(11)       IFEQ      '0'
104800010322     C                   SETON                                        99
104900010322     C                   ENDIF
105000010322     C     $TASTO        WHENEQ    F12
105100010322     C     $FC(12)       IFEQ      '0'
105200010322     C                   SETON                                        99
105300010322     C                   ENDIF
105400010322     C     $TASTO        WHENEQ    F13
105500010322     C     $FC(13)       IFEQ      '0'
105600010322     C                   SETON                                        99
105700010322     C                   ENDIF
105800010322     C     $TASTO        WHENEQ    F14
105900010322     C     $FC(14)       IFEQ      '0'
106000010322     C                   SETON                                        99
106100010322     C                   ENDIF
106200010322     C     $TASTO        WHENEQ    F15
106300010322     C     $FC(15)       IFEQ      '0'
106400010322     C                   SETON                                        99
106500010322     C                   ENDIF
106600010322     C     $TASTO        WHENEQ    F16
106700010322     C     $FC(16)       IFEQ      '0'
106800010322     C                   SETON                                        99
106900010322     C                   ENDIF
107000010322     C     $TASTO        WHENEQ    F17
107100010322     C     $FC(17)       IFEQ      '0'
107200010322     C                   SETON                                        99
107300010322     C                   ENDIF
107400010322     C     $TASTO        WHENEQ    F18
107500010322     C     $FC(18)       IFEQ      '0'
107600010322     C                   SETON                                        99
107700010322     C                   ENDIF
107800010322     C     $TASTO        WHENEQ    F19
107900010322     C     $FC(19)       IFEQ      '0'
108000010322     C                   SETON                                        99
108100010322     C                   ENDIF
108200010322     C     $TASTO        WHENEQ    F20
108300010322     C     $FC(20)       IFEQ      '0'
108400010322     C                   SETON                                        99
108500010322     C                   ENDIF
108600010322     C     $TASTO        WHENEQ    F21
108700010322     C     $FC(21)       IFEQ      '0'
108800010322     C                   SETON                                        99
108900010322     C                   ENDIF
109000010322     C     $TASTO        WHENEQ    F22
109100010322     C     $FC(22)       IFEQ      '0'
109200010322     C                   SETON                                        99
109300010322     C                   ENDIF
109400010322     C     $TASTO        WHENEQ    F23
109500010322     C     $FC(23)       IFEQ      '0'
109600010322     C                   SETON                                        99
109700010322     C                   ENDIF
109800010322     C     $TASTO        WHENEQ    F24
109900010322     C     $FC(24)       IFEQ      '0'
110000010322     C                   SETON                                        99
110100010322     C                   ENDIF
110200010322     C                   ENDSL
110300010322     C                   ENDSR
110400010322     C************************************************************
110500010322     C* RIEMPIMENTO TASTI FUNZIONE
110600010322     C************************************************************
110700010322     C     RIEKEY        BEGSR
110800010322     C*
110900010322     C* Input :
111000010322     C* - $FX = schiera con tutti i testi disponibili
111100010322     C* - $FC = schiere con i flag di validità associati ai testi
111200010322     C* - $FR = schiere con i flag di REVERSE IMAGE
111300010322     C* - $ULK = ultimo testo caricato
111400010322     C* - $L01 = Lunghezza prima riga
111500010322     C* - $L02 = Lunghezza seconda riga
111600010322     C* Output :
111700010322     C* - $ULK
111800010322     C* - $KEY1= 1° riga di tasti funzione
111900010322     C* - $KEY2= 2° riga di tasti funzione
112000010322     C*
112100010322     C                   CALLB     'XKEYMSG'
112200010322     C* input
112300010322     C                   PARM                    $FX
112400010322     C                   PARM                    $FC
112500010322     C                   PARM                    $FR
112600010322     C                   PARM                    $ULK              3 0
112700010322     C* output
112800010322     C                   PARM                    $KEY1            79
112900010322     C                   PARM                    $KEY2            79
113000010322    >C* input
113100010322    >C                   PARM      79            $L01              3 0
113200010322    >C                   PARM      79            $L02              3 0
113300010322     C*
000000060224A1148C                   PARM                    $ALLFUNCT       480
000000060224A1148C*
113400010322     C                   ENDSR
113500010322     C*
113600010323     C************************************************************
113700010322** TASTI DI FUNZIONE  D1
113800010322PROMSG    *LIBL     KEY0001  01
113900010322PROMSG    *LIBL     KEY0002  02
114000010322PROMSG    *LIBL     KEY0003  03
114100010322PROMSG    *LIBL     KEY0004  04
114200010322PROMSG    *LIBL     KEY0005  05
114300010322PROMSG    *LIBL     KEY0006  06
114400010322PROMSG    *LIBL     KEY0007  07
114500010322PROMSG    *LIBL     KEY0008  08
114600010322PROMSG    *LIBL     KEY0009  09
114700010322PROMSG    *LIBL     KEY0010  10
114800010322PROMSG    *LIBL     KEY0011  11
114900010322PROMSG    *LIBL     KEY0012  12
115000010322PROMSG    *LIBL     KEY0013  13
115100010322PROMSG    *LIBL     KEY0014  14
115200010322PROMSG    *LIBL     KEY0015  15
115300010322PROMSG    *LIBL     KEY0016  16
115400010322PROMSG    *LIBL     KEY0017  17
115500010322PROMSG    *LIBL     KEY0018  18
115600010322PROMSG    *LIBL     KEY0019  19
115700010322PROMSG    *LIBL     KEY0020  20
115800010322PROMSG    *LIBL     KEY0021  21
115900010322PROMSG    *LIBL     KEY0022  22
116000010322PROMSG    *LIBL     KEY0023  23
116100010322PROMSG    *LIBL     KEY0024  24
116200010322**  ABILITAZIONE D1
1163000103220N  01
1164000103220N  02
1165000103221N  03
1166000103221N  04
1167000103221N  05
1168000103221N  06
1169000103220N  07
1170000103221N  08
1171000103220N  09
1172000103220N  10
1173000103220N  11
1174000103221N  12
1175000103220N  13
1176000103220N  14
1177000103220N  15
1178000103220N  16
1179000103220N  17
1180000103220N  18
1181000103220N  19
1182000103220N  20
1183000103220N  21
1184000103220N  22
1185000103220N  23   Impostare a '1' se si usano le opzioni
1186000103221N  24   Lasciare sempre impostata a '1' la colonna $FC
