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
002500100409     Fazute01L  uF   E           K DISK    usropn
002600100121     Fazorg01L  IF   E           K DISK
002700100121     Fanutn01L  IF   E           K DISK
002800100121     Fkpprf01L  IF   E           K DISK
002900100121$001 FTNTBAUTD2 CF   E             WORKSTN
003000940201     F                                     INFDS(DSFMT)
003100100121     Ftntbe01L  UF A E           K DISK
003200940128     D*----------------------------------------------------*
003300940127     D* Passaggio Parametri
003400940127     D KPJBA         E DS
003500100121     D dAUT          E DS
003600100409     D dUte01        E DS
003700030113      *-------------
003800030113     D* Parametri in ricezione
003900030113     D  TABDS          DS
004000030113     D  XTAOPZ                 1      2
004100030113     D  XTARET                 3      3
004200030113     D  XTAOPR                 4      4
004300030113     D  XTAERR                 5      5
004400100409     D  XTAKEY1                6     20
004500100409     D  XTAKEY2               21     33
004600100409     D  XTAcod                31     33
004700100325     D*-------------
004800100325     D DSP_dsCAMPI     DS
004900100325     D  dsp_SKcampi            1     24    DIM(24)
005000100325      *
005100100325     D  D1AUTMTC               1      1
005200100325     D  D1AUTSDT               2      2
005300100325     D  D1AUTRET               3      3
005400100325     D  D1AUTCON               4      4
005500100325     D  D1AUTCRS               5      5
005600100325     D  D1AUTSTDCC             6      6
005700100325     d  D1_AUT                 1      6
005800100325     D  dsp_SKaut              1      6    DIM(6)
005900100325      *
006000100325     D  D1AUTVTCC              7      7
006100100325     D  D1AUTMTCC              8      8
006200100325     D  D1AUTSDTC              9      9
006300100325     D  D1AUTRETC             10     10
006400100325     D  D1AUTCRSC             11     11
006500100325     D  D1AUTCONC             12     12
006600100325     D  D1AUTGSTM             13     13
006700100325     d  D1_COOP                7     13
006800100325     D  dsp_SKcoop             7     13    DIM(7)
006900100325      *
007000100726     D  D1AFFMTC              16     16
007100100325     D  D1AFFCON              17     17
007200100325     D  D1AFFCRS              18     18
007300100325     D  D1AFFGVSTD            19     19
007400100325     D  D1AFFAGV              20     20
007500100325     D  D1AFFCPSTD            21     21
007600100325     D  D1AFFARRM             22     22
007700100726     d  D1_AFFDEF             16     22
007800100726     D  dsp_SKaff             16     22    DIM(7)
007900100325      *
008000100325     D  D1RIEABI              23     23
008100100325     D  D1AUTCPRRE            24     24
008200100325      *
008300940201     D*-------------
008400030204     D  S_TASTO        S                   like($Tasto)
008500940201     D DSFMT           DS
008600940506     D  $TASTO               369    369
008700940201     D  NRG                  370    370
008800940201     D  NCL                  371    371
008900940201     D*-------------
009000940201     D* posizione cursore
009100940201     D CURSOR          DS
009200940223     D  RIRI                   1      2B 0 INZ
009300940201     D  R$$                    2      2
009400940223     D  COCO                   3      4B 0 INZ
009500940201     D  C$$                    4      4
009600940207     D*-------------
009700940207     D* Reperimento nome PGM
009800940207     D STATUS         SDS           333
009900940207     D  DSPGM            *PROC
010000940225     D*-------------
010100940225     D* COSTANTI
010200940225     D*-------------
010300940506     D* Tasti di funzione
010400940506     D F01             C                   CONST(X'31')
010500940506     D F02             C                   CONST(X'32')
010600940506     D F03             C                   CONST(X'33')
010700940506     D F04             C                   CONST(X'34')
010800940506     D F05             C                   CONST(X'35')
010900940506     D F06             C                   CONST(X'36')
011000940506     D F07             C                   CONST(X'37')
011100940506     D F08             C                   CONST(X'38')
011200940506     D F09             C                   CONST(X'39')
011300940506     D F10             C                   CONST(X'3A')
011400940506     D F11             C                   CONST(X'3B')
011500940506     D F12             C                   CONST(X'3C')
011600940506     D F13             C                   CONST(X'B1')
011700940506     D F14             C                   CONST(X'B2')
011800940506     D F15             C                   CONST(X'B3')
011900940506     D F16             C                   CONST(X'B4')
012000940506     D F17             C                   CONST(X'B5')
012100940506     D F18             C                   CONST(X'B6')
012200940506     D F19             C                   CONST(X'B7')
012300940506     D F20             C                   CONST(X'B8')
012400940506     D F21             C                   CONST(X'B9')
012500940506     D F22             C                   CONST(X'BA')
012600940506     D F23             C                   CONST(X'BB')
012700940506     D F24             C                   CONST(X'BC')
012800940506     D ENTER           C                   CONST(X'F1')
012900940506     D ROLDWN          C                   CONST(X'F4')
013000940506     D ROLLUP          C                   CONST(X'F5')
013100940506     D*-------------
013200941108     D DATA            C                   CONST('0001-01-01')
013300030114     D Annull          C                   CONST('  ANNULLATO  ')
013400100323     D OPz_Immissione  C                   CONST('IMMISSIONE')
013500100323     D OPz_Variazione  C                   CONST('VARIAZIONE')
013600100323     D OPz_Annulla     C                   CONST(' ANNULLA  ')
013700100323     D OPz_Visualizza  C                   CONST('VISUALIZZA')
013800100323     D OPz_Riattiva    C                   CONST(' RIATTIVA ')
013900940127     C*----------------------------------------------------*
014000940127     C*                MAIN LINE PROGRAM
014100940127     C*----------------------------------------------------*
014200100121     c                   exsr      INIZIO
014300940223     C*
014400940223     C     $FINE         DOWEQ     *OFF
014500940202     C     $GEST         CASEQ     'D1'          GESD1
014600940117     C                   END
014700940117     C                   END
014800940325     C* fine programma
014900030113     c                   movel     tabds         kpjbu
015000030113     C                   Seton                                        LR
015100940131     C************************************************************
015200100121     C* OPERAZIONI INIZIALI
015300100121     C************************************************************
015400100121     C     INIZIO        BEGSR
015500100121     C*
015600100121     C* Reperimento parametri
015700100121     C     *ENTRY        PLIST
015800100121     C                   PARM                    KPJBA
015900100121      *
016000100121     C                   movel     kpjbu         tabds
016100100121     c                   setoff                                       08
016200100121     C*
016300100121     C* Variabili per gestione videate
016400100121     C                   MOVE      'D1'          $GEST
016500100121     C                   MOVE      *OFF          $FINE
016600100121     C                   MOVE      *ON           $INZD1
016700100121     C                   MOVE      *BLANK        $LASTG
016800100121     C                   MOVE      *BLANK        $LASTV
016900100121     C                   Z-ADD     0             $ULKD1            3 0
017000100121     C                   MOVEL     'AUT'         tbe_COD           3
017100100121     C                   CLEAR                   dAUT
017200100121     C*
017300100121     C* Variabili appoggio
017400100121     C                   Z-ADD     0             CURR
017500100121     C                   Z-ADD     0             CURC
017600100121     C*
017700100121     C* Valorizzazione campi univoci testate
017800100121     C                   CLEAR                   T1
017900100121     C                   MOVEL     KNSIF         NOMSIF
018000100121     C******             MOVEL     XSCDSI        NOMDIT
018100100121     C                   MOVEL     DSPGM         NOMPGM
018200100121     C*
018300100121     C* Inizializzazione/Decodifica prima videata
018400100121     C                   EXSR      INZD1
018500100121     C                   MOVE      *OFF          $INZD1
018600100121     C*
018700100121     C                   ENDSR
018800100121     C************************************************************
018900100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
019000100121     C************************************************************
019100100121     C     DEFCAM        BEGSR
019200100121     C*
019300100121     C* klist
019400100121     C     k_edstbl2     kLIST
019500100121     C                   kfld                    KEYsegmento       3
019600100121     C                   kfld                    KEYTABella       35
019700100121     C                   kfld                    KEYCliente       35
019800100121      *
019900100121     C     k_tbe01       kLIST
020000100121     C                   kfld                    tbe_cod
020100100121     C                   kfld                    tbe_key1         15
020200100121     C                   kfld                    tbe_key2         15
020300100409      *
020400100409     C     k_tbe011      kLIST
020500100409     C                   kfld                    tbe_cod
020600100409     C                   kfld                    xtakey1
020700100121     C*
020800100121     C* Variabili per gestione videate
020900100121     C*
021000100121     C                   MOVE      *BLANK        $GEST             2
021100100121     C                   MOVE      *BLANK        $FINE             1
021200100121     C                   MOVE      *BLANK        $INZD1            1
021300100121     C                   MOVE      *BLANK        $LASTG            2
021400100121     C                   MOVE      *BLANK        $LASTV            2
021500100121     C*
021600100121     C* Variabili appoggio
021700100121     C                   Z-ADD     0             CURR              2 0
021800100121     C                   Z-ADD     0             CURC              2 0
021900100121     C                   MOVE      *ZEROS        WIN              99
022000100121     C                   MOVE      *BLANK        WTBERR            1
022100100121     C*
022200100121     C* Indici
022300100121     C                   Z-ADD     0             X                 3 0
022400100121     C                   Z-ADD     0             Y                 3 0
022500100121     C*
022600100121     C                   ENDSR
022700100121     C************************************************************
022800940510     C* GESTIONE VIDEO RECORD D1
022900940131     C************************************************************
023000940127     C     GESD1         BEGSR
023100030113      *
023200940225     C* inizializzazione videata
023300940223     C     $INZD1        IFEQ      *ON
023400940127     C                   EXSR      INZD1
023500940223     C                   MOVE      *OFF          $INZD1
023600940117     C                   END
023700030113      *
023800940225     C* emissione piede videata se proveniente da altra
023900940225     C* salvataggio valore formato di provenienza
024000940207     C     $LASTV        IFNE      'D1'
024100940614     C                   WRITE     T1
024200940202     C                   MOVE      $LASTV        $LASTG
024300940202     C                   MOVE      'D1'          $LASTV
024400940202     C                   END
024500940131     C*
024600940203     C                   SELECT
024700100323     C* Inserimento e Copia
024800030114    >C     xtaopz        WHENEQ    '01'
024900100121    >C     xtaopz        OREQ      '03'
025000100323     c                   movel     Opz_ImmissioneD1Fun
025100100323     C* Visualizzazione
025200030114    >C     xtaopz        WHENEQ    '05'
025300100323     c                   movel     Opz_VisualizzaD1Fun
025400030114     C                   SETON                                        020306
025500100323     C* Annulla
025600030114    >C     xtaopz        WHENEQ    '04'
025700100323     c                   movel     Opz_Annulla   D1Fun
025800030114     C                   SETON                                        0203
025900030114     C                   SETOFF                                         06
026000100323     C* Ripristina
026100100323    >C     xtaopz        WHENEQ    '08'
026200100323     c                   movel     OPz_Riattiva  D1Fun
026300100323     C                   SETON                                        02  03
026400100323     C                   SETON                                        06  18
026500940620     C* Manutenzione
026600030113    >C     xtaopz        WHENEQ    '02'
026700100323     c                   movel     Opz_VariazioneD1Fun
026800030114     C                   SETON                                        02
026900030114     C                   SETOFF                                       0306
027000940620     C*
027100940620     C                   OTHER
027200090226     C*
027300030114     C                   SETOFF                                       020306
027400940203     C                   ENDSL
027500030114     C*
027600030114     c                   SETOFF                                         99
027700940620     C* Immissione/Modifica/Copia
027800030113     C*               *----------------*
027900940620     C                   EXFMT     D1
028000030113     C*               *----------------*
028100030114     C*
028200030114     C     *IN99         IFEQ      *OFF
028300030114     C*
0284000301141    C                   SELECT
028500030114     C* F3=Fine
028600030114     C     $TASTO        WHENEQ    F03
028700030114     C                   EXSR      F03D1
028800030114     C* F12=Ritorno
028900030114     C     $TASTO        WHENEQ    F12
029000030114     C                   EXSR      F12D1
029100030114     C*
0292000301141O   C                   OTHER
029300030114     C*
029400030114     C                   EXSR      CTRD1
029500030113     C*
029600940616     C     *IN99         IFEQ      *OFF
029700940616     C* F6
0298000302052    C     $TASTO        ifeq      F06
0299001003232    C     $TASTO        oreq      F18
0300000301152    C     $TASTO        oreq      F23
030100940224     C* eseguo aggiornamento
030200940315    >C                   EXSR      AGGANA
030300940224     C* eseguo operazioni del dopo-aggiornamento
030400030114     C  N99              EXSR      GESAGG
030500030205     c                   end
030600030114     C*
030700030114     C                   ENDIF
030800030114     C*
0309000301142-   C                   ENDSL
031000030114     C*
0311000301142-   C                   ENDIF
031200030114     C*
031300940117     C                   ENDSR
031400940117      ************************************************************
031500940207      * INIZIALIZZAZIONE VIDEATA DATI
031600940117      ************************************************************
031700940127     C     INZD1         BEGSR
031800940131     C*
031900940615     C                   CLEAR                   D1
032000100121     C*
032100100121     c                   clear                   a1FIL             3
032200100409     C                   eval      A1FIL = %Subst(xtaKEY1:1:3)
032300100121     C                   move      A1FIL         d1FIL
032400100409     C                   eval      d1COD = %Subst(xtaKEY1:4:10)
032500100409     c                   if        %Subst(xtaKEY2:1:3) <> *blank
032600100409     C                   eval      A1FIL = %Subst(xtaKEY2:1:3)
032700100121     C                   move      A1FIL         d1FIL1
032800100121     c                   end
032900100121     C*
033000100121     c* Prendo tipo segmento generale
033100100121     c     d1FIL         chain     azorg01l
033200100121     c                   if        %Found(azorg01l)
033300100121     C                   eval      d1dfil = orgDES
033400100121     c                   end
033500940207     C*
033600100121     c* Prendo descrizione del Profilo
033700100121     c     d1cod         chain     kpprf01l
033800100121     c                   if        %Found(kpprf01l)
033900100121     C                   eval      d1dcod = prfTXT
034000100121     c                   end
034100100121     C*
034200100121     C*  utilizzo particolare della tabella x la sola Filiale
034300100121     c                   if        d1FIL1 > 0
034400100121     c     d1FIL1        chain     azorg01l
034500100121     c                   if        %Found(azorg01l)
034600100121     C                   eval      d1dfil1 = orgDES
034700100121     c                   end
034800100121     c                   end
034900100121     C*
035000940224     C* valorizzo la chiave univoca per reperire i dati
035100940224     C* se non immissione
0352000301141   >C     xtaopz        IFNE      '01'
035300940315    >C*-------------------------------------------------------------
035400940315    >C* impostare i campi chiave ricevuti
035500940315    >C*-------------------------------------------------------------
035600100121     C                   exsr      Imposta_KEY
035700100121     C     K_tbe01       chain(N)  tntbe01L
035800100121     C                   IF        %Found(tntbe01L)
035900940224     C* riempio la videata
036000940224     C                   EXSR      RIED1
036100030113      *
036200940224     C* richiamo routine dei ctrl per decodificarla
036300940614    >C                   EXSR      CTRD1
036400030113      *
036500940224     C                   MOVE      *ALL'0'       IN5098
036600940224     C                   MOVEA     IN5098        *IN(50)
036700030113      *
036800940224     C* definizione indicatori su video
036900940224     C                   SETOFF                                       02
0370009402243    C                   SELECT
037100030114    >C     xtaopz        WHENEQ    '02'
037200940224     C                   SETON                                        02
037300030114    >C     xtaopz        WHENEQ    '05'
0374009402243-   C                   ENDSL
037500940224     C*
0376009402242-   C                   ENDIF
037700030114     C*
0378000301141   >C                   Else
037900030114     C* Se in Immissione nuovo record
038000030114     c                   exsr      NewD1
0381009402241-   C                   ENDIF
038200940224     C*
038300940117     C                   ENDSR
038400940207     C/EJECT
038500940207     C************************************************************
038600940510     C* RIEMPIMENTO VIDEATA  D1
038700940207     C************************************************************
038800030114     C     NEWD1         BEGSR
038900940207     C*
039000030114     c                   clear                   d1
039100100325     C*
039200100726     c                   if        xtaopz = '01' and xtakey1 <> *blank
039300100726     c                   eval      a1fil = %Subst(xtakey1:1:3)
039400100726     C                   move      A1FIL         d1FIL
039500100726     c                   eval      d1cod = %Subst(xtakey1:4:10)
039600100726     c                   end
039601100929     C*
039602100929     c                   if        xtaopz = '01'
039603100929     C                   eval      D1AUTMTC  = 'N'
039604100929     C                   eval      D1RIEABI  = 'N'
039605100929     C                   eval      D1AUTSDT  = 'N'
039606100929     C                   eval      D1AUTRET  = 'N'
039607100929     C                   eval      D1AFFMTC  = 'N'
039608100929     C                   eval      D1AUTCON  = 'N'
039609100929     C                   eval      D1AFFSDT  = 'N'
039610100929     C                   eval      D1AUTCRS  = 'N'
039611100929     C                   eval      D1AFFRET  = 'N'
039612100929     C                   eval      D1AFFCON  = 'N'
039613100929     C                   eval      D1AUTVTCC = 'N'
039614100929     C                   eval      D1AFFCRS  = 'N'
039615100929     C                   eval      D1AUTMTCC = 'N'
039616100929     C                   eval      D1AUTSDTC = 'N'
039617100929     C                   eval      D1AUTRETC = 'N'
039618100929     C                   eval      D1AUTCRSC = 'N'
039619100929     C                   eval      D1AUTCONC = 'N'
039620100929     C*
039621100929     C                   eval      D1AFFGVSTD = 'N'
039622100929     C                   eval      D1AFFAGV   = 'N'
039623100929     C                   eval      D1AFFCPSTD = 'N'
039624100929     C                   eval      D1AFFARRM  = 'N'
039625100929     C                   eval      D1AUTCPRRE = 'N'
039626100929     C                   eval      D1AUTGSTM  = 'N'
039627100929     C                   eval      D1AUTSTDCC = 'N'
039628100929     c                   end
039700100322     C*
039800030114     C                   ENDSR
039900030114     C************************************************************
040000030114     C* RIEMPIMENTO VIDEATA  D1
040100030114     C************************************************************
040200030114     C     RIED1         BEGSR
040300030114     C*
040400100121     c                   eval      dAUT = tbeUNI
040500100121     C                   eval      D1AUTMTC  = §AUTMTC
040600100121     C                   eval      D1RIEABI  = §RIEABI
040700100121     C                   eval      D1AUTSDT  = §AUTSDT
040800100121     C                   eval      D1AUTRET  = §AUTRET
040900100121     C                   eval      D1AFFMTC  = §AFFMTC
041000100121     C                   eval      D1AUTCON  = §AUTCON
041100100121     C                   eval      D1AFFSDT  = §AFFSDT
041200100121     C                   eval      D1AUTCRS  = §AUTCRS
041300100121     C                   eval      D1AFFRET  = §AFFRET
041400100121     C                   eval      D1AFFCON  = §AFFCON
041500100121     C                   eval      D1AUTVTCC = §AUTVTCC
041600100121     C                   eval      D1AFFCRS  = §AFFCRS
041700100121     C                   eval      D1AUTMTCC = §AUTMTCC
041800100121     C                   eval      D1AUTSDTC = §AUTSDTC
041900100121     C                   eval      D1AUTRETC = §AUTRETC
042000100121     C                   eval      D1AUTCRSC = §AUTCRSC
042100100121     C                   eval      D1AUTCONC = §AUTCONC
042200100322     C*
042300100322     C                   eval      D1AFFGVSTD = §AFFGVSTD
042400100322     C                   eval      D1AFFAGV   = §AFFAGV
042500100322     C                   eval      D1AFFCPSTD = §AFFCPSTD
042600100322     C                   eval      D1AFFARRM  = §AFFARRM
042700100322     C                   eval      D1AUTCPRRE = §AUTCPRRE
042800100322     C                   eval      D1AUTGSTM  = §AUTGSTM
042900100322     C                   eval      D1AUTSTDCC = §AUTSTDCC
043000100322     C*
043100100121     c                   setoff                                       08
043200100121     c                   if        tbeATB <> *blank
043300100121     c                   eval      d1ann = ' A N N U L L A T O '
043400100121     c                   seton                                        08
043500100121     c                   end
043600030204     C*
043700940207     C                   ENDSR
043800940309     C************************************************************
043900940309     C* GESTIONE F03 VIDEO D1
044000940309     C************************************************************
044100940309     C     F03D1         BEGSR
044200940309     C*
044300940309     C                   MOVE      *ON           $FINE
044400030113    >C                   MOVE      '1'           xtaret
044500940325     C*
044600940325     C                   ENDSR
044700940309     C************************************************************
044800940309     C* GESTIONE F12 VIDEO D1
044900940309     C************************************************************
045000940309     C     F12D1         BEGSR
045100940309     C*
045200940309     C                   MOVE      *ON           $FINE
045300100325    >C     xtaopz        ifeq      '01'
045400100325    >C                   MOVE      '2'           xtaret
045500100325     c                   end
045600940309     C*
045700940309     C                   ENDSR
045800940131     C************************************************************
045900940207     C* CONTROLLO VIDEATA
046000940131     C************************************************************
046100940127     C     CTRD1         BEGSR
046200940131     C*
046300940127     C                   SETOFF                                       99
046400030123      *
046500940127     C* non imposto pos. del cursore
046600940127     C                   Z-ADD     0             H1RIGA
046700940127     C                   Z-ADD     0             H1COLO
046800940621     C*
046900940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0470000301141   >C     xtaopz        IFNE      '04'
047100971110     C*
047200971110     C* controllo codice vuoto
047300100121$017 C     D1fil         IFEQ      *zeros
047400100121$017 C     D1cod         andEQ     *ALL' '
047500100121     C                   SETON                                        515399
047600090226     C                   Else
047700090226     C*** se già presente
047800090226     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
047900090226    >C     xtaopz        IFEQ      '01'
048000090226    >C     xtaopz        oreq      '03'
048100100121     C                   exsr      Imposta_KEY
048200100121     C     K_tbe01       chain(N)  tntbe01L
048300100121     c                   if        %Found(tntbe01L)
048400100121     C                   SETON                                        5499
048500090226     C                   EndIF
048600100121     c                   else
048700100121      *
048800100121      *
048900090226     C                   ENDIF
049000090226     C                   ENDIF
049100030114     C*
049200030114     C* controllo codice vuoto
049300100121$017 C     D1fil         IFEQ      *zero
049400100121     C                   SETON                                        5199
049500100121     C                   Else
049600100121     c* Prendo tipo segmento generale
049700100121     c     d1FIL         chain     azorg01l
049800100121     c                   if        not %Found(azorg01l)
049900100121     C                   SETON                                        5299
050000100121     c                   end
050100100121     C*
050200100121     C                   ENDIF
050300090226     C*
050400100121     C* controllo profilo vuoto
050500100121$017 C     D1cod         ifEQ      *ALL' '
050600100121     C                   SETON                                        5399
050700100121     C                   Else
050800100121     c     d1cod         chain     kpprf01l
050900100121     c                   if        not %Found(kpprf01l)
051000100121     c     d1cod         chain     anutn01l
051100100121     c                   if        not %Found(anutn01l)
051200100121     C                   SETON                                        5599
051300100121     c                   end
051400100121     c                   end
051500100121     C*
051600100121     C                   ENDIF
051700100121     C*
051800100121     C* controllo codice tabella
051900100121    >C     xtaopz        IFne      '01'
052000100121    >C     xtaopz        andNE     '03'
052100100121     C                   exsr      Imposta_KEY
052200100121     C     k_tbe01       chain     tntbe01l
052300100121     c                   if        not %Found(tntbe01l)
052400100121     C**** se sbagliato il codice
052500100121     C                   SETON                                        5599
052600090226     c                   end
052700100121     c                   endIF
052800090226     C                   ENDIF
052900100121      *
053000100121$017 C     D1fil1        IFnE      *zeros
053100100121     c* Prendo tipo segmento generale
053200100121     c     d1FIL1        chain     azorg01l
053300100121     c                   if        not %Found(azorg01l)
053400100121     C                   SETON                                        5699
053500100121     c                   end
053600100121     c                   endIF
053700100325      * -------------
053800100325      *   Controlli sui campi e gruppi di campi
053900100325      * -------------
054000100325      *  se non è stato immesso nulla
054100100325     c                   if        DSP_dsCAMPI = *blank
054200100325     c                   seton                                        616263
054300100325     c                   seton                                        646599
054400100325     C                   ENDIF
054500100325      *
054600100325      *  se una sezione è stata immessa ma non completamente
054700100726    >C     xtaopz        ifne      '04'
054800100325      *    Autisti:
054900100325     c                   If        d1_aut <> *blank
055000100325     c     ' '           scan      d1_aut                                 61
055100100325     c                   if        *in61
055200100325     c                   seton                                            99
055300100325     c                   end
055400100325     c                   endIf
055500100325      *    Coop:
055600100325     c                   If        d1_coop <> *blank
055700100325     c     ' '           scan      d1_coop                                62
055800100325     c                   if        *in62
055900100325     c                   seton                                            99
056000100325     c                   end
056100100325     c                   endIf
056200100325      *    Affl/Defl:
056300100325     c                   If        D1_AFFDEF <> *blank
056400100325     c     ' '           scan      D1_AFFDEF                              63
056500100325     c                   if        *in63
056600100325     c                   seton                                            99
056700100325     c                   end
056800100325     c                   endIf
056900100726     c                   end
057000100325      *    Altri:
057100100325      *
057200100325      * -------------
057300940302     C     *IN99         IFEQ      *ON
057400940510     C     $GEST         ANDEQ     'D1'
057500940117     C                   MOVEA     *IN           WIN
057600940128     C                   MOVE      *ALL'0'       IN5098           49
057700940128     C                   MOVEA     IN5098        *IN(50)
057800940614     C                   WRITE     D1
057900940117     C                   MOVEA     WIN           *IN
058000940302     C                   ENDIF
058100940127     C*
058200940117     C                   ENDSR
058300030204     C/EJECT
058400030204     C************************************************************
058500030204    >C* AGGIORNAMENTO ANAGRAFICA
058600030204     C************************************************************
058700100325    >C     FLDAGG        BEGSR
058800030204     C*
058900030204     C*  campi chiave da scrivere e aggiornare
059000100121     c                   eval      tbecod = 'AUT'
059100100121     c                   clear                   tbeke1
059200100121     c                   clear                   tbeke2
059300100121     c                   move      d1FIL         a1fil
059400100121     c                   eval      %Subst(tbeke1:1:3) = a1fil
059500100121     c                   eval      %Subst(tbeke1:4:10)= d1cod
059600100121     c                   if        d1fil1 > 0
059700100121     c                   move      d1FIL1        a1fil
059800100121     c                   eval      %Subst(tbeke2:1:3) = a1fil
059900100121     c                   end
060000100121     C                   eval      §AUTMTC  =  D1AUTMTC
060100100121     C                   eval      §RIEABI  =  D1RIEABI
060200100121     C                   eval      §AUTSDT  =  D1AUTSDT
060300100121     C                   eval      §AUTRET  =  D1AUTRET
060400100121     C                   eval      §AFFMTC  =  D1AFFMTC
060500100121     C                   eval      §AUTCON  =  D1AUTCON
060600100121     C                   eval      §AFFSDT  =  D1AFFSDT
060700100121     C                   eval      §AUTCRS  =  D1AUTCRS
060800100121     C                   eval      §AFFRET  =  D1AFFRET
060900100121     C                   eval      §AFFCON  =  D1AFFCON
061000100121     C                   eval      §AUTVTCC =  D1AUTVTCC
061100100121     C                   eval      §AFFCRS  =  D1AFFCRS
061200100121     C                   eval      §AUTMTCC =  D1AUTMTCC
061300100121     C                   eval      §AUTSDTC =  D1AUTSDTC
061400100121     C                   eval      §AUTRETC =  D1AUTRETC
061500100121     C                   eval      §AUTCRSC =  D1AUTCRSC
061600100121     C                   eval      §AUTCONC =  D1AUTCONC
061700100322     C*
061800100322     C                   eval       §AFFGVSTD = D1AFFGVSTD
061900100322     C                   eval       §AFFAGV   = D1AFFAGV
062000100322     C                   eval       §AFFCPSTD = D1AFFCPSTD
062100100322     C                   eval       §AFFARRM  = D1AFFARRM
062200100322     C                   eval       §AUTCPRRE = D1AUTCPRRE
062300100322     C                   eval       §AUTGSTM  = D1AUTGSTM
062400100322     C                   eval       §AUTSTDCC = D1AUTSTDCC
062500100322     C*
062600100121     C                   MOVEl     DAut          tbeUNI
062700100121     C*
062800100121     c                   clear                   tbeATB
062900100121     C*
063000030204     C                   ENDSR
063100940207     C/EJECT
063200940224     C************************************************************
063300940315    >C* AGGIORNAMENTO ANAGRAFICA
063400940224     C************************************************************
063500940315    >C     AGGANA        BEGSR
063600940207     C*
063700100408     c                   clear                   aggiorna_FLG      1
063800100408     C*
063900030204     C                   exsr      FLDAGG
064000030204     C*
064100940207     C                   SELECT
064200030204     C*
064300100323      *  Immette/Copia il record
064400030114    >C     xtaopz        WHENEQ    '01'
064500030114    >C     xtaopz        OREQ      '03'
064600030204     C*
064700100408     c                   eval      aggiorna_FLG = 'S'
064800100408     C*
064900090226    >C                   EXSR      RIEANA
065000100324     c                   exsr      wind_trasm
065100100121     c                   write     tntbe000                             59
065200940318     C* se rcd non scrivibile attivo errore
065300030114    >C   59              SETON                                          99
065400030204      *
065500100323      *  Modifica il record
065600030114    >C     xtaopz        WHENEQ    '02'
065700100408     c*
065800100408     c                   eval      aggiorna_FLG = 'S'
065900100408     c*
066000100121     C                   exsr      Imposta_KEY
066100100121     C     K_tbe01       chain     tntbe01L
066200030204     C                   exsr      FLDAGG
066300100324     c                   exsr      wind_trasm
066400090226    >C                   EXSR      RIEANA
066500100121     c                   update    tntbe000                             59
066600030114    >C   59              SETON                                          99
066700030204      *
066800100323      *  Cancella il record
066900030114    >C     xtaopz        WHENEQ    '04'
067000100408     c*
067100100408     c                   eval      aggiorna_FLG = 'S'
067200100408     c*
067300100121     C                   exsr      Imposta_KEY
067400100121     C     K_tbe01       chain     tntbe01L
067500100121     c                   move      'A'           tbeATB
067600100324     c                   exsr      wind_trasm
067700100121     c                   update    tntbe000                             59
067800030114    >C   59              SETON                                          99
067900100323      *
068000100323      *  Riattiva il record
068100100323    >C     xtaopz        WHENEQ    '08'
068200100408     c*
068300100408     c                   eval      aggiorna_FLG = 'S'
068400100408     c*
068500100323     C                   exsr      Imposta_KEY
068600100323     C     K_tbe01       chain     tntbe01L
068700100323     c                   move      ' '           tbeATB
068800100324     c                   exsr      wind_trasm
068900100323     c                   update    tntbe000                             59
069000100323    >C   59              SETON                                          99
069100940621     C*
069200940207     C                   ENDSL
069300940203     C*
069400100408     C* se da verificare o aggiornare su AZUTE la scrittura del flag di controllo
069500100408     C*  per dati presenti sul TNTBE AUT per il profilo selezionato
069600100408     c                   If        aggiorna_FLG = 'S'
069700100408      **
069800100409     c                   Exsr      AGG_AZute
069900100408      **
070000100408     c                   EndIf
070100100408     C*
070200940203     C                   ENDSR
070300940224     C************************************************************
070400940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
070500940224     C************************************************************
070600090226     C     RIEANA        BEGSR
070700030114     C*
070800100121     C                   eval      §AUTMTC  =  D1AUTMTC
070900100121     C                   eval      §RIEABI  =  D1RIEABI
071000100121     C                   eval      §AUTSDT  =  D1AUTSDT
071100100121     C                   eval      §AUTRET  =  D1AUTRET
071200100121     C                   eval      §AFFMTC  =  D1AFFMTC
071300100121     C                   eval      §AUTCON  =  D1AUTCON
071400100121     C                   eval      §AFFSDT  =  D1AFFSDT
071500100121     C                   eval      §AUTCRS  =  D1AUTCRS
071600100121     C                   eval      §AFFRET  =  D1AFFRET
071700100121     C                   eval      §AFFCON  =  D1AFFCON
071800100121     C                   eval      §AUTVTCC =  D1AUTVTCC
071900100121     C                   eval      §AFFCRS  =  D1AFFCRS
072000100121     C                   eval      §AUTMTCC =  D1AUTMTCC
072100100121     C                   eval      §AUTSDTC =  D1AUTSDTC
072200100121     C                   eval      §AUTRETC =  D1AUTRETC
072300100121     C                   eval      §AUTCRSC =  D1AUTCRSC
072400100121     C                   eval      §AUTCONC =  D1AUTCONC
072500100322     C*
072600100322     C                   eval       §AFFGVSTD = D1AFFGVSTD
072700100322     C                   eval       §AFFAGV   = D1AFFAGV
072800100322     C                   eval       §AFFCPSTD = D1AFFCPSTD
072900100322     C                   eval       §AFFARRM  = D1AFFARRM
073000100322     C                   eval       §AUTCPRRE = D1AUTCPRRE
073100100322     C                   eval       §AUTGSTM  = D1AUTGSTM
073200100322     C                   eval       §AUTSTDCC = D1AUTSTDCC
073300100322     C*
073400100121     C                   MOVEl     DAut          tbeUNI
073500100121     C*
073600100324     C                   ENDSR
073700100324     C************************************************************
073800100409     C* Aggiornamento di AZUTE
073900100409     C************************************************************
074000100409     C     AGG_AZute     BEGSR
074100100409     C*
074200100409     c                   clear                   TbeAUT_found      1
074300100409     C*
074400100409     C* controlla esistenza comunque di codici FIL/PROFILO su AUT
074500100409     C*  NON annullati quindi aggiorna AZUTE
074600100409     C*
074700100409     C     K_tbe011      setll     tntbe01L
074800100409     C     K_tbe011      reade     tntbe01L
074900100409     C*
075000100409     C                   Dow       not %Eof(tntbe01l)
075100100409     C*  se ce n'è almeno uno attivo
075200100409     c                   if        tbeATB = ' '
075300100409     c                   move      'S'           TbeAUT_found      1
075400100409     C                   Leave
075500100409     C                   END
075600100409     C*
075700100409     C     K_tbe011      reade     tntbe01L
075800100409     C                   END
075900100409     C*
076000100409     C*  aggancia AZUTE
076100100409     c                   open      azute01l
076200100409     c     d1cod         chain     azute01l
076300100409     c                   if        %Found(azute01l)
076400100409     C*
076500100409     c                   movel     utefaf        dute01
076600100409     C*  imposta il flag
076700100409     c                   if        TbeAUT_found = 'S'
076800100409     c                   eval      §UTEtbeAUT   = 'S'
076900100409     C                   else
077000100409     c                   eval      §UTEtbeAUT   = ' '
077100100409     C                   end
077200100409     C*
077300100409     c                   movel     dute01        utefaf
077400100409     C*  aggiorna AZUTE
077500100409     c                   update    azute000
077600100409     C                   end
077700100409     c                   close     azute01l
077800100409     C*
077900100409     C                   ENDSR
078000100409     C************************************************************
078100100324     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
078200100324     C************************************************************
078300100324     C     Wind_trasm    BEGSR
078400100324     C*
078500100324     C                   mOVEl     tbeFTT        t1trs
078600100728     c     t1trs         ifeq      *blank
078700100728    >C     xtaopz        andeq     '01'
078800100728     C                   mOVEl     'S'           t1trs
078900100728     c                   end
079000100324     C                   z-add     tbeFLT        t1fil
079100100324     c                   exfmt     w1tras
079200100324     C                   MOVEl     t1trs         tbeFTT
079300100324     C                   z-add     t1fil         tbeFLT
079400100324     c                   clear                   tbeFTR
079500100324     c                   clear                   tbeDTR
079600030114     C*
079700940207     C                   ENDSR
079800940224     C************************************************************
079900940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
080000940224     C************************************************************
080100940224     C     GESAGG        BEGSR
080200940224     C*
080300940224     C* segnalo al pgm chiamante l'avvenuta conferma
080400030114    >C                   MOVE      *ON           xtaopr
080500940224     C* segnalo al pgm chiamante l'aver premuto l'F6
080600030114    >C                   MOVE      '0'           xtaret
080700940224     C*
0808009402241    C                   SELECT
080900940224     C* nel caso di immissione, ripristino la videata iniziale
081000030114    >C     xtaopz        WHENEQ    '01'
081100940224     C                   MOVE      *ON           $INZD1
081200030114     C                   MOVE      *ON           $FINE
081300940224     C* altrimenti ritorno al pgm chiamante
081400940224     C                   OTHER
081500940224     C                   MOVE      *ON           $FINE
0816009402241-   C                   ENDSL
081700940224     C*
081800940224     C                   ENDSR
081900940131     C************************************************************
082000100121     C*
082100100121     C************************************************************
082200100121     C     Imposta_KEY   BEGSR
082300100121     C*
082400100121     c                   clear                   tbe_Key1
082500100121     c                   clear                   tbe_Key2
082600100121     c                   move      d1FIL         a1fil
082700100121     c                   eval      %Subst(tbe_key1:1:3) = A1fil
082800100121     c                   eval      %Subst(tbe_key1:4:10)= d1cod
082900100121     c                   if        d1fil1 > 0
083000100121     c                   move      d1FIL1        a1fil
083100100121     c                   eval      %Subst(tbe_key2:1:3) = a1fil
083200100121     c                   end
083300100121     C*
083400100121     C                   ENDSR
