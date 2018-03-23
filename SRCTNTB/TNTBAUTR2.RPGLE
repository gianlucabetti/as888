000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400120906      *
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
004900120906     D  dsp_SKcampi            1     26    DIM(26)
005000100325      *
005100100325     D  D1AUTMTC               1      1
005200100325     D  D1AUTSDT               2      2
005300100325     D  D1AUTRET               3      3
005400100325     D  D1AUTCON               4      4
005500100325     D  D1AUTCRS               5      5
005600100325     D  D1AUTSTDCC             6      6
005700120906     D  D1AUTGFIR              7      7
005800120906     d  D1_AUT                 1      7
005900120906     D  dsp_SKaut              1      7    DIM(7)
006000100325      *
006100120906     D  D1AUTVTCC              8      8
006200120906     D  D1AUTMTCC              9      9
006300120906     D  D1AUTSDTC             10     10
006400120906     D  D1AUTRETC             11     11
006500120906     D  D1AUTCRSC             12     12
006600120906     D  D1AUTCONC             13     13
006700121030     D  D1COPGFIR             14     14
006800121030     d  D1_COOP                8     14
006900121030     D  dsp_SKcoop             8     14    DIM(7)
007000100325      *
007100100726     D  D1AFFMTC              16     16
007200100325     D  D1AFFCON              17     17
007300100325     D  D1AFFCRS              18     18
007400100325     D  D1AFFGVSTD            19     19
007500100325     D  D1AFFAGV              20     20
007600100325     D  D1AFFCPSTD            21     21
007700100325     D  D1AFFARRM             22     22
007800120906     D  D1AFFGFIR             23     23
007900120906     d  D1_AFFDEF             16     23
008000120906     D  dsp_SKaff             16     23    DIM(8)
008100100325      *
008200120906     D  D1RIEABI              24     24
008300120906     D  D1AUTCPRRE            25     25
008400120906     D  D1AUTGFPRE            26     26
008500100325      *
008600940201     D*-------------
008700030204     D  S_TASTO        S                   like($Tasto)
008800940201     D DSFMT           DS
008900940506     D  $TASTO               369    369
009000940201     D  NRG                  370    370
009100940201     D  NCL                  371    371
009200940201     D*-------------
009300940201     D* posizione cursore
009400940201     D CURSOR          DS
009500940223     D  RIRI                   1      2B 0 INZ
009600940201     D  R$$                    2      2
009700940223     D  COCO                   3      4B 0 INZ
009800940201     D  C$$                    4      4
009900940207     D*-------------
010000940207     D* Reperimento nome PGM
010100940207     D STATUS         SDS           333
010200940207     D  DSPGM            *PROC
010300940225     D*-------------
010400940225     D* COSTANTI
010500940225     D*-------------
010600940506     D* Tasti di funzione
010700940506     D F01             C                   CONST(X'31')
010800940506     D F02             C                   CONST(X'32')
010900940506     D F03             C                   CONST(X'33')
011000940506     D F04             C                   CONST(X'34')
011100940506     D F05             C                   CONST(X'35')
011200940506     D F06             C                   CONST(X'36')
011300940506     D F07             C                   CONST(X'37')
011400940506     D F08             C                   CONST(X'38')
011500940506     D F09             C                   CONST(X'39')
011600940506     D F10             C                   CONST(X'3A')
011700940506     D F11             C                   CONST(X'3B')
011800940506     D F12             C                   CONST(X'3C')
011900940506     D F13             C                   CONST(X'B1')
012000940506     D F14             C                   CONST(X'B2')
012100940506     D F15             C                   CONST(X'B3')
012200940506     D F16             C                   CONST(X'B4')
012300940506     D F17             C                   CONST(X'B5')
012400940506     D F18             C                   CONST(X'B6')
012500940506     D F19             C                   CONST(X'B7')
012600940506     D F20             C                   CONST(X'B8')
012700940506     D F21             C                   CONST(X'B9')
012800940506     D F22             C                   CONST(X'BA')
012900940506     D F23             C                   CONST(X'BB')
013000940506     D F24             C                   CONST(X'BC')
013100940506     D ENTER           C                   CONST(X'F1')
013200940506     D ROLDWN          C                   CONST(X'F4')
013300940506     D ROLLUP          C                   CONST(X'F5')
013400940506     D*-------------
013500941108     D DATA            C                   CONST('0001-01-01')
013600030114     D Annull          C                   CONST('  ANNULLATO  ')
013700100323     D OPz_Immissione  C                   CONST('IMMISSIONE')
013800100323     D OPz_Variazione  C                   CONST('VARIAZIONE')
013900100323     D OPz_Annulla     C                   CONST(' ANNULLA  ')
014000100323     D OPz_Visualizza  C                   CONST('VISUALIZZA')
014100100323     D OPz_Riattiva    C                   CONST(' RIATTIVA ')
014200940127     C*----------------------------------------------------*
014300940127     C*                MAIN LINE PROGRAM
014400940127     C*----------------------------------------------------*
014500100121     c                   exsr      INIZIO
014600940223     C*
014700940223     C     $FINE         DOWEQ     *OFF
014800940202     C     $GEST         CASEQ     'D1'          GESD1
014900940117     C                   END
015000940117     C                   END
015100940325     C* fine programma
015200030113     c                   movel     tabds         kpjbu
015300030113     C                   Seton                                        LR
015400940131     C************************************************************
015500100121     C* OPERAZIONI INIZIALI
015600100121     C************************************************************
015700100121     C     INIZIO        BEGSR
015800100121     C*
015900100121     C* Reperimento parametri
016000100121     C     *ENTRY        PLIST
016100100121     C                   PARM                    KPJBA
016200100121      *
016300100121     C                   movel     kpjbu         tabds
016400100121     c                   setoff                                       08
016500100121     C*
016600100121     C* Variabili per gestione videate
016700100121     C                   MOVE      'D1'          $GEST
016800100121     C                   MOVE      *OFF          $FINE
016900100121     C                   MOVE      *ON           $INZD1
017000100121     C                   MOVE      *BLANK        $LASTG
017100100121     C                   MOVE      *BLANK        $LASTV
017200100121     C                   Z-ADD     0             $ULKD1            3 0
017300100121     C                   MOVEL     'AUT'         tbe_COD           3
017400100121     C                   CLEAR                   dAUT
017500100121     C*
017600100121     C* Variabili appoggio
017700100121     C                   Z-ADD     0             CURR
017800100121     C                   Z-ADD     0             CURC
017900100121     C*
018000100121     C* Valorizzazione campi univoci testate
018100100121     C                   CLEAR                   T1
018200100121     C                   MOVEL     KNSIF         NOMSIF
018300100121     C******             MOVEL     XSCDSI        NOMDIT
018400100121     C                   MOVEL     DSPGM         NOMPGM
018500100121     C*
018600100121     C* Inizializzazione/Decodifica prima videata
018700100121     C                   EXSR      INZD1
018800100121     C                   MOVE      *OFF          $INZD1
018900100121     C*
019000100121     C                   ENDSR
019100100121     C************************************************************
019200100121     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
019300100121     C************************************************************
019400100121     C     DEFCAM        BEGSR
019500100121     C*
019600100121     C* klist
019700100121     C     k_edstbl2     kLIST
019800100121     C                   kfld                    KEYsegmento       3
019900100121     C                   kfld                    KEYTABella       35
020000100121     C                   kfld                    KEYCliente       35
020100100121      *
020200100121     C     k_tbe01       kLIST
020300100121     C                   kfld                    tbe_cod
020400100121     C                   kfld                    tbe_key1         15
020500100121     C                   kfld                    tbe_key2         15
020600100409      *
020700100409     C     k_tbe011      kLIST
020800100409     C                   kfld                    tbe_cod
020900100409     C                   kfld                    xtakey1
021000100121     C*
021100100121     C* Variabili per gestione videate
021200100121     C*
021300100121     C                   MOVE      *BLANK        $GEST             2
021400100121     C                   MOVE      *BLANK        $FINE             1
021500100121     C                   MOVE      *BLANK        $INZD1            1
021600100121     C                   MOVE      *BLANK        $LASTG            2
021700100121     C                   MOVE      *BLANK        $LASTV            2
021800100121     C*
021900100121     C* Variabili appoggio
022000100121     C                   Z-ADD     0             CURR              2 0
022100100121     C                   Z-ADD     0             CURC              2 0
022200100121     C                   MOVE      *ZEROS        WIN              99
022300100121     C                   MOVE      *BLANK        WTBERR            1
022400100121     C*
022500100121     C* Indici
022600100121     C                   Z-ADD     0             X                 3 0
022700100121     C                   Z-ADD     0             Y                 3 0
022800100121     C*
022900100121     C                   ENDSR
023000100121     C************************************************************
023100940510     C* GESTIONE VIDEO RECORD D1
023200940131     C************************************************************
023300940127     C     GESD1         BEGSR
023400030113      *
023500940225     C* inizializzazione videata
023600940223     C     $INZD1        IFEQ      *ON
023700940127     C                   EXSR      INZD1
023800940223     C                   MOVE      *OFF          $INZD1
023900940117     C                   END
024000030113      *
024100940225     C* emissione piede videata se proveniente da altra
024200940225     C* salvataggio valore formato di provenienza
024300940207     C     $LASTV        IFNE      'D1'
024400940614     C                   WRITE     T1
024500940202     C                   MOVE      $LASTV        $LASTG
024600940202     C                   MOVE      'D1'          $LASTV
024700940202     C                   END
024800940131     C*
024900940203     C                   SELECT
025000100323     C* Inserimento e Copia
025100030114    >C     xtaopz        WHENEQ    '01'
025200100121    >C     xtaopz        OREQ      '03'
025300100323     c                   movel     Opz_ImmissioneD1Fun
025400100323     C* Visualizzazione
025500030114    >C     xtaopz        WHENEQ    '05'
025600100323     c                   movel     Opz_VisualizzaD1Fun
025700030114     C                   SETON                                        020306
025800100323     C* Annulla
025900030114    >C     xtaopz        WHENEQ    '04'
026000100323     c                   movel     Opz_Annulla   D1Fun
026100030114     C                   SETON                                        0203
026200030114     C                   SETOFF                                         06
026300100323     C* Ripristina
026400100323    >C     xtaopz        WHENEQ    '08'
026500100323     c                   movel     OPz_Riattiva  D1Fun
026600100323     C                   SETON                                        02  03
026700100323     C                   SETON                                        06  18
026800940620     C* Manutenzione
026900030113    >C     xtaopz        WHENEQ    '02'
027000100323     c                   movel     Opz_VariazioneD1Fun
027100030114     C                   SETON                                        02
027200030114     C                   SETOFF                                       0306
027300940620     C*
027400940620     C                   OTHER
027500090226     C*
027600030114     C                   SETOFF                                       020306
027700940203     C                   ENDSL
027800030114     C*
027900030114     c                   SETOFF                                         99
028000940620     C* Immissione/Modifica/Copia
028100030113     C*               *----------------*
028200940620     C                   EXFMT     D1
028300030113     C*               *----------------*
028400030114     C*
028500030114     C     *IN99         IFEQ      *OFF
028600030114     C*
0287000301141    C                   SELECT
028800030114     C* F3=Fine
028900030114     C     $TASTO        WHENEQ    F03
029000030114     C                   EXSR      F03D1
029100030114     C* F12=Ritorno
029200030114     C     $TASTO        WHENEQ    F12
029300030114     C                   EXSR      F12D1
029400030114     C*
0295000301141O   C                   OTHER
029600030114     C*
029700101019     C     $TASTO        ifne      F18
029800030114     C                   EXSR      CTRD1
029900101019     c                   end
030000030113     C*
030100940616     C     *IN99         IFEQ      *OFF
030200940616     C* F6
0303000302052    C     $TASTO        ifeq      F06
0304001003232    C     $TASTO        oreq      F18
0305000301152    C     $TASTO        oreq      F23
030600940224     C* eseguo aggiornamento
030700940315    >C                   EXSR      AGGANA
030800940224     C* eseguo operazioni del dopo-aggiornamento
030900030114     C  N99              EXSR      GESAGG
031000030205     c                   end
031100030114     C*
031200030114     C                   ENDIF
031300030114     C*
0314000301142-   C                   ENDSL
031500030114     C*
0316000301142-   C                   ENDIF
031700030114     C*
031800940117     C                   ENDSR
031900940117      ************************************************************
032000940207      * INIZIALIZZAZIONE VIDEATA DATI
032100940117      ************************************************************
032200940127     C     INZD1         BEGSR
032300940131     C*
032400940615     C                   CLEAR                   D1
032500100121     C*
032600100121     c                   clear                   a1FIL             3
032700100409     C                   eval      A1FIL = %Subst(xtaKEY1:1:3)
032800100121     C                   move      A1FIL         d1FIL
032900100409     C                   eval      d1COD = %Subst(xtaKEY1:4:10)
033000100409     c                   if        %Subst(xtaKEY2:1:3) <> *blank
033100100409     C                   eval      A1FIL = %Subst(xtaKEY2:1:3)
033200100121     C                   move      A1FIL         d1FIL1
033300100121     c                   end
033400100121     C*
033500100121     c* Prendo tipo segmento generale
033600100121     c     d1FIL         chain     azorg01l
033700100121     c                   if        %Found(azorg01l)
033800100121     C                   eval      d1dfil = orgDES
033900100121     c                   end
034000940207     C*
034100100121     c* Prendo descrizione del Profilo
034200100121     c     d1cod         chain     kpprf01l
034300100121     c                   if        %Found(kpprf01l)
034400100121     C                   eval      d1dcod = prfTXT
034500100121     c                   end
034600100121     C*
034700100121     C*  utilizzo particolare della tabella x la sola Filiale
034800100121     c                   if        d1FIL1 > 0
034900100121     c     d1FIL1        chain     azorg01l
035000100121     c                   if        %Found(azorg01l)
035100100121     C                   eval      d1dfil1 = orgDES
035200100121     c                   end
035300100121     c                   end
035400100121     C*
035500940224     C* valorizzo la chiave univoca per reperire i dati
035600940224     C* se non immissione
0357000301141   >C     xtaopz        IFNE      '01'
035800940315    >C*-------------------------------------------------------------
035900940315    >C* impostare i campi chiave ricevuti
036000940315    >C*-------------------------------------------------------------
036100100121     C                   exsr      Imposta_KEY
036200100121     C     K_tbe01       chain(N)  tntbe01L
036300100121     C                   IF        %Found(tntbe01L)
036400940224     C* riempio la videata
036500940224     C                   EXSR      RIED1
036600030113      *
036700940224     C* richiamo routine dei ctrl per decodificarla
036800940614    >C                   EXSR      CTRD1
036900030113      *
037000940224     C                   MOVE      *ALL'0'       IN5098
037100940224     C                   MOVEA     IN5098        *IN(50)
037200030113      *
037300940224     C* definizione indicatori su video
037400940224     C                   SETOFF                                       02
0375009402243    C                   SELECT
037600030114    >C     xtaopz        WHENEQ    '02'
037700940224     C                   SETON                                        02
037800030114    >C     xtaopz        WHENEQ    '05'
0379009402243-   C                   ENDSL
038000940224     C*
0381009402242-   C                   ENDIF
038200030114     C*
0383000301141   >C                   Else
038400030114     C* Se in Immissione nuovo record
038500030114     c                   exsr      NewD1
0386009402241-   C                   ENDIF
038700940224     C*
038800940117     C                   ENDSR
038900940207     C/EJECT
039000940207     C************************************************************
039100940510     C* RIEMPIMENTO VIDEATA  D1
039200940207     C************************************************************
039300030114     C     NEWD1         BEGSR
039400940207     C*
039500030114     c                   clear                   d1
039600100325     C*
039700100726     c                   if        xtaopz = '01' and xtakey1 <> *blank
039800100726     c                   eval      a1fil = %Subst(xtakey1:1:3)
039900100726     C                   move      A1FIL         d1FIL
040000100726     c                   eval      d1cod = %Subst(xtakey1:4:10)
040100100726     c                   end
040200100929     C*
040300100929     c                   if        xtaopz = '01'
040400100929     C                   eval      D1AUTMTC  = 'N'
040500100929     C                   eval      D1RIEABI  = 'N'
040600100929     C                   eval      D1AUTSDT  = 'N'
040700100929     C                   eval      D1AUTRET  = 'N'
040800100929     C                   eval      D1AFFMTC  = 'N'
040900100929     C                   eval      D1AUTCON  = 'N'
041000100929     C                   eval      D1AFFSDT  = 'N'
041100100929     C                   eval      D1AUTCRS  = 'N'
041200100929     C                   eval      D1AFFRET  = 'N'
041300100929     C                   eval      D1AFFCON  = 'N'
041400100929     C                   eval      D1AUTVTCC = 'N'
041500100929     C                   eval      D1AFFCRS  = 'N'
041600100929     C                   eval      D1AUTMTCC = 'N'
041700100929     C                   eval      D1AUTSDTC = 'N'
041800100929     C                   eval      D1AUTRETC = 'N'
041900100929     C                   eval      D1AUTCRSC = 'N'
042000100929     C                   eval      D1AUTCONC = 'N'
042100100929     C*
042200100929     C                   eval      D1AFFGVSTD = 'N'
042300100929     C                   eval      D1AFFAGV   = 'N'
042400100929     C                   eval      D1AFFCPSTD = 'N'
042500100929     C                   eval      D1AFFARRM  = 'N'
042600100929     C                   eval      D1AUTCPRRE = 'N'
042700100929     C                   eval      D1AUTGSTM  = 'N'
042800100929     C                   eval      D1AUTSTDCC = 'N'
042900120906     C*
043000120906     C                   eval      D1AUTgfir  = 'N'
043100120906     C                   eval      D1AFFgfir  = 'N'
043200120906     C                   eval      D1COPgfir  = 'N'
043300120906     C                   eval      D1AUTgfPRE = 'N'
043400100929     c                   end
043500100322     C*
043600030114     C                   ENDSR
043700030114     C************************************************************
043800030114     C* RIEMPIMENTO VIDEATA  D1
043900030114     C************************************************************
044000030114     C     RIED1         BEGSR
044100030114     C*
044200100121     c                   eval      dAUT = tbeUNI
044300100121     C                   eval      D1AUTMTC  = §AUTMTC
044400100121     C                   eval      D1RIEABI  = §RIEABI
044500100121     C                   eval      D1AUTSDT  = §AUTSDT
044600100121     C                   eval      D1AUTRET  = §AUTRET
044700100121     C                   eval      D1AFFMTC  = §AFFMTC
044800100121     C                   eval      D1AUTCON  = §AUTCON
044900100121     C                   eval      D1AFFSDT  = §AFFSDT
045000100121     C                   eval      D1AUTCRS  = §AUTCRS
045100100121     C                   eval      D1AFFRET  = §AFFRET
045200100121     C                   eval      D1AFFCON  = §AFFCON
045300100121     C                   eval      D1AUTVTCC = §AUTVTCC
045400100121     C                   eval      D1AFFCRS  = §AFFCRS
045500100121     C                   eval      D1AUTMTCC = §AUTMTCC
045600100121     C                   eval      D1AUTSDTC = §AUTSDTC
045700100121     C                   eval      D1AUTRETC = §AUTRETC
045800100121     C                   eval      D1AUTCRSC = §AUTCRSC
045900100121     C                   eval      D1AUTCONC = §AUTCONC
046000100322     C*
046100100322     C                   eval      D1AFFGVSTD = §AFFGVSTD
046200100322     C                   eval      D1AFFAGV   = §AFFAGV
046300100322     C                   eval      D1AFFCPSTD = §AFFCPSTD
046400100322     C                   eval      D1AFFARRM  = §AFFARRM
046500100322     C                   eval      D1AUTCPRRE = §AUTCPRRE
046600100322     C                   eval      D1AUTGSTM  = §AUTGSTM
046700100322     C                   eval      D1AUTSTDCC = §AUTSTDCC
046800100322     C*
046900120906     C                   eval      D1AUTgfir  =  §AUTRFCIT
047000120906     C                   eval      D1AFFgfir  =  §AUTRFCOP
047100120906     C                   eval      D1COPgfir  =  §AUTRFAFD
047200120906     C                   eval      D1AUTgfPRE =  §AUTRFPRE
047300100121     c                   setoff                                       08
047400100121     c                   if        tbeATB <> *blank
047500100121     c                   eval      d1ann = ' A N N U L L A T O '
047600100121     c                   seton                                        08
047700100121     c                   end
047800030204     C*
047900940207     C                   ENDSR
048000940309     C************************************************************
048100940309     C* GESTIONE F03 VIDEO D1
048200940309     C************************************************************
048300940309     C     F03D1         BEGSR
048400940309     C*
048500940309     C                   MOVE      *ON           $FINE
048600030113    >C                   MOVE      '1'           xtaret
048700940325     C*
048800940325     C                   ENDSR
048900940309     C************************************************************
049000940309     C* GESTIONE F12 VIDEO D1
049100940309     C************************************************************
049200940309     C     F12D1         BEGSR
049300940309     C*
049400940309     C                   MOVE      *ON           $FINE
049500100325    >C     xtaopz        ifeq      '01'
049600100325    >C                   MOVE      '2'           xtaret
049700100325     c                   end
049800940309     C*
049900940309     C                   ENDSR
050000940131     C************************************************************
050100940207     C* CONTROLLO VIDEATA
050200940131     C************************************************************
050300940127     C     CTRD1         BEGSR
050400940131     C*
050500940127     C                   SETOFF                                       99
050600030123      *
050700940127     C* non imposto pos. del cursore
050800940127     C                   Z-ADD     0             H1RIGA
050900940127     C                   Z-ADD     0             H1COLO
051000940621     C*
051100940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0512000301141   >C     xtaopz        IFNE      '04'
051300971110     C*
051400971110     C* controllo codice vuoto
051500100121$017 C     D1fil         IFEQ      *zeros
051600100121$017 C     D1cod         andEQ     *ALL' '
051700100121     C                   SETON                                        515399
051800090226     C                   Else
051900090226     C*** se già presente
052000090226     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
052100090226    >C     xtaopz        IFEQ      '01'
052200090226    >C     xtaopz        oreq      '03'
052300100121     C                   exsr      Imposta_KEY
052400100121     C     K_tbe01       chain(N)  tntbe01L
052500100121     c                   if        %Found(tntbe01L)
052600100121     C                   SETON                                        5499
052700090226     C                   EndIF
052800100121     c                   else
052900100121      *
053000100121      *
053100090226     C                   ENDIF
053200090226     C                   ENDIF
053300030114     C*
053400030114     C* controllo codice vuoto
053500100121$017 C     D1fil         IFEQ      *zero
053600100121     C                   SETON                                        5199
053700100121     C                   Else
053800100121     c* Prendo tipo segmento generale
053900100121     c     d1FIL         chain     azorg01l
054000100121     c                   if        not %Found(azorg01l)
054100100121     C                   SETON                                        5299
054200100121     c                   end
054300100121     C*
054400100121     C                   ENDIF
054500090226     C*
054600100121     C* controllo profilo vuoto
054700100121$017 C     D1cod         ifEQ      *ALL' '
054800100121     C                   SETON                                        5399
054900100121     C                   Else
055000100121     c     d1cod         chain     kpprf01l
055100100121     c                   if        not %Found(kpprf01l)
055200100121     c     d1cod         chain     anutn01l
055300100121     c                   if        not %Found(anutn01l)
055400100121     C                   SETON                                        5599
055500100121     c                   end
055600100121     c                   end
055700100121     C*
055800100121     C                   ENDIF
055900100121     C*
056000100121     C* controllo codice tabella
056100100121    >C     xtaopz        IFne      '01'
056200100121    >C     xtaopz        andNE     '03'
056300100121     C                   exsr      Imposta_KEY
056400100121     C     k_tbe01       chain     tntbe01l
056500100121     c                   if        not %Found(tntbe01l)
056600100121     C**** se sbagliato il codice
056700100121     C                   SETON                                        5599
056800090226     c                   end
056900100121     c                   endIF
057000090226     C                   ENDIF
057100100121      *
057200100121$017 C     D1fil1        IFnE      *zeros
057300100121     c* Prendo tipo segmento generale
057400100121     c     d1FIL1        chain     azorg01l
057500100121     c                   if        not %Found(azorg01l)
057600100121     C                   SETON                                        5699
057700100121     c                   end
057800100121     c                   endIF
057900100325      * -------------
058000100325      *   Controlli sui campi e gruppi di campi
058100100325      * -------------
058200100325      *  se non è stato immesso nulla
058300100325     c                   if        DSP_dsCAMPI = *blank
058400100325     c                   seton                                        616263
058500100325     c                   seton                                        646599
058600100325     C                   ENDIF
058700100325      *
058800100325      *  se una sezione è stata immessa ma non completamente
058900100726    >C     xtaopz        ifne      '04'
059000100325      *    Autisti:
059100100325     c                   If        d1_aut <> *blank
059200100325     c     ' '           scan      d1_aut                                 61
059300100325     c                   if        *in61
059400100325     c                   seton                                            99
059500100325     c                   end
059600100325     c                   endIf
059700100325      *    Coop:
059800100325     c                   If        d1_coop <> *blank
059900100325     c     ' '           scan      d1_coop                                62
060000100325     c                   if        *in62
060100100325     c                   seton                                            99
060200100325     c                   end
060300100325     c                   endIf
060400100325      *    Affl/Defl:
060500100325     c                   If        D1_AFFDEF <> *blank
060600100325     c     ' '           scan      D1_AFFDEF                              63
060700100325     c                   if        *in63
060800100325     c                   seton                                            99
060900100325     c                   end
061000100325     c                   endIf
061100100726     c                   end
061200100325      *    Altri:
061300100325      *
061400100325      * -------------
061500940302     C     *IN99         IFEQ      *ON
061600940510     C     $GEST         ANDEQ     'D1'
061700940117     C                   MOVEA     *IN           WIN
061800940128     C                   MOVE      *ALL'0'       IN5098           49
061900940128     C                   MOVEA     IN5098        *IN(50)
062000940614     C                   WRITE     D1
062100940117     C                   MOVEA     WIN           *IN
062200940302     C                   ENDIF
062300940127     C*
062400940117     C                   ENDSR
062500030204     C/EJECT
062600030204     C************************************************************
062700030204    >C* AGGIORNAMENTO ANAGRAFICA
062800030204     C************************************************************
062900100325    >C     FLDAGG        BEGSR
063000030204     C*
063100030204     C*  campi chiave da scrivere e aggiornare
063200100121     c                   eval      tbecod = 'AUT'
063300100121     c                   clear                   tbeke1
063400100121     c                   clear                   tbeke2
063500100121     c                   move      d1FIL         a1fil
063600100121     c                   eval      %Subst(tbeke1:1:3) = a1fil
063700100121     c                   eval      %Subst(tbeke1:4:10)= d1cod
063800100121     c                   if        d1fil1 > 0
063900100121     c                   move      d1FIL1        a1fil
064000100121     c                   eval      %Subst(tbeke2:1:3) = a1fil
064100100121     c                   end
064200100121     C                   eval      §AUTMTC  =  D1AUTMTC
064300100121     C                   eval      §RIEABI  =  D1RIEABI
064400100121     C                   eval      §AUTSDT  =  D1AUTSDT
064500100121     C                   eval      §AUTRET  =  D1AUTRET
064600100121     C                   eval      §AFFMTC  =  D1AFFMTC
064700100121     C                   eval      §AUTCON  =  D1AUTCON
064800100121     C                   eval      §AFFSDT  =  D1AFFSDT
064900100121     C                   eval      §AUTCRS  =  D1AUTCRS
065000100121     C                   eval      §AFFRET  =  D1AFFRET
065100100121     C                   eval      §AFFCON  =  D1AFFCON
065200100121     C                   eval      §AUTVTCC =  D1AUTVTCC
065300100121     C                   eval      §AFFCRS  =  D1AFFCRS
065400100121     C                   eval      §AUTMTCC =  D1AUTMTCC
065500100121     C                   eval      §AUTSDTC =  D1AUTSDTC
065600100121     C                   eval      §AUTRETC =  D1AUTRETC
065700100121     C                   eval      §AUTCRSC =  D1AUTCRSC
065800100121     C                   eval      §AUTCONC =  D1AUTCONC
065900100322     C*
066000100322     C                   eval       §AFFGVSTD = D1AFFGVSTD
066100100322     C                   eval       §AFFAGV   = D1AFFAGV
066200100322     C                   eval       §AFFCPSTD = D1AFFCPSTD
066300100322     C                   eval       §AFFARRM  = D1AFFARRM
066400100322     C                   eval       §AUTCPRRE = D1AUTCPRRE
066500100322     C                   eval       §AUTGSTM  = D1AUTGSTM
066600100322     C                   eval       §AUTSTDCC = D1AUTSTDCC
066700120906     C*
066800120906     C                   eval       §AUTRFCIT = D1AUTgfir
066900120906     C                   eval       §AUTRFCOP = D1AFFgfir
067000120906     C                   eval       §AUTRFAFD = D1COPgfir
067100120906     C                   eval       §AUTRFPRE = D1AUTgfPRE
067200100322     C*
067300100121     C                   MOVEl     DAut          tbeUNI
067400100121     C*
067500100121     c                   clear                   tbeATB
067600100121     C*
067700030204     C                   ENDSR
067800940207     C/EJECT
067900940224     C************************************************************
068000940315    >C* AGGIORNAMENTO ANAGRAFICA
068100940224     C************************************************************
068200940315    >C     AGGANA        BEGSR
068300940207     C*
068400100408     c                   clear                   aggiorna_FLG      1
068500100408     C*
068600030204     C                   exsr      FLDAGG
068700030204     C*
068800940207     C                   SELECT
068900030204     C*
069000100323      *  Immette/Copia il record
069100030114    >C     xtaopz        WHENEQ    '01'
069200030114    >C     xtaopz        OREQ      '03'
069300030204     C*
069400100408     c                   eval      aggiorna_FLG = 'S'
069500100408     C*
069600090226    >C                   EXSR      RIEANA
069700100324     c                   exsr      wind_trasm
069800100121     c                   write     tntbe000                             59
069900940318     C* se rcd non scrivibile attivo errore
070000030114    >C   59              SETON                                          99
070100030204      *
070200100323      *  Modifica il record
070300030114    >C     xtaopz        WHENEQ    '02'
070400100408     c*
070500100408     c                   eval      aggiorna_FLG = 'S'
070600100408     c*
070700100121     C                   exsr      Imposta_KEY
070800100121     C     K_tbe01       chain     tntbe01L
070900030204     C                   exsr      FLDAGG
071000100324     c                   exsr      wind_trasm
071100090226    >C                   EXSR      RIEANA
071200100121     c                   update    tntbe000                             59
071300030114    >C   59              SETON                                          99
071400030204      *
071500100323      *  Cancella il record
071600030114    >C     xtaopz        WHENEQ    '04'
071700100408     c*
071800100408     c                   eval      aggiorna_FLG = 'S'
071900100408     c*
072000100121     C                   exsr      Imposta_KEY
072100100121     C     K_tbe01       chain     tntbe01L
072200100121     c                   move      'A'           tbeATB
072300100324     c                   exsr      wind_trasm
072400100121     c                   update    tntbe000                             59
072500030114    >C   59              SETON                                          99
072600100323      *
072700100323      *  Riattiva il record
072800100323    >C     xtaopz        WHENEQ    '08'
072900100408     c*
073000100408     c                   eval      aggiorna_FLG = 'S'
073100100408     c*
073200100323     C                   exsr      Imposta_KEY
073300100323     C     K_tbe01       chain     tntbe01L
073400100323     c                   move      ' '           tbeATB
073500100324     c                   exsr      wind_trasm
073600100323     c                   update    tntbe000                             59
073700100323    >C   59              SETON                                          99
073800101019     c  n99              setoff                                       0203
073900101019     C  n99              MOVE      *On           $INZD1
074000940621     C*
074100940207     C                   ENDSL
074200940203     C*
074300100408     C* se da verificare o aggiornare su AZUTE la scrittura del flag di controllo
074400100408     C*  per dati presenti sul TNTBE AUT per il profilo selezionato
074500100408     c                   If        aggiorna_FLG = 'S'
074600100408      **
074700100409     c                   Exsr      AGG_AZute
074800100408      **
074900100408     c                   EndIf
075000100408     C*
075100940203     C                   ENDSR
075200940224     C************************************************************
075300940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
075400940224     C************************************************************
075500090226     C     RIEANA        BEGSR
075600030114     C*
075700100121     C                   eval      §AUTMTC  =  D1AUTMTC
075800100121     C                   eval      §RIEABI  =  D1RIEABI
075900100121     C                   eval      §AUTSDT  =  D1AUTSDT
076000100121     C                   eval      §AUTRET  =  D1AUTRET
076100100121     C                   eval      §AFFMTC  =  D1AFFMTC
076200100121     C                   eval      §AUTCON  =  D1AUTCON
076300100121     C                   eval      §AFFSDT  =  D1AFFSDT
076400100121     C                   eval      §AUTCRS  =  D1AUTCRS
076500100121     C                   eval      §AFFRET  =  D1AFFRET
076600100121     C                   eval      §AFFCON  =  D1AFFCON
076700100121     C                   eval      §AUTVTCC =  D1AUTVTCC
076800100121     C                   eval      §AFFCRS  =  D1AFFCRS
076900100121     C                   eval      §AUTMTCC =  D1AUTMTCC
077000100121     C                   eval      §AUTSDTC =  D1AUTSDTC
077100100121     C                   eval      §AUTRETC =  D1AUTRETC
077200100121     C                   eval      §AUTCRSC =  D1AUTCRSC
077300100121     C                   eval      §AUTCONC =  D1AUTCONC
077400100322     C*
077500100322     C                   eval       §AFFGVSTD = D1AFFGVSTD
077600100322     C                   eval       §AFFAGV   = D1AFFAGV
077700100322     C                   eval       §AFFCPSTD = D1AFFCPSTD
077800100322     C                   eval       §AFFARRM  = D1AFFARRM
077900100322     C                   eval       §AUTCPRRE = D1AUTCPRRE
078000100322     C                   eval       §AUTGSTM  = D1AUTGSTM
078100100322     C                   eval       §AUTSTDCC = D1AUTSTDCC
078200120906     C*
078300120906     C                   eval       §AUTRFCIT = D1AUTgfir
078400120906     C                   eval       §AUTRFCOP = D1AFFgfir
078500120906     C                   eval       §AUTRFAFD = D1COPgfir
078600120906     C                   eval       §AUTRFPRE = D1AUTgfPRE
078700100322     C*
078800100121     C                   MOVEl     DAut          tbeUNI
078900100121     C*
079000100324     C                   ENDSR
079100100324     C************************************************************
079200100409     C* Aggiornamento di AZUTE
079300100409     C************************************************************
079400100409     C     AGG_AZute     BEGSR
079500100409     C*
079600100409     c                   clear                   TbeAUT_found      1
079700100409     C*
079800100409     C* controlla esistenza comunque di codici FIL/PROFILO su AUT
079900100409     C*  NON annullati quindi aggiorna AZUTE
080000100409     C*
080100100409     C     K_tbe011      setll     tntbe01L
080200100409     C     K_tbe011      reade     tntbe01L
080300100409     C*
080400100409     C                   Dow       not %Eof(tntbe01l)
080500100409     C*  se ce n'è almeno uno attivo
080600100409     c                   if        tbeATB = ' '
080700100409     c                   move      'S'           TbeAUT_found      1
080800100409     C                   Leave
080900100409     C                   END
081000100409     C*
081100100409     C     K_tbe011      reade     tntbe01L
081200100409     C                   END
081300100409     C*
081400100409     C*  aggancia AZUTE
081500100409     c                   open      azute01l
081600100409     c     d1cod         chain     azute01l
081700100409     c                   if        %Found(azute01l)
081800100409     C*
081900100409     c                   movel     utefaf        dute01
082000100409     C*  imposta il flag
082100100409     c                   if        TbeAUT_found = 'S'
082200100409     c                   eval      §UTEtbeAUT   = 'S'
082300100409     C                   else
082400100409     c                   eval      §UTEtbeAUT   = ' '
082500100409     C                   end
082600100409     C*
082700100409     c                   movel     dute01        utefaf
082800100409     C*  aggiorna AZUTE
082900100409     c                   update    azute000
083000100409     C                   end
083100100409     c                   close     azute01l
083200100409     C*
083300100409     C                   ENDSR
083400100409     C************************************************************
083500100324     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
083600100324     C************************************************************
083700100324     C     Wind_trasm    BEGSR
083800100324     C*
083900100324     C                   mOVEl     tbeFTT        t1trs
084000100728     c     t1trs         ifeq      *blank
084100100728    >C     xtaopz        andeq     '01'
084200100728     C                   mOVEl     'S'           t1trs
084300100728     c                   end
084400100324     C                   z-add     tbeFLT        t1fil
084500100324     c                   exfmt     w1tras
084600100324     C                   MOVEl     t1trs         tbeFTT
084700100324     C                   z-add     t1fil         tbeFLT
084800100324     c                   clear                   tbeFTR
084900100324     c                   clear                   tbeDTR
085000030114     C*
085100940207     C                   ENDSR
085200940224     C************************************************************
085300940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
085400940224     C************************************************************
085500940224     C     GESAGG        BEGSR
085600940224     C*
085700940224     C* segnalo al pgm chiamante l'avvenuta conferma
085800030114    >C                   MOVE      *ON           xtaopr
085900940224     C* segnalo al pgm chiamante l'aver premuto l'F6
086000030114    >C                   MOVE      '0'           xtaret
086100940224     C*
0862009402241    C                   SELECT
086300940224     C* nel caso di immissione, ripristino la videata iniziale
086400030114    >C     xtaopz        WHENEQ    '01'
086500940224     C                   MOVE      *ON           $INZD1
086600030114     C                   MOVE      *ON           $FINE
086700940224     C* altrimenti ritorno al pgm chiamante
086800940224     C                   OTHER
086900101019      *
087000101019      * Solo se non ripristinato il record
087100101019     C     xtaopz        ifeq      '08'
087200101019     c                   eval       xtaopz = '02'
087300101019     c                   else
087400940224     C                   MOVE      *ON           $FINE
087500101019     c                   end
087600101019     C*
0877009402241-   C                   ENDSL
087800940224     C*
087900940224     C                   ENDSR
088000940131     C************************************************************
088100100121     C*
088200100121     C************************************************************
088300100121     C     Imposta_KEY   BEGSR
088400100121     C*
088500100121     c                   clear                   tbe_Key1
088600100121     c                   clear                   tbe_Key2
088700100121     c                   move      d1FIL         a1fil
088800100121     c                   eval      %Subst(tbe_key1:1:3) = A1fil
088900100121     c                   eval      %Subst(tbe_key1:4:10)= d1cod
089000100121     c                   if        d1fil1 > 0
089100100121     c                   move      d1FIL1        a1fil
089200100121     c                   eval      %Subst(tbe_key2:1:3) = a1fil
089300100121     c                   end
089400100121     C*
089500100121     C                   ENDSR
