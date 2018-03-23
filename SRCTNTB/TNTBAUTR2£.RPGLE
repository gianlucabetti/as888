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
029400101019     C     $TASTO        ifne      F18
029500030114     C                   EXSR      CTRD1
029600101019     c                   end
029700030113     C*
029800940616     C     *IN99         IFEQ      *OFF
029900940616     C* F6
0300000302052    C     $TASTO        ifeq      F06
0301001003232    C     $TASTO        oreq      F18
0302000301152    C     $TASTO        oreq      F23
030300940224     C* eseguo aggiornamento
030400940315    >C                   EXSR      AGGANA
030500940224     C* eseguo operazioni del dopo-aggiornamento
030600030114     C  N99              EXSR      GESAGG
030700030205     c                   end
030800030114     C*
030900030114     C                   ENDIF
031000030114     C*
0311000301142-   C                   ENDSL
031200030114     C*
0313000301142-   C                   ENDIF
031400030114     C*
031500940117     C                   ENDSR
031600940117      ************************************************************
031700940207      * INIZIALIZZAZIONE VIDEATA DATI
031800940117      ************************************************************
031900940127     C     INZD1         BEGSR
032000940131     C*
032100940615     C                   CLEAR                   D1
032200100121     C*
032300100121     c                   clear                   a1FIL             3
032400100409     C                   eval      A1FIL = %Subst(xtaKEY1:1:3)
032500100121     C                   move      A1FIL         d1FIL
032600100409     C                   eval      d1COD = %Subst(xtaKEY1:4:10)
032700100409     c                   if        %Subst(xtaKEY2:1:3) <> *blank
032800100409     C                   eval      A1FIL = %Subst(xtaKEY2:1:3)
032900100121     C                   move      A1FIL         d1FIL1
033000100121     c                   end
033100100121     C*
033200100121     c* Prendo tipo segmento generale
033300100121     c     d1FIL         chain     azorg01l
033400100121     c                   if        %Found(azorg01l)
033500100121     C                   eval      d1dfil = orgDES
033600100121     c                   end
033700940207     C*
033800100121     c* Prendo descrizione del Profilo
033900100121     c     d1cod         chain     kpprf01l
034000100121     c                   if        %Found(kpprf01l)
034100100121     C                   eval      d1dcod = prfTXT
034200100121     c                   end
034300100121     C*
034400100121     C*  utilizzo particolare della tabella x la sola Filiale
034500100121     c                   if        d1FIL1 > 0
034600100121     c     d1FIL1        chain     azorg01l
034700100121     c                   if        %Found(azorg01l)
034800100121     C                   eval      d1dfil1 = orgDES
034900100121     c                   end
035000100121     c                   end
035100100121     C*
035200940224     C* valorizzo la chiave univoca per reperire i dati
035300940224     C* se non immissione
0354000301141   >C     xtaopz        IFNE      '01'
035500940315    >C*-------------------------------------------------------------
035600940315    >C* impostare i campi chiave ricevuti
035700940315    >C*-------------------------------------------------------------
035800100121     C                   exsr      Imposta_KEY
035900100121     C     K_tbe01       chain(N)  tntbe01L
036000100121     C                   IF        %Found(tntbe01L)
036100940224     C* riempio la videata
036200940224     C                   EXSR      RIED1
036300030113      *
036400940224     C* richiamo routine dei ctrl per decodificarla
036500940614    >C                   EXSR      CTRD1
036600030113      *
036700940224     C                   MOVE      *ALL'0'       IN5098
036800940224     C                   MOVEA     IN5098        *IN(50)
036900030113      *
037000940224     C* definizione indicatori su video
037100940224     C                   SETOFF                                       02
0372009402243    C                   SELECT
037300030114    >C     xtaopz        WHENEQ    '02'
037400940224     C                   SETON                                        02
037500030114    >C     xtaopz        WHENEQ    '05'
0376009402243-   C                   ENDSL
037700940224     C*
0378009402242-   C                   ENDIF
037900030114     C*
0380000301141   >C                   Else
038100030114     C* Se in Immissione nuovo record
038200030114     c                   exsr      NewD1
0383009402241-   C                   ENDIF
038400940224     C*
038500940117     C                   ENDSR
038600940207     C/EJECT
038700940207     C************************************************************
038800940510     C* RIEMPIMENTO VIDEATA  D1
038900940207     C************************************************************
039000030114     C     NEWD1         BEGSR
039100940207     C*
039200030114     c                   clear                   d1
039300100325     C*
039400100726     c                   if        xtaopz = '01' and xtakey1 <> *blank
039500100726     c                   eval      a1fil = %Subst(xtakey1:1:3)
039600100726     C                   move      A1FIL         d1FIL
039700100726     c                   eval      d1cod = %Subst(xtakey1:4:10)
039800100726     c                   end
039900100929     C*
040000100929     c                   if        xtaopz = '01'
040100100929     C                   eval      D1AUTMTC  = 'N'
040200100929     C                   eval      D1RIEABI  = 'N'
040300100929     C                   eval      D1AUTSDT  = 'N'
040400100929     C                   eval      D1AUTRET  = 'N'
040500100929     C                   eval      D1AFFMTC  = 'N'
040600100929     C                   eval      D1AUTCON  = 'N'
040700100929     C                   eval      D1AFFSDT  = 'N'
040800100929     C                   eval      D1AUTCRS  = 'N'
040900100929     C                   eval      D1AFFRET  = 'N'
041000100929     C                   eval      D1AFFCON  = 'N'
041100100929     C                   eval      D1AUTVTCC = 'N'
041200100929     C                   eval      D1AFFCRS  = 'N'
041300100929     C                   eval      D1AUTMTCC = 'N'
041400100929     C                   eval      D1AUTSDTC = 'N'
041500100929     C                   eval      D1AUTRETC = 'N'
041600100929     C                   eval      D1AUTCRSC = 'N'
041700100929     C                   eval      D1AUTCONC = 'N'
041800100929     C*
041900100929     C                   eval      D1AFFGVSTD = 'N'
042000100929     C                   eval      D1AFFAGV   = 'N'
042100100929     C                   eval      D1AFFCPSTD = 'N'
042200100929     C                   eval      D1AFFARRM  = 'N'
042300100929     C                   eval      D1AUTCPRRE = 'N'
042400100929     C                   eval      D1AUTGSTM  = 'N'
042500100929     C                   eval      D1AUTSTDCC = 'N'
042600100929     c                   end
042700100322     C*
042800030114     C                   ENDSR
042900030114     C************************************************************
043000030114     C* RIEMPIMENTO VIDEATA  D1
043100030114     C************************************************************
043200030114     C     RIED1         BEGSR
043300030114     C*
043400100121     c                   eval      dAUT = tbeUNI
043500100121     C                   eval      D1AUTMTC  = §AUTMTC
043600100121     C                   eval      D1RIEABI  = §RIEABI
043700100121     C                   eval      D1AUTSDT  = §AUTSDT
043800100121     C                   eval      D1AUTRET  = §AUTRET
043900100121     C                   eval      D1AFFMTC  = §AFFMTC
044000100121     C                   eval      D1AUTCON  = §AUTCON
044100100121     C                   eval      D1AFFSDT  = §AFFSDT
044200100121     C                   eval      D1AUTCRS  = §AUTCRS
044300100121     C                   eval      D1AFFRET  = §AFFRET
044400100121     C                   eval      D1AFFCON  = §AFFCON
044500100121     C                   eval      D1AUTVTCC = §AUTVTCC
044600100121     C                   eval      D1AFFCRS  = §AFFCRS
044700100121     C                   eval      D1AUTMTCC = §AUTMTCC
044800100121     C                   eval      D1AUTSDTC = §AUTSDTC
044900100121     C                   eval      D1AUTRETC = §AUTRETC
045000100121     C                   eval      D1AUTCRSC = §AUTCRSC
045100100121     C                   eval      D1AUTCONC = §AUTCONC
045200100322     C*
045300100322     C                   eval      D1AFFGVSTD = §AFFGVSTD
045400100322     C                   eval      D1AFFAGV   = §AFFAGV
045500100322     C                   eval      D1AFFCPSTD = §AFFCPSTD
045600100322     C                   eval      D1AFFARRM  = §AFFARRM
045700100322     C                   eval      D1AUTCPRRE = §AUTCPRRE
045800100322     C                   eval      D1AUTGSTM  = §AUTGSTM
045900100322     C                   eval      D1AUTSTDCC = §AUTSTDCC
046000100322     C*
046100100121     c                   setoff                                       08
046200100121     c                   if        tbeATB <> *blank
046300100121     c                   eval      d1ann = ' A N N U L L A T O '
046400100121     c                   seton                                        08
046500100121     c                   end
046600030204     C*
046700940207     C                   ENDSR
046800940309     C************************************************************
046900940309     C* GESTIONE F03 VIDEO D1
047000940309     C************************************************************
047100940309     C     F03D1         BEGSR
047200940309     C*
047300940309     C                   MOVE      *ON           $FINE
047400030113    >C                   MOVE      '1'           xtaret
047500940325     C*
047600940325     C                   ENDSR
047700940309     C************************************************************
047800940309     C* GESTIONE F12 VIDEO D1
047900940309     C************************************************************
048000940309     C     F12D1         BEGSR
048100940309     C*
048200940309     C                   MOVE      *ON           $FINE
048300100325    >C     xtaopz        ifeq      '01'
048400100325    >C                   MOVE      '2'           xtaret
048500100325     c                   end
048600940309     C*
048700940309     C                   ENDSR
048800940131     C************************************************************
048900940207     C* CONTROLLO VIDEATA
049000940131     C************************************************************
049100940127     C     CTRD1         BEGSR
049200940131     C*
049300940127     C                   SETOFF                                       99
049400030123      *
049500940127     C* non imposto pos. del cursore
049600940127     C                   Z-ADD     0             H1RIGA
049700940127     C                   Z-ADD     0             H1COLO
049800940621     C*
049900940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0500000301141   >C     xtaopz        IFNE      '04'
050100971110     C*
050200971110     C* controllo codice vuoto
050300100121$017 C     D1fil         IFEQ      *zeros
050400100121$017 C     D1cod         andEQ     *ALL' '
050500100121     C                   SETON                                        515399
050600090226     C                   Else
050700090226     C*** se già presente
050800090226     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
050900090226    >C     xtaopz        IFEQ      '01'
051000090226    >C     xtaopz        oreq      '03'
051100100121     C                   exsr      Imposta_KEY
051200100121     C     K_tbe01       chain(N)  tntbe01L
051300100121     c                   if        %Found(tntbe01L)
051400100121     C                   SETON                                        5499
051500090226     C                   EndIF
051600100121     c                   else
051700100121      *
051800100121      *
051900090226     C                   ENDIF
052000090226     C                   ENDIF
052100030114     C*
052200030114     C* controllo codice vuoto
052300100121$017 C     D1fil         IFEQ      *zero
052400100121     C                   SETON                                        5199
052500100121     C                   Else
052600100121     c* Prendo tipo segmento generale
052700100121     c     d1FIL         chain     azorg01l
052800100121     c                   if        not %Found(azorg01l)
052900100121     C                   SETON                                        5299
053000100121     c                   end
053100100121     C*
053200100121     C                   ENDIF
053300090226     C*
053400100121     C* controllo profilo vuoto
053500100121$017 C     D1cod         ifEQ      *ALL' '
053600100121     C                   SETON                                        5399
053700100121     C                   Else
053800100121     c     d1cod         chain     kpprf01l
053900100121     c                   if        not %Found(kpprf01l)
054000100121     c     d1cod         chain     anutn01l
054100100121     c                   if        not %Found(anutn01l)
054200100121     C                   SETON                                        5599
054300100121     c                   end
054400100121     c                   end
054500100121     C*
054600100121     C                   ENDIF
054700100121     C*
054800100121     C* controllo codice tabella
054900100121    >C     xtaopz        IFne      '01'
055000100121    >C     xtaopz        andNE     '03'
055100100121     C                   exsr      Imposta_KEY
055200100121     C     k_tbe01       chain     tntbe01l
055300100121     c                   if        not %Found(tntbe01l)
055400100121     C**** se sbagliato il codice
055500100121     C                   SETON                                        5599
055600090226     c                   end
055700100121     c                   endIF
055800090226     C                   ENDIF
055900100121      *
056000100121$017 C     D1fil1        IFnE      *zeros
056100100121     c* Prendo tipo segmento generale
056200100121     c     d1FIL1        chain     azorg01l
056300100121     c                   if        not %Found(azorg01l)
056400100121     C                   SETON                                        5699
056500100121     c                   end
056600100121     c                   endIF
056700100325      * -------------
056800100325      *   Controlli sui campi e gruppi di campi
056900100325      * -------------
057000100325      *  se non è stato immesso nulla
057100100325     c                   if        DSP_dsCAMPI = *blank
057200100325     c                   seton                                        616263
057300100325     c                   seton                                        646599
057400100325     C                   ENDIF
057500100325      *
057600100325      *  se una sezione è stata immessa ma non completamente
057700100726    >C     xtaopz        ifne      '04'
057800100325      *    Autisti:
057900100325     c                   If        d1_aut <> *blank
058000100325     c     ' '           scan      d1_aut                                 61
058100100325     c                   if        *in61
058200100325     c                   seton                                            99
058300100325     c                   end
058400100325     c                   endIf
058500100325      *    Coop:
058600100325     c                   If        d1_coop <> *blank
058700100325     c     ' '           scan      d1_coop                                62
058800100325     c                   if        *in62
058900100325     c                   seton                                            99
059000100325     c                   end
059100100325     c                   endIf
059200100325      *    Affl/Defl:
059300100325     c                   If        D1_AFFDEF <> *blank
059400100325     c     ' '           scan      D1_AFFDEF                              63
059500100325     c                   if        *in63
059600100325     c                   seton                                            99
059700100325     c                   end
059800100325     c                   endIf
059900100726     c                   end
060000100325      *    Altri:
060100100325      *
060200100325      * -------------
060300940302     C     *IN99         IFEQ      *ON
060400940510     C     $GEST         ANDEQ     'D1'
060500940117     C                   MOVEA     *IN           WIN
060600940128     C                   MOVE      *ALL'0'       IN5098           49
060700940128     C                   MOVEA     IN5098        *IN(50)
060800940614     C                   WRITE     D1
060900940117     C                   MOVEA     WIN           *IN
061000940302     C                   ENDIF
061100940127     C*
061200940117     C                   ENDSR
061300030204     C/EJECT
061400030204     C************************************************************
061500030204    >C* AGGIORNAMENTO ANAGRAFICA
061600030204     C************************************************************
061700100325    >C     FLDAGG        BEGSR
061800030204     C*
061900030204     C*  campi chiave da scrivere e aggiornare
062000100121     c                   eval      tbecod = 'AUT'
062100100121     c                   clear                   tbeke1
062200100121     c                   clear                   tbeke2
062300100121     c                   move      d1FIL         a1fil
062400100121     c                   eval      %Subst(tbeke1:1:3) = a1fil
062500100121     c                   eval      %Subst(tbeke1:4:10)= d1cod
062600100121     c                   if        d1fil1 > 0
062700100121     c                   move      d1FIL1        a1fil
062800100121     c                   eval      %Subst(tbeke2:1:3) = a1fil
062900100121     c                   end
063000100121     C                   eval      §AUTMTC  =  D1AUTMTC
063100100121     C                   eval      §RIEABI  =  D1RIEABI
063200100121     C                   eval      §AUTSDT  =  D1AUTSDT
063300100121     C                   eval      §AUTRET  =  D1AUTRET
063400100121     C                   eval      §AFFMTC  =  D1AFFMTC
063500100121     C                   eval      §AUTCON  =  D1AUTCON
063600100121     C                   eval      §AFFSDT  =  D1AFFSDT
063700100121     C                   eval      §AUTCRS  =  D1AUTCRS
063800100121     C                   eval      §AFFRET  =  D1AFFRET
063900100121     C                   eval      §AFFCON  =  D1AFFCON
064000100121     C                   eval      §AUTVTCC =  D1AUTVTCC
064100100121     C                   eval      §AFFCRS  =  D1AFFCRS
064200100121     C                   eval      §AUTMTCC =  D1AUTMTCC
064300100121     C                   eval      §AUTSDTC =  D1AUTSDTC
064400100121     C                   eval      §AUTRETC =  D1AUTRETC
064500100121     C                   eval      §AUTCRSC =  D1AUTCRSC
064600100121     C                   eval      §AUTCONC =  D1AUTCONC
064700100322     C*
064800100322     C                   eval       §AFFGVSTD = D1AFFGVSTD
064900100322     C                   eval       §AFFAGV   = D1AFFAGV
065000100322     C                   eval       §AFFCPSTD = D1AFFCPSTD
065100100322     C                   eval       §AFFARRM  = D1AFFARRM
065200100322     C                   eval       §AUTCPRRE = D1AUTCPRRE
065300100322     C                   eval       §AUTGSTM  = D1AUTGSTM
065400100322     C                   eval       §AUTSTDCC = D1AUTSTDCC
065500100322     C*
065600100121     C                   MOVEl     DAut          tbeUNI
065700100121     C*
065800100121     c                   clear                   tbeATB
065900100121     C*
066000030204     C                   ENDSR
066100940207     C/EJECT
066200940224     C************************************************************
066300940315    >C* AGGIORNAMENTO ANAGRAFICA
066400940224     C************************************************************
066500940315    >C     AGGANA        BEGSR
066600940207     C*
066700100408     c                   clear                   aggiorna_FLG      1
066800100408     C*
066900030204     C                   exsr      FLDAGG
067000030204     C*
067100940207     C                   SELECT
067200030204     C*
067300100323      *  Immette/Copia il record
067400030114    >C     xtaopz        WHENEQ    '01'
067500030114    >C     xtaopz        OREQ      '03'
067600030204     C*
067700100408     c                   eval      aggiorna_FLG = 'S'
067800100408     C*
067900090226    >C                   EXSR      RIEANA
068000100324     c                   exsr      wind_trasm
068100100121     c                   write     tntbe000                             59
068200940318     C* se rcd non scrivibile attivo errore
068300030114    >C   59              SETON                                          99
068400030204      *
068500100323      *  Modifica il record
068600030114    >C     xtaopz        WHENEQ    '02'
068700100408     c*
068800100408     c                   eval      aggiorna_FLG = 'S'
068900100408     c*
069000100121     C                   exsr      Imposta_KEY
069100100121     C     K_tbe01       chain     tntbe01L
069200030204     C                   exsr      FLDAGG
069300100324     c                   exsr      wind_trasm
069400090226    >C                   EXSR      RIEANA
069500100121     c                   update    tntbe000                             59
069600030114    >C   59              SETON                                          99
069700030204      *
069800100323      *  Cancella il record
069900030114    >C     xtaopz        WHENEQ    '04'
070000100408     c*
070100100408     c                   eval      aggiorna_FLG = 'S'
070200100408     c*
070300100121     C                   exsr      Imposta_KEY
070400100121     C     K_tbe01       chain     tntbe01L
070500100121     c                   move      'A'           tbeATB
070600100324     c                   exsr      wind_trasm
070700100121     c                   update    tntbe000                             59
070800030114    >C   59              SETON                                          99
070900100323      *
071000100323      *  Riattiva il record
071100100323    >C     xtaopz        WHENEQ    '08'
071200100408     c*
071300100408     c                   eval      aggiorna_FLG = 'S'
071400100408     c*
071500100323     C                   exsr      Imposta_KEY
071600100323     C     K_tbe01       chain     tntbe01L
071700100323     c                   move      ' '           tbeATB
071800100324     c                   exsr      wind_trasm
071900100323     c                   update    tntbe000                             59
072000100323    >C   59              SETON                                          99
072100101019     c  n99              setoff                                       0203
072200101019     C  n99              MOVE      *On           $INZD1
072300940621     C*
072400940207     C                   ENDSL
072500940203     C*
072600100408     C* se da verificare o aggiornare su AZUTE la scrittura del flag di controllo
072700100408     C*  per dati presenti sul TNTBE AUT per il profilo selezionato
072800100408     c                   If        aggiorna_FLG = 'S'
072900100408      **
073000100409     c                   Exsr      AGG_AZute
073100100408      **
073200100408     c                   EndIf
073300100408     C*
073400940203     C                   ENDSR
073500940224     C************************************************************
073600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
073700940224     C************************************************************
073800090226     C     RIEANA        BEGSR
073900030114     C*
074000100121     C                   eval      §AUTMTC  =  D1AUTMTC
074100100121     C                   eval      §RIEABI  =  D1RIEABI
074200100121     C                   eval      §AUTSDT  =  D1AUTSDT
074300100121     C                   eval      §AUTRET  =  D1AUTRET
074400100121     C                   eval      §AFFMTC  =  D1AFFMTC
074500100121     C                   eval      §AUTCON  =  D1AUTCON
074600100121     C                   eval      §AFFSDT  =  D1AFFSDT
074700100121     C                   eval      §AUTCRS  =  D1AUTCRS
074800100121     C                   eval      §AFFRET  =  D1AFFRET
074900100121     C                   eval      §AFFCON  =  D1AFFCON
075000100121     C                   eval      §AUTVTCC =  D1AUTVTCC
075100100121     C                   eval      §AFFCRS  =  D1AFFCRS
075200100121     C                   eval      §AUTMTCC =  D1AUTMTCC
075300100121     C                   eval      §AUTSDTC =  D1AUTSDTC
075400100121     C                   eval      §AUTRETC =  D1AUTRETC
075500100121     C                   eval      §AUTCRSC =  D1AUTCRSC
075600100121     C                   eval      §AUTCONC =  D1AUTCONC
075700100322     C*
075800100322     C                   eval       §AFFGVSTD = D1AFFGVSTD
075900100322     C                   eval       §AFFAGV   = D1AFFAGV
076000100322     C                   eval       §AFFCPSTD = D1AFFCPSTD
076100100322     C                   eval       §AFFARRM  = D1AFFARRM
076200100322     C                   eval       §AUTCPRRE = D1AUTCPRRE
076300100322     C                   eval       §AUTGSTM  = D1AUTGSTM
076400100322     C                   eval       §AUTSTDCC = D1AUTSTDCC
076500100322     C*
076600100121     C                   MOVEl     DAut          tbeUNI
076700100121     C*
076800100324     C                   ENDSR
076900100324     C************************************************************
077000100409     C* Aggiornamento di AZUTE
077100100409     C************************************************************
077200100409     C     AGG_AZute     BEGSR
077300100409     C*
077400100409     c                   clear                   TbeAUT_found      1
077500100409     C*
077600100409     C* controlla esistenza comunque di codici FIL/PROFILO su AUT
077700100409     C*  NON annullati quindi aggiorna AZUTE
077800100409     C*
077900100409     C     K_tbe011      setll     tntbe01L
078000100409     C     K_tbe011      reade     tntbe01L
078100100409     C*
078200100409     C                   Dow       not %Eof(tntbe01l)
078300100409     C*  se ce n'è almeno uno attivo
078400100409     c                   if        tbeATB = ' '
078500100409     c                   move      'S'           TbeAUT_found      1
078600100409     C                   Leave
078700100409     C                   END
078800100409     C*
078900100409     C     K_tbe011      reade     tntbe01L
079000100409     C                   END
079100100409     C*
079200100409     C*  aggancia AZUTE
079300100409     c                   open      azute01l
079400100409     c     d1cod         chain     azute01l
079500100409     c                   if        %Found(azute01l)
079600100409     C*
079700100409     c                   movel     utefaf        dute01
079800100409     C*  imposta il flag
079900100409     c                   if        TbeAUT_found = 'S'
080000100409     c                   eval      §UTEtbeAUT   = 'S'
080100100409     C                   else
080200100409     c                   eval      §UTEtbeAUT   = ' '
080300100409     C                   end
080400100409     C*
080500100409     c                   movel     dute01        utefaf
080600100409     C*  aggiorna AZUTE
080700100409     c                   update    azute000
080800100409     C                   end
080900100409     c                   close     azute01l
081000100409     C*
081100100409     C                   ENDSR
081200100409     C************************************************************
081300100324     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
081400100324     C************************************************************
081500100324     C     Wind_trasm    BEGSR
081600100324     C*
081700100324     C                   mOVEl     tbeFTT        t1trs
081800100728     c     t1trs         ifeq      *blank
081900100728    >C     xtaopz        andeq     '01'
082000100728     C                   mOVEl     'S'           t1trs
082100100728     c                   end
082200100324     C                   z-add     tbeFLT        t1fil
082300100324     c                   exfmt     w1tras
082400100324     C                   MOVEl     t1trs         tbeFTT
082500100324     C                   z-add     t1fil         tbeFLT
082600100324     c                   clear                   tbeFTR
082700100324     c                   clear                   tbeDTR
082800030114     C*
082900940207     C                   ENDSR
083000940224     C************************************************************
083100940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
083200940224     C************************************************************
083300940224     C     GESAGG        BEGSR
083400940224     C*
083500940224     C* segnalo al pgm chiamante l'avvenuta conferma
083600030114    >C                   MOVE      *ON           xtaopr
083700940224     C* segnalo al pgm chiamante l'aver premuto l'F6
083800030114    >C                   MOVE      '0'           xtaret
083900940224     C*
0840009402241    C                   SELECT
084100940224     C* nel caso di immissione, ripristino la videata iniziale
084200030114    >C     xtaopz        WHENEQ    '01'
084300940224     C                   MOVE      *ON           $INZD1
084400030114     C                   MOVE      *ON           $FINE
084500940224     C* altrimenti ritorno al pgm chiamante
084600940224     C                   OTHER
084700101019      *
084800101019      * Solo se non ripristinato il record
084900101019     C     xtaopz        ifeq      '08'
085000101019     c                   eval       xtaopz = '02'
085100101019     c                   else
085200940224     C                   MOVE      *ON           $FINE
085300101019     c                   end
085400101019     C*
0855009402241-   C                   ENDSL
085600940224     C*
085700940224     C                   ENDSR
085800940131     C************************************************************
085900100121     C*
086000100121     C************************************************************
086100100121     C     Imposta_KEY   BEGSR
086200100121     C*
086300100121     c                   clear                   tbe_Key1
086400100121     c                   clear                   tbe_Key2
086500100121     c                   move      d1FIL         a1fil
086600100121     c                   eval      %Subst(tbe_key1:1:3) = A1fil
086700100121     c                   eval      %Subst(tbe_key1:4:10)= d1cod
086800100121     c                   if        d1fil1 > 0
086900100121     c                   move      d1FIL1        a1fil
087000100121     c                   eval      %Subst(tbe_key2:1:3) = a1fil
087100100121     c                   end
087200100121     C*
087300100121     C                   ENDSR
