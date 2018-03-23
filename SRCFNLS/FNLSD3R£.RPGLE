000100010116     H DECEDIT('0,') DATEDIT(*DMY.)
000200071227      * FNLSD3R *-----------------------------------------------------*
000300071227      *?          ELENCO SEGNACOLLI NON SPUNTATI          - BATCH -  ?*
000400071227      *---------------------------------------------------------------*
000500010510     FFISGN12L  IF   E           K DISK
000600960322     FFNBLP01L  IF   E           K DISK
000700000225     FFNBLT27L  IF   E           K DISK
000800041029     FFNart01l  IF   E           K DISK
000900021203     FFIAPD01L  IF   E           K DISK
001000010524     FAZORG01L  IF   E           K DISK
001100071227      *
001200010514     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001300010514     F                                     USROPN
001400071227      *
001500071227      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
001600071227      *
001700071228     D $ST             S             66    DIM(17) CTDATA PERRCD(1)
001800071228     D $OVR            S             40    DIM(1)  CTDATA PERRCD(1)
001900010522     D L1              S              3  0 DIM(30)
002000010522     D L6C             S              3  0 DIM(30)
002100010514     D L6S             S              3  0 DIM(30)
002200071227      *
002300071227      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
002400071227      *
002500071227     d Status         sds
002600071227     d  SDSpgm           *proc
002700071227      *
002800071227     D* PASSAGGIO DATI DAL FILTRO                         - FNLSD2R -
002900071227     D KPJBA         E DS
003000071227     d FNLSD3ds      e ds
003100071227      *
003200071227      * - Parametri x Controllo profilo utenti
003300071227     d TIBS34ds      e ds
003400071227      * - Ds di riferimento al file esterno AZUTE00F
003500071227     d AZUTEds       e ds                       extname(AZUTE00F)
003600071227      * - Ds per dati organigramma
003700071227     d dDatiUte      e ds
003800071227      *
003900071227     D* ds per pgm TRUL06R - caricamento £x
004000071227     d TRUL06ds      e ds                  inz
004100071227     d  Lin                    1     90  0 inz  dim(30)
004200960327     D*
004300960325     D                 DS
004400960329     D  AA                     1      4  0
004500960329     D  MM                     5      6  0
004600960329     D  GG                     7      8  0
004700960325     D  WDATA                  1      8  0
004800000224     D                 DS
004900000224     D  BLPAAS                 1      4  0
005000000224     D  BLPMGS                 5      8  0
005100071227     D  BLPDSP                 1      8  0
005200071227      *
005300071227      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
005400071227      *
005500071228      * - Flags booleani
005600071228     d $Opn            s              1    inz(*off)
005700071228      * - Indici di schiera
005800071228     d z               s              2  0 inz
005900071228     d wa              s              3  0 inz
006000071228     d wb              s              3  0 inz
006100071228     d wc              s              3  0 inz
006200071228      * - Variabili di comodo
006300071228     d GgMm            s              4  0 inz
006400071228      * - Campi "chiave"
006500071227     d KST2            s                   like(SGNst2)  inz
006600071227     d KLNP1           s                   like(SGNlnp)  inz
006700071227     d KPDR            s                   like(SGNcpd)  inz
006800071227     D kpdpdr          S                   LIKE(APDpdr)  inz
006900071227     D kpdtip          S                   LIKE(APDtip)  inz
007000071227      * - Campi in stampa
007100071227     d STAdvd          s                   like(DD3dvd)  inz
007200071227     d STAdva          s                   like(DD3dva)  inz
007300071228     d STAlnp          s                   like(ORGfil)  inz
007400071227     d DESlnp          s                   like(ORGdes)  inz
007500071227     d STArsm          s             19    inz
007600071227     d STArsd          s             20    inz
007700071227     d STAlod          s             15    inz
007800071227     d STApdr          s             15    inz
007900071227     d STAdsp          s              8  0 inz
008000071227     d STAdfv          s              6  0 inz
008100071227     d STAnpp          s              2    inz
008200071228     d TOTbol          s              9  0 inz(*loval)
008300071227      *
008400071227      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
008500071227      *
008600071227     C     KSGN          KLIST
008700071227     C                   KFLD                    SIMFEL
008800071227     C                   KFLD                    KST2
008900071227     C     KSGN1         KLIST
009000071227     C                   KFLD                    SIMFEL
009100071227     C                   KFLD                    KST2
009200071227     C                   KFLD                    DD3PO
009300071227     C                   KFLD                    KPDR
009400071227     C     KSGN2         KLIST
009500071227     C                   KFLD                    SIMFEL
009600071227     C                   KFLD                    KST2
009700071227     C                   KFLD                    DD3PO
009800071227     C                   KFLD                    KPDR
009900071227     C                   KFLD                    DD3KSC
010000071227     C     KSGN3         KLIST
010100071227     C                   KFLD                    SIMFEL
010200071227     C                   KFLD                    KST2
010300071227     C                   KFLD                    KLNP1
010400071227     C     KBLP          KLIST
010500071227     C                   KFLD                    SGNAAS
010600071227     C                   KFLD                    SGNLNP
010700071227     C                   KFLD                    SGNNRS
010800071227     C                   KFLD                    SGNNSP
010900071227     C     KBLT          KLIST
011000071227     C                   KFLD                    SGNFLS
011100071227     C                   KFLD                    SGNLNA
011200071227     C                   KFLD                    SGNNRS
011300071227     C                   KFLD                    SGNNCD
011400071227
011500071227     C     Kfapd         klist
011600071227     C                   kfld                    kpdtip
011700071227     C                   kfld                    kpdpdr
011800071227     C     Kart          KLIST
011900071227     C                   KFLD                    bltAAS
012000071227     C                   KFLD                    bltLNP
012100071227     C                   KFLD                    bltNRS
012200071227     C                   KFLD                    bltNSP
012300071227     C                   KFLD                    bltNSc
012400071227
012500960321     C*****************************************************************
012600071227     C*?RIEPILOGO INDICATORI                                         ?*
012700960321     C*****************************************************************
012800960328     C* 06    - IMMESSO UNO SPECIFICO CODICE CLIENTE
012900000224     C* 07    - IMMESSA LA DATA SPEDIZIONE
013000071227     C* 08-12 - IMMESSO PADRONCINO
013100071227     C* 13    - NUMERO SERIE <> 0  --->  STAMPO TRATTINO SEPARATORE
013200960329     C* 17    - USATO PER NON STAMPARE LA 1° VOLTA IL TOTALE BOLLE
013300010321     C* 20 ON - NON STAMPA
013400960329     C* 22 OFF- NON C'E' NULLA DA STAMPARE: STAMPO LA TESTATA
013500960326     C* 30/32 - DI COMODO
013600960321     C*****************************************************************
013700071227
013800960325     C****  E L A B O R A Z I O N E  ****
013900010522    1C     *IN08         IFEQ      *OFF
014000071228     c     DD3sst        orEQ      'S'
014100010522     C                   EXSR      ELAPO
014200010522   X1C                   ELSE
014300010522     C                   EXSR      ELAPDR
014400010522    1C                   ENDIF
014500001020      *
014600000224     C* STAMPO ULTIMO TOTALE BOLLE
014700000224     C     *IN22         IFEQ      *ON
014800000224     C*
014900000224    1C     *INOF         IFEQ      *ON
015000000224     C                   EXCEPT    TES
015100071228     c                   if        DD3sst = 'S'
015200071228     c                   except    TES_Fil
015300071228     c                   else
015400071227     c                   except    Tes_B
015500071227     c                   endif
015600000224     C                   SETOFF                                       OF
015700000224    1C                   ENDIF
015800000224     C*
015900071228     c                   if        DD3sst = 'S'
016000071228     c                   except    TOT_Fil
016100071228     c                   else
016200000224     C                   EXCEPT    TOT
016300071227     c                   endif
016400000224     C*
016500000224     C****  SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA  ****
016600000224    1C                   ELSE
016700000224     C*
016800010528     C     *IN08         IFEQ      *ON
016900010528     C     DD3BRK        OREQ      ' '
017000071228     c***  DD3sst        oreq      'S'    (già in DD3brk = " ")
017100010608     C                   EXCEPT    TES
017200071228     c                   if        DD3sst = 'S'
017300071228     c                   except    TES_Fil
017400071228     c                   else
017500071227     c                   except    Tes_B
017600071227     c                   endif
017700010608     C                   EXCEPT    FITT
017800010528     C                   ENDIF
017900000224    1C                   ENDIF
018000000224     C*
018100000224     C****  STAMPA DICITURA: FINE STAMPA  ****
018200000224     C                   EXCEPT    FINEST
018300000000     C                   SETON                                        LR
018400010522     C*
018500010522     C*-- ELABORAZIONE PER P.O. ---------------------------------------
018600010522     C     ELAPO         BEGSR
018700071228     c                   if        DD3sst <> 'S'
018800071228     C                   reset                   $Opn
018900071228     c                   endif
019000071228     C                   Z-ADD     1             Z
019100010528     C     Z             DOWLE     30
019200010528    1C     L1(Z)         ANDGT     *ZEROS
019300010522     C     L1(Z)         LOOKUP    L6C                                    32
019400010522    2C     *IN32         IFEQ      *OFF
019500010522     C                   Z-ADD     L1(Z)         KLNP1
019600010524     C* Operazioni a rottura di P.O.
019700010528     C     DD3BRK        IFEQ      'S'
019800071228     c     DD3sst        orEQ      'S'
019900010524     C                   EXSR      BRKLNP
020000010528     C                   ENDIF
020100080128     c                   if        TotBol = *loval
020200080128     c                   clear                   TotBol
020300080128     c                   endif
020400010522     C****
020500071227     C     KSGN3         SETLL     FISGN000
020600071227     C     KSGN3         READE     FISGN000                               30
020700010522    3C     *IN30         DOWEQ     *OFF
020800010522     C                   EXSR      ELARCD
020900071227     C     KSGN3         READE     FISGN000                               30
021000010522    3C                   ENDDO
021100010522     C* VEDO SE QUESTO P.O. GESTISCE ALTRI P.O.
021200010522     C                   EXSR      CARL6S
021300010522     C     Z6            DOWLE     30
021400010522    3C     L6S(Z6)       ANDGT     0
021500010524     C* DEVO ARE IL TEST SEGUENTE PERCHÈ SE PER IL P.O. NON ESISTE
021600010524     C* TABELLA £6 IL PGM TRUL06R RITORNA LA KEY IN PRIMA POSIZIONE
021700010524     C* DELLA SCHIERA ANCHE SE E' STATO RICHIAMATO CON D06ESC='S'
021800010522     C                   Z-ADD     L6S(Z6)       KLNP1
021900071227     C     KSGN3         SETLL     FISGN000
022000071227     C     KSGN3         READE     FISGN000                               30
022100010522    4C     *IN30         DOWEQ     *OFF
022200010522     C                   EXSR      ELARCD
022300071227     C     KSGN3         READE     FISGN000                               30
022400010522    4C                   ENDDO
022500010522     C                   ADD       1             Z6
022600010522    3C                   ENDDO
022700010522    2C                   ENDIF
022800010522     C                   ADD       1             Z
022900010522    1C                   ENDDO
023000010522     C                   ENDSR
023100010522     C*
023200010522     C*-- ELABORAZIONE PER CODICE AUTOTRASPORTATORE -------------------
023300010522     C     ELAPDR        BEGSR
023400080110     c                   clear                   TotBol
023500010522     C  N06KSGN1         SETLL     FISGN000
023600010522     C   06KSGN2         SETLL     FISGN000
023700010522     C*
023800010522     C* SE HO UN SOLO COD.AUTOTRASPORTATORE LEGGO
023900010522    1C     *IN09         IFEQ      *OFF
024000010522     C  N06KSGN1         READE     FISGN000                               30
024100010522     C   06KSGN2         READE     FISGN000                               30
024200010522   X1C                   ELSE
024300010522     C     KSGN          READE     FISGN000                               30
024400010522    1C                   ENDIF
024500010522    1C     *IN30         DOWEQ     *OFF
024600010522     C                   EXSR      ELARCD
024700010522    2C     *IN09         IFEQ      *OFF
024800010522     C  N06KSGN1         READE     FISGN000                               30
024900010522     C   06KSGN2         READE     FISGN000                               30
025000010522   X2C                   ELSE
025100010522     C     KSGN          READE     FISGN000                               30
025200010522    2C                   ENDIF
025300010522    1C                   ENDDO
025400010522     C                   ENDSR
025500071227      *
025600010524     C*-- OPERAZIONI A ROTTURA DI P.O. --------------------------------
025700010524     C     BRKLNP        BEGSR
025800071228      *
025900071228if  1c                   IF        DD3sst =  'S'
026000071228      *
026100071228if  2c                   if        TotBol <> *loval
026200071228if  3c                   if        *inOF
026300071228     c                   except    TES
026400071228     c                   except    TES_Fil
026500071228     C                   eval      *inOF  =  *off
026600071228e   3c                   endif
026700071228     c                   except    TOT_Fil
026800071228e   2c                   endif
026900071228      *
027000071228x   1c                   ELSE
027100071228      *
027200010524     C* Operazioni su file printer
027300071228    3C     $Opn          IFEQ      *on
027400010524     C* Stampo ultimo totale bolle
027500010524    4C     *IN22         IFEQ      *ON
027600010524     C*
027700010524    5C     *INOF         IFEQ      *ON
027800010524     C                   EXCEPT    TES
027900071227     c                   except    Tes_B
028000010524     C                   SETOFF                                       OF
028100010524    5C                   ENDIF
028200010524     C                   EXCEPT    TOT
028300010524    4C                   ENDIF
028400010524     C* Stampa dicitura: FINE STAMPA
028500010524     C                   EXCEPT    FINEST
028600010524     C                   CLOSE     PRTF198
028700071228     C                   reset                   $Opn
028800010524    3C                   ENDIF
028900010524     C* ESEGUO OVRPRTF PER SPECIFICARE DATI UTENTE
029000010524     C     KLNP1         IFNE      SIMFEL
029100010524     C                   Z-ADD     40            LUNG2
029200071228     C                   MOVEA     $OVR(1)       COMMAN
029300071227     C                   MOVEL     SDSpgm        W011A            11
029400010524     C                   MOVEL     KLNP1         W004A             4
029500010524     C                   MOVE      ')'           W004A
029600010524     C                   MOVE      W004A         W011A
029700010524     C                   MOVE      W011A         COMMAN
029800010524     C                   CALL      'QCMDEXC'
029900010524     C                   PARM                    COMMAN           40
030000010524     C                   PARM                    LUNG2            15 5
030100010524     C                   ENDIF
030200010524     C* Apro file printer
030300010524     C                   OPEN      PRTF198
030400071228     C                   MOVE      *on           $Opn
030500010524     C                   SETOFF                                       22OF
030600010524     C* Stampo testata
030700010524     C                   EXCEPT    TES
030800071227     c                   except    Tes_B
030900071228      *
031000071228e   1c                   ENDIF
031100071228      *
031200010524     C* Decodifico il p.o.
031300071228     c                   eval      STAlnp = Klnp1
031400010524     C     KLNP1         CHAIN     AZORG01L                           32
031500010524     C  N32              MOVEL     ORGDES        DESLNP
031600010524     C   32              CLEAR                   DESLNP
031700071228     c                   if        DD3sst <> 'S'
031800010524     C                   EXCEPT    TES1
031900071227     c                   endif
032000080128      *
032100080128     c                   clear                   TotBol
032200080128      *
032300010524     C                   ENDSR
032400010514     C*
032500010522     C*-- ELABORAZIONE RECORD DI FISGN --------------------------------
032600010522     C     ELARCD        BEGSR
032700010522      * ELABORO RCD NON ANNULLATI
032800010522    1C     SGNATB        IFEQ      *BLANKS
032900010522      *
033000010522     C* SE IMMESSO COD PADRONCINO LNP DEVE ESSERE UGUALE
033100010522    2C     *IN08         IFEQ      *OFF
033200010522     C     SGNLNP        OREQ      DD3PO
033300010522     C* CHAIN BOLLA
033400071227     C     KBLP          CHAIN     FNBLP000                           31
033500010522     C*
033600010522    3C  N31DD3DVD        IFEQ      0
033700010522     C     BLPDSP        ORGE      DD3DVD
033800010522     C     BLPDSP        ANDLE     DD3DVA
033900010522     C*
034000010522     C* CLIENTE
034100010522    4C     DD3KSC        IFEQ      0
034200010522     C     DD3KSC        OREQ      SGNKSC
034300010522     C*
034400010522     C* CONTROLLO TUTTI GLI AUTOTRASPORTATORI
034500071228    5C     *IN08         IFEQ      *OFF
034600010522     C*
034700010522    5C     DD3PDR        OREQ      BLPPDR
034800010522     C     DD3PD2        OREQ      BLPPDR
034900010522     C     DD3PD3        OREQ      BLPPDR
035000010522     C     DD3PD4        OREQ      BLPPDR
035100010522     C     DD3PD5        OREQ      BLPPDR
035200010522     C** STAMPO
035300010522     C                   EXSR      STADET
035400010522    5C                   ENDIF
035500010522    4C                   ENDIF
035600010522    3C                   ENDIF
035700010522    2C                   ENDIF
035800010522    1C                   ENDIF
035900010522     C                   ENDSR
036000071227      *
036100010514     C*--- POSIZIONAMENTO E LETTURA SU FISGN ------------------------*
036200010522     C     CARL6S        BEGSR
036300010514     C* SE HO APPENA ELABORATO UNA LINEA NON GESTITA CARICO LA SUA £6
036400010514     C* PER VEDERE SE ELABORARE DEI P.O. DA LEI GESTITI
036500071227     C                   CLEAR                   TRUL06ds
036600010514     C                   MOVE      '£6'          D06COD
036700010522     C                   MOVEL     KLNP1         D06KEY
036800010522     C                   MOVEL     'S'           D06ESC
036900071227     C                   MOVEL     TRUL06ds      KPJBU
037000010514     C                   CALL      'TRUL06R'
037100010514     C                   PARM                    KPJBA
037200071227     C                   MOVEL     KPJBU         TRUL06ds
037300010514     C                   MOVEA     LIN           L6S
037400010514     C                   SORTA     L6S
037500010522     C                   Z-ADD     1             Z6                3 0
037600010528     C     Z6            DOWLE     30
037700010528    2C     L6S(Z6)       ANDEQ     0
037800010522     C                   ADD       1             Z6
037900010514    2C                   ENDDO
038000010514     C                   ENDSR
038100960322     C*
038200960326     C*--- STAMPO DETTAGLIO ------------------------------------------*
038300960326     C     STADET        BEGSR
038400960329     C                   SETON                                        22
038500000225     C                   CLEAR                   STANPP
038600000225     C                   CLEAR                   STADFV
038700960326     C*
038800960326     C****  STAMPO TESTATA  ****
038900000224     C     *INOF         IFEQ      *ON
039000071228     c     DD3sst        andNE     'S'
039100960327     C                   EXCEPT    TES
039200071227     c                   except    Tes_B
039300960329     C                   SETOFF                                       OF
039400960326     C                   ENDIF
039500000225     C*
039600000225     C* CHAIN FNBLT
039700071227     C     KBLT          CHAIN     FNBLT000                           31
039800071228if  1C     *IN31         IFEQ      *OFF
039900041029     c* chaino anche art--> se c'e' i campi dcm e dam li controllo da
040000041029     c*  art invece che da blt
040100071227     c     kart          chain     fnart000
040200000225     C*
040300000225     C* GIRO DATA PARTENZA COLLO
040400071228if  2C     BLTDFV        IFGT      0
040500000225     C                   Z-ADD     BLTDFV        WDATA
040600071227     C                   Z-ADD     MM            GGMM
040700000225     C                   MOVEL     GG            GGMM
040800000225     C                   Z-ADD     AA            STADFV
040900071227     C                   MOVEL     GGMM          STADFV
041000071227     C                   MOVEL     BLTNPP        STANPP
041100071228e   2C                   ENDIF
041200010321      *
041300010321      * CONTROLLO BLTDAM E BLTDCM SE UNO DEI DUE DIVERSO DA ZERO NON DEVO
041400010321      * STAMPARE
041500010321     C                   SETOFF                                       20
041600071228if  2c                   if        %found(fnart01l)
041700041029     c
041800041029     c                   if        artdcm<>0 or artdam<>0
041900041029     C                   SETON                                        20
042000041029     c                   endif
042100071228x   2c                   else
042200041029     c
042300010321     C     BLTDAM        IFNE      *ZEROS
042400010321     C     BLTDCM        ORNE      *ZEROS
042500010321     C                   SETON                                        20
042600010321     C                   ENDIF
042700071228e   2C                   ENDIF
042800010321      *
042900071228e   1C                   ENDIF
043000960326     C*
043100960326     C****  STAMPO DETTAGLIO  ****
043200010321      *
043300071228if  1C     *IN20         IFEQ      *OFF
043400010321      *
043500071228if  2c                   if        DD3sst <> 'S'
043600071227      *
043700071227     C* 13 ON  - NUMERO SERIE <> 0  ---> STAMPO TRATTINO SEPARATORE
043800071227     C     BLPNRS        COMP      0                                  1313
043900960326     C*
044000960326     C* GIRO DATA SPEDIZIONE
044100000224     C                   Z-ADD     BLPDSP        WDATA
044200071227     C                   Z-ADD     MM            GGMM
044300000224     C                   MOVEL     GG            GGMM
044400000224     C                   Z-ADD     AA            STADSP
044500071227     C                   MOVEL     GGMM          STADSP
044600960326     C*
044700960401     C* RAGIONE SOCIALE MITTENTE E DESTINATARIO
044800071227     C                   MOVEL     BLPRSM        STARSM
044900071227     C                   MOVEL     BLPRSD        STARSD
045000071227     C                   MOVEL     BLPLOD        STALOD
045100000224     C*
045200021205     C                   move      BlpPdr        kpdpdr
045300021203     C                   move      'A'           kpdtip
045400071227     C     kfapd         chain     fiapd000                           31
045500071227     C  N31              MOVEL     APDRSC        STAPDR
045600071227      *
045700071228e   2c                   endif
045800960326     C*
045900071227     C                   ADD       1             TOTBOL
046000960329     C*
046100071228     c                   if        DD3sst <> 'S'
046200960326     C                   EXCEPT    DETT
046300071227     c                   endif
046400010321      *
046500071228e   1C                   ENDIF
046600010321      *
046700960326     C                   ENDSR
046800960326     C*
046900960321     C*--- SR INIZIALE -----------------------------------------------*
047000960321     C     *INZSR        BEGSR
047100010514     C*
047200960325     C****  PRENDO DATA E ORA CORRENTE  ****
047300960325     C                   TIME                    W0140            14 0
047400960325     C                   MOVE      W0140         UDATE8            8 0
047500960329     C                   MOVEL     W0140         UTIME             6 0
047600960329     C*
047700071227      * Reperimento dati job
047800071227     c     *dtaara       define    §azute        AZUTEds
047900071227     c     *dtaara       define    §datiute      dDATIUTE
048000071227      *
048100071227     c                   in(E)     *dtaara
048200071227     c                   IF        %ERROR or RSUT = *blanks
048300071227     c                   clear                   Tibs34Ds
048400071227     c                   call      'TIBS34R'
048500071227     c                   parm                    Tibs34Ds
048600071227     c                   in        *dtaara
048700071227     c                   ENDIF
048800960327     C*
048900960326     C****  PARAMETRI PASSATI  ****
049000960326     C     *ENTRY        PLIST
049100960326     C                   PARM                    KPJBA
049200071227     C                   MOVEL     KPJBU         FNLSD3ds
049300960329     C*
049400960329     C****  CODICE CLIENTE  ****
049500071227     c                   eval      *in06 = (DD3ksc > *zeros)
049600960329     C*
049700000217     C****  DATA SEDIZIONE   ****
049800000217     C* SE DATA SPEDIZIONE AL = 0 --> METTO DATA DAL IN DATA AL
049900071227     c                   eval      *in07 = (DD3dvd > *zeros)
050000000217    1C     DD3DVD        IFGT      0
050100000217     C     DD3DVA        IFEQ      0
050200000217     C                   Z-ADD     DD3DVD        DD3DVA
050300960328     C                   ENDIF
050400000217     C* GIRO DATA SPEDIZIONE DAL
050500000217     C                   Z-ADD     DD3DVD        WDATA
050600071227     C                   Z-ADD     MM            GGMM
050700960328     C                   MOVEL     GG            GGMM
050800960329     C                   Z-ADD     AA            STADVD
050900960329     C                   MOVEL     GGMM          STADVD
051000000217     C* GIRO DATA SPEDIZIONE  AL
051100000217     C                   Z-ADD     DD3DVA        WDATA
051200960329     C                   Z-ADD     MM            GGMM
051300960329     C                   MOVEL     GG            GGMM
051400960329     C                   Z-ADD     AA            STADVA
051500960329     C                   MOVEL     GGMM          STADVA
051600961016    1C                   ENDIF
051700961016     C*
051800000217     C****  COD.AUTOTRASP ****
051900071227     c                   eval      *in08 = (DD3pdr > *zeros)
052000071227     c                   eval      *in09 = (DD3pd2 > *zeros)
052100071227     c                   eval      *in10 = (DD3pd3 > *zeros)
052200071227     c                   eval      *in11 = (DD3pd4 > *zeros)
052300071227     c                   eval      *in12 = (DD3pd5 > *zeros)
052400000224    1C     DD3PDR        IFGT      0
052500071227     C                   Z-ADD     DD3PDR        KPDR
052600961016    1C                   ENDIF
052700010514     C*
052800010522     C* CARICO LA £1 SE NON E' STATO RICHIESTO UN P.O. SPECIFICO
052900010522     C     DD3PO         IFEQ      0
053000071227     C                   CLEAR                   TRUL06ds
053100010522     C                   MOVE      '£1'          D06COD
053200020508     c                   movel     simfel        d06key
053300071227     C                   MOVEL     TRUL06ds      KPJBU
053400010522     C                   CALL      'TRUL06R'
053500010522     C                   PARM                    KPJBA
053600071227     C                   MOVEL     KPJBU         TRUL06ds
053700010522     C                   MOVEA     LIN           L1
053800010522     C                   ELSE
053900010522     C                   MOVEL     DD3PO         L1(1)
054000010522     C                   ENDIF
054100020508      * per ogni elemento di £1 carico la £6
054200020508     c                   clear                   wc
054300020508     c                   do        30            wa
054400020508     c                   if        l1(wa) = *zeros
054500020508     c                   leave
054600020508     c                   endif
054700020508     c                   clear                   kpjbu
054800071227     c                   clear                   TRUL06ds
054900020508     c                   eval      d06cod = '£6'
055000020508     c                   movel     l1(wa)        d06key
055100020508     c                   eval      d06esc = 'S'
055200071227     c                   movel     TRUL06ds      kpjbu
055300020508     c                   call      'TRUL06R'
055400020508     c                   parm                    kpjba
055500071227     c                   movel     kpjbu         TRUL06ds
055600020508 b3  c                   do        30            wb
055700020508     c                   if        lin(wb) = *zeros
055800020508     c                   leave
055900020508     c                   endif
056000020508     c                   add       1             wc
056100020508     c                   z-add     lin(wb)       l6c(wc)
056200020508 e3  c                   enddo
056300020508 e2  c                   enddo
056400010522     C* ACCENDO INDICATORE DI OVERFLOW
056500010528     C* ESEGUO OVRPRTF PER SPECIFICARE DATI UTENTE
056600071228     c     *in08         IFEQ      *on
056700071228     C     DD3PO         andNE     SIMFEL
056800010528     C                   Z-ADD     40            LUNG2
056900071228     C                   MOVEA     $OVR(1)       COMMAN
057000071227     C                   MOVEL     SDSpgm        W011A            11
057100010528     C                   MOVEL     DD3PO         W004A             4
057200010528     C                   MOVE      ')'           W004A
057300010528     C                   MOVE      W004A         W011A
057400010528     C                   MOVE      W011A         COMMAN
057500010528     C                   CALL      'QCMDEXC'
057600010528     C                   PARM                    COMMAN           40
057700010528     C                   PARM                    LUNG2            15 5
057800010528     C                   ENDIF
057900010528     C     *IN08         IFEQ      *ON
058000010528     C     DD3BRK        OREQ      ' '
058100010528     C                   OPEN      PRTF198
058200071228     C                   MOVE      *on           $Opn
058300010528     C                   SETON                                        OF
058400010528     C                   ENDIF
058500960329     C*
058600920925     C                   ENDSR
058700960326     C*
058800960325     O*---------------------------------------------------------------*
058900960328     O****  TESTATA  ****
059000000224     OPRTF198   E            TES              02
059100071228     O                       $ST(1)             132
059200071228     O                       $ST(2)             198
059300960328     O                       RSUT                20
059400960328     O                                           21 '/'
059500010522     O                       SIMFEL              24
059600071227     O                       SDSpgm             153
059700000218     O                       UDATE8             178 '  /  /    '
059800000218     O                       PAGE          Z    198
059900960328     O          E            TES              03
060000960328     O                       KNSIF               10
060100960328     O                       KNMUS               24
060200000224     O                       UTIME              178 '  :  :  '
060300071227     O          E            TES              05
060400071228     O                       $ST(3)              66
060500071228     O                       $ST(4)             132
060600071227     O               06      DD3KSC        Z     51
060700071227     O                  07   STADVD              86 '  /  /    '
060800071227     O                  07   STADVA             100 '  /  /    '
060900000224     O                     08DD3PDR             136
061000000224     O                     09DD3PD2            +  1
061100000224     O                     10DD3PD3            +  1
061200000224     O                     11DD3PD4            +  1
061300000224     O                     12DD3PD5            +  1
061400960328     O*
061500071227     O          E            TES_B            07
061600071228     O                       $ST(5)              66
061700071228     O                       $ST(8)             132
061800071228     O                       $ST(11)            198
061900071227     O          E            TES_B          1 08
062000071228     O                       $ST(6)              66
062100071228     O                       $ST(9)             132
062200071228     O                       $ST(12)            198
062300071228     o          e            TES_Fil     3
062400071228     o                       $ST(15)             66
062500071228     o          e            TES_Fil     1
062600071228     o                       $ST(16)             66
062700071227     O* Filiale
062800010524     O          E            TES1           3 10
062900071227     O                                           16 'FILIALE:'
063000010524     O                       KLNP1             +  1
063100010524     O                       DESLNP            +  1
063200960328     O****  DETTAGLIO  ****
063300960328     O          E            DETT        1
063400000224     O                       BLPPDR
063500000224     O                                         +  0 '-'
063600000224     O                       STAPDR            +  0
063700000224     O                       BLPLNP        Z   +  1
063800071227     O               13      BLPNRS        Z   +  1
063900071227     O               13                        +  0 '-'
064000000224     O                       BLPNSP        Z   +  0
064100000224     O                       STADSP            +  1 '  /  /    '
064200000224     O                       BLPLNA            +  1
064300000224     O                       SGNKSC            +  2
064400000224     O                                         +  0 '-'
064500000224     O                       STARSM            +  0
064600000224     O                       STARSD            +  1
064700000224     O                       BLPPRD            +  1
064800000224     O                       BLPNZD            +  1
064900000224     O                       BLPRMN        Z   +  1
065000000224     O                       BLPRMA            +  1
065100000225     O                       SGNWHO            +  1
065200010321     O                       SGNNCD        Z   +  1
065300010321     O                       STADFV            +  1 '  /  /  '
065400000225     O                       STANPP            +  1
065500960401     O*
065600960328     O****  TOTALE BOLLE VARIATE PER BOLLETTISTA  ****
065700960328     O          E            TOT         2  3
065800000224     O                                           25 'TOTALE COLLI :'
065900960328     O                       TOTBOL        2     37
066000071228     o          e            Tot_Fil     2
066100071228     o                       STAlnp
066200071227     o                       DESlnp            +  1
066300071227     o                       TOTbol        k   +  5
066400960329     O****  FITTIZZIO PER IL SALTO GIUSTO DELLE RIGHE  ****
066500000224     O          E            FITT             20
066600960329     O                                              ' '
066700960329     O****  DICITURA FINE STAMPA  ****
066800960329     O          E            FINEST      2
066900071228     O                       $ST(14)            132
067000960326     O*---------------------------------------------------------------*
067100071228** - $ST
067200010321 ***   ELENCO SEGNACOLLI NON SPUNTATI DISK  "C"   ***
067300000218                                                         PAG. XXXX  2
067400000224                               COD.CLIENTE:             DATA SPEDI  3
067500000224ZIONE DAL            AL                 COD.AUTOTRASPORTATORE:      4
067600000225  AUTOTRASPORTATORE     S  P  E  D  I  Z  I  O  N  E      M I T T   5
067700000224                        LNP   NUMERO     DATA     LNA               6
067800000224XXXXXXX-XXXXXXXXXXXXXXX XXX XX-XXXXXXX XX/XX/XXXX XXX  XXXXXXX-XXX  7
067900000225E N T E             D E S T I N A T A R I O        R I F E R I M E  8
068000000225                 RAGIONE SOCIALE     PRV NAZ        NUMERICO ALFAB  9
068100000225XXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXX XX XXX XXXXXXXXXXXXXXX XXXXX  10
068200010321 N T I     "C H I    S O N O"                   NUMERO   PARTENZA   11
068300010321ETICO      IDENTIFICATIVO COLLO                 SEGNAC.  DATA  PIS  12
068400010321XXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXX XX/XX/XX XX  13
068500000224            * * *    F I N E    S T A M P A    * * *                14
068600071227FILIALE                   TOT. SPEDIZIONI                           15
068700071228------------------------  ---------------                           16
068800071228123 12345678901234567890     123.456.789-                           17
068900071228** - $OVR
069000010524OVRPRTF FILE(PRTF198) USRDTA(FNLSD3R   )
