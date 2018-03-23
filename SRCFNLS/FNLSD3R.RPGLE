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
003100081202     d   $Ksc                  1     70  0 dim(10)
003200071227      *
003300071227      * - Parametri x Controllo profilo utenti
003400071227     d TIBS34ds      e ds
003500071227      * - Ds di riferimento al file esterno AZUTE00F
003600071227     d AZUTEds       e ds                       extname(AZUTE00F)
003700071227      * - Ds per dati organigramma
003800071227     d dDatiUte      e ds
003900071227      *
004000071227     D* ds per pgm TRUL06R - caricamento £x
004100071227     d TRUL06ds      e ds                  inz
004200071227     d  Lin                    1     90  0 inz  dim(30)
004300960327     D*
004400960325     D                 DS
004500960329     D  AA                     1      4  0
004600960329     D  MM                     5      6  0
004700960329     D  GG                     7      8  0
004800960325     D  WDATA                  1      8  0
004900000224     D                 DS
005000000224     D  BLPAAS                 1      4  0
005100000224     D  BLPMGS                 5      8  0
005200071227     D  BLPDSP                 1      8  0
005300071227      *
005400071227      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
005500071227      *
005600071228      * - Flags booleani
005700071228     d $Opn            s              1    inz(*off)
005800071228      * - Indici di schiera
005900071228     d z               s              2  0 inz
006000071228     d wa              s              3  0 inz
006100071228     d wb              s              3  0 inz
006200071228     d wc              s              3  0 inz
006300071228      * - Variabili di comodo
006400071228     d GgMm            s              4  0 inz
006500071228      * - Campi "chiave"
006600071227     d KST2            s                   like(SGNst2)  inz
006700071227     d KLNP1           s                   like(SGNlnp)  inz
006800071227     d KPDR            s                   like(SGNcpd)  inz
006900071227     D kpdpdr          S                   LIKE(APDpdr)  inz
007000071227     D kpdtip          S                   LIKE(APDtip)  inz
007100071227      * - Campi in stampa
007200071227     d STAdvd          s                   like(DD3dvd)  inz
007300071227     d STAdva          s                   like(DD3dva)  inz
007400071228     d STAlnp          s                   like(ORGfil)  inz
007500071227     d DESlnp          s                   like(ORGdes)  inz
007600071227     d STArsm          s             19    inz
007700071227     d STArsd          s             20    inz
007800071227     d STAlod          s             15    inz
007900071227     d STApdr          s             15    inz
008000071227     d STAdsp          s              8  0 inz
008100071227     d STAdfv          s              6  0 inz
008200071227     d STAnpp          s              2    inz
008300071228     d TOTbol          s              9  0 inz(*loval)
008400071227      *
008500071227      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
008600071227      *
008700081202     C     k02sgn12      KLIST
008800071227     C                   KFLD                    SIMFEL
008900071227     C                   KFLD                    KST2
009000081202     C     k03sgn12      KLIST
009100081202     C                   KFLD                    SIMFEL
009200081202     C                   KFLD                    KST2
009300081202     C                   KFLD                    KLNP1
009400081202     C     k04sgn12      KLIST
009500071227     C                   KFLD                    SIMFEL
009600071227     C                   KFLD                    KST2
009700071227     C                   KFLD                    DD3PO
009800071227     C                   KFLD                    KPDR
009900081202     C     k04blp01      KLIST
010000071227     C                   KFLD                    SGNAAS
010100071227     C                   KFLD                    SGNLNP
010200071227     C                   KFLD                    SGNNRS
010300071227     C                   KFLD                    SGNNSP
010400071227     C     KBLT          KLIST
010500071227     C                   KFLD                    SGNFLS
010600071227     C                   KFLD                    SGNLNA
010700071227     C                   KFLD                    SGNNRS
010800071227     C                   KFLD                    SGNNCD
010900071227
011000071227     C     Kfapd         klist
011100071227     C                   kfld                    kpdtip
011200071227     C                   kfld                    kpdpdr
011300071227     C     Kart          KLIST
011400071227     C                   KFLD                    bltAAS
011500071227     C                   KFLD                    bltLNP
011600071227     C                   KFLD                    bltNRS
011700071227     C                   KFLD                    bltNSP
011800071227     C                   KFLD                    bltNSc
011900071227
012000960321     C*****************************************************************
012100071227     C*?RIEPILOGO INDICATORI                                         ?*
012200960321     C*****************************************************************
012300000224     C* 07    - IMMESSA LA DATA SPEDIZIONE
012400071227     C* 08-12 - IMMESSO PADRONCINO
012500081202     C* 13-22 - IMMESSO CODICE CLIENTE
012600081202     C* 23    - NUMERO SERIE <> 0  --->  STAMPO TRATTINO SEPARATORE
012700010321     C* 20 ON - NON STAMPA
012800960329     C* 22 OFF- NON C'E' NULLA DA STAMPARE: STAMPO LA TESTATA
012900960326     C* 30/32 - DI COMODO
013000960321     C*****************************************************************
013100071227
013200960325     C****  E L A B O R A Z I O N E  ****
013300010522    1C     *IN08         IFEQ      *OFF
013400071228     c     DD3sst        orEQ      'S'
013500010522     C                   EXSR      ELAPO
013600010522   X1C                   ELSE
013700010522     C                   EXSR      ELAPDR
013800010522    1C                   ENDIF
013900001020      *
014000000224     C* STAMPO ULTIMO TOTALE BOLLE
014100000224     C     *IN22         IFEQ      *ON
014200000224     C*
014300000224    1C     *INOF         IFEQ      *ON
014400000224     C                   EXCEPT    TES
014500071228     c                   if        DD3sst = 'S'
014600071228     c                   except    TES_Fil
014700071228     c                   else
014800071227     c                   except    Tes_B
014900071227     c                   endif
015000000224     C                   SETOFF                                       OF
015100000224    1C                   ENDIF
015200000224     C*
015300071228     c                   if        DD3sst = 'S'
015400071228     c                   except    TOT_Fil
015500071228     c                   else
015600000224     C                   EXCEPT    TOT
015700071227     c                   endif
015800000224     C*
015900000224     C****  SE NON C'E' NIENTE DA STAMPARE STAMPO LA TESTATA  ****
016000000224    1C                   ELSE
016100000224     C*
016200010528     C     *IN08         IFEQ      *ON
016300010528     C     DD3BRK        OREQ      ' '
016400071228     c***  DD3sst        oreq      'S'    (già in DD3brk = " ")
016500010608     C                   EXCEPT    TES
016600071228     c                   if        DD3sst = 'S'
016700071228     c                   except    TES_Fil
016800071228     c                   else
016900071227     c                   except    Tes_B
017000071227     c                   endif
017100010608     C                   EXCEPT    FITT
017200010528     C                   ENDIF
017300000224    1C                   ENDIF
017400000224     C*
017500000224     C****  STAMPA DICITURA: FINE STAMPA  ****
017600000224     C                   EXCEPT    FINEST
017700000000     C                   SETON                                        LR
017800010522     C*
017900010522     C*-- ELABORAZIONE PER P.O. ---------------------------------------
018000010522     C     ELAPO         BEGSR
018100071228     c                   if        DD3sst <> 'S'
018200071228     C                   reset                   $Opn
018300071228     c                   endif
018400071228     C                   Z-ADD     1             Z
018500010528     C     Z             DOWLE     30
018600010528    1C     L1(Z)         ANDGT     *ZEROS
018700010522     C     L1(Z)         LOOKUP    L6C                                    32
018800010522    2C     *IN32         IFEQ      *OFF
018900010522     C                   Z-ADD     L1(Z)         KLNP1
019000010524     C* Operazioni a rottura di P.O.
019100010528     C     DD3BRK        IFEQ      'S'
019200071228     c     DD3sst        orEQ      'S'
019300010524     C                   EXSR      BRKLNP
019400010528     C                   ENDIF
019500080128     c                   if        TotBol = *loval
019600080128     c                   clear                   TotBol
019700080128     c                   endif
019800010522     C****
019900081202     C     k03sgn12      SETLL     FISGN000
020000081202     C     k03sgn12      READE     FISGN000                               30
020100010522    3C     *IN30         DOWEQ     *OFF
020200010522     C                   EXSR      ELARCD
020300081202     C     k03sgn12      READE     FISGN000                               30
020400010522    3C                   ENDDO
020500010522     C* VEDO SE QUESTO P.O. GESTISCE ALTRI P.O.
020600010522     C                   EXSR      CARL6S
020700010522     C     Z6            DOWLE     30
020800010522    3C     L6S(Z6)       ANDGT     0
020900010524     C* DEVO ARE IL TEST SEGUENTE PERCHÈ SE PER IL P.O. NON ESISTE
021000010524     C* TABELLA £6 IL PGM TRUL06R RITORNA LA KEY IN PRIMA POSIZIONE
021100010524     C* DELLA SCHIERA ANCHE SE E' STATO RICHIAMATO CON D06ESC='S'
021200010522     C                   Z-ADD     L6S(Z6)       KLNP1
021300081202     C     k03sgn12      SETLL     FISGN000
021400081202     C     k03sgn12      READE     FISGN000                               30
021500010522    4C     *IN30         DOWEQ     *OFF
021600010522     C                   EXSR      ELARCD
021700081202     C     k03sgn12      READE     FISGN000                               30
021800010522    4C                   ENDDO
021900010522     C                   ADD       1             Z6
022000010522    3C                   ENDDO
022100010522    2C                   ENDIF
022200010522     C                   ADD       1             Z
022300010522    1C                   ENDDO
022400010522     C                   ENDSR
022500010522     C*
022600010522     C*-- ELABORAZIONE PER CODICE AUTOTRASPORTATORE -------------------
022700010522     C     ELAPDR        BEGSR
022800080110     c                   clear                   TotBol
022900081202     C     k04sgn12      SETLL     FISGN000
023000010522     C*
023100010522     C* SE HO UN SOLO COD.AUTOTRASPORTATORE LEGGO
023200010522    1C     *IN09         IFEQ      *OFF
023300081202     C     k04sgn12      READE     FISGN000                               30
023400010522   X1C                   ELSE
023500081202     C     k02sgn12      READE     FISGN000                               30
023600010522    1C                   ENDIF
023700010522    1C     *IN30         DOWEQ     *OFF
023800010522     C                   EXSR      ELARCD
023900010522    2C     *IN09         IFEQ      *OFF
024000081202     C     k04sgn12      READE     FISGN000                               30
024100010522   X2C                   ELSE
024200081202     C     k02sgn12      READE     FISGN000                               30
024300010522    2C                   ENDIF
024400010522    1C                   ENDDO
024500010522     C                   ENDSR
024600071227      *
024700010524     C*-- OPERAZIONI A ROTTURA DI P.O. --------------------------------
024800010524     C     BRKLNP        BEGSR
024900071228      *
025000071228if  1c                   IF        DD3sst =  'S'
025100071228      *
025200071228if  2c                   if        TotBol <> *loval
025300071228if  3c                   if        *inOF
025400071228     c                   except    TES
025500071228     c                   except    TES_Fil
025600071228     C                   eval      *inOF  =  *off
025700071228e   3c                   endif
025800071228     c                   except    TOT_Fil
025900071228e   2c                   endif
026000071228      *
026100071228x   1c                   ELSE
026200071228      *
026300010524     C* Operazioni su file printer
026400071228    3C     $Opn          IFEQ      *on
026500010524     C* Stampo ultimo totale bolle
026600010524    4C     *IN22         IFEQ      *ON
026700010524     C*
026800010524    5C     *INOF         IFEQ      *ON
026900010524     C                   EXCEPT    TES
027000071227     c                   except    Tes_B
027100010524     C                   SETOFF                                       OF
027200010524    5C                   ENDIF
027300010524     C                   EXCEPT    TOT
027400010524    4C                   ENDIF
027500010524     C* Stampa dicitura: FINE STAMPA
027600010524     C                   EXCEPT    FINEST
027700010524     C                   CLOSE     PRTF198
027800071228     C                   reset                   $Opn
027900010524    3C                   ENDIF
028000010524     C* ESEGUO OVRPRTF PER SPECIFICARE DATI UTENTE
028100010524     C     KLNP1         IFNE      SIMFEL
028200010524     C                   Z-ADD     40            LUNG2
028300071228     C                   MOVEA     $OVR(1)       COMMAN
028400071227     C                   MOVEL     SDSpgm        W011A            11
028500010524     C                   MOVEL     KLNP1         W004A             4
028600010524     C                   MOVE      ')'           W004A
028700010524     C                   MOVE      W004A         W011A
028800010524     C                   MOVE      W011A         COMMAN
028900010524     C                   CALL      'QCMDEXC'
029000010524     C                   PARM                    COMMAN           40
029100010524     C                   PARM                    LUNG2            15 5
029200010524     C                   ENDIF
029300010524     C* Apro file printer
029400010524     C                   OPEN      PRTF198
029500071228     C                   MOVE      *on           $Opn
029600010524     C                   SETOFF                                       22OF
029700010524     C* Stampo testata
029800010524     C                   EXCEPT    TES
029900071227     c                   except    Tes_B
030000071228      *
030100071228e   1c                   ENDIF
030200071228      *
030300010524     C* Decodifico il p.o.
030400071228     c                   eval      STAlnp = Klnp1
030500010524     C     KLNP1         CHAIN     AZORG01L                           32
030600010524     C  N32              MOVEL     ORGDES        DESLNP
030700010524     C   32              CLEAR                   DESLNP
030800071228     c                   if        DD3sst <> 'S'
030900010524     C                   EXCEPT    TES1
031000071227     c                   endif
031100080128      *
031200080128     c                   clear                   TotBol
031300080128      *
031400010524     C                   ENDSR
031500081202      /free
031600081202       //--------------------------------------------------------------
031700081202       //?Elaborazione record di FISGN
031800081202       //--------------------------------------------------------------
031900081202       BEGSR  ElaRcd;
032000081202
032100081202         //?Selezione record segnacollo?
032200081202         select;
032300081202
032400081202           // Scarta record annullati
032500081202           when  SGNatb <> *blank;
032600081202             leavesr;
032700081202
032800081202           // Scarta le bolle con LNP divera
032900081202           when  DD3po  <> *zero   and
033000081202                 SGNlnp <> DD3po;
033100081202             leavesr;
033200081202
033300081202           // Seleziona per CLIENTE
033400081202           when  %xfoot($Ksc)          <> *zero   and
033500081202                 %lookup(SGNksc : $Ksc) = *zero;
033600081202             leavesr;
033700081202
033800081202         endsl;
033900081202
034000081202         //?Chain bolla?
034100081202         chain  k04blp01  FNBLP000;
034200081202
034300081202         //?Selezione record bolla?
034400081202         select;
034500081202
034600081202           // Spedizione non trovata
034700081202           when  NOT %found(FNBLP01L);
034800081202             leavesr;
034900081202
035000081202           // Seleziona per data spedizione
035100081202           when  DD3dvd <> *zero    and
035200081202                (BLPdsp <  DD3dvd   or
035300081202                 BLPdsp >  DD3dva);
035400081202             leavesr;
035500081202
035600081202           // Seleziona per autotrasportatore
035700081202           when  DD3pdr <> *zero   and
035800081202                (BLPpdr <> DD3pdr  and
035900081202                 BLPpdr <> DD3pd2  and
036000081202                 BLPpdr <> DD3pd3  and
036100081202                 BLPpdr <> DD3pd4  and
036200081202                 BLPpdr <> DD3pd5);
036300081202             leavesr;
036400081202
036500081202         endsl;
036600081202
036700081202         //?Stampa?
036800081202         exsr  StaDet;
036900081202
037000081202       ENDSR;
037100081202      /end-free
037200071227      *
037300010514     C*--- POSIZIONAMENTO E LETTURA SU FISGN ------------------------*
037400010522     C     CARL6S        BEGSR
037500010514     C* SE HO APPENA ELABORATO UNA LINEA NON GESTITA CARICO LA SUA £6
037600010514     C* PER VEDERE SE ELABORARE DEI P.O. DA LEI GESTITI
037700071227     C                   CLEAR                   TRUL06ds
037800010514     C                   MOVE      '£6'          D06COD
037900010522     C                   MOVEL     KLNP1         D06KEY
038000010522     C                   MOVEL     'S'           D06ESC
038100071227     C                   MOVEL     TRUL06ds      KPJBU
038200010514     C                   CALL      'TRUL06R'
038300010514     C                   PARM                    KPJBA
038400071227     C                   MOVEL     KPJBU         TRUL06ds
038500010514     C                   MOVEA     LIN           L6S
038600010514     C                   SORTA     L6S
038700010522     C                   Z-ADD     1             Z6                3 0
038800010528     C     Z6            DOWLE     30
038900010528    2C     L6S(Z6)       ANDEQ     0
039000010522     C                   ADD       1             Z6
039100010514    2C                   ENDDO
039200010514     C                   ENDSR
039300960322     C*
039400960326     C*--- STAMPO DETTAGLIO ------------------------------------------*
039500960326     C     STADET        BEGSR
039600960329     C                   SETON                                        22
039700000225     C                   CLEAR                   STANPP
039800000225     C                   CLEAR                   STADFV
039900960326     C*
040000960326     C****  STAMPO TESTATA  ****
040100000224     C     *INOF         IFEQ      *ON
040200071228     c     DD3sst        andNE     'S'
040300960327     C                   EXCEPT    TES
040400071227     c                   except    Tes_B
040500960329     C                   SETOFF                                       OF
040600960326     C                   ENDIF
040700000225     C*
040800000225     C* CHAIN FNBLT
040900071227     C     KBLT          CHAIN     FNBLT000                           31
041000071228if  1C     *IN31         IFEQ      *OFF
041100041029     c* chaino anche art--> se c'e' i campi dcm e dam li controllo da
041200041029     c*  art invece che da blt
041300071227     c     kart          chain     fnart000
041400000225     C*
041500000225     C* GIRO DATA PARTENZA COLLO
041600071228if  2C     BLTDFV        IFGT      0
041700000225     C                   Z-ADD     BLTDFV        WDATA
041800071227     C                   Z-ADD     MM            GGMM
041900000225     C                   MOVEL     GG            GGMM
042000000225     C                   Z-ADD     AA            STADFV
042100071227     C                   MOVEL     GGMM          STADFV
042200071227     C                   MOVEL     BLTNPP        STANPP
042300071228e   2C                   ENDIF
042400010321      *
042500010321      * CONTROLLO BLTDAM E BLTDCM SE UNO DEI DUE DIVERSO DA ZERO NON DEVO
042600010321      * STAMPARE
042700010321     C                   SETOFF                                       20
042800071228if  2c                   if        %found(fnart01l)
042900041029     c
043000041029     c                   if        artdcm<>0 or artdam<>0
043100041029     C                   SETON                                        20
043200041029     c                   endif
043300071228x   2c                   else
043400041029     c
043500010321     C     BLTDAM        IFNE      *ZEROS
043600010321     C     BLTDCM        ORNE      *ZEROS
043700010321     C                   SETON                                        20
043800010321     C                   ENDIF
043900071228e   2C                   ENDIF
044000010321      *
044100071228e   1C                   ENDIF
044200960326     C*
044300960326     C****  STAMPO DETTAGLIO  ****
044400010321      *
044500071228if  1C     *IN20         IFEQ      *OFF
044600010321      *
044700071228if  2c                   if        DD3sst <> 'S'
044800071227      *
044900081202     C* 23 ON  - NUMERO SERIE <> 0  ---> STAMPO TRATTINO SEPARATORE
045000081202     C     BLPNRS        COMP      0                                  2323
045100960326     C*
045200960326     C* GIRO DATA SPEDIZIONE
045300000224     C                   Z-ADD     BLPDSP        WDATA
045400071227     C                   Z-ADD     MM            GGMM
045500000224     C                   MOVEL     GG            GGMM
045600000224     C                   Z-ADD     AA            STADSP
045700071227     C                   MOVEL     GGMM          STADSP
045800960326     C*
045900960401     C* RAGIONE SOCIALE MITTENTE E DESTINATARIO
046000071227     C                   MOVEL     BLPRSM        STARSM
046100071227     C                   MOVEL     BLPRSD        STARSD
046200071227     C                   MOVEL     BLPLOD        STALOD
046300000224     C*
046400021205     C                   move      BlpPdr        kpdpdr
046500021203     C                   move      'A'           kpdtip
046600071227     C     kfapd         chain     fiapd000                           31
046700071227     C  N31              MOVEL     APDRSC        STAPDR
046800071227      *
046900071228e   2c                   endif
047000960326     C*
047100071227     C                   ADD       1             TOTBOL
047200960329     C*
047300071228     c                   if        DD3sst <> 'S'
047400960326     C                   EXCEPT    DETT
047500071227     c                   endif
047600010321      *
047700071228e   1C                   ENDIF
047800010321      *
047900960326     C                   ENDSR
048000960326     C*
048100960321     C*--- SR INIZIALE -----------------------------------------------*
048200960321     C     *INZSR        BEGSR
048300010514     C*
048400960325     C****  PRENDO DATA E ORA CORRENTE  ****
048500960325     C                   TIME                    W0140            14 0
048600960325     C                   MOVE      W0140         UDATE8            8 0
048700960329     C                   MOVEL     W0140         UTIME             6 0
048800960329     C*
048900071227      * Reperimento dati job
049000071227     c     *dtaara       define    §azute        AZUTEds
049100071227     c     *dtaara       define    §datiute      dDATIUTE
049200071227      *
049300071227     c                   in(E)     *dtaara
049400071227     c                   IF        %ERROR or RSUT = *blanks
049500071227     c                   clear                   Tibs34Ds
049600071227     c                   call      'TIBS34R'
049700071227     c                   parm                    Tibs34Ds
049800071227     c                   in        *dtaara
049900071227     c                   ENDIF
050000960327     C*
050100960326     C****  PARAMETRI PASSATI  ****
050200960326     C     *ENTRY        PLIST
050300960326     C                   PARM                    KPJBA
050400071227     C                   MOVEL     KPJBU         FNLSD3ds
050500960329     C*
050600960329     C****  CODICE CLIENTE  ****
050700081202      /free
050800081202         *in13 = (DD3ksc <> *zero);
050900081202         *in14 = (DD3ks2 <> *zero);
051000081202         *in15 = (DD3ks3 <> *zero);
051100081202         *in16 = (DD3ks4 <> *zero);
051200081202         *in17 = (DD3ks5 <> *zero);
051300081202         *in18 = (DD3ks6 <> *zero);
051400081202         *in19 = (DD3ks7 <> *zero);
051500081202         *in20 = (DD3ks8 <> *zero);
051600081202         *in21 = (DD3ks9 <> *zero);
051700081202         *in22 = (DD3ksA <> *zero);
051800081202      /end-free
051900960329     C*
052000000217     C****  DATA SEDIZIONE   ****
052100000217     C* SE DATA SPEDIZIONE AL = 0 --> METTO DATA DAL IN DATA AL
052200071227     c                   eval      *in07 = (DD3dvd > *zeros)
052300000217    1C     DD3DVD        IFGT      0
052400000217     C     DD3DVA        IFEQ      0
052500000217     C                   Z-ADD     DD3DVD        DD3DVA
052600960328     C                   ENDIF
052700000217     C* GIRO DATA SPEDIZIONE DAL
052800000217     C                   Z-ADD     DD3DVD        WDATA
052900071227     C                   Z-ADD     MM            GGMM
053000960328     C                   MOVEL     GG            GGMM
053100960329     C                   Z-ADD     AA            STADVD
053200960329     C                   MOVEL     GGMM          STADVD
053300000217     C* GIRO DATA SPEDIZIONE  AL
053400000217     C                   Z-ADD     DD3DVA        WDATA
053500960329     C                   Z-ADD     MM            GGMM
053600960329     C                   MOVEL     GG            GGMM
053700960329     C                   Z-ADD     AA            STADVA
053800960329     C                   MOVEL     GGMM          STADVA
053900961016    1C                   ENDIF
054000961016     C*
054100000217     C****  COD.AUTOTRASP ****
054200071227     c                   eval      *in08 = (DD3pdr > *zeros)
054300071227     c                   eval      *in09 = (DD3pd2 > *zeros)
054400071227     c                   eval      *in10 = (DD3pd3 > *zeros)
054500071227     c                   eval      *in11 = (DD3pd4 > *zeros)
054600071227     c                   eval      *in12 = (DD3pd5 > *zeros)
054700000224    1C     DD3PDR        IFGT      0
054800071227     C                   Z-ADD     DD3PDR        KPDR
054900961016    1C                   ENDIF
055000010514     C*
055100010522     C* CARICO LA £1 SE NON E' STATO RICHIESTO UN P.O. SPECIFICO
055200010522     C     DD3PO         IFEQ      0
055300071227     C                   CLEAR                   TRUL06ds
055400010522     C                   MOVE      '£1'          D06COD
055500020508     c                   movel     simfel        d06key
055600071227     C                   MOVEL     TRUL06ds      KPJBU
055700010522     C                   CALL      'TRUL06R'
055800010522     C                   PARM                    KPJBA
055900071227     C                   MOVEL     KPJBU         TRUL06ds
056000010522     C                   MOVEA     LIN           L1
056100010522     C                   ELSE
056200010522     C                   MOVEL     DD3PO         L1(1)
056300010522     C                   ENDIF
056400020508      * per ogni elemento di £1 carico la £6
056500020508     c                   clear                   wc
056600020508     c                   do        30            wa
056700020508     c                   if        l1(wa) = *zeros
056800020508     c                   leave
056900020508     c                   endif
057000020508     c                   clear                   kpjbu
057100071227     c                   clear                   TRUL06ds
057200020508     c                   eval      d06cod = '£6'
057300020508     c                   movel     l1(wa)        d06key
057400020508     c                   eval      d06esc = 'S'
057500071227     c                   movel     TRUL06ds      kpjbu
057600020508     c                   call      'TRUL06R'
057700020508     c                   parm                    kpjba
057800071227     c                   movel     kpjbu         TRUL06ds
057900020508 b3  c                   do        30            wb
058000020508     c                   if        lin(wb) = *zeros
058100020508     c                   leave
058200020508     c                   endif
058300020508     c                   add       1             wc
058400020508     c                   z-add     lin(wb)       l6c(wc)
058500020508 e3  c                   enddo
058600020508 e2  c                   enddo
058700010522     C* ACCENDO INDICATORE DI OVERFLOW
058800010528     C* ESEGUO OVRPRTF PER SPECIFICARE DATI UTENTE
058900071228     c     *in08         IFEQ      *on
059000071228     C     DD3PO         andNE     SIMFEL
059100010528     C                   Z-ADD     40            LUNG2
059200071228     C                   MOVEA     $OVR(1)       COMMAN
059300071227     C                   MOVEL     SDSpgm        W011A            11
059400010528     C                   MOVEL     DD3PO         W004A             4
059500010528     C                   MOVE      ')'           W004A
059600010528     C                   MOVE      W004A         W011A
059700010528     C                   MOVE      W011A         COMMAN
059800010528     C                   CALL      'QCMDEXC'
059900010528     C                   PARM                    COMMAN           40
060000010528     C                   PARM                    LUNG2            15 5
060100010528     C                   ENDIF
060200010528     C     *IN08         IFEQ      *ON
060300010528     C     DD3BRK        OREQ      ' '
060400010528     C                   OPEN      PRTF198
060500071228     C                   MOVE      *on           $Opn
060600010528     C                   SETON                                        OF
060700010528     C                   ENDIF
060800960329     C*
060900920925     C                   ENDSR
061000960326     C*
061100960325     O*---------------------------------------------------------------*
061200081202      ****  TESTATA  ****
061300000224     OPRTF198   E            TES              02
061400071228     O                       $ST(1)             132
061500071228     O                       $ST(2)             198
061600960328     O                       RSUT                20
061700960328     O                                           21 '/'
061800010522     O                       SIMFEL              24
061900071227     O                       SDSpgm             153
062000000218     O                       UDATE8             178 '  /  /    '
062100000218     O                       PAGE          Z    198
062200081202     o          e            Tes         0  0
062300081202     O                       $ST(1)             132
062400081202     O          E            TES         1
062500960328     O                       KNSIF               10
062600960328     O                       KNMUS               24
062700000224     O                       UTIME              178 '  :  :  '
062800081202     O          E            TES         2
062900081202     O                       $ST(3)
063000081202     O                       $ST(4)            +  0
063100081202     o                       DD3pdr        z     26
063200081202     o                       DD3pd2        z   +  1
063300081202     o                       DD3pd3        z   +  1
063400081202     o                       DD3pd4        z   +  1
063500081202     o                       DD3pd5        z   +  1
063600081202     o                       STAdvd              91 '  /  /    '
063700081202     o                       STAdva             105 '  /  /    '
063800081202     o                       DD3ksc        z    124
063900081202     o                       DD3ks2        z   +  1
064000081202     o                       DD3ks3        z   +  1
064100081202     o                       DD3ks4        z   +  1
064200081202     o                       DD3ks5        z   +  1
064300081202     o                       DD3ks6        z   +  1
064400081202     o                       DD3ks7        z   +  1
064500081202     o                       DD3ks8        z   +  1
064600081202     o                       DD3ks9        z   +  1
064700081202     o                       DD3ksA        z   +  1
064800081202      *
064900081203     O          E            TES_B       2
065000071228     O                       $ST(5)              66
065100071228     O                       $ST(8)             132
065200071228     O                       $ST(11)            198
065300081202     o          e            Tes_b       0  0
065400081202     o                       $ST(5)              66
065500081202     o                       $ST(8)             132
065600081202     o                       $ST(11)            198
065700081202     O          E            TES_B       1
065800071228     O                       $ST(6)              66
065900071228     O                       $ST(9)             132
066000071228     O                       $ST(12)            198
066100081202     o          e            Tes_b       0  1
066200081202     o                       $ST(6)              66
066300081202     o                       $ST(9)             132
066400081202     o                       $ST(12)            198
066500081203      *
066600081203     o          e            TES_Fil     2
066700071228     o                       $ST(15)             66
066800071228     o          e            TES_Fil     1
066900071228     o                       $ST(16)             66
067000081202      * Filiale
067100081203     O          E            TES1        1  1
067200071227     O                                           16 'FILIALE:'
067300010524     O                       KLNP1             +  1
067400010524     O                       DESLNP            +  1
067500081202      ****  DETTAGLIO  ****
067600960328     O          E            DETT        1
067700000224     O                       BLPPDR
067800000224     O                                         +  0 '-'
067900000224     O                       STAPDR            +  0
068000000224     O                       BLPLNP        Z   +  1
068100081202     O               23      BLPNRS        Z   +  1
068200081202     O               23                        +  0 '-'
068300000224     O                       BLPNSP        Z   +  0
068400000224     O                       STADSP            +  1 '  /  /    '
068500000224     O                       BLPLNA            +  1
068600000224     O                       SGNKSC            +  2
068700000224     O                                         +  0 '-'
068800000224     O                       STARSM            +  0
068900000224     O                       STARSD            +  1
069000000224     O                       BLPPRD            +  1
069100000224     O                       BLPNZD            +  1
069200000224     O                       BLPRMN        Z   +  1
069300000224     O                       BLPRMA            +  1
069400000225     O                       SGNWHO            +  1
069500010321     O                       SGNNCD        Z   +  1
069600010321     O                       STADFV            +  1 '  /  /  '
069700000225     O                       STANPP            +  1
069800081202      *
069900081202      ****  TOTALE BOLLE VARIATE PER BOLLETTISTA  ****
070000081202     O          E            TOT         2  0
070100000224     O                                           25 'TOTALE COLLI :'
070200960328     O                       TOTBOL        2     37
070300081202     o          e            Tot         0  3
070400081202     o                                           25 'TOTALE COLLI :'
070500081202     o                       TotBol        2     37
070600071228     o          e            Tot_Fil     2
070700071228     o                       STAlnp
070800071227     o                       DESlnp            +  1
070900071227     o                       TOTbol        k   +  5
071000081202      ****  FITTIZZIO PER IL SALTO GIUSTO DELLE RIGHE  ****
071100000224     O          E            FITT             20
071200960329     O                                              ' '
071300081202      ****  DICITURA FINE STAMPA  ****
071400960329     O          E            FINEST      2
071500071228     O                       $ST(14)            132
071600081202     o          e            FineSt      0
071700081202     o                       $St(14)            132
071800081202
071900081202** - $ST --------------------------------------------------------*
072000081202 ***   ELENCO SEGNACOLLI NON SPUNTATI DISK  "C"   ***               1
072100000218                                                         PAG. XXXX  2
072200081202Autotrasportatori: 1234567 1234567 1234567 1234567 1234567   Data   3
072300081202spedizione dal __/__/____ al __/__/____   Clienti: 1234567 1234567  4
072400000225  AUTOTRASPORTATORE     S  P  E  D  I  Z  I  O  N  E      M I T T   5
072500000224                        LNP   NUMERO     DATA     LNA               6
072600000224XXXXXXX-XXXXXXXXXXXXXXX XXX XX-XXXXXXX XX/XX/XXXX XXX  XXXXXXX-XXX  7
072700000225E N T E             D E S T I N A T A R I O        R I F E R I M E  8
072800000225                 RAGIONE SOCIALE     PRV NAZ        NUMERICO ALFAB  9
072900000225XXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXX XX XXX XXXXXXXXXXXXXXX XXXXX  10
073000010321 N T I     "C H I    S O N O"                   NUMERO   PARTENZA   11
073100010321ETICO      IDENTIFICATIVO COLLO                 SEGNAC.  DATA  PIS  12
073200010321XXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXX XX/XX/XX XX  13
073300000224            * * *    F I N E    S T A M P A    * * *                14
073400071227FILIALE                   TOT. SPEDIZIONI                           15
073500071228------------------------  ---------------                           16
073600071228123 12345678901234567890     123.456.789-                           17
073700071228** - $OVR
073800010524OVRPRTF FILE(PRTF198) USRDTA(FNLSD3R   )
