000100141127      *
000200970214     H DECEDIT('0,') DATEDIT(*DMY.)
000300940223      *
000400940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000500940620      *  03           PROTECT TUTTI I CAMPI
000600090608      *  19           se Contratto RISTAMPATO
000700940223      *  21           GENERICO OPERAZIONI I/O
000800940223      *  22           GENERICO ERRORE OPERAZIONI I/O
000900940223      *  30           SFLDSP
001000940223      * N31           SFLCLR
001100940128      *  31           SFLDSPCTL
001200940128      *  32           SFLNXTCHG
001300940128      *  33           SFLEND
001400940128      *  39           OF PRTF
001500940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001600940317    > *  Specificare l'uso dei singoli indicatori
001700940315      *  50 <---> 98  ERRORI SU VIDEO
001800940317    > *  Specificare l'uso dei singoli indicatori
001900940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002000940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002100940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002200940128      *  99           INDIC. GENERALE DI ERRORE
002300940128     F*----------------------------------------------------*
002400090827     FAITRS00f  iF   E             DISK
002500090827     FAitra00f  iF   E             DISK
002600090331      *
002700090827     FAitra01l  iF   E           k DISK
002800090324     F                                     rename(Aitra000:Aitra001)
002900090319      *
003000090506     FfiADT01L  iF   E           K DISK
003100090506     FfiTGT01L  iF   E           K DISK
003200090319     FAzorg01L  iF   E           K DISK
003300090416     Ftabel00F  iF   E           K DISK
003400120217     FtnTBE01l  iF   E           K DISK
003500090409     FanRCO98j  iF   E           k disk
003600090317      *
003700090827$001 FTRMZ65SD2 CF   E             WORKSTN
003800940201     F                                     INFDS(DSFMT)
003900940128     D*----------------------------------------------------*
004000940127     D* Passaggio Parametri
004100940127     D KPJBA         E DS
004200090320     D KPJBus          S                   like(KPJBU)
004300090319     D tibs02ds      E DS
004400090408     D trmz70s1ds    E DS                  prefix(S1_)
004500090506     D trmz70s4ds    E DS                  prefix(S4_)
004600090522     D parm_s4CDF      s                   like(tgtCDF)
004700090522     D parm_s4SOC      s                   like(tgtSOC)
004800090522     D parm_s4FIL      s                   like(tgtFIL)
004900090319     d daip          e ds
005000090416     d dsPR          e ds
005100090608     d dtrsflr       E DS
005200150305     d dtrsflr1      E DS
005300160208     d dsrg          e ds
005400030113      *-------------
005500030113     D* Parametri in ricezione
005600090311     D XTABDS          DS
005700090311     D  XTAOPZ                 1      1
005800090311     D  XTAFLG                 2      2
005900030113     D  XTARET                 3      3
006000030113     D  XTAOPR                 4      4
006100030113     D  XTAERR                 5      5
006200090319     D  XTArecord              6     14  0
006300090319     D  XTAKEY                15     21  0
006400940201     D*-------------
006500090320     D* Parametri in ricezione x selezionare l'Autista di AITRA da modificare
006600090320     D ataBDS          DS
006700090320     D  ataOPZ                 1      1
006800090320     D  ataFLG                 2      2
006900090320     D  ataRET                 3      3
007000090320     D  ataOPR                 4      4
007100090320     D  ataERR                 5      5
007200090320     D  atarecord              6     14  0
007300090320     D  ataKEY                15     21  0
007400090320     D*-------------
007500030204     D  S_TASTO        S                   like($Tasto)
007600940201     D DSFMT           DS
007700940506     D  $TASTO               369    369
007800940201     D  NRG                  370    370
007900940201     D  NCL                  371    371
008000940201     D*-------------
008100940201     D* posizione cursore
008200940201     D CURSOR          DS
008300940223     D  RIRI                   1      2B 0 INZ
008400940201     D  R$$                    2      2
008500940223     D  COCO                   3      4B 0 INZ
008600940201     D  C$$                    4      4
008700940207     D*-------------
008800940207     D* Reperimento nome PGM
008900940207     D STATUS         SDS           333
009000940207     D  DSPGM            *PROC
009100940225     D*-------------
009200940225     D* COSTANTI
009300940225     D*-------------
009400940506     D* Tasti di funzione
009500940506     D F01             C                   CONST(X'31')
009600940506     D F02             C                   CONST(X'32')
009700940506     D F03             C                   CONST(X'33')
009800940506     D F04             C                   CONST(X'34')
009900940506     D F05             C                   CONST(X'35')
010000940506     D F06             C                   CONST(X'36')
010100940506     D F07             C                   CONST(X'37')
010200940506     D F08             C                   CONST(X'38')
010300940506     D F09             C                   CONST(X'39')
010400940506     D F10             C                   CONST(X'3A')
010500940506     D F11             C                   CONST(X'3B')
010600940506     D F12             C                   CONST(X'3C')
010700940506     D F13             C                   CONST(X'B1')
010800940506     D F14             C                   CONST(X'B2')
010900940506     D F15             C                   CONST(X'B3')
011000940506     D F16             C                   CONST(X'B4')
011100940506     D F17             C                   CONST(X'B5')
011200940506     D F18             C                   CONST(X'B6')
011300940506     D F19             C                   CONST(X'B7')
011400940506     D F20             C                   CONST(X'B8')
011500940506     D F21             C                   CONST(X'B9')
011600940506     D F22             C                   CONST(X'BA')
011700940506     D F23             C                   CONST(X'BB')
011800940506     D F24             C                   CONST(X'BC')
011900940506     D ENTER           C                   CONST(X'F1')
012000940506     D ROLDWN          C                   CONST(X'F4')
012100940506     D ROLLUP          C                   CONST(X'F5')
012200940506     D*-------------
012300941108     D DATA            C                   CONST('0001-01-01')
012400030114     D Annull          C                   CONST('  ANNULLATO  ')
012500090318     D OPz_Vis         C                   CONST('     VISUALIZZA     ')
012600090827     D OPz_DETaut      C                   CONST(' DETTAGLIO  AUTISTI ')
012700090319      *
012800090319      * Opzioni --> Azioni da fare :
012900090319     D Visualizza      C                   CONST('5')
013000090827     D DetAutisti      C                   CONST('D')
013100090319     D Acceso          C                   '1'
013200090319     D Spento          C                   '0'
013300090323     D*-------------
013400090323      * Controllo IVA
013500151117     d xcfiva1ds     e ds
013600090323     d  ivaeu                  3      4
013700090323     d  ivait                  5     18
013800090317
013900090408      *----
014000090319     d Trovato_TRS     s              1    inz('N')
014100090319     d Trovato_AITRA   s              1    inz('N')
014200090317      *
014300090317     D Wsocg           S                   like(d1socG)
014400090317     D Wsocgd          S                   like(d1dsocG)
014500090317     d dataiso         s               d   datfmt(*iso)
014600090317     d dataeur         s               d   datfmt(*eur)
014700090323     d Data_oggi       s              8  0
014800090317     ***************************************************************************
014900090317     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
015000090317     D ESITO_OK...
015100090317     D                 C                   0
015200090317     ***************************************************************************
015300090317     **
015400090317     ** Dichiarazione prototipi procedure esterne.
015500090317     **
015600090317     ***************************************************************************
015700090317      /DEFINE DFTACTGRP_YES
015800090317     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
015900090317      /UNDEFINE DFTACTGRP_YES
016000090317     ***************************************************************************
016100090317     **
016200090317     ** Definizione strutture dati.
016300090317     **
016400090317      **************************************************************************
016500090317     D tibsSocI0...
016600090317     D               E DS                  QUALIFIED
016700090317     D                                     INZ
016800090317     D tibsSocO0...
016900090317     D               E DS                  QUALIFIED
017000090317     D                                     INZ
017100090317     ***************************************************************************
017200090317     **
017300090317     ** Definizione variabili modulo/programma.
017400090317     **
017500090317     ***************************************************************************
017600090317     D prmRqsOpCode...
017700090317     D                 S             10A
017800090317     D prmRpyOpCode...
017900090317     D                 S             10A
018000090317     D prmRpyIdMsg...
018100090317     D                 S             10I 0
018200090317     D prmRqsFormato...
018300090317     D                 S             10A
018400090317     D prmRqsDataSize...
018500090317     D                 S             10I 0
018600090317     D prmRpyFormato...
018700090317     D                 S             10A
018800090317     D prmRpyDataSize...
018900090317     D                 S             10I 0
019000090317
019100090409     d digits          c                   '0123456789'
019200090827     C*----------------------------------------------------*
019300090827     C*                MAIN LINE PROGRAM
019400090827     C*----------------------------------------------------*
019500090827     ** Inizializzo il programma.
019600090827     C                   CALL      'TIBSSOCR'
019700090827     C                   PARM      'INIT'        prmRqsOpCode
019800090827     C                   PARM                    prmRpyOpCode
019900090827     C                   PARM                    prmRpyIdMsg
020000090827     C*
020100090827      * ? * Entra nella sequenza delle videate *
020200090827     C     $FINE         DOWEQ     *OFF
020300090827     C     $GEST         CASEQ     'D1'          GESD1
020400090827     C     $GEST         CASEQ     'D2'          GESD2
020500090827     C                   END
020600090827     C                   END
020700090827     C*
020800090827     ** Chiudo il programma.
020900090827     C                   CALL      'TIBSSOCR'
021000090827     C                   PARM      'FINALIZE'    prmRqsOpCode
021100090827     C                   PARM                    prmRpyOpCode
021200090827     C                   PARM                    prmRpyIdMsg
021300090827     C*
021400090827     C* fine programma
021500090827     c                   movel     Xtabds        kpjbu
021600090827     C                   Seton                                        LR
021700090827     C************************************************************
021800090827      *?  OPERAZIONI INIZIALI                             ?
021900090827     C************************************************************
022000090827     C     *INZSR        BEGSR
022100090827     C*
022200090827     C* Reperimento parametri
022300090827     C     *ENTRY        PLIST
022400090827     C                   PARM                    KPJBA
022500090827     C                   movel     kpjbu         Xtabds
022600090827     c                   setoff                                       23
022700090827     C*
022800090827     C* Variabili per gestione videate
022900090827     C                   MOVE      *OFF          $FINE
023000090827     C                   MOVE      *ON           $INZD1
023100090827     C                   MOVE      *BLANK        $LASTG
023200090827     C                   MOVE      *BLANK        $LASTV
023300090827     C                   Z-ADD     0             $ULKD1            3 0
023400090827     C*
023500090827     C* Variabili appoggio
023600090827     C                   Z-ADD     0             CURR
023700090827     C                   Z-ADD     0             CURC
023800090827     C*
023900090827     C* Valorizzazione campi univoci testate
024000090827     C*
024100090827     C* ESSENDO SEQUENZIALI I 3 DISPLAY LI PULISCO TUTTI UNA VOLTA SOLA
024200090827     C* Pulisce entrambe i 3 video
024300090827     C                   CLEAR                   D1
024400090827     C                   CLEAR                   D2
024500090827     C*
024600090827     C                   MOVEL     KNSIF         NOMSIF
024700090827     C                   MOVEL     DSPGM         NOMPGM           10
024800090827     C*
024900090827     C*  giro la udate da GMA in AMG
025000090827     C                   move      *date         dataeur
025100090827     C                   move      dataeur       dataiso
025200090827     C                   move      dataiso       data_oggi
025300090827     C*
025400090827     C* Inizializzazione/Decodifica prima videata e seconda
025500090827     C                   MOVE      'D1'          $GEST
025600090827     C                   MOVE      *ON           $INZD1
025700090827     C                   MOVE      *OFF          $INZD2
025800090827     C*
025900090827     C                   ENDSR
026000940131     C************************************************************
026100090319      *?  GESTIONE VIDEO RECORD D1                               ?
026200940131     C************************************************************
026300940127     C     GESD1         BEGSR
026400030113      *
026500940225     C* inizializzazione videata
026600090319     C* settaggi 1°video campi attivi o campi protetti
026700090319     C     $INZD1        IFEQ      *On
026800940127     C                   EXSR      INZD1
026900940223     C                   MOVE      *OFF          $INZD1
027000940117     C                   END
027100090827      *
027200090827     c                   goto      no_video1
027300090827      *
027400090827     C*   solo non in Visualizzazione
027500090827    >C                   if        xtaopz <> Visualizza
027600030113     C*               *----------------*
027700940620     C                   EXFMT     D1
027800030113     C*               *----------------*
0279000301141    C                   SELECT
028000030114     C* F3=Fine
028100030114     C     $TASTO        WHENEQ    F03
028200090319     C                   EXSR      F03Fine
028300090319     c                   leaveSR
028400030114     C* F12=Ritorno
028500030114     C     $TASTO        WHENEQ    F12
028600090319     C                   EXSR      F12Ritorna
028700090319     c                   leaveSR
028800030114     C*
0289000903202-   C                   ENDSL
029000030114     C*
029100030114     C                   EXSR      CTRD1
029200090319     c                   end
029300090827     C*
029400090827     c     no_video1     tag
029500090827     C*
029600090319     C* Video 2 o Video 3
029700090319      *  Dati Societari
029800090319     C                   EVAL      $GEST ='D2'
029900090319     C                   EVAL      $INZD2= *ON
030000090319      *
030100940117     C                   ENDSR
030200940117      ************************************************************
030300090319      *?  INIZIALIZZAZIONE VIDEATA 1                       ?
030400940117      ************************************************************
030500940127     C     INZD1         BEGSR
030600090319     C*
030700090312     C                   MOVEL     DSPGM         d1PGM
030800090317     C                   z-add     xtakey        d1NRC
030900090317     C                   z-add     xtakey        H1NRC
031000090320     C                   z-add     xtarecord     H1recTRS
031100090319     c                   move      'N'           trovato_TRS
031200090319     C*
031300090319      * ? * Reperisce informazioni della società solo se non in IMMISSIONE *
031400090319      *                   il record di AITRS00F ?
031500090320     c                   exsr      Quale_AITRS
031600090319     C                   IF        Trovato_TRS = 'S'
031700090320     C*
031800090320     c                   eval      d1fil    = trsFIL
031900090320     c                   eval      d1SOCG   = trsSOCG
032000090320     c                   eval      d1IVA    = trsIVA
032100090319     C*
032200090319      * ? * richiamo routine dei ctrl per decodifiche e controlli
032300940614    >C                   EXSR      CTRD1
0324000903172-   C                   ENDIF
032500940224     C*
032600940117     C                   ENDSR
032700940207     C/EJECT
032800090320     C************************************************************
032900090827      *?  Decodifiche 1 Video                              ?
033000090320     C************************************************************
033100090320     C     CTRD1         BEGSR
033200090320     C*
033300940127     C                   SETOFF                                       99
033400090317     C*
033500090317     C* controllo codice vuoto  Filiale
033600090317     c                   clear                   d1dfil
033700090827$017 C     D1fil         IFne      *zeros
033800090410     c     D1Fil         chain     azorg01l
033900090410     c                   if        %Found(azorg01l) and orgfag = 'F'
034000090317     c                   eval      d1dfil = orgdes
034100090317     c                   end
034200090317     C                   ENDIF
034300090317     C*
034400090317     C* controllo codice vuoto  Società
034500090317     c                   clear                   d1dsocg
034600090827$017 C     D1socG        IFne      *blank
034700090317     c                   eval      wsocG = d1socG
034800090317     c                   exsr      decod_societa
034900090317     c                   if        prmRpyIdMsg < 0
035000090402     c                              or TIBSSOCO0.IDSOCIETA = *blank
035100090317     c                   else
035200090317     c                   eval      d1dsocG = wSocGd
035300090317     c                   end
035400090317     C                   ENDIF
035500030114     C*
035600090317     C* controllo codice vuoto P.IVA
035700090827$017 C     D1IVA         IFne      *blank
035800090323      **
035900090323      ** Controllo Formale P.IVA
036000090323      *  richiama il nuovo driver fatto per controllare il codice
036100090323      * fiscale o la partita iva
036200151117     c                   clear                   xcfiva1ds
036300090323     c                   eval      xcfivapar = D1iva
036400151117     c                   call      'XCFIVAR1'
036500151117     c                   parm                    xcfiva1ds
036600090323      * --> errore
036700090827     c                   if        XCFIVAFLG >= *zeros
036800090323      **
036900090317     c                   eval      trsIVA  = d1IVA
037000090317     c                   eval      trsSocG = d1SocG
037100090317     c                   eval      trsFil  = d1Fil
037200090317     c                   exsr      Decod_RAG_IVA
037300090319      *
037400090408     c                   if        S1_Trovato = *Off or S1_Errore  <> '0'
037500090317     C                   Else
037600090408     C                   eval      d1ProJ = s1_RagSocKSC
037700090408     C                   eval      d1ProJ1= s1_RagSocKSC1
037800170320     C                   eval      d1ProJ2= s1_INDIRIZZO
037900170320     C                   eval      d1ProJ3= s1_CAP + s1_LOCALITA + '(' +
038000170320     c                                s1_PROVINCIA + ')'
038100090320     C                   End
038200090827      *
038300090323     C                   End
038400090317      *
0385009402241-   C                   END
038600090323     C*
038700940117     C                   ENDSR
038800090319     C************************************************************
038900090319      *?  GESTIONE VIDEO RECORD D2                               ?
039000090319     C************************************************************
039100090319     C     GESD2         BEGSR
039200090319      *
039300090319     C* inizializzazione videata
039400090319     C* settaggi 2°video campi attivi o campi protetti
039500090319     C     $INZD2        IFEQ      *ON
039600090319     C                   EXSR      INZD2
039700090319     C                   MOVE      *OFF          $INZD2
039800090319     C                   END
039900090327      *
040000090319      *  emette il secondo video dei dati società
040100090827    >C     xtaopz        ifNE      Visualizza
040200090827      *
040300090827     c                   exsr      quale_aitra
040400090827    >C                   eval      xtaopz = Visualizza
040500090827     C                   eval      $TASTO = F12
040600090827      *
040700090827     c                   else
040800090319     C*               *----------------*
040900090319     C                   EXFMT     D2
041000090319     C*               *----------------*
041100090827     c                   end
041200090319     C*
0413000903191    C                   SELECT
041400120104     C* F1=Appendici
041500120104     C     $TASTO        WHENEQ    F01
041600120104     C                   EXSR      F01appendici
041700120104     c                   leaveSR
041800090402     C* F3=Fine
041900090402     C     $TASTO        WHENEQ    F03
042000090402     C                   EXSR      F03Fine
042100090402     c                   leaveSR
042200090319     C* F12=Ritorno
042300090319     C     $TASTO        WHENEQ    F12
042400090319     C                   EXSR      F12Ritorna
042500090319     c                   leaveSR
042600090319     C*
0427000903202-   C                   ENDSL
042800090319     C*
042900090319    >C                   EXSR      CTRD2
043000090319      *
043100090319     C                   ENDSR
043200090319      ************************************************************
043300090319      *?  INIZIALIZZAZIONE VIDEATA 2                       ?
043400090319      ************************************************************
043500090319     C     INZD2         BEGSR
043600090319     C*
043700120104     c                   setoff                                       19
043800120104     c                   eval      dtrsflr = trsflr
043900120104     c                   if         §trsris = 'R'
044000120104     c                   setoN                                        19
044100120104     c                   end
044200090320     C                   z-add     H1recTRS      H2recTRS
044300090317     c                   eval      d2PGM  =  D1PGM
044400090410     C*
044500090317     c                   eval      d2NRC  =  D1Nrc
044600090319     c                   eval      H2NRC  =  H1Nrc
044700090317     c                   eval      D2Ann  =  D1Ann
044800090317     c                   eval      D2Fun  =  D1Fun
044900090317     c                   eval      D2FIL  =  D1FIL
045000090317     c                   eval      D2DFIL =  D1DFIL
045100090317     c                   eval      D2SOCG =  D1SOCG
045200090317     c                   eval      D2DSCG =  D1DSoCG
045300090317     c                   eval      D2IVA  =  D1IVA
045400090408     C                   eval      d2ProJ = s1_RagSocKSC
045500090408     C                   eval      d2ProJ1= s1_RagSocKSC1
045600170320     c                   if        d2ProJ1 <> *blank
045700170320     C                   eval      d2ProJ2= s1_INDIRIZZO
045800170320     C                   eval      d2ProJ3= s1_CAP + s1_LOCALITA + '(' +
045900170320     c                                s1_PROVINCIA + ')'
046000170320     c                   else
046100170320     C                   eval      d2ProJ1= s1_INDIRIZZO
046200170320     C                   eval      d2ProJ2= s1_CAP + s1_LOCALITA + '(' +
046300170320     c                                s1_PROVINCIA + ')'
046400170320     c                   clear                   d2proj3
046500170320     c                   end
046600090409      *------
046700090319     c                   if        trsdec > 0
046800090319     c                   move      TRsDec        dataiso
046900090319     c                   move      dataiso       dataeur
047000090319     c                   move      dataeur       d2DEC
047100090324     c                   move      TRsDec        h2DEC
047200090407     c                   move      TRsDec        h2savdec
047300090319     c                   endif
047400090319      *
047500090319     c                   if        trsdfc > 0
047600090319     c                   move      TRsDfc        dataiso
047700090319     c                   move      dataiso       dataeur
047800090319     c                   move      dataeur       d2DfC
047900090324     c                   move      TRsDfc        h2DfC
048000090319     c                   endif
048100090319
048200090319     c                   if        trsdrc > 0
048300090319     c                   move      TRsDrc        dataiso
048400090319     c                   move      dataiso       dataeur
048500090319     c                   move      dataeur       d2DrC
048600090324     c                   move      TRsDrc        h2DrC
048700090319     c                   endif
048800090319
048900090522     c                   if        trsdsc > 0
049000090319     c                   move      TRsDsc        dataiso
049100090319     c                   move      dataiso       dataeur
049200090319     c                   move      dataeur       d2DsC
049300120104     c                   move      TRsDsc        h2DsC
049400120104     c  n19              if        §trsunodc = *zeros or
049500120104     c                             §trsunodc =  *blank
049600120104     c                   move      dataeur       d2unodc
049700120104     c                   endif
049800090319     c                   endif
049900120104
050000120104     c                   if        §trsunodc <> *zeros and
050100120104     c                             §trsunodc <> *blank
050200120104     c                   move      §trsunodc     §trsunodc0        8 0
050300120104     c                   move      §trsunodc0    dataiso
050400120104     c                   move      dataiso       dataeur
050500120104     c                   move      dataeur       d2Unodc
050600120104     c                   move      §trsunodc     h2Unodc
050700120104     c                   endif
050800120104     c                   if        d2unodc = d2dsc
050900120104     c                   clear                   d2dsc
051000120104     c                   endif
051100120217
051200120217      *
051300120217      *    riporta sulla DS i campi del filler
051400120217     c                   eval      dTRSflr = TRSflr
051500150305     c                   eval      dTRSflr1= TRSflr1
051600150305      *
051700150305      ** DATA    D.U.R.C.
051800150305     c                   if        §trsdDURC <> *zeros and
051900150305     c                             §trsdDURC <> *blank
052000150305     c                   move      §trsdDURC     §trsdDURC0        8 0
052100150305     c                   move      §trsdDURC0    dataiso
052200150305     c                   move      dataiso       dataeur
052300150305     c                   move      dataeur       d2dDURC
052400150305     c                   move      §trsdDURC     h2dDURC
052500150305     c                   endif
052600150305      **
052700150305      ** DATA    C.I.P.
052800150305     c                   if        §trsdCIP  <> *zeros and
052900150305     c                             §trsdCIP  <> *blank
053000150305     c                   move      §trsdCIP      §trsdCIP0         8 0
053100150305     c                   move      §trsdCIP0     dataiso
053200150305     c                   move      dataiso       dataeur
053300150305     c                   move      dataeur       d2dCIP
053400150305     c                   move      §trsdCIP      h2dCIP
053500150305     c                   endif
053600160208     c                   eval      d2COR = TRSCOR
053700160208     c                   clear                   d2cord
053800160208     c                   if        d2cor <> *blank
053900160208     c                   movel     'SRG'         TBECOD
054000160208     c                   movel(p)  d2cor         TBEke1
054100160208     c                   clear                   TBEke2
054200160208     c     k_TBE         chain     tnTBE01l
054300160208     c                   if        %Found(tnTBE01L)
054400160208     c                   movel     tbeuni        dsrg
054500160208     c                   movel     §srgdes       d2cord
054600160208     c                   end
054700160208     c                   end
054800090319
054900090319     c                   eval      d2CIT = TRSCIT
055000090319     c                   eval      d2NIS = TRSNIS
055100090319     c                   eval      d2PIA = TRSPIA
055200090319     c                   eval      d2NIA = TRSNIA
055300090319
055400090319     c                   if        trsdia > 0
055500090319     c                   move      TRsDia        dataiso
055600090319     c                   move      dataiso       dataeur
055700090319     c                   move      dataeur       d2Dia
055800090319     c                   endif
055900090319     C*
056000090319      * ? * richiamo routine dei ctrl per decodifiche e controlli
056100090319    >C                   EXSR      CTRD2
056200090409     C*
056300090409     C*  comunque decodifica dal forntitore i campi:
056400090409     C*    Registro Imprese, Codice Fiscale, Provincia e Città e se
056500090409     C*    il Codice Fiscale è alfanumerico imposta la Ragione Sociale del
056600090409     C*    Fornitore nel Legale Rappresentante.
056700090409     c                   clear                   CaratteriAlfa     1
056800090409      *------
056900090409     c                   if        d2NIS = *blank
057000090409     c                   eval      d2NIS = S1_CodiceFisc
057100090409      *  controllo validità distinta
057200090417     c                   clear                   dove              3 0
057300090417     c     digits        check     d2NIS:1       dove
057400090417     c                   if        dove > 0
057500090409     c                   move      'S'           CaratteriAlfa     1
057600090409     c                   end
057700090409     c                   endif
057800090409      *
057900090409     c                   if        d2PIA = *blank
058000090409     c                   eval      d2PIA = S1_Provincia
058100090409     c                   endif
058200090409      *
058300090409     c                   if        d2CIT = *blank
058400090416     C*
058500090416     c                   z-add     1             tblkut
058600090416     c                   movel     'PR'          tblCod
058700090416     c                   movel(p)  d2PIA         tblkey
058800090416     c     Ktab          chain     tabel00f
058900090416     c                   if        %Found(Tabel00f)
059000090416     c                   eval      dspr = tbluni
059100090416     c                   eval      d2CIT = §PRDES
059200090416     c                   endif
059300090409     c                   endif
059400090409      *
059500090317     C                   ENDSR
059600090317     C************************************************************
059700090319      *?  CONTROLLO VIDEATA 2 CAMPI                        ?
059800090317     C************************************************************
059900090317     C     CTRD2         BEGSR
060000090317      *
060100090608      * Se è stata fatta la ristampa del contratto 19 = ON
060200090608     c                   setoff                                       19
060300090608     c                   eval      dtrsflr = trsflr
060400090608     c                   if         §trsris = 'R'
060500090608     c                   setoN                                        19
060600090608     c                   end
060700090319      *
060800090319      * Data emissione Contratto
060900090323     c                   clear                   h2DEC
061000090319$017 C     d2dec         IFgt      *zeros
061100090319     c     *eur          test(d)                 d2dec                  50
061200090319     c                   if        *in50
061300160208     c     *eur          test(d)                 d2dec                  50
061400160208     c  n50*eur          move      d2dec         dataeur
061500090319     c  n50              move      dataeur       d2dec
061600090319     C                   end
061700090319     C                   end
061800090323      * gira la data in AMG
061900090323$017 C     d2dec         IFgt      *zeros
062000090323     c                   move      d2dec         dataeur
062100090323     c                   move      dataeur       dataiso
062200090323     c                   move      dataiso       h2DeC
062300090323     C                   end
062400090320      *
062500090319      * Data rientro copia firmata
062600090324      *  deve essere superiore alla data Emissione Contratto e alla Data di Stampa
062700090323     c                   clear                   h2DrC
062800090319$017 C     d2drc         IFgt      *zeros
062900090319     c     *eur          test(d)                 d2drc                  51
063000090319     c                   if        *in51
063100160208     c     *eur          test(d)                 d2drc                  51
063200160208     c  n51*eur          move      d2drc         dataeur
063300090319     c  n51              move      dataeur       d2drc
063400090319     C                   end
063500090319     C                   end
063600090323      * gira la data in AMG
063700090323$017 C     d2drc         IFgt      *zeros
063800090323     c                   move      d2drc         dataeur
063900090323     c                   move      dataeur       dataiso
064000090323     c                   move      dataiso       h2DrC
064100090323     C                   end
064200090506      *
064300090319      *   Data Iscriz.Albo
064400090323     c                   clear                   h2Dia
064500090319$017 C     d2dia         IFgt      *zeros
064600090319     c     *eur          test(d)                 d2dia                  56
064700090319     c                   if        *in56
064800160208     c     *eur          test(d)                 d2dia                  56
064900160208     c  n56*eur          move      d2dia         dataeur
065000090319     c  n56              move      dataeur       d2dia
065100090319     C                   end
065200090319     C                   end
065300090323      * gira la data in AMG
065400090323$017 C     d2dia         IFgt      *zeros
065500090323     c                   move      d2dia         dataeur
065600090323     c                   move      dataeur       dataiso
065700090323     c                   move      dataiso       h2Dia
065800090323     C                   end
065900090410      *
066000960305     C                   ENDSR
066100090320     C/EJECT
066200090317     c*--------------------------------------------------------------
066300090317     c* tramite Società e Unità decodifica P.IVA su PROJ
066400090317     c*--------------------------------------------------------------
066500090317     C     Decod_Rag_IVA BegSR
066600090317      **
066700090317      **  Routine x Reperire dati Fornitore:
066800090317      **    La routine in base alla sottonatura può funzionare
066900090317      **     x F=Fornitore/C=Cliente
067000090408     C                   clear                   trmz70s1ds                     Input
067100090408     C                   movel(p)  TRSiva        s1_PartitaIVA                  Input
067200090408     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
067300090408     C                   movel(p)  TRSSOCG       s1_Societa                     Input/Output
067400090408     C                   movel(p)  TRSFIL        s1_Unita                       Input/Output
067500090827     c                   call      'TRMZ70SR1'
067600090408     C                   PARM                    trmz70s1ds                     Input
067700090408      *
067800090317     C                   ENDSR
067900090317     C/EJECT
068000090522     c*--------------------------------------------------------------
068100090522     c* tramite Società e Unità decodifica P.IVA su PROJ
068200090522     c*--------------------------------------------------------------
068300090522     C     Decod_P_IVA   BegSR
068400090522      **
068500090522      **  Routine x Reperire dati PIVA:
068600090522      **    La routine in base alla sottonatura può funzionare
068700090522      **     x F=Fornitore/C=Cliente
068800090522     C                   clear                   trmz70s4ds                     Input
068900090522     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
069000090522     C                   move      parm_S4CDF    s4_keyKSC                      Input
069100090522     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
069200090522     C                   movel(p)  parm_S4SOC    s4_Societa                     Input/Output
069300090525     c                   if        parm_S4FIL  > 0
069400090522     C                   movel(p)  parm_S4FIL    s4_Unita                       Input/Output
069500090525     c                   end
069600090827     c                   call      'TRMZ70SR4'
069700090522     c                   parm                    trmz70s4ds
069800090610      *
069900090610      *  se torna con l'errore riprova e aveva passato l'unità
070000090610      *   riprova passandogli l'unità vuota
070100090610     C                   clear                   trmz70s4ds                     Input
070200090610     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
070300090610     C                   move      parm_S4CDF    s4_keyKSC                      Input
070400090610     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
070500090610     C                   movel(p)  parm_S4SOC    s4_Societa                     Input/Output
070600090827     c                   call      'TRMZ70SR4'
070700090610     c                   parm                    trmz70s4ds
070800090522      *
070900090522     C                   ENDSR
071000090522     C/EJECT
071100940128     C************************************************************
071200090319     C*
071300090319     C************************************************************
071400090319     C     Decod_societa begSR
071500090319      *
071600090319     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
071700090319     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
071800090319     C                   RESET                   tibsSocI0
071900090319     C                   EVAL      tibsSocI0.idSocieta = wSOCG
072000090319     C
072100090319     C                   CALL      'TIBSSOCR'
072200090319     C                   PARM      'GETANAGRAF'  prmRqsOpCode
072300090319     C                   PARM      *BLANK        prmRpyOpCode
072400090319     C                   PARM      *ZERO         prmRpyIdMsg
072500090319     C                   PARM      'TIBSSOCI0'   prmRqsFormato
072600090319     C                   PARM                    tibsSocI0
072700090319     C                   PARM                    prmRqsDataSize
072800090319     C                   PARM      'TIBSSOCO0'   prmRpyFormato
072900090319     C                   PARM                    tibsSocO0
073000090319     C                   PARM                    prmRpyDataSize
073100090323     c                   if         PRMRPYIDMSG >= 0
073200090402     c                              and TIBSSOCO0.IDSOCIETA <> *blank
073300090323     c                   eval      wsocGd = tibsSocO0.RAGSOCIALE
073400090323     c                   else
073500090323     c                   eval      wsocGd = 'Errore'
073600090323     c                   end
073700090319      *
073800090319     C                   ENDSR
073900090402     C************************************************************
074000090402     C* GESTIONE F03 VIDEO D1
074100090402     C************************************************************
074200090402     C     F03Fine       BEGSR
074300090402     C*
074400090319     C                   MOVE      *ON           $FINE
074500090319    >C                   MOVE      '1'           xtaret
074600090319     C*
074700090319     C                   ENDSR
074800090319     C************************************************************
074900090319     C* GESTIONE F12 VIDEO D1
075000090319     C************************************************************
075100090319     C     F12Ritorna    BEGSR
075200090319     C*
075300090319     C                   MOVE      *ON           $FINE
075400090319     C*
075500090319     C                   ENDSR
075600090319     C************************************************************
075700090319     C*
075800090319     C************************************************************
075900090320     C     Quale_AITRS   begSR
076000090319     C*
076100090320     C*  Aggancia la decodifica della società
076200160208     C     H1recTRS      chain     AITRs00F
076300090320     C                   if        %Found(AITRs00F)
076400090320     c                   move      'S'           trovato_TRS
076500090320     c                   eval      H2recTRS = H1recTRS
076600090320      *
076700090320     c                   if        TrsANN <> *blank
076800090320     c                   seton                                        23
076900090320     C                   MOVEl     Annull        D1Ann
077000090320     C                   end
077100090320     c                   endIF
077200090320     C*
077300090320     C                   ENDSR
077400090320     C************************************************************
077500090320     C*
077600090320     C************************************************************
077700090320     C     Quale_AITRA   begSR
077800090320     C*
077900090319     C*     SFL di selezione quale autista prendere in considerazione
078000090319     C*
078100090320     c                   eval      kpjbuS = kpjbu
078200090320     c                   clear                   atabDS
078300120131     c                   eval      ataOPZ = xtaOPZ
078400090827     c                   eval      ataKEY = d2NRC
078500090320     c                   eval      KPJBU = atabDS
078600090827     c                   call      'TRMZ65SR0'
078700090320     c                   parm                    KPJBA
078800090320     c                   eval      atabDS = KPJBU
078900090320     c                   eval      kpjbu = kpjbuS
079000090319     C*
079100090319     c                   eval      trovato_AITRA = 'N'
079200090327     c                   clear                   h2recTRA
079300090327     c                   clear                   h1recTRA
079400090319     C*
079500090319     C* Aggancia AITRA
079600160208     C     ATArecord     chain     AITRa00f
079700090320     C                   IF        %Found(AITRa00f)
079800090319     c                   eval      trovato_AITRA = 'S'
079900090320     c                   eval      h2recTRA = ataRecord
080000090320     c                   eval      h1recTRA = ataRecord
080100090319     C                   END
080200090319     C*
080300090319     C/EJECT
080400090330     C                   ENDSR
080500090324     C************************************************************
080600090324     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
080700090324     C************************************************************
080800090324     C     GESAGG        BEGSR
080900090327     C*
081000090319     C* segnalo al pgm chiamante l'aver premuto l'F6
081100090319    >C                   MOVE      '0'           xtaret
081200090327     C*  E deve chiudere
081300090327     C                   MOVE      *ON           $FINE
081400090327     C*
081500090319     C*
081600090319     C                   ENDSR
081700120104     C************************************************************
081800120104     C* GESTIONE F01 Appendici al contratto
081900120104     C************************************************************
082000120104     C     F01appendici  BEGSR
082100120104      *
082200120104     c                   eval      kpjbuS = kpjbu
082300120104     c                   movel     h2nrc         kpjbu
082400120104     c                   call      'TRMZ79CR'
082500120104     c                   parm                    kpjba
082600120104     c                   eval      kpjbu  = kpjbuS
082700120104      *
082800120104     C                   ENDSR
082900090320     C************************************************************
083000090506      *? DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...   ?
083100090506     C************************************************************
083200090506     C     DEFCAM        BEGSR
083300090506     C*
083400090320     C* Variabili per gestione videate
083500090320     C                   MOVE      *BLANK        $GEST             2
083600090320     C                   MOVE      *BLANK        $FINE             1
083700090320     C                   MOVE      *BLANK        $INZD1            1
083800090320     C                   MOVE      *BLANK        $INZD2            1
083900090320     C                   MOVE      *BLANK        $LASTG            2
084000090320     C                   MOVE      *BLANK        $LASTV            2
084100090320     C*
084200090320     C* Variabili appoggio
084300090320     C                   Z-ADD     0             CURR              2 0
084400090320     C                   Z-ADD     0             CURC              2 0
084500090320     C                   MOVE      *ZEROS        WIN              99
084600090320     C                   MOVE      *BLANK        WTBERR            1
084700090320     C*
084800090320     c     Kaut          klist
084900090320     c                   kfld                    tipo              1
085000090320     c                   kfld                    autista           7 0
085100090320     C*
085200090320     c     kTRS02        klist
085300090320     c                   kfld                    TRSSOCG
085400090320     c                   kfld                    TRSIVA
085500090320     c                   kfld                    TRSDFC
085600120217     C*
085700120217     c     K_TBE         klist
085800120217     c                   kfld                    tbeCOD
085900120217     c                   kfld                    tbeKE1
086000120217     c                   kfld                    tbeKE2
086100090409     C*
086200090409     C     Krco98j       KLIST
086300090409     C                   KFLD                    RCOSOCIETA
086400090409     C                   KFLD                    subnatura         1
086500090409     C                   KFLD                    RCOKSC
086600090416     C*
086700090416     c     Ktab          klist
086800090416     c                   kfld                    tblkut
086900090416     c                   kfld                    tblcod
087000090416     c                   kfld                    tblkey
087100090416     c                   z-add     1             tblkut
087200090506     C*
087300090506     c     Ktgt          klist
087400090506     c                   kfld                    TgtPDR
087500090506     c                   kfld                    TgtSML
087600090320     C*
087700090320     C* Indici
087800090320     C                   Z-ADD     0             X                 3 0
087900090320     C                   Z-ADD     0             Y                 3 0
088000090320     C*
088100090320     C                   ENDSR
088200090317     C************************************************************
