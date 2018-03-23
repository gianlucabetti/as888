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
005201160208     d dsrg          e ds
005300030113      *-------------
005400030113     D* Parametri in ricezione
005500090311     D XTABDS          DS
005600090311     D  XTAOPZ                 1      1
005700090311     D  XTAFLG                 2      2
005800030113     D  XTARET                 3      3
005900030113     D  XTAOPR                 4      4
006000030113     D  XTAERR                 5      5
006100090319     D  XTArecord              6     14  0
006200090319     D  XTAKEY                15     21  0
006300940201     D*-------------
006400090320     D* Parametri in ricezione x selezionare l'Autista di AITRA da modificare
006500090320     D ataBDS          DS
006600090320     D  ataOPZ                 1      1
006700090320     D  ataFLG                 2      2
006800090320     D  ataRET                 3      3
006900090320     D  ataOPR                 4      4
007000090320     D  ataERR                 5      5
007100090320     D  atarecord              6     14  0
007200090320     D  ataKEY                15     21  0
007300090320     D*-------------
007400030204     D  S_TASTO        S                   like($Tasto)
007500940201     D DSFMT           DS
007600940506     D  $TASTO               369    369
007700940201     D  NRG                  370    370
007800940201     D  NCL                  371    371
007900940201     D*-------------
008000940201     D* posizione cursore
008100940201     D CURSOR          DS
008200940223     D  RIRI                   1      2B 0 INZ
008300940201     D  R$$                    2      2
008400940223     D  COCO                   3      4B 0 INZ
008500940201     D  C$$                    4      4
008600940207     D*-------------
008700940207     D* Reperimento nome PGM
008800940207     D STATUS         SDS           333
008900940207     D  DSPGM            *PROC
009000940225     D*-------------
009100940225     D* COSTANTI
009200940225     D*-------------
009300940506     D* Tasti di funzione
009400940506     D F01             C                   CONST(X'31')
009500940506     D F02             C                   CONST(X'32')
009600940506     D F03             C                   CONST(X'33')
009700940506     D F04             C                   CONST(X'34')
009800940506     D F05             C                   CONST(X'35')
009900940506     D F06             C                   CONST(X'36')
010000940506     D F07             C                   CONST(X'37')
010100940506     D F08             C                   CONST(X'38')
010200940506     D F09             C                   CONST(X'39')
010300940506     D F10             C                   CONST(X'3A')
010400940506     D F11             C                   CONST(X'3B')
010500940506     D F12             C                   CONST(X'3C')
010600940506     D F13             C                   CONST(X'B1')
010700940506     D F14             C                   CONST(X'B2')
010800940506     D F15             C                   CONST(X'B3')
010900940506     D F16             C                   CONST(X'B4')
011000940506     D F17             C                   CONST(X'B5')
011100940506     D F18             C                   CONST(X'B6')
011200940506     D F19             C                   CONST(X'B7')
011300940506     D F20             C                   CONST(X'B8')
011400940506     D F21             C                   CONST(X'B9')
011500940506     D F22             C                   CONST(X'BA')
011600940506     D F23             C                   CONST(X'BB')
011700940506     D F24             C                   CONST(X'BC')
011800940506     D ENTER           C                   CONST(X'F1')
011900940506     D ROLDWN          C                   CONST(X'F4')
012000940506     D ROLLUP          C                   CONST(X'F5')
012100940506     D*-------------
012200941108     D DATA            C                   CONST('0001-01-01')
012300030114     D Annull          C                   CONST('  ANNULLATO  ')
012400090318     D OPz_Vis         C                   CONST('     VISUALIZZA     ')
012500090827     D OPz_DETaut      C                   CONST(' DETTAGLIO  AUTISTI ')
012600090319      *
012700090319      * Opzioni --> Azioni da fare :
012800090319     D Visualizza      C                   CONST('5')
012900090827     D DetAutisti      C                   CONST('D')
013000090319     D Acceso          C                   '1'
013100090319     D Spento          C                   '0'
013200090323     D*-------------
013300090323      * Controllo IVA
013400151117     d xcfiva1ds     e ds
013500090323     d  ivaeu                  3      4
013600090323     d  ivait                  5     18
013700090317
013800090408      *----
013900090319     d Trovato_TRS     s              1    inz('N')
014000090319     d Trovato_AITRA   s              1    inz('N')
014100090317      *
014200090317     D Wsocg           S                   like(d1socG)
014300090317     D Wsocgd          S                   like(d1dsocG)
014400090317     d dataiso         s               d   datfmt(*iso)
014500090317     d dataeur         s               d   datfmt(*eur)
014600090323     d Data_oggi       s              8  0
014700090317     ***************************************************************************
014800090317     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
014900090317     D ESITO_OK...
015000090317     D                 C                   0
015100090317     ***************************************************************************
015200090317     **
015300090317     ** Dichiarazione prototipi procedure esterne.
015400090317     **
015500090317     ***************************************************************************
015600090317      /DEFINE DFTACTGRP_YES
015700090317     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
015800090317      /UNDEFINE DFTACTGRP_YES
015900090317     ***************************************************************************
016000090317     **
016100090317     ** Definizione strutture dati.
016200090317     **
016300090317      **************************************************************************
016400090317     D tibsSocI0...
016500090317     D               E DS                  QUALIFIED
016600090317     D                                     INZ
016700090317     D tibsSocO0...
016800090317     D               E DS                  QUALIFIED
016900090317     D                                     INZ
017000090317     ***************************************************************************
017100090317     **
017200090317     ** Definizione variabili modulo/programma.
017300090317     **
017400090317     ***************************************************************************
017500090317     D prmRqsOpCode...
017600090317     D                 S             10A
017700090317     D prmRpyOpCode...
017800090317     D                 S             10A
017900090317     D prmRpyIdMsg...
018000090317     D                 S             10I 0
018100090317     D prmRqsFormato...
018200090317     D                 S             10A
018300090317     D prmRqsDataSize...
018400090317     D                 S             10I 0
018500090317     D prmRpyFormato...
018600090317     D                 S             10A
018700090317     D prmRpyDataSize...
018800090317     D                 S             10I 0
018900090317
019000090409     d digits          c                   '0123456789'
019100090827     C*----------------------------------------------------*
019200090827     C*                MAIN LINE PROGRAM
019300090827     C*----------------------------------------------------*
019400090827     ** Inizializzo il programma.
019500090827     C                   CALL      'TIBSSOCR'
019600090827     C                   PARM      'INIT'        prmRqsOpCode
019700090827     C                   PARM                    prmRpyOpCode
019800090827     C                   PARM                    prmRpyIdMsg
019900090827     C*
020000090827      * ? * Entra nella sequenza delle videate *
020100090827     C     $FINE         DOWEQ     *OFF
020200090827     C     $GEST         CASEQ     'D1'          GESD1
020300090827     C     $GEST         CASEQ     'D2'          GESD2
020400090827     C                   END
020500090827     C                   END
020600090827     C*
020700090827     ** Chiudo il programma.
020800090827     C                   CALL      'TIBSSOCR'
020900090827     C                   PARM      'FINALIZE'    prmRqsOpCode
021000090827     C                   PARM                    prmRpyOpCode
021100090827     C                   PARM                    prmRpyIdMsg
021200090827     C*
021300090827     C* fine programma
021400090827     c                   movel     Xtabds        kpjbu
021500090827     C                   Seton                                        LR
021600090827     C************************************************************
021700090827      *?  OPERAZIONI INIZIALI                             ?
021800090827     C************************************************************
021900090827     C     *INZSR        BEGSR
022000090827     C*
022100090827     C* Reperimento parametri
022200090827     C     *ENTRY        PLIST
022300090827     C                   PARM                    KPJBA
022400090827     C                   movel     kpjbu         Xtabds
022500090827     c                   setoff                                       23
022600090827     C*
022700090827     C* Variabili per gestione videate
022800090827     C                   MOVE      *OFF          $FINE
022900090827     C                   MOVE      *ON           $INZD1
023000090827     C                   MOVE      *BLANK        $LASTG
023100090827     C                   MOVE      *BLANK        $LASTV
023200090827     C                   Z-ADD     0             $ULKD1            3 0
023300090827     C*
023400090827     C* Variabili appoggio
023500090827     C                   Z-ADD     0             CURR
023600090827     C                   Z-ADD     0             CURC
023700090827     C*
023800090827     C* Valorizzazione campi univoci testate
023900090827     C*
024000090827     C* ESSENDO SEQUENZIALI I 3 DISPLAY LI PULISCO TUTTI UNA VOLTA SOLA
024100090827     C* Pulisce entrambe i 3 video
024200090827     C                   CLEAR                   D1
024300090827     C                   CLEAR                   D2
024400090827     C*
024500090827     C                   MOVEL     KNSIF         NOMSIF
024600090827     C                   MOVEL     DSPGM         NOMPGM           10
024700090827     C*
024800090827     C*  giro la udate da GMA in AMG
024900090827     C                   move      *date         dataeur
025000090827     C                   move      dataeur       dataiso
025100090827     C                   move      dataiso       data_oggi
025200090827     C*
025300090827     C* Inizializzazione/Decodifica prima videata e seconda
025400090827     C                   MOVE      'D1'          $GEST
025500090827     C                   MOVE      *ON           $INZD1
025600090827     C                   MOVE      *OFF          $INZD2
025700090827     C*
025800090827     C                   ENDSR
025900940131     C************************************************************
026000090319      *?  GESTIONE VIDEO RECORD D1                               ?
026100940131     C************************************************************
026200940127     C     GESD1         BEGSR
026300030113      *
026400940225     C* inizializzazione videata
026500090319     C* settaggi 1°video campi attivi o campi protetti
026600090319     C     $INZD1        IFEQ      *On
026700940127     C                   EXSR      INZD1
026800940223     C                   MOVE      *OFF          $INZD1
026900940117     C                   END
027000090827      *
027100090827     c                   goto      no_video1
027200090827      *
027300090827     C*   solo non in Visualizzazione
027400090827    >C                   if        xtaopz <> Visualizza
027500030113     C*               *----------------*
027600940620     C                   EXFMT     D1
027700030113     C*               *----------------*
0278000301141    C                   SELECT
027900030114     C* F3=Fine
028000030114     C     $TASTO        WHENEQ    F03
028100090319     C                   EXSR      F03Fine
028200090319     c                   leaveSR
028300030114     C* F12=Ritorno
028400030114     C     $TASTO        WHENEQ    F12
028500090319     C                   EXSR      F12Ritorna
028600090319     c                   leaveSR
028700030114     C*
0288000903202-   C                   ENDSL
028900030114     C*
029000030114     C                   EXSR      CTRD1
029100090319     c                   end
029200090827     C*
029300090827     c     no_video1     tag
029400090827     C*
029500090319     C* Video 2 o Video 3
029600090319      *  Dati Societari
029700090319     C                   EVAL      $GEST ='D2'
029800090319     C                   EVAL      $INZD2= *ON
029900090319      *
030000940117     C                   ENDSR
030100940117      ************************************************************
030200090319      *?  INIZIALIZZAZIONE VIDEATA 1                       ?
030300940117      ************************************************************
030400940127     C     INZD1         BEGSR
030500090319     C*
030600090312     C                   MOVEL     DSPGM         d1PGM
030700090317     C                   z-add     xtakey        d1NRC
030800090317     C                   z-add     xtakey        H1NRC
030900090320     C                   z-add     xtarecord     H1recTRS
031000090319     c                   move      'N'           trovato_TRS
031100090319     C*
031200090319      * ? * Reperisce informazioni della società solo se non in IMMISSIONE *
031300090319      *                   il record di AITRS00F ?
031400090320     c                   exsr      Quale_AITRS
031500090319     C                   IF        Trovato_TRS = 'S'
031600090320     C*
031700090320     c                   eval      d1fil    = trsFIL
031800090320     c                   eval      d1SOCG   = trsSOCG
031900090320     c                   eval      d1IVA    = trsIVA
032000090319     C*
032100090319      * ? * richiamo routine dei ctrl per decodifiche e controlli
032200940614    >C                   EXSR      CTRD1
0323000903172-   C                   ENDIF
032400940224     C*
032500940117     C                   ENDSR
032600940207     C/EJECT
032700090320     C************************************************************
032800090827      *?  Decodifiche 1 Video                              ?
032900090320     C************************************************************
033000090320     C     CTRD1         BEGSR
033100090320     C*
033200940127     C                   SETOFF                                       99
033300090317     C*
033400090317     C* controllo codice vuoto  Filiale
033500090317     c                   clear                   d1dfil
033600090827$017 C     D1fil         IFne      *zeros
033700090410     c     D1Fil         chain     azorg01l
033800090410     c                   if        %Found(azorg01l) and orgfag = 'F'
033900090317     c                   eval      d1dfil = orgdes
034000090317     c                   end
034100090317     C                   ENDIF
034200090317     C*
034300090317     C* controllo codice vuoto  Società
034400090317     c                   clear                   d1dsocg
034500090827$017 C     D1socG        IFne      *blank
034600090317     c                   eval      wsocG = d1socG
034700090317     c                   exsr      decod_societa
034800090317     c                   if        prmRpyIdMsg < 0
034900090402     c                              or TIBSSOCO0.IDSOCIETA = *blank
035000090317     c                   else
035100090317     c                   eval      d1dsocG = wSocGd
035200090317     c                   end
035300090317     C                   ENDIF
035400030114     C*
035500090317     C* controllo codice vuoto P.IVA
035600090827$017 C     D1IVA         IFne      *blank
035700090323      **
035800090323      ** Controllo Formale P.IVA
035900090323      *  richiama il nuovo driver fatto per controllare il codice
036000090323      * fiscale o la partita iva
036100151117     c                   clear                   xcfiva1ds
036200090323     c                   eval      xcfivapar = D1iva
036300151117     c                   call      'XCFIVAR1'
036400151117     c                   parm                    xcfiva1ds
036500090323      * --> errore
036600090827     c                   if        XCFIVAFLG >= *zeros
036700090323      **
036800090317     c                   eval      trsIVA  = d1IVA
036900090317     c                   eval      trsSocG = d1SocG
037000090317     c                   eval      trsFil  = d1Fil
037100090317     c                   exsr      Decod_RAG_IVA
037200090319      *
037300090408     c                   if        S1_Trovato = *Off or S1_Errore  <> '0'
037400090317     C                   Else
037500090408     C                   eval      d1ProJ = s1_RagSocKSC
037600090408     C                   eval      d1ProJ1= s1_RagSocKSC1
037700090320     C                   End
037800090827      *
037900090323     C                   End
038000090317      *
0381009402241-   C                   END
038200090323     C*
038300940117     C                   ENDSR
038400090319     C************************************************************
038500090319      *?  GESTIONE VIDEO RECORD D2                               ?
038600090319     C************************************************************
038700090319     C     GESD2         BEGSR
038800090319      *
038900090319     C* inizializzazione videata
039000090319     C* settaggi 2°video campi attivi o campi protetti
039100090319     C     $INZD2        IFEQ      *ON
039200090319     C                   EXSR      INZD2
039300090319     C                   MOVE      *OFF          $INZD2
039400090319     C                   END
039500090327      *
039600090319      *  emette il secondo video dei dati società
039700090827    >C     xtaopz        ifNE      Visualizza
039800090827      *
039900090827     c                   exsr      quale_aitra
040000090827    >C                   eval      xtaopz = Visualizza
040100090827     C                   eval      $TASTO = F12
040200090827      *
040300090827     c                   else
040400090319     C*               *----------------*
040500090319     C                   EXFMT     D2
040600090319     C*               *----------------*
040700090827     c                   end
040800090319     C*
0409000903191    C                   SELECT
041000120104     C* F1=Appendici
041100120104     C     $TASTO        WHENEQ    F01
041200120104     C                   EXSR      F01appendici
041300120104     c                   leaveSR
041400090402     C* F3=Fine
041500090402     C     $TASTO        WHENEQ    F03
041600090402     C                   EXSR      F03Fine
041700090402     c                   leaveSR
041800090319     C* F12=Ritorno
041900090319     C     $TASTO        WHENEQ    F12
042000090319     C                   EXSR      F12Ritorna
042100090319     c                   leaveSR
042200090319     C*
0423000903202-   C                   ENDSL
042400090319     C*
042500090319    >C                   EXSR      CTRD2
042600090319      *
042700090319     C                   ENDSR
042800090319      ************************************************************
042900090319      *?  INIZIALIZZAZIONE VIDEATA 2                       ?
043000090319      ************************************************************
043100090319     C     INZD2         BEGSR
043200090319     C*
043300120104     c                   setoff                                       19
043400120104     c                   eval      dtrsflr = trsflr
043500120104     c                   if         §trsris = 'R'
043600120104     c                   setoN                                        19
043700120104     c                   end
043800090320     C                   z-add     H1recTRS      H2recTRS
043900090317     c                   eval      d2PGM  =  D1PGM
044000090410     C*
044100090317     c                   eval      d2NRC  =  D1Nrc
044200090319     c                   eval      H2NRC  =  H1Nrc
044300090317     c                   eval      D2Ann  =  D1Ann
044400090317     c                   eval      D2Fun  =  D1Fun
044500090317     c                   eval      D2FIL  =  D1FIL
044600090317     c                   eval      D2DFIL =  D1DFIL
044700090317     c                   eval      D2SOCG =  D1SOCG
044800090317     c                   eval      D2DSCG =  D1DSoCG
044900090317     c                   eval      D2IVA  =  D1IVA
045000090408     C                   eval      d2ProJ = s1_RagSocKSC
045100090408     C                   eval      d2ProJ1= s1_RagSocKSC1
045200090409      *------
045300090319     c                   if        trsdec > 0
045400090319     c                   move      TRsDec        dataiso
045500090319     c                   move      dataiso       dataeur
045600090319     c                   move      dataeur       d2DEC
045700090324     c                   move      TRsDec        h2DEC
045800090407     c                   move      TRsDec        h2savdec
045900090319     c                   endif
046000090319      *
046100090319     c                   if        trsdfc > 0
046200090319     c                   move      TRsDfc        dataiso
046300090319     c                   move      dataiso       dataeur
046400090319     c                   move      dataeur       d2DfC
046500090324     c                   move      TRsDfc        h2DfC
046600090319     c                   endif
046700090319
046800090319     c                   if        trsdrc > 0
046900090319     c                   move      TRsDrc        dataiso
047000090319     c                   move      dataiso       dataeur
047100090319     c                   move      dataeur       d2DrC
047200090324     c                   move      TRsDrc        h2DrC
047300090319     c                   endif
047400090319
047500090522     c                   if        trsdsc > 0
047600090319     c                   move      TRsDsc        dataiso
047700090319     c                   move      dataiso       dataeur
047800090319     c                   move      dataeur       d2DsC
047900120104     c                   move      TRsDsc        h2DsC
048000120104     c  n19              if        §trsunodc = *zeros or
048100120104     c                             §trsunodc =  *blank
048200120104     c                   move      dataeur       d2unodc
048300120104     c                   endif
048400090319     c                   endif
048500120104
048600120104     c                   if        §trsunodc <> *zeros and
048700120104     c                             §trsunodc <> *blank
048800120104     c                   move      §trsunodc     §trsunodc0        8 0
048900120104     c                   move      §trsunodc0    dataiso
049000120104     c                   move      dataiso       dataeur
049100120104     c                   move      dataeur       d2Unodc
049200120104     c                   move      §trsunodc     h2Unodc
049300120104     c                   endif
049400120104     c                   if        d2unodc = d2dsc
049500120104     c                   clear                   d2dsc
049600120104     c                   endif
049700120217
049800120217      *
049900120217      *    riporta sulla DS i campi del filler
050000120217     c                   eval      dTRSflr = TRSflr
050100150305     c                   eval      dTRSflr1= TRSflr1
050200150305      *
050300150305      ** DATA    D.U.R.C.
050400150305     c                   if        §trsdDURC <> *zeros and
050500150305     c                             §trsdDURC <> *blank
050600150305     c                   move      §trsdDURC     §trsdDURC0        8 0
050700150305     c                   move      §trsdDURC0    dataiso
050800150305     c                   move      dataiso       dataeur
050900150305     c                   move      dataeur       d2dDURC
051000150305     c                   move      §trsdDURC     h2dDURC
051100150305     c                   endif
051200150305      **
051300150305      ** DATA    C.I.P.
051400150305     c                   if        §trsdCIP  <> *zeros and
051500150305     c                             §trsdCIP  <> *blank
051600150305     c                   move      §trsdCIP      §trsdCIP0         8 0
051700150305     c                   move      §trsdCIP0     dataiso
051800150305     c                   move      dataiso       dataeur
051900150305     c                   move      dataeur       d2dCIP
052000150305     c                   move      §trsdCIP      h2dCIP
052100150305     c                   endif
052101160208     c                   eval      d2COR = TRSCOR
052102160208     c                   clear                   d2cord
053600160208     c                   if        d2cor <> *blank
053700160208     c                   movel     'SRG'         TBECOD
053800160208     c                   movel(p)  d2cor         TBEke1
053900160208     c                   clear                   TBEke2
054000160208     c     k_TBE         chain     tnTBE01l
054100160208     c                   if        %Found(tnTBE01L)
054200160208     c                   movel     tbeuni        dsrg
054201160208     c                   movel     §srgdes       d2cord
054300160208     c                   end
054400160208     c                   end
054500090319
055100090319     c                   eval      d2CIT = TRSCIT
055200090319     c                   eval      d2NIS = TRSNIS
055300090319     c                   eval      d2PIA = TRSPIA
055400090319     c                   eval      d2NIA = TRSNIA
055500090319
055600090319     c                   if        trsdia > 0
055700090319     c                   move      TRsDia        dataiso
055800090319     c                   move      dataiso       dataeur
055900090319     c                   move      dataeur       d2Dia
056000090319     c                   endif
056100090319     C*
056200090319      * ? * richiamo routine dei ctrl per decodifiche e controlli
056300090319    >C                   EXSR      CTRD2
056400090409     C*
056500090409     C*  comunque decodifica dal forntitore i campi:
056600090409     C*    Registro Imprese, Codice Fiscale, Provincia e Città e se
056700090409     C*    il Codice Fiscale è alfanumerico imposta la Ragione Sociale del
056800090409     C*    Fornitore nel Legale Rappresentante.
056900090409     c                   clear                   CaratteriAlfa     1
057000090409      *------
057100090409     c                   if        d2NIS = *blank
057200090409     c                   eval      d2NIS = S1_CodiceFisc
057300090409      *  controllo validità distinta
057400090417     c                   clear                   dove              3 0
057500090417     c     digits        check     d2NIS:1       dove
057600090417     c                   if        dove > 0
057700090409     c                   move      'S'           CaratteriAlfa     1
057800090409     c                   end
057900090409     c                   endif
058000090409      *
058100090409     c                   if        d2PIA = *blank
058200090409     c                   eval      d2PIA = S1_Provincia
058300090409     c                   endif
058400090409      *
058500090409     c                   if        d2CIT = *blank
058600090416     C*
058700090416     c                   z-add     1             tblkut
058800090416     c                   movel     'PR'          tblCod
058900090416     c                   movel(p)  d2PIA         tblkey
059000090416     c     Ktab          chain     tabel00f
059100090416     c                   if        %Found(Tabel00f)
059200090416     c                   eval      dspr = tbluni
059300090416     c                   eval      d2CIT = §PRDES
059400090416     c                   endif
059500090409     c                   endif
059600090409      *
059700090317     C                   ENDSR
059800090317     C************************************************************
059900090319      *?  CONTROLLO VIDEATA 2 CAMPI                        ?
060000090317     C************************************************************
060100090317     C     CTRD2         BEGSR
060200090317      *
060300090608      * Se è stata fatta la ristampa del contratto 19 = ON
060400090608     c                   setoff                                       19
060500090608     c                   eval      dtrsflr = trsflr
060600090608     c                   if         §trsris = 'R'
060700090608     c                   setoN                                        19
060800090608     c                   end
060900090319      *
061000090319      * Data emissione Contratto
061100090323     c                   clear                   h2DEC
061200090319$017 C     d2dec         IFgt      *zeros
061300090319     c     *eur          test(d)                 d2dec                  50
061400090319     c                   if        *in50
061500160208     c     *eur          test(d)                 d2dec                  50
061600160208     c  n50*eur          move      d2dec         dataeur
061700090319     c  n50              move      dataeur       d2dec
061800090319     C                   end
061900090319     C                   end
062000090323      * gira la data in AMG
062100090323$017 C     d2dec         IFgt      *zeros
062200090323     c                   move      d2dec         dataeur
062300090323     c                   move      dataeur       dataiso
062400090323     c                   move      dataiso       h2DeC
062500090323     C                   end
062600090320      *
062700090319      * Data rientro copia firmata
062800090324      *  deve essere superiore alla data Emissione Contratto e alla Data di Stampa
062900090323     c                   clear                   h2DrC
063000090319$017 C     d2drc         IFgt      *zeros
063100090319     c     *eur          test(d)                 d2drc                  51
063200090319     c                   if        *in51
063300160208     c     *eur          test(d)                 d2drc                  51
063400160208     c  n51*eur          move      d2drc         dataeur
063500090319     c  n51              move      dataeur       d2drc
063600090319     C                   end
063700090319     C                   end
063800090323      * gira la data in AMG
063900090323$017 C     d2drc         IFgt      *zeros
064000090323     c                   move      d2drc         dataeur
064100090323     c                   move      dataeur       dataiso
064200090323     c                   move      dataiso       h2DrC
064300090323     C                   end
064400090506      *
064500090319      *   Data Iscriz.Albo
064600090323     c                   clear                   h2Dia
064700090319$017 C     d2dia         IFgt      *zeros
064800090319     c     *eur          test(d)                 d2dia                  56
064900090319     c                   if        *in56
065000160208     c     *eur          test(d)                 d2dia                  56
065100160208     c  n56*eur          move      d2dia         dataeur
065200090319     c  n56              move      dataeur       d2dia
065300090319     C                   end
065400090319     C                   end
065500090323      * gira la data in AMG
065600090323$017 C     d2dia         IFgt      *zeros
065700090323     c                   move      d2dia         dataeur
065800090323     c                   move      dataeur       dataiso
065900090323     c                   move      dataiso       h2Dia
066000090323     C                   end
066100090410      *
068000960305     C                   ENDSR
068100090320     C/EJECT
068200090317     c*--------------------------------------------------------------
068300090317     c* tramite Società e Unità decodifica P.IVA su PROJ
068400090317     c*--------------------------------------------------------------
068500090317     C     Decod_Rag_IVA BegSR
068600090317      **
068700090317      **  Routine x Reperire dati Fornitore:
068800090317      **    La routine in base alla sottonatura può funzionare
068900090317      **     x F=Fornitore/C=Cliente
069000090408     C                   clear                   trmz70s1ds                     Input
069100090408     C                   movel(p)  TRSiva        s1_PartitaIVA                  Input
069200090408     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
069300090408     C                   movel(p)  TRSSOCG       s1_Societa                     Input/Output
069400090408     C                   movel(p)  TRSFIL        s1_Unita                       Input/Output
069500090827     c                   call      'TRMZ70SR1'
069600090408     C                   PARM                    trmz70s1ds                     Input
069700090408      *
069800090317     C                   ENDSR
069900090317     C/EJECT
070000090522     c*--------------------------------------------------------------
070100090522     c* tramite Società e Unità decodifica P.IVA su PROJ
070200090522     c*--------------------------------------------------------------
070300090522     C     Decod_P_IVA   BegSR
070400090522      **
070500090522      **  Routine x Reperire dati PIVA:
070600090522      **    La routine in base alla sottonatura può funzionare
070700090522      **     x F=Fornitore/C=Cliente
070800090522     C                   clear                   trmz70s4ds                     Input
070900090522     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
071000090522     C                   move      parm_S4CDF    s4_keyKSC                      Input
071100090522     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
071200090522     C                   movel(p)  parm_S4SOC    s4_Societa                     Input/Output
071300090525     c                   if        parm_S4FIL  > 0
071400090522     C                   movel(p)  parm_S4FIL    s4_Unita                       Input/Output
071500090525     c                   end
071600090827     c                   call      'TRMZ70SR4'
071700090522     c                   parm                    trmz70s4ds
071800090610      *
071900090610      *  se torna con l'errore riprova e aveva passato l'unità
072000090610      *   riprova passandogli l'unità vuota
072100090610     C                   clear                   trmz70s4ds                     Input
072200090610     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
072300090610     C                   move      parm_S4CDF    s4_keyKSC                      Input
072400090610     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
072500090610     C                   movel(p)  parm_S4SOC    s4_Societa                     Input/Output
072600090827     c                   call      'TRMZ70SR4'
072700090610     c                   parm                    trmz70s4ds
072800090522      *
072900090522     C                   ENDSR
073000090522     C/EJECT
073100940128     C************************************************************
073200090319     C*
073300090319     C************************************************************
073400090319     C     Decod_societa begSR
073500090319      *
073600090319     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
073700090319     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
073800090319     C                   RESET                   tibsSocI0
073900090319     C                   EVAL      tibsSocI0.idSocieta = wSOCG
074000090319     C
074100090319     C                   CALL      'TIBSSOCR'
074200090319     C                   PARM      'GETANAGRAF'  prmRqsOpCode
074300090319     C                   PARM      *BLANK        prmRpyOpCode
074400090319     C                   PARM      *ZERO         prmRpyIdMsg
074500090319     C                   PARM      'TIBSSOCI0'   prmRqsFormato
074600090319     C                   PARM                    tibsSocI0
074700090319     C                   PARM                    prmRqsDataSize
074800090319     C                   PARM      'TIBSSOCO0'   prmRpyFormato
074900090319     C                   PARM                    tibsSocO0
075000090319     C                   PARM                    prmRpyDataSize
075100090323     c                   if         PRMRPYIDMSG >= 0
075200090402     c                              and TIBSSOCO0.IDSOCIETA <> *blank
075300090323     c                   eval      wsocGd = tibsSocO0.RAGSOCIALE
075400090323     c                   else
075500090323     c                   eval      wsocGd = 'Errore'
075600090323     c                   end
075700090319      *
075800090319     C                   ENDSR
075900090402     C************************************************************
076000090402     C* GESTIONE F03 VIDEO D1
076100090402     C************************************************************
076200090402     C     F03Fine       BEGSR
076300090402     C*
076400090319     C                   MOVE      *ON           $FINE
076500090319    >C                   MOVE      '1'           xtaret
076600090319     C*
076700090319     C                   ENDSR
076800090319     C************************************************************
076900090319     C* GESTIONE F12 VIDEO D1
077000090319     C************************************************************
077100090319     C     F12Ritorna    BEGSR
077200090319     C*
077300090319     C                   MOVE      *ON           $FINE
077400090319     C*
077500090319     C                   ENDSR
077600090319     C************************************************************
077700090319     C*
077800090319     C************************************************************
077900090320     C     Quale_AITRS   begSR
078000090319     C*
078100090320     C*  Aggancia la decodifica della società
078200160208     C     H1recTRS      chain     AITRs00F
078300090320     C                   if        %Found(AITRs00F)
078400090320     c                   move      'S'           trovato_TRS
078500090320     c                   eval      H2recTRS = H1recTRS
078600090320      *
078700090320     c                   if        TrsANN <> *blank
078800090320     c                   seton                                        23
078900090320     C                   MOVEl     Annull        D1Ann
079000090320     C                   end
079100090320     c                   endIF
079200090320     C*
079300090320     C                   ENDSR
079400090320     C************************************************************
079500090320     C*
079600090320     C************************************************************
079700090320     C     Quale_AITRA   begSR
079800090320     C*
079900090319     C*     SFL di selezione quale autista prendere in considerazione
080000090319     C*
080100090320     c                   eval      kpjbuS = kpjbu
080200090320     c                   clear                   atabDS
080300120131     c                   eval      ataOPZ = xtaOPZ
080400090827     c                   eval      ataKEY = d2NRC
080500090320     c                   eval      KPJBU = atabDS
080600090827     c                   call      'TRMZ65SR0'
080700090320     c                   parm                    KPJBA
080800090320     c                   eval      atabDS = KPJBU
080900090320     c                   eval      kpjbu = kpjbuS
081000090319     C*
081100090319     c                   eval      trovato_AITRA = 'N'
081200090327     c                   clear                   h2recTRA
081300090327     c                   clear                   h1recTRA
081400090319     C*
081500090319     C* Aggancia AITRA
081600160208     C     ATArecord     chain     AITRa00f
081700090320     C                   IF        %Found(AITRa00f)
081800090319     c                   eval      trovato_AITRA = 'S'
081900090320     c                   eval      h2recTRA = ataRecord
082000090320     c                   eval      h1recTRA = ataRecord
082100090319     C                   END
082200090319     C*
082300090319     C/EJECT
082400090330     C                   ENDSR
082500090324     C************************************************************
082600090324     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
082700090324     C************************************************************
082800090324     C     GESAGG        BEGSR
082900090327     C*
083000090319     C* segnalo al pgm chiamante l'aver premuto l'F6
083100090319    >C                   MOVE      '0'           xtaret
083200090327     C*  E deve chiudere
083300090327     C                   MOVE      *ON           $FINE
083400090327     C*
083500090319     C*
083600090319     C                   ENDSR
083700120104     C************************************************************
083800120104     C* GESTIONE F01 Appendici al contratto
083900120104     C************************************************************
084000120104     C     F01appendici  BEGSR
084100120104      *
084200120104     c                   eval      kpjbuS = kpjbu
084300120104     c                   movel     h2nrc         kpjbu
084400120104     c                   call      'TRMZ79CR'
084500120104     c                   parm                    kpjba
084600120104     c                   eval      kpjbu  = kpjbuS
084700120104      *
084800120104     C                   ENDSR
084900090320     C************************************************************
085000090506      *? DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...   ?
085100090506     C************************************************************
085200090506     C     DEFCAM        BEGSR
085300090506     C*
085400090320     C* Variabili per gestione videate
085500090320     C                   MOVE      *BLANK        $GEST             2
085600090320     C                   MOVE      *BLANK        $FINE             1
085700090320     C                   MOVE      *BLANK        $INZD1            1
085800090320     C                   MOVE      *BLANK        $INZD2            1
085900090320     C                   MOVE      *BLANK        $LASTG            2
086000090320     C                   MOVE      *BLANK        $LASTV            2
086100090320     C*
086200090320     C* Variabili appoggio
086300090320     C                   Z-ADD     0             CURR              2 0
086400090320     C                   Z-ADD     0             CURC              2 0
086500090320     C                   MOVE      *ZEROS        WIN              99
086600090320     C                   MOVE      *BLANK        WTBERR            1
086700090320     C*
086800090320     c     Kaut          klist
086900090320     c                   kfld                    tipo              1
087000090320     c                   kfld                    autista           7 0
087100090320     C*
087200090320     c     kTRS02        klist
087300090320     c                   kfld                    TRSSOCG
087400090320     c                   kfld                    TRSIVA
087500090320     c                   kfld                    TRSDFC
087600120217     C*
087700120217     c     K_TBE         klist
087800120217     c                   kfld                    tbeCOD
087900120217     c                   kfld                    tbeKE1
088000120217     c                   kfld                    tbeKE2
088100090409     C*
088200090409     C     Krco98j       KLIST
088300090409     C                   KFLD                    RCOSOCIETA
088400090409     C                   KFLD                    subnatura         1
088500090409     C                   KFLD                    RCOKSC
088600090416     C*
088700090416     c     Ktab          klist
088800090416     c                   kfld                    tblkut
088900090416     c                   kfld                    tblcod
089000090416     c                   kfld                    tblkey
089100090416     c                   z-add     1             tblkut
089200090506     C*
089300090506     c     Ktgt          klist
089400090506     c                   kfld                    TgtPDR
089500090506     c                   kfld                    TgtSML
089600090320     C*
089700090320     C* Indici
089800090320     C                   Z-ADD     0             X                 3 0
089900090320     C                   Z-ADD     0             Y                 3 0
090000090320     C*
090100090320     C                   ENDSR
090200090317     C************************************************************
