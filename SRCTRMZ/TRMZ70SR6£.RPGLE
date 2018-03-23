000100141127      *
000200141127     H*
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400940223      *
000500940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000600940620      *  03           PROTECT TUTTI I CAMPI
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
002400090407     FAITRS00f  UF A E             DISK
002500090331      *
002600090319     FAzorg01L  iF   E           K DISK
002700090319     FFiapd01L  iF   E           K DISK
002800120217     FTnTBE01L  iF   E           K DISK
002900090317      *
003000090407$001 FTRMZ70SD6 CF   E             WORKSTN INFDS(DSFMT)
003100940128     D*----------------------------------------------------*
003200940127     D* Passaggio Parametri
003300940127     D KPJBA         E DS
003400090320     D KPJBus          S                   like(KPJBU)
003500090319     D tibs02ds      E DS
003600090319     d daip          e ds
003700090407      *-------------
003800090407     D* Parametri in ricezione
003900090407     D XTABDS          DS
004000090407     D  XTAOPZ                 1      1
004100090407     D  XTAFLG                 2      2
004200090407     D  XTARET                 3      3
004300090407     D  XTAOPR                 4      4
004400090407     D  XTAERR                 5      5
004500090407     D  XTArecord              6     14  0
004600090407     D  XTAKEY                15     21  0
004700090320     D*-------------
004800030204     D  S_TASTO        S                   like($Tasto)
004900940201     D DSFMT           DS
005000940506     D  $TASTO               369    369
005100940201     D  NRG                  370    370
005200940201     D  NCL                  371    371
005300940201     D*-------------
005400940201     D* posizione cursore
005500940201     D CURSOR          DS
005600940223     D  RIRI                   1      2B 0 INZ
005700940201     D  R$$                    2      2
005800940223     D  COCO                   3      4B 0 INZ
005900940201     D  C$$                    4      4
006000940207     D*-------------
006100940207     D* Reperimento nome PGM
006200940207     D STATUS         SDS           333
006300940207     D  DSPGM            *PROC
006400940225     D*-------------
006500940225     D* COSTANTI
006600940225     D*-------------
006700940506     D* Tasti di funzione
006800940506     D F01             C                   CONST(X'31')
006900940506     D F02             C                   CONST(X'32')
007000940506     D F03             C                   CONST(X'33')
007100940506     D F04             C                   CONST(X'34')
007200940506     D F05             C                   CONST(X'35')
007300940506     D F06             C                   CONST(X'36')
007400940506     D F07             C                   CONST(X'37')
007500940506     D F08             C                   CONST(X'38')
007600940506     D F09             C                   CONST(X'39')
007700940506     D F10             C                   CONST(X'3A')
007800940506     D F11             C                   CONST(X'3B')
007900940506     D F12             C                   CONST(X'3C')
008000940506     D F13             C                   CONST(X'B1')
008100940506     D F14             C                   CONST(X'B2')
008200940506     D F15             C                   CONST(X'B3')
008300940506     D F16             C                   CONST(X'B4')
008400940506     D F17             C                   CONST(X'B5')
008500940506     D F18             C                   CONST(X'B6')
008600940506     D F19             C                   CONST(X'B7')
008700940506     D F20             C                   CONST(X'B8')
008800940506     D F21             C                   CONST(X'B9')
008900940506     D F22             C                   CONST(X'BA')
009000940506     D F23             C                   CONST(X'BB')
009100940506     D F24             C                   CONST(X'BC')
009200940506     D ENTER           C                   CONST(X'F1')
009300940506     D ROLDWN          C                   CONST(X'F4')
009400940506     D ROLLUP          C                   CONST(X'F5')
009500940506     D*-------------
009600941108     D DATA            C                   CONST('0001-01-01')
009700090407     D Annull          C                   CONST('  ANNULLATO  ')
009800090318     D OPz_Var         C                   CONST('     VARIAZIONE     ')
009900090319      *
010000090319      * Opzioni --> Azioni da fare :
010100090319     D Modifica        C                   CONST('2')
010200090319     D Acceso          C                   '1'
010300090319     D Spento          C                   '0'
010400090323     D*-------------
010500120216     d dtrsflr       E DS
010600150305     d dtrsflr1      E DS
010700120216      *-------------
010800090323      * Controllo IVA
010900151117     d xcfiva1ds     e ds
011000090323     d  ivaeu                  3      4
011100090323     d  ivait                  5     18
011200090317
011300090317     D*-------------
011400090410     D trmz70s1ds    E DS                  prefix(S1_)
011500090407      *-----
011600090331     d Da_duplicare    s              1
011700090319     d Trovato_TRS     s              1    inz('N')
011800090319     d Trovato_AITRA   s              1    inz('N')
011900090317     D anRCO98j      E DS
012000090317      *
012100090317     D Wsocg           S                   like(d1socG)
012200090317     D Wsocgd          S                   like(d1dsocG)
012300090317     d dataiso         s               d   datfmt(*iso)
012400090317     d dataeur         s               d   datfmt(*eur)
012500090323     d Data_oggi       s              8  0
012600090318     d Errore          s              1
012700090317     ***************************************************************************
012800090317     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
012900090317     D ESITO_OK...
013000090317     D                 C                   0
013100090317     ***************************************************************************
013200090317     **
013300090317     ** Dichiarazione prototipi procedure esterne.
013400090317     **
013500090317     ***************************************************************************
013600090317      /DEFINE DFTACTGRP_YES
013700090317     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
013800090317      /UNDEFINE DFTACTGRP_YES
013900090317     ***************************************************************************
014000090317     **
014100090317     ** Definizione strutture dati.
014200090317     **
014300090317      **************************************************************************
014400090317     D tibsSocI0...
014500090317     D               E DS                  QUALIFIED
014600090317     D                                     INZ
014700090317     D tibsSocO0...
014800090317     D               E DS                  QUALIFIED
014900090317     D                                     INZ
015000090317     ***************************************************************************
015100090317     **
015200090317     ** Definizione variabili modulo/programma.
015300090317     **
015400090317     ***************************************************************************
015500090317     D prmRqsOpCode...
015600090317     D                 S             10A
015700090317     D prmRpyOpCode...
015800090317     D                 S             10A
015900090317     D prmRpyIdMsg...
016000090317     D                 S             10I 0
016100090317     D prmRqsFormato...
016200090317     D                 S             10A
016300090317     D prmRqsDataSize...
016400090317     D                 S             10I 0
016500090317     D prmRpyFormato...
016600090317     D                 S             10A
016700090317     D prmRpyDataSize...
016800090317     D                 S             10I 0
016900090317
017000940127     C*----------------------------------------------------*
017100940127     C*                MAIN LINE PROGRAM
017200940127     C*----------------------------------------------------*
017300090319     ** Inizializzo il programma.
017400090319     C                   CALL      'TIBSSOCR'
017500090319     C                   PARM      'INIT'        prmRqsOpCode
017600090319     C                   PARM                    prmRpyOpCode
017700090319     C                   PARM                    prmRpyIdMsg
017800940223     C*
017900090319      * ? * Entra nella sequenza delle 3 videate + aggiornamento D.Base *
018000940223     C     $FINE         DOWEQ     *OFF
018100940202     C     $GEST         CASEQ     'D1'          GESD1
018200090319     C     $GEST         CASEQ     'D2'          GESD2
018300940117     C                   END
018400940117     C                   END
018500090317     C*
018600090317     ** Chiudo il programma.
018700090317     C                   CALL      'TIBSSOCR'
018800090317     C                   PARM      'FINALIZE'    prmRqsOpCode
018900090317     C                   PARM                    prmRpyOpCode
019000090317     C                   PARM                    prmRpyIdMsg
019100090317     C*
019200940325     C* fine programma
019300090407     c                   movel     Xtabds        kpjbu
019400030113     C                   Seton                                        LR
019500940131     C************************************************************
019600090319      *?  GESTIONE VIDEO RECORD D1                               ?
019700940131     C************************************************************
019800940127     C     GESD1         BEGSR
019900030113      *
020000940225     C* inizializzazione videata
020100090319     C* settaggi 1°video campi attivi o campi protetti
020200090319     C     $INZD1        IFEQ      *On
020300940127     C                   EXSR      INZD1
020400940223     C                   MOVE      *OFF          $INZD1
020500940117     C                   END
020600030114     C*
020700090319     c   99              eval      errore = *On
020800030114     c                   SETOFF                                         99
020900090407     C* Solo x Modifica
021000030113     C*               *----------------*
021100940620     C                   EXFMT     D1
021200030113     C*               *----------------*
021300090318     c                   eval      errore = *off
021400030114     C*
0215000301141    C                   SELECT
021600030114     C* F3=Fine
021700030114     C     $TASTO        WHENEQ    F03
021800090319     C                   EXSR      F03Fine
021900090319     c                   leaveSR
022000030114     C* F12=Ritorno
022100030114     C     $TASTO        WHENEQ    F12
022200090319     C                   EXSR      F12Ritorna
022300090319     c                   leaveSR
022400030114     C*
0225000903202-   C                   ENDSL
022600030114     C*
022700030114     C                   EXSR      CTRD1
022800090407     C* Video 2
022900940506     C     *IN99         IFEQ      *OFF
023000090319      *  Dati Societari
023100090319     C                   EVAL      $GEST ='D2'
023200090319     C                   EVAL      $INZD2= *ON
0233000301142-   C                   ENDIF
023400030114     C*
023500940117     C                   ENDSR
023600940117      ************************************************************
023700090319      *?  INIZIALIZZAZIONE VIDEATA 1                       ?
023800940117      ************************************************************
023900940127     C     INZD1         BEGSR
024000090319     C*
024100090312     C                   MOVEL     DSPGM         d1PGM
024200090317     C                   z-add     xtakey        d1NRC
024300090317     C                   z-add     xtakey        H1NRC
024400090320     C                   z-add     xtarecord     H1recTRS
024500090319     c                   move      'N'           trovato_TRS
024600090406     c                   clear                   d2dec
024700090323     C*
024800090319      * ? * Reperisce informazioni della società solo se non in IMMISSIONE *
024900090319      *                   il record di AITRS00F ?
025000090320     c                   exsr      Quale_AITRS
025100090319     C                   IF        Trovato_TRS = 'S'
025200090320     C*
025300090320     c                   eval      d1fil    = trsFIL
025400090320     c                   eval      d1SOCG   = trsSOCG
025500090320     c                   eval      d1IVA    = trsIVA
025600090319     C*
025700090319      * ? * richiamo routine dei ctrl per decodifiche e controlli
025800940614    >C                   EXSR      CTRD1
0259000903172-   C                   ENDIF
026000940224     C*
026100940117     C                   ENDSR
026200940207     C/EJECT
026300090320     C************************************************************
026400090320      *?  CONTROLLO VIDEATA 1                              ?
026500090320     C************************************************************
026600090320     C     CTRD1         BEGSR
026700090320     C*
026800940127     C                   SETOFF                                       99
026900090317     C*
027000090317     C* controllo codice vuoto  Filiale
027100090317     c                   clear                   d1dfil
027200090317$017 C     D1fil         IFEQ      *zeros
027300090317     C                   SETON                                        5199
027400090319     c                   leavesr
027500090317     C                   Else
027600090317     c     D1Fil         chain     azorg01l
027700090317     c                   if        %Found(azorg01l)
027800090317     c                   eval      d1dfil = orgdes
027900090317     c                   else
028000090317     C                   SETON                                        5199
028100090319     c                   leavesr
028200090317     c                   end
028300090317     C                   ENDIF
028400090317     C*
028500090317     C* controllo codice vuoto  Società
028600090317     c                   clear                   d1dsocg
028700090317$017 C     D1socG        IFEQ      *blank
028800090317     C                   SETON                                        5299
028900090319     c                   leavesr
029000090317     C                   Else
029100090317     c                   eval      wsocG = d1socG
029200090317     c                   exsr      decod_societa
029300090317     c                   if        prmRpyIdMsg < 0
029400090402     c                              or TIBSSOCO0.IDSOCIETA = *blank
029500090317     C                   SETON                                        5299
029600090319     c                   leavesr
029700090317     c                   else
029800090317     c                   eval      d1dsocG = wSocGd
029900090317     c                   end
030000090317     C                   ENDIF
030100030114     C*
030200090317     C* controllo codice vuoto P.IVA
030300090317$017 C     D1IVA         IFEQ      *ALL' '
030400090317     C                   SETON                                        5499
030500090319     c                   leavesr
030600090317     C                   ELSE
030700090323      **
030800090323      ** Controllo Formale P.IVA
030900090323      *  richiama il nuovo driver fatto per controllare il codice
031000090323      * fiscale o la partita iva
031100151117     c                   clear                   xcfiva1ds
031200090323     c                   eval      xcfivapar = D1iva
031300151117     c                   call      'XCFIVAR1'
031400151117     c                   parm                    xcfiva1ds
031500090323      * --> errore
031600090323     c                   if        XCFIVAFLG < *zeros
031700090323     C                   SETON                                        5499
031800090323     c                   leavesr
031900090323     c                   Else
032000090323      **
032100090317     c                   eval      trsIVA  = d1IVA
032200090317     c                   eval      trsSocG = d1SocG
032300090317     c                   eval      trsFil  = d1Fil
032400090317     c                   exsr      Decod_RAG_IVA
032500090319      *
032600090410     c                   if        S1_Trovato = *Off
032700090406     C                   SETON                                        5599
032800090406     c                   leavesr
032900090317     C                   Else
033000090410     C                   eval      d1ProJ = S1_RagSocKSC
033100090320     C                   End
033200090407      *
033300090323     C                   End
033400090317      *
0335009402241-   C                   END
033600090323     C*
033700940117     C                   ENDSR
033800090319     C************************************************************
033900090319      *?  GESTIONE VIDEO RECORD D2                               ?
034000090319     C************************************************************
034100090319     C     GESD2         BEGSR
034200090319      *
034300090319     C* inizializzazione videata
034400090319     C* settaggi 2°video campi attivi o campi protetti
034500090319     C     $INZD2        IFEQ      *ON
034600090319     C                   EXSR      INZD2
034700090319     C                   MOVE      *OFF          $INZD2
034800090319     C                   END
034900090319     C*
035000090319     c   99              eval      errore = *on
035100090319     c                   SETOFF                                         99
035200090327      *
035300090319      *  emette il secondo video dei dati società
035400090319     C*               *----------------*
035500090319     C                   EXFMT     D2
035600090319     C*               *----------------*
035700090319     c                   eval      errore = *off
035800090319     C*
0359000903191    C                   SELECT
036000090402     C* F3=Fine
036100090402     C     $TASTO        WHENEQ    F03
036200090402     C                   EXSR      F03Fine
036300090402     c                   leaveSR
036400090319     C* F12=Ritorno
036500090319     C     $TASTO        WHENEQ    F12
036600090319     C                   EXSR      F12Ritorna
036700090319     c                   leaveSR
036800090319     C*
0369000903202-   C                   ENDSL
037000090319     C*
037100090319    >C                   EXSR      CTRD2
037200090319      *
037300090319     C     *IN99         IFEQ      *OFF
037400090327     C     $TASTO        ifeq      F06
037500090327     C*  deve aggiornare il Data Base
037600090327     c                   exsr      AggDataBase
037700090327     C                   END
037800090327     C*
037900090319     C                   ENDIF
038000090319     C*
038100090319     C                   ENDSR
038200090319      ************************************************************
038300090319      *?  INIZIALIZZAZIONE VIDEATA 2                       ?
038400090319      ************************************************************
038500090319     C     INZD2         BEGSR
038600090319     C*
038700090320     C                   z-add     H1recTRS      H2recTRS
038800090317     c                   eval      d2PGM  =  D1PGM
038900090317     c                   eval      d2NRC  =  D1Nrc
039000090319     c                   eval      H2NRC  =  H1Nrc
039100090317     c                   eval      D2Ann  =  D1Ann
039200090317     c                   eval      D2Fun  =  D1Fun
039300090317     c                   eval      D2FIL  =  D1FIL
039400090317     c                   eval      D2DFIL =  D1DFIL
039500090317     c                   eval      D2SOCG =  D1SOCG
039600090317     c                   eval      D2DSCG =  D1DSoCG
039700090317     c                   eval      D2IVA  =  D1IVA
039800090410     C                   eval      d2ProJ = S1_RagSocKSC
039900090319      *
040000090319     c                   if        trsdec > 0
040100090319     c                   move      TRsDec        dataiso
040200090319     c                   move      dataiso       dataeur
040300090319     c                   move      dataeur       d2DEC
040400090324     c                   move      TRsDec        h2DEC
040500090319     c                   endif
040600090319      *
040700090319     c                   if        trsdfc > 0
040800090319     c                   move      TRsDfc        dataiso
040900090319     c                   move      dataiso       dataeur
041000090319     c                   move      dataeur       d2DfC
041100090324     c                   move      TRsDfc        h2DfC
041200090319     c                   endif
041300090319
041400090319     c                   if        trsdrc > 0
041500090319     c                   move      TRsDrc        dataiso
041600090319     c                   move      dataiso       dataeur
041700090319     c                   move      dataeur       d2DrC
041800090324     c                   move      TRsDrc        h2DrC
041900090319     c                   endif
042000090319
042100090330     c                   setoff                                       15
042200090319     c                   if        trsdsc > 0
042300090330     c                   seton                                        15
042400090319     c                   move      TRsDsc        dataiso
042500090319     c                   move      dataiso       dataeur
042600090319     c                   move      dataeur       d2DsC
042700090324     c                   move      TRsDsc        h2DsC
042800090319     c                   endif
042900090319
043000120508     c*********          eval      d2CFI = TRSCFI
043100120217      *
043200120217      *    riporta sulla DS i campi del filler
043300120216     c                   eval      dTRSflr = TRSflr
043400120216     c                   eval      hTRSflr = TRSflr
043500150305     c                   eval      dTRSflr1= TRSflr1
043600150305     c                   eval      hTRSflr1= TRSflr1
043700150305      **
043800150305      ** DATA    D.U.R.C.
043900150305     c                   if        §trsdDURC <> *zeros and
044000150305     c                             §trsdDURC <> *blank
044100150305     c                   move      §trsdDURC     §trsdDURC0        8 0
044200150305     c                   move      §trsdDURC0    dataiso
044300150305     c                   move      dataiso       dataeur
044400150305     c                   move      dataeur       d2dDURC
044500150305     c                   move      §trsdDURC     h2dDURC
044600150305     c                   endif
044700150305      **
044800150305      ** DATA    C.I.P.
044900150305     c                   if        §trsdCIP  <> *zeros and
045000150305     c                             §trsdCIP  <> *blank
045100150305     c                   move      §trsdCIP      §trsdCIP0         8 0
045200150305     c                   move      §trsdCIP0     dataiso
045300150305     c                   move      dataiso       dataeur
045400150305     c                   move      dataeur       d2dCIP
045500150305     c                   move      §trsdCIP      h2dCIP
045600150305     c                   endif
045700150305      ****
045800120217      *   quindi porta a video i 2 flags:
045900120217      **** Stabilimento e Q.li dichiarati nella iscrizione all'Albo
046000150305     c****               eval      d2STB = §TRSSTB
046100150305     c****               eval      d2QLI = §TRSQLI
046200150305AB    ***
046300150305AB    *** TOLTI DAL VIDEO3 i 3 campi sostituiti con le 2 date DURC e CIP:
046400150305AB    ***  Adeguamento L.395/1071..:
046500150305AB    ***  Stabilimento............:
046600150305AB    ***  Portata Q.li ...........:
046700150305      ****
046800120217      *  decodifica i Qli. riportati nel flag
046900150305     c****               clear                   d2dqli
047000150305     c****               if          d2QLI <> *blank
047100150305     c****               movel     'QLI'         TBECOD
047200150305     c****               movel(p)  d2QLI         TBEke1
047300150305     c****               clear                   TBEke2
047400150305     c**** k_TBE         chain     tnTBE01l
047500150305     c****               if        %Found(tnTBE01L)
047600150305     c****               movel     tbeuni        d2dqli
047700150305     c****               end
047800150305     c****               endif
047900150305      ****
048000090319     c                   eval      d2COR = TRSCOR
048100150305     c******             eval      d2IDP = TRSIDP
048200120508     c********           eval      d2IDM = TRSIDM
048300120508     c********           eval      d2IDA = TRSIDA
048400090319     c                   eval      d2CIT = TRSCIT
048500090319     c                   eval      d2NIS = TRSNIS
048600090319     c                   eval      d2PIA = TRSPIA
048700090319     c                   eval      d2NIA = TRSNIA
048800090319     c                   eval      d2LRA = TRSLRA
048900090319
049000090319     c                   if        trsdia > 0
049100090319     c                   move      TRsDia        dataiso
049200090319     c                   move      dataiso       dataeur
049300090319     c                   move      dataeur       d2Dia
049400090319     c                   endif
049500090319     C*
049600090319      * ? * richiamo routine dei ctrl per decodifiche e controlli
049700090319    >C                   EXSR      CTRD2
049800090317     C*
049900090317     C                   ENDSR
050000090317     C************************************************************
050100090319      *?  CONTROLLO VIDEATA 2 CAMPI                        ?
050200090317     C************************************************************
050300090317     C     CTRD2         BEGSR
050400090317      *
050500090317     C                   SETOFF                                       99
050600090330      *
050700090330      ***  se Contratto già stampato protegge i campi sensibili da non
050800090330      **  modificare più.
050900090330     c                   setoff                                       15
051000090330     c                   if        trsdsc > 0
051100090330     c                   seton                                        15
051200090330     c                   end
051300090319      *
051400090407      * Data emissione Contratto
051500090323      * gira la data in AMG
051600090407     c                   clear                   h2DeC
051700090323$017 C     d2dec         IFgt      *zeros
051800090323     c                   move      d2dec         dataeur
051900090323     c                   move      dataeur       dataiso
052000090323     c                   move      dataiso       h2DeC
052100090323     C                   end
052200090320      *
052300090319      * Data rientro copia firmata
052400090324      *  deve essere superiore alla data Emissione Contratto e alla Data di Stampa
052500090323     c                   clear                   h2DrC
052600090323      * gira la data in AMG
052700090323$017 C     d2drc         IFgt      *zeros
052800090323     c                   move      d2drc         dataeur
052900090323     c                   move      dataeur       dataiso
053000090323     c                   move      dataiso       h2DrC
053100090323     C                   end
053200090319      *
053300090319      *   Data Iscriz.Albo
053400090323     c                   clear                   h2Dia
053500090323      * gira la data in AMG
053600090323$017 C     d2dia         IFgt      *zeros
053700090323     c                   move      d2dia         dataeur
053800090323     c                   move      dataeur       dataiso
053900090323     c                   move      dataiso       h2Dia
054000090323     C                   end
054100090319      *
054200090319      *   Idoneità Professionale
054300090319      *   Mese/Anno
054400120508     c*********          clear                   D2DIDP
054500120508     c*********          if        D2IDP <> *blank
054600120508     c*********          clear                   tibs02ds
054700120508     c*********          movel     'C'           t02mod
054800120508     c*********          movel     knsif         t02sif
054900120508     c*********          movel     'AIP'         t02cod
055000120508     c*********          movel     D2IDP         t02ke1
055100120508     c*********          call      'TIBS02R'
055200120508     c*********          parm                    KPJBA
055300120508     c*********          parm                    TIBS02DS
055400120508     c*********          if        t02err =  *blanks
055500120508     c*********          movel     t02uni        daip
055600120508     c*********          movel     §aipdes       D2DIDP
055700120508     c*********          else
055800120508     C*********          SETON                                        5899
055900120508     c*********          leavesr
056000120508     c*********          endif
056100120508     c*********          endif
056200120508      *********
056300120508     c*********          if        d2idm > 0  or d2ida > 0
056400120508     c*********          if        D2IDP = *blank
056500120508     C*********          SETON                                        5899
056600120508     c*********          leavesr
056700120508     c*********          endif
056800120508     c*********          endif
056900090319      *
057000090324      *  SE OBBLIGATORI anno/mese
057100120508     C*********          IF        §AIPMEAN = 'S' AND d2idm = 0
057200120508     C*********          SETON                                        5999
057300120508     c*********          leavesr
057400120508     c*********          endif
057500090327      *---------------
057600120217      * Ricerca
057700150305     c**** '?'           scan      d2qli
057800150305    3c****               if        %found
057900150305     c****               clear                   tibs02ds
058000150305     C****               MOVEL     'R'           t02mod
058100150305     C****               MOVEL     knsif         t02sif
058200150305     C****               MOVEL     'QLI'         t02cod
058300150305      ****
058400150305     C****               CALL      'TIBS02R'
058500150305     C****               PARM                    KPJBA
058600150305     C****               PARM                    TIBS02DS
058700150305      ****
058800150305    4C****               IF        T02err = *BLANKS
058900150305     C****               movel     T02uni        D2dQLI
059000150305     C****               movel     T02ke1        d2qli
059100150305      ** errore (non ritorna nulla dalla selezione pulisco il campo)
059200150305     C****               ELSE
059300150305     c****               clear                   D2dQLI
059400150305     c****               clear                   d2qli
059500150305    4C****               ENDIF
059600150305     c****               end
059700150305      ****
059800150305      * Decodifica il flag dei Qli inseriti sull'ALBO
059900150305     c****               clear                   d2dqli
060000150305     c****               if          d2QLI <> *blank
060100150305     c****               movel     'QLI'         TBECOD
060200150305     c****               movel(p)  d2QLI         TBEke1
060300150305     c****               clear                   TBEke2
060400150305     c**** k_TBE         chain     tnTBE01l
060500150305     c****               if        %Found(tnTBE01L)
060600150305     c****               movel     tbeuni        d2dqli
060700150305     c****               else
060800150305     C****               SETON                                        7899
060900150305     c****               end
061000150305     c****               endif
061100150309      *
061200150309      * Date documentazione DURC/CIP vuote
061300150309     c                   if        d2Ddurc = *zeros and d2dCIP = *zeros
061400150309     C                   SETON                                        7899
061500150309     c                   leavesr
061600150309     C                   ENDIF
061700150305      *
061800150305      * Data documentazione DURC
061900150305     c                   clear                   h2Ddurc
062000150305$017 C     d2Ddurc       IFgt      *zeros
062100150305     c     *eur          test(d)                 d2Ddurc                58
062200150305     c                   if        *in58
062300150305     c     *dmy          test(d)                 d2Ddurc                58
062400150305     c  n58*dmy          move      d2Ddurc       dataeur
062500150305     c  n58              move      dataeur       d2Ddurc
062600150305     C                   end
062700150305     C                   end
062800150305     c                   if        d2Ddurc <> *zeros and *in58
062900150305     C                   SETON                                        5899
063000150305     c                   leavesr
063100150305     C                   ENDIF
063200150305      *
063300150305      * gira la data in AMG
063400150305$017 C     d2dDURC       IFgt      *zeros
063500150305     c                   move      d2dDURC       dataeur
063600150305     c                   move      dataeur       dataiso
063700150305     c                   move      dataiso       h2Ddurc
063800150305     C                   end
063900150305      **
064000150305      * Data documentazione CIP
064100150305     c                   clear                   h2Dcip
064200150305$017 C     d2Dcip        IFgt      *zeros
064300150305     c     *eur          test(d)                 d2Dcip                 59
064400150305     c                   if        *in59
064500150305     c     *dmy          test(d)                 d2Dcip                 59
064600150305     c  n59*dmy          move      d2Dcip        dataeur
064700150305     c  n59              move      dataeur       d2Dcip
064800150305     C                   end
064900150305     C                   end
065000150305     c                   if        d2Dcip  <> *zeros and *in59
065100150305     C                   SETON                                        5999
065200150305     c                   leavesr
065300150305     C                   ENDIF
065400150305      *
065500150305      * gira la data in AMG
065600150305$017 C     d2Dcip        IFgt      *zeros
065700150305     c                   move      d2Dcip        dataeur
065800150305     c                   move      dataeur       dataiso
065900150305     c                   move      dataiso       h2Dcip
066000150305     C                   end
066100150309      *
066200150309      * Sequenza delle Date DURC/CIP
066300150309     c                   if        h2Ddurc > Data_Oggi
066400150309     C                   SETON                                        7999
066500150309     c                   leavesr
066600150309     C                   ENDIF
066700150309      *
066800150309      * Sequenza delle Date DURC/CIP
066900150309     c                   if        h2Dcip  > Data_Oggi
067000150309     C                   SETON                                        8099
067100150309     c                   leavesr
067200150309     C                   ENDIF
067300150309      *
067400150309      * Sequenza fra le Date DURC/CIP
067500150309     c                   if        h2Dcip  > h2Ddurc
067600150309     c                              and h2Ddurc > 0
067700150309     C                   SETON                                        8199
067800150309     c                   leavesr
067900150309     C                   ENDIF
068000150309      **
068100090317      *
068200960305     C                   ENDSR
068300090320     C/EJECT
068400090319      ************************************************************
068500090320      *?  Aggiorna il Data Base                           ?
068600090319      ************************************************************
068700090319     C     AggDataBase   BEGSR
068800090319     C*
068900090407     C* eseguo aggiornamento
069000090407     c                   setoff                                       99
069100090407     c                   exsr      Modifica_Db2
069200090319      *
069300090319     C* eseguo operazioni del dopo-aggiornamento
069400090319     C  N99              EXSR      GESAGG
069500090330      *
069600090330     c   99              eval      d4FUN  =  D1FUN
069700090324     c   99              exfmt     ALERTWIND
069800090319     C*
069900090319     C                   ENDSR
070000090317     c*--------------------------------------------------------------
070100090317     c* tramite Società e Unità decodifica P.IVA su PROJ
070200090317     c*--------------------------------------------------------------
070300090317     C     Decod_Rag_IVA BegSR
070400090317      **
070500090317      **  Routine x Reperire dati Fornitore:
070600090317      **    La routine in base alla sottonatura può funzionare
070700090317      **     x F=Fornitore/C=Cliente
070800090410     C                   clear                   trmz70s1ds                     Input
070900090410     C                   movel(p)  TRSiva        s1_PartitaIVA                  Input
071000090410     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
071100090410     C                   movel(p)  TRSSOCG       s1_Societa                     Input/Output
071200090410     C                   movel(p)  TRSFIL        s1_Unita                       Input/Output
071300090317      *
071400090401     c                   call      'TRMZ70SR1'
071500090410     C                   PARM                    trmz70s1ds                     Input
071600090317     c*
071700090317     C                   ENDSR
071800090317     C/EJECT
071900090407     C************************************************************
072000090407      *
072100090319     C************************************************************
072200090319     C     Decod_societa begSR
072300090319      *
072400090319     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
072500090319     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
072600090319     C                   RESET                   tibsSocI0
072700090319     C                   EVAL      tibsSocI0.idSocieta = wSOCG
072800090319     C
072900090319     C                   CALL      'TIBSSOCR'
073000090319     C                   PARM      'GETANAGRAF'  prmRqsOpCode
073100090319     C                   PARM      *BLANK        prmRpyOpCode
073200090319     C                   PARM      *ZERO         prmRpyIdMsg
073300090319     C                   PARM      'TIBSSOCI0'   prmRqsFormato
073400090319     C                   PARM                    tibsSocI0
073500090319     C                   PARM                    prmRqsDataSize
073600090319     C                   PARM      'TIBSSOCO0'   prmRpyFormato
073700090319     C                   PARM                    tibsSocO0
073800090319     C                   PARM                    prmRpyDataSize
073900090323     c                   if         PRMRPYIDMSG >= 0
074000090402     c                              and TIBSSOCO0.IDSOCIETA <> *blank
074100090323     c                   eval      wsocGd = tibsSocO0.RAGSOCIALE
074200090323     c                   else
074300090323     c                   eval      wsocGd = 'Errore'
074400090323     c                   end
074500090319      *
074600090319     C                   ENDSR
074700090406     C************************************************************
074800090406     C* GESTIONE F04 interrogazioni
074900090406     C************************************************************
075000090406     C     F04Ricerche   BEGSR
075100090406      *
075200090407     c*****              if        H2NMFL = 'D2AUTC'
075300090407     c*****              eval      kpjbuS = kpjbu
075400090407     c*****              eval      kpjbu  = kpjbuS
075500090407     c*****              leavesr
075600090407     c*****              end
075700090406      *
075800090402     C                   ENDSR
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
078200090320     C     H1recTRS      chain(N)  AITRs00F
078300090320     C                   if        %Found(AITRs00F)
078400090320     c                   move      'S'           trovato_TRS
078500090320     c                   eval      H2recTRS = H1recTRS
078600090320     c                   if        TrsANN <> *blank
078700090320     c                   seton                                        23
078800090320     C                   MOVEl     Annull        D1Ann
078900090320     C                   end
079000090320     c                   endIF
079100090320     C*
079200090320     C                   ENDSR
079300090319     C/EJECT
079400090330     C************************************************************
079500090330     C* Modifica Dati
079600090330     C************************************************************
079700090330     C     Modifica_Db2  BEGSR
079800090330     C*
079900090331     C     h2recTRS      chain     aitrs00f
080000090330     c                   eval      trsann = *blank
080100090330    >C                   EXSR      TRS_Modifica
080200090330     c                   update    aitrs000                             79
080300090330     C* se rcd non scrivibile attivo errore
080400090330    >C   79              SETON                                          99
080500090330      *
080600090330     C                   ENDSR
080700090330     C************************************************************
080800090330     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
080900090330     C************************************************************
081000090330     C     Imposta_Campi BEGSR
081100090330     C*
081200090324     C*  del TRS in immissione
081300090324     C*
081400090319     C* imposta i campi dal video al file
081500090327     c                   eval      TRSDVA  =  Data_oggi
081600090324     c                   exsr      TRS_Modifica
081700090319     C*
081800090319     C                   ENDSR
081900090324     C************************************************************
082000090324     C* Campi di Modifica   TRS
082100090324     C************************************************************
082200090324     C     TRS_Modifica  BEGSR
082300090324     C*
082400090324     c                   eval      TRsDEC   =  h2DEC
082500090324     c                   eval      TRsDrC   =  h2DrC
082600120508     c*********          eval      TRsCFI   =  D2CFI
082700090324     c                   eval      TRsCOR   =  D2COR
082800150305     c******             eval      TRsIDP   =  D2IDP
082900120508     c*********          eval      TRsIDM   =  D2IDM
083000120508     c*********          eval      TRsIDA   =  D2IDA
083100090324     c                   eval      TRsCIT   =  D2CIT
083200090324     c                   eval      TRsNIS   =  D2NIS
083300090324     c                   eval      TRsDIA   =  h2DIA
083400090324     c                   eval      TRsPIA   =  D2PIA
083500090324     c                   eval      TRsNIA   =  D2NIA
083600090324     c                   eval      TRsLRA   =  D2LRA
083700090324     c                   eval      TRsUTE   =  knmus
083800090324     C*
083900120216      *      riporta sulla DS i campi del filler
084000120216      **** 2 flag : Stabilimento e Q.li dichiarati nella iscrizione all'Albo
084100150305     c*********          eval      dTRSflr = hTRSflr
084200120217      *  imposta eventuali modifiche ai 2 flag
084300150305      *****    e reimposta il campo DS
084400150305     c*********          eval      §TRSSTB = d2STB
084500150305     c*********          eval      §TRSQLI = d2QLI
084600150305     c*********          eval      TRsFLR   =  dTRSflr
084700150305     C*
084800150305     c                   eval      dTRSflr1= hTRSflr1
084900150305      *  imposta eventuali modifiche alle 2 DATE
085000150305      *        e reimposta il campo DS
085100150305     c                   move      h2Ddurc       §TRSDdurc
085200150305     c                   move      h2Dcip        §TRSDcip
085300150305     c                   eval      TRsFLR1  =  dTRSflr1
085400120216     C*
085500090324     C                   ENDSR
085600090324     C************************************************************
085700090324     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
085800090324     C************************************************************
085900090324     C     GESAGG        BEGSR
086000090327     C*
086100090327     C*
086200090319     C* segnalo al pgm chiamante l'avvenuta conferma
086300090331    >C********           MOVE      *ON           xtaopr
086400090331     C*   non lo imposto + a *ON per non ricaricare nuovamente il SFL
086500090331     C*   nel chiamante.
086600090331     C*
086700090319     C* segnalo al pgm chiamante l'aver premuto l'F6
086800090319    >C                   MOVE      '0'           xtaret
086900090319     C*
087000090327     C*  E deve chiudere
087100090327     C                   MOVE      *ON           $FINE
087200090327     C*
087300090319     C*
087400090319     C                   ENDSR
087500090320     C************************************************************
087600090320      *? DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...   ?
087700090320     C************************************************************
087800090320     C     DEFCAM        BEGSR
087900090320     C*
088000090320     C* Variabili per gestione videate
088100090320     C                   MOVE      *BLANK        $GEST             2
088200090320     C                   MOVE      *BLANK        $FINE             1
088300090320     C                   MOVE      *BLANK        $INZD1            1
088400090320     C                   MOVE      *BLANK        $INZD2            1
088500090320     C                   MOVE      *BLANK        $LASTG            2
088600090320     C                   MOVE      *BLANK        $LASTV            2
088700090320     C*
088800090320     C* Variabili appoggio
088900090320     C                   Z-ADD     0             CURR              2 0
089000090320     C                   Z-ADD     0             CURC              2 0
089100090320     C                   MOVE      *ZEROS        WIN              99
089200090320     C                   MOVE      *BLANK        WTBERR            1
089300090320     C*
089400090320     c     Kaut          klist
089500090320     c                   kfld                    tipo              1
089600090320     c                   kfld                    autista           7 0
089700120217     C*
089800120217     c     K_TBE         klist
089900120217     c                   kfld                    tbeCOD
090000120217     c                   kfld                    tbeKE1
090100120217     c                   kfld                    tbeKE2
090200090320     C*
090300090320     C* Indici
090400090320     C                   Z-ADD     0             X                 3 0
090500090320     C                   Z-ADD     0             Y                 3 0
090600090320     C*
090700090320     C                   ENDSR
090800090407     C/EJECT
090900090407     C************************************************************
091000090407      *?  OPERAZIONI INIZIALI                             ?
091100090407     C************************************************************
091200090407     C     *INZSR        BEGSR
091300090407     C*
091400090407     C* Reperimento parametri
091500090407     C*
091600090407     C     *ENTRY        PLIST
091700090407     C                   PARM                    KPJBA
091800090407     C                   movel     kpjbu         Xtabds
091900090407     c                   setoff                                       23
092000090407     C*
092100090407     C* Variabili per gestione videate
092200090407     C                   MOVE      *OFF          $FINE
092300090407     C                   MOVE      *ON           $INZD1
092400090407     C                   MOVE      *BLANK        $LASTG
092500090407     C                   MOVE      *BLANK        $LASTV
092600090407     C                   Z-ADD     0             $ULKD1            3 0
092700090407     C*
092800090407     C* Variabili appoggio
092900090407     C                   Z-ADD     0             CURR
093000090407     C                   Z-ADD     0             CURC
093100090407     C*
093200090407     C* Valorizzazione campi univoci testate
093300090407     C                   MOVEL     KNSIF         NOMSIF
093400090407     C                   MOVEL     DSPGM         NOMPGM           10
093500090407     C*
093600090407     C* ESSENDO SEQUENZIALI I 3 DISPLAY LI PULISCO TUTTI UNA VOLTA SOLA
093700090407     C* Pulisce entrambe i 3 video
093800090407     C                   CLEAR                   D1
093900090407     C                   CLEAR                   D2
094000090407     C*
094100090407     C*  giro la udate da GMA in AMG
094200090407     C                   move      *date         dataeur
094300090407     C                   move      dataeur       dataiso
094400090407     C                   move      dataiso       data_oggi
094500090407     C*
094600090407     C* Inizializzazione/Decodifica prima videata e seconda
094700090407     C                   MOVE      'D1'          $GEST
094800090407     C                   MOVE      *ON           $INZD1
094900090407     C                   MOVE      *OFF          $INZD2
095000090407     C*
095100090407     C*  Campi protetti o HI o RI
095200090407     C                   SETOFF                                       020304
095300090407     C                   SETOFF                                       050607
095400090407     C                   SETOFF                                       080911
095500090407     C                   SETOFF                                       101213
095600090407     C                   SETOFF                                       1516
095700090407     C*
095800090407     c                   movel     Opz_Var       D1Fun
095900090407     C                   SETON                                        02
096000090407     C*
096100090407     C                   ENDSR
096200090317     C************************************************************
