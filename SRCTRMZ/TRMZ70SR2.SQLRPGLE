000100120727     H OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR('PRNPGM') ACTGRP('QILE')
000200970214     H DECEDIT('0,') DATEDIT(*DMY.)
000300940223      *
000400940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000500940620      *  03           PROTECT TUTTI I CAMPI
000600160105      *  06           PROTECT TUTTI I CAMPI
000700090608      *  19           se Contratto RISTAMPATO
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
002500090319     FAITRS00f  UF A E             DISK    commit
002600090320     FAitra00f  uF a E             DISK    commit
002700090331      *
002800090324     FAitra01l  uF   E           k DISK    commit
002900090324     F                                     rename(Aitra000:Aitra001)
003000090423     FAitra03l  iF   E           k DISK    commit
003100090423     F                                     rename(Aitra000:Aitra003)
003200090423     FAitra05l  iF   E           k DISK    commit
003300090423     F                                     rename(Aitra000:Aitra005)
003400090319      *
003500090319     FAitrs02L  iF   E           K DISK    rename(Aitrs000:Aitrs002)
003600150624     FAitrc01L  iF   E           K DISK
003700090331      *
003800090506     FfiADT01L  iF   E           K DISK
003900090506     FfiTGT01L  iF   E           K DISK
004000090319     FAzorg01L  iF   E           K DISK
004100090416     Ftabel00F  iF   E           K DISK
004200120217     FtnTBE01l  iF   E           K DISK
004300090319     FFiapd01L  iF   E           K DISK
004400090409     FanRCO98j  iF   E           k disk
004500090317      *
004600090311$001 FTRMZ70SD2 CF   E             WORKSTN
004700940201     F                                     INFDS(DSFMT)
004800940128     D*----------------------------------------------------*
004900940127     D* Passaggio Parametri
005000940127     D KPJBA         E DS
005100090320     D KPJBus          S                   like(KPJBU)
005200090317     D trul33ds      E DS
005300090319     D tibs02ds      E DS
005400090409     D*** fnlv24ds      E DS
005500090409     D fnlv28ds      E DS
005600090410     D fnlv55ds      E DS
005700090408     D trmz70s1ds    E DS                  prefix(S1_)
005800090506     D trmz70s4ds    E DS                  prefix(S4_)
005900150624     D trmz70s7ds    E DS                  prefix(S7_)
006000150624     D trmz70s8ds    E DS                  prefix(S8_)
006100090522     D parm_s4CDF      s                   like(tgtCDF)
006200090522     D parm_s4SOC      s                   like(tgtSOC)
006300090522     D parm_s4FIL      s                   like(tgtFIL)
006400090617     D cod__TGTSOC     s                   like(tgtSOC)
006500090617     D cod__TGTCDF     s                   like(tgtCDF)
006600170302     D sav4ris         S                   like(d4ris)
006700090522     D Data_confronto  s                   like(d2dec)
006800090522     D PIVA_confronto  s                   like(d2IVA)
006900091130     D  Soc_confronto  s                   like(d2socG)
007000110401     dprimo_tentativo  s              1
007100120322     D TraPIVA         s                   like(trsIVA)
007200170221     d dTPA          e ds
007300090319     d daip          e ds
007400090416     d dsPR          e ds
007500160204     d dsrg          e ds
007600090608     d dtrsflr       E DS
007700150305     d dtrsflr1      E DS
007800150623     d dtrafiller    E DS
007900130527     d VALUE_IND       s              5i 0
008000130527     d PIVA_lunga11    s             13
008100170324     d Forza_cFISC     s              1
008200030113      *-------------
008300030113     D* Parametri in ricezione
008400090311     D XTABDS          DS
008500090311     D  XTAOPZ                 1      1
008600090311     D  XTAFLG                 2      2
008700030113     D  XTARET                 3      3
008800030113     D  XTAOPR                 4      4
008900030113     D  XTAERR                 5      5
009000090319     D  XTArecord              6     14  0
009100090319     D  XTAKEY                15     21  0
009200160105      *--------------------------
009300160105      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
009400160105     D WLBDAT          DS                  INZ
009500160105     D  G02DAT                 1      8  0
009600160105     D  G02INV                 9     16  0
009700160105     D  G02ERR                17     17
009800160105     D  G02TGI                18     22  0
009900160105      *
010000940201     D*-------------
010100090320     D* Parametri in ricezione x selezionare l'Autista di AITRA da modificare
010200090320     D ataBDS          DS
010300090320     D  ataOPZ                 1      1
010400090320     D  ataFLG                 2      2
010500090320     D  ataRET                 3      3
010600090320     D  ataOPR                 4      4
010700090320     D  ataERR                 5      5
010800090320     D  atarecord              6     14  0
010900090320     D  ataKEY                15     21  0
011000090320     D*-------------
011100030204     D  S_TASTO        S                   like($Tasto)
011200940201     D DSFMT           DS
011300940506     D  $TASTO               369    369
011400940201     D  NRG                  370    370
011500940201     D  NCL                  371    371
011600940201     D*-------------
011700940201     D* posizione cursore
011800940201     D CURSOR          DS
011900940223     D  RIRI                   1      2B 0 INZ
012000940201     D  R$$                    2      2
012100940223     D  COCO                   3      4B 0 INZ
012200940201     D  C$$                    4      4
012300940207     D*-------------
012400940207     D* Reperimento nome PGM
012500940207     D STATUS         SDS           333
012600940207     D  DSPGM            *PROC
012700940225     D*-------------
012800940225     D* COSTANTI
012900940225     D*-------------
013000940506     D* Tasti di funzione
013100940506     D F01             C                   CONST(X'31')
013200940506     D F02             C                   CONST(X'32')
013300940506     D F03             C                   CONST(X'33')
013400940506     D F04             C                   CONST(X'34')
013500940506     D F05             C                   CONST(X'35')
013600940506     D F06             C                   CONST(X'36')
013700940506     D F07             C                   CONST(X'37')
013800940506     D F08             C                   CONST(X'38')
013900940506     D F09             C                   CONST(X'39')
014000940506     D F10             C                   CONST(X'3A')
014100940506     D F11             C                   CONST(X'3B')
014200940506     D F12             C                   CONST(X'3C')
014300940506     D F13             C                   CONST(X'B1')
014400940506     D F14             C                   CONST(X'B2')
014500940506     D F15             C                   CONST(X'B3')
014600940506     D F16             C                   CONST(X'B4')
014700940506     D F17             C                   CONST(X'B5')
014800940506     D F18             C                   CONST(X'B6')
014900940506     D F19             C                   CONST(X'B7')
015000940506     D F20             C                   CONST(X'B8')
015100940506     D F21             C                   CONST(X'B9')
015200940506     D F22             C                   CONST(X'BA')
015300940506     D F23             C                   CONST(X'BB')
015400940506     D F24             C                   CONST(X'BC')
015500940506     D ENTER           C                   CONST(X'F1')
015600940506     D ROLDWN          C                   CONST(X'F4')
015700940506     D ROLLUP          C                   CONST(X'F5')
015800940506     D*-------------
015900941108     D DATA            C                   CONST('0001-01-01')
016000030114     D Annull          C                   CONST('  ANNULLATO  ')
016100090318     D OPz_Imm         C                   CONST('     IMMISSIONE     ')
016200090318     D OPz_Var         C                   CONST('     VARIAZIONE     ')
016300090318     D OPz_Del         C                   CONST('       ANNULLA      ')
016400090318     D OPz_Vis         C                   CONST('     VISUALIZZA     ')
016500090313     D OPz_AGGaut      C                   CONST('  AGGIUNTA AUTISTA  ')
016600090313     D OPz_CMBsoc      C                   CONST('   CAMBIO SOCIETA   ')
016700090317     D OPz_CMBpiva     C                   CONST('   CAMBIO  P.IVA    ')
016800090313     D OPz_FINEc       C                   CONST('   FINE CONTRATTO   ')
016900090327     D OPz_ModAUTista  C                   CONST('  MODIFICA AUTISTA  ')
017000090706     D OPz_RicezC      C                   CONST('  RICEZ. CONTRATTO  ')
017100160111     D OPz_MACscorta   C                   CONST('IMMIS.AUTO DI SCORTA')
017200120323     D ContrSCADUT     C                   CONST('*_ SCADUTO __*')
017300090318
017400091215     D Attivato_a_Perdere...
017500100219     d                 c                   CONST('S')
017600091215
017700110505     D Altra_Filiale   C                   CONST('Filiale che cambia la Ns.Soci-
017800110505     d                                     età Operativa :')
017900090331     D cosa_Societa    C                   CONST('   la nuova Società in data:')
018000090331     D cosa_PIVA       C                   CONST('     la nuova P.IVA in data:')
018100090331     D cosa_DATAfine   C                   CONST(' la  Data  Fine  Contratto :')
018200090318     D NOTA1           C                   CONST('NB.: verrà impostata la data -
018300090318     d                                     di Fine Contratto sulla')
018400090331     D NOTA2           C                   CONST('     Società attuale e disacc-
018500090331     d                                     reditati tutti gli autisti')
018600090320     D NOTA3           C                   CONST('  verrà poi aperta una nuova -
018700090320     D                                     società con stessa data')
018800090319      *
018900090319      * Opzioni --> Azioni da fare :
019000090319     D Immissione      C                   CONST('1')
019100090319     D Modifica        C                   CONST('2')
019200090319     D Copia           C                   CONST('3')
019300090319     D Cancella        C                   CONST('4')
019400090319     D Visualizza      C                   CONST('5')
019500090319     D DataFineC       C                   CONST('D')
019600090319     D AddAutista      C                   CONST('A')
019700090319     D CambiaPIva      C                   CONST('P')
019800090319     D CambiaSocieta   C                   CONST('S')
019900090327     D ModAutista      C                   CONST('M')
020000090706     D DataRicezione   C                   CONST('R')
020100160108     D AddAutoDiScor   C                   CONST('Z')
020200090319     D Acceso          C                   '1'
020300090319     D Spento          C                   '0'
020400090323     D*-------------
020500090323      *
020600090323      * Controllo IVA
020700151117     d xcfiva1ds     e ds
020800090323     d  ivaeu                  3      4
020900090323     d  ivait                  5     18
021000090317
021100090408      *----
021200090331     d Da_duplicare    s              1
021300090319     d Trovato_TRS     s              1    inz('N')
021400090319     d Trovato_AITRA   s              1    inz('N')
021500090320     d savin10         s              1
021600120323     d savin           s             99
021700090317      *
021800151223     D F15ivac         S                   like(d4ivac)
021900101102     D tgtpdr_sav      S                   like(tgtpdr)
022000090317     D Wsocg           S                   like(d1socG)
022100090317     D Wsocgd          S                   like(d1dsocG)
022200090317     d dataiso         s               d   datfmt(*iso)
022300090317     d dataeur         s               d   datfmt(*eur)
022400090323     d Data_oggi       s              8  0
022500160204     d Datapiu3m       s              8  0
022600160128     d Data_5annIpiu   s              8  0
022700090318     d Errore          s              1
022701180213     d Esiste          s              1  0
022702180213
022800090506     d SOLO_decodifica...
022900090506     d                 s              1
023000090507     d se_accreditato  s              1
023100090507     d se_convalidata  s              1
023200090506     d se_attiva       s              1
023300090506     d in_stesso_die   s              1
023400090508     d ultimo_disaccr  s              8s 0
023500110510     d Tariffa_Trovata...
023600110510     d                 s              1
023700090317     ***************************************************************************
023800090317     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
023900090317     D ESITO_OK...
024000090317     D                 C                   0
024100090317     ***************************************************************************
024200090317     **
024300090317     ** Dichiarazione prototipi procedure esterne.
024400090317     **
024500090317     ***************************************************************************
024600090317      /DEFINE DFTACTGRP_YES
024700090317     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
024800090317      /UNDEFINE DFTACTGRP_YES
024900090317     ***************************************************************************
025000090317     **
025100090317     ** Definizione strutture dati.
025200090317     **
025300090317      **************************************************************************
025400090317     D tibsSocI0...
025500090317     D               E DS                  QUALIFIED
025600090317     D                                     INZ
025700090317     D tibsSocO0...
025800090317     D               E DS                  QUALIFIED
025900090317     D                                     INZ
026000090317     ***************************************************************************
026100090317     **
026200090317     ** Definizione variabili modulo/programma.
026300090317     **
026400090317     ***************************************************************************
026500090317     D prmRqsOpCode...
026600090317     D                 S             10A
026700090317     D prmRpyOpCode...
026800090317     D                 S             10A
026900090317     D prmRpyIdMsg...
027000090317     D                 S             10I 0
027100090317     D prmRqsFormato...
027200090317     D                 S             10A
027300090317     D prmRqsDataSize...
027400090317     D                 S             10I 0
027500090317     D prmRpyFormato...
027600090317     D                 S             10A
027700090317     D prmRpyDataSize...
027800090317     D                 S             10I 0
027900090317
028000090409     d digits          c                   '0123456789'
028100940127     C*----------------------------------------------------*
028200940127     C*                MAIN LINE PROGRAM
028300940127     C*----------------------------------------------------*
028400090319     ** Inizializzo il programma.
028500090319     C                   CALL      'TIBSSOCR'
028600090319     C                   PARM      'INIT'        prmRqsOpCode
028700090319     C                   PARM                    prmRpyOpCode
028800090319     C                   PARM                    prmRpyIdMsg
028900940223     C*
029000090319      * ? * Entra nella sequenza delle 3 videate + aggiornamento D.Base *
029100940223     C     $FINE         DOWEQ     *OFF
029200940202     C     $GEST         CASEQ     'D1'          GESD1
029300090319     C     $GEST         CASEQ     'D2'          GESD2
029400090319     C     $GEST         CASEQ     'D3'          GESD3
029500090320     C     $GEST         CASEQ     'D4'          GESD4
029600151223     C     $GEST         CASEQ     'D5'          GESD5
029700940117     C                   END
029800940117     C                   END
029900090317     C*
030000090317     ** Chiudo il programma.
030100090317     C                   CALL      'TIBSSOCR'
030200090317     C                   PARM      'FINALIZE'    prmRqsOpCode
030300090317     C                   PARM                    prmRpyOpCode
030400090317     C                   PARM                    prmRpyIdMsg
030500090317     C*
030600940325     C* fine programma
030700090311     c                   movel     Xtabds        kpjbu
030800030113     C                   Seton                                        LR
030900940131     C************************************************************
031000090319      *?  GESTIONE VIDEO RECORD D1                               ?
031100940131     C************************************************************
031200940127     C     GESD1         BEGSR
031300030113      *
031400940225     C* inizializzazione videata
031500090319     C* settaggi 1°video campi attivi o campi protetti
031600090319     C     $INZD1        IFEQ      *On
031700940127     C                   EXSR      INZD1
031800940223     C                   MOVE      *OFF          $INZD1
031900940117     C                   END
032000030114     C*
032100090416     C*  Tranne che in visualizzazione
032200090416    >C                   if        xtaopz <> Visualizza
032300090319     c   99              eval      errore = *On
032400090416     c                   end
032500030114     c                   SETOFF                                         99
032600090311     C*
032700090319     C* Solo x immissione / Modifica o Errori
032800090319    >C                   if        xtaopz = Immissione
032900090319     c                               or
033000090319    >C                             errore = *On
033100030113     C*               *----------------*
033200940620     C                   EXFMT     D1
033300030113     C*               *----------------*
033400090318     c                   eval      errore = *off
033500030114     C*
0336000301141    C                   SELECT
033700030114     C* F3=Fine
033800030114     C     $TASTO        WHENEQ    F03
033900090319     C                   EXSR      F03Fine
034000090319     c                   leaveSR
034100030114     C* F12=Ritorno
034200030114     C     $TASTO        WHENEQ    F12
034300090319     C                   EXSR      F12Ritorna
034400090319     c                   leaveSR
034500030114     C*
0346000903202-   C                   ENDSL
034700030114     C*
034800030114     C                   EXSR      CTRD1
034900090319     C*
035000090319     c                   end
035100090319     C* Video 2 o Video 3
035200940506     C     *IN99         IFEQ      *OFF
035300090319      *
035400090319      * se cambio Società/P.IVA o Data fine Contratto Video 3
035500090319    >C                   if        xtaopz = DataFineC
035600090319     c                               or
035700090319    >C                             xtaopz = CambiaPIva
035800090319     c                               or
035900090319    >C                             xtaopz = CambiaSocieta
036000090319      *
036100090319     C                   EVAL      $GEST ='D3'
036200090319     c                   EVAL      $INZD3= *ON
036300090319     c                   else
036400090319      *  Dati Societari
036500090319     C                   EVAL      $GEST ='D2'
036600090319     C                   EVAL      $INZD2= *ON
036700090319     c                   end
036800090319      *
0369000301142-   C                   ENDIF
037000030114     C*
037100940117     C                   ENDSR
037200940117      ************************************************************
037300090319      *?  INIZIALIZZAZIONE VIDEATA 1                       ?
037400940117      ************************************************************
037500940127     C     INZD1         BEGSR
037600090319     C*
037700090312     C                   MOVEL     DSPGM         d1PGM
037800090317     C                   z-add     xtakey        d1NRC
037900090317     C                   z-add     xtakey        H1NRC
038000090320     C                   z-add     xtarecord     H1recTRS
038100090319     c                   move      'N'           trovato_TRS
038200090319     C*
0383000903191   >C     xtaopz        IFEQ      Immissione
038400090319      * ? * Reperisce il Numeratore x Nuova Società *
038500090320     c                   if        H1recTRS = 0
038600090410      **
038700090319     c                   end
038800090319     C*
038900090406     c                   clear                   d2dec
039000090323     C*
0391000903191   >C                   ELSE
039200090319     C*
039300090319      * ? * Reperisce informazioni della società solo se non in IMMISSIONE *
039400090319      *                   il record di AITRS00F ?
039500090320     c                   exsr      Quale_AITRS
039600090319     C                   IF        Trovato_TRS = 'S'
039700090320     C*
039800090320     c                   eval      d1fil    = trsFIL
039900090320     c                   eval      d1SOCG   = trsSOCG
040000090320     c                   eval      d1IVA    = trsIVA
040100090319     C*
040200090319      * ? * richiamo routine dei ctrl per decodifiche e controlli
040300940614    >C                   EXSR      CTRD1
040400030113      *
0405009402242-   C                   ENDIF
0406000903172-   C                   ENDIF
040700940224     C*
040800940117     C                   ENDSR
040900940207     C/EJECT
041000090320     C************************************************************
041100090320      *?  CONTROLLO VIDEATA 1                              ?
041200090320     C************************************************************
041300090320     C     CTRD1         BEGSR
041400090320     C*
041500940127     C                   SETOFF                                       99
041600090317     C*
041700090317     C* controllo codice vuoto  Filiale
041800090317     c                   clear                   d1dfil
041900090317$017 C     D1fil         IFEQ      *zeros
042000090317     C                   SETON                                        5199
042100090319     c                   leavesr
042200090317     C                   Else
042300090410     c     D1Fil         chain     azorg01l
042400090410     c                   if        %Found(azorg01l) and orgfag = 'F'
042500090317     c                   eval      d1dfil = orgdes
042600090317     c                   else
042700090317     C                   SETON                                        5199
042800090319     c                   leavesr
042900090317     c                   end
043000090317     C                   ENDIF
043100090317     C*
043200090317     C* controllo codice vuoto  Società
043300090317     c                   clear                   d1dsocg
043400090317$017 C     D1socG        IFEQ      *blank
043500090317     C                   SETON                                        5299
043600090319     c                   leavesr
043700090317     C                   Else
043800090317     c                   eval      wsocG = d1socG
043900090317     c                   exsr      decod_societa
044000090317     c                   if        prmRpyIdMsg < 0
044100090402     c                              or TIBSSOCO0.IDSOCIETA = *blank
044200090317     C                   SETON                                        5299
044300090319     c                   leavesr
044400090317     c                   else
044500090317     c                   eval      d1dsocG = wSocGd
044600090317     c                   end
044700090317     C                   ENDIF
044800030114     C*
044900090317     C* controllo codice vuoto P.IVA
045000090317$017 C     D1IVA         IFEQ      *ALL' '
045100090317     C                   SETON                                        5499
045200090319     c                   leavesr
045300090317     C                   ELSE
045400090323      **
045500090323      ** Controllo Formale P.IVA
045600090323      *  richiama il nuovo driver fatto per controllare il codice
045700090323      * fiscale o la partita iva
045800151117     c                   clear                   xcfiva1ds
045900090323     c                   eval      xcfivapar = D1iva
046000151117     c                   call      'XCFIVAR1'
046100151117     c                   parm                    xcfiva1ds
046200090323      * --> errore
046300090323     c                   if        XCFIVAFLG < *zeros
046400090323     C                   SETON                                        5499
046500090323     c                   leavesr
046600090323     c                   Else
046700090323      **
046800090317     c                   eval      trsIVA  = d1IVA
046900090317     c                   eval      trsSocG = d1SocG
047000090317     c                   eval      trsFil  = d1Fil
047100090317     c                   exsr      Decod_RAG_IVA
047200090319      *
047300090408     c                   if        S1_Trovato = *Off or S1_Errore  <> '0'
047400090410     c                   if        S1_Errore <>'2'
047500090406     C                   SETON                                        5599
047600090406     c                   leavesr
047700090410     C                   Elseif    S1_Errore = '2'
047800090410     C                   SETON                                        5699
047900090410     c                   leavesr
048000090410     C                   End
048100090317     C                   Else
048200090408     C                   eval      d1ProJ = s1_RagSocKSC
048300090408     C                   eval      d1ProJ1= s1_RagSocKSC1
048400090319      *
048500090323     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
048600090317      * in immissione non ci deve essere la stessa P.IVA x la società Filiale
048700090319    >c     xtaopz        ifEQ      Immissione
048800090323    >C     xtaopz        oreq      Copia
048900090319     c                   eval      TRSSOCG = d1socG
049000090319     c                   eval      TRSIVA  = d1Iva
049100090319     c                   eval      TRSDFC  = 0
049200090327     c     kTRS02        setll     aiTRS02l
049300090327     c     kTRS02        reade     aiTRS02l
049400090327     c                   dow       not %EoF(AITRS02l)
049500090327     c                   if        trsann = *blank
049600090319     C                   SETON                                        5399
049700090319     c                   leavesr
049800090319     C                   End
049900090327     c     kTRS02        reade     aiTRS02l
050000090327     C                   End
050100090317     C                   End
050200090320     C                   End
050300090323     C                   End
050400090317      *
0505009402241-   C                   END
050600090323     C*
050700940117     C                   ENDSR
050800090319     C************************************************************
050900090319      *?  GESTIONE VIDEO RECORD D2                               ?
051000090319     C************************************************************
051100090319     C     GESD2         BEGSR
051200090319      *
051300090319     C* inizializzazione videata
051400090319     C* settaggi 2°video campi attivi o campi protetti
051500090319     C     $INZD2        IFEQ      *ON
051600090319     C                   EXSR      INZD2
051700090319     C                   MOVE      *OFF          $INZD2
051800090319     C                   END
051900090319     C*
052000090319     c   99              eval      errore = *on
052100090319     c                   SETOFF                                         99
052200090324      *
052300090324      * Se si vuole solo visualizzare non si devono segnalare errori
052400090324      *  ma servono solo le decodifiche dei campi.
052500090324     C* Pulizia campi e indicatori
052600090324    >c     xtaopz        ifEQ      Visualizza
052700090324     C                   MOVE      *ALL'0'       IN5069           20
052800090324     C                   MOVEA     IN5069        *IN(50)
052900090324     c                   end
053000090319      *
053100090327      * se aggiunta Autista deve saltare al video di immssione Autista
053200090327    >c     xtaopz        ifEQ      AddAutista
053300090327    >c     xtaopz        oreq      ModAutista
053400090327     C                   write     D2
053500090327     c                   eval      $GEST = 'D4'
053600090327     c                   eval      $inzD4= *on
053700090327     c                   leavesr
053800090327     c                   end
053900151223      *
054000151223      * se aggiunta Macchina di scorta deve saltare alla finestra a parte
054100160108    >c                   if         xtaopz = AddAutoDiScor
054200151223     C                   write     D2
054300151223     c                   eval      $GEST = 'D5'
054400151223     c                   eval      $inzD5= *on
054500151223     c                   leavesr
054600151223     c                   end
054700090327      *
054800090319      *  emette il secondo video dei dati società
054900090319     C*               *----------------*
055000090319     C                   EXFMT     D2
055100090319     C*               *----------------*
055200090319     c                   eval      errore = *off
055300090319     C*
0554000903191    C                   SELECT
055500090406     C*
055600111229     C* F1=Appendici al contratto
055700111229     C     $TASTO        WHENEQ    F01
055800111229     C                   EXSR      F01appendici
055900111229     c                   leaveSR
056000090406     C* F2=Ricerche Tariffe
056100090406     C     $TASTO        WHENEQ    F02
056200090406     C                   EXSR      F02Tariffe
056300090406     c                   leaveSR
056400090406     C*
056500090406     C* F4=Ricerche generiche sui campi
056600090402     C     $TASTO        WHENEQ    F04
056700090406     C                   EXSR      F04Ricerche
056800090409     c                   leaveSR
056900090402     C* F3=Fine
057000090402     C     $TASTO        WHENEQ    F03
057100090402     C                   EXSR      F03Fine
057200090402     c                   leaveSR
057300090319     C* F12=Ritorno
057400090319     C     $TASTO        WHENEQ    F12
057500090319     C                   EXSR      F12Ritorna
057600090319     c                   leaveSR
057700090319     C*
0578000903202-   C                   ENDSL
057900090319     C*
058000090319    >C                   EXSR      CTRD2
058100091215     C*
058200091215     C*  Per ANNULLAMENTO
058300091215     C*   si vuole un'ulteriore conferma di sicurezza sull'operazione
058400091215     c                   if         xtaopz = Cancella
058500091215     c                   exsr      Conf_Cancella
058600091215     C     $TASTO        ifeq      F12
058700091215     C                   EXSR      F12Ritorna
058800091215     c                   leaveSR
058900091215     c                   end
059000091215     c                   end
059100090319      *
059200090319     C     *IN99         IFEQ      *OFF
059300090327     C     $TASTO        ifeq      F06
059400160108     C**** $TASTO        oreq      F08
059500160108     C* l'F8 non ha più senso per come è strutturato adesso il programma
059600160108     C*
059700160108     C*  se in Immissione deve modificare dati dell'autista su finestra D4
059800160108     C*  per completare tutte le informazioni che nel tempo sono cambiate
059900160108     C*  ed aumentate notevolmente.
060000160108    >c     xtaopz        ifEQ      Immissione
060100160108     C                   SETOFF                                       020304
060200160108     C                   SETOFF                                       050607
060300160108     C                   SETOFF                                       080910
060400160108     C                   SETOFF                                       1112
060500160108     C*
060600160108     C*  se in Immissione deve impostare il Video dell'autista
060700160108     C                   SETON                                        13
060800160108     c                   eval      $GEST = 'D4'
060900160108     c                   eval      $inzD4= *on
061000160108     C*
061100160108     C* Avendo spostato tutti i dati dell'autista su D4 è meglio aggiornare
061200160108     C* all'F6 della finestra del D4 se sono in IMMISSIONE
061300160108     C*
061400160108     C                   ElsE
061500160108     C*
061600160108     C*  deve aggiornare il Data Base NON in IMMISSIONE
061700160108     c                   exsr      AggDataBase
061800160108     C*
061900160108     C                   END
062000160108     C*******
062100160108     C*******  NON ha più senso l'F8 con la nuova struttura del giro dei dati
062200160108     C***  $TASTO        ifeq      F08
062300160108    >c*******            eval      xtaopz = AddAutista
062400160108     C*******            SETOFF                                       020304
062500160108     C*******            SETOFF                                       0506
062600160108     C*******            SETOFF                                       080910
062700160108     C*******            SETOFF                                       111213
062800090327     C*
062900160108     C*******            SETON                                        07
063000160108     c*******            eval      $GEST = 'D4'
063100160108     c*******            eval      $inzD4= *on
063200160108     C*******            END
063300160108     C**
063400090327     C                   END
063500090327     C*
063600090319     C                   ENDIF
063700090319     C*
063800090319     C                   ENDSR
063900090319      ************************************************************
064000090319      *?  INIZIALIZZAZIONE VIDEATA 2                       ?
064100090319      ************************************************************
064200090319     C     INZD2         BEGSR
064300090319     C*
064400090320     C                   z-add     H1recTRS      H2recTRS
064500090317     c                   eval      d2PGM  =  D1PGM
064600130527     C*
064700090410     C*  se in Immissione
064800090410     c                   if        D1nrc = 0 and xtaopz = Immissione
064900090410     c                   exsr      repNUM
065000090410     c                   z-add     NUMERO_nrc    D1nrc
065100090410     c                   z-add     NUMERO_nrc    h1nrc
065200090410     c                   z-add     D1nrc         D2nrc
065300090410     c                   z-add     h1nrc         h2nrc
065400090410     c                   end
065500090410     C*
065600090317     c                   eval      d2NRC  =  D1Nrc
065700090319     c                   eval      H2NRC  =  H1Nrc
065800090317     c                   eval      D2Ann  =  D1Ann
065900090317     c                   eval      D2Fun  =  D1Fun
066000090317     c                   eval      D2FIL  =  D1FIL
066100090317     c                   eval      D2DFIL =  D1DFIL
066200090317     c                   eval      D2SOCG =  D1SOCG
066300090317     c                   eval      D2DSCG =  D1DSoCG
066400090317     c                   eval      D2IVA  =  D1IVA
066500090408     C                   eval      d2ProJ = s1_RagSocKSC
066600090408     C                   eval      d2ProJ1= s1_RagSocKSC1
066700090409      *------
066800090409      *  NON in IMMISSIONE
0669000903191   >C     xtaopz        IFne      Immissione
067000120104     c                   setoff                                       19
067100120104     c                   eval      dtrsflr = trsflr
067200150305     c                   eval      dtrsflr1= trsflr1
067300150305      *
067400120104     c                   if         §trsris = 'R'
067500120104     c                   setoN                                        19
067600120104     c                   end
067700090417      *  imposta automaticamente sui contratti vecchi da modificare
0678000904171   >C                   IF        xtaopz = Modifica and
067900090417     c                             trsdec < 20090501
068000090417     c                   eval      trsdec = 20090501
068100090417     c                   endif
068200090417      *
068300090319     c                   if        trsdec > 0
068400090319     c                   move      TRsDec        dataiso
068500090319     c                   move      dataiso       dataeur
068600090319     c                   move      dataeur       d2DEC
068700090324     c                   move      TRsDec        h2DEC
068800090407     c                   move      TRsDec        h2savdec
068900090319     c                   endif
069000090319      *
069100090319     c                   if        trsdfc > 0
069200090319     c                   move      TRsDfc        dataiso
069300090319     c                   move      dataiso       dataeur
069400090319     c                   move      dataeur       d2DfC
069500090324     c                   move      TRsDfc        h2DfC
069600090319     c                   endif
069700090319
069800090319     c                   if        trsdrc > 0
069900090319     c                   move      TRsDrc        dataiso
070000090319     c                   move      dataiso       dataeur
070100090319     c                   move      dataeur       d2DrC
070200090324     c                   move      TRsDrc        h2DrC
070300090319     c                   endif
070400090319
070500090330     c                   setoff                                       15
070600090522     c                   if        trsdsc > 0
070700090330     c                   seton                                        15
070800090319     c                   move      TRsDsc        dataiso
070900090319     c                   move      dataiso       dataeur
071000090319     c                   move      dataeur       d2DsC
071100090324     c                   move      TRsDsc        h2DsC
071200120104     c  n19              if        §trsunodc = *zeros or
071300120104     c                             §trsunodc =  *blank
071400120104     c                   move      dataeur       d2unodc
071500120104     c                   endif
071600090319     c                   endif
071700111228     c                   if        §trsunodc <> *zeros and
071800111228     c                             §trsunodc <> *blank
071900111228     c                   move      §trsunodc     §trsunodc0        8 0
072000111228     c                   move      §trsunodc0    dataiso
072100111228     c                   move      dataiso       dataeur
072200111228     c                   move      dataeur       d2Unodc
072300111228     c                   move      §trsunodc     h2Unodc
072400111228     c                   endif
072500120103     c                   if        d2unodc = d2dsc
072600120103     c                   clear                   d2dsc
072700120103     c                   endif
072800150305      *
072900120217      *    riporta sulla DS i campi del filler
073000120217     c                   eval      dTRSflr = TRSflr
073100120217     c                   eval      hTRSflr = TRSflr
073200150305     c                   eval      dTRSflr1= TRSflr1
073300150305     c                   eval      hTRSflr1= TRSflr1
073400150305      **
073500150305      ** DATA    D.U.R.C.
073600150305     c                   if        §trsdDURC <> *zeros and
073700150305     c                             §trsdDURC <> *blank
073800150305     c                   move      §trsdDURC     §trsdDURC0        8 0
073900150305     c                   move      §trsdDURC0    dataiso
074000150305     c                   move      dataiso       dataeur
074100150305     c                   move      dataeur       d2dDURC
074200150305     c                   move      §trsdDURC     h2dDURC
074300150305     c                   endif
074400150305      **
074500150305      ** DATA    C.I.P.
074600150305     c                   if        §trsdCIP  <> *zeros and
074700150305     c                             §trsdCIP  <> *blank
074800150305     c                   move      §trsdCIP      §trsdCIP0         8 0
074900150305     c                   move      §trsdCIP0     dataiso
075000150305     c                   move      dataiso       dataeur
075100150305     c                   move      dataeur       d2dCIP
075200150305     c                   move      §trsdCIP      h2dCIP
075300150305     c                   endif
075400150305      ****
075500090319     c                   eval      d2CIT = TRSCIT
075600090319     c                   eval      d2NIS = TRSNIS
075700090319     c                   eval      d2PIA = TRSPIA
075800090319     c                   eval      d2NIA = TRSNIA
075900160204     c                   eval      d2cor = TRScor
076000090319
076100090319     c                   if        trsdia > 0
076200090319     c                   move      TRsDia        dataiso
076300090319     c                   move      dataiso       dataeur
076400090319     c                   move      dataeur       d2Dia
076500090319     c                   endif
076600090319     C*
076700090319      * ? * richiamo routine dei ctrl per decodifiche e controlli
076800090409     c                   eval      SOLO_decodifica ='S'
076900090319    >C                   EXSR      CTRD2
077000090409     c                   eval      SOLO_decodifica = *blank
077100090317     C                   END
077200090409     C*
077300090409     C*  comunque decodifica dal forntitore i campi:
077400090409     C*    Registro Imprese, Codice Fiscale, Provincia e Città e se
077500090409     C*    il Codice Fiscale è alfanumerico imposta la Ragione Sociale del
077600090409     C*    Fornitore nel Legale Rappresentante.
077700090409     c                   clear                   CaratteriAlfa     1
077800090409      *------
077900090409     c                   if        d2NIS = *blank
078000090409     c                   eval      d2NIS = S1_CodiceFisc
078100090409      *  controllo validità distinta
078200090417     c                   clear                   dove              3 0
078300090417     c     digits        check     d2NIS:1       dove
078400090417     c                   if        dove > 0
078500090409     c                   move      'S'           CaratteriAlfa     1
078600090409     c                   end
078700090409     c                   endif
078800090409      *
078900090409     c                   if        d2PIA = *blank
079000090409     c                   eval      d2PIA = S1_Provincia
079100090409     c                   endif
079200090409      *
079300090409     c                   if        d2CIT = *blank
079400090416     C*
079500090416     c                   z-add     1             tblkut
079600090416     c                   movel     'PR'          tblCod
079700090416     c                   movel(p)  d2PIA         tblkey
079800090416     c     Ktab          chain     tabel00f
079900090416     c                   if        %Found(Tabel00f)
080000090416     c                   eval      dspr = tbluni
080100090416     c                   eval      d2CIT = §PRDES
080200090416     c                   endif
080300090409     c                   endif
080400090409      *
080500090317     C                   ENDSR
080600090317     C************************************************************
080700090319      *?  CONTROLLO VIDEATA 2 CAMPI                        ?
080800090317     C************************************************************
080900090317     C     CTRD2         BEGSR
081000090317      *
081100090409     C                   SETOFF                                       9998
081200090608      *
081300090608      * Se è stata fatta la ristampa del contratto 19 = ON
081400090608     c                   setoff                                       19
081500090608     c                   eval      dtrsflr = trsflr
081600150305     c                   eval      dtrsflr1= trsflr1
081700150305      *
081800090608     c                   if         §trsris = 'R'
081900090608     c                   setoN                                        19
082000090608     c                   end
082100090608      *
082200090409      * Se richiamato solo come decodifica e NON X CONTROLLI
082300090409     c                   IF        SOLO_decodifica = 'S'
082400090409     c                   seton                                        98
082500090409     c                   end
082600090330      *
082700090330      ***  se Contratto già stampato protegge i campi sensibili da non
082800090330      **  modificare più.
082900090330     c                   setoff                                       15
083000090522     c                   if        trsdsc > 0
083100090330     c                   seton                                        15
083200090330     c                   end
083300090317      *
083400090317      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0835000903191   >C     xtaopz        IFNE      Cancella
083600090319      *
083700090319      * Data emissione Contratto
083800090323     c                   clear                   h2DEC
083900160105      ********
084000160105$017 C     d2dec         IFgt      *zeros
084100160105     C                   Z-ADD     d2dec         G02DAT
084200160105     C                   MOVEL     *BLANK        G02ERR
084300160105     C                   CALL      'XSRDA8'
084400160105     C                   PARM                    WLBDAT
084500160105     C                   Z-ADD     G02DAT        d2dec
084600160105      * gira la data in AMG
084700160105     C                   IF        G02err = '0'
084800160105     c                   z-add     g02INV        h2DeC
084900160105     C                   endIF
085000160105     c                   end
085100160105      ********
085200160105     c                   if        d2dec  <> *zeros and G02err <>'0'
085300090319     c                              or
085400090319     c                             d2dec  =  *zeros
085500090409     C  n98              SETON                                        5099
085600090409     c  n98              leavesr
085700090319     C                   ENDIF
085800090320      *
085900090407      *  in Modifica se la data è di un contratto convertito
086000090417      *    ossia < 20090420 si deve segnalare l'errore
086100090417     c                   if        xtaopz = Modifica and h2savDEc < 20090420
086200090417     c                             and h2dec <> 20090420
086300090409     C  n98              SETON                                        4899
086400090409     c  n98              leavesr
086500090407     C                   end
086600090407      *
086700090407     c                   if        xtaopz = Immissione and h2DEc < 20090501
086800090409     C  n98              SETON                                        4999
086900090409     c  n98              leavesr
087000090407     C                   end
087100090506      *
087200090506      * Non è possibile modificare la Data del contratto se c'è già almeno una
087300090506      *  Tariffa confermata (a parità di P.IVA)
087400090506     c                   if        xtaopz = Modifica and h2savDEc <> h2dec
087500090506      *
087600090506     c                   clear                   se_convalidata
087700090506     c                   exsr      se_TAR_ConV
087800090506      *
087900090506     c                   if        se_convalidata='S'
088000090506     C  n98              SETON                                        6199
088100090506     c  n98              leavesr
088200090506     C                   end
088300090506      *
088400090506     C                   end
088500090320      *
088600090319      * Data rientro copia firmata
088700090324      *  deve essere superiore alla data Emissione Contratto e alla Data di Stampa
088800090323     c                   clear                   h2DrC
088900160105      ********
089000160105$017 C     d2DrC         IFgt      *zeros
089100160105     C                   Z-ADD     d2DrC         G02DAT
089200160105     C                   MOVEL     *BLANK        G02ERR
089300160105     C                   CALL      'XSRDA8'
089400160105     C                   PARM                    WLBDAT
089500160105     C                   Z-ADD     G02DAT        d2DrC
089600160105      * gira la data in AMG
089700160105     C                   IF        G02err = '0'
089800160105     c                   z-add     g02INV        h2DrC
089900160105     C                   endIF
090000160105     c                   end
090100160105      ********
090200160105     c                   if        d2drc  <> *zeros and G02err <>'0'
090300090409     C  n98              SETON                                        5199
090400090409     c  n98              leavesr
090500090319     C                   ENDIF
090600090323      * gira la data in AMG
090700160105$017 C     h2drc         IFgt      *zeros
090800090324     c                   if        h2DrC < h2DeC and h2Dec >0 or
090900090324     c                             h2DrC < h2DsC and h2Dsc >0
091000090409     C  n98              SETON                                        5199
091100090409     c  n98              leavesr
091200090324     C                   end
091300090324     c                   if        h2DsC = 0
091400090409     C  n98              SETON                                        6099
091500090409     c  n98              leavesr
091600090324     C                   end
091700090323     C                   end
091800090506      *  Inoltre non deve essere superiore della UDATE
091900090506     c                   if        h2DrC > Data_Oggi
092000090506     C  n98              SETON                                        5799
092100090506     c  n98              leavesr
092200090506     C                   end
092300090506      *
092400090409      *   Iscrizione CCIAA  Registro delle Imprese
092500090319     c                   If        d2cit  =  *blank
092600090409     C  n98              SETON                                        5299
092700090409     c  n98              leavesr
092800090319     C                   ENDIF
092900090317      *
093000090319      *   Nr Iscrizione CCIAA
093100090319     c                   If        d2nis  =  *blank
093200090409     C  n98              SETON                                        5399
093300090409     c  n98              leavesr
093400090319     C                   ENDIF
093500090319      *
093600090319      *   Iscr.ALBO trasp.
093700090319     c                   If        d2pia  =  *blank
093800090409     C  n98              SETON                                        5499
093900090409     c  n98              leavesr
094000090319     C                   ENDIF
094100090319      *
094200090319      *   Nr Iscr.ALBO trasp.
094300090319     c                   If        d2nia  =  *blank
094400090409     C  n98              SETON                                        5599
094500090409     c  n98              leavesr
094600090319     C                   ENDIF
094700090319      *   Data Iscriz.Albo
094800090323     c                   clear                   h2Dia
094900160105      ********
095000160105$017 C     d2Dia         IFgt      *zeros
095100160105     C                   Z-ADD     d2Dia         G02DAT
095200160105     C                   MOVEL     *BLANK        G02ERR
095300160105     C                   CALL      'XSRDA8'
095400160105     C                   PARM                    WLBDAT
095500160105     C                   Z-ADD     G02DAT        d2Dia
095600160105      * gira la data in AMG
095700160105     C                   IF        G02err = '0'
095800160105     c                   z-add     g02INV        h2Dia
095900160105     C                   endIF
096000160105     c                   end
096100160105      ********
096200160105     c                   if        d2dia  <> *zeros and G02err <>'0'
096300090409     C  n98              SETON                                        5699
096400090409     c  n98              leavesr
096500090409     C                   ENDIF
096600090410      *
096700150305      *
096800150305      * Data documentazione DURC
096900150305     c                   clear                   h2Ddurc
097000160105      ********
097100160105$017 C     d2Ddurc       IFgt      *zeros
097200160105     C                   Z-ADD     d2Ddurc       G02DAT
097300160105     C                   MOVEL     *BLANK        G02ERR
097400160105     C                   CALL      'XSRDA8'
097500160105     C                   PARM                    WLBDAT
097600160105     C                   Z-ADD     G02DAT        d2Ddurc
097700160105      * gira la data in AMG
097800160105     C                   IF        G02err = '0'
097900160105     c                   z-add     g02INV        h2Ddurc
098000160105     C                   endIF
098100160105     c                   end
098200160105      ********
098300160105     c                   if        d2Ddurc<> *zeros and G02err <>'0'
098400150305     C  n98              SETON                                        5899
098500150305     c  n98              leavesr
098600150305     C                   ENDIF
098700160204      *
098800160204      * Sequenza delle Date DURC/CIP
098900160204     c                   if        h2Ddurc > Data_Oggi
099000160204     C  n98              SETON                                        7999
099100160204     c  n98              leavesr
099200160204     C                   ENDIF
099300160204      *
099400150305      *
099500160204      *data legge 190
099600150305     c                   clear                   h2Dcip
099700160105      ********
099800160105$017 C     d2Dcip        IFgt      *zeros
099900160105     C                   Z-ADD     d2Dcip        G02DAT
100000160105     C                   MOVEL     *BLANK        G02ERR
100100160105     C                   CALL      'XSRDA8'
100200160105     C                   PARM                    WLBDAT
100300160105     C                   Z-ADD     G02DAT        d2Dcip
100400160105      * gira la data in AMG
100500160105     C                   IF        G02err = '0'
100600160105     c                   z-add     g02INV        h2Dcip
100700160105     C                   endIF
100800160105     c                   end
100900160105      ********
101000160105     c                   if        d2Dcip <> *zeros and G02err <>'0'
101100160205     c                             or
101200160205    >C                             xtaopz = Immissione and
101300160205     c                             d2dcip = *zeros
101400150305     C  n98              SETON                                        5999
101500150305     c  n98              leavesr
101600150305     C                   ENDIF
101700160204      * S
101800160429     c                   if        h2Dcip  > Datapiu3m and
101900160429     c                             h2dcip <> 20391231
102000160204     C  n98              SETON                                        7899
102100160204     c  n98              leavesr
102200160204     C                   ENDIF
102300160204      *flag legge 190
102400160204     c                   clear                   dsrg
102500160204     c                   if        d2cor <> *blank
102600160204     c                   move      'SRG'         tbecod
102700160204     c                   movel     d2cor         tbeke1
102800160204     c                   clear                   tbeke2
102900160204     c     k_tbe         chain     tntbe01l
103000160204     c                   if        %found(tntbe01l)
103100160204     c                   movel     tbeuni        dsrg
103200160204     c                   movel     §SRGDES       d2cord
103300160204     c                   else
103400160204     C                   SETON                                        8099
103500160204     c                   leavesr
103600160204     c                   endif
103700160204     c                   else
103800160205    >C                   if        xtaopz = Immissione
103900160204     C                   SETON                                        8099
104000160204     c                   leavesr
104100160205     c                   endif
104200160204     C                   ENDIF
104300160105      **
104400160205     c                   if        d2Dcip <> *zeros and d2cor = *blank
104500160205     c                             or
104600160205    >C                             d2cor <> *blank and
104700160205     c                             d2dcip = *zeros
104800160205     C  n98              SETON                                        8199
104900160205     c  n98              leavesr
105000160205     C                   ENDIF
105100090327      *---------------
1052000903171-   C                   ENDIF
105300090506      *---------------
105400960305     C                   ENDSR
105500090319      ************************************************************
105600090320      *?  Gestione VIDEATA 3 CAMPI                        ?
105700090319      ************************************************************
105800090319     C     GESD3         BEGSR
105900090319     C*
106000090319     C* inizializzazione videata
106100090319     C* settaggi 3°video campi attivi o campi protetti
106200090319     C     $INZD3        IFEQ      *ON
106300090320     C                   EXSR      INZD2
106400090319     C                   EXSR      INZD3
106500090320      * stampa sotto la videata
106600090320     C                   write     D2
106700090319     C                   MOVE      *OFF          $INZD3
106800090319     C                   END
106900090319     C*
107000090319     c   99              eval      errore = *on
107100090319     c                   SETOFF                                         99
107200090319      *
107300090319      * e sopra esegue la window delle funzioni particolari
107400090319     C*               *----------------*
107500090319     C                   EXFMT     D3
107600090319     C*               *----------------*
107700090319     c                   eval      errore = *off
107800090319     C*
1079000903191    C                   SELECT
108000090319     C* F3=Fine
108100090319     C     $TASTO        WHENEQ    F03
108200090319     C                   EXSR      F03Fine
108300090319     c                   leaveSR
108400090319     C* F12=Ritorno
108500090319     C     $TASTO        WHENEQ    F12
108600090319     C                   EXSR      F12Ritorna
108700090319     c                   leaveSR
108800090319     C* F06=Ritorno
108900090319     C     $TASTO        WHENEQ    F06
109000090319      * Forzatura di controllo
109100090319     C                   if        *in10 = *off
109200090320     c                   eval      savin10 = *off
109300090319     c                   seton                                        10
109400090320     c                   elseif    *in10 = *on
109500090320     c                   eval      savin10 = *on
109600090320     c                   end
109700090319     C*
1098000903202-   C                   ENDSL
109900090319     C*
110000090319    >C                   EXSR      CTRD3
110100090319     C*
110200090319     C     *IN99         IFEQ      *OFF
110300090319     C     $TASTO        andeq     F06
110400090319     C     *IN10         andeq     *ON
110500090320     C     savIN10       andeq     *ON
110600090327     c                   exsr      AggDataBase
110700090319     C                   ENDIF
110800090319      *
110900090319     C                   ENDSR
111000090318     C/EJECT
111100090319      ************************************************************
111200090320      *?  INIZIALIZZAZIONE VIDEATA DATI 3                 ?
111300090319      ************************************************************
111400090319     C     INZD3         BEGSR
111500090319     C*
111600090320     C                   z-add     H1recTRS      H3recTRS
111700090319     c                   eval      D3Fun  =  D1Fun
111800090406     c                   clear                   d3data
111900090319      *
112000090319      * Cambio P.IVA / Società
112100090319     c                   eval      d3not1 = NOTA1
112200090319     c                   eval      d3not2 = NOTA2
112300090319      *
112400090320     c                   setoff                                       080910
112500090319     C                   Select
1126000903191   >C     xtaopz        whenEQ    CambiaSocieta
112700090319     c                   eval      d3cosa = cosa_Societa
112800090319     c                   eval      d3cod  = 'Societa'
112900090320     c                   eval      d3not3 = NOTA3
113000110505     c                   eval      d3des1 = Altra_Filiale
113100090319     c                   seton                                        08
113200090319      *
1133000903191   >C     xtaopz        whenEQ    CambiaPIva
113400090319     c                   eval      d3cosa = cosa_Piva
113500090319     c                   eval      d3cod  = ' P. IVA'
113600090320     c                   eval      d3not3 = NOTA3
113700090319     c                   seton                                        09
113800090319      *
1139000903191   >C     xtaopz        whenEQ    DataFineC
114000090319     c                   eval      d3cosa = cosa_DATAfine
114100090507     c                   eval      d3not2 = '        Società attuale. '
114200090319     C*
114300090319     C                   EndSL
114400090319     C*
114500090319     C                   ENDSR
114600090319     C/EJECT
114700090318     C************************************************************
114800090320      *?  CONTROLLO VIDEATA D3                            ?
114900090318     C************************************************************
115000090318     C     CTRD3         BEGSR
115100090318      *
115200090318     C                   SETOFF                                       99
115300090318     c                   clear                   d3des
115400090506      *
115500090506      *  Attenzione x Cambio P.IVA o x Fine Contratto
115600090506    >C     xtaopz        IfEQ      CambiaPIva
115700090506    >C     xtaopz        orEQ      DataFineC
115800090506      *
115900090506      *   se il contratto non è stato stampato --> Errore
116000090506      *  con Società/P.IVA
116100090506     c                   eval      TRSSOCG = d1socG
116200090506     c                   eval      TRSIVA  = d1Iva
116300090506     c                   eval      TRSDFC  = 0
116400090506     c     kTRS02        setll     aiTRS02l
116500090506     c     kTRS02        reade     aiTRS02l
116600090506     c                   dow       not %EoF(AITRS02l)
116700090506      * Se Contratto non ancora stampato Dare errore
116800091102     c                   if        trsann = *blank and trsDFC =0
116900091102     c                   if        trsDSC =0 or trsDRC =0
117000090506     C                   SETON                                        6799
117100090506     c                   leavesr
117200090506     C                   End
117300091102     C                   End
117400090506     c     kTRS02        reade     aiTRS02l
117500090506     C                   End
117600090506      *
117700090506      *   Sempre x Cambio P.IVA o x Fine Contratto
117800090506      *    dare errore se ci sono tariffe ancora non scadute
117900090506      *     (ovviamente sempre con P.IVA = a quella del contratto)
118000090506      *
118100090506     c                   clear                   se_attiva
118200090506     c                   exsr      se_TAR_ATTIVA
118300090506     c                   if        se_attiva = 'S'
118400090506     C                   SETON                                        6899
118500090506     c                   leavesr
118600090506     C                   End
118700090506      *
118800090506     C                   EndIF
118900090506      *
119000090507      *
119100090507      *  Se data Fine Contratto e c'è almeno un autista ancora accreditato
119200090507      *   deve segnalare l'errore.
119300090507    >C     xtaopz        ifEQ      DataFineC
119400090507      *
119500090507     c                   clear                   se_accreditato
119600090508     c                   clear                   ultimo_disaccr
119700090507     c                   exsr      se_AUT_ATTIVI
119800090507     c                   if        se_accreditato = 'S'
119900090507     C                   SETON                                        7099
120000090507     c                   leavesr
120100090507     C                   End
120200090508      *
120300090508      *   e se tutti disaccreditati, NON si può inserire una data fine
120400090508      *   contratto precedente all'ultimo disaccreditato deve essere x forza
120500090508      *   successiva o nella data stessa.
120600090508      *
120700090507     C                   EndIF
120800090318      *
120900090318      * Data Fine Contratto
121000160105     c                   clear                   h3Data
121100160105      ********
121200160105$017 C     d3Data        IFgt      *zeros
121300160105     C                   Z-ADD     d3Data        G02DAT
121400160105     C                   MOVEL     *BLANK        G02ERR
121500160105     C                   CALL      'XSRDA8'
121600160105     C                   PARM                    WLBDAT
121700160105     C                   Z-ADD     G02DAT        d3Data
121800160105      * gira la data in AMG
121900160105     C                   IF        G02err = '0'
122000160105     c                   z-add     g02INV        h3Data
122100160105     C                   endIF
122200160105     c                   end
122300160105      ********
122400160105     c                   if        d3data <> *zeros and G02err <>'0'
122500090318     c                              or
122600090318     c                             d3data =  *zeros
122700090318     C                   SETON                                        6099
122800090319     c                   leaveSR
122900090318     C                   ENDIF
123000090508      *
123100090508      * La data Fine Contratto non può essere precedente all'ultimo
123200090508      *   Autista disaccreditato
123300090508    >C     xtaopz        ifEQ      DataFineC
123400090508     c     h3data        andlt     Ultimo_disaccr
123500090508     C                   SETON                                        7199
123600090508     c                   leaveSR
123700090508     C                   end
123800090506      *
123900090506      *  Attenzione x Cambio P.IVA o x Fine Contratto
124000090506    >C     xtaopz        IfEQ      CambiaPIva
124100090506    >C     xtaopz        orEQ      DataFineC
124200090506      *
124300090506      *   E se tutti gli allegati tariffari non sono scaduti nello stesso giorno
124400090506      *    segnalare l'errore
124500090506      *
124600090506     c                   clear                   in_stesso_die
124700090506     c                   exsr      tutte_ASSIEME
124800090506     c                   if        in_stesso_die = 'N'
124900090506     C                   SETON                                        6999
125000090506     c                   leavesr
125100090506     C                   End
125200090506      *
1253000905061-   C                   ENDIF
125400090506      *
125500090318      *
125600090318     C* controllo codice vuoto  Società
125700090319    >C     xtaopz        IfEQ      CambiaSocieta
125800110505      *
125900090318$017 C     D3socg        IFEQ      *blank
126000090402     C                   SETON                                        6199
126100090319     c                   leaveSR
126200090318     C                   Else
126300090402      *
126400090402      *  Società non corretta
126500090402     c                   eval      wsocG = d3socG
126600090402     c                   exsr      decod_societa
126700090402     c                   if        prmRpyIdMsg < 0
126800090402     c                              or TIBSSOCO0.IDSOCIETA = *blank
126900090402     C                   SETON                                        6199
127000090402     c                   leavesr
127100090402     c                   end
127200110505      * = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
127300110505      *  DA QUI occorre definire la filiale come unità di ricerca ??????????????
127400110505      *   per la nuova società --> P.IVA coerente altrimenti deve dare errore.
127500110505      * = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
127600110505      *
127700090402      *
127800090402      *  Anagrafica PROJ non aperta
127900090402     c                   eval      trsSOCg = d3socg
128000090402     c                   eval      trsIVA  = d1iva
128100110505     c                   eval      trsFIL  = d3fil
128200110505      ******
128300090402     c                   exsr      Decod_RAG_IVA
128400090402      *
128500090408     c                   if        S1_Trovato = *Off or S1_Errore  <>'0'
128600090318     C                   SETON                                        6299
128700090319     c                   leaveSR
128800090318     c                   else
128900090408     c                   eval      d3des = s1_RagSocKSC
129000090402     c                   eval      TRSSOCG = d3socG
129100090402     c                   eval      TRSIVA  = d1Iva
129200090402     c                   eval      TRSDFC  = 0
129300090318     c                   end
129400090402      *
129500090318     C                   ENDIF
129600090402      *
129700090318     C                   ENDIF
129800090318      *
129900090318     C* controllo codice vuoto P.IVA
130000090319    >C     xtaopz        IfEQ      CambiaPIva
130100090318$017 C     D3IVA         IFEQ      *blank
130200090318     C                   SETON                                        6499
130300090319     c                   leaveSR
130400090318     C                   ELSE
130500090318     c                   eval      trsIVA  = d3IVA
130600090318     c                   eval      trsSocG = d1SocG
130700090318     c                   eval      trsFil  = d1Fil
130800090318     c                   exsr      Decod_RAG_IVA
130900090408     c                   if        S1_Trovato = *Off or S1_Errore  <>'0'
131000090318     C                   SETON                                        6499
131100090319     c                   leaveSR
131200090318     C                   Else
131300090318      * in immissione non ci deve essere la stessa P.IVA x la società Filiale
131400090408     C                   eval      d3des = s1_RagSocKSC
131500090408     C                   eval      d3des1= s1_RagSocKSC1
131600090402      *
131700090318     C                   End
1318000903181-   C                   END
1319000903181-   C                   ENDIF
132000090318     C                   ENDSR
132100090320     C/EJECT
132200090320     C************************************************************
132300090320      *?  GESTIONE  VIDEATA 4                              ?
132400090320     C************************************************************
132500090320     C     GESD4         BEGSR
132600090320     C*
132700090320     C* inizializzazione videata
132800090320     C* settaggi 3°video campi attivi o campi protetti
132900090320     C     $INZD4        IFEQ      *ON
133000090320     C                   EXSR      INZD4
133100090320     C                   MOVE      *OFF          $INZD4
133200090320     C                   END
133300090320     C*
133400090320     c   99              eval      errore = *on
133500090320     c                   SETOFF                                         99
133600090327     C*
133700090327     c                   if        xtaopz = ModAutista and
133800090327     c                             atarecord = 0
133900090327     C                   EVAL      $GEST ='D2'
134000090327     C                   EXSR      F12Ritorna
134100090327     c                   leavesr
134200090327     C                   END
134300091215      *
134400091215      *   visualizza e gestisce l'<A PERDERE>
134500120323     c                   setoFF                                       24
134600091215     c                   if        Attivato_a_Perdere = 'N'
134700091215     c                   setoN                                        24
134800091215     c                   end
134900090320      *
135000120323      *   visualizza e gestisce
135100120323      *     le date Apertura e chiusura del Contratto SCADUTO
135200120323     c                   setoFF                                       88
135300120323     c                   if        su_ContrSCADUT = 'S'
135400120323     c                   move      h2dfc         h4dfc
135500160107     c                   movel     d2dfc         d4dfc
135600160107     c                   move      d2dfc         sav2alf           2
135700160107     c                   move      sav2alf       d4DfC
135800120323     c                   setoN                                        88
135900120323     c                   end
136000120323      *
136100120323      *  Nel giro Particolare del'Accreditamento su Contratto SCADUTO
136200120323      *   in caso di Errore per riemetter la window (d4)
136300120323     c                   If        errore = *on
136400120323     c                   movea     *in           savin
136500120323     c                   move      *all'0'       *in
136600120323     c                   If              su_ContrSCADUT = 'S'
136700120323     c                   seton                                        2588
136800120323     c                   end
136900120323     C                   write     D4
137000120323     c                   movea     savin         *in
137100120323     c                   end
137200120323      *
137300090320      * e sopra esegue la window delle funzioni particolari
137400090320     C*               *----------------*
137500090320     C                   EXFMT     D4
137600090320     C*               *----------------*
137700090320     c                   eval      errore = *off
137800090320     C*
1379000903201    C                   SELECT
138000090320     C* F3=Fine
138100090320     C     $TASTO        WHENEQ    F03
138200090320     C                   EXSR      F03Fine
138300090320     c                   leaveSR
138400090406     C* F2=Tariffe
138500090406     C     $TASTO        WHENEQ    F02
138600090406     C                   EXSR      F02Tariffe
138700090406     c                   leaveSR
138800090406     C* F4=Ricerche
138900090406     C     $TASTO        WHENEQ    F04
139000090406     C                   EXSR      F04Ricerche
139100090409     c                   leaveSR
139200150710     C* F15=inserimento Anagr. Consorziate
139300150710     C     $TASTO        WHENEQ    F15
139400150710     C                   eval        F15ivac = D4ivac
139500150710     C                   EXSR      F15CoopCons
139600150624     c                   leaveSR
139700090320     C* F12=Ritorno
139800090320     C     $TASTO        WHENEQ    F12
139900160108    >C     xtaopz        IfEQ      Immissione
140000160108      *  Dati Societari
140100160108     C                   EVAL      $GEST ='D2'
140200160108     C                   EVAL      $INZD2= *OFF
140300160108     c                   else
140400090320     C                   EXSR      F12Ritorna
140500160108     c                   end
140600090320     c                   leaveSR
140700170324     C* F07=Forzatura
140800170324     C     $TASTO        WHENEQ    F07
140900170324     C                   EXSR      F07ForzaCfisc
141000090320     C*
1411000903202-   C                   ENDSL
141200090320     C*
141300090320    >C                   EXSR      CTRD4
141400090320      *
141500090320     C     *IN99         IFEQ      *OFF
141600090327     C     $TASTO        ifeq      F06
141700090327     C     $TASTO        oreq      F08
141800090323     C*
141900090323    >C     xtaopz        IfEQ      Immissione
142000090324    >C     xtaopz        orEQ      AddAutista
142100090327    >C     xtaopz        orEQ      ModAutista
142200090327     c                   exsr      AggDataBase
142300090327     c                   end
142400160108     C*
142500120323      ** se su contratto SCADUTO deve uscire forzatamente
142600120323     c                   if        su_ContrSCADUT = 'S'
142700120323     c                   eval       $FINE = 'S'
142800120323     c                   else
142900160108     C*
143000090327     C     $TASTO        ifeq      F06
143100090327     c                   eval      $GEST = 'D2'
143200090327     c                   else
143300090327     C                   eval      $INZD4 = *on
143400090327     c                   eval      $GEST = 'D4'
143500090323     c                   end
143600160108     C*
143700120323     c                   endIF
143800090320     C                   ENDIF
143900090327     C                   ENDIF
144000090320      *
144100090320     C                   ENDSR
144200090320     C/EJECT
144300090320     C************************************************************
144400090320      *?  IMPOSTA   VIDEATA 4                              ?
144500090320     C************************************************************
144600090320     C     INZD4         BEGSR
144700090320     C*
144800090320     C                   z-add     xtakey        d4NRC
144900090320     C                   z-add     xtakey        H4NRC
145000090320     c                   eval      d4NRC  =  D1Nrc
145100160111     c                   eval      d4dec  =  D2dec
145200090320     c                   eval      d4FUN  =  D1FUN
145300120323     c                   eval      d4ANN  =  D1ANN
145400090320     c                   eval      H4NRC  =  H1Nrc
145500090320     c                   clear                   d4dautC
145600090320     c                   clear                   d4dautAD
145700130527     c                   clear                   d4autCIT
145800130527     c                   clear                   d4autAFF
145900130527     c                   CLEAR                   SAVautCIT         7 0
146000130527     c                   CLEAR                   SAVautAFF         7 0
146100090327      *
146200090327     c                   setoff                                       0405
146300090327      * Solo se Aggiunta AUTISTA deve pulire i campi di immissione
146400090327    >C     xtaopz        ifeq      AddAutista
146500160108    >C     xtaopz        oreq      Immissione
146600090327     C*
146700090327     c                   clear                   d4RIS
146800170221     c                   clear                   d4RISd
146900090921     c                   clear                   d4aper
147000090408     c                   clear                   h4AUTC
147100090408     c                   clear                   d4AUTC
147200090408     c                   clear                   d4AUTC1
147300090327     c                   clear                   d4dAUTC
147400090408     c                   clear                   h4AUTAD
147500090408     c                   clear                   d4AUTAD
147600090408     c                   clear                   d4AUTAD1
147700090327     c                   clear                   d4dAUTAD
147800160107     c                   clear                   h4fila
147900130717     c                   clear                   d4dipa
148000170626     c                   clear                   d4atp
148100151223     c                   clear                   d4orel
148200151223     c                   clear                   d4dUnil
148300151223     c                   clear                   d4dTsPs
148400151223     c                   clear                   d4dTrPs
148500160105     c                   clear                   D4dtrps
148600160105     c                   clear                   D4dtsps
148700151223     c                   clear                   d4Liv
148800151223     c                   clear                   d4LivD
148900151223     c                   clear                   d4QuaPr
149000151223     c                   clear                   d4QuaPrD
149100151223     c                   clear                   d4CCNL
149200151223     c                   clear                   d4CCNLd
149300151223     c                   clear                   d4Cfisc
149400170324     c                   clear                   Forza_cFISC
149500170324     c                   clear                   d4MSG
149600160105     C*
149700160105     c                   clear                   h4dUnil
149800160105     c                   clear                   h4dTsPs
149900160105     c                   clear                   h4dTrPs
150000160105     c                   clear                   h4dtrps
150100160105     c                   clear                   h4dtsps
150200170307     c                   clear                   h4ris
150300160108     C*
150400160108     C* la Filiale in immissione coincide con quella di gestione
150500160108    >C     xtaopz        ifeq      Immissione
150600160108     c                   z-add     d2FIL         h4filA
150700160108     c                   end
150800090320     C*
150900090327     c                   else
151000090327      * Solo se non Aggiunta AUTISTA
151100090320      * ? *    si deve scegliere quale dei records dell'AITRA esaminare    *
151200090320      *                   quale record di AITRA00F ? ?
151300090320     c                   exsr      Quale_AITRA
151400090320     C                   IF        Trovato_AITRA ='S'
151500151223     C*
151600090320     c                   eval      d4RIS    = traRIS
151700170221     c                   if        d4ris <> *blank
151800170221     c                   move      'TPA'         tbecod
151900170221     c                   movel(p)  d4ris         tbeke1
152000170221     c                   clear                   tbeke2
152100170221     c     k_tbe         chain     tntbe01l
152200170221     c                   if        %found(tntbe01l)
152300170221     c                   movel     tbeuni        dtpa
152400170221     c                   movel     §tpaDES       d4risD
152500170221     c                   end
152600170221     c                   endif
152700170221     C*
152800091215     c                   if        Attivato_a_Perdere ='S'
152900090921     c                   eval      d4aper   = traCOR
153000091215     c                   end
153100170221     C*
153200090408     c                   movel     trakau        d4AUTC
153300090408     c                   move      trakau        d4AUTC1
153400090408     c                   move      trakau        h4AUTC
153500090408     c                   movel     traBPT        d4AUTAD
153600090408     c                   move      traBPT        d4AUTAD1
153700090408     c                   move      traBPT        h4AUTAD
153800151223     c                   z-add     traorel       d4orel
153900150309     c                   move      traitaest     d4ie
154000160114     c                   move      traitaest     H4ie
154100170626     c                   move      traatp        d4atp
154200130717     c                   move      tradipa       d4dipa
154300150623     c                   movel     trafiller     dtrafiller
154400150623     c                   movel     §traIVAC      d4ivac
154500151223      *  Data
154600160105     c                   if        traUNIL  > 0
154700160105     c                   move      traUNIL       dataiso
154800151223     c                   move      dataiso       dataeur
154900151223     c                   move      dataeur       d4dUnil
155000160105     c                   move      traUNIL       h4dUnil
155100151223     c                   end
155200160104      *
155300151223     c                   if        tradTsPs > 0
155400151223     c                   move      tradTsPs      dataiso
155500151223     c                   move      dataiso       dataeur
155600151223     c                   move      dataeur       d4dTsPs
155700151223     c                   move      tradTsPs      h4dTsPs
155800151223     c                   end
155900160104      *
156000160104     c                   if        tradTrPs > 0
156100160104     c                   move      tradTrPs      dataiso
156200160104     c                   move      dataiso       dataeur
156300160104     c                   move      dataeur       d4dTrPs
156400160104     c                   z-add     tradTrPs      h4dTrPs
156500160104     c                   end
156600160104      *
156700151223     c                   movel     traLIQ        d4Liv
156800151223     c                   movel     traCNL        d4CCNL
156900160104     c                   movel     traQUA        d4QuaPr
157000151223     c                   movel     tracF         d4Cfisc
157100151223     C*
157200090415      *  Data Accreditamento
157300090415     c                   if        tradin > 0
157400090415     c                   move      TRaDin        dataiso
157500090415     c                   move      dataiso       dataeur
157600090415     c                   move      dataeur       d4Din
157700090415     c                   move      TRaDin        h4Din
157800090415     c                   end
157900160108      *
158000160108      * se Autista NO macchina di Scorta e viceversa
158100160108     c                   move      traFIL        h4filA
158200160108     c                   move      trasco        h4sco
158300170307     c                   move      traris        h4ris
158400090415      *
158500090320     C*  Decod Aut Distribuzione
158600090409     c     d4autC        ifGT      0
158700090409     c                   seton                                        04
158800090409     c                   eval      h4protc ='1'
158900090409     c                   end
159000090320     C*  Decod Aut Affl./Defl.
159100090409     c     d4autAD       ifGT      0
159200090409     c                   seton                                        05
159300090409     c                   eval      h4protad='1'
159400090409     c                   end
159500090320      *
159600090320     c                   endIF
159700090320     C                   END
159800090320     C*
159900090320     C                   EXSR      CTRD4
160000150623     C*
160100090320     C                   ENDSR
160200090320     C/EJECT
160300090320     C************************************************************
160400090320      *?  CONTROLLO VIDEATA 4                              ?
160500090320     C************************************************************
160600090320     C     CTRD4         BEGSR
160700090320     C*
160800090409     c                   clear                   d4dautc
160900090409     c                   clear                   d4dautad
161000170301     c                   clear                   Cambio_codice     1
161100160105     C*
161200090320     C                   SETOFF                                       99
161300090410      * Data accreditamento
161400090410     c                   clear                   h4din
161500160105      ********
161600160105$017 C     d4din         IFgt      *zeros
161700160105     C                   Z-ADD     d4din         G02DAT
161800160105     C                   MOVEL     *BLANK        G02ERR
161900160105     C                   CALL      'XSRDA8'
162000160105     C                   PARM                    WLBDAT
162100160105     C                   Z-ADD     G02DAT        d4din
162200160105      * gira la data in AMG
162300160105     C                   IF        G02err = '0'
162400160105     c                   z-add     g02INV        h4din
162500160105     C                   endIF
162600160105     c                   end
162700160105      ********
162800160105     c                   if        d4din  <> *zeros and G02err <>'0'
162900090410     c                              or
163000090410     c                             d4din  =  *zeros
163100090410     C                   SETON                                        7599
163200090410     c                   leavesr
163300090410     C                   ENDIF
163400130717      *------
163500090410      * Non può essere inferiore alla data emissione del contratto
163600090526     c                   if        xtaopz = AddAutista
163700160108     c                              or
163800160108     c                             xtaopz = Immissione
163900090410$017 C     h4din         IFlt      h2dec
164000090417     C                   SETON                                        7599
164100090417     c                   leavesr
164200090410     C                   end
164300090526     C                   end
164400160105      *
164500090327     C* prende la filiale dei 2 codici
164600090320$017 C     D4autC        IFeq      *zeros
164700090320$017 C     D4autAD       andEQ     *zeros
164800090320     C                   SETON                                        555699
164900090320     c                   leavesr
1650000903201-   C                   END
165100090409     C*
165200090409     C* Imposta la Filiale come punto di riferimento
165300160107     c                   if        h4filA = 0
165400090409$017 C     D4autC        IFgt      *zeros
165500160107     c                   eval      h4filA = D4autC
1656000904091-   C                   Else
165700090409$017 C     D4autAD       IFgt      *zeros
165800160107     c                   eval      h4filA = D4autAD
1659000904091-   C                   END
1660000904091-   C                   END
1661000904091-   C                   END
166200090320     C*
166300090320     C* controllo cod AUT Distribuzione
166400090320$017 C     D4autC        IFgt      *zeros
166500090408$017 C     D4autC1       ORgt      *zeros
166600090327      *
166700090324      * se non è della filiale di gestione
166800160108     c                   if        d4autC <> h4filA
166900160108     c                             and d4autC > 0 and h4filA > 0
167000090331     C                   SETON                                        5799
167100090331     c                   leavesr
167200090331     C                   End
167300090410     c*
167400090410      *  Controlla che sia un codice Filiale in organigramma
167500090410     c     D4AUTc        chain     azorg01l
167600090410     c                   if        %Found(azorg01l) and orgfag='F'
167700090410     c                   else
167800090410     C                   SETON                                        5599
167900090410     c                   leavesr
168000090410     c                   end
168100090324      *
168200090320     c                   eval      tipo = 'A'
168300090408     c                   movel     D4autC        autista
168400090408     c                   move      D4autC1       autista
168500090320     c     kAUT          chain     fiapd01l
168600090320     c                   if        %Found(fiapd01l)
168700090408     c                   eval      d4dautc = apdrsf
168800090409     c                   eval      h4autc = apdpdr
168900090408     c                   if        APDATB <> *blank
169000090408     c                   move      ' ANNULLATO'  d4dautc
169100090409     C                   SETON                                        5599
169200090409     c                   leavesr
169300090408     C                   End
169400160108      **
169500160108      * Controlli di immissione sull'autista ed il Contratto:
169600160108      * Data emissione Contratto non può essere successiva
169700160108      *  alla data decorrenza dell'allegato tariffario
169800160108      *
169900160108      * Va bene se :
170000160108      *     se non ha trovato l'allegato tariffario oppure
170100160108      *     se trovato non è ancora stato stampato e ha data superiore o uguale
170200160108      *     a quella del contratto......
170300160108      * Altrimenti ERRORE !!!!
170400090409      **
170500090617      *  Prende dati dalla Tariffa se valida
170600090617     c                   eval      Data_confronto = H4din
170700090617     c                   eval      PIVA_confronto = d2iva
170800091130     c                   eval       SOC_confronto = d2socG
170900090617     c                   exsr      Chk_DEC_AUTC
171000160108     c* in Immissione
171100160108      * se Data Decorrenza Tariffa inferiore (al momento abbandonato caricam. Data_decorr)
171200160108     c                   if        Data_decorr > 0 and Data_decorr < h2dEc
171300160108     c                             and xtaopz = Immissione
171400160108     C                   SETON                                        6299
171500160108     c                   leavesr
1716001601081-   C                   END
171700090617      **
171800090409      **  Controllo incrociato con la PartitaIVA della società
171900090617     c                   if        cod__TGTCDF <> *blank and
172000090617     c                             cod__TGTSOC <> *blank
172100090617     c                   eval      RCOKSC     = cod__TGTCDF
172200090617     c                   eval      RCOSOCIETA = cod__TGTSOC
172300090617     c                   else
172400090409     c                   movel     apdcsf        RCOSOCIETA
172500090409     c                   move      *all'0'       RCOKSC
172600090409     c                   move      apdksc        RCOKSC
172700090617     c                   end
172800090617     C                   move      'F'           subnatura
172900090409      * - controllare la congruenza fra la PIVA immessa e quella desunta
173000090409      * dal fornitore di autofatturazione preso dal codice autista FIAPD00F.
173100090409     C     Krco98j       chain     anRCO98j
173200090409     c                   if        %Found(anRCO98j)
173300090409      * confronta la P.IVA del fornitore con quella immessa e
173400090409      * se diversa segnala l'errore
173500090417     c                   if        SOGPARTIVA <> d2IVA or
173600090417     c                             RCOsocieta <> d2socG
173700090409     C                   SETON                                        7299
173800090409     c                   leavesr
173900090409     c                   endif
174000090409     c                   endif
174100090409      **
174200090320     C                   Else
174300090320     C                   SETON                                        5599
174400090320     c                   leavesr
174500090320     C                   End
174600090320      *
174700090327      * Controlla se il codice Autista è già stato legato al contratto
174800090327     c                   if        not *in04
174900090327      *
175000090327      * Solo in aggiunta dell'altro autista su AITRA deve controllare che la
175100090327      *  filiale sia la stessa dell'altro codice precedentemente inserito
175200090327    >C     xtaopz        ifeq      ModAutista
175300090408     c     d4autC        andne     d4autAD
175400090508      * Aperta la possibilità di eliminare un codice messo precedentemente
175500090508     C*********          SETON                                        6799
175600090508     c*********          leavesr
175700090327     C                   End
175800090327      *
175900090423     c     h4autC        setll     aitra03l
176000090423     c     h4autC        reade     aitra03l
1761000904231-   C                   dow       not %Eof(aitra03l)
176200090327      * se accreditato
176300090506     c                   if        traDFI = 0 and h4autC = traKAU and traANN=' '
176301180213     c                             or traDFI >= h4din and h4autC = traKAU
176303180213     c                             and traann=' '
176400090327     C                   SETON                                        6599
176500090327     c                   leavesr
1766000903271-   C                   END
176700120323      ***
176800120323      *** oppure
176900120323      ***     stesso controllo su CONTRATTO SCADUTO in ACCREDITAMENTO
177000120323     c                   if        traDFI > 0 and h4autC = traKAU and traANN=' '
177100120327     c                             and tranrc = h4nrc
177200120323     c                                   and su_ContrSCADUT = 'S'
177300120323     C                   SETON                                        6599
177400120323     c                   leavesr
1775001203231-   C                   END
177600120323      ***
177700090423     c     h4autC        reade     aitra03l
1778000903271-   C                   END
177900090327      *
178000090327     C                   End
178100090327      *
1782000905081-   C                   ELSE
178300090508      * se azzerato
178400090508     c                   clear                   h4AUTC
178500090508      *
1786000905081-   C                   END
178700090320     C*
178800090522     C*  Controllo sulla data di accreditamento e la tariffa
178900090522     C*    NON si può accreditare in data successiva alla data inizio periodo
179000090522     C*    della tariffa.
179100090609     c                   if        h4autC >0  and
179200160108     c                             (xtaopz = AddAutista or xtaopz = ModAutista
179300160108     c                              or xtaopz = Immissione)
179400090522     c*
179500090522      * se Data Decorrenza Tariffa inferiore della data Accreditamento
179600090522      * oppure
179700090525      * se non trovata nessuna Tariffa nuova, occorre vedere la Data Scadenza
179800090525      * dell'ultima Tariffa precedente e l'accreditamento deve essere superiore.
179900090526      *
180000090525     c                   if        Data_decorr < h4dIN and Data_decorr > 0
180100090525     c                   move      Data_decorr   dataiso
180200090525     c                   move      dataiso       dataeur
180300090525     c                   move      dataeur       Data_decor_gma    8 0
180400090525     c                   eval      d4msg0 = ' '
180500090525     c                   eval      d4msg1 = 'Data Accreditamento successiva-
180600090525     c                              alla data Decorrenza'
180700090525     c                   eval      d4msg2 = 'della tariffa. Decorrenza dal: ' +
180800090525     c                             %editw(Data_decor_gma : '  /  /    ')
180900090525     c                   eval      d4msg3 = 'Non è possibile accreditare un -
181000090525     c                             Autista in una data'
181100090525     c                   eval      d4msg4 = 'successiva alla data Decorrenza-
181200090525     c                              della Tariffa !'
181300090525     c                   eval      d4msg5 = ' '
1814000905251-   C                   Exfmt     MsgWind
181500090525     C                   SETON                                        7699
181600090525     c                   leavesr
1817000905251-   C                   END
181800090522     C*
181900090525     c                   If        Data_scade >= h4dIN and Data_decorr = 0
182000090525     c                   move      Data_scade    dataiso
182100090525     c                   move      dataiso       dataeur
182200090525     c                   move      dataeur       Data_scade_gma    8 0
182300090610      *
182400090610     c                   if        Tar_stampata = 'S'
182500090610      *
182600090610     c                   eval      d4msg0 = ' '
182700090525     c                   eval      d4msg1 = 'L''Autista ha una tariffa con scad-
182800090525     c                             enza :' +
182900090525     c                             %editw(Data_scade_gma : '  /  /    ')
183000090525     c                   eval      d4msg2 = 'con un Fornitore diverso da quello-
183100090525     c                              su cui si vuole '
183200090525     c                   eval      d4msg3 = 'accreditare.'
183300090525     c                   eval      d4msg4 = 'La data Accreditamento deve essere-
183400090525     c                              successiva '
183500090525     c                   eval      d4msg5 = 'a tale scadenza.'
183600090610      *
183700090618     c                   elseif    Tar_stampata <>'S'
183800090610      *
183900090610     c                   eval      d4msg0 = ' '
184000090610     c                   eval      d4msg1 = 'L''Autista ha una tariffa con scad-
184100090610     c                             enza :' +
184200090610     c                             %editw(Data_scade_gma : '  /  /    ')
184300090610     c                   eval      d4msg2 = 'e NON è ancora stata convalidata.'
184400090610     c                   eval      d4msg3 = ' '
184500090610     c                   eval      d4msg4 = 'Convalidare la tariffa.'
184600090610     c                   eval      d4msg5 = ' '
184700090610      *
184800090610     c                   end
184900090610      *
1850000905251-   C                   Exfmt     MsgWind
185100090525     C                   SETON                                        7699
185200090525     c                   leavesr
1853000905251-   C                   END
185400090526      *
185500090526     c                   end
185600090522     C*
185700110506     C****
185800110506     C*  Se però NON ha trovato la tariffa perchè senza il fornitore in quanto NON
185900110506     C*    convalidata,  deve dare la segnalazione di errore poichè prima si deve
186000110506     C*    convalidare la tariffa e poi accreditare.
186100110509     C****     e NON è una macchina di scorta
186200110506      *
186300110506     c                   if        Tar_stampata = ' ' and Data_trovata = ' '
186400110506     c                                 and
186500110509     c                               h4dIN > 0
186600110509     c                                 and
186700160111     c                               h4sco = ' '
186800110510     c                                   and
186900110510     c                             Tariffa_trovata = 'S'
187000110506      *
187100110506     c                   eval      d4msg0 = ' '
187200110506     c                   eval      d4msg1 = ' '
187300110506     c                   eval      d4msg2 = 'Prima di poter Accreditare l''Auti-
187400110506     c                             sta,'
187500110506     c                   eval      d4msg3 = 'occorre Convalidare la tariffa.'
187600110506     c                   eval      d4msg4 = ' '
187700110506     c                   eval      d4msg5 = ' '
187800110506      *
1879001105061-   C                   Exfmt     MsgWind
188000110506     C                   SETON                                        7699
188100110506     c                   leavesr
1882001105061-   C                   END
188300110506     C****
188400130527     c                   clear                   d4autcit          7 0
188500130527     c                   movel     d4autC        d4autcit
188600130527     c                   move      d4autC1       d4autcit
188700130607     c                   move      d4autcit      aut_x_msg         7
188800130527      **
188900130523      * Aggiunto un ulteriore controllo sulla presenza della P.IVA dell'AUTISTA
189000130523      * Sulla Tabella "PIA" utilizzata per quei Fornitori a cui fanno capo più
189100130523      * Società, quindi + P.IVA
189200130527      **    (La emetto solo una volta durante il controllo)
189300130527     C*
189400130527     C*  Controllo in accreditamento e modifica
189500160108     c                   if        h4autC >0  and (xtaopz = AddAutista
189600160108     c                                        or  xtaopz = Immissione)
189700130527     c*
189800130527     c                   IF          d4autCIT <> SAVautCIT
189900130527     c                   eval           SAVautCIT = d4autCIT
190000170301     c                   eval        Cambio_codice= 'S'
190100170302     c                   clear                   Gia_visto         1
190200130527      *
190300130527     c                   exsr      chk_tab_PIA
190400130607      *
190500130607     c                   if          su_piu_societa = 'S' and *in99
190600130607     c                   leavesr
190700130607     c                   end
190800130607      *
190900130527     c                   endIF
191000130527      *
191100130527     c                   end
191200130523     C****
191300110506     C*
191400090320     C* controllo cod AUT Affluenze / Defluenze
191500090320$017 C     D4autAD       IFgt      *zeros
191600090408$017 C     D4autAD1      ORgt      *zeros
191700090327      *
191800090324      * se non è della filiale di gestione
191900160107     c                   if        d4autAD <> h4filA and h4filA > 0
192000090408     c                             and d4autAD <> *zeros
192100090331     C                   SETON                                        5899
192200090331     c                   leavesr
192300090331     C                   End
192400090410      *
192500090410     C*  SOLO PER AFFLUENZE/DEFLUENZE
192600090410     C*  il codice fornitore deve appartenere ad un terminal
192700090410     C                   clear                   FNLV55DS
192800090410     C                   eval      D55lin  = d4autAD
192900100420     C                   eval      D55drf  = data_oggi
193000090410     C                   call      'FNLV55R'
193100090410     C                   parm                    FNLV55DS
193200090410     C                   IF        d55tfp <> d4autAD and
193300090410     C                             d55tfa <> d4autAD
193400090410     C                   SETON                                        7499
193500090410     c                   leavesr
193600090410     c                   end
193700090410     c*
193800090410      *  Controlla che sia un codice Filiale in organigramma
193900090410     c     D4AUTad       chain     azorg01l
194000090410     c                   if        %Found(azorg01l) and orgfag='F'
194100090410     c                   else
194200090410     C                   SETON                                        5699
194300090410     c                   leavesr
194400090410     c                   end
194500090410      *
194600090320     c                   eval      tipo = DataFineC
194700090408     c                   movel     D4autAD       autista
194800090408     c                   move      D4autAD1      autista
194900090320     c     kAUT          chain     fiapd01l
195000090320     c                   if        %Found(fiapd01l)
195100090408     c                   eval      d4dautad= apdrsf
195200090409     c                   eval      h4autad = apdpdr
195300090408     c                   if        APDATB <> *blank
195400090408     c                   move      ' ANNULLATO'  d4dautad
195500090409     C                   SETON                                        5699
195600090409     c                   leavesr
195700090408     C                   End
195800160108      **
195900160108      * Controlli di immissione sull'autista ed il Contratto:
196000160108      * Data emissione Contratto non può essere successiva
196100160108      *  alla data decorrenza dell'allegato tariffario
196200160108      *
196300160108      * Va bene se :
196400160108      *     se non ha trovato l'allegato tariffario oppure
196500160108      *     se trovato non è ancora stato stampato e ha data superiore o uguale
196600160108      *     a quella del contratto......
196700160108      * Altrimenti ERRORE !!!!
196800090617     c                   eval      Data_confronto = H4din
196900090617     c                   eval      PIVA_confronto = d2iva
197000091130     c                   eval       SOC_confronto = d2socG
197100090617     c                   exsr      Chk_DEC_AUTAD
197200160108     c
197300160108     c* in Immissione
197400160108      * se Data Decorrenza Tariffa inferiore (al momento abbandonato caricam. Data_decorr)
197500160108     c                   if        Data_decorr > 0 and Data_decorr < h2dEc
197600160108     c                             and xtaopz = Immissione
197700160108     C                   SETON                                        6299
197800160108     c                   leavesr
1979001601081-   C                   END
198000160108      **
198100090617      **
198200090409      **  Controllo incrociato con la PartitaIVA della società
198300090617     c                   if        cod__TGTCDF <> *blank and
198400090617     c                             cod__TGTSOC <> *blank
198500090617     c                   eval      RCOKSC     = cod__TGTCDF
198600090617     c                   eval      RCOSOCIETA = cod__TGTSOC
198700090617     c                   else
198800090409     c                   movel     apdcsf        RCOSOCIETA
198900090409     c                   move      *all'0'       RCOKSC
199000090409     c                   move      apdksc        RCOKSC
199100090617     C                   End
199200090617     C                   move      'F'           subnatura
199300090409      * - controllare la congruenza fra la PIVA immessa e quella desunta
199400090409      * dal fornitore di autofatturazione preso dal codice autista FIAPD00F.
199500090409     C     Krco98j       chain     anRCO98j
199600090409     c                   if        %Found(anRCO98j)
199700090409      * confronta la P.IVA del fornitore con quella immessa e
199800090409      * se diversa segnala l'errore
199900090417     c                   if        SOGPARTIVA <> d2IVA or
200000090417     c                             RCOsocieta <> d2socG
200100090409     C                   SETON                                        7399
200200090409     c                   leavesr
200300090409     c                   endif
200400090409     c                   endif
200500090409      **
200600090320     C                   Else
200700090320     C                   SETON                                        5699
200800090320     c                   leavesr
200900090320     C                   End
201000090327      *
201100090327      * Controlla se il codice Autista è già stato legato al contratto
201200090327     c                   if        not *in05
201300090327      *
201400090327      * Solo in aggiunta dell'altro autista su AITRA deve controllare che la
201500090327      *  filiale sia la stessa dell'altro codice precedentemente inserito
201600090327    >C     xtaopz        ifeq      ModAutista
201700090408     c     d4autC        andne     d4autAD
201800090508      * Aperta la possibilità di eliminare un codice messo precedentemente
201900090508     C*********          SETON                                        6899
202000090508     c*********          leavesr
202100090327     C                   End
202200090327      *
202300090423     c     h4autad       setll     aitra05l
202400090423     c     h4autad       reade     aitra05l
2025000904231-   C                   dow       not %Eof(aitra05l)
202600090327      * se accreditato
202700090506     c                   if        traDFI = 0 and h4autAD= traBPT and traANN=' '
202701180213     c                             or traDFI >= h4din and h4autad = trabpt
202704180213     c                             and traann=' '
202800090327     C                   SETON                                        6699
202900090327     c                   leavesr
2030000903271-   C                   END
203100120323      ***
203200120323      *** oppure
203300120323      ***     stesso controllo su CONTRATTO SCADUTO in ACCREDITAMENTO
203400120323     c                   if        traDFI > 0 and h4autAD= traBPT and traANN=' '
203500120327     c                             and tranrc = h4nrc
203600120323     c                                   and su_ContrSCADUT = 'S'
203700120323     C                   SETON                                        6699
203800120323     c                   leavesr
2039001203231-   C                   END
204000120323      ***
204100090423     c     h4autad       reade     aitra05l
2042000903271-   C                   END
204300090320      *
204400090327     C                   End
204500090327      *
2046000905081-   C                   ELSE
204700090508      * se azzerato
204800090508     c                   clear                   h4AUTAD
204900090508      *
2050000903201-   C                   END
205100090522     C*
205200090522     C*  Controllo sulla data di accreditamento e la tariffa
205300090522     C*    NON si può accreditare in data successiva alla data inizio periodo
205400090522     C*    della tariffa.
205500090526     c                   if        h4autAD >0  and
205600160108     c                             (xtaopz = AddAutista or xtaopz = ModAutista
205700160108     c                             or xtaopz = Immissione)
205800090522      *
205900090522      * se Data Decorrenza Tariffa inferiore della data Accreditamento
206000090522      * oppure
206100090525      * se non trovata nessuna Tariffa nuova, occorre vedere la Data Scadenza
206200090525      * dell'ultima Tariffa precedente e l'accreditamento deve essere superiore.
206300090526      *
206400090525     c                   if        Data_decorr < h4dIN and Data_decorr > 0
206500090525     c                   move      Data_decorr   dataiso
206600090525     c                   move      dataiso       dataeur
206700090525     c                   move      dataeur       Data_decor_gma    8 0
206800090525     c                   eval      d4msg0 = ' '
206900090525     c                   eval      d4msg1 = 'Data Accreditamento successiva-
207000090525     c                              alla data Decorrenza'
207100090525     c                   eval      d4msg2 = 'della tariffa. Decorrenza dal: ' +
207200090525     c                             %editw(Data_decor_gma : '  /  /    ')
207300090525     c                   eval      d4msg3 = 'Non è possibile accreditare un -
207400090525     c                             Autista in una data'
207500090525     c                   eval      d4msg4 = 'successiva alla data Decorrenza-
207600090525     c                              della Tariffa !'
207700090525     c                   eval      d4msg5 = ' '
2078000905251-   C                   Exfmt     MsgWind
207900090525     C                   SETON                                        7699
208000090525     c                   leavesr
2081000905221-   C                   END
208200090610      *
208300090525     c                   if        Data_scade >= h4dIN and Data_decorr = 0
208400090525     c                   move      Data_scade    dataiso
208500090525     c                   move      dataiso       dataeur
208600090525     c                   move      dataeur       Data_scade_gma    8 0
208700090610      *
208800090610     c                   if        Tar_stampata = 'S'
208900090610      *
209000090525     c                   eval      d4msg0 = ' '
209100090525     c                   eval      d4msg1 = 'L''Autista ha una tariffa con scad-
209200090525     c                             enza :' +
209300090525     c                             %editw(Data_scade_gma : '  /  /    ')
209400090525     c                   eval      d4msg2 = 'con un Fornitore diverso da quello-
209500090525     c                              su cui si vuole '
209600090525     c                   eval      d4msg3 = 'accreditare.'
209700090525     c                   eval      d4msg4 = 'La data Accreditamento deve essere-
209800090525     c                              successiva '
209900090525     c                   eval      d4msg5 = 'a tale scadenza.'
210000090610     C*
210100090618     c                   elseif    Tar_stampata <>'S'
210200090610      *
210300090610     c                   eval      d4msg0 = ' '
210400090610     c                   eval      d4msg1 = 'L''Autista ha una tariffa con scad-
210500090610     c                             enza :' +
210600090610     c                             %editw(Data_scade_gma : '  /  /    ')
210700090610     c                   eval      d4msg2 = 'e NON è ancora stata convalidata.'
210800090610     c                   eval      d4msg3 = ' '
210900090610     c                   eval      d4msg4 = 'Convalidare la tariffa.'
211000090610     c                   eval      d4msg5 = ' '
211100090610      *
211200090610     c                   end
211300090610     C*
2114000905251-   C                   Exfmt     MsgWind
211500090525     C                   SETON                                        7699
211600090525     c                   leavesr
2117000905251-   C                   END
211800110506     C****
211900110506     C*  Se però NON ha trovato la tariffa perchè senza il fornitore in quanto NON
212000110506     C*    convalidata,  deve dare la segnalazione di errore poichè prima si deve
212100110506     C*    convalidare la tariffa e poi accreditare.
212200110510     C****     e NON è una macchina di scorta
212300110506      *
212400110506     c                   if        Tar_stampata = ' ' and Data_trovata = ' '
212500110506     c                                 and
212600110506     c                               h4dIN > 0
212700110510     c                                 and
212800160111     c                               h4sco = ' '
212900110510     c                                    and
213000110510     c                             Tariffa_trovata = 'S'
213100110506      *
213200110506     c                   eval      d4msg0 = ' '
213300110506     c                   eval      d4msg1 = ' '
213400110506     c                   eval      d4msg2 = 'Prima di poter Accreditare l''Auti-
213500110506     c                             sta,'
213600110506     c                   eval      d4msg3 = 'occorre Convalidare la tariffa.'
213700110506     c                   eval      d4msg4 = ' '
213800110506     c                   eval      d4msg5 = ' '
213900110506      *
2140001105061-   C                   Exfmt     MsgWind
214100110506     C                   SETON                                        7699
214200110506     c                   leavesr
2143001105061-   C                   END
214400130527     C****
214500130527     c                   clear                   d4autaff          7 0
214600130527     c                   movel     d4autAD       d4autaff
214700130527     c                   move      d4autAD1      d4autaff
214800130607     c                   move      d4autaff      aut_x_msg         7
214900130527      **
215000130527      * Aggiunto un ulteriore controllo sulla presenza della P.IVA dell'AUTISTA
215100130527      * Sulla Tabella "PIA" utilizzata per quei Fornitori a cui fanno capo più
215200130527      * Società, quindi + P.IVA
215300130527      **
215400130527     C*  Controllo in accreditamento e modifica
215500160108     c                   if        h4autAD >0  and (xtaopz = AddAutista
215600160108     c                                         or  xtaopz = Immissione)
215700130527     c*
215800130527     c                   IF          d4autAFF <> SAVautAFF
215900130527     c                   eval           SAVautAFF = d4autAFF
216000170301     c                   eval        Cambio_codice= 'S'
216100170302     c                   clear                   Gia_visto         1
216200130527      *
216300130527     c                   exsr      chk_tab_PIA
216400130607      *
216500130607     c                   if          su_piu_societa = 'S' and *in99
216600130607     c                   leavesr
216700130607     c                   end
216800130607      *
216900130527     c                   endIF
217000130527      *
217100130527     c                   end
217200110506     C****
217300090526     c                   end
217400160114      *------
217500170221      * Ricerca  Senza Automezzo (ex Aut di Riserva)
217600170221     C****
217700170221     c                   clear                   d4risD
217800170221     c     '?'           scan      d4ris
217900170221     c                   if        %found
218000170221     c                   clear                   d4ris
218100170221     c                   clear                   tibs02ds
218200170221     C                   MOVEL     'R'           t02mod
218300170221     C                   MOVEL     knsif         t02sif
218400170221     c                   MOVEL     'TPA'         t02cod
218500170221     C                   CALL      'TIBS02R'
218600170221     C                   PARM                    KPJBA
218700170221     C                   PARM                    TIBS02DS
218800170221     C                   IF        T02err = *BLANKS
218900170221     C                   movel     T02uni        dTPA
219000170221     C                   movel     §tpaDES       d4risD
219100170221     C                   movel     T02ke1        d4ris
219200170221     C                   ENDIF
219300170221     C                   ENDIF
219400170221      *------
219500170221      * Ricerca
219600160114     c                   clear                   d4ccnlD
219700160114     c     '?'           scan      d4ccnl
219800160114     c                   if        %found
219900160114     c                   clear                   d4ccnl
220000160114     c                   clear                   tibs02ds
220100160114     C                   MOVEL     'R'           t02mod
220200160114     C                   MOVEL     knsif         t02sif
220300160114     c                   MOVEL     'CNL'         t02cod
220400160114     C                   CALL      'TIBS02R'
220500160114     C                   PARM                    KPJBA
220600160114     C                   PARM                    TIBS02DS
220700160114     C                   IF        T02err = *BLANKS
220800160114     C                   movel     T02uni        d4ccnlD
220900160114     C                   movel     T02ke1        d4ccnl
221000160114     C                   ENDIF
221100160114     C                   ENDIF
221200160114      *------
221300160114     c                   clear                   d4quaprD
221400160114     c     '?'           scan      d4quapr
221500160114     c                   if        %found
221600160114     c                   clear                   d4quapr
221700160114     c                   clear                   tibs02ds
221800160114     C                   MOVEL     'R'           t02mod
221900160114     C                   MOVEL     knsif         t02sif
222000160114     c                   MOVEL     'QUA'         t02cod
222100160114     C                   CALL      'TIBS02R'
222200160114     C                   PARM                    KPJBA
222300160114     C                   PARM                    TIBS02DS
222400160114     C                   IF        T02err = *BLANKS
222500160114     C                   movel     T02uni        d4quaprD
222600160114     C                   movel     T02ke1        d4quapr
222700160114     C                   ENDIF
222800160114     C                   ENDIF
222900160114      *------
223000160114     c                   clear                   d4livD
223100160114     c     '?'           scan      d4liv
223200160114     c                   if        %found
223300160114     c                   clear                   d4liv
223400160114     c                   clear                   tibs02ds
223500160114     C                   MOVEL     'R'           t02mod
223600160114     C                   MOVEL     knsif         t02sif
223700160114     c                   MOVEL     'LIQ'         t02cod
223800160114     C                   CALL      'TIBS02R'
223900160114     C                   PARM                    KPJBA
224000160114     C                   PARM                    TIBS02DS
224100160114     C                   IF        T02err = *BLANKS
224200160114     C                   movel     T02uni        d4livD
224300160114     C                   movel     T02ke1        d4liv
224400160114     C                   ENDIF
224500160114     C                   ENDIF
224600160114      *------
224700160105      *  Se Italia o Estero
224800160114     c                   if        d4ie =*blank and
224900160114     c                             (  xtaopz = Immissione
225000160114     c                             or xtaopz = AddAutista )
225100170904      **** Richiesta di Taglialatela (4/9/2017)
225200170904      *** se INTERINALE non deve controllare
225300170904      **** basta solo il codice fiscale poichè altri dati non sono al momento
225400170904      *** dell'inserimento disponibili. Quindi deve evitare i controlli per
225500170904      **** un autista Dipendente.
225600170904     c                   if        d4dipa <> 'I' or  d4ie <> *blank
225700160105     C                   SETON                                        6399
225800160105     c                   leavesr
225900170904     c                   end
226000160105     C                   ENDIF
226100160105      **
226200160105      *- Se Dipendente le ORE
226300160105     c                   if        d4dipa = 'D' and d4orel = 0  or
226400170904      *** se INTERINALE non deve controllare
226500170904     c*****                             d4dipa = 'I' and d4orel = 0  or
226600160108     c                             d4dipa = 'A' and d4orel > 0  or
226700160108     c                             d4dipa = *blank
226800160118     c                   if        xtaopz <> ModAutista
226900160105     C                   SETON                                        7799
227000160105     c                   leavesr
227100160118     C                   ENdIF
227200160111     C                   END
227300160111      **
227400160111      *-  Codice FISCALE corretto  ma NON OBBLIGATORIO
227500160111     c                   if          d4Cfisc <> *blank
227600170324     c******                       or xtaopz = Immissione
227700170324     c******                       or xtaopz = AddAutista
227800170324      **  pulisce la forzatura
227900170324     c                   clear                   Forza_cFISC
228000170324     c                   clear                   d4MSG
228100170324     c                   setoff                                       92
228200160111      **
228300160111      *  richiama il nuovo driver fatto per controllare il codice
228400160111      * fiscale o la partita iva
228500160111     c                   clear                   xcfiva1ds
228600160111     c                   eval      xcfivapar =  d4Cfisc
228700160111     c                   call      'XCFIVAR1'
228800160111     c                   parm                    xcfiva1ds
228900170324      ** SOLO SE IL CODICE È PIENO E NON CORRETTO DA ERRORE - VUOTO SALTA
229000160111      * --> errore
229100160111     c                   if        XCFIVAFLG < *zeros
229200160111     C                   SETON                                        8199
229300160111     c                   leavesr
229400160111     c                   End
229500180214      **in immissione controlla se esiste già nella stessa filiale il codice fiscale
229503180214     c                   if        xtaopz = AddAutista
229504180213     c                   clear                   esiste
229505180213     C/EXEC SQL
229506180213     C+ SELECT count(*) INTO :esiste FROM aitra01l
229507180213     C+ WHERE  tradfi >= :h4din  AND tracf = :d4cfisc and
229508180213     C+ trafil = :h4filA
229509180213     C+ or tradfi = 0  AND tracf = :d4cfisc and
229510180213     C+ trafil = :h4filA
229511180213     C/END-EXEC
229512180213     c                   if        esiste > 0
229513180213     c                   seton                                        9299
229514180213     c                   eval      D4msg ='C.F. già presente con altro codice -
229515180213     c                             in questa filiale'
229516180213     c                   leavesr
229517180213     c                   endif
229518180214     c                   endif
229519180213      *
229600170324     c                   ElsE
229700170324      **
229800170324      **  SE è lasciato vuoto appositamente,
229900170324      **    occorre forzare per andare avanti
230000170324     c                   if          Forza_cFISC = *blank
230100170327     c                               and $INZD4 = *OFF
230200170324     c                   eval      D4msg ='F7=Forza Cod.Fiscale vuoto'
230300170324     C                   SETON                                        9299
230400170324     c                   leavesr
230500170324     c                   End
230600170324      **
230700160111     c                   EndIF
230800160111      **
230900150624      *---------------
231000160111      *----- I NUOVI CAMPI devono essere controllati solo in IMMISSIONE
231100160111      *---------------  perchè in MANUTENZIONE dei vecchi records potrei
231200160111      *---------------  non avere ancora i dati sotto mano da inserire.
231300170904      *** .... e se INTERINALE non deve controllare
231400170302     c                   clear                   d4risD
231500170302      *
231600170302     c                   if           d4ris  = *blank
231700170302     c                                  or   sav4ris <> d4ris
231800170302     c                   eval          sav4ris = d4ris
231900170302     c                   clear                   gia_visto
232000170302     c                   End
232100170302     C*                             =====================
232200170302     c                   if           d4ris <> *blank
232300160108     C*                             =====================
232400170302     c                   IF          xtaopz = Immissione
232500170302     c                             or xtaopz = AddAutista
232600170302     c                             or xtaopz = ModAutista
232700170221      *-  Senza Automezzo
232800170221     C*
232900170221     c                   movel     'TPA'         tbeCOD
233000170221     c                   movel(p)  d4ris         tbeKE1
233100170221     c                   clear                   tbeKE2
233200170221     c     K_TBE         chain     tnTBE01L
233300170221     c                   if        %found(tnTBE01L)
233400170221     c                   movel     tbeuni        dTPA
233500170221     c                   movel     §tpaDES       d4risD
233600170301      **
233700170302      *? --------------------------                                                       ?
233800170301      **  Il test lo deve fare solo a cambio codice CITTA o AFFDEF
233900170307     c                   if          $INZD4 = *OFF  and h4RIS <> d4ris
234000170307      **
234100170302     c                   if          Cambio_codice ='S' or gia_visto <>'S'
234200170302     c                   eval            gia_visto ='S'
234300170301      **
234400170302      *? se ci sono Delle Valorizzazioni
234500170302     c                   z-add     0             totale           15 0
234600170302     C/EXEC SQL
234700170302     C+ SELECT count(*) INTO :totale FROM Fiftt00F
234800170302     C+ WHERE  fttDDC >= :h4din   AND
234900170302     C+     fttpdr= :h4autC
235000170302     C/END-EXEC
235100170302      *
235200170302      *  Esce segnalando l'errore:
235300170302     c                   if        totale > 0
235400170302      *   Solo se torna con "S" dopo il check sulla tabella emette una finestra
235500170302      *    di avviso
235600170302     c                   eval      d4msg0 = %editc(h4autC:'X') + ' ' + d4dautc
235700170302     c                   eval      d4msg1 = d4Ris + ' ' + d4risD
235800170302     c                   eval      d4msg2 = *blank
235900170302     c                   eval      d4msg3 = ' Attenzione: l''AUTISTA ha delle-
236000170307     c                              VALORIZZAZIONI attive!'
236100170307     c                   eval      d4msg4 = *blank
236200170302     c                   eval      d4msg5 = *blank
2363001703021-   C                   Exfmt     MsgWind
236400170302     c                   end
236500170302      *
236600170302     c                   end
236700170307     c                   END
236800170302      *? --------------------------                                                       ?
236900170301      **
237000170221     c                   else
237100170302     c                   clear                   Gia_visto         1
237200170221     C                   SETON                                        6999
237300170221     c                   leavesr
237400170302     c                   end
237500170302     c                   endif
237600170221      **
237700170221     c                   end
237800170221      **
237900160119     c                   IF          xtaopz = Immissione  and d4dipa = 'D'
238000160119     c                             or xtaopz = AddAutista and d4dipa = 'D'
238100170904      *** se INTERINALE non deve controllare
238200170904     c****                             or xtaopz = Immissione  and d4dipa = 'I'
238300170904     c****                             or xtaopz = AddAutista and d4dipa = 'I'
238400160114     c                             or d4ccnl <> *blank
238500160111      *-  Codice CCNL
238600160105     c                   clear                   d4ccnld
238700160107     C*
238800160107     c                   movel     'CNL'         tbeCOD
238900160107     c                   movel(p)  d4ccnl        tbeKE1
239000160107     c                   clear                   tbeKE2
239100160107     c     K_TBE         chain     tnTBE01L
239200160107     c                   if        %found(tnTBE01L)
239300160107     c                   movel     tbeuni        d4ccnld
239400160107     c                   else
239500160107     C                   SETON                                        8799
239600160107     c                   leavesr
239700160107     c                   end
239800160105      **
239900160114     c                   end
240000160114      **
240100160114     C*                             =====================
240200160119     c                   IF          xtaopz = Immissione  and d4dipa = 'D'
240300160119     c                             or xtaopz = AddAutista and d4dipa = 'D'
240400170904      *** se INTERINALE non deve controllare
240500170904     c****                             or xtaopz = Immissione  and d4dipa = 'I'
240600170904     c****                             or xtaopz = AddAutista and d4dipa = 'I'
240700160114     c                             or d4quapr<> *blank
240800160105      *-  Codice Qual.professionale
240900160105     c                   clear                   d4quaprd
241000160107     C*
241100160107     c                   movel     'QUA'         tbeCOD
241200160107     c                   movel(p)  d4quapr       tbeKE1
241300160107     c                   clear                   tbeKE2
241400160107     c     K_TBE         chain     tnTBE01L
241500160107     c                   if        %found(tnTBE01L)
241600160107     c                   movel     tbeuni        d4quaprd
241700160107     c                   else
241800160107     C                   SETON                                        8999
241900160107     c                   leavesr
242000160107     c                   end
242100160105      **
242200160114     c                   end
242300160114     C*                             =====================
242400160119     c                   IF          xtaopz = Immissione  and d4dipa = 'D'
242500160119     c                             or xtaopz = AddAutista and d4dipa = 'D'
242600170904      *** se INTERINALE non deve controllare
242700170904     c****                             or xtaopz = Immissione  and d4dipa = 'I'
242800170904     c****                             or xtaopz = AddAutista and d4dipa = 'I'
242900160114     c                             or d4liv  <> *blank
243000160114      **
243100160105      *-  Codice Liv. Contrattuale
243200160105     c                   clear                   d4livd
243300160105      **
243400160107     c                   movel     'LIQ'         tbeCOD
243500160107     c                   movel(p)  d4liv         tbeKE1
243600160107     c                   clear                   tbeKE2
243700160107     c     K_TBE         chain     tnTBE01L
243800160107     c                   if        %found(tnTBE01L)
243900160107     c                   movel     tbeuni        d4livd
244000160107     c                   else
244100160107     C                   SETON                                        9099
244200160107     c                   leavesr
244300160107     c                   end
244400160114      *------
244500160111     C                   ENDIF
244600160111      *-               ==========
244700160107      **
244800160105      *-  Data UNILAV
244900160105     c                   clear                   h4dUNIL
245000160105      **
245100160119$017 C                   IF        d4dUNIL >0
245200160105     C                   Z-ADD     d4dUNIL       G02DAT
245300160105     C                   MOVEL     *BLANK        G02ERR
245400160105     C                   CALL      'XSRDA8'
245500160105     C                   PARM                    WLBDAT
245600160105     C                   Z-ADD     G02DAT        d4dUNIL
245700160105      * gira la data in AMG
245800160105     C                   IF        G02err = '0'
245900160105     c                   z-add     g02INV        h4dUNIL
246000160105     C                   endIF
246100160105     c                   end
246200160111      **
246300160111      * Non può essere inferiore alla data emissione del contratto
246400160105      **
246500160105     c                   if        d4dUNIL <> *zeros and G02err <> '0'
246600160105     c                              or
246700160111     c                             d4dUNIL  = *zeros and xtaopz = Immissione
246800160119     c                                               and d4dipa = 'D'
246900160114     c                              or
247000160114     c                             d4dUNIL  = *zeros and xtaopz = AddAutista
247100160119     c                                               and d4dipa = 'D'
247200170904      *** se INTERINALE non deve controllare
247300170904     c****                              or
247400170904     c****                             d4dUNIL  = *zeros and xtaopz = Immissione
247500170904     c****                                               and d4dipa = 'I'
247600170904     c****                              or
247700170904     c****                             d4dUNIL  = *zeros and xtaopz = AddAutista
247800170904     c****                                               and d4dipa = 'I'
247900160111     c                              or
248000160111$017 C                             h4dUNIL < h2dec and h4dUNIL > 0
248100160105     C                   SETON                                        7899
248200160105     c                   leavesr
248300160105     C                   ENDIF
248400160105      *------
248500160111     c                   clear                   h4dtsps
248600160111     c                   clear                   h4dtrps
248700160114      **
248800160114      **  in Immissione se Autista "E" Estero occorre inserire la data del permesso
248900160114      **  di soggiorno o del rinnovo
249000160114     c                   if        d4dtsps  = *zeros and d4dtrps  = *zeros
249100160114     c                             and d4ie ='E'
249200160114     c                   IF          xtaopz = Immissione
249300160114     c                             or xtaopz = AddAutista
249400160114     c                             or h4ie <> d4ie
249500160114     C                   SETON                                        91  99
249600160114     c                   leavesr
249700160114     C                   end
249800160114     C                   end
249900160111      *------
250000160105      *-  Data Permesso di Soggiorno
250100160105$017 C     d4dtsps       IFgt      *zeros
250200160105     C                   Z-ADD     d4dtsps       G02DAT
250300160105     C                   MOVEL     *BLANK        G02ERR
250400160105     C                   CALL      'XSRDA8'
250500160105     C                   PARM                    WLBDAT
250600160105     C                   Z-ADD     G02DAT        d4dtsps
250700160105      * gira la data in AMG
250800160105     C                   IF        G02err = '0'
250900160105     c                   z-add     g02INV        h4dtsps
251000160105     C                   endIF
251100160105     c                   end
251200160105      **
251300160105     c                   if        d4dtsps <> *zeros and G02err <> '0'
251400160111     c                              or
251500160120      **** Richiesta di Taglialatela poichè è facile che sia antecedente al contratto
251600160120$017 C**************               h4dTsPs < h2dec and h4dTsPs >0
251700160120     c**************                or
251800160128$017 C                             h4dTsPs > data_5annIPiu and h4dTsPs > 0
251900160118     c                             and h4dTsPs <> 99991231
252000160105     C                   SETON                                        7999
252100160105     c                   leavesr
252200160105     C                   ENDIF
252300160105      *------
252400160105      *-  Data Rinnovo Permesso di Soggiorno
252500160105$017 C     d4dtrps       IFgt      *zeros
252600160105     C                   Z-ADD     d4dtrps       G02DAT
252700160105     C                   MOVEL     *BLANK        G02ERR
252800160105     C                   CALL      'XSRDA8'
252900160105     C                   PARM                    WLBDAT
253000160105     C                   Z-ADD     G02DAT        d4dtrps
253100160105      * gira la data in AMG
253200160105     C                   IF        G02err = '0'
253300160105     c                   z-add     g02INV        h4dtrps
253400160105     C                   endIF
253500160105     c                   end
253600160105      **
253700160105     c                   if        d4dtrps <> *zeros and G02err <> '0'
253800160105     c                              or
253900160120      **** Richiesta di Taglialatela poichè è facile che sia antecedente al contratto
254000160120$017 C************                 h4dTrPs < h2dec and h4dTrPs >0
254100160120     c************                  or
254200160128$017 C                             h4dTrPs > data_5annIpiu and h4dTrPs > 0
254300160118     c                             and h4dTrPs <> 99991231
254400160107     C                   SETON                                        8099
254500160105     c                   leavesr
254600160105     C                   ENDIF
254700160111      **
254800160105      *---------------
254900150624      *  P.IVA Coop Consorziate
255000150624     c     '?'           scan      d4IVAc
255100150624     c                   if        %Found
255200150624     c                   movel     kpjbu         kpjbus
255300150624     c                   movel     '?'           S7_ds7OPZ
255400150624     c                   movel     D2SOCG        S7_ds7socg
255500150624     c                   movel     D2IVA         S7_ds7iva
255600150624     c                   movel     trmz70s7ds    kpjbu
255700150624     c                   call      'TRMZ70SR7'
255800150624     c                   parm                    kpjba
255900150624     c                   eval      trmz70s7ds = kpjbu
256000150624     c                   eval      kpjbu = kpjbus
256100150624     c                   eval      d4IVAc = S7_ds7IVAA
256200150624     c                   end
256300150624      *
256400150624      *  Controlli sulla congruenza del dato e se la società è un consorzio
256500150831     c                   if        consorzio = 'S'
256600150624      *  se non c'è la PIVA coop
256700150624     c                   if          d4ivaC = *blank
256800150624     C                   SETON                                        8299
256900150624     c                   leavesr
257000150624     c                   else
257100150624      **
257200150624      ** Controllo Formale P.IVA
257300150624      *  richiama il nuovo driver fatto per controllare il codice
257400150624      * fiscale o la partita iva
257500151117     c                   clear                   xcfiva1ds
257600150624     c                   eval      xcfivapar = d4ivac
257700151117     c                   call      'XCFIVAR1'
257800151117     c                   parm                    xcfiva1ds
257900150624      * --> errore
258000150624     c                   if        XCFIVAFLG < *zeros
258100160107     C                   SETON                                        8499
258200150624     c                   leavesr
258300150624     c                   End
258400150624      **
258500150624      **  Deve controllare se esiste l'anagrafica inserita AITRC
258600150624     C                   eval        trcIVA  = d2IVA
258700150624     C                   eval        trcPICF = d4IVAC
258800150624     c                   move      'N'           found_aitrc       1
258900150624     c     Ktrc          setll     aitrc01l
259000150624     c     Ktrc          reade     aitrc01l
259100150624     c                   dow       not %EoF(aitrc01l)
259200150624     c                   if        TRCDFC > data_oggi or TRCDFC = 0
259300150624     c                   eval        found_aitrc ='S'
259400150624     c                   leave
259500150624     c                   end
259600150624     c     Ktrc          reade     aitrc01l
259700150624     c                   end
259800150624      **  se manca l'anagrafica messaggio che avvisa di inserirla
259900150624     c                   if          found_aitrc ='N'
260000150624     C                   SETON                                        8599
260100150624     c                   leavesr
260200150624     c                   end
260300150624      **
260400150624     c                   end
260500150831      *
260600150624     c                   else
260700150624      *
260800150624      *  se  c'è la PIVA coop dove non ci vuole
260900150624     c                   if          d4ivaC <> *blank
261000150624     C                   SETON                                        8399
261100150624     c                   leavesr
261200150624     c                   end
261300150624      *
261400150624     c                   end
261500150624      *
261600160105      *---------------
261700160105     C* Ripassa al video D2 i dati reimpostati qui: sugli AUTISTI
261800160107     c                   eval      h2filA  = h4filA
261900150624      *---------------
262000090320     C                   ENDSR
262100090320     C/EJECT
262200170324      **
262300170324      ************************************************************
262400170324      *?  Gestione Forzatura Codice Fiscale VUOTO         ?
262500170324      ************************************************************
262600170324     C     F07ForzaCfisc BEGSR
262700170324     C*
262800170324     c                   eval      Forza_cFISC = 'S'
262900170324     c                   eval      D4msg = *blank
263000170324     c                   setoff                                       92
263100170327     c                   seton                                        99
263200170324     C*
263300170324     C                   ENDSR
263400170324     C/EJECT
263500160105      ************************************************************
263600160105      *?  Gestione VIDEATA 5                              ?
263700160105      ************************************************************
263800160105     C     GESD5         BEGSR
263900160105     C*
264000160105     C* inizializzazione videata
264100160105     C* settaggi 5°video campi attivi o campi protetti
264200160105     C     $INZD5        IFEQ      *ON
264300160105     C                   EXSR      INZD5
264400160105      * stampa sotto la videata
264500160108     c********           write     D2
264600160105     C                   MOVE      *OFF          $INZD5
264700160105     C                   END
264800160105     C*
264900160105     c   99              eval      errore = *on
265000160105     c                   SETOFF                                         99
265100160105      *
265200160105      * e sopra esegue la window delle funzioni particolari
265300160105     C*               *----------------*
265400160105     C                   EXFMT     D5
265500160105     C*               *----------------*
265600160105     c                   eval      errore = *off
265700160105     C*
2658001601051    C                   SELECT
265900160105     C* F3=Fine
266000160105     C     $TASTO        WHENEQ    F03
266100160105     C                   EXSR      F03Fine
266200160105     c                   leaveSR
266300160105     C* F12=Ritorno
266400160105     C     $TASTO        WHENEQ    F12
266500160108    >C     xtaopz        IfEQ      Immissione
266600160108      *  Dati Societari
266700160108     C                   EVAL      $GEST ='D2'
266800160108     C                   EVAL      $INZD2= *OFF
266900160108     c                   else
267000160108     C                   EXSR      F12Ritorna
267100160108     c                   end
267200160105     c                   leaveSR
267300160105     C*
2674001601052-   C                   ENDSL
267500160105     C*
267600160105    >C                   EXSR      CTRD5
267700160105     C*
267800160105     C     *IN99         IFEQ      *OFF
267900160105     C     $TASTO        andeq     F06
268000160105     c                   exsr      AggDataBase
268100160105     C                   ENDIF
268200160105      *
268300160105     C                   ENDSR
268400160105     C/EJECT
268500160105      ************************************************************
268600160105      *?  INIZIALIZZAZIONE VIDEATA DATI 5                 ?
268700160105      ************************************************************
268800160105     C     INZD5         BEGSR
268900160107     C*-----------
269000160107     c                   eval      d5FUN  =  D1FUN
269100160107     c                   eval      d5ANN  =  D1ANN
269200160107     c                   eval      H5NRC  =  H1Nrc
269300160107      *
269400160107     c                   setoff                                       0405
269500160111      *
269600160107      * Solo se Aggiunta AUTISTA deve pulire i campi di immissione
269700160111     C*
269800160107     c                   clear                   d5FILA
269900160107     c                   clear                   d5FILAd
270000160111     c                   clear                   d5Din
270100160111     c                   clear                   h5Din
270200160107     C*
270300160105     C                   ENDSR
270400160105     C/EJECT
270500160105     C************************************************************
270600160105      *?  CONTROLLO VIDEATA D5                            ?
270700160105     C************************************************************
270800160105     C     CTRD5         BEGSR
270900160108     C*
271000160108     c                   clear                   d5FILAd
271100160105      *
271200160105     C                   SETOFF                                       99
271300160108      * Data accreditamento
271400160108     c                   clear                   h5din
271500160108      ********
271600160108$017 C     d5din         IFgt      *zeros
271700160108     C                   Z-ADD     d5din         G02DAT
271800160108     C                   MOVEL     *BLANK        G02ERR
271900160108     C                   CALL      'XSRDA8'
272000160108     C                   PARM                    WLBDAT
272100160108     C                   Z-ADD     G02DAT        d5din
272200160108      * gira la data in AMG
272300160108     C                   IF        G02err = '0'
272400160108     c                   z-add     g02INV        h5din
272500160108     C                   endIF
272600160108     c                   end
272700160108      ********
272800160108     c                   if        d5din  <> *zeros and G02err <>'0'
272900160108     c                              or
273000160108     c                             d5din  =  *zeros
273100160108     C                   SETON                                        7599
273200160108     c                   leavesr
273300160108     C                   ENDIF
273400160108      *------
273500160108      * Non può essere inferiore alla data emissione del contratto
273600160108     c                   if        xtaopz = AddAutoDiScor
273700160108$017 C     h5din         IFlt      h2dec
273800160108     C                   SETON                                        7599
273900160108     c                   leavesr
274000160108     C                   end
274100160108     C                   end
274200160105      *
274300160108     C* controllo codice vuoto  Filiale
274400160108$017 C     D5fila        IFEQ      *zeros
274500160108     C                   SETON                                        6999
274600160108     c                   leavesr
274700160108     C                   Else
274800160108     c     D5fila        chain     azorg01l
274900160108     c                   if        %Found(azorg01l) and orgfag = 'F'
275000160108     c                   eval      D5filaD = orgdes
275100160108     c                   else
275200160108     C                   SETON                                        6999
275300160108     c                   leavesr
275400160108     c                   end
275500160108     C                   ENDIF
275600160105      *
275700160111     c                   eval      h2sco   = 'S'
275800160108     c                   eval      h2filA  = d5filA
275900160108      *
276000160105     C                   ENDSR
276100160105     C/EJECT
276200090319      ************************************************************
276300090320      *?  Aggiorna il Data Base                           ?
276400090319      ************************************************************
276500090319     C     AggDataBase   BEGSR
276600090319     C*
276700090319     C* eseguo aggiornamento
276800160112    >C                   EXSR      AGG_Db
276900090319      *
277000090319     C* eseguo operazioni del dopo-aggiornamento
277100090319     C  N99              EXSR      GESAGG
277200090324     c  N99              commit
277300090330      *
277400090330     c   99              eval      d4FUN  =  D1FUN
277500090324     c   99              rolbk
277600090324     c   99              exfmt     ALERTWIND
277700090319     C*
277800090319     C                   ENDSR
277900030204     C/EJECT
278000940131     C************************************************************
278100090320      *?  OPERAZIONI INIZIALI                             ?
278200940131     C************************************************************
278300940131     C     *INZSR        BEGSR
278400090706     C*
278500940127     C* Reperimento parametri
278600940127     C*
278700940117     C     *ENTRY        PLIST
278800940117     C                   PARM                    KPJBA
278900090311     C                   movel     kpjbu         Xtabds
279000120323     c                   setoff                                       23  25
279100090312     C*
279200120323     C*   ATTENZIONE: NOVITA' dal 1/4/2012
279300120323     C* ===================================  Accreditamento su CONTRATTO SCADUTO
279400120323     C*  nuova OPZ => "X"  equivalente ad "A" ma su contratto SCADUTO
279500120323     C*  (questo è a causa del nuovo modo di impostare Contratti e Tariffe del 1/4/2012)
279600120323     C*
279700120323     c                   clear                   su_ContrSCADUT    1
279800120323      * trasforma l'opzione in A=Accreditamento ma si imposta il flag x ricordare la
279900120323      * particolarità del Contratto ormai SCADUTO.
280000120323     c                   if        xtaOPZ = 'X'
280100120323     c                   eval         xtaOPZ = 'A'
280200120323     c                   eval         su_ContrSCADUT = 'S'
280300120323     c                   end
280400120323     C*
280500940223     C* Variabili per gestione videate
280600940223     C                   MOVE      *OFF          $FINE
280700940223     C                   MOVE      *ON           $INZD1
280800940224     C                   MOVE      *BLANK        $LASTG
280900940224     C                   MOVE      *BLANK        $LASTV
281000940506     C                   Z-ADD     0             $ULKD1            3 0
281100940127     C*
281200940223     C* Variabili appoggio
281300940224     C                   Z-ADD     0             CURR
281400940224     C                   Z-ADD     0             CURC
281500940207     C*
281600940207     C* Valorizzazione campi univoci testate
281700940614     C                   MOVEL     KNSIF         NOMSIF
281800090312     C                   MOVEL     DSPGM         NOMPGM           10
281900090319     C*
282000090319     C* ESSENDO SEQUENZIALI I 3 DISPLAY LI PULISCO TUTTI UNA VOLTA SOLA
282100090319     C* Pulisce entrambe i 3 video
282200090319     C                   CLEAR                   D1
282300090319     C                   CLEAR                   D2
282400090319     C                   CLEAR                   D3
282500090319     C*
282600090323     C*  giro la udate da GMA in AMG
282700090323     C                   move      *date         dataeur
282800090323     C                   move      dataeur       dataiso
282900090323     C                   move      dataiso       data_oggi
283000160204     c                   adddur    3:*m          dataiso
283100160204     c                   move      dataiso       datapiu3m
283200160204     c                   adddur    3:*m          dataiso
283300090323     C*
283400160111     C* data x controlli un anno da oggi
283500160128     c                   adddur    5:*y          dataiso
283600160128     C                   move      dataiso       data_5annIpiu
283700160111     C*
283800090319     C* Inizializzazione/Decodifica prima videata e seconda
283900090319     C                   MOVE      'D1'          $GEST
284000090319     C                   MOVE      *ON           $INZD1
284100090319     C                   MOVE      *OFF          $INZD2
284200090319     C                   MOVE      *OFF          $INZD3
284300090319     C*
284400090319     C*  Campi protetti o HI o RI
284500090319     C                   SETOFF                                       020304
284600090319     C                   SETOFF                                       050607
284700090319     C                   SETOFF                                       080911
284800090327     C                   SETOFF                                       101213
284900090706     C                   SETOFF                                       151601
285000090706     C*
285100090706     C*  Attenzione : è possibile ricevere come tipo funzione la modifica alla
285200090706     C*  sola DATA RICEZIONE CONTRATTO che può avvenire successivamente alla
285300090706     C*  chiusura del contratto.
285400090706     C*
285500090319     C                   SELECT
285600090319    >C     xtaopz        WHENEQ    Immissione
285700090319     c                   movel     Opz_Imm       D1Fun
285800090327     C                   SETON                                        11
285900090319    >C     xtaopz        WHENEQ    CambiaSocieta
286000090319     c                   movel     OPz_CMBsoc    D1Fun
286100090319     C                   SETON                                        020306
286200090319     C                   SETON                                        08
286300090319    >C     xtaopz        WHENEQ    CambiaPIva
286400090319     c                   movel     OPz_CMBpiva   D1Fun
286500090319     C                   SETON                                        020306
286600090319     C                   SETON                                        09
286700090319    >C     xtaopz        WHENEQ    DataFineC
286800090319     c                   movel     OPz_FINEc     D1Fun
286900090319     C                   SETON                                        020306
287000090330     C                   SETON                                        12  16
287100090706    >C     xtaopz        WHENEQ    DataRicezione
287200090706     c                   movel     OPz_RicezC    D1Fun
287300090706     C                   SETON                                        020306
287400090706     C                   SETON                                        01
287500090319    >C     xtaopz        WHENEQ    Visualizza
287600090319     c                   movel     Opz_Vis       D1Fun
287700090319     C                   SETON                                        020306
287800090319    >C     xtaopz        WHENEQ    Cancella
287900090319     c                   movel     Opz_Del       D1Fun
288000090319     C                   SETON                                        0203
288100090319    >C     xtaopz        WHENEQ    Modifica
288200090319     c                   movel     Opz_Var       D1Fun
288300090319     C                   SETON                                        02
288400090319    >C     xtaopz        WHENEQ    AddAutista
288500090319     c                   movel     OPz_AGGaut    D1Fun
288600090319     C                   SETON                                        07
288700090327    >C     xtaopz        WHENEQ    ModAutista
288800090327     c                   movel     OPz_ModAUTistaD1Fun
288900090327     C                   SETON                                        13
289000160108    >C     xtaopz        WHENEQ    AddAutoDiScor
289100160108     c                   movel     OPz_MACscorta D1Fun
289200160108     C                   SETON                                        07
289300090327     C*
289400090319     C                   OTHER
289500090330     C*
289600090330     C*
289700090319     C                   ENDSL
289800940127     C*
289900940117     C                   ENDSR
290000090317     c*--------------------------------------------------------------
290100090317     c* tramite Società e Unità decodifica P.IVA su PROJ
290200090317     c*--------------------------------------------------------------
290300090317     C     Decod_Rag_IVA BegSR
290400110401      **
290500110401     c                   eval      primo_tentativo ='S'
290600090317      **
290700090317      **  Routine x Reperire dati Fornitore:
290800090317      **    La routine in base alla sottonatura può funzionare
290900090317      **     x F=Fornitore/C=Cliente
291000110401     c     senza_trsFIL  tag
291100110401      *
291200090408     C                   clear                   trmz70s1ds                     Input
291300090408     C                   movel(p)  TRSiva        s1_PartitaIVA                  Input
291400090408     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
291500090408     C                   movel(p)  TRSSOCG       s1_Societa                     Input/Output
291600110401      * solo al primo tentativo carica la filiale
291700110401      *   in un secondo la lascia blank
291800110401     c                   If        primo_tentativo ='S'
291900090408     C                   movel(p)  TRSFIL        s1_Unita                       Input/Output
292000110401     c                   end
292100090401     c                   call      'TRMZ70SR1'
292200090408     C                   PARM                    trmz70s1ds                     Input
292300090408      *
292400110401      *  Se non lo aggancia perchè la filiale che ha concluso il contratto
292500110401      *   è diversa in un CAMBIO DI SOCIETA',  deve ritentare senza
292600110401      *   passare la filiale ossia lasciandola vuota.
292700110401    >c                   If         xtaopz = CambiaSocieta
292800110401     c                                and
292900110401     c                              S1_Trovato = *Off and S1_Errore <> '0'
293000110401     c                                and
293100110401     c                             primo_tentativo ='S'
293200110401      *
293300110401      * lo può fare solo una volta poi basta:
293400110401     c                   eval      primo_tentativo ='N'
293500110505      *****  Non lo deve fare più
293600110505     c***********        goTO      senza_trsFIL
293700110401     c                   EnD
293800150623      *
293900150623      *  se si tratta di un consorzio
294000150623     c                   clear                   consorzio         1
294100150623     c                   if        s1_NatGiur ='11'
294200150623     c                   eval       consorzio = 'S'
294300150623     c                   end
294400150623      *
294500090317     C                   ENDSR
294600090317     C/EJECT
294700090522     c*--------------------------------------------------------------
294800090522     c* tramite Società e Unità decodifica P.IVA su PROJ
294900090522     c*--------------------------------------------------------------
295000090522     C     Decod_P_IVA   BegSR
295100090522      **
295200090522      **  Routine x Reperire dati PIVA:
295300090522      **    La routine in base alla sottonatura può funzionare
295400090522      **     x F=Fornitore/C=Cliente
295500090522     C                   clear                   trmz70s4ds                     Input
295600090522     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
295700090522     C                   move      parm_S4CDF    s4_keyKSC                      Input
295800090522     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
295900090522     C                   movel(p)  parm_S4SOC    s4_Societa                     Input/Output
296000090525     c                   if        parm_S4FIL  > 0
296100090522     C                   movel(p)  parm_S4FIL    s4_Unita                       Input/Output
296200090525     c                   end
296300090522     c                   call      'TRMZ70SR4'
296400090522     c                   parm                    trmz70s4ds
296500090610      *
296600090610      *  se torna con l'errore riprova e aveva passato l'unità
296700090610      *   riprova passandogli l'unità vuota
296800091130     c                   if        s4_errore <> '0'
296900090610     C                   clear                   trmz70s4ds                     Input
297000090610     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
297100090610     C                   move      parm_S4CDF    s4_keyKSC                      Input
297200090610     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
297300090610     C                   movel(p)  parm_S4SOC    s4_Societa                     Input/Output
297400090610     c                   call      'TRMZ70SR4'
297500090610     c                   parm                    trmz70s4ds
297600091130     c                   end
297700090522      *
297800090522     C                   ENDSR
297900090522     C/EJECT
298000130527     c**********************************************************************
298100130527     c* Controlla se la partita IVA è presente sulla tab.PIA x quegli
298200130527     c* autisti appartenenti a società facenti capo ad un unica Proprietà
298300130527     c**********************************************************************
298400130527     c     chk_tab_PIA   begsr
298500130607      *
298600130607     c                   clear                   su_piu_societa    1
298700130607     c                   eval      PIVA_lunga11 = '%' + D2IVA
298800130607     c                   eval      PIVA_lunga11 = %Trim(PIVA_lunga11) + '%'
298900130527      *
299000130527     c                   clear                   trovata_PIA       1
299100130527     c                   eval       SQLCOD = 0
299200130527     c                   eval       VALUE_IND = 0
299300130527      *
299400130527      *  Controlla la PIVA dal contratto fra quelle presenti sulla "PIA"
299500130527      *
299600130527     C/EXEC SQL
299700130527     C+ SELECT  'S' into :trovata_PIA           INDICATOR :VALUE_IND
299800130527     C+   FROM  TNTBE00F   WHERE  TBECOD = 'PIA' AND
299900130527     C+    TBEUNI like  :PIVA_lunga11
300000130527     C+    fetch first row only
300100130527     C/END-EXEC
300200130527     c                   if           SQLCOD = 0 and VALUE_IND = 0
300300130527     c                   eval      su_piu_SOCIETA = trovata_PIA
300400130527     c                   end
300500130607      *
300600130607      *   Solo se torna con "S" dopo il check sulla tabella emette una finestra
300700130607      *    di avviso
300800130607     c                   if          su_piu_societa = 'S'
300900130607     c                   eval      d4msg0 = *blank
301000130607     c                   eval      d4msg1 = ' Attenzione: l''AUTISTA appartie-
301100130607     c                             ne a SOC. da monitorare'
301200130607     c                   eval      d4msg2 = *blank
301300130607     c                   eval      d4msg3 = '          il Cod.Autista è: ' +
301400130607     c                             aut_x_msg
301500130607     c                   eval      d4msg4 = *blank
301600130607     c                   eval      d4msg5 = *blank
3017001306071-   C                   Exfmt     MsgWind
301800130607     C                   SETON                                          99
301900130607     c                   leavesr
302000130607     c                   end
302100130527      *
302200130527     C                   ENDSR
302300130527     C/EJECT
302400090317     c**********************************************************************
302500090317     c* reperimento numero contratto
302600090317     c**********************************************************************
302700090317     C     repnum        BEGSR
302800090317     c                   clear                   numero_NRC        7 0
302900090317     c                   clear                   trul33ds
303000090317     c                   move      'L'           i33tla
303100090317     c                   z-add     32            i33cnu
303200090317     c                   z-add     1             i33num
303300090317     c                   movel     trul33ds      kpjbu
303400090317     c                   call      'TRUL33R'
303500090317     c                   PARM                    kpjba
303600090317     c                   movel     kpjbu         trul33ds
303700090317     c                   if        o33err <> *zeros
303800090317     c                   dump
303900090317     c                   else
304000090317     c                   z-add     o33nrf        numero_NRC
304100090317     c                   endif
304200090317     C                   ENDSR
304300940128     C************************************************************
304400090319     C*
304500090319     C************************************************************
304600090319     C     Decod_societa begSR
304700090319      *
304800090319     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
304900090319     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
305000090319     C                   RESET                   tibsSocI0
305100090319     C                   EVAL      tibsSocI0.idSocieta = wSOCG
305200090319     C
305300090319     C                   CALL      'TIBSSOCR'
305400090319     C                   PARM      'GETANAGRAF'  prmRqsOpCode
305500090319     C                   PARM      *BLANK        prmRpyOpCode
305600090319     C                   PARM      *ZERO         prmRpyIdMsg
305700090319     C                   PARM      'TIBSSOCI0'   prmRqsFormato
305800090319     C                   PARM                    tibsSocI0
305900090319     C                   PARM                    prmRqsDataSize
306000090319     C                   PARM      'TIBSSOCO0'   prmRpyFormato
306100090319     C                   PARM                    tibsSocO0
306200090319     C                   PARM                    prmRpyDataSize
306300090323     c                   if         PRMRPYIDMSG >= 0
306400090402     c                              and TIBSSOCO0.IDSOCIETA <> *blank
306500090323     c                   eval      wsocGd = tibsSocO0.RAGSOCIALE
306600090323     c                   else
306700090323     c                   eval      wsocGd = 'Errore'
306800090323     c                   end
306900090319      *
307000090319     C                   ENDSR
307100111229     C************************************************************
307200111229     C* GESTIONE F01 Appendici al contratto
307300111229     C************************************************************
307400111229     C     F01appendici  BEGSR
307500111229      *
307600111229     c                   eval      kpjbuS = kpjbu
307700111229     c                   movel     h2nrc         kpjbu
307800111229     c                   call      'TRMZ79CR'
307900111229     c                   parm                    kpjba
308000111229     c                   eval      kpjbu  = kpjbuS
308100111229      *
308200111229     C                   ENDSR
308300090319     C************************************************************
308400090406     C* GESTIONE F02 interrogazione Tariffe
308500090402     C************************************************************
308600090406     C     F02Tariffe    BEGSR
308700090406      *
308800160108     c                   if        H2NMFL = 'D4AUTC' or
308900090408     c                             H4NMFL = 'D4AUTC1'
309000090406     c                   eval      kpjbuS = kpjbu
309100090406     c                   call      'FICN01R'
309200090406     c                   parm                    kpjba
309300090406     c                   eval      kpjbu  = kpjbuS
309400090406     c                   leavesr
309500090406     c                   end
309600090406      *
309700090406     C                   ENDSR
309800090406     C************************************************************
309900090406     C* GESTIONE F04 interrogazioni
310000090406     C************************************************************
310100090406     C     F04Ricerche   BEGSR
310200090406      *
310300160205      *
310400160205     c                   if        H2NMFL = 'D2COR'
310500160205     c                   eval      kpjbuS = kpjbu
310600160205     c                   clear                   d4ccnl
310700160205     c                   clear                   tibs02ds
310800160205     C                   MOVEL     'R'           t02mod
310900160205     C                   MOVEL     knsif         t02sif
311000160205     c                   MOVEL     'SRG'         t02cod
311100160205     C                   movel     d2cor         T02ke1
311200160205     C                   CALL      'TIBS02R'
311300160205     C                   PARM                    KPJBA
311400160205     C                   PARM                    TIBS02DS
311500160205     C                   IF        T02err = *BLANKS
311600160205     C                   movel     T02uni        dsrg
311700160205     C                   movel     T02ke1        d2cor
311800160205     C                   movel     §srgdes       d2cord
311900160205     C                   ENDIF
312000160205     c                   endif
312100160205      *
312200090409     c                   if        H4NMFL = 'D4AUTC' or
312300090409     c                             H4NMFL = 'D4AUTC1'
312400090409     c                   if        H4protc <>'1'
312500090409     c                   eval      kpjbuS = kpjbu
312600090409     c                   clear                   kpjbu
312700090409     c                   clear                   fnlv28ds
312800090409     c                   eval      D28TIPI ='A'
312900160107     c                   if        h4fila > 0
313000160107     c                   eval      D28FILI = h4fila
313100090409     c                   else
313200090409     c                   if        D4AUTC > 0
313300090409     c                   eval      D28FILI = D4AUTC
313400151223     c                   end
313500090409     c                   end
313600090409     c                   eval      kpjbu  = fnlv28ds
313700090409     c                   call      'FNLV28R'
313800090409     c                   parm                    kpjba
313900090409     c                   eval      fnlv28ds = kpjbu
314000090409     c                   if        D28ERR = *blank
314100090409     c                   eval      d4autc = D28PDRO/10000
314200090409     c                   z-add     D28PDRO       d4autc1
314300090409     c                   else
314400090409      * x errore
314500090409     c                   seton                                        7099
314600090409     c                   end
314700090409     c                   eval      kpjbu  = kpjbuS
314800090409     c                   leavesr
314900090409     c                   end
315000090409     c                   end
315100090409      *
315200090409     c                   if        H4NMFL = 'D4AUTAD' or
315300090409     c                             H4NMFL = 'D4AUTAD1'
315400090409     c                   if        H4protad<>'1'
315500090409     c                   eval      kpjbuS = kpjbu
315600090409     c                   clear                   kpjbu
315700090409     c                   clear                   fnlv28ds
315800090409     c                   eval      D28TIPI ='D'
315900160107     c                   if        h4fila > 0
316000160107     c                   eval      D28FILI = h4fila
316100090409     c                   else
316200090409     c                   if        D4AUTAD > 0
316300090409     c                   eval      D28FILI = D4AUTAD
316400090409     c                   end
316500090409     c                   end
316600090409     c                   eval      kpjbu  = fnlv28ds
316700090409     c                   call      'FNLV28R'
316800090409     c                   parm                    kpjba
316900090409     c                   eval      fnlv28ds = kpjbu
317000090409     c                   if        D28ERR = *blank
317100090409     c                   eval      d4autad = D28PDRO/10000
317200090409     c                   z-add     D28PDRO       d4autad1
317300090409     c                   else
317400090409      * x errore
317500090409     c                   seton                                        7199
317600090409     c                   end
317700090409     c                   eval      kpjbu  = kpjbuS
317800090409     c                   leavesr
317900090409     c                   end
318000090409     c                   end
318100090409      *
318200090402     C                   ENDSR
318300150624     C************************************************************
318400150624     C* GESTIONE F08 Gestione Coop Consorziate P.IVA coop
318500150624     C************************************************************
318600150710     C     F15CoopCons   BEGSR
318700150624      *
318800150624     C                   RESET                   TRMZ70S8DS
318900150624     C                   MOVEL     '1'           S8_ds8opz
319000150624     C                   MOVE      *ZERO         S8_ds8ret
319100150624     C                   MOVE      '1'           S8_ds8opr
319200150624     C                   MOVEl     d2socG        S8_ds8SOCG
319300150624     C                   MOVEl     d2iva         S8_ds8IVA
319400150710     C                   MOVEl     F15ivac       S8_ds8IVAA
319500150624     C                   movel     kpjbu         KPJBUs
319600150624     C                   clear                   KPJBU
319700150624     C                   MOVEL     TRMZ70S8DS    KPJBU
319800150624$004 C                   CALL      'TRMZ70SR8'
319900150624     C                   PARM                    KPJBA
320000150624     C                   MOVEL     KPJBU         TRMZ70S8DS
320100150624     C                   movel     kpjbus        KPJBU
320200150624      **
320300150624     C                   ENDSR
320400090402     C************************************************************
320500090402     C* GESTIONE F03 VIDEO D1
320600090402     C************************************************************
320700090402     C     F03Fine       BEGSR
320800090402     C*
320900090319     C                   MOVE      *ON           $FINE
321000090319    >C                   MOVE      '1'           xtaret
321100090319     C*
321200090319     C                   ENDSR
321300090319     C************************************************************
321400090319     C* GESTIONE F12 VIDEO D1
321500090319     C************************************************************
321600090319     C     F12Ritorna    BEGSR
321700090319     C*
321800090319     C                   MOVE      *ON           $FINE
321900090319     C*
322000090319     C                   ENDSR
322100090319     C************************************************************
322200090319     C*
322300090319     C************************************************************
322400090320     C     Quale_AITRS   begSR
322500090319     C*
322600090320     C*  Aggancia la decodifica della società
322700090320     C     H1recTRS      chain(N)  AITRs00F
322800090320     C                   if        %Found(AITRs00F)
322900090320     c                   move      'S'           trovato_TRS
323000090320     c                   eval      H2recTRS = H1recTRS
323100090320     c                   eval      H3recTRS = H1recTRS
323200090320     c                   eval      H4recTRS = H1recTRS
323300160107     c                   eval      H5recTRS = H1recTRS
323400090320      *
323500090320     c                   if        TrsANN <> *blank
323600090320     c                   seton                                        23
323700090320     C                   MOVEl     Annull        D1Ann
323800090320     C                   end
323900120323     c                   if        TrsDFC > 0  and not *IN23
324000120323     c                   seton                                        25
324100120323     C                   MOVEl     ContrSCADUT   D1Ann
324200120323     C                   end
324300090320     c                   endIF
324400090320     C*
324500090320     C                   ENDSR
324600090320     C************************************************************
324700090320     C*
324800090320     C************************************************************
324900090320     C     Quale_AITRA   begSR
325000090320     C*
325100090319     C*     SFL di selezione quale autista prendere in considerazione
325200090319     C*
325300090320     c                   eval      kpjbuS = kpjbu
325400090320     c                   clear                   atabDS
325500090320     c                   eval      ataKEY = d4NRC
325600090320     c                   eval      KPJBU = atabDS
325700090320     c                   call      'TRMZ70SR0'
325800090320     c                   parm                    KPJBA
325900090320     c                   eval      atabDS = KPJBU
326000090320     c                   eval      kpjbu = kpjbuS
326100090319     C*
326200090319     c                   eval      trovato_AITRA = 'N'
326300160107     c                   clear                   h5recTRA
326400160107     c                   clear                   h4recTRA
326500160107     c                   clear                   h3recTRA
326600160107     c                   clear                   h2recTRA
326700160107     c                   clear                   h1recTRA
326800090319     C*
326900090319     C* Aggancia AITRA
327000090320     C     ATArecord     chain(N)  AITRa00f
327100090320     C                   IF        %Found(AITRa00f)
327200090319     c                   eval      trovato_AITRA = 'S'
327300160107     c                   eval      h5recTRA = ataRecord
327400160107     c                   eval      h4recTRA = ataRecord
327500160107     c                   eval      h3recTRA = ataRecord
327600160107     c                   eval      h2recTRA = ataRecord
327700160107     c                   eval      h1recTRA = ataRecord
327800090319     C                   END
327900090319     C*
328000090319     C                   ENDSR
328100090319     C/EJECT
328200090319     C************************************************************
328300090319    >C* AGGIORNAMENTO ANAGRAFICA
328400090319     C************************************************************
328500160112    >C     AGG_Db        BEGSR
328600090319     C*
328700090324     c                   setoff                                       99
328800090324     C*
328900090319     C                   SELECT
329000090319     C*
329100090319    >C     xtaopz        WHENEQ    Immissione
329200090330    >C**** xtaopz        OREQ      Copia
329300160112     c                   exsr      Scrive_Db
329400090330      *
329500090324    >C     xtaopz        WHENEQ    ADDautista
329600090330     c                   exsr      Aggiunge_Aut
329700160108      *
329800160108    >C     xtaopz        WHENEQ    AddAutoDiScor
329900160108     c                   exsr      Aggiunge_Scor
330000090330      *
330100090319    >C     xtaopz        WHENEQ    Modifica
330200090706    >C     xtaopz        OREQ      DataRicezione
330300160112     c                   exsr      Modifica_TRS
330400090327      *
330500090327    >C     xtaopz        WHENEQ    ModAutista
330600090330     c                   exsr      Modifica_Aut
330700090319      *
330800090319    >C     xtaopz        WHENEQ    Cancella
330900160112     c                   exsr      Annulla_Db
331000090319      *
331100090330    >C     xtaopz        WHENEQ    DataFineC
331200090401    >C     xtaopz        orEQ      CambiaPIva
331300090401    >C     xtaopz        orEQ      CambiaSocieta
331400160112     c                   exsr      Cambia_Db
331500090330      *
331600090319     C                   ENDSL
331700090330     C*
331800090330     C                   ENDSR
331900090330     C************************************************************
332000090401     C* LANCIA una routine esterna per la gestione di questi aggiornamenti
332100090330     C************************************************************
332200160112     C     Cambia_Db     BEGSR
332300090331     C*
332400090331     C* Dalla Window 3 imposta la data di chiusura contratto con
332500090331     C*  la vecchia P.IVA e riapre contratto con la nuova P.IVA
332600090331     C* E disaccredita tutti gli AUT legati al vecchio contratto
332700090331     C*  riaccreditandoli poi sul nuovo contratto.
332800090331     c                   move      h3Data        dataIso
332900090331     c     dataiso       subdur    1:*d          dataiso
333000090421      **
333100090421     c                   if        xtaopz = DataFineC
333200090421     c                   move      h3Data        S3_Data_Fine
333300090421     c                   else
333400090421     c                   move      dataiso       S3_Data_Fine
333500090421     c                   end
333600090401     c                   move      h3Data        S3_Data_Inizio
333700090421      **
333800090401     c                   eval      S3_Tipo_Aggior = xtaopz
333900090421      **
334000090401     c                   if        d3SocG <> *blank
334100090401     c                   eval      S3_Societa     = d3SocG
334200110505      ** Passa la Filiale per il cambio Società
334300110505     c                   eval      S3_Filiale     = d3Fil
334400090401     c                   else
334500090401     c                   eval      S3_Societa     = d2SocG
334600090401     c                   end
334700090401     c                   eval      S3_DesSocieta  = d3Des
334800090401     c                   if        d3IVA <> *blank
334900090401     c                   eval      S3_PartitaIva  = d3IVA
335000090401     c                   else
335100090401     c                   eval      S3_PartitaIva  = d2IVA
335200090401     c                   end
335300090401     C                   z-add     H3RECTRS      S3_NRec_TRS
335400090401     c                   clear                   s3_Errore
335500090401      * routine esterna
335600090401      *   x cambio P.IVA/cambio Società/Data Fine Contratto
335700090401     C                   call      'TRMZ70SR3'
335800110506     c                   parm                    kpjba
335900090401     C                   parm                    S3_Tipo_Aggior    1
336000090401     C                   parm                    S3_Data_Fine      8 0
336100090401     C                   parm                    S3_Data_Inizio    8 0
336200090401     C                   parm                    S3_Societa        3
336300090401     C                   parm                    S3_DesSocieta    45
336400090401     C                   parm                    S3_PartitaIva    16
336500110505     C                   parm                    S3_Filiale        3 0
336600090401     C                   parm                    S3_NRec_TRS       9 0
336700090401     C                   parm                    S3_Errore         1
336800090331      *
336900090401      *  Errore che deve innescare il ROLBACK
337000090401     C                   if        S3_Errore = *on
337100090401     c                   seton                                        99
337200090401     c                   end
337300090319     C*
337400090330     C                   ENDSR
337500090319     C************************************************************
337600090330     C* Immissione
337700090319     C************************************************************
337800160112     C     Scrive_Db     BEGSR
337900090319     C*
338000090410     c                   clear                   aitrs000
338100090330     c                   eval      trsann = *blank
338200090410     C*
338300090409     C*  del TRS in immissione
338400090409     C*
338500090409     C* imposta i campi dal video al file
338600090409     c                   eval      TRSDVA  =  Data_oggi
338700090409     c                   eval      TRsDIM  =  Data_oggi
338800090409     c                   eval      TRSDVA  =  0
338900090409     C*
339000090409     c                   eval      TRsNRC   =  d2NRC
339100090409     c                   eval      TRsFIL   =  D2Fil
339200090409     c                   eval      TRsSOCG  =  D2SOCG
339300090409     c                   eval      TRsIVA   =  D2IVA
339400090409     c                   exsr      TRS_Modifica
339500090409      *
339600090409     c                   exsr      TRA_Immissione
339700160112     c                   exsr      TRA_Modifica
339800160112      *
339900090330     c                   write     aitrs000                             79
340000090330     c  n79              write     aitra000                             79
340100090330     C* se rcd non scrivibile attivo errore
340200090330    >C   79              SETON                                          99
340300090330      *
340400090330     C                   ENDSR
340500090330     C************************************************************
340600090330     C* Aggiunge Autista
340700090330     C************************************************************
340800090330     C     Aggiunge_Aut  BEGSR
340900090330     C*
341000090330     c                   exsr      TRA_Immissione
341100090409    >C                   EXSR      TRA_Modifica
341200160112     C*
341300090330     c                   write     aitra000                             79
341400090330     C* se rcd non scrivibile attivo errore
341500090330    >C   79              SETON                                          99
341600090330      *
341700090330     C                   ENDSR
341800160108     C************************************************************
341900160108     C* Aggiunge Auto di Scorta
342000160108     C************************************************************
342100160108     C     Aggiunge_Scor BEGSR
342200160108     C*
342300160108     c                   exsr      TRA_immissione
342400160108      *
342500160111     c                   eval      TRaSCO   =  'S'
342600160108     c                   eval      TRaDIN   =  h5din
342700160111     c                   eval      TRaFIL   =  D5fila
342800160108      *
342900160108     c                   write     aitra000                             79
343000160108     C* se rcd non scrivibile attivo errore
343100160108    >C   79              SETON                                          99
343200160108      *
343300160108     C                   ENDSR
343400090330     C************************************************************
343500090330     C* Modifica Dati
343600090330     C************************************************************
343700160112     C     Modifica_TRS  BEGSR
343800090330     C*
343900090331     C     h2recTRS      chain     aitrs00f
344000090330     c                   eval      trsann = *blank
344100090330    >C                   EXSR      TRS_Modifica
344200090330     c                   update    aitrs000                             79
344300090330     C* se rcd non scrivibile attivo errore
344400090330    >C   79              SETON                                          99
344500090330      *
344600090330     C                   ENDSR
344700090330     C************************************************************
344800090330     C* Modifica Autista
344900090330     C************************************************************
345000090330     C     Modifica_Aut  BEGSR
345100090330     C*
345200160107     c                   if        h2recTRA > 0
345300160107     C     h2recTRA      chain     AITRa00f
345400090330     c                   if        %Found(AITRa00f)
345500090330    >C                   EXSR      TRA_Modifica
345600090330     c                   update    aitra000                             79
345700090330     C* se rcd non scrivibile attivo errore
345800090330    >C   79              SETON                                          99
345900090330     c                   end
346000090330     c                   end
346100090330     C*
346200090330     C                   ENDSR
346300090330     C************************************************************
346400090330     C* Cancella
346500090330     C************************************************************
346600160112     C     Annulla_Db    BEGSR
346700090330     C*
346800090330     C     h2recTRS      chain     aitrs00f
346900090330     c                   move      'A'           trsANN
347000090330     c                   if        $tasto = F23
347100090330     c                   delete    aitrs000                             79
347200090330     c                   else
347300090330     c                   update    aitrs000                             79
347400090330     c                   end
347500090330      *
347600090330      *  Deve annullare tutti i record collegati di AITRA a quel contratto
347700090330     c                   if        *in79 = *off
347800090330     c     h2nrc         setll     aitra01l
347900090330     c     h2nrc         reade     aitra01l
348000090330      *
348100090330     c                   Dow       not %EoF(aitra01l)
348200090330     c                   eval      traANN = 'A'
348300090330      *
348400090330     c                   if        $tasto = F23
348500090330     c                   delete    aitra001                             79
348600090330     c                   else
348700090330     c                   update    aitra001                             79
348800090330     c                   end
348900090330    >C   79              leave
349000090330      *
349100090330     c     h2nrc         reade     aitra01l
349200090330     c                   endDO
349300090330     c                   end
349400090330      *
349500090330    >C   79              SETON                                          99
349600090330      *
349700090330     C                   ENDSR
349800090324     C************************************************************
349900090324     C* Campi di Modifica   TRS
350000090324     C************************************************************
350100090324     C     TRS_Modifica  BEGSR
350200090324     C*
350300090324     c                   eval      TRsDEC   =  h2DEC
350400090324     c                   eval      TRsDrC   =  h2DrC
350500090324     c                   eval      TRsCIT   =  D2CIT
350600090324     c                   eval      TRsNIS   =  D2NIS
350700090324     c                   eval      TRsDIA   =  h2DIA
350800090324     c                   eval      TRsPIA   =  D2PIA
350900090324     c                   eval      TRsNIA   =  D2NIA
351000090324     c                   eval      TRsUTE   =  knmus
351100160204     c                   eval      TRscor   =  D2cor
351200120217     C*
351300150305     c                   eval      dTRSflr1= hTRSflr1
351400150305      *  imposta eventuali modifiche alle 2 DATE
351500150305      *        e reimposta il campo DS
351600150305     c                   move      h2Ddurc       §TRSDdurc
351700150305     c                   move      h2Dcip        §TRSDcip
351800150305     c
351900150305     c                   eval      TRsFLR1  =  dTRSflr1
352000150305     C*
352100090324     C*
352200090324     C                   ENDSR
352300090324     C************************************************************
352400090324     C* Campi di Immissione TRA
352500090324     C************************************************************
352600090324     C     TRA_ImmissioneBEGSR
352700090324     C*
352800090410     c                   clear                   aitra000
352900090324     c                   eval      TRaSOCG  =  D2SOCG
353000090324     c                   eval      TRaTIP   =  'C'
353100090410     c                   eval      TRaSOC   =  D2PROJ
353200090324     c                   eval      TRaNRC   =  TRsNRC
353300090409      *
353400090324     C                   ENDSR
353500090324     C************************************************************
353600090324     C* Campi di Modifica   TRA
353700090324     C************************************************************
353800090324     C     TRA_Modifica  BEGSR
353900090324     C*
354000160108     c                   eval      TRaAUT   =  D4DAUTC
354100160108     c                   if        traAUT = *blank and d4DAUTAD<> *blank
354200160108     c                   eval      TRaAUT   =  D4DAUTAD
354300160108     c                   end
354400160108      *
354500090409     c                   eval      TRaKAU   =  h4AUTC
354600090409     c                   eval      TRaBPT   =  h4AUTAD
354700090409     c                   eval      TRaRIS   =  D4RIS
354800151223     c                   z-add     d4orel        traorel
354900130717     c                   move      d4dipa        tradipa
355000170626     c                   move      d4atp         traatp
355100150309     c                   move      d4ie          traitaest
355200160104     c                   EVAL      traLIQ   =  d4Liv
355300160104     c                   EVAL      traCNL   =  d4CCNL
355400160104     c                   EVAL      traQUA   =  d4QuaPr
355500160104     c                   EVAL      tracF    =  d4Cfisc
355600160104     C*
355700091215     c                   if        Attivato_a_Perdere ='S'
355800090921     c                   eval      TRaCOR   =  D4aper
355900091215     c                   end
356000160104     C*
356100090410     c                   eval      TRaDIN   =  H4din
356200160107     C*
356300160104     c                   eval      traUNIL  =  H4dUNIL
356400160104     c                   eval      tradTsPs =  H4dTsPs
356500160104     c                   eval      tradTrPs =  H4dTrPs
356600160107     c                   eval      TRaFIL   =  h4filA
356700150623     c                   eval      §TRAivac =  D4ivaC
356800150623     c                   movel     dtraFILLER    trafiller
356900090417      *
357000160107     c                   eval      TRaSCO   =  h4SCO
357100160107     C*
357200090417      *  Se sono in modifica dell'autista e sto gestendo come
357300090417      *   Autista di riserva --> si devono pulire tutti i dati del mezzo
357400090417    >C                   if        xtaopz = ModAutista and
357500170221     C                             (D4RIS <>' ' or (D4aper = 'S' and
357600091215     c                             Attivato_a_Perdere = 'S'))
357700090417     c                   clear                   TRATIA
357800090417     c                   clear                   TRATAA
357900090417     c                   clear                   TRABPO
358000090417     c                   clear                   TRAANT
358100090417     c                   clear                   TRATMP
358200090417     c                   clear                   TRADTM
358300090417     c                   clear                   TRADASS
358400090417     c                   clear                   TRAPUB
358500090417     c                   clear                   TRAFUR
358600090417     c                   clear                   TRASPI
358700090417     c                   clear                   TRANT1
358800090417     c                   clear                   TRANT2
358900090417     c                   clear                   TRADT2
359000090417     c                   clear                   TRADT3
359100090417     c                   clear                   TRADT4
359200090417     c                   clear                   TRADRR
359300090417     c                   clear                   TRADPR
359400090417     c                   clear                   TRADP2
359500090417     c                   clear                   TRADP3
359600090417     c                   clear                   TRADP4
359700090417     c                   clear                   TRANRP
359800090417     c                   clear                   TRAFF1
359900090417     c                   clear                   TRAFF2
360000090417     c                   clear                   TRARIF
360100090417     C                   END
360200120323     C*============
360300120323     C* FORZATURE:   su Contratto già SCADUTO
360400120323     C*============
360500120323      *  Se Accreditamento su Contratto già SCADUTO
360600120323      *   si deve mettere come Data DISACCREDITAMENTO la data di FINE CONTRATTO
360700120323     c                   If         su_ContrSCADUT = 'S'
360800120323     C*  attenzione x ACCREDITAMENTO su CONTRATTO già SCADUTO occorre avere
360900170221     C*  la "R" come RISERVA e "N" su TURNOVER e DATA Fine Contratto
361000120323     C*    come data ACCREDITAMENTO.
361100120323     c                   eval      TRaDFI  = h4DFC
361200120323     c                   eval      TRaTUR  = 'N'
361300170221     c                   eval      TRaRIS  = 'R'
361400120323     c                   endIf
361500090331      *
361600090324     C                   ENDSR
361700090324     C************************************************************
361800090324     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
361900090324     C************************************************************
362000090324     C     GESAGG        BEGSR
362100090327     C*
362200090327     C* Attenzione se è stato richiesta l'aggiunta Autista con il tasto
362300090327     C*  Funzionale F8
362400090327     C*   Devo rimanere dentro questo programma e non uscire al chiamante
362500090327     C     $TASTO        ifeq      F08
362600090327    >C     xtaopz        oreq      AddAutista
362700090327    >c                   eval      xtaopz = AddAutista
362800090327     C                   MOVE      *ON           $INZD4
362900090327     c                   eval      $GEST = 'D4'
363000090327     c                   leavesr
363100090327     c                   end
363200090327     C*
363300090319     C* segnalo al pgm chiamante l'avvenuta conferma
363400090331    >C********           MOVE      *ON           xtaopr
363500090331     C*   non lo imposto + a *ON per non ricaricare nuovamente il SFL
363600090331     C*   nel chiamante.
363700090331     C*
363800090319     C* segnalo al pgm chiamante l'aver premuto l'F6
363900090319    >C                   MOVE      '0'           xtaret
364000090319     C*
3641000903191    C                   SELECT
364200090319     C* nel caso di immissione, ripristino la videata iniziale
364300090319    >C     xtaopz        WHENEQ    Immissione
364400090319     C                   MOVE      *ON           $INZD1
364500090319     C* altrimenti ritorno al pgm chiamante
364600090319     C                   OTHER
3647000903191-   C                   ENDSL
364800090327     C*  E deve chiudere
364900090327     C                   MOVE      *ON           $FINE
365000090327     C*
365100090319     C*
365200090319     C                   ENDSR
365300090320     C************************************************************
365400090506      *? Controlla sulle Tariffe degli AUT legati al contratto     ?
365500090506      *?    se presenti già Tariffe Confermate.                    ?
365600090320     C************************************************************
365700090506     C     se_TAR_ConV   BEGSR
365800090320     C*
365900090506     C* Routine di Controllo legata al video D2 e alla data emissione Contratto
366000090506     C*
366100090506      ** Tutti gli AUT legati al contratto
366200090506     C*   il primo che ha una tariffa confermata fa uscire dal ciclo
366300090506     c     d2nrc         setll     aitra01l
366400090506     c     d2nrc         reade     aitra01l
3665000905061-   C                   dow       not %Eof(aitra01l)
366600090506     c                   if        traDFI = 0 and traANN = *blank
366700090506      * AUT città
366800090506     c                   if        traKAU > 0
366900090506      *
367000090506     c                   eval      TGTPDR = trakau
367100090506     c                   eval      TGTSML = ' '
367200091006     c*****Ktgt          setll     fitgt01l
367300091006     c*****Ktgt          reade     fitgt01l
367400091006     c     Ktgt          setgt     fitgt01l
367500091006     c     Ktgt          readpe    fitgt01l
367600090506     c                   Dow       not %EoF(fitgt01l)
367700090506      *
367800090506      * se la decorrenza è maggiore dalla data emissione del contratto
367900090506      *  e se Tariffa già CONVALIDATA
368000090506     c                   if        tgtDDT >= h2dec and tgtDTS >0
368100090506      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
368200090506      *   a quella del contratto
368300090522     C                   move (p)  tgtCDF        parm_s4CDF                        Input
368400090522     C                   movel(p)  tgtSOC        parm_s4SOC                        Input/Output
368500090522     C                   movel(p)  tgtFIL        parm_s4FIL                        Input/Output
368600090522     c                   exsr      DECOD_P_IVA
368700090506      * se la P.IVA coincide
368800090506     c                   if        s4_errore ='0' and
368900090506     c                             s4_PARTITAIVA = trsIVA
369000090506      *
369100090506      *   allora deve essere data per convalidata
369200090506     c                   eval      se_convalidata='S'
369300091006     c********           leave
369400091006     c                   else
369500091006     c                   leave
369600091006     c                   end
369700090506      *
369800090506     c                   end
369900090506      *
370000091006     c**** Ktgt          reade     fitgt01l
370100091006     c     Ktgt          readpe    fitgt01l
370200090506     c                   endDo
370300090506      *
370400090506      * AUT affl./defl.
370500090506     c                   elseif    traBPT > 0
370600090506      *
370700091006     c**** traBPT        setll     fiadt01l
370800091006     c**** traBPT        reade     fiadt01l
370900091006     c     traBPT        setgt     fiadt01l
371000091006     c     traBPT        readpe    fiadt01l
371100090506     c                   Dow       not %EoF(fiadt01l)
371200090506      *
371300090506      * se la decorrenza è maggiore dalla data emissione del contratto
371400090506      *  e se Tariffa già CONVALIDATA
371500090506     c                   if        adtDDT >= h2dec and adtDTS >0
371600090506      *
371700090506      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
371800090506      *   a quella del contratto
371900090522     C                   move (p)  adtCDF        parm_s4CDF                        Input
372000090522     C                   movel(p)  adtCSF        parm_s4SOC                        Input/Output
372100090522     C                   movel(p)  adtFIL        parm_s4FIL                        Input/Output
372200090522     c                   exsr      DECOD_P_IVA
372300090506      * se la P.IVA coincide
372400090506     c                   if        s4_errore ='0' and
372500090506     c                             s4_PARTITAIVA = trsIVA
372600090506      *
372700090506      *   allora deve essere data per convalidata
372800090506     c                   eval      se_convalidata='S'
372900091006     c************       leave
373000091006     c                   else
373100091006     c                   leave
373200091006     c                   end
373300090506      *
373400090506     c                   end
373500090506      *
373600091006     c**** traBPT        reade     fiadt01l
373700091006     c     traBPT        readpe    fiadt01l
373800090506     c                   endDo
373900090506      *
374000090506     c                   end
374100090506      *
374200090506     c                   end
374300090506      *
374400090506     c     d2nrc         reade     aitra01l
374500090506     c                   endDO
374600090506     C*
374700090506     C                   ENDSR
374800090506     C************************************************************
374900090506      *? Controlla sulle Tariffe degli AUT legati al contratto     ?
375000090506      *?    se qualcuna non è ancora SCADUTA.                      ?
375100090506     C************************************************************
375200090506     C     se_TAR_ATTIVA BEGSR
375300090506     C*
375400090506     C* Routine di Controllo legata al video D3 al cambio P.IVA o Società
375500090506     C*
375600090506      ** Tutti gli AUT legati al contratto
375700090506     C*   il primo che ha una tariffa ATTIVA fa uscire dal ciclo
375800090506     c     d2nrc         setll     aitra01l
375900090506     c     d2nrc         reade     aitra01l
3760000905061-   C                   dow       not %Eof(aitra01l)
376100090506     c                   if        traDFI = 0 and traANN = *blank
376200090506      * AUT città
376300090506     c                   if        traKAU > 0
376400090506      *
376500090506     c                   eval      TGTPDR = trakau
376600090506     c                   eval      TGTSML = ' '
376700090506     c     Ktgt          setll     fitgt01l
376800090506     c     Ktgt          reade     fitgt01l
376900090506     c                   Dow       not %EoF(fitgt01l)
377000090506      *
377100090506      * se la decorrenza è maggiore dalla data emissione del contratto
377200090506      *  e se Tariffa non SCADUTA
377300101102     c                   if        tgtDDT >= h2dec and tgtDST =0 and tgtatb=' '
377400090506      *
377500090506      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
377600090506      *   a quella del contratto
377700090507     c                   if        tgtDTS >0
377800090522     C                   move (p)  tgtCDF        parm_s4CDF                        Input
377900090522     C                   movel(p)  tgtSOC        parm_s4SOC                        Input/Output
378000090522     C                   movel(p)  tgtFIL        parm_s4FIL                        Input/Output
378100090522     c                   exsr      DECOD_P_IVA
378200090506      * se la P.IVA coincide
378300090506     c                   if        s4_errore ='0' and
378400090506     c                             s4_PARTITAIVA = trsIVA
378500090506      *   allora deve essere data per NON scaduta
378600090506     c                   eval      se_attiva ='S'
378700090506     c                   leave
378800090522     c                   end
378900090522      *
379000090506     c                   end
379100090506      *
379200090506     c                   end
379300090506      *
379400090506     c     Ktgt          reade     fitgt01l
379500090506     c                   endDo
379600090506      *
379700090506      * AUT affl./defl.
379800090506     c                   elseif    traBPT > 0
379900090506      *
380000090506     C*   il primo che ha una tariffa ATTIVA fa uscire dal ciclo
380100090506     c     traBPT        setll     fiadt01l
380200090506     c     traBPT        reade     fiadt01l
380300090506     c                   Dow       not %EoF(fiadt01l)
380400090506      *
380500090506      * se la decorrenza è maggiore dalla data emissione del contratto
380600090506      *  e se Tariffa non SCADUTA
380700101102     c                   if        adtDDT >= h2dec and adtDST =0  and adtatb=' '
380800090506      *
380900090506      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
381000090506      *   a quella del contratto
381100090507     C                   IF        adtDTS >0
381200090522     C                   move (p)  adtCDF        parm_s4CDF                        Input
381300090522     C                   movel(p)  adtCSF        parm_s4SOC                        Input/Output
381400090522     C                   movel(p)  adtFIL        parm_S4FIL                        Input/Output
381500090522     c                   exsr      DECOD_P_IVA
381600090506      * se la P.IVA coincide
381700090506     c                   if        s4_errore ='0' and
381800090506     c                             s4_PARTITAIVA = trsIVA
381900090506      *   allora deve essere data per NON scaduta
382000090506     c                   eval      se_attiva ='S'
382100090506     c                   leave
382200090506     c                   end
382300090522      *
382400090522     C                   END
382500090506      *
382600090506     c                   end
382700090506      *
382800090506     c     traBPT        reade     fiadt01l
382900090506     c                   endDo
383000090506      *
383100090506     c                   end
383200090506      *
383300090506     c                   end
383400090506      *
383500090506     c     d2nrc         reade     aitra01l
383600090506     c                   endDO
383700090506     C*
383800090506     C                   ENDSR
383900090506     C************************************************************
384000090506      *? Controlla che le Tariffe siano scadute tutte nello        ?
384100090506      *?    stesso giorno.                                         ?
384200090506     C************************************************************
384300090506     C     tutte_ASSIEME BEGSR
384400101102      *
384500101102      * SOLO per cambio PARTITA IVA occorre confrontare con il giorno prima
384600101102    >C     xtaopz        IfEQ      CambiaPIva
384700101102     c                   move      h3Data        h3dataSalva       8 0
384800101102     c                   move      h3Data        dataIso
384900101102     c     dataiso       subdur    1:*d          dataiso
385000101102     c                   move      dataIso       h3Data
385100101102     c                   end
385200090506     C*
385300090506     C* Routine di Controllo legata al video D3 al cambio P.IVA o Società
385400090506     C*
385500090506      ** Tutti gli AUT legati al contratto
385600090506     C*   devono avere le tariffe scadute nello stesso momento.
385700090506     c     d2nrc         setll     aitra01l
385800090506     c     d2nrc         reade     aitra01l
3859000905061-   C                   dow       not %Eof(aitra01l)
386000090506     c                   if        traDFI = 0 and traANN = *blank
386100090506      * AUT città
386200090506     c                   if        traKAU > 0
386300090506      *
386400090506     c                   eval      TGTPDR = trakau
386500090506     c                   eval      TGTSML = ' '
386600090506     c     Ktgt          setll     fitgt01l
386700090506     c     Ktgt          reade     fitgt01l
386800090506     c                   Dow       not %EoF(fitgt01l)
386900090506      *
387000090506      * se la decorrenza è maggiore dalla data emissione del contratto
387100090506      *  e se Tariffa è scaduta
387200090506     c                   if        tgtDDT >= h2dec and tgtDST >0
387300101102      **
387400101102      **  Se precedentemente era stato segnalatola disuguaglianza sulle tariffe
387500101102      **   occorre, a cambio di codice autista, forzare l'uscita per dare msg
387600101102      **    di errore.
387700101102     c                   if        tgtpdr <> tgtpdr_sav and in_stesso_die = 'N'
387800101102     c                   LEAVE
387900101102     c                   end
388000101102      **
388100101102      *
388200090506      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
388300090506      *   a quella del contratto
388400090522     c                   if        tgtDTS >0
388500090522     C                   move (p)  tgtCDF        parm_s4CDF                        Input
388600090522     C                   movel(p)  tgtSOC        parm_s4SOC                        Input/Output
388700090522     C                   movel(p)  tgtFIL        parm_s4FIL                        Input/Output
388800090522     c                   exsr      DECOD_P_IVA
388900101102      *
389000101102      * se per caso aveva letto una tariffa precedente non adatta al paragone
389100101102      *  resetta il caso per non dare falsa segnalazione.
389200101102     c                   if        s4_errore ='0' and
389300101102     c                             s4_PARTITAIVA = trsIVA and
389400101102     c                             tgtDST = h3Data  and
389500101102     c                             tgtpdr = tgtpdr_sav and
389600101102     c                             in_stesso_die = 'N'
389700101102     c                   eval      in_stesso_die = *blank
389800101102     c                   end
389900101102      **
390000090506      * se la P.IVA coincide
390100090507     c                   if        s4_errore<>'0' or
390200090507     c                             (
390300090507     c                             s4_errore ='0' and
390400090506     c                             s4_PARTITAIVA = trsIVA and
390500090506     c                             tgtDST <> h3Data
390600090507     c                             )
390700090506      *
390800090506     c                   eval      in_stesso_die ='N'
390900101102     c******             leave
391000101102     c                   z-add     tgtpdr        tgtpdr_sav
391100090506     c                   end
391200090506      *
391300090522     c                   end
391400090522      *
391500090506     c                   end
391600090506      *
391700090506     c     Ktgt          reade     fitgt01l
391800090506     c                   endDo
391900090506      *
392000090506      * AUT affl./defl.
392100090506     c                   elseif    traBPT > 0
392200090506      *
392300090506     c     traBPT        setll     fiadt01l
392400090506     c     traBPT        reade     fiadt01l
392500090506     c                   Dow       not %EoF(fiadt01l)
392600090506      *
392700090506      * se la decorrenza è maggiore dalla data emissione del contratto
392800090506      *  e se Tariffa è SCADUTA
392900090506     c                   if        adtDDT >= h2dec and adtDST >0
393000090506      *
393100090506      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
393200090506      *   a quella del contratto
393300090507     c                   if        adtDTS >0
393400090522     C                   move (p)  adtCDF        parm_s4CDF                        Input
393500090522     C                   movel(p)  adtCSF        parm_s4SOC                        Input/Output
393600090522     C                   movel(p)  adtFIL        parm_s4FIL                        Input/Output
393700090522     c                   exsr      DECOD_P_IVA
393800090506      * se la P.IVA coincide
393900090507     c                   if        s4_errore<>'0' or
394000090507     c                             (
394100090507     c                             s4_errore ='0' and
394200090506     c                             s4_PARTITAIVA = trsIVA and
394300090506     c                             adtDST <> h3Data
394400090507     c                             )
394500090506      *
394600090506     c                   eval      in_stesso_die ='N'
394700090506     c                   leave
394800090506     c                   end
394900090522      *
395000090522     c                   end
395100090506      *
395200090506     c                   end
395300090506      *
395400090506     c     traBPT        reade     fiadt01l
395500090506     c                   endDo
395600090506      *
395700090506     c                   end
395800090506      *
395900090506     c                   end
396000090506      *
396100090506     c     d2nrc         reade     aitra01l
396200090506     c                   endDO
396300101102     C*
396400101102     C*  comunque reimposta la data salvata ad inizio routine
396500101105    >C     xtaopz        IfEQ      CambiaPIva
396600101102     c                   eval      h3Data = h3dataSalva
396700101105     c                   end
396800090506     C*
396900090506     C                   ENDSR
397000090507     C************************************************************
397100090507      *? Controlla Tariffa su allegato tariffario che, se trovata, ?
397200090507      *?   NON sia decorrente prima del contratto.                 ?
397300090507     C************************************************************
397400090507     C     CHK_Tar_AUTC  BEGSR
397500090507     C*
397600091130     C*   Routine al momento ABBANDONATA !!!!!!
397700091130     C*  ***************************************
397800091130     C*
397900091130     C*
398000091130     C*
398100091130     C*
398200091130     C*
398300090507     C*  Se imposta la data di decorrenza
398400090507     c                   clear                   Data_decorr       8 0
398500090617     c                   clear                   cod__TGTSOC
398600090617     c                   clear                   cod__TGTCDF
398700090507     C*
398800090507     C*  in immissione Contratto
398900090507     C* Routine di Controllo legata al video D2
399000090507      ** l'AUT impostato:
399100090507     c                   eval      TGTPDR = Autista
399200090507     c                   eval      TGTSML = ' '
399300090507     c     Ktgt          setll     fitgt01l
399400090507     c     Ktgt          reade     fitgt01l
399500090507      *
399600090507     c                   Dow       not %EoF(fitgt01l)
399700090507      *
399800090507      * se la decorrenza è inferiore alla data emissione del contratto
399900090507      *  e se Tariffa non STAMPATA
400000090522     c                   if        tgtDDT <  Data_Confronto and
400100090522     c                             tgtDST >= Data_Confronto
400200090507      *
400300090507      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
400400090507      *   a quella del contratto
400500090507     c                   if        tgtDTS > 0
400600090522     C                   move (p)  tgtCDF        parm_s4CDF                        Input
400700090522     C                   movel(p)  tgtSOC        parm_s4SOC                        Input/Output
400800090522     C                   movel(p)  tgtFIL        parm_s4FIL                        Input/Output
400900090522     c                   exsr      DECOD_P_IVA
401000090609     c                   end
401100090609      *
401200090507      * se la P.IVA coincide
401300090609     c********           if        (s4_errore <>'0' and tgtDTS > 0)   or
401400090609     c                   IF        (
401500090609     c                             s4_errore ='0' and tgtDTS > 0 and
401600090609     c                             s4_PARTITAIVA = PIVA_Confronto
401700091130     c                             and s4_SOCIETA = SOC_Confronto
401800090507     c                             )
401900090507     c                             or    tgtDTS = 0
402000090507      *   se congruente alla PArtita IVA
402100090507     c                   eval      Data_decorr = tgtDDT
402200090617     c                   eval        cod__TGTSOC  =  TGTSOC
402300090617     c                   eval        cod__TGTCDF  =  TGTCDF
402400090507     c                   leave
402500090507     c                   end
402600090522      *
402700090507     c                   end
402800090507      *
402900090507     c     Ktgt          reade     fitgt01l
403000090507     c                   endDo
403100090507      *
403200090507     C                   ENDSR
403300090507     C************************************************************
403400090507      *? Controlla Tariffa su allegato tariffario che, se trovata, ?
403500090507      *?   NON sia decorrente prima del contratto.                 ?
403600090507     C************************************************************
403700090507     C     CHK_Tar_AUTAD BEGSR
403800090507     C*
403900091130     C*   Routine al momento ABBANDONATA !!!!!!
404000091130     C*  ***************************************
404100091130     C*
404200091130     C*
404300091130     C*
404400091130     C*
404500091130     C*
404600091130     C*
404700090507     C*  Se imposta la data di decorrenza
404800090507     c                   clear                   Data_decorr       8 0
404900090617     c                   clear                   cod__TGTSOC
405000090617     c                   clear                   cod__TGTCDF
405100090507     C*
405200090507     C*  in immissione Contratto
405300090507     C* Routine di Controllo legata al video D2
405400090507      ** l'AUT impostato:
405500090507     c     Autista       setll     fiadt01l
405600090507     c     Autista       reade     fiadt01l
405700090507     c                   Dow       not %EoF(fiadt01l)
405800090507      *
405900090507      * se la decorrenza è inferiore alla data emissione del contratto
406000090507      *  e se Tariffa non STAMPATA
406100090522     c                   if        adtDDT <  Data_Confronto and
406200090522     c                             adtDST >= Data_Confronto
406300090507      *
406400090507      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
406500090507      *   a quella del contratto
406600090507     c                   if        adtDTS > 0
406700090522     C                   move (p)  adtCDF        parm_s4CDF                        Input
406800090522     C                   movel(p)  adtCSF        parm_s4SOC                        Input/Output
406900090522     C                   movel(p)  adtFIL        parm_s4FIL                        Input/Output
407000090522     c                   exsr      DECOD_P_IVA
407100090609     c                   end
407200090609      *
407300090507      * se la P.IVA coincide
407400090609     c********           if        (s4_errore <>'0' and adtDTS > 0)  or
407500090609     c                   IF        (
407600090609     c                             s4_errore ='0' and  adtDTS > 0 and
407700090609     c                             s4_PARTITAIVA = PIVA_Confronto
407800091130     c                             and s4_SOCIETA = SOC_Confronto
407900090507     c                             )
408000090507     c                             or    adtDTS = 0
408100090507      *
408200090507      *   se congruente alla PArtita IVA
408300090507     c                   eval      Data_decorr = adtDDT
408400090617     c                   eval        cod__TGTSOC  =  adtCSF
408500090617     c                   eval        cod__TGTCDF  =  adtCDF
408600090507     c                   leave
408700090507     c                   end
408800090507      *
408900090522     c                   end
409000090522      *
409100090507     c     Autista       reade     fiadt01l
409200090507     c                   endDo
409300090507      *
409400090507     C                   ENDSR
409500090522     C************************************************************
409600090522      *? Controlla Tariffa su allegato tariffario che, se trovata, ?
409700090522      *?   NON sia decorrente prima dell'accreditamento.           ?
409800090522     C************************************************************
409900090522     C     CHK_DEC_AUTC  BEGSR
410000090522     C*
410100100129     C*  Controlla che non vi sia un disaccreditamento sulla stessa società
410200100129     C*  precedente perchè l'autista è andato via per un periodo poi è tornato
410300100129     C*  sotto la stessa società. Se è così deve saltare il controllo della
410400100129     C*  tariffa.
410500110510     c                   eval      Tariffa_trovata = ' '
410600100129     c                   move      'S'           check_tariffa     1
410700120322     c                   clear                   Data_Limite       8 0
410800120323      **
410900120323      ** Solo nella PARTICOLARITA' che
411000120323      **  si stia eseguendo un accreditamento su UN CONTRATTO SCADUTO
411100120323     c                   IF           su_ContrSCADUT = 'S'
411200120323     c                   eval          Data_Limite = h2DFC
411300120323     c                   eLSe
411400120323      **
411500120323      **  Nella Normalità dei casi:
411600100129     c     autista       setgt     aitra03l
411700100129     c     autista       readpe    aitra03l
411800120322      *
411900120322     c                   dow       not %Eof(aitra03l)
412000120322      *
412100120322      *  Controlla la PIVA dal contratto
412200120322     c                   clear                   traPIVA
412300120322     C/EXEC SQL
412400120322     C+ SELECT  TRSIVA into :trapiva
412500120322     C+   FROM  AITRS00F
412600120322     C+  WHERE  TRSNRC = :tranrc
412700120322     C/END-EXEC
412800120322      *
412900120322      *  se disaccreditato e (società o PIVA diversa) DEVE SALTARE il controllo
413000120322     c                   if         tradfi > 0
413100120322     c                              AND
413200120322     c                                ( trasocG <> d1SocG
413300120322     c                                     or
413400120322     c                                  traPIVA <> d1IVA )
413500120509     c********           clear                   check_tariffa
413600120322     c                   LEAVE
413700120322      *
413800120322      * Se invece è stato disaccreditato su un contratto precedente della
413900120322      *  stessa società
414000120322     c                   eLSeIF     tradfi > 0 and trasocG = d1SocG
414100120322     c                                         and traPIVA = d1IVA
414200120322      *
414300120322      *   deve memorizzarsi la data di DISACCREDITAMENTO per verificare
414400120322      *   solo le Tariffe maggiori e non considerare date precedenti
414500120322      *
414600120322     c                   eval         Data_Limite   = traDFI
414700120322     c                   LEAVE
414800120322      *
414900120322     c                   end
415000120322      *
415100120322     c     autista       readpe    aitra03l
415200120322     c                   endDO
415300100129     C*
415400120323     c                   endIF
415500120323      *
415600090522     C*  Se imposta la data di decorrenza
415700090522     c                   clear                   Data_decorr       8 0
415800090522     c                   clear                   Data_scade        8 0
415900090617      *
416000090522     c                   clear                   Data_trovata      1
416100090610     c                   clear                   Tar_Stampata      1
416200090617     c                   clear                   cod__TGTSOC
416300090617     c                   clear                   cod__TGTCDF
416400090522     C*
416500100129     C* esegue la ricerca della tariffa solo se non si trova nella condizione
416600100129     C* di essere stato disaccreditato precedentemente sullo stesso contratto
416700100129     c                   if        check_tariffa = 'S'
416800090522     C*  in immissione Contratto
416900090522     C* Routine di Controllo legata al video D4
417000090522      ** l'AUT impostato:
417100090522     c                   eval      TGTPDR = Autista
417200090522     c                   eval      TGTSML = ' '
417300120322      *
417400091006     c     Ktgt          setgt     fitgt01l
417500091006     c     Ktgt          readpe    fitgt01l
417600090522      *
417700090522     c                   Dow       not %EoF(fitgt01l)
417800090522     c                   If        tgtATB = *blank
417900090522      *
418000120322      *  Deve eseguire il controllo se NON ha preso alcun limite di DATA su
418100120322      *   precedenti Contratti SCADUTI
418200120323      *  oppure
418300120323      *   se ha trovato un contratto SCADUTO la tariffa da prendere in considerazione
418400120323      *   deve decorrere da DOPO la scadenza del contratto.
418500120323      ** oppure x accreditamento su Contratto SCADUTO deve prendere la Tariffa che SCADE
418600120323      **  entro la DATA di CHIUSURA CONTRATTO
418700120323      *
418800120323     c                   If           Data_Limite   = 0
418900120323     c                                   and su_ContrSCADUT = *blank
419000120323     c                                  or
419100120323     c                                Data_Limite   < tgtDDT
419200120323     c                                   and su_ContrSCADUT = *blank
419300120323      *- oppure x accred.to su Contratto SCADUTO
419400120323     c                                  or
419500120323     c                                  Data_Limite >= tgtDST
419600120323     c                                   and su_ContrSCADUT = 'S'
419700120323      *
419800120323$017 C                   IF        h4din < tgtdst
419900120323     c                                   and su_ContrSCADUT = *blank
420000120323     c                               or
420100120323      *- Oppure x Accreditamento su Contratto SCADUTO
420200120323$017 C                             h4din <= tgtddt
420300120323     c                                   and su_ContrSCADUT = 'S'
420400110510     c                   eval      Tariffa_trovata = 'S'
420500110510     c                   end
420600110510      *
420700090522      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
420800090522      *   a quella del contratto
420900090522     C                   move (p)  tgtCDF        parm_s4CDF                        Input
421000090522     C                   movel(p)  tgtSOC        parm_s4SOC                        Input/Output
421100090522     C                   movel(p)  tgtFIL        parm_s4FIL                        Input/Output
421200090522     c                   exsr      DECOD_P_IVA
421300090522      * se la P.IVA coincide
421400090522     c                   If        s4_errore ='0' and
421500090522     c                             s4_PARTITAIVA = PIVA_Confronto
421600091130      * controllo con anche la Società
421700091130     c                             and s4_SOCIETA = SOC_Confronto
421800091006     c                   eval        cod__TGTSOC  =  TGTSOC
421900091006     c                   eval        cod__TGTCDF  =  TGTCDF
422000091006      * periodo
422100091006     c                   eval      Data_decorr = tgtDDT
422200091006     c                   eval      Data_scade  = tgtDST
422300091006      * Tariffa Convalidata
422400091006     c                   if        tgtDTS > 0
422500091006     c                   eval        Tar_Stampata = 'S'
422600091006     c                   else
422700091006     c                   eval        Tar_Stampata = 'N'
422800091006     c                   end
422900090522      *   se congruente alla PArtita IVA
423000090522     c                   move      'S'           Data_trovata      1
423100091006     c********           leave
423200091006     c                   else
423300090522     c                   leave
423400090522     c                   end
423500090522      *
423600120322     c                   endIF
423700120322      *
423800090522     c                   end
423900091006     c     Ktgt          readpe    fitgt01l
424000090522     c                   endDo
424100090522      *
424200100129     c                   end
424300100129      *
424400090522      *  Se non ha trovato nulla pulisce la decorrenza ma lascia impostata
424500090522      *   l'ultima scadenza.
424600090522     c                   if        Data_trovata = *blank
424700090522     c                   clear                   Data_decorr
424800090617     c                   clear                   cod__TGTSOC
424900090617     c                   clear                   cod__TGTCDF
425000090522     c                   end
425100090522      *
425200090522     C                   ENDSR
425300090522     C************************************************************
425400090522      *? Controlla Tariffa su allegato tariffario che, se trovata, ?
425500090522      *?   NON sia decorrente prima del contratto.                 ?
425600090522     C************************************************************
425700090522     C     CHK_DEC_AUTAD BEGSR
425800100129     C*
425900100129     C*  Controlla che non vi sia un disaccreditamento sulla stessa società
426000100129     C*  precedente perchè l'autista è andato via per un periodo poi è tornato
426100100129     C*  sotto la stessa società. Se è così deve saltare il controllo della
426200100129     C*  tariffa.
426300110510     c                   eval      Tariffa_trovata = ' '
426400100129     c                   move      'S'           check_tariffa     1
426500120322     c                   clear                   Data_Limite       8 0
426600120322      **
426700120323      ** Solo nella PARTICOLARITA' che
426800120323      **  si stia eseguendo un accreditamento su UN CONTRATTO SCADUTO
426900120323     c                   IF           su_ContrSCADUT = 'S'
427000120323     c                   eval          Data_Limite = h2DFC
427100120323     c                   eLSe
427200120323      **
427300120323      **  Nella Normalità dei casi:
427400100129     c     autista       setgt     aitra05l
427500100129     c     autista       readpe    aitra05l
427600120322      **
427700120322     c                   dow       not %Eof(aitra05l)
427800120322      *
427900120322      *  Controlla la PIVA dal contratto
428000120322     c                   clear                   traPIVA
428100120322     C/EXEC SQL
428200120322     C+ SELECT  TRSIVA into :trapiva
428300120322     C+   FROM  AITRS00F
428400120322     C+  WHERE  TRSNRC = :tranrc
428500120322     C/END-EXEC
428600120322      *
428700120322      *  se disaccreditato e (società o PIVA diversa) DEVE SALTARE il controllo
428800120322     c                   if         tradfi > 0
428900120322     c                              AND
429000120322     c                                ( trasocG <> d1SocG
429100120322     c                                     or
429200120322     c                                  traPIVA <> d1IVA )
429300120509     c********           clear                   check_tariffa
429400120322     c                   LEAVE
429500120322      *
429600120322      * Se invece è stato disaccreditato su un contratto precedente della
429700120322      *  stessa società
429800120322     c                   eLSeIF     tradfi > 0 and trasocG = d1SocG
429900120322     c                                         and traPIVA = d1IVA
430000120322      *
430100120322      *   deve memorizzarsi la data di DISACCREDITAMENTO per verificare
430200120322      *   solo le Tariffe maggiori e non considerare date precedenti
430300120322      *
430400120322     c                   eval         Data_Limite   = traDFI
430500120322     c                   LEAVE
430600120322      *
430700120322     c                   end
430800120322      *
430900120322     c     autista       readpe    aitra05l
431000120322     c                   endDO
431100120322      *
431200120323     c                   endIF
431300120323      *
431400090522     C*  Se imposta la data di decorrenza
431500090522     c                   clear                   Data_decorr       8 0
431600090522     c                   clear                   Data_scade        8 0
431700090522     c                   clear                   Data_trovata      1
431800110506     c                   clear                   Tar_Stampata      1
431900090617     c                   clear                   cod__TGTSOC
432000090617     c                   clear                   cod__TGTCDF
432100090522     C*
432200100129     C* esegue la ricerca della tariffa solo se non si trova nella condizione
432300100129     C* di essere stato disaccreditato precedentemente sullo stesso contratto
432400100129     c                   if        check_tariffa = 'S'
432500090522     C*  in immissione Contratto
432600090522     C* Routine di Controllo legata al video D4
432700090522      ** l'AUT impostato:
432800091006     c     Autista       setgt     fiadt01l
432900091006     c     Autista       readpe    fiadt01l
433000090522     c                   Dow       not %EoF(fiadt01l)
433100090522     c                   If        adtATB = *blank
433200120322      *
433300120322      *  Deve eseguire il controllo se NON ha preso alcun limite di DATA su
433400120322      *   precedenti Contratti SCADUTI
433500120322      *  oppure
433600120322      *   se ha trovato un contratto SCADUTO la tariffa da prendere in considerazione
433700120322      *   deve decorrere da DOPO la scadenza del contratto.
433800120323      ** oppure x accreditamento su Contratto SCADUTO deve prendere la Tariffa che SCADE
433900120323      **  entro la DATA di CHIUSURA CONTRATTO
434000120323      *
434100120323     c                   If           Data_Limite   = 0
434200120323     c                                   and su_ContrSCADUT = *blank
434300120323     c                                  or
434400120322     c                                Data_Limite   < adtDDT
434500120323     c                                   and su_ContrSCADUT = *blank
434600120323      *- oppure x accred.to su Contratto SCADUTO
434700120323     c                                  or
434800120323     c                                  Data_Limite >= adtDST
434900120323     c                                   and su_ContrSCADUT = 'S'
435000090522      *
435100110510$017 C                   IF        h4din < adtdst
435200120323     c                                   and su_ContrSCADUT = *blank
435300120323     c                               or
435400120323      *- Oppure x Accreditamento su Contratto SCADUTO
435500120323$017 C                             h4din <= adtddt
435600120323     c                                   and su_ContrSCADUT = 'S'
435700110510     c                   eval      Tariffa_trovata = 'S'
435800110510     c                   end
435900110510      *
436000090522      *  con Società e Fornitore aggancia la P.IVA che deve essere uguale
436100090522      *   a quella del contratto
436200090522     C                   move (p)  adtCDF        parm_s4CDF                        Input
436300090522     C                   movel(p)  adtCSF        parm_s4SOC                        Input/Output
436400090522     C                   movel(p)  adtFIL        parm_s4FIL                        Input/Output
436500090522     c                   exsr      DECOD_P_IVA
436600090522      * se la P.IVA coincide
436700090522     c                   If        s4_errore ='0' and
436800090522     c                             s4_PARTITAIVA = PIVA_Confronto
436900091130      * controllo con anche la Società
437000091130     c                             and s4_SOCIETA = SOC_Confronto
437100091006     c                   eval        cod__TGTSOC  =  adtCSF
437200091006     c                   eval        cod__TGTCDF  =  adtCDF
437300091006      * periodo
437400091006     c                   eval      Data_decorr = adtDDT
437500091006     c                   eval      Data_scade  = adtDST
437600091006      * Tariffa Convalidata
437700091006     c                   if        adtDTS > 0
437800091006     c                   eval        Tar_Stampata = 'S'
437900091006     c                   else
438000091006     c                   eval        Tar_Stampata = 'N'
438100091006     c                   end
438200090522      *   se congruente alla PArtita IVA
438300090522     c                   move      'S'           Data_trovata      1
438400091006     c********           leave
438500091006     c                   else
438600091006     c                   leave
438700090522     c                   end
438800120322      *
438900120322     c                   end
439000090522      *
439100090522     c                   end
439200091006     c     Autista       readpe    fiadt01l
439300090522     c                   endDo
439400120322      *
439500120322     c                   end
439600100129      *
439700090522      *  Se non ha trovato nulla pulisce la decorrenza ma lascia impostata
439800090522      *   l'ultima scadenza.
439900090522     c                   if        Data_trovata = *blank
440000090522     c                   clear                   Data_decorr
440100090617     c                   clear                   cod__TGTSOC
440200090617     c                   clear                   cod__TGTCDF
440300090522     c                   end
440400090522      *
440500090522     C                   ENDSR
440600090507     C************************************************************
440700090507      *? VOLENDO chiudere il CONTRATTO controlla se presenti       ?
440800090507      *?  Autisti ancora ACCREDITATI.                              ?
440900090507     C************************************************************
441000090507     C     se_AUT_ATTIVI BEGSR
441100090507     C*
441200090507     C* Routine di Controllo legata al video D3 x DATA Fine contratto
441300090507     C*
441400090507      ** Tutti gli AUT legati al contratto
441500090507     C*   il primo che ha una tariffa ATTIVA fa uscire dal ciclo
441600090507     c     d2nrc         setll     aitra01l
441700090507     c     d2nrc         reade     aitra01l
441800090507     C*
4419000905071-   C                   dow       not %Eof(aitra01l)
442000090507     C*
442100090507      * è ancora accreditato.
442200090507     c                   if        traDFI = 0 and traANN = *blank
442300090507     c                   if        traKAU > 0 or traBPT > 0
442400090507     c                   eval      se_accreditato ='S'
442500090507     c                   leave
442600090507     c                   end
442700090507     c                   end
442800090507      *
442900090508      * è invece disaccreditato. Allora memorizza la data dell'ultimo AUT
443000090508      *   che è stato disaccreditato.
443100090508     c                   if        traDFI > 0 and traANN = *blank
443200090508     c                             and tradfi > ultimo_disaccr
443300090508     c                   if        traKAU > 0 or traBPT > 0
443400090508     c                   eval      ultimo_disaccr = tradfi
443500090508     c                   end
443600090508     c                   end
443700090508      *
443800090507     c     d2nrc         reade     aitra01l
443900090507     c                   endDO
444000090507     C*
444100090507     C                   ENDSR
444200090506     C************************************************************
444300090506      *? DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...   ?
444400090506     C************************************************************
444500090506     C     DEFCAM        BEGSR
444600090506     C*
444700090320     C* Variabili per gestione videate
444800090320     C                   MOVE      *BLANK        $GEST             2
444900090320     C                   MOVE      *BLANK        $FINE             1
445000090320     C                   MOVE      *BLANK        $INZD1            1
445100090320     C                   MOVE      *BLANK        $INZD2            1
445200090320     C                   MOVE      *BLANK        $INZD3            1
445300090320     C                   MOVE      *BLANK        $INZD4            1
445400160105     C                   MOVE      *BLANK        $INZD5            1
445500090320     C                   MOVE      *BLANK        $LASTG            2
445600090320     C                   MOVE      *BLANK        $LASTV            2
445700090320     C*
445800090320     C* Variabili appoggio
445900090320     C                   Z-ADD     0             CURR              2 0
446000090320     C                   Z-ADD     0             CURC              2 0
446100090320     C                   MOVE      *ZEROS        WIN              99
446200090320     C                   MOVE      *BLANK        WTBERR            1
446300090320     C*
446400090320     c     Kaut          klist
446500090320     c                   kfld                    tipo              1
446600090320     c                   kfld                    autista           7 0
446700090320     C*
446800090320     c     kTRS02        klist
446900090320     c                   kfld                    TRSSOCG
447000090320     c                   kfld                    TRSIVA
447100090320     c                   kfld                    TRSDFC
447200150624      *
447300150624     C     Ktrc          KLIST
447400150624     C                   KFLD                    trcIVA
447500150624     C                   KFLD                    trcPICF
447600090409     C*
447700090409     C     Krco98j       KLIST
447800090409     C                   KFLD                    RCOSOCIETA
447900090409     C                   KFLD                    subnatura         1
448000090409     C                   KFLD                    RCOKSC
448100090416     C*
448200090416     c     Ktab          klist
448300090416     c                   kfld                    tblkut
448400090416     c                   kfld                    tblcod
448500090416     c                   kfld                    tblkey
448600090416     c                   z-add     1             tblkut
448700090506     C*
448800090506     c     Ktgt          klist
448900090506     c                   kfld                    TgtPDR
449000090506     c                   kfld                    TgtSML
449100120217     C*
449200120217     c     K_TBE         klist
449300120217     c                   kfld                    tbeCOD
449400120217     c                   kfld                    tbeKE1
449500120217     c                   kfld                    tbeKE2
449600090320     C* Indici
449700090320     C                   Z-ADD     0             X                 3 0
449800090320     C                   Z-ADD     0             Y                 3 0
449900090320     C*
450000090320     C                   ENDSR
450100090317     C************************************************************
450200091215     C* Opz.di Cancellazione della Società Finestra di Conferma
450300091215     C************************************************************
450400091215     C     CONF_Cancella begSR
450500091215     C*
450600091215     c                   movel     Opz_Del       W4Fun
450700091215     c                   eval      w4PGM  =  D1PGM
450800091215     c                   eval      w4Ann  =  D1Ann
450900091215     c                   eval      w4Nrc  =  D1Nrc
451000091215     c                   eval      wh4Nrc  =  h2Nrc
451100091215     c                   clear                   W4MSG0
451200091215     c                   clear                   W4MSG1
451300091215     c                   clear                   W4MSG2
451400091215     c                   clear                   W4MSG3
451500091215     c                   clear                   W4MSG4
451600091215      *
451700091215     c                   eval      W4MSG1 = 'L''annullamento della società farà-
451800091215     c                              annullare tutti gli'
451900091215     c                   eval      W4MSG2 = 'autisti ad essa collegati'
452000091215     c                   eval      W4MSG3 = 'F6 Conferma l''Annullamento / -
452100091215     c                             F12 per NON fare Nulla'
452200091215     C*
452300091215     c* controlla se ci sono autisti ancora accreditati
452400091215     c     wh4nrc        setll     aitra01l
452500091215     c     wh4nrc        reade     aitra01l
452600091215      *
452700091215     c                   Dow       not %EoF(aitra01l)
452800091215     c                   if        traDFI = *zero
452900091215      *
453000091215     c                   eval      W4MSG0 = 'Attenzione: ci sono autisti ancora-
453100091215     c                              accreditati !!!'
453200091215    >C                   leave
453300091215     c                   end
453400091215     c     wh4nrc        reade     aitra01l
453500091215     c                   endDO
453600091215     C*
453700091215     C*                *-------------------*
453800091215     c                   exfmt     DELwind
453900091215     C*                *-------------------*
454000091215     C*
454100091215     C                   ENDSR
454200091215     C************************************************************
