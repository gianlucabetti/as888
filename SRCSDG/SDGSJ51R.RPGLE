000100100705     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO)
000200100705     H*PARMS BNDDIR(PJXBND PJCBND)
000300100705     H*PARMS ACTGRP(QILE)
000400100705      *****************************************************************
000500100705     H DECEDIT('0,') DATEDIT(*DMY.)
000600100705      *****************************************************************
000700100705      *
000800141229      *                Estrazione dichiarazioni d'intento
000900141229      *                     fornitori
001000100705      *           -------------------------------------
001100100705      *
001200100705      *****************************************************************
001300100705      *------------
001400141229     FSDGSJ51D  CF   E             WORKSTN
001500141229     F                                     RENAME(SJ51T1:FMTT1)
001600141229     F                                     RENAME(SJ51D1:FMTD1)
001700141229     F                                     RENAME(SJ51Z1:FMTZ1)
001800100705     F* modifica valori assunti
001900141229     F                                     RENAME(SJ51D3:FMTD3)
002000141229     F                                     RENAME(SJ51Z3:FMTZ3)
002001160530R224 F                                     RENAME(SJ51D4:FMTD4)
002002160530R224 F                                     RENAME(SJ51Z4:FMTZ4)
002100100705     F                                     INFDS(DSFMT)
002200100705      *****************************************************************
002300100705     D* Schiere per la gestione dei tasti di funzione
002400100705     D $FM             S             27    DIM(24)
002500100705     D $FX             S             50    DIM(24)
002600100705     D $FC             S              1    DIM(24)
002700100705     D $FR             S              1    DIM(24)
002800100705     D* Video D1
002900100705     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003000100705     D $FX1            S             50    DIM(24) ALT($FM1)
003100100705     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003200100705     D $FR1            S              1    DIM(24) ALT($FC1)
003300100705     D* Video D3 Valori assunti
003400100705     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
003500100705     D $FX3            S             50    DIM(24) ALT($FM3)
003600100705     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
003700100705     D $FR3            S              1    DIM(24) ALT($FC3)
003701160530R224 D* Video D4 Valori assunti
003702160530 "   D $FM4            S             27    DIM(24) CTDATA PERRCD(1)
003703160530 "   D $FX4            S             50    DIM(24) ALT($FM4)
003704160530 "   D $FC4            S              1    DIM(24) CTDATA PERRCD(1)
003705160530R224 D $FR4            S              1    DIM(24) ALT($FC4)
003800100705     D*-------------
003900100705     D* Passaggio Parametri
004000100705     D KPJBA         E DS
004100100705     D*-------------
004200100705     D* Valori assunti
004300100705     D XMPEDS        E DS                  INZ
004400100705     D XMPEDS1       E DS                  INZ
004500100705     D DSMPE           DS                  INZ
004600100705     D  $VAL                          1
004700100820     D $OkExeLan       S              1
004800100820     D $P1COS2161      S             20A
004900100830     D $P1PRO2184      S             10A
005000100820     D*----------------
005100100820     D* ds per nome file
005200100820     D                 DS
005300100820     D  WAnno                  3      6  0
005400100820     D  WPrgInvio              9     10  0
005500100820     D  WNumero                1     10
005600100705     D*-------------
005700100705     D* Attributi video
005800100705     D Normale         C                   Const(X'32')
005900100705     D Protetto        C                   Const(X'A0')
006000100705     D*-------------
006100100705     D* Reperimento nome PGM
006200100705     D STATUS         SDS           333
006300100705     D  DSPGM            *PROC
006400100705     D  PARMS            *PARMS
006500100705     D*-------------
006600100705     D* Dati di ambiente ottenuti da SDGSOCDR
006700100705     D SOC001        E DS                  EXTNAME(SDGSOCDS )
006800100705     D*-------------
006900100705     D* DS Interna per dati di output di SDGSOCDR
007000100705     D XSOCDS          DS          1000
007100100820     D*-------------
007200100820     D* DS Esterna gestione messaggi
007300100820     D SDGMSGDS      E DS                  INZ
007400100820     D  STS          E                     EXTFLD(MSGSTS)
007500100820     D                                     DIM(333)
007600100820     D  JBA          E                     EXTFLD(MSGJBA)
007700100820     D                                     DIM(502)
007800100705     D*-------------
007900100705     D DSFMT           DS
008000100705     D  $TASTO               369    369
008100100705     D  NRG                  370    370
008200100705     D  NCL                  371    371
008300100705     D*-------------
008400100705     D* posizione cursore
008500100705     D CURSOR          DS
008600100705     D  RIRI                   1      2B 0 INZ
008700100705     D  R$$                    2      2
008800100705     D  COCO                   3      4B 0 INZ
008900100705     D  C$$                    4      4
008901160530R224 D*-------------
008902160530 "   D DSDatiAna       DS                  INZ
008911160530R224 D $Dest                               Like(D4Dest   )
009300100827     D*-------------
009400100827     D* Parametri modulo SCARSUMA
009500100827     D ANP022DS      E DS
009600100705     D*-------------
009700100705     D* Passaggio Parametri al PGM BATCH
009800141229     D SDGSJ52DS     E DS
009900141229     D  RETJ52       E                     INZ('0')
010000141229     D  OPRJ52       E                     INZ('0')
010100141229     D  ERRJ52       E                     INZ('0')
011200100705     D*-------------
011300100705     D* Tasti di funzione
011400100705     D F01             C                   CONST(X'31')
011500100705     D F02             C                   CONST(X'32')
011600100705     D F03             C                   CONST(X'33')
011700100705     D F04             C                   CONST(X'34')
011800100705     D F05             C                   CONST(X'35')
011900100705     D F06             C                   CONST(X'36')
012000100705     D F07             C                   CONST(X'37')
012100100705     D F08             C                   CONST(X'38')
012200100705     D F09             C                   CONST(X'39')
012300100705     D F10             C                   CONST(X'3A')
012400100705     D F11             C                   CONST(X'3B')
012500100705     D F12             C                   CONST(X'3C')
012600100705     D F13             C                   CONST(X'B1')
012700100705     D F14             C                   CONST(X'B2')
012800100705     D F15             C                   CONST(X'B3')
012900100705     D F16             C                   CONST(X'B4')
013000100705     D F17             C                   CONST(X'B5')
013100100705     D F18             C                   CONST(X'B6')
013200100705     D F19             C                   CONST(X'B7')
013300100705     D F20             C                   CONST(X'B8')
013400100705     D F21             C                   CONST(X'B9')
013500100705     D F22             C                   CONST(X'BA')
013600100705     D F23             C                   CONST(X'BB')
013700100705     D F24             C                   CONST(X'BC')
013800100705     D ENTER           C                   CONST(X'F1')
013900100705     D ROLDWN          C                   CONST(X'F4')
014000100705     D ROLLUP          C                   CONST(X'F5')
014100100705     D*-------------
014200100705     D* Sottotitolo di testata
014300100706     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
014400100705     D                                          SDC0053')
014500141229     D*-------------
014600141229     D* Sottotitolo di testata
014700141229     D* "Fornitori"
014800141229     D SOTTITT1        C                   CONST('PROMSG    *LIBL-
014900141229     D                                          COS0127')
015300100705     D*-------------
015400131004     D* Maschera dati autorizzazioni
015500100705     D CONCIVDS      E DS
015600100705     D*-------------
015700100705     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
015800100705     D*-------------
015900100705     D* Passaggio Autorizzazioni
016000100705     D DSAUT           DS           512
016100100705     D*-------------
016200100705     D* Dati per pgm XNOAUT
016300100705     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
016400100705     D*-------------
016500100705     D* Ricerca legende
016600100705     D Ana964DS      E DS
017300100705     D*-------------
017400100705     D* Parametri per ricerca codice ANATB00F
017500100705     D XTabDS        E DS                  inz
017600100705     D*-------------
017700100705     D* Controlli validità legende
017800100705     D X07ValDS      E DS
017900100705     D*-------------
018000100705     D* Richiamo a SDGTABDR
018100100705     D SDGTABDS      E DS                  inz
018101160530R224 D*---------
018102160530 "   D* tabella destinazione dati aggiuntivi
018103160530R224 D SDGHC4DS      E DS                  INZ
018200100705     D*-------------
018300100705     D* DS esterna per dati soggetti
018400100705     D DVASOG        E DS                  INZ
018500100705     D*-------------
018600100705     D* Definizioni parametri driver anagrafici
018700100705     D Dvaric          S              1
018800100705     D DvaSocieta      S              3
018900100705     D DvaUnita        S              8
019000100705     D DvaStrutt       S             10
019100100705     D DvaDtRif        S               D
019200100705     D DvaLenOut       S              9B 0
019300100705     D DvaSnatura      S              1
019400100705     D DvaCodice       S              8
019500100705     D DvaLineav       S              3
019600100705     D DvaFiliale      S              3  0
019700100705     D DvaSogg         S              8
019800100705     D DvaTpInd        S              2
019900100705     D DvaCdInd        S              3
020000100705     D DvaErrore       S              1
020100100705     D DvaOutput       S           4000
020200100705     D*-------------
020300100705     D* Controllo conti
020400100705     D ND002DS       E DS                  INZ
020500100705     D*-------------
020600100705     D* Definizioni parametri standard chiamate moduli
020700100705     D Esito           S              1
020800100705     D GesMsg          S              1
020900100705     D LenOut          S              9B 0
021000100705     D Data            S             10
021100100705     D StrutturaO      S             10
021200100705     D*-------------
021300100705     D* ricerca soggetti
021400100705     D ANA040DS      E DS                  INZ
022200100706     D*-------------
023100100706     D* Passaggio Parametri al PGM fornitori
023200100706     D ANA400DS      E DS
023300100706     D  CMT400       E                     INZ('0')
023400100706     D  RET400       E                     INZ('0')
023500100706     D  OPR400       E                     INZ('0')
023600100706     D  ERR400       E                     INZ('0')
023700100706     D  FSC400       E                     INZ('S')
023800100705     D*-------------
023900100705     D WSogg           S                   Like(DVSSogg)
024000100705     D WKcc            S              6A
024100100705     D WKsc            S              8A
024200100705     D*-------------
024300100705     D UDateISO        S               D    datfmt(*iso)
024700100705     C******************************************************
024800100705     C*                MAIN LINE PROGRAM
024900100705     C******************************************************
025000100705     C*
025100100705     C* Inizializzazioni
025200100705     C                   EXSR      INZVAR
025300100705     C*
025400100705     C     $FINE         DOWEQ     *OFF
025500100705     C     $GEST         CASEQ     'D1'          GESD1
025600100705     C     $GEST         CASEQ     'D3'          GESD3
025601160530R224 C     $GEST         CASEQ     'D4'          GESD4
025700100705     C                   ENDCS
025800100705     C                   ENDDO
025900100705     C*
026000100705     C* fine programma
026100100705     C                   EXSR      ENDPGM
026200100705     C/EJECT
026300100705     C************************************************************
026400100705     C* Fine pgm
026500100705     C************************************************************
026600100705     C     ENDPGM        BEGSR
026700100705     C*
026800100705     C* se ho confermato l'uscita proseguo
026900100705     C     $FINE         IFEQ      *ON
027000100705     C*
027100100705     C                   SETON                                            LR
027200100705     C                   RETURN
027300100705     C*
027400100705     C                   ENDIF
027500100705     C*
027600100705     C                   ENDSR
027700100705     C/EJECT
027800100705     C************************************************************
027900100705     C* RICERCHE
028000100705     C************************************************************
028100100705     C     SEARCH        BEGSR
028200100705     C*
028300100705     C* determino Riga/Colonna del cursore
028400100705     C                   MOVE      NRG           R$$
028500100705     C                   MOVE      NCL           C$$
028600100705     C                   Z-ADD     RIRI          CURR
028700100705     C                   Z-ADD     COCO          CURC
028800100705     C*
028900100705     C                   EXSR      CallAllinea
029000100705     C*
029100100705     C                   SELECT
030800100705     C*
030900100705     C*  Codice IVA
031000110422     C     H1NMFL        WHENEQ    'D1PARTIVA'
031100110422     C                   EXSR      F4_PartIVA
031200100705     C*
031300100705     C*  Codice fiscale
031400100705     C     H1NMFL        WHENEQ    'D1CDFISC'
031500100705     C                   EXSR      F4_CdFisc
031600100705     C*
031700100705     C*  Soggetto
031800100705     C     H1NMFL        WHENEQ    'D1SOGG'
031900100705     C                   EXSR      F4_Sogg
033400100706     C*
033500141229     C*  Fornitore
033600100706     C     H1NMFL        WHENEQ    'D1KSC'
033800141229     C                   EXSR      F4_Ksc
033801141229     C*
033802141229     C* FMTD1 - tipo operazione
033803141229     C     H1NMFL        WHENEQ    'D1TPOPE '
033804141229     C                   EXSR      F4_TpOpe
033805150112     C*
033806150112     C* FMTD1 - tipo cancellazione movimenti
033807150112     C     H1NMFL        WHENEQ    'D1DELETE'
033808150112     C                   EXSR      F4_Delete
033809160530R224 C*
033810160530 "   C* FMTD4 - Destinazione dati anagrafici aggiuntivi
033811160530 "   C     H1NMFL        WHENEQ    'D4DEST   '
033812160530R224 C                   EXSR      F4_Dest
034400100705     C*
034500100705     C* ricerca non abilitata in questa posizione
034600100705     C                   OTHER
034700100705     C* imposto pos. del cursore
034800100705     C                   Z-ADD     CURR          H1RIGA
034900100705     C                   Z-ADD     CURC          H1COLO
035000100705     C                   SETON                                        9899
035100100705     C*
035200100705     C                   ENDSL
035300100705     C*
035400100705     C                   MOVE      *HIVAL        $LASTV
035500100705     C*
035600100705     C                   ENDSR
035700100705      /EJECT
035800100705     C************************************************************
035900100705     C* Richiamo programma per allineare i codici
036000100705     C************************************************************
036100100705     C     CallAllinea   BEGSR
036200100705     C*
036300100705     C     D1SOGG        IFNE      *BLANK
036400100705     C                   CALLb     'XALLINEA'
036500100705     C                   PARM                    D1SOGG
036600100705     C                   PARM      8             LEN               5 0
036700100705     C                   PARM                    XSCINP
036800100705     C                   ENDIF
041100100705     C*
041200100705     C     D1KSC         IFNE      *BLANKS
041300100705     C                   CALLB     'XALLINEA'
041400100705     C                   PARM                    D1KSC
041500100705     C                   PARM      8             LEN               5 0
041600100705     C                   PARM                    XSCINP
041700100705     C                   ENDIF
041800100705     C*
041900100705     C                   ENDSR
042000100705      /EJECT
051700100705     C************************************************************
051800100705     C* Ricerca codice IVA
051900100705     C************************************************************
052000110422     C     F4_PartIVA    BEGSR
052100100705     C*
052200100705     C                   RESET                   ANA040DS
052300100705     C*
052400100705     C                   MOVEL     '01'          OPZ040
052500110422     C                   MoveL     D1PartIVA     IVA040
052600100705     C                   MOVEL     ANA040DS      KPJBU
052700100705     C                   CALL      'ANA040R'
052800100705     C                   PARM                    KPJBA
052900100705     C*
053000100705     C                   MOVEL     KPJBU         ANA040DS
053100100705     C*
053200100705     C                   if        Ret040     <> '2'
053300100705     C*
053400100705     C                   MoveL     Cod040        WSogg
053500100705     C                   EXSR      CallDvaSog
053600100705     C*
053700100705     C                   if        DVAErrore  <> '1'
053800100705     C*
053900100705     C                   Eval      %subst(DVASOG:1:DVALENOUT)
054000100705     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
054100100705     C*
054200110422     C                   MoveL     DVSPartIVA    D1PartIVA
054300100705     C*
054400100705     C                   endif
054500100705     C*
054600100705     C                   endif
054700100705     C*
054800100705     C                   ENDSR
054900100705      /EJECT
055000100705     C************************************************************
055100100705     C* Ricerca codice fiscale
055200100705     C************************************************************
055300100705     C     F4_CdFisc     BEGSR
055400100705     C*
055500100705     C                   RESET                   ANA040DS
055600100705     C*
055700100705     C                   MOVEL     '01'          OPZ040
055800100705     C                   MoveL     D1CdFisc      CdF040
055900100705     C                   MOVEL     ANA040DS      KPJBU
056000100705     C                   CALL      'ANA040R'
056100100705     C                   PARM                    KPJBA
056200100705     C*
056300100705     C                   MOVEL     KPJBU         ANA040DS
056400100705     C*
056500100705     C                   if        Ret040     <> '2'
056600100705     C*
056700100705     C                   MoveL     Cod040        WSogg
056800100705     C                   EXSR      CallDvaSog
056900100705     C*
057000100705     C                   if        DVAErrore  <> '1'
057100100705     C*
057200100705     C                   Eval      %subst(DVASOG:1:DVALENOUT)
057300100705     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
057400100705     C*
057500100705     C                   MoveL     DVSCdFisc     D1CdFisc                       *
057600100705     C*
057700100705     C                   endif
057800100705     C*
057900100705     C                   endif
058000100705     C*
058100100705     C                   ENDSR
058200100705     C/EJECT
058300100705     C************************************************************
058400100705     C* Ricerca codice soggetto
058500100705     C************************************************************
058600100705     C     F4_Sogg       BEGSR
058700100705     C*
058800100705     C                   RESET                   ANA040DS
058900100705     C*
059000100705     C                   MOVEL     '01'          OPZ040
059100100705     C                   MoveL     D1Sogg        Cod040
059200100705     C                   MOVEL     ANA040DS      KPJBU
059300100705     C                   CALL      'ANA040R'
059400100705     C                   PARM                    KPJBA
059500100705     C*
059600100705     C                   MOVEL     KPJBU         ANA040DS
059700100705     C*
059800100705     C                   if        Ret040     <> '2'
059900100705     C                   MoveL     Cod040        D1Sogg
060000100705     C                   endif
060100100705     C*
060200100705     C                   ENDSR
060300100705     C/EJECT
069000100706     C************************************************************
069100100706     C* Ricerca fornitore di estrazione
069200100706     C************************************************************
069300141229     C     F4_Ksc        BEGSR
069400100706     C*
069500100706    >C                   RESET                   ANA400DS
069600100706     C*
069700100706    >C                   MOVEL     '01'          OPZ400
069800100706    >C                   MOVEL     D1Ksc         KSC400
069900100706     C*
070000100706    >C                   MOVEL     ANA400DS      KPJBU
070100100706     C*
070200100706    >C                   CALL      'ANA400R'                            21
070300100706     C                   PARM                    KPJBA
070400100706     C*
070500100706    >C                   MOVEL     KPJBU         ANA400DS
070600100706     C*
0707001007062   >C     KSC400        IFNE      *BLANK
070800100706    >C                   MOVE      KSC400        D1Ksc
0709001007062-  >C                   ENDIF
071000100705     C*
071100100705     C                   ENDSR
071200100705     C/EJECT
071201141229     C************************************************************
071202141229     C* Ricerca tipo operazione
071203141229     C************************************************************
071204141229     C     F4_TpOpe      BEGSR
071205141229     C*
071206141229     C                   reset                   ana964ds
071207141229     C*
071208141229     C                   movel     '01'          Opz964
071209150112     C                   MoveL     'SDGSJ52S '   Trc964
071210150112     C                   MoveL     'TPOPEJ52 '   Cam964
071211141229     C                   movel     *On           All964
071212141229     C*
071213141229     C                   movel     ana964ds      Kpjbu
071214141229     C                   call      'ANA964R'
071215141229     C                   parm                    Kpjba
071216141229     C                   movel     Kpjbu         Ana964ds
071217141229     C*
071218141229     C                   if        Ret964     <> '2'
071219141229     C                   movel     Val964        D1TpOpe
071220141229     C                   endif
071221141229     C*
071222141229     C                   ENDSR
071223141229     C/EJECT
071224150112     4************************************************************
071225150112     C* Ricerca tipo cancellazione movimenti
071226150112     C************************************************************
071227150112     C     F4_Delete     BEGSR
071228150112     C*
071229150112     C                   reset                   ana964ds
071230150112     C*
071231150112     C                   movel     '01'          Opz964
071232150112     C                   MoveL     'SDGSJ52S '   Trc964
071233150112     C                   MoveL     'DELETEJ52'   Cam964
071234150112     C                   movel     *On           All964
071235150112     C*
071236150112     C                   movel     ana964ds      Kpjbu
071237150112     C                   call      'ANA964R'
071238150112     C                   parm                    Kpjba
071239150112     C                   movel     Kpjbu         Ana964ds
071240150112     C*
071241150112     C                   if        Ret964     <> '2'
071242150112     C                   movel     Val964        D1Delete
071243150112     C                   endif
071244150112     C*
071245150112     C                   ENDSR
071246150112     C/EJECT
071247160530R224 C************************************************************
071248160530 "   C* Ricerca valori per Destinazione dati anagrafici aggiuntivi
071249160530 "   C************************************************************
071250160530R224 C     F4_Dest       BEGSR
071251160530     C*
071252160530     C*
071253160530     C                   RESET                   XTABDS
071254160530     C*
071255160530     C                   MOVE      '01'          XTAOPZ
071256160530     C                   MOVE      XSCSOC        XTAAZI
071257160530     C                   MOVE      XSCLIN        XTALIN
071258160530     C                   Z-ADD     CURR          XTARIG
071259160530     C                   Z-ADD     CURC          XTACOL
071260160530     C                   CLEAR                   KPJBU
071261160530     C                   MOVEL     XTABDS        KPJBU
071262160530     C                   CALL      'SDGHC4R'
071263160530     C                   PARM                    KPJBA
071264160530     C                   MOVEL     KPJBU         XTABDS
071265160530     C*
071266160530     C                   if        XTaErr     <> '1'  and
071267160530     C                             XTaKey     <> *Blank
071268160530     C                   Move      XTaKey        D4Dest
071269160530     C                   endif
071270160530     c*
071271160530R224 C                   ENDSR
071272160530R224 C/EJECT
071300100705     C************************************************************
071400100705     C* GESTIONE EMISSIONE TASTI FUNZIONE
071500100705     C************************************************************
071600100705     C     GESPIE        BEGSR
071700100705     C*
071800100705     C                   SELECT
071900100705     C*
072000100705     C     $GEST         WHENEQ    'D1'
072100100705     C     $ULKD1        IFEQ      0
072200100705     C* riempimento opzioni
072300100705     C                   EXSR      F24D1
072400100705     C                   ELSE
072500100705     C                   WRITE     FMTZ1
072600100705     C                   ENDIF
072700100705     C*
072800100705     C* Modifica valori assunti
072900100705     C     $GEST         WHENEQ    'D3'
073000100705     C     $ULKD3        IFEQ      0
073100100705     C                   EXSR      F24D3
073200100705     C                   ELSE
073300100705     C                   WRITE     FMTZ3
073400100705     C                   ENDIF
073401160530R224 C*
073402160530 "   C* Dati aggiuntivi anagrafici
073403160530 "   C     $GEST         WHENEQ    'D4'
073405160530 "   C     $ULKD4        IFEQ      0
073406160530 "   C                   EXSR      F24D4
073407160530 "   C                   ELSE
073408160530 "   C                   WRITE     FMTZ4
073409160530 "   C                   ENDIF
073410160530R224 C*
073500100705     C                   ENDSL
073600100705     C*
073700100705     C                   ENDSR
073800100705     C/EJECT
073900100705     C************************************************************
074000100705     C* GESTIONE VIDEO RECORD D1
074100100705     C************************************************************
074200100705     C     GESD1         BEGSR
074300110422     C*
074400100705     C* inizializzazione videata
074500100705     C     $INZD1        IFEQ      *ON
074600100705     C                   EXSR      INZD1
074700100705     C                   MOVE      *OFF          $INZD1
074800100705     C                   ENDIF
074900100705     C* emissione piede videata se proveniente da altra
075000100705     C* salvataggio valore formato di provenienza
075100100705     C     $LASTV        IFNE      'D1'
075200100705     C                   WRITE     FMTT1
075300100705     C                   EXSR      GESPIE
075400100705     C                   MOVE      $LASTV        $LASTG
075500100705     C                   MOVE      'D1'          $LASTV
075600100705     C                   ENDIF
075700100705     C*
075800100705     C* se esistono errori sulla videata
075900100705     C* emetto la write del formato a indicatori spenti per vedere
076000100705     C* le eventuali decodifiche
076100100705     C     *IN99         IFEQ      *ON
076200100705     C     $GEST         ANDEQ     'D1'
076300100705     C                   MOVEA     *IN           WIN
076400100705     C                   MOVE      *ALL'0'       IN5098           49
076500100705     C                   MOVEA     IN5098        *IN(50)
076600100705     C                   WRITE     FMTD1
076700100705     C                   MOVEA     WIN           *IN
076800100705     C                   ENDIF
076900100705     C*
077000100705     C                   EXFMT     FMTD1
077100100705     C*
077200100705     C* controllo abilitazione tasti funzione
077300100705     C                   EXSR      CTRKD1
077400100705     C*
077500100705     C     *IN99         IFEQ      *OFF
077600100705     C*
077700100705     C                   SELECT
077800100705     C* F2=Decodifica
077900100705     C     $TASTO        WHENEQ    F02
078000100705     C                   EXSR      F02D1
078100100705     C* F3=Fine
078200100705     C     $TASTO        WHENEQ    F03
078300100705     C                   EXSR      F03D1
078400100705      * F4=Ricerca
078500100705     C     $TASTO        WHENEQ    F04
078600100705     C                   EXSR      SEARCH
078700100705     C* F5=Refresh
078800100705     C     $TASTO        WHENEQ    F05
078900100705     C                   EXSR      F05D1
079000100705     C* F12=Ritorno
079100100705     C     $TASTO        WHENEQ    F12
079200100705     C                   EXSR      F12D1
079300110422     C* F14=Regole per estrazione movimenti
079400100705     C     $TASTO        WHENEQ    F14
079500100705     C                   EXSR      F14D1
079600100705     C* F18=Valori assunti
079700100705     C     $TASTO        WHENEQ    F18
079800100705     C                   EXSR      F18D1
080200100705     C* F21=Pers. batch
080300100705     C     $TASTO        WHENEQ    F21
080400100705     C                   EXSR      F21D1
080401160530R224 C* F22=Destinazione dati anagrafici
080402160530 "   C     $TASTO        WHENEQ    F22
080403160530R224 C                   EXSR      F22D1
080500100705     C* F24=Altri tasti
080600100705     C     $TASTO        WHENEQ    F24
080700100705     C                   EXSR      F24D1
080800100705     C*
080900100705     C                   OTHER
081000100705     C                   EXSR      CTRD1
081100100705     C*
081200100705     C     $TASTO        IFEQ      F06
081300100705     C     *IN99         ANDEQ     *OFF
081400100820     C*
081500100820     C* Controllo se esiste già il file con il nome reperito
081600100820     C* Se lo trovo,
081700100820     C* segnalo che il file di spedizione per il periodo
081800100820     C* in esame è stato trovato nel catalogo e che pertanto
081900100820     C* potrebbe essere già stato spedito
082000100820     C                   EXSR      CtrFileTel
082100100820     C*
082200100820     C                   if        $OkExeLan   = *On
082300100705     C                   EXSR      EXELAN
082400100705     C                   EXSR      Endpgm
082500100820     C                   endif
082600100820     C*
082700100705     C                   ENDIF
082800100705     C*
082900100705     C                   ENDSL
083000100705     C*
083100100705     C                   ENDIF
083200100705     C*
083300100705     C                   ENDSR
083400100705     C/EJECT
083500100820     C************************************************************
083600100820     C* Verifico esistenza file con stesso nome
083700100820     C************************************************************
083800100820     C     CtrFileTel    BegSr
083900100820     C*
084000100820     C* Reperimento nome file telematico
084100100820     C                   EXSR      RepNomeF
084200100820     C*
084300100820     C*  CHKOBJ     OBJ($LIBRF/&FILE) OBJTYPE(*FILE)
084400100820     C                   Eval      Cmd         =
084500100820     C                             'CHKOBJ OBJ(' +
084600100820     C                             %Trim(Lib022) + '/' +
084700100820     C                             %Trim($File)  +
084800100820     C                             ') ' +
084900100820     C                             'OBJTYPE(*FILE)'
085000100820     C*
085100100820     C                   Call      'QCMDEXC'                            21
085200100820     C                   Parm                    Cmd             120
085300100820     C                   Parm      120           LenCmd           15 5
085400100820     C*
085500100820     C*
085600100820     C                   Eval      $OkExeLan   = *Off
085700100820     C*
085800100820     C* se il file è presente nella libreria lo segnalo
085900100820     C                   If        *In21       = *Off
086000100820     C*
086100100820     C                   EXSR      MsgErr
086200100820     C*
086300100820     C                   Select
086400100820     C*
086500100820     C* richiesto di continuare
086600100820     C                   When      MsgRtn      = '1'
086700100820     C                   Eval      $OkExeLan   = *On
086800100820     C*
086900100820     C*
087000100820     C* richiesto di non continuare
087100100820     C                   When      MsgRtn      = '2'
087200100820     C                   Eval      $OkExeLan   = *Off
087300100820     C*
087400100820     C                   endsl
087500100820     C*
087600100820     C                   else
087700100820     C* se file non presente nella libreria, eseguo lancio
087800100820     C                   Eval      $OkExeLan   = *On
087900100820     C*
088000100820     C                   endIf
088100100820     C*
088200100820     C                   ENDSR
088300100820     C/EJECT
088400100820     C************************************************************
088500100820     C* Reperimento nome file telematico
088600100820     C************************************************************
088700100820     C     RepNomeF      BEGSR
088800100820     C*
088900100820     C                   Eval      WNumero     = *All'0'
089000100820     C                   Z-Add     D1Anno        WAnno
089100100820     C                   Z-Add     0             WPrgInvio
089200100820     C                   Move      WNumero       $Numero
089300100820     C*
089400100820     C                   CALL      'X35NOMFR'
089500100820     C                   PARM                    XScSoc
089600100820     C                   PARM      *Blank        $Unita            8
089700110422     C                   PARM      'E3'          $CdScarico        2
089800100820     C                   PARM                    $Numero           9
089900100820     C                   PARM                    $File            10
090000100820     C                   PARM                    Lib022
090100100820     C                   PARM      *Off          $Errore           1
090200100820     C*
090300100820     C                   if        $Errore    <> '0'
090400100820     C                   Seton                                        9499
090500100820     C                   endif
090600100820     C*
090700100820     C                   ENDSR
090800100820     C/EJECT
090900100820     C************************************************************
091000100820     C* Messaggio errore
091100100820     C************************************************************
091200100820     C     MsgErr        BEGSR
091300100820     C*
091400100820     C                   Reset                   SDGMSGDS
091500100820     C*
091600100820     C* Risposte possibili
091700100820     C* "1) Continuare      2) Annulla
091800100820     C                   Eval      IDMSG       =
091900100820     C                             'PROMSG    *LIBL     COS1204'
092000100820     C                   CALLB     'XRTVM'
092100100820     C                   PARM                    IDMSG
092200100820     C                   PARM      *Blank        TXTMSG
092300100820     C                   PARM                    ERRMSG
092400100820     C     ERRMSG        IFNE      *ON
092500100820     C                   MoveL     TXTMSG        XMSGTXT
092600100820     C                   ELSE
092700100820     C                   MoveL     *ALL'?'       XMSGTXT
092800100820     C                   ENDIf
092900100820     C*
093000100820     C                   MoveL     'S'           MSGEMV
093100100820     C                   Z-Add     10            MSGRGP
093200100820     C                   MoveL     'S'           MSGRSP
093300100820     C                   MoveL     '12'          MSGTPR
093400100820      *
093500100820     C                   Movel     $File         $P1COS2161
093600100820     C                   Move      Lib022        $P1COS2161
093700100820      *
093800100820     C                   call      'SDGMSGR'
093900100820     C                   parm                    SDGMSGDS
094000100820     C                   parm                    XMsgTxt         135
094100100820     C*
094200100820     C* Modo di inizializzazione della window:
094300100820     C* ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
094400100820     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
094500100820     C*                       per mantenere il sottofondo del video princip.)
094600100820     C                   parm      '1'           XMsgInz           1
094700100820     C*
094800100820      * COS2161-Il file &1 esiste già nella libreria &2'
094900100820     C                   parm      'COS2161'     XMsgCd1           7
095000100820     C                   parm                    XMsgCd2           7
095100100820     C                   parm                    XMsgCd3           7
095200100820     C                   parm                    XMsgSn1           7
095300100820     C                   parm                    XMsgCdTxT        63
095400100820     C                   parm                    MsgCdTit         14
095500100820     C                   parm                    MsgClPrtf         1
095600100820     C                   parm      $P1COS2161    VarMsg1         100
095700100820     C                   parm                    VarMsg2         100
095800100820     C                   parm                    VarMsg3         100
095900100820     C*
096000100820     C                   ENDSR
096100100820     C/EJECT
096200100705     C************************************************************
096300100705     C* INIZIALIZZAZIONE VIDEATA DATI
096400100705     C************************************************************
096500100705     C     INZD1         BEGSR
096600100705     C*
096700100705     C                   CLEAR                   FMTD1
096800100705     C*
097600100723     C                   Eval      D1On1       = XScOn
097700100723     C                   Eval      D1Off1      = XScOff
098000100705     C*
098100100705     C* Se vuole impostazione lancio da ultima elaborazione, li imposto da anmpe
098200100705     C     $Val          IFEQ      *ON
098300100705     C* riempio la videata da parametri memorizzati
098400100705     C                   EXSR      RIED1
098500100705     C*
098600100705     C                   ELSE
098700141229     C                   Eval      D1Anno      = *Year
099300150112     C                   Eval      D1Delete    = '1'
099400150108     C                   Eval      D1StpAnn    = XScOn
099500100705     C                   endIf
099600100705     C*
099700100705     C* richiamo routine dei ctrl per decodificarla
099800100705     C                   EXSR      CTRD1
099900100705     C                   MOVE      *ALL'0'       IN5098
100000100705     C                   MOVEA     IN5098        *IN(50)
100100100705     C*
100200100705     C                   EXSR      ATRD1
100300100705     C*
100400100705     C                   ENDSR
100500100705     C/EJECT
100600100705     C************************************************************
100700100705     C* RIEMPIMENTO VIDEATA  D1
100800100705     C************************************************************
100900100705     C     RIED1         BEGSR
101000100705     C*
101100100705     C*imposto i campi della videata dai parametri memorizzati su anmpe
101200100705     C*
101300100705     C                   ENDSR
101400100705     C/EJECT
101500100705     C************************************************************
101600100705     C* RIPRISTINA GLI ATTRIBUTI DEL VIDEO
101700100705     C************************************************************
101800100705     C     ATRD1         BEGSR
101900100705     C*
102000100705     C*
102100100705     C                   ENDSR
102200100705     C/EJECT
102201160530R224 C************************************************************
102202160530 "   C* GESTIONE VIDEO RECORD  D4
102203160530 "   C************************************************************
102204160530R224 C     GESD4         BegSr
102205160530     C*
102206160530     C* inizializzo videata
102207160530     C     $InzD4        IfEq      *ON
102208160530     C                   Exsr      InzD4
102209160530     C                   Move      *OFF          $InzD4
102210160530     C                   EndIf
102211160530     C*
102212160530     C     $LastV        IFNE      'D4'
102213160530     C                   Write     FMTT1
102214160530     C                   Exsr      GESPIE
102215160530     C                   Move      $LastV        $LastG
102216160530     C                   Move      'D4'          $LastV
102217160530     C                   EndIf
102218160530     C*
102219160530     C* se esistono errori sulla videata
102220160530     C* emetto la Write del formato a indicatori spenti per vedere
102221160530     C* le eventuali decodifiche
102222160530     C     *IN99         IfEq      *ON
102223160530     C     $GEST         ANDEQ     'D4'
102224160530     C                   MoveA     *IN           WIN
102225160530     C                   Move      *ALL'0'       IN5098           49
102226160530     C                   MoveA     IN5098        *IN(50)
102227160530     C                   Write     FMTD4
102228160530     C                   MoveA     WIN           *IN
102229160530     C                   EndIf
102230160530     C*
102231160530     C                   EXFMT     FMTD4
102232160530     C*
102233160530     C* controllo abilitazione tasti funzione
102234160530     C                   Exsr      CTRKD4
102235160530     C*
102236160530     C     *IN99         IfEq      *OFF
102237160530     C*
102238160530     C                   Select
102239160530     C* F3=Fine
102240160530     C     $Tasto        WhenEq    F03
102241160530     C                   Exsr      F03D1
102242160530     C* F4=Ricerca
102243160530     C     $Tasto        WhenEq    F04
102244160530     C                   Exsr      SEARCH
102245160530     C* F5=Refresh
102246160530     C     $Tasto        WhenEq    F05
102247160530     C                   Exsr      F05D4
102251160530     C* F12=Ritorno
102252160530     C     $Tasto        WhenEq    F12
102253160530     C                   Exsr      F12D4
102254160530     C* F24=Altri tasti
102255160530     C     $Tasto        WhenEq    F24
102256160530     C                   Exsr      F24D1
102257160530     C*
102258160530     C                   Other
102259160530     C                   Exsr      CTRD4
102260160530     C     *IN99         IfEq      *OFF
102261160530     C                   EndIf
102262160530     C*
102263160530     C*
102264160530     C     $Tasto        IfEq      F06
102265160530     C     *IN99         ANDEQ     *OFF
102266160530     C*
102267160530     C                   Exsr      AggD4
102268160530     C                   Move      'D1'          $GEST
102269160530     C*
102270160530     C                   EndIf
102271160530     C*
102272160530     C*
102273160530     C                   EndSl
102274160530     C*
102275160530     C                   EndIf
102276160530R224 C*
102277160530R224 C                   EndSr
102278160530R224 C************************************************************
102279160530 "   C* REPERIMENTO dati per intermediario
102280160530 "   C************************************************************
102281160530R224 C     InzD4         BegSr
102282160530     C*
102283160530     C* preparo i parametri di chiamata al driver
102284160530     C                   Clear                   XMPEDS
102285160530     C                   EVAL      TpChiamata = '01'
102286160530     C                   EVAL      NChiavi = 5
102287160530     C                   EVAL      VLogica = 2
102288160530     C*
102289160530     C* preparo le chiavi per la chiamata al driver
102290160530     C                   Clear                   XMPEDS1
102291160530     C                   MoveL     XSCSOC        DSMPESOC
102292160609     C                   MoveL     'SDGSJ51R A'  DSMPEPGM
102294160530     C                   MoveL     *blanks       DSMPEPRF
102295160530     C* Si reperiscono i dati di default a seconda delle modalita con
102296160530     C* cui il pgm e usato
102297160530     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
102298160530     C* MPETIP = 'PV'  pgm richiamato da menu
102299160530     C* MPETIP = 'PB'  x lancio Batch
102300160530     C                   MoveL     'PB'          DSMPETIP
102301160530     C*
102302160530     C                   CALLB     'XMPE'
102303160530     C                   PARM                    XMPEDS
102304160530     C                   PARM                    XMPEDS1
102305160530     C*
102306160530     C                   Clear                   DSDatiAna
102308160530     C*
102309160530     C     Risultato     IfEq      '0'
102310160530     C                   MoveL     DSMPEPAR      DSDatiAna
102311160530     C*
102341160530     C                   MoveL     $Dest         D4Dest
102342160530     C*
102343160530     C                   EndIf
102344160530     C*
102346160530R224 C                   EndSr
102347160530R224 C************************************************************
102348160530 "   C* GESTIONE F05 VIDEO D4
102349160530 "   C************************************************************
102350160530R224 C     F05D4         BegSr
102351160530     C*
102352160530     C                   Move      *ON           $InzD4
102353160530     C                   Move      'D4'          $GEST
102354160530     C*
102355160530R224 C                   EndSr
102356160530R224 C************************************************************
102357160530 "   C* GESTIONE F12 VIDEO D4
102358160530 "   C************************************************************
102359160530R224 C     F12D4         BegSr
102360160530     C*
102361160530     C                   Move      'D1'          $GEST
102362160530     C*
102363160530R224 C                   EndSr
102364160530R224 C************************************************************
102365160530 "   C* CONTROLLO VIDEATA  D4
102366160530 "   C************************************************************
102367160530R224 C     CtrD4         BegSr
102368160530     C*
102369160530     C                   SETOFF                                       99
102370160530     C* non imposto pos. del cursore
102371160530     C                   Z-Add     0             H1RIGA
102372160530     C                   Z-Add     0             H1COLO
102373160530     C*
102455160530     C* Destinazione dati aggiuntivi
102456160530     C                   EXSR      CtrDest
102459160530     C*
102460160530R224 C                   EndSr
102461160530R224 C************************************************************
102462160530 "   C* Controllo destinazione
102463160530 "   C************************************************************
102464160530R224 C     CtrDest       BEGSR
102465160530     C*
102466160530     C                   Clear                   D4DestD
102467160530     C*
102468160530     C                   Clear                   SDGHC4DS
102469160530     C*
102470160530     C                   Clear                   SDGTABDS
102471160530     C*
102472160530     C                   if        D4Dest     <> *Blank
102473160530     C*
102474160530     C                   Eval      XTbRic      = '1'
102475160530     C                   Eval      XTbAzi      = XScSoc
102476160530     C                   Eval      XTbKey      = *Zero
102477160530     C                   Eval      XTbCod      = 'HC4'
102478160530     C                   Move      D4Dest        XTbKey
102479160530     C*
102480160530     C                   CALL      'SDGTABDR'
102481160530     C                   PARM                    SDGTABDS
102482160530     C*
102483160530     C                   if        XTbErr     <> '0'
102484160530     C                   SetOn                                        7699
102485160530     C*
102486160530     C                   else
102487160530     C                   MoveL     XTbUni        SDGHC4DS
102488160530     C                   MoveL     DesHC4        D4DestD
102489160530     C*
102490160530     C                   endif
102491160530     C*
102492160530     C                   else
102493160530     C* "Generica"
102494160530     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS1529'
102495160530     C                   EXSR      PrendiMsg
102496160530     C                   if        ERRMSG     <> *ON
102497160530     C                   MoveL     TXTMSG        D4DestD
102498160530     C                   else
102499160530     C                   MoveL     *ALL'?'       D4DestD
102500160530     C                   endif
102501160530     C*
102502160530     C                   endif
102503160530     C*
102504160530R224 C                   ENDSR
102505160530R224 C/EJECT
102506160530R224 C************************************************************
102507160530 "   C* Reperimento testo messaggi
102508160530 "   C************************************************************
102509160530R224 C     PrendiMsg     BEGSR
102510160530     C*
102511160530     C                   CALLB     'XRTVM'
102512160530     C                   PARM                    IDMSG            27
102513160530     C                   PARM                    TXTMSG          100
102514160530     C                   PARM                    ERRMSG            1
102515160530     C*
102516160530R224 C                   ENDSR
102517160530R224 C/EJECT
102518160530R224 C************************************************************
102519160530 "   C* Aggiornamento D4
102520160530 "   C************************************************************
102521160530R224 C     AggD4         BegSr
102522160530     C*
102523160530     C*
102524160530     C* preparo i parametri di chiamata al driver
102525160530     C                   Clear                   XMPEDS
102526160530     C                   EVAL      TpChiamata = '03'
102527160530     C                   EVAL      NChiavi = 5
102528160530     C                   EVAL      VLogica = 2
102529160530     C*
102530160530     C* preparo le chiavi per la chiamata al driver
102531160530     C                   Clear                   XMPEDS1
102532160530     C                   MoveL     XscSoc        DSMPESOC
102533160609     C                   MoveL     'SDGSJ51R A'  DSMPEPGM
102535160530     C                   MoveL     *blanks       DSMPEPRF
102536160530     C*
102537160530     C* Si memorizzano i dati di default a seconda delle modalita con
102538160530     C* cui il pgm e usato
102539160530     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
102540160530     C* MPETIP = 'PV'  pgm richiamato da menu
102541160530     C* MPETIP = 'PB'  x lancio Batch
102542160530     C*
102543160530     C                   MoveL     'PB'          DSMPETIP
102544160530     C* Dati in recupero
102545160530     C*
102546160530     C                   Clear                   DSDatiAna
102547160530     C*
102556160530     C                   MoveL     D4Dest        $Dest
102560160530     C*
102561160530     C                   MoveL     DSDatiAna     DSMPEPAR
102562160530     C*
102563160530     C                   CALLB     'XMPE'
102564160530     C                   PARM                    XMPEDS
102565160530     C                   PARM                    XMPEDS1
102566160530     C*
102567160530R224 C                   EndSr
102568100705      ************************************************************
102569100705     C* GESTIONE F02 VIDEO D1
102570100705     C************************************************************
102600100705     C     F02D1         BEGSR
102700100705     C*
102800100705     C* per decodificare la videata richiamo la routine dei ctrl
102900100705     C* e poi disattivo gli eventuali messaggi di errore
103000100705     C                   EXSR      CTRD1
103100100705     C*
103200100705     C                   MOVE      *ALL'0'       IN5098
103300100705     C                   MOVEA     IN5098        *IN(50)
103400100705     C*
103500100705     C                   ENDSR
103600100705     C/EJECT
103700100705     C************************************************************
103800100705     C* GESTIONE F03 VIDEO D1
103900100705     C************************************************************
104000100705     C     F03D1         BEGSR
104100100705     C*
104200100705     C                   MOVE      *ON           $FINE
104300100705      * fine programma
104400100705     C                   EXSR      ENDPGM
104500100705     C*
104600100705     C                   ENDSR
104700100705     C/EJECT
104800100705     C************************************************************
104900100705     C* GESTIONE F05 VIDEO D1
105000100705     C************************************************************
105100100705     C     F05D1         BEGSR
105200100705     C*
105300100705     C                   MOVE      *ON           $INZD1
105400100705     C*
105500100705     C                   ENDSR
105600100705     C/EJECT
105700100705     C************************************************************
105800100705     C* GESTIONE F12 VIDEO D1
105900100705     C************************************************************
106000100705     C     F12D1         BEGSR
106100100705     C*
106200100705     C                   MOVE      *ON           $FINE
106300100705      * fine programma
106400100705     C                   EXSR      ENDPGM
106500100705     C*
106600100705     C                   ENDSR
106700100705     C/EJECT
106800100705     C************************************************************
106900100705     C* GESTIONE F14 VIDEO D1
107000100705     C************************************************************
107100100705     C     F14D1         BEGSR
107200100705     C*
107300100705     C                   MOVE      *HIVAL        $LASTV
107400100705     C*
107500100705     C                   ENDSR
107600100705     C/EJECT
107700100705     C************************************************************
107800100705     C* GESTIONE F18 VIDEO D1
107900100705     C************************************************************
108000100705     C     F18D1         BEGSR
108100100705     C*
108200100705     C                   MOVE      'D3'          $GEST
108300100705     C                   MOVE      *ON           $INZD3
108400100705     C*
108500100705     C                   ENDSR
108600100705     C/EJECT
111600100705     C************************************************************
111700100705     C* GESTIONE F21 VIDEO D1
111800100705     C************************************************************
111900100705     C     F21D1         BEGSR
112000100705     C*
112100100705     C                   MOVEl     '1'           KRITB
112200141229     C                   MOVE      'SJ52'        KCOAZ
112300100705     C                   CALL      'BCH09'
112400100705     C                   PARM                    KPJBA
112500100705     C*
112600100705     C                   ENDSR
112700100705     C/EJECT
112701160530R224 C************************************************************
112702160530 "   C* GESTIONE F22 VIDEO D4
112703160530 "   C************************************************************
112704160530R224 C     F22D1         BegSr
112705160530     C*
112706160530     C                   Move      'D4'          $GEST
112707160530     C                   Move      *ON           $INZD4
112708160530R224 C*
112709160530R224 C                   EndSr
112710160530R224 C************************************************************
112711160530 "   C* GESTIONE F24 VIDEO D4
112712160530 "   C************************************************************
112713160530R224 C     F24D4         BEGSR
112714160530     C*
112715160530     C                   MOVEA     $FX4          $FX
112716160530     C                   MOVEA     $FC4          $FC
112717160530     C                   MOVEA     $FR4          $FR
112718160530     C                   Z-ADD     $ULKD4        $ULK
112719160530     C                   Z-ADD     79            $L01
112720160530     C                   Z-ADD     79            $L02
112721160530     C                   EXSR      RIEKEY
112722160530     C                   Z-ADD     $ULK          $ULKD4            3 0
112723160530     C                   MOVEA     $FC           $FC4
112724160530     C                   MOVEL     $KEY1         Z4KE1
112725160530     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
112726160530     C                   MOVEL     $KEY2         Z4KE2
112727160530     C                   WRITE     FMTZ4
112728160530     C*
112729160530R224 C                   ENDSR
112730160530R224 C/EJECT
112800100705     C************************************************************
112900100705     C* GESTIONE F24 VIDEO D1
113000100705     C************************************************************
113100100705     C     F24D1         BEGSR
113200100705     C*
113300100705     C                   MOVEA     $FX1          $FX
113400100705     C                   MOVEA     $FC1          $FC
113500100705     C                   MOVEA     $FR1          $FR
113600100705     C                   Z-ADD     $ULKD1        $ULK
113700100705     C                   Z-ADD     79            $L01
113800100705     C                   Z-ADD     79            $L02
113900100705     C                   EXSR      RIEKEY
114000100705     C                   Z-ADD     $ULK          $ULKD1            3 0
114100100705     C                   MOVEA     $FC           $FC1
114200100705     C                   MOVEL     $KEY1         Z1KE1
114300100705     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
114400100705     C                   MOVEL     $KEY2         Z1KE2
114500100705     C                   WRITE     FMTZ1
114600100705     C*
114700100705     C                   ENDSR
114800100705     C/EJECT
114900100705     C************************************************************
115000100705     C* GESTIONE F24 VIDEO D3
115100100705     C************************************************************
115200100705     C     F24D3         BEGSR
115300100705     C*
115400100705     C                   MOVEA     $FX3          $FX
115500100705     C                   MOVEA     $FC3          $FC
115600100705     C                   MOVEA     $FR3          $FR
115700100705     C                   Z-ADD     $ULKD3        $ULK
115800100705     C                   Z-ADD     44            $L01
115900100705     C                   Z-ADD     0             $L02
116000100705     C                   EXSR      RIEKEY
116100100705     C                   Z-ADD     $ULK          $ULKD3            3 0
116200100705     C                   MOVEA     $FC           $FC3
116300100705     C                   MOVEL     $KEY1         Z3KE1
116400100705     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
116500100705     C                   WRITE     FMTZ3
116600100705     C*
116700100705     C                   ENDSR
116800100705     C/EJECT
116900100705     C************************************************************
117000100705     C* CONTROLLO VIDEATA
117100100705     C************************************************************
117200100705     C     CTRD1         BEGSR
117300100705     C*
117400100705     C                   SETOFF                                       99
117500100705     C* non imposto pos. del cursore
117600100705     C                   Z-ADD     0             H1RIGA
117700100705     C                   Z-ADD     0             H1COLO
118600140327     C*
118700100705     C                   EXSR      CtrAnno
119000100705     C*
119100100705     C                   EXSR      CtrSogg
119200100705     C*
119300100705     C                   EXSR      CtrKsc
119301141229     C*
119302141229     C                   EXSR      CtrTpOpe
119303141229     C*
119304141229     C                   EXSR      CtrNrInter
120600110715     C*
120700150112     C                   EXSR      CtrDelete
120800110720     C*
120900110720     C                   EXSR      CtrStpAnn
121000100705     C*
121100100705     C                   ENDSR
121200100705     C/EJECT
127500100705     C***********************************************************
127600100705     C* Controllo anno di competenza
127700100705     C***********************************************************
127800100705     C     CtrAnno       BEGSR
127900100705     C*
128000100705     C                   if        D1Anno = 0
128100100705     C                   SetOn                                        5399
128200100705     C                   endif
128300100705     C*
128400100705     C                   ENDSR
128500100705     C/EJECT
133500100705     C***********************************************************
133600100705     C* Controllo codice soggetto
133700100705     C***********************************************************
133800100705     C     CtrSogg       BEGSR
133900100705     C*
134000100705     C                   Clear                   D1SoggD
134100100705     C                   if        D1Sogg     <> *Blank
134200100705     C*
134300100705     C     D1SOGG        IFNE      *BLANK
134400100705     C                   CALLb     'XALLINEA'
134500100705     C                   PARM                    D1SOGG
134600100705     C                   PARM      8             LEN               5 0
134700100705     C                   PARM                    XSCINP
134800100705     C                   ENDIF
134900100705     C*
135000100705     C                   MoveL     D1Sogg        WSogg
135100100705     C                   EXSR      CallDvaSog
135200100705     C*
135300100705     C                   if        DVAErrore  <> '1'
135400100705     C*
135500100705     C                   Eval      %subst(DVASOG:1:DVALENOUT)
135600100705     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
135700100705     C*
135800100705     C                   MoveL     DVSDes        D1SoggD
135900100705     C*
136000100705     C                   else
136100100705     C*
136200100705     C                   SetOn                                        5599
136300100705     C*
136400100705     C                   endif
136500100705     C*
136600100705     C                   endif
136700100705     C*
136800100705     C                   ENDSR
136900100705     C/EJECT
137000100705     C************************************************************
137100100705     C* Chiamata al driver dei soggetti
137200100705     C************************************************************
137300100705     C     CallDvaSog    BEGSR
137400100705     C*
137500100705     C                   EVAL      DVALenOut = %Size(DVASOG)
137600100705     C*
137700100705     C                   CALLB     'NDDVASOG'
137800100705     C                   PARM      '1'           DVARIC
137900100705     C                   PARM      XScSoc        DVASOCIETA
138000100705     C                   PARM                    DVAUNITA
138100100705     C                   PARM      'DVASOG    '  DVASTRUTT
138200100705     C                   PARM                    DVADTRIF
138300100705     C                   PARM                    DVALENOUT
138400100705     C* Se si accede con il codice cli/for impostare i campi seguenti
138500100705     C* se si accede con il soggetto ometterli e impostare gli altri
138600100705     C*                  PARM                    DVASNATURA
138700100705     C*                  PARM                    DVACODICE
138800100705     C*                  PARM                    DVALINEAV
138900100705     C*                  PARM                    DVAFILIALE
139000100705     C                   PARM                    *omit
139100100705     C                   PARM                    *omit
139200100705     C                   PARM                    *omit
139300100705     C                   PARM                    *omit
139400100705     C                   PARM      WSogg         DVSSOGG
139500100705     C                   PARM      *Blank        DVSTPIND
139600100705     C                   PARM      *Blank        DVSCDIND
139700100705     C                   PARM                    DVAERRORE
139800100705     C                   PARM                    DVAOUTPUT
139900100705     C*
140000100705     C                   ENDSR
140100100705     C/EJECT
160600100705     C***********************************************************
160700100705     C* Controllo conto
160800100705     C***********************************************************
160900100705     C     CtrKsc        BEGSR
161000100705     C*
161100100705     C                   Clear                   WKcc
161200100705     C                   Clear                   WKsc
161300100705     C*
161400100705     C                   Clear                   D1KSCD
161500100705     C*
161600100705     C                   if        D1Ksc      <> *Blank
161700100705     C*
161800100705     C     D1KSC         IFNE      *Blanks
161900100705     C                   CALLB     'XALLINEA'
162000100705     C                   PARM                    D1KSC
162100100705     C                   PARM      8             LEN               5 0
162200100705     C                   PARM                    XSCINP
162300100705     C                   ENDIF
163000100705     C*
163100141229     C* reperisco capoconto fornitore
163400100705     C                   CALLB     'XCAPCLIFOR'
163500100705     C                   PARM                    XScSoc
163600141229     C                   PARM      'F'           WKcc
163700100705     C                   PARM                    D1Ksc
163900100705     C*
164000100705     C*
164100100705     C                   Eval      WKsc        = D1Ksc
164200100705     C                   EXSR      CALLMVC002
164300100705     C                   if        Esito       = *On
164400100705     C                   SetOn                                        5699
164500100705     C                   else
164600100705     C                   MoveL     DesBrev002    D1KSCD
164700100705     C                   endif
165000100720     C*
165100100720     C                   endif
165200100705     C*
165300100705     C                   ENDSR
165400100705     C/EJECT
165500100705     C***********************************************************
165600100705     C* Driver per controllo conto
165700100705     C***********************************************************
165800100705     C     CallMvc002    BEGSR
165900100705     C*
166000100705     C                   Clear                   ND002DS
166100100705     C*
166200100705     C                   EVAL      LenOut = %Size(ND002DS)
166300100705     C*
166400100705     C                   CallB     'NDMVC002'
166500100705     C                   PARM                    XScSoc
166600100705     C                   PARM                    WKCC
166700100705     C                   PARM                    WKSC
166800100705     C                   PARM                    Data
166900100705     C                   PARM      *OFF          GesMsg
167000100705     C                   PARM      *OFF          Esito
167100100705     C                   PARM      'ND002DS'     StrutturaO
167200100705     C                   PARM                    ND002DS
167300100705     C                   PARM                    LenOut
167400100705     C*
167500100705     C                   ENDSR
167600100705     C/EJECT
167601141229     C***********************************************************
167602141229     C* Controllo tipo operazione
167603141229     C***********************************************************
167604141229     C     CtrTpOpe      BEGSR
167605141229     C*
167606141229     C                   Clear                   D1TpOpeD
167607141229     C*
167608141229     C                   Clear                   X07ValDs
167609141229     C*
167610141229     C                   Move      '1'           X07Ric
167611141229     C                   Move      *On           X07All
167612150112     C                   Movel     'SDGSJ52S '   X07TRc
167613150112     C                   Movel     'TPOPEJ52 '   X07Cam
167614141229     C                   Movel     D1TpOpe       X07VAL
167615141229     C*
167616141229     C                   Callb     'X07VALR'
167617141229     C                   Parm                    X07ValDs
167618141229     C*
167619141229     C     X07ERR        IFEQ      *Off
167620141229     C                   MOVEL     X07DES        D1TpOpeD
167621141229     C                   else
167622141229     C                   SetOn                                        7599
167623141229     C                   endif
167624141229     C*
167625141229     C                   ENDSR
167626141229     C/EJECT
167627141229     C***********************************************************
167628141229     C* Controllo numeri interni
167629141229     C***********************************************************
167630141229     C     CtrNrInter    BEGSR
167631141229     C*
167632141229     C                   if        D1NrInter1  > D1NrInter2   and
167633141229     C                             D1NrInter2  > 0
167634141229     C                   SetOn                                        7399
167635141229     C                   endif
167636141229     C*
167637141229     C                   ENDSR
167638141229     C/EJECT
167700110715     C***********************************************************
167800150112     C* Controllo tipo cancellazione movimenti
167900110715     C***********************************************************
168000150112     C     CtrDelete     BEGSR
168501150112     C*
168502150112     C                   Clear                   D1DeleteD
168503150112     C*
168504150112     C                   Clear                   X07ValDs
168505150112     C*
168506150112     C                   Move      '1'           X07Ric
168507150112     C                   Move      *On           X07All
168508150112     C                   Movel     'SDGSJ52S '   X07TRc
168509150112     C                   Movel     'DELETEJ52'   X07Cam
168510150112     C                   Movel     D1Delete      X07VAL
168511150112     C*
168512150112     C                   Callb     'X07VALR'
168513150112     C                   Parm                    X07ValDs
168514150112     C*
168515150112     C     X07ERR        IFEQ      *Off
168516150112     C                   MOVEL     X07DES        D1DeleteD
168517150112     C                   else
168518150112     C                   SetOn                                        7099
168519150112     C                   endif
168600110715     C*
168700110715     C                   ENDSR
168800110715     C/EJECT
168900110720     C***********************************************************
169000110720     C* Controllo stampa annullamenti
169100110720     C***********************************************************
169200110720     C     CtrStpAnn     BEGSR
169300110720     C*
169400110720     C                   if        D1StpAnn   <> XScOn   and
169500110720     C                             D1StpAnn   <> XScOff
169600110720     C                   SetOn                                        5899
169700110720     C                   endif
169800110720     C*
169900110720     C                   ENDSR
170000110720     C/EJECT
170100100705     C************************************************************
170200100705     C* GESTIONE VALORI ASSUNTI
170300100705     C************************************************************
170400100705     C     GESD3         BEGSR
170500100705     C*
170600100705   B1C     $INZD3        IFEQ      *ON
170700100705     C                   EXSR      INZD3
170800100705     C                   MOVE      *OFF          $INZD3
170900100705   E1C                   ENDIF
171000100705     C*
171100100705   B1C     $LASTV        IFNE      'D3'
171200100705     C                   EXSR      GESPIE
171300100705     C                   MOVE      $LASTV        $LASTG
171400100705     C                   MOVE      'D3'          $LASTV
171500100705   E1C                   ENDIF
171600100705     C*
171700100705     C* se esistono errori sulla videata
171800100705     C* emetto la write del formato a indicatori spenti per vedere
171900100705     C* le eventuali decodifiche
172000100705   B1C     *IN99         IFEQ      *ON
172100100705     C                   MOVEA     *IN           WIN
172200100705     C                   MOVE      *ALL'0'       IN5098           49
172300100705     C                   MOVEA     IN5098        *IN(50)
172400100705     C                   WRITE     FMTD3
172500100705     C                   MOVEA     WIN           *IN
172600100705   E1C                   ENDIF
172700100705     C*
172800100705     C                   EXFMT     FMTD3
172900100705     C*
173000100705     C* controllo abilitazione tasti funzione
173100100705     C                   EXSR      CTRKD3
173200100705     C*
173300100705   B1C     *IN99         IFEQ      *OFF
173400100705     C*
173500100705   B2C                   SELECT
173600100705     C* F3=Fine
173700100705     C     $TASTO        WHENEQ    F03
173800100705     C                   EXSR      F03D1
173900100705     C* F12=Ritorno
174000100705     C     $TASTO        WHENEQ    F12
174100100705     C                   EXSR      F12D3
174200100705     C* F24=Altri tasti
174300100705     C     $TASTO        WHENEQ    F24
174400100705     C                   EXSR      F24D3
174500100705     C*
174600100705     C                   OTHER
174700100705     C                   EXSR      CTRD3
174800100705     C     *IN99         IFEQ      *OFF
174900100705     C                   MOVE      $LASTG        $GEST
175000100705     C*
175100100705     C* definisco i nuovi valori di default
175200100705     C*
175300100705     C                   ENDIF
175400100705     C*
175500100705   E2C                   ENDSL
175600100705     C*
175700100705     C* aggiorno valori assunti
175800100705     C* se richiesto il salvataggio
175900100705     C     D3BSV         IFEQ      1
176000100705     C                   EXSR      AGGMPE
176100100705     C                   ENDIF
176200100705     C*
176300100705   E1C                   ENDIF
176400100705     C*
176500100705     C                   ENDSR
176600100705     C/EJECT
176700100705     C************************************************************
176800100705     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
176900100705     C************************************************************
177000100705     C     INZD3         BEGSR
177100100705     C*
177200100705     C                   CLEAR                   FMTD3
177300100705     C*
177400100705     C                   EVAL      D3BSVA      = Normale
177500100705     C*
177600100705     C*
177700100705     C                   ENDSR
177800100705     C/EJECT
177900100705     C************************************************************
178000100705     C* GESTIONE F12 VIDEO D3
178100100705     C************************************************************
178200100705     C     F12D3         BEGSR
178300100705     C*
178400100705     C                   MOVE      $LASTG        $GEST
178500100705     C*
178600100705     C                   ENDSR
178700100705     C/EJECT
178800100705     C************************************************************
178900100705     C* CONTROLLO VIDEATA  D3
179000100705     C************************************************************
179100100705     C     CTRD3         BEGSR
179200100705     C*
179300100705     C                   SETOFF                                       99
179400100705     C* non imposto pos. del cursore
179500100705     C                   Z-ADD     0             H1RIGA
179600100705     C                   Z-ADD     0             H1COLO
179700100705     C*
179800100705     C* Controllo VALORE
179900100705     C**** D3VAL         IFNE      XSCON
180000100705     C**** D3VAL         ANDNE     XSCOFF
180100100705     C****               SETON                                        5099
180200100705     C****               ENDIF
180300100705     C*
180400100705     C**** D3VAL         IFEQ      XSCON
180500100705     C****               MOVE      *ON           $VAL
180600100705     C****               ELSE
180700100705     C****               MOVE      *OFF          $val
180800100705     C****               endif
180900100705     C*
181000100705     C                   ENDSR
181100100705     C/EJECT
181200100705     C************************************************************
181300100705     C* ESECUZIONE LANCIO
181400100705     C************************************************************
181500100705     C     EXELAN        BEGSR
181600100705     C*
181700141229     C                   reset                   SDGSJ52DS
181800100705     C*
181900141229     C                   MoveL     XScSoc        SocietaJ52
182200141229     C                   Z-Add     D1Anno        AnnoJ52
182900141229     C                   MoveL     D1PartIVA     PartIVAJ52
183000141229     C                   MoveL     D1CdFisc      CdFiscJ52
183100141229     C                   MoveL     D1Sogg        SoggJ52
183800100707     C                   Clear                   WKcc
183900100707     C                   if        D1Ksc      <> *Blank
184000100707     C                   CALLB     'XCAPCLIFOR'
184100100707     C                   PARM                    XScSoc
184200141229     C                   PARM      'F'           WKcc
184300100707     C                   PARM                    D1Ksc
184400100707     C                   endif
184600141229     C                   MoveL     D1Ksc         KscJ52
184601150113     C*
184800150113     C                   Eval      TpOpeJ52    = D1TpOpe
184801150113     C                   Eval      NrInte1J52  = D1NrInter1
184802150113     C                   Eval      NrInte2J52  = D1NrInter2
184803150113     C*
184804150113     C                   Eval      DeleteJ52   = D1Delete
185200110720     C                   if        D1StpAnn    = XScOn
185300141229     C                   Eval      StpAnnJ52   = *On
185400110720     C                   else
185500141229     C                   Eval      StpAnnJ52   = *Off
185600110720     C                   endif
185700100705     C*
186400141229     C                   movel     SDGSJ52DS     Kpjbu
186500100705     C*
186600100719     C                   Move      0             Debug             1 0
186700100719     C                   if        Debug       = 1
186800141229     C                   CALL      'SDGSJ52R'
186900100719     C                   PARM                    Kpjba
187000100705     C*
187100100705     C                   ELSE
187200100705     C*
187300141229     C                   MOVE      'SJ52'        KCoAz
187400100705     C                   call      'BCH10'
187500100705     C                   parm                    Kpjba
187600100705     C*
187700100705     C                   ENDIF
187800100705     C*
187900100705     C* aggiorno valori assunti
188000100705     C*****              EXSR      AGGMPE
188100100705     C* vado a fine
188200100705     C                   MOVEL     *on           $Fine
188300100705     C*
188400100705     C     XExeLan       ENDSR
188500100705     C/EJECT
188600100705     C************************************************************
188700100705     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
188800100705     C************************************************************
188900100705     C     CTRKD1        BEGSR
189000100705     C*
189100100705     C                   SETOFF                                       99
189200100705     C                   MOVEA     $FC1          $FC
189300100705     C                   EXSR      CTRKEY
189400100705     C     *IN99         IFEQ      *ON
189500100705     C                   SETON                                        97
189600100705     C                   ENDIF
189700100705     C*
189800100705     C                   ENDSR
189900100705     C/EJECT
190000100705     C************************************************************
190100100705     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
190200100705     C* INPUT
190300100705     C* $FC3 = Abilitazioni videata D3
190400100705     C* OUTPUT
190500100705     C* *IN97 *IN99 Errori
190600100705     C************************************************************
190700100705     C     CTRKD3        BEGSR
190800100705     C*
190900100705     C                   SETOFF                                       99
191000100705     C                   MOVEA     $FC3          $FC
191100100705     C                   EXSR      CTRKEY
191200100705     C     *IN99         IFEQ      *ON
191300100705     C                   SETON                                        97
191400100705     C                   ENDIF
191500100705     C*
191600100705     C                   ENDSR
191700100705     C/EJECT
191701160530R224 C************************************************************
191702160530 "   C* CONTROLLO ABILITAZIONE TASTI VIDEATA D4
191703160530 "   C* INPUT
191704160530 "   C* $FC4 = Abilitazioni videata D4
191705160530 "   C* OUTPUT
191706160530 "   C* *IN97 *IN99 Errori
191707160530 "   C************************************************************
191708160530R224 C     CTRKD4        BEGSR
191709160530     C*
191710160530     C                   SETOFF                                       99
191711160530     C                   MOVEA     $FC4          $FC
191712160530     C                   EXSR      CTRKEY
191713160530     C     *IN99         IFEQ      *ON
191714160530     C                   SETON                                        97
191715160530     C                   ENDIF
191716160530     C*
191717160530R224 C                   ENDSR
191718160530R244 C/EJECT
191800100705     C************************************************************
191900100705     C* CONTROLLO ABILITAZIONE TASTI
192000100705     C************************************************************
192100100705     C     CTRKEY        BEGSR
192200100705     C*
192300100705     C                   SELECT
192400100705     C     $TASTO        WHENEQ    F01
192500100705     C     $FC(1)        IFEQ      '0'
192600100705     C                   SETON                                        99
192700100705     C                   ENDIF
192800100705     C     $TASTO        WHENEQ    F02
192900100705     C     $FC(2)        IFEQ      '0'
193000100705     C                   SETON                                        99
193100100705     C                   ENDIF
193200100705     C     $TASTO        WHENEQ    F03
193300100705     C     $FC(3)        IFEQ      '0'
193400100705     C                   SETON                                        99
193500100705     C                   ENDIF
193600100705     C     $TASTO        WHENEQ    F04
193700100705     C     $FC(4)        IFEQ      '0'
193800100705     C                   SETON                                        99
193900100705     C                   ENDIF
194000100705     C     $TASTO        WHENEQ    F05
194100100705     C     $FC(5)        IFEQ      '0'
194200100705     C                   SETON                                        99
194300100705     C                   ENDIF
194400100705     C     $TASTO        WHENEQ    F06
194500100705     C     $FC(6)        IFEQ      '0'
194600100705     C                   SETON                                        99
194700100705     C                   ENDIF
194800100705     C     $TASTO        WHENEQ    F07
194900100705     C     $FC(7)        IFEQ      '0'
195000100705     C                   SETON                                        99
195100100705     C                   ENDIF
195200100705     C     $TASTO        WHENEQ    F08
195300100705     C     $FC(8)        IFEQ      '0'
195400100705     C                   SETON                                        99
195500100705     C                   ENDIF
195600100705     C     $TASTO        WHENEQ    F09
195700100705     C     $FC(09)       IFEQ      '0'
195800100705     C                   SETON                                        99
195900100705     C                   ENDIF
196000100705     C     $TASTO        WHENEQ    F10
196100100705     C     $FC(10)       IFEQ      '0'
196200100705     C                   SETON                                        99
196300100705     C                   ENDIF
196400100705     C     $TASTO        WHENEQ    F11
196500100705     C     $FC(11)       IFEQ      '0'
196600100705     C                   SETON                                        99
196700100705     C                   ENDIF
196800100705     C     $TASTO        WHENEQ    F12
196900100705     C     $FC(12)       IFEQ      '0'
197000100705     C                   SETON                                        99
197100100705     C                   ENDIF
197200100705     C     $TASTO        WHENEQ    F13
197300100705     C     $FC(13)       IFEQ      '0'
197400100705     C                   SETON                                        99
197500100705     C                   ENDIF
197600100705     C     $TASTO        WHENEQ    F14
197700100705     C     $FC(14)       IFEQ      '0'
197800100705     C                   SETON                                        99
197900100705     C                   ENDIF
198000100705     C     $TASTO        WHENEQ    F15
198100100705     C     $FC(15)       IFEQ      '0'
198200100705     C                   SETON                                        99
198300100705     C                   ENDIF
198400100705     C     $TASTO        WHENEQ    F16
198500100705     C     $FC(16)       IFEQ      '0'
198600100705     C                   SETON                                        99
198700100705     C                   ENDIF
198800100705     C     $TASTO        WHENEQ    F17
198900100705     C     $FC(17)       IFEQ      '0'
199000100705     C                   SETON                                        99
199100100705     C                   ENDIF
199200100705     C     $TASTO        WHENEQ    F18
199300100705     C     $FC(18)       IFEQ      '0'
199400100705     C                   SETON                                        99
199500100705     C                   ENDIF
199600100705     C     $TASTO        WHENEQ    F19
199700100705     C     $FC(19)       IFEQ      '0'
199800100705     C                   SETON                                        99
199900100705     C                   ENDIF
200000100705     C     $TASTO        WHENEQ    F20
200100100705     C     $FC(20)       IFEQ      '0'
200200100705     C                   SETON                                        99
200300100705     C                   ENDIF
200400100705     C     $TASTO        WHENEQ    F21
200500100705     C     $FC(21)       IFEQ      '0'
200600100705     C                   SETON                                        99
200700100705     C                   ENDIF
200800100705     C     $TASTO        WHENEQ    F22
200900100705     C     $FC(22)       IFEQ      '0'
201000100705     C                   SETON                                        99
201100100705     C                   ENDIF
201200100705     C     $TASTO        WHENEQ    F23
201300100705     C     $FC(23)       IFEQ      '0'
201400100705     C                   SETON                                        99
201500100705     C                   ENDIF
201600100705     C     $TASTO        WHENEQ    F24
201700100705     C     $FC(24)       IFEQ      '0'
201800100705     C                   SETON                                        99
201900100705     C                   ENDIF
202000100705     C                   ENDSL
202100100705     C*
202200100705     C                   ENDSR
202300100705     C/EJECT
202400100705     C************************************************************
202500100705     C* OPERAZIONI INIZIALI
202600100705     C************************************************************
202700100705     C     *INZSR        BEGSR
202800100705     C*
202900100705     C     *ENTRY        PLIST
203000100705     C                   PARM                    KPJBA
203100100705     C*
203200100705     C* Reperimento tasti di funzione
203300100705     C                   EXSR      REPKEY
203400100705     C*
203500100705     C     *DMY          Move      UDate         UDateISO
203600100705     C*
203700100705     C                   ENDSR
203800100705     C/EJECT
203900100705     C************************************************************
204000100705     C* REPERIMENTO TASTI DI FUNZIONE
204100100705     C************************************************************
204200100705     C     REPKEY        BEGSR
204300100705     C*
204400100705     C                   MOVEA     $FM1          $FM
204500100705     C                   MOVEA     $Fc1          $Fc
204600100705     C                   EXSR      REPKKK
204700100705     C                   MOVEA     $FX           $FX1
204800100705     C*
204900100705     C                   MOVEA     $FM3          $FM
205000100705     C                   MOVEA     $Fc3          $FM
205100100705     C                   EXSR      REPKKK
205200100705     C                   MOVEA     $FX           $FX3
205201160530R224 C*
205202160530 "   C                   MOVEA     $FM4          $FM
205203160530 "   C                   MOVEA     $Fc4          $FM
205204160530 "   C                   EXSR      REPKKK
205205160530R224 C                   MOVEA     $FX           $FX4
205300100705     C*
205400100705     C                   ENDSR
205500100705     C/EJECT
205600100705     C************************************************************
205700100705     C* REPERIMENTO TASTI DI FUNZIONE
205800100705     C************************************************************
205900100705     C     REPKKK        BEGSR
206000100705     C*
206100100705     C                   CLEAR                   $FX
206200100705     C     1             DO        24            X
206300100705     C*
206400100705     C     $FM(X)        IFNE      *BLANK
206500100705     C     $fc(X)        andne     '0'
206600100705     C*
206700100705     C                   MOVEL     $FM(X)        IDMSG
206800100705     C                   CALLB     'XRTVM'
206900100705     C                   PARM                    IDMSG            27
207000100705     C                   PARM                    TXTMSG          100
207100100705     C                   PARM                    ERRMSG            1
207200100705     C*
207300100705     C     ERRMSG        IFNE      *ON
207400100705     C                   MOVEL     TXTMSG        $FX(X)
207500100705     C                   ELSE
207600100705     C                   MOVEL     *ALL' '       $FX(X)
207700100705     C                   ENDIF
207800100705     C*
207900100705     C                   ENDIF
208000100705     C*
208100100705     C                   ENDDO
208200100705     C*
208300100705     C                   ENDSR
208400100705     C/EJECT
208500100705     C************************************************************
208600100705     C* INIZIALIZZAZIONE VARIABILI
208700100705     C************************************************************
208800100705     C     INZVAR        BEGSR
208900100705     C*
209000100705     C* Reperimento dati società
209100100705     C                   MOVEL     'SOC001'      TIPXSC
209200100705     C                   MOVEL     *BLANK        SOCXSC
209300100705     C                   EXSR      REPSOC
209400100705     C     RTNXSC        IFNE      '1'
209500100705     C                   MOVEL     XSOCDS        SOC001
209600100705     C                   ELSE
209700100705     C                   RETURN
209800100705     C                   ENDIF
210200100705     C*
210300100705     C* Reperimento autorizzazioni
210400100705     C                   EXSR      REPAUT
210800100820     C*
210900100820     C* Reperisco la libreria in cui vengono memorizzati i file
211000100820     C                   EXSR      RepLibFile
211100100705     C*
211200100705     C* Reperimento valori default utente
211300100705     C                   EXSR      REPDFT
211400100705     C*
211500100705     C* Definizione dei tasti funzione utilizzabili a pgm
211600100705     C                   EXSR      DEFOPZ
211700100705     C*
211800100705     C* Variabili per gestione videate
211900100705     C                   MOVE      'D1'          $GEST                          fmt in gestione
212000100705     C                   MOVE      *ON           $INZD1                         inizializzione D1
212001160530R224 C                   Move      *BLANK        $INZD4            1
212100100705     C                   MOVE      *OFF          $FINE
212200100705     C                   MOVE      *BLANK        $LASTG
212300100705     C                   MOVE      *BLANK        $LASTV
212400100705     C                   Z-ADD     0             $ULKD1            3 0
212500100705     C                   Z-ADD     0             $ULKD3            3 0
212501160530R224 C                   Z-ADD     0             $ULKD4            3 0
212600100705     C*
212700100705     C* Variabili appoggio
212800100705     C                   Z-ADD     0             CURR
212900100705     C                   Z-ADD     0             CURC
213000100705     C*
213100100705     C* Valorizzazione campi univoci testate
213200100705     C                   CLEAR                   FMTT1
213300100705     C                   MOVEL     KNSIF         NOMSIF
213400100705     C                   MOVEL     XSCDSI        NOMDIT
213500100705     C                   MOVEL     KCDAZ         NOMAZN
213600100705     C                   MOVEL     DSPGM         NOMPGM           10
213700100705     C* recupero il sottotitolo (se c'è)
213800100706     C                   MOVEL     *BLANK        D1TIT
213900131004     C     SOTTITD1      IFNE      *BLANK
214000131004     C                   MOVEL     SOTTITD1      IDMSG
214100100705     C                   CALLB     'XRTVM'
214200100705     C                   PARM                    IDMSG            27
214300100705     C                   PARM                    TXTMSG          100
214400100705     C                   PARM                    ERRMSG            1
214500100705     C     ERRMSG        IFNE      *ON
214600100706     C                   MOVEL     TXTMSG        D1TIT
214700100705     C                   ELSE
214800100706     C                   MOVEL     *ALL'?'       D1TIT
214900100705     C                   ENDIF
215000131004     C                   ENDIF
215100141229     C*
215200141229     C*
215300141229     C* recupero il titolo con il tipo comunicazione
215400141229     C                   MOVEL     *BLANK        T1TIT
216400141229     C*
216500141229     C* Fornitori
216600141229     C                   if        SOTTITT1   <> *BLANK
216700141229     C                   MOVEL     SOTTITT1      IDMSG
216800141229     C                   endif
217100141229     C*
217200141229     C                   CALLB     'XRTVM'
217300141229     C                   PARM                    IDMSG            27
217400141229     C                   PARM                    TXTMSG          100
217500141229     C                   PARM                    ERRMSG            1
217600141229     C                   PARM      *ON           CTRMSG            1            centratura
217700141229     C                   PARM      30            LENMSG            3 0          lun output.
217800141229     C     ERRMSG        IFNE      *ON
217900141229     C                   MOVEL     TXTMSG        T1TIT
218000141229     C                   ELSE
218100141229     C                   MOVEL     *ALL'?'       T1TIT
218200141229     C                   ENDIF
218300141229     C*
218400100705     C* Inizializzazione/Decodifica prima videata
218500100705     C                   EXSR      INZD1
218600100705     C                   MOVE      *OFF          $INZD1
218700100705     C*
218800100705     C                   ENDSR
218900100705     C/EJECT
219000100705     C************************************************************
219100100705     C* REPERIMENTO DATI SOCIETA'
219200100705     C************************************************************
219300100705     C     REPSOC        BEGSR
219400100705     C*
219500100705     C                   CALL      'SDGSOCDR'
219600100705     C                   PARM                    TIPXSC            6
219700100705     C                   PARM                    SOCXSC            3
219800100705     C                   PARM                    CDSXSC            9 0
219900100705     C                   PARM                    MODXSC            3
220000100705     C                   PARM      *BLANKS       RTNXSC            1
220100100705     C                   PARM                    XSOCDS
220200100705     C                   PARM                    KPJBA
220300100705     C*
220400100705     C                   ENDSR
220500100705     C/EJECT
220600100705     C************************************************************
220700100705     C* REPERIMENTO AUTORIZZAZIONI UTENTE
220800100705     C************************************************************
220900100705     C     REPAUT        BEGSR
221000100705     C*
221100100705     C                   CLEAR                   AUTDS
221200100705     C*
221300100705     C                   MOVEL     XSCSOC        XCASOC
221400100705     C                   MOVEL     XSCDUN        XCAUNI
221500100705     C                   Z-ADD     0             XCAGRP
221600100705     C                   MOVEL     KNMUS         XCAPRF
221700100705     C                   MOVEL     'CONCIV'      XCAFNC
221800100705     C                   MOVEL     *ZERO         XCAVFU
221900100705     C                   MOVE      'CK'          XCATCK
222000100705     C                   MOVE      '1'           XCANAU
222100100705     C*
222200100705     C                   CALLB     'XCHKAUT'
222300100705     C                   PARM                    AUTDS
222400100705     C*
222500100705     C* errori/non abilitazione
222600100705     C     XCARTN        IFNE      0
222700100705     C                   MOVE      *ON           $fine
222800100705     C                   EXSR      ENDPGM
222900100705     C                   ELSE
223000100705     C                   MOVEL     XCADDS        CONCIVDS
223100100705     C* Se non autorizzati alla funzione                                     ed
223200100705     C     ESTALL        IFEQ      *OFF
223300100705     C                   MOVE      XCAFNC        XNOFNC
223400100705     C                   MOVE      XCAPRF        XNOPRF
223500100705     C                   MOVE      XCACUT        XNOCUT
223600100705     C                   MOVE      XCATCK        XNOTCK
223700100705     C                   MOVE      '1'           XNOERR
223800100705     C                   CALL      'XNOAUTR'
223900100705     C                   PARM                    XNOADS
224000100705     C                   MOVE      *ON           $fine
224100100705     C                   EXSR      ENDPGM
224200100705     C                   ENDIF
224300100705     C                   ENDIF
224400100705     C*
224500100705     C                   ENDSR
224600100705     C/EJECT
228000100830     C************************************************************
228100100830     C* Messaggio errore 2
228200100830     C************************************************************
228300100830     C     MsgErr2       BEGSR
228400100830     C*
228500100830     C                   Reset                   SDGMSGDS
228600100830     C*
228700100830     C                   MoveL     'S'           MSGEMV
228800100830     C                   Z-Add     10            MSGRGP
228900100830     C                   MoveL     'N'           MSGRSP
229000100830     C                   MoveL     '  '          MSGTPR
229100100830     C*
229200100830     C                   MoveL     XPaPar        $P1PRO2184
229300100830     C*
229400100830     C                   call      'SDGMSGR'
229500100830     C                   parm                    SDGMSGDS
229600100830     C                   parm                    XMsgTxt         135
229700100830     C*
229800100830     C* Modo di inizializzazione della window:
229900100830     C* ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
230000100830     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
230100100830     C*                       per mantenere il sottofondo del video princip.)
230200100830     C                   parm      '1'           XMsgInz           1
230300100830     C*
230400100830     C* PRO2184  - Fallito reperimento recupero parametro modulo &1
230500100830     C                   parm      'PRO2184'     XMsgCd1           7
230600100830     C                   parm                    XMsgCd2           7
230700100830     C                   parm                    XMsgCd3           7
230800100830     C                   parm                    XMsgSn1           7
230900100830     C                   parm                    XMsgCdTxT        63
231000100830     C                   parm                    MsgCdTit         14
231100100830     C                   parm                    MsgClPrtf         1
231200100830     C                   parm      $P1PRO2184    VarMsg1         100
231300100830     C                   parm                    VarMsg2         100
231400100830     C                   parm                    VarMsg3         100
231500100830     C*
231600100830     C                   ENDSR
231700100830     C/EJECT
231800100820     C************************************************************
231900100820     C* Rerimento libreria per scarico su supporto magnetico
232000100820     C************************************************************
232100100820     C     RepLibFile    BEGSR
232200100820     C*
232300100820     C                   Clear                   ANP022DS
232400100820     C*
232500100820     C                   CALLB     'XPAR'
232600100820     C                   Parm                    XScSoc
232700100820     C                   Parm      'SCARSUMA'    XPaPar            8
232800100820     C                   Parm                    XPaOut         2000
232900100820     C                   Parm      *OFF          XPaErr            1
233000100820     C                   Parm      ' '           XPaRic            1
233100100820     C*
233200100820     C                   if        XPaErr      = *On
233300100820     C*
233400100820     C* Errore: fallito reperimento recupero parametro modulo &1
233500100820     C* Valorizzo le variabili del msg PRO2184
233600100830     C                   EXSR      MsgErr2
233700100830     C                   Eval      $Fine       = *On
233800100827     C                   EXSR      ENDPGM
233900100820     C*
234000100820     C                   else
234100100820     C                   MoveL     XPaOut        ANP022DS
234200100820     C                   endif
234300100820     C*
234400100820     C                   ENDSR
234500100820     C/EJECT
234600100705     C************************************************************
234700100705     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
234800100705     C************************************************************
234900100705     C     REPDFT        BEGSR
235000100705     C*
235100100705     C* preparo i parametri di chiamata al driver
235200100705     C                   CLEAR                   XMPEDS
235300100705     C                   EVAL      TpChiamata = '01'
235400100705     C                   EVAL      NChiavi = 5
235500100705     C                   EVAL      VLogica = 2
235600100705     C*
235700100705     C* preparo le chiavi per la chiamata al driver
235800100705     C                   CLEAR                   XMPEDS1
235900100705     C                   MOVEL     XSCSOC        DSMPESOC
236000100705     C                   MOVEL     DSPGM         DSMPEPGM
236100100705     C                   MOVEL     KNMUS         DSMPEPRF
236200100705     C* Si reperiscono i dati di default a seconda delle modalità con
236300100705     C* cui il pgm è usato
236400100705     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
236500100705     C* MPETIP = 'PV'  pgm richiamato da menù
236600100705     C* MPETIP = 'PB'  x lancio Batch
236700100705     C                   MOVEL     'PB'          DSMPETIP
236800100705     C*
236900100705     C                   CALLB     'XMPE'
237000100705     C                   PARM                    XMPEDS
237100100705     C                   PARM                    XMPEDS1
237200100705     C*
237300100705     C     Risultato     IFEQ      '0'
237400100705     C                   MOVEL     DSMPEPAR      DSMPE
237500100705     C* nel caso sia la prima volta che l'utente apre questa anagrafica
237600100705     C* i suoi valori di default vengono decisi a pgm
237700100705     C                   ELSE
237800100705     C**************+    MOVE      *On           $Val
237900100705     C                   EndIf
238000100705     C*
238100100705     C                   ENDSR
238200100705     C/EJECT
238300100705     C************************************************************
238400100705     C* AGGIORNAMENTO VALORI ASSUNTI
238500100705     C************************************************************
238600100705     C     AGGMPE        BEGSR
238700100705     C*
238800100705     C* preparo i parametri di chiamata al driver
238900100705     C                   CLEAR                   XMPEDS
239000100705     C                   EVAL      TpChiamata = '03'
239100100705     C                   EVAL      NChiavi = 5
239200100705     C                   EVAL      VLogica = 2
239300100705     C*
239400100705     C* preparo le chiavi per la chiamata al driver
239500100705     C                   CLEAR                   XMPEDS1
239600100705     C                   MOVEL     XSCSOC        DSMPESOC
239700100705     C                   MOVEL     DSPGM         DSMPEPGM
239800100705     C                   MOVEL     KNMUS         DSMPEPRF
239900100705     C*
240000100705     C* Si memorizzano i dati di default a seconda delle modalità con
240100100705     C* cui il pgm è usato
240200100705     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
240300100705     C* MPETIP = 'PV'  pgm richiamato da menù
240400100705     C* MPETIP = 'PB'  x lancio Batch
240500100705     C*
240600100705     C                   MOVEL     'PB'          DSMPETIP
240700100705     C* dati
240800100705     C*
240900100705     C                   MOVEL     DSMPE         DSMPEPAR
241000100705     C*
241100100705     C                   CALLB     'XMPE'
241200100705     C                   PARM                    XMPEDS
241300100705     C                   PARM                    XMPEDS1
241400100705     C*
241500100705     C                   ENDSR
241600100705     C/EJECT
241700100705     C************************************************************
241800100705     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
241900100705     C************************************************************
242000100705     C     DEFOPZ        BEGSR
242100100705     C*
242200100705     C* Questa routine deve valorizzare la schiera $FC1/$FR1
242300100705     C* con S o N a seconda della utilizzabilità o meno
242400100705     C* del tasto funzione correlato
242500100705     C*
242600100705     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
242700100705     C* di compilazione
242800100705     C                   RESET                   $FC1
242900100705     C                   RESET                   $FR1
243000100705     C                   RESET                   $FC3
243100100705     C                   RESET                   $FR3
243200100705     C*
243300100705     C                   ENDSR
243400100705     C/EJECT
243500100705     C************************************************************
243600100705     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
243700100705     C************************************************************
243800100705     C     DEFCAM        BEGSR
243900100705     C*
244000100705     C* klist
244100100705     C*
244200100705     C* Variabili per gestione videate
244300100705     C*
244400100705     C                   MOVE      *BLANK        $GEST             2
244500100705     C                   MOVE      *BLANK        $FINE             1
244600100705     C                   MOVE      *BLANK        $INZD1            1
244700100705     C                   MOVE      *BLANK        $INZD3            1
244800100705     C                   MOVE      *BLANK        $LASTG            2
244900100705     C                   MOVE      *BLANK        $LASTV            2
245000100705     C*
245100100705     C* Variabili appoggio
245200100705     C*
245300100705     C                   Z-ADD     0             CURR              2 0
245400100705     C                   Z-ADD     0             CURC              2 0
245500100705     C                   MOVE      *ZEROS        WIN              99
245600100705     C*
245700100705     C* Indici
245800100705     C*
245900100705     C                   Z-ADD     0             X                 3 0
246000100705     C                   Z-ADD     0             Y                 3 0
246100100705     C*
246200100705     C                   ENDSR
246300100705     C/EJECT
246400100705     C************************************************************
246500100705     C* RIEMPIMENTO TASTI FUNZIONE
246600100705     C************************************************************
246700100705     C     RIEKEY        BEGSR
246800100705     C*
246900100705     C* Input :
247000100705     C* - $FX = schiera con tutti i testi disponibili
247100100705     C* - $FC = schiere con i flag di validità associati ai testi
247200100705     C* - $FR = schiere con i flag di REVERSE IMAGE
247300100705     C* - $ULK = ultimo testo caricato
247400100705     C* - $L01 = Lunghezza prima riga
247500100705     C* - $L02 = Lunghezza seconda riga
247600100705     C* Output :
247700100705     C* - $ULK
247800100705     C* - $KEY1= 1° riga di tasti funzione
247900100705     C* - $KEY2= 2° riga di tasti funzione
248000100705     C*
248100100705     C                   CALLB     'XKEYMSG'
248200100705     C* input
248300100705     C                   PARM                    $FX
248400100705     C                   PARM                    $FC
248500100705     C                   PARM                    $FR
248600100705     C                   PARM                    $ULK              3 0
248700100705     C* output
248800100705     C                   PARM                    $KEY1            79
248900100705     C                   PARM                    $KEY2            79
249000100705     C* input
249100100705     C                   PARM                    $L01              3 0
249200100705     C                   PARM                    $L02              3 0
249300100705     C*
249400100705     C                   PARM                    $ALLFUNCT       480
249500100705     C*
249600100705     C                   ENDSR
249700100705     C/EJECT
249800100705      *------------------------------------------------------------
249900100705** TASTI DI FUNZIONE  D1
250000100705PROMSG    *LIBL     KEY0001  01
250100100705PROMSG    *LIBL     KEY0002  02
250200100705PROMSG    *LIBL     KEY0003  03
250300100705PROMSG    *LIBL     KEY0004  04
250400100705PROMSG    *LIBL     KEY0005  05
250500100705PROMSG    *LIBL     KEY0006  06
250600100705PROMSG    *LIBL     KEY0007  07
250700100705PROMSG    *LIBL     KEY0008  08
250800100705PROMSG    *LIBL     KEY0009  09
250900100705PROMSG    *LIBL     KEY0010  10
251000100705PROMSG    *LIBL     KEY0011  11
251100100705PROMSG    *LIBL     KEY0012  12
251200100705PROMSG    *LIBL     KEY0013  13
251300110422PROMSG    *LIBL     KEY0014  14
251400110422SDGMSG    *LIBL     KEY0015  15
251500100705PROMSG    *LIBL     KEY0016  16
251600110422SDGMSG    *LIBL     SDK0010  17
251700100705PROMSG    *LIBL     KEY0018  18
251800110422PROMSG    *LIBL     KEY0248  19
251900100705PROMSG    *LIBL     KEY0020  20
252000100705PROMSG    *LIBL     KEY0021  21
252001160530SDGMSG    *LIBL     SDK0022  22              ## R224
252200100705PROMSG    *LIBL     KEY0023  23
252300100705PROMSG    *LIBL     KEY0024  24
252400100705**  ABILITAZIONE D1
2525001007050N  01
2526001007050N  02
2527001007051N  03
2528001007051N  04
2529001007050N  05
2530001007051N  06
2531001007050N  07
2532001007050N  08
2533001007050N  09
2534001007050N  10
2535001104220N  11
2536001007051N  12
2537001007050N  13
2538001104220N  14
2539001104220N  15
2540001007050N  16
2541001104220N  17
2542001007060N  18
2543001412290N  19
2544001007050N  20
2545001007051N  21
2546001605301N  22      ## R224
2547001007050N  23   Impostare a '1' se si usano le opzioni
2548001007051N  24   Lasciare sempre impostata a '1' il valore della riga 24
254900100705** TASTI DI FUNZIONE  D3
255000100705PROMSG    *LIBL     KEY0001  01
255100100705PROMSG    *LIBL     KEY0002  02
255200100705PROMSG    *LIBL     KEY0003  03
255300100705PROMSG    *LIBL     KEY0004  04
255400100705PROMSG    *LIBL     KEY0005  05
255500100705PROMSG    *LIBL     KEY0006  06
255600100705PROMSG    *LIBL     KEY0007  07
255700100705PROMSG    *LIBL     KEY0008  08
255800100705PROMSG    *LIBL     KEY0009  09
255900100705PROMSG    *LIBL     KEY0010  10
256000100705PROMSG    *LIBL     KEY0011  11
256100100705PROMSG    *LIBL     KEY0012  12
256200100705PROMSG    *LIBL     KEY0013  13
256300100705PROMSG    *LIBL     KEY0014  14
256400100705PROMSG    *LIBL     KEY0015  15
256500100705PROMSG    *LIBL     KEY0016  16
256600100705PROMSG    *LIBL     KEY0017  17
256700100705PROMSG    *LIBL     KEY0018  18
256800100705PROMSG    *LIBL     KEY0019  19
256900100705PROMSG    *LIBL     KEY0020  20
257000100705PROMSG    *LIBL     KEY0021  21
257001160530PROMSG    *LIBL     KEY0022  22
257200100705PROMSG    *LIBL     KEY0023  23
257300100705PROMSG    *LIBL     KEY0024  24
257400100705**  ABILITAZIONE D3
2575001007050N  01
2576001007050N  02
2577001007051N  03
2578001007050N  04
2579001007050N  05
2580001007050N  06
2581001007050N  07
2582001007050N  08
2583001007050N  09
2584001007050N  10
2585001007050N  11
2586001007051N  12
2587001007050N  13
2588001007050N  14
2589001007050N  15
2590001007050N  16
2591001007050N  17
2592001007050N  18
2593001007050N  19
2594001007050N  20
2595001007050N  21
2596001605300N  22
2597001007050N  23   Impostare a '1' se si usano le opzioni
2598001007051N  24   Lasciare sempre impostata a '1' il valore della riga 24
259900160610** TASTI DI FUNZIONE  D4     ## R224   aggiunti tasti funzione per D4
260000160530PROMSG    *LIBL     KEY0001  01
260100160530PROMSG    *LIBL     KEY0002  02
260200160530PROMSG    *LIBL     KEY0003  03
260300160530PROMSG    *LIBL     KEY0004  04
260400160530PROMSG    *LIBL     KEY0005  05
260500160530PROMSG    *LIBL     KEY0006  06
260600160530PROMSG    *LIBL     KEY0007  07
260700160530PROMSG    *LIBL     KEY0008  08
260800160530PROMSG    *LIBL     KEY0009  09
260900160530PROMSG    *LIBL     KEY0010  10
261000160530PROMSG    *LIBL     KEY0011  11
261100160530PROMSG    *LIBL     KEY0012  12
261200160530PROMSG    *LIBL     KEY0013  13
261300160530PROMSG    *LIBL     KEY0014  14
261400160530PROMSG    *LIBL     KEY0015  15
261500160530PROMSG    *LIBL     KEY0016  16
261600160530PROMSG    *LIBL     KEY0017  17
261700160530PROMSG    *LIBL     KEY0018  18
261800160530PROMSG    *LIBL     KEY0019  19
261900160530PROMSG    *LIBL     KEY0020  20
262000160530PROMSG    *LIBL     KEY0021  21
262100160530PROMSG    *LIBL     KEY0022  22
262200160530PROMSG    *LIBL     KEY0023  23
262300160530PROMSG    *LIBL     KEY0024  24
262400160610**  ABILITAZIONE D4          ## R224   aggiunti tasti funzione per D4
2625001605300N  01
2626001605300N  02
2627001605300N  03
2628001605301N  04
2629001605301N  05
2630001605301N  06
2631001605300N  07
2632001605300N  08
2633001605300N  09
2634001605300N  10
2635001605300N  11
2636001605301N  12
2637001605300N  13
2638001605300N  14
2639001605300N  15
2640001605300N  16
2641001605300N  17
2642001605300N  18
2643001605300N  19
2644001605300N  20
2645001605300N  21
2646001605300N  22
2647001605300N  23   Impostare a '1' se si usano le opzioni
2648001605301N  24   Lasciare sempre impostata a '1' il valore della riga 24
