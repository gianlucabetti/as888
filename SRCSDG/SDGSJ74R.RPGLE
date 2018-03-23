000100141222     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200000000     H*PARMS CVTOPT(*NONE) ACTGRP(QILE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*****************************************************************
000500000000      *
000600141222      *                Dichiarazioni di intento fornitori
000700000000      *           ---------------------------------------
000800000000      *
000900000000     F*----------------------------------------------------*
001000141222     F* Dichiarazioni di intento fornitori estratti
001100141222     FSDGDIE01L UF A E           K DISK
001200000000     F*------------
001300141222     FSDGSJ74D  CF   E             WORKSTN
001400141222     F                                     RENAME(SJ74D1:FMTD1)
001500141222     F                                     RENAME(SJ74T1:FMTT1)
001600141222     F                                     RENAME(SJ74Z1:FMTZ1)
001700000000     F* eventuali schermi supplementari
001800141222     F                                     RENAME(SJ74D2:FMTD2)
001900000000     F                                     INFDS(DSFMT)
002000000000     F*------------
002100000000     D*----------------------------------------------------*
002200000000     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
002300000000     D $ID             S             27    DIM(5) ALT($OZ)
002400000000     D* Schiere per la gestione dei tasti di funzione
002500000000     D $FM             S             27    DIM(24)
002600000000     D $FX             S             50    DIM(24)
002700000000     D $FC             S              1    DIM(24)
002800000000     D $FR             S              1    DIM(24)
002900000000     D* Video D1
003000000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003100000000     D $FX1            S             50    DIM(24) ALT($FM1)
003200000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003300000000     D $FR1            S              1    DIM(24) ALT($FC1)
003400000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
003500000000     D* funzione personalizzati
003600000000     D $PGF1           S             10    DIM(25)
003700000000     D*----------------------------------------------------*
003800000000     D $D1L01          S              3  0
003900000000     D $D1L02          S              3  0
004000000000     D*-------------
004100000000     D* Passaggio Parametri
004200000000     D KPJBA         E DS
004300000000     D*-------------
004400000000     D* DS Esterna gestione messaggi
004500000000     D SDGMSGDS      E DS
004600000000     D  STS          E                     EXTFLD(MSGSTS)
004700000000     D                                     DIM(333)
004800000000     D  JBA          E                     EXTFLD(MSGJBA)
004900000000     D                                     DIM(502)
005000000000     D*-------------
005100000000     D* Reperimento Autorizzazioni
005200000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
005300000000     D*-------------
005400000000     D* Maschera dati autorizzazioni
005500000000     D CONCIVDS      E DS
005600000000     D*-------------
005700000000     D* Reperimento nome PGM
005800000000     D STATUS         SDS           333
005900000000     D  DSPGM            *PROC
006000000000     D  PARMS            *PARMS
006100000000     D*-------------
006200000000     D* Dati di ambiente ottenuti da SDGSOCDR
006300000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
006400000000     D*-------------
006500000000     D* DS Interna per dati di output di SDGSOCDR
006600000000     D XSOCDS          DS          1000
006700000000     D*-------------
006800000000     D* Passaggio Parametri al PGM di manutenzione
006900141222     D SDGSJ74DS     E DS
007000130906     D*-------------
007100000000     D*-------------
007200000000     D* Parametri per ricerca codice ANATB00F
007300000000     D XTABDS        E DS                  inz
007400000000     D*-------------
007500000000     D* Richiamo a SDGTABDR
007600000000     D SDGTABDS      E DS                  inz
007700000000     D*-------------
007800000000     D* ricerca soggetti
007900000000     D ANA040DS      E DS                  INZ
008000000000     D*-------------
008100000000     D* DS esterna per dati soggetti
008200000000     D DVASOG        E DS                  INZ
008300000000     D*-------------
008400000000     D* Definizioni parametri driver anagrafici
008500000000     D Dvaric          S              1
008600000000     D DvaSocieta      S              3
008700000000     D DvaUnita        S              8
008800000000     D DvaStrutt       S             10
008900000000     D DvaDtRif        S               D
009000000000     D DvaLenOut       S              9B 0
009100000000     D DvaSnatura      S              1
009200000000     D DvaCodice       S              8
009300000000     D DvaLineav       S              3
009400000000     D DvaFiliale      S              3  0
009500000000     D DvaSogg         S              8
009600000000     D DvaTpInd        S              2
009700000000     D DvaCdInd        S              3
009800000000     D DvaErrore       S              1
009900000000     D DvaOutput       S           4000
010000141222     D*-------------
010100141222     D* Ricerca capoconti
010200141222     D ANA200DS      E DS                  INZ
010300141222     D*-------------
010400141222     D* Ricerca fornitori
010500141222     D ANA400DS      E DS                  INZ
010600141222     D*-------------
010700141222     D* DS esterna per dati fornitori
010800141222     D DVAFOR        E DS
010900000000     D*-------------
011000000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
011100000000     D* alla funzione personalizzata
011200141222     D $SaveDS         S                   like(SDGSJ74DS)
011300000000     D*-------------
011400000000     D* Ds per ricerca legende
011500000000     D ANA964DS      E DS                  INZ
011600000000     D*-------------
011700000000     D* Controlli validità per valore di una legenda
011800000000     D X07VALDS      E DS                  Inz
011900000000     D*-------------
012000000000     D DSFMT           DS
012100000000     D  $TASTO               369    369
012200000000     D  NRG                  370    370
012300000000     D  NCL                  371    371
012400000000     D* riga cursore window
012500000000     D  NRgWin               382    382
012600000000     D* colonna cursore window
012700000000     D  NClWin               383    383
012800000000     D*-------------
012900000000     D* DS per UserExit
013000000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
013100000000     D*-------------
013200000000     D* posizione cursore
013300000000     D CURSOR          DS
013400000000     D  RIRI                   1      2B 0 INZ
013500000000     D  R$$                    2      2
013600000000     D  COCO                   3      4B 0 INZ
013700000000     D  C$$                    4      4
013800000000     D*-------------
013900000000     D* Costanti
014000000000     D*-------------
014100000000     D* Tasti di funzione
014200000000     D F01             C                   CONST(X'31')
014300000000     D F02             C                   CONST(X'32')
014400000000     D F03             C                   CONST(X'33')
014500000000     D F04             C                   CONST(X'34')
014600000000     D F05             C                   CONST(X'35')
014700000000     D F06             C                   CONST(X'36')
014800000000     D F07             C                   CONST(X'37')
014900000000     D F08             C                   CONST(X'38')
015000000000     D F09             C                   CONST(X'39')
015100000000     D F10             C                   CONST(X'3A')
015200000000     D F11             C                   CONST(X'3B')
015300000000     D F12             C                   CONST(X'3C')
015400000000     D F13             C                   CONST(X'B1')
015500000000     D F14             C                   CONST(X'B2')
015600000000     D F15             C                   CONST(X'B3')
015700000000     D F16             C                   CONST(X'B4')
015800000000     D F17             C                   CONST(X'B5')
015900000000     D F18             C                   CONST(X'B6')
016000000000     D F19             C                   CONST(X'B7')
016100000000     D F20             C                   CONST(X'B8')
016200000000     D F21             C                   CONST(X'B9')
016300000000     D F22             C                   CONST(X'BA')
016400000000     D F23             C                   CONST(X'BB')
016500000000     D F24             C                   CONST(X'BC')
016600000000     D ENTER           C                   CONST(X'F1')
016700000000     D ROLDWN          C                   CONST(X'F4')
016800000000     D ROLLUP          C                   CONST(X'F5')
016900000000     D*-------------
017000000000     D* Sottotitolo di testata
017100000000     D SOTTIT          C                   CONST('PROMSG    *LIBL-
017200141223     D                                          COS0127')
017300000000     D*-------------
017400000000     D* Variabili per gestione videate
017500000000     D $Gest           S              2A
017600000000     D $Fine           S              1A
017700000000     D $InzD1          S              1A
017800000000     D $LastG          S              2A
017900000000     D $LastV          S              2A
018000000000     D $FlgDef         S              1A
018100000000     D*-------------
018200000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
018300000000     D CurR            S              3S 0
018400000000     D CurC            S              3S 0
018500000000     D Win             S             99A
018600000000     D*-------------
018700000000     D* Variabili appoggio specifiche della singola anagrafica
018800000000     D*  Indici
018900000000     D X               S              5S 0
019000000000     D Y               S              5S 0
019100000000     D* Indice della schiera tasti funzione
019200000000     D PosFun          S              2  0
019300000000     D*-------------
019400000000     D* Campi di lavoro
019500130930     D WIn16           S              1
019600130926     D WIn17           S              1
019700000000     D UDateISO        S               D
019800000000     D WSogg           S                   Like(DVSSogg)
019900141222     D WDtInter        S               D
020000141222     D WKcc            S              6A
020100141222     D WDtInDic        S               D
020200141222     D WDtFiDic        S               D
020300000000     C*----------------------------------------------------*
020400000000     C*                MAIN LINE PROGRAM
020500000000     C*----------------------------------------------------*
020600000000     C*
020700000000     C* inizializzazione variabili
020800000000     C                   EXSR      INZVAR
020900000000     C*
021000000000     C     $FINE         DOWEQ     *OFF
021100000000     C     $GEST         CASEQ     'D1'          GESD1
021200000000     C     $GEST         CASEQ     'D2'          GESD2
021300000000     C                   ENDCS
021400000000     C                   ENDDO
021500000000     C*
021600000000     C* fine programma
021700000000     C                   EXSR      ENDPGM
021800000000     C/EJECT
021900000000     C************************************************************
022000000000     C* Fine Programma
022100000000     C************************************************************
022200000000     C     ENDPGM        BEGSR
022300000000     C*
022400000000     C* se non esco per errore
022500141222    >C     ErrJ74        IFEQ      '0'
022600000000     C*
022700000000     C* se il COMMIT non è demandato al pgm chiamante
022800141222    >C     CMTJ74        IFEQ      '0'
022900000000     C*
023000000000     C* test se esistono transazioni attive ed eventualmente cosa fare
023100000000     C                   Z-ADD     0             EXECOD
023200000000     C                   CALL      'XRCMTI'
023300000000     C                   PARM                    EXECOD            1 0
023400000000     C*
023500000000     C                   SELECT
023600000000     C* Annulla richiesta
023700000000     C     EXECOD        WHENEQ    1
023800000000     C                   MOVEL     *OFF          $FINE
023900000000     C                   MOVEL     *HIVAL        $LASTV
024000000000     C* Conferma uscita
024100000000     C     EXECOD        WHENEQ    2
024200000000     C*                  ROLBK
024300000000     C*
024400000000     C                   ENDSL
024500000000     C*
024600000000     C                   ENDIF
024700000000     C*
024800000000     C                   ENDIF
024900000000     C*
025000000000     C* se ho confermato l'uscita proseguo
025100000000     C     $FINE         IFEQ      *ON
025200000000     C*
025300000000     C* disallocazione rcd nel caso di modifica/annullamento
025400141222    >C     OPZJ74        IFEQ      '02'
025500141222    >C     OPZJ74        OREQ      '04'
025600000000     C*
025700000000     C* chiudo in LR per disallocare record
025800000000     C                   SETON                                            LR
025900000000     C                   Else
026000000000     C                   SETON                                            RT
026100000000     C                   ENDIF
026200000000     C*
026300000000     C                   RETURN
026400000000     C*
026500000000     C                   ENDIF
026600000000     C*
026700000000     C                   ENDSR
026800000000     C/EJECT
026900000000     C************************************************************
027000000000     C* RICERCHE
027100000000     C************************************************************
027200000000     C     SEARCH        BEGSR
027300000000     C*
027400000000     C* determino Riga/Colonna del cursore
027500000000     C                   MOVE      NRG           R$$
027600000000     C                   MOVE      NCL           C$$
027700000000     C                   Z-ADD     RIRI          CURR
027800000000     C                   Z-ADD     COCO          CURC
027900000000     C*
028000000000     C                   SELECT
028100000000     C*
028200000000     C*  Soggetto
028300000000     C     H1NMFL        WHENEQ    'D1SOGG'
028400000000     C                   EXSR      F4_Sogg
028500141222     C*
028600141222     C*  Fornitore sottoconto
028700141222     C     H1NMFL        WHENEQ    'D1KSC '
028800141222     C                   EXSR      F4_Ksc
028900141223     C*
029000141223     C*  Tipo natura giuridica
029100141223     C     H1NMFL        WHENEQ    'D1TPN    '
029200141223     C                   EXSR      F4_TpN
029300130911     C*
029400000000     C*  Partita IVA
029500000000     C     H1NMFL        WHENEQ    'D1PARTIVA'
029600000000     C                   EXSR      F4_PartIva
029700000000     C*
029800000000     C*  Codice fiscale
029900000000     C     H1NMFL        WHENEQ    'D1CDFISC'
030000000000     C                   EXSR      F4_CdFisc
030100000000     C*
030200141222     C*  Sesso
030300141222     C     H1NMFL        WHENEQ    'D1SEX  '
030400141222     C                   EXSR      F4_Sex
030500000000     C*
030600141222     C*  Tipo operazione
030700141222     C     H1NMFL        WHENEQ    'D1TPOPE  '
030800141222     C                   EXSR      F4_TpOpe
030900141222     C*
031000141222     C* FMTD2 - Tipo annullamento
031100141222     C     H1NMFL        WHENEQ    'D1TPANN'
031200141222     C                   EXSR      F4_TpAnn
031300141222     C*
031400141222     C* origine
031500141222     C     H1NMFL        WHENEQ    'D1ORIGINE'
031600141222     C                   EXSR      F4_Origine
031700000000     C*
031800000000     C* ricerca non abilitata in questa posizione
031900000000     C                   OTHER
032000000000     C                   SETON                                        9899
032100000000     C*
032200000000     C                   ENDSL
032300000000     C*
032400000000     C* imposto pos. del cursore
032500000000     C                   Z-ADD     CURR          H1RIGA
032600000000     C                   Z-ADD     CURC          H1COLO
032700000000     C*
032800000000     C                   MOVE      *HIVAL        $LASTV
032900000000     C*
033000000000     C                   ENDSR
033100000000     C/EJECT
033200000000     C************************************************************
033300000000     C* Ricerca codice soggetto
033400000000     C************************************************************
033500000000     C     F4_Sogg       BEGSR
033600000000     C*
033700000000     C                   RESET                   ANA040DS
033800000000     C*
033900000000     C                   MOVEL     '01'          OPZ040
034000000000     C                   MoveL     D1Sogg        CoD040
034100000000     C                   MOVEL     ANA040DS      KPJBU
034200000000     C                   CALL      'ANA040R'
034300000000     C                   PARM                    KPJBA
034400000000     C*
034500000000     C                   MOVEL     KPJBU         ANA040DS
034600000000     C*
034700000000     C                   if        Ret040     <> '2'
034800000000     C                   MoveL     CoD040        D1Sogg
034900000000     C                   endif
035000000000     C*
035100000000     C                   ENDSR
035200000000     C/EJECT
035300141222     C************************************************************
035400141222     C* Ricerca sottoconto fornitore
035500141222     C************************************************************
035600141222     C     F4_Ksc        BEGSR
035700141222     C*
035800141222     C                   RESET                   ANA400DS
035900141222     C*
036000141222     C                   Move      XSCSOC        Societa400
036100141222     C                   Movel     '01'          Opz400
036200141222     C                   Move      D1KSC         KSC400
036300141222     C                   MoveL     ANA400DS      KPJBU
036400141222     C                   CALL      'ANA400R'
036500141222     C                   PARM                    KPJBA
036600141222     C                   MoveL     KPJBU         ANA400DS
036700141222     C*
036800141222     C                   if        KSC400     <> *BLANK
036900141222     C                   MoveL     KSC400        D1KSC
037000141222     C                   endif
037100141222     C*
037200141222     C                   ENDSR
037300141222     C/EJECT
037400141223     C************************************************************
037500141223     C* Ricerca tipo natura giuridica
037600141223     C************************************************************
037700141223     C     F4_TpN        BEGSR
037800141223     C*
037900141223     C                   Reset                   ANA964DS
038000141223     C                   Movel     '01'          Opz964
038100141223     C                   Move      *Off          All964
038200141223     C                   MoveL     'SDGDIE000'   TRc964
038300141223     C                   Movel     'DIETPN  '    Cam964
038400141223     C                   Movel     ANA964DS      KPJBU
038500141223     C                   Call      'ANA964R'
038600141223     C                   Parm                    KPJBA
038700141223     C                   Movel     KPJBU         ANA964DS
038800141223     C* Se non sei uscito con F12
038900141223     C                   if        Ret964  <>   '2'
039000141223     C                   Movel     VAL964        D1TpN
039100141223     C                   endif
039200141223     C*
039300141223     C                   ENDSR
039400141223      /EJECT
039500000000     C************************************************************
039600000000     C* Ricerca codice IVA
039700000000     C************************************************************
039800000000     C     F4_PartIva    BEGSR
039900000000     C*
040000000000     C                   RESET                   ANA040DS
040100000000     C*
040200000000     C                   MOVEL     '01'          OPZ040
040300000000     C                   MoveL     D1PartIva     IVA040
040400000000     C                   MoveL     D1Sogg        CoD040
040500000000     C                   MOVEL     ANA040DS      KPJBU
040600000000     C                   CALL      'ANA040R'
040700000000     C                   PARM                    KPJBA
040800000000     C*
040900000000     C                   MOVEL     KPJBU         ANA040DS
041000000000     C*
041100000000     C                   if        Ret040     <> '2'
041200000000     C*
041300000000     C                   MoveL     CoD040        WSogg
041400000000     C                   EXSR      CallDvaSog
041500000000     C*
041600000000     C                   if        DVAErrore  <> '1'
041700000000     C*
041800000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
041900000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
042000000000     C*
042100000000     C                   MoveL     DVSPartIVA    D1PartIva
042200000000     C*
042300000000     C                   endif
042400000000     C*
042500000000     C                   endif
042600000000     C*
042700000000     C                   ENDSR
042800000000      /EJECT
042900000000     C************************************************************
043000000000     C* Ricerca codice fiscale
043100000000     C************************************************************
043200000000     C     F4_CdFisc     BEGSR
043300000000     C*
043400000000     C                   RESET                   ANA040DS
043500000000     C*
043600000000     C                   MOVEL     '01'          OPZ040
043700000000     C                   MoveL     D1Sogg        CoD040
043800000000     C                   MoveL     D1CdFisc      CdF040
043900000000     C                   MOVEL     ANA040DS      KPJBU
044000000000     C                   CALL      'ANA040R'
044100000000     C                   PARM                    KPJBA
044200000000     C*
044300000000     C                   MOVEL     KPJBU         ANA040DS
044400000000     C*
044500000000     C                   if        Ret040     <> '2'
044600000000     C*
044700000000     C                   MoveL     CoD040        WSogg
044800000000     C                   EXSR      CallDvaSog
044900000000     C*
045000000000     C                   if        DVAErrore  <> '1'
045100000000     C*
045200000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
045300000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
045400000000     C*
045500000000     C                   MoveL     DVSCdFisc     D1CdFisc
045600000000     C*
045700000000     C                   endif
045800000000     C*
045900000000     C                   endif
046000000000     C*
046100000000     C                   ENDSR
046200000000     C/EJECT
046300000000     C************************************************************
046400141222     C* Ricerca sesso del fornitore
046500000000     C************************************************************
046600141222     C     F4_Sex        BEGSR
046700000000     C*
046800141222     C                   Reset                   ANA964DS
046900141222     C                   Movel     '01'          Opz964
047000141222     C                   Move      *Off          All964
047100141222     C                   MoveL     'SDGDIE000'   TRc964
047200141222     C                   Movel     'DIESEX  '    Cam964
047300141222     C                   Movel     ANA964DS      KPJBU
047400141222     C                   Call      'ANA964R'
047500141222     C                   Parm                    KPJBA
047600141222     C                   Movel     KPJBU         ANA964DS
047700141222     C* Se non sei uscito con F12
047800141222     C                   if        Ret964  <>   '2'
047900141222     C                   Movel     VAL964        D1Sex
048000141222     C                   endif
048100000000     C*
048200000000     C                   ENDSR
048300000000      /EJECT
048400000000     C************************************************************
048500141222     C* Ricerca tipo operazione
048600000000     C************************************************************
048700141222     C     F4_TpOpe      BEGSR
048800000000     C*
048900141222     C                   Reset                   ANA964DS
049000141222     C                   Movel     '01'          Opz964
049100141222     C                   Move      *Off          All964
049200141222     C                   MoveL     'SDGDIE000'   TRc964
049300141222     C                   Movel     'DIETPOPE'    Cam964
049400141222     C                   Movel     ANA964DS      KPJBU
049500141222     C                   Call      'ANA964R'
049600141222     C                   Parm                    KPJBA
049700141222     C                   Movel     KPJBU         ANA964DS
049800141222     C*Se non sei uscito con F12
049900141222     C                   if        Ret964  <>   '2'
050000141222     C                   Movel     VAL964        D1TpOpe
050100141222     C                   endif
050200141222     C*
050300141222     C                   ENDSR
050400141222      /EJECT
050500141222     C************************************************************
050600141222     C* Ricerca tipo annullamento
050700141222     C************************************************************
050800141222     C     F4_TpAnn      BEGSR
050900141222     C*
051000141222     C                   Reset                   ANA964DS
051100141222     C*
051200141222     C                   MoveL     '01'          Opz964
051300141229     C                   MoveL     '1'           All964
051400141229     C                   MoveL     'SDGDIE000'   Trc964
051500141229     C                   MoveL     'DIETPANN'    Cam964
051600141222     C*
051700141222     C                   MoveL     ANA964DS      KPJBU
051800141222     C                   CALL      'ANA964R'
051900141222     C                   PARM                    KPJBA
052000141222     C                   MoveL     KPJBU         ANA964DS
052100141222     C*
052200141222     C                   if        Ret964     <> '2'
052300141222     C                   MoveL     Val964        D1TpAnn
052400141222     C                   endif
052500141222     C*
052600141222     C                   ENDSR
052700141222     C/EJECT
052800141222     C************************************************************
052900141222     C* Origine
053000141222     C************************************************************
053100141222     C     F4_Origine    BEGSR
053200141222     C*
053300141222     C                   Reset                   ANA964DS
053400141222     C*
053500141222     C                   MoveL     '01'          Opz964
053600141229     C                   MoveL     'SDGDIE000'   Trc964
053700141229     C                   MoveL     'DIEORIGINE'  Cam964
053800141222     C                   MoveL     *Off          All964
053900141222     C*
054000141222     C                   MoveL     ANA964DS      KPJBU
054100141222     C                   CALL      'ANA964R'
054200141222     C                   PARM                    KPJBA
054300141222     C                   MoveL     KPJBU         ANA964DS
054400141222     C*
054500141222     C                   if        Ret964     <> '2'
054600141222     C                   MoveL     Val964        D1Origine
054700141222     C                   endif
054800141222     C*
054900141222     C                   ENDSR
055000141222     C/EJECT
055100000000     C************************************************************
055200000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
055300000000     C************************************************************
055400000000     C     GESPIE        BEGSR
055500000000     C*
055600000000     C                   SELECT
055700000000     C*
055800000000     C     $GEST         WHENEQ    'D1'
055900000000     C     $ULKD1        IFEQ      0
056000000000     C* riempimento opzioni
056100000000     C                   EXSR      F24D1
056200000000     C                   ELSE
056300000000     C                   WRITE     FMTZ1
056400000000     C                   ENDIF
056500000000     C*
056600000000     C     $GEST         WHENEQ    'D2'
056700000000     C     $ULKD1        IFEQ      0
056800000000     C* riempimento opzioni
056900000000     C                   EXSR      F24D2
057000000000     C                   ELSE
057100000000     C                   WRITE     FMTZ1
057200000000     C                   ENDIF
057300000000     C                   ENDSL
057400000000     C*
057500000000     C                   ENDSR
057600000000     C/EJECT
057700000000     C************************************************************
057800000000     C* Chiamata programma con tasto personalizzato
057900000000     C************************************************************
058000000000     C     FPerD1        BEGSR
058100000000     C*
058200000000     C* salvo la DS di ingresso
058300141222    >C                   MOVEL     SDGSJ74DS     $SaveDS
058400000000     C*
058500141222    >C                   MOVE      xscsoc        SocietaJ74
058600141222     C* passare nei campi appositi della DS SDGSJ74DS il valore
058700000000     C* della chiave della VL univoca
058800141222     C                   Eval      DtInterJ74  = WDtInter
058900141222     C                   Eval      NrInterJ74  = D1NrInter
059000000000     C*
059100141222    >C                   MOVEL     SDGSJ74DS     KPJBU
059200000000     C                   CALL      $PGF1(PosFun)                        22
059300000000     C                   PARM                    KPJBA
059400000000     C                   PARM                    NOMPGM
059500000000     C*
059600000000     C* ripristino la DS di ingresso
059700141222    >C                   MOVEL     $SaveDS       SDGSJ74DS
059800000000     C*
059900000000     C                   ENDSR
060000000000     C/EJECT
060100000000     C************************************************************
060200000000     C* GESTIONE VIDEO RECORD D1
060300000000     C************************************************************
060400000000     C     GESD1         BEGSR
060500000000     C* inizializzazione videata
060600000000   B1C     $INZD1        IFEQ      *ON
060700000000     C                   EXSR      INZD1
060800000000     C                   MOVE      *OFF          $INZD1
060900000000   E1C                   ENDIF
061000000000     C* emissione piede videata se proveniente da altra
061100000000     C* salvataggio valore formato di provenienza
061200000000   B1C     $LASTV        IFNE      'D1'
061300000000     C                   WRITE     FMTT1
061400000000     C                   EXSR      GESPIE
061500000000     C                   MOVE      $LASTV        $LASTG
061600000000     C                   MOVE      'D1'          $LASTV
061700000000   E1C                   ENDIF
061800000000     C*
061900000000     C* se esistono errori sulla videata
062000000000     C* emetto la write del formato a indicatori spenti per vedere
062100000000     C* le eventuali decodifiche
062200130926   B1C                   if        (*IN99       = *On   or
062300131001   B1C                              *In16       = *On      )  and
062400130926     C                             $GEST        = 'D1'
062500000000     C                   MOVEA     *IN           WIN
062600000000     C                   MOVE      *ALL'0'       IN5098           49
062700130926     C                   MOVEA     IN5098        *IN(50)
062800130930     C                   Eval      WIn16        = *In16
062900130930     C                   Eval      *In16        = *Off
063000000000     C                   WRITE     FMTD1
063100000000     C                   MOVEA     WIN           *IN
063200130930     C                   Eval      *In16        = WIn16
063300000000   E1C                   ENDIF
063400000000     C* Inizializzazione indice tasti funzione personalizzati
063500000000     C                   Eval      PosFun = 25
063600000000     C*
063700000000   B1C                   SELECT
063800000000     C* Annullamento
063900141222    >C     OPZJ74        WHENEQ    '04'
064000000000     C* Visualizzazione
064100141222    >C     OPZJ74        OREQ      '05'
064200000000     C                   WRITE     FMTD1
064300000000     C                   EXFMT     PROTECT
064400000000     C* Immissione/Modifica/Copia
064500000000   X1C                   OTHER
064600000000     C                   EXFMT     FMTD1
064700000000   E1C                   ENDSL
064800000000     C*
064900000000     C* azzero la posizione del cursore
065000000000     C                   Z-ADD     0             H1RIGA
065100000000     C                   Z-ADD     0             H1COLO
065200000000     C*
065300000000     C* controllo abilitazione tasti funzione
065400000000     C                   EXSR      CTRKD1
065500000000     C*
065600000000   B1C     *IN99         IFEQ      *OFF
065700000000     C*
065800000000   B2C                   SELECT
065900000000     C* ROLLUP
066000000000    >C     $TASTO        WHENEQ    ROLLUP
066100000000    >C                   MOVE      'D2'          $GEST
066200000000     C* F2=Decodifica
066300000000     C     $TASTO        WHENEQ    F02
066400000000     C                   EXSR      F02D1
066500000000     C* F3=Fine
066600000000     C     $TASTO        WHENEQ    F03
066700000000     C                   EXSR      F03D1
066800000000     C* F4=Ricerca
066900000000     C     $TASTO        WHENEQ    F04
067000000000     C                   EXSR      SEARCH
067100000000     C* F5=Refresh
067200000000     C     $TASTO        WHENEQ    F05
067300000000     C                   EXSR      F05D1
067400000000     C* F8=Successivo
067500000000     C     $TASTO        WHENEQ    F08
067600000000     C                   EXSR      F08D1
067700000000     C* F12=Ritorno
067800000000     C     $TASTO        WHENEQ    F12
067900000000     C                   EXSR      F12D1
068000000000     C* F24=Altri tasti
068100000000     C     $TASTO        WHENEQ    F24
068200000000     C                   EXSR      F24D1
068300000000     C* Personalizzata
068400141222     C* NB chiamata NON possibile perchè SDGSJ74DS più lunga
068500000000     C* di KPJBU 256 caratteri                          a
068600000000     C**** $PGF1(PosFun) WHENNE    *BLANK
068700000000     C****               EXSR      FPerD1
068800000000     C*
068900000000   X2C                   OTHER
069000000000     C                   EXSR      CTRD1
069100000000     C* aggiungere le chiamate ai controlli delle videate supplementari
069200000000    >C     *IN99         IFEQ      *OFF
069300000000    >C                   EXSR      CTRD2
069400130926   B1C                   if        *IN99       = *On
069500000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
069600000000    >C* indicatori uguali in videate differenti, genererei conflitti
069700000000    >C                   MOVE      *ALL'0'       IN5098           49
069800000000    >C                   MOVEA     IN5098        *IN(50)
069900000000    >C                   SETON                                        9996                       96
070000000000    >C                   ENDIF
070100000000    >C                   ENDIF
070200000000     C*
070300000000   B3C     $TASTO        IFEQ      F06
070400000000     C     *IN99         ANDEQ     *OFF
070500000000     C* eseguo aggiornamento
070600000000     C                   EXSR      AGGANA
070700000000     C* eseguo operazioni del dopo-aggiornamento
070800000000   B4C     *IN99         IFEQ      *OFF
070900000000     C                   EXSR      GESAGG
071000000000   E4C                   ENDIF
071100000000   E3C                   ENDIF
071200000000     C*
071300000000   E2C                   ENDSL
071400000000     C*
071500000000   E1C                   ENDIF
071600000000     C*
071700000000     C                   ENDSR
071800000000     C/EJECT
071900000000     C************************************************************
072000000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
072100000000     C************************************************************
072200000000     C     CTRKD1        BEGSR
072300000000     C*
072400000000     C                   SETOFF                                       99
072500000000     C                   MOVEA     $FC1          $FC
072600000000     C                   EXSR      CTRKEY
072700000000     C     *IN99         IFEQ      *ON
072800000000     C                   SETON                                        97
072900000000     C                   ENDIF
073000000000     C*
073100000000     C                   ENDSR
073200000000     C/EJECT
073300000000     C************************************************************
073400000000     C* INIZIALIZZAZIONE VIDEATA DATI
073500000000     C************************************************************
073600000000     C     INZD1         BEGSR
073700000000     C*
073800000000     C                   CLEAR                   FMTD1
073900000000    >C                   CLEAR                   FMTD2
074000141222     C*
074100141222     C                   Eval      D1On1       = XScOn
074200141222     C                   Eval      D1On2       = XScOn
074300141222     C                   Eval      D1Off1      = XScOff
074400141222     C                   Eval      D1Off2      = XScOff
074500000000     C*
074600141222     C                   Eval      D1Anno      = AnnoJ74
074700000000     C*
074800000000     C* per tutte le opzioni NON immissione
074900141222     C     OPZJ74        IFNE      '01'
075000141222     C                   Eval      DIESocieta  = SocietaJ74
075100141222     C                   Eval      DIEDtInter  = DtInterJ74
075200141222     C                   Eval      DIENrInter  = NrInterJ74
075300000000     C*
075400000000     C     *IN22         DOUEQ     *OFF
075500000000     C     MSGRTN        ORNE      'R'
075600000000     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
075700141222     C     OPZJ74        IFEQ      '02'
075800141222    >C     OPZJ74        OREQ      '04'
075900141222     C     K03DIE01      CHAIN     SDGDIE01L                          2122
076000000000     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
076100000000   X3C                   ELSE
076200141222     C     K03DIE01      CHAIN(N)  SDGDIE01L                          21
076300000000   E3C                   ENDIF
076400141222     C*
076500141222     C* Gestione RCD allocato
076600141222   B3C     *IN22         IFEQ      *ON
076700141222    >C                   MoveL     'SDGDIE01L'   MSGFIL
076800141222     C                   EXSR      MSGPGM
076900141222   E3C                   ENDIF
077000000000     C*
077100000000   E2C                   ENDDO
077200000000     C*
077300000000     C* se il rcd da gestire non viene trovato
077400000000     C* oppure è allocato da altro pgm
077500000000     C* ritorno al pgm guida avvertendolo dell'errore
077600141222   B2C     *IN21         IFEQ      *ON
077700141222     C     *IN22         OREQ      *ON
077800000000     C                   MOVE      *ON           $FINE
077900141222    >C                   MOVE      '1'           ERRJ74
078000000000     C*
078100000000     C* altrimenti
078200000000   X2C                   ELSE
078300000000     C* riempio la videata
078400000000     C                   EXSR      RIED1
078500000000    >C* Aggiungere le routine di riempimento dei record succcessivi
078600000000    >C                   EXSR      RIED2
078700000000     C* richiamo routine dei ctrl per decodificarla
078800000000     C                   EXSR      CTRD1
078900000000    >C* Aggiungere le routine di controllo dei record succcessivi
079000000000    >C                   EXSR      CTRD2
079100000000     C                   MOVE      *ALL'0'       IN5098
079200000000     C                   MOVEA     IN5098        *IN(50)
079300000000   E2C                   ENDIF
079400000000     C*
079500000000     C* per immissione
079600000000   X1C                   ELSE
079700000000     C*
079800000000     C* riempio con i default di immissione  (se reperiti)
079900000000     C                   IF        $FlgDef     = *on
080000000000     C                   EXSR      RIEDEF
080100000000     C                   ENDIF
080200141222     C*
080300141222     C* imposto i campi chiave ricevuti anche se si è in immissione
080400141222     C* perchè potrebbero essere stati passati dei default di chiave
080500000000     C*
080600000000     C* decodifico le costanti
080700000000     C                   EXSR      F02D1
080800000000   E1C                   ENDIF
080900141222     C*
081000141222     C     OPZJ74        IFEQ      '01'
081100141222    >C     OPZJ74        OREQ      '03'
081200141222     C                   Eval      D1Origine   = 'M'
081300141222     C*
081400141222     C                   endif
081500000000     C*
081600000000     C* imposto quegli attributi della videata che sono modificabili
081700000000     C* al richiamo del determinato pgm
081800000000     C                   EXSR      ATRD1
081900000000     C*
082000000000     C                   ENDSR
082100000000     C/EJECT
082200000000     C************************************************************
082300000000     C* RIEMPIMENTO VIDEATA  D1
082400000000     C************************************************************
082500000000     C     RIED1         BEGSR
082600141222     C*
082700141222     C                   Eval      D1Anno      = DIEAnno
082800141222     C                   if        DIEDtInter <> *Loval
082900141222     C     *JobRun       Move      DIEDtInter    D1DtInter
083000141222     C                   endif
083100141222     C                   Eval      D1NrInter   = DIENrInter
083200141222     C                   if        DIEDogana   = '1'
083300141222     C                   Eval      D1Dogana    = XScOn
083400141222     C                   else
083500141222     C                   Eval      D1Dogana    = XScOff
083600141222     C                   endif
083700141222     C                   Eval      D1Sogg      = DIESogg
083800141222     C                   Eval      D1Ksc       = DIEKsc
083900141223     C                   Eval      D1TpN       = DIETpN
084000141222     C                   Eval      D1PartIVA   = DIEPartIVA
084100141222     C                   Eval      D1CdFisc    = DIECdFisc
084200141222     C                   Eval      D1CogRag    = DIECogRag
084300141222     C                   Eval      D1Nome      = DIENome
084400141222     C                   Eval      D1Sex       = DIESex
084500141222     C                   Eval      D1Descr     = DIEDescr
084600141222     C                   Eval      D1TpOpe     = DIETpOpe
084700141222     C                   Eval      D1Importo   = DIEImporto
084800141222     C                   if        DIEDtInDic <> *Loval
084900141222     C     *JobRun       Move      DIEDtInDic    D1DtInDic
085000141222     C                   endif
085100141222     C                   if        DIEDtFiDic <> *Loval
085200141222     C     *JobRun       Move      DIEDtFiDic    D1DtFiDic
085300141222     C                   endif
085400141222     C                   if        DIEAnn      = '1'
085500141222     C                   Eval      D1Ann       = XScOn
085600141222     C                   else
085700141222     C                   Eval      D1Ann       = XScOff
085800141222     C                   endif
085900141222     C                   Eval      D1TpAnn     = DIETpAnn
086000141222     C                   Eval      D1Origine   = DIEOrigine
086100000000     C*
086200000000     C                   ENDSR
086300000000     C************************************************************
086400000000     C* RIPRISTINA GLI ATTRIBUTI DELLA VIDEATA
086500000000     C************************************************************
086600000000     C     ATRD1         BEGSR
086700000000     C*
086800000000    >C* in questa routine bisogna attivare indicatori o campi programma
086900000000    >C* che gestiscono gli attributi di uno più campi di questa videata;
087000000000    >C* attenzione : non è questo il posto in cui eseguire controlli,
087100000000    >C*  dovrebbero già arrivare dei flag
087200000000     C*
087300000000     C                   SETOFF                                       02
087400000000     C*
087500000000     C* in caso di manutenzione
087600141222    >C     OPZJ74        IFEQ      '02'
087700000000     C* proteggo i campi chiave
087800000000     C                   SETON                                        02        PR campi key
087900000000   E3C                   ENDIF
088000130911     C*
088100130911     C* in caso di insertimento o copia
088200130911     C* abilito i campi D1SOGG e D1TPN
088300141222    >C     OPZJ74        IFEQ      '01'
088400141222    >C     OPZJ74        orEQ      '03'
088500130911     C                   SETON                                        11        PR campi key
088600130911   E3C                   ENDIF
088700000000     C*
088800000000     C                   ENDSR
088900000000     C/EJECT
089000000000     C************************************************************
089100000000     C* RIEMPIMENTO CAMPI DAI DEFAULT
089200000000     C************************************************************
089300000000     C     RIEDEF        BEGSR
089400000000     C*
089500141223     C                   Eval      D1Anno      = AnnoJ74
089600141223     C                   Eval      D1PartIVA   = PartIVAJ74
089700141223     C                   Eval      D1CdFisc    = CdFiscJ74
089800141223     C                   Eval      D1Sogg      = SoggJ74
089900141223     C                   Eval      D1Ksc       = KscJ74
090000141223     C                   Eval      D1TpOpe     = TpOpeJ74
090100141223     C*
090200141223     C                   Select
090300141223     C* solo gli attivi, quindi imposto non annullato
090400141223     C                   When      AnnJ74      = '2'
090500141223     C                   Eval      D1Ann       = XScOff
090600141223     C                   Eval      D1TpAnn     = TpAnnJ74
090700141223     C* solo gli non attivi, quindi imposto annullato
090800141223     C                   When      AnnJ74      = '3'
090900141223     C                   Eval      D1Ann       = XScOn
091000141223     C                   Eval      D1TpAnn     = TpAnnJ74
091100141223     C                   other
091200141223     C                   Eval      D1Ann       = XScOff
091300141223     C                   Eval      D1TpAnn     = *Blank
091400141223     C                   endSl
091500000000     C*
091600000000     C                   ENDSR
091700000000     C/EJECT
091800000000     C************************************************************
091900000000     C* GESTIONE F02 VIDEO D1
092000000000     C************************************************************
092100000000     C     F02D1         BEGSR
092200000000     C*
092300000000     C* per decodificare la videata richiamo la routine dei ctrl
092400000000     C* e poi disattivo gli eventuali messaggi di errore
092500000000     C                   EXSR      CTRD1
092600000000     C*
092700000000     C                   MOVE      *ALL'0'       IN5098
092800000000     C                   MOVEA     IN5098        *IN(50)
092900000000     C*
093000000000     C                   ENDSR
093100000000     C/EJECT
093200000000     C************************************************************
093300000000     C* GESTIONE F03 VIDEO D1
093400000000     C************************************************************
093500000000     C     F03D1         BEGSR
093600000000     C*
093700000000     C                   MOVE      *ON           $FINE
093800141222    >C                   MOVE      '1'           RETJ74
093900000000     C* fine programma
094000000000     C                   EXSR      ENDPGM
094100000000     C*
094200000000     C                   ENDSR
094300000000     C/EJECT
094400000000     C************************************************************
094500000000     C* GESTIONE F05 VIDEO D1
094600000000     C************************************************************
094700000000     C     F05D1         BEGSR
094800000000     C*
094900000000     C                   MOVE      *ON           $INZD1
095000000000     C*
095100000000     C                   ENDSR
095200000000     C/EJECT
095300000000     C************************************************************
095400000000     C* GESTIONE F08 VIDEO D1
095500000000     C************************************************************
095600000000     C     F08D1         BEGSR
095700000000     C*
095800000000     C                   MOVE      *ON           $FINE
095900141222    >C                   MOVE      '0'           RETJ74
096000000000     C*
096100000000     C                   ENDSR
096200000000     C/EJECT
096300000000     C************************************************************
096400000000     C* GESTIONE F12 VIDEO D1
096500000000     C************************************************************
096600000000     C     F12D1         BEGSR
096700000000     C*
096800000000     C                   MOVE      *ON           $FINE
096900141222    >C                   MOVE      '2'           RETJ74
097000000000     C*
097100000000     C                   ENDSR
097200000000     C/EJECT
097300000000     C************************************************************
097400000000     C* GESTIONE F24 VIDEO D1
097500000000     C************************************************************
097600000000     C     F24D1         BEGSR
097700000000     C*
097800000000     C                   MOVEA     $FX1          $FX
097900000000     C                   MOVEA     $FC1          $FC
098000000000     C                   MOVEA     $FR1          $FR
098100000000     C                   Z-ADD     $ULKD1        $ULK
098200000000     C                   Z-ADD     $D1L01        $L01
098300000000     C                   Z-ADD     $D1L02        $L02
098400000000     C                   EXSR      RIEKEY
098500000000     C                   Z-ADD     $ULK          $ULKD1            3 0
098600000000     C                   MOVEA     $FC           $FC1
098700000000     C                   MOVEL     $KEY1         Z1KE1
098800000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
098900000000     C                   MOVEL     $KEY2         Z1KE2
099000000000     C                   WRITE     FMTZ1
099100000000     C*
099200000000     C                   ENDSR
099300000000     C/EJECT
099400000000     C************************************************************
099500000000     C* CONTROLLO VIDEATA
099600000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
099700000000     C* 95 96 97 98
099800000000     C************************************************************
099900141223     C     CTRD1         BEGSR
100000000000     C*
100100000000     C                   SETOFF                                       99
100200000000     C*
100300000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
100400000000     C*
100500141222     C     OPZJ74        IFNE      '04'
100600130911     C*
100700141222     C                   EXSR      CtrDtInter
100800141222     C                   EXSR      CtrNrInter
100900141223     C*
101000141223     C                   EXSR      CtrDogana
101100141223     C*
101200141223     C                   EXSR      CtrSogg
101300141223     C*
101400141223     C                   EXSR      CtrKsc
101500141223     C*
101600141229     C*--------------------------------------------------
101700141229     C* FORNITORE NAZIONALE
101800141223     C                   if        D1Dogana   <>  XScOn
101900141223     C                   EXSR      CtrTpN
102000000000     C*
102100130919     C                   EXSR      CtrPIVA_CDF
102200130919     C*
102300130919     C                   EXSR      CtrCogRag
102400000000     C*
102500141229     C*
102600141223     C* controllo alcuni dati solo se persona fisica
102700141223     C                   if        D1TpN      = '1'
102800000000     C                   EXSR      CtrNome
102900141223     C                   EXSR      CtrSex
103000141223     C                   else
103100141223     C                   Eval      D1Nome     = *Blank
103200141223     C                   Eval      D1Sex      = *Blank
103300141229     C                   Clear                   D1SexD
103400141223     C                   endif
103500141222     C*
103600141229     C* FORNITORE extra NAZIONALE (riferimento DOGANA)
103700141229     C                   else
103800141223     C                   Eval      D1TpN      = *Blank
103900141229     C                   Eval      D1TpND     = *Blank
104000141223     C                   Eval      D1PartIVA  = *Blank
104100141223     C                   Eval      D1CdFisc   = *Blank
104200141223     C                   if        D1Ksc     <> *Blank
104300150122     C                   if        D1CogRag  =  *Blank
104400141223     C                   Eval      D1CogRag   = DVFDesBrev
104500150122     C                   endif
104600141223     C                   else
104700141223     C                   Eval      D1CogRag   = 'DOGANA'
104800141223     C                   endif
104900141223     C                   Eval      D1Nome     = *Blank
105000141223     C                   Eval      D1Sex      = *Blank
105100141229     C                   Eval      D1SexD     = *Blank
105200141229     C*
105300141223     C                   endif
105400141229     C*--------------------------------------------------
105500141222     C*
105600141222     C                   EXSR      CtrTpOpe
105700141222     C*
105800141222     C                   EXSR      CtrImporto
105900141222     C*
106000141222     C                   EXSR      CtrDtPer
106100000000     C*
106200000000     C*
106300000000     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
106400141222     C     OPZJ74        IFEQ      '01'
106500141222    >C     OPZJ74        OREQ      '03'
106600141222     C*
106700141222     C* imposto i campi chiave e verifico l'esistenza del record
106800141222     C*
106900141222     C                   MoveL     XscSoc        DIESocieta
107000141222     C                   MoveL     WDtInter      DIEDtInter
107100150120     C                   Move      D1NrInter     DIENrInter
107200141222     C     K03DIE01      SETLL     SDGDIE01L                              21
107300141222     C     *IN21         IFEQ      '1'
107400141222     C                   SETON                                        9099
107500141222     C                   ENDIF
107600141222     C                   ENDIF
107700000000     C*
107800000000     C                   ENDIF
107900000000     C*
108000000000     C                   ENDSR
108100000000     C/EJECT
108200130919     C***********************************************************
108300141222     C* Controllo la data del protocollo interno
108400130919     C***********************************************************
108500141222     C     CtrDtInter    BEGSR
108600130919     C*
108700141222     C                   Clear                   WDtInter
108800141222     C*
108900141222     C                   if        D1DtInter   = 0
109000141223     C                   SETON                                        5099
109100141222     C                   else
109200141222     C     *Jobrun       Test(d)                 D1DtInter              21
109300141222     C*
109400141222     C                   if        *In21       = *Off
109500141222     C     *Jobrun       move      D1DtInter     WDtInter
109600141222     C                   else
109700141223     C                   SETON                                        5199
109800141222     C                   endif
109900141222     C*
110000141222     C                   endif
110100130919     C*
110200130919     C                   ENDSR
110300130919     C/EJECT
110400130919     C***********************************************************
110500141222     C* Controllo numero protocollo interno
110600130919     C***********************************************************
110700141222     C     CtrNrInter    BEGSR
110800130919     C*
110900150120     C                   if        D1NrInter   = *Blank
111000141222     C                   SETON                                        5299
111100141222     C                   endif
111200141222     C*
111300141222     C                   ENDSR
111400141222     C/EJECT
111500141222     C***********************************************************
111600141222     C* Controllo flag dogana
111700141222     C***********************************************************
111800141222     C     CtrDogana     BEGSR
111900141222     C*
112000141222     C                   if        D1Dogana   <> XScOn   and
112100141222     C                             D1Dogana   <> XScOff
112200141223     C                   SETON                                        5399
112300141222     C                   endif
112400150108     C*
112500150108     C* se Dogana, ammesso solo Operazione specifica
112501150414R179 C* o Operazioni fino a concorrenza di importo (dal 13/04/15)
112600150108     C                   if        D1Dogana    = XScOn   and
112700150108     C                             D1TpOpe    <> '1'
112701150414R179 C                             and
112702150414R179 C                             D1TpOpe    <> '2'
112800150108     C                   SETON                                        7199
112900150108     C                   endif
113000141222     C*
113100141222     C                   ENDSR
113200141222     C/EJECT
113300141222     C***********************************************************
113400141222     C* Controllo codice soggetto
113500141222     C***********************************************************
113600141222     C     CtrSogg       BEGSR
113700141222     C*
113800141222     C                   Clear                   D1SoggD
113900141222     C*
114000141222     C     D1SOGG        IFNE      *BLANK
114100141222     C                   CALLb     'XALLINEA'
114200141222     C                   PARM                    D1SOGG
114300141222     C                   PARM      8             LEN               5 0
114400141222     C                   PARM                    XSCINP
114500141222     C                   ENDIF
114600141222     C*
114700141222     C                   MoveL     D1Sogg        WSogg
114800141222     C                   if        D1Sogg     <> *Blank
114900141222     C                   EXSR      CallDvaSog
115000141222     C*
115100141222     C                   if        DVAErrore  <> '1'
115200141222     C*
115300141222     C                   Eval      %subst(DVASOG:1:DVALENOUT)
115400141222     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
115500141222     C*
115600141222     C                   MoveL     DVSDes        D1SoggD
115700141222     C*
115800141222     C                   else
115900141222     C*
116000141222     C                   Eval      *In16       = *On
116100141222     C*
116200141222     C                   endif
116300141222     C*
116400141222     C                   endif
116500141222     C*
116600141222     C                   ENDSR
116700141222     C/EJECT
116800141222     C************************************************************
116900141222     C* Chiamata al driver dei soggetti
117000141222     C************************************************************
117100141222     C     CallDvaSog    BEGSR
117200141222     C*
117300141222     C                   EVAL      DVALenOut = %Size(DVASOG)
117400141222     C*
117500141222     C                   CALLB     'NDDVASOG'
117600141222     C                   PARM      '1'           DVARIC
117700141222     C                   PARM      XScSoc        DVASOCIETA
117800141222     C                   PARM                    DVAUNITA
117900141222     C                   PARM      'DVASOG    '  DVASTRUTT
118000141222     C                   PARM                    DVADTRIF
118100141222     C                   PARM                    DVALENOUT
118200141222     C* Se si accede con il codice cli/for impostare i campi seguenti
118300141222     C* se si accede con il soggetto ometterli e impostare gli altri
118400141222     C*****              PARM                    DVASNATURA
118500141222     C*****              PARM                    DVACODICE
118600141222     C*****              PARM                    DVALINEAV
118700141222     C*****              PARM                    DVAFILIALE
118800141222     C                   PARM                    *omit
118900141222     C                   PARM                    *omit
119000141222     C                   PARM                    *omit
119100141222     C                   PARM                    *omit
119200141222     C                   PARM      WSogg         DVSSOGG
119300141222     C                   PARM      *Blank        DVSTPIND
119400141222     C                   PARM      *Blank        DVSCDIND
119500141222     C                   PARM                    DVAERRORE
119600141222     C                   PARM                    DVAOUTPUT
119700141222     C*
119800141222     C                   ENDSR
119900141222     C/EJECT
120000141222     C************************************************************
120100141222     C* Controllo fornitore
120200141222     C************************************************************
120300141223     C     CtrKsc        BEGSR
120400141222     C*
120500141222     C                   Clear                   D1KscD
120600141222     C*
120700141222     C                   if        D1KSC      <> *Blanks
120800141222     C*
120900141222     C                   CALLB     'XCAPCLIFOR'
121000141222     C                   PARM                    XSCSOC
121100141222     C                   PARM      'F     '      WKcc
121200141222     C                   PARM                    D1Ksc
121300141222     C*
121400141222     C                   CALLB     'XALLINEA'
121500141222     C                   PARM                    D1KSC
121600141222     C                   PARM      8             LEN               5 0
121700141222     C                   PARM                    XSCINP
121800141222     C*
121900141222     C                   Eval      DVALenOut   = %Size(DVAFOR)
122000141222     C*
122100141222     C                   CALLB     'NDDVASOG'
122200141222     C                   PARM      '1'           DVARIC
122300141222     C                   PARM      XScSoc        DVASOCIETA
122400141222     C                   PARM                    DVAUNITA
122500141222     C                   PARM      'DVAFOR    '  DVASTRUTT
122600141222     C                   PARM                    DVADTRIF
122700141222     C                   PARM                    DVALENOUT
122800141222     C                   PARM      'F'           DVASNATURA
122900141222     C                   PARM      D1Ksc         DVACODICE
123000141222     C                   PARM                    DVALINEAV
123100141222     C                   PARM                    DVAFILIALE
123200141222     C                   PARM                    *Omit
123300141222     C                   PARM                    DVATPIND
123400141222     C                   PARM                    DVACDIND
123500141222     C                   PARM                    DVAERRORE
123600141222     C                   PARM                    DVAOUTPUT
123700141222     C*
123800141222     C                   if        DVAErrore   = *Off
123900141222     C*
124000141222     C                   Eval      %SUBST(DVAFOR:1:DVALENOUT)
124100141222     C                                 = %SUBST(DVAOUTPUT:1:DVALENOUT)
124200141222     C*
124300141222     C                   else
124400141222     C*
124500141222     C                   Clear                   DVAFOR
124600141223     C                   SetOn                                        5499
124700141222     C*
124800141222     C                   endif
124900141222     C*
125000141222     C                   MoveL     DVFDesBrev    D1KScD
125100141222     C*
125200141222     C                   endif
125300141222     C*
125400141222     C                   ENDSR
125500141222     C/EJECT
125600141223     C***********************************************************
125700141223     C* Controllo tipo natura giuridica
125800141223     C***********************************************************
125900141223     C     CtrTPN        BEGSR
126000141223     C*
126100141223     C                   Clear                   X07ValDs
126200141223     C                   Move      '1'           X07Ric
126300141223     C                   Move      *Off          X07All
126400141223     C                   MoveL     'SDGDIE000'   X07TRc
126500141223     C                   MoveL     'DIETPN  '    X07Cam
126600141223     C                   MoveL     D1TpN         X07VAL
126700141223     C                   CALLB     'X07VALR'
126800141223     C                   PARM                    X07ValDs
126900141223     C*
127000141223     C     X07ERR        IFEQ      *Off
127100141223     C                   MoveL     X07DES        D1TpND
127200141223     C                   else
127300141223     C                   SetOn                                        5599
127400141223     C                   endif
127500141223     C*
127600141223     C                   ENDSR
127700141223     C/EJECT
127800141222     C***********************************************************
127900141222     C* Controllo partita IVA / codice fiscale
128000141222     C***********************************************************
128100141222     C     CtrPIVA_CDF   BEGSR
128200141222     C*
128300141222     C* se non è dogana, sono obbligatori la partita IVA / codice fisc.
128400141229     C                   if        D1Dogana   <> XScOn
128500141229     C*
128600141229     C                   if        D1PartIVA   = *Blank    and
128700130919     C                             D1CdFisc    = *Blank
128800141223     C                   Seton                                        5699
128900130919     C                   endif
129000141229     C*
129100141229     C* se persona fisica, obbligatorio codice fiscale
129200141229     C                   if        D1TpN       = '1'       and
129300141229     C                             D1CdFisc    = *Blank
129400141229     C                   Seton                                        7099
129500141229     C                   endif
129600141229     C*
129700141229     C                   if        D1CdFisc   <> *Blank    and
129800141229     C                             D1PartIVA   = *Blank
129900141229     C                   Eval      D1PartIVA   = D1CdFisc
130000141229     C                   endif
130100130919     C*
130200141229     C                   endif
130300141229     C*
130400130919     C                   ENDSR
130500130919     C/EJECT
130600130919     C***********************************************************
130700130919     C* Controllo cognome/ragione sociale
130800130919     C***********************************************************
130900130919     C     CtrCogRag     BEGSR
131000141223     C*
131100150122     C                   if        D1CogRag  =  *Blank
131200141223     C                   if        D1Sogg    <> *Blank
131300141223     C                   Eval      D1CogRag   = DVSDes
131400141223     C                   endif
131500141223     C*
131600141223     C                   if        D1Ksc     <> *Blank
131700141223     C                   Eval      D1CogRag   = DVFDesBrev
131800141223     C                   endif
131900150122     C                   endif
132000130919     C*
132100130919     C                   if        D1CogRag    = *Blank
132200141223     C                   SetOn                                        5799
132300130919     C                   endif
132400130919     C*
132500130919     C     ''''          Scan      D1CogRag                               22
132600130919     C     *IN22         IFEQ      *On
132700130919     C                   SetOn                                        5899
132800130919     C                   endif
132900130919     C*
133000130919     C                   ENDSR
133100130919     C/EJECT
133200130919     C***********************************************************
133300130919     C* Controllo nome
133400130919     C***********************************************************
133500130919     C     CtrNome       BEGSR
133600141223     C*
133700141223     C                   if        D1Nome      = *Blank
133800141223     C                   SetOn                                        6999
133900141223     C                   endif
134000130919     C*
134100130919     C     ''''          Scan      D1Nome                                 22
134200130919     C     *IN22         IFEQ      *On
134300130919     C                   SetOn                                        5999
134400130919     C                   endif
134500130919     C*
134600130919     C                   ENDSR
134700130919     C/EJECT
134800130919     C************************************************************
134900141222     C* Controllo sesso
135000130919     C************************************************************
135100141222     C     CtrSex        BEGSR
135200141222     C*
135300141222     C                   Clear                   X07VALDS
135400141222     C                   Move      '1'           X07Ric
135500141222     C                   Move      *Off          X07All
135600141222     C                   Movel     'SDGDIE000'   X07TRc
135700141222     C                   Movel     'DIESEX  '    X07Cam
135800141222     C                   Movel     D1Sex         X07VAL
135900141222     C                   Callb     'X07VALR'
136000141222     C                   Parm                    X07VALDS
136100141222     C*
136200141222     C     X07ERR        IFEQ      *Off
136300141222     C                   MOVEL     X07DES        D1SexD
136400141222     C                   Else
136500141223     C                   SetOn                                        6099
136600141222     C                   ENDIF
136700141222     C*
136800141222     C                   ENDSR
136900141222     C/EJECT
137000141222     C***********************************************************
137100141222     C* Controllo tipo operazione
137200141222     C***********************************************************
137300141222     C     CtrTpOpe      BEGSR
137400141222     C*
137500141222     C                   Clear                   D1TpOpeD
137600141222     C*
137700141222     C                   Clear                   X07VALDS
137800141222     C                   Move      '1'           X07Ric
137900141222     C                   Move      *Off          X07All
138000141222     C                   Movel     'SDGDIE000'   X07TRc
138100141222     C                   Movel     'DIETPOPE'    X07Cam
138200141222     C                   Movel     D1TpOpe       X07VAL
138300141222     C                   Callb     'X07VALR'
138400141222     C                   Parm                    X07VALDS
138500141222     C*
138600141222     C     X07ERR        IFEQ      *Off
138700141222     C                   MOVEL     X07DES        D1TpOpeD
138800141222     C                   Else
138900141223     C                   SetOn                                        6199
139000141222     C                   ENDIF
139100141222     C*
139200141222     C                   Select
139300141222     C                   When      D1TpOpe     = '1'
139400141223     C********           Clear                   D1Anno
139500141222     C                   Clear                   D1DtInDic
139600141222     C                   Clear                   D1DtFiDic
139700141222     C*
139800141222     C                   When      D1TpOpe     = '2'
139900141222     C                   Clear                   D1DtInDic
140000141222     C                   Clear                   D1DtFiDic
140100141222     C*
140200141222     C                   When      D1TpOpe     = '3'
140300141223     C********           Clear                   D1Anno
140400141222     C                   Clear                   D1Importo
140500141222     C*
140600141222     C                   endSl
140700141222     C*
140800141222     C                   ENDSR
140900141222     C/EJECT
141000141222     C***********************************************************
141100141222     C* Controllo importo
141200141222     C***********************************************************
141300141222     C     CtrImporto    BEGSR
141400141222     C*
141500141222     C*
141600141222     C                   Select
141700141222     C                   When      D1TpOpe     = '1'  or
141800141222     C                             D1TpOpe     = '2'
141900141222     C                   if        D1Importo   = 0
142000141223     C                   SetOn                                        6299
142100141222     C                   endif
142200141222     C*
142300141222     C                   When      D1TpOpe     = '3'
142400141222     C                   Z-ADD     0             D1Importo
142500141222     C*
142600141222     C                   endSl
142700141222     C*
142800141222     C                   ENDSR
142900141222     C/EJECT
143000141222     C***********************************************************
143100141222     C* Controllo date del periodo
143200141222     C***********************************************************
143300141222     C     CtrDtPer      BEGSR
143400141222     C*
143500141222     C                   Clear                   WDtInDic
143600141222     C                   Clear                   WDtFiDic
143700141222     C*
143800141222     C                   if        D1TpOpe     = '3'
143900141222     C*
144000141222     C                   if        D1DtInDic   = 0
144100141223     C                   SETON                                        6399
144200141222     C                   else
144300141222     C*
144400141222     C                   if        *In21       = *Off
144500141222     C     *Jobrun       move      D1DtInDic     WDtInDic
144600141222     C                   else
144700141222     C* PRO0001-Data errata
144800141223     C                   SETON                                        6499
144900141222     C                   endif
145000141222     C*
145100141222     C                   endif
145200141222     C*
145300141222     C*
145400141222     C                   if        D1DtFiDic   = 0
145500141223     C                   SETON                                        6599
145600141222     C                   else
145700141222     C     *Jobrun       Test(d)                 D1DtFiDic              21
145800141222     C*
145900141222     C                   if        *In21       = *Off
146000141222     C     *Jobrun       move      D1DtFiDic     WDtFiDic
146100141222     C                   else
146200141222     C* PRO0001-Data errata
146300141223     C                   SETON                                        6699
146400141222     C                   endif
146500141222     C*
146600141222     C                   endif
146700141222     C*
146800141222     C*
146900141222     C* controllo congruenza
147000141222     C                   if        WDtInDic    > WDtFiDic  and
147100141222     C                             WDtFiDic    > *Loval
147200141223     C                   SETON                                        6799
147300141222     C                   endif
147400141222     C*
147500141222     C                   endif
147600141222     C*
147700141222     C                   ENDSR
147800141222     C/EJECT
147900141223     C***********************************************************
148000141223     C* Controllo descrizione merce
148100141223     C***********************************************************
148200141223     C     CtrDescr      BEGSR
148300141223     C*
148400141223     C     ''''          Scan      D1Descr                                22
148500141223     C     *IN22         IFEQ      *On
148600141223     C                   SetOn                                        6899
148700141223     C                   endif
148800141223     C*
148900141223     C                   ENDSR
149000141223     C/EJECT
149100000000     C************************************************************
149200000000     C* GESTIONE VIDEO RECORD  D2
149300000000     C************************************************************
149400000000    >C     GESD2         BEGSR
149500000000     C* emissione piede videata se proveniente da altra
149600000000     C* salvataggio valore formato di provenienza
149700000000   B1C     $LASTV        IFNE      'D2'
149800000000     C                   WRITE     FMTT1
149900000000     C                   EXSR      GESPIE
150000000000     C                   MOVE      $LASTV        $LASTG
150100000000     C                   MOVE      'D2'          $LASTV
150200000000   E1C                   ENDIF
150300000000     C*
150400000000     C* se esistono errori sulla videata
150500000000     C* emetto la write del formato a indicatori spenti per vedere
150600000000     C* le eventuali decodifiche
150700131001   B1C                   if        (*IN99       = *On   or
150800131001   B1C                              *In17       = *On      )  and
150900130926     C                             $GEST        = 'D2'
151000000000     C                   MOVEA     *IN           WIN
151100000000     C                   MOVE      *ALL'0'       IN5098           49
151200000000     C                   MOVEA     IN5098        *IN(50)
151300131001     C                   Eval      WIn17        = *In17
151400131001     C                   Eval      *In17        = *Off
151500000000     C                   WRITE     FMTD2
151600000000     C                   MOVEA     WIN           *IN
151700131001     C                   Eval      *In17        = WIn17
151800000000   E1C                   ENDIF
151900000000     C* Inizializzazione indice tasti funzione personalizzati
152000000000     C                   Eval      PosFun = 25
152100000000     C*
152200000000   B1C                   SELECT
152300000000     C* Annullamento
152400141222    >C     OPZJ74        WHENEQ    '04'
152500000000     C* Visualizzazione
152600141222    >C     OPZJ74        OREQ      '05'
152700000000     C                   WRITE     FMTD2
152800000000     C                   EXFMT     PROTECT
152900000000     C* Immissione/Modifica/Copia
153000000000   X1C                   OTHER
153100000000     C                   EXFMT     FMTD2
153200000000   E1C                   ENDSL
153300000000     C*
153400000000     C* azzero la posizione del cursore
153500000000     C                   Z-ADD     0             H1RIGA
153600000000     C                   Z-ADD     0             H1COLO
153700000000     C*
153800000000     C* controllo abilitazione tasti funzione
153900000000     C                   EXSR      CTRKD1
154000000000     C*
154100000000   B1C     *IN99         IFEQ      *OFF
154200000000     C*
154300000000   B2C                   SELECT
154400000000     C* ROLDOWN
154500000000     C     $TASTO        WHENEQ    ROLDWN
154600000000     C                   MOVE      'D1'          $GEST
154700000000     C* F3=Fine
154800000000     C     $TASTO        WHENEQ    F03
154900000000     C                   EXSR      F03D1
155000000000     C* F4=Ricerca
155100000000     C     $TASTO        WHENEQ    F04
155200000000     C                   EXSR      SEARCH
155300000000     C* F5=Refresh
155400000000     C     $TASTO        WHENEQ    F05
155500000000     C                   EXSR      F05D2
155600000000     C* F8=Successivo
155700000000     C     $TASTO        WHENEQ    F08
155800000000     C                   EXSR      F08D1
155900000000     C* F12=Ritorno
156000000000     C     $TASTO        WHENEQ    F12
156100000000     C                   EXSR      F12D1
156200000000     C* F24=Altri tasti
156300000000     C     $TASTO        WHENEQ    F24
156400000000     C                   EXSR      F24D2
156500000000     C* Personalizzata
156600141222     C* NB chiamata NON possibile perchè SDGSJ74DS più lunga
156700000000     C* di KPJBU 256 caratteri                          a
156800000000     C**** $PGF1(PosFun) Whenne    *BLANK
156900000000     C****               EXSR      FperD1
157000000000     C*
157100000000   X2C                   OTHER
157200000000     C                   EXSR      CTRD2
157300000000   B3C     *IN99         IFEQ      *OFF
157400000000     C                   EXSR      CTRD1
157500130926   B1C                   if        *IN99       = *On
157600000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
157700000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
157800000000     C* videate
157900000000     C                   MOVE      *ALL'0'       IN5098           49
158000000000     C                   MOVEA     IN5098        *IN(50)
158100000000     C                   SETON                                        9996                       96
158200000000     C                   ENDIF
158300000000     C                   ENDIF
158400000000     C*
158500000000     C*
158600000000   B3C     $TASTO        IFEQ      F06
158700000000     C     *IN99         ANDEQ     *OFF
158800000000     C* eseguo aggiornamento
158900000000     C                   EXSR      AGGANA
159000000000     C* eseguo operazioni del dopo-aggiornamento
159100000000   B4C     *IN99         IFEQ      *OFF
159200000000     C                   EXSR      GESAGG
159300000000   E4C                   ENDIF
159400000000   E3C                   ENDIF
159500000000     C*
159600000000   E2C                   ENDSL
159700000000     C*
159800000000   E1C                   ENDIF
159900000000     C*
160000000000    >C                   ENDSR
160100000000     C/EJECT
160200000000     C************************************************************
160300000000     C* RIEMPIMENTO VIDEATA D2
160400000000     C************************************************************
160500000000    >C     RIED2         BEGSR
160600000000     C*
160700000000     C*
160800000000    >C                   ENDSR
160900000000     C/EJECT
161000000000     C************************************************************
161100000000     C* GESTIONE F05 VIDEO D2
161200000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
161300000000     C* e il riposizionamento alla prima
161400000000     C************************************************************
161500000000    >C     F05D2         BEGSR
161600000000     C*
161700000000     C                   MOVE      *ON           $INZD1
161800000000     C                   MOVE      'D1'          $GEST
161900000000     C*
162000000000    >C                   ENDSR
162100000000     C************************************************************
162200000000     C* GESTIONE F24 VIDEO D2
162300000000     C************************************************************
162400000000    >C     F24D2         BEGSR
162500000000     C*
162600000000     C                   MOVEA     $FX1          $FX
162700000000     C                   MOVEA     $FC1          $FC
162800000000     C                   MOVEA     $FR1          $FR
162900000000     C                   Z-ADD     $ULKD1        $ULK
163000000000     C                   Z-ADD     $D1L01        $L01
163100000000     C                   Z-ADD     $D1L02        $L02
163200000000     C                   EXSR      RIEKEY
163300000000     C                   Z-ADD     $ULK          $ULKD1            3 0
163400000000     C                   MOVEA     $FC           $FC1
163500000000     C                   MOVEL     $KEY1         Z1KE1
163600000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
163700000000     C                   MOVEL     $KEY2         Z1KE2
163800000000     C                   WRITE     FMTZ1
163900000000     C*
164000000000    >C                   ENDSR
164100000000     C************************************************************
164200000000     C* CONTROLLO VIDEATA  D2
164300000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
164400000000     C* 95 96 97 98
164500000000     C************************************************************
164600000000    >C     CTRD2         BEGSR
164700000000     C*
164800000000     C                   SETOFF                                       99
164900000000     C*
165000000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
165100000000     C*
165200141222    >C     OPZJ74        IFNE      '04'
165300000000     C*
165400141223     C                   EXSR      CtrOrigine
165500141223     C*
165600141223     C                   EXSR      CtrAnn
165700141223     C                   EXSR      CtrTpAnn
165800000000     C*
165900000000     C                   ENDIF
166000000000     C*
166100000000     C                   ENDSR
166200000000     C/EJECT
166300141223     C************************************************************
166400141223     C* Controllo origine record
166500141223     C************************************************************
166600141223     C     CtrOrigine    BEGSR
166700141223     C*
166800141223     C                   Clear                   D1OrigineD
166900141223     C                   Clear                   X07ValDs
167000141223     C*
167100141223     C                   Move      '1'           X07Ric
167200141223     C                   Move      *On           X07All
167300141223     C                   MoveL     'SDGDIE000'   X07TRc
167400141223     C                   MoveL     'DIEORIGINE'  X07Cam
167500141223     C                   MoveL     D1Origine     X07VAL
167600141223     C*
167700141223     C                   CALLB     'X07VALR'
167800141223     C                   PARM                    X07ValDs
167900141223     C*
168000141223     C     X07ERR        IFEQ      *Off
168100141223     C                   MoveL     X07DES        D1OrigineD
168200141223     C                   else
168300141223     C                   SetOn                                        7599
168400141223     C                   endif
168500141223     C*
168600141223     C                   ENDSR
168700141223     C/EJECT
168800141223     C************************************************************
168900141223     C* Controllo annullato
169000141223     C************************************************************
169100141223     C     CtrAnn        BEGSR
169200141223     C*
169300141223     C                   if        D1Ann      <> XScOn    and
169400141223     C                             D1Ann      <> XScOff
169500141223     C                   SetOn                                        6399
169600141223     C                   endif
169700141223     C*
169800141223     C                   ENDSR
169900141223     C/EJECT
170000141223     C***********************************************************
170100141223     C* Controllo tipo annullamento
170200141223     C***********************************************************
170300141223     C     CtrTpAnn      BEGSR
170400141223     C*
170500141229     C                   if        D1Ann     <>  XScOn
170600141229     C                   Clear                   D1TpAnn
170700141229     C                   endif
170800141229     C*
170900150113     C                   Clear                   D1TpAnnD
171000141223     C                   Clear                   X07ValDs
171100141223     C                   Move      '1'           X07Ric
171200141229     C                   Move      *On           X07All
171300141223     C                   MoveL     'SDGDIE000'   X07TRc
171400141223     C                   MoveL     'DIETPANN'    X07Cam
171500141223     C                   MoveL     D1TpAnn       X07VAL
171600141223     C                   CALLB     'X07VALR'
171700141223     C                   PARM                    X07ValDs
171800141223     C*
171900141223     C     X07ERR        IFEQ      *Off
172000141223     C                   MoveL     X07DES        D1TpAnnD
172100141223     C                   else
172200141223     C                   SetOn                                        6499
172300141223     C                   endif
172400150115     C*
172500150115     C                   if        D1Ann     =   XScOn  and
172600150115     C                             D1TpAnn   =   *blank
172700150115     C                   SetOn                                        6499
172800150115     C                   endif
172900141223     C*
173000141223     C                   ENDSR
173100141223     C/EJECT
173200000000     C************************************************************
173300000000     C* AGGIORNAMENTO ANAGRAFICA
173400000000     C************************************************************
173500000000     C     AGGANA        BEGSR
173600000000     C*
173700000000     C                   SELECT
173800141222    >C     OPZJ74        WHENEQ    '01'
173900141222    >C     OPZJ74        OREQ      '03'
174000000000     C                   EXSR      RIEANA
174100141222    >C                   WRITE     SDGDIE000                            22
174200000000     C* se rcd non scrivibile attivo errore
174300000000     C     *IN22         IFEQ      *ON
174400000000     C                   SETON                                        9599
174500000000     C                   ENDIF
174600141222    >C     OPZJ74        WHENEQ    '02'
174700141222     C                   EXSR      RIEANA
174800141222     C                   UPDATE    SDGDIE000                            22
174900141222    >C     OPZJ74        WHENEQ    '04'
175000141222     C                   DELETE    SDGDIE000
175100000000     C                   ENDSL
175200000000     C*
175300000000     C* richiama pgm User Exit
175400000000     C     USRERR        IFEQ      *BLANK
175500000000     C* passa opzione ricevuta a user exit
175600141222    >C                   MOVEL     OPZJ74        XUKOPZ
175700000000???? C* imposta chiave univoca  per user exit
175800000000???? C* la chiave univoca non esiste perchè sto lavorando
175900000000???? C* con raggruppamento di dati
176000000000     C                   MOVE      XSCSOC        XUKSOC
176100000000     C                   MOVE      XSCDUN        XUKDUN
176200000000     C                   MOVEL     DSPGM         XUKPGM
176300000000    >C                   MOVEL     *Blank        XUKFL1
176400000000    >C                   MOVEL     *Blank        XUKFL2
176500000000    >C* ho fino a 6 campi chiave passabili
176600000000     C                   MOVEL     XUKDS         KPJBU
176700000000     C     DSPGM         CAT       'U':0         USREXT           10
176800000000     C                   CALL      USREXT                               21
176900000000     C                   PARM                    KPJBA
177000000000     C     *IN21         IFEQ      *ON
177100000000     C                   MOVE      *ON           USRERR            1
177200000000     C                   ENDIF
177300000000     C                   ENDIF
177400000000     C*
177500000000     C                   ENDSR
177600000000     C/EJECT
177700000000     C************************************************************
177800000000     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
177900000000     C************************************************************
178000000000     C     RIEANA        BEGSR
178100000000     C*
178200141222     C                   if        OPZJ74      = '01'   or
178300141222     C                             OPZJ74      = '03'
178400000000     C*
178500141222     C                   CLEAR                   SDGDIE000
178600000000     C*
178700141222     C                   Eval      DIEORIGINE  = 'M'
178800000000     C*
178900000000     C* imposto la data e l'utente di immissione
179000141222     C                   Eval      DIEDtImm    = UDateIso
179100141222     C                   Eval      DIEUteImm   = XScCUt
179200000000     C*
179300141222     C                   Eval      DIEAnno     = AnnoJ74
179400110824     C*
179500141222     C                   endif
179600141222     C*
179700141222     C                   Eval      DIEAnno     = D1Anno
179800141222     C                   if        D1DtInter  <> 0
179900141222     C     *JobRun       Move      D1DtInter     DIEDtInter
180000141222     C                   endif
180100141222     C                   Eval      DIENrInter  = D1NrInter
180200141222     C                   if        D1Dogana    = XScOn
180300141222     C                   Eval      DIEDogana   = '1'
180400141222     C                   else
180500141222     C                   Eval      DIEDogana   = '0'
180600141222     C                   endif
180700141222     C                   Eval      DIESogg     = D1Sogg
180800141222     C                   Eval      DIEKsc      = D1Ksc
180900141223     C                   Eval      DIETpN      = D1TpN
181000141223     C                   Eval      DIEPartIVA  = D1PartIVA
181100141222     C                   Eval      DIECdFisc   = D1CdFisc
181200141222     C                   Eval      DIECogRag   = D1CogRag
181300141222     C                   Eval      DIENome     = D1Nome
181400141222     C                   Eval      DIESex      = D1Sex
181500141222     C                   Eval      DIEDescr    = D1Descr
181600141222     C                   Eval      DIETpOpe    = D1TpOpe
181700141222     C                   Eval      DIEImporto  = D1Importo
181800141222     C                   if        D1DtInDic  <> 0
181900141222     C     *JobRun       Move      D1DtInDic     DIEDtInDic
182000141222     C                   endif
182100141222     C                   if        D1DtFiDic  <> 0
182200141222     C     *JobRun       Move      D1DtFiDic     DIEDtFiDic
182300141222     C                   endif
182400141222     C                   if        D1Ann       = XScOn
182500141222     C                   Eval      DIEAnn      = '1'
182600141222     C                   else
182700141222     C                   Eval      DIEAnn      = '0'
182800141222     C                   endif
182900141222     C                   Eval      DIETpAnn    = D1TpAnn
183000141222     C                   Eval      DIEOrigine  = D1Origine
183100000000     C*
183200000000     C* imposto la data e l'utente di ultima modifica
183300141222     C                   Eval      DIEDtUlMo   = UDateIso
183400141222     C                   Eval      DIEUteUlMo  = XScCUt
183500000000     C*
183600000000     C* imposto i campi del file da quelli video
183700141222     C                   MOVEL     XscSoc        DIESocieta
183800141222     C                   Eval      DIEPartIva  = D1PartIva
183900141222     C                   Eval      DIECdFisc   = D1CdFisc
184000141222     C                   Eval      DIECogRag   = D1CogRag
184100141222     C                   Eval      DIENome     = D1Nome
184200141222     C                   Eval      DIESex      = D1Sex
184300000000     C*
184400000000     C                   ENDSR
184500000000     C/EJECT
184600000000     C************************************************************
184700000000     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
184800000000     C************************************************************
184900000000     C     GESAGG        BEGSR
185000000000     C*
185100000000     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
185200141222    >C     CMTJ74        IFEQ      '0'
185300000000     C**************     COMMIT
185400000000     C                   ENDIF
185500000000     C*
185600000000     C* segnalo al pgm chiamante l'avvenuta conferma
185700141222    >C                   MOVE      *ON           OPRJ74
185800000000     C* segnalo al pgm chiamante l'aver premuto l'F6
185900141222    >C                   MOVE      '0'           RETJ74
186000000000     C*
186100000000     C                   SELECT
186200000000     C*
186300000000     C* nel caso di immissione
186400141222    >C     OPZJ74        WHENEQ    '01'
186500141222    >C     OPZJ74        orEQ      '03'
186600000000     C* ripristino la videata iniziale
186700000000     C                   MOVE      *ON           $INZD1
186800000000     C                   MOVE      'D1'          $GEST
186900000000     C*
187000000000     C* forzo il ritorno al pgm chiamante perchè devo chiamare
187100000000     C* i pgm di manutenzione del dettaglio
187200000000     C                   MOVE      *ON           $FINE
187300000000     C*
187400141222    >C                   MOVE      xscsoc        SocietaJ74
187500141222     C* passare nei campi appositi della DS SDGSJ74DS il valore
187600000000     C* della chiave della VL univoca
187700141222     C                   Eval      DtInterJ74  = WDtInter
187800141222     C                   Eval      NrInterJ74  = D1NrInter
187900000000     C*
188000000000     C* nel caso di modifica
188100141222    >C     OPZJ74        WHENEQ    '02'
188200000000     C* ripasso i campi che devono essere reimpostati sul sfl
188300141222     C                   Eval      PartIvaJ74  = D1PartIva
188400141222     C                   Eval      CdFiscJ74   = D1CdFisc
188500150112     C                   Eval      KscJ74      = D1Ksc
188600141222     C                   Eval      CogRagJ74   = D1CogRag
188700150113     C                   Eval      OrigineJ74  = D1Origine
188800000000     C* e ritorno al pgm chiamante
188900000000     C                   MOVE      *ON           $FINE
189000000000     C*
189100000000     C* per tutte le altre opzioni
189200000000     C                   OTHER
189300000000     C* ritorno al pgm chiamante
189400000000     C                   MOVE      *ON           $FINE
189500000000     C                   ENDSL
189600000000     C*
189700000000     C                   ENDSR
189800000000     C/EJECT
189900000000     C************************************************************
190000000000     C* CONTROLLO ABILITAZIONE TASTI
190100000000     C************************************************************
190200000000     C     CTRKEY        BEGSR
190300000000     C*
190400000000     C                   SELECT
190500000000     C     $TASTO        WHENEQ    F01
190600000000     C                   Z-ADD     01            PosFun
190700000000     C     $FC(1)        IFEQ      '0'
190800000000     C                   SETON                                        99
190900000000     C                   ENDIF
191000000000     C     $TASTO        WHENEQ    F02
191100000000     C                   Z-ADD     02            PosFun
191200000000     C     $FC(2)        IFEQ      '0'
191300000000     C                   SETON                                        99
191400000000     C                   ENDIF
191500000000     C     $TASTO        WHENEQ    F03
191600000000     C                   Z-ADD     03            PosFun
191700000000     C     $FC(3)        IFEQ      '0'
191800000000     C                   SETON                                        99
191900000000     C                   ENDIF
192000000000     C     $TASTO        WHENEQ    F04
192100000000     C                   Z-ADD     04            PosFun
192200000000     C     $FC(4)        IFEQ      '0'
192300000000     C                   SETON                                        99
192400000000     C                   ENDIF
192500000000     C     $TASTO        WHENEQ    F05
192600000000     C                   Z-ADD     05            PosFun
192700000000     C     $FC(5)        IFEQ      '0'
192800000000     C                   SETON                                        99
192900000000     C                   ENDIF
193000000000     C     $TASTO        WHENEQ    F06
193100000000     C                   Z-ADD     06            PosFun
193200000000     C     $FC(6)        IFEQ      '0'
193300000000     C                   SETON                                        99
193400000000     C                   ENDIF
193500000000     C     $TASTO        WHENEQ    F07
193600000000     C                   Z-ADD     07            PosFun
193700000000     C     $FC(7)        IFEQ      '0'
193800000000     C                   SETON                                        99
193900000000     C                   ENDIF
194000000000     C     $TASTO        WHENEQ    F08
194100000000     C                   Z-ADD     08            PosFun
194200000000     C     $FC(8)        IFEQ      '0'
194300000000     C                   SETON                                        99
194400000000     C                   ENDIF
194500000000     C     $TASTO        WHENEQ    F09
194600000000     C                   Z-ADD     09            PosFun
194700000000     C     $FC(09)       IFEQ      '0'
194800000000     C                   SETON                                        99
194900000000     C                   ENDIF
195000000000     C     $TASTO        WHENEQ    F10
195100000000     C                   Z-ADD     10            PosFun
195200000000     C     $FC(10)       IFEQ      '0'
195300000000     C                   SETON                                        99
195400000000     C                   ENDIF
195500000000     C     $TASTO        WHENEQ    F11
195600000000     C                   Z-ADD     11            PosFun
195700000000     C     $FC(11)       IFEQ      '0'
195800000000     C                   SETON                                        99
195900000000     C                   ENDIF
196000000000     C     $TASTO        WHENEQ    F12
196100000000     C                   Z-ADD     12            PosFun
196200000000     C     $FC(12)       IFEQ      '0'
196300000000     C                   SETON                                        99
196400000000     C                   ENDIF
196500000000     C     $TASTO        WHENEQ    F13
196600000000     C                   Z-ADD     13            PosFun
196700000000     C     $FC(13)       IFEQ      '0'
196800000000     C                   SETON                                        99
196900000000     C                   ENDIF
197000000000     C     $TASTO        WHENEQ    F14
197100000000     C                   Z-ADD     14            PosFun
197200000000     C     $FC(14)       IFEQ      '0'
197300000000     C                   SETON                                        99
197400000000     C                   ENDIF
197500000000     C     $TASTO        WHENEQ    F15
197600000000     C                   Z-ADD     15            PosFun
197700000000     C     $FC(15)       IFEQ      '0'
197800000000     C                   SETON                                        99
197900000000     C                   ENDIF
198000000000     C     $TASTO        WHENEQ    F16
198100000000     C                   Z-ADD     16            PosFun
198200000000     C     $FC(16)       IFEQ      '0'
198300000000     C                   SETON                                        99
198400000000     C                   ENDIF
198500000000     C     $TASTO        WHENEQ    F17
198600000000     C                   Z-ADD     17            PosFun
198700000000     C     $FC(17)       IFEQ      '0'
198800000000     C                   SETON                                        99
198900000000     C                   ENDIF
199000000000     C     $TASTO        WHENEQ    F18
199100000000     C                   Z-ADD     18            PosFun
199200000000     C     $FC(18)       IFEQ      '0'
199300000000     C                   SETON                                        99
199400000000     C                   ENDIF
199500000000     C     $TASTO        WHENEQ    F19
199600000000     C                   Z-ADD     19            PosFun
199700000000     C     $FC(19)       IFEQ      '0'
199800000000     C                   SETON                                        99
199900000000     C                   ENDIF
200000000000     C     $TASTO        WHENEQ    F20
200100000000     C                   Z-ADD     20            PosFun
200200000000     C     $FC(20)       IFEQ      '0'
200300000000     C                   SETON                                        99
200400000000     C                   ENDIF
200500000000     C     $TASTO        WHENEQ    F21
200600000000     C                   Z-ADD     21            PosFun
200700000000     C     $FC(21)       IFEQ      '0'
200800000000     C                   SETON                                        99
200900000000     C                   ENDIF
201000000000     C     $TASTO        WHENEQ    F22
201100000000     C                   Z-ADD     22            PosFun
201200000000     C     $FC(22)       IFEQ      '0'
201300000000     C                   SETON                                        99
201400000000     C                   ENDIF
201500000000     C     $TASTO        WHENEQ    F23
201600000000     C                   Z-ADD     23            PosFun
201700000000     C     $FC(23)       IFEQ      '0'
201800000000     C                   SETON                                        99
201900000000     C                   ENDIF
202000000000     C     $TASTO        WHENEQ    F24
202100000000     C                   Z-ADD     24            PosFun
202200000000     C     $FC(24)       IFEQ      '0'
202300000000     C                   SETON                                        99
202400000000     C                   ENDIF
202500000000     C                   ENDSL
202600000000     C*
202700000000     C                   ENDSR
202800000000     C/EJECT
202900000000     C******************************************************
203000000000     C* PGM PER GESTIONE MESSAGGI ERRORE
203100000000     C******************************************************
203200000000     C     MSGPGM        BEGSR
203300000000     C*
203400000000     C                   Z-ADD     2             MSGMSG
203500000000     C                   MOVEL     'S'           MSGRSP
203600000000     C                   MOVEL     'CR'          MSGTPR
203700000000     C                   MOVEL     'S'           MSGEMV
203800000000     C                   Z-ADD     10            MSGRGP
203900000000     C                   MOVEL     'S'           MSGLCK
204000000000     C                   MOVEA     STATUS        STS
204100000000     C                   MOVEL     'N'           MSGCNL
204200000000     C                   MOVEL     'N'           MSGVID
204300000000     C                   MOVEL     'N'           MSGOPE
204400000000     C                   MOVEL     'N'           MSGSTP
204500000000     C                   CALL      'SDGMSGR'
204600000000     C                   PARM                    SDGMSGDS
204700000000     C*
204800000000     C                   ENDSR
204900000000     C/EJECT
205000000000     C************************************************************
205100000000     C* OPERAZIONI INIZIALI
205200000000     C************************************************************
205300000000     C     *INZSR        BEGSR
205400000000     C*
205500000000     C* Reperimento parametri
205600000000     C*
205700000000     C     *ENTRY        PLIST
205800000000     C                   PARM                    KPJBA
205900141222     C                   PARM                    SDGSJ74DS
206000000000     C*
206100000000     C* Reperimento dati società
206200000000     C*
206300000000     C                   MOVEL     'SOC001'      TIPXSC
206400000000     C                   MOVEL     *BLANK        SOCXSC
206500000000     C                   EXSR      REPSOC
206600000000     C*
206700000000     C* Controlli dati società
206800000000     C*
206900000000     C     RTNXSC        IFNE      '1'
207000000000     C                   MOVEL     XSOCDS        SOC001
207100000000     C                   ELSE
207200141222    >C                   MOVE      '1'           ErrJ74
207300000000     C                   MOVE      *on           $Fine
207400000000     C                   EXSR      EndPgm
207500000000     C                   ENDIF
207600000000     C*
207700000000     C                   MOVE      *BLANK        AUTDS
207800000000     C*
207900000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
208000000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
208100000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
208200000000     C*
208300000000     C* Reperimento tasti di funzione
208400000000     C*
208500000000     C                   EXSR      REPKEY
208600000000     C*
208700000000     C* Richiamo routine per aggiunta tasti funzione personalizzati
208800000000     C                   CAllb     'X46USREXT'
208900000000     C                   PARM                    *omit
209000000000     C                   PARM                    *omit
209100000000     C                   PARM                    *omit
209200000000     C                   PARM                    *omit
209300000000     C                   PARM                    *omit
209400000000     C                   PARM                    $FX1
209500000000     C                   PARM                    $FC1
209600000000     C                   PARM                    $PGF1
209700000000     C                   PARM                    DsPgm
209800000000     C                   PARM                    xsccut
209900000000     C                   PARM                    xscgrp
210000000000     C*
210100000000     C     *DMY          Move      UDate         UDateISO
210200000000     C*
210300000000     C* reperisco DATFMT del job
210400000000     C                   CallB     'XDATFMT'
210500000000     C                   Parm                    $DatFmt           4
210600000000     C*
210700000000     C                   ENDSR
210800000000     C/EJECT
210900000000     C************************************************************
211000000000     C* REPERIMENTO TASTI DI FUNZIONE
211100000000     C************************************************************
211200000000     C     REPKEY        BEGSR
211300000000     C*
211400000000     C                   MOVEA     $FM1          $FM
211500000000     C                   MOVEA     $FC1          $FC
211600000000     C                   EXSR      REPKKK
211700000000     C                   MOVEA     $FX           $FX1
211800000000     C*
211900000000     C                   ENDSR
212000000000     C/EJECT
212100000000     C************************************************************
212200000000     C* REPERIMENTO TASTI DI FUNZIONE
212300000000     C************************************************************
212400000000     C     REPKKK        BEGSR
212500000000     C*
212600000000     C                   CLEAR                   $FX
212700000000     C     1             DO        24            X
212800000000     C*
212900000000     C     $FM(X)        IFNE      *BLANK
213000000000     C     $FC(X)        ANDNE     '0'
213100000000     C*
213200000000     C                   MOVEL     $FM(X)        IDMSG
213300000000     C                   CALLB     'XRTVM'
213400000000     C                   PARM                    IDMSG            27
213500000000     C                   PARM                    TXTMSG          100
213600000000     C                   PARM                    ERRMSG            1
213700000000     C*
213800000000     C     ERRMSG        IFNE      *ON
213900000000     C                   MOVEL     TXTMSG        $FX(X)
214000000000     C                   ELSE
214100000000     C                   MOVEL     *ALL' '       $FX(X)
214200000000     C                   ENDIF
214300000000     C*
214400000000     C                   ENDIF
214500000000     C*
214600000000     C                   ENDDO
214700000000     C*
214800000000     C                   ENDSR
214900000000     C/EJECT
215000000000     C************************************************************
215100000000     C* INIZIALIZZAZIONE VARIABILI
215200000000     C************************************************************
215300000000     C     INZVAR        BEGSR
215400000000     C*
215500000000     C* Dati da PGM guida
215600141222    >C***********        MOVEL     KPJBU         SDGSJ74DS
215700000000     C*
215800000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
215900000000     C* reperimento dati società
216000141222    >C                   If        SocietaJ74 <> xscsoc    and
216100141222    >C                             OpzJ74 <> *blank
216200000000     C                   MOVEL     'SOC001'      TIPXSC
216300141222    >C                   MOVEL     SocietaJ74    SOCXSC
216400000000     C                   EXSR      REPSOC
216500000000     C     RTNXSC        IFNE      '1'
216600000000     C                   MOVEL     XSOCDS        SOC001
216700000000     C                   ELSE
216800141222    >C                   MOVE      '1'           ErrJ74
216900000000     C                   MOVE      *on           $Fine
217000000000     C                   EXSR      EndPgm
217100000000     C                   ENDIF
217200000000     C                   endif
217300000000     C*
217400000000     C* Reperimento autorizzazioni
217500000000     C*
217600000000     C* le autorizzazioni potrebbero essere già state passate,
217700000000     C* nel qual caso non vanno reperite
217800000000    >C     AUTDS         IFEQ      *BLANK
217900000000    >C                   EXSR      REPAUT
218000000000    >C                   ELSE
218100000000    >C                   MOVEL     XCADDS        CONCIVDS
218200000000    >C                   ENDIF
218300000000     C*
218400000000     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
218500000000     C* le autorizzazione
218600000000     C*
218700000000    >C                   EXSR      TSTAUT
218800000000     C*
218900000000     C* Definizione dei tasti funzione utilizzabili a pgm
219000000000     C*
219100000000     C                   EXSR      DEFOPZ
219200000000     C*
219300000000     C* Variabili per gestione videate
219400000000     C*
219500000000     C                   MOVE      'D1'          $GEST                          fmt in gestione
219600000000     C                   MOVE      *OFF          $FINE                          fine pgm
219700000000     C                   MOVE      *OFF          $FlgDef                        fine pgm
219800000000     C                   MOVE      *ON           $INZD1                         inizializzione D1
219900000000     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
220000000000     C                   MOVE      *BLANK        $LASTV                         ultima emissione
220100000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
220200000000     C* azzero la posizione del cursore
220300000000     C                   Z-ADD     0             H1RIGA
220400000000     C                   Z-ADD     0             H1COLO
220500000000     C*
220600000000     C* Variabili appoggio
220700000000     C*
220800000000     C                   Z-ADD     0             CURR
220900000000     C                   Z-ADD     0             CURC
221000000000     C*
221100000000     C* Valorizzazione campi univoci testate
221200000000     C*
221300000000     C                   CLEAR                   FMTT1
221400000000     C                   MOVEL     KNSIF         NOMSIF
221500000000     C                   MOVEL     XSCDSI        NOMDIT
221600000000     C                   MOVEL     DSPGM         NOMPGM           10
221700000000     C                   MOVEL     KCDAZ         NOMAZN
221800000000     C* recupero il sottotitolo (se c'è)
221900000000     C                   MOVEL     *BLANK        T1TIT
222000000000     C     SOTTIT        IFNE      *BLANK
222100000000     C                   MOVEL     SOTTIT        IDMSG
222200000000     C                   CALLB     'XRTVM'
222300000000     C                   PARM                    IDMSG            27
222400000000     C                   PARM                    TXTMSG          100
222500000000     C                   PARM                    ERRMSG            1
222600000000     C                   PARM      *ON           CTRMSG            1            centratura
222700000000     C                   PARM      30            LENMSG            3 0          lun output.
222800000000     C     ERRMSG        IFNE      *ON
222900000000     C                   MOVEL     TXTMSG        T1TIT
223000000000     C                   ELSE
223100000000     C                   MOVEL     *ALL'?'       T1TIT
223200000000     C                   ENDIF
223300000000     C                   ENDIF
223400000000     C* recupero il tipo operazione
223500000000     C                   Z-ADD     1             X
223600141222    >C     OPZJ74        LOOKUP    $OZ(X)                                 21
223700000000     C     *IN21         IFEQ      *ON
223800000000     C                   MOVEL     $ID(X)        IDMSG
223900000000     C                   CALLB     'XRTVM'
224000000000     C                   PARM                    IDMSG            27
224100000000     C                   PARM                    TXTMSG          100
224200000000     C                   PARM                    ERRMSG            1
224300000000     C                   PARM      *Off          CTRMSG            1            centratura
224400000000     C                   PARM      15            LENMSG            3 0          lun output.
224500000000     C     ERRMSG        IFNE      *ON
224600000000     C                   MOVEL     TXTMSG        T1OPE
224700000000     C                   ELSE
224800000000     C                   MOVEL     *ALL'?'       T1OPE
224900000000     C                   ENDIF
225000000000     C                   ENDIF
225100000000     C*
225200000000     C* Inizializzazione/Decodifica prima videata
225300000000     C                   EXSR      REPDEF
225400000000     C                   EXSR      INZD1
225500000000     C                   MOVE      *OFF          $INZD1
225600130918     C*
225700000000     C                   ENDSR
225800000000     C/EJECT
225900000000     C************************************************************
226000000000     C* REPERIMENTO DATI SOCIETA'
226100000000     C************************************************************
226200000000     C     REPSOC        BEGSR
226300000000     C*
226400000000     C                   CALL      'SDGSOCDR'
226500000000     C                   PARM                    TIPXSC            6
226600000000     C                   PARM                    SOCXSC            3
226700000000     C                   PARM                    CDSXSC            9 0
226800000000     C                   PARM                    MODXSC            3
226900000000     C                   PARM      *BLANKS       RTNXSC            1
227000000000     C                   PARM                    XSOCDS
227100000000     C                   PARM                    KPJBA
227200000000     C*
227300000000     C                   ENDSR
227400000000     C/EJECT
227500000000     C************************************************************
227600000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
227700000000     C************************************************************
227800000000     C     REPAUT        BEGSR
227900000000     C*
228000000000     C                   CLEAR                   AUTDS
228100000000     C                   Clear                   ConCivDs
228200000000     C*
228300000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
228400000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
228500000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
228600000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
228700000000    >C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
228800000000    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
228900000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
229000000000    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
229100000000     C*
229200000000     C                   CALLB     'XCHKAUT'
229300000000     C                   PARM                    AUTDS
229400000000     C*
229500000000     C* errori/non abilitazione
229600000000     C     XCARTN        IFNE      0
229700141222    >C                   MOVE      '2'           ErrJ74
229800000000     C                   MOVE      *ON           $FINE
229900000000     C                   EXSR      ENDPGM
230000000000     C                   ELSE
230100000000    >C                   MOVEL     XCADDS        CONCIVDS
230200000000     C                   ENDIF
230300000000     C*
230400000000     C                   ENDSR
230500000000     C/EJECT
230600000000     C************************************************************
230700000000     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
230800000000     C************************************************************
230900000000     C     TSTAUT        BEGSR
231000000000     C*
231100000000     C                   SELECT
231200000000     C*
231300000000     C* se richiamato in immissione
231400141222    >C     OPZJ74        WHENEQ    '01'
231500000000     C* ma l'utente non è abilitato
231600000000    >C     IMMALL        IFEQ      *off
231700000000     C* esco da pgm avvertendolo
231800141222    >C                   MOVE      '2'           ERRJ74
231900000000     C                   MOVE      *ON           $FINE
232000000000     C                   EXSR      ENDPGM
232100000000     C                   ENDIF
232200000000     C*
232300000000     C* se richiamato in modifica
232400141222    >C     OPZJ74        WHENEQ    '02'
232500000000     C* ma l'utente non è abilitato
232600000000    >C     MANALL        IFEQ      *OFF
232700000000     C* testo se l'utente è abilitato almeno alla visualizzazione
232800000000    >C     VISALL        IFEQ      *ON
232900141222    >C                   MOVE      '05'          OPZJ74
233000000000     C                   ELSE
233100000000     C* altrimenti esco da pgm avvertendolo
233200141222    >C                   MOVE      '2'           ERRJ74
233300000000     C                   MOVE      *ON           $FINE
233400000000     C                   EXSR      ENDPGM
233500000000     C                   ENDIF
233600000000     C                   ENDIF
233700000000     C*
233800000000     C* se richiamato in copia
233900141222    >C     OPZJ74        WHENEQ    '03'
234000000000     C* ma l'utente non è abilitato
234100000000    >C     IMMALL        IFEQ      *OFF
234200000000     C* esco da pgm avvertendolo
234300141222    >C                   MOVE      '2'           ERRJ74
234400000000     C                   MOVE      *ON           $FINE
234500000000     C                   EXSR      ENDPGM
234600000000     C                   ENDIF
234700000000     C*
234800000000     C* se richiamato in annullamento
234900141222    >C     OPZJ74        WHENEQ    '04'
235000000000     C* ma l'utente non è abilitato
235100000000    >C     ANNALL        IFEQ      *OFF
235200000000     C* esco da pgm avvertendolo
235300141222    >C                   MOVE      '2'           ERRJ74
235400000000     C                   MOVE      *ON           $FINE
235500000000     C                   EXSR      ENDPGM
235600000000     C                   ENDIF
235700000000     C*
235800000000     C* se richiamato in visualizzazione
235900141222    >C     OPZJ74        WHENEQ    '05'
236000000000     C* ma l'utente non è abilitato
236100000000    >C     VISALL        IFEQ      *OFF
236200000000     C* esco da pgm avvertendolo
236300141222    >C                   MOVE      '2'           ERRJ74
236400000000     C                   MOVE      *ON           $FINE
236500000000     C                   EXSR      ENDPGM
236600000000     C                   ENDIF
236700000000     C*
236800000000     C                   ENDSL
236900000000     C*
237000000000     C                   ENDSR
237100000000     C/EJECT
237200000000     C************************************************************
237300000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
237400000000     C************************************************************
237500000000     C     DEFOPZ        BEGSR
237600000000     C*
237700000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1
237800000000     C* con S o N a seconda della utilizzabilità o meno
237900000000     C* del tasto funzione correlato
238000000000     C*
238100000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
238200000000     C* di compilazione
238300000000     C                   RESET                   $FC1
238400000000     C                   RESET                   $FR1
238500000000     C*
238600000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
238700000000     C* invalido alcune opzioni
238800000000     C                   SELECT
238900000000     C* Se pgm richiamato in interrogazione disattivo :
239000141222    >C     OPZJ74        WHENEQ    '05'
239100000000     C* F2=Decodifica
239200000000     C                   Z-ADD     2             X
239300000000     C                   MOVE      '0'           $FC1(X)
239400000000     C* F4=Ricerca
239500000000     C                   Z-ADD     4             X
239600000000     C                   MOVE      '0'           $FC1(X)
239700000000     C* F5=Ripristino
239800000000     C                   Z-ADD     5             X
239900000000     C                   MOVE      '0'           $FC1(X)
240000000000     C* F6=Conferma
240100000000     C                   Z-ADD     06            X
240200000000     C                   MOVE      '0'           $FC1(X)
240300000000     C* Se pgm richiamato in annullamento disattivo :
240400141222    >C     OPZJ74        WHENEQ    '04'
240500000000     C* F2=Decodifica
240600000000     C                   Z-ADD     2             X
240700000000     C                   MOVE      '0'           $FC1(X)
240800000000     C* F4=Ricerca
240900000000     C                   Z-ADD     4             X
241000000000     C                   MOVE      '0'           $FC1(X)
241100000000     C* F5=Ripristino
241200000000     C                   Z-ADD     5             X
241300000000     C                   MOVE      '0'           $FC1(X)
241400000000     C*
241500000000     C                   OTHER
241600000000     C*
241700000000     C                   ENDSL
241800000000     C*
241900000000     C                   ENDSR
242000000000     C/EJECT
242100000000     C************************************************************
242200000000     C* REPERIMENTO CAMPI DI DEFAULT
242300000000     C************************************************************
242400000000     C     REPDEF        BEGSR
242500000000     C*
242600000000     C                   Eval      $FlgDef     = *On
242700000000     C*
242800000000     C                   ENDSR
242900000000     C************************************************************
243000000000     C* DEFINIZIONE KLIST
243100000000     C************************************************************
243200000000     C     DefKlist      BEGSR
243300000000     C*
243400000000     C* klist
243500141222     C*
243600141222     C* SDGDIE01L
243700141222     C     K03DIE01      KLIST
243800141222     C                   KFLD                    DIESocieta
243900141222     C                   KFLD                    DIEDtInter
244000141222     C                   KFLD                    DIENrInter
244100000000     C*
244200000000     C                   ENDSR
244300000000     C/EJECT
244400000000     C************************************************************
244500000000     C* RIEMPIMENTO TASTI FUNZIONE
244600000000     C************************************************************
244700000000     C     RIEKEY        BEGSR
244800000000     C*
244900000000     C* Input :
245000000000     C* - $FX = schiera con tutti i testi disponibili
245100000000     C* - $FC = schiere con i flag di validità associati ai testi
245200000000     C* - $FR = schiere con i flag di REVERSE IMAGE
245300000000     C* - $ULK = ultimo testo caricato
245400000000     C* - $L01 = Lunghezza prima riga
245500000000     C* - $L02 = Lunghezza seconda riga
245600000000     C* Output :
245700000000     C* - $ULK
245800000000     C* - $KEY1= 1° riga di tasti funzione
245900000000     C* - $KEY2= 2° riga di tasti funzione
246000000000     C*
246100000000     C                   CALLB     'XKEYMSG'
246200000000     C* input
246300000000     C                   PARM                    $FX
246400000000     C                   PARM                    $FC
246500000000     C                   PARM                    $FR
246600000000     C                   PARM                    $ULK              3 0
246700000000     C* output
246800000000     C                   PARM                    $KEY1            79
246900000000     C                   PARM                    $KEY2            79
247000000000     C* input
247100000000     C                   PARM                    $L01              3 0
247200000000     C                   PARM                    $L02              3 0
247300000000     C*
247400000000     C                   PARM                    $ALLFUNCT       480
247500000000     C*
247600000000     C                   ENDSR
247700000000** $OZ / $ID
24780000000001SDGMSG    *LIBL     SDC0002
24790000000002PROMSG    *LIBL     COS0002
24800000000003PROMSG    *LIBL     COS0003
24810000000004PROMSG    *LIBL     COS0004
24820000000005PROMSG    *LIBL     COS0005
248300000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
248400000000PROMSG    *LIBL     KEY0001  01
248500000000PROMSG    *LIBL     KEY0002  02
248600000000PROMSG    *LIBL     KEY0003  03
248700000000PROMSG    *LIBL     KEY0004  04
248800000000PROMSG    *LIBL     KEY0005  05
248900000000PROMSG    *LIBL     KEY0006  06
249000000000PROMSG    *LIBL     KEY0007  07
249100000000PROMSG    *LIBL     KEY0008  08
249200000000PROMSG    *LIBL     KEY0009  09
249300000000PROMSG    *LIBL     KEY0010  10
249400000000PROMSG    *LIBL     KEY0011  11
249500000000PROMSG    *LIBL     KEY0012  12
249600000000PROMSG    *LIBL     KEY0013  13
249700000000PROMSG    *LIBL     KEY0014  14
249800000000PROMSG    *LIBL     KEY0015  15
249900000000PROMSG    *LIBL     KEY0016  16
250000000000PROMSG    *LIBL     KEY0017  17
250100000000PROMSG    *LIBL     KEY0018  18
250200000000PROMSG    *LIBL     KEY0019  19
250300000000PROMSG    *LIBL     KEY0020  20
250400000000PROMSG    *LIBL     KEY0021  21
250500000000PROMSG    *LIBL     KEY0022  22
250600000000PROMSG    *LIBL     KEY0023  23
250700000000PROMSG    *LIBL     KEY0024  24
250800000000**  ABILITAZIONE D1 : $FC1 / $FR1
2509000000000N  01
2510000000000N  02
2511000000001N  03
2512000000001N  04
2513000000001N  05
2514000000001N  06
2515000000000N  07
2516000000001N  08
2517000000000N  09
2518000000000N  10
2519000000000N  11
2520000000001N  12
2521000000000N  13
2522000000000N  14
2523000000000N  15
2524000000000N  16
2525000000000N  17
2526000000000N  18
2527000000000N  19
2528000000000N  20
2529000000000N  21
2530000000000N  22
2531000000000N  23   Impostare a '1' se si utilizzano opzioni
2532000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
