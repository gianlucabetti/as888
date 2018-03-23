000100000113     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400940127      *----------*
000500940317    > * STD002R  *
000600940127      *----------*
000700940117      *
000800940613      *            GESTIONE TABELLA
000900940223      *
001000940223      *
001100940615      * DOCUMENTI MICROANALISI :
001200940223      *
001300940223      * NOTE TECNICHE :
001400940223      *
001500940223      *
001600940614      * PERSONALIZZAZIONI
001700940223      *
001800940223      *
001900940223      *
002000940223      *               MAPPA INDICATORI STD
002100940223      *
002200940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002300940620      *  03           PROTECT TUTTI I CAMPI
002400940223      *  21           GENERICO OPERAZIONI I/O
002500940223      *  22           GENERICO ERRORE OPERAZIONI I/O
002600940223      *  30           SFLDSP
002700940223      * N31           SFLCLR
002800940128      *  31           SFLDSPCTL
002900940128      *  32           SFLNXTCHG
003000940128      *  33           SFLEND
003100940128      *  39           OF PRTF
003200940315      *  40 <---> 49  DSPATR ERRORI SU SFL
003300940317    > *  Specificare l'uso dei singoli indicatori
003400940315      *  50 <---> 98  ERRORI SU VIDEO
003500940317    > *  Specificare l'uso dei singoli indicatori
003600940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003700940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003800940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003900940128      *  99           INDIC. GENERALE DI ERRORE
004000940128     F*----------------------------------------------------*
004100940614$001 FANGYP3D2  CF   E             WORKSTN
004200940201     F                                     INFDS(DSFMT)
004300940621     FANTBT01L  UF   E           K DISK
004400040616     F*--------
004500040616     FANCLN01L  IF   E           K DISK
004600940128     D*----------------------------------------------------*
004700940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
004800940224     D $ID             S             27    DIM(5) ALT($OZ)
004900940506     D* Schiere per la gestione dei tasti di funzione
005000940506     D $FM             S             27    DIM(24)
005100940506     D $FX             S             50    DIM(24)
005200940506     D $FC             S              1    DIM(24)
005300940506     D $FR             S              1    DIM(24)
005400940506     D* Video D1
005500940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005600940506     D $FX1            S             50    DIM(24) ALT($FM1)
005700940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005800940506     D $FR1            S              1    DIM(24) ALT($FC1)
005900940615     D* campi in lingua
006000940622$010 D*LIN             S             06    DIM(0011) CTDATA PERRCD(001)
006100940622$010 D*LIND            S             50    DIM(0011) ALT(LIN)
006200940614$003 D EXTDS         E DS                  EXTNAME(ANGYP3DS) INZ
006300940127     D* Passaggio Parametri
006400940127     D KPJBA         E DS
006500040616     D* ds input ANA201DS
006600040616     D ANA201DS      E DS
006700040616     D* ds input X17CLVALDS
006800040616     D X17CLVALDS    E DS                  Inz
006900040616  "  D* Controlli validità legende
007000040616A0970D X07DS         E DS                  EXTNAME(X07VALDS)
007100040616     D* DS esterna contabilità
007200040616     D ANGG71DS      E DS
007300040616     D* Ricerca legende
007400040616     D ANA964DS      E DS
007500040616     D* Parametri per ricerca codice ANATB00F
007600040616     D XTABDS        E DS                  INZ
007700941103     D*-------------
007800941103      * DS Società
007900941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
008000960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
008100960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
008200941103     D*-------------
008300941103      * DS Interna per dati utente
008400941103     D XSOCDS          DS          1000
008500941103     D*-------------
008600940616     D XATBDS        E DS                  INZ
008700940318     D*-------------
008800940318     D* DS Esterna gestione messaggi per COMIT
008900940616     D XMSGDS        E DS                  INZ
009000940318     D  STS          E                     EXTFLD(MSGSTS)
009100940318     D                                     DIM(333)
009200940318     D  JBA          E                     EXTFLD(MSGJBA)
009300940318     D                                     DIM(502)
009400940203     D*-------------
009500940620     D* DS Esterna autorizzazioni tabelle
009600940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
009700940620     D*
009800940620     D* DS  restituzione autorizzazioni
009900940620     D* IND=indice
010000940620     D* GRU=man. di gruppo
010100940620     D* AZI=man. azienda
010200940712     D* ABI=abilit. ai riservati
010300941111     D AUTDS           DS
010400940620     D  MANIND                 1      1
010500940620     D  MANGRU                 2      2
010600940620     D  MANAZI                 3      3
010700940712     D  ABIRIS                 4      4
010800940620     D*-------------
010900940207     D* opzione richiesta
011000940614    >D D002DS          DS
011100940614     D  OPZ002                 1      2
011200940614     D* 0 = ritorno normale
011300940614     D* 1 = F3
011400940614     D* 2 = F12
011500940614     D  RET002                 3      3
011600940614     D* operazione eseguite dal pgm chiamato :
011700940614     D* 0 = nessuna operazione
011800940614     D* 1 = eseguito aggiornamento
011900940614     D  OPR002                 4      4
012000940614     D* funzione non eseguibile per errore :
012100940614     D* 0 = nessun errore
012200940614     D* 1 = funzione richiamata chiusa in errore
012300940614     D* Aggiungere eventuali altri codici di errore
012400940614     D  ERR002                 5      5
012500940614     D* Azienda
012600940614     D  AZI002                 6      8
012700940614     D* Lingua
012800940614     D  LIN002                 9      9
012900940614     D* Codice scelto
013000940614     D  KEY002                10     24
013100940620     D* Autorizzazione
013200940620     D  AUT002                25     25
013300970509     D* Unita
013400970509     D  UNI002                26     33
013500940614     D*-------------
013600940201     D*-------------
013700940201     D DSFMT           DS
013800940506     D  $TASTO               369    369
013900940201     D  NRG                  370    370
014000940201     D  NCL                  371    371
014100940201     D*-------------
014200940201     D* posizione cursore
014300940201     D CURSOR          DS
014400940223     D  RIRI                   1      2B 0 INZ
014500940201     D  R$$                    2      2
014600940223     D  COCO                   3      4B 0 INZ
014700940201     D  C$$                    4      4
014800940207     D*-------------
014900940207     D* Reperimento nome PGM
015000940207     D STATUS         SDS           333
015100940207     D  DSPGM            *PROC
015200010406X0113D*-------------
015300010406  "  D* Reperimento titolo tabella
015400010618  "  D  WRKTIT         S             35A
015500010406  "  D  WRKTBCOD       S                   LIKE(XTBCOD)
015600010406  "  D  LENTIT         S              3  0
015700010406X0113D  I              S              3  0
015800940225     D*-------------
015900940225     D* COSTANTI
016000940225     D*-------------
016100940506     D*
016200940506     D* Tasti di funzione
016300940506     D F01             C                   CONST(X'31')
016400940506     D F02             C                   CONST(X'32')
016500940506     D F03             C                   CONST(X'33')
016600940506     D F04             C                   CONST(X'34')
016700940506     D F05             C                   CONST(X'35')
016800940506     D F06             C                   CONST(X'36')
016900940506     D F07             C                   CONST(X'37')
017000940506     D F08             C                   CONST(X'38')
017100940506     D F09             C                   CONST(X'39')
017200940506     D F10             C                   CONST(X'3A')
017300940506     D F11             C                   CONST(X'3B')
017400940506     D F12             C                   CONST(X'3C')
017500940506     D F13             C                   CONST(X'B1')
017600940506     D F14             C                   CONST(X'B2')
017700940506     D F15             C                   CONST(X'B3')
017800940506     D F16             C                   CONST(X'B4')
017900940506     D F17             C                   CONST(X'B5')
018000940506     D F18             C                   CONST(X'B6')
018100940506     D F19             C                   CONST(X'B7')
018200940506     D F20             C                   CONST(X'B8')
018300940506     D F21             C                   CONST(X'B9')
018400940506     D F22             C                   CONST(X'BA')
018500940506     D F23             C                   CONST(X'BB')
018600940506     D F24             C                   CONST(X'BC')
018700940506     D ENTER           C                   CONST(X'F1')
018800940506     D ROLDWN          C                   CONST(X'F4')
018900940506     D ROLLUP          C                   CONST(X'F5')
019000940506     D*-------------
019100940506     D*
019200941108     D DATA            C                   CONST('0001-01-01')
019300040616     D F14x17pgm       C                   CONST('F14=Simula calcolo gg valuta')
019400940128     I*----------------------------------------------------*
019500940615     ID1
019600940614$005 I              D1RIMLUN                    RIMLUNYP3
019700940614$005 I              D1RIMMAR                    RIMMARYP3
019800940614$005 I              D1RIMMER                    RIMMERYP3
019900940614$005 I              D1RIMGIO                    RIMGIOYP3
020000940614$005 I              D1RIMVEN                    RIMVENYP3
020100940614$005 I              D1GVALUN                    GVALUNYP3
020200940614$005 I              D1GVAMAR                    GVAMARYP3
020300940614$005 I              D1GVAMER                    GVAMERYP3
020400940614$005 I              D1GVAGIO                    GVAGIOYP3
020500940614$005 I              D1GVAVEN                    GVAVENYP3
020600940614$005 I              D1CALDTV                    CALDTVYP3
020700940614$005 I              D1CALEND                    CALENDYP3
020800940127     I*
020900940127     C*----------------------------------------------------*
021000940127     C*                MAIN LINE PROGRAM
021100940127     C*----------------------------------------------------*
021200940127     C*
021300940223     C* inizializzazione variabili
021400940223     C                   EXSR      INZVAR
021500940223     C*
021600940223     C     $FINE         DOWEQ     *OFF
021700940202     C     $GEST         CASEQ     'D1'          GESD1
021800940117     C                   END
021900940117     C                   END
022000940201     C*
022100940325     C* fine programma
022200940325     C                   EXSR      ENDPGM
022300940325     C/EJECT
022400940325     C************************************************************
022500940325     C* RICERCHE
022600940325     C************************************************************
022700940325     C     ENDPGM        BEGSR
022800940325     C*
022900940614     C* disallocazione rcd nel caso di
023000971110$007 C                   MOVE      'YP3'         XTBCOD
023100940614    >C                   MOVE      '7'           XTBRIC
023200970214     C                   CALLB     'XATB'
023300940614     C                   PARM                    XATBDS
023400940325     C*
023500940325     C* passaggio dati al pgm chiamante
023600940715     C                   MOVE      *BLANKS       KPJBU
023700940325    >C                   MOVEL     D002DS        KPJBU
023800940325     C*
023900940325     C                   SETON                                            RT
024000940325     C*
024100940325     C                   ENDSR
024200940325     C/EJECT
024300940201     C************************************************************
024400940201     C* RICERCHE
024500940201     C************************************************************
024600940127     C     SEARCH        BEGSR
024700940127     C*
024800940127     C* determino Riga/Colonna del cursore
024900940127     C                   MOVE      NRG           R$$
025000940127     C                   MOVE      NCL           C$$
025100940127     C                   Z-ADD     RIRI          CURR
025200940127     C                   Z-ADD     COCO          CURC
025300940127     C*
025400940224     C* I campi che al momento dell'F4 risultano protetti non devono
025500940224     C* essere abilitati alla funzione di ricerca
025600940224     C*
0257009401271    C                   SELECT
025800940127     C*
025900040616     C* FMTD1  - CLIENTE
026000040616    >C     H1NMFL        WHENeq    'D1COD'
026100940207     C* imposto pos. del cursore
026200040616     C                   Z-ADD     CURR          H1RIGA
026300040616     C                   Z-ADD     CURC          H1COLO
026400940127     C*
026500040616     C                   RESET                   ANA201DS
026600040616     C                   MOVE      xscSOC        SOC201
026700040616     C                   MOVEL     'C'           KCC201
026800040616     C                   MOVEL     D1COD         KSC201
026900040616     C                   MOVEL     ANA201DS      KPJBU
027000040616     C                   CALL      'ANA201R'
027100040616     C                   PARM                    KPJBA
027200040616     C                   MOVEL     KPJBU         ANA201DS
027300040616     C     KSC201        IFNE      *BLANK
027400040616     C                   MOVEL     KSC201        D1COD
027500040616     C     KEYcln        CHAIN     ancln01l                           51
027600040616     c  n51              eval      d1dcod = CLNDESBREV
027700040616     C                   ENDIF
027800040616     C*
027900040616     C* FMTD1  - Metodo
028000040616    >C     H1NMFL        WHENeq    'D1CALDTV'
028100040616     C                   RESET                   ANA964DS
028200040616     C                   MOVEL     '01'          OPZ964
028300040616     C                   MOVEL     'ANBCO000'    TRC964
028400040616     C                   MOVE      *on           All964
028500040616     C                   MOVEL     'BCOCALDTV'   CAM964
028600040616     C                   MOVEL     ANA964DS      KPJBU
028700040616     C                   CALL      'ANA964R'
028800040616     C                   PARM                    KPJBA
028900040616     C                   MOVEL     KPJBU         ANA964DS
029000040616     C*
029100040616     C     VAL964        IFNE      *BLANK
029200040616     C                   MOVEL     VAL964        CALDTVyp3
029300040616     C                   ENDIF
029400040616     C*
029500040616     C* FMTD1  - Calendario
029600040616    >C     H1NMFL        WHENeq    'D1CALEND'
029700040616     C                   RESET                   xtabds
029800040616     C                   MOVE      *blank        XTAKEY
029900040616     C                   MOVE      *blank        CalendYP3
030000040616     C                   MOVE      '01'          XTAOPZ
030100040616     C                   MOVE      XSCSOC        XTAAZI
030200040616     C                   MOVE      XSCLIN        XTALIN
030300040616     C                   Z-ADD     CURR          XTARIG
030400040616     C                   Z-ADD     CURC          XTACOL
030500040616     C                   CLEAR                   KPJBU
030600040616     C                   MOVEL     XTABDS        KPJBU
030700040616     C                   CALL      'ANGG71R'
030800040616     C                   PARM                    KPJBA
030900040616     C                   MOVEL     KPJBU         XTABDS
031000040616     C     XTAERR        IFNE      '1'
031100040616     C                   MOVE      XTAKEY        CalendYP3
031200040616     C                   ELSE
031300040616     C                   MOVE      *BLANK        CalendYP3
031400040616     C                   ENDIF
031500940223     C*
031600940223     C* ricerca non abilitata in questa posizione
0317009402231O   C                   OTHER
031800941005     C                   SETON                                        9899
031900941005     C* imposto pos. del cursore
032000941005     C                   Z-ADD     CURR          H1RIGA
032100941005     C                   Z-ADD     CURC          H1COLO
032200940127     C*
0323009401271-   C                   ENDSL
032400940127     C*
032500940506     C                   MOVE      *HIVAL        $LASTV
032600940127     C                   ENDSR
032700940615     C/EJECT
032800940615     C************************************************************
032900940615     C* TRADUZIONI
033000940615     C************************************************************
033100940615     C     LINGUA        BEGSR
033200940615     C*
033300940615     C* determino Riga/Colonna del cursore
033400940615     C                   MOVE      NRG           R$$
033500940615     C                   MOVE      NCL           C$$
033600940615     C                   Z-ADD     RIRI          CURR
033700940615     C                   Z-ADD     COCO          CURC
033800940615     C*
033900940615     C*
034000940615     C*
034100940615     C* FMTD1  - CAMPI DA TRADURRE
034200940615     C* imposto pos. del cursore
034300940615     C                   Z-ADD     CURR          H1RIGA
034400940615     C                   Z-ADD     CURC          H1COLO
034500940621     C                   MOVEL     H1NMFL        W005              5
034600940621     C                   MOVE      W005          W003              3
034700940621     C                   MOVEL     W003          W006              6
034800040617     C                   MOVE      *blank        KEY              15
034900040616     C                   MOVEL     D1COD         KEY
035000940615     C                   SELECT
035100940615     C*
035200940615     C*
035300940615     C* ricerca non abilitata in questa posizione
0354009406151O   C                   OTHER
035500940615     C                   SETON                                        98
035600940615     C*
0357009406151-   C                   ENDSL
035800940615     C*
035900940615     C                   MOVE      *HIVAL        $LASTV
036000940615     C                   ENDSR
036100940225     C/EJECT
036200940225     C************************************************************
036300940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
036400940225     C************************************************************
036500940225     C     GESPIE        BEGSR
036600940225     C*
036700940225     C                   SELECT
036800940225     C*
036900940506     C     $GEST         WHENEQ    'D1'
037000940506     C     $ULKD1        IFEQ      0
037100940506     C* riempimento opzioni
037200940506     C                   EXSR      F24D1
037300940506     C                   ELSE
037400940614     C                   WRITE     Z1
037500940506     C                   ENDIF
037600940225     C*
037700940510     C*
037800940225     C                   ENDSL
037900940225     C*
038000940225     C                   ENDSR
038100940131     C/EJECT
038200940131     C************************************************************
038300940510     C* GESTIONE VIDEO RECORD D1
038400940131     C************************************************************
038500940127     C     GESD1         BEGSR
038600940225     C* inizializzazione videata
038700940223     C     $INZD1        IFEQ      *ON
038800940127     C                   EXSR      INZD1
038900940223     C                   MOVE      *OFF          $INZD1
039000940117     C                   END
039100940225     C* emissione piede videata se proveniente da altra
039200940225     C* salvataggio valore formato di provenienza
039300940207     C     $LASTV        IFNE      'D1'
039400940614     C                   WRITE     T1
039500940225     C                   EXSR      GESPIE
039600940202     C                   MOVE      $LASTV        $LASTG
039700940202     C                   MOVE      'D1'          $LASTV
039800940202     C                   END
039900940131     C*
040000940203     C                   SELECT
040100940203     C* Annullamento
040200940315    >C     OPZ002        WHENEQ    '04'
040300940224     C* Visualizzazione
040400940315    >C     OPZ002        OREQ      '05'
040500940620     C                   SETON                                        0203
040600940620     C* Manutenzione
040700940620    >C     OPZ002        WHENEQ    '02'
040800940620     C                   SETON                                        02
040900940620     C                   SETOFF                                       03
041000940620     C*
041100940620     C                   OTHER
041200940620     C*
041300940620     C                   SETOFF                                       0203
041400940203     C                   ENDSL
041500940620     C* Immissione/Modifica/Copia
041600940620     C                   EXFMT     D1
041700940506     C* controllo abilitazione tasti funzione
041800940506     C*
041900940506     C                   EXSR      CTRKD1
042000940506     C*
042100940506     C     *IN99         IFEQ      *OFF
042200940131     C*
0423009402031    C                   SELECT
042400940510     c* ROLLUP
042500940131     C* F3=Fine
042600940506     C     $TASTO        WHENEQ    F03
042700940309     C                   EXSR      F03D1
042800940131     C* F4=Ricerca
042900940506     C     $TASTO        WHENEQ    F04
043000940127     C                   EXSR      SEARCH
043100940224     C* F5=Refresh
043200940506     C     $TASTO        WHENEQ    F05
043300940309     C                   EXSR      F05D1
043400940223     C* F8=Successivo
043500940506     C     $TASTO        WHENEQ    F08
043600940309     C                   EXSR      F08D1
043700940131     C* F12=Ritorno
043800940506     C     $TASTO        WHENEQ    F12
043900940309     C                   EXSR      F12D1
044000040616     C* F14=simulazione
044100040616     C     $TASTO        WHENEQ    F14
044200040616     C                   EXSR      F14D1
044300940315     C* F24=Altri tasti
044400940506     C     $TASTO        WHENEQ    F24
044500940315     C                   EXSR      F24D1
044600940131     C*
0447009402031O   C                   OTHER
044800940127     C                   EXSR      CTRD1
044900940616     C     *IN99         IFEQ      *OFF
045000940616     C                   SELECT
045100940616     C* F7=Traduzione
045200940616     C     $TASTO        WHENEQ    F07
045300940616     C                   EXSR      LINGUA
045400940616     C* F6
0455009406162    C     $TASTO        WHENEQ    F06
045600940224     C* eseguo aggiornamento
045700940315    >C                   EXSR      AGGANA
045800940224     C* eseguo operazioni del dopo-aggiornamento
0459009403183    C     *IN99         IFEQ      *OFF
046000940318     C                   EXSR      GESAGG
0461009403183-   C                   ENDIF
0462009406162-   C                   ENDSL
046300940616     C                   ENDIF
046400940203     C*
0465009402241-   C                   ENDSL
046600940224     C*
0467009405062-   C                   ENDIF
046800940117     C                   ENDSR
046900940506     C/EJECT
047000940506     C************************************************************
047100940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
047200940506     C************************************************************
047300940506     C     CTRKD1        BEGSR
047400940506     C                   SETOFF                                       99
047500940506     C                   MOVEA     $FC1          $FC
047600940506     C                   EXSR      CTRKEY
047700940506     C     *IN99         IFEQ      *ON
047800940506     C                   SETON                                        97
047900940506     C                   ENDIF
048000940506     C                   ENDSR
048100940117      ************************************************************
048200940207      * INIZIALIZZAZIONE VIDEATA DATI
048300940117      ************************************************************
048400940127     C     INZD1         BEGSR
048500940131     C*
048600940615     C                   CLEAR                   D1
048700040616     c                   eval      rimlunyp3 = xscoff
048800040616     c                   eval      rimmaryp3 = xscoff
048900040616     c                   eval      rimmeryp3 = xscoff
049000040616     c                   eval      rimgioyp3 = xscoff
049100040616     c                   eval      rimvenyp3 = xscoff
049200010406X0113C*
049300010406  "  C* reperimento titolo
049400010406  "  C                   clear                   xatbds
049500010406  "  C                   eval      xtbkey      = *zero
049600010417X0113C* inserita la seguenta riga
049700010417$002 C                   move      'YP3'         xtbkey
049800010417X0113C                   eval      xtbazi      = xscsoc
049900010406  "  C                   eval      xtblin      = xsclin
050000010406  "  C                   eval      xtbcod      = 'AAA'
050100010406  "  C                   eval      xtbric      = '1'
050200010406  "  C                   eval      xtbalc      = '0'
050300010406  "  C                   CALLB     'XATB'
050400010406  "  C                   PARM                    XATBDS
050500010406  "  C                   if        xtberr <> *off
050600010406  "  C                   movel     *all'?'       WrkTit
050700010406  "  C                   else
050800010406  "  C                   movel     xtbuni        WrkTit
050900010618  "  C                   move      '     '       WrkTit
051000010406  "  C                   endif
051100010406  "  C*
051200010406  "  C* centratura
051300010406  "  C                   eval      WrkTit=%trimr(Wrktit)
051400010406  "  C     ' '           CHECKR    Wrktit        I
051500010406  "  C                   call      'X71CENTRA'
051600010406  "  C                   parm                    Wrktit
051700010406  "  C                   parm                    I
051800010406  "  C                   parm                    lentit
051900010406  "  C                   movel     Wrktit        T1tit
052000010406X0113C*
052100990827X0079C* Aggiunta riga seguente
052200040616$028 C                   move      xscon         D1RIM1
052300040616$028 C                   move      xscoFF        D1RIM0
052400990827$028 C
052500941006     C                   MOVE      '0'           D1RIS
052600040616     C                   MOVEL     KEY002        D1COD
052700940615     C                   MOVE      AZI002        XTBAZI
052800940615     C                   MOVE      LIN002        XTBLIN
052900940616     C                   MOVE      *ALL'0'       XTBKEY
053000940615     C                   MOVE      KEY002        XTBKEY
053100970509     C                   MOVE      UNI002        XTBUNT
053200940615$007 C                   MOVE      'YP3'         XTBCOD
053300940225     C*
053400940207     C*
053500940224     C* valorizzo la chiave univoca per reperire i dati
053600940224     C* se non immissione
0537009403151   >C     OPZ002        IFNE      '01'
053800940315    >C*-------------------------------------------------------------
053900940315    >C* impostare i campi chiave ricevuti
054000940315    >C*-------------------------------------------------------------
054100940318     C*
054200940614     C*
054300940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0544009406142   >C     OPZ002        IFEQ      '02'
054500940315    >C     OPZ002        OREQ      '04'
054600940614     C                   MOVE      '1'           XTBALC
054700940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0548009406142E   C                   ELSE
054900940614     C                   MOVE      '0'           XTBALC
0550009406142-   C                   ENDIF
055100940614     C*
055200940616     C                   MOVE      '1'           XTBRIC
055300970214     C                   CALLB     'XATB'
055400940614     C                   PARM                    XATBDS
055500940318     C*
055600940614     C     XTBERR        IFEQ      '0'
055700940614     C                   MOVEL     XTBUNI        EXTDS
055800970512     c                   move      xtbunt        d1uniTA           8
055900941108     C* Giro la data se non blank
056000970214     c                   move      xtbdsc        xtbdscA          10
056100970214     C     XTBDSCa       IFNE      DATA
056200970214     C                   CALLB     'XDT4000'
056300970214     C                   PARM      XTBDSCA       XDTAMG           10
056400940621     C                   PARM      *ZEROS        XDTGMA            6 0
056500940621     C                   PARM      2             XDTSTA            1 0
056600940621     C                   MOVE      XDTGMA        D1DSC
056700940712     C                   MOVE      XTBRIS        D1RIS
056800941108     C                   ELSE
056900941108     C                   MOVE      *ZEROS        D1DSC
057000941108     C                   ENDIF
057100941108     C                   ENDIF
057200940614     C*
057300940224     C*
057400940224     C* se il rcd da gestire non viene trovato
057500940318     C* oppure è allocato da altro pgm
057600940224     C* ritorno al pgm guida avvertendolo dell'errore
0577009406142    C     XTBERR        IFNE      '0'
057800940224     C                   MOVE      *ON           $FINE
057900940315    >C                   MOVE      '1'           ERR002
058000940224     C*
058100940224     C* altrimenti
0582009402242E   C                   ELSE
058300940224     C* riempio la videata
058400940224     C                   EXSR      RIED1
058500940224     C* richiamo routine dei ctrl per decodificarla
058600940614    >C                   EXSR      CTRD1
058700960228     C                   MOVEL     *IN99         SAV99             1
058800960228     C* Se la gestione è più videate
058900960305    >C                   EXSR      CTRD2
059000960305    >C     *IN99         IFEQ      *ON
059100960305    >C                   MOVEL     *IN99         SAV99             1
059200960305    >C                   ENDIF
059300940224     C                   MOVE      *ALL'0'       IN5098
059400940224     C                   MOVEA     IN5098        *IN(50)
059500940224     C* definizione indicatori su video
059600940224     C                   SETOFF                                       02
0597009402243    C                   SELECT
059800940315    >C     OPZ002        WHENEQ    '02'
059900940224     C                   SETON                                        02
060000940315    >C     OPZ002        WHENEQ    '05'
060100940506     C* disabilita tasti
060200940720     C                   MOVE      '0'           $FC(6)
060300940720     C                   MOVE      '0'           $FC(5)
060400940720     C                   MOVE      '0'           $FC(4)
0605009402243-   C                   ENDSL
060600940224     C*
0607009402242-   C                   ENDIF
060800940223     C*
0609009402241-   C                   ENDIF
061000940224     C*
061100940117     C                   ENDSR
061200940207     C/EJECT
061300940207     C************************************************************
061400940510     C* RIEMPIMENTO VIDEATA  D1
061500940207     C************************************************************
061600940207     C     RIED1         BEGSR
061700940207     C*
061800990827$025 C     RIMLUNYP3     IFEQ      *ON
061900990827$025 C                   MOVE      XSCON         RIMLUNYP3
062000990827$025 C                   ELSE
062100990827$025 C                   MOVE      XSCOFF        RIMLUNYP3
062200990827$025 C                   ENDIF
062300990827$025 C     RIMMARYP3     IFEQ      *ON
062400990827$025 C                   MOVE      XSCON         RIMMARYP3
062500990827$025 C                   ELSE
062600990827$025 C                   MOVE      XSCOFF        RIMMARYP3
062700990827$025 C                   ENDIF
062800990827$025 C     RIMMERYP3     IFEQ      *ON
062900990827$025 C                   MOVE      XSCON         RIMMERYP3
063000990827$025 C                   ELSE
063100990827$025 C                   MOVE      XSCOFF        RIMMERYP3
063200990827$025 C                   ENDIF
063300990827$025 C     RIMGIOYP3     IFEQ      *ON
063400990827$025 C                   MOVE      XSCON         RIMGIOYP3
063500990827$025 C                   ELSE
063600990827$025 C                   MOVE      XSCOFF        RIMGIOYP3
063700990827$025 C                   ENDIF
063800990827$025 C     RIMVENYP3     IFEQ      *ON
063900990827$025 C                   MOVE      XSCON         RIMVENYP3
064000990827$025 C                   ELSE
064100990827$025 C                   MOVE      XSCOFF        RIMVENYP3
064200990827$025 C                   ENDIF
064300990827$025 C
064400940207     C*
064500940207     C                   ENDSR
064600940309     C/EJECT
064700940309     C************************************************************
064800940309     C* GESTIONE F03 VIDEO D1
064900940309     C************************************************************
065000940309     C     F03D1         BEGSR
065100940309     C*
065200940309     C                   MOVE      *ON           $FINE
065300940315    >C                   MOVE      '1'           RET002
065400940325     C* fine programma
065500940325     C                   EXSR      ENDPGM
065600940325     C*
065700940325     C                   ENDSR
065800940309     C/EJECT
065900940309     C************************************************************
066000940309     C* GESTIONE F05 VIDEO D1
066100940309     C************************************************************
066200940309     C     F05D1         BEGSR
066300940309     C*
066400940309     C                   MOVE      *ON           $INZD1
066500940309     C*
066600940309     C                   ENDSR
066700940309     C/EJECT
066800940309     C************************************************************
066900940309     C* GESTIONE F08 VIDEO D1
067000940309     C************************************************************
067100940309     C     F08D1         BEGSR
067200940309     C*
067300940309     C                   MOVE      *ON           $FINE
067400940315    >C                   MOVE      '0'           RET002
067500940309     C*
067600940309     C                   ENDSR
067700940309     C/EJECT
067800040616     C************************************************************
067900040616     C* GESTIONE F14 VIDEO D1
068000040616     C************************************************************
068100040616     C     F14D1         BEGSR
068200040616     C*
068300040616     C*  CALCOLO DATA VALUTA
068400040616     C                   Call      'X17CLVALR'
068500040616     C                   Parm                    KPJBA
068600040616     C*
068700040616     C                   ENDSR
068800040616     C/EJECT
068900940309     C************************************************************
069000940309     C* GESTIONE F12 VIDEO D1
069100940309     C************************************************************
069200940309     C     F12D1         BEGSR
069300940309     C*
069400940309     C                   MOVE      *ON           $FINE
069500940315    >C                   MOVE      '2'           RET002
069600940309     C*
069700940309     C                   ENDSR
069800940309     C/EJECT
069900940309     C************************************************************
070000940309     C* GESTIONE F24 VIDEO D1
070100940309     C************************************************************
070200940309     C     F24D1         BEGSR
070300940309     C*
070400940506     C*
070500940506     C                   MOVEA     $FX1          $FX
070600940506     C                   MOVEA     $FC1          $FC
070700940506     C                   MOVEA     $FR1          $FR
070800940506     C                   Z-ADD     $ULKD1        $ULK
070900940506     C                   EXSR      RIEKEY
071000941017     C                   MOVEA     $FC           $FC1
071100940506     C                   Z-ADD     $ULK          $ULKD1            3 0
071200940506     C                   MOVEL     $KEY1         Z1KE1
071300940506     C                   MOVEL     $KEY2         Z1KE2
071400940614     C                   WRITE     Z1
071500940506     C*
071600940309     C                   ENDSR
071700940207     C/EJECT
071800940131     C************************************************************
071900940207     C* CONTROLLO VIDEATA
072000940131     C************************************************************
072100940127     C     CTRD1         BEGSR
072200940131     C*
072300940127     C                   SETOFF                                       99
072400940127     C* non imposto pos. del cursore
072500940127     C                   Z-ADD     0             H1RIGA
072600940127     C                   Z-ADD     0             H1COLO
072700940621     C*
072800940621     C* CTRL DATA
072900940621     C*
073000941109     C                   MOVE      DATA          WRKDSC           10
073100940621     C     D1DSC         IFNE      0
073200970214     C                   CALLB     'XDT4000'
073300941109     C                   PARM                    XDTAMG           10
073400940621     C                   PARM      D1DSC         XDTGMA            6 0
073500940621     C                   PARM      3             XDTSTA            1 0
073600940621     C     XDTSTA        IFNE      0
073700940621     C                   SETON                                        5599
073800940621     C                   ELSE
073900941108     C                   MOVE      XDTAMG        WRKDSC
074000940621     C                   ENDIF
074100940621     C                   ENDIF
074200940223     C*
074300940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
074400940223     C*
0745009403151   >C     OPZ002        IFNE      '04'
074600971110     C*
074700971110     C* controllo codice vuoto
074800050211     C                   SELECT
074900050211$017 C     D1COD         WHENEQ    *ALL' '
075000971110     C                   SETON                                        5099
075100050211$017 C     D1COD         WHENEQ    *ZERO
075200050211     c                   eval      d1dcod = 'Rimborso standard'
075300050211     C                   OTHER
075400040616     C     KEYcln        CHAIN     ancln01l                           51
075500040616     C                   if        *in51
075600040616     C                   SETON                                          99
075700040616     C                   Else
075800040616     c                   eval      d1dcod = CLNDESBREV
075900040616     C                   END
076000050211     C                   ENDSL
076100971110     C*
076200940614$009 C     RIMLUNYP3     IFEQ      *BLANK
076300940614$009 C                   SETON                                        6099
076400940614$009 C                   ENDIF
076500940614$009 C     RIMLUNYP3     IFNE      XSCON
076600940614$009 C     RIMLUNYP3     ANDNE     XSCOFF
076700940614$009 C                   SETON                                        6099
076800940614$009 C                   ENDIF
076900940614$009 C     RIMMARYP3     IFEQ      *BLANK
077000940614$009 C                   SETON                                        6199
077100940614$009 C                   ENDIF
077200940614$009 C     RIMMARYP3     IFNE      XSCON
077300940614$009 C     RIMMARYP3     ANDNE     XSCOFF
077400940614$009 C                   SETON                                        6199
077500940614$009 C                   ENDIF
077600940614$009 C     RIMMERYP3     IFEQ      *BLANK
077700940614$009 C                   SETON                                        6299
077800940614$009 C                   ENDIF
077900940614$009 C     RIMMERYP3     IFNE      XSCON
078000940614$009 C     RIMMERYP3     ANDNE     XSCOFF
078100940614$009 C                   SETON                                        6299
078200940614$009 C                   ENDIF
078300940614$009 C     RIMGIOYP3     IFEQ      *BLANK
078400940614$009 C                   SETON                                        6399
078500940614$009 C                   ENDIF
078600940614$009 C     RIMGIOYP3     IFNE      XSCON
078700940614$009 C     RIMGIOYP3     ANDNE     XSCOFF
078800940614$009 C                   SETON                                        6399
078900940614$009 C                   ENDIF
079000940614$009 C     RIMVENYP3     IFEQ      *BLANK
079100940614$009 C                   SETON                                        6499
079200940614$009 C                   ENDIF
079300940614$009 C     RIMVENYP3     IFNE      XSCON
079400940614$009 C     RIMVENYP3     ANDNE     XSCOFF
079500940614$009 C                   SETON                                        6499
079600940614$009 C                   ENDIF
079700940207     C*
079800040616     C* non possono essere tutti a "N"
079900040616     c                   if        RIMlunYP3 = xscoff   and
080000040616     c                             RIMmarYP3 = xscoff   and
080100040616     c                             RIMmerYP3 = xscoff   and
080200040616     c                             RIMgioYP3 = xscoff   and
080300040616     c                             RIMvenYP3 = xscoff
080400040616$009 C                   SETON                                        6599
080500040616$009 C                   ENDIF
080600040616     C*
080700040616     C* Se almeno un giorno valuta è impostato occorre avere un Metodo
080800040616     C*  di calcolo
080900040616     c                   if        GVALUNYP3 > 0   or
081000040616     c                             GVAMARYP3 > 0   or
081100040616     c                             GVAMERYP3 > 0   or
081200040616     c                             GVAGIOYP3 > 0   or
081300040616     c                             GVAVENYP3 > 0
081400040616     c                   if        CALDTVYP3 = *blank
081500040616$009 C                   SETON                                        6699
081600040616$009 C                   ENDIF
081700040616     c                   if        CALENDYP3 = *blank
081800040616$009 C                   SETON                                        6799
081900040616$009 C                   ENDIF
082000040616$009 C                   ENDIF
082100040616  "  C*
082200040616  "  C* ctrl notifica
082300040616  "  C                   MOVEL     *BLANK        D1DCALDTV
082400040616  "  C                   MOVE      '1'           X07RIC
082500040616  "  C                   MOVEL     *on           X07ALL
082600040616  "  C                   MOVEL     'ANBCO000'    X07TRC
082700040616  "  C                   MOVEL     'BCOCALDTV'   X07CAM
082800040616  "  C                   MOVEL     CALDTVYP3     X07VAL
082900040616  "  C                   CALLb     'X07VALR'
083000040616  "  C                   PARM                    X07DS
083100040616  "  C     X07ERR        IFEQ      '0'
083200040616  "  C                   MOVEL     X07DES        D1DCALDTV
083300040616  "  C                   ELSE
083400040616  "  C                   SETON                                        6699
083500040616  "  C                   ENDIF
083600040616A0970C*
083700040616     C*
083800040616      * CTR CALENDARIO
083900040616     C                   MOVEL     *BLANK        D1dCalen
084000040616    >C     Calendyp3     IFNE      *BLANK
084100040616     C                   MOVE      '1'           XTBRIC
084200040616     C                   MOVE      xscSOC        XTBAZI
084300040616     C                   MOVE      *ZERO         XTBKEY
084400040616     C                   MOVE      'G71'         XTBCOD
084500040616     C                   MOVE      calendyp3     XTBKEY
084600040616     C                   CALLB     'XATB'
084700040616     C                   PARM                    XATBDS
084800040616   B3C     XTBERR        IFEQ      '0'
084900040616     C                   MOVEL     XTBUNI        angg71ds
085000040616     C                   MOVEL     DESG71        D1dcalen
085100040616   X3C                   ELSE
085200040616     C                   SETON                                        6799
085300040616   E3C                   ENDIF
085400040616   E3C                   ENDIF
085500040616     C*
085600940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
085700940315    >C     OPZ002        IFEQ      '01'
085800940315    >C     OPZ002        OREQ      '03'
085900940315    >C*-------------------------------------------------------------
086000940315    >C* impostare i campi chiave e verificare l'esistenza del record
086100940315    >C*-------------------------------------------------------------
086200971110$007 C                   MOVE      'YP3'         XTBCOD
086300040616     C                   MOVE      *blank        XTBKEY
086400040616    >C                   MOVEL     D1COD         XTBKEY
086500970512    >C                   MOVE      D1UNITA       XTBUNT
086600940614     C                   MOVE      '0'           XTBALC
086700940614     C                   MOVE      '1'           XTBRIC
086800970214    >C                   CALLB     'XATB'
086900940614     C                   PARM                    XATBDS
087000940614     C     XTBERR        IFEQ      '0'
087100940223     C                   SETON                                        5999
087200940223     C                   ENDIF
087300940620     C* Controllo se compreso nei range indice
087400940620     C     XTBERR        IFEQ      '3'
087500940620    >C                   SETON                                        5899
087600940620     C                   END
087700940620     C*
087800940620     C* REPERISCO AUTORIZZAZIONI UTENTE
087900940620     C                   CLEAR                   XCHATB
088000941118     C                   MOVE      XSCSOC        XCTSOC
088100941118     C                   MOVE      XSCDUN        XCTUNI
088200940620     C                   MOVEL     KNMUS         XCTPRF
088300940620     C                   MOVE      XTBCOD        XCTCOD
088400940620     C                   CALL      'XCHKATB'
088500940620     C                   PARM                    XCHATB
088600941111     C                   MOVEL     XCTDDS        AUTDS
088700941027     C     XCTRTN        IFEQ      1
088800941027     C                   MOVEL     'N'           MANGRU
088900941027     C                   ENDIF
089000940620     C* Non ammessa immissione di gruppo
089100940620     C     XTBGRU        IFEQ      'G'
089200940620     C     MANGRU        ANDEQ     'N'
089300940620     C                   SETON                                        5799
089400940620     C                   END
089500940620     C* Non ammessa immissione di azienda
089600940620     C     XTBGRU        IFEQ      'A'
089700940620     C     MANAZI        ANDEQ     'N'
089800940620     C                   SETON                                        5699
089900940620     C                   END
090000940712     C* codice riservato non accessibile
090100941006     C     XTBRIS        IFGT      ABIRIS
090200941005    >C     OPZ002        ANDNE     '01'
090300941005    >C     OPZ002        ANDNE     '03'
090400940712     C                   SETON                                        5499
090500940712     C                   END
090600940224     C                   ENDIF
090700940224     C*
0908009402241-   C                   END
090900940131     C*
091000940302     C     *IN99         IFEQ      *ON
091100940510     C     $GEST         ANDEQ     'D1'
091200960228     C     OPZ002        ANDNE     '07'
091300940117     C                   MOVEA     *IN           WIN
091400940128     C                   MOVE      *ALL'0'       IN5098           49
091500940128     C                   MOVEA     IN5098        *IN(50)
091600940614     C                   WRITE     D1
091700940117     C                   MOVEA     WIN           *IN
091800940302     C                   ENDIF
091900940127     C*
092000940117     C                   ENDSR
092100960305     C/EJECT
092200960305     C************************************************************
092300960305     C* CONTROLLO VIDEATA D2
092400960305     C************************************************************
092500960305     C     CTRD2         BEGSR
092600960305     C*
092700960305     C                   ENDSR
092800940207     C/EJECT
092900940224     C************************************************************
093000940315    >C* AGGIORNAMENTO ANAGRAFICA
093100940224     C************************************************************
093200940315    >C     AGGANA        BEGSR
093300971110     C*
093400971110$007 C                   MOVE      'YP3'         XTBCOD
093500040617     C                   MOVE      *blank        XTBKEY
093600040616     C                   MOVEL     D1COD         XTBKEY
093700970512     C                   MOVE      D1UNITA       XTBUNT
093800940207     C*
093900940207     C                   SELECT
094000940315    >C     OPZ002        WHENEQ    '01'
094100940315    >C     OPZ002        OREQ      '03'
094200940315    >C                   EXSR      RIEANA
094300950609     C                   MOVE      XSCCUT        XTBUTE
094400940614     C                   MOVE      '5'           XTBRIC
094500970214     C                   CALLB     'XATB'
094600940614    >C                   PARM                    XATBDS
094700940318     C* se rcd non scrivibile attivo errore
094800940615     C     XTBERR        IFEQ      '1'
094900940318    >C                   SETON                                        5999
095000940318     C                   END
095100940315    >C     OPZ002        WHENEQ    '02'
095200940621     C                   MOVE      '1'           XTBRIC
095300940621     C                   MOVE      '1'           XTBALC
095400970214     C                   CALLB     'XATB'
095500940621     C                   PARM                    XATBDS
095600940622    >C                   EXSR      RIEANA
095700950609     C                   MOVE      XSCCUT        XTBUTE
095800940614     C                   MOVE      '4'           XTBRIC
095900970214     C                   CALLB     'XATB'
096000940614    >C                   PARM                    XATBDS
096100940315    >C     OPZ002        WHENEQ    '04'
096200940621     C                   MOVE      '1'           XTBRIC
096300940621     C                   MOVE      '1'           XTBALC
096400970214     C                   CALLB     'XATB'
096500940621     C                   PARM                    XATBDS
096600940614     C                   MOVE      '6'           XTBRIC
096700970214     C                   CALLB     'XATB'
096800940614    >C                   PARM                    XATBDS
096900940621     C* annullo traduzioni
097000940621     C     XTBERR        IFEQ      '0'
097100940621     C                   EXSR      ANNTBT
097200940621     C                   END
097300940621     C*
097400940207     C                   ENDSL
097500940203     C*
097600940203     C                   ENDSR
097700940224     C/EJECT
097800940224     C************************************************************
097900940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
098000940224     C************************************************************
098100940616     C     RIEANA        BEGSR
098200940616     C                   CLEAR                   XTBUNI
098300940621     C                   MOVE      WRKDSC        XTBDSC
098400940712     C                   MOVE      D1RIS         XTBRIS
098500981209$026 C     RIMLUNYP3     IFEQ      XSCON
098600981209$026 C                   MOVE      *ON           RIMLUNYP3
098700981209$026 C                   ELSE
098800981209$026 C                   MOVE      *OFF          RIMLUNYP3
098900981209$026 C                   ENDIF
099000981209$026 C     RIMMARYP3     IFEQ      XSCON
099100981209$026 C                   MOVE      *ON           RIMMARYP3
099200981209$026 C                   ELSE
099300981209$026 C                   MOVE      *OFF          RIMMARYP3
099400981209$026 C                   ENDIF
099500981209$026 C     RIMMERYP3     IFEQ      XSCON
099600981209$026 C                   MOVE      *ON           RIMMERYP3
099700981209$026 C                   ELSE
099800981209$026 C                   MOVE      *OFF          RIMMERYP3
099900981209$026 C                   ENDIF
100000981209$026 C     RIMGIOYP3     IFEQ      XSCON
100100981209$026 C                   MOVE      *ON           RIMGIOYP3
100200981209$026 C                   ELSE
100300981209$026 C                   MOVE      *OFF          RIMGIOYP3
100400981209$026 C                   ENDIF
100500981209$026 C     RIMVENYP3     IFEQ      XSCON
100600981209$026 C                   MOVE      *ON           RIMVENYP3
100700981209$026 C                   ELSE
100800981209$026 C                   MOVE      *OFF          RIMVENYP3
100900981209$026 C                   ENDIF
101000981209$026 C
101100940616     C                   MOVEL     EXTDS         XTBUNI
101200940207     C                   ENDSR
101300940224     C/EJECT
101400940224     C************************************************************
101500940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
101600940224     C************************************************************
101700940224     C     GESAGG        BEGSR
101800940224     C*
101900940224     C* segnalo al pgm chiamante l'avvenuta conferma
102000940315    >C                   MOVE      *ON           OPR002
102100940224     C* segnalo al pgm chiamante l'aver premuto l'F6
102200940315    >C                   MOVE      '0'           RET002
102300940224     C*
1024009402241    C                   SELECT
102500940224     C* nel caso di immissione, ripristino la videata iniziale
102600940315    >C     OPZ002        WHENEQ    '01'
102700940224     C                   MOVE      *ON           $INZD1
102800940224     C* altrimenti ritorno al pgm chiamante
102900940224     C                   OTHER
103000940224     C                   MOVE      *ON           $FINE
1031009402241-   C                   ENDSL
103200940224     C*
103300940224     C                   ENDSR
103400940621     C/EJECT
103500940621     C************************************************************
103600940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
103700940621     C************************************************************
103800940621    >C     ANNTBT        BEGSR
103900940621     C                   MOVE      XTBAZI        TBTAZI
104000940621     C                   MOVE      XTBCOD        TBTCOD
104100940621     C                   MOVE      XTBKEY        TBTKEY
104200940621     C     KTBT          SETLL     ANTBT01L
104300940621     C                   DO        *HIVAL
104400940621     C     KTBT          DELETE    ANTBT01L                           21
104500940621     C  N21              END
104600940621     C*
104700940621     C                   ENDSR
104800940207     C/EJECT
104900940131     C************************************************************
105000940131     C* OPERAZIONI INIZIALI
105100940131     C************************************************************
105200940131     C     *INZSR        BEGSR
105300940131     C*
105400940127     C* Reperimento parametri
105500940127     C*
105600940117     C     *ENTRY        PLIST
105700940117     C                   PARM                    KPJBA
105800940712     C*
105900940712     C* Reperimento Codice utente
106000940712     C*
106100940223     C*
106200940506     C*
106300940506     C* Reperimento tasti di funzione
106400940506     C*
106500940506     C                   EXSR      REPKEY
106600940621     C*
106700940621     C* KLIST
106800940621     C*
106900940621     C     KTBT          KLIST
107000940621     C                   KFLD                    TBTAZI
107100940621     C                   KFLD                    TBTCOD
107200940621     C                   KFLD                    TBTKEY
107300040616     C*
107400040616     C     KEYCLN        KLIST
107500040616     C                   KFLD                    xscSOC
107600040616     C                   KFLD                    d1cod
107700010406X0113C*
107800010406  "  C* reperisco le dimensioni della variabile del titolo
107900010406X0113C                   EVAL      LENTIT = %Size(T1Tit)
108000940506     C*
108100940223     C                   ENDSR
108200941103     C/EJECT
108300941103      ************************************************************
108400941103      * Reperimento dati società
108500941103      ************************************************************
108600941103     C     REPSOC        BEGSR
108700941103      *
108800970214     C                   CALLB     'XSOC'
108900941103     C                   PARM                    TIPXSC            6
109000941103     C                   PARM                    SOCXSC            3
109100941103     C                   PARM                    CDSXSC            9 0
109200941103     C                   PARM                    MODXSC            3
109300941103     C                   PARM      *BLANKS       RTNXSC            1
109400941103     C                   PARM                    XSOCDS
109500941103     C                   PARM                    KPJBA
109600941103      *
109700941103     C                   ENDSR
109800940223     C/EJECT
109900940506     C************************************************************
110000940506     C* REPERIMENTO TASTI DI FUNZIONE
110100940506     C************************************************************
110200940506     C     REPKEY        BEGSR
110300940506     C                   MOVEA     $FM1          $FM
110400940506     C                   EXSR      REPKKK
110500940506     C                   MOVEA     $FX           $FX1
110600940614     C                   ENDSR
110700940506     C/EJECT
110800940506     C************************************************************
110900940506     C* REPERIMENTO TASTI DI FUNZIONE
111000940506     C************************************************************
111100940506     C     REPKKK        BEGSR
111200940506     C*
111300940506     C                   CLEAR                   $FX
111400940506     C     1             DO        24            X
111500940506     C*
111600940506     C     $FM(X)        IFNE      *BLANK
111700940506     C*
111800940506     C                   MOVEL     $FM(X)        IDMSG
111900970214     C                   CALLB     'XRTVM'
112000940506     C                   PARM                    IDMSG            27
112100940506     C                   PARM                    TXTMSG          100
112200940506     C                   PARM                    ERRMSG            1
112300940506     C*
112400940506     C     ERRMSG        IFNE      *ON
112500940506     C                   MOVEL     TXTMSG        $FX(X)
112600940506     C                   ELSE
112700940506     C                   MOVEL     *ALL' '       $FX(X)
112800940506     C                   END
112900940506     C*
113000040616     C* Eccezione F14 -> Pgm calcolo giorni di Valuta
113100040616     c                   if        x = 14
113200040616     C                   MOVEL     F14x17pgm     $FX(X)
113300040616     c                   end
113400040616     C*
113500940506     C                   END
113600940506     C*
113700940506     C                   END
113800940506     C*
113900940506     C                   ENDSR
114000940509     C/EJECT
114100940223     C************************************************************
114200940223     C* INIZIALIZZAZIONE VARIABILI
114300940223     C************************************************************
114400940223     C     INZVAR        BEGSR
114500951124     C*
114600951124     C* Chiamata XSOC
114700951124     C*
114800951124     C                   MOVEL     'SOC001'      TIPXSC
114900951124     C                   EXSR      REPSOC
115000951124     C     RTNXSC        IFNE      '1'
115100951124     C                   MOVEL     XSOCDS        SOC001
115200951124     C                   ELSE
115300951124     C                   MOVE      *ON           $FINE
115400951124     C* fine programma
115500951124     C                   EXSR      ENDPGM
115600951124     C                   ENDIF
115700940224     C*
115800940224     C* Dati da PGM guida
115900940224     C*
116000940315    >C                   MOVEL     KPJBU         D002DS
116100940128     C*
116200940223     C* Variabili per gestione videate
116300940128     C*
116400940223     C                   MOVE      'D1'          $GEST
116500940223     C                   MOVE      *OFF          $FINE
116600940223     C                   MOVE      *ON           $INZD1
116700940224     C                   MOVE      *BLANK        $LASTG
116800940224     C                   MOVE      *BLANK        $LASTV
116900940506     C                   Z-ADD     0             $ULKD1            3 0
117000940127     C*
117100940223     C* Variabili appoggio
117200940127     C*
117300940224     C                   Z-ADD     0             CURR
117400940224     C                   Z-ADD     0             CURC
117500940207     C*
117600940207     C* Valorizzazione campi univoci testate
117700940207     C*
117800940614     C                   CLEAR                   T1
117900940614     C                   MOVEL     KNSIF         NOMSIF
118000941109     C                   MOVEL     XSCDSI        NOMDIT
118100941027     C                   MOVEL     DSPGM         NOMPGM
118200940224     C                   Z-ADD     1             X
118300940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
118400940224     C     *IN21         IFEQ      *ON
118500940224     C                   MOVEL     $ID(X)        IDMSG
118600970214     C                   CALLB     'XRTVM'
118700940224     C                   PARM                    IDMSG            27
118800940224     C                   PARM                    TXTMSG          100
118900940224     C                   PARM                    ERRMSG            1
119000940224     C                   PARM      *ON           CTRMSG            1
119100940224     C                   PARM      30            LENMSG            3 0
119200940224     C     ERRMSG        IFNE      *ON
119300940224     C                   MOVEL     TXTMSG        T1OPE
119400940224     C                   ELSE
119500940224     C                   MOVEL     *ALL'?'       T1OPE
119600940224     C                   ENDIF
119700940224     C                   END
119800940224     C*
119900940224     C* Inizializzazione/Decodifica prima videata
120000940224     C*
120100940224     C                   EXSR      INZD1
120200940224     C                   MOVE      *OFF          $INZD1
120300960228     C* Se opzione = '07' richiesti solo controlli
120400960228     C     OPZ002        IFEQ      '07'
120500960228     C                   MOVEL     *IN99         ERR002
120600960228     C                   MOVEL     '0'           RET002
120700960228     C                   MOVEL     *ON           $FINE
120800960228     C                   ENDIF
120900940127     C*
121000940117     C                   ENDSR
121100940128     C************************************************************
121200940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
121300940223     C************************************************************
121400940223     C     DEFCAM        BEGSR
121500940223     C*
121600940223     C* klist
121700940207     C*
121800940223     C*
121900940223     C* Variabili per gestione videate
122000940223     C*
122100940223     C                   MOVE      *BLANK        $GEST             2
122200940223     C                   MOVE      *BLANK        $FINE             1
122300940223     C                   MOVE      *BLANK        $INZD1            1
122400940223     C                   MOVE      *BLANK        $LASTG            2
122500940223     C                   MOVE      *BLANK        $LASTV            2
122600940223     C*
122700940223     C* Variabili appoggio
122800940223     C*
122900940223     C                   Z-ADD     0             CURR              2 0
123000940223     C                   Z-ADD     0             CURC              2 0
123100940223     C                   MOVE      *ZEROS        WIN              99
123200940224     C                   MOVE      *BLANK        WTBERR            1
123300940223     C*
123400940223     C* Indici
123500940223     C*
123600940223     C                   Z-ADD     0             X                 3 0
123700940223     C                   Z-ADD     0             Y                 3 0
123800940127     C*
123900940117     C                   ENDSR
124000940128     C************************************************************
124100940509     C/EJECT
124200940509     C************************************************************
124300940509     C* CONTROLLO ABILITAZIONE TASTI
124400940509     C************************************************************
124500940509     C     CTRKEY        BEGSR
124600940509     C                   SELECT
124700940509     C     $TASTO        WHENEQ    F01
124800940720     C     $FC(1)        IFEQ      '0'
124900940509     C                   SETON                                        99
125000940509     C                   ENDIF
125100940509     C     $TASTO        WHENEQ    F02
125200940720     C     $FC(2)        IFEQ      '0'
125300940509     C                   SETON                                        99
125400940509     C                   ENDIF
125500940509     C     $TASTO        WHENEQ    F03
125600940720     C     $FC(3)        IFEQ      '0'
125700940509     C                   SETON                                        99
125800940509     C                   ENDIF
125900940509     C     $TASTO        WHENEQ    F04
126000940720     C     $FC(4)        IFEQ      '0'
126100940509     C                   SETON                                        99
126200940509     C                   ENDIF
126300940509     C     $TASTO        WHENEQ    F05
126400940720     C     $FC(5)        IFEQ      '0'
126500940509     C                   SETON                                        99
126600940509     C                   ENDIF
126700940509     C     $TASTO        WHENEQ    F06
126800940720     C     $FC(6)        IFEQ      '0'
126900940509     C                   SETON                                        99
127000940509     C                   ENDIF
127100940509     C     $TASTO        WHENEQ    F07
127200940720     C     $FC(7)        IFEQ      '0'
127300940509     C                   SETON                                        99
127400940509     C                   ENDIF
127500940509     C     $TASTO        WHENEQ    F08
127600940720     C     $FC(8)        IFEQ      '0'
127700940509     C                   SETON                                        99
127800940509     C                   ENDIF
127900940509     C     $TASTO        WHENEQ    F09
128000940720     C     $FC(09)       IFEQ      '0'
128100940509     C                   SETON                                        99
128200940509     C                   ENDIF
128300940509     C     $TASTO        WHENEQ    F10
128400940720     C     $FC(10)       IFEQ      '0'
128500940509     C                   SETON                                        99
128600940509     C                   ENDIF
128700940509     C     $TASTO        WHENEQ    F11
128800940720     C     $FC(11)       IFEQ      '0'
128900940509     C                   SETON                                        99
129000940509     C                   ENDIF
129100940509     C     $TASTO        WHENEQ    F12
129200940720     C     $FC(12)       IFEQ      '0'
129300940509     C                   SETON                                        99
129400940509     C                   ENDIF
129500940509     C     $TASTO        WHENEQ    F13
129600940720     C     $FC(13)       IFEQ      '0'
129700940509     C                   SETON                                        99
129800940509     C                   ENDIF
129900940509     C     $TASTO        WHENEQ    F14
130000940720     C     $FC(14)       IFEQ      '0'
130100940509     C                   SETON                                        99
130200940509     C                   ENDIF
130300940509     C     $TASTO        WHENEQ    F15
130400940720     C     $FC(15)       IFEQ      '0'
130500940509     C                   SETON                                        99
130600940509     C                   ENDIF
130700940509     C     $TASTO        WHENEQ    F16
130800940720     C     $FC(16)       IFEQ      '0'
130900940509     C                   SETON                                        99
131000940509     C                   ENDIF
131100940509     C     $TASTO        WHENEQ    F17
131200940720     C     $FC(17)       IFEQ      '0'
131300940509     C                   SETON                                        99
131400940509     C                   ENDIF
131500940509     C     $TASTO        WHENEQ    F18
131600940720     C     $FC(18)       IFEQ      '0'
131700940509     C                   SETON                                        99
131800940509     C                   ENDIF
131900940509     C     $TASTO        WHENEQ    F19
132000940720     C     $FC(19)       IFEQ      '0'
132100940509     C                   SETON                                        99
132200940509     C                   ENDIF
132300940509     C     $TASTO        WHENEQ    F20
132400940720     C     $FC(20)       IFEQ      '0'
132500940509     C                   SETON                                        99
132600940509     C                   ENDIF
132700940509     C     $TASTO        WHENEQ    F21
132800940720     C     $FC(21)       IFEQ      '0'
132900940509     C                   SETON                                        99
133000940509     C                   ENDIF
133100940509     C     $TASTO        WHENEQ    F22
133200940720     C     $FC(22)       IFEQ      '0'
133300940509     C                   SETON                                        99
133400940509     C                   ENDIF
133500940509     C     $TASTO        WHENEQ    F23
133600940720     C     $FC(23)       IFEQ      '0'
133700940509     C                   SETON                                        99
133800940509     C                   ENDIF
133900940509     C     $TASTO        WHENEQ    F24
134000940720     C     $FC(24)       IFEQ      '0'
134100940509     C                   SETON                                        99
134200940509     C                   ENDIF
134300940509     C                   ENDSL
134400940509     C                   ENDSR
134500940509     C/EJECT
134600940509     C************************************************************
134700940509     C* RIEMPIMENTO TASTI FUNZIONE
134800940509     C************************************************************
134900940509     C     RIEKEY        BEGSR
135000940509     C*
135100940509     C* Input :
135200940509     C* - $FX = schiera con tutti i testi disponibili
135300940509     C* - $FC = schiere con i flag di validità associati ai testi
135400940509     C* - $FR = schiere con i flag di REVERSE IMAGE
135500940509     C* - $ULK = ultimo testo caricato
135600940509     C* Output :
135700940509     C* - $ULK
135800940509     c* - $KEY1= 1° riga di tasti funzione
135900940509     c* - $KEY2= 2° riga di tasti funzione
136000940509     C*
136100970214     C                   CALLB     'XKEYMSG'
136200940509     C* input
136300940509     C                   PARM                    $FX
136400940509     C                   PARM                    $FC
136500940509     C                   PARM                    $FR
136600940509     C                   PARM                    $ULK              3 0
136700940509     C* output
136800940509     C                   PARM                    $KEY1            79
136900940509     C                   PARM                    $KEY2            79
137000940509     C*
137100940509     C                   ENDSR
137200940615**
13730094022401PROMSG    *LIBL     COS0001    immissione
13740094022402PROMSG    *LIBL     COS0002    modifica
13750094022403PROMSG    *LIBL     COS0003    copia
13760094022404PROMSG    *LIBL     COS0004    annullamento
13770094022405PROMSG    *LIBL     COS0005    visualizzazione
137800940509** TASTI DI FUNZIONE  D1
137900940509PROMSG    *LIBL     KEY0001  01
138000940509PROMSG    *LIBL     KEY0002  02
138100940509PROMSG    *LIBL     KEY0003  03
138200940509PROMSG    *LIBL     KEY0004  04
138300940509PROMSG    *LIBL     KEY0005  05
138400940509PROMSG    *LIBL     KEY0006  06
138500940509PROMSG    *LIBL     KEY0007  07
138600940509PROMSG    *LIBL     KEY0008  08
138700940509PROMSG    *LIBL     KEY0009  09
138800940509PROMSG    *LIBL     KEY0010  10
138900940509PROMSG    *LIBL     KEY0011  11
139000940509PROMSG    *LIBL     KEY0012  12
139100940509PROMSG    *LIBL     KEY0013  13
139200940509PROMSG    *LIBL     KEY0014  14
139300940509PROMSG    *LIBL     KEY0015  15
139400940509PROMSG    *LIBL     KEY0016  16
139500940509PROMSG    *LIBL     KEY0017  17
139600940509PROMSG    *LIBL     KEY0018  18
139700940509PROMSG    *LIBL     KEY0019  19
139800940509PROMSG    *LIBL     KEY0020  20
139900940509PROMSG    *LIBL     KEY0021  21
140000940509PROMSG    *LIBL     KEY0022  22
140100940509PROMSG    *LIBL     KEY0023  23
140200940509PROMSG    *LIBL     KEY0024  24
140300940509**  ABILITAZIONE D1
1404000406160N  01
1405009407200N  02
1406009407201N  03
1407009407201N  04
1408009407201N  05
1409009407201N  06
1410009407201N  07
1411009407201N  08
1412009407200N  09
1413000406160N  10
1414009407200N  11
1415009407201N  12
1416009407200N  13
1417000406161N  14
1418009407200N  15
1419009407200N  16
1420009407200N  17
1421000406160N  18
1422009407200N  19
1423000406160N  20
1424009407200N  21
1425009407200N  22
1426009407200N  23
1427009407200N  24
