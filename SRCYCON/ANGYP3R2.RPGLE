000100050429     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
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
004700050429     D Green           C                   X'20'
004800050429     D Blue            C                   X'3A'
004900940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
005000940224     D $ID             S             27    DIM(5) ALT($OZ)
005100940506     D* Schiere per la gestione dei tasti di funzione
005200940506     D $FM             S             27    DIM(24)
005300940506     D $FX             S             50    DIM(24)
005400940506     D $FC             S              1    DIM(24)
005500940506     D $FR             S              1    DIM(24)
005600940506     D* Video D1
005700940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005800940506     D $FX1            S             50    DIM(24) ALT($FM1)
005900940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006000940506     D $FR1            S              1    DIM(24) ALT($FC1)
006100940615     D* campi in lingua
006200940622$010 D*LIN             S             06    DIM(0011) CTDATA PERRCD(001)
006300940622$010 D*LIND            S             50    DIM(0011) ALT(LIN)
006400940614$003 D EXTDS         E DS                  EXTNAME(ANGYP3DS) INZ
006500940127     D* Passaggio Parametri
006600940127     D KPJBA         E DS
006700040616     D* ds input ANA201DS
006800040616     D ANA201DS      E DS
006900040616     D* ds input X17CLVALDS
007000040616     D X17CLVALDS    E DS                  Inz
007100040616  "  D* Controlli validità legende
007200040616A0970D X07DS         E DS                  EXTNAME(X07VALDS)
007300040616     D* DS esterna contabilità
007400040616     D ANGG71DS      E DS
007500040616     D* Ricerca legende
007600040616     D ANA964DS      E DS
007700040616     D* Parametri per ricerca codice ANATB00F
007800040616     D XTABDS        E DS                  INZ
007900941103     D*-------------
008000941103      * DS Società
008100941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
008200960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
008300960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
008400941103     D*-------------
008500941103      * DS Interna per dati utente
008600941103     D XSOCDS          DS          1000
008700941103     D*-------------
008800940616     D XATBDS        E DS                  INZ
008900940318     D*-------------
009000940318     D* DS Esterna gestione messaggi per COMIT
009100940616     D XMSGDS        E DS                  INZ
009200940318     D  STS          E                     EXTFLD(MSGSTS)
009300940318     D                                     DIM(333)
009400940318     D  JBA          E                     EXTFLD(MSGJBA)
009500940318     D                                     DIM(502)
009600940203     D*-------------
009700940620     D* DS Esterna autorizzazioni tabelle
009800940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
009900940620     D*
010000940620     D* DS  restituzione autorizzazioni
010100940620     D* IND=indice
010200940620     D* GRU=man. di gruppo
010300940620     D* AZI=man. azienda
010400940712     D* ABI=abilit. ai riservati
010500941111     D AUTDS           DS
010600940620     D  MANIND                 1      1
010700940620     D  MANGRU                 2      2
010800940620     D  MANAZI                 3      3
010900940712     D  ABIRIS                 4      4
011000940620     D*-------------
011100940207     D* opzione richiesta
011200940614    >D D002DS          DS
011300940614     D  OPZ002                 1      2
011400940614     D* 0 = ritorno normale
011500940614     D* 1 = F3
011600940614     D* 2 = F12
011700940614     D  RET002                 3      3
011800940614     D* operazione eseguite dal pgm chiamato :
011900940614     D* 0 = nessuna operazione
012000940614     D* 1 = eseguito aggiornamento
012100940614     D  OPR002                 4      4
012200940614     D* funzione non eseguibile per errore :
012300940614     D* 0 = nessun errore
012400940614     D* 1 = funzione richiamata chiusa in errore
012500940614     D* Aggiungere eventuali altri codici di errore
012600940614     D  ERR002                 5      5
012700940614     D* Azienda
012800940614     D  AZI002                 6      8
012900940614     D* Lingua
013000940614     D  LIN002                 9      9
013100940614     D* Codice scelto
013200940614     D  KEY002                10     24
013300940620     D* Autorizzazione
013400940620     D  AUT002                25     25
013500970509     D* Unita
013600970509     D  UNI002                26     33
013700940614     D*-------------
013800940201     D*-------------
013900940201     D DSFMT           DS
014000940506     D  $TASTO               369    369
014100940201     D  NRG                  370    370
014200940201     D  NCL                  371    371
014300940201     D*-------------
014400940201     D* posizione cursore
014500940201     D CURSOR          DS
014600940223     D  RIRI                   1      2B 0 INZ
014700940201     D  R$$                    2      2
014800940223     D  COCO                   3      4B 0 INZ
014900940201     D  C$$                    4      4
015000940207     D*-------------
015100940207     D* Reperimento nome PGM
015200940207     D STATUS         SDS           333
015300940207     D  DSPGM            *PROC
015400010406X0113D*-------------
015500010406  "  D* Reperimento titolo tabella
015600010618  "  D  WRKTIT         S             35A
015700010406  "  D  WRKTBCOD       S                   LIKE(XTBCOD)
015800010406  "  D  LENTIT         S              3  0
015900010406X0113D  I              S              3  0
016000940225     D*-------------
016100940225     D* COSTANTI
016200940225     D*-------------
016300940506     D*
016400940506     D* Tasti di funzione
016500940506     D F01             C                   CONST(X'31')
016600940506     D F02             C                   CONST(X'32')
016700940506     D F03             C                   CONST(X'33')
016800940506     D F04             C                   CONST(X'34')
016900940506     D F05             C                   CONST(X'35')
017000940506     D F06             C                   CONST(X'36')
017100940506     D F07             C                   CONST(X'37')
017200940506     D F08             C                   CONST(X'38')
017300940506     D F09             C                   CONST(X'39')
017400940506     D F10             C                   CONST(X'3A')
017500940506     D F11             C                   CONST(X'3B')
017600940506     D F12             C                   CONST(X'3C')
017700940506     D F13             C                   CONST(X'B1')
017800940506     D F14             C                   CONST(X'B2')
017900940506     D F15             C                   CONST(X'B3')
018000940506     D F16             C                   CONST(X'B4')
018100940506     D F17             C                   CONST(X'B5')
018200940506     D F18             C                   CONST(X'B6')
018300940506     D F19             C                   CONST(X'B7')
018400940506     D F20             C                   CONST(X'B8')
018500940506     D F21             C                   CONST(X'B9')
018600940506     D F22             C                   CONST(X'BA')
018700940506     D F23             C                   CONST(X'BB')
018800940506     D F24             C                   CONST(X'BC')
018900940506     D ENTER           C                   CONST(X'F1')
019000940506     D ROLDWN          C                   CONST(X'F4')
019100940506     D ROLLUP          C                   CONST(X'F5')
019200940506     D*-------------
019300940506     D*
019400941108     D DATA            C                   CONST('0001-01-01')
019500040616     D F14x17pgm       C                   CONST('F14=Simula calcolo gg valuta')
019600940128     I*----------------------------------------------------*
019700940615     ID1
019800940614$005 I              D1RIMLUN                    RIMLUNYP3
019900940614$005 I              D1RIMMAR                    RIMMARYP3
020000940614$005 I              D1RIMMER                    RIMMERYP3
020100940614$005 I              D1RIMGIO                    RIMGIOYP3
020200940614$005 I              D1RIMVEN                    RIMVENYP3
020300940614$005 I              D1GVALUN                    GVALUNYP3
020400940614$005 I              D1GVAMAR                    GVAMARYP3
020500940614$005 I              D1GVAMER                    GVAMERYP3
020600940614$005 I              D1GVAGIO                    GVAGIOYP3
020700940614$005 I              D1GVAVEN                    GVAVENYP3
020800940614$005 I              D1CALDTV                    CALDTVYP3
020900940614$005 I              D1CALEND                    CALENDYP3
021000940127     I*
021100940127     C*----------------------------------------------------*
021200940127     C*                MAIN LINE PROGRAM
021300940127     C*----------------------------------------------------*
021400940127     C*
021500940223     C* inizializzazione variabili
021600940223     C                   EXSR      INZVAR
021700940223     C*
021800940223     C     $FINE         DOWEQ     *OFF
021900940202     C     $GEST         CASEQ     'D1'          GESD1
022000940117     C                   END
022100940117     C                   END
022200940201     C*
022300940325     C* fine programma
022400940325     C                   EXSR      ENDPGM
022500940325     C/EJECT
022600940325     C************************************************************
022700940325     C* RICERCHE
022800940325     C************************************************************
022900940325     C     ENDPGM        BEGSR
023000940325     C*
023100940614     C* disallocazione rcd nel caso di
023200971110$007 C                   MOVE      'YP3'         XTBCOD
023300940614    >C                   MOVE      '7'           XTBRIC
023400970214     C                   CALLB     'XATB'
023500940614     C                   PARM                    XATBDS
023600940325     C*
023700940325     C* passaggio dati al pgm chiamante
023800940715     C                   MOVE      *BLANKS       KPJBU
023900940325    >C                   MOVEL     D002DS        KPJBU
024000940325     C*
024100940325     C                   SETON                                            RT
024200940325     C*
024300940325     C                   ENDSR
024400940325     C/EJECT
024500940201     C************************************************************
024600940201     C* RICERCHE
024700940201     C************************************************************
024800940127     C     SEARCH        BEGSR
024900940127     C*
025000940127     C* determino Riga/Colonna del cursore
025100940127     C                   MOVE      NRG           R$$
025200940127     C                   MOVE      NCL           C$$
025300940127     C                   Z-ADD     RIRI          CURR
025400940127     C                   Z-ADD     COCO          CURC
025500940127     C*
025600940224     C* I campi che al momento dell'F4 risultano protetti non devono
025700940224     C* essere abilitati alla funzione di ricerca
025800940224     C*
0259009401271    C                   SELECT
026000940127     C*
026100040616     C* FMTD1  - CLIENTE
026200040616    >C     H1NMFL        WHENeq    'D1COD'
026300940207     C* imposto pos. del cursore
026400040616     C                   Z-ADD     CURR          H1RIGA
026500040616     C                   Z-ADD     CURC          H1COLO
026600940127     C*
026700040616     C                   RESET                   ANA201DS
026800040616     C                   MOVE      xscSOC        SOC201
026900040616     C                   MOVEL     'C'           KCC201
027000040616     C                   MOVEL     D1COD         KSC201
027100040616     C                   MOVEL     ANA201DS      KPJBU
027200040616     C                   CALL      'ANA201R'
027300040616     C                   PARM                    KPJBA
027400040616     C                   MOVEL     KPJBU         ANA201DS
027500040616     C     KSC201        IFNE      *BLANK
027600040616     C                   MOVEL     KSC201        D1COD
027700040616     C     KEYcln        CHAIN     ancln01l                           51
027800040616     c  n51              eval      d1dcod = CLNDESBREV
027900040616     C                   ENDIF
028000040616     C*
028100040616     C* FMTD1  - Metodo
028200040616    >C     H1NMFL        WHENeq    'D1CALDTV'
028300050429    >C     H1NMFL        OREQ      'CDLIMCYP3'
028400050429    >C     H1NMFL        OREQ      'CDLSMCYP3'
028500040616     C                   RESET                   ANA964DS
028600040616     C                   MOVEL     '01'          OPZ964
028700040616     C                   MOVEL     'ANBCO000'    TRC964
028800040616     C                   MOVE      *on           All964
028900040616     C                   MOVEL     'BCOCALDTV'   CAM964
029000040616     C                   MOVEL     ANA964DS      KPJBU
029100040616     C                   CALL      'ANA964R'
029200040616     C                   PARM                    KPJBA
029300040616     C                   MOVEL     KPJBU         ANA964DS
029400040616     C*
029500050429     C     ret964        IFEQ      *BLANK                                       Premuto invio
029600050429     C                   SELECT
029700050429    >C     H1NMFL        WHENEQ    'D1CALDTV'
029800040616     C                   MOVEL     VAL964        CALDTVyp3
029900050429    >C     H1NMFL        WHENEQ    'CDLIMCYP3'
030000050429     C                   MOVEL     VAL964        CDLIMCYP3
030100050429    >C     H1NMFL        WHENEQ    'CDLSMCYP3'
030200050429     C                   MOVEL     VAL964        CDLSMCYP3
030300050429     C                   ENDSL
030400040616     C                   ENDIF
030500040616     C*
030600040616     C* FMTD1  - Calendario
030700040616    >C     H1NMFL        WHENeq    'D1CALEND'
030800040616     C                   RESET                   xtabds
030900040616     C                   MOVE      *blank        XTAKEY
031000040616     C                   MOVE      '01'          XTAOPZ
031100040616     C                   MOVE      XSCSOC        XTAAZI
031200040616     C                   MOVE      XSCLIN        XTALIN
031300040616     C                   Z-ADD     CURR          XTARIG
031400040616     C                   Z-ADD     CURC          XTACOL
031500040616     C                   CLEAR                   KPJBU
031600040616     C                   MOVEL     XTABDS        KPJBU
031700040616     C                   CALL      'ANGG71R'
031800040616     C                   PARM                    KPJBA
031900040616     C                   MOVEL     KPJBU         XTABDS
032000050429     C     XTARet        IFEQ      *BLANK                                       Premuto invio
032100040616     C                   MOVE      XTAKEY        CalendYP3
032200040616     C                   ENDIF
032300940223     C*
032400940223     C* ricerca non abilitata in questa posizione
0325009402231O   C                   OTHER
032600941005     C                   SETON                                        9899
032700941005     C* imposto pos. del cursore
032800941005     C                   Z-ADD     CURR          H1RIGA
032900941005     C                   Z-ADD     CURC          H1COLO
033000940127     C*
0331009401271-   C                   ENDSL
033200940127     C*
033300940506     C                   MOVE      *HIVAL        $LASTV
033400940127     C                   ENDSR
033500940615     C/EJECT
033600940615     C************************************************************
033700940615     C* TRADUZIONI
033800940615     C************************************************************
033900940615     C     LINGUA        BEGSR
034000940615     C*
034100940615     C* determino Riga/Colonna del cursore
034200940615     C                   MOVE      NRG           R$$
034300940615     C                   MOVE      NCL           C$$
034400940615     C                   Z-ADD     RIRI          CURR
034500940615     C                   Z-ADD     COCO          CURC
034600940615     C*
034700940615     C*
034800940615     C*
034900940615     C* FMTD1  - CAMPI DA TRADURRE
035000940615     C* imposto pos. del cursore
035100940615     C                   Z-ADD     CURR          H1RIGA
035200940615     C                   Z-ADD     CURC          H1COLO
035300940621     C                   MOVEL     H1NMFL        W005              5
035400940621     C                   MOVE      W005          W003              3
035500940621     C                   MOVEL     W003          W006              6
035600040617     C                   MOVE      *blank        KEY              15
035700040616     C                   MOVEL     D1COD         KEY
035800940615     C                   SELECT
035900940615     C*
036000940615     C*
036100940615     C* ricerca non abilitata in questa posizione
0362009406151O   C                   OTHER
036300940615     C                   SETON                                        98
036400940615     C*
0365009406151-   C                   ENDSL
036600940615     C*
036700940615     C                   MOVE      *HIVAL        $LASTV
036800940615     C                   ENDSR
036900940225     C/EJECT
037000940225     C************************************************************
037100940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
037200940225     C************************************************************
037300940225     C     GESPIE        BEGSR
037400940225     C*
037500940225     C                   SELECT
037600940225     C*
037700940506     C     $GEST         WHENEQ    'D1'
037800940506     C     $ULKD1        IFEQ      0
037900940506     C* riempimento opzioni
038000940506     C                   EXSR      F24D1
038100940506     C                   ELSE
038200940614     C                   WRITE     Z1
038300940506     C                   ENDIF
038400940225     C*
038500940510     C*
038600940225     C                   ENDSL
038700940225     C*
038800940225     C                   ENDSR
038900940131     C/EJECT
039000940131     C************************************************************
039100940510     C* GESTIONE VIDEO RECORD D1
039200940131     C************************************************************
039300940127     C     GESD1         BEGSR
039400940225     C* inizializzazione videata
039500940223     C     $INZD1        IFEQ      *ON
039600940127     C                   EXSR      INZD1
039700940223     C                   MOVE      *OFF          $INZD1
039800940117     C                   END
039900940225     C* emissione piede videata se proveniente da altra
040000940225     C* salvataggio valore formato di provenienza
040100940207     C     $LASTV        IFNE      'D1'
040200940614     C                   WRITE     T1
040300940225     C                   EXSR      GESPIE
040400940202     C                   MOVE      $LASTV        $LASTG
040500940202     C                   MOVE      'D1'          $LASTV
040600940202     C                   END
040700940131     C*
040800940203     C                   SELECT
040900940203     C* Annullamento
041000940315    >C     OPZ002        WHENEQ    '04'
041100940224     C* Visualizzazione
041200940315    >C     OPZ002        OREQ      '05'
041300940620     C                   SETON                                        0203
041400940620     C* Manutenzione
041500940620    >C     OPZ002        WHENEQ    '02'
041600940620     C                   SETON                                        02
041700940620     C                   SETOFF                                       03
041800940620     C*
041900940620     C                   OTHER
042000940620     C*
042100940620     C                   SETOFF                                       0203
042200940203     C                   ENDSL
042300940620     C* Immissione/Modifica/Copia
042400940620     C                   EXFMT     D1
042500940506     C* controllo abilitazione tasti funzione
042600940506     C*
042700940506     C                   EXSR      CTRKD1
042800940506     C*
042900940506     C     *IN99         IFEQ      *OFF
043000940131     C*
0431009402031    C                   SELECT
043200940510     c* ROLLUP
043300940131     C* F3=Fine
043400940506     C     $TASTO        WHENEQ    F03
043500940309     C                   EXSR      F03D1
043600940131     C* F4=Ricerca
043700940506     C     $TASTO        WHENEQ    F04
043800940127     C                   EXSR      SEARCH
043900940224     C* F5=Refresh
044000940506     C     $TASTO        WHENEQ    F05
044100940309     C                   EXSR      F05D1
044200940223     C* F8=Successivo
044300940506     C     $TASTO        WHENEQ    F08
044400940309     C                   EXSR      F08D1
044500940131     C* F12=Ritorno
044600940506     C     $TASTO        WHENEQ    F12
044700940309     C                   EXSR      F12D1
044800040616     C* F14=simulazione
044900040616     C     $TASTO        WHENEQ    F14
045000040616     C                   EXSR      F14D1
045100940315     C* F24=Altri tasti
045200940506     C     $TASTO        WHENEQ    F24
045300940315     C                   EXSR      F24D1
045400940131     C*
0455009402031O   C                   OTHER
045600940127     C                   EXSR      CTRD1
045700940616     C     *IN99         IFEQ      *OFF
045800940616     C                   SELECT
045900940616     C* F7=Traduzione
046000940616     C     $TASTO        WHENEQ    F07
046100940616     C                   EXSR      LINGUA
046200940616     C* F6
0463009406162    C     $TASTO        WHENEQ    F06
046400940224     C* eseguo aggiornamento
046500940315    >C                   EXSR      AGGANA
046600940224     C* eseguo operazioni del dopo-aggiornamento
0467009403183    C     *IN99         IFEQ      *OFF
046800940318     C                   EXSR      GESAGG
0469009403183-   C                   ENDIF
0470009406162-   C                   ENDSL
047100940616     C                   ENDIF
047200940203     C*
0473009402241-   C                   ENDSL
047400940224     C*
0475009405062-   C                   ENDIF
047600940117     C                   ENDSR
047700940506     C/EJECT
047800940506     C************************************************************
047900940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
048000940506     C************************************************************
048100940506     C     CTRKD1        BEGSR
048200940506     C                   SETOFF                                       99
048300940506     C                   MOVEA     $FC1          $FC
048400940506     C                   EXSR      CTRKEY
048500940506     C     *IN99         IFEQ      *ON
048600940506     C                   SETON                                        97
048700940506     C                   ENDIF
048800940506     C                   ENDSR
048900940117      ************************************************************
049000940207      * INIZIALIZZAZIONE VIDEATA DATI
049100940117      ************************************************************
049200940127     C     INZD1         BEGSR
049300940131     C*
049400940615     C                   CLEAR                   D1
049500040616     c                   eval      rimlunyp3 = xscoff
049600040616     c                   eval      rimmaryp3 = xscoff
049700040616     c                   eval      rimmeryp3 = xscoff
049800040616     c                   eval      rimgioyp3 = xscoff
049900040616     c                   eval      rimvenyp3 = xscoff
050000010406X0113C*
050100010406  "  C* reperimento titolo
050200010406  "  C                   clear                   xatbds
050300010406  "  C                   eval      xtbkey      = *zero
050400010417X0113C* inserita la seguenta riga
050500010417$002 C                   move      'YP3'         xtbkey
050600010417X0113C                   eval      xtbazi      = xscsoc
050700010406  "  C                   eval      xtblin      = xsclin
050800010406  "  C                   eval      xtbcod      = 'AAA'
050900010406  "  C                   eval      xtbric      = '1'
051000010406  "  C                   eval      xtbalc      = '0'
051100010406  "  C                   CALLB     'XATB'
051200010406  "  C                   PARM                    XATBDS
051300010406  "  C                   if        xtberr <> *off
051400010406  "  C                   movel     *all'?'       WrkTit
051500010406  "  C                   else
051600010406  "  C                   movel     xtbuni        WrkTit
051700010618  "  C                   move      '     '       WrkTit
051800010406  "  C                   endif
051900010406  "  C*
052000010406  "  C* centratura
052100010406  "  C                   eval      WrkTit=%trimr(Wrktit)
052200010406  "  C     ' '           CHECKR    Wrktit        I
052300010406  "  C                   call      'X71CENTRA'
052400010406  "  C                   parm                    Wrktit
052500010406  "  C                   parm                    I
052600010406  "  C                   parm                    lentit
052700010406  "  C                   movel     Wrktit        T1tit
052800010406X0113C*
052900990827X0079C* Aggiunta riga seguente
053000040616$028 C                   move      xscon         D1RIM1
053100040616$028 C                   move      xscoFF        D1RIM0
053200990827$028 C
053300941006     C                   MOVE      '0'           D1RIS
053400040616     C                   MOVEL     KEY002        D1COD
053500940615     C                   MOVE      AZI002        XTBAZI
053600940615     C                   MOVE      LIN002        XTBLIN
053700940616     C                   MOVE      *ALL'0'       XTBKEY
053800940615     C                   MOVE      KEY002        XTBKEY
053900970509     C                   MOVE      UNI002        XTBUNT
054000940615$007 C                   MOVE      'YP3'         XTBCOD
054100940225     C*
054200940207     C*
054300940224     C* valorizzo la chiave univoca per reperire i dati
054400940224     C* se non immissione
0545009403151   >C     OPZ002        IFNE      '01'
054600940315    >C*-------------------------------------------------------------
054700940315    >C* impostare i campi chiave ricevuti
054800940315    >C*-------------------------------------------------------------
054900940318     C*
055000940614     C*
055100940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0552009406142   >C     OPZ002        IFEQ      '02'
055300940315    >C     OPZ002        OREQ      '04'
055400940614     C                   MOVE      '1'           XTBALC
055500940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0556009406142E   C                   ELSE
055700940614     C                   MOVE      '0'           XTBALC
0558009406142-   C                   ENDIF
055900940614     C*
056000940616     C                   MOVE      '1'           XTBRIC
056100970214     C                   CALLB     'XATB'
056200940614     C                   PARM                    XATBDS
056300940318     C*
056400940614     C     XTBERR        IFEQ      '0'
056500940614     C                   MOVEL     XTBUNI        EXTDS
056600970512     c                   move      xtbunt        d1uniTA           8
056700941108     C* Giro la data se non blank
056800970214     c                   move      xtbdsc        xtbdscA          10
056900970214     C     XTBDSCa       IFNE      DATA
057000970214     C                   CALLB     'XDT4000'
057100970214     C                   PARM      XTBDSCA       XDTAMG           10
057200940621     C                   PARM      *ZEROS        XDTGMA            6 0
057300940621     C                   PARM      2             XDTSTA            1 0
057400940621     C                   MOVE      XDTGMA        D1DSC
057500940712     C                   MOVE      XTBRIS        D1RIS
057600941108     C                   ELSE
057700941108     C                   MOVE      *ZEROS        D1DSC
057800941108     C                   ENDIF
057900941108     C                   ENDIF
058000940614     C*
058100940224     C*
058200940224     C* se il rcd da gestire non viene trovato
058300940318     C* oppure è allocato da altro pgm
058400940224     C* ritorno al pgm guida avvertendolo dell'errore
0585009406142    C     XTBERR        IFNE      '0'
058600940224     C                   MOVE      *ON           $FINE
058700940315    >C                   MOVE      '1'           ERR002
058800940224     C*
058900940224     C* altrimenti
0590009402242E   C                   ELSE
059100940224     C* riempio la videata
059200940224     C                   EXSR      RIED1
059300940224     C* richiamo routine dei ctrl per decodificarla
059400940614    >C                   EXSR      CTRD1
059500960228     C                   MOVEL     *IN99         SAV99             1
059600960228     C* Se la gestione è più videate
059700960305    >C                   EXSR      CTRD2
059800960305    >C     *IN99         IFEQ      *ON
059900960305    >C                   MOVEL     *IN99         SAV99             1
060000960305    >C                   ENDIF
060100940224     C                   MOVE      *ALL'0'       IN5098
060200940224     C                   MOVEA     IN5098        *IN(50)
060300940224     C* definizione indicatori su video
060400940224     C                   SETOFF                                       02
0605009402243    C                   SELECT
060600940315    >C     OPZ002        WHENEQ    '02'
060700940224     C                   SETON                                        02
060800940315    >C     OPZ002        WHENEQ    '05'
060900940506     C* disabilita tasti
061000940720     C                   MOVE      '0'           $FC(6)
061100940720     C                   MOVE      '0'           $FC(5)
061200940720     C                   MOVE      '0'           $FC(4)
0613009402243-   C                   ENDSL
061400940224     C*
0615009402242-   C                   ENDIF
061600940223     C*
0617009402241-   C                   ENDIF
061800940224     C*
061900940117     C                   ENDSR
062000940207     C/EJECT
062100940207     C************************************************************
062200940510     C* RIEMPIMENTO VIDEATA  D1
062300940207     C************************************************************
062400940207     C     RIED1         BEGSR
062500940207     C*
062600990827$025 C     RIMLUNYP3     IFEQ      *ON
062700990827$025 C                   MOVE      XSCON         RIMLUNYP3
062800990827$025 C                   ELSE
062900990827$025 C                   MOVE      XSCOFF        RIMLUNYP3
063000990827$025 C                   ENDIF
063100990827$025 C     RIMMARYP3     IFEQ      *ON
063200990827$025 C                   MOVE      XSCON         RIMMARYP3
063300990827$025 C                   ELSE
063400990827$025 C                   MOVE      XSCOFF        RIMMARYP3
063500990827$025 C                   ENDIF
063600990827$025 C     RIMMERYP3     IFEQ      *ON
063700990827$025 C                   MOVE      XSCON         RIMMERYP3
063800990827$025 C                   ELSE
063900990827$025 C                   MOVE      XSCOFF        RIMMERYP3
064000990827$025 C                   ENDIF
064100990827$025 C     RIMGIOYP3     IFEQ      *ON
064200990827$025 C                   MOVE      XSCON         RIMGIOYP3
064300990827$025 C                   ELSE
064400990827$025 C                   MOVE      XSCOFF        RIMGIOYP3
064500990827$025 C                   ENDIF
064600990827$025 C     RIMVENYP3     IFEQ      *ON
064700990827$025 C                   MOVE      XSCON         RIMVENYP3
064800990827$025 C                   ELSE
064900990827$025 C                   MOVE      XSCOFF        RIMVENYP3
065000990827$025 C                   ENDIF
065100990827$025 C
065200940207     C*
065300940207     C                   ENDSR
065400940309     C/EJECT
065500940309     C************************************************************
065600940309     C* GESTIONE F03 VIDEO D1
065700940309     C************************************************************
065800940309     C     F03D1         BEGSR
065900940309     C*
066000940309     C                   MOVE      *ON           $FINE
066100940315    >C                   MOVE      '1'           RET002
066200940325     C* fine programma
066300940325     C                   EXSR      ENDPGM
066400940325     C*
066500940325     C                   ENDSR
066600940309     C/EJECT
066700940309     C************************************************************
066800940309     C* GESTIONE F05 VIDEO D1
066900940309     C************************************************************
067000940309     C     F05D1         BEGSR
067100940309     C*
067200940309     C                   MOVE      *ON           $INZD1
067300940309     C*
067400940309     C                   ENDSR
067500940309     C/EJECT
067600940309     C************************************************************
067700940309     C* GESTIONE F08 VIDEO D1
067800940309     C************************************************************
067900940309     C     F08D1         BEGSR
068000940309     C*
068100940309     C                   MOVE      *ON           $FINE
068200940315    >C                   MOVE      '0'           RET002
068300940309     C*
068400940309     C                   ENDSR
068500940309     C/EJECT
068600040616     C************************************************************
068700040616     C* GESTIONE F14 VIDEO D1
068800040616     C************************************************************
068900040616     C     F14D1         BEGSR
069000040616     C*
069100040616     C*  CALCOLO DATA VALUTA
069200040616     C                   Call      'X17CLVALR'
069300040616     C                   Parm                    KPJBA
069400040616     C*
069500040616     C                   ENDSR
069600040616     C/EJECT
069700940309     C************************************************************
069800940309     C* GESTIONE F12 VIDEO D1
069900940309     C************************************************************
070000940309     C     F12D1         BEGSR
070100940309     C*
070200940309     C                   MOVE      *ON           $FINE
070300940315    >C                   MOVE      '2'           RET002
070400940309     C*
070500940309     C                   ENDSR
070600940309     C/EJECT
070700940309     C************************************************************
070800940309     C* GESTIONE F24 VIDEO D1
070900940309     C************************************************************
071000940309     C     F24D1         BEGSR
071100940309     C*
071200940506     C*
071300940506     C                   MOVEA     $FX1          $FX
071400940506     C                   MOVEA     $FC1          $FC
071500940506     C                   MOVEA     $FR1          $FR
071600940506     C                   Z-ADD     $ULKD1        $ULK
071700940506     C                   EXSR      RIEKEY
071800941017     C                   MOVEA     $FC           $FC1
071900940506     C                   Z-ADD     $ULK          $ULKD1            3 0
072000050429     C                   MOVEL     $KEY1         Z1KE1
072100050429     C                   MOVEL     $KEY2         Z1KE2
072200050429     C     Green:Blue    XLATE     z1ke1         z1ke1
072300050429     C     Green:Blue    XLATE     z1ke2         z1ke2
072400940614     C                   WRITE     Z1
072500940506     C*
072600940309     C                   ENDSR
072700940207     C/EJECT
072800940131     C************************************************************
072900940207     C* CONTROLLO VIDEATA
073000940131     C************************************************************
073100940127     C     CTRD1         BEGSR
073200940131     C*
073300940127     C                   SETOFF                                       99
073400940127     C* non imposto pos. del cursore
073500940127     C                   Z-ADD     0             H1RIGA
073600940127     C                   Z-ADD     0             H1COLO
073700940621     C*
073800940621     C* CTRL DATA
073900940621     C*
074000941109     C                   MOVE      DATA          WRKDSC           10
074100940621     C     D1DSC         IFNE      0
074200970214     C                   CALLB     'XDT4000'
074300941109     C                   PARM                    XDTAMG           10
074400940621     C                   PARM      D1DSC         XDTGMA            6 0
074500940621     C                   PARM      3             XDTSTA            1 0
074600940621     C     XDTSTA        IFNE      0
074700940621     C                   SETON                                        5599
074800940621     C                   ELSE
074900941108     C                   MOVE      XDTAMG        WRKDSC
075000940621     C                   ENDIF
075100940621     C                   ENDIF
075200940223     C*
075300940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
075400940223     C*
0755009403151   >C     OPZ002        IFNE      '04'
075600971110     C*
075700971110     C* controllo codice vuoto
075800050211     C                   SELECT
075900050211$017 C     D1COD         WHENEQ    *ALL' '
076000971110     C                   SETON                                        5099
076100050211$017 C     D1COD         WHENEQ    *ZERO
076200050211     c                   eval      d1dcod = 'Rimborso standard'
076300050211     C                   OTHER
076400040616     C     KEYcln        CHAIN     ancln01l                           51
076500040616     C                   if        *in51
076600040616     C                   SETON                                          99
076700040616     C                   Else
076800040616     c                   eval      d1dcod = CLNDESBREV
076900040616     C                   END
077000050211     C                   ENDSL
077100971110     C*
077200940614$009 C     RIMLUNYP3     IFEQ      *BLANK
077300940614$009 C                   SETON                                        6099
077400940614$009 C                   ENDIF
077500940614$009 C     RIMLUNYP3     IFNE      XSCON
077600940614$009 C     RIMLUNYP3     ANDNE     XSCOFF
077700940614$009 C                   SETON                                        6099
077800940614$009 C                   ENDIF
077900940614$009 C     RIMMARYP3     IFEQ      *BLANK
078000940614$009 C                   SETON                                        6199
078100940614$009 C                   ENDIF
078200940614$009 C     RIMMARYP3     IFNE      XSCON
078300940614$009 C     RIMMARYP3     ANDNE     XSCOFF
078400940614$009 C                   SETON                                        6199
078500940614$009 C                   ENDIF
078600940614$009 C     RIMMERYP3     IFEQ      *BLANK
078700940614$009 C                   SETON                                        6299
078800940614$009 C                   ENDIF
078900940614$009 C     RIMMERYP3     IFNE      XSCON
079000940614$009 C     RIMMERYP3     ANDNE     XSCOFF
079100940614$009 C                   SETON                                        6299
079200940614$009 C                   ENDIF
079300940614$009 C     RIMGIOYP3     IFEQ      *BLANK
079400940614$009 C                   SETON                                        6399
079500940614$009 C                   ENDIF
079600940614$009 C     RIMGIOYP3     IFNE      XSCON
079700940614$009 C     RIMGIOYP3     ANDNE     XSCOFF
079800940614$009 C                   SETON                                        6399
079900940614$009 C                   ENDIF
080000940614$009 C     RIMVENYP3     IFEQ      *BLANK
080100940614$009 C                   SETON                                        6499
080200940614$009 C                   ENDIF
080300940614$009 C     RIMVENYP3     IFNE      XSCON
080400940614$009 C     RIMVENYP3     ANDNE     XSCOFF
080500940614$009 C                   SETON                                        6499
080600940614$009 C                   ENDIF
080700940207     C*
080800040616     C* non possono essere tutti a "N"
080900040616     c                   if        RIMlunYP3 = xscoff   and
081000040616     c                             RIMmarYP3 = xscoff   and
081100040616     c                             RIMmerYP3 = xscoff   and
081200040616     c                             RIMgioYP3 = xscoff   and
081300040616     c                             RIMvenYP3 = xscoff
081400040616$009 C                   SETON                                        6599
081500040616$009 C                   ENDIF
081600040616     C*
081700040616     C* Se almeno un giorno valuta è impostato occorre avere un Metodo
081800040616     C*  di calcolo
081900040616     c                   if        GVALUNYP3 > 0   or
082000040616     c                             GVAMARYP3 > 0   or
082100040616     c                             GVAMERYP3 > 0   or
082200040616     c                             GVAGIOYP3 > 0   or
082300040616     c                             GVAVENYP3 > 0
082400040616     c                   if        CALDTVYP3 = *blank
082500040616$009 C                   SETON                                        6699
082600040616$009 C                   ENDIF
082700040616     c                   if        CALENDYP3 = *blank
082800040616$009 C                   SETON                                        6799
082900040616$009 C                   ENDIF
083000040616$009 C                   ENDIF
083100040616  "  C*
083200040616  "  C* ctrl notifica
083300040616  "  C                   MOVE      '1'           X07RIC
083400040616  "  C                   MOVEL     *on           X07ALL
083500040616  "  C                   MOVEL     'ANBCO000'    X07TRC
083600040616  "  C                   MOVEL     'BCOCALDTV'   X07CAM
083700040616  "  C                   MOVEL     CALDTVYP3     X07VAL
083800040616  "  C                   CALLb     'X07VALR'
083900040616  "  C                   PARM                    X07DS
084000040616  "  C     X07ERR        IFEQ      '0'
084100040616  "  C                   ELSE
084200040616  "  C                   SETON                                        6699
084300040616  "  C                   ENDIF
084400040616A0970C*
084500040616     C*
084600040616      * CTR CALENDARIO
084700040616     C                   MOVEL     *BLANK        D1dCalen
084800040616    >C     Calendyp3     IFNE      *BLANK
084900040616     C                   MOVE      '1'           XTBRIC
085000040616     C                   MOVE      xscSOC        XTBAZI
085100040616     C                   MOVE      *ZERO         XTBKEY
085200040616     C                   MOVE      'G71'         XTBCOD
085300040616     C                   MOVE      calendyp3     XTBKEY
085400040616     C                   CALLB     'XATB'
085500040616     C                   PARM                    XATBDS
085600040616   B3C     XTBERR        IFEQ      '0'
085700040616     C                   MOVEL     XTBUNI        angg71ds
085800040616     C                   MOVEL     DESG71        D1dcalen
085900040616   X3C                   ELSE
086000040616     C                   SETON                                        6799
086100040616   E3C                   ENDIF
086200040616   E3C                   ENDIF
086300050429  "  **?Controllo metodo calcolo data data limite incasso.
086400050429  "  C                   MOVE      '1'           X07RIC
086500050429  "  C                   MOVEL     *on           X07ALL
086600050429  "  C                   MOVEL     'ANBCO000'    X07TRC
086700050429  "  C                   MOVEL     'BCOCALDTV'   X07CAM
086800050429  "  C                   MOVEL     CDLIMCYP3     X07VAL
086900050429  "  C                   CALLb     'X07VALR'
087000050429  "  C                   PARM                    X07DS
087100050429  "  C     X07ERR        IFEQ      '0'
087200050429  "  C                   ELSE
087300050429  "  C                   SETON                                        4999
087400050429  "  C                   ENDIF
087500050429  "  **?Controllo metodo calcolo data data limite spedizioni.
087600050429  "  C                   MOVE      '1'           X07RIC
087700050429  "  C                   MOVEL     *on           X07ALL
087800050429  "  C                   MOVEL     'ANBCO000'    X07TRC
087900050429  "  C                   MOVEL     'BCOCALDTV'   X07CAM
088000050429  "  C                   MOVEL     CDLSMCYP3     X07VAL
088100050429  "  C                   CALLb     'X07VALR'
088200050429  "  C                   PARM                    X07DS
088300050429  "  C     X07ERR        IFEQ      '0'
088400050429  "  C                   ELSE
088500050429  "  C                   SETON                                        4899
088600050429  "  C                   ENDIF
088700040616     C*
088800940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
088900940315    >C     OPZ002        IFEQ      '01'
089000940315    >C     OPZ002        OREQ      '03'
089100940315    >C*-------------------------------------------------------------
089200940315    >C* impostare i campi chiave e verificare l'esistenza del record
089300940315    >C*-------------------------------------------------------------
089400971110$007 C                   MOVE      'YP3'         XTBCOD
089500040616     C                   MOVE      *blank        XTBKEY
089600040616    >C                   MOVEL     D1COD         XTBKEY
089700970512    >C                   MOVE      D1UNITA       XTBUNT
089800940614     C                   MOVE      '0'           XTBALC
089900940614     C                   MOVE      '1'           XTBRIC
090000970214    >C                   CALLB     'XATB'
090100940614     C                   PARM                    XATBDS
090200940614     C     XTBERR        IFEQ      '0'
090300940223     C                   SETON                                        5999
090400940223     C                   ENDIF
090500940620     C* Controllo se compreso nei range indice
090600940620     C     XTBERR        IFEQ      '3'
090700940620    >C                   SETON                                        5899
090800940620     C                   END
090900940620     C*
091000940620     C* REPERISCO AUTORIZZAZIONI UTENTE
091100940620     C                   CLEAR                   XCHATB
091200941118     C                   MOVE      XSCSOC        XCTSOC
091300941118     C                   MOVE      XSCDUN        XCTUNI
091400940620     C                   MOVEL     KNMUS         XCTPRF
091500940620     C                   MOVE      XTBCOD        XCTCOD
091600940620     C                   CALL      'XCHKATB'
091700940620     C                   PARM                    XCHATB
091800941111     C                   MOVEL     XCTDDS        AUTDS
091900941027     C     XCTRTN        IFEQ      1
092000941027     C                   MOVEL     'N'           MANGRU
092100941027     C                   ENDIF
092200940620     C* Non ammessa immissione di gruppo
092300940620     C     XTBGRU        IFEQ      'G'
092400940620     C     MANGRU        ANDEQ     'N'
092500940620     C                   SETON                                        5799
092600940620     C                   END
092700940620     C* Non ammessa immissione di azienda
092800940620     C     XTBGRU        IFEQ      'A'
092900940620     C     MANAZI        ANDEQ     'N'
093000940620     C                   SETON                                        5699
093100940620     C                   END
093200940712     C* codice riservato non accessibile
093300941006     C     XTBRIS        IFGT      ABIRIS
093400941005    >C     OPZ002        ANDNE     '01'
093500941005    >C     OPZ002        ANDNE     '03'
093600940712     C                   SETON                                        5499
093700940712     C                   END
093800940224     C                   ENDIF
093900940224     C*
0940009402241-   C                   END
094100940131     C*
094200940302     C     *IN99         IFEQ      *ON
094300940510     C     $GEST         ANDEQ     'D1'
094400960228     C     OPZ002        ANDNE     '07'
094500940117     C                   MOVEA     *IN           WIN
094600940128     C                   MOVE      *ALL'0'       IN5098           49
094700940128     C                   MOVEA     IN5098        *IN(50)
094800940614     C                   WRITE     D1
094900940117     C                   MOVEA     WIN           *IN
095000940302     C                   ENDIF
095100940127     C*
095200940117     C                   ENDSR
095300960305     C/EJECT
095400960305     C************************************************************
095500960305     C* CONTROLLO VIDEATA D2
095600960305     C************************************************************
095700960305     C     CTRD2         BEGSR
095800960305     C*
095900960305     C                   ENDSR
096000940207     C/EJECT
096100940224     C************************************************************
096200940315    >C* AGGIORNAMENTO ANAGRAFICA
096300940224     C************************************************************
096400940315    >C     AGGANA        BEGSR
096500971110     C*
096600971110$007 C                   MOVE      'YP3'         XTBCOD
096700040617     C                   MOVE      *blank        XTBKEY
096800040616     C                   MOVEL     D1COD         XTBKEY
096900970512     C                   MOVE      D1UNITA       XTBUNT
097000940207     C*
097100940207     C                   SELECT
097200940315    >C     OPZ002        WHENEQ    '01'
097300940315    >C     OPZ002        OREQ      '03'
097400940315    >C                   EXSR      RIEANA
097500950609     C                   MOVE      XSCCUT        XTBUTE
097600940614     C                   MOVE      '5'           XTBRIC
097700970214     C                   CALLB     'XATB'
097800940614    >C                   PARM                    XATBDS
097900940318     C* se rcd non scrivibile attivo errore
098000940615     C     XTBERR        IFEQ      '1'
098100940318    >C                   SETON                                        5999
098200940318     C                   END
098300940315    >C     OPZ002        WHENEQ    '02'
098400940621     C                   MOVE      '1'           XTBRIC
098500940621     C                   MOVE      '1'           XTBALC
098600970214     C                   CALLB     'XATB'
098700940621     C                   PARM                    XATBDS
098800940622    >C                   EXSR      RIEANA
098900950609     C                   MOVE      XSCCUT        XTBUTE
099000940614     C                   MOVE      '4'           XTBRIC
099100970214     C                   CALLB     'XATB'
099200940614    >C                   PARM                    XATBDS
099300940315    >C     OPZ002        WHENEQ    '04'
099400940621     C                   MOVE      '1'           XTBRIC
099500940621     C                   MOVE      '1'           XTBALC
099600970214     C                   CALLB     'XATB'
099700940621     C                   PARM                    XATBDS
099800940614     C                   MOVE      '6'           XTBRIC
099900970214     C                   CALLB     'XATB'
100000940614    >C                   PARM                    XATBDS
100100940621     C* annullo traduzioni
100200940621     C     XTBERR        IFEQ      '0'
100300940621     C                   EXSR      ANNTBT
100400940621     C                   END
100500940621     C*
100600940207     C                   ENDSL
100700940203     C*
100800940203     C                   ENDSR
100900940224     C/EJECT
101000940224     C************************************************************
101100940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
101200940224     C************************************************************
101300940616     C     RIEANA        BEGSR
101400940616     C                   CLEAR                   XTBUNI
101500940621     C                   MOVE      WRKDSC        XTBDSC
101600940712     C                   MOVE      D1RIS         XTBRIS
101700981209$026 C     RIMLUNYP3     IFEQ      XSCON
101800981209$026 C                   MOVE      *ON           RIMLUNYP3
101900981209$026 C                   ELSE
102000981209$026 C                   MOVE      *OFF          RIMLUNYP3
102100981209$026 C                   ENDIF
102200981209$026 C     RIMMARYP3     IFEQ      XSCON
102300981209$026 C                   MOVE      *ON           RIMMARYP3
102400981209$026 C                   ELSE
102500981209$026 C                   MOVE      *OFF          RIMMARYP3
102600981209$026 C                   ENDIF
102700981209$026 C     RIMMERYP3     IFEQ      XSCON
102800981209$026 C                   MOVE      *ON           RIMMERYP3
102900981209$026 C                   ELSE
103000981209$026 C                   MOVE      *OFF          RIMMERYP3
103100981209$026 C                   ENDIF
103200981209$026 C     RIMGIOYP3     IFEQ      XSCON
103300981209$026 C                   MOVE      *ON           RIMGIOYP3
103400981209$026 C                   ELSE
103500981209$026 C                   MOVE      *OFF          RIMGIOYP3
103600981209$026 C                   ENDIF
103700981209$026 C     RIMVENYP3     IFEQ      XSCON
103800981209$026 C                   MOVE      *ON           RIMVENYP3
103900981209$026 C                   ELSE
104000981209$026 C                   MOVE      *OFF          RIMVENYP3
104100981209$026 C                   ENDIF
104200981209$026 C
104300940616     C                   MOVEL     EXTDS         XTBUNI
104400940207     C                   ENDSR
104500940224     C/EJECT
104600940224     C************************************************************
104700940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
104800940224     C************************************************************
104900940224     C     GESAGG        BEGSR
105000940224     C*
105100940224     C* segnalo al pgm chiamante l'avvenuta conferma
105200940315    >C                   MOVE      *ON           OPR002
105300940224     C* segnalo al pgm chiamante l'aver premuto l'F6
105400940315    >C                   MOVE      '0'           RET002
105500940224     C*
1056009402241    C                   SELECT
105700940224     C* nel caso di immissione, ripristino la videata iniziale
105800940315    >C     OPZ002        WHENEQ    '01'
105900940224     C                   MOVE      *ON           $INZD1
106000940224     C* altrimenti ritorno al pgm chiamante
106100940224     C                   OTHER
106200940224     C                   MOVE      *ON           $FINE
1063009402241-   C                   ENDSL
106400940224     C*
106500940224     C                   ENDSR
106600940621     C/EJECT
106700940621     C************************************************************
106800940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
106900940621     C************************************************************
107000940621    >C     ANNTBT        BEGSR
107100940621     C                   MOVE      XTBAZI        TBTAZI
107200940621     C                   MOVE      XTBCOD        TBTCOD
107300940621     C                   MOVE      XTBKEY        TBTKEY
107400940621     C     KTBT          SETLL     ANTBT01L
107500940621     C                   DO        *HIVAL
107600940621     C     KTBT          DELETE    ANTBT01L                           21
107700940621     C  N21              END
107800940621     C*
107900940621     C                   ENDSR
108000940207     C/EJECT
108100940131     C************************************************************
108200940131     C* OPERAZIONI INIZIALI
108300940131     C************************************************************
108400940131     C     *INZSR        BEGSR
108500940131     C*
108600940127     C* Reperimento parametri
108700940127     C*
108800940117     C     *ENTRY        PLIST
108900940117     C                   PARM                    KPJBA
109000940712     C*
109100940712     C* Reperimento Codice utente
109200940712     C*
109300940223     C*
109400940506     C*
109500940506     C* Reperimento tasti di funzione
109600940506     C*
109700940506     C                   EXSR      REPKEY
109800940621     C*
109900940621     C* KLIST
110000940621     C*
110100940621     C     KTBT          KLIST
110200940621     C                   KFLD                    TBTAZI
110300940621     C                   KFLD                    TBTCOD
110400940621     C                   KFLD                    TBTKEY
110500040616     C*
110600040616     C     KEYCLN        KLIST
110700040616     C                   KFLD                    xscSOC
110800040616     C                   KFLD                    d1cod
110900010406X0113C*
111000010406  "  C* reperisco le dimensioni della variabile del titolo
111100010406X0113C                   EVAL      LENTIT = %Size(T1Tit)
111200940506     C*
111300940223     C                   ENDSR
111400941103     C/EJECT
111500941103      ************************************************************
111600941103      * Reperimento dati società
111700941103      ************************************************************
111800941103     C     REPSOC        BEGSR
111900941103      *
112000970214     C                   CALLB     'XSOC'
112100941103     C                   PARM                    TIPXSC            6
112200941103     C                   PARM                    SOCXSC            3
112300941103     C                   PARM                    CDSXSC            9 0
112400941103     C                   PARM                    MODXSC            3
112500941103     C                   PARM      *BLANKS       RTNXSC            1
112600941103     C                   PARM                    XSOCDS
112700941103     C                   PARM                    KPJBA
112800941103      *
112900941103     C                   ENDSR
113000940223     C/EJECT
113100940506     C************************************************************
113200940506     C* REPERIMENTO TASTI DI FUNZIONE
113300940506     C************************************************************
113400940506     C     REPKEY        BEGSR
113500940506     C                   MOVEA     $FM1          $FM
113600940506     C                   EXSR      REPKKK
113700940506     C                   MOVEA     $FX           $FX1
113800940614     C                   ENDSR
113900940506     C/EJECT
114000940506     C************************************************************
114100940506     C* REPERIMENTO TASTI DI FUNZIONE
114200940506     C************************************************************
114300940506     C     REPKKK        BEGSR
114400940506     C*
114500940506     C                   CLEAR                   $FX
114600940506     C     1             DO        24            X
114700940506     C*
114800940506     C     $FM(X)        IFNE      *BLANK
114900940506     C*
115000940506     C                   MOVEL     $FM(X)        IDMSG
115100970214     C                   CALLB     'XRTVM'
115200940506     C                   PARM                    IDMSG            27
115300940506     C                   PARM                    TXTMSG          100
115400940506     C                   PARM                    ERRMSG            1
115500940506     C*
115600940506     C     ERRMSG        IFNE      *ON
115700940506     C                   MOVEL     TXTMSG        $FX(X)
115800940506     C                   ELSE
115900940506     C                   MOVEL     *ALL' '       $FX(X)
116000940506     C                   END
116100940506     C*
116200040616     C* Eccezione F14 -> Pgm calcolo giorni di Valuta
116300040616     c                   if        x = 14
116400040616     C                   MOVEL     F14x17pgm     $FX(X)
116500040616     c                   end
116600040616     C*
116700940506     C                   END
116800940506     C*
116900940506     C                   END
117000940506     C*
117100940506     C                   ENDSR
117200940509     C/EJECT
117300940223     C************************************************************
117400940223     C* INIZIALIZZAZIONE VARIABILI
117500940223     C************************************************************
117600940223     C     INZVAR        BEGSR
117700951124     C*
117800951124     C* Chiamata XSOC
117900951124     C*
118000951124     C                   MOVEL     'SOC001'      TIPXSC
118100951124     C                   EXSR      REPSOC
118200951124     C     RTNXSC        IFNE      '1'
118300951124     C                   MOVEL     XSOCDS        SOC001
118400951124     C                   ELSE
118500951124     C                   MOVE      *ON           $FINE
118600951124     C* fine programma
118700951124     C                   EXSR      ENDPGM
118800951124     C                   ENDIF
118900940224     C*
119000940224     C* Dati da PGM guida
119100940224     C*
119200940315    >C                   MOVEL     KPJBU         D002DS
119300940128     C*
119400940223     C* Variabili per gestione videate
119500940128     C*
119600940223     C                   MOVE      'D1'          $GEST
119700940223     C                   MOVE      *OFF          $FINE
119800940223     C                   MOVE      *ON           $INZD1
119900940224     C                   MOVE      *BLANK        $LASTG
120000940224     C                   MOVE      *BLANK        $LASTV
120100940506     C                   Z-ADD     0             $ULKD1            3 0
120200940127     C*
120300940223     C* Variabili appoggio
120400940127     C*
120500940224     C                   Z-ADD     0             CURR
120600940224     C                   Z-ADD     0             CURC
120700940207     C*
120800940207     C* Valorizzazione campi univoci testate
120900940207     C*
121000940614     C                   CLEAR                   T1
121100940614     C                   MOVEL     KNSIF         NOMSIF
121200941109     C                   MOVEL     XSCDSI        NOMDIT
121300941027     C                   MOVEL     DSPGM         NOMPGM
121400940224     C                   Z-ADD     1             X
121500940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
121600940224     C     *IN21         IFEQ      *ON
121700940224     C                   MOVEL     $ID(X)        IDMSG
121800970214     C                   CALLB     'XRTVM'
121900940224     C                   PARM                    IDMSG            27
122000940224     C                   PARM                    TXTMSG          100
122100940224     C                   PARM                    ERRMSG            1
122200940224     C                   PARM      *ON           CTRMSG            1
122300940224     C                   PARM      30            LENMSG            3 0
122400940224     C     ERRMSG        IFNE      *ON
122500940224     C                   MOVEL     TXTMSG        T1OPE
122600940224     C                   ELSE
122700940224     C                   MOVEL     *ALL'?'       T1OPE
122800940224     C                   ENDIF
122900940224     C                   END
123000940224     C*
123100940224     C* Inizializzazione/Decodifica prima videata
123200940224     C*
123300940224     C                   EXSR      INZD1
123400940224     C                   MOVE      *OFF          $INZD1
123500960228     C* Se opzione = '07' richiesti solo controlli
123600960228     C     OPZ002        IFEQ      '07'
123700960228     C                   MOVEL     *IN99         ERR002
123800960228     C                   MOVEL     '0'           RET002
123900960228     C                   MOVEL     *ON           $FINE
124000960228     C                   ENDIF
124100940127     C*
124200940117     C                   ENDSR
124300940128     C************************************************************
124400940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
124500940223     C************************************************************
124600940223     C     DEFCAM        BEGSR
124700940223     C*
124800940223     C* klist
124900940207     C*
125000940223     C*
125100940223     C* Variabili per gestione videate
125200940223     C*
125300940223     C                   MOVE      *BLANK        $GEST             2
125400940223     C                   MOVE      *BLANK        $FINE             1
125500940223     C                   MOVE      *BLANK        $INZD1            1
125600940223     C                   MOVE      *BLANK        $LASTG            2
125700940223     C                   MOVE      *BLANK        $LASTV            2
125800940223     C*
125900940223     C* Variabili appoggio
126000940223     C*
126100940223     C                   Z-ADD     0             CURR              2 0
126200940223     C                   Z-ADD     0             CURC              2 0
126300940223     C                   MOVE      *ZEROS        WIN              99
126400940224     C                   MOVE      *BLANK        WTBERR            1
126500940223     C*
126600940223     C* Indici
126700940223     C*
126800940223     C                   Z-ADD     0             X                 3 0
126900940223     C                   Z-ADD     0             Y                 3 0
127000940127     C*
127100940117     C                   ENDSR
127200940128     C************************************************************
127300940509     C/EJECT
127400940509     C************************************************************
127500940509     C* CONTROLLO ABILITAZIONE TASTI
127600940509     C************************************************************
127700940509     C     CTRKEY        BEGSR
127800940509     C                   SELECT
127900940509     C     $TASTO        WHENEQ    F01
128000940720     C     $FC(1)        IFEQ      '0'
128100940509     C                   SETON                                        99
128200940509     C                   ENDIF
128300940509     C     $TASTO        WHENEQ    F02
128400940720     C     $FC(2)        IFEQ      '0'
128500940509     C                   SETON                                        99
128600940509     C                   ENDIF
128700940509     C     $TASTO        WHENEQ    F03
128800940720     C     $FC(3)        IFEQ      '0'
128900940509     C                   SETON                                        99
129000940509     C                   ENDIF
129100940509     C     $TASTO        WHENEQ    F04
129200940720     C     $FC(4)        IFEQ      '0'
129300940509     C                   SETON                                        99
129400940509     C                   ENDIF
129500940509     C     $TASTO        WHENEQ    F05
129600940720     C     $FC(5)        IFEQ      '0'
129700940509     C                   SETON                                        99
129800940509     C                   ENDIF
129900940509     C     $TASTO        WHENEQ    F06
130000940720     C     $FC(6)        IFEQ      '0'
130100940509     C                   SETON                                        99
130200940509     C                   ENDIF
130300940509     C     $TASTO        WHENEQ    F07
130400940720     C     $FC(7)        IFEQ      '0'
130500940509     C                   SETON                                        99
130600940509     C                   ENDIF
130700940509     C     $TASTO        WHENEQ    F08
130800940720     C     $FC(8)        IFEQ      '0'
130900940509     C                   SETON                                        99
131000940509     C                   ENDIF
131100940509     C     $TASTO        WHENEQ    F09
131200940720     C     $FC(09)       IFEQ      '0'
131300940509     C                   SETON                                        99
131400940509     C                   ENDIF
131500940509     C     $TASTO        WHENEQ    F10
131600940720     C     $FC(10)       IFEQ      '0'
131700940509     C                   SETON                                        99
131800940509     C                   ENDIF
131900940509     C     $TASTO        WHENEQ    F11
132000940720     C     $FC(11)       IFEQ      '0'
132100940509     C                   SETON                                        99
132200940509     C                   ENDIF
132300940509     C     $TASTO        WHENEQ    F12
132400940720     C     $FC(12)       IFEQ      '0'
132500940509     C                   SETON                                        99
132600940509     C                   ENDIF
132700940509     C     $TASTO        WHENEQ    F13
132800940720     C     $FC(13)       IFEQ      '0'
132900940509     C                   SETON                                        99
133000940509     C                   ENDIF
133100940509     C     $TASTO        WHENEQ    F14
133200940720     C     $FC(14)       IFEQ      '0'
133300940509     C                   SETON                                        99
133400940509     C                   ENDIF
133500940509     C     $TASTO        WHENEQ    F15
133600940720     C     $FC(15)       IFEQ      '0'
133700940509     C                   SETON                                        99
133800940509     C                   ENDIF
133900940509     C     $TASTO        WHENEQ    F16
134000940720     C     $FC(16)       IFEQ      '0'
134100940509     C                   SETON                                        99
134200940509     C                   ENDIF
134300940509     C     $TASTO        WHENEQ    F17
134400940720     C     $FC(17)       IFEQ      '0'
134500940509     C                   SETON                                        99
134600940509     C                   ENDIF
134700940509     C     $TASTO        WHENEQ    F18
134800940720     C     $FC(18)       IFEQ      '0'
134900940509     C                   SETON                                        99
135000940509     C                   ENDIF
135100940509     C     $TASTO        WHENEQ    F19
135200940720     C     $FC(19)       IFEQ      '0'
135300940509     C                   SETON                                        99
135400940509     C                   ENDIF
135500940509     C     $TASTO        WHENEQ    F20
135600940720     C     $FC(20)       IFEQ      '0'
135700940509     C                   SETON                                        99
135800940509     C                   ENDIF
135900940509     C     $TASTO        WHENEQ    F21
136000940720     C     $FC(21)       IFEQ      '0'
136100940509     C                   SETON                                        99
136200940509     C                   ENDIF
136300940509     C     $TASTO        WHENEQ    F22
136400940720     C     $FC(22)       IFEQ      '0'
136500940509     C                   SETON                                        99
136600940509     C                   ENDIF
136700940509     C     $TASTO        WHENEQ    F23
136800940720     C     $FC(23)       IFEQ      '0'
136900940509     C                   SETON                                        99
137000940509     C                   ENDIF
137100940509     C     $TASTO        WHENEQ    F24
137200940720     C     $FC(24)       IFEQ      '0'
137300940509     C                   SETON                                        99
137400940509     C                   ENDIF
137500940509     C                   ENDSL
137600940509     C                   ENDSR
137700940509     C/EJECT
137800940509     C************************************************************
137900940509     C* RIEMPIMENTO TASTI FUNZIONE
138000940509     C************************************************************
138100940509     C     RIEKEY        BEGSR
138200940509     C*
138300940509     C* Input :
138400940509     C* - $FX = schiera con tutti i testi disponibili
138500940509     C* - $FC = schiere con i flag di validità associati ai testi
138600940509     C* - $FR = schiere con i flag di REVERSE IMAGE
138700940509     C* - $ULK = ultimo testo caricato
138800940509     C* Output :
138900940509     C* - $ULK
139000940509     c* - $KEY1= 1° riga di tasti funzione
139100940509     c* - $KEY2= 2° riga di tasti funzione
139200940509     C*
139300970214     C                   CALLB     'XKEYMSG'
139400940509     C* input
139500940509     C                   PARM                    $FX
139600940509     C                   PARM                    $FC
139700940509     C                   PARM                    $FR
139800940509     C                   PARM                    $ULK              3 0
139900940509     C* output
140000940509     C                   PARM                    $KEY1            79
140100940509     C                   PARM                    $KEY2            79
140200940509     C*
140300940509     C                   ENDSR
140400940615**
14050094022401PROMSG    *LIBL     COS0001    immissione
14060094022402PROMSG    *LIBL     COS0002    modifica
14070094022403PROMSG    *LIBL     COS0003    copia
14080094022404PROMSG    *LIBL     COS0004    annullamento
14090094022405PROMSG    *LIBL     COS0005    visualizzazione
141000940509** TASTI DI FUNZIONE  D1
141100940509PROMSG    *LIBL     KEY0001  01
141200940509PROMSG    *LIBL     KEY0002  02
141300940509PROMSG    *LIBL     KEY0003  03
141400940509PROMSG    *LIBL     KEY0004  04
141500940509PROMSG    *LIBL     KEY0005  05
141600940509PROMSG    *LIBL     KEY0006  06
141700940509PROMSG    *LIBL     KEY0007  07
141800940509PROMSG    *LIBL     KEY0008  08
141900940509PROMSG    *LIBL     KEY0009  09
142000940509PROMSG    *LIBL     KEY0010  10
142100940509PROMSG    *LIBL     KEY0011  11
142200940509PROMSG    *LIBL     KEY0012  12
142300940509PROMSG    *LIBL     KEY0013  13
142400940509PROMSG    *LIBL     KEY0014  14
142500940509PROMSG    *LIBL     KEY0015  15
142600940509PROMSG    *LIBL     KEY0016  16
142700940509PROMSG    *LIBL     KEY0017  17
142800940509PROMSG    *LIBL     KEY0018  18
142900940509PROMSG    *LIBL     KEY0019  19
143000940509PROMSG    *LIBL     KEY0020  20
143100940509PROMSG    *LIBL     KEY0021  21
143200940509PROMSG    *LIBL     KEY0022  22
143300940509PROMSG    *LIBL     KEY0023  23
143400940509PROMSG    *LIBL     KEY0024  24
143500940509**  ABILITAZIONE D1
1436000406160N  01
1437009407200N  02
1438009407201N  03
1439009407201N  04
1440009407201N  05
1441009407201N  06
1442009407201N  07
1443009407201N  08
1444009407200N  09
1445000406160N  10
1446009407200N  11
1447009407201N  12
1448009407200N  13
1449000406161N  14
1450009407200N  15
1451009407200N  16
1452009407200N  17
1453000406160N  18
1454009407200N  19
1455000406160N  20
1456009407200N  21
1457009407200N  22
1458009407200N  23
1459009407200N  24
