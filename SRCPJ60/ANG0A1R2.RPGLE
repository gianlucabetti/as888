000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*NEW)
000200951031     H*PARMS CVTOPT(*DATETIME)
000300940223     H DECEDIT('0,') DATEDIT(*DMY.)
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
004100950214$001 FANG0A1D2  CF   E             WORKSTN
004200940201     F                                     INFDS(DSFMT)
004300940621     FANTBT01L  UF   E           K DISK
004400940128     D*----------------------------------------------------*
004500940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
004600940224     D $ID             S             27    DIM(5) ALT($OZ)
004700940506     D* Schiere per la gestione dei tasti di funzione
004800940506     D $FM             S             27    DIM(24)
004900940506     D $FX             S             50    DIM(24)
005000940506     D $FC             S              1    DIM(24)
005100940506     D $FR             S              1    DIM(24)
005200940506     D* Video D1
005300940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005400940506     D $FX1            S             50    DIM(24) ALT($FM1)
005500940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005600940506     D $FR1            S              1    DIM(24) ALT($FC1)
005700940615     D* campi in lingua
005800940622$010 D LIN             S             06    DIM(0001) CTDATA PERRCD(001)
005900940622$010 D LIND            S             50    DIM(0001) ALT(LIN)
006000941129     D* Schiera campi
006100941129     D $D              S             30    DIM(10)                              Descriz.video
006200941129     D $C              S              4    DIM(10)                              Campi finestra
006300941202     D $P              S              2  0 DIM(10)                              Posiz.finestra
006400950214$003 D EXTDS         E DS                  EXTNAME(ANG0A1DS) INZ
006500941128     D*
006600950214     D TAB0A2        E DS                  EXTNAME(ANG0A2DS) INZ
006700950214     D TAB0A3        E DS                  EXTNAME(ANG0A3DS) INZ
006800940127     D* Passaggio Parametri
006900940127     D KPJBA         E DS
007000941103     D*-------------
007100941103      * DS Società
007200941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
007300941103     D*-------------
007400941103      * DS Interna per dati utente
007500941103     D XSOCDS          DS          1000
007600941103     D*-------------
007700940616     D XATBDS        E DS                  INZ
007800940318     D*-------------
007900940318     D* DS Esterna gestione messaggi per COMIT
008000940616     D XMSGDS        E DS                  INZ
008100940318     D  STS          E                     EXTFLD(MSGSTS)
008200940318     D                                     DIM(333)                                           bili
008300940318     D  JBA          E                     EXTFLD(MSGJBA)
008400940318     D                                     DIM(502)                                           bili
008500940203     D*-------------
008600940620     D* DS Esterna autorizzazioni tabelle
008700940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
008800940620     D*
008900940620     D* DS  restituzione autorizzazioni
009000940620     D* IND=indice
009100940620     D* GRU=man. di gruppo
009200940620     D* AZI=man. azienda
009300940712     D* ABI=abilit. ai riservati
009400941111     D AUTDS           DS
009500940620     D  MANIND                 1      1
009600940620     D  MANGRU                 2      2
009700940620     D  MANAZI                 3      3
009800940712     D  ABIRIS                 4      4
009900940620     D*-------------
010000940207     D* opzione richiesta
010100940614    >D D002DS          DS
010200940614     D  OPZ002                 1      2
010300940614     D* 0 = ritorno normale
010400940614     D* 1 = F3
010500940614     D* 2 = F12
010600940614     D  RET002                 3      3
010700940614     D* operazione eseguite dal pgm chiamato :
010800940614     D* 0 = nessuna operazione
010900940614     D* 1 = eseguito aggiornamento
011000940614     D  OPR002                 4      4
011100940614     D* funzione non eseguibile per errore :
011200940614     D* 0 = nessun errore
011300940614     D* 1 = funzione richiamata chiusa in errore
011400940614     D* Aggiungere eventuali altri codici di errore
011500940614     D  ERR002                 5      5
011600940614     D* Azienda
011700940614     D  AZI002                 6      8
011800940614     D* Lingua
011900940614     D  LIN002                 9      9
012000940614     D* Codice scelto
012100940614     D  KEY002                10     24
012200940620     D* Autorizzazione
012300940620     D  AUT002                25     25
012400940614     D*-------------
012500940201     D*-------------
012600940201     D DSFMT           DS
012700940506     D  $TASTO               369    369
012800940201     D  NRG                  370    370
012900940201     D  NCL                  371    371
013000940201     D*-------------
013100940201     D* posizione cursore
013200940201     D CURSOR          DS
013300940223     D  RIRI                   1      2B 0 INZ
013400940201     D  R$$                    2      2
013500940223     D  COCO                   3      4B 0 INZ
013600940201     D  C$$                    4      4
013700940207     D*-------------
013800940207     D* Reperimento nome PGM
013900940207     D STATUS         SDS           333
014000940207     D  DSPGM            *PROC
014100940225     D*-------------
014200940225     D* COSTANTI
014300940225     D*-------------
014400940506     D*
014500940506     D* Tasti di funzione
014600940506     D F01             C                   CONST(X'31')
014700940506     D F02             C                   CONST(X'32')
014800940506     D F03             C                   CONST(X'33')
014900940506     D F04             C                   CONST(X'34')
015000940506     D F05             C                   CONST(X'35')
015100940506     D F06             C                   CONST(X'36')
015200940506     D F07             C                   CONST(X'37')
015300940506     D F08             C                   CONST(X'38')
015400940506     D F09             C                   CONST(X'39')
015500940506     D F10             C                   CONST(X'3A')
015600940506     D F11             C                   CONST(X'3B')
015700940506     D F12             C                   CONST(X'3C')
015800940506     D F13             C                   CONST(X'B1')
015900940506     D F14             C                   CONST(X'B2')
016000940506     D F15             C                   CONST(X'B3')
016100940506     D F16             C                   CONST(X'B4')
016200940506     D F17             C                   CONST(X'B5')
016300940506     D F18             C                   CONST(X'B6')
016400940506     D F19             C                   CONST(X'B7')
016500940506     D F20             C                   CONST(X'B8')
016600940506     D F21             C                   CONST(X'B9')
016700940506     D F22             C                   CONST(X'BA')
016800940506     D F23             C                   CONST(X'BB')
016900940506     D F24             C                   CONST(X'BC')
017000940506     D ENTER           C                   CONST(X'F1')
017100940506     D ROLDWN          C                   CONST(X'F4')
017200940506     D ROLLUP          C                   CONST(X'F5')
017300940506     D*-------------
017400940506     D*
017500941108     D DATA            C                   CONST('0001-01-01')
017600940127     D*
017700941128     D*-------------
017800941128     D                 DS
017900941128     D  D1POS                  1      2
018000941128     D  D1KEY                  3      6
018100941128     D  D1COD                  1      6
018200941128     D*-------------
018300941128     D                 DS
018400941128     D  D1K                    1      4
018500941128     D  D1K1                   1      1
018600941128     D  D1K2                   2      2
018700941128     D  D1K3                   3      3
018800941128     D  D1K4                   4      4
018900941128     D* Parametri per ricerca codice ANATB00F
019000941128     D XTABDS          DS                  INZ
019100941128     D* '  ' = Gestione
019200941128     D* '01' = Ricerca
019300941128     D* '02' = Gestione
019400941128     D* '05' = Visualizzazione
019500941128     D* '11' = Abilitazione
019600941128     D  OPZTAB                 1      2
019700941128     D* 0 = ritorno normale
019800941128     D* 1 = F3
019900941128     D* 2 = F12
020000941128     D  RETTAB                 3      3
020100941128     D* operazione eseguite dal pgm chiamato :
020200941128     D* 0 = nessuna operazione
020300941128     D* 1 = eseguito aggiornamento
020400941128     D  OPRTAB                 4      4
020500941128     D* funzione non eseguibile per errore :
020600941128     D* 0 = nessun errore
020700941128     D* 1 = funzione richiamata chiusa in errore
020800941128     D* Aggiungere eventuali altri codici di errore
020900941128     D  ERRTAB                 5      5
021000941128     D* Azienda
021100941128     D  AZITAB                 6      8
021200941128     D* Lingua
021300941128     D  LINTAB                 9      9
021400941128     D* Codice scelto
021500941128     D  KEYTAB                10     24
021600941128     D* riga cursore chiamante
021700941128     D  RIGTAB                25     26  0
021800941128     D* colonna cursore chiamante
021900941128     D  COLTAB                27     28  0
022000951031     D* posizione colonna
022100951031     D  POSTAB                29     30
022200941129     D                 DS
022300941129     D  $A                     1     60
022400941129     D                                     DIM(30)                              Campi file
022500941129     D  $AX                    1     60
022600940128     I*----------------------------------------------------*
022700940615     ID1
022800950214$005 I              D1DES                       DES0A1
022900941129     IW1
023000941129$005 I              W1D01                       $D(1)
023100941129$005 I              W1D02                       $D(2)
023200941129$005 I              W1D03                       $D(3)
023300941129$005 I              W1D04                       $D(4)
023400941129$005 I              W1D05                       $D(5)
023500941129$005 I              W1D06                       $D(6)
023600941129$005 I              W1D07                       $D(7)
023700941129$005 I              W1D08                       $D(8)
023800941129$005 I              W1D09                       $D(9)
023900941129$005 I              W1D10                       $D(10)
024000941129$005 I              W1C01                       $C(1)
024100941129$005 I              W1C02                       $C(2)
024200941129$005 I              W1C03                       $C(3)
024300941129$005 I              W1C04                       $C(4)
024400941129$005 I              W1C05                       $C(5)
024500941129$005 I              W1C06                       $C(6)
024600941129$005 I              W1C07                       $C(7)
024700941129$005 I              W1C08                       $C(8)
024800941129$005 I              W1C09                       $C(9)
024900941129$005 I              W1C10                       $C(10)
025000940127     C*----------------------------------------------------*
025100940127     C*                MAIN LINE PROGRAM
025200940127     C*----------------------------------------------------*
025300940127     C*
025400940223     C* inizializzazione variabili
025500940223     C                   EXSR      INZVAR
025600940223     C*
025700940223     C     $FINE         DOWEQ     *OFF
025800940202     C     $GEST         CASEQ     'D1'          GESD1
025900940117     C                   END
026000940117     C                   END
026100940201     C*
026200940325     C* fine programma
026300940325     C                   EXSR      ENDPGM
026400940325     C/EJECT
026500940325     C************************************************************
026600940325     C* RICERCHE
026700940325     C************************************************************
026800940325     C     ENDPGM        BEGSR
026900940325     C*
027000940614     C* disallocazione rcd nel caso di
027100941130     C                   MOVE      SAVCOD        XTBCOD
027200940614    >C                   MOVE      '7'           XTBRIC
027300940614     C                   CALL      'XATB'
027400940614     C                   PARM                    XATBDS
027500940325     C*
027600940325     C* passaggio dati al pgm chiamante
027700940715     C                   MOVE      *BLANKS       KPJBU
027800940325    >C                   MOVEL     D002DS        KPJBU
027900940325     C*
028000940325     C                   SETON                                            RT
028100940325     C*
028200940325     C                   ENDSR
028300940325     C/EJECT
028400940201     C************************************************************
028500940201     C* RICERCHE
028600940201     C************************************************************
028700940127     C     SEARCH        BEGSR
028800940127     C*
028900940127     C* determino Riga/Colonna del cursore
029000940127     C                   MOVE      NRG           R$$
029100940127     C                   MOVE      NCL           C$$
029200940127     C                   Z-ADD     RIRI          CURR
029300940127     C                   Z-ADD     COCO          CURC
029400940127     C*
029500940224     C* I campi che al momento dell'F4 risultano protetti non devono
029600940224     C* essere abilitati alla funzione di ricerca
029700940224     C*
0298009401271    C                   SELECT
029900940127     C*
030000941128     C* FMTD1  - BYTES CHIAVE
030100941130    >C     H1NMFL        WHENEQ    'D1POS'
030200941129     C     *IN02         ANDEQ     *OFF
030300941129     C     *IN03         ANDEQ     *OFF
030400941129    >C     H1NMFL        OREQ      'W1C01'
030500941129    >C     H1NMFL        OREQ      'W1C02'
030600941129    >C     H1NMFL        OREQ      'W1C03'
030700941129    >C     H1NMFL        OREQ      'W1C04'
030800941129    >C     H1NMFL        OREQ      'W1C05'
030900941129    >C     H1NMFL        OREQ      'W1C06'
031000941129    >C     H1NMFL        OREQ      'W1C07'
031100941129    >C     H1NMFL        OREQ      'W1C08'
031200941129    >C     H1NMFL        OREQ      'W1C09'
031300941129    >C     H1NMFL        OREQ      'W1C10'
031400941128     C                   MOVE      '01'          OPZTAB
031500941128     C                   MOVE      *ZEROS        KEYTAB
031600941128     C                   MOVE      XSCSOC        AZITAB
031700941128     C                   MOVE      XSCLIN        LINTAB
031800951031     C     H1NMFL        IFEQ      'W1C01'
031900951031     C                   MOVE      $P(1)         POSTAB
032000951031     C                   END
032100951031     C     H1NMFL        IFEQ      'W1C02'
032200951031     C                   MOVE      $P(2)         POSTAB
032300951031     C                   END
032400951031     C     H1NMFL        IFEQ      'W1C03'
032500951031     C                   MOVE      $P(3)         POSTAB
032600951031     C                   END
032700951031     C     H1NMFL        IFEQ      'W1C04'
032800951031     C                   MOVE      $P(4)         POSTAB
032900951031     C                   END
033000951031     C     H1NMFL        IFEQ      'W1C05'
033100951031     C                   MOVE      $P(5)         POSTAB
033200951031     C                   END
033300951031     C     H1NMFL        IFEQ      'W1C06'
033400951031     C                   MOVE      $P(6)         POSTAB
033500951031     C                   END
033600951031     C     H1NMFL        IFEQ      'W1C07'
033700951031     C                   MOVE      $P(7)         POSTAB
033800951031     C                   END
033900951031     C     H1NMFL        IFEQ      'W1C08'
034000951031     C                   MOVE      $P(8)         POSTAB
034100951031     C                   END
034200951031     C     H1NMFL        IFEQ      'W1C09'
034300951031     C                   MOVE      $P(9)         POSTAB
034400951031     C                   END
034500951031     C     H1NMFL        IFEQ      'W1C10'
034600951031     C                   MOVE      $P(10)        POSTAB
034700951031     C                   END
034800941128     C                   CLEAR                   KPJBU
034900941128     C                   MOVEL     XTABDS        KPJBU
035000951031     C                   CALL      'ANG0A1R'
035100941128     C                   PARM                    KPJBA
035200941128     C                   MOVEL     KPJBU         XTABDS
035300951031     C                   MOVE      KEYTAB        WWCXX
035400951031     C     WWCXX         IFNE      *BLANKS
035500951031     C     WWCXX         ANDNE     *zeros
035600951031     C                   SELECT
035700951031    >C     H1NMFL        WHENEQ    'W1C01'
035800951031     C                   MOVE      WWCXX         $C(1)
035900951031    >C     H1NMFL        WHENEQ    'W1C02'
036000951031     C                   MOVE      WWCXX         $C(2)
036100951031    >C     H1NMFL        WHENEQ    'W1C03'
036200951031     C                   MOVE      WWCXX         $C(3)
036300951031    >C     H1NMFL        WHENEQ    'W1C04'
036400951031     C                   MOVE      WWCXX         $C(4)
036500951031    >C     H1NMFL        WHENEQ    'W1C05'
036600951031     C                   MOVE      WWCXX         $C(5)
036700951031    >C     H1NMFL        WHENEQ    'W1C06'
036800951031     C                   MOVE      WWCXX         $C(6)
036900951031    >C     H1NMFL        WHENEQ    'W1C07'
037000951031     C                   MOVE      WWCXX         $C(7)
037100951031    >C     H1NMFL        WHENEQ    'W1C08'
037200951031     C                   MOVE      WWCXX         $C(8)
037300951031    >C     H1NMFL        WHENEQ    'W1C09'
037400951031     C                   MOVE      WWCXX         $C(9)
037500951031    >C     H1NMFL        WHENEQ    'W1C10'
037600951031     C                   MOVE      WWCXX         $C(10)
037700951031     C                   ENDSL
037800951031     C                   WRITE     W1
037900941227     C                   ENDIF
038000941128     C*
038100940223     C* ricerca non abilitata in questa posizione
0382009402231O   C                   OTHER
038300941005     C                   SETON                                        9899
038400940127     C*
0385009401271-   C                   ENDSL
038600940127     C*
038700951031     C* imposto pos. del cursore
038800951031     C                   EVAL      H1RIGA = CURR - 8
038900951031     C                   EVAL      H1COLO = CURC - 4 - 1
039000940506     C                   MOVE      *HIVAL        $LASTV
039100940127     C                   ENDSR
039200940615     C/EJECT
039300940615     C************************************************************
039400940615     C* TRADUZIONI
039500940615     C************************************************************
039600940615     C     LINGUA        BEGSR
039700940615     C*
039800940615     C* determino Riga/Colonna del cursore
039900940615     C                   MOVE      NRG           R$$
040000940615     C                   MOVE      NCL           C$$
040100940615     C                   Z-ADD     RIRI          CURR
040200940615     C                   Z-ADD     COCO          CURC
040300940615     C*
040400940615     C*
040500940615     C*
040600940615     C* FMTD1  - CAMPI DA TRADURRE
040700940615     C* imposto pos. del cursore
040800940615     C                   Z-ADD     CURR          H1RIGA
040900940615     C                   Z-ADD     CURC          H1COLO
041000940621     C                   MOVEL     H1NMFL        W005              5
041100940621     C                   MOVE      W005          W003              3
041200940621     C                   MOVEL     W003          W006              6
041300940620     C                   MOVE      *ZERO         KEY              15
041400940616     C                   MOVE      D1COD         KEY
041500940615     C                   SELECT
041600940621$014 C     W003          WHENEQ    'DES   '
041700940621$014 C                   Z-ADD     1             I                 3 0
041800940621$014 C     W006          LOOKUP    LIN(I)                                 21
041900940621$014 C                   CALL      'ANA101R'
042000940621$014 C                   PARM      W003          CAMPO            10
042100950214$014 C                   PARM      '0A1'         COD               3
042200940621$014 C                   PARM                    KEY              15
042300940621$014 C                   PARM      030           LUN               3 0
042400940621$014 C                   PARM      001           POS               3 0
042500950214$014 C     DES0A1        PARM      DES0A1        DES              50
042600940621$014 C                   PARM      LIND(I)       FLD              50
042700940621$014 C                   PARM      XTBAZI        AZI               3
042800940621$014 C                   PARM      LIN002        LNG               1
042900940621$014 C                   PARM      OPZ002        OPZ               2
043000940615     C*
043100940615     C*
043200940615     C* ricerca non abilitata in questa posizione
0433009406151O   C                   OTHER
043400940615     C                   SETON                                        98
043500940615     C*
0436009406151-   C                   ENDSL
043700940615     C*
043800940615     C                   MOVE      *HIVAL        $LASTV
043900940615     C                   ENDSR
044000940225     C/EJECT
044100940225     C************************************************************
044200940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
044300940225     C************************************************************
044400940225     C     GESPIE        BEGSR
044500940225     C*
044600940225     C                   SELECT
044700940225     C*
044800940506     C     $GEST         WHENEQ    'D1'
044900940506     C     $ULKD1        IFEQ      0
045000940506     C* riempimento opzioni
045100940506     C                   EXSR      F24D1
045200940506     C                   ELSE
045300940614     C                   WRITE     Z1
045400940506     C                   ENDIF
045500940225     C*
045600940510     C*
045700940225     C                   ENDSL
045800940225     C*
045900940225     C                   ENDSR
046000940131     C/EJECT
046100940131     C************************************************************
046200940510     C* GESTIONE VIDEO RECORD D1
046300940131     C************************************************************
046400940127     C     GESD1         BEGSR
046500940225     C* inizializzazione videata
046600940223     C     $INZD1        IFEQ      *ON
046700940127     C                   EXSR      INZD1
046800940223     C                   MOVE      *OFF          $INZD1
046900940117     C                   END
047000940225     C* emissione piede videata se proveniente da altra
047100940225     C* salvataggio valore formato di provenienza
047200940207     C     $LASTV        IFNE      'D1'
047300940614     C                   WRITE     T1
047400940225     C                   EXSR      GESPIE
047500940202     C                   MOVE      $LASTV        $LASTG
047600940202     C                   MOVE      'D1'          $LASTV
047700940202     C                   END
047800940131     C*
047900940203     C                   SELECT
048000940203     C* Annullamento
048100940315    >C     OPZ002        WHENEQ    '04'
048200940224     C* Visualizzazione
048300940315    >C     OPZ002        OREQ      '05'
048400940620     C                   SETON                                        0203
048500940620     C* Manutenzione
048600940620    >C     OPZ002        WHENEQ    '02'
048700940620     C                   SETON                                        02
048800940620     C                   SETOFF                                       03
048900940620     C*
049000940620     C                   OTHER
049100940620     C*
049200940620     C                   SETOFF                                       0203
049300940203     C                   ENDSL
049400940620     C* Immissione/Modifica/Copia
049500940620     C                   EXFMT     D1
049600940506     C* controllo abilitazione tasti funzione
049700940506     C*
049800940506     C                   EXSR      CTRKD1
049900940506     C*
050000940506     C     *IN99         IFEQ      *OFF
050100940131     C*
0502009402031    C                   SELECT
050300940510     c* ROLLUP
050400940131     C* F3=Fine
050500940506     C     $TASTO        WHENEQ    F03
050600940309     C                   EXSR      F03D1
050700940131     C* F4=Ricerca
050800940506     C     $TASTO        WHENEQ    F04
050900940127     C                   EXSR      SEARCH
051000940224     C* F5=Refresh
051100940506     C     $TASTO        WHENEQ    F05
051200940309     C                   EXSR      F05D1
051300940223     C* F8=Successivo
051400940506     C     $TASTO        WHENEQ    F08
051500940309     C                   EXSR      F08D1
051600940131     C* F12=Ritorno
051700940506     C     $TASTO        WHENEQ    F12
051800940309     C                   EXSR      F12D1
051900940315     C* F24=Altri tasti
052000940506     C     $TASTO        WHENEQ    F24
052100940315     C                   EXSR      F24D1
052200940131     C*
0523009402031O   C                   OTHER
052400940127     C                   EXSR      CTRD1
052500940616     C     *IN99         IFEQ      *OFF
052600940616     C                   SELECT
052700940616     C* F7=Traduzione
052800940616     C     $TASTO        WHENEQ    F07
052900940616     C                   EXSR      LINGUA
053000950504     C* F6 (emetto il padre o i padri se ultimo di più catene)
0531009406162    C     $TASTO        WHENEQ    F06
053200950504     C* Non faccio vedere il padre se siamo in cancellazione
053300950504     C     OPZ002        IFNE      '04'
053400941129     C                   EXSR      CARW1
053500941130     C* Se $Z = 0 il campo è il primo della catena
053600941130     C     $Z            IFEQ      0
053700941130     C                   GOTO      ENDW1
053800941130     C                   END
053900941129     C* Emetto finestra
054000941129     C     W1EXF         TAG
054100941129     C                   EXFMT     W1
054200941130     C   KL              GOTO      ENDW1
054300941130     C   KD              EXSR      SEARCH
054400941129     C                   EXSR      CTRW1
054500941130     C   KD
054600941130     CORNKF
054700941129     COR 99              GOTO      W1EXF
054800941129     C     ENDW1         TAG
054900950504     C                   ENDIF
055000941129     C* eseguo aggiornamento
055100941129     C     *INKF         IFEQ      *ON
055200940315    >C                   EXSR      AGGANA
055300941129     C                   END
055400940224     C* eseguo operazioni del dopo-aggiornamento
0555009403183    C     *IN99         IFEQ      *OFF
055600941130     C     *INKF         ANDEQ     *ON
055700940318     C                   EXSR      GESAGG
0558009403183-   C                   ENDIF
0559009406162-   C                   ENDSL
056000940616     C                   ENDIF
056100940203     C*
0562009402241-   C                   ENDSL
056300940224     C*
0564009405062-   C                   ENDIF
056500940117     C                   ENDSR
056600940506     C/EJECT
056700940506     C************************************************************
056800940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
056900940506     C************************************************************
057000940506     C     CTRKD1        BEGSR
057100940506     C                   SETOFF                                       99
057200940506     C                   MOVEA     $FC1          $FC
057300940506     C                   EXSR      CTRKEY
057400940506     C     *IN99         IFEQ      *ON
057500940506     C                   SETON                                        97
057600940506     C                   ENDIF
057700940506     C                   ENDSR
057800940117      ************************************************************
057900940207      * INIZIALIZZAZIONE VIDEATA DATI
058000940117      ************************************************************
058100940127     C     INZD1         BEGSR
058200940131     C*
058300940615     C                   CLEAR                   D1
058400941130     C                   CLEAR                   W1
058500941006     C                   MOVE      '0'           D1RIS
058600940616     C                   MOVE      KEY002        D1COD
058700940615     C                   MOVE      AZI002        XTBAZI
058800940615     C                   MOVE      LIN002        XTBLIN
058900940616     C                   MOVE      *ALL'0'       XTBKEY
059000940615     C                   MOVE      KEY002        XTBKEY
059100950214$007 C                   MOVE      '0A1'         SAVCOD
059200940225     C*
059300940207     C*
059400940224     C* valorizzo la chiave univoca per reperire i dati
059500940224     C* se non immissione
0596009403151   >C     OPZ002        IFNE      '01'
059700940315    >C*-------------------------------------------------------------
059800940315    >C* impostare i campi chiave ricevuti
059900940315    >C*-------------------------------------------------------------
060000940318     C*
060100940614     C*
060200940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0603009406142   >C     OPZ002        IFEQ      '02'
060400940315    >C     OPZ002        OREQ      '04'
060500940614     C                   MOVE      '1'           XTBALC
060600940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0607009406142E   C                   ELSE
060800940614     C                   MOVE      '0'           XTBALC
0609009406142-   C                   ENDIF
061000940614     C*
061100941130     C                   MOVE      SAVCOD        XTBCOD
061200940616     C                   MOVE      '1'           XTBRIC
061300940614     C                   CALL      'XATB'
061400940614     C                   PARM                    XATBDS
061500940318     C*
061600940614     C     XTBERR        IFEQ      '0'
061700940614     C                   MOVEL     XTBUNI        EXTDS
061800950214     C                   MOVE      CD10A1        $C(1)
061900950214     C                   MOVE      CD20A1        $C(2)
062000950214     C                   MOVE      CD30A1        $C(3)
062100950214     C                   MOVE      CD40A1        $C(4)
062200950214     C                   MOVE      CD50A1        $C(5)
062300950214     C                   MOVE      CD60A1        $C(6)
062400950214     C                   MOVE      CD70A1        $C(7)
062500950214     C                   MOVE      CD80A1        $C(8)
062600950214     C                   MOVE      CD90A1        $C(9)
062700950214     C                   MOVE      CD00A1        $C(10)
062800950214     C                   MOVE      PS10A1        $P(1)
062900950214     C                   MOVE      PS20A1        $P(2)
063000950214     C                   MOVE      PS30A1        $P(3)
063100950214     C                   MOVE      PS40A1        $P(4)
063200950214     C                   MOVE      PS50A1        $P(5)
063300950214     C                   MOVE      PS60A1        $P(6)
063400950214     C                   MOVE      PS70A1        $P(7)
063500950214     C                   MOVE      PS80A1        $P(8)
063600950214     C                   MOVE      PS90A1        $P(9)
063700950214     C                   MOVE      PS00A1        $P(10)
063800941108     C* Giro la data se non blank
063900941108     C     XTBDSC        IFNE      DATA
064000941108     C                   CALL      'XDT4000'
064100941108     C                   PARM      XTBDSC        XDTAMG           10
064200940621     C                   PARM      *ZEROS        XDTGMA            6 0
064300940621     C                   PARM      2             XDTSTA            1 0
064400940621     C                   MOVE      XDTGMA        D1DSC
064500940712     C                   MOVE      XTBRIS        D1RIS
064600941108     C                   ELSE
064700941108     C                   MOVE      *ZEROS        D1DSC
064800941108     C                   ENDIF
064900090504>>>>>C                   MOVE      XTBRIS        D1RIS
065000941108     C                   ENDIF
065100940614     C*
065200940224     C*
065300940224     C* se il rcd da gestire non viene trovato
065400940318     C* oppure è allocato da altro pgm
065500940224     C* ritorno al pgm guida avvertendolo dell'errore
0656009406142    C     XTBERR        IFNE      '0'
065700940224     C                   MOVE      *ON           $FINE
065800940315    >C                   MOVE      '1'           ERR002
065900940224     C*
066000940224     C* altrimenti
0661009402242E   C                   ELSE
066200940224     C* riempio la videata
066300940224     C                   EXSR      RIED1
066400940224     C* richiamo routine dei ctrl per decodificarla
066500940614    >C                   EXSR      CTRD1
066600940224     C                   MOVE      *ALL'0'       IN5098
066700940224     C                   MOVEA     IN5098        *IN(50)
066800940224     C* definizione indicatori su video
066900940224     C                   SETOFF                                       02
0670009402243    C                   SELECT
067100940315    >C     OPZ002        WHENEQ    '02'
067200940224     C                   SETON                                        02        PR campi key
067300940315    >C     OPZ002        WHENEQ    '05'
067400940506     C* disabilita tasti
067500940720     C                   MOVE      '0'           $FC(6)
067600940720     C                   MOVE      '0'           $FC(5)
067700940720     C                   MOVE      '0'           $FC(4)
0678009402243-   C                   ENDSL
067900940224     C*
0680009402242-   C                   ENDIF
068100940223     C*
0682009402241-   C                   ENDIF
068300940224     C*
068400940117     C                   ENDSR
068500940207     C/EJECT
068600940207     C************************************************************
068700940510     C* RIEMPIMENTO VIDEATA  D1
068800940207     C************************************************************
068900940207     C     RIED1         BEGSR
069000940207     C*
069100940207     C*
069200940207     C                   ENDSR
069300940309     C/EJECT
069400940309     C************************************************************
069500940309     C* GESTIONE F03 VIDEO D1
069600940309     C************************************************************
069700940309     C     F03D1         BEGSR
069800940309     C*
069900940309     C                   MOVE      *ON           $FINE
070000940315    >C                   MOVE      '1'           RET002
070100940325     C* fine programma
070200940325     C                   EXSR      ENDPGM
070300940325     C*
070400940325     C                   ENDSR
070500940309     C/EJECT
070600940309     C************************************************************
070700940309     C* GESTIONE F05 VIDEO D1
070800940309     C************************************************************
070900940309     C     F05D1         BEGSR
071000940309     C*
071100940309     C                   MOVE      *ON           $INZD1
071200940309     C*
071300940309     C                   ENDSR
071400940309     C/EJECT
071500940309     C************************************************************
071600940309     C* GESTIONE F08 VIDEO D1
071700940309     C************************************************************
071800940309     C     F08D1         BEGSR
071900940309     C*
072000940309     C                   MOVE      *ON           $FINE
072100940315    >C                   MOVE      '0'           RET002
072200940309     C*
072300940309     C                   ENDSR
072400940309     C/EJECT
072500940309     C************************************************************
072600940309     C* GESTIONE F12 VIDEO D1
072700940309     C************************************************************
072800940309     C     F12D1         BEGSR
072900940309     C*
073000940309     C                   MOVE      *ON           $FINE
073100940315    >C                   MOVE      '2'           RET002
073200940309     C*
073300940309     C                   ENDSR
073400940309     C/EJECT
073500940309     C************************************************************
073600940309     C* GESTIONE F24 VIDEO D1
073700940309     C************************************************************
073800940309     C     F24D1         BEGSR
073900940309     C*
074000940506     C*
074100940506     C                   MOVEA     $FX1          $FX
074200940506     C                   MOVEA     $FC1          $FC
074300940506     C                   MOVEA     $FR1          $FR
074400940506     C                   Z-ADD     $ULKD1        $ULK
074500940506     C                   EXSR      RIEKEY
074600941017     C                   MOVEA     $FC           $FC1
074700940506     C                   Z-ADD     $ULK          $ULKD1            3 0
074800940506     C                   MOVEL     $KEY1         Z1KE1
074900060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
075000940506     C                   MOVEL     $KEY2         Z1KE2
075100940614     C                   WRITE     Z1
075200940506     C*
075300940309     C                   ENDSR
075400940207     C/EJECT
075500940131     C************************************************************
075600940207     C* CONTROLLO VIDEATA
075700940131     C************************************************************
075800940127     C     CTRD1         BEGSR
075900940131     C*
076000940127     C                   SETOFF                                       99
076100940127     C* non imposto pos. del cursore
076200940127     C                   Z-ADD     0             H1RIGA
076300940127     C                   Z-ADD     0             H1COLO
076400941109     C*
076500941109     C* controllo codice vuoto
076600941128$017 C     D1COD         IFEQ      *ALL' '
076700941109     C                   SETON                                        5099
076800941109     C                   ENDIF
076900941128     C* controllo codice vuoto
077000941128$017 C     D1KEY         IFEQ      *ALL' '
077100941128     C                   SETON                                        5099
077200941128     C                   ENDIF
077300940621     C*
077400940621     C* CTRL DATA
077500940621     C*
077600941109     C                   MOVE      DATA          WRKDSC           10
077700940621     C     D1DSC         IFNE      0
077800941108     C                   CALL      'XDT4000'
077900941109     C                   PARM                    XDTAMG           10
078000940621     C                   PARM      D1DSC         XDTGMA            6 0
078100940621     C                   PARM      3             XDTSTA            1 0
078200940621     C     XDTSTA        IFNE      0
078300940621     C                   SETON                                        5599
078400940621     C                   ELSE
078500941108     C                   MOVE      XDTAMG        WRKDSC
078600940621     C                   ENDIF
078700940621     C                   ENDIF
078800940223     C*
078900940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
079000940223     C*
0791009403151   >C     OPZ002        IFNE      '04'
079200950214$009 C     DES0A1        IFEQ      *BLANK
079300940614$009 C                   SETON                                          6099
079400940614$009 C                   ENDIF
079500941129     C* Controllo byte
079600941130     C     *IN99         IFEQ      *OFF
079700941129     C                   MOVE      *ZERO         XTBKEY
079800941129     C                   MOVE      D1POS         XTBKEY                         *
079900941129     C                   MOVEL     D1KEY         D1K
080000941129     C                   EXSR      CTRBYT
080100941129     C   99              SETON                                        51
080200941130     C                   END
080300941128     C*
080400940207     C*
080500940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
080600940315    >C     OPZ002        IFEQ      '01'
080700940315    >C     OPZ002        OREQ      '03'
080800940315    >C*-------------------------------------------------------------
080900940315    >C* impostare i campi chiave e verificare l'esistenza del record
081000940315    >C*-------------------------------------------------------------
081100940614     C                   MOVE      *ALL'0'       XTBKEY
081200940620    >C                   MOVE      D1COD         XTBKEY
081300940614     C                   MOVE      '0'           XTBALC
081400941130     C                   MOVE      SAVCOD        XTBCOD
081500940614     C                   MOVE      '1'           XTBRIC
081600940614    >C                   CALL      'XATB'
081700940614     C                   PARM                    XATBDS
081800940614     C     XTBERR        IFEQ      '0'
081900940223     C                   SETON                                        5999
082000940223     C                   ENDIF
082100940620     C* Controllo se compreso nei range indice
082200940620     C     XTBERR        IFEQ      '3'
082300940620    >C                   SETON                                        5899
082400940620     C                   END
082500940620     C*
082600940620     C* REPERISCO AUTORIZZAZIONI UTENTE
082700940620     C                   CLEAR                   XCHATB
082800941118     C                   MOVE      XSCSOC        XCTSOC
082900941118     C                   MOVE      XSCDUN        XCTUNI
083000940620     C                   MOVEL     KNMUS         XCTPRF
083100941130     C                   MOVE      SAVCOD        XCTCOD
083200940620     C                   CALL      'XCHKATB'
083300940620     C                   PARM                    XCHATB
083400941111     C                   MOVEL     XCTDDS        AUTDS
083500941027     C     XCTRTN        IFEQ      1
083600941027     C                   MOVEL     'N'           MANGRU
083700941027     C                   ENDIF
083800940620     C* Non ammessa immissione di gruppo
083900940620     C     XTBGRU        IFEQ      'G'
084000940620     C     MANGRU        ANDEQ     'N'
084100940620     C                   SETON                                        5799
084200940620     C                   END
084300940620     C* Non ammessa immissione di azienda
084400940620     C     XTBGRU        IFEQ      'A'
084500940620     C     MANAZI        ANDEQ     'N'
084600940620     C                   SETON                                        5699
084700940620     C                   END
084800940712     C* codice riservato non accessibile
084900941006     C     XTBRIS        IFGT      ABIRIS
085000941005    >C     OPZ002        ANDNE     '01'
085100941005    >C     OPZ002        ANDNE     '03'
085200940712     C                   SETON                                        5499
085300940712     C                   END
085400940224     C                   ENDIF
085500940224     C*
0856009402241-   C                   END
085700940131     C*
085800940302     C     *IN99         IFEQ      *ON
085900940510     C     $GEST         ANDEQ     'D1'
086000940117     C                   MOVEA     *IN           WIN
086100940128     C                   MOVE      *ALL'0'       IN5098           49
086200940128     C                   MOVEA     IN5098        *IN(50)
086300940614     C                   WRITE     D1
086400940117     C                   MOVEA     WIN           *IN
086500940302     C                   ENDIF
086600950504     C*
086700950504     C* SE ANNULLAMENTO CONTROLLO CHE IL CODICE DA ANNULLARE
086800950504     C* NON SIA a sua volta il padre di un altro codice, deve essere
086900950504     C* cioè l'ultimo della catena
087000950504     C     OPZ002        IFEQ      '04'
087100950504     C*
087200950504     C* Cerco nel file delle catene
087300950504     C                   MOVE      XSCSOC        XTBAZI
087400950504     C                   MOVE      XSCLIN        XTBLIN
087500950504     C                   MOVE      *ALL'0'       XTBKEY
087600950504     C                   MOVE      '0'           XTBALC
087700950504     C                   MOVE      '2'           XTBRIC
087800950504     C                   MOVE      '0A3'         XTBCOD
087900950504     C                   MOVE      XSCCUT        XTBUTE
088000950504    >C                   CALL      'XATB'
088100950504     C                   PARM                    XATBDS
088200950504   B1C     XTBERR        DOWEQ     '0'
088300950504     C                   MOVE      XTBKEY        SAVKEY
088400950504     C                   MOVEL     XTBUNI        TAB0A3
088500950504     C                   MOVEL     TAB0A3        $AX
088600950504     C                   Z-ADD     1             $X                3 0
088700950504     C     D1POS         LOOKUP    $A($X)                                 13
088800950504   B2C     *IN13         IFEQ      *ON
088900950504     C     $X            IFNE      30
089000950504     C                   ADD       1             $X
089100950504     C     $A($X)        IFNE      *ZEROS
089200950504     C                   SETON                                        8199
089300950504     C                   ENDIF
089400950504     C                   ENDIF
089500950504     C                   ENDIF
089600950504     C*
089700950504     C                   MOVE      '3'           XTBRIC
089800950504    >C                   CALL      'XATB'
089900950504     C                   PARM                    XATBDS
090000950504     C                   ENDDO
090100950504     C                   ENDIF
090200950504     C*
090300940127     C*
090400940117     C                   ENDSR
090500940207     C/EJECT
090600941129     C************************************************************
090700941129    >C* CONTROLLO BYTE
090800941129     C************************************************************
090900941129    >C     CTRBYT        BEGSR
091000941129     C*  Vedo quanti caratteri sono ammessi
091100941129     C                   MOVE      '1'           XTBRIC
091200941129     C                   MOVE      XSCSOC        XTBAZI
091300941130     C                   MOVE      '0'           XTBALC
091400950214     C                   MOVE      '0A2'         XTBCOD
091500941129     C                   CALL      'XATB'
091600941129     C                   PARM                    XATBDS
091700941130     C     XTBERR        IFEQ      *ON
091800941130     C                   SETON                                        99
091900941130     C                   ELSE
092000950214     C                   MOVEL     XTBUNI        TAB0A2
092100950214     C     BYT0A2        IFEQ      1
092200941129     C     D1K2          IFNE      *BLANKS
092300941129     C     D1K3          ORNE      *BLANKS
092400941129     C     D1K4          ORNE      *BLANKS
092500941129     C                   SETON                                          99
092600941129     C                   END
092700941129     C                   END
092800941129     C*
092900950214     C     BYT0A2        IFEQ      2
093000941129     C     D1K3          IFNE      *BLANKS
093100941129     C     D1K4          ORNE      *BLANKS
093200941129     C                   SETON                                          99
093300941129     C                   END
093400941129     C                   END
093500941129     C*
093600950214     C     BYT0A2        IFEQ      3
093700941129     C     D1K4          ANDNE     *BLANKS
093800941129     C                   SETON                                          99
093900941129     C                   END
094000941129     C*
094100941129     C                   END
094200941129     C                   ENDSR
094300941129     C/EJECT
094400940224     C************************************************************
094500940315    >C* AGGIORNAMENTO ANAGRAFICA
094600940224     C************************************************************
094700940315    >C     AGGANA        BEGSR
094800940614     C                   MOVE      *ALL'0'       XTBKEY
094900940614     C                   MOVE      D1COD         XTBKEY
095000941130     C                   MOVE      SAVCOD        XTBCOD
095100940207     C*
095200940207     C                   SELECT
095300940315    >C     OPZ002        WHENEQ    '01'
095400940315    >C     OPZ002        OREQ      '03'
095500941202    >C                   EXSR      RIEANA
095600940712     C                   MOVE      XSCCUT        XTBUTE
095700940614     C                   MOVE      '5'           XTBRIC
095800940614     C                   CALL      'XATB'
095900940614    >C                   PARM                    XATBDS
096000940318     C* se rcd non scrivibile attivo errore
096100940615     C     XTBERR        IFEQ      '1'
096200940318    >C                   SETON                                        5999
096300940318     C                   END
096400940315    >C     OPZ002        WHENEQ    '02'
096500940621     C                   MOVE      '1'           XTBRIC
096600940621     C                   MOVE      '1'           XTBALC
096700940621     C                   CALL      'XATB'
096800940621     C                   PARM                    XATBDS
096900940622    >C                   EXSR      RIEANA
097000940712     C                   MOVE      XSCCUT        XTBUTE
097100940614     C                   MOVE      '4'           XTBRIC
097200940614     C                   CALL      'XATB'
097300940614    >C                   PARM                    XATBDS
097400940315    >C     OPZ002        WHENEQ    '04'
097500940621     C                   MOVE      '1'           XTBRIC
097600940621     C                   MOVE      '1'           XTBALC
097700940621     C                   CALL      'XATB'
097800940621     C                   PARM                    XATBDS
097900940614     C                   MOVE      '6'           XTBRIC
098000940614     C                   CALL      'XATB'
098100940614    >C                   PARM                    XATBDS
098200940621     C* annullo traduzioni
098300940621     C     XTBERR        IFEQ      '0'
098400940621     C                   EXSR      ANNTBT
098500940621     C                   END
098600940621     C*
098700940207     C                   ENDSL
098800940203     C*
098900940203     C                   ENDSR
099000940224     C/EJECT
099100940224     C************************************************************
099200940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
099300940224     C************************************************************
099400940616     C     RIEANA        BEGSR
099500940616     C                   CLEAR                   XTBUNI
099600940621     C                   MOVE      WRKDSC        XTBDSC
099700940712     C                   MOVE      D1RIS         XTBRIS
099800950214     C                   MOVE      $C(1)         CD10A1
099900950214     C                   MOVE      $C(2)         CD20A1
100000950214     C                   MOVE      $C(3)         CD30A1
100100950214     C                   MOVE      $C(4)         CD40A1
100200950214     C                   MOVE      $C(5)         CD50A1
100300950214     C                   MOVE      $C(6)         CD60A1
100400950214     C                   MOVE      $C(7)         CD70A1
100500950214     C                   MOVE      $C(8)         CD80A1
100600950214     C                   MOVE      $C(9)         CD90A1
100700950214     C                   MOVE      $C(10)        CD00A1
100800950214     C                   MOVE      $P(1)         PS10A1
100900950214     C                   MOVE      $P(2)         PS20A1
101000950214     C                   MOVE      $P(3)         PS30A1
101100950214     C                   MOVE      $P(4)         PS40A1
101200950214     C                   MOVE      $P(5)         PS50A1
101300950214     C                   MOVE      $P(6)         PS60A1
101400950214     C                   MOVE      $P(7)         PS70A1
101500950214     C                   MOVE      $P(8)         PS80A1
101600950214     C                   MOVE      $P(9)         PS90A1
101700950214     C                   MOVE      $P(10)        PS00A1
101800940616     C                   MOVEL     EXTDS         XTBUNI
101900940207     C                   ENDSR
102000940224     C/EJECT
102100940224     C************************************************************
102200940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
102300940224     C************************************************************
102400940224     C     GESAGG        BEGSR
102500940224     C*
102600940224     C* segnalo al pgm chiamante l'avvenuta conferma
102700940315    >C                   MOVE      *ON           OPR002
102800940224     C* segnalo al pgm chiamante l'aver premuto l'F6
102900940315    >C                   MOVE      '0'           RET002
103000940224     C*
1031009402241    C                   SELECT
103200940224     C* nel caso di immissione, ripristino la videata iniziale
103300940315    >C     OPZ002        WHENEQ    '01'
103400940224     C                   MOVE      *ON           $INZD1
103500940224     C* altrimenti ritorno al pgm chiamante
103600940224     C                   OTHER
103700940224     C                   MOVE      *ON           $FINE
1038009402241-   C                   ENDSL
103900940224     C*
104000940224     C                   ENDSR
104100940621     C/EJECT
104200940621     C************************************************************
104300940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
104400940621     C************************************************************
104500940621    >C     ANNTBT        BEGSR
104600940621     C                   MOVE      XTBAZI        TBTAZI
104700941130     C                   MOVE      SAVCOD        TBTCOD
104800940621     C                   MOVE      XTBKEY        TBTKEY
104900940621     C     KTBT          SETLL     ANTBT01L
105000940621     C                   DO        *HIVAL
105100940621     C     KTBT          DELETE    ANTBT01L                           21
105200940621     C  N21              END
105300940621     C*
105400940621     C                   ENDSR
105500940207     C/EJECT
105600940131     C************************************************************
105700940131     C* OPERAZIONI INIZIALI
105800940131     C************************************************************
105900940131     C     *INZSR        BEGSR
106000940131     C*
106100940127     C* Reperimento parametri
106200940127     C*
106300940117     C     *ENTRY        PLIST
106400940117     C                   PARM                    KPJBA
106500940712     C*
106600940712     C*
106700940223     C*
106800940506     C*
106900940506     C* Reperimento tasti di funzione
107000940506     C*
107100940506     C                   EXSR      REPKEY
107200940621     C*
107300940621     C* KLIST
107400940621     C*
107500940621     C     KTBT          KLIST
107600940621     C                   KFLD                    TBTAZI
107700940621     C                   KFLD                    TBTCOD
107800940621     C                   KFLD                    TBTKEY
107900940506     C*
108000940223     C                   ENDSR
108100941103     C/EJECT
108200941103      ************************************************************
108300941103      * Reperimento dati società
108400941103      ************************************************************
108500941103     C     REPSOC        BEGSR
108600941103      *
108700941103     C                   CALL      'XSOC'
108800941103     C                   PARM                    TIPXSC            6
108900941103     C                   PARM                    SOCXSC            3
109000941103     C                   PARM                    CDSXSC            9 0
109100941103     C                   PARM                    MODXSC            3
109200941103     C                   PARM      *BLANKS       RTNXSC            1
109300941103     C                   PARM                    XSOCDS
109400941103     C                   PARM                    KPJBA
109500941103      *
109600941103     C                   ENDSR
109700940223     C/EJECT
109800940506     C************************************************************
109900940506     C* REPERIMENTO TASTI DI FUNZIONE
110000940506     C************************************************************
110100940506     C     REPKEY        BEGSR
110200940506     C                   MOVEA     $FM1          $FM
110300940506     C                   EXSR      REPKKK
110400940506     C                   MOVEA     $FX           $FX1
110500940614     C                   ENDSR
110600940506     C/EJECT
110700940506     C************************************************************
110800940506     C* REPERIMENTO TASTI DI FUNZIONE
110900940506     C************************************************************
111000940506     C     REPKKK        BEGSR
111100940506     C*
111200940506     C                   CLEAR                   $FX
111300940506     C     1             DO        24            X
111400940506     C*
111500940506     C     $FM(X)        IFNE      *BLANK
111600940506     C*
111700940506     C                   MOVEL     $FM(X)        IDMSG
111800940506     C                   CALL      'XRTVM'
111900940506     C                   PARM                    IDMSG            27
112000940506     C                   PARM                    TXTMSG          100
112100940506     C                   PARM                    ERRMSG            1
112200940506     C*
112300940506     C     ERRMSG        IFNE      *ON
112400940506     C                   MOVEL     TXTMSG        $FX(X)
112500940506     C                   ELSE
112600940506     C                   MOVEL     *ALL' '       $FX(X)
112700940506     C                   END
112800940506     C*
112900940506     C                   END
113000940506     C*
113100940506     C                   END
113200940506     C*
113300940506     C                   ENDSR
113400941129     C/EJECT
113500941129     C************************************************************
113600941129     C* CARICA FINESTRA
113700941129     C************************************************************
113800941129     C     CARW1         BEGSR
113900941130     C                   CLEAR                   $D
114000941130     C                   CLEAR                   $P
114100941129     C                   Z-ADD     0             $Z                3 0
114200941129     C*
114300941129     C* Cerco nel file delle catene il padre  (codice immediatamente
114400941129     C* superiore)
114500941129     C                   MOVE      XSCSOC        XTBAZI
114600941129     C                   MOVE      XSCLIN        XTBLIN
114700941129     C                   MOVE      *ALL'0'       XTBKEY
114800941129     C                   MOVE      '0'           XTBALC
114900941129     C                   MOVE      '2'           XTBRIC
115000950214     C                   MOVE      '0A3'         XTBCOD
115100941129     C                   MOVE      XSCCUT        XTBUTE
115200941129    >C                   CALL      'XATB'
115300941129     C                   PARM                    XATBDS
115400941129   B1C     XTBERR        DOWEQ     '0'
115500941130     C                   MOVE      XTBKEY        SAVKEY
115600950214     C                   MOVEL     XTBUNI        TAB0A3
115700950214     C                   MOVEL     TAB0A3        $AX
115800941129     C                   Z-ADD     1             $X                3 0
115900941129     C     D1POS         LOOKUP    $A($X)                                 13
116000941129   B2C     *IN13         IFEQ      *ON
116100941129     C     $X            IFNE      1
116200941129     C     $X            SUB       1             $X
116300941129     C* la posizione che mi serve è $A,$X
116400950214     C* cerco la definizione nella tabella 0A2
116500941130     C                   MOVE      XSCSOC        XTBAZI
116600941130     C                   MOVE      XSCLIN        XTBLIN
116700941130     C                   MOVE      *ALL'0'       XTBKEY
116800941130     C                   MOVE      $A($X)        XTBKEY
116900941130     C                   MOVE      '0'           XTBALC
117000941130     C                   MOVE      '1'           XTBRIC
117100950214     C                   MOVE      '0A2'         XTBCOD
117200941130     C                   MOVE      XSCCUT        XTBUTE
117300941130    >C                   CALL      'XATB'
117400941130     C                   PARM                    XATBDS
117500941130   B1C     XTBERR        IFEQ      '0'
117600950214     C                   MOVEL     XTBUNI        TAB0A2
117700941129     C                   ADD       1             $Z
117800941129   B4C     $Z            IFLE      10
117900950214     C                   MOVEL     DES0A2        $D($Z)
118000941130     C                   MOVE      XTBKEY        $P($Z)
118100941129   E4C                   END
118200941129   E3C                   END
118300941129   E2C                   END
118400941129     C                   END
118500941130     C*
118600941130     C* Eseguo setll con chiave completa(perche mi sono sposizionata)
118700950214     C* e lettura su tabella 0A3
118800941130     C                   MOVE      XSCSOC        XTBAZI
118900941130     C                   MOVE      XSCLIN        XTBLIN
119000941130     C                   MOVE      *ALL'0'       XTBKEY
119100941130     C                   MOVE      SAVKEY        XTBKEY
119200941130     C                   MOVE      '0'           XTBALC
119300941130     C                   MOVE      '2'           XTBRIC
119400950214     C                   MOVE      '0A3'         XTBCOD
119500941130     C                   MOVE      XSCCUT        XTBUTE
119600941130    >C                   CALL      'XATB'
119700941130     C                   PARM                    XATBDS
119800941130     C*
119900941130     C     XTBERR        IFEQ      '0'
120000941129     C                   MOVE      '3'           XTBRIC
120100941129    >C                   CALL      'XATB'
120200941129     C                   PARM                    XATBDS
120300941130     C                   END
120400941130     C*
120500941129   E1C                   END
120600941129     C*
120700941129     C                   ENDSR
120800941129     C/EJECT
120900941129     C************************************************************
121000941129     C* CONTROLLI FINESTRA
121100941129     C************************************************************
121200941129     C     CTRW1         BEGSR
121300941129     C                   SETOFF                                       99
121400941129     C* controllo obbligatorietà
121500941129     C                   DO        10            $X
121600941129     C     $D($X)        IFNE      *BLANKS
121700941129     C     $C($X)        ANDEQ     *BLANKS
121800941129     C                   SETON                                        99
121900941129     C     $X            ADD       60            $X
122000941129     C                   MOVE      '1'           *IN($X)
122100941130     C                   GOTO      ENDCW1
122200941129     C                   ENDIF
122300941129     C                   ENDDO
122400941129     C*
122500941129     C* controllo bytes
122600941129     C                   DO        10            $X
122700941129     C     $C($X)        IFNE      *BLANKS
122800941129     C                   MOVE      *ZEROS        XTBKEY
122900941129     C                   MOVE      $P($X)        XTBKEY
123000941129     C                   MOVEL     $C($X)        D1K
123100941129     C                   EXSR      CTRBYT
123200941130     C   99              DO
123300941129     C     $X            ADD       70            $X
123400941129     C                   MOVE      '1'           *IN($X)
123500941130     C                   GOTO      ENDCW1
123600941130     C                   ENDDO
123700941129     C                   ENDIF
123800941129     C                   ENDDO
123900941130     C*
124000941130     C* controllo campi
124100941130     C                   DO        10            $X
124200941130     C     $C($X)        IFNE      *BLANKS
124300941130     C                   MOVE      '1'           XTBRIC
124400941130     C                   MOVE      XSCSOC        XTBAZI
124500941130     C                   MOVE      *ZEROS        XTBKEY
124600941130     C                   MOVEL     $P($X)        WRK6              6
124700941130     C                   MOVE      $C($X)        WRK6
124800941130     C                   MOVE      WRK6          XTBKEY
124900941130     C                   MOVE      '0'           XTBALC
125000950214     C                   MOVE      '0A1'         XTBCOD
125100941130     C                   CALL      'XATB'
125200941130     C                   PARM                    XATBDS
125300941130     C     XTBERR        IFEQ      *ON
125400941130     C                   SETON                                        99
125500941130     C     $X            ADD       50            $X
125600941130     C                   MOVE      '1'           *IN($X)
125700941130     C                   GOTO      ENDCW1
125800941130     C                   END
125900941130     C                   END
126000941130     C                   ENDDO
126100941130     c*
126200941130     C     ENDCW1        ENDSR
126300940509     C/EJECT
126400940223     C************************************************************
126500940223     C* INIZIALIZZAZIONE VARIABILI
126600940223     C************************************************************
126700940223     C     INZVAR        BEGSR
126800951129     C*
126900951129     C* Chiamata XSOC
127000951129     C*
127100951129     C                   MOVEL     'SOC001'      TIPXSC
127200951129     C                   EXSR      REPSOC
127300951129     C     RTNXSC        IFNE      '1'
127400951129     C                   MOVEL     XSOCDS        SOC001
127500951129     C                   ELSE
127600951129     C                   MOVE      *ON           $FINE
127700951129     C* fine programma
127800951129     C                   EXSR      ENDPGM
127900951129     C                   ENDIF
128000940224     C*
128100940224     C* Dati da PGM guida
128200940224     C*
128300940315    >C                   MOVEL     KPJBU         D002DS
128400940128     C*
128500940223     C* Variabili per gestione videate
128600940128     C*
128700940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
128800940223     C                   MOVE      *OFF          $FINE                          fine pgm
128900940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
129000940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
129100940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
129200940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
129300940127     C*
129400940223     C* Variabili appoggio
129500940127     C*
129600940224     C                   Z-ADD     0             CURR
129700940224     C                   Z-ADD     0             CURC
129800940207     C*
129900940207     C* Valorizzazione campi univoci testate
130000940207     C*
130100940614     C                   CLEAR                   T1
130200940614     C                   MOVEL     KNSIF         NOMSIF
130300941109     C                   MOVEL     XSCDSI        NOMDIT
130400941027     C                   MOVEL     DSPGM         NOMPGM
130500940224     C                   Z-ADD     1             X
130600940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
130700940224     C     *IN21         IFEQ      *ON
130800940224     C                   MOVEL     $ID(X)        IDMSG
130900940224     C                   CALL      'XRTVM'
131000940224     C                   PARM                    IDMSG            27
131100940224     C                   PARM                    TXTMSG          100
131200940224     C                   PARM                    ERRMSG            1
131300940224     C                   PARM      *ON           CTRMSG            1            centratura
131400940224     C                   PARM      30            LENMSG            3 0          lun output.
131500940224     C     ERRMSG        IFNE      *ON
131600940224     C                   MOVEL     TXTMSG        T1OPE
131700940224     C                   ELSE
131800940224     C                   MOVEL     *ALL'?'       T1OPE
131900940224     C                   ENDIF
132000940224     C                   END
132100940224     C*
132200940224     C* Inizializzazione/Decodifica prima videata
132300940224     C*
132400940224     C                   EXSR      INZD1
132500940224     C                   MOVE      *OFF          $INZD1
132600940127     C*
132700940117     C                   ENDSR
132800940128     C************************************************************
132900940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
133000940223     C************************************************************
133100940223     C     DEFCAM        BEGSR
133200940223     C*
133300940223     C* klist
133400940207     C*
133500940223     C*
133600940223     C* Variabili per gestione videate
133700940223     C*
133800940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
133900940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
134000940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
134100940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
134200940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
134300941130     C*
134400941130     C     *LIKE         DEFINE    XTBKEY        SAVKEY
134500941130     C     *LIKE         DEFINE    XTBCOD        SAVCOD
134600951031     C     *LIKE         DEFINE    $C(1)         WWCXX
134700940223     C*
134800940223     C* Variabili appoggio
134900940223     C*
135000940223     C                   Z-ADD     0             CURR              2 0
135100940223     C                   Z-ADD     0             CURC              2 0
135200940223     C                   MOVE      *ZEROS        WIN              99
135300940224     C                   MOVE      *BLANK        WTBERR            1
135400940223     C*
135500940223     C* Indici
135600940223     C*
135700940223     C                   Z-ADD     0             X                 3 0
135800940223     C                   Z-ADD     0             Y                 3 0
135900940127     C*
136000940117     C                   ENDSR
136100940128     C************************************************************
136200940509     C/EJECT
136300940509     C************************************************************
136400940509     C* CONTROLLO ABILITAZIONE TASTI
136500940509     C************************************************************
136600940509     C     CTRKEY        BEGSR
136700940509     C                   SELECT
136800940509     C     $TASTO        WHENEQ    F01
136900940720     C     $FC(1)        IFEQ      '0'
137000940509     C                   SETON                                        99
137100940509     C                   ENDIF
137200940509     C     $TASTO        WHENEQ    F02
137300940720     C     $FC(2)        IFEQ      '0'
137400940509     C                   SETON                                        99
137500940509     C                   ENDIF
137600940509     C     $TASTO        WHENEQ    F03
137700940720     C     $FC(3)        IFEQ      '0'
137800940509     C                   SETON                                        99
137900940509     C                   ENDIF
138000940509     C     $TASTO        WHENEQ    F04
138100940720     C     $FC(4)        IFEQ      '0'
138200940509     C                   SETON                                        99
138300940509     C                   ENDIF
138400940509     C     $TASTO        WHENEQ    F05
138500940720     C     $FC(5)        IFEQ      '0'
138600940509     C                   SETON                                        99
138700940509     C                   ENDIF
138800940509     C     $TASTO        WHENEQ    F06
138900940720     C     $FC(6)        IFEQ      '0'
139000940509     C                   SETON                                        99
139100940509     C                   ENDIF
139200940509     C     $TASTO        WHENEQ    F07
139300940720     C     $FC(7)        IFEQ      '0'
139400940509     C                   SETON                                        99
139500940509     C                   ENDIF
139600940509     C     $TASTO        WHENEQ    F08
139700940720     C     $FC(8)        IFEQ      '0'
139800940509     C                   SETON                                        99
139900940509     C                   ENDIF
140000940509     C     $TASTO        WHENEQ    F09
140100940720     C     $FC(09)       IFEQ      '0'
140200940509     C                   SETON                                        99
140300940509     C                   ENDIF
140400940509     C     $TASTO        WHENEQ    F10
140500940720     C     $FC(10)       IFEQ      '0'
140600940509     C                   SETON                                        99
140700940509     C                   ENDIF
140800940509     C     $TASTO        WHENEQ    F11
140900940720     C     $FC(11)       IFEQ      '0'
141000940509     C                   SETON                                        99
141100940509     C                   ENDIF
141200940509     C     $TASTO        WHENEQ    F12
141300940720     C     $FC(12)       IFEQ      '0'
141400940509     C                   SETON                                        99
141500940509     C                   ENDIF
141600940509     C     $TASTO        WHENEQ    F13
141700940720     C     $FC(13)       IFEQ      '0'
141800940509     C                   SETON                                        99
141900940509     C                   ENDIF
142000940509     C     $TASTO        WHENEQ    F14
142100940720     C     $FC(14)       IFEQ      '0'
142200940509     C                   SETON                                        99
142300940509     C                   ENDIF
142400940509     C     $TASTO        WHENEQ    F15
142500940720     C     $FC(15)       IFEQ      '0'
142600940509     C                   SETON                                        99
142700940509     C                   ENDIF
142800940509     C     $TASTO        WHENEQ    F16
142900940720     C     $FC(16)       IFEQ      '0'
143000940509     C                   SETON                                        99
143100940509     C                   ENDIF
143200940509     C     $TASTO        WHENEQ    F17
143300940720     C     $FC(17)       IFEQ      '0'
143400940509     C                   SETON                                        99
143500940509     C                   ENDIF
143600940509     C     $TASTO        WHENEQ    F18
143700940720     C     $FC(18)       IFEQ      '0'
143800940509     C                   SETON                                        99
143900940509     C                   ENDIF
144000940509     C     $TASTO        WHENEQ    F19
144100940720     C     $FC(19)       IFEQ      '0'
144200940509     C                   SETON                                        99
144300940509     C                   ENDIF
144400940509     C     $TASTO        WHENEQ    F20
144500940720     C     $FC(20)       IFEQ      '0'
144600940509     C                   SETON                                        99
144700940509     C                   ENDIF
144800940509     C     $TASTO        WHENEQ    F21
144900940720     C     $FC(21)       IFEQ      '0'
145000940509     C                   SETON                                        99
145100940509     C                   ENDIF
145200940509     C     $TASTO        WHENEQ    F22
145300940720     C     $FC(22)       IFEQ      '0'
145400940509     C                   SETON                                        99
145500940509     C                   ENDIF
145600940509     C     $TASTO        WHENEQ    F23
145700940720     C     $FC(23)       IFEQ      '0'
145800940509     C                   SETON                                        99
145900940509     C                   ENDIF
146000940509     C     $TASTO        WHENEQ    F24
146100940720     C     $FC(24)       IFEQ      '0'
146200940509     C                   SETON                                        99
146300940509     C                   ENDIF
146400940509     C                   ENDSL
146500940509     C                   ENDSR
146600940509     C/EJECT
146700940509     C************************************************************
146800940509     C* RIEMPIMENTO TASTI FUNZIONE
146900940509     C************************************************************
147000940509     C     RIEKEY        BEGSR
147100940509     C*
147200940509     C* Input :
147300940509     C* - $FX = schiera con tutti i testi disponibili
147400940509     C* - $FC = schiere con i flag di validità associati ai testi
147500940509     C* - $FR = schiere con i flag di REVERSE IMAGE
147600940509     C* - $ULK = ultimo testo caricato
147700940509     C* Output :
147800940509     C* - $ULK
147900940509     c* - $KEY1= 1° riga di tasti funzione
148000940509     c* - $KEY2= 2° riga di tasti funzione
148100940509     C*
148200940509     C                   CALL      'XKEYMSG'
148300940509     C* input
148400940509     C                   PARM                    $FX
148500940509     C                   PARM                    $FC
148600940509     C                   PARM                    $FR
148700940509     C                   PARM                    $ULK              3 0
148800940509     C* output
148900940509     C                   PARM                    $KEY1            79
149000940509     C                   PARM                    $KEY2            79
149100940509     C*
149200060224A1148C                   PARM      79            $L01              3 0
149300060224A1148C                   PARM      79            $L02              3 0
149400060224A1148C*
149500060224A1148C                   PARM                    $ALLFUNCT       480
149600060224A1148C*
149700940509     C                   ENDSR
149800940615**
14990094022401PROMSG    *LIBL     COS0001    immissione
15000094022402PROMSG    *LIBL     COS0002    modifica
15010094022403PROMSG    *LIBL     COS0003    copia
15020094022404PROMSG    *LIBL     COS0004    annullamento
15030094022405PROMSG    *LIBL     COS0005    visualizzazione
150400940509** TASTI DI FUNZIONE  D1
150500940509PROMSG    *LIBL     KEY0001  01
150600940509PROMSG    *LIBL     KEY0002  02
150700940509PROMSG    *LIBL     KEY0003  03
150800940509PROMSG    *LIBL     KEY0004  04
150900940509PROMSG    *LIBL     KEY0005  05
151000940509PROMSG    *LIBL     KEY0006  06
151100940509PROMSG    *LIBL     KEY0007  07
151200940509PROMSG    *LIBL     KEY0008  08
151300940509PROMSG    *LIBL     KEY0009  09
151400940509PROMSG    *LIBL     KEY0010  10
151500940509PROMSG    *LIBL     KEY0011  11
151600940509PROMSG    *LIBL     KEY0012  12
151700940509PROMSG    *LIBL     KEY0013  13
151800940509PROMSG    *LIBL     KEY0014  14
151900940509PROMSG    *LIBL     KEY0015  15
152000940509PROMSG    *LIBL     KEY0016  16
152100940509PROMSG    *LIBL     KEY0017  17
152200940509PROMSG    *LIBL     KEY0018  18
152300940509PROMSG    *LIBL     KEY0019  19
152400940509PROMSG    *LIBL     KEY0020  20
152500940509PROMSG    *LIBL     KEY0021  21
152600940509PROMSG    *LIBL     KEY0022  22
152700940509PROMSG    *LIBL     KEY0023  23
152800940509PROMSG    *LIBL     KEY0024  24
152900940509**  ABILITAZIONE D1
1530009407200N  01
1531009407200N  02
1532009407201N  03
1533009407201N  04
1534009407201N  05
1535009407201N  06
1536009407201N  07
1537009407201N  08
1538009407200N  09
1539009407200N  10
1540009407200N  11
1541009407201N  12
1542009407200N  13
1543009407200N  14
1544009407200N  15
1545009407200N  16
1546009407200N  17
1547009407200N  18
1548009407200N  19
1549009407200N  20
1550009407200N  21
1551009407200N  22
1552009407200N  23
1553009407200N  24
155400940615**
155500940615DES   Descrizione ................
