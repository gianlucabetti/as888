000100150325     H DFTACTGRP(*NO) BNDDIR('PJXBND')
000200150325     H CVTOPT(*DATETIME)
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
004100940614$001 FANGYP1D2  CF   E             WORKSTN
004200940201     F                                     INFDS(DSFMT)
004300940621     FANTBT01L  UF   E           K DISK
004400980720     Fanrco99j  IF   E           K DISK
004500980720     Fansog01l  IF   E           K DISK
004600980720     Fanope01l  IF   E           K DISK
004700940128     D*----------------------------------------------------*
004800940224     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
004900940224     D $ID             S             27    DIM(5) ALT($OZ)
005000940506     D* Schiere per la gestione dei tasti di funzione
005100940506     D $FM             S             27    DIM(24)
005200940506     D $FX             S             50    DIM(24)
005300940506     D $FC             S              1    DIM(24)
005400940506     D $FR             S              1    DIM(24)
005500940506     D* Video D1
005600940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005700940506     D $FX1            S             50    DIM(24) ALT($FM1)
005800940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005900940506     D $FR1            S              1    DIM(24) ALT($FC1)
006000940615     D* campi in lingua
006100940622$010 D*LIN             S             06    DIM(0011) CTDATA PERRCD(001)
006200940622$010 D*LIND            S             50    DIM(0011) ALT(LIN)
006300940614$003 D EXTDS         E DS                  EXTNAME(ANGYP1DS) INZ
006400940127     D* Passaggio Parametri
006500940127     D KPJBA         E DS
006600941103     D*-------------
006700941103      * DS Società
006800941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
006900960123    >D  SALDOC       E                     EXTFLD(XSCSALDOC)
007000960123    >D  SALDOA       E                     EXTFLD(XSCSALDOA)
007100941103     D*-------------
007200941103      * DS Interna per dati utente
007300941103     D XSOCDS          DS          1000
007400941103     D*-------------
007500940616     D XATBDS        E DS                  INZ
007600980807     D xtabds        E DS                  inz
007700940318     D*-------------
007800980720     D a250ds        E DS                  extname(ana250ds)
007900980720     D ana200ds      E DS
008000980720     D* Ricerca causali
008100980720     D A070DS        E DS                  EXTNAME(ANA070DS) INZ
008200940318     D* DS Esterna gestione messaggi per COMIT
008300940616     D XMSGDS        E DS                  INZ
008400940318     D  STS          E                     EXTFLD(MSGSTS)
008500940318     D                                     DIM(333)
008600940318     D  JBA          E                     EXTFLD(MSGJBA)
008700940318     D                                     DIM(502)
008800940203     D*-------------
008900940620     D* DS Esterna autorizzazioni tabelle
009000940620     D XCHATB        E DS                  EXTNAME(XCHKATBDS)
009100940620     D*
009200940620     D* DS  restituzione autorizzazioni
009300940620     D* IND=indice
009400940620     D* GRU=man. di gruppo
009500940620     D* AZI=man. azienda
009600940712     D* ABI=abilit. ai riservati
009700941111     D AUTDS           DS
009800940620     D  MANIND                 1      1
009900940620     D  MANGRU                 2      2
010000940620     D  MANAZI                 3      3
010100940712     D  ABIRIS                 4      4
010200940620     D*-------------
010300940207     D* opzione richiesta
010400940614    >D D002DS          DS
010500940614     D  OPZ002                 1      2
010600940614     D* 0 = ritorno normale
010700940614     D* 1 = F3
010800940614     D* 2 = F12
010900940614     D  RET002                 3      3
011000940614     D* operazione eseguite dal pgm chiamato :
011100940614     D* 0 = nessuna operazione
011200940614     D* 1 = eseguito aggiornamento
011300940614     D  OPR002                 4      4
011400940614     D* funzione non eseguibile per errore :
011500940614     D* 0 = nessun errore
011600940614     D* 1 = funzione richiamata chiusa in errore
011700940614     D* Aggiungere eventuali altri codici di errore
011800940614     D  ERR002                 5      5
011900940614     D* Azienda
012000940614     D  AZI002                 6      8
012100940614     D* Lingua
012200940614     D  LIN002                 9      9
012300940614     D* Codice scelto
012400940614     D  KEY002                10     24
012500940620     D* Autorizzazione
012600940620     D  AUT002                25     25
012700970509     D* Unita
012800970509     D  UNI002                26     33
012900940614     D*-------------
013000940201     D*-------------
013100940201     D DSFMT           DS
013200940506     D  $TASTO               369    369
013300940201     D  NRG                  370    370
013400940201     D  NCL                  371    371
013500940201     D*-------------
013600940201     D* posizione cursore
013700940201     D CURSOR          DS
013800940223     D  RIRI                   1      2B 0 INZ
013900940201     D  R$$                    2      2
014000940223     D  COCO                   3      4B 0 INZ
014100940201     D  C$$                    4      4
014200940207     D*-------------
014300940207     D* Reperimento nome PGM
014400940207     D STATUS         SDS           333
014500940207     D  DSPGM            *PROC
014600940225     D*-------------
014700940225     D* COSTANTI
014800940225     D*-------------
014900940506     D*
015000940506     D* Tasti di funzione
015100940506     D F01             C                   CONST(X'31')
015200940506     D F02             C                   CONST(X'32')
015300940506     D F03             C                   CONST(X'33')
015400940506     D F04             C                   CONST(X'34')
015500940506     D F05             C                   CONST(X'35')
015600940506     D F06             C                   CONST(X'36')
015700940506     D F07             C                   CONST(X'37')
015800940506     D F08             C                   CONST(X'38')
015900940506     D F09             C                   CONST(X'39')
016000940506     D F10             C                   CONST(X'3A')
016100940506     D F11             C                   CONST(X'3B')
016200940506     D F12             C                   CONST(X'3C')
016300940506     D F13             C                   CONST(X'B1')
016400940506     D F14             C                   CONST(X'B2')
016500940506     D F15             C                   CONST(X'B3')
016600940506     D F16             C                   CONST(X'B4')
016700940506     D F17             C                   CONST(X'B5')
016800940506     D F18             C                   CONST(X'B6')
016900940506     D F19             C                   CONST(X'B7')
017000940506     D F20             C                   CONST(X'B8')
017100940506     D F21             C                   CONST(X'B9')
017200940506     D F22             C                   CONST(X'BA')
017300940506     D F23             C                   CONST(X'BB')
017400940506     D F24             C                   CONST(X'BC')
017500940506     D ENTER           C                   CONST(X'F1')
017600940506     D ROLDWN          C                   CONST(X'F4')
017700940506     D ROLLUP          C                   CONST(X'F5')
017800011008     d a               s              3  0
017900011008     d b               s              3  0
018000940506     D*-------------
018100940506     D*
018200941108     D DATA            C                   CONST('0001-01-01')
018300940128     I*----------------------------------------------------*
018400980807     D                 DS
018500980807     D vtip                    1      6
018600980807     D vtip4                   3      6
018700980807     D                 DS
018800011005     D  d1tpa                  1     20    dim(20)
018900980807     D  d1tpa01                1      1
019000980807     D  d1tpa02                2      2
019100980807     D  d1tpa03                3      3
019200980807     D  d1tpa04                4      4
019300980807     D  d1tpa05                5      5
019400980807     D  d1tpa06                6      6
019500980807     D  d1tpa07                7      7
019600980807     D  d1tpa08                8      8
019700980807     D  d1tpa09                9      9
019800980807     D  d1tpa10               10     10
019900011005     D  VTIPA                  1     10
020000011005     D  d1tpa11               11     11
020100011005     D  d1tpa12               12     12
020200011005     D  d1tpa13               13     13
020300011005     D  d1tpa14               14     14
020400011005     D  d1tpa15               15     15
020500011005     D  d1tpa16               16     16
020600011005     D  d1tpa17               17     17
020700011005     D  d1tpa18               18     18
020800011005     D  d1tpa19               19     19
020900011005     D  d1tpa20               20     20
021000011005     D  VTIPA1                11     20
021100980807     D*---
021200940615     ID1
021300980717$005 I              D1MAN                       §P1MAN
021400980717$005 I              D1ACC                       §P1ACC
021500980717$005 I              D1RGH                       §P1RGH
021600980717$005 I              D1STB                       §P1STB
021700980717$005 I              D1GES                       §P1GES
021800980717$005 I              D1KSC                       §P1KSC
021900980717$005 I              D1NRT                       §P1NRT
022000980717$005 I              D1CAU                       §P1CAU
022100980717$005 I              D1KCC                       §P1KCC
022200980717$005 I              D1TPE                       §P1TPE
022300980717$005 I              D1OUT                       §P1OUT
022400980717$005 I              D1DET                       §P1DET
022500980717$005 I              D1CAB                       §P1CAB
022600980811$005 I              D1CGP                       §P1CGP
022700990216$005 I              D1DAN                       §P1DAN
022800940127     I*
022900940127     C*----------------------------------------------------*
023000940127     C*                MAIN LINE PROGRAM
023100940127     C*----------------------------------------------------*
023200940127     C*
023300940223     C* inizializzazione variabili
023400940223     C                   EXSR      INZVAR
023500940223     C*
023600940223     C     $FINE         DOWEQ     *OFF
023700940202     C     $GEST         CASEQ     'D1'          GESD1
023800940117     C                   END
023900940117     C                   END
024000940201     C*
024100940325     C* fine programma
024200940325     C                   EXSR      ENDPGM
024300940325     C/EJECT
024400940325     C************************************************************
024500940325     C* RICERCHE
024600940325     C************************************************************
024700940325     C     ENDPGM        BEGSR
024800940325     C*
024900940614     C* disallocazione rcd nel caso di
025000971110$007 C                   MOVE      'YP1'         XTBCOD
025100940614    >C                   MOVE      '7'           XTBRIC
025200970214     C                   CALLB     'XATB'
025300940614     C                   PARM                    XATBDS
025400940325     C*
025500940325     C* passaggio dati al pgm chiamante
025600940715     C                   MOVE      *BLANKS       KPJBU
025700940325    >C                   MOVEL     D002DS        KPJBU
025800940325     C*
025900940325     C                   SETON                                            RT
026000940325     C*
026100940325     C                   ENDSR
026200940325     C/EJECT
026300940201     C************************************************************
026400940201     C* RICERCHE
026500940201     C************************************************************
026600940127     C     SEARCH        BEGSR
026700940127     C*
026800940127     C* determino Riga/Colonna del cursore
026900940127     C                   MOVE      NRG           R$$
027000940127     C                   MOVE      NCL           C$$
027100940127     C                   Z-ADD     RIRI          CURR
027200940127     C                   Z-ADD     COCO          CURC
027300940127     C*
027400940224     C* I campi che al momento dell'F4 risultano protetti non devono
027500940224     C* essere abilitati alla funzione di ricerca
027600940224     C*
0277009401271    C                   SELECT
027800940127     C*
027900980720     C* FMTD1  - codice
028000980720    >C     H1NMFL        WHEneq    'D1COD'
028100980720     C                   RESET                   Ana200DS
028200980720     C                   MOVEL     '01'          OPZ200
028300980720     C                   clear                   KCC200
028400980720     C                   MOVEL     Ana200DS      KPJBU
028500980720     C                   CALL      'ANA200R'
028600980720     C                   PARM                    KPJBA
028700980720     C                   MOVEL     KPJBU         Ana200DS
028800980720     C     KCC200        IFNE      *BLANK
028900980720     C                   MOVEL     KCC200        D1cod
029000980720     C                   ENDIF
029100980720     C* imposto pos. del cursore
029200980720     C                   Z-ADD     CURR          H1RIGA
029300980720     C                   Z-ADD     CURC          H1COLO
029400980720     C*
029500980720     C* FMTD1  - capoconto
029600980720    >C     H1NMFL        WHEneq    'D1KCC'
029700980720     C                   RESET                   Ana200DS
029800980720     C                   MOVEL     '01'          OPZ200
029900980720     C                   clear                   KCC200
030000980720     C                   MOVEL     Ana200DS      KPJBU
030100980720     C                   CALL      'ANA200R'
030200980720     C                   PARM                    KPJBA
030300980720     C                   MOVEL     KPJBU         Ana200DS
030400980720     C     KCC200        IFNE      *BLANK
030500980720     C                   MOVEL     KCC200        §p1Kcc
030600980720     C                   ENDIF
030700980720     C* imposto pos. del cursore
030800980720     C                   Z-ADD     CURR          H1RIGA
030900980720     C                   Z-ADD     CURC          H1COLO
031000980720     C*
031100980720     C* Ricerca sottoconti (ANA250R)
031200980720     C     H1NMFL        WHENEQ    'D1KSC'
031300980720     C                   clear                   §p1ksc
031400980720     C                   RESET                   a250DS
031500980720     C                   MOVEL     '01'          OPZ250
031600980720     C                   MOVEL     §p1kcc        KCC250
031700980720     C*                  MOVE      D1KSC         KSC250
031800980720     C                   clear                   KSC250
031900980720     C                   MOVEL     a250DS        KPJBU
032000980720     C                   CALL      'ANA250R'
032100980720     C                   PARM                    KPJBA
032200980720     C                   MOVEL     KPJBU         a250DS
032300980720     C     KSC250        IFNE      *BLANK
032400980720     C                   MOVEL     KCC250        §p1kcc
032500980720     C                   MOVEL     KSC250        §p1ksc
032600980720     C                   ENDIF
032700980720     C* imposto pos. del cursore
032800980720     C                   Z-ADD     CURR          H1RIGA
032900980720     C                   Z-ADD     CURC          H1COLO
033000980720     C*
033100980720    >C*  FMTD1  - CAUSALE 1
033200980720     C     H1NMFL        WHENEQ    'D1CAU'
033300980720     C                   RESET                   A070DS
033400980720     C                   MOVEL     '01'          OPZ070
033500980720     C                   MOVEL     A070DS        KPJBU
033600980720     C                   CALL      'ANA070R'
033700980720     C                   PARM                    KPJBA
033800980720     C                   MOVEL     KPJBU         A070DS
033900980720     C                   MOVEL     CAU070        WWCAUSC1          4
034000980720     C*
034100980720     C     WWCAUSC1      IFNE      *BLANK
034200980720     C                   MOVEL     WWCAUSC1      §p1cau
034300980720     C                   ENDIF
034400940207     C* imposto pos. del cursore
034500980720     C                   Z-ADD     CURR          H1RIGA
034600980720     C                   Z-ADD     CURC          H1COLO
034700980720     C*
034800980720    >C*  FMTD1  - CAUSALE 1
034900980720     C     H1NMFL        WHENEQ    'D1CAB'
035000980720     C                   RESET                   A070DS
035100980720     C                   MOVEL     '01'          OPZ070
035200980720     C                   MOVEL     A070DS        KPJBU
035300980720     C                   CALL      'ANA070R'
035400980720     C                   PARM                    KPJBA
035500980720     C                   MOVEL     KPJBU         A070DS
035600980720     C                   MOVEL     CAU070        WWCAUSC1          4
035700980720     C*
035800980720     C     WWCAUSC1      IFNE      *BLANK
035900980720     C                   MOVEL     WWCAUSC1      §p1cab
036000980720     C                   ENDIF
036100980720     C* imposto pos. del cursore
036200980720     C                   Z-ADD     CURR          H1RIGA
036300980720     C                   Z-ADD     CURC          H1COLO
036400980720     C*
036500940223     C* ricerca non abilitata in questa posizione
0366009402231O   C                   OTHER
036700941005     C                   SETON                                        9899
036800941005     C* imposto pos. del cursore
036900941005     C                   Z-ADD     CURR          H1RIGA
037000941005     C                   Z-ADD     CURC          H1COLO
037100940127     C*
0372009401271-   C                   ENDSL
037300940127     C*
037400940506     C                   MOVE      *HIVAL        $LASTV
037500940127     C                   ENDSR
037600940615     C/EJECT
037700940615     C************************************************************
037800940615     C* TRADUZIONI
037900940615     C************************************************************
038000940615     C     LINGUA        BEGSR
038100940615     C*
038200940615     C* determino Riga/Colonna del cursore
038300940615     C                   MOVE      NRG           R$$
038400940615     C                   MOVE      NCL           C$$
038500940615     C                   Z-ADD     RIRI          CURR
038600940615     C                   Z-ADD     COCO          CURC
038700940615     C*
038800940615     C*
038900940615     C*
039000940615     C* FMTD1  - CAMPI DA TRADURRE
039100940615     C* imposto pos. del cursore
039200940615     C                   Z-ADD     CURR          H1RIGA
039300940615     C                   Z-ADD     CURC          H1COLO
039400940621     C                   MOVEL     H1NMFL        W005              5
039500940621     C                   MOVE      W005          W003              3
039600940621     C                   MOVEL     W003          W006              6
039700940620     C                   MOVE      *ZERO         KEY              15
039800940616     C                   MOVE      D1COD         KEY
039900940615     C                   SELECT
040000940615     C*
040100940615     C*
040200940615     C* ricerca non abilitata in questa posizione
0403009406151O   C                   OTHER
040400940615     C                   SETON                                        98
040500940615     C*
0406009406151-   C                   ENDSL
040700940615     C*
040800940615     C                   MOVE      *HIVAL        $LASTV
040900940615     C                   ENDSR
041000940225     C/EJECT
041100940225     C************************************************************
041200940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
041300940225     C************************************************************
041400940225     C     GESPIE        BEGSR
041500940225     C*
041600940225     C                   SELECT
041700940225     C*
041800940506     C     $GEST         WHENEQ    'D1'
041900940506     C     $ULKD1        IFEQ      0
042000940506     C* riempimento opzioni
042100940506     C                   EXSR      F24D1
042200940506     C                   ELSE
042300940614     C                   WRITE     Z1
042400940506     C                   ENDIF
042500940225     C*
042600940510     C*
042700940225     C                   ENDSL
042800940225     C*
042900940225     C                   ENDSR
043000940131     C/EJECT
043100940131     C************************************************************
043200940510     C* GESTIONE VIDEO RECORD D1
043300940131     C************************************************************
043400940127     C     GESD1         BEGSR
043500940225     C* inizializzazione videata
043600940223     C     $INZD1        IFEQ      *ON
043700940127     C                   EXSR      INZD1
043800940223     C                   MOVE      *OFF          $INZD1
043900940117     C                   END
044000940225     C* emissione piede videata se proveniente da altra
044100940225     C* salvataggio valore formato di provenienza
044200940207     C     $LASTV        IFNE      'D1'
044300940614     C                   WRITE     T1
044400940225     C                   EXSR      GESPIE
044500940202     C                   MOVE      $LASTV        $LASTG
044600940202     C                   MOVE      'D1'          $LASTV
044700940202     C                   END
044800940131     C*
044900940203     C                   SELECT
045000940203     C* Annullamento
045100940315    >C     OPZ002        WHENEQ    '04'
045200940224     C* Visualizzazione
045300940315    >C     OPZ002        OREQ      '05'
045400940620     C                   SETON                                        0203
045500940620     C* Manutenzione
045600940620    >C     OPZ002        WHENEQ    '02'
045700940620     C                   SETON                                        02
045800940620     C                   SETOFF                                       03
045900940620     C*
046000940620     C                   OTHER
046100940620     C*
046200940620     C                   SETOFF                                       0203
046300940203     C                   ENDSL
046400940620     C* Immissione/Modifica/Copia
046500940620     C                   EXFMT     D1
046600940506     C* controllo abilitazione tasti funzione
046700940506     C*
046800940506     C                   EXSR      CTRKD1
046900940506     C*
047000940506     C     *IN99         IFEQ      *OFF
047100940131     C*
0472009402031    C                   SELECT
047300940510     c* ROLLUP
047400940131     C* F3=Fine
047500940506     C     $TASTO        WHENEQ    F03
047600940309     C                   EXSR      F03D1
047700940131     C* F4=Ricerca
047800940506     C     $TASTO        WHENEQ    F04
047900940127     C                   EXSR      SEARCH
048000940224     C* F5=Refresh
048100940506     C     $TASTO        WHENEQ    F05
048200940309     C                   EXSR      F05D1
048300940223     C* F8=Successivo
048400940506     C     $TASTO        WHENEQ    F08
048500940309     C                   EXSR      F08D1
048600940131     C* F12=Ritorno
048700940506     C     $TASTO        WHENEQ    F12
048800940309     C                   EXSR      F12D1
048900940315     C* F24=Altri tasti
049000940506     C     $TASTO        WHENEQ    F24
049100940315     C                   EXSR      F24D1
049200940131     C*
0493009402031O   C                   OTHER
049400940127     C                   EXSR      CTRD1
049500940616     C     *IN99         IFEQ      *OFF
049600940616     C                   SELECT
049700940616     C* F7=Traduzione
049800940616     C     $TASTO        WHENEQ    F07
049900940616     C                   EXSR      LINGUA
050000940616     C* F6
0501009406162    C     $TASTO        WHENEQ    F06
050200940224     C* eseguo aggiornamento
050300940315    >C                   EXSR      AGGANA
050400940224     C* eseguo operazioni del dopo-aggiornamento
0505009403183    C     *IN99         IFEQ      *OFF
050600940318     C                   EXSR      GESAGG
0507009403183-   C                   ENDIF
0508009406162-   C                   ENDSL
050900940616     C                   ENDIF
051000940203     C*
0511009402241-   C                   ENDSL
051200940224     C*
0513009405062-   C                   ENDIF
051400940117     C                   ENDSR
051500940506     C/EJECT
051600940506     C************************************************************
051700940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
051800940506     C************************************************************
051900940506     C     CTRKD1        BEGSR
052000940506     C                   SETOFF                                       99
052100940506     C                   MOVEA     $FC1          $FC
052200940506     C                   EXSR      CTRKEY
052300940506     C     *IN99         IFEQ      *ON
052400940506     C                   SETON                                        97
052500940506     C                   ENDIF
052600940506     C                   ENDSR
052700940117      ************************************************************
052800940207      * INIZIALIZZAZIONE VIDEATA DATI
052900940117      ************************************************************
053000940127     C     INZD1         BEGSR
053100940131     C*
053200940615     C                   CLEAR                   D1
053300041115     C                   EVAL      §P1CauAbi = '48000'
053400061016     C                   EVAL      §P1MICR = 'N'
053500941006     C                   MOVE      '0'           D1RIS
053600940616     C                   MOVE      KEY002        D1COD
053700940615     C                   MOVE      AZI002        XTBAZI
053800940615     C                   MOVE      LIN002        XTBLIN
053900940616     C                   MOVE      *ALL'0'       XTBKEY
054000940615     C                   MOVE      KEY002        XTBKEY
054100970509     C                   MOVE      UNI002        XTBUNT
054200940615$007 C                   MOVE      'YP1'         XTBCOD
054300940225     C*
054400940207     C*
054500940224     C* valorizzo la chiave univoca per reperire i dati
054600940224     C* se non immissione
0547009403151   >C     OPZ002        IFNE      '01'
054800940315    >C*-------------------------------------------------------------
054900940315    >C* impostare i campi chiave ricevuti
055000940315    >C*-------------------------------------------------------------
055100940318     C*
055200940614     C*
055300940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0554009406142   >C     OPZ002        IFEQ      '02'
055500940315    >C     OPZ002        OREQ      '04'
055600940614     C                   MOVE      '1'           XTBALC
055700940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0558009406142E   C                   ELSE
055900940614     C                   MOVE      '0'           XTBALC
0560009406142-   C                   ENDIF
056100940614     C*
056200940616     C                   MOVE      '1'           XTBRIC
056300970214     C                   CALLB     'XATB'
056400940614     C                   PARM                    XATBDS
056500940318     C*
056600940614     C     XTBERR        IFEQ      '0'
056700940614     C                   MOVEL     XTBUNI        EXTDS
056800970512     c                   move      xtbunt        d1uniTA           8
056900980807     c                   z-add     1             x
057000980807     c                   movea     §p1tpa        d1tpa(x)
057100011005     c                   z-add     11            x
057200011005     c                   movea     §p1tpa1       d1tpa(x)
057300941108     C* Giro la data se non blank
057400970214     c                   move      xtbdsc        xtbdscA          10
057500970214     C     XTBDSCa       IFNE      DATA
057600970214     C                   CALLB     'XDT4000'
057700970214     C                   PARM      XTBDSCA       XDTAMG           10
057800940621     C                   PARM      *ZEROS        XDTGMA            6 0
057900940621     C                   PARM      2             XDTSTA            1 0
058000940621     C                   MOVE      XDTGMA        D1DSC
058100940712     C                   MOVE      XTBRIS        D1RIS
058200941108     C                   ELSE
058300941108     C                   MOVE      *ZEROS        D1DSC
058400941108     C                   ENDIF
058500941108     C                   ENDIF
058600940614     C*
058700940224     C*
058800940224     C* se il rcd da gestire non viene trovato
058900940318     C* oppure è allocato da altro pgm
059000940224     C* ritorno al pgm guida avvertendolo dell'errore
0591009406142    C     XTBERR        IFNE      '0'
059200940224     C                   MOVE      *ON           $FINE
059300940315    >C                   MOVE      '1'           ERR002
059400940224     C*
059500940224     C* altrimenti
0596009402242E   C                   ELSE
059700940224     C* riempio la videata
059800940224     C                   EXSR      RIED1
059900940224     C* richiamo routine dei ctrl per decodificarla
060000940614    >C                   EXSR      CTRD1
060100960228     C                   MOVEL     *IN99         SAV99             1
060200960228     C* Se la gestione è più videate
060300960305    >C                   EXSR      CTRD2
060400960305    >C     *IN99         IFEQ      *ON
060500960305    >C                   MOVEL     *IN99         SAV99             1
060600960305    >C                   ENDIF
060700940224     C                   MOVE      *ALL'0'       IN5098
060800940224     C                   MOVEA     IN5098        *IN(50)
060900940224     C* definizione indicatori su video
061000940224     C                   SETOFF                                       02
0611009402243    C                   SELECT
061200940315    >C     OPZ002        WHENEQ    '02'
061300940224     C                   SETON                                        02
061400940315    >C     OPZ002        WHENEQ    '05'
061500940506     C* disabilita tasti
061600940720     C                   MOVE      '0'           $FC(6)
061700940720     C                   MOVE      '0'           $FC(5)
061800940720     C                   MOVE      '0'           $FC(4)
0619009402243-   C                   ENDSL
062000940224     C*
0621009402242-   C                   ENDIF
062200940223     C*
0623009402241-   C                   ENDIF
062400940224     C*
062500940117     C                   ENDSR
062600940207     C/EJECT
062700940207     C************************************************************
062800940510     C* RIEMPIMENTO VIDEATA  D1
062900940207     C************************************************************
063000940207     C     RIED1         BEGSR
063100940207     C*
063200940207     C*
063300940207     C                   ENDSR
063400940309     C/EJECT
063500940309     C************************************************************
063600940309     C* GESTIONE F03 VIDEO D1
063700940309     C************************************************************
063800940309     C     F03D1         BEGSR
063900940309     C*
064000940309     C                   MOVE      *ON           $FINE
064100940315    >C                   MOVE      '1'           RET002
064200940325     C* fine programma
064300940325     C                   EXSR      ENDPGM
064400940325     C*
064500940325     C                   ENDSR
064600940309     C/EJECT
064700940309     C************************************************************
064800940309     C* GESTIONE F05 VIDEO D1
064900940309     C************************************************************
065000940309     C     F05D1         BEGSR
065100940309     C*
065200940309     C                   MOVE      *ON           $INZD1
065300940309     C*
065400940309     C                   ENDSR
065500940309     C/EJECT
065600940309     C************************************************************
065700940309     C* GESTIONE F08 VIDEO D1
065800940309     C************************************************************
065900940309     C     F08D1         BEGSR
066000940309     C*
066100940309     C                   MOVE      *ON           $FINE
066200940315    >C                   MOVE      '0'           RET002
066300940309     C*
066400940309     C                   ENDSR
066500940309     C/EJECT
066600940309     C************************************************************
066700940309     C* GESTIONE F12 VIDEO D1
066800940309     C************************************************************
066900940309     C     F12D1         BEGSR
067000940309     C*
067100940309     C                   MOVE      *ON           $FINE
067200940315    >C                   MOVE      '2'           RET002
067300940309     C*
067400940309     C                   ENDSR
067500940309     C/EJECT
067600940309     C************************************************************
067700940309     C* GESTIONE F24 VIDEO D1
067800940309     C************************************************************
067900940309     C     F24D1         BEGSR
068000940309     C*
068100940506     C*
068200940506     C                   MOVEA     $FX1          $FX
068300940506     C                   MOVEA     $FC1          $FC
068400940506     C                   MOVEA     $FR1          $FR
068500940506     C                   Z-ADD     $ULKD1        $ULK
068600940506     C                   EXSR      RIEKEY
068700941017     C                   MOVEA     $FC           $FC1
068800940506     C                   Z-ADD     $ULK          $ULKD1            3 0
068900940506     C                   MOVEL     $KEY1         Z1KE1
069000940506     C                   MOVEL     $KEY2         Z1KE2
069100940614     C                   WRITE     Z1
069200940506     C*
069300940309     C                   ENDSR
069400940207     C/EJECT
069500940131     C************************************************************
069600940207     C* CONTROLLO VIDEATA
069700940131     C************************************************************
069800940127     C     CTRD1         BEGSR
069900940131     C*
070000940127     C                   SETOFF                                       99
070100940127     C* non imposto pos. del cursore
070200940127     C                   Z-ADD     0             H1RIGA
070300940127     C                   Z-ADD     0             H1COLO
070400940621     C*
070500940621     C* CTRL DATA
070600940621     C*
070700941109     C                   MOVE      DATA          WRKDSC           10
070800940621     C     D1DSC         IFNE      0
070900970214     C                   CALLB     'XDT4000'
071000941109     C                   PARM                    XDTAMG           10
071100940621     C                   PARM      D1DSC         XDTGMA            6 0
071200940621     C                   PARM      3             XDTSTA            1 0
071300940621     C     XDTSTA        IFNE      0
071400940621     C                   SETON                                        5599
071500940621     C                   ELSE
071600941108     C                   MOVE      XDTAMG        WRKDSC
071700940621     C                   ENDIF
071800940621     C                   ENDIF
071900940223     C*
072000940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
072100940223     C*
0722009403151   >C     OPZ002        IFNE      '04'
072300971110     C*
072400971110     C* controllo codice vuoto
072500971110$017 C     D1COD         IFEQ      *ALL' '
072600971110     C                   SETON                                        5099
072700971110     C                   ENDIF
072800971110     C*
072900980720     c                   move      d1cod         vkcc
073000980720     C                   clear                   KSCA
073100980720     C     KACO          CHAIN     anrco99j                           83
073200980720     C   83              seton                                        6099
073300980720     C                   MOVEL     pdcdesbrev    D1codd
073400980720     C*
073500980720     c                   if        §p1kcc        <> *blanks
073600980720     c                   move      §p1kcc        vkcc
073700980720     C                   clear                   KSCA
073800980720     C     KACO          CHAIN     anrco99j                           83
073900980720     C   83              seton                                        6199
074000980720     C                   MOVEL     pdcdesbrev    D1kccd
074100980720     C*
074200980720     C     §p1ksc        IFNE      *blank
074300980720     C                   movel     §p1ksc        KSCA
074400980720     C     KACO          CHAIN     anrco99j                           90
074500981022     C  n90              MOVEL     pdcdesbrev    D1kscd
074600981022     C     *in90         ifeq      *off
074700981022     C     rcosogg       andne     *blanks
074800981022     C     rcosogg       chain     ansog01l                           90
074900981022     C  n90              MOVEL     sogdes        D1kscd
075000981022     C                   endif
075100980720     C*
075200980720     C     *IN90         IFEQ      '1'
075300980720     C                   SETON                                        6299
075400980720     C                   ELSE
075500980720     c                   endif
075600980720     c                   endif
075700980720     c                   endif
075800940207     C*
075900980720     c                   if        §p1cau        <> *blanks
076000980720     c                   move      §p1cau        kkcau             4
076100980720     c     k02ope01      chain     ANope01l                           70
076200980720     c                   if        *in70         = *on
076300980720     c                   seton                                        6399
076400980720     c                   else
076500980720     c                   movel     opedesbrev    d1caud
076600980720     c                   endif
076700980720     c                   endif
076800980720     c*
076900980720     c                   if        §p1cab        <> *blanks
077000980720     c                   move      §p1cab        kkcau             4
077100980720     c     k02ope01      chain     ANope01l                           70
077200980720     c                   if        *in70         = *on
077300980720     c                   seton                                        6499
077400980720     c                   else
077500980720     c                   movel     opedesbrev    d1cabd
077600980720     c                   endif
077700980720     c*
077800980807     c                   movel     '02'          Vtip
077900011005     C*
078000011008     c                   z-add     10            b
078100011008     c                   do        20            a
078200011008     c                   if        a = 11
078300011008     c                   z-add     70            b
078400011008     c                   end
078500011008     c                   add       1             b
078600011008     c                   move      '0'           *in(b)
078700011008     c                   if        d1tpa(a) <> ' '
078800011008     c                   movel     d1tpa(a)      vtip4
078900011005     C                   exsr      CtrTab0A1
079000011005     c                   if        *in99         = *on
079100011008     c                   move      *on           *in(b)
079200011005     c                   else
079300011008     c                   move      *off          *in(b)
079400011005     c                   end
079500011005     c                   end
079600011005     c                   enddo
079700980807      *
079800980807     c                   endif
079900980807      *
080000940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
080100940315    >C     OPZ002        IFEQ      '01'
080200940315    >C     OPZ002        OREQ      '03'
080300940315    >C*-------------------------------------------------------------
080400940315    >C* impostare i campi chiave e verificare l'esistenza del record
080500940315    >C*-------------------------------------------------------------
080600971110$007 C                   MOVE      'YP1'         XTBCOD
080700940614     C                   MOVE      *ALL'0'       XTBKEY
080800940620    >C                   MOVE      D1COD         XTBKEY
080900970512    >C                   MOVE      D1UNITA       XTBUNT
081000940614     C                   MOVE      '0'           XTBALC
081100940614     C                   MOVE      '1'           XTBRIC
081200970214    >C                   CALLB     'XATB'
081300940614     C                   PARM                    XATBDS
081400940614     C     XTBERR        IFEQ      '0'
081500940223     C                   SETON                                        5999
081600940223     C                   ENDIF
081700940620     C* Controllo se compreso nei range indice
081800940620     C     XTBERR        IFEQ      '3'
081900940620    >C                   SETON                                        5899
082000940620     C                   END
082100940620     C*
082200940620     C* REPERISCO AUTORIZZAZIONI UTENTE
082300940620     C                   CLEAR                   XCHATB
082400941118     C                   MOVE      XSCSOC        XCTSOC
082500941118     C                   MOVE      XSCDUN        XCTUNI
082600940620     C                   MOVEL     KNMUS         XCTPRF
082700940620     C                   MOVE      XTBCOD        XCTCOD
082800940620     C                   CALL      'XCHKATB'
082900940620     C                   PARM                    XCHATB
083000941111     C                   MOVEL     XCTDDS        AUTDS
083100941027     C     XCTRTN        IFEQ      1
083200941027     C                   MOVEL     'N'           MANGRU
083300941027     C                   ENDIF
083400940620     C* Non ammessa immissione di gruppo
083500940620     C     XTBGRU        IFEQ      'G'
083600940620     C     MANGRU        ANDEQ     'N'
083700940620     C                   SETON                                        5799
083800940620     C                   END
083900940620     C* Non ammessa immissione di azienda
084000940620     C     XTBGRU        IFEQ      'A'
084100940620     C     MANAZI        ANDEQ     'N'
084200940620     C                   SETON                                        5699
084300940620     C                   END
084400940712     C* codice riservato non accessibile
084500941006     C     XTBRIS        IFGT      ABIRIS
084600941005    >C     OPZ002        ANDNE     '01'
084700941005    >C     OPZ002        ANDNE     '03'
084800940712     C                   SETON                                        5499
084900940712     C                   END
085000940224     C                   ENDIF
085100940224     C*
0852009402241-   C                   END
085300940131     C*
085400940302     C     *IN99         IFEQ      *ON
085500940510     C     $GEST         ANDEQ     'D1'
085600960228     C     OPZ002        ANDNE     '07'
085700940117     C                   MOVEA     *IN           WIN
085800940128     C                   MOVE      *ALL'0'       IN5098           49
085900940128     C                   MOVEA     IN5098        *IN(50)
086000940614     C                   WRITE     D1
086100940117     C                   MOVEA     WIN           *IN
086200940302     C                   ENDIF
086300940127     C*
086400940117     C                   ENDSR
086500980807     C*
086600980807     C*
086700980807     C*
086800980807     c     CtrTab0A1     begsr
086900980807     C*
087000980807     c                   setoff                                       99
087100980807     c                   clear                   xtabds
087200980807     C                   MOVE      Xsccut        XTBUTE
087300980807     C                   MOVE      '1'           XTBRIC
087400980807     C                   MOVE      Xscsoc        XTBAZI
087500980807     C                   MOVE      *ZERO         XTBKEY
087600980807     C                   MOVE      '0A1'         XTBCOD
087700980807     C                   MOVE      Vtip          XTBKEY
087800980807     C                   CALL      'XATB'
087900980807     C                   PARM                    XATBDS
088000980807    4C     XTBERR        IFEQ      *ON
088100980807     C                   SETON                                          99
088200980807     c                   endif
088300980807     C*
088400980807     C                   ENDSR
088500960305     C************************************************************
088600960305     C* CONTROLLO VIDEATA D2
088700960305     C************************************************************
088800960305     C     CTRD2         BEGSR
088900960305     C*
089000960305     C                   ENDSR
089100940207     C/EJECT
089200940224     C************************************************************
089300940315    >C* AGGIORNAMENTO ANAGRAFICA
089400940224     C************************************************************
089500940315    >C     AGGANA        BEGSR
089600971110     C*
089700971110$007 C                   MOVE      'YP1'         XTBCOD
089800940614     C                   MOVE      *ALL'0'       XTBKEY
089900940614     C                   MOVE      D1COD         XTBKEY
090000970512     C                   MOVE      D1UNITA       XTBUNT
090100940207     C*
090200940207     C                   SELECT
090300940315    >C     OPZ002        WHENEQ    '01'
090400940315    >C     OPZ002        OREQ      '03'
090500940315    >C                   EXSR      RIEANA
090600950609     C                   MOVE      XSCCUT        XTBUTE
090700940614     C                   MOVE      '5'           XTBRIC
090800970214     C                   CALLB     'XATB'
090900940614    >C                   PARM                    XATBDS
091000940318     C* se rcd non scrivibile attivo errore
091100940615     C     XTBERR        IFEQ      '1'
091200940318    >C                   SETON                                        5999
091300940318     C                   END
091400940315    >C     OPZ002        WHENEQ    '02'
091500940621     C                   MOVE      '1'           XTBRIC
091600940621     C                   MOVE      '1'           XTBALC
091700970214     C                   CALLB     'XATB'
091800940621     C                   PARM                    XATBDS
091900940622    >C                   EXSR      RIEANA
092000950609     C                   MOVE      XSCCUT        XTBUTE
092100940614     C                   MOVE      '4'           XTBRIC
092200970214     C                   CALLB     'XATB'
092300940614    >C                   PARM                    XATBDS
092400940315    >C     OPZ002        WHENEQ    '04'
092500940621     C                   MOVE      '1'           XTBRIC
092600940621     C                   MOVE      '1'           XTBALC
092700970214     C                   CALLB     'XATB'
092800940621     C                   PARM                    XATBDS
092900940614     C                   MOVE      '6'           XTBRIC
093000970214     C                   CALLB     'XATB'
093100940614    >C                   PARM                    XATBDS
093200940621     C* annullo traduzioni
093300940621     C     XTBERR        IFEQ      '0'
093400940621     C                   EXSR      ANNTBT
093500940621     C                   END
093600940621     C*
093700940207     C                   ENDSL
093800940203     C*
093900940203     C                   ENDSR
094000940224     C/EJECT
094100940224     C************************************************************
094200940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
094300940224     C************************************************************
094400940616     C     RIEANA        BEGSR
094500940616     C                   CLEAR                   XTBUNI
094600940621     C                   MOVE      WRKDSC        XTBDSC
094700940712     C                   MOVE      D1RIS         XTBRIS
094800011005     C                   MOVEl(p)  vtipa         §p1tpa
094900011005     C                   MOVEl(p)  vtipa1        §p1tpa1
095000940616     C                   MOVEL     EXTDS         XTBUNI
095100940207     C                   ENDSR
095200940224     C/EJECT
095300940224     C************************************************************
095400940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
095500940224     C************************************************************
095600940224     C     GESAGG        BEGSR
095700940224     C*
095800940224     C* segnalo al pgm chiamante l'avvenuta conferma
095900940315    >C                   MOVE      *ON           OPR002
096000940224     C* segnalo al pgm chiamante l'aver premuto l'F6
096100940315    >C                   MOVE      '0'           RET002
096200940224     C*
0963009402241    C                   SELECT
096400940224     C* nel caso di immissione, ripristino la videata iniziale
096500940315    >C     OPZ002        WHENEQ    '01'
096600940224     C                   MOVE      *ON           $INZD1
096700940224     C* altrimenti ritorno al pgm chiamante
096800940224     C                   OTHER
096900940224     C                   MOVE      *ON           $FINE
0970009402241-   C                   ENDSL
097100940224     C*
097200940224     C                   ENDSR
097300940621     C/EJECT
097400940621     C************************************************************
097500940621    >C* ANNULLAMENTO ARCHIVIO TRADUZIONI
097600940621     C************************************************************
097700940621    >C     ANNTBT        BEGSR
097800940621     C                   MOVE      XTBAZI        TBTAZI
097900940621     C                   MOVE      XTBCOD        TBTCOD
098000940621     C                   MOVE      XTBKEY        TBTKEY
098100940621     C     KTBT          SETLL     ANTBT01L
098200940621     C                   DO        *HIVAL
098300940621     C     KTBT          DELETE    ANTBT01L                           21
098400940621     C  N21              END
098500940621     C*
098600940621     C                   ENDSR
098700940207     C/EJECT
098800940131     C************************************************************
098900940131     C* OPERAZIONI INIZIALI
099000940131     C************************************************************
099100940131     C     *INZSR        BEGSR
099200940131     C*
099300940127     C* Reperimento parametri
099400940127     C*
099500940117     C     *ENTRY        PLIST
099600940117     C                   PARM                    KPJBA
099700940712     C*
099800940712     C* Reperimento Codice utente
099900940712     C*
100000940223     C*
100100940506     C*
100200940506     C* Reperimento tasti di funzione
100300940506     C*
100400940506     C                   EXSR      REPKEY
100500940621     C*
100600940621     C* KLIST
100700940621     C*
100800940621     C     KTBT          KLIST
100900940621     C                   KFLD                    TBTAZI
101000940621     C                   KFLD                    TBTCOD
101100940621     C                   KFLD                    TBTKEY
101200980720     C*                                                    *
101300980720     C     KACO          KLIST
101400980720     C                   KFLD                    xscsoc
101500980720     C                   KFLD                    VKCC              6
101600980720     C                   KFLD                    KSCA              8
101700980720     C*                                                    *
101800980720     C     K02ope01      KLIST
101900980720     C                   KFLD                    xscsoc
102000980720     C                   KFLD                    kkcau
102100940506     C*
102200940223     C                   ENDSR
102300941103     C/EJECT
102400941103      ************************************************************
102500941103      * Reperimento dati società
102600941103      ************************************************************
102700941103     C     REPSOC        BEGSR
102800941103      *
102900970214     C                   CALLB     'XSOC'
103000941103     C                   PARM                    TIPXSC            6
103100941103     C                   PARM                    SOCXSC            3
103200941103     C                   PARM                    CDSXSC            9 0
103300941103     C                   PARM                    MODXSC            3
103400941103     C                   PARM      *BLANKS       RTNXSC            1
103500941103     C                   PARM                    XSOCDS
103600941103     C                   PARM                    KPJBA
103700941103      *
103800941103     C                   ENDSR
103900940223     C/EJECT
104000940506     C************************************************************
104100940506     C* REPERIMENTO TASTI DI FUNZIONE
104200940506     C************************************************************
104300940506     C     REPKEY        BEGSR
104400940506     C                   MOVEA     $FM1          $FM
104500940506     C                   EXSR      REPKKK
104600940506     C                   MOVEA     $FX           $FX1
104700940614     C                   ENDSR
104800940506     C/EJECT
104900940506     C************************************************************
105000940506     C* REPERIMENTO TASTI DI FUNZIONE
105100940506     C************************************************************
105200940506     C     REPKKK        BEGSR
105300940506     C*
105400940506     C                   CLEAR                   $FX
105500940506     C     1             DO        24            X
105600940506     C*
105700940506     C     $FM(X)        IFNE      *BLANK
105800940506     C*
105900940506     C                   MOVEL     $FM(X)        IDMSG
106000970214     C                   CALLB     'XRTVM'
106100940506     C                   PARM                    IDMSG            27
106200940506     C                   PARM                    TXTMSG          100
106300940506     C                   PARM                    ERRMSG            1
106400940506     C*
106500940506     C     ERRMSG        IFNE      *ON
106600940506     C                   MOVEL     TXTMSG        $FX(X)
106700940506     C                   ELSE
106800940506     C                   MOVEL     *ALL' '       $FX(X)
106900940506     C                   END
107000940506     C*
107100940506     C                   END
107200940506     C*
107300940506     C                   END
107400940506     C*
107500940506     C                   ENDSR
107600940509     C/EJECT
107700940223     C************************************************************
107800940223     C* INIZIALIZZAZIONE VARIABILI
107900940223     C************************************************************
108000940223     C     INZVAR        BEGSR
108100951124     C*
108200951124     C* Chiamata XSOC
108300951124     C*
108400951124     C                   MOVEL     'SOC001'      TIPXSC
108500951124     C                   EXSR      REPSOC
108600951124     C     RTNXSC        IFNE      '1'
108700951124     C                   MOVEL     XSOCDS        SOC001
108800951124     C                   ELSE
108900951124     C                   MOVE      *ON           $FINE
109000951124     C* fine programma
109100951124     C                   EXSR      ENDPGM
109200951124     C                   ENDIF
109300940224     C*
109400940224     C* Dati da PGM guida
109500940224     C*
109600940315    >C                   MOVEL     KPJBU         D002DS
109700940128     C*
109800940223     C* Variabili per gestione videate
109900940128     C*
110000940223     C                   MOVE      'D1'          $GEST
110100940223     C                   MOVE      *OFF          $FINE
110200940223     C                   MOVE      *ON           $INZD1
110300940224     C                   MOVE      *BLANK        $LASTG
110400940224     C                   MOVE      *BLANK        $LASTV
110500940506     C                   Z-ADD     0             $ULKD1            3 0
110600940127     C*
110700940223     C* Variabili appoggio
110800940127     C*
110900940224     C                   Z-ADD     0             CURR
111000940224     C                   Z-ADD     0             CURC
111100940207     C*
111200940207     C* Valorizzazione campi univoci testate
111300940207     C*
111400940614     C                   CLEAR                   T1
111500940614     C                   MOVEL     KNSIF         NOMSIF
111600941109     C                   MOVEL     XSCDSI        NOMDIT
111700941027     C                   MOVEL     DSPGM         NOMPGM
111800940224     C                   Z-ADD     1             X
111900940315    >C     OPZ002        LOOKUP    $OZ(X)                                 21
112000940224     C     *IN21         IFEQ      *ON
112100940224     C                   MOVEL     $ID(X)        IDMSG
112200970214     C                   CALLB     'XRTVM'
112300940224     C                   PARM                    IDMSG            27
112400940224     C                   PARM                    TXTMSG          100
112500940224     C                   PARM                    ERRMSG            1
112600940224     C                   PARM      *ON           CTRMSG            1
112700940224     C                   PARM      30            LENMSG            3 0
112800940224     C     ERRMSG        IFNE      *ON
112900940224     C                   MOVEL     TXTMSG        T1OPE
113000940224     C                   ELSE
113100940224     C                   MOVEL     *ALL'?'       T1OPE
113200940224     C                   ENDIF
113300940224     C                   END
113400940224     C*
113500940224     C* Inizializzazione/Decodifica prima videata
113600940224     C*
113700940224     C                   EXSR      INZD1
113800940224     C                   MOVE      *OFF          $INZD1
113900960228     C* Se opzione = '07' richiesti solo controlli
114000960228     C     OPZ002        IFEQ      '07'
114100960228     C                   MOVEL     *IN99         ERR002
114200960228     C                   MOVEL     '0'           RET002
114300960228     C                   MOVEL     *ON           $FINE
114400960228     C                   ENDIF
114500940127     C*
114600940117     C                   ENDSR
114700940128     C************************************************************
114800940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
114900940223     C************************************************************
115000940223     C     DEFCAM        BEGSR
115100940223     C*
115200940223     C* klist
115300940207     C*
115400940223     C*
115500940223     C* Variabili per gestione videate
115600940223     C*
115700940223     C                   MOVE      *BLANK        $GEST             2
115800940223     C                   MOVE      *BLANK        $FINE             1
115900940223     C                   MOVE      *BLANK        $INZD1            1
116000940223     C                   MOVE      *BLANK        $LASTG            2
116100940223     C                   MOVE      *BLANK        $LASTV            2
116200940223     C*
116300940223     C* Variabili appoggio
116400940223     C*
116500940223     C                   Z-ADD     0             CURR              2 0
116600940223     C                   Z-ADD     0             CURC              2 0
116700940223     C                   MOVE      *ZEROS        WIN              99
116800940224     C                   MOVE      *BLANK        WTBERR            1
116900940223     C*
117000940223     C* Indici
117100940223     C*
117200940223     C                   Z-ADD     0             X                 3 0
117300940223     C                   Z-ADD     0             Y                 3 0
117400940127     C*
117500940117     C                   ENDSR
117600940128     C************************************************************
117700940509     C/EJECT
117800940509     C************************************************************
117900940509     C* CONTROLLO ABILITAZIONE TASTI
118000940509     C************************************************************
118100940509     C     CTRKEY        BEGSR
118200940509     C                   SELECT
118300940509     C     $TASTO        WHENEQ    F01
118400940720     C     $FC(1)        IFEQ      '0'
118500940509     C                   SETON                                        99
118600940509     C                   ENDIF
118700940509     C     $TASTO        WHENEQ    F02
118800940720     C     $FC(2)        IFEQ      '0'
118900940509     C                   SETON                                        99
119000940509     C                   ENDIF
119100940509     C     $TASTO        WHENEQ    F03
119200940720     C     $FC(3)        IFEQ      '0'
119300940509     C                   SETON                                        99
119400940509     C                   ENDIF
119500940509     C     $TASTO        WHENEQ    F04
119600940720     C     $FC(4)        IFEQ      '0'
119700940509     C                   SETON                                        99
119800940509     C                   ENDIF
119900940509     C     $TASTO        WHENEQ    F05
120000940720     C     $FC(5)        IFEQ      '0'
120100940509     C                   SETON                                        99
120200940509     C                   ENDIF
120300940509     C     $TASTO        WHENEQ    F06
120400940720     C     $FC(6)        IFEQ      '0'
120500940509     C                   SETON                                        99
120600940509     C                   ENDIF
120700940509     C     $TASTO        WHENEQ    F07
120800940720     C     $FC(7)        IFEQ      '0'
120900940509     C                   SETON                                        99
121000940509     C                   ENDIF
121100940509     C     $TASTO        WHENEQ    F08
121200940720     C     $FC(8)        IFEQ      '0'
121300940509     C                   SETON                                        99
121400940509     C                   ENDIF
121500940509     C     $TASTO        WHENEQ    F09
121600940720     C     $FC(09)       IFEQ      '0'
121700940509     C                   SETON                                        99
121800940509     C                   ENDIF
121900940509     C     $TASTO        WHENEQ    F10
122000940720     C     $FC(10)       IFEQ      '0'
122100940509     C                   SETON                                        99
122200940509     C                   ENDIF
122300940509     C     $TASTO        WHENEQ    F11
122400940720     C     $FC(11)       IFEQ      '0'
122500940509     C                   SETON                                        99
122600940509     C                   ENDIF
122700940509     C     $TASTO        WHENEQ    F12
122800940720     C     $FC(12)       IFEQ      '0'
122900940509     C                   SETON                                        99
123000940509     C                   ENDIF
123100940509     C     $TASTO        WHENEQ    F13
123200940720     C     $FC(13)       IFEQ      '0'
123300940509     C                   SETON                                        99
123400940509     C                   ENDIF
123500940509     C     $TASTO        WHENEQ    F14
123600940720     C     $FC(14)       IFEQ      '0'
123700940509     C                   SETON                                        99
123800940509     C                   ENDIF
123900940509     C     $TASTO        WHENEQ    F15
124000940720     C     $FC(15)       IFEQ      '0'
124100940509     C                   SETON                                        99
124200940509     C                   ENDIF
124300940509     C     $TASTO        WHENEQ    F16
124400940720     C     $FC(16)       IFEQ      '0'
124500940509     C                   SETON                                        99
124600940509     C                   ENDIF
124700940509     C     $TASTO        WHENEQ    F17
124800940720     C     $FC(17)       IFEQ      '0'
124900940509     C                   SETON                                        99
125000940509     C                   ENDIF
125100940509     C     $TASTO        WHENEQ    F18
125200940720     C     $FC(18)       IFEQ      '0'
125300940509     C                   SETON                                        99
125400940509     C                   ENDIF
125500940509     C     $TASTO        WHENEQ    F19
125600940720     C     $FC(19)       IFEQ      '0'
125700940509     C                   SETON                                        99
125800940509     C                   ENDIF
125900940509     C     $TASTO        WHENEQ    F20
126000940720     C     $FC(20)       IFEQ      '0'
126100940509     C                   SETON                                        99
126200940509     C                   ENDIF
126300940509     C     $TASTO        WHENEQ    F21
126400940720     C     $FC(21)       IFEQ      '0'
126500940509     C                   SETON                                        99
126600940509     C                   ENDIF
126700940509     C     $TASTO        WHENEQ    F22
126800940720     C     $FC(22)       IFEQ      '0'
126900940509     C                   SETON                                        99
127000940509     C                   ENDIF
127100940509     C     $TASTO        WHENEQ    F23
127200940720     C     $FC(23)       IFEQ      '0'
127300940509     C                   SETON                                        99
127400940509     C                   ENDIF
127500940509     C     $TASTO        WHENEQ    F24
127600940720     C     $FC(24)       IFEQ      '0'
127700940509     C                   SETON                                        99
127800940509     C                   ENDIF
127900940509     C                   ENDSL
128000940509     C                   ENDSR
128100940509     C/EJECT
128200940509     C************************************************************
128300940509     C* RIEMPIMENTO TASTI FUNZIONE
128400940509     C************************************************************
128500940509     C     RIEKEY        BEGSR
128600940509     C*
128700940509     C* Input :
128800940509     C* - $FX = schiera con tutti i testi disponibili
128900940509     C* - $FC = schiere con i flag di validità associati ai testi
129000940509     C* - $FR = schiere con i flag di REVERSE IMAGE
129100940509     C* - $ULK = ultimo testo caricato
129200940509     C* Output :
129300940509     C* - $ULK
129400940509     c* - $KEY1= 1° riga di tasti funzione
129500940509     c* - $KEY2= 2° riga di tasti funzione
129600940509     C*
129700970214     C                   CALLB     'XKEYMSG'
129800940509     C* input
129900940509     C                   PARM                    $FX
130000940509     C                   PARM                    $FC
130100940509     C                   PARM                    $FR
130200940509     C                   PARM                    $ULK              3 0
130300940509     C* output
130400940509     C                   PARM                    $KEY1            79
130500940509     C                   PARM                    $KEY2            79
130600940509     C*
130700940509     C                   ENDSR
130800940615**
13090094022401PROMSG    *LIBL     COS0001    immissione
13100094022402PROMSG    *LIBL     COS0002    modifica
13110094022403PROMSG    *LIBL     COS0003    copia
13120094022404PROMSG    *LIBL     COS0004    annullamento
13130094022405PROMSG    *LIBL     COS0005    visualizzazione
131400940509** TASTI DI FUNZIONE  D1
131500940509PROMSG    *LIBL     KEY0001  01
131600940509PROMSG    *LIBL     KEY0002  02
131700940509PROMSG    *LIBL     KEY0003  03
131800940509PROMSG    *LIBL     KEY0004  04
131900940509PROMSG    *LIBL     KEY0005  05
132000940509PROMSG    *LIBL     KEY0006  06
132100940509PROMSG    *LIBL     KEY0007  07
132200940509PROMSG    *LIBL     KEY0008  08
132300940509PROMSG    *LIBL     KEY0009  09
132400940509PROMSG    *LIBL     KEY0010  10
132500940509PROMSG    *LIBL     KEY0011  11
132600940509PROMSG    *LIBL     KEY0012  12
132700940509PROMSG    *LIBL     KEY0013  13
132800940509PROMSG    *LIBL     KEY0014  14
132900940509PROMSG    *LIBL     KEY0015  15
133000940509PROMSG    *LIBL     KEY0016  16
133100940509PROMSG    *LIBL     KEY0017  17
133200940509PROMSG    *LIBL     KEY0018  18
133300940509PROMSG    *LIBL     KEY0019  19
133400940509PROMSG    *LIBL     KEY0020  20
133500940509PROMSG    *LIBL     KEY0021  21
133600940509PROMSG    *LIBL     KEY0022  22
133700940509PROMSG    *LIBL     KEY0023  23
133800940509PROMSG    *LIBL     KEY0024  24
133900940509**  ABILITAZIONE D1
1340009407200N  01
1341009407200N  02
1342009407201N  03
1343009407201N  04
1344009407201N  05
1345009407201N  06
1346009407201N  07
1347009407201N  08
1348009407200N  09
1349009407200N  10
1350009407200N  11
1351009407201N  12
1352009407200N  13
1353009407200N  14
1354009407200N  15
1355009407200N  16
1356009407200N  17
1357009407200N  18
1358009407200N  19
1359009407200N  20
1360009407200N  21
1361009407200N  22
1362009407200N  23
1363009407200N  24
