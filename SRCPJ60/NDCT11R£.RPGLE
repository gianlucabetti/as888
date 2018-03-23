000100981019     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND) ACTGRP(QILE)
000200981019     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400941115      *
000500970416    > *            Estratto conto tesoreria - a movimenti
000600951108      *           ----------------------------------------------
000700940224      *
000800940128     F*----------------------------------------------------*
000900951127     F* Questo pgm può essere richiamato in 1 modO:
001000951115     F* 02 = Gestione
001100951115     F* Indicatori che condizionano campi a video:
001200951115     F* 11 Unita
001300951127     F* 14 sottolineo causale se esistono descriz aggiuntive
001400970129     F* 41 protegge opzione nel caso di riga saldo e se registraz. ancora
001500970129     F*    non confermata (causa commit)
001600951115    >F*----------------------------------------------------------------
001700940314    >F* Viste logiche utilizzate per i diversi ordinamenti
001800940314    >F*----------------------------------------------------------------
001900940909     F*------------
002000970416    >FNDBMV03L  IF   E           K DISK
002100970416    >F                                     RENAME(NDBMV000:NDBMV03)
002200970416    >FNDBMV04L  IF   E           K DISK
002300970416    >F                                     RENAME(NDBMV000:NDBMV04)
002400951127     FNDDES01L  IF   E           K DISK
002500951127     FNDREG01L  IF   E           K DISK
002600970416     FNDMOV01L  IF   E           K DISK
002700940211     F*------------
002800970416    >FNDCT11D   CF   E             WORKSTN
002900970416     F                                     SFILE(CT11S1:S1NRR)
003000940201     F                                     INFDS(DSFMT)
003100940128     D*----------------------------------------------------*
003200940302     D* Schiere per la gestione delle opzioni
003300940302     D*
003400950614     D* dimensione della schiera $MS1
003500040212C1718X*$DIMOP          C                   CONST(9)
003501040212C1718D $DIMOP          C                   CONST(15)
003600950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003700950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
003800950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003900950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
004000941214     D $TX             S             50    DIM(50)
004100941214     D $FL             S              1    DIM(50)
004200940909     D* Schiere generali per la gestione dei tasti di funzione
004300940506     D $FM             S             27    DIM(24)
004400940506     D $FX             S             50    DIM(24)
004500940506     D $FC             S              1    DIM(24)
004600940506     D $FR             S              1    DIM(24)
004700940909     D* Schiere per la gestione dei tasti di funzione
004800951108     D* Video S1
004900940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005000940506     D $FX1            S             50    DIM(24) ALT($FM1)
005100940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005200940506     D $FR1            S              1    DIM(24) ALT($FC1)
005300940302     D*
005400940211     D/EJECT
005500951109     D*-------------
005600951109     D* Interrogazione conti
005700951206     D ANA399DS      E DS                  inz
005800950524     D*-------------
005900940211     D* Passaggio Parametri
006000940211     D KPJBA         E DS
006100940211     D*-------------
006200940225     D* Reperimento Autorizzazioni
006300941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
006400941124     D*-------------
006500950410     D* Passaggio Autorizzazioni
006600950410     D DSAUT           DS           512
006700970422     D*-------------
006800970422     D* Parametri gestione tesoreria
006900970422     D ANP021DS      E DS
007000970422     D*-------------
007100970422     D* DS gestione parametri modulo
007200970422     D XPAOUT          DS          2000
007300950524     D*-------------
007400950524     D* Maschera dati autorizzazioni
007500970416    >D CONTESDS      E DS
007600941209     D*-------------
007700941209     D* Dati di ambiente ottenuti da XSOC
007800941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
007900950906     D    FgO1                        1    overlay(XscFgO)
008000961121     D*-------------
008100961121     D* Chiamata ricorsiva
008200961121    >D X48CHPGMDS    E DS                  INZ
008300951109     D*-------------
008400951109     D* Parametri per PGM X01CAMBI
008500951109     D X01CambiDs    E DS                  inz
008600951109     D*-------------                       INZ
008700951109     D* Routine editazione di un numero
008800951110     D $SrcVar         s             30S 0
008900951110     D $SrcVarPr       s              9B 0 inz(30)
009000951109     D $SrcVarDe       s              9B 0
009100951117     D $EdtCode        s              1    inz('1')
009200951109     D $LenFld         s              3S 0
009300951109     D $RcvrVar        s             50
009400951109     D $Err            s              1
009500940325     D*-------------
009600940325     D* Parametri in ricezione
009700970416    >D NDCT11DS      E DS
009800970417     D*-------------
009900970417     D* Parametri routine saldi
010000980120    >D NDCT21DS      E DS                  inz occurs(2)
010100980121    >D AppT21DS        s                   like(NDCT21DS) dim(2)
010200970515     D*-------------
010300970605     D* Parametri pgm per riconciliazione
010400970605    >D NDCT14DS      E DS                  inz
010500970613     D*-------------
010600970613     D* Parametri pgm per Sriconciliazione
010700970613    >D NDCT15DS      E DS                  inz
010800970710    >D  OPRT15       E                     INZ('0')
010900970710    >D  ERRT15       E                     INZ('0')
011000970724     d*-------------
011100970724     D* Ds per opzione riconciliazione
011200970724     D NdcT43Ds      E DS                  Inz
011300970724    >D  OPRT43       E                     INZ('0')
011400970724    >D  RetT43       E                     INZ('0')
011500970724    >D  ERRT43       E                     INZ('0')
011600970710     D*-------------
011700970710     D* ds per attaccare nr. blocco in errore al messaggio di errore
011800970710     D                 DS
011900970710     D  C1ndc0315              1      8
012000970710     D  Pack0315               1      8P 0
012100970605     D*-------------
012200970515     D* Parametri routine stampa ec        inz
012300970515    >D NDCT31DS      E DS
012400951128     D*-------------
012500951128     D* Parametri del PGM descrizioni aggiuntive
012600951128     D NDC027DS      E DS                  INZ
012700951128     D  RET027       E                     INZ('0')
012800951128     D  OPR027       E                     INZ('0')
012900951128     D  ERR027       E                     INZ('0')
013000951128     D*-------------
013100951128     D* Parametri per gestione p.nota interattiva
013200951128     D NDC001DS      E DS                  INZ
013300951128     D  RET001       E                     INZ('0')
013400951128     D  OPR001       E                     INZ('0')
013500951128     D  ERR001       E                     INZ('0')
013600951205     D  DTREG1001    E                     inz('0001-01-01')
013700951205     D  DTREG2001    E                     inz('0001-01-01')
013800951205     D  DTLGIO1001   E                     inz('0001-01-01')
013900951205     D  DTLGIO2001   E                     inz('0001-01-01')
014000951205     D  DTULMO1001   E                     inz('0001-01-01')
014100951205     D  DTULMO2001   E                     inz('0001-01-01')
014200951128     D  DTDOC001     E                     inz('0001-01-01')
014300951128     D*-------------
014400960318      * Parametri per call pgm visualizzazione partite
014500960318     D NDC073DS      E DS                  INZ
014600960318     D  OPR073       E                     INZ('0')
014700960318     D  RET073       E                     INZ('0')
014800960318     D  ERR073       E                     INZ('0')
014900960318     D  Exf073       E                     INZ('0')
015000951128     D*-------------
015100951128      * Parametri per call diretta finestra tesoreria
015200951128     D NDC084DS      E DS                  INZ
015300951128     D  RET084       E                     INZ('0')
015400951128     D  ERR084       E                     INZ('0')
015500951128     D  Exf084       E                     INZ('0')
015501040212c1718D*-------------
015502040212  "   * Parametri per call CASH FLOW
015503040212  "  D NDCt68DS      E DS                  INZ
015504040212  "  D  RETt68       E                     INZ('0')
015505040212  "  D  errt68       E                     INZ('0')
015506040212c1718D  INZT68       E                     INZ('0')
015507051205c1886D*-------------
015508051205  "   * Parametri per interrogazione movimenti HB
015509051205  "  D NDCK40DS      E DS                  INZ
015510051205  "  D  RETK40       E                     INZ('0')
015511051205  "  D  errK40       E                     INZ('0')
015512051205  "  D  DaTCARDK40   E                     inz('0001-01-01')
015513051205  "  D  DaTCARAK40   E                     inz('0001-01-01')
015514051205  "  D  DOPDAK40     E                     inz('0001-01-01')
015515051205  "  D  DOPAK40      E                     inz('0001-01-01')
015516051205  "  D  DVADAK40     E                     inz('0001-01-01')
015517051205C1886D  DVAAK40      E                     inz('0001-01-01')
015600951127     D*-------------
015700951127     D* Controlli validità legende
015800951127     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
015900951127     D*-------------
016000970416     D* ds campi BOB
016100970416     D ANBOB         E DS                  inz ExtName(ANBOB00f)
016200940211     D*-------------
016300940211     D DSFMT           DS           512
016400940506     D  $TASTO               369    369
016500940211     D  NRG                  370    370
016600940211     D  NCL                  371    371
016700940211     D  SFLNRR               378    379B 0
016800940211     D*-------------
016900940211     D* posizione cursore
017000940211     D CURSOR          DS
017100940211     D  RIRI                   1      2B 0 INZ
017200940211     D  R$$                    2      2
017300940211     D  COCO                   3      4B 0 INZ
017400940211     D  C$$                    4      4
017500940127     D*-------------
017600940127     D* Reperimento nome PGM
017700940127     D STATUS         SDS           333
017800940127     D  DSPGM            *PROC
017900950411     D  PARMS            *PARMS
018000951127     D*-------------
018100951127     D* Definizioni parametri standard chiamate moduli
018200951127     D Esito           S              1
018300951127     D gesmsg          S              1
018400951127     D lenout          S              9B 0
018500951127     D Data            S             10
018600951127     D strutturaO      S             10
018700951127     D Operazione      S              2
018800951127     D LenIn           S              9B 0
018900951127     D refresh         S              1
019000951127     D uso             S              1
019100951127     DOutput           S           4000
019200970417     D dtope           S               D
019300950704     D*-------------
019400950704     D* Attributi video
019500950704     D* (definisco come costanti qugli attributi video che potrei
019600950704     D*  testare e quindi preferisco assegnargli un valore "parlante")
019700950704     D*  bottone presente
019800950704     D PRESENTE        C                   CONST(X'A1')
019900940616     D*-------------
020000940506     D* Tasti di funzione
020100940506     D F01             C                   CONST(X'31')
020200940506     D F02             C                   CONST(X'32')
020300940506     D F03             C                   CONST(X'33')
020400940506     D F04             C                   CONST(X'34')
020500940506     D F05             C                   CONST(X'35')
020600940506     D F06             C                   CONST(X'36')
020700940506     D F07             C                   CONST(X'37')
020800940506     D F08             C                   CONST(X'38')
020900940506     D F09             C                   CONST(X'39')
021000940506     D F10             C                   CONST(X'3A')
021100940506     D F11             C                   CONST(X'3B')
021200940506     D F12             C                   CONST(X'3C')
021300940506     D F13             C                   CONST(X'B1')
021400940506     D F14             C                   CONST(X'B2')
021500940506     D F15             C                   CONST(X'B3')
021600940506     D F16             C                   CONST(X'B4')
021700940506     D F17             C                   CONST(X'B5')
021800940506     D F18             C                   CONST(X'B6')
021900940506     D F19             C                   CONST(X'B7')
022000940506     D F20             C                   CONST(X'B8')
022100940506     D F21             C                   CONST(X'B9')
022200940506     D F22             C                   CONST(X'BA')
022300940506     D F23             C                   CONST(X'BB')
022400940506     D F24             C                   CONST(X'BC')
022500940506     D ENTER           C                   CONST(X'F1')
022600940506     D ROLDWN          C                   CONST(X'F4')
022700940506     D ROLLUP          C                   CONST(X'F5')
022800951109     D*-------------
022900951109     D DATMIN          C                   CONST('0001-01-01')
023000951109     D DATMAX          C                   CONST('9999-12-31')
023100950524     D*-------------
023200950606     D* Variabili per gestione videate
023300950606     D $Gest           S              2A
023400950606     D $Fine           S              1A
023500950606     D $InzD1          S              1A
023600950606     D $InzS1          S              1A
023700950606     D $LastG          S              2A
023800950606     D $LastV          S              2A
023900950606     D $EndFile        S              1A
024000950606     D $Esci           S              1A
024100950606     D $RcdOK          S              1A
024200951108     D $carpag         S              1A
024300951108     D $ord            S              1S 0
024400960105     D sflpag          S              8S 0
024500950606     D*-------------
024600950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
024700950606     D S1Nrr           S                   like(C1Rcd)
024800951109     D SFLNRRSAV       S                   like(SFLNRR)
024900950606     D WSfl            S                   like(C1Nrr)
025000950606     D WMax            S                   like(C1Rcd)
025100950606     D WPag            S                   like(C1Rcd)
025200950606     D WInzS1          S                   like($InzS1)
025300970416    >D WOpr            S                   like(OprT11)
025400950606     D CurR            S              2S 0
025500950606     D CurC            S              2S 0
025600950606     D Win             S             99A
025700950606     D W1Pos           S                   like(C1Pos)
025800951109     D Wdate           S             10
025900951109    >D WDivisa         S                   like(X01Divisa)
026000970422    >D Wkcc            S                   like(kccT11)
026100970422    >D Wksc            S                   like(kscT11)
026200951109    >D SavDivisa       S                   like(X01Divisa)
026300970416    >D SavDivVis       S                   like(divvisT11)
026400980121    >D NrDecMC         S              3S 0
026500980121    >D NrDecDiv        S              3S 0
026600951110    >D WNrDec          S              3S 0
026700951110     D Wsegno          S              1A
026800951128     D WRIGsald        S              1A
026900970416    >D WImporto        S             19  3
027000980130    >D WDare           S             19  3
027100980130    >D WAvere          S             19  3
027200980130    >D WDareD          S             19  3
027300980130    >D WAvereD         S             19  3
027400970416    >D Wcausale        S                   like(causal1T11)
027500970416    >D SavCausRig      S                   like(causal1T11)
027600970422    >D SavNd002ds      S                   like(nd002ds)
027700960318    >D Winz073         S                   like(exf073)
027800960105    >D Wexf084         S                   like(exf084)
027801040212C1718D WexfT68         S                   like(INZT68)
027900980119    >D NrCicli         S              3s 0
028000980120    >D GiroDivisa      S              3s 0
028100970422     D*------------
028200970422     d* Constante ripresa saldo
028300970422     D RipresaC        C                   CONST('PROMSG    *LIBL-
028400970422     D                                          COS0261')
028500950606     D*-------------
028600950606     D*  Indici
028700950606     D X               S              3S 0
028800950606     D Y               S              3S 0
028900950606     D*-------------
029000970416     D* Controllo conti
029100970416     D ND002DS       E DS                  inz
029200970515     d* titolo stampa e/c
029300970515     D $TIT            S             27    DIM(1) CTDATA PERRCD(1)              Titolo
029400000117C1309D ZDateISO        S               D
029401051206C1886D DataIso         S               D
029402051206C1886D DataIso1        S               D
029500940127     C*----------------------------------------------------*
029600940127     C*                MAIN LINE PROGRAM
029700940127     C*----------------------------------------------------*
029800940127     C*
029900940223     C* inizializzazione variabili
030000940223     C                   EXSR      INZVAR
030100940223     C*
030200940223     C     $FINE         DOWEQ     *OFF
030300940131     C     $GEST         CASEQ     'S1'          GESS1
030400941027     C                   ENDCS
030500941027     C                   ENDDO
030600940325     C* fine programma
030700940325     C                   EXSR      ENDPGM
030800940225     C*
030900940325     C/EJECT
031000940325     C************************************************************
031100940325     C* FINE PROGRAMMA
031200940325     C************************************************************
031300940325     C     ENDPGM        BEGSR
031400951122     c* Per far riemettere la testata
031500951122     C                   MOVE      *HIVAL        $LASTV
031600940325     C*
031700940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
031800970416    >C     OPZT11        IFEQ      *BLANK
031900940325     C                   SETON                                            LR
032000940325     C* altrimenti chiudo in RT
032100940325     C                   ELSE
032200940325     C                   SETON                                            RT
032300940325     C                   ENDIF
032400940616     C                   RETURN
032500940325     C*
032600940325     C                   ENDSR
032700940224     C/EJECT
032800940201     C************************************************************
032900940201     C* RICERCHE
033000940201     C************************************************************
033100940127     C     SEARCH        BEGSR
033200940127     C*
033300940127     C* determino Riga/Colonna del cursore
033400940127     C                   MOVE      NRG           R$$
033500940127     C                   MOVE      NCL           C$$
033600940127     C                   Z-ADD     RIRI          CURR
033700940127     C                   Z-ADD     COCO          CURC
033800950704     C*
033900941118   B1C                   SELECT
034000940127     C*
034100950703    >C*--------------------------------------------------------------
034200950703    >C* inserire le routine di ricerca
034300950703    >C*--------------------------------------------------------------
034400951108    >C* FMTD1 -
034500951108    >C     1             WHENEQ    1
034600940223     C*
034700940223     C* ricerca non abilitata in questa posizione
034800941118   X1C                   OTHER
034900940929     C                   SETON                                        9899
035000940127     C*
035100941118   E1C                   ENDSL
035200950612     C*
035300950612     C* imposto pos. del cursore
035400950612     C                   Z-ADD     CURR          H1RIGA
035500950612     C                   Z-ADD     CURC          H1COLO
035600940127     C*
035700940506     C                   MOVE      *HIVAL        $LASTV
035800950612     C*
035900940127     C                   ENDSR
036000940131     C/EJECT
036100940224     C************************************************************
036200940131     C* RIEMPIMENTO OPZIONI
036300940224     C************************************************************
036400940131     C     RIEOPZ        BEGSR
036500940223     C*
036600940223     C* Input :
036700940223     C* - $TX = schiera con tutti i testi disponibili
036800940223     C* - $FL = schiere con i flag di validità associati ai testi
036900940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
037000940223     C*          opzione usato
037100940223     C* - WMXO = variabile contenente il numero massimo di testi
037200940223     C*          opzione usabili
037300940721     C* - $L01 = Lunghezza prima riga
037400940721     C* - $L02 = Lunghezza seconda riga
037500940223     C* Output :
037600940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
037700940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
037800941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
037900940223     C*
038000950522     C                   CALLB     'X04OPMSG'
038100940223     C* input
038200940223     C                   PARM                    $TX
038300940223     C                   PARM                    $FL
038400940225     C                   PARM                    WCOZ              3 0
038500940223     C                   PARM                    WMXO              3 0
038600940223     C* output
038700940223     C                   PARM                    WOPZ1            76
038800940223     C                   PARM                    WOPZ2            76
038900941214     C                   PARM                    FF23              1
039000950301     C* input
039100940721    >C                   PARM                    $L01              3 0
039200951108    >C                   PARM                    $L02              3 0
039300940223     C*
000000060224C1913C                   PARM                    $ALLOPT         960
000000060224C1913C*
039400940131     C                   ENDSR
039500940506     C/EJECT
039600940506     C************************************************************
039700940506     C* RIEMPIMENTO TASTI FUNZIONE
039800940506     C************************************************************
039900940506     C     RIEKEY        BEGSR
040000940506     C*
040100940506     C* Input :
040200940506     C* - $FX = schiera con tutti i testi disponibili
040300940506     C* - $FC = schiere con i flag di validità associati ai testi
040400940506     C* - $FR = schiere con i flag di REVERSE IMAGE
040500940506     C* - $ULK = ultimo testo caricato
040600940721     C* - $L01 = Lunghezza prima riga
040700940721     C* - $L02 = Lunghezza seconda riga
040800940506     C* Output :
040900940506     C* - $ULK
041000940609     C* - $KEY1= 1° riga di tasti funzione
041100940609     C* - $KEY2= 2° riga di tasti funzione
041200940506     C*
041300950522     C                   CALLB     'XKEYMSG'
041400940506     C* input
041500940506     C                   PARM                    $FX
041600940506     C                   PARM                    $FC
041700940506     C                   PARM                    $FR
041800940506     C                   PARM                    $ULK              3 0
041900940506     C* output
042000940506     C                   PARM                    $KEY1            79
042100940506     C                   PARM                    $KEY2            79
042200950301     C* input
042300940721    >C                   PARM                    $L01              3 0
042400940721    >C                   PARM                    $L02              3 0
042500940506     C*
000000060224C1913C                   PARM                    $ALLFUNCT       480
000000060224C1913C*
042600940506     C                   ENDSR
042700940224     C/EJECT
042800940224     C************************************************************
042900940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
043000940224     C************************************************************
043100940224     C     GESPIE        BEGSR
043200940224     C*
043300940224     C                   SELECT
043400940506     C* Lista
043500940506     C     $GEST         WHENEQ    'S1'
043600940506     C     $ULKS1        IFEQ      0
043700951110     C*
043800951127     C* totali
043900951110     C* da fare solo una volta poichè si fa quando il caricamento pagina
044000951113     C* non è presente ( se c'era il caricamento a pagina, avevo già reperit
044100951113     C* o questi dati nella routine INZVAR)
044200951110     C                   IF        $carpag = *off
044300980130     C*
044400980130     C* predispongo il piede nei campi editabili
044500980130     C* dati in m.c.
044600980202     C*
044700980202     C* reperisco decimali se divisa diversa dalla precedente letta
044800980202     C                   IF        Z1DivisaD <> SavDivisa
044900980202     C                   EVAL      SavDivisa = Z1DivisaD
045000980202     C                   EVAL      WDivisa = Z1DivisaD
045100980202     C                   EXSR      RepDivisa
045200980202     C                   EVAL      NrDecDiv = X01NrDec
045300980202     C                   ENDIF
045400010522D0935C*
045500010522D0935C                   Eval      WNrDec      = NrDecMC
045600980130     C*  Dare
045700980130     C                   Z-ADD     WDare         WImporto
045800980130     C                   EVAL      $LenFld = %Size(Z1Dare)
045900980130     C                   SUB       5             $LenFld
046000980130     C                   EXSR      Edit
046100980130     C                   Eval      Z1Dare  = $RcvrVar
046200980130     C*  Avere
046300980130     C                   Z-ADD     WAvere        WImporto
046400980130     C                   EVAL      $LenFld = %Size(Z1Avere)
046500980130     C                   EXSR      Edit
046600980130     C                   Eval      Z1Avere = $RcvrVar
046700980130     C* dati in divisa
046800980130     C                   EVAL      WNrDec = NrDecDiv
046900980130     C*  Dare
047000980130     C                   Z-ADD     WDareD        WImporto
047100980130     C                   EVAL      $LenFld = %Size(Z1DareD)
047200980130     C                   SUB       5             $LenFld
047300980130     C                   EXSR      Edit
047400980130     C                   Eval      Z1DareD  = $RcvrVar
047500980130     C*  Avere
047600980130     C                   Z-ADD     WAvereD       WImporto
047700980130     C                   EVAL      $LenFld = %Size(Z1AvereD)
047800980130     C                   EXSR      Edit
047900980130     C                   Eval      Z1AvereD = $RcvrVar
048000980130     C*
048100980121     C* calcolo segno e saldo
048200980119     C* (anche per i valori in divisa del c/valutario)
048300980121     C                   exsr      ClcSegSal
048400961128     C*
048500970422     C                   eval      z1darek = Kdare021
048600970422     C                   eval      z1averek= Kavere021
048700951110     C                   endif
048800951127     C*
048900940506     C                   EXSR      F24S1
049000940506     C                   ELSE
049100970416     C                   WRITE     CT11Z1
049200940506     C                   ENDIF
049300940506     C                   ENDSL
049400940506     C*
049500940224     C                   ENDSR
049600951110     C/EJECT
049700951110     C************************************************************
049800980121     C* Calcolo segno e saldo
049900951110     C************************************************************
050000980121     C     ClcSegSal     BEGSR
050100980119     C*
050200980130     C                   EVAL      $LenFld = %Size(Z1Saldo)
050300980130     C                   EVAL      WNrDec = NrDecMC
050400980130     C*
050500980119    1c                   SELECT
050600980130     C*
050700980130     C                   When      WDare  > WAvere
050800980130     C                   EVAL      Z1Segno = Dare021
050900980130     C     WDare         SUB       WAvere        WImporto
051000980130     C                   SUB       5             $LenFld
051100980130     C                   EXSR      Edit
051200980130     C                   EVAL      Z1Saldo = $RcvrVar
051300980130     C*
051400980130     C                   when      WDare  < WAvere
051500980130     C                   eval      Z1segno = Avere021
051600980130     C     WAvere        sub       WDare         WImporto
051700980130     C                   EXSR      Edit
051800980130     C                   EVAL      Z1Saldo = $RcvrVar
051900980130     C*
052000980130     C                   when      WDare  = Wavere
052100980130     C                   eval      Z1Segno = ' '
052200980130     c                   EVAL      WImporto = 0
052300980130     C                   SUB       5             $LenFld
052400980130     C                   EXSR      Edit
052500980130     C                   EVAL      Z1Saldo = $RcvrVar
052600980119    1C                   ENDSL
052700980130     C*
052800980130     C                   EVAL      $LenFld = %Size(Z1SaldoD)
052900980130     C                   EVAL      WNrDec = NrDecDiv
053000980119     C*
053100980119    1c                   SELECT
053200980130     C*
053300980130     C                   When      WDareD > WavereD
053400980130     C                   eval      Z1SegnoD = Dare021
053500980130     C     WDareD        sub       WAvereD       WImporto
053600980130     C                   SUB       5             $LenFld
053700980130     C                   EXSR      Edit
053800980130     C                   EVAL      Z1SaldoD = $RcvrVar
053900980130     C*
054000980130     C                   when      WDareD < WAvereD
054100980130     C     WAvereD       sub       WDareD        WImporto
054200980119     C                   eval      Z1SegnoD = Avere021
054300980130     C                   EXSR      Edit
054400980130     C                   EVAL      Z1SaldoD = $RcvrVar
054500980130     C*
054600980130     C                   when      WDareD = WAvereD
054700980119     C                   eval      Z1SegnoD = ' '
054800980130     c                   EVAL      WImporto = 0
054900980130     C                   SUB       5             $LenFld
055000980130     C                   EXSR      Edit
055100980130     C                   EVAL      Z1SaldoD = $RcvrVar
055200980119    1C                   ENDSL
055300951113     C*
055400951113     C                   ENDSR
055500951110     C*
055600940224     C/EJECT
055700940127     C************************************************************
055800940131     C* GESTIONE LISTA
055900940127     C************************************************************
056000940127     C     GESS1         BEGSR
056100940223     C* inizializzazione videata
056200940223     C     $INZS1        IFEQ      *ON
056300940127     C                   EXSR      INZS1
056400940223     C                   MOVE      *OFF          $INZS1
056500940127     C                   ENDIF
056600940223     C* emissione piede videata se proveniente da altra
056700940202     C     $LASTV        IFNE      'S1'
056800980119     C* oppure se premuto F11=Cambio moneta
056900980119     C     $LASTV        OREQ      'S1'
057000970416     C                   WRITE     CT11T1
057100940224     C                   EXSR      GESPIE
057200980119     C* salvataggio valore formato di provenienza
057300940202     C                   MOVE      $LASTV        $LASTG
057400940202     C                   MOVE      'S1'          $LASTV
057500941027     C                   ENDIF
057600960325     C* non ci sono record
057700940223     C     WMAX          IFEQ      0
057800970416     C                   WRITE     CT11D1
057900940211     C                   ENDIF
058000960325     C* ce ne sono più di 9999
058100960325     C     S1NRR         IFEQ      9999
058200960325     C                   SETOFF                                       30
058300960325     c                   clear                   z1dare
058400960325     c                   clear                   z1avere
058500960325     c                   clear                   z1saldo
058600960325     c                   clear                   z1segno
058700980119     c                   clear                   z1dareD
058800980119     c                   clear                   z1avereD
058900980119     c                   clear                   z1saldoD
059000980119     c                   clear                   z1segnoD
059100970422     C                   clear                   z1darek
059200970422     C                   clear                   z1averek
059300970416     C                   WRITE     CT11D2
059400960325     C                   ENDIF
059500951109     C                   Eval      *in09=c1mode
059600970724     C                   MOVE      '1'           WrkWdwT43         1
059700940127     C*
059800970416     C                   EXFMT     CT11C1
059900960105     C* determino Riga/Colonna del cursore
060000960105     C                   MOVE      NRG           R$$
060100960105     C                   MOVE      NCL           C$$
060200960105     C                   Z-ADD     RIRI          CURR
060300960105     C                   Z-ADD     COCO          CURC
060400940204     C     C1NRR         IFNE      0
060500940204     C                   Z-ADD     C1NRR         WSFL
060600940204     C                   ENDIF
060700951109     C*
060800951109     C     SFLNRR        IFNE      0
060900951109     C                   Z-ADD     SFLNRR        C1RCD
061000951109     C                   Z-ADD     SFLNRR        SFLNRRSAV
061100951109     C                   ELSE
061200951109     C                   Z-ADD     SFLNRRSAV     C1RCD
061300951109     C                   ENDIF
061400960104     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
061500960318     C                   MOVE      '1'           Winz073
061600960105     C                   MOVE      '1'           Wexf084
061601040212C1718C                   MOVE      '1'           WexfT68
061700940506     C*
061800940506     C* controllo abilitazione tasti funzione
061900940506     C                   EXSR      CTRKS1
062000941205     C*
062100940506     C     *IN99         IFEQ      *OFF
062200940127     C*
062300941027     C                   SELECT
062400940127     C* F3=Fine
062500940506     C     $TASTO        WHENEQ    F03
062600940309     C                   EXSR      F03S1
062700951120     C* F8=Selezione seguente
062800951120     C     $TASTO        WHENEQ    F08
062900951120     C                   EXSR      F08S1
063000951108     C* F11=Cambio moneta
063100951108     C     $TASTO        WHENEQ    F11
063200951108     C                   EXSR      F11S1
063300940131     C* F12=Ritorno
063400940506     C     $TASTO        WHENEQ    F12
063500940309     C                   EXSR      F12S1
063600951108     C* F15=Interrogazioni
063700951108     C     $TASTO        WHENEQ    F15
063800951108     C                   EXSR      F15S1
063900970515     C* F20=Stampa e/c
064000970515     C     $TASTO        WHENEQ    F20
064100970515     C                   EXSR      F20S1
064200940224     C* F23=Altre opzioni
064300940506     C     $TASTO        WHENEQ    F23
064400940309     C                   EXSR      F23S1
064500940224     C* F24=Altri tasti
064600940506     C     $TASTO        WHENEQ    F24
064700940309     C                   EXSR      F24S1
064800951108     C* ROLL-UP (se gestito a pagina)
064900940506     C     $TASTO        WHENEQ    ROLLUP
065000951108     C     $CARPAG       ANDEQ     *ON
065100940127     C                   EXSR      ROLS1
065200940127     C*
065300941027     C                   OTHER
065400940127     C* CONTROLLO DATI
065500940131     C                   EXSR      CTRC1
065600940201     C     *IN99         IFEQ      *OFF
065700940131     C                   EXSR      CTRS1
065800941027     C                   ENDIF
065900941205     C*
066000941027     C                   ENDSL
066100940127     C*
066200940506     C                   ENDIF
066300941205     C*
066400940127     C                   ENDSR
066500940224     C/EJECT
066600940127     C************************************************************
066700940131     C* INIZIALIZZAZIONE LISTA
066800940127     C************************************************************
066900940127     C     INZS1         BEGSR
067000940302     C* pulizia SFL
067100940128     C                   SETOFF                                         3031
067200970416     C                   WRITE     CT11C1
067300940128     C                   SETON                                          31
067400940127     C*
067500940128     C* riempimento opzioni
067600940223     C                   MOVEA     $TX1          $TX
067700940223     C                   MOVEA     $FL1          $FL
067800940309     C                   Z-ADD     0             WCOZ
067900940225     C*  equivale al num.di elementi della schiera $TX1
068000940509     C                   Z-ADD     $DIMOP        WMXO
068100941220    >C                   Z-ADD     76            $L01
068200941220    >C                   Z-ADD     76            $L02
068300940128     C                   EXSR      RIEOPZ
068400941214     C                   Z-ADD     23            X
068500951109     C                   MOVE      FF23          $FC1(X)
068600940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
068700951108     C                   MOVEL     WOPZ2         C1OPZ2
068800940225     C                   Z-ADD     WCOZ          C1COZ
068900951108     C* imposta control
069000970416     C                   EVAL      C1KCC = kccT11
069100970416     C                   EVAL      C1KsC = kscT11
069200970416     C                   EVAL      C1Descr = DESCR002
069300970423     C                   EVAL      C1Banca = BancaT11
069400970423     C                   EVAL      C1Linea = LineaT11
069500970422     C                   CALLb     'XDT4000'
069600970422     C                   PARM      DtChiuT11     XDTAMG
069700970422     C     C1DtChiu      PARM                    XDTGMA
069800970422     C                   PARM      2             XDTSTA
069900940128     C*
070000951108     C* CARICAMENTO SFL A PAGINE o intero
070100940128     C*
070200940201     C                   Z-ADD     0             S1NRR
070300940128     C                   Z-ADD     0             WMAX
070400970910     C                   CLEAR                   WrigSald
070500940224     C*
070600940224     C* Posizionamento su file pilota
070700940302     C                   CLEAR                   C1POS
070800941214     C                   EXSR      SETANA
070900940302     C* ripristino i valori del posizionamento
071000940302     C                   MOVE      W1POS         C1POS
071100940131     C*
071200940201     C* Lettura file pilota
071300941214     C                   EXSR      REDANA
071400940131     C*
071500951108     C* Carico 1a pagina o tutto
071600940127     C                   EXSR      ROLS1
071700940201     C                   Z-ADD     SFLPAG        WPAG
071800940127     C*
071900940127     C                   ENDSR
072000970416     C/EJECT
072100970416     C*----------------------------------------------------------
072200970416     C* Chiamata NDMVC002
072300970416     C*----------------------------------------------------------
072400970416     C     CallMvc002    BEGSR
072500970416     C*
072600970416     C* Controllo conto
072700970416     C                   Clear                   ND002DS
072800970416     C                   EVAL      LenOut = %Size(ND002DS)
072900970416     C                   eval      Data = Datmin
073000970416     C                   CallB     'NDMVC002'
073100970416     C                   PARM                    xscsoc
073200970422     C                   PARM                    Wkcc
073300970422     C                   PARM                    Wksc
073400970416     C                   PARM                    Data
073500970416     C                   PARM      *OFF          GesMsg
073600970416     C                   PARM      *OFF          Esito
073700970416     C                   PARM      'ND002DS'     StrutturaO
073800970416     C                   PARM                    ND002DS
073900970416     C                   PARM                    LenOut
074000970416     C*
074100970416     C                   ENDSR
074200940127     C/EJECT
074300940127     C************************************************************
074400940131     C* CARICAMENTO PAGINA LISTA
074500940127     C************************************************************
074600940127     C     ROLS1         BEGSR
074700940127     C*
074800940128     C                   SETOFF                                       32
074900940223     C                   Z-ADD     0             Y
075000940127     C                   Z-ADD     WMAX          S1NRR
075100951128     C*
075200951128     C* se richiesto ripresa saldo scrivo la prima riga col saldo
075300970416     C                   if        ripsaldT11 = *on and Wrigsald <> *on
075400980121     C                   EXSR      RieRipSald
075500951128     C                   eval      wrigsald = *on
075600951128     C                   endif
075700940127     C*
075800940127     C* Leggo dal file anagrafico per caricare la lista
075900940127     C*
076000950606     C     $EndFile      DOWEQ     *OFF
076100940201     C     Y             ANDLT     WPAG
076200940127     C*
076300940131     C                   EXSR      RIES1
076400940127     C*
076500960325     C*  se s1nrr = 9999, che è il massimo comsentito, mi fermo e do
076600960325     c* messaggio di errore
076700960325     c                   if        s1nrr < 9999
076800960325     c*
076900940127     C                   ADD       1             S1NRR
077000940127     C                   ADD       1             Y
077100940127     C*
077200970416     C                   WRITE     CT11S1
077300940131     C*
077400941214     C                   EXSR      REDANA
077500960325     c*
077600960325     C                   else
077700960325     c                   eval      $endfile = *on
077800960325     c                   endif
077900940128     C*
078000941027     C                   ENDDO
078100940127     C*
078200940223     C                   Z-ADD     S1NRR         WMAX                 30
078300940127     C*
078400940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
078500940127     C*
078600940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
078700940127     C                   MVR                     RESTO             3 0
078800940131     C     PAGINE        MULT      SFLPAG        C1RCD
078900941027     C     RESTO         IFGT      0
079000940127     C                   ADD       1             C1RCD
079100941027     C                   ELSE
079200940407     C                   SUB       SFLPAG        C1RCD
079300940407     C                   ADD       1             C1RCD
079400941027     C                   ENDIF
079500940128     C*
079600940127     C                   ENDSR
079700951128     C/EJECT
079800951128     C************************************************************
079900951128     C* riempie riga saldo
080000951128     C************************************************************
080100980121     C     RieRipSald    BEGSR
080200951128     C*
080300980121     C* IMPOSTAZIONI FISSE SIA PER LA VISUALIZZAZIONE DEI DATI IN M.C. CHE
080400980121     C* PER QUELLA DEI DATI IN DIVISA
080500980121     C*
080600970416     C                   CLEAR                   CT11S1
080700951128     C                   MOVEL     *ZERO         desaggh
080800951128     C                   MOVEL     *on           in41h
080900980121     C                   EVAL      h1Divisa   = DivBRAT11
081000980121     C                   EVAL      h1Monconto = xscdiv
081100980121     C*
081200980121     C* reperisco decimali se divisa diversa dalla precedente letta
081300980121     C                   IF        H1Divisa <> SavDivisa
081400980121     C                   EVAL      SavDivisa = H1Divisa
081500980121     C                   EVAL      WDivisa = H1Divisa
081600980121     C                   EXSR      RepDivisa
081700980121     C                   EVAL      NrDecDiv = X01NrDec
081800980121     C                   ENDIF
081900951128     C*
082000980121     C* CICLO 2 VOLTE PER IMPOSTARE SUL SFL IL SALDO SIA IN M.C. CHE IN
082100980121     C* DIVISA (se il rapporto bancario non è un c/valutario, il 2o giro
082200980121     C*  serve solo per allineare il dato in modo da essere leggibile
082300980121     C*  assieme a divise diverse)
082400980121     C*
082500980121    1C                   DO        2             x
082600980121     C*
082700980121     C* stabilisco la lunghezza massima del campo alfanumerico che può
082800980121     C* contenere l'importo editato (i campi di riga 1 e riga 2 devono
082900980121     C*  essere delle stesse dimensioni)
083000980121     C                   Eval      $LenFld = %Size(S1Saldo1)
083100980121     C*
083200980121     C* recupero i dati dei saldi dalla schiera di appoggio
083300980121     C                   MOVEL     AppT21DS(x)   NDCT21Ds
083400980121     C*
083500980121     C* modalità data valuta
083600980121     C                   if        ModalitT11 = '0'
083700980121     C                   select
083800980121     C                   when      SlInDVaT21 > SlInAVaT21
083900970417     C                   EVAL      wimporto =  SlInDVaT21 - SlInAVaT21
084000980121     C                   EVAL      WSegno = Dare021
084100980121     C                   when      SlInDVaT21 < SlInAVat21
084200970417     C                   EVAL      wimporto =  SlInAVaT21 - SlInDVaT21
084300980121     C                   EVAL      WSegno = Avere021
084400980121     C                   when      SlInDVaT21 = SlInAVat21
084500980121     C                   eval      Wimporto = 0
084600980121     C                   EVAL      WSegno = ' '
084700980121     C                   endsl
084800980121     C*
084900980121     C* modalità data operazione
085000980121     C                   ELSE
085100980121     C                   select
085200980121     C                   when      SlInDOpT21 > SlInAOpt21
085300980121     C                   EVAL      wimporto =  SlInDOpT21 - SlInAOpT21
085400980121     C                   EVAL      WSegno = Dare021
085500980121     C                   when      SlInDOpT21 < SlInAOpt21
085600980121     C                   EVAL      wimporto =  SlInAOpT21 - SlInDOpT21
085700980121     C                   EVAL      WSegno = Avere021
085800980121     C                   when      SlInDOpT21 = SlInAOpt21
085900980121     C                   eval      Wimporto = 0
086000980121     C                   EVAL      WSegno = ' '
086100980121     C                   endsl
086200980121     C                   endif
086300980121     C*
086400980121     C* se primo giro
086500980121     C                   IF        x = 1
086600980121     C* valorizzo il nr.decimali con quelli della m.c.
086700980121     C                   Z-ADD     NrDecMC       WNrDec
086800980121     C* se secondo giro
086900980121     C                   ELSE
087000980121     C* con quelli della divisa di rapporto
087100980121     C                   Z-ADD     NrDecDiv      WNrDec
087200980121     C                   ENDIF
087300980121     C*
087400980121     C* se il segno è DARE
087500980121     C                   if        WSegno = Dare021
087600980121     C* allineo l'importo a sinistra
087700951128     C                   SUB       5             $lenfld
087800951128     C                   Endif
087900980121     C*
088000980121     C* se secondo giro
088100980121     C                   IF        x = 2
088200980121     C* siccome bisogna allinearsi anche agli importi delle righe di BMV,
088300980121     C* che possono avere divise diverse e quindi decimali diversi, bisogna
088400980121     C* riproporzionare il campo di arrivo
088500951128     C                   Select
088600951128     C                   When      WNrDec = 2
088700980121     C                   Eval      $LenFld = $LenFld - 1
088800951128     C                   When      WNrDec = 1
088900980121     C                   Eval      $LenFld = $LenFld - 2
089000951128     C                   When      WNrDec = 0
089100980121     C                   Eval      $LenFld = $LenFld - 4
089200980121     C                   Endsl
089300980121     C                   ENDIF
089400980121     C*
089500980121     C* editazione importo
089600951128     C                   EXSR      Edit
089700980121     C*
089800980121     C* se primo giro
089900980121     C                   IF        x = 1
090000980121     C* salvo i dati della m.c.
090100980121     C                   EVAL      h1segnom = WSegno
090200980121     C                   Eval      h1saldom = $RcvrVar
090300980121     C* se secondo giro
090400980121     C                   ELSE
090500980121     C* salvo i dati della divisa
090600980121     C                   EVAL      h1segnod = WSegno
090700980121     C                   Eval      h1saldod = $RcvrVar
090800980121     C                   ENDIF
090900980121     C*
091000980121     C                   ENDDO
091100980121     C*
091200980121     C* il segno è sempre lo stesso
091300980121     C                   EVAL      S1Segno1 = H1SegnoM
091400980121     C                   EVAL      S1Segno2 = H1SegnoM
091500951128     C                   select
091600980121     C* se devo visualizzare in prima riga la m.c.
091700980121     C                   when      SavDivVis = '0'
091800980121     C                   eval      S1Saldo1 = H1SaldoM
091900980121     C                   EVAL      S1Divisa1  = H1MonConto
092000980121     C                   eval      S1Saldo2 = H1saldoD
092100980121     C                   EVAL      S1Divisa2  = H1Divisa
092200980121     C* se devo visualizzare in prima riga la divisa
092300951128     C                   when      Savdivvis = '1'
092400951128     C                   eval      s1saldo1 = h1saldod
092500951128     C                   EVAL      S1DiVisa1  = h1divisa
092600951128     C                   eval      s1saldo2 = h1saldom
092700951128     C                   EVAL      S1DiVisa2  = h1monconto
092800951128     C                   endsl
092900951128     C*
093000951128     C                   eval      s1causrig = 'SALD'
093100970422     C                   eval      s1causrigd = Ripresa
093200951128     C*
093300951128     C                   exsr      atrs1
093400951128     C                   ADD       1             S1NRR
093500951128     C                   ADD       1             Y
093600970416     C                   WRITE     CT11S1
093700951128     C*
093800951128     C                   ENDSR
093900940224     C/EJECT
094000940224     C************************************************************
094100940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
094200940224     C************************************************************
094300941214     C     SETANA        BEGSR
094400940224     C*
094500970416     C                   MOVE      XSCSOC        BMVSOCIETA
094600970421     C                   MOVE      CTBT11        BMVCTB
094700970416     C                   MOVE      BancaT11      BmvBanca
094800970416     C                   MOVE      LineaT11      BmvLinea
094900950704     C*
095000940224     C                   SELECT
095100941214     C*
095200941214     C* Gestione posizionamento
095300950301     C* Se il campo W1POS è > *blank imposto il posizionamento con
095400950301     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
095500941214     C*
095600970416    >C* Modalità 0 =  data valuta
095700970416     C     ModalitT11    WHENEQ    '0'
095800950705     C*
095900950705   B1C     W1POS         IFNE      0
096000951109     C                   CALLb     'XDT4000'
096100951109     C                   PARM                    XDTAMG           10
096200951109     C                   PARM      w1pos         XDTGMA            6 0
096300951109     C                   PARM      4             XDTSTA            1 0
096400970416    >C                   MOVE      xdtamg        BmvDtVal
096500940302     C                   ELSE
096600970417     c* discendente
096700970417     c                   if        $ord = 1
096800970416   B2C     DtperaT11     IFEQ      datmin
096900970416    >C                   MOVE      DATMAX        BmvDtVal
097000951114     C                   ELSE
097100970416    >C                   MOVE      DTPERaT11     BmvDtVal
097200950705   E2C                   ENDIF
097300970417     c                   else
097400970417     c* Ascendente
097500970417   B2C     DtperdaT11    IFEQ      datmin
097600970417    >C                   MOVE      DATMAX        BmvDtVal
097700970417     C                   ELSE
097800970417    >C                   MOVE      DTPERdaT11    BmvDtVal
097900970417   E2C                   ENDIF
098000951114   E2C                   ENDIF
098100970417   E2C                   ENDIF
098200951127     C*
098300970416     c* Discendente
098400970416     C                   if        $ord = 1
098500970421    >C     K05Bmv04      SETGT     NDBmv04
098600970416     c                   else
098700970416     c* Ascendente
098800970421    >C     K05Bmv04      SETll     NDBmv04
098900970416     c                   endif
099000970416     c*
099100970416    >C* Modalità 1 =  data operazione
099200970416     C     ModalitT11    WHENEQ    '1'
099300951108     c*
099400950705   B1C     W1POS         IFNE      0
099500951109     C                   CALLb     'XDT4000'
099600951109     C                   PARM                    XDTAMG
099700951109     C                   PARM      w1pos         XDTGMA
099800951109     C                   PARM      4             XDTSTA
099900970416    >C                   MOVE      xdtamg        BmvDtOpe
100000940302     C                   ELSE
100100970417     c* discendente
100200970417     c                   if        $ord = 1
100300970417   B2C     DtperaT11     IFEQ      datmin
100400970416    >C                   MOVE      DATMIN        BmvDtOpe
100500951114     C                   ELSE
100600970417    >C                   MOVE      DTPERAT11     BmvDtOpe
100700951114   E2C                   ENDIF
100800970417     c                   else
100900970417     c* Ascendente
101000970417   B2C     DtperdaT11    IFEQ      datmin
101100970417    >C                   MOVE      DATMAX        BmvDtOpe
101200970417     C                   ELSE
101300970417    >C                   MOVE      DTPERdaT11    BmvDtOpe
101400970417   E2C                   ENDIF
101500970417   E2C                   ENDIF
101600970417   E2C                   ENDIF
101700970416     C*
101800970416     c* Discendente
101900970416     C                   if        $ord = 1
102000970421    >C     K05Bmv03      SETGT     NDBmv03
102100970416     c                   else
102200970416     c* Ascendente
102300970421    >C     K05Bmv03      SETll     NDBmv03
102400970416     c                   endif
102500951108     c*
102600940224     C                   ENDSL
102700940224     C*
102800940224     C                   ENDSR
102900940128     C/EJECT
103000940128     C************************************************************
103100940131     C* LETTURA RCD ARCHIVIO PILOTA
103200940128     C************************************************************
103300941214     C     REDANA        BEGSR
103400940128     C*
103500950606     C                   MOVEL     *OFF          $EndFile
103600940131     C                   MOVEL     *OFF          $RCDOK
103700940131     C*
103800950606     C     $EndFile      DOUEQ     *ON
103900940131     C     $RCDOK        OREQ      *ON
104000940131     C*
104100941027     C                   SELECT
104200951113     C* Discendente
104300940131     C     $ORD          WHENEQ    1
104400970416     c                   if        ModalitT11= '0'
104500970421    >C     K04bmv04      READPE    NDBmv04                                21
104600970416     c                   else
104700970421    >C     K04bmv03      READPE    NDBmv03                                21
104800970416     c                   endif
104900951127     C*
105000951113     C* Ascendente
105100940131     C     $ORD          WHENEQ    2
105200951127     C*
105300970416     c                   if        ModalitT11= '0'
105400970421    >C     K04bmv04      READE     NDBmv04                                21
105500970416     c                   else
105600970421    >C     K04bmv03      READE     NDBmv03                                21
105700970416     c                   endif
105800970416     c*
105900941027     C                   ENDSL
106000940131     C*
106100950606     C                   MOVE      *IN21         $EndFile
106200940131     C*
106300950606     C     $EndFile      IFEQ      *OFF
106400941214     C                   EXSR      SELANA
106500941027     C                   ENDIF
106600940131     C*
106700941027     C                   ENDDO
106800940131     C*
106900940131     C                   MOVE      *IN21         *IN33
107000940131     C*
107100940131     C                   ENDSR
107200940224     C/EJECT
107300940224     C************************************************************
107400940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
107500940224     C************************************************************
107600940224     C     CLCPAG        BEGSR
107700940224     C* Input :
107800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
107900940224     C*          cursore
108000940224     C* - SFLPAG = numero rcd per pagina sfl
108100940224     C* Output :
108200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
108300940224     C*
108400940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
108500940224     C                   MVR                     RESTO             3 0
108600940224     C     RESTO         IFGT      0
108700940224     C                   ADD       1             PAGINE
108800940224     C                   ENDIF
108900940224     C     PAGINE        MULT      SFLPAG        WPAG
109000940224     C*
109100940224     C                   ENDSR
109200940131     C/EJECT
109300940131     C************************************************************
109400940131     C* SELEZIONI SULLA RIGA LETTA
109500940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
109600940314     C* non rientra nei parametri di parzializzazione
109700940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
109800940314     C* superiore stabilito nei parametri
109900940314     C*
110000940131     C************************************************************
110100941214     C     SELANA        BEGSR
110200940131     C*
110300940202     C                   MOVEL     *ON           $RCDOK
110400940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
110400940131A1211C                   MOVEL     *hival        ALL9             15
110500970416     C*
110600970416     C* Aggancio NDMOV che serve sempre per selezioni e caricamento sfl
110700970416     c                   eval      MovSys = BmvSys
110800970416     c                   eval      MovNrasReg= BmvNrasReg
110900970416     c                   eval      MovNrrigaM = BmvNRrigaM
111000970416     c     k03Mov01      chain     NdMov01l                           21
111100970724     C*
111200970724     C* aggancio NDREG che serve per la contropartita
111300970724     c                   clear                   wrkkcccpu
111400970724     c                   clear                   wrkksccpu
111500970724     C                   move      Bmvsys        regsys
111600970724     C                   move      Bmvnrasreg    regnrasreg
111700030617D1377X***  K02reg01      chain     ndreg01l                           21
111800030617  "  X***                move      *in21         FlgReg            1
111900030617  "  C     K02reg01      chain     ndreg01l                           22
112000030617D1377C                   move      *in22         FlgReg            1
112100970724   1 C                   if        movkcccpu<>*blank
112200970724     c* imposto contropartita nei campi WRk
112300970724     C                   eval      Wrkkcccpu = movkcccpu
112400970724     C                   eval      wrkksccpu = movksccpu
112500970724     C                   else
112600970724   2 C                   if        FlgReg=*off
112700970724   3 C                   if        movdare=1
112800970724     C                   move      regkcccpa     wrkkcccpu         6
112900970724     C                   move      regksccpa     wrkksccpu         8
113000970724     C                   else
113100970724   4 C                   if        movavere=1
113200970724     C                   move      regkcccpd     wrkkcccpu
113300970724     C                   move      regksccpd     wrkksccpu
113400970724   4eC                   endif
113500970724   3eC                   endif
113600970724   2eC                   endif
113700970724   1eC                   endif
113800970416     c*
113900951110     C                   SELECT
114000940314     C*
114100940314     C* SELEZIONI RISPETTO AI VALORI RCD
114200940314     C*
114300950314     C* Scartare i record che non rispondono alle selezioni effettuate
114400950301     C* esempio : esclusione degli annullati.
114500950906     c*
114600950906     C* Unità diversa
114700951110     C
114800970416     C                   When      UnitaT11 <> *blanks and
114900970416     C                             BmvUnita <> UnitaT11
115000950906     C                   move      *off          $RCDok
115100951109     c*
115200951109     C* Periodo
115300970416     C                   When      (BmvDtOpe <  DtperDaT11 or
115400970416     C                             BmvDtOpe >  DtperAT11) and
115500970416     c                             ModalitT11 = '1'
115600951109     C                   move      *off          $RCDok
115700970416     c*
115800970416     C                   When      (BmvDtVal <  DtperDaT11 or
115900970416     C                             BmvDtVal >  DtperAT11) and
116000970416     c                             ModalitT11 = '0'
116100970416     C                   move      *off          $RCDok
116200970417     c*
116300970417     C* Data di registrazione
116400970417     C                   When      BmvDtReg <  DtRegDaT11 or
116500970417     C                             BmvDtReg >  DtRegAT11
116600970417     C                   move      *off          $RCDok
116700970417     c*
116800970417     C* Data operazione/valuta
116900970417     C                   When      (BmvDtOpe <  DtDatDaT11 or
117000970417     C                             BmvDtOpe >  DtDatAT11) and
117100030417D1329x***                          modalitt11 = '1' or
117200030417D1329c                             modalitt11 = '0' or
117300970417     C                             (BmvDtVal <  DtDatDaT11 or
117400970417     C                             BmvDtVal >  DtDatAT11) and
117500030417D1329x***                          modalitt11 = '0'
117600030417D1329c                             modalitt11 = '1'
117700970417     C                   move      *off          $RCDok
117800970417     c*
117900970417     C* Importo
118000970417     C                   When      BmvImporto < ImportDT11 or
118100970417     C                             BmvImporto > ImportAT11
118200970417     C                   move      *off          $RCDok
118300951127     C*
118400951127     C* Provvis, definit, gestionali
118500970416     C                   WHEN      (MovdefT11 = *off   and
118600970416     C                             BmvTpRegz = 'D') or
118700970416     C                             (movprovT11 = *off   and
118800970416     C                             Bmvtpregz = 'P') or
118900970416     C                             (movgesT11  = *off   and
119000970416     C                             Bmvtpregz = 'G')
119100951127     C                   eval      $rcdok = *off
119200951127     c*
119300951109     c*
119400951109     C* Divisa selezionata (includi)
119500970416     C                   When      DivSelT11 = 'I' and divisaT11 <> *blanks
119600970416     C                             and BmvDivisa <>  divisaT11
119700951109     C                   move      *off          $RCDok
119800951109     c*
119900951109     C* Divisa selezionata (ometti)
120000970416     C                   When      DivSelT11 = 'O' and divisaT11 <> *blanks
120100970416     C                             and BmvDivisa =  divisaT11
120200951109     C                   move      *off          $RCDok
120300970605     c*
120400970605     C* Riconciliato
120500970911     C                   When      RiconciT11='1' and BmvStatus =  ' '  or
120600970911     C                             RiconciT11='2' and BmvStatus <> ' '
120700970605     C                   move      *off          $RCDok
120800951109     c*
120900951109     C                   ENDSL
121000970730     c*
121100970730     c* Contropartita
121200970730     c                   if        $rcdok = *on
121300970730     C                   if        KcccpT11 <> *blanks
121400970730     c* se diverso capoconto o sottoconto se richiesto
121500970730     c                   if        WrkKcccpu <> kccCpT11  or
121600970730     c                             kscCpT11  <> *blanks and
121700970730     c                             kscCpT11  <> wrkksccpu
121800970730     C                   move      *off          $RCDok
121900970730     C                   endif
122000970730     C                   endif
122100970730     C                   endif
122200970730     c*
122300970730     C* Classe
122400970730     c                   if        $rcdok = *on
122500971030     C* con queste IF avrebbe sempre scartato tutti i movimenti che avevano
122600971030     C* una classe <> dalla prima indicata ma magari uguale a una delle
122700971030     C* successive
122800971030     C*                  if        Classe1T11 <> *blanks and
122900971030     C*                            movclasse  <> Classe1T11
123000971030     C*                  move      *off          $RCDok
123100971030     C*                  endif
123200971030     C*                  if        Classe2T11 <> *blanks and
123300971030     C*                            movclasse  <> Classe2T11
123400971030     C*                  move      *off          $RCDok
123500971030     C*                  endif
123600971030     C*                  if        Classe3T11 <> *blanks and
123700971030     C*                            movclasse  <> Classe3T11
123800971030     C*                  move      *off          $RCDok
123900971030     C*                  endif
124000971030     C*                  if        Classe4T11 <> *blanks and
124100971030     C*                            movclasse  <> Classe4T11
124200971030     C*                  move      *off          $RCDok
124300971030     C*                  endif
124400971030     C*                  if        Classe5T11 <> *blanks and
124500971030     C*                            movclasse  <> Classe5T11
124600971030     C*                  move      *off          $RCDok
124700971030     C*                  endif
124800971030     C*                  if        Classe6T11 <> *blanks and
124900971030     C*                            movclasse  <> Classe6T11
125000971030     C*                  move      *off          $RCDok
125100971030     C*                  endif
125200971030     C*                  if        Classe7T11 <> *blanks and
125300971030     C*                            movclasse  <> Classe7T11
125400971030     C*                  move      *off          $RCDok
125500971030     C*                  endif
125600971030     C*                  if        Classe8T11 <> *blanks and
125700971030     C*                            movclasse  <> Classe8T11
125800971030     C*                  move      *off          $RCDok
125900971030     C*                  endif
126000971030     C*                  if        Classe9T11 <> *blanks and
126100971030     C*                            movclasse  <> Classe9T11
126200971030     C*                  move      *off          $RCDok
126300971030     C*                  endif
126400971030     C*                  if        Classe0T11 <> *blanks and
126500971030     C*                            movclasse  <> Classe0T11
126600971030     C*                  move      *off          $RCDok
126700971030     C*                  endif
126800971031     C* se c'è almeno una selezione per classe
126900971031     C                   IF        Classe1T11 <> *blank or
127000971031     C                             Classe2T11 <> *blank or
127100971031     C                             Classe3T11 <> *blank or
127200971031     C                             Classe4T11 <> *blank or
127300971031     C                             Classe5T11 <> *blank or
127400971031     C                             Classe6T11 <> *blank or
127500971031     C                             Classe7T11 <> *blank or
127600971031     C                             Classe8T11 <> *blank or
127700971031     C                             Classe9T11 <> *blank or
127800971031     C                             Classe0T11 <> *blank
127900971031     C* e la classe letta è diversa da ognuna
128000971031     C                   IF        MOVClasse <> Classe1T11 and
128100971031     C                             MOVClasse <> Classe2T11 and
128200971031     C                             MOVClasse <> Classe3T11 and
128300971031     C                             MOVClasse <> Classe4T11 and
128400971031     C                             MOVClasse <> Classe5T11 and
128500971031     C                             MOVClasse <> Classe6T11 and
128600971031     C                             MOVClasse <> Classe7T11 and
128700971031     C                             MOVClasse <> Classe8T11 and
128800971031     C                             MOVClasse <> Classe9T11 and
128900971031     C                             MOVClasse <> Classe0T11
129000971031     C* scarto il rcd
129100971031     C                   MOVE      *off          $RcdOk
129200971031     C                   ENDIF
129300971031     C                   ENDIF
129400970730     C                   endif
129500970730     c*
129600970730     C* Serie
129700970730     c                   if        $rcdok = *on
129800971030     C* con queste IF avrebbe sempre scartato tutti i movimenti che avevano
129900971030     C* una serie <> dalla prima indicata ma magari uguale a una delle
130000971030     C* successive
130100971030     C*                  if        Serie1T11 <> *blanks and
130200971030     C*                            movserreg <> Serie1T11
130300971030     C*                  move      *off          $RCDok
130400971030     C*                  endif
130500971030     C*                  if        Serie2T11 <> *blanks and
130600971030     C*                            movserreg <> Serie2T11
130700971030     C*                  move      *off          $RCDok
130800971030     C*                  endif
130900971030     C*                  if        Serie3T11 <> *blanks and
131000971030     C*                            movserreg <> Serie3T11
131100971030     C*                  move      *off          $RCDok
131200971030     C*                  endif
131300971030     C*                  if        Serie4T11 <> *blanks and
131400971030     C*                            movserreg <> Serie4T11
131500971030     C*                  move      *off          $RCDok
131600971030     C*                  endif
131700971030     C*                  if        Serie5T11 <> *blanks and
131800971030     C*                            movserreg <> Serie5T11
131900971030     C*                  move      *off          $RCDok
132000971030     C*                  endif
132100971031     C* se c'è almeno una selezione per serie
132200971031     C                   IF        Serie1T11  <> *blank or
132300971031     C                             Serie2T11  <> *blank or
132400971031     C                             Serie3T11  <> *blank or
132500971031     C                             Serie4T11  <> *blank or
132600971031     C                             Serie5T11  <> *blank
132700971031     C* e la serie letta è diversa da ognuna
132800971031     C                   IF        MOVSerReg <> Serie1T11  and
132900971031     C                             MOVSerReg <> Serie2T11  and
133000971031     C                             MOVSerReg <> Serie3T11  and
133100971031     C                             MOVSerReg <> Serie4T11  and
133200971031     C                             MOVSerReg <> Serie5T11
133300971031     C* scarto il rcd
133400971031     C                   MOVE      *off          $RcdOk
133500971031     C                   ENDIF
133600971031     C                   ENDIF
133700970730     C                   endif
133800970730     c*
133900970730     C* Causali
134000970730     c                   if        $rcdok = *on
134100971030     C* con queste IF avrebbe sempre scartato tutti i movimenti che avevano
134200971030     C* una causale <> dalla prima indicata ma magari uguale a una delle
134300971030     C* successive
134400971030     C*                  if        Causal1T11 <> *blanks and
134500971030     C*                            bmvcauban  <> Causal1T11
134600971030     C*                  move      *off          $RCDok
134700971030     C*                  endif
134800971030     C*                  if        Causal2T11 <> *blanks and
134900971030     C*                            bmvcauban  <> Causal2T11
135000971030     C*                  move      *off          $RCDok
135100971030     C*                  endif
135200971030     C*                  if        Causal3T11 <> *blanks and
135300971030     C*                            Bmvcauban  <> Causal3T11
135400971030     C*                  move      *off          $RCDok
135500971030     C*                  endif
135600971030     C*                  if        Causal4T11 <> *blanks and
135700971030     C*                            Bmvcauban  <> Causal4T11
135800971030     C*                  move      *off          $RCDok
135900971030     C*                  endif
136000971030     C*                  if        Causal5T11 <> *blanks and
136100971030     C*                            Bmvcauban  <> Causal5T11
136200971030     C*                  move      *off          $RCDok
136300971030     C*                  endif
136400971031     C* se c'è almeno una selezione per causale
136500971031     C                   IF        Causal1T11 <> *blank or
136600971031     C                             Causal2T11 <> *blank or
136700971031     C                             Causal3T11 <> *blank or
136800971031     C                             Causal4T11 <> *blank or
136900971031     C                             Causal5T11 <> *blank
137000971031     C* e la causale letta è diversa da ognuna
137100971031     C                   IF        BMVCauBan <> Causal1T11 and
137200971031     C                             BMVCauBan <> Causal2T11 and
137300971031     C                             BMVCauBan <> Causal3T11 and
137400971031     C                             BMVCauBan <> Causal4T11 and
137500971031     C                             BMVCauBan <> Causal5T11
137600971031     C* scarto il rcd
137700971031     C                   MOVE      *off          $RcdOk
137800971031     C                   ENDIF
137900971031     C                   ENDIF
138000971030     C                   endif
138100951109     C*
138200940202     C                   ENDSR
138300940131     C/EJECT
138400940131     C************************************************************
138500940131     C* RIEMPIMENTO RIGA LISTA
138600940131     C************************************************************
138700951127     C     RIES1         BEGSR
138800940131     C*
138900970416     C                   CLEAR                   CT11S1
139000950704     C* azzero le memorizzazioni degli attributi non bottoni
139100951127     C                   MOVEL     *ZERO         desaggh
139200951128     C                   MOVEL     *ZERO         in41h
139300951128     C*
139400970724     C* dati da ndreg
139500970724     C                   if        flgReg   = *off
139600951127     C                   eval      s1nrreg  = regnrreg
139700951127     C                   eval      s1utente = reguteulmo
139800970129     c                   else
139900970129     c* proteggo opzione
140000970129     C                   MOVEL     *on           in41h
140100951127     C                   ENDIF
140200951109     C*
140300970416     C                   move      bmvsys        h1sys
140400970416     C                   move      bmvnrasreg    h1nrasreg
140500970416     C                   move      bmvnrrigam    h1nrrigam
140600970416     c* data valuta
140700970416     C                   CALLb     'XDT4000'
140800970416     C                   PARM      Bmvdtval      XDTAMG
140900970417     C     S1DTvaluta    PARM                    XDTGMA
141000970416     C                   PARM      2             XDTSTA
141100970416     c* data operazione
141200970416     C                   CALLb     'XDT4000'
141300970416     C                   PARM      Bmvdtope      XDTAMG
141400970416     C     S1DTope       PARM                    XDTGMA
141500970416     C                   PARM      2             XDTSTA
141600951127     c* registrazione
141700951109     C                   CALLb     'XDT4000'
141800970416     C                   PARM      Bmvdtreg      XDTAMG
141900951127     C     S1DTreg       PARM                    XDTGMA
142000951109     C                   PARM      2             XDTSTA
142100951127     C                   MOVE      movserreg     S1SERreg
142200951127     c*Segnalo solo le registrazioni non definitive, per queste lascio blank
142300970416     C                   IF        Bmvtpregz <> 'D'
142400970416     C                   eval      s1tpregz  = bmvtpregz
142500951127     C                   endif
142600970724     c*Per quelle definitive e riconciliate, emetto una barra
142700970724     C                   IF        Bmvtpregz =  'D' and
142800970911     C                             (Bmvstatus =  '1'  or
142900970911     C                              Bmvstatus =  '7'     )
143000970724     C                   eval      s1tpregz  = '/'
143100970724     C                   endif
143200951127     c*
143300951127     C* decodifica causale (se diversa dalla precedente)
143400970416     C                   eval      s1causrig = bmvcauban
143500970416     C                   if        bmvcauban  <> Savcausrig
143600970416     C                   eval      savcausrig = BmvCauban
143700970416     C                   eval      wcausale    = BmvCauban
143800970416     C                   EXSR      CALLDvaBco
143900951127     C                   IF        Esito = *off
144000970416     C                   EVAL      S1CausrigD = BobDesbrev
144100951127     C                   ENDIF
144200951127     C                   else
144300970416     C                   EVAL      S1CausrigD = BobDesbrev
144400951127     C                   endif
144500951127     c* documento
144600951127     C                   CALLb     'XDT4000'
144700951127     C                   PARM      MovDTdoc      XDTAMG
144800951127     C     S1DTdoc       PARM                    XDTGMA
144900951127     C                   PARM      2             XDTSTA
145000951127     C                   eval      s1nrdoc = movnrdoc
145100951127     C                   eval      s1serdoc = movserdoc
145200951127     c*
145300951127     C                   eval      s1Cambio   = movcambio
145400970422     c* contropartita
145500970724     C                   eval      s1kcccpu = wrkkcccpu
145600970724     C                   eval      s1ksccpu = wrkksccpu
145700970422     C* decodifica contropartita
145800970422     C                   if        s1kcccpu<>*blank
145900970422     c                   eval      wkcc = s1kcccpu
146000970422     c                   eval      wksc = s1ksccpu
146100970422     c                   eval      Savnd002ds = nd002ds
146200970422     c                   exsr      CallMvc002
146300970422     c                   if        esito = *off
146400970422     c                   eval      s1ksccpud = desbrev002
146500970422     c                   endif
146600970422     c* rimetto ds banca
146700970422     c                   eval      nd002ds = savNd002ds
146800970422     c                   endif
146900970422     c*
147000970416     C                   EVAL      S1UNITA = bmvunita
147100951127     C                   eval      s1desbrev= movdesbrev
147200970731     C                   eval      s1caustes = movcaustes
147300970422     C                   eval      s1causriga= movcausrig
147400951127     C                   eval      s1classe = movclasse
147500970416     c                   eval      s1status = bmvstatus
147600970616     c                   eval      h1nrteso = bmvnrteso
147700951127     C*
147800951109     c* divise fisse
147900970416     C                   EVAL      h1Divisa   = bmvdivisa
148000951127     C                   EVAL      h1Monconto = xscdiv
148100951127     C*
148200951127     C* Calcola importi tramite routine edit
148300951127     C                   exsr      Importi
148400951110     C*
148500951110     c* Campi variabili a seguito richiesta moneta di conto o divisa
148600951110     C                   SELECT
148700951110     C* divisa
148800951110     C                   when      SavDivVis = '1'
148900970416     C                   EVAL      S1DiVisa1  = bmvdivisa
149000951110     C                   eval      s1saldo1   = H1saldod
149100951113     C                   eval      s1segno1   = H1segnod
149200951110     C                   EVAL      S1DiVisa2  = xscdiv
149300951110     C                   eval      s1saldo2   = H1saldom
149400951113     C                   eval      s1segno2   = H1segnom
149500951110     C* moneta di conto
149600951110     C                   when      SavDivVis = '0'
149700951110     C                   EVAL      S1Divisa1  = xscdiv
149800951110     C                   eval      s1saldo1   = H1saldom
149900951113     C                   eval      s1segno1   = H1segnom
150000970416     C                   EVAL      S1Divisa2  = bmvdivisa
150100951110     C                   eval      s1saldo2   = H1saldod
150200951113     C                   eval      s1segno2   = H1segnod
150300951110     C                   endsl
150400951115     C*
150500951115     C* se le divise sono uguali, imposto il valore del campo in drop
150600951115     C* uguale a quello della prima riga, così almeno sono allineati
150700970416     C                   IF        bmvdivisa = xscdiv
150800951115     C                   eval      s1saldo2 = s1saldo1
150900951115     C                   endif
151000951127     C*
151100951127     C* verifico se esistono descrizioni aggiuntive(riga)
151200951127     C                   EXSR      TestDesAGG
151300950301     C*
151400950301     C* imposto quegli attributi del sfl che sono modificabili
151500950301     C* al richiamo del determinato pgm
151600950301     C                   EXSR      ATRS1
151700940131     C*
151800940131     C                   ENDSR
151900970416     C/EJECT
152000970416     C*----------------------------------------------------------
152100970416     C* Controllo esistenza cauban con nddva
152200970416     C*----------------------------------------------------------
152300970416     C     CallDvaBco    BEGSR
152400970416     C*
152500970416     c                   move      Datmin        Dtope
152600970416     c                   eval      StrutturaO = 'ANBOB'
152700970416     C                   Eval      LenOut=%Size(ANBOB)
152800970416     C                   CallB     'NDDVABCO'
152900970416     C                   PARM      '01'          Operazione
153000970416     C                   PARM                    xscsoc
153100970416     C                   PARM                    BancaT11
153200970416     C                   PARM                    LineaT11
153300970416     C                   PARM                    WCausale
153400970416     C                   PARM                    DtOpe
153500970416     C                   PARM                    StrutturaO
153600970416     C                   PARM                    LenOut
153700970416     C                   PARM                    Esito
153800970416     C                   PARM                    ANBOB
153900970416     C*
154000970416     C                   ENDSR
154100951109     C/EJECT
154200951127     C************************************************************
154300951127     C* testa esistenza descrizioni aggiuntive di testata o riga
154400951127     C************************************************************
154500951127     C     TestDesAGG    BEGSR
154600951127     C                   EVAL      Desaggh   = '0'
154700951127     C* (si esegue setll sul file)
154800951127     C                   eval      DesSys = H1Sys
154900951127     C                   eval      DesNrAsReg = H1NrAsReg
155000951127     C                   eval      DesTipDes  = 'M'
155100951127     C                   eval      DesNrRigaM = H1NrRigaM
155200951127     C     K04DES01      SETLL     NDDES01L                               21
155300951127     C* se il bottone è presente
155400951127     C                   IF        *in21  = '1'
155500951127     C* memorizzo l'attributo di presente
155600951127     C                   EVAL      Desaggh   = '1'
155700951127     C                   else
155800951127     c* provo a vedere se ce ne sono di testata
155900951127     C                   eval      DesSys = H1Sys
156000951127     C                   eval      DesNrAsReg = H1NrAsReg
156100951127     C                   eval      DesTipDes  = 'T'
156200951127     C     K03DES01      SETLL     NDDES01L                               21
156300951127     C* se il bottone è presente
156400951127     C                   IF        *in21  = '1'
156500951127     C* memorizzo l'attributo di presente
156600951127     C                   EVAL      Desaggh   = '1'
156700951127     C                   ENDIF
156800951127     C                   endif
156900951127     C*
157000951127     C                   ENDSR
157100951109     C/EJECT
157200951109     C************************************************************
157300951109     C* Calcolo saldo per partita e totale se lo deve fare
157400951109     C************************************************************
157500980121     C     Importi       BEGSR
157600951113     C*
157700951113     C* addiziono per totale CONTO se lo deve fare (è sempre in moneta
157800951113     C* di conto)
157900951113     C                   if        $carpag = *off
158000970416     C                   if        bmvdare = 1
158100980130     C                   add       bmvimporto    WDare
158200980130     C* l'importo in divisa lo sommo solo se è della stessa divisa del
158300980130     C* rapporto bancario
158400980130     C                   IF        BMVDivisa = DivBRAT11 or
158500980130     C                             BMVDivisa = XscDiv and
158600980130     C                             DivBRAT11 = *blank
158700980130     C                   ADD       BMVImportD    WDareD
158800980130     C                   ENDIF
158900980130     C                   ENDIF
159000970416     C                   if        bmvavere = 1
159100980130     C                   add       bmvimporto    WAvere
159200980130     C* l'importo in divisa lo sommo solo se è della stessa divisa del
159300980130     C* rapporto bancario
159400980130     C                   IF        BMVDivisa = DivBRAT11 or
159500980130     C                             BMVDivisa = XscDiv and
159600980130     C                             DivBRAT11 = *blank
159700980130     C                   ADD       BMVImportD    WAvereD
159800980130     C                   ENDIF
159900951113     C                   endif
160000951127     C                   endif
160100951113     C*
160200951113     C* Dati in MONETA DI CONTO
160300951109     C*
160400951110     C* Segno
160500970416     C                   if        bmvdare = 1
160600970422     C                   eval      h1segnom  = Dare021
160700951127     C                   endif
160800970416     C                   if        bmvavere= 1
160900970422     C                   eval      h1segnom  = Avere021
161000951127     C                   endif
161100951110     C* Importo
161101051206C1886C                   Eval      H1saldomC= BmvImporto
161200970416     C                   Z-add     bmvimporto    WImporto
161300980121     C                   Z-add     NrDecMC       WNrDec
161400951110     C                   Eval      $LenFld = %Size(h1SaldoM)
161500951110     C* allineo dare a sinistra
161600970422     C                   if        h1segnoM = Dare021
161700951110     C                   SUB       5             $lenfld
161800951110     C                   Endif
161900951109     C                   EXSR      Edit
162000951127     C                   Eval      h1saldom = $RcvrVar
162100951109     C*
162200951113     C* Dati in DIVISA
162300951110     C* segno
162400951127     C                   eval      h1segnoD  = h1segnoM
162500951109     C*
162600951110     C* Importo in divisa
162700951110     C* reperisco decimali se divisa diversa dalla precedente letta
162800970416     C                   if        bmvdivisa <> Savdivisa
162900970416     C                   eval      SavDivisa = bmvdivisa
163000970416     C                   Eval      WDivisa =  bmvdivisa
163100951113     C                   Exsr      RepDivisa
163200980121     C                   Eval      NrDecDiv = X01NrDec
163300951113     C                   endif
163400951110     C*
163500951110     C* importo
163600970416     C                   EVAL      Wimporto = bmvImportD
163700980121     C                   Z-add     NrDecDiv      WNrDec
163800951110     C                   Eval      $LenFld = %Size(h1SaldoD)
163900951110     C*
164000951110     C* Imposto la dimensione del campo video in modo da allineare
164100951110     C* valute gestite con un numero diverso di decimali
164200951109     C                   Select
164300951110     C                   When      WNrDec = 2
164400951110     C                   Eval      $LenFld = %Size(h1SaldoD) - 1
164500951110     C                   When      WNrDec = 1
164600951110     C                   Eval      $LenFld = %Size(h1Saldod) - 2
164700951110     C                   When      WNrDec = 0
164800951110     C                   Eval      $LenFld = %Size(h1Saldod) - 4
164900951109     C                   Other
165000951110     C                   Eval      $LenFld = %Size(h1Saldod)
165100951109     C                   Endsl
165200951110     C* allineo dare a sinistra
165300970422     C                   if        H1SegnoD = Dare021
165400951110     C                   SUB       5             $lenfld
165500951110     C                   Endif
165600951109     C                   EXSR      Edit
165700951110     C                   Eval      h1Saldod  = $RcvrVar
165800951109     C*
165900951109     C                   ENDSR
166000950301     C/EJECT
166100950301     C************************************************************
166200950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
166300950301     C************************************************************
166400950301     C     ATRS1         BEGSR
166500950301     C*
166600951127    >C* impostare le condizioni per ripristinare gli attributi
166700951127    >C* della riga del sfl (se ce ne sono)
166800951127      * attiva o no unità
166900951127     C                   if        fgo1=*on
167000951127     C                   eval      *in11=*on
167100951127     C                   else
167200951127     C                   eval      *in11=*off
167300951127     C                   endif
167400951127      * se ci sono descrizioni aggiuntive sulla riga sottolineo causale
167500951127     C                   if        desaggh  = *on
167600951127     C                   eval      *in14=*on
167700951127     C                   else
167800951127     C                   eval      *in14=*off
167900951127     C                   endif
168000970129      * opzione protetta per riga saldo o per registrazione non ancora
168100970129     c* confermata, quindi non presente su ndreg
168200951128     C                   eval      *in41=in41h
168300950301     C*
168400950301     C                   ENDSR
168500941205     C/EJECT
168600941205     C************************************************************
168700941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
168800941205     C* INPUT
168900951109     C* $FC1 = Abilitazioni videata S1
169000941205     C* OUTPUT
169100941205     C* *IN97 *IN99 Errori
169200941205     C************************************************************
169300941205     C     CTRKS1        BEGSR
169400941205     C*
169500941205     C                   SETOFF                                       99
169600951109     C                   MOVEA     $FC1          $FC
169700941205     C                   EXSR      CTRKEY
169800941205     C     *IN99         IFEQ      *ON
169900941205     C                   SETON                                        97
170000941205     C                   ENDIF
170100941205     C*
170200941205     C                   ENDSR
170300941206     C/EJECT
170400941206     C************************************************************
170500941206     C* GESTIONE F08 VIDEO S1
170600941206     C************************************************************
170700941206     C     F08S1         BEGSR
170800941206     C*
170900941206     C                   MOVE      *ON           $FINE
171000970416    >C                   MOVE      '0'           RETT11
171100941206     C*
171200941206     C                   ENDSR
171300940309     C/EJECT
171400940309     C************************************************************
171500940309     C* GESTIONE F03 VIDEO S1
171600940309     C************************************************************
171700940309     C     F03S1         BEGSR
171800940309     C*
171900940309     C                   MOVE      *ON           $FINE
172000970416    >C                   MOVE      '1'           RETT11
172100940325     C* fine programma
172200940325     C                   EXSR      ENDPGM
172300940309     C*
172400940309     C                   ENDSR
172500951109     C/EJECT
172600951109     C************************************************************
172700951109     C* GESTIONE F11 VIDEO S1 - cambio moneta
172800951109     C************************************************************
172900951109     C     F11S1         BEGSR
173000951113     C*
173100951113     C* Eseguo solo se ci sono record nel sfl
173200951113     C                   IF        Wmax <> 0
173300951110     C* Scambio richiesta
173400951110     C                   select
173500951110     C                   when      Savdivvis = '0'
173600951110     C                   eval      Savdivvis = '1'
173700951110     C*
173800951110     C                   when      Savdivvis = '1'
173900951110     C                   eval      Savdivvis = '0'
174000951110     C                   endsl
174100951110     C*
174200951110     C                   DO        *hival        $X                4 0
174300970416     C     $X            chain     cT11s1                             21
174400951110     C   21              LEAVE
174500951110     C                   select
174600951110     C                   when      Savdivvis = '0'
174700951110     C                   eval      s1saldo1 = h1saldom
174800951127     C                   EVAL      S1DiVisa1  = h1monconto
174900951110     C                   eval      s1saldo2 = h1saldod
175000951127     C                   EVAL      S1DiVisa2  = h1divisa
175100951110     c*
175200951110     C                   when      Savdivvis = '1'
175300951110     C                   eval      s1saldo1 = h1saldod
175400951127     C                   EVAL      S1DiVisa1  = h1divisa
175500951110     C                   eval      s1saldo2 = h1saldom
175600951127     C                   EVAL      S1DiVisa2  = h1monconto
175700951110     C                   endsl
175800951115     C*
175900951115     C* se le divise sono uguali, imposto il valore del campo in drop
176000951115     C* uguale a quello della prima riga, così almeno sono allineati
176100951115     C                   IF        s1divisa1 = s1divisa2
176200951115     C                   eval      s1saldo2 = s1saldo1
176300951115     C                   endif
176400960626     c                   if        s1opz <> 0
176500960626     C                   seton                                        32
176600960627     c                   else
176700960627     c                   setoff                                       32
176800960626     c                   endif
176900951113     C* imposto quegli attributi del sfl che sono modificabili
177000951113     C* al richiamo del determinato pgm
177100951113     C                   EXSR      ATRS1
177200960627     c* spengo indicatore r.i. per opzione
177300960627     C                   setoff                                       40
177400951113     C*
177500970416     C                   UPDATE    CT11S1
177600951110     C*
177700951110     C                   ENDDO
177800951109     C*
177900951113     C                   ENDIF
178000980119     C*
178100980119     C* se il conto in esame è valutario (la divisa del rapporto è <> m.c.)
178200980119     C                   IF        DivBRAT11 <> XscDiv
178300980119     C* eseguo il cambio moneta anche sui dati di saldo
178400980119     C                   IF        *in08 = *off
178500980119     C                   SETON                                        08
178600980119     C                   ELSE
178700980119     C                   SETOFF                                       08
178800980119     C                   ENDIF
178900980119     C                   ENDIF
179000980119     C*
179100951109     C                   ENDSR
179200940309     C/EJECT
179300940309     C************************************************************
179400940309     C* GESTIONE F12 VIDEO S1
179500940309     C************************************************************
179600940309     C     F12S1         BEGSR
179700940309     C*
179800940309     C                   MOVE      *ON           $FINE
179900970416    >C                   MOVE      '2'           RETT11
180000940309     C*
180100940309     C                   ENDSR
180200951109     C/EJECT
180300951109     C************************************************************
180400951109     C* GESTIONE F15 VIDEO S1
180500951109     C************************************************************
180600951109     C     F15S1         BEGSR
180700951109     C*
180800951109     C                   reset                   ana399ds
180900951109     C                   eval      A39societa = XSCSOC
181000970416     C                   eval      A39unita = unitaT11
181100970416     C                   eval      A39ctb = ctbT11
181200970416     C                   eval      A39kcc = kccT11
181300970416     C                   eval      A39ksc = KSCT11
181400970416     C                   eval      A39SNATURA = snatura002
181500951109     C                   movel     ana399ds      kpjbu
181600951109     C                   CALL      'ANA399R'                            21
181700951109     C                   PARM                    KPJBA
181800951109     C     A39ERRORI     IFEQ      '0'
181900951109     C                   endif
182000951109     C*
182100951109     C                   ENDSR
182200970515     C/EJECT
182300970515     C************************************************************
182400970515     C* GESTIONE F20 VIDEO S1 - Stampa E/C
182500970515     C************************************************************
182600970515     C     F20S1         BEGSR
182700970515     C* Reperimento titolo stampa
182800970515     C                   EXSR      REPTIT
182900970515     C                   RESET                   NDCT31DS
183000970515     C                   MOVE      xscsoc        socT31
183100970515     C                   MOVE      CtbT11        ctbT31
183200970515     C                   MOVE      kcct11        KCCT31
183300970515     C                   MOVE      ksct11        KSCT31
183400970515     C                   MOVE      bancaT11      bancaT31
183500970515     C                   MOVE      lineaT11      lineaT31
183600970515     C                   move      movdefT11     defT31
183700970515     C                   move      movprovT11    prvT31
183800970515     C                   move      movgesT11     gesT31
183900970515     C                   move      dtperdaT11    dtdT31
184000970515     C                   move      dtperaT11     dtaT31
184100970515     C                   MOVE      modalitT11    modT31
184200970515     C                   MOVE      divvisT11     divT31
184300980911     C                   MOVE      riconciT11    RiconcT31
184400970515     c                   eval      lrT31 = *on
184500970515     C                   MOVEL     NDCT31DS      KPJBU
184600970515     C                   CALL      'NDCT33R1'
184700970515     C                   PARM                    KPJBA
184800970515     C                   PARM                    TIT              50
184900970515     C*
185000970515     c                   endsr
185100970515     C/EJECT
185200970515     C************************************************************
185300970515     C* REPERIMENTO TITOLO stampa
185400970515     C************************************************************
185500970515     C     REPTIT        BEGSR
185600970515     C*
185700970515     C                   MOVE      *BLANKS       TIT
185800970515     C*
185900970515     C                   MOVEL     $TIT(1)       IDMSG
186000970515     C                   CALLB     'XRTVM'
186100970515     C                   PARM                    IDMSG            27
186200970515     C                   PARM                    TXTMSG          100
186300970515     C                   PARM                    ERRMSG            1
186400970515     C                   PARM      *ON           CTRMSG            1            centratura
186500970515     C                   PARM      50            LENMSG            3 0          lun output.
186600970515     C*
186700970515     C     ERRMSG        IFNE      *ON
186800970515     C                   MOVEL     TXTMSG        TIT
186900970515     C                   ELSE
187000970515     C                   MOVEL     *ALL'?'       TIT
187100970515     C                   ENDIF
187200970515     C*
187300970515     C                   ENDSR
187400940309     C/EJECT
187500940309     C************************************************************
187600940309     C* GESTIONE F23 VIDEO S1
187700940309     C************************************************************
187800940309     C     F23S1         BEGSR
187900940309     C*
188000941220    >C                   Z-ADD     76            $L01
188100941220    >C                   Z-ADD     76            $L02
188200940309     C                   EXSR      RIEOPZ
188300941214     C                   Z-ADD     23            X
188400951109     C                   MOVE      FF23          $FC1(X)
188500940608     C                   MOVEL     WOPZ1         C1OPZ1
000000060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
188600951109     C                   MOVEL     WOPZ2         C1OPZ2
188700940309     C                   Z-ADD     WCOZ          C1COZ
188800940309     C*
188900940309     C                   ENDSR
189000940309     C/EJECT
189100940309     C************************************************************
189200940309     C* GESTIONE F24 VIDEO S1
189300940309     C************************************************************
189400940309     C     F24S1         BEGSR
189500940506     C*
189600951109     C                   MOVEA     $FX1          $FX
189700951109     C                   MOVEA     $FC1          $FC
189800951109     C                   MOVEA     $FR1          $FR
189900940506     C                   Z-ADD     $ULKS1        $ULK
190000941220    >C                   Z-ADD     79            $L01
190100941220    >C                   Z-ADD     79            $L02
190200940506     C                   EXSR      RIEKEY
190300940506     C                   Z-ADD     $ULK          $ULKS1            3 0
190400951109     C                   MOVEA     $FC           $FC1
190500951109     C                   MOVEL     $KEY1         Z1KE1
000000060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
190600951109     C                   MOVEL     $KEY2         Z1KE2
190700970416     C                   WRITE     CT11Z1
190800940506     C*
190900940309     C                   ENDSR
191000940128     C/EJECT
191100940128     C************************************************************
191200940131     C* CONTROLLO TESTATA LISTA
191300940128     C************************************************************
191400940131     C     CTRC1         BEGSR
191500940128     C*
191600940201     C                   MOVE      *OFF          *IN99
191700951109     C*
191800951109     C                   if        c1pos <> 0
191900951109     C                   CALLb     'XDT4000'
192000951109     C                   PARM                    XDTAMG           10
192100951109     C                   PARM      c1pos         XDTGMA            6 0
192200951109     C                   PARM      3             XDTSTA            1 0
192300951109     C     XDTSTA        IFLT      0
192400951109     C                   SETON                                        6099
192500951109     C                   endif
192600951109     C                   endif
192700940131     C*
192800940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
192900950707     C* L'INIZIALIZZAZIONE LISTA   (lo attivo anche se w1pos = c1pos, per ri
193000950707     C* partire dal primo numero partita
193100951109     C                   IF        *in99 = *off
193200950707     C     C1POS         IFNE      W1POS
193300950704    >C* aggiungere in OR altri campi posizionamento se ci sono
193400940202     C                   EXSR      POSS1
193500950707     C                   ENDIF
193600951109     C                   Endif
193700940202     C*
193800940202     C                   ENDSR
193900940202     C/EJECT
194000940202     C************************************************************
194100940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
194200940202     C************************************************************
194300940202     C     POSS1         BEGSR
194400940202     C*
194500940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
194600940202     C*
194700941118   B1C     WMAX          IFGT      0
194800970416     C                   READC     CT11S1                                 21
194900941118   B2C     *IN21         DOWEQ     *OFF
195000940202     C     *IN99         ANDEQ     *OFF
195100940202     C*
195200941118   B3C     S1OPZ         IFNE      0
195300940202     C                   SETON                                        51  99
195400941118   E3C                   ENDIF
195500940202     C* se rilevata opzione attiva, attivo sflnxtchg
195600941118   B3C     *IN99         IFEQ      *ON
195700940202     C                   MOVE      *ON           *IN32
195800950301     C*
195900950301     C* imposto quegli attributi del sfl che sono modificabili
196000950301     C* al richiamo del determinato pgm
196100941118     C                   EXSR      ATRS1
196200941118     C*
196300970416     C                   UPDATE    CT11S1
196400941118   X3C                   ELSE
196500970416     C                   READC     CT11S1                                 21
196600941118   E3C                   ENDIF
196700940202     C*
196800941118   E2C                   ENDDO
196900940202     C*
197000941118   E1C                   ENDIF
197100940202     C*
197200940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
197300940221     C* AL VALORE DI POSIZIONAMENTO :
197400940221     C* se questo è maggiore del valore del rispettivo campo di
197500940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
197600940221     C* con i valori di parzializzazione.
197700940221     C*
197800941118   B1C     *IN99         IFEQ      *OFF
197900950704    >C                   MOVE      *ZEROS        W1POS
198000940202     C*
198100950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
198200950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
198300950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
198400950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
198500950525     C* verificata la condizione suddetta avverrà con posizionamento a
198600950525     C* partire dal valore di parzializzazione
198700950525     C*
198800941118   B2C                   SELECT
198900950704    >C* Attenzione : se un ordinamento ha un campo di posizionamento
199000950704    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
199100950704    >C* dei relativi campi
199200940202     C*
199300940202     C     $ORD          WHENEQ    1
199400950406     C                   MOVEL     C1POS         W1POS
199500940202     C*
199600940202     C     $ORD          WHENEQ    2
199700941214     C                   MOVE      C1POS         W1POS
199800950525     C*
199900941118   E2C                   ENDSL
200000950525     C*
200100950525     C* richiedo l'inizializzazione sfl
200200950525     C                   MOVE      *ON           *IN99
200300950525     C                   MOVE      *ON           $INZS1
200400940202     C*
200500941118   E1C                   ENDIF
200600940202     C*
200700940131     C                   ENDSR
200800940131     C/EJECT
200900940131     C************************************************************
201000940131     C* CONTROLLO OPZIONI LISTA
201100940131     C************************************************************
201200940131     C     CTRS1         BEGSR
201300940131     C*
201400940202     C                   MOVEL     *OFF          $ESCI
201500940201     C                   SETOFF                                       99
201600940131     C                   Z-ADD     0             S1OPZ
201700940131     C*
201800940127     C* Leggo il sfl solo se ci sono rcd
201900941118   B1C     WMAX          IFGT      0
202000970416     C                   READC     CT11S1                                 21
202100940127     C*
202200940131     C* esce se fine sfl o errore che richiede l'uscita
202300941118   B2C     *IN21         DOWEQ     *OFF
202400940131     C     $ESCI         ANDEQ     *OFF
202500940201     C                   Z-ADD     S1NRR         C1RCD
202600940131     C* ctrl su riga
202700940131     C                   EXSR      RECS1
202800940131     C* gestione opzioni
202900941118   B3C     S1OPZ         IFNE      0
203000940201     C     *IN99         ANDEQ     *OFF
203100940131     C                   EXSR      OPZS1
203200941118   E3C                   ENDIF
203300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
203400941118   B3C     *IN99         IFEQ      *ON
203500940131     C                   MOVE      *ON           *IN32
203600941118   E3C                   ENDIF
203700950301     C*
203800950301     C* imposto quegli attributi del sfl che sono modificabili
203900950301     C* al richiamo del determinato pgm
204000941118     C                   EXSR      ATRS1
204100941118     C*
204200940131     C                   Z-ADD     0             S1OPZ
204300940223     C*
204400970416     C                   UPDATE    CT11S1
204500941125     C                   MOVE      *OFF          *IN32
204600940223     C*
204700940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
204800941118   B3C     $ESCI         IFEQ      *OFF
204900970416     C                   READC     CT11S1                                 21
205000940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
205100941118   B4C     *IN21         IFEQ      *ON
205200940201     C                   MOVE      WINZS1        $INZS1
205300940204     C* calcolo pagina a cui deve posizionarsi
205400940728     C     WSFL          IFGT      0
205500940728     C                   EXSR      CLCPAG
205600940728     C                   ELSE
205700940728     C                   Z-ADD     SFLPAG        WPAG
205800940728     C                   ENDIF
205900941118   E4C                   ENDIF
206000941118   E4C                   ENDIF
206100940131     C*
206200941118   E2C                   ENDDO
206300940127     C*
206400941118   E1C                   ENDIF
206500940131     C*
206600940127     C                   ENDSR
206700940127     C/EJECT
206800940127     C************************************************************
206900940131     C* CONTROLLO CAMPI I/O RIGA LISTA
207000940127     C************************************************************
207100940131     C     RECS1         BEGSR
207200940131     C*
207300940201     C* reset indicatori DSPATR
207400940201     C                   MOVE      *ALL'0'       IN4049           10
207500940201     C                   MOVEA     IN4049        *IN(40)
207600940201     C*
207700940131     C* Controllo se Utente Autorizzato alla scelta
207800940202     C     S1OPZ         IFNE      0
207900940131     C                   Z-ADD     1             X
208000940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
208100940211     C     *IN21         IFEQ      *OFF
208200940223     C     $FL1(X)       OREQ      'N'
208300940131     C                   MOVE      *ON           $ESCI
208400940201     C                   SETON                                        504099
208500940131     C                   ENDIF
208600940202     C                   ENDIF
208700970613     c*
208800970613     C* Se opzione riconcilia, controllo se utente autorizzato
208900970724     c                   if        s1opz = 52 and TesRicMan = *off
209000970613     C                   SETON                                        944099
209100970613     c                   endif
209200970911     C*
209300970911     C* E' possibile l'opzione 50=riconciliazione solo se il movimento è
209400970911     C* riconciliato o chiuso ed esiste il numero di riconciliazione
209500970911     c                   IF        S1Opz = 50   and
209600980922     c                             (S1Status = ' ' or H1NrTeso = 0)
209700970724     C                   MOVE      *ON           $ESCI
209800970724     C                   SETON                                        407499
209900970724     c                   endif
210000970911     C*
210100970911     C* E' possibile l'opzione 51=De/riconciliazione solo se il movimento è
210200970911     C* riconciliato ed esiste il numero di riconciliazione
210300970911     c                   IF        S1Opz = 51   and
210400980922     c                             (S1Status <> '1' or H1NrTeso = 0)
210500970911     C                   MOVE      *ON           $ESCI
210600970911     C                   SETON                                        407499
210700970911     c                   endif
210800950706     C*
210900940131     C                   ENDSR
211000941118     C/EJECT
211100941118     C************************************************************
211200941118     C* GESTIONE OPZIONI LISTA
211300941118     C************************************************************
211400941118     C     OPZS1         BEGSR
211500950301     C*
211600950301     C                   Z-ADD     RIRI          CURR
211700950301     C                   Z-ADD     COCO          CURC
211800941118     C*
211900941027     C                   SELECT
212000960318     C*
212100960318     C* se opz=22 richiamo visualizzazione partite
212200960318     C* passo la chiave al pgm chiamante
212300960318     C     S1OPZ         WHENEQ    22
212400960318     C*
212500960318     C                   RESET                   NDC073DS
212600960318     c                   move      xscsoc        societa073
212700960318     C                   MOVEL     H1Sys         Sys073
212800970605     C                   z-add     H1NrAsReg     NrAsReg073
212900970605     C                   z-add     H1NrRigaM     NrRigaM073
213000970416     C                   movel     descr002      kcckscd073
213100960318     C                   move      winz073       exf073
213200960318     C                   MOVE      '05'          Opz073
213300970416     c                   eval      snatura073  = snatura002
213400960318     C*
213500960318     C                   movel     NDC073DS      KPJBU
213600960318     C                   CALL      'NDC073R'
213700960318     C                   PARM                    Kpjba
213800960318     C                   Movel     kpjbu         NDC073DS
213900960318     C*
214000960318     C                   Exsr      Rtn073
214100951205     C*
214200951205     C* se opz=23 richiamo tesoreria
214300951205     C* passo la chiave al pgm chiamante
214400951205     C     S1OPZ         WHENEQ    23
214500951205     C*
214600951205     C                   RESET                   NDC084DS
214700951205     C                   MOVEL     H1Sys         Sys084
214800970605     C                   z-add     H1NrAsReg     NrAsReg084
214900970605     C                   z-add     H1NrRigaM     NrRigaM084
215000951205     C                   MOVE      '02'          Opz084
215100960105     C                   move      wexf084       exf084
215200951205     C*
215300951205     C                   movel     NDC084DS      KPJBU
215400951205     C                   CALL      'NDC084R'
215500951205     C                   PARM                    Kpjba
215600951205     C                   Movel     kpjbu         NDC084DS
215700951205     C*
215800951205     C                   Exsr      Rtn084
215801040212     C*
215802040212C1718C* se opz=25 richiamo cash flow
215803040212  "  C     S1OPZ         WHENEQ    25
215805040212  "  C                   RESET                   NDCT68DS
215806040212  "  C                   MOVEL     '02'          OPZt68
215809040212  "  C                   z-add     H1Sys         SYST68
215810040212  "  C                   z-add     H1NrAsReg     NRASREGT68
215811040212  "  C                   z-add     H1NrRigaM     NRRigamT68
215812040212  "  C                   move      wexft68       inzt68
215813040220  "  c                   eval      divisaT68 = h1divisa
215814040910  "  c                   eval      DescrT68  = s1CausrigD
215815040212  "  C*
215816040212  "  C                   movel     NDCt68DS      KPJBU
215817040212  "  C                   CALL      'NDCT68R'
215818040212  "  C                   PARM                    Kpjba
215819040212  "  C                   movel     KPJBU         ndcT68DS
215820040212  "  C* ritorno da PGM gestione
215821040212C1718C                   EXSR      RTNT68
215900960104     C*
215901051205C1886C* se opz=30 Movimento home banking
215902051205  "  C     S1OPZ         WHENEQ    30
215903051213  "  c                   reset                   x48chPgmds
215904051213  "  C                   eval      x48pgm = 'NDCK40R'
215905051213  "  c                   eval      kpjbu = x48chpgmds
215906051213  "  c                   call      'X48CHPGM'
215907051213  "  c                   parm                    kpjba
215908051213  "  c                   movel     kpjbu         x48chpgmds
215909051213  "  c                   if        x48errore = *off
215910051205  "  C                   RESET                   NDCK40DS
215911051213  "  C                   MOVEL     '05'          OPZK40
215912051206  "  c                   move      xscsoc        SocietaK40
215913051206  "  c                   movel     C1Banca       BancaK40
215914051206  "  c                   movel     C1Linea       LineaK40
215915051206  "  c                   z-add     H1SaldomC     ImpoDaK40
215916051206  "  c                   z-add     H1SaldomC     ImpoAK40
215917051206  "  c                   move      H1SegnoM      SegnoK40
215918051206  "  C     *JobRun       Move      s1dtvaluta    Dataiso
215919051206  "  c     Dataiso       subdur    2:*D          dataiso1
215920051206  "  c                   move      Dataiso1      DvaDaK40
215921051206  "  c     Dataiso       adddur    2:*D          dataiso1
215922051206  "  c                   move      Dataiso1      DvaAK40
215923051206  "  C     *JobRun       Move      s1dtope       Dataiso
215924051206  "  c     Dataiso       subdur    2:*D          dataiso1
215925051206  "  c                   move      Dataiso1      DopDaK40
215926051206  "  c     Dataiso       adddur    2:*D          dataiso1
215927051206  "  c                   move      Dataiso1      DopAK40
215928051205  "  C                   movel     NDCK40DS      KPJBU
215929051205  "  C                   CALL      'NDCK40R'
215930051205  "  C                   PARM                    Kpjba
215931051205  "  C                   movel     KPJBU         ndcK40DS
215932051205  "  C* ritorno da PGM gestione
215933051213  "  C                   EXSR      RTNK40
215934051213c1886c                   endif
216000951205     C*
216100951205     C* Richiamo prima nota
216200951205     C     S1OPZ         WHENEQ    41
216300961121     c*controllo chiamata ricorsiva p.nota
216400961121     c                   reset                   x48chPgmds
216500961121     C                   eval      x48pgm = 'NDC001R'
216600961121     c                   eval      kpjbu = x48chpgmds
216700961121     c                   call      'X48CHPGM'
216800961121     c                   parm                    kpjba
216900961121     c                   movel     kpjbu         x48chpgmds
217000961121     c                   if        x48errore = *off
217100951205      *
217200951205     C                   RESET                   NDC001DS
217300951205     C                   MOVEL     '02'          OPZ001
217400951205     C* passare nei campi appositi della DS C004DS il valore
217500951205     C* della chiave della VL univoca
217600951205     C                   MOVEL     H1Sys         UQ1001
217700951205     C                   MOVEL     H1NrAsReg     UQ2001
217800951205     C*
217900951205     C                   movel     NDC001DS      KPJBU
218000951205     C                   CALL      'NDC001R'
218100951205     C                   PARM                    Kpjba
218200951205     C                   movel     KPJBU         ndc001ds
218300951205     C* ritorno da PGM gestione
218400951205     C                   EXSR      RTN001
218500961121     c                   endif
218600951128     C*
218700951128     C* se opz=44 o 45 descrizioni aggiuntive
218800951128     C* passo la chiave al pgm chiamante
218900951128     C     S1OPZ         WHENEQ    44
219000951128     C     S1OPZ         oreq      45
219100951128     C                   RESET                   NDC027DS
219200951128     C*
219300951128     C                   MOVEL     '02'          OPZ027
219400951128     C* passare nei campi appositi della DS NDC027DS il valore
219500951128     C* della chiave della VL univoca
219600951128     C                   MOVEL     H1sys         Sys027
219700970605     C                   z-add     h1nrasreg     NrAsReg027
219800951128     c                   if        S1opz = 45
219900970605     C                   z-add     h1nrrigam     NrRigaM027
220000951128     C                   MOVEL     'M'           TipDes027
220100951128     C                   else
220200951128     C                   MOVEL     'T'           TipDes027
220300951128     C                   endif
220400951128     C* Faccio eseguire il commit al pgm chiamato
220500951128     C                   MOVE      '0'           Cmt027
220600951128     C                   MOVEL     *on           Inz027
220700951128     C*
220800951128     C* passo tutti quei campi che possono servire al pgm
220900951128     C                   MOVEL     NDC027DS      KPJBU
221000951128     C                   CALL      'NDC027R'
221100951128     C                   PARM                    KPJBA
221200951128     C                   MOVEL     KPJBU         NDC027DS
221300951128     C* ritorno da PGM gestione
221400951128     C                   EXSR      RTN027
221500970724     C*
221600970724     C* se opz=Riconciliazione
221700970724     C     S1OPZ         WHENEQ    50
221800970724     c                   Reset                   NdcT43Ds
221900970724     c                   movel     XScSoc        SocietaT43
222000970724     c                   movel     C1Banca       BancaT43
222100970724     c                   movel     C1Linea       LineaT43
222200970724     c                   Z-add     H1NrTeso      NrRicT43
222300970724     C                   eval      WdwT43     =  WrkWdwT43
222400970724     C                   eval      kpjbu = ndct43ds
222500970724     C                   CALL      'NDCT43R'
222600970724     C                   PARM                    KPJBA
222700970724     C                   movel     kpjbu         NdcT43Ds
222800970724     C* ritorno da PGM
222900970724     C                   EXSR      RTNT43
223000970724     C*
223100970724     C                   MOVE      *HIVAL        $LASTV
223200970613     C*
223300970723     C* se opz=51 richiamo programma per de-riconciliare
223400970613     C     S1OPZ         WHENEQ    51
223500970613     C*
223600970613     C                   RESET                   NDCt15DS
223700970613     C                   MOVE      '01'          OpzT15
223800970613     C                   move      xscsoc        SocietaT15
223900970613     c                   move      C1Banca       BancaT15
224000970613     c                   move      C1Linea       LineaT15
224100970613     C                   z-add     H1NrTeso      NrTesoT15
224200970613     C*
224300970613     C                   movel     NDCt15DS      KPJBU
224400970613     C                   CALL      'NDCT15R'
224500970613     C                   PARM                    Kpjba
224600970613     C                   Movel     kpjbu         NDCT15DS
224700970613     C*
224800970613     C                   Exsr      Rtnt15
224900970710     c                   if        errT15 = '4'
225000970710     C                   Z-add     nrtesoT15     Pack0315
225100970710     c                   endif
225200970724     c*
225300970724     C                   MOVE      *HIVAL        $LASTV
225400970724     C*
225500970724     C* se opz=52 richiamo programma per riconciliare
225600970724     C     S1OPZ         WHENEQ    52
225700970724     C*
225800970724     C                   RESET                   NDCt14DS
225900970724     C                   MOVE      '01'          OpzT14
226000970724     C                   MOVEL     H1Sys         Syst14
226100970724     C                   z-add     H1NrAsReg     NrAsRegT14
226200970724     C                   z-add     H1NrRigaM     NrRigaMT14
226300970724     C*
226400970724     C                   movel     NDCt14DS      KPJBU
226500970724     C                   CALL      'NDCT14R'
226600970724     C                   PARM                    Kpjba
226700970724     C                   Movel     kpjbu         NDCT14DS
226800970724     C*
226900970724     C                   Exsr      Rtnt14
227000940225     C*
227100950620     C* se altre opzioni
227200950620     C* richiamo pgm gestione anagrafica
227300950907     C* anche per opzione blocco/sblocco, dove eseguirò solo aggiornamento
227400950907     C* archivio
227500941027     C                   OTHER
227600940131     C*
227700940506     C                   MOVE      *HIVAL        $LASTV
227800950907     C*
227900950907     C*
228000941027     C                   ENDSL
228100940225     C*
228200940131     C                   ENDSR
228300951128     C/EJECT
228400951128     C************************************************************
228500951128     C* GESTIONE RITORNO DA PGM DESCRIZIONI AGGIUNTIVE
228600951128     C************************************************************
228700951128     C     RTN027        BEGSR
228800951128     C*
228900951128     C* modo di ritorno
229000951128     C*
229100951128   B1C                   SELECT
229200951128     C* 1 = F3
229300951128     C     RET027        WHENEQ    '1'
229400951128     C                   MOVE      *ON           $ESCI
229500951128     C                   MOVE      *ON           $FINE
229600970416     C                   MOVE      '1'           RETT11
229700951128     C* 2 = F12
229800951128     C     RET027        WHENEQ    '2'
229900951128     C                   MOVE      *ON           $ESCI
230000951128     C*
230100951128   E1C                   ENDSL
230200951128     C*
230300951128     C* operazione eseguite dal pgm chiamato
230400951128     C*
230500951128    >C                   MOVE      OPR027        WOPR
230600951128     C                   SELECT
230700951128     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
230800951128     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
230900951128    >C     OPR027        WHENEQ    '1'
231000951128     C* verifico se esistono descrizioni aggiuntive(riga)
231100951128     C* non lo faccio perchè non verranno quasi mai modificate
231200951128     C*****              EXSR      TestDesAGG
231300951128     C*
231400951128     C                   ENDSL
231500951128     C*
231600951128     C* funzione non eseguibile per errore :
231700951128     C*
231800951128     C                   SELECT
231900951128     C* 1 = funzione richiamata chiusa in errore
232000951128    >C     ERR027        WHENEQ    '1'
232100951128     C                   MOVE      *ON           $ESCI
232200951128     C                   SETON                                        9599
232300951128     C* 2 = funzione richiamata senza la dovuta autorizzazione
232400951128    >C     ERR027        WHENEQ    '2'
232500951128     C                   MOVE      *ON           $ESCI
232600951128     C                   SETON                                        9499
232700951128     C*
232800951128     C                   ENDSL
232900951128     C*
233000951128     C                   ENDSR
233100951128     C/EJECT
233200951128     C************************************************************
233300951128     C* GESTIONE RITORNO DA PGM primanota generica
233400951128     C************************************************************
233500951128     C     RTN001        BEGSR
233600951128     C*
233700951128     C* modo di ritorno
233800951128     C*
233900951128   B1C                   SELECT
234000951128     C* 1 = F3
234100951128     C     RET001        WHENEQ    '1'
234200951128     C                   MOVE      *ON           $ESCI
234300951128     C                   MOVE      *ON           $FINE
234400970416     C                   MOVE      '1'           RETT11
234500951128     C* 2 = F12
234600951128     C     RET001        WHENEQ    '2'
234700960125     c     opr001        andne     '1'
234800951128     C                   MOVE      *ON           $ESCI
234900951128     C*
235000951128   E1C                   ENDSL
235100951128     C*
235200951128     C* operazione eseguite dal pgm chiamato
235300951128     C*
235400951128    >C                   MOVE      OPR001        WOPR
235500951128     C                   SELECT
235600951128     C* 1 = eseguito aggiornamento (richiesto cariamento subfile
235700951128     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
235800951128    >C     OPR001        WHENEQ    '1'
235900951128     C                   move      *ON           winzs1
236000970422     c* richiamo routine per riaggiornare il saldo finale
236100970422     c* richiamo routine per scrivere saldi totali
236200970422     c                   exsr      CallSaldi
236300951128     C*
236400951128     C                   ENDSL
236500951128     C*
236600951128     C* funzione non eseguibile per errore :
236700951128     C*
236800951128     C                   SELECT
236900951128     C* 1 = funzione richiamata chiusa in errore
237000951128    >C     ERR001        WHENEQ    '1'
237100951128     C                   MOVE      *ON           $ESCI
237200951128     C                   SETON                                        9599
237300951128     C* 2 = funzione richiamata senza la dovuta autorizzazione
237400951128    >C     ERR001        WHENEQ    '2'
237500951128     C                   MOVE      *ON           $ESCI
237600951128     C                   SETON                                        9499
237700951128     C* 3 = registrazione non modificabile
237800951128     C     ERR001        WHENEQ    '3'
237900951128     C                   MOVE      *ON           $ESCI
238000951128     C                   SETON                                        9399
238100951128     C*
238200951128     C                   ENDSL
238300951128     C*
238400951128     C*
238500951128     C                   ENDSR
238600960318     C/EJECT
238700960318     C************************************************************
238800960318     C* GESTIONE RITORNO DA PGM partite
238900960318     C************************************************************
239000960318     C     RTN073        BEGSR
239100960318     C*
239200960318     C* modo di ritorno
239300960318     C*
239400960318   B1C                   SELECT
239500960318     C* 1 = F3
239600960318     C     RET073        WHENEQ    '1'
239700960318     C                   MOVE      *ON           $ESCI
239800960318     C                   MOVE      *ON           $FINE
239900970416     C                   MOVE      '1'           RETT11
240000960318     C* 2 = F12
240100960318     C     RET073        WHENEQ    '2'
240200960318     C                   MOVE      *ON           $ESCI
240300960318     C*
240400960318   E1C                   ENDSL
240500960318     C*
240600960318     C* operazione eseguite dal pgm chiamato
240700960318     C*
240800960318    >C                   MOVE      OPR073        WOPR
240900960318     C                   SELECT
241000960318     C* 1 = eseguito aggiornamento
241100960318    >C     OPR073        WHENEQ    '1'
241200960318     C                   move      *ON           winzs1
241300960318     C*
241400960318     C                   ENDSL
241500960318     C*
241600960318     C* funzione non eseguibile per errore :
241700960318     C*
241800960318     C                   SELECT
241900960318     C* 1 = funzione richiamata chiusa in errore
242000960318    >C     ERR073        WHENEQ    '1'
242100960318     C                   MOVE      *ON           $ESCI
242200960318     C                   SETON                                        9599
242300960318     C* 2 = funzione richiamata senza la dovuta autorizzazione
242400960318    >C     ERR073        WHENEQ    '2'
242500960318     C                   MOVE      *ON           $ESCI
242600960318     C                   SETON                                        9499
242700960318     C*
242800960318     C                   ENDSL
242900960318     C*
243000960318     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
243100960318     C* di sola finestra
243200960318     C                   MOVEL     '0'           winz073
243300960318     C*
243400960318     C                   ENDSR
243500951128     C/EJECT
243600951128     C************************************************************
243700951128     C* GESTIONE RITORNO DA PGM tesoreria
243800951128     C************************************************************
243900951128     C     RTN084        BEGSR
244000951128     C*
244100951128     C* modo di ritorno
244200951128     C*
244300951128   B1C                   SELECT
244400951128     C* 1 = F3
244500951128     C     RET084        WHENEQ    '1'
244600951128     C                   MOVE      *ON           $ESCI
244700951128     C                   MOVE      *ON           $FINE
244800970416     C                   MOVE      '1'           RETT11
244900951128     C* 2 = F12
245000951128     C     RET084        WHENEQ    '2'
245100951128     C                   MOVE      *ON           $ESCI
245200951128     C*
245300951128   E1C                   ENDSL
245400951128     C*
245500951128     C* operazione eseguite dal pgm chiamato
245600951128     C*
245700951128    >C                   MOVE      OPR084        WOPR
245800951128     C                   SELECT
245900960129     C* 1 = eseguito aggiornamento
246000951128    >C     OPR084        WHENEQ    '1'
246100960129     C                   move      *ON           winzs1
246200951128     C*
246300951128     C                   ENDSL
246400951128     C*
246500951128     C* funzione non eseguibile per errore :
246600951128     C*
246700951128     C                   SELECT
246800951128     C* 1 = funzione richiamata chiusa in errore
246900951128    >C     ERR084        WHENEQ    '1'
247000951128     C                   MOVE      *ON           $ESCI
247100951128     C                   SETON                                        9599
247200951128     C* 2 = funzione richiamata senza la dovuta autorizzazione
247300951128    >C     ERR084        WHENEQ    '2'
247400951128     C                   MOVE      *ON           $ESCI
247500951128     C                   SETON                                        9499
247600951128     C*
247700951128     C                   ENDSL
247800960105     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
247900960105     C* di sola finestra
248000960105     C                   MOVEL     '0'           wexf084
248100951128     C*
248200951128     C                   ENDSR
248300951205     C*
248400970605     C/EJECT
248401040212C1718C************************************************************
248402040212     C* GESTIONE RITORNO DA PGM cash flow
248403040212C1718C************************************************************
248404040212     C     RTNT68        BEGSR
248405040212     C*
248406040212     C* modo di ritorno
248407040212     C*
248408040212   B1C                   SELECT
248409040212     C* 1 = F3
248410040212     C     RETt68        WHENEQ    '1'
248411040212     C                   MOVE      *ON           $ESCI
248412040212     C                   MOVE      *ON           $FINE
248413040212     C                   MOVE      '1'           RETT11
248414040212     C* 2 = F12
248415040212     C     RETt68        WHENEQ    '2'
248416040212     C                   MOVE      *ON           $ESCI
248417040212     C*
248418040212   E1C                   ENDSL
248419040212     C*
248420040212     C* operazione eseguite dal pgm chiamato
248421040212     C*
248422040212    >C                   MOVE      OPRt68        WOPR
248423040212     C                   SELECT
248424040212     C* 1 = eseguito aggiornamento
248425040212    >C     OPRt68        WHENEQ    '1'
248426040212     C                   move      *ON           winzs1
248427040212     C*
248428040212     C                   ENDSL
248429040212     C*
248430040212     C* funzione non eseguibile per errore :
248431040212     C*
248432040212     C                   SELECT
248433040212     C* 1 = funzione richiamata chiusa in errore
248434040212    >C     ERRt68        WHENEQ    '1'
248435040212     C                   MOVE      *ON           $ESCI
248436040212     C                   SETON                                        9599
248437040212     C* 2 = funzione richiamata senza la dovuta autorizzazione
248438040212    >C     ERRt68        WHENEQ    '2'
248439040212     C                   MOVE      *ON           $ESCI
248440040212     C                   SETON                                        9499
248441040212     C*
248442040212     C                   ENDSL
248443040212     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
248444040212     C* di sola finestra
248445040212     C                   MOVEL     '0'           wexft68
248446040212     C*
248447040212c1718C                   ENDSR
248448051205C1886C************************************************************
248449051205     C* GESTIONE Ritorno da pgm home banking
248450051205C1886C************************************************************
248451051205     C     RTNK40        BEGSR
248452051205     C*
248453051205     C* modo di ritorno
248454051205     C*
248455051205   B1C                   SELECT
248456051205     C* 1 = F3
248457051205     C     RETk40        WHENEQ    '1'
248458051205     C                   MOVE      *ON           $ESCI
248459051205     C                   MOVE      *ON           $FINE
248460051205     C                   MOVE      '1'           RETT11
248461051205     C* 2 = F12
248462051205     C     RETk40        WHENEQ    '2'
248463051205     C                   MOVE      *ON           $ESCI
248464051205     C*
248465051205   E1C                   ENDSL
248466051205     C*
248467051205     C* operazione eseguite dal pgm chiamato
248468051205     C*
248469051205    >C                   MOVE      OPRk40        WOPR
248470051205     C                   SELECT
248471051205     C* 1 = eseguito aggiornamento
248472051205    >C     OPRk40        WHENEQ    '1'
248473051205     C                   move      *ON           winzs1
248474051205     C*
248475051205     C                   ENDSL
248476051205     C*
248477051205     C* funzione non eseguibile per errore :
248478051205     C*
248479051205     C                   SELECT
248480051205     C* 1 = funzione richiamata chiusa in errore
248481051205    >C     ERRk40        WHENEQ    '1'
248482051205     C                   MOVE      *ON           $ESCI
248483051205     C                   SETON                                        9599
248484051205     C* 2 = funzione richiamata senza la dovuta autorizzazione
248485051205    >C     ERRk40        WHENEQ    '2'
248486051205     C                   MOVE      *ON           $ESCI
248487051205     C                   SETON                                        9499
248488051205     C*
248489051205     C                   ENDSL
248493051205     C*
248494051205c1886C                   ENDSR
248500970605     C************************************************************
248600970613     C* GESTIONE RITORNO DA PGM riconcilia
248700970605     C************************************************************
248800970605     C     RTNT14        BEGSR
248900970605     C*
249000970605     C* operazione eseguite dal pgm chiamato
249100970605     C*
249200970605    >C                   MOVE      OPRT14        WOPR
249300970605     C                   SELECT
249400970605     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
249500970605     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
249600970605    >C     OPRt14        WHENEQ    '1'
249700970605     C                   move      *ON           winzs1
249800970605     C*
249900970605     C                   ENDSL
250000970605     C*
250100970605     C* funzione non eseguibile per errore :
250200970605     C*
250300970605     C                   SELECT
250400970605     C* 1 = funzione richiamata chiusa in errore
250500970605    >C     ERRt14        WHENEQ    '1'
250600970605     C                   MOVE      *ON           $ESCI
250700970605     C                   SETON                                        9599
250800970605     C* 2 = funzione richiamata senza la dovuta autorizzazione
250900970605    >C     ERRT14        WHENEQ    '2'
251000970605     C                   MOVE      *ON           $ESCI
251100970605     C                   SETON                                        9499
251200970605     C* 3 = Riconciliazione non possibile
251300970605    >C     ERRT14        WHENEQ    '3'
251400970605     C                   MOVE      *ON           $ESCI
251500970605     C                   SETON                                        8899
251600970605     C*
251700970605     C                   ENDSL
251800970605     C*
251900970605     C                   ENDSR
252000970613     C/EJECT
252100970613     C************************************************************
252200970613     C* GESTIONE RITORNO DA PGM reset riconcilia
252300970613     C************************************************************
252400970613     C     RTNT15        BEGSR
252500970613     C*
252600970613     C* operazione eseguite dal pgm chiamato
252700970613     C*
252800970613    >C                   MOVE      OPRT15        WOPR
252900970613     C                   SELECT
253000970613     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
253100970613     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
253200970613    >C     OPRt15        WHENEQ    '1'
253300970613     C                   move      *ON           winzs1
253400970613     C*
253500970613     C                   ENDSL
253600970613     C*
253700970613     C* funzione non eseguibile per errore :
253800970613     C*
253900970613     C                   SELECT
254000970613     C* 1 = funzione richiamata chiusa in errore
254100970613    >C     ERRt15        WHENEQ    '1'
254200970613     C                   MOVE      *ON           $ESCI
254300970613     C                   SETON                                        9599
254400970613     C* 2 = funzione richiamata senza la dovuta autorizzazione
254500970613    >C     ERRT15        WHENEQ    '2'
254600970613     C                   MOVE      *ON           $ESCI
254700970613     C                   SETON                                        9499
254800970613     C* 3 = Reset riconciliazione non possibile
254900970613    >C     ERRT15        WHENEQ    '3'
255000970613     C                   MOVE      *ON           $ESCI
255100970613     C                   SETON                                        8999
255200970704     C* 4 = De-riconciliazione non riuscita per data blocco
255300970704    >C     ERRT15        WHENEQ    '4'
255400970704     C                   MOVE      *ON           $ESCI
255500970704     C                   SETON                                        8799
255600970613     C*
255700970613     C                   ENDSL
255800970613     C*
255900970613     C                   ENDSR
256000970724     C/EJECT
256100970724     C************************************************************
256200970724     C* GESTIONE RITORNO da roconciliazione
256300970724     C************************************************************
256400970724    >C     RTNT43        BEGSR
256500970724     C*
256600970724     C* modo di ritorno
256700970724     C*
256800970724     C                   SELECT
256900970724     C* questi modi di utilizzo dei valori di ritorno dal
257000970724     C* pgm di manutenzione rcd di anagrafica sono delle
257100970724     C* proposte, normalmente sempre valide, ma modificabili
257200970724     C* per situazioni particolari
257300970724     C* 1 = F3
257400970724    >C     RetT43        WHENEQ    '1'
257500970724     C                   MOVE      *ON           $ESCI                          sospese READC
257600970724     C                   MOVE      *ON           $FINE                          fine pgm
257700970724     C                   MOVE      '1'           RETT11
257800970724     C* 2 = F12
257900970724    >C     RETT43        WHENEQ    '2'
258000970724     C* se il pgm di manutenzione ha prima una guida, è necessario anche
258100970724     c* questo test, altrimenti prevale il retT41 e il sfl non viene
258200970724     C* ricaricato
258300970724     C     OPRT43        ANDNE     '1'
258400970724     C                   MOVE      *ON           $ESCI                          sospese READC
258500970724     C*
258600970724     C                   ENDSL
258700970724     C                   MOVE      '0'           WrkWdwT43
258800970724     C*
258900970724     C                   ENDSR
259000970724     C/EJECT
259100950301     C/EJECT
259200950301     C************************************************************
259300950301     C* AGGIORNAMENTO RIGA LISTA
259400950301     C************************************************************
259500950301     C     AGGS1         BEGSR
259600950301     C*
259700950301     C                   ENDSR
259800941205     C/EJECT
259900941205     C************************************************************
260000941205     C* CONTROLLO ABILITAZIONE TASTI
260100941205     C* INPUT
260200941205     C* $TASTO =Tasto funzione premuto
260300941205     C* $FC = Tasti abilitati
260400941205     C* OUTPUT
260500941205     C* IN99 = ERrore
260600941205     C************************************************************
260700941205     C     CTRKEY        BEGSR
260800941205     C*
260900941205     C                   SELECT
261000941205     C     $TASTO        WHENEQ    F01
261100941205     C     $FC(1)        IFEQ      '0'
261200941205     C                   SETON                                        99
261300941205     C                   ENDIF
261400941205     C     $TASTO        WHENEQ    F02
261500941205     C     $FC(2)        IFEQ      '0'
261600941205     C                   SETON                                        99
261700941205     C                   ENDIF
261800941205     C     $TASTO        WHENEQ    F03
261900941205     C     $FC(3)        IFEQ      '0'
262000941205     C                   SETON                                        99
262100941205     C                   ENDIF
262200941205     C     $TASTO        WHENEQ    F04
262300941205     C     $FC(4)        IFEQ      '0'
262400941205     C                   SETON                                        99
262500941205     C                   ENDIF
262600941205     C     $TASTO        WHENEQ    F05
262700941205     C     $FC(5)        IFEQ      '0'
262800941205     C                   SETON                                        99
262900941205     C                   ENDIF
263000941205     C     $TASTO        WHENEQ    F06
263100941205     C     $FC(6)        IFEQ      '0'
263200941205     C                   SETON                                        99
263300941205     C                   ENDIF
263400941205     C     $TASTO        WHENEQ    F07
263500941205     C     $FC(7)        IFEQ      '0'
263600941205     C                   SETON                                        99
263700941205     C                   ENDIF
263800941205     C     $TASTO        WHENEQ    F08
263900941205     C     $FC(8)        IFEQ      '0'
264000941205     C                   SETON                                        99
264100941205     C                   ENDIF
264200941205     C     $TASTO        WHENEQ    F09
264300941205     C     $FC(09)       IFEQ      '0'
264400941205     C                   SETON                                        99
264500941205     C                   ENDIF
264600941205     C     $TASTO        WHENEQ    F10
264700941205     C     $FC(10)       IFEQ      '0'
264800941205     C                   SETON                                        99
264900941205     C                   ENDIF
265000941205     C     $TASTO        WHENEQ    F11
265100941205     C     $FC(11)       IFEQ      '0'
265200941205     C                   SETON                                        99
265300941205     C                   ENDIF
265400941205     C     $TASTO        WHENEQ    F12
265500941205     C     $FC(12)       IFEQ      '0'
265600941205     C                   SETON                                        99
265700941205     C                   ENDIF
265800941205     C     $TASTO        WHENEQ    F13
265900941205     C     $FC(13)       IFEQ      '0'
266000941205     C                   SETON                                        99
266100941205     C                   ENDIF
266200941205     C     $TASTO        WHENEQ    F14
266300941205     C     $FC(14)       IFEQ      '0'
266400941205     C                   SETON                                        99
266500941205     C                   ENDIF
266600941205     C     $TASTO        WHENEQ    F15
266700941205     C     $FC(15)       IFEQ      '0'
266800941205     C                   SETON                                        99
266900941205     C                   ENDIF
267000941205     C     $TASTO        WHENEQ    F16
267100941205     C     $FC(16)       IFEQ      '0'
267200941205     C                   SETON                                        99
267300941205     C                   ENDIF
267400941205     C     $TASTO        WHENEQ    F17
267500941205     C     $FC(17)       IFEQ      '0'
267600941205     C                   SETON                                        99
267700941205     C                   ENDIF
267800941205     C     $TASTO        WHENEQ    F18
267900941205     C     $FC(18)       IFEQ      '0'
268000941205     C                   SETON                                        99
268100941205     C                   ENDIF
268200941205     C     $TASTO        WHENEQ    F19
268300941205     C     $FC(19)       IFEQ      '0'
268400941205     C                   SETON                                        99
268500941205     C                   ENDIF
268600941205     C     $TASTO        WHENEQ    F20
268700941205     C     $FC(20)       IFEQ      '0'
268800941205     C                   SETON                                        99
268900941205     C                   ENDIF
269000941205     C     $TASTO        WHENEQ    F21
269100941205     C     $FC(21)       IFEQ      '0'
269200941205     C                   SETON                                        99
269300941205     C                   ENDIF
269400941205     C     $TASTO        WHENEQ    F22
269500941205     C     $FC(22)       IFEQ      '0'
269600941205     C                   SETON                                        99
269700941205     C                   ENDIF
269800941205     C     $TASTO        WHENEQ    F23
269900941205     C     $FC(23)       IFEQ      '0'
270000941205     C                   SETON                                        99
270100941205     C                   ENDIF
270200941205     C     $TASTO        WHENEQ    F24
270300941205     C     $FC(24)       IFEQ      '0'
270400941205     C                   SETON                                        99
270500941205     C                   ENDIF
270600941205     C                   ENDSL
270700941205     C*
270800941205     C                   ENDSR
270900940223     C/EJECT
271000940131     C************************************************************
271100940131     C* OPERAZIONI INIZIALI
271200940131     C************************************************************
271300940131     C     *INZSR        BEGSR
271400000117C1309C     *DMY          Move      Udate         ZDateISO
271500000117C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
271600940131     C*
271700940127     C* Reperimento parametri
271800940127     C*
271900940117     C     *ENTRY        PLIST
272000940117     C                   PARM                    KPJBA
272100970416     c                   PARM                    ndcT11ds
272200951109     C                   PARM                    SOC001
272300950410     C                   PARM                    DSAUT
272400950410     C*
272500951110     C     PARMS         IFLT      4
272600950410     C                   MOVE      *BLANK        AUTDS
272700950410     C                   ELSE
272800950410     C                   MOVEL     DSAUT         AUTDS
272900950410     C                   ENDIF
273000951108     C*
273100951108     C* Reperimento autorizzazioni
273200951108     C* le autorizzazioni potrebbero essere già state passate,
273300951108     C* nel qual caso non vanno reperite
273400951108    >C     AUTDS         IFEQ      *BLANK
273500951108    >C                   EXSR      REPAUT
273600960930     c                   else
273700970416     c                   movel     xcadds        conTESds
273800951108     C                   ENDIF
273900940225     C*
274000940223     C* Reperimento opzioni
274100940127     C*
274200940128     C                   EXSR      REPOPZ
274300950406     C*
274400940506     C* Reperimento tasti di funzione
274500940506     C*
274600940506     C                   EXSR      REPKEY
274700951109     C* Udate in iso
274800951109     C                   CALLb     'XDT4000'
274900951109     C     WDATE         PARM                    XDTAMG
275000000117C1309C*****              PARM      UDATE         XDTGMA
275100000117C1309C                   PARM      ZDate         XDTGMA
275200951109     C                   PARM      4             XDTSTA
275300970422     c*
275400970422     C* recupero le costanti per la riga da emettere
275500970422     C                   MOVEL     *BLANK        Ripresa          30
275600970422     C                   MOVEL     RipresaC      IDMSG
275700970422     C                   CALLB     'XRTVM'
275800970422     C                   PARM                    IDMSG            27
275900970422     C                   PARM                    TXTMSG          100
276000970422     C                   PARM                    ERRMSG            1
276100970422     C                   PARM      *Off          CTRMSG            1            centratura
276200970422     C                   PARM      30            LENMSG            3 0          lun output.
276300970422     C     ERRMSG        IFNE      *ON
276400970422     C                   MOVEL     TXTMSG        Ripresa
276500970422     C                   ELSE
276600970422     C                   MOVEL     *ALL'?'       Ripresa
276700970422     C                   ENDIF
276800940127     C*
276900940117     C                   ENDSR
277000970422     c/eject
277100970422     C************************************************************
277200970422     C* REPERIMENTO PARAMETRI GESTIONE
277300970422     C************************************************************
277400970422     C     REPPAR        BEGSR
277500970422     C*
277600970422     C                   CALLB     'XPAR'
277700970422     C                   PARM                    XPASOC            3
277800970422     C                   PARM                    XPAPAR            8
277900970422     C                   PARM                    XPAOUT
278000970422     C                   PARM                    XPAERR            1
278100970422     C                   PARM                    XPARIC            1
278200970422     C*
278300970422     C                   ENDSR
278400940223     C/EJECT
278500940223     C************************************************************
278600940223     C* INIZIALIZZAZIONE VARIABILI
278700940223     C************************************************************
278800940223     C     INZVAR        BEGSR
278900970429     C*
279000970429     C* Reperimento parametri gestione modulo tesoreria
279100970429     C* deve stare nell'inzvar perchè se si cambia società ed il programma
279200970429     c* si chiude in RT, DEVE rifare il controllo
279300970429     C*
279400970429     C                   MOVEL     *BLANK        XPARIC
279500970429     C                   MOVEL     XSCSOC        XPASOC
279600970429     C                   MOVEL     'STRTESO '    XPAPAR
279700970429     C                   MOVEL     *BLANK        XPAOUT
279800970429     C                   EXSR      REPPAR
279900970429     C     XPAERR        IFEQ      '0'
280000970429     C                   MOVEL     XPAOUT        ANP021DS
280100970429     C                   ELSE
280200970429     C                   SETON                                        LR
280300970429     C                   RETURN
280400970429     C                   ENDIF
280500970417     c* reperisco dati conto
280600970422     c                   eval      Wkcc = kcct11
280700970422     c                   eval      Wksc = ksct11
280800970417     c                   exsr      CallMvc002
280900940506     C*
281000940506     C* Definizione delle opzioni utilizzabili a pgm
281100940506     C*
281200940506     C                   EXSR      DEFOPZ
281300950330     C*
281400940223     C* Variabili per gestione videate
281500940223     C*
281600940223     C                   MOVE      *OFF          $FINE
281700940223     C                   MOVE      *OFF          $INZS1
281800950606     C                   MOVE      *OFF          $EndFile
281900980119     C                   MOVE      *OFF          $ESCI
282000940223     C                   MOVE      *OFF          $RCDOK
282100940509     C                   MOVE      *HIVAL        $LASTV
282200940506     C                   Z-ADD     0             $ULKS1            3 0
282300940318     C                   MOVE      *ON           $INZS1
282400951109     C                   MOVE      'S1'          $GEST
282500980130     C                   CLEAR                   WDare
282600980130     C                   CLEAR                   WAvere
282700980130     C                   CLEAR                   WDareD
282800980130     C                   CLEAR                   WAvereD
282900970417     C                   CLEAR                   Wrigsald
283000980119     C* se il conto in esame è valutario (la divisa del rapporto è <> m.c.)
283100980119     C                   MOVE      *off          *in08
283200980130    1C                   IF        DivBRAT11 <> XscDiv
283300980119     C* a seconda della divisa da visualizzare
283400980130    2c                   IF        DivVisT11 = '0'
283500980119     C* mostro i dati di saldo in m.c.
283600980119     C                   MOVE      *off          *in08
283700980130    2c                   ELSE
283800980119     C* oppure quelli nella divisa del c/valutario
283900980119     C                   MOVE      *on           *in08
284000980130    2C                   ENDIF
284100980130    1C                   ENDIF
284200951122     C*
284300951122     C* Valorizzazione campi univoci testate
284400951122     C*
284500970416     C                   CLEAR                   CT11T1
284600951122     C                   MOVEL     KNSIF         NOMSIF
284700951122     C                   MOVEL     XSCDSI        NOMDIT
284800951122     C                   MOVEL     DSPGM         NOMPGM
284900951109     C*
285000951109     C* visualizzo il sfl troncato
285100951109     C                   Eval      c1mode=*on
285200951113     C*
285300970416     C* il sottotitollo è la decodifica della modalita'
285400951113     C                   MOVE      *BLANK        T1tit
285500951113     C                   RESET                   X07DS
285600951113     C                   MOVE      '1'           X07RIC
285700951113     C                   MOVEL     'ANDIZ000'    X07TRC
285800970416     C                   MOVEL     'DSPMODTES'   X07CAM
285900970416     C                   MOVEL     MODALITT11    X07VAL
286000951113     C                   MOVE      *OFF          X07ALL
286100960924     C                   CALLB     'X07VALR'
286200951113     C                   PARM                    X07DS
286300980130   B1C     X07ERR        IFEQ      '0'
286400951113     C                   MOVEL     x07des        t1tit
286500951114     C                   MOVEl     t1tit         £STR            132
286600951114     C                   MOVE      30            £LSTRC            3 0
286700951114      * Routine di centratura
286800951114      *
286900951114     C     ' '           CHECKR    £STR          £LEN              3 0
287000951114     C     £LSTRC        SUB       £LEN          £LEN
287100951114     C                   DIV       2             £LEN
287200951114     C     ' '           CAT(P)    £STR:£LEN     £STRC           132
287300951114     C                   MOVEL     £strc         t1tit
287400980130     C*
287500980130   X1C                   Endif
287600951109     C*
287700980130     C* Reperimento dati moneta di conto e divisa di rapporto
287800951109     C                   Eval      WDivisa =  XscDiv
287900951109     C                   Exsr      RepDivisa
288000980121     C                   Eval      NrDecMC = X01NrDec
288100980130     C* se la divisa del rapporto NON è la m.c.
288200980130    1C                   IF        DivBRAT11 <> XscDiv
288300980130     C* se la divisa del rapporto non è l'ultima reperita
288400980130    2C                   IF        DivBRAT11 <> SavDivisa
288500980130     C* reperisco i dati della divisa
288600980130     C                   EVAL      SavDivisa = DivBRAT11
288700980130     C                   EVAL      WDivisa =  DivBRAT11
288800980130     C                   Exsr      RepDivisa
288900980130     C                   Eval      NrDecDiv = X01NrDec
289000980130    2C                   ENDIF
289100980130    1C                   ELSE
289200980130     C* i dati della divisa del rapporto sono gli stessi della m.c.
289300980130     C                   Eval      NrDecDiv = NrDecMC
289400980130    1C                   ENDIF
289500951108     C*
289600951108     C* Definisco ordinamento lettura
289700951108     C*
289800950410     C                   EXSR      DEFORD
289900980130     C*
290000951113     C* Definisco caricamento subfile
290100980130     C*
290200980130     C* i dati della divisa del saldo sono fissi
290300980130     C                   MOVEL     XscDiv        Z1Divisa
290400980130     C                   MOVEL     DivBRAT11     Z1DivisaD
290500970417     C* Se NON chiesti parametri particolari, imposto caricamento a pagina
290600970417     c* e i saldi li prendo dalla routine
290700970416     C                   if        SelPartT11 = *off
290800951108     C                   EVAL      $carpag = *on
290900951108     C                   EVAL      SFLPAG = 8
291000970422     c* richiamo routine per scrivere saldi totali
291100970422     c                   exsr      CallSaldi
291200951109     C*
291300951108     C                   else
291400951109     C*
291500951108     c                   eval      $carpag = *off
291600951108     C                   EVAL      SFLPAG = 9999
291700980130     C                   CLEAR                   Z1Saldo
291800980130     C                   CLEAR                   Z1Dare
291900980130     C                   CLEAR                   Z1Avere
292000980130     C                   CLEAR                   Z1Segno
292100980130     C                   CLEAR                   Z1SegnoD
292200980130     c                   CLEAR                   z1dareD
292300980130     c                   CLEAR                   z1avereD
292400980130     c                   CLEAR                   z1saldoD
292500980130     c                   CLEAR                   z1segnoD
292600951108     C                   endif
292700970422     c*
292800970422     c                   eval      z1darek = kdare021
292900970422     c                   eval      z1averek= kavere021
293000951108     C*
293100951108     C* Variabili appoggio
293200951108     C*
293300951108     C                   Z-ADD     SFLPAG        WPAG
293400951108     C                   CLEAR                   W1POS
293500951110     C* Salvo divisa in entrata
293600970416     C                   EVAL      SavDivVis =   DivVisT11
293700940223     C*
293800940223     C                   ENDSR
293900970422     C/EJECT
294000970422     C************************************************************
294100970422     C* Richiama routine calcolo saldi
294200970422     C************************************************************
294300970422     C     CallSaldi     BEGSR
294400970422     C*
294500980119     C                   EVAL      NrCicli = 1
294600980119     C* se il conto in gestione è valutario (divisa del rapporto <> m.c.)
294700980119    1C                   IF        DivBRAT11 <> XscDiv
294800980119     C* allora devo ciclare 2 volte per reperire anche i saldi in divisa
294900980119     C                   EVAL      NrCicli = 2
295000980119    1C                   ENDIF
295100980119     C*
295200980119    1C                   DO        NrCicli       x
295300980120     C*
295400980120     C                   EVAL      GiroDivisa = x
295500970422     c                   exsr      RieCt21ds
295600970422     c                   call      'NDCT21R'
295700970422     C                   PARM                    NDCT21DS
295800980121     C*
295900980121     C                   MOVEL     NDCT21DS      AppT21DS(x)
296000980121     C*
296100980121    2C                   if        erroreT21 = *off
296200980119     C*
296300980119     c* Modalità per Data valuta
296400980119    3C                   IF        ModalitT11 = '0'
296500980119     c* ciclo per m.c.
296600980119    4C                   IF        x = 1
296700980130     C                   Z-ADD     SlFiDVaT21    WDare
296800980130     C                   Z-ADD     SlFiAVaT21    WAvere
296900980119     c* ciclo per divisa
297000980119    4c                   ELSE
297100980130     C                   Z-add     SlFiDVaT21    WDareD
297200980130     C                   Z-add     SlFiAVaT21    WAvereD
297300980119    4c                   ENDIF
297400980119     c* Modalità per Data operazione
297500980119    3c                   ELSE
297600980119     c* ciclo per m.c.
297700980119    4C                   IF        x = 1
297800980130     C                   Z-add     SlFiDOpT21    WDare
297900980130     C                   Z-add     SlFiAOpT21    WAvere
298000980119     c* ciclo per divisa
298100980119    4c                   ELSE
298200980130     C                   Z-add     SlFiDOpT21    WDareD
298300980130     C                   Z-add     SlFiAOpT21    WAvereD
298400980119    4c                   ENDIF
298500980119    3c                   ENDIF
298600980130     C*
298700980130     C* predispongo il piede nei campi editabili
298800980130     C*  ciclo m.c.
298900980130     C                   IF        x = 1
299000980130     C                   EVAL      WNrDec = NrDecMC
299100980130     C*  Dare
299200980130     C                   Z-ADD     WDare         WImporto
299300980130     C                   EVAL      $LenFld = %Size(Z1Dare)
299400980130     C                   SUB       5             $LenFld
299500980130     C                   EXSR      Edit
299600980130     C                   Eval      Z1Dare  = $RcvrVar
299700980130     C*  Avere
299800980130     C                   Z-ADD     WAvere        WImporto
299900980130     C                   EVAL      $LenFld = %Size(Z1Avere)
300000980130     C                   EXSR      Edit
300100980130     C                   Eval      Z1Avere = $RcvrVar
300200980130     C*  ciclo divisa del rapporto
300300980130     C                   ELSE
300400980202     C*
300500980202     C* reperisco decimali se divisa diversa dalla precedente letta
300600980202     C                   IF        Z1DivisaD <> SavDivisa
300700980202     C                   EVAL      SavDivisa = Z1DivisaD
300800980202     C                   EVAL      WDivisa = Z1DivisaD
300900980202     C                   EXSR      RepDivisa
301000980202     C                   EVAL      NrDecDiv = X01NrDec
301100980202     C                   ENDIF
301200980130     C                   EVAL      WNrDec = NrDecDiv
301300980130     C*  Dare
301400980130     C                   Z-ADD     WDareD        WImporto
301500980130     C                   EVAL      $LenFld = %Size(Z1DareD)
301600980130     C                   SUB       5             $LenFld
301700980130     C                   EXSR      Edit
301800980130     C                   Eval      Z1DareD  = $RcvrVar
301900980130     C*  Avere
302000980130     C                   Z-ADD     WAvereD       WImporto
302100980130     C                   EVAL      $LenFld = %Size(Z1AvereD)
302200980130     C                   EXSR      Edit
302300980130     C                   Eval      Z1AvereD = $RcvrVar
302400980130     C                   ENDIF
302500980130     C*
302600980119    2C                   endif
302700980119    1C                   ENDDO
302800980119     C*
302900980121     C* calcolo segno e saldo (sia in m.c. che in divisa)
303000980121     C                   EXSR      ClcSegSal
303100980119     C*
303200980121     C* se il conto non è valutario
303300980121     C                   IF        NrCicli = 1
303400980121     C* ricopio i dati di saldo della m.c. anche per la divisa di rapporto
303500980121     C* che in questo caso è la stessa
303600980121     C                   MOVEL     AppT21DS(1)   AppT21DS(2)
303700980121    3c                   ENDIF
303800980121     C*
303900970422     c                   endsr
304000970417     C/EJECT
304100970417     C************************************************************
304200970417     C* Riempie dati della ds calcolo saldi
304300970417     C************************************************************
304400970417     C     RieCt21ds     BEGSR
304500970417     C*
304600970417     c                   reset                   NDCT21DS
304700970417     c                   eval      SocietaT21 = Xscsoc
304800970417     c                   eval      CtbT21     = CtbT11
304900970417     c                   eval      BancaT21   = BancaT11
305000970417     c                   eval      LineaT21   = LineaT11
305100980120     c***                eval      DivisaT21  = XscDiv
305200980120     C* al primo giro reperisco sempre il valore in m.c.
305300980120     C                   IF        GiroDivisa = 1
305400980120     C                   EVAL      DivisaT21  = XscDiv
305500980120     C* al secondo (se il conto è valutario) vengono reperiti i dati in
305600980120     C* divisa di rapporto bancario
305700980120     C                   ELSE
305800980120     C                   EVAL      DivisaT21  = DivBraT11
305900980120     C                   ENDIF
306000970417     c                   eval      DefinT21   = MovDefT11
306100970417     c                   eval      ProvvT21   = MovProvT11
306200970417     c                   eval      GestiT21   = MovGesT11
306300970417     c                   eval      DataDaT21  = DtPerDaT11
306400970417     c                   eval      DataAT21   = DtPerAT11
306500970417     c                   eval      RipresaT21 = RipSaldT11
306600970417     C*
306700970417     c                   endsr
306800951109     C/EJECT
306900951109     C************************************************************
307000951109     C* Reperisco dati divisa
307100951109     C************************************************************
307200951109     C     RepDivisa     BEGSR
307300951109     C*
307400951109     C* Divisa
307500951109     C                   reset                   X01CambiDs
307600951109     C                   MOVE      xscsoc        X01societa
307700951109     C                   MOVEL     WDivisa       X01divisa
307800951109     C                   MOVEL     '1'           X01flgcam
307900951109     C                   MOVE      Wdate         x01dtRifCa
308000951109     C                   CALL      'X01CAMBI'
308100951109     C                   PARM                    X01CambiDs
308200951109     C*
308300951109     C                   ENDSR
308400951109     c/EJECT
308500951109     C************************************************************
308600951109     C* Editazione di un numero
308700951109     C************************************************************
308800951109     C     Edit          Begsr
308900951109      *
309000951109     C                   Select
309100951110     C                   When      WNrDec = 0
309200951110     C                   Z-add     WImporto      $SrcVar
309300951110     C                   When      WNrDec = 1
309400951110     C     WImporto      Mult      10            $SrcVar
309500951110     C                   When      WNrDec = 2
309600951110     C     WImporto      Mult      100           $SrcVar
309700951109     C                   Other
309800951110     C     WImporto      Mult      1000          $SrcVar
309900951109     C                   Endsl
310000951109     C*
310100951110     C                   Eval      $SrcVarDe = WNrDec
310200951109     C*
310300951109     C                   CallB     'X23EDITR'
310400951109     C                   Parm                    $SrcVar
310500951109     C                   Parm                    $SrcVarPr
310600951109     C                   Parm                    $SrcVarDe
310700951109     C                   Parm                    $EdtCode
310800951109     C                   Parm                    $LenFld
310900951109     C                   Parm      *blanks       $RcvrVar
311000951109     C                   Parm                    $Err
311100951109      *
311200951109     C                   ENDSR
311300951109     C*
311400951108     C/EJECT
311500951108     C************************************************************
311600951108     C* DEFINIZIONE DELL'ORDINAMENTO
311700951108     C************************************************************
311800951108     C     DEFORD        BEGSR
311900951108     C*
312000951108     C* definisco l'ordinamento della lista a seconda dei valori
312100951108     C* impostati nel video di parzializzazione
312200951108     C*
312300951108     C                   SELECT
312400970416    >C     ORDT11        WHENEQ    'D'
312500951108     C                   Z-ADD     1             $ORD
312600970416    >C     ORDT11        WHENeq    'A'
312700951108     C                   Z-ADD     2             $ORD
312800951108     C                   ENDSL
312900951108     C*
313000951108     C                   ENDSR
313100941124     C/EJECT
313200941124     C************************************************************
313300941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
313400941124     C************************************************************
313500941124     C     REPAUT        BEGSR
313600941124     C*
313700941124     C                   CLEAR                   AUTDS
313800941124     C*
313900941124     C                   MOVEL     XSCSOC        XCASOC
314000941124     C                   MOVEL     XSCDUN        XCAUNI
314100941124     C                   Z-ADD     0             XCAGRP
314200941124     C                   MOVEL     KNMUS         XCAPRF
314300970416    >C                   MOVEL     'CONTES'      XCAFNC
314400941124    >C                   MOVEL     *ZERO         XCAVFU
314500941124    >C                   MOVE      'CK'          XCATCK
314600941124    >C                   MOVE      '1'           XCANAU
314700941124     C*
314800950522     C                   CALLB     'XCHKAUT'
314900941124     C                   PARM                    AUTDS
315000941124     C*
315100941124     C* errori/non abilitazione
315200941124     C     XCARTN        IFNE      0
315300941124     C                   EXSR      ENDPGM
315400941124     C                   ELSE
315500970416    >C                   MOVEL     XCADDS        CONtesDS
315600941124     C                   ENDIF
315700941124     C*
315800941124     C                   ENDSR
315900940223     C/EJECT
316000940131     C************************************************************
316100940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
316200940131     C************************************************************
316300940225     C     DEFOPZ        BEGSR
316400940131     C*
316500940225     C* Questa routine deve valorizzare la schiera $FL1
316600940225     C* con S o N a seconda della utilizzabilità o meno
316700940225     C* dell'opzione correlata
316800940225     C*
316900950301     C* Inizialmente si considerano tutte le opzioni come valide
317000940317     C     1             DO        $DIMOP        X
317100940225     C                   MOVE      'S'           $FL1(X)
317200941027     C                   ENDDO
317300940715     C*
317400951109     C* Questa routine deve valorizzare la schiera $FC1/$FR1
317500941018     C* con 1 o 0 a seconda della utilizzabilità o meno
317600940715     C* del tasto funzione correlato
317700940715     C*
317800940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
317900940715     C* di compilazione
318000940715     C                   RESET                   $FC1
318100940715     C                   RESET                   $FR1
318200940225     C*
318300940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
318400940225     C* invalido alcune opzioni
318500940225     C                   SELECT
318600940225     C* Se pgm richiamato in ricerca disattivo :
318700970416    >C     OPZT11        WHENEQ    '01'
318800940603     C* F3=Fine
318900940603     C                   Z-ADD     3             X
319000941216     C                   MOVE      '0'           $FC1(X)
319100950116     C*
319200951114     C* Se pgm richiamato in gestione       disattivo :
319300970416    >C     OPZT11        WHENEQ    '02'
319400940225     C* 1=Selezione
319500940225     C                   Z-ADD     1             X
319600940225     C     01            LOOKUP    $OP1(X)                                21
319700940225     C     *IN21         IFEQ      *ON
319800940225     C                   MOVE      'N'           $FL1(X)
319900940225     C                   ENDIF
320000940225     C*
320100950301     C* In tutti gli altri casi, disattivo subito :
320200941228     C                   OTHER
320300941228     C*
320400940225     C                   ENDSL
320500960318     c*
320600960318     C* Se conto non gestito a partite, disattivo opzione 22
320700960318     C* 22=partite
320800970416     C                   if        tpgest002  =   '0'
320900960318     C                   Z-ADD     1             X
321000960318     C     22            LOOKUP    $OP1(X)                                21
321100960318     C     *IN21         IFEQ      *ON
321200960318     C                   MOVE      'N'           $FL1(X)
321300960318     C                   ENDIF
321400960318     C                   endif
321500940225     C*
321600940225     C                   ENDSR
321700940225     C/EJECT
321800940225     C************************************************************
321900940225     C* REPERIMENTO OPZIONI
322000940225     C************************************************************
322100940225     C     REPOPZ        BEGSR
322200940225     C*
322300940314     C     1             DO        $DIMOP        X
322400940128     C*
322500940223     C     $MS1(X)       IFNE      *BLANK
322600940128     C*
322700940223     C                   MOVEL     $MS1(X)       IDMSG
322800950522     C                   CALLB     'XRTVM'
322900940201     C                   PARM                    IDMSG            27
323000940201     C                   PARM                    TXTMSG          100
323100940201     C                   PARM                    ERRMSG            1
323200940128     C*
323300940128     C     ERRMSG        IFNE      *ON
323400940223     C                   MOVEL     TXTMSG        $TX1(X)
323500940128     C                   ELSE
323600940223     C                   MOVEL     *ALL'?'       $TX1(X)
323700941027     C                   ENDIF
323800940128     C*
323900941027     C                   ENDIF
324000940128     C*
324100941027     C                   ENDDO
324200940128     C*
324300940128     C                   ENDSR
324400940506     C/EJECT
324500940506     C************************************************************
324600940506     C* REPERIMENTO TASTI DI FUNZIONE
324700940506     C************************************************************
324800940506     C     REPKEY        BEGSR
324900940506     C                   MOVEA     $FM1          $FM
325000950906     C                   MOVEA     $Fc1          $Fc
325100940506     C                   EXSR      REPKKK
325200940506     C                   MOVEA     $FX           $FX1
325300940506     C*
325400951109     C                   MOVEA     $FM1          $FM
325500951109     C                   MOVEA     $Fc1          $Fc
325600940506     C                   EXSR      REPKKK
325700951109     C                   MOVEA     $FX           $FX1
325800940919     C*
325900940919     C                   ENDSR
326000940506     C/EJECT
326100940506     C************************************************************
326200940506     C* REPERIMENTO TASTI DI FUNZIONE
326300940506     C************************************************************
326400940506     C     REPKKK        BEGSR
326500940506     C*
326600940506     C                   CLEAR                   $FX
326700940506     C     1             DO        24            X
326800940506     C*
326900940506     C     $FM(X)        IFNE      *BLANK
327000950906     C     $Fc(X)        andne     '0'
327100940506     C*
327200940506     C                   MOVEL     $FM(X)        IDMSG
327300950522     C                   CALLB     'XRTVM'
327400940506     C                   PARM                    IDMSG            27
327500940506     C                   PARM                    TXTMSG          100
327600940506     C                   PARM                    ERRMSG            1
327700940506     C*
327800940506     C     ERRMSG        IFNE      *ON
327900940506     C                   MOVEL     TXTMSG        $FX(X)
328000940506     C                   ELSE
328100940506     C                   MOVEL     *ALL' '       $FX(X)
328200941027     C                   ENDIF
328300940506     C*
328400941027     C                   ENDIF
328500940506     C*
328600941027     C                   ENDDO
328700940506     C*
328800940506     C                   ENDSR
328900940223     C/EJECT
329000940128     C************************************************************
329100950606     C* DEFINIZIONE KLIST
329200940128     C************************************************************
329300950606     C     DefKlist      BEGSR
329400940128     C*
329500940223     C* klist
329600940223     C*
329700970416    >C* ndmov01L
329800970416    >C     K03mov01      KLIST
329900970416    >C                   KFLD                    movSYS
330000970416    >C                   KFLD                    movNrasREg
330100970416    >C                   KFLD                    movNrrigam
330200940127     C*
330300970416    >C* ndbmv03l
330400970421    >C     K05bmv03      KLIST
330500970416    >C                   KFLD                    bmvSOCIETA
330600970421    >C                   KFLD                    bmvCTB
330700970416    >C                   KFLD                    bmvbanca
330800970416    >C                   KFLD                    bmvlinea
330900970416    >C                   KFLD                    bmvdtope
331000970421    >C     K04bmv03      KLIST
331100970416    >C                   KFLD                    bmvSOCIETA
331200970421    >C                   KFLD                    bmvCTB
331300970416    >C                   KFLD                    bmvbanca
331400970416    >C                   KFLD                    bmvlinea
331500970416     C*
331600970416    >C* ndbmv04l
331700970421    >C     K05bmv04      KLIST
331800970416    >C                   KFLD                    bmvSOCIETA
331900970421    >C                   KFLD                    bmvCTB
332000970416    >C                   KFLD                    bmvbanca
332100970416    >C                   KFLD                    bmvlinea
332200970416    >C                   KFLD                    bmvdtval
332300970421    >C     K04bmv04      KLIST
332400970421    >C                   KFLD                    bmvSOCIETA
332500970421    >C                   KFLD                    bmvCTB
332600970416    >C                   KFLD                    bmvbanca
332700970416    >C                   KFLD                    bmvlinea
332800951127     C*
332900951127     C* NDDES01L
333000951127     C     K04DES01      KLIST
333100951127     C                   KFLD                    DesSys
333200951127     C                   KFLD                    DesNrAsReg
333300951127     C                   KFLD                    DesTipDes
333400951127     C                   KFLD                    DesNrRigaM
333500951127     C     K03DES01      KLIST
333600951127     C                   KFLD                    DesSys
333700951127     C                   KFLD                    DesNrAsReg
333800951127     C                   KFLD                    DesTipDes
333900951127     C*
334000951127    >C* ndREG01L
334100951127    >C     K02reg01      KLIST
334200951127    >C                   KFLD                    Regsys
334300951127    >C                   KFLD                    regNrasreg
334400950704     C*
334500940117     C                   ENDSR
334600940223     C/EJECT
334601040315C1737c* Aggiunta opzione OPZ0503 25=CASH FLOW
334700941214**
334800940718PROMSG    *LIBL     OPZ0001
334900960318PROMSG    *LIBL     OPZ0108
335000951127PROMSG    *LIBL     OPZ0109
335001040212PROMSG    *LIBL     OPZ0503
335002051205PROMSG    *LIBL     OPZ0562                                      C1886
335100951127PROMSG    *LIBL     OPZ0041
335200951127PROMSG    *LIBL     OPZ0144
335300951127PROMSG    *LIBL     OPZ0145
335400970723PROMSG    *LIBL     OPZ0239
335500970723PROMSG    *LIBL     OPZ0240
335600970723PROMSG    *LIBL     OPZ0229
335700940201**
33580094022501S      <Selezione
33590096031822S      <Partite
33600095112723S      <Tesoreria
33600104021225S      <Cash flow
33600205120530S      <Movimento HB                                           C1886
33610095112741S      <Contabilità
33620095112744S      <Descriz.  testata
33630095112745S      <Descriz.  riga
33640097072350S      <Riconciliazione
33650097072351S      <De-Riconcilia
33660097072352S      <Riconcilia
336700940916** TASTI DI FUNZIONE  S1
336800940916PROMSG    *LIBL     KEY0001  01
336900940916PROMSG    *LIBL     KEY0002  02
337000940916PROMSG    *LIBL     KEY0003  03
337100940916PROMSG    *LIBL     KEY0004  04
337200940916PROMSG    *LIBL     KEY0005  05
337300940916PROMSG    *LIBL     KEY0006  06
337400940916PROMSG    *LIBL     KEY0007  07
337500940916PROMSG    *LIBL     KEY0008  08
337600940916PROMSG    *LIBL     KEY0009  09
337700940916PROMSG    *LIBL     KEY0010  10
337800951109PROMSG    *LIBL     KEY0031  11
337900940916PROMSG    *LIBL     KEY0012  12
338000940916PROMSG    *LIBL     KEY0013  13
338100940916PROMSG    *LIBL     KEY0014  14
338200951109PROMSG    *LIBL     KEY0053  15
338300940916PROMSG    *LIBL     KEY0016  16
338400940916PROMSG    *LIBL     KEY0017  17
338500940916PROMSG    *LIBL     KEY0018  18
338600940916PROMSG    *LIBL     KEY0019  19
338700970515PROMSG    *LIBL     KEY0116  20
338800940916PROMSG    *LIBL     KEY0021  21
338900940916PROMSG    *LIBL     KEY0022  22
339000940916PROMSG    *LIBL     KEY0023  23
339100940916PROMSG    *LIBL     KEY0024  24
339200940916**  ABILITAZIONE S1
3393009409160N  01
3394009409160N  02
3395009409161N  03
3396009409160N  04
3397009409160N  05
3398009409160N  06
3399009409160N  07
3400009511201N  08
3401009511081N  09
3402009507040N  10
3403009511081N  11
3404009409161N  12
3405009409160N  13
3406009409160N  14
3407009511081N  15
3408009409160N  16
3409009511080N  17
3410009511090N  18
3411009409160N  19
3412009705151N  20
3413009409160N  21
3414009409160N  22
3415009509131N  23   Impostare a '1' se si utilizzano opzioni
3416009509131N  24   Lasciare sempre impostata a '1' il valore della riga 24
341700970515**
341800970515PROMSG    *LIBL     COS0266
