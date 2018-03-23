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
003600040212C1718D $DIMOP          C                   CONST(15)
003700950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003800950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
003900950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004000950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
004100941214     D $TX             S             50    DIM(50)
004200941214     D $FL             S              1    DIM(50)
004300940909     D* Schiere generali per la gestione dei tasti di funzione
004400940506     D $FM             S             27    DIM(24)
004500940506     D $FX             S             50    DIM(24)
004600940506     D $FC             S              1    DIM(24)
004700940506     D $FR             S              1    DIM(24)
004800940909     D* Schiere per la gestione dei tasti di funzione
004900951108     D* Video S1
005000940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005100940506     D $FX1            S             50    DIM(24) ALT($FM1)
005200940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005300940506     D $FR1            S              1    DIM(24) ALT($FC1)
005400940302     D*
005500940211     D/EJECT
005600951109     D*-------------
005700951109     D* Interrogazione conti
005800951206     D ANA399DS      E DS                  inz
005900950524     D*-------------
006000940211     D* Passaggio Parametri
006100940211     D KPJBA         E DS
006200940211     D*-------------
006300940225     D* Reperimento Autorizzazioni
006400941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
006500941124     D*-------------
006600950410     D* Passaggio Autorizzazioni
006700950410     D DSAUT           DS           512
006800970422     D*-------------
006900970422     D* Parametri gestione tesoreria
007000970422     D ANP021DS      E DS
007100970422     D*-------------
007200970422     D* DS gestione parametri modulo
007300970422     D XPAOUT          DS          2000
007400950524     D*-------------
007500950524     D* Maschera dati autorizzazioni
007600970416    >D CONTESDS      E DS
007700941209     D*-------------
007800941209     D* Dati di ambiente ottenuti da XSOC
007900941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
008000950906     D    FgO1                        1    overlay(XscFgO)
008100961121     D*-------------
008200961121     D* Chiamata ricorsiva
008300961121    >D X48CHPGMDS    E DS                  INZ
008400951109     D*-------------
008500951109     D* Parametri per PGM X01CAMBI
008600951109     D X01CambiDs    E DS                  inz
008700951109     D*-------------                       INZ
008800951109     D* Routine editazione di un numero
008900951110     D $SrcVar         s             30S 0
009000951110     D $SrcVarPr       s              9B 0 inz(30)
009100951109     D $SrcVarDe       s              9B 0
009200951117     D $EdtCode        s              1    inz('1')
009300951109     D $LenFld         s              3S 0
009400951109     D $RcvrVar        s             50
009500951109     D $Err            s              1
009600940325     D*-------------
009700940325     D* Parametri in ricezione
009800970416    >D NDCT11DS      E DS
009900970417     D*-------------
010000970417     D* Parametri routine saldi
010100980120    >D NDCT21DS      E DS                  inz occurs(2)
010200980121    >D AppT21DS        s                   like(NDCT21DS) dim(2)
010300970515     D*-------------
010400970605     D* Parametri pgm per riconciliazione
010500970605    >D NDCT14DS      E DS                  inz
010600970613     D*-------------
010700970613     D* Parametri pgm per Sriconciliazione
010800970613    >D NDCT15DS      E DS                  inz
010900970710    >D  OPRT15       E                     INZ('0')
011000970710    >D  ERRT15       E                     INZ('0')
011100970724     d*-------------
011200970724     D* Ds per opzione riconciliazione
011300970724     D NdcT43Ds      E DS                  Inz
011400970724    >D  OPRT43       E                     INZ('0')
011500970724    >D  RetT43       E                     INZ('0')
011600970724    >D  ERRT43       E                     INZ('0')
011700970710     D*-------------
011800970710     D* ds per attaccare nr. blocco in errore al messaggio di errore
011900970710     D                 DS
012000970710     D  C1ndc0315              1      8
012100970710     D  Pack0315               1      8P 0
012200970605     D*-------------
012300970515     D* Parametri routine stampa ec        inz
012400970515    >D NDCT31DS      E DS
012500951128     D*-------------
012600951128     D* Parametri del PGM descrizioni aggiuntive
012700951128     D NDC027DS      E DS                  INZ
012800951128     D  RET027       E                     INZ('0')
012900951128     D  OPR027       E                     INZ('0')
013000951128     D  ERR027       E                     INZ('0')
013100951128     D*-------------
013200951128     D* Parametri per gestione p.nota interattiva
013300951128     D NDC001DS      E DS                  INZ
013400951128     D  RET001       E                     INZ('0')
013500951128     D  OPR001       E                     INZ('0')
013600951128     D  ERR001       E                     INZ('0')
013700951205     D  DTREG1001    E                     inz('0001-01-01')
013800951205     D  DTREG2001    E                     inz('0001-01-01')
013900951205     D  DTLGIO1001   E                     inz('0001-01-01')
014000951205     D  DTLGIO2001   E                     inz('0001-01-01')
014100951205     D  DTULMO1001   E                     inz('0001-01-01')
014200951205     D  DTULMO2001   E                     inz('0001-01-01')
014300951128     D  DTDOC001     E                     inz('0001-01-01')
014400951128     D*-------------
014500960318      * Parametri per call pgm visualizzazione partite
014600960318     D NDC073DS      E DS                  INZ
014700960318     D  OPR073       E                     INZ('0')
014800960318     D  RET073       E                     INZ('0')
014900960318     D  ERR073       E                     INZ('0')
015000960318     D  Exf073       E                     INZ('0')
015100951128     D*-------------
015200951128      * Parametri per call diretta finestra tesoreria
015300951128     D NDC084DS      E DS                  INZ
015400951128     D  RET084       E                     INZ('0')
015500951128     D  ERR084       E                     INZ('0')
015600951128     D  Exf084       E                     INZ('0')
015700040212c1718D*-------------
015800040212  "   * Parametri per call CASH FLOW
015900040212  "  D NDCt68DS      E DS                  INZ
016000040212  "  D  RETt68       E                     INZ('0')
016100040212  "  D  errt68       E                     INZ('0')
016200040212c1718D  INZT68       E                     INZ('0')
016300051205c1886D*-------------
016400051205  "   * Parametri per interrogazione movimenti HB
016500051205  "  D NDCK40DS      E DS                  INZ
016600051205  "  D  RETK40       E                     INZ('0')
016700051205  "  D  errK40       E                     INZ('0')
016800051205  "  D  DaTCARDK40   E                     inz('0001-01-01')
016900051205  "  D  DaTCARAK40   E                     inz('0001-01-01')
017000051205  "  D  DOPDAK40     E                     inz('0001-01-01')
017100051205  "  D  DOPAK40      E                     inz('0001-01-01')
017200051205  "  D  DVADAK40     E                     inz('0001-01-01')
017300051205C1886D  DVAAK40      E                     inz('0001-01-01')
017400951127     D*-------------
017500951127     D* Controlli validità legende
017600951127     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
017700951127     D*-------------
017800970416     D* ds campi BOB
017900970416     D ANBOB         E DS                  inz ExtName(ANBOB00f)
018000940211     D*-------------
018100940211     D DSFMT           DS           512
018200940506     D  $TASTO               369    369
018300940211     D  NRG                  370    370
018400940211     D  NCL                  371    371
018500940211     D  SFLNRR               378    379B 0
018600940211     D*-------------
018700940211     D* posizione cursore
018800940211     D CURSOR          DS
018900940211     D  RIRI                   1      2B 0 INZ
019000940211     D  R$$                    2      2
019100940211     D  COCO                   3      4B 0 INZ
019200940211     D  C$$                    4      4
019300940127     D*-------------
019400940127     D* Reperimento nome PGM
019500940127     D STATUS         SDS           333
019600940127     D  DSPGM            *PROC
019700950411     D  PARMS            *PARMS
019800951127     D*-------------
019900951127     D* Definizioni parametri standard chiamate moduli
020000951127     D Esito           S              1
020100951127     D gesmsg          S              1
020200951127     D lenout          S              9B 0
020300951127     D Data            S             10
020400951127     D strutturaO      S             10
020500951127     D Operazione      S              2
020600951127     D LenIn           S              9B 0
020700951127     D refresh         S              1
020800951127     D uso             S              1
020900951127     DOutput           S           4000
021000970417     D dtope           S               D
021100950704     D*-------------
021200950704     D* Attributi video
021300950704     D* (definisco come costanti qugli attributi video che potrei
021400950704     D*  testare e quindi preferisco assegnargli un valore "parlante")
021500950704     D*  bottone presente
021600950704     D PRESENTE        C                   CONST(X'A1')
021700940616     D*-------------
021800940506     D* Tasti di funzione
021900940506     D F01             C                   CONST(X'31')
022000940506     D F02             C                   CONST(X'32')
022100940506     D F03             C                   CONST(X'33')
022200940506     D F04             C                   CONST(X'34')
022300940506     D F05             C                   CONST(X'35')
022400940506     D F06             C                   CONST(X'36')
022500940506     D F07             C                   CONST(X'37')
022600940506     D F08             C                   CONST(X'38')
022700940506     D F09             C                   CONST(X'39')
022800940506     D F10             C                   CONST(X'3A')
022900940506     D F11             C                   CONST(X'3B')
023000940506     D F12             C                   CONST(X'3C')
023100940506     D F13             C                   CONST(X'B1')
023200940506     D F14             C                   CONST(X'B2')
023300940506     D F15             C                   CONST(X'B3')
023400940506     D F16             C                   CONST(X'B4')
023500940506     D F17             C                   CONST(X'B5')
023600940506     D F18             C                   CONST(X'B6')
023700940506     D F19             C                   CONST(X'B7')
023800940506     D F20             C                   CONST(X'B8')
023900940506     D F21             C                   CONST(X'B9')
024000940506     D F22             C                   CONST(X'BA')
024100940506     D F23             C                   CONST(X'BB')
024200940506     D F24             C                   CONST(X'BC')
024300940506     D ENTER           C                   CONST(X'F1')
024400940506     D ROLDWN          C                   CONST(X'F4')
024500940506     D ROLLUP          C                   CONST(X'F5')
024600951109     D*-------------
024700951109     D DATMIN          C                   CONST('0001-01-01')
024800951109     D DATMAX          C                   CONST('9999-12-31')
024900950524     D*-------------
025000950606     D* Variabili per gestione videate
025100950606     D $Gest           S              2A
025200950606     D $Fine           S              1A
025300950606     D $InzD1          S              1A
025400950606     D $InzS1          S              1A
025500950606     D $LastG          S              2A
025600950606     D $LastV          S              2A
025700950606     D $EndFile        S              1A
025800950606     D $Esci           S              1A
025900950606     D $RcdOK          S              1A
026000951108     D $carpag         S              1A
026100951108     D $ord            S              1S 0
026200960105     D sflpag          S              8S 0
026300950606     D*-------------
026400950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
026500950606     D S1Nrr           S                   like(C1Rcd)
026600951109     D SFLNRRSAV       S                   like(SFLNRR)
026700950606     D WSfl            S                   like(C1Nrr)
026800950606     D WMax            S                   like(C1Rcd)
026900950606     D WPag            S                   like(C1Rcd)
027000950606     D WInzS1          S                   like($InzS1)
027100970416    >D WOpr            S                   like(OprT11)
027200950606     D CurR            S              2S 0
027300950606     D CurC            S              2S 0
027400950606     D Win             S             99A
027500950606     D W1Pos           S                   like(C1Pos)
027600951109     D Wdate           S             10
027700951109    >D WDivisa         S                   like(X01Divisa)
027800970422    >D Wkcc            S                   like(kccT11)
027900970422    >D Wksc            S                   like(kscT11)
028000951109    >D SavDivisa       S                   like(X01Divisa)
028100970416    >D SavDivVis       S                   like(divvisT11)
028200980121    >D NrDecMC         S              3S 0
028300980121    >D NrDecDiv        S              3S 0
028400951110    >D WNrDec          S              3S 0
028500951110     D Wsegno          S              1A
028600951128     D WRIGsald        S              1A
028700970416    >D WImporto        S             19  3
028800980130    >D WDare           S             19  3
028900980130    >D WAvere          S             19  3
029000980130    >D WDareD          S             19  3
029100980130    >D WAvereD         S             19  3
029200970416    >D Wcausale        S                   like(causal1T11)
029300970416    >D SavCausRig      S                   like(causal1T11)
029400970422    >D SavNd002ds      S                   like(nd002ds)
029500960318    >D Winz073         S                   like(exf073)
029600960105    >D Wexf084         S                   like(exf084)
029700040212C1718D WexfT68         S                   like(INZT68)
029800980119    >D NrCicli         S              3s 0
029900980120    >D GiroDivisa      S              3s 0
030000970422     D*------------
030100970422     d* Constante ripresa saldo
030200970422     D RipresaC        C                   CONST('PROMSG    *LIBL-
030300970422     D                                          COS0261')
030400950606     D*-------------
030500950606     D*  Indici
030600950606     D X               S              3S 0
030700950606     D Y               S              3S 0
030800950606     D*-------------
030900970416     D* Controllo conti
031000970416     D ND002DS       E DS                  inz
031100970515     d* titolo stampa e/c
031200970515     D $TIT            S             27    DIM(1) CTDATA PERRCD(1)              Titolo
031300000117C1309D ZDateISO        S               D
031400051206C1886D DataIso         S               D
031500051206C1886D DataIso1        S               D
031600940127     C*----------------------------------------------------*
031700940127     C*                MAIN LINE PROGRAM
031800940127     C*----------------------------------------------------*
031900940127     C*
032000940223     C* inizializzazione variabili
032100940223     C                   EXSR      INZVAR
032200940223     C*
032300940223     C     $FINE         DOWEQ     *OFF
032400940131     C     $GEST         CASEQ     'S1'          GESS1
032500941027     C                   ENDCS
032600941027     C                   ENDDO
032700940325     C* fine programma
032800940325     C                   EXSR      ENDPGM
032900940225     C*
033000940325     C/EJECT
033100940325     C************************************************************
033200940325     C* FINE PROGRAMMA
033300940325     C************************************************************
033400940325     C     ENDPGM        BEGSR
033500951122     c* Per far riemettere la testata
033600951122     C                   MOVE      *HIVAL        $LASTV
033700940325     C*
033800940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
033900970416    >C     OPZT11        IFEQ      *BLANK
034000940325     C                   SETON                                            LR
034100940325     C* altrimenti chiudo in RT
034200940325     C                   ELSE
034300940325     C                   SETON                                            RT
034400940325     C                   ENDIF
034500940616     C                   RETURN
034600940325     C*
034700940325     C                   ENDSR
034800940224     C/EJECT
034900940201     C************************************************************
035000940201     C* RICERCHE
035100940201     C************************************************************
035200940127     C     SEARCH        BEGSR
035300940127     C*
035400940127     C* determino Riga/Colonna del cursore
035500940127     C                   MOVE      NRG           R$$
035600940127     C                   MOVE      NCL           C$$
035700940127     C                   Z-ADD     RIRI          CURR
035800940127     C                   Z-ADD     COCO          CURC
035900950704     C*
036000941118   B1C                   SELECT
036100940127     C*
036200950703    >C*--------------------------------------------------------------
036300950703    >C* inserire le routine di ricerca
036400950703    >C*--------------------------------------------------------------
036500951108    >C* FMTD1 -
036600951108    >C     1             WHENEQ    1
036700940223     C*
036800940223     C* ricerca non abilitata in questa posizione
036900941118   X1C                   OTHER
037000940929     C                   SETON                                        9899
037100940127     C*
037200941118   E1C                   ENDSL
037300950612     C*
037400950612     C* imposto pos. del cursore
037500950612     C                   Z-ADD     CURR          H1RIGA
037600950612     C                   Z-ADD     CURC          H1COLO
037700940127     C*
037800940506     C                   MOVE      *HIVAL        $LASTV
037900950612     C*
038000940127     C                   ENDSR
038100940131     C/EJECT
038200940224     C************************************************************
038300940131     C* RIEMPIMENTO OPZIONI
038400940224     C************************************************************
038500940131     C     RIEOPZ        BEGSR
038600940223     C*
038700940223     C* Input :
038800940223     C* - $TX = schiera con tutti i testi disponibili
038900940223     C* - $FL = schiere con i flag di validità associati ai testi
039000940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
039100940223     C*          opzione usato
039200940223     C* - WMXO = variabile contenente il numero massimo di testi
039300940223     C*          opzione usabili
039400940721     C* - $L01 = Lunghezza prima riga
039500940721     C* - $L02 = Lunghezza seconda riga
039600940223     C* Output :
039700940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
039800940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
039900941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
040000940223     C*
040100950522     C                   CALLB     'X04OPMSG'
040200940223     C* input
040300940223     C                   PARM                    $TX
040400940223     C                   PARM                    $FL
040500940225     C                   PARM                    WCOZ              3 0
040600940223     C                   PARM                    WMXO              3 0
040700940223     C* output
040800940223     C                   PARM                    WOPZ1            76
040900940223     C                   PARM                    WOPZ2            76
041000941214     C                   PARM                    FF23              1
041100950301     C* input
041200940721    >C                   PARM                    $L01              3 0
041300951108    >C                   PARM                    $L02              3 0
041400940223     C*
041500060224C1913C                   PARM                    $ALLOPT         960
041600060224C1913C*
041700940131     C                   ENDSR
041800940506     C/EJECT
041900940506     C************************************************************
042000940506     C* RIEMPIMENTO TASTI FUNZIONE
042100940506     C************************************************************
042200940506     C     RIEKEY        BEGSR
042300940506     C*
042400940506     C* Input :
042500940506     C* - $FX = schiera con tutti i testi disponibili
042600940506     C* - $FC = schiere con i flag di validità associati ai testi
042700940506     C* - $FR = schiere con i flag di REVERSE IMAGE
042800940506     C* - $ULK = ultimo testo caricato
042900940721     C* - $L01 = Lunghezza prima riga
043000940721     C* - $L02 = Lunghezza seconda riga
043100940506     C* Output :
043200940506     C* - $ULK
043300940609     C* - $KEY1= 1° riga di tasti funzione
043400940609     C* - $KEY2= 2° riga di tasti funzione
043500940506     C*
043600950522     C                   CALLB     'XKEYMSG'
043700940506     C* input
043800940506     C                   PARM                    $FX
043900940506     C                   PARM                    $FC
044000940506     C                   PARM                    $FR
044100940506     C                   PARM                    $ULK              3 0
044200940506     C* output
044300940506     C                   PARM                    $KEY1            79
044400940506     C                   PARM                    $KEY2            79
044500950301     C* input
044600940721    >C                   PARM                    $L01              3 0
044700940721    >C                   PARM                    $L02              3 0
044800940506     C*
044900060224C1913C                   PARM                    $ALLFUNCT       480
045000060224C1913C*
045100940506     C                   ENDSR
045200940224     C/EJECT
045300940224     C************************************************************
045400940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
045500940224     C************************************************************
045600940224     C     GESPIE        BEGSR
045700940224     C*
045800940224     C                   SELECT
045900940506     C* Lista
046000940506     C     $GEST         WHENEQ    'S1'
046100940506     C     $ULKS1        IFEQ      0
046200951110     C*
046300951127     C* totali
046400951110     C* da fare solo una volta poichè si fa quando il caricamento pagina
046500951113     C* non è presente ( se c'era il caricamento a pagina, avevo già reperit
046600951113     C* o questi dati nella routine INZVAR)
046700951110     C                   IF        $carpag = *off
046800980130     C*
046900980130     C* predispongo il piede nei campi editabili
047000980130     C* dati in m.c.
047100980202     C*
047200980202     C* reperisco decimali se divisa diversa dalla precedente letta
047300980202     C                   IF        Z1DivisaD <> SavDivisa
047400980202     C                   EVAL      SavDivisa = Z1DivisaD
047500980202     C                   EVAL      WDivisa = Z1DivisaD
047600980202     C                   EXSR      RepDivisa
047700980202     C                   EVAL      NrDecDiv = X01NrDec
047800980202     C                   ENDIF
047900010522D0935C*
048000010522D0935C                   Eval      WNrDec      = NrDecMC
048100980130     C*  Dare
048200980130     C                   Z-ADD     WDare         WImporto
048300980130     C                   EVAL      $LenFld = %Size(Z1Dare)
048400980130     C                   SUB       5             $LenFld
048500980130     C                   EXSR      Edit
048600980130     C                   Eval      Z1Dare  = $RcvrVar
048700980130     C*  Avere
048800980130     C                   Z-ADD     WAvere        WImporto
048900980130     C                   EVAL      $LenFld = %Size(Z1Avere)
049000980130     C                   EXSR      Edit
049100980130     C                   Eval      Z1Avere = $RcvrVar
049200980130     C* dati in divisa
049300980130     C                   EVAL      WNrDec = NrDecDiv
049400980130     C*  Dare
049500980130     C                   Z-ADD     WDareD        WImporto
049600980130     C                   EVAL      $LenFld = %Size(Z1DareD)
049700980130     C                   SUB       5             $LenFld
049800980130     C                   EXSR      Edit
049900980130     C                   Eval      Z1DareD  = $RcvrVar
050000980130     C*  Avere
050100980130     C                   Z-ADD     WAvereD       WImporto
050200980130     C                   EVAL      $LenFld = %Size(Z1AvereD)
050300980130     C                   EXSR      Edit
050400980130     C                   Eval      Z1AvereD = $RcvrVar
050500980130     C*
050600980121     C* calcolo segno e saldo
050700980119     C* (anche per i valori in divisa del c/valutario)
050800980121     C                   exsr      ClcSegSal
050900961128     C*
051000970422     C                   eval      z1darek = Kdare021
051100970422     C                   eval      z1averek= Kavere021
051200951110     C                   endif
051300951127     C*
051400940506     C                   EXSR      F24S1
051500940506     C                   ELSE
051600970416     C                   WRITE     CT11Z1
051700940506     C                   ENDIF
051800940506     C                   ENDSL
051900940506     C*
052000940224     C                   ENDSR
052100951110     C/EJECT
052200951110     C************************************************************
052300980121     C* Calcolo segno e saldo
052400951110     C************************************************************
052500980121     C     ClcSegSal     BEGSR
052600980119     C*
052700980130     C                   EVAL      $LenFld = %Size(Z1Saldo)
052800980130     C                   EVAL      WNrDec = NrDecMC
052900980130     C*
053000980119    1c                   SELECT
053100980130     C*
053200980130     C                   When      WDare  > WAvere
053300980130     C                   EVAL      Z1Segno = Dare021
053400980130     C     WDare         SUB       WAvere        WImporto
053500980130     C                   SUB       5             $LenFld
053600980130     C                   EXSR      Edit
053700980130     C                   EVAL      Z1Saldo = $RcvrVar
053800980130     C*
053900980130     C                   when      WDare  < WAvere
054000980130     C                   eval      Z1segno = Avere021
054100980130     C     WAvere        sub       WDare         WImporto
054200980130     C                   EXSR      Edit
054300980130     C                   EVAL      Z1Saldo = $RcvrVar
054400980130     C*
054500980130     C                   when      WDare  = Wavere
054600980130     C                   eval      Z1Segno = ' '
054700980130     c                   EVAL      WImporto = 0
054800980130     C                   SUB       5             $LenFld
054900980130     C                   EXSR      Edit
055000980130     C                   EVAL      Z1Saldo = $RcvrVar
055100980119    1C                   ENDSL
055200980130     C*
055300980130     C                   EVAL      $LenFld = %Size(Z1SaldoD)
055400980130     C                   EVAL      WNrDec = NrDecDiv
055500980119     C*
055600980119    1c                   SELECT
055700980130     C*
055800980130     C                   When      WDareD > WavereD
055900980130     C                   eval      Z1SegnoD = Dare021
056000980130     C     WDareD        sub       WAvereD       WImporto
056100980130     C                   SUB       5             $LenFld
056200980130     C                   EXSR      Edit
056300980130     C                   EVAL      Z1SaldoD = $RcvrVar
056400980130     C*
056500980130     C                   when      WDareD < WAvereD
056600980130     C     WAvereD       sub       WDareD        WImporto
056700980119     C                   eval      Z1SegnoD = Avere021
056800980130     C                   EXSR      Edit
056900980130     C                   EVAL      Z1SaldoD = $RcvrVar
057000980130     C*
057100980130     C                   when      WDareD = WAvereD
057200980119     C                   eval      Z1SegnoD = ' '
057300980130     c                   EVAL      WImporto = 0
057400980130     C                   SUB       5             $LenFld
057500980130     C                   EXSR      Edit
057600980130     C                   EVAL      Z1SaldoD = $RcvrVar
057700980119    1C                   ENDSL
057800951113     C*
057900951113     C                   ENDSR
058000951110     C*
058100940224     C/EJECT
058200940127     C************************************************************
058300940131     C* GESTIONE LISTA
058400940127     C************************************************************
058500940127     C     GESS1         BEGSR
058600940223     C* inizializzazione videata
058700940223     C     $INZS1        IFEQ      *ON
058800940127     C                   EXSR      INZS1
058900940223     C                   MOVE      *OFF          $INZS1
059000940127     C                   ENDIF
059100940223     C* emissione piede videata se proveniente da altra
059200940202     C     $LASTV        IFNE      'S1'
059300980119     C* oppure se premuto F11=Cambio moneta
059400980119     C     $LASTV        OREQ      'S1'
059500970416     C                   WRITE     CT11T1
059600940224     C                   EXSR      GESPIE
059700980119     C* salvataggio valore formato di provenienza
059800940202     C                   MOVE      $LASTV        $LASTG
059900940202     C                   MOVE      'S1'          $LASTV
060000941027     C                   ENDIF
060100960325     C* non ci sono record
060200940223     C     WMAX          IFEQ      0
060300970416     C                   WRITE     CT11D1
060400940211     C                   ENDIF
060500960325     C* ce ne sono più di 9999
060600960325     C     S1NRR         IFEQ      9999
060700960325     C                   SETOFF                                       30
060800960325     c                   clear                   z1dare
060900960325     c                   clear                   z1avere
061000960325     c                   clear                   z1saldo
061100960325     c                   clear                   z1segno
061200980119     c                   clear                   z1dareD
061300980119     c                   clear                   z1avereD
061400980119     c                   clear                   z1saldoD
061500980119     c                   clear                   z1segnoD
061600970422     C                   clear                   z1darek
061700970422     C                   clear                   z1averek
061800970416     C                   WRITE     CT11D2
061900960325     C                   ENDIF
062000951109     C                   Eval      *in09=c1mode
062100970724     C                   MOVE      '1'           WrkWdwT43         1
062200940127     C*
062300970416     C                   EXFMT     CT11C1
062400960105     C* determino Riga/Colonna del cursore
062500960105     C                   MOVE      NRG           R$$
062600960105     C                   MOVE      NCL           C$$
062700960105     C                   Z-ADD     RIRI          CURR
062800960105     C                   Z-ADD     COCO          CURC
062900940204     C     C1NRR         IFNE      0
063000940204     C                   Z-ADD     C1NRR         WSFL
063100940204     C                   ENDIF
063200951109     C*
063300951109     C     SFLNRR        IFNE      0
063400951109     C                   Z-ADD     SFLNRR        C1RCD
063500951109     C                   Z-ADD     SFLNRR        SFLNRRSAV
063600951109     C                   ELSE
063700951109     C                   Z-ADD     SFLNRRSAV     C1RCD
063800951109     C                   ENDIF
063900960104     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
064000960318     C                   MOVE      '1'           Winz073
064100960105     C                   MOVE      '1'           Wexf084
064200040212C1718C                   MOVE      '1'           WexfT68
064300940506     C*
064400940506     C* controllo abilitazione tasti funzione
064500940506     C                   EXSR      CTRKS1
064600941205     C*
064700940506     C     *IN99         IFEQ      *OFF
064800940127     C*
064900941027     C                   SELECT
065000940127     C* F3=Fine
065100940506     C     $TASTO        WHENEQ    F03
065200940309     C                   EXSR      F03S1
065300951120     C* F8=Selezione seguente
065400951120     C     $TASTO        WHENEQ    F08
065500951120     C                   EXSR      F08S1
065600951108     C* F11=Cambio moneta
065700951108     C     $TASTO        WHENEQ    F11
065800951108     C                   EXSR      F11S1
065900940131     C* F12=Ritorno
066000940506     C     $TASTO        WHENEQ    F12
066100940309     C                   EXSR      F12S1
066200951108     C* F15=Interrogazioni
066300951108     C     $TASTO        WHENEQ    F15
066400951108     C                   EXSR      F15S1
066500970515     C* F20=Stampa e/c
066600970515     C     $TASTO        WHENEQ    F20
066700970515     C                   EXSR      F20S1
066800940224     C* F23=Altre opzioni
066900940506     C     $TASTO        WHENEQ    F23
067000940309     C                   EXSR      F23S1
067100940224     C* F24=Altri tasti
067200940506     C     $TASTO        WHENEQ    F24
067300940309     C                   EXSR      F24S1
067400951108     C* ROLL-UP (se gestito a pagina)
067500940506     C     $TASTO        WHENEQ    ROLLUP
067600951108     C     $CARPAG       ANDEQ     *ON
067700940127     C                   EXSR      ROLS1
067800940127     C*
067900941027     C                   OTHER
068000940127     C* CONTROLLO DATI
068100940131     C                   EXSR      CTRC1
068200940201     C     *IN99         IFEQ      *OFF
068300940131     C                   EXSR      CTRS1
068400941027     C                   ENDIF
068500941205     C*
068600941027     C                   ENDSL
068700940127     C*
068800940506     C                   ENDIF
068900941205     C*
069000940127     C                   ENDSR
069100940224     C/EJECT
069200940127     C************************************************************
069300940131     C* INIZIALIZZAZIONE LISTA
069400940127     C************************************************************
069500940127     C     INZS1         BEGSR
069600940302     C* pulizia SFL
069700940128     C                   SETOFF                                         3031
069800970416     C                   WRITE     CT11C1
069900940128     C                   SETON                                          31
070000940127     C*
070100940128     C* riempimento opzioni
070200940223     C                   MOVEA     $TX1          $TX
070300940223     C                   MOVEA     $FL1          $FL
070400940309     C                   Z-ADD     0             WCOZ
070500940225     C*  equivale al num.di elementi della schiera $TX1
070600940509     C                   Z-ADD     $DIMOP        WMXO
070700941220    >C                   Z-ADD     76            $L01
070800941220    >C                   Z-ADD     76            $L02
070900940128     C                   EXSR      RIEOPZ
071000941214     C                   Z-ADD     23            X
071100951109     C                   MOVE      FF23          $FC1(X)
071200940608     C                   MOVEL     WOPZ1         C1OPZ1
071300060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
071400951108     C                   MOVEL     WOPZ2         C1OPZ2
071500940225     C                   Z-ADD     WCOZ          C1COZ
071600951108     C* imposta control
071700970416     C                   EVAL      C1KCC = kccT11
071800970416     C                   EVAL      C1KsC = kscT11
071900970416     C                   EVAL      C1Descr = DESCR002
072000970423     C                   EVAL      C1Banca = BancaT11
072100970423     C                   EVAL      C1Linea = LineaT11
072200970422     C                   CALLb     'XDT4000'
072300970422     C                   PARM      DtChiuT11     XDTAMG
072400970422     C     C1DtChiu      PARM                    XDTGMA
072500970422     C                   PARM      2             XDTSTA
072600940128     C*
072700951108     C* CARICAMENTO SFL A PAGINE o intero
072800940128     C*
072900940201     C                   Z-ADD     0             S1NRR
073000940128     C                   Z-ADD     0             WMAX
073100970910     C                   CLEAR                   WrigSald
073200940224     C*
073300940224     C* Posizionamento su file pilota
073400940302     C                   CLEAR                   C1POS
073500941214     C                   EXSR      SETANA
073600940302     C* ripristino i valori del posizionamento
073700940302     C                   MOVE      W1POS         C1POS
073800940131     C*
073900940201     C* Lettura file pilota
074000941214     C                   EXSR      REDANA
074100940131     C*
074200951108     C* Carico 1a pagina o tutto
074300940127     C                   EXSR      ROLS1
074400940201     C                   Z-ADD     SFLPAG        WPAG
074500940127     C*
074600940127     C                   ENDSR
074700970416     C/EJECT
074800970416     C*----------------------------------------------------------
074900970416     C* Chiamata NDMVC002
075000970416     C*----------------------------------------------------------
075100970416     C     CallMvc002    BEGSR
075200970416     C*
075300970416     C* Controllo conto
075400970416     C                   Clear                   ND002DS
075500970416     C                   EVAL      LenOut = %Size(ND002DS)
075600970416     C                   eval      Data = Datmin
075700970416     C                   CallB     'NDMVC002'
075800970416     C                   PARM                    xscsoc
075900970422     C                   PARM                    Wkcc
076000970422     C                   PARM                    Wksc
076100970416     C                   PARM                    Data
076200970416     C                   PARM      *OFF          GesMsg
076300970416     C                   PARM      *OFF          Esito
076400970416     C                   PARM      'ND002DS'     StrutturaO
076500970416     C                   PARM                    ND002DS
076600970416     C                   PARM                    LenOut
076700970416     C*
076800970416     C                   ENDSR
076900940127     C/EJECT
077000940127     C************************************************************
077100940131     C* CARICAMENTO PAGINA LISTA
077200940127     C************************************************************
077300940127     C     ROLS1         BEGSR
077400940127     C*
077500940128     C                   SETOFF                                       32
077600940223     C                   Z-ADD     0             Y
077700940127     C                   Z-ADD     WMAX          S1NRR
077800951128     C*
077900951128     C* se richiesto ripresa saldo scrivo la prima riga col saldo
078000970416     C                   if        ripsaldT11 = *on and Wrigsald <> *on
078100980121     C                   EXSR      RieRipSald
078200951128     C                   eval      wrigsald = *on
078300951128     C                   endif
078400940127     C*
078500940127     C* Leggo dal file anagrafico per caricare la lista
078600940127     C*
078700950606     C     $EndFile      DOWEQ     *OFF
078800940201     C     Y             ANDLT     WPAG
078900940127     C*
079000940131     C                   EXSR      RIES1
079100940127     C*
079200960325     C*  se s1nrr = 9999, che è il massimo comsentito, mi fermo e do
079300960325     c* messaggio di errore
079400960325     c                   if        s1nrr < 9999
079500960325     c*
079600940127     C                   ADD       1             S1NRR
079700940127     C                   ADD       1             Y
079800940127     C*
079900970416     C                   WRITE     CT11S1
080000940131     C*
080100941214     C                   EXSR      REDANA
080200960325     c*
080300960325     C                   else
080400960325     c                   eval      $endfile = *on
080500960325     c                   endif
080600940128     C*
080700941027     C                   ENDDO
080800940127     C*
080900940223     C                   Z-ADD     S1NRR         WMAX                 30
081000940127     C*
081100940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
081200940127     C*
081300940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
081400940127     C                   MVR                     RESTO             3 0
081500940131     C     PAGINE        MULT      SFLPAG        C1RCD
081600941027     C     RESTO         IFGT      0
081700940127     C                   ADD       1             C1RCD
081800941027     C                   ELSE
081900940407     C                   SUB       SFLPAG        C1RCD
082000940407     C                   ADD       1             C1RCD
082100941027     C                   ENDIF
082200940128     C*
082300940127     C                   ENDSR
082400951128     C/EJECT
082500951128     C************************************************************
082600951128     C* riempie riga saldo
082700951128     C************************************************************
082800980121     C     RieRipSald    BEGSR
082900951128     C*
083000980121     C* IMPOSTAZIONI FISSE SIA PER LA VISUALIZZAZIONE DEI DATI IN M.C. CHE
083100980121     C* PER QUELLA DEI DATI IN DIVISA
083200980121     C*
083300970416     C                   CLEAR                   CT11S1
083400951128     C                   MOVEL     *ZERO         desaggh
083500951128     C                   MOVEL     *on           in41h
083600980121     C                   EVAL      h1Divisa   = DivBRAT11
083700980121     C                   EVAL      h1Monconto = xscdiv
083800980121     C*
083900980121     C* reperisco decimali se divisa diversa dalla precedente letta
084000980121     C                   IF        H1Divisa <> SavDivisa
084100980121     C                   EVAL      SavDivisa = H1Divisa
084200980121     C                   EVAL      WDivisa = H1Divisa
084300980121     C                   EXSR      RepDivisa
084400980121     C                   EVAL      NrDecDiv = X01NrDec
084500980121     C                   ENDIF
084600951128     C*
084700980121     C* CICLO 2 VOLTE PER IMPOSTARE SUL SFL IL SALDO SIA IN M.C. CHE IN
084800980121     C* DIVISA (se il rapporto bancario non è un c/valutario, il 2o giro
084900980121     C*  serve solo per allineare il dato in modo da essere leggibile
085000980121     C*  assieme a divise diverse)
085100980121     C*
085200980121    1C                   DO        2             x
085300980121     C*
085400980121     C* stabilisco la lunghezza massima del campo alfanumerico che può
085500980121     C* contenere l'importo editato (i campi di riga 1 e riga 2 devono
085600980121     C*  essere delle stesse dimensioni)
085700980121     C                   Eval      $LenFld = %Size(S1Saldo1)
085800980121     C*
085900980121     C* recupero i dati dei saldi dalla schiera di appoggio
086000980121     C                   MOVEL     AppT21DS(x)   NDCT21Ds
086100980121     C*
086200980121     C* modalità data valuta
086300980121     C                   if        ModalitT11 = '0'
086400980121     C                   select
086500980121     C                   when      SlInDVaT21 > SlInAVaT21
086600970417     C                   EVAL      wimporto =  SlInDVaT21 - SlInAVaT21
086700980121     C                   EVAL      WSegno = Dare021
086800980121     C                   when      SlInDVaT21 < SlInAVat21
086900970417     C                   EVAL      wimporto =  SlInAVaT21 - SlInDVaT21
087000980121     C                   EVAL      WSegno = Avere021
087100980121     C                   when      SlInDVaT21 = SlInAVat21
087200980121     C                   eval      Wimporto = 0
087300980121     C                   EVAL      WSegno = ' '
087400980121     C                   endsl
087500980121     C*
087600980121     C* modalità data operazione
087700980121     C                   ELSE
087800980121     C                   select
087900980121     C                   when      SlInDOpT21 > SlInAOpt21
088000980121     C                   EVAL      wimporto =  SlInDOpT21 - SlInAOpT21
088100980121     C                   EVAL      WSegno = Dare021
088200980121     C                   when      SlInDOpT21 < SlInAOpt21
088300980121     C                   EVAL      wimporto =  SlInAOpT21 - SlInDOpT21
088400980121     C                   EVAL      WSegno = Avere021
088500980121     C                   when      SlInDOpT21 = SlInAOpt21
088600980121     C                   eval      Wimporto = 0
088700980121     C                   EVAL      WSegno = ' '
088800980121     C                   endsl
088900980121     C                   endif
089000980121     C*
089100980121     C* se primo giro
089200980121     C                   IF        x = 1
089300980121     C* valorizzo il nr.decimali con quelli della m.c.
089400980121     C                   Z-ADD     NrDecMC       WNrDec
089500980121     C* se secondo giro
089600980121     C                   ELSE
089700980121     C* con quelli della divisa di rapporto
089800980121     C                   Z-ADD     NrDecDiv      WNrDec
089900980121     C                   ENDIF
090000980121     C*
090100980121     C* se il segno è DARE
090200980121     C                   if        WSegno = Dare021
090300980121     C* allineo l'importo a sinistra
090400951128     C                   SUB       5             $lenfld
090500951128     C                   Endif
090600980121     C*
090700980121     C* se secondo giro
090800980121     C                   IF        x = 2
090900980121     C* siccome bisogna allinearsi anche agli importi delle righe di BMV,
091000980121     C* che possono avere divise diverse e quindi decimali diversi, bisogna
091100980121     C* riproporzionare il campo di arrivo
091200951128     C                   Select
091300951128     C                   When      WNrDec = 2
091400980121     C                   Eval      $LenFld = $LenFld - 1
091500951128     C                   When      WNrDec = 1
091600980121     C                   Eval      $LenFld = $LenFld - 2
091700951128     C                   When      WNrDec = 0
091800980121     C                   Eval      $LenFld = $LenFld - 4
091900980121     C                   Endsl
092000980121     C                   ENDIF
092100980121     C*
092200980121     C* editazione importo
092300951128     C                   EXSR      Edit
092400980121     C*
092500980121     C* se primo giro
092600980121     C                   IF        x = 1
092700980121     C* salvo i dati della m.c.
092800980121     C                   EVAL      h1segnom = WSegno
092900980121     C                   Eval      h1saldom = $RcvrVar
093000980121     C* se secondo giro
093100980121     C                   ELSE
093200980121     C* salvo i dati della divisa
093300980121     C                   EVAL      h1segnod = WSegno
093400980121     C                   Eval      h1saldod = $RcvrVar
093500980121     C                   ENDIF
093600980121     C*
093700980121     C                   ENDDO
093800980121     C*
093900980121     C* il segno è sempre lo stesso
094000980121     C                   EVAL      S1Segno1 = H1SegnoM
094100980121     C                   EVAL      S1Segno2 = H1SegnoM
094200951128     C                   select
094300980121     C* se devo visualizzare in prima riga la m.c.
094400980121     C                   when      SavDivVis = '0'
094500980121     C                   eval      S1Saldo1 = H1SaldoM
094600980121     C                   EVAL      S1Divisa1  = H1MonConto
094700980121     C                   eval      S1Saldo2 = H1saldoD
094800980121     C                   EVAL      S1Divisa2  = H1Divisa
094900980121     C* se devo visualizzare in prima riga la divisa
095000951128     C                   when      Savdivvis = '1'
095100951128     C                   eval      s1saldo1 = h1saldod
095200951128     C                   EVAL      S1DiVisa1  = h1divisa
095300951128     C                   eval      s1saldo2 = h1saldom
095400951128     C                   EVAL      S1DiVisa2  = h1monconto
095500951128     C                   endsl
095600951128     C*
095700951128     C                   eval      s1causrig = 'SALD'
095800970422     C                   eval      s1causrigd = Ripresa
095900951128     C*
096000951128     C                   exsr      atrs1
096100951128     C                   ADD       1             S1NRR
096200951128     C                   ADD       1             Y
096300970416     C                   WRITE     CT11S1
096400951128     C*
096500951128     C                   ENDSR
096600940224     C/EJECT
096700940224     C************************************************************
096800940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
096900940224     C************************************************************
097000941214     C     SETANA        BEGSR
097100940224     C*
097200970416     C                   MOVE      XSCSOC        BMVSOCIETA
097300970421     C                   MOVE      CTBT11        BMVCTB
097400970416     C                   MOVE      BancaT11      BmvBanca
097500970416     C                   MOVE      LineaT11      BmvLinea
097600950704     C*
097700940224     C                   SELECT
097800941214     C*
097900941214     C* Gestione posizionamento
098000950301     C* Se il campo W1POS è > *blank imposto il posizionamento con
098100950301     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
098200941214     C*
098300970416    >C* Modalità 0 =  data valuta
098400970416     C     ModalitT11    WHENEQ    '0'
098500950705     C*
098600950705   B1C     W1POS         IFNE      0
098700951109     C                   CALLb     'XDT4000'
098800951109     C                   PARM                    XDTAMG           10
098900951109     C                   PARM      w1pos         XDTGMA            6 0
099000951109     C                   PARM      4             XDTSTA            1 0
099100970416    >C                   MOVE      xdtamg        BmvDtVal
099200940302     C                   ELSE
099300970417     c* discendente
099400970417     c                   if        $ord = 1
099500970416   B2C     DtperaT11     IFEQ      datmin
099600970416    >C                   MOVE      DATMAX        BmvDtVal
099700951114     C                   ELSE
099800970416    >C                   MOVE      DTPERaT11     BmvDtVal
099900950705   E2C                   ENDIF
100000970417     c                   else
100100970417     c* Ascendente
100200970417   B2C     DtperdaT11    IFEQ      datmin
100300970417    >C                   MOVE      DATMAX        BmvDtVal
100400970417     C                   ELSE
100500970417    >C                   MOVE      DTPERdaT11    BmvDtVal
100600970417   E2C                   ENDIF
100700951114   E2C                   ENDIF
100800970417   E2C                   ENDIF
100900951127     C*
101000970416     c* Discendente
101100970416     C                   if        $ord = 1
101200970421    >C     K05Bmv04      SETGT     NDBmv04
101300970416     c                   else
101400970416     c* Ascendente
101500970421    >C     K05Bmv04      SETll     NDBmv04
101600970416     c                   endif
101700970416     c*
101800970416    >C* Modalità 1 =  data operazione
101900970416     C     ModalitT11    WHENEQ    '1'
102000951108     c*
102100950705   B1C     W1POS         IFNE      0
102200951109     C                   CALLb     'XDT4000'
102300951109     C                   PARM                    XDTAMG
102400951109     C                   PARM      w1pos         XDTGMA
102500951109     C                   PARM      4             XDTSTA
102600970416    >C                   MOVE      xdtamg        BmvDtOpe
102700940302     C                   ELSE
102800970417     c* discendente
102900970417     c                   if        $ord = 1
103000970417   B2C     DtperaT11     IFEQ      datmin
103100970416    >C                   MOVE      DATMIN        BmvDtOpe
103200951114     C                   ELSE
103300970417    >C                   MOVE      DTPERAT11     BmvDtOpe
103400951114   E2C                   ENDIF
103500970417     c                   else
103600970417     c* Ascendente
103700970417   B2C     DtperdaT11    IFEQ      datmin
103800970417    >C                   MOVE      DATMAX        BmvDtOpe
103900970417     C                   ELSE
104000970417    >C                   MOVE      DTPERdaT11    BmvDtOpe
104100970417   E2C                   ENDIF
104200970417   E2C                   ENDIF
104300970417   E2C                   ENDIF
104400970416     C*
104500970416     c* Discendente
104600970416     C                   if        $ord = 1
104700970421    >C     K05Bmv03      SETGT     NDBmv03
104800970416     c                   else
104900970416     c* Ascendente
105000970421    >C     K05Bmv03      SETll     NDBmv03
105100970416     c                   endif
105200951108     c*
105300940224     C                   ENDSL
105400940224     C*
105500940224     C                   ENDSR
105600940128     C/EJECT
105700940128     C************************************************************
105800940131     C* LETTURA RCD ARCHIVIO PILOTA
105900940128     C************************************************************
106000941214     C     REDANA        BEGSR
106100940128     C*
106200950606     C                   MOVEL     *OFF          $EndFile
106300940131     C                   MOVEL     *OFF          $RCDOK
106400940131     C*
106500950606     C     $EndFile      DOUEQ     *ON
106600940131     C     $RCDOK        OREQ      *ON
106700940131     C*
106800941027     C                   SELECT
106900951113     C* Discendente
107000940131     C     $ORD          WHENEQ    1
107100970416     c                   if        ModalitT11= '0'
107200970421    >C     K04bmv04      READPE    NDBmv04                                21
107300970416     c                   else
107400970421    >C     K04bmv03      READPE    NDBmv03                                21
107500970416     c                   endif
107600951127     C*
107700951113     C* Ascendente
107800940131     C     $ORD          WHENEQ    2
107900951127     C*
108000970416     c                   if        ModalitT11= '0'
108100970421    >C     K04bmv04      READE     NDBmv04                                21
108200970416     c                   else
108300970421    >C     K04bmv03      READE     NDBmv03                                21
108400970416     c                   endif
108500970416     c*
108600941027     C                   ENDSL
108700940131     C*
108800950606     C                   MOVE      *IN21         $EndFile
108900940131     C*
109000950606     C     $EndFile      IFEQ      *OFF
109100941214     C                   EXSR      SELANA
109200941027     C                   ENDIF
109300940131     C*
109400941027     C                   ENDDO
109500940131     C*
109600940131     C                   MOVE      *IN21         *IN33
109700940131     C*
109800940131     C                   ENDSR
109900940224     C/EJECT
110000940224     C************************************************************
110100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
110200940224     C************************************************************
110300940224     C     CLCPAG        BEGSR
110400940224     C* Input :
110500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
110600940224     C*          cursore
110700940224     C* - SFLPAG = numero rcd per pagina sfl
110800940224     C* Output :
110900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
111000940224     C*
111100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
111200940224     C                   MVR                     RESTO             3 0
111300940224     C     RESTO         IFGT      0
111400940224     C                   ADD       1             PAGINE
111500940224     C                   ENDIF
111600940224     C     PAGINE        MULT      SFLPAG        WPAG
111700940224     C*
111800940224     C                   ENDSR
111900940131     C/EJECT
112000940131     C************************************************************
112100940131     C* SELEZIONI SULLA RIGA LETTA
112200940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
112300940314     C* non rientra nei parametri di parzializzazione
112400940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
112500940314     C* superiore stabilito nei parametri
112600940314     C*
112700940131     C************************************************************
112800941214     C     SELANA        BEGSR
112900940131     C*
113000940202     C                   MOVEL     *ON           $RCDOK
113100940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
113200940131A1211C                   MOVEL     *hival        ALL9             15
113300970416     C*
113400970416     C* Aggancio NDMOV che serve sempre per selezioni e caricamento sfl
113500970416     c                   eval      MovSys = BmvSys
113600970416     c                   eval      MovNrasReg= BmvNrasReg
113700970416     c                   eval      MovNrrigaM = BmvNRrigaM
113800970416     c     k03Mov01      chain     NdMov01l                           21
113900970724     C*
114000970724     C* aggancio NDREG che serve per la contropartita
114100970724     c                   clear                   wrkkcccpu
114200970724     c                   clear                   wrkksccpu
114300970724     C                   move      Bmvsys        regsys
114400970724     C                   move      Bmvnrasreg    regnrasreg
114500030617D1377X***  K02reg01      chain     ndreg01l                           21
114600030617  "  X***                move      *in21         FlgReg            1
114700030617  "  C     K02reg01      chain     ndreg01l                           22
114800030617D1377C                   move      *in22         FlgReg            1
114900970724   1 C                   if        movkcccpu<>*blank
115000970724     c* imposto contropartita nei campi WRk
115100970724     C                   eval      Wrkkcccpu = movkcccpu
115200970724     C                   eval      wrkksccpu = movksccpu
115300970724     C                   else
115400970724   2 C                   if        FlgReg=*off
115500970724   3 C                   if        movdare=1
115600970724     C                   move      regkcccpa     wrkkcccpu         6
115700970724     C                   move      regksccpa     wrkksccpu         8
115800970724     C                   else
115900970724   4 C                   if        movavere=1
116000970724     C                   move      regkcccpd     wrkkcccpu
116100970724     C                   move      regksccpd     wrkksccpu
116200970724   4eC                   endif
116300970724   3eC                   endif
116400970724   2eC                   endif
116500970724   1eC                   endif
116600970416     c*
116700951110     C                   SELECT
116800940314     C*
116900940314     C* SELEZIONI RISPETTO AI VALORI RCD
117000940314     C*
117100950314     C* Scartare i record che non rispondono alle selezioni effettuate
117200950301     C* esempio : esclusione degli annullati.
117300950906     c*
117400950906     C* Unità diversa
117500951110     C
117600970416     C                   When      UnitaT11 <> *blanks and
117700970416     C                             BmvUnita <> UnitaT11
117800950906     C                   move      *off          $RCDok
117900951109     c*
118000951109     C* Periodo
118100970416     C                   When      (BmvDtOpe <  DtperDaT11 or
118200970416     C                             BmvDtOpe >  DtperAT11) and
118300970416     c                             ModalitT11 = '1'
118400951109     C                   move      *off          $RCDok
118500970416     c*
118600970416     C                   When      (BmvDtVal <  DtperDaT11 or
118700970416     C                             BmvDtVal >  DtperAT11) and
118800970416     c                             ModalitT11 = '0'
118900970416     C                   move      *off          $RCDok
119000970417     c*
119100970417     C* Data di registrazione
119200970417     C                   When      BmvDtReg <  DtRegDaT11 or
119300970417     C                             BmvDtReg >  DtRegAT11
119400970417     C                   move      *off          $RCDok
119500970417     c*
119600970417     C* Data operazione/valuta
119700970417     C                   When      (BmvDtOpe <  DtDatDaT11 or
119800970417     C                             BmvDtOpe >  DtDatAT11) and
119900030417D1329x***                          modalitt11 = '1' or
120000030417D1329c                             modalitt11 = '0' or
120100970417     C                             (BmvDtVal <  DtDatDaT11 or
120200970417     C                             BmvDtVal >  DtDatAT11) and
120300030417D1329x***                          modalitt11 = '0'
120400030417D1329c                             modalitt11 = '1'
120500970417     C                   move      *off          $RCDok
120600970417     c*
120700970417     C* Importo
120800970417     C                   When      BmvImporto < ImportDT11 or
120900970417     C                             BmvImporto > ImportAT11
121000970417     C                   move      *off          $RCDok
121100951127     C*
121200951127     C* Provvis, definit, gestionali
121300970416     C                   WHEN      (MovdefT11 = *off   and
121400970416     C                             BmvTpRegz = 'D') or
121500970416     C                             (movprovT11 = *off   and
121600970416     C                             Bmvtpregz = 'P') or
121700970416     C                             (movgesT11  = *off   and
121800970416     C                             Bmvtpregz = 'G')
121900951127     C                   eval      $rcdok = *off
122000951127     c*
122100951109     c*
122200951109     C* Divisa selezionata (includi)
122300970416     C                   When      DivSelT11 = 'I' and divisaT11 <> *blanks
122400970416     C                             and BmvDivisa <>  divisaT11
122500951109     C                   move      *off          $RCDok
122600951109     c*
122700951109     C* Divisa selezionata (ometti)
122800970416     C                   When      DivSelT11 = 'O' and divisaT11 <> *blanks
122900970416     C                             and BmvDivisa =  divisaT11
123000951109     C                   move      *off          $RCDok
123100970605     c*
123200970605     C* Riconciliato
123300970911     C                   When      RiconciT11='1' and BmvStatus =  ' '  or
123400970911     C                             RiconciT11='2' and BmvStatus <> ' '
123500970605     C                   move      *off          $RCDok
123600951109     c*
123700951109     C                   ENDSL
123800970730     c*
123900970730     c* Contropartita
124000970730     c                   if        $rcdok = *on
124100970730     C                   if        KcccpT11 <> *blanks
124200970730     c* se diverso capoconto o sottoconto se richiesto
124300970730     c                   if        WrkKcccpu <> kccCpT11  or
124400970730     c                             kscCpT11  <> *blanks and
124500970730     c                             kscCpT11  <> wrkksccpu
124600970730     C                   move      *off          $RCDok
124700970730     C                   endif
124800970730     C                   endif
124900970730     C                   endif
125000970730     c*
125100970730     C* Classe
125200970730     c                   if        $rcdok = *on
125300971030     C* con queste IF avrebbe sempre scartato tutti i movimenti che avevano
125400971030     C* una classe <> dalla prima indicata ma magari uguale a una delle
125500971030     C* successive
125600971030     C*                  if        Classe1T11 <> *blanks and
125700971030     C*                            movclasse  <> Classe1T11
125800971030     C*                  move      *off          $RCDok
125900971030     C*                  endif
126000971030     C*                  if        Classe2T11 <> *blanks and
126100971030     C*                            movclasse  <> Classe2T11
126200971030     C*                  move      *off          $RCDok
126300971030     C*                  endif
126400971030     C*                  if        Classe3T11 <> *blanks and
126500971030     C*                            movclasse  <> Classe3T11
126600971030     C*                  move      *off          $RCDok
126700971030     C*                  endif
126800971030     C*                  if        Classe4T11 <> *blanks and
126900971030     C*                            movclasse  <> Classe4T11
127000971030     C*                  move      *off          $RCDok
127100971030     C*                  endif
127200971030     C*                  if        Classe5T11 <> *blanks and
127300971030     C*                            movclasse  <> Classe5T11
127400971030     C*                  move      *off          $RCDok
127500971030     C*                  endif
127600971030     C*                  if        Classe6T11 <> *blanks and
127700971030     C*                            movclasse  <> Classe6T11
127800971030     C*                  move      *off          $RCDok
127900971030     C*                  endif
128000971030     C*                  if        Classe7T11 <> *blanks and
128100971030     C*                            movclasse  <> Classe7T11
128200971030     C*                  move      *off          $RCDok
128300971030     C*                  endif
128400971030     C*                  if        Classe8T11 <> *blanks and
128500971030     C*                            movclasse  <> Classe8T11
128600971030     C*                  move      *off          $RCDok
128700971030     C*                  endif
128800971030     C*                  if        Classe9T11 <> *blanks and
128900971030     C*                            movclasse  <> Classe9T11
129000971030     C*                  move      *off          $RCDok
129100971030     C*                  endif
129200971030     C*                  if        Classe0T11 <> *blanks and
129300971030     C*                            movclasse  <> Classe0T11
129400971030     C*                  move      *off          $RCDok
129500971030     C*                  endif
129600971031     C* se c'è almeno una selezione per classe
129700971031     C                   IF        Classe1T11 <> *blank or
129800971031     C                             Classe2T11 <> *blank or
129900971031     C                             Classe3T11 <> *blank or
130000971031     C                             Classe4T11 <> *blank or
130100971031     C                             Classe5T11 <> *blank or
130200971031     C                             Classe6T11 <> *blank or
130300971031     C                             Classe7T11 <> *blank or
130400971031     C                             Classe8T11 <> *blank or
130500971031     C                             Classe9T11 <> *blank or
130600971031     C                             Classe0T11 <> *blank
130700971031     C* e la classe letta è diversa da ognuna
130800971031     C                   IF        MOVClasse <> Classe1T11 and
130900971031     C                             MOVClasse <> Classe2T11 and
131000971031     C                             MOVClasse <> Classe3T11 and
131100971031     C                             MOVClasse <> Classe4T11 and
131200971031     C                             MOVClasse <> Classe5T11 and
131300971031     C                             MOVClasse <> Classe6T11 and
131400971031     C                             MOVClasse <> Classe7T11 and
131500971031     C                             MOVClasse <> Classe8T11 and
131600971031     C                             MOVClasse <> Classe9T11 and
131700971031     C                             MOVClasse <> Classe0T11
131800971031     C* scarto il rcd
131900971031     C                   MOVE      *off          $RcdOk
132000971031     C                   ENDIF
132100971031     C                   ENDIF
132200970730     C                   endif
132300970730     c*
132400970730     C* Serie
132500970730     c                   if        $rcdok = *on
132600971030     C* con queste IF avrebbe sempre scartato tutti i movimenti che avevano
132700971030     C* una serie <> dalla prima indicata ma magari uguale a una delle
132800971030     C* successive
132900971030     C*                  if        Serie1T11 <> *blanks and
133000971030     C*                            movserreg <> Serie1T11
133100971030     C*                  move      *off          $RCDok
133200971030     C*                  endif
133300971030     C*                  if        Serie2T11 <> *blanks and
133400971030     C*                            movserreg <> Serie2T11
133500971030     C*                  move      *off          $RCDok
133600971030     C*                  endif
133700971030     C*                  if        Serie3T11 <> *blanks and
133800971030     C*                            movserreg <> Serie3T11
133900971030     C*                  move      *off          $RCDok
134000971030     C*                  endif
134100971030     C*                  if        Serie4T11 <> *blanks and
134200971030     C*                            movserreg <> Serie4T11
134300971030     C*                  move      *off          $RCDok
134400971030     C*                  endif
134500971030     C*                  if        Serie5T11 <> *blanks and
134600971030     C*                            movserreg <> Serie5T11
134700971030     C*                  move      *off          $RCDok
134800971030     C*                  endif
134900971031     C* se c'è almeno una selezione per serie
135000971031     C                   IF        Serie1T11  <> *blank or
135100971031     C                             Serie2T11  <> *blank or
135200971031     C                             Serie3T11  <> *blank or
135300971031     C                             Serie4T11  <> *blank or
135400971031     C                             Serie5T11  <> *blank
135500971031     C* e la serie letta è diversa da ognuna
135600971031     C                   IF        MOVSerReg <> Serie1T11  and
135700971031     C                             MOVSerReg <> Serie2T11  and
135800971031     C                             MOVSerReg <> Serie3T11  and
135900971031     C                             MOVSerReg <> Serie4T11  and
136000971031     C                             MOVSerReg <> Serie5T11
136100971031     C* scarto il rcd
136200971031     C                   MOVE      *off          $RcdOk
136300971031     C                   ENDIF
136400971031     C                   ENDIF
136500970730     C                   endif
136600970730     c*
136700970730     C* Causali
136800970730     c                   if        $rcdok = *on
136900971030     C* con queste IF avrebbe sempre scartato tutti i movimenti che avevano
137000971030     C* una causale <> dalla prima indicata ma magari uguale a una delle
137100971030     C* successive
137200971030     C*                  if        Causal1T11 <> *blanks and
137300971030     C*                            bmvcauban  <> Causal1T11
137400971030     C*                  move      *off          $RCDok
137500971030     C*                  endif
137600971030     C*                  if        Causal2T11 <> *blanks and
137700971030     C*                            bmvcauban  <> Causal2T11
137800971030     C*                  move      *off          $RCDok
137900971030     C*                  endif
138000971030     C*                  if        Causal3T11 <> *blanks and
138100971030     C*                            Bmvcauban  <> Causal3T11
138200971030     C*                  move      *off          $RCDok
138300971030     C*                  endif
138400971030     C*                  if        Causal4T11 <> *blanks and
138500971030     C*                            Bmvcauban  <> Causal4T11
138600971030     C*                  move      *off          $RCDok
138700971030     C*                  endif
138800971030     C*                  if        Causal5T11 <> *blanks and
138900971030     C*                            Bmvcauban  <> Causal5T11
139000971030     C*                  move      *off          $RCDok
139100971030     C*                  endif
139200971031     C* se c'è almeno una selezione per causale
139300971031     C                   IF        Causal1T11 <> *blank or
139400971031     C                             Causal2T11 <> *blank or
139500971031     C                             Causal3T11 <> *blank or
139600971031     C                             Causal4T11 <> *blank or
139700971031     C                             Causal5T11 <> *blank
139800971031     C* e la causale letta è diversa da ognuna
139900971031     C                   IF        BMVCauBan <> Causal1T11 and
140000971031     C                             BMVCauBan <> Causal2T11 and
140100971031     C                             BMVCauBan <> Causal3T11 and
140200971031     C                             BMVCauBan <> Causal4T11 and
140300971031     C                             BMVCauBan <> Causal5T11
140400971031     C* scarto il rcd
140500971031     C                   MOVE      *off          $RcdOk
140600971031     C                   ENDIF
140700971031     C                   ENDIF
140800971030     C                   endif
140900951109     C*
141000940202     C                   ENDSR
141100940131     C/EJECT
141200940131     C************************************************************
141300940131     C* RIEMPIMENTO RIGA LISTA
141400940131     C************************************************************
141500951127     C     RIES1         BEGSR
141600940131     C*
141700970416     C                   CLEAR                   CT11S1
141800950704     C* azzero le memorizzazioni degli attributi non bottoni
141900951127     C                   MOVEL     *ZERO         desaggh
142000951128     C                   MOVEL     *ZERO         in41h
142100951128     C*
142200970724     C* dati da ndreg
142300970724     C                   if        flgReg   = *off
142400951127     C                   eval      s1nrreg  = regnrreg
142500951127     C                   eval      s1utente = reguteulmo
142600970129     c                   else
142700970129     c* proteggo opzione
142800970129     C                   MOVEL     *on           in41h
142900951127     C                   ENDIF
143000951109     C*
143100970416     C                   move      bmvsys        h1sys
143200970416     C                   move      bmvnrasreg    h1nrasreg
143300970416     C                   move      bmvnrrigam    h1nrrigam
143400970416     c* data valuta
143500970416     C                   CALLb     'XDT4000'
143600970416     C                   PARM      Bmvdtval      XDTAMG
143700970417     C     S1DTvaluta    PARM                    XDTGMA
143800970416     C                   PARM      2             XDTSTA
143900970416     c* data operazione
144000970416     C                   CALLb     'XDT4000'
144100970416     C                   PARM      Bmvdtope      XDTAMG
144200970416     C     S1DTope       PARM                    XDTGMA
144300970416     C                   PARM      2             XDTSTA
144400951127     c* registrazione
144500951109     C                   CALLb     'XDT4000'
144600970416     C                   PARM      Bmvdtreg      XDTAMG
144700951127     C     S1DTreg       PARM                    XDTGMA
144800951109     C                   PARM      2             XDTSTA
144900951127     C                   MOVE      movserreg     S1SERreg
145000951127     c*Segnalo solo le registrazioni non definitive, per queste lascio blank
145100970416     C                   IF        Bmvtpregz <> 'D'
145200970416     C                   eval      s1tpregz  = bmvtpregz
145300951127     C                   endif
145400970724     c*Per quelle definitive e riconciliate, emetto una barra
145500970724     C                   IF        Bmvtpregz =  'D' and
145600970911     C                             (Bmvstatus =  '1'  or
145700970911     C                              Bmvstatus =  '7'     )
145800970724     C                   eval      s1tpregz  = '/'
145900970724     C                   endif
146000951127     c*
146100951127     C* decodifica causale (se diversa dalla precedente)
146200970416     C                   eval      s1causrig = bmvcauban
146300970416     C                   if        bmvcauban  <> Savcausrig
146400970416     C                   eval      savcausrig = BmvCauban
146500970416     C                   eval      wcausale    = BmvCauban
146600970416     C                   EXSR      CALLDvaBco
146700951127     C                   IF        Esito = *off
146800970416     C                   EVAL      S1CausrigD = BobDesbrev
146900951127     C                   ENDIF
147000951127     C                   else
147100970416     C                   EVAL      S1CausrigD = BobDesbrev
147200951127     C                   endif
147300951127     c* documento
147400951127     C                   CALLb     'XDT4000'
147500951127     C                   PARM      MovDTdoc      XDTAMG
147600951127     C     S1DTdoc       PARM                    XDTGMA
147700951127     C                   PARM      2             XDTSTA
147800951127     C                   eval      s1nrdoc = movnrdoc
147900951127     C                   eval      s1serdoc = movserdoc
148000951127     c*
148100951127     C                   eval      s1Cambio   = movcambio
148200970422     c* contropartita
148300970724     C                   eval      s1kcccpu = wrkkcccpu
148400970724     C                   eval      s1ksccpu = wrkksccpu
148500970422     C* decodifica contropartita
148600970422     C                   if        s1kcccpu<>*blank
148700970422     c                   eval      wkcc = s1kcccpu
148800970422     c                   eval      wksc = s1ksccpu
148900970422     c                   eval      Savnd002ds = nd002ds
149000970422     c                   exsr      CallMvc002
149100970422     c                   if        esito = *off
149200970422     c                   eval      s1ksccpud = desbrev002
149300970422     c                   endif
149400970422     c* rimetto ds banca
149500970422     c                   eval      nd002ds = savNd002ds
149600970422     c                   endif
149700970422     c*
149800970416     C                   EVAL      S1UNITA = bmvunita
149900951127     C                   eval      s1desbrev= movdesbrev
150000970731     C                   eval      s1caustes = movcaustes
150100970422     C                   eval      s1causriga= movcausrig
150200951127     C                   eval      s1classe = movclasse
150300970416     c                   eval      s1status = bmvstatus
150400970616     c                   eval      h1nrteso = bmvnrteso
150500951127     C*
150600951109     c* divise fisse
150700970416     C                   EVAL      h1Divisa   = bmvdivisa
150800951127     C                   EVAL      h1Monconto = xscdiv
150900951127     C*
151000951127     C* Calcola importi tramite routine edit
151100951127     C                   exsr      Importi
151200951110     C*
151300951110     c* Campi variabili a seguito richiesta moneta di conto o divisa
151400951110     C                   SELECT
151500951110     C* divisa
151600951110     C                   when      SavDivVis = '1'
151700970416     C                   EVAL      S1DiVisa1  = bmvdivisa
151800951110     C                   eval      s1saldo1   = H1saldod
151900951113     C                   eval      s1segno1   = H1segnod
152000951110     C                   EVAL      S1DiVisa2  = xscdiv
152100951110     C                   eval      s1saldo2   = H1saldom
152200951113     C                   eval      s1segno2   = H1segnom
152300951110     C* moneta di conto
152400951110     C                   when      SavDivVis = '0'
152500951110     C                   EVAL      S1Divisa1  = xscdiv
152600951110     C                   eval      s1saldo1   = H1saldom
152700951113     C                   eval      s1segno1   = H1segnom
152800970416     C                   EVAL      S1Divisa2  = bmvdivisa
152900951110     C                   eval      s1saldo2   = H1saldod
153000951113     C                   eval      s1segno2   = H1segnod
153100951110     C                   endsl
153200951115     C*
153300951115     C* se le divise sono uguali, imposto il valore del campo in drop
153400951115     C* uguale a quello della prima riga, così almeno sono allineati
153500970416     C                   IF        bmvdivisa = xscdiv
153600951115     C                   eval      s1saldo2 = s1saldo1
153700951115     C                   endif
153800951127     C*
153900951127     C* verifico se esistono descrizioni aggiuntive(riga)
154000951127     C                   EXSR      TestDesAGG
154100950301     C*
154200950301     C* imposto quegli attributi del sfl che sono modificabili
154300950301     C* al richiamo del determinato pgm
154400950301     C                   EXSR      ATRS1
154500940131     C*
154600940131     C                   ENDSR
154700970416     C/EJECT
154800970416     C*----------------------------------------------------------
154900970416     C* Controllo esistenza cauban con nddva
155000970416     C*----------------------------------------------------------
155100970416     C     CallDvaBco    BEGSR
155200970416     C*
155300970416     c                   move      Datmin        Dtope
155400970416     c                   eval      StrutturaO = 'ANBOB'
155500970416     C                   Eval      LenOut=%Size(ANBOB)
155600970416     C                   CallB     'NDDVABCO'
155700970416     C                   PARM      '01'          Operazione
155800970416     C                   PARM                    xscsoc
155900970416     C                   PARM                    BancaT11
156000970416     C                   PARM                    LineaT11
156100970416     C                   PARM                    WCausale
156200970416     C                   PARM                    DtOpe
156300970416     C                   PARM                    StrutturaO
156400970416     C                   PARM                    LenOut
156500970416     C                   PARM                    Esito
156600970416     C                   PARM                    ANBOB
156700970416     C*
156800970416     C                   ENDSR
156900951109     C/EJECT
157000951127     C************************************************************
157100951127     C* testa esistenza descrizioni aggiuntive di testata o riga
157200951127     C************************************************************
157300951127     C     TestDesAGG    BEGSR
157400951127     C                   EVAL      Desaggh   = '0'
157500951127     C* (si esegue setll sul file)
157600951127     C                   eval      DesSys = H1Sys
157700951127     C                   eval      DesNrAsReg = H1NrAsReg
157800951127     C                   eval      DesTipDes  = 'M'
157900951127     C                   eval      DesNrRigaM = H1NrRigaM
158000951127     C     K04DES01      SETLL     NDDES01L                               21
158100951127     C* se il bottone è presente
158200951127     C                   IF        *in21  = '1'
158300951127     C* memorizzo l'attributo di presente
158400951127     C                   EVAL      Desaggh   = '1'
158500951127     C                   else
158600951127     c* provo a vedere se ce ne sono di testata
158700951127     C                   eval      DesSys = H1Sys
158800951127     C                   eval      DesNrAsReg = H1NrAsReg
158900951127     C                   eval      DesTipDes  = 'T'
159000951127     C     K03DES01      SETLL     NDDES01L                               21
159100951127     C* se il bottone è presente
159200951127     C                   IF        *in21  = '1'
159300951127     C* memorizzo l'attributo di presente
159400951127     C                   EVAL      Desaggh   = '1'
159500951127     C                   ENDIF
159600951127     C                   endif
159700951127     C*
159800951127     C                   ENDSR
159900951109     C/EJECT
160000951109     C************************************************************
160100951109     C* Calcolo saldo per partita e totale se lo deve fare
160200951109     C************************************************************
160300980121     C     Importi       BEGSR
160400951113     C*
160500951113     C* addiziono per totale CONTO se lo deve fare (è sempre in moneta
160600951113     C* di conto)
160700951113     C                   if        $carpag = *off
160800970416     C                   if        bmvdare = 1
160900980130     C                   add       bmvimporto    WDare
161000980130     C* l'importo in divisa lo sommo solo se è della stessa divisa del
161100980130     C* rapporto bancario
161200980130     C                   IF        BMVDivisa = DivBRAT11 or
161300980130     C                             BMVDivisa = XscDiv and
161400980130     C                             DivBRAT11 = *blank
161500980130     C                   ADD       BMVImportD    WDareD
161600980130     C                   ENDIF
161700980130     C                   ENDIF
161800970416     C                   if        bmvavere = 1
161900980130     C                   add       bmvimporto    WAvere
162000980130     C* l'importo in divisa lo sommo solo se è della stessa divisa del
162100980130     C* rapporto bancario
162200980130     C                   IF        BMVDivisa = DivBRAT11 or
162300980130     C                             BMVDivisa = XscDiv and
162400980130     C                             DivBRAT11 = *blank
162500980130     C                   ADD       BMVImportD    WAvereD
162600980130     C                   ENDIF
162700951113     C                   endif
162800951127     C                   endif
162900951113     C*
163000951113     C* Dati in MONETA DI CONTO
163100951109     C*
163200951110     C* Segno
163300970416     C                   if        bmvdare = 1
163400970422     C                   eval      h1segnom  = Dare021
163500951127     C                   endif
163600970416     C                   if        bmvavere= 1
163700970422     C                   eval      h1segnom  = Avere021
163800951127     C                   endif
163900951110     C* Importo
164000051206C1886C                   Eval      H1saldomC= BmvImporto
164100970416     C                   Z-add     bmvimporto    WImporto
164200980121     C                   Z-add     NrDecMC       WNrDec
164300951110     C                   Eval      $LenFld = %Size(h1SaldoM)
164400951110     C* allineo dare a sinistra
164500970422     C                   if        h1segnoM = Dare021
164600951110     C                   SUB       5             $lenfld
164700951110     C                   Endif
164800951109     C                   EXSR      Edit
164900951127     C                   Eval      h1saldom = $RcvrVar
165000951109     C*
165100951113     C* Dati in DIVISA
165200951110     C* segno
165300951127     C                   eval      h1segnoD  = h1segnoM
165400951109     C*
165500951110     C* Importo in divisa
165600951110     C* reperisco decimali se divisa diversa dalla precedente letta
165700970416     C                   if        bmvdivisa <> Savdivisa
165800970416     C                   eval      SavDivisa = bmvdivisa
165900970416     C                   Eval      WDivisa =  bmvdivisa
166000951113     C                   Exsr      RepDivisa
166100980121     C                   Eval      NrDecDiv = X01NrDec
166200951113     C                   endif
166300951110     C*
166400951110     C* importo
166500970416     C                   EVAL      Wimporto = bmvImportD
166600980121     C                   Z-add     NrDecDiv      WNrDec
166700951110     C                   Eval      $LenFld = %Size(h1SaldoD)
166800951110     C*
166900951110     C* Imposto la dimensione del campo video in modo da allineare
167000951110     C* valute gestite con un numero diverso di decimali
167100951109     C                   Select
167200951110     C                   When      WNrDec = 2
167300951110     C                   Eval      $LenFld = %Size(h1SaldoD) - 1
167400951110     C                   When      WNrDec = 1
167500951110     C                   Eval      $LenFld = %Size(h1Saldod) - 2
167600951110     C                   When      WNrDec = 0
167700951110     C                   Eval      $LenFld = %Size(h1Saldod) - 4
167800951109     C                   Other
167900951110     C                   Eval      $LenFld = %Size(h1Saldod)
168000951109     C                   Endsl
168100951110     C* allineo dare a sinistra
168200970422     C                   if        H1SegnoD = Dare021
168300951110     C                   SUB       5             $lenfld
168400951110     C                   Endif
168500951109     C                   EXSR      Edit
168600951110     C                   Eval      h1Saldod  = $RcvrVar
168700951109     C*
168800951109     C                   ENDSR
168900950301     C/EJECT
169000950301     C************************************************************
169100950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
169200950301     C************************************************************
169300950301     C     ATRS1         BEGSR
169400950301     C*
169500951127    >C* impostare le condizioni per ripristinare gli attributi
169600951127    >C* della riga del sfl (se ce ne sono)
169700951127      * attiva o no unità
169800951127     C                   if        fgo1=*on
169900951127     C                   eval      *in11=*on
170000951127     C                   else
170100951127     C                   eval      *in11=*off
170200951127     C                   endif
170300951127      * se ci sono descrizioni aggiuntive sulla riga sottolineo causale
170400951127     C                   if        desaggh  = *on
170500951127     C                   eval      *in14=*on
170600951127     C                   else
170700951127     C                   eval      *in14=*off
170800951127     C                   endif
170900970129      * opzione protetta per riga saldo o per registrazione non ancora
171000970129     c* confermata, quindi non presente su ndreg
171100951128     C                   eval      *in41=in41h
171200950301     C*
171300950301     C                   ENDSR
171400941205     C/EJECT
171500941205     C************************************************************
171600941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
171700941205     C* INPUT
171800951109     C* $FC1 = Abilitazioni videata S1
171900941205     C* OUTPUT
172000941205     C* *IN97 *IN99 Errori
172100941205     C************************************************************
172200941205     C     CTRKS1        BEGSR
172300941205     C*
172400941205     C                   SETOFF                                       99
172500951109     C                   MOVEA     $FC1          $FC
172600941205     C                   EXSR      CTRKEY
172700941205     C     *IN99         IFEQ      *ON
172800941205     C                   SETON                                        97
172900941205     C                   ENDIF
173000941205     C*
173100941205     C                   ENDSR
173200941206     C/EJECT
173300941206     C************************************************************
173400941206     C* GESTIONE F08 VIDEO S1
173500941206     C************************************************************
173600941206     C     F08S1         BEGSR
173700941206     C*
173800941206     C                   MOVE      *ON           $FINE
173900970416    >C                   MOVE      '0'           RETT11
174000941206     C*
174100941206     C                   ENDSR
174200940309     C/EJECT
174300940309     C************************************************************
174400940309     C* GESTIONE F03 VIDEO S1
174500940309     C************************************************************
174600940309     C     F03S1         BEGSR
174700940309     C*
174800940309     C                   MOVE      *ON           $FINE
174900970416    >C                   MOVE      '1'           RETT11
175000940325     C* fine programma
175100940325     C                   EXSR      ENDPGM
175200940309     C*
175300940309     C                   ENDSR
175400951109     C/EJECT
175500951109     C************************************************************
175600951109     C* GESTIONE F11 VIDEO S1 - cambio moneta
175700951109     C************************************************************
175800951109     C     F11S1         BEGSR
175900951113     C*
176000951113     C* Eseguo solo se ci sono record nel sfl
176100951113     C                   IF        Wmax <> 0
176200951110     C* Scambio richiesta
176300951110     C                   select
176400951110     C                   when      Savdivvis = '0'
176500951110     C                   eval      Savdivvis = '1'
176600951110     C*
176700951110     C                   when      Savdivvis = '1'
176800951110     C                   eval      Savdivvis = '0'
176900951110     C                   endsl
177000951110     C*
177100951110     C                   DO        *hival        $X                4 0
177200970416     C     $X            chain     cT11s1                             21
177300951110     C   21              LEAVE
177400951110     C                   select
177500951110     C                   when      Savdivvis = '0'
177600951110     C                   eval      s1saldo1 = h1saldom
177700951127     C                   EVAL      S1DiVisa1  = h1monconto
177800951110     C                   eval      s1saldo2 = h1saldod
177900951127     C                   EVAL      S1DiVisa2  = h1divisa
178000951110     c*
178100951110     C                   when      Savdivvis = '1'
178200951110     C                   eval      s1saldo1 = h1saldod
178300951127     C                   EVAL      S1DiVisa1  = h1divisa
178400951110     C                   eval      s1saldo2 = h1saldom
178500951127     C                   EVAL      S1DiVisa2  = h1monconto
178600951110     C                   endsl
178700951115     C*
178800951115     C* se le divise sono uguali, imposto il valore del campo in drop
178900951115     C* uguale a quello della prima riga, così almeno sono allineati
179000951115     C                   IF        s1divisa1 = s1divisa2
179100951115     C                   eval      s1saldo2 = s1saldo1
179200951115     C                   endif
179300960626     c                   if        s1opz <> 0
179400960626     C                   seton                                        32
179500960627     c                   else
179600960627     c                   setoff                                       32
179700960626     c                   endif
179800951113     C* imposto quegli attributi del sfl che sono modificabili
179900951113     C* al richiamo del determinato pgm
180000951113     C                   EXSR      ATRS1
180100960627     c* spengo indicatore r.i. per opzione
180200960627     C                   setoff                                       40
180300951113     C*
180400970416     C                   UPDATE    CT11S1
180500951110     C*
180600951110     C                   ENDDO
180700951109     C*
180800951113     C                   ENDIF
180900980119     C*
181000980119     C* se il conto in esame è valutario (la divisa del rapporto è <> m.c.)
181100980119     C                   IF        DivBRAT11 <> XscDiv
181200980119     C* eseguo il cambio moneta anche sui dati di saldo
181300980119     C                   IF        *in08 = *off
181400980119     C                   SETON                                        08
181500980119     C                   ELSE
181600980119     C                   SETOFF                                       08
181700980119     C                   ENDIF
181800980119     C                   ENDIF
181900980119     C*
182000951109     C                   ENDSR
182100940309     C/EJECT
182200940309     C************************************************************
182300940309     C* GESTIONE F12 VIDEO S1
182400940309     C************************************************************
182500940309     C     F12S1         BEGSR
182600940309     C*
182700940309     C                   MOVE      *ON           $FINE
182800970416    >C                   MOVE      '2'           RETT11
182900940309     C*
183000940309     C                   ENDSR
183100951109     C/EJECT
183200951109     C************************************************************
183300951109     C* GESTIONE F15 VIDEO S1
183400951109     C************************************************************
183500951109     C     F15S1         BEGSR
183600951109     C*
183700951109     C                   reset                   ana399ds
183800951109     C                   eval      A39societa = XSCSOC
183900970416     C                   eval      A39unita = unitaT11
184000970416     C                   eval      A39ctb = ctbT11
184100970416     C                   eval      A39kcc = kccT11
184200970416     C                   eval      A39ksc = KSCT11
184300970416     C                   eval      A39SNATURA = snatura002
184400951109     C                   movel     ana399ds      kpjbu
184500951109     C                   CALL      'ANA399R'                            21
184600951109     C                   PARM                    KPJBA
184700951109     C     A39ERRORI     IFEQ      '0'
184800951109     C                   endif
184900951109     C*
185000951109     C                   ENDSR
185100970515     C/EJECT
185200970515     C************************************************************
185300970515     C* GESTIONE F20 VIDEO S1 - Stampa E/C
185400970515     C************************************************************
185500970515     C     F20S1         BEGSR
185600970515     C* Reperimento titolo stampa
185700970515     C                   EXSR      REPTIT
185800970515     C                   RESET                   NDCT31DS
185900970515     C                   MOVE      xscsoc        socT31
186000970515     C                   MOVE      CtbT11        ctbT31
186100970515     C                   MOVE      kcct11        KCCT31
186200970515     C                   MOVE      ksct11        KSCT31
186300970515     C                   MOVE      bancaT11      bancaT31
186400970515     C                   MOVE      lineaT11      lineaT31
186500970515     C                   move      movdefT11     defT31
186600970515     C                   move      movprovT11    prvT31
186700970515     C                   move      movgesT11     gesT31
186800970515     C                   move      dtperdaT11    dtdT31
186900970515     C                   move      dtperaT11     dtaT31
187000970515     C                   MOVE      modalitT11    modT31
187100970515     C                   MOVE      divvisT11     divT31
187200980911     C                   MOVE      riconciT11    RiconcT31
187300970515     c                   eval      lrT31 = *on
187400970515     C                   MOVEL     NDCT31DS      KPJBU
187500970515     C                   CALL      'NDCT33R1'
187600970515     C                   PARM                    KPJBA
187700970515     C                   PARM                    TIT              50
187800970515     C*
187900970515     c                   endsr
188000970515     C/EJECT
188100970515     C************************************************************
188200970515     C* REPERIMENTO TITOLO stampa
188300970515     C************************************************************
188400970515     C     REPTIT        BEGSR
188500970515     C*
188600970515     C                   MOVE      *BLANKS       TIT
188700970515     C*
188800970515     C                   MOVEL     $TIT(1)       IDMSG
188900970515     C                   CALLB     'XRTVM'
189000970515     C                   PARM                    IDMSG            27
189100970515     C                   PARM                    TXTMSG          100
189200970515     C                   PARM                    ERRMSG            1
189300970515     C                   PARM      *ON           CTRMSG            1            centratura
189400970515     C                   PARM      50            LENMSG            3 0          lun output.
189500970515     C*
189600970515     C     ERRMSG        IFNE      *ON
189700970515     C                   MOVEL     TXTMSG        TIT
189800970515     C                   ELSE
189900970515     C                   MOVEL     *ALL'?'       TIT
190000970515     C                   ENDIF
190100970515     C*
190200970515     C                   ENDSR
190300940309     C/EJECT
190400940309     C************************************************************
190500940309     C* GESTIONE F23 VIDEO S1
190600940309     C************************************************************
190700940309     C     F23S1         BEGSR
190800940309     C*
190900941220    >C                   Z-ADD     76            $L01
191000941220    >C                   Z-ADD     76            $L02
191100940309     C                   EXSR      RIEOPZ
191200941214     C                   Z-ADD     23            X
191300951109     C                   MOVE      FF23          $FC1(X)
191400940608     C                   MOVEL     WOPZ1         C1OPZ1
191500060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
191600951109     C                   MOVEL     WOPZ2         C1OPZ2
191700940309     C                   Z-ADD     WCOZ          C1COZ
191800940309     C*
191900940309     C                   ENDSR
192000940309     C/EJECT
192100940309     C************************************************************
192200940309     C* GESTIONE F24 VIDEO S1
192300940309     C************************************************************
192400940309     C     F24S1         BEGSR
192500940506     C*
192600951109     C                   MOVEA     $FX1          $FX
192700951109     C                   MOVEA     $FC1          $FC
192800951109     C                   MOVEA     $FR1          $FR
192900940506     C                   Z-ADD     $ULKS1        $ULK
193000941220    >C                   Z-ADD     79            $L01
193100941220    >C                   Z-ADD     79            $L02
193200940506     C                   EXSR      RIEKEY
193300940506     C                   Z-ADD     $ULK          $ULKS1            3 0
193400951109     C                   MOVEA     $FC           $FC1
193500951109     C                   MOVEL     $KEY1         Z1KE1
193600060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
193700951109     C                   MOVEL     $KEY2         Z1KE2
193800970416     C                   WRITE     CT11Z1
193900940506     C*
194000940309     C                   ENDSR
194100940128     C/EJECT
194200940128     C************************************************************
194300940131     C* CONTROLLO TESTATA LISTA
194400940128     C************************************************************
194500940131     C     CTRC1         BEGSR
194600940128     C*
194700940201     C                   MOVE      *OFF          *IN99
194800951109     C*
194900951109     C                   if        c1pos <> 0
195000951109     C                   CALLb     'XDT4000'
195100951109     C                   PARM                    XDTAMG           10
195200951109     C                   PARM      c1pos         XDTGMA            6 0
195300951109     C                   PARM      3             XDTSTA            1 0
195400951109     C     XDTSTA        IFLT      0
195500951109     C                   SETON                                        6099
195600951109     C                   endif
195700951109     C                   endif
195800940131     C*
195900940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
196000950707     C* L'INIZIALIZZAZIONE LISTA   (lo attivo anche se w1pos = c1pos, per ri
196100950707     C* partire dal primo numero partita
196200951109     C                   IF        *in99 = *off
196300950707     C     C1POS         IFNE      W1POS
196400950704    >C* aggiungere in OR altri campi posizionamento se ci sono
196500940202     C                   EXSR      POSS1
196600950707     C                   ENDIF
196700951109     C                   Endif
196800940202     C*
196900940202     C                   ENDSR
197000940202     C/EJECT
197100940202     C************************************************************
197200940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
197300940202     C************************************************************
197400940202     C     POSS1         BEGSR
197500940202     C*
197600940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
197700940202     C*
197800941118   B1C     WMAX          IFGT      0
197900970416     C                   READC     CT11S1                                 21
198000941118   B2C     *IN21         DOWEQ     *OFF
198100940202     C     *IN99         ANDEQ     *OFF
198200940202     C*
198300941118   B3C     S1OPZ         IFNE      0
198400940202     C                   SETON                                        51  99
198500941118   E3C                   ENDIF
198600940202     C* se rilevata opzione attiva, attivo sflnxtchg
198700941118   B3C     *IN99         IFEQ      *ON
198800940202     C                   MOVE      *ON           *IN32
198900950301     C*
199000950301     C* imposto quegli attributi del sfl che sono modificabili
199100950301     C* al richiamo del determinato pgm
199200941118     C                   EXSR      ATRS1
199300941118     C*
199400970416     C                   UPDATE    CT11S1
199500941118   X3C                   ELSE
199600970416     C                   READC     CT11S1                                 21
199700941118   E3C                   ENDIF
199800940202     C*
199900941118   E2C                   ENDDO
200000940202     C*
200100941118   E1C                   ENDIF
200200940202     C*
200300940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
200400940221     C* AL VALORE DI POSIZIONAMENTO :
200500940221     C* se questo è maggiore del valore del rispettivo campo di
200600940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
200700940221     C* con i valori di parzializzazione.
200800940221     C*
200900941118   B1C     *IN99         IFEQ      *OFF
201000950704    >C                   MOVE      *ZEROS        W1POS
201100940202     C*
201200950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
201300950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
201400950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
201500950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
201600950525     C* verificata la condizione suddetta avverrà con posizionamento a
201700950525     C* partire dal valore di parzializzazione
201800950525     C*
201900941118   B2C                   SELECT
202000950704    >C* Attenzione : se un ordinamento ha un campo di posizionamento
202100950704    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
202200950704    >C* dei relativi campi
202300940202     C*
202400940202     C     $ORD          WHENEQ    1
202500950406     C                   MOVEL     C1POS         W1POS
202600940202     C*
202700940202     C     $ORD          WHENEQ    2
202800941214     C                   MOVE      C1POS         W1POS
202900950525     C*
203000941118   E2C                   ENDSL
203100950525     C*
203200950525     C* richiedo l'inizializzazione sfl
203300950525     C                   MOVE      *ON           *IN99
203400950525     C                   MOVE      *ON           $INZS1
203500940202     C*
203600941118   E1C                   ENDIF
203700940202     C*
203800940131     C                   ENDSR
203900940131     C/EJECT
204000940131     C************************************************************
204100940131     C* CONTROLLO OPZIONI LISTA
204200940131     C************************************************************
204300940131     C     CTRS1         BEGSR
204400940131     C*
204500940202     C                   MOVEL     *OFF          $ESCI
204600940201     C                   SETOFF                                       99
204700940131     C                   Z-ADD     0             S1OPZ
204800940131     C*
204900940127     C* Leggo il sfl solo se ci sono rcd
205000941118   B1C     WMAX          IFGT      0
205100970416     C                   READC     CT11S1                                 21
205200940127     C*
205300940131     C* esce se fine sfl o errore che richiede l'uscita
205400941118   B2C     *IN21         DOWEQ     *OFF
205500940131     C     $ESCI         ANDEQ     *OFF
205600940201     C                   Z-ADD     S1NRR         C1RCD
205700940131     C* ctrl su riga
205800940131     C                   EXSR      RECS1
205900940131     C* gestione opzioni
206000941118   B3C     S1OPZ         IFNE      0
206100940201     C     *IN99         ANDEQ     *OFF
206200940131     C                   EXSR      OPZS1
206300941118   E3C                   ENDIF
206400940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
206500941118   B3C     *IN99         IFEQ      *ON
206600940131     C                   MOVE      *ON           *IN32
206700941118   E3C                   ENDIF
206800950301     C*
206900950301     C* imposto quegli attributi del sfl che sono modificabili
207000950301     C* al richiamo del determinato pgm
207100941118     C                   EXSR      ATRS1
207200941118     C*
207300940131     C                   Z-ADD     0             S1OPZ
207400940223     C*
207500970416     C                   UPDATE    CT11S1
207600941125     C                   MOVE      *OFF          *IN32
207700940223     C*
207800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
207900941118   B3C     $ESCI         IFEQ      *OFF
208000970416     C                   READC     CT11S1                                 21
208100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
208200941118   B4C     *IN21         IFEQ      *ON
208300940201     C                   MOVE      WINZS1        $INZS1
208400940204     C* calcolo pagina a cui deve posizionarsi
208500940728     C     WSFL          IFGT      0
208600940728     C                   EXSR      CLCPAG
208700940728     C                   ELSE
208800940728     C                   Z-ADD     SFLPAG        WPAG
208900940728     C                   ENDIF
209000941118   E4C                   ENDIF
209100941118   E4C                   ENDIF
209200940131     C*
209300941118   E2C                   ENDDO
209400940127     C*
209500941118   E1C                   ENDIF
209600940131     C*
209700940127     C                   ENDSR
209800940127     C/EJECT
209900940127     C************************************************************
210000940131     C* CONTROLLO CAMPI I/O RIGA LISTA
210100940127     C************************************************************
210200940131     C     RECS1         BEGSR
210300940131     C*
210400940201     C* reset indicatori DSPATR
210500940201     C                   MOVE      *ALL'0'       IN4049           10
210600940201     C                   MOVEA     IN4049        *IN(40)
210700940201     C*
210800940131     C* Controllo se Utente Autorizzato alla scelta
210900940202     C     S1OPZ         IFNE      0
211000940131     C                   Z-ADD     1             X
211100940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
211200940211     C     *IN21         IFEQ      *OFF
211300940223     C     $FL1(X)       OREQ      'N'
211400940131     C                   MOVE      *ON           $ESCI
211500940201     C                   SETON                                        504099
211600940131     C                   ENDIF
211700940202     C                   ENDIF
211800970613     c*
211900970613     C* Se opzione riconcilia, controllo se utente autorizzato
212000970724     c                   if        s1opz = 52 and TesRicMan = *off
212100970613     C                   SETON                                        944099
212200970613     c                   endif
212201160310>>>>>c                   if        s1opz = 51 and TesRicMan = *off
212202160310>>>>>C                   SETON                                        944099
212203160310>>>>>c                   endif
212300970911     C*
212400970911     C* E' possibile l'opzione 50=riconciliazione solo se il movimento è
212500970911     C* riconciliato o chiuso ed esiste il numero di riconciliazione
212600970911     c                   IF        S1Opz = 50   and
212700980922     c                             (S1Status = ' ' or H1NrTeso = 0)
212800970724     C                   MOVE      *ON           $ESCI
212900970724     C                   SETON                                        407499
213000970724     c                   endif
213100970911     C*
213200970911     C* E' possibile l'opzione 51=De/riconciliazione solo se il movimento è
213300970911     C* riconciliato ed esiste il numero di riconciliazione
213400970911     c                   IF        S1Opz = 51   and
213500980922     c                             (S1Status <> '1' or H1NrTeso = 0)
213600970911     C                   MOVE      *ON           $ESCI
213700970911     C                   SETON                                        407499
213800970911     c                   endif
213900950706     C*
214000940131     C                   ENDSR
214100941118     C/EJECT
214200941118     C************************************************************
214300941118     C* GESTIONE OPZIONI LISTA
214400941118     C************************************************************
214500941118     C     OPZS1         BEGSR
214600950301     C*
214700950301     C                   Z-ADD     RIRI          CURR
214800950301     C                   Z-ADD     COCO          CURC
214900941118     C*
215000941027     C                   SELECT
215100960318     C*
215200960318     C* se opz=22 richiamo visualizzazione partite
215300960318     C* passo la chiave al pgm chiamante
215400960318     C     S1OPZ         WHENEQ    22
215500960318     C*
215600960318     C                   RESET                   NDC073DS
215700960318     c                   move      xscsoc        societa073
215800960318     C                   MOVEL     H1Sys         Sys073
215900970605     C                   z-add     H1NrAsReg     NrAsReg073
216000970605     C                   z-add     H1NrRigaM     NrRigaM073
216100970416     C                   movel     descr002      kcckscd073
216200960318     C                   move      winz073       exf073
216300960318     C                   MOVE      '05'          Opz073
216400970416     c                   eval      snatura073  = snatura002
216500960318     C*
216600960318     C                   movel     NDC073DS      KPJBU
216700960318     C                   CALL      'NDC073R'
216800960318     C                   PARM                    Kpjba
216900960318     C                   Movel     kpjbu         NDC073DS
217000960318     C*
217100960318     C                   Exsr      Rtn073
217200951205     C*
217300951205     C* se opz=23 richiamo tesoreria
217400951205     C* passo la chiave al pgm chiamante
217500951205     C     S1OPZ         WHENEQ    23
217600951205     C*
217700951205     C                   RESET                   NDC084DS
217800951205     C                   MOVEL     H1Sys         Sys084
217900970605     C                   z-add     H1NrAsReg     NrAsReg084
218000970605     C                   z-add     H1NrRigaM     NrRigaM084
218100951205     C                   MOVE      '02'          Opz084
218200960105     C                   move      wexf084       exf084
218300951205     C*
218400951205     C                   movel     NDC084DS      KPJBU
218500951205     C                   CALL      'NDC084R'
218600951205     C                   PARM                    Kpjba
218700951205     C                   Movel     kpjbu         NDC084DS
218800951205     C*
218900951205     C                   Exsr      Rtn084
219000040212     C*
219100040212C1718C* se opz=25 richiamo cash flow
219200040212  "  C     S1OPZ         WHENEQ    25
219300040212  "  C                   RESET                   NDCT68DS
219400040212  "  C                   MOVEL     '02'          OPZt68
219500040212  "  C                   z-add     H1Sys         SYST68
219600040212  "  C                   z-add     H1NrAsReg     NRASREGT68
219700040212  "  C                   z-add     H1NrRigaM     NRRigamT68
219800040212  "  C                   move      wexft68       inzt68
219900040220  "  c                   eval      divisaT68 = h1divisa
220000040910  "  c                   eval      DescrT68  = s1CausrigD
220100040212  "  C*
220200040212  "  C                   movel     NDCt68DS      KPJBU
220300040212  "  C                   CALL      'NDCT68R'
220400040212  "  C                   PARM                    Kpjba
220500040212  "  C                   movel     KPJBU         ndcT68DS
220600040212  "  C* ritorno da PGM gestione
220700040212C1718C                   EXSR      RTNT68
220800960104     C*
220900051205C1886C* se opz=30 Movimento home banking
221000051205  "  C     S1OPZ         WHENEQ    30
221100051213  "  c                   reset                   x48chPgmds
221200051213  "  C                   eval      x48pgm = 'NDCK40R'
221300051213  "  c                   eval      kpjbu = x48chpgmds
221400051213  "  c                   call      'X48CHPGM'
221500051213  "  c                   parm                    kpjba
221600051213  "  c                   movel     kpjbu         x48chpgmds
221700051213  "  c                   if        x48errore = *off
221800051205  "  C                   RESET                   NDCK40DS
221900051213  "  C                   MOVEL     '05'          OPZK40
222000051206  "  c                   move      xscsoc        SocietaK40
222100051206  "  c                   movel     C1Banca       BancaK40
222200051206  "  c                   movel     C1Linea       LineaK40
222300051206  "  c                   z-add     H1SaldomC     ImpoDaK40
222400051206  "  c                   z-add     H1SaldomC     ImpoAK40
222500051206  "  c                   move      H1SegnoM      SegnoK40
222600051206  "  C     *JobRun       Move      s1dtvaluta    Dataiso
222700051206  "  c     Dataiso       subdur    2:*D          dataiso1
222800051206  "  c                   move      Dataiso1      DvaDaK40
222900051206  "  c     Dataiso       adddur    2:*D          dataiso1
223000051206  "  c                   move      Dataiso1      DvaAK40
223100051206  "  C     *JobRun       Move      s1dtope       Dataiso
223200051206  "  c     Dataiso       subdur    2:*D          dataiso1
223300051206  "  c                   move      Dataiso1      DopDaK40
223400051206  "  c     Dataiso       adddur    2:*D          dataiso1
223500051206  "  c                   move      Dataiso1      DopAK40
223600051205  "  C                   movel     NDCK40DS      KPJBU
223700051205  "  C                   CALL      'NDCK40R'
223800051205  "  C                   PARM                    Kpjba
223900051205  "  C                   movel     KPJBU         ndcK40DS
224000051205  "  C* ritorno da PGM gestione
224100051213  "  C                   EXSR      RTNK40
224200051213c1886c                   endif
224300951205     C*
224400951205     C* Richiamo prima nota
224500951205     C     S1OPZ         WHENEQ    41
224600961121     c*controllo chiamata ricorsiva p.nota
224700961121     c                   reset                   x48chPgmds
224800961121     C                   eval      x48pgm = 'NDC001R'
224900961121     c                   eval      kpjbu = x48chpgmds
225000961121     c                   call      'X48CHPGM'
225100961121     c                   parm                    kpjba
225200961121     c                   movel     kpjbu         x48chpgmds
225300961121     c                   if        x48errore = *off
225400951205      *
225500951205     C                   RESET                   NDC001DS
225600951205     C                   MOVEL     '02'          OPZ001
225700951205     C* passare nei campi appositi della DS C004DS il valore
225800951205     C* della chiave della VL univoca
225900951205     C                   MOVEL     H1Sys         UQ1001
226000951205     C                   MOVEL     H1NrAsReg     UQ2001
226100951205     C*
226200951205     C                   movel     NDC001DS      KPJBU
226300951205     C                   CALL      'NDC001R'
226400951205     C                   PARM                    Kpjba
226500951205     C                   movel     KPJBU         ndc001ds
226600951205     C* ritorno da PGM gestione
226700951205     C                   EXSR      RTN001
226800961121     c                   endif
226900951128     C*
227000951128     C* se opz=44 o 45 descrizioni aggiuntive
227100951128     C* passo la chiave al pgm chiamante
227200951128     C     S1OPZ         WHENEQ    44
227300951128     C     S1OPZ         oreq      45
227400951128     C                   RESET                   NDC027DS
227500951128     C*
227600951128     C                   MOVEL     '02'          OPZ027
227700951128     C* passare nei campi appositi della DS NDC027DS il valore
227800951128     C* della chiave della VL univoca
227900951128     C                   MOVEL     H1sys         Sys027
228000970605     C                   z-add     h1nrasreg     NrAsReg027
228100951128     c                   if        S1opz = 45
228200970605     C                   z-add     h1nrrigam     NrRigaM027
228300951128     C                   MOVEL     'M'           TipDes027
228400951128     C                   else
228500951128     C                   MOVEL     'T'           TipDes027
228600951128     C                   endif
228700951128     C* Faccio eseguire il commit al pgm chiamato
228800951128     C                   MOVE      '0'           Cmt027
228900951128     C                   MOVEL     *on           Inz027
229000951128     C*
229100951128     C* passo tutti quei campi che possono servire al pgm
229200951128     C                   MOVEL     NDC027DS      KPJBU
229300951128     C                   CALL      'NDC027R'
229400951128     C                   PARM                    KPJBA
229500951128     C                   MOVEL     KPJBU         NDC027DS
229600951128     C* ritorno da PGM gestione
229700951128     C                   EXSR      RTN027
229800970724     C*
229900970724     C* se opz=Riconciliazione
230000970724     C     S1OPZ         WHENEQ    50
230100970724     c                   Reset                   NdcT43Ds
230200970724     c                   movel     XScSoc        SocietaT43
230300970724     c                   movel     C1Banca       BancaT43
230400970724     c                   movel     C1Linea       LineaT43
230500970724     c                   Z-add     H1NrTeso      NrRicT43
230600970724     C                   eval      WdwT43     =  WrkWdwT43
230700970724     C                   eval      kpjbu = ndct43ds
230800970724     C                   CALL      'NDCT43R'
230900970724     C                   PARM                    KPJBA
231000970724     C                   movel     kpjbu         NdcT43Ds
231100970724     C* ritorno da PGM
231200970724     C                   EXSR      RTNT43
231300970724     C*
231400970724     C                   MOVE      *HIVAL        $LASTV
231500970613     C*
231600970723     C* se opz=51 richiamo programma per de-riconciliare
231700970613     C     S1OPZ         WHENEQ    51
231800970613     C*
231900970613     C                   RESET                   NDCt15DS
232000970613     C                   MOVE      '01'          OpzT15
232100970613     C                   move      xscsoc        SocietaT15
232200970613     c                   move      C1Banca       BancaT15
232300970613     c                   move      C1Linea       LineaT15
232400970613     C                   z-add     H1NrTeso      NrTesoT15
232500970613     C*
232600970613     C                   movel     NDCt15DS      KPJBU
232700970613     C                   CALL      'NDCT15R'
232800970613     C                   PARM                    Kpjba
232900970613     C                   Movel     kpjbu         NDCT15DS
233000970613     C*
233100970613     C                   Exsr      Rtnt15
233200970710     c                   if        errT15 = '4'
233300970710     C                   Z-add     nrtesoT15     Pack0315
233400970710     c                   endif
233500970724     c*
233600970724     C                   MOVE      *HIVAL        $LASTV
233700970724     C*
233800970724     C* se opz=52 richiamo programma per riconciliare
233900970724     C     S1OPZ         WHENEQ    52
234000970724     C*
234100970724     C                   RESET                   NDCt14DS
234200970724     C                   MOVE      '01'          OpzT14
234300970724     C                   MOVEL     H1Sys         Syst14
234400970724     C                   z-add     H1NrAsReg     NrAsRegT14
234500970724     C                   z-add     H1NrRigaM     NrRigaMT14
234600970724     C*
234700970724     C                   movel     NDCt14DS      KPJBU
234800970724     C                   CALL      'NDCT14R'
234900970724     C                   PARM                    Kpjba
235000970724     C                   Movel     kpjbu         NDCT14DS
235100970724     C*
235200970724     C                   Exsr      Rtnt14
235300940225     C*
235400950620     C* se altre opzioni
235500950620     C* richiamo pgm gestione anagrafica
235600950907     C* anche per opzione blocco/sblocco, dove eseguirò solo aggiornamento
235700950907     C* archivio
235800941027     C                   OTHER
235900940131     C*
236000940506     C                   MOVE      *HIVAL        $LASTV
236100950907     C*
236200950907     C*
236300941027     C                   ENDSL
236400940225     C*
236500940131     C                   ENDSR
236600951128     C/EJECT
236700951128     C************************************************************
236800951128     C* GESTIONE RITORNO DA PGM DESCRIZIONI AGGIUNTIVE
236900951128     C************************************************************
237000951128     C     RTN027        BEGSR
237100951128     C*
237200951128     C* modo di ritorno
237300951128     C*
237400951128   B1C                   SELECT
237500951128     C* 1 = F3
237600951128     C     RET027        WHENEQ    '1'
237700951128     C                   MOVE      *ON           $ESCI
237800951128     C                   MOVE      *ON           $FINE
237900970416     C                   MOVE      '1'           RETT11
238000951128     C* 2 = F12
238100951128     C     RET027        WHENEQ    '2'
238200951128     C                   MOVE      *ON           $ESCI
238300951128     C*
238400951128   E1C                   ENDSL
238500951128     C*
238600951128     C* operazione eseguite dal pgm chiamato
238700951128     C*
238800951128    >C                   MOVE      OPR027        WOPR
238900951128     C                   SELECT
239000951128     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
239100951128     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
239200951128    >C     OPR027        WHENEQ    '1'
239300951128     C* verifico se esistono descrizioni aggiuntive(riga)
239400951128     C* non lo faccio perchè non verranno quasi mai modificate
239500951128     C*****              EXSR      TestDesAGG
239600951128     C*
239700951128     C                   ENDSL
239800951128     C*
239900951128     C* funzione non eseguibile per errore :
240000951128     C*
240100951128     C                   SELECT
240200951128     C* 1 = funzione richiamata chiusa in errore
240300951128    >C     ERR027        WHENEQ    '1'
240400951128     C                   MOVE      *ON           $ESCI
240500951128     C                   SETON                                        9599
240600951128     C* 2 = funzione richiamata senza la dovuta autorizzazione
240700951128    >C     ERR027        WHENEQ    '2'
240800951128     C                   MOVE      *ON           $ESCI
240900951128     C                   SETON                                        9499
241000951128     C*
241100951128     C                   ENDSL
241200951128     C*
241300951128     C                   ENDSR
241400951128     C/EJECT
241500951128     C************************************************************
241600951128     C* GESTIONE RITORNO DA PGM primanota generica
241700951128     C************************************************************
241800951128     C     RTN001        BEGSR
241900951128     C*
242000951128     C* modo di ritorno
242100951128     C*
242200951128   B1C                   SELECT
242300951128     C* 1 = F3
242400951128     C     RET001        WHENEQ    '1'
242500951128     C                   MOVE      *ON           $ESCI
242600951128     C                   MOVE      *ON           $FINE
242700970416     C                   MOVE      '1'           RETT11
242800951128     C* 2 = F12
242900951128     C     RET001        WHENEQ    '2'
243000960125     c     opr001        andne     '1'
243100951128     C                   MOVE      *ON           $ESCI
243200951128     C*
243300951128   E1C                   ENDSL
243400951128     C*
243500951128     C* operazione eseguite dal pgm chiamato
243600951128     C*
243700951128    >C                   MOVE      OPR001        WOPR
243800951128     C                   SELECT
243900951128     C* 1 = eseguito aggiornamento (richiesto cariamento subfile
244000951128     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
244100951128    >C     OPR001        WHENEQ    '1'
244200951128     C                   move      *ON           winzs1
244300970422     c* richiamo routine per riaggiornare il saldo finale
244400970422     c* richiamo routine per scrivere saldi totali
244500970422     c                   exsr      CallSaldi
244600951128     C*
244700951128     C                   ENDSL
244800951128     C*
244900951128     C* funzione non eseguibile per errore :
245000951128     C*
245100951128     C                   SELECT
245200951128     C* 1 = funzione richiamata chiusa in errore
245300951128    >C     ERR001        WHENEQ    '1'
245400951128     C                   MOVE      *ON           $ESCI
245500951128     C                   SETON                                        9599
245600951128     C* 2 = funzione richiamata senza la dovuta autorizzazione
245700951128    >C     ERR001        WHENEQ    '2'
245800951128     C                   MOVE      *ON           $ESCI
245900951128     C                   SETON                                        9499
246000951128     C* 3 = registrazione non modificabile
246100951128     C     ERR001        WHENEQ    '3'
246200951128     C                   MOVE      *ON           $ESCI
246300951128     C                   SETON                                        9399
246400951128     C*
246500951128     C                   ENDSL
246600951128     C*
246700951128     C*
246800951128     C                   ENDSR
246900960318     C/EJECT
247000960318     C************************************************************
247100960318     C* GESTIONE RITORNO DA PGM partite
247200960318     C************************************************************
247300960318     C     RTN073        BEGSR
247400960318     C*
247500960318     C* modo di ritorno
247600960318     C*
247700960318   B1C                   SELECT
247800960318     C* 1 = F3
247900960318     C     RET073        WHENEQ    '1'
248000960318     C                   MOVE      *ON           $ESCI
248100960318     C                   MOVE      *ON           $FINE
248200970416     C                   MOVE      '1'           RETT11
248300960318     C* 2 = F12
248400960318     C     RET073        WHENEQ    '2'
248500960318     C                   MOVE      *ON           $ESCI
248600960318     C*
248700960318   E1C                   ENDSL
248800960318     C*
248900960318     C* operazione eseguite dal pgm chiamato
249000960318     C*
249100960318    >C                   MOVE      OPR073        WOPR
249200960318     C                   SELECT
249300960318     C* 1 = eseguito aggiornamento
249400960318    >C     OPR073        WHENEQ    '1'
249500960318     C                   move      *ON           winzs1
249600960318     C*
249700960318     C                   ENDSL
249800960318     C*
249900960318     C* funzione non eseguibile per errore :
250000960318     C*
250100960318     C                   SELECT
250200960318     C* 1 = funzione richiamata chiusa in errore
250300960318    >C     ERR073        WHENEQ    '1'
250400960318     C                   MOVE      *ON           $ESCI
250500960318     C                   SETON                                        9599
250600960318     C* 2 = funzione richiamata senza la dovuta autorizzazione
250700960318    >C     ERR073        WHENEQ    '2'
250800960318     C                   MOVE      *ON           $ESCI
250900960318     C                   SETON                                        9499
251000960318     C*
251100960318     C                   ENDSL
251200960318     C*
251300960318     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
251400960318     C* di sola finestra
251500960318     C                   MOVEL     '0'           winz073
251600960318     C*
251700960318     C                   ENDSR
251800951128     C/EJECT
251900951128     C************************************************************
252000951128     C* GESTIONE RITORNO DA PGM tesoreria
252100951128     C************************************************************
252200951128     C     RTN084        BEGSR
252300951128     C*
252400951128     C* modo di ritorno
252500951128     C*
252600951128   B1C                   SELECT
252700951128     C* 1 = F3
252800951128     C     RET084        WHENEQ    '1'
252900951128     C                   MOVE      *ON           $ESCI
253000951128     C                   MOVE      *ON           $FINE
253100970416     C                   MOVE      '1'           RETT11
253200951128     C* 2 = F12
253300951128     C     RET084        WHENEQ    '2'
253400951128     C                   MOVE      *ON           $ESCI
253500951128     C*
253600951128   E1C                   ENDSL
253700951128     C*
253800951128     C* operazione eseguite dal pgm chiamato
253900951128     C*
254000951128    >C                   MOVE      OPR084        WOPR
254100951128     C                   SELECT
254200960129     C* 1 = eseguito aggiornamento
254300951128    >C     OPR084        WHENEQ    '1'
254400960129     C                   move      *ON           winzs1
254500951128     C*
254600951128     C                   ENDSL
254700951128     C*
254800951128     C* funzione non eseguibile per errore :
254900951128     C*
255000951128     C                   SELECT
255100951128     C* 1 = funzione richiamata chiusa in errore
255200951128    >C     ERR084        WHENEQ    '1'
255300951128     C                   MOVE      *ON           $ESCI
255400951128     C                   SETON                                        9599
255500951128     C* 2 = funzione richiamata senza la dovuta autorizzazione
255600951128    >C     ERR084        WHENEQ    '2'
255700951128     C                   MOVE      *ON           $ESCI
255800951128     C                   SETON                                        9499
255900951128     C*
256000951128     C                   ENDSL
256100960105     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
256200960105     C* di sola finestra
256300960105     C                   MOVEL     '0'           wexf084
256400951128     C*
256500951128     C                   ENDSR
256600951205     C*
256700970605     C/EJECT
256800040212C1718C************************************************************
256900040212     C* GESTIONE RITORNO DA PGM cash flow
257000040212C1718C************************************************************
257100040212     C     RTNT68        BEGSR
257200040212     C*
257300040212     C* modo di ritorno
257400040212     C*
257500040212   B1C                   SELECT
257600040212     C* 1 = F3
257700040212     C     RETt68        WHENEQ    '1'
257800040212     C                   MOVE      *ON           $ESCI
257900040212     C                   MOVE      *ON           $FINE
258000040212     C                   MOVE      '1'           RETT11
258100040212     C* 2 = F12
258200040212     C     RETt68        WHENEQ    '2'
258300040212     C                   MOVE      *ON           $ESCI
258400040212     C*
258500040212   E1C                   ENDSL
258600040212     C*
258700040212     C* operazione eseguite dal pgm chiamato
258800040212     C*
258900040212    >C                   MOVE      OPRt68        WOPR
259000040212     C                   SELECT
259100040212     C* 1 = eseguito aggiornamento
259200040212    >C     OPRt68        WHENEQ    '1'
259300040212     C                   move      *ON           winzs1
259400040212     C*
259500040212     C                   ENDSL
259600040212     C*
259700040212     C* funzione non eseguibile per errore :
259800040212     C*
259900040212     C                   SELECT
260000040212     C* 1 = funzione richiamata chiusa in errore
260100040212    >C     ERRt68        WHENEQ    '1'
260200040212     C                   MOVE      *ON           $ESCI
260300040212     C                   SETON                                        9599
260400040212     C* 2 = funzione richiamata senza la dovuta autorizzazione
260500040212    >C     ERRt68        WHENEQ    '2'
260600040212     C                   MOVE      *ON           $ESCI
260700040212     C                   SETON                                        9499
260800040212     C*
260900040212     C                   ENDSL
261000040212     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
261100040212     C* di sola finestra
261200040212     C                   MOVEL     '0'           wexft68
261300040212     C*
261400040212c1718C                   ENDSR
261500051205C1886C************************************************************
261600051205     C* GESTIONE Ritorno da pgm home banking
261700051205C1886C************************************************************
261800051205     C     RTNK40        BEGSR
261900051205     C*
262000051205     C* modo di ritorno
262100051205     C*
262200051205   B1C                   SELECT
262300051205     C* 1 = F3
262400051205     C     RETk40        WHENEQ    '1'
262500051205     C                   MOVE      *ON           $ESCI
262600051205     C                   MOVE      *ON           $FINE
262700051205     C                   MOVE      '1'           RETT11
262800051205     C* 2 = F12
262900051205     C     RETk40        WHENEQ    '2'
263000051205     C                   MOVE      *ON           $ESCI
263100051205     C*
263200051205   E1C                   ENDSL
263300051205     C*
263400051205     C* operazione eseguite dal pgm chiamato
263500051205     C*
263600051205    >C                   MOVE      OPRk40        WOPR
263700051205     C                   SELECT
263800051205     C* 1 = eseguito aggiornamento
263900051205    >C     OPRk40        WHENEQ    '1'
264000051205     C                   move      *ON           winzs1
264100051205     C*
264200051205     C                   ENDSL
264300051205     C*
264400051205     C* funzione non eseguibile per errore :
264500051205     C*
264600051205     C                   SELECT
264700051205     C* 1 = funzione richiamata chiusa in errore
264800051205    >C     ERRk40        WHENEQ    '1'
264900051205     C                   MOVE      *ON           $ESCI
265000051205     C                   SETON                                        9599
265100051205     C* 2 = funzione richiamata senza la dovuta autorizzazione
265200051205    >C     ERRk40        WHENEQ    '2'
265300051205     C                   MOVE      *ON           $ESCI
265400051205     C                   SETON                                        9499
265500051205     C*
265600051205     C                   ENDSL
265700051205     C*
265800051205c1886C                   ENDSR
265900970605     C************************************************************
266000970613     C* GESTIONE RITORNO DA PGM riconcilia
266100970605     C************************************************************
266200970605     C     RTNT14        BEGSR
266300970605     C*
266400970605     C* operazione eseguite dal pgm chiamato
266500970605     C*
266600970605    >C                   MOVE      OPRT14        WOPR
266700970605     C                   SELECT
266800970605     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
266900970605     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
267000970605    >C     OPRt14        WHENEQ    '1'
267100970605     C                   move      *ON           winzs1
267200970605     C*
267300970605     C                   ENDSL
267400970605     C*
267500970605     C* funzione non eseguibile per errore :
267600970605     C*
267700970605     C                   SELECT
267800970605     C* 1 = funzione richiamata chiusa in errore
267900970605    >C     ERRt14        WHENEQ    '1'
268000970605     C                   MOVE      *ON           $ESCI
268100970605     C                   SETON                                        9599
268200970605     C* 2 = funzione richiamata senza la dovuta autorizzazione
268300970605    >C     ERRT14        WHENEQ    '2'
268400970605     C                   MOVE      *ON           $ESCI
268500970605     C                   SETON                                        9499
268600970605     C* 3 = Riconciliazione non possibile
268700970605    >C     ERRT14        WHENEQ    '3'
268800970605     C                   MOVE      *ON           $ESCI
268900970605     C                   SETON                                        8899
269000970605     C*
269100970605     C                   ENDSL
269200970605     C*
269300970605     C                   ENDSR
269400970613     C/EJECT
269500970613     C************************************************************
269600970613     C* GESTIONE RITORNO DA PGM reset riconcilia
269700970613     C************************************************************
269800970613     C     RTNT15        BEGSR
269900970613     C*
270000970613     C* operazione eseguite dal pgm chiamato
270100970613     C*
270200970613    >C                   MOVE      OPRT15        WOPR
270300970613     C                   SELECT
270400970613     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
270500970613     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
270600970613    >C     OPRt15        WHENEQ    '1'
270700970613     C                   move      *ON           winzs1
270800970613     C*
270900970613     C                   ENDSL
271000970613     C*
271100970613     C* funzione non eseguibile per errore :
271200970613     C*
271300970613     C                   SELECT
271400970613     C* 1 = funzione richiamata chiusa in errore
271500970613    >C     ERRt15        WHENEQ    '1'
271600970613     C                   MOVE      *ON           $ESCI
271700970613     C                   SETON                                        9599
271800970613     C* 2 = funzione richiamata senza la dovuta autorizzazione
271900970613    >C     ERRT15        WHENEQ    '2'
272000970613     C                   MOVE      *ON           $ESCI
272100970613     C                   SETON                                        9499
272200970613     C* 3 = Reset riconciliazione non possibile
272300970613    >C     ERRT15        WHENEQ    '3'
272400970613     C                   MOVE      *ON           $ESCI
272500970613     C                   SETON                                        8999
272600970704     C* 4 = De-riconciliazione non riuscita per data blocco
272700970704    >C     ERRT15        WHENEQ    '4'
272800970704     C                   MOVE      *ON           $ESCI
272900970704     C                   SETON                                        8799
273000970613     C*
273100970613     C                   ENDSL
273200970613     C*
273300970613     C                   ENDSR
273400970724     C/EJECT
273500970724     C************************************************************
273600970724     C* GESTIONE RITORNO da roconciliazione
273700970724     C************************************************************
273800970724    >C     RTNT43        BEGSR
273900970724     C*
274000970724     C* modo di ritorno
274100970724     C*
274200970724     C                   SELECT
274300970724     C* questi modi di utilizzo dei valori di ritorno dal
274400970724     C* pgm di manutenzione rcd di anagrafica sono delle
274500970724     C* proposte, normalmente sempre valide, ma modificabili
274600970724     C* per situazioni particolari
274700970724     C* 1 = F3
274800970724    >C     RetT43        WHENEQ    '1'
274900970724     C                   MOVE      *ON           $ESCI                          sospese READC
275000970724     C                   MOVE      *ON           $FINE                          fine pgm
275100970724     C                   MOVE      '1'           RETT11
275200970724     C* 2 = F12
275300970724    >C     RETT43        WHENEQ    '2'
275400970724     C* se il pgm di manutenzione ha prima una guida, è necessario anche
275500970724     c* questo test, altrimenti prevale il retT41 e il sfl non viene
275600970724     C* ricaricato
275700970724     C     OPRT43        ANDNE     '1'
275800970724     C                   MOVE      *ON           $ESCI                          sospese READC
275900970724     C*
276000970724     C                   ENDSL
276100970724     C                   MOVE      '0'           WrkWdwT43
276200970724     C*
276300970724     C                   ENDSR
276400970724     C/EJECT
276500950301     C/EJECT
276600950301     C************************************************************
276700950301     C* AGGIORNAMENTO RIGA LISTA
276800950301     C************************************************************
276900950301     C     AGGS1         BEGSR
277000950301     C*
277100950301     C                   ENDSR
277200941205     C/EJECT
277300941205     C************************************************************
277400941205     C* CONTROLLO ABILITAZIONE TASTI
277500941205     C* INPUT
277600941205     C* $TASTO =Tasto funzione premuto
277700941205     C* $FC = Tasti abilitati
277800941205     C* OUTPUT
277900941205     C* IN99 = ERrore
278000941205     C************************************************************
278100941205     C     CTRKEY        BEGSR
278200941205     C*
278300941205     C                   SELECT
278400941205     C     $TASTO        WHENEQ    F01
278500941205     C     $FC(1)        IFEQ      '0'
278600941205     C                   SETON                                        99
278700941205     C                   ENDIF
278800941205     C     $TASTO        WHENEQ    F02
278900941205     C     $FC(2)        IFEQ      '0'
279000941205     C                   SETON                                        99
279100941205     C                   ENDIF
279200941205     C     $TASTO        WHENEQ    F03
279300941205     C     $FC(3)        IFEQ      '0'
279400941205     C                   SETON                                        99
279500941205     C                   ENDIF
279600941205     C     $TASTO        WHENEQ    F04
279700941205     C     $FC(4)        IFEQ      '0'
279800941205     C                   SETON                                        99
279900941205     C                   ENDIF
280000941205     C     $TASTO        WHENEQ    F05
280100941205     C     $FC(5)        IFEQ      '0'
280200941205     C                   SETON                                        99
280300941205     C                   ENDIF
280400941205     C     $TASTO        WHENEQ    F06
280500941205     C     $FC(6)        IFEQ      '0'
280600941205     C                   SETON                                        99
280700941205     C                   ENDIF
280800941205     C     $TASTO        WHENEQ    F07
280900941205     C     $FC(7)        IFEQ      '0'
281000941205     C                   SETON                                        99
281100941205     C                   ENDIF
281200941205     C     $TASTO        WHENEQ    F08
281300941205     C     $FC(8)        IFEQ      '0'
281400941205     C                   SETON                                        99
281500941205     C                   ENDIF
281600941205     C     $TASTO        WHENEQ    F09
281700941205     C     $FC(09)       IFEQ      '0'
281800941205     C                   SETON                                        99
281900941205     C                   ENDIF
282000941205     C     $TASTO        WHENEQ    F10
282100941205     C     $FC(10)       IFEQ      '0'
282200941205     C                   SETON                                        99
282300941205     C                   ENDIF
282400941205     C     $TASTO        WHENEQ    F11
282500941205     C     $FC(11)       IFEQ      '0'
282600941205     C                   SETON                                        99
282700941205     C                   ENDIF
282800941205     C     $TASTO        WHENEQ    F12
282900941205     C     $FC(12)       IFEQ      '0'
283000941205     C                   SETON                                        99
283100941205     C                   ENDIF
283200941205     C     $TASTO        WHENEQ    F13
283300941205     C     $FC(13)       IFEQ      '0'
283400941205     C                   SETON                                        99
283500941205     C                   ENDIF
283600941205     C     $TASTO        WHENEQ    F14
283700941205     C     $FC(14)       IFEQ      '0'
283800941205     C                   SETON                                        99
283900941205     C                   ENDIF
284000941205     C     $TASTO        WHENEQ    F15
284100941205     C     $FC(15)       IFEQ      '0'
284200941205     C                   SETON                                        99
284300941205     C                   ENDIF
284400941205     C     $TASTO        WHENEQ    F16
284500941205     C     $FC(16)       IFEQ      '0'
284600941205     C                   SETON                                        99
284700941205     C                   ENDIF
284800941205     C     $TASTO        WHENEQ    F17
284900941205     C     $FC(17)       IFEQ      '0'
285000941205     C                   SETON                                        99
285100941205     C                   ENDIF
285200941205     C     $TASTO        WHENEQ    F18
285300941205     C     $FC(18)       IFEQ      '0'
285400941205     C                   SETON                                        99
285500941205     C                   ENDIF
285600941205     C     $TASTO        WHENEQ    F19
285700941205     C     $FC(19)       IFEQ      '0'
285800941205     C                   SETON                                        99
285900941205     C                   ENDIF
286000941205     C     $TASTO        WHENEQ    F20
286100941205     C     $FC(20)       IFEQ      '0'
286200941205     C                   SETON                                        99
286300941205     C                   ENDIF
286400941205     C     $TASTO        WHENEQ    F21
286500941205     C     $FC(21)       IFEQ      '0'
286600941205     C                   SETON                                        99
286700941205     C                   ENDIF
286800941205     C     $TASTO        WHENEQ    F22
286900941205     C     $FC(22)       IFEQ      '0'
287000941205     C                   SETON                                        99
287100941205     C                   ENDIF
287200941205     C     $TASTO        WHENEQ    F23
287300941205     C     $FC(23)       IFEQ      '0'
287400941205     C                   SETON                                        99
287500941205     C                   ENDIF
287600941205     C     $TASTO        WHENEQ    F24
287700941205     C     $FC(24)       IFEQ      '0'
287800941205     C                   SETON                                        99
287900941205     C                   ENDIF
288000941205     C                   ENDSL
288100941205     C*
288200941205     C                   ENDSR
288300940223     C/EJECT
288400940131     C************************************************************
288500940131     C* OPERAZIONI INIZIALI
288600940131     C************************************************************
288700940131     C     *INZSR        BEGSR
288800000117C1309C     *DMY          Move      Udate         ZDateISO
288900000117C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
289000940131     C*
289100940127     C* Reperimento parametri
289200940127     C*
289300940117     C     *ENTRY        PLIST
289400940117     C                   PARM                    KPJBA
289500970416     c                   PARM                    ndcT11ds
289600951109     C                   PARM                    SOC001
289700950410     C                   PARM                    DSAUT
289800950410     C*
289900951110     C     PARMS         IFLT      4
290000950410     C                   MOVE      *BLANK        AUTDS
290100950410     C                   ELSE
290200950410     C                   MOVEL     DSAUT         AUTDS
290300950410     C                   ENDIF
290400951108     C*
290500951108     C* Reperimento autorizzazioni
290600951108     C* le autorizzazioni potrebbero essere già state passate,
290700951108     C* nel qual caso non vanno reperite
290800951108    >C     AUTDS         IFEQ      *BLANK
290900951108    >C                   EXSR      REPAUT
291000960930     c                   else
291100970416     c                   movel     xcadds        conTESds
291200951108     C                   ENDIF
291300940225     C*
291400940223     C* Reperimento opzioni
291500940127     C*
291600940128     C                   EXSR      REPOPZ
291700950406     C*
291800940506     C* Reperimento tasti di funzione
291900940506     C*
292000940506     C                   EXSR      REPKEY
292100951109     C* Udate in iso
292200951109     C                   CALLb     'XDT4000'
292300951109     C     WDATE         PARM                    XDTAMG
292400000117C1309C*****              PARM      UDATE         XDTGMA
292500000117C1309C                   PARM      ZDate         XDTGMA
292600951109     C                   PARM      4             XDTSTA
292700970422     c*
292800970422     C* recupero le costanti per la riga da emettere
292900970422     C                   MOVEL     *BLANK        Ripresa          30
293000970422     C                   MOVEL     RipresaC      IDMSG
293100970422     C                   CALLB     'XRTVM'
293200970422     C                   PARM                    IDMSG            27
293300970422     C                   PARM                    TXTMSG          100
293400970422     C                   PARM                    ERRMSG            1
293500970422     C                   PARM      *Off          CTRMSG            1            centratura
293600970422     C                   PARM      30            LENMSG            3 0          lun output.
293700970422     C     ERRMSG        IFNE      *ON
293800970422     C                   MOVEL     TXTMSG        Ripresa
293900970422     C                   ELSE
294000970422     C                   MOVEL     *ALL'?'       Ripresa
294100970422     C                   ENDIF
294200940127     C*
294300940117     C                   ENDSR
294400970422     c/eject
294500970422     C************************************************************
294600970422     C* REPERIMENTO PARAMETRI GESTIONE
294700970422     C************************************************************
294800970422     C     REPPAR        BEGSR
294900970422     C*
295000970422     C                   CALLB     'XPAR'
295100970422     C                   PARM                    XPASOC            3
295200970422     C                   PARM                    XPAPAR            8
295300970422     C                   PARM                    XPAOUT
295400970422     C                   PARM                    XPAERR            1
295500970422     C                   PARM                    XPARIC            1
295600970422     C*
295700970422     C                   ENDSR
295800940223     C/EJECT
295900940223     C************************************************************
296000940223     C* INIZIALIZZAZIONE VARIABILI
296100940223     C************************************************************
296200940223     C     INZVAR        BEGSR
296300970429     C*
296400970429     C* Reperimento parametri gestione modulo tesoreria
296500970429     C* deve stare nell'inzvar perchè se si cambia società ed il programma
296600970429     c* si chiude in RT, DEVE rifare il controllo
296700970429     C*
296800970429     C                   MOVEL     *BLANK        XPARIC
296900970429     C                   MOVEL     XSCSOC        XPASOC
297000970429     C                   MOVEL     'STRTESO '    XPAPAR
297100970429     C                   MOVEL     *BLANK        XPAOUT
297200970429     C                   EXSR      REPPAR
297300970429     C     XPAERR        IFEQ      '0'
297400970429     C                   MOVEL     XPAOUT        ANP021DS
297500970429     C                   ELSE
297600970429     C                   SETON                                        LR
297700970429     C                   RETURN
297800970429     C                   ENDIF
297900970417     c* reperisco dati conto
298000970422     c                   eval      Wkcc = kcct11
298100970422     c                   eval      Wksc = ksct11
298200970417     c                   exsr      CallMvc002
298300940506     C*
298400940506     C* Definizione delle opzioni utilizzabili a pgm
298500940506     C*
298600940506     C                   EXSR      DEFOPZ
298700950330     C*
298800940223     C* Variabili per gestione videate
298900940223     C*
299000940223     C                   MOVE      *OFF          $FINE
299100940223     C                   MOVE      *OFF          $INZS1
299200950606     C                   MOVE      *OFF          $EndFile
299300980119     C                   MOVE      *OFF          $ESCI
299400940223     C                   MOVE      *OFF          $RCDOK
299500940509     C                   MOVE      *HIVAL        $LASTV
299600940506     C                   Z-ADD     0             $ULKS1            3 0
299700940318     C                   MOVE      *ON           $INZS1
299800951109     C                   MOVE      'S1'          $GEST
299900980130     C                   CLEAR                   WDare
300000980130     C                   CLEAR                   WAvere
300100980130     C                   CLEAR                   WDareD
300200980130     C                   CLEAR                   WAvereD
300300970417     C                   CLEAR                   Wrigsald
300400980119     C* se il conto in esame è valutario (la divisa del rapporto è <> m.c.)
300500980119     C                   MOVE      *off          *in08
300600980130    1C                   IF        DivBRAT11 <> XscDiv
300700980119     C* a seconda della divisa da visualizzare
300800980130    2c                   IF        DivVisT11 = '0'
300900980119     C* mostro i dati di saldo in m.c.
301000980119     C                   MOVE      *off          *in08
301100980130    2c                   ELSE
301200980119     C* oppure quelli nella divisa del c/valutario
301300980119     C                   MOVE      *on           *in08
301400980130    2C                   ENDIF
301500980130    1C                   ENDIF
301600951122     C*
301700951122     C* Valorizzazione campi univoci testate
301800951122     C*
301900970416     C                   CLEAR                   CT11T1
302000951122     C                   MOVEL     KNSIF         NOMSIF
302100951122     C                   MOVEL     XSCDSI        NOMDIT
302200951122     C                   MOVEL     DSPGM         NOMPGM
302300951109     C*
302400951109     C* visualizzo il sfl troncato
302500951109     C                   Eval      c1mode=*on
302600951113     C*
302700970416     C* il sottotitollo è la decodifica della modalita'
302800951113     C                   MOVE      *BLANK        T1tit
302900951113     C                   RESET                   X07DS
303000951113     C                   MOVE      '1'           X07RIC
303100951113     C                   MOVEL     'ANDIZ000'    X07TRC
303200970416     C                   MOVEL     'DSPMODTES'   X07CAM
303300970416     C                   MOVEL     MODALITT11    X07VAL
303400951113     C                   MOVE      *OFF          X07ALL
303500960924     C                   CALLB     'X07VALR'
303600951113     C                   PARM                    X07DS
303700980130   B1C     X07ERR        IFEQ      '0'
303800951113     C                   MOVEL     x07des        t1tit
303900951114     C                   MOVEl     t1tit         £STR            132
304000951114     C                   MOVE      30            £LSTRC            3 0
304100951114      * Routine di centratura
304200951114      *
304300951114     C     ' '           CHECKR    £STR          £LEN              3 0
304400951114     C     £LSTRC        SUB       £LEN          £LEN
304500951114     C                   DIV       2             £LEN
304600951114     C     ' '           CAT(P)    £STR:£LEN     £STRC           132
304700951114     C                   MOVEL     £strc         t1tit
304800980130     C*
304900980130   X1C                   Endif
305000951109     C*
305100980130     C* Reperimento dati moneta di conto e divisa di rapporto
305200951109     C                   Eval      WDivisa =  XscDiv
305300951109     C                   Exsr      RepDivisa
305400980121     C                   Eval      NrDecMC = X01NrDec
305500980130     C* se la divisa del rapporto NON è la m.c.
305600980130    1C                   IF        DivBRAT11 <> XscDiv
305700980130     C* se la divisa del rapporto non è l'ultima reperita
305800980130    2C                   IF        DivBRAT11 <> SavDivisa
305900980130     C* reperisco i dati della divisa
306000980130     C                   EVAL      SavDivisa = DivBRAT11
306100980130     C                   EVAL      WDivisa =  DivBRAT11
306200980130     C                   Exsr      RepDivisa
306300980130     C                   Eval      NrDecDiv = X01NrDec
306400980130    2C                   ENDIF
306500980130    1C                   ELSE
306600980130     C* i dati della divisa del rapporto sono gli stessi della m.c.
306700980130     C                   Eval      NrDecDiv = NrDecMC
306800980130    1C                   ENDIF
306900951108     C*
307000951108     C* Definisco ordinamento lettura
307100951108     C*
307200950410     C                   EXSR      DEFORD
307300980130     C*
307400951113     C* Definisco caricamento subfile
307500980130     C*
307600980130     C* i dati della divisa del saldo sono fissi
307700980130     C                   MOVEL     XscDiv        Z1Divisa
307800980130     C                   MOVEL     DivBRAT11     Z1DivisaD
307900970417     C* Se NON chiesti parametri particolari, imposto caricamento a pagina
308000970417     c* e i saldi li prendo dalla routine
308100970416     C                   if        SelPartT11 = *off
308200951108     C                   EVAL      $carpag = *on
308300951108     C                   EVAL      SFLPAG = 8
308400970422     c* richiamo routine per scrivere saldi totali
308500970422     c                   exsr      CallSaldi
308600951109     C*
308700951108     C                   else
308800951109     C*
308900951108     c                   eval      $carpag = *off
309000951108     C                   EVAL      SFLPAG = 9999
309100980130     C                   CLEAR                   Z1Saldo
309200980130     C                   CLEAR                   Z1Dare
309300980130     C                   CLEAR                   Z1Avere
309400980130     C                   CLEAR                   Z1Segno
309500980130     C                   CLEAR                   Z1SegnoD
309600980130     c                   CLEAR                   z1dareD
309700980130     c                   CLEAR                   z1avereD
309800980130     c                   CLEAR                   z1saldoD
309900980130     c                   CLEAR                   z1segnoD
310000951108     C                   endif
310100970422     c*
310200970422     c                   eval      z1darek = kdare021
310300970422     c                   eval      z1averek= kavere021
310400951108     C*
310500951108     C* Variabili appoggio
310600951108     C*
310700951108     C                   Z-ADD     SFLPAG        WPAG
310800951108     C                   CLEAR                   W1POS
310900951110     C* Salvo divisa in entrata
311000970416     C                   EVAL      SavDivVis =   DivVisT11
311100940223     C*
311200940223     C                   ENDSR
311300970422     C/EJECT
311400970422     C************************************************************
311500970422     C* Richiama routine calcolo saldi
311600970422     C************************************************************
311700970422     C     CallSaldi     BEGSR
311800970422     C*
311900980119     C                   EVAL      NrCicli = 1
312000980119     C* se il conto in gestione è valutario (divisa del rapporto <> m.c.)
312100980119    1C                   IF        DivBRAT11 <> XscDiv
312200980119     C* allora devo ciclare 2 volte per reperire anche i saldi in divisa
312300980119     C                   EVAL      NrCicli = 2
312400980119    1C                   ENDIF
312500980119     C*
312600980119    1C                   DO        NrCicli       x
312700980120     C*
312800980120     C                   EVAL      GiroDivisa = x
312900970422     c                   exsr      RieCt21ds
313000970422     c                   call      'NDCT21R'
313100970422     C                   PARM                    NDCT21DS
313200980121     C*
313300980121     C                   MOVEL     NDCT21DS      AppT21DS(x)
313400980121     C*
313500980121    2C                   if        erroreT21 = *off
313600980119     C*
313700980119     c* Modalità per Data valuta
313800980119    3C                   IF        ModalitT11 = '0'
313900980119     c* ciclo per m.c.
314000980119    4C                   IF        x = 1
314100980130     C                   Z-ADD     SlFiDVaT21    WDare
314200980130     C                   Z-ADD     SlFiAVaT21    WAvere
314300980119     c* ciclo per divisa
314400980119    4c                   ELSE
314500980130     C                   Z-add     SlFiDVaT21    WDareD
314600980130     C                   Z-add     SlFiAVaT21    WAvereD
314700980119    4c                   ENDIF
314800980119     c* Modalità per Data operazione
314900980119    3c                   ELSE
315000980119     c* ciclo per m.c.
315100980119    4C                   IF        x = 1
315200980130     C                   Z-add     SlFiDOpT21    WDare
315300980130     C                   Z-add     SlFiAOpT21    WAvere
315400980119     c* ciclo per divisa
315500980119    4c                   ELSE
315600980130     C                   Z-add     SlFiDOpT21    WDareD
315700980130     C                   Z-add     SlFiAOpT21    WAvereD
315800980119    4c                   ENDIF
315900980119    3c                   ENDIF
316000980130     C*
316100980130     C* predispongo il piede nei campi editabili
316200980130     C*  ciclo m.c.
316300980130     C                   IF        x = 1
316400980130     C                   EVAL      WNrDec = NrDecMC
316500980130     C*  Dare
316600980130     C                   Z-ADD     WDare         WImporto
316700980130     C                   EVAL      $LenFld = %Size(Z1Dare)
316800980130     C                   SUB       5             $LenFld
316900980130     C                   EXSR      Edit
317000980130     C                   Eval      Z1Dare  = $RcvrVar
317100980130     C*  Avere
317200980130     C                   Z-ADD     WAvere        WImporto
317300980130     C                   EVAL      $LenFld = %Size(Z1Avere)
317400980130     C                   EXSR      Edit
317500980130     C                   Eval      Z1Avere = $RcvrVar
317600980130     C*  ciclo divisa del rapporto
317700980130     C                   ELSE
317800980202     C*
317900980202     C* reperisco decimali se divisa diversa dalla precedente letta
318000980202     C                   IF        Z1DivisaD <> SavDivisa
318100980202     C                   EVAL      SavDivisa = Z1DivisaD
318200980202     C                   EVAL      WDivisa = Z1DivisaD
318300980202     C                   EXSR      RepDivisa
318400980202     C                   EVAL      NrDecDiv = X01NrDec
318500980202     C                   ENDIF
318600980130     C                   EVAL      WNrDec = NrDecDiv
318700980130     C*  Dare
318800980130     C                   Z-ADD     WDareD        WImporto
318900980130     C                   EVAL      $LenFld = %Size(Z1DareD)
319000980130     C                   SUB       5             $LenFld
319100980130     C                   EXSR      Edit
319200980130     C                   Eval      Z1DareD  = $RcvrVar
319300980130     C*  Avere
319400980130     C                   Z-ADD     WAvereD       WImporto
319500980130     C                   EVAL      $LenFld = %Size(Z1AvereD)
319600980130     C                   EXSR      Edit
319700980130     C                   Eval      Z1AvereD = $RcvrVar
319800980130     C                   ENDIF
319900980130     C*
320000980119    2C                   endif
320100980119    1C                   ENDDO
320200980119     C*
320300980121     C* calcolo segno e saldo (sia in m.c. che in divisa)
320400980121     C                   EXSR      ClcSegSal
320500980119     C*
320600980121     C* se il conto non è valutario
320700980121     C                   IF        NrCicli = 1
320800980121     C* ricopio i dati di saldo della m.c. anche per la divisa di rapporto
320900980121     C* che in questo caso è la stessa
321000980121     C                   MOVEL     AppT21DS(1)   AppT21DS(2)
321100980121    3c                   ENDIF
321200980121     C*
321300970422     c                   endsr
321400970417     C/EJECT
321500970417     C************************************************************
321600970417     C* Riempie dati della ds calcolo saldi
321700970417     C************************************************************
321800970417     C     RieCt21ds     BEGSR
321900970417     C*
322000970417     c                   reset                   NDCT21DS
322100970417     c                   eval      SocietaT21 = Xscsoc
322200970417     c                   eval      CtbT21     = CtbT11
322300970417     c                   eval      BancaT21   = BancaT11
322400970417     c                   eval      LineaT21   = LineaT11
322500980120     c***                eval      DivisaT21  = XscDiv
322600980120     C* al primo giro reperisco sempre il valore in m.c.
322700980120     C                   IF        GiroDivisa = 1
322800980120     C                   EVAL      DivisaT21  = XscDiv
322900980120     C* al secondo (se il conto è valutario) vengono reperiti i dati in
323000980120     C* divisa di rapporto bancario
323100980120     C                   ELSE
323200980120     C                   EVAL      DivisaT21  = DivBraT11
323300980120     C                   ENDIF
323400970417     c                   eval      DefinT21   = MovDefT11
323500970417     c                   eval      ProvvT21   = MovProvT11
323600970417     c                   eval      GestiT21   = MovGesT11
323700970417     c                   eval      DataDaT21  = DtPerDaT11
323800970417     c                   eval      DataAT21   = DtPerAT11
323900970417     c                   eval      RipresaT21 = RipSaldT11
324000970417     C*
324100970417     c                   endsr
324200951109     C/EJECT
324300951109     C************************************************************
324400951109     C* Reperisco dati divisa
324500951109     C************************************************************
324600951109     C     RepDivisa     BEGSR
324700951109     C*
324800951109     C* Divisa
324900951109     C                   reset                   X01CambiDs
325000951109     C                   MOVE      xscsoc        X01societa
325100951109     C                   MOVEL     WDivisa       X01divisa
325200951109     C                   MOVEL     '1'           X01flgcam
325300951109     C                   MOVE      Wdate         x01dtRifCa
325400951109     C                   CALL      'X01CAMBI'
325500951109     C                   PARM                    X01CambiDs
325600951109     C*
325700951109     C                   ENDSR
325800951109     c/EJECT
325900951109     C************************************************************
326000951109     C* Editazione di un numero
326100951109     C************************************************************
326200951109     C     Edit          Begsr
326300951109      *
326400951109     C                   Select
326500951110     C                   When      WNrDec = 0
326600951110     C                   Z-add     WImporto      $SrcVar
326700951110     C                   When      WNrDec = 1
326800951110     C     WImporto      Mult      10            $SrcVar
326900951110     C                   When      WNrDec = 2
327000951110     C     WImporto      Mult      100           $SrcVar
327100951109     C                   Other
327200951110     C     WImporto      Mult      1000          $SrcVar
327300951109     C                   Endsl
327400951109     C*
327500951110     C                   Eval      $SrcVarDe = WNrDec
327600951109     C*
327700951109     C                   CallB     'X23EDITR'
327800951109     C                   Parm                    $SrcVar
327900951109     C                   Parm                    $SrcVarPr
328000951109     C                   Parm                    $SrcVarDe
328100951109     C                   Parm                    $EdtCode
328200951109     C                   Parm                    $LenFld
328300951109     C                   Parm      *blanks       $RcvrVar
328400951109     C                   Parm                    $Err
328500951109      *
328600951109     C                   ENDSR
328700951109     C*
328800951108     C/EJECT
328900951108     C************************************************************
329000951108     C* DEFINIZIONE DELL'ORDINAMENTO
329100951108     C************************************************************
329200951108     C     DEFORD        BEGSR
329300951108     C*
329400951108     C* definisco l'ordinamento della lista a seconda dei valori
329500951108     C* impostati nel video di parzializzazione
329600951108     C*
329700951108     C                   SELECT
329800970416    >C     ORDT11        WHENEQ    'D'
329900951108     C                   Z-ADD     1             $ORD
330000970416    >C     ORDT11        WHENeq    'A'
330100951108     C                   Z-ADD     2             $ORD
330200951108     C                   ENDSL
330300951108     C*
330400951108     C                   ENDSR
330500941124     C/EJECT
330600941124     C************************************************************
330700941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
330800941124     C************************************************************
330900941124     C     REPAUT        BEGSR
331000941124     C*
331100941124     C                   CLEAR                   AUTDS
331200941124     C*
331300941124     C                   MOVEL     XSCSOC        XCASOC
331400941124     C                   MOVEL     XSCDUN        XCAUNI
331500941124     C                   Z-ADD     0             XCAGRP
331600941124     C                   MOVEL     KNMUS         XCAPRF
331700970416    >C                   MOVEL     'CONTES'      XCAFNC
331800941124    >C                   MOVEL     *ZERO         XCAVFU
331900941124    >C                   MOVE      'CK'          XCATCK
332000941124    >C                   MOVE      '1'           XCANAU
332100941124     C*
332200950522     C                   CALLB     'XCHKAUT'
332300941124     C                   PARM                    AUTDS
332400941124     C*
332500941124     C* errori/non abilitazione
332600941124     C     XCARTN        IFNE      0
332700941124     C                   EXSR      ENDPGM
332800941124     C                   ELSE
332900970416    >C                   MOVEL     XCADDS        CONtesDS
333000941124     C                   ENDIF
333100941124     C*
333200941124     C                   ENDSR
333300940223     C/EJECT
333400940131     C************************************************************
333500940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
333600940131     C************************************************************
333700940225     C     DEFOPZ        BEGSR
333800940131     C*
333900940225     C* Questa routine deve valorizzare la schiera $FL1
334000940225     C* con S o N a seconda della utilizzabilità o meno
334100940225     C* dell'opzione correlata
334200940225     C*
334300950301     C* Inizialmente si considerano tutte le opzioni come valide
334400940317     C     1             DO        $DIMOP        X
334500940225     C                   MOVE      'S'           $FL1(X)
334600941027     C                   ENDDO
334700940715     C*
334800951109     C* Questa routine deve valorizzare la schiera $FC1/$FR1
334900941018     C* con 1 o 0 a seconda della utilizzabilità o meno
335000940715     C* del tasto funzione correlato
335100940715     C*
335200940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
335300940715     C* di compilazione
335400940715     C                   RESET                   $FC1
335500940715     C                   RESET                   $FR1
335600940225     C*
335700940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
335800940225     C* invalido alcune opzioni
335900940225     C                   SELECT
336000940225     C* Se pgm richiamato in ricerca disattivo :
336100970416    >C     OPZT11        WHENEQ    '01'
336200940603     C* F3=Fine
336300940603     C                   Z-ADD     3             X
336400941216     C                   MOVE      '0'           $FC1(X)
336500950116     C*
336600951114     C* Se pgm richiamato in gestione       disattivo :
336700970416    >C     OPZT11        WHENEQ    '02'
336800940225     C* 1=Selezione
336900940225     C                   Z-ADD     1             X
337000940225     C     01            LOOKUP    $OP1(X)                                21
337100940225     C     *IN21         IFEQ      *ON
337200940225     C                   MOVE      'N'           $FL1(X)
337300940225     C                   ENDIF
337400940225     C*
337500950301     C* In tutti gli altri casi, disattivo subito :
337600941228     C                   OTHER
337700941228     C*
337800940225     C                   ENDSL
337900960318     c*
338000960318     C* Se conto non gestito a partite, disattivo opzione 22
338100960318     C* 22=partite
338200970416     C                   if        tpgest002  =   '0'
338300960318     C                   Z-ADD     1             X
338400960318     C     22            LOOKUP    $OP1(X)                                21
338500960318     C     *IN21         IFEQ      *ON
338600960318     C                   MOVE      'N'           $FL1(X)
338700960318     C                   ENDIF
338800960318     C                   endif
338900940225     C*
339000940225     C                   ENDSR
339100940225     C/EJECT
339200940225     C************************************************************
339300940225     C* REPERIMENTO OPZIONI
339400940225     C************************************************************
339500940225     C     REPOPZ        BEGSR
339600940225     C*
339700940314     C     1             DO        $DIMOP        X
339800940128     C*
339900940223     C     $MS1(X)       IFNE      *BLANK
340000940128     C*
340100940223     C                   MOVEL     $MS1(X)       IDMSG
340200950522     C                   CALLB     'XRTVM'
340300940201     C                   PARM                    IDMSG            27
340400940201     C                   PARM                    TXTMSG          100
340500940201     C                   PARM                    ERRMSG            1
340600940128     C*
340700940128     C     ERRMSG        IFNE      *ON
340800940223     C                   MOVEL     TXTMSG        $TX1(X)
340900940128     C                   ELSE
341000940223     C                   MOVEL     *ALL'?'       $TX1(X)
341100941027     C                   ENDIF
341200940128     C*
341300941027     C                   ENDIF
341400940128     C*
341500941027     C                   ENDDO
341600940128     C*
341700940128     C                   ENDSR
341800940506     C/EJECT
341900940506     C************************************************************
342000940506     C* REPERIMENTO TASTI DI FUNZIONE
342100940506     C************************************************************
342200940506     C     REPKEY        BEGSR
342300940506     C                   MOVEA     $FM1          $FM
342400950906     C                   MOVEA     $Fc1          $Fc
342500940506     C                   EXSR      REPKKK
342600940506     C                   MOVEA     $FX           $FX1
342700940506     C*
342800951109     C                   MOVEA     $FM1          $FM
342900951109     C                   MOVEA     $Fc1          $Fc
343000940506     C                   EXSR      REPKKK
343100951109     C                   MOVEA     $FX           $FX1
343200940919     C*
343300940919     C                   ENDSR
343400940506     C/EJECT
343500940506     C************************************************************
343600940506     C* REPERIMENTO TASTI DI FUNZIONE
343700940506     C************************************************************
343800940506     C     REPKKK        BEGSR
343900940506     C*
344000940506     C                   CLEAR                   $FX
344100940506     C     1             DO        24            X
344200940506     C*
344300940506     C     $FM(X)        IFNE      *BLANK
344400950906     C     $Fc(X)        andne     '0'
344500940506     C*
344600940506     C                   MOVEL     $FM(X)        IDMSG
344700950522     C                   CALLB     'XRTVM'
344800940506     C                   PARM                    IDMSG            27
344900940506     C                   PARM                    TXTMSG          100
345000940506     C                   PARM                    ERRMSG            1
345100940506     C*
345200940506     C     ERRMSG        IFNE      *ON
345300940506     C                   MOVEL     TXTMSG        $FX(X)
345400940506     C                   ELSE
345500940506     C                   MOVEL     *ALL' '       $FX(X)
345600941027     C                   ENDIF
345700940506     C*
345800941027     C                   ENDIF
345900940506     C*
346000941027     C                   ENDDO
346100940506     C*
346200940506     C                   ENDSR
346300940223     C/EJECT
346400940128     C************************************************************
346500950606     C* DEFINIZIONE KLIST
346600940128     C************************************************************
346700950606     C     DefKlist      BEGSR
346800940128     C*
346900940223     C* klist
347000940223     C*
347100970416    >C* ndmov01L
347200970416    >C     K03mov01      KLIST
347300970416    >C                   KFLD                    movSYS
347400970416    >C                   KFLD                    movNrasREg
347500970416    >C                   KFLD                    movNrrigam
347600940127     C*
347700970416    >C* ndbmv03l
347800970421    >C     K05bmv03      KLIST
347900970416    >C                   KFLD                    bmvSOCIETA
348000970421    >C                   KFLD                    bmvCTB
348100970416    >C                   KFLD                    bmvbanca
348200970416    >C                   KFLD                    bmvlinea
348300970416    >C                   KFLD                    bmvdtope
348400970421    >C     K04bmv03      KLIST
348500970416    >C                   KFLD                    bmvSOCIETA
348600970421    >C                   KFLD                    bmvCTB
348700970416    >C                   KFLD                    bmvbanca
348800970416    >C                   KFLD                    bmvlinea
348900970416     C*
349000970416    >C* ndbmv04l
349100970421    >C     K05bmv04      KLIST
349200970416    >C                   KFLD                    bmvSOCIETA
349300970421    >C                   KFLD                    bmvCTB
349400970416    >C                   KFLD                    bmvbanca
349500970416    >C                   KFLD                    bmvlinea
349600970416    >C                   KFLD                    bmvdtval
349700970421    >C     K04bmv04      KLIST
349800970421    >C                   KFLD                    bmvSOCIETA
349900970421    >C                   KFLD                    bmvCTB
350000970416    >C                   KFLD                    bmvbanca
350100970416    >C                   KFLD                    bmvlinea
350200951127     C*
350300951127     C* NDDES01L
350400951127     C     K04DES01      KLIST
350500951127     C                   KFLD                    DesSys
350600951127     C                   KFLD                    DesNrAsReg
350700951127     C                   KFLD                    DesTipDes
350800951127     C                   KFLD                    DesNrRigaM
350900951127     C     K03DES01      KLIST
351000951127     C                   KFLD                    DesSys
351100951127     C                   KFLD                    DesNrAsReg
351200951127     C                   KFLD                    DesTipDes
351300951127     C*
351400951127    >C* ndREG01L
351500951127    >C     K02reg01      KLIST
351600951127    >C                   KFLD                    Regsys
351700951127    >C                   KFLD                    regNrasreg
351800950704     C*
351900940117     C                   ENDSR
352000940223     C/EJECT
352100040315C1737c* Aggiunta opzione OPZ0503 25=CASH FLOW
352200941214**
352300940718PROMSG    *LIBL     OPZ0001
352400960318PROMSG    *LIBL     OPZ0108
352500951127PROMSG    *LIBL     OPZ0109
352600040212PROMSG    *LIBL     OPZ0503
352700051205PROMSG    *LIBL     OPZ0562                                      C1886
352800951127PROMSG    *LIBL     OPZ0041
352900951127PROMSG    *LIBL     OPZ0144
353000951127PROMSG    *LIBL     OPZ0145
353100970723PROMSG    *LIBL     OPZ0239
353200970723PROMSG    *LIBL     OPZ0240
353300970723PROMSG    *LIBL     OPZ0229
353400940201**
35350094022501S      <Selezione
35360096031822S      <Partite
35370095112723S      <Tesoreria
35380004021225S      <Cash flow
35390005120530S      <Movimento HB                                           C1886
35400095112741S      <Contabilità
35410095112744S      <Descriz.  testata
35420095112745S      <Descriz.  riga
35430097072350S      <Riconciliazione
35440097072351S      <De-Riconcilia
35450097072352S      <Riconcilia
354600940916** TASTI DI FUNZIONE  S1
354700940916PROMSG    *LIBL     KEY0001  01
354800940916PROMSG    *LIBL     KEY0002  02
354900940916PROMSG    *LIBL     KEY0003  03
355000940916PROMSG    *LIBL     KEY0004  04
355100940916PROMSG    *LIBL     KEY0005  05
355200940916PROMSG    *LIBL     KEY0006  06
355300940916PROMSG    *LIBL     KEY0007  07
355400940916PROMSG    *LIBL     KEY0008  08
355500940916PROMSG    *LIBL     KEY0009  09
355600940916PROMSG    *LIBL     KEY0010  10
355700951109PROMSG    *LIBL     KEY0031  11
355800940916PROMSG    *LIBL     KEY0012  12
355900940916PROMSG    *LIBL     KEY0013  13
356000940916PROMSG    *LIBL     KEY0014  14
356100951109PROMSG    *LIBL     KEY0053  15
356200940916PROMSG    *LIBL     KEY0016  16
356300940916PROMSG    *LIBL     KEY0017  17
356400940916PROMSG    *LIBL     KEY0018  18
356500940916PROMSG    *LIBL     KEY0019  19
356600970515PROMSG    *LIBL     KEY0116  20
356700940916PROMSG    *LIBL     KEY0021  21
356800940916PROMSG    *LIBL     KEY0022  22
356900940916PROMSG    *LIBL     KEY0023  23
357000940916PROMSG    *LIBL     KEY0024  24
357100940916**  ABILITAZIONE S1
3572009409160N  01
3573009409160N  02
3574009409161N  03
3575009409160N  04
3576009409160N  05
3577009409160N  06
3578009409160N  07
3579009511201N  08
3580009511081N  09
3581009507040N  10
3582009511081N  11
3583009409161N  12
3584009409160N  13
3585009409160N  14
3586009511081N  15
3587009409160N  16
3588009511080N  17
3589009511090N  18
3590009409160N  19
3591009705151N  20
3592009409160N  21
3593009409160N  22
3594009509131N  23   Impostare a '1' se si utilizzano opzioni
3595009509131N  24   Lasciare sempre impostata a '1' il valore della riga 24
359600970515**
359700970515PROMSG    *LIBL     COS0266
