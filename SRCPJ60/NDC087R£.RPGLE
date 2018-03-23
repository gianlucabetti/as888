000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300951108     H*PARMS CVTOPT(*DATETIME)
000400940211     H DECEDIT('0,') DATEDIT(*DMY.)
000500941115      *
000600951114    > *            Estratto conto contabile - DETTAGLIO PARTITE
000700951108      *           ----------------------------------------------
000800940128     F*----------------------------------------------------*
000900951113     F* Questo pgm può essere richiamato in 2 modi:
001000951114     F* 02 = Gestione
001100951113     F* 01 = Ricerca
001200940314    >F*----------------------------------------------------------------
001300940314    >F* Viste logiche utilizzate per i diversi ordinamenti
001400940314    >F*----------------------------------------------------------------
001500940909     F*------------
001600951114    >FNDPAS02J  IF   E           K DISK
001700951115     FNDDES01L  IF   E           K DISK
001800061123C1979FNDPSC01L  IF   E           K DISK
001900940211     F*------------
002000951114    >FNDC087D   CF   E             WORKSTN
002100960314     F                                     SFILE(C087S2:S1NRR)
002200960314     F                                     SFILE(C087S1:S1NRR)
002300940201     F                                     INFDS(DSFMT)
002400940128     D*----------------------------------------------------*
002500940302     D* Schiere per la gestione delle opzioni
002600940302     D*
002700950614     D* dimensione della schiera $MS1
002800011213C1530x*$DIMOP          C                   CONST(8)
002900020118C1530D $DIMOP          C                   CONST(30)
003000950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003100950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
003200950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003300950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
003400941214     D $TX             S             50    DIM(50)
003500941214     D $FL             S              1    DIM(50)
003600940909     D* Schiere generali per la gestione dei tasti di funzione
003700940506     D $FM             S             27    DIM(24)
003800940506     D $FX             S             50    DIM(24)
003900940506     D $FC             S              1    DIM(24)
004000940506     D $FR             S              1    DIM(24)
004100940909     D* Schiere per la gestione dei tasti di funzione
004200951108     D* Video S1
004300940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
004400940506     D $FX1            S             50    DIM(24) ALT($FM1)
004500940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
004600940506     D $FR1            S              1    DIM(24) ALT($FC1)
004700051216     D* Schiera costanti
004800051216C1892D $Cost           S             27    DIM(2) CTDATA PERRCD(1)
004900940302     D*
005000940211     D/EJECT
005100950524     D*-------------
005200940211     D* Passaggio Parametri
005300940211     D KPJBA         E DS
005400940211     D*-------------
005500940225     D* Reperimento Autorizzazioni
005600941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
005700941124     D*-------------
005800950410     D* Passaggio Autorizzazioni
005900950410     D DSAUT           DS           512
006000950524     D*-------------
006100950524     D* Maschera dati autorizzazioni
006200951108    >D CONECCDS      E DS
006300941209     D*-------------
006400941209     D* Dati di ambiente ottenuti da XSOC
006500941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
006600950906     D    FgO1                        1    overlay(XscFgO)
006700961009     D    XscDare                     1    overlay(xscFgo:7)                    Scadenz. retroattivo
006800961009     D    XscAvere                    1    overlay(xscFgo:8)                    Scadenz. retroattivo
006900011213C1530D    XscLaguna                   1    overlay(xscSkp:8)                    Scadenz. retroattivo
007000951109     D*-------------
007100951109     D* Parametri per PGM X01CAMBI
007200951109     D X01CambiDs    E DS                  inz
007300951109     D*-------------                       INZ
007400951109     D* Routine editazione di un numero
007500951110     D $SrcVar         s             30S 0
007600951110     D $SrcVarPr       s              9B 0 inz(30)
007700951109     D $SrcVarDe       s              9B 0
007800951117     D $EdtCode        s              1    inz('1')
007900951109     D $LenFld         s              3S 0
008000951109     D $RcvrVar        s             50
008100951109     D $Err            s              1
008200960314     D*---------------
008300960314     D* DS di anmpe
008400960314     D* Valori assunti
008500960314     D XMPEDS        E DS                  INZ
008600960314     D XMPEDS1       E DS                  INZ
008700960314     D DSMPE           DS                  INZ
008800960314     D  $modalit                      1
008900960314     D  $movdef                       1
009000960314     D  $movprov                      1
009100960314     D  $movges                       1
009200960314     D  $ord                          1
009300960314     D  $divvis                       1
009400960314     D  $divsel                       1
009500960314     D  $divisa                       4
009600960314     D  $estmul                       1
009700960314     D  $sitpar                       1
009800960314     D  $ripsaldo                     1
009900960314     D  $visdati                      1
010000981009     D*-------------
010100981009     D* Dati anagrafici del conto
010200981009     D ANA397DS      E DS                  inz
010300951115     D*-------------
010400951115     D* Interrogazione conti
010500951206     D ANA399DS      E DS                  inz
010600940325     D*-------------
010700940325     D* Parametri in ricezione
010800951114    >D NDC087DS      E DS
010900970805     D*-------------
011000970805     D* Controllo conti
011100970805     D ND002DS       E DS                  inz
011200020118C1530D*-------------
011300020116  "  D* dati catalogazione Laguna
011400020118C1530D ANLGN00F      E DS
011500970805     D*-------------
011600960319     D*schiera partite in ricerca
011700960319     D SkPartite       DS                  occurs(50)
011800960319     D  SkDtPar                            like(dtpar087)
011900960319     D  SkNrPar                            like(nrpar087)
012000960319     D  SkSerPar                           like(Serpar087)
012100960319     D  SkImportD                    19S 3
012200960319     D  SkDtSca                            like(dtpar087)
012300990115     D  SkSegnoD                      1
012400070205C1973D  SkImporto                    19S 3
012500070205C1973D  SkDivisa                           like(Divisa087)
012600951115     D*-------------
012700951115     D* Parametri del PGM descrizioni aggiuntive
012800951115     D NDC027DS      E DS                  INZ
012900951115     D  RET027       E                     INZ('0')
013000951115     D  OPR027       E                     INZ('0')
013100951115     D  ERR027       E                     INZ('0')
013200960318     D*-------------
013300960318      * Parametri per call pgm visualizzazione partite
013400960318     D NDC073DS      E DS                  INZ
013500960318     D  OPR073       E                     INZ('0')
013600960318     D  RET073       E                     INZ('0')
013700960318     D  ERR073       E                     INZ('0')
013800960318     D  Exf073       E                     INZ('0')
013900960318     D*-------------
014000961121     D* Chiamata ricorsiva
014100961121    >D X48CHPGMDS    E DS                  INZ
014200951115     D*-------------
014300951115     D* Parametri per gestione p.nota interattiva
014400951127     D NDC001DS      E DS                  INZ
014500951127     D  RET001       E                     INZ('0')
014600951127     D  OPR001       E                     INZ('0')
014700951127     D  ERR001       E                     INZ('0')
014800951205     D  DTREG1001    E                     inz('0001-01-01')
014900951205     D  DTREG2001    E                     inz('0001-01-01')
015000951205     D  DTLGIO1001   E                     inz('0001-01-01')
015100951205     D  DTLGIO2001   E                     inz('0001-01-01')
015200951205     D  DTULMO1001   E                     inz('0001-01-01')
015300951205     D  DTULMO2001   E                     inz('0001-01-01')
015400951127     D  DTDOC001     E                     inz('0001-01-01')
015500951115     D*-------------
015600951115      * Parametri per call diretta finestra analitica
015700951115     D NDC083DS      E DS                  INZ
015800951115     D  RET083       E                     INZ('0')
015900951115     D  ERR083       E                     INZ('0')
016000951115     D  Exf083       E                     INZ('0')
016100051109     D*-------------
016200051109c1881 * Parametri per call diretta finestra tesoreria
016300051109  "  D NDC084DS      E DS                  INZ
016400051109  "  D  RET084       E                     INZ('0')
016500051109  "  D  ERR084       E                     INZ('0')
016600051109c1881D  exf084       E                     INZ('0')
016700951219     D*-------------
016800951219     D* Parametri del PGM di effetti passivi NDCH01R
016900951219    >D NDCH01ds      E DS                  inz
017000951219    >D RETH01        E                     INZ('0')
017100951219    >D OPRH01        E                     INZ('0')
017200951219    >D ERRH01        E                     INZ('0')
017300980526    >D DTDISTH01     E                     INZ('0001-01-01')
017400951219     D*-------------
017500951219     D* Parametri del PGM di effetti attivii NDCE01r
017600951219    >D NDCE01ds      E DS                  inz
017700951219    >D RETe01        E                     INZ('0')
017800951219    >D OPRe01        E                     INZ('0')
017900951219    >D ERRe01        E                     INZ('0')
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
019300951115     D*-------------
019400951115     D* Definizioni parametri standard chiamate moduli
019500970805     D esito           S              1
019600951115     D gesmsg          S              1
019700951115     D lenout          S              9B 0
019800951115     D Data            S             10
019900951115     D strutturaO      S             10
020000951115     D Operazione      S              2
020100951115     D LenIn           S              9B 0
020200951115     D refresh         S              1
020300951115     D uso             S              1
020400951115     DOutput           S           4000
020500940127     D*-------------
020600940127     D* Reperimento nome PGM
020700940127     D STATUS         SDS           333
020800940127     D  DSPGM            *PROC
020900950411     D  PARMS            *PARMS
021000951115     D*-------------
021100951115     DAnope          E DS                  EXTNAME(ANOPE00F) inz
021200951115     D  OPEDTANN     E                     inz('0001-01-01')
021300951115     D  OPEDTIMM     E                     inz('0001-01-01')
021400951115     D  OPEDTULMO    E                     inz('0001-01-01')
021500950704     D*-------------
021600950704     D* Attributi video
021700950704     D* (definisco come costanti qugli attributi video che potrei
021800950704     D*  testare e quindi preferisco assegnargli un valore "parlante")
021900950704     D*  bottone presente
022000950704     D PRESENTE        C                   CONST(X'A1')
022100940616     D*-------------
022200940506     D* Tasti di funzione
022300940506     D F01             C                   CONST(X'31')
022400940506     D F02             C                   CONST(X'32')
022500940506     D F03             C                   CONST(X'33')
022600940506     D F04             C                   CONST(X'34')
022700940506     D F05             C                   CONST(X'35')
022800940506     D F06             C                   CONST(X'36')
022900940506     D F07             C                   CONST(X'37')
023000940506     D F08             C                   CONST(X'38')
023100940506     D F09             C                   CONST(X'39')
023200940506     D F10             C                   CONST(X'3A')
023300940506     D F11             C                   CONST(X'3B')
023400940506     D F12             C                   CONST(X'3C')
023500940506     D F13             C                   CONST(X'B1')
023600940506     D F14             C                   CONST(X'B2')
023700940506     D F15             C                   CONST(X'B3')
023800940506     D F16             C                   CONST(X'B4')
023900940506     D F17             C                   CONST(X'B5')
024000940506     D F18             C                   CONST(X'B6')
024100940506     D F19             C                   CONST(X'B7')
024200940506     D F20             C                   CONST(X'B8')
024300940506     D F21             C                   CONST(X'B9')
024400940506     D F22             C                   CONST(X'BA')
024500940506     D F23             C                   CONST(X'BB')
024600940506     D F24             C                   CONST(X'BC')
024700940506     D ENTER           C                   CONST(X'F1')
024800940506     D ROLDWN          C                   CONST(X'F4')
024900940506     D ROLLUP          C                   CONST(X'F5')
025000951109     D*-------------
025100951109     D DATMIN          C                   CONST('0001-01-01')
025200951109     D DATMAX          C                   CONST('9999-12-31')
025300951114     D* sottotitolo
025400951114     D SOTTIT          C                   CONST('PROMSG    *LIBL-
025500951114     D                                          COS0207')
025600950524     D*-------------
025700950606     D* Variabili per gestione videate
025800950606     D $Gest           S              2A
025900950606     D $Fine           S              1A
026000950606     D $InzD1          S              1A
026100950606     D $InzS1          S              1A
026200950606     D $LastG          S              2A
026300950606     D $LastV          S              2A
026400950606     D $EndFile        S              1A
026500950606     D $Esci           S              1A
026600950606     D $RcdOK          S              1A
026700950606     D*-------------
026800950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
026900950606     D S1Nrr           S                   like(C1Rcd)
027000951109     D SFLNRRSAV       S                   like(SFLNRR)
027100950606     D WSfl            S                   like(C1Nrr)
027200950606     D WMax            S                   like(C1Rcd)
027300950606     D WPag            S                   like(C1Rcd)
027400950606     D WInzS1          S                   like($InzS1)
027500951114    >D WOpr            S                   like(Opr087)
027600950606     D CurR            S              2S 0
027700950606     D CurC            S              2S 0
027800950606     D Win             S             99A
027900951114     D C1Pos           S              6S 0
028000950606     D W1Pos           S                   like(C1Pos)
028100951109     D Wdate           S             10
028200951109    >D WDivisa         S                   like(X01Divisa)
028300951109    >D SavDivisa       S                   like(X01Divisa)
028400951124    >D SavDivVis       S                   like(divvis087)
028500951115    >D SavCausRig      S                   like(s1CausRig)
028600951115    >D Causale         S                   like(s1CausRig)
028700951115    >D WImporto        S             19S 3
028800951110    >D NrDec           S              3S 0
028900951110    >D NrDecd          S              3S 0
029000951110    >D WNrDec          S              3S 0
029100951110     D Wsegno          S              1A
029200951114    >D WSaldo          S                   like(SaldoM087)
029300960318    >D Winz073         S                   like(exf073)
029400960319     D wopzs1          S              1
029500960319     D wopzs2          S              1
029600051109c1881D Wdw084wrk       S              1
029700020116C1530D*-------------
029800020116  "   * Parametri driver Laguna
029900020116  "  D Opzione         S              2A
030000020116  "  D Errore          S              1A
030100020116  "  D Vista           S              2S 0
030200020116  "  D Dati            S           4000A
030300020116  "  D Lunghezza       S              9B 0
030400020116  "  D Commit          S              1A
030500020116  "  D NrChiavi        S              2S 0
030600020116  "  D Alloca          S              1A
030700020118  "  D DataISO         S               D
030800020117C1530D*
030900011213     D*-------------
031000950606     D*  Indici
031100950606     D X               S              3S 0
031200950606     D Y               S              3S 0
031300011213C1530D*-------------
031400011213  "  D* Chiamata a Laguna
031500021120C1530D Cmd670          S             15
031600021120C1605x*Parm670         S             30    dim(30)
031700071019C2055x*Parm670         S             44    dim(30)   ex C1605
031800071019C2055D Parm670         S             44    dim(70)
031900021120C1530D Err670          S              1
032000020116C1530D Ret670          S             32A
032100950606     D*-------------
032200000118C1309D ZDateISO        S               D
032300940127     C*----------------------------------------------------*
032400940127     C*                MAIN LINE PROGRAM
032500940127     C*----------------------------------------------------*
032600940127     C*
032700940223     C* inizializzazione variabili
032800940223     C                   EXSR      INZVAR
032900940223     C*
033000940223     C     $FINE         DOWEQ     *OFF
033100940131     C     $GEST         CASEQ     'S1'          GESS1
033200941027     C                   ENDCS
033300941027     C                   ENDDO
033400940325     C* fine programma
033500940325     C                   EXSR      ENDPGM
033600940225     C*
033700940325     C/EJECT
033800940325     C************************************************************
033900940325     C* FINE PROGRAMMA
034000940325     C************************************************************
034100940325     C     ENDPGM        BEGSR
034200940325     C*
034300970624     c                   move      wopr          opr087
034400940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
034500951114    >C     OPZ087        IFEQ      *BLANK
034600940325     C                   SETON                                            LR
034700940325     C* altrimenti chiudo in RT
034800940325     C                   ELSE
034900940325     C                   SETON                                            RT
035000940325     C                   ENDIF
035100940616     C                   RETURN
035200940325     C*
035300940325     C                   ENDSR
035400940224     C/EJECT
035500940201     C************************************************************
035600940201     C* RICERCHE
035700940201     C************************************************************
035800940127     C     SEARCH        BEGSR
035900940127     C*
036000940127     C* determino Riga/Colonna del cursore
036100940127     C                   MOVE      NRG           R$$
036200940127     C                   MOVE      NCL           C$$
036300940127     C                   Z-ADD     RIRI          CURR
036400940127     C                   Z-ADD     COCO          CURC
036500950704     C*
036600941118   B1C                   SELECT
036700940127     C*
036800950703    >C*--------------------------------------------------------------
036900950703    >C* inserire le routine di ricerca
037000950703    >C*--------------------------------------------------------------
037100951108    >C* FMTD1 -
037200951108    >C     1             WHENEQ    1
037300940223     C*
037400940223     C* ricerca non abilitata in questa posizione
037500941118   X1C                   OTHER
037600940929     C                   SETON                                        9899
037700940127     C*
037800941118   E1C                   ENDSL
037900950612     C*
038000950612     C* imposto pos. del cursore
038100950612     C                   Z-ADD     CURR          H1RIGA
038200950612     C                   Z-ADD     CURC          H1COLO
038300940127     C*
038400940506     C                   MOVE      *HIVAL        $LASTV
038500950612     C*
038600940127     C                   ENDSR
038700940131     C/EJECT
038800940224     C************************************************************
038900940131     C* RIEMPIMENTO OPZIONI
039000940224     C************************************************************
039100940131     C     RIEOPZ        BEGSR
039200940223     C*
039300940223     C* Input :
039400940223     C* - $TX = schiera con tutti i testi disponibili
039500940223     C* - $FL = schiere con i flag di validità associati ai testi
039600940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
039700940223     C*          opzione usato
039800940223     C* - WMXO = variabile contenente il numero massimo di testi
039900940223     C*          opzione usabili
040000940721     C* - $L01 = Lunghezza prima riga
040100940721     C* - $L02 = Lunghezza seconda riga
040200940223     C* Output :
040300940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
040400940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
040500941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
040600940223     C*
040700950522     C                   CALLB     'X04OPMSG'
040800940223     C* input
040900940223     C                   PARM                    $TX
041000940223     C                   PARM                    $FL
041100940225     C                   PARM                    WCOZ              3 0
041200940223     C                   PARM                    WMXO              3 0
041300940223     C* output
041400940223     C                   PARM                    WOPZ1            76
041500940223     C                   PARM                    WOPZ2            76
041600941214     C                   PARM                    FF23              1
041700950301     C* input
041800940721    >C                   PARM                    $L01              3 0
041900951108    >C                   PARM                    $L02              3 0
042000940223     C*
042100060224C1913C                   PARM                    $ALLOPT         960
042200060224C1913C*
042300940131     C                   ENDSR
042400940506     C/EJECT
042500940506     C************************************************************
042600940506     C* RIEMPIMENTO TASTI FUNZIONE
042700940506     C************************************************************
042800940506     C     RIEKEY        BEGSR
042900940506     C*
043000940506     C* Input :
043100940506     C* - $FX = schiera con tutti i testi disponibili
043200940506     C* - $FC = schiere con i flag di validità associati ai testi
043300940506     C* - $FR = schiere con i flag di REVERSE IMAGE
043400940506     C* - $ULK = ultimo testo caricato
043500940721     C* - $L01 = Lunghezza prima riga
043600940721     C* - $L02 = Lunghezza seconda riga
043700940506     C* Output :
043800940506     C* - $ULK
043900940609     C* - $KEY1= 1° riga di tasti funzione
044000940609     C* - $KEY2= 2° riga di tasti funzione
044100940506     C*
044200950522     C                   CALLB     'XKEYMSG'
044300940506     C* input
044400940506     C                   PARM                    $FX
044500940506     C                   PARM                    $FC
044600940506     C                   PARM                    $FR
044700940506     C                   PARM                    $ULK              3 0
044800940506     C* output
044900940506     C                   PARM                    $KEY1            79
045000940506     C                   PARM                    $KEY2            79
045100950301     C* input
045200940721    >C                   PARM                    $L01              3 0
045300940721    >C                   PARM                    $L02              3 0
045400940506     C*
045500060224C1913C                   PARM                    $ALLFUNCT       480
045600060224C1913C*
045700940506     C                   ENDSR
045800940224     C/EJECT
045900940224     C************************************************************
046000940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
046100940224     C************************************************************
046200940224     C     GESPIE        BEGSR
046300940224     C*
046400940224     C                   SELECT
046500940506     C* Lista
046600940506     C     $GEST         WHENEQ    'S1'
046700940506     C     $ULKS1        IFEQ      0
046800940506     C                   EXSR      F24S1
046900940506     C                   ELSE
047000951114     C                   WRITE     C087Z1
047100940506     C                   ENDIF
047200940506     C                   ENDSL
047300940506     C*
047400940224     C                   ENDSR
047500951110     C/EJECT
047600951110     C*
047700940224     C/EJECT
047800940127     C************************************************************
047900960314     C* GESTIONE LISTA ( i due sfl sono uguali come campi
048000960314     c* SONO NASCOSTI QUELLI CHE non servono nel secondo) e come
048100960314     c* tasti ed opzioni : è tutto uguale, esclusa la visualizzazione
048200940127     C************************************************************
048300940127     C     GESS1         BEGSR
048400940223     C* inizializzazione videata
048500940223     C     $INZS1        IFEQ      *ON
048600940127     C                   EXSR      INZS1
048700940223     C                   MOVE      *OFF          $INZS1
048800940127     C                   ENDIF
048900940223     C* emissione piede videata se proveniente da altra
049000940223     C* salvataggio valore formato di provenienza
049100940202     C     $LASTV        IFNE      'S1'
049200951114     C                   WRITE     C087T1
049300940224     C                   EXSR      GESPIE
049400940202     C                   MOVE      $LASTV        $LASTG
049500940202     C                   MOVE      'S1'          $LASTV
049600941027     C                   ENDIF
049700940223     C*
049800940223     C     WMAX          IFEQ      0
049900951114     C                   WRITE     C087D1
050000940211     C                   ENDIF
050100960314     C                   Eval      *in09=c1mode
050200051109     c*
050300960314     c*
050400960314     C* emetto un sfl o l'altro a seconda del parametro richiesto
050500960314     c*
050600960314     c                   select
050700960314     c                   when      $visdati = '1'
050800951114     C                   EXFMT     C087C1
050900960314     c                   when      $visdati = '2'
051000960314     C                   EXFMT     C087C2
051100960314     c                   endsl
051200960314     C*
051300940204     C     C1NRR         IFNE      0
051400940204     C                   Z-ADD     C1NRR         WSFL
051500940204     C                   ENDIF
051600951109     C*
051700951109     C     SFLNRR        IFNE      0
051800951109     C                   Z-ADD     SFLNRR        C1RCD
051900951109     C                   Z-ADD     SFLNRR        SFLNRRSAV
052000951109     C                   ELSE
052100951109     C                   Z-ADD     SFLNRRSAV     C1RCD
052200951109     C                   ENDIF
052300960318     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
052400960318     C                   MOVE      '1'           Winz073
052500051110c1881C                   MOVE      '1'           Wdw084wrk
052600940506     C*
052700940506     C* controllo abilitazione tasti funzione
052800940506     C                   EXSR      CTRKS1
052900941205     C*
053000940506     C     *IN99         IFEQ      *OFF
053100940127     C*
053200941027     C                   SELECT
053300981009     C* F2=Dati conto
053400981009     C     $TASTO        WHENEQ    F02
053500981009     C                   EXSR      F02S1
053600940127     C* F3=Fine
053700940506     C     $TASTO        WHENEQ    F03
053800951115     C                   EXSR      F03S1
053900951115     C* F8=Seguente
054000951115     C     $TASTO        WHENEQ    F08
054100951115     C                   EXSR      F08S1
054200951116     C* F11=Cambio moneta
054300951116     C     $TASTO        WHENEQ    F11
054400951116     C                   EXSR      F11S1
054500940131     C* F12=Ritorno
054600940506     C     $TASTO        WHENEQ    F12
054700940309     C                   EXSR      F12S1
054800951115     C* F15=Interrogazioni
054900951115     C     $TASTO        WHENEQ    F15
055000951115     C                   EXSR      F15S1
055100940224     C* F23=Altre opzioni
055200940506     C     $TASTO        WHENEQ    F23
055300940309     C                   EXSR      F23S1
055400940224     C* F24=Altri tasti
055500940506     C     $TASTO        WHENEQ    F24
055600940309     C                   EXSR      F24S1
055700951114     C* ROLL-UP
055800940506     C     $TASTO        WHENEQ    ROLLUP
055900940127     C                   EXSR      ROLS1
056000940127     C*
056100941027     C                   OTHER
056200940127     C* CONTROLLO DATI
056300940131     C                   EXSR      CTRC1
056400940201     C     *IN99         IFEQ      *OFF
056500960319     c* controllo che non vengano scelte opzioni miste di selezione se
056600960319     C* scelta opzione multipla
056700960319     c                   if        multip087 = *on
056800960319     C                   EXSR      CTRopz
056900960319     C                   endif
057000960319     C     *IN99         IFEQ      *OFF
057100940131     C                   EXSR      CTRS1
057200941027     C                   ENDIF
057300960319     C                   ENDIF
057400941205     C*
057500941027     C                   ENDSL
057600940127     C*
057700940506     C                   ENDIF
057800941205     C*
057900940127     C                   ENDSR
058000960319     C/EJECT
058100960319     C************************************************************
058200960319     C* CONTROLLI PER opzioni miste se scelta selezione multipla
058300960319     C************************************************************
058400960319     C     CTROPZ        BEGSR
058500960319     C*
058600960319     C                   SETOFF                                       99
058700960319     c                   clear                   wopzs1
058800960319     c                   clear                   wopzs2
058900960319     C*
059000960319   B1C     WMAX          IFGT      0
059100960319     C                   READC     C087S1                                 21
059200960319   B2C     *IN21         DOWEQ     *OFF
059300960319     C*
059400960319   B3C     S1OPZ         IFEQ      1
059500960319     C                   eval      wopzs1 = *on
059600960319   E3C                   ENDIF
059700960319   B3C     S1OPZ         IFNE      1
059800960404   B3C     S1OPZ         andne     0
059900960319     C                   eval      wopzs2 = *on
060000960319   E3C                   ENDIF
060100960319     c*
060200960319     C                   MOVE      *ON           *IN32
060300960319     C* imposto quegli attributi del sfl che sono modificabili
060400960319     C* al richiamo del determinato pgm
060500960319     C                   EXSR      ATRS1
060600960319     C                   UPDATE    C087S1
060700960319     C                   MOVE      *Off          *IN32
060800960319     c*
060900960319     C                   READC     C087S1                                 21
061000960319     C*
061100960319   E2C                   ENDDO
061200960319     C*
061300960319     C* se rilevate opzioni miste , attivo sflnxtchg
061400960319     C                   if        wopzs1 = *on and
061500960319     C                             wopzs2 = *on
061600960319     C                   seton                                        9992
061700960319     C                   endif
061800960319     C*
061900960319   E1C                   ENDIF
062000960319     C*
062100960319     C                   ENDSR
062200960319     C/EJECT
062300940127     C************************************************************
062400940131     C* INIZIALIZZAZIONE LISTA
062500940127     C************************************************************
062600940127     C     INZS1         BEGSR
062700940302     C* pulizia SFL
062800940128     C                   SETOFF                                         3031
062900960314     c                   select
063000960314     c                   when      $visdati = '1'
063100951114     C                   WRITE     C087C1
063200960314     c                   when      $visdati = '2'
063300960314     C                   WRITE     C087C2
063400960314     c                   endsl
063500960314     C                   SETON                                          31
063600051215     c*
063700051215C1892C* accendo indicatore 17 per differenti costanti dei titoli
063800051215  "  c                   if        dataSf087=*blanks
063900051215  "  c                   setoff                                       17
064000051215  "  c                   else
064100051215  "  c                   seton                                        17
064200051215c1892c                   endif
064300940127     C*
064400940128     C* riempimento opzioni
064500940223     C                   MOVEA     $TX1          $TX
064600940223     C                   MOVEA     $FL1          $FL
064700940309     C                   Z-ADD     0             WCOZ
064800940225     C*  equivale al num.di elementi della schiera $TX1
064900940509     C                   Z-ADD     $DIMOP        WMXO
065000941220    >C                   Z-ADD     76            $L01
065100941220    >C                   Z-ADD     76            $L02
065200940128     C                   EXSR      RIEOPZ
065300941214     C                   Z-ADD     23            X
065400951109     C                   MOVE      FF23          $FC1(X)
065500940608     C                   MOVEL     WOPZ1         C1OPZ1
065600060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
065700951108     C                   MOVEL     WOPZ2         C1OPZ2
065800940225     C                   Z-ADD     WCOZ          C1COZ
065900951114     C* azzero gli attributi non bottoni
066000951114     C                   MOVEL     X'A0'         SCADUTOA
066100951114     C                   MOVEL     X'A0'         ASCADEREA
066200951114     C                   MOVEL     X'A0'         RISCHIOA
066300951114     C                   MOVEL     X'A0'         PORTAFOGLA
066400951114     C                   MOVEL     X'A0'         INSRITA
066500951114     C                   MOVEL     X'A0'         COMMENTIA
066600951114     C                   MOVEL     X'A0'         BONsola
066700951108     C* imposta control
066800951124     C                   EVAL      C1KCC = kcc087
066900951124     C                   EVAL      C1KsC = ksc087
067000951124     C                   EVAL      C1Descr = DESCR087
067100951124     C                   EVAL      C1Desagg = DESAGG087
067200951114     C                   EVAL      C1Unita  = UNita087
067300951114     C                   EVAL      C1kscage = kscage087
067400951114     C                   CALLb     'XDT4000'
067500951114     C                   PARM      DTPAR087      XDTAMG           10
067600951114     C     C1DTPAR       PARM                    XDTGMA            6 0
067700951114     C                   PARM      2             XDTSTA            1 0
067800951114     C                   EVAL      C1nrpar  = nrpar087
067900951114     C                   EVAL      C1Serpar = Serpar087
068000951114     C                   eval      c1divisa = divisa087
068100951114     C                   eval      c1monconTO = moncon087
068200951115     C                   eval      c1saldod = saldod087
068300951115     C                   eval      c1saldom = saldom087
068400951114     C                   eval      c1segnod = segnod087
068500951114     C                   eval      c1segnom = segnom087
068600951114     C* Attivo o no flag
068700951114     C                   IF        SCADUTO087 = *on
068800951114     C                   EVAL      scadutoA = presente
068900951114     C                   ENDIF
069000951114     C                   IF        ASCADER087 = *on
069100951114     C                   EVAL      Ascaderea = presente
069200951114     C                   ENDIF
069300951114     C                   IF        rischio087 = *on
069400951114     C                   EVAL      rischioA = presente
069500951114     C                   ENDIF
069600951114     C                   IF        portafo087 = *on
069700951114     C                   EVAL      portafoglA = presente
069800951114     C                   ENDIF
069900951114     C                   IF        insrit087 = *on
070000951114     C                   EVAL      insrita = presente
070100951114     C                   ENDIF
070200951114     C                   IF        comment087 = *on
070300951114     C                   EVAL      commentia = presente
070400951114     C                   ENDIF
070500951114     C                   IF        bonsol087 = *on
070600951114     C                   EVAL      bonsola = presente
070700951114     C                   ENDIF
070800951114     C                   eval      c1bkamm = bkamm087
070900951115     C* Attivo o no unita
071000951115     C                   IF        fgo1 = *on
071100951115     c                   eval      *in11 = *on
071200951115     C                   else
071300951115     c                   eval      *in11 = *off
071400951115     C                   endif
071500951108     C* Attivo o no scritta percipiente
071600951124     C                   IF        percipi087 = *on
071700951108     c                   eval      *in12 = *on
071800951108     C                   else
071900951108     c                   eval      *in12 = *off
072000951108     C                   endif
072100951115     C* Attivo o no agente
072200951124     C                   IF        snatura087 = 'C'
072300951115     c                   eval      *in13 = *on
072400951115     C                   else
072500951115     c                   eval      *in13 = *off
072600951115     C                   endif
072700940128     C*
072800951114     C* CARICAMENTO SFL A PAGINE
072900940128     C*
073000940201     C                   Z-ADD     0             S1NRR
073100940128     C                   Z-ADD     0             WMAX
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
074300050608D1830C                   Z-ADD     SFLPAG        WPAG
074400940127     C                   EXSR      ROLS1
074500940201     C                   Z-ADD     SFLPAG        WPAG
074600940127     C*
074700940127     C                   ENDSR
074800940127     C/EJECT
074900940127     C************************************************************
075000940131     C* CARICAMENTO PAGINA LISTA
075100940127     C************************************************************
075200940127     C     ROLS1         BEGSR
075300940127     C*
075400940128     C                   SETOFF                                       32
075500940223     C                   Z-ADD     0             Y
075600940127     C                   Z-ADD     WMAX          S1NRR
075700940127     C*
075800940127     C* Leggo dal file anagrafico per caricare la lista
075900940127     C*
076000950606     C     $EndFile      DOWEQ     *OFF
076100940201     C     Y             ANDLT     WPAG
076200940127     C*
076300940131     C                   EXSR      RIES1
076400940127     C*
076500940127     C                   ADD       1             S1NRR
076600940127     C                   ADD       1             Y
076700940127     C*
076800960314     c                   select
076900960314     c                   when      $visdati = '1'
077000951114     C                   WRITE     C087S1
077100960314     c                   when      $visdati = '2'
077200960314     C                   WRITE     C087S2
077300960314     c                   endsl
077400940131     C*
077500941214     C                   EXSR      REDANA
077600940128     C*
077700941027     C                   ENDDO
077800940127     C*
077900940223     C                   Z-ADD     S1NRR         WMAX                 30
078000940127     C*
078100940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
078200940127     C*
078300940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
078400940127     C                   MVR                     RESTO             3 0
078500940131     C     PAGINE        MULT      SFLPAG        C1RCD
078600941027     C     RESTO         IFGT      0
078700940127     C                   ADD       1             C1RCD
078800941027     C                   ELSE
078900940407     C                   SUB       SFLPAG        C1RCD
079000940407     C                   ADD       1             C1RCD
079100941027     C                   ENDIF
079200940128     C*
079300940127     C                   ENDSR
079400940224     C/EJECT
079500940224     C************************************************************
079600940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
079700940224     C************************************************************
079800941214     C     SETANA        BEGSR
079900940224     C*
080000951114     C                   MOVE      XSCSOC        PASSOCIETA
080100951124     C                   MOVE      CTB087        PASctb
080200951124     C                   MOVE      KCC087        PASKCC
080300951124     C                   MOVE      KSC087        PASKSC
080400951114     C                   MOVE      DTPAR087      PASDTPAR
080500951114     C                   MOVE      NRPAR087      PASNRPAR
080600951114     C                   MOVE      SERPAR087     PASSERPAR
080700941214     C*
080800951114    >C     K07PAS02      SETLL     NDPAS02J
080900940224     C*
081000940224     C                   ENDSR
081100940128     C/EJECT
081200940128     C************************************************************
081300940131     C* LETTURA RCD ARCHIVIO PILOTA
081400940128     C************************************************************
081500941214     C     REDANA        BEGSR
081600940128     C*
081700950606     C                   MOVEL     *OFF          $EndFile
081800940131     C                   MOVEL     *OFF          $RCDOK
081900940131     C*
082000950606     C     $EndFile      DOUEQ     *ON
082100940131     C     $RCDOK        OREQ      *ON
082200940131     C*
082300951114    >C     K07PAS02      READE     NDPAS02J                               21
082400940131     C*
082500950606     C                   MOVE      *IN21         $EndFile
082600940131     C*
082700950606     C     $EndFile      IFEQ      *OFF
082800941214     C                   EXSR      SELANA
082900941027     C                   ENDIF
083000940131     C*
083100941027     C                   ENDDO
083200940131     C*
083300940131     C                   MOVE      *IN21         *IN33
083400940131     C*
083500940131     C                   ENDSR
083600940224     C/EJECT
083700940224     C************************************************************
083800940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
083900940224     C************************************************************
084000940224     C     CLCPAG        BEGSR
084100940224     C* Input :
084200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
084300940224     C*          cursore
084400940224     C* - SFLPAG = numero rcd per pagina sfl
084500940224     C* Output :
084600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
084700940224     C*
084800940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
084900940224     C                   MVR                     RESTO             3 0
085000940224     C     RESTO         IFGT      0
085100940224     C                   ADD       1             PAGINE
085200940224     C                   ENDIF
085300940224     C     PAGINE        MULT      SFLPAG        WPAG
085400940224     C*
085500940224     C                   ENDSR
085600940131     C/EJECT
085700940131     C************************************************************
085800940131     C* SELEZIONI SULLA RIGA LETTA
085900940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
086000940314     C* non rientra nei parametri di parzializzazione
086100940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
086200940314     C* superiore stabilito nei parametri
086300940314     C*
086400940131     C************************************************************
086500941214     C     SELANA        BEGSR
086600940131     C*
086700940202     C                   MOVEL     *ON           $RCDOK
086800070205C1973X****               MOVEL     *ALL'9'       ALL9             15
086900070205C1973C                   MOVEL     *Hival        ALL9             15
087000951109     C*
087100951116     C* SELEZIONI RISPETTO AI VALORI RCD
087200951116     C*
087300951116     C* Scartare i record che non rispondono alle selezioni effettuate
087400951116     C* esempio : esclusione degli annullati.
087500951116     C                   SELECT
087600951116     C* Provvis, definit, gestionali
087700951116     C*
087800951124     C                   WHEN      (Movdef087 = *off   and
087900960102     C                             pastpregz = 'D') or
088000951124     C                             (movprov087 = *off   and
088100960102     C                             pastpregz = 'P') or
088200951124     C                             (movges087  = *off   and
088300960102     C                             pastpregz = 'G')
088400951116     C                   eval      $rcdok = *off
088500951116     C* date limite registrazione P, G, D
088600960313     C                   WHEN      (Movdef087 =*on and pasdtreg > dtregD087 and
088700960313     C                              Pastpregz = 'D') or
088800960313     C                             (Movprov087 =*on and pasdtreg > dtregP087 and
088900960313     C                              Pastpregz = 'P') or
089000960313     C                             (Movges087 =*on and pasdtreg > dtregG087  and
089100960313     C                              Pastpregz = 'G')
089200951116     C                   eval      $rcdok = *off
089300951116     C*
089400951116     C                   ENDSL
089500951109     C*
089600940202     C                   ENDSR
089700940131     C/EJECT
089800940131     C************************************************************
089900940131     C* RIEMPIMENTO RIGA LISTA
090000940131     C************************************************************
090100940131     C     RIES1         BEGSR
090200940131     C*
090300951114     C                   CLEAR                   C087S1
090400960314     C                   CLEAR                   C087S2
090500951109     C*
090600951115     C                   MOVE      PasSYS        H1SYS
090700951115     C                   MOVE      PasNRASREG    H1NRASREG
090800960102     C                   MOVE      movnrrigam    H1NRrigam
090900960318     C                   MOVE      pasnrriga     H1NRrigaP
091000951116     c* data libro giornale
091100951115     C                   CALLb     'XDT4000'
091200090504>>>>>C                   PARM      PasDtReg      XDTAMG
091300951115     C     S1DTlgio      PARM                    XDTGMA
091400951115     C                   PARM      2             XDTSTA
091500951116     c* registrazione
091600951116     C                   CALLb     'XDT4000'
091700960102     C                   PARM      pasDTreg      XDTAMG
091800951116     C     S1DTreg       PARM                    XDTGMA
091900951116     C                   PARM      2             XDTSTA
092000951115     C                   MOVE      RegNRreg      S1NRreg
092100951116     C                   eval      s1serreg = regserreg
092200951116     c*Segnalo solo le registrazioni non definitive, per queste lascio blank
092300951115     C                   IF        Pastpregz <> 'D'
092400951115     C                   eval      s1tpregz  = pastpregz
092500951115     C                   endif
092600951116     c* documento
092700951109     C                   CALLb     'XDT4000'
092800951115     C                   PARM      MovDTdoc      XDTAMG
092900951115     C     S1DTdoc       PARM                    XDTGMA
093000951109     C                   PARM      2             XDTSTA
093100951115     C                   eval      s1nrdoc = movnrdoc
093200951116     C                   eval      s1serdoc = movserdoc
093300051215     c* scadenza
093400951115     C                   CALLb     'XDT4000'
093500951115     C                   PARM      pasDTsca      XDTAMG
093600051215c1892C     H1DTsca       PARM                    XDTGMA
093700051215c1892x***  S1DTsca       PARM                    XDTGMA
093800951115     C                   PARM      2             XDTSTA
093900951115     c*
094000951115     C* decodifica causale (se diversa dalla precedente)
094100951116     C                   eval      s1causrig = pascausrig
094200951115     C                   if        pascausrig <> Savcausrig
094300951115     C                   eval      savcausrig = pascausrig
094400951115     C                   eval      causale    = pascausrig
094500951115     C                   EXSR      CALLMVC003
094600951115     C                   IF        Esito = *off
094700951115     C                   EVAL      S1CausrigD = OPEdesbrev
094800951128     C                   EVAL      portafh    = OPEportaf
094900020305C1530C                   EVAL      H1Stp      = OPEStp
095000951115     C                   ENDIF
095100951116     C                   else
095200951116     C                   EVAL      S1CausrigD = OPEdesbrev
095300951128     C                   EVAL      portafh    = OPEportaf
095400020305C1530C                   EVAL      H1Stp      = OPEStp
095500951115     C                   endif
095600951115     C                   eval      s1causrig = pascausrig
095700951116     c*
095800951115     C* Ci può essere o mdpag o mdrd
095900951115     C                   IF        PASmdpag <> ' '
096000951115     C                   eval      s1mdpag = PASMdpag
096100951115     C                   endif
096200951115     C                   IF        PASmdrd  <> ' '
096300951115     C                   eval      s1mdpag = PASMdrd
096400951115     C                   endif
096500951116     C*
096600951115     C                   eval      s1divcoge = movdivisa
096700951115     C                   eval      s1Cambio   = movcambio
096800051215     c* flusso
096900951115     C                   CALLb     'XDT4000'
097000951115     C                   PARM      pasDTflu      XDTAMG
097100051215C1892X***  S1DTflu       PARM                    XDTGMA
097200051215C1892C     H1DTflu       PARM                    XDTGMA
097300951115     C                   PARM      2             XDTSTA
097400051216     c*
097500051216C1892C* Riempio icampi S1DtSca e S1dtFlu a seconda della richiesta di
097600051216  "  c*visualizzazione
097700051216C1892c                   exsr      DataScaFlu
097800051215     c*
097900951115     C                   CALLb     'XDT4000'
098000951115     C                   PARM      movdtcoco     XDTAMG
098100951115     C     S1DTcoco      PARM                    XDTGMA
098200951115     C                   PARM      2             XDTSTA
098300951115     C                   eval      s1unita = pasunita
098400951115     C                   eval      s1kcccpu = movkcccpu
098500951115     C                   eval      s1ksccpu = movksccpu
098600970805     C* decodifica contropartita
098700970805     c                   if        s1kcccpu <> *blanks and s1ksccpu <>
098800970805     c                             *blanks
098900970805     C                   EXSR      CALLMVC002
099000970805     c                   endif
099100951115     C                   eval      s1desbrev= movdesbrev
099200951115     C                   eval      s1caustes= movcaustes
099300951116     C*
099400951116     C* causale testata(trovo decodifica solo se diversa da causale riga)
099500981002     C                   IF        movcaustes <> savcausrig
099600981002     C                   eval      savcausrig = movcaustes
099700981002     C                   eval      causale    = movcaustes
099800951115     C                   EXSR      CALLMVC003
099900951115     C                   IF        Esito = *off
100000951115     C                   EVAL      S1CaustesD = OPEdesbrev
100100951115     C                   ENDIF
100200951115     C                   else
100300981002     C                   EVAL      S1CaustesD = opedesbrev
100400951115     C                   endif
100500951116     C*
100600951115     C                   eval      s1classe = regclasse
100700951115     C                   eval      s1utente = reguteulmo
100800951116     C*
100900951116     C* Calcola importi tramite routine edit
101000951116     C                   exsr      Importi
101100951116     C*
101200951116     c* Imposta importi a seguito richiesta moneta di conto o divisa
101300951116     C                   SELECT
101400951116     C* divisa
101500951116     C                   when      SavDivVis = '1'
101600951116     C                   eval      s1importo1 = H1Importod
101700951116     C                   eval      s1segno1   = H1segnod
101800951116     C                   eval      s1importo2 = H1importom
101900951116     C                   eval      s1segno2   = H1segnom
102000951116     C* moneta di conto
102100951116     C                   when      SavDivVis = '0'
102200951116     C                   eval      s1importo1 = H1importom
102300951116     C                   eval      s1segno1   = H1segnom
102400951116     C                   eval      s1importo2 = H1importod
102500951116     C                   eval      s1segno2   = H1segnod
102600951116     C                   endsl
102700951116     C*
102800951116     C* se le divise sono uguali, imposto il valore del campo in drop
102900951116     C* uguale a quello della prima riga, così almeno sono allineati
103000951116     C                   IF        pasdivisa = xscdiv
103100951116     C                   eval      s1importo2= s1importo1
103200951116     C                   endif
103300951115     C*
103400951115     C* segnalo che esistono altri movimenti  di ndmov
103500951115     C                   if        movimporto <> pasimporto
103600951115     C                   EVAL      S1MOVPAS = '>'
103700951115     C                   endif
103800960314     c* Importo coge: nel sfl analitico c'è sempre
103900960314     c* Importo coge: nel sfl sintetico c'è solo se diverso da pas
104000960314     c                   select
104100960314     c                   when      $visdati = '1'
104200960314     C                   eval      s1impcoged = movimportd
104300960314     c                   when      $visdati = '2'
104400960314     C                   if        movimporto <> pasimporto
104500960314     C                   eval      s1impcoged = movimportd
104600960314     c                   endif
104700960314     c                   endsl
104800951115     C*
104900951115     C* verifico se esistono descrizioni aggiuntive(riga)
105000951115     C                   EXSR      TestDesAGG
105100061123C1979C* verifico se esistono blocchi sulla rate solo per fornitori
105200061123  "  c                   if        Snatura087='F'
105300061123  "  C                   EXSR      TestBlRata
105400061123C1979c                   endif
105500020116C1530C*
105600020116  "  C* Testo esistenza documenti catalogati su Laguna (se questo
105700020116  "  C*  attivo)
105800020116  "  C                   IF        xscLaguna = *on
105900020116  "  C                   EXSR      DriverLGN
106000020116  "  C*
106100020118  "  C                   EVAL      H1ClassLg = *blanks
106200020117  "  C                   EVAL      H1IdLgn = 0
106300020116  "  C                   IF        Errore = *off
106400020116  "  C                   EVAL      ANLGN00F = Dati
106500020118  "  C* salvo la chiave univoca per richiamare Laguna
106600020118  "  C                   EVAL      H1ClassLg = LGNClassLg
106700020116  "  C                   EVAL      H1IdLgn   = LGNIdLgn
106800020116  "  C                   ENDIF
106900020116  "  C*
107000020116C1530C                   ENDIF
107100950301     C*
107200950301     C* imposto quegli attributi del sfl che sono modificabili
107300950301     C* al richiamo del determinato pgm
107400950301     C                   EXSR      ATRS1
107500940131     C*
107600940131     C                   ENDSR
107700951116     C*
107800051216c1892C************************************************************
107900051216     C* riempimento data scadenza o flusso a seconda della richiesta
108000051216c1892C************************************************************
108100051216     C     DataScaFlu    BEGSR
108200051216     c*
108300051216     c* sfl analitico
108400051216     c                   select
108500051216     c                   when      $visdati = '1'
108600051216     c                   if        dataSf087=*blanks
108700051216     c                   eval      S1dtSca=H1Dtsca
108800051216     c                   eval      IdMsg= $Cost(1)
108900051216     C                   EXSR      RtvMsg
109000051216     c                   eval      S1dtFlu=H1DtFlu
109100051216     c                   else
109200051216     c                   eval      S1dtSca=H1DtFlu
109300051216     c                   eval      IdMsg= $Cost(2)
109400051216     C                   EXSR      RtvMsg
109500051216     c                   eval      S1dtFlu=H1DtSca
109600060419D1984c**se non c'è la data flusso , ci metto la data scadenza
109700060419  "  c                   if        S1Dtsca=0
109800060419  "  c                   eval      S1dtSca=H1DtSca
109900060419D1984c                   endif
110000051216     c                   endif
110100051216     c* sfl sintetico(c'è solo la data S1DTSCA)
110200051216     c                   when      $visdati = '2'
110300051216     c                   if        dataSf087=*blanks
110400051216     c                   eval      S1dtSca=H1Dtsca
110500051216     c                   else
110600051216     c                   eval      S1dtSca=H1DtFlu
110700060419D1984c**se non c'è la data flusso , ci metto la data scadenza
110800060419  "  c                   if        S1Dtsca=0
110900060419  "  c                   eval      S1dtSca=H1DtSca
111000060419D1984c                   endif
111100051216     c                   endif
111200051216     c                   endsl
111300051216     C*
111400051216c1892C                   ENDSR
111500051216c1892C************************************************************
111600051216     C* Chiamata a xrtvm
111700051216c1892C************************************************************
111800051216     C     RtvMsg        BEGSR
111900051216     C*
112000051216     C                   CALLB     'XRTVM'
112100051216     C                   PARM                    IDMSG            27
112200051216     C                   PARM                    TXTMSG          100
112300051216     C                   PARM                    ERRMSGRTV         1
112400051216     C                   PARM      *OFF          CTRMSG            1            centratura
112500051216     C                   PARM                    LENMSG            3 0          lun output.
112600051216     C     ERRMSGRTV     IFNE      *ON
112700051216     C                   MOVEL     TxTMsg        S1CostData                     *
112800051216     C                   ENDIF
112900051216     C*
113000051216C1892C                   ENDSR
113100020116C1530C************************************************************
113200020116  "  C* reperimento dati sui documenti catalogati in Laguna
113300020116  "  C************************************************************
113400020117C1530C     DriverLGN     BEGSR
113500020116     C*
113600020116     C                   Clear                   Dati
113700020116     C                   Clear                   ANLGN00F
113800020305     C*
113900020305     C* Se Sottotipologia causale=Doc.Iva Clienti ....
114000020305     C                   IF        H1Stp = 'C'
114100020117     C* imposto le chiavi per fatture attive
114200020117     C                   EVAL      LGNClassPJ = '030'
114300020118     C                   EVAL      LGNClassLg = 'DOC'
114400020117     C                   EVAL      LGNKey101 = xscsoc
114500020117     C                   MOVEL     '0001-01-01'  LGNKey102
114600020117     C                   IF        S1DtDoc <> 0
114700020117     C     *jobrun       MOVEL     S1DtDoc       DataISO
114800020117     C                   MOVEL     DataISO       LGNKey102
114900020117     C                   ENDIF
115000020117     C                   MOVEL     S1NrDoc       LGNKey103
115100020117     C                   EVAL      LGNKey104 = S1SerDoc
115200020118     C                   EVAL      NrChiavi = 6
115300020305     C                   ENDIF
115400020117     C*
115500020305     C* Se Sottotipologia causale=Doc.Iva Fornitori ...
115600020305     C                   IF        H1Stp = 'F'
115700020305     C* Per documenti IVA Fornitori, la chiave 2 è 'F' se fattura
115800020305     C* o 'N' se nota di accredito: lo determino da Tipo Doc.IVA
115900020305     C                   SELECT
116000020305     C* fattura
116100020305     C                   WHEN      OPETpDocI = '0' or
116200060303D1937C                             OPETpDocI = '2' or
116300060303     C                             OPETpDocI = '3' or
116400020305     C                             OPETpDocI = '5' or
116500020305     C                             OPETpDocI = '6'
116600020305     C                   EVAL      LGNKey102 = 'F'
116700020305     C* nota
116800020305     C                   WHEN      OPETpDocI = '1' or
116900020305     C                             OPETpDocI = '4' or
117000020305     C                             OPETpDocI = '7'
117100020305     C                   EVAL      LGNKey102 = 'N'
117200020305     C                   ENDSL
117300020117     C* imposto le chiavi per fatture passive
117400020117     C                   EVAL      LGNClassPJ = '020'
117500020118     C                   EVAL      LGNClassLg = 'DOC'
117600020117     C                   EVAL      LGNKey101 = xscsoc
117700020117     C                   MOVEL     '0001-01-01'  LGNKey103
117800020117     C                   IF        S1DtDoc <> 0
117900020117     C     *jobrun       MOVEL     S1DtDoc       DataISO
118000020117     C                   MOVEL     DataISO       LGNKey103
118100020117     C                   ENDIF
118200020117     C                   MOVEL     S1NrDoc       LGNKey104
118300020117     C                   EVAL      LGNKey105 = S1SerDoc
118400020117     C                   EVAL      LGNKey106 = C1Ksc
118500020118     C                   EVAL      NrChiavi = 8
118600020305     C                   ENDIF
118700020117     C*
118800020116     C                   MOVEL     ANLGN00F      Dati
118900020116     C                   eval      Lunghezza = %size(ANLGN00F)
119000020116     C                   eval      Errore    = *off
119100020116     C*
119200020116     C                   CALLB     'PJDR1701R'
119300020116     C                   parm      '01'          Opzione                        1
119400020116     C                   parm                    Errore                         2
119500020116     C                   parm      2             Vista                          5
119600020116     C                   parm                    Dati                           3
119700020116     C                   parm                    Lunghezza                      4
119800020116     C                   parm      *off          Commit                         6
119900020117     C                   parm                    NrChiavi                       7
120000020116     C                   parm      *off          Alloca                         8
120100020116     C                   parm                    *omit                          9
120200020116     C*
120300020116C1530C                   ENDSR
120400951116     C/EJECT
120500951116     C*----------------------------------------------------------
120600951116     C* Imposta importi con routine edit
120700951116     C*----------------------------------------------------------
120800951116     C     Importi       BEGSR
120900951116     C*
121000951116     C* Importo in divisa
121100951116     C* Segno
121200951116     C                   if        pasdare = 1
121300961009     C                   eval      H1segnoD= XscDare
121400951116     C                   else
121500961009     C                   eval      H1segnoD= XscAvere
121600951116     C                   endif
121700951116     C* reperisco decimali se divisa diversa dalla precedente letta
121800951116     C                   if        Pasdivisa <> Savdivisa
121900951116     C                   eval      SavDivisa = PasDivisa
122000951116     C                   Eval      WDivisa =  PasDivisa
122100951116     C                   Exsr      RepDivisa
122200951116     C                   Eval      NrDecD = X01NrDec
122300951116     C                   endif
122400951116     C*
122500951116     C                   EVAL      Wimporto = Pasimportd
122600951116     C                   Z-add     NrDecD        WNrDec
122700951116     C                   Eval      $LenFld = %Size(H1importod)
122800960319     c* memorizza saldo indivisa
122900960319     c                   eval      h1saldodiv = PasImportd
123000081014D2440c* memorizza saldo in MC
123100081014D2440c                   eval      h1saldoMC  = PasImportO
123200951116     C*
123300951116     C* Imposto la dimensione del campo video in modo da allineare
123400951116     C* valute gestite con un numero diverso di decimali
123500951116     C                   Select
123600951116     C                   When      WNrDec = 2
123700951116     C                   Eval      $LenFld = %Size(h1importod) - 1
123800951116     C                   When      WNrDec = 1
123900951116     C                   Eval      $LenFld = %Size(h1importod) - 2
124000951116     C                   When      WNrDec = 0
124100951116     C                   Eval      $LenFld = %Size(h1importod) - 4
124200951116     C                   Other
124300951116     C                   Eval      $LenFld = %Size(h1importod)
124400951116     C                   Endsl
124500951116     C* allineo dare a sinistra
124600961009     C                   if        H1SegnoD = XscDare
124700951116     C                   SUB       5             $lenfld
124800951116     C                   Endif
124900951116     C                   EXSR      Edit
125000951116     C                   Eval      h1importod = $RcvrVar
125100951116     C*
125200951116     C* moneta di conto
125300951116     C* Segno
125400951116     C                   if        pasdare = 1
125500961009     C                   eval      H1segnoM= XscDare
125600951116     C                   else
125700961009     C                   eval      H1segnoM= XscAvere
125800951116     C                   endif
125900951116     C* Importo
126000951116     C                   Z-add     Pasimporto    WImporto
126100951116     C                   Z-add     NrDec         WNrDec
126200951116     C                   Eval      $LenFld = %Size(h1ImportoM)
126300951116     C* allineo dare a sinistra
126400961009     C                   if        h1segnoM = XscDare
126500951116     C                   SUB       5             $lenfld
126600951116     C                   Endif
126700951116     C                   EXSR      Edit
126800951116     C                   Eval      h1Importom = $RcvrVar
126900951116     C*
127000951116     C                   ENDSR
127100970805     C/EJECT
127200970805     C*----------------------------------------------------------
127300970805     C* Chiamata NDMVC002
127400970805     C*----------------------------------------------------------
127500970805     C     CallMvc002    BEGSR
127600970805     C*
127700970805     C* Controllo conto
127800970805     C                   Clear                   ND002DS
127900970805     C                   EVAL      LenOut = %Size(ND002DS)
128000970805     C                   eval      Data = Datmin
128100970805     C                   CallB     'NDMVC002'
128200970805     C                   PARM                    xscsoc
128300970805     C                   PARM                    s1kcccpu
128400970805     C                   PARM                    s1ksccpu
128500970805     C                   PARM                    Data
128600970805     C                   PARM      *OFF          GesMsg
128700970805     C                   PARM      *OFF          Esito
128800970805     C                   PARM      'ND002DS'     StrutturaO
128900970805     C                   PARM                    ND002DS
129000970805     C                   PARM                    LenOut
129100970805     C*
129200970805     c                   if         esito = *off
129300970805     c                   eval       s1ksccpud  = desbrev002
129400970805     c                   endif
129500970805     C*
129600970805     C                   ENDSR
129700951115     C/EJECT
129800951115     C*----------------------------------------------------------
129900951115     C* Chiamata NDMVC003
130000951115     C*----------------------------------------------------------
130100951115     C     CallMvc003    BEGSR
130200951115     C*
130300951115     C* Controllo causale
130400951115     C                   CLEAR                   ANOPE
130500951115     C                   eval      Data = Datmin
130600951115     C                   EVAL      LenOut = %Size(ANOPE)
130700951115     C                   CallB     'NDMVC003'
130800951115     C                   PARM                    xscsoc
130900951115     C                   PARM                    causale
131000951115     C                   PARM                    Data
131100951115     C                   PARM                    Uso
131200951115     C                   PARM      *OFF          GesMsg
131300951115     C                   PARM      *OFF          Esito
131400951115     C                   PARM      'ANOPE'       StrutturaO
131500951115     C                   PARM                    ANOPE
131600951115     C                   PARM                    LenOut
131700990121     C                   PARM      '0'           Refresh
131800951115     C*
131900951115     C                   ENDSR
132000951115     C/EJECT
132100951115     C************************************************************
132200951115     C* testa esistenza descrizioni aggiuntive di testata o riga
132300951115     C************************************************************
132400951115     C     TestDesAGG    BEGSR
132500951115     C                   EVAL      Desaggh   = '0'
132600951115     C* (si esegue setll sul file)
132700951115     C                   eval      DesSys = H1Sys
132800951115     C                   eval      DesNrAsReg = H1NrAsReg
132900951115     C                   eval      DesTipDes  = 'M'
133000951115     C                   eval      DesNrRigaM = H1NrRigaM
133100951115     C     K04DES01      SETLL     NDDES01L                               21
133200951115     C* se il bottone è presente
133300951115     C                   IF        *in21  = '1'
133400951115     C* memorizzo l'attributo di presente
133500951115     C                   EVAL      Desaggh   = '1'
133600951115     C                   else
133700951115     c* provo a vedere se ce ne sono di testata
133800951115     C                   eval      DesSys = H1Sys
133900951115     C                   eval      DesNrAsReg = H1NrAsReg
134000951115     C                   eval      DesTipDes  = 'T'
134100951116     C     K03DES01      SETLL     NDDES01L                               21
134200951115     C* se il bottone è presente
134300951115     C                   IF        *in21  = '1'
134400951115     C* memorizzo l'attributo di presente
134500951115     C                   EVAL      Desaggh   = '1'
134600951115     C                   ENDIF
134700951115     C                   endif
134800951115     C*
134900951115     C                   ENDSR
135000950301     C/EJECT
135100061122c1979C************************************************************
135200061122     C* testa esistenza blocchi rata
135300061122c1979C************************************************************
135400061122     C     TestBlRata    BEGSR
135500061123     C*
135600061123     c                   eval      PscSys=  Sys087
135700061123     c                   eval      PscNrasInt=NrasInt087
135800061123     c                   eval      PscDtSca  =PasDtSca
135900061123     c                   eval      PscNrRata =PasNrRata
136000061123     C*
136100061123     C     K04Psc01      chain     Ndpsc01l
136200061123     c                   if        %found(ndpsc01L) and
136300061123     c                             pscbkamm <> *blanks
136400061123     c                   eval      H1BlocRat= *on
136500061123     c                   endif
136600061122     C*
136700061122c1979C                   ENDSR
136800950301     C************************************************************
136900950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
137000950301     C************************************************************
137100950301     C     ATRS1         BEGSR
137200950704    >C* impostare le condizioni per ripristinare gli attributi
137300950704    >C* della riga del sfl (se ce ne sono)
137400951113      * attiva o no unità
137500951113     C                   if        fgo1=*on
137600951113     C                   eval      *in11=*on
137700951113     C                   else
137800951113     C                   eval      *in11=*off
137900951113     C                   endif
138000951115      * se dala libro <> da data reg
138100951115     C                   if        s1dtlgio <> S1dtreg
138200951115     C                   eval      *in15=*on
138300951115     C                   else
138400951115     C                   eval      *in15=*off
138500951115     C                   endif
138600951115      * se ci sono descrizioni aggiuntive sulla riga sottolineo causale
138700951115     C                   if        desaggh  = *on
138800951115     C                   eval      *in14=*on
138900951115     C                   else
139000951115     C                   eval      *in14=*off
139100951115     C                   endif
139200960327      * se c'è l'importo coge faccio vedere anche la scritta
139300960327     C                   if        s1impcoged <> 0
139400960327     C                   eval      *in16=*on
139500960327     C                   else
139600960327     C                   eval      *in16=*off
139700960327     C                   endif
139800020116C1530 * se c'è valorizzata l'Id di Laguna
139900020116  "  C                   EVAL      *in41=*off
140000020116  "  C                   IF        H1IdLgn <> 0
140100020116  "  C* metto in RI il numero documento per segnalare che esiste un
140200020116  "  C* documento catalogato per questa registrazione (o una fattura
140300020116  "  C*  o una nota)
140400020116  "  C                   EVAL      *in41=*on
140500020116C1530C                   endif
140600061122c1979 * se ci sono dei blocchi sulle rate
140700061122  "  C                   if        H1BlocRat= *on
140800061123  "  C                   eval      *in18=*on
140900061122  "  C                   else
141000061123  "  C                   eval      *in18=*off
141100061122c1979C                   endif
141200950301     C*
141300950301     C                   ENDSR
141400941205     C/EJECT
141500941205     C************************************************************
141600941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
141700941205     C* INPUT
141800951109     C* $FC1 = Abilitazioni videata S1
141900941205     C* OUTPUT
142000941205     C* *IN97 *IN99 Errori
142100941205     C************************************************************
142200941205     C     CTRKS1        BEGSR
142300941205     C*
142400941205     C                   SETOFF                                       99
142500951109     C                   MOVEA     $FC1          $FC
142600941205     C                   EXSR      CTRKEY
142700941205     C     *IN99         IFEQ      *ON
142800941205     C                   SETON                                        97
142900941205     C                   ENDIF
143000941205     C*
143100941205     C                   ENDSR
143200981009     C/EJECT
143300981009     C************************************************************
143400981009     C* GESTIONE F02 VIDEO S1  dati anagrafici conto
143500981009     C************************************************************
143600981009     C     F02S1         BEGSR
143700981009     C*
143800981009     C                   reset                   ana397ds
143900981009     C                   eval      societa397 = XSCSOC
144000981009     C                   eval      unita397 = unita087
144100981009     C                   eval      kcc397 = kcc087
144200981009     C                   eval      ksc397 = KSC087
144300981009     C                   eval      wdw397 = *on
144400981009     C                   movel     ana397ds      kpjbu
144500981009     C                   CALL      'ANA397R'                            21
144600981009     C                   PARM                    KPJBA
144700981009     c                   movel     kpjbu         ana397ds
144800981009     C*
144900981009     C                   ENDSR
145000981009     C/EJECT
145100940309     C/EJECT
145200940309     C************************************************************
145300940309     C* GESTIONE F03 VIDEO S1
145400940309     C************************************************************
145500940309     C     F03S1         BEGSR
145600940309     C*
145700940309     C                   MOVE      *ON           $FINE
145800951114    >C                   MOVE      '1'           RET087
145900940325     C* fine programma
146000940325     C                   EXSR      ENDPGM
146100940309     C*
146200940309     C                   ENDSR
146300951115     C/EJECT
146400951115     C************************************************************
146500951115     C* GESTIONE F08 VIDEO S1
146600951115     C************************************************************
146700951115     C     F08S1         BEGSR
146800951115     C*
146900951115     C                   MOVE      *ON           $FINE
147000951115    >C                   MOVE      '0'           RET087
147100951115     C*
147200951115     C                   ENDSR
147300951116     C************************************************************
147400951116     C* GESTIONE F11 VIDEO S1 - cambio moneta
147500951116     C************************************************************
147600951116     C     F11S1         BEGSR
147700951116     C*
147800951116     C* Eseguo solo se ci sono record nel sfl
147900951116     C                   IF        Wmax <> 0
148000951116     C* Scambio richiesta
148100951116     C                   select
148200951116     C                   when      Savdivvis = '0'
148300951116     C                   eval      Savdivvis = '1'
148400951116     C*
148500951116     C                   when      Savdivvis = '1'
148600951116     C                   eval      Savdivvis = '0'
148700951116     C                   endsl
148800951116     C*
148900951116     C                   DO        *hival        $X                4 0
149000960314     c*
149100960314     c                   select
149200960314     c                   when      $visdati = '1'
149300951116     C     $X            chain     c087s1                             21
149400960314     c                   when      $visdati = '2'
149500960314     C     $X            chain     c087s2                             21
149600960314     c                   endsl
149700960314     C*
149800951116     C   21              LEAVE
149900951116     C                   select
150000951116     C                   when      Savdivvis = '0'
150100951116     C                   eval      s1importo1= h1importom
150200951116     C                   eval      s1importo2= h1importod
150300951116     c*
150400951116     C                   when      Savdivvis = '1'
150500951116     C                   eval      s1importo1= h1importod
150600951116     C                   eval      s1importo2= h1importom
150700951116     C                   endsl
150800951116     C*
150900951116     C* se le divise sono uguali, imposto il valore del campo in drop
151000951116     C* uguale a quello della prima riga, così almeno sono allineati
151100951116     C                   IF        C1divisa  = C1monconto
151200951116     C                   eval      s1importo2= s1importo1
151300951116     C                   endif
151400951116     C* imposto quegli attributi del sfl che sono modificabili
151500951116     C* al richiamo del determinato pgm
151600960626     c                   if        s1opz  <> 0
151700960626     C                   seton                                        32
151800960627     c                   else
151900960627     c                   setoff                                       32
152000960626     c                   endif
152100951116     C                   EXSR      ATRS1
152200960627     c* Spengo  r.i. per campo opzione
152300960627     c                   setoff                                       40
152400951116     C*
152500960314     C                   SELECT
152600960314     C                   when      $visdati = '1'
152700951116     C                   UPDATE    C087S1
152800960314     c                   when      $visdati = '2'
152900960314     C                   UPDATE    C087S2
153000960314     c                   endsl
153100951116     C*
153200951116     C                   ENDDO
153300951116     C*
153400951116     C                   ENDIF
153500951116     C                   ENDSR
153600940309     C/EJECT
153700940309     C************************************************************
153800940309     C* GESTIONE F12 VIDEO S1
153900940309     C************************************************************
154000940309     C     F12S1         BEGSR
154100940309     C*
154200940309     C                   MOVE      *ON           $FINE
154300951114    >C                   MOVE      '2'           RET087
154400940309     C*
154500940309     C                   ENDSR
154600951109     C/EJECT
154700951109     C************************************************************
154800951114     C* GESTIONE F20 VIDEO S1
154900951109     C************************************************************
155000951115     C     F15S1         BEGSR
155100951109     C*
155200951115     C                   reset                   ana399ds
155300951115     C                   eval      A39societa = XSCSOC
155400951115     C                   eval      A39unita = unita087
155500951124     C                   eval      A39ctb = ctb087
155600951124     C                   eval      A39kcc = kcc087
155700951124     C                   eval      A39ksc = KSC087
155800951124     C                   eval      A39SNATURA = snatura087
155900951115     C                   movel     ana399ds      kpjbu
156000951115     C                   CALL      'ANA399R'                            21
156100951115     C                   PARM                    KPJBA
156200951115     C     A39ERRORI     IFEQ      '0'
156300951115     C                   endif
156400951109     C*
156500951109     C                   ENDSR
156600940309     C/EJECT
156700940309     C************************************************************
156800940309     C* GESTIONE F23 VIDEO S1
156900940309     C************************************************************
157000940309     C     F23S1         BEGSR
157100940309     C*
157200941220    >C                   Z-ADD     76            $L01
157300941220    >C                   Z-ADD     76            $L02
157400940309     C                   EXSR      RIEOPZ
157500941214     C                   Z-ADD     23            X
157600951109     C                   MOVE      FF23          $FC1(X)
157700940608     C                   MOVEL     WOPZ1         C1OPZ1
157800060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
157900951109     C                   MOVEL     WOPZ2         C1OPZ2
158000940309     C                   Z-ADD     WCOZ          C1COZ
158100940309     C*
158200940309     C                   ENDSR
158300940309     C/EJECT
158400940309     C************************************************************
158500940309     C* GESTIONE F24 VIDEO S1
158600940309     C************************************************************
158700940309     C     F24S1         BEGSR
158800940506     C*
158900951109     C                   MOVEA     $FX1          $FX
159000951109     C                   MOVEA     $FC1          $FC
159100951109     C                   MOVEA     $FR1          $FR
159200940506     C                   Z-ADD     $ULKS1        $ULK
159300941220    >C                   Z-ADD     79            $L01
159400941220    >C                   Z-ADD     79            $L02
159500940506     C                   EXSR      RIEKEY
159600940506     C                   Z-ADD     $ULK          $ULKS1            3 0
159700951109     C                   MOVEA     $FC           $FC1
159800951109     C                   MOVEL     $KEY1         Z1KE1
159900060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
160000951109     C                   MOVEL     $KEY2         Z1KE2
160100951114     C                   WRITE     C087Z1
160200940506     C*
160300940309     C                   ENDSR
160400940128     C/EJECT
160500940128     C************************************************************
160600940131     C* CONTROLLO TESTATA LISTA
160700940128     C************************************************************
160800940131     C     CTRC1         BEGSR
160900940128     C*
161000940201     C                   MOVE      *OFF          *IN99
161100951109     C*
161200940131     C*
161300940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
161400950707     C* L'INIZIALIZZAZIONE LISTA   (lo attivo anche se w1pos = c1pos, per ri
161500950707     C* partire dal primo numero partita
161600940202     C*
161700940202     C                   ENDSR
161800940131     C/EJECT
161900940131     C************************************************************
162000940131     C* CONTROLLO OPZIONI LISTA
162100940131     C************************************************************
162200940131     C     CTRS1         BEGSR
162300940131     C*
162400940202     C                   MOVEL     *OFF          $ESCI
162500940201     C                   SETOFF                                       99
162600940131     C                   Z-ADD     0             S1OPZ
162700940131     C*
162800940127     C* Leggo il sfl solo se ci sono rcd
162900941118   B1C     WMAX          IFGT      0
163000960314     c                   select
163100960314     c                   when      $visdati = '1'
163200951114     C                   READC     C087S1                                 21
163300960314     c                   when      $visdati = '2'
163400960314     C                   READC     C087S2                                 21
163500960314     c                   endsl
163600940127     C*
163700940131     C* esce se fine sfl o errore che richiede l'uscita
163800941118   B2C     *IN21         DOWEQ     *OFF
163900940131     C     $ESCI         ANDEQ     *OFF
164000940201     C                   Z-ADD     S1NRR         C1RCD
164100940131     C* ctrl su riga
164200940131     C                   EXSR      RECS1
164300940131     C* gestione opzioni
164400941118   B3C     S1OPZ         IFNE      0
164500940201     C     *IN99         ANDEQ     *OFF
164600940131     C                   EXSR      OPZS1
164700941118   E3C                   ENDIF
164800940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
164900941118   B3C     *IN99         IFEQ      *ON
165000940131     C                   MOVE      *ON           *IN32
165100941118   E3C                   ENDIF
165200950301     C*
165300950301     C* imposto quegli attributi del sfl che sono modificabili
165400950301     C* al richiamo del determinato pgm
165500941118     C                   EXSR      ATRS1
165600941118     C*
165700940131     C                   Z-ADD     0             S1OPZ
165800940223     C*
165900960314     c                   select
166000960314     c                   when      $visdati = '1'
166100951114     C                   UPDATE    C087S1
166200960314     c                   when      $visdati = '2'
166300960314     C                   UPDATE    C087S2
166400960314     c                   endsl
166500941125     C                   MOVE      *OFF          *IN32
166600940223     C*
166700940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
166800941118   B3C     $ESCI         IFEQ      *OFF
166900960314     c                   select
167000960314     c                   when      $visdati = '1'
167100951114     C                   READC     C087S1                                 21
167200960314     c                   when      $visdati = '2'
167300960314     C                   READC     C087S2                                 21
167400960314     c                   endsl
167500940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
167600941118   B4C     *IN21         IFEQ      *ON
167700940201     C                   MOVE      WINZS1        $INZS1
167800050608D1830C                   MOVE      *off          WINZS1
167900050608  "  C* se è avvenuta almeno una modifica
168000050608D1830C                   IF        $INZS1 = *on
168100940204     C* calcolo pagina a cui deve posizionarsi
168200940728     C     WSFL          IFGT      0
168300940728     C                   EXSR      CLCPAG
168400940728     C                   ELSE
168500940728     C                   Z-ADD     SFLPAG        WPAG
168600940728     C                   ENDIF
168700941118   E4C                   ENDIF
168800941118   E4C                   ENDIF
168900050608D1830C                   ENDIF
169000940131     C*
169100941118   E2C                   ENDDO
169200940127     C*
169300941118   E1C                   ENDIF
169400940131     C*
169500940127     C                   ENDSR
169600940127     C/EJECT
169700940127     C************************************************************
169800940131     C* CONTROLLO CAMPI I/O RIGA LISTA
169900940127     C************************************************************
170000940131     C     RECS1         BEGSR
170100940131     C*
170200940201     C* reset indicatori DSPATR
170300940201     C                   MOVE      *ALL'0'       IN4049           10
170400940201     C                   MOVEA     IN4049        *IN(40)
170500940201     C*
170600940131     C* Controllo se Utente Autorizzato alla scelta
170700940202     C     S1OPZ         IFNE      0
170800940131     C                   Z-ADD     1             X
170900940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
171000940211     C     *IN21         IFEQ      *OFF
171100940223     C     $FL1(X)       OREQ      'N'
171200940131     C                   MOVE      *ON           $ESCI
171300940201     C                   SETON                                        504099
171400940131     C                   ENDIF
171500940202     C                   ENDIF
171600951128     C*
171700951128     C* Se richiesto portafoglio, la causale deve essere di tipo emissione porta
171800970804     c* per i clienti
171900951128     C     S1OPZ         IFEQ      43
172000951128     C     *in99         andeq     *off
172100951128     C     PORTAFH       andne     '1'
172200970804     C     snatura087    andeq     'C'
172300951128     C                   MOVE      *ON           $ESCI
172400951128     C                   SETON                                        524099
172500951128     C                   ENDIF
172600970804     C*
172700970804     C* Se richiesto portafoglio, la causale non deve essere = a blank      orta
172800970804     c* per i fornitori
172900970804     C     S1OPZ         IFEQ      43
173000970804     C     *in99         andeq     *off
173100970804     C     PORTAFH       andeq     ' '
173200970804     C     snatura087    andeq     'F'
173300970804     C                   MOVE      *ON           $ESCI
173400970804     C                   SETON                                        524099
173500970804     C                   ENDIF
173600020118C1530C*
173700020118  "  C* controllo che l'opzione 80=visione dei documenti su Laguna sia
173800020118  "  C* data solo sulle registrazioni che ne hanno
173900020118  "  C     S1OPZ         IFEQ      80
174000020118  "  C     H1IdLgn       ANDEQ     0
174100020118  "  C                   SETON                                        504099
174200020118  "  C                   ENDIF
174300020326C1530C*
174400950706     C*
174500940131     C                   ENDSR
174600941118     C/EJECT
174700941118     C************************************************************
174800941118     C* GESTIONE OPZIONI LISTA
174900941118     C************************************************************
175000941118     C     OPZS1         BEGSR
175100950301     C*
175200950301     C                   Z-ADD     RIRI          CURR
175300950301     C                   Z-ADD     COCO          CURC
175400941118     C*
175500941027     C                   SELECT
175600940225     C*
175700950620     C* se opz=Selezione
175800950620     C* passo la chiave al pgm chiamante
175900940225     C     S1OPZ         WHENEQ    1
176000960327     C     S1OPZ         OREQ      10
176100951116     C* passare nei campi appositi della DS NDC087DS il valore
176200951116     C* della chiave della VL univoca (se c'è)(gli altri campi sono già
176300951116     C* impostati)
176400951116     C                   CALLb     'XDT4000'
176500951116     C     DTSCA087      PARM                    XDTAMG           10
176600051215C1892X***                PARM      S1DTSCA       XDTGMA            6 0
176700051215C1892C                   PARM      H1DTSCA       XDTGMA            6 0
176800951116     C                   PARM      4             XDTSTA            1 0
176900960123     c                   eval      ret087 = '3'
177000960319     C*
177100960319     C* Imposto la ds multipla se richiesto
177200960319     C                   If        multip087 = *on  and parms >=5
177300960319     C                   ADD       1             IndPar
177400960319     c                   if        indPar<=50
177500960319     c     indpar        occur     sKpartite
177600960319     c                   z-add     C1nrpar       SKNRpar
177700960319     c                   move      C1serpar      SKSerPar
177800960319     C                   CALLb     'XDT4000'
177900960319     C     skdtpar       PARM                    XDTAMG
178000960319     C                   PARM      C1dtpar       XDTGMA
178100960319     C                   PARM      4             XDTSTA
178200960327     c* se scelta con importo
178300960327     C     S1OPZ         ifEQ      10
178400960319     c                   z-ADD     H1saldodiv    skImportD
178500990115     c                   move      h1segnoD      sksegnoD
178600081014D2440C                   move      s1DivCoge     skdivisa
178700081014D2440c                   z-ADD     H1saldoMC     skImporto
178800960327     C                   endif
178900960319     C                   CALLb     'XDT4000'
179000960319     C     skdtsca       PARM                    XDTAMG
179100051215c1892x***                PARM      s1dtsca       XDTGMA
179200051215c1892c                   PARM      h1dtsca       XDTGMA
179300960319     C                   PARM      4             XDTSTA
179400960319     c                   endif
179500960319     C                   MOVE      *ON           $FINE
179600960319     c                   else
179700960319     C                   MOVE      *ON           $ESCI
179800960319     C                   MOVE      *ON           $FINE
179900960319     c                   endif
180000940909     C*
180100940909     C                   MOVE      *HIVAL        $LASTV
180200951115     C*
180300951115     C*
180400951115     C* se opz=21 richiamo analitica
180500951115     C     S1OPZ         WHENEQ    21
180600951115     C*
180700951115     C                   RESET                   NDC083DS
180800951115     C                   MOVEL     H1Sys         Sys083
180900951115     C                   MOVEL     H1NrAsReg     NrAsReg083
181000951115     C                   MOVEL     H1NrRigaM     NrRigaM083
181100951128     C                   MOVE      '02'          Opz083
181200951115     C*
181300951115     C                   movel     NDC083DS      KPJBU
181400951115     C                   CALL      'NDC083R'
181500951115     C                   PARM                    Kpjba
181600951115     C                   Movel     kpjbu         NDC083DS
181700951115     C*
181800951115     C                   Exsr      Rtn083
181900960318     C*
182000960318     C* se opz=22 richiamo visualizzazione partite
182100960318     C* passo la chiave al pgm chiamante
182200960318     C     S1OPZ         WHENEQ    22
182300960318     C*
182400960318     C                   RESET                   NDC073DS
182500960318     c                   move      xscsoc        societa073
182600960318     C                   MOVEL     H1Sys         Sys073
182700960318     C                   MOVEL     H1NrAsReg     NrAsReg073
182800960318     C                   MOVEL     H1NrRigaM     NrRigaM073
182900960318     C                   MOVEL     H1NrRigaP     NrRigaP073
183000960318     C                   movel     descr087      kcckscd073
183100960412     c                   eval      snatura073 = snatura087
183200960318     C                   move      winz073       exf073
183300960318     C                   MOVE      '05'          Opz073
183400960318     C*
183500960318     C                   movel     NDC073DS      KPJBU
183600960318     C                   CALL      'NDC073R'
183700960318     C                   PARM                    Kpjba
183800960318     C                   Movel     kpjbu         NDC073DS
183900960318     C*
184000960318     C                   Exsr      Rtn073
184100051109     C*
184200051109c1881c* se opz=tesoreria
184300051109  "  C     S1OPZ         WHENEQ    23
184400051109  "  C*
184500051109  "  C                   RESET                   NDC084DS
184600051109  "  C                   MOVEL     H1Sys         Sys084
184700051109  "  C                   z-add     H1NrAsReg     NrAsReg084
184800051109  "  C                   z-add     H1NrRigaM     NrRigaM084
184900051109  "  C                   MOVE      '02'          Opz084
185000051109  "  C                   move      wdw084wrk     exf084
185100051109  "  C*
185200051109  "  C                   movel     NDC084DS      KPJBU
185300051109  "  C                   CALL      'NDC084R'
185400051109  "  C                   PARM                    Kpjba
185500051109  "  C                   Movel     kpjbu         NDC084DS
185600051109  "  C*
185700051109c1881C                   Exsr      Rtn084
185800051109     c*
185900951205     C*
186000951205     C* se opz=43 chiamo portafoglio attivo se cliente o passivo se fornitor
186100951205     C     S1OPZ         WHENEQ    43
186200951219     C* portafoglio attivo
186300951219     c                   select
186400951219     c                   when      snatura087 = 'C'
186500961121     c                   reset                   x48chPgmds
186600961121     C                   eval      x48pgm = 'NDCE01R'
186700961121     c                   eval      kpjbu = x48chpgmds
186800961121     c                   call      'X48CHPGM'
186900961121     c                   parm                    kpjba
187000961121     c                   movel     kpjbu         x48chpgmds
187100961121     c                   if        x48errore = *off
187200951219     C                   RESET                   NDCE01DS
187300951219     C                   eval      opze01    = '02'
187400951219     C                   eval      societae01= xscsoc
187500951219     C                   eval      ctbe01    = ctb087
187600951219     C                   eval      nreffE01  = s1nrreg
187700020911D1197c                   eval      UnitaE01 = s1unita
187800951219     C                   movel     NDCE01DS      Kpjbu
187900951219     C                   CALL      'NDCE01R'
188000951219     C                   PARM                    KPJBA
188100951219     C                   movel     kpjbu         NDce01ds
188200951219     C*
188300951205     C* ritorno da PGM gestione
188400951219     C                   EXSR      RTNCE01
188500961121     C                   endif
188600951205     c* Portafoglio passivo
188700951205     c                   when      snatura087 = 'F'
188800961121     c                   reset                   x48chPgmds
188900961121     C                   eval      x48pgm = 'NDCH01R'
189000961121     c                   eval      kpjbu = x48chpgmds
189100961121     c                   call      'X48CHPGM'
189200961121     c                   parm                    kpjba
189300961121     c                   movel     kpjbu         x48chpgmds
189400961121     c                   if        x48errore = *off
189500951219     C                   RESET                   NDCH01DS
189600951219     C                   eval      opzH01    = '02'
189700951219     C                   eval      societaH01= xscsoc
189800951219     C                   eval      ctbH01    = ctb087
189900951219     C                   eval      nreffH01  = s1nrreg
190000020911D1197c                   eval      UnitaH01 = s1unita
190100951219     C                   movel     NDCH01DS      Kpjbu
190200951219     C                   CALL      'NDCH01R'
190300951219     C                   PARM                    KPJBA
190400951219     C                   movel     kpjbu         NDcH01ds
190500951205     C* ritorno da PGM gestione
190600951205     C                   EXSR      RTNCH01
190700961121     C                   endif
190800951205     C                   endsl
190900951115     C*
191000951116     C* se opz=44 o 45 descrizioni aggiuntive
191100951116     C     S1OPZ         WHENEQ    44
191200951116     C     S1OPZ         oreq      45
191300951115     C                   RESET                   NDC027DS
191400951115     C*
191500951115     C                   MOVEL     '02'          OPZ027
191600951115     C* passare nei campi appositi della DS NDC027DS il valore
191700951115     C* della chiave della VL univoca
191800951115     C                   MOVEL     H1sys         Sys027
191900951115     C                   MOVEL     h1nrasreg     NrAsReg027
192000951116     c                   if        S1opz = 45
192100951115     C                   MOVEL     h1nrrigam     NrRigaM027
192200951116     C                   MOVEL     'M'           TipDes027
192300951116     C                   else
192400951116     C                   MOVEL     'T'           TipDes027
192500951116     C                   endif
192600951115     C* Faccio eseguire il commit al pgm chiamato
192700951115     C                   MOVE      '0'           Cmt027
192800951115     C                   MOVEL     *on           Inz027
192900951115     C*
193000951115     C* passo tutti quei campi che possono servire al pgm
193100951115     C                   MOVEL     NDC027DS      KPJBU
193200951115     C                   CALL      'NDC027R'
193300951115     C                   PARM                    KPJBA
193400951115     C                   MOVEL     KPJBU         NDC027DS
193500951115     C* ritorno da PGM gestione
193600951115     C                   EXSR      RTN027
193700951115     C*
193800951115     C* Richiamo prima nota
193900951115     C     S1OPZ         WHENEQ    41
194000961121     c*controllo chiamata ricorsiva p.nota
194100961121     c                   reset                   x48chPgmds
194200961121     C                   eval      x48pgm = 'NDC001R'
194300961121     c                   eval      kpjbu = x48chpgmds
194400961121     c                   call      'X48CHPGM'
194500961121     c                   parm                    kpjba
194600961121     c                   movel     kpjbu         x48chpgmds
194700961121     c                   if        x48errore = *off
194800951115      *
194900951127     C                   RESET                   NDC001DS
195000951127     C                   MOVEL     '02'          OPZ001
195100951115     C* passare nei campi appositi della DS C004DS il valore
195200951115     C* della chiave della VL univoca
195300951127     C                   MOVEL     H1Sys         UQ1001
195400951127     C                   MOVEL     H1NrAsReg     UQ2001
195500951115     C*
195600961121     C                   movel     NDC001DS      KPJBU
195700951127     C                   CALL      'NDC001R'
195800951115     C                   PARM                    Kpjba
195900951127     C                   movel     KPJBU         ndc001ds
196000951115     C* ritorno da PGM gestione
196100951127     C                   EXSR      RTN001
196200961121     c                   endif
196300011213C1530C*
196400020117  "  C* Richiamo Laguna per visualizzazione documento
196500020118  "  C     S1OPZ         WHENEQ    80
196600020118  "  C                   MOVEL     H1ClassLg     Parm670(1)
196700021120C1530C                   MOVEL     H1IdLgn       Parm670(2)
196800021120C1605C                   MOVEL     knsif         Parm670(3)
196900071019C2055x*** ex C1530       CALL      'ANA670R'
197000071019C2055C                   CALL      'PJXX038R'
197100071019C1530C                   PARM                    xscsoc
197200020118  "  C                   PARM      'VIEW'        Cmd670
197300020117  "  C                   PARM                    Parm670
197400020117  "  C                   PARM                    Err670
197500020117  "  C                   PARM                    Ret670
197600020118  "  C* ritorno da PGM gestione
197700020118  "  C                   EXSR      RTN670
197800020117C1530C*
197900951115     C                   ENDSL
198000951115     C*
198100951115     C                   ENDSR
198200951115     C/EJECT
198300951115     C************************************************************
198400951115     C* GESTIONE RITORNO DA PGM DESCRIZIONI AGGIUNTIVE
198500951115     C************************************************************
198600951115     C     RTN027        BEGSR
198700951115     C*
198800951115     C* modo di ritorno
198900951115     C*
199000020118   B1C                   SELECT
199100951115     C* 1 = F3
199200951115     C     RET027        WHENEQ    '1'
199300951115     C                   MOVE      *ON           $ESCI
199400951115     C                   MOVE      *ON           $FINE
199500951115     C                   MOVE      '1'           RET087
199600951115     C* 2 = F12
199700951115     C     RET027        WHENEQ    '2'
199800970624     C     opr027        andne     '1'
199900951115     C                   MOVE      *ON           $ESCI
200000951115     C*
200100951115   E1C                   ENDSL
200200951115     C*
200300951115     C* operazione eseguite dal pgm chiamato
200400951115     C*
200500951115    >C                   MOVE      OPR027        WOPR
200600951115     C                   SELECT
200700951115     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
200800951115     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
200900951115    >C     OPR027        WHENEQ    '1'
201000951115     C* verifico se esistono descrizioni aggiuntive(riga)
201100951116     C* non lo faccio perchè non verranno quasi mai modificate
201200951116     C*****              EXSR      TestDesAGG
201300951115     C*
201400951115     C                   ENDSL
201500951115     C*
201600951115     C* funzione non eseguibile per errore :
201700951115     C*
201800951115     C                   SELECT
201900951115     C* 1 = funzione richiamata chiusa in errore
202000951115    >C     ERR027        WHENEQ    '1'
202100951115     C                   MOVE      *ON           $ESCI
202200951115     C                   SETON                                        9599
202300951115     C* 2 = funzione richiamata senza la dovuta autorizzazione
202400951115    >C     ERR027        WHENEQ    '2'
202500951115     C                   MOVE      *ON           $ESCI
202600951115     C                   SETON                                        9499
202700951115     C*
202800951115     C                   ENDSL
202900951115     C*
203000951115     C                   ENDSR
203100951115     C/EJECT
203200951115     C************************************************************
203300951115     C* GESTIONE RITORNO DA PGM primanota generica
203400951115     C************************************************************
203500951127     C     RTN001        BEGSR
203600951115     C*
203700951115     C* modo di ritorno
203800951115     C*
203900951115   B1C                   SELECT
204000951115     C* 1 = F3
204100951127     C     RET001        WHENEQ    '1'
204200951115     C                   MOVE      *ON           $ESCI
204300951115     C                   MOVE      *ON           $FINE
204400951115     C                   MOVE      '1'           RET087
204500951115     C* 2 = F12
204600951127     C     RET001        WHENEQ    '2'
204700960125     C     opr001        ANDNE     '1'
204800951115     C                   MOVE      *ON           $ESCI
204900951115     C*
205000951115   E1C                   ENDSL
205100951115     C*
205200951115     C* operazione eseguite dal pgm chiamato
205300951115     C*
205400951127    >C                   MOVE      OPR001        WOPR
205500951115     C                   SELECT
205600951115     C* 1 = eseguito aggiornamento (richiesto cariamento subfile
205700951115     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
205800951127    >C     OPR001        WHENEQ    '1'
205900951115     C                   move      *ON           winzs1
206000951115     C*
206100951115     C                   ENDSL
206200951115     C*
206300951115     C* funzione non eseguibile per errore :
206400951115     C*
206500951115     C                   SELECT
206600951115     C* 1 = funzione richiamata chiusa in errore
206700951127    >C     ERR001        WHENEQ    '1'
206800951115     C                   MOVE      *ON           $ESCI
206900951115     C                   SETON                                        9599
207000951115     C* 2 = funzione richiamata senza la dovuta autorizzazione
207100951127    >C     ERR001        WHENEQ    '2'
207200951115     C                   MOVE      *ON           $ESCI
207300951115     C                   SETON                                        9499
207400951116     C* 3 = registrazione non modificabile
207500951127     C     ERR001        WHENEQ    '3'
207600951116     C                   MOVE      *ON           $ESCI
207700951116     C                   SETON                                        9399
207800951115     C*
207900951115     C                   ENDSL
208000951115     C*
208100951115     C*
208200940131     C                   ENDSR
208300951115     C/EJECT
208400951115     C************************************************************
208500951115     C* GESTIONE RITORNO DA PGM analitica
208600951115     C************************************************************
208700951115     C     RTN083        BEGSR
208800951115     C*
208900951115     C* modo di ritorno
209000951115     C*
209100951115   B1C                   SELECT
209200951115     C* 1 = F3
209300951115     C     RET083        WHENEQ    '1'
209400951115     C                   MOVE      *ON           $ESCI
209500951115     C                   MOVE      *ON           $FINE
209600951115     C                   MOVE      '1'           RET087
209700951115     C* 2 = F12
209800951115     C     RET083        WHENEQ    '2'
209900970624     c     opr083        andne     '1'
210000951115     C                   MOVE      *ON           $ESCI
210100951115     C*
210200951115   E1C                   ENDSL
210300951115     C*
210400951115     C* operazione eseguite dal pgm chiamato
210500951115     C*
210600951115    >C                   MOVE      OPR083        WOPR
210700951115     C                   SELECT
210800951115     C* 1 = eseguito aggiornamento non faccio niente
210900951115    >C     OPR083        WHENEQ    '1'
211000960129     C                   move      *ON           winzs1
211100951115     C*
211200951115     C                   ENDSL
211300951115     C*
211400951115     C* funzione non eseguibile per errore :
211500951115     C*
211600951115     C                   SELECT
211700951115     C* 1 = funzione richiamata chiusa in errore
211800951115    >C     ERR083        WHENEQ    '1'
211900951115     C                   MOVE      *ON           $ESCI
212000951115     C                   SETON                                        9599
212100951115     C* 2 = funzione richiamata senza la dovuta autorizzazione
212200951115    >C     ERR083        WHENEQ    '2'
212300951115     C                   MOVE      *ON           $ESCI
212400951115     C                   SETON                                        9499
212500951115     C*
212600951115     C                   ENDSL
212700951115     C*
212800951115     C                   ENDSR
212900960318     C/EJECT
213000051109c1881C************************************************************
213100051109     C* GESTIONE RITORNO DA PGM tesoreria
213200051109c1881C************************************************************
213300051109     C     RTN084        BEGSR
213400051109     C*
213500051109     C* modo di ritorno
213600051109     C*
213700051109   B1C                   SELECT
213800051109     C* 1 = F3
213900051109     C     RET084        WHENEQ    '1'
214000051109     C                   MOVE      *ON           $ESCI
214100051109     C                   MOVE      *ON           $FINE
214200051109     C                   MOVE      '1'           RET087
214300051109     C* 2 = F12
214400051109     C     RET084        WHENEQ    '2'
214500051109     c     opr084        andne     *on
214600051109     C                   MOVE      *ON           $ESCI
214700051109     C*
214800051109   E1C                   ENDSL
214900051109     C*
215000051109     C* operazione eseguite dal pgm chiamato
215100051109     C*
215200051109    >C                   MOVE      OPR084        WOPR
215300051109     C                   SELECT
215400051109     C* 1 = eseguito aggiornamento
215500051109    >C     OPR084        WHENEQ    '1'
215600051109     C                   move      *ON           winzs1
215700051109     C*
215800051109     C                   ENDSL
215900051109     C*
216000051109     C* funzione non eseguibile per errore :
216100051109     C*
216200051109     C                   SELECT
216300051109     C* 1 = funzione richiamata chiusa in errore
216400051109    >C     ERR084        WHENEQ    '1'
216500051109     C                   MOVE      *ON           $ESCI
216600051109     C                   SETON                                        9599
216700051109     C* 2 = funzione richiamata senza la dovuta autorizzazione
216800051109    >C     ERR084        WHENEQ    '2'
216900051109     C                   MOVE      *ON           $ESCI
217000051109     C                   SETON                                        9499
217100051109     C*
217200051109     C                   ENDSL
217300051109     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
217400051109     C* di sola finestra
217500051109     C                   MOVEL     '0'           wdw084wrk
217600051109     C*
217700051109c1881C                   ENDSR
217800051109     C/EJECT
217900960318     C************************************************************
218000960318     C* GESTIONE RITORNO DA PGM PARTITE
218100960318     C************************************************************
218200960318     C     RTN073        BEGSR
218300960318     C*
218400960318     C* modo di ritorno
218500960318     C*
218600960318   B1C                   SELECT
218700960318     C* 1 = F3
218800960318     C     RET073        WHENEQ    '1'
218900960318     C                   MOVE      *ON           $ESCI
219000960318     C                   MOVE      *ON           $FINE
219100960318     C                   MOVE      '1'           RET087
219200960318     C* 2 = F12
219300960318     C     RET073        WHENEQ    '2'
219400970624     c     opr073        andne     '1'
219500960318     C                   MOVE      *ON           $ESCI
219600960318     C*
219700960318   E1C                   ENDSL
219800960318     C*
219900960318     C* operazione eseguite dal pgm chiamato
220000960318     C*
220100960318    >C                   MOVE      OPR073        WOPR
220200960318     C                   SELECT
220300960318     C* 1 = eseguito aggiornamento non faccio niente
220400960318    >C     OPR073        WHENEQ    '1'
220500960318     C                   move      *ON           winzs1
220600960318     C*
220700960318     C                   ENDSL
220800960318     C*
220900960318     C* funzione non eseguibile per errore :
221000960318     C*
221100960318     C                   SELECT
221200960318     C* 1 = funzione richiamata chiusa in errore
221300960318    >C     ERR073        WHENEQ    '1'
221400960318     C                   MOVE      *ON           $ESCI
221500960318     C                   SETON                                        9599
221600960318     C* 2 = funzione richiamata senza la dovuta autorizzazione
221700960318    >C     ERR073        WHENEQ    '2'
221800960318     C                   MOVE      *ON           $ESCI
221900960318     C                   SETON                                        9499
222000960318     C*
222100960318     C                   ENDSL
222200960318     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
222300960318     C* di sola finestra
222400960318     C                   MOVEL     '0'           winz073
222500960318     C*
222600960318     C                   ENDSR
222700951205     C/EJECT
222800951205     C************************************************************
222900951205     C* GESTIONE RITORNO DA PGM PORTAFOGLIO ATTIVO
223000951205     C************************************************************
223100951219     C     RTNCE01       BEGSR
223200951205     C*
223300951205     C* modo di ritorno
223400951205     C*
223500951205   B1C                   SELECT
223600951219     C* 1 = F3
223700951219     C     RETE01        WHENEQ    '1'
223800951205     C                   MOVE      *ON           $ESCI
223900951205     C                   MOVE      *ON           $FINE
224000951205     C                   MOVE      '1'           RET087
224100951219     C* 2 = F12
224200951219     C     RETE01        WHENEQ    '2'
224300970211     C     oprE01        ANDNE     '1'
224400951205     C                   MOVE      *ON           $ESCI
224500951205     C*
224600951205   E1C                   ENDSL
224700970211     C*
224800970211     C* operazione eseguite dal pgm chiamato
224900970211     C*
225000970211    >C                   MOVE      OPRE01        WOPR
225100970211     C                   SELECT
225200970211     C* 1 = eseguito aggiornamento (richiesto cariamento subfile
225300970211     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
225400970211    >C     OPRE01        WHENEQ    '1'
225500970211     C                   move      *ON           winzs1
225600970211     C*
225700970211     C                   ENDSL
225800951205     C*
225900951205     C* funzione non eseguibile per errore :
226000951205     C*
226100951205     C                   SELECT
226200951205     C* 1 = funzione richiamata chiusa in errore
226300951219    >C     ERRE01        WHENEQ    '1'
226400951205     C                   MOVE      *ON           $ESCI
226500951205     C                   SETON                                        9599
226600951205     C*
226700951205     C                   ENDSL
226800951205     C*
226900951205     C                   ENDSR
227000951205     C/EJECT
227100951205     C************************************************************
227200951205     C* GESTIONE RITORNO DA PGM PORTAFOGLIO PASSIVO
227300951205     C************************************************************
227400951205     C     RTNCH01       BEGSR
227500951205     C*
227600951205     C* modo di ritorno
227700951205     C*
227800951205   B1C                   SELECT
227900951219     C* 1 = F3
228000951219     C     RETH01        WHENEQ    '1'
228100951205     C                   MOVE      *ON           $ESCI
228200951205     C                   MOVE      *ON           $FINE
228300951205     C                   MOVE      '1'           RET087
228400951219     C* 2 = F12
228500951219     C     RETH01        WHENEQ    '2'
228600970211     C     oprH01        ANDNE     '1'
228700951205     C                   MOVE      *ON           $ESCI
228800951205     C*
228900951205   E1C                   ENDSL
229000970211     C*
229100970211     C* operazione eseguite dal pgm chiamato
229200970211     C*
229300970211    >C                   MOVE      OPRh01        WOPR
229400970211     C                   SELECT
229500970211     C* 1 = eseguito aggiornamento (richiesto cariamento subfile
229600970211     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
229700970211    >C     OPRh01        WHENEQ    '1'
229800970211     C                   move      *ON           winzs1
229900970211     C*
230000970211     C                   ENDSL
230100951205     C*
230200951205     C* funzione non eseguibile per errore :
230300951205     C*
230400951205     C                   SELECT
230500951205     C* 1 = funzione richiamata chiusa in errore
230600951219    >C     ERRH01        WHENEQ    '1'
230700951205     C                   MOVE      *ON           $ESCI
230800951205     C                   SETON                                        9599
230900951205     C*
231000951205     C                   ENDSL
231100951205     C*
231200951205     C                   ENDSR
231300020118C1530C************************************************************
231400020118  "  C* GESTIONE RITORNO DA PGM visualizzazione documenti da Laguna
231500020118  "  C************************************************************
231600020118C1530C     RTN670        BEGSR
231700020118     C*
231800020118     C* modo di ritorno
231900020118     C*
232000020118     C* al ritorno da questa opzione non continuo il ciclo
232100020118     C                   MOVE      *ON           $ESCI
232200020118     C*
232300020118C1530C                   ENDSR
232400941205     C/EJECT
232500941205     C************************************************************
232600941205     C* CONTROLLO ABILITAZIONE TASTI
232700941205     C* INPUT
232800941205     C* $TASTO =Tasto funzione premuto
232900941205     C* $FC = Tasti abilitati
233000941205     C* OUTPUT
233100941205     C* IN99 = ERrore
233200941205     C************************************************************
233300941205     C     CTRKEY        BEGSR
233400941205     C*
233500941205     C                   SELECT
233600941205     C     $TASTO        WHENEQ    F01
233700941205     C     $FC(1)        IFEQ      '0'
233800941205     C                   SETON                                        99
233900941205     C                   ENDIF
234000941205     C     $TASTO        WHENEQ    F02
234100941205     C     $FC(2)        IFEQ      '0'
234200941205     C                   SETON                                        99
234300941205     C                   ENDIF
234400941205     C     $TASTO        WHENEQ    F03
234500941205     C     $FC(3)        IFEQ      '0'
234600941205     C                   SETON                                        99
234700941205     C                   ENDIF
234800941205     C     $TASTO        WHENEQ    F04
234900941205     C     $FC(4)        IFEQ      '0'
235000941205     C                   SETON                                        99
235100941205     C                   ENDIF
235200941205     C     $TASTO        WHENEQ    F05
235300941205     C     $FC(5)        IFEQ      '0'
235400941205     C                   SETON                                        99
235500941205     C                   ENDIF
235600941205     C     $TASTO        WHENEQ    F06
235700941205     C     $FC(6)        IFEQ      '0'
235800941205     C                   SETON                                        99
235900941205     C                   ENDIF
236000941205     C     $TASTO        WHENEQ    F07
236100941205     C     $FC(7)        IFEQ      '0'
236200941205     C                   SETON                                        99
236300941205     C                   ENDIF
236400941205     C     $TASTO        WHENEQ    F08
236500941205     C     $FC(8)        IFEQ      '0'
236600941205     C                   SETON                                        99
236700941205     C                   ENDIF
236800941205     C     $TASTO        WHENEQ    F09
236900941205     C     $FC(09)       IFEQ      '0'
237000941205     C                   SETON                                        99
237100941205     C                   ENDIF
237200941205     C     $TASTO        WHENEQ    F10
237300941205     C     $FC(10)       IFEQ      '0'
237400941205     C                   SETON                                        99
237500941205     C                   ENDIF
237600941205     C     $TASTO        WHENEQ    F11
237700941205     C     $FC(11)       IFEQ      '0'
237800941205     C                   SETON                                        99
237900941205     C                   ENDIF
238000941205     C     $TASTO        WHENEQ    F12
238100941205     C     $FC(12)       IFEQ      '0'
238200941205     C                   SETON                                        99
238300941205     C                   ENDIF
238400941205     C     $TASTO        WHENEQ    F13
238500941205     C     $FC(13)       IFEQ      '0'
238600941205     C                   SETON                                        99
238700941205     C                   ENDIF
238800941205     C     $TASTO        WHENEQ    F14
238900941205     C     $FC(14)       IFEQ      '0'
239000941205     C                   SETON                                        99
239100941205     C                   ENDIF
239200941205     C     $TASTO        WHENEQ    F15
239300941205     C     $FC(15)       IFEQ      '0'
239400941205     C                   SETON                                        99
239500941205     C                   ENDIF
239600941205     C     $TASTO        WHENEQ    F16
239700941205     C     $FC(16)       IFEQ      '0'
239800941205     C                   SETON                                        99
239900941205     C                   ENDIF
240000941205     C     $TASTO        WHENEQ    F17
240100941205     C     $FC(17)       IFEQ      '0'
240200941205     C                   SETON                                        99
240300941205     C                   ENDIF
240400941205     C     $TASTO        WHENEQ    F18
240500941205     C     $FC(18)       IFEQ      '0'
240600941205     C                   SETON                                        99
240700941205     C                   ENDIF
240800941205     C     $TASTO        WHENEQ    F19
240900941205     C     $FC(19)       IFEQ      '0'
241000941205     C                   SETON                                        99
241100941205     C                   ENDIF
241200941205     C     $TASTO        WHENEQ    F20
241300941205     C     $FC(20)       IFEQ      '0'
241400941205     C                   SETON                                        99
241500941205     C                   ENDIF
241600941205     C     $TASTO        WHENEQ    F21
241700941205     C     $FC(21)       IFEQ      '0'
241800941205     C                   SETON                                        99
241900941205     C                   ENDIF
242000941205     C     $TASTO        WHENEQ    F22
242100941205     C     $FC(22)       IFEQ      '0'
242200941205     C                   SETON                                        99
242300941205     C                   ENDIF
242400941205     C     $TASTO        WHENEQ    F23
242500941205     C     $FC(23)       IFEQ      '0'
242600941205     C                   SETON                                        99
242700941205     C                   ENDIF
242800941205     C     $TASTO        WHENEQ    F24
242900941205     C     $FC(24)       IFEQ      '0'
243000941205     C                   SETON                                        99
243100941205     C                   ENDIF
243200941205     C                   ENDSL
243300941205     C*
243400941205     C                   ENDSR
243500940223     C/EJECT
243600940131     C************************************************************
243700940131     C* OPERAZIONI INIZIALI
243800940131     C************************************************************
243900940131     C     *INZSR        BEGSR
244000000118C1309C     *DMY          Move      Udate         ZDateISO
244100000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
244200940131     C*
244300940127     C* Reperimento parametri
244400940127     C*
244500940117     C     *ENTRY        PLIST
244600940117     C                   PARM                    KPJBA
244700951114     c                   PARM                    ndc087ds
244800951109     C                   PARM                    SOC001
244900950410     C                   PARM                    DSAUT
245000960319     C                   PARM                    skpartite
245100950410     C*
245200951124     C     PARMS         IFLT      4
245300950410     C                   MOVE      *BLANK        AUTDS
245400950410     C                   ELSE
245500950410     C                   MOVEL     DSAUT         AUTDS
245600950410     C                   ENDIF
245700951108     C*
245800951108     C* Reperimento autorizzazioni
245900951108     C* le autorizzazioni potrebbero essere già state passate,
246000951108     C* nel qual caso non vanno reperite
246100951108    >C     AUTDS         IFEQ      *BLANK
246200951108    >C                   EXSR      REPAUT
246300960930     c                   else
246400960930     c                   movel     xcadds        coneccds
246500951108     C                   ENDIF
246600940225     C*
246700940223     C* Reperimento opzioni
246800940127     C*
246900940128     C                   EXSR      REPOPZ
247000950406     C*
247100940506     C* Reperimento tasti di funzione
247200940506     C*
247300940506     C                   EXSR      REPKEY
247400940506     C*
247500951109     C* Udate in iso
247600951109     C                   CALLb     'XDT4000'
247700951109     C     WDATE         PARM                    XDTAMG
247800000118C1309C*****              PARM      UDATE         XDTGMA
247900000118C1309C                   PARM      ZDate         XDTGMA
248000951109     C                   PARM      4             XDTSTA
248100940127     C*
248200940117     C                   ENDSR
248300940223     C/EJECT
248400940223     C************************************************************
248500940223     C* INIZIALIZZAZIONE VARIABILI
248600940223     C************************************************************
248700940223     C     INZVAR        BEGSR
248800940506     C*
248900940506     C* Definizione delle opzioni utilizzabili a pgm
249000940506     C*
249100940506     C                   EXSR      DEFOPZ
249200960314     C*
249300960314     C* Reperimento sfl da visualizzare
249400960314     C*
249500960314     C                   EXSR      REPDFT
249600950330     C*
249700940223     C* Variabili per gestione videate
249800940223     C*
249900940223     C                   MOVE      *OFF          $FINE
250000940223     C                   MOVE      *OFF          $INZS1
250100950606     C                   MOVE      *OFF          $EndFile
250200940223     C                   MOVE      *OFF          $ESCI
250300940223     C                   MOVE      *OFF          $RCDOK
250400940509     C                   MOVE      *HIVAL        $LASTV
250500940506     C                   Z-ADD     0             $ULKS1            3 0
250600940318     C                   MOVE      *ON           $INZS1
250700951109     C                   MOVE      'S1'          $GEST
250800970624     c                   clear                   wopr
250900951122     C*
251000951122     C* Valorizzazione campi univoci testate
251100951122     C*
251200951122     C                   CLEAR                   C087T1
251300951122     C                   MOVEL     KNSIF         NOMSIF
251400951122     C                   MOVEL     XSCDSI        NOMDIT
251500951122     C                   MOVEL     DSPGM         NOMPGM
251600951113     C*
251700951114     C* Reperimento sottotitolo
251800951114     C                   MOVEL     *BLANK        T1TIT
251900951114     C     SOTTIT        IFNE      *BLANK
252000951114     C                   MOVEL     SOTTIT        IDMSG
252100951114     C                   CALLB     'XRTVM'
252200951114     C                   PARM                    IDMSG            27
252300951114     C                   PARM                    TXTMSG          100
252400951114     C                   PARM                    ERRMSG            1
252500951114     C                   PARM      *ON           CTRMSG            1
252600951114     C                   PARM      30            LENMSG            3 0
252700951114     C     ERRMSG        IFNE      *ON
252800951114     C                   MOVEL     TXTMSG        T1TIT
252900951114     C                   ELSE
253000951114     C                   MOVEL     *ALL'?'       T1TIT
253100951114     C                   ENDIF
253200951114     C                   ENDIF
253300951109     C*
253400951109     C* Reperimento dati moneta di conto
253500951109     C                   Eval      WDivisa =  XscDiv
253600951109     C                   Exsr      RepDivisa
253700970521     C                   Eval      NrDec   = X01NrDec
253800951116     C*
253900951116     C* Salvo divisa in entrata
254000951124     C                   EVAL      SavDivVis =   DivVis087
254100960319     c*
254200960319     c* pulisco indice e schiera partite
254300960319     c                   if        parms >=5 and multip087 = *on
254400960319     c                   z-ADD     0             IndPar            3 0
254500960319     c                   do        50            $X
254600960319     c     $x            occur     skpartite
254700960319     C                   CLEAR                   SkPartite
254800960319     C                   enddo
254900960319     C                   endif
255000951108     C*
255100951108     C* Variabili appoggio
255200951108     C*
255300951108     C                   Z-ADD     SFLPAG        WPAG
255400951108     C                   CLEAR                   W1POS
255500940223     C*
255600940223     C                   ENDSR
255700951109     C/EJECT
255800951109     C************************************************************
255900951109     C* Reperisco dati divisa
256000951109     C************************************************************
256100951109     C     RepDivisa     BEGSR
256200951109     C*
256300951109     C* Divisa
256400951109     C                   reset                   X01CambiDs
256500951109     C                   MOVE      xscsoc        X01societa
256600951109     C                   MOVEL     WDivisa       X01divisa
256700951109     C                   MOVEL     '1'           X01flgcam
256800951109     C                   MOVE      Wdate         x01dtRifCa
256900951109     C                   CALL      'X01CAMBI'
257000951109     C                   PARM                    X01CambiDs
257100951109     C*
257200951109     C                   ENDSR
257300951109     c/EJECT
257400951109     C************************************************************
257500951109     C* Editazione di un numero
257600951109     C************************************************************
257700951109     C     Edit          Begsr
257800951109      *
257900951109     C                   Select
258000951110     C                   When      WNrDec = 0
258100951110     C                   Z-add     WImporto      $SrcVar
258200951110     C                   When      WNrDec = 1
258300951110     C     WImporto      Mult      10            $SrcVar
258400951110     C                   When      WNrDec = 2
258500951110     C     WImporto      Mult      100           $SrcVar
258600951109     C                   Other
258700951110     C     WImporto      Mult      1000          $SrcVar
258800951109     C                   Endsl
258900951109     C*
259000951110     C                   Eval      $SrcVarDe = WNrDec
259100951109     C*
259200951109     C                   CallB     'X23EDITR'
259300951109     C                   Parm                    $SrcVar
259400951109     C                   Parm                    $SrcVarPr
259500951109     C                   Parm                    $SrcVarDe
259600951109     C                   Parm                    $EdtCode
259700951109     C                   Parm                    $LenFld
259800951109     C                   Parm      *blanks       $RcvrVar
259900951109     C                   Parm                    $Err
260000951109      *
260100951109     C                   ENDSR
260200951109     C*
260300941124     C/EJECT
260400941124     C************************************************************
260500941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
260600941124     C************************************************************
260700941124     C     REPAUT        BEGSR
260800941124     C*
260900941124     C                   CLEAR                   AUTDS
261000941124     C*
261100941124     C                   MOVEL     XSCSOC        XCASOC
261200941124     C                   MOVEL     XSCDUN        XCAUNI
261300941124     C                   Z-ADD     0             XCAGRP
261400941124     C                   MOVEL     KNMUS         XCAPRF
261500951114    >C                   MOVEL     'CONECC'      XCAFNC
261600941124    >C                   MOVEL     *ZERO         XCAVFU
261700941124    >C                   MOVE      'CK'          XCATCK
261800941124    >C                   MOVE      '1'           XCANAU
261900941124     C*
262000950522     C                   CALLB     'XCHKAUT'
262100941124     C                   PARM                    AUTDS
262200941124     C*
262300941124     C* errori/non abilitazione
262400941124     C     XCARTN        IFNE      0
262500941124     C                   EXSR      ENDPGM
262600941124     C                   ELSE
262700951108    >C                   MOVEL     XCADDS        CONECCDS
262800941124     C                   ENDIF
262900941124     C*
263000941124     C                   ENDSR
263100940223     C/EJECT
263200940131     C************************************************************
263300940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
263400940131     C************************************************************
263500940225     C     DEFOPZ        BEGSR
263600940131     C*
263700940225     C* Questa routine deve valorizzare la schiera $FL1
263800940225     C* con S o N a seconda della utilizzabilità o meno
263900940225     C* dell'opzione correlata
264000940225     C*
264100950301     C* Inizialmente si considerano tutte le opzioni come valide
264200940317     C     1             DO        $DIMOP        X
264300020321C1530C* Imposta tutti i flg, delle opzioni non a *blank, a 'S'
264400020321C1530C                   If        $TX1(X)  <> *Blank
264500940225     C                   MOVE      'S'           $FL1(X)
264600020321C1530C                   Else
264700020321  "  C                   Leave
264800020321C1530C                   EndIf
264900941027     C                   ENDDO
265000940715     C*
265100951109     C* Questa routine deve valorizzare la schiera $FC1/$FR1
265200941018     C* con 1 o 0 a seconda della utilizzabilità o meno
265300940715     C* del tasto funzione correlato
265400940715     C*
265500940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
265600940715     C* di compilazione
265700940715     C                   RESET                   $FC1
265800940715     C                   RESET                   $FR1
265900940225     C*
266000940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
266100940225     C* invalido alcune opzioni
266200940225     C                   SELECT
266300940225     C* Se pgm richiamato in ricerca disattivo :
266400951114    >C     OPZ087        WHENEQ    '01'
266500940603     C* F3=Fine
266600940603     C                   Z-ADD     3             X
266700941216     C                   MOVE      '0'           $FC1(X)
266800950116     C*
266900951114     C* Se pgm richiamato in manutenzione   disattivo :
267000951114    >C     OPZ087        WHENEQ    '02'
267100940225     C* 1=Selezione
267200940225     C                   Z-ADD     1             X
267300940225     C     01            LOOKUP    $OP1(X)                                21
267400940225     C     *IN21         IFEQ      *ON
267500940225     C                   MOVE      'N'           $FL1(X)
267600940225     C                   ENDIF
267700960327     c* 10=scelta con importo
267800960327     C                   Z-ADD     1             X
267900960327     C     10            LOOKUP    $OP1(X)                                21
268000960327     C     *IN21         IFEQ      *ON
268100960327     C                   MOVE      'N'           $FL1(X)
268200960327     C                   ENDIF
268300940225     C*
268400980617     C* Se pgm richiamato in interrogazione disattivo :
268500980617    >C     OPZ087        WHENEQ    '05'
268600980617     C* 1=Selezione
268700980617     C                   Z-ADD     1             X
268800980617     C     01            LOOKUP    $OP1(X)                                21
268900980617     C     *IN21         IFEQ      *ON
269000980617     C                   MOVE      'N'           $FL1(X)
269100980617     C                   ENDIF
269200990115     C* 10=Scelta con importo
269300990115     C                   Z-ADD     1             X
269400990115     C     10            LOOKUP    $OP1(X)                                21
269500990115     C     *IN21         IFEQ      *ON
269600990115     C                   MOVE      'N'           $FL1(X)
269700990115     C                   ENDIF
269800980617     C*
269900950301     C* In tutti gli altri casi, disattivo subito :
270000941228     C                   OTHER
270100941228     C*
270200940225     C                   ENDSL
270300960327     c*
270400960327     c* disattivo scelta con importo se non richiamato in scelta multipla
270500960327     c* 10=Scelta con importo
270600960327     C                   IF        multip087 <>*on
270700960327     C                   Z-ADD     1             X
270800960327     C     10            LOOKUP    $OP1(X)                                21
270900960327     C     *IN21         IFEQ      *ON
271000960327     C                   MOVE      'N'           $FL1(X)
271100960327     C                   ENDIF
271200960327     C                   ENDIF
271300951115     c*
271400951127     C* Se non cliente/fornitore disattivo  portafoglio
271500951115     C* 43=portafoglio
271600951127     C                   if        snatura087 <> 'C' and snatura087 <> 'F'
271700951115     C                   Z-ADD     1             X
271800951115     C     43            LOOKUP    $OP1(X)                                21
271900951115     C     *IN21         IFEQ      *ON
272000951115     C                   MOVE      'N'           $FL1(X)
272100951115     C                   ENDIF
272200951115     C                   endif
272300051109     c*
272400051109C1881C* Se non conto banca, disattivo opzione 23=tesoreria
272500051109  "  C                   if        snatura087 <> '2'
272600051109  "  C                   Z-ADD     1             X
272700051109  "  C     23            LOOKUP    $OP1(X)                                21
272800051109  "  C     *IN21         IFEQ      *ON
272900051109  "  C                   MOVE      'N'           $FL1(X)
273000051109  "  C                   ENDIF
273100051109C1881C                   endif
273200951115     c*
273300951115     C* Se conto non possiede analitica disattivo opzione
273400951115     c* e anche se da autorizzazioni ho e/c limitato per partite
273500951116     C* 21=analitica
273600951124     C                   if        analit087  =  '0' or
273700951124     C                             (Eccpar = '1' )
273800951115     C                   Z-ADD     1             X
273900951116     C     21            LOOKUP    $OP1(X)                                21
274000951115     C     *IN21         IFEQ      *ON
274100951115     C                   MOVE      'N'           $FL1(X)
274200951115     C                   ENDIF
274300951115     C                   endif
274400951115     C*
274500011213C1530C*
274600011213  "  C* Se Laguna non è presente o non è attivo per la società
274700011213  "  C                   IF        XscLaguna <> '1'
274800020117  "  C* oppure la sottonatura del conto non è né cliente né fornitore
274900020117  "  C                             or (SNatura087 <> 'C' and
275000020117  "  C                                 SNatura087 <> 'F')
275100020118  "  C* disattivo l'opzione 80
275200011213  "  C                   Z-ADD     1             X
275300020118  "  C     80            LOOKUP    $OP1(X)                                21
275400011213  "  C     *IN21         IFEQ      *ON
275500011213  "  C                   MOVE      'N'           $FL1(X)
275600011213  "  C                   ENDIF
275700011213C1530C                   ENDIF
275800940225     C*
275900940225     C                   ENDSR
276000940225     C/EJECT
276100940225     C************************************************************
276200940225     C* REPERIMENTO OPZIONI
276300940225     C************************************************************
276400940225     C     REPOPZ        BEGSR
276500940225     C*
276600940314     C     1             DO        $DIMOP        X
276700940128     C*
276800940223     C     $MS1(X)       IFNE      *BLANK
276900940128     C*
277000940223     C                   MOVEL     $MS1(X)       IDMSG
277100950522     C                   CALLB     'XRTVM'
277200940201     C                   PARM                    IDMSG            27
277300940201     C                   PARM                    TXTMSG          100
277400940201     C                   PARM                    ERRMSG            1
277500940128     C*
277600940128     C     ERRMSG        IFNE      *ON
277700940223     C                   MOVEL     TXTMSG        $TX1(X)
277800940128     C                   ELSE
277900940223     C                   MOVEL     *ALL'?'       $TX1(X)
278000941027     C                   ENDIF
278100940128     C*
278200941027     C                   ENDIF
278300940128     C*
278400941027     C                   ENDDO
278500940128     C*
278600940128     C                   ENDSR
278700940506     C/EJECT
278800940506     C************************************************************
278900940506     C* REPERIMENTO TASTI DI FUNZIONE
279000940506     C************************************************************
279100940506     C     REPKEY        BEGSR
279200940506     C                   MOVEA     $FM1          $FM
279300950906     C                   MOVEA     $Fc1          $Fc
279400940506     C                   EXSR      REPKKK
279500940506     C                   MOVEA     $FX           $FX1
279600940506     C*
279700951109     C                   MOVEA     $FM1          $FM
279800951109     C                   MOVEA     $Fc1          $Fc
279900940506     C                   EXSR      REPKKK
280000951109     C                   MOVEA     $FX           $FX1
280100940919     C*
280200940919     C                   ENDSR
280300940506     C/EJECT
280400940506     C************************************************************
280500940506     C* REPERIMENTO TASTI DI FUNZIONE
280600940506     C************************************************************
280700940506     C     REPKKK        BEGSR
280800940506     C*
280900940506     C                   CLEAR                   $FX
281000940506     C     1             DO        24            X
281100940506     C*
281200940506     C     $FM(X)        IFNE      *BLANK
281300950906     C     $Fc(X)        andne     '0'
281400940506     C*
281500940506     C                   MOVEL     $FM(X)        IDMSG
281600950522     C                   CALLB     'XRTVM'
281700940506     C                   PARM                    IDMSG            27
281800940506     C                   PARM                    TXTMSG          100
281900940506     C                   PARM                    ERRMSG            1
282000940506     C*
282100940506     C     ERRMSG        IFNE      *ON
282200940506     C                   MOVEL     TXTMSG        $FX(X)
282300940506     C                   ELSE
282400940506     C                   MOVEL     *ALL' '       $FX(X)
282500941027     C                   ENDIF
282600940506     C*
282700941027     C                   ENDIF
282800940506     C*
282900941027     C                   ENDDO
283000940506     C*
283100940506     C                   ENDSR
283200940223     C/EJECT
283300940128     C************************************************************
283400950606     C* DEFINIZIONE KLIST
283500940128     C************************************************************
283600950606     C     DefKlist      BEGSR
283700940128     C*
283800940223     C* klist
283900940223     C*
284000951114    >C* ndpas02j
284100951114    >C     K07PAS02      KLIST
284200951114    >C                   KFLD                    PASSOCIETA
284300951114    >C                   KFLD                    PASctb
284400951114    >C                   KFLD                    PASKCC
284500951114    >C                   KFLD                    PASKSC
284600951114    >C                   KFLD                    PASDTPAR
284700951114    >C                   KFLD                    PASNRPAR
284800951114    >C                   KFLD                    PASSERPAR
284900951115     C*
285000951115     C* NDDES01L
285100951115     C     K04DES01      KLIST
285200951115     C                   KFLD                    DesSys
285300951115     C                   KFLD                    DesNrAsReg
285400951115     C                   KFLD                    DesTipDes
285500951115     C                   KFLD                    DesNrRigaM
285600951116     C     K03DES01      KLIST
285700951116     C                   KFLD                    DesSys
285800951116     C                   KFLD                    DesNrAsReg
285900951116     C                   KFLD                    DesTipDes
286000061123c1979C* NdPsc01L
286100061123  "  C     K04Psc01      KLIST
286200061123  "  C                   KFLD                    PscSys
286300061123  "  C                   KFLD                    PscNrasInt
286400061123  "  C                   KFLD                    PscDtSca
286500061123c1979C                   KFLD                    PscNrRata
286600950704     C*
286700940117     C                   ENDSR
286800940223     C/EJECT
286900960314     C************************************************************
287000960314     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
287100960314     c* serve per vedere quale sfl devo visualizzare
287200960314     C************************************************************
287300960314     C     REPDFT        BEGSR
287400960314     C*
287500960314     C* preparo i parametri di chiamata al driver
287600960314     C                   CLEAR                   XMPEDS
287700960314     C                   EVAL      TpChiamata = '01'
287800960314     C                   EVAL      NChiavi = 5
287900960314     C                   EVAL      VLogica = 2
288000960314     C*
288100960314     C* preparo le chiavi per la chiamata al driver
288200960314     C                   CLEAR                   XMPEDS1
288300960314     C                   MOVEL     XSCSOC        DSMPESOC
288400960314     C                   MOVEL     'NDC085R'     DSMPEPGM
288500960314     C                   MOVEL     KNMUS         DSMPEPRF
288600960314     C* Si reperiscono i dati di default a seconda delle modalità con
288700960314     C* cui il pgm è usato
288800960314     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
288900960314     C* MPETIP = 'PV'  pgm richiamato da menù
289000960314     C* MPETIP = 'PB'  x lancio Batch
289100960314     C                   SELECT
289200960314    >C     OPZ087        WHENEQ    '01'
289300960314     C                   MOVEL     'PX'          DSMPETIP
289400960314     C                   OTHER
289500960314     C                   MOVEL     'PV'          DSMPETIP
289600960314     C                   ENDSL
289700960314     C*
289800960314     C                   CALLB     'XMPE'
289900960314     C                   PARM                    XMPEDS
290000960314     C                   PARM                    XMPEDS1
290100960314     C*
290200960314     C     Risultato     IFEQ      '0'
290300960314     C                   MOVEL     DSMPEPAR      DSMPE
290400960314     C                   ENDIF
290500960314     c*
290600960314     c* se $visdati = *blanks, lo imposto a 1 (il campo è stato
290700960314     C* aggiunto dopo
290800960314     c                   if        $visdati = ' '
290900960314     C                   eval      $visdati = '1'
291000960314     c                   endif
291100960314     c*
291200960314     c                   z-ADD     0             SFLPAG            3 0
291300960314     C                   SELECT
291400960314     C                   when      $visdati = '1'
291500960314     c                   eval      sflpag = 8
291600960314     C* visualizzo il sfl troncato
291700960314     C                   Eval      c1mode=*on
291800960314     C*
291900960314     C                   when      $visdati = '2'
292000960513     c                   eval      sflpag = 8
292100960314     C* visualizzo il sfl aperto
292200960314     C                   Eval      c1mode=*off
292300960314     C                   ENDSL
292400960314     C*
292500960314     C                   ENDSR
292600051109     C*C1881: aggiunta opzione 23=tesoreria
292700941214**
292800940718PROMSG    *LIBL     OPZ0001
292900960327PROMSG    *LIBL     OPZ0168
293000951114PROMSG    *LIBL     OPZ0107
293100960318PROMSG    *LIBL     OPZ0108
293200051109PROMSG    *LIBL     OPZ0109                                   C1881
293300951116PROMSG    *LIBL     OPZ0041
293400951116PROMSG    *LIBL     OPZ0143
293500951116PROMSG    *LIBL     OPZ0144
293600951116PROMSG    *LIBL     OPZ0145
293700011213PROMSG    *LIBL     OPZ0451
293800940201**
29390094022501S      <Selezione
29400096032710S      <Scelta con importo
29410095111421S      <analitica
29420096031822S      <partite
29430005110923S      <tesoreria                                               C1881
29440095111441S      <Contabilità
29450095111443S      <Portafoglio
29460095111644S      <descrizioni aggiuntive T
29470095111645S      <descrizioni aggiuntive M
29480002011880S      <Laguna
294900940916** TASTI DI FUNZIONE  S1
295000940916PROMSG    *LIBL     KEY0001  01
295100940916PROMSG    *LIBL     KEY0002  02
295200940916PROMSG    *LIBL     KEY0003  03
295300940916PROMSG    *LIBL     KEY0004  04
295400940916PROMSG    *LIBL     KEY0005  05
295500940916PROMSG    *LIBL     KEY0006  06
295600940916PROMSG    *LIBL     KEY0007  07
295700940916PROMSG    *LIBL     KEY0008  08
295800940916PROMSG    *LIBL     KEY0009  09
295900940916PROMSG    *LIBL     KEY0010  10
296000951109PROMSG    *LIBL     KEY0031  11
296100940916PROMSG    *LIBL     KEY0012  12
296200940916PROMSG    *LIBL     KEY0013  13
296300940916PROMSG    *LIBL     KEY0014  14
296400951109PROMSG    *LIBL     KEY0053  15
296500940916PROMSG    *LIBL     KEY0016  16
296600940916PROMSG    *LIBL     KEY0017  17
296700940916PROMSG    *LIBL     KEY0018  18
296800940916PROMSG    *LIBL     KEY0019  19
296900951115PROMSG    *LIBL     KEY0020  20
297000940916PROMSG    *LIBL     KEY0021  21
297100940916PROMSG    *LIBL     KEY0022  22
297200940916PROMSG    *LIBL     KEY0023  23
297300940916PROMSG    *LIBL     KEY0024  24
297400940916**  ABILITAZIONE S1
2975009409160N  01
2976009810091N  02
2977009409161N  03
2978009409160N  04
2979009409160N  05
2980009409160N  06
2981009409160N  07
2982009511151N  08
2983009511081N  09
2984009507040N  10
2985009511151N  11
2986009409161N  12
2987009409160N  13
2988009409160N  14
2989009511151N  15
2990009409160N  16
2991009511080N  17
2992009511090N  18
2993009409160N  19
2994009511150N  20
2995009409160N  21
2996009409160N  22
2997009509131N  23   Impostare a '1' se si utilizzano opzioni
2998009509131N  24   Lasciare sempre impostata a '1' il valore della riga 24
299900051216** $Cost
300000051216PROMSG    *LIBL     COS2601                                  Dt.flusso
300100051216PROMSG    *LIBL     COS2602                                  Dt.scaden.
