000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300951108     H*PARMS CVTOPT(*DATETIME)
000400940211     H DECEDIT('0,') DATEDIT(*DMY.)
000500941115      *
000600960126    > *            Guida riconciliazione partite
000700960126      *           ------------------------------
000800990810      *----------------------------------------------------------------
000900041130c1807F* 16 sottolinea la data partita se ci sono commenti
001000990810      *----------------------------------------------------------------
001100940224      *
001200951115    >F*----------------------------------------------------------------
001300940314    >F* Viste logiche utilizzate per i diversi ordinamenti
001400940314    >F*----------------------------------------------------------------
001500940909     F*------------
001600951113    >FNDPPA01L  IF   E           K DISK
001700950703    >F                                     RENAME(NDPPA000:NDPPA01)
001800951113    >FNDPPA03L  IF   E           K DISK
001900951113    >F                                     RENAME(NDPPA000:NDPPA03)
002000960129    >FNDPPS01L  IF   E           K DISK
002100041130C1807FNDPSC01L  IF   E           K DISK
002200960126    >FANUNI01L  IF   E           K DISK
002300960126    >F                                     RENAME(ANUNI000:ANUNI01)
002400960201    >FNDEPA01L  IF   E           K DISK
002500960201    >F                                     RENAME(NDEPA000:NDEPA01)
002600030122D1271FNDMRA04L  IF   E           K DISK
002700940211     F*------------
002800960126    >FNDC110D   CF   E             WORKSTN
002900960126     F                                     RENAME(C110T1:FMTT1)
003000960126     F                                     RENAME(C110D1:FMTD1)
003100960126     F                                     RENAME(C110D2:FMTD2)
003200960126     F                                     RENAME(C110D3:FMTD3)
003300960126     F                                     RENAME(C110S1:FMTS1)
003400960126     F                                     RENAME(C110C1:FMTC1)
003500960126     F                                     RENAME(C110Z1:FMTZ1)
003600960126     F                                     RENAME(C110Z2:FMTZ2)
003700960126     F                                     RENAME(C110Z3:FMTZ3)
003800960126     F                                     SFILE(FMTS1:S1NRR)
003900960126     F                                     INFDS(DSFMT)
004000940128     D*----------------------------------------------------*
004100940302     D* Schiere per la gestione delle opzioni
004200940302     D*
004300950614     D* dimensione della schiera $MS1
004400960129     D $DIMOP          C                   CONST(6)
004500950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
004600950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
004700950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004800950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
004900941214     D $TX             S             50    DIM(50)
005000941214     D $FL             S              1    DIM(50)
005100940909     D* Schiere generali per la gestione dei tasti di funzione
005200940506     D $FM             S             27    DIM(24)
005300940506     D $FX             S             50    DIM(24)
005400940506     D $FC             S              1    DIM(24)
005500940506     D $FR             S              1    DIM(24)
005600940909     D* Schiere per la gestione dei tasti di funzione
005700960126     D* Video D1
005800960126     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
005900960126     D $FX3            S             50    DIM(24) ALT($FM3)
006000960126     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
006100960126     D $FR3            S              1    DIM(24) ALT($FC3)
006200960126     D* Schiere per la gestione dei tasti di funzione
006300960126     D* Video S1
006400960126     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006500960126     D $FX1            S             50    DIM(24) ALT($FM1)
006600960126     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006700960126     D $FR1            S              1    DIM(24) ALT($FC1)
006800960126     D* Schiere per la gestione dei tasti di funzione
006900960126     D* Video D2
007000960126     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
007100960126     D $FX2            S             50    DIM(24) ALT($FM2)
007200960126     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
007300960126     D $FR2            S              1    DIM(24) ALT($FC2)
007400960126     D*
007500960126     D $D1L01          S              3  0
007600960126     D $D1L02          S              3  0
007700960126     D $S1L01          S              3  0
007800960126     D $S1L02          S              3  0
007900960126     D $C1Op1          S              3  0
008000960126     D $C1Op2          S              3  0
008100960126     D $D2L01          S              3  0
008200960126     D $D2L02          S              3  0
008300971008     D*-------------
008400971008     D* Testi bottoni risposta di XMSGR
008500971008     D TxtBott         C                   CONST('PROMSG    *LIBL-
008600971010     D                                          COS1204')
008700960126     D*-------------
008800960126     D* Valori assunti
008900960126     D XMPEDS        E DS                  INZ
009000960126     D XMPEDS1       E DS                  INZ
009100960126     D DSMPE           DS                  INZ
009200960126     D  $LOP                   1      1  0
009300960126     D  $ORD                   2      3  0
009400051220C1895D  $NRP                   4     12  0
009500950524     D*-------------
009600940211     D* Passaggio Parametri
009700960130     D*-------------
009800940211     D KPJBA         E DS
009900960130     D* DS Esterna gestione messaggi
010000960130     D XMSGDS        E DS
010100960130     D  STS          E                     EXTFLD(MSGSTS)
010200960130     D                                     DIM(333)
010300960130     D  JBA          E                     EXTFLD(MSGJBA)
010400960130     D                                     DIM(502)
010500940211     D*-------------
010600940225     D* Reperimento Autorizzazioni
010700941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
010800941124     D*-------------
010900950410     D* Passaggio Autorizzazioni
011000950410     D DSAUT           DS           512
011100950524     D*-------------
011200950524     D* Maschera dati autorizzazioni
011300960126    >D CONPNODS      E DS
011400960126     D*-------------
011500960126     D* Contollo autorizzazione contabilità
011600960126     D XCTBDS        E DS                  EXTNAME(XAUTCTBDS)
011700960126     D*-------------
011800960126     D* Autorizzazioni alla unità
011900960126     D XAutUniDS     E DS
012000990806C1212 *
012100990806  "  D XAutUniDS1    E DS
012200990806  "   *-------------
012300990811  "   *Variabili utili alla messaggistica di XmsgR
012400990806  "  D $IdMsg                              like(IdMsg)
012500990811  "   *------
012600990806  "  D $TpReg          S              1
012700990811  "   *------
012800990806  "   * DS per emettere nella window di XMsgr l'unità +
012900990806  "   * i riferimenti partita  (e la data partita con '/')
013000990806  "  D AutUniWDS       DS                  inz
013100990806  "  D   Unita                        8a
013200990806  "  D   DtPar1                       2a
013300990806  "  D                                1a   inz('/')
013400990806  "  D   DtPar2                       2a
013500990806  "  D                                1a   inz('/')
013600990806  "  D   DtPar3                       2a
013700990806  "  D   NrPar                        9p 0
013800990806  "  D   SerPar                       4a
013900990806  "   *-------------
014000990806  "  D DataDS          ds
014100990806  "  D  Data01                        2s 0
014200990806  "  D  Data02                        2s 0
014300990806  "  D  Data03                        2s 0
014400990811  "   *----------------------
014500990811  "   *se = '1' -> le par. da riconciliare hanno unità uguale
014600990811  "  D $UnaUni         S              1
014700990811  "   *----------------------
014800990811  "   *utile per capire se esistono unità diverse tra le par. da riconc
014900990811  "  D $Unita          S                   Like(S1Unita)
015000990806C1212 *
015100941209     D*-------------
015200941209     D* Dati di ambiente ottenuti da XSOC
015300941209    >D SOC001        E DS                  EXTNAME(XSOC001DS)
015400950906     D    FgO1                        1    overlay(XscFgO)
015500961009     D    XscDare                     1    overlay(xscFgo:7)                    Scadenz. retroattivo
015600961009     D    XscAvere                    1    overlay(xscFgo:8)                    Scadenz. retroattivo
015700960126     D*-------------
015800960126     D* DS Interna per dati di output di XSOC
015900960126     D XSOCDS          DS          1000
016000960126     D*-------------
016100960126     D* Ricerca legende
016200960126     D A964DS        E DS                  EXTNAME(ANA964DS) INZ
016300951113     D*-------------
016400951113     D* Controlli validità legende
016500951113     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
016600951109     D*-------------
016700951109     D* Parametri per PGM X01CAMBI
016800951109     D X01CambiDs    E DS                  inz
016900951109     D*-------------
017000951109     D* Parametri per reperimento saldo
017100951109     D X20SCADDS     E DS                  INZ
017200951109     D* DS parametro esteso se richiesto x20parscd='S'
017300951206     D X20SCADXDS    E DS                  inz
017400951206     D*-------------
017500951109     D* Routine editazione di un numero
017600951110     D $SrcVar         s             30S 0
017700951110     D $SrcVarPr       s              9B 0 inz(30)
017800951109     D $SrcVarDe       s              9B 0
017900951117     D $EdtCode        s              1    inz('1')
018000951109     D $LenFld         s              3S 0
018100951109     D $RcvrVar        s             50
018200951109     D $Err            s              1
018300940325     D*-------------
018400940325     D* Parametri in ricezione
018500960126    >D NDC110DS      E DS
018600940325     D*-------------
018700960126     D* Parametri del PGM di dettaglio partita NDC111R
018800960126    >D NDC111DS      E DS                  inz
018900960126    >D  RET111       E                     INZ('0')
019000960126    >D  OPR111       E                     INZ('0')
019100960126    >D  ERR111       E                     INZ('0')
019200960201     D*-------------
019300960201     D* Parametri del PGM di riconciliazione partite
019400960201    >D NDC113DS      E DS                  inz
019500960201    >D  OPR113       E                     INZ('0')
019600960201    >D  ERR113       E                     INZ('0')
019700960126     D*-------------
019800960126     D* Ricerca unità
019900960126    >D Ana009ds      E DS                  inz
020000960126     D*-------------
020100960126     D* Ricerca capoconti
020200960126     D Ana200ds      E DS                  inz
020300960126     D*-------------
020400960126     D* Ricerca sottoconti
020500960126     D Ana201ds      E DS                  inz
020600960126     D*-------------
020700960126     D* Interrogazione conti
020800960126     D ANA399DS      E DS                  inz
020900960126     D*-------------
021000960126     D* Parametri del PGM di gestione commenti ANA725R
021100960126     D ANA725DS      E DS                  INZ
021200960126     D  RET725       E                     INZ('0')
021300960126     D  OPR725       E                     INZ('0')
021400960126     D  ERR725       E                     INZ('0')
021500951108     D*-------------
021600951108     D* Parametri del PGM di manutenzione NDC060R particolarità partite
021700951108    >D NDC060DS      E DS                  inz
021800951108    >D  RET060       E                     INZ('0')
021900951108    >D  OPR060       E                     INZ('0')
022000951108    >D  ERR060       E                     INZ('0')
022100960313     D  dtchiu060    E                     INZ('0001-01-01')
022200950707     D*-------------
022300950707     D* Parametri del PGM di manutenzione NDC065R mix
022400950707    >D NDC065DS      E DS                  inz
022500950707    >D  RET065       E                     INZ('0')
022600950707    >D  OPR065       E                     INZ('0')
022700950707    >D  ERR065       E                     INZ('0')
022800060104     D*-------------
022900060104C1895D* lista movimenti NDPAS (tasto F7)
023000060104c1895D NDC619DS      E DS                   inz
023100960122     D*-------------
023200960122     D* Controllo conti
023300960122     D ND002DS       E DS                  inz
023400960122     D*-------------
023500960122     D* Definizioni parametri standard chiamate moduli
023600960122     D Esito           S              1
023700960122     D gesmsg          S              1
023800960122     D lenout          S              9B 0
023900960122     D Data            S             10
024000960126     D struttura       S             10
024100960122     D Operazione      S              2
024200960122     D LenIn           S              9B 0
024300960122     D refresh         S              1
024400960122     D uso             S              1
024500960122     DOutput           S           4000
024600960126     D*-------------
024700960126     D* Parametri per controllo      ANATB00F
024800960126     D XATBDS        E DS
024900960126     D*-------------
025000960126     D* Parametri per ricerca codice ANATB00F
025100960126     D XTABDS        E DS
025200960126     D*-------------
025300960126     D* Tabella contabilità
025400960126     D TABG84        E DS                  EXTNAME(ANGG84DS)
025500041130     D*-------------
025600041130c1807D* Parametri del PGM di test esistenza commenti ANA724R
025700041130  "  D A724DS        E DS                  EXTNAME(ANA724DS) INZ
025800041130  "  D  EXI724       E                     INZ('0')
025900041130c1807D  RIS724       E                     INZ('0')
026000940211     D*-------------
026100940211     D DSFMT           DS           512
026200940506     D  $TASTO               369    369
026300940211     D  NRG                  370    370
026400940211     D  NCL                  371    371
026500940211     D  SFLNRR               378    379B 0
026600940211     D*-------------
026700940211     D* posizione cursore
026800940211     D CURSOR          DS
026900940211     D  RIRI                   1      2B 0 INZ
027000940211     D  R$$                    2      2
027100940211     D  COCO                   3      4B 0 INZ
027200940211     D  C$$                    4      4
027300940127     D*-------------
027400940127     D* Reperimento nome PGM
027500940127     D STATUS         SDS           333
027600940127     D  DSPGM            *PROC
027700950411     D  PARMS            *PARMS
027800950704     D*-------------
027900950704     D* Attributi video
028000950704     D* (definisco come costanti qugli attributi video che potrei
028100950704     D*  testare e quindi preferisco assegnargli un valore "parlante")
028200950704     D*  bottone presente
028300950704     D PRESENTE        C                   CONST(X'A1')
028400940616     D*-------------
028500940506     D* Tasti di funzione
028600940506     D F01             C                   CONST(X'31')
028700940506     D F02             C                   CONST(X'32')
028800940506     D F03             C                   CONST(X'33')
028900940506     D F04             C                   CONST(X'34')
029000940506     D F05             C                   CONST(X'35')
029100940506     D F06             C                   CONST(X'36')
029200940506     D F07             C                   CONST(X'37')
029300940506     D F08             C                   CONST(X'38')
029400940506     D F09             C                   CONST(X'39')
029500940506     D F10             C                   CONST(X'3A')
029600940506     D F11             C                   CONST(X'3B')
029700940506     D F12             C                   CONST(X'3C')
029800940506     D F13             C                   CONST(X'B1')
029900940506     D F14             C                   CONST(X'B2')
030000940506     D F15             C                   CONST(X'B3')
030100940506     D F16             C                   CONST(X'B4')
030200940506     D F17             C                   CONST(X'B5')
030300940506     D F18             C                   CONST(X'B6')
030400940506     D F19             C                   CONST(X'B7')
030500940506     D F20             C                   CONST(X'B8')
030600940506     D F21             C                   CONST(X'B9')
030700940506     D F22             C                   CONST(X'BA')
030800940506     D F23             C                   CONST(X'BB')
030900940506     D F24             C                   CONST(X'BC')
031000940506     D ENTER           C                   CONST(X'F1')
031100940506     D ROLDWN          C                   CONST(X'F4')
031200940506     D ROLLUP          C                   CONST(X'F5')
031300951109     D*-------------
031400951109     D DATMIN          C                   CONST('0001-01-01')
031500060104C1895X*DATMAX          C                   CONST('9999-12-31')
031600060104C1895D DATMAX          C                   CONST('2039-12-31')
031700950524     D*-------------
031800950606     D* Variabili per gestione videate
031900950606     D $Gest           S              2A
032000950606     D $Fine           S              1A
032100950606     D $InzD1          S              1A
032200960126     D $InzD2          S              1A
032300950606     D $InzS1          S              1A
032400950606     D $LastG          S              2A
032500950606     D $LastV          S              2A
032600950606     D $EndFile        S              1A
032700950606     D $Esci           S              1A
032800950606     D $RcdOK          S              1A
032900960126     D $Unique         S              1A
033000951108     D $carpag         S              1A
033100951109     D sflpag          S              4S 0
033200950606     D*-------------
033300950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
033400950606     D S1Nrr           S                   like(C1Rcd)
033500951109     D SFLNRRSAV       S                   like(SFLNRR)
033600950606     D WSfl            S                   like(C1Nrr)
033700950606     D WMax            S                   like(C1Rcd)
033800950606     D WPag            S                   like(C1Rcd)
033900950606     D WInzS1          S                   like($InzS1)
034000960126    >D WOpr            S                   like(Opr111)
034100950606     D CurR            S              2S 0
034200950606     D CurC            S              2S 0
034300950606     D Win             S             99A
034400950606     D W1Pos           S                   like(C1Pos)
034500960126     D WINKR           S              1A
034600960126     D DtparAR         S             10
034700960126     D DtparDaR        S             10
034800951109     D Wdate           S             10
034900960126     D UdateR          S             10
035000960126    >D Wcalcsaldo      S              1a
035100960126    >D Wwctb           S                   like(d1ctb)
035200960126    >D WwSITPAR        S                   like(d1sitpar)
035300960126    >D WDivisa         S                   like(X01Divisa)
035400951109    >D SavDivisa       S                   like(X01Divisa)
035500960126    >D SavDivVis       S              1A
035600960126    >D WImporto        S             19S 3
035700951110    >D NrDec           S              3S 0
035800951110    >D NrDecd          S              3S 0
035900951110    >D WNrDec          S              3S 0
036000951110     D Wsegno          S              1A
036100960126    >D WSaldo          S             19S 3
036200960126    >D WSaldoTotD      S             19S 3
036300960126    >D WSaldoTotA      S             19S 3
036400960126    >D WTotDare        S             19S 3
036500960126    >D WTotAvere       S             19S 3
036600960126    >D WTotDareD       S             19S 3
036700960126    >D WTotAvereD      S             19S 3
036800960129    >D Wtaglia         S              3S 0
036900960129    >D Wincolla        S              3S 0
037000030122D1271D WRitenute       S                   like(H1Ritenute)
037100960130    >D Wunita          S                   like(s1unita)
037200960130    >D Wkscage         S                   like(h1kscage)
037300960130    >D Wdivisaok       S              1A
037400030122D1271D WRitenutOk      S              1A
037500960130     D Wxmsginz        S              1A
037600960201    >D Wdtpar          S                   like(s1dtpar)
037700960201    >D Wnrpar          S                   like(s1nrpar)
037800960201    >D Wserpar         S                   like(s1serpar)
037900041130C1807D WrkBkAmm        S                   like(pscbkamm)
038000950606     D*-------------
038100950606     D*  Indici
038200950606     D X               S              3S 0
038300950606     D Y               S              3S 0
038400960201     D* ds per estremi partita da emettere nella segnalazione
038500960131     D DsPartita       DS            30
038600960131     D  DSDTPAR                1      6
038700960131     D  DSNRPAR               10     19
038800960131     D  DSSERPAR              22     25
038900950606     D*-------------
039000000118C1309D ZDateISO        S               D
039100940127     C*----------------------------------------------------*
039200940127     C*                MAIN LINE PROGRAM
039300940127     C*----------------------------------------------------*
039400940127     C*
039500940223     C* inizializzazione variabili
039600940223     C                   EXSR      INZVAR
039700940223     C*
039800940223     C     $FINE         DOWEQ     *OFF
039900960126     C     $GEST         CASEQ     'D1'          GESD1
040000960126     C     $GEST         CASEQ     'D2'          GESD2
040100940131     C     $GEST         CASEQ     'S1'          GESS1
040200941027     C                   ENDCS
040300941027     C                   ENDDO
040400940325     C* fine programma
040500940325     C                   EXSR      ENDPGM
040600940225     C*
040700940325     C/EJECT
040800940325     C************************************************************
040900940325     C* FINE PROGRAMMA
041000940325     C************************************************************
041100940325     C     ENDPGM        BEGSR
041200951122     c* Per far riemettere la testata
041300951122     C                   MOVE      *HIVAL        $LASTV
041400960318     c                   move      wopr          opr110
041500960318     c                   movel     ndc110ds      kpjbu
041600940325     C*
041700940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
041800960126    >C     OPZ110        IFEQ      *BLANK
041900940325     C                   SETON                                            LR
042000940325     C* altrimenti chiudo in RT
042100940325     C                   ELSE
042200940325     C                   SETON                                            RT
042300940325     C                   ENDIF
042400940616     C                   RETURN
042500940325     C*
042600940325     C                   ENDSR
042700940224     C/EJECT
042800940201     C************************************************************
042900940201     C* RICERCHE
043000940201     C************************************************************
043100940127     C     SEARCH        BEGSR
043200940127     C*
043300940127     C* determino Riga/Colonna del cursore
043400940127     C                   MOVE      NRG           R$$
043500940127     C                   MOVE      NCL           C$$
043600940127     C                   Z-ADD     RIRI          CURR
043700940127     C                   Z-ADD     COCO          CURC
043800960126     C*
043900960126     C     D1KCC         IFNE      *BLANKS
044000960126     C     D1KCC         ANDNE     'C     '
044100960126     C     D1KCC         ANDNE     'F     '
044200960926     C                   CALLB     'XALLINEA'
044300960126     C                   PARM                    D1KCC
044400960126     C                   PARM      6             LEN               5 0
044500960126     C                   PARM                    XSCINP
044600960126     C                   ENDIF
044700960126     C*
044800960126     C     D1KSC         IFNE      *BLANKS
044900960926     C                   CALLB     'XALLINEA'
045000960126     C                   PARM                    D1KSC
045100960126     C                   PARM      8             LEN               5 0
045200960126     C                   PARM                    XSCINP
045300960126     C                   ENDIF
045400960126     C*
045500960126   B1C                   SELECT
045600960126    >C* C085D1- UNITA
045700960126    >C     H1NMFL        WHENEQ    'D1UNITA'
045800960126    >C                   RESET                   Ana009ds
045900960126    >C                   MOVEL     '01'          OPZ009
046000960126    >C                   MOVEL     XSCSOC        SOC009
046100960126    >C                   MOVEL     Ana009ds      KPJBU
046200960126    >C                   CALL      'ANA009R'
046300960126     C                   PARM                    KPJBA
046400960126    >C                   MOVEL     KPJBU         Ana009ds
046500960126     C*
0466009601262   >C     UNI009        IFNE      *BLANK
046700960126    >C                   MOVE      UNI009        D1UNITA
0468009601262-  >C                   ENDIF
046900960126     C*
047000960126    >C*  C085D1 - Contabilità
047100960126     C     H1NMFL        WHENEQ    'D1CTB'
047200960126     C                   RESET                   XTABDS
047300960126     C                   MOVE      *BLANK        WWCTB
047400960126     C                   MOVE      WWCTB         XTAKEY
047500960126     C                   MOVE      '01'          XTAOPZ
047600960126     C                   MOVE      XSCSOC        XTAAZI
047700960126     C                   MOVE      XSCLIN        XTALIN
047800960126     C                   Z-ADD     CURR          XTARIG
047900960126     C                   Z-ADD     CURC          XTACOL
048000960126     C                   CLEAR                   KPJBU
048100960126     C                   MOVEL     XTABDS        KPJBU
048200960126     C                   CALL      'ANGG84R'
048300960126     C                   PARM                    KPJBA
048400960126     C                   MOVEL     KPJBU         XTABDS
048500960126     C     XTAERR        IFNE      '1'
048600960126     C                   MOVE      XTAKEY        WWCTB
048700960126     C                   ELSE
048800960126     C                   MOVE      *BLANK        WWCTB
048900960126     C                   ENDIF
049000960126     C*
049100960126     C     WWCTB         IFNE      *BLANK
049200960126     C                   MOVEL     WWCTB         D1CTB
049300960126     C                   ENDIF
049400960126    >C* C085D1- KCC
049500960126    >C     H1NMFL        WHENEQ    'D1KCC'
049600960126     C                   RESET                   Ana200ds
049700960126     C                   MOVEL     '01'          OPZ200
049800960126     C                   MOVE      D1KCC         KCC200
049900960126     C                   MOVEL     Ana200ds      KPJBU
050000960126     C                   CALL      'ANA200R'
050100960126     C                   PARM                    KPJBA
050200960126     C                   MOVEL     KPJBU         Ana200ds
050300960126     C*
050400960126     C     KCC200        IFNE      *BLANK
050500960126     C                   MOVEL     KCC200        D1KCC
050600960126     C                   ENDIF
050700960126     C*
050800960126     C* C085D1 - SOTTOCONTO
050900960126     C     H1NMFL        WHENEQ    'D1KSC'
051000960126     C                   RESET                   Ana201ds
051100960126     C                   MOVE      XSCSOC        SOC201
051200960126     C                   MOVE      D1KCC         KCC201
051300960126     C                   MOVE      D1KsC         KSC201
051400960126     C                   MOVEL     Ana201ds      KPJBU
051500960126     C                   CALL      'ANA201R'
051600960126     C                   PARM                    KPJBA
051700960126     C                   MOVEL     KPJBU         Ana201ds
051800960126     C*
051900960126     C     KSC201        IFNE      *BLANK
052000960126     C                   MOVEL     KCC201        D1KCC
052100960126     C                   MOVEL     KSC201        D1KSC
052200960126     C                   ENDIF
052300960126     C*
052400960126     C*  Situazione Partite
052500960126     C     H1NMFL        WHENEQ    'D1SITPAR'
052600960126     C                   RESET                   A964DS
052700960126     C                   MOVEL     '01'          OPZ964
052800960126     C                   MOVEL     'ANDIZ000'    TRC964
052900960126     C                   MOVEL     'DSPSITPAR'   CAM964
053000960126     C                   MOVEL     A964DS        KPJBU
053100960126     C                   CALL      'ANA964R'
053200960126     C                   PARM                    KPJBA
053300960126     C                   MOVEL     KPJBU         A964DS
053400960126     C                   MOVEL     VAL964        WWsitPar
053500960126     C*
053600960126     C     WWSitPAR      IFNE      *BLANK
053700960126     C                   MOVEL     WWSitPar      D1SitPAR
053800960126     C                   ENDIF
053900940223     C*
054000940223     C* ricerca non abilitata in questa posizione
054100941118   X1C                   OTHER
054200940929     C                   SETON                                        9899
054300940127     C*
054400941118   E1C                   ENDSL
054500960126     C*
054600950612     C* imposto pos. del cursore
054700950612     C                   Z-ADD     CURR          H1RIGA
054800950612     C                   Z-ADD     CURC          H1COLO
054900940127     C*
055000940506     C                   MOVE      *HIVAL        $LASTV
055100950612     C*
055200940127     C                   ENDSR
055300960126     C/EJECT
055400960126     C************************************************************
055500960126     C* GESTIONE PARZIALIZZAZIONI
055600960126     C************************************************************
055700960126     C     GESD1         BEGSR
055800960126     c*
055900960126     C* inizializzazione videata
056000960126     C     $INZD1        IFEQ      *ON
056100960126     C                   EXSR      INZD1
056200960126     C                   MOVE      *OFF          $INZD1
056300960126     C                   ENDIF
056400960126     C* emissione piede videata se proveniente da altra
056500960126     C* salvataggio valore formato di provenienza
056600960126     C     $LASTV        IFNE      'D1'
056700960126     C* salvo stato F17 per sapere cosa fare in caso di F12
056800960126     C                   MOVE      *INKR         WINKR
056900960126     C                   WRITE     FMTT1
057000960126     C                   EXSR      GESPIE
057100960126     C                   MOVE      $LASTV        $LASTG
057200960126     C                   MOVE      'D1'          $LASTV
057300960126     C                   ENDIF
057400960126     C*
057500960126     C* se esistono errori sulla videata
057600960126     C* emetto la write del formato a indicatori spenti per vedere
057700960126     C* le eventuali decodifiche
057800960126     C     *IN99         IFEQ      *ON
057900960126     C                   MOVEA     *IN           WIN
058000960126     C                   MOVE      *ALL'0'       IN5098           49
058100960126     C                   MOVEA     IN5098        *IN(50)
058200960126     C                   WRITE     FMTD1
058300960126     C                   MOVEA     WIN           *IN
058400960126     C                   ENDIF
058500960126     C*
058600960126     C                   EXFMT     FMTD1
058700960126     C*
058800960126     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
058900960126     C* INPUT
059000960129     C* $FC3 = Abilitazioni videata D1
059100960126     C* OUTPUT
059200960126     C* *IN97 + *IN99 Errori
059300960126     C                   MOVEA     $FC3          $FC
059400960126     C                   EXSR      CTRKEY
059500960126     C*
059600960126     C     *IN99         IFEQ      *OFF
059700960126     C*
059800960126     C                   SELECT
059900960126     C* F2=Decodifica
060000960126     C     $TASTO        WHENEQ    F02
060100960126     C                   EXSR      F02D1
060200960126     C* F3=Fine
060300960126     C     $TASTO        WHENEQ    F03
060400960126     C                   EXSR      F03D1
060500960126     C* F4=Ricerca
060600960126     C     $TASTO        WHENEQ    F04
060700960126     C                   EXSR      SEARCH
060800960126     C* F5=Refresh
060900960126     C     $TASTO        WHENEQ    F05
061000960126     C                   EXSR      F05D1
061100060104c1895C* F7=Movimenti
061200060104  "  C     $TASTO        WHENEQ    F07
061300060104c1895C                   EXSR      F07D1
061400960126     C* F8=Successivo
061500960126     C     $TASTO        WHENEQ    F08
061600960126     C                   EXSR      F08D1
061700960126     C* F12=Ritorno
061800960126     C     $TASTO        WHENEQ    F12
061900960126     C                   EXSR      F12D1
062000960126     C* F18=Modifica valori assunti
062100960126     C     $TASTO        WHENEQ    F18
062200960126     C                   EXSR      F18D1
062300960126     C* F24=Altri tasti
062400960126     C     $TASTO        WHENEQ    F24
062500960126     C                   EXSR      F24D1
062600960126     C*
062700960126     C                   OTHER
062800960126     C                   EXSR      CTRD1
062900960126     C     *IN99         IFEQ      *OFF
063000960126     C* definisco l'ordinamento
063100960126     C                   EXSR      DEFORD
063200960126     C* richiamo la lista
063300960126     C                   MOVE      'S1'          $GEST
063400960126     C                   MOVE      *ON           $INZS1
063500960126     C                   clear                   W1POS
063600960126     C                   ENDIF
063700960126     C*
063800960126     C                   ENDSL
063900960126     C*
064000960126     C                   ENDIF
064100960126     C*
064200960126     C                   ENDSR
064300960126     C/EJECT
064400960126     C************************************************************
064500960126     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
064600960126     C************************************************************
064700960126     C     INZD1         BEGSR
064800960126     C*
064900960126     C                   CLEAR                   FMTD1
065000960126      *
065100960126      * Impostazione indicatore 11 per gestione campo unità
065200960126     C                   if        fgo1=*on
065300960126     C                   eval      *in11=*on
065400960126     C                   else
065500960126     C                   eval      *in11=*off
065600960126     C                   endif
065700960126     C*
065800960126     C* Se pgm  richiamato con parametri forniti , li imposto nelle
065900960126     C*    parzializzazioni
066000960126     C                   SELECT
066100960126     C* il pgm è richiamato con chiave univoca per gestione POSTA
066200960126     C     $Unique       WHENEQ    *ON
066300960126     C* il pgm è richiamato e
066400960126     C* sono stati passati dei valori di parzializzazione ma non
066500960126     C* la chiave univoca
066600960126    >C     OPZ110        WHENNE    *BLANK
066700960126     C* imposto i valori passati sui campi a video
066800960126     c                   eval      d1ctb = ctb110
066900960126     c                   eval      d1unita = unita110
067000960126     c                   eval      d1kcc   = kcc110
067100960126     c                   eval      d1ksc   = ksc110
067200960126     C                   CALLb     'XDT4000'
067300960126     C                   PARM      dtparDa110    XDTAMG
067400960126     C     D1DtparDA     PARM                    XDTGMA
067500960126     C                   PARM      2             XDTSTA
067600960126     C                   CALLb     'XDT4000'
067700960126     C                   PARM      dtparA110     XDTAMG
067800960126     C     D1DtparA      PARM                    XDTGMA
067900960126     C                   PARM      2             XDTSTA
068000960126    >C                   MOVE      sitpar110     D1sitpar
068100960318     c                   if        d1sitpar = *blanks
068200960318     c                   eval      d1sitpar = '0'
068300960318     C                   endif
068400051220c1895c                   z-add     NrPar110      d1Nrpar
068500960126     C* il pgm non è richiamato
068600960126    >C     OPZ110        WHENEQ    *BLANK
068700960126     C* imposto dei valori di default
068800960126     C                   MOVE      xscdun        d1unita
068900960126     C                   MOVE      xscdco        d1ctb
069000150701>>>>>c                   move      '4'           d1sitpar
069100051220C1895C*se alfanumerico
069200051220  "  c                   if        %subst(dsmpe:4:9)=*blanks
069300051220  "  c                   eval      %subst(dsmpe:4:9)=*zeros
069400051220  "  C                   endif
069500051220c1895c                   z-add     $nrp          d1Nrpar
069600960126     C                   ENDSL
069601150701>>>>>C
069602150701>>>>>C                   EVAL      d1serPar = '*ALL'
069700960126     C*
069800960126     C                   ENDSR
069900960126     C/EJECT
070000960126     C************************************************************
070100960126     C* GESTIONE F02 VIDEO D1
070200960126     C************************************************************
070300960126     C     F02D1         BEGSR
070400960126     C*
070500960126     C* per decodificare la videata richiamo la routine dei ctrl
070600960126     C* e poi disattivo gli eventuali messaggi di errore
070700960126     C                   EXSR      CTRD1
070800960126     C*
070900960126     C                   MOVE      *ALL'0'       IN5098
071000960126     C                   MOVEA     IN5098        *IN(50)
071100960126     C*
071200960126     C                   ENDSR
071300960126     C/EJECT
071400960126     C************************************************************
071500960126     C* GESTIONE F03 VIDEO D1
071600960126     C************************************************************
071700960126     C     F03D1         BEGSR
071800960126     C*
071900960126     C                   MOVE      *ON           $FINE
072000960126    >C                   MOVE      '1'           RET110
072100960126     C* fine programma
072200960126     C                   EXSR      ENDPGM
072300960126     C*
072400960126     C                   ENDSR
072500960126     C/EJECT
072600960126     C************************************************************
072700960126     C* GESTIONE F05 VIDEO D1
072800960126     C************************************************************
072900960126     C     F05D1         BEGSR
073000960126     C*
073100960126     C                   MOVE      *ON           $INZD1
073200960126     C*
073300960126     C                   ENDSR
073400960126     C/EJECT
073500060104c1895C************************************************************
073600060104     C* GESTIONE F07 VIDEO S1
073700060104c1895C************************************************************
073800060104     C     F07D1         BEGSR
073900060104     C*
074000060104     C                   RESET                   NDC619DS
074100060104     C                   MOVEL     '02'          OPZ619
074200060104     C                   move      Xscsoc        Societa619
074300060104     C                   MOVE      d1Ctb         Ctb619
074400060104     C                   MOVE      d1Unita       Unita619
074500060104     c* capoconto
074600060104     c                   if        (d1kcc =  'C  '  or
074700060104     c                             d1kcc =  'F  ') and
074800060104     c                             d1ksc <> *blanks
074900060104     C                   CALLB     'XALLINEA'
075000060104     C                   PARM                    D1KSC
075100060104     C                   PARM      8             LEN               5 0
075200060104     C                   PARM                    XSCINP
075300060104     C                   CALLB     'XCAPCLIFOR'
075400060104     C                   PARM                    xscsoc
075500060104     C                   PARM                    D1kcc
075600060104     C                   PARM                    d1ksc
075700060104     c                   endif
075800060104     c                   if        d1kcc <> 'C  '  and
075900060104     c                             d1kcc <> 'F  '
076000060104     C                   MOVE      D1kcc         KCC619
076100060104     c                   endif
076200060104     c* date
076300060104     c                   exsr      CtrDate
076400060104     c                   if        *in99=*off
076500060104     C                   if        DtparDaR <> *blanks
076600060104     c                   move      DtParDaR      DtparDa619
076700060104     c                   else
076800060104     c                   move      '0001-01-01'  DtparDa619
076900060104     c                   endif
077000060104     C                   if        DtparaR <> *blanks and
077100060104     c                             dtparaR <> '2039-12-31'
077200060104     c                   move      DtParAR       DtParA619
077300060104     c                   else
077400060104     c                   move      '0001-01-01'  DtParA619
077500060104     c                   endif
077600060104     c                   move      D1NrPar       NrPar619
077700060104     c*
077800060104     C                   MOVEL     NDC619DS      KPJBU
077900060104     C                   CALL      'NDC619R'
078000060104     C                   PARM                    KPJBA
078100060104     c*
078200060104     c                   eval      NDC619DS=KPJBU
078300060104     C* 1 = F3
078400060104     c                   move      Ret619        Ret110
078500060104    >C     Ret619        ifeq      '1'
078600060104     C                   MOVE      *ON           $FINE
078700060104     c                   endif
078800060104     c                   endif
078900060104     C*
079000060104c1895C                   ENDSR
079100960126     C************************************************************
079200960126     C* GESTIONE F08 VIDEO D1
079300960126     C************************************************************
079400960126     C     F08D1         BEGSR
079500960126     C*
079600960126     C                   MOVE      *ON           $FINE
079700960126    >C                   MOVE      '0'           RET110
079800960126     C*
079900960126     C                   ENDSR
080000960126     C/EJECT
080100960126     C************************************************************
080200960126     C* GESTIONE F12 VIDEO D1
080300960126     C************************************************************
080400960126     C     F12D1         BEGSR
080500960126     C*
080600960126     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
080700960126     C*  per F12 torno alla lista
080800960126     C     $LOP          IFEQ      2
080900960126     C     WINKR         OREQ      *ON
081000960126     C                   MOVE      'S1'          $GEST
081100960126     C*  altrimenti, per F12 torno a menu
081200960126     C                   ELSE
081300960126     C                   MOVE      *ON           $FINE
081400960126    >C                   MOVE      '2'           RET110
081500960126     C                   ENDIF
081600960126     C*
081700960126     C                   ENDSR
081800960126     C/EJECT
081900960126     C************************************************************
082000960126     C* GESTIONE F18 VIDEO D1
082100960126     C************************************************************
082200960126     C     F18D1         BEGSR
082300960126     C*
082400960126     C                   MOVE      'D2'          $GEST
082500960126     C                   MOVEL     *ON           $INZD2
082600960126     C*
082700960126     C                   ENDSR
082800960126     C/EJECT
082900960126     C************************************************************
083000960126     C* GESTIONE F24 VIDEO D1
083100960126     C* input
083200960126     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
083300960129     C* $FX3 =Testi tasti funzione
083400960129     C* $FC3 = Abilitazioni
083500960129     C* $FR3 = Inversioni di fondo
083600960126     C************************************************************
083700960126     C     F24D1         BEGSR
083800960126     C*
083900960126     C                   MOVEA     $FX3          $FX
084000960126     C                   MOVEA     $FC3          $FC
084100960126     C                   MOVEA     $FR3          $FR
084200960126     C                   Z-ADD     $ULKD1        $ULK
084300960126     C                   Z-ADD     $D1L01        $L01
084400960126     C                   Z-ADD     $D1L02        $L02
084500960126     C                   EXSR      RIEKEY
084600960126     C                   Z-ADD     $ULK          $ULKD1            3 0
084700960129     C                   MOVEA     $FC           $FC3
084800960126     C                   MOVEL     $KEY1         Z3KE1
084900060224C1913C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
085000960126     C                   MOVEL     $KEY2         Z3KE2
085100960126     C                   WRITE     FMTZ3
085200960126     C*
085300960126     C*
085400960126     C                   ENDSR
085500960126     C/EJECT
085600960126     C************************************************************
085700960126     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
085800960126     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
085900960126     C* 93 94 96 97 98
086000960126     C************************************************************
086100960126     C     CTRD1         BEGSR
086200960126     C*
086300960126     C                   SETOFF                                       99
086400960126     C* non imposto pos. del cursore
086500960126     C                   Z-ADD     0             H1RIGA
086600960126     C                   Z-ADD     0             H1COLO
086700960126     C*
086800960126     c*Unita
086900960126     C     FGO1          ifeq      *on
087000960126     C                   MOVE      *BLANKS       D1Unitad
087100960126     C     D1Unita       IFNE      *BLANKS
087200960126     C                   MOVE      XSCsoc        unisocieta
087300960126     C                   MOVE      D1unita       uniunita
087400960126     C     K02UNI01      chain     anuni01l                           21
087500960126     C     *in21         IFEQ      *off
087600960126     C                   MOVEL     unidesbrev    D1unitad
087700960126     C                   ELSE
087800990805C1217C*******************SETON                                        5699
087900990805C1217C                   SETON                                        5799
088000960126     C                   END
088100960126     C                   endif
088200960126     C*
088300960126     C* controllo autorizzazione
088400960126     C*
088500960126     C                   RESET                   XAutUniDS
088600960126     C                   EVAL      XAUPrf = KNmUs
088700960126     C                   EVAL      XAUSoc = XscSoc
088800960126     C                   EVAL      XAUUni = D1Unita
088900960126     C                   CALL      'XAUTUNI'
089000960126     C                   PARM                    XAutUniDS
089100960126     C                   IF        XAUAbl = *off
089200990805C1217C*******************SETON                                        5799
089300990805C1217C                   SETON                                        5899
089400960126     C                   ENDIF
089500960126     C                   END
089600960126     C*
089700960126     c* Codice contabilità
089800960126     C                   MOVE      *BLANKS       D1CTBD
089900960126     C                   MOVE      XCACUT        XTBUTE
090000960126     C                   MOVE      '1'           XTBRIC
090100960126     C                   MOVE      XSCSOC        XTBAZI
090200960126     C                   MOVE      *ZERO         XTBKEY
090300960126     C                   MOVE      'G84'         XTBCOD
090400960126     C                   MOVE      D1CTB         XTBKEY
090500960216     C                   CALLb     'XATB'
090600960126     C                   PARM                    XATBDS
090700960126     C     XTBERR        IFEQ      *ON
090800960129     C                   SETON                                        5599
090900960126     C                   ELSE
091000960126     C                   MOVEL     XTBUNI        TABG84
091100960126     C                   MOVEL     DSBG84        D1CTBD
091200960126     C*
091300960126     C* controllo se abilitato alla contabilità
091400960126     C*
091500960126     C                   MOVE      KNMUS         XACPRF
091600960126     C                   MOVE      XSCSOC        XACSOC
091700960126     C                   MOVE      D1CTB         XACCTB
091800960126     C                   MOVE      *BLANK        XACABL
091900960126     C                   CALL      'XAUTCTB'
092000960126     C                   PARM                    XCTBDS
092100960126     C*
092200960126     C     XACABL        IFEQ      *OFF
092300990805C1217C*******************SETON                                        5599
092400990805C1217C                   SETON                                        5699
092500960126     C                   ENDIF
092600960126     C                   ENDIF
092700960126     C*
092800960126     C* Controllo del conto ( Deve essere preente anche il sottoconto)
092900960126     C*
093000960126     C                   CLEAR                   D1KSCD
093100960126     C     D1KCC         IFNE      'C'
093200960126     C     D1KCC         ANDNE     'F'
093300960926     C                   CALLB     'XALLINEA'
093400960126     C                   PARM                    D1KCC
093500960126     C                   PARM      6             LEN               5 0
093600960126     C                   PARM                    XSCINP
093700960126     C                   ENDIF
093800960126     C*
093900960926     C                   CALLB     'XALLINEA'
094000960126     C                   PARM                    D1KSC
094100960126     C                   PARM      8             LEN               5 0
094200960126     C                   PARM                    XSCINP
094300960126     C*
094400960126     C* reperisco capoconto se cliente/fornitore
094500960126   B1C     D1KCC         IFEQ      'C       '
094600960126     C     D1KCC         OREQ      'F       '
094700960126     C                   CALLB     'XCAPCLIFOR'
094800960126     C                   PARM                    xscsoc
094900960126     C                   PARM                    D1kcc
095000960126     C                   PARM                    d1ksc
095100960126   E1C                   ENDIF
095200960126     C*
095300960126     C* Richiamo modulo controllo conto
095400960126     C                   EXSR      CALLMVC002
095500960126     C* Errore
095600960126   B1C     esito         IFEQ      *ON
095700960126     C     D1KSC         OREQ      *BLANKS
095800960126     C                   SETON                                        5099
095900960126     C                   ELSE
096000960126     C                   MOVEL     DESBREV002    D1KSCD
096100960126     C*
096200960126     C* Controllo se conto gestito a partite
096300960126     C                   if        tpgest002 <>'1' and tpgest002 <> '2'
096400960126     C                   SETON                                        9951
096500960126     C                   ENDIF
096600960126     C                   ENDIF
096700960126     C*
096800060104     c* controllo date
096900060104     c                   exsr      CtrDate
097000960126     C*
097100960126     C* Controllo situazione partite
097200960126     C                   MOVE      *BLANK        D1Sitpard
097300960126     C                   RESET                   X07DS
097400960126     C                   MOVE      '1'           X07RIC
097500960126     C                   MOVEL     'ANDIZ000'    X07TRC
097600960126     C                   MOVEL     'DSPSITPAR'   X07CAM
097700960126     C                   MOVEL     D1SitPar      X07VAL
097800960126     C                   MOVE      *OFF          X07ALL
097900960926     C                   CALLB     'X07VALR'
098000960126     C                   PARM                    X07DS
098100960126   B3C     X07ERR        IFEQ      '0'
098200960126     C                   MOVEL     X07DES        D1Sitpard
098300960126   X3C                   ELSE
098400960126     C                   SETON                                        9959
098500960126   E3C                   ENDIF
098600960126     c*
098700960126     C                   ENDSR
098800960126     C/EJECT
098900060104     C*----------------------------------------------------------
099000060104     C* Cotnrollo date
099100060104     C*----------------------------------------------------------
099200060104     C     CtrDate       BEGSR
099300060104     C*
099400060104     c* Data partita da
099500060104     C*
099600060104     c                   eval      dtpardaR = Datmin
099700060104     c                   if        d1dtparda <> 0
099800060104     C                   CALLb     'XDT4000'
099900060104     C     dtparDAR      PARM                    XDTAMG
100000060104     C                   PARM      d1dtparDA     XDTGMA
100100060104     C                   PARM      3             XDTSTA
100200060104     C                   IF        xdtsta < 0
100300060104     C                   SETON                                        5299
100400060104     C                   Endif
100500060104     C                   Endif
100600060104     C*
100700060104     c* Data partita a
100800060104     C*
100900060104     c                   eval      dtparaR = Datmax
101000060104     c                   if        d1dtpara <> 0
101100060104     C                   CALLb     'XDT4000'
101200060104     C     dtparAr       PARM                    XDTAMG
101300060104     C                   PARM      d1dtpara      XDTGMA
101400060104     C                   PARM      3             XDTSTA
101500060104     C                   IF        xdtsta < 0
101600060104     C                   SETON                                        5399
101700060104     C                   Endif
101800060104     C                   Endif
101900060104     C*
102000060104     c* Limiti errati
102100060104     c                   if        dtparaR  < dtpardaR
102200060104     C                   SETON                                        5499
102300060104     c                   endif
102400060104     c*
102500060104     C                   ENDSR
102600960126     C*----------------------------------------------------------
102700960126     C* Chiamata NDMVC002
102800960126     C*----------------------------------------------------------
102900960126     C     CallMvc002    BEGSR
103000960126     C*
103100960126     C* Controllo conto
103200960126     C                   Clear                   ND002DS
103300960126     C                   EVAL      LenOut = %Size(ND002DS)
103400960126     C                   CallB     'NDMVC002'
103500960126     C                   PARM                    xscsoc
103600960126     C                   PARM                    d1kcc
103700960126     C                   PARM                    d1ksc
103800960126     C                   PARM      datmin        Wdate
103900960126     C                   PARM      *OFF          GesMsg
104000960126     C                   PARM      *OFF          Esito
104100960126     C                   PARM      'ND002DS'     Struttura
104200960126     C                   PARM                    ND002DS
104300960126     C                   PARM                    LenOut
104400960126     C*
104500960126     C                   ENDSR
104600960126     C/EJECT
104700960126     C************************************************************
104800960126     C* DEFINIZIONE DELL'ORDINAMENTO
104900960126     C************************************************************
105000960126     C     DEFORD        BEGSR
105100960126     C*
105200960126     C                   Z-ADD     1             $ORD
105300960126     c*
105400960126     C                   ENDSR
105500960126     C/EJECT
105600960126     C************************************************************
105700960126     C* GESTIONE VALORI ASSUNTI
105800960126     C************************************************************
105900960126     C     GESD2         BEGSR
106000960126     C*
106100960126   B1C     $INZD2        IFEQ      *ON
106200960126     C                   EXSR      INZD2
106300960126     C                   MOVE      *OFF          $INZD2
106400960126   E1C                   ENDIF
106500960126     C*
106600960126   B1C     $LASTV        IFNE      'D2'
106700960126     C                   EXSR      GESPIE
106800960126     C                   MOVE      $LASTV        $LASTG
106900960126     C                   MOVE      'D2'          $LASTV
107000960126   E1C                   ENDIF
107100960126     C*
107200960126     C* se esistono errori sulla videata
107300960126     C* emetto la write del formato a indicatori spenti per vedere
107400960126     C* le eventuali decodifiche
107500960126   B1C     *IN99         IFEQ      *ON
107600960126     C                   MOVEA     *IN           WIN
107700960126     C                   MOVE      *ALL'0'       IN5098           49
107800960126     C                   MOVEA     IN5098        *IN(50)
107900960126     C                   WRITE     FMTD2
108000960126     C                   MOVEA     WIN           *IN
108100960126   E1C                   ENDIF
108200960126     C*
108300960126     C                   EXFMT     FMTD2
108400960126     C*
108500960126     C* CONTROLLO ABILITAZIONE TASTI
108600960126     C* INPUT
108700960126     C* $FC3 = Abilitazioni videata D2
108800960126     C* OUTPUT
108900960126     C* *IN97 + *IN99 Errori
109000960126     C                   MOVEA     $FC2          $FC
109100960126     C                   EXSR      CTRKEY
109200960126     C*
109300960126   B1C     *IN99         IFEQ      *OFF
109400960126     C*
109500960126   B2C                   SELECT
109600960126     C* F3=Fine
109700960126     C     $TASTO        WHENEQ    F03
109800960126     C                   EXSR      F03D2
109900960126     C* F12=Ritorno
110000960126     C     $TASTO        WHENEQ    F12
110100960126     C                   EXSR      F12D2
110200960126     C* F24=Altri tasti
110300960126     C     $TASTO        WHENEQ    F24
110400960126     C                   EXSR      F24D2
110500960126     C*
110600960126     C                   OTHER
110700960126     C                   EXSR      CTRD2
110800960126   B3C     *IN99         IFEQ      *OFF
110900960126     C                   MOVE      $LASTG        $GEST
111000960126   B4C                   SELECT
111100960126     C     $LASTG        WHENEQ    'S1'
111200960126     C* se è stato modificato l'ordinamento di default, ricarico la lista
111300960126   B5C     $ORD          IFNE      D2ORD
111400960126     C                   MOVE      *ON           $INZS1
111500960126     C                   clear                   W1POS
111600960126   E5C                   ENDIF
111700960126     C     $LASTG        WHENEQ    'D1'
111800960126     C* per ora niente
111900960126   E4C                   ENDSL
112000960126     C* definisco i nuovi valori di default
112100960126     C                   Z-ADD     D2LOP         $LOP
112200960126     C                   Z-ADD     D2ORD         $ORD
112300051220C1895C                   z-add     d2nrpar       $nrp
112400960126     C*
112500960126     C* aggiorno valori assunti
112600960126     C* se richiesto il salvataggio
112700960126   B4C     D2BSV         IFEQ      1
112800960126     C                   EXSR      AGGMPE
112900960126   E4C                   ENDIF
113000960126     C*
113100960126   E3C                   ENDIF
113200960126     C*
113300960126   E2C                   ENDSL
113400960126     C*
113500960126   E1C                   ENDIF
113600960126     C*
113700960126     C                   ENDSR
113800960126     C/EJECT
113900960126     C************************************************************
114000960126     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
114100960126     C************************************************************
114200960126     C     INZD2         BEGSR
114300960126     C*
114400960126     C                   CLEAR                   FMTD2
114500960126     C                   MOVEL     $LOP          D2LOP
114600960126     C                   MOVE      $ORD          D2ORD
114700051220C1895C*se alfanumerico
114800051220  "  c                   if        %subst(dsmpe:4:9)=*blanks
114900051220  "  c                   eval      %subst(dsmpe:4:9)=*zeros
115000051220  "  C                   endif
115100051220c1895c                   z-add     $nrp          D2NrPar
115200051220     c*
115300960126     C                   ENDSR
115400960126     C/EJECT
115500960126     C************************************************************
115600960126     C* GESTIONE F03 VIDEO D2
115700960126     C************************************************************
115800960126     C     F03D2         BEGSR
115900960126     C*
116000960126     C                   MOVE      *ON           $FINE
116100960126     C* fine programma
116200960126     C                   EXSR      ENDPGM
116300960126     C*
116400960126     C                   ENDSR
116500960126     C/EJECT
116600960126     C************************************************************
116700960126     C* GESTIONE F12 VIDEO D2
116800960126     C************************************************************
116900960126     C     F12D2         BEGSR
117000960126     C*
117100960126     C                   MOVE      $LASTG        $GEST
117200960126     C*
117300960126     C                   ENDSR
117400960126     C/EJECT
117500960126     C************************************************************
117600960126     C* GESTIONE F24 VIDEO D2
117700960126     C************************************************************
117800960126     C     F24D2         BEGSR
117900960126     C*
118000960126     C                   MOVEA     $FX2          $FX
118100960126     C                   MOVEA     $FC2          $FC
118200960126     C                   MOVEA     $FR2          $FR
118300960126     C                   Z-ADD     $ULKD2        $ULK
118400960126     C                   Z-ADD     $D2L01        $L01
118500960126     C                   Z-ADD     $D2L02        $L02
118600960126     C                   EXSR      RIEKEY
118700960126     C                   Z-ADD     $ULK          $ULKD2            3 0
118800960126     C                   MOVEA     $FC           $FC2
118900960126     C                   MOVEL     $KEY1         Z2KE1
119000060224C1913C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
119100960126     C                   WRITE     FMTZ2
119200960126     C*
119300960126     C                   ENDSR
119400960126     C/EJECT
119500960126     C************************************************************
119600960126     C* CONTROLLO VIDEATA VALORI ASSUNTI
119700960126     C************************************************************
119800960126     C     CTRD2         BEGSR
119900960126     C*
120000960126     C                   SETOFF                                       99
120100960126     C* non imposto pos. del cursore
120200960126     C                   Z-ADD     0             H1RIGA
120300960126     C                   Z-ADD     0             H1COLO
120400960126     C*
120500960126     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
120600960126     C     D2LOP         IFEQ      0
120700960126     C                   SETON                                          5099
120800960126     C                   ENDIF
120900960126     C*
121000960126     C* OBBLIGATORIO VALORE PER ORDINAMENTO
121100960126     C     D2ORD         IFEQ      0
121200960126     C                   SETON                                          5199
121300960126     C                   ENDIF
121400960126     C*
121500960126     C                   ENDSR
121600940131     C/EJECT
121700940224     C************************************************************
121800940131     C* RIEMPIMENTO OPZIONI
121900940224     C************************************************************
122000940131     C     RIEOPZ        BEGSR
122100940223     C*
122200940223     C* Input :
122300940223     C* - $TX = schiera con tutti i testi disponibili
122400940223     C* - $FL = schiere con i flag di validità associati ai testi
122500940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
122600940223     C*          opzione usato
122700940223     C* - WMXO = variabile contenente il numero massimo di testi
122800940223     C*          opzione usabili
122900940721     C* - $L01 = Lunghezza prima riga
123000940721     C* - $L02 = Lunghezza seconda riga
123100940223     C* Output :
123200940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
123300940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
123400941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
123500940223     C*
123600950522     C                   CALLB     'X04OPMSG'
123700940223     C* input
123800940223     C                   PARM                    $TX
123900940223     C                   PARM                    $FL
124000940225     C                   PARM                    WCOZ              3 0
124100940223     C                   PARM                    WMXO              3 0
124200940223     C* output
124300940223     C                   PARM                    WOPZ1            76
124400940223     C                   PARM                    WOPZ2            76
124500941214     C                   PARM                    FF23              1
124600950301     C* input
124700940721    >C                   PARM                    $L01              3 0
124800951108    >C                   PARM                    $L02              3 0
124900940223     C*
125000060224C1913C                   PARM                    $ALLOPT         960
125100060224C1913C*
125200940131     C                   ENDSR
125300940506     C/EJECT
125400940506     C************************************************************
125500940506     C* RIEMPIMENTO TASTI FUNZIONE
125600940506     C************************************************************
125700940506     C     RIEKEY        BEGSR
125800940506     C*
125900940506     C* Input :
126000940506     C* - $FX = schiera con tutti i testi disponibili
126100940506     C* - $FC = schiere con i flag di validità associati ai testi
126200940506     C* - $FR = schiere con i flag di REVERSE IMAGE
126300940506     C* - $ULK = ultimo testo caricato
126400940721     C* - $L01 = Lunghezza prima riga
126500940721     C* - $L02 = Lunghezza seconda riga
126600940506     C* Output :
126700940506     C* - $ULK
126800940609     C* - $KEY1= 1° riga di tasti funzione
126900940609     C* - $KEY2= 2° riga di tasti funzione
127000940506     C*
127100950522     C                   CALLB     'XKEYMSG'
127200940506     C* input
127300940506     C                   PARM                    $FX
127400940506     C                   PARM                    $FC
127500940506     C                   PARM                    $FR
127600940506     C                   PARM                    $ULK              3 0
127700940506     C* output
127800940506     C                   PARM                    $KEY1            79
127900940506     C                   PARM                    $KEY2            79
128000950301     C* input
128100940721    >C                   PARM                    $L01              3 0
128200940721    >C                   PARM                    $L02              3 0
128300940506     C*
128400060224C1913C                   PARM                    $ALLFUNCT       480
128500060224C1913C*
128600940506     C                   ENDSR
128700940224     C/EJECT
128800940224     C************************************************************
128900940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
129000940224     C************************************************************
129100940224     C     GESPIE        BEGSR
129200940224     C*
129300940224     C                   SELECT
129400960126     C* Parzializzazioni
129500960126     C     $GEST         WHENEQ    'D1'
129600960126     C     $ULKD1        IFEQ      0
129700960126     C* riempimento opzioni
129800960126     C                   EXSR      F24D1
129900960126     C                   ELSE
130000960126     C                   WRITE     FMTZ3
130100960126     C                   ENDIF
130200960126     C* Modifica valori assunti
130300960126     C     $GEST         WHENEQ    'D2'
130400960126     C     $ULKD2        IFEQ      0
130500960126     C                   EXSR      F24D2
130600960126     C                   ELSE
130700960126     C                   WRITE     FMTZ2
130800960126     C                   ENDIF
130900940506     C* Lista
131000940506     C     $GEST         WHENEQ    'S1'
131100940506     C     $ULKS1        IFEQ      0
131200951110     C*
131300951110     C* Call routine editazione x importo in moneta di conto
131400951110     C* da fare solo una volta poichè si fa quando il caricamento pagina
131500951113     C* non è presente ( se c'era il caricamento a pagina, avevo già reperit
131600960129     C* o questi dati nella routine INZS1)
131700951110     C                   IF        $carpag = *off
131800951113     C* segno in moneta di conto
131900951113     C                   Z-ADD     WSaldoTotd    WtotDare
132000951113     C                   Z-ADD     WSaldoTotA    WTotAvere
132100951110     C                   exsr      RepSegno
132200951110     C* importo
132300951110     C                   Z-add     Wsaldo        WImporto
132400971104     C                   Z-add     NrDec         WNrDec
132500951110     C                   Eval      $LenFld = %Size(Z1Saldo)
132600951110     C* allineo dare a sinistra
132700961009     C                   if        Wsegno = XscDare
132800951110     C                   SUB       5             $lenfld
132900951110     C                   endif
133000951110     C                   EXSR      Edit
133100951110     C                   Eval      Z1Saldo = $RcvrVar
133200951110     C                   eval      z1segno = Wsegno
133300951110     C                   endif
133400951110     c*
133500940506     C                   EXSR      F24S1
133600940506     C                   ELSE
133700960126     C                   WRITE     FMTZ1
133800940506     C                   ENDIF
133900940506     C                   ENDSL
134000940506     C*
134100940224     C                   ENDSR
134200951110     C/EJECT
134300951110     C************************************************************
134400951113     C* Repsegno Reperisce segno moneta di conto
134500951110     C************************************************************
134600951110     C     RepSegno      BEGSR
134700951110     C*
134800951110     C                   Select
134900951113     C                   when      Wtotdare   > WtotAvere
135000961009     C                   eval      WSegno   = XscDare
135100951113     C                   eval      WSaldo   = Wtotdare   - Wtotavere
135200951110     C*
135300951113     C                   when      Wtotdare   < Wtotavere
135400961009     C                   eval      WSegno   = XscAvere
135500951113     C                   eval      WSaldo   = Wtotavere  - Wtotdare
135600951110     C*
135700951113     C                   when      Wtotdare   = Wtotavere
135800951110     C                   eval      WSegno   = ' '
135900951110     C                   eval      WSaldo   = 0
136000951110     C                   endsl
136100951113     C*
136200951113     C                   ENDSR
136300951110     C*
136400951113     C/EJECT
136500951113     C************************************************************
136600951113     C* Repsegno Reperisce segno divisa
136700951113     C************************************************************
136800951113     C     RepSegnod     BEGSR
136900951113     C*
137000951113     C                   Select
137100951113     C                   when      WtotdareD  > WtotAvereD
137200961009     C                   eval      WSegno   = XscDare
137300951113     C                   eval      WSaldo   = WtotdareD  - WtotavereD
137400951113     C*
137500951113     C                   when      WtotdareD  < WtotavereD
137600961009     C                   eval      WSegno   = XscAvere
137700951113     C                   eval      WSaldo   = WtotavereD - WtotdareD
137800951113     C*
137900951113     C                   when      WtotdareD  = WtotavereD
138000951113     C                   eval      WSegno   = ' '
138100951113     C                   eval      WSaldo   = 0
138200951113     C                   endsl
138300951113     C*
138400951110     C                   ENDSR
138500940224     C/EJECT
138600940127     C************************************************************
138700940131     C* GESTIONE LISTA
138800940127     C************************************************************
138900940127     C     GESS1         BEGSR
139000940223     C* inizializzazione videata
139100940223     C     $INZS1        IFEQ      *ON
139200940127     C                   EXSR      INZS1
139300940223     C                   MOVE      *OFF          $INZS1
139400940127     C                   ENDIF
139500940223     C* emissione piede videata se proveniente da altra
139600940223     C* salvataggio valore formato di provenienza
139700940202     C     $LASTV        IFNE      'S1'
139800960126     C                   WRITE     FMTT1
139900940224     C                   EXSR      GESPIE
140000940202     C                   MOVE      $LASTV        $LASTG
140100940202     C                   MOVE      'S1'          $LASTV
140200941027     C                   ENDIF
140300940223     C*
140400940223     C     WMAX          IFEQ      0
140500960126     C                   WRITE     FMTD3
140600940211     C                   ENDIF
140700951109     C                   Eval      *in09=c1mode
140800940127     C*
140900960126     C                   EXFMT     FMTC1
141000940204     C     C1NRR         IFNE      0
141100940204     C                   Z-ADD     C1NRR         WSFL
141200940204     C                   ENDIF
141300951109     C*
141400951109     C     SFLNRR        IFNE      0
141500951109     C                   Z-ADD     SFLNRR        C1RCD
141600951109     C                   Z-ADD     SFLNRR        SFLNRRSAV
141700951109     C                   ELSE
141800951109     C                   Z-ADD     SFLNRRSAV     C1RCD
141900951109     C                   ENDIF
142000960130     C                   MOVE      '1'           Wxmsginz
142100940506     C*
142200940506     C* controllo abilitazione tasti funzione
142300960129     C                   MOVEA     $FC1          $FC
142400960129     C                   EXSR      CTRKey
142500941205     C*
142600940506     C     *IN99         IFEQ      *OFF
142700940127     C*
142800941027     C                   SELECT
142900940127     C* F3=Fine
143000940506     C     $TASTO        WHENEQ    F03
143100940309     C                   EXSR      F03S1
143200951120     C* F8=Selezione seguente
143300951120     C     $TASTO        WHENEQ    F08
143400951120     C                   EXSR      F08S1
143500951108     C* F11=Cambio moneta
143600951108     C     $TASTO        WHENEQ    F11
143700951108     C                   EXSR      F11S1
143800940131     C* F12=Ritorno
143900940506     C     $TASTO        WHENEQ    F12
144000940309     C                   EXSR      F12S1
144100951108     C* F15=Interrogazioni
144200951108     C     $TASTO        WHENEQ    F15
144300951108     C                   EXSR      F15S1
144400960129     C* F17=Parzializzazioni
144500960129     C     $TASTO        WHENEQ    F17
144600960129     C                   EXSR      F17S1
144700960129     C* F18=Modifica valori assunti
144800960129     C     $TASTO        WHENEQ    F18
144900960129     C                   EXSR      F18S1
145000940224     C* F23=Altre opzioni
145100940506     C     $TASTO        WHENEQ    F23
145200940309     C                   EXSR      F23S1
145300940224     C* F24=Altri tasti
145400940506     C     $TASTO        WHENEQ    F24
145500940309     C                   EXSR      F24S1
145600951108     C* ROLL-UP (se gestito a pagina)
145700940506     C     $TASTO        WHENEQ    ROLLUP
145800951108     C     $CARPAG       ANDEQ     *ON
145900940127     C                   EXSR      ROLS1
146000940127     C*
146100941027     C                   OTHER
146200940127     C* CONTROLLO DATI
146300940131     C                   EXSR      CTRC1
146400940201     C     *IN99         IFEQ      *OFF
146500940131     C                   EXSR      CTRS1
146600941027     C                   ENDIF
146700960129     c*
146800960129     c* se digitato F6 eseguo controllo opzioni taglia e incolla
146900960129     C     $TASTO        ifeq      F06
147000960129     C     *IN99         andeq     *OFF
147100960129     C                   EXSR      CTRopz
147200990811      *
147300960129     C     *IN99         ifeq      *OFF
147400990819C1212C     FGO1          AndEq     *On
147500990819  "   *Controllo le abilitazioni alle unità e alle riconciliazioni
147600990819  "   *se la societa gestisce l'unità
147700990806  "  C                   EXSR      CTRAutUni
147800990811  "  C                   EndIf
147900990806  "   *
148000990811  "   *  se ho tutte le abilitazioni necessarie
148100990811C1212C                   If        *In99  =  *Off
148200990806     c                   exsr      Riconcilia
148300960129     c                   endif
148400960129     c                   endif
148500941205     C*
148600941027     C                   ENDSL
148700940127     C*
148800940506     C                   ENDIF
148900941205     C*
149000940127     C                   ENDSR
149100940224     C/EJECT
149200940127     C************************************************************
149300940131     C* INIZIALIZZAZIONE LISTA
149400940127     C************************************************************
149500940127     C     INZS1         BEGSR
149600940302     C* pulizia SFL
149700940128     C                   SETOFF                                         3031
149800960126     C                   WRITE     FMTC1
149900940128     C                   SETON                                          31
150000940127     C*
150100940128     C* riempimento opzioni
150200960129     C* Se non fornitore e neanche cliente disattivo opzione mix
150300960129     C* Prima la attivo
150400960129     C                   Z-ADD     1             X
150500960129     C     32            LOOKUP    $OP1(X)                                21
150600960129     C     *IN21         IFEQ      *ON
150700960129     C                   MOVE      'S'           $FL1(X)
150800960129     C                   ENDIF
150900960129     c* poi disattivo se...
151000960129     C                   if        snatura002 <> 'F' and snatura002 <> 'C'
151100960129     C                   Z-ADD     1             X
151200960129     C     32            LOOKUP    $OP1(X)                                21
151300960129     C     *IN21         IFEQ      *ON
151400960129     C                   MOVE      'N'           $FL1(X)
151500960129     C                   ENDIF
151600960129     C                   endif
151700940223     C                   MOVEA     $TX1          $TX
151800940223     C                   MOVEA     $FL1          $FL
151900940309     C                   Z-ADD     0             WCOZ
152000940225     C*  equivale al num.di elementi della schiera $TX1
152100960126     C                   Z-ADD     $DIMOP        WMXO
152200960126     C                   Z-ADD     $C1Op1        $L01
152300960126     C                   Z-ADD     $C1Op2        $L02
152400940128     C                   EXSR      RIEOPZ
152500941214     C                   Z-ADD     23            X
152600951109     C                   MOVE      FF23          $FC1(X)
152700940608     C                   MOVEL     WOPZ1         C1OPZ1
152800060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
152900951108     C                   MOVEL     WOPZ2         C1OPZ2
153000940225     C                   Z-ADD     WCOZ          C1COZ
153100960126     C                   EVAL      C1KCC = d1kcc
153200960126     C                   EVAL      C1KsC = d1ksc
153300960126     C                   EVAL      C1Descr = DESCR002
153400960126     C                   EVAL      C1Desagg = DESAGG002
153500960129     C*
153600960129     C* visualizzo il sfl troncato
153700960129     C                   Eval      c1mode=*on
153800960129     C*
153900960129     C* Se non richiesti parametri particolari, chiamo routine X20SCADR
154000960129     C* che mi restituisce il saldo del conto e imposto caricamento a
154100960129     c* pagine, altrimenti il caricamento è totale
154200960129     c                   eval      Wcalcsaldo = *off
154300960129     C                   if        d1dtparda  = 0 and
154400960129     C                             d1dtpara   = 0 and
154500960129     C                             (D1sitpar = '0' or d1sitpar = '1' or
154600960129     C                              D1Sitpar = '4')
154700960129     c                   exsr      callX20
154800960129     c                   endif
154900960129     C*
155000960129     C* Definisco caricamento subfile
155100960129     C* Se il saldo è già stato calcolato, il caricamento è a pagina e
155200960129     c* ho già anche i flag di presenza a totale di conto
155300960129     C                   if        Wcalcsaldo = *on
155400960129     C                   EVAL      $carpag = *on
155500960129     C                   EVAL      SFLPAG = 8
155600960129     C                   Z-add     Wsaldo        WImporto
155700971104     C                   Z-add     NrDec         WNrDec
155800960129     C                   Eval      $LenFld = %Size(Z1Saldo)
155900961009     C                   if        Wsegno   = XscDare
156000960129     C                   sub       5             $lenFld
156100960129     C                   endif
156200960129     C                   EXSR      Edit
156300960129     C                   Eval      Z1Saldo = $RcvrVar
156400960129     C                   eval      z1segno = Wsegno
156500960129     C*
156600960129     C                   else
156700960129     C* caricamento totale
156800960129     c                   eval      $carpag = *off
156900960129     C                   EVAL      SFLPAG = 9999
157000960129     C                   eval      z1saldo = *blanks
157100960129     C                   eval      z1segno = ' '
157200960129     C                   endif
157300960129     C*
157400960129     C* Variabili appoggio
157500960129     C*
157600960129     C                   Z-ADD     SFLPAG        WPAG
157700940128     C*
157800951108     C* CARICAMENTO SFL A PAGINE o intero
157900940128     C*
158000940201     C                   Z-ADD     0             S1NRR
158100940128     C                   Z-ADD     0             WMAX
158200940224     C*
158300940224     C* Posizionamento su file pilota
158400940302     C                   CLEAR                   C1POS
158500941214     C                   EXSR      SETANA
158600940302     C* ripristino i valori del posizionamento
158700940302     C                   MOVE      W1POS         C1POS
158800940131     C*
158900940201     C* Lettura file pilota
159000941214     C                   EXSR      REDANA
159100940131     C*
159200951108     C* Carico 1a pagina o tutto
159300940127     C                   EXSR      ROLS1
159400940201     C                   Z-ADD     SFLPAG        WPAG
159500940127     C*
159600940127     C                   ENDSR
159700960129     C/EJECT
159800960129     C************************************************************
159900960129     C* richiamo routine x20scad per saldo conto
160000960129     C************************************************************
160100960129     C     CallX20       BEGSR
160200960129     C*
160300960129     C                   exsr      riex20scad
160400960129     C                   CALL      'X20SCADR'
160500960129     C                   PARM                    X20SCADDS
160600960129     C                   PARM                    X20SCADXDS
160700960129     C                   if        x20errori = ' '
160800960129     C                   eval      Wcalcsaldo = *on
160900960129     C* calcolo saldo in moneta di conto per il conto richiesto
161000960129     C                   clear                   Wtotdare
161100960129     C                   clear                   Wtotavere
161200960129     C                   add       x20tdld       wtotdare
161300960129     C                   add       x20tdlp       wtotdare
161400960129     C                   add       x20tdlg       wtotdare
161500960129     C                   add       x20tald       wtotavere
161600960129     C                   add       x20talp       wtotavere
161700960129     C                   add       x20talg       wtotavere
161800960129     c                   exsr      repsegno
161900960129     c                   endif
162000960129     C*
162100960129     c                   endsr
162200960129     C/EJECT
162300960129     C************************************************************
162400960129     C* RIEMPIMENTO DS PER saldo conto
162500960129     C************************************************************
162600960129     C     RIEx20scad    BEGSR
162700960129     c*
162800960129     C                   RESET                   x20scadds
162900960129     C                   RESET                   x20scadxds
163000960129     C                   eval      x20societa = xscsoc
163100960129     C                   eval      x20unita   = d1unita
163200960129     C                   eval      x20ctb     = d1ctb
163300960129    >C                   eval      x20def     = *on
163400960129    >C                   eval      x20prov    = *on
163500960129    >C                   eval      x20gest    = *on
163600960129     C                   eval      x20dtregmd = datmax
163700960129     C                   eval      x20dtregmp = datmax
163800960129     C                   eval      x20dtregmg = datmax
163900960129     C                   eval      x20kcc     = d1kcc
164000960129     C                   eval      x20ksc     = d1ksc
164100980821     C                   eval      x20snatura = snatura002
164200960129     C                   eval      x20parsca  = 'S'
164300960129     C                   eval      x20elasca  = 'R'
164400960129     C                   eval      x20DtSca   = datmax
164500960129     C                   eval      x20ppa     = 'N'
164600960129     C                   eval      x20lrt     = 'R'
164700960129     C*
164800960129     C                   ENDSR
164900940127     C/EJECT
165000940127     C************************************************************
165100940131     C* CARICAMENTO PAGINA LISTA
165200940127     C************************************************************
165300940127     C     ROLS1         BEGSR
165400940127     C*
165500940128     C                   SETOFF                                       32
165600940223     C                   Z-ADD     0             Y
165700940127     C                   Z-ADD     WMAX          S1NRR
165800940127     C*
165900940127     C* Leggo dal file anagrafico per caricare la lista
166000940127     C*
166100950606     C     $EndFile      DOWEQ     *OFF
166200940201     C     Y             ANDLT     WPAG
166300940127     C*
166400940131     C                   EXSR      RIES1
166500940127     C*
166600940127     C                   ADD       1             S1NRR
166700940127     C                   ADD       1             Y
166800940127     C*
166900960126     C                   WRITE     FMTS1
167000940131     C*
167100941214     C                   EXSR      REDANA
167200940128     C*
167300941027     C                   ENDDO
167400940127     C*
167500940223     C                   Z-ADD     S1NRR         WMAX                 30
167600940127     C*
167700940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
167800940127     C*
167900940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
168000940127     C                   MVR                     RESTO             3 0
168100940131     C     PAGINE        MULT      SFLPAG        C1RCD
168200941027     C     RESTO         IFGT      0
168300940127     C                   ADD       1             C1RCD
168400941027     C                   ELSE
168500940407     C                   SUB       SFLPAG        C1RCD
168600940407     C                   ADD       1             C1RCD
168700941027     C                   ENDIF
168800940128     C*
168900940127     C                   ENDSR
169000940224     C/EJECT
169100940224     C************************************************************
169200940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
169300940224     C************************************************************
169400941214     C     SETANA        BEGSR
169500940224     C*
169600950704     C                   MOVE      XSCSOC        PPASOCIETA
169700960126     C                   MOVE      d1ctb         PPActb
169800960126     C                   MOVE      d1kcc         PPAKCC
169900960126     C                   MOVE      d1ksc         PPAKSC
170000950704     C*
170100940224     C                   SELECT
170200941214     C*
170300941214     C* Gestione posizionamento
170400950301     C* Se il campo W1POS è > *blank imposto il posizionamento con
170500950301     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
170600941214     C*
170700960126    >C* Ord.2 = chiave ordinamento 1 (ascendente)
170800960126     C     $ORD          WHENEQ    1
170900950705   B1C     W1POS         IFNE      0
171000951109     C                   CALLb     'XDT4000'
171100951109     C                   PARM                    XDTAMG
171200951109     C                   PARM      w1pos         XDTGMA
171300951109     C                   PARM      4             XDTSTA
171400951109    >C                   MOVE      xdtamg        ppadtpar
171500940302     C                   ELSE
171600960126    >C                   MOVE      DtpardaR      ppadtpar
171700950705   E1C                   ENDIF
171800960126     C* Se chieste partite solo aperte
171900960126     C* uso la vista logica che omette partite chiuse
172000960126     C                   if        d1sitpar  <> '1'
172100951113    >C     K05PPA01      SETLL     NDPPA03
172200951113     C                   else
172300951113    >C     K05PPA01      SETLL     NDPPA01
172400951113     C                   endif
172500951108     c*
172600940224     C                   ENDSL
172700940224     C*
172800940224     C                   ENDSR
172900940128     C/EJECT
173000940128     C************************************************************
173100940131     C* LETTURA RCD ARCHIVIO PILOTA
173200940128     C************************************************************
173300941214     C     REDANA        BEGSR
173400940128     C*
173500950606     C                   MOVEL     *OFF          $EndFile
173600940131     C                   MOVEL     *OFF          $RCDOK
173700940131     C*
173800950606     C     $EndFile      DOUEQ     *ON
173900940131     C     $RCDOK        OREQ      *ON
174000940131     C*
174100941027     C                   SELECT
174200951113     C* Ascendente
174300960126     C     $ORD          WHENEQ    1
174400951113     C* Solo aperte
174500960126     C                   if        d1sitpar  <> '1'
174600951113    >C     K04PPA01      READE     NDPPA03                                21
174700951113     C                   else
174800951113    >C     K04PPA01      READE     NDPPA01                                21
174900951113     C                   endif
175000941027     C                   ENDSL
175100940131     C*
175200950606     C                   MOVE      *IN21         $EndFile
175300940131     C*
175400950606     C     $EndFile      IFEQ      *OFF
175500941214     C                   EXSR      SELANA
175600941027     C                   ENDIF
175700940131     C*
175800941027     C                   ENDDO
175900940131     C*
176000940131     C                   MOVE      *IN21         *IN33
176100940131     C*
176200940131     C                   ENDSR
176300940224     C/EJECT
176400940224     C************************************************************
176500940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
176600940224     C************************************************************
176700940224     C     CLCPAG        BEGSR
176800940224     C* Input :
176900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
177000940224     C*          cursore
177100940224     C* - SFLPAG = numero rcd per pagina sfl
177200940224     C* Output :
177300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
177400940224     C*
177500940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
177600940224     C                   MVR                     RESTO             3 0
177700940224     C     RESTO         IFGT      0
177800940224     C                   ADD       1             PAGINE
177900940224     C                   ENDIF
178000940224     C     PAGINE        MULT      SFLPAG        WPAG
178100940224     C*
178200940224     C                   ENDSR
178300940131     C/EJECT
178400940131     C************************************************************
178500940131     C* SELEZIONI SULLA RIGA LETTA
178600940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
178700940314     C* non rientra nei parametri di parzializzazione
178800940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
178900940314     C* superiore stabilito nei parametri
179000940314     C*
179100940131     C************************************************************
179200941214     C     SELANA        BEGSR
179300940131     C*
179400940202     C                   MOVEL     *ON           $RCDOK
179500940131A1211X*                  MOVEL     *ALL'9'       ALL9             15
179600940131A1211C                   MOVEL     *hival        ALL9             15
179700950314     C*
179800951110     C                   SELECT
179900940314     C*
180000940314     C* SELEZIONI RISPETTO AI VALORI RCD
180100940314     C*
180200950314     C* Scartare i record che non rispondono alle selezioni effettuate
180300950301     C* esempio : esclusione degli annullati.
180400951109     c*
180500960201     C* Unità
180600960201     C                   When      PPaunita <> D1unita  and
180700960201     C                             d1unita <>  *blanks
180800960201     C                   move      *off          $RCDok
180900960126     C* Date partite
181000960126     C                   When      PPaDtpar <  DtparDaR   or
181100960126     C                             PPaDtpar >  DtparAR
181200951109     C                   move      *off          $RCDok
181300051220C1895C* Nr.  partita
181400051220  "  C                   When      PPaNrPar <> D1Nrpar and
181500051221  "  c                             D1NrPar <> 0
181600060119  "  C                   move      *off          $RCDok
181700051221C1895c*
181800960207     C* se già riconciliata
181900960207     C                   When      PPamodric = '2'
182000960207     C                   move      *off          $RCDok
182001150701>>>>>** Serie partita
182002150701>>>>>C                   When      d1serPar <> '*ALL' AND ppaSerPar <> d1serPar
182004150701>>>>>C                   move      *off          $RCDok
182100951109     c*
182200951109     C                   ENDSL
182300951113     C*
182400951113     C*  chiamo routine  per altre selezioni
182500951113     C*  se selezioni precedentI  sono ok e se la partita non è chiusa
182600951113     C*  perchè in questo caso non mi servono i dati di x20 perchè sono
182700951113     C*  tutti a 0
182800960201     C                   clear                   WtotDare
182900960201     C                   clear                   WtotAvere
183000960201     C                   clear                   WtotDared
183100960201     C                   clear                   WtotAvereD
183200960201     c*
183300951113     C                   if        $rcdok = *on and ppaparchiu <> '1'
183400951113     C                   exsr      riex20scad
183500960129     C                   eval      x20dtpar   = PPADTPAR
183600960129     C                   eval      x20nrpar   = PPANRPAR
183700960129     C                   eval      x20serpar  = PPASerpar
183800951113     C                   CALL      'X20SCADR'
183900951113     C                   PARM                    X20SCADDS
184000951113     C                   PARM                    X20SCADXDS
184100951113     C* Se tutto ok
184200951113     C                   if        x20errori = ' '  and
184300951113     C                             x20NrMov <> 0
184400951113     C* calcolo totale dare e avere sia in moneta di conto che divisa della
184500951113     C* partita
184600951113     C* mc
184700951113     C                   add       x20tdld       WtotDare
184800951113     C                   add       x20tdlp       WtotDare
184900951113     C                   add       x20tdlg       WtotDare
185000951113     C                   add       x20tald       WtotAvere
185100951113     C                   add       x20talp       WtotAvere
185200951113     C                   add       x20talg       WtotAvere
185300951113     C* divisa
185400951113     C                   add       x20tddd       WtotDareD
185500951113     C                   add       x20tddp       WtotDared
185600951113     C                   add       x20tddg       WtotDareD
185700951113     C                   add       x20tadd       WtotAvereD
185800951113     C                   add       x20tadp       WtotAvereD
185900951113     C                   add       x20tadg       WtotAvereD
186000951113     C* Controlli situazione partita
186100951113     C                   EXSR      CtrSitPar
186200951116     C                   else
186300951116     C                   eval      $rcdok = *off
186400951113     C                   endif
186500951113     C                   endif
186600951109     C*
186700940202     C                   ENDSR
186800951113     C/EJECT
186900951113     C************************************************************
187000951113     C* controlli situazione partita dopo chiamata a X20SCADR
187100951113     C************************************************************
187200951113     C     CtrSitPar     BEGSR
187300951113     C*
187400951113     C* testo flag situazione partita richiesto sui risultati avuti da
187500951113     C* x20scadr
187600951113     C                   select
187700951113     C* Aperte/ a rischio
187800960126     C                   When      D1Sitpar   = '0'
187900960103     C* Sono aperte se dare <> da avere  in moneta di conto def e gestionale
188000960314     C* Sono aperte se dare <> da avere  in divisa          def e gestionale
188100960314     C* se dare provvisorio <> 0 o avere provvisorio <> 0 in moneta di conto
188200960314     C* se dare provvisorio <> 0 o avere provvisorio <> 0 in divisa
188300960103     C* ese uno dei campi scaduto, a scadere, a rischio,
188400960103     C* portafoglio, é <> 0
188500960103     C                   if        x20tdld <> x20tald or
188600960103     C                             x20tdlg <> x20talg or
188700960314     C                             x20tddd <> x20tadd or
188800960314     C                             x20tddg <> x20tadg or
188900960103     C                             x20tdlp <> 0       or
189000960103     C                             x20talp <> 0       or
189100960314     C                             x20tddp <> 0       or
189200960314     C                             x20tadp <> 0       or
189300960102     C                             X20LScadu <> 0 or x20lascad <> 0 or
189400960102     C                             X20Risc <> 0 or X20Portaf <> 0
189500951113     C                   move      *on           $RCDok
189600951113     C                   else
189700951113     C                   move      *off          $RCDok
189800951113     C                   endif
189900951113     c* Aperte dare (testo solo moneta di conto)
190000951113     C* sono aperte in dare se dare <> da avere e dare > avere
190100960126     C                   When      D1sitpar   = '2'
190200951113     C                   if        WtotDare <> WtotAvere and
190300951113     C                             WtotDare >  WtotAvere
190400951113     C                   move      *on           $RCDok
190500951113     C                   else
190600951113     C                   move      *off          $RCDok
190700951113     C                   endif
190800951113     c* Aperte avere (testo solo moneta di conto)
190900951113     C* sono aperte in avere se dare <> da avere e dare < avere
191000960126     C                   When      d1SitPar   = '3'
191100951113     C                   if        WtotDare <> WtotAvere and
191200951113     C                             WtotDare <  WtotAvere
191300951113     C                   move      *on           $RCDok
191400951113     C                   else
191500951113     C                   move      *off          $RCDok
191600951113     C                   endif
191700951113     c* Aperte dare o avere (come rischiesta 0 senza però verifica rischio)
191800960126     C                   When      D1SitPar   = '4'
191900951113     C                   if        WtotDare <> WtotAvere or
192000951113     C                             WtotDareD <> WtotAvered
192100951113     C                   move      *on           $RCDok
192200951113     C                   else
192300951113     C                   move      *off          $RCDok
192400951113     C                   endif
192500951113     C*
192600951113     C                   endsl
192700951113     c*
192800951113     C                   ENDSR
192900940131     C/EJECT
193000940131     C************************************************************
193100940131     C* RIEMPIMENTO RIGA LISTA
193200940131     C************************************************************
193300940131     C     RIES1         BEGSR
193400940131     C*
193500960126     C                   CLEAR                   fmtS1
193600951109     C*
193700951109     C* Campi da NDppa
193800951109     C                   MOVE      PPASYS        H1SYS
193900951109     C                   MOVE      PPANRASINT    H1NRASINT
194000951109     C                   CALLb     'XDT4000'
194100951109     C                   PARM      PPADTPAR      XDTAMG
194200951109     C     S1DTPAR       PARM                    XDTGMA
194300951109     C                   PARM      2             XDTSTA
194400951109     C                   MOVE      PPANRPAR      S1NRPAR
194500951109     C                   MOVE      PPASERPAR     S1SERPAR
194600951113     C                   EVAL      S1UNITA = ppaunita
194700951109     c* divise fisse
194800960126     C                   EVAL      h1Divisa   = ppadivisa
194900960126     C                   EVAL      h1Monconto = xscdiv
195000960129     c* Dare/Avere
195100960129     C                   z-add     x20tdld       s1defdare
195200960129     C                   z-add     x20tdlp       s1prodare
195300960129     C                   z-add     x20tdlg       s1gesdare
195400960129     C                   z-add     x20tald       s1defavere
195500960129     C                   z-add     x20talp       s1proavere
195600960129     C                   z-add     x20talg       s1gesavere
195700951113     C*
195800951113     C* Campi da X20SCADR
195900951113     C* + calcolo totale saldo se lo deve fare
196000951109     C                   exsr      calcsaldo
196100951110     C*
196200951110     c* Campi variabili a seguito richiesta moneta di conto o divisa
196300951110     C                   SELECT
196400951110     C* divisa
196500951110     C                   when      SavDivVis = '1'
196600951110     C                   EVAL      S1DiVisa1  = ppadivisa
196700951110     C                   eval      s1saldo1   = H1saldod
196800951113     C                   eval      s1segno1   = H1segnod
196900951110     C                   EVAL      S1DiVisa2  = xscdiv
197000951110     C                   eval      s1saldo2   = H1saldom
197100951113     C                   eval      s1segno2   = H1segnom
197200951110     C* moneta di conto
197300951110     C                   when      SavDivVis = '0'
197400951110     C                   EVAL      S1Divisa1  = xscdiv
197500951110     C                   eval      s1saldo1   = H1saldom
197600951113     C                   eval      s1segno1   = H1segnom
197700951110     C                   EVAL      S1Divisa2  = ppadivisa
197800951110     C                   eval      s1saldo2   = H1saldod
197900951113     C                   eval      s1segno2   = H1segnod
198000951110     C                   endsl
198100951115     C*
198200951115     C* se le divise sono uguali, imposto il valore del campo in drop
198300951115     C* uguale a quello della prima riga, così almeno sono allineati
198400951115     C                   IF        ppadivisa = xscdiv
198500951115     C                   eval      s1saldo2 = s1saldo1
198600951115     C                   endif
198700960129     C* cerco agente
198800041130C1807c                   clear                   WrkBkAmm
198900960129     C                   MOVE      PPASYS        PPSSYS
199000960129     C                   MOVE      PPANRASINT    PPSNRASINT
199100960129    >C     K02PPS01      CHAIN     NDPPS000                           21
199200960129     c                   if        *IN21 = *off
199300960129     C                   eval      h1kscage = ppskscage
199400960201     C                   eval      h1boninc = ppsboninc
199500041130c1807c                   eval      WrkBkAmm = ppsbkamm
199600041130  "  c* Controllo se esistono delle rate bloccate solo se non esistono
199700041130  "  c* già dei blocchi su ndpps e solo per fornitori
199800041130  "  c                   if        wrkbkamm = *blank and snatura002='F'
199900041130  "  c                   exsr      CtrRateBl
200000041130c1807c                   endif
200100960129     C                   endif
200200041130C1807C                   eval      s1bkamm=wrkbkamm
200300960201     c*
200400960201     C* nel caso di cliente
200500960201     C* reperisco se solleciti presenti (deve essere presente nel file e non
200600960201     C* annullato). Questo controllo si fa solo se lo scadutoin M.c. è <> 0!
200700960201     C                   if        snatura002 = 'C'
200800960201     C                   eval      epacsi  = xscsoc
200900960201     C                   eval      epakcc  = c1kcc
201000960201     C                   eval      epaksc  = c1ksc
201100960201     C                   CALLb     'XDT4000'
201200960201     C     epadrp        PARM                    XDTAMG
201300960201     C                   PARM      s1dtpar       XDTGMA
201400960201     C                   PARM      4             XDTSTA
201500960201     C                   eval      epanrp  = s1nrpar
201600960201     C                   eval      epaserpar = s1serpar
201700960201     C     K06EPA01      CHAIN     NDEPA01L                           21
201800960201     C                   if        *in21 = *off and epaann = *blanks
201900960201     C                   eval      h1sollec   = *on
202000960201     C                   endif
202100960201     C                   endif
202200030122     c*
202300030122D1271C* nel caso di fornitore
202400030122  "  C* reperisco se ritenute  presenti (deve essere presente nel file e non
202500030122  "  C* annullato).
202600030122  "  C                   if        snatura002 = 'F'
202700030122  "  C                   eval      mrasocieta= xscsoc
202800030122  "  C                   eval      mrakcc  = c1kcc
202900030122  "  C                   eval      mraksc  = c1ksc
203000030122  "  C                   CALLb     'XDT4000'
203100030122  "  C     mradrp        PARM                    XDTAMG
203200030122  "  C                   PARM      s1dtpar       XDTGMA
203300030122  "  C                   PARM      4             XDTSTA
203400030122  "  C                   eval      mranrp  = s1nrpar
203500030122  "  C                   eval      mraserpar = s1serpar
203600030122  "  C     K06Mra04      CHAIN     NDMra04L                           21
203700030122  "  C                   if        *in21 = *off and mraflag= *blanks
203800030122  "  C                   eval      h1Ritenute = *on
203900030122  "  C                   endif
204000030122D1271C                   endif
204100041130     C*
204200041130c1807C* reperisco se commento presente
204300041130  "  C                   MOVE      *BLANK        EXI724
204400041130  "  C                   RESET                   A724DS
204500041130  "  C                   MOVEL     '01'          OPZ724
204600041130  "  C                   MOVEL     XscSOC        SOC724
204700041130  "  C                   MOVEL     *BLANK        UNI724
204800041130  "  C                   MOVEL     'PPA'         TPA724
204900041130  "  C                   MOVEL     d1ctb         KY1724
205000041130  "  C                   MOVEL     d1KCC         KY2724
205100041130  "  C                   MOVE      d1KSC         KY2724
205200041130  "  C                   MOVEL     S1DTPAR       KY3724
205300041130  "  C                   MOVEL     s1nrpar       KY4724
205400041130  "  C                   MOVE      s1serpar      KY4724
205500041130  "  C                   MOVEL     A724DS        KPJBU
205600041130  "  C                   CALL      'ANA724R'
205700041130  "  C                   PARM                    KPJBA
205800041130  "  C                   MOVEL     KPJBU         A724DS
205900041130  "  C* memorizzo lo stato del  bottone TCM
206000041130c1807C                   MOVE      EXI724        COMMENTIH
206100950301     C*
206200950301     C* imposto quegli attributi del sfl che sono modificabili
206300950301     C* al richiamo del determinato pgm
206400950301     C                   EXSR      ATRS1
206500940131     C*
206600940131     C                   ENDSR
206700951109     C/EJECT
206800041130c1807C************************************************************
206900041130  "  C* controllo se esistono delle rate bloccate
207000041130  "  C************************************************************
207100041130  "  C     CtrRateBl     BEGSR
207200041130  "  C*
207300041130  "  C     K02PpS01      Setll     Ndpsc01l
207400041130  "  c                   do        *hival
207500041130  "  C     K02PpS01      reade     Ndpsc01l                               24
207600041130  "  c   24              leave
207700041130  "  c                   if        pscbkamm <> *blanks
207800041130  "  c                   eval      wrkbkamm = '+'
207900041130  "  c                   leave
208000041130  "  c                   endif
208100041130  "  c                   enddo
208200041130  "  C*
208300041130C1807C                   ENDSR
208400041130     C/EJECT
208500951109     C************************************************************
208600951109     C* Calcolo saldo per partita e totale se lo deve fare
208700951109     C************************************************************
208800951109     C     CalcSaldo     BEGSR
208900951113     C*
209000951113     C* addiziono per totale CONTO se lo deve fare (è sempre in moneta
209100951113     C* di conto)
209200951113     C                   if        $carpag = *off
209300951113     C                   add       WtotDare      WsaldototD
209400951113     C                   add       WtotAvere     WsaldototA
209500951113     C                   endif
209600951113     C*
209700951113     C* Dati in MONETA DI CONTO
209800951109     C*
209900951110     C* Segno
210000951113     C                   exsr      RepSegno
210100951113     C                   eval      h1segnom  = Wsegno
210200951110     C* Importo
210300951109     C                   Z-add     Wsaldo        WImporto
210400971104     C                   Z-add     NrDec         WNrDec
210500951110     C                   Eval      $LenFld = %Size(h1SaldoM)
210600951110     C* allineo dare a sinistra
210700961009     C                   if        h1segnoM = XscDare
210800951110     C                   SUB       5             $lenfld
210900951110     C                   Endif
211000951109     C                   EXSR      Edit
211100951110     C                   Eval      h1Saldom = $RcvrVar
211200951109     C*
211300951113     C* Dati in DIVISA
211400951110     C* segno
211500951113     C                   exsr      RepSegnoD
211600951113     C                   eval      H1SegnoD  = Wsegno
211700951109     C*
211800951110     C* Importo in divisa
211900951110     C* reperisco decimali se divisa diversa dalla precedente letta
212000951113     C                   if        Ppadivisa <> Savdivisa
212100951113     C                   eval      SavDivisa = PPaDivisa
212200951113     C                   Eval      WDivisa =  PpaDivisa
212300951113     C                   Exsr      RepDivisa
212400951113     C                   Eval      NrDecD = X01NrDec
212500951113     C                   endif
212600951110     C* metto sempre 3 decimali in divisa
212700951113     C*******            eval      NRDecd = 3
212800951110     C*
212900951110     C* importo
213000951109     C                   EVAL      Wimporto = Wsaldo
213100951110     C                   Z-add     NrDecD        WNrDec
213200951110     C                   Eval      $LenFld = %Size(h1SaldoD)
213300951110     C*
213400951110     C* Imposto la dimensione del campo video in modo da allineare
213500951110     C* valute gestite con un numero diverso di decimali
213600951109     C                   Select
213700951110     C                   When      WNrDec = 2
213800951110     C                   Eval      $LenFld = %Size(h1SaldoD) - 1
213900951110     C                   When      WNrDec = 1
214000951110     C                   Eval      $LenFld = %Size(h1Saldod) - 2
214100951110     C                   When      WNrDec = 0
214200951110     C                   Eval      $LenFld = %Size(h1Saldod) - 4
214300951109     C                   Other
214400951110     C                   Eval      $LenFld = %Size(h1Saldod)
214500951109     C                   Endsl
214600951110     C* allineo dare a sinistra
214700961009     C                   if        H1SegnoD = XscDare
214800951110     C                   SUB       5             $lenfld
214900951110     C                   Endif
215000951109     C                   EXSR      Edit
215100951110     C                   Eval      h1Saldod  = $RcvrVar
215200951109     C*
215300951109     C                   ENDSR
215400950301     C/EJECT
215500950301     C************************************************************
215600950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
215700950301     C************************************************************
215800950301     C     ATRS1         BEGSR
215900950301     C*
216000950704    >C* impostare le condizioni per ripristinare gli attributi
216100950704    >C* della riga del sfl (se ce ne sono)
216200951113      * attiva o no unità
216300951113     C                   if        fgo1=*on
216400951113     C                   eval      *in11=*on
216500951113     C                   else
216600951113     C                   eval      *in11=*off
216700951113     C                   endif
216800041130     c*
216900041130c1807c* sottolinea la data partita se ci sono commenti
217000041130  "  C     COMMENTIH     IFEQ      *on
217100041130  "  C                   eval      *in16=*on
217200041130  "  C                   else
217300041130  "  C                   eval      *in16=*off
217400041130c1807C                   endif
217500950301     C*
217600950301     C                   ENDSR
217700941206     C/EJECT
217800941206     C************************************************************
217900941206     C* GESTIONE F08 VIDEO S1
218000941206     C************************************************************
218100941206     C     F08S1         BEGSR
218200941206     C*
218300941206     C                   MOVE      *ON           $FINE
218400960126    >C                   MOVE      '0'           RET110
218500941206     C*
218600941206     C                   ENDSR
218700940309     C/EJECT
218800940309     C************************************************************
218900940309     C* GESTIONE F03 VIDEO S1
219000940309     C************************************************************
219100940309     C     F03S1         BEGSR
219200940309     C*
219300940309     C                   MOVE      *ON           $FINE
219400960126    >C                   MOVE      '1'           RET110
219500940325     C* fine programma
219600940325     C                   EXSR      ENDPGM
219700940309     C*
219800940309     C                   ENDSR
219900951109     C/EJECT
220000951109     C************************************************************
220100951109     C* GESTIONE F11 VIDEO S1 - cambio moneta
220200951109     C************************************************************
220300951109     C     F11S1         BEGSR
220400951113     C*
220500951113     C* Eseguo solo se ci sono record nel sfl
220600951113     C                   IF        Wmax <> 0
220700951110     C* Scambio richiesta
220800951110     C                   select
220900951110     C                   when      Savdivvis = '0'
221000951110     C                   eval      Savdivvis = '1'
221100951110     C*
221200951110     C                   when      Savdivvis = '1'
221300951110     C                   eval      Savdivvis = '0'
221400951110     C                   endsl
221500951110     C*
221600951110     C                   DO        *hival        $X                4 0
221700960126     C     $X            chain     fmts1                              21
221800951110     C   21              LEAVE
221900951110     C                   select
222000951110     C                   when      Savdivvis = '0'
222100951110     C                   eval      s1saldo1 = h1saldom
222200960126     C                   EVAL      S1DiVisa1  = h1monconto
222300951110     C                   eval      s1saldo2 = h1saldod
222400960126     C                   EVAL      S1DiVisa2  = h1divisa
222500951110     c*
222600951110     C                   when      Savdivvis = '1'
222700951110     C                   eval      s1saldo1 = h1saldod
222800960126     C                   EVAL      S1DiVisa1  = h1divisa
222900951110     C                   eval      s1saldo2 = h1saldom
223000960126     C                   EVAL      S1DiVisa2  = h1monconto
223100951110     C                   endsl
223200951115     C*
223300951115     C* se le divise sono uguali, imposto il valore del campo in drop
223400951115     C* uguale a quello della prima riga, così almeno sono allineati
223500951115     C                   IF        s1divisa1 = s1divisa2
223600951115     C                   eval      s1saldo2 = s1saldo1
223700951115     C                   endif
223800960627     c* sflnxtchg
223900960626     c                   if        s1opz <> 0
224000960626     c                   seton                                        32
224100960627     c                   else
224200960627     c                   setoff                                       32
224300960626     c                   endif
224400951113     C* imposto quegli attributi del sfl che sono modificabili
224500951113     C* al richiamo del determinato pgm
224600951113     C                   EXSR      ATRS1
224700960627     c*
224800960627     C* Spengo indicatore di reverse image del campo opzione
224900960627     c                   setoff                                       40
225000960627     C*
225100960126     C                   UPDATE    fmtS1
225200951110     C*
225300951110     C                   ENDDO
225400951109     C*
225500951113     C                   ENDIF
225600951109     C                   ENDSR
225700940309     C/EJECT
225800940309     C************************************************************
225900940309     C* GESTIONE F12 VIDEO S1
226000940309     C************************************************************
226100940309     C     F12S1         BEGSR
226200940309     C*
226300960126     C*
226400960126     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
226500960126     C     $LOP          IFEQ      1
226600960126     C                   MOVE      'D1'          $GEST
226700960126     C*  se la 1a videata è questa, per F12 torno a menu
226800960126     C                   ELSE
226900960126     C                   MOVE      *ON           $FINE
227000960126    >C                   MOVE      '2'           RET110
227100960126     C                   ENDIF
227200940309     C*
227300940309     C                   ENDSR
227400951109     C/EJECT
227500951109     C************************************************************
227600951109     C* GESTIONE F15 VIDEO S1
227700951109     C************************************************************
227800951109     C     F15S1         BEGSR
227900951109     C*
228000951109     C                   reset                   ana399ds
228100951109     C                   eval      A39societa = XSCSOC
228200960126     C                   eval      A39unita = d1unita
228300960126     C                   eval      A39ctb = d1ctb
228400960126     C                   eval      A39kcc = d1kcc
228500960126     C                   eval      A39ksc = d1ksc
228600960126     C                   eval      A39SNATURA = snatura002
228700951109     C                   movel     ana399ds      kpjbu
228800951109     C                   CALL      'ANA399R'                            21
228900951109     C                   PARM                    KPJBA
229000951109     C     A39ERRORI     IFEQ      '0'
229100951109     C                   endif
229200951109     C*
229300951109     C                   ENDSR
229400960129     C/EJECT
229500960129     C************************************************************
229600960129     C* GESTIONE F17 VIDEO S1
229700960129     C************************************************************
229800960129     C     F17S1         BEGSR
229900960129     C*
230000960129     C                   MOVE      'D1'          $GEST
230100960129     C*
230200960129     C                   ENDSR
230300960129     C/EJECT
230400960129     C************************************************************
230500960129     C* GESTIONE F18 VIDEO S1
230600960129     C************************************************************
230700960129     C     F18S1         BEGSR
230800960129     C*
230900960129     C                   MOVE      'D2'          $GEST
231000960129     C                   MOVEL     *ON           $INZD2
231100960129     C*
231200960129     C                   ENDSR
231300940309     C/EJECT
231400940309     C************************************************************
231500940309     C* GESTIONE F23 VIDEO S1
231600940309     C************************************************************
231700940309     C     F23S1         BEGSR
231800940309     C*
231900941220    >C                   Z-ADD     76            $L01
232000941220    >C                   Z-ADD     76            $L02
232100940309     C                   EXSR      RIEOPZ
232200941214     C                   Z-ADD     23            X
232300951109     C                   MOVE      FF23          $FC1(X)
232400940608     C                   MOVEL     WOPZ1         C1OPZ1
232500060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
232600951109     C                   MOVEL     WOPZ2         C1OPZ2
232700940309     C                   Z-ADD     WCOZ          C1COZ
232800940309     C*
232900940309     C                   ENDSR
233000940309     C/EJECT
233100940309     C************************************************************
233200940309     C* GESTIONE F24 VIDEO S1
233300940309     C************************************************************
233400940309     C     F24S1         BEGSR
233500940506     C*
233600951109     C                   MOVEA     $FX1          $FX
233700951109     C                   MOVEA     $FC1          $FC
233800951109     C                   MOVEA     $FR1          $FR
233900940506     C                   Z-ADD     $ULKS1        $ULK
234000960126     C                   Z-ADD     $S1L01        $L01
234100960126     C                   Z-ADD     $S1L02        $L02
234200940506     C                   EXSR      RIEKEY
234300940506     C                   Z-ADD     $ULK          $ULKS1            3 0
234400951109     C                   MOVEA     $FC           $FC1
234500951109     C                   MOVEL     $KEY1         Z1KE1
234600060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
234700951109     C                   MOVEL     $KEY2         Z1KE2
234800960126     C                   WRITE     FMTZ1
234900940506     C*
235000940309     C                   ENDSR
235100940128     C/EJECT
235200940128     C************************************************************
235300940131     C* CONTROLLO TESTATA LISTA
235400940128     C************************************************************
235500940131     C     CTRC1         BEGSR
235600940128     C*
235700940201     C                   MOVE      *OFF          *IN99
235800951109     C*
235900951109     C                   if        c1pos <> 0
236000951109     C                   CALLb     'XDT4000'
236100951109     C                   PARM                    XDTAMG           10
236200951109     C                   PARM      c1pos         XDTGMA            6 0
236300951109     C                   PARM      3             XDTSTA            1 0
236400951109     C     XDTSTA        IFLT      0
236500951109     C                   SETON                                        6099
236600951109     C                   endif
236700951109     C                   endif
236800940131     C*
236900940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
237000950707     C* L'INIZIALIZZAZIONE LISTA   (lo attivo anche se w1pos = c1pos, per ri
237100950707     C* partire dal primo numero partita
237200951109     C                   IF        *in99 = *off
237300950707     C     C1POS         IFNE      W1POS
237400950704    >C* aggiungere in OR altri campi posizionamento se ci sono
237500940202     C                   EXSR      POSS1
237600950707     C                   ENDIF
237700951109     C                   Endif
237800940202     C*
237900940202     C                   ENDSR
238000940202     C/EJECT
238100940202     C************************************************************
238200940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
238300940202     C************************************************************
238400940202     C     POSS1         BEGSR
238500940202     C*
238600940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
238700940202     C*
238800941118   B1C     WMAX          IFGT      0
238900960126     C                   READC     fmtS1                                  21
239000941118   B2C     *IN21         DOWEQ     *OFF
239100940202     C     *IN99         ANDEQ     *OFF
239200940202     C*
239300941118   B3C     S1OPZ         IFNE      0
239400940202     C                   SETON                                        51  99
239500941118   E3C                   ENDIF
239600940202     C* se rilevata opzione attiva, attivo sflnxtchg
239700941118   B3C     *IN99         IFEQ      *ON
239800940202     C                   MOVE      *ON           *IN32
239900950301     C*
240000950301     C* imposto quegli attributi del sfl che sono modificabili
240100950301     C* al richiamo del determinato pgm
240200941118     C                   EXSR      ATRS1
240300941118     C*
240400960126     C                   UPDATE    FMTS1
240500941118   X3C                   ELSE
240600960126     C                   READC     FMTS1                                  21
240700941118   E3C                   ENDIF
240800940202     C*
240900941118   E2C                   ENDDO
241000940202     C*
241100941118   E1C                   ENDIF
241200940202     C*
241300940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
241400940221     C* AL VALORE DI POSIZIONAMENTO :
241500940221     C* se questo è maggiore del valore del rispettivo campo di
241600940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
241700940221     C* con i valori di parzializzazione.
241800940221     C*
241900941118   B1C     *IN99         IFEQ      *OFF
242000950704    >C                   MOVE      *ZEROS        W1POS
242100940202     C*
242200950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
242300950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
242400950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
242500950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
242600950525     C* verificata la condizione suddetta avverrà con posizionamento a
242700950525     C* partire dal valore di parzializzazione
242800950525     C*
242900941118   B2C                   SELECT
243000950704    >C* Attenzione : se un ordinamento ha un campo di posizionamento
243100950704    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
243200950704    >C* dei relativi campi
243300940202     C*
243400940202     C     $ORD          WHENEQ    1
243500950406     C                   MOVEL     C1POS         W1POS
243600950525     C*
243700941118   E2C                   ENDSL
243800950525     C*
243900950525     C* richiedo l'inizializzazione sfl
244000950525     C                   MOVE      *ON           *IN99
244100950525     C                   MOVE      *ON           $INZS1
244200940202     C*
244300941118   E1C                   ENDIF
244400940202     C*
244500940131     C                   ENDSR
244600940131     C/EJECT
244700940131     C************************************************************
244800940131     C* CONTROLLO OPZIONI LISTA
244900940131     C************************************************************
245000940131     C     CTRS1         BEGSR
245100940131     C*
245200940202     C                   MOVEL     *OFF          $ESCI
245300940201     C                   SETOFF                                       99
245400940131     C                   Z-ADD     0             S1OPZ
245500940131     C*
245600940127     C* Leggo il sfl solo se ci sono rcd
245700941118   B1C     WMAX          IFGT      0
245800960126     C                   READC     FMTS1                                  21
245900940127     C*
246000940131     C* esce se fine sfl o errore che richiede l'uscita
246100941118   B2C     *IN21         DOWEQ     *OFF
246200940131     C     $ESCI         ANDEQ     *OFF
246300940201     C                   Z-ADD     S1NRR         C1RCD
246400940131     C* ctrl su riga
246500940131     C                   EXSR      RECS1
246600940131     C* gestione opzioni
246700941118   B3C     S1OPZ         IFNE      0
246800940201     C     *IN99         ANDEQ     *OFF
246900940131     C                   EXSR      OPZS1
247000941118   E3C                   ENDIF
247100940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
247200941118   B3C     *IN99         IFEQ      *ON
247300940131     C                   MOVE      *ON           *IN32
247400941118   E3C                   ENDIF
247500950301     C*
247600950301     C* imposto quegli attributi del sfl che sono modificabili
247700950301     C* al richiamo del determinato pgm
247800941118     C                   EXSR      ATRS1
247900960129     c*
248000960129     C* Non pulisco opzioni taglia e incolla, perchè vengono trattate dopo
248100960129     c                   if        s1opz <> 50 AND S1OPZ <> 51
248200940131     C                   Z-ADD     0             S1OPZ
248300960129     C                   endif
248400940223     C*
248500960126     C                   UPDATE    FMTS1
248600941125     C                   MOVE      *OFF          *IN32
248700940223     C*
248800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
248900941118   B3C     $ESCI         IFEQ      *OFF
249000960126     C                   READC     FMTS1                                  21
249100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
249200941118   B4C     *IN21         IFEQ      *ON
249300940201     C                   MOVE      WINZS1        $INZS1
249400940204     C* calcolo pagina a cui deve posizionarsi
249500940728     C     WSFL          IFGT      0
249600940728     C                   EXSR      CLCPAG
249700940728     C                   ELSE
249800940728     C                   Z-ADD     SFLPAG        WPAG
249900940728     C                   ENDIF
250000941118   E4C                   ENDIF
250100941118   E4C                   ENDIF
250200940131     C*
250300941118   E2C                   ENDDO
250400940127     C*
250500941118   E1C                   ENDIF
250600940131     C*
250700940127     C                   ENDSR
250800960130     C/EJECT
250900041130     C************************************************************
251000041130     C* CONTROLLI PER opzioni taglia e  incolla
251100041130     c* Non ci devono essere 2 incolla
251200041130     C************************************************************
251300041130     C     CTROPZ        BEGSR
251400041130     C*
251500041130     C                   SETOFF                                       99
251600041130D1271c                   move      *off          wRitenute
251700041130     c                   clear                   wincolla
251800041130     c                   clear                   wtaglia
251900041130     c                   clear                   wdivisa
252000041130     c                   clear                   wunita
252100041130     c                   clear                   wkscage
252200041130     c                   clear                   wdtpar
252300041130     c                   clear                   wnrpar
252400041130     c                   clear                   wserpar
252500041130     c                   eval      wdivisaok = *on
252600041130d1271c                   eval      writenutok = *on
252700041130     C*
252800041130   B1C     WMAX          IFGT      0
252900041130     C*
253000041130     c                   do        *hival        $X                4 0
253100041130     C     $x            chain     FMTS1                              21
253200041130     c   21              leave
253300041130     C*
253400041130   B3C     S1OPZ         IFEQ      51
253500041130     C* memorizzo estremi partita 'incolla'
253600041130     C                   eval      wdtpar = s1dtpar
253700041130     C                   eval      wnrpar = s1nrpar
253800041130     C                   eval      wserpar= s1serpar
253900041130     C                   add       1             wincolla
254000041130   E3C                   ENDIF
254100041130     c*
254200041130   B3C     S1OPZ         IFEQ      50
254300041130     C                   add       1             wtaglia
254400041130   E3C                   ENDIF
254500041130     c*
254600041130     c                   if        s1opz = 50 or s1opz = 51
254700041130     C*
254800041130D1271c* memorizzo se presenti ritenute per la prima partita che trovo
254900041130  "  C                   if        wRitenute=*off
255000041130  "  C                   eval      writenute=h1ritenute
255100041130D1271c                   endif
255200041130     C*
255300041130     c* memorizzo prima divisa che trovo
255400041130     C                   if        wdivisa = *blanks
255500041130     C                   eval      wdivisa = h1divisa
255600041130     c                   endif
255700041130     c*
255800041130     c* memorizzo prima unità  che trovo
255900041130     C                   if        wunita  = *blanks and s1unita <> *blanks
256000041130     C                   eval      wunita  = s1unita
256100041130     c                   endif
256200041130     c*
256300041130     c* memorizzo primo agente che trovo
256400041130     C                   if        wkscage = *blanks and h1kscage <> *blanks
256500041130     C                   eval      wkscage = h1kscage
256600041130     c                   endif
256700041130     c*
256800041130     c* se divisa diversa da quella memorizzata, accendo flag x errore
256900041130     C* ed esco subito perchè errore bloccante
257000041130     c                   if        h1divisa <> wdivisa
257100041130     c                   eval      wdivisaok = *off
257200041130     c                   z-add     s1nrr         c1rcd
257300041130     c                   leave
257400041130     c                   endif
257500041130     c*
257600041130D1271c* se presente ritenuta anche sulla seconda partita, accendo flag
257700041130  "  C* ed esco subito perchè errore bloccante
257800041130  "  c                   if        h1ritenute= *on and writenute=*on
257900041130  "  c                   eval      writenutOk=*off
258000041130  "  c                   z-add     s1nrr         c1rcd
258100041130  "  c                   leave
258200041130D1271c                   endif
258300041130     C* Imposto ultima opzione letta
258400041130     c                   z-add     s1nrr         c1rcd
258500041130     c                   endif
258600041130     c*
258700041130     C                   MOVE      *ON           *IN32
258800041130     C* imposto quegli attributi del sfl che sono modificabili
258900041130     C* al richiamo del determinato pgm
259000041130     C                   EXSR      ATRS1
259100041130     C                   UPDATE    FMTS1
259200041130     C                   MOVE      *Off          *IN32
259300041130     c*
259400041130     C                   ENDDO
259500041130     C*
259600041130     C* ERRORI BLOCCANTI
259700041130     C* divisa
259800041130     C                   if        wdivisaOK = *off
259900041130     C                   seton                                        9952
260000041130     C                   endif
260100041130d1271C* ritenute
260200041130  "  C                   if        writenutok= *off
260300041130  "  C                   seton                                        9962
260400041130d1271C                   endif
260500041130     c*
260600041130     C* se rilevate + d1i un incolla o nessun taglia, errore
260700041130     c* questo controllo si può fare solo se letti tutti i record del
260800041130     c* SFL, quindi a indicatore 99 spento
260900041130     C                   IF        *in99 = *off
261000041130     C                   if        wincolla <> 1 or wtaglia = 0
261100041130     C                   seton                                        9959
261200041130     C                   endif
261300041130     C                   endif
261400041130     C*
261500041130   E1C                   ENDIF
261600041130     C*
261700041130     C                   ENDSR
261800041130     C/EJECT
261900960130     C************************************************************
262000960130     c* Segnalazione errori non bloccanti
262100960130     C************************************************************
262200960130     C     Segnalaz      BEGSR
262300960130     C*
262400960131     c                   clear                   msgrtn
262500960131     C*
262600960131     C                   clear                   msgms1
262700971013     C                   eval      IDMSG = 'PROMSG    *LIBL     COS0310'
262800971013     C                   CALLB     'XRTVM'
262900971013     C                   PARM                    IDMSG            27
263000971013     C                   PARM                    TXTMSG          100
263100971013     C                   PARM                    ERRMSG            1
263200971013     C                   PARM      *OFF          CTRMSG            1            centratura
263300971013     C                   PARM      70            LENMSG            3 0          lun output.
263400971013     C     ERRMSG        IFNE      *ON
263500971013     C                   MOVEL     TXTMSG        MSGMS1
263600971013     C                   ELSE
263700971013     C                   MOVEL     *ALL'?'       MSGMS1
263800971013     C                   ENDIF
263900960131     C                   MOVE      s1dtpar       dsdtpar
264000960131     C                   MOVE      s1nrpar       dsnrpar
264100960131     C                   MOVE      s1serpar      dsserpar
264200960131     c     msgms1        cat       dspartita:1   msgms1
264300960131     C*
264400960130     C                   DO
264500960130     C*
264600960130     C* Unità
264700970214     c                   IF        s1unita <> Wunita and s1unita <> *blanks
264800960130     C                   eval      IDMSG = 'PROMSG    *LIBL     NDC0149'
264900960130     c                   exsr      msgpgm
265000960130     c                   if        msgrtn = '2'
265100960130     C                   leave
265200960130     c                   endif
265300960130     C                   ENDIF
265400960130     C*
265500960130     C* Agente
265600970214     c                   IF        h1kscage <> Wkscage and h1kscage <> *blanks
265700960130     C                   eval      IDMSG = 'PROMSG    *LIBL     NDC0150'
265800960130     c                   exsr      msgpgm
265900960130     c                   if        msgrtn = '2'
266000960130     C                   leave
266100960130     c                   endif
266200960130     C                   ENDIF
266300960201     C*
266400960201     C* Bonifici in corso se fornitore
266500960201     c                   IF        snatura002 = 'F' and h1boninc = *on
266600960201     C                   eval      IDMSG = 'PROMSG    *LIBL     NDC0151'
266700960201     c                   exsr      msgpgm
266800960201     c                   if        msgrtn = '2'
266900960201     C                   leave
267000960201     c                   endif
267100960201     C                   ENDIF
267200960201     C*
267300960201     C* Solleciti presenti se cliente
267400960201     c                   IF        snatura002 = 'C' and h1sollec = *on
267500960201     C                   eval      IDMSG = 'PROMSG    *LIBL     NDC0152'
267600960201     c                   exsr      msgpgm
267700960201     c                   if        msgrtn = '2'
267800960201     C                   leave
267900960201     c                   endif
268000960201     C                   ENDIF
268100041201     C*
268200041201c1807C* Presente blocco partita
268300041201  "  c                   IF        s1bkamm <> *blanks
268400041201  "  C                   eval      IDMSG = 'PROMSG    *LIBL     NDC0403'
268500041201  "  c                   exsr      msgpgm
268600041201  "  c                   if        msgrtn = '2'
268700041201  "  C                   leave
268800041201  "  c                   endif
268900041201c1807C                   ENDIF
269000960130     C*
269100960130     c                   enddo
269200960130     C*
269300960130     C                   ENDSR
269400960130     C/EJECT
269500960130     C************************************************************
269600960130     C* Reperimento testo messaggi per errori non bloccanti
269700960130     C************************************************************
269800960130     C     PrendiMsg     BEGSR
269900960130     C*
270000960130     C                   CALLB     'XRTVM'
270100960130     C                   PARM                    IDMSG            27
270200960130     C                   PARM                    TXTMSG          100
270300960130     C                   PARM                    ERRMSG            1
270400960130     C                   PARM      *ON           CTRMSG            1            centratura
270500960130     C                   PARM      70            LENMSG            3 0          lun output.
270600960130     c*
270700960130     C     ERRMSG        IFNE      *ON
270800960131     C                   MOVEL     TXTMSG        MSGMS2
270900960130     C                   ELSE
271000971013     C                   MOVEL     *ALL'?'       MSGMS2
271100960130     C                   ENDIF
271200960130     C*
271300960130     C                   ENDSR
271400960130     C/EJECT
271500960130     C******************************************************
271600960130     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
271700960130     C******************************************************
271800960130     C     MSGPGM        BEGSR
271900960130     C*
272000971008     C* Txt messagio
272100971008     C                   exsr      prendimsg
272200971008     C* Txt bottoni risposta
272300971008     C                   MOVEL     TxtBott       IDMSG
272400971008     C                   CALLB     'XRTVM'
272500971008     C                   PARM                    IDMSG            27
272600971008     C                   PARM                    TXTMSG          100
272700971008     C                   PARM                    ERRMSG            1
272800971008     C                   PARM      *OFF          CTRMSG            1            centratura
272900971008     C                   PARM      135           LENMSG            3 0          lun output.
273000971008     C     ERRMSG        IFNE      *ON
273100971008     C                   MOVEL     TXTMSG        XMStxt
273200971008     C                   ELSE
273300971008     C                   MOVEL     *ALL'?'       XMStxt
273400971008     C                   ENDIF
273500960130     C*
273600960130     C                   Z-ADD     0             MSGMSG
273700960130     C                   MOVEL     'S'           MSGRSP
273800960130     C                   MOVEL     '12'          MSGTPR
273900960130     C                   MOVEL     'S'           MSGEMV
274000960130     C                   Z-ADD     10            MSGRGP
274100960130     C                   MOVEL     'N'           MSGLCK
274200960130     C                   MOVEL     'N'           MSGCNL
274300960130     C                   MOVEL     'N'           MSGVID
274400960130     C                   MOVEL     'N'           MSGOPE
274500960130     C                   MOVEL     'N'           MSGSTP
274600960130     C                   CALL      'XMSGR'
274700960130     C                   PARM                    XMSGDS
274800960130     C                   PARM                    XMStxt          135
274900960130     C                   PARM      wxmsginz      XMSGinz           1
275000960130     C*
275100960130     c                   move      *off          wxmsginz
275200960130     C                   ENDSR
275300960129     C/EJECT
275400990806C1212 ************************************************************
275500990806  "   * Controllo autorizzazione unita e alla riconciliazione
275600990806  "   ************************************************************
275700990806  "  C     CTRAutUni     BEGSR
275800990806  "   *
275900990811  "   *Controllo l'abilitazione all'unità (solo) (-ErrmsgId relativo)
276000990811  "  C                   EXSR      AutUni_Uni
276100990811  "   *
276200990811  "   *Se esiste l'autorizzazione a tutte le unità
276300990811  "  C                   If        *In99       = *Off
276400990811  "   *
276500990811  "   *  Ctr che le par. da riconc. appartengono ad una sola unit
276600990811  "   *  (se non ci sono partite con unità diverse non controllo
276700990811  "   *  l'autorizzazione alla riconciliazione con altre unità)
276800990811  "  C                   EXSR      AutUni_Una
276900990811  "   *
277000990811  "   *  Ctr autorizzazione all'unità generale: sui tipi
277100990811  "   *  registrazione e sulla riconciliazione con altre unità
277200990811  "  C                   EXSR      AutUni_Gen
277300990811  "   *
277400990811  "  C                   EndIf
277500990811  "   *
277600990811  "  C     XCtrAutUni    ENDSR
277700990811  "   ************************************************************
277800990811  "   *Controllo l'abilitazione all'unità (solo) (-ErrmsgId relativo)
277900990811  "   ************************************************************
278000990811  "  C     AutUni_Uni    BEGSR
278100990811  "   *
278200990811  "  C     WMAX          IFGT      0
278300990811  "   *
278400990811  "  C                   DO        *Hival        $X                4 0
278500990811  "  C     $x            Chain     FMTS1                              21
278600990811  "   *
278700990811  "  C                   If        *In21 = *On  OR  *In99 = *On
278800990811  "  C                   Leave
278900990811  "  C                   EndIf
279000990811  "   *
279100990811  "   *Gestione errori bloccanti con segnalazione
279200990811  "  C     S1OPZ         IFEQ      50
279300990811  "  C     S1OPZ         oreq      51
279400990811  "   *
279500990811  "  C                   RESET                   XAutUniDS
279600990811  "  C                   EVAL      XAUPrf = KNmUs
279700990811  "  C                   EVAL      XAUSoc = XscSoc
279800990811  "  C                   EVAL      XAUUni = S1Unita
279900990811  "  C                   CALL      'XAUTUNI'
280000990811  "  C                   PARM                    XAutUniDS
280100990811  "   *
280200990811  "   * Non abilitato all'unità
280300990811  "  C                   IF        XAUAbl = *Off
280400990811  "  C                   SetOn                                        406199
280500990811  "  C                   ENDIF
280600990811  "   *
280700990811  "   * attributi sfl
280800990811  "  C                   MOVE      *ON           *IN32
280900990811  "  C                   EXSR      ATRS1
281000990811  "  C                   UPDATE    FMTS1
281100990811  "  C                   MOVE      *Off          *IN32
281200990811  "   *
281300990811  "  C                   ENDIf
281400990811  "   *
281500990811  "  C                   ENDDO
281600990811  "   *
281700990811  "  C                   ENDIf
281800990811  "   *
281900990811  "  C     XAutUni_Ui    ENDSR
282000990811  "   ************************************************************
282100990811  "   *Controllo se le partite da riconc. appartengono ad una sola unit
282200990811  "   *(se non ci sono partite con unità diverse
282300990811  "   * non controllo il flag di riconciliazione con altre unità
282400990811  "   * sull'autorizzazione all'unità)
282500990811  "   ************************************************************
282600990811  "  C     AutUni_Una    BEGSR
282700990811  "   *
282800990811  "  C                   Clear                   $Unita
282900990811  "   *se = '1' -> le par. da riconciliare hanno unità uguale
283000990811  "  C                   Eval      $UnaUni = *On
283100990811  "   *
283200990811  "  C     WMAX          IFGT      0
283300990811  "   *
283400990811  "  C                   DO        *Hival        $X                4 0
283500990811  "  C     $x            Chain     FMTS1                              21
283600990811  "   *
283700990811  "  C                   If        *In21 = *On  OR  $UnaUni = *Off
283800990811  "  C                   Leave
283900990811  "  C                   EndIf
284000990811  "   *
284100990811  "   *Gestione errori bloccanti con segnalazione tramite xmsg
284200990811  "  C     S1OPZ         IFEQ      50
284300990811  "  C     S1OPZ         oreq      51
284400990811  "   *
284500990811  "   * se è la I° par che incontro, memorizzo la sua unità
284600990811  "  C                   If        $Unita      = *Blank
284700990811  "  C                   Eval      $Unita      = S1Unita
284800990811  "  C                   EndIf
284900990811  "   *
285000990811  "   * se l'unità della par che sto leggendo <> dall'unità memorizzata
285100990811  "  C                   If        $Unita     <> S1Unita
285200990811  "   *   non esiste una unica unita tra le par. da riconciliare
285300990811  "  C                   Eval      $UnaUni     = *Off
285400990811  "  C                   EndIf
285500990811  "   *
285600990811  "   * attributi sfl
285700990811  "  C                   MOVE      *ON           *IN32
285800990811  "  C                   EXSR      ATRS1
285900990811  "  C                   UPDATE    FMTS1
286000990811  "  C                   MOVE      *Off          *IN32
286100990811  "   *
286200990811  "  C                   ENDIf
286300990811  "   *
286400990811  "  C                   ENDDO
286500990811  "   *
286600990811  "  C                   ENDIf
286700990811  "   *
286800990811  "  C     XAutUni_Ua    ENDSR
286900990811  "   ************************************************************
287000990811  "   *  Ctr autorizzazione all'unità generale: sui tipi
287100990811  "   *  registrazione e sulla riconciliazione con altre unità
287200990811  "   ************************************************************
287300990811  "  C     AutUni_Gen    BEGSR
287400990811  "   *
287500990806  "  C     WMAX          IFGT      0
287600990806  "   *
287700990806  "  C                   DO        *Hival        $X                4 0
287800990806  "  C     $x            Chain     FMTS1                              21
287900990806  "   *
288000990806  "  C                   If        *In21 = *On  OR  *In99 = *On
288100990806  "  C                   Leave
288200990806  "  C                   EndIf
288300990806  "   *
288400990806  "   *Gestione errori bloccanti con segnalazione tramite xmsg
288500990806  "  C     S1OPZ         IFEQ      50
288600990806  "  C     S1OPZ         oreq      51
288700990806  "   *
288800990806  "  C                   RESET                   XAutUniDS
288900990806  "  C                   RESET                   XAutUniDS1
289000990806  "  C                   EVAL      XAUPrf = KNmUs
289100990806  "  C                   EVAL      XAUSoc = XscSoc
289200990806  "  C                   EVAL      XAUUni = S1Unita
289300990806  "  C                   CALL      'XAUTUNI'
289400990806  "  C                   PARM                    XAutUniDS
289500990806  "  C                   PARM                    XAutUniDS1
289600990806  "   *
289700990806  "   * Abilitato all'unità
289800990806  "  C                   IF        XAUAbl = *On
289900990806  "   *
290000990806  "  C                   SELECT
290100990806  "   *
290200990811  "   *   se non abilitato alla riconciliaz. con altre unita
290300990811  "   *   e  esistono unita diverse
290400990811  "  C                   WHEN      XAURico = *Off  AND
290500990811  "  C                             $UnaUni = *Off
290600990806  "   *     emetto window con msg relativo:
290700990806  "   *     'Utente non abilitato alla riconciliazione con altre unità'
290800990806  "  C                   Eval      $IDMSG = 'PROMSG    *LIBL     COS0510'
290900990806  "  C                   Eval      $TpReg  = *Blank
291000990806  "  C                   EXSR      AutUni_Win
291100990806  "  C                   Eval      *in99   = *On
291200990806  "   *
291300990810  "   *   Solo per la partita da 'Tagliare' (50):
291400990811  "   *   e se  abilitato riconc. con altre unità in presenza di + unit
291500990811  "   *   o non abilitat. a riconc. con altre un. in presenza di 1 unit
291600990810  "  C                   WHEN      S1Opz   = 50      And
291700990811  "  C                             ((XAURico=*On  And  $UnaUni=*Off) OR
291800990811  "  C                             (XAURico=*Off And  $UnaUni=*On))
291900990811  "   *
292000990811  "  C                   Select
292100990811  "   *     non abilitato al tipo registrazione definitiva
292200990811  "  C                   When      XAUDef = *Off And
292300990811  "  C                             (S1DefDare<>0 Or S1DefAvere<>0)
292400990811  "   *       emetto window con msg relativo:
292500990811  "   *       'Utente non abilitato al tipo di registrazione &1'
292600990806  "  C                   Eval      $IDMSG = 'PROMSG    *LIBL     COS0511'
292700990806  "  C                   Eval      $TpReg  = 'D'
292800990806  "  C                   EXSR      AutUni_Win
292900990806  "  C                   Eval      *in99   = *On
293000990806  "   *
293100990811  "   *     non al tipo registrazione provvisoria
293200990811  "  C                   When      XAUProv = *Off And
293300990811  "  C                             (S1ProDare<>0 Or S1ProAvere<>0)
293400990811  "   *       emetto window con msg relativo:
293500990811  "   *       'Utente non abilitato al tipo di registrazione &1'
293600990806  "  C                   Eval      $IDMSG = 'PROMSG    *LIBL     COS0511'
293700990806  "  C                   Eval      $TpReg  = 'P'
293800990806  "  C                   EXSR      AutUni_Win
293900990806  "  C                   Eval      *in99   = *On
294000990806  "   *
294100990811  "   *     non al tipo registrazione gestionale
294200990811  "  C                   When      XAUGest = *Off And
294300990811  "  C                             (S1GesDare<>0 Or S1GesAvere<>0)
294400990811  "   *       emetto window con msg relativo:
294500990811  "   *       'Utente non abilitato al tipo di registrazione &1'
294600990806  "  C                   Eval      $IDMSG = 'PROMSG    *LIBL     COS0511'
294700990806  "  C                   Eval      $TpReg  = 'G'
294800990806  "  C                   EXSR      AutUni_Win
294900990806  "  C                   Eval      *in99   = *On
295000990811  "   *
295100990811  "  C                   EndSl
295200990806  "   *
295300990806  "  C                   EndSl
295400990806  "   *
295500990806  "  C                   ENDIF
295600990806  "   *
295700990806  "   * attributi sfl
295800990806  "  C                   MOVE      *ON           *IN32
295900990806  "  C                   EXSR      ATRS1
296000990806  "  C                   UPDATE    FMTS1
296100990806  "  C                   MOVE      *Off          *IN32
296200990806  "   *
296300990806  "  C                   ENDIf
296400990806  "   *
296500990806  "  C                   ENDDO
296600990806  "   *
296700990806  "  C                   ENDIf
296800990806  "   *
296900990811  "  C     XAutUni_Ge    ENDSR
297000990811  "   ************************************************************
297100990811  "   * Window con msg relativi alla non abilitazione all'unita
297200990811  "   ************************************************************
297300990806  "  C     AutUni_Win    BEGSR
297400990806  "   *
297500990806  "  C                   CLEAR                   XMSGDS
297600990806  "  C                   CLEAR                   XMSGTXT
297700990806  "   *
297800990806  "   * se il tipo registrazione è valorizzato, significa che
297900990806  "   * devo reperire il msg relativo alla inabilitazione al tipo
298000990806  "   * di reg. altrimenti alla riconciliazione con altre partite
298100990806  "  C                   Eval      IDMSG       = $IdMsg
298200990806  "  C                   If        $TpReg     <> *Blank
298300990806  "  C                   CALLB     'XRTVM'
298400990806  "  C                   PARM                    IDMSG            27
298500990806  "  C                   PARM      *Blank        TXTMSG          100
298600990806  "  C                   PARM                    ERRMSG            1
298700990806  "  C                   PARM                    *omit
298800990806  "  C                   PARM                    *omit
298900990806  "  C                   PARM      $TpReg        Msgdta          100
299000990806  "  C                   Else
299100990806  "  C                   CALLB     'XRTVM'
299200990806  "  C                   PARM                    IDMSG
299300990806  "  C                   PARM      *Blank        TXTMSG
299400990806  "  C                   PARM                    ERRMSG
299500990806  "  C                   EndIf
299600990806  "  C     ERRMSG        IFNE      *ON
299700990806  "  C                   MOVEL     TXTMSG        MSGMS1           70
299800990806  "  C                   ELSE
299900990806  "  C                   MOVEL     *ALL'?'       MSGMS1
300000990806  "  C                   ENDIF
300100990806  "   *
300200990806  "   * 'per l'unità &1 della partita &2 &3 &4'
300300990806  "   *      rif. par. alla quale il msg si riferisce:
300400990806  "  C                   RESET                   AutUniWDS
300500990806  "  C                   MOVEL     S1Unita       Unita
300600990806  "  C                   Z-ADD     S1NrPar       NrPar
300700990806  "  C                   MOVEL     S1SerPar      SerPar
300800990806  "  C                   MOVEL     S1DtPar       DataDS
300900990806  "  C                   MOVEL     Data01        DtPar1
301000990806  "  C                   MOVEL     Data02        DtPar2
301100990806  "  C                   MOVEL     Data03        DtPar3
301200990806  "  C                   Eval      IDMSG = 'PROMSG    *LIBL     COS0509'
301300990806  "  C                   CALLB     'XRTVM'
301400990806  "  C                   PARM                    IDMSG            27
301500990806  "  C                   PARM      *Blank        TXTMSG          100
301600990806  "  C                   PARM                    ERRMSG            1
301700990806  "  C                   PARM                    *omit
301800990806  "  C                   PARM                    *omit
301900990806  "  C                   PARM      AutUniWDS     Msgdta          100
302000990806  "  C     ERRMSG        IFNE      *ON
302100990806  "  C                   MOVEL     TXTMSG        MSGMS2           70
302200990806  "  C                   ELSE
302300990806  "  C                   MOVEL     *ALL'?'       MSGMS2
302400990806  "  C                   ENDIF
302500990806  "   *
302600990806  "  C                   MOVEL     'S'           MSGEMV
302700990806  "  C                   Z-ADD     10            MSGRGP
302800990806  "  C                   MOVEL     'N'           MSGRSP
302900990806  "   *Modo di inizializzazione della window:
303000990806  "   *('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
303100990806  "   *('0'->rimane aperta -->tutte le emissioni successive se consecutive,
303200990806  "   *                       per mantenere il sottofondo del video princip.)
303300990806  "  C                   Eval      XMSGINZ     = WXMsgInz
303400990806  "  C                   If        WXMsgInz    = *On
303500990806  "  C                   Eval      WXMsgInz    = *Off
303600990806  "  C                   EndIf
303700990806  "   *
303800990806  "  C                   CALL      'XMSGR'
303900990806  "  C                   PARM                    XMSGDS
304000990806  "  C                   PARM                    XMSGTXT         135
304100990806  "  C                   PARM                    XMSGINZ           1
304200990806  "   *
304300990806  "  C     XAutUni_Wi    ENDSR
304400990806C1212 *
304500960129     C************************************************************
304600960129     c* Riconciliazione partite
304700960129     C************************************************************
304800960129     C     Riconcilia    BEGSR
304900960129     C*
305000960201   B1C     WMAX          IFGT      0
305100960201     C*
305200960201     C                   do        *hival        $X                4 0
305300960201     C     $x            chain     FMTS1                              21
305400960201     C   21              leave
305500960202     c*
305600960202     c* gestione errori non bloccanti con segnalazione tramite xmsg
305700960202   B3C     S1OPZ         IFEQ      50
305800960202   B3C     S1OPZ         oreq      51
305900960202     c                   exsr      Segnalaz
306000960202     C* se risposta = '2' significa lasciare
306100960202     c                   if        msgrtn = '2'
306200960202     c                   eval      $inzs1 = *on
306300960202     c                   ROLBK
306400960202     C                   leave
306500960202     c                   endif
306600960202     c                   endif
306700960201     C*
306800960201     C* Per ogni partita da tagliare chiamo pgm riconciliazione
306900960201   B3C     S1OPZ         IFEQ      50
307000960201     C                   Reset                   ndc113ds
307100960201     c                   eval      societa113 = xscsoc
307200960201     c                   eval      snatura113 = snatura002
307300960201     c                   eval      ctb113     = d1ctb
307400960201     c                   eval      kcc113     = c1kcc
307500960201     c                   eval      ksc113     = c1ksc
307600960201     C                   CALLb     'XDT4000'
307700960201     C     dtpar113T     PARM                    XDTAMG
307800960201     C                   PARM      s1dtpar       XDTGMA
307900960201     C                   PARM      4             XDTSTA
308000960201     c                   eval      nrpar113T  = s1nrpar
308100960201     c                   eval      serpar113T = s1serpar
308200960201     C                   CALLb     'XDT4000'
308300960201     C     dtpar113I     PARM                    XDTAMG
308400960201     C                   PARM      wdtpar        XDTGMA
308500960201     C                   PARM      4             XDTSTA
308600960201     c                   eval      nrpar113I  = wnrpar
308700960201     c                   eval      serpar113I = wserpar
308800960201     c                   eval      kpjbu = ndc113ds
308900960201     c                   call      'NDC113R'
309000960201     c                   parm                    kpjba
309100960201     c                   parm                    autds
309200960201     c                   eval      ndc113ds = kpjbu
309300960201     c* Se si è verificato un errore, esco dal ciclo
309400960201     C                   if        err113 = *on
309500960202     c                   z-add     s1nrr         c1rcd
309600960201     c                   leave
309700960201   E3C                   ENDIF
309800960201     c*
309900960201   E3C                   ENDIF
310000960201     C                   MOVE      *ON           *IN32
310100960201     C* imposto quegli attributi del sfl che sono modificabili
310200960201     C* al richiamo del determinato pgm
310300960201     C                   EXSR      ATRS1
310400960201     C                   UPDATE    FMTS1
310500960201     C                   MOVE      *Off          *IN32
310600960201     c*
310700960201     C                   ENDDO
310800960201     C*
310900960201     c* Se si è verificato almeno un errore, rollback e riemetto sfl, altri
311000960201     c* menti commit e passo videata di parzializzazione
311100960201     C                   if        err113 = *on
311200960201     c                   ROLBK
311300960201     c                   seton                                        9599
311400960201     c                   else
311500960202     c                   if        msgrtn <> '2'
311600960201     c                   COMMIT
311700960201     c                   eval      $INZS1= *on
311800960201     c                   endif
311900960202     c                   endif
312000960201     c*
312100960201   E3C                   ENDIF
312200960129     C*
312300960129     C                   ENDSR
312400940127     C/EJECT
312500940127     C************************************************************
312600940131     C* CONTROLLO CAMPI I/O RIGA LISTA
312700940127     C************************************************************
312800940131     C     RECS1         BEGSR
312900940131     C*
313000940201     C* reset indicatori DSPATR
313100940201     C                   MOVE      *ALL'0'       IN4049           10
313200940201     C                   MOVEA     IN4049        *IN(40)
313300940201     C*
313400940131     C* Controllo se Utente Autorizzato alla scelta
313500940202     C     S1OPZ         IFNE      0
313600940131     C                   Z-ADD     1             X
313700940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
313800940211     C     *IN21         IFEQ      *OFF
313900940223     C     $FL1(X)       OREQ      'N'
314000940131     C                   MOVE      *ON           $ESCI
314100940201     C                   SETON                                        504099
314200940131     C                   ENDIF
314300940202     C                   ENDIF
314400960130     c*
314500960130     C* Controllo autorizzazioni per opzione 50 o 51 taglia e incolla
314600960130     C     S1OPZ         IFEQ      50
314700960130     C     S1OPZ         oreq      51
314800960130     c                   select
314900960130     C                   when      snatura002 = 'C'
315000960130     c                   if        ricparc = *off
315100960627     C                   seton                                        999440
315200960130     c                   endif
315300960130     C*
315400960130     C                   when      snatura002 = 'F'
315500960130     c                   if        ricparF = *off
315600960627     C                   seton                                        999440
315700960130     c                   endif
315800960130     C*
315900960130     C                   other
316000960130     c                   if        ricparA = *off
316100960627     C                   seton                                        999440
316200960130     c                   endif
316300960130     C                   endsl
316400960130     c                   ENDIF
316500950706     C*
316600940131     C                   ENDSR
316700941118     C/EJECT
316800941118     C************************************************************
316900941118     C* GESTIONE OPZIONI LISTA
317000941118     C************************************************************
317100941118     C     OPZS1         BEGSR
317200950301     C*
317300950301     C                   Z-ADD     RIRI          CURR
317400950301     C                   Z-ADD     COCO          CURC
317500941118     C*
317600941027     C                   SELECT
317700940225     C*
317800951114     C* se opz=Movimenti
317900951114     C* richiamo il pgm di gestione dettaglio partita
318000951114     C     S1OPZ         WHENEQ    11
318100960126     C                   RESET                   NDC111DS
318200951114     c*
318300960123     C* Se richiamato in ricerca, passo stessa opzione
318400960126     C                   MOVE      OPZ110        OPZ111
318500960126     C                   move      d1ctb         ctb111
318600960126     C                   move      s1unita       unita111
318700960126     C                   move      d1kcc         kcc111
318800960126     C                   move      d1ksc         ksc111
318900951124     C* Campi specifici della scelta
319000951114     C                   CALLb     'XDT4000'
319100960126     C     dtpar111      PARM                    XDTAMG
319200951114     C                   PARM      s1dtpar       XDTGMA
319300951114     C                   PARM      4             XDTSTA
319400960126     C                   move      s1nrpar       nrpar111
319500960126     C                   move      s1serpar      serpar111
319600960129     C                   eval      descr111  = descr002
319700960129     C                   eval      desagg111 = desagg002
319800960129     C                   eval      snatura111= snatura002
319900960129     c                   eval      divisa111 = H1divisa
320000960129     c                   eval      kscage111 = H1kscage
320100960126     c                   movel     ndc111ds      KPJBU
320200960126     C                   CALL      'NDC111R'
320300951114     C                   PARM                    KPJBA
320400960129     C                   PARM                    SOC001
320500960126     c                   movel     KPJBU         ndc111ds
320600951114     C*
320700951114     C* ritorno da PGM movimenti
320800960126     C                   EXSR      RTN111
320900951114     C
321000940831     C*
321100950620     C* se opz=Commenti
321200950620     C* richiamo il pgm di gestione
321300940831     C     S1OPZ         WHENEQ    14
321400950606     C                   RESET                   ANA725DS
321500941214     C                   MOVE      '02'          OPZ725
321600951120    >C                   MOVE      'PPA'         TPA725
321700941117     C* inserire chiavi commenti
321800960126    >C                   MOVEL     d1ctb         KY1725
321900950907    >C                   MOVEL     C1KCC         KY2725
322000950907    >C                   MOVE      C1KSC         KY2725
322100950907    >C                   MOVEL     S1DTPAR       KY3725
322200950907    >C                   MOVEL     s1nrpar       KY4725
322300950907    >C                   MOVE      s1serpar      KY4725
322400941214     C                   MOVE      *BLANK        KPJBU
322500950606     C                   MOVEL     ANA725DS      KPJBU
322600941026     C                   CALL      'ANA725R'
322700940831     C                   PARM                    KPJBA
322800950606     C                   MOVEL     KPJBU         ANA725DS
322900941124     C*
323000941124     C* ritorno da PGM commenti
323100941124     C                   EXSR      RTN725
323200940831     C*
323300940831     C                   MOVE      *HIVAL        $LASTV
323400951109     C*
323500951109     C* se opz=Particolarità partite
323600951109     C* richiamo il pgm di gestione
323700951109     C     S1OPZ         WHENEQ    20
323800951109     C                   RESET                   NDC060DS
323900951113     c*
324000951114     C                   MOVE      '02'          OPZ060
324100960126     C                   MOVE      s1unita       unita060
324200960126     C                   MOVE      d1ctb         ctb060
324300951109     C                   MOVE      C1KCC         KCC060
324400951109     C                   MOVE      C1KSC         KSC060
324500951109     C                   CALLb     'XDT4000'
324600951109     C     dtpar060      PARM                    XDTAMG
324700951109     C                   PARM      s1dtpar       XDTGMA
324800951109     C                   PARM      4             XDTSTA
324900951109     C                   move      s1nrpar       nrpar060
325000951109     C                   move      s1serpar      serpar060
325100951109     C*imposto chiave unique per la lista
325200951109    >C                   MOVEL     ctb060        UQ1060
325300951109    >C                   MOVEL     kcc060        UQ2060
325400951109    >C                   MOVEL     ksc060        UQ3060
325500951109     C                   move      dtpar060      UQ4060
325600951109     C                   move      nrpar060      UQ5060
325700951109     C                   move      serpar060     UQ6060
325800951109     C                   MOVE      *BLANK        KPJBU
325900951109     C                   MOVEL     NDC060DS      KPJBU
326000951109     C                   CALL      'NDC060R'
326100951109     C                   PARM                    KPJBA
326200951109     C                   MOVEL     KPJBU         NDC060DS
326300951109     C*
326400951109     C* ritorno da PGM commenti
326500951109     C                   EXSR      RTN060
326600951109     C*
326700951109     C                   MOVE      *HIVAL        $LASTV
326800950707     C*
326900950707     C* se opz=MIX
327000950707     C* richiamo il pgm di gestione
327100950707     C     S1OPZ         WHENEQ    32
327200950707     C                   RESET                   NDC065DS
327300950707     C                   MOVE      '02'          OPZ065
327400960126     C                   MOVE      s1unita       unita065
327500960126     C                   MOVE      d1ctb         ctb065
327600950707     C                   MOVE      C1KCC         KCC065
327700950707     C                   MOVE      C1KSC         KSC065
327800951109     C                   CALLb     'XDT4000'
327900951109     C     dtpar065      PARM                    XDTAMG
328000951109     C                   PARM      s1dtpar       XDTGMA
328100951109     C                   PARM      4             XDTSTA
328200950707     C                   move      s1nrpar       nrpar065
328300950707     C                   move      s1serpar      serpar065
328400950718     C*imposto chiave unique per la lista
328500950906    >C                   MOVEL     ctb065        UQ1065
328600950906    >C                   MOVEL     kcc065        UQ2065
328700950906    >C                   MOVEL     ksc065        UQ3065
328800950906     C                   move      dtpar065      UQ4065
328900950906     C                   move      nrpar065      UQ5065
329000950906     C                   move      serpar065     UQ6065
329100950707     C                   MOVE      *BLANK        KPJBU
329200950707     C                   MOVEL     NDC065DS      KPJBU
329300950707     C                   CALL      'NDC065R'
329400950707     C                   PARM                    KPJBA
329500950707     C                   MOVEL     KPJBU         NDC065DS
329600950707     C*
329700950707     C* ritorno da PGM commenti
329800950707     C                   EXSR      RTN065
329900950707     C*
330000950707     C                   MOVE      *HIVAL        $LASTV
330100951204     C*
330200941027     C                   OTHER
330300940131     C*
330400940506     C                   MOVE      *HIVAL        $LASTV
330500950907     C*
330600950907     C*
330700941027     C                   ENDSL
330800940225     C*
330900940131     C                   ENDSR
331000940223     C/EJECT
331100940223     C************************************************************
331200951109     C* GESTIONE RITORNO DA PGM movimenti
331300940223     C************************************************************
331400960126    >C     RTN111        BEGSR
331500940223     C*
331600940223     C* modo di ritorno
331700940223     C*
331800941027     C                   SELECT
331900950301     C* questi modi di utilizzo dei valori di ritorno dal
332000950301     C* pgm di manutenzione rcd di anagrafica sono delle
332100950301     C* proposte, normalmente sempre valide, ma modificabili
332200950301     C* per situazioni particolari
332300940223     C* 1 = F3
332400960126    >C     RET111        WHENEQ    '1'
332500940224     C                   MOVE      *ON           $ESCI
332600940223     C                   MOVE      *ON           $FINE
332700960126     C                   MOVE      '1'           RET110
332800940223     C* 2 = F12
332900960126    >C     RET111        WHENEQ    '2'
333000960314    >C     opr111        andne     '1'
333100940223     C                   MOVE      *ON           $ESCI
333200940223     C*
333300941027     C                   ENDSL
333400940223     C*
333500940223     C* operazione eseguite dal pgm chiamato
333600940223     C*
333700960126    >C                   MOVE      OPR111        WOPR
333800941027     C                   SELECT
333900940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
334000951115     C* non ricarico subfile perchè non può cambiare
334100960126    >C     OPR111        WHENEQ    '1'
334200960314     C                   MOVE      *ON           WINZS1
334300940223     C*
334400941027     C                   ENDSL
334500940223     C*
334600940223     C* funzione non eseguibile per errore :
334700940223     C*
334800941027     C                   SELECT
334900940223     C* 1 = funzione richiamata chiusa in errore
335000960126    >C     ERR111        WHENEQ    '1'
335100940223     C                   MOVE      *ON           $ESCI
335200951109     C                   SETON                                        9599
335300950301     C* 2 = funzione richiamata senza la dovuta autorizzazione
335400960126    >C     ERR111        WHENEQ    '2'
335500950301     C                   MOVE      *ON           $ESCI
335600951109     C                   SETON                                        9499
335700941027     C*
335800941027     C                   ENDSL
335900940223     C*
336000940223     C                   ENDSR
336100951109     C/EJECT
336200951109     C************************************************************
336300951109     C* GESTIONE RITORNO DA PGM DI particolarità partita
336400951109     C************************************************************
336500951109    >C     RTN060        BEGSR
336600951109     C*
336700951109     C* modo di ritorno
336800951109     C*
336900951109     C                   SELECT
337000951109     C* questi modi di utilizzo dei valori di ritorno dal
337100951109     C* pgm di manutenzione rcd di anagrafica sono delle
337200951109     C* proposte, normalmente sempre valide, ma modificabili
337300951109     C* per situazioni particolari
337400951109     C* 1 = F3
337500951109    >C     RET060        WHENEQ    '1'
337600951109     C                   MOVE      *ON           $ESCI
337700951109     C                   MOVE      *ON           $FINE
337800960126     C                   MOVE      '1'           RET110
337900951114     C* 2 = F12 (ma non c'è un aggiornamento)
338000951109    >C     RET060        WHENEQ    '2'
338100960314     c     OPR060        ANDNE     '1'
338200951109     C                   MOVE      *ON           $ESCI
338300951109     C*
338400951109     C                   ENDSL
338500951109     C*
338600951109     C* operazione eseguite dal pgm chiamato
338700951109     C*
338800951109    >C                   MOVE      OPR060        WOPR
338900951109     C                   SELECT
339000951115     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
339100951109    >C     OPR060        WHENEQ    '1'
339200960130     C                   MOVE      *on           Winzs1
339300951109     C                   ENDSL
339400951109     C*
339500951109     C* funzione non eseguibile per errore :
339600951109     C*
339700951109     C                   SELECT
339800951109     C* 1 = funzione richiamata chiusa in errore
339900951109    >C     ERR060        WHENEQ    '1'
340000951109     C                   MOVE      *ON           $ESCI
340100951109     C                   SETON                                        9599
340200951109     C* 2 = funzione richiamata senza la dovuta autorizzazione
340300951109    >C     ERR060        WHENEQ    '2'
340400951109     C                   MOVE      *ON           $ESCI
340500951109     C                   SETON                                        9499
340600951109     C*
340700951109     C                   ENDSL
340800951109     C*
340900951109     C                   ENDSR
341000950707     C/EJECT
341100950707     C************************************************************
341200950707     C* GESTIONE RITORNO DA PGM DI GESTIONE MIX DI PARTITA
341300950707     C************************************************************
341400950707    >C     RTN065        BEGSR
341500950707     C*
341600950707     C* modo di ritorno
341700950707     C*
341800950707     C                   SELECT
341900950707     C* questi modi di utilizzo dei valori di ritorno dal
342000950707     C* pgm di manutenzione rcd di anagrafica sono delle
342100950707     C* proposte, normalmente sempre valide, ma modificabili
342200950707     C* per situazioni particolari
342300950707     C* 1 = F3
342400950707    >C     RET065        WHENEQ    '1'
342500950707     C                   MOVE      *ON           $ESCI
342600950707     C                   MOVE      *ON           $FINE
342700960126     C                   MOVE      '1'           RET110
342800950707     C* 2 = F12
342900950707    >C     RET065        WHENEQ    '2'
343000960314     c     opr065        andne     '1'
343100950707     C                   MOVE      *ON           $ESCI
343200950707     C*
343300950707     C                   ENDSL
343400950707     C*
343500950707     C* operazione eseguite dal pgm chiamato
343600950707     C*
343700950707    >C                   MOVE      OPR065        WOPR
343800951109     C                   SELECT
343900951109     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
344000951115     c* Non eseguo ricaricamento subfile perchè non può cambiare
344100951109    >C     OPR065        WHENEQ    '1'
344200960314     C                   MOVE      *ON           WINZS1
344300950707     C*
344400950707     C                   ENDSL
344500950707     C*
344600950707     C* funzione non eseguibile per errore :
344700950707     C*
344800950707     C                   SELECT
344900950707     C* 1 = funzione richiamata chiusa in errore
345000950707    >C     ERR065        WHENEQ    '1'
345100950707     C                   MOVE      *ON           $ESCI
345200951109     C                   SETON                                        9599
345300950707     C* 2 = funzione richiamata senza la dovuta autorizzazione
345400950707    >C     ERR065        WHENEQ    '2'
345500950707     C                   MOVE      *ON           $ESCI
345600951109     C                   SETON                                        9499
345700950707     C*
345800950707     C                   ENDSL
345900950707     C*
346000950707     C                   ENDSR
346100950301     C/EJECT
346200950301     C************************************************************
346300950301     C* AGGIORNAMENTO RIGA LISTA
346400950301     C************************************************************
346500950301     C     AGGS1         BEGSR
346600950301     C*
346700950301     C*
346800950301     C                   ENDSR
346900941124     C/EJECT
347000941124     C************************************************************
347100941124     C* GESTIONE RITORNO DA PGM DEI COMMENTI
347200941124     C************************************************************
347300941124     C     RTN725        BEGSR
347400941124     C*
347500941124     C* modo di ritorno
347600941124     C*
347700941124     C                   SELECT
347800950301     C* questi modi di utilizzo dei valori di ritorno dal
347900950301     C* pgm di manutenzione rcd di anagrafica sono delle
348000950301     C* proposte, normalmente sempre valide, ma modificabili
348100950301     C* per situazioni particolari
348200941124     C* 1 = F3
348300941124     C     RET725        WHENEQ    '1'
348400941124     C                   MOVE      *ON           $ESCI
348500941124     C                   MOVE      *ON           $FINE
348600960126     C                   MOVE      '1'           RET110
348700941124     C* 2 = F12
348800941124     C     RET725        WHENEQ    '2'
348900941124     C                   MOVE      *ON           $ESCI
349000941124     C*
349100941124     C                   ENDSL
349200941124     C*
349300941124     C* operazione eseguite dal pgm chiamato
349400941124     C*
349500941124     C                   SELECT
349600941124     C* 1 = eseguita scrittura o modifica commento
349700941124     C     OPR725        WHENEQ    '1'
349800950704     C*
349900951115     C*
350000941124     C                   ENDSL
350100941124     C*
350200941124     C* funzione non eseguibile per errore :
350300941124     C*
350400941124     C                   SELECT
350500941124     C* 1 = funzione richiamata chiusa in errore
350600941124     C     ERR725        WHENEQ    '1'
350700941124     C                   MOVE      *ON           $ESCI
350800951109     C                   SETON                                        9599
350900950331     C* 2 = funzione richiamata senza la dovuta autorizzazione
351000950331     C     ERR725        WHENEQ    '2'
351100950331     C                   MOVE      *ON           $ESCI
351200951109     C                   SETON                                        9499
351300950704    >C*  eventualmente gestire altri codici di errore
351400941124     C*
351500941124     C                   ENDSL
351600941124     C*
351700941124     C                   ENDSR
351800941205     C/EJECT
351900941205     C************************************************************
352000941205     C* CONTROLLO ABILITAZIONE TASTI
352100941205     C* INPUT
352200941205     C* $TASTO =Tasto funzione premuto
352300941205     C* $FC = Tasti abilitati
352400941205     C* OUTPUT
352500941205     C* IN99 = ERrore
352600941205     C************************************************************
352700941205     C     CTRKEY        BEGSR
352800960126     C                   SETOFF                                       99
352900941205     C*
353000941205     C                   SELECT
353100941205     C     $TASTO        WHENEQ    F01
353200941205     C     $FC(1)        IFEQ      '0'
353300960129     C                   SETON                                        9997
353400941205     C                   ENDIF
353500941205     C     $TASTO        WHENEQ    F02
353600941205     C     $FC(2)        IFEQ      '0'
353700960129     C                   SETON                                        9997
353800941205     C                   ENDIF
353900941205     C     $TASTO        WHENEQ    F03
354000941205     C     $FC(3)        IFEQ      '0'
354100960129     C                   SETON                                        9997
354200941205     C                   ENDIF
354300941205     C     $TASTO        WHENEQ    F04
354400941205     C     $FC(4)        IFEQ      '0'
354500960129     C                   SETON                                        9997
354600941205     C                   ENDIF
354700941205     C     $TASTO        WHENEQ    F05
354800941205     C     $FC(5)        IFEQ      '0'
354900960129     C                   SETON                                        9997
355000941205     C                   ENDIF
355100941205     C     $TASTO        WHENEQ    F06
355200941205     C     $FC(6)        IFEQ      '0'
355300960129     C                   SETON                                        9997
355400941205     C                   ENDIF
355500941205     C     $TASTO        WHENEQ    F07
355600941205     C     $FC(7)        IFEQ      '0'
355700960129     C                   SETON                                        9997
355800941205     C                   ENDIF
355900941205     C     $TASTO        WHENEQ    F08
356000941205     C     $FC(8)        IFEQ      '0'
356100960129     C                   SETON                                        9997
356200941205     C                   ENDIF
356300941205     C     $TASTO        WHENEQ    F09
356400941205     C     $FC(09)       IFEQ      '0'
356500960129     C                   SETON                                        9997
356600941205     C                   ENDIF
356700941205     C     $TASTO        WHENEQ    F10
356800941205     C     $FC(10)       IFEQ      '0'
356900960129     C                   SETON                                        9997
357000941205     C                   ENDIF
357100941205     C     $TASTO        WHENEQ    F11
357200941205     C     $FC(11)       IFEQ      '0'
357300960129     C                   SETON                                        9997
357400941205     C                   ENDIF
357500941205     C     $TASTO        WHENEQ    F12
357600941205     C     $FC(12)       IFEQ      '0'
357700960129     C                   SETON                                        9997
357800941205     C                   ENDIF
357900941205     C     $TASTO        WHENEQ    F13
358000941205     C     $FC(13)       IFEQ      '0'
358100960129     C                   SETON                                        9997
358200941205     C                   ENDIF
358300941205     C     $TASTO        WHENEQ    F14
358400941205     C     $FC(14)       IFEQ      '0'
358500960129     C                   SETON                                        9997
358600941205     C                   ENDIF
358700941205     C     $TASTO        WHENEQ    F15
358800941205     C     $FC(15)       IFEQ      '0'
358900960129     C                   SETON                                        9997
359000941205     C                   ENDIF
359100941205     C     $TASTO        WHENEQ    F16
359200941205     C     $FC(16)       IFEQ      '0'
359300960129     C                   SETON                                        9997
359400941205     C                   ENDIF
359500941205     C     $TASTO        WHENEQ    F17
359600941205     C     $FC(17)       IFEQ      '0'
359700960129     C                   SETON                                        9997
359800941205     C                   ENDIF
359900941205     C     $TASTO        WHENEQ    F18
360000941205     C     $FC(18)       IFEQ      '0'
360100960129     C                   SETON                                        9997
360200941205     C                   ENDIF
360300941205     C     $TASTO        WHENEQ    F19
360400941205     C     $FC(19)       IFEQ      '0'
360500960129     C                   SETON                                        9997
360600941205     C                   ENDIF
360700941205     C     $TASTO        WHENEQ    F20
360800941205     C     $FC(20)       IFEQ      '0'
360900960129     C                   SETON                                        9997
361000941205     C                   ENDIF
361100941205     C     $TASTO        WHENEQ    F21
361200941205     C     $FC(21)       IFEQ      '0'
361300960129     C                   SETON                                        9997
361400941205     C                   ENDIF
361500941205     C     $TASTO        WHENEQ    F22
361600941205     C     $FC(22)       IFEQ      '0'
361700960129     C                   SETON                                        9997
361800941205     C                   ENDIF
361900941205     C     $TASTO        WHENEQ    F23
362000941205     C     $FC(23)       IFEQ      '0'
362100960129     C                   SETON                                        9997
362200941205     C                   ENDIF
362300941205     C     $TASTO        WHENEQ    F24
362400941205     C     $FC(24)       IFEQ      '0'
362500960129     C                   SETON                                        9997
362600941205     C                   ENDIF
362700941205     C                   ENDSL
362800941205     C*
362900941205     C                   ENDSR
363000940223     C/EJECT
363100940131     C************************************************************
363200940131     C* OPERAZIONI INIZIALI
363300940131     C************************************************************
363400940131     C     *INZSR        BEGSR
363500000118C1309C     *DMY          Move      Udate         ZDateISO
363600000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
363700940131     C*
363800940127     C* Reperimento parametri
363900940127     C*
364000940117     C     *ENTRY        PLIST
364100940117     C                   PARM                    KPJBA
364200960129     C                   PARM                    DSAUT
364300950410     C*
364400960129     C     PARMS         IFLT      2
364500950410     C                   MOVE      *BLANK        AUTDS
364600950410     C                   ELSE
364700950410     C                   MOVEL     DSAUT         AUTDS
364800950410     C                   ENDIF
364900960126     C*
365000960126     C* Reperimento dati società
365100960126     C*
365200960126    >C                   MOVEL     'SOC001'      TIPXSC
365300960126     C                   MOVEL     *BLANK        SOCXSC
365400960126     C                   EXSR      REPSOC
365500960126     C     RTNXSC        IFNE      '1'
365600960126    >C                   MOVEL     XSOCDS        SOC001
365700960126     C                   ELSE
365800960126     C                   MOVE      *ON           $FINE
365900960126     C                   EXSR      ENDPGM
366000960126     C                   ENDIF
366100951108     C*
366200951108     C* Reperimento autorizzazioni
366300951108     C* le autorizzazioni potrebbero essere già state passate,
366400951108     C* nel qual caso non vanno reperite
366500951108    >C     AUTDS         IFEQ      *BLANK
366600951108    >C                   EXSR      REPAUT
366700951108     C                   ENDIF
366800960126     C*
366900960126     C* reperisco le dimensioni delle variabili che conterranno i tasti
367000960126     C                   EVAL      $D1L01 = %Size(Z3Ke1)
367100960126     C                   EVAL      $D1L02 = %Size(Z3Ke2)
367200960126     C                   EVAL      $S1L01 = %Size(Z1Ke1)
367300960126     C                   EVAL      $S1L02 = %Size(Z1Ke2)
367400960126     C                   EVAL      $C1Op1 = %Size(C1Opz1)
367500960126     C                   EVAL      $C1Op2 = %Size(C1Opz2)
367600960126     C                   EVAL      $D2L01 = %Size(Z2Ke1)
367700960126     C                   EVAL      $D2L02 = 0
367800940225     C*
367900940223     C* Reperimento opzioni
368000940127     C*
368100940128     C                   EXSR      REPOPZ
368200950406     C*
368300940506     C* Reperimento tasti di funzione
368400940506     C*
368500940506     C                   EXSR      REPKEY
368600951109     C* Udate in iso
368700951109     C                   CALLb     'XDT4000'
368800960126     C     UdateR        PARM                    XDTAMG
368900000118C1309C*****              PARM      UDATE         XDTGMA
369000000118C1309C                   PARM      ZDate         XDTGMA
369100951109     C                   PARM      4             XDTSTA
369200960202     C* Eseguo strcmtctl
369300960202     C                   CALLb     'XSTRCMT'
369400940127     C*
369500940117     C                   ENDSR
369600960126     C/EJECT
369700960126     C************************************************************
369800960126     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
369900960126     C************************************************************
370000960126     C     REPDFT        BEGSR
370100960126     C*
370200960126     C* preparo i parametri di chiamata al driver
370300960126     C                   CLEAR                   XMPEDS
370400960126     C                   EVAL      TpChiamata = '01'
370500960126     C                   EVAL      NChiavi = 5
370600960126     C                   EVAL      VLogica = 2
370700960126     C*
370800960126     C* preparo le chiavi per la chiamata al driver
370900960126     C                   CLEAR                   XMPEDS1
371000960126     C                   MOVEL     XSCSOC        DSMPESOC
371100960126     C                   MOVEL     DSPGM         DSMPEPGM
371200960126     C                   MOVEL     KNMUS         DSMPEPRF
371300960126     C* Si reperiscono i dati di default a seconda delle modalità con
371400960126     C* cui il pgm è usato
371500960126     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
371600960126     C* MPETIP = 'PV'  pgm richiamato da menù
371700960126     C* MPETIP = 'PB'  x lancio Batch
371800960126     C                   SELECT
371900960126    >C     OPZ110        WHENEQ    '01'
372000960126     C                   MOVEL     'PX'          DSMPETIP
372100960126     C                   OTHER
372200960126     C                   MOVEL     'PV'          DSMPETIP
372300960126     C                   ENDSL
372400960126     C*
372500960126     C                   CALLB     'XMPE'
372600960126     C                   PARM                    XMPEDS
372700960126     C                   PARM                    XMPEDS1
372800960126     C*
372900960126     C     Risultato     IFEQ      '0'
373000960126     C                   MOVEL     DSMPEPAR      DSMPE
373100960126     C* nel caso sia la prima volta che l'utente apre questa anagrafica
373200960126     C* i suoi valori di default vengono decisi a pgm
373300960126     C                   ELSE
373400960126     C                   Z-ADD     1             $LOP
373500960126     C                   Z-ADD     1             $ORD
373600051220C1895C                   MOVE      *zero         $nrp
373700960126     C                   ENDIF
373800960126     C*
373900960126     C                   ENDSR
374000940223     C/EJECT
374100940223     C************************************************************
374200940223     C* INIZIALIZZAZIONE VARIABILI
374300940223     C************************************************************
374400940223     C     INZVAR        BEGSR
374500960126     C*
374600960126     C* Dati da PGM chiamante
374700960126     C*
374800960126    >C                   MOVEL     KPJBU         ndc110DS
374900960126     C*
375000960126     C* Se società diversa da quella impostata nella *inzsr, rieseguo
375100960126     C* reperimento dati società
375200960126     C                   If        Societa110 <> xscsoc
375300960126    >C                   MOVEL     'SOC001'      TIPXSC
375400960126     C                   MOVEL     Societa110    SOCXSC
375500960126     C                   EXSR      REPSOC
375600960126     C     RTNXSC        IFNE      '1'
375700960126    >C                   MOVEL     XSOCDS        SOC001
375800960126     C                   ELSE
375900960126     C                   MOVE      *ON           $FINE
376000960126     C                   EXSR      ENDPGM
376100960126     C                   ENDIF
376200960126     C                   endif
376300960126     C*
376400960126     C* Reperimento valori default utente
376500960126     C*
376600960126     C                   EXSR      REPDFT
376700940506     C*
376800940506     C* Definizione delle opzioni utilizzabili a pgm
376900940506     C*
377000940506     C                   EXSR      DEFOPZ
377100950330     C*
377200940223     C* Variabili per gestione videate
377300940223     C*
377400940223     C                   MOVE      *OFF          $FINE
377500960126     C                   MOVE      *OFF          $INZD1                         inizializzione D1
377600960126     C                   MOVE      *OFF          $INZS1                         inizializzione S1
377700960126     C                   MOVE      *OFF          $INZD2                         inizializzione D2
377800950606     C                   MOVE      *OFF          $EndFile
377900940223     C                   MOVE      *OFF          $ESCI
378000940223     C                   MOVE      *OFF          $RCDOK
378100940509     C                   MOVE      *HIVAL        $LASTV
378200960126     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
378300960126     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
378400960126     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
378500940318     C                   MOVE      *ON           $INZS1
378600960126     C                   MOVE      *ON           $INZD1
378700951110     C                   CLEAR                   Wsaldototd
378800951110     C                   CLEAR                   Wsaldotota
378900960126     C*
379000960126     C* definisco qual'è la prima videata
379100960126     C* (se la prima videata risulta la lista, devo comunque caricare
379200960126     C*  la videata di parzializzazione e gestire la definizione
379300960126     C*  dell'ordinamento, perchè, dai default oppure se sono stati
379400960126     C*  passati dei dati, può variare il caricamento)
379500960126     C                   SELECT
379600960126     C     $LOP          WHENEQ    1
379700960126     C                   MOVE      'D1'          $GEST
379800960126     C     $LOP          WHENEQ    2
379900960126     C                   MOVE      'S1'          $GEST
380000960126     C                   EXSR      INZD1
380100960126     C                   MOVE      *OFF          $INZD1
380200960126     C                   EXSR      DEFORD
380300960126     C* Controlli da eseguire se prima videata è lista
380400960126     C                   EXSR      CTRD1
380500960126     C                   if        *IN99 = *on
380600960126     C                   MOVE      'D1'          $GEST
380700960126     C                   MOVE      *ON           $INZD1
380800960126     C                   EVAL      $LOP  = 1
380900960126     C                   endif
381000960126     C                   ENDSL
381100951122     C*
381200951122     C* Valorizzazione campi univoci testate
381300951122     C*
381400960126     C                   CLEAR                   FMTT1
381500951122     C                   MOVEL     KNSIF         NOMSIF
381600951122     C                   MOVEL     XSCDSI        NOMDIT
381700951122     C                   MOVEL     DSPGM         NOMPGM
381800951109     C*
381900951109     C* Reperimento dati moneta di conto
382000951109     C                   Eval      WDivisa =  XscDiv
382100951109     C                   Exsr      RepDivisa
382200951110     C                   Eval      WNrDec  = X01NrDec
382300971104     C*
382400971104     C* Imposto il numero dei decimali della moneta di conto
382500971104     C                   Eval      NrDec  = X01NrDec
382600951108     C*
382700951108     C* Definisco ordinamento lettura
382800951108     C*
382900950410     C                   EXSR      DEFORD
383000951110     C* Salvo divisa in entrata
383100960126     C                   EVAL      SavDivVis =   '1'
383200940223     C*
383300940223     C                   ENDSR
383400960126     C/EJECT
383500960126     C************************************************************
383600960126     C* REPERIMENTO DATI SOCIETA'
383700960126     C************************************************************
383800960126     C     REPSOC        BEGSR
383900960126     C*
384000960126     C                   CALLB     'XSOC'
384100960126     C                   PARM                    TIPXSC            6
384200960126     C                   PARM                    SOCXSC            3
384300960126     C                   PARM                    CDSXSC            9 0
384400960126     C                   PARM                    MODXSC            3
384500960126     C                   PARM      *BLANKS       RTNXSC            1
384600960126     C                   PARM                    XSOCDS
384700960126     C                   PARM                    KPJBA
384800960126     C*
384900960126     C                   ENDSR
385000951109     C/EJECT
385100951109     C************************************************************
385200951109     C* Reperisco dati divisa
385300951109     C************************************************************
385400951109     C     RepDivisa     BEGSR
385500951109     C*
385600951109     C* Divisa
385700951109     C                   reset                   X01CambiDs
385800951109     C                   MOVE      xscsoc        X01societa
385900951109     C                   MOVEL     WDivisa       X01divisa
386000951109     C                   MOVEL     '1'           X01flgcam
386100951109     C                   MOVE      Wdate         x01dtRifCa
386200951109     C                   CALL      'X01CAMBI'
386300951109     C                   PARM                    X01CambiDs
386400951109     C*
386500951109     C                   ENDSR
386600951109     c/EJECT
386700951109     C************************************************************
386800951109     C* Editazione di un numero
386900951109     C************************************************************
387000951109     C     Edit          Begsr
387100951109      *
387200951109     C                   Select
387300951110     C                   When      WNrDec = 0
387400951110     C                   Z-add     WImporto      $SrcVar
387500951110     C                   When      WNrDec = 1
387600951110     C     WImporto      Mult      10            $SrcVar
387700951110     C                   When      WNrDec = 2
387800951110     C     WImporto      Mult      100           $SrcVar
387900951109     C                   Other
388000951110     C     WImporto      Mult      1000          $SrcVar
388100951109     C                   Endsl
388200951109     C*
388300951110     C                   Eval      $SrcVarDe = WNrDec
388400951109     C*
388500951109     C                   CallB     'X23EDITR'
388600951109     C                   Parm                    $SrcVar
388700951109     C                   Parm                    $SrcVarPr
388800951109     C                   Parm                    $SrcVarDe
388900951109     C                   Parm                    $EdtCode
389000951109     C                   Parm                    $LenFld
389100951109     C                   Parm      *blanks       $RcvrVar
389200951109     C                   Parm                    $Err
389300951109      *
389400951109     C                   ENDSR
389500951109     C*
389600941124     C/EJECT
389700941124     C************************************************************
389800941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
389900941124     C************************************************************
390000941124     C     REPAUT        BEGSR
390100941124     C*
390200941124     C                   CLEAR                   AUTDS
390300941124     C*
390400941124     C                   MOVEL     XSCSOC        XCASOC
390500941124     C                   MOVEL     XSCDUN        XCAUNI
390600941124     C                   Z-ADD     0             XCAGRP
390700941124     C                   MOVEL     KNMUS         XCAPRF
390800960126    >C                   MOVEL     'CONPNO'      XCAFNC
390900941124    >C                   MOVEL     *ZERO         XCAVFU
391000941124    >C                   MOVE      'CK'          XCATCK
391100941124    >C                   MOVE      '1'           XCANAU
391200941124     C*
391300950522     C                   CALLB     'XCHKAUT'
391400941124     C                   PARM                    AUTDS
391500941124     C*
391600941124     C* errori/non abilitazione
391700941124     C     XCARTN        IFNE      0
391800941124     C                   EXSR      ENDPGM
391900941124     C                   ELSE
392000960126    >C                   MOVEL     XCADDS        CONPNODS
392100941124     C                   ENDIF
392200941124     C*
392300941124     C                   ENDSR
392400940223     C/EJECT
392500940131     C************************************************************
392600940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
392700940131     C************************************************************
392800940225     C     DEFOPZ        BEGSR
392900940131     C*
393000940225     C* Questa routine deve valorizzare la schiera $FL1
393100940225     C* con S o N a seconda della utilizzabilità o meno
393200940225     C* dell'opzione correlata
393300940225     C*
393400950301     C* Inizialmente si considerano tutte le opzioni come valide
393500940317     C     1             DO        $DIMOP        X
393600940225     C                   MOVE      'S'           $FL1(X)
393700941027     C                   ENDDO
393800940715     C*
393900951109     C* Questa routine deve valorizzare la schiera $FC1/$FR1
394000941018     C* con 1 o 0 a seconda della utilizzabilità o meno
394100940715     C* del tasto funzione correlato
394200940715     C*
394300940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
394400940715     C* di compilazione
394500940715     C                   RESET                   $FC1
394600940715     C                   RESET                   $FR1
394700960126     C                   RESET                   $FC2
394800960126     C                   RESET                   $FR2
394900960126     C                   RESET                   $FC3
395000960126     C                   RESET                   $FR3
395100960126     C*
395200960126     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
395300960126     C* invalido alcune opzioni
395400960126     C                   SELECT
395500960126     C* Se pgm richiamato in ricerca disattivo :
395600960126    >C     OPZ110        WHENEQ    '01'
395700960126     C* F3=Fine
395800960126     C                   Z-ADD     3             X
395900960126     C                   MOVE      '0'           $FC1(X)
396000960126     C                   MOVE      '0'           $FC2(X)
396100960126     C                   MOVE      '0'           $FC3(X)
396200960126     C*
396300960126     C     OPZ110        WHENEQ    *BLANKS
396400960126     C* Se pgm non richiamato, disattivo subito :
396500960126     C* F8=Selezione seguente
396600960126     C                   Z-ADD     8             X
396700960126     C                   MOVE      '0'           $FC3(X)
396800960126     C                   MOVE      '0'           $FC1(X)
396900960126     C*
397000960126     C* In tutti gli altri casi, disattivo subito :
397100960126     C                   OTHER
397200960126     C*
397300960126     C                   ENDSL
397400940225     C*
397500940225     C*
397600940225     C                   ENDSR
397700940225     C/EJECT
397800940225     C************************************************************
397900940225     C* REPERIMENTO OPZIONI
398000940225     C************************************************************
398100940225     C     REPOPZ        BEGSR
398200940225     C*
398300940314     C     1             DO        $DIMOP        X
398400940128     C*
398500940223     C     $MS1(X)       IFNE      *BLANK
398600940128     C*
398700940223     C                   MOVEL     $MS1(X)       IDMSG
398800950522     C                   CALLB     'XRTVM'
398900940201     C                   PARM                    IDMSG            27
399000940201     C                   PARM                    TXTMSG          100
399100940201     C                   PARM                    ERRMSG            1
399200940128     C*
399300940128     C     ERRMSG        IFNE      *ON
399400940223     C                   MOVEL     TXTMSG        $TX1(X)
399500940128     C                   ELSE
399600940223     C                   MOVEL     *ALL'?'       $TX1(X)
399700941027     C                   ENDIF
399800940128     C*
399900941027     C                   ENDIF
400000940128     C*
400100941027     C                   ENDDO
400200940128     C*
400300940128     C                   ENDSR
400400940506     C/EJECT
400500940506     C************************************************************
400600940506     C* REPERIMENTO TASTI DI FUNZIONE
400700940506     C************************************************************
400800940506     C     REPKEY        BEGSR
400900940506     C                   MOVEA     $FM1          $FM
401000950906     C                   MOVEA     $Fc1          $Fc
401100940506     C                   EXSR      REPKKK
401200940506     C                   MOVEA     $FX           $FX1
401300940506     C*
401400960126     C                   MOVEA     $FM2          $FM
401500960126     C                   MOVEA     $Fc2          $Fc
401600940506     C                   EXSR      REPKKK
401700960126     C                   MOVEA     $FX           $FX2
401800960126     C*
401900960126     C                   MOVEA     $FM3          $FM
402000960126     C                   MOVEA     $Fc3          $Fc
402100960126     C                   EXSR      REPKKK
402200960126     C                   MOVEA     $FX           $FX3
402300940919     C*
402400940919     C                   ENDSR
402500940506     C/EJECT
402600940506     C************************************************************
402700940506     C* REPERIMENTO TASTI DI FUNZIONE
402800940506     C************************************************************
402900940506     C     REPKKK        BEGSR
403000940506     C*
403100940506     C                   CLEAR                   $FX
403200940506     C     1             DO        24            X
403300940506     C*
403400940506     C     $FM(X)        IFNE      *BLANK
403500950906     C     $Fc(X)        andne     '0'
403600940506     C*
403700940506     C                   MOVEL     $FM(X)        IDMSG
403800950522     C                   CALLB     'XRTVM'
403900940506     C                   PARM                    IDMSG            27
404000940506     C                   PARM                    TXTMSG          100
404100940506     C                   PARM                    ERRMSG            1
404200940506     C*
404300940506     C     ERRMSG        IFNE      *ON
404400940506     C                   MOVEL     TXTMSG        $FX(X)
404500940506     C                   ELSE
404600940506     C                   MOVEL     *ALL' '       $FX(X)
404700941027     C                   ENDIF
404800940506     C*
404900941027     C                   ENDIF
405000940506     C*
405100941027     C                   ENDDO
405200940506     C*
405300940506     C                   ENDSR
405400940223     C/EJECT
405500940128     C************************************************************
405600950606     C* DEFINIZIONE KLIST
405700940128     C************************************************************
405800950606     C     DefKlist      BEGSR
405900940128     C*
406000940223     C* klist
406100940223     C*
406200950704    >C* ndppa01L
406300950906    >C     K04PPA01      KLIST
406400950704    >C                   KFLD                    PPASOCIETA
406500950906    >C                   KFLD                    PPActb
406600950704    >C                   KFLD                    PPAKCC
406700950704    >C                   KFLD                    PPAKSC
406800940127     C*
406900951113    >C     K05PPA01      KLIST
407000950704    >C                   KFLD                    PPASOCIETA
407100950906    >C                   KFLD                    PPActb
407200950704    >C                   KFLD                    PPAKCC
407300950704    >C                   KFLD                    PPAKSC
407400950704    >C                   KFLD                    PPADTPAR
407500960126     C*
407600960126    >C* anuni01L
407700960126    >C     K02UNI01      KLIST
407800960126    >C                   KFLD                    UNISOCIETA
407900960126    >C                   KFLD                    uniunita
408000960129     C*
408100960129    >C* ndpps01L
408200960129    >C     K02pps01      KLIST
408300960129     c                   kfld                    ppssys
408400960129    >C                   KFLD                    ppsnrasint
408500960201    >C* Ndepa01l
408600960201    >C     K06EPA01      KLIST
408700960201    >C                   KFLD                    EPACSI
408800960201    >C                   KFLD                    EPAKCC
408900960201    >C                   KFLD                    EPAKSC
409000960201    >C                   KFLD                    EPADRP
409100960201    >C                   KFLD                    EPANRP
409200960201    >C                   KFLD                    EPASERPAR
409300030122D1271C* NdMra04l
409400030122  " >C     K06Mra04      KLIST
409500030122  " >C                   KFLD                    MraSocieta
409600030122  " >C                   KFLD                    MraKCC
409700030122  " >C                   KFLD                    MraKSC
409800030122  " >C                   KFLD                    MraDRP
409900030122  " >C                   KFLD                    MraNRP
410000030122D1271C                   KFLD                    MraSERPAR
410100950704     C*
410200940117     C                   ENDSR
410300940223     C/EJECT
410400960126     C************************************************************
410500960126     C* AGGIORNAMENTO VALORI ASSUNTI
410600960126     C************************************************************
410700960126     C     AGGMPE        BEGSR
410800960126     C*
410900960126     C* preparo i parametri di chiamata al driver
411000960126     C                   CLEAR                   XMPEDS
411100960126     C                   EVAL      TpChiamata = '03'
411200960126     C                   EVAL      NChiavi = 5
411300960126     C                   EVAL      VLogica = 2
411400960126     C*
411500960126     C* preparo le chiavi per la chiamata al driver
411600960126     C                   CLEAR                   XMPEDS1
411700960126     C                   MOVEL     XSCSOC        DSMPESOC
411800960126     C                   MOVEL     DSPGM         DSMPEPGM
411900960126     C                   MOVEL     KNMUS         DSMPEPRF
412000960126     C*
412100960126     C* Si memorizzano i dati di default a seconda delle modalità con
412200960126     C* cui il pgm è usato
412300960126     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
412400960126     C* MPETIP = 'PV'  pgm richiamato da menù
412500960126     C* MPETIP = 'PB'  x lancio Batch
412600960126     C*
412700960126     C                   SELECT
412800960126    >C     OPZ110        WHENEQ    '01'
412900960126     C                   MOVEL     'PX'          DSMPETIP
413000960126     C                   OTHER
413100960126     C                   MOVEL     'PV'          DSMPETIP
413200960126     C                   ENDSL
413300960126     C* dati
413400960126     C                   MOVEL     DSMPE         DSMPEPAR
413500960126     C*
413600960126     C                   CALLB     'XMPE'
413700960126     C                   PARM                    XMPEDS
413800960126     C                   PARM                    XMPEDS1
413900960126     C*
414000960126     C                   ENDSR
414100960126     C/EJECT
414200941214**
414300951108PROMSG    *LIBL     OPZ0138
414400951108PROMSG    *LIBL     OPZ0014
414500951108PROMSG    *LIBL     OPZ0139
414600951108PROMSG    *LIBL     OPZ0111
414700960129PROMSG    *LIBL     OPZ0156
414800960129PROMSG    *LIBL     OPZ0157
414900940201**
41500095110811S      <Movimenti
41510094083114S      <Gestione commenti
41520095110820S      <Particolarità
41530095070432S      <Mix
41540096012950S      <Taglia
41550096012951S      <Incolla
415600960126** TASTI DI FUNZIONE  D1
415700960126PROMSG    *LIBL     KEY0001  01
415800960126PROMSG    *LIBL     KEY0002  02
415900960126PROMSG    *LIBL     KEY0003  03
416000960126PROMSG    *LIBL     KEY0004  04
416100960126PROMSG    *LIBL     KEY0005  05
416200960126PROMSG    *LIBL     KEY0006  06
416300060104PROMSG    *LIBL     KEY0150  07
416400960126PROMSG    *LIBL     KEY0008  08
416500960126PROMSG    *LIBL     KEY0009  09
416600960126PROMSG    *LIBL     KEY0010  10
416700960126PROMSG    *LIBL     KEY0011  11
416800960126PROMSG    *LIBL     KEY0012  12
416900960126PROMSG    *LIBL     KEY0013  13
417000960126PROMSG    *LIBL     KEY0014  14
417100960126PROMSG    *LIBL     KEY0015  15
417200960126PROMSG    *LIBL     KEY0016  16
417300960126PROMSG    *LIBL     KEY0017  17
417400960126PROMSG    *LIBL     KEY0018  18
417500960126PROMSG    *LIBL     KEY0019  19
417600960126PROMSG    *LIBL     KEY0020  20
417700960126PROMSG    *LIBL     KEY0021  21
417800960126PROMSG    *LIBL     KEY0022  22
417900960126PROMSG    *LIBL     KEY0023  23
418000960126PROMSG    *LIBL     KEY0024  24
418100960126**  ABILITAZIONE D1
4182009601260N  01
4183009601261N  02
4184009601261N  03
4185009601261N  04
4186009601260N  05
4187009601260N  06
4188000601041N  07
4189009601261N  08
4190009601260N  09
4191009601260N  10
4192009601260N  11
4193009601261N  12
4194009601260N  13
4195009601260N  14
4196009601260N  15
4197009601260N  16
4198009601260N  17
4199009601261N  18
4200009601260N  19
4201009601260N  20
4202009601260N  21
4203009601260N  22
4204009601260N  23   Impostare a '1' se si utilizzano opzioni
4205009601261N  24   Lasciare sempre impostata a '1' il valore della riga 24
420600940916** TASTI DI FUNZIONE  S1
420700940916PROMSG    *LIBL     KEY0001  01
420800940916PROMSG    *LIBL     KEY0002  02
420900940916PROMSG    *LIBL     KEY0003  03
421000940916PROMSG    *LIBL     KEY0004  04
421100940916PROMSG    *LIBL     KEY0005  05
421200940916PROMSG    *LIBL     KEY0006  06
421300060104PROMSG    *LIBL     KEY0150  07
421400940916PROMSG    *LIBL     KEY0008  08
421500940916PROMSG    *LIBL     KEY0009  09
421600940916PROMSG    *LIBL     KEY0010  10
421700951109PROMSG    *LIBL     KEY0031  11
421800940916PROMSG    *LIBL     KEY0012  12
421900940916PROMSG    *LIBL     KEY0013  13
422000940916PROMSG    *LIBL     KEY0014  14
422100951109PROMSG    *LIBL     KEY0053  15
422200940916PROMSG    *LIBL     KEY0016  16
422300940916PROMSG    *LIBL     KEY0017  17
422400940916PROMSG    *LIBL     KEY0018  18
422500940916PROMSG    *LIBL     KEY0019  19
422600960109PROMSG    *LIBL     KEY0116  20
422700940916PROMSG    *LIBL     KEY0021  21
422800940916PROMSG    *LIBL     KEY0022  22
422900940916PROMSG    *LIBL     KEY0023  23
423000940916PROMSG    *LIBL     KEY0024  24
423100940916**  ABILITAZIONE S1
4232009409160N  01
4233009409160N  02
4234009409161N  03
4235009409160N  04
4236009409160N  05
4237009601261N  06
4238000601040N  07
4239009511201N  08
4240009511081N  09
4241009507040N  10
4242009511081N  11
4243009409161N  12
4244009409160N  13
4245009409160N  14
4246009511081N  15
4247009409160N  16
4248009601261N  17
4249009601291N  18
4250009409160N  19
4251009601260N  20
4252009409160N  21
4253009409160N  22
4254009509131N  23   Impostare a '1' se si utilizzano opzioni
4255009509131N  24   Lasciare sempre impostata a '1' il valore della riga 24
425600960126** TASTI DI FUNZIONE  D2
425700960126PROMSG    *LIBL     KEY0001  01
425800960126PROMSG    *LIBL     KEY0002  02
425900960126PROMSG    *LIBL     KEY0003  03
426000960126PROMSG    *LIBL     KEY0004  04
426100960126PROMSG    *LIBL     KEY0005  05
426200960126PROMSG    *LIBL     KEY0006  06
426300960126PROMSG    *LIBL     KEY0007  07
426400960126PROMSG    *LIBL     KEY0008  08
426500960126PROMSG    *LIBL     KEY0009  09
426600960126PROMSG    *LIBL     KEY0010  10
426700960126PROMSG    *LIBL     KEY0011  11
426800960126PROMSG    *LIBL     KEY0012  12
426900960126PROMSG    *LIBL     KEY0013  13
427000960126PROMSG    *LIBL     KEY0014  14
427100960126PROMSG    *LIBL     KEY0015  15
427200960126PROMSG    *LIBL     KEY0016  16
427300960126PROMSG    *LIBL     KEY0017  17
427400960126PROMSG    *LIBL     KEY0018  18
427500960126PROMSG    *LIBL     KEY0019  19
427600960126PROMSG    *LIBL     KEY0020  20
427700960126PROMSG    *LIBL     KEY0021  21
427800960126PROMSG    *LIBL     KEY0022  22
427900960126PROMSG    *LIBL     KEY0023  23
428000960126PROMSG    *LIBL     KEY0024  24
428100960126**  ABILITAZIONE D2
4282009601260N  01
4283009601260N  02
4284009601261N  03
4285009601260N  04
4286009601260N  05
4287009601260N  06
4288009601260N  07
4289009601260N  08
4290009601260N  09
4291009601260N  10
4292009601260N  11
4293009601261N  12
4294009601260N  13
4295009601260N  14
4296009601260N  15
4297009601260N  16
4298009601260N  17
4299009601260N  18
4300009601260N  19
4301009601260N  20
4302009601260N  21
4303009601260N  22
4304009601260N  23   Impostare a '1' se si utilizzano opzioni
4305009601261N  24   Lasciare sempre impostata a '1' il valore della riga 24
