000100171218     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200171218     H*PARMS OPTION(*NOXREF) CVTOPT(*NONE)
000300171218     H DECEDIT('0,') DATEDIT(*DMY.)
000400171218      *
000500171218      *          Regole per detrazione IVA a Credito
000600171218      *             Guida/lista
000700171218      *           --------------------------------
000800171218      *
000900171218R275  * Questo pgm è stato creato con il task R275 Slitt. data liquidazion IVA
001000171218R275  * Decreto Legge 50/2017
001100171218      *
001200171218      *
001300171218     F*----------------------------------------------------*
001400171218     F* Regole per detrazione IVA a Credito
001500171218     F* Key: Soc/Reg/Libro/DtInVal Unique
001600171218     FSDGRIC01L IF   E           K DISK
001700171218     F*------------
001800171218     F* Anagrafico libri IVA
001900171218     F* Key: FSC/TRI/LIB
002000171218     FANDLI01L  IF   E           K DISK
002100171218     F*------------
002200171218     FSDGSI30D  CF   E             WORKSTN
002300171218     F                                     RENAME(SI30D1:FMTD1)
002400171218     F                                     RENAME(SI30D2:FMTD2)
002500171218     F                                     RENAME(SI30D3:FMTD3)
002600171218     F                                     RENAME(SI30S1:FMTS1)
002700171218     F                                     RENAME(SI30C1:FMTC1)
002800171218     F                                     RENAME(SI30T1:FMTT1)
002900171218     F                                     RENAME(SI30Z1:FMTZ1)
003000171218     F                                     RENAME(SI30Z2:FMTZ2)
003100171218     F                                     RENAME(SI30Z3:FMTZ3)
003200171218     F                                     SFILE(FMTS1:S1NRR)
003300171218     F                                     INFDS(DSFMT)
003400171218     D*----------------------------------------------------*
003500171218     D* Schiere per la gestione delle opzioni
003600171218     D*
003700171218     D $MS1            S             27    DIM(6) CTDATA PERRCD(1)
003800171218     D $TX1            S             50    DIM(6) ALT($MS1)
003900171218     D $OP1            S              2  0 DIM(6) CTDATA PERRCD(1)
004000171218     D $FL1            S              1    DIM(6) ALT($OP1)
004100171218     D $TX             S             50    DIM(50)
004200171218     D $FL             S              1    DIM(50)
004300171218     D* Schiere generali per la gestione dei tasti di funzione
004400171218     D $FM             S             27    DIM(24)
004500171218     D $FX             S             50    DIM(24)
004600171218     D $FC             S              1    DIM(24)
004700171218     D $FR             S              1    DIM(24)
004800171218     D* Schiere per la gestione dei tasti di funzione
004900171218     D* Video D1
005000171218     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005100171218     D $FX1            S             50    DIM(24) ALT($FM1)
005200171218     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005300171218     D $FR1            S              1    DIM(24) ALT($FC1)
005400171218     D* Schiere per la gestione dei tasti di funzione
005500171218     D* Video S1
005600171218     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005700171218     D $FX2            S             50    DIM(24) ALT($FM2)
005800171218     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
005900171218     D $FR2            S              1    DIM(24) ALT($FC2)
006000171218     D* Schiere per la gestione dei tasti di funzione
006100171218     D* Video D2
006200171218     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
006300171218     D $FX3            S             50    DIM(24) ALT($FM3)
006400171218     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
006500171218     D $FR3            S              1    DIM(24) ALT($FC3)
006600171218     D*-------------
006700171218     D* Valori assunti
006800171218     D XMPEDS        E DS                  INZ
006900171218     D XMPEDS1       E DS                  INZ
007000171218     D DSMPE           DS                  INZ
007100171218     D  $LOP                   1      1  0
007200171218     D  $ORD                   2      3  0
007300171218     D  $TpReg                 4      4
007400171218     D  $Libro                 5      7
007500171218     D  $TpRIC                 8      8
007600171218     D*-------------
007700171218     D* Abilitazione da capofila
007800171218     D A066DS        E DS                  EXTNAME(ANA066DS) INZ
007900171218     D  RET066       E                     INZ('0')
008000171218     D  OPR066       E                     INZ('0')
008100171218     D  ERR066       E                     INZ('0')
008200171218     D*-------------
008300171218     D* Passaggio Parametri
008400171218     D KPJBA         E DS
008500171218     D*-------------
008600171218     D* Reperimento Autorizzazioni
008700171218     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
008800171218     D*-------------
008900171218     D* Passaggio Autorizzazioni
009000171218     D DSAUT           DS           512
009100171218     D*-------------
009200171218     D* Maschera dati autorizzazioni
009300171218     D ANGIVADS      E DS
009400171218     D*-------------
009500171218     D* Dati di ambiente ottenuti da XSOC
009600171218     D SOC001        E DS                  EXTNAME(XSOC001DS)
009700171218     D*-------------
009800171218     D* DS Interna per dati di output di XSOC
009900171218     D XSOCDS          DS          1000
010000171218     D*-------------
010100171218     D* Parametri gestione anagrafica
010200171218     D ANP045DS      E DS
010300171218     D*-------------
010400171218     D* Parametri in ricezione
010500171218     D SDGSI30DS     E DS
010600171218     D*-------------
010700171218     D* Parametri del PGM di manutenzione SDGSI31R
010800171218     D SDGSI31DS     E DS                  INZ
010900171218     D  RETI31       E                     INZ('0')
011000171218     D  OPRI31       E                     INZ('0')
011100171218     D  ERRI31       E                     INZ('0')
011200171218     D*-------------
011300171218     D DSFMT           DS           512
011400171218     D  $TASTO               369    369
011500171218     D  NRG                  370    370
011600171218     D  NCL                  371    371
011700171218     D  SFLNRR               378    379B 0
011800171218     D* Ricerca legende
011900171218     D ANA964DS      E DS                  INZ
012000171218     D* Controlli validità legende
012100171218     D X07VALDS      E DS                  INZ
012200171218     D*-------------
012300171218     D* posizione cursore
012400171218     D CURSOR          DS
012500171218     D  RIRI                   1      2B 0 INZ
012600171218     D  R$$                    2      2
012700171218     D  COCO                   3      4B 0 INZ
012800171218     D  C$$                    4      4
012900171218     D*-------------
013000171218     D* Reperimento nome PGM
013100171218     D STATUS         SDS           333
013200171218     D  DSPGM            *PROC
013300171218     D  PARMS            *PARMS
013400171218     D*-------------
013500171218     D XPAOUT          DS          2000
013600171218     D*-------------
013700171218     D* Costanti
013800171218     D* nr.di righe del sfl (SFLPAG)
013900171218     D SFLPAG          C                   CONST(10)
014000171218     D* dimensione della schiera $MS1
014100171218     D $DIMOP          C                   CONST(6)
014200171218     D*-------------
014300171218     D* Tasti di funzione
014400171218     D F01             C                   CONST(X'31')
014500171218     D F02             C                   CONST(X'32')
014600171218     D F03             C                   CONST(X'33')
014700171218     D F04             C                   CONST(X'34')
014800171218     D F05             C                   CONST(X'35')
014900171218     D F06             C                   CONST(X'36')
015000171218     D F07             C                   CONST(X'37')
015100171218     D F08             C                   CONST(X'38')
015200171218     D F09             C                   CONST(X'39')
015300171218     D F10             C                   CONST(X'3A')
015400171218     D F11             C                   CONST(X'3B')
015500171218     D F12             C                   CONST(X'3C')
015600171218     D F13             C                   CONST(X'B1')
015700171218     D F14             C                   CONST(X'B2')
015800171218     D F15             C                   CONST(X'B3')
015900171218     D F16             C                   CONST(X'B4')
016000171218     D F17             C                   CONST(X'B5')
016100171218     D F18             C                   CONST(X'B6')
016200171218     D F19             C                   CONST(X'B7')
016300171218     D F20             C                   CONST(X'B8')
016400171218     D F21             C                   CONST(X'B9')
016500171218     D F22             C                   CONST(X'BA')
016600171218     D F23             C                   CONST(X'BB')
016700171218     D F24             C                   CONST(X'BC')
016800171218     D ENTER           C                   CONST(X'F1')
016900171218     D ROLDWN          C                   CONST(X'F4')
017000171218     D ROLLUP          C                   CONST(X'F5')
017100171218     D*-------------
017200171218     D* Attributi video
017300171218     D* (definisco come costanti qugli attributi video che potrei
017400171218     D*  testare e quindi preferisco assegnargli un valore "parlante")
017500171218     D*  bottone presente
017600171218     D PRES            C                   CONST(X'A1')
017700171218     D*  cliente abilitato in società di riferimento
017800171218     D ABIL            C                   CONST(X'A2')
017900171218     D*-------------
018000171218     D ZDateISO        S               D
018100171218     D*-------------
018200171218     D* Attributi video
018300171218     D Normale         C                   Const(X'32')
018400171218     D Protetto        C                   Const(X'A0')
018500171218     C*----------------------------------------------------*
018600171218     C*                MAIN LINE PROGRAM
018700171218     C*----------------------------------------------------*
018800171218     C*
018900171218     C* inizializzazione variabili
019000171218     C                   EXSR      INZVAR
019100171218     C*
019200171218     C     $FINE         DOWEQ     *OFF
019300171218     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
019400171218     C     $GEST         CASEQ     'S1'          GESS1                          Lista
019500171218     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
019600171218     C                   ENDCS
019700171218     C                   ENDDO
019800171218     C* fine programma
019900171218     C                   EXSR      ENDPGM
020000171218     C*
020100171218     C/EJECT
020200171218     C************************************************************
020300171218     C* FINE PROGRAMMA
020400171218     C************************************************************
020500171218     C     ENDPGM        BEGSR
020600171218     C*
020700171218     C* passaggio dati al pgm chiamante
020800171218     C                   MOVEL     WOPR          OPRI30                                     ne S1
020900171218     C                   MOVEL     SDGSI30DS     KPJBU
021000171218     C*
021100171218     C* Se pgm Guida non è stato richiamato posso chiudere in LR
021200171218     C     OPZI30        IFEQ      *BLANK
021300171218     C                   SETON                                            LR
021400171218     C* altrimenti chiudo in RT
021500171218     C                   ELSE
021600171218     C                   SETON                                            RT
021700171218     C                   ENDIF
021800171218     C                   RETURN
021900171218     C*
022000171218     C                   ENDSR
022100171218     C/EJECT
022200171218     C************************************************************
022300171218     C* RICERCHE
022400171218     C************************************************************
022500171218     C     SEARCH        BEGSR
022600171218     C*
022700171218     C* determino Riga/Colonna del cursore
022800171218     C                   MOVE      NRG           R$$
022900171218     C                   MOVE      NCL           C$$
023000171218     C                   Z-ADD     RIRI          CURR
023100171218     C                   Z-ADD     COCO          CURC
023200171218     C*
023300171218   B1C                   SELECT
023400171218     C*
023500171218     C*
023600171219     C* LEGENDA TIPO REGISTRO
023700171218     C     H1NMFL        WHENEQ    'D1TPREG'
023800171218     C     H1NMFL        OREQ      'D2TPREG'
023900171218     C                   EXSR      F4_TpReg
024000171219     C*
024100171219     C*
024200171219     C* LEGENDA TIPO regola
024300171219     C     H1NMFL        WHENEQ    'D1TPRIC'
024400171219     C     H1NMFL        OREQ      'D2TPRIC'
024500171219     C                   EXSR      F4_TpRIC
024600171218     C*
024700171218     C* ricerca non abilitata in questa posizione
024800171218   X1C                   OTHER
024900171218     C                   SETON                                        9899
025000171218     C*
025100171218   E1C                   ENDSL
025200171218     C*
025300171218     C* imposto pos. del cursore
025400171218     C                   Z-ADD     CURR          H1RIGA
025500171218     C                   Z-ADD     CURC          H1COLO
025600171218     C                   MOVE      *HIVAL        $LASTV
025700171218     C*
025800171218     C                   ENDSR
025900171218     C/EJECT
026000171218     C************************************************************
026100171219     C* Ricerca tipo registro IVA
026200171218     C************************************************************
026300171218     C     F4_TpReg      BEGSR
026400171218     C*
026500171218     C                   Reset                   ANA964DS
026600171218     C*
026700171218     C                   MOVEL     '01'          OPZ964
026800171218     C                   MOVEL     'ANDLI000'    TRC964
026900171218     C                   MOVEL     'DLITPREG'    CAM964
027000171218     C                   MoveL     *On           All964
027100171218     C*
027200171218     C                   MoveL     ANA964DS      KPJBU
027300171218     C                   CALL      'ANA964R'
027400171218     C                   PARM                    KPJBA
027500171218     C                   MoveL     KPJBU         ANA964DS
027600171218     C*
027700171218     C                   if        Ret964     <> '2'
027800171218     C*
027900171218     C                   Select
028000171218     C                   When      H1NMFL      = 'D1TPREG'
028100171218     C                   MoveL     Val964        D1TpReg
028200171219     C                   MoveL     Des964        D1TpRegD
028300171218     C                   When      H1NMFL      = 'D2TPREG'
028400171218     C                   MoveL     Val964        D2TpReg
028500171218     C                   endSl
028600171218     C*
028700171218     C                   endif
028800171218     C*
028900171218     C                   ENDSR
029000171218     C/EJECT
029100171219     C************************************************************
029200171219     C* Ricerca tipo regola / periodo
029300171219     C************************************************************
029400171219     C     F4_TpRIC      BEGSR
029500171219     C*
029600171219     C                   Reset                   ANA964DS
029700171219     C*
029800171219     C                   MOVEL     '01'          OPZ964
029900171219     C                   MOVEL     'SDGSI30S'    TRC964
030000171219     C                   MOVEL     'TPRICI30'    CAM964
030100171219     C*
030200171219     C                   Select
030300171219     C                   When      H1NMFL      = 'D1TPRIC'
030400171219     C                   MoveL     *On           All964
030500171219     C                   When      H1NMFL      = 'D2TPRIC'
030600171219     C                   MoveL     *Off          All964
030700171219     C                   endSl
030800171219     C*
030900171219     C*
031000171219     C                   MoveL     ANA964DS      KPJBU
031100171219     C                   CALL      'ANA964R'
031200171219     C                   PARM                    KPJBA
031300171219     C                   MoveL     KPJBU         ANA964DS
031400171219     C*
031500171219     C                   if        Ret964     <> '2'
031600171219     C*
031700171219     C                   Select
031800171219     C                   When      H1NMFL      = 'D1TPRIC'
031900171219     C                   MoveL     Val964        D1TpRIC
032000171219     C                   MoveL     Des964        D1TpRICD
032100171219     C                   When      H1NMFL      = 'D2TPRIC'
032200171219     C                   MoveL     Val964        D2TpRIC
032300171219     C                   endSl
032400171219     C*
032500171219     C                   endif
032600171219     C*
032700171219     C                   ENDSR
032800171219     C/EJECT
032900171218     C************************************************************
033000171218     C* RIEMPIMENTO OPZIONI
033100171218     C************************************************************
033200171218     C     RIEOPZ        BEGSR
033300171218     C*
033400171218     C* Input :
033500171218     C* - $TX = schiera con tutti i testi disponibili
033600171218     C* - $FL = schiere con i flag di validità associati ai testi
033700171218     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
033800171218     C*          opzione usato
033900171218     C* - WMXO = variabile contenente il numero massimo di testi
034000171218     C*          opzione usabili
034100171218     C* - $L01 = Lunghezza prima riga
034200171218     C* - $L02 = Lunghezza seconda riga
034300171218     C* Output :
034400171218     C* - WOPZ1 = variabile contenente la prima riga opzioni
034500171218     C* - WOPZ2 = variabile contenente la seconda riga opzioni
034600171218     C* - FF23  = flag che indica se c'è bisogno del tasto F23
034700171218     C*
034800171218     C                   CALLb     'X04OPMSG'
034900171218     C* input
035000171218     C                   PARM                    $TX
035100171218     C                   PARM                    $FL
035200171218     C                   PARM                    WCOZ              3 0
035300171218     C                   PARM                    WMXO              3 0
035400171218     C* output
035500171218     C                   PARM                    WOPZ1            76
035600171218     C                   PARM                    WOPZ2            76
035700171218     C                   PARM                    FF23              1
035800171218     C* input
035900171218    >C                   PARM                    $L01              3 0
036000171218    >C                   PARM                    $L02              3 0
036100171218     C*
036200171218     C                   PARM                    $ALLOPT         960
036300171218     C*
036400171218     C                   ENDSR
036500171218     C/EJECT
036600171218     C************************************************************
036700171218     C* RIEMPIMENTO TASTI FUNZIONE
036800171218     C************************************************************
036900171218     C     RIEKEY        BEGSR
037000171218     C*
037100171218     C* Input :
037200171218     C* - $FX = schiera con tutti i testi disponibili
037300171218     C* - $FC = schiere con i flag di validità associati ai testi
037400171218     C* - $FR = schiere con i flag di REVERSE IMAGE
037500171218     C* - $ULK = ultimo testo caricato
037600171218     C* - $L01 = Lunghezza prima riga
037700171218     C* - $L02 = Lunghezza seconda riga
037800171218     C* Output :
037900171218     C* - $ULK
038000171218     C* - $KEY1= 1° riga di tasti funzione
038100171218     C* - $KEY2= 2° riga di tasti funzione
038200171218     C*
038300171218     C                   CALLb     'XKEYMSG'
038400171218     C* input
038500171218     C                   PARM                    $FX
038600171218     C                   PARM                    $FC
038700171218     C                   PARM                    $FR
038800171218     C                   PARM                    $ULK              3 0
038900171218     C* output
039000171218     C                   PARM                    $KEY1            79
039100171218     C                   PARM                    $KEY2            79
039200171218     C* input
039300171218    >C                   PARM                    $L01              3 0
039400171218    >C                   PARM                    $L02              3 0
039500171218     C*
039600171218     C                   PARM                    $ALLFUNCT       480
039700171218     C*
039800171218     C                   ENDSR
039900171218     C/EJECT
040000171218     C************************************************************
040100171218     C* GESTIONE EMISSIONE TASTI FUNZIONE
040200171218     C************************************************************
040300171218     C     GESPIE        BEGSR
040400171218     C*
040500171218     C                   SELECT
040600171218     C* Parzializzazioni
040700171218     C     $GEST         WHENEQ    'D1'
040800171218     C     $ULKD1        IFEQ      0
040900171218     C* riempimento opzioni
041000171218     C                   EXSR      F24D1
041100171218     C                   ELSE
041200171218     C                   WRITE     FMTZ1
041300171218     C                   ENDIF
041400171218     C* Lista
041500171218     C     $GEST         WHENEQ    'S1'
041600171218     C     $ULKS1        IFEQ      0
041700171218     C                   EXSR      F24S1
041800171218     C                   ELSE
041900171218     C                   WRITE     FMTZ2
042000171218     C                   ENDIF
042100171218     C* Modifica valori assunti
042200171218     C     $GEST         WHENEQ    'D2'
042300171218     C     $ULKD2        IFEQ      0
042400171218     C                   EXSR      F24D2
042500171218     C                   ELSE
042600171218     C                   WRITE     FMTZ3
042700171218     C                   ENDIF
042800171218     C                   ENDSL
042900171218     C*
043000171218     C                   ENDSR
043100171218     C/EJECT
043200171218     C************************************************************
043300171218     C* GESTIONE PARZIALIZZAZIONI
043400171218     C************************************************************
043500171218     C     GESD1         BEGSR
043600171218     C* inizializzazione videata
043700171218     C     $INZD1        IFEQ      *ON
043800171218     C                   EXSR      INZD1
043900171218     C                   MOVE      *OFF          $INZD1
044000171218     C                   ENDIF
044100171218     C* emissione piede videata se proveniente da altra
044200171218     C* salvataggio valore formato di provenienza
044300171218     C     $LASTV        IFNE      'D1'
044400171218     C* salvo stato F17 per sapere cosa fare in caso di F12
044500171218     C                   MOVE      *INKR         WINKR
044600171218     C                   WRITE     FMTT1
044700171218     C                   EXSR      GESPIE
044800171218     C                   MOVE      $LASTV        $LASTG
044900171218     C                   MOVE      'D1'          $LASTV
045000171218     C                   ENDIF
045100171218     C*
045200171218     C* se esistono errori sulla videata
045300171218     C* emetto la write del formato a indicatori spenti per vedere
045400171218     C* le eventuali decodifiche
045500171218     C     *IN99         IFEQ      *ON
045600171218     C                   MOVEA     *IN           WIN
045700171218     C                   MOVE      *ALL'0'       IN5098           49
045800171218     C                   MOVEA     IN5098        *IN(50)
045900171218     C                   WRITE     FMTD1
046000171218     C                   MOVEA     WIN           *IN
046100171218     C                   ENDIF
046200171218     C*
046300171218     C                   EXFMT     FMTD1
046400171218     C*
046500171218     C* controllo abilitazione tasti funzione
046600171218     C                   EXSR      CTRKD1
046700171218     C*
046800171218     C     *IN99         IFEQ      *OFF
046900171218     C*
047000171218     C                   SELECT
047100171218     C* F2=Decodifica
047200171218     C     $TASTO        WHENEQ    F02
047300171218     C                   EXSR      F02D1
047400171218     C* F3=Fine
047500171218     C     $TASTO        WHENEQ    F03
047600171218     C                   EXSR      F03D1
047700171218     C* F4=Ricerca
047800171218     C     $TASTO        WHENEQ    F04
047900171218     C                   EXSR      SEARCH
048000171218     C* F5=Refresh
048100171218     C     $TASTO        WHENEQ    F05
048200171218     C                   EXSR      F05D1
048300171218     C* F8=Successivo
048400171218     C     $TASTO        WHENEQ    F08
048500171218     C                   EXSR      F08D1
048600171218     C* F10=Immissione
048700171218     C     $TASTO        WHENEQ    F10
048800171218     C                   EXSR      F10D1
048900171218     C* F12=Ritorno
049000171218     C     $TASTO        WHENEQ    F12
049100171218     C                   EXSR      F12D1
049200171218     C* F18=Modifica valori assunti
049300171218     C     $TASTO        WHENEQ    F18
049400171218     C                   EXSR      F18D1
049500171218     C* F24=Altri tasti
049600171218     C     $TASTO        WHENEQ    F24
049700171218     C                   EXSR      F24D1
049800171218     C*
049900171218     C                   OTHER
050000171218     C                   EXSR      CTRD1
050100171218     C     *IN99         IFEQ      *OFF
050200171218     C* definisco l'ordinamento
050300171218     C                   EXSR      DEFORD
050400171218     C* richiamo la lista
050500171218     C                   MOVE      'S1'          $GEST
050600171218     C                   MOVE      *ON           $INZS1
050700171218     C                   MOVE      *BLANK        W1POS
050800171218     C                   ENDIF
050900171218     C*
051000171218     C                   ENDSL
051100171218     C*
051200171218     C                   ENDIF
051300171218     C                   ENDSR
051400171218     C/EJECT
051500171218     C************************************************************
051600171218     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
051700171218     C************************************************************
051800171218     C     INZD1         BEGSR
051900171218     C*
052000171218     C                   CLEAR                   FMTD1
052100171218     C* Se pgm  richiamato con parametri forniti , li imposto nelle
052200171218     C*    parzializzazioni
052300171218     C                   SELECT
052400171218     C* il pgm è richiamato e
052500171218     C* sono stati passati dei valori di parzializzazione ma non
052600171218     C* la chiave univoca
052700171218     C     OPZI30        WHENNE    *BLANK
052800171218     C* imposto i valori passati sui campi a video
052900171218     C                   MOVE      TpRegI30      D1TpReg
053000171218     C                   MOVE      LibroI30      D1Libro
053100171218     C                   MOVE      TpRICI30      D1TpRIC
053200171218     C* se passata la richiesta di gestione differenziata di
053300171218     C* società o capofila, imposto il flag a video col valore in
053400171218     C* ingresso
053500171218     C     FSCI30        IFNE      *BLANK
053600171218     C                   MOVEL     FSCI30        D1FSC
053700171218     C* altrimenti, come default, gestisco la società di riferimento
053800171218     C                   ELSE
053900171218     C                   MOVEL     'S'           D1FSC
054000171218     C                   ENDIF
054100171218     C* il pgm non è richiamato
054200171218     C     OPZI30        WHENEQ    *BLANK
054300171218     C* imposto dei valori di default
054400171218     C                   MOVE      *BLANK        D1TpReg
054500171218     C                   MOVE      *BLANK        D1Libro
054600171218     C                   MOVE      *BLANK        D1TpRIC
054700171218     C                   MOVEL     'S'           D1FSC
054800171218     C                   ENDSL
054900171218     C*
055000171218     C* se siamo in logica di gruppo sia a livello aziendale
055100171218     C     XSCSOC        IFNE      XSCCPA
055200171218     C* che per la singola procedura anagrafica
055300171218     C     GSP045        ANDEQ     'G'
055400171218     C* ma non siamo in ricerca
055500171218     C     OPZI30        ANDNE     '01'
055600171218     C* abilito la scelta alla società capofila
055700171218     C                   SETON                                        09
055800171218     C                   ENDIF
055900171218     C*
056000171218     C*
056100171218     C                   ENDSR
056200171218     C/EJECT
056300171218     C************************************************************
056400171218     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
056500171218     C* INPUT
056600171218     C* $FC1 = Abilitazioni videata D1
056700171218     C* OUTPUT
056800171218     C* *IN97 *IN99 Errori
056900171218     C************************************************************
057000171218     C     CTRKD1        BEGSR
057100171218     C*
057200171218     C                   SETOFF                                       99
057300171218     C                   MOVEA     $FC1          $FC
057400171218     C                   EXSR      CTRKEY
057500171218     C     *IN99         IFEQ      *ON
057600171218     C                   SETON                                        97
057700171218     C                   ENDIF
057800171218     C*
057900171218     C                   ENDSR
058000171218     C/EJECT
058100171218     C************************************************************
058200171218     C* GESTIONE F02 VIDEO D1
058300171218     C************************************************************
058400171218     C     F02D1         BEGSR
058500171218     C*
058600171218     C* per decodificare la videata richiamo la routine dei ctrl
058700171218     C* e poi disattivo gli eventuali messaggi di errore
058800171218     C                   EXSR      CTRD1
058900171218     C*
059000171218     C                   MOVE      *ALL'0'       IN5098
059100171218     C                   MOVEA     IN5098        *IN(50)
059200171218     C*
059300171218     C                   ENDSR
059400171218     C/EJECT
059500171218     C************************************************************
059600171218     C* GESTIONE F03 VIDEO D1
059700171218     C************************************************************
059800171218     C     F03D1         BEGSR
059900171218     C*
060000171218     C                   MOVE      *ON           $FINE
060100171218     C                   MOVE      '1'           RETI30
060200171218     C* fine programma
060300171218     C                   EXSR      ENDPGM
060400171218     C*
060500171218     C                   ENDSR
060600171218     C/EJECT
060700171218     C************************************************************
060800171218     C* GESTIONE F05 VIDEO D1
060900171218     C************************************************************
061000171218     C     F05D1         BEGSR
061100171218     C*
061200171218     C                   MOVE      *ON           $INZD1
061300171218     C*
061400171218     C                   ENDSR
061500171218     C/EJECT
061600171218     C************************************************************
061700171218     C* GESTIONE F08 VIDEO D1
061800171218     C************************************************************
061900171218     C     F08D1         BEGSR
062000171218     C*
062100171218     C                   MOVE      *ON           $FINE
062200171218     C                   MOVE      '0'           RETI30
062300171218     C*
062400171218     C                   ENDSR
062500171218     C/EJECT
062600171218     C************************************************************
062700171218     C* GESTIONE F10 VIDEO D1
062800171218     C************************************************************
062900171218     C     F10D1         BEGSR
063000171218     C*
063100171218     C                   RESET                   SDGSI31DS
063200171218     C                   MOVEL     '01'          OPZI31
063300171218     C* passare anche le parzializzazioni ricevute relative alla chiave
063400171218     C* univoca : se ce ne sono saranno usate come default d'immissione
063500171218     C                   MOVEL     D1FSC         FSCI31
063600171218     C                   MOVEL     TpRegI30      TpRegI31
063700171218     C                   MOVEL     LibroI30      LibroI31
063800171218     C                   MOVEL     TpRICI30      TpRICI31
063900171218     C* codice di riferimento per le valorizzazioni di default
064000171218     C                   MOVE      $TpReg        DF1I31
064100171218     C                   MOVE      $Libro        DF2I31
064200171218     C                   MOVEL     SDGSI31DS     KPJBU
064300171218     C                   CALL      'SDGSI31R'
064400171218     C                   PARM                    KPJBA
064500171218     C                   MOVEL     KPJBU         SDGSI31DS
064600171218     C* ritorno da PGM gestione
064700171218     C                   EXSR      RTNI31
064800171218     C*
064900171218     C                   MOVE      *HIVAL        $LASTV
065000171218     C*
065100171218     C                   ENDSR
065200171218     C/EJECT
065300171218     C************************************************************
065400171218     C* GESTIONE F12 VIDEO D1
065500171218     C************************************************************
065600171218     C     F12D1         BEGSR
065700171218     C*
065800171218     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
065900171218     C*  per F12 torno alla lista
066000171218     C     $LOP          IFEQ      2
066100171218     C     WINKR         OREQ      *ON
066200171218     C                   MOVE      'S1'          $GEST
066300171218     C*  altrimenti, per F12 torno a menu
066400171218     C                   ELSE
066500171218     C                   MOVE      *ON           $FINE
066600171218    >C                   MOVE      '2'           RETI30
066700171218     C                   ENDIF
066800171218     C*
066900171218     C                   ENDSR
067000171218     C/EJECT
067100171218     C************************************************************
067200171218     C* GESTIONE F18 VIDEO D1
067300171218     C************************************************************
067400171218     C     F18D1         BEGSR
067500171218     C*
067600171218     C                   MOVE      'D2'          $GEST
067700171218     C                   MOVEL     *ON           $INZD2
067800171218     C*
067900171218     C                   ENDSR
068000171218     C/EJECT
068100171218     C************************************************************
068200171218     C* GESTIONE F24 VIDEO D1
068300171218     C* input
068400171218     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
068500171218     C* $FX1 =Testi tasti funzione
068600171218     C* $FC1 = Abilitazioni
068700171218     C* $FR1 = Inversioni di fondo
068800171218     C************************************************************
068900171218     C     F24D1         BEGSR
069000171218     C*
069100171218     C                   MOVEA     $FX1          $FX
069200171218     C                   MOVEA     $FC1          $FC
069300171218     C                   MOVEA     $FR1          $FR
069400171218     C                   Z-ADD     $ULKD1        $ULK
069500171218    >C                   Z-ADD     79            $L01
069600171218    >C                   Z-ADD     79            $L02
069700171218     C                   EXSR      RIEKEY
069800171218     C                   Z-ADD     $ULK          $ULKD1            3 0
069900171218     C                   MOVEA     $FC           $FC1
070000171218     C                   MOVEL     $KEY1         Z1KE1
070100171218     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
070200171218     C                   MOVEL     $KEY2         Z1KE2
070300171218     C                   WRITE     FMTZ1
070400171218     C*
070500171218     C                   ENDSR
070600171218     C/EJECT
070700171218     C************************************************************
070800171218     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
070900171218     C************************************************************
071000171218     C     CTRD1         BEGSR
071100171218     C*
071200171218     C                   SETOFF                                       99
071300171218     C* non imposto pos. del cursore
071400171218     C                   Z-ADD     0             H1RIGA
071500171218     C                   Z-ADD     0             H1COLO
071600171218     C*
071700171218     C*  elencare gli eventuali controlli sui campi di
071800171218     C*  parzializzazione, ricordando che quasi sempre essi
071900171218     C*  non ne avranno perchè determinano solo il range di
072000171218     C*  scrittura della lista e quindi (nel caso di campi
072100171218     C*  alfanumerici) saranno trattati come chiavi parziali
072200171219     C*****D1FSC         IFNE      WFSC
072300171218     C*
072400171218     C* Imposto campo WWWSOC
072500171218     C*
072600171218     C     D1FSC         IFEQ      'S'
072700171218     C                   MOVE      XSCSOC        WWWSOC
072800171218     C                   ELSE
072900171218     C                   MOVE      XSCCPA        WWWSOC
073000171218     C                   ENDIF
073100171218     C*
073200171218     C* la 1a volta non serve perchè è già stata fatta dalla routine
073300171218     C* INZVAR, le successive invece serve perchè bisogna riportare
073400171218     C* tutte le schiere al loro valore iniziale
073500171218     C     WFSC          IFNE      *BLANK
073600171218     C                   EXSR      DEFOPZ
073700171218     C                   ENDIF
073800171218     C*
073900171218     C* Ulteriore definizione delle opzioni utilizzabili a pgm
074000171218     C                   EXSR      ULTOPZ
074100171218     C                   MOVE      D1FSC         WFSC
074200171218     C*
074300171219     C                   EXSR      CtrD1TpReg
074400171218     C                   EXSR      CtrD1Libro
074500171219     C                   EXSR      CtrD1TpRIC
074600171218     C*
074700171219     C******             ENDIF
074800171218     C*
074900171218     C                   ENDSR
075000171218     C/EJECT
075100171219     C************************************************************
075200171219     C* CONTROLLO tipo registro IVA
075300171219     C************************************************************
075400171219     C     CtrD1TpReg    BEGSR
075500171219     C*
075600171219     C                   Clear                   D1TpRegD
075700171219     C*
075800171219     C     D1TpReg       IFNE      *BLANK
075900171219     C*
076000171219     C                   RESET                   X07VALDS
076100171219     C                   MOVE      '1'           X07RIC
076200171219     C                   MOVEL     'ANDLI000'    X07TRC
076300171219     C                   MOVEL     'DLITPREG'    X07CAM
076400171219     C                   MOVEL     D1TpReg       X07VAL
076500171219     C                   CALLb     'X07VALR'
076600171219     C                   PARM                    X07VALDS
076700171219     C*
076800171219   B3C     X07ERR        IFNE      '0'
076900171219     C                   SETON                                          5099
077000171219     C                   else
077100171219     C                   Eval      D1TpRegD    = X07Des
077200171219   E3C                   ENDIF
077300171219     C*
077400171219   E3C                   ENDIF
077500171219     C*
077600171219     C                   ENDSR
077700171219     C/EJECT
077800171218     C************************************************************
077900171218     C* Controllo Libro
078000171218     C************************************************************
078100171218     C     CtrD1Libro    BEGSR
078200171218     C*
078300171219     C                   Clear                   D1LibroD
078400171219     C*
078500171218     C     D1Libro       IFNE      *BLANK
078600171218     C                   CALLb     'XALLINEA'
078700171218     C                   PARM                    D1Libro
078800171218     C                   PARM      3             LEN               5 0
078900171218     C                   PARM                    XSCINP
079000171218     C                   ENDIF
079100171219     C*
079200171219     C* CTRL ESISTENZA registro/LIBRO IN ANAGRAFICA LIBRI IVA
079300171219     C     D1Libro       IFNE      *BLANK
079400171219     C     D1TpReg       andne     *BLANK
079500171219     C                   MOVE      WWWSOC        DLISOCIETA
079600171219     C                   MOVE      D1TpReg       DLITpReg
079700171219     C                   MOVE      D1Libro       DLILibro
079800171219     C     K03DLI01      CHAIN     ANDLI01L                           21
079900171219     C                   IF        *IN21=*OFF
080000171219     C                   MOVEL     DLIDESBREV    D1LibroD
080100171219     c                   ELSE
080200171219     c                   SETON                                        5199
080300171219     C                   ENDIF
080400171219     C                   ENDIF
080500171218     C*
080600171218     C                   ENDSR
080700171218     C/EJECT
080800171219     C************************************************************
080900171219     C* CONTROLLO tipo regola/periodo
081000171219     C************************************************************
081100171219     C     CtrD1TpRIC    BEGSR
081200171219     C*
081300171219     C                   Clear                   D1TpRICD
081400171219     C*
081500171219     C                   RESET                   X07VALDS
081600171219     C                   MOVE      '1'           X07RIC
081700171219     C                   MOVEL     'SDGSI30S'    X07TRC
081800171219     C                   MOVEL     'TPRICI30'    X07CAM
081900171219     C                   MOVEL     D1TpRIC       X07VAL
082000171219     C                   MOVE      *On           X07ALL
082100171219     C                   CALLb     'X07VALR'
082200171219     C                   PARM                    X07VALDS
082300171219     C*
082400171219   B3C     X07ERR        IFNE      '0'
082500171219     C                   SETON                                          5299
082600171219     C                   else
082700171219     C                   Eval      D1TpRICD    = X07Des
082800171219   E3C                   ENDIF
082900171219     C*
083000171219     C                   ENDSR
083100171219     C/EJECT
083200171218     C************************************************************
083300171218     C* DEFINIZIONE DELL'ORDINAMENTO
083400171218     C************************************************************
083500171218     C     DEFORD        BEGSR
083600171218     C*
083700171218     C* definisco l'ordinamento della lista a seconda dei valori
083800171218     C* impostati nel video di parzializzazione
083900171218     C*
084000171218     C                   SELECT
084100171218     C*  nel caso di più viste logiche e quindi più campi di
084200171218     C*  parzializzazione, qui vanno definiti i criteri per
084300171218     C*  determinare quale VL va letta, a seconda di quali campi
084400171218     C*  sono stati valorizzati
084500171218     C     D1TpReg       WHENNE    *BLANK
084600171218     C                   Z-ADD     1             $ORD
084700171218     C     D1Libro       WHENNE    *BLANK
084800171218     C                   Z-ADD     1             $ORD
084900171218     C* imposto l'ordinamento 99 per ottenere in selezione solo il
085000171218     C* rcd con la chiave passata
085100171218     C     $UNIQ         WHENEQ    *ON
085200171218     C                   Z-ADD     99            $ORD
085300171218     C* nel caso non sia stato digitato alcun valore in parzializzazione
085400171218     C* utilizzerò l'ordinamento precedentemente memorizzato
085500171218     C* (temporaneamente o definitivamente) nei valori assunti
085600171218     C                   OTHER
085700171218     C                   ENDSL
085800171218     C*
085900171218     C                   ENDSR
086000171218     C/EJECT
086100171218     C************************************************************
086200171218     C* GESTIONE LISTA
086300171218     C************************************************************
086400171218     C     GESS1         BEGSR
086500171218     C*
086600171218     C* inizializzazione videata
086700171218     C     $INZS1        IFEQ      *ON
086800171218     C                   EXSR      INZS1
086900171218     C                   MOVE      *OFF          $INZS1
087000171218     C                   ENDIF
087100171218     C* emissione piede videata se proveniente da altra
087200171218     C* salvataggio valore formato di provenienza
087300171218     C     $LASTV        IFNE      'S1'
087400171218     C                   WRITE     FMTT1
087500171218     C                   EXSR      GESPIE
087600171218     C                   MOVE      $LASTV        $LASTG
087700171218     C                   MOVE      'S1'          $LASTV
087800171218     C                   ENDIF
087900171218     C*
088000171218     C     WMAX          IFEQ      0
088100171218     C                   WRITE     FMTD3                                        NO RCD
088200171218     C                   ENDIF
088300171218     C*
088400171218     C                   EXFMT     FMTC1
088500171218     C     C1NRR         IFNE      0
088600171218     C                   Z-ADD     C1NRR         WSFL
088700171218     C                   ENDIF
088800171218     C                   Z-ADD     SFLNRR        C1RCD
088900171218     C*
089000171218     C* controllo abilitazione tasti funzione
089100171218     C                   EXSR      CTRKS1
089200171218     C*
089300171218     C     *IN99         IFEQ      *OFF
089400171218     C*
089500171218     C                   SELECT
089600171218     C* F3=Fine
089700171218     C     $TASTO        WHENEQ    F03
089800171218     C                   EXSR      F03S1
089900171218     C* F8=Successivo
090000171218     C     $TASTO        WHENEQ    F08
090100171218     C                   EXSR      F08S1
090200171218     C* F10=Immissione
090300171218     C     $TASTO        WHENEQ    F10
090400171218     C                   EXSR      F10S1
090500171218     C* F12=Ritorno
090600171218     C     $TASTO        WHENEQ    F12
090700171218     C                   EXSR      F12S1
090800171218     C* F17=Parzializzazioni
090900171218     C     $TASTO        WHENEQ    F17
091000171218     C                   EXSR      F17S1
091100171218     C* F18=Modifica valori assunti
091200171218     C     $TASTO        WHENEQ    F18
091300171218     C                   EXSR      F18S1
091400171218     C* F23=Altre opzioni
091500171218     C     $TASTO        WHENEQ    F23
091600171218     C                   EXSR      F23S1
091700171218     C* F24=Altri tasti
091800171218     C     $TASTO        WHENEQ    F24
091900171218     C                   EXSR      F24S1
092000171218     C* ROLL-UP
092100171218     C     $TASTO        WHENEQ    ROLLUP
092200171218     C                   EXSR      ROLS1
092300171218     C*
092400171218     C                   OTHER
092500171218     C* CONTROLLO DATI
092600171218     C                   EXSR      CTRC1
092700171218     C     *IN99         IFEQ      *OFF
092800171218     C                   EXSR      CTRS1
092900171218     C                   ENDIF
093000171218     C*
093100171218     C                   ENDSL
093200171218     C*
093300171218     C                   ENDIF
093400171218     C*
093500171218     C                   ENDSR
093600171218     C/EJECT
093700171218     C************************************************************
093800171218     C* INIZIALIZZAZIONE LISTA
093900171218     C************************************************************
094000171218     C     INZS1         BEGSR
094100171218     C* pulizia SFL
094200171218     C                   SETOFF                                         3031
094300171218     C                   WRITE     FMTC1
094400171218     C                   SETON                                          31
094500171218     C*
094600171218     C* riempimento opzioni
094700171218     C                   MOVEA     $TX1          $TX
094800171218     C                   MOVEA     $FL1          $FL
094900171218     C                   Z-ADD     0             WCOZ
095000171218     C*  equivale al num.di elementi della schiera $TX1
095100171218     C                   Z-ADD     $DIMOP        WMXO
095200171218     C                   Z-ADD     76            $L01
095300171218     C                   Z-ADD     76            $L02
095400171218     C                   EXSR      RIEOPZ
095500171218     C                   Z-ADD     23            X
095600171218     C                   MOVE      FF23          $FC2(X)
095700171218     C                   MOVEL     WOPZ1         C1OPZ1
095800171218     C                   MOVEL     $ALLOPT       H1ALLOPT
095900171218     C                   MOVEL     WOPZ2         C1OPZ2
096000171218     C                   Z-ADD     WCOZ          C1COZ
096100171218     C*
096200171218     C* CARICAMENTO SFL A PAGINE
096300171218     C*
096400171218     C                   Z-ADD     0             S1NRR
096500171218     C                   Z-ADD     0             WMAX
096600171218     C*
096700171218     C* Posizionamento su file pilota
096800171218     C                   CLEAR                   C1POS
096900171218     C                   EXSR      SETANA
097000171218     C* ripristino i valori del posizionamento
097100171218     C                   MOVE      W1POS         C1POS
097200171218    >C* aggiungere gestione altri campi posizionamento se ci sono
097300171218     C*
097400171218     C* Lettura file pilota
097500171218     C                   EXSR      REDANA
097600171218     C*
097700171218     C* Carico 1a pagina
097800171218     C                   EXSR      ROLS1
097900171218     C                   Z-ADD     SFLPAG        WPAG
098000171218     C*
098100171218     C                   ENDSR
098200171218     C/EJECT
098300171218     C************************************************************
098400171218     C* CARICAMENTO PAGINA LISTA
098500171218     C************************************************************
098600171218     C     ROLS1         BEGSR
098700171218     C*
098800171218     C                   SETOFF                                       32
098900171218     C                   Z-ADD     0             Y
099000171218     C                   Z-ADD     WMAX          S1NRR
099100171218     C*
099200171218     C* Leggo dal file anagrafico per caricare la lista
099300171218     C*
099400171218     C     $EFILE        DOWEQ     *OFF
099500171218     C     Y             ANDLT     WPAG
099600171218     C*
099700171218     C                   EXSR      RIES1
099800171218     C*
099900171218     C                   ADD       1             S1NRR
100000171218     C                   ADD       1             Y
100100171218     C*
100200171218     C                   WRITE     FMTS1
100300171218     C*
100400171218     C                   EXSR      REDANA
100500171218     C*
100600171218     C                   ENDDO
100700171218     C*
100800171218     C                   Z-ADD     S1NRR         WMAX                 30
100900171218     C*
101000171218     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
101100171218     C*
101200171218     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
101300171218     C                   MVR                     RESTO             3 0
101400171218     C     PAGINE        MULT      SFLPAG        C1RCD
101500171218     C     RESTO         IFGT      0
101600171218     C                   ADD       1             C1RCD
101700171218     C                   ELSE
101800171218     C                   SUB       SFLPAG        C1RCD
101900171218     C                   ADD       1             C1RCD
102000171218     C                   ENDIF
102100171218     C*
102200171218     C                   ENDSR
102300171218     C/EJECT
102400171218     C************************************************************
102500171218     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
102600171218     C************************************************************
102700171218     C     SETANA        BEGSR
102800171218     C*
102900171218     C                   SELECT
103000171218     C*
103100171218     C* Gestione posizionamento
103200171218     C* Se il campo W1POS è > *blank imposto il posizionamento con
103300171218     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
103400171218     C*
103500171218    >C* Ord.1 = chiave ordinamento 1
103600171218     C     $ORD          WHENEQ    1
103700171218     C     W1POS         IFNE      *BLANK
103800171218     C                   MOVEL     WWWSOC        RICSocieta
103900171218     C*
104000171218     C                   MOVEL     W1POS         RICTpReg
104100171218     C                   MOVEL     *BLANK        RICLibro
104200171218     C                   MOVEL     *Blank        RICTpRIC
104300171218     C                   ELSE
104400171218     C                   MOVEL     WWWSOC        RICSocieta
104500171218     C                   MOVEL     D1TpReg       RICTpReg
104600171218     C                   MOVEL     D1Libro       RICLibro
104700171218     C                   ENDIF
104800171218     C     K03RIC01      SETLL     SDGRIC01L
104900171218     C* Ord.99= Chiave univoca
105000171218     C* (quando viene gestita la chiave univoca NON deve essere gestito
105100171218     C*  il posizionamento C1POS/W1POS)
105200171218     C     $ORD          WHENEQ    99
105300171218     C                   MOVEL     WWWSOC        RICSocieta
105400171218     C                   MOVEL     D1TpReg       RICTpReg
105500171218     C                   MOVEL     D1Libro       RICLibro
105600171218     C     K03RIC01      SETLL     SDGRIC01L
105700171218     C                   ENDSL
105800171218     C*
105900171218     C                   ENDSR
106000171218     C/EJECT
106100171218     C************************************************************
106200171218     C* LETTURA RCD ARCHIVIO PILOTA
106300171218     C************************************************************
106400171218     C     REDANA        BEGSR
106500171218     C*
106600171218     C                   MOVEL     *OFF          $EFILE
106700171218     C                   MOVEL     *OFF          $RCDOK
106800171218     C*
106900171218     C     $EFILE        DOUEQ     *ON
107000171218     C     $RCDOK        OREQ      *ON
107100171218     C*
107200171218     C                   SELECT
107300171218     C     $ORD          WHENEQ    1
107400171218     C                   MOVEL     WWWSOC        RICSOCIETA
107500171218     C     RICSOCIETA    READE     SDGRIC01L                              21
107600171218     C     $ORD          WHENEQ    99
107700171218     C                   MOVEL     WWWSOC        RICSOCIETA
107800171218     C     RICSOCIETA    READE     SDGRIC01L                              21
107900171218     C                   ENDSL
108000171218     C*
108100171218     C                   MOVE      *IN21         $EFILE
108200171218     C*
108300171218     C     $EFILE        IFEQ      *OFF
108400171218     C                   EXSR      SELANA
108500171218     C                   ENDIF
108600171218     C*
108700171218     C                   ENDDO
108800171218     C*
108900171218     C                   MOVE      *IN21         *IN33                          SflEnd
109000171218     C*
109100171218     C                   ENDSR
109200171218     C/EJECT
109300171218     C************************************************************
109400171218     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
109500171218     C************************************************************
109600171218     C     CLCPAG        BEGSR
109700171218     C* Input :
109800171218     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
109900171218     C*          cursore
110000171218     C* - SFLPAG = numero rcd per pagina sfl
110100171218     C* Output :
110200171218     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
110300171218     C*
110400171218     C     WSFL          DIV       SFLPAG        PAGINE            4 0
110500171218     C                   MVR                     RESTO             3 0
110600171218     C     RESTO         IFGT      0
110700171218     C                   ADD       1             PAGINE
110800171218     C                   ENDIF
110900171218     C     PAGINE        MULT      SFLPAG        WPAG
111000171218     C*
111100171218     C                   ENDSR
111200171218     C/EJECT
111300171218     C************************************************************
111400171218     C* SELEZIONI SULLA RIGA LETTA
111500171218     C* impostare a *OFF il campo $RCDOK per scartare un record che
111600171218     C* non rientra nei parametri di parzializzazione
111700171218     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
111800171218     C* superiore stabilito nei parametri
111900171218     C*
112000171218     C************************************************************
112100171218     C     SELANA        BEGSR
112200171218     C*
112300171218     C                   MOVEL     *ON           $RCDOK
112400171218     C                   MOVEL     *ALL'9'       ALL9             15
112500171218     C* creo limite massimo parzializzazioni :
112600171218     C* per ogni valore alfanumerico creo il limite superiore
112700171218     C* per cui caricare, creando una variabile che contiene
112800171218     C* il valore digitato nel campo a video riempito di "9"
112900171218     C     D1Libro       CAT       ALL9:0        Libro999
113000171218     C*
113100171218     C*
113200171218     C                   SELECT
113300171218     C*
113400171218     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
113500171218     C*
113600171218     C     RICLibro      WHENLT    D1Libro
113700171218     C     RICLibro      ORGT      Libro999
113800171218     C                   MOVEL     *OFF          $RCDOK
113900171218     C*
114000171218     C     RICTpRIC      WHENNE    D1TpRIC
114100171218     C     D1TpRic       andNE     *Blank
114200171218     C                   MOVEL     *OFF          $RCDOK
114300171218     C*
114400171218     C                   ENDSL
114500171218     C*
114600171218     C* FINE SELEZIONE
114700171218     C*  nel caso che il dato letto sia > del limite superiore e
114800171218     C*  questo dato è la chiave della VL in lettura, allora per
114900171218     C*  evitare altri accessi inutili salto a fine file
115000171218     C                   MOVEL     WWWSOC        RICSocieta
115100171218     C                   SELECT
115200171218     C* ordinamento per registro
115300171218     C     D1TpReg       WhenNE    *BLANK
115400171218     C     RICTpReg      andNE     D1TpReg
115500171218     C     $ORD          ANDEQ     1
115600171218     C     K02RIC01      SETGT     SDGRIC01L
115700171218     C*
115800171218     C* ordinamento per registro/libro
115900171218     C     RICLibro      WHENGT    Libro999
116000171218     C     $ORD          ANDEQ     1
116100171218     C     K03RIC01      SETGT     SDGRIC01L
116200171218     C                   ENDSL
116300171218     C*
116400171218     C                   ENDSR
116500171218     C/EJECT
116600171218     C************************************************************
116700171218     C* RIEMPIMENTO RIGA LISTA
116800171218     C************************************************************
116900171218     C     RIES1         BEGSR
117000171218     C*
117100171218     C                   CLEAR                   FMTS1
117200171218     C*
117300171218     C* riempio i campi del sfl, compresi gli hidden con i valori
117400171218     C* del rcd letto
117500171218     C                   MOVEL     RICTpReg      S1TpReg
117600171218     C                   MOVEL     RICLibro      S1Libro
117700171218     C                   MOVEL     RICTpRIC      S1TpRIC
117800171218     C                   MOVEL     RICDescr      S1Descr
117900171218     C                   IF        RICDtInVal <> *Loval
118000171218     C     *JobRun       MOVE      RICDtInVal    S1DtInVal
118100171218     C                   ENDIF
118200171218     C                   IF        RICDtFiVal <> *Hival
118300171218     C     *JobRun       MOVE      RICDtFiVal    S1DtFiVal
118400171218     C                   ENDIF
118401171228     C                   if        RICFatt     = *On
118402171228     C                   Eval      S1Fatt      = XScOn
118403171228     C                   else
118404171228     C                   Eval      S1Fatt      = XScOff
118405171228     C                   endif
118500171219     C                   if        RICNAcc     = *On
118600171219     C                   Eval      S1NAcc      = XScOn
118700171219     C                   else
118800171219     C                   Eval      S1NAcc      = XScOff
118900171219     C                   endif
119000171218     C*
119100171218     C* azzero le memorizzazioni degli attributi
119200171218     C* - rcd abilitato
119300171218     C                   MOVEL     *ZERO         H1ABIL
119400171218     C*
119500171218     C* se stiamo visualizzando gli slitt. dei libri della cap. anagrafica
119600171218     C* testo se per il codice recuperato ne esiste uno relativo sulla
119700171218     C* società di appartenenza dell'utente
119800171218     C     D1FSC         IFEQ      'C'
119900171218     C* salvo quei campi che potrebbero sporcarsi
120000171218     C                   MOVE      RICSOCIETA    WSOC
120100171218     C* eseguo il test
120200171218     C                   MOVE      XSCSOC        RICSOCIETA
120300171218     C     K03RIC01      SETLL     SDGRIC01L                              21
120400171218     C                   MOVEL     *IN21         H1ABIL
120500171218     C* ripristino i campi che si potevano sporcare
120600171218     C                   MOVE      WSOC          RICSOCIETA
120700171218     C* mi riposiziono sul rcd successivo a quello scritto sul sfl
120800171218     C* altrimenti non si potrebbe essere sicuri di cosa leggerà
120900171218     C     K03RIC01      SETGT     SDGRIC01L
121000171218     C                   ENDIF
121100171218     C*
121200171218     C* imposto quegli attributi del sfl che sono modificabili
121300171218     C* al richiamo del determinato pgm
121400171218     C                   EXSR      ATRS1
121500171218     C*
121600171218     C                   ENDSR
121700171218     C/EJECT
121800171218     C************************************************************
121900171218     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
122000171218     C************************************************************
122100171218     C     ATRS1         BEGSR
122200171218     C*
122300171218     C* azzero gli altri attributi
122400171218     C                   MOVEL     X'A0'         S1TpRegA
122500171218     C                   MOVEL     X'A0'         S1LibroA
122600171218     c*
122700171218     C* li valorizzo a seconda dello stato ricevuto o testato
122800171218     C     H1ABIL        IFEQ      *ON
122900171218     C                   MOVEL     ABIL          S1TpRegA
123000171218     C                   MOVEL     ABIL          S1LibroA
123100171218     C                   ENDIF
123200171218     C*
123300171218     C                   ENDSR
123400171218     C/EJECT
123500171218     C************************************************************
123600171218     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
123700171218     C* INPUT
123800171218     C* $FC2 = Abilitazioni videata S1
123900171218     C* OUTPUT
124000171218     C* *IN97 *IN99 Errori
124100171218     C************************************************************
124200171218     C     CTRKS1        BEGSR
124300171218     C*
124400171218     C                   SETOFF                                       99
124500171218     C                   MOVEA     $FC2          $FC
124600171218     C                   EXSR      CTRKEY
124700171218     C     *IN99         IFEQ      *ON
124800171218     C                   SETON                                        97
124900171218     C                   ENDIF
125000171218     C*
125100171218     C                   ENDSR
125200171218     C/EJECT
125300171218     C************************************************************
125400171218     C* GESTIONE F08 VIDEO S1
125500171218     C************************************************************
125600171218     C     F08S1         BEGSR
125700171218     C*
125800171218     C                   MOVE      *ON           $FINE
125900171218     C                   MOVE      '0'           RETI30
126000171218     C*
126100171218     C                   ENDSR
126200171218     C/EJECT
126300171218     C************************************************************
126400171218     C* GESTIONE F03 VIDEO S1
126500171218     C************************************************************
126600171218     C     F03S1         BEGSR
126700171218     C*
126800171218     C                   MOVE      *ON           $FINE
126900171218    >C                   MOVE      '1'           RETI30
127000171218     C* fine programma
127100171218     C                   EXSR      ENDPGM
127200171218     C*
127300171218     C                   ENDSR
127400171218     C/EJECT
127500171218     C************************************************************
127600171218     C* GESTIONE F10 VIDEO S1
127700171218     C* AGGIUNTA RECORD
127800171218     C************************************************************
127900171218     C     F10S1         BEGSR
128000171218     C*
128100171218     C                   RESET                   SDGSI31DS
128200171218     C                   MOVEL     '01'          OPZI31
128300171218     C* passare anche le parzializzazioni ricevute relative alla chiave
128400171218     C* univoca : se ce ne sono saranno usate come default d'immissione
128500171218     C                   MOVEL     d1fsc         FSCI31
128600171218     C                   MOVEL     TpRegI30      TpRegI31
128700171218     C                   MOVEL     LibroI30      LibroI31
128800171218     C                   MOVEL     TpRICI30      TpRICI31
128900171218     C* codice di riferimento per le valorizzazioni di default
129000171218     C                   MOVE      $TpReg        DF1I31
129100171218     C                   MOVE      $Libro        DF2I31
129200171218     C                   MOVEL     SDGSI31DS     KPJBU
129300171218     C                   CALL      'SDGSI31R'
129400171218     C                   PARM                    KPJBA
129500171218     C                   MOVEL     KPJBU         SDGSI31DS
129600171218     C* ritorno da PGM gestione
129700171218     C                   EXSR      RTNI31
129800171218     C     WINZS1        IFEQ      *ON
129900171218     C                   MOVE      *ON           $INZS1
130000171218     C* se esistevano già righe sul sfl
130100171218     C* calcolo pagina a cui deve posizionarsi
130200171218     C     WSFL          IFGT      0
130300171218     C                   EXSR      CLCPAG
130400171218     C* altrimenti carico solo la 1a pagina
130500171218     C                   ELSE
130600171218     C                   Z-ADD     SFLPAG        WPAG
130700171218     C                   ENDIF
130800171218     C                   ENDIF
130900171218     C*
131000171218     C                   MOVE      *HIVAL        $LASTV
131100171218     C*
131200171218     C                   ENDSR
131300171218     C/EJECT
131400171218     C************************************************************
131500171218     C* GESTIONE F12 VIDEO S1
131600171218     C************************************************************
131700171218     C     F12S1         BEGSR
131800171218     C*
131900171218     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
132000171218     C     $LOP          IFEQ      1
132100171218     C                   MOVE      'D1'          $GEST
132200171218     C*  se la 1a videata è questa, per F12 torno a menu
132300171218     C                   ELSE
132400171218     C                   MOVE      *ON           $FINE
132500171218    >C                   MOVE      '2'           RETI30
132600171218     C                   ENDIF
132700171218     C*
132800171218     C                   ENDSR
132900171218     C/EJECT
133000171218     C************************************************************
133100171218     C* GESTIONE F17 VIDEO S1
133200171218     C************************************************************
133300171218     C     F17S1         BEGSR
133400171218     C*
133500171218     C                   MOVE      'D1'          $GEST
133600171218     C* se il pgm è stato richiamato con chiave univoca
133700171218     C* e dopo è stata richiesta una parzializzazione
133800171218     C* tolgo la gestione della chiave univoca (azzere/abblenco il
133900171218     C* valore; propongo come ordinamento il 1°)
134000171218     C     $UNIQ         IFEQ      *ON
134100171218     C                   MOVE      *OFF          $UNIQ
134200171218     C                   ENDIF
134300171218     C     $ORD          IFEQ      99
134400171218     C                   Z-ADD     1             $ORD
134500171218     C                   MOVE      *ON           $INZD1
134600171218     C                   ENDIF
134700171218     C*
134800171218     C                   ENDSR
134900171218     C/EJECT
135000171218     C************************************************************
135100171218     C* GESTIONE F18 VIDEO S1
135200171218     C************************************************************
135300171218     C     F18S1         BEGSR
135400171218     C*
135500171218     C                   MOVE      'D2'          $GEST
135600171218     C                   MOVEL     *ON           $INZD2
135700171218     C     $ORD          IFEQ      99
135800171218     C                   Z-ADD     1             $ORD
135900171218     C                   ENDIF
136000171218     C*
136100171218     C                   ENDSR
136200171218     C/EJECT
136300171218     C************************************************************
136400171218     C* GESTIONE F23 VIDEO S1
136500171218     C************************************************************
136600171218     C     F23S1         BEGSR
136700171218     C*
136800171218    >C                   Z-ADD     76            $L01
136900171218    >C                   Z-ADD     76            $L02
137000171218     C                   EXSR      RIEOPZ
137100171218     C                   Z-ADD     23            X
137200171218     C                   MOVE      FF23          $FC2(X)
137300171218     C                   MOVEL     WOPZ1         C1OPZ1
137400171218     C                   MOVEL     $ALLOPT       H1ALLOPT
137500171218     C                   MOVEL     WOPZ2         C1OPZ2
137600171218     C                   Z-ADD     WCOZ          C1COZ
137700171218     C*
137800171218     C                   ENDSR
137900171218     C/EJECT
138000171218     C************************************************************
138100171218     C* GESTIONE F24 VIDEO S1
138200171218     C************************************************************
138300171218     C     F24S1         BEGSR
138400171218     C*
138500171218     C                   MOVEA     $FX2          $FX
138600171218     C                   MOVEA     $FC2          $FC
138700171218     C                   MOVEA     $FR2          $FR
138800171218     C                   Z-ADD     $ULKS1        $ULK
138900171218    >C                   Z-ADD     79            $L01
139000171218    >C                   Z-ADD     79            $L02
139100171218     C                   EXSR      RIEKEY
139200171218     C                   Z-ADD     $ULK          $ULKS1            3 0
139300171218     C                   MOVEA     $FC           $FC2
139400171218     C                   MOVEL     $KEY1         Z2KE1
139500171218     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
139600171218     C                   MOVEL     $KEY2         Z2KE2
139700171218     C                   WRITE     FMTZ2
139800171218     C*
139900171218     C                   ENDSR
140000171218     C/EJECT
140100171218     C************************************************************
140200171218     C* CONTROLLO TESTATA LISTA
140300171218     C************************************************************
140400171218     C     CTRC1         BEGSR
140500171218     C*
140600171218     C                   MOVE      *OFF          *IN99
140700171218     C*
140800171218     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
140900171218     C* L'INIZIALIZZAZIONE LISTA
141000171218     C*
141100171218     C     C1POS         IFNE      W1POS
141200171218    >C* aggiungere in OR altri campi posizionamento se ci sono
141300171218     C                   EXSR      POSS1
141400171218     C                   ENDIF
141500171218     C*
141600171218     C                   ENDSR
141700171218     C/EJECT
141800171218     C************************************************************
141900171218     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
142000171218     C************************************************************
142100171218     C     POSS1         BEGSR
142200171218     C*
142300171218     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
142400171218     C*
142500171218   B1C     WMAX          IFGT      0
142600171218     C                   READC     FMTS1                                  21
142700171218   B2C     *IN21         DOWEQ     *OFF
142800171218     C     *IN99         ANDEQ     *OFF
142900171218     C*
143000171218   B3C     S1OPZ         IFNE      0
143100171218     C                   SETON                                        51  99
143200171218   E3C                   ENDIF
143300171218     C* se rilevata opzione attiva, attivo sflnxtchg
143400171218   B3C     *IN99         IFEQ      *ON
143500171218     C                   MOVE      *ON           *IN32
143600171218     C*
143700171218     C* imposto quegli attributi del sfl che sono modificabili
143800171218     C* al richiamo del determinato pgm
143900171218     C                   EXSR      ATRS1
144000171218     C*
144100171218     C                   UPDATE    FMTS1
144200171218   X3C                   ELSE
144300171218     C                   READC     FMTS1                                  21
144400171218   E3C                   ENDIF
144500171218     C*
144600171218   E2C                   ENDDO
144700171218     C*
144800171218   E1C                   ENDIF
144900171218     C*
145000171218     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
145100171218     C* AL VALORE DI POSIZIONAMENTO :
145200171218     C* se questo è maggiore del valore del rispettivo campo di
145300171218     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
145400171218     C* con i valori di parzializzazione.
145500171218     C*
145600171218   B1C     *IN99         IFEQ      *OFF
145700171218    >C                   MOVE      *BLANK        W1POS
145800171218     C*
145900171218   B2C                   SELECT
146000171218     C*
146100171218     C     $ORD          WHENEQ    1
146200171218     C*
146300171218    >C     C1POS         IFGT      D1TpReg
146400171218     C                   MOVEL     C1POS         W1POS
146500171218    >C* sostituire C1POS/W1POS con altri campi posizionamento
146600171218    >C* se il campo in parzializzazione ne richiede uno specifico
146700171218     C                   ENDIF
146800171218     C*
146900171218     C                   MOVE      *ON           *IN99
147000171218     C                   MOVE      *ON           $INZS1
147100171218     C*
147200171218     C     $ORD          WHENEQ    99
147300171218     C*
147400171218     C                   MOVE      *ON           *IN99
147500171218     C                   MOVE      *ON           $INZS1
147600171218     C*
147700171218     C*
147800171218     C     $ORD          WHENEQ    2
147900171218     C*
148000171218    >C     C1POS         IFGT      D1Libro
148100171218     C                   MOVE      C1POS         W1POS
148200171218    >C* sostituire C1POS/W1POS con altri campi posizionamento
148300171218    >C* se il campo in parzializzazione ne richiede uno specifico
148400171218     C                   ENDIF
148500171218     C*
148600171218     C                   MOVE      *ON           *IN99
148700171218     C                   MOVE      *ON           $INZS1
148800171218     C*
148900171218   E2C                   ENDSL
149000171218     C*
149100171218   E1C                   ENDIF
149200171218     C*
149300171218     C                   ENDSR
149400171218     C/EJECT
149500171218     C************************************************************
149600171218     C* CONTROLLO OPZIONI LISTA
149700171218     C************************************************************
149800171218     C     CTRS1         BEGSR
149900171218     C*
150000171218     C                   MOVEL     *OFF          $ESCI
150100171218     C                   SETOFF                                       99
150200171218     C                   Z-ADD     0             S1OPZ
150300171218     C*
150400171218     C* Leggo il sfl solo se ci sono rcd
150500171218   B1C     WMAX          IFGT      0
150600171218     C                   READC     FMTS1                                  21
150700171218     C*
150800171218     C* esce se fine sfl o errore che richiede l'uscita
150900171218   B2C     *IN21         DOWEQ     *OFF
151000171218     C     $ESCI         ANDEQ     *OFF
151100171218     C                   Z-ADD     S1NRR         C1RCD
151200171218     C* ctrl su riga
151300171218     C                   EXSR      RECS1
151400171218     C* gestione opzioni
151500171218   B3C     S1OPZ         IFNE      0
151600171218     C     *IN99         ANDEQ     *OFF
151700171218     C                   EXSR      OPZS1
151800171218   E3C                   ENDIF
151900171218     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
152000171218   B3C     *IN99         IFEQ      *ON
152100171218     C                   MOVE      *ON           *IN32
152200171218   E3C                   ENDIF
152300171218     C*
152400171218     C* imposto quegli attributi del sfl che sono modificabili
152500171218     C* al richiamo del determinato pgm
152600171218     C                   EXSR      ATRS1
152700171218     C*
152800171218     C                   Z-ADD     0             S1OPZ
152900171218     C*
153000171218     C                   UPDATE    FMTS1
153100171218     C                   MOVE      *OFF          *IN32
153200171218     C*
153300171218     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
153400171218   B3C     $ESCI         IFEQ      *OFF
153500171218     C                   READC     FMTS1                                  21
153600171218     C* a fine ciclo ripristino il flag richiesta iniz.sfl
153700171218   B4C     *IN21         IFEQ      *ON
153800171218     C                   MOVE      WINZS1        $INZS1
153900171218     C* calcolo pagina a cui deve posizionarsi
154000171218     C     WSFL          IFGT      0
154100171218     C                   EXSR      CLCPAG
154200171218     C                   ELSE
154300171218     C                   Z-ADD     SFLPAG        WPAG
154400171218     C                   ENDIF
154500171218   E4C                   ENDIF
154600171218   E4C                   ENDIF
154700171218     C*
154800171218   E2C                   ENDDO
154900171218     C*
155000171218   E1C                   ENDIF
155100171218     C*
155200171218     C                   ENDSR
155300171218     C/EJECT
155400171218     C************************************************************
155500171218     C* CONTROLLO CAMPI I/O RIGA LISTA
155600171218     C************************************************************
155700171218     C     RECS1         BEGSR
155800171218     C*
155900171218     C* reset indicatori DSPATR
156000171218     C                   MOVE      *ALL'0'       IN4049           10
156100171218     C                   MOVEA     IN4049        *IN(40)
156200171218     C*
156300171218     C* Controllo se Utente Autorizzato alla scelta
156400171218     C     S1OPZ         IFNE      0
156500171218     C                   Z-ADD     1             X
156600171218     C     S1OPZ         LOOKUP    $OP1(X)                                21
156700171218     C     *IN21         IFEQ      *OFF
156800171218     C     $FL1(X)       OREQ      'N'
156900171218     C                   MOVE      *ON           $ESCI
157000171218     C                   SETON                                        504099
157100171218     C                   ENDIF
157200171218     C                   ENDIF
157300171218     C*
157400171218     C                   ENDSR
157500171218     C/EJECT
157600171218     C************************************************************
157700171218     C* GESTIONE OPZIONI LISTA
157800171218     C************************************************************
157900171218     C     OPZS1         BEGSR
158000171218     C*
158100171218     C                   Z-ADD     RIRI          CURR
158200171218     C                   Z-ADD     COCO          CURC
158300171218     C*
158400171218     C                   SELECT
158500171218     C*
158600171218     C* se opz=Selezione passo la chiave al pgm chiamante
158700171218     C     S1OPZ         WHENEQ    1
158800171218    >C                   MOVE      S1OPZ         OPZI30
158900171218     C* passare nei campi appositi della DS SDGSI30DS il valore
159000171218     C* della chiave della VL univoca (se c'è)
159100171218     C                   MOVEL     S1TpReg       TpRegI30
159200171218     C                   MOVEL     S1Libro       LibroI30
159300171218     C                   MOVEL     S1TpRIC       TpRICI30
159400171218    >C                   MOVE      D1FSC         FSCI30
159500171218     C                   MOVE      *ON           $ESCI
159600171218     C                   MOVE      *ON           $FINE
159700171218     C*
159800171218     C                   MOVE      *HIVAL        $LASTV
159900171218     C* ABILITA
160000171218     C*
160100171218     C     S1OPZ         WHENEQ    11
160200171218     C     H1ABIL        IFNE      *ON
160300171218     C                   RESET                   A066DS
160400171218     C                   MOVE      '11'          OPZ066
160500171218     C                   MOVE      XSCCPA        CAP066
160600171218     C                   MOVE      XSCSOC        SOC066
160700171218     C                   MOVE      S1TpReg       TPR066
160800171218     C                   MOVE      S1Libro       LIB066
160900171218     C                   MOVEL     A066DS        KPJBU
161000171218     C                   CALL      'ANA066R'
161100171218     C                   PARM                    KPJBA
161200171218     C                   MOVEL     KPJBU         A066DS
161300171218     C*
161400171218     C* ritorno da PGM abilitazione
161500171218     C                   EXSR      RTN066
161600171218     C*
161700171218     C* se è già abilitato do msg d'errore
161800171218     C                   ELSE
161900171218     C                   SETON                                        5399
162000171218     C                   ENDIF
162100171218     C*
162200171218     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
162300171218     C                   OTHER
162400171218     C*
162500171218    >C                   RESET                   SDGSI31DS
162600171218    >C                   MOVEL     S1OPZ         OPZI31
162700171218     C* passare nei campi appositi della DS il valore
162800171218     C* della chiave della VL univoca
162900171218    >C                   MOVEL     S1TpReg       TpRegI31
163000171218    >C                   MOVEL     S1Libro       LibroI31
163100171218    >C                   MOVEL     S1TpRIC       TpRICI31
163200171218     c                   IF        S1DtInVal  <> 0
163300171218     C     *JobRun       MOVE      S1DTInVal     DtInValI31
163400171218     c                   ENDIF
163500171218     c                   IF        S1DtFiVal  <> 0
163600171218     C     *JobRun       MOVE      S1DTFiVal     DtFiValI31
163700171218     c                   ENDIF
163800171218    >C                   MOVEL     D1FSC         FSCI31
163900171218     C*
164000171218    >C                   MOVEL     SDGSI31DS     KPJBU
164100171218    >C                   CALL      'SDGSI31R'
164200171218     C                   PARM                    KPJBA
164300171218    >C*  se vengono gestite le autorizzazioni
164400171218    >C*  le passo al pgm di manutenzione per evitargli di doversele
164500171218    >C*  riprendere
164600171218    >C                   PARM                    AUTDS
164700171218     C*
164800171218    >C                   MOVEL     KPJBU         SDGSI31DS
164900171218     C*
165000171218     C* ritorno da PGM gestione
165100171218    >C                   EXSR      RTNI31
165200171218     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
165300171218     C     *IN99         IFEQ      *ON
165400171218     C                   SETON                                        40
165500171218     C                   ENDIF
165600171218     C*
165700171218     C                   MOVE      *HIVAL        $LASTV
165800171218     C                   ENDSL
165900171218     C*
166000171218     C                   ENDSR
166100171218     C/EJECT
166200171218     C************************************************************
166300171218     C* GESTIONE RITORNO DA PGM DI GESTIONE
166400171218     C************************************************************
166500171218    >C     RTNI31        BEGSR
166600171218     C*
166700171218     C* modo di ritorno
166800171218     C*
166900171218     C                   SELECT
167000171218     C* questi modi di utilizzo dei valori di ritorno dal
167100171218     C* pgm di manutenzione rcd di anagrafica sono delle
167200171218     C* proposte, normalmente sempre valide, ma modificabili
167300171218     C* per situazioni particolari
167400171218     C* 1 = F3
167500171218    >C     RETI31        WHENEQ    '1'
167600171218     C                   MOVE      *ON           $ESCI                          sospese READC
167700171218     C                   MOVE      *ON           $FINE                          fine pgm
167800171218     C                   MOVE      '1'           RETI30
167900171218     C* 2 = F12
168000171218    >C     RETI31        WHENEQ    '2'
168100171218     C                   MOVE      *ON           $ESCI                          sospese READC
168200171218     C*
168300171218     C                   ENDSL
168400171218     C*
168500171218     C* operazione eseguite dal pgm chiamato
168600171218     C*
168700171218     C                   SELECT
168800171218     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
168900171218    >C     OPRI31        WHENEQ    '1'
169000171218     C*
169100171218     C* se l'opzione con cui è stato chiamato non è la modifica
169200171218     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
169300171218    >C     OPZI31        IFNE      '02'
169400171218     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
169500171218    >C                   MOVE      OPRI31        WOPR                           avverto pgm chiamant
169600171218     C* se l'opzione con cui è stato chiamato è la modifica
169700171218     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
169800171218     C                   ELSE
169900171218     C                   EXSR      AGGS1
170000171218     C                   ENDIF
170100171218     C*
170200171218     C                   ENDSL
170300171218     C*
170400171218     C* funzione non eseguibile per errore :
170500171218     C*
170600171218     C                   SELECT
170700171218     C* 1 = funzione richiamata chiusa in errore
170800171218    >C     ERRI31        WHENEQ    '1'
170900171218     C                   MOVE      *ON           $ESCI                          sospese READC
171000171218     C                   SETON                                        5299      emiss.SFLMSGID
171100171218     C* 2 = funzione richiamata senza la dovuta autorizzazione
171200171218    >C     ERRI31        WHENEQ    '2'
171300171218     C                   MOVE      *ON           $ESCI                          sospese READC
171400171218     C                   SETON                                        5499      emiss.SFLMSGID
171500171218    >C*  eventualmente gestire altri codici di errore
171600171218     C*
171700171218     C                   ENDSL
171800171218     C*
171900171218     C                   ENDSR
172000171218     C/EJECT
172100171218     C************************************************************
172200171218     C* GESTIONE RITORNO DA PGM DI ABILITAZIONE
172300171218     C************************************************************
172400171218     C     RTN066        BEGSR
172500171218     C*
172600171218     C* modo di ritorno
172700171218     C*
172800171218     C                   SELECT
172900171218     C* questi modi di utilizzo dei valori di ritorno dal
173000171218     C* pgm di manutenzione rcd di anagrafica sono delle
173100171218     C* proposte, normalmente sempre valide, ma modificabili
173200171218     C* per situazioni particolari
173300171218     C* 1 =
173400171218     C     RET066        WHENEQ    '1'
173500171218     C*
173600171218     C                   ENDSL
173700171218     C*
173800171218     C* operazione eseguite dal pgm chiamato
173900171218     C*
174000171218     C                   SELECT
174100171218     C* 1 = Abilitato
174200171218     C     OPR066        WHENEQ    '1'
174300171218     C*
174400171218     C                   MOVEL     *ON           H1ABIL
174500171218     C*
174600171218     C                   ENDSL
174700171218     C*
174800171218     C* funzione non eseguibile per errore :
174900171218     C*
175000171218     C                   SELECT
175100171218     C* 1 = funzione richiamata chiusa in errore
175200171218     C     ERR066        WHENEQ    '1'
175300171218     C                   MOVE      *ON           $ESCI                          sospese READC
175400171218     C                   SETON                                        5299      emiss.SFLMSGID
175500171218     C*
175600171218     C                   ENDSL
175700171218     C*
175800171218     C                   ENDSR
175900171218     C/EJECT
176000171218     C************************************************************
176100171218     C* AGGIORNAMENTO RIGA LISTA
176200171218     C************************************************************
176300171218     C     AGGS1         BEGSR
176400171218     C*
176500171218     C* riempio i campi del sfl che sono stati ripassati dal pgm
176600171218     C* di manutenzione dopo una opzione 02
176700171218     C*
176800171218     C                   MOVEL     TpRegI31      S1TpReg
176900171218     C                   MOVEL     LibroI31      S1Libro
177000171218     C                   MOVEL     TpRICI31      S1TpRIC
177100171218     C                   MOVEL     DescrI31      S1Descr
177200171218     C                   IF        DtInValI31 <> *Loval
177300171218     C     *JobRun       MOVE      DtInValI31    S1DtInVal
177400171218     C                   ELSE
177500171218     C                   Z-ADD     0             S1DtInVal
177600171218     C                   ENDIF
177700171218     C                   IF        DtFiValI31 <> *Hival
177800171219     C     *JobRun       MOVE      DtFiValI31    S1DtFiVal
177900171218     C                   ELSE
178000171218     C                   Z-ADD     0             S1DtFiVal
178100171218     C                   ENDIF
178101171228     C                   if        FattI31     = *On
178102171228     C                   Eval      S1Fatt      = XScOn
178103171228     C                   else
178104171228     C                   Eval      S1Fatt      = XScOff
178105171228     C                   endif
178200171219     C                   if        NAccI31     = *On
178300171219     C                   Eval      S1NAcc      = XScOn
178400171219     C                   else
178500171219     C                   Eval      S1NAcc      = XScOff
178600171219     C                   endif
178700171218     C*
178800171218     C                   ENDSR
178900171218     C/EJECT
179000171218     C************************************************************
179100171218     C* GESTIONE RITORNO DA PGM DEI COMMENTI
179200171218     C************************************************************
179300171218     C*          RTN725    BEGSR
179400171218     C*
179500171218     C* modo di ritorno
179600171218     C*
179700171218     C*                    SELEC
179800171218     C* questi modi di utilizzo dei valori di ritorno dal
179900171218     C* pgm di manutenzione rcd di anagrafica sono delle
180000171218     C* proposte, normalmente sempre valide, ma modificabili
180100171218     C* per situazioni particolari
180200171218     C* 1 = F3
180300171218     C*          RET725    WHEQ '1'
180400171218     C*                    MOVE *ON       $ESCI            sospese READC
180500171218     C*                    MOVE *ON       $FINE            fine pgm
180600171218     C* 2 = F12
180700171218     C*          RET725    WHEQ '2'
180800171218     C*                    MOVE *ON       $ESCI            sospese READC
180900171218     C*
181000171218     C*                    ENDSL
181100171218     C*
181200171218     C* operazione eseguite dal pgm chiamato
181300171218     C*
181400171218     C*                    SELEC
181500171218     C* 1 = eseguita scrittura o modifica commento
181600171218     C*          OPR725    WHEQ '1'
181700171218    >C* segnalare su sfl guida l'avvenuto aggiornamento
181800171218     C*
181900171218     C*                    ENDSL
182000171218     C*
182100171218     C* funzione non eseguibile per errore :
182200171218     C*
182300171218     C*                    SELEC
182400171218     C* 1 = funzione richiamata chiusa in errore
182500171218     C*          ERR725    WHEQ '1'
182600171218     C*                    MOVE *ON       $ESCI            sospese READC
182700171218     C*                    SETON                     5299  emiss.SFLMSGID
182800171218     C* 2 = funzione richiamata senza la dovuta autorizzazione
182900171218     C*          ERR725    WHEQ '2'
183000171218     C*                    MOVE *ON       $ESCI            sospese READC
183100171218     C*                    SETON                     5499  emiss.SFLMSGID
183200171218    >C*  eventualmente gestire altri codici di errore
183300171218     C*
183400171218     C*                    ENDSL
183500171218     C*
183600171218     C*                    ENDSR
183700171218     C/EJECT
183800171218      ************************************************************
183900171218     C* GESTIONE RITORNO DA PGM INVIO
184000171218     C************************************************************
184100171218     C*          RTN727    BEGSR
184200171218     C*
184300171218     C* modo di ritorno
184400171218     C*
184500171218     C*                    SELEC
184600171218     C* questi modi di utilizzo dei valori di ritorno dal
184700171218     C* pgm di manutenzione rcd di anagrafica sono delle
184800171218     C* proposte, normalmente sempre valide, ma modificabili
184900171218     C* per situazioni particolari
185000171218     C* 1 = F3
185100171218     C*          RET727    WHEQ '1'
185200171218     C*                    MOVE *ON       $ESCI            sospese READC
185300171218     C*                    MOVE *ON       $FINE            fine pgm
185400171218     C* 2 = F12
185500171218     C*          RET727    WHEQ '2'
185600171218     C*                    MOVE *ON       $ESCI            sospese READC
185700171218     C*
185800171218     C*                    ENDSL
185900171218     C*
186000171218     C* operazione eseguite dal pgm chiamato
186100171218     C*
186200171218     C*                    SELEC
186300171218     C* 1 = eseguito aggiornamento
186400171218     C*          OPR727    WHEQ '1'
186500171218     C*
186600171218     C*                    ENDSL
186700171218     C*
186800171218     C* funzione non eseguibile per errore :
186900171218     C*
187000171218     C*                    SELEC
187100171218     C* 1 = funzione richiamata chiusa in errore
187200171218     C*          ERR727    WHEQ '1'
187300171218     C*                    MOVE *ON       $ESCI            sospese READC
187400171218     C*                    SETON                     5299  emiss.SFLMSGID
187500171218     C* 2 = funzione richiamata senza la dovuta autorizzazione
187600171218     C*          ERR727    WHEQ '2'
187700171218     C*                    MOVE *ON       $ESCI            sospese READC
187800171218     C*                    SETON                     5499  emiss.SFLMSGID
187900171218     C*
188000171218     C*                    ENDSL
188100171218     C*
188200171218     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
188300171218     C* di sola finestra
188400171218     C*                    MOVEL'0'       WIN727
188500171218     C*
188600171218     C*                    ENDSR
188700171218     C/EJECT
188800171218     C************************************************************
188900171218     C* GESTIONE VALORI ASSUNTI
189000171218     C************************************************************
189100171218     C     GESD2         BEGSR
189200171218     C*
189300171218   B1C     $INZD2        IFEQ      *ON
189400171218     C                   EXSR      INZD2
189500171218     C                   MOVE      *OFF          $INZD2
189600171218   E1C                   ENDIF
189700171218     C*
189800171218   B1C     $LASTV        IFNE      'D2'
189900171218     C                   EXSR      GESPIE
190000171218     C                   MOVE      $LASTV        $LASTG
190100171218     C                   MOVE      'D2'          $LASTV
190200171218   E1C                   ENDIF
190300171218     C*
190400171218     C* se esistono errori sulla videata
190500171218     C* emetto la write del formato a indicatori spenti per vedere
190600171218     C* le eventuali decodifiche
190700171218   B1C     *IN99         IFEQ      *ON
190800171218     C                   MOVEA     *IN           WIN
190900171218     C                   MOVE      *ALL'0'       IN5098           49
191000171218     C                   MOVEA     IN5098        *IN(50)
191100171218     C                   WRITE     FMTD2
191200171218     C                   MOVEA     WIN           *IN
191300171218   E1C                   ENDIF
191400171218     C*
191500171218     C                   EXFMT     FMTD2
191600171218     C*
191700171218     C* controllo abilitazione tasti funzione
191800171218     C                   EXSR      CTRKD2
191900171218     C*
192000171218   B1C     *IN99         IFEQ      *OFF
192100171218     C*
192200171218   B2C                   SELECT
192300171218     C* F3=Fine
192400171218     C     $TASTO        WHENEQ    F03
192500171218     C                   EXSR      F03D2
192600171218     C* F12=Ritorno
192700171218     C     $TASTO        WHENEQ    F12
192800171218     C                   EXSR      F12D2
192900171218     C* F24=Altri tasti
193000171218     C     $TASTO        WHENEQ    F24
193100171218     C                   EXSR      F24D2
193200171218     C*
193300171218     C                   OTHER
193400171218     C                   EXSR      CTRD2
193500171218   B3C     *IN99         IFEQ      *OFF
193600171218     C                   MOVE      $LASTG        $GEST
193700171218   B4C                   SELECT
193800171218     C     $LASTG        WHENEQ    'S1'
193900171218     C* se è stato modificato l'ordinamento di default, ricarico la lista
194000171218   B5C     $ORD          IFNE      D2ORD
194100171218     C                   MOVE      *ON           $INZS1
194200171218     C                   MOVE      *BLANK        W1POS
194300171218   E5C                   ENDIF
194400171218     C     $LASTG        WHENEQ    'D1'
194500171218     C* per ora niente
194600171218   E4C                   ENDSL
194700171218     C* definisco i nuovi valori di default
194800171218     C                   Z-ADD     D2LOP         $LOP
194900171218     C                   Z-ADD     D2ORD         $ORD
195000171218    >C                   MOVE      D2TpReg       $TpReg
195100171218    >C                   MOVE      D2Libro       $Libro
195200171218    >C                   MOVE      D2TpRIC       $TpRIC
195300171218    >C*  elenco altri dati default specifici dell'anagrafica
195400171218     C*
195500171218     C* aggiorno valori assunti
195600171218     C* se richiesto il salvataggio
195700171218   B4C     D2BSV         IFEQ      1
195800171218     C                   EXSR      AGGMPE
195900171218   E4C                   ENDIF
196000171218     C*
196100171218   E3C                   ENDIF
196200171218     C*
196300171218   E2C                   ENDSL
196400171218     C*
196500171218   E1C                   ENDIF
196600171218     C*
196700171218     C                   ENDSR
196800171218     C/EJECT
196900171218     C************************************************************
197000171218     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
197100171218     C************************************************************
197200171218     C     INZD2         BEGSR
197300171218     C*
197400171218     C                   CLEAR                   FMTD2
197500171218     C                   EVAL      D2BSVA = Normale
197600171218     C* se sono entrato con chiave univoca
197700171218     C* impedisco il salvataggio valori assunti
197800171218     C     $UNIQ         IFEQ      *ON
197900171218     C                   EVAL      D2BSVA = Protetto
198000171218     C                   ENDIF
198100171218     C                   MOVEL     $LOP          D2LOP
198200171218     C                   MOVE      $ORD          D2ORD
198300171218     C                   MOVE      $TpReg        D2TpReg
198400171218     C                   MOVE      $Libro        D2Libro
198500171218     C                   MOVE      $TpRIC        D2TpRIC
198600171218     C*
198700171218     C                   ENDSR
198800171218     C/EJECT
198900171218     C************************************************************
199000171218     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
199100171218     C* INPUT
199200171218     C* $FC3 = Abilitazioni videata D2
199300171218     C* OUTPUT
199400171218     C* *IN97 *IN99 Errori
199500171218     C************************************************************
199600171218     C     CTRKD2        BEGSR
199700171218     C*
199800171218     C                   SETOFF                                       99
199900171218     C                   MOVEA     $FC3          $FC
200000171218     C                   EXSR      CTRKEY
200100171218     C     *IN99         IFEQ      *ON
200200171218     C                   SETON                                        97
200300171218     C                   ENDIF
200400171218     C*
200500171218     C                   ENDSR
200600171218     C/EJECT
200700171218     C************************************************************
200800171218     C* GESTIONE F03 VIDEO D2
200900171218     C************************************************************
201000171218     C     F03D2         BEGSR
201100171218     C*
201200171218     C                   MOVE      *ON           $FINE
201300171218     C* fine programma
201400171218     C                   EXSR      ENDPGM
201500171218     C*
201600171218     C                   ENDSR
201700171218     C/EJECT
201800171218     C************************************************************
201900171218     C* GESTIONE F12 VIDEO D2
202000171218     C************************************************************
202100171218     C     F12D2         BEGSR
202200171218     C*
202300171218     C                   MOVE      $LASTG        $GEST
202400171218     C*
202500171218     C                   ENDSR
202600171218     C/EJECT
202700171218     C************************************************************
202800171218     C* GESTIONE F24 VIDEO D2
202900171218     C************************************************************
203000171218     C     F24D2         BEGSR
203100171218     C*
203200171218     C                   MOVEA     $FX3          $FX
203300171218     C                   MOVEA     $FC3          $FC
203400171218     C                   MOVEA     $FR3          $FR
203500171218     C                   Z-ADD     $ULKD2        $ULK
203600171218    >C                   Z-ADD     44            $L01
203700171218    >C                   Z-ADD     0             $L02
203800171218     C                   EXSR      RIEKEY
203900171218     C                   Z-ADD     $ULK          $ULKD2            3 0
204000171218     C                   MOVEA     $FC           $FC3
204100171218     C                   MOVEL     $KEY1         Z3KE1
204200171218     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
204300171218     C                   WRITE     FMTZ3
204400171218     C*
204500171218     C                   ENDSR
204600171218     C/EJECT
204700171218     C************************************************************
204800171218     C* CONTROLLO VIDEATA VALORI ASSUNTI
204900171218     C************************************************************
205000171218     C     CTRD2         BEGSR
205100171218     C*
205200171218     C                   SETOFF                                       99
205300171218     C* non imposto pos. del cursore
205400171218     C                   Z-ADD     0             H1RIGA
205500171218     C                   Z-ADD     0             H1COLO
205600171218     C*
205700171218     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
205800171218     C     D2LOP         IFEQ      0
205900171218     C                   SETON                                          5099
206000171218     C                   ENDIF
206100171218     C*
206200171218     C* OBBLIGATORIO VALORE PER ORDINAMENTO
206300171218     C     D2ORD         IFEQ      0
206400171218     C                   SETON                                          5199
206500171218     C                   ENDIF
206600171218     C*
206700171218     C* CTRL VALIDITA' TIPO REGISTRO
206800171218     C                   EXSR      CtrD2TpReg
206900171218     C                   EXSR      CtrD2Libro
207000171219     C                   EXSR      CtrD2TpRIC
207100171218    >C*  elenco controlli altri dati default specifici dell'anagrafica
207200171218     C*
207300171218     C                   ENDSR
207400171218     C/EJECT
207500171218     C************************************************************
207600171218     C* CONTROLLO tipo registro IVA
207700171218     C************************************************************
207800171218     C     CtrD2TpReg    BEGSR
207900171218     C*
208000171218     C     D2TpReg       IFNE      *BLANK
208100171218     C*
208200171218     C                   RESET                   X07VALDS
208300171218     C                   MOVE      '1'           X07RIC
208400171218     C                   MOVEL     'ANDLI000'    X07TRC
208500171218     C                   MOVEL     'DLITPREG'    X07CAM
208600171218     C                   MOVEL     D2TpReg       X07VAL
208700171218     C                   CALLb     'X07VALR'
208800171218     C                   PARM                    X07VALDS
208900171218     C*
209000171218   B3C     X07ERR        IFNE      '0'
209100171218     C                   SETON                                          5299
209200171218   E3C                   ENDIF
209300171218     C*
209400171218   E3C                   ENDIF
209500171218     C*
209600171218     C                   ENDSR
209700171218     C/EJECT
209800171218     C************************************************************
209900171218     C* CONTROLLO libro IVA
210000171218     C************************************************************
210100171218     C     CtrD2Libro    BEGSR
210200171218     C*
210300171218     C     D2Libro       IFEQ      *BLANK
210400171218     C                   CALLb     'XALLINEA'
210500171218     C                   PARM                    D2Libro
210600171218     C                   PARM      3             LEN               5 0
210700171218     C                   PARM                    XSCINP
210800171218     C                   ENDIF
210900171219     C*
211000171219     C     D2TpReg       IFNE      *BLANK
211100171219     C     D2Libro       andNE     *BLANK
211200171219     C                   MOVEL     WWWSOC        DLISOCIETA
211300171219     C                   MOVEL     D2TpReg       DLITPREG
211400171219     C                   MOVEL     D2Libro       DLILIBRO
211500171219     C     K03DLI01      SETLL     ANDLI01L                               21
211600171219     C     *IN21         IFEQ      *OFF
211700171219     C                   SETON                                        5699
211800171219     C                   ENDIF
211900171219     C                   ENDIF
212000171218     C*
212100171218     C                   ENDSR
212200171218     C/EJECT
212300171219     C************************************************************
212400171219     C* CONTROLLO tipo regola/periodo
212500171219     C************************************************************
212600171219     C     CtrD2TpRIC    BEGSR
212700171219     C*
212800171219     C     D2TpRIC       IFNE      *BLANK
212900171219     C*
213000171219     C                   RESET                   X07VALDS
213100171219     C                   MOVE      '1'           X07RIC
213200171219     C                   MOVEL     'SDGSI30S'    X07TRC
213300171219     C                   MOVEL     'TPRICI30'    X07CAM
213400171219     C                   MOVEL     D2TpRIC       X07VAL
213500171219     C                   CALLb     'X07VALR'
213600171219     C                   PARM                    X07VALDS
213700171219     C*
213800171219   B3C     X07ERR        IFNE      '0'
213900171219     C                   SETON                                          5799
214000171219   E3C                   ENDIF
214100171219     C*
214200171219   E3C                   ENDIF
214300171219     C*
214400171219     C                   ENDSR
214500171219     C/EJECT
214600171218     C************************************************************
214700171218     C* CONTROLLO ABILITAZIONE TASTI
214800171218     C* INPUT
214900171218     C* $TASTO =Tasto funzione premuto
215000171218     C* $FC = Tasti abilitati
215100171218     C* OUTPUT
215200171218     C* IN99 = ERrore
215300171218     C************************************************************
215400171218     C     CTRKEY        BEGSR
215500171218     C*
215600171218     C                   SELECT
215700171218     C     $TASTO        WHENEQ    F01
215800171218     C     $FC(1)        IFEQ      '0'
215900171218     C                   SETON                                        99
216000171218     C                   ENDIF
216100171218     C     $TASTO        WHENEQ    F02
216200171218     C     $FC(2)        IFEQ      '0'
216300171218     C                   SETON                                        99
216400171218     C                   ENDIF
216500171218     C     $TASTO        WHENEQ    F03
216600171218     C     $FC(3)        IFEQ      '0'
216700171218     C                   SETON                                        99
216800171218     C                   ENDIF
216900171218     C     $TASTO        WHENEQ    F04
217000171218     C     $FC(4)        IFEQ      '0'
217100171218     C                   SETON                                        99
217200171218     C                   ENDIF
217300171218     C     $TASTO        WHENEQ    F05
217400171218     C     $FC(5)        IFEQ      '0'
217500171218     C                   SETON                                        99
217600171218     C                   ENDIF
217700171218     C     $TASTO        WHENEQ    F06
217800171218     C     $FC(6)        IFEQ      '0'
217900171218     C                   SETON                                        99
218000171218     C                   ENDIF
218100171218     C     $TASTO        WHENEQ    F07
218200171218     C     $FC(7)        IFEQ      '0'
218300171218     C                   SETON                                        99
218400171218     C                   ENDIF
218500171218     C     $TASTO        WHENEQ    F08
218600171218     C     $FC(8)        IFEQ      '0'
218700171218     C                   SETON                                        99
218800171218     C                   ENDIF
218900171218     C     $TASTO        WHENEQ    F09
219000171218     C     $FC(09)       IFEQ      '0'
219100171218     C                   SETON                                        99
219200171218     C                   ENDIF
219300171218     C     $TASTO        WHENEQ    F10
219400171218     C     $FC(10)       IFEQ      '0'
219500171218     C                   SETON                                        99
219600171218     C                   ENDIF
219700171218     C     $TASTO        WHENEQ    F11
219800171218     C     $FC(11)       IFEQ      '0'
219900171218     C                   SETON                                        99
220000171218     C                   ENDIF
220100171218     C     $TASTO        WHENEQ    F12
220200171218     C     $FC(12)       IFEQ      '0'
220300171218     C                   SETON                                        99
220400171218     C                   ENDIF
220500171218     C     $TASTO        WHENEQ    F13
220600171218     C     $FC(13)       IFEQ      '0'
220700171218     C                   SETON                                        99
220800171218     C                   ENDIF
220900171218     C     $TASTO        WHENEQ    F14
221000171218     C     $FC(14)       IFEQ      '0'
221100171218     C                   SETON                                        99
221200171218     C                   ENDIF
221300171218     C     $TASTO        WHENEQ    F15
221400171218     C     $FC(15)       IFEQ      '0'
221500171218     C                   SETON                                        99
221600171218     C                   ENDIF
221700171218     C     $TASTO        WHENEQ    F16
221800171218     C     $FC(16)       IFEQ      '0'
221900171218     C                   SETON                                        99
222000171218     C                   ENDIF
222100171218     C     $TASTO        WHENEQ    F17
222200171218     C     $FC(17)       IFEQ      '0'
222300171218     C                   SETON                                        99
222400171218     C                   ENDIF
222500171218     C     $TASTO        WHENEQ    F18
222600171218     C     $FC(18)       IFEQ      '0'
222700171218     C                   SETON                                        99
222800171218     C                   ENDIF
222900171218     C     $TASTO        WHENEQ    F19
223000171218     C     $FC(19)       IFEQ      '0'
223100171218     C                   SETON                                        99
223200171218     C                   ENDIF
223300171218     C     $TASTO        WHENEQ    F20
223400171218     C     $FC(20)       IFEQ      '0'
223500171218     C                   SETON                                        99
223600171218     C                   ENDIF
223700171218     C     $TASTO        WHENEQ    F21
223800171218     C     $FC(21)       IFEQ      '0'
223900171218     C                   SETON                                        99
224000171218     C                   ENDIF
224100171218     C     $TASTO        WHENEQ    F22
224200171218     C     $FC(22)       IFEQ      '0'
224300171218     C                   SETON                                        99
224400171218     C                   ENDIF
224500171218     C     $TASTO        WHENEQ    F23
224600171218     C     $FC(23)       IFEQ      '0'
224700171218     C                   SETON                                        99
224800171218     C                   ENDIF
224900171218     C     $TASTO        WHENEQ    F24
225000171218     C     $FC(24)       IFEQ      '0'
225100171218     C                   SETON                                        99
225200171218     C                   ENDIF
225300171218     C                   ENDSL
225400171218     C*
225500171218     C                   ENDSR
225600171218     C/EJECT
225700171218     C************************************************************
225800171218     C* AGGIORNAMENTO VALORI ASSUNTI
225900171218     C************************************************************
226000171218     C     AGGMPE        BEGSR
226100171218     C*
226200171218     C* preparo i parametri di chiamata al driver
226300171218     C                   CLEAR                   XMPEDS
226400171218     C                   EVAL      TpChiamata = '03'
226500171218     C                   EVAL      NChiavi = 5
226600171218     C                   EVAL      VLogica = 2
226700171218     C*
226800171218     C* preparo le chiavi per la chiamata al driver
226900171218     C                   CLEAR                   XMPEDS1
227000171218     C                   MOVEL     XSCSOC        DSMPESOC
227100171218     C                   MOVEL     DSPGM         DSMPEPGM
227200171218     C                   MOVEL     KNMUS         DSMPEPRF
227300171218     C*
227400171218     C* Si memorizzano i dati di default a seconda delle modalità con
227500171218     C* cui il pgm è usato
227600171218     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
227700171218     C* MPETIP = 'PV'  pgm richiamato da menù
227800171218     C* MPETIP = 'PB'  x lancio Batch
227900171218     C*
228000171218     C                   SELECT
228100171218    >C     OPZI30        WHENEQ    '01'
228200171218     C                   MOVEL     'PX'          DSMPETIP
228300171218     C                   OTHER
228400171218     C                   MOVEL     'PV'          DSMPETIP
228500171218     C                   ENDSL
228600171218     C* dati
228700171218     C                   MOVEL     DSMPE         DSMPEPAR
228800171218     C*
228900171218     C                   CALLB     'XMPE'
229000171218     C                   PARM                    XMPEDS
229100171218     C                   PARM                    XMPEDS1
229200171218     C*
229300171218     C                   ENDSR
229400171218     C/EJECT
229500171218     C************************************************************
229600171218     C* OPERAZIONI INIZIALI
229700171218     C************************************************************
229800171218     C     *INZSR        BEGSR
229900171218     C*
230000171218     C     *DMY          Move      Udate         ZDateISO
230100171218     C     *JobRun       Move      ZDateISO      ZDate             6 0
230200171218     C*
230300171218     C* Reperimento parametri
230400171218     C*
230500171218     C     *ENTRY        PLIST
230600171218     C                   PARM                    KPJBA
230700171218     C                   PARM                    DSAUT
230800171218     C*
230900171218     C     PARMS         IFLT      2
231000171218     C                   MOVE      *BLANK        AUTDS
231100171218     C                   ELSE
231200171218     C                   MOVEL     DSAUT         AUTDS
231300171218     C                   ENDIF
231400171218     C*
231500171218     C* Reperimento opzioni
231600171218     C                   EXSR      REPOPZ
231700171218     C*
231800171218     C* Reperimento tasti di funzione
231900171218     C                   EXSR      REPKEY
232000171218     C*
232100171218     C                   ENDSR
232200171218     C/EJECT
232300171218     C************************************************************
232400171218     C* INIZIALIZZAZIONE VARIABILI
232500171218     C************************************************************
232600171218     C     INZVAR        BEGSR
232700171218     C*
232800171218     C* Dati da PGM chiamante
232900171218    >C                   MOVEL     KPJBU         SDGSI30DS
233000171218     C*
233100171218     C* Reperimento dati società
233200171218     C*
233300171218    >C                   MOVEL     'SOC001'      TIPXSC
233400171218     C                   MOVEL     *BLANK        SOCXSC
233500171218     C                   EXSR      REPSOC
233600171218     C     RTNXSC        IFNE      '1'
233700171218    >C                   MOVEL     XSOCDS        SOC001
233800171218     C                   ELSE
233900171218     C                   MOVE      *ON           $FINE
234000171218     C                   EXSR      ENDPGM
234100171218     C                   ENDIF
234200171218     C*
234300171218     C* Reperimento parametri di gestione
234400171218    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
234500171218    >C*   asteriscare la chiamata alla subroutine REPPAR
234600171218     C*
234700171218     C                   MOVEL     *BLANK        XPARIC
234800171218     C                   MOVEL     XSCSOC        XPASOC
234900171218    >C                   MOVEL     'IVA     '    XPAPAR
235000171218     C                   MOVEL     *BLANK        XPAOUT
235100171218     C                   EXSR      REPPAR
235200171218     C     XPAERR        IFEQ      '0'
235300171218     C                   MOVEL     XPAOUT        ANP045DS
235400171218     C                   ELSE
235500171218     C                   RETURN
235600171218     C                   ENDIF
235700171218     C*
235800171218     C* Reperimento autorizzazioni
235900171218     C*
236000171218    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
236100171218    >C*   asteriscare la chiamata alla subroutine REPAUT
236200171218    >C                   EXSR      REPAUT
236300171218     C*
236400171218     C* Reperimento valori default utente
236500171218     C*
236600171218     C                   EXSR      REPDFT
236700171218     C*
236800171218     C* Definizione delle opzioni utilizzabili a pgm
236900171218     C*
237000171218     C                   EXSR      DEFOPZ
237100171218     C*
237200171218     C* Variabili per gestione videate
237300171218     C*
237400171218     C                   MOVE      *OFF          $FINE                          fine pgm
237500171218     C                   MOVE      *OFF          $INZD1                         inizializzione D1
237600171218     C                   MOVE      *OFF          $INZS1                         inizializzione S1
237700171218     C                   MOVE      *OFF          $INZD2                         inizializzione D2
237800171218     C                   MOVE      *OFF          $EFILE                         memo fine file
237900171218     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
238000171218     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
238100171218     C                   MOVE      *HIVAL        $LASTV
238200171218     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
238300171218     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
238400171218     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
238500171218     C*
238600171218     C                   MOVE      *ON           $INZD1
238700171218     C                   MOVE      *ON           $INZS1
238800171218     C*
238900171218     C* se pgm richiamato con chiave univoca impostata,
239000171218     C* forzo la emissione della lista e imposto l'ordinamento
239100171218    >C*  in questo esempio la chiave univoca per la posta è
239200171218    >C*   OBJ/TYP/PRG ma basta testare il primo campo :
239300171218    >C*    se è valorizzata la chiave primaria lo sarà anche la
239400171218    >C*    secondaria se obbligatoria
239500171218     C*
239600171218     C* Variabili appoggio
239700171218     C*
239800171218     C                   Z-ADD     SFLPAG        WPAG
239900171218     C                   MOVE      *BLANK        WFSC
240000171218     C                   MOVEL     *OFF          WOPR
240100171218     C*
240200171218     C* definisco qual'è la prima videata
240300171218     C* (se la prima videata risulta la lista, devo comunque caricare
240400171218     C*  la videata di parzializzazione e gestire la definizione
240500171218     C*  dell'ordinamento, perchè, dai default oppure se sono stati
240600171218     C*  passati dei dati, può variare il caricamento)
240700171218     C                   SELECT
240800171218     C     $LOP          WHENEQ    1
240900171218     C                   MOVE      'D1'          $GEST
241000171218     C     $LOP          WHENEQ    2
241100171218     C                   MOVE      'S1'          $GEST
241200171218     C                   EXSR      INZD1
241300171218     C                   MOVE      *OFF          $INZD1
241400171218     C                   EXSR      DEFORD
241500171218     C                   ENDSL
241600171218     C* Esegue sempre inizializzazione videata parzializzazioni
241700171218     C* perchè controlla nella ULTOPZ il campo D1FSC
241800171218     C                   EXSR      INZD1
241900171218     C                   MOVE      *OFF          $INZD1
242000171218     C*
242100171218     C* per decodificare la videata richiamo la routine dei ctrl
242200171218     C* e poi disattivo gli eventuali messaggi di errore
242300171218     C                   EXSR      CTRD1
242400171218     C*
242500171218     C                   MOVE      *ALL'0'       IN5098
242600171218     C                   MOVEA     IN5098        *IN(50)
242700171218     C*
242800171218     C* Imposto campo WWWSOC
242900171218     C*
243000171218     C     D1FSC         IFEQ      'C'
243100171218     C                   MOVE      XSCCPA        WWWSOC
243200171218     C                   ELSE
243300171218     C                   MOVE      XSCSOC        WWWSOC
243400171218     C                   END
243500171218     C* Ulteriore definizione delle opzioni utilizzabili a pgm
243600171218     C*
243700171218     C                   EXSR      ULTOPZ
243800171218     C*
243900171218     C                   MOVE      D1FSC         WFSC
244000171218     C*
244100171218     C* Valorizzazione campi univoci testate
244200171218     C*
244300171218     C                   CLEAR                   FMTT1
244400171218     C                   MOVEL     KNSIF         NOMSIF
244500171218     C                   MOVEL     XSCDSI        NOMDIT
244600171219     C                   MOVEL     KCDAZ         NOMAZN
244700171218     C****               MOVEL     DSPGM         NOMPGM
244800171218     C*
244900171218     C                   ENDSR
245000171218     C/EJECT
245100171218     C************************************************************
245200171218     C* REPERIMENTO DATI SOCIETA'
245300171218     C************************************************************
245400171218     C     REPSOC        BEGSR
245500171218     C*
245600171218     C                   CALLb     'XSOC'
245700171218     C                   PARM                    TIPXSC            6
245800171218     C                   PARM                    SOCXSC            3
245900171218     C                   PARM                    CDSXSC            9 0
246000171218     C                   PARM                    MODXSC            3
246100171218     C                   PARM      *BLANKS       RTNXSC            1
246200171218     C                   PARM                    XSOCDS
246300171218     C                   PARM                    KPJBA
246400171218     C*
246500171218     C                   ENDSR
246600171218     C/EJECT
246700171218     C************************************************************
246800171218     C* REPERIMENTO AUTORIZZAZIONI UTENTE
246900171218     C************************************************************
247000171218     C     REPAUT        BEGSR
247100171218     C*
247200171218     C                   CLEAR                   AUTDS
247300171218     C*
247400171218     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
247500171218     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
247600171218     C                   Z-ADD     0             XCAGRP                         GRUPPO
247700171218     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
247800171218     C                   MOVEL     'ANGIVA'      XCAFNC                         FUNZIONE
247900171218    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
248000171218    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
248100171218    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
248200171218     C*
248300171218     C                   CALLb     'XCHKAUT'
248400171218     C                   PARM                    AUTDS
248500171218     C*
248600171218     C* errori/non abilitazione
248700171218     C     XCARTN        IFNE      0
248800171218     C                   EXSR      ENDPGM
248900171218     C                   ELSE
249000171218     C                   MOVEL     XCADDS        ANGIVADS
249100171218     C                   ENDIF
249200171218     C*
249300171218     C                   ENDSR
249400171218     C/EJECT
249500171218     C************************************************************
249600171218     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
249700171218     C************************************************************
249800171218     C     REPDFT        BEGSR
249900171218     C*
250000171218     C* preparo i parametri di chiamata al driver
250100171218     C                   CLEAR                   XMPEDS
250200171218     C                   EVAL      TpChiamata = '01'
250300171218     C                   EVAL      NChiavi = 5
250400171218     C                   EVAL      VLogica = 2
250500171218     C*
250600171218     C* preparo le chiavi per la chiamata al driver
250700171218     C                   CLEAR                   XMPEDS1
250800171218     C                   MOVEL     XSCSOC        DSMPESOC
250900171218     C                   MOVEL     DSPGM         DSMPEPGM
251000171218     C                   MOVEL     KNMUS         DSMPEPRF
251100171218     C* Si reperiscono i dati di default a seconda delle modalità con
251200171218     C* cui il pgm è usato
251300171218     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
251400171218     C* MPETIP = 'PV'  pgm richiamato da menù
251500171218     C* MPETIP = 'PB'  x lancio Batch
251600171218     C                   SELECT
251700171218    >C     OPZI30        WHENEQ    '01'
251800171218     C                   MOVEL     'PX'          DSMPETIP
251900171218     C                   OTHER
252000171218     C                   MOVEL     'PV'          DSMPETIP
252100171218     C                   ENDSL
252200171218     C*
252300171218     C                   CALLB     'XMPE'
252400171218     C                   PARM                    XMPEDS
252500171218     C                   PARM                    XMPEDS1
252600171218     C*
252700171218     C     Risultato     IFEQ      '0'
252800171218     C                   MOVEL     DSMPEPAR      DSMPE
252900171218     C* nel caso sia la prima volta che l'utente apre questa anagrafica
253000171218     C* i suoi valori di default vengono decisi a pgm
253100171218     C                   ELSE
253200171218     C                   Z-ADD     1             $LOP
253300171218     C                   Z-ADD     1             $ORD
253400171218     C                   MoveL     *Blank        $TpReg
253500171218     C                   MoveL     *Blank        $Libro
253600171218     C                   MoveL     *Blank        $TpRIC
253700171218     C                   ENDIF
253800171218     C*
253900171218     C                   ENDSR
254000171218     C/EJECT
254100171218     C************************************************************
254200171218     C* REPERIMENTO PARAMETRI GESTIONE
254300171218     C************************************************************
254400171218     C     REPPAR        BEGSR
254500171218     C*
254600171218     C                   CALLb     'XPAR'
254700171218     C                   PARM                    XPASOC            3
254800171218     C                   PARM                    XPAPAR            8
254900171218     C                   PARM                    XPAOUT
255000171218     C                   PARM                    XPAERR            1
255100171218     C                   PARM                    XPARIC            1
255200171218     C*
255300171218     C                   ENDSR
255400171218     C/EJECT
255500171218     C************************************************************
255600171218     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
255700171218     C************************************************************
255800171218     C     DEFOPZ        BEGSR
255900171218     C*
256000171218     C* Questa routine deve valorizzare la schiera $FL1
256100171218     C* con S o N a seconda della utilizzabilità o meno
256200171218     C* dell'opzione correlata
256300171218     C*
256400171218     C* Inizialmente si considerano tutte le opzioni come valide
256500171218     C     1             DO        $DIMOP        X
256600171218     C                   MOVE      'S'           $FL1(X)
256700171218     C                   ENDDO
256800171218     C*
256900171218     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
257000171218     C* con 1 o 0 a seconda della utilizzabilità o meno
257100171218     C* del tasto funzione correlato
257200171218     C*
257300171218     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
257400171218     C* di compilazione
257500171218     C                   RESET                   $FC1
257600171218     C                   RESET                   $FR1
257700171218     C                   RESET                   $FC2
257800171218     C                   RESET                   $FR2
257900171218     C                   RESET                   $FC3
258000171218     C                   RESET                   $FR3
258100171218     C*
258200171218     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
258300171218     C* invalido alcune opzioni
258400171218     C                   SELECT
258500171218     C* Se pgm richiamato in ricerca disattivo :
258600171218    >C     OPZI30        WHENEQ    '01'
258700171218     C* F3=Fine
258800171218     C                   Z-ADD     3             X
258900171218     C                   MOVE      '0'           $FC1(X)
259000171218     C                   MOVE      '0'           $FC2(X)
259100171218     C                   MOVE      '0'           $FC3(X)
259200171218     C*
259300171218    >C*  aggiungere nollo stesso modo le altre opzioni da
259400171218    >C*  disattivare (quelle specifiche per ogni pgm)
259500171218     C*
259600171218     C* Se pgm richiamato in interrogazione disattivo :
259700171218    >C     OPZI30        WHENEQ    '05'
259800171218     C* F10=Immissione
259900171218     C                   Z-ADD     10            X
260000171218     C                   MOVE      '0'           $FC1(X)
260100171218     C                   MOVE      '0'           $FC2(X)
260200171218     C* 1=Selezione
260300171218     C                   Z-ADD     1             X
260400171218     C     01            LOOKUP    $OP1(X)                                21
260500171218     C     *IN21         IFEQ      *ON
260600171218     C                   MOVE      'N'           $FL1(X)
260700171218     C                   ENDIF
260800171218     C* 2=Modifica
260900171218     C                   Z-ADD     1             X
261000171218     C     02            LOOKUP    $OP1(X)                                21
261100171218     C     *IN21         IFEQ      *ON
261200171218     C                   MOVE      'N'           $FL1(X)
261300171218     C                   ENDIF
261400171218     C* 3=Copia
261500171218     C                   Z-ADD     1             X
261600171218     C     03            LOOKUP    $OP1(X)                                21
261700171218     C     *IN21         IFEQ      *ON
261800171218     C                   MOVE      'N'           $FL1(X)
261900171218     C                   ENDIF
262000171218     C* 4=Annullamento
262100171218     C                   Z-ADD     1             X
262200171218     C     04            LOOKUP    $OP1(X)                                21
262300171218     C     *IN21         IFEQ      *ON
262400171218     C                   MOVE      'N'           $FL1(X)
262500171218     C                   ENDIF
262600171218     C*11=Abilita
262700171218     C                   Z-ADD     1             X
262800171218     C     11            LOOKUP    $OP1(X)                                21
262900171218     C     *IN21         IFEQ      *ON
263000171218     C                   MOVE      'N'           $FL1(X)
263100171218     C                   ENDIF
263200171218     C*
263300171218     C     OPZI30        WHENEQ    *BLANKS
263400171218     C* Se pgm non richiamato, disattivo subito :
263500171218     C* F8=Selezione seguente
263600171218     C                   Z-ADD     8             X
263700171218     C                   MOVE      '0'           $FC1(X)
263800171218     C                   MOVE      '0'           $FC2(X)
263900171218     C* 1=Selezione
264000171218     C                   Z-ADD     1             X
264100171218     C     01            LOOKUP    $OP1(X)                                21
264200171218     C     *IN21         IFEQ      *ON
264300171218     C                   MOVE      'N'           $FL1(X)
264400171218     C                   ENDIF
264500171218     C*
264600171218     C* In tutti gli altri casi, disattivo subito :
264700171218     C                   OTHER
264800171218     C* 1=Selezione
264900171218     C                   Z-ADD     1             X
265000171218     C     01            LOOKUP    $OP1(X)                                21
265100171218     C     *IN21         IFEQ      *ON
265200171218     C                   MOVE      'N'           $FL1(X)
265300171218     C                   ENDIF
265400171218     C*
265500171218     C                   ENDSL
265600171218     C*
265700171218     C                   ENDSR
265800171218     C/EJECT
265900171218     C************************************************************
266000171218     C* ULTERIORI DEFINIZIONE OPZIONI UTILIZZABILI A PGM
266100171218     C************************************************************
266200171218     C     ULTOPZ        BEGSR
266300171218     C*
266400171218     C* Questa routine deve eseguire una ulteriore definizione
266500171218     C* delle opzioni a seconda se siamo in logica di gruppo o meno
266600171218     C* e, se lo siamo, a seconda della società richiesta (capofila
266700171218     C* oppure di riferimento)
266800171218     C*
266900171218     C                   SELECT
267000171218     C*
267100171218     C* se siamo in logica di gruppo per società disattivo :
267200171218     C*
267300171218     C     XSCSOC        WHENNE    XSCCPA
267400171218     C     GSP045        ANDEQ     'G'
267500171218     C     D1FSC         ANDEQ     'S'
267600171218     C* F10=Immissione
267700171218     C                   Z-ADD     10            X
267800171218     C                   MOVE      '0'           $FC1(X)
267900171218     C                   MOVE      '0'           $FC2(X)
268000171218     C* 3=Copia
268100171218     C                   Z-ADD     1             X
268200171218     C     03            LOOKUP    $OP1(X)                                21
268300171218     C     *IN21         IFEQ      *ON
268400171218     C                   MOVE      'N'           $FL1(X)
268500171218     C                   ENDIF
268600171218     C* 11=Abilita
268700171218     C                   Z-ADD     1             X
268800171218     C     11            LOOKUP    $OP1(X)                                21
268900171218     C     *IN21         IFEQ      *ON
269000171218     C                   MOVE      'N'           $FL1(X)
269100171218     C                   ENDIF
269200171218     C* a seconda delle autorizzazioni
269300171218     C* - il ctrl sull'autorizzazione alla manutenzione viene
269400171218     C*   demandato al pgm chiamato
269500171218     C*   altrimenti non si riuscirebbe a chiamare il filtro anche se
269600171218     C*   si è abilitati ad altre funzioni
269700171218     C* - annullamento
269800171218     C     ANNDLI        IFEQ      *OFF
269900171218     C* 4=Annullamento
270000171218     C                   Z-ADD     1             X
270100171218     C     04            LOOKUP    $OP1(X)                                21
270200171218     C     *IN21         IFEQ      *ON
270300171218     C                   MOVE      'N'           $FL1(X)
270400171218     C                   ENDIF
270500171218     C                   ENDIF
270600171218     C* - il ctrl sull'autorizzazione alla visualizzazione viene
270700171218     C*   demandato al pgm chiamato
270800171218     C*   altrimenti non si riuscirebbe a chiamare il filtro anche se
270900171218     C*   si è abilitati ad altre funzioni
271000171218     C*
271100171218     C* se siamo in logica di gruppo per capofila disattivo :
271200171218     C*
271300171218     C     XSCSOC        WHENNE    XSCCPA
271400171218     C     GSP045        ANDEQ     'G'
271500171218     C     D1FSC         ANDEQ     'C'
271600171218     C* F10=Immissione
271700171218     C                   Z-ADD     10            X
271800171218     C                   MOVE      '0'           $FC1(X)
271900171218     C                   MOVE      '0'           $FC2(X)
272000171218     C* tutte le opzioni
272100171218     C                   DO        $DIMOP        X
272200171218     C                   MOVE      'N'           $FL1(X)
272300171218     C                   ENDDO
272400171218     C* tranne
272500171218     C* 11=Abilita (a seconda dell'autorizzazione)
272600171218     C     ABIDLI        IFEQ      *ON
272700171218     C                   Z-ADD     1             X
272800171218     C     11            LOOKUP    $OP1(X)                                21
272900171218     C     *IN21         IFEQ      *ON
273000171218     C                   MOVE      'S'           $FL1(X)
273100171218     C                   ENDIF
273200171218     C                   ENDIF
273300171218     C* - il ctrl sull'autorizzazione alla visualizzazione viene
273400171218     C*   demandato al pgm chiamato
273500171218     C*   altrimenti non si riuscirebbe a chiamare il filtro anche se
273600171218     C*   si è abilitati ad altre funzioni
273700171218     C*   quindi l'abilito comunque
273800171218     C                   Z-ADD     1             X
273900171218     C     05            LOOKUP    $OP1(X)                                21
274000171218     C     *IN21         IFEQ      *ON
274100171218     C                   MOVE      'S'           $FL1(X)
274200171218     C                   ENDIF
274300171218     C*
274400171218     C* se siamo in logica di società disattivo :
274500171218     C*
274600171218     C     XSCSOC        WHENEQ    XSCCPA
274700171218     C     GSP045        ORNE      'G'
274800171218     C* 11=Abilita
274900171218     C                   Z-ADD     1             X
275000171218     C     11            LOOKUP    $OP1(X)                                21
275100171218     C     *IN21         IFEQ      *ON
275200171218     C                   MOVE      'N'           $FL1(X)
275300171218     C                   ENDIF
275400171218     C* e, a seconda delle autorizzazioni
275500171218     C* - immissione/copia
275600171218     C     IMMDLI        IFEQ      *OFF
275700171218     C* F10=Immissione
275800171218     C                   Z-ADD     10            X
275900171218     C                   MOVE      '0'           $FC1(X)
276000171218     C                   MOVE      '0'           $FC2(X)
276100171218     C* 3=Copia
276200171218     C                   Z-ADD     1             X
276300171218     C     03            LOOKUP    $OP1(X)                                21
276400171218     C     *IN21         IFEQ      *ON
276500171218     C                   MOVE      'N'           $FL1(X)
276600171218     C                   ENDIF
276700171218     C                   ENDIF
276800171218     C* - il ctrl sull'autorizzazione alla manutenzione viene
276900171218     C*   demandato al pgm chiamato
277000171218     C*   altrimenti non si riuscirebbe a chiamare il filtro anche se
277100171218     C*   si è abilitati ad altre funzioni
277200171218     C* - annullamento
277300171218     C     ANNDLI        IFEQ      *OFF
277400171218     C* 4=Annullamento
277500171218     C                   Z-ADD     1             X
277600171218     C     04            LOOKUP    $OP1(X)                                21
277700171218     C     *IN21         IFEQ      *ON
277800171218     C                   MOVE      'N'           $FL1(X)
277900171218     C                   ENDIF
278000171218     C                   ENDIF
278100171218     c*
278200171218     C* - il ctrl sull'autorizzazione alla visualizzazione viene
278300171218     C*   demandato al pgm chiamato
278400171218     C*   altrimenti non si riuscirebbe a chiamare il filtro anche se
278500171218     C*   si è abilitati ad altre funzioni
278600171218     C*
278700171218     C                   ENDSL
278800171218     C*
278900171218     C                   ENDSR
279000171218     C/EJECT
279100171218     C************************************************************
279200171218     C* REPERIMENTO OPZIONI
279300171218     C************************************************************
279400171218     C     REPOPZ        BEGSR
279500171218     C*
279600171218     C     1             DO        $DIMOP        X
279700171218     C*
279800171218     C     $MS1(X)       IFNE      *BLANK
279900171218     C*
280000171218     C                   MOVEL     $MS1(X)       IDMSG
280100171218     C                   CALLb     'XRTVM'
280200171218     C                   PARM                    IDMSG            27
280300171218     C                   PARM                    TXTMSG          100
280400171218     C                   PARM                    ERRMSG            1
280500171218     C*
280600171218     C     ERRMSG        IFNE      *ON
280700171218     C                   MOVEL     TXTMSG        $TX1(X)
280800171218     C                   ELSE
280900171218     C                   MOVEL     *ALL'?'       $TX1(X)
281000171218     C                   ENDIF
281100171218     C*
281200171218     C                   ENDIF
281300171218     C*
281400171218     C                   ENDDO
281500171218     C*
281600171218     C                   ENDSR
281700171218     C/EJECT
281800171218     C************************************************************
281900171218     C* REPERIMENTO TASTI DI FUNZIONE
282000171218     C************************************************************
282100171218     C     REPKEY        BEGSR
282200171218     C                   MOVEA     $FM1          $FM
282300171218     C                   MOVEA     $FC1          $FC
282400171218     C                   EXSR      REPKKK
282500171218     C                   MOVEA     $FX           $FX1
282600171218     C*
282700171218     C                   MOVEA     $FM2          $FM
282800171218     C                   MOVEA     $FC2          $FC
282900171218     C                   EXSR      REPKKK
283000171218     C                   MOVEA     $FX           $FX2
283100171218     C*
283200171218     C                   MOVEA     $FM3          $FM
283300171218     C                   MOVEA     $FC3          $FC
283400171218     C                   EXSR      REPKKK
283500171218     C                   MOVEA     $FX           $FX3
283600171218     C                   ENDSR
283700171218     C/EJECT
283800171218     C************************************************************
283900171218     C* REPERIMENTO TASTI DI FUNZIONE
284000171218     C************************************************************
284100171218     C     REPKKK        BEGSR
284200171218     C*
284300171218     C                   CLEAR                   $FX
284400171218     C     1             DO        24            X
284500171218     C*
284600171218     C     $FM(X)        IFNE      *BLANK
284700171218     C     $FC(X)        ANDNE     '0'
284800171218     C*
284900171218     C                   MOVEL     $FM(X)        IDMSG
285000171218     C                   CALLb     'XRTVM'
285100171218     C                   PARM                    IDMSG            27
285200171218     C                   PARM                    TXTMSG          100
285300171218     C                   PARM                    ERRMSG            1
285400171218     C*
285500171218     C     ERRMSG        IFNE      *ON
285600171218     C                   MOVEL     TXTMSG        $FX(X)
285700171218     C                   ELSE
285800171218     C                   MOVEL     *ALL' '       $FX(X)
285900171218     C                   ENDIF
286000171218     C*
286100171218     C                   ENDIF
286200171218     C*
286300171218     C                   ENDDO
286400171218     C*
286500171218     C                   ENDSR
286600171218     C/EJECT
286700171218     C************************************************************
286800171218     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
286900171218     C************************************************************
287000171218     C     DEFCAM        BEGSR
287100171218     C*
287200171218     C* klist
287300171218     C*
287400171218     C* SDGRIC01L
287500171218     C     K03RIC01      KLIST
287600171218     C                   KFLD                    RICSocieta
287700171218     C                   KFLD                    RICTpReg
287800171218     C                   KFLD                    RICLibro
287900171218     C*
288000171218     C     K02RIC01      KLIST
288100171218     C                   KFLD                    RICSOCIETA
288200171218     C                   KFLD                    RICTPREG
288300171218     C* ANDLI01L
288400171218     C     K03DLI01      KLIST
288500171218     C                   KFLD                    DLISOCIETA
288600171218     C                   KFLD                    DLITPREG
288700171218     C                   KFLD                    DLILIBRO
288800171218     C*
288900171218     C* Variabili per gestione videate
289000171218     C*
289100171218     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
289200171218     C                   MOVE      *BLANK        $FINE             1            fine pgm
289300171218     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
289400171218     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
289500171218     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
289600171218     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
289700171218     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
289800171218     C                   MOVE      *BLANK        $EFILE            1            memo fine file
289900171218     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
290000171218     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
290100171218     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
290200171218     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
290300171218     C                   MOVE      *BLANK        $UNIQ             1            gest.POSTA
290400171218     C*
290500171218     C* Variabili appoggio sempre presenti in tutte le anagrafiche
290600171218     C*
290700171218     C     *LIKE         DEFINE    XSCSOC        WWWSOC
290800171218     C     *LIKE         DEFINE    C1RCD         S1NRR
290900171218     C     *LIKE         DEFINE    C1NRR         WSFL
291000171218     C     *LIKE         DEFINE    C1RCD         WMAX
291100171218     C     *LIKE         DEFINE    C1RCD         WPAG
291200171218     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
291300171218     C     *LIKE         DEFINE    *INKR         WINKR                                      ne S1
291400171218     C     *LIKE         DEFINE    OPRI30        WOPR                                       ne S1
291500171218     C     *LIKE         DEFINE    D1FSC         WFSC                                       ne S1
291600171218     C     *LIKE         DEFINE    D1Libro       Libro999                                   ne S1
291700171218     C                   MOVE      *BLANK        WWGRP             3
291800171218     C                   Z-ADD     0             CURR              2 0
291900171218     C                   Z-ADD     0             CURC              2 0
292000171218     C                   MOVE      *BLANK        WIN              99
292100171218     C     *LIKE         DEFINE    C1POS         W1POS
292200171218     C     *LIKE         DEFINE    RICSOCIETA    WSOC
292300171218    >C* aggiungere definizione altri campi posizionamento se ci sono
292400171218     C*
292500171218     C* Indici
292600171218     C*
292700171218     C                   Z-ADD     0             X                 3 0
292800171218     C                   Z-ADD     0             Y                 3 0
292900171218     C*
293000171218     C                   ENDSR
293100171218     C/EJECT
293200171218**
293300171218PROMSG    *LIBL     OPZ0001
293400171218PROMSG    *LIBL     OPZ0002
293500171218PROMSG    *LIBL     OPZ0003
293600171218PROMSG    *LIBL     OPZ0004
293700171218PROMSG    *LIBL     OPZ0005
293800171218PROMSG    *LIBL     OPZ0013
293900171218**
29400017121801S      <Selezione
29410017121802S      <Modifica
29420017121803S      <Copia
29430017121804S      <Annullamento
29440017121805S      <Visualizzazione
29450017121811S      <Abilita
294600171218** TASTI DI FUNZIONE  D1
294700171218PROMSG    *LIBL     KEY0001  01
294800171218PROMSG    *LIBL     KEY0002  02
294900171218PROMSG    *LIBL     KEY0003  03
295000171218PROMSG    *LIBL     KEY0004  04
295100171218PROMSG    *LIBL     KEY0005  05
295200171218PROMSG    *LIBL     KEY0006  06
295300171218PROMSG    *LIBL     KEY0007  07
295400171218PROMSG    *LIBL     KEY0008  08
295500171218PROMSG    *LIBL     KEY0009  09
295600171218PROMSG    *LIBL     KEY0010  10
295700171218PROMSG    *LIBL     KEY0011  11
295800171218PROMSG    *LIBL     KEY0012  12
295900171218PROMSG    *LIBL     KEY0013  13
296000171218PROMSG    *LIBL     KEY0014  14
296100171218PROMSG    *LIBL     KEY0015  15
296200171218PROMSG    *LIBL     KEY0016  16
296300171218PROMSG    *LIBL     KEY0017  17
296400171218PROMSG    *LIBL     KEY0018  18
296500171218PROMSG    *LIBL     KEY0019  19
296600171218PROMSG    *LIBL     KEY0020  20
296700171218PROMSG    *LIBL     KEY0021  21
296800171218PROMSG    *LIBL     KEY0022  22
296900171218PROMSG    *LIBL     KEY0023  23
297000171218PROMSG    *LIBL     KEY0024  24
297100171218**  ABILITAZIONE D1
2972001712180N  01
2973001712191N  02
2974001712181N  03
2975001712181N  04
2976001712180N  05
2977001712180N  06
2978001712180N  07
2979001712181N  08
2980001712180N  09
2981001712181N  10
2982001712180N  11
2983001712181N  12
2984001712180N  13
2985001712180N  14
2986001712180N  15
2987001712180N  16
2988001712180N  17
2989001712181N  18
2990001712180N  19
2991001712180N  20
2992001712180N  21
2993001712180N  22
2994001712180N  23   Impostare a '1' se si utilizzano opzioni
2995001712181N  24   Lasciare sempre impostata a '1' il valore della riga 24
299600171218** TASTI DI FUNZIONE  S1
299700171218PROMSG    *LIBL     KEY0001  01
299800171218PROMSG    *LIBL     KEY0002  02
299900171218PROMSG    *LIBL     KEY0003  03
300000171218PROMSG    *LIBL     KEY0004  04
300100171218PROMSG    *LIBL     KEY0005  05
300200171218PROMSG    *LIBL     KEY0006  06
300300171218PROMSG    *LIBL     KEY0007  07
300400171218PROMSG    *LIBL     KEY0008  08
300500171218PROMSG    *LIBL     KEY0009  09
300600171218PROMSG    *LIBL     KEY0010  10
300700171218PROMSG    *LIBL     KEY0011  11
300800171218PROMSG    *LIBL     KEY0012  12
300900171218PROMSG    *LIBL     KEY0013  13
301000171218PROMSG    *LIBL     KEY0014  14
301100171218PROMSG    *LIBL     KEY0015  15
301200171218PROMSG    *LIBL     KEY0016  16
301300171218PROMSG    *LIBL     KEY0017  17
301400171218PROMSG    *LIBL     KEY0018  18
301500171218PROMSG    *LIBL     KEY0019  19
301600171218PROMSG    *LIBL     KEY0020  20
301700171218PROMSG    *LIBL     KEY0021  21
301800171218PROMSG    *LIBL     KEY0022  22
301900171218PROMSG    *LIBL     KEY0023  23
302000171218PROMSG    *LIBL     KEY0024  24
302100171218**  ABILITAZIONE S1
3022001712180N  01
3023001712180N  02
3024001712181N  03
3025001712180N  04
3026001712180N  05
3027001712180N  06
3028001712180N  07
3029001712181N  08
3030001712180N  09
3031001712181N  10
3032001712180N  11
3033001712181N  12
3034001712180N  13
3035001712180N  14
3036001712180N  15
3037001712180N  16
3038001712181N  17
3039001712181N  18
3040001712180N  19
3041001712180N  20
3042001712180N  21
3043001712180N  22
3044001712181N  23   Impostare a '1' se si utilizzano opzioni
3045001712181N  24   Lasciare sempre impostata a '1' il valore della riga 24
304600171218** TASTI DI FUNZIONE  D3
304700171218PROMSG    *LIBL     KEY0001  01
304800171218PROMSG    *LIBL     KEY0002  02
304900171218PROMSG    *LIBL     KEY0003  03
305000171218PROMSG    *LIBL     KEY0004  04
305100171218PROMSG    *LIBL     KEY0005  05
305200171218PROMSG    *LIBL     KEY0006  06
305300171218PROMSG    *LIBL     KEY0007  07
305400171218PROMSG    *LIBL     KEY0008  08
305500171218PROMSG    *LIBL     KEY0009  09
305600171218PROMSG    *LIBL     KEY0010  10
305700171218PROMSG    *LIBL     KEY0011  11
305800171218PROMSG    *LIBL     KEY0012  12
305900171218PROMSG    *LIBL     KEY0013  13
306000171218PROMSG    *LIBL     KEY0014  14
306100171218PROMSG    *LIBL     KEY0015  15
306200171218PROMSG    *LIBL     KEY0016  16
306300171218PROMSG    *LIBL     KEY0017  17
306400171218PROMSG    *LIBL     KEY0018  18
306500171218PROMSG    *LIBL     KEY0019  19
306600171218PROMSG    *LIBL     KEY0020  20
306700171218PROMSG    *LIBL     KEY0021  21
306800171218PROMSG    *LIBL     KEY0022  22
306900171218PROMSG    *LIBL     KEY0023  23
307000171218PROMSG    *LIBL     KEY0024  24
307100171218**  ABILITAZIONE D3
3072001712180N  01
3073001712180N  02
3074001712181N  03
3075001712180N  04
3076001712180N  05
3077001712180N  06
3078001712180N  07
3079001712180N  08
3080001712180N  09
3081001712180N  10
3082001712180N  11
3083001712181N  12
3084001712180N  13
3085001712180N  14
3086001712180N  15
3087001712180N  16
3088001712180N  17
3089001712180N  18
3090001712180N  19
3091001712180N  20
3092001712180N  21
3093001712180N  22
3094001712180N  23   Impostare a '1' se si utilizzano opzioni
3095001712181N  24   Lasciare sempre impostata a '1' il valore della riga 24
