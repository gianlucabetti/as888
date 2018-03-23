000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200970619     H*PARMS CVTOPT(*NONE)
000300970619     H DECEDIT('0,') DATEDIT(*DMY.)
000400970619      *
000500101111      *
000600101111      *            ------------------------------
000700101111      *             Guida stampa file Black List
000800101111      *            ------------------------------
000900970619      *
001000101111      *
001100970619     F*----------------------------------------------------*
001200970619     F*------------
001300970623    >FANCFM02L  IF   E           K DISK
001400970623    >F                                     RENAME(ANCFM000:ANCFM02)
001500970620     F*------------
001600970620    >FANSIF01L  IF   E           K DISK
001700970619     F*------------
001800101111    >FSDGSD14D  CF   E             WORKSTN
001900101111    >F                                     RENAME(SD14D1:FMTD1)
002000101111    >F                                     RENAME(SD14D2:FMTD2)
002100101111    >F                                     RENAME(SD14D3:FMTD3)
002200101111    >F                                     RENAME(SD14S1:FMTS1)
002300101111    >F                                     RENAME(SD14C1:FMTC1)
002400101111    >F                                     RENAME(SD14T1:FMTT1)
002500101111    >F                                     RENAME(SD14Z1:FMTZ1)
002600101111    >F                                     RENAME(SD14Z2:FMTZ2)
002700101111    >F                                     RENAME(SD14Z3:FMTZ3)
002800970619     F                                     SFILE(FMTS1:S1NRR)
002900970619     F                                     INFDS(DSFMT)
003000970619     D*----------------------------------------------------*
003100970619     D* Schiere per la gestione delle opzioni
003200970619     D*
003300970619     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003400970619     D $TX1            S             50    DIM($dimop) ALT($MS1)
003500970619     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003600970619     D $FL1            S              1    DIM($dimop) ALT($OP1)
003700970619     D* schiera contenente i nomi dei programma richiamati attraverso
003800970619     D* le opzioni personalizzate
003900970619     D $PGO1           S             10    DIM($dimop)
004000970619     D $TX             S             50    DIM(50)
004100970619     D $FL             S              1    DIM(50)
004200970619     D* Schiere generali per la gestione dei tasti di funzione
004300970619     D $FM             S             27    DIM(24)
004400970619     D $FX             S             50    DIM(24)
004500970619     D $FC             S              1    DIM(24)
004600970619     D $FR             S              1    DIM(24)
004700970619     D* Schiere per la gestione dei tasti di funzione
004800970619     D* Video D1
004900970619     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005000970619     D $FX1            S             50    DIM(24) ALT($FM1)
005100970619     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005200970619     D $FR1            S              1    DIM(24) ALT($FC1)
005300970619     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005400970619     D* funzione personalizzata S1
005500970619     D $PGF2           S             10    DIM(25)
005600970619     D* Schiere per la gestione dei tasti di funzione
005700970619     D* Video S1
005800970619     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005900970619     D $FX2            S             50    DIM(24) ALT($FM2)
006000970619     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006100970619     D $FR2            S              1    DIM(24) ALT($FC2)
006200970619     D* Schiere per la gestione dei tasti di funzione
006300970619     D* Video D2
006400970619     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
006500970619     D $FX3            S             50    DIM(24) ALT($FM3)
006600970619     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
006700970619     D $FR3            S              1    DIM(24) ALT($FC3)
006800970619     D*
006900970619     D $D1L01          S              3  0
007000970619     D $D1L02          S              3  0
007100970619     D $S1L01          S              3  0
007200970619     D $S1L02          S              3  0
007300970619     D $C1Op1          S              3  0
007400970619     D $C1Op2          S              3  0
007500970619     D $D2L01          S              3  0
007600970619     D $D2L02          S              3  0
007700970619     D*
007800970619     D*-------------
007900970619     D* Indice della schiera tasti funzione
008000970619     D PosFun          S              2  0
008100970619     D*-------------
008200970619     D* Valori assunti
008300970619     D XMPEDS        E DS                  INZ
008400970619     D XMPEDS1       E DS                  INZ
008500970619     D DSMPE           DS                  INZ
008600970619     D  $LOP                   1      1  0
008700970619     D  $ORD                   2      3  0
008800970619     D*-------------
008900970619     D KPJBA         E DS
009000970619     D*-------------
009100970619     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009200970619     D*-------------
009300970619     D DSAUT           DS           512
009400970619     D*-------------
009500101111     D* Dati di ambiente ottenuti da SDGSOCDR
009600101111     D SOC001        E DS                  EXTNAME(SDGSOCDS )
009700970619     D*-------------
009800101111     D* DS Interna per dati di output di SDGSOCDR
009900970619     D XSOCDS          DS          1000
010000970619     D*-------------
010100970619     D* Parametri in ricezione
010200101111    >D SDGSD14DS     E DS
010300101111     D*
010400131122     D SDGSD11DS     E DS
010500131119R132 D SDGSG61DS     E DS
010600970619     D*-------------
010700970619     D* Parametri del PGM di manutenzione ANA123R
010800101111     D ANA123DS      E DS                  inz
010900101111     D  RET123       E                     INZ('0')
011000101111     D  OPR123       E                     INZ('0')
011100101111     D  ERR123       E                     INZ('0')
011200970619     D*-------------
011300970619     D* campo di salvataggio per la DS d'ingresso durante la chiamata
011400970619     D* alla funzione personalizzata
011500101111    >D $SaveDS         S                   like(SDGSD14DS)
011600970619     D*-------------
011700970620     D* Ricerca società
011800970620     D A113DS        E DS                  EXTNAME(ANA113DS) INZ
011900970620     D*-------------
012000970620     D* Ricerca legende
012100970620     D A964DS        E DS                  EXTNAME(ANA964DS) INZ
012200970620     D*-------------
012300970620     D* Controlli validità per valore di una legenda
012400970620     D X07DS         E DS                  EXTNAME(X07VALDS)
012500970620     D*-------------
012600970619     D DSFMT           DS           512
012700970619     D  $TASTO               369    369
012800970619     D  NRG                  370    370
012900970619     D  NCL                  371    371
013000970619     D  SFLNRR               378    379B 0
013100970619     D*-------------
013200970619     D* posizione cursore
013300970619     D CURSOR          DS
013400970619     D  RIRI                   1      2B 0 INZ
013500970619     D  R$$                    2      2
013600970619     D  COCO                   3      4B 0 INZ
013700970619     D  C$$                    4      4
013800970619     D*-------------
013900970619     D* Reperimento nome PGM
014000970619     D STATUS         SDS           333
014100970619     D  DSPGM            *PROC
014200970619     D  PARMS            *PARMS
014300970619     D*-------------
014400970619     D* Tasti di funzione
014500970619     D F01             C                   CONST(X'31')
014600970619     D F02             C                   CONST(X'32')
014700970619     D F03             C                   CONST(X'33')
014800970619     D F04             C                   CONST(X'34')
014900970619     D F05             C                   CONST(X'35')
015000970619     D F06             C                   CONST(X'36')
015100970619     D F07             C                   CONST(X'37')
015200970619     D F08             C                   CONST(X'38')
015300970619     D F09             C                   CONST(X'39')
015400970619     D F10             C                   CONST(X'3A')
015500970619     D F11             C                   CONST(X'3B')
015600970619     D F12             C                   CONST(X'3C')
015700970619     D F13             C                   CONST(X'B1')
015800970619     D F14             C                   CONST(X'B2')
015900970619     D F15             C                   CONST(X'B3')
016000970619     D F16             C                   CONST(X'B4')
016100970619     D F17             C                   CONST(X'B5')
016200970619     D F18             C                   CONST(X'B6')
016300970619     D F19             C                   CONST(X'B7')
016400970619     D F20             C                   CONST(X'B8')
016500970619     D F21             C                   CONST(X'B9')
016600970619     D F22             C                   CONST(X'BA')
016700970619     D F23             C                   CONST(X'BB')
016800970619     D F24             C                   CONST(X'BC')
016900970619     D ENTER           C                   CONST(X'F1')
017000970619     D ROLDWN          C                   CONST(X'F4')
017100970619     D ROLLUP          C                   CONST(X'F5')
017200970619     D*-------------
017300970619     D* Sottotitolo di testata
017400970619     D SOTTIT          C                   CONST('               -
017500970619     D                                                 ')
017600981028     D*-------------
017700981028     D* variabili di appoggio alla chiamata a x65runqry
017800981028     D $File           S             10A
017900981028     D $Lib            S             10A
018000981028     D $Membro         S             10A
018100981028     D $Output         S              1A
018200981030     D $Errore         S              1A
018300970619     D*-------------
018400970619     D DATMIN          C                   CONST('0001-01-01')
018500970619     D DATMAX          C                   CONST('9999-12-31')
018600101116     DOggi             S               D    datfmt(*iso)
018700101116     DData0            S               D    datfmt(*iso)
018800101116     DData1            S               D    datfmt(*iso)
018900101116     DData2            S               D    datfmt(*iso)
019000970619     D*-------------
019100970619     D* Variabili per gestione videate
019200970619     D $Gest           S              2A
019300970619     D $Fine           S              1A
019400970619     D $InzD1          S              1A
019500970619     D $InzS1          S              1A
019600970619     D $InzD2          S              1A
019700970619     D $LastG          S              2A
019800970619     D $LastV          S              2A
019900970619     D $EndFile        S              1A
020000970619     D $Esci           S              1A
020100970619     D $RcdOK          S              1A
020200970619     D $Unique         S              1A
020300970619     D*-------------
020400970619     D* Variabili appoggio sempre presenti in tutte le anagrafiche
020500970619     D S1Nrr           S                   like(C1Rcd)
020600970619     D WSfl            S                   like(C1Nrr)
020700970619     D WMax            S                   like(C1Rcd)
020800970619     D WPag            S                   like(C1Rcd)
020900970619     D WInzS1          S                   like($InzS1)
021000101111    >D WOpr            S                   like(OprD14)
021100970619     D CurR            S              2S 0
021200970619     D CurC            S              2S 0
021300970619     D Win             S             99A
021400970619     D W1Pos           S                   like(C1Pos)
021500970619     D WINKR           S              1A
021600970829     D WExf            S                   like(Exf123)
021700970619     D*-------------
021800970619     D*  Indici
021900970619     D X               S              3S 0
022000970619     D Y               S              3S 0
022100970619     D* Indice della schiera per le opzioni personalizzate
022200970619     D PosOpz          S              2  0
022300970619     D* numero max opzioni
022400970619     D NumOpz          S              2  0 Inz($DIMOP)
022500970619     D* numero effettive opzioni
022600970619     D OpzLen          S              2  0
022700970619     D*-------------
022800101111     D ZDateISO        S               D
022900101111     D*-------------
023000101111     D SFLPAG          C                   CONST(12)
023100101111     D $DIMOP          C                   CONST(30)
023200131119R132 D*-------------
023300131119  "  D* Sottotitolo di testata
023400131119  "  D* "Tracciato Polivalente "
023500131119  "  D SOTTITT1        C                   CONST('SDGMSG    *LIBL-
023600131119R132 D                                          SDC0077')
023700970619     C*----------------------------------------------------*
023800970619     C*                MAIN LINE PROGRAM
023900970619     C*----------------------------------------------------*
024000970619     C*
024100970619     C* inizializzazione variabili
024200101111     C                   Exsr      INZVAR
024300970619     C*
024400101111     C     $FINE         DoWEq     *OFF
024500101111     C     $GEST         CasEq     'D1'          GESD1                          Parzializz.
024600101111     C     $GEST         CasEq     'S1'          GESS1                          Lista
024700101111     C     $GEST         CasEq     'D2'          GESD2                          Valori ass.
024800101111     C                   EndCS
024900101111     C                   EndDo
025000970619     C* fine programma
025100101111     C                   Exsr      ENDPGM
025200970619     C*
025300970619     C************************************************************
025400970619     C* FINE PROGRAMMA
025500970619     C************************************************************
025600101111     C     ENDPGM        BegSr
025700970619     C*
025800970619     C* passaggio dati al pgm chiamante
025900101111     C                   MoveL     WOPR          OPRD14                                     ne S1
026000101111     C                   MoveL     SDGSD14DS     KPJBU
026100970619     C*
026200970619     C* Se pgm Guida non è stato richiamato posso chiudere in LR
026300101111     C     OPZD14        IFEQ      *BLANK
026400970619     C                   SETON                                            LR
026500970619     C* altrimenti chiudo in RT
026600970619     C                   ELSE
026700970619     C                   SETON                                            RT
026800101111     C                   EndIf
026900970619     C                   RETURN
027000970619     C*
027100101111     C                   EndSr
027200970619     C************************************************************
027300970619     C* RICERCHE
027400970619     C************************************************************
027500101111     C     SEARCH        BegSr
027600970619     C*
027700970619     C* determino Riga/Colonna del cursore
027800101111     C                   Move      NRG           R$$
027900101111     C                   Move      NCL           C$$
028000970619     C                   Z-ADD     RIRI          CURR
028100970619     C                   Z-ADD     COCO          CURC
028200970619     C*
028300101111     C                   Select
028400970619     C*
028500101111     C* FMTD1 - Società
028600101111     C     H1NMFL        WHENEQ    'D1SOCIETA'
028700101111     C                   RESET                   A113DS
028800101111     C                   MoveL     '01'          OPZ113
028900101111     c                   Move      '1'           INZ113
029000101111     C                   MoveL     A113DS        KPJBU
029100101111     C                   CALL      'ANA113R'
029200970620     C                   PARM                    KPJBA
029300101111     C                   MoveL     KPJBU         A113DS
029400970620     C     SOCIETA113    IFNE      *BLANK
029500101111     C                   MoveL     SOCIETA113    D1Societa
029600101111     C                   EndIf
029700101118     C*
029800101118     C*
029900101118     C     H1NMFL        WHENEQ    'D1STPL'
030000101118     C                   EXSR      F4_STPL
030100970620     C*
030200970619     C* ricerca non abilitata in questa posizione
030300101111     C                   OTHER
030400970619     C                   SETON                                        9899
030500970619     C*
030600101111     C                   EndSl
030700970619     C*
030800970619     C* imposto pos. del cursore
030900970619     C                   Z-ADD     CURR          H1RIGA
031000970619     C                   Z-ADD     CURC          H1COLO
031100970619     C*
031200101111     C                   Move      *HIVAL        $LASTV
031300970619     C*
031400101111     C                   EndSr
031500970619     C************************************************************
031600970619     C* RIEMPIMENTO OPZIONI
031700970619     C************************************************************
031800101111     C     RIEOPZ        BegSr
031900970619     C*
032000970619     C                   CALLB     'X04OPMSG'
032100970619     C* input
032200970619     C                   PARM                    $TX
032300970619     C                   PARM                    $FL
032400970619     C                   PARM                    WCOZ              3 0
032500970619     C                   PARM                    WMXO              3 0
032600970619     C* output
032700970619     C                   PARM                    WOPZ1            76
032800970619     C                   PARM                    WOPZ2            76
032900970619     C                   PARM                    FF23              1
033000970619     C* input
033100970619     C                   PARM                    $L01              3 0
033200970619     C                   PARM                    $L02              3 0
033300970619     C*
033400101111     C                   PARM                    $ALLOPT         960
033500101111     C*
033600101111     C                   EndSr
033700970619     C************************************************************
033800970619     C* RIEMPIMENTO TASTI FUNZIONE
033900970619     C************************************************************
034000101111     C     RIEKEY        BegSr
034100970619     C*
034200970619     C* Input :
034300970619     C* - $FX = schiera con tutti i testi disponibili
034400970619     C* - $FC = schiere con i flag di validità associati ai testi
034500970619     C* - $FR = schiere con i flag di REVERSE IMAGE
034600970619     C* - $ULK = ultimo testo caricato
034700970619     C* - $L01 = Lunghezza prima riga
034800970619     C* - $L02 = Lunghezza seconda riga
034900970619     C* Output :
035000970619     C* - $ULK
035100970619     C* - $KEY1= 1° riga di tasti funzione
035200970619     C* - $KEY2= 2° riga di tasti funzione
035300970619     C*
035400970619     C                   CALLB     'XKEYMSG'
035500970619     C* input
035600970619     C                   PARM                    $FX
035700970619     C                   PARM                    $FC
035800970619     C                   PARM                    $FR
035900970619     C                   PARM                    $ULK              3 0
036000970619     C* output
036100970619     C                   PARM                    $KEY1            79
036200970619     C                   PARM                    $KEY2            79
036300970619     C* input
036400970619     C                   PARM                    $L01              3 0
036500970619     C                   PARM                    $L02              3 0
036600970619     C*
036700101111     C                   PARM                    $ALLFUNCT       480
036800101111     C*
036900101111     C                   EndSr
037000970619     C************************************************************
037100970619     C* GESTIONE EMISSIONE TASTI FUNZIONE
037200970619     C************************************************************
037300101111     C     GESPIE        BegSr
037400970619     C*
037500101111     C                   Select
037600970619     C* Parzializzazioni
037700970619     C     $GEST         WHENEQ    'D1'
037800970619     C     $ULKD1        IFEQ      0
037900970619     C* riempimento opzioni
038000101111     C                   Exsr      F24D1
038100970619     C                   ELSE
038200970619     C                   WRITE     FMTZ1
038300101111     C                   EndIf
038400970619     C* Lista
038500970619     C     $GEST         WHENEQ    'S1'
038600970619     C     $ULKS1        IFEQ      0
038700101111     C                   Exsr      F24S1
038800970619     C                   ELSE
038900970619     C                   WRITE     FMTZ2
039000101111     C                   EndIf
039100970619     C* Modifica valori assunti
039200970619     C     $GEST         WHENEQ    'D2'
039300970619     C     $ULKD2        IFEQ      0
039400101111     C                   Exsr      F24D2
039500970619     C                   ELSE
039600970619     C                   WRITE     FMTZ3
039700101111     C                   EndIf
039800101111     C                   EndSl
039900970619     C*
040000101111     C                   EndSr
040100970619     C************************************************************
040200970619     C* GESTIONE PARZIALIZZAZIONI
040300970619     C************************************************************
040400101111     C     GESD1         BegSr
040500970619     C* inizializzazione videata
040600970619     C     $INZD1        IFEQ      *ON
040700101111     C                   Exsr      INZD1
040800101111     C                   Move      *OFF          $INZD1
040900101111     C                   EndIf
041000970619     C* emissione piede videata se proveniente da altra
041100970619     C* salvataggio valore formato di provenienza
041200970619     C     $LASTV        IFNE      'D1'
041300970619     C* salvo stato F17 per sapere cosa fare in caso di F12
041400101111     C                   Move      *INKR         WINKR
041500970619     C                   WRITE     FMTT1
041600101111     C                   Exsr      GESPIE
041700101111     C                   Move      $LASTV        $LASTG
041800101111     C                   Move      'D1'          $LASTV
041900101111     C                   EndIf
042000970619     C*
042100970619     C* se esistono errori sulla videata
042200970619     C* emetto la write del formato a indicatori spenti per vedere
042300970619     C* le eventuali decodifiche
042400970619     C     *IN99         IFEQ      *ON
042500101111     C                   MoveA     *IN           WIN
042600101111     C                   Move      *ALL'0'       IN5098           49
042700101111     C                   MoveA     IN5098        *IN(50)
042800970619     C                   WRITE     FMTD1
042900101111     C                   MoveA     WIN           *IN
043000101111     C                   EndIf
043100970619     C*
043200970619     C                   EXFMT     FMTD1
043300970619     C*
043400970619     C* azzero la posizione del cursore
043500970619     C                   Z-ADD     0             H1RIGA
043600970619     C                   Z-ADD     0             H1COLO
043700970619     C*
043800970619     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
043900970619     C* INPUT
044000970619     C* $FC1 = Abilitazioni videata D1
044100970619     C* OUTPUT
044200970619     C* *IN97 + *IN99 Errori
044300101111     C                   MoveA     $FC1          $FC
044400101111     C                   Exsr      CTRKEY
044500970619     C*
044600970619     C     *IN99         IFEQ      *OFF
044700970619     C*
044800101111     C                   Select
044900970619     C* F2=Decodifica
045000970619     C     $TASTO        WHENEQ    F02
045100101111     C                   Exsr      F02D1
045200970619     C* F3=Fine
045300970619     C     $TASTO        WHENEQ    F03
045400101111     C                   Exsr      F03D1
045500970619     C* F4=Ricerca
045600970619     C     $TASTO        WHENEQ    F04
045700101111     C                   Exsr      SEARCH
045800970619     C* F5=Refresh
045900970619     C     $TASTO        WHENEQ    F05
046000101111     C                   Exsr      F05D1
046100970619     C* F8=Successivo
046200970619     C     $TASTO        WHENEQ    F08
046300101111     C                   Exsr      F08D1
046400970619     C* F10=Immissione
046500970619     C     $TASTO        WHENEQ    F10
046600101111     C                   Exsr      F10D1
046700970619     C* F12=Ritorno
046800970619     C     $TASTO        WHENEQ    F12
046900101111     C                   Exsr      F12D1
047000970619     C* F18=Modifica valori assunti
047100970619     C     $TASTO        WHENEQ    F18
047200101111     C                   Exsr      F18D1
047300970619     C* F24=Altri tasti
047400970619     C     $TASTO        WHENEQ    F24
047500101111     C                   Exsr      F24D1
047600970619     C*
047700970619     C                   OTHER
047800101111     C                   Exsr      CTRD1
047900970619     C     *IN99         IFEQ      *OFF
048000970619     C* definisco l'ordinamento
048100101111     C                   Exsr      DEFORD
048200970619     C* richiamo la lista
048300101111     C                   Move      'S1'          $GEST
048400101111     C                   Move      *ON           $INZS1
048500970623     C                   Z-ADD     0             W1POS
048600101111     C                   EndIf
048700970619     C*
048800101111     C                   EndSl
048900970619     C*
049000101111     C                   EndIf
049100970619     C*
049200101111     C                   EndSr
049300970619     C************************************************************
049400970619     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
049500970619     C************************************************************
049600101111     C     INZD1         BegSr
049700970619     C*
049800970619     C                   CLEAR                   FMTD1
049900101111     C*
050000131122R132 C*
050100160208R132 C                   Select
050200160208R195 C*
050300160208 "   C* Black List annuale trac. polivalente
050400160208 "   C                   When      TpComD14    = 'L'
050500160208 "   C                   Move      'BA'          D1TpScar
050600160208 "   C*
050700160208R195 C* Black List mensile/trimestrale trac. polivalente
050800160208R132 C                   When      TpComD14    = 'B'
050900160208R132 C                   Move      'BP'          D1TpScar
051000160208R195 C*
051100131122R132 C                   Other
051200160208R195 C* Black List mensile/trimestrale trac. non polivalente
051300131122     C                   Move      'BL'          D1TpScar
051400131122R132 C                   endSl
051500131122R132 C*
051600101111     C*
051700101111     C                   Select
051800970619     C* il pgm è richiamato e
051900970619     C* sono stati passati dei valori di parzializzazione ma non
052000101111     C* la chiave univoca
052100101111     C     OPZD14        WHENNE    *BLANK
052200101111     C* imposto i valori passati sui campi a video
052300101111     C                   Move      XscSoc        D1Societa
052400101111     C                   Move      XscDSI        D1SocietaD
052500131122R132 C*
052600160208R132 C                   Select
052700160208R195 C*
052800160208 "   C* Black List annuale trac. polivalente
052900160208 "   C                   When      TpComD14    = 'L'
053000160208 "   C                   Move      'BA'          D1TpScar
053100160208 "   C*
053200160208R195 C* Black List mensile/trimestrale trac. polivalente
053300160208R132 C                   When      TpComD14    = 'B'
053400160208R132 C                   Move      'BP'          D1TpScar
053500160208R195 C*
053600131122R132 C                   Other
053700160208R195 C* Black List mensile/trimestrale trac. non polivalente
053800131122     C                   Move      'BL'          D1TpScar
053900131122R132 C                   endSl
054000131122R132 C*
054100101111     C                   MOVE      *Blank        D1TpScarD
054200101111     C                   If        D1TpScar <> *Blank
054300101111     C                   RESET                   X07DS
054400101111     C                   MOVE      '1'           X07RIC
054500101111     C                   MOVEL     'ANCFM000'    X07TRC
054600101111     C                   MOVEL     'CFMTPSCAR'   X07CAM
054700101111     C                   MOVEL     D1TpScar      X07VAL
054800101111     C                   MOVE      *OFF          X07ALL
054900101111     C                   CALLb     'X07VALR'
055000101111     C                   PARM                    X07DS
055100101111   B3C     X07ERR        IfEq      '0'
055200101111     C                   MoveL     X07DES        D1TpScarD
055300101111   E3C                   EndIf
055400101111     C                   EndIf
055500101111     C* il pgm non è richiamato
055600101111     C     OPZD14        WHENEQ    *BLANK
055700101111     C* imposto dei valori di default
055800101111     C                   EndSl
055900131122R132 C*
056000131122 "   C                   Exsr      CTRD1
056100131122 "   C*
056200131122 "   C                   Move      *ALL'0'       IN5098
056300131122R132 C                   MoveA     IN5098        *IN(50)
056400970813     C*
056500101111     C                   EndSr
056600970619     C************************************************************
056700970619     C* GESTIONE F02 VIDEO D1
056800970619     C************************************************************
056900101111     C     F02D1         BegSr
057000970619     C*
057100970619     C* per decodificare la videata richiamo la routine dei ctrl
057200970619     C* e poi disattivo gli eventuali messaggi di errore
057300101111     C                   Exsr      CTRD1
057400970619     C*
057500101111     C                   Move      *ALL'0'       IN5098
057600101111     C                   MoveA     IN5098        *IN(50)
057700970619     C*
057800101111     C                   EndSr
057900970619     C************************************************************
058000970619     C* GESTIONE F03 VIDEO D1
058100970619     C************************************************************
058200101111     C     F03D1         BegSr
058300970619     C*
058400101111     C                   Move      *ON           $FINE
058500101111     C                   Move      '1'           RETD14
058600970619     C* fine programma
058700101111     C                   Exsr      ENDPGM
058800970619     C*
058900101111     C                   EndSr
059000970619     C************************************************************
059100970619     C* GESTIONE F05 VIDEO D1
059200970619     C************************************************************
059300101111     C     F05D1         BegSr
059400970619     C*
059500101111     C                   Move      *ON           $INZD1
059600970619     C*
059700101111     C                   EndSr
059800970619     C************************************************************
059900970619     C* GESTIONE F08 VIDEO D1
060000970619     C************************************************************
060100101111     C     F08D1         BegSr
060200970619     C*
060300101111     C                   Move      *ON           $FINE
060400101111     C                   Move      '0'           RETD14
060500101111     C*
060600101111     C                   EndSr
060700101111     C/EJECT
060800101111     C************************************************************
060900970619     C* GESTIONE F10 VIDEO D1
061000970619     C************************************************************
061100101111     C     F10D1         BegSr
061200970619     C*
061300101111     C                   RESET                   ANA123DS
061400101111     C                   Move      xscsoc        Societa123
061500101111     C                   MoveL     '01'          OPZ123
061600101111     C     CMTD14        IFEQ      '1'
061700101111     C                   MoveL     '1'           CMT123
061800101111     C                   ELSE
061900101111     C                   MoveL     '0'           CMT123
062000101111     C                   EndIf
062100101111     C                   EVAL      Exf123 = WExf
062200101111     C                   MoveL     ANA123DS      KPJBU
062300101111     C                   CALL      'ANA123R'
062400101111     C                   PARM                    KPJBA
062500101111     C                   MoveL     KPJBU         ANA123DS
062600101111     C* ritorno da PGM gestione
062700101111     C                   Exsr      RTN123
062800970619     C*
062900101111     C                   Move      *HIVAL        $LASTV
063000970619     C*
063100101111     C                   EndSr
063200970619     C************************************************************
063300970619     C* GESTIONE F12 VIDEO D1
063400970619     C************************************************************
063500101111     C     F12D1         BegSr
063600970619     C*
063700970619     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
063800970619     C*  per F12 torno alla lista
063900970619     C     $LOP          IFEQ      2
064000970619     C     WINKR         OREQ      *ON
064100101111     C                   Move      'S1'          $GEST
064200970619     C*  altrimenti, per F12 torno a menu
064300970619     C                   ELSE
064400101111     C                   Move      *ON           $FINE
064500101111    >C                   Move      '2'           RETD14
064600101111     C                   EndIf
064700970619     C*
064800101111     C                   EndSr
064900970619     C************************************************************
065000970619     C* GESTIONE F18 VIDEO D1
065100970619     C************************************************************
065200101111     C     F18D1         BegSr
065300970619     C*
065400101111     C                   Move      'D2'          $GEST
065500101111     C                   MoveL     *ON           $INZD2
065600970619     C*
065700101111     C                   EndSr
065800970619     C************************************************************
065900970619     C* GESTIONE F24 VIDEO D1
066000970619     C************************************************************
066100101111     C     F24D1         BegSr
066200970619     C*
066300101111     C                   MoveA     $FX1          $FX
066400101111     C                   MoveA     $FC1          $FC
066500101111     C                   MoveA     $FR1          $FR
066600970619     C                   Z-ADD     $ULKD1        $ULK
066700970619     C                   Z-ADD     $D1L01        $L01
066800970619     C                   Z-ADD     $D1L02        $L02
066900101111     C                   Exsr      RIEKEY
067000970619     C                   Z-ADD     $ULK          $ULKD1            3 0
067100101111     C                   MoveA     $FC           $FC1
067200101111     C                   MoveL     $KEY1         Z1KE1
067300101111     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
067400101111     C                   MoveL     $KEY2         Z1KE2
067500970619     C                   WRITE     FMTZ1
067600970619     C*
067700101111     C                   EndSr
067800970619     C************************************************************
067900970619     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
068000970619     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
068100970619     C* 93 94 96 97 98
068200970619     C************************************************************
068300101111     C     CTRD1         BegSr
068400970619     C*
068500970619     C                   SETOFF                                       99
068600970620     C*
068700970620     C* Società
068800101111     C                   Move      *Blank        D1SocietaD
068900970620     C     D1Societa     IFNE      *BLANK
069000970620     C     D1Societa     CHAIN     ANSIF01L                           50
069100970620     C   50              SETON                                        99
069200101111     C  N50              MoveL     SIFDesBrev    D1SocietaD
069300101111     C                   EndIf
069400101111     C*
069500101111     C* Tipo scarico
069600101111     C                   MOVE      *Blank        D1TpScarD
069700101111     C                   If        D1TpScar <> *Blank
069800101111     C                   RESET                   X07DS
069900101111     C                   MOVE      '1'           X07RIC
070000101111     C                   MOVEL     'ANCFM000'    X07TRC
070100101111     C                   MOVEL     'CFMTPSCAR'   X07CAM
070200101111     C                   MOVEL     D1TpScar      X07VAL
070300101111     C                   MOVE      *OFF          X07ALL
070400101111     C                   CALLb     'X07VALR'
070500101111     C                   PARM                    X07DS
070600101111   B3C     X07ERR        IFEQ      '0'
070700101111     C                   MOVEL     X07DES        D1TpScarD
070800101111     C                   ELSE
070900101111     C                   SetOn                                        5199
071000101111   E3C                   ENDIF
071100101111     C                   EndIf
071200101116     C*
071300101116     C* se Data "periodo da:" è zero => primo giorno dell'anno
071400101116     C                   IF        D1DATADA = 0
071500101116     C*
071600101116     C                   CALL      'XDATCD'
071700101116     C                   PARM      5             CODICE            2 0
071800101116     C                   PARM                    OGGI
071900101116     C                   IF        OGGI <> *LOVAL
072000101116     C     *JobRun       MOVE      OGGI          D1DATADA
072100101116     C                   MOVE      OGGI          DATA1
072200101116     C                   ENDIF
072300101116     C*
072400101116     C                   ENDIF
072500101116     C*
072600101116     C* se Data "periodo a:" è zero => data odierna rovesciata
072700101116     C                   IF        D1DATAA = 0
072800101116     C                   MOVE      ZDate         D1DATAA
072900101116     C     *dmy          MOVE      UDATE         DATA2
073000101116     C                   ENDIF
073100101116     C*
073200101116     C* Controlli formali date
073300101116     C     *JobRun       TEST(D)                 D1DATADA               72
073400101116     C   72              SetOn                                        99
073500101116     C                   IF        *IN72 = *OFF
073600101116     C     *JobRun       MOVE      D1DATADA      DATA1
073700101116     C                   ENDIF
073800101116     C*
073900101116     C     *JobRun       TEST(D)                 D1DATAA                62
074000101116     C   62              SetOn                                        99
074100101116     C                   IF        *IN62 = *OFF
074200101116     C     *JobRun       MOVE      D1DATAA       DATA2
074300101116     C                   ENDIF
074400101116     C*
074500101116     C* Controllo congruenza date
074600101116     C                   IF        DATA1 > DATA2
074700101116     C                   SETON                                          9961
074800101116     C                   ENDIF
074900101116     C*
075000101118R039 C                   Clear                   D1StpLD
075100101118R039 C                   Exsr      CtrStpl
075200101111     C*
075300101111     C                   EndSr
075400101118     C************************************************************
075500101118     C* Ricerca tipo stampa
075600101118     C************************************************************
075700101118     C     F4_STPL       BEGSR
075800101118     C*
075900101118     C                   Reset                   A964DS
076000101118     C*
076100101118     C                   MoveL     '01'          Opz964
076200101118     C                   MoveL     'ANDIZ000'    Trc964
076300101118     C                   MoveL     'DSPSTAMBL'   Cam964
076400101123     C                   MoveL     *Off          All964
076500101118     C*
076600101118     C                   MoveL     A964DS        KPJBU
076700101118     C                   CALL      'ANA964R'
076800101118     C                   PARM                    KPJBA
076900101118     C                   MoveL     KPJBU         A964DS
077000101118     C*
077100101118     C                   if        Ret964     <> '2'
077200101118     C                   MoveL     Val964        D1STPL
077300101118     C                   endif
077400101118     C*
077500101118     C                   ENDSR
077600101118     C***********************************************************
077700101118     C* Controllo tipo stampa
077800101118     C***********************************************************
077900101118     C     CtrSTPL       BEGSR
078000101118     C*
078100101118     C                   Clear                   D1STPLD
078200101118     C*
078300101118     C                   Clear                   X07Ds
078400101118     C*
078500101118     C                   Move      '1'           X07Ric
078600101123     C                   Move      *Off          X07All
078700101118     C                   Movel     'ANDIZ000'    X07TRc
078800101118     C                   Movel     'DSPSTAMBL'   X07Cam
078900101118     C                   Movel     D1STPL        X07VAL
079000101118     C*
079100101118     C                   Callb     'X07VALR'
079200101118     C                   Parm                    X07Ds
079300101118     C*
079400101118     C     X07ERR        IFEQ      *Off
079500101118     C                   MOVEL     X07DES        D1STPLD
079600101118     C                   else
079700101118     C                   SetOn                                        6899
079800101118     C                   endif
079900101118     C*
080000101118     C                   EndSr
080100970619     C************************************************************
080200970619     C* DEFINIZIONE DELL'ORDINAMENTO
080300970619     C************************************************************
080400101111     C     DEFORD        BegSr
080500970619     C*
080600970619     C                   Z-ADD     1             $ORD
080700970619     C*
080800101111     C                   EndSr
080900970619     C************************************************************
081000970619     C* Chiamata programma con tasto personalizzato
081100970619     C************************************************************
081200101111     C     FPerS1        BegSr
081300970619     C*
081400101111     C* salvo la DS di ingresso
081500101111     C                   MoveL     SDGSD14DS     $SaveDS
081600101111     C*
081700101111     C                   MoveL     SDGSD14DS     KPJBU
081800101111     C                   CALL      $PGF2(PosFun)                        22
081900101111     C                   PARM                    KPJBA
082000970619     C*
082100970619     C* ripristino la DS di ingresso
082200101111     C                   MoveL     $SaveDS       SDGSD14DS
082300101111     C*
082400101111     C                   EndSr
082500101111     C************************************************************
082600101111     C* GESTIONE LISTA
082700101111     C************************************************************
082800101111     C     GESS1         BegSr
082900101111     C* inizializzazione videata
083000101111     C     $INZS1        IFEQ      *ON
083100101111     C                   Exsr      INZS1
083200101111     C                   Move      *OFF          $INZS1
083300101111     C                   EndIf
083400970619     C* emissione piede videata se proveniente da altra
083500970619     C* salvataggio valore formato di provenienza
083600970619     C     $LASTV        IFNE      'S1'
083700970619     C                   WRITE     FMTT1
083800101111     C                   Exsr      GESPIE
083900101111     C                   Move      $LASTV        $LASTG
084000101111     C                   Move      'S1'          $LASTV
084100101111     C                   EndIf
084200970619     C*
084300970619     C     WMAX          IFEQ      0
084400970619     C                   WRITE     FMTD3                                        NO RCD
084500101111     C                   EndIf
084600971015     C* Serve per emissione finestra pgm invio ANA727r (per open/close
084700971015     C* window)
084800101111     C                   Move      '1'           WIN727            1
084900970619     C* Inizializzazione indice tasti funzione personalizzati
085000970619     C                   Eval      PosFun = 25
085100970619     C*
085200970619     C                   EXFMT     FMTC1
085300970619     C*
085400970619     C* azzero la posizione del cursore
085500970619     C                   Z-ADD     0             H1RIGA
085600970619     C                   Z-ADD     0             H1COLO
085700970619     C*
085800970619     C     C1NRR         IFNE      0
085900970619     C                   Z-ADD     C1NRR         WSFL
086000101111     C                   EndIf
086100970619     C                   IF        SFLNRR > 0
086200970619     C                   Z-ADD     SFLNRR        C1RCD
086300101111     C                   EndIf
086400970829     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
086500970829     C                   EVAL      WExf = *On
086600970619     C* inizializzazione indice opzioni personalizzate
086700970619     C                   Eval      PosOpz = 0
086800101111     C                   MoveA     $FC2          $FC
086900101111     C                   Exsr      CTRKEY
087000970619     C*
087100970619     C     *IN99         IFEQ      *OFF
087200970619     C*
087300101111     C                   Select
087400970619     C* F3=Fine
087500970619     C     $TASTO        WHENEQ    F03
087600101111     C                   Exsr      F03S1
087700970619     C* F8=Successivo
087800970619     C     $TASTO        WHENEQ    F08
087900101111     C                   Exsr      F08S1
088000970619     C* F10=Immissione
088100970619     C     $TASTO        WHENEQ    F10
088200101111     C                   Exsr      F10S1
088300970619     C* F12=Ritorno
088400970619     C     $TASTO        WHENEQ    F12
088500101111     C                   Exsr      F12S1
088600970619     C* F17=Parzializzazioni
088700970619     C     $TASTO        WHENEQ    F17
088800101111     C                   Exsr      F17S1
088900970619     C* F18=Modifica valori assunti
089000970619     C     $TASTO        WHENEQ    F18
089100101111     C                   Exsr      F18S1
089200970619     C* F23=Altre opzioni
089300970619     C     $TASTO        WHENEQ    F23
089400101111     C                   Exsr      F23S1
089500970619     C* F24=Altri tasti
089600970619     C     $TASTO        WHENEQ    F24
089700101111     C                   Exsr      F24S1
089800970619     C* ROLL-UP
089900970619     C     $TASTO        WHENEQ    ROLLUP
090000101111     C                   Exsr      ROLS1
090100970619     C* Personalizzata
090200970619     C     $PGF2(PosFun) Whenne    *BLANK
090300101111     C                   Exsr      FperS1
090400970619     C*
090500970619     C                   OTHER
090600970619     C* CONTROLLO DATI
090700101111     C                   Exsr      CTRC1
090800971015     C                   If        *In99 = *Off
090900101111     C                   Exsr      CTRS1
091000101111     C                   EndIf
091100970619     C*
091200101111     C                   EndSl
091300970619     C*
091400101111     C                   EndIf
091500970619     C*
091600101111     C                   EndSr
091700970619     C************************************************************
091800970619     C* INIZIALIZZAZIONE LISTA
091900970619     C************************************************************
092000101111     C     INZS1         BegSr
092100970619     C* pulizia SFL
092200970619     C                   SETOFF                                         3031
092300970619     C                   WRITE     FMTC1
092400970619     C                   SETON                                          31
092500970619     C*
092600970619     C* riempimento opzioni
092700101111     C                   MoveA     $TX1          $TX
092800101111     C                   MoveA     $FL1          $FL
092900970619     C                   Z-ADD     0             WCOZ
093000970619     C                   Z-ADD     0             WMXO
093100970619     C*  equivale al num.di elementi della schiera $TX1
093200970619     C                   Z-add     OpzLen        WMXO
093300970619     C                   Z-ADD     $C1Op1        $L01
093400970619     C                   Z-ADD     $C1Op2        $L02
093500101111     C                   Exsr      RIEOPZ
093600970619     C                   Z-ADD     23            X
093700101111     C                   Move      FF23          $FC2(X)
093800101111     C                   MoveL     WOPZ1         C1OPZ1
093900101111     C                   MoveL     $ALLOPT       H1ALLOPT
094000101111     C                   MoveL     WOPZ2         C1OPZ2
094100970619     C                   Z-ADD     WCOZ          C1COZ
094200970619     C*
094300970619     C* CARICAMENTO SFL A PAGINE
094400970619     C*
094500970619     C                   Z-ADD     0             S1NRR
094600970619     C                   Z-ADD     0             WMAX
094700970619     C*
094800970619     C* Posizionamento su file pilota
094900970619     C                   CLEAR                   C1POS
095000101111     C                   Exsr      SETANA
095100970619     C* ripristino i valori del posizionamento
095200101111     C                   Move      W1POS         C1POS
095300970619     C*
095400970619     C* Lettura file pilota
095500101111     C                   Exsr      REDANA
095600970619     C*
095700970619     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
095800970619     C                   IF        $LastV = 'D1'
095900970619     C                   Z-ADD     SFLPAG        WPAG
096000101111     C                   EndIf
096100101111     C                   Exsr      ROLS1
096200970619     C* imposto il caricamento sfl del numero massimo di righe per pagina
096300970619     C                   Z-ADD     SFLPAG        WPAG
096400970619     C*
096500101111     C                   EndSr
096600970619     C************************************************************
096700970619     C* CARICAMENTO PAGINA LISTA
096800970619     C************************************************************
096900101111     C     ROLS1         BegSr
097000970619     C*
097100970619     C                   SETOFF                                       32
097200970619     C                   Z-ADD     0             Y
097300970619     C                   Z-ADD     WMAX          S1NRR
097400970619     C*
097500970619     C* Leggo dal file anagrafico per caricare la lista
097600970619     C*
097700101111     C     $EndFile      DoWEq     *OFF
097800970619     C     Y             ANDLT     WPAG
097900970619     C*
098000101111     C                   Exsr      RIES1
098100970619     C*
098200970619     C                   ADD       1             S1NRR
098300970619     C                   ADD       1             Y
098400970619     C*
098500970619     C                   WRITE     FMTS1
098600970619     C*
098700101111     C                   Exsr      REDANA
098800970619     C*
098900101111     C                   EndDo
099000970619     C*
099100970619     C                   Z-ADD     S1NRR         WMAX                 30
099200970619     C*
099300970619     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
099400970619     C*
099500970619     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
099600970619     C                   MVR                     RESTO             3 0
099700970619     C     PAGINE        MULT      SFLPAG        C1RCD
099800970619     C     RESTO         IFGT      0
099900970619     C                   ADD       1             C1RCD
100000970619     C                   ELSE
100100970619     C                   SUB       SFLPAG        C1RCD
100200970619     C                   ADD       1             C1RCD
100300101111     C                   EndIf
100400970619     C*
100500101111     C                   EndSr
100600970619     C************************************************************
100700970619     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
100800970619     C************************************************************
100900101111     C     SETANA        BegSr
101000970619     C*
101100101111     C                   Select
101200970619     C*
101300970619     C* Gestione posizionamento
101400970623     C* Se il campo W1POS è > 0 imposto il posizionamento con
101500970619     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
101600970619     C*
101700970619    >C* Ord.1 = chiave ordinamento 1
101800970619     C     $ORD          WHENEQ    1
101900970623     C     W1POS         IFNE      0
102000101111     C     *JobRun       MoveL     W1POS         CFMData
102100160825brt  C                   Move      *loval        CFMOra
102200970619     C                   ELSE
102300101111     C                   Move      *HIVAL        CFMData
102400160825brt  C                   Move      *HIVAL        CFMOra
102500101111     C                   EndIf
102600981028     C* (se non impostata la società in parzializz. mi posiziono con *hival)
102700981028     C                   If        D1Societa <> *Blank
102800101111     C                   Move      D1Societa     CFMSocieta
102900101111     C                   Else
103000101111     C                   Move      *All'9'       CFMSocieta
103100101111     C                   EndIf
103200160825brt  C***  k02cfm02      SETGT     ANCFM02
103300160825brt  C     k03cfm02      SETGT     ANCFM02
103400101111     C                   EndSl
103500101111     C*
103600101111     C                   EndSr
103700101111     C************************************************************
103800101111     C* LETTURA RCD ARCHIVIO PILOTA
103900970619     C************************************************************
104000101111     C     REDANA        BegSr
104100970619     C*
104200101111     C                   MoveL     *OFF          $EndFile
104300101111     C                   MoveL     *OFF          $RCDOK
104400970619     C*
104500970619     C     $EndFile      DOUEQ     *ON
104600970619     C     $RCDOK        OREQ      *ON
104700970619     C*
104800101111     C                   Select
104900970619     C     $ORD          WHENEQ    1
105000101111     C                   READP     ANCFM02                                21
105100101111     C                   EndSl
105200101111     C*
105300101111     C                   Move      *IN21         $EndFile
105400101111     C*
105500101111     C     $EndFile      IFEQ      *OFF
105600101111     C                   Exsr      SELANA
105700101111     C                   EndIf
105800970619     C*
105900101111     C                   EndDo
106000970619     C*
106100101111     C                   Move      *IN21         *IN33                          SflEnd
106200970619     C*
106300101111     C                   EndSr
106400970619     C************************************************************
106500970619     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
106600970619     C************************************************************
106700101111     C     CLCPAG        BegSr
106800970619     C* Input :
106900970619     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
107000970619     C*          cursore
107100970619     C* - SFLPAG = numero rcd per pagina sfl
107200970619     C* Output :
107300970619     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
107400970619     C*
107500970619     C     WSFL          DIV       SFLPAG        PAGINE            4 0
107600970619     C                   MVR                     RESTO             3 0
107700970619     C     RESTO         IFGT      0
107800970619     C                   ADD       1             PAGINE
107900101111     C                   EndIf
108000970619     C     PAGINE        MULT      SFLPAG        WPAG
108100970619     C*
108200101111     C                   EndSr
108300970619     C************************************************************
108400970619     C* SELEZIONI SULLA RIGA LETTA
108500970619     C* impostare a *OFF il campo $RCDOK per scartare un record che
108600970619     C* non rientra nei parametri di parzializzazione
108700970619     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
108800970619     C* superiore stabilito nei parametri
108900970619     C*
109000970619     C************************************************************
109100101111     C     SELANA        BegSr
109200970619     C*
109300101111     C                   MoveL     *ON           $RCDOK
109400970619     C*
109500101116     C     *JobRun       TEST(D)                 D1DATADA               18
109600101116     C                   IF        *IN18 = *OFF
109700101116     C     *JobRun       MOVE      D1DATADA      DATA1
109800101116     C                   ENDIF
109900101116     C     *JobRun       TEST(D)                 D1DATAA                18
110000101116     C                   IF        *IN18 = *OFF
110100101116     C     *JobRun       MOVE      D1DATAA       DATA2
110200101116     C                   ENDIF
110300101116     C*
110400101111     C                   Select
110500101111     C* Società non compresa nella scelta
110600101111     C     CFMSocieta    WHENNE    D1Societa
110700101111     C     D1Societa     ANDNE     *Blank
110800101111     C                   MoveL     *OFF          $RCDOK
110900101111     C* Tipo scarico non compreso nella scelta
111000101111     C     CFMTpScar     WHENNE    D1TpScar
111100101111     C     D1TpScar      ANDNE     *Blank
111200101111     C                   MoveL     *OFF          $RCDOK
111300101111     C*
111400101116     C* Controllo datae
111500101116     C     CFMData       WHENLT    Data1
111600101116     C     CFMDATA       OrGt      Data2
111700101116     C                   MoveL     *OFF          $RCDOK
111800101116     C*
111900970619     C                   IF        $RcdOk = *off
112000101111     C                   EndIf
112100970623     C*
112200101111     C                   EndSl
112300970619     C*
112400101111     C                   EndSr
112500970619     C************************************************************
112600970619     C* RIEMPIMENTO RIGA LISTA
112700970619     C************************************************************
112800101111     C     RIES1         BegSr
112900970619     C*
113000970619     C                   CLEAR                   FMTS1
113100970619     C*
113200101111     C                   MoveL     CFMSocieta    S1Societa
113300101111     C                   MoveL     CFMFile       S1File
113400101111     C                   MoveL     CFMDescr      S1DES1
113500101111     C     *JobRun       Move      CFMData       S1Data
113600101111     C*
113700101111     C                   MoveL     CFMLib        H1Lib
113800970619     C*
113900970619     C* imposto quegli attributi del sfl che sono modificabili
114000970619     C* al richiamo del determinato pgm
114100101111     C                   Exsr      ATRS1
114200970619     C*
114300101111     C                   EndSr
114400970619     C************************************************************
114500970619     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
114600970619     C************************************************************
114700101111     C     ATRS1         BegSr
114800970619     C*
114900101111     C                   EndSr
115000970619     C************************************************************
115100970619     C* GESTIONE F08 VIDEO S1
115200970619     C************************************************************
115300101111     C     F08S1         BegSr
115400970619     C*
115500101111     C                   Move      *ON           $FINE
115600101111     C                   Move      '0'           RETD14
115700970619     C*
115800101111     C                   EndSr
115900970619     C************************************************************
116000970619     C* GESTIONE F03 VIDEO S1
116100970619     C************************************************************
116200101111     C     F03S1         BegSr
116300970619     C*
116400101111     C                   Move      *ON           $FINE
116500101111     C                   Move      '1'           RETD14
116600970619     C* fine programma
116700101111     C                   Exsr      ENDPGM
116800970619     C*
116900101111     C                   EndSr
117000970619     C************************************************************
117100970619     C* GESTIONE F10 VIDEO S1
117200970619     C* AGGIUNTA RECORD
117300970619     C************************************************************
117400101111     C     F10S1         BegSr
117500970619     C*
117600101111     C                   RESET                   ANA123DS
117700101111     C                   Move      xscsoc        Societa123
117800101111     C                   MoveL     '01'          OPZ123
117900101111     C     CMTD14        IFEQ      '1'
118000101111     C                   MoveL     '1'           CMT123
118100101111     C                   ELSE
118200101111     C                   MoveL     '0'           CMT123
118300101111     C                   EndIf
118400101111     C* passo eventuale EXFMT per chiamata a pgm di sola finestra
118500970829     C                   EVAL      Exf123 = WExf
118600101111     C                   MoveL     ANA123DS      KPJBU
118700101111     C*
118800101111     C                   CALL      'ANA123R'
118900101111     C                   PARM                    KPJBA
119000101111     C                   MoveL     KPJBU         ANA123DS
119100101111     C* ritorno da PGM gestione
119200101111     C                   Exsr      RTN123
119300101111     C     WINZS1        IFEQ      *ON
119400101111     C                   Move      *ON           $INZS1
119500970619     C* se esistevano già righe sul sfl
119600970619     C* calcolo pagina a cui deve posizionarsi
119700970619     C     WSFL          IFGT      0
119800101111     C                   Exsr      CLCPAG
119900970619     C* altrimenti carico solo la 1a pagina
120000970619     C                   ELSE
120100970619     C                   Z-ADD     SFLPAG        WPAG
120200101111     C                   EndIf
120300101111     C                   EndIf
120400970619     C*
120500101111     C                   Move      *HIVAL        $LASTV
120600970619     C*
120700101111     C                   EndSr
120800970619     C************************************************************
120900970619     C* GESTIONE F12 VIDEO S1
121000970619     C************************************************************
121100101111     C     F12S1         BegSr
121200970619     C*
121300970619     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
121400970619     C     $LOP          IFEQ      1
121500101111     C                   Move      'D1'          $GEST
121600970619     C*  se la 1a videata è questa, per F12 torno a menu
121700970619     C                   ELSE
121800101111     C                   Move      *ON           $FINE
121900101111     C                   Move      '2'           RETD14
122000101111     C                   EndIf
122100101111     C*
122200101111     C                   EndSr
122300101111     C************************************************************
122400101111     C* GESTIONE F17 VIDEO S1
122500101111     C************************************************************
122600101111     C     F17S1         BegSr
122700970619     C*
122800101111     C                   Move      'D1'          $GEST
122900970619     C*
123000101111     C                   EndSr
123100970619     C************************************************************
123200970619     C* GESTIONE F18 VIDEO S1
123300970619     C************************************************************
123400101111     C     F18S1         BegSr
123500970619     C*
123600101111     C                   Move      'D2'          $GEST
123700101111     C                   MoveL     *ON           $INZD2
123800970619     C*
123900101111     C                   EndSr
124000970619     C************************************************************
124100970619     C* GESTIONE F23 VIDEO S1
124200970619     C************************************************************
124300101111     C     F23S1         BegSr
124400970619     C*
124500970619     C                   Z-ADD     $C1Op1        $L01
124600970619     C                   Z-ADD     $C1Op2        $L02
124700101111     C                   Exsr      RIEOPZ
124800970619     C                   Z-ADD     23            X
124900101111     C                   Move      FF23          $FC2(X)
125000101111     C                   MoveL     WOPZ1         C1OPZ1
125100101111     C                   MoveL     $ALLOPT       H1ALLOPT
125200101111     C                   MoveL     WOPZ2         C1OPZ2
125300970619     C                   Z-ADD     WCOZ          C1COZ
125400970619     C*
125500101111     C                   EndSr
125600970619     C************************************************************
125700970619     C* GESTIONE F24 VIDEO S1
125800970619     C************************************************************
125900101111     C     F24S1         BegSr
126000970619     C*
126100101111     C                   MoveA     $FX2          $FX
126200101111     C                   MoveA     $FC2          $FC
126300101111     C                   MoveA     $FR2          $FR
126400970619     C                   Z-ADD     $ULKS1        $ULK
126500970619     C                   Z-ADD     $S1L01        $L01
126600970619     C                   Z-ADD     $S1L02        $L02
126700101111     C                   Exsr      RIEKEY
126800970619     C                   Z-ADD     $ULK          $ULKS1            3 0
126900101111     C                   MoveA     $FC           $FC2
127000101111     C                   MoveL     $KEY1         Z2KE1
127100101111     C                   MoveL     $ALLFUNCT     H2ALLFUNCT
127200101111     C                   MoveL     $KEY2         Z2KE2
127300970619     C                   WRITE     FMTZ2
127400970619     C*
127500101111     C                   EndSr
127600970619     C************************************************************
127700970619     C* CONTROLLO TESTATA LISTA
127800970619     C************************************************************
127900101111     C     CTRC1         BegSr
128000970619     C*
128100101111     C                   Move      *OFF          *IN99
128200970623     C*
128300970623     C* controllo data valida in C1POS
128400970623     C     C1POS         IFNE      0
128500101111     C     *JobRun       TEST(D)                 C1POS                  55
128600970623     C   55              SETON                                        99
128700101111     C                   EndIf
128800970619     C*
128900970619     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
129000970619     C* L'INIZIALIZZAZIONE LISTA
129100970619     C*
129200970619     C     C1POS         IFNE      W1POS
129300970623     C     *IN99         ANDEQ     *OFF
129400101111     C                   Exsr      POSS1
129500101111     C                   EndIf
129600970619     C*
129700101111     C                   EndSr
129800970619     C************************************************************
129900970619     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
130000970619     C************************************************************
130100101111     C     POSS1         BegSr
130200970619     C*
130300101111     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
130400101111     C*
130500101111     C     WMAX          IFGT      0
130600101111     C                   READC     FMTS1                                  21
130700101111     C     *IN21         DoWEq     *OFF
130800101111     C     *IN99         ANDEQ     *OFF
130900101111     C*
131000101111     C     S1OPZ         IFNE      0
131100101111     C                   SETON                                        92  99
131200101111     C                   EndIf
131300970619     C* se rilevata opzione attiva, attivo sflnxtchg
131400101111     C     *IN99         IFEQ      *ON
131500101111     C                   Move      *ON           *IN32
131600101111     C*
131700101111     C* imposto quegli attributi del sfl che sono modificabili
131800101111     C* al richiamo del determinato pgm
131900101111     C                   Exsr      ATRS1
132000970619     C*
132100970619     C                   UPDATE    FMTS1
132200101111     C                   ELSE
132300101111     C                   READC     FMTS1                                  21
132400101111     C                   EndIf
132500101111     C*
132600101111     C                   EndDo
132700101111     C*
132800101111     C                   EndIf
132900101111     C*
133000101111     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
133100101111     C* AL VALORE DI POSIZIONAMENTO :
133200970619     C* se questo è maggiore del valore del rispettivo campo di
133300970619     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
133400970619     C* con i valori di parzializzazione.
133500970619     C*
133600101111     C     *IN99         IFEQ      *OFF
133700101111     C                   Z-ADD     0             W1POS
133800101111     C*
133900101111     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
134000101111     C* è maggiore del relativo valore di parzializzazione, nel qual caso
134100101111     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
134200101111     C* richiedo comunque un ricaricamento del sfl che, se non si è
134300101111     C* verificata la condizione suddetta avverrà con posizionamento a
134400101111     C* partire dal valore di parzializzazione
134500101111     C*
134600101111     C                   Select
134700101111     C*
134800101111     C     $ORD          WHENEQ    1
134900101111     C     C1POS         IFGT      0
135000101111     C                   MoveL     C1POS         W1POS
135100101111     C                   EndIf
135200101111     C*
135300101111     C                   EndSl
135400101111     C*
135500101111     C* richiedo l'inizializzazione sfl
135600101111     C                   Move      *ON           $INZS1
135700101111     C*
135800101111     C                   EndIf
135900101111     C*
136000101111     C                   EndSr
136100970619     C************************************************************
136200970619     C* CONTROLLO OPZIONI LISTA
136300970619     C************************************************************
136400101111     C     CTRS1         BegSr
136500970619     C*
136600101111     C                   MoveL     *OFF          $ESCI
136700970619     C                   SETOFF                                       99
136800970619     C                   Z-ADD     0             S1OPZ
136900970619     C*
137000970619     C* Leggo il sfl solo se ci sono rcd
137100101111     C     WMAX          IFGT      0
137200101111     C                   READC     FMTS1                                  21
137300101111     C*
137400101111     C* esce se fine sfl o errore che richiede l'uscita
137500101111     C     *IN21         DoWEq     *OFF
137600101111     C     $ESCI         ANDEQ     *OFF
137700101111     C                   Z-ADD     S1NRR         C1RCD
137800101111     C* ctrl su riga
137900101111     C                   Exsr      RECS1
138000101111     C* gestione opzioni
138100101111     C     S1OPZ         IFNE      0
138200101111     C     *IN99         ANDEQ     *OFF
138300101111     C                   Exsr      OPZS1
138400101111     C                   EndIf
138500101111     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
138600101111     C     *IN99         IFEQ      *ON
138700101111     C                   Move      *ON           *IN32
138800101111     C                   EndIf
138900101111     C*
139000101111     C* imposto quegli attributi del sfl che sono modificabili
139100970619     C* al richiamo del determinato pgm
139200101111     C                   Exsr      ATRS1
139300970619     C*
139400970619     C                   Z-ADD     0             S1OPZ
139500970619     C*
139600970619     C                   UPDATE    FMTS1
139700101111     C                   Move      *OFF          *IN32
139800970619     C*
139900970619     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
140000101111     C     $ESCI         IFEQ      *OFF
140100101111     C                   READC     FMTS1                                  21
140200101111     C* a fine ciclo ripristino il flag richiesta iniz.sfl
140300101111     C     *IN21         IFEQ      *ON
140400101111     C                   Move      WINZS1        $INZS1
140500101111     C* calcolo pagina a cui deve posizionarsi
140600101111     C     WSFL          IFGT      0
140700101111     C                   Exsr      CLCPAG
140800101111     C                   ELSE
140900101111     C                   Z-ADD     SFLPAG        WPAG
141000101111     C                   EndIf
141100101111     C                   EndIf
141200101111     C                   EndIf
141300101111     C*
141400101111     C                   EndDo
141500101111     C*
141600101111     C                   EndIf
141700101111     C*
141800101111     C                   EndSr
141900101111     C************************************************************
142000970619     C* CONTROLLO CAMPI I/O RIGA LISTA
142100970619     C************************************************************
142200101111     C     RECS1         BegSr
142300970619     C*
142400970619     C* reset indicatori DSPATR
142500101111     C                   Move      *ALL'0'       IN4049           10
142600101111     C                   MoveA     IN4049        *IN(40)
142700970619     C*
142800970619     C* Controllo se Utente Autorizzato alla scelta
142900970619     C     S1OPZ         IFNE      0
143000970619     C                   Z-ADD     1             X
143100970619     C     S1OPZ         LOOKUP    $OP1(X)                                21
143200970619     C* mi salvo l'indice dell' opzione
143300970619     C   21              Z-add     X             PosOpz            2 0
143400970619     C     *IN21         IFEQ      *OFF
143500970619     C     $FL1(X)       OREQ      'N'
143600101111     C                   Move      *ON           $ESCI
143700970619     C                   SETON                                        409199
143800101111     C                   EndIf
143900101111     C                   EndIf
144000970813     C*
144100101111     C                   EndSr
144200970619     C************************************************************
144300970619     C* GESTIONE OPZIONI LISTA
144400970619     C************************************************************
144500101111     C     OPZS1         BegSr
144600970619     C*
144700970619     C                   Z-ADD     RIRI          CURR
144800970619     C                   Z-ADD     COCO          CURC
144900970619     C*
145000101111     C                   Select
145100970619     C*
145200970619     C* se opz=Selezione
145300970619     C* passo la chiave al pgm chiamante
145400101111     C     S1OPZ         WHENEQ    1
145500101111     C                   Move      S1OPZ         OPZD14
145600101111     C* passare nei campi appositi della DS SDGSD14DSil valore
145700101111     C* della chiave della VL univoca (se c'è)
145800131122R132 C*
145900160208R132 C                   Select
146000160208R195 C* Black List mensile/trimestrale trac. polivalente
146100160208R132 C                   When      TpComD14    = 'B'
146200160208R195 C                             or
146300160208 "   C* Black List annuale trac. polivalente
146400160208R195 C                             TpComD14    = 'L'
146500160208R132 C*
146600131122 "   C                   Eval      FileTelG61  = S1File
146700131122 "   C                   Eval      LibG61      = H1Lib
146800160208R132 C                   Eval      STPLG61     = D1STPL
146900160208R195 C                   Eval      TpComG61    = TpComD14
147000160208R195 C*
147100160208R132 C                   MoveL     SDGSG61DS     KPJBU
147200131122 "   C*
147300131122 "   C                   CALL      'SDGSG61C2'
147400131122 "   C                   PARM                    KPJBA                          *
147500131122 "   C*
147600131122 "   C                   Other
147700131122R132 C*
147800160208R195 C* Black List mensile/trimestrale trac. non polivalente
147900131122     C                   Eval      FileD11     = S1File
148000131122     C                   Eval      LibD11      = H1Lib
148100131122     C                   Eval      STPLD11     = D1STPL
148200131122     C                   MoveL     SDGSD11DS     KPJBU
148300131122     C*
148400131122     C                   CALL      'SDGSD11C1'
148500131122     C                   PARM                    KPJBA                          *
148600131122R132 C*
148700131122 "   C                   endSl
148800131122R132 C*
148900101111     C*
149000101111     C                   Move      *ON           $ESCI
149100101111     C                   Move      *ON           $FINE
149200101111     C*
149300101111     C                   Move      *HIVAL        $LASTV
149400970619     C*
149500970619     C* se selezionata opzione personalizzata, richiamo il programma
149600970619     C* corrispondente
149700970619     C     $PGO1(PosOpz) Whenne    *Blank
149800101111     C                   RESET                   ANA123DS
149900101111     C                   Move      xscsoc        Societa123
150000101111     C                   MoveL     S1OPZ         OPZ123
150100101111     C     CMTD14        IFEQ      '1'
150200101111     C                   MoveL     '1'           CMT123
150300101111     C                   ELSE
150400101111     C                   MoveL     '0'           CMT123
150500101111     C                   EndIf
150600101111     C* passare nei campi appositi della DS ANA123DS il valore
150700101111     C* della chiave della VL univoca
150800101111     C                   MoveL     S1Societa     Soc123
150900101111     C                   MoveL     S1File        File123
151000101111     C*
151100101111     C                   MoveL     ANA123DS      KPJBU
151200101111     C                   CALL      $PGO1(PosOpz)                        22
151300101111     C                   PARM                    KPJBA
151400101111     C*
151500101111     C                   MoveL     KPJBU         ANA123DS
151600101111     C* ritorno da PGM
151700101111     C                   Exsr      RTN123
151800970619     C*
151900970619     C* se altre opzioni
152000970619     C* richiamo pgm gestione anagrafica
152100970619     C                   OTHER
152200970619     C*
152300101111     C                   RESET                   ANA123DS
152400101111     C                   Move      xscsoc        Societa123
152500101111     C                   MoveL     S1OPZ         OPZ123
152600101111     C     CMTD14        IFEQ      '1'
152700101111     C                   MoveL     '1'           CMT123
152800101111     C                   ELSE
152900101111     C                   MoveL     '0'           CMT123
153000101111     C                   EndIf
153100101111     C* passare nei campi appositi della DS ANA123DS il valore
153200101111     C* della chiave della VL univoca
153300101111    >C                   MoveL     S1Societa     Soc123
153400101111    >C                   MoveL     S1File        File123
153500970829     C* passo eventuale EXFMT per chiamata a pgm di sola finestra
153600970829     C                   EVAL      Exf123 = WExf
153700970619     C*
153800101111    >C                   MoveL     ANA123DS      KPJBU
153900970619    >C                   CALL      'ANA123R'
154000970619     C                   PARM                    KPJBA
154100970620    >C                   PARM                    AUTDS
154200970619     C*
154300101111    >C                   MoveL     KPJBU         ANA123DS
154400970619     C*
154500970619     C* ritorno da PGM gestione
154600101111    >C                   Exsr      RTN123
154700970619     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
154800970619     C     *IN99         IFEQ      *ON
154900970619     C                   SETON                                        40
155000101111     C                   EndIf
155100970619     C*
155200101111     C                   Move      *HIVAL        $LASTV
155300101111     C                   EndSl
155400970619     C*
155500101111     C                   EndSr
155600970619     C************************************************************
155700970619     C* GESTIONE RITORNO DA PGM DI GESTIONE
155800101111     C************************************************************
155900101111     C     RTN123        BegSr
156000101111     C*
156100101111     C* modo di ritorno
156200101111     C*
156300101111     C                   Select
156400101111     C     RET123        WHENEQ    '1'
156500101111     C                   Move      *ON           $ESCI                          sospese READC
156600101111     C                   Move      *ON           $FINE                          fine pgm
156700101111     C                   Move      '1'           RETD14
156800101111     C* 2 = F12
156900101111     C     RET123        WHENEQ    '2'
157000970619     C* se il pgm di manutenzione ha prima una guida, è necessario anche
157100970619     c* questo test, altrimenti prevale il ret123 e il sfl non viene
157200970619     C* ricaricato
157300970619     C     OPR123        ANDNE     '1'
157400101111     C                   Move      *ON           $ESCI                          sospese READC
157500970619     C*
157600101111     C                   EndSl
157700970619     C*
157800970619     C* operazione eseguite dal pgm chiamato
157900970619     C*
158000101111     C                   Move      OPR123        WOPR                           avverto pgm chiamant
158100101111     C                   Select
158200101111     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
158300101111     C     OPR123        WHENEQ    '1'
158400101111     C*
158500101111     C* se l'opzione con cui è stato chiamato non è la modifica
158600101111     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
158700101111     C     OPZ123        IFNE      '02'
158800101111     C                   Move      *ON           WINZS1                         iniz.sfl fine ciclo
158900101111     C* se l'opzione con cui è stato chiamato è la modifica
159000970619     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
159100970619     C                   ELSE
159200101111     C                   Exsr      AGGS1
159300101111     C                   EndIf
159400970619     C*
159500101111     C                   EndSl
159600970619     C*
159700970619     C* funzione non eseguibile per errore :
159800970619     C*
159900101111     C                   Select
160000970619     C* 1 = funzione richiamata chiusa in errore
160100101111     C     ERR123        WHENEQ    '1'
160200101111     C                   Move      *ON           $ESCI                          sospese READC
160300101111     C                   SETON                                        9399      emiss.SFLMSGID
160400101111     C* 2 = funzione richiamata senza la dovuta autorizzazione
160500101111     C     ERR123        WHENEQ    '2'
160600101111     C                   Move      *ON           $ESCI                          sospese READC
160700101111     C                   SETON                                        9499      emiss.SFLMSGID
160800101111     C*
160900101111     C                   EndSl
161000101111     C*
161100970829     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
161200970829     C* di sola finestra
161300970829     C                   EVAL       WEXF = *Off
161400970619     C*
161500101111     C                   EndSr
161600970619     C************************************************************
161700970619     C* AGGIORNAMENTO RIGA LISTA
161800970619     C************************************************************
161900101111     C     AGGS1         BegSr
162000970619     C*
162100970619     C*
162200101111     C                   EndSr
162300101111     C************************************************************
162400101111     C* GESTIONE VALORI ASSUNTI
162500101111     C************************************************************
162600101111     C     GESD2         BegSr
162700101111     C*
162800101111     C     $INZD2        IFEQ      *ON
162900101111     C                   Exsr      INZD2
163000101111     C                   Move      *OFF          $INZD2
163100101111     C                   EndIf
163200101111     C*
163300101111     C     $LASTV        IFNE      'D2'
163400101111     C                   Exsr      GESPIE
163500101111     C     $LASTV        IFNE      *HIVAL
163600101111     C                   Move      $LASTV        $LASTG
163700101111     C                   EndIf
163800101111     C                   Move      'D2'          $LASTV
163900101111     C                   EndIf
164000101111     C*
164100101111     C* se esistono errori sulla videata
164200101111     C* emetto la write del formato a indicatori spenti per vedere
164300970619     C* le eventuali decodifiche
164400101111     C     *IN99         IFEQ      *ON
164500101111     C                   MoveA     *IN           WIN
164600101111     C                   Move      *ALL'0'       IN5098           49
164700101111     C                   MoveA     IN5098        *IN(50)
164800101111     C                   WRITE     FMTD2
164900101111     C                   MoveA     WIN           *IN
165000101111     C                   EndIf
165100101111     C*
165200101111     C                   EXFMT     FMTD2
165300101111     C*
165400970619     C* azzero la posizione del cursore
165500970619     C                   Z-ADD     0             H1RIGA
165600970619     C                   Z-ADD     0             H1COLO
165700970619     C*
165800970619     C* CONTROLLO ABILITAZIONE TASTI
165900970619     C* INPUT
166000970619     C* $FC3 = Abilitazioni videata D2
166100970619     C* OUTPUT
166200970619     C* *IN97 + *IN99 Errori
166300101111     C                   MoveA     $FC3          $FC
166400101111     C                   Exsr      CTRKEY
166500970619     C*
166600101111     C     *IN99         IFEQ      *OFF
166700101111     C*
166800101111     C                   Select
166900101111     C* F3=Fine
167000101111     C     $TASTO        WHENEQ    F03
167100101111     C                   Exsr      F03D2
167200101111     C* F12=Ritorno
167300101111     C     $TASTO        WHENEQ    F12
167400101111     C                   Exsr      F12D2
167500101111     C* F24=Altri tasti
167600970619     C     $TASTO        WHENEQ    F24
167700101111     C                   Exsr      F24D2
167800970619     C*
167900970619     C                   OTHER
168000101111     C                   Exsr      CTRD2
168100101111     C     *IN99         IFEQ      *OFF
168200101111     C                   Move      $LASTG        $GEST
168300101111     C                   Select
168400101111     C     $LASTG        WHENEQ    'S1'
168500101111     C* se è stato modificato l'ordinamento di default, ricarico la lista
168600101111     C     $ORD          IFNE      D2ORD
168700101111     C                   Move      *ON           $INZS1
168800101111     C                   Z-ADD     0             W1POS
168900101111     C                   EndIf
169000101111     C     $LASTG        WHENEQ    'D1'
169100970619     C* per ora niente
169200101111     C                   EndSl
169300101111     C* definisco i nuovi valori di default
169400101111     C                   Z-ADD     D2LOP         $LOP
169500101111     C                   Z-ADD     D2ORD         $ORD
169600101111     C*
169700101111     C* aggiorno valori assunti
169800101111     C* se richiesto il salvataggio
169900101111     C     D2BSV         IFEQ      1
170000101111     C                   Exsr      AGGMPE
170100101111     C                   EndIf
170200970619     C*
170300101111     C                   EndIf
170400101111     C*
170500101111     C                   EndSl
170600101111     C*
170700101111     C                   EndIf
170800101111     C*
170900101111     C                   EndSr
171000101111     C************************************************************
171100101111     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
171200970619     C************************************************************
171300101111     C     INZD2         BegSr
171400970619     C*
171500970619     C                   CLEAR                   FMTD2
171600970619     C*
171700101111     C                   MoveL     $LOP          D2LOP
171800101111     C                   Move      $ORD          D2ORD
171900970619     C*
172000101111     C                   EndSr
172100970619     C************************************************************
172200970619     C* GESTIONE F03 VIDEO D2
172300970619     C************************************************************
172400101111     C     F03D2         BegSr
172500970619     C*
172600101111     C                   Move      *ON           $FINE
172700970619     C* fine programma
172800101111     C                   Exsr      ENDPGM
172900970619     C*
173000101111     C                   EndSr
173100970619     C************************************************************
173200970619     C* GESTIONE F12 VIDEO D2
173300970619     C************************************************************
173400101111     C     F12D2         BegSr
173500970619     C*
173600101111     C                   Move      $LASTG        $GEST
173700970619     C*
173800101111     C                   EndSr
173900970619     C************************************************************
174000970619     C* GESTIONE F24 VIDEO D2
174100970619     C************************************************************
174200101111     C     F24D2         BegSr
174300970619     C*
174400101111     C                   MoveA     $FX3          $FX
174500101111     C                   MoveA     $FC3          $FC
174600101111     C                   MoveA     $FR3          $FR
174700970619     C                   Z-ADD     $ULKD2        $ULK
174800970619     C                   Z-ADD     $D2L01        $L01
174900970619     C                   Z-ADD     $D2L02        $L02
175000101111     C                   Exsr      RIEKEY
175100970619     C                   Z-ADD     $ULK          $ULKD2            3 0
175200101111     C                   MoveA     $FC           $FC3
175300101111     C                   MoveL     $KEY1         Z3KE1
175400101111     C                   MoveL     $ALLFUNCT     H3ALLFUNCT
175500970619     C                   WRITE     FMTZ3
175600970619     C*
175700101111     C                   EndSr
175800970619     C************************************************************
175900970619     C* CONTROLLO VIDEATA VALORI ASSUNTI
176000970619     C************************************************************
176100101111     C     CTRD2         BegSr
176200970619     C*
176300970619     C                   SETOFF                                       99
176400970619     C*
176500970619     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
176600970619     C     D2LOP         IFEQ      0
176700970619     C                   SETON                                          5099
176800101111     C                   EndIf
176900970619     C*
177000970619     C* OBBLIGATORIO VALORE PER ORDINAMENTO
177100970619     C     D2ORD         IFEQ      0
177200970619     C                   SETON                                          5199
177300101111     C                   EndIf
177400970619     C*
177500101111     C                   EndSr
177600970619     C************************************************************
177700970619     C* CONTROLLO ABILITAZIONE TASTI
177800970619     C************************************************************
177900101111     C     CTRKEY        BegSr
178000970619     C*
178100970619     C                   SETOFF                                       99
178200970619     C*
178300101111     C                   Select
178400970619     C     $TASTO        WHENEQ    F01
178500970619     C                   z-add     01            posfun
178600970619     C     $FC(1)        IFEQ      '0'
178700970619     C                   SETON                                        9997
178800101111     C                   EndIf
178900970619     C     $TASTO        WHENEQ    F02
179000970619     C                   z-add     02            posfun
179100970619     C     $FC(2)        IFEQ      '0'
179200970619     C                   SETON                                        9997
179300101111     C                   EndIf
179400970619     C     $TASTO        WHENEQ    F03
179500970619     C                   z-add     03            posfun
179600970619     C     $FC(3)        IFEQ      '0'
179700970619     C                   SETON                                        9997
179800101111     C                   EndIf
179900970619     C     $TASTO        WHENEQ    F04
180000970619     C                   z-add     04            posfun
180100970619     C     $FC(4)        IFEQ      '0'
180200970619     C                   SETON                                        9997
180300101111     C                   EndIf
180400970619     C     $TASTO        WHENEQ    F05
180500970619     C                   z-add     05            posfun
180600970619     C     $FC(5)        IFEQ      '0'
180700970619     C                   SETON                                        9997
180800101111     C                   EndIf
180900970619     C     $TASTO        WHENEQ    F06
181000970619     C                   z-add     06            posfun
181100970619     C     $FC(6)        IFEQ      '0'
181200970619     C                   SETON                                        9997
181300101111     C                   EndIf
181400970619     C     $TASTO        WHENEQ    F07
181500970619     C                   z-add     07            posfun
181600970619     C     $FC(7)        IFEQ      '0'
181700970619     C                   SETON                                        9997
181800101111     C                   EndIf
181900970619     C     $TASTO        WHENEQ    F08
182000970619     C                   z-add     08            posfun
182100970619     C     $FC(8)        IFEQ      '0'
182200970619     C                   SETON                                        9997
182300101111     C                   EndIf
182400970619     C     $TASTO        WHENEQ    F09
182500970619     C                   z-add     09            posfun
182600970619     C     $FC(09)       IFEQ      '0'
182700970619     C                   SETON                                        9997
182800101111     C                   EndIf
182900970619     C     $TASTO        WHENEQ    F10
183000970619     C                   z-add     10            posfun
183100970619     C     $FC(10)       IFEQ      '0'
183200970619     C                   SETON                                        9997
183300101111     C                   EndIf
183400970619     C     $TASTO        WHENEQ    F11
183500970619     C                   z-add     11            posfun
183600970619     C     $FC(11)       IFEQ      '0'
183700970619     C                   SETON                                        9997
183800101111     C                   EndIf
183900970619     C     $TASTO        WHENEQ    F12
184000970619     C                   z-add     12            posfun
184100970619     C     $FC(12)       IFEQ      '0'
184200970619     C                   SETON                                        9997
184300101111     C                   EndIf
184400970619     C     $TASTO        WHENEQ    F13
184500970619     C                   z-add     13            posfun
184600970619     C     $FC(13)       IFEQ      '0'
184700970619     C                   SETON                                        9997
184800101111     C                   EndIf
184900970619     C     $TASTO        WHENEQ    F14
185000970619     C                   z-add     14            posfun
185100970619     C     $FC(14)       IFEQ      '0'
185200970619     C                   SETON                                        9997
185300101111     C                   EndIf
185400970619     C     $TASTO        WHENEQ    F15
185500970619     C                   z-add     15            posfun
185600970619     C     $FC(15)       IFEQ      '0'
185700970619     C                   SETON                                        9997
185800101111     C                   EndIf
185900970619     C     $TASTO        WHENEQ    F16
186000970619     C                   z-add     16            posfun
186100970619     C     $FC(16)       IFEQ      '0'
186200970619     C                   SETON                                        9997
186300101111     C                   EndIf
186400970619     C     $TASTO        WHENEQ    F17
186500970619     C                   z-add     17            posfun
186600970619     C     $FC(17)       IFEQ      '0'
186700970619     C                   SETON                                        9997
186800101111     C                   EndIf
186900970619     C     $TASTO        WHENEQ    F18
187000970619     C                   z-add     18            posfun
187100970619     C     $FC(18)       IFEQ      '0'
187200970619     C                   SETON                                        9997
187300101111     C                   EndIf
187400970619     C     $TASTO        WHENEQ    F19
187500970619     C                   z-add     19            posfun
187600970619     C     $FC(19)       IFEQ      '0'
187700970619     C                   SETON                                        9997
187800101111     C                   EndIf
187900970619     C     $TASTO        WHENEQ    F20
188000970619     C                   z-add     20            posfun
188100970619     C     $FC(20)       IFEQ      '0'
188200970619     C                   SETON                                        9997
188300101111     C                   EndIf
188400970619     C     $TASTO        WHENEQ    F21
188500970619     C                   z-add     21            posfun
188600970619     C     $FC(21)       IFEQ      '0'
188700970619     C                   SETON                                        9997
188800101111     C                   EndIf
188900970619     C     $TASTO        WHENEQ    F22
189000970619     C                   z-add     22            posfun
189100970619     C     $FC(22)       IFEQ      '0'
189200970619     C                   SETON                                        9997
189300101111     C                   EndIf
189400970619     C     $TASTO        WHENEQ    F23
189500970619     C                   z-add     23            posfun
189600970619     C     $FC(23)       IFEQ      '0'
189700970619     C                   SETON                                        9997
189800101111     C                   EndIf
189900970619     C     $TASTO        WHENEQ    F24
190000970619     C                   z-add     24            posfun
190100970619     C     $FC(24)       IFEQ      '0'
190200970619     C                   SETON                                        9997
190300101111     C                   EndIf
190400101111     C                   EndSl
190500970619     C*
190600101111     C                   EndSr
190700970619     C************************************************************
190800970619     C* AGGIORNAMENTO VALORI ASSUNTI
190900970619     C************************************************************
191000101111     C     AGGMPE        BegSr
191100970619     C*
191200970619     C* preparo i parametri di chiamata al driver
191300970619     C                   CLEAR                   XMPEDS
191400970619     C                   EVAL      TpChiamata = '03'
191500970619     C                   EVAL      NChiavi = 5
191600970619     C                   EVAL      VLogica = 2
191700970619     C*
191800970619     C* preparo le chiavi per la chiamata al driver
191900970619     C                   CLEAR                   XMPEDS1
192000101111     C                   MoveL     XSCSOC        DSMPESOC
192100101111     C                   MoveL     DSPGM         DSMPEPGM
192200101111     C                   MoveL     KNMUS         DSMPEPRF
192300970619     C*
192400970619     C* Si memorizzano i dati di default a seconda delle modalità con
192500970619     C* cui il pgm è usato
192600970619     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
192700970619     C* MPETIP = 'PV'  pgm richiamato da menù
192800970619     C* MPETIP = 'PB'  x lancio Batch
192900970619     C*
193000101111     C                   Select
193100101111     C     OPZD14        WHENEQ    '01'
193200101111     C                   MoveL     'PX'          DSMPETIP
193300970619     C                   OTHER
193400101111     C                   MoveL     'PV'          DSMPETIP
193500101111     C                   EndSl
193600970619     C* dati
193700101111     C                   MoveL     DSMPE         DSMPEPAR
193800970619     C*
193900970619     C                   CALLB     'XMPE'
194000970619     C                   PARM                    XMPEDS
194100970619     C                   PARM                    XMPEDS1
194200970619     C*
194300101111     C                   EndSr
194400970619     C************************************************************
194500970619     C* OPERAZIONI INIZIALI
194600970619     C************************************************************
194700101111     C     *INZSR        BegSr
194800101111     C     *DMY          Move      Udate         ZDateISO
194900101111     C     *JobRun       Move      ZDateISO      ZDate             6 0
195000970619     C*
195100970619     C* Reperimento parametri
195200970619     C     *ENTRY        PLIST
195300970619     C                   PARM                    KPJBA
195400970619     C                   PARM                    DSAUT
195500970619     C*
195600970619     C* testo se sono stati passati i dati di autorizzazione
195700970619     C     PARMS         IFLT      2
195800101111     C                   Move      *BLANK        AUTDS
195900970619     C                   ELSE
196000101111     C                   MoveL     DSAUT         AUTDS
196100101111     C                   EndIf
196200970619     C*
196300970619     C* Reperimento dati società
196400101111     C                   MoveL     'SOC001'      TIPXSC
196500101111     C                   MoveL     *BLANK        SOCXSC
196600101111     C                   Exsr      REPSOC
196700970619     C*
196800970619     C* Controlli dati società
196900970619     C     RTNXSC        IFNE      '1'
197000101111     C                   MoveL     XSOCDS        SOC001
197100970619     C                   ELSE
197200101111     C                   Move      *ON           $FINE
197300101111     C                   Move      '1'           ErrD14
197400101111     C                   Exsr      ENDPGM
197500101111     C                   EndIf
197600101111     C*
197700101111     C* reperisco le dimensioni delle variabili che conterranno i tasti
197800101111     C                   EVAL      $D1L01 = %Size(Z1Ke1)
197900101111     C                   EVAL      $D1L02 = %Size(Z1Ke2)
198000101111     C                   EVAL      $S1L01 = %Size(Z2Ke1)
198100101111     C                   EVAL      $S1L02 = %Size(Z2Ke2)
198200101111     C                   EVAL      $C1Op1 = %Size(C1Opz1)
198300970619     C                   EVAL      $C1Op2 = %Size(C1Opz2)
198400970619     C                   EVAL      $D2L01 = %Size(Z3Ke1)
198500970619     C                   EVAL      $D2L02 = 0
198600970619     C*
198700970619     C* Reperimento opzioni
198800101111     C                   Exsr      REPOPZ
198900970619     C*
199000970619     C* Reperimento tasti di funzione
199100101111     C                   Exsr      REPKEY
199200970619     C*
199300970619     C* Chiamata a routine per il reperimento delle opzioni
199400970619     C* personalizzate
199500970619     C                   CAllb     'X46USREXT'
199600970619     C                   PARM                    $OP1
199700970619     C                   PARM                    $TX1
199800970619     C                   PARM                    $FL1
199900970619     C                   PARM                    $PGO1
200000970619     C                   PARM      $dimOp        NumOpz
200100970619     C                   PARM                    $FX2
200200970619     C                   PARM                    $FC2
200300970619     C                   PARM                    $PGF2
200400970619     C                   PARM                    DsPgm
200500970619     C                   PARM                    xsccut
200600970619     C                   PARM                    xscgrp
200700970619     C*
200800970619     C                   Z-add     1             OPZLEN
200900970619     C     *blank        Lookup    $TX1(opzlen)                           21
201000970619     C                   if        *in21 = *on
201100970619     C                   sub       1             OPZLEN
201200970619     C                   else
201300970619     C                   Z-add     $DimOp        OPZLEN
201400101111     C                   EndIf
201500970619     C*
201600101111     C                   EndSr
201700970619     C************************************************************
201800970619     C* INIZIALIZZAZIONE VARIABILI
201900970619     C************************************************************
202000101111     C     INZVAR        BegSr
202100970619     C*
202200101111     C                   MoveL     KPJBU         SDGSD14DS
202300970619     C*
202400101111     C* Forzo la ricerca
202500101111     C                   MoveL     '01'          Opzd14
202600101111     C*
202700970619     C* Se società diversa da quella impostata nella *inzsr, rieseguo
202800970619     C* reperimento dati società
202900101111     C                   IF        SocietaD14 <> xscsoc  and
203000101111     C                             OpzD14 <>*blank
203100101111     C                   MoveL     'SOC001'      TIPXSC
203200101111     C                   MoveL     SocietaD14    SOCXSC
203300101111     C                   Exsr      REPSOC
203400101111     C     RTNXSC        IFNE      '1'
203500101111     C                   MoveL     XSOCDS        SOC001
203600101111     C                   ELSE
203700101111     C                   Move      *ON           $FINE
203800101111     C                   Move      '1'           ErrD14
203900101111     C                   Exsr      ENDPGM
204000101111     C                   EndIf
204100101111     C                   EndIf
204200970619     C*
204300970619     C* Reperimento valori default utente
204400101111     C                   Exsr      REPDFT
204500970619     C*
204600970619     C* Definizione delle opzioni utilizzabili a pgm
204700101111     C                   Exsr      DEFOPZ
204800970619     C*
204900970619     C* Variabili per gestione videate
205000970619     C*
205100101111     C                   Move      *OFF          $FINE                          fine pgm
205200101111     C                   Move      *OFF          $INZD1                         inizializzione D1
205300101111     C                   Move      *OFF          $INZS1                         inizializzione S1
205400101111     C                   Move      *OFF          $INZD2                         inizializzione D2
205500101111     C                   Move      *OFF          $EndFile                       memo fine file
205600101111     C                   Move      *OFF          $ESCI                          rich.uscita ciclo
205700101111     C                   Move      *OFF          $RCDOK                         rcd letto valido
205800101111     C                   Move      *HIVAL        $LASTV
205900970619     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
206000970619     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
206100970619     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
206200970619     C* azzero la posizione del cursore
206300970619     C                   Z-ADD     0             H1RIGA
206400970619     C                   Z-ADD     0             H1COLO
206500970619     C*
206600101111     C                   Move      *ON           $INZD1
206700101111     C                   Move      *ON           $INZS1
206800970619     C*
206900970619     C* Variabili appoggio
207000970619     C                   Z-ADD     SFLPAG        WPAG
207100101111     C                   MoveL     *OFF          WOPR
207200970619     C                   CLEAR                   W1POS
207300970619     C*
207400101111     C                   Select
207500970619     C     $LOP          WHENEQ    1
207600101111     C                   Move      'D1'          $GEST
207700970619     C     $LOP          WHENEQ    2
207800101111     C                   Move      'S1'          $GEST
207900101111     C                   Exsr      INZD1
208000101111     C                   Move      *OFF          $INZD1
208100101111     C                   Exsr      DEFORD
208200101111     C                   EndSl
208300970619     C*
208400970619     C* Valorizzazione campi univoci testate
208500970619     C                   CLEAR                   FMTT1
208600101111     C                   MoveL     KNSIF         NOMSIF
208700101111     C                   MoveL     XSCDSI        NOMDIT
208800101116     C                   MOVEL     KCDAZ         NOMAZN
208900101116     C                   MOVEL     DSPGM         NOMPGM           10
209000970619     C* recupero il sottotitolo (se c'è)
209100101111     C                   MoveL     *BLANK        T1TIT
209200970619     C     SOTTIT        IFNE      *BLANK
209300101111     C                   MoveL     SOTTIT        IDMSG
209400970619     C                   CALLB     'XRTVM'
209500970619     C                   PARM                    IDMSG            27
209600970619     C                   PARM                    TXTMSG          100
209700970619     C                   PARM                    ERRMSG            1
209800970619     C                   PARM      *ON           CTRMSG            1            centratura
209900970619     C                   PARM      30            LENMSG            3 0          lun output.
210000970619     C     ERRMSG        IFNE      *ON
210100101111     C                   MoveL     TXTMSG        T1TIT
210200970619     C                   ELSE
210300101111     C                   MoveL     *ALL'?'       T1TIT
210400101111     C                   EndIf
210500101111     C                   EndIf
210600131119R132 C*
210700131119  "  C*
210800131119  "  C* recupero il titolo con il tipo comunicazione
210900131119  "  C                   MOVEL     *BLANK        T1TIT
211000131119  "  C*
211100131119  "  C                   Select
211200131119  "  C* Comunicazione Tracciato Polivalente
211300160208R132 C                   When      TpComD14    = 'B'
211400160208R195 C                             or
211500160208  "  C* Black List Annuale Tracciato Polivalente
211600160208R195 C                             TpComD14    = 'L'
211700160208R132 C*
211800131119  "  C                   if        SOTTITT1   <> *BLANK
211900131119  "  C                   MOVEL     SOTTITT1      IDMSG
212000131119  "  C*
212100131119  "  C                   CALLB     'XRTVM'
212200131119  "  C                   PARM                    IDMSG            27
212300131122  "  C                   PARM      *Blank        TXTMSG          100
212400131119  "  C                   PARM                    ERRMSG            1
212500131119  "  C                   PARM      *ON           CTRMSG            1            centratura
212600131119  "  C                   PARM      30            LENMSG            3 0          lun output.
212700131119  "  C     ERRMSG        IFNE      *ON
212800131119  "  C                   MOVEL     TXTMSG        T1TIT
212900131119  "  C                   ELSE
213000131119  "  C                   MOVEL     *ALL'?'       T1TIT
213100131122  "  C                   ENDIF
213200131122  "  C*
213300131122  "  C                   endif
213400131122  "  C*
213500131122R132 C                   endSl
213600970619     C*
213700101111     C                   EndSr
213800970619     C************************************************************
213900970619     C* REPERIMENTO DATI SOCIETA'
214000970619     C************************************************************
214100101111     C     REPSOC        BegSr
214200970619     C*
214300101111     C                   CALL      'SDGSOCDR'
214400101111     C                   PARM                    TIPXSC            6
214500101111     C                   PARM                    SOCXSC            3
214600101111     C                   PARM                    CDSXSC            9 0
214700101111     C                   PARM                    MODXSC            3
214800101111     C                   PARM      *BLANKS       RTNXSC            1
214900101111     C                   PARM                    XSOCDS
215000101111     C                   PARM                    KPJBA
215100970619     C*
215200101111     C                   EndSr
215300970619     C************************************************************
215400970619     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
215500970619     C************************************************************
215600101111     C     REPDFT        BegSr
215700970619     C*
215800970619     C* preparo i parametri di chiamata al driver
215900970619     C                   CLEAR                   XMPEDS
216000970619     C                   EVAL      TpChiamata = '01'
216100970619     C                   EVAL      NChiavi = 5
216200970619     C                   EVAL      VLogica = 2
216300970619     C*
216400970619     C* preparo le chiavi per la chiamata al driver
216500970619     C                   CLEAR                   XMPEDS1
216600101111     C                   MoveL     XSCSOC        DSMPESOC
216700101111     C                   MoveL     DSPGM         DSMPEPGM
216800101111     C                   MoveL     KNMUS         DSMPEPRF
216900970619     C* Si reperiscono i dati di default a seconda delle modalità con
217000970619     C* cui il pgm è usato
217100970619     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
217200970619     C* MPETIP = 'PV'  pgm richiamato da menù
217300970619     C* MPETIP = 'PB'  x lancio Batch
217400101111     C                   Select
217500101111    >C     OPZD14        WHENEQ    '01'
217600101111     C                   MoveL     'PX'          DSMPETIP
217700970619     C                   OTHER
217800101111     C                   MoveL     'PV'          DSMPETIP
217900101111     C                   EndSl
218000970619     C*
218100970619     C                   CALLB     'XMPE'
218200970619     C                   PARM                    XMPEDS
218300970619     C                   PARM                    XMPEDS1
218400970619     C*
218500970619     C     Risultato     IFEQ      '0'
218600101111     C                   MoveL     DSMPEPAR      DSMPE
218700970619     C* nel caso sia la prima volta che l'utente apre questa anagrafica
218800970619     C* i suoi valori di default vengono decisi a pgm
218900970619     C                   ELSE
219000970619     C                   Z-ADD     1             $LOP
219100970619     C                   Z-ADD     1             $ORD
219200101111     C                   EndIf
219300970619     C*
219400101111     C                   EndSr
219500970619     C/EJECT
219600970619     C************************************************************
219700970619     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
219800970619     C************************************************************
219900101111     C     DEFOPZ        BegSr
220000970619     C*
220100970619     C* Questa routine deve valorizzare la schiera $FL1
220200970619     C* con S o N a seconda della utilizzabilità o meno
220300970619     C* dell'opzione correlata
220400970619     C*
220500970619     C* Inizialmente si considerano tutte le opzioni come valide
220600970619     C     1             DO        $DIMOP        X
220700970619     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
220800970619     C                   If        $TX1(X)  <> *Blank
220900101111     C                   Move      'S'           $FL1(X)
221000970619     C                   Else
221100970619     C                   Leave
221200101111     C                   EndIf
221300101111     C                   EndDo
221400970619     C*
221500970619     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
221600970619     C* con 1 o 0 a seconda della utilizzabilità o meno
221700970619     C* del tasto funzione correlato
221800970619     C*
221900970619     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
222000970619     C* di compilazione
222100970619     C                   RESET                   $FC1
222200970619     C                   RESET                   $FR1
222300970619     C                   RESET                   $FC2
222400970619     C                   RESET                   $FR2
222500970619     C                   RESET                   $FC3
222600970619     C                   RESET                   $FR3
222700970619     C*
222800970619     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
222900970619     C* invalido alcune opzioni
223000101111     C                   Select
223100970619     C* Se pgm richiamato in ricerca disattivo :
223200101111    >C     OPZD14        WHENEQ    '01'
223300970619     C* F3=Fine
223400101111     C                   Z-ADD     3             X
223500101111     C***                Move      '0'           $FC1(X)
223600101111     C                   Move      '0'           $FC2(X)
223700101111     C                   Move      '0'           $FC3(X)
223800101111     C* 15
223900101111     C                   Z-ADD     1             X
224000101111     C     15            LOOKUP    $OP1(X)                                21
224100101111     C     *IN21         IFEQ      *ON
224200101111     C                   Move      'N'           $FL1(X)
224300101111     C                   EndIf
224400101111     C* 16
224500101111     C                   Z-ADD     1             X
224600101111     C     16            LOOKUP    $OP1(X)                                21
224700101111     C     *IN21         IFEQ      *ON
224800101111     C                   Move      'N'           $FL1(X)
224900101111     C                   EndIf
225000101111     C* 20
225100101111     C                   Z-ADD     1             X
225200101111     C     20            LOOKUP    $OP1(X)                                21
225300101111     C     *IN21         IFEQ      *ON
225400101111     C                   Move      'N'           $FL1(X)
225500101111     C                   EndIf
225600101111     C* 21
225700101111     C                   Z-ADD     1             X
225800101111     C     21            LOOKUP    $OP1(X)                                21
225900101111     C     *IN21         IFEQ      *ON
226000101111     C                   Move      'N'           $FL1(X)
226100101111     C                   EndIf
226200101111     C* 29
226300101111     C                   Z-ADD     1             X
226400101111     C     29            LOOKUP    $OP1(X)                                21
226500101111     C     *IN21         IFEQ      *ON
226600101111     C                   Move      'N'           $FL1(X)
226700101111     C                   EndIf
226800970619     C*
226900970619     C* Se pgm richiamato in interrogazione disattivo :
227000101111    >C     OPZD14        WHENEQ    '05'
227100970619     C* F10=Immissione
227200970619     C                   Z-ADD     10            X
227300101111     C                   Move      '0'           $FC1(X)
227400101111     C                   Move      '0'           $FC2(X)
227500970619     C* 1=Selezione
227600970619     C                   Z-ADD     1             X
227700970619     C     01            LOOKUP    $OP1(X)                                21
227800970619     C     *IN21         IFEQ      *ON
227900101111     C                   Move      'N'           $FL1(X)
228000101111     C                   EndIf
228100970619     C* 2=Modifica
228200970619     C                   Z-ADD     1             X
228300970619     C     02            LOOKUP    $OP1(X)                                21
228400970619     C     *IN21         IFEQ      *ON
228500101111     C                   Move      'N'           $FL1(X)
228600101111     C                   EndIf
228700970619     C* 3=Copia
228800970619     C                   Z-ADD     1             X
228900970619     C     03            LOOKUP    $OP1(X)                                21
229000970619     C     *IN21         IFEQ      *ON
229100101111     C                   Move      'N'           $FL1(X)
229200101111     C                   EndIf
229300970619     C* 4=Annullamento
229400970619     C                   Z-ADD     1             X
229500970619     C     04            LOOKUP    $OP1(X)                                21
229600970619     C     *IN21         IFEQ      *ON
229700101111     C                   Move      'N'           $FL1(X)
229800101111     C                   EndIf
229900970619     C*
230000101111     C     OPZD14        WHENEQ    *BLANKS
230100970619     C* Se pgm non richiamato, disattivo subito :
230200970619     C* F8=Selezione seguente
230300970619     C                   Z-ADD     8             X
230400101111     C                   Move      '0'           $FC1(X)
230500101111     C                   Move      '0'           $FC2(X)
230600970619     C* 1=Selezione
230700970619     C                   Z-ADD     1             X
230800970619     C     01            LOOKUP    $OP1(X)                                21
230900970619     C     *IN21         IFEQ      *ON
231000101111     C                   Move      'N'           $FL1(X)
231100101111     C                   EndIf
231200970619     C*
231300970619     C* In tutti gli altri casi, disattivo subito :
231400970619     C                   OTHER
231500970619     C* 1=Selezione
231600970619     C                   Z-ADD     1             X
231700970619     C     01            LOOKUP    $OP1(X)                                21
231800970619     C     *IN21         IFEQ      *ON
231900101111     C                   Move      'N'           $FL1(X)
232000101111     C                   EndIf
232100970619     C*
232200101111     C                   EndSl
232300970619     C*
232400101111     C                   EndSr
232500970619     C************************************************************
232600970619     C* REPERIMENTO OPZIONI
232700970619     C************************************************************
232800101111     C     REPOPZ        BegSr
232900970619     C*
233000970619     C     1             DO        $DIMOP        X
233100970619     C*
233200970619     C     $MS1(X)       IFNE      *BLANK
233300970619     C*
233400101111     C                   MoveL     $MS1(X)       IDMSG
233500970619     C                   CALLB     'XRTVM'
233600970619     C                   PARM                    IDMSG            27
233700970619     C                   PARM                    TXTMSG          100
233800970619     C                   PARM                    ERRMSG            1
233900970619     C*
234000970619     C     ERRMSG        IFNE      *ON
234100101111     C                   MoveL     TXTMSG        $TX1(X)
234200970619     C                   ELSE
234300101111     C                   MoveL     *ALL'?'       $TX1(X)
234400101111     C                   EndIf
234500970619     C*
234600101111     C                   EndIf
234700970619     C*
234800101111     C                   EndDo
234900970619     C*
235000101111     C                   EndSr
235100970619     C************************************************************
235200970619     C* REPERIMENTO TASTI DI FUNZIONE
235300970619     C************************************************************
235400101111     C     REPKEY        BegSr
235500101111     C                   MoveA     $FM1          $FM
235600101111     C                   MoveA     $FC1          $FC
235700101111     C                   Exsr      REPKKK
235800101111     C                   MoveA     $FX           $FX1
235900970619     C*
236000101111     C                   MoveA     $FM2          $FM
236100101111     C                   MoveA     $FC2          $FC
236200101111     C                   Exsr      REPKKK
236300101111     C                   MoveA     $FX           $FX2
236400970619     C*
236500101111     C                   MoveA     $FM3          $FM
236600101111     C                   MoveA     $FC3          $FC
236700101111     C                   Exsr      REPKKK
236800101111     C                   MoveA     $FX           $FX3
236900101111     C                   EndSr
237000970619     C************************************************************
237100970619     C* REPERIMENTO TASTI DI FUNZIONE
237200970619     C************************************************************
237300101111     C     REPKKK        BegSr
237400970619     C*
237500970619     C                   CLEAR                   $FX
237600970619     C     1             DO        24            X
237700970619     C*
237800970619     C     $FM(X)        IFNE      *BLANK
237900970619     C     $Fc(X)        andne     '0'
238000970619     C*
238100101111     C                   MoveL     $FM(X)        IDMSG
238200970619     C                   CALLB     'XRTVM'
238300970619     C                   PARM                    IDMSG            27
238400970619     C                   PARM                    TXTMSG          100
238500970619     C                   PARM                    ERRMSG            1
238600970619     C*
238700970619     C     ERRMSG        IFNE      *ON
238800101111     C                   MoveL     TXTMSG        $FX(X)
238900970619     C                   ELSE
239000101111     C                   MoveL     *ALL' '       $FX(X)
239100101111     C                   EndIf
239200970619     C*
239300101111     C                   EndIf
239400970619     C*
239500101111     C                   EndDo
239600970619     C*
239700101111     C                   EndSr
239800970619     C************************************************************
239900970619     C* DEFINIZIONE KLIST
240000970619     C************************************************************
240100101111     C     DefKlist      BegSr
240200970619     C*
240300970619     C* klist
240400970619     C*
240500160825brt  C***  k02cfm02      KLIST
240600160825brt  C     k03cfm02      KLIST
240700970623     C                   KFLD                    CFMData
240800160825brt  C                   KFLD                    CFMOra
240900970623     C                   KFLD                    CFMSocieta
241000970619     C*
241100101111     C                   EndSr
241200101111     C************************************************************
241300970619**
241400970619PROMSG    *LIBL     OPZ0001
241500970619PROMSG    *LIBL     OPZ0005
241600101111PROMSG    *LIBL     OPZ0001
241700101111PROMSG    *LIBL     OPZ0005
241800101111PROMSG    *LIBL     OPZ0001
241900101111PROMSG    *LIBL     OPZ0005
242000101111PROMSG    *LIBL     OPZ0001
242100970619**
24220097061901S      <Selezione
24230097061905S      <Visualizzazione
24240010111115N
24250010111116N
24260010111120N
24270010111121N
24280010111129N
242900970619** TASTI DI FUNZIONE  D1
243000970619PROMSG    *LIBL     KEY0001  01
243100970619PROMSG    *LIBL     KEY0002  02
243200970619PROMSG    *LIBL     KEY0003  03
243300970619PROMSG    *LIBL     KEY0004  04
243400970619PROMSG    *LIBL     KEY0005  05
243500970619PROMSG    *LIBL     KEY0006  06
243600970619PROMSG    *LIBL     KEY0007  07
243700970619PROMSG    *LIBL     KEY0008  08
243800970619PROMSG    *LIBL     KEY0009  09
243900970619PROMSG    *LIBL     KEY0010  10
244000970619PROMSG    *LIBL     KEY0011  11
244100970619PROMSG    *LIBL     KEY0012  12
244200970619PROMSG    *LIBL     KEY0013  13
244300970619PROMSG    *LIBL     KEY0014  14
244400970619PROMSG    *LIBL     KEY0015  15
244500970619PROMSG    *LIBL     KEY0016  16
244600970619PROMSG    *LIBL     KEY0017  17
244700970619PROMSG    *LIBL     KEY0018  18
244800970619PROMSG    *LIBL     KEY0019  19
244900970619PROMSG    *LIBL     KEY0020  20
245000970619PROMSG    *LIBL     KEY0021  21
245100970619PROMSG    *LIBL     KEY0022  22
245200970619PROMSG    *LIBL     KEY0023  23
245300970619PROMSG    *LIBL     KEY0024  24
245400970619**  ABILITAZIONE D1
2455009706190N  01
2456001011110N  02
2457001011111N  03
2458009706191N  04
2459009706190N  05
2460009706190N  06
2461009706190N  07
2462009706191N  08
2463009706190N  09
2464009706190N  10
2465009706190N  11
2466001011110N  12
2467009706190N  13
2468009706190N  14
2469009706190N  15
2470009706190N  16
2471009706190N  17
2472009706191N  18
2473009706190N  19
2474009706190N  20
2475009706190N  21
2476009706190N  22
2477009706190N  23   Impostare a '1' se si utilizzano opzioni
2478009706191N  24   Lasciare sempre impostata a '1' il valore della riga 24
247900970619** TASTI DI FUNZIONE  S1
248000970619PROMSG    *LIBL     KEY0001  01
248100970619PROMSG    *LIBL     KEY0002  02
248200970619PROMSG    *LIBL     KEY0003  03
248300970619PROMSG    *LIBL     KEY0004  04
248400970619PROMSG    *LIBL     KEY0005  05
248500970619PROMSG    *LIBL     KEY0006  06
248600970619PROMSG    *LIBL     KEY0007  07
248700970619PROMSG    *LIBL     KEY0008  08
248800970619PROMSG    *LIBL     KEY0009  09
248900970619PROMSG    *LIBL     KEY0010  10
249000970619PROMSG    *LIBL     KEY0011  11
249100970619PROMSG    *LIBL     KEY0012  12
249200970619PROMSG    *LIBL     KEY0013  13
249300970619PROMSG    *LIBL     KEY0014  14
249400970619PROMSG    *LIBL     KEY0015  15
249500970619PROMSG    *LIBL     KEY0016  16
249600970619PROMSG    *LIBL     KEY0017  17
249700970619PROMSG    *LIBL     KEY0018  18
249800970619PROMSG    *LIBL     KEY0019  19
249900970619PROMSG    *LIBL     KEY0020  20
250000970619PROMSG    *LIBL     KEY0021  21
250100970619PROMSG    *LIBL     KEY0022  22
250200970619PROMSG    *LIBL     KEY0023  23
250300970619PROMSG    *LIBL     KEY0024  24
250400970619**  ABILITAZIONE S1
2505009706190N  01
2506009706190N  02
2507009706191N  03
2508009706190N  04
2509009706190N  05
2510009706190N  06
2511009706190N  07
2512009706191N  08
2513000410271N  09
2514009706190N  10
2515009706190N  11
2516009706191N  12
2517009706190N  13
2518009706190N  14
2519009706190N  15
2520009706190N  16
2521009706191N  17
2522009706191N  18
2523009706190N  19
2524009706190N  20
2525009706190N  21
2526009706190N  22
2527009706191N  23   Impostare a '1' se si utilizzano opzioni
2528009706191N  24   Lasciare sempre impostata a '1' il valore della riga 24
252900970619** TASTI DI FUNZIONE  D3
253000970619PROMSG    *LIBL     KEY0001  01
253100970619PROMSG    *LIBL     KEY0002  02
253200970619PROMSG    *LIBL     KEY0003  03
253300970619PROMSG    *LIBL     KEY0004  04
253400970619PROMSG    *LIBL     KEY0005  05
253500970619PROMSG    *LIBL     KEY0006  06
253600970619PROMSG    *LIBL     KEY0007  07
253700970619PROMSG    *LIBL     KEY0008  08
253800970619PROMSG    *LIBL     KEY0009  09
253900970619PROMSG    *LIBL     KEY0010  10
254000970619PROMSG    *LIBL     KEY0011  11
254100970619PROMSG    *LIBL     KEY0012  12
254200970619PROMSG    *LIBL     KEY0013  13
254300970619PROMSG    *LIBL     KEY0014  14
254400970619PROMSG    *LIBL     KEY0015  15
254500970619PROMSG    *LIBL     KEY0016  16
254600970619PROMSG    *LIBL     KEY0017  17
254700970619PROMSG    *LIBL     KEY0018  18
254800970619PROMSG    *LIBL     KEY0019  19
254900970619PROMSG    *LIBL     KEY0020  20
255000970619PROMSG    *LIBL     KEY0021  21
255100970619PROMSG    *LIBL     KEY0022  22
255200970619PROMSG    *LIBL     KEY0023  23
255300970619PROMSG    *LIBL     KEY0024  24
255400970619**  ABILITAZIONE D3
2555009706190N  01
2556009706190N  02
2557009706191N  03
2558009706190N  04
2559009706190N  05
2560009706190N  06
2561009706190N  07
2562009706190N  08
2563009706190N  09
2564009706190N  10
2565009706190N  11
2566009706191N  12
2567009706190N  13
2568009706190N  14
2569009706190N  15
2570009706190N  16
2571009706190N  17
2572009706190N  18
2573009706190N  19
2574009706190N  20
2575009706190N  21
2576009706190N  22
2577009706190N  23   Impostare a '1' se si utilizzano opzioni
2578009706191N  24   Lasciare sempre impostata a '1' il valore della riga 24
