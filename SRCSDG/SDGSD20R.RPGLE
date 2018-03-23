000100120305     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200120305     H*PARMS CVTOPT(*NONE)
000300120305     H DECEDIT('0,') DATEDIT(*DMY.)
000400120305      *
000500120305      *
000600120305      *            Black list : importi soglia
000700120305      *                      guida
000800120305      *
000900120305     F*----------------------------------------------------*
001000120305     FSDGBLS01L IF   E           K DISK
001100120305     F*------------
001200120305     FSDGSD20D  CF   E             WORKSTN
001300120305     F                                     RENAME(SD20D1:FMTD1)
001400120305     F                                     RENAME(SD20D2:FMTD2)
001500120305     F                                     RENAME(SD20D3:FMTD3)
001600120305     F                                     RENAME(SD20S1:FMTS1)
001700120305     F                                     RENAME(SD20C1:FMTC1)
001800120305     F                                     RENAME(SD20T1:FMTT1)
001900120305     F                                     RENAME(SD20Z1:FMTZ1)
002000120305     F                                     RENAME(SD20Z2:FMTZ2)
002100120305     F                                     RENAME(SD20Z3:FMTZ3)
002200120305     F                                     SFILE(FMTS1:S1NRR)
002300120305     F                                     INFDS(DSFMT)
002400120305     D*----------------------------------------------------*
002500120305     D* Schiere per la gestione delle opzioni
002600120305     D*
002700120305     D* dimensione della schiera $MS1, da non cambiare
002800120305     D $DIMOP          C                   CONST(30)
002900120305     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003000120305     D $TX1            S             50    DIM($dimop) ALT($MS1)
003100120305     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003200120305     D $FL1            S              1    DIM($dimop) ALT($OP1)
003300120305     D* schiera contenente i nomi dei programma richiamati attraverso
003400120305     D* le opzioni personalizzate
003500120305     D $PGO1           S             10    DIM($dimop)
003600120305     D $TX             S             50    DIM(50)
003700120305     D $FL             S              1    DIM(50)
003800120305     D* Schiere generali per la gestione dei tasti di funzione
003900120305     D $FM             S             27    DIM(24)
004000120305     D $FX             S             50    DIM(24)
004100120305     D $FC             S              1    DIM(24)
004200120305     D $FR             S              1    DIM(24)
004300120305     D* Schiere per la gestione dei tasti di funzione
004400120305     D* Video D1
004500120305     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
004600120305     D $FX1            S             50    DIM(24) ALT($FM1)
004700120305     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
004800120305     D $FR1            S              1    DIM(24) ALT($FC1)
004900120305     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005000120305     D* funzione personalizzata S1
005100120305     D $PGF2           S             10    DIM(25)
005200120305     D* Schiere per la gestione dei tasti di funzione
005300120305     D* Video S1
005400120305     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005500120305     D $FX2            S             50    DIM(24) ALT($FM2)
005600120305     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
005700120305     D $FR2            S              1    DIM(24) ALT($FC2)
005800120305     D* Schiere per la gestione dei tasti di funzione
005900120305     D* Video D2
006000120305     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
006100120305     D $FX3            S             50    DIM(24) ALT($FM3)
006200120305     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
006300120305     D $FR3            S              1    DIM(24) ALT($FC3)
006400120305     D*
006500120305     D $D1L01          S              3  0
006600120305     D $D1L02          S              3  0
006700120305     D $S1L01          S              3  0
006800120305     D $S1L02          S              3  0
006900120305     D $C1Op1          S              3  0
007000120305     D $C1Op2          S              3  0
007100120305     D $D2L01          S              3  0
007200120305     D $D2L02          S              3  0
007300120305      * Inserire qui la definizione di altre variabili per reperire
007400120305      * la lunghezza di altre videate che usano un piede diverso da
007500120305      * STD
007600120305     D*
007700120305     D*-------------
007800120305     D* Indice della schiera tasti funzione
007900120305     D PosFun          S              2  0
008000120305     D/EJECT
008100120305     D*----------------------------------------------------*
008200120305     D* Valori assunti
008300120305     D XMPEDS        E DS                  INZ
008400120305     D XMPEDS1       E DS                  INZ
008500120305     D DSMPE           DS                  INZ
008600120305     D  $LOP                   1      1  0
008700120305     D  $ORD                   2      3  0
008800120305     D*-------------
008900120305     D* Passaggio Parametri
009000120305     D KPJBA         E DS
009100120305     D*-------------
009200120305     D* Reperimento Autorizzazioni
009300120305     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009400120305     D*-------------
009500120305     D* Passaggio Autorizzazioni
009600120305     D DSAUT           DS           512
009700120305     D*-------------
009800120305     D* Maschera dati autorizzazioni
009900120305     D CONCIVDS      E DS
010000120305     D*-------------
010100120305     D* Dati di ambiente ottenuti da XSOC
010200120305     D SOC001        E DS                  EXTNAME(SDGSOCDS)
010300120305     D*-------------
010400120305     D* DS Interna per dati di output di SDGSOCDR
010500120305     D XSOCDS          DS          1000
010600120305     D*-------------
010700120305     D* Parametri in ricezione
010800120305     D SDGSD20DS     E DS
010900120305     D*-------------
011000120305     D* Parametri del PGM di manutenzione SDGSD21R
011100120305     D SDGSD21DS     E DS                  inz
011200120305     D  RETD21       E                     INZ('0')
011300120305     D  OPRD21       E                     INZ('0')
011400120305     D  ERRD21       E                     INZ('0')
011500120305     D* CMTD21       E                     INZ('0')
011600120305     D*-------------
011700120305     D* campo di salvataggio per la DS d'ingresso durante la chiamata
011800120305     D* alla funzione personalizzata
011900120305     D $SaveDS         S                   like(SDGSD20DS)
012000120305     D*-------------
012100120305     D DSFMT           DS           512
012200120305     D  $TASTO               369    369
012300120305     D  NRG                  370    370
012400120305     D  NCL                  371    371
012500120305     D  SFLNRR               378    379B 0
012600120305     D*-------------
012700120305     D* posizione cursore
012800120305     D CURSOR          DS
012900120305     D  RIRI                   1      2B 0 INZ
013000120305     D  R$$                    2      2
013100120305     D  COCO                   3      4B 0 INZ
013200120305     D  C$$                    4      4
013300120305     D*-------------
013400120305     D* Reperimento nome PGM
013500120305     D STATUS         SDS           333
013600120305     D  DSPGM            *PROC
013700120305     D  PARMS            *PARMS
013800120305     D*-------------
013900120305     D* Costanti
014000120305     D* nr.di righe del sfl (SFLPAG)
014100120306     D SFLPAG          C                   CONST(11)
014200120305     D*-------------
014300120305     D* Tasti di funzione
014400120305     D F01             C                   CONST(X'31')
014500120305     D F02             C                   CONST(X'32')
014600120305     D F03             C                   CONST(X'33')
014700120305     D F04             C                   CONST(X'34')
014800120305     D F05             C                   CONST(X'35')
014900120305     D F06             C                   CONST(X'36')
015000120305     D F07             C                   CONST(X'37')
015100120305     D F08             C                   CONST(X'38')
015200120305     D F09             C                   CONST(X'39')
015300120305     D F10             C                   CONST(X'3A')
015400120305     D F11             C                   CONST(X'3B')
015500120305     D F12             C                   CONST(X'3C')
015600120305     D F13             C                   CONST(X'B1')
015700120305     D F14             C                   CONST(X'B2')
015800120305     D F15             C                   CONST(X'B3')
015900120305     D F16             C                   CONST(X'B4')
016000120305     D F17             C                   CONST(X'B5')
016100120305     D F18             C                   CONST(X'B6')
016200120305     D F19             C                   CONST(X'B7')
016300120305     D F20             C                   CONST(X'B8')
016400120305     D F21             C                   CONST(X'B9')
016500120305     D F22             C                   CONST(X'BA')
016600120305     D F23             C                   CONST(X'BB')
016700120305     D F24             C                   CONST(X'BC')
016800120305     D ENTER           C                   CONST(X'F1')
016900120305     D ROLDWN          C                   CONST(X'F4')
017000120305     D ROLLUP          C                   CONST(X'F5')
017100120305     D*-------------
017200120305     D* Sottotitolo di testata
017300120305     D SOTTITT1        C                   CONST('SDGMSG    *LIBL-
017400120305     D                                          SDC0070')
017500120305     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
017600120305     D                                          SDC0001')
017700120305     D*-------------
017800120305     D* Variabili per gestione videate
017900120305     D $Gest           S              2A
018000120305     D $Fine           S              1A
018100120305     D $InzD1          S              1A
018200120305     D $InzS1          S              1A
018300120305     D $InzD2          S              1A
018400120305     D $LastG          S              2A
018500120305     D $LastV          S              2A
018600120305     D $EndFile        S              1A
018700120305     D $Esci           S              1A
018800120305     D $RcdOK          S              1A
018900120305     D $Unique         S              1A
019000120305     D*-------------
019100120305     D* Variabili appoggio sempre presenti in tutte le anagrafiche
019200120305     D S1Nrr           S                   like(C1Rcd)
019300120305     D WSfl            S                   like(C1Nrr)
019400120305     D WMax            S                   like(C1Rcd)
019500120305     D WPag            S                   like(C1Rcd)
019600120305     D WInzS1          S                   like($InzS1)
019700120305     D WOpr            S                   like(OprD20)
019800120305     D CurR            S              2S 0
019900120305     D CurC            S              2S 0
020000120305     D Win             S             99A
020100120305     D WINKR           S              1A
020200120305     D WExf            S              1A
020300120305     D*-------------
020400120305     D*  Indici
020500120305     D X               S              3S 0
020600120305     D Y               S              3S 0
020700120305     D* Indice della schiera per le opzioni personalizzate
020800120305     D PosOpz          S              2  0
020900120305     D* numero max opzioni
021000120305     D NumOpz          S              2  0 Inz($DIMOP)
021100120305     D* numero effettive opzioni
021200120305     D OpzLen          S              2  0
021300120305     D*----------------
021400120305     D* campi di lavoro
021500120305     D WDtIni          S               D
021600120305     D WDtFin          S               D
021700120305     C*----------------------------------------------------*
021800120305     C*                MAIN LINE PROGRAM
021900120305     C*----------------------------------------------------*
022000120305     C*
022100120305     C* inizializzazione variabili
022200120305     C                   EXSR      INZVAR
022300120305     C*
022400120305     C     $FINE         DOWEQ     *OFF
022500120305     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
022600120305     C     $GEST         CASEQ     'S1'          GESS1                          Lista
022700120305     C*    $GEST         CASEQ     'D2'          GESD2                          Valori ass.
022800120305     C                   ENDCS
022900120305     C                   ENDDO
023000120305     C* fine programma
023100120305     C                   EXSR      ENDPGM
023200120305     C*
023300120305     C/EJECT
023400120305     C************************************************************
023500120305     C* FINE PROGRAMMA
023600120305     C************************************************************
023700120305     C     ENDPGM        BEGSR
023800120305     C*
023900120305     C* passaggio dati al pgm chiamante
024000120305     C                   MOVEL     WOPR          OPRD20                                     ne S1
024100120305     C                   MOVEL     SDGSD20DS     KPJBU
024200120305     C*
024300120305     C* Se pgm Guida non è stato richiamato posso chiudere in LR
024400120305     C     OPZD20        IFEQ      *BLANK
024500120305     C                   SETON                                            LR
024600120305     C* altrimenti chiudo in RT
024700120305     C                   ELSE
024800120305     C                   SETON                                            RT
024900120305     C                   ENDIF
025000120305     C                   RETURN
025100120305     C*
025200120305     C                   ENDSR
025300120305     C/EJECT
025400120305     C************************************************************
025500120305     C* RICERCHE
025600120305     C************************************************************
025700120305     C     SEARCH        BEGSR
025800120305     C*
025900120305     C* determino Riga/Colonna del cursore
026000120305     C                   MOVE      NRG           R$$
026100120305     C                   MOVE      NCL           C$$
026200120305     C                   Z-ADD     RIRI          CURR
026300120305     C                   Z-ADD     COCO          CURC
026400120305     C*
026500120305   B1C                   SELECT
026600120305     C*
026700120305     C* ricerca non abilitata in questa posizione
026800120305   X1C                   OTHER
026900120305     C                   SETON                                        9899
027000120305     C*
027100120305   E1C                   ENDSL
027200120305     C*
027300120305     C* imposto pos. del cursore
027400120305     C                   Z-ADD     CURR          H1RIGA
027500120305     C                   Z-ADD     CURC          H1COLO
027600120305     C*
027700120305     C                   MOVE      *HIVAL        $LASTV
027800120305     C*
027900120305     C                   ENDSR
028000120305     C/EJECT
028100120305     C************************************************************
028200120305     C* RIEMPIMENTO OPZIONI
028300120305     C************************************************************
028400120305     C     RIEOPZ        BEGSR
028500120305     C*
028600120305     C* Input :
028700120305     C* - $TX = schiera con tutti i testi disponibili
028800120305     C* - $FL = schiere con i flag di validità associati ai testi
028900120305     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
029000120305     C*          opzione usato
029100120305     C* - WMXO = variabile contenente il numero massimo di testi
029200120305     C*          opzione usabili
029300120305     C* - $L01 = Lunghezza prima riga
029400120305     C* - $L02 = Lunghezza seconda riga
029500120305     C* Output :
029600120305     C* - WOPZ1 = variabile contenente la prima riga opzioni
029700120305     C* - WOPZ2 = variabile contenente la seconda riga opzioni
029800120305     C* - FF23  = flag che indica se c'è bisogno del tasto F23
029900120305     C*
030000120305     C                   CALLB     'X04OPMSG'
030100120305     C* input
030200120305     C                   PARM                    $TX
030300120305     C                   PARM                    $FL
030400120305     C                   PARM                    WCOZ              3 0
030500120305     C                   PARM                    WMXO              3 0
030600120305     C* output
030700120305     C                   PARM                    WOPZ1            76
030800120305     C                   PARM                    WOPZ2            76
030900120305     C                   PARM                    FF23              1
031000120305     C* input
031100120305     C                   PARM                    $L01              3 0
031200120305     C                   PARM                    $L02              3 0
031300120305     C*
031400120305     C                   PARM                    $ALLOPT         960
031500120305     C*
031600120305     C                   ENDSR
031700120305     C/EJECT
031800120305     C************************************************************
031900120305     C* RIEMPIMENTO TASTI FUNZIONE
032000120305     C************************************************************
032100120305     C     RIEKEY        BEGSR
032200120305     C*
032300120305     C* Input :
032400120305     C* - $FX = schiera con tutti i testi disponibili
032500120305     C* - $FC = schiere con i flag di validità associati ai testi
032600120305     C* - $FR = schiere con i flag di REVERSE IMAGE
032700120305     C* - $ULK = ultimo testo caricato
032800120305     C* - $L01 = Lunghezza prima riga
032900120305     C* - $L02 = Lunghezza seconda riga
033000120305     C* Output :
033100120305     C* - $ULK
033200120305     C* - $KEY1= 1° riga di tasti funzione
033300120305     C* - $KEY2= 2° riga di tasti funzione
033400120305     C*
033500120305     C                   CALLB     'XKEYMSG'
033600120305     C* input
033700120305     C                   PARM                    $FX
033800120305     C                   PARM                    $FC
033900120305     C                   PARM                    $FR
034000120305     C                   PARM                    $ULK              3 0
034100120305     C* output
034200120305     C                   PARM                    $KEY1            79
034300120305     C                   PARM                    $KEY2            79
034400120305     C* input
034500120305     C                   PARM                    $L01              3 0
034600120305     C                   PARM                    $L02              3 0
034700120305     C*
034800120305     C                   PARM                    $ALLFUNCT       480
034900120305     C*
035000120305     C                   ENDSR
035100120305     C/EJECT
035200120305     C************************************************************
035300120305     C* GESTIONE EMISSIONE TASTI FUNZIONE
035400120305     C************************************************************
035500120305     C     GESPIE        BEGSR
035600120305     C*
035700120305     C                   SELECT
035800120305     C* Parzializzazioni
035900120305     C     $GEST         WHENEQ    'D1'
036000120305     C     $ULKD1        IFEQ      0
036100120305     C* riempimento opzioni
036200120305     C                   EXSR      F24D1
036300120305     C                   ELSE
036400120305     C                   WRITE     FMTZ1
036500120305     C                   ENDIF
036600120305     C* Lista
036700120305     C     $GEST         WHENEQ    'S1'
036800120305     C     $ULKS1        IFEQ      0
036900120305     C                   EXSR      F24S1
037000120305     C                   ELSE
037100120305     C                   WRITE     FMTZ2
037200120305     C                   ENDIF
037300120305     C* Modifica valori assunti
037400120305     C*    $GEST         WHENEQ    'D2'
037500120305     C*    $ULKD2        IFEQ      0
037600120305     C*                  EXSR      F24D2
037700120305     C*                  ELSE
037800120305     C*                  WRITE     FMTZ3
037900120305     C*                  ENDIF
038000120305     C                   ENDSL
038100120305     C*
038200120305     C                   ENDSR
038300120305     C/EJECT
038400120305     C************************************************************
038500120305     C* GESTIONE PARZIALIZZAZIONI
038600120305     C************************************************************
038700120305     C     GESD1         BEGSR
038800120305     C*
038900120305     C* inizializzazione videata
039000120305     C     $INZD1        IFEQ      *ON
039100120305     C                   EXSR      INZD1
039200120305     C                   MOVE      *OFF          $INZD1
039300120305     C                   ENDIF
039400120305     C*
039500120305     C* pulitura sottotitolo
039600120305     C                   MOVEL     *BLANK        T1TIT
039700120305     C*
039800120305     C* riempimento sottotitolo
039900120305     C                   if        SOTTITT1   <> *Blank
040000120305     C                   MOVEL     SOTTITT1      IDMSG
040100120305     C                   CALLB     'XRTVM'
040200120305     C                   PARM                    IDMSG            27
040300120305     C                   PARM                    TXTMSG          100
040400120305     C                   PARM                    ERRMSG            1
040500120305     C                   PARM      *ON           CTRMSG            1            centratura
040600120305     C                   PARM      30            LENMSG            3 0          lun output.
040700120305     C     ERRMSG        IFNE      *ON
040800120305     C                   MOVEL     TXTMSG        T1TIT
040900120305     C                   ELSE
041000120305     C                   MOVEL     *ALL'?'       T1TIT
041100120305     C                   ENDIF
041200120305     C*
041300120305     C                   ENDIF
041400120305     C*
041500120305     C* emissione piede videata se proveniente da altra
041600120305     C* salvataggio valore formato di provenienza
041700120305     C     $LASTV        IFNE      'D1'
041800120305     C*
041900120305     C* recupero il sottotitolo (se c'è)
042000120305     C                   Clear                   D1TIT
042100120305     C     SOTTITD1      IFNE      *BLANK
042200120305     C                   MOVEL     SOTTITD1      IDMSG
042300120305     C                   CALLB     'XRTVM'
042400120305     C                   PARM                    IDMSG            27
042500120305     C                   PARM                    TXTMSG          100
042600120305     C                   PARM                    ERRMSG            1
042700120305     C                   PARM      *Off          CTRMSG            1            centratura
042800120305     C                   PARM      15            LENMSG            3 0          lun output.
042900120305     C     ERRMSG        IFNE      *ON
043000120305     C                   MOVEL     TXTMSG        D1TIT
043100120305     C                   ELSE
043200120305     C                   MOVEL     *ALL'?'       D1TIT
043300120305     C                   ENDIF
043400120305     C                   ENDIF
043500120305     C*
043600120305     C* salvo stato F17 per sapere cosa fare in caso di F12
043700120305     C                   MOVE      *INKR         WINKR
043800120305     C                   WRITE     FMTT1
043900120305     C                   EXSR      GESPIE
044000120305     C                   MOVE      $LASTV        $LASTG
044100120305     C                   MOVE      'D1'          $LASTV
044200120305     C                   ENDIF
044300120305     C*
044400120305     C* se esistono errori sulla videata
044500120305     C* emetto la write del formato a indicatori spenti per vedere
044600120305     C* le eventuali decodifiche
044700120305     C     *IN99         IFEQ      *ON
044800120305     C                   MOVEA     *IN           WIN
044900120305     C                   MOVE      *ALL'0'       IN5098           49
045000120305     C                   MOVEA     IN5098        *IN(50)
045100120305     C                   WRITE     FMTD1
045200120305     C                   MOVEA     WIN           *IN
045300120305     C                   ENDIF
045400120305     C*
045500120305     C                   EXFMT     FMTD1
045600120305     C*
045700120305     C* azzero la posizione del cursore
045800120305     C                   Z-ADD     0             H1RIGA
045900120305     C                   Z-ADD     0             H1COLO
046000120305     C*
046100120305     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
046200120305     C* INPUT
046300120305     C* $FC1 = Abilitazioni videata D1
046400120305     C* OUTPUT
046500120305     C* *IN97 + *IN99 Errori
046600120305     C                   MOVEA     $FC1          $FC
046700120305     C                   EXSR      CTRKEY
046800120305     C*
046900120305     C     *IN99         IFEQ      *OFF
047000120305     C*
047100120305     C                   SELECT
047200120305     C* F2=Decodifica
047300120305     C     $TASTO        WHENEQ    F02
047400120305     C                   EXSR      F02D1
047500120305     C* F3=Fine
047600120305     C     $TASTO        WHENEQ    F03
047700120305     C                   EXSR      F03D1
047800120305     C* F4=Ricerca
047900120305     C     $TASTO        WHENEQ    F04
048000120305     C                   EXSR      SEARCH
048100120305     C* F5=Refresh
048200120305     C*    $TASTO        WHENEQ    F05
048300120305     C*                  EXSR      F05D1
048400120305     C* F8=Successivo
048500120305     C*    $TASTO        WHENEQ    F08
048600120305     C*                  EXSR      F08D1
048700120305     C* F10=Immissione
048800120305     C     $TASTO        WHENEQ    F10
048900120305     C                   EXSR      F10D1
049000120305     C* F12=Ritorno
049100120305     C     $TASTO        WHENEQ    F12
049200120305     C                   EXSR      F12D1
049300120305     C* F18=Modifica valori assunti
049400120305     C*    $TASTO        WHENEQ    F18
049500120305     C*                  EXSR      F18D1
049600120305     C* F24=Altri tasti
049700120305     C     $TASTO        WHENEQ    F24
049800120305     C                   EXSR      F24D1
049900120305     C*
050000120305     C                   OTHER
050100120305     C                   EXSR      CTRD1
050200120305     C     *IN99         IFEQ      *OFF
050300120305     C* definisco l'ordinamento
050400120305     C                   EXSR      DEFORD
050500120305     C* richiamo la lista
050600120305     C                   MOVE      'S1'          $GEST
050700120305     C                   MOVE      *ON           $INZS1
050800120305     C*                  MOVE      *BLANK        W1POS
050900120305     C                   ENDIF
051000120305     C*
051100120305     C                   ENDSL
051200120305     C*
051300120305     C                   ENDIF
051400120305     C*
051500120305     C                   ENDSR
051600120305     C/EJECT
051700120305     C************************************************************
051800120305     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
051900120305     C************************************************************
052000120305     C     INZD1         BEGSR
052100120305     C*
052200120305     C                   CLEAR                   FMTD1
052300120305     C*
052400120305     C* Se pgm  richiamato con parametri forniti , li imposto nelle
052500120305     C*    parzializzazioni
052600120305     C                   SELECT
052700120305     C* il pgm è richiamato e
052800120305     C* sono stati passati dei valori di parzializzazione ma non
052900120305     C* la chiave univoca
053000120305     C     OPZD20        WHENNE    *BLANK
053100120305     C* imposto i valori passati sui campi a video
053200120305     C                   if        DtIniD20   <> *Loval
053300120305     C     *Jobrun       MOVE      DtIniD20      D1DtIni
053400120305     C                   endif
053500120305     C                   if        DtfinD20   <> *Loval   and
053600120305     C                             DtfinD20   <> *Hival
053700120305     C     *Jobrun       MOVE      DtFinD20      D1DtFin
053800120305     C                   endif
053900120305     C                   ENDSL
054000120305     C*
054100120305     C                   ENDSR
054200120305     C/EJECT
054300120305     C************************************************************
054400120305     C* GESTIONE F02 VIDEO D1
054500120305     C************************************************************
054600120305     C     F02D1         BEGSR
054700120305     C*
054800120305     C* per decodificare la videata richiamo la routine dei ctrl
054900120305     C* e poi disattivo gli eventuali messaggi di errore
055000120305     C                   EXSR      CTRD1
055100120305     C*
055200120305     C                   MOVE      *ALL'0'       IN5098
055300120305     C                   MOVEA     IN5098        *IN(50)
055400120305     C*
055500120305     C                   ENDSR
055600120305     C/EJECT
055700120305     C************************************************************
055800120305     C* GESTIONE F03 VIDEO D1
055900120305     C************************************************************
056000120305     C     F03D1         BEGSR
056100120305     C*
056200120305     C                   MOVE      *ON           $FINE
056300120305     C                   MOVE      '1'           RETD20
056400120305     C* fine programma
056500120305     C                   EXSR      ENDPGM
056600120305     C*
056700120305     C                   ENDSR
056800120305     C/EJECT
056900120305     C************************************************************
057000120305     C* GESTIONE F05 VIDEO D1
057100120305     C************************************************************
057200120305     C*    F05D1         BEGSR
057300120305     C*
057400120305     C*                  MOVE      *ON           $INZD1
057500120305     C*
057600120305     C*                  ENDSR
057700120305     C/EJECT
057800120305     C************************************************************
057900120305     C* GESTIONE F08 VIDEO D1
058000120305     C************************************************************
058100120305     C     F08D1         BEGSR
058200120305     C*
058300120305     C                   MOVE      *ON           $FINE
058400120305     C                   MOVE      '0'           RETD20
058500120305     C*
058600120305     C                   ENDSR
058700120305     C/EJECT
058800120305     C************************************************************
058900120305     C* GESTIONE F10 VIDEO D1
059000120305     C************************************************************
059100120305     C     F10D1         BEGSR
059200120305     C*
059300120305     C                   RESET                   SDGSD21DS
059400120305     C*
059500120305     C                   MOVE      xscsoc        SocietaD21
059600120305     C                   MOVEL     '01'          OPZD21
059700120305     C* passare anche le parzializzazioni ricevute relative alla chiave
059800120305     C* univoca :
059900120305     C*  se ce ne sono, potranno essere usate come dft d'immissione
060000120305     C                   if        OPZD20     <> *Blank
060100120305     C                   Eval      DtIniD21    = DtIniD20
060200120305     C                   Eval      DtFinD21    = DtFinD20
060300120305     C                   else
060400120305     C                   if        D1DtIni    <> 0
060500120305     C     *JobRun       Move      D1DtIni       DtIniD21
060600120305     C                   endif
060700120305     C                   if        D1DtFin    <> 0
060800120305     C     *JobRun       Move      D1DtFin       DtFinD21
060900120305     C                   endif
061000120305     C                   ENDIF
061100120305     C*
061200120305     C                   MOVEL     SDGSD21DS     KPJBU
061300120305     C                   CALL      'SDGSD21R'
061400120305     C                   PARM                    KPJBA
061500120305     C                   MOVEL     KPJBU         SDGSD21DS
061600120305     C* ritorno da PGM gestione
061700120305     C                   EXSR      RTND21
061800120305     C*
061900120305     C                   MOVE      *HIVAL        $LASTV
062000120305     C*
062100120305     C                   ENDSR
062200120305     C/EJECT
062300120305     C************************************************************
062400120305     C* GESTIONE F12 VIDEO D1
062500120305     C************************************************************
062600120305     C     F12D1         BEGSR
062700120305     C*
062800120305     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
062900120305     C*  per F12 torno alla lista
063000120305     C*    $LOP          IFEQ      2
063100120305     C*    WINKR         OREQ      *ON
063200120305     C*                  MOVE      'S1'          $GEST
063300120305     C*  altrimenti, per F12 torno a menu
063400120305     C*                  ELSE
063500120305     C                   MOVE      *ON           $FINE
063600120305     C                   MOVE      '2'           RETD20
063700120305     C*                  ENDIF
063800120305     C*
063900120305     C                   ENDSR
064000120305     C/EJECT
064100120305     C************************************************************
064200120305     C* GESTIONE F18 VIDEO D1
064300120305     C************************************************************
064400120305     C*    F18D1         BEGSR
064500120305     C*
064600120305     C*                  MOVE      'D2'          $GEST
064700120305     C*                  MOVEL     *ON           $INZD2
064800120305     C*
064900120305     C*                  ENDSR
065000120305     C/EJECT
065100120305     C************************************************************
065200120305     C* GESTIONE F24 VIDEO D1
065300120305     C* input
065400120305     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
065500120305     C* $FX1 =Testi tasti funzione
065600120305     C* $FC1 = Abilitazioni
065700120305     C* $FR1 = Inversioni di fondo
065800120305     C************************************************************
065900120305     C     F24D1         BEGSR
066000120305     C*
066100120305     C                   MOVEA     $FX1          $FX
066200120305     C                   MOVEA     $FC1          $FC
066300120305     C                   MOVEA     $FR1          $FR
066400120305     C                   Z-ADD     $ULKD1        $ULK
066500120305     C                   Z-ADD     $D1L01        $L01
066600120305     C                   Z-ADD     $D1L02        $L02
066700120305     C                   EXSR      RIEKEY
066800120305     C                   Z-ADD     $ULK          $ULKD1            3 0
066900120305     C                   MOVEA     $FC           $FC1
067000120305     C                   MOVEL     $KEY1         Z1KE1
067100120305     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
067200120305     C                   MOVEL     $KEY2         Z1KE2
067300120305     C                   WRITE     FMTZ1
067400120305     C*
067500120305     C                   ENDSR
067600120305     C/EJECT
067700120305     C************************************************************
067800120305     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
067900120305     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
068000120305     C* 93 94 96 97 98
068100120305     C************************************************************
068200120305     C     CTRD1         BEGSR
068300120305     C*
068400120305     C                   SETOFF                                       99
068500120306     C*
068600120306     C                   Clear                   WDtIni
068700120306     C                   if        D1DtIni    <> 0
068800120306     C     *JobRun       Move      D1DtIni       WDtIni
068900120306     C                   endif
069000120306     C*
069100120306     C                   Clear                   WDtFin
069200120306     C                   if        D1DtFin    <> 0
069300120306     C     *JobRun       Move      D1DtFin       WDtFin
069400120306     C                   endif
069500120305     C*
069600120305     C                   ENDSR
069700120305     C/EJECT
069800120305     C************************************************************
069900120305     C* DEFINIZIONE DELL'ORDINAMENTO
070000120305     C************************************************************
070100120305     C     DEFORD        BEGSR
070200120305     C*
070300120305     C* definisco l'ordinamento della lista a seconda dei valori
070400120305     C* impostati nel video di parzializzazione
070500120305     C*
070600120305     C                   Z-ADD     1             $ORD
070700120305     C*
070800120305     C                   ENDSR
070900120305     C/EJECT
071000120305     C************************************************************
071100120305     C* Chiamata programma con tasto personalizzato
071200120305     C************************************************************
071300120305     C     FPerS1        BEGSR
071400120305     C*
071500120305     C* salvo la DS di ingresso
071600120305     C                   MOVEL     SDGSD20DS     $SaveDS
071700120305     C*
071800120305     C                   MOVEL     SDGSD20DS     KPJBU
071900120305     C                   CALL      $PGF2(PosFun)                        22
072000120305     C                   PARM                    KPJBA
072100120305     C                   PARM                    NOMPGM           10
072200120305     C*
072300120305     C* ripristino la DS di ingresso
072400120305     C                   MOVEL     $SaveDS       SDGSD20DS
072500120305     C*
072600120305     C                   ENDSR
072700120305     C/EJECT
072800120305     C************************************************************
072900120305     C* GESTIONE LISTA
073000120305     C************************************************************
073100120305     C     GESS1         BEGSR
073200120305     C*
073300120305     C* inizializzazione videata
073400120305     C     $INZS1        IFEQ      *ON
073500120305     C                   EXSR      INZS1
073600120305     C                   MOVE      *OFF          $INZS1
073700120305     C                   ENDIF
073800120305     C*
073900120305     C* emissione piede videata se proveniente da altra
074000120305     C* salvataggio valore formato di provenienza
074100120305     C     $LASTV        IFNE      'S1'
074200120305     C                   WRITE     FMTT1
074300120305     C                   EXSR      GESPIE
074400120305     C                   MOVE      $LASTV        $LASTG
074500120305     C                   MOVE      'S1'          $LASTV
074600120305     C                   ENDIF
074700120305     C*
074800120305     C     WMAX          IFEQ      0
074900120305     C                   WRITE     FMTD3                                        NO RCD
075000120305     C                   ENDIF
075100120305     C* Inizializzazione indice tasti funzione personalizzati
075200120305     C                   Eval      PosFun = 25
075300120305     C* salvo la condizionedice tasti funzione personalizzati
075400120305     C                   Eval      *In09       = C1Mode
075500120305     C*
075600120305     C                   EXFMT     FMTC1
075700120305     C*
075800120305     C* azzero la posizione del cursore
075900120305     C                   Z-ADD     0             H1RIGA
076000120305     C                   Z-ADD     0             H1COLO
076100120305     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
076200120305     C* di refresh dello sfondo
076300120305     C                   MOVE      '1'           WExf
076400120305     C*
076500120305     C     C1NRR         IFNE      0
076600120305     C                   Z-ADD     C1NRR         WSFL
076700120305     C                   ENDIF
076800120305     C                   IF        SFLNRR > 0
076900120305     C                   Z-ADD     SFLNRR        C1RCD
077000120305     C                   ENDIF
077100120305     C* inizializzazione indice opzioni personalizzate
077200120305     C                   Eval      PosOpz = 0
077300120305     C* CONTROLLO ABILITAZIONE TASTI
077400120305     C* INPUT
077500120305     C* $FC2 = Abilitazioni videata S1
077600120305     C* OUTPUT
077700120305     C* *IN97 + *IN99 Errori
077800120305     C                   MOVEA     $FC2          $FC
077900120305     C                   EXSR      CTRKEY
078000120305     C*
078100120305     C     *IN99         IFEQ      *OFF
078200120305     C*
078300120305     C                   SELECT
078400120305     C* F3=Fine
078500120305     C     $TASTO        WHENEQ    F03
078600120305     C                   EXSR      F03S1
078700120305     C* F8=Successivo
078800120305     C*    $TASTO        WHENEQ    F08
078900120305     C*                  EXSR      F08S1
079000120305     C* F10=Immissione
079100120305     C     $TASTO        WHENEQ    F10
079200120305     C                   EXSR      F10S1
079300120305     C* F12=Ritorno
079400120305     C     $TASTO        WHENEQ    F12
079500120305     C                   EXSR      F12S1
079600120305     C* F17=Parzializzazioni
079700120305     C     $TASTO        WHENEQ    F17
079800120305     C                   EXSR      F17S1
079900120305     C* F18=Modifica valori assunti
080000120305     C*    $TASTO        WHENEQ    F18
080100120305     C*                  EXSR      F18S1
080200120305     C* F23=Altre opzioni
080300120305     C     $TASTO        WHENEQ    F23
080400120305     C                   EXSR      F23S1
080500120305     C* F24=Altri tasti
080600120305     C     $TASTO        WHENEQ    F24
080700120305     C                   EXSR      F24S1
080800120305     C* ROLL-UP
080900120305     C     $TASTO        WHENEQ    ROLLUP
081000120305     C                   EXSR      ROLS1
081100120305     C* Personalizzata
081200120305     C     $PGF2(PosFun) Whenne    *BLANK
081300120305     C                   EXSR      FperS1
081400120305     C*
081500120305     C                   OTHER
081600120305     C* CONTROLLO DATI
081700120305     C*                  EXSR      CTRC1
081800120305     C     *IN99         IFEQ      *OFF
081900120305     C                   EXSR      CTRS1
082000120305     C                   ENDIF
082100120305     C*
082200120305     C                   ENDSL
082300120305     C*
082400120305     C                   ENDIF
082500120305     C*
082600120305     C                   ENDSR
082700120305     C/EJECT
082800120305     C************************************************************
082900120305     C* INIZIALIZZAZIONE LISTA
083000120305     C************************************************************
083100120305     C     INZS1         BEGSR
083200120305     C*
083300120305     C* pulizia SFL
083400120305     C                   SETOFF                                         3031
083500120305     C                   WRITE     FMTC1
083600120305     C                   SETON                                          31
083700120305     C*
083800120305     C                   Clear                   D1Tit
083900120305     C*
084000120305     C* riempimento sottotitolo
084100120305     C                   if        SOTTITT1   <> *Blank
084200120305     C                   MOVEL     SOTTITT1      IDMSG
084300120305     C                   CALLB     'XRTVM'
084400120305     C                   PARM                    IDMSG            27
084500120305     C                   PARM                    TXTMSG          100
084600120305     C                   PARM                    ERRMSG            1
084700120305     C                   PARM      *ON           CTRMSG            1            centratura
084800120305     C                   PARM      30            LENMSG            3 0          lun output.
084900120305     C     ERRMSG        IFNE      *ON
085000120305     C                   MOVEL     TXTMSG        T1TIT
085100120305     C                   ELSE
085200120305     C                   MOVEL     *ALL'?'       T1TIT
085300120305     C                   ENDIF
085400120305     C*
085500120305     C                   ENDIF
085600120305     C*
085700120305     C* riempimento opzioni
085800120305     C                   MOVEA     $TX1          $TX
085900120305     C                   MOVEA     $FL1          $FL
086000120305     C                   Z-ADD     0             WCOZ
086100120305     C                   Z-ADD     0             WMXO
086200120305     C*  equivale al num.di elementi della schiera $TX1
086300120305     C                   Z-add     OpzLen        WMXO
086400120305     C                   Z-ADD     $C1Op1        $L01
086500120305     C                   Z-ADD     $C1Op2        $L02
086600120305     C                   EXSR      RIEOPZ
086700120305     C                   Z-ADD     23            X
086800120305     C                   MOVE      FF23          $FC2(X)
086900120305     C                   MOVEL     WOPZ1         C1OPZ1
087000120305     C                   MOVEL     $ALLOPT       H1ALLOPT
087100120305     C                   MOVEL     WOPZ2         C1OPZ2
087200120305     C                   Z-ADD     WCOZ          C1COZ
087300120305     C*
087400120305     C* CARICAMENTO SFL A PAGINE
087500120305     C*
087600120305     C                   Z-ADD     0             S1NRR
087700120305     C                   Z-ADD     0             WMAX
087800120305     C*
087900120305     C* Posizionamento su file pilota
088000120305     C*                  CLEAR                   C1POS
088100120305     C                   EXSR      SETANA
088200120305     C* ripristino i valori del posizionamento
088300120305     C*                  MOVE      W1POS         C1POS
088400120305     C* aggiungere gestione altri campi posizionamento se ci sono
088500120305     C*
088600120305     C* Lettura file pilota
088700120305     C                   EXSR      REDANA
088800120305     C*
088900120305     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
089000120305     C                   IF        $LastV = 'D1'
089100120305     C                   Z-ADD     SFLPAG        WPAG
089200120305     C                   ENDIF
089300120305     C                   EXSR      ROLS1
089400120305     C* imposto il caricamento sfl del numero massimo di righe per pagina
089500120305     C                   Z-ADD     SFLPAG        WPAG
089600120305     C*
089700120305     C                   ENDSR
089800120305     C/EJECT
089900120305     C************************************************************
090000120305     C* CARICAMENTO PAGINA LISTA
090100120305     C************************************************************
090200120305     C     ROLS1         BEGSR
090300120305     C*
090400120305     C                   SETOFF                                       32
090500120305     C                   Z-ADD     0             Y
090600120305     C                   Z-ADD     WMAX          S1NRR
090700120305     C*
090800120305     C* Leggo dal file anagrafico per caricare la lista
090900120305     C*
091000120305     C     $EndFile      DOWEQ     *OFF
091100120305     C     Y             ANDLT     WPAG
091200120305     C*
091300120305     C                   EXSR      RIES1
091400120305     C*
091500120305     C                   ADD       1             S1NRR
091600120305     C                   ADD       1             Y
091700120305     C*
091800120305     C                   WRITE     FMTS1
091900120305     C*
092000120305     C                   EXSR      REDANA
092100120305     C*
092200120305     C                   ENDDO
092300120305     C*
092400120305     C                   Z-ADD     S1NRR         WMAX                 30
092500120305     C*
092600120305     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
092700120305     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
092800120305     C                   MVR                     RESTO             3 0
092900120305     C     PAGINE        MULT      SFLPAG        C1RCD
093000120305     C     RESTO         IFGT      0
093100120305     C                   ADD       1             C1RCD
093200120305     C                   ELSE
093300120305     C                   SUB       SFLPAG        C1RCD
093400120305     C                   ADD       1             C1RCD
093500120305     C                   ENDIF
093600120305     C*
093700120305     C                   ENDSR
093800120305     C/EJECT
093900120305     C************************************************************
094000120305     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
094100120305     C************************************************************
094200120305     C     SETANA        BEGSR
094300120305     C*
094400120305     C                   MOVE      XScSoc        BLSSocieta
094500120305     C*
094600120305     C                   Select
094700120305     C*
094800120305     C* Gestione posizionamento
094900120305     C* Se il campo W1POS è > *blank imposto il posizionamento con
095000120305     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
095100120305     C*
095200120305     C* Ord.1 = chiave ordinamento 1
095300120305     C     $ORD          WHENEQ    1
095400120305     C                   if        D1DtIni    <> 0
095500120305     C     *Jobrun       Move      D1DtIni       BLSDtIni
095600120306     C     K02BLS01      SETLL     SDGBLS01L
095700120306     C                   else
095800120306     C     XScSoc        SETLL     SDGBLS01L
095900120305     C                   endif
096000120305     C*
096100120305     C                   ENDSL
096200120305     C*
096300120305     C                   ENDSR
096400120305     C/EJECT
096500120305     C************************************************************
096600120305     C* LETTURA RCD ARCHIVIO PILOTA
096700120305     C************************************************************
096800120305     C     REDANA        BEGSR
096900120305     C*
097000120305     C                   MOVEL     *OFF          $EndFile
097100120305     C                   MOVEL     *OFF          $RCDOK
097200120305     C*
097300120305     C     $EndFile      DOUEQ     *ON
097400120305     C     $RCDOK        OREQ      *ON
097500120305     C*
097600120305     C                   SELECT
097700120305     C*
097800120305     C     $ORD          WHENEQ    1
097900120305     C     XScSoc        READE     SDGBLS01L                              21
098000120305     C                   ENDSL
098100120305     C*
098200120305     C                   MOVE      *IN21         $EndFile
098300120305     C*
098400120305     C     $EndFile      IFEQ      *OFF
098500120305     C                   EXSR      SELANA
098600120305     C                   ENDIF
098700120305     C*
098800120305     C                   ENDDO
098900120305     C*
099000120305     C                   MOVE      *IN21         *IN33                          SflEnd
099100120305     C*
099200120305     C                   ENDSR
099300120305     C/EJECT
099400120305     C************************************************************
099500120305     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
099600120305     C************************************************************
099700120305     C     CLCPAG        BEGSR
099800120305     C* Input :
099900120305     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
100000120305     C*          cursore
100100120305     C* - SFLPAG = numero rcd per pagina sfl
100200120305     C* Output :
100300120305     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
100400120305     C*
100500120305     C     WSFL          DIV       SFLPAG        PAGINE            4 0
100600120305     C                   MVR                     RESTO             3 0
100700120305     C     RESTO         IFGT      0
100800120305     C                   ADD       1             PAGINE
100900120305     C                   ENDIF
101000120305     C     PAGINE        MULT      SFLPAG        WPAG
101100120305     C*
101200120305     C                   ENDSR
101300120305     C/EJECT
101400120305     C************************************************************
101500120305     C* SELEZIONI SULLA RIGA LETTA
101600120305     C* impostare a *OFF il campo $RCDOK per scartare un record che
101700120305     C* non rientra nei parametri di parzializzazione
101800120305     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
101900120305     C* superiore stabilito nei parametri
102000120305     C*
102100120305     C************************************************************
102200120305     C     SELANA        BEGSR
102300120305     C*
102400120305     C                   MOVEL     *ON           $RCDOK
102500120305     C                   MOVEL     *ALL'9'       ALL9             15
102600120305     C*
102700120305     C                   SELECT
102800120306     C*
102900120306     C                   When      BLSDtIni    < WDtIni
103000120306     C                   MoveL     *Off          $RCDOK
103100120306     C*
103200120306     C                   When      BLSDtFin    > WDtFin    and
103300120306     C                             WDtFin      > *Loval
103400120306     C                   MoveL     *Off          $RCDOK
103500120305     C*
103600120305     C                   ENDSL
103700120305     C*
103800120305     C                   ENDSR
103900120305     C/EJECT
104000120305     C************************************************************
104100120305     C* RIEMPIMENTO RIGA LISTA
104200120305     C************************************************************
104300120305     C     RIES1         BEGSR
104400120305     C*
104500120305     C                   CLEAR                   FMTS1
104600120305     C*
104700120305     C* riempio i campi del sfl, compresi gli hidden con i valori
104800120305     C* del rcd letto
104900120305     C                   if        BLSDtIni   <> *Loval
105000120305     C     *Jobrun       Move      BLSDtIni      S1DtIni
105100120305     C                   else
105200120305     C                   Eval      S1DtIni     = 0
105300120305     C                   endif
105400120305     C*
105500120305     C                   if        BLSDtFin   <> *Loval
105600120305     C     *Jobrun       Move      BLSDtFin      S1DtFin
105700120305     C                   else
105800120305     C                   Eval      S1DtFin     = 0
105900120305     C                   endif
106000120305     C*
106100120305     C                   MoveL     BLSDivisa     S1Divisa
106200120305     C                   Z-ADD     BLSImpSogl    S1ImpSogl
106300120305     C*
106400120305     C* imposto quegli attributi del sfl che sono modificabili
106500120305     C* al richiamo del determinato pgm
106600120305     C                   EXSR      ATRS1
106700120305     C*
106800120305     C                   ENDSR
106900120305     C/EJECT
107000120305     C************************************************************
107100120305     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
107200120305     C************************************************************
107300120305     C     ATRS1         BEGSR
107400120305     C*
107500120305     C* in questa routine bisogna attivare indicatori o campi programma
107600120305     C* che gestiscono gli attributi di uno o più campi di questa videata;
107700120305     C* attenzione : non è questo il posto in cui eseguire controlli,
107800120305     C*  dovrebbero già arrivare dei flag
107900120305     C*
108000120305     C                   ENDSR
108100120305     C/EJECT
108200120305     C************************************************************
108300120305     C* GESTIONE F08 VIDEO S1
108400120305     C************************************************************
108500120305     C     F08S1         BEGSR
108600120305     C*
108700120305     C                   MOVE      *ON           $FINE
108800120305     C                   MOVE      '0'           RETD20
108900120305     C*
109000120305     C                   ENDSR
109100120305     C/EJECT
109200120305     C************************************************************
109300120305     C* GESTIONE F03 VIDEO S1
109400120305     C************************************************************
109500120305     C     F03S1         BEGSR
109600120305     C*
109700120305     C                   MOVE      *ON           $FINE
109800120305     C                   MOVE      '1'           RETD20
109900120305     C* fine programma
110000120305     C                   EXSR      ENDPGM
110100120305     C*
110200120305     C                   ENDSR
110300120305     C/EJECT
110400120305     C************************************************************
110500120305     C* GESTIONE F10 VIDEO S1
110600120305     C* AGGIUNTA RECORD
110700120305     C************************************************************
110800120305     C     F10S1         BEGSR
110900120305     C*
111000120305     C                   RESET                   SDGSD21DS
111100120305     C*
111200120305     C                   MOVE      XScSoc        SocietaD21
111300120305     C                   MOVEL     '01'          OPZD21
111400120305     C* passare anche le parzializzazioni ricevute relative alla chiave
111500120305     C* univoca :
111600120305     C*  se ce ne sono, potranno essere usate come dft d'immissione
111700120305     C                   IF        OPZD20 <> *Blank
111800120305     C                   Eval      DtIniD21    = DtIniD20
111900120305     C                   Eval      DtFinD21    = DtFinD20
112000120305     C                   else
112100120305     C                   if        D1DtIni    <> 0
112200120305     C     *JobRun       Move      D1DtIni       DtIniD21
112300120305     C                   endif
112400120305     C                   if        D1DtFin    <> 0
112500120305     C     *JobRun       Move      D1DtFin       DtFinD21
112600120305     C                   endif
112700120305     C                   ENDIF
112800120305     C*
112900120305     C                   MOVEL     SDGSD21DS     KPJBU
113000120305     C                   CALL      'SDGSD21R'
113100120305     C                   PARM                    KPJBA
113200120305     C                   MOVEL     KPJBU         SDGSD21DS
113300120305     C* ritorno da PGM gestione
113400120305     C                   EXSR      RTND21
113500120305     C     WINZS1        IFEQ      *ON
113600120305     C                   MOVE      *ON           $INZS1
113700120305     C* se esistevano già righe sul sfl
113800120305     C* calcolo pagina a cui deve posizionarsi
113900120305     C     WSFL          IFGT      0
114000120305     C                   EXSR      CLCPAG
114100120305     C* altrimenti carico solo la 1a pagina
114200120305     C                   ELSE
114300120305     C                   Z-ADD     SFLPAG        WPAG
114400120305     C                   ENDIF
114500120305     C                   ENDIF
114600120305     C*
114700120305     C                   MOVE      *HIVAL        $LASTV
114800120305     C*
114900120305     C                   ENDSR
115000120305     C/EJECT
115100120305     C************************************************************
115200120305     C* GESTIONE F12 VIDEO S1
115300120305     C************************************************************
115400120305     C     F12S1         BEGSR
115500120305     C*
115600120305     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
115700120305     C     $LOP          IFEQ      1
115800120305     C                   MOVE      'D1'          $GEST
115900120305     C*  se la 1a videata è questa, per F12 torno a menu
116000120305     C                   ELSE
116100120305     C                   MOVE      *ON           $FINE
116200120305     C                   MOVE      '2'           RETD20
116300120305     C                   ENDIF
116400120305     C*
116500120305     C                   ENDSR
116600120305     C/EJECT
116700120305     C************************************************************
116800120305     C* GESTIONE F17 VIDEO S1
116900120305     C************************************************************
117000120305     C     F17S1         BEGSR
117100120305     C*
117200120305     C                   MOVE      'D1'          $GEST
117300120305     C* se il pgm è stato richiamato con chiave univoca
117400120305     C* e dopo è stata richiesta una parzializzazione
117500120305     C* tolgo la gestione della chiave univoca (azzere/abblenco il
117600120305     C* valore; propongo come ordinamento il 1°)
117700120305     C*    $Unique       IFEQ      *ON
117800120305     C*                  MOVE      *OFF          $Unique
117900120305     C*                  MOVE      *BLANK        UQ1001
118000120305     C*                  ENDIF
118100120305     C     $ORD          IFEQ      99
118200120305     C                   Z-ADD     1             $ORD
118300120305     C                   MOVE      *ON           $INZD1
118400120305     C                   ENDIF
118500120305     C*
118600120305     C                   ENDSR
118700120305     C/EJECT
118800120305     C************************************************************
118900120305     C* GESTIONE F18 VIDEO S1
119000120305     C************************************************************
119100120305     C     F18S1         BEGSR
119200120305     C*
119300120305     C                   MOVE      'D2'          $GEST
119400120305     C                   MOVEL     *ON           $INZD2
119500120305     C     $ORD          IFEQ      99
119600120305     C                   Z-ADD     1             $ORD
119700120305     C                   ENDIF
119800120305     C*
119900120305     C                   ENDSR
120000120305     C/EJECT
120100120305     C************************************************************
120200120305     C* GESTIONE F23 VIDEO S1
120300120305     C************************************************************
120400120305     C     F23S1         BEGSR
120500120305     C*
120600120305     C                   Z-ADD     $C1Op1        $L01
120700120305     C                   Z-ADD     $C1Op2        $L02
120800120305     C                   EXSR      RIEOPZ
120900120305     C                   Z-ADD     23            X
121000120305     C                   MOVE      FF23          $FC2(X)
121100120305     C                   MOVEL     WOPZ1         C1OPZ1
121200120305     C                   MOVEL     $ALLOPT       H1ALLOPT
121300120305     C                   MOVEL     WOPZ2         C1OPZ2
121400120305     C                   Z-ADD     WCOZ          C1COZ
121500120305     C*
121600120305     C                   ENDSR
121700120305     C/EJECT
121800120305     C************************************************************
121900120305     C* GESTIONE F24 VIDEO S1
122000120305     C************************************************************
122100120305     C     F24S1         BEGSR
122200120305     C*
122300120305     C                   MOVEA     $FX2          $FX
122400120305     C                   MOVEA     $FC2          $FC
122500120305     C                   MOVEA     $FR2          $FR
122600120305     C                   Z-ADD     $ULKS1        $ULK
122700120305     C                   Z-ADD     $S1L01        $L01
122800120305     C                   Z-ADD     $S1L02        $L02
122900120305     C                   EXSR      RIEKEY
123000120305     C                   Z-ADD     $ULK          $ULKS1            3 0
123100120305     C                   MOVEA     $FC           $FC2
123200120305     C                   MOVEL     $KEY1         Z2KE1
123300120305     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
123400120305     C                   MOVEL     $KEY2         Z2KE2
123500120305     C                   WRITE     FMTZ2
123600120305     C*
123700120305     C                   ENDSR
123800120305     C/EJECT
123900120305     C************************************************************
124000120305     C* CONTROLLO TESTATA LISTA
124100120305     C************************************************************
124200120305     C*    CTRC1         BEGSR
124300120305     C*
124400120305     C*                  MOVE      *OFF          *IN99
124500120305     C*
124600120305     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
124700120305     C* L'INIZIALIZZAZIONE LISTA
124800120305     C*
124900120305     C*    C1POS         IFNE      W1POS
125000120305      * aggiungere in OR altri campi posizionamento se ci sono
125100120305     C*                  EXSR      POSS1
125200120305     C*                  ENDIF
125300120305     C*
125400120305     C*                  ENDSR
125500120305     C/EJECT
125600120305     C************************************************************
125700120305     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
125800120305     C************************************************************
125900120305     C*    POSS1         BEGSR
126000120305     C*
126100120305     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
126200120305     C*
126300120305   B1C*    WMAX          IFGT      0
126400120305     C*                  READC     FMTS1                                  21
126500120305   B2C*    *IN21         DOWEQ     *OFF
126600120305     C*    *IN99         ANDEQ     *OFF
126700120305     C*
126800120305   B3C*    S1OPZ         IFNE      0
126900120305     C*                  SETON                                        92  99
127000120305   E3C*                  ENDIF
127100120305     C* se rilevata opzione attiva, attivo sflnxtchg
127200120305   B3C*    *IN99         IFEQ      *ON
127300120305     C*                  MOVE      *ON           *IN32
127400120305     C*
127500120305     C* imposto quegli attributi del sfl che sono modificabili
127600120305     C* al richiamo del determinato pgm
127700120305     C*                  EXSR      ATRS1
127800120305     C*
127900120305     C*                  UPDATE    FMTS1
128000120305   X3C*                  ELSE
128100120305     C*                  READC     FMTS1                                  21
128200120305   E3C*                  ENDIF
128300120305     C*
128400120305   E2C*                  ENDDO
128500120305     C*
128600120305   E1C*                  ENDIF
128700120305     C*
128800120305     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
128900120305     C* AL VALORE DI POSIZIONAMENTO :
129000120305     C* se questo è maggiore del valore del rispettivo campo di
129100120305     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
129200120305     C* con i valori di parzializzazione.
129300120305     C*
129400120305   B1C*    *IN99         IFEQ      *OFF
129500120305     C*                  MOVE      *BLANK        W1POS
129600120305     C*
129700120305     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
129800120305     C* è maggiore del relativo valore di parzializzazione, nel qual caso
129900120305     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
130000120305     C* richiedo comunque un ricaricamento del sfl che, se non si è
130100120305     C* verificata la condizione suddetta avverrà con posizionamento a
130200120305     C* partire dal valore di parzializzazione
130300120305     C*
130400120305   B2C*                  SELECT
130500120305      * Attenzione : se un ordinamento ha un campo di posizionamento
130600120305      * specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
130700120305      * dei relativi campi
130800120305     C*
130900120305     C*    $ORD          WHENEQ    1
131000120305     C*    C1POS         IFGT      D1Codice
131100120305     C*                  MOVEL     C1POS         W1POS
131200120305      * se uno di questi codici prevede l'uso di XALLINEA
131300120305      * bisogna metterlo qui
131400120305     C*                  CALLB     'XALLINEA'
131500120305     C*                  PARM                    W1Pos
131600120305     C*                  PARM      8             LEN               5 0
131700120305     C*                  PARM                    XSCINP
131800120305     C*                  ENDIF
131900120305     C*
132000120305     C*    $ORD          WHENEQ    2
132100120305     C*    C1POS         IFGT      D1Descr
132200120305     C*                  MOVE      C1POS         W1POS
132300120305     C*                  ENDIF
132400120305     C*
132500120305     C*    $ORD          WHENEQ    3
132600120305     C*    C1POS         IFGT      D1Banca
132700120305     C*                  MOVE      C1POS         W1POS
132800120305     C*                  ENDIF
132900120305     C*
133000120305     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
133100120305     C*
133200120305   E2C*                  ENDSL
133300120305     C*
133400120305     C* richiedo l'inizializzazione sfl
133500120305     C*                  MOVE      *ON           $INZS1
133600120305     C*
133700120305   E1C*                  ENDIF
133800120305     C*
133900120305     C*                  ENDSR
134000120305     C/EJECT
134100120305     C************************************************************
134200120305     C* CONTROLLO OPZIONI LISTA
134300120305     C************************************************************
134400120305     C     CTRS1         BEGSR
134500120305     C*
134600120305     C                   MOVEL     *OFF          $ESCI
134700120305     C                   SETOFF                                       99
134800120305     C                   Z-ADD     0             S1OPZ
134900120305     C*
135000120305     C* Leggo il sfl solo se ci sono rcd
135100120305   B1C     WMAX          IFGT      0
135200120305     C                   READC     FMTS1                                  21
135300120305     C*
135400120305     C* esce se fine sfl o errore che richiede l'uscita
135500120305   B2C     *IN21         DOWEQ     *OFF
135600120305     C     $ESCI         ANDEQ     *OFF
135700120305     C                   Z-ADD     S1NRR         C1RCD
135800120305     C* ctrl su riga
135900120305     C                   EXSR      RECS1
136000120305     C* gestione opzioni
136100120305   B3C     S1OPZ         IFNE      0
136200120305     C     *IN99         ANDEQ     *OFF
136300120305     C                   EXSR      OPZS1
136400120305   E3C                   ENDIF
136500120305     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
136600120305   B3C     *IN99         IFEQ      *ON
136700120305     C                   MOVE      *ON           *IN32
136800120305   E3C                   ENDIF
136900120305     C*
137000120305     C* imposto quegli attributi del sfl che sono modificabili
137100120305     C* al richiamo del determinato pgm
137200120305     C                   EXSR      ATRS1
137300120305     C*
137400120305     C                   Z-ADD     0             S1OPZ
137500120305     C*
137600120305     C                   UPDATE    FMTS1
137700120305     C                   MOVE      *OFF          *IN32
137800120305     C*
137900120305     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
138000120305   B3C     $ESCI         IFEQ      *OFF
138100120305     C                   READC     FMTS1                                  21
138200120305     C* a fine ciclo ripristino il flag richiesta iniz.sfl
138300120305   B4C     *IN21         IFEQ      *ON
138400120305     C                   MOVE      WINZS1        $INZS1
138500120305     C* se è avvenuta almeno una modifica
138600120305   B5C                   IF        $INZS1 = *on
138700120305     C* calcolo pagina a cui deve posizionarsi
138800120305     C     WSFL          IFGT      0
138900120305     C                   EXSR      CLCPAG
139000120305     C                   ELSE
139100120305     C                   Z-ADD     SFLPAG        WPAG
139200120305     C                   ENDIF
139300120305   E5C                   ENDIF
139400120305   E4C                   ENDIF
139500120305   E3C                   ENDIF
139600120305     C*
139700120305   E2C                   ENDDO
139800120305     C*
139900120305   E1C                   ENDIF
140000120305     C*
140100120305     C                   ENDSR
140200120305     C/EJECT
140300120305     C************************************************************
140400120305     C* CONTROLLO CAMPI I/O RIGA LISTA
140500120305     C************************************************************
140600120305     C     RECS1         BEGSR
140700120305     C*
140800120305     C* reset indicatori DSPATR
140900120305     C                   MOVE      *ALL'0'       IN4049           10
141000120305     C                   MOVEA     IN4049        *IN(40)
141100120305     C*
141200120305     C* Controllo se Utente Autorizzato alla scelta
141300120305     C     S1OPZ         IFNE      0
141400120305     C                   Z-ADD     1             X
141500120305     C     S1OPZ         LOOKUP    $OP1(X)                                21
141600120305     C* mi salvo l'indice dell' opzione
141700120305     C   21              Z-add     X             PosOpz            2 0
141800120305     C     *IN21         IFEQ      *OFF
141900120305     C     $FL1(X)       OREQ      'N'
142000120305     C                   MOVE      *ON           $ESCI
142100120305     C                   SETON                                        409199
142200120305     C                   ENDIF
142300120305     C                   ENDIF
142400120305     C*
142500120305     C* introdurre eventuali controlli
142600120305     C*
142700120305     C                   ENDSR
142800120305     C/EJECT
142900120305     C************************************************************
143000120305     C* GESTIONE OPZIONI LISTA
143100120305     C************************************************************
143200120305     C     OPZS1         BEGSR
143300120305     C*
143400120305     C                   Z-ADD     RIRI          CURR
143500120305     C                   Z-ADD     COCO          CURC
143600120305     C*
143700120305     C                   SELECT
143800120305     C*
143900120305     C* se opz=Selezione
144000120305     C* passo la chiave al pgm chiamante
144100120305     C     S1OPZ         WHENEQ    1
144200120305     C                   MOVE      S1OPZ         OPZD20
144300120305     C* passare nei campi appositi della DS SDGSD20DS il valore
144400120305     C* della chiave della VL univoca (se c'è)
144500120305     C*                  MOVE      S1Codice      Codice001
144600120305     C                   MOVE      *ON           $ESCI
144700120305     C                   MOVE      *ON           $FINE
144800120305     C*
144900120305     C                   MOVE      *HIVAL        $LASTV
145000120305     C*
145100120305     C* se selezionata opzione personalizzata, richiamo il programma
145200120305     C* corrispondente
145300120305     C     $PGO1(PosOpz) Whenne    *Blank
145400120305     C                   RESET                   SDGSD21DS
145500120305     C                   MOVE      xscsoc        SocietaD21
145600120305     C                   MOVEL     S1OPZ         OPZD21
145700120305     C* passare nei campi appositi della DS SDGSD21DS il valore
145800120305     C* della chiave della VL univoca
145900120305     C*                  MOVEL     S1Codice      Codice002
146000120305     C*
146100120305     C                   MOVEL     SDGSD21DS     KPJBU
146200120305     C                   CALL      $PGO1(PosOpz)                        22
146300120305     C                   PARM                    KPJBA
146400120305     C                   PARM                    NOMPGM
146500120305     C*
146600120305     C                   MOVEL     KPJBU         SDGSD21DS
146700120305     C* ritorno da PGM
146800120305     C                   EXSR      RTND21
146900120305     C*
147000120305     C* se altre opzioni
147100120305     C* richiamo pgm gestione anagrafica
147200120305     C                   OTHER
147300120305     C*
147400120305     C                   RESET                   SDGSD21DS
147500120305     C*
147600120305     C                   MOVE      xscsoc        SocietaD21
147700120305     C                   MOVEL     S1OPZ         OPZD21
147800120305     C* passare nei campi appositi della DS SDGSD21DS il valore
147900120305     C* della chiave della VL univoca
148000120305     C                   if        S1DtIni    <> *Loval
148100120305     C     *Jobrun       Move      S1DtIni       DtIniD21
148200120305     C                   endif
148300120305     C                   if        S1DtFin    <> *Loval
148400120305     C     *Jobrun       Move      S1DtFin       DtFinD21
148500120305     C                   endif
148600120305     C*
148700120305     C                   MOVEL     SDGSD21DS     KPJBU
148800120305     C                   CALL      'SDGSD21R'
148900120305     C                   PARM                    KPJBA
149000120305     C                   PARM                    AUTDS
149100120305     C*
149200120305     C                   MOVEL     KPJBU         SDGSD21DS
149300120305     C*
149400120305     C* ritorno da PGM gestione
149500120305     C                   EXSR      RTND21
149600120305     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
149700120305     C     *IN99         IFEQ      *ON
149800120305     C                   SETON                                        40
149900120305     C                   ENDIF
150000120305     C*
150100120305     C                   MOVE      *HIVAL        $LASTV
150200120305     C                   ENDSL
150300120305     C*
150400120305     C                   ENDSR
150500120305     C/EJECT
150600120305     C************************************************************
150700120305     C* GESTIONE RITORNO DA PGM DI GESTIONE
150800120305     C************************************************************
150900120305     C     RTND21        BEGSR
151000120305     C*
151100120305     C* modo di ritorno
151200120305     C*
151300120305     C                   SELECT
151400120305     C* questi modi di utilizzo dei valori di ritorno dal
151500120305     C* pgm di manutenzione rcd di anagrafica sono delle
151600120305     C* proposte, normalmente sempre valide, ma modificabili
151700120305     C* per situazioni particolari
151800120305     C* 1 = F3
151900120305     C     RETD21        WHENEQ    '1'
152000120305     C                   MOVE      *ON           $ESCI                          sospese READC
152100120305     C                   MOVE      *ON           $FINE                          fine pgm
152200120305     C                   MOVE      '1'           RETD20
152300120305     C* 2 = F12
152400120305     C     RETD21        WHENEQ    '2'
152500120305     C* se il pgm di manutenzione ha prima una guida, è necessario anche
152600120305     c* questo test, altrimenti prevale il ret002 e il sfl non viene
152700120305     C* ricaricato
152800120305     C     OPRD21        ANDNE     '1'
152900120305     C                   MOVE      *ON           $ESCI                          sospese READC
153000120305     C*
153100120305     C                   ENDSL
153200120305     C*
153300120305     C* operazione eseguite dal pgm chiamato
153400120305     C*
153500120305     C                   SELECT
153600120305     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
153700120305     C     OPRD21        WHENEQ    '1'
153800120305     C                   MOVE      OPRD21        WOPR                           avverto pgm chiamant
153900120305     C*
154000120305     C* se l'opzione con cui è stato chiamato non è la modifica
154100120305     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
154200120305     C     OPZD21        IFNE      '02'
154300120305     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
154400120305     C* se l'opzione con cui è stato chiamato è la modifica
154500120305     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
154600120305     C                   ELSE
154700120305     C                   EXSR      AGGS1
154800120305     C                   ENDIF
154900120305     C*
155000120305     C                   ENDSL
155100120305     C*
155200120305     C* funzione non eseguibile per errore :
155300120305     C*
155400120305     C                   SELECT
155500120305     C* 1 = funzione richiamata chiusa in errore
155600120305     C     ERRD21        WHENEQ    '1'
155700120305     C                   MOVE      *ON           $ESCI                          sospese READC
155800120305     C                   SETON                                        9399      emiss.SFLMSGID
155900120305     C* 2 = funzione richiamata senza la dovuta autorizzazione
156000120305     C     ERRD21        WHENEQ    '2'
156100120305     C                   MOVE      *ON           $ESCI                          sospese READC
156200120305     C                   SETON                                        9499      emiss.SFLMSGID
156300120305      *  eventualmente gestire altri codici di errore
156400120305     C*
156500120305     C                   ENDSL
156600120305     C*
156700120305     C                   ENDSR
156800120305     C/EJECT
156900120305     C************************************************************
157000120305     C* AGGIORNAMENTO RIGA LISTA
157100120305     C************************************************************
157200120305     C     AGGS1         BEGSR
157300120305     C*
157400120305     C* riempio i campi del sfl che sono stati ripassati dal pgm
157500120305     C* di manutenzione dopo una opzione 02
157600120305     C*
157700120305     C                   if        DtIniD21   <> *Loval
157800120305     C     *Jobrun       Move      DtIniD21      S1DtIni
157900120305     C                   endif
158000120305     C                   if        DtFinD21   <> *Loval
158100120305     C     *Jobrun       Move      DtFinD21      S1DtFin
158200120305     C                   endif
158300120305     C                   Eval      S1Divisa    = DivisaD21
158400120305     C                   Z-Add     ImpSoglD21    S1ImpSogl
158500120305     C*
158600120305     C                   ENDSR
158700120305     C/EJECT
158800120305     C************************************************************
158900120305     C* CONTROLLO ABILITAZIONE TASTI
159000120305     C* INPUT
159100120305     C* $TASTO =Tasto funzione premuto
159200120305     C* $FC = Tasti abilitati
159300120305     C* OUTPUT
159400120305     C* IN99 = ERrore
159500120305     C************************************************************
159600120305     C     CTRKEY        BEGSR
159700120305     C*
159800120305     C                   SETOFF                                       99
159900120305     C*
160000120305     C                   SELECT
160100120305     C     $TASTO        WHENEQ    F01
160200120305     C                   z-add     01            posfun
160300120305     C     $FC(1)        IFEQ      '0'
160400120305     C                   SETON                                        9997
160500120305     C                   ENDIF
160600120305     C     $TASTO        WHENEQ    F02
160700120305     C                   z-add     02            posfun
160800120305     C     $FC(2)        IFEQ      '0'
160900120305     C                   SETON                                        9997
161000120305     C                   ENDIF
161100120305     C     $TASTO        WHENEQ    F03
161200120305     C                   z-add     03            posfun
161300120305     C     $FC(3)        IFEQ      '0'
161400120305     C                   SETON                                        9997
161500120305     C                   ENDIF
161600120305     C     $TASTO        WHENEQ    F04
161700120305     C                   z-add     04            posfun
161800120305     C     $FC(4)        IFEQ      '0'
161900120305     C                   SETON                                        9997
162000120305     C                   ENDIF
162100120305     C     $TASTO        WHENEQ    F05
162200120305     C                   z-add     05            posfun
162300120305     C     $FC(5)        IFEQ      '0'
162400120305     C                   SETON                                        9997
162500120305     C                   ENDIF
162600120305     C     $TASTO        WHENEQ    F06
162700120305     C                   z-add     06            posfun
162800120305     C     $FC(6)        IFEQ      '0'
162900120305     C                   SETON                                        9997
163000120305     C                   ENDIF
163100120305     C     $TASTO        WHENEQ    F07
163200120305     C                   z-add     07            posfun
163300120305     C     $FC(7)        IFEQ      '0'
163400120305     C                   SETON                                        9997
163500120305     C                   ENDIF
163600120305     C     $TASTO        WHENEQ    F08
163700120305     C                   z-add     08            posfun
163800120305     C     $FC(8)        IFEQ      '0'
163900120305     C                   SETON                                        9997
164000120305     C                   ENDIF
164100120305     C     $TASTO        WHENEQ    F09
164200120305     C                   z-add     09            posfun
164300120305     C     $FC(09)       IFEQ      '0'
164400120305     C                   SETON                                        9997
164500120305     C                   ENDIF
164600120305     C     $TASTO        WHENEQ    F10
164700120305     C                   z-add     10            posfun
164800120305     C     $FC(10)       IFEQ      '0'
164900120305     C                   SETON                                        9997
165000120305     C                   ENDIF
165100120305     C     $TASTO        WHENEQ    F11
165200120305     C                   z-add     11            posfun
165300120305     C     $FC(11)       IFEQ      '0'
165400120305     C                   SETON                                        9997
165500120305     C                   ENDIF
165600120305     C     $TASTO        WHENEQ    F12
165700120305     C                   z-add     12            posfun
165800120305     C     $FC(12)       IFEQ      '0'
165900120305     C                   SETON                                        9997
166000120305     C                   ENDIF
166100120305     C     $TASTO        WHENEQ    F13
166200120305     C                   z-add     13            posfun
166300120305     C     $FC(13)       IFEQ      '0'
166400120305     C                   SETON                                        9997
166500120305     C                   ENDIF
166600120305     C     $TASTO        WHENEQ    F14
166700120305     C                   z-add     14            posfun
166800120305     C     $FC(14)       IFEQ      '0'
166900120305     C                   SETON                                        9997
167000120305     C                   ENDIF
167100120305     C     $TASTO        WHENEQ    F15
167200120305     C                   z-add     15            posfun
167300120305     C     $FC(15)       IFEQ      '0'
167400120305     C                   SETON                                        9997
167500120305     C                   ENDIF
167600120305     C     $TASTO        WHENEQ    F16
167700120305     C                   z-add     16            posfun
167800120305     C     $FC(16)       IFEQ      '0'
167900120305     C                   SETON                                        9997
168000120305     C                   ENDIF
168100120305     C     $TASTO        WHENEQ    F17
168200120305     C                   z-add     17            posfun
168300120305     C     $FC(17)       IFEQ      '0'
168400120305     C                   SETON                                        9997
168500120305     C                   ENDIF
168600120305     C     $TASTO        WHENEQ    F18
168700120305     C                   z-add     18            posfun
168800120305     C     $FC(18)       IFEQ      '0'
168900120305     C                   SETON                                        9997
169000120305     C                   ENDIF
169100120305     C     $TASTO        WHENEQ    F19
169200120305     C                   z-add     19            posfun
169300120305     C     $FC(19)       IFEQ      '0'
169400120305     C                   SETON                                        9997
169500120305     C                   ENDIF
169600120305     C     $TASTO        WHENEQ    F20
169700120305     C                   z-add     20            posfun
169800120305     C     $FC(20)       IFEQ      '0'
169900120305     C                   SETON                                        9997
170000120305     C                   ENDIF
170100120305     C     $TASTO        WHENEQ    F21
170200120305     C                   z-add     21            posfun
170300120305     C     $FC(21)       IFEQ      '0'
170400120305     C                   SETON                                        9997
170500120305     C                   ENDIF
170600120305     C     $TASTO        WHENEQ    F22
170700120305     C                   z-add     22            posfun
170800120305     C     $FC(22)       IFEQ      '0'
170900120305     C                   SETON                                        9997
171000120305     C                   ENDIF
171100120305     C     $TASTO        WHENEQ    F23
171200120305     C                   z-add     23            posfun
171300120305     C     $FC(23)       IFEQ      '0'
171400120305     C                   SETON                                        9997
171500120305     C                   ENDIF
171600120305     C     $TASTO        WHENEQ    F24
171700120305     C                   z-add     24            posfun
171800120305     C     $FC(24)       IFEQ      '0'
171900120305     C                   SETON                                        9997
172000120305     C                   ENDIF
172100120305     C                   ENDSL
172200120305     C*
172300120305     C                   ENDSR
172400120305     C/EJECT
172500120305     C************************************************************
172600120305     C* OPERAZIONI INIZIALI
172700120305     C************************************************************
172800120305     C     *INZSR        BEGSR
172900120305     C*
173000120305     C* Reperimento parametri
173100120305     C*
173200120305     C     *ENTRY        PLIST
173300120305     C                   PARM                    KPJBA
173400120305     C                   PARM                    DSAUT
173500120305     C*
173600120305     C* testo se sono stati passati i dati di autorizzazione
173700120305     C*
173800120305     C     PARMS         IFLT      2
173900120305     C                   MOVE      *BLANK        AUTDS
174000120305     C                   ELSE
174100120305     C                   MOVEL     DSAUT         AUTDS
174200120305     C                   ENDIF
174300120305     C*
174400120305     C* Reperimento dati società
174500120305     C*
174600120305     C                   MOVEL     'SOC001'      TIPXSC
174700120305     C                   MOVEL     *BLANK        SOCXSC
174800120305     C                   EXSR      REPSOC
174900120305     C*
175000120305     C* Controlli dati società
175100120305     C     RTNXSC        IFNE      '1'
175200120305     C                   MOVEL     XSOCDS        SOC001
175300120305     C                   ELSE
175400120305     C                   MOVE      *ON           $FINE
175500120305     C                   MOVE      '1'           ErrD20
175600120305     C                   EXSR      ENDPGM
175700120305     C                   ENDIF
175800120305     C*
175900120305     C* reperisco le dimensioni delle variabili che conterranno i tasti
176000120305     C                   EVAL      $D1L01 = %Size(Z1Ke1)
176100120305     C                   EVAL      $D1L02 = %Size(Z1Ke2)
176200120305     C                   EVAL      $S1L01 = %Size(Z2Ke1)
176300120305     C                   EVAL      $S1L02 = %Size(Z2Ke2)
176400120305     C                   EVAL      $C1Op1 = %Size(C1Opz1)
176500120305     C                   EVAL      $C1Op2 = %Size(C1Opz2)
176600120305     C*                  EVAL      $D2L01 = %Size(Z3Ke1)
176700120305     C                   EVAL      $D2L02 = 0
176800120305     C* aggiungere altre variabili nel caso di videate indipendenti
176900120305     C* ulteriori a quelle std
177000120305     C*
177100120305     C* Reperimento opzioni
177200120305     C*
177300120305     C                   EXSR      REPOPZ
177400120305     C*
177500120305     C* Reperimento tasti di funzione
177600120305     C*
177700120305     C                   EXSR      REPKEY
177800120305     C* Chiamata a routine per il reperimento delle opzioni
177900120305     C* personalizzate
178000120305     C* se si vogliono abilitare i tasti personalizzati in ogni videata
178100120305     C* ripeterla passando le schiere dei tasti funzione
178200120305     C                   CAllb     'X46USREXT'
178300120305     C                   PARM                    $OP1
178400120305     C                   PARM                    $TX1
178500120305     C                   PARM                    $FL1
178600120305     C                   PARM                    $PGO1
178700120305     C                   PARM      $dimOp        NumOpz
178800120305     C                   PARM                    $FX2
178900120305     C                   PARM                    $FC2
179000120305     C                   PARM                    $PGF2
179100120305     C                   PARM                    DsPgm
179200120305     C                   PARM                    xsccut
179300120305     C                   PARM                    xscgrp
179400120305     C*
179500120305     C                   Z-add     1             OPZLEN
179600120305     C     *blank        Lookup    $TX1(opzlen)                           21
179700120305     C                   if        *in21 = *on
179800120305     C                   sub       1             OPZLEN
179900120305     C                   else
180000120305     C                   Z-add     $DimOp        OPZLEN
180100120305     C                   endif
180200120305     C*
180300120305     C                   ENDSR
180400120305     C/EJECT
180500120305     C************************************************************
180600120305     C* INIZIALIZZAZIONE VARIABILI
180700120305     C************************************************************
180800120305     C     INZVAR        BEGSR
180900120305     C*
181000120305     C                   MOVEL     KPJBU         SDGSD20DS
181100120305     C*
181200120305     C* Se società diversa da quella impostata nella *inzsr, rieseguo
181300120305     C* reperimento dati società
181400120305     C                   IF        SocietaD20 <> xscsoc  and
181500120305     C                             OpzD20 <>*blank
181600120305     C                   MOVEL     'SOC001'      TIPXSC
181700120305     C                   MOVEL     SocietaD20    SOCXSC
181800120305     C                   EXSR      REPSOC
181900120305     C     RTNXSC        IFNE      '1'
182000120305     C                   MOVEL     XSOCDS        SOC001
182100120305     C                   ELSE
182200120305     C                   MOVE      *ON           $FINE
182300120305     C                   MOVE      '1'           ErrD20
182400120305     C                   EXSR      ENDPGM
182500120305     C                   ENDIF
182600120305     C                   ENDIF
182700120305     C*
182800120305     C* Reperimento autorizzazioni
182900120305     C*
183000120305      *  nel caso l'anagrafica non gestisse le autorizzazioni
183100120305      *   asteriscare la chiamata alla subroutine REPAUT
183200120305     C* le autorizzazioni potrebbero essere già state passate,
183300120305     C* nel qual caso non vanno reperite
183400120305     C     AUTDS         IFEQ      *BLANK
183500120305     C                   EXSR      REPAUT
183600120305     C                   ELSE
183700120305     C                   MOVEL     XCADDS        CONCIVDS
183800120305     C                   ENDIF
183900120305     C*
184000120305     C* Reperimento valori default utente
184100120305     C                   EXSR      REPDFT
184200120305     C*
184300120305     C* Definizione delle opzioni utilizzabili a pgm
184400120305     C                   EXSR      DEFOPZ
184500120305     C*
184600120305     C* Variabili per gestione videate
184700120305     C                   MOVE      *OFF          $FINE                          fine pgm
184800120305     C                   MOVE      *OFF          $INZD1                         inizializzione D1
184900120305     C                   MOVE      *OFF          $INZS1                         inizializzione S1
185000120305     C                   MOVE      *OFF          $INZD2                         inizializzione D2
185100120305     C                   MOVE      *OFF          $EndFile                       memo fine file
185200120305     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
185300120305     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
185400120305     C                   MOVE      *HIVAL        $LASTV
185500120305     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
185600120305     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
185700120305     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
185800120305     C* azzero la posizione del cursore
185900120305     C                   Z-ADD     0             H1RIGA
186000120305     C                   Z-ADD     0             H1COLO
186100120305     C*
186200120305     C* visualizzo il sfl chiuso
186300120305     C                   Eval      C1Mode      = *On
186400120305     C*
186500120305     C                   MOVE      *ON           $INZD1
186600120305     C                   MOVE      *ON           $INZS1
186700120305     C                   z-add     1             $lop
186800120305     C*
186900120305     C* Variabili appoggio
187000120305     C*
187100120305     C                   Z-ADD     SFLPAG        WPAG
187200120305     C                   MOVEL     *OFF          WOPR
187300120305     C*                  CLEAR                   W1POS
187400120305     C*
187500120305     C* definisco qual'è la prima videata
187600120305     C* (se la prima videata risulta la lista, devo comunque caricare
187700120305     C*  la videata di parzializzazione e gestire la definizione
187800120305     C*  dell'ordinamento, perchè, dai default oppure se sono stati
187900120305     C*  passati dei dati, può variare il caricamento)
188000120305     C                   SELECT
188100120305     C     $LOP          WHENEQ    1
188200120305     C                   MOVE      'D1'          $GEST
188300120305     C     $LOP          WHENEQ    2
188400120305     C                   MOVE      'S1'          $GEST
188500120305     C                   EXSR      INZD1
188600120305     C                   MOVE      *OFF          $INZD1
188700120305     C                   EXSR      DEFORD
188800120305     C                   ENDSL
188900120305     C*
189000120305     C* Valorizzazione campi univoci testate
189100120305     C                   CLEAR                   FMTT1
189200120305     C                   MOVEL     KNSIF         NOMSIF
189300120305     C                   MOVEL     XSCDSI        NOMDIT
189400120305     C                   MOVEL     KCDAZ         NOMAZN
189500120305     C                   MOVEL     DSPGM         NOMPGM
189600120305     C*
189700120305     C* recupero il sottotitolo (se c'è)
189800120305     C                   MOVEL     *BLANK        T1TIT
189900120305     C     SOTTITT1      IFNE      *BLANK
190000120305     C                   MOVEL     SOTTITT1      IDMSG
190100120305     C                   CALLB     'XRTVM'
190200120305     C                   PARM                    IDMSG            27
190300120305     C                   PARM                    TXTMSG          100
190400120305     C                   PARM                    ERRMSG            1
190500120305     C                   PARM      *ON           CTRMSG            1            centratura
190600120305     C                   PARM      30            LENMSG            3 0          lun output.
190700120305     C     ERRMSG        IFNE      *ON
190800120305     C                   MOVEL     TXTMSG        T1TIT
190900120305     C                   ELSE
191000120305     C                   MOVEL     *ALL'?'       T1TIT
191100120305     C                   ENDIF
191200120305     C                   ENDIF
191300120305     C*
191400120305     C                   ENDSR
191500120305     C/EJECT
191600120305     C************************************************************
191700120305     C* REPERIMENTO DATI SOCIETA'
191800120305     C************************************************************
191900120305     C     REPSOC        BEGSR
192000120305     C*
192100120305     C                   CALL      'SDGSOCDR'
192200120305     C                   PARM                    TIPXSC            6
192300120305     C                   PARM                    SOCXSC            3
192400120305     C                   PARM                    CDSXSC            9 0
192500120305     C                   PARM                    MODXSC            3
192600120305     C                   PARM      *BLANKS       RTNXSC            1
192700120305     C                   PARM                    XSOCDS
192800120305     C                   PARM                    KPJBA
192900120305     C*
193000120305     C                   ENDSR
193100120305     C/EJECT
193200120305     C************************************************************
193300120305     C* REPERIMENTO AUTORIZZAZIONI UTENTE
193400120305     C************************************************************
193500120305     C     REPAUT        BEGSR
193600120305     C*
193700120305     C                   CLEAR                   AUTDS
193800120305     C*
193900120305     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
194000120305     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
194100120305     C                   Z-ADD     0             XCAGRP                         GRUPPO
194200120305     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
194300120305     C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
194400120305     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
194500120305     C                   MOVE      'CK'          XCATCK                         TIPO CTRL
194600120305     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
194700120305     C*
194800120305     C                   CALLB     'XCHKAUT'
194900120305     C                   PARM                    AUTDS
195000120305     C*
195100120305     C* errori/non abilitazione
195200120305     C     XCARTN        IFNE      0
195300120305     C                   MOVE      '2'           ErrD20
195400120305     C                   EXSR      ENDPGM
195500120305     C                   ELSE
195600120305     C                   MOVEL     XCADDS        CONCIVDS
195700120305     C                   ENDIF
195800120305     C*
195900120305     C                   ENDSR
196000120305     C/EJECT
196100120305     C************************************************************
196200120305     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
196300120305     C************************************************************
196400120305     C     REPDFT        BEGSR
196500120305     C*
196600120305     C* preparo i parametri di chiamata al driver
196700120305     C                   CLEAR                   XMPEDS
196800120305     C                   EVAL      TpChiamata = '01'
196900120305     C                   EVAL      NChiavi = 5
197000120305     C                   EVAL      VLogica = 2
197100120305     C*
197200120305     C* preparo le chiavi per la chiamata al driver
197300120305     C                   CLEAR                   XMPEDS1
197400120305     C                   MOVEL     XSCSOC        DSMPESOC
197500120305     C                   MOVEL     DSPGM         DSMPEPGM
197600120305     C                   MOVEL     KNMUS         DSMPEPRF
197700120305     C* Si reperiscono i dati di default a seconda delle modalità con
197800120305     C* cui il pgm è usato
197900120305     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
198000120305     C* MPETIP = 'PV'  pgm richiamato da menù
198100120305     C* MPETIP = 'PB'  x lancio Batch
198200120305     C                   SELECT
198300120305     C     OPZD20        WHENEQ    '01'
198400120305     C                   MOVEL     'PX'          DSMPETIP
198500120305     C                   OTHER
198600120305     C                   MOVEL     'PV'          DSMPETIP
198700120305     C                   ENDSL
198800120305     C*
198900120305     C                   CALLB     'XMPE'
199000120305     C                   PARM                    XMPEDS
199100120305     C                   PARM                    XMPEDS1
199200120305     C*
199300120305     C     Risultato     IFEQ      '0'
199400120305     C                   MOVEL     DSMPEPAR      DSMPE
199500120305     C* nel caso sia la prima volta che l'utente apre questa anagrafica
199600120305     C* i suoi valori di default vengono decisi a pgm
199700120305     C                   ELSE
199800120305     C                   Z-ADD     1             $LOP
199900120305     C                   Z-ADD     1             $ORD
200000120305      *  elenco altri dati default specifici dell'anagrafica
200100120305     C                   ENDIF
200200120305     C*
200300120305     C                   ENDSR
200400120305     C/EJECT
200500120305     C************************************************************
200600120305     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
200700120305     C************************************************************
200800120305     C     DEFOPZ        BEGSR
200900120305     C*
201000120305     C* Questa routine deve valorizzare la schiera $FL1
201100120305     C* con S o N a seconda della utilizzabilità o meno
201200120305     C* dell'opzione correlata
201300120305     C*
201400120305     C* Inizialmente si considerano tutte le opzioni come valide
201500120305     C     1             DO        $DIMOP        X
201600120305     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
201700120305     C                   If        $TX1(X)  <> *Blank
201800120305     C                   MOVE      'S'           $FL1(X)
201900120305     C                   Else
202000120305     C                   Leave
202100120305     C                   EndIf
202200120305     C                   ENDDO
202300120305     C*
202400120305     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
202500120305     C* con 1 o 0 a seconda della utilizzabilità o meno
202600120305     C* del tasto funzione correlato
202700120305     C*
202800120305     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
202900120305     C* di compilazione
203000120305     C                   RESET                   $FC1
203100120305     C                   RESET                   $FR1
203200120305     C                   RESET                   $FC2
203300120305     C                   RESET                   $FR2
203400120305     C                   RESET                   $FC3
203500120305     C                   RESET                   $FR3
203600120305     C*
203700120305     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
203800120305     C* invalido alcune opzioni
203900120305     C                   SELECT
204000120305     C* Se pgm richiamato in ricerca disattivo :
204100120305     C     OPZD20        WHENEQ    '01'
204200120305     C* F3=Fine
204300120305     C                   Z-ADD     3             X
204400120305     C                   MOVE      '0'           $FC1(X)
204500120305     C                   MOVE      '0'           $FC2(X)
204600120305     C                   MOVE      '0'           $FC3(X)
204700120305     C*
204800120305     C*  aggiungere nello stesso modo le altre opzioni da
204900120305     C*  disattivare (quelle specifiche per ogni pgm)
205000120305     C*
205100120305     C* Se pgm richiamato in interrogazione disattivo :
205200120305     C     OPZD20        WHENEQ    '05'
205300120305     C* F10=Immissione
205400120305     C                   Z-ADD     10            X
205500120305     C                   MOVE      '0'           $FC1(X)
205600120305     C                   MOVE      '0'           $FC2(X)
205700120305     C* 1=Selezione
205800120305     C                   Z-ADD     1             X
205900120305     C     01            LOOKUP    $OP1(X)                                21
206000120305     C     *IN21         IFEQ      *ON
206100120305     C                   MOVE      'N'           $FL1(X)
206200120305     C                   ENDIF
206300120305     C* 2=Modifica
206400120305     C                   Z-ADD     1             X
206500120305     C     02            LOOKUP    $OP1(X)                                21
206600120305     C     *IN21         IFEQ      *ON
206700120305     C                   MOVE      'N'           $FL1(X)
206800120305     C                   ENDIF
206900120305     C* 3=Copia
207000120305     C                   Z-ADD     1             X
207100120305     C     03            LOOKUP    $OP1(X)                                21
207200120305     C     *IN21         IFEQ      *ON
207300120305     C                   MOVE      'N'           $FL1(X)
207400120305     C                   ENDIF
207500120305     C* 4=Annullamento
207600120305     C                   Z-ADD     1             X
207700120305     C     04            LOOKUP    $OP1(X)                                21
207800120305     C     *IN21         IFEQ      *ON
207900120305     C                   MOVE      'N'           $FL1(X)
208000120305     C                   ENDIF
208100120305     C*
208200120305     C     OPZD20        WHENEQ    *BLANKS
208300120305     C* Se pgm non richiamato, disattivo subito :
208400120305     C* F8=Selezione seguente
208500120305     C                   Z-ADD     8             X
208600120305     C                   MOVE      '0'           $FC1(X)
208700120305     C                   MOVE      '0'           $FC2(X)
208800120305     C* 1=Selezione
208900120305     C                   Z-ADD     1             X
209000120305     C     01            LOOKUP    $OP1(X)                                21
209100120305     C     *IN21         IFEQ      *ON
209200120305     C                   MOVE      'N'           $FL1(X)
209300120305     C                   ENDIF
209400120305     C*
209500120305     C* In tutti gli altri casi, disattivo subito :
209600120305     C                   OTHER
209700120305     C* 1=Selezione
209800120305     C                   Z-ADD     1             X
209900120305     C     01            LOOKUP    $OP1(X)                                21
210000120305     C     *IN21         IFEQ      *ON
210100120305     C                   MOVE      'N'           $FL1(X)
210200120305     C                   ENDIF
210300120305     C*
210400120305     C                   ENDSL
210500120305     C*
210600120305     C* poi, disattivo a seconda delle autorizzazioni :
210700120305     C     IMMALL        IFEQ      *off
210800120305     C                   Z-ADD     1             X
210900120305     C     03            LOOKUP    $OP1(X)                                21
211000120305     C     *IN21         IFEQ      *ON
211100120305     C                   MOVE      'N'           $FL1(X)
211200120305     C                   ENDIF
211300120305     C* F10=Immissione
211400120305     C                   Z-ADD     10            X
211500120305     C                   MOVE      '0'           $FC1(X)
211600120305     C                   MOVE      '0'           $FC2(X)
211700120305     C                   ENDIF
211800120305     C* manutenzione
211900120305     C     MANALL        IFEQ      *off
212000120305     C                   Z-ADD     1             X
212100120305     C     02            LOOKUP    $OP1(X)                                21
212200120305     C     *IN21         IFEQ      *ON
212300120305     C                   MOVE      'N'           $FL1(X)
212400120305     C                   ENDIF
212500120305     C                   ENDIF
212600120305     C* annullamento
212700120305     C     ANNALL        IFEQ      *off
212800120305     C                   Z-ADD     1             X
212900120305     C     04            LOOKUP    $OP1(X)                                21
213000120305     C     *IN21         IFEQ      *ON
213100120305     C                   MOVE      'N'           $FL1(X)
213200120305     C                   ENDIF
213300120305     C                   ENDIF
213400120305     C* visualizzazione
213500120305     C     VISALL        IFEQ      *off
213600120305     C                   Z-ADD     1             X
213700120305     C     05            LOOKUP    $OP1(X)                                21
213800120305     C     *IN21         IFEQ      *ON
213900120305     C                   MOVE      'N'           $FL1(X)
214000120305     C                   ENDIF
214100120305     C                   ENDIF
214200120305     C                   ENDSR
214300120305     C/EJECT
214400120305     C************************************************************
214500120305     C* REPERIMENTO OPZIONI
214600120305     C************************************************************
214700120305     C     REPOPZ        BEGSR
214800120305     C*
214900120305     C     1             DO        $DIMOP        X
215000120305     C*
215100120305     C     $MS1(X)       IFNE      *BLANK
215200120305     C*
215300120305     C                   MOVEL     $MS1(X)       IDMSG
215400120305     C                   CALLB     'XRTVM'
215500120305     C                   PARM                    IDMSG            27
215600120305     C                   PARM                    TXTMSG          100
215700120305     C                   PARM                    ERRMSG            1
215800120305     C*
215900120305     C     ERRMSG        IFNE      *ON
216000120305     C                   MOVEL     TXTMSG        $TX1(X)
216100120305     C                   ELSE
216200120305     C                   MOVEL     *ALL'?'       $TX1(X)
216300120305     C                   ENDIF
216400120305     C*
216500120305     C                   ENDIF
216600120305     C*
216700120305     C                   ENDDO
216800120305     C*
216900120305     C                   ENDSR
217000120305     C/EJECT
217100120305     C************************************************************
217200120305     C* REPERIMENTO TASTI DI FUNZIONE
217300120305     C************************************************************
217400120305     C     REPKEY        BEGSR
217500120305     C                   MOVEA     $FM1          $FM
217600120305     C                   MOVEA     $FC1          $FC
217700120305     C                   EXSR      REPKKK
217800120305     C                   MOVEA     $FX           $FX1
217900120305     C*
218000120305     C                   MOVEA     $FM2          $FM
218100120305     C                   MOVEA     $FC2          $FC
218200120305     C                   EXSR      REPKKK
218300120305     C                   MOVEA     $FX           $FX2
218400120305     C*
218500120305     C                   MOVEA     $FM3          $FM
218600120305     C                   MOVEA     $FC3          $FC
218700120305     C                   EXSR      REPKKK
218800120305     C                   MOVEA     $FX           $FX3
218900120305     C                   ENDSR
219000120305     C/EJECT
219100120305     C************************************************************
219200120305     C* REPERIMENTO TASTI DI FUNZIONE
219300120305     C************************************************************
219400120305     C     REPKKK        BEGSR
219500120305     C*
219600120305     C                   CLEAR                   $FX
219700120305     C     1             DO        24            X
219800120305     C*
219900120305     C     $FM(X)        IFNE      *BLANK
220000120305     C     $Fc(X)        andne     '0'
220100120305     C*
220200120305     C                   MOVEL     $FM(X)        IDMSG
220300120305     C                   CALLB     'XRTVM'
220400120305     C                   PARM                    IDMSG            27
220500120305     C                   PARM                    TXTMSG          100
220600120305     C                   PARM                    ERRMSG            1
220700120305     C*
220800120305     C     ERRMSG        IFNE      *ON
220900120305     C                   MOVEL     TXTMSG        $FX(X)
221000120305     C                   ELSE
221100120305     C                   MOVEL     *ALL' '       $FX(X)
221200120305     C                   ENDIF
221300120305     C*
221400120305     C                   ENDIF
221500120305     C*
221600120305     C                   ENDDO
221700120305     C*
221800120305     C                   ENDSR
221900120305     C/EJECT
222000120305     C************************************************************
222100120305     C* DEFINIZIONE KLIST
222200120305     C************************************************************
222300120305     C     DefKlist      BEGSR
222400120305     C*
222500120305     C* klist
222600120305     C*
222700120305     C* SDGBLS01L
222800120305     C     K02BLS01      KLIST
222900120305     C                   KFLD                    BLSSocieta
223000120305     C                   KFLD                    BLSDtIni
223100120305     C*
223200120305     C                   ENDSR
223300120305     C/EJECT
223400120305** $MS1 / $TX1
223500120305PROMSG    *LIBL     OPZ0001
223600120305PROMSG    *LIBL     OPZ0002
223700120305PROMSG    *LIBL     OPZ0003
223800120305PROMSG    *LIBL     OPZ0004
223900120305PROMSG    *LIBL     OPZ0005
224000120305** $OP1 / $FL1
22410012030501S      <Selezione
22420012030502S      <Modifica
22430012030503S      <Copia
22440012030504S      <Annullamento
22450012030505S      <Visualizzazione
224600120305** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
224700120305PROMSG    *LIBL     KEY0001  01
224800120305PROMSG    *LIBL     KEY0002  02
224900120305PROMSG    *LIBL     KEY0003  03
225000120305PROMSG    *LIBL     KEY0004  04
225100120305PROMSG    *LIBL     KEY0005  05
225200120305PROMSG    *LIBL     KEY0006  06
225300120305PROMSG    *LIBL     KEY0007  07
225400120305PROMSG    *LIBL     KEY0008  08
225500120305PROMSG    *LIBL     KEY0009  09
225600120305PROMSG    *LIBL     KEY0010  10
225700120305PROMSG    *LIBL     KEY0011  11
225800120305PROMSG    *LIBL     KEY0012  12
225900120305PROMSG    *LIBL     KEY0013  13
226000120305PROMSG    *LIBL     KEY0014  14
226100120305PROMSG    *LIBL     KEY0015  15
226200120305PROMSG    *LIBL     KEY0016  16
226300120305SDGMSG    *LIBL     SDK0001  17
226400120305PROMSG    *LIBL     KEY0018  18
226500120305PROMSG    *LIBL     KEY0019  19
226600120305PROMSG    *LIBL     KEY0020  20
226700120305PROMSG    *LIBL     KEY0021  21
226800120305PROMSG    *LIBL     KEY0022  22
226900120305PROMSG    *LIBL     KEY0023  23
227000120305PROMSG    *LIBL     KEY0024  24
227100120305**  ABILITAZIONE D1 : $FC1 / $FR1
2272001203050N  01
2273001203051N  02
2274001203051N  03
2275001203060N  04
2276001203050N  05
2277001203050N  06
2278001203050N  07
2279001203050N  08
2280001203050N  09
2281001203051N  10
2282001203050N  11
2283001203051N  12
2284001203050N  13
2285001203050N  14
2286001203050N  15
2287001203050N  16
2288001203050N  17
2289001203050N  18
2290001203050N  19
2291001203050N  20
2292001203050N  21
2293001203050N  22
2294001203050N  23   Impostare a '1' se si utilizzano opzioni
2295001203051N  24   Lasciare sempre impostata a '1' il valore della riga 24
229600120305** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
229700120305PROMSG    *LIBL     KEY0001  01
229800120305PROMSG    *LIBL     KEY0002  02
229900120305PROMSG    *LIBL     KEY0003  03
230000120305PROMSG    *LIBL     KEY0004  04
230100120305PROMSG    *LIBL     KEY0005  05
230200120305PROMSG    *LIBL     KEY0006  06
230300120305PROMSG    *LIBL     KEY0007  07
230400120305PROMSG    *LIBL     KEY0008  08
230500120305PROMSG    *LIBL     KEY0009  09
230600120305PROMSG    *LIBL     KEY0010  10
230700120305PROMSG    *LIBL     KEY0011  11
230800120305PROMSG    *LIBL     KEY0012  12
230900120305PROMSG    *LIBL     KEY0013  13
231000120305PROMSG    *LIBL     KEY0014  14
231100120305PROMSG    *LIBL     KEY0015  15
231200120305PROMSG    *LIBL     KEY0016  16
231300120305SDGMSG    *LIBL     SDK0001  17
231400120305PROMSG    *LIBL     KEY0018  18
231500120305PROMSG    *LIBL     KEY0019  19
231600120305PROMSG    *LIBL     KEY0020  20
231700120305PROMSG    *LIBL     KEY0021  21
231800120305PROMSG    *LIBL     KEY0022  22
231900120305PROMSG    *LIBL     KEY0023  23
232000120305PROMSG    *LIBL     KEY0024  24
232100120305**  ABILITAZIONE S1 : $FC2 / $FR2
2322001203050N  01
2323001203050N  02
2324001203051N  03
2325001203050N  04
2326001203050N  05
2327001203050N  06
2328001203050N  07
2329001203050N  08
2330001203050N  09
2331001203051N  10
2332001203050N  11
2333001203051N  12
2334001203050N  13
2335001203050N  14
2336001203050N  15
2337001203050N  16
2338001203051N  17
2339001203050N  18
2340001203050N  19
2341001203050N  20
2342001203050N  21
2343001203050N  22
2344001203051N  23   Impostare a '1' se si utilizzano opzioni
2345001203051N  24   Lasciare sempre impostata a '1' il valore della riga 24
234600120305** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
234700120305PROMSG    *LIBL     KEY0001  01
234800120305PROMSG    *LIBL     KEY0002  02
234900120305PROMSG    *LIBL     KEY0003  03
235000120305PROMSG    *LIBL     KEY0004  04
235100120305PROMSG    *LIBL     KEY0005  05
235200120305PROMSG    *LIBL     KEY0006  06
235300120305PROMSG    *LIBL     KEY0007  07
235400120305PROMSG    *LIBL     KEY0008  08
235500120305PROMSG    *LIBL     KEY0009  09
235600120305PROMSG    *LIBL     KEY0010  10
235700120305PROMSG    *LIBL     KEY0011  11
235800120305PROMSG    *LIBL     KEY0012  12
235900120305PROMSG    *LIBL     KEY0013  13
236000120305PROMSG    *LIBL     KEY0014  14
236100120305PROMSG    *LIBL     KEY0015  15
236200120305PROMSG    *LIBL     KEY0016  16
236300120305PROMSG    *LIBL     KEY0017  17
236400120305PROMSG    *LIBL     KEY0018  18
236500120305PROMSG    *LIBL     KEY0019  19
236600120305PROMSG    *LIBL     KEY0020  20
236700120305PROMSG    *LIBL     KEY0021  21
236800120305PROMSG    *LIBL     KEY0022  22
236900120305PROMSG    *LIBL     KEY0023  23
237000120305PROMSG    *LIBL     KEY0024  24
237100120305**  ABILITAZIONE D3 : $FC3 / $FR3
2372001203050N  01
2373001203050N  02
2374001203051N  03
2375001203050N  04
2376001203050N  05
2377001203050N  06
2378001203050N  07
2379001203050N  08
2380001203050N  09
2381001203050N  10
2382001203050N  11
2383001203051N  12
2384001203050N  13
2385001203050N  14
2386001203050N  15
2387001203050N  16
2388001203050N  17
2389001203050N  18
2390001203050N  19
2391001203050N  20
2392001203050N  21
2393001203050N  22
2394001203050N  23   Impostare a '1' se si utilizzano opzioni
2395001203051N  24   Lasciare sempre impostata a '1' il valore della riga 24
