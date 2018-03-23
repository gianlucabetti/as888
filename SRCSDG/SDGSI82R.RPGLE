000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200970619     H*PARMS CVTOPT(*NONE)
000300970619     H DECEDIT('0,') DATEDIT(*DMY.)
000400970619      *
000500170719      *         ------------------------------------------------
000600170719      *                      Spesometro Trimestrale
000700170719      *                 Elenco file spesometro trimestrale
000800170719      *         ------------------------------------------------
000900170719      *
001000170719R258  * Questa PGM è stato creato con il task R258
001100170719      *
001200170719      *****************************************************************
001300170731    >FSDGCSF01L IF   E           K DISK
001400170731    >F                                     RENAME(SDGCSF000:SDGCSF01)
001500970620     F*------------
001600970620    >FANSIF01L  IF   E           K DISK
001700970619     F*------------
001800170719    >FSDGSI82D  CF   E             WORKSTN
001900170719    >F                                     RENAME(SI82D1:FMTD1)
002000170719    >F                                     RENAME(SI82D2:FMTD2)
002100170719    >F                                     RENAME(SI82D3:FMTD3)
002200170719    >F                                     RENAME(SI82S1:FMTS1)
002300170719    >F                                     RENAME(SI82C1:FMTC1)
002400170719    >F                                     RENAME(SI82T1:FMTT1)
002500170719    >F                                     RENAME(SI82Z1:FMTZ1)
002600170719    >F                                     RENAME(SI82Z2:FMTZ2)
002700170719    >F                                     RENAME(SI82Z3:FMTZ3)
002800970619     F                                     SFILE(FMTS1:S1NRR)
002900970619     F                                     INFDS(DSFMT)
003000970619     D*----------------------------------------------------*
003100970619     D* Schiere per la gestione delle opzioni
003200970619     D*
003300970619     D* dimensione della schiera $MS1, da non cambiare
003400970619     D $DIMOP          C                   CONST(30)
003500970619     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003600970619     D $TX1            S             50    DIM($dimop) ALT($MS1)
003700970619     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003800970619     D $FL1            S              1    DIM($dimop) ALT($OP1)
003900970619     D* schiera contenente i nomi dei programma richiamati attraverso
004000970619     D* le opzioni personalizzate
004100970619     D $PGO1           S             10    DIM($dimop)
004200970619     D $TX             S             50    DIM(50)
004300970619     D $FL             S              1    DIM(50)
004400970619     D* Schiere generali per la gestione dei tasti di funzione
004500970619     D $FM             S             27    DIM(24)
004600970619     D $FX             S             50    DIM(24)
004700970619     D $FC             S              1    DIM(24)
004800970619     D $FR             S              1    DIM(24)
004900970619     D* Schiere per la gestione dei tasti di funzione
005000970619     D* Video D1
005100970619     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005200970619     D $FX1            S             50    DIM(24) ALT($FM1)
005300970619     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005400970619     D $FR1            S              1    DIM(24) ALT($FC1)
005500970619     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005600970619     D* funzione personalizzata S1
005700970619     D $PGF2           S             10    DIM(25)
005800970619     D* Schiere per la gestione dei tasti di funzione
005900970619     D* Video S1
006000970619     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006100970619     D $FX2            S             50    DIM(24) ALT($FM2)
006200970619     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006300970619     D $FR2            S              1    DIM(24) ALT($FC2)
006400970619     D* Schiere per la gestione dei tasti di funzione
006500970619     D* Video D2
006600970619     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
006700970619     D $FX3            S             50    DIM(24) ALT($FM3)
006800970619     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
006900970619     D $FR3            S              1    DIM(24) ALT($FC3)
007000970619     D*
007100970619     D $D1L01          S              3  0
007200970619     D $D1L02          S              3  0
007300970619     D $S1L01          S              3  0
007400970619     D $S1L02          S              3  0
007500970619     D $C1Op1          S              3  0
007600970619     D $C1Op2          S              3  0
007700970619     D $D2L01          S              3  0
007800970619     D $D2L02          S              3  0
007900970619     D*
008000970619     D*-------------
008100970619     D* Indice della schiera tasti funzione
008200970619     D PosFun          S              2  0
008300970619     D*----------------------------------------------------*
008400970619     D* Valori assunti
008500970619     D XMPEDS        E DS                  INZ
008600970619     D XMPEDS1       E DS                  INZ
008700970619     D DSMPE           DS                  INZ
008800970619     D  $LOP                   1      1  0
008900970619     D  $ORD                   2      3  0
009000970619     D*-------------
009100970619     D* Passaggio Parametri
009200970619     D KPJBA         E DS
009300970619     D*-------------
009400970619     D* Reperimento Autorizzazioni
009500970619     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009600970619     D*-------------
009700970619     D* Passaggio Autorizzazioni
009800970619     D DSAUT           DS           512
009900970619     D*-------------
010000970619     D* Maschera dati autorizzazioni
010100981026    >D ANGCFIDS      E DS
010200970619     D*-------------
010300970619     D* Dati di ambiente ottenuti da XSOC
010400170731     D SOC001        E DS                  EXTNAME(SDGSOCDS )
010500970619     D*-------------
010600970619     D* DS Interna per dati di output di XSOC
010700970619     D XSOCDS          DS          1000
010800970619     D*-------------
010900970619     D* Parametri in ricezione
011000170719    >D SDGSI82DS     E DS
011100970619     D*-------------
011200170719     D* Parametri del PGM di manutenzione
011300170719    >D SDGSI83DS     E DS                  inz
011400170719    >D  RETI83       E                     INZ('0')
011500170719    >D  OPRI83       E                     INZ('0')
011600170719    >D  ERRI83       E                     INZ('0')
011700170810     D*-------------
011800170810     D* Visualizza movimenti
011900170810     D SDGSI73DS     E DS                  inz
012000170810     D  RETI73       E                     INZ('0')
012100170810     D  OPRI73       E                     INZ('0')
012200170810     D  ERRI73       E                     INZ('0')
012300170810     D  CMTI73       E                     INZ('0')
012400970619     D*-------------
012500970619     D* campo di salvataggio per la DS d'ingresso durante la chiamata
012600970619     D* alla funzione personalizzata
012700170719    >D $SaveDS         S                   like(SDGSI82DS)
012800970619     D*-------------
012900970620     D* Ricerca società
013000970620     D A113DS        E DS                  EXTNAME(ANA113DS) INZ
013100970620     D*-------------
013200970620     D* Ricerca legende
013300170731     D Ana964DS      E DS
013400970620     D*-------------
013500970620     D* Controlli validità per valore di una legenda
013600170731     D X07VALDS      E DS
013700970620     D*-------------
013800970619     D DSFMT           DS           512
013900970619     D  $TASTO               369    369
014000970619     D  NRG                  370    370
014100970619     D  NCL                  371    371
014200970619     D  SFLNRR               378    379B 0
014300970619     D*-------------
014400970619     D* posizione cursore
014500970619     D CURSOR          DS
014600970619     D  RIRI                   1      2B 0 INZ
014700970619     D  R$$                    2      2
014800970619     D  COCO                   3      4B 0 INZ
014900970619     D  C$$                    4      4
015000970619     D*-------------
015100970619     D* Reperimento nome PGM
015200970619     D STATUS         SDS           333
015300970619     D  DSPGM            *PROC
015400970619     D  PARMS            *PARMS
015500970619     D*-------------
015600970619     D* Costanti
015700970619     D* nr.di righe del sfl (SFLPAG)
015800170731     D SFLPAG          C                   CONST(12)
015900970619     D*-------------
016000970619     D* Tasti di funzione
016100970619     D F01             C                   CONST(X'31')
016200970619     D F02             C                   CONST(X'32')
016300970619     D F03             C                   CONST(X'33')
016400970619     D F04             C                   CONST(X'34')
016500970619     D F05             C                   CONST(X'35')
016600970619     D F06             C                   CONST(X'36')
016700970619     D F07             C                   CONST(X'37')
016800970619     D F08             C                   CONST(X'38')
016900970619     D F09             C                   CONST(X'39')
017000970619     D F10             C                   CONST(X'3A')
017100970619     D F11             C                   CONST(X'3B')
017200970619     D F12             C                   CONST(X'3C')
017300970619     D F13             C                   CONST(X'B1')
017400970619     D F14             C                   CONST(X'B2')
017500970619     D F15             C                   CONST(X'B3')
017600970619     D F16             C                   CONST(X'B4')
017700970619     D F17             C                   CONST(X'B5')
017800970619     D F18             C                   CONST(X'B6')
017900970619     D F19             C                   CONST(X'B7')
018000970619     D F20             C                   CONST(X'B8')
018100970619     D F21             C                   CONST(X'B9')
018200970619     D F22             C                   CONST(X'BA')
018300970619     D F23             C                   CONST(X'BB')
018400970619     D F24             C                   CONST(X'BC')
018500970619     D ENTER           C                   CONST(X'F1')
018600970619     D ROLDWN          C                   CONST(X'F4')
018700970619     D ROLLUP          C                   CONST(X'F5')
018800970619     D*-------------
018900970619     D* Sottotitolo di testata
019000170731     D SOTTIT          C                   CONST('SDGMSG    *LIBL-
019100170731     D                                          SDC0115')
019200981028     D*-------------
019300981028     D* variabili di appoggio alla chiamata a x65runqry
019400981028     D $File           S             10A
019500981028     D $Lib            S             10A
019600981028     D $Membro         S             10A
019700981028     D $Output         S              1A
019800981030     D $Errore         S              1A
019900970619     D*-------------
020000970619     D DATMIN          C                   CONST('0001-01-01')
020100970619     D DATMAX          C                   CONST('9999-12-31')
020200970619     D*-------------
020300970619     D* Variabili per gestione videate
020400970619     D $Gest           S              2A
020500970619     D $Fine           S              1A
020600970619     D $InzD1          S              1A
020700970619     D $InzS1          S              1A
020800970619     D $InzD2          S              1A
020900970619     D $LastG          S              2A
021000970619     D $LastV          S              2A
021100970619     D $EndFile        S              1A
021200970619     D $Esci           S              1A
021300970619     D $RcdOK          S              1A
021400970619     D $Unique         S              1A
021500970619     D*-------------
021600970619     D* Variabili appoggio sempre presenti in tutte le anagrafiche
021700970619     D S1Nrr           S                   like(C1Rcd)
021800970619     D WSfl            S                   like(C1Nrr)
021900970619     D WMax            S                   like(C1Rcd)
022000970619     D WPag            S                   like(C1Rcd)
022100970619     D WInzS1          S                   like($InzS1)
022200170719    >D WOpr            S                   like(OprI82)
022300970619     D CurR            S              2S 0
022400970619     D CurC            S              2S 0
022500970619     D Win             S             99A
022600970619     D WINKR           S              1A
022700170719     D WExf            S                   like(ExfI83)
022800970619     D*-------------
022900970619     D*  Indici
023000970619     D X               S              3S 0
023100970619     D Y               S              3S 0
023200970619     D* Indice della schiera per le opzioni personalizzate
023300970619     D PosOpz          S              2  0
023400970619     D* numero max opzioni
023500970619     D NumOpz          S              2  0 Inz($DIMOP)
023600970619     D* numero effettive opzioni
023700970619     D OpzLen          S              2  0
023800971015     D*-------------
023900170731     D ZDateISO        S               D
024000970619     C*----------------------------------------------------*
024100970619     C*                MAIN LINE PROGRAM
024200970619     C*----------------------------------------------------*
024300970619     C*
024400970619     C* inizializzazione variabili
024500970619     C                   EXSR      INZVAR
024600970619     C*
024700970619     C     $FINE         DOWEQ     *OFF
024800970619     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
024900970619     C     $GEST         CASEQ     'S1'          GESS1                          Lista
025000970619     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
025100970619     C                   ENDCS
025200970619     C                   ENDDO
025300970619     C* fine programma
025400970619     C                   EXSR      ENDPGM
025500970619     C*
025600970619     C/EJECT
025700970619     C************************************************************
025800970619     C* FINE PROGRAMMA
025900970619     C************************************************************
026000970619     C     ENDPGM        BEGSR
026100970619     C*
026200970619     C* passaggio dati al pgm chiamante
026300170719    >C                   MOVEL     WOPR          OPRI82                                     ne S1
026400170719    >C                   MOVEL     SDGSI82DS     KPJBU
026500970619     C*
026600970619     C* Se pgm Guida non è stato richiamato posso chiudere in LR
026700170719    >C     OPZI82        IFEQ      *BLANK
026800970619     C                   SETON                                            LR
026900970619     C* altrimenti chiudo in RT
027000970619     C                   ELSE
027100970619     C                   SETON                                            RT
027200970619     C                   ENDIF
027300970619     C                   RETURN
027400970619     C*
027500970619     C                   ENDSR
027600970619     C/EJECT
027700970619     C************************************************************
027800970619     C* RICERCHE
027900970619     C************************************************************
028000970619     C     SEARCH        BEGSR
028100970619     C*
028200970619     C* determino Riga/Colonna del cursore
028300970619     C                   MOVE      NRG           R$$
028400970619     C                   MOVE      NCL           C$$
028500970619     C                   Z-ADD     RIRI          CURR
028600970619     C                   Z-ADD     COCO          CURC
028700970619     C*
028800970619   B1C                   SELECT
028900170731     C*
029000170731     C     H1NMFL        WHENEQ    'D1TRIM'
029100170731     C                   Reset                   ANA964DS
029200170731     C*
029300170731     C                   MoveL     '01'          Opz964
029400170731     C                   MoveL     'SDGCST000 '  Trc964
029500170731     C                   MoveL     'CSTTRIM   '  Cam964
029600170731     C                   MoveL     *On           All964
029700170731     C*
029800170731     C                   MoveL     ANA964DS      KPJBU
029900170731     C                   CALL      'ANA964R'
030000170731     C                   PARM                    KPJBA
030100170731     C                   MoveL     KPJBU         ANA964DS
030200170731     C*
030300170731     C                   if        Ret964     <> '2'
030400170731     C                   MoveL     Val964        D1Trim
030500170731     C                   endif
030600170804     C*
030700170804     C*  Natura soggetto
030800170804     C     H1NMFL        WHENEQ    'D1CF'
030900170804     C                   Reset                   ANA964DS
031000170804     C*
031100170804     C                   MoveL     '01'          Opz964
031200170804     C                   MoveL     'ANDIZ000'    Trc964
031300170804     C                   MoveL     'DSPCLIFOR'   Cam964
031400170804     C                   MoveL     *On           All964
031500170804     C*
031600170804     C                   MoveL     ANA964DS      KPJBU
031700170804     C                   CALL      'ANA964R'
031800170804     C                   PARM                    KPJBA
031900170804     C                   MoveL     KPJBU         ANA964DS
032000170804     C*
032100170804     C                   if        Ret964     <> '2'
032200170804     C                   MoveL     Val964        D1CF
032300170804     C                   endif
032400970620     C*
032500970619     C* ricerca non abilitata in questa posizione
032600970619   X1C                   OTHER
032700970619     C                   SETON                                        9899
032800970619     C*
032900970619   E1C                   ENDSL
033000970619     C*
033100970619     C* imposto pos. del cursore
033200970619     C                   Z-ADD     CURR          H1RIGA
033300970619     C                   Z-ADD     CURC          H1COLO
033400970619     C*
033500970619     C                   MOVE      *HIVAL        $LASTV
033600970619     C*
033700970619     C                   ENDSR
033800970619     C/EJECT
033900970619     C************************************************************
034000970619     C* RIEMPIMENTO OPZIONI
034100970619     C************************************************************
034200970619     C     RIEOPZ        BEGSR
034300970619     C*
034400970619     C* Input :
034500970619     C* - $TX = schiera con tutti i testi disponibili
034600970619     C* - $FL = schiere con i flag di validità associati ai testi
034700970619     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
034800970619     C*          opzione usato
034900970619     C* - WMXO = variabile contenente il numero massimo di testi
035000970619     C*          opzione usabili
035100970619     C* - $L01 = Lunghezza prima riga
035200970619     C* - $L02 = Lunghezza seconda riga
035300970619     C* Output :
035400970619     C* - WOPZ1 = variabile contenente la prima riga opzioni
035500970619     C* - WOPZ2 = variabile contenente la seconda riga opzioni
035600970619     C* - FF23  = flag che indica se c'è bisogno del tasto F23
035700970619     C*
035800970619     C                   CALLB     'X04OPMSG'
035900970619     C* input
036000970619     C                   PARM                    $TX
036100970619     C                   PARM                    $FL
036200970619     C                   PARM                    WCOZ              3 0
036300970619     C                   PARM                    WMXO              3 0
036400970619     C* output
036500970619     C                   PARM                    WOPZ1            76
036600970619     C                   PARM                    WOPZ2            76
036700970619     C                   PARM                    FF23              1
036800970619     C* input
036900970619     C                   PARM                    $L01              3 0
037000970619     C                   PARM                    $L02              3 0
037100970619     C*
037200170731     C                   PARM                    $ALLOPT         960
037300170731     C*
037400970619     C                   ENDSR
037500970619     C/EJECT
037600970619     C************************************************************
037700970619     C* RIEMPIMENTO TASTI FUNZIONE
037800970619     C************************************************************
037900970619     C     RIEKEY        BEGSR
038000970619     C*
038100970619     C* Input :
038200970619     C* - $FX = schiera con tutti i testi disponibili
038300970619     C* - $FC = schiere con i flag di validità associati ai testi
038400970619     C* - $FR = schiere con i flag di REVERSE IMAGE
038500970619     C* - $ULK = ultimo testo caricato
038600970619     C* - $L01 = Lunghezza prima riga
038700970619     C* - $L02 = Lunghezza seconda riga
038800970619     C* Output :
038900970619     C* - $ULK
039000970619     C* - $KEY1= 1° riga di tasti funzione
039100970619     C* - $KEY2= 2° riga di tasti funzione
039200970619     C*
039300970619     C                   CALLB     'XKEYMSG'
039400970619     C* input
039500970619     C                   PARM                    $FX
039600970619     C                   PARM                    $FC
039700970619     C                   PARM                    $FR
039800970619     C                   PARM                    $ULK              3 0
039900970619     C* output
040000970619     C                   PARM                    $KEY1            79
040100970619     C                   PARM                    $KEY2            79
040200970619     C* input
040300970619     C                   PARM                    $L01              3 0
040400970619     C                   PARM                    $L02              3 0
040500970619     C*
040600170731     C                   PARM                    $ALLFUNCT       480
040700170731     C*
040800970619     C                   ENDSR
040900970619     C/EJECT
041000970619     C************************************************************
041100970619     C* GESTIONE EMISSIONE TASTI FUNZIONE
041200970619     C************************************************************
041300970619     C     GESPIE        BEGSR
041400970619     C*
041500970619     C                   SELECT
041600970619     C* Parzializzazioni
041700970619     C     $GEST         WHENEQ    'D1'
041800970619     C     $ULKD1        IFEQ      0
041900970619     C* riempimento opzioni
042000970619     C                   EXSR      F24D1
042100970619     C                   ELSE
042200970619     C                   WRITE     FMTZ1
042300970619     C                   ENDIF
042400970619     C* Lista
042500970619     C     $GEST         WHENEQ    'S1'
042600970619     C     $ULKS1        IFEQ      0
042700970619     C                   EXSR      F24S1
042800970619     C                   ELSE
042900970619     C                   WRITE     FMTZ2
043000970619     C                   ENDIF
043100970619     C* Modifica valori assunti
043200970619     C     $GEST         WHENEQ    'D2'
043300970619     C     $ULKD2        IFEQ      0
043400970619     C                   EXSR      F24D2
043500970619     C                   ELSE
043600970619     C                   WRITE     FMTZ3
043700970619     C                   ENDIF
043800970619     C                   ENDSL
043900970619     C*
044000970619     C                   ENDSR
044100970619     C/EJECT
044200970619     C************************************************************
044300970619     C* GESTIONE PARZIALIZZAZIONI
044400970619     C************************************************************
044500970619     C     GESD1         BEGSR
044600970619     C* inizializzazione videata
044700970619     C     $INZD1        IFEQ      *ON
044800970619     C                   EXSR      INZD1
044900970619     C                   MOVE      *OFF          $INZD1
045000970619     C                   ENDIF
045100970619     C* emissione piede videata se proveniente da altra
045200970619     C* salvataggio valore formato di provenienza
045300970619     C     $LASTV        IFNE      'D1'
045400970619     C* salvo stato F17 per sapere cosa fare in caso di F12
045500970619     C                   MOVE      *INKR         WINKR
045600970619     C                   WRITE     FMTT1
045700970619     C                   EXSR      GESPIE
045800970619     C                   MOVE      $LASTV        $LASTG
045900970619     C                   MOVE      'D1'          $LASTV
046000970619     C                   ENDIF
046100970619     C*
046200970619     C* se esistono errori sulla videata
046300970619     C* emetto la write del formato a indicatori spenti per vedere
046400970619     C* le eventuali decodifiche
046500970619     C     *IN99         IFEQ      *ON
046600970619     C                   MOVEA     *IN           WIN
046700970619     C                   MOVE      *ALL'0'       IN5098           49
046800970619     C                   MOVEA     IN5098        *IN(50)
046900970619     C                   WRITE     FMTD1
047000970619     C                   MOVEA     WIN           *IN
047100970619     C                   ENDIF
047200970619     C*
047300970619     C                   EXFMT     FMTD1
047400970619     C*
047500970619     C* azzero la posizione del cursore
047600970619     C                   Z-ADD     0             H1RIGA
047700970619     C                   Z-ADD     0             H1COLO
047800970619     C*
047900970619     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
048000970619     C* INPUT
048100970619     C* $FC1 = Abilitazioni videata D1
048200970619     C* OUTPUT
048300970619     C* *IN97 + *IN99 Errori
048400970619     C                   MOVEA     $FC1          $FC
048500970619     C                   EXSR      CTRKEY
048600970619     C*
048700970619     C     *IN99         IFEQ      *OFF
048800970619     C*
048900970619     C                   SELECT
049000970619     C* F2=Decodifica
049100970619     C     $TASTO        WHENEQ    F02
049200970619     C                   EXSR      F02D1
049300970619     C* F3=Fine
049400970619     C     $TASTO        WHENEQ    F03
049500970619     C                   EXSR      F03D1
049600970619     C* F4=Ricerca
049700970619     C     $TASTO        WHENEQ    F04
049800970619     C                   EXSR      SEARCH
049900970619     C* F5=Refresh
050000970619     C     $TASTO        WHENEQ    F05
050100970619     C                   EXSR      F05D1
050200970619     C* F8=Successivo
050300970619     C     $TASTO        WHENEQ    F08
050400970619     C                   EXSR      F08D1
050500970619     C* F10=Immissione
050600970619     C     $TASTO        WHENEQ    F10
050700970619     C                   EXSR      F10D1
050800970619     C* F12=Ritorno
050900970619     C     $TASTO        WHENEQ    F12
051000970619     C                   EXSR      F12D1
051100970619     C* F18=Modifica valori assunti
051200970619     C     $TASTO        WHENEQ    F18
051300970619     C                   EXSR      F18D1
051400970619     C* F24=Altri tasti
051500970619     C     $TASTO        WHENEQ    F24
051600970619     C                   EXSR      F24D1
051700970619     C*
051800970619     C                   OTHER
051900970619     C                   EXSR      CTRD1
052000970619     C     *IN99         IFEQ      *OFF
052100970619     C* definisco l'ordinamento
052200970619     C                   EXSR      DEFORD
052300970619     C* richiamo la lista
052400970619     C                   MOVE      'S1'          $GEST
052500970619     C                   MOVE      *ON           $INZS1
052600970619     C                   ENDIF
052700970619     C*
052800970619     C                   ENDSL
052900970619     C*
053000970619     C                   ENDIF
053100970619     C*
053200970619     C                   ENDSR
053300970619     C/EJECT
053400970619     C************************************************************
053500970619     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
053600970619     C************************************************************
053700970619     C     INZD1         BEGSR
053800970619     C*
053900970619     C                   CLEAR                   FMTD1
054000970619     C* Se pgm  richiamato con parametri forniti , li imposto nelle
054100970619     C*    parzializzazioni
054200970619     C                   SELECT
054300970619     C* il pgm è richiamato e
054400970619     C* sono stati passati dei valori di parzializzazione ma non
054500970619     C* la chiave univoca
054600170719    >C     OPZI82        WHENNE    *BLANK
054700970619     C* imposto i valori passati sui campi a video
054800170719    >C                   MOVE      TrimI82       D1Trim
054900170719    >C                   MOVE      AnnoI82       D1Anno
055000170807    >C                   MOVE      CfI82         D1Cf
055100170807    >C                   MOVEL     IdFileI82     D1IdFile
055200170807    >C                   MOVEL     FileI82       D1File
055300970619     C* il pgm non è richiamato
055400170719    >C     OPZI82        WHENEQ    *BLANK
055500970619     C* imposto dei valori di default
055600970619     C                   ENDSL
055700170804     C*
055800970619     C                   ENDSR
055900970619     C/EJECT
056000970619     C************************************************************
056100970619     C* GESTIONE F02 VIDEO D1
056200970619     C************************************************************
056300970619     C     F02D1         BEGSR
056400970619     C*
056500970619     C* per decodificare la videata richiamo la routine dei ctrl
056600970619     C* e poi disattivo gli eventuali messaggi di errore
056700970619     C                   EXSR      CTRD1
056800970619     C*
056900970619     C                   MOVE      *ALL'0'       IN5098
057000970619     C                   MOVEA     IN5098        *IN(50)
057100970619     C*
057200970619     C                   ENDSR
057300970619     C/EJECT
057400970619     C************************************************************
057500970619     C* GESTIONE F03 VIDEO D1
057600970619     C************************************************************
057700970619     C     F03D1         BEGSR
057800970619     C*
057900970619     C                   MOVE      *ON           $FINE
058000170719    >C                   MOVE      '1'           RETI82
058100970619     C* fine programma
058200970619     C                   EXSR      ENDPGM
058300970619     C*
058400970619     C                   ENDSR
058500970619     C/EJECT
058600970619     C************************************************************
058700970619     C* GESTIONE F05 VIDEO D1
058800970619     C************************************************************
058900970619     C     F05D1         BEGSR
059000970619     C*
059100970619     C                   MOVE      *ON           $INZD1
059200970619     C*
059300970619     C                   ENDSR
059400970619     C/EJECT
059500970619     C************************************************************
059600970619     C* GESTIONE F08 VIDEO D1
059700970619     C************************************************************
059800970619     C     F08D1         BEGSR
059900970619     C*
060000970619     C                   MOVE      *ON           $FINE
060100170719    >C                   MOVE      '0'           RETI82
060200970619     C*
060300970619     C                   ENDSR
060400970619     C/EJECT
060500970619     C************************************************************
060600970619     C* GESTIONE F10 VIDEO D1
060700970619     C************************************************************
060800970619     C     F10D1         BEGSR
060900970619     C*
061000170719    >C                   RESET                   SDGSI83DS
061100170719    >C                   MOVE      xscsoc        SocietaI83
061200170719    >C                   MOVEL     '01'          OPZI83
061300170719    >C     CMTI82        IFEQ      '1'
061400170719    >C                   MOVEL     '1'           CMTI83
061500970619     C                   ELSE
061600170719    >C                   MOVEL     '0'           CMTI83
061700970619     C                   ENDIF
061800970829     C* passo eventuale EXFMT per chiamata a pgm di sola finestra
061900170719     C                   EVAL      ExfI83 = WExf
062000170719    >C                   MOVEL     SDGSI83DS     KPJBU
062100970619     C*
062200170719    >C                   CALL      'SDGSI83R'
062300970619     C                   PARM                    KPJBA
062400170719    >C                   MOVEL     KPJBU         SDGSI83DS
062500970619     C* ritorno da PGM gestione
062600170719    >C                   EXSR      RTNI83
062700970619     C*
062800970619     C                   MOVE      *HIVAL        $LASTV
062900970619     C*
063000970619     C                   ENDSR
063100970619     C/EJECT
063200970619     C************************************************************
063300970619     C* GESTIONE F12 VIDEO D1
063400970619     C************************************************************
063500970619     C     F12D1         BEGSR
063600970619     C*
063700970619     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
063800970619     C*  per F12 torno alla lista
063900970619     C     $LOP          IFEQ      2
064000970619     C     WINKR         OREQ      *ON
064100970619     C                   MOVE      'S1'          $GEST
064200970619     C*  altrimenti, per F12 torno a menu
064300970619     C                   ELSE
064400970619     C                   MOVE      *ON           $FINE
064500170719    >C                   MOVE      '2'           RETI82
064600970619     C                   ENDIF
064700970619     C*
064800970619     C                   ENDSR
064900970619     C/EJECT
065000970619     C************************************************************
065100970619     C* GESTIONE F18 VIDEO D1
065200970619     C************************************************************
065300970619     C     F18D1         BEGSR
065400970619     C*
065500970619     C                   MOVE      'D2'          $GEST
065600970619     C                   MOVEL     *ON           $INZD2
065700970619     C*
065800970619     C                   ENDSR
065900970619     C/EJECT
066000970619     C************************************************************
066100970619     C* GESTIONE F24 VIDEO D1
066200970619     C* input
066300970619     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
066400970619     C* $FX1 =Testi tasti funzione
066500970619     C* $FC1 = Abilitazioni
066600970619     C* $FR1 = Inversioni di fondo
066700970619     C************************************************************
066800970619     C     F24D1         BEGSR
066900970619     C*
067000970619     C                   MOVEA     $FX1          $FX
067100970619     C                   MOVEA     $FC1          $FC
067200970619     C                   MOVEA     $FR1          $FR
067300970619     C                   Z-ADD     $ULKD1        $ULK
067400970619     C                   Z-ADD     $D1L01        $L01
067500970619     C                   Z-ADD     $D1L02        $L02
067600970619     C                   EXSR      RIEKEY
067700970619     C                   Z-ADD     $ULK          $ULKD1            3 0
067800970619     C                   MOVEA     $FC           $FC1
067900970619     C                   MOVEL     $KEY1         Z1KE1
068000170731     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
068100970619     C                   MOVEL     $KEY2         Z1KE2
068200970619     C                   WRITE     FMTZ1
068300970619     C*
068400970619     C*
068500970619     C                   ENDSR
068600970619     C/EJECT
068700970619     C************************************************************
068800970619     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
068900970619     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
069000970619     C* 93 94 96 97 98
069100970619     C************************************************************
069200970619     C     CTRD1         BEGSR
069300970619     C*
069400970619     C                   SETOFF                                       99
069500970619     C*
069600170731     C                   Clear                   D1TrimD
069700170731     C                   if        D1Trim <> 0
069800170731     C                   EXSR      CtrTrim
069900170731     C                   endif
070000170731     C*
070100170804     C                   EXSR      CtrCF
070200170804     C*
070300970619     C                   ENDSR
070400970619     C/EJECT
070500170731     C***********************************************************
070600170731     C* Controllo trimestre
070700170731     C***********************************************************
070800170731     C     CtrTrim       BEGSR
070900170731     C*
071000170731     C                   Clear                   X07ValDs
071100170731     C*
071200170731     C                   Move      '1'           X07Ric
071300170731     C                   Move      *On           X07All
071400170731     C                   Movel     'SDGCST000 '  X07TRc
071500170731     C                   Movel     'CSTTRIM   '  X07Cam
071600170731     C                   MoveL     D1Trim        X07VAL
071700170731     C*
071800170731     C                   Callb     'X07VALR'
071900170731     C                   Parm                    X07ValDs
072000170731     C*
072100170731     C     X07ERR        IFEQ      *Off
072200170731     C                   MOVEL     X07DES        D1TrimD
072300170731     C                   else
072400170804     C                   Seton                                        5499
072500170731     C                   endif
072600170731     C*
072700170731     C                   ENDSR
072800170731     C/EJECT
072900170804     C***********************************************************
073000170804     C* Controllo tipo soggetto
073100170804     C***********************************************************
073200170804     C     CtrCF         BEGSR
073300170804     C*
073400170804     C                   Clear                   D1CFD
073500170804     C*
073600170804     C                   Clear                   X07ValDs
073700170804     C*
073800170804     C                   Move      '1'           X07Ric
073900170804     C                   Move      *On           X07All
074000170804     C                   Movel     'ANDIZ000'    X07TRc
074100170804     C                   Movel     'DSPCLIFOR'   X07Cam
074200170804     C                   Movel     D1CF          X07VAL
074300170804     C*
074400170804     C                   Callb     'X07VALR'
074500170804     C                   Parm                    X07ValDs
074600170804     C*
074700170804     C     X07ERR        IFEQ      *Off
074800170804     C                   MOVEL     X07DES        D1CFD
074900170804     C                   else
075000170804     C                   SetOn                                        5499
075100170804     C                   endif
075200170804     C*
075300170804     C                   ENDSR
075400170804     C/EJECT
075500970619     C************************************************************
075600970619     C* DEFINIZIONE DELL'ORDINAMENTO
075700970619     C************************************************************
075800970619     C     DEFORD        BEGSR
075900970619     C*
076000970619     C* definisco l'ordinamento della lista a seconda dei valori
076100970619     C* impostati nel video di parzializzazione
076200970619     C*
076300970619     C                   Z-ADD     1             $ORD
076400970619     C*
076500970619     C                   ENDSR
076600970619     C/EJECT
076700970619     C************************************************************
076800970619     C* Chiamata programma con tasto personalizzato
076900970619     C************************************************************
077000970619     C     FPerS1        BEGSR
077100970619     C*
077200970619     C* salvo la DS di ingresso
077300170719    >C                   MOVEL     SDGSI82DS     $SaveDS
077400970619     C*
077500170719    >C                   MOVEL     SDGSI82DS     KPJBU
077600970619     C                   CALL      $PGF2(PosFun)                        22
077700970619     C                   PARM                    KPJBA
077800970619     C                   PARM                    NOMPGM
077900970619     C*
078000970619     C* ripristino la DS di ingresso
078100170719    >C                   MOVEL     $SaveDS       SDGSI82DS
078200970619     C*
078300970619     C                   ENDSR
078400970619     C/EJECT
078500970619     C************************************************************
078600970619     C* GESTIONE LISTA
078700970619     C************************************************************
078800970619     C     GESS1         BEGSR
078900970619     C* inizializzazione videata
079000970619     C     $INZS1        IFEQ      *ON
079100970619     C                   EXSR      INZS1
079200970619     C                   MOVE      *OFF          $INZS1
079300970619     C                   ENDIF
079400970619     C* emissione piede videata se proveniente da altra
079500970619     C* salvataggio valore formato di provenienza
079600970619     C     $LASTV        IFNE      'S1'
079700970619     C                   WRITE     FMTT1
079800970619     C                   EXSR      GESPIE
079900970619     C                   MOVE      $LASTV        $LASTG
080000970619     C                   MOVE      'S1'          $LASTV
080100970619     C                   ENDIF
080200970619     C*
080300170807     C                   Eval      *In09 = C1Mode
080400170807     C*
080500970619     C     WMAX          IFEQ      0
080600970619     C                   WRITE     FMTD3                                        NO RCD
080700970619     C                   ENDIF
080800971015     C* Serve per emissione finestra pgm invio ANA727r (per open/close
080900971015     C* window)
081000970619     C                   MOVE      '1'           WIN727            1
081100970619     C* Inizializzazione indice tasti funzione personalizzati
081200970619     C                   Eval      PosFun = 25
081300970619     C*
081400970619     C                   EXFMT     FMTC1
081500970619     C*
081600970619     C* azzero la posizione del cursore
081700970619     C                   Z-ADD     0             H1RIGA
081800970619     C                   Z-ADD     0             H1COLO
081900970619     C*
082000970619     C     C1NRR         IFNE      0
082100970619     C                   Z-ADD     C1NRR         WSFL
082200970619     C                   ENDIF
082300970619     C                   IF        SFLNRR > 0
082400970619     C                   Z-ADD     SFLNRR        C1RCD
082500970619     C                   ENDIF
082600970829     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
082700970829     C                   EVAL      WExf = *On
082800970619     C* inizializzazione indice opzioni personalizzate
082900970619     C                   Eval      PosOpz = 0
083000970619     C* CONTROLLO ABILITAZIONE TASTI
083100970619     C* INPUT
083200970619     C* $FC2 = Abilitazioni videata S1
083300970619     C* OUTPUT
083400970619     C* *IN97 + *IN99 Errori
083500970619     C                   MOVEA     $FC2          $FC
083600970619     C                   EXSR      CTRKEY
083700970619     C*
083800970619     C     *IN99         IFEQ      *OFF
083900970619     C*
084000970619     C                   SELECT
084100970619     C* F3=Fine
084200970619     C     $TASTO        WHENEQ    F03
084300970619     C                   EXSR      F03S1
084400970619     C* F8=Successivo
084500970619     C     $TASTO        WHENEQ    F08
084600970619     C                   EXSR      F08S1
084700970619     C* F10=Immissione
084800970619     C     $TASTO        WHENEQ    F10
084900970619     C                   EXSR      F10S1
085000970619     C* F12=Ritorno
085100970619     C     $TASTO        WHENEQ    F12
085200970619     C                   EXSR      F12S1
085300970619     C* F17=Parzializzazioni
085400970619     C     $TASTO        WHENEQ    F17
085500970619     C                   EXSR      F17S1
085600970619     C* F18=Modifica valori assunti
085700970619     C     $TASTO        WHENEQ    F18
085800970619     C                   EXSR      F18S1
085900970619     C* F23=Altre opzioni
086000970619     C     $TASTO        WHENEQ    F23
086100970619     C                   EXSR      F23S1
086200970619     C* F24=Altri tasti
086300970619     C     $TASTO        WHENEQ    F24
086400970619     C                   EXSR      F24S1
086500970619     C* ROLL-UP
086600970619     C     $TASTO        WHENEQ    ROLLUP
086700970619     C                   EXSR      ROLS1
086800970619     C* Personalizzata
086900970619     C     $PGF2(PosFun) Whenne    *BLANK
087000970619     C                   EXSR      FperS1
087100970619     C*
087200970619     C                   OTHER
087300970619     C* CONTROLLO DATI
087400970619     C                   EXSR      CTRC1
087500971015     C                   If        *In99 = *Off
087600170719     C                   EXSR      CTRS1
087700170719     C                   ENDIF
087800170719     C*
087900970619     C                   ENDSL
088000970619     C*
088100970619     C                   ENDIF
088200970619     C*
088300970619     C                   ENDSR
088400970619     C/EJECT
088500970619     C************************************************************
088600970619     C* INIZIALIZZAZIONE LISTA
088700970619     C************************************************************
088800970619     C     INZS1         BEGSR
088900970619     C* pulizia SFL
089000970619     C                   SETOFF                                         3031
089100970619     C                   WRITE     FMTC1
089200970619     C                   SETON                                          31
089300970619     C*
089400970619     C* riempimento opzioni
089500970619     C                   MOVEA     $TX1          $TX
089600970619     C                   MOVEA     $FL1          $FL
089700970619     C                   Z-ADD     0             WCOZ
089800970619     C                   Z-ADD     0             WMXO
089900970619     C*  equivale al num.di elementi della schiera $TX1
090000970619     C                   Z-add     OpzLen        WMXO
090100970619     C                   Z-ADD     $C1Op1        $L01
090200970619     C                   Z-ADD     $C1Op2        $L02
090300970619     C                   EXSR      RIEOPZ
090400970619     C                   Z-ADD     23            X
090500970619     C                   MOVE      FF23          $FC2(X)
090600970619     C                   MOVEL     WOPZ1         C1OPZ1
090700170731     C                   MOVEL     $ALLOPT       H1ALLOPT
090800970619     C                   MOVEL     WOPZ2         C1OPZ2
090900970619     C                   Z-ADD     WCOZ          C1COZ
091000970619     C*
091100970619     C* CARICAMENTO SFL A PAGINE
091200970619     C*
091300970619     C                   Z-ADD     0             S1NRR
091400970619     C                   Z-ADD     0             WMAX
091500170807     C*
091600170807     C* visualizzo il sfl aperto
091700170807     C                   Eval      C1Mode = *Off
091800970619     C*
091900970619     C* Posizionamento su file pilota
092000970619     C                   EXSR      SETANA
092100970619     C* ripristino i valori del posizionamento
092200970619     C*
092300970619     C* Lettura file pilota
092400970619     C                   EXSR      REDANA
092500970619     C*
092600970619     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
092700970619     C                   IF        $LastV = 'D1'
092800970619     C                   Z-ADD     SFLPAG        WPAG
092900970619     C                   ENDIF
093000970619     C                   EXSR      ROLS1
093100970619     C* imposto il caricamento sfl del numero massimo di righe per pagina
093200970619     C                   Z-ADD     SFLPAG        WPAG
093300970619     C*
093400970619     C                   ENDSR
093500970619     C/EJECT
093600970619     C************************************************************
093700970619     C* CARICAMENTO PAGINA LISTA
093800970619     C************************************************************
093900970619     C     ROLS1         BEGSR
094000970619     C*
094100970619     C                   SETOFF                                       32
094200970619     C                   Z-ADD     0             Y
094300970619     C                   Z-ADD     WMAX          S1NRR
094400970619     C*
094500970619     C* Leggo dal file anagrafico per caricare la lista
094600970619     C*
094700970619     C     $EndFile      DOWEQ     *OFF
094800970619     C     Y             ANDLT     WPAG
094900970619     C*
095000970619     C                   EXSR      RIES1
095100970619     C*
095200970619     C                   ADD       1             S1NRR
095300970619     C                   ADD       1             Y
095400970619     C*
095500970619     C                   WRITE     FMTS1
095600970619     C*
095700970619     C                   EXSR      REDANA
095800970619     C*
095900970619     C                   ENDDO
096000970619     C*
096100970619     C                   Z-ADD     S1NRR         WMAX                 30
096200970619     C*
096300970619     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
096400970619     C*
096500970619     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
096600970619     C                   MVR                     RESTO             3 0
096700970619     C     PAGINE        MULT      SFLPAG        C1RCD
096800970619     C     RESTO         IFGT      0
096900970619     C                   ADD       1             C1RCD
097000970619     C                   ELSE
097100970619     C                   SUB       SFLPAG        C1RCD
097200970619     C                   ADD       1             C1RCD
097300970619     C                   ENDIF
097400970619     C*
097500970619     C                   ENDSR
097600970619     C/EJECT
097700970619     C************************************************************
097800970619     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
097900970619     C************************************************************
098000970619     C     SETANA        BEGSR
098100970619     C*
098200970619     C                   SELECT
098300970619     C*
098400970619     C*
098500970619    >C* Ord.1 = chiave ordinamento 1
098600970619     C     $ORD          WHENEQ    1
098700981028     C* (se non impostata la società in parzializz. mi posiziono con *hival)
098800170731     C                   If        SocietaI82 <> *Blank
098900170731    >C                   MOVE      SocietaI82    CSFSocieta
099000981028     C                   Else
099001171016R2673X*                  MOVE      *All'9'       CSFSocieta
099002171016R2673C                   MOVE      XscSoc        CSFSocieta
099200981028     C                   EndIf
099300170731    >C     K01CSF01      SETGT     SDGCSF01
099400970619     C                   ENDSL
099500970619     C*
099600970619     C                   ENDSR
099700970619     C/EJECT
099800970619     C************************************************************
099900970619     C* LETTURA RCD ARCHIVIO PILOTA
100000970619     C************************************************************
100100970619     C     REDANA        BEGSR
100200970619     C*
100300970619     C                   MOVEL     *OFF          $EndFile
100400970619     C                   MOVEL     *OFF          $RCDOK
100500970619     C*
100600970619     C     $EndFile      DOUEQ     *ON
100700970619     C     $RCDOK        OREQ      *ON
100800970619     C*
100900970619     C                   SELECT
101000970619     C     $ORD          WHENEQ    1
101001171016R2673C*                  READP     SDGCSF01                               21
101002171016R2673C     K01CSF01      READPE    SDGCSF01                               21
101200970619     C                   ENDSL
101300970619     C*
101400970619     C                   MOVE      *IN21         $EndFile
101500970619     C*
101600970619     C     $EndFile      IFEQ      *OFF
101700970619     C                   EXSR      SELANA
101800970619     C                   ENDIF
101900970619     C*
102000970619     C                   ENDDO
102100970619     C*
102200970619     C                   MOVE      *IN21         *IN33                          SflEnd
102300970619     C*
102400970619     C                   ENDSR
102500970619     C/EJECT
102600970619     C************************************************************
102700970619     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
102800970619     C************************************************************
102900970619     C     CLCPAG        BEGSR
103000970619     C* Input :
103100970619     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
103200970619     C*          cursore
103300970619     C* - SFLPAG = numero rcd per pagina sfl
103400970619     C* Output :
103500970619     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
103600970619     C*
103700970619     C     WSFL          DIV       SFLPAG        PAGINE            4 0
103800970619     C                   MVR                     RESTO             3 0
103900970619     C     RESTO         IFGT      0
104000970619     C                   ADD       1             PAGINE
104100970619     C                   ENDIF
104200970619     C     PAGINE        MULT      SFLPAG        WPAG
104300970619     C*
104400970619     C                   ENDSR
104500970619     C/EJECT
104600970619     C************************************************************
104700970619     C* SELEZIONI SULLA RIGA LETTA
104800970619     C* impostare a *OFF il campo $RCDOK per scartare un record che
104900970619     C* non rientra nei parametri di parzializzazione
105000970619     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
105100970619     C* superiore stabilito nei parametri
105200970619     C*
105300970619     C************************************************************
105400970619     C     SELANA        BEGSR
105500970619     C*
105600970619     C                   MOVEL     *ON           $RCDOK
105700970619     C*
105800970619     C                   SELECT
105900970619     C*
106000970619     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
106100170719     C*
106200170719    >C* Trimestre
106300170719    >C     CSFTrim       WHENNE    D1Trim
106400170719     C     D1Trim        ANDNE     *zeros
106500170719    >C                   MOVEL     *OFF          $RCDOK
106600170719     C*
106700170719    >C* Anno
106800170719    >C     CSFAnno       WHENNE    D1Anno
106900170719     C     D1Anno        ANDNE     *zeros
107000170719    >C                   MOVEL     *OFF          $RCDOK
107100170801     C*
107200170801    >C* Id File
107300170801    >C     CSFIdFile     WHENNE    D1IdFile
107400170801     C     D1IdFile      ANDNE     *blank
107500170801    >C                   MOVEL     *OFF          $RCDOK
107600170804     C*
107700170804    >C* File
107800170804    >C     CSFFile       WHENNE    D1File
107900170804     C     D1File        ANDNE     *blank
108000170804    >C                   MOVEL     *OFF          $RCDOK
108100170810     C*
108200170810    >C* File raggruppante
108300170811    >C     CSFFileRgp    WHENNE    D1FileRgp
108400170811     C     D1FileRgp     ANDNE     *blank
108500170810    >C                   MOVEL     *OFF          $RCDOK
108600170804     C*
108700170804    >C     D1Cf          WHENNE    *Blanks
108800170804    >C     CSFCf         ANDNE     D1Cf
108900170804    >C                   MOVEL     *OFF          $RCDOK
109000170719     C*
109100170719     C                   ENDSL
109200970619     C*
109300970619     C                   IF        $RcdOk = *off
109400970619     C*
109500970619     C                   SELECT
109600970623     C*
109700970623     C*
109800970619     C                   ENDSL
109900970619     C*
110000970619     C                   ENDIF
110100970619     C*
110200970619     C                   ENDSR
110300970619     C/EJECT
110400970619     C************************************************************
110500970619     C* RIEMPIMENTO RIGA LISTA
110600970619     C************************************************************
110700970619     C     RIES1         BEGSR
110800970619     C*
110900970619     C                   CLEAR                   FMTS1
111000970619     C*
111100170731     C                   MOVEL     CSFAnno       S1Anno
111200170731     C                   MOVEL     CSFTrim       S1Trim
111300170719     C                   MOVEL     CSFFile       S1File
111400170810     C                   MOVEL     CSFFileRgp    H1FileRgp
111500170807     C                   MOVEL     CSFIdFile     H1IdFile
111600170719     C                   MOVEL     CSFNote       S1Descr
111700170807     C                   MOVEL     CSFCf         S1Cf
111800970619     C*
111900170807     C                   eval      H1Ord      = *off
112000170807     C                   eval      H1Ret      = *off
112100170807     C                   eval      H1Ann      = *off
112200170807     C                   eval      H1In16     = *off
112300170719     C*
112400170719     C                   Select
112500170719     C                   When      CSFTpRec   = 'O'
112600170807     C                   eval      H1Ord      = *on
112700170719     C                   When      CSFTpRec   = 'R'
112800170807     C                   eval      H1Ret      = *on
112900170719     C                   When      CSFTpRec   = 'A'
113000170807     C                   eval      H1Ann      = *on
113100170719     C                   EndSl
113200170719     C*
113300170807     C                   if        CSFIdFile <> *blank
113400170807     C                   eval      H1In16     = *on
113500170807     C                   endif
113600170807     C*
113700970619     C* imposto quegli attributi del sfl che sono modificabili
113800970619     C* al richiamo del determinato pgm
113900970619     C                   EXSR      ATRS1
114000970619     C*
114100970619     C                   ENDSR
114200970619     C/EJECT
114300970619     C************************************************************
114400970619     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
114500970619     C************************************************************
114600970619     C     ATRS1         BEGSR
114700170807     C*
114800170807     c                   eval      *in41 = H1Ord
114900170807     c                   eval      *in43 = H1Ret
115000170807     c                   eval      *in42 = H1Ann
115100170807     c                   eval      *in16 = H1In16
115200170807     C*
115300970619     C                   ENDSR
115400970619     C/EJECT
115500970619     C************************************************************
115600970619     C* GESTIONE F08 VIDEO S1
115700970619     C************************************************************
115800970619     C     F08S1         BEGSR
115900970619     C*
116000970619     C                   MOVE      *ON           $FINE
116100170719    >C                   MOVE      '0'           RETI82
116200970619     C*
116300970619     C                   ENDSR
116400970619     C/EJECT
116500970619     C************************************************************
116600970619     C* GESTIONE F03 VIDEO S1
116700970619     C************************************************************
116800970619     C     F03S1         BEGSR
116900970619     C*
117000970619     C                   MOVE      *ON           $FINE
117100170719    >C                   MOVE      '1'           RETI82
117200970619     C* fine programma
117300970619     C                   EXSR      ENDPGM
117400970619     C*
117500970619     C                   ENDSR
117600970619     C/EJECT
117700970619     C************************************************************
117800970619     C* GESTIONE F10 VIDEO S1
117900970619     C* AGGIUNTA RECORD
118000970619     C************************************************************
118100970619     C     F10S1         BEGSR
118200970619     C*
118300170719    >C                   RESET                   SDGSI83DS
118400170719    >C                   MOVE      xscsoc        SocietaI83
118500170719    >C                   MOVEL     '01'          OPZI83
118600170719    >C     CMTI82        IFEQ      '1'
118700170719    >C                   MOVEL     '1'           CMTI83
118800970619     C                   ELSE
118900170719    >C                   MOVEL     '0'           CMTI83
119000970619     C                   ENDIF
119100970829     C* passo eventuale EXFMT per chiamata a pgm di sola finestra
119200170719     C                   EVAL      ExfI83 = WExf
119300170719    >C                   MOVEL     SDGSI83DS     KPJBU
119400970619     C*
119500170719    >C                   CALL      'SDGSI83R'
119600970619     C                   PARM                    KPJBA
119700170719    >C                   MOVEL     KPJBU         SDGSI83DS
119800970619     C* ritorno da PGM gestione
119900170719    >C                   EXSR      RTNI83
120000970619     C     WINZS1        IFEQ      *ON
120100970619     C                   MOVE      *ON           $INZS1
120200970619     C* se esistevano già righe sul sfl
120300970619     C* calcolo pagina a cui deve posizionarsi
120400970619     C     WSFL          IFGT      0
120500970619     C                   EXSR      CLCPAG
120600970619     C* altrimenti carico solo la 1a pagina
120700970619     C                   ELSE
120800970619     C                   Z-ADD     SFLPAG        WPAG
120900970619     C                   ENDIF
121000970619     C                   ENDIF
121100970619     C*
121200970619     C                   MOVE      *HIVAL        $LASTV
121300970619     C*
121400970619     C                   ENDSR
121500970619     C/EJECT
121600970619     C************************************************************
121700970619     C* GESTIONE F12 VIDEO S1
121800970619     C************************************************************
121900970619     C     F12S1         BEGSR
122000970619     C*
122100970619     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
122200970619     C     $LOP          IFEQ      1
122300970619     C                   MOVE      'D1'          $GEST
122400970619     C*  se la 1a videata è questa, per F12 torno a menu
122500970619     C                   ELSE
122600970619     C                   MOVE      *ON           $FINE
122700170719    >C                   MOVE      '2'           RETI82
122800970619     C                   ENDIF
122900970619     C*
123000970619     C                   ENDSR
123100970619     C/EJECT
123200970619     C************************************************************
123300970619     C* GESTIONE F17 VIDEO S1
123400970619     C************************************************************
123500970619     C     F17S1         BEGSR
123600970619     C*
123700970619     C                   MOVE      'D1'          $GEST
123800970619     C*
123900970619     C                   ENDSR
124000970619     C/EJECT
124100970619     C************************************************************
124200970619     C* GESTIONE F18 VIDEO S1
124300970619     C************************************************************
124400970619     C     F18S1         BEGSR
124500970619     C*
124600970619     C                   MOVE      'D2'          $GEST
124700970619     C                   MOVEL     *ON           $INZD2
124800970619     C*
124900970619     C                   ENDSR
125000970619     C/EJECT
125100970619     C************************************************************
125200970619     C* GESTIONE F23 VIDEO S1
125300970619     C************************************************************
125400970619     C     F23S1         BEGSR
125500970619     C*
125600970619     C                   Z-ADD     $C1Op1        $L01
125700970619     C                   Z-ADD     $C1Op2        $L02
125800970619     C                   EXSR      RIEOPZ
125900970619     C                   Z-ADD     23            X
126000970619     C                   MOVE      FF23          $FC2(X)
126100970619     C                   MOVEL     WOPZ1         C1OPZ1
126200170731     C                   MOVEL     $ALLOPT       H1ALLOPT
126300970619     C                   MOVEL     WOPZ2         C1OPZ2
126400970619     C                   Z-ADD     WCOZ          C1COZ
126500970619     C*
126600970619     C                   ENDSR
126700970619     C/EJECT
126800970619     C************************************************************
126900970619     C* GESTIONE F24 VIDEO S1
127000970619     C************************************************************
127100970619     C     F24S1         BEGSR
127200970619     C*
127300970619     C                   MOVEA     $FX2          $FX
127400970619     C                   MOVEA     $FC2          $FC
127500970619     C                   MOVEA     $FR2          $FR
127600970619     C                   Z-ADD     $ULKS1        $ULK
127700970619     C                   Z-ADD     $S1L01        $L01
127800970619     C                   Z-ADD     $S1L02        $L02
127900970619     C                   EXSR      RIEKEY
128000970619     C                   Z-ADD     $ULK          $ULKS1            3 0
128100970619     C                   MOVEA     $FC           $FC2
128200970619     C                   MOVEL     $KEY1         Z2KE1
128300170731     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
128400970619     C                   MOVEL     $KEY2         Z2KE2
128500970619     C                   WRITE     FMTZ2
128600970619     C*
128700970619     C                   ENDSR
128800970619     C/EJECT
128900970619     C************************************************************
129000970619     C* CONTROLLO TESTATA LISTA
129100970619     C************************************************************
129200970619     C     CTRC1         BEGSR
129300970619     C*
129400970619     C                   MOVE      *OFF          *IN99
129500970619     C*
129600970619     C*
129700970619     C                   ENDSR
129800970619     C/EJECT
129900970619     C************************************************************
130000970619     C* CONTROLLO OPZIONI LISTA
130100970619     C************************************************************
130200970619     C     CTRS1         BEGSR
130300970619     C*
130400970619     C                   MOVEL     *OFF          $ESCI
130500970619     C                   SETOFF                                       99
130600970619     C                   Z-ADD     0             S1OPZ
130700970619     C*
130800970619     C* Leggo il sfl solo se ci sono rcd
130900970619   B1C     WMAX          IFGT      0
131000970619     C                   READC     FMTS1                                  21
131100970619     C*
131200970619     C* esce se fine sfl o errore che richiede l'uscita
131300970619   B2C     *IN21         DOWEQ     *OFF
131400970619     C     $ESCI         ANDEQ     *OFF
131500970619     C                   Z-ADD     S1NRR         C1RCD
131600970619     C* ctrl su riga
131700970619     C                   EXSR      RECS1
131800970619     C* gestione opzioni
131900970619   B3C     S1OPZ         IFNE      0
132000970619     C     *IN99         ANDEQ     *OFF
132100970619     C                   EXSR      OPZS1
132200970619   E3C                   ENDIF
132300970619     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
132400970619   B3C     *IN99         IFEQ      *ON
132500970619     C                   MOVE      *ON           *IN32
132600970619   E3C                   ENDIF
132700970619     C*
132800970619     C* imposto quegli attributi del sfl che sono modificabili
132900970619     C* al richiamo del determinato pgm
133000970619     C                   EXSR      ATRS1
133100970619     C*
133200970619     C                   Z-ADD     0             S1OPZ
133300970619     C*
133400970619     C                   UPDATE    FMTS1
133500970619     C                   MOVE      *OFF          *IN32
133600970619     C*
133700970619     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
133800970619   B3C     $ESCI         IFEQ      *OFF
133900970619     C                   READC     FMTS1                                  21
134000970619     C* a fine ciclo ripristino il flag richiesta iniz.sfl
134100970619   B4C     *IN21         IFEQ      *ON
134200970619     C                   MOVE      WINZS1        $INZS1
134300970619     C* calcolo pagina a cui deve posizionarsi
134400970619     C     WSFL          IFGT      0
134500970619     C                   EXSR      CLCPAG
134600970619     C                   ELSE
134700970619     C                   Z-ADD     SFLPAG        WPAG
134800970619     C                   ENDIF
134900970619   E4C                   ENDIF
135000970619   E4C                   ENDIF
135100970619     C*
135200970619   E2C                   ENDDO
135300970619     C*
135400970619   E1C                   ENDIF
135500970619     C*
135600970619     C                   ENDSR
135700970619     C/EJECT
135800970619     C************************************************************
135900970619     C* CONTROLLO CAMPI I/O RIGA LISTA
136000970619     C************************************************************
136100970619     C     RECS1         BEGSR
136200970619     C*
136300970619     C* reset indicatori DSPATR
136400970619     C                   MOVE      *ALL'0'       IN4049           10
136500970619     C                   MOVEA     IN4049        *IN(40)
136600970619     C*
136700970619     C* Controllo se Utente Autorizzato alla scelta
136800970619     C     S1OPZ         IFNE      0
136900970619     C                   Z-ADD     1             X
137000970619     C     S1OPZ         LOOKUP    $OP1(X)                                21
137100970619     C* mi salvo l'indice dell' opzione
137200970619     C   21              Z-add     X             PosOpz            2 0
137300970619     C     *IN21         IFEQ      *OFF
137400970619     C     $FL1(X)       OREQ      'N'
137500970619     C                   MOVE      *ON           $ESCI
137600970619     C                   SETON                                        409199
137700970619     C                   ENDIF
137800970619     C                   ENDIF
137900970619     C*
138000970619     C                   ENDSR
138100970619     C/EJECT
138200970619     C************************************************************
138300970619     C* GESTIONE OPZIONI LISTA
138400970619     C************************************************************
138500970619     C     OPZS1         BEGSR
138600970619     C*
138700970619     C                   Z-ADD     RIRI          CURR
138800970619     C                   Z-ADD     COCO          CURC
138900970619     C*
139000970619     C                   SELECT
139100970619     C*
139200970619     C* se opz=Selezione
139300970619     C* passo la chiave al pgm chiamante
139400970619     C     S1OPZ         WHENEQ    1
139500170719    >C                   MOVE      S1OPZ         OPZI82
139600170719     C* passare nei campi appositi della DS SDGSI82DSil valore
139700970619     C* della chiave della VL univoca (se c'è)
139800170801    >C                   MOVE      XscSoc        SocietaI82
139900170807    >C                   MOVEL     S1File        FileI82
140000170807    >C                   MOVEL     H1IdFile      IdFileI82
140100970619     C                   MOVE      *ON           $ESCI
140200970619     C                   MOVE      *ON           $FINE
140300970619     C*
140400970619     C                   MOVE      *HIVAL        $LASTV
140500981028     C*
140600981028     C* se opz=visualizza dati o stampa dati
140700981028     C* passo la chiave al pgm chiamante
140800170719     C**** S1OPZ         WHENEQ    15
140900170719     C**** S1OPZ         OREQ      16
141000981028     C*
141100170719     C****               IF        S1Opz = 15
141200170719     C****               MOVEL     'D'           $Output
141300170719     C****               ELSE
141400170719     C****               MOVEL     'P'           $Output
141500170719     C****               ENDIF
141600981028     C*
141700170719     C****               Call      'X65RUNQRY'
141800170719     C****               PARM      S1File        $File
141900170719     C**** ????          PARM      *Blank        $Membro
142000170719     C****               PARM                    $Output
142100170719     C****               PARM                    $Errore
142200981028     C*
142300170719     C****               If        $Errore = '1'
142400170719     C****               SetOn                                            93
142500170719     C****               EndIf
142600981030     C*
142700981028     C                   MOVE      *HIVAL        $LASTV
142800170810     C*
142900170810     C* Chiamata alla lista movimenti
143000170810     C     S1OPZ         WHENEQ    15
143100170810    >C                   RESET                   SDGSI73DS
143200170810    >C                   MOVE      xscsoc        SocietaI73
143300170810    >C                   MOVEL     S1OPZ         OPZI73
143400170810    >C     CMTI82        IFEQ      '1'
143500170810    >C                   MOVEL     '1'           CMTI73
143600170810     C                   ELSE
143700170810    >C                   MOVEL     '0'           CMTI73
143800170810     C                   ENDIF
143900170810     C* passare nei campi appositi della DS SDGSI73DS il valore
144000170810     C* della chiave
144100170810    >C                   MOVEL     S1Anno        AnnoI73
144200170810    >C                   MOVEL     S1Trim        TrimI73
144300170810    >C                   MOVEL     S1CF          CFI73
144400170810    >C                   MOVEL     S1File        FileI73
144500170810    >C                   MOVEL     H1IdFile      IdFileI73
144600170810     C                   Select
144700170810     C                   When      H1Ann = *on
144800170810     C                   Movel     'A'           TpRecI73
144900170810     C                   When      H1Ret = *on
145000170810     C                   Movel     'R'           TpRecI73
145100170810     C                   When      H1Ord = *on
145200170810     C                   Movel     'O'           TpRecI73
145300170810     C                   EndSL
145400170810     C                   Movel     'T'           TpComI73
145500170810     C*
145600170810     C                   Movel     '1'           AnnI73
145700170810     C*
145800170810    >C                   MOVEL     SDGSI73DS     KPJBU
145900170810    >C                   CALL      'SDGSI73R'
146000170810     C                   PARM                    KPJBA
146100170810    >C                   PARM                    AUTDS
146200170810     C*
146300170810    >C                   MOVEL     KPJBU         SDGSI73DS
146400170810     C*
146500170810     C* ritorno da PGM gestione
146600170810    >C                   EXSR      RTNI73
146700170810     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
146800170810     C     *IN99         IFEQ      *ON
146900170810     C                   SETON                                        40
147000170810     C                   ENDIF
147100170810     C*
147200170810     C                   MOVE      *HIVAL        $LASTV
147300970619     C*
147400970619     C* se selezionata opzione personalizzata, richiamo il programma
147500970619     C* corrispondente
147600970619     C     $PGO1(PosOpz) Whenne    *Blank
147700170719    >C                   RESET                   SDGSI83DS
147800170719    >C                   MOVE      xscsoc        SocietaI83
147900170719    >C                   MOVEL     S1OPZ         OPZI83
148000170719    >C     CMTI82        IFEQ      '1'
148100170719    >C                   MOVEL     '1'           CMTI83
148200970619     C                   ELSE
148300170719    >C                   MOVEL     '0'           CMTI83
148400970619     C                   ENDIF
148500170719     C* passare nei campi appositi della DS SDGSI83DS il valore
148600970619     C* della chiave della VL univoca
148700170719    >C                   MOVEL     S1File        FileI83
148800970619     C*
148900170719    >C                   MOVEL     SDGSI83DS     KPJBU
149000970619     C                   CALL      $PGO1(PosOpz)                        22
149100970619     C                   PARM                    KPJBA
149200970619     C                   PARM                    NOMPGM
149300970619     C*
149400170719    >C                   MOVEL     KPJBU         SDGSI83DS
149500970619     C* ritorno da PGM
149600170719     C                   EXSR      RTNI83
149700970619     C*
149800970619     C* se altre opzioni
149900970619     C* richiamo pgm gestione anagrafica
150000970619     C                   OTHER
150100970619     C*
150200170719    >C                   RESET                   SDGSI83DS
150300170719    >C                   MOVE      xscsoc        SocietaI83
150400170719    >C                   MOVEL     S1OPZ         OPZI83
150500170719    >C     CMTI82        IFEQ      '1'
150600170719    >C                   MOVEL     '1'           CMTI83
150700970619     C                   ELSE
150800170719    >C                   MOVEL     '0'           CMTI83
150900970619     C                   ENDIF
151000170719     C* passare nei campi appositi della DS SDGSI83DS il valore
151100970619     C* della chiave della VL univoca
151200170719    >C                   MOVEL     S1File        FileI83
151300970829     C* passo eventuale EXFMT per chiamata a pgm di sola finestra
151400170719     C                   EVAL      ExfI83 = WExf
151500970619     C*
151600170719    >C                   MOVEL     SDGSI83DS     KPJBU
151700170719    >C                   CALL      'SDGSI83R'
151800970619     C                   PARM                    KPJBA
151900970620    >C                   PARM                    AUTDS
152000970619     C*
152100170719    >C                   MOVEL     KPJBU         SDGSI83DS
152200970619     C*
152300970619     C* ritorno da PGM gestione
152400170719    >C                   EXSR      RTNI83
152500970619     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
152600970619     C     *IN99         IFEQ      *ON
152700970619     C                   SETON                                        40
152800970619     C                   ENDIF
152900970619     C*
153000970619     C                   MOVE      *HIVAL        $LASTV
153100970619     C                   ENDSL
153200970619     C*
153300970619     C                   ENDSR
153400970619     C/EJECT
153500970619     C************************************************************
153600970619     C* GESTIONE RITORNO DA PGM DI GESTIONE
153700970619     C************************************************************
153800170719    >C     RTNI83        BEGSR
153900970619     C*
154000970619     C* modo di ritorno
154100970619     C*
154200970619     C                   SELECT
154300970619     C* questi modi di utilizzo dei valori di ritorno dal
154400970619     C* pgm di manutenzione rcd di anagrafica sono delle
154500970619     C* proposte, normalmente sempre valide, ma modificabili
154600970619     C* per situazioni particolari
154700970619     C* 1 = F3
154800170719    >C     RETI83        WHENEQ    '1'
154900970619     C                   MOVE      *ON           $ESCI                          sospese READC
155000970619     C                   MOVE      *ON           $FINE                          fine pgm
155100170719     C                   MOVE      '1'           RETI82
155200970619     C* 2 = F12
155300170719    >C     RETI83        WHENEQ    '2'
155400970619     C* se il pgm di manutenzione ha prima una guida, è necessario anche
155500170719     c* questo test, altrimenti prevale il retI83 e il sfl non viene
155600970619     C* ricaricato
155700170719     C     OPRI83        ANDNE     '1'
155800970619     C                   MOVE      *ON           $ESCI                          sospese READC
155900970619     C*
156000970619     C                   ENDSL
156100970619     C*
156200970619     C* operazione eseguite dal pgm chiamato
156300970619     C*
156400170719    >C                   MOVE      OPRI83        WOPR                           avverto pgm chiamant
156500970619     C                   SELECT
156600970619     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
156700170719    >C     OPRI83        WHENEQ    '1'
156800170807     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
156900970619     C*
157000970619     C* se l'opzione con cui è stato chiamato non è la modifica
157100970619     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
157200170719    >C     OPZI83        IFNE      '02'
157300970619     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
157400970619     C* se l'opzione con cui è stato chiamato è la modifica
157500970619     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
157600970619     C                   ELSE
157700970619     C                   EXSR      AGGS1
157800970619     C                   ENDIF
157900970619     C*
158000970619     C                   ENDSL
158100970619     C*
158200970619     C* funzione non eseguibile per errore :
158300970619     C*
158400970619     C                   SELECT
158500970619     C* 1 = funzione richiamata chiusa in errore
158600170719    >C     ERRI83        WHENEQ    '1'
158700970619     C                   MOVE      *ON           $ESCI                          sospese READC
158800970619     C                   SETON                                        9399      emiss.SFLMSGID
158900970619     C* 2 = funzione richiamata senza la dovuta autorizzazione
159000170719    >C     ERRI83        WHENEQ    '2'
159100970619     C                   MOVE      *ON           $ESCI                          sospese READC
159200970619     C                   SETON                                        9499      emiss.SFLMSGID
159300970619     C*
159400970619     C                   ENDSL
159500970829     C*
159600970829     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
159700970829     C* di sola finestra
159800970829     C                   EVAL       WEXF = *Off
159900970619     C*
160000970619     C                   ENDSR
160100170810     C/EJECT
160200170810     C************************************************************
160300170810     C* GESTIONE RITORNO DA PGM DI Visualizzazione movimenti
160400170810     C************************************************************
160500170810    >C     RTNI73        BEGSR
160600170810     C*
160700170810     C* modo di ritorno
160800170810     C*
160900170810     C                   SELECT
161000170810     C* questi modi di utilizzo dei valori di ritorno dal
161100170810     C* pgm di manutenzione rcd di anagrafica sono delle
161200170810     C* proposte, normalmente sempre valide, ma modificabili
161300170810     C* per situazioni particolari
161400170810     C* 1 = F3
161500170810    >C     RETI73        WHENEQ    '1'
161600170810     C                   MOVE      *ON           $ESCI                          sospese READC
161700170810     C                   MOVE      *ON           $FINE                          fine pgm
161800170810     C                   MOVE      '1'           RETI82
161900170810     C* 2 = F12
162000170810    >C     RETI73        WHENEQ    '2'
162100170810     C* se il pgm di manutenzione ha prima una guida, è necessario anche
162200170810     c* questo test, altrimenti prevale il retI83 e il sfl non viene
162300170810     C* ricaricato
162400170810     C     OPRI73        ANDNE     '1'
162500170810     C                   MOVE      *ON           $ESCI                          sospese READC
162600170810     C*
162700170810     C                   ENDSL
162800170810     C*
162900170810     C* operazione eseguite dal pgm chiamato
163000170810     C*
163100170810    >C                   MOVE      OPRI73        WOPR                           avverto pgm chiamant
163200170810     C                   SELECT
163300170810     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
163400170810    >C     OPRI73        WHENEQ    '1'
163500170810     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
163600170810     C*
163700170810     C* se l'opzione con cui è stato chiamato non è la modifica
163800170810     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
163900170810    >C     OPZI73        IFNE      '02'
164000170810     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
164100170810     C* se l'opzione con cui è stato chiamato è la modifica
164200170810     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
164300170810     C                   ELSE
164400170810     C                   EXSR      AGGS1
164500170810     C                   ENDIF
164600170810     C*
164700170810     C                   ENDSL
164800170810     C*
164900170810     C* funzione non eseguibile per errore :
165000170810     C*
165100170810     C                   SELECT
165200170810     C* 1 = funzione richiamata chiusa in errore
165300170810    >C     ERRI73        WHENEQ    '1'
165400170810     C                   MOVE      *ON           $ESCI                          sospese READC
165500170810     C                   SETON                                        9399      emiss.SFLMSGID
165600170810     C* 2 = funzione richiamata senza la dovuta autorizzazione
165700170810    >C     ERRI73        WHENEQ    '2'
165800170810     C                   MOVE      *ON           $ESCI                          sospese READC
165900170810     C                   SETON                                        9499      emiss.SFLMSGID
166000170810     C*
166100170810     C                   ENDSL
166200170810     C*
166300170810     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
166400170810     C* di sola finestra
166500170810     C                   EVAL       WEXF = *Off
166600170810     C*
166700170810     C                   ENDSR
166800970619     C/EJECT
166900970619     C************************************************************
167000970619     C* AGGIORNAMENTO RIGA LISTA
167100970619     C************************************************************
167200970619     C     AGGS1         BEGSR
167300970619     C*
167400970619     C* riempio i campi del sfl che sono stati ripassati dal pgm
167500970619     C* di manutenzione dopo una opzione 02
167600970619     C*
167700170719    >C*****              MOVEL     DESI83        S1DES
167800970619     C*
167900970619     C                   ENDSR
168000970619     C/EJECT
168100970619     C************************************************************
168200970619     C* GESTIONE VALORI ASSUNTI
168300970619     C************************************************************
168400970619     C     GESD2         BEGSR
168500970619     C*
168600970619   B1C     $INZD2        IFEQ      *ON
168700970619     C                   EXSR      INZD2
168800970619     C                   MOVE      *OFF          $INZD2
168900970619   E1C                   ENDIF
169000970619     C*
169100970619   B1C     $LASTV        IFNE      'D2'
169200970619     C                   EXSR      GESPIE
169300970619   B1C     $LASTV        IFNE      *HIVAL
169400970619     C                   MOVE      $LASTV        $LASTG
169500970619     C                   ENDIF
169600970619     C                   MOVE      'D2'          $LASTV
169700970619   E1C                   ENDIF
169800970619     C*
169900970619     C* se esistono errori sulla videata
170000970619     C* emetto la write del formato a indicatori spenti per vedere
170100970619     C* le eventuali decodifiche
170200970619   B1C     *IN99         IFEQ      *ON
170300970619     C                   MOVEA     *IN           WIN
170400970619     C                   MOVE      *ALL'0'       IN5098           49
170500970619     C                   MOVEA     IN5098        *IN(50)
170600970619     C                   WRITE     FMTD2
170700970619     C                   MOVEA     WIN           *IN
170800970619   E1C                   ENDIF
170900970619     C*
171000970619     C                   EXFMT     FMTD2
171100970619     C*
171200970619     C* azzero la posizione del cursore
171300970619     C                   Z-ADD     0             H1RIGA
171400970619     C                   Z-ADD     0             H1COLO
171500970619     C*
171600970619     C* CONTROLLO ABILITAZIONE TASTI
171700970619     C* INPUT
171800970619     C* $FC3 = Abilitazioni videata D2
171900970619     C* OUTPUT
172000970619     C* *IN97 + *IN99 Errori
172100970619     C                   MOVEA     $FC3          $FC
172200970619     C                   EXSR      CTRKEY
172300970619     C*
172400970619   B1C     *IN99         IFEQ      *OFF
172500970619     C*
172600970619   B2C                   SELECT
172700970619     C* F3=Fine
172800970619     C     $TASTO        WHENEQ    F03
172900970619     C                   EXSR      F03D2
173000970619     C* F12=Ritorno
173100970619     C     $TASTO        WHENEQ    F12
173200970619     C                   EXSR      F12D2
173300970619     C* F24=Altri tasti
173400970619     C     $TASTO        WHENEQ    F24
173500970619     C                   EXSR      F24D2
173600970619     C*
173700970619     C                   OTHER
173800970619     C                   EXSR      CTRD2
173900970619   B3C     *IN99         IFEQ      *OFF
174000970619     C                   MOVE      $LASTG        $GEST
174100970619   B4C                   SELECT
174200970619     C     $LASTG        WHENEQ    'S1'
174300970619     C* se è stato modificato l'ordinamento di default, ricarico la lista
174400970619   B5C     $ORD          IFNE      D2ORD
174500970619     C                   MOVE      *ON           $INZS1
174600970619   E5C                   ENDIF
174700970619     C     $LASTG        WHENEQ    'D1'
174800970619     C* per ora niente
174900970619   E4C                   ENDSL
175000970619     C* definisco i nuovi valori di default
175100970619     C                   Z-ADD     D2LOP         $LOP
175200970619     C                   Z-ADD     D2ORD         $ORD
175300970619     C*
175400970619     C* aggiorno valori assunti
175500970619     C* se richiesto il salvataggio
175600970619   B4C     D2BSV         IFEQ      1
175700970619     C                   EXSR      AGGMPE
175800970619   E4C                   ENDIF
175900970619     C*
176000970619   E3C                   ENDIF
176100970619     C*
176200970619   E2C                   ENDSL
176300970619     C*
176400970619   E1C                   ENDIF
176500970619     C*
176600970619     C                   ENDSR
176700970619     C/EJECT
176800970619     C************************************************************
176900970619     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
177000970619     C************************************************************
177100970619     C     INZD2         BEGSR
177200970619     C*
177300970619     C                   CLEAR                   FMTD2
177400970619     C*
177500970619     C                   MOVEL     $LOP          D2LOP
177600970619     C                   MOVE      $ORD          D2ORD
177700970619     C*
177800970619     C                   ENDSR
177900970619     C/EJECT
178000970619     C************************************************************
178100970619     C* GESTIONE F03 VIDEO D2
178200970619     C************************************************************
178300970619     C     F03D2         BEGSR
178400970619     C*
178500970619     C                   MOVE      *ON           $FINE
178600970619     C* fine programma
178700970619     C                   EXSR      ENDPGM
178800970619     C*
178900970619     C                   ENDSR
179000970619     C/EJECT
179100970619     C************************************************************
179200970619     C* GESTIONE F12 VIDEO D2
179300970619     C************************************************************
179400970619     C     F12D2         BEGSR
179500970619     C*
179600970619     C                   MOVE      $LASTG        $GEST
179700970619     C*
179800970619     C                   ENDSR
179900970619     C/EJECT
180000970619     C************************************************************
180100970619     C* GESTIONE F24 VIDEO D2
180200970619     C************************************************************
180300970619     C     F24D2         BEGSR
180400970619     C*
180500970619     C                   MOVEA     $FX3          $FX
180600970619     C                   MOVEA     $FC3          $FC
180700970619     C                   MOVEA     $FR3          $FR
180800970619     C                   Z-ADD     $ULKD2        $ULK
180900970619     C                   Z-ADD     $D2L01        $L01
181000970619     C                   Z-ADD     $D2L02        $L02
181100970619     C                   EXSR      RIEKEY
181200970619     C                   Z-ADD     $ULK          $ULKD2            3 0
181300970619     C                   MOVEA     $FC           $FC3
181400970619     C                   MOVEL     $KEY1         Z3KE1
181500170731     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
181600970619     C                   WRITE     FMTZ3
181700970619     C*
181800970619     C                   ENDSR
181900970619     C/EJECT
182000970619     C************************************************************
182100970619     C* CONTROLLO VIDEATA VALORI ASSUNTI
182200970619     C************************************************************
182300970619     C     CTRD2         BEGSR
182400970619     C*
182500970619     C                   SETOFF                                       99
182600970619     C*
182700970619     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
182800970619     C     D2LOP         IFEQ      0
182900970619     C                   SETON                                          5099
183000970619     C                   ENDIF
183100970619     C*
183200970619     C* OBBLIGATORIO VALORE PER ORDINAMENTO
183300970619     C     D2ORD         IFEQ      0
183400970619     C                   SETON                                          5199
183500970619     C                   ENDIF
183600970619     C*
183700970619     C                   ENDSR
183800970619     C/EJECT
183900970619     C************************************************************
184000970619     C* CONTROLLO ABILITAZIONE TASTI
184100970619     C* INPUT
184200970619     C* $TASTO =Tasto funzione premuto
184300970619     C* $FC = Tasti abilitati
184400970619     C* OUTPUT
184500970619     C* IN99 = ERrore
184600970619     C************************************************************
184700970619     C     CTRKEY        BEGSR
184800970619     C*
184900970619     C                   SETOFF                                       99
185000970619     C*
185100970619     C                   SELECT
185200970619     C     $TASTO        WHENEQ    F01
185300970619     C                   z-add     01            posfun
185400970619     C     $FC(1)        IFEQ      '0'
185500970619     C                   SETON                                        9997
185600970619     C                   ENDIF
185700970619     C     $TASTO        WHENEQ    F02
185800970619     C                   z-add     02            posfun
185900970619     C     $FC(2)        IFEQ      '0'
186000970619     C                   SETON                                        9997
186100970619     C                   ENDIF
186200970619     C     $TASTO        WHENEQ    F03
186300970619     C                   z-add     03            posfun
186400970619     C     $FC(3)        IFEQ      '0'
186500970619     C                   SETON                                        9997
186600970619     C                   ENDIF
186700970619     C     $TASTO        WHENEQ    F04
186800970619     C                   z-add     04            posfun
186900970619     C     $FC(4)        IFEQ      '0'
187000970619     C                   SETON                                        9997
187100970619     C                   ENDIF
187200970619     C     $TASTO        WHENEQ    F05
187300970619     C                   z-add     05            posfun
187400970619     C     $FC(5)        IFEQ      '0'
187500970619     C                   SETON                                        9997
187600970619     C                   ENDIF
187700970619     C     $TASTO        WHENEQ    F06
187800970619     C                   z-add     06            posfun
187900970619     C     $FC(6)        IFEQ      '0'
188000970619     C                   SETON                                        9997
188100970619     C                   ENDIF
188200970619     C     $TASTO        WHENEQ    F07
188300970619     C                   z-add     07            posfun
188400970619     C     $FC(7)        IFEQ      '0'
188500970619     C                   SETON                                        9997
188600970619     C                   ENDIF
188700970619     C     $TASTO        WHENEQ    F08
188800970619     C                   z-add     08            posfun
188900970619     C     $FC(8)        IFEQ      '0'
189000970619     C                   SETON                                        9997
189100970619     C                   ENDIF
189200970619     C     $TASTO        WHENEQ    F09
189300970619     C                   z-add     09            posfun
189400970619     C     $FC(09)       IFEQ      '0'
189500970619     C                   SETON                                        9997
189600970619     C                   ENDIF
189700970619     C     $TASTO        WHENEQ    F10
189800970619     C                   z-add     10            posfun
189900970619     C     $FC(10)       IFEQ      '0'
190000970619     C                   SETON                                        9997
190100970619     C                   ENDIF
190200970619     C     $TASTO        WHENEQ    F11
190300970619     C                   z-add     11            posfun
190400970619     C     $FC(11)       IFEQ      '0'
190500970619     C                   SETON                                        9997
190600970619     C                   ENDIF
190700970619     C     $TASTO        WHENEQ    F12
190800970619     C                   z-add     12            posfun
190900970619     C     $FC(12)       IFEQ      '0'
191000970619     C                   SETON                                        9997
191100970619     C                   ENDIF
191200970619     C     $TASTO        WHENEQ    F13
191300970619     C                   z-add     13            posfun
191400970619     C     $FC(13)       IFEQ      '0'
191500970619     C                   SETON                                        9997
191600970619     C                   ENDIF
191700970619     C     $TASTO        WHENEQ    F14
191800970619     C                   z-add     14            posfun
191900970619     C     $FC(14)       IFEQ      '0'
192000970619     C                   SETON                                        9997
192100970619     C                   ENDIF
192200970619     C     $TASTO        WHENEQ    F15
192300970619     C                   z-add     15            posfun
192400970619     C     $FC(15)       IFEQ      '0'
192500970619     C                   SETON                                        9997
192600970619     C                   ENDIF
192700970619     C     $TASTO        WHENEQ    F16
192800970619     C                   z-add     16            posfun
192900970619     C     $FC(16)       IFEQ      '0'
193000970619     C                   SETON                                        9997
193100970619     C                   ENDIF
193200970619     C     $TASTO        WHENEQ    F17
193300970619     C                   z-add     17            posfun
193400970619     C     $FC(17)       IFEQ      '0'
193500970619     C                   SETON                                        9997
193600970619     C                   ENDIF
193700970619     C     $TASTO        WHENEQ    F18
193800970619     C                   z-add     18            posfun
193900970619     C     $FC(18)       IFEQ      '0'
194000970619     C                   SETON                                        9997
194100970619     C                   ENDIF
194200970619     C     $TASTO        WHENEQ    F19
194300970619     C                   z-add     19            posfun
194400970619     C     $FC(19)       IFEQ      '0'
194500970619     C                   SETON                                        9997
194600970619     C                   ENDIF
194700970619     C     $TASTO        WHENEQ    F20
194800970619     C                   z-add     20            posfun
194900970619     C     $FC(20)       IFEQ      '0'
195000970619     C                   SETON                                        9997
195100970619     C                   ENDIF
195200970619     C     $TASTO        WHENEQ    F21
195300970619     C                   z-add     21            posfun
195400970619     C     $FC(21)       IFEQ      '0'
195500970619     C                   SETON                                        9997
195600970619     C                   ENDIF
195700970619     C     $TASTO        WHENEQ    F22
195800970619     C                   z-add     22            posfun
195900970619     C     $FC(22)       IFEQ      '0'
196000970619     C                   SETON                                        9997
196100970619     C                   ENDIF
196200970619     C     $TASTO        WHENEQ    F23
196300970619     C                   z-add     23            posfun
196400970619     C     $FC(23)       IFEQ      '0'
196500970619     C                   SETON                                        9997
196600970619     C                   ENDIF
196700970619     C     $TASTO        WHENEQ    F24
196800970619     C                   z-add     24            posfun
196900970619     C     $FC(24)       IFEQ      '0'
197000970619     C                   SETON                                        9997
197100970619     C                   ENDIF
197200970619     C                   ENDSL
197300970619     C*
197400970619     C                   ENDSR
197500970619     C/EJECT
197600970619     C************************************************************
197700970619     C* AGGIORNAMENTO VALORI ASSUNTI
197800970619     C************************************************************
197900970619     C     AGGMPE        BEGSR
198000970619     C*
198100970619     C* preparo i parametri di chiamata al driver
198200970619     C                   CLEAR                   XMPEDS
198300970619     C                   EVAL      TpChiamata = '03'
198400970619     C                   EVAL      NChiavi = 5
198500970619     C                   EVAL      VLogica = 2
198600970619     C*
198700970619     C* preparo le chiavi per la chiamata al driver
198800970619     C                   CLEAR                   XMPEDS1
198900970619     C                   MOVEL     XSCSOC        DSMPESOC
199000970619     C                   MOVEL     DSPGM         DSMPEPGM
199100970619     C                   MOVEL     KNMUS         DSMPEPRF
199200970619     C*
199300970619     C* Si memorizzano i dati di default a seconda delle modalità con
199400970619     C* cui il pgm è usato
199500970619     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
199600970619     C* MPETIP = 'PV'  pgm richiamato da menù
199700970619     C* MPETIP = 'PB'  x lancio Batch
199800970619     C*
199900970619     C                   SELECT
200000170719    >C     OPZI82        WHENEQ    '01'
200100970619     C                   MOVEL     'PX'          DSMPETIP
200200970619     C                   OTHER
200300970619     C                   MOVEL     'PV'          DSMPETIP
200400970619     C                   ENDSL
200500970619     C* dati
200600970619     C                   MOVEL     DSMPE         DSMPEPAR
200700970619     C*
200800970619     C                   CALLB     'XMPE'
200900970619     C                   PARM                    XMPEDS
201000970619     C                   PARM                    XMPEDS1
201100970619     C*
201200970619     C                   ENDSR
201300970619     C/EJECT
201400970619     C************************************************************
201500970619     C* OPERAZIONI INIZIALI
201600970619     C************************************************************
201700970619     C     *INZSR        BEGSR
201800170731     C     *DMY          Move      Udate         ZDateISO
201900170731     C     *JobRun       Move      ZDateISO      ZDate             6 0
202000970619     C*
202100970619     C* Reperimento parametri
202200970619     C*
202300970619     C     *ENTRY        PLIST
202400970619     C                   PARM                    KPJBA
202500970619     C                   PARM                    DSAUT
202600970619     C*
202700970619     C* testo se sono stati passati i dati di autorizzazione
202800970619     C*
202900970619     C     PARMS         IFLT      2
203000970619     C                   MOVE      *BLANK        AUTDS
203100970619     C                   ELSE
203200970619     C                   MOVEL     DSAUT         AUTDS
203300970619     C                   ENDIF
203400970619     C*
203500970619     C* Reperimento dati società
203600970619     C*
203700970619     C                   MOVEL     'SOC001'      TIPXSC
203800970619     C                   MOVEL     *BLANK        SOCXSC
203900970619     C                   EXSR      REPSOC
204000970619     C*
204100970619     C* Controlli dati società
204200970619     C*
204300970619     C     RTNXSC        IFNE      '1'
204400970619     C                   MOVEL     XSOCDS        SOC001
204500970619     C                   ELSE
204600970619     C                   MOVE      *ON           $FINE
204700170719    >C                   MOVE      '1'           ErrI82
204800970619     C                   EXSR      ENDPGM
204900970619     C                   ENDIF
205000970619     C*
205100970619     C* reperisco le dimensioni delle variabili che conterranno i tasti
205200970619     C                   EVAL      $D1L01 = %Size(Z1Ke1)
205300970619     C                   EVAL      $D1L02 = %Size(Z1Ke2)
205400970619     C                   EVAL      $S1L01 = %Size(Z2Ke1)
205500970619     C                   EVAL      $S1L02 = %Size(Z2Ke2)
205600970619     C                   EVAL      $C1Op1 = %Size(C1Opz1)
205700970619     C                   EVAL      $C1Op2 = %Size(C1Opz2)
205800970619     C                   EVAL      $D2L01 = %Size(Z3Ke1)
205900970619     C                   EVAL      $D2L02 = 0
206000970619     C*
206100970619     C* Reperimento opzioni
206200970619     C*
206300970619     C                   EXSR      REPOPZ
206400970619     C*
206500970619     C* Reperimento tasti di funzione
206600970619     C*
206700970619     C                   EXSR      REPKEY
206800970619     C*
206900970619     C* Chiamata a routine per il reperimento delle opzioni
207000970619     C* personalizzate
207100970619     C                   CAllb     'X46USREXT'
207200970619     C                   PARM                    $OP1
207300970619     C                   PARM                    $TX1
207400970619     C                   PARM                    $FL1
207500970619     C                   PARM                    $PGO1
207600970619     C                   PARM      $dimOp        NumOpz
207700970619     C                   PARM                    $FX2
207800970619     C                   PARM                    $FC2
207900970619     C                   PARM                    $PGF2
208000970619     C                   PARM                    DsPgm
208100970619     C                   PARM                    xsccut
208200970619     C                   PARM                    xscgrp
208300970619     C*
208400970619     C                   Z-add     1             OPZLEN
208500970619     C     *blank        Lookup    $TX1(opzlen)                           21
208600970619     C                   if        *in21 = *on
208700970619     C                   sub       1             OPZLEN
208800970619     C                   else
208900970619     C                   Z-add     $DimOp        OPZLEN
209000970619     C                   endif
209100970619     C*
209200970619     C                   ENDSR
209300970619     C/EJECT
209400970619     C************************************************************
209500970619     C* INIZIALIZZAZIONE VARIABILI
209600970619     C************************************************************
209700970619     C     INZVAR        BEGSR
209800970619     C*
209900970619     C* Dati da PGM chiamante
210000970619     C*
210100170719     C                   MOVEL     KPJBU         SDGSI82DS
210200970619     C*
210300970619     C* Se società diversa da quella impostata nella *inzsr, rieseguo
210400970619     C* reperimento dati società
210500170719    >C                   IF        SocietaI82 <> xscsoc  and
210600170719    >C                             OpzI82 <>*blank
210700970619     C                   MOVEL     'SOC001'      TIPXSC
210800170719    >C                   MOVEL     SocietaI82    SOCXSC
210900970619     C                   EXSR      REPSOC
211000970619     C     RTNXSC        IFNE      '1'
211100970619     C                   MOVEL     XSOCDS        SOC001
211200970619     C                   ELSE
211300970619     C                   MOVE      *ON           $FINE
211400170719    >C                   MOVE      '1'           ErrI82
211500970619     C                   EXSR      ENDPGM
211600970619     C                   ENDIF
211700970619     C                   ENDIF
211800970619     C*
211900970619     C* Reperimento autorizzazioni
212000970619     C*
212100970619     C* le autorizzazioni potrebbero essere già state passate,
212200970619     C* nel qual caso non vanno reperite
212300970620    >C     AUTDS         IFEQ      *BLANK
212400981026    >C                   EXSR      REPAUT
212500970619    >C                   ELSE
212600981026    >C                   MOVEL     XCADDS        ANGCFIDS
212700970619    >C                   ENDIF
212800970619     C*
212900970619     C* Reperimento valori default utente
213000970619     C*
213100970619     C                   EXSR      REPDFT
213200970619     C*
213300970619     C* Definizione delle opzioni utilizzabili a pgm
213400970619     C*
213500970619     C                   EXSR      DEFOPZ
213600970619     C*
213700970619     C* Variabili per gestione videate
213800970619     C*
213900970619     C                   MOVE      *OFF          $FINE                          fine pgm
214000970619     C                   MOVE      *OFF          $INZD1                         inizializzione D1
214100970619     C                   MOVE      *OFF          $INZS1                         inizializzione S1
214200970619     C                   MOVE      *OFF          $INZD2                         inizializzione D2
214300970619     C                   MOVE      *OFF          $EndFile                       memo fine file
214400970619     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
214500970619     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
214600970619     C                   MOVE      *HIVAL        $LASTV
214700970619     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
214800970619     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
214900970619     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
215000970619     C* azzero la posizione del cursore
215100970619     C                   Z-ADD     0             H1RIGA
215200970619     C                   Z-ADD     0             H1COLO
215300970619     C*
215400970619     C                   MOVE      *ON           $INZD1
215500970619     C                   MOVE      *ON           $INZS1
215600970619     C*
215700970619     C* Variabili appoggio
215800970619     C*
215900970619     C                   Z-ADD     SFLPAG        WPAG
216000970619     C                   MOVEL     *OFF          WOPR
216100970619     C*
216200970619     C* definisco qual'è la prima videata
216300970619     C* (se la prima videata risulta la lista, devo comunque caricare
216400970619     C*  la videata di parzializzazione e gestire la definizione
216500970619     C*  dell'ordinamento, perchè, dai default oppure se sono stati
216600970619     C*  passati dei dati, può variare il caricamento)
216700970619     C                   SELECT
216800970619     C     $LOP          WHENEQ    1
216900970619     C                   MOVE      'D1'          $GEST
217000970619     C     $LOP          WHENEQ    2
217100970619     C                   MOVE      'S1'          $GEST
217200970619     C                   EXSR      INZD1
217300970619     C                   MOVE      *OFF          $INZD1
217400970619     C                   EXSR      DEFORD
217500970619     C                   ENDSL
217600970619     C*
217700970619     C* Valorizzazione campi univoci testate
217800970619     C*
217900970619     C                   CLEAR                   FMTT1
218000970619     C                   MOVEL     KNSIF         NOMSIF
218100970619     C                   MOVEL     XSCDSI        NOMDIT
218200170719     C                   MOVEL     KCDAZ         NOMAZN
218300170719     C                   MOVEL     DSPGM         NOMPGM           10
218400970619     C* recupero il sottotitolo (se c'è)
218500970619     C                   MOVEL     *BLANK        T1TIT
218600970619     C     SOTTIT        IFNE      *BLANK
218700970619     C                   MOVEL     SOTTIT        IDMSG
218800970619     C                   CALLB     'XRTVM'
218900970619     C                   PARM                    IDMSG            27
219000970619     C                   PARM                    TXTMSG          100
219100970619     C                   PARM                    ERRMSG            1
219200970619     C                   PARM      *ON           CTRMSG            1            centratura
219300970619     C                   PARM      30            LENMSG            3 0          lun output.
219400970619     C     ERRMSG        IFNE      *ON
219500970619     C                   MOVEL     TXTMSG        T1TIT
219600970619     C                   ELSE
219700970619     C                   MOVEL     *ALL'?'       T1TIT
219800970619     C                   ENDIF
219900970619     C                   ENDIF
220000970619     C*
220100970619     C                   ENDSR
220200970619     C/EJECT
220300970619     C************************************************************
220400970619     C* REPERIMENTO DATI SOCIETA'
220500970619     C************************************************************
220600970619     C     REPSOC        BEGSR
220700970619     C*
220800170731     C                   CALL      'SDGSOCDR'
220900970619     C                   PARM                    TIPXSC            6
221000970619     C                   PARM                    SOCXSC            3
221100970619     C                   PARM                    CDSXSC            9 0
221200970619     C                   PARM                    MODXSC            3
221300970619     C                   PARM      *BLANKS       RTNXSC            1
221400970619     C                   PARM                    XSOCDS
221500970619     C                   PARM                    KPJBA
221600970619     C*
221700970619     C                   ENDSR
221800970619     C/EJECT
221900970619     C************************************************************
222000970619     C* REPERIMENTO AUTORIZZAZIONI UTENTE
222100970619     C************************************************************
222200970619     C     REPAUT        BEGSR
222300970619     C*
222400970619     C                   CLEAR                   AUTDS
222500970619     C*
222600970619     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
222700970619     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
222800970619     C                   Z-ADD     0             XCAGRP                         GRUPPO
222900970619     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
223000981026    >C                   MOVEL     'ANGCFI'      XCAFNC                         FUNZIONE
223100970619    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
223200970619    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
223300970619    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
223400970619     C*
223500970619     C                   CALLB     'XCHKAUT'
223600970619     C                   PARM                    AUTDS
223700970619     C*
223800970619     C* errori/non abilitazione
223900970619     C     XCARTN        IFNE      0
224000170719    >C                   MOVE      '2'           ErrI82
224100970619     C                   EXSR      ENDPGM
224200970619     C                   ELSE
224300981026    >C                   MOVEL     XCADDS        ANGCFIDS
224400970619     C                   ENDIF
224500970619     C*
224600970619     C                   ENDSR
224700970619     C/EJECT
224800970619     C************************************************************
224900970619     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
225000970619     C************************************************************
225100970619     C     REPDFT        BEGSR
225200970619     C*
225300970619     C* preparo i parametri di chiamata al driver
225400970619     C                   CLEAR                   XMPEDS
225500970619     C                   EVAL      TpChiamata = '01'
225600970619     C                   EVAL      NChiavi = 5
225700970619     C                   EVAL      VLogica = 2
225800970619     C*
225900970619     C* preparo le chiavi per la chiamata al driver
226000970619     C                   CLEAR                   XMPEDS1
226100970619     C                   MOVEL     XSCSOC        DSMPESOC
226200970619     C                   MOVEL     DSPGM         DSMPEPGM
226300970619     C                   MOVEL     KNMUS         DSMPEPRF
226400970619     C* Si reperiscono i dati di default a seconda delle modalità con
226500970619     C* cui il pgm è usato
226600970619     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
226700970619     C* MPETIP = 'PV'  pgm richiamato da menù
226800970619     C* MPETIP = 'PB'  x lancio Batch
226900970619     C                   SELECT
227000170719    >C     OPZI82        WHENEQ    '01'
227100970619     C                   MOVEL     'PX'          DSMPETIP
227200970619     C                   OTHER
227300970619     C                   MOVEL     'PV'          DSMPETIP
227400970619     C                   ENDSL
227500970619     C*
227600970619     C                   CALLB     'XMPE'
227700970619     C                   PARM                    XMPEDS
227800970619     C                   PARM                    XMPEDS1
227900970619     C*
228000970619     C     Risultato     IFEQ      '0'
228100970619     C                   MOVEL     DSMPEPAR      DSMPE
228200970619     C* nel caso sia la prima volta che l'utente apre questa anagrafica
228300970619     C* i suoi valori di default vengono decisi a pgm
228400970619     C                   ELSE
228500970619     C                   Z-ADD     1             $LOP
228600970619     C                   Z-ADD     1             $ORD
228700970619     C                   ENDIF
228800970619     C*
228900970619     C                   ENDSR
229000970619     C/EJECT
229100970619     C************************************************************
229200970619     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
229300970619     C************************************************************
229400970619     C     DEFOPZ        BEGSR
229500970619     C*
229600970619     C* Questa routine deve valorizzare la schiera $FL1
229700970619     C* con S o N a seconda della utilizzabilità o meno
229800970619     C* dell'opzione correlata
229900970619     C*
230000970619     C* Inizialmente si considerano tutte le opzioni come valide
230100970619     C     1             DO        $DIMOP        X
230200970619     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
230300970619     C                   If        $TX1(X)  <> *Blank
230400970619     C                   MOVE      'S'           $FL1(X)
230500970619     C                   Else
230600970619     C                   Leave
230700970619     C                   EndIf
230800970619     C                   ENDDO
230900970619     C*
231000970619     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
231100970619     C* con 1 o 0 a seconda della utilizzabilità o meno
231200970619     C* del tasto funzione correlato
231300970619     C*
231400970619     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
231500970619     C* di compilazione
231600970619     C                   RESET                   $FC1
231700970619     C                   RESET                   $FR1
231800970619     C                   RESET                   $FC2
231900970619     C                   RESET                   $FR2
232000970619     C                   RESET                   $FC3
232100970619     C                   RESET                   $FR3
232200970619     C*
232300970619     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
232400970619     C* invalido alcune opzioni
232500970619     C                   SELECT
232600970619     C* Se pgm richiamato in ricerca disattivo :
232700170719    >C     OPZI82        WHENEQ    '01'
232800970619     C* F3=Fine
232900970619     C                   Z-ADD     3             X
233000970619     C                   MOVE      '0'           $FC1(X)
233100970619     C                   MOVE      '0'           $FC2(X)
233200970619     C                   MOVE      '0'           $FC3(X)
233300970619     C*
233400970619     C* Se pgm richiamato in interrogazione disattivo :
233500170719    >C     OPZI82        WHENEQ    '05'
233600970619     C* F10=Immissione
233700970619     C                   Z-ADD     10            X
233800970619     C                   MOVE      '0'           $FC1(X)
233900970619     C                   MOVE      '0'           $FC2(X)
234000970619     C* 1=Selezione
234100970619     C                   Z-ADD     1             X
234200970619     C     01            LOOKUP    $OP1(X)                                21
234300970619     C     *IN21         IFEQ      *ON
234400970619     C                   MOVE      'N'           $FL1(X)
234500970619     C                   ENDIF
234600970619     C* 2=Modifica
234700970619     C                   Z-ADD     1             X
234800970619     C     02            LOOKUP    $OP1(X)                                21
234900970619     C     *IN21         IFEQ      *ON
235000970619     C                   MOVE      'N'           $FL1(X)
235100970619     C                   ENDIF
235200970619     C* 3=Copia
235300970619     C                   Z-ADD     1             X
235400970619     C     03            LOOKUP    $OP1(X)                                21
235500970619     C     *IN21         IFEQ      *ON
235600970619     C                   MOVE      'N'           $FL1(X)
235700970619     C                   ENDIF
235800970619     C* 4=Annullamento
235900970619     C                   Z-ADD     1             X
236000970619     C     04            LOOKUP    $OP1(X)                                21
236100970619     C     *IN21         IFEQ      *ON
236200970619     C                   MOVE      'N'           $FL1(X)
236300970619     C                   ENDIF
236400170719     C*
236500170719     C     OPZI82        WHENEQ    *BLANKS
236600970619     C* Se pgm non richiamato, disattivo subito :
236700970619     C* F8=Selezione seguente
236800970619     C                   Z-ADD     8             X
236900970619     C                   MOVE      '0'           $FC1(X)
237000970619     C                   MOVE      '0'           $FC2(X)
237100970619     C* 1=Selezione
237200970619     C                   Z-ADD     1             X
237300970619     C     01            LOOKUP    $OP1(X)                                21
237400970619     C     *IN21         IFEQ      *ON
237500970619     C                   MOVE      'N'           $FL1(X)
237600970619     C                   ENDIF
237700970619     C*
237800970619     C* In tutti gli altri casi, disattivo subito :
237900970619     C                   OTHER
238000970619     C* 1=Selezione
238100970619     C                   Z-ADD     1             X
238200970619     C     01            LOOKUP    $OP1(X)                                21
238300970619     C     *IN21         IFEQ      *ON
238400970619     C                   MOVE      'N'           $FL1(X)
238500970619     C                   ENDIF
238600170719     C*
238700970619     C                   ENDSL
238800970619     C*
238900970619     C                   ENDSR
239000970619     C/EJECT
239100970619     C************************************************************
239200970619     C* REPERIMENTO OPZIONI
239300970619     C************************************************************
239400970619     C     REPOPZ        BEGSR
239500970619     C*
239600970619     C     1             DO        $DIMOP        X
239700970619     C*
239800970619     C     $MS1(X)       IFNE      *BLANK
239900970619     C*
240000970619     C                   MOVEL     $MS1(X)       IDMSG
240100970619     C                   CALLB     'XRTVM'
240200970619     C                   PARM                    IDMSG            27
240300970619     C                   PARM                    TXTMSG          100
240400970619     C                   PARM                    ERRMSG            1
240500970619     C*
240600970619     C     ERRMSG        IFNE      *ON
240700970619     C                   MOVEL     TXTMSG        $TX1(X)
240800970619     C                   ELSE
240900970619     C                   MOVEL     *ALL'?'       $TX1(X)
241000970619     C                   ENDIF
241100970619     C*
241200970619     C                   ENDIF
241300970619     C*
241400970619     C                   ENDDO
241500970619     C*
241600970619     C                   ENDSR
241700970619     C/EJECT
241800970619     C************************************************************
241900970619     C* REPERIMENTO TASTI DI FUNZIONE
242000970619     C************************************************************
242100970619     C     REPKEY        BEGSR
242200970619     C                   MOVEA     $FM1          $FM
242300970619     C                   MOVEA     $FC1          $FC
242400970619     C                   EXSR      REPKKK
242500970619     C                   MOVEA     $FX           $FX1
242600970619     C*
242700970619     C                   MOVEA     $FM2          $FM
242800970619     C                   MOVEA     $FC2          $FC
242900970619     C                   EXSR      REPKKK
243000970619     C                   MOVEA     $FX           $FX2
243100970619     C*
243200970619     C                   MOVEA     $FM3          $FM
243300970619     C                   MOVEA     $FC3          $FC
243400970619     C                   EXSR      REPKKK
243500970619     C                   MOVEA     $FX           $FX3
243600970619     C                   ENDSR
243700970619     C/EJECT
243800970619     C************************************************************
243900970619     C* REPERIMENTO TASTI DI FUNZIONE
244000970619     C************************************************************
244100970619     C     REPKKK        BEGSR
244200970619     C*
244300970619     C                   CLEAR                   $FX
244400970619     C     1             DO        24            X
244500970619     C*
244600970619     C     $FM(X)        IFNE      *BLANK
244700970619     C     $Fc(X)        andne     '0'
244800970619     C*
244900970619     C                   MOVEL     $FM(X)        IDMSG
245000970619     C                   CALLB     'XRTVM'
245100970619     C                   PARM                    IDMSG            27
245200970619     C                   PARM                    TXTMSG          100
245300970619     C                   PARM                    ERRMSG            1
245400970619     C*
245500970619     C     ERRMSG        IFNE      *ON
245600970619     C                   MOVEL     TXTMSG        $FX(X)
245700970619     C                   ELSE
245800970619     C                   MOVEL     *ALL' '       $FX(X)
245900970619     C                   ENDIF
246000970619     C*
246100970619     C                   ENDIF
246200970619     C*
246300970619     C                   ENDDO
246400970619     C*
246500970619     C                   ENDSR
246600970619     C/EJECT
246700970619     C************************************************************
246800970619     C* DEFINIZIONE KLIST
246900970619     C************************************************************
247000970619     C     DefKlist      BEGSR
247100970619     C*
247200970619     C* klist
247300970619     C*
247400170731     C     K02CSF01      KLIST
247500170719     C                   KFLD                    CSFSocieta
247600170731     C                   KFLD                    CSFFile
247700170731     C*
247800170731     C     K01CSF01      KLIST
247900170731     C                   KFLD                    CSFSocieta
248000970619     C*
248100970619     C                   ENDSR
248200970619     C/EJECT
248300170719     C***********************************************************
248400970619**
248500970619PROMSG    *LIBL     OPZ0001
248600170801PROMSG    *LIBL     OPZ0002
248700170801PROMSG    *LIBL     OPZ0003
248800170801PROMSG    *LIBL     OPZ0004
248900970619PROMSG    *LIBL     OPZ0005
249000170810SDGMSG    *LIBL     SDO0056
249100970619**
24920097061901S      <Selezione
24930017080102S      <Modifica
24940017080103S      <Copia
24950017080104S      <Annullamento
24960097061905S      <Visualizzazione
24970017081015S      <Visualizza Movimenti
249800970619** TASTI DI FUNZIONE  D1
249900970619PROMSG    *LIBL     KEY0001  01
250000970619PROMSG    *LIBL     KEY0002  02
250100970619PROMSG    *LIBL     KEY0003  03
250200970619PROMSG    *LIBL     KEY0004  04
250300970619PROMSG    *LIBL     KEY0005  05
250400970619PROMSG    *LIBL     KEY0006  06
250500970619PROMSG    *LIBL     KEY0007  07
250600970619PROMSG    *LIBL     KEY0008  08
250700970619PROMSG    *LIBL     KEY0009  09
250800970619PROMSG    *LIBL     KEY0010  10
250900970619PROMSG    *LIBL     KEY0011  11
251000970619PROMSG    *LIBL     KEY0012  12
251100970619PROMSG    *LIBL     KEY0013  13
251200970619PROMSG    *LIBL     KEY0014  14
251300970619PROMSG    *LIBL     KEY0015  15
251400970619PROMSG    *LIBL     KEY0016  16
251500970619PROMSG    *LIBL     KEY0017  17
251600970619PROMSG    *LIBL     KEY0018  18
251700970619PROMSG    *LIBL     KEY0019  19
251800970619PROMSG    *LIBL     KEY0020  20
251900970619PROMSG    *LIBL     KEY0021  21
252000970619PROMSG    *LIBL     KEY0022  22
252100970619PROMSG    *LIBL     KEY0023  23
252200970619PROMSG    *LIBL     KEY0024  24
252300970619**  ABILITAZIONE D1
2524009706190N  01
2525009706191N  02
2526009706191N  03
2527009706191N  04
2528009706190N  05
2529009706190N  06
2530009706190N  07
2531009706191N  08
2532009706190N  09
2533009706190N  10
2534009706190N  11
2535009706191N  12
2536009706190N  13
2537009706190N  14
2538009706190N  15
2539009706190N  16
2540009706190N  17
2541009706191N  18
2542009706190N  19
2543009706190N  20
2544009706190N  21
2545009706190N  22
2546009706190N  23   Impostare a '1' se si utilizzano opzioni
2547009706191N  24   Lasciare sempre impostata a '1' il valore della riga 24
254800970619** TASTI DI FUNZIONE  S1
254900970619PROMSG    *LIBL     KEY0001  01
255000970619PROMSG    *LIBL     KEY0002  02
255100970619PROMSG    *LIBL     KEY0003  03
255200970619PROMSG    *LIBL     KEY0004  04
255300970619PROMSG    *LIBL     KEY0005  05
255400970619PROMSG    *LIBL     KEY0006  06
255500970619PROMSG    *LIBL     KEY0007  07
255600970619PROMSG    *LIBL     KEY0008  08
255700970619PROMSG    *LIBL     KEY0009  09
255800970619PROMSG    *LIBL     KEY0010  10
255900970619PROMSG    *LIBL     KEY0011  11
256000970619PROMSG    *LIBL     KEY0012  12
256100970619PROMSG    *LIBL     KEY0013  13
256200970619PROMSG    *LIBL     KEY0014  14
256300970619PROMSG    *LIBL     KEY0015  15
256400970619PROMSG    *LIBL     KEY0016  16
256500970619PROMSG    *LIBL     KEY0017  17
256600970619PROMSG    *LIBL     KEY0018  18
256700970619PROMSG    *LIBL     KEY0019  19
256800970619PROMSG    *LIBL     KEY0020  20
256900970619PROMSG    *LIBL     KEY0021  21
257000970619PROMSG    *LIBL     KEY0022  22
257100970619PROMSG    *LIBL     KEY0023  23
257200970619PROMSG    *LIBL     KEY0024  24
257300970619**  ABILITAZIONE S1
2574009706190N  01
2575009706190N  02
2576009706191N  03
2577009706190N  04
2578009706190N  05
2579009706190N  06
2580009706190N  07
2581009706191N  08
2582000410271N  09
2583009706190N  10
2584009706190N  11
2585009706191N  12
2586009706190N  13
2587009706190N  14
2588001708110N  15
2589009706190N  16
2590009706191N  17
2591009706191N  18
2592009706190N  19
2593009706190N  20
2594009706190N  21
2595009706190N  22
2596009706191N  23   Impostare a '1' se si utilizzano opzioni
2597009706191N  24   Lasciare sempre impostata a '1' il valore della riga 24
259800970619** TASTI DI FUNZIONE  D3
259900970619PROMSG    *LIBL     KEY0001  01
260000970619PROMSG    *LIBL     KEY0002  02
260100970619PROMSG    *LIBL     KEY0003  03
260200970619PROMSG    *LIBL     KEY0004  04
260300970619PROMSG    *LIBL     KEY0005  05
260400970619PROMSG    *LIBL     KEY0006  06
260500970619PROMSG    *LIBL     KEY0007  07
260600970619PROMSG    *LIBL     KEY0008  08
260700970619PROMSG    *LIBL     KEY0009  09
260800970619PROMSG    *LIBL     KEY0010  10
260900970619PROMSG    *LIBL     KEY0011  11
261000970619PROMSG    *LIBL     KEY0012  12
261100970619PROMSG    *LIBL     KEY0013  13
261200970619PROMSG    *LIBL     KEY0014  14
261300970619PROMSG    *LIBL     KEY0015  15
261400970619PROMSG    *LIBL     KEY0016  16
261500970619PROMSG    *LIBL     KEY0017  17
261600970619PROMSG    *LIBL     KEY0018  18
261700970619PROMSG    *LIBL     KEY0019  19
261800970619PROMSG    *LIBL     KEY0020  20
261900970619PROMSG    *LIBL     KEY0021  21
262000970619PROMSG    *LIBL     KEY0022  22
262100970619PROMSG    *LIBL     KEY0023  23
262200970619PROMSG    *LIBL     KEY0024  24
262300970619**  ABILITAZIONE D3
2624009706190N  01
2625009706190N  02
2626009706191N  03
2627009706190N  04
2628009706190N  05
2629009706190N  06
2630009706190N  07
2631009706190N  08
2632009706190N  09
2633009706190N  10
2634009706190N  11
2635009706191N  12
2636009706190N  13
2637009706190N  14
2638009706190N  15
2639009706190N  16
2640009706190N  17
2641009706190N  18
2642009706190N  19
2643009706190N  20
2644009706190N  21
2645009706190N  22
2646009706190N  23   Impostare a '1' se si utilizzano opzioni
2647009706191N  24   Lasciare sempre impostata a '1' il valore della riga 24
