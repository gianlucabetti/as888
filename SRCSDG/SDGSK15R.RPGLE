000100160616     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200160616     H*PARMS BNDDIR(PJXBND PJCBND)
000300160616     H*PARMS CVTOPT(*NONE)
000400160616     H DECEDIT('0,') DATEDIT(*DMY.)
000500160616      *
000600160616     >*            GUIDA manutenzione mod.770 percipienti esteri
000700160616      *           -----------------------------------------------
000800160616      *
000801160620R218 A* Questo pgm è stato creato nuovo con il task R218
000900160616      *
004700160616     F*------------
004800160616    >FANAPE01L  IF   E           K DISK
004900160616    >F                                     RENAME(anape000:anape01)
005000160616     F*------------
005100160617     FSDGN7705L IF   E           K DISK
005300160616     F*------------
005400160616    >FSDGSK15D  CF   E             WORKSTN
005500160616    >F                                     RENAME(SK15D1:FMTD1)
005600160616    >F                                     RENAME(SK15D2:FMTD2)
005700160616    >F                                     RENAME(SK15D3:FMTD3)
005800160616    >F                                     RENAME(SK15S1:FMTS1)
005900160616    >F                                     RENAME(SK15C1:FMTC1)
006000160616    >F                                     RENAME(SK15T1:FMTT1)
006100160616    >F                                     RENAME(SK15Z1:FMTZ1)
006200160616    >F                                     RENAME(SK15Z2:FMTZ2)
006300160616    >F                                     RENAME(SK15Z3:FMTZ3)
006400160616     F                                     SFILE(FMTS1:S1NRR)
006500160616     F                                     INFDS(DSFMT)
006600160616     D*----------------------------------------------------*
006700160616     D* Schiere per la gestione delle opzioni
006800160616     D*
006900160616     D* dimensione della schiera $MS1, da non cambiare
007000160616     D $DIMOP          C                   CONST(30)
007100160616     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
007200160616     D $TX1            S             50    DIM($dimop) ALT($MS1)
007300160616     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
007400160616     D $FL1            S              1    DIM($dimop) ALT($OP1)
007500160616     D* schiera contenente i nomi dei programma richiamati attraverso
007600160616     D* le opzioni personalizzate
007700160616     D $PGO1           S             10    DIM($dimop)
007800160616     D $TX             S             50    DIM(50)
007900160616     D $FL             S              1    DIM(50)
008000160616     D* Schiere generali per la gestione dei tasti di funzione
008100160616     D $FM             S             27    DIM(24)
008200160616     D $FX             S             50    DIM(24)
008300160616     D $FC             S              1    DIM(24)
008400160616     D $FR             S              1    DIM(24)
008500160616     D* Schiere per la gestione dei tasti di funzione
008600160616     D* Video D1
008700160616     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
008800160616     D $FX1            S             50    DIM(24) ALT($FM1)
008900160616     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
009000160616     D $FR1            S              1    DIM(24) ALT($FC1)
009100160616     D* Schiera contente i nomi dei programmi da richiamare con i tasti
009200160616     D* funzione personalizzata S1
009300160616     D $PGF2           S             10    DIM(25)
009400160616     D* Schiere per la gestione dei tasti di funzione
009500160616     D* Video S1
009600160616     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
009700160616     D $FX2            S             50    DIM(24) ALT($FM2)
009800160616     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
009900160616     D $FR2            S              1    DIM(24) ALT($FC2)
010000160616     D* Schiere per la gestione dei tasti di funzione
010100160616     D* Video D2
010200160616     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
010300160616     D $FX3            S             50    DIM(24) ALT($FM3)
010400160616     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
010500160616     D $FR3            S              1    DIM(24) ALT($FC3)
010600160616     D*
010700160616     D $D1L01          S              3  0
010800160616     D $D1L02          S              3  0
010900160616     D $S1L01          S              3  0
011000160616     D $S1L02          S              3  0
011100160616     D $C1Op1          S              3  0
011200160616     D $C1Op2          S              3  0
011300160616     D $D2L01          S              3  0
011400160616     D $D2L02          S              3  0
011500160616    >D* Inserire qui la definizione di altre variabili per reperire
011600160616    >D* la lunghezza di altre videate che usano un piede diverso da
011700160616    >D* STD
011800160616     D*
011900160616     D*-------------
012000160616     D* Indice della schiera tasti funzione
012100160616     D PosFun          S              2  0
012200160616     D/EJECT
012300160616     D*----------------------------------------------------*
012400160616     D*-------------
012500160616     D* Valori assunti
012600160616     D XMPEDS        E DS                  INZ
012700160616     D XMPEDS1       E DS                  INZ
012800160616     D DSMPE           DS                  INZ
012900160616     D  $LOP                   1      1  0
013000160616     D  $ORD                   2      3  0
013100160616    >D**$DFn                   4     xx
015300160616     D*-------------
015400160616     D* Passaggio Parametri
015500160616     D KPJBA         E DS
015600160616     D*-------------
015700160616     D* Reperimento Autorizzazioni
015800160616     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
015900160616     D*-------------
016000160616     D* Passaggio Autorizzazioni
016100160616     D DSAUT           DS           512
016200160616     D*-------------
016201160617     D* Dati di ambiente ottenuti da SDGSOCDR
016202160617     D SOC001        E DS                  EXTNAME(SDGSOCDS )
016500160616     D*-------------
016501160617     D* DS Interna per dati di output di SDGSOCDR
016700160616     D XSOCDS          DS          1000
016800160616     D*-------------
016900160616     D* Parametri del PGM di manutenzione SDGSK15R
017000160616    >D SDGSK15DS     E DS                  inz
017100160616    >D  RETK15       E                     INZ('0')
017200160616    >D  OPRK15       E                     INZ('0')
017300160616    >D  ERRK15       E                     INZ('0')
017400160616     D*-------------
017500160616     D* campo di salvataggio per la DS d'ingresso durante la chiamata
017600160616     D* alla funzione personalizzata
017700160616    >D $SaveDS         S                   like(SDGSK15DS)
017800160616     D*-------------
017900160616     D DSFMT           DS           512
018000160616     D  $TASTO               369    369
018100160616     D  NRG                  370    370
018200160616     D  NCL                  371    371
018300160616     D  SFLNRR               378    379B 0
018400160616     D*-------------
018500160616     D* posizione cursore
018600160616     D CURSOR          DS
018700160616     D  RIRI                   1      2B 0 INZ
018800160616     D  R$$                    2      2
018900160616     D  COCO                   3      4B 0 INZ
019000160616     D  C$$                    4      4
019100160616     D*-------------
019200160616     D* Reperimento nome PGM
019300160616     D STATUS         SDS           333
019400160616     D  DSPGM            *PROC
019500160616     D  PARMS            *PARMS
019600160616     D*-------------
019700160616     D* Costanti
019800160616     D* nr.di righe del sfl (SFLPAG)
019900160616    >D SFLPAG          C                   CONST(11)
020000160616     D*-------------
020100160616     D* Tasti di funzione
020200160616     D F01             C                   CONST(X'31')
020300160616     D F02             C                   CONST(X'32')
020400160616     D F03             C                   CONST(X'33')
020500160616     D F04             C                   CONST(X'34')
020600160616     D F05             C                   CONST(X'35')
020700160616     D F06             C                   CONST(X'36')
020800160616     D F07             C                   CONST(X'37')
020900160616     D F08             C                   CONST(X'38')
021000160616     D F09             C                   CONST(X'39')
021100160616     D F10             C                   CONST(X'3A')
021200160616     D F11             C                   CONST(X'3B')
021300160616     D F12             C                   CONST(X'3C')
021400160616     D F13             C                   CONST(X'B1')
021500160616     D F14             C                   CONST(X'B2')
021600160616     D F15             C                   CONST(X'B3')
021700160616     D F16             C                   CONST(X'B4')
021800160616     D F17             C                   CONST(X'B5')
021900160616     D F18             C                   CONST(X'B6')
022000160616     D F19             C                   CONST(X'B7')
022100160616     D F20             C                   CONST(X'B8')
022200160616     D F21             C                   CONST(X'B9')
022300160616     D F22             C                   CONST(X'BA')
022400160616     D F23             C                   CONST(X'BB')
022500160616     D F24             C                   CONST(X'BC')
022600160616     D ENTER           C                   CONST(X'F1')
022700160616     D ROLDWN          C                   CONST(X'F4')
022800160616     D ROLLUP          C                   CONST(X'F5')
022900160616     D*-------------
023000160616     D* Sottotitolo di testata
023100160616    >D* se non si hanno sottotitoli, mettere tutta la costante = ' '
023200160616     D SOTTIT          C                   CONST('               -
023300160616     D                                                 ')
023301160617     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
023302160617     D                                          SDC0001')
023400160616     D*-------------
023500160616     D* Variabili per gestione videate
023600160616     D $Gest           S              2A
023700160616     D $Fine           S              1A
023800160616     D $InzD1          S              1A
023900160616     D $InzS1          S              1A
024000160616     D $InzD2          S              1A
024100160616     D $LastG          S              2A
024200160616     D $LastV          S              2A
024300160616     D $EndFile        S              1A
024400160616     D $Esci           S              1A
024500160616     D $RcdOK          S              1A
024600160616     D $Unique         S              1A
024700160616     D*-------------
024800160616     D* Variabili appoggio sempre presenti in tutte le anagrafiche
024900160616     D S1Nrr           S                   like(C1Rcd)
025000160616     D WSfl            S                   like(C1Nrr)
025100160616     D WMax            S                   like(C1Rcd)
025200160616     D WPag            S                   like(C1Rcd)
025300160616     D WInzS1          S                   like($InzS1)
025400160616    >D WOpr            S                   like(OprK15)
025500160616     D CurR            S              2S 0
025600160616     D CurC            S              2S 0
025700160616     D Win             S             99A
025800160616     D W1Pos           S                   like(C1Pos)
025900160616     D WINKR           S              1A
026000160616     D WExf            S              1A
026100160616     D*-------------
026200160616     D* Variabili appoggio specifiche della singola anagrafica
026300160616     D*  campi per parzializzazioni su rcd (solo alfanumerici)
026400160616    >D Anno999         S                   like(AnnoK15)
026500160616    >D Qua999          S                   like(QuaK15)
026600160616    >D*
026700160616     D*  Indici
026800160616     D X               S              3S 0
026900160616     D Y               S              3S 0
027000160616     D* Indice della schiera per le opzioni personalizzate
027100160616     D PosOpz          S              2  0
027200160616     D* numero max opzioni
027300160616     D NumOpz          S              2  0 Inz($DIMOP)
027400160616     D* numero effettive opzioni
027500160616     D OpzLen          S              2  0
027600160616     D*-------------
027700160616     D* Ricerca capoconti
027800160616     D A200DS        E DS                  EXTNAME(ANA200DS) INZ
027900160616     D* Ricerca sottoconti
028000160616     D A201DS        E DS                  EXTNAME(ANA201DS) INZ
028100160616     D*-------------
028200160616     D* DS esterna per dati fornitori
028300160616     D DVAFOR        E DS                  INZ
028400160616     D* DS per controllo conto
028500160616     D ND002DS       E DS                  INZ
028600160616     D* Definizioni parametri driver anagrafici
028700160616     D Dvaric          S              1
028800160616     D DvaSocieta      S              3
028900160616     D DvaUnita        S              8
029000160616     D DvaStrutt       S             10
029100160616     D DvaDtRif        S               D
029200160616     D DvaLenOut       S              9B 0
029300160616     D DvaSnatura      S              1
029400160616     D DvaCodice       S              8
029500160616     D DvaLineav       S              3
029600160616     D DvaFiliale      S              3  0
029700160616     D DvaSogg         S              8
029800160616     D DvaTpInd        S              2
029900160616     D DvaCdInd        S              3
030000160616     D DvaErrore       S              1
030100160616     D DvaOutput       S           4000
030200160616     D*-------------
030300160616     D* Definizioni parametri standard chiamate moduli
030400160616     D Esito           S              1
030500160616     D GesMsg          S              1
030600160616     D LenOut          S              9B 0
030700160616     D Struttura       S             10
030800160616     D Data            S               D
030900160616     D*-------------
031000160616     D $Kcc            S                   LIKE(N77Kcc)
031100160616     D $Ksc            S                   LIKE(N77Ksc)
031200160616     D*-------------
031300160617     D* Attributi video
031400160617     D Normale         C                   Const(X'32')
031500160617     D Protetto        C                   Const(X'A0')
031600160616     C*----------------------------------------------------*
031700160616     C*                MAIN LINE PROGRAM
031800160616     C*----------------------------------------------------*
031900160616     C*
032000160616     C* inizializzazione variabili
032100160616     C                   EXSR      INZVAR
032200160616     C*
032300160616     C     $FINE         DOWEQ     *OFF
032400160616     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
032500160616     C     $GEST         CASEQ     'S1'          GESS1                          Lista
032600160616     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
032700160616     C                   ENDCS
032800160616     C                   ENDDO
032900160616     C* fine programma
033000160616     C                   EXSR      ENDPGM
033100160616     C*
033200160616     C/EJECT
033300160616     C************************************************************
033400160616     C* FINE PROGRAMMA
033500160616     C************************************************************
033600160616     C     ENDPGM        BEGSR
033700160616     C*
033800160616     C* passaggio dati al pgm chiamante
033900160616    >C                   MOVEL     WOPR          OPRK15                                     ne S1
034000160616    >C                   MOVEL     SDGSK15DS     KPJBU
034100160616     C*
034200160616     C* Se pgm Guida non è stato richiamato posso chiudere in LR
034300160616    >C     OPZK15        IFEQ      *BLANK
034400160616     C                   SETON                                            LR
034500160616     C* altrimenti chiudo in RT
034600160616     C                   ELSE
034700160616     C                   SETON                                            RT
034800160616     C                   ENDIF
034900160616     C                   RETURN
035000160616     C*
035100160616     C                   ENDSR
035200160616     C/EJECT
035300160616     C************************************************************
035400160616     C* RICERCHE
035500160616     C************************************************************
035600160616     C     SEARCH        BEGSR
035700160616     C*
035800160616     C* determino Riga/Colonna del cursore
035900160616     C                   MOVE      NRG           R$$
036000160616     C                   MOVE      NCL           C$$
036100160616     C                   Z-ADD     RIRI          CURR
036200160616     C                   Z-ADD     COCO          CURC
036300160616      *
036400160616     C     D1kcc         IFNE      *BLANKS
036500160616     C     D1kcc         ANDNE     'C     '
036600160616     C     D1kcc         ANDNE     'F     '
036700160616     C                   CALLB     'XALLINEA'
036800160616     C                   PARM                    D1kcc
036900160616     C                   PARM      6             LEN               5 0
037000160616     C                   PARM                    XSCINP
037100160616     C                   ENDIF
037200160616      *
037300160616     C     D1ksc         IFNE      *BLANKS
037400160616     C                   CALLB     'XALLINEA'
037500160616     C                   PARM                    D1ksc
037600160616     C                   PARM      8             LEN               5 0
037700160616     C                   PARM                    XSCINP
037800160616     C                   ENDIF
037900160616     C*
038000160616   B1C                   SELECT
038100160616     C*
040200160616      * FMTD1
040300160616     C     H1NMFL        WHENEQ    'D1KCC'
040400160616     C                   RESET                   A200DS
040500160616     C                   MOVEL     '01'          OPZ200
040600160616     C                   MOVE      D1KCC         KCC200
040700160616     C                   MOVEL     A200DS        KPJBU
040800160616     C                   CALL      'ANA200R'
040900160616     C                   PARM                    KPJBA
041000160616     C                   MOVEL     KPJBU         A200DS
041100160616      *
041200160616     C     KCC200        IFNE      *BLANK
041300160616     C                   MOVEL     KCC200        D1KCC
041400160616     C                   MOVEL     *blank        D1KSC
041500160616     C                   ENDIF
041600160616      *
041700160616      * FMTD1  - SOTTOCONTO
041800160616     C     H1NMFL        WHENEQ    'D1KSC'
041900160616     C                   RESET                   A201DS
042000160616     C                   MOVE      XSCSOC        SOC201
042100160616     C                   MOVE      D1KCC         KCC201
042200160616     C                   MOVE      D1KSC         KSC201
042300160616     C                   MOVEL     A201DS        KPJBU
042400160616     C                   CALL      'ANA201R'
042500160616     C                   PARM                    KPJBA
042600160616     C                   MOVEL     KPJBU         A201DS
042700160616      *
042800160616     C     KSC201        IFNE      *BLANK
042900160616     C                   MOVEL     KCC201        D1KCC
043000160616     C                   MOVEL     KSC201        D1KSC
043100160616     C                   ENDIF
043200160616    >C*
043300160616     C* ricerca non abilitata in questa posizione
043400160616   X1C                   OTHER
043500160616     C                   SETON                                        9899
043600160616     C*
043700160616   E1C                   ENDSL
043800160616     C*
043900160616     C* imposto pos. del cursore
044000160616     C                   Z-ADD     CURR          H1RIGA
044100160616     C                   Z-ADD     CURC          H1COLO
044200160616     C*
044300160616     C                   MOVE      *HIVAL        $LASTV
044400160616     C*
044500160616     C                   ENDSR
044600160616     C/EJECT
044700160616     C************************************************************
044800160616     C* RIEMPIMENTO OPZIONI
044900160616     C************************************************************
045000160616     C     RIEOPZ        BEGSR
045100160616     C*
045200160616     C* Input :
045300160616     C* - $TX = schiera con tutti i testi disponibili
045400160616     C* - $FL = schiere con i flag di validità associati ai testi
045500160616     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
045600160616     C*          opzione usato
045700160616     C* - WMXO = variabile contenente il numero massimo di testi
045800160616     C*          opzione usabili
045900160616     C* - $L01 = Lunghezza prima riga
046000160616     C* - $L02 = Lunghezza seconda riga
046100160616     C* Output :
046200160616     C* - WOPZ1 = variabile contenente la prima riga opzioni
046300160616     C* - WOPZ2 = variabile contenente la seconda riga opzioni
046400160616     C* - FF23  = flag che indica se c'è bisogno del tasto F23
046500160616     C*
046600160616     C                   CALLB     'X04OPMSG'
046700160616     C* input
046800160616     C                   PARM                    $TX
046900160616     C                   PARM                    $FL
047000160616     C                   PARM                    WCOZ              3 0
047100160616     C                   PARM                    WMXO              3 0
047200160616     C* output
047300160616     C                   PARM                    WOPZ1            76
047400160616     C                   PARM                    WOPZ2            76
047500160616     C                   PARM                    FF23              1
047600160616     C* input
047700160616     C                   PARM                    $L01              3 0
047800160616     C                   PARM                    $L02              3 0
047900160616     C*
048000160617     C                   PARM                    $ALLOPT         960
048100160617     C*
048200160616     C                   ENDSR
048300160616     C/EJECT
048400160616     C************************************************************
048500160616     C* RIEMPIMENTO TASTI FUNZIONE
048600160616     C************************************************************
048700160616     C     RIEKEY        BEGSR
048800160616     C*
048900160616     C* Input :
049000160616     C* - $FX = schiera con tutti i testi disponibili
049100160616     C* - $FC = schiere con i flag di validità associati ai testi
049200160616     C* - $FR = schiere con i flag di REVERSE IMAGE
049300160616     C* - $ULK = ultimo testo caricato
049400160616     C* - $L01 = Lunghezza prima riga
049500160616     C* - $L02 = Lunghezza seconda riga
049600160616     C* Output :
049700160616     C* - $ULK
049800160616     C* - $KEY1= 1° riga di tasti funzione
049900160616     C* - $KEY2= 2° riga di tasti funzione
050000160616     C*
050100160616     C                   CALLB     'XKEYMSG'
050200160616     C* input
050300160616     C                   PARM                    $FX
050400160616     C                   PARM                    $FC
050500160616     C                   PARM                    $FR
050600160616     C                   PARM                    $ULK              3 0
050700160616     C* output
050800160616     C                   PARM                    $KEY1            79
050900160616     C                   PARM                    $KEY2            79
051000160616     C* input
051100160616     C                   PARM                    $L01              3 0
051200160616     C                   PARM                    $L02              3 0
051300160616     C*
051400160617     C                   PARM                    $ALLFUNCT       480
051500160617     C*
051600160616     C                   ENDSR
051700160616     C/EJECT
051800160616     C************************************************************
051900160616     C* GESTIONE EMISSIONE TASTI FUNZIONE
052000160616     C************************************************************
052100160616     C     GESPIE        BEGSR
052200160616     C*
052300160616     C                   SELECT
052400160616     C* Parzializzazioni
052500160616     C     $GEST         WHENEQ    'D1'
052600160616     C     $ULKD1        IFEQ      0
052700160616     C* riempimento opzioni
052800160616     C                   EXSR      F24D1
052900160616     C                   ELSE
053000160616     C                   WRITE     FMTZ1
053100160616     C                   ENDIF
053200160616     C* Lista
053300160616     C     $GEST         WHENEQ    'S1'
053400160616     C     $ULKS1        IFEQ      0
053500160616     C                   EXSR      F24S1
053600160616     C                   ELSE
053700160616     C                   WRITE     FMTZ2
053800160616     C                   ENDIF
053900160616     C* Modifica valori assunti
054000160616     C     $GEST         WHENEQ    'D2'
054100160616     C     $ULKD2        IFEQ      0
054200160616     C                   EXSR      F24D2
054300160616     C                   ELSE
054400160616     C                   WRITE     FMTZ3
054500160616     C                   ENDIF
054600160616     C                   ENDSL
054700160616     C*
054800160616     C                   ENDSR
054900160616     C/EJECT
055000160616     C************************************************************
055100160616     C* GESTIONE PARZIALIZZAZIONI
055200160616     C************************************************************
055300160616     C     GESD1         BEGSR
055400160616     C* inizializzazione videata
055500160616     C     $INZD1        IFEQ      *ON
055600160616     C                   EXSR      INZD1
055700160616     C                   MOVE      *OFF          $INZD1
055800160616     C                   ENDIF
055900160616     C* emissione piede videata se proveniente da altra
056000160616     C* salvataggio valore formato di provenienza
056100160616     C     $LASTV        IFNE      'D1'
056101160617     C*
056102160617     C* recupero il sottotitolo (se c'è)
056103160617     C                   Clear                   D1TIT
056104160617     C     SOTTITD1      IFNE      *BLANK
056105160617     C                   MOVEL     SOTTITD1      IDMSG
056106160617     C                   CALLB     'XRTVM'
056107160617     C                   PARM                    IDMSG            27
056108160617     C                   PARM                    TXTMSG          100
056109160617     C                   PARM                    ERRMSG            1
056110160617     C                   PARM      *Off          CTRMSG            1            centratura
056111160617     C                   PARM      15            LENMSG            3 0          lun output.
056112160617     C     ERRMSG        IFNE      *ON
056113160617     C                   MOVEL     TXTMSG        D1TIT
056114160617     C                   ELSE
056115160617     C                   MOVEL     *ALL'?'       D1TIT
056116160617     C                   ENDIF
056117160617     C                   ENDIF
056118160617     C*
056200160616     C* salvo stato F17 per sapere cosa fare in caso di F12
056300160616     C                   MOVE      *INKR         WINKR
056400160616     C                   WRITE     FMTT1
056500160616     C                   EXSR      GESPIE
056600160616     C                   MOVE      $LASTV        $LASTG
056700160616     C                   MOVE      'D1'          $LASTV
056800160616     C                   ENDIF
056900160616     C*
057000160616     C* se esistono errori sulla videata
057100160616     C* emetto la write del formato a indicatori spenti per vedere
057200160616     C* le eventuali decodifiche
057300160616     C     *IN99         IFEQ      *ON
057400160616     C                   MOVEA     *IN           WIN
057500160616     C                   MOVE      *ALL'0'       IN5098           49
057600160616     C                   MOVEA     IN5098        *IN(50)
057700160616     C                   WRITE     FMTD1
057800160616     C                   MOVEA     WIN           *IN
057900160616     C                   ENDIF
058000160616     C*
058100160616     C                   EXFMT     FMTD1
058200160616     C*
058300160616     C* azzero la posizione del cursore
058400160616     C                   Z-ADD     0             H1RIGA
058500160616     C                   Z-ADD     0             H1COLO
058600160616     C*
058700160616     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
058800160616     C* INPUT
058900160616     C* $FC1 = Abilitazioni videata D1
059000160616     C* OUTPUT
059100160616     C* *IN97 + *IN99 Errori
059200160616     C                   MOVEA     $FC1          $FC
059300160616     C                   EXSR      CTRKEY
059400160616     C*
059500160616     C     *IN99         IFEQ      *OFF
059600160616     C*
059700160616     C                   SELECT
059800160616     C* F2=Decodifica
059900160616     C     $TASTO        WHENEQ    F02
060000160616     C                   EXSR      F02D1
060100160616     C* F3=Fine
060200160616     C     $TASTO        WHENEQ    F03
060300160616     C                   EXSR      F03D1
060400160616     C* F4=Ricerca
060500160616     C     $TASTO        WHENEQ    F04
060600160616     C                   EXSR      SEARCH
060700160616     C* F5=Refresh
060800160616     C     $TASTO        WHENEQ    F05
060900160616     C                   EXSR      F05D1
061000160616     C* F8=Successivo
061100160616     C     $TASTO        WHENEQ    F08
061200160616     C                   EXSR      F08D1
061300160616     C* F10=Immissione
061400160616     C     $TASTO        WHENEQ    F10
061500160616     C                   EXSR      F10D1
061600160616     C* F12=Ritorno
061700160616     C     $TASTO        WHENEQ    F12
061800160616     C                   EXSR      F12D1
061900160616     C* F18=Modifica valori assunti
062000160616     C     $TASTO        WHENEQ    F18
062100160616     C                   EXSR      F18D1
062200160616     C* F24=Altri tasti
062300160616     C     $TASTO        WHENEQ    F24
062400160616     C                   EXSR      F24D1
062500160616     C*
062600160616     C                   OTHER
062700160616     C                   EXSR      CTRD1
062800160616     C     *IN99         IFEQ      *OFF
062900160616     C* definisco l'ordinamento
063000160616     C                   EXSR      DEFORD
063100160616     C* richiamo la lista
063200160616     C                   MOVE      'S1'          $GEST
063300160616     C                   MOVE      *ON           $INZS1
063400160616     C                   MOVE      *blanks       W1POS
063500160616     C                   ENDIF
063600160616     C*
063700160616     C                   ENDSL
063800160616     C*
063900160616     C                   ENDIF
064000160616     C*
064100160616     C                   ENDSR
064200160616     C/EJECT
064300160616     C************************************************************
064400160616     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
064500160616     C************************************************************
064600160616     C     INZD1         BEGSR
064700160616     C*
064800160616     C                   CLEAR                   FMTD1
064900160616     C* Se pgm  richiamato con parametri forniti , li imposto nelle
065000160616     C*    parzializzazioni
065100160616     C                   SELECT
065200160616     C* il pgm è richiamato con chiave univoca per gestione POSTA
065300160616    >C*  nell'esempio la chiave univoca è formata dai campi
065400160616    >C*  Societa/Codice
065500160616     C*    $Unique       WHENEQ    *ON
065600160616    >C*                  MOVE      UQ1001        D1Anno
065700160616     C* il pgm è richiamato e
065800160616     C* sono stati passati dei valori di parzializzazione ma non
065900160616     C* la chiave univoca
066000160616    >C     OPZK15        WHENNE    *BLANK
066100160616     C* imposto i valori passati sui campi a video
066200160616    >C                   MOVE      AnnoK15       D1Anno
066700160616     C                   ENDSL
066800160616     C*
066900160616     C                   ENDSR
067000160616     C/EJECT
067100160616     C************************************************************
067200160616     C* GESTIONE F02 VIDEO D1
067300160616     C************************************************************
067400160616     C     F02D1         BEGSR
067500160616     C*
067600160616     C* per decodificare la videata richiamo la routine dei ctrl
067700160616     C* e poi disattivo gli eventuali messaggi di errore
067800160616     C                   EXSR      CTRD1
067900160616     C*
068000160616     C                   MOVE      *ALL'0'       IN5098
068100160616     C                   MOVEA     IN5098        *IN(50)
068200160616     C*
068300160616     C                   ENDSR
068400160616     C/EJECT
068500160616     C************************************************************
068600160616     C* GESTIONE F03 VIDEO D1
068700160616     C************************************************************
068800160616     C     F03D1         BEGSR
068900160616     C*
069000160616     C                   MOVE      *ON           $FINE
069100160616    >C                   MOVE      '1'           RETK15
069200160616     C* fine programma
069300160616     C                   EXSR      ENDPGM
069400160616     C*
069500160616     C                   ENDSR
069600160616     C/EJECT
069700160616     C************************************************************
069800160616     C* GESTIONE F05 VIDEO D1
069900160616     C************************************************************
070000160616     C     F05D1         BEGSR
070100160616     C*
070200160616     C                   MOVE      *ON           $INZD1
070300160616     C*
070400160616     C                   ENDSR
070500160616     C/EJECT
070600160616     C************************************************************
070700160616     C* GESTIONE F08 VIDEO D1
070800160616     C************************************************************
070900160616     C     F08D1         BEGSR
071000160616     C*
071100160616     C                   MOVE      *ON           $FINE
071200160616    >C                   MOVE      '0'           RETK15
071300160616     C*
071400160616     C                   ENDSR
071500160616     C/EJECT
071600160616     C************************************************************
071700160616     C* GESTIONE F10 VIDEO D1
071800160616     C************************************************************
071900160616     C     F10D1         BEGSR
072000160616     C*
072100160616    >C                   RESET                   SDGSK15DS
072200160616    >C                   MOVE      xscsoc        SOCIETAK15
072300160616    >C                   MOVEL     '01'          OPZK15
072400160616    >C     CMTK15        IFEQ      '1'
072500160616    >C                   MOVEL     '1'           CMTK15
072600160616     C                   ELSE
072700160616    >C                   MOVEL     '0'           CMTK15
072800160616     C                   ENDIF
072900160616     C* passare anche le parzializzazioni ricevute relative alla chiave
073000160616     C* univoca :
073100160616     C*  se ce ne sono, potranno essere usate come dft d'immissione
073200160616    >C                   IF        OPZK15 <> *Blank
073300160616    >C                   MOVEL     XscSoc        SocietaK15
073400160616    >C                   MOVEL     D1Anno        AnnoK15
073500160616    >C                   MOVEL     'SY'          QuaK15
073600160617    >C                   MOVEL     S1Kcc         KccK15
073700160617    >C                   MOVEL     S1Ksc         KscK15
073800160616    >C                   MOVEL     H1Rgt         RgtK15
073900160616    >C                   MOVEL     H1Prg         PrgK15
074000160617     C                   MoveL     H1C77         C77K15
074100160617     C                   MOVEL     H1Cog         COGK15
074200160617     C                   MOVEL     H1Nom         NOMK15
074300160617     C                   MoveL     H1CDE         CDEK15
074400160616     C                   ENDIF
074700160616    >C                   MOVEL     SDGSK15DS     KPJBU
074800160616    >C                   CALL      'SDGSK15R1'
074900160616     C                   PARM                    KPJBA
075000160616    >C                   MOVEL     KPJBU         SDGSK15DS
075100160616     C* ritorno da PGM gestione
075200160616    >C                   EXSR      RTN002
075300160616     C*
075400160616     C                   MOVE      *HIVAL        $LASTV
075500160616     C*
075600160616     C                   ENDSR
075700160616     C/EJECT
075800160616     C************************************************************
075900160616     C* GESTIONE F12 VIDEO D1
076000160616     C************************************************************
076100160616     C     F12D1         BEGSR
076200160616     C*
076300160616     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
076400160616     C*  per F12 torno alla lista
076500160616     C     $LOP          IFEQ      2
076600160616     C     WINKR         OREQ      *ON
076700160616     C                   MOVE      'S1'          $GEST
076800160616     C*  altrimenti, per F12 torno a menu
076900160616     C                   ELSE
077000160616     C                   MOVE      *ON           $FINE
077100160616    >C                   MOVE      '2'           RETK15
077200160616     C                   ENDIF
077300160616     C*
077400160616     C                   ENDSR
077500160616     C/EJECT
077600160616     C************************************************************
077700160616     C* GESTIONE F18 VIDEO D1
077800160616     C************************************************************
077900160616     C     F18D1         BEGSR
078000160616     C*
078100160616     C                   MOVE      'D2'          $GEST
078200160616     C                   MOVEL     *ON           $INZD2
078300160616     C*
078400160616     C                   ENDSR
078500160616     C/EJECT
078600160616     C************************************************************
078700160616     C* GESTIONE F24 VIDEO D1
078800160616     C* input
078900160616     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
079000160616     C* $FX1 =Testi tasti funzione
079100160616     C* $FC1 = Abilitazioni
079200160616     C* $FR1 = Inversioni di fondo
079300160616     C************************************************************
079400160616     C     F24D1         BEGSR
079500160616     C*
079600160616     C                   MOVEA     $FX1          $FX
079700160616     C                   MOVEA     $FC1          $FC
079800160616     C                   MOVEA     $FR1          $FR
079900160616     C                   Z-ADD     $ULKD1        $ULK
080000160616     C                   Z-ADD     $D1L01        $L01
080100160616     C                   Z-ADD     $D1L02        $L02
080200160616     C                   EXSR      RIEKEY
080300160616     C                   Z-ADD     $ULK          $ULKD1            3 0
080400160616     C                   MOVEA     $FC           $FC1
080500160616     C                   MOVEL     $KEY1         Z1KE1
080600160617     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
080700160616     C                   MOVEL     $KEY2         Z1KE2
080800160616     C                   WRITE     FMTZ1
080900160616     C*
081000160616     C*
081100160616     C                   ENDSR
081200160616     C/EJECT
081300160616     C************************************************************
081400160616     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
081500160616     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
081600160616     C* 93 94 96 97 98
081700160616     C************************************************************
081800160616     C     CTRD1         BEGSR
081900160616     C*
082000160616     C                   SETOFF                                       99
082100160616     C*
082200160616     C* Anno
082300160617     C     D1Anno        IFeq      0
082400160616     C                   SetOn                                        5199
082500160616     C                   EndIf
082600160616     C*
082700160616     C* Conto
082800160616     C                   CLEAR                   D1KScd
082900160617     C     D1KCC         IFNE      *BLANKS
083000160616     C     D1KSC         ORNE      *BLANKS
083100160616     C*
083200160616     C* xallinea
083300160616     C     D1KCC         IFNE      'C     '
083400160616     C     D1KCC         andne     'F     '
083500160616     C     D1KCC         ANDNE     *blanks
083600160616     C                   CALLB     'XALLINEA'
083700160616     C                   PARM                    D1KCC
083800160616     C                   PARM      6             LEN               5 0
083900160616     C                   PARM                    XSCINP
084000160616     C                   ENDIF
084100160616     C     D1KSC         IFNE      *BLANKS
084200160616     C                   CALLB     'XALLINEA'
084300160616     C                   PARM                    D1KSC
084400160616     C                   PARM      8             LEN               5 0
084500160616     C                   PARM                    XSCINP
084600160616     C                   Endif
084700160616     C*
084800160616     C* xcapclifor
084900160616     C                   MOVE      D1KCC         $kcc
085000160616     C                   MOVE      D1KSC         $ksc
085100160616     C     D1KCC         IFEQ      'C       '
085200160616     C     D1KCC         OREQ      'F       '
085300160616     C                   CALLB     'XCAPCLIFOR'
085400160616     C                   PARM                    XSCSOC
085500160616     C                   PARM                    $kcc
085600160616     C                   PARM                    $ksc
085700160616     C                   MOVE      $kcc          D1KCC
085800160616     C                   ENDIF
085900160616     C*
086000160616     C* Richiamo routine per controllare conto
086100160616     C                   Clear                   ND002DS
086200160616     C                   EVAL      LenOut = %Size(ND002DS)
086300160616     C                   CallB     'NDMVC002'
086400160616     C                   PARM                    XSCSOC
086500160616     C                   PARM                    $Kcc
086600160616     C                   PARM                    $Ksc
086700160616     C                   PARM      *Loval        Data
086800160616     C                   PARM      *OFF          GesMsg
086900160616     C                   PARM      *OFF          Esito
087000160616     C                   PARM      'ND002DS'     Struttura
087100160616     C                   PARM                    ND002DS
087200160616     C                   PARM                    LenOut
087300160616     C*
087400160616A1   C     Esito         IFEQ      *ON
087500160616     C                   SETON                                        5299
087600160616x1   C                   ELSE
087700160616     C*
087800160616     C                   MOVEL     DESBREV002    D1KSCD
087900160616     C*
088000160616     C* Se è c'è anche il sottoconto, deve essere percipiente
088100160616a2   C                   if        D1Ksc <> *Blanks
088200160616     C*
088300160616     C* Controllo esistenza in dvasog come fornitore percipiente
088400160616a3   C                   if        snatura002='F'
088500160616     C*
088600160616     C                   MOVE      '1'           DVARIC
088700160616     C                   MOVE      xscsoc        DVASOCIETA
088800160616     C                   MOVE      snatura002    DVASNATURA
088900160616     C                   MOVE      tpind002      DVATPIND
089000160616     C                   MOVE      cdind002      DVACDIND
089100160616     C                   MOVE      d1ksc         DVAcodice
089200160616     C                   MOVEL     'DVAFOR'      DVAstrutt
089300160616     C                   EVAL      DVALenOut = %Size(DVAFOR)
089400160616     C*
089500160616     C                   CALLB     'NDDVASOG'    PDVASOG
089600160616     C*
089700160616a4   C                   if        DVAerrore <> *ON
089800160616     C                   eval      %subst(DVAFOR:1:DVALENOUT)
089900160616     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
090000160616a5   C                   if        DVfkcca = *blank
090100160616     C* il fornitore non è un percipiente
090200160616     C                   seton                                        5499
090300160616e5   C                   endif
090400160616x4   C                   else
090500160616     C* conto inesistente
090600160616     C                   seton                                        5299
090700160616e4   C                   endif
090800160616     C*
090900160616x3   C                   else
091000160616     C*
091100160616     C* codice fornitore inesistente
091200160616     C                   seton                                        5599
091300160616     C*
091400160616e3   C                   endif
091500160616     C*
091600160616e2   C                   endif
091700160616     C*
091800160616e1   C                   endif
091900160616     C*
092000160616e0   C                   endif
092100160616     C*
092200160617     C                   If        D1Kcc         <> *BLANKS   and
092300160617     C                             D1Ksc         <> *BLANKS   and
092400160617     C                             D1CDE         <> *BLANKS
092500160617     C                   Seton                                        5699
092600160617     C                   EndiF
092700160617     C*
092800160616     C                   ENDSR
092900160616     C/EJECT
093000160616     C************************************************************
093100160616     C* DEFINIZIONE DELL'ORDINAMENTO
093200160616     C************************************************************
093300160616     C     DEFORD        BEGSR
093400160616     C*
093500160616     C* definisco l'ordinamento della lista a seconda dei valori
093600160616     C* impostati nel video di parzializzazione
093700160616     C*
093800160616     C                   SELECT
093900160616    >C*  nel caso di più viste logiche e quindi più campi di
094000160616    >C*  parzializzazione, qui vanno definiti i criteri per
094100160616    >C*  determinare quale VL va letta, a seconda di quali campi
094200160616    >C*  sono stati valorizzati
094300160616    >C     D1Anno        WHENNE    0
094400160616     C                   Z-ADD     1             $ORD
094500160616    >C*    D1Descr       WHENNE    *BLANK
094600160616     C*                  Z-ADD     2             $ORD
094700160616     C* imposto l'ordinamento 99 per ottenere in selezione solo il
094800160616     C* rcd con la chiave passata
094900160616     C     $Unique       WHENEQ    *ON
095000160616     C                   Z-ADD     99            $ORD
095100160616     C* nel caso non sia stato digitato alcun valore in parzializzazione
095200160616     C* utilizzerò l'ordinamento precedentemente memorizzato
095300160616     C* (temporaneamente o definitivamente) nei valori assunti
095400160616     C                   OTHER
095500160616     C                   ENDSL
095600160616     C                   ENDSR
095700160616     C/EJECT
095800160616     C************************************************************
095900160616     C* Chiamata programma con tasto personalizzato
096000160616     C************************************************************
096100160616     C     FPerS1        BEGSR
096200160616     C*
096300160616     C* salvo la DS di ingresso
096400160616    >C                   MOVEL     SDGSK15DS     $SaveDS
096500160616     C*
096600160616    >C                   MOVEL     SDGSK15DS     KPJBU
096700160616     C                   CALL      $PGF2(PosFun)                        22
096800160616     C                   PARM                    KPJBA
096801160617     C****               PARM                    NOMPGM
097000160616     C*
097100160616     C* ripristino la DS di ingresso
097200160616    >C                   MOVEL     $SaveDS       SDGSK15DS
097300160616     C*
097400160616     C                   ENDSR
097500160616     C/EJECT
097600160616     C************************************************************
097700160616     C* GESTIONE LISTA
097800160616     C************************************************************
097900160616     C     GESS1         BEGSR
098000160616     C* inizializzazione videata
098100160616     C     $INZS1        IFEQ      *ON
098200160616     C                   EXSR      INZS1
098300160616     C                   MOVE      *OFF          $INZS1
098400160616     C                   ENDIF
098500160616     C* emissione piede videata se proveniente da altra
098600160616     C* salvataggio valore formato di provenienza
098700160616     C     $LASTV        IFNE      'S1'
098800160616     C                   WRITE     FMTT1
098900160616     C                   EXSR      GESPIE
099000160616     C                   MOVE      $LASTV        $LASTG
099100160616     C                   MOVE      'S1'          $LASTV
099200160616     C                   ENDIF
099300160616     C*
099400160616     C     WMAX          IFEQ      0
099500160616     C                   WRITE     FMTD3                                        NO RCD
099600160616     C                   ENDIF
099700160616     C* Inizializzazione indice tasti funzione personalizzati
099800160616     C                   Eval      PosFun = 25
099900160616     C*
100000160616     C                   EXFMT     FMTC1
100100160616     C*
100200160616     C* azzero la posizione del cursore
100300160616     C                   Z-ADD     0             H1RIGA
100400160616     C                   Z-ADD     0             H1COLO
100500160616     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
100600160616     C* di refresh dello sfondo
100700160616     C                   MOVE      '1'           WExf
100800160616     C*
100900160616     C     C1NRR         IFNE      0
101000160616     C                   Z-ADD     C1NRR         WSFL
101100160616     C                   ENDIF
101200160616     C                   IF        SFLNRR > 0
101300160616     C                   Z-ADD     SFLNRR        C1RCD
101400160616     C                   ENDIF
101500160616     C* inizializzazione indice opzioni personalizzate
101600160616     C                   Eval      PosOpz = 0
101700160616     C* CONTROLLO ABILITAZIONE TASTI
101800160616     C* INPUT
101900160616     C* $FC2 = Abilitazioni videata S1
102000160616     C* OUTPUT
102100160616     C* *IN97 + *IN99 Errori
102200160616     C                   MOVEA     $FC2          $FC
102300160616     C                   EXSR      CTRKEY
102400160616     C*
102500160616     C     *IN99         IFEQ      *OFF
102600160616     C*
102700160616     C                   SELECT
102800160616     C* F3=Fine
102900160616     C     $TASTO        WHENEQ    F03
103000160616     C                   EXSR      F03S1
103100160616     C* F8=Successivo
103200160616     C     $TASTO        WHENEQ    F08
103300160616     C                   EXSR      F08S1
103400160616     C* F10=Immissione
103500160616     C     $TASTO        WHENEQ    F10
103600160616     C                   EXSR      F10S1
103700160616     C* F12=Ritorno
103800160616     C     $TASTO        WHENEQ    F12
103900160616     C                   EXSR      F12S1
104000160616     C* F17=Parzializzazioni
104100160616     C     $TASTO        WHENEQ    F17
104200160616     C                   EXSR      F17S1
104300160616     C* F18=Modifica valori assunti
104400160616     C     $TASTO        WHENEQ    F18
104500160616     C                   EXSR      F18S1
104600160616     C* F23=Altre opzioni
104700160616     C     $TASTO        WHENEQ    F23
104800160616     C                   EXSR      F23S1
104900160616     C* F24=Altri tasti
105000160616     C     $TASTO        WHENEQ    F24
105100160616     C                   EXSR      F24S1
105200160616     C* ROLL-UP
105300160616     C     $TASTO        WHENEQ    ROLLUP
105400160616     C                   EXSR      ROLS1
105500160616     C* Personalizzata
105600160616     C     $PGF2(PosFun) Whenne    *BLANK
105700160616     C                   EXSR      FperS1
105800160616     C*
105900160616     C                   OTHER
106000160616     C* CONTROLLO DATI
106100160616     C                   EXSR      CTRC1
106200160616     C     *IN99         IFEQ      *OFF
106300160616     C                   EXSR      CTRS1
106400160616     C                   ENDIF
106500160616     C*
106600160616     C                   ENDSL
106700160616     C*
106800160616     C                   ENDIF
106900160616     C*
107000160616     C                   ENDSR
107100160616     C/EJECT
107200160616     C************************************************************
107300160616     C* INIZIALIZZAZIONE LISTA
107400160616     C************************************************************
107500160616     C     INZS1         BEGSR
107600160616     C* pulizia SFL
107700160616     C                   SETOFF                                         3031
107800160616     C                   WRITE     FMTC1
107900160616     C                   SETON                                          31
107901160617     C*
107902160617     C* pulizia del sottotitolo
107903160617     C                   Clear                   D1TIT
108000160616     C*
108100160616     C* riempimento opzioni
108200160616     C                   MOVEA     $TX1          $TX
108300160616     C                   MOVEA     $FL1          $FL
108400160616     C                   Z-ADD     0             WCOZ
108500160616     C                   Z-ADD     0             WMXO
108600160616     C*  equivale al num.di elementi della schiera $TX1
108700160616     C                   Z-add     OpzLen        WMXO
108800160616     C                   Z-ADD     $C1Op1        $L01
108900160616     C                   Z-ADD     $C1Op2        $L02
109000160616     C                   EXSR      RIEOPZ
109100160616     C                   Z-ADD     23            X
109200160616     C                   MOVE      FF23          $FC2(X)
109300160616     C                   MOVEL     WOPZ1         C1OPZ1
109400160617     C                   MOVEL     $ALLOPT       H1ALLOPT
109500160616     C                   MOVEL     WOPZ2         C1OPZ2
109600160616     C                   Z-ADD     WCOZ          C1COZ
109700160616     C*
109800160616     C* CARICAMENTO SFL A PAGINE
109900160616     C*
110000160616     C                   Z-ADD     0             S1NRR
110100160616     C                   Z-ADD     0             WMAX
110200160616     C*
110300160616     C* Posizionamento su file pilota
110400160616     C                   CLEAR                   C1POS
110500160616     C                   EXSR      SETANA
110600160616     C* ripristino i valori del posizionamento
110700160616     C                   MOVE      W1POS         C1POS
110800160616    >C* aggiungere gestione altri campi posizionamento se ci sono
110900160616     C*
111000160616     C* Lettura file pilota
111100160616     C                   EXSR      REDANA
111200160616     C*
111300160616     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
111400160616     C                   IF        $LastV = 'D1'
111500160616     C                   Z-ADD     SFLPAG        WPAG
111600160616     C                   ENDIF
111700160616     C                   EXSR      ROLS1
111800160616     C* imposto il caricamento sfl del numero massimo di righe per pagina
111900160616     C                   Z-ADD     SFLPAG        WPAG
112000160616     C*
112100160616     C                   ENDSR
112200160616     C/EJECT
112300160616     C************************************************************
112400160616     C* CARICAMENTO PAGINA LISTA
112500160616     C************************************************************
112600160616     C     ROLS1         BEGSR
112700160616     C*
112800160616     C                   SETOFF                                       32
112900160616     C                   Z-ADD     0             Y
113000160616     C                   Z-ADD     WMAX          S1NRR
113100160616     C*
113200160616     C* Leggo dal file anagrafico per caricare la lista
113300160616     C*
113400160616     C     $EndFile      DOWEQ     *OFF
113500160616     C     Y             ANDLT     WPAG
113600160616     C*
113700160616     C                   EXSR      RIES1
113800160616     C*
113900160616     C                   ADD       1             S1NRR
114000160616     C                   ADD       1             Y
114100160616     C*
114200160616     C                   WRITE     FMTS1
114300160616     C*
114400160616     C                   EXSR      REDANA
114500160616     C*
114600160616     C                   ENDDO
114700160616     C*
114800160616     C                   Z-ADD     S1NRR         WMAX                 30
114900160616     C*
115000160616     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
115100160616     C*
115200160616     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
115300160616     C                   MVR                     RESTO             3 0
115400160616     C     PAGINE        MULT      SFLPAG        C1RCD
115500160616     C     RESTO         IFGT      0
115600160616     C                   ADD       1             C1RCD
115700160616     C                   ELSE
115800160616     C                   SUB       SFLPAG        C1RCD
115900160616     C                   ADD       1             C1RCD
116000160616     C                   ENDIF
116100160616     C*
116200160616     C                   ENDSR
116300160616     C/EJECT
116400160616     C************************************************************
116500160616     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
116600160616     C************************************************************
116700160616     C     SETANA        BEGSR
116800160616     C*
116900160616    >C* se tutte le VL hanno la società in chiave (ed è la principale)
117000160616    >C* si può impostare una volta per tutte al di fuori dei test
117100160616    >C                   MOVE      XscSoc        N77Societa
117200160616     C*
117300160616     C                   SELECT
117400160616     C*
117500160616     C* Gestione posizionamento
117600160616     C* Se il campo W1POS è > *blank imposto il posizionamento con
117700160616     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
117800160616     C*
117900160616    >C* Ord.1 = chiave ordinamento 1
118000160616     C     $ORD          WHENEQ    1
118100160616     C     W1POS         IFNE      *blanks
118200160616    >C                   MOVEL     W1POS         N77Cog
118300160616     C                   ELSE
118400160616    >C                   MOVEL     *blanks       N77Cog
118500160616     C                   ENDIF
118600160616    >C                   z-add     D1Anno        N77ACF
118700160616     C                   MOVEL     'SY'          N77Qua
118800160617    >C     K04N7705      SETLL     SDGN7705L
120400160616     C                   ENDSL
120500160616     C*
120600160616     C                   ENDSR
120700160616     C/EJECT
120800160616     C************************************************************
120900160616     C* LETTURA RCD ARCHIVIO PILOTA
121000160616     C************************************************************
121100160616     C     REDANA        BEGSR
121200160616     C*
121300160616     C                   MOVEL     *OFF          $EndFile
121400160616     C                   MOVEL     *OFF          $RCDOK
121500160616     C*
121600160616     C     $EndFile      DOUEQ     *ON
121700160616     C     $RCDOK        OREQ      *ON
121800160616     C*
121900160616     C                   SELECT
122000160616     C*
122100160616     C     $ORD          WHENEQ    1
122200160616    >C* per quei campi che usano XALLINEA e/o che sono a riempimento
122300160616    >C* completo (che, quindi, non hanno la gestione del range di lettura)
122400160616    >C* si può usare la READE con la chiave completa (che se è UNIQUE darà
122500160616    >C* ovviamente un rcd solo)
122600160616    >C                   MOVEL     XscSoc        N77Societa
122700160616    >C                   z-add     D1Anno        N77ACF
122800160616     C                   MOVEL     'SY'          N77Qua
122900160617    >C     K03N7705      READE     SDGN7705L                              21
123000160616     C*
125100160616     C                   ENDSL
125200160616     C*
125300160616     C                   MOVE      *IN21         $EndFile
125400160616     C*
125500160616     C     $EndFile      IFEQ      *OFF
125600160616     C                   EXSR      SELANA
125700160616     C                   ENDIF
125800160616     C*
125900160616     C                   ENDDO
126000160616     C*
126100160616     C                   MOVE      *IN21         *IN33                          SflEnd
126200160616     C*
126300160616     C                   ENDSR
126400160616     C/EJECT
126500160616     C************************************************************
126600160616     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
126700160616     C************************************************************
126800160616     C     CLCPAG        BEGSR
126900160616     C* Input :
127000160616     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
127100160616     C*          cursore
127200160616     C* - SFLPAG = numero rcd per pagina sfl
127300160616     C* Output :
127400160616     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
127500160616     C*
127600160616     C     WSFL          DIV       SFLPAG        PAGINE            4 0
127700160616     C                   MVR                     RESTO             3 0
127800160616     C     RESTO         IFGT      0
127900160616     C                   ADD       1             PAGINE
128000160616     C                   ENDIF
128100160616     C     PAGINE        MULT      SFLPAG        WPAG
128200160616     C*
128300160616     C                   ENDSR
128400160616     C/EJECT
128500160616     C************************************************************
128600160616     C* SELEZIONI SULLA RIGA LETTA
128700160616     C* impostare a *OFF il campo $RCDOK per scartare un record che
128800160616     C* non rientra nei parametri di parzializzazione
128900160616     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
129000160616     C* superiore stabilito nei parametri
129100160616     C*
129200160616     C************************************************************
129300160616     C     SELANA        BEGSR
129400160616     C*
129500160616     C                   MOVEL     *ON           $RCDOK
130400160616     C*
130500160617     C                   Clear                   WrkCDE           20
130600160617     C                   If        D1CDE <> *blanks
130700160617     C                   Select
130800160617     C                   When      N77CDE        <> *blanks
130900160617     C                   MOVEL     N77CDE        WrkCDE
131200160617     C                   EndSl
131300160617     C                   EndIf
131400160616     C*
131500160616     C                   SELECT
133500160616     C
133600160616     C     N77Kcc        WhenNE    D1Kcc
133700160616     C     D1Kcc         AndNE     *Blanks
133800160616     C                   MOVEL     *OFF          $RCDOK
133900160616     C
134000160616     C     N77Ksc        WhenNE    D1Ksc
134100160616     C     D1Ksc         AndNE     *Blanks
134200160616     C                   MOVEL     *OFF          $RCDOK
134300160617     C
134400160617     C     WrkCDE        WhenNE    D1CDE
134500160617     C     D1CDE         AndNE     *Blanks
134600160617     C                   MOVEL     *OFF          $RCDOK
134800160616     C*
135300160616     C*
135400160616     C                   ENDSL
145300160616     C*
145400160616     C                   ENDSR
145500160616     C/EJECT
145600160616     C************************************************************
145700160616     C* RIEMPIMENTO RIGA LISTA
145800160616     C************************************************************
145900160616     C     RIES1         BEGSR
146000160616     C*
146100160616     C                   CLEAR                   FMTS1
146200160616     C*
146300160616     C* riempio i campi del sfl, compresi gli hidden con i valori
146400160616     C* del rcd letto
146500160616    >C     N77Cog        Cat       N77Nom:1      S1Descr
146600160616    >C                   MOVEL     N77KCC        S1KCC
146700160616    >C                   MOVEL     N77KSC        S1KSC
146800160616    >C                   MOVEL     N77Qua        S1Qua
146900160616    >C                   MOVEL     N77RGT        H1Rgt
147000160616    >C                   MOVEL     N77PRG        H1Prg
147100160617     C                   MoveL     N77C77        H1C77
147200160616    >C                   MOVEL     N77COG        H1COG
147300160616    >C                   MOVEL     N77NOM        H1NOM
147800160617     C                   If        N77CDE        <> *blanks
147900160617     C                   MOVEL     N77CDE        S1CDE
147901160617     C                   MOVEL     N77CDE        H1CDE
148100160617     C                   EndIf
148600160616     C*
148700160616     C* imposto quegli attributi del sfl che sono modificabili
148800160616     C* al richiamo del determinato pgm
148900160616     C                   EXSR      ATRS1
149000160616     C*
149100160616     C                   ENDSR
149200160616     C/EJECT
152100160616     C************************************************************
152200160616     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
152300160616     C************************************************************
152400160616     C     ATRS1         BEGSR
152500160616     C*
152600160616     C* in questa routine bisogna attivare indicatori o campi programma
152700160616     C* che gestiscono gli attributi di uno o più campi di questa videata;
152800160616     C* attenzione : non è questo il posto in cui eseguire controlli,
152900160616     C*  dovrebbero già arrivare dei flag
153700160616     C*
153800160616     C                   ENDSR
153900160616     C/EJECT
154000160616     C************************************************************
154100160616     C* GESTIONE F08 VIDEO S1
154200160616     C************************************************************
154300160616     C     F08S1         BEGSR
154400160616     C*
154500160616     C                   MOVE      *ON           $FINE
154600160616    >C                   MOVE      '0'           RETK15
154700160616     C*
154800160616     C                   ENDSR
154900160616     C/EJECT
155000160616     C************************************************************
155100160616     C* GESTIONE F03 VIDEO S1
155200160616     C************************************************************
155300160616     C     F03S1         BEGSR
155400160616     C*
155500160616     C                   MOVE      *ON           $FINE
155600160616    >C                   MOVE      '1'           RETK15
155700160616     C* fine programma
155800160616     C                   EXSR      ENDPGM
155900160616     C*
156000160616     C                   ENDSR
156100160616     C/EJECT
156200160616     C************************************************************
156300160616     C* GESTIONE F10 VIDEO S1
156400160616     C* AGGIUNTA RECORD
156500160616     C************************************************************
156600160616     C     F10S1         BEGSR
156700160616     C*
156800160616    >C                   RESET                   SDGSK15DS
156900160616    >C                   MOVE      xscsoc        SocietaK15
157000160616    >C                   MOVEL     '01'          OPZK15
157100160616    >C     CMTK15        IFEQ      '1'
157200160616    >C                   MOVEL     '1'           CMTK15
157300160616     C                   ELSE
157400160616    >C                   MOVEL     '0'           CMTK15
157500160616     C                   ENDIF
157600160616     C* passare anche le parzializzazioni ricevute relative alla chiave
157700160616     C* univoca :
157800160616     C*  se ce ne sono, potranno essere usate come dft d'immissione
157900160616    >C                   IF        OPZK15 <> *Blank
158000160616    >C                   MOVEL     D1anno        AnnoK15
158100160616     C                   MOVEL     'SY'          QuaK15
158200160617    >C                   MOVEL     S1Kcc         KccK15
158300160617    >C                   MOVEL     S1Ksc         KscK15
158400160616    >C                   MOVEL     H1Rgt         RgtK15
158500160616    >C                   MOVEL     H1PRG         PrgK15
158600160617     C                   MoveL     H1C77         C77K15
158700160616    >C                   MOVEL     H1COG         CogK15
158800160616    >C                   MOVEL     H1NOM         NomK15
158900160617     C                   MoveL     S1CDE         CDEK15
159000160616     C                   ENDIF
159300160616    >C                   MOVEL     SDGSK15DS     KPJBU
159400160616    >C                   CALL      'SDGSK15R1'
159500160616     C                   PARM                    KPJBA
159600160616    >C                   MOVEL     KPJBU         SDGSK15DS
159700160616     C* ritorno da PGM gestione
159800160616    >C                   EXSR      RTN002
159900160616     C     WINZS1        IFEQ      *ON
160000160616     C                   MOVE      *ON           $INZS1
160100160616     C* se esistevano già righe sul sfl
160200160616     C* calcolo pagina a cui deve posizionarsi
160300160616     C     WSFL          IFGT      0
160400160616     C                   EXSR      CLCPAG
160500160616     C* altrimenti carico solo la 1a pagina
160600160616     C                   ELSE
160700160616     C                   Z-ADD     SFLPAG        WPAG
160800160616     C                   ENDIF
160900160616     C                   ENDIF
161000160616     C*
161100160616     C                   MOVE      *HIVAL        $LASTV
161200160616     C*
161300160616     C                   ENDSR
161400160616     C/EJECT
161500160616     C************************************************************
161600160616     C* GESTIONE F12 VIDEO S1
161700160616     C************************************************************
161800160616     C     F12S1         BEGSR
161900160616     C*
162000160616     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
162100160616     C     $LOP          IFEQ      1
162200160616     C                   MOVE      'D1'          $GEST
162300160616     C*  se la 1a videata è questa, per F12 torno a menu
162400160616     C                   ELSE
162500160616     C                   MOVE      *ON           $FINE
162600160616    >C                   MOVE      '2'           RETK15
162700160616     C                   ENDIF
162800160616     C*
162900160616     C                   ENDSR
163000160616     C/EJECT
163100160616     C************************************************************
163200160616     C* GESTIONE F17 VIDEO S1
163300160616     C************************************************************
163400160616     C     F17S1         BEGSR
163500160616     C*
163600160616     C                   MOVE      'D1'          $GEST
163700160616     C* se il pgm è stato richiamato con chiave univoca
163800160616     C* e dopo è stata richiesta una parzializzazione
163900160616     C* tolgo la gestione della chiave univoca (azzere/abblenco il
164000160616     C* valore; propongo come ordinamento il 1°)
164100160616     C     $Unique       IFEQ      *ON
164200160616     C                   MOVE      *OFF          $Unique
164400160616     C                   ENDIF
164500160616     C     $ORD          IFEQ      99
164600160616     C                   Z-ADD     1             $ORD
164700160616     C                   MOVE      *ON           $INZD1
164800160616     C                   ENDIF
164900160616     C*
165000160616     C                   ENDSR
165100160616     C/EJECT
165200160616     C************************************************************
165300160616     C* GESTIONE F18 VIDEO S1
165400160616     C************************************************************
165500160616     C     F18S1         BEGSR
165600160616     C*
165700160616     C                   MOVE      'D2'          $GEST
165800160616     C                   MOVEL     *ON           $INZD2
165900160616     C     $ORD          IFEQ      99
166000160616     C                   Z-ADD     1             $ORD
166100160616     C                   ENDIF
166200160616     C*
166300160616     C                   ENDSR
166400160616     C/EJECT
166500160616     C************************************************************
166600160616     C* GESTIONE F23 VIDEO S1
166700160616     C************************************************************
166800160616     C     F23S1         BEGSR
166900160616     C*
167000160616     C                   Z-ADD     $C1Op1        $L01
167100160616     C                   Z-ADD     $C1Op2        $L02
167200160616     C                   EXSR      RIEOPZ
167300160616     C                   Z-ADD     23            X
167400160616     C                   MOVE      FF23          $FC2(X)
167500160616     C                   MOVEL     WOPZ1         C1OPZ1
167600160617     C                   MOVEL     $ALLOPT       H1ALLOPT
167700160616     C                   MOVEL     WOPZ2         C1OPZ2
167800160616     C                   Z-ADD     WCOZ          C1COZ
167900160616     C*
168000160616     C                   ENDSR
168100160616     C/EJECT
168200160616     C************************************************************
168300160616     C* GESTIONE F24 VIDEO S1
168400160616     C************************************************************
168500160616     C     F24S1         BEGSR
168600160616     C*
168700160616     C                   MOVEA     $FX2          $FX
168800160616     C                   MOVEA     $FC2          $FC
168900160616     C                   MOVEA     $FR2          $FR
169000160616     C                   Z-ADD     $ULKS1        $ULK
169100160616     C                   Z-ADD     $S1L01        $L01
169200160616     C                   Z-ADD     $S1L02        $L02
169300160616     C                   EXSR      RIEKEY
169400160616     C                   Z-ADD     $ULK          $ULKS1            3 0
169500160616     C                   MOVEA     $FC           $FC2
169600160616     C                   MOVEL     $KEY1         Z2KE1
169700160617     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
169800160616     C                   MOVEL     $KEY2         Z2KE2
169900160616     C                   WRITE     FMTZ2
170000160616     C*
170100160616     C                   ENDSR
170200160616     C/EJECT
170300160616     C************************************************************
170400160616     C* CONTROLLO TESTATA LISTA
170500160616     C************************************************************
170600160616     C     CTRC1         BEGSR
170700160616     C*
170800160616     C                   MOVE      *OFF          *IN99
170900160616     C*
171000160616     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
171100160616     C* L'INIZIALIZZAZIONE LISTA
171200160616     C*
171300160616     C     C1POS         IFNE      W1POS
171400160616    >C* aggiungere in OR altri campi posizionamento se ci sono
171500160616     C                   EXSR      POSS1
171600160616     C                   ENDIF
171700160616     C*
171800160616     C                   ENDSR
171900160616     C/EJECT
172000160616     C************************************************************
172100160616     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
172200160616     C************************************************************
172300160616     C     POSS1         BEGSR
172400160616     C*
172500160616     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
172600160616     C*
172700160616   B1C     WMAX          IFGT      0
172800160616     C                   READC     FMTS1                                  21
172900160616   B2C     *IN21         DOWEQ     *OFF
173000160616     C     *IN99         ANDEQ     *OFF
173100160616     C*
173200160616   B3C     S1OPZ         IFNE      0
173300160616     C                   SETON                                        92  99
173400160616   E3C                   ENDIF
173500160616     C* se rilevata opzione attiva, attivo sflnxtchg
173600160616   B3C     *IN99         IFEQ      *ON
173700160616     C                   MOVE      *ON           *IN32
173800160616     C*
173900160616     C* imposto quegli attributi del sfl che sono modificabili
174000160616     C* al richiamo del determinato pgm
174100160616     C                   EXSR      ATRS1
174200160616     C*
174300160616     C                   UPDATE    FMTS1
174400160616   X3C                   ELSE
174500160616     C                   READC     FMTS1                                  21
174600160616   E3C                   ENDIF
174700160616     C*
174800160616   E2C                   ENDDO
174900160616     C*
175000160616   E1C                   ENDIF
175100160616     C*
175200160616     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
175300160616     C* AL VALORE DI POSIZIONAMENTO :
175400160616     C* se questo è maggiore del valore del rispettivo campo di
175500160616     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
175600160616     C* con i valori di parzializzazione.
175700160616     C*
175800160616   B1C     *IN99         IFEQ      *OFF
175900160616    >C                   MOVE      *blanks       W1POS
176000160616     C*
176100160616     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
176200160616     C* è maggiore del relativo valore di parzializzazione, nel qual caso
176300160616     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
176400160616     C* richiedo comunque un ricaricamento del sfl che, se non si è
176500160616     C* verificata la condizione suddetta avverrà con posizionamento a
176600160616     C* partire dal valore di parzializzazione
176700160616     C*
176800160616   B2C                   SELECT
176900160616    >C* Attenzione : se un ordinamento ha un campo di posizionamento
177000160616    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
177100160616    >C* dei relativi campi
177200160616     C*
177300160616     C     $ORD          WHENEQ    1
177400160616    >C     C1POS         IFGT      *blanks
177500160616    >C                   MOVEL     C1POS         W1POS
178200160616     C                   ENDIF
179500160616     C*
179600160616   E2C                   ENDSL
179700160616     C*
179800160616     C* richiedo l'inizializzazione sfl
179900160616     C                   MOVE      *ON           $INZS1
180000160616     C*
180100160616   E1C                   ENDIF
180200160616     C*
180300160616     C                   ENDSR
180400160616     C/EJECT
180500160616     C************************************************************
180600160616     C* CONTROLLO OPZIONI LISTA
180700160616     C************************************************************
180800160616     C     CTRS1         BEGSR
180900160616     C*
181000160616     C                   MOVEL     *OFF          $ESCI
181100160616     C                   SETOFF                                       99
181200160616     C                   Z-ADD     0             S1OPZ
181300160616     C*
181400160616     C* Leggo il sfl solo se ci sono rcd
181500160616   B1C     WMAX          IFGT      0
181600160616     C                   READC     FMTS1                                  21
181700160616     C*
181800160616     C* esce se fine sfl o errore che richiede l'uscita
181900160616   B2C     *IN21         DOWEQ     *OFF
182000160616     C     $ESCI         ANDEQ     *OFF
182100160616     C                   Z-ADD     S1NRR         C1RCD
182200160616     C* ctrl su riga
182300160616     C                   EXSR      RECS1
182400160616     C* gestione opzioni
182500160616   B3C     S1OPZ         IFNE      0
182600160616     C     *IN99         ANDEQ     *OFF
182700160616     C                   EXSR      OPZS1
182800160616   E3C                   ENDIF
182900160616     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
183000160616   B3C     *IN99         IFEQ      *ON
183100160616     C                   MOVE      *ON           *IN32
183200160616   E3C                   ENDIF
183300160616     C*
183400160616     C* imposto quegli attributi del sfl che sono modificabili
183500160616     C* al richiamo del determinato pgm
183600160616     C                   EXSR      ATRS1
183700160616     C*
183800160616     C                   Z-ADD     0             S1OPZ
183900160616     C*
184000160616     C                   UPDATE    FMTS1
184100160616     C                   MOVE      *OFF          *IN32
184200160616     C*
184300160616     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
184400160616   B3C     $ESCI         IFEQ      *OFF
184500160616     C                   READC     FMTS1                                  21
184600160616     C* a fine ciclo ripristino il flag richiesta iniz.sfl
184700160616   B4C     *IN21         IFEQ      *ON
184800160616     C                   MOVE      WINZS1        $INZS1
184900160616     C* calcolo pagina a cui deve posizionarsi
185000160616     C     WSFL          IFGT      0
185100160616     C                   EXSR      CLCPAG
185200160616     C                   ELSE
185300160616     C                   Z-ADD     SFLPAG        WPAG
185400160616     C                   ENDIF
185500160616   E4C                   ENDIF
185600160616   E4C                   ENDIF
185700160616     C*
185800160616   E2C                   ENDDO
185900160616     C*
186000160616   E1C                   ENDIF
186100160616     C*
186200160616     C                   ENDSR
186300160616     C/EJECT
186400160616     C************************************************************
186500160616     C* CONTROLLO CAMPI I/O RIGA LISTA
186600160616     C************************************************************
186700160616     C     RECS1         BEGSR
186800160616     C*
186900160616     C* reset indicatori DSPATR
187000160616     C                   MOVE      *ALL'0'       IN4049           10
187100160616     C                   MOVEA     IN4049        *IN(40)
187200160616     C*
187300160616     C* Controllo se Utente Autorizzato alla scelta
187400160616     C     S1OPZ         IFNE      0
187500160616     C                   Z-ADD     1             X
187600160616     C     S1OPZ         LOOKUP    $OP1(X)                                21
187700160616     C* mi salvo l'indice dell' opzione
187800160616     C   21              Z-add     X             PosOpz            2 0
187900160616     C     *IN21         IFEQ      *OFF
188000160616     C     $FL1(X)       OREQ      'N'
188100160616     C                   MOVE      *ON           $ESCI
188200160616     C                   SETON                                        409199
188300160616     C                   ENDIF
188400160616     C                   ENDIF
188500160616     C*
188600160616     C                   ENDSR
188700160616     C/EJECT
188800160616     C************************************************************
188900160616     C* GESTIONE OPZIONI LISTA
189000160616     C************************************************************
189100160616     C     OPZS1         BEGSR
189200160616     C*
189300160616     C                   Z-ADD     RIRI          CURR
189400160616     C                   Z-ADD     COCO          CURC
189500160616     C*
189600160616     C                   SELECT
189700160616     C*
189800160616     C* se opz=Selezione
189900160616     C* passo la chiave al pgm chiamante
190000160616     C     S1OPZ         WHENEQ    1
190100160616    >C                   MOVE      S1OPZ         OPZK15
190200160616     C* passare nei campi appositi della DS SDGSK15DS il valore
190300160616     C* della chiave della VL univoca (se c'è)
190400160616    >C                   MOVE      D1Anno        AnnoK15
190500160616    >C                   MOVE      'SY'          QuaK15
190600160616     C                   MOVE      *ON           $ESCI
190700160616     C                   MOVE      *ON           $FINE
190800160616     C*
190900160616     C                   MOVE      *HIVAL        $LASTV
196800160616     C*
196900160616     C* se selezionata opzione personalizzata, richiamo il programma
197000160616     C* corrispondente
197100160616     C     $PGO1(PosOpz) Whenne    *Blank
197200160616    >C                   RESET                   SDGSK15DS
197300160616    >C                   MOVE      xscsoc        SocietaK15
197400160616    >C                   MOVEL     S1OPZ         OPZK15
197500160616    >C     CMTK15        IFEQ      '1'
197600160616    >C                   MOVEL     '1'           CMTK15
197700160616     C                   ELSE
197800160616    >C                   MOVEL     '0'           CMTK15
197900160616     C                   ENDIF
198000160616     C* passare nei campi appositi della DS SDGSK15DS il valore
198100160616     C* della chiave della VL univoca
198200160616    >C                   MOVEL     D1anno        AnnoK15
198300160616    >C                   MOVEL     'SY'          QuaK15
198400160616     C*
198500160616    >C                   MOVEL     SDGSK15DS     KPJBU
198600160616     C                   CALL      $PGO1(PosOpz)                        22
198700160616     C                   PARM                    KPJBA
198800160617     C*****              PARM                    NOMPGM
198900160616     C*
199000160616    >C                   MOVEL     KPJBU         SDGSK15DS
199100160616     C* ritorno da PGM
199200160616     C                   EXSR      RTN002
199300160616      *
199400160616      *
199500160616      * se altre opzioni
199600160616      * richiamo pgm gestione anagrafica
199700160616     C                   OTHER
199800160616     C*
199900160616    >C                   RESET                   SDGSK15DS
200000160616    >C                   MOVE      xscsoc        SocietaK15
200100160616    >C                   MOVEL     S1OPZ         OPZK15
200200160616    >C     CMTK15        IFEQ      '1'
200300160616    >C                   MOVEL     '1'           CMTK15
200400160616     C                   ELSE
200500160616    >C                   MOVEL     '0'           CMTK15
200600160616     C                   ENDIF
200700160616     C* passare nei campi appositi della DS SDGSK15DS il valore
200800160616     C* della chiave della VL univoca
200900160616    >C                   MOVEL     D1Anno        AnnoK15
201000160616    >C                   MOVEL     'SY'          QuaK15
201100160617    >C                   MOVEL     S1Kcc         KccK15
201200160617    >C                   MOVEL     S1Ksc         KscK15
201300160616    >C                   MOVEL     H1RGT         RGTK15
201400160616    >C                   MOVEL     H1PRG         PRGK15
201500160617     C                   MoveL     H1C77         C77K15
201600160617     C                   MOVEL     H1Cog         COGK15
201700160617     C                   MOVEL     H1Nom         NOMK15
201800160617     C                   MoveL     H1CDE         CDEK15
201900160616     C*
202000160616    >C                   MOVEL     SDGSK15DS     KPJBU
202100160616    >C                   CALL      'SDGSK15R1'
202200160616     C                   PARM                    KPJBA
202300160616    >C** se vengono gestite le autorizzazioni
202400160616    >C** le passo al pgm di manutenzione per evitargli di doversele
202500160616    >C** riprendere
202600160616    >C*                  PARM                    AUTDS
202700160616     C*
202800160616    >C                   MOVEL     KPJBU         SDGSK15DS
202900160616     C*
203000160616     C* ritorno da PGM gestione
203100160616    >C                   EXSR      RTN002
203200160616     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
203300160616     C     *IN99         IFEQ      *ON
203400160616     C                   SETON                                        40
203500160616     C                   ENDIF
203600160616     C*
203700160616     C                   MOVE      *HIVAL        $LASTV
203800160616     C                   ENDSL
203900160616     C*
204000160616     C                   ENDSR
204100160616     C/EJECT
204200160616     C************************************************************
204300160616     C* GESTIONE RITORNO DA trasmisisone anagrafica factor/debitore
204400160616     C************************************************************
204500160616    >C     RTNU30        BEGSR
204600160616     C*
204700160616     C* modo di ritorno
204800160616     C*
204900160616     C                   SELECT
205000160616     C* 1 = errori riscontrati durante l'estrazione debitori
205100160616    >C     ERRK15        WHENEQ    '1'
205200160616     C                   SETON                                        5399      emiss.SFLMSGID
205300160616     C* 0 = funzione richiamata andata a buon fine
205400160616    >C     ERRK15        WHENEQ    '0'
205500160616    >C*  eventualmente gestire altri codici di errore
205600160616     C*
205700160616     C                   ENDSL
205800160616     C*
205900160616     C                   ENDSR
206000160616     C************************************************************
206100160616     C* GESTIONE RITORNO DA PGM DI GESTIONE
206200160616     C************************************************************
206300160616    >C     RTN002        BEGSR
206400160616     C*
206500160616     C* modo di ritorno
206600160616     C*
206700160616     C                   SELECT
206800160616     C* questi modi di utilizzo dei valori di ritorno dal
206900160616     C* pgm di manutenzione rcd di anagrafica sono delle
207000160616     C* proposte, normalmente sempre valide, ma modificabili
207100160616     C* per situazioni particolari
207200160616     C* 1 = F3
207300160616    >C     RETK15        WHENEQ    '1'
207400160616     C                   MOVE      *ON           $ESCI                          sospese READC
207500160616     C                   MOVE      *ON           $FINE                          fine pgm
207600160616     C                   MOVE      '1'           RETK15
207700160616     C* 2 = F12
207800160616    >C     RETK15        WHENEQ    '2'
207900160616     C* se il pgm di manutenzione ha prima una guida, è necessario anche
208000160616     c* questo test, altrimenti prevale il ret002 e il sfl non viene
208100160616     C* ricaricato
208200160616     C     OPRK15        ANDNE     '1'
208300160616     C                   MOVE      *ON           $ESCI                          sospese READC
208400160616     C*
208500160616     C                   ENDSL
208600160616     C*
208700160616     C* operazione eseguite dal pgm chiamato
208800160616     C*
208900160616    >C                   MOVE      OPRK15        WOPR                           avverto pgm chiamant
209000160616     C                   SELECT
209100160616     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
209200160616    >C     OPRK15        WHENEQ    '1'
209300160616     C*
209400160616     C* se l'opzione con cui è stato chiamato non è la modifica
209500160616     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
209600160616    >C     OPZK15        IFNE      '02'
209700160616     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
209800160616     C* se l'opzione con cui è stato chiamato è la modifica
209900160616     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
210000160616     C                   ELSE
210100160616     C                   EXSR      AGGS1
210200160616     C                   ENDIF
210300160616     C*
210400160616     C                   ENDSL
210500160616     C*
210600160616     C* funzione non eseguibile per errore :
210700160616     C*
210800160616     C                   SELECT
210900160616     C* 1 = funzione richiamata chiusa in errore
211000160616    >C     ERRK15        WHENEQ    '1'
211100160616     C                   MOVE      *ON           $ESCI                          sospese READC
211200160616     C                   SETON                                        9399      emiss.SFLMSGID
211300160616     C* 2 = funzione richiamata senza la dovuta autorizzazione
211400160616    >C     ERRK15        WHENEQ    '2'
211500160616     C                   MOVE      *ON           $ESCI                          sospese READC
211600160616     C                   SETON                                        9499      emiss.SFLMSGID
211700160616    >C*  eventualmente gestire altri codici di errore
211800160616     C*
211900160616     C                   ENDSL
212000160616     C*
212100160616     C                   ENDSR
212200160616     C/EJECT
212300160616     C************************************************************
212400160616     C* AGGIORNAMENTO RIGA LISTA
212500160616     C************************************************************
212600160616     C     AGGS1         BEGSR
212700160616     C*
212800160616     C* riempio i campi del sfl che sono stati ripassati dal pgm
212900160616     C* di manutenzione dopo una opzione 02
213000160616     C*
213200160616    >C                   MOVEL     QuaK15        S1Qua
213300160617    >C                   MOVEL     KCcK15        S1KCC
213400160617    >C                   MOVEL     KScK15        S1KSC
213500160616    >C                   MOVEL     RGTK15        H1Rgt
213600160616    >C                   MOVEL     PRGK15        H1PRG
213700160617     C                   MoveL     C77K15        H1C77
213800160617     C                   MOVEL     COGK15        H1COG
213900160617     C                   MOVEL     NOMK15        H1NOM
214000160617     C                   MoveL     CDEK15        H1CDE
214100160617     C                   MoveL     CDEK15        S1CDE
214200160617     C     H1Cog         Cat       H1Nom:1       S1Descr
214300160616     C*
214400160616     C                   ENDSR
214500160616     C/EJECT
224300160616     C************************************************************
224400160616     C* GESTIONE VALORI ASSUNTI
224500160616     C************************************************************
224600160616     C     GESD2         BEGSR
224700160616     C*
224800160616   B1C     $INZD2        IFEQ      *ON
224900160616     C                   EXSR      INZD2
225000160616     C                   MOVE      *OFF          $INZD2
225100160616   E1C                   ENDIF
225200160616     C*
225300160616   B1C     $LASTV        IFNE      'D2'
225400160616     C                   EXSR      GESPIE
225500160616   B1C     $LASTV        IFNE      *HIVAL
225600160616     C                   MOVE      $LASTV        $LASTG
225700160616     C                   ENDIF
225800160616     C                   MOVE      'D2'          $LASTV
225900160616   E1C                   ENDIF
226000160616     C*
226100160616     C* se esistono errori sulla videata
226200160616     C* emetto la write del formato a indicatori spenti per vedere
226300160616     C* le eventuali decodifiche
226400160616   B1C     *IN99         IFEQ      *ON
226500160616     C                   MOVEA     *IN           WIN
226600160616     C                   MOVE      *ALL'0'       IN5098           49
226700160616     C                   MOVEA     IN5098        *IN(50)
226800160616     C                   WRITE     FMTD2
226900160616     C                   MOVEA     WIN           *IN
227000160616   E1C                   ENDIF
227100160616     C*
227200160616     C                   EXFMT     FMTD2
227300160616     C*
227400160616     C* azzero la posizione del cursore
227500160616     C                   Z-ADD     0             H1RIGA
227600160616     C                   Z-ADD     0             H1COLO
227700160616     C*
227800160616     C* CONTROLLO ABILITAZIONE TASTI
227900160616     C* INPUT
228000160616     C* $FC3 = Abilitazioni videata D2
228100160616     C* OUTPUT
228200160616     C* *IN97 + *IN99 Errori
228300160616     C                   MOVEA     $FC3          $FC
228400160616     C                   EXSR      CTRKEY
228500160616     C*
228600160616   B1C     *IN99         IFEQ      *OFF
228700160616     C*
228800160616   B2C                   SELECT
228900160616     C* F3=Fine
229000160616     C     $TASTO        WHENEQ    F03
229100160616     C                   EXSR      F03D2
229200160616     C* F12=Ritorno
229300160616     C     $TASTO        WHENEQ    F12
229400160616     C                   EXSR      F12D2
229500160616     C* F24=Altri tasti
229600160616     C     $TASTO        WHENEQ    F24
229700160616     C                   EXSR      F24D2
229800160616     C*
229900160616     C                   OTHER
230000160616     C                   EXSR      CTRD2
230100160616   B3C     *IN99         IFEQ      *OFF
230200160616     C                   MOVE      $LASTG        $GEST
230300160616   B4C                   SELECT
230400160616     C     $LASTG        WHENEQ    'S1'
230500160616     C* se è stato modificato l'ordinamento di default, ricarico la lista
230600160616   B5C     $ORD          IFNE      D2ORD
230700160616     C                   MOVE      *ON           $INZS1
230800160616     C                   MOVE      *blanks       W1POS
230900160616   E5C                   ENDIF
231000160616     C     $LASTG        WHENEQ    'D1'
231100160616     C* per ora niente
231200160616   E4C                   ENDSL
231300160616     C* definisco i nuovi valori di default
231400160616     C                   Z-ADD     D2LOP         $LOP
231500160616     C                   Z-ADD     D2ORD         $ORD
231700160616    >C*  elenco altri dati default specifici dell'anagrafica
231800160616     C*
231900160616     C* aggiorno valori assunti
232000160616     C* se richiesto il salvataggio
232100160616   B4C     D2BSV         IFEQ      1
232200160616     C                   EXSR      AGGMPE
232300160616   E4C                   ENDIF
232400160616     C*
232500160616   E3C                   ENDIF
232600160616     C*
232700160616   E2C                   ENDSL
232800160616     C*
232900160616   E1C                   ENDIF
233000160616     C*
233100160616     C                   ENDSR
233200160616     C/EJECT
233300160616     C************************************************************
233400160616     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
233500160616     C************************************************************
233600160616     C     INZD2         BEGSR
233700160616     C*
233800160616     C                   CLEAR                   FMTD2
233900160617     C                   EVAL      D2BSVA = Normale
234000160616     C* se sono entrato con chiave univoca
234100160616     C* impedisco il salvataggio valori assunti
234200160616     C     $Unique       IFEQ      *ON
234400160617     C                   EVAL      D2BSVA = Protetto
234500160616     C                   ENDIF
234600160616     C                   MOVEL     $LOP          D2LOP
234700160616     C                   MOVE      $ORD          D2ORD
234900160616    >c*  elenco altri dati default specifici dell'anagrafica
235000160616     C*
235100160616     C                   ENDSR
235200160616     C/EJECT
235300160616     C************************************************************
235400160616     C* GESTIONE F03 VIDEO D2
235500160616     C************************************************************
235600160616     C     F03D2         BEGSR
235700160616     C*
235800160616     C                   MOVE      *ON           $FINE
235900160616     C* fine programma
236000160616     C                   EXSR      ENDPGM
236100160616     C*
236200160616     C                   ENDSR
236300160616     C/EJECT
236400160616     C************************************************************
236500160616     C* GESTIONE F12 VIDEO D2
236600160616     C************************************************************
236700160616     C     F12D2         BEGSR
236800160616     C*
236900160616     C                   MOVE      $LASTG        $GEST
237000160616     C*
237100160616     C                   ENDSR
237200160616     C/EJECT
237300160616     C************************************************************
237400160616     C* GESTIONE F24 VIDEO D2
237500160616     C************************************************************
237600160616     C     F24D2         BEGSR
237700160616     C*
237800160616     C                   MOVEA     $FX3          $FX
237900160616     C                   MOVEA     $FC3          $FC
238000160616     C                   MOVEA     $FR3          $FR
238100160616     C                   Z-ADD     $ULKD2        $ULK
238200160616     C                   Z-ADD     $D2L01        $L01
238300160616     C                   Z-ADD     $D2L02        $L02
238400160616     C                   EXSR      RIEKEY
238500160616     C                   Z-ADD     $ULK          $ULKD2            3 0
238600160616     C                   MOVEA     $FC           $FC3
238700160616     C                   MOVEL     $KEY1         Z3KE1
238800160617     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
238900160616     C                   WRITE     FMTZ3
239000160616     C*
239100160616     C                   ENDSR
239200160616     C/EJECT
239300160616     C************************************************************
239400160616     C* CONTROLLO VIDEATA VALORI ASSUNTI
239500160616     C************************************************************
239600160616     C     CTRD2         BEGSR
239700160616     C*
239800160616     C                   SETOFF                                       99
239900160616     C*
240000160616     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
240100160616     C     D2LOP         IFEQ      0
240200160616     C                   SETON                                          5099
240300160616     C                   ENDIF
240400160616     C*
240500160616     C* OBBLIGATORIO VALORE PER ORDINAMENTO
240600160616     C     D2ORD         IFEQ      0
240700160616     C                   SETON                                          5199
240800160616     C                   ENDIF
243100160616     C*
243200160616     C                   ENDSR
243300160616     C/EJECT
243400160616     C************************************************************
243500160616     C* CONTROLLO ABILITAZIONE TASTI
243600160616     C* INPUT
243700160616     C* $TASTO =Tasto funzione premuto
243800160616     C* $FC = Tasti abilitati
243900160616     C* OUTPUT
244000160616     C* IN99 = ERrore
244100160616     C************************************************************
244200160616     C     CTRKEY        BEGSR
244300160616     C*
244400160616     C                   SETOFF                                       99
244500160616     C*
244600160616     C                   SELECT
244700160616     C     $TASTO        WHENEQ    F01
244800160616     C                   z-add     01            posfun
244900160616     C     $FC(1)        IFEQ      '0'
245000160616     C                   SETON                                        9997
245100160616     C                   ENDIF
245200160616     C     $TASTO        WHENEQ    F02
245300160616     C                   z-add     02            posfun
245400160616     C     $FC(2)        IFEQ      '0'
245500160616     C                   SETON                                        9997
245600160616     C                   ENDIF
245700160616     C     $TASTO        WHENEQ    F03
245800160616     C                   z-add     03            posfun
245900160616     C     $FC(3)        IFEQ      '0'
246000160616     C                   SETON                                        9997
246100160616     C                   ENDIF
246200160616     C     $TASTO        WHENEQ    F04
246300160616     C                   z-add     04            posfun
246400160616     C     $FC(4)        IFEQ      '0'
246500160616     C                   SETON                                        9997
246600160616     C                   ENDIF
246700160616     C     $TASTO        WHENEQ    F05
246800160616     C                   z-add     05            posfun
246900160616     C     $FC(5)        IFEQ      '0'
247000160616     C                   SETON                                        9997
247100160616     C                   ENDIF
247200160616     C     $TASTO        WHENEQ    F06
247300160616     C                   z-add     06            posfun
247400160616     C     $FC(6)        IFEQ      '0'
247500160616     C                   SETON                                        9997
247600160616     C                   ENDIF
247700160616     C     $TASTO        WHENEQ    F07
247800160616     C                   z-add     07            posfun
247900160616     C     $FC(7)        IFEQ      '0'
248000160616     C                   SETON                                        9997
248100160616     C                   ENDIF
248200160616     C     $TASTO        WHENEQ    F08
248300160616     C                   z-add     08            posfun
248400160616     C     $FC(8)        IFEQ      '0'
248500160616     C                   SETON                                        9997
248600160616     C                   ENDIF
248700160616     C     $TASTO        WHENEQ    F09
248800160616     C                   z-add     09            posfun
248900160616     C     $FC(09)       IFEQ      '0'
249000160616     C                   SETON                                        9997
249100160616     C                   ENDIF
249200160616     C     $TASTO        WHENEQ    F10
249300160616     C                   z-add     10            posfun
249400160616     C     $FC(10)       IFEQ      '0'
249500160616     C                   SETON                                        9997
249600160616     C                   ENDIF
249700160616     C     $TASTO        WHENEQ    F11
249800160616     C                   z-add     11            posfun
249900160616     C     $FC(11)       IFEQ      '0'
250000160616     C                   SETON                                        9997
250100160616     C                   ENDIF
250200160616     C     $TASTO        WHENEQ    F12
250300160616     C                   z-add     12            posfun
250400160616     C     $FC(12)       IFEQ      '0'
250500160616     C                   SETON                                        9997
250600160616     C                   ENDIF
250700160616     C     $TASTO        WHENEQ    F13
250800160616     C                   z-add     13            posfun
250900160616     C     $FC(13)       IFEQ      '0'
251000160616     C                   SETON                                        9997
251100160616     C                   ENDIF
251200160616     C     $TASTO        WHENEQ    F14
251300160616     C                   z-add     14            posfun
251400160616     C     $FC(14)       IFEQ      '0'
251500160616     C                   SETON                                        9997
251600160616     C                   ENDIF
251700160616     C     $TASTO        WHENEQ    F15
251800160616     C                   z-add     15            posfun
251900160616     C     $FC(15)       IFEQ      '0'
252000160616     C                   SETON                                        9997
252100160616     C                   ENDIF
252200160616     C     $TASTO        WHENEQ    F16
252300160616     C                   z-add     16            posfun
252400160616     C     $FC(16)       IFEQ      '0'
252500160616     C                   SETON                                        9997
252600160616     C                   ENDIF
252700160616     C     $TASTO        WHENEQ    F17
252800160616     C                   z-add     17            posfun
252900160616     C     $FC(17)       IFEQ      '0'
253000160616     C                   SETON                                        9997
253100160616     C                   ENDIF
253200160616     C     $TASTO        WHENEQ    F18
253300160616     C                   z-add     18            posfun
253400160616     C     $FC(18)       IFEQ      '0'
253500160616     C                   SETON                                        9997
253600160616     C                   ENDIF
253700160616     C     $TASTO        WHENEQ    F19
253800160616     C                   z-add     19            posfun
253900160616     C     $FC(19)       IFEQ      '0'
254000160616     C                   SETON                                        9997
254100160616     C                   ENDIF
254200160616     C     $TASTO        WHENEQ    F20
254300160616     C                   z-add     20            posfun
254400160616     C     $FC(20)       IFEQ      '0'
254500160616     C                   SETON                                        9997
254600160616     C                   ENDIF
254700160616     C     $TASTO        WHENEQ    F21
254800160616     C                   z-add     21            posfun
254900160616     C     $FC(21)       IFEQ      '0'
255000160616     C                   SETON                                        9997
255100160616     C                   ENDIF
255200160616     C     $TASTO        WHENEQ    F22
255300160616     C                   z-add     22            posfun
255400160616     C     $FC(22)       IFEQ      '0'
255500160616     C                   SETON                                        9997
255600160616     C                   ENDIF
255700160616     C     $TASTO        WHENEQ    F23
255800160616     C                   z-add     23            posfun
255900160616     C     $FC(23)       IFEQ      '0'
256000160616     C                   SETON                                        9997
256100160616     C                   ENDIF
256200160616     C     $TASTO        WHENEQ    F24
256300160616     C                   z-add     24            posfun
256400160616     C     $FC(24)       IFEQ      '0'
256500160616     C                   SETON                                        9997
256600160616     C                   ENDIF
256700160616     C                   ENDSL
256800160616     C*
256900160616     C                   ENDSR
257000160616     C/EJECT
257100160616     C************************************************************
257200160616     C* AGGIORNAMENTO VALORI ASSUNTI
257300160616     C************************************************************
257400160616     C     AGGMPE        BEGSR
257500160616     C*
257600160616     C* preparo i parametri di chiamata al driver
257700160616     C                   CLEAR                   XMPEDS
257800160616     C                   EVAL      TpChiamata = '03'
257900160616     C                   EVAL      NChiavi = 5
258000160616     C                   EVAL      VLogica = 2
258100160616     C*
258200160616     C* preparo le chiavi per la chiamata al driver
258300160616     C                   CLEAR                   XMPEDS1
258400160616     C                   MOVEL     XSCSOC        DSMPESOC
258500160616     C                   MOVEL     DSPGM         DSMPEPGM
258600160616     C                   MOVEL     KNMUS         DSMPEPRF
258700160616     C*
258800160616     C* Si memorizzano i dati di default a seconda delle modalità con
258900160616     C* cui il pgm è usato
259000160616     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
259100160616     C* MPETIP = 'PV'  pgm richiamato da menù
259200160616     C* MPETIP = 'PB'  x lancio Batch
259300160616     C*
259400160616     C                   SELECT
259500160616    >C     OPZK15        WHENEQ    '01'
259600160616     C                   MOVEL     'PX'          DSMPETIP
259700160616     C                   OTHER
259800160616     C                   MOVEL     'PV'          DSMPETIP
259900160616     C                   ENDSL
260000160616     C* dati
260100160616     C                   MOVEL     DSMPE         DSMPEPAR
260200160616     C*
260300160616     C                   CALLB     'XMPE'
260400160616     C                   PARM                    XMPEDS
260500160616     C                   PARM                    XMPEDS1
260600160616     C*
260700160616     C                   ENDSR
260800160616     C/EJECT
260900160616     C************************************************************
261000160616     C* OPERAZIONI INIZIALI
261100160616     C************************************************************
261200160616     C     *INZSR        BEGSR
261300160616     C*
261400160616     C* Reperimento parametri
261500160616     C*
261600160616     C     *ENTRY        PLIST
261700160616     C                   PARM                    KPJBA
261800160616     C                   PARM                    DSAUT
261900160616     C*
262000160616     C* testo se sono stati passati i dati di autorizzazione
262100160616     C*
262200160616     C     PARMS         IFLT      2
262300160616     C                   MOVE      *BLANK        AUTDS
262400160616     C                   ELSE
262500160616     C                   MOVEL     DSAUT         AUTDS
262600160616     C                   ENDIF
262700160616     C*
262800160616     C* Reperimento dati società
262900160616     C*
263000160616     C                   MOVEL     'SOC001'      TIPXSC
263100160616     C                   MOVEL     *BLANK        SOCXSC
263200160616     C                   EXSR      REPSOC
263300160616     C*
263400160616     C* Controlli dati società
263500160616     C*
263600160616     C     RTNXSC        IFNE      '1'
263700160616     C                   MOVEL     XSOCDS        SOC001
263800160616     C                   ELSE
263900160616     C                   MOVE      *ON           $FINE
264000160616    >C                   MOVE      '1'           ERRK15
264100160616     C                   EXSR      ENDPGM
264200160616     C                   ENDIF
264300160616     C*
264400160616     C* reperisco le dimensioni delle variabili che conterranno i tasti
264500160616     C                   EVAL      $D1L01 = %Size(Z1Ke1)
264600160616     C                   EVAL      $D1L02 = %Size(Z1Ke2)
264700160616     C                   EVAL      $S1L01 = %Size(Z2Ke1)
264800160616     C                   EVAL      $S1L02 = %Size(Z2Ke2)
264900160616     C                   EVAL      $C1Op1 = %Size(C1Opz1)
265000160616     C                   EVAL      $C1Op2 = %Size(C1Opz2)
265100160616     C                   EVAL      $D2L01 = %Size(Z3Ke1)
265200160616     C                   EVAL      $D2L02 = 0
265300160616    >C* aggiungere altre variabili nel caso di videate indipendenti
265400160616    >C* ulteriori a quelle std
265500160616     C*
265600160616     C* Reperimento opzioni
265700160616     C*
265800160616     C                   EXSR      REPOPZ
265900160616     C*
266000160616     C* Reperimento tasti di funzione
266100160616     C*
266200160616     C                   EXSR      REPKEY
266300160616     C* Chiamata a routine per il reperimento delle opzioni
266400160616     C* personalizzate
266500160616    >C* se si vogliono abilitare i tasti personalizzati in ogni videata
266600160616    >C* ripeterla passando le schiere dei tasti funzione
266700160616     C                   CAllb     'X46USREXT'
266800160616     C                   PARM                    $OP1
266900160616     C                   PARM                    $TX1
267000160616     C                   PARM                    $FL1
267100160616     C                   PARM                    $PGO1
267200160616     C                   PARM      $dimOp        NumOpz
267300160616     C                   PARM                    $FX2
267400160616     C                   PARM                    $FC2
267500160616     C                   PARM                    $PGF2
267600160616     C                   PARM                    DsPgm
267700160616     C                   PARM                    xsccut
267800160616     C                   PARM                    xscgrp
267900160616     C*
268000160616     C                   Z-add     1             OPZLEN
268100160616     C     *blank        Lookup    $TX1(opzlen)                           21
268200160616     C                   if        *in21 = *on
268300160616     C                   sub       1             OPZLEN
268400160616     C                   else
268500160616     C                   Z-add     $DimOp        OPZLEN
268600160616     C                   endif
268700160616     C*
268800160616     C                   ENDSR
268900160616     C/EJECT
269000160616     C************************************************************
269100160616     C* INIZIALIZZAZIONE VARIABILI
269200160616     C************************************************************
269300160616     C     INZVAR        BEGSR
269400160616     C*
269500160616     C* Dati da PGM chiamante
269600160616     C*
269700160616     C                   MOVEL     KPJBU         SDGSK15DS
269800160616     C*
269900160616     C* Se società diversa da quella impostata nella *inzsr, rieseguo
270000160616     C* reperimento dati società
270100160616    >C                   IF        SocietaK15 <> xscsoc  and
270200160616    >C                             OPZK15 <>*blank
270300160616     C                   MOVEL     'SOC001'      TIPXSC
270400160616    >C                   MOVEL     SocietaK15    SOCXSC
270500160616     C                   EXSR      REPSOC
270600160616     C     RTNXSC        IFNE      '1'
270700160616     C                   MOVEL     XSOCDS        SOC001
270800160616     C                   ELSE
270900160616     C                   MOVE      *ON           $FINE
271000160616    >C                   MOVE      '1'           ERRK15
271100160616     C                   EXSR      ENDPGM
271200160616     C                   ENDIF
271300160616     C                   ENDIF
271400160616     C*
271500160616     C* Reperimento autorizzazioni
271600160616     C*
271700160616    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
271800160616    >C*   asteriscare la chiamata alla subroutine REPAUT
271900160616     C* le autorizzazioni potrebbero essere già state passate,
272000160616     C* nel qual caso non vanno reperite
272100160616    >C     AUTDS         IFEQ      *BLANK
272200160616    >C****               EXSR      REPAUT
272300160616    >C                   ELSE
272400160616    >C*****              MOVEL     XCADDS
272500160616    >C                   ENDIF
272600160616    >C*                  ENDIF
272700160616     C* Reperimento valori default utente
272800160616     C*
272900160616     C                   EXSR      REPDFT
273000160616     C*
273100160616     C* Definizione delle opzioni utilizzabili a pgm
273200160616     C*
273300160616     C                   EXSR      DEFOPZ
273400160616     C*
273500160616     C* Variabili per gestione videate
273600160616     C*
273700160616     C                   MOVE      *OFF          $FINE                          fine pgm
273800160616     C                   MOVE      *OFF          $INZD1                         inizializzione D1
273900160616     C                   MOVE      *OFF          $INZS1                         inizializzione S1
274000160616     C                   MOVE      *OFF          $INZD2                         inizializzione D2
274100160616     C                   MOVE      *OFF          $EndFile                       memo fine file
274200160616     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
274300160616     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
274400160616     C                   MOVE      *HIVAL        $LASTV
274500160616     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
274600160616     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
274700160616     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
274800160616     C* azzero la posizione del cursore
274900160616     C                   Z-ADD     0             H1RIGA
275000160616     C                   Z-ADD     0             H1COLO
275100160616     C*
275200160616     C                   MOVE      *ON           $INZD1
275300160616     C                   MOVE      *ON           $INZS1
277000160616     C*
277100160616     C* Variabili appoggio
277200160616     C*
277300160616     C                   Z-ADD     SFLPAG        WPAG
277400160616     C                   MOVEL     *OFF          WOPR
277500160616     C                   CLEAR                   W1POS
277600160616     C*
277700160616     C* definisco qual'è la prima videata
277800160616     C* (se la prima videata risulta la lista, devo comunque caricare
277900160616     C*  la videata di parzializzazione e gestire la definizione
278000160616     C*  dell'ordinamento, perchè, dai default oppure se sono stati
278100160616     C*  passati dei dati, può variare il caricamento)
278200160616     C                   SELECT
278300160616     C     $LOP          WHENEQ    1
278400160616     C                   MOVE      'D1'          $GEST
278500160616     C     $LOP          WHENEQ    2
278600160616     C                   MOVE      'S1'          $GEST
278700160616     C                   EXSR      INZD1
278800160616     C                   MOVE      *OFF          $INZD1
278900160616     C                   EXSR      DEFORD
279000160616     C                   ENDSL
279100160616     C*
279200160616     C* Valorizzazione campi univoci testate
279300160616     C*
279400160616     C                   CLEAR                   FMTT1
279500160616     C                   MOVEL     KNSIF         NOMSIF
279600160616     C                   MOVEL     XSCDSI        NOMDIT
279601160617     C                   MOVEL     KCDAZ         NOMAZN
279701160617     C****               MOVEL     DSPGM         NOMPGM
279800160616     C* recupero il sottotitolo (se c'è)
279900160616     C                   MOVEL     *BLANK        T1TIT
280000160616     C     SOTTIT        IFNE      *BLANK
280100160616     C                   MOVEL     SOTTIT        IDMSG
280200160616     C                   CALLB     'XRTVM'
280300160616     C                   PARM                    IDMSG            27
280400160616     C                   PARM                    TXTMSG          100
280500160616     C                   PARM                    ERRMSG            1
280600160616     C                   PARM      *ON           CTRMSG            1            centratura
280700160616     C                   PARM      30            LENMSG            3 0          lun output.
280800160616     C     ERRMSG        IFNE      *ON
280900160616     C                   MOVEL     TXTMSG        T1TIT
281000160616     C                   ELSE
281100160616     C                   MOVEL     *ALL'?'       T1TIT
281200160616     C                   ENDIF
281300160616     C                   ENDIF
281400160616     C*
281500160616     C                   ENDSR
281600160616     C/EJECT
281700160616     C************************************************************
281800160616     C* REPERIMENTO DATI SOCIETA'
281900160616     C************************************************************
282000160616     C     REPSOC        BEGSR
282100160616     C*
282101160617     C                   CALL      'SDGSOCDR'
282300160616     C                   PARM                    TIPXSC            6
282400160616     C                   PARM                    SOCXSC            3
282500160616     C                   PARM                    CDSXSC            9 0
282600160616     C                   PARM                    MODXSC            3
282700160616     C                   PARM      *BLANKS       RTNXSC            1
282800160616     C                   PARM                    XSOCDS
282900160616     C                   PARM                    KPJBA
283000160616     C*
283100160616     C                   ENDSR
283200160616     C/EJECT
283300160616     C************************************************************
283400160616     C* REPERIMENTO AUTORIZZAZIONI UTENTE
283500160616     C************************************************************
283600160616     C     REPAUT        BEGSR
283700160616     C*
283800160616     C                   CLEAR                   AUTDS
283900160616     C*
284000160616     C*
284100160616     C                   ENDSR
284200160616     C/EJECT
284300160616     C************************************************************
284400160616     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
284500160616     C************************************************************
284600160616     C     REPDFT        BEGSR
284700160616     C*
284800160616     C* preparo i parametri di chiamata al driver
284900160616     C                   CLEAR                   XMPEDS
285000160616     C                   EVAL      TpChiamata = '01'
285100160616     C                   EVAL      NChiavi = 5
285200160616     C                   EVAL      VLogica = 2
285300160616     C*
285400160616     C* preparo le chiavi per la chiamata al driver
285500160616     C                   CLEAR                   XMPEDS1
285600160616     C                   MOVEL     XSCSOC        DSMPESOC
285700160616     C                   MOVEL     DSPGM         DSMPEPGM
285800160616     C                   MOVEL     KNMUS         DSMPEPRF
285900160616     C* Si reperiscono i dati di default a seconda delle modalità con
286000160616     C* cui il pgm è usato
286100160616     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
286200160616     C* MPETIP = 'PV'  pgm richiamato da menù
286300160616     C* MPETIP = 'PB'  x lancio Batch
286400160616     C                   SELECT
286500160616    >C     OPZK15        WHENEQ    '01'
286600160616     C                   MOVEL     'PX'          DSMPETIP
286700160616     C                   OTHER
286800160616     C                   MOVEL     'PV'          DSMPETIP
286900160616     C                   ENDSL
287000160616     C*
287100160616     C                   CALLB     'XMPE'
287200160616     C                   PARM                    XMPEDS
287300160616     C                   PARM                    XMPEDS1
287400160616     C*
287500160616     C     Risultato     IFEQ      '0'
287600160616     C                   MOVEL     DSMPEPAR      DSMPE
287700160616     C* nel caso sia la prima volta che l'utente apre questa anagrafica
287800160616     C* i suoi valori di default vengono decisi a pgm
287900160616     C                   ELSE
288000160616     C                   Z-ADD     1             $LOP
288100160616     C                   Z-ADD     1             $ORD
288200160616    >C*  elenco altri dati default specifici dell'anagrafica
288300160616     C                   ENDIF
288400160616     C*
288500160616     C                   ENDSR
288600160616     C/EJECT
288700160616     C************************************************************
288800160616     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
288900160616     C************************************************************
289000160616     C     DEFOPZ        BEGSR
289100160616     C*
289200160616     C* Questa routine deve valorizzare la schiera $FL1
289300160616     C* con S o N a seconda della utilizzabilità o meno
289400160616     C* dell'opzione correlata
289500160616     C*
289600160616     C* Inizialmente si considerano tutte le opzioni come valide
289700160616     C     1             DO        $DIMOP        X
289800160616     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
289900160616     C                   If        $TX1(X)  <> *Blank
290000160616     C                   MOVE      'S'           $FL1(X)
290100160616     C                   Else
290200160616     C                   Leave
290300160616     C                   EndIf
290400160616     C                   ENDDO
290500160616     C*
290600160616     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
290700160616     C* con 1 o 0 a seconda della utilizzabilità o meno
290800160616     C* del tasto funzione correlato
290900160616     C*
291000160616     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
291100160616     C* di compilazione
291200160616     C                   RESET                   $FC1
291300160616     C                   RESET                   $FR1
291400160616     C                   RESET                   $FC2
291500160616     C                   RESET                   $FR2
291600160616     C                   RESET                   $FC3
291700160616     C                   RESET                   $FR3
291800160616     C*
291900160616     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
292000160616     C* invalido alcune opzioni
292100160616     C                   SELECT
292200160616     C* Se pgm richiamato in ricerca disattivo :
292300160616    >C     OPZK15        WHENEQ    '01'
292400160616     C* F3=Fine
292500160616     C                   Z-ADD     3             X
292600160616     C                   MOVE      '0'           $FC1(X)
292700160616     C                   MOVE      '0'           $FC2(X)
292800160616     C                   MOVE      '0'           $FC3(X)
292900160616     C*
293000160616    >C*  aggiungere nello stesso modo le altre opzioni da
293100160616    >C*  disattivare (quelle specifiche per ogni pgm)
293200160616     C*
293300160616     C* Se pgm richiamato in interrogazione disattivo :
293400160616    >C     OPZK15        WHENEQ    '05'
293500160616     C* F10=Immissione
293600160616     C                   Z-ADD     10            X
293700160616     C                   MOVE      '0'           $FC1(X)
293800160616     C                   MOVE      '0'           $FC2(X)
293900160616     C* 1=Selezione
294000160616     C                   Z-ADD     1             X
294100160616     C     01            LOOKUP    $OP1(X)                                21
294200160616     C     *IN21         IFEQ      *ON
294300160616     C                   MOVE      'N'           $FL1(X)
294400160616     C                   ENDIF
294500160616     C* 2=Modifica
294600160616     C                   Z-ADD     1             X
294700160616     C     02            LOOKUP    $OP1(X)                                21
294800160616     C     *IN21         IFEQ      *ON
294900160616     C                   MOVE      'N'           $FL1(X)
295000160616     C                   ENDIF
295100160616     C* 3=Copia
295200160616     C                   Z-ADD     1             X
295300160616     C     03            LOOKUP    $OP1(X)                                21
295400160616     C     *IN21         IFEQ      *ON
295500160616     C                   MOVE      'N'           $FL1(X)
295600160616     C                   ENDIF
295700160616     C* 4=Annullamento
295800160616     C                   Z-ADD     1             X
295900160616     C     04            LOOKUP    $OP1(X)                                21
296000160616     C     *IN21         IFEQ      *ON
296100160616     C                   MOVE      'N'           $FL1(X)
296200160616     C                   ENDIF
296300160616     C*
296400160616    >C     OPZK15        WHENEQ    *BLANKS
296500160616     C* Se pgm non richiamato, disattivo subito :
296600160616     C* F8=Selezione seguente
296700160616     C                   Z-ADD     8             X
296800160616     C                   MOVE      '0'           $FC1(X)
296900160616     C                   MOVE      '0'           $FC2(X)
297000160616     C* 1=Selezione
297100160616     C                   Z-ADD     1             X
297200160616     C     01            LOOKUP    $OP1(X)                                21
297300160616     C     *IN21         IFEQ      *ON
297400160616     C                   MOVE      'N'           $FL1(X)
297500160616     C                   ENDIF
297600160616     C*
297700160616     C* In tutti gli altri casi, disattivo subito :
297800160616     C                   OTHER
297900160616     C* 1=Selezione
298000160616     C                   Z-ADD     1             X
298100160616     C     01            LOOKUP    $OP1(X)                                21
298200160616     C     *IN21         IFEQ      *ON
298300160616     C                   MOVE      'N'           $FL1(X)
298400160616     C                   ENDIF
298500160616     C*
298600160616     C                   ENDSL
300100160616     C*
300200160616     C                   ENDSR
300300160616     C/EJECT
300400160616     C************************************************************
300500160616     C* REPERIMENTO OPZIONI
300600160616     C************************************************************
300700160616     C     REPOPZ        BEGSR
300800160616     C*
300900160616     C     1             DO        $DIMOP        X
301000160616     C*
301100160616     C     $MS1(X)       IFNE      *BLANK
301200160616     C*
301300160616     C                   MOVEL     $MS1(X)       IDMSG
301400160616     C                   CALLB     'XRTVM'
301500160616     C                   PARM                    IDMSG            27
301600160616     C                   PARM                    TXTMSG          100
301700160616     C                   PARM                    ERRMSG            1
301800160616     C*
301900160616     C     ERRMSG        IFNE      *ON
302000160616     C                   MOVEL     TXTMSG        $TX1(X)
302100160616     C                   ELSE
302200160616     C                   MOVEL     *ALL'?'       $TX1(X)
302300160616     C                   ENDIF
302400160616     C*
302500160616     C                   ENDIF
302600160616     C*
302700160616     C                   ENDDO
302800160616     C*
302900160616     C                   ENDSR
303000160616     C/EJECT
303100160616     C************************************************************
303200160616     C* REPERIMENTO TASTI DI FUNZIONE
303300160616     C************************************************************
303400160616     C     REPKEY        BEGSR
303500160616     C                   MOVEA     $FM1          $FM
303600160616     C                   MOVEA     $FC1          $FC
303700160616     C                   EXSR      REPKKK
303800160616     C                   MOVEA     $FX           $FX1
303900160616     C*
304000160616     C                   MOVEA     $FM2          $FM
304100160616     C                   MOVEA     $FC2          $FC
304200160616     C                   EXSR      REPKKK
304300160616     C                   MOVEA     $FX           $FX2
304400160616     C*
304500160616     C                   MOVEA     $FM3          $FM
304600160616     C                   MOVEA     $FC3          $FC
304700160616     C                   EXSR      REPKKK
304800160616     C                   MOVEA     $FX           $FX3
304900160616     C                   ENDSR
305000160616     C/EJECT
305100160616     C************************************************************
305200160616     C* REPERIMENTO TASTI DI FUNZIONE
305300160616     C************************************************************
305400160616     C     REPKKK        BEGSR
305500160616     C*
305600160616     C                   CLEAR                   $FX
305700160616     C     1             DO        24            X
305800160616     C*
305900160616     C     $FM(X)        IFNE      *BLANK
306000160616     C     $Fc(X)        andne     '0'
306100160616     C*
306200160616     C                   MOVEL     $FM(X)        IDMSG
306300160616     C                   CALLB     'XRTVM'
306400160616     C                   PARM                    IDMSG            27
306500160616     C                   PARM                    TXTMSG          100
306600160616     C                   PARM                    ERRMSG            1
306700160616     C*
306800160616     C     ERRMSG        IFNE      *ON
306900160616     C                   MOVEL     TXTMSG        $FX(X)
307000160616     C                   ELSE
307100160616     C                   MOVEL     *ALL' '       $FX(X)
307200160616     C                   ENDIF
307300160616     C*
307400160616     C                   ENDIF
307500160616     C*
307600160616     C                   ENDDO
307700160616     C*
307800160616     C                   ENDSR
307900160616     C/EJECT
308000160616     C************************************************************
308100160616     C* DEFINIZIONE KLIST
308200160616     C************************************************************
308300160616     C     DefKlist      BEGSR
308400160616     C*
308500160616     C* klist
308600160616     C*
308700160616    >C* anape01L
308800160616    >C     K03APE01      KLIST
308900160616    >C                   KFLD                    APESOCIETA
309000160616    >C                   KFLD                    APEKcc
309100160616    >C                   KFLD                    APEKsc
309200160616     C*
309300160617    >C* SDGN7705L
309400160616    >C     K03N7705      KLIST
309500160616    >C                   KFLD                    N77SOCIETA
309600160616    >C                   KFLD                    N77ACF
309700160616    >C                   KFLD                    N77Qua
309800160616     C*
309900160617    >C* SDGN7705L
310000160616    >C     K04N7705      KLIST
310100160616    >C                   KFLD                    N77SOCIETA
310200160616    >C                   KFLD                    N77ACF
310300160616    >C                   KFLD                    N77Qua
310400160616    >C                   KFLD                    N77Cog
310500160616      *
310600160616     C* PLIST per NDVASOG
310700160616     C     PDVASOG       PLIST
310800160616     C                   PARM                    DVARIC
310900160616     C                   PARM                    DVASOCIETA
311000160616     C                   PARM                    DVAUNITA
311100160616     C                   PARM                    DVASTRUTT
311200160616     C                   PARM                    DVADTRIF
311300160616     C                   PARM                    DVALENOUT
311400160616     C* Se si accede con il codice cli/for impostare i campi seguenti
311500160616     C* se si accede con il soggetto ometterli e impostare gli altri
311600160616     C                   PARM                    DVASNATURA
311700160616     C                   PARM                    DVACODICE
311800160616     C                   PARM                    DVALINEAV
311900160616     C                   PARM                    DVAFILIALE
312000160616     C                   PARM                    *omit
312100160616     C*                  PARM                    *omit
312200160616     C*                  PARM                    *omit
312300160616     C*                  PARM                    DVSSOGG
312400160616     C                   PARM                    DVATPIND
312500160616     C                   PARM                    DVACDIND
312600160616     C                   PARM                    DVAERRORE
312700160616     C                   PARM                    DVAOUTPUT
312800160616     C*
312900160616     C                   ENDSR
313000160616     C/EJECT
313100160616**
313200160616PROMSG    *LIBL     OPZ0001
313300160616PROMSG    *LIBL     OPZ0002
313400160616PROMSG    *LIBL     OPZ0003
313500160616PROMSG    *LIBL     OPZ0004
313600160616PROMSG    *LIBL     OPZ0005
313700160616
313800160616**
31390016061601S      <Selezione
31400016061602S      <Modifica
31410016061603S      <Copia
31420016061604S      <Annullamento
31430016061605S      <Visualizzazione
31440016061639S      <Estraz. debitori
314500160616** TASTI DI FUNZIONE  D1
314600160616PROMSG    *LIBL     KEY0001  01
314700160616PROMSG    *LIBL     KEY0002  02
314800160616PROMSG    *LIBL     KEY0003  03
314900160616PROMSG    *LIBL     KEY0004  04
315000160616PROMSG    *LIBL     KEY0005  05
315100160616PROMSG    *LIBL     KEY0006  06
315200160616PROMSG    *LIBL     KEY0007  07
315300160616PROMSG    *LIBL     KEY0008  08
315400160616PROMSG    *LIBL     KEY0009  09
315500160616PROMSG    *LIBL     KEY0010  10
315600160616PROMSG    *LIBL     KEY0011  11
315700160616PROMSG    *LIBL     KEY0012  12
315800160616PROMSG    *LIBL     KEY0013  13
315900160616PROMSG    *LIBL     KEY0014  14
316000160616PROMSG    *LIBL     KEY0015  15
316100160616PROMSG    *LIBL     KEY0016  16
316101160617SDGMSG    *LIBL     SDK0001  17
316300160616PROMSG    *LIBL     KEY0018  18
316400160616PROMSG    *LIBL     KEY0019  19
316500160616PROMSG    *LIBL     KEY0020  20
316600160616PROMSG    *LIBL     KEY0021  21
316700160616PROMSG    *LIBL     KEY0022  22
316800160616PROMSG    *LIBL     KEY0023  23
316900160616PROMSG    *LIBL     KEY0024  24
317000160616**  ABILITAZIONE D1
3171001606160N  01
3172001606161N  02
3173001606161N  03
3174001606161N  04
3175001606160N  05
3176001606160N  06
3177001606160N  07
3178001606160N  08
3179001606160N  09
3180001606160N  10
3181001606160N  11
3182001606161N  12
3183001606160N  13
3184001606160N  14
3185001606160N  15
3186001606160N  16
3187001606160N  17
3188001606161N  18
3189001606160N  19
3190001606160N  20
3191001606160N  21
3192001606160N  22
3193001606160N  23   Impostare a '1' se si utilizzano opzioni
3194001606161N  24   Lasciare sempre impostata a '1' il valore della riga 24
319500160616** TASTI DI FUNZIONE  S1
319600160616PROMSG    *LIBL     KEY0001  01
319700160616PROMSG    *LIBL     KEY0002  02
319800160616PROMSG    *LIBL     KEY0003  03
319900160616PROMSG    *LIBL     KEY0004  04
320000160616PROMSG    *LIBL     KEY0005  05
320100160616PROMSG    *LIBL     KEY0006  06
320200160616PROMSG    *LIBL     KEY0007  07
320300160616PROMSG    *LIBL     KEY0008  08
320400160616PROMSG    *LIBL     KEY0009  09
320500160616PROMSG    *LIBL     KEY0010  10
320600160616PROMSG    *LIBL     KEY0011  11
320700160616PROMSG    *LIBL     KEY0012  12
320800160616PROMSG    *LIBL     KEY0013  13
320900160616PROMSG    *LIBL     KEY0014  14
321000160616PROMSG    *LIBL     KEY0015  15
321100160616PROMSG    *LIBL     KEY0016  16
321101160617SDGMSG    *LIBL     SDK0001  17
321300160616PROMSG    *LIBL     KEY0018  18
321400160616PROMSG    *LIBL     KEY0019  19
321500160616PROMSG    *LIBL     KEY0020  20
321600160616PROMSG    *LIBL     KEY0021  21
321700160616PROMSG    *LIBL     KEY0022  22
321800160616PROMSG    *LIBL     KEY0023  23
321900160616PROMSG    *LIBL     KEY0024  24
322000160616**  ABILITAZIONE S1
3221001606160N  01
3222001606160N  02
3223001606161N  03
3224001606160N  04
3225001606160N  05
3226001606160N  06
3227001606160N  07
3228001606160N  08
3229001606160N  09
3230001606161N  10
3231001606160N  11
3232001606161N  12
3233001606160N  13
3234001606160N  14
3235001606160N  15
3236001606160N  16
3237001606161N  17
3238001606161N  18
3239001606160N  19
3240001606160N  20
3241001606160N  21
3242001606160N  22
3243001606161N  23   Impostare a '1' se si utilizzano opzioni
3244001606161N  24   Lasciare sempre impostata a '1' il valore della riga 24
324500160616** TASTI DI FUNZIONE  D3
324600160616PROMSG    *LIBL     KEY0001  01
324700160616PROMSG    *LIBL     KEY0002  02
324800160616PROMSG    *LIBL     KEY0003  03
324900160616PROMSG    *LIBL     KEY0004  04
325000160616PROMSG    *LIBL     KEY0005  05
325100160616PROMSG    *LIBL     KEY0006  06
325200160616PROMSG    *LIBL     KEY0007  07
325300160616PROMSG    *LIBL     KEY0008  08
325400160616PROMSG    *LIBL     KEY0009  09
325500160616PROMSG    *LIBL     KEY0010  10
325600160616PROMSG    *LIBL     KEY0011  11
325700160616PROMSG    *LIBL     KEY0012  12
325800160616PROMSG    *LIBL     KEY0013  13
325900160616PROMSG    *LIBL     KEY0014  14
326000160616PROMSG    *LIBL     KEY0015  15
326100160616PROMSG    *LIBL     KEY0016  16
326101160617SDGMSG    *LIBL     SDK0001  17
326300160616PROMSG    *LIBL     KEY0018  18
326400160616PROMSG    *LIBL     KEY0019  19
326500160616PROMSG    *LIBL     KEY0020  20
326600160616PROMSG    *LIBL     KEY0021  21
326700160616PROMSG    *LIBL     KEY0022  22
326800160616PROMSG    *LIBL     KEY0023  23
326900160616PROMSG    *LIBL     KEY0024  24
327000160616**  ABILITAZIONE D3
3271001606160N  01
3272001606160N  02
3273001606161N  03
3274001606160N  04
3275001606160N  05
3276001606160N  06
3277001606160N  07
3278001606160N  08
3279001606160N  09
3280001606160N  10
3281001606160N  11
3282001606161N  12
3283001606160N  13
3284001606160N  14
3285001606160N  15
3286001606160N  16
3287001606160N  17
3288001606160N  18
3289001606160N  19
3290001606160N  20
3291001606160N  21
3292001606160N  22
3293001606160N  23   Impostare a '1' se si utilizzano opzioni
3294001606161N  24   Lasciare sempre impostata a '1' il valore della riga 24
