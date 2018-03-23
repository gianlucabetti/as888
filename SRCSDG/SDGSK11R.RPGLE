000100150511     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200150511     H*PARMS CVTOPT(*NONE)
000300150511     H DECEDIT('0,') DATEDIT(*DMY.)
000301150511      *
000302160728      *           ------------------------------------------------
000303160728      *                Storico percipienti a regime agevolato
000305150511      *                       Guida
000306160728      *           ------------------------------------------------
000308150511      *
000309161020R235  * Pgm creato con il task R235 CU Percipiente in regime agevolato
000310161020R235  *
004200150511     F*----------------------------------------------------------------
004400160728     F* Storico percipienti a regime agevolato
004401160728     F* key: Soc/Kcc/Ksc/DtIni(desc)    UNIQUE
004500160728     FSDGSPA01L IF   E           K DISK
004700150511     F*------------
004800160728    >FSDGSK11D  CF   E             WORKSTN
004900160728    >F                                     RENAME(SK11D1:FMTD1)
005000160728    >F                                     RENAME(SK11D2:FMTD2)
005100160728    >F                                     RENAME(SK11D3:FMTD3)
005200160728    >F                                     RENAME(SK11S1:FMTS1)
005300160728    >F                                     RENAME(SK11C1:FMTC1)
005400160728    >F                                     RENAME(SK11T1:FMTT1)
005500160728    >F                                     RENAME(SK11Z1:FMTZ1)
005600160728    >F                                     RENAME(SK11Z2:FMTZ2)
005700160728    >F                                     RENAME(SK11Z3:FMTZ3)
005800150511     F                                     SFILE(FMTS1:S1NRR)
005900150511     F                                     INFDS(DSFMT)
006000150511     D*----------------------------------------------------*
006100150511     D* Schiere per la gestione delle opzioni
006200150511     D*
006300150511     D* dimensione della schiera $MS1, da non cambiare
006400150511     D $DIMOP          C                   CONST(30)
006500150511     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
006600150511     D $TX1            S             50    DIM($dimop) ALT($MS1)
006700150511     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
006800150511     D $FL1            S              1    DIM($dimop) ALT($OP1)
006900150511     D* schiera contenente i nomi dei programma richiamati attraverso
007000150511     D* le opzioni personalizzate
007100150511     D $PGO1           S             10    DIM($dimop)
007200150511     D $TX             S             50    DIM(50)
007300150511     D $FL             S              1    DIM(50)
007400150511     D* Schiere generali per la gestione dei tasti di funzione
007500150511     D $FM             S             27    DIM(24)
007600150511     D $FX             S             50    DIM(24)
007700150511     D $FC             S              1    DIM(24)
007800150511     D $FR             S              1    DIM(24)
007900150511     D* Schiere per la gestione dei tasti di funzione
008000150511     D* Video D1
008100150511     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
008200150511     D $FX1            S             50    DIM(24) ALT($FM1)
008300150511     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
008400150511     D $FR1            S              1    DIM(24) ALT($FC1)
008500150511     D* Schiera contente i nomi dei programmi da richiamare con i tasti
008600150511     D* funzione personalizzata S1
008700150511     D $PGF2           S             10    DIM(25)
008800150511     D* Schiere per la gestione dei tasti di funzione
008900150511     D* Video S1
009000150511     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
009100150511     D $FX2            S             50    DIM(24) ALT($FM2)
009200150511     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
009300150511     D $FR2            S              1    DIM(24) ALT($FC2)
009400150511     D* Schiere per la gestione dei tasti di funzione
009500150511     D* Video D2
009600150511     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
009700150511     D $FX3            S             50    DIM(24) ALT($FM3)
009800150511     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
009900150511     D $FR3            S              1    DIM(24) ALT($FC3)
010000150511     D*
010100150511     D $D1L01          S              3  0
010200150511     D $D1L02          S              3  0
010300150511     D $S1L01          S              3  0
010400150511     D $S1L02          S              3  0
010500150511     D $C1Op1          S              3  0
010600150511     D $C1Op2          S              3  0
010700150511     D $D2L01          S              3  0
010800150511     D $D2L02          S              3  0
010900150511     D*
011000150511     D*-------------
011100150511     D* Indice della schiera tasti funzione
011200150511     D PosFun          S              2  0
011400150511     D*----------------------------------------------------*
011500150511     D*-------------
011600150511     D* Valori assunti
011700150511     D XMPEDS        E DS                  INZ
011800150511     D XMPEDS1       E DS                  INZ
011900150511     D DSMPE           DS                  INZ
012000150511     D  $LOP                   1      1  0
012100150511     D  $ORD                   2      3  0
012200150511     D*-------------
012300150511     D* Passaggio Parametri
012400150511     D KPJBA         E DS
012500150511     D*-------------
012600150511     D* Reperimento Autorizzazioni
012700150511     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
012800150511     D*-------------
012900150511     D* Passaggio Autorizzazioni
013000150511     D DSAUT           DS           512
013001150512     D*-------------
013002150512     D* Dati di ambiente ottenuti da SDGSOCDR
013003150512     D SOC001        E DS                  EXTNAME(SDGSOCDS )
013005150512     D*-------------
013006150512     D* DS Interna per dati di output di SDGSOCDR
013007150512     D XSOCDS          DS          1000
013700150511     D*-------------
013800150511     D* Parametri in ricezione
013900160728    >D SDGSK11DS     E DS
014000150511     D*-------------
014100160728     D* Parametri del PGM di manutenzione SDGSK12R
014200160728    >D SDGSK12DS     E DS                  Inz
014300160728    >D  RETK12       E                     INZ('0')
014400160728    >D  OPRK12       E                     INZ('0')
014500160728    >D  ERRK12       E                     INZ('0')
014600160728    >D  CMTK12       E                     INZ('0')
014601160728     D*-------------
014602160728     D* Passaggio Parametri al PGM capoconti
014603160728     D ANA200DS      E DS
014604160728     D  CMT200       E                     INZ('0')
014605160728     D  RET200       E                     INZ('0')
014606160728     D  OPR200       E                     INZ('0')
014607160728     D  ERR200       E                     INZ('0')
014608160728     D  FSC200       E                     INZ('S')
014609160728     D*-------------
014610160728     D* Ricerca sottoconti
014611160728     D ANA201DS      E DS                  INZ
014612160728     D*-------------
014613160728     D* Controllo conti
014614160728     D ND002DS       E DS                  INZ
014615160728     D*-------------
014616160728     D* Definizioni parametri standard chiamate moduli
014617160728     D Esito           S              1
014618160728     D GesMsg          S              1
014619160728     D LenOut          S              9B 0
014620160728     D Data            S             10
014621160728     D StrutturaO      S             10
017000150511     D*-------------
017100150511     D* campo di salvataggio per la DS d'ingresso durante la chiamata
017200150511     D* alla funzione personalizzata
017300160728    >D $SaveDS         S                   like(SDGSK11DS)
017400150511     D*-------------
017500150511     D DSFMT           DS           512
017600150511     D  $TASTO               369    369
017700150511     D  NRG                  370    370
017800150511     D  NCL                  371    371
017900150511     D  SFLNRR               378    379B 0
018000150511     D*-------------
018100150511     D* posizione cursore
018200150511     D CURSOR          DS
018300150511     D  RIRI                   1      2B 0 INZ
018400150511     D  R$$                    2      2
018500150511     D  COCO                   3      4B 0 INZ
018600150511     D  C$$                    4      4
018700150511     D*-------------
018800150511     D* Reperimento nome PGM
018900150511     D STATUS         SDS           333
019000150511     D  DSPGM            *PROC
019100150511     D  PARMS            *PARMS
019200150511     D*-------------
019300150511     D* Costanti
019400150511     D* nr.di righe del sfl (SFLPAG)
019500150512    >D SFLPAG          C                   CONST(11)
019600150511     D*-------------
019700150511     D* Tasti di funzione
019800150511     D F01             C                   CONST(X'31')
019900150511     D F02             C                   CONST(X'32')
020000150511     D F03             C                   CONST(X'33')
020100150511     D F04             C                   CONST(X'34')
020200150511     D F05             C                   CONST(X'35')
020300150511     D F06             C                   CONST(X'36')
020400150511     D F07             C                   CONST(X'37')
020500150511     D F08             C                   CONST(X'38')
020600150511     D F09             C                   CONST(X'39')
020700150511     D F10             C                   CONST(X'3A')
020800150511     D F11             C                   CONST(X'3B')
020900150511     D F12             C                   CONST(X'3C')
021000150511     D F13             C                   CONST(X'B1')
021100150511     D F14             C                   CONST(X'B2')
021200150511     D F15             C                   CONST(X'B3')
021300150511     D F16             C                   CONST(X'B4')
021400150511     D F17             C                   CONST(X'B5')
021500150511     D F18             C                   CONST(X'B6')
021600150511     D F19             C                   CONST(X'B7')
021700150511     D F20             C                   CONST(X'B8')
021800150511     D F21             C                   CONST(X'B9')
021900150511     D F22             C                   CONST(X'BA')
022000150511     D F23             C                   CONST(X'BB')
022100150511     D F24             C                   CONST(X'BC')
022200150511     D ENTER           C                   CONST(X'F1')
022300150511     D ROLDWN          C                   CONST(X'F4')
022400150511     D ROLLUP          C                   CONST(X'F5')
022500150511     D*-------------
022600150511     D* Attributi video
022700150511     D Normale         C                   Const(X'32')
022800150511     D Protetto        C                   Const(X'A0')
022900150511     D*-------------
023000150511     D* Sottotitolo di testata
023100150511     D SOTTIT          C                   CONST(' ')
023101150512     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
023102150512     D                                          SDC0001')
023200150511     D*-------------
023300150511     D* Variabili per gestione videate
023400150511     D $Gest           S              2A
023500150511     D $Fine           S              1A
023600150511     D $InzD1          S              1A
023700150511     D $InzS1          S              1A
023800150511     D $InzD2          S              1A
023900150511     D $LastG          S              2A
024000150511     D $LastV          S              2A
024100150511     D $EndFile        S              1A
024200150511     D $Esci           S              1A
024300150511     D $RcdOK          S              1A
024400150511     D $Unique         S              1A
024500150511     D*-------------
024600150511     D* Variabili appoggio sempre presenti in tutte le anagrafiche
024700150511     D S1Nrr           S                   like(C1Rcd)
024800150511     D WSfl            S                   like(C1Nrr)
024900150511     D WMax            S                   like(C1Rcd)
025000150511     D WPag            S                   like(C1Rcd)
025100150511     D WInzS1          S                   like($InzS1)
025200160728    >D WOpr            S                   like(OprK11)
025300150511     D CurR            S              2S 0
025400150511     D CurC            S              2S 0
025500150511     D Win             S             99A
025600150511     D W1Pos           S                   like(C1Pos)
025700150511     D WINKR           S              1A
025800150511     D WExf            S              1A
025900150511     D*-------------
026000150511     D* Variabili appoggio specifiche della singola anagrafica
026400150511     D*  Indici
026500150511     D X               S              3S 0
026600150511     D Y               S              3S 0
026700150511     D* Indice della schiera per le opzioni personalizzate
026800150511     D PosOpz          S              2  0
026900150511     D* numero max opzioni
027000150511     D NumOpz          S              2  0 Inz($DIMOP)
027100150511     D* numero effettive opzioni
027200150511     D OpzLen          S              2  0
027300150511     D*-------------
027301150512     D* Campi di lavoro
027303160728     D WKcc            S                   Like(SPAKcc )
027304160728     D WKsc            S                   Like(SPAKsc )
027305150512     D $DtVal          S               D
027400150511     C*----------------------------------------------------*
027500150511     C*                MAIN LINE PROGRAM
027600150511     C*----------------------------------------------------*
027700150511     C*
027800150511     C* inizializzazione variabili
027900150511     C                   EXSR      INZVAR
028000150511     C*
028100150511     C     $FINE         DOWEQ     *OFF
028200150511     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
028300150511     C     $GEST         CASEQ     'S1'          GESS1                          Lista
028400150511     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
028500150511     C                   ENDCS
028600150511     C                   ENDDO
028700150511     C* fine programma
028800150511     C                   EXSR      ENDPGM
028900150511     C*
029000150511     C/EJECT
029100150511     C************************************************************
029200150511     C* FINE PROGRAMMA
029300150511     C************************************************************
029400150511     C     ENDPGM        BEGSR
029500150511     C*
029600150511     C* passaggio dati al pgm chiamante
029700160728    >C                   MOVEL     WOPR          OPRK11                                     ne S1
029800160728    >C                   MOVEL     SDGSK11DS     KPJBU
029900150511     C*
030000150511     C* Se pgm Guida non è stato richiamato posso chiudere in LR
030100160728    >C     OPZK11        IFEQ      *BLANK
030200150511     C                   SETON                                            LR
030300150511     C* altrimenti chiudo in RT
030400150511     C                   ELSE
030500150511     C                   SETON                                            RT
030600150511     C                   ENDIF
030700150511     C                   RETURN
030800150511     C*
030900150511     C                   ENDSR
031000150511     C/EJECT
031100150511     C************************************************************
031200150511     C* RICERCHE
031300150511     C************************************************************
031400150511     C     SEARCH        BEGSR
031500150511     C*
031600150511     C* determino Riga/Colonna del cursore
031700150511     C                   MOVE      NRG           R$$
031800150511     C                   MOVE      NCL           C$$
031900150511     C                   Z-ADD     RIRI          CURR
032000150511     C                   Z-ADD     COCO          CURC
032001160728     C*
032003160728     C     D1KCC         IFNE      'C'
032004160728     C     D1KCC         ANDNE     'F'
032005160728     C                   CALLB     'XALLINEA'
032006160728     C                   PARM                    D1KCC
032007160728     C                   PARM      6             LEN               5 0
032008160728     C                   PARM                    XSCINP
032009160728     C                   ENDIF
032900150511     C*
032902160728     C                   if        D1Ksc      <> *Blank
033000150511     C                   CALLB     'XALLINEA'
033100160728     C                   PARM                    D1Ksc
033200150511     C                   PARM      8             LEN               5 0
033300150511     C                   PARM                    XSCINP
033301160728     C*
033302160728     C     D1KCC         IFEQ      'C       '
033303160728     C     D1KCC         OREQ      'F       '
033304160728     C                   CALLB     'XCAPCLIFOR'
033305160728     C                   PARM                    XScSoc
033306160728     C                   PARM      D1KCC         D1Kcc
033307160728     C                   PARM                    D1Ksc
033308160728     C                   ENDIF
033309160728     C*
033310160728     C                   endif
033400150511     C*
033500150511   B1C                   SELECT
033506160728     C*
033507160728     C* capoconto
033508160728     C     H1NMFL        WHENEQ    'D1KCC'
033509160728     C                   EXSR      F4_Kcc
033510160728     C*
033511160728     C* sottoconto
033512160728     C     H1NMFL        WHENEQ    'D1KSC'
033514160728     C                   EXSR      F4_Ksc
036600150511     C*
036700150511     C* ricerca non abilitata in questa posizione
036800150511   X1C                   OTHER
036900150511     C                   SETON                                        9899
037000150511     C*
037100150511   E1C                   ENDSL
037200150511     C*
037300150511     C* imposto pos. del cursore
037400150511     C                   Z-ADD     CURR          H1RIGA
037500150511     C                   Z-ADD     CURC          H1COLO
037600150511     C*
037700150511     C                   MOVE      *HIVAL        $LASTV
037800150511     C*
037900150511     C                   ENDSR
038000150511     C/EJECT
038001160728     C************************************************************
038002160728     C* Ricerca capoconto
038003160728     C************************************************************
038004160728     C     F4_Kcc        BEGSR
038005160728     C*
038006160728     C                   Reset                   ANA200DS
038007160728     C*
038012160728     C                   MoveL     '01'          OPZ200
038014160728     C                   MoveL     D1Kcc         KCC200
038015160728     C                   MoveL     ANA200DS      KPJBU
038016160728     C                   CALL      'ANA200R'                            21
038017160728     C                   PARM                    KPJBA
038018160728     C                   MoveL     KPJBU         ANA200DS
038019160728     C*
038020160728     C     KCC200        IFNE      *BLANK
038021160728     C                   MOVE      KCC200        D1Kcc
038022160728     C                   ENDIF
038023160728     C*
038024160728     C                   ENDSR
038025160728     C/EJECT
038056160728     C************************************************************
038057160728     C* Ricerca conto
038058160728     C************************************************************
038059160728     C     F4_Ksc        BEGSR
038060160728     C*
038061160728     C                   Reset                   ANA201DS
038062160728     C                   MOVE      XSCSOC        SOC201
038063160728     C                   MOVE      D1KCC         KCC201
038064160728     C                   MOVE      D1Ksc         KSC201
038069160728     C                   MoveL     '01'          OPZ201
038071160728     C                   MoveL     ANA201DS      KPJBU
038072160728     C                   CALL      'ANA201R'
038073160728     C                   PARM                    KPJBA
038074160728     C                   MoveL     KPJBU         ANA201DS
038075160728     C*
038076160728     C     KSC201        IFNE      *BLANK
038077160728     C                   MoveL     KCC201        D1KCC
038078160728     C                   MoveL     KSC201        D1KSC
038079160728     C                   ENDIF
038080160728     C*
038081160728     C                   ENDSR
038082160728     C/EJECT
038542150512     C************************************************************
038543150512     C* RIEMPIMENTO OPZIONI
038544150512     C************************************************************
038545150512     C     RIEOPZ        BEGSR
038546150512     C*
038600150511     C* Input :
038700150511     C* - $TX = schiera con tutti i testi disponibili
038800150511     C* - $FL = schiere con i flag di validità associati ai testi
038900150511     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
039000150511     C*          opzione usato
039100150511     C* - WMXO = variabile contenente il numero massimo di testi
039200150511     C*          opzione usabili
039300150511     C* - $L01 = Lunghezza prima riga
039400150511     C* - $L02 = Lunghezza seconda riga
039500150511     C* Output :
039600150511     C* - WOPZ1 = variabile contenente la prima riga opzioni
039700150511     C* - WOPZ2 = variabile contenente la seconda riga opzioni
039800150511     C* - FF23  = flag che indica se c'è bisogno del tasto F23
039900150511     C*
040000150511     C                   CALLB     'X04OPMSG'
040100150511     C* input
040200150511     C                   PARM                    $TX
040300150511     C                   PARM                    $FL
040400150511     C                   PARM                    WCOZ              3 0
040500150511     C                   PARM                    WMXO              3 0
040600150511     C* output
040700150511     C                   PARM                    WOPZ1            76
040800150511     C                   PARM                    WOPZ2            76
040900150511     C                   PARM                    FF23              1
041000150511     C* input
041100150511     C                   PARM                    $L01              3 0
041200150511     C                   PARM                    $L02              3 0
041300150511     C*
041400150512     C                   PARM                    $ALLOPT         960
041500150512     C*
041600150511     C                   ENDSR
041700150511     C/EJECT
041800150511     C************************************************************
041900150511     C* RIEMPIMENTO TASTI FUNZIONE
042000150511     C************************************************************
042100150511     C     RIEKEY        BEGSR
042200150511     C*
042300150511     C* Input :
042400150511     C* - $FX = schiera con tutti i testi disponibili
042500150511     C* - $FC = schiere con i flag di validità associati ai testi
042600150511     C* - $FR = schiere con i flag di REVERSE IMAGE
042700150511     C* - $ULK = ultimo testo caricato
042800150511     C* - $L01 = Lunghezza prima riga
042900150511     C* - $L02 = Lunghezza seconda riga
043000150511     C* Output :
043100150511     C* - $ULK
043200150511     C* - $KEY1= 1° riga di tasti funzione
043300150511     C* - $KEY2= 2° riga di tasti funzione
043400150511     C*
043500150511     C                   CALLB     'XKEYMSG'
043600150511     C* input
043700150511     C                   PARM                    $FX
043800150511     C                   PARM                    $FC
043900150511     C                   PARM                    $FR
044000150511     C                   PARM                    $ULK              3 0
044100150511     C* output
044200150511     C                   PARM                    $KEY1            79
044300150511     C                   PARM                    $KEY2            79
044400150511     C* input
044500150511     C                   PARM                    $L01              3 0
044600150511     C                   PARM                    $L02              3 0
044700150511     C*
044800150512     C                   PARM                    $ALLFUNCT       480
044900150512     C*
045000150511     C                   ENDSR
045100150511     C/EJECT
045200150511     C************************************************************
045300150511     C* GESTIONE EMISSIONE TASTI FUNZIONE
045400150511     C************************************************************
045500150511     C     GESPIE        BEGSR
045600150511     C*
045700150511     C                   SELECT
045800150511     C* Parzializzazioni
045900150511     C     $GEST         WHENEQ    'D1'
046000150511     C     $ULKD1        IFEQ      0
046100150511     C* riempimento opzioni
046200150511     C                   EXSR      F24D1
046300150511     C                   ELSE
046400150511     C                   WRITE     FMTZ1
046500150511     C                   ENDIF
046600150511     C* Lista
046700150511     C     $GEST         WHENEQ    'S1'
046800150511     C     $ULKS1        IFEQ      0
046900150511     C                   EXSR      F24S1
047000150511     C                   ELSE
047100150511     C                   WRITE     FMTZ2
047200150511     C                   ENDIF
047300150511     C* Modifica valori assunti
047400150511     C     $GEST         WHENEQ    'D2'
047500150511     C     $ULKD2        IFEQ      0
047600150511     C                   EXSR      F24D2
047700150511     C                   ELSE
047800150511     C                   WRITE     FMTZ3
047900150511     C                   ENDIF
048000150511     C                   ENDSL
048100150511     C*
048200150511     C                   ENDSR
048300150511     C/EJECT
048400150511     C************************************************************
048500150511     C* GESTIONE PARZIALIZZAZIONI
048600150511     C************************************************************
048700150511     C     GESD1         BEGSR
048800150511     C* inizializzazione videata
048900150511     C     $INZD1        IFEQ      *ON
049000150511     C                   EXSR      INZD1
049100150511     C                   MOVE      *OFF          $INZD1
049200150511     C                   ENDIF
049300150511     C* emissione piede videata se proveniente da altra
049400150511     C* salvataggio valore formato di provenienza
049500150511     C     $LASTV        IFNE      'D1'
049501150512     C*
049502150512     C* recupero il sottotitolo (se c'è)
049503150512     C                   Clear                   D1TIT
049504150512     C     SOTTITD1      IFNE      *BLANK
049505150512     C                   MOVEL     SOTTITD1      IDMSG
049506150512     C                   CALLB     'XRTVM'
049507150512     C                   PARM                    IDMSG            27
049508150512     C                   PARM                    TXTMSG          100
049509150512     C                   PARM                    ERRMSG            1
049510150512     C                   PARM      *Off          CTRMSG            1            centratura
049511150512     C                   PARM      15            LENMSG            3 0          lun output.
049512150512     C     ERRMSG        IFNE      *ON
049513150512     C                   MOVEL     TXTMSG        D1TIT
049514150512     C                   ELSE
049515150512     C                   MOVEL     *ALL'?'       D1TIT
049516150512     C                   ENDIF
049517150512     C                   ENDIF
049518150512     C*
049600150511     C* salvo stato F17 per sapere cosa fare in caso di F12
049700150511     C                   MOVE      *INKR         WINKR
049800150511     C                   WRITE     FMTT1
049900150511     C                   EXSR      GESPIE
050000150511     C                   MOVE      $LASTV        $LASTG
050100150511     C                   MOVE      'D1'          $LASTV
050200150511     C                   ENDIF
050300150511     C*
050400150511     C* se esistono errori sulla videata
050500150511     C* emetto la write del formato a indicatori spenti per vedere
050600150511     C* le eventuali decodifiche
050700150511     C     *IN99         IFEQ      *ON
050800150511     C                   MOVEA     *IN           WIN
050900150511     C                   MOVE      *ALL'0'       IN5098           49
051000150511     C                   MOVEA     IN5098        *IN(50)
051100150511     C                   WRITE     FMTD1
051200150511     C                   MOVEA     WIN           *IN
051300150511     C                   ENDIF
051400150511     C*
051500150511     C                   EXFMT     FMTD1
051600150511     C*
051700150511     C* azzero la posizione del cursore
051800150511     C                   Z-ADD     0             H1RIGA
051900150511     C                   Z-ADD     0             H1COLO
052000150511     C*
052100150511     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
052200150511     C* INPUT
052300150511     C* $FC1 = Abilitazioni videata D1
052400150511     C* OUTPUT
052500150511     C* *IN97 + *IN99 Errori
052600150511     C                   MOVEA     $FC1          $FC
052700150511     C                   EXSR      CTRKEY
052800150511     C*
052900150511     C     *IN99         IFEQ      *OFF
053000150511     C*
053100150511     C                   SELECT
053200150511     C* F2=Decodifica
053300150511     C     $TASTO        WHENEQ    F02
053400150511     C                   EXSR      F02D1
053500150511     C* F3=Fine
053600150511     C     $TASTO        WHENEQ    F03
053700150511     C                   EXSR      F03D1
053800150511     C* F4=Ricerca
053900150511     C     $TASTO        WHENEQ    F04
054000150511     C                   EXSR      SEARCH
054100150511     C* F5=Refresh
054200150511     C     $TASTO        WHENEQ    F05
054300150511     C                   EXSR      F05D1
054400150511     C* F8=Successivo
054500150511     C     $TASTO        WHENEQ    F08
054600150511     C                   EXSR      F08D1
054700150511     C* F10=Immissione
054800150511     C     $TASTO        WHENEQ    F10
054900150511     C                   EXSR      F10D1
055000150511     C* F12=Ritorno
055100150511     C     $TASTO        WHENEQ    F12
055200150511     C                   EXSR      F12D1
055300150511     C* F18=Modifica valori assunti
055400150511     C     $TASTO        WHENEQ    F18
055500150511     C                   EXSR      F18D1
055600150511     C* F24=Altri tasti
055700150511     C     $TASTO        WHENEQ    F24
055800150511     C                   EXSR      F24D1
055900150511     C*
056000150511     C                   OTHER
056100150511     C                   EXSR      CTRD1
056200150511     C     *IN99         IFEQ      *OFF
056300150511     C* definisco l'ordinamento
056400150511     C                   EXSR      DEFORD
056500150511     C* richiamo la lista
056600150511     C                   MOVE      'S1'          $GEST
056700150511     C                   MOVE      *ON           $INZS1
056800150511     C                   MOVE      *BLANK        W1POS
056900150511     C                   ENDIF
057000150511     C*
057100150511     C                   ENDSL
057200150511     C*
057300150511     C                   ENDIF
057400150511     C*
057500150511     C                   ENDSR
057600150511     C/EJECT
057700150511     C************************************************************
057800150511     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
057900150511     C************************************************************
058000150511     C     INZD1         BEGSR
058100150511     C*
058200150511     C                   CLEAR                   FMTD1
058209150514     C*
058300150511     C* Se pgm  richiamato con parametri forniti , li imposto nelle
058400150511     C*    parzializzazioni
058500150511     C                   SELECT
058600150511     C* il pgm è richiamato e
058700150511     C* sono stati passati dei valori di parzializzazione ma non
058800150511     C* la chiave univoca
058900160728    >C     OPZK11        WHENNE    *BLANK
059000150511     C* imposto i valori passati sui campi a video
059100160728    >C                   MoveL     KccK11        D1Kcc
059101160728    >C                   MoveL     KscK11        D1Ksc
059300160729     C                   if        DtValK11   <> *Loval
059400160729     C     *JobRun       Move      DtValK11      D1DtVal
059500150512     C                   endif
059501150512     C*
059600150511     C* il pgm non è richiamato
059700160728    >C     OPZK11        WHENEQ    *BLANK
059800150511     C* imposto dei valori di default
059900150511     C                   ENDSL
060000150511     C*
060100150511     C                   ENDSR
060200150511     C/EJECT
060300150511     C************************************************************
060400150511     C* GESTIONE F02 VIDEO D1
060500150511     C************************************************************
060600150511     C     F02D1         BEGSR
060700150511     C*
060800150525     C* per decodificare la videata richiamo la routine dei ctrl
060900150511     C* e poi disattivo gli eventuali messaggi di errore
061000150511     C                   EXSR      CTRD1
061100150511     C*
061200150511     C                   MOVE      *ALL'0'       IN5098
061300150511     C                   MOVEA     IN5098        *IN(50)
061400150511     C*
061500150511     C                   ENDSR
061600150511     C/EJECT
061700150511     C************************************************************
061800150511     C* GESTIONE F03 VIDEO D1
061900150511     C************************************************************
062000150511     C     F03D1         BEGSR
062100150511     C*
062200150511     C                   MOVE      *ON           $FINE
062300160728    >C                   MOVE      '1'           RETK11
062400150511     C* fine programma
062500150511     C                   EXSR      ENDPGM
062600150511     C*
062700150511     C                   ENDSR
062800150511     C/EJECT
062900150511     C************************************************************
063000150511     C* GESTIONE F05 VIDEO D1
063100150511     C************************************************************
063200150511     C     F05D1         BEGSR
063300150511     C*
063400150511     C                   MOVE      *ON           $INZD1
063500150511     C*
063600150511     C                   ENDSR
063700150511     C/EJECT
063800150511     C************************************************************
063900150511     C* GESTIONE F08 VIDEO D1
064000150511     C************************************************************
064100150511     C     F08D1         BEGSR
064200150511     C*
064300150511     C                   MOVE      *ON           $FINE
064400160728    >C                   MOVE      '0'           RETK11
064500150511     C*
064600150511     C                   ENDSR
064700150511     C/EJECT
064800150511     C************************************************************
064900150511     C* GESTIONE F10 VIDEO D1
065000150511     C************************************************************
065100150511     C     F10D1         BEGSR
065200150511     C*
065300160728    >C                   RESET                   SDGSK12DS
065400160728    >C                   MOVE      xscsoc        SocietaK12
065500160728    >C                   MOVEL     '01'          OPZK12
065600160728    >C     CMTK11        IFEQ      '1'
065700160728    >C                   MOVEL     '1'           CMTK12
065800150511     C                   ELSE
065900160728    >C                   MOVEL     '0'           CMTK12
066000150511     C                   ENDIF
066100150511     C* passare anche le parzializzazioni ricevute relative alla chiave
066200150511     C* univoca :
066300150511     C*  se ce ne sono, potranno essere usate come dft d'immissione
066400160728    >C                   IF        OPZK11 <> *Blank
066500160728     C                   MoveL     D1Ksc         KscK12
066501160728     C                   MoveL     D1Kcc         KccK12
066700150511     C                   ENDIF
066701150512     C*
066800160728    >C                   MOVEL     SDGSK12DS     KPJBU
066900160728    >C                   CALL      'SDGSK12R'
067000150511     C                   PARM                    KPJBA
067100160728    >C                   MOVEL     KPJBU         SDGSK12DS
067200150511     C* ritorno da PGM gestione
067300160728    >C                   EXSR      RTNK12
067400150511     C*
067500150511     C                   MOVE      *HIVAL        $LASTV
067600150511     C*
067700150511     C                   ENDSR
067800150511     C/EJECT
067900150511     C************************************************************
068000150511     C* GESTIONE F12 VIDEO D1
068100150511     C************************************************************
068200150511     C     F12D1         BEGSR
068300150511     C*
068400150511     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
068500150511     C*  per F12 torno alla lista
068600150511     C     $LOP          IFEQ      2
068700150511     C     WINKR         OREQ      *ON
068800150511     C                   MOVE      'S1'          $GEST
068900150511     C*  altrimenti, per F12 torno a menu
069000150511     C                   ELSE
069100150511     C                   MOVE      *ON           $FINE
069200160728    >C                   MOVE      '2'           RETK11
069300150511     C                   ENDIF
069400150511     C*
069500150511     C                   ENDSR
069600150511     C/EJECT
069700150511     C************************************************************
069800150511     C* GESTIONE F18 VIDEO D1
069900150511     C************************************************************
070000150511     C     F18D1         BEGSR
070100150511     C*
070200150511     C                   MOVE      'D2'          $GEST
070300150511     C                   MOVEL     *ON           $INZD2
070400150511     C*
070500150511     C                   ENDSR
070600150511     C/EJECT
070700150511     C************************************************************
070800150511     C* GESTIONE F24 VIDEO D1
070900150511     C* input
071000150511     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
071100150511     C* $FX1 =Testi tasti funzione
071200150511     C* $FC1 = Abilitazioni
071300150511     C* $FR1 = Inversioni di fondo
071400150511     C************************************************************
071500150511     C     F24D1         BEGSR
071600150511     C*
071700150511     C                   MOVEA     $FX1          $FX
071800150511     C                   MOVEA     $FC1          $FC
071900150511     C                   MOVEA     $FR1          $FR
072000150511     C                   Z-ADD     $ULKD1        $ULK
072100150511     C                   Z-ADD     $D1L01        $L01
072200150511     C                   Z-ADD     $D1L02        $L02
072300150511     C                   EXSR      RIEKEY
072400150511     C                   Z-ADD     $ULK          $ULKD1            3 0
072500150511     C                   MOVEA     $FC           $FC1
072600150511     C                   MOVEL     $KEY1         Z1KE1
072700150512     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
072800150511     C                   MOVEL     $KEY2         Z1KE2
072900150511     C                   WRITE     FMTZ1
073000150511     C*
073100150511     C*
073200150511     C                   ENDSR
073300150511     C/EJECT
073400150511     C************************************************************
073500150511     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
073600150511     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
073700150511     C* 93 94 96 97 98
073800150511     C************************************************************
073900150511     C     CTRD1         BEGSR
074000150511     C*
074100150511     C                   SETOFF                                       99
075000150511     C*
075001160728     C                   EXSR      CtrPercip
075004150512     C*
075005150512     C                   EXSR      CtrDtVal
079100150511     C*
079200150511     C                   ENDSR
079300150511     C/EJECT
079845160728     C************************************************************
079846160728     C* Controllo esistenza conto percipiente
079848160728     C************************************************************
079849160728     C     CtrPercip     BEGSR
079850160728     C*
079851160728     C                   Clear                   WKcc
079852160728     C                   Clear                   WKsc
079853160728     C*
079854160728     C                   Clear                   D1KSCD
079855160728     C*
079856160728     C     D1KCC         IFNE      'C'
079857160728     C     D1KCC         ANDNE     'F'
079858160728     C                   CALLB     'XALLINEA'
079859160728     C                   PARM                    D1KCC
079860160728     C                   PARM      6             LEN               5 0
079861160728     C                   PARM                    XSCINP
079862160728     C                   ENDIF
079863160728     C*
079864160728     C                   if        D1Ksc      <> *Blank
079865160728     C*
079866160728     C     D1KSC         IFNE      *Blanks
079867160728     C                   CALLB     'XALLINEA'
079868160728     C                   PARM                    D1KSC
079869160728     C                   PARM      8             LEN               5 0
079870160728     C                   PARM                    XSCINP
079871160728     C                   ENDIF
079872160728     C*
079873160728     C     D1KCC         IFEQ      'C       '
079874160728     C     D1KCC         OREQ      'F       '
079875160728     C                   CALLB     'XCAPCLIFOR'
079876160728     C                   PARM                    XScSoc
079877160728     C                   PARM      D1KCC         D1Kcc
079878160728     C                   PARM                    D1Ksc
079879160728   E1C                   ENDIF
079880160728     C*
079881160728     C                   Eval      WKsc        = D1Ksc
079882160728     C                   Eval      WKcc        = D1Kcc
079883160728     C                   EXSR      CALLMVC002
079884160728     C                   if        Esito       = *On
079886160728     C                   else
079891160728     C                   MoveL     DesBrev002    D1KSCD
079901160728     C                   endif
079902160728     C*
079903160728     C                   endif
079904160728     C*
079905160728     C                   ENDSR
079906160728     C/EJECT
079907160728     C***********************************************************
079908160728     C* Driver per controllo conto
079909160728     C***********************************************************
079910160728     C     CALLMvc002    BEGSR
079911160728     C*
079912160728     C                   Clear                   ND002DS
079913160728     C*
079914160728     C                   Eval      LenOut = %Size(ND002DS)
079915160728     C*
079916160728     C                   CALLB     'NDMVC002'
079917160728     C                   PARM                    XScSoc
079918160728     C                   PARM                    WKCC
079919160728     C                   PARM                    WKSC
079920160728     C                   PARM                    Data
079921160728     C                   PARM      *Off          GesMsg
079922160728     C                   PARM      *Off          Esito
079923160728     C                   PARM      'ND002DS'     StrutturaO
079924160728     C                   PARM                    ND002DS
079925160728     C                   PARM                    LenOut
079926160728     C*
079927160728     C                   ENDSR
079928160728     C/EJECT
081700150511     C************************************************************
081800150512     C* Controllo data validità
081900150511     C************************************************************
082000150512     C     CtrDtVal      BEGSR
082100150511     C*
082101150512     C                   Move      *Loval        $DtVal
082102150512     C                   if        D1DtVal    <> 0
082103150512     C     *JobRun       Test(D)                 D1DtVal                21
082104150512     C                   if        *In21       = *Off
082105150512     C     *JobRun       Move      D1DtVal       $DtVal
082106150512     C                   else
082107150514     C                   SetOn                                          5299
082108150512     C                   endif
082109150512     C                   endif
082110150512     C*
082111150512     C                   ENDSR
082112150512     C/EJECT
082113150512     C************************************************************
082114150512     C* DEFINIZIONE DELL'ORDINAMENTO
082115150512     C************************************************************
082116150512     C     DEFORD        BEGSR
082117150512     C*
082200150511     C                   Z-ADD     1             $ORD
082300150511     C*
082400150511     C                   ENDSR
082500150511     C/EJECT
082600150511     C************************************************************
082700150511     C* Chiamata programma con tasto personalizzato
082800150511     C************************************************************
082900150511     C     FPerS1        BEGSR
083000150511     C*
083100150511     C* salvo la DS di ingresso
083200160728    >C                   MOVEL     SDGSK11DS     $SaveDS
083300150511     C*
083400160728    >C                   MOVEL     SDGSK11DS     KPJBU
083500150511     C                   CALL      $PGF2(PosFun)                        22
083600150511     C                   PARM                    KPJBA
083700150512     C****               PARM                    NOMPGM
083800150511     C*
083900150525     C* ripristino la DS di ingresso
084000160728    >C                   MOVEL     $SaveDS       SDGSK11DS
084100150511     C*
084200150511     C                   ENDSR
084300150511     C/EJECT
084400150511     C************************************************************
084500150511     C* GESTIONE LISTA
084600150511     C************************************************************
084700150511     C     GESS1         BEGSR
084701150512     C*
084800150511     C* inizializzazione videata
084900150511     C     $INZS1        IFEQ      *ON
085000150511     C                   EXSR      INZS1
085100150511     C                   MOVE      *OFF          $INZS1
085200150511     C                   ENDIF
085201150512     C*
085300150511     C* emissione piede videata se proveniente da altra
085400150511     C* salvataggio valore formato di provenienza
085500150511     C     $LASTV        IFNE      'S1'
085600150511     C                   WRITE     FMTT1
085700150511     C                   EXSR      GESPIE
085800150511     C                   MOVE      $LASTV        $LASTG
085900150511     C                   MOVE      'S1'          $LASTV
086000150511     C                   ENDIF
086100150511     C*
086200150511     C     WMAX          IFEQ      0
086300150511     C                   WRITE     FMTD3                                        NO RCD
086400150511     C                   ENDIF
086500150511     C* Inizializzazione indice tasti funzione personalizzati
086600150511     C                   Eval      PosFun = 25
086700150511     C*
086800150511     C                   EXFMT     FMTC1
086900150511     C*
087000150511     C* azzero la posizione del cursore
087100150511     C                   Z-ADD     0             H1RIGA
087200150511     C                   Z-ADD     0             H1COLO
087300150511     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
087400150511     C* di refresh dello sfondo
087500150511     C                   MOVE      '1'           WExf
087600150511     C*
087700150511     C     C1NRR         IFNE      0
087800150511     C                   Z-ADD     C1NRR         WSFL
087900150511     C                   ENDIF
088000150511     C                   IF        SFLNRR > 0
088100150511     C                   Z-ADD     SFLNRR        C1RCD
088200150511     C                   ENDIF
088300150511     C* inizializzazione indice opzioni personalizzate
088400150511     C                   Eval      PosOpz = 0
088500150511     C* CONTROLLO ABILITAZIONE TASTI
088600150511     C* INPUT
088700150511     C* $FC2 = Abilitazioni videata S1
088800150511     C* OUTPUT
088900150511     C* *IN97 + *IN99 Errori
089000150511     C                   MOVEA     $FC2          $FC
089100150511     C                   EXSR      CTRKEY
089200150511     C*
089300150511     C     *IN99         IFEQ      *OFF
089400150511     C*
089500150511     C                   SELECT
089600150511     C* F3=Fine
089700150511     C     $TASTO        WHENEQ    F03
089800150511     C                   EXSR      F03S1
089900150511     C* F8=Successivo
090000150511     C     $TASTO        WHENEQ    F08
090100150511     C                   EXSR      F08S1
090200150511     C* F10=Immissione
090300150511     C     $TASTO        WHENEQ    F10
090400150511     C                   EXSR      F10S1
090500150511     C* F12=Ritorno
090600150511     C     $TASTO        WHENEQ    F12
090700150511     C                   EXSR      F12S1
090800150511     C* F17=Parzializzazioni
090900150511     C     $TASTO        WHENEQ    F17
091000150511     C                   EXSR      F17S1
091100150511     C* F18=Modifica valori assunti
091200150511     C     $TASTO        WHENEQ    F18
091300150511     C                   EXSR      F18S1
091400150511     C* F23=Altre opzioni
091500150511     C     $TASTO        WHENEQ    F23
091600150511     C                   EXSR      F23S1
091700150511     C* F24=Altri tasti
091800150511     C     $TASTO        WHENEQ    F24
091900150511     C                   EXSR      F24S1
092000150511     C* ROLL-UP
092100150511     C     $TASTO        WHENEQ    ROLLUP
092200150511     C                   EXSR      ROLS1
092300150511     C* Personalizzata
092400150511     C     $PGF2(PosFun) Whenne    *BLANK
092500150511     C                   EXSR      FperS1
092600150511     C*
092700150511     C                   OTHER
092800150511     C* CONTROLLO DATI
092900150511     C                   EXSR      CTRC1
093000150511     C     *IN99         IFEQ      *OFF
093100150511     C                   EXSR      CTRS1
093200150511     C                   ENDIF
093300150511     C*
093400150511     C                   ENDSL
093500150511     C*
093600150511     C                   ENDIF
093700150511     C*
093800150511     C                   ENDSR
093900150511     C/EJECT
094000150511     C************************************************************
094100150511     C* INIZIALIZZAZIONE LISTA
094200150511     C************************************************************
094300150511     C     INZS1         BEGSR
094400150511     C* pulizia SFL
094500150511     C                   SETOFF                                         3031
094600150511     C                   WRITE     FMTC1
094700150511     C                   SETON                                          31
094701150512     C*
094702150512     C* pulizia del sottotitolo
094703150512     C                   Clear                   D1TIT
094800150511     C*
094900150511     C* riempimento opzioni
095000150511     C                   MOVEA     $TX1          $TX
095100150511     C                   MOVEA     $FL1          $FL
095200150511     C                   Z-ADD     0             WCOZ
095300150511     C                   Z-ADD     0             WMXO
095400150511     C*  equivale al num.di elementi della schiera $TX1
095500150511     C                   Z-add     OpzLen        WMXO
095600150511     C                   Z-ADD     $C1Op1        $L01
095700150511     C                   Z-ADD     $C1Op2        $L02
095800150511     C                   EXSR      RIEOPZ
095900150511     C                   Z-ADD     23            X
096000150511     C                   MOVE      FF23          $FC2(X)
096100150511     C                   MOVEL     WOPZ1         C1OPZ1
096200150512     C                   MOVEL     $ALLOPT       H1ALLOPT
096300150511     C                   MOVEL     WOPZ2         C1OPZ2
096400150511     C                   Z-ADD     WCOZ          C1COZ
096500150511     C*
096600150511     C* CARICAMENTO SFL A PAGINE
096700150511     C*
096800150511     C                   Z-ADD     0             S1NRR
096900150511     C                   Z-ADD     0             WMAX
097000150511     C*
097100150511     C* Posizionamento su file pilota
097200150511     C                   CLEAR                   C1POS
097300150511     C                   EXSR      SETANA
097400150525     C* ripristino i valori del posizionamento
097500150511     C                   MOVE      W1POS         C1POS
097600150511     C*
097700150511     C* Lettura file pilota
097800150511     C                   EXSR      REDANA
097900150511     C*
098000150511     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
098100150511     C                   IF        $LastV = 'D1'
098200150511     C                   Z-ADD     SFLPAG        WPAG
098300150511     C                   ENDIF
098400150511     C                   EXSR      ROLS1
098500150511     C* imposto il caricamento sfl del numero massimo di righe per pagina
098600150511     C                   Z-ADD     SFLPAG        WPAG
098700150511     C*
098800150511     C                   ENDSR
098900150511     C/EJECT
099000150511     C************************************************************
099100150511     C* CARICAMENTO PAGINA LISTA
099200150511     C************************************************************
099300150511     C     ROLS1         BEGSR
099400150511     C*
099500150511     C                   SETOFF                                       32
099600150511     C                   Z-ADD     0             Y
099700150511     C                   Z-ADD     WMAX          S1NRR
099800150511     C*
099900150511     C* Leggo dal file anagrafico per caricare la lista
100000150511     C*
100100150511     C     $EndFile      DOWEQ     *OFF
100200150511     C     Y             ANDLT     WPAG
100300150511     C*
100400150511     C                   EXSR      RIES1
100500150511     C*
100600150511     C                   ADD       1             S1NRR
100700150511     C                   ADD       1             Y
100800150511     C*
100900150511     C                   WRITE     FMTS1
101000150511     C*
101100150511     C                   EXSR      REDANA
101200150511     C*
101300150511     C                   ENDDO
101400150511     C*
101500150511     C                   Z-ADD     S1NRR         WMAX                 30
101600150511     C*
101700150511     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
101800150511     C*
101900150511     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
102000150511     C                   MVR                     RESTO             3 0
102100150511     C     PAGINE        MULT      SFLPAG        C1RCD
102200150511     C     RESTO         IFGT      0
102300150511     C                   ADD       1             C1RCD
102400150511     C                   ELSE
102500150511     C                   SUB       SFLPAG        C1RCD
102600150511     C                   ADD       1             C1RCD
102700150511     C                   ENDIF
102800150511     C*
102900150511     C                   ENDSR
103000150511     C/EJECT
103100150511     C************************************************************
103200150511     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
103300150511     C************************************************************
103400150511     C     SETANA        BEGSR
103500150511     C*
103600160728    >C                   MOVE      XScSoc        SPASocieta
103700150511     C*
103800150511     C                   SELECT
103900150511     C*
104000150511     C* Gestione posizionamento
104100150511     C* Se il campo W1POS è > *blank imposto il posizionamento con
104200150511     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
104300150511     C*
104400150511    >C* Ord.1 = chiave ordinamento 1
104500150511     C     $ORD          WHENEQ    1
104600150511     C     W1POS         IFNE      *BLANK
104700150511    >C****               MOVEL     W1POS         ___Codice
104800150511     C                   ELSE
104900150511     C                   ENDIF
105000160728    >C     SPASocieta    SETLL     SDGSPA01L
105100150511     C                   ENDSL
105200150511     C*
105300150511     C                   ENDSR
105400150511     C/EJECT
105500150511     C************************************************************
105600150511     C* LETTURA RCD ARCHIVIO PILOTA
105700150511     C************************************************************
105800150511     C     REDANA        BEGSR
105900150511     C*
106000150511     C                   MOVEL     *OFF          $EndFile
106100150511     C                   MOVEL     *OFF          $RCDOK
106200150511     C*
106300150511     C     $EndFile      DOUEQ     *ON
106400150511     C     $RCDOK        OREQ      *ON
106500150511     C*
106600150511     C                   SELECT
106700150511     C*
106800150511     C     $ORD          WHENEQ    1
106900160728     C                   MOVE      XscSoc        SPASocieta
107000160728     C     SPASocieta    READE     SDGSPA01L                              21
107100150511     C                   ENDSL
107200150511     C*
107300150511     C                   MOVE      *IN21         $EndFile
107400150511     C*
107500150511     C     $EndFile      IFEQ      *OFF
107600150511     C                   EXSR      SELANA
107700150511     C                   ENDIF
107800150511     C*
107900150511     C                   ENDDO
108000150511     C*
108100150511     C                   MOVE      $EndFile      *IN33                          SflEnd
108200150511     C*
108300150511     C                   ENDSR
108400150511     C/EJECT
108500150511     C************************************************************
108600150511     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
108700150511     C************************************************************
108800150511     C     CLCPAG        BEGSR
108900150511     C* Input :
109000150511     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
109100150511     C*          cursore
109200150511     C* - SFLPAG = numero rcd per pagina sfl
109300150511     C* Output :
109400150511     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
109500150511     C*
109600150511     C     WSFL          DIV       SFLPAG        PAGINE            4 0
109700150511     C                   MVR                     RESTO             3 0
109800150511     C     RESTO         IFGT      0
109900150511     C                   ADD       1             PAGINE
110000150511     C                   ENDIF
110100150511     C     PAGINE        MULT      SFLPAG        WPAG
110200150511     C*
110300150511     C                   ENDSR
110400150511     C/EJECT
110500150511     C************************************************************
110600150511     C* SELEZIONI SULLA RIGA LETTA
110700150511     C* impostare a *OFF il campo $RCDOK per scartare un record che
110800150511     C* non rientra nei parametri di parzializzazione
110900150511     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
111000150511     C* superiore stabilito nei parametri
111100150511     C*
111200150511     C************************************************************
111300150511     C     SELANA        BEGSR
111400150511     C*
111500150511     C                   MOVEL     *ON           $RCDOK
111600150511     C                   MOVEL     *ALL'9'       ALL9             15
111800150511     C*
111900150511     C                   SELECT
112300150511     C*
112302160728     C* Percipiente non compreso nella scelta
112303160728     C                   When      SPAKcc     <> D1Kcc      and
112304160728     C                             D1Kcc      <> *Blanks
112305150512     C                   MOVEL     *OFF          $RCDOK
112306160728     C*
112307160728     C                   When      SPAKsc     <> D1Ksc      and
112308160728     C                             D1Ksc      <> *Blanks
112309160728     C                   MOVEL     *OFF          $RCDOK
113300150511     C*
113400150511     C* data validità non compresa nella scelta
113500150511     C                   When      $DtVal <> *Loval AND (
113600160729     C                              (SPADtFin < $DtVal And SPADtFin <> *Hival)
113700150511     C                                OR
113800160728     C                              (SPADtIni > $DtVal And SPADtIni <> *Loval) )
113900150511     C                   MOVEL     *OFF          $RCDOK
114000150511     C*
114100150511     C                   ENDSL
118400150511     C*
118500150511     C                   ENDSR
118600150511     C/EJECT
118700150511     C************************************************************
118800150511     C* RIEMPIMENTO RIGA LISTA
118900150511     C************************************************************
119000150511     C     RIES1         BEGSR
119100150511     C*
119200150511     C                   CLEAR                   FMTS1
119300150511     C*
119400150511     C* riempio i campi del sfl, compresi gli hidden con i valori
119500150511     C* del rcd letto
119600160728     C                   MoveL     SPAKcc        S1Kcc
119601160728     C                   MoveL     SPAKsc        S1Ksc
119602150512     C*
119603150512     C*
119604160728     C                   Clear                   S1KscD
119608160728     C                   Eval      WKcc        = S1Kcc
119609160728     C                   Eval      WKsc        = S1Ksc
119612160728     C                   EXSR      CALLMVC002
119613160728     C                   if        Esito      <> *On
119614160728     C                   MoveL     DesBrev002    S1KSCD
119615160728     C                   endif
120100150511     C*
120200160728     C                   if        SPADtIni   <> *Loval
120300160728     C     *JobRun       MoveL     SPADtIni      S1DtIni
120400150512     C                   endif
120401150512     C*
120402160729     C                   if        SPADtFin   <> *Hival
120403160728     C     *JobRun       MOVEL     SPADtFin      S1DtFin
120404150512     C                   endif
120405150512     C*
120406160728     C                   Eval      S1Tributo   = SPATributo
120407160729     C                   Eval      S1TriProg   = SPATriProg
122000150511     C*
122100150511     C* imposto quegli attributi del sfl che sono modificabili
122200150511     C* al richiamo del determinato pgm
122300150511     C                   EXSR      ATRS1
122400150511     C*
122500150511     C                   ENDSR
122600150511     C/EJECT
122700150511     C************************************************************
122800150525     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
122900150511     C************************************************************
123000150511     C     ATRS1         BEGSR
123100150511     C*
123200150525     C* in questa routine bisogna attivare indicatori o campi programma
123300150511     C* che gestiscono gli attributi di uno o più campi di questa videata;
123400150511     C* attenzione : non è questo il posto in cui eseguire controlli,
123500150511     C*  dovrebbero già arrivare dei flag
123600150511     C*
123700150511     C*
123800150511     C                   ENDSR
123900150511     C/EJECT
124000150511     C************************************************************
124100150511     C* GESTIONE F08 VIDEO S1
124200150511     C************************************************************
124300150511     C     F08S1         BEGSR
124400150511     C*
124500150511     C                   MOVE      *ON           $FINE
124600160728    >C                   MOVE      '0'           RETK11
124700150511     C*
124800150511     C                   ENDSR
124900150511     C/EJECT
125000150511     C************************************************************
125100150511     C* GESTIONE F03 VIDEO S1
125200150511     C************************************************************
125300150511     C     F03S1         BEGSR
125400150511     C*
125500150511     C                   MOVE      *ON           $FINE
125600160728    >C                   MOVE      '1'           RETK11
125700150511     C* fine programma
125800150511     C                   EXSR      ENDPGM
125900150511     C*
126000150511     C                   ENDSR
126100150511     C/EJECT
126200150511     C************************************************************
126300150511     C* GESTIONE F10 VIDEO S1
126400150511     C* AGGIUNTA RECORD
126500150511     C************************************************************
126600150511     C     F10S1         BEGSR
126700150511     C*
126800160728    >C                   RESET                   SDGSK12DS
126900160728    >C                   MOVE      xscsoc        SocietaK12
127000160728    >C                   MOVEL     '01'          OPZK12
127100160728    >C     CMTK11        IFEQ      '1'
127200160728    >C                   MOVEL     '1'           CMTK12
127300150511     C                   ELSE
127400160728    >C                   MOVEL     '0'           CMTK12
127500150511     C                   ENDIF
127600150511     C* passare anche le parzializzazioni ricevute relative alla chiave
127700150511     C* univoca :
127800150511     C*  se ce ne sono, potranno essere usate come dft d'immissione
127900160728    >C                   IF        OPZK11 <> *Blank
128000160728     C                   MoveL     D1Ksc         KscK12
128001160728     C                   MoveL     D1Kcc         KccK12
128200150511     C                   ENDIF
128300160728    >C                   MOVEL     SDGSK12DS     KPJBU
128400160728    >C                   CALL      'SDGSK12R'
128500150511     C                   PARM                    KPJBA
128600160728    >C                   MOVEL     KPJBU         SDGSK12DS
128700150511     C* ritorno da PGM gestione
128800160728    >C                   EXSR      RTNK12
128900150511     C     WINZS1        IFEQ      *ON
129000150511     C                   MOVE      *ON           $INZS1
129100150511     C* se esistevano già righe sul sfl
129200150511     C* calcolo pagina a cui deve posizionarsi
129300150511     C     WSFL          IFGT      0
129400150511     C                   EXSR      CLCPAG
129500150511     C* altrimenti carico solo la 1a pagina
129600150511     C                   ELSE
129700150511     C                   Z-ADD     SFLPAG        WPAG
129800150511     C                   ENDIF
129900150511     C                   ENDIF
130000150511     C*
130100150511     C                   MOVE      *HIVAL        $LASTV
130200150511     C*
130300150511     C                   ENDSR
130400150511     C/EJECT
130500150511     C************************************************************
130600150511     C* GESTIONE F12 VIDEO S1
130700150511     C************************************************************
130800150511     C     F12S1         BEGSR
130900150511     C*
131000150511     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
131100150511     C     $LOP          IFEQ      1
131200150511     C                   MOVE      'D1'          $GEST
131300150511     C*  se la 1a videata è questa, per F12 torno a menu
131400150511     C                   ELSE
131500150511     C                   MOVE      *ON           $FINE
131600160728    >C                   MOVE      '2'           RETK11
131700150511     C                   ENDIF
131800150511     C*
131900150511     C                   ENDSR
132000150511     C/EJECT
132100150511     C************************************************************
132200150511     C* GESTIONE F17 VIDEO S1
132300150511     C************************************************************
132400150511     C     F17S1         BEGSR
132500150511     C*
132600150511     C                   MOVE      'D1'          $GEST
132700150511     C* se il pgm è stato richiamato con chiave univoca
132800150511     C* e dopo è stata richiesta una parzializzazione
132900150511     C* tolgo la gestione della chiave univoca (azzere/abblenco il
133000150511     C* valore; propongo come ordinamento il 1°)
133100150511     C     $Unique       IFEQ      *ON
133200150511     C                   MOVE      *OFF          $Unique
133300150511     C                   ENDIF
133400150511     C     $ORD          IFEQ      99
133500150511     C                   Z-ADD     1             $ORD
133600150511     C                   MOVE      *ON           $INZD1
133700150511     C                   ENDIF
133800150511     C*
133900150511     C                   ENDSR
134000150511     C/EJECT
134100150511     C************************************************************
134200150511     C* GESTIONE F18 VIDEO S1
134300150511     C************************************************************
134400150511     C     F18S1         BEGSR
134500150511     C*
134600150511     C                   MOVE      'D2'          $GEST
134700150511     C                   MOVEL     *ON           $INZD2
134800150511     C     $ORD          IFEQ      99
134900150511     C                   Z-ADD     1             $ORD
135000150511     C                   ENDIF
135100150511     C*
135200150511     C                   ENDSR
135300150511     C/EJECT
135400150511     C************************************************************
135500150511     C* GESTIONE F23 VIDEO S1
135600150511     C************************************************************
135700150511     C     F23S1         BEGSR
135800150511     C*
135900150511     C                   Z-ADD     $C1Op1        $L01
136000150511     C                   Z-ADD     $C1Op2        $L02
136100150511     C                   EXSR      RIEOPZ
136200150511     C                   Z-ADD     23            X
136300150511     C                   MOVE      FF23          $FC2(X)
136400150511     C                   MOVEL     WOPZ1         C1OPZ1
136500150512     C                   MOVEL     $ALLOPT       H1ALLOPT
136600150511     C                   MOVEL     WOPZ2         C1OPZ2
136700150511     C                   Z-ADD     WCOZ          C1COZ
136800150511     C*
136900150511     C                   ENDSR
137000150511     C/EJECT
137100150511     C************************************************************
137200150511     C* GESTIONE F24 VIDEO S1
137300150511     C************************************************************
137400150511     C     F24S1         BEGSR
137500150511     C*
137600150511     C                   MOVEA     $FX2          $FX
137700150511     C                   MOVEA     $FC2          $FC
137800150511     C                   MOVEA     $FR2          $FR
137900150511     C                   Z-ADD     $ULKS1        $ULK
138000150511     C                   Z-ADD     $S1L01        $L01
138100150511     C                   Z-ADD     $S1L02        $L02
138200150511     C                   EXSR      RIEKEY
138300150511     C                   Z-ADD     $ULK          $ULKS1            3 0
138400150511     C                   MOVEA     $FC           $FC2
138500150511     C                   MOVEL     $KEY1         Z2KE1
138600150512     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
138700150511     C                   MOVEL     $KEY2         Z2KE2
138800150511     C                   WRITE     FMTZ2
138900150511     C*
139000150511     C                   ENDSR
139100150511     C/EJECT
139200150511     C************************************************************
139300150511     C* CONTROLLO TESTATA LISTA
139400150511     C************************************************************
139500150511     C     CTRC1         BEGSR
139600150511     C*
139700150511     C                   MOVE      *OFF          *IN99
139800150511     C*
139900150511     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
140000150511     C* L'INIZIALIZZAZIONE LISTA
140100150511     C*
140200150511     C     C1POS         IFNE      W1POS
140300150511     C                   EXSR      POSS1
140400150511     C                   ENDIF
140500150511     C*
140600150511     C                   ENDSR
140700150511     C/EJECT
140800150511     C************************************************************
140900150511     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
141000150511     C************************************************************
141100150511     C     POSS1         BEGSR
141200150511     C*
141300150511     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
141400150511     C*
141500150511   B1C     WMAX          IFGT      0
141600150511     C                   READC     FMTS1                                  21
141700150511   B2C     *IN21         DOWEQ     *OFF
141800150511     C     *IN99         ANDEQ     *OFF
141900150511     C*
142000150511   B3C     S1OPZ         IFNE      0
142100150511     C                   SETON                                        92  99
142200150511   E3C                   ENDIF
142300150511     C* se rilevata opzione attiva, attivo sflnxtchg
142400150511   B3C     *IN99         IFEQ      *ON
142500150511     C                   MOVE      *ON           *IN32
142600150511     C*
142700150511     C* imposto quegli attributi del sfl che sono modificabili
142800150511     C* al richiamo del determinato pgm
142900150511     C                   EXSR      ATRS1
143000150511     C*
143100150511     C                   UPDATE    FMTS1
143200150511   X3C                   ELSE
143300150511     C                   READC     FMTS1                                  21
143400150511   E3C                   ENDIF
143500150511     C*
143600150511   E2C                   ENDDO
143700150511     C*
143800150511   E1C                   ENDIF
143900150511     C*
144000150511     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
144100150511     C* AL VALORE DI POSIZIONAMENTO :
144200150511     C* se questo è maggiore del valore del rispettivo campo di
144300150511     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
144400150511     C* con i valori di parzializzazione.
144500150511     C*
144600150511   B1C     *IN99         IFEQ      *OFF
144700150511    >C                   MOVE      *BLANK        W1POS
144800150511     C*
144900150511     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
145000150511     C* è maggiore del relativo valore di parzializzazione, nel qual caso
145100150511     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
145200150511     C* richiedo comunque un ricaricamento del sfl che, se non si è
145300150511     C* verificata la condizione suddetta avverrà con posizionamento a
145400150511     C* partire dal valore di parzializzazione
145500150511     C*
145600150511   B2C                   SELECT
145700150511     C*
145800150511     C     $ORD          WHENEQ    1
145900150511    >C     C1POS         IFGT      *Blanks
146000150511    >C                   MOVEL     C1POS         W1POS
146100150511     C                   ENDIF
146200150511     C*
146300150511   E2C                   ENDSL
146400150511     C*
146500150511     C* richiedo l'inizializzazione sfl
146600150511     C                   MOVE      *ON           $INZS1
146700150511     C*
146800150511   E1C                   ENDIF
146900150511     C*
147000150511     C                   ENDSR
147100150511     C/EJECT
147200150511     C************************************************************
147300150511     C* CONTROLLO OPZIONI LISTA
147400150511     C************************************************************
147500150511     C     CTRS1         BEGSR
147600150511     C*
147700150511     C                   MOVEL     *OFF          $ESCI
147800150511     C                   SETOFF                                       99
147900150511     C                   Z-ADD     0             S1OPZ
148000150511     C*
148100150511     C* Leggo il sfl solo se ci sono rcd
148200150511   B1C     WMAX          IFGT      0
148300150511     C                   READC     FMTS1                                  21
148400150511     C*
148500150511     C* esce se fine sfl o errore che richiede l'uscita
148600150511   B2C     *IN21         DOWEQ     *OFF
148700150511     C     $ESCI         ANDEQ     *OFF
148800150511     C                   Z-ADD     S1NRR         C1RCD
148900150511     C* ctrl su riga
149000150511     C                   EXSR      RECS1
149100150511     C* gestione opzioni
149200150511   B3C     S1OPZ         IFNE      0
149300150511     C     *IN99         ANDEQ     *OFF
149400150511     C                   EXSR      OPZS1
149500150511   E3C                   ENDIF
149600150511     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
149700150511   B3C     *IN99         IFEQ      *ON
149800150511     C                   MOVE      *ON           *IN32
149900150511   E3C                   ENDIF
150000150511     C*
150100150511     C* imposto quegli attributi del sfl che sono modificabili
150200150511     C* al richiamo del determinato pgm
150300150511     C                   EXSR      ATRS1
150400150511     C*
150500150511     C                   Z-ADD     0             S1OPZ
150600150511     C*
150700150511     C                   UPDATE    FMTS1
150800150511     C                   MOVE      *OFF          *IN32
150900150511     C*
151000160728     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
151100150511   B3C     $ESCI         IFEQ      *OFF
151200150511     C                   READC     FMTS1                                  21
151300150525     C* a fine ciclo ripristino il flag richiesta iniz.sfl
151400150511   B4C     *IN21         IFEQ      *ON
151500150511     C                   MOVE      WINZS1        $INZS1
151600150511     C                   MOVE      *off          WINZS1
151700150511     C* se è avvenuta almeno una modifica
151800150511   B5C                   IF        $INZS1 = *on
151900150511     C* calcolo pagina a cui deve posizionarsi
152000150511     C     WSFL          IFGT      0
152100150511     C                   EXSR      CLCPAG
152200150511     C                   ELSE
152300150511     C                   Z-ADD     SFLPAG        WPAG
152400150511     C                   ENDIF
152500150511   E5C                   ENDIF
152600150511   E4C                   ENDIF
152700150511   E3C                   ENDIF
152800150511     C*
152900150511   E2C                   ENDDO
153000150511     C*
153100150511   E1C                   ENDIF
153200150511     C*
153300150511     C                   ENDSR
153400150511     C/EJECT
153500150511     C************************************************************
153600150511     C* CONTROLLO CAMPI I/O RIGA LISTA
153700150511     C************************************************************
153800150511     C     RECS1         BEGSR
153900150511     C*
154000150511     C* reset indicatori DSPATR
154100150511     C                   MOVE      *ALL'0'       IN4049           10
154200150511     C                   MOVEA     IN4049        *IN(40)
154300150511     C*
154400150511     C* Controllo se Utente Autorizzato alla scelta
154500150511     C     S1OPZ         IFNE      0
154600150511     C                   Z-ADD     1             X
154700150511     C     S1OPZ         LOOKUP    $OP1(X)                                21
154800150511     C* mi salvo l'indice dell' opzione
154900150511     C   21              Z-add     X             PosOpz            2 0
155000150511     C     *IN21         IFEQ      *OFF
155100150511     C     $FL1(X)       OREQ      'N'
155200150511     C                   MOVE      *ON           $ESCI
155300150511     C                   SETON                                        409199
155400150511     C                   ENDIF
155500150511     C                   ENDIF
155600150511     C*
155700150511     C                   ENDSR
155800150511     C/EJECT
155900150511     C************************************************************
156000150511     C* GESTIONE OPZIONI LISTA
156100150511     C************************************************************
156200150511     C     OPZS1         BEGSR
156300150511     C*
156400150511     C                   Z-ADD     RIRI          CURR
156500150511     C                   Z-ADD     COCO          CURC
156600150511     C*
156700150511     C                   SELECT
156800150511     C*
156900150511     C* se opz=Selezione
157000150511     C* passo la chiave al pgm chiamante
157100150511     C     S1OPZ         WHENEQ    1
157200160728    >C                   MOVE      S1OPZ         OPZK11
157300160728     C* passare nei campi appositi della DS SDGSK11DS il valore
157400150511     C* della chiave della VL univoca (se c'è)
157500160728    >C                   MoveL     S1Kcc         KccK11
157501160728    >C                   MoveL     S1Ksc         KscK11
157700150512     C                   if        S1DtIni    <> 0
157800160728    >C     *JobRun       MOVE      S1DtIni       DtIniK11
157900150512     C                   else
158000160728    >C                   MOVE      *Loval        DtIniK11
158100150512     C                   endif
158200150511     C                   MOVE      *ON           $ESCI
158300150511     C                   MOVE      *ON           $FINE
158400150511     C*
158500150511     C                   MOVE      *HIVAL        $LASTV
158600150511     C*
158700150511     C* Spengo il flag che mi avverte dell'EXFMT fatta dal video di questo
158800150511     C* pgm, in modo che, se non viene eseguita e si richiama il pgm di
158900150511     C* sola finestra, questo non esegua il refresh dello sfondo
159000150511     C                   MOVE      '0'           WExf
159100150511     C*
159200150511     C* se selezionata opzione personalizzata, richiamo il programma
159300150511     C* corrispondente
159400150511     C     $PGO1(PosOpz) Whenne    *Blank
159500160728    >C                   RESET                   SDGSK12DS
159600160728    >C                   MOVE      xscsoc        SocietaK12
159700160728    >C                   MOVEL     S1OPZ         OPZK12
159800160728    >C     CMTK11        IFEQ      '1'
159900160728    >C                   MOVEL     '1'           CMTK12
160000150511     C                   ELSE
160100160728    >C                   MOVEL     '0'           CMTK12
160200150511     C                   ENDIF
160300160728     C* passare nei campi appositi della DS SDGSK12DS il valore
160400150511     C* della chiave della VL univoca
160401160728    >C                   MoveL     S1Kcc         KccK11
160402160728    >C                   MoveL     S1Ksc         KscK11
160404150512     C                   if        S1DtIni    <> 0
160405160728    >C     *JobRun       MOVE      S1DtIni       DtIniK11
160406150512     C                   else
160407160728    >C                   MOVE      *Loval        DtIniK11
160408150512     C                   endif
161200150511     C*
161300160728    >C                   MOVEL     SDGSK12DS     KPJBU
161400150511     C                   CALL      $PGO1(PosOpz)                        22
161500150511     C                   PARM                    KPJBA
161600150512     C****               PARM                    NOMPGM
161700150511     C*
161800160728    >C                   MOVEL     KPJBU         SDGSK12DS
161900150511     C* ritorno da PGM
162000160728    >C                   EXSR      RTNK12
162100150511     C*
162200150511     C* se altre opzioni
162300150511     C* richiamo pgm gestione anagrafica
162400150511     C                   OTHER
162500150511     C*
162600160728    >C                   RESET                   SDGSK12DS
162700160728    >C                   MOVE      xscsoc        SocietaK12
162800160728    >C                   MOVEL     S1OPZ         OPZK12
162900160728    >C     CMTK11        IFEQ      '1'
163000160728    >C                   MOVEL     '1'           CMTK12
163100150511     C                   ELSE
163200160728    >C                   MOVEL     '0'           CMTK12
163300150511     C                   ENDIF
163400160728     C* passare nei campi appositi della DS SDGSK12DS il valore
163500150511     C* della chiave della VL univoca
163501160728    >C                   MoveL     S1Kcc         KccK12
163502160728    >C                   MoveL     S1Ksc         KscK12
163503150512     C                   if        S1DtIni    <> 0
163504160728    >C     *JobRun       MOVE      S1DtIni       DtIniK12
163505150512     C                   else
163506160728    >C                   MOVE      *Loval        DtIniK12
163507150512     C                   endif
164300150511     C*
164400160728    >C                   MOVEL     SDGSK12DS     KPJBU
164500160728    >C                   CALL      'SDGSK12R'
164600150511     C                   PARM                    KPJBA
164700150511    >C****               PARM                    AUTDS
164800150511     C*
164900160728    >C                   MOVEL     KPJBU         SDGSK12DS
165000150511     C*
165100150511     C* ritorno da PGM gestione
165200160728    >C                   EXSR      RTNK12
165300150511     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
165400150511     C     *IN99         IFEQ      *ON
165500150511     C                   SETON                                        40
165600150511     C                   ENDIF
165700150511     C*
165800150511     C                   MOVE      *HIVAL        $LASTV
165900150511     C                   ENDSL
166000150511     C*
166100150511     C                   ENDSR
166200150511     C/EJECT
166300150511     C************************************************************
166400150511     C* GESTIONE RITORNO DA PGM DI GESTIONE
166500150511     C************************************************************
166600160728    >C     RTNK12        BEGSR
166700150511     C*
166800150511     C* modo di ritorno
166900150511     C*
167000150511     C                   SELECT
167100150511     C* questi modi di utilizzo dei valori di ritorno dal
167200150511     C* pgm di manutenzione rcd di anagrafica sono delle
167300150511     C* proposte, normalmente sempre valide, ma modificabili
167400150511     C* per situazioni particolari
167500150511     C* 1 = F3
167600160728    >C     RETK12        WHENEQ    '1'
167700150511     C                   MOVE      *ON           $ESCI                          sospese READC
167800150511     C                   MOVE      *ON           $FINE                          fine pgm
167900160728    >C                   MOVE      '1'           RETK11
168000150511     C* 2 = F12
168100160728    >C     RETK12        WHENEQ    '2'
168200150511     C* se il pgm di manutenzione ha prima una guida, è necessario anche
168300160728     c* questo test, altrimenti prevale il retK12 e il sfl non viene
168400150511     C* ricaricato
168500160728    >C     OPRK12        ANDNE     '1'
168600150511     C                   MOVE      *ON           $ESCI                          sospese READC
168700150511     C*
168800150511     C                   ENDSL
168900150511     C*
169000150511     C* operazione eseguite dal pgm chiamato
169100150511     C*
169200150511     C                   SELECT
169300150511     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
169400160728    >C     OPRK12        WHENEQ    '1'
169500160728    >C                   MOVE      OPRK12        WOPR                           avverto pgm chiamant
169600150511     C*
169700150511     C* se l'opzione con cui è stato chiamato non è la modifica
169800150525     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
169900160728    >C     OPZK12        IFNE      '02'
170000150511     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
170100150511     C* se l'opzione con cui è stato chiamato è la modifica
170200150525     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
170300150511     C                   ELSE
170400150511     C                   EXSR      AGGS1
170500150511     C                   ENDIF
170600150511     C*
170700150511     C                   ENDSL
170800150511     C*
170900150511     C* funzione non eseguibile per errore :
171000150511     C*
171100150511     C                   SELECT
171200150511     C* 1 = funzione richiamata chiusa in errore
171300160728    >C     ERRK12        WHENEQ    '1'
171400150511     C                   MOVE      *ON           $ESCI                          sospese READC
171500150511     C                   SETON                                        9399      emiss.SFLMSGID
171600150511     C* 2 = funzione richiamata senza la dovuta autorizzazione
171700160728    >C     ERRK12        WHENEQ    '2'
171800150511     C                   MOVE      *ON           $ESCI                          sospese READC
171900150511     C                   SETON                                        9499      emiss.SFLMSGID
172000150511     C*
172100150511     C                   ENDSL
172200150511     C*
172300150511     C                   ENDSR
172400150511     C/EJECT
172500150511     C************************************************************
172600150511     C* AGGIORNAMENTO RIGA LISTA
172700150511     C************************************************************
172800150511     C     AGGS1         BEGSR
172900150511     C*
173000150511     C* riempio i campi del sfl che sono stati ripassati dal pgm
173100150511     C* di manutenzione dopo una opzione 02
173200150511     C*
173300160728     C                   MoveL     KccK12        S1Kcc
173301160728     C                   MoveL     KscK12        S1Ksc
173302150512     C*
173303160728     C*
173304160728     C                   Clear                   S1KscD
173305160728     C                   Eval      WKcc        = S1Kcc
173306160728     C                   Eval      WKsc        = S1Ksc
173307160728     C                   EXSR      CALLMVC002
173308160728     C                   if        Esito      <> *On
173309160728     C                   MoveL     DesBrev002    S1KSCD
173310160728     C                   endif
173800150511     C*
173900160728     C                   if        DtIniK12   <> *Loval
174000160728     C     *JobRun       MoveL     DtIniK12      S1DtIni
174100150512     C                   else
174200150512     C                   Z-Add     0             S1DtIni
174300150512     C                   endif
174301150512     C*
174302160729     C                   if        DtFinK12   <> *Hival
174500160728     C     *JobRun       Movel     DtFinK12      S1DtFin
174600150512     C                   else
174700150511     C                   Z-Add     0             S1DtFin
174800150512     C                   endif
174801150512     C*
174802160729     C                   MoveL     TributoK12    S1Tributo
174803160729     C                   MoveL     TriProgK12    S1TriProg
176100150511     C*
176200150511     C                   ENDSR
176300150511     C/EJECT
176400150511     C************************************************************
176500150511     C* GESTIONE VALORI ASSUNTI
176600150511     C************************************************************
176700150511     C     GESD2         BEGSR
176800150511     C*
176900150511   B1C     $INZD2        IFEQ      *ON
177000150511     C                   EXSR      INZD2
177100150511     C                   MOVE      *OFF          $INZD2
177200150511   E1C                   ENDIF
177300150511     C*
177400150511   B1C     $LASTV        IFNE      'D2'
177500150511     C                   EXSR      GESPIE
177600150511   B1C     $LASTV        IFNE      *HIVAL
177700150511     C                   MOVE      $LASTV        $LASTG
177800150511     C                   ENDIF
177900150511     C                   MOVE      'D2'          $LASTV
178000150511   E1C                   ENDIF
178100150511     C*
178200150511     C* se esistono errori sulla videata
178300150511     C* emetto la write del formato a indicatori spenti per vedere
178400150511     C* le eventuali decodifiche
178500150511   B1C     *IN99         IFEQ      *ON
178600150511     C                   MOVEA     *IN           WIN
178700150511     C                   MOVE      *ALL'0'       IN5098           49
178800150511     C                   MOVEA     IN5098        *IN(50)
178900150511     C                   WRITE     FMTD2
179000150511     C                   MOVEA     WIN           *IN
179100150511   E1C                   ENDIF
179200150511     C*
179300150511     C                   EXFMT     FMTD2
179400150511     C*
179500150511     C* azzero la posizione del cursore
179600150511     C                   Z-ADD     0             H1RIGA
179700150511     C                   Z-ADD     0             H1COLO
179800150511     C*
179900150511     C* CONTROLLO ABILITAZIONE TASTI
180000150511     C* INPUT
180100150511     C* $FC3 = Abilitazioni videata D2
180200150511     C* OUTPUT
180300150511     C* *IN97 + *IN99 Errori
180400150511     C                   MOVEA     $FC3          $FC
180500150511     C                   EXSR      CTRKEY
180600150511     C*
180700150511   B1C     *IN99         IFEQ      *OFF
180800150511     C*
180900150511   B2C                   SELECT
181000150511     C* F3=Fine
181100150511     C     $TASTO        WHENEQ    F03
181200150511     C                   EXSR      F03D2
181300150511     C* F12=Ritorno
181400150511     C     $TASTO        WHENEQ    F12
181500150511     C                   EXSR      F12D2
181600150511     C* F24=Altri tasti
181700150511     C     $TASTO        WHENEQ    F24
181800150511     C                   EXSR      F24D2
181900150511     C*
182000150511     C                   OTHER
182100150511     C                   EXSR      CTRD2
182200150511   B3C     *IN99         IFEQ      *OFF
182300150511     C                   MOVE      $LASTG        $GEST
182400150511   B4C                   SELECT
182500150511     C     $LASTG        WHENEQ    'S1'
182600150511     C* se è stato modificato l'ordinamento di default, ricarico la lista
182700150511   B5C     $ORD          IFNE      D2ORD
182800150511     C                   MOVE      *ON           $INZS1
182900150511     C                   MOVE      *BLANK        W1POS
183000150511   E5C                   ENDIF
183100150511     C     $LASTG        WHENEQ    'D1'
183200150511     C* per ora niente
183300150511   E4C                   ENDSL
183400150511     C* definisco i nuovi valori di default
183500150511     C                   Z-ADD     D2LOP         $LOP
183600150511     C                   Z-ADD     D2ORD         $ORD
183700150511     C*
183800150511     C* aggiorno valori assunti
183900150511     C* se richiesto il salvataggio
184000150511   B4C     D2BSV         IFEQ      1
184100150511     C                   EXSR      AGGMPE
184200150511   E4C                   ENDIF
184300150511     C*
184400150511   E3C                   ENDIF
184500150511     C*
184600150511   E2C                   ENDSL
184700150511     C*
184800150511   E1C                   ENDIF
184900150511     C*
185000150511     C                   ENDSR
185100150511     C/EJECT
185200150511     C************************************************************
185300150511     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
185400150511     C************************************************************
185500150511     C     INZD2         BEGSR
185600150511     C*
185700150511     C                   CLEAR                   FMTD2
185800150511     C                   EVAL      D2BSVA = Normale
185900150511     C* se sono entrato con chiave univoca
186000150511     C* impedisco il salvataggio valori assunti
186100150511     C     $Unique       IFEQ      *ON
186200150511     C                   EVAL      D2BSVA = Protetto
186300150511     C                   ENDIF
186400150511     C                   MOVEL     $LOP          D2LOP
186500150511     C                   MOVE      $ORD          D2ORD
186600150511     C*
186700150511     C                   ENDSR
186800150511     C/EJECT
186900150511     C************************************************************
187000150511     C* GESTIONE F03 VIDEO D2
187100150511     C************************************************************
187200150511     C     F03D2         BEGSR
187300150511     C*
187400150511     C                   MOVE      *ON           $FINE
187500150511     C* fine programma
187600150511     C                   EXSR      ENDPGM
187700150511     C*
187800150511     C                   ENDSR
187900150511     C/EJECT
188000150511     C************************************************************
188100150511     C* GESTIONE F12 VIDEO D2
188200150511     C************************************************************
188300150511     C     F12D2         BEGSR
188400150511     C*
188500150511     C                   MOVE      $LASTG        $GEST
188600150511     C*
188700150511     C                   ENDSR
188800150511     C/EJECT
188900150511     C************************************************************
189000150511     C* GESTIONE F24 VIDEO D2
189100150511     C************************************************************
189200150511     C     F24D2         BEGSR
189300150511     C*
189400150511     C                   MOVEA     $FX3          $FX
189500150511     C                   MOVEA     $FC3          $FC
189600150511     C                   MOVEA     $FR3          $FR
189700150511     C                   Z-ADD     $ULKD2        $ULK
189800150511     C                   Z-ADD     $D2L01        $L01
189900150511     C                   Z-ADD     $D2L02        $L02
190000150511     C                   EXSR      RIEKEY
190100150511     C                   Z-ADD     $ULK          $ULKD2            3 0
190200150511     C                   MOVEA     $FC           $FC3
190300150511     C                   MOVEL     $KEY1         Z3KE1
190400150512     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
190500150511     C                   WRITE     FMTZ3
190600150511     C*
190700150511     C                   ENDSR
190800150511     C/EJECT
190900150511     C************************************************************
191000150511     C* CONTROLLO VIDEATA VALORI ASSUNTI
191100150511     C************************************************************
191200150511     C     CTRD2         BEGSR
191300150511     C*
191400150511     C                   SETOFF                                       99
191500150511     C*
191600150511     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
191700150511     C     D2LOP         IFEQ      0
191800150511     C                   SETON                                          5099
191900150511     C                   ENDIF
192000150511     C*
192100150511     C* OBBLIGATORIO VALORE PER ORDINAMENTO
192200150511     C     D2ORD         IFEQ      0
192300150511     C                   SETON                                          5199
192400150511     C                   ENDIF
192500150511     C*
192600150511     C                   ENDSR
192700150511     C/EJECT
192800150511     C************************************************************
192900150511     C* CONTROLLO ABILITAZIONE TASTI
193000150511     C* INPUT
193100150511     C* $TASTO =Tasto funzione premuto
193200150511     C* $FC = Tasti abilitati
193300150511     C* OUTPUT
193400150511     C* IN99 = ERrore
193500150511     C************************************************************
193600150511     C     CTRKEY        BEGSR
193700150511     C*
193800150511     C                   SETOFF                                       99
193900150511     C*
194000150511     C                   SELECT
194100150511     C     $TASTO        WHENEQ    F01
194200150511     C                   z-add     01            posfun
194300150511     C     $FC(1)        IFEQ      '0'
194400150511     C                   SETON                                        9997
194500150511     C                   ENDIF
194600150511     C     $TASTO        WHENEQ    F02
194700150511     C                   z-add     02            posfun
194800150511     C     $FC(2)        IFEQ      '0'
194900150511     C                   SETON                                        9997
195000150511     C                   ENDIF
195100150511     C     $TASTO        WHENEQ    F03
195200150511     C                   z-add     03            posfun
195300150511     C     $FC(3)        IFEQ      '0'
195400150511     C                   SETON                                        9997
195500150511     C                   ENDIF
195600150511     C     $TASTO        WHENEQ    F04
195700150511     C                   z-add     04            posfun
195800150511     C     $FC(4)        IFEQ      '0'
195900150511     C                   SETON                                        9997
196000150511     C                   ENDIF
196100150511     C     $TASTO        WHENEQ    F05
196200150511     C                   z-add     05            posfun
196300150511     C     $FC(5)        IFEQ      '0'
196400150511     C                   SETON                                        9997
196500150511     C                   ENDIF
196600150511     C     $TASTO        WHENEQ    F06
196700150511     C                   z-add     06            posfun
196800150511     C     $FC(6)        IFEQ      '0'
196900150511     C                   SETON                                        9997
197000150511     C                   ENDIF
197100150511     C     $TASTO        WHENEQ    F07
197200150511     C                   z-add     07            posfun
197300150511     C     $FC(7)        IFEQ      '0'
197400150511     C                   SETON                                        9997
197500150511     C                   ENDIF
197600150511     C     $TASTO        WHENEQ    F08
197700150511     C                   z-add     08            posfun
197800150511     C     $FC(8)        IFEQ      '0'
197900150511     C                   SETON                                        9997
198000150511     C                   ENDIF
198100150511     C     $TASTO        WHENEQ    F09
198200150511     C                   z-add     09            posfun
198300150511     C     $FC(09)       IFEQ      '0'
198400150511     C                   SETON                                        9997
198500150511     C                   ENDIF
198600150511     C     $TASTO        WHENEQ    F10
198700150511     C                   z-add     10            posfun
198800150511     C     $FC(10)       IFEQ      '0'
198900150511     C                   SETON                                        9997
199000150511     C                   ENDIF
199100150511     C     $TASTO        WHENEQ    F11
199200150511     C                   z-add     11            posfun
199300150511     C     $FC(11)       IFEQ      '0'
199400150511     C                   SETON                                        9997
199500150511     C                   ENDIF
199600150511     C     $TASTO        WHENEQ    F12
199700150511     C                   z-add     12            posfun
199800150511     C     $FC(12)       IFEQ      '0'
199900150511     C                   SETON                                        9997
200000150511     C                   ENDIF
200100150511     C     $TASTO        WHENEQ    F13
200200150511     C                   z-add     13            posfun
200300150511     C     $FC(13)       IFEQ      '0'
200400150511     C                   SETON                                        9997
200500150511     C                   ENDIF
200600150511     C     $TASTO        WHENEQ    F14
200700150511     C                   z-add     14            posfun
200800150511     C     $FC(14)       IFEQ      '0'
200900150511     C                   SETON                                        9997
201000150511     C                   ENDIF
201100150511     C     $TASTO        WHENEQ    F15
201200150511     C                   z-add     15            posfun
201300150511     C     $FC(15)       IFEQ      '0'
201400150511     C                   SETON                                        9997
201500150511     C                   ENDIF
201600150511     C     $TASTO        WHENEQ    F16
201700150511     C                   z-add     16            posfun
201800150511     C     $FC(16)       IFEQ      '0'
201900150511     C                   SETON                                        9997
202000150511     C                   ENDIF
202100150511     C     $TASTO        WHENEQ    F17
202200150511     C                   z-add     17            posfun
202300150511     C     $FC(17)       IFEQ      '0'
202400150511     C                   SETON                                        9997
202500150511     C                   ENDIF
202600150511     C     $TASTO        WHENEQ    F18
202700150511     C                   z-add     18            posfun
202800150511     C     $FC(18)       IFEQ      '0'
202900150511     C                   SETON                                        9997
203000150511     C                   ENDIF
203100150511     C     $TASTO        WHENEQ    F19
203200150511     C                   z-add     19            posfun
203300150511     C     $FC(19)       IFEQ      '0'
203400150511     C                   SETON                                        9997
203500150511     C                   ENDIF
203600150511     C     $TASTO        WHENEQ    F20
203700150511     C                   z-add     20            posfun
203800150511     C     $FC(20)       IFEQ      '0'
203900150511     C                   SETON                                        9997
204000150511     C                   ENDIF
204100150511     C     $TASTO        WHENEQ    F21
204200150511     C                   z-add     21            posfun
204300150511     C     $FC(21)       IFEQ      '0'
204400150511     C                   SETON                                        9997
204500150511     C                   ENDIF
204600150511     C     $TASTO        WHENEQ    F22
204700150511     C                   z-add     22            posfun
204800150511     C     $FC(22)       IFEQ      '0'
204900150511     C                   SETON                                        9997
205000150511     C                   ENDIF
205100150511     C     $TASTO        WHENEQ    F23
205200150511     C                   z-add     23            posfun
205300150511     C     $FC(23)       IFEQ      '0'
205400150511     C                   SETON                                        9997
205500150511     C                   ENDIF
205600150511     C     $TASTO        WHENEQ    F24
205700150511     C                   z-add     24            posfun
205800150511     C     $FC(24)       IFEQ      '0'
205900150511     C                   SETON                                        9997
206000150511     C                   ENDIF
206100150511     C                   ENDSL
206200150511     C*
206300150511     C                   ENDSR
206400150511     C/EJECT
206500150511     C************************************************************
206600150511     C* AGGIORNAMENTO VALORI ASSUNTI
206700150511     C************************************************************
206800150511     C     AGGMPE        BEGSR
206900150511     C*
207000150511     C* preparo i parametri di chiamata al driver
207100150511     C                   CLEAR                   XMPEDS
207200150511     C                   EVAL      TpChiamata = '03'
207300150511     C                   EVAL      NChiavi = 5
207400150511     C                   EVAL      VLogica = 2
207500150511     C*
207600150511     C* preparo le chiavi per la chiamata al driver
207700150511     C                   CLEAR                   XMPEDS1
207800150511     C                   MOVEL     XSCSOC        DSMPESOC
207900150511     C                   MOVEL     DSPGM         DSMPEPGM
208000150511     C                   MOVEL     KNMUS         DSMPEPRF
208100150511     C*
208200150511     C* Si memorizzano i dati di default a seconda delle modalità con
208300150511     C* cui il pgm è usato
208400150511     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
208500150511     C* MPETIP = 'PV'  pgm richiamato da menù
208600150511     C* MPETIP = 'PB'  x lancio Batch
208700150511     C*
208800150511     C                   SELECT
208900160728    >C     OPZK11        WHENEQ    '01'
209000150511     C                   MOVEL     'PX'          DSMPETIP
209100150511     C                   OTHER
209200150511     C                   MOVEL     'PV'          DSMPETIP
209300150511     C                   ENDSL
209400150511     C* dati
209500150511     C                   MOVEL     DSMPE         DSMPEPAR
209600150511     C*
209700150511     C                   CALLB     'XMPE'
209800150511     C                   PARM                    XMPEDS
209900150511     C                   PARM                    XMPEDS1
210000150511     C*
210100150511     C                   ENDSR
210200150511     C/EJECT
210300150511     C************************************************************
210400150511     C* OPERAZIONI INIZIALI
210500150511     C************************************************************
210600150511     C     *INZSR        BEGSR
210700150511     C*
210800150511     C* Reperimento parametri
210900150511     C*
211000150511     C     *ENTRY        PLIST
211100150511     C                   PARM                    KPJBA
211200150511     C                   PARM                    DSAUT
211300150511     C*
211400150511     C* testo se sono stati passati i dati di autorizzazione
211500150511     C*
211600150511     C     PARMS         IFLT      2
211700150511     C                   MOVE      *BLANK        AUTDS
211800150511     C                   ELSE
211900150511     C                   MOVEL     DSAUT         AUTDS
212000150511     C                   ENDIF
212100150511     C*
212200150511     C* Reperimento dati società
212300150511     C*
212400150511     C                   MOVEL     'SOC001'      TIPXSC
212500150511     C                   MOVEL     *BLANK        SOCXSC
212600150511     C                   EXSR      REPSOC
212700150511     C*
212800150511     C* Controlli dati società
212900150511     C*
213000150511     C     RTNXSC        IFNE      '1'
213100150511     C                   MOVEL     XSOCDS        SOC001
213200150511     C                   ELSE
213300150511     C                   MOVE      *ON           $FINE
213400160728    >C                   MOVE      '1'           ErrK11
213500150511     C                   EXSR      ENDPGM
213600150511     C                   ENDIF
213700150511     C*
213800150511     C* reperisco le dimensioni delle variabili che conterranno i tasti
213900150511     C                   EVAL      $D1L01 = %Size(Z1Ke1)
214000150511     C                   EVAL      $D1L02 = %Size(Z1Ke2)
214100150511     C                   EVAL      $S1L01 = %Size(Z2Ke1)
214200150511     C                   EVAL      $S1L02 = %Size(Z2Ke2)
214300150511     C                   EVAL      $C1Op1 = %Size(C1Opz1)
214400150511     C                   EVAL      $C1Op2 = %Size(C1Opz2)
214500150511     C                   EVAL      $D2L01 = %Size(Z3Ke1)
214600150511     C                   EVAL      $D2L02 = 0
214700150511     C*
214800150511     C* Reperimento opzioni
215000150511     C                   EXSR      REPOPZ
215100150511     C*
215200150511     C* Reperimento tasti di funzione
215400150511     C                   EXSR      REPKEY
215401150512     C*
215500150525     C* Chiamata a routine per il reperimento delle opzioni
215600150511     C* personalizzate
215700150511     C                   CAllb     'X46USREXT'
215800150511     C                   PARM                    $OP1
215900150511     C                   PARM                    $TX1
216000150511     C                   PARM                    $FL1
216100150511     C                   PARM                    $PGO1
216200150511     C                   PARM      $dimOp        NumOpz
216300150511     C                   PARM                    $FX2
216400150511     C                   PARM                    $FC2
216500150511     C                   PARM                    $PGF2
216600150511     C                   PARM                    DsPgm
216700150511     C                   PARM                    xsccut
216800150511     C                   PARM                    xscgrp
216900150511     C*
217000150511     C                   Z-add     1             OPZLEN
217100150511     C     *blank        Lookup    $TX1(opzlen)                           21
217200150511     C                   if        *in21 = *on
217300150511     C                   sub       1             OPZLEN
217400150511     C                   else
217500150511     C                   Z-add     $DimOp        OPZLEN
217600150511     C                   endif
217700150511     C*
217800150511     C                   ENDSR
217900150511     C/EJECT
218000150511     C************************************************************
218100150511     C* INIZIALIZZAZIONE VARIABILI
218200150511     C************************************************************
218300150511     C     INZVAR        BEGSR
218400150511     C*
218500150511     C* Dati da PGM chiamante
218600150511     C*
218700160728    >C                   MOVEL     KPJBU         SDGSK11DS
218800150511     C*
218900150511     C* Se società diversa da quella impostata nella *inzsr, rieseguo
219000150511     C* reperimento dati società
219100160728    >C                   IF        SocietaK11 <> xscsoc  and
219200160728    >C                             OpzK11 <>*blank
219300150511     C                   MOVEL     'SOC001'      TIPXSC
219400160728    >C                   MOVEL     SocietaK11    SOCXSC
219500150511     C                   EXSR      REPSOC
219600150511     C     RTNXSC        IFNE      '1'
219700150511     C                   MOVEL     XSOCDS        SOC001
219800150511     C                   ELSE
219900150511     C                   MOVE      *ON           $FINE
220000160728    >C                   MOVE      '1'           ErrK11
220100150511     C                   EXSR      ENDPGM
220200150511     C                   ENDIF
220300150511     C                   ENDIF
220400150511     C*
220500150511     C* Reperimento valori default utente
220600150511     C*
220700150511     C                   EXSR      REPDFT
220800150511     C*
220900150511     C* Definizione delle opzioni utilizzabili a pgm
221000150511     C*
221100150511     C                   EXSR      DEFOPZ
221200150511     C*
221300150511     C* Variabili per gestione videate
221400150511     C*
221500150511     C                   MOVE      *OFF          $FINE                          fine pgm
221600150511     C                   MOVE      *OFF          $INZD1                         inizializzione D1
221700150511     C                   MOVE      *OFF          $INZS1                         inizializzione S1
221800150511     C                   MOVE      *OFF          $INZD2                         inizializzione D2
221900150511     C                   MOVE      *OFF          $EndFile                       memo fine file
222000150511     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
222100150511     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
222200150511     C                   MOVE      *HIVAL        $LASTV
222300150511     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
222400150511     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
222500150511     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
222600150511     C* azzero la posizione del cursore
222700150511     C                   Z-ADD     0             H1RIGA
222800150511     C                   Z-ADD     0             H1COLO
222900150511     C*
223000150511     C                   MOVE      *ON           $INZD1
223100150511     C                   MOVE      *ON           $INZS1
223200150511     C*
223300150511     C* se pgm richiamato con chiave univoca impostata,
223400150511     C* forzo la emissione della lista e imposto l'ordinamento
223500160728    >C     OpzK11        IFNE      *BLANK
223600160728    >C**** UK1101        IFNE      *BLANK
223700150511     C****               MOVE      *ON           $Unique
223800150511     C****               Z-ADD     2             $LOP
223900150511     C****               Z-ADD     99            $ORD
224000150511     C****               ELSE
224100150511     C****               MOVE      *OFF          $Unique
224200150511     C****               ENDIF
224300150511     C                   ENDIF
224400150511     C*
224500150511     C* Variabili appoggio
224600150511     C*
224700150511     C                   Z-ADD     SFLPAG        WPAG
224800150511     C                   MOVEL     *OFF          WOPR
224900150511     C                   CLEAR                   W1POS
225000150511     C*
225100150511     C* definisco qual'è la prima videata
225200150511     C* (se la prima videata risulta la lista, devo comunque caricare
225300150511     C*  la videata di parzializzazione e gestire la definizione
225400150511     C*  dell'ordinamento, perchè, dai default oppure se sono stati
225500150511     C*  passati dei dati, può variare il caricamento)
225600150511     C                   SELECT
225700150511     C     $LOP          WHENEQ    1
225800150511     C                   MOVE      'D1'          $GEST
225900150511     C     $LOP          WHENEQ    2
226000150511     C                   MOVE      'S1'          $GEST
226100150511     C                   EXSR      INZD1
226200150511     C                   MOVE      *OFF          $INZD1
226300150511     C                   EXSR      DEFORD
226400150511     C                   ENDSL
226500150511     C*
226600150511     C* Valorizzazione campi univoci testate
226700150511     C*
226800150511     C                   CLEAR                   FMTT1
226900150511     C                   MOVEL     KNSIF         NOMSIF
227000150511     C                   MOVEL     XSCDSI        NOMDIT
227001150526     C                   MOVEL     KCDAZ         NOMAZN
227100150512     C****               MOVEL     DSPGM         NOMPGM
227200150511     C* recupero il sottotitolo (se c'è)
227300150511     C                   MOVEL     *BLANK        T1TIT
227400150511     C     SOTTIT        IFNE      *BLANK
227500150511     C                   MOVEL     SOTTIT        IDMSG
227600150511     C                   CALLB     'XRTVM'
227700150511     C                   PARM                    IDMSG            27
227800150511     C                   PARM                    TXTMSG          100
227900150511     C                   PARM                    ERRMSG            1
228000150511     C                   PARM      *ON           CTRMSG            1            centratura
228100150511     C                   PARM      30            LENMSG            3 0          lun output.
228200150511     C     ERRMSG        IFNE      *ON
228300150511     C                   MOVEL     TXTMSG        T1TIT
228400150511     C                   ELSE
228500150511     C                   MOVEL     *ALL'?'       T1TIT
228600150511     C                   ENDIF
228700150511     C                   ENDIF
230100150511     C*
230200150511     C                   ENDSR
230300150511     C/EJECT
230400150511     C************************************************************
230500150511     C* REPERIMENTO DATI SOCIETA'
230600150511     C************************************************************
230700150511     C     REPSOC        BEGSR
230800150511     C*
230801150512     C                   CALL      'SDGSOCDR'
231000150511     C                   PARM                    TIPXSC            6
231100150511     C                   PARM                    SOCXSC            3
231200150511     C                   PARM                    CDSXSC            9 0
231300150511     C                   PARM                    MODXSC            3
231400150511     C                   PARM      *BLANKS       RTNXSC            1
231500150511     C                   PARM                    XSOCDS
231600150511     C                   PARM                    KPJBA
231700150511     C*
231800150511     C                   ENDSR
231900150511     C/EJECT
232419150512     C************************************************************
232420150512     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
232421150512     C************************************************************
232422150512     C     REPDFT        BEGSR
232423150512     C*
232500150511     C* preparo i parametri di chiamata al driver
232600150511     C                   CLEAR                   XMPEDS
232700150511     C                   EVAL      TpChiamata = '01'
232800150511     C                   EVAL      NChiavi = 5
232900150511     C                   EVAL      VLogica = 2
233000150511     C*
233100150511     C* preparo le chiavi per la chiamata al driver
233200150511     C                   CLEAR                   XMPEDS1
233300150511     C                   MOVEL     XSCSOC        DSMPESOC
233400150511     C                   MOVEL     DSPGM         DSMPEPGM
233500150511     C                   MOVEL     KNMUS         DSMPEPRF
233600150511     C* Si reperiscono i dati di default a seconda delle modalità con
233700150511     C* cui il pgm è usato
233800150511     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
233900150511     C* MPETIP = 'PV'  pgm richiamato da menù
234000150511     C* MPETIP = 'PB'  x lancio Batch
234100150511     C                   SELECT
234200160728    >C     OPZK11        WHENEQ    '01'
234300150511     C                   MOVEL     'PX'          DSMPETIP
234400150511     C                   OTHER
234500150511     C                   MOVEL     'PV'          DSMPETIP
234600150511     C                   ENDSL
234700150511     C*
234800150511     C                   CALLB     'XMPE'
234900150511     C                   PARM                    XMPEDS
235000150511     C                   PARM                    XMPEDS1
235100150511     C*
235200150511     C     Risultato     IFEQ      '0'
235300150511     C                   MOVEL     DSMPEPAR      DSMPE
235400150511     C* nel caso sia la prima volta che l'utente apre questa anagrafica
235500150511     C* i suoi valori di default vengono decisi a pgm
235600150511     C                   ELSE
235700150511     C                   Z-ADD     1             $LOP
235800150511     C                   Z-ADD     1             $ORD
235900150511     C                   ENDIF
236000150511     C*
236100150511     C                   ENDSR
236200150511     C/EJECT
236300150511     C************************************************************
236400150511     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
236500150511     C************************************************************
236600150511     C     DEFOPZ        BEGSR
236700150511     C*
236800150525     C* Questa routine deve valorizzare la schiera $FL1
236900150511     C* con S o N a seconda della utilizzabilità o meno
237000150511     C* dell'opzione correlata
237100150511     C*
237200150511     C* Inizialmente si considerano tutte le opzioni come valide
237300150511     C     1             DO        $DIMOP        X
237400150511     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
237500150511     C                   If        $TX1(X)  <> *Blank
237600150511     C                   MOVE      'S'           $FL1(X)
237700150511     C                   Else
237800150511     C                   Leave
237900150511     C                   EndIf
238000150511     C                   ENDDO
238100150511     C*
238200150525     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
238300150511     C* con 1 o 0 a seconda della utilizzabilità o meno
238400150511     C* del tasto funzione correlato
238500150511     C*
238600150525     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
238700150511     C* di compilazione
238800150511     C                   RESET                   $FC1
238900150511     C                   RESET                   $FR1
239000150511     C                   RESET                   $FC2
239100150511     C                   RESET                   $FR2
239200150511     C                   RESET                   $FC3
239300150511     C                   RESET                   $FR3
239400150511     C*
239500150511     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
239600150511     C* invalido alcune opzioni
239700150511     C                   SELECT
239800150511     C* Se pgm richiamato in ricerca disattivo :
239900160728    >C     OPZK11        WHENEQ    '01'
240000150511     C* F3=Fine
240100150511     C                   Z-ADD     3             X
240200150511     C                   MOVE      '0'           $FC1(X)
240300150511     C                   MOVE      '0'           $FC2(X)
240400150511     C                   MOVE      '0'           $FC3(X)
240500150511     C*
240600150511     C* Se pgm richiamato in interrogazione disattivo :
240700160728    >C     OPZK11        WHENEQ    '05'
240800150511     C* F10=Immissione
240900150511     C                   Z-ADD     10            X
241000150511     C                   MOVE      '0'           $FC1(X)
241100150511     C                   MOVE      '0'           $FC2(X)
241200150511     C* 1=Selezione
241300150511     C                   Z-ADD     1             X
241400150511     C     01            LOOKUP    $OP1(X)                                21
241500150511     C     *IN21         IFEQ      *ON
241600150511     C                   MOVE      'N'           $FL1(X)
241700150511     C                   ENDIF
241800150511     C* 2=Modifica
241900150511     C                   Z-ADD     1             X
242000150511     C     02            LOOKUP    $OP1(X)                                21
242100150511     C     *IN21         IFEQ      *ON
242200150511     C                   MOVE      'N'           $FL1(X)
242300150511     C                   ENDIF
242400150511     C* 3=Copia
242500150511     C                   Z-ADD     1             X
242600150511     C     03            LOOKUP    $OP1(X)                                21
242700150511     C     *IN21         IFEQ      *ON
242800150511     C                   MOVE      'N'           $FL1(X)
242900150511     C                   ENDIF
243000150511     C* 4=Annullamento
243100150511     C                   Z-ADD     1             X
243200150511     C     04            LOOKUP    $OP1(X)                                21
243300150511     C     *IN21         IFEQ      *ON
243400150511     C                   MOVE      'N'           $FL1(X)
243500150511     C                   ENDIF
243600150511     C*
243700160728    >C     OPZK11        WHENEQ    *BLANKS
243800150511     C* Se pgm non richiamato, disattivo subito :
243900150511     C* F8=Selezione seguente
244000150511     C                   Z-ADD     8             X
244100150511     C                   MOVE      '0'           $FC1(X)
244200150511     C                   MOVE      '0'           $FC2(X)
244300150511     C* 1=Selezione
244400150511     C                   Z-ADD     1             X
244500150511     C     01            LOOKUP    $OP1(X)                                21
244600150511     C     *IN21         IFEQ      *ON
244700150511     C                   MOVE      'N'           $FL1(X)
244800150511     C                   ENDIF
244900150511     C*
245000150511     C* In tutti gli altri casi, disattivo subito :
245100150511     C                   OTHER
245200150511     C* 1=Selezione
245300150511     C                   Z-ADD     1             X
245400150511     C     01            LOOKUP    $OP1(X)                                21
245500150511     C     *IN21         IFEQ      *ON
245600150511     C                   MOVE      'N'           $FL1(X)
245700150511     C                   ENDIF
245800150511     C*
245900150511     C                   ENDSL
246000150511     C*
246100150511     C                   ENDSR
246200150511     C/EJECT
246300150511     C************************************************************
246400150511     C* REPERIMENTO OPZIONI
246500150511     C************************************************************
246600150511     C     REPOPZ        BEGSR
246700150511     C*
246800150511     C     1             DO        $DIMOP        X
246900150511     C*
247000150511     C     $MS1(X)       IFNE      *BLANK
247100150511     C*
247200150511     C                   MOVEL     $MS1(X)       IDMSG
247300150511     C                   CALLB     'XRTVM'
247400150511     C                   PARM                    IDMSG            27
247500150511     C                   PARM                    TXTMSG          100
247600150511     C                   PARM                    ERRMSG            1
247700150511     C*
247800150511     C     ERRMSG        IFNE      *ON
247900150511     C                   MOVEL     TXTMSG        $TX1(X)
248000150511     C                   ELSE
248100150511     C                   MOVEL     *ALL'?'       $TX1(X)
248200150511     C                   ENDIF
248300150511     C*
248400150511     C                   ENDIF
248500150511     C*
248600150511     C                   ENDDO
248700150511     C*
248800150511     C                   ENDSR
248900150511     C/EJECT
249000150511     C************************************************************
249100150511     C* REPERIMENTO TASTI DI FUNZIONE
249200150511     C************************************************************
249300150511     C     REPKEY        BEGSR
249400150511     C                   MOVEA     $FM1          $FM
249500150511     C                   MOVEA     $FC1          $FC
249600150511     C                   EXSR      REPKKK
249700150511     C                   MOVEA     $FX           $FX1
249800150511     C*
249900150511     C                   MOVEA     $FM2          $FM
250000150511     C                   MOVEA     $FC2          $FC
250100150511     C                   EXSR      REPKKK
250200150511     C                   MOVEA     $FX           $FX2
250300150511     C*
250400150511     C                   MOVEA     $FM3          $FM
250500150511     C                   MOVEA     $FC3          $FC
250600150511     C                   EXSR      REPKKK
250700150511     C                   MOVEA     $FX           $FX3
250800150511     C                   ENDSR
250900150511     C/EJECT
251000150511     C************************************************************
251100150511     C* REPERIMENTO TASTI DI FUNZIONE
251200150511     C************************************************************
251300150511     C     REPKKK        BEGSR
251400150511     C*
251500150511     C                   CLEAR                   $FX
251600150511     C     1             DO        24            X
251700150511     C*
251800150511     C     $FM(X)        IFNE      *BLANK
251900150511     C     $Fc(X)        andne     '0'
252000150511     C*
252100150511     C                   MOVEL     $FM(X)        IDMSG
252200150511     C                   CALLB     'XRTVM'
252300150511     C                   PARM                    IDMSG            27
252400150511     C                   PARM                    TXTMSG          100
252500150511     C                   PARM                    ERRMSG            1
252600150511     C*
252700150511     C     ERRMSG        IFNE      *ON
252800150511     C                   MOVEL     TXTMSG        $FX(X)
252900150511     C                   ELSE
253000150511     C                   MOVEL     *ALL' '       $FX(X)
253100150511     C                   ENDIF
253200150511     C*
253300150511     C                   ENDIF
253400150511     C*
253500150511     C                   ENDDO
253600150511     C*
253700150511     C                   ENDSR
253800150511     C/EJECT
253900150511     C************************************************************
254000150511     C* DEFINIZIONE KLIST
254100150511     C************************************************************
254200150511     C     DefKlist      BEGSR
254300150511     C*
254400150511     C* klist
254600150511     C*
254700150511     C                   ENDSR
254800150511     C/EJECT
254900150511** $MS1 / $TX1
255000150511PROMSG    *LIBL     OPZ0001
255100150511PROMSG    *LIBL     OPZ0002
255200150511PROMSG    *LIBL     OPZ0003
255300150511PROMSG    *LIBL     OPZ0004
255400150511PROMSG    *LIBL     OPZ0005
255500150511** $OP1 / $FL1
25560015051101S      <Selezione
25570015051102S      <Modifica
25580015051103S      <Copia
25590015051104S      <Annullamento
25600015051105S      <Visualizzazione
256100150511** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
256200150511PROMSG    *LIBL     KEY0001  01
256300150511PROMSG    *LIBL     KEY0002  02
256400150511PROMSG    *LIBL     KEY0003  03
256500150511PROMSG    *LIBL     KEY0004  04
256600150511PROMSG    *LIBL     KEY0005  05
256700150511PROMSG    *LIBL     KEY0006  06
256800150511PROMSG    *LIBL     KEY0007  07
256900150511PROMSG    *LIBL     KEY0008  08
257000150511PROMSG    *LIBL     KEY0009  09
257100150511PROMSG    *LIBL     KEY0010  10
257200150511PROMSG    *LIBL     KEY0011  11
257300150511PROMSG    *LIBL     KEY0012  12
257400150511PROMSG    *LIBL     KEY0013  13
257500150511PROMSG    *LIBL     KEY0014  14
257600150511PROMSG    *LIBL     KEY0015  15
257700150511PROMSG    *LIBL     KEY0016  16
257701150512SDGMSG    *LIBL     SDK0001  17
257900150511PROMSG    *LIBL     KEY0018  18
258000150511PROMSG    *LIBL     KEY0019  19
258100150511PROMSG    *LIBL     KEY0020  20
258200150511PROMSG    *LIBL     KEY0021  21
258300150511PROMSG    *LIBL     KEY0022  22
258400150511PROMSG    *LIBL     KEY0023  23
258500150511PROMSG    *LIBL     KEY0024  24
258600150511**  ABILITAZIONE D1 : $FC1 / $FR1
2587001505110N  01
2588001505111N  02
2589001505111N  03
2590001505111N  04
2591001505110N  05
2592001505110N  06
2593001505110N  07
2594001505111N  08
2595001505110N  09
2596001505111N  10
2597001505110N  11
2598001505111N  12
2599001505110N  13
2600001505110N  14
2601001505110N  15
2602001505110N  16
2603001505110N  17
2604001505111N  18
2605001505110N  19
2606001505110N  20
2607001505110N  21
2608001505110N  22
2609001505110N  23   Impostare a '1' se si utilizzano opzioni
2610001505111N  24   Lasciare sempre impostata a '1' il valore della riga 24
261100150511** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
261200150511PROMSG    *LIBL     KEY0001  01
261300150511PROMSG    *LIBL     KEY0002  02
261400150511PROMSG    *LIBL     KEY0003  03
261500150511PROMSG    *LIBL     KEY0004  04
261600150511PROMSG    *LIBL     KEY0005  05
261700150511PROMSG    *LIBL     KEY0006  06
261800150511PROMSG    *LIBL     KEY0007  07
261900150511PROMSG    *LIBL     KEY0008  08
262000150511PROMSG    *LIBL     KEY0009  09
262100150511PROMSG    *LIBL     KEY0010  10
262200150511PROMSG    *LIBL     KEY0011  11
262300150511PROMSG    *LIBL     KEY0012  12
262400150511PROMSG    *LIBL     KEY0013  13
262500150511PROMSG    *LIBL     KEY0014  14
262600150511PROMSG    *LIBL     KEY0015  15
262700150511PROMSG    *LIBL     KEY0016  16
262701150512SDGMSG    *LIBL     SDK0001  17
262900150511PROMSG    *LIBL     KEY0018  18
263000150511PROMSG    *LIBL     KEY0019  19
263100150511PROMSG    *LIBL     KEY0020  20
263200150511PROMSG    *LIBL     KEY0021  21
263300150511PROMSG    *LIBL     KEY0022  22
263400150511PROMSG    *LIBL     KEY0023  23
263500150511PROMSG    *LIBL     KEY0024  24
263600150511**  ABILITAZIONE S1 : $FC2 / $FR2
2637001505110N  01
2638001505110N  02
2639001505111N  03
2640001505110N  04
2641001505110N  05
2642001505110N  06
2643001505110N  07
2644001505111N  08
2645001505110N  09
2646001505111N  10
2647001505110N  11
2648001505111N  12
2649001505110N  13
2650001505110N  14
2651001505110N  15
2652001505110N  16
2653001505111N  17
2654001505111N  18
2655001505110N  19
2656001505110N  20
2657001505110N  21
2658001505110N  22
2659001505111N  23   Impostare a '1' se si utilizzano opzioni
2660001505111N  24   Lasciare sempre impostata a '1' il valore della riga 24
266100150511** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
266200150511PROMSG    *LIBL     KEY0001  01
266300150511PROMSG    *LIBL     KEY0002  02
266400150511PROMSG    *LIBL     KEY0003  03
266500150511PROMSG    *LIBL     KEY0004  04
266600150511PROMSG    *LIBL     KEY0005  05
266700150511PROMSG    *LIBL     KEY0006  06
266800150511PROMSG    *LIBL     KEY0007  07
266900150511PROMSG    *LIBL     KEY0008  08
267000150511PROMSG    *LIBL     KEY0009  09
267100150511PROMSG    *LIBL     KEY0010  10
267200150511PROMSG    *LIBL     KEY0011  11
267300150511PROMSG    *LIBL     KEY0012  12
267400150511PROMSG    *LIBL     KEY0013  13
267500150511PROMSG    *LIBL     KEY0014  14
267600150511PROMSG    *LIBL     KEY0015  15
267700150511PROMSG    *LIBL     KEY0016  16
267701150512SDGMSG    *LIBL     SDK0001  17
267900150511PROMSG    *LIBL     KEY0018  18
268000150511PROMSG    *LIBL     KEY0019  19
268100150511PROMSG    *LIBL     KEY0020  20
268200150511PROMSG    *LIBL     KEY0021  21
268300150511PROMSG    *LIBL     KEY0022  22
268400150511PROMSG    *LIBL     KEY0023  23
268500150511PROMSG    *LIBL     KEY0024  24
268600150511**  ABILITAZIONE D3 : $FC3 / $FR3
2687001505110N  01
2688001505110N  02
2689001505111N  03
2690001505110N  04
2691001505110N  05
2692001505110N  06
2693001505110N  07
2694001505110N  08
2695001505110N  09
2696001505110N  10
2697001505110N  11
2698001505111N  12
2699001505110N  13
2700001505110N  14
2701001505110N  15
2702001505110N  16
2703001505110N  17
2704001505110N  18
2705001505110N  19
2706001505110N  20
2707001505110N  21
2708001505110N  22
2709001505110N  23   Impostare a '1' se si utilizzano opzioni
2710001505111N  24   Lasciare sempre impostata a '1' il valore della riga 24
