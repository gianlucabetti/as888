000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200000000     H*PARMS CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400160609      *   Task R218: nuovo oggetto
000500160609      *****************************************************************
000600110825      *         ------------------------------------------------
000700160429      *                          Modello 770
000800160429      *                      Manutenzione Ritenute
000900110825      *         ------------------------------------------------
001000000000      *
001100000000      *
001200000000      *
001300160609      *****************************************************************
001400000000     F*----------------------------------------------------------------
001500000000     F*-------------
001600160603     FANTRI01L  IF   E           K DISK
001700160429     F* Key: Soc/TpReg/STpReg/NrRiga
001800160504     FND77R01L  IF A E           K DISK    RENAME(nd77r000:nd77r001)
001900160503     F                                     INFDS(DS77R)
002000160504     FND77R00F  UF A E             DISK
002100160504     F                                     USROPN
002200160513     F                                     COMMIT
002300000000     F*------------
002400160603     FSDGSK03D  CF   E             WORKSTN
002500160603     F                                     RENAME(SK03D1:FMTD1)
002600160603     F                                     RENAME(SK03S1:FMTS1)
002700160603     F                                     RENAME(SK03C1:FMTC1)
002800160603     F                                     RENAME(SK03T1:FMTT1)
002900160603     F                                     RENAME(SK03Z1:FMTZ1)
003000160603     F                                     RENAME(SK03Z2:FMTZ2)
003100000000     F                                     SFILE(FMTS1:S1NrR)
003200000000     F                                     INFDS(DSFMT)
003300000000     D*----------------------------------------------------*
003400000000     D* dimensione della schiera $MS1, da non cambiare
003500000000     D $DIMOP          C                   CONST(30)
003600000000     D*----------------------------------------------------*
003700000000     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
003800000000     D $ID             S             27    DIM(5) ALT($OZ)
003900000000     D* Schiere generali per la gestione dei tasti di funzione
004000000000     D $FM             S             27    DIM(24)
004100000000     D $FX             S             50    DIM(24)
004200000000     D $FC             S              1    DIM(24)
004300000000     D $FR             S              1    DIM(24)
004400000000     D* Schiere per la gestione dei tasti di funzione
004500000000     D* Video D1
004600000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
004700000000     D $FX1            S             50    DIM(24) ALT($FM1)
004800000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
004900000000     D $FR1            S              1    DIM(24) ALT($FC1)
005000000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005100000000     D* funzione personalizzata S1
005200000000     D $PGF2           S             10    DIM(25)
005300000000     D* Schiere per la gestione dei tasti di funzione
005400000000     D* Video S1
005500000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005600000000     D $FX2            S             50    DIM(24) ALT($FM2)
005700000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
005800000000     D $FR2            S              1    DIM(24) ALT($FC2)
005900000000     D*
006000000000     D $D1L01          S              3  0
006100000000     D $D1L02          S              3  0
006200000000     D $S1L01          S              3  0
006300000000     D $S1L02          S              3  0
006400000000     D $C1Op1          S              3  0
006500000000     D $C1Op2          S              3  0
006600000000     D* Inserire qui la definizione di altre variabili per reperire
006700000000     D* la lunghezza di altre videate che usano un piede diverso da
006800000000     D* STD
006900000000     D*-------------
007000000000     D* Indice della schiera tasti funzione
007100000000     D PosFun          S              2  0
007200000000     D*-------------
007300000000     D* Valori assunti
007400000000     D XMPEDS        E DS                  INZ
007500000000     D XMPEDS1       E DS                  INZ
007600000000     D DSMPE           DS                  INZ
007700000000     D  $LOP                   1      1  0
007800000000     D  $ORD                   2      3  0
007900000000     D*-------------
008000000000     D* Passaggio Parametri
008100000000     D KPJBA         E DS
008200000000     D*-------------
008300000000     D* DS Esterna gestione messaggi
008400000000     D SDGMSGDS      E DS
008500000000     D  STS          E                     EXTFLD(MSGSTS)
008600000000     D                                     DIM(333)
008700000000     D  JBA          E                     EXTFLD(MSGJBA)
008800000000     D                                     DIM(502)
008900000000     D*-------------
009000000000     D* Reperimento Autorizzazioni
009100000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009200000000     D*-------------
009300000000     D* Passaggio Autorizzazioni
009400000000     D DSAUT           DS           512
009500000000     D*-------------
009600000000     D XPAOUT          DS          2000                                         *
009700000000     D*-------------
009800000000     D* Ricerca legende
009900000000     D Ana964DS      E DS
010000000000     D*-------------
010100000000     D* Parametri per ricerca codice ANATB00F
010200000000     D XTABDS        E DS                  inz
010300000000     D*-------------
010400000000     D* Parametri per controllo codice ANATB00F
010500000000     D SDGTABDS      E DS                  INZ
010600000000     D*-------------
010700000000     D* Controlli validità legende
010800000000     D X07ValDS      E DS
010900000000     D*-------------
011000000000     D* Parametri in ricezione
011100160603     D SDGSK03DS     E DS                  INZ
011200000000     D*-------------------------
011300000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
011400000000     D* alla funzione personalizzata
011500160603     D $SaveDS         S                   like(SDGSK03DS)
011600000000     D*-------------
011700000000     D* Maschera dati autorizzazioni
011800000000     D***------DS       E DS
011900000000     D*-------------
012000000000     D DSFMT           DS           512
012100000000     D  $TASTO               369    369
012200000000     D  NRG                  370    370
012300000000     D  NCL                  371    371
012400000000     D  SflNrR               378    379B 0
012500160503     D*-------------
012600160503     D DS77R           DS           512
012700160503     D  Nrr                  397    400B 0
012800000000     D*-------------
012900000000     D* posizione cursore
013000000000     D CURSOR          DS
013100000000     D  RIRI                   1      2B 0 INZ
013200000000     D  R$$                    2      2
013300000000     D  COCO                   3      4B 0 INZ
013400000000     D  C$$                    4      4
013500000000     D*-------------
013600000000     D* DS per UserExit
013700000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
013800000000     D*-------------
013900000000     D* Reperimento nome PGM
014000000000     D STATUS         SDS           333
014100000000     D  DSPGM            *PROC
014200000000     D  PARMS            *PARMS
014300000000     D*-------------
014400000000     D* Dati di ambiente ottenuti da XSOC
014500000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
014600000000     D*-------------
014700000000     D* DS Interna per dati di output di XSOC
014800000000     D XSOCDS          DS          1000
014900000000     D*-------------
015000000000     D* Costanti
015100000000     D* nr.di righe del sfl (SflPag)
015200000000     D SFLPAG          S              4S 0 Inz(12)
015300000000     D*-------------
015400000000     D* Schiera per il controllo delle opzioni
015500000000     D $OPZ            S              5  0 DIM(200) inz(0)
015600000000     D*-------------
015700000000     D* Tasti di funzione
015800000000     D F01             C                   CONST(X'31')
015900000000     D F02             C                   CONST(X'32')
016000000000     D F03             C                   CONST(X'33')
016100000000     D F04             C                   CONST(X'34')
016200000000     D F05             C                   CONST(X'35')
016300000000     D F06             C                   CONST(X'36')
016400000000     D F07             C                   CONST(X'37')
016500000000     D F08             C                   CONST(X'38')
016600000000     D F09             C                   CONST(X'39')
016700000000     D F10             C                   CONST(X'3A')
016800000000     D F11             C                   CONST(X'3B')
016900000000     D F12             C                   CONST(X'3C')
017000000000     D F13             C                   CONST(X'B1')
017100000000     D F14             C                   CONST(X'B2')
017200000000     D F15             C                   CONST(X'B3')
017300000000     D F16             C                   CONST(X'B4')
017400000000     D F17             C                   CONST(X'B5')
017500000000     D F18             C                   CONST(X'B6')
017600000000     D F19             C                   CONST(X'B7')
017700000000     D F20             C                   CONST(X'B8')
017800000000     D F21             C                   CONST(X'B9')
017900000000     D F22             C                   CONST(X'BA')
018000000000     D F23             C                   CONST(X'BB')
018100000000     D F24             C                   CONST(X'BC')
018200000000     D ENTER           C                   CONST(X'F1')
018300000000     D ROLDWN          C                   CONST(X'F4')
018400000000     D ROLLUP          C                   CONST(X'F5')
018500000000     D*-------------
018600000000     D* Sottotitolo di testata
018700000000     D SOTTIT          C                   CONST('PROMSG    *LIBL-
018800000000     D                                          COS0002')
018900000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
019000000000     D                                          SDC0001')
019100000000     D*-------------
019200000000     D* Variabili per gestione videate
019300000000     D $Gest           S              2A
019400000000     D $Fine           S              1A
019500000000     D $InzD1          S              1A
019600000000     D $InzS1          S              1A
019700000000     D $LastV          S              2A
019800000000     D $LastG          S              2A
019900000000     D $RcdOK          S              1A
020000000000     D $EndFile        S              1A
020100000000     D*-------------
020200000000     D* Variabile appoggio per n. record
020300000000     D Win             S             99A
020400000000     D*-------------
020500000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
020600000000     D S1NrR           S                   like(C1Rcd)
020700000000     D WSfl            S                   like(C1Nrr)
020800000000     D WMax            S                   like(C1Rcd)
020900000000     D Resto           S              4  0
021000000000     D Risul           S              4  0
021100000000     D WInzS1          S                   like($InzS1)
021200160603     D WOpr            S                   like(OprK03)
021300000000     D CurR            S              2S 0
021400000000     D CurC            S              2S 0
021500000000     D Wrkpos          S             15S 0
021600000000     D WINKR           S              1A
021700000000     D WExf            S              1A
021800000000     D*-------------
021900000000     D*  Indici
022000000000     D $X              S              4S 0
022100000000     D $Y              S              3S 0
022200000000     D* Indice della schiera per le opzioni personalizzate
022300000000     D PosOpz          S              2  0
022400000000     D* numero max opzioni
022500000000     D NumOpz          S              2  0 Inz($DIMOP)
022600000000     D* numero effettive opzioni
022700000000     D OpzLen          S              2  0
022800160503      *----------------------------------------------------*
022900110830     D*------------
023000000000     D* campi di lavoro
023100000000     D $FineS1         S              1
023200000000     D $FineLista      S              1
023300160503     D $Fine77R        S              1
023400000000     D $AlmenoUna      S              1
023500000000     D $RigaV          S              1
023600000000     D LenStr          S              3  0
023700000000     D LenTit          S              3  0
023800160511     D WDqv            S               D
023900160510     D*
024000160511     D $Progr          S              4S 0
024100160511     D SavNrRiga       S                   Like(H1NrRiga)
024200160510     D SavMac          S                   like(S1Mac)
024300160510     D SavRac          S                   like(S1Rac)
024400160510     D SavRef          S                   like(S1Ref)
024500160510     D SavIus          S                   like(S1Ius)
024600160510     D SavVec          S                   like(S1Vec)
024700160510     D SavCim          S                   like(S1Cim)
024800160510     D SavIro          S                   like(S1Iro)
024900160510     D SavRavvedi      S                   like(S1Ravvedi)
025000160510     D Savnote         S                   like(S1note)
025100160510     D SavTri          S                   like(S1Tri)
025200160510     D SavTeso         S                   like(S1Teso)
025300160510     D SavDqv          s                   like(S1Dqv)
025400160510     D*
025500160503     D                 DS
025600160503     D  $Mese                  1      2  0
025700160503     D  SEC20                  3      4  0
025800160503     D  $Anno                  5      6  0
025900160530     D  $Anno4                 3      6  0
026000160503     D  $Meands                1      6  0
026100160503     D
026200000000     C*----------------------------------------------------*
026300000000     C*                MAIN LINE PROGRAM
026400000000     C*----------------------------------------------------*
026500000000     C*
026600000000     C* inizializzazione variabili
026700000000     C                   EXSR      INZVAR
026800000000     C*
026900000000     C     $FINE         DOWEQ     *OFF
027000000000     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
027100000000     C     $GEST         CASEQ     'S1'          GESS1                          Lista
027200000000     C                   ENDCS
027300000000     C                   ENDDO
027400000000     C* fine programma
027500000000     C                   EXSR      ENDPGM
027600000000     C*
027700000000     C/EJECT
027800000000     C************************************************************
027900000000     C* FINE PROGRAMMA
028000000000     C************************************************************
028100000000     C     ENDPGM        BEGSR
028200000000     C*
028300160603     C                   MOVEL     SDGSK03DS     KPJBU
028400000000     C*
028500000000     C                   SetOn                                            LR
028600000000     C                   RETURN
028700000000     C*
028800000000     C                   ENDSR
028900000000     C/EJECT
029000000000     C************************************************************
029100000000     C* RIEMPIMENTO TASTI FUNZIONE
029200000000     C************************************************************
029300000000     C     RIEKEY        BEGSR
029400000000     C*
029500000000     C* Input :
029600000000     C* - $FX = schiera con tutti i testi disponibili
029700000000     C* - $FC = schiere con i flag di validità associati ai testi
029800000000     C* - $FR = schiere con i flag di REVERSE IMAGE
029900000000     C* - $ULK = ultimo testo caricato
030000000000     C* - $L01 = Lunghezza prima riga
030100000000     C* - $L02 = Lunghezza seconda riga
030200000000     C* Output :
030300000000     C* - $ULK
030400000000     C* - $KEY1= 1° riga di tasti funzione
030500000000     C* - $KEY2= 2° riga di tasti funzione
030600000000     C*
030700000000     C                   CALL      'XKEYMSG'
030800000000     C* input
030900000000     C                   PARM                    $FX
031000000000     C                   PARM                    $FC
031100000000     C                   PARM                    $FR
031200000000     C                   PARM                    $ULK              3 0
031300000000     C* output
031400000000     C                   PARM                    $KEY1            79
031500000000     C                   PARM                    $KEY2            79
031600000000     C* input
031700000000     C                   PARM                    $L01              3 0
031800000000     C                   PARM                    $L02              3 0
031900000000     C*
032000000000     C                   PARM                    $ALLFUNCT       480
032100000000     C*
032200000000     C                   ENDSR
032300000000     C/EJECT
032400000000     C************************************************************
032500000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
032600000000     C************************************************************
032700000000     C     GESPIE        BEGSR
032800000000     C*
032900000000     C                   SELECT
033000000000     C* Parzializzazioni
033100000000     C     $GEST         WHENEQ    'D1'
033200000000     C     $ULKD1        IFEQ      0
033300000000     C* riempimento opzioni
033400000000     C                   EXSR      F24D1
033500000000     C                   ELSE
033600000000     C                   WRITE     FMTZ1
033700000000     C                   ENDIF
033800000000     C* Lista
033900000000     C     $GEST         WHENEQ    'S1'
034000000000     C     $ULKS1        IFEQ      0
034100000000     C                   EXSR      F24S1
034200000000     C                   ELSE
034300000000     C                   WRITE     FMTZ2
034400000000     C                   ENDIF
034500000000      *
034600000000     C                   ENDSL
034700000000     C*
034800000000     C                   ENDSR
034900000000     C/EJECT
035000000000     C************************************************************
035100000000     C* GESTIONE PARZIALIZZAZIONI
035200000000     C************************************************************
035300000000     C     GESD1         BEGSR
035400000000     C* inizializzazione videata
035500000000     C     $INZD1        IFEQ      *ON
035600000000     C                   EXSR      INZD1
035700000000     C                   MOVE      *OFF          $INZD1
035800000000     C                   ENDIF
035900000000     C*
036000000000     C* pulitura sottotitolo
036100000000     C                   MOVEL     *BLANK        D1TIT
036200000000     C***                MOVEL     *BLANK        T1TIT
036300000000     C* emissione piede videata se proveniente da altra
036400000000     C* salvataggio valore formato di provenienza
036500000000     C     $LASTV        IFNE      'D1'
036600000000     C* recupero il sottotitolo (se c'è)
036700000000     C                   Clear                   D1TIT
036800000000     C     SOTTITD1      IFNE      *BLANK
036900000000     C                   MOVEL     SOTTITD1      IDMSG
037000000000     C                   CALLB     'XRTVM'
037100000000     C                   PARM                    IDMSG            27
037200000000     C                   PARM                    TXTMSG          100
037300000000     C                   PARM                    ERRMSG            1
037400000000     C                   PARM      *Off          CTRMSG            1            centratura
037500000000     C                   PARM      15            LENMSG            3 0          lun output.
037600000000     C     ERRMSG        IFNE      *ON
037700000000     C                   MOVEL     TXTMSG        D1TIT
037800000000     C                   ELSE
037900000000     C                   MOVEL     *ALL'?'       D1TIT
038000000000     C                   ENDIF
038100000000     C                   ENDIF
038200000000     C* salvo stato F17 per sapere cosa fare in caso di F12
038300000000     C                   MOVE      *INKR         WINKR
038400000000     C                   WRITE     FMTT1
038500000000     C                   EXSR      GESPIE
038600000000     C                   MOVE      $LASTV        $LASTG
038700000000     C                   MOVE      'D1'          $LASTV
038800000000     C                   ENDIF
038900000000     C*
039000000000     C* se esistono errori sulla videata
039100000000     C* emetto la write del formato a indicatori spenti per vedere
039200000000     C* le eventuali decodifiche
039300000000     C     *IN99         IFEQ      *ON
039400000000     C                   MOVEA     *IN           WIN
039500000000     C                   MOVE      *ALL'0'       IN5098           49
039600000000     C                   MOVEA     IN5098        *IN(50)
039700000000     C                   WRITE     FMTD1
039800000000     C                   MOVEA     WIN           *IN
039900000000     C                   ENDIF
040000000000     C*
040100000000     C                   EXFMT     FMTD1
040200000000     C*
040300000000     C* azzero la posizione del cursore
040400000000     C                   Z-ADD     0             H1RIGA
040500000000     C                   Z-ADD     0             H1COLO
040600000000     C*
040700000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
040800000000     C* INPUT
040900000000     C* $FC1 = Abilitazioni videata D1
041000000000     C* OUTPUT
041100000000     C* *IN97 + *IN99 Errori
041200000000     C                   MOVEA     $FC1          $FC
041300000000     C                   EXSR      CTRKEY
041400000000     C*
041500000000     C     *IN99         IFEQ      *OFF
041600000000     C*
041700000000     C                   SELECT
041800000000     C* F2=Decodifica
041900000000     C     $TASTO        WHENEQ    F02
042000000000     C                   EXSR      F02D1
042100000000     C* F3=Fine
042200000000     C     $TASTO        WHENEQ    F03
042300000000     C                   EXSR      F03D1
042400000000     C* F4=Ricerca
042500000000     C     $TASTO        WHENEQ    F04
042600000000     C                   EXSR      SEARCH
042700000000     C* F10=Immissione
042800000000     C     $TASTO        WHENEQ    F10
042900000000     C                   EXSR      F10D1
043000000000     C* F12=Ritorno
043100000000     C     $TASTO        WHENEQ    F12
043200000000     C                   EXSR      F12D1
043300000000     C* F24=Altri tasti
043400000000     C     $TASTO        WHENEQ    F24
043500000000     C                   EXSR      F24D1
043600000000     C*
043700000000     C                   OTHER
043800000000     C                   EXSR      CTRD1
043900000000     C     *IN99         IFEQ      *OFF
044000000000     C* definisco l'ordinamento
044100000000     C                   EXSR      DEFORD
044200000000     C* richiamo la lista
044300000000     C                   MOVE      'S1'          $GEST
044400000000     C                   MOVE      *ON           $INZS1
044500160603     C                   MOVE      '02'          OpzK03
044600000000     C                   ENDIF
044700000000     C*
044800000000     C                   ENDSL
044900000000     C*
045000000000     C                   ENDIF
045100000000     C*
045200000000     C                   ENDSR
045300000000     C/EJECT
045400000000     C************************************************************
045500000000     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
045600000000     C************************************************************
045700000000     C     INZD1         BEGSR
045800000000     C*
045900000000     C                   CLEAR                   FMTD1
046000160512     C*
046100160512     C                   SELECT
046200160512     C*
046300160603    >C     OPZK03        WHENNE    *BLANK
046400160512     C* imposto i valori passati sui campi a video
046500160603    >C                   MOVE      AcfK03        D1ACf
046600160512     C* il pgm non è richiamato
046700160603    >C     OPZK03        WHENEQ    *BLANK
046800160512     C* imposto dei valori di default
046900160512    >C                   Eval            D1Acf  =  *Year - 1
047000160512     C                   ENDSL
047100160512     C*
047200000000     C*
047300000000     C                   ENDSR
047400000000     C/EJECT
047500000000     C************************************************************
047600000000     C* GESTIONE F02 VIDEO D1
047700000000     C************************************************************
047800000000     C     F02D1         BEGSR
047900000000     C*
048000000000     C* per decodificare la videata richiamo la routine dei ctrl
048100000000     C* e poi disattivo gli eventuali messaggi di errore
048200000000     C                   EXSR      CTRD1
048300000000     C*
048400000000     C                   MOVE      *ALL'0'       IN5098
048500000000     C                   MOVEA     IN5098        *IN(50)
048600000000     C*
048700000000     C                   ENDSR
048800000000     C/EJECT
048900000000     C************************************************************
049000000000     C* GESTIONE F03 VIDEO D1
049100000000     C************************************************************
049200000000     C     F03D1         BEGSR
049300000000     C*
049400000000     C                   MOVE      *ON           $FINE
049500160603     C                   MOVE      '1'           RETK03
049600000000     C* fine programma
049700000000     C                   EXSR      ENDPGM
049800000000     C*
049900000000     C                   ENDSR
050000000000     C/EJECT
050100000000     C************************************************************
050200000000     C* GESTIONE F10 VIDEO D1
050300000000     C************************************************************
050400000000     C     F10D1         BEGSR
050500000000     C*
050600000000     C                   MOVE      'S1'          $GEST
050700000000     C*
050800000000     C                   MOVE      *HIVAL        $LASTV
050900000000     C*
051000000000     C                   ENDSR
051100000000     C/EJECT
051200000000     C************************************************************
051300000000     C* GESTIONE F12 VIDEO D1
051400000000     C************************************************************
051500000000     C     F12D1         BEGSR
051600000000     C*
051700000000     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
051800000000     C*  per F12 torno alla lista
051900000000     C     $LOP          IFEQ      2
052000000000     C     WINKR         OREQ      *ON
052100000000     C                   MOVE      'S1'          $GEST
052200000000     C*  altrimenti, per F12 torno a menu
052300000000     C                   ELSE
052400000000     C                   MOVE      *ON           $FINE
052500160603     C                   MOVE      '2'           RETK03
052600000000     C                   ENDIF
052700000000     C*
052800000000     C                   ENDSR
052900000000     C/EJECT
053000000000     C************************************************************
053100000000     C* GESTIONE F24 VIDEO D1
053200000000     C* input
053300000000     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
053400000000     C* $FX1 =Testi tasti funzione
053500000000     C* $FC1 = Abilitazioni
053600000000     C* $FR1 = Inversioni di fondo
053700000000     C************************************************************
053800000000     C     F24D1         BEGSR
053900000000     C*
054000000000     C                   MOVEA     $FX1          $FX
054100000000     C                   MOVEA     $FC1          $FC
054200000000     C                   MOVEA     $FR1          $FR
054300000000     C                   Z-ADD     $ULKD1        $ULK
054400000000     C                   Z-ADD     $D1L01        $L01
054500000000     C                   Z-ADD     $D1L02        $L02
054600000000     C                   EXSR      RIEKEY
054700000000     C                   Z-ADD     $ULK          $ULKD1            3 0
054800000000     C                   MOVEA     $FC           $FC1
054900000000     C                   MOVEL     $KEY1         Z1KE1
055000000000     C                   MOVEL     $KEY2         Z1KE2
055100000000     C                   WRITE     FMTZ1
055200000000     C*
055300000000     C*
055400000000     C                   ENDSR
055500000000     C/EJECT
055600000000     C************************************************************
055700000000     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
055800000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
055900000000     C* 93 94 96 97 98
056000000000     C************************************************************
056100000000     C     CTRD1         BEGSR
056200000000     C*
056300000000     C                   SETOFF                                       99
056400000000     C*
056500000000     C                   ENDSR
056600160429     C/EJECT
056700130823     C************************************************************
056800110830     C* GESTIONE LISTA
056900110830     C************************************************************
057000110830     C     GESS1         BEGSR
057100110830     C*
057200000000     C* inizializzazione videata
057300000000     C     $InzS1        IFEQ      *ON
057400000000     C                   EXSR      INZS1
057500000000     C                   MOVE      *OFF          $INZS1
057600000000     C                   ENDIF
057700000000     C*
057800000000     C* emissione piede videata se proveniente da altra
057900000000     C* salvataggio valore formato di provenienza
058000000000     C     $LASTV        IFNE      'S1'
058100000000     C*
058200000000     C* riempimento sottotitolo
058300000000     C     SOTTIT        IFNE      *BLANK
058400000000     C                   MOVEL     SOTTIT        IDMSG
058500000000     C                   CALLB     'XRTVM'
058600000000     C                   PARM                    IDMSG            27
058700000000     C                   PARM                    TXTMSG          100
058800000000     C                   PARM                    ERRMSG            1
058900000000     C                   PARM      *OFF          CTRMSG            1            centratura
059000000000     C                   PARM      30            LENMSG            3 0          lun output.
059100000000     C     ERRMSG        IFNE      *ON
059200000000     C                   MOVEL     TXTMSG        D1TIT
059300000000     C                   ELSE
059400000000     C                   MOVEL     *ALL'?'       D1TIT
059500000000     C                   ENDIF
059600000000     C                   ENDIF
059700000000      *
059800000000     C                   WRITE     FMTT1
059900000000     C                   EXSR      GESPIE
060000000000     C*                  MOVE      $LASTV        $LASTG
060100000000     C                   MOVE      'S1'          $LASTV
060200000000     C                   ENDIF
060300000000     C*
060400000000     C                   Eval      *In09       = C1Mode
060500000000     C*
060600000000     C* Errori
060700000000   B1C     *IN99         IFEQ      *ON
060800000000     C                   MOVEA     *IN           WIN
060900000000     C                   MOVE      *ALL'0'       IN5098           49
061000000000     C                   MOVEA     IN5098        *IN(50)
061100000000     C                   WRITE     FMTC1
061200000000     C                   MOVEA     WIN           *IN
061300000000   E1C                   ENDIF
061400000000     C*
061500000000   B1C                   SELECT
061600000000     C* Annullamento
061700160603     C     OPZK03        WHENEQ    '04'
061800000000     C* Visualizzazione
061900160603     C     OPZK03        OREQ      '05'
062000000000     C                   WRITE     FMTC1
062100000000     C                   EXFMT     PROTECT
062200000000     C* Immissione/Modifica/Copia
062300000000   X1C                   OTHER
062400000000     C                   EXFMT     FMTC1
062500000000   E1C                   ENDSL
062600000000     C*
062700000000     C*
062800000000     C* inizializzazione indice opzioni personalizzate
062900000000     C                   Eval      PosOpz      = 0
063000000000     C                   if        SflNrR     <> 0
063100000000     C                   Z-Add     SflNrR        C1Rcd
063200000000     C                   endif
063300000000     C*
063400000000     C* CONTROLLO ABILITAZIONE TASTI
063500000000     C* INPUT
063600000000     C* $FC2 = Abilitazioni videata S1
063700000000     C* OUTPUT
063800000000     C* *IN97 + *IN99 Errori
063900000000     C                   MOVEA     $FC2          $FC
064000000000     C                   EXSR      CTRKEY
064100000000     C*
064200000000     C     *IN99         IFEQ      *OFF
064300000000     C*
064400000000     C                   SELECT
064500000000     C* F3=Fine
064600000000     C     $TASTO        WHENEQ    F03
064700000000     C                   EXSR      F03S1
064800000000     C* F4=Ricerca
064900000000     C     $TASTO        WHENEQ    F04
065000000000     C                   EXSR      SEARCH
065100000000     C* F5=Ripristina
065200000000     C     $TASTO        WHENEQ    F05
065300000000     C                   EXSR      F05S1
065400000000     C* F8=Successivo
065500000000     C     $TASTO        WHENEQ    F08
065600000000     C                   EXSR      F08S1
065700000000     C* F12=Ritorno
065800000000     C     $TASTO        WHENEQ    F12
065900000000     C                   EXSR      F12S1
066000000000     C* F24=Altri tasti
066100000000     C     $TASTO        WHENEQ    F24
066200000000     C                   EXSR      F24S1
066300000000     C* ROLL-UP
066400000000     C     $TASTO        WHENEQ    ROLLUP
066500000000     C                   EXSR      ROLS1
066600000000     C*
066700000000     C                   OTHER
066800000000     C*
066900000000     C* Controllo dati nella testata subfile
067000000000     C                   EXSR      CTRC1
067100000000     C*
067200000000     C* Controllo dati nella lista subfile
067300000000     C                   EXSR      CTRS1
067400000000     C*
067500000000     C     $TASTO        IFEQ      F06
067600000000     C     *IN99         ANDEQ     *OFF
067700000000     C*
067800000000     C* Salvataggio e controllo
067900000000     C                   EXSR      AGGANA
068000000000     C                   ENDIF
068100000000     C*
068200000000     C                   ENDSL
068300000000     C*
068400000000     C                   ENDIF
068500000000     C*
068600000000     C                   ENDSR
068700000000     C/EJECT
068800000000     C************************************************************
068900000000     C* INIZIALIZZAZIONE LISTA
069000000000     C************************************************************
069100000000     C     INZS1         BEGSR
069200000000     C* pulizia SFL
069300000000     C                   SETOFF                                         3031
069400000000     C                   WRITE     FMTC1
069500000000     C                   SETON                                          3031
069600160512     C*
069700000000     C                   Z-ADD     0             S1NRR
069800000000     C                   Z-ADD     0             WMAX
069900000000     C*
070000000000     C* Riempimento testata
070100000000     C                   EXSR      RIEC1
070200000000     C*
070300000000     C* Imposto gli attributi del subfile control
070400000000     C                   EXSR      ATRC1
070500000000     C*
070600000000     C*
070700000000     C* carico tutte le righe di dettaglio, fino al max del sfl
070800000000     C                   Eval      SflPag      = 9999
070900000000     C*
071000160511     C                   EXSR      ReadND77R
071100000000     C*
071200000000     C* Caricamento pagina intera vuota
071300000000     C                   EXSR      ROLS1
071400000000     C                   Z-ADD     1             C1RCD
071500000000     C*
071600000000     C                   ENDSR
071700000000     C/EJECT
071800000000     C************************************************************
071900000000     C* Riempimento dati di testata
072000000000     C************************************************************
072100000000     C     RIEC1         BEGSR
072200000000     C*
072300160429     C                   If        D1Acf <> 0
072400160511     C                   Move      D1Acf         AnnoAlfa          4
072500160603     C                   eval      C1Tit = 'Anno di competenza ..  '  +
072600160511     C                                     %trim(Annoalfa)
072700160429     C                   Endif
072800000000     C*
072900000000     C                   ENDSR
073000000000     C/EJECT
073100000000     C************************************************************
073200000000     C* CARICAMENTO PAGINA VUOTA
073300000000     C************************************************************
073400000000     C     ROLS1         BEGSR
073500000000     C*
073600000000     C                   SETOFF                                       32
073700000000     C*
073800000000     C                   Z-ADD     WMAX          S1NRR
073900000000     C*
074000000000     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
074100000000     C                   Clear                   FMTS1
074200000000     C*
074300000000     C* Carico righe rimanenti o pagina vuota
074400000000     C     S1NRR         DIV       SFLPAG        RISUL
074500000000     C                   MVR                     RESTO
074600000000     C                   ADD       1             RESTO
074700000000     C*
074800000000     C     RESTO         DO        SFLPAG        RESTO
074900000000     C                   ADD       1             S1NRR
075000000000     C                   Z-ADD     S1Nrr         H1NrRiga
075100000000     C* Attributi sfl
075200000000     C                   EXSR      ATRS1
075300000000     C                   WRITE     FMTS1
075400000000     C                   Clear                   FMTS1
075500000000     C                   ENDDO
075600000000     C*
075700000000     C                   Z-ADD     S1NRR         WMAX
075800000000     C                   Z-ADD     S1NRR         C1RCD
075900000000      *
076000000000      * POSIZIONAMENTO AL 1° RCD DELLA PAGINA
076100000000     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
076200000000     C                   MVR                     RESTO
076300000000     C     PAGINE        MULT      SFLPAG        C1RCD
076400000000     C     RESTO         IFGT      0
076500000000     C                   ADD       1             C1RCD
076600000000     C                   ELSE
076700000000     C                   SUB       SFLPAG        C1RCD
076800000000     C                   ADD       1             C1RCD
076900000000     C                   ENDIF
077000000000     C*
077100000000     C                   ENDSR
077200000000     C/EJECT
077300000000     C************************************************************
077400160429     C* Lettura ritenute
077500000000     C************************************************************
077600160429     C     ReadND77R     BEGSR
077700000000     C*
077800000000     C                   MOVEL     *OFF          $EndFile
077900000000     C                   MOVEL     *OFF          $RCDOK
078000000000     C                   Z-ADD     0             $Y
078100000000     C*
078200160503     C                   Eval      R77Societa  = XScSoc
078300160429     C                   Eval      R77Acf      = D1Acf
078400160512     C     K0277R01      SETLL     Nd77R01L
078500000000     C*
078600000000     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
078700160603     C     OPZK03        IFEQ      '02'
078800160603     C     OPZK03        OREQ      '04'
078900160512     C     K0277R01      READE     Nd77R01l                               21
079000000000     C                   else
079100160512     C     K0277R01      READE(N)  Nd77R01l                               21
079200000000     C                   endif
079300000000     C*
079400000000     C                   Eval      $EndFile    = *In21
079500000000     C*
079600000000     C                   DOW       $EndFile    = *Off    and
079700000000     C                             $Y          < SflPag
079800000000     C*
079900000000     C                   EXSR      SELANA
080000000000     C*
080100000000     C                   ADD       1             S1NrR
080200000000     C                   EXSR      RIES1
080300000000     C* Attributi sfl
080400000000     C                   EXSR      ATRS1
080500000000     C*
080600000000     C* attivo sempre sflnxtchg
080700000000     C                   Move      *ON           *IN32
080800000000     C                   WRITE     FMTS1
080900000000     C*
081000000000     C                   ADD       1             $Y
081100000000     C*
081200000000     C*
081300000000     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
081400160603     C     OPZK03        IFEQ      '02'
081500160603     C     OPZK03        OREQ      '04'
081600160512     C     K0277R01      READE     Nd77R01L                               21
081700000000     C                   else
081800160512     C     K0277R01      READE(N)  Nd77R01l                               21
081900000000     C                   endif
082000000000     C*
082100000000     C                   Eval      $EndFile    = *In21
082200000000     C                   ENDDO
082300000000     C*
082400000000     C*
082500000000     C                   Z-ADD     S1Nrr         WMax
082600000000     C*
082700000000     C* Reimposto il numero di righe per pagina al valore iniziale
082800000000     C                   Reset                   SflPag
082900000000     C*
083000000000     C                   ENDSR
083100000000     C/EJECT
083200000000     C************************************************************
083300000000     C* SELEZIONI SULLA RIGA LETTA
083400000000     C* impostare a *OFF il campo $RCDOK per scartare un record che
083500000000     C* non rientra nei parametri di parzializzazione
083600000000     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
083700000000     C* superiore stabilito nei parametri
083800000000     C*
083900000000     C************************************************************
084000000000     C     SELANA        BEGSR
084100160503     C
084200000000     C                   MOVEL     *ON           $RCDOK
084300160503     C*
084400160503     C                   Movel     R77Mac        $Mese
084500160503     C                   Move      R77Mac        $Anno
084600160503     C*
084700160513     C                   If        D1Mac <> 0 and
084800160513     C                             $MeAnDS <> D1MAC
084900160503     C                   MOVEL     *Off          $RCDOK
085000160503     C                   Endif
085100000000     C*
085200000000     C                   ENDSR
085300000000     C/EJECT
085400000000     C************************************************************
085500000000     C* RIEMPIMENTO RIGA LISTA
085600000000     C************************************************************
085700000000     C     RIES1         BEGSR
085800000000     C*
085900000000     C                   CLEAR                   FMTS1
086000160510     C*
086100160510     C                   Z-Add     S1Nrr         H1NrRiga
086200160510     C                   Z-Add     Nrr           H1Nrr
086300160503     C                   Eval      S1Mac       = $meands
086400160503     C                   Eval      S1Rac       = R77Rac
086500160503     C                   Eval      S1Ref       = R77Ref
086600160503     C                   Eval      S1Ius       = R77Ius
086700160503     C                   Eval      S1Vec       = R77Vec
086800160503     C                   Eval      S1Cim       = R77Cim
086900160503     C                   Eval      S1Imv       = R77Imv
087000160503     C                   Eval      S1Iro       = R77Iro
087100160503     C                   If        R77Ravvedi  = *on
087200160503     C                   Eval      S1Ravvedi   = '/'
087300160503     C                   Else
087400160503     C                   Eval      S1Ravvedi   = ' '
087500160503     C                   Endif
087600160503     C                   Eval      S1Note      = R77Note
087700160503     C                   Eval      S1Tri       = R77Tri
087800160503     C                   If        R77Teso     = *on
087900160503     C                   Eval      S1Teso      = '/'
088000160503     C                   Else
088100160503     C                   Eval      S1Teso      = ' '
088200160503     C                   Endif
088300160620R218 X***                Eval      C1EVE = R77Eve
088400160620R218 X***                Eval      C1CDFS= R77CDFS
088500160620R218 C                   Eval      S1EVE = R77Eve
088600160620R218 C                   Eval      S1CDFS= R77CDFS
088700160503      *
088800160511     C                   If        R77Dqv <> *loval
088900160503     C     *jobrun       Move      R77Dqv        S1Dqv
089000160511     C                   Endif
089100160603     C                   Eval      S1Nqv = R77Nqv
089200160513     C                   Eval      H1Qua = R77Qua
089300160513     C                   Eval      H1TPQ = R77Tpq
089400160513     C                   Eval      H1PTR = R77Ptr
089500160513     C                   Eval      H1REG = R77Reg
089600160503      *
089700000000     C                   ENDSR
089800000000     C/EJECT
089900000000     C************************************************************
090000000000     C* RIPRISTINA GLI ATTRIBUTI DELLA TESTATA DEL SFL
090100000000     C************************************************************
090200000000     C     ATRC1         BEGSR
090300000000     C*
090400000000     C*
090500000000     C                   ENDSR
090600000000     C/EJECT
090700000000     C************************************************************
090800000000     C* CONTROLLO TESTATA LISTA
090900000000     C************************************************************
091000000000     C     CTRC1         BEGSR
091100000000     C*
091200000000     C                   MOVE      *OFF          *IN99
091300160620R218 X***                If        C1EVE <> *blanks and
091400160620  "  X***                          C1CDFS = *blanks
091500160620  "  X***                Seton                                        5899
091600160620R218 X***                Endif
091700000000     C*
091800000000     C                   ENDSR
091900000000     C/EJECT
092000000000     C************************************************************
092100000000     C* CONTROLLO DI RIGHE DEL SUBFILE
092200000000     C************************************************************
092300000000     C     CTRS1         BEGSR
092400000000     C*
092500000000     C                   Eval      $AlmenoUna  = *Off
092600000000     C                   Eval      $RigaV      = *Off
092700000000     C*
092800000000     C*
092900000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
093000000000     C*
093100160603     C     OPZK03        IFNE      '04'
093200000000     C*
093300000000     C                   Eval      $Progr      = 1
093400000000     C     $Progr        CHAIN     FMTS1                              21
093500000000     C                   Eval      $FineLista  = *In21
093600000000     C*
093700000000     C* esce se fine sfl o errore che richiede l'uscita
093800000000     C                   DOW       $FineLista  = *Off
093900000000     C*
094000000000     C* Ctrl su riga
094100000000     C                   EXSR      RECS1
094200000000     C*
094300000000     C* se rilevato errore lo segnalo e
094400000000     C     *IN99         IFEQ      *ON
094500000000     C* porto il cursore sulla riga
094600000000     C                   Z-ADD     S1NrR         C1Rcd
094700000000     C                   Eval      $FineLista  = *On
094800000000     C                   ENDIF
094900000000     C*
095000000000     C* imposto gli attributi
095100000000     C                   EXSR      ATRS1
095200000000     C*
095300000000     C* Attivo sempre sflnxtchg
095400000000     C                   MOVE      *ON           *IN32
095500000000     C                   UPDATE    FMTS1
095600000000     C                   MOVE      *OFF          *IN32
095700000000     C*
095800000000     C*
095900000000     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
096000000000     C                   if        $FineLista  = *Off
096100000000     C                   Add       1             $Progr
096200000000     C     $Progr        CHAIN     FMTS1                              21
096300000000     C                   Eval      $FineLista  = *In21
096400000000     C                   endif
096500000000     C                   ENDDO
096600000000     C*
096700000000     C                   ENDIF
096800000000     C*
096900000000     C                   ENDSR
097000000000     C/EJECT
097100000000     C************************************************************
097200000000     C* CONTROLLO CAMPI I/O RIGA LISTA
097300000000     C************************************************************
097400000000     C     RECS1         BEGSR
097500000000     C*
097600000000     C* Reset indicatori DSPATR(PC)
097700000000     C                   MOVE      *ALL'0'       IN4049           10
097800000000     C                   MOVEA     IN4049        *IN(40)
097900000000     C*
098000000000     C* controllo che almeno un campo sia valorizzato
098100160510     C                   If        S1Mac      <>0       or
098200160510     C                             S1Rac      <>0       or
098300160510     C                             S1Ref      <>0       or
098400160510     C                             S1Ius      <>0       or
098500160510     C                             S1Vec      <>0       or
098600160510     C                             S1Cim      <>0       or
098700160527     C                             S1Imv      <>0       or
098800160510     C                             S1Iro      <>0       or
098900160510     C                             S1Ravvedi  <>*blank  or
099000160510     C                             S1note     <>*blank  or
099100160510     C                             S1Tri      <>*blank  or
099200160510     C                             S1Teso     <>*blank  or
099300160510     C                             S1Dqv      <>0
099400000000     C*
099500160510     C                   if        H1NrRiga    > 1
099600000000     C                   Eval      $AlmenoUna  = *On
099700160503     C* controllo se riga già inserita con gli stessi valori
099800160503     C                   EXSR      CtrRigaDoppia
099900160510     C                   endif
100000160527     C*
100100160527     C                   Exsr      CtrIncongru
100200160527     C*
100300160527     C                   Exsr      CtrPeriodo
100400160527     C*
100500160527     C                   Exsr      CtrIntro
100600160527     C*
100700160510     C                   Exsr      CtrRavvedi
100800160510     C*
100900160510     C                   Exsr      CtrTeso
101000160510     C*
101100160510     C                   Exsr      CtrDqv
101200160620R218 C*
101300160620R218 C                   Exsr      CtrSos
101400160510     C*
101500000000     C                   else
101600000000     C*
101700000000     C                   if        $RigaV      = *Off
101800000000     C                   Eval      $RigaV      = *On
101900000000     C                   endif
102000000000     C*
102100000000     C                   endif
102200000000     C*
102300000000     C                   ENDSR
102400160527     C/EJECT
102500160527     C************************************************************
102600160527     C* CONTROLLO periodo
102700160527     C************************************************************
102800160527     C     CtrPeriodo    BEGSR
102900160527     C*
103000160527     C                   Eval      $Meands     = S1Mac
103100160530     C                   If        $Anno4 <> D1Acf  or
103200160527     C                             ($Mese < 1 and $mese > 12)
103300160527     C                   Seton                                        7099
103400160527     C                   Endif
103500160527     C*
103600160527     C                   ENdsr
103700000000     C/EJECT
103800160527     C************************************************************
103900160527     C* CONTROLLO incongruenze
104000160527     C************************************************************
104100160527     C     CtrIncongru   BEGSR
104200160527     C*
104300160527     C                   If        S1Mac = 0 or
104400160530     C                             S1Tri = *blanks or
104500160530     C                             (S1Rac = 0 and
104600160530     C                             S1Ref = 0 and
104700160530     C                             S1Ius = 0 and
104800160530     C                             S1Vec = 0 and
104900160530     C                             S1Cim = 0 and
105000160530     C                             S1Imv = 0 and
105100160530     C                             S1Iro = 0 and
105200160530     C                             S1Mac <> 0 and
105300160530     C                             S1Tri <> *blanks)
105400160527     C                   Seton                                        7299
105500160527     C                   Endif
105600160527     C*
105700160527     C                   ENdsr
105800160527     C/EJECT
105900160527     C************************************************************
106000160527     C* CONTROLLO interessi
106100160527     C************************************************************
106200160527     C     CtrIntro      BEGSR
106300160527     C*
106400160527     C                   If        S1Iro > S1Imv
106500160527     C                   Seton                                        7199
106600160527     C                   Endif
106700160527     C*
106800160527     C                   ENdsr
106900160527     C/EJECT
107000160510     C************************************************************
107100000000     C* CONTROLLO riga doppia
107200000000     C************************************************************
107300000000     C     CtrRigaDoppia BEGSR
107400000000     C*
107500000000     C* salvo i dati dell'ultima riga letta
107600160511     C                   Eval      SavNrRiga   = H1NrRiga
107700160510     C                   Eval      SavMac      = S1Mac
107800160510     C                   Eval      SavRac      = S1Rac
107900160510     C                   Eval      SavRef      = S1Ref
108000160510     C                   Eval      SavIus      = S1Ius
108100160510     C                   Eval      SavVec      = S1Vec
108200160510     C                   Eval      SavCim      = S1Cim
108300160510     C                   Eval      SavIro      = S1Iro
108400160510     C                   Eval      SavRavvedi  = S1Ravvedi
108500160510     C                   Eval      Savnote     = S1note
108600160510     C                   Eval      SavTri      = S1Tri
108700160510     C                   Eval      SavTeso     = S1Teso
108800160510     C                   Eval      SavDqv      = S1Dqv
108900000000     C*
109000000000     C* mi posiziono sulla 1° riga e scorro la lista fino a che non ne
109100000000     C* trovo una con gli stessi valori oppure non arrivo alla riga
109200000000     C* corrente
109300000000     C                   Eval      $Progr      = 1
109400000000     C     $Progr        CHAIN     FMTS1                              21
109500000000     C                   Eval      $FineS1     = *In21
109600000000     C*
109700000000     C                   DOW       $FineS1     = *OFF         and
109800000000     C                             H1NrRiga    < SavNrRiga
109900000000     C*
110000160510     C                   if        SavMac      = S1Mac        and
110100160510     C                             SavRac      = S1Rac        and
110200160510     C                             SavRef      = S1Ref        and
110300160510     C                             SavIus      = S1Ius        and
110400160510     C                             SavVec      = S1Vec        and
110500160510     C                             SavCim      = S1Cim        and
110600160510     C                             SavIro      = S1Iro        and
110700160510     C                             SavRavvedi  = S1Ravvedi    and
110800160510     C                             Savnote     = S1note       and
110900160510     C                             SavTri      = S1Tri        and
111000160510     C                             SavTeso     = S1Teso       and
111100160510     C                             SavDqv      = S1Dqv
111200000000     C*
111300000000     C* mi riposiziono sulla riga corrente
111400000000     C                   Eval      $Progr      = SavNrRiga
111500000000     C     $Progr        CHAIN     FMTS1                              21
111600000000     C*
111700000000     C* riga doppia
111800160510     C                   SetOn                                        594099
111900130823     C*
112000000000     C* porto il cursore sulla riga
112100000000     C                   Z-ADD     S1NrR         C1Rcd
112200000000     C*
112300000000     C* dico di uscire dal ciclo
112400000000     C                   Eval      $FineS1     = *On
112500000000     C                   ITER
112600000000     C                   endif
112700000000     C*
112800000000     C*
112900000000     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
113000000000     C                   Add       1             $Progr            4 0
113100000000     C     $Progr        CHAIN     FMTS1                              21
113200000000     C                   Eval      $FineS1     = *In21
113300000000     C                   ENDDO
113400000000     C*
113500000000     C                   ENDSR
113600160511     C/EJECT
113700160510     C************************************************************
113800160510     C* CONTROLLO ravvedimento
113900160510     C************************************************************
114000160510     C     CtrRavvedi    BEGSR
114100160510     C*
114200160510     C                   If        S1Ravvedi <> '/' and
114300160510     C                             S1Ravvedi <> ' '
114400160510     C                   Seton                                        7399
114500160510     C                   ENdif
114600160510     C*
114700160510     C                   Endsr
114800160511     C/EJECT
114900160510     C************************************************************
115000160510     C* CONTROLLO Data quietanza
115100160510     C************************************************************
115200160510     C     CtrDqv        BEGSR
115300160510     C*
115400160511     C*
115500160511     C                   Clear                   WDqv
115600160511     C*
115700160511     C                   If        S1Dqv     <> 0
115800160511     C     *DMY          TEST(D)                 S1Dqv                  75
115900160511     C*
116000160511     C* data corretta
116100160511     C                   If        *In75       = *Off
116200160511     C     *JobRun       Move      s1Dqv         WDqv
116300160511     C                   Else
116400160511     C                   Seton                                        7599
116500160511     C                   Endif
116600160527     C                   Else
116700160527     C* Se importo versato valorizzato ci deve essere anche la data
116800160527     C                   If        S1Imv > 0
116900160527     C                   Seton                                        7699
117000160527     C                   Endif
117100160511     C                   Endif
117200160510     C*
117300160510     C                   Endsr
117400160510     C*
117500160511     C/EJECT
117600160510     C************************************************************
117700160510     C* CONTROLLO Tesoreria
117800160510     C************************************************************
117900160510     C     CtrTeso       BEGSR
118000160510     C*
118100160510     C                   If        S1Teso    <> '/' and
118200160510     C                             S1Teso    <> ' '
118300160510     C                   Seton                                        7499
118400160510     C                   ENdif
118500160510     C*
118600160510     C                   Endsr
118700160510     C*
118800160510     C/EJECT
118900160620R218 C************************************************************
119000160620  "  C* Controllo dati sostituto
119100160620  "  C************************************************************
119200160620R218 C     CTRSOS        BEGSR
119300160621     C*
119400160621     C                   If        S1Eve <> *blanks
119500160621     C                   CALL      'XALLINEA'
119600160621     C                   Parm                    S1Eve
119700160621     C                   Parm      2             $Len              5 0
119800160621     C                   Parm      '2'           Tipo              1
119900160621     C                   Endif
120000160621     C*
120100160620     C                   If        S1EVE  <> *blanks and
120200160620     C                             S1CDFS = *blanks or
120300160620     C                             S1EVE  = *blanks and
120400160620     C                             S1CDFS <> *blanks
120500160620     C                   Seton                                        5899
120600160620     C                   Endif
120700160620     C*
120800160620R218 C                   Endsr
120900160620R218 C/EJECT
121000110830     C************************************************************
121100110830     C* Imposto gli attributi della riga di sfl
121200110830     C************************************************************
121300110830     C     ATRS1         BEGSR
121400000000     C*
121500160503     C                   setoff                                       37
121600000000     C*
121700160503     C                   If        H1Nrr    <> 0
121800160503     C                   SetOn                                        37
121900000000     C                   EndIf
122000000000     C*
122100000000     C                   ENDSR
122200000000     C/EJECT
122300000000     C************************************************************
122400000000     C* GESTIONE F03 VIDEO S1
122500000000     C************************************************************
122600000000     C     F03S1         BEGSR
122700000000     C*
122800160503     C                   MOVE      *ON           $FINE
122900160603     C                   MOVE      '1'           RETK03
123000000000     C* fine programma
123100000000     C                   EXSR      ENDPGM
123200000000     C*
123300000000     C                   ENDSR
123400000000     C/EJECT
123500000000     C************************************************************
123600000000     C* RICERCHE
123700000000     C************************************************************
123800000000     C     SEARCH        BEGSR
123900000000     C*
124000000000     C*
124100000000     C                   ENDSR
124200000000     C/EJECT
124300000000     C************************************************************
124400000000     C* GESTIONE F05 VIDEO S1
124500000000     C************************************************************
124600000000     C     F05S1         BEGSR
124700000000     C*
124800000000     C                   Move      *On           $InzS1
124900000000     C                   ROLBK
125000000000     C*
125100000000     C                   ENDSR
125200000000     C/EJECT
125300000000     C************************************************************
125400000000     C* GESTIONE F08 VIDEO S1
125500000000     C************************************************************
125600000000     C     F08S1         BEGSR
125700000000     C*
125800000000     C                   MOVE      *ON           $FINE
125900160603     C                   MOVE      '0'           RETK03
126000000000     C*
126100000000     C                   ENDSR
126200000000     C/EJECT
126300000000     C************************************************************
126400000000     C* GESTIONE F12 VIDEO S1
126500000000     C************************************************************
126600000000     C     F12S1         BEGSR
126700000000     C*
126800000000     C                   MOVE      'D1'          $GEST
126900000000     C*
127000000000     C                   ENDSR
127100000000     C/EJECT
127200000000     C************************************************************
127300000000     C* GESTIONE F24 VIDEO S1
127400000000     C************************************************************
127500000000     C     F24S1         BEGSR
127600000000     C*
127700000000     C                   MOVEA     $FX2          $FX
127800000000     C                   MOVEA     $FC2          $FC
127900000000     C                   MOVEA     $FR2          $FR
128000000000     C                   Z-ADD     $ULKS1        $ULK
128100000000     C                   Z-ADD     $S1L01        $L01
128200000000     C                   Z-ADD     $S1L02        $L02
128300000000     C                   EXSR      RIEKEY
128400000000     C                   Z-ADD     $ULK          $ULKS1            3 0
128500000000     C                   MOVEA     $FC           $FC2
128600000000     C                   MOVEL     $KEY1         Z2KE1
128700000000     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
128800000000     C                   MOVEL     $KEY2         Z2KE2
128900000000     C                   WRITE     FMTZ2
129000000000     C*
129100000000     C                   ENDSR
129200000000     C/EJECT
129300000000     C************************************************************
129400000000     C* GESTIONE F06 VIDEO S1
129500000000     C************************************************************
129600000000     C     AGGANA        BEGSR
129700000000     C*
129800000000     C                   SELECT
129900000000     C*
130000160603     C     OPZK03        WHENEQ    '02'
130100000000     C*
130200160503     C                   EXSR      AggDett
130300000000     C*
130400000000     C                   ENDSL
130500000000     C*
130600000000     C                   COMMIT
130700000000     C*
130800000000     C* Segnalo al pgm chiamante l'avvenuta conferma
130900160603     C                   Move      '1'           OPRK03
131000000000     C*
131100000000     C* Segnalo al pgm chiamante l'aver premuto l'F6
131200160603     C                   Move      '0'           RETK03
131300000000     C*
131400000000     C                   Select
131500160603     C     OPZK03        WhenEQ    '01'
131600160603     C     OPZK03        oreq      '02'
131700000000     C*
131800000000     C* Riparto
131900000000     C                   MOVE      *On           $InzS1
132000000000     C                   MOVE      'S1'          $LastV
132100000000     C*
132200000000     C                   Other
132300000000     C*
132400000000     C                   MOVE      *On           $Fine
132500000000     C*
132600000000     C                   endsl
132700000000     C*
132800000000     C                   ENDSR
132900000000     C/EJECT
133000000000     C************************************************************
133100160503     C* Aggiorno file ND77R00F  da sfl
133200000000     C************************************************************
133300160503     C     AggDett       BEGSR
133400000000     C*
133500000000     C                   DO        *Hival        $X                4 0
133600000000     C     $X            CHAIN     FMTS1                              21
133700000000     C*
133800000000     C* Fine lettura subfile
133900000000     C                   if        *In21       = *On
134000000000     C                   Leave
134100000000     C                   endif
134200000000     C*
134300000000     C* leggo riga successiva se dati non significativi
134400160503     C                   if        H1Nrr      = 0       and
134500160503     C                             S1Mac      = 0       and
134600160503     C                             S1Rac      = 0       and
134700160503     C                             S1Ref      = 0       and
134800160503     C                             S1Ius      = 0       and
134900160503     C                             S1Vec      = 0       and
135000160503     C                             S1Cim      = 0       and
135100160503     C                             S1Iro      = 0       and
135200160503     C                             S1Ravvedi  = *blank  and
135300160503     C                             S1note     = *blank  and
135400160503     C                             S1Tri      = *blank  and
135500160503     C                             S1Teso     = *blank  and
135600160503     C                             S1Dqv      = 0
135700000000     C                   iter
135800000000     C                   endif
135900000000     C*
136000160503     C     H1Nrr         Ifne      0
136100160503     C*
136200160503     C     H1Nrr         Chain     Nd77r00f                           2122
136300160503     C*
136400160513     C                   Else
136500160513     C
136600160513     C                   Seton                                        21
136700160513     C                   Endif
136800160513     C
136900160503     C* Gestione RCD allocato
137000160503   B3C     *in22         IFEQ      *ON
137100160511     C                   CLEAR                   SDGMSGDS
137200160511    >C                   MOVEL     'ND77R00f'    MSGFIL
137300160503     C                   MOVE      'S'           MSGLCK
137400160503     C                   Z-ADD     2             MSGMSG
137500160503     C                   MOVEL     'R'           MSGTPR
137600160503     C                   MOVEL     'S'           MSGRSP
137700160503     C                   MOVEL     'S'           MSGEMV
137800160503     C                   Z-ADD     10            MSGRGP
137900160503     C                   MOVEA     STATUS        STS
138000160503     C                   MOVEL     'N'           MSGCNL
138100160503     C                   MOVEL     'N'           MSGVID
138200160503     C                   MOVEL     'N'           MSGOPE
138300160503     C                   MOVEL     'N'           MSGSTP
138400160511     C                   CALL      'SDGMSGR'
138500160511     C                   PARM                    SDGMSGDS
138600160503   E3C                   endif
138700160503     C*
138800160513     C                   If        S1Mac     =  0       and
138900160513     C                             S1Rac     =  0       and
139000160513     C                             S1Ref     =  0       and
139100160513     C                             S1Ius     =  0       and
139200160513     C                             S1Vec     =  0       and
139300160513     C                             S1Cim     =  0       and
139400160513     C                             S1Iro     =  0       and
139500160513     C                             S1Ravvedi =  *blank  and
139600160513     C                             S1note    =  *blank  and
139700160513     C                             S1Tri     =  *blank  and
139800160513     C                             S1Teso    =  *blank  and
139900160503     C                             S1Dqv     =  0
140000160503     C                   If        *in21 = *off
140100160503     C                   Delete    Nd77r000
140200160503     C                   Endif
140300160503     C                   Else
140400160503     C*
140500000000     C                   EXSR      RieANA
140600000000     C*
140700160503     C                   If        *in21 = *off
140800160503     C                   UPDATE    Nd77r000
140900160503     C                   Else
141000160503     C                   Write     Nd77r000
141100160503     C                   Endif
141200160504     C                   Endif
141300000000     C*
141400000000     C                   ENDDO
141500000000     C*
141600000000     C                   ENDSR
141700000000     C/EJECT
141800000000     C************************************************************
141900160504     C* Aggiorno file ND77R00F  da sfl - dettaglio
142000000000     C************************************************************
142100000000     C     RieANA        BEGSR
142200000000     C*
142300160503     C                   CLEAR                   ND77R000
142400160601     C*
142500160620R218 X***                Eval      R77EVE = C1Eve
142600160620R218 X***                Eval      R77CDFS= C1CDFS
142700160620R218 C                   Eval      R77EVE = S1Eve
142800160620R218 C                   Eval      R77CDFS= S1CDFS
142900000000     C*
143000160503     C                   Eval      R77SOCIETA = XscSoc
143100160513     C                   Eval      R77ACF      = D1Acf
143200160513     C                   Eval      R77Qua      = H1Qua
143300160503     C                   Eval      $Meands     = S1Mac
143400160513     C                   Move      $Anno         R77Mac
143500160513     C                   Movel     $Mese         R77Mac
143600160503     C                   Eval      R77Rac      = S1Rac
143700160503     C                   Eval      R77Ref      = S1Ref
143800160503     C                   Eval      R77Ius      = S1Ius
143900160503     C                   Eval      R77Vec      = S1Vec
144000160503     C                   Eval      R77Cim      = S1Cim
144100160503     C                   Eval      R77Imv      = S1Imv
144200160503     C                   Eval      R77Iro      = S1Iro
144300160503     C                   If        S1Ravvedi  = '/'
144400160503     C                   Eval      R77Ravvedi  = *on
144500160503     C                   Endif
144600160503     C                   Eval      R77Note     = S1Note
144700160503     C                   Eval      R77Tri      = S1Tri
144800160511     C                   If        S1Teso     = '/'
144900160503     C                   Eval      R77Teso     = *on
145000160503     C                   Endif
145100160503     C*
145200160513     C                   If        s1dqv <> 0
145300160513     C     *JobRun       Move      s1Dqv         R77Dqv
145400160513     C                   Endif
145500160513     C*
145600160603     C                   Eval      R77Nqv = S1Nqv
145700160513     C                   Eval      R77TPQ = H1Tpq
145800160513     C                   Eval      R77PTR = H1Ptr
145900160513     C                   Eval      R77REG = H1Reg
146000160603     C*
146100160603     C                   If        R77Qua = '  '
146200160603     C                   Eval      R77Qua = 'AU'
146300160603     C                   Endif
146400000000     C*
146500160503     C                   ENDSR
146600000000     C/EJECT
146700000000     C************************************************************
146800000000     C* CONTROLLO ABILITAZIONE TASTI
146900000000     C* INPUT
147000000000     C* $TASTO =Tasto funzione premuto
147100000000     C* $FC = Tasti abilitati
147200000000     C* OUTPUT
147300000000     C* IN99 = ERrore
147400000000     C************************************************************
147500000000     C     CTRKEY        BEGSR
147600000000     C*
147700000000     C                   SETOFF                                       99
147800000000     C*
147900000000     C                   SELECT
148000000000     C     $TASTO        WHENEQ    F01
148100000000     C                   z-add     01            posfun
148200000000     C     $FC(1)        IFEQ      '0'
148300000000     C                   SETON                                        9997
148400000000     C                   ENDIF
148500000000     C     $TASTO        WHENEQ    F02
148600000000     C                   z-add     02            posfun
148700000000     C     $FC(2)        IFEQ      '0'
148800000000     C                   SETON                                        9997
148900000000     C                   ENDIF
149000000000     C     $TASTO        WHENEQ    F03
149100000000     C                   z-add     03            posfun
149200000000     C     $FC(3)        IFEQ      '0'
149300000000     C                   SETON                                        9997
149400000000     C                   ENDIF
149500000000     C     $TASTO        WHENEQ    F04
149600000000     C                   z-add     04            posfun
149700000000     C     $FC(4)        IFEQ      '0'
149800000000     C                   SETON                                        9997
149900000000     C                   ENDIF
150000000000     C     $TASTO        WHENEQ    F05
150100000000     C                   z-add     05            posfun
150200000000     C     $FC(5)        IFEQ      '0'
150300000000     C                   SETON                                        9997
150400000000     C                   ENDIF
150500000000     C     $TASTO        WHENEQ    F06
150600000000     C                   z-add     06            posfun
150700000000     C     $FC(6)        IFEQ      '0'
150800000000     C                   SETON                                        9997
150900000000     C                   ENDIF
151000000000     C     $TASTO        WHENEQ    F07
151100000000     C                   z-add     07            posfun
151200000000     C     $FC(7)        IFEQ      '0'
151300000000     C                   SETON                                        9997
151400000000     C                   ENDIF
151500000000     C     $TASTO        WHENEQ    F08
151600000000     C                   z-add     08            posfun
151700000000     C     $FC(8)        IFEQ      '0'
151800000000     C                   SETON                                        9997
151900000000     C                   ENDIF
152000000000     C     $TASTO        WHENEQ    F09
152100000000     C                   z-add     09            posfun
152200000000     C     $FC(09)       IFEQ      '0'
152300000000     C                   SETON                                        9997
152400000000     C                   ENDIF
152500000000     C     $TASTO        WHENEQ    F10
152600000000     C                   z-add     10            posfun
152700000000     C     $FC(10)       IFEQ      '0'
152800000000     C                   SETON                                        9997
152900000000     C                   ENDIF
153000000000     C     $TASTO        WHENEQ    F11
153100000000     C                   z-add     11            posfun
153200000000     C     $FC(11)       IFEQ      '0'
153300000000     C                   SETON                                        9997
153400000000     C                   ENDIF
153500000000     C     $TASTO        WHENEQ    F12
153600000000     C                   z-add     12            posfun
153700000000     C     $FC(12)       IFEQ      '0'
153800000000     C                   SETON                                        9997
153900000000     C                   ENDIF
154000000000     C     $TASTO        WHENEQ    F13
154100000000     C                   z-add     13            posfun
154200000000     C     $FC(13)       IFEQ      '0'
154300000000     C                   SETON                                        9997
154400000000     C                   ENDIF
154500000000     C     $TASTO        WHENEQ    F14
154600000000     C                   z-add     14            posfun
154700000000     C     $FC(14)       IFEQ      '0'
154800000000     C                   SETON                                        9997
154900000000     C                   ENDIF
155000000000     C     $TASTO        WHENEQ    F15
155100000000     C                   z-add     15            posfun
155200000000     C     $FC(15)       IFEQ      '0'
155300000000     C                   SETON                                        9997
155400000000     C                   ENDIF
155500000000     C     $TASTO        WHENEQ    F16
155600000000     C                   z-add     16            posfun
155700000000     C     $FC(16)       IFEQ      '0'
155800000000     C                   SETON                                        9997
155900000000     C                   ENDIF
156000000000     C     $TASTO        WHENEQ    F17
156100000000     C                   z-add     17            posfun
156200000000     C     $FC(17)       IFEQ      '0'
156300000000     C                   SETON                                        9997
156400000000     C                   ENDIF
156500000000     C     $TASTO        WHENEQ    F18
156600000000     C                   z-add     18            posfun
156700000000     C     $FC(18)       IFEQ      '0'
156800000000     C                   SETON                                        9997
156900000000     C                   ENDIF
157000000000     C     $TASTO        WHENEQ    F19
157100000000     C                   z-add     19            posfun
157200000000     C     $FC(19)       IFEQ      '0'
157300000000     C                   SETON                                        9997
157400000000     C                   ENDIF
157500000000     C     $TASTO        WHENEQ    F20
157600000000     C                   z-add     20            posfun
157700000000     C     $FC(20)       IFEQ      '0'
157800000000     C                   SETON                                        9997
157900000000     C                   ENDIF
158000000000     C     $TASTO        WHENEQ    F21
158100000000     C                   z-add     21            posfun
158200000000     C     $FC(21)       IFEQ      '0'
158300000000     C                   SETON                                        9997
158400000000     C                   ENDIF
158500000000     C     $TASTO        WHENEQ    F22
158600000000     C                   z-add     22            posfun
158700000000     C     $FC(22)       IFEQ      '0'
158800000000     C                   SETON                                        9997
158900000000     C                   ENDIF
159000000000     C     $TASTO        WHENEQ    F23
159100000000     C                   z-add     23            posfun
159200000000     C     $FC(23)       IFEQ      '0'
159300000000     C                   SETON                                        9997
159400000000     C                   ENDIF
159500000000     C     $TASTO        WHENEQ    F24
159600000000     C                   z-add     24            posfun
159700000000     C     $FC(24)       IFEQ      '0'
159800000000     C                   SETON                                        9997
159900000000     C                   ENDIF
160000000000     C                   ENDSL
160100000000     C*
160200000000     C                   ENDSR
160300000000     C/EJECT
160400000000     C************************************************************
160500000000     C* DEFINIZIONE DELL'ORDINAMENTO
160600000000     C************************************************************
160700000000     C     DEFORD        BEGSR
160800000000     C*
160900000000     C* definisco l'ordinamento della lista a seconda dei valori
161000000000     C* impostati nel video di parzializzazione
161100000000     C*
161200160503     C**                 SELECT
161300000000      *  nel caso di più viste logiche e quindi più campi di
161400000000      *  parzializzazione, qui vanno definiti i criteri per
161500000000      *  determinare quale VL va letta, a seconda di quali campi
161600000000      *  sono stati valorizzati
161700000000     C                   Z-ADD     1             $ORD
161800000000     C* imposto l'ordinamento 99 per ottenere in selezione solo il
161900000000     C* rcd con la chiave passata
162000000000     C*    $Unique       WHENEQ    *ON
162100000000     C*                  Z-ADD     99            $ORD
162200000000     C* nel caso non sia stato digitato alcun valore in parzializzazione
162300000000     C* utilizzerò l'ordinamento precedentemente memorizzato
162400000000     C* (temporaneamente o definitivamente) nei valori assunti
162500160511     C*                  OTHER
162600160511     C*                  ENDSL
162700000000     C                   ENDSR
162800000000     C/EJECT
162900000000     C************************************************************
163000000000     C* OPERAZIONI INIZIALI
163100000000     C************************************************************
163200000000     C     *INZSR        BEGSR
163300000000     C*
163400000000     C* Reperimento parametri
163500000000     C*
163600000000     C     *ENTRY        PLIST
163700000000     C                   PARM                    KPJBA
163800000000     C                   PARM                    DSAUT
163900000000     C*
164000160503     C                   Eval      Sec20=20
164100000000     C* Reperimento dati società
164200000000     C*
164300000000     C                   MOVEL     'SOC001'      TIPXSC
164400000000     C                   MOVEL     *BLANK        SOCXSC
164500000000     C                   EXSR      REPSOC
164600000000     C*
164700000000     C* Controlli dati società
164800000000     C*
164900000000     C     RTNXSC        IFNE      '1'
165000000000     C                   MOVEL     XSOCDS        SOC001
165100000000     C                   ELSE
165200160603     C                   MOVE      '1'           ErrK03
165300000000     C                   MOVE      *On           $Fine
165400000000     C                   EXSR      ENDPGM
165500000000     C                   ENDIF
165600000000     C*
165700000000     C* testo se sono stati passati i dati di autorizzazione
165800000000     C     PARMS         IFLT      2
165900000000     C                   MOVE      *BLANK        AUTDS
166000000000     C                   ELSE
166100000000     C                   MOVEL     DSAUT         AUTDS
166200000000     C                   ENDIF
166300000000     C*
166400000000     C* Controlli dati società
166500000000     C                   IF        XSOCDS <> *BLANKS
166600000000     C                   MOVEL     XSOCDS        SOC001
166700000000     C                   ENDIF
166800000000      *
166900000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
167000000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
167100000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
167200000000     C                   Eval      $S1L01 = %Size(Z1Ke1)
167300000000     C                   Eval      $S1L02 = %Size(Z1Ke2)
167400000000     C*
167500000000     C* Reperimento tasti di funzione
167600000000     C*
167700000000     C                   EXSR      REPKEY
167800000000     C*
167900000000     C                   Z-add     1             OPZLEN
168000000000     C*
168100000000     C* Attiva Commitment control se necessario
168200000000     C*
168300160513     C                   CALLB     'XSTRCMT'
168400160504     C                   OPEN      ND77R00F
168500000000     C*
168600000000     C                   ENDSR
168700000000     C/EJECT
168800000000     C************************************************************
168900000000     C* INIZIALIZZAZIONE VARIABILI
169000000000     C************************************************************
169100000000     C     INZVAR        BEGSR
169200000000     C*
169300000000     C* Dati da PGM chiamante
169400000000     C*
169500160603     C                   MOVEL     KPJBU         SDGSK03DS
169600000000     C*
169700000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
169800000000     C* reperimento dati società
169900160603     C                   If        SocietaK03 <> XScSoc    and
170000160603     C                             OPZK03 <> *blank
170100000000     C                   MOVEL     'SOC001'      TIPXSC
170200160603     C                   MOVEL     SocietaK03    SOCXSC
170300000000     C                   EXSR      REPSOC
170400000000     C     RTNXSC        IFNE      '1'
170500000000     C                   MOVEL     XSOCDS        SOC001
170600000000     C                   ELSE
170700160603     C                   MOVE      '1'           ERRK03
170800000000     C                   MOVE      *On           $Fine
170900000000     C                   EXSR      ENDPGM
171000000000     C                   ENDIF
171100000000     C                   endif
171200000000     C*
171300000000     C* Reperimento autorizzazioni
171400000000     C*
171500000000      *  nel caso l'anagrafica non gestisse le autorizzazioni
171600000000      *   asteriscare la chiamata alla subroutine REPAUT
171700000000     C* le autorizzazioni potrebbero essere già state passate,
171800000000     C* nel qual caso non vanno reperite
171900000000     C**** AUTDS         IFEQ      *BLANK
172000000000     C****               EXSR      REPAUT
172100000000     C****               ELSE
172200000000     C*****              MOVEL     XCADDS        ------DS
172300000000     C****               ENDIF
172400000000     C*
172500000000     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
172600000000     C* le autorizzazione
172700000000     C*
172800000000      *  nel caso l'anagrafica non gestisse le autorizzazioni
172900000000      *   asteriscare la chiamata alla subroutine TSTAUT
173000000000     C******             EXSR      TSTAUT
173100000000     C*
173200000000     C* Variabili per gestione videate
173300000000     C*
173400000000     C                   MOVE      'D1'          $GEST                          fmt in gestione
173500000000     C                   MOVE      *On           $INZD1                         inizializzione D1
173600000000     C                   MOVE      *On           $InzS1                         fmt in gestione
173700000000     C                   MOVE      *OFF          $FINE                          fine pgm
173800000000     C*                  MOVE      *OFF          $FlgDef                        fine pgm
173900000000     C                   MOVE      *BLANK        $LASTV                         ultima emissione
174000000000     C*
174100160512     C* Subfile aperto
174200160512     C                   Eval      C1Mode      = *Off
174300000000     C*
174400000000     C* Variabili appoggio
174500000000     C*
174600000000     C                   Z-ADD     0             CURR
174700000000     C                   Z-ADD     0             CURC
174800000000     C*
174900000000     C* Valorizzazione campi univoci testate
175000000000     C*
175100000000     C                   CLEAR                   FMTT1
175200000000     C                   MOVEL     KNSIF         NOMSIF
175300000000     C                   MOVEL     XSCDSI        NOMDIT
175400000000     C                   MOVEL     KCDAZ         NOMAZN
175500000000     C* recupero il tipo operazione
175600000000     C                   Z-ADD     1             $X
175700160603     C     OPZK03        LOOKUP    $OZ($X)                                21
175800000000     C     *In21         IFEQ      *ON
175900000000     C                   MOVEL     $ID($X)       IDMSG
176000000000     C                   CALLB     'XRTVM'
176100000000     C                   PARM                    IDMSG            27
176200000000     C                   PARM                    TXTMSG          100
176300000000     C                   PARM                    ERRMSG            1
176400000000     C                   PARM      *ON           CTRMSG            1            centratura
176500000000     C                   PARM      30            LENMSG            3 0          lun output.
176600000000     C     ERRMSG        IFNE      *ON
176700000000     C***                MOVEL     TXTMSG        T1TIT
176800000000     C                   ELSE
176900000000     C***                MOVEL     *ALL'?'       T1TIT
177000000000     C                   ENDIF
177100000000     C                   ENDIF
177200160429     C*
177300160429     C                   MOVEL     *BLANK        T1TIT
177400000000     C*
177500000000     C                   ENDSR
177600000000     C/EJECT
177700000000     C************************************************************
177800000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
177900000000     C************************************************************
178000000000     C     REPAUT        BEGSR
178100000000     C*
178200000000     C                   CLEAR                   AUTDS
178300000000     C*
178400000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
178500000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
178600000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
178700000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
178800000000     C                   MOVEL     '------'      XCAFNC                         FUNZIONE
178900000000     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
179000000000     C                   MOVE      'CK'          XCATCK                         TIPO CTRL
179100000000     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
179200000000     C*
179300000000     C                   CALLB     'XCHKAUT'
179400000000     C                   PARM                    AUTDS
179500000000     C*
179600000000     C* errori/non abilitazione
179700000000     C     XCARTN        IFNE      0
179800160603     C                   MOVE      '2'           ErrK03
179900000000     C                   MOVE      *ON           $FINE
180000000000     C                   EXSR      ENDPGM
180100000000     C                   ELSE
180200000000     C********           MOVEL     XCADDS        ------DS
180300000000     C                   ENDIF
180400000000     C*
180500000000     C                   ENDSR
180600000000     C/EJECT
180700000000     C************************************************************
180800000000     C* REPERIMENTO DATI SOCIETA'
180900000000     C************************************************************
181000000000     C     REPSOC        BEGSR
181100000000     C*
181200000000     C                   CALL      'SDGSOCDR'
181300000000     C                   PARM                    TIPXSC            6
181400000000     C                   PARM                    SOCXSC            3
181500000000     C                   PARM                    CDSXSC            9 0
181600000000     C                   PARM                    MODXSC            3
181700000000     C                   PARM      *BLANKS       RTNXSC            1
181800000000     C                   PARM                    XSOCDS
181900000000     C                   PARM                    KPJBA
182000000000     C*
182100000000     C                   ENDSR
182200000000     C/EJECT
182300000000     C************************************************************
182400000000     C* REPERIMENTO TASTI DI FUNZIONE
182500000000     C************************************************************
182600000000     C     REPKEY        BEGSR
182700000000     C                   MOVEA     $FM1          $FM
182800000000     C                   MOVEA     $FC1          $FC
182900000000     C                   EXSR      REPKKK
183000000000     C                   MOVEA     $FX           $FX1
183100000000     C*
183200000000     C                   MOVEA     $FM2          $FM
183300000000     C                   MOVEA     $FC2          $FC
183400000000     C                   EXSR      REPKKK
183500000000     C                   MOVEA     $FX           $FX2
183600000000     C*
183700000000     C                   ENDSR
183800000000     C/EJECT
183900000000     C************************************************************
184000000000     C* REPERIMENTO TASTI DI FUNZIONE
184100000000     C************************************************************
184200000000     C     REPKKK        BEGSR
184300000000     C*
184400000000     C                   CLEAR                   $FX
184500000000     C     1             DO        24            $X
184600000000     C*
184700000000     C     $FM($X)       IFNE      *BLANK
184800000000     C     $Fc($X)       andne     '0'
184900000000     C*
185000000000     C                   MOVEL     $FM($X)       IDMSG
185100000000     C                   CALL      'XRTVM'
185200000000     C                   PARM                    IDMSG            27
185300000000     C                   PARM                    TXTMSG          100
185400000000     C                   PARM                    ERRMSG            1
185500000000     C*
185600000000     C     ERRMSG        IFNE      *ON
185700000000     C                   MOVEL     TXTMSG        $FX($X)
185800000000     C                   ELSE
185900000000     C                   MOVEL     *ALL' '       $FX($X)
186000000000     C                   ENDIF
186100000000     C*
186200000000     C                   ENDIF
186300000000     C*
186400000000     C                   ENDDO
186500000000     C*
186600000000     C                   ENDSR
186700000000     C/EJECT
186800000000     C************************************************************
186900000000     C* DEFINIZIONE KLIST
187000000000     C************************************************************
187100000000     C     DefKlist      BEGSR
187200000000     C*
187300000000     C* klist
187400000000     C*
187500160503     C* nd77r01l
187600160603     C     K0277R01      KLIST
187700160503     C                   KFLD                    R77Societa
187800160503     C                   KFLD                    R77Acf
187900000000     C*
188000000000     C                   ENDSR
188100000000     C/EJECT
188200000000** $OZ / $ID
18830000000001PROMSG    *LIBL     COS0001
18840000000002PROMSG    *LIBL     COS0002
18850000000003PROMSG    *LIBL     COS0003
18860000000004PROMSG    *LIBL     COS0004
18870000000005PROMSG    *LIBL     COS0005
188800000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
188900000000PROMSG    *LIBL     KEY0001  01
189000000000PROMSG    *LIBL     KEY0002  02
189100000000PROMSG    *LIBL     KEY0003  03
189200000000PROMSG    *LIBL     KEY0004  04
189300000000PROMSG    *LIBL     KEY0005  05
189400000000PROMSG    *LIBL     KEY0006  06
189500000000PROMSG    *LIBL     KEY0007  07
189600000000PROMSG    *LIBL     KEY0008  08
189700000000PROMSG    *LIBL     KEY0009  09
189800000000PROMSG    *LIBL     KEY0010  10
189900000000PROMSG    *LIBL     KEY0011  11
190000000000PROMSG    *LIBL     KEY0012  12
190100000000PROMSG    *LIBL     KEY0013  13
190200000000PROMSG    *LIBL     KEY0014  14
190300000000PROMSG    *LIBL     KEY0015  15
190400000000PROMSG    *LIBL     KEY0016  16
190500000000PROMSG    *LIBL     KEY0017  17
190600000000PROMSG    *LIBL     KEY0018  18
190700000000PROMSG    *LIBL     KEY0019  19
190800000000PROMSG    *LIBL     KEY0020  20
190900000000PROMSG    *LIBL     KEY0021  21
191000000000PROMSG    *LIBL     KEY0022  22
191100000000PROMSG    *LIBL     KEY0023  23
191200000000PROMSG    *LIBL     KEY0024  24
191300000000**  ABILITAZIONE D1 : $FC1 / $FR1
1914000000000N  01
1915000000001N  02
1916000000001N  03
1917000000001N  04
1918000000000N  05
1919000000000N  06
1920000000000N  07
1921000000000N  08
1922000000000N  09
1923000000000N  10
1924000000000N  11
1925000000001N  12
1926000000000N  13
1927000000000N  14
1928000000000N  15
1929000000000N  16
1930000000000N  17
1931000000000N  18
1932000000000N  19
1933000000000N  20
1934000000000N  21
1935000000000N  22
1936000000000N  23   Impostare a '1' se si utilizzano opzioni
1937000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
193800000000** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
193900000000PROMSG    *LIBL     KEY0001  01
194000000000PROMSG    *LIBL     KEY0002  02
194100000000PROMSG    *LIBL     KEY0003  03
194200000000PROMSG    *LIBL     KEY0004  04
194300000000PROMSG    *LIBL     KEY0005  05
194400000000PROMSG    *LIBL     KEY0006  06
194500000000PROMSG    *LIBL     KEY0114  07
194600000000PROMSG    *LIBL     KEY0008  08
194700000000PROMSG    *LIBL     KEY0009  09
194800000000PROMSG    *LIBL     KEY0010  10
194900000000PROMSG    *LIBL     KEY0011  11
195000000000PROMSG    *LIBL     KEY0012  12
195100000000PROMSG    *LIBL     KEY0013  13
195200000000PROMSG    *LIBL     KEY0014  14
195300000000PROMSG    *LIBL     KEY0015  15
195400000000PROMSG    *LIBL     KEY0016  16
195500000000PROMSG    *LIBL     KEY0017  17
195600000000PROMSG    *LIBL     KEY0018  18
195700000000PROMSG    *LIBL     KEY0019  19
195800000000PROMSG    *LIBL     KEY0020  20
195900000000PROMSG    *LIBL     KEY0021  21
196000000000PROMSG    *LIBL     KEY0022  22
196100000000PROMSG    *LIBL     KEY0023  23
196200000000PROMSG    *LIBL     KEY0024  24
196300000000**  ABILITAZIONE S1 : $FC2 / $FR2
1964000000000N  01
1965000000000N  02
1966000000001N  03
1967000000001N  04
1968000000001N  05
1969000000001N  06
1970000000000N  07
1971000000001N  08
1972000000001N  09
1973000000000N  10
1974000000000N  11
1975000000001N  12
1976000000000N  13
1977000000000N  14
1978000000000N  15
1979000000000N  16
1980000000000N  17
1981000000000N  18
1982000000000N  19
1983000000000N  20
1984000000000N  21
1985000000000N  22
1986000000000N  23   Impostare a '1' se si utilizzano opzioni
1987000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
