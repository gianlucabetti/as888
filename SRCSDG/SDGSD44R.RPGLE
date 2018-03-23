000100160705     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200160705     H*PARMS CVTOPT(*NONE)
000300160705     H DECEDIT('0,') DATEDIT(*DMY.)
000400160705      *
000500160705     >*            Modifica codice soggetto
000600160705      *           ---------------------------------------
000700160705      *
000701160708R2322A* Pgm creato con le specifiche R2322
000800160705      *
004100160705     F*----------------------------------------------------*
005300160705     F*------------
005400160705     F* legami registrazioni contabili di bolle e fatture
005401160705     FSDGLRC01L UF A E           K DISK
008200160705     F*------------
008300160711    >FSDGSD44D  CF   E             WORKSTN USROPN
008400160705    >F                                     RENAME(SD44D1:FMTD1)
008500160705    >F                                     RENAME(SD44Z1:FMTZ1)
008600160705    >F                                     RENAME(SD44Z3:FMTZ3)
008700160705     F                                     INFDS(DSFMT)
008800160705     D*----------------------------------------------------*
008900160705     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
009000160705     D $ID             S             27    DIM(5) ALT($OZ)
009100160705     D* Schiere per la gestione dei tasti di funzione
009200160705     D $FM             S             27    DIM(24)
009300160705     D $FX             S             50    DIM(24)
009400160705     D $FC             S              1    DIM(24)
009500160705     D $FR             S              1    DIM(24)
009600160705     D* Video D1
009700160705     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
009800160705     D $FX1            S             50    DIM(24) ALT($FM1)
009900160705     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
010000160705     D $FR1            S              1    DIM(24) ALT($FC1)
010100160705     D* Schiera contente i nomi dei programmi da richiamare con i tasti
010200160705     D* funzione personalizzati
010300160705     D $PGF1           S             10    DIM(25)
010400160705     D*----------------------------------------------------*
010500160705     D $D1L01          S              3  0
010600160705     D $D1L02          S              3  0
010700160705    > * Inserire qui la definizione di altre variabili per reperire
010800160705    > * la lunghezza di altre videate che usano un piede diverso da
010900160705    > * STD
011000160705     D*-------------
011100160705     D* Passaggio Parametri
011200160705     D KPJBA         E DS
011300160705     D*-------------
011400160705     D* DS Esterna gestione messaggi
011401160707     D SDGMSGDS      E DS                  INZ
011600160705     D  STS          E                     EXTFLD(MSGSTS)
011700160705     D                                     DIM(333)
011800160705     D  JBA          E                     EXTFLD(MSGJBA)
011900160705     D                                     DIM(502)
012900160705     D*-------------
013000160705     D* Reperimento nome PGM
013100160705     D STATUS         SDS           333
013200160705     D  DSPGM            *PROC
013300160705     D  PARMS            *PARMS
013400160705     D*-------------
013500160707     D* Dati di ambiente ottenuti da SDGSOCDR
013501160707     D SOC001        E DS                  EXTNAME(SDGSOCDS )
013700160705     D*-------------
013701160707     D* DS Interna per dati di output di SDGSOCDR
013900160705     D XSOCDS          DS          1000
014000160705     D*-------------
014100160705     D* parametri in ricezione
014200160705    >D SDGSD44DS     E DS
014300160705     D*-------------
014400160705     D* Parametri per ricerca soggetto
014500160705     D ANA040DS      E DS
014600160705     D*-------------
014700160705     D* DS per ricezione dati anagrafici per clienti/fornitori
014800160705     D DVASOG        E DS
014900160705     D*-------------
015000160705     D* Definizioni parametri driver soggetti
015100160705     DDvaric           S              1
015200160705     DDvaSocieta       S              3
015300160705     DDvaUnita         S              8
015400160705     DDvaDtRif         S               D
015500160705     DDvaStrutt        S             10
015600160705     DDvaLenOut        S              9B 0
015700160705     DDvaSnatura       S              1
015800160705     DDvaCodice        S              8
015900160705     DDvaLineav        S              3
016000160705     DDvaFiliale       S              3  0
016100160705     DDvaSogg          S              8
016200160705     DDvaTpInd         S              2
016300160705     DDvaCdInd         S              3
016400160705     DDvaOutput        S           4000
016500160705     DDvaErrore        S              1
016600160705     D*-------------
016700160705     D* campo di salvataggio per la DS d'ingresso durante la chiamata
016800160705     D* alla funzione personalizzata
016900160705    >D $SaveDS         S                   like(SDGSD44DS)
017000160705     D*-------------
017100160705     D DSFMT           DS
017200160705     D  $TASTO               369    369
017300160705     D  NRG                  370    370
017400160705     D  NCL                  371    371
017800160705     D*-------------
017900160705     D* posizione cursore
018000160705     D CURSOR          DS
018100160705     D  RIRI                   1      2B 0 INZ
018200160705     D  R$$                    2      2
018300160705     D  COCO                   3      4B 0 INZ
018400160705     D  C$$                    4      4
018500160705     D*-------------
018600160705     D* Costanti
018700160705     D*-------------
018800160705     D* Tasti di funzione
018900160705     D F01             C                   CONST(X'31')
019000160705     D F02             C                   CONST(X'32')
019100160705     D F03             C                   CONST(X'33')
019200160705     D F04             C                   CONST(X'34')
019300160705     D F05             C                   CONST(X'35')
019400160705     D F06             C                   CONST(X'36')
019500160705     D F07             C                   CONST(X'37')
019600160705     D F08             C                   CONST(X'38')
019700160705     D F09             C                   CONST(X'39')
019800160705     D F10             C                   CONST(X'3A')
019900160705     D F11             C                   CONST(X'3B')
020000160705     D F12             C                   CONST(X'3C')
020100160705     D F13             C                   CONST(X'B1')
020200160705     D F14             C                   CONST(X'B2')
020300160705     D F15             C                   CONST(X'B3')
020400160705     D F16             C                   CONST(X'B4')
020500160705     D F17             C                   CONST(X'B5')
020600160705     D F18             C                   CONST(X'B6')
020700160705     D F19             C                   CONST(X'B7')
020800160705     D F20             C                   CONST(X'B8')
020900160705     D F21             C                   CONST(X'B9')
021000160705     D F22             C                   CONST(X'BA')
021100160705     D F23             C                   CONST(X'BB')
021200160705     D F24             C                   CONST(X'BC')
021300160705     D ENTER           C                   CONST(X'F1')
021400160705     D ROLDWN          C                   CONST(X'F4')
021500160705     D ROLLUP          C                   CONST(X'F5')
022000160705     D*-------------
022100160705     D* Variabili per gestione videate
022200160705     D $Gest           S              2A
022300160705     D $Fine           S              1A
022400160705     D $InzD1          S              1A
022500160705     D $LastG          S              2A
022600160705     D $LastV          S              2A
022700160705     D $FlgDef         S              1A
022800160705     D*-------------
022900160705     D* Variabili appoggio sempre presenti in tutte le anagrafiche
023000160705     D CurR            S              2S 0
023100160705     D CurC            S              2S 0
023200160705     D Win             S             99A
023300160705     D*-------------
023400160705     D* Variabili appoggio specifiche della singola anagrafica
023500160705    > * elenco tutti i campi di appoggio default
023600160705     D*  Indici
023700160705     D X               S              3S 0
023800160705     D Y               S              3S 0
023900160705     D* Indice della schiera tasti funzione
024000160705     D PosFun          S              2  0
024001160705     D*-------------
024018160705     D* campi di lavoro
024101160705     D ZdateIso        s               D
024200160705     C*----------------------------------------------------*
024300160705     C*                MAIN LINE PROGRAM
024400160705     C*----------------------------------------------------*
024500160705     C*
024700160705     C                   EXSR      INZVAR
024800160705     C*
024900160705     C     $FINE         DOWEQ     *OFF
025000160705     C     $GEST         CASEQ     'D1'          GESD1
025100160705     C                   ENDCS
025200160705     C                   ENDDO
025300160705     C*
025500160705     C                   EXSR      ENDPGM
025600160705     C/EJECT
025700160705     C************************************************************
025800160705     C* Fine Programma
025900160705     C************************************************************
026000160705     C     ENDPGM        BEGSR
026300160705     C*
026400160705     C* se ho confermato l'uscita proseguo
026500160705     C     $FINE         IFEQ      *ON
026600160705     C*
026700160705     C* passaggio dati al pgm chiamante
026800160705    >C                   MOVEL     SDGSD44DS     KPJBU
026900160705     C*
027000160705     C                   SETON                                            RT
027100160705     C                   RETURN
027200160705     C*
027300160705     C                   ENDIF
027400160705     C*
027500160705     C                   ENDSR
027600160705     C/EJECT
027700160705     C************************************************************
027800160705     C* RICERCHE
027900160705     C************************************************************
028000160705     C     SEARCH        BEGSR
028100160705     C*
028200160705     C* determino Riga/Colonna del cursore
028300160705     C                   MOVE      NRG           R$$
028400160705     C                   MOVE      NCL           C$$
028500160705     C                   Z-ADD     RIRI          CURR
028600160705     C                   Z-ADD     COCO          CURC
028700160705     C*
028800160705     C                   SELECT
028900160705     C*
029000160705      *--------------------------------------------------------------
029100160705      * inserire le routine di ricerca
029200160705      * RICORDATE: per tutti i campi che in certe condizioni risultano PR,
029300160705      * NON deve essere possibile richiamare i rispettivi pgm in ricerca,
029400160705      * SEMMAI richiamarli in visualizzazione (opzione=05)
029500160705      *--------------------------------------------------------------
029600160705     C** FMTD1 - cd soggetto sostituente
029700160705     C     H1NMFL        WHENEQ    'D1SOGG2'
029800160705     C                   RESET                   ANA040DS
029900160705     C                   MOVEL     '01'          OPZ040
030000160705     C                   MOVEL     ANA040DS      KPJBU
030100160705     C                   CALL      'ANA040R'
030200160705     C                   PARM                    KPJBA
030300160705     C                   MOVEL     KPJBU         ANA040DS
030400160705     C*
030500160705     C     Cod040        IFNE      *BLANK
030600160705     C                   MOVEL     Cod040        D1Sogg2
030700160705     C                   ENDIF
030800160705     C*
031000160705     C* ricerca non abilitata in questa posizione
031100160705     C                   OTHER
031200160705     C                   SETON                                        9899
031300160705     C*
031400160705     C                   ENDSL
031500160705     C*
031600160705     C* imposto pos. del cursore
031700160707     C                   Eval      H1RIGA = CURR - 2
031800160707     C                   Eval      H1Colo = CURC - 11 - 1
031900160705     C*
032000160705     C                   MOVE      *HIVAL        $LASTV
032100160705     C*
032200160705     C                   ENDSR
032300160705     C/EJECT
032400160705     C************************************************************
032500160705     C* GESTIONE EMISSIONE TASTI FUNZIONE
032600160705     C************************************************************
032700160705     C     GESPIE        BEGSR
032800160705     C*
032900160705     C                   SELECT
033000160705     C*
033100160705     C     $GEST         WHENEQ    'D1'
033200160705     C     $ULKD1        IFEQ      0
033300160705     C* riempimento opzioni
033400160705     C                   EXSR      F24D1
033500160705     C                   ELSE
033600160705     C                   WRITE     FMTZ1
033700160705     C                   ENDIF
033800160705     C*
033900160705     C                   ENDSL
034000160705     C*
034100160705     C                   ENDSR
034200160705     C/EJECT
034300160705     C************************************************************
034400160705     C* Chiamata programma con tasto personalizzato
034500160705     C************************************************************
034600160705     C     FPerD1        BEGSR
034700160705     C*
034800160705     C* salvo la DS di ingresso
034900160705    >C                   MOVEL     SDGSD44DS     $SaveDS
035000160705     C*
035100160705    >C                   MOVE      d1sogg1       SoggD44
035200160705     C* passare nei campi appositi della DS SDGSD44DS il valore
035300160705     C* della chiave della VL univoca
035400160705     C*
035500160705    >C                   MOVEL     SDGSD44DS     KPJBU
035600160705    >C                   CALL      $PGF1(PosFun)                        22
035700160705     C                   PARM                    KPJBA
035800160705     C*
035900160705     C* ripristino la DS di ingresso
036000160705    >C                   MOVEL     $SaveDS       SDGSD44DS
036100160705     C*
036200160705     C                   ENDSR
036300160705     C/EJECT
036400160705     C************************************************************
036500160705     C* GESTIONE VIDEO RECORD D1
036600160705     C************************************************************
036700160705     C     GESD1         BEGSR
036701160705     C*
036800160705     C* inizializzazione videata
036900160705   B1C     $INZD1        IFEQ      *ON
037000160705     C                   EXSR      INZD1
037100160705     C                   MOVE      *OFF          $INZD1
037200160705   E1C                   ENDIF
037201160705     C*
037300160705     C* emissione piede videata se proveniente da altra
037400160705     C* salvataggio valore formato di provenienza
037500160705   B1C     $LASTV        IFNE      'D1'
037600160705     C*********          WRITE     FMTT1
037700160705     C                   EXSR      GESPIE
037800160705     C                   MOVE      $LASTV        $LASTG
037900160705     C                   MOVE      'D1'          $LASTV
038000160705   E1C                   ENDIF
038100160705     C*
038200160705     C* se esistono errori sulla videata
038300160705     C* emetto la write del formato a indicatori spenti per vedere
038400160705     C* le eventuali decodifiche
038500160705   B1C     *IN99         IFEQ      *ON
038600160705     C     $GEST         ANDEQ     'D1'
038700160705     C                   MOVEA     *IN           WIN
038800160705     C                   MOVE      *ALL'0'       IN5098           49
038900160705     C                   MOVEA     IN5098        *IN(50)
039000160705     C                   WRITE     FMTD1
039100160705     C                   MOVEA     WIN           *IN
039200160705   E1C                   ENDIF
039201160705     C*
039300160705     C* Inizializzazione indice tasti funzione personalizzati
039400160705     C                   Eval      PosFun = 25
039500160705     C*
039600160705     C                   EXFMT     FMTD1
039700160705     C*
039800160705     C* azzero la posizione del cursore
039900160705     C                   Z-ADD     0             H1RIGA
040000160705     C                   Z-ADD     0             H1COLO
040100160705     C*
040200160705     C* controllo abilitazione tasti funzione
040300160705     C                   EXSR      CTRKD1
040400160705     C*
040500160705   B1C     *IN99         IFEQ      *OFF
040600160705     C*
040700160705   B2C                   SELECT
040800160705     C* F2=Decodifica
040900160705     C     $TASTO        WHENEQ    F02
041000160705     C                   EXSR      F02D1
041100160705     C* F3=Fine
041200160705     C     $TASTO        WHENEQ    F03
041300160705     C                   EXSR      F03D1
041400160705     C* F4=Ricerca
041500160705     C     $TASTO        WHENEQ    F04
041600160705     C                   EXSR      SEARCH
041700160705     C* F5=Refresh
041800160705     C     $TASTO        WHENEQ    F05
041900160705     C                   EXSR      F05D1
042000160705     C* F8=Successivo
042100160705     C     $TASTO        WHENEQ    F08
042200160705     C                   EXSR      F08D1
042300160705     C* F12=Ritorno
042400160705     C     $TASTO        WHENEQ    F12
042500160705     C                   EXSR      F12D1
042600160705     C* F24=Altri tasti
042700160705     C     $TASTO        WHENEQ    F24
042800160705     C                   EXSR      F24D1
042900160705     C* Personalizzata
043000160705     C     $PGF1(PosFun) WHENNE    *BLANK
043100160705     C                   EXSR      FPerD1
043200160705     C*
043300160705   X2C                   OTHER
043400160705     C                   EXSR      CTRD1
043500160705     C*
043600160705   B3C     $TASTO        IFEQ      F06
043700160705     C     *IN99         ANDEQ     *OFF
043701160705     C*
045700160705     C* aggiorno il legame della bolla senza fattura collegata
045800160705     C                   EXSR      AGGANA
045900160705     C* eseguo operazioni del dopo-aggiornamento
046000160705   B4C     *IN99         IFEQ      *OFF
046100160705     C                   EXSR      GESAGG
046200160705   E4C                   ENDIF
046300160705   E3C                   ENDIF
046400160705     C*
046500160705   E2C                   ENDSL
046600160705     C*
046700160705   E1C                   ENDIF
046800160705     C*
046900160705     C                   ENDSR
047000160705     C/EJECT
047100160705     C************************************************************
047200160705     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
047300160705     C************************************************************
047400160705     C     CTRKD1        BEGSR
047500160705     C*
047600160705     C                   SETOFF                                       99
047700160705     C                   MOVEA     $FC1          $FC
047800160705     C                   EXSR      CTRKEY
047900160705     C     *IN99         IFEQ      *ON
048000160705     C                   SETON                                        97
048100160705     C                   ENDIF
048200160705     C*
048300160705     C                   ENDSR
048400160705     C/EJECT
048500160705     C************************************************************
048600160705     C* INIZIALIZZAZIONE VIDEATA DATI
048700160705     C************************************************************
048800160705     C     INZD1         BEGSR
048900160705     C*
049000160705     C                   CLEAR                   FMTD1
049100160705     C* riempio la videata
049200160705     C                   EXSR      RIED1
049300160705     C* richiamo routine dei ctrl per decodificarla
049400160705     C                   EXSR      CTRD1
049500160705     C                   MOVE      *ALL'0'       IN5098
049600160705     C                   MOVEA     IN5098        *IN(50)
049700160705     C*
049800160705     C*
049900160705     C                   ENDSR
050000160705     C/EJECT
050100160705     C************************************************************
050200160705     C* RIEMPIMENTO VIDEATA  D1
050300160705     C************************************************************
050400160705     C     RIED1         BEGSR
050500160705     C*
051400160705     C                   Eval      D1Sogg1  =    SoggD44
051500160705     C*
051600160705     C*decodifica sogg.da sostituire
051700160705     C                   If        D1Sogg1 <>*blank
051800160705     C*
051900160705     C                   MOVE      '1'           DVARIC
052000160705     C                   MOVEL     'DVASOG'      dvastrutt
052100160705     C                   MOVE      societaD44    DVASOCIETA
052200160705     C                   MOVE      d1sogg1       DVASOGG
052300160705     C                   MOVE      *blank        DVATPIND
052400160705     C                   MOVE      *blank        DVACDIND
052500160705     C                   EVAL      DVALenOut = %Size(DVASOG)
052600160705     C                   CALLB     'NDDVASOG'    PDVASOG
052700160705   B C     DVAERRORE     IFEQ      *OFF
052800160705     C                   eval      %subst(DVASOG:1:DVALENOUT)
052900160705     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
053000160705     C*
053100160705     c                   eval      D1Des1     =  DVSDes
053200160705     c                   eval      D1DesAgg1  =  DVSDesAgg
053300160705     c                   eval      D1Indriz1  =  DVSIndriz
053400160705     c                   eval      D1Cap1     =  DVSCap
053500160705     c                   eval      D1Localit1 =  DVSLocalit
053600160705     c                   eval      D1Prov1    =  DVSProv
053700160705     c                   eval      D1Stato1   =  DVSStato
053800160705     C                   EndIf
053900160705     C                   EndIf
054000160705     C*
054100160705     C*
054200160705     C                   ENDSR
054300160705     C************************************************************
054400160705     C* RIEMPIMENTO CAMPI DAI DEFAULT
054500160705     C************************************************************
054600160705     C     RIEDEF        BEGSR
054700160705     C*
054800160705    > * riempire i campi con i default recuperati dal codice di
054900160705    > * riferimento
055000160705     C*
055100160705     C                   ENDSR
055200160705     C/EJECT
055300160705     C************************************************************
055400160705     C* GESTIONE F02 VIDEO D1
055500160705     C************************************************************
055600160705     C     F02D1         BEGSR
055700160705     C*
055800160705     C* per decodificare la videata richiamo la routine dei ctrl
055900160705     C* e poi disattivo gli eventuali messaggi di errore
056000160705     C                   EXSR      CTRD1
056100160705     C*
056200160705     C                   MOVE      *ALL'0'       IN5098
056300160705     C                   MOVEA     IN5098        *IN(50)
056400160705     C*
056500160705     C                   ENDSR
056600160705     C/EJECT
056700160705     C************************************************************
056800160705     C* GESTIONE F03 VIDEO D1
056900160705     C************************************************************
057000160705     C     F03D1         BEGSR
057100160705     C*
057200160705     C                   MOVE      *ON           $FINE
057300160705    >C                   MOVE      '1'           RETD44
057400160705     C* fine programma
057500160705     C                   EXSR      ENDPGM
057600160705     C*
057700160705     C                   ENDSR
057800160705     C/EJECT
057900160705     C************************************************************
058000160705     C* GESTIONE F05 VIDEO D1
058100160705     C************************************************************
058200160705     C     F05D1         BEGSR
058300160705     C*
058400160705     C                   MOVE      *ON           $INZD1
058500160705     C*
058600160705     C                   ENDSR
058700160705     C/EJECT
058800160705     C************************************************************
058900160705     C* GESTIONE F08 VIDEO D1
059000160705     C************************************************************
059100160705     C     F08D1         BEGSR
059200160705     C*
059300160705     C                   MOVE      *ON           $FINE
059400160705    >C                   MOVE      '0'           RETD44
059500160705     C*
059600160705     C                   ENDSR
059700160705     C/EJECT
059800160705     C************************************************************
059900160705     C* GESTIONE F12 VIDEO D1
060000160705     C************************************************************
060100160705     C     F12D1         BEGSR
060200160705     C*
060300160705     C                   MOVE      *ON           $FINE
060400160705    >C                   MOVE      '2'           RETD44
060500160705     C*
060600160705     C                   ENDSR
060700160705     C/EJECT
060800160705     C************************************************************
060900160705     C* GESTIONE F24 VIDEO D1
061000160705     C************************************************************
061100160705     C     F24D1         BEGSR
061200160705     C*
061300160705     C                   MOVEA     $FX1          $FX
061400160705     C                   MOVEA     $FC1          $FC
061500160705     C                   MOVEA     $FR1          $FR
061600160705     C                   Z-ADD     $ULKD1        $ULK
061700160705     C                   Z-ADD     $D1L01        $L01
061800160705     C                   Z-ADD     $D1L02        $L02
061900160705     C                   EXSR      RIEKEY
062000160705     C                   Z-ADD     $ULK          $ULKD1            3 0
062100160705     C                   MOVEA     $FC           $FC1
062200160705     C                   MOVEL     $KEY1         Z1KE1
062300160705     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
062400160705     C                   MOVEL     $KEY2         Z1KE2
062500160705     C                   WRITE     FMTZ1
062600160705     C*
062700160705     C                   ENDSR
062800160705     C/EJECT
062900160705     C************************************************************
063000160705     C* CONTROLLO VIDEATA
063100160705     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
063200160705     C* 95 96 97 98
063300160705     C************************************************************
063400160705     C     CTRD1         BEGSR
063500160705     C*
063600160705     C                   SETOFF                                       99
063700160705     c                   eval      D1Des2     =  *blanks
063800160705     c                   eval      D1DesAgg2  =  *blanks
063900160705     c                   eval      D1Indriz2  =  *blanks
064000160705     c                   eval      D1Cap2     =  *blanks
064100160705     c                   eval      D1Localit2 =  *blanks
064200160705     c                   eval      D1Prov2    =  *blanks
064300160705     c                   eval      D1Stato2   =  *blanks
064301160707     C*
064400160705     C*controllo sogg.sostituente
064500160705     C                   If        D1Sogg2 = *blank
064600160707     C********           Seton                                        5499
064700160705     C                   else
064800160705     C*
064900160705     C                   CALLb     'XALLINEA'
065000160705     C                   PARM                    D1Sogg2
065100160705     C                   PARM      8             LEN               5 0
065200160705     C                   PARM                    XSCINP
065300160705     C*
065400160705     C                   MOVE      '1'           DVARIC
065500160705     C                   MOVEL     'DVASOG'      dvastrutt
065600160705     C                   MOVE      societaD44    DVASOCIETA
065700160705     C                   MOVE      d1sogg2       DVASOGG
065800160705     C                   MOVE      *blank        DVATPIND
065900160705     C                   MOVE      *blank        DVACDIND
066000160705     C                   EVAL      DVALenOut = %Size(DVASOG)
066100160705     C                   CALLB     'NDDVASOG'    PDVASOG
066200160705   B C     DVAERRORE     IFEQ      *OFF
066300160705     C                   eval      %subst(DVASOG:1:DVALENOUT)
066400160705     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
066500160705     C*
066600160705     c                   eval      D1Des2     =  DVSDes
066700160705     c                   eval      D1DesAgg2  =  DVSDesAgg
066800160705     c                   eval      D1Indriz2  =  DVSIndriz
066900160705     c                   eval      D1Cap2     =  DVSCap
067000160705     c                   eval      D1Localit2 =  DVSLocalit
067100160705     c                   eval      D1Prov2    =  DVSProv
067200160705     c                   eval      D1Stato2   =  DVSStato
067600160705     c                   else
067700160705     C                   Seton                                        5599
067800160705     C                   EndIf
067900160705     C                   EndIf
068000160705     C*
068100160705     C                   ENDSR
068200160705     C/EJECT
107100160705     C************************************************************
107200160705     C* AGGIORNAMENTO legame bolla senza collegamento a fattura
107300160705     C************************************************************
107400160705     C     AGGANA        BEGSR
107500160705     C*
107501160705     C                   Eval      LRCSocieta  = Xscsoc
107504160705     C                   Eval      LRCSys      = SysD44
107505160705     C                   Eval      LRCNrAsReg  = NrAsRegD44
107506160705     C*
107508160705     C     K03LRC01      CHAIN     SDGLRC01L                          2122
107509160705     C*
107510160705     C* Gestione RCD allocato
107511160705     C     *in22         IFEQ      *ON
107512160707     C                   CLEAR                   SDGMSGDS
107513160705    >C                   MOVEL     'SDGLRC00F'   MSGFIL
107514160705     C                   MOVE      'S'           MSGLCK
107515160705     C                   Z-ADD     2             MSGMSG
107516160705     C                   MOVEL     'R'           MSGTPR
107517160705     C                   MOVEL     'S'           MSGRSP
107518160705     C                   MOVEL     'S'           MSGEMV
107519160705     C                   Z-ADD     10            MSGRGP
107520160705     C                   MOVEA     STATUS        STS
107521160705     C                   MOVEL     'N'           MSGCNL
107522160705     C                   MOVEL     'N'           MSGVID
107523160705     C                   MOVEL     'N'           MSGOPE
107524160705     C                   MOVEL     'N'           MSGSTP
107525160707     C                   CALL      'SDGMSGR'
107526160707     C                   PARM                    SDGMSGDS
107527160705   E3C                   endif
107528160705     C*
107529160705     C                   EXSR      RieANA
107530160705     C*
107532160705     C     *In21         IfEq      *off
107533160707     C                   if        LRCSogg     = *Blank
107534160707     C                   Delete    SDGLRC000                            22
107535160707     C                   else
107536160705     C                   Update    SDGLRC000                            22
107537160707     C                   endif
107538160705     C                   else
107539160705     C                   Write     SDGLRC000                            22
107540160705     C                   endif
107541160705     C*
107542160707     C* se rcd non scrivibile, modificabile o cancellabile, attivo errore
107543160705     C     *IN22         IfEq      *ON
107544160705     C                   SetOn                                        9599
107545160705     C                   EndIf
113500160705     C*
113600160705     C                   ENDSR
113700160705     C/EJECT
113701160705     C**************************************************6*********
113702160705     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
113703160705     C************************************************************
113704160705     C     RIEANA        BEGSR
113705160705     C*
113707160705     C                   if        *In21       = *On
113708160705     C                   Clear                   SDGLRC000
113709160705     C                   Eval      LRCDtImm    = ZDateIso
113710160705     C                   Eval      LRCUteImm   = XScCUt
113711160705     C                   Eval      LRCSYS      = SysD44
113712160705     C                   Eval      LRCNRASREG  = NrAsRegD44
113714160705     C                   Eval      LRCNRASL    = 0
113715160705     C                   Eval      LRCSYSL     = 0
113716160705     C                   endif
113719160705     C*
113725160705     C                   Eval      LRCSogg     = D1Sogg2
113729160705     C                   Eval      LRCDtUlMo   = ZDateIso
113730160705     C                   Eval      LRCUteUlMo  = XScCUt
113731160705     C*
113732160705     C                   MoveL     XScSoc        LRCSocieta
113733160705     C*
113735160705     C                   ENDSR
113736160705     C/EJECT
134500160705     C************************************************************
134600160705     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
134700160705     C************************************************************
134800160705     C     GESAGG        BEGSR
135400160705     C*
135500160705     C* segnalo al pgm chiamante l'avvenuta conferma
135600160705    >C                   MOVE      *ON           OPRD44
135700160705     C* segnalo al pgm chiamante l'aver premuto l'F6
135800160705    >C                   MOVE      '0'           RETD44
135900160705     C*
136000160705    >C                   MOVE      *on           $fine
136100160705     C*
136200160705     C                   ENDSR
136300160705     C/EJECT
136400160705     C************************************************************
136500160705     C* CONTROLLO ABILITAZIONE TASTI
136600160705     C************************************************************
136700160705     C     CTRKEY        BEGSR
136800160705     C*
136900160705     C                   SELECT
137000160705     C     $TASTO        WHENEQ    F01
137100160705     C                   Z-ADD     01            PosFun
137200160705     C     $FC(1)        IFEQ      '0'
137300160705     C                   SETON                                        99
137400160705     C                   ENDIF
137500160705     C     $TASTO        WHENEQ    F02
137600160705     C                   Z-ADD     02            PosFun
137700160705     C     $FC(2)        IFEQ      '0'
137800160705     C                   SETON                                        99
137900160705     C                   ENDIF
138000160705     C     $TASTO        WHENEQ    F03
138100160705     C                   Z-ADD     03            PosFun
138200160705     C     $FC(3)        IFEQ      '0'
138300160705     C                   SETON                                        99
138400160705     C                   ENDIF
138500160705     C     $TASTO        WHENEQ    F04
138600160705     C                   Z-ADD     04            PosFun
138700160705     C     $FC(4)        IFEQ      '0'
138800160705     C                   SETON                                        99
138900160705     C                   ENDIF
139000160705     C     $TASTO        WHENEQ    F05
139100160705     C                   Z-ADD     05            PosFun
139200160705     C     $FC(5)        IFEQ      '0'
139300160705     C                   SETON                                        99
139400160705     C                   ENDIF
139500160705     C     $TASTO        WHENEQ    F06
139600160705     C                   Z-ADD     06            PosFun
139700160705     C     $FC(6)        IFEQ      '0'
139800160705     C                   SETON                                        99
139900160705     C                   ENDIF
140000160705     C     $TASTO        WHENEQ    F07
140100160705     C                   Z-ADD     07            PosFun
140200160705     C     $FC(7)        IFEQ      '0'
140300160705     C                   SETON                                        99
140400160705     C                   ENDIF
140500160705     C     $TASTO        WHENEQ    F08
140600160705     C                   Z-ADD     08            PosFun
140700160705     C     $FC(8)        IFEQ      '0'
140800160705     C                   SETON                                        99
140900160705     C                   ENDIF
141000160705     C     $TASTO        WHENEQ    F09
141100160705     C                   Z-ADD     09            PosFun
141200160705     C     $FC(09)       IFEQ      '0'
141300160705     C                   SETON                                        99
141400160705     C                   ENDIF
141500160705     C     $TASTO        WHENEQ    F10
141600160705     C                   Z-ADD     10            PosFun
141700160705     C     $FC(10)       IFEQ      '0'
141800160705     C                   SETON                                        99
141900160705     C                   ENDIF
142000160705     C     $TASTO        WHENEQ    F11
142100160705     C                   Z-ADD     11            PosFun
142200160705     C     $FC(11)       IFEQ      '0'
142300160705     C                   SETON                                        99
142400160705     C                   ENDIF
142500160705     C     $TASTO        WHENEQ    F12
142600160705     C                   Z-ADD     12            PosFun
142700160705     C     $FC(12)       IFEQ      '0'
142800160705     C                   SETON                                        99
142900160705     C                   ENDIF
143000160705     C     $TASTO        WHENEQ    F13
143100160705     C                   Z-ADD     13            PosFun
143200160705     C     $FC(13)       IFEQ      '0'
143300160705     C                   SETON                                        99
143400160705     C                   ENDIF
143500160705     C     $TASTO        WHENEQ    F14
143600160705     C                   Z-ADD     14            PosFun
143700160705     C     $FC(14)       IFEQ      '0'
143800160705     C                   SETON                                        99
143900160705     C                   ENDIF
144000160705     C     $TASTO        WHENEQ    F15
144100160705     C                   Z-ADD     15            PosFun
144200160705     C     $FC(15)       IFEQ      '0'
144300160705     C                   SETON                                        99
144400160705     C                   ENDIF
144500160705     C     $TASTO        WHENEQ    F16
144600160705     C                   Z-ADD     16            PosFun
144700160705     C     $FC(16)       IFEQ      '0'
144800160705     C                   SETON                                        99
144900160705     C                   ENDIF
145000160705     C     $TASTO        WHENEQ    F17
145100160705     C                   Z-ADD     17            PosFun
145200160705     C     $FC(17)       IFEQ      '0'
145300160705     C                   SETON                                        99
145400160705     C                   ENDIF
145500160705     C     $TASTO        WHENEQ    F18
145600160705     C                   Z-ADD     18            PosFun
145700160705     C     $FC(18)       IFEQ      '0'
145800160705     C                   SETON                                        99
145900160705     C                   ENDIF
146000160705     C     $TASTO        WHENEQ    F19
146100160705     C                   Z-ADD     19            PosFun
146200160705     C     $FC(19)       IFEQ      '0'
146300160705     C                   SETON                                        99
146400160705     C                   ENDIF
146500160705     C     $TASTO        WHENEQ    F20
146600160705     C                   Z-ADD     20            PosFun
146700160705     C     $FC(20)       IFEQ      '0'
146800160705     C                   SETON                                        99
146900160705     C                   ENDIF
147000160705     C     $TASTO        WHENEQ    F21
147100160705     C                   Z-ADD     21            PosFun
147200160705     C     $FC(21)       IFEQ      '0'
147300160705     C                   SETON                                        99
147400160705     C                   ENDIF
147500160705     C     $TASTO        WHENEQ    F22
147600160705     C                   Z-ADD     22            PosFun
147700160705     C     $FC(22)       IFEQ      '0'
147800160705     C                   SETON                                        99
147900160705     C                   ENDIF
148000160705     C     $TASTO        WHENEQ    F23
148100160705     C                   Z-ADD     23            PosFun
148200160705     C     $FC(23)       IFEQ      '0'
148300160705     C                   SETON                                        99
148400160705     C                   ENDIF
148500160705     C     $TASTO        WHENEQ    F24
148600160705     C                   Z-ADD     24            PosFun
148700160705     C     $FC(24)       IFEQ      '0'
148800160705     C                   SETON                                        99
148900160705     C                   ENDIF
149000160705     C                   ENDSL
149100160705     C*
149200160705     C                   ENDSR
149300160705     C/EJECT
149400160705     C******************************************************
149500160705     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
149600160705     C******************************************************
149700160705     C     MSGPGM        BEGSR
149800160705     C*
149900160705     C                   Z-ADD     2             MSGMSG
150000160705     C                   MOVEL     'S'           MSGRSP
150100160705     C                   MOVEL     'CR'          MSGTPR
150200160705     C                   MOVEL     'S'           MSGEMV
150300160705     C                   Z-ADD     10            MSGRGP
150400160705     C                   MOVEL     'S'           MSGLCK
150500160705     C                   MOVEA     STATUS        STS
150600160705     C                   MOVEL     'N'           MSGCNL
150700160705     C                   MOVEL     'N'           MSGVID
150800160705     C                   MOVEL     'N'           MSGOPE
150900160705     C                   MOVEL     'N'           MSGSTP
151000160707     C                   CALL      'SDGMSGR'
151100160707     C                   PARM                    SDGMSGDS
151200160705     C*
151300160705     C                   ENDSR
151400160705     C/EJECT
151500160705     C************************************************************
151600160705     C* OPERAZIONI INIZIALI
151700160705     C************************************************************
151800160705     C     *INZSR        BEGSR
151900160705     C*
151901160705     C     *DMY          Move      Udate         ZDateISO
151902160705     C     *JobRun       Move      ZDateISO      ZDate             6 0
151903160705     C*
152000160705     C* Reperimento parametri
152100160705     C*
152200160705     C     *ENTRY        PLIST
152300160705     C                   PARM                    KPJBA
152500160705     C*
152600160705     C* Reperimento dati società
152700160705     C*
152800160705    >C                   MOVEL     'SOC001'      TIPXSC
152900160705     C                   MOVEL     *BLANK        SOCXSC
153000160705     C                   EXSR      REPSOC
153100160705     C*
153200160705     C* Controlli dati società
153300160705     C*
153400160705     C     RTNXSC        IFNE      '1'
153500160705     C                   MOVEL     XSOCDS        SOC001
153600160705     C                   ELSE
153700160705    >C                   MOVE      '1'           ERRD44
153800160705     C                   MOVE      *on           $Fine
153900160705     C                   EXSR      EndPgm
154000160705     C                   ENDIF
154900160705     C*
155000160705     C* reperisco le dimensioni delle variabili che conterranno i tasti
155100160705     C                   EVAL      $D1L01 = %Size(Z1Ke1)
155200160705     C                   EVAL      $D1L02 = %Size(Z1Ke2)
155300160705    > * aggiungere altre variabili nel caso di videate indipendenti
155400160705    > * ulteriori a quelle std
155500160705     C*
155600160705     C* Reperimento tasti di funzione
155700160705     C*
155800160705     C                   EXSR      REPKEY
155900160705     C*
156000160705     C* Richiamo routine per aggiunta tasti funzione personalizzati
156100160705     C*
156200160705     C                   CAllb     'X46USREXT'
156300160705     C                   PARM                    *omit
156400160705     C                   PARM                    *omit
156500160705     C                   PARM                    *omit
156600160705     C                   PARM                    *omit
156700160705     C                   PARM                    *omit
156800160705     C                   PARM                    $FX1
156900160705     C                   PARM                    $FC1
157000160705     C                   PARM                    $PGF1
157100160705     C                   PARM                    DsPgm
157200160705     C                   PARM                    xsccut
157300160705     C                   PARM                    xscgrp
158800160705     C*
159800160705     C                   ENDSR
159900160705     C/EJECT
160000160705     C************************************************************
160100160705     C* REPERIMENTO TASTI DI FUNZIONE
160200160705     C************************************************************
160300160705     C     REPKEY        BEGSR
160400160705     C*
160500160705     C                   MOVEA     $FM1          $FM
160600160705     C                   MOVEA     $FC1          $FC
160700160705     C                   EXSR      REPKKK
160800160705     C                   MOVEA     $FX           $FX1
160900160705     C*
161000160705     C                   ENDSR
161100160705     C/EJECT
161200160705     C************************************************************
161300160705     C* REPERIMENTO TASTI DI FUNZIONE
161400160705     C************************************************************
161500160705     C     REPKKK        BEGSR
161600160705     C*
161700160705     C                   CLEAR                   $FX
161800160705     C     1             DO        24            X
161900160705     C*
162000160705     C     $FM(X)        IFNE      *BLANK
162100160705     C     $FC(X)        ANDNE     '0'
162200160705     C*
162300160705     C                   MOVEL     $FM(X)        IDMSG
162400160705     C                   CALLB     'XRTVM'
162500160705     C                   PARM                    IDMSG            27
162600160705     C                   PARM                    TXTMSG          100
162700160705     C                   PARM                    ERRMSG            1
162800160705     C*
162900160705     C     ERRMSG        IFNE      *ON
163000160705     C                   MOVEL     TXTMSG        $FX(X)
163100160705     C                   ELSE
163200160705     C                   MOVEL     *ALL' '       $FX(X)
163300160705     C                   ENDIF
163400160705     C*
163500160705     C                   ENDIF
163600160705     C*
163700160705     C                   ENDDO
163800160705     C*
163900160705     C                   ENDSR
164000160705     C/EJECT
164100160705     C************************************************************
164200160705     C* INIZIALIZZAZIONE VARIABILI
164300160705     C************************************************************
164400160705     C     INZVAR        BEGSR
164900160705     C*
165000160705    >C                   MOVEL     KPJBU         SDGSD44DS
165001160711     C*
165002160711     C* Apertura guidata finestra
165004160711     C                   if        WDWD44      = '1'
165005160711     C                   CLOSE     SDGSD44D
165006160711     C                   OPEN      SDGSD44D
165007160711     C                   endif
165008160711     C*
165009160711     C                   WRITE     FMTZ3
165100160705     C*
165400160705    >C                   If        SOCIETAD44 <> xscsoc
165500160705     C                   MOVEL     'SOC001'      TIPXSC
165600160705    >C                   MOVEL     SOCIETAD44    SOCXSC
165700160705     C                   EXSR      REPSOC
165800160705     C     RTNXSC        IFNE      '1'
165900160705     C                   MOVEL     XSOCDS        SOC001
166000160705     C                   ELSE
166100160705    >C                   MOVE      '1'           ERRD44
166200160705     C                   MOVE      *on           $Fine
166300160705     C                   EXSR      EndPgm
166400160705     C                   ENDIF
166500160705     C                   endif
168500160705     C*
168600160705     C* Definizione dei tasti funzione utilizzabili a pgm
168700160705     C*
168800160705     C                   EXSR      DEFOPZ
168900160705     C*
169000160705     C* Variabili per gestione videate
169100160705     C*
169200160705     C                   MOVE      'D1'          $GEST                          fmt in gestione
169300160705     C                   MOVE      *OFF          $FINE                          fine pgm
169400160705     C                   MOVE      *OFF          $FlgDef                        fine pgm
169500160705     C                   MOVE      *ON           $INZD1                         inizializzione D1
169600160705     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
169700160705     C                   MOVE      *BLANK        $LASTV                         ultima emissione
169800160705     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
169900160705     C* azzero la posizione del cursore
170000160705     C                   Z-ADD     0             H1RIGA
170100160705     C                   Z-ADD     0             H1COLO
170200160705     C*
170300160705     C* Variabili appoggio
170400160705     C*
170500160705     C                   Z-ADD     0             CURR
170600160705     C                   Z-ADD     0             CURC
170700160705     C*
170800160705     C* Inizializzazione/Decodifica prima videata
170900160705     C*
171000160705     C                   EXSR      INZD1
171100160705     C                   MOVE      *OFF          $INZD1
171200160705     C*
171300160705     C                   ENDSR
171400160705     C/EJECT
173400160705     C************************************************************
173500160705     C* REPERIMENTO DATI SOCIETA'
173600160705     C************************************************************
173700160705     C     REPSOC        BEGSR
173800160705     C*
173801160707     C                   CALL      'SDGSOCDR'
174000160705     C                   PARM                    TIPXSC            6
174100160705     C                   PARM                    SOCXSC            3
174200160705     C                   PARM                    CDSXSC            9 0
174300160705     C                   PARM                    MODXSC            3
174400160705     C                   PARM      *BLANKS       RTNXSC            1
174500160705     C                   PARM                    XSOCDS
174600160705     C                   PARM                    KPJBA
174700160705     C*
174800160705     C                   ENDSR
174900160705     C/EJECT
178000160705     C************************************************************
178100160705     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
178200160705     C************************************************************
178300160705     C     DEFOPZ        BEGSR
178400160705     C*
178500160705     C* Questa routine deve valorizzare la schiera $FC1/$FR1
178600160705     C* con S o N a seconda della utilizzabilità o meno
178700160705     C* del tasto funzione correlato
178800160705     C*
178900160705     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
179000160705     C* di compilazione
179100160705     C                   RESET                   $FC1
179200160705     C                   RESET                   $FR1
179300160705     C*
179400160705     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
179500160705     C* invalido alcune opzioni
179600160705     C                   SELECT
179700160705     C* Se pgm richiamato in interrogazione disattivo :
179800160705     C*
179900160705     C                   OTHER
180000160705     C*
180100160705     C                   ENDSL
180200160705     C*
180300160705     C                   ENDSR
180400160705     C/EJECT
180500160705     C************************************************************
180600160705     C* REPERIMENTO CAMPI DI DEFAULT
180700160705     C************************************************************
180800160705     C     REPDEF        BEGSR
180900160705     C*
181000160705    > * riempire dei campi di appoggio con tutti quei valori
181100160705    > * che possono essere usati come default
181200160705     C*
181300160705     C                   ENDSR
181400160705     C/EJECT
181500160705     C************************************************************
181600160705     C* DEFINIZIONE KLIST
181700160705     C************************************************************
181800160705     C     DefKlist      BEGSR
181900160705     C*
182000160705     C* klist
182001160705     C*
182002160705     C* SDGLRC01L
182003160705     C     K03LRC01      Klist
182004160705     C                   Kfld                    LRCSocieta
182005160705     C                   Kfld                    LRCSys
182006160705     C                   Kfld                    LRCNrAsReg
182007160705     C*
185700160705     C*per driver soggetti
185800160705     C     PDVASOG       PLIST
185900160705     C                   PARM                    DVARIC
186000160705     C                   PARM                    DVASOCIETA
186100160705     C                   PARM                    DVAUNITA
186200160705     C                   PARM                    DVASTRUTT
186300160705     C                   PARM                    DVADTRIF
186400160705     C                   PARM                    DVALENOUT
186500160705     C                   PARM                    *OMIT
186600160705     C                   PARM                    *OMIT
186700160705     C                   PARM                    DVALINEAV
186800160705     C                   PARM                    DVAFILIALE
186900160705     C                   PARM                    DVASOGG
187000160705     C                   PARM                    DVATPIND
187100160705     C                   PARM                    DVACDIND
187200160705     C                   PARM                    DVAERRORE
187300160705     C                   PARM                    DVAOUTPUT
187400160705     C*
187500160705     C                   ENDSR
187600160705     C/EJECT
187700160705     C************************************************************
187800160705     C* RIEMPIMENTO TASTI FUNZIONE
187900160705     C************************************************************
188000160705     C     RIEKEY        BEGSR
188100160705     C*
188200160705     C* Input :
188300160705     C* - $FX = schiera con tutti i testi disponibili
188400160705     C* - $FC = schiere con i flag di validità associati ai testi
188500160705     C* - $FR = schiere con i flag di REVERSE IMAGE
188600160705     C* - $ULK = ultimo testo caricato
188700160705     C* - $L01 = Lunghezza prima riga
188800160705     C* - $L02 = Lunghezza seconda riga
188900160705     C* Output :
189000160705     C* - $ULK
189100160705     C* - $KEY1= 1° riga di tasti funzione
189200160705     C* - $KEY2= 2° riga di tasti funzione
189300160705     C*
189400160705     C                   CALLB     'XKEYMSG'
189500160705     C* input
189600160705     C                   PARM                    $FX
189700160705     C                   PARM                    $FC
189800160705     C                   PARM                    $FR
189900160705     C                   PARM                    $ULK              3 0
190000160705     C* output
190100160705     C                   PARM                    $KEY1            79
190200160705     C                   PARM                    $KEY2            79
190300160705     C* input
190400160705     C                   PARM                    $L01              3 0
190500160705     C                   PARM                    $L02              3 0
190600160705     C*
190700160705     C                   PARM                    $ALLFUNCT       480
190800160705     C*
190900160705     C                   ENDSR
191000160705     C*****************************************************************
194500160705** $OZ / $ID
19460016070501PROMSG    *LIBL     COS0001
19470016070502PROMSG    *LIBL     COS0002
19480016070503PROMSG    *LIBL     COS0003
19490016070504PROMSG    *LIBL     COS0004
19500016070505PROMSG    *LIBL     COS0005
195100160705** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
195200160705PROMSG    *LIBL     KEY0001  01
195300160705PROMSG    *LIBL     KEY0002  02
195400160705PROMSG    *LIBL     KEY0003  03
195500160705PROMSG    *LIBL     KEY0004  04
195600160705PROMSG    *LIBL     KEY0005  05
195700160705PROMSG    *LIBL     KEY0006  06
195800160705PROMSG    *LIBL     KEY0007  07
195900160705PROMSG    *LIBL     KEY0008  08
196000160705PROMSG    *LIBL     KEY0009  09
196100160705PROMSG    *LIBL     KEY0010  10
196200160705PROMSG    *LIBL     KEY0011  11
196300160705PROMSG    *LIBL     KEY0012  12
196400160705PROMSG    *LIBL     KEY0013  13
196500160705PROMSG    *LIBL     KEY0014  14
196600160705PROMSG    *LIBL     KEY0015  15
196700160705PROMSG    *LIBL     KEY0016  16
196800160705PROMSG    *LIBL     KEY0017  17
196900160705PROMSG    *LIBL     KEY0018  18
197000160705PROMSG    *LIBL     KEY0019  19
197100160705PROMSG    *LIBL     KEY0020  20
197200160705PROMSG    *LIBL     KEY0021  21
197300160705PROMSG    *LIBL     KEY0022  22
197400160705PROMSG    *LIBL     KEY0023  23
197500160705PROMSG    *LIBL     KEY0024  24
197600160705**  ABILITAZIONE D1 : $FC1 / $FR1
1977001607050N  01
1978001607050N  02
1979001607051N  03
1980001607051N  04
1981001607050N  05
1982001607051N  06
1983001607050N  07
1984001607050N  08
1985001607050N  09
1986001607050N  10
1987001607050N  11
1988001607051N  12
1989001607050N  13
1990001607050N  14
1991001607050N  15
1992001607050N  16
1993001607050N  17
1994001607050N  18
1995001607050N  19
1996001607050N  20
1997001607050N  21
1998001607050N  22
1999001607050N  23   Impostare a '1' se si utilizzano opzioni
2000001607051N  24   Lasciare sempre impostata a '1' il valore della riga 24
