000100000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(QILE)
000200000000     H*PARMS BNDDIR(PJXBND PJCBND) CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400160609      * Task R218: nuovo oggetto
000500000000      *****************************************************************
000600000000      *
000700000000      *         ------------------------------------------------
000800160524      *                            MOD 770
000900160524      *                 Lancio estrazione movimenti ritenute
001000000000      *         ------------------------------------------------
001100000000      *
001200000000      *****************************************************************
001300000000     F*----------------------------------------------------*
001400160524     FSDGSK01D  CF   E             WORKSTN
001500160524     F                                     RENAME(SK01T1:FMTT1)
001600160524     F                                     RENAME(SK01D1:FMTD1)
001700160524     F                                     RENAME(SK01Z1:FMTZ1)
001900160524     F                                     RENAME(SK01D3:FMTD3)
002000160524     F                                     RENAME(SK01Z3:FMTZ3)
002200000000     F                                     INFDS(DSFMT)
002500000000     D*----------------------------------------------------*
002600000000     D* Schiere per la gestione dei tasti di funzione
002700000000     D $FM             S             27    DIM(24)
002800000000     D $FX             S             50    DIM(24)
002900000000     D $FC             S              1    DIM(24)
003000000000     D $FR             S              1    DIM(24)
003100000000     D* Video D1/D2
003200000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003300000000     D $FX1            S             50    DIM(24) ALT($FM1)
003400000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003500000000     D $FR1            S              1    DIM(24) ALT($FC1)
004100000000     D* Video D3 Valori assunti
004200000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
004300000000     D $FX3            S             50    DIM(24) ALT($FM3)
004400000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
004500000000     D $FR3            S              1    DIM(24) ALT($FC3)
004600000000     D*----------------------------------------------------*
004700000000     D*-------------
004800000000     D* Passaggio Parametri
004900000000     D KPJBA         E DS
005000000000     D*-------------
005100000000     D* DS Esterna gestione messaggi
005200000000     D SDGMSGDS      E DS                  INZ
005300000000     D  STS          E                     EXTFLD(MSGSTS)
005400000000     D                                     DIM(333)
005500000000     D  JBA          E                     EXTFLD(MSGJBA)
005600000000     D                                     DIM(502)
005700000000     D*-------------
005800000000     D*Asteriscare se non presenti
005900000000     D* Reperimento Autorizzazioni
006000000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
006100000000     D*-------------
006200000000     D* Dati per pgm XNOAUT
006300000000     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
006400000000     D*-------------
006500000000     D* Passaggio Autorizzazioni
006600000000     D DSAUT           DS           512
007000101118     D*-------------
007100000000     D* Valori assunti
007200000000     D XMPEDS        E DS                  INZ
007300000000     D XMPEDS1       E DS                  INZ
007400000000     D DSMPE           DS                  INZ
007500000000     D  $VAL                          1
007600000000     D  $Anno                         4S 0
007601160620R218 D  $RAGGRGT                           like(D1RaggrGt)
007602160617  "  D  $INDIR                             like(D1Indir)
007603160617  "  D  $ESCITC                            like(D1EscItc)
007604160620R218 D  $ESCIMN                            like(D1EscIMn)
008200000000     D*-------------
010100000000     D* Reperimento nome PGM
010200000000     D STATUS         SDS           333
010300000000     D  DSPGM            *PROC
010400000000     D  PARMS            *PARMS
010401160620R218 D*-------------
010402160617  "  D* Values
010403160617  "  D X07ValDs      E DS
010404160617  "  D*-------------
010405160617  "  D* Values
010406160620R218 D Ana964Ds      E DS
010500000000     D*-------------
010600000000     D* Dati di ambiente ottenuti da SDGSOCDR
010700000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
010800000000     D*-------------
010900000000     D* DS Interna per dati di output di SDGSOCDR
011000000000     D XSOCDS          DS          1000
011100000000     D*-------------
011200000000     D* Ds per ricezione parametri
011300160524     D SDGSK01DS     E DS
011400160524     D  ERRK01       E                     INZ('0')
011500000000     D*-------------
011600000000     D* Passaggio Parametri al PGM di estrazione dati
011701160524     D ndcr70ds      E DS
011900000000     D*-------------
012800000000     D DSFMT           DS
012900000000     D  $Tasto               369    369
013000000000     D  NRG                  370    370
013100000000     D  NCL                  371    371
013200000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
013300000000     D*-------------
013400000000     D* posizione cursore
013500000000     D CURSOR          DS
013600000000     D  RIRI                   1      2B 0 INZ
013700000000     D  R$$                    2      2
013800000000     D  COCO                   3      4B 0 INZ
013900000000     D  C$$                    4      4
014000000000     D*-------------
014100000000     D* Tasti di funzione
014200000000     D F01             C                   CONST(X'31')
014300000000     D F02             C                   CONST(X'32')
014400000000     D F03             C                   CONST(X'33')
014500000000     D F04             C                   CONST(X'34')
014600000000     D F05             C                   CONST(X'35')
014700000000     D F06             C                   CONST(X'36')
014800000000     D F07             C                   CONST(X'37')
014900000000     D F08             C                   CONST(X'38')
015000000000     D F09             C                   CONST(X'39')
015100000000     D F10             C                   CONST(X'3A')
015200000000     D F11             C                   CONST(X'3B')
015300000000     D F12             C                   CONST(X'3C')
015400000000     D F13             C                   CONST(X'B1')
015500000000     D F14             C                   CONST(X'B2')
015600000000     D F15             C                   CONST(X'B3')
015700000000     D F16             C                   CONST(X'B4')
015800000000     D F17             C                   CONST(X'B5')
015900000000     D F18             C                   CONST(X'B6')
016000000000     D F19             C                   CONST(X'B7')
016100000000     D F20             C                   CONST(X'B8')
016200000000     D F21             C                   CONST(X'B9')
016300000000     D F22             C                   CONST(X'BA')
016400000000     D F23             C                   CONST(X'BB')
016500000000     D F24             C                   CONST(X'BC')
016600000000     D ENTER           C                   CONST(X'F1')
016700000000     D ROLDWN          C                   CONST(X'F4')
016800000000     D ROLLUP          C                   CONST(X'F5')
016900000000     I*-------------
017000000000     I* Sottotitolo di testata
017100000000     I* se non si hanno sottotitoli, mettere tutta la costante = ' '
017200000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
017300000000     D                                          SDC0056')
017400000000     D SOTTITD2        C                   CONST('SDGMSG    *LIBL-
017500000000     D                                          SDC0054')
017600000000     D*-----------
017700000000     D* Campi di lavoro
017800000000     D CallPgm         S              1
018000000000     D $AnnoAlfa       S              4
018100000000     D DataISO         S               D
018200110811     D WDtImp          S               D
018300150204     D WDtFirDic       S               D
018400000000     D $DT0            C                   CONST('0001-01-01')
018500130904     D*
021200130904     C*----------------------------------------------------*
021300130904     C*                MAIN LINE PROGRAM
021400130904     C*----------------------------------------------------*
021500130904     C*
021600130904     C* inizializzazione variabili
021700130904     C                   Exsr      INZVAR
021800130904     C*
021900000000     C     $FINE         DOWEQ     *OFF
022000000000     C     $GEST         CASEQ     'D1'          GESD1
022200000000     C     $GEST         CASEQ     'D3'          GESD3
022300000000     C                   ENDCS
022400000000     C                   ENDDO
022500000000     C*
022600000000     C* fine programma
022700000000     C                   Exsr      EndPgm
022800000000     C************************************************************
022900000000     C* RICERCHE
023000000000     C************************************************************
023100000000     C     EndPgm        BegSr
023200000000     C*
023300000000     C*
023400000000     C* se ho confermato l'uscita proseguo
023500000000     C     $Fine         IfEq      *ON
023600000000     C*
023700000000     C                   Seton                                            LR
023800000000     C                   Return
023900000000     C*
024000000000     C                   EndIf
024100000000     C*
024200000000     C                   EndSr
024300000000     C************************************************************
024400000000     C* RICERCHE
024500000000     C************************************************************
024600000000     C     Search        BegSr
029300000000     C*
029302160620R218 C* determino Riga/Colonna del cursore
029303160617  "  C                   MOVE      NRG           R$$
029304160617  "  C                   MOVE      NCL           C$$
029305160617  "  C                   Z-ADD     RIRI          CURR
029306160617  "  C                   Z-ADD     COCO          CURC
029307160617  "  C*
029308160617  "  C                   Select
029309160617  "  C*
029310160617  "  C* FMTD2 - Calcolo valuta media
029311160617  "  C     H1NMFL        WHENEQ    'D1INDIR'
029312160617  "  C                   Reset                   Ana964Ds
029313160617  "  C                   Movel     '01'          Opz964
029314160617  "  C                   MoveL     'ANDIZ000'    TRc964
029315160617  "  C                   Movel     'DSPINDIR'    Cam964
029316160617  "  C                   Movel     Ana964DS      KPJBU
029317160617  "  C                   Call      'ANA964R'
029318160617  "  C                   Parm                    KPJBA
029319160617  "  C                   Movel     KPJBU         Ana964DS
029320160617  "  C*
029321160617  "  C     VAL964        IFNE      *BLANKS
029322160617  "  C                   Movel     VAL964        D1Indir
029323160617  "  C                   ENDIF
029338160617  "  C*
029339160617  "  C* ricerca non abilitata in questa posizione
029340160617  "  C                   OTHER
029341160617  "  C                   SETON                                        9899
029342160617  "  C*
029343160617  "  C                   ENDSL
029344160617  "  C*
029345160617  "  C* imposto pos. del cursore
029346160617  "  C                   Z-ADD     CURR          H1RIGA
029347160617  "  C                   Z-ADD     CURC          H1COLO
029348160617  "  C*
029349160617  "  C                   MOVE      *HIVAL        $LASTV
029350160620R218 C*
029400000000     C                   EndSr
033900110421     C************************************************************
034000110421     C* GESTIONE EMISSIONE TASTI FUNZIONE
034100110421     C************************************************************
034200110421     C     GesPie        BegSr
034300110421     C*
034400110421     C                   Select
034500110421     C*
034600110421     C     $GEST         WhenEq    'D1'
034700110421     C     $ULKD1        IfEq      0
034800110421     C* riempimento opzioni
034900000000     C                   Exsr      F24D1
035000000000     C                   ElSe
035100000000     C                   Write     FMTZ1
035200000000     C                   EndIf
036100000000     c*
036200000000     C* Modifica valori assunti
036300000000     C     $GEST         WhenEq    'D3'
036400000000     C     $ULKD3        IfEq      0
036500000000     C                   Exsr      F24D3
036600000000     C                   ElSe
036700000000     C                   Write     FMTZ3
036800000000     C                   EndIf
036900000000     C                   EndSl
037000000000     C*
037100000000     C                   EndSr
037200000000     C************************************************************
037300000000     C* GESTIONE VIDEO RECORD D1
037400000000     C************************************************************
037500000000     C     GesD1         BegSr
037600000000     C* inizializzo videata
037700000000     C     $InzD1        IfEq      *ON
037800000000     C                   Exsr      InzD1
037900000000     C                   Move      *OFF          $InzD1
038000000000     C                   EndIf
038100000000     C* emissione piede videata se proveniente da altra
038200000000     C* salvataggio valore formato di provenienza
038300000000     C     $LastV        IFNE      'D1'
038400000000     C                   Write     FMTT1
038500000000     C                   Exsr      GESPIE
038600000000     C                   Move      $LastV        $LastG
038700000000     C                   Move      'D1'          $LastV
038800000000     C                   EndIf
038900000000     C*
039000000000     C* se esistono errori sulla videata
039100000000     C* emetto la Write del formato a indicatori spenti per vedere
039200000000     C* le eventuali decodifiche
039300000000     C     *IN99         IfEq      *ON
039400000000     C     $GEST         ANDEQ     'D1'
039500000000     C                   MoveA     *IN           WIN
039600000000     C                   Move      *ALL'0'       IN5098           49
039700000000     C                   MoveA     IN5098        *IN(50)
039800000000     C                   Write     FMTD1
039900000000     C                   MoveA     WIN           *IN
040000000000     C                   EndIf
040100000000     C*
040200000000     C                   EXFMT     FMTD1
040300000000     C*
040400000000     C* controllo abilitazione tasti funzione
040500000000     C                   Exsr      CTRKD1
040600000000     C*
040700000000     C     *IN99         IfEq      *OFF
040800000000     C*
040900000000     C                   Select
041000000000     C* ROLLUP
041300000000     C* F2=Decodifica
041400000000     C     $Tasto        WhenEq    F02
041500000000     C                   Exsr      F02D1
041600000000     C* F3=Fine
041700000000     C     $Tasto        WhenEq    F03
041800000000     C                   Exsr      F03D1
041900000000     C* F4=Ricerca
042000000000     C     $Tasto        WhenEq    F04
042100000000     C                   Exsr      SEARCH
042200000000     C* F5=Refresh
042300000000     C     $Tasto        WhenEq    F05
042400000000     C                   Exsr      F05D1
042500000000     C* F8=Successivo
042600000000     C     $Tasto        WhenEq    F08
042700000000     C                   Exsr      F08D1
042800000000     C* F12=Ritorno
042900000000     C     $Tasto        WhenEq    F12
043000000000     C                   Exsr      F12D1
043100000000     C* F18=Valori assunti
043200000000     C     $Tasto        WhenEq    F18
043300000000     C                   Exsr      F18D1
043700000000     C* F21=Pers. batch
043800000000     C     $Tasto        WhenEq    F21
043900000000     C                   Exsr      F21D1
044000000000     C* F24=Altri tasti
044100000000     C     $Tasto        WhenEq    F24
044200000000     C                   Exsr      F24D1
044300000000     C*
044400000000     C                   Other
044500000000     C                   Exsr      CTRD1
045300000000     C*
045400000000     C     $Tasto        IfEq      F06
045500000000     C     *IN99         ANDEQ     *OFF
045600000000     C*
046800000000     C* lancio
046900000000     C                   Exsr      ExeLan
047000000000     C                   Exsr      EndPgm
047100000000     C*
047200000000     C                   EndIf
047500000000     C*
047600000000     C                   EndSl
047700000000     C*
047800000000     C                   EndIf
047900000000     C*
048000000000     C                   EndSr
048100000000     C************************************************************
048200000000     C* INIZIALIZZAZIONE VIDEATA DATI
048300000000     C************************************************************
048400000000     C     InzD1         BegSr
048500000000     C*
048600000000     C                   Clear                   FMTD1
048601160620R218 C                   Move      XScOn         D1On1
048602160617  "  C                   Move      XScOff        D1Off1
048603160617  "  C                   Move      XScOn         D1On2
048604160617  "  C                   Move      XScOff        D1Off2
048605160617  "  C                   Move      XScOn         D1On3
048606160617  "  C                   Move      XScOff        D1Off3
048607160617  "  C                   Move      XScOff        D1RaggRGT
048608160617  "  C                   Move      '1'           D1Indir
048611160617  "  C                   Move      XScOff        D1EscItc
048612160617  "  C                   Move      XScOff        D1EscImn
048613160620R218 C*
048800000000     C*
048900000000     C                   Z-Add     1             $PIED1
049000000000     C*
049100110802     C* se il pgm e'stato richiamato imposto i valori ricevuti dalla DS
049200000000     C                   If        CallPgm     = '1'
049300000000     C*
049400160524     C                   Exsr      RieD1K01
049500000000     C*
049600110802     C* se il pgm NON e stato richiamato
049700000000     C                   ElSe
049800000000     C*
049900000000     C* Se vuole impostazione lancio da ultima elaborazione, li imposto da anmpe
050000000000     C     $Val          IfEq      *ON
050100000000     C* riempio la videata da parametri memorizzati
050200000000     C                   Exsr      RIED1
050300000000     C*  Aggiungere le routine di riempimento dei record succcessivi
050500000000     C                   ElSe
050600000000     C* Immettere i valodi di default a video
050700000000     C                   Eval      D1Anno      = *Year
050800110802     C                   Eval      D1Anno      = D1Anno - 1
051100130809     C                   EndIf
051200130809     C*
051300130809     C                   EndIf
051400130809     C*
051500130809     C* richiamo routine dei ctrl per decodificarla
052100130809     C*
052200130809     C                   EndSr
052300130809     C************************************************************
052400130809     C* Riempimento videata  D1 da richiamo pgm
052500130809     C************************************************************
052600160525     C     RieD1K01      BegSr
052700130809     C*
052800160524     C                   Z-Add     AnnoK01       D1Anno
053500130809     C*
053600130809     C                   EndSr
053700130809     C************************************************************
053800130809     C* RIEMPIMENTO VIDEATA  D1
053900130809     C************************************************************
054000130809     C     RIED1         BegSr
054100130809     C*
054200130809     C* impostare i campi della videata dai parametri memorizzati su anmpe
054300130809     C                   Z-Add     $Anno         D1Anno
054301160620R218 C                   movel     $RAGGRGT      D1RaggrGt
054302160617  "  C                   movel     $INDIR        D1Indir
054303160617  "  C                   movel     $ESCITC       D1EscItc
054304160617  "  C                   movel     $ESCIMN       D1EscIMn
054800160620R218 C*
054900130809     C                   EndSr
055000130809     C************************************************************
055100130809     C* GESTIONE F02 VIDEO D1
055200130809     C************************************************************
055300130809     C     F02D1         BegSr
055400130809     C*
055500130809     C* per decodificare la videata richiamo la routine dei ctrl
055600130809     C* e poi disattivo gli eventuali messaggi di errore
055700130809     C                   Exsr      CTRD1
055800130809     C*
055900130809     C                   Move      *ALL'0'       IN5098
056000130809     C                   MoveA     IN5098        *IN(50)
056100130809     C*
056200130809     C                   EndSr
056300000000     C************************************************************
056400000000     C* GESTIONE F03 VIDEO D1
056500000000     C************************************************************
056600000000     C     F03D1         BegSr
056700000000     C*
056800000000     C                   Move      *ON           $FINE
056900000000     C* fine programma
057000000000     C                   Exsr      EndPgm
057100000000     C*
057200000000     C                   EndSr
057300000000     C************************************************************
057400000000     C* GESTIONE F05 VIDEO D1
057500000000     C************************************************************
057600000000     C     F05D1         BegSr
057700000000     C*
057800000000     C                   Move      *ON           $InzD1
057900000000     C*
058000000000     C                   EndSr
058100000000     C************************************************************
058200000000     C* GESTIONE F08 VIDEO D1
058300000000     C************************************************************
058400000000     C     F08D1         BegSr
058500000000     C*
058600000000     C                   Move      *ON           $FINE
058700000000     C*
058800000000     C                   EndSr
058900000000     C************************************************************
059000000000     C* GESTIONE F12 VIDEO D1
059100000000     C************************************************************
059200000000     C     F12D1         BegSr
059300000000     C*
059400000000     C                   Move      *ON           $FINE
059500000000     C*
059600000000     C                   EndSr
059700000000     C************************************************************
059800000000     C* GESTIONE F18 VIDEO D1
059900000000     C************************************************************
060000000000     C     F18D1         BegSr
060100000000     C*
060200000000     C                   Move      'D3'          $GEST
060300000000     C                   Move      *ON           $INZD3
060400000000     C*
060500000000     C                   EndSr
061500000000     C************************************************************
061600000000     C* GESTIONE F21 VIDEO D1
061700000000     C************************************************************
061800000000     C     F21D1         BegSr
061900000000     C*
062000160603     C                   Move      'SK02'        KCOAZ
062100000000     C                   CALL      'BCH09'
062200000000     C                   PARM                    KPJBA
062300000000     C*
062400000000     C                   EndSr
062500000000     C************************************************************
062600000000     C* GESTIONE F24 VIDEO D1
062700000000     C************************************************************
062800000000     C     F24D1         BegSr
062900000000     C*
063000000000     C                   MoveA     $FX1          $FX
063100000000     C                   MoveA     $FC1          $FC
063200000000     C                   MoveA     $FR1          $FR
063300000000     C                   Z-Add     $ULKD1        $ULK
063400000000     C                   Z-Add     79            $L01
063500000000     C                   Z-Add     79            $L02
063600000000     C                   Exsr      RIEKEY
063700000000     C                   Z-Add     $ULK          $ULKD1            3 0
063800000000     C                   MoveA     $FC           $FC1
063900000000     C                   MoveL     $KEY1         Z1KE1
064000000000     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
064100000000     C                   MoveL     $KEY2         Z1KE2
064200000000     C                   Write     FMTZ1
064300000000     C*
064400000000     C                   EndSr
066500000000     C************************************************************
066600000000     C* GESTIONE F24 VIDEO D3
066700000000     C************************************************************
066800000000     C     F24D3         BegSr
066900000000     C*
067000000000     C                   MoveA     $FX3          $FX
067100000000     C                   MoveA     $FC3          $FC
067200000000     C                   MoveA     $FR3          $FR
067300000000     C                   Z-Add     $ULKD3        $ULK
067400000000     C                   Z-Add     44            $L01
067500000000     C                   Z-Add     0             $L02
067600000000     C                   Exsr      RIEKEY
067700000000     C                   Z-Add     $ULK          $ULKD3            3 0
067800000000     C                   MoveA     $FC           $FC3
067900000000     C                   MoveL     $KEY1         Z3KE1
068000000000     C                   MoveL     $ALLFUNCT     H3ALLFUNCT
068100000000     C                   Write     FMTZ3
068200000000     C*
068300000000     C                   EndSr
068400000000     C************************************************************
068500000000     C* CONTROLLO VIDEATA
068600000000     C************************************************************
068700000000     C     CtrD1         BegSr
068800000000     C*
068900000000     C                   SETOFF                                       99
069000130809     C* non imposto pos. del cursore
069100130809     C                   Z-Add     0             H1RIGA
069200130809     C                   Z-Add     0             H1COLO
069300130809     C*
069400130809     C* anno
069500130809     C                   Exsr      CtrAnno
069501160620R218 C*
069502160617  "  C                   If        D1RaggRGT <> XScOn and
069503160617  "  C                             D1RaggRGT <> XScOff
069504160617  "  C                   SetOn                                        5199
069505160617  "  C                   EndIf
069506160617  "  C*
069507160617  "  C* Provenienza indirizzo
069508160617  "  C                   Clear                   D1IndirD
069509160617  "  C                   RESET                   X07ValDS
069510160617  "  C                   MOVE      '1'           X07RIC
069511160617  "  C                   MOVEL     'ANDIZ000'    X07TRC
069512160617  "  C                   MOVEL     'DSPINDIR'    X07CAM
069513160617  "  C                   MOVEL     D1Indir       X07VAL
069514160617  "  C                   CALLb     'X07VALR'
069515160617  "  C                   PARM                    X07ValDS
069516160617  "  C     X07ERR        IFEQ      '0'
069517160617  "  C                   MOVEL     X07DES        D1Indird
069518160617  "  C                   ELSE
069519160617  "  C                   SETON                                        9952
069520160617  "  C                   ENDIF
069521160617  "  C*
069522160617  "  C* controllo flag di esclusione da Ammontare lordo
069523160617  "  C                   If        D1EscItc  <> XScOn and
069524160617  "  C                             D1EscItc  <> XScOff
069525160617  "  C                   SetOn                                        5799
069526160617  "  C                   EndIf
069527160617  "  C*
069528160617  "  C* controllo flag di esclusione da Imponibile non soggetto
069529160617  "  C                   If        D1EscImn  <> XScOn and
069530160617  "  C                             D1EscImn  <> XScOff
069531160617  "  C                   SetOn                                        5899
069532160617  "  C                   EndIf
069533160620R218 C*
069600130809     C*
071000000000     C                   EndSr
071100000000     C***********************************************************
071200000000     C* Controllo anno
071300000000     C***********************************************************
071400000000     C     CtrAnno       BegSr
071500000000     C*
071600000000     C                   If        D1Anno      = 0
071700000000     C                   Seton                                        5099
071800000000     C                   EndIf
071900000000     C*
072000000000     C                   EndSr
132600000000     C************************************************************
132700000000     C* GESTIONE VALORI ASSUNTI
132800000000     C************************************************************
132900000000     C     GESD3         BegSr
133000000000     C*
133100000000     C     $INZD3        IfEq      *ON
133200000000     C                   Exsr      INZD3
133300000000     C                   Move      *OFF          $INZD3
133400000000     C                   EndIf
133500000000     C*
133600000000     C     $LastV        IFNE      'D3'
133700000000     C                   Exsr      GESPIE
133800000000     C                   Move      $LastV        $LastG
133900000000     C                   Move      'D3'          $LastV
134000000000     C                   EndIf
134100000000     C*
134200000000     C* se esistono errori sulla videata
134300000000     C* emetto la Write del formato a indicatori spenti per vedere
134400000000     C* le eventuali decodifiche
134500000000     C     *IN99         IfEq      *ON
134600000000     C                   MoveA     *IN           WIN
134700000000     C                   Move      *ALL'0'       IN5098           49
134800000000     C                   MoveA     IN5098        *IN(50)
134900000000     C                   Write     FMTD3
135000000000     C                   MoveA     WIN           *IN
135100000000     C                   EndIf
135200000000     C*
135300000000     C                   EXFMT     FMTD3
135400000000     C*
135500000000     C* controllo abilitazione tasti funzione
135600000000     C                   Exsr      CTRKD3
135700000000     C*
135800000000     C     *IN99         IfEq      *OFF
135900000000     C*
136000000000     C                   Select
136100000000     C* F3=Fine
136200000000     C     $Tasto        WhenEq    F03
136300000000     C                   Exsr      F03D1
136400000000     C* F12=Ritorno
136500000000     C     $Tasto        WhenEq    F12
136600000000     C                   Exsr      F12D3
136700000000     C* F24=Altri tasti
136800000000     C     $Tasto        WhenEq    F24
136900000000     C                   Exsr      F24D3
137000000000     C*
137100000000     C                   Other
137200000000     C                   Exsr      CTRD3
137300000000     C     *IN99         IfEq      *OFF
137400000000     C                   Move      $LastG        $GEST
137500000000     C                   EndIf
137600000000     C*
137700000000     C                   EndSl
137800000000     C*
137900000000     C                   EndIf
138000000000     C*
138100000000     C                   EndSr
138200000000     C************************************************************
138300000000     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
138400000000     C************************************************************
138500000000     C     INZD3         BegSr
138600000000     C*
138700000000     C                   Clear                   FMTD3
138800000000     C     $VAL          IfEq      *ON
138900000000     C                   Move      XSCON         D3VAL
139000000000     C                   ElSe
139100000000     C                   Move      XSCOFF        D3VAL
139200000000     C                   EndIf
139300000000     C                   Move      XSCON         D3ON
139400000000     C                   Move      XSCOFF        D3OF
139500000000     C*
139600000000     C                   EndSr
142300000000     C************************************************************
142400000000     C* GESTIONE F12 VIDEO D3
142500000000     C************************************************************
142600000000     C     F12D3         BegSr
142700000000     C*
142800000000     C                   Move      $LastG        $GEST
142900000000     C*
143000000000     C                   EndSr
159700150204     C/EJECT
159800130913     C************************************************************
159900130913     C* CONTROLLO VIDEATA  D3
160000130913     C************************************************************
160100130913     C     CTRD3         BegSr
160200130913     C*
160300000000     C                   SETOFF                                       99
160400000000     C* non imposto pos. del cursore
160500000000     C                   Z-Add     0             H1RIGA
160600000000     C                   Z-Add     0             H1COLO
160700000000     C*
160800000000     c* Controllo VALORE
160900000000     C     D3VAL         IFNE      XSCON
161000000000     C     D3VAL         ANDNE     XSCOFF
161100000000     C                   Seton                                        5099
161200000000     C                   EndIf
161300000000     C*
161400000000     C     D3VAL         IfEq      XSCON
161500000000     C                   Move      *ON           $VAL
161600000000     C                   ElSe
161700000000     C                   Move      *OFF          $val
161800000000     c                   EndIf
161900000000     C*
162000000000     C                   EndSr
162100000000     C************************************************************
162200000000     C* ESECUZIONE LANCIO
162300000000     C************************************************************
162400130809     C     ExeLan        BegSr
162500130809     C*
162600160525     C                   Reset                   NDCR70DS
162902160620R218 C*
162903160617  "  C                   move      XscSoc        socR70
162904160617  "  C                   z-add     d1anno        annoR70
162905160617  "  C                   move      *on           flagDR70
162906160617  "  C                   move      *on           flagD1R70
162907160617  "  C                   move      *on           flagDBR70
162908160617  "  C                   If        D1RaggRGT = XScOn
162909160617  "  C                   Move      *On           RaggRGTR70
162910160617  "  C                   Else
162911160617  "  C                   Move      *Off          RaggRGTR70
162912160617  "  C                   EndIf
162913160617  "  C                   move      D1Indir       IndiRR70
162914160617  "  C                   If        D1EscItc  = XScOn
162915160617  "  C                   Move      *On           EscItcR70
162916160617  "  C                   Else
162917160617  "  C                   Move      *Off          EscItcR70
162918160617  "  C                   EndIf
162919160617  "  C                   If        D1EscImn  = XScOn
162920160617  "  C                   Move      *On           EscImnR70
162921160617  "  C                   Else
162922160617  "  C                   Move      *Off          EscImnR70
162923160620R218 C                   EndIf
163800160617     C*
163900160527     C                   MoveL     NDCR70DS      KPJBU
164000000000     C*
164001160603     C                   Movel     *off          $Debug            1
164002160603     C*
164003160603     C                   If        $Debug = '1'
164100160603     C                   CALL      'SDGSK02C'
164200000000     C                   PARM                    KPJBA
164201160603     C*
164202160603     C                   Else
164203160603     C*
164204160603     C                   MOVE      'SK02'        KCOAZ
164205160603     C                   CALL      'BCH10'
164206160603     C                   PARM                    KPJBA
164300000000     C*
164301160603     C                   Endif
164800000000     C*
164900000000     C* aggiorno valori assunti
165000000000     C                   Exsr      AGGMPE
165001160603     C* vado a fine
165002160603     C                   MOVEL     *on           $Fine
165100000000     C*
165200000000     C                   EndSr
167400000000     C************************************************************
167500000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
167600000000     C************************************************************
167700000000     C     CTRKD1        BegSr
167800000000     C*
167900000000     C                   SETOFF                                       99
168000000000     C                   MoveA     $FC1          $FC
168100000000     C                   Exsr      CTRKEY
168200000000     C     *IN99         IfEq      *ON
168300000000     C                   Seton                                        97
168400000000     C                   EndIf
168500000000     C*
168600000000     C                   EndSr
170000000000     C************************************************************
170100000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
170200000000     C* INPUT
170300000000     C* $FC3 = Abilitazioni videata D3
170400000000     C* OUTPUT
170500000000     C* *IN97 *IN99 Errori
170600000000     C************************************************************
170700000000     C     CTRKD3        BegSr
170800000000     C*
170900000000     C                   SETOFF                                       99
171000000000     C                   MoveA     $FC3          $FC
171100000000     C                   Exsr      CTRKEY
171200000000     C     *IN99         IfEq      *ON
171300000000     C                   Seton                                        97
171400000000     C                   EndIf
171500000000     C*
171600000000     C                   EndSr
171700000000     C************************************************************
171800000000     C* CONTROLLO ABILITAZIONE TASTI
171900000000     C************************************************************
172000000000     C     CTRKEY        BegSr
172100000000     C*
172200000000     C                   Select
172300000000     C     $Tasto        WhenEq    F01
172400000000     C     $FC(1)        IfEq      '0'
172500000000     C                   Seton                                        99
172600000000     C                   EndIf
172700000000     C     $Tasto        WhenEq    F02
172800000000     C     $FC(2)        IfEq      '0'
172900000000     C                   Seton                                        99
173000000000     C                   EndIf
173100000000     C     $Tasto        WhenEq    F03
173200000000     C     $FC(3)        IfEq      '0'
173300000000     C                   Seton                                        99
173400000000     C                   EndIf
173500000000     C     $Tasto        WhenEq    F04
173600000000     C     $FC(4)        IfEq      '0'
173700000000     C                   Seton                                        99
173800000000     C                   EndIf
173900000000     C     $Tasto        WhenEq    F05
174000000000     C     $FC(5)        IfEq      '0'
174100000000     C                   Seton                                        99
174200000000     C                   EndIf
174300000000     C     $Tasto        WhenEq    F06
174400000000     C     $FC(6)        IfEq      '0'
174500000000     C                   Seton                                        99
174600000000     C                   EndIf
174700000000     C     $Tasto        WhenEq    F07
174800000000     C     $FC(7)        IfEq      '0'
174900000000     C                   Seton                                        99
175000000000     C                   EndIf
175100000000     C     $Tasto        WhenEq    F08
175200000000     C     $FC(8)        IfEq      '0'
175300000000     C                   Seton                                        99
175400000000     C                   EndIf
175500000000     C     $Tasto        WhenEq    F09
175600000000     C     $FC(09)       IfEq      '0'
175700000000     C                   Seton                                        99
175800000000     C                   EndIf
175900000000     C     $Tasto        WhenEq    F10
176000000000     C     $FC(10)       IfEq      '0'
176100000000     C                   Seton                                        99
176200000000     C                   EndIf
176300000000     C     $Tasto        WhenEq    F11
176400000000     C     $FC(11)       IfEq      '0'
176500000000     C                   Seton                                        99
176600000000     C                   EndIf
176700000000     C     $Tasto        WhenEq    F12
176800000000     C     $FC(12)       IfEq      '0'
176900000000     C                   Seton                                        99
177000000000     C                   EndIf
177100000000     C     $Tasto        WhenEq    F13
177200000000     C     $FC(13)       IfEq      '0'
177300000000     C                   Seton                                        99
177400000000     C                   EndIf
177500000000     C     $Tasto        WhenEq    F14
177600000000     C     $FC(14)       IfEq      '0'
177700000000     C                   Seton                                        99
177800000000     C                   EndIf
177900000000     C     $Tasto        WhenEq    F15
178000000000     C     $FC(15)       IfEq      '0'
178100000000     C                   Seton                                        99
178200000000     C                   EndIf
178300000000     C     $Tasto        WhenEq    F16
178400000000     C     $FC(16)       IfEq      '0'
178500000000     C                   Seton                                        99
178600000000     C                   EndIf
178700000000     C     $Tasto        WhenEq    F17
178800000000     C     $FC(17)       IfEq      '0'
178900000000     C                   Seton                                        99
179000000000     C                   EndIf
179100000000     C     $Tasto        WhenEq    F18
179200000000     C     $FC(18)       IfEq      '0'
179300000000     C                   Seton                                        99
179400000000     C                   EndIf
179500000000     C     $Tasto        WhenEq    F19
179600000000     C     $FC(19)       IfEq      '0'
179700000000     C                   Seton                                        99
179800000000     C                   EndIf
179900000000     C     $Tasto        WhenEq    F20
180000000000     C     $FC(20)       IfEq      '0'
180100000000     C                   Seton                                        99
180200000000     C                   EndIf
180300000000     C     $Tasto        WhenEq    F21
180400000000     C     $FC(21)       IfEq      '0'
180500000000     C                   Seton                                        99
180600000000     C                   EndIf
180700000000     C     $Tasto        WhenEq    F22
180800000000     C     $FC(22)       IfEq      '0'
180900000000     C                   Seton                                        99
181000000000     C                   EndIf
181100000000     C     $Tasto        WhenEq    F23
181200000000     C     $FC(23)       IfEq      '0'
181300000000     C                   Seton                                        99
181400000000     C                   EndIf
181500000000     C     $Tasto        WhenEq    F24
181600000000     C     $FC(24)       IfEq      '0'
181700000000     C                   Seton                                        99
181800000000     C                   EndIf
181900000000     C                   EndSl
182000000000     C*
182100000000     C                   EndSr
182200000000     C************************************************************
182300000000     C* OPERAZIONI INIZIALI
182400000000     C************************************************************
182500000000     C     *INZSR        BegSr
182600000000     C*
182700000000     C* Reperimento parametri
182800000000     C*
182900000000     C     *ENTRY        PLIST
183000000000     C                   PARM                    KPJBA
183100150210     C*
183200150210     C* reperisco DATFMT del job
183300150210     C                   CallB     'XDATFMT'
183400150210     C                   Parm                    $DatFmt           4
183500000000     C*
183600000000     C* Reperimento tasti di funzione
183700000000     C*
183800000000     C                   Exsr      REPKEY
183900000000     C*
184000000000     C                   EndSr
184100000000     C************************************************************
184200000000     C* REPERIMENTO TASTI DI FUNZIONE
184300000000     C************************************************************
184400000000     C     REPKEY        BegSr
184500000000     C*
184600000000     C                   MoveA     $FM1          $FM
184700000000     C                   MoveA     $Fc1          $Fc
184800000000     C                   Exsr      REPKKK
184900000000     C                   MoveA     $FX           $FX1
185500000000     C*
185600000000     C                   MoveA     $FM3          $FM
185700000000     C                   MoveA     $Fc3          $Fc
185800000000     C                   Exsr      REPKKK
185900000000     C                   MoveA     $FX           $FX3
186000000000     C*
186100000000     C                   EndSr
186200000000     C************************************************************
186300000000     C* REPERIMENTO TASTI DI FUNZIONE
186400000000     C************************************************************
186500000000     C     REPKKK        BegSr
186600000000     C*
186700000000     C                   Clear                   $FX
186800000000     C     1             DO        24            X
186900000000     C*
187000000000     C     $FM(X)        IFNE      *BLANK
187100000000     C     $fc(X)        andne     '0'
187200000000     C*
187300000000     C                   MoveL     $FM(X)        IDMSG
187400000000     C                   CALLB     'XRTVM'
187500000000     C                   PARM                    IDMSG            27
187600000000     C                   PARM                    TXTMSG          100
187700000000     C                   PARM                    ERRMSG            1
187800000000     C*
187900000000     C     ERRMSG        IFNE      *ON
188000000000     C                   MoveL     TXTMSG        $FX(X)
188100000000     C                   ElSe
188200000000     C                   MoveL     *ALL' '       $FX(X)
188300000000     C                   EndIf
188400000000     C*
188500000000     C                   EndIf
188600000000     C*
188700000000     C                   ENDDO
188800000000     C*
188900000000     C                   EndSr
189000000000     C************************************************************
189100000000     C* INIZIALIZZAZIONE VARIABILI
189200000000     C************************************************************
189300000000     C     INZVAR        BegSr
189400000000     C*
189500160525     C                   MoveL     KPJBU         SDGSK01DS
189600150204     C*
189700000000     C* se il pgm e stato richiamato imposto i valori ricevuti dalla DS
189800160525     C                   If        SocietaK01 <> *Blanks
189900000000     C                   Eval      CallPgm     = '1'
190000000000     C                   ElSe
190100000000     C                   Eval      CallPgm     = '0'
190200000000     C                   EndIf
190300000000     C*
190400000000     C* Reperimento dati societa
190500000000     C*
190600000000     C                   MoveL     'SOC001'      TIPXSC
190700160525     C                   MoveL     SocietaK01    SOCXSC
190800000000     C                   Exsr      REPSOC
190900000000     C     RTNXSC        IFNE      '1'
191000000000     C                   MoveL     XSOCDS        SOC001
191100000000     C                   ElSe
191200000000     C                   Return
191300000000     C                   EndIf
191400000000     C*
191500000000     C* Reperimento valori default utente
191600000000     C*
191700000000     C                   Exsr      REPDFT
191800000000     C*
191900000000     C* Definizione dei tasti funzione utilizzabili a pgm
192000000000     C*
192100000000     C                   Exsr      DEFOPZ
192200000000     C*
192300000000     C* Variabili per gestione videate
192400000000     C*
192500000000     C                   Move      'D1'          $GEST
192600000000     C                   Move      *OFF          $FINE
192700000000     C                   Move      *ON           $InzD1
192800000000     C                   Move      *BLANK        $LastG
192900000000     C                   Move      *BLANK        $LastV
193000000000     C                   Z-Add     0             $ULKD1            3 0
193100000000     C                   Z-Add     0             $ULKD2            3 0
193200000000     C                   Z-Add     0             $ULKD3            3 0
193300000000     C*
193400000000     C* Variabili appoggio
193500000000     C*
193600000000     C                   Z-Add     0             CURR
193700000000     C                   Z-Add     0             CURC
193800000000     C*
193900000000     C* Valorizzazione campi univoci testate
194000000000     C*
194100000000     C                   Clear                   FMTT1
194200000000     C                   MoveL     KNSIf         NOMSIf
194300000000     C                   MoveL     XSCDSI        NOMDIT
194400000000     C                   MOVEL     KCDAZ         NOMAZN
194500000000     C                   MOVEL     DSPGM         NOMPGM           10
194600000000     C* recupero il sottotitolo (se c'e)
194700160527     C                   Eval      T1TIT = '          Modello 770 '
197900000000     C*
198000000000     C* Inizializzazione/Decodifica prima videata
198100000000     C*
198200000000     C                   Exsr      InzD1
198300000000     C                   Move      *OFF          $InzD1
198700000000     C*
198800000000     C                   EndSr
198900000000     C************************************************************
199000000000     C* REPERIMENTO DATI SOCIETA'
199100000000     C************************************************************
199200000000     C     REPSOC        BegSr
199300000000     C*
199400000000     C                   CALL      'SDGSOCDR'
199500000000     C                   PARM                    TIPXSC            6
199600000000     C                   PARM                    SOCXSC            3
199700000000     C                   PARM                    CDSXSC            9 0
199800000000     C                   PARM                    MODXSC            3
199900000000     C                   PARM      *BLANKS       RTNXSC            1
200000000000     C                   PARM                    XSOCDS
200100000000     C                   PARM                    KPJBA
200200000000     C*
200300000000     C                   EndSr
200400000000     C************************************************************
200500000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
200600000000     C************************************************************
200700000000     C     REPDFT        BegSr
200800000000     C*
200900000000     C* preparo i parametri di chiamata al driver
201000000000     C                   Clear                   XMPEDS
201100000000     C                   EVAL      TpChiamata = '01'
201200000000     C                   EVAL      NChiavi = 5
201300000000     C                   EVAL      VLogica = 2
201400000000     C*
201500000000     C* preparo le chiavi per la chiamata al driver
201600000000     C                   Clear                   XMPEDS1
201700000000     C                   MoveL     XSCSOC        DSMPESOC
201800000000     C                   MoveL     DSPGM         DSMPEPGM
201900150203     C                   Move      'R'           DSMPEPGM
202000000000     C                   MoveL     KNMUS         DSMPEPRF
202100000000     C* Si reperiscono i dati di default a seconda delle modalita con
202200000000     C* cui il pgm e usato
202300000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
202400000000     C* MPETIP = 'PV'  pgm richiamato da menu
202500000000     C* MPETIP = 'PB'  x lancio Batch
202600000000     C                   MoveL     'PB'          DSMPETIP
202700000000     C*
202800000000     C                   CALLB     'XMPE'
202900000000     C                   PARM                    XMPEDS
203000000000     C                   PARM                    XMPEDS1
203100000000     C*
203200000000     C     Risultato     IfEq      '0'
203300000000     C                   MoveL     DSMPEPAR      DSMPE
203400000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
203500000000     C* i suoi valori di default vengono decisi a pgm
203600000000     C*
203700000000     C                   ElSe
203800000000     C                   EndIf
203900000000     C*
204000000000     C                   EndSr
217700000000     C************************************************************
217800000000     C* Aggiornamento valori assunti default
217900000000     C************************************************************
218000000000     C     Aggmpe        BegSr
218100000000     C*
218200000000     C* preparo i parametri di chiamata al driver
218300000000     C                   Clear                   XMPEDS
218400000000     C                   EVAL      TpChiamata = '03'
218500000000     C                   EVAL      NChiavi = 5
218600000000     C                   EVAL      VLogica = 2
218700000000     C*
218800000000     C* preparo le chiavi per la chiamata al driver
218900000000     C                   Clear                   XMPEDS1
219000000000     C                   MoveL     XscSoc        DSMPESOC
219100000000     C                   MoveL     DSPGM         DSMPEPGM
219200150203     C                   Move      'R'           DSMPEPGM
219300000000     C                   MoveL     KNMUS         DSMPEPRF
219400000000     C*
219500000000     C* Si memorizzano i dati di default a seconda delle modalita con
219600000000     C* cui il pgm e usato
219700000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
219800000000     C* MPETIP = 'PV'  pgm richiamato da menu
219900000000     C* MPETIP = 'PB'  x lancio Batch
220000130809     C*
220100130809     C                   MoveL     'PB'          DSMPETIP
220200130809     C* Dati in recupero
220300130809     C                   Move      D1Anno        $Anno
220301160620R218 C                   Movel     D1RAGGRGT     $RaggrGt
220302160617 "   C                   Movel     D1INDIR       $Indir
220303160617 "   C                   Movel     D1ESCITC      $EscItc
220304160620R218 C                   Movel     D1ESCIMN      $EscIMn
220800130809     C*
220900130809     C                   MoveL     DSMPE         DSMPEPAR
221000130809     C*
221100130809     C                   CALLB     'XMPE'
221200130809     C                   PARM                    XMPEDS
221300000000     C                   PARM                    XMPEDS1
221400000000     C*
221500000000     C                   EndSr
221600000000     C************************************************************
221700000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
221800000000     C************************************************************
221900000000     C     DEFOPZ        BegSr
222000000000     C*
222100000000     C                   Reset                   $FC1
222200000000     C                   Reset                   $FR1
222300000000     C                   Reset                   $FC3
222400000000     C                   Reset                   $FR3
222500000000     C*
222600000000     C                   EndSr
222700000000     C************************************************************
222800000000     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
222900000000     C************************************************************
223000000000     C     DEFCAM        BegSr
224400000000     C*
224500000000     C* Variabili per gestione videate
224600000000     C*
224700000000     C                   Move      *BLANK        $GEST             2
224800000000     C                   Move      *BLANK        $FINE             1
224900000000     C                   Move      *BLANK        $InzD1            1
225100000000     C                   Move      *BLANK        $INZD3            1
225200000000     C                   Move      *BLANK        $LastG            2
225300000000     C                   Move      *BLANK        $LastV            2
225400000000     C                   Z-Add     0             $PIED1            3 0
225500000000     C*
225600000000     C* Variabili appoggio
225700000000     C*
225800000000     C                   Z-Add     0             CURR              2 0
225900000000     C                   Z-Add     0             CURC              2 0
226000000000     C                   Move      *ZEROS        WIN              99
226100000000     C*
226200000000     C* Indici
226300000000     C                   Z-Add     0             X                 3 0
226400000000     C                   Z-Add     0             Y                 3 0
226500000000     C*
226600000000     C                   EndSr
226700000000     C************************************************************
226800000000     C* RIEMPIMENTO TASTI FUNZIONE
226900000000     C************************************************************
227000000000     C     RIEKEY        BegSr
227100000000     C*
227200000000     C* Input :
227300000000     C* - $FX = schiera con tutti i testi disponibili
227400000000     C* - $FC = schiere con i flag di validita associati ai testi
227500000000     C* - $FR = schiere con i flag di REVERSE IMAGE
227600000000     C* - $ULK = ultimo testo caricato
227700000000     C* - $L01 = Lunghezza prima riga
227800000000     C* - $L02 = Lunghezza seconda riga
227900000000     C* Output :
228000000000     C* - $ULK
228100000000     C* - $KEY1= 1ø riga di tasti funzione
228200000000     C* - $KEY2= 2ø riga di tasti funzione
228300000000     C*
228400000000     C                   CALLB     'XKEYMSG'
228500000000     C* input
228600000000     C                   PARM                    $FX
228700000000     C                   PARM                    $FC
228800000000     C                   PARM                    $FR
228900000000     C                   PARM                    $ULK              3 0
229000000000     C* output
229100000000     C                   PARM                    $KEY1            79
229200000000     C                   PARM                    $KEY2            79
229300000000     C* input
229400000000     C                   PARM                    $L01              3 0
229500000000     C                   PARM                    $L02              3 0
229600000000     C*
229700000000     C                   PARM                    $ALLFUNCT       480
229800000000     C*
229900000000     C                   EndSr
230000000000** TASTI DI FUNZIONE  D1
230100000000PROMSG    *LIBL     KEY0001  01
230200000000PROMSG    *LIBL     KEY0002  02
230300000000PROMSG    *LIBL     KEY0003  03
230400000000PROMSG    *LIBL     KEY0004  04
230500000000PROMSG    *LIBL     KEY0005  05
230600000000PROMSG    *LIBL     KEY0006  06
230700000000PROMSG    *LIBL     KEY0007  07
230800000000PROMSG    *LIBL     KEY0008  08
230900000000PROMSG    *LIBL     KEY0009  09
231000000000PROMSG    *LIBL     KEY0010  10
231100000000PROMSG    *LIBL     KEY0011  11
231200000000PROMSG    *LIBL     KEY0012  12
231300000000PROMSG    *LIBL     KEY0013  13
231400000000PROMSG    *LIBL     KEY0014  14
231500000000PROMSG    *LIBL     KEY0015  15
231600000000PROMSG    *LIBL     KEY0016  16
231700000000PROMSG    *LIBL     KEY0017  17
231800000000PROMSG    *LIBL     KEY0018  18
231900000000PROMSG    *LIBL     KEY0019  19
232000160524SDGMSG    *LIBL     KEY0020  20
232100000000PROMSG    *LIBL     KEY0021  21
232200000000PROMSG    *LIBL     KEY0022  22
232300000000PROMSG    *LIBL     KEY0023  23
232400000000PROMSG    *LIBL     KEY0024  24
232500000000**  ABILITAZIONE D1 /D2
2326000000000N  01
2327000000000N  02
2328000000001N  03
2329000000001N  04
2330000000001N  05
2331000000001N  06
2332000000000N  07
2333000000000N  08
2334000000000N  09
2335000000000N  10
2336000000000N  11
2337000000001N  12
2338000000000N  13
2339000000000N  14
2340000000000N  15
2341000000000N  16
2342000000000N  17
2343000000001N  18
2344000000000N  19
2345001605240N  20
2346001606031N  21
2347000000000N  22
2348000000000N  23   Impostare '1' se si usano le opzioni
2349000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
240000000000** TASTI DI FUNZIONE  D3
240100000000PROMSG    *LIBL     KEY0001  01
240200000000PROMSG    *LIBL     KEY0002  02
240300000000PROMSG    *LIBL     KEY0003  03
240400000000PROMSG    *LIBL     KEY0004  04
240500000000PROMSG    *LIBL     KEY0005  05
240600000000PROMSG    *LIBL     KEY0006  06
240700000000PROMSG    *LIBL     KEY0007  07
240800000000PROMSG    *LIBL     KEY0008  08
240900000000PROMSG    *LIBL     KEY0009  09
241000000000PROMSG    *LIBL     KEY0010  10
241100000000PROMSG    *LIBL     KEY0011  11
241200000000PROMSG    *LIBL     KEY0012  12
241300000000PROMSG    *LIBL     KEY0013  13
241400000000PROMSG    *LIBL     KEY0014  14
241500000000PROMSG    *LIBL     KEY0015  15
241600000000PROMSG    *LIBL     KEY0016  16
241700000000PROMSG    *LIBL     KEY0017  17
241800000000PROMSG    *LIBL     KEY0018  18
241900000000PROMSG    *LIBL     KEY0019  19
242000000000PROMSG    *LIBL     KEY0020  20
242100000000PROMSG    *LIBL     KEY0021  21
242200000000PROMSG    *LIBL     KEY0022  22
242300000000PROMSG    *LIBL     KEY0023  23
242400000000PROMSG    *LIBL     KEY0024  24
242500000000**  ABILITAZIONE D3
2426000000000N  01
2427000000000N  02
2428000000001N  03
2429000000000N  04
2430000000000N  05
2431000000000N  06
2432000000000N  07
2433000000000N  08
2434000000000N  09
2435000000000N  10
2436000000000N  11
2437000000001N  12
2438000000000N  13
2439000000000N  14
2440000000000N  15
2441000000000N  16
2442000000000N  17
2443000000000N  18
2444000000000N  19
2445000000000N  20
2446000000000N  21
2447000000000N  22
2448000000000N  23   Impostare '1' se si usano le opzioni
2449000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
