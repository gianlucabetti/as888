000100000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(QILE)
000200000000     H*PARMS BNDDIR(PJXBND PJCBND) CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500000000      *****************************************************************
000600000000      *
000700000000      *         ------------------------------------------------
000800170711      *                      Spesometro Trimestrale
000900170505      *                 Lancio creazione file telematico XML
001000000000      *         ------------------------------------------------
001100170508      *
001200170711R258  * Questa PGM è stato creato con il task R258
001300000000      *
001400000000      *****************************************************************
001500000000     F*----------------------------------------------------*
001600170711     FSDGSI60D  CF   E             WORKSTN
001700170711     F                                     RENAME(SI60T1:FMTT1)
001800170711     F                                     RENAME(SI60D1:FMTD1)
001900170711     F                                     RENAME(SI60Z1:FMTZ1)
002000170711     F                                     RENAME(SI60D3:FMTD3)
002100170711     F                                     RENAME(SI60Z3:FMTZ3)
002200170711     F                                     RENAME(SI60Z2:FMTZ2)
002300170711     F                                     RENAME(SI60D4:FMTD4)
002400000000     F                                     INFDS(DSFMT)
002500000000     F*------------
002600170807     FSDGCSF04L IF   E           K DISK
002700000000     D*----------------------------------------------------*
002800000000     D* Schiere per la gestione dei tasti di funzione
002900000000     D $FM             S             27    DIM(24)
003000000000     D $FX             S             50    DIM(24)
003100000000     D $FC             S              1    DIM(24)
003200000000     D $FR             S              1    DIM(24)
003300170721     D* Video D1/D4
003400000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003500000000     D $FX1            S             50    DIM(24) ALT($FM1)
003600000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003700000000     D $FR1            S              1    DIM(24) ALT($FC1)
003800170721     D* Video D1/D4
003900000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
004000000000     D $FX2            S             50    DIM(24) ALT($FM2)
004100000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
004200000000     D $FR2            S              1    DIM(24) ALT($FC2)
004300000000     D* Video D3 Valori assunti
004400000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
004500000000     D $FX3            S             50    DIM(24) ALT($FM3)
004600000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
004700000000     D $FR3            S              1    DIM(24) ALT($FC3)
004800000000     D*----------------------------------------------------*
004900000000     D*-------------
005000000000     D* Passaggio Parametri
005100000000     D KPJBA         E DS
005200170508     D*-------------
005300170508     D* Reperimento nome PGM
005400170508     D STATUS         SDS           333
005500170508     D  DSPGM            *PROC
005600170508     D  PARMS            *PARMS
005700170508     D*-------------
005800170508     D* Dati di ambiente ottenuti da SDGSOCDR
005900170508     D SOC001        E DS                  EXTNAME(SDGSOCDS )
006000170508     D*-------------
006100170508     D* DS Interna per dati di output di SDGSOCDR
006200170508     D XSOCDS          DS          1000
006300000000     D*-------------
006400170725     D* Parametri modulo SDSPETRI
006500170725     D SDGPA12DS     E DS
006600170725     D*-------------
006700000000     D* DS Esterna gestione messaggi
006800000000     D SDGMSGDS      E DS                  INZ
006900000000     D  STS          E                     EXTFLD(MSGSTS)
007000000000     D                                     DIM(333)
007100000000     D  JBA          E                     EXTFLD(MSGJBA)
007200000000     D                                     DIM(502)
007300110421     D*-------------
007400110421     D* Ricerca legende
007500110421     D Ana964DS      E DS
007600170508     D*-------------
007700170508     D X07VALDS      E DS
007800170505     D*-------------
007900000000     D* Valori assunti
008000000000     D XMPEDS        E DS                  INZ
008100000000     D XMPEDS1       E DS                  INZ
008200000000     D DSMPE           DS                  INZ
008300000000     D  $VAL                          1
008400000000     D  $Anno                         4S 0
008500170505     D  $Trim                         1S 0
008600170721     D  $MaxRec                       5S 0
008700170512     D*-------------
008800170515     D DsIFS           DS                  INZ
008900170512     D $Cartella                     90A
009000000000     D*-------------
009100000000     D* Ds per ricezione parametri
009200170711     D SDGSI60DS     E DS
009300170711     D  ERRI60       E                     INZ('0')
009400000000     D*-------------
009500000000     D* Passaggio Parametri al PGM di estrazione dati
009600170711     D SDGSI61DS     E DS
009700170711     D  ERRI61       E                     INZ('0')
009800170807     D*-------------
009900170807     D* Passaggio Parametri al PGM di estrazione dati
010000170807     D SDGSI82DS     E DS
010100170807     D  ERRI82       E                     INZ('0')
010200170721     D*-------------
010300170721     D* manutenzione  dati di testata
010400170721     D SDGSI74DS     E DS                  inz
010500170721     D  RETI74       E                     INZ('0')
010600170721     D  OPRI74       E                     INZ('0')
010700170721     D  ERRI74       E                     INZ('0')
010800170721     D  CMTI74       E                     INZ('0')
010900000000     D*-------------
011000000000     D DSFMT           DS
011100000000     D  $Tasto               369    369
011200000000     D  NRG                  370    370
011300000000     D  NCL                  371    371
011400000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
011500000000     D*-------------
011600000000     D* posizione cursore
011700000000     D CURSOR          DS
011800000000     D  RIRI                   1      2B 0 INZ
011900000000     D  R$$                    2      2
012000000000     D  COCO                   3      4B 0 INZ
012100000000     D  C$$                    4      4
012200000000     D*-------------
012300000000     D* Tasti di funzione
012400000000     D F01             C                   CONST(X'31')
012500000000     D F02             C                   CONST(X'32')
012600000000     D F03             C                   CONST(X'33')
012700000000     D F04             C                   CONST(X'34')
012800000000     D F05             C                   CONST(X'35')
012900000000     D F06             C                   CONST(X'36')
013000000000     D F07             C                   CONST(X'37')
013100000000     D F08             C                   CONST(X'38')
013200000000     D F09             C                   CONST(X'39')
013300000000     D F10             C                   CONST(X'3A')
013400000000     D F11             C                   CONST(X'3B')
013500000000     D F12             C                   CONST(X'3C')
013600000000     D F13             C                   CONST(X'B1')
013700000000     D F14             C                   CONST(X'B2')
013800000000     D F15             C                   CONST(X'B3')
013900000000     D F16             C                   CONST(X'B4')
014000000000     D F17             C                   CONST(X'B5')
014100000000     D F18             C                   CONST(X'B6')
014200000000     D F19             C                   CONST(X'B7')
014300000000     D F20             C                   CONST(X'B8')
014400000000     D F21             C                   CONST(X'B9')
014500000000     D F22             C                   CONST(X'BA')
014600000000     D F23             C                   CONST(X'BB')
014700000000     D F24             C                   CONST(X'BC')
014800000000     D ENTER           C                   CONST(X'F1')
014900000000     D ROLDWN          C                   CONST(X'F4')
015000000000     D ROLLUP          C                   CONST(X'F5')
015100000000     I*-------------
015200000000     I* Sottotitolo di testata
015300000000     I* se non si hanno sottotitoli, mettere tutta la costante = ' '
015400170505     D* Creazione
015500000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
015600000000     D                                          SDC0056')
015700170505     D* File per invio telematico
015800000000     D SOTTITD2        C                   CONST('SDGMSG    *LIBL-
015900000000     D                                          SDC0054')
016000000000     D*-----------
016100000000     D* Campi di lavoro
016200170725     D $Cont           S              1S 0
016300000000     D CallPgm         S              1
016400170508     D WDtImp4         S               D
016500170508     D WDtIso          S               D
016600170721     D MaxRec          C                   1000
016700130904     C*----------------------------------------------------*
016800130904     C*                MAIN LINE PROGRAM
016900130904     C*----------------------------------------------------*
017000130904     C*
017100170505     C                   EXSR      INZVAR
017200130904     C*
017300000000     C     $FINE         DOWEQ     *OFF
017400000000     C     $GEST         CASEQ     'D1'          GESD1
017500000000     C     $GEST         CASEQ     'D3'          GESD3
017600170512     C     $GEST         CASEQ     'D4'          GESD4
017700000000     C                   ENDCS
017800000000     C                   ENDDO
017900000000     C*
018000170505     C                   EXSR      ENDPGM
018100170508     C/EJECT
018200000000     C************************************************************
018300000000     C* RICERCHE
018400000000     C************************************************************
018500170505     C     ENDPGM        BEGSR
018600000000     C*
018700000000     C* se ho confermato l'uscita proseguo
018800000000     C     $Fine         IfEq      *ON
018900000000     C*
019000000000     C                   Seton                                            LR
019100000000     C                   Return
019200000000     C*
019300000000     C                   EndIf
019400000000     C*
019500170505     C                   ENDSR
019600170508     C/EJECT
019700000000     C************************************************************
019800000000     C* RICERCHE
019900000000     C************************************************************
020000170505     C     Search        BEGSR
020100000000     C*
020200000000     C* determino Riga/Colonna del cursore
020300000000     C                   Move      NRG           R$$
020400000000     C                   Move      NCL           C$$
020500000000     C                   Z-Add     RIRI          CURR
020600000000     C                   Z-Add     COCO          CURC
020700000000     C*
020800000000     C* I campi che al momento dell'F4 risultano protetti non devono
020900000000     C* essere abilitati alla funzione di ricerca
021000000000     C*
021100000000     C                   Select
021200170508     C*
021300170508     C     H1NMFL        WHENEQ    'D1TRIM'
021400170508     C                   EXSR      F4_Trim
021500170711     C*
021600170711     C*  Natura soggetto
021700170711     C     H1NMFL        WHENEQ    'D1CF'
021800170711     C                   EXSR      F4_CF
021900170711     C*
022000170711     C     H1NMFL        WHENEQ    'D1TIPINV'
022100170711     C                   EXSR      F4_TIPINV
022200170807     C*
022300170807     C     H1NMFL        WHENEQ    'D1IDFILE'
022400170807     C     H1NMFL        OREQ      'D1POSIZ'
022500170807     C                   EXSR      F4_File
022600130809     C*
022700130809     C* ricerca non abilitata in questa posizione
022800130809     C                   Other
022900130809     C                   Seton                                        9899
023000130809     C*
023100110421     C                   EndSl
023200110421     C*
023300000000     C* imposto pos. del cursore
023400000000     C                   Z-Add     CURR          H1RIGA
023500000000     C                   Z-Add     CURC          H1COLO
023600000000     C*
023700000000     C                   Move      *HIVAL        $LastV
023800000000     C*
023900170505     C                   ENDSR
024000170508     C/EJECT
024100170503     C************************************************************
024200170508     C* Ricerca trimestre di imposta
024300170503     C************************************************************
024400170508     C     F4_Trim       BEGSR
024500170503     C*
024600170503     C                   Reset                   ANA964DS
024700170503     C*
024800170503     C                   MoveL     '01'          Opz964
024900170711     C                   MoveL     'SDGCST000 '  Trc964
025000170711     C                   MoveL     'CSTTRIM   '  Cam964
025100170503     C                   MoveL     *On           All964
025200170503     C*
025300170503     C                   MoveL     ANA964DS      KPJBU
025400170503     C                   CALL      'ANA964R'
025500170503     C                   PARM                    KPJBA
025600170503     C                   MoveL     KPJBU         ANA964DS
025700170503     C*
025800170503     C                   if        Ret964     <> '2'
025900170508     C                   MoveL     Val964        D1Trim
026000170503     C                   endif
026100170503     C*
026200170505     C                   ENDSR
026300170508     C/EJECT
026400170711     C************************************************************
026500170711     C* Ricerca natura del conto
026600170711     C************************************************************
026700170711     C     F4_CF         BEGSR
026800170711     C*
026900170711     C                   Reset                   ANA964DS
027000170711     C*
027100170711     C                   MoveL     '01'          Opz964
027200170711     C                   MoveL     'ANDIZ000'    Trc964
027300170711     C                   MoveL     'DSPCLIFOR'   Cam964
027400170711     C                   MoveL     *On           All964
027500170711     C*
027600170711     C                   MoveL     ANA964DS      KPJBU
027700170711     C                   CALL      'ANA964R'
027800170711     C                   PARM                    KPJBA
027900170711     C                   MoveL     KPJBU         ANA964DS
028000170711     C*
028100170711     C                   if        Ret964     <> '2'
028200170711     C                   MoveL     Val964        D1CF
028300170711     C                   endif
028400170711     C*
028500170711     C                   ENDSR
028600170711      /EJECT
028700170711     C************************************************************
028800170711     C* Ricerca tipo invio
028900170711     C************************************************************
029000170711     C     F4_TipInv     BEGSR
029100170711     C*
029200170711     C                   Reset                   ANA964DS
029300170711     C*
029400170711     C                   MoveL     '01'          Opz964
029500170711     C                   MoveL     'SDGSI60S'    Trc964
029600170711     C                   MoveL     'TIPINVI60'   Cam964
029700170711     C                   MoveL     *On           All964
029800170711     C*
029900170711     C                   MoveL     ANA964DS      KPJBU
030000170711     C                   CALL      'ANA964R'
030100170711     C                   PARM                    KPJBA
030200170711     C                   MoveL     KPJBU         ANA964DS
030300170711     C*
030400170711     C                   if        Ret964     <> '2'
030500170711     C                   MoveL     Val964        D1TIPINV
030600170711     C                   endif
030700170711     C*
030800170711     C                   ENDSR
030900170807     C************************************************************
031000170807     C* Ricerca file telematico
031100170807     C************************************************************
031200170807     C     F4_File       BEGSR
031300170807     C*
031400170807     C                   Reset                   SDGSI82DS
031500170807     C                   z-add     D1Trim        TrimI82
031600170807     C                   z-add     D1Anno        AnnoI82
031700170807     C                   Move      D1Cf          CfI82
031800170807     C                   Move      '01'          OpzI82
031900170807     C                   Move      XScSoc        SocietaI82
032000170807     C                   Movel     D1IdFile      IdFileI82
032100170807     C*
032200170807     C                   Clear                   KPJBU
032300170807     C                   Movel     SDGSI82DS     KPJBU
032400170807     C                   Call      'SDGSI82R'
032500170807     C                   Parm                    KPJBA
032600170807     C                   Movel     KPJBU         SDGSI82DS
032700170807     C*
032800170807     C                   if        IdFileI82  <> *Blanks
032900170807     C                   MoveL     IdFileI82     D1IdFile
033000170807     C                   endif
033100170807     C*
033200170807     C                   ENDSR
033300170807     C/EJECT
033400110421     C************************************************************
033500110421     C* GESTIONE EMISSIONE TASTI FUNZIONE
033600110421     C************************************************************
033700170505     C     GesPie        BEGSR
033800110421     C*
033900110421     C                   Select
034000110421     C*
034100110421     C     $GEST         WhenEq    'D1'
034200110421     C     $ULKD1        IfEq      0
034300110421     C* riempimento opzioni
034400170505     C                   EXSR      F24D1
034500000000     C                   ElSe
034600000000     C                   Write     FMTZ1
034700000000     C                   EndIf
034800000000     C*
034900170721     C     $GEST         WhenEq    'D4'
035000000000     C     $ULKD2        IfEq      0
035100000000     C* riempimento opzioni
035200170505     C                   EXSR      F24D2
035300000000     C                   ElSe
035400000000     C                   Write     FMTZ2
035500000000     C                   EndIf
035600000000     c*
035700000000     C* Modifica valori assunti
035800000000     C     $GEST         WhenEq    'D3'
035900000000     C     $ULKD3        IfEq      0
036000170505     C                   EXSR      F24D3
036100000000     C                   ElSe
036200000000     C                   Write     FMTZ3
036300000000     C                   EndIf
036400170512     C*
036500000000     C                   EndSl
036600000000     C*
036700170505     C                   ENDSR
036800170508     C/EJECT
036900000000     C************************************************************
037000000000     C* GESTIONE VIDEO RECORD D1
037100000000     C************************************************************
037200170505     C     GesD1         BEGSR
037300170505     C*
037400000000     C* inizializzo videata
037500000000     C     $InzD1        IfEq      *ON
037600170505     C                   EXSR      InzD1
037700000000     C                   Move      *OFF          $InzD1
037800000000     C                   EndIf
037900000000     C* emissione piede videata se proveniente da altra
038000000000     C* salvataggio valore formato di provenienza
038100000000     C     $LastV        IFNE      'D1'
038200000000     C                   Write     FMTT1
038300170505     C                   EXSR      GESPIE
038400000000     C                   Move      $LastV        $LastG
038500000000     C                   Move      'D1'          $LastV
038600000000     C                   EndIf
038700000000     C*
038800000000     C* se esistono errori sulla videata
038900000000     C* emetto la Write del formato a indicatori spenti per vedere
039000000000     C* le eventuali decodifiche
039100000000     C     *IN99         IfEq      *ON
039200000000     C     $GEST         ANDEQ     'D1'
039300000000     C                   MoveA     *IN           WIN
039400000000     C                   Move      *ALL'0'       IN5098           49
039500000000     C                   MoveA     IN5098        *IN(50)
039600000000     C                   Write     FMTD1
039700000000     C                   MoveA     WIN           *IN
039800000000     C                   EndIf
039900000000     C*
040000000000     C                   EXFMT     FMTD1
040100000000     C*
040200000000     C* controllo abilitazione tasti funzione
040300170505     C                   EXSR      CTRKD1
040400000000     C*
040500000000     C     *IN99         IfEq      *OFF
040600000000     C*
040700000000     C                   Select
040800000000     C* F2=Decodifica
040900000000     C     $Tasto        WhenEq    F02
041000170505     C                   EXSR      F02D1
041100000000     C* F3=Fine
041200000000     C     $Tasto        WhenEq    F03
041300170505     C                   EXSR      F03D1
041400000000     C* F4=Ricerca
041500000000     C     $Tasto        WhenEq    F04
041600170505     C                   EXSR      SEARCH
041700000000     C* F5=Refresh
041800000000     C     $Tasto        WhenEq    F05
041900170505     C                   EXSR      F05D1
042000000000     C* F8=Successivo
042100000000     C     $Tasto        WhenEq    F08
042200170505     C                   EXSR      F08D1
042300000000     C* F12=Ritorno
042400000000     C     $Tasto        WhenEq    F12
042500170505     C                   EXSR      F12D1
042600000000     C* F18=Valori assunti
042700000000     C     $Tasto        WhenEq    F18
042800170505     C                   EXSR      F18D1
042900170512     C* F20=Dati Frontespizio
043000000000     C     $Tasto        WhenEq    F20
043100170505     C                   EXSR      F20D1
043200000000     C* F21=Pers. batch
043300000000     C     $Tasto        WhenEq    F21
043400170505     C                   EXSR      F21D1
043500170805     C* F22=Dati per IFS
043600170512     C     $Tasto        WhenEq    F22
043700170512     C                   EXSR      F22D1
043800000000     C* F24=Altri tasti
043900000000     C     $Tasto        WhenEq    F24
044000170505     C                   EXSR      F24D1
044100000000     C*
044200000000     C                   Other
044300170505     C                   EXSR      CTRD1
044400000000     C* aggiungere le chiamate ai controlli delle videate supplementari
044500170512     C     *IN99         IfEq      *OFF
044600170512     C                   EXSR      CTRD4
044700170512     C     *IN99         IfEq      *ON
044800170512     C                   Seton                                            96
044900000000     C                   EndIf
045000170724     C                   EndIf
045100000000     C*
045200000000     C     $Tasto        IfEq      F06
045300000000     C     *IN99         ANDEQ     *OFF
045400000000     C*
045500170505     C                   EXSR      ExeLan
045600170505     C                   EXSR      ENDPGM
045700000000     C*
045800000000     C                   EndIf
045900000000     C*
046000000000     C                   EndSl
046100000000     C*
046200000000     C                   EndIf
046300000000     C*
046400170505     C                   ENDSR
046500170508     C/EJECT
046600000000     C************************************************************
046700000000     C* INIZIALIZZAZIONE VIDEATA DATI
046800000000     C************************************************************
046900170505     C     InzD1         BEGSR
047000000000     C*
047100000000     C                   Clear                   FMTD1
047200000000     C*
047300000000     C                   Z-Add     1             $PIED1
047400000000     C*
047401170828R264 C                   Eval      D1TipInv    = 'O'
047500110802     C* se il pgm e'stato richiamato imposto i valori ricevuti dalla DS
047600000000     C                   If        CallPgm     = '1'
047700000000     C*
047800170711     C                   EXSR      RieD1I60
047900000000     C*
048000110802     C* se il pgm NON e stato richiamato
048100000000     C                   ElSe
048200000000     C*
048300000000     C* Se vuole impostazione lancio da ultima elaborazione, li imposto da anmpe
048400000000     C     $Val          IfEq      *ON
048500000000     C* riempio la videata da parametri memorizzati
048600170505     C                   EXSR      RIED1
048700000000     C*  Aggiungere le routine di riempimento dei record succcessivi
048800170512     C                   EXSR      RIED4
048900000000     C                   ElSe
049000000000     C* Immettere i valodi di default a video
049100000000     C                   Eval      D1Anno      = *Year
049200170801     C                   Eval      D1TipInv    = 'O'
049300170721     C                   Eval      D1MaxRec    = MaxRec
049400130809     C                   EndIf
049500130809     C*
049600130809     C                   EndIf
049700130809     C*
049800170505     C                   ENDSR
049900170508     C/EJECT
050000130809     C************************************************************
050100130809     C* Riempimento videata  D1 da richiamo pgm
050200130809     C************************************************************
050300170711     C     RieD1I60      BEGSR
050400130809     C*
050500170711     C                   Z-Add     AnnoI60       D1Anno
050600170711     C                   Z-Add     TrimI60       D1Trim
050700170711     C                   move      IdFileI60     D1IdFile
050800170711     C                   Z-Add     PosizI60      D1Posiz
050900170711     C                   Eval      D1TipInv    = TipInvI60
051000170721     C                   Z-Add     MaxRecI60     D1MaxRec
051100130809     C*
051200170505     C                   ENDSR
051300170508     C/EJECT
051400130809     C************************************************************
051500130809     C* RIEMPIMENTO VIDEATA  D1
051600130809     C************************************************************
051700170505     C     RIED1         BEGSR
051800130809     C*
051900130809     C* impostare i campi della videata dai parametri memorizzati su anmpe
052000130809     C                   Z-Add     $Anno         D1Anno
052100170508     C                   Z-Add     $Trim         D1Trim
052200170721     C                   Z-Add     $MaxRec       D1MaxRec
052300130809     C*
052400170505     C                   ENDSR
052500170508     C/EJECT
052600130809     C************************************************************
052700130809     C* GESTIONE F02 VIDEO D1
052800130809     C************************************************************
052900170505     C     F02D1         BEGSR
053000130809     C*
053100130809     C* per decodificare la videata richiamo la routine dei ctrl
053200130809     C* e poi disattivo gli eventuali messaggi di errore
053300170505     C                   EXSR      CTRD1
053400130809     C*
053500130809     C                   Move      *ALL'0'       IN5098
053600130809     C                   MoveA     IN5098        *IN(50)
053700130809     C*
053800170505     C                   ENDSR
053900170508     C/EJECT
054000000000     C************************************************************
054100000000     C* GESTIONE F03 VIDEO D1
054200000000     C************************************************************
054300170505     C     F03D1         BEGSR
054400000000     C*
054500000000     C                   Move      *ON           $FINE
054600000000     C* fine programma
054700170505     C                   EXSR      ENDPGM
054800000000     C*
054900170505     C                   ENDSR
055000170508     C/EJECT
055100000000     C************************************************************
055200000000     C* GESTIONE F05 VIDEO D1
055300000000     C************************************************************
055400170505     C     F05D1         BEGSR
055500000000     C*
055600000000     C                   Move      *ON           $InzD1
055700000000     C*
055800170505     C                   ENDSR
055900170508     C/EJECT
056000000000     C************************************************************
056100000000     C* GESTIONE F08 VIDEO D1
056200000000     C************************************************************
056300170505     C     F08D1         BEGSR
056400000000     C*
056500000000     C                   Move      *ON           $FINE
056600000000     C*
056700170505     C                   ENDSR
056800170508     C/EJECT
056900000000     C************************************************************
057000000000     C* GESTIONE F12 VIDEO D1
057100000000     C************************************************************
057200170505     C     F12D1         BEGSR
057300000000     C*
057400000000     C                   Move      *ON           $FINE
057500000000     C*
057600170505     C                   ENDSR
057700170508     C/EJECT
057800000000     C************************************************************
057900000000     C* GESTIONE F18 VIDEO D1
058000000000     C************************************************************
058100170505     C     F18D1         BEGSR
058200000000     C*
058300000000     C                   Move      'D3'          $GEST
058400000000     C                   Move      *ON           $INZD3
058500000000     C*
058600170505     C                   ENDSR
058700170508     C/EJECT
058800000000     C************************************************************
058900170721     C* GESTIONE F20 VIDEO D1
059000000000     C************************************************************
059100170505     C     F20D1         BEGSR
059200000000     C*
059300170721     C                   RESET                   SDGSI74DS
059400170721     C                   eval      SocietaI74  = XscSoc
059500170721     C                   eval      OpzI74      = '20'
059600170721     C                   Eval      AnnoI74     = D1Anno
059700170721     C                   Eval      TrimI74     = D1Trim
059800170721     C*
059900170721    >C                   CALL      'SDGSI74R'
060000170721     C                   PARM                    KPJBA
060100170721    >C                   PARM                    SDGSI74DS
060200000000     C*
060300170505     C                   ENDSR
060400170508     C/EJECT
060500000000     C************************************************************
060600000000     C* GESTIONE F21 VIDEO D1
060700000000     C************************************************************
060800170505     C     F21D1         BEGSR
060900000000     C*
061000000000     C                   Move      '    '        KCOAZ
061100000000     C                   CALL      'BCH09'
061200000000     C                   PARM                    KPJBA
061300000000     C*
061400170505     C                   ENDSR
061500170508     C/EJECT
061600170512     C************************************************************
061700170512     C* GESTIONE F22 VIDEO D4
061800170512     C************************************************************
061900170512     C     F22D1         BEGSR
062000170512     C*
062100170512     C                   Move      'D4'          $GEST
062200170512     C                   Move      *ON           $INZD4
062300170512     C*
062400170512     C                   ENDSR
062500170512     C/EJECT
062600000000     C************************************************************
062700000000     C* GESTIONE F24 VIDEO D1
062800000000     C************************************************************
062900170505     C     F24D1         BEGSR
063000000000     C*
063100000000     C                   MoveA     $FX1          $FX
063200000000     C                   MoveA     $FC1          $FC
063300000000     C                   MoveA     $FR1          $FR
063400000000     C                   Z-Add     $ULKD1        $ULK
063500000000     C                   Z-Add     79            $L01
063600000000     C                   Z-Add     79            $L02
063700170505     C                   EXSR      RIEKEY
063800000000     C                   Z-Add     $ULK          $ULKD1            3 0
063900000000     C                   MoveA     $FC           $FC1
064000000000     C                   MoveL     $KEY1         Z1KE1
064100000000     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
064200000000     C                   MoveL     $KEY2         Z1KE2
064300000000     C                   Write     FMTZ1
064400000000     C*
064500170505     C                   ENDSR
064600170508     C/EJECT
064700000000     C************************************************************
064800000000     C* GESTIONE F24 VIDEO D3
064900000000     C************************************************************
065000170505     C     F24D2         BEGSR
065100000000     C*
065200000000     C                   MoveA     $FX2          $FX
065300000000     C                   MoveA     $FC2          $FC
065400000000     C                   MoveA     $FR2          $FR
065500000000     C                   Z-Add     $ULKD2        $ULK
065600000000     C                   Z-Add     79            $L01
065700000000     C                   Z-Add     79            $L02
065800170505     C                   EXSR      RIEKEY
065900000000     C                   Z-Add     $ULK          $ULKD2            3 0
066000000000     C                   MoveA     $FC           $FC2
066100000000     C                   MoveL     $KEY1         Z2KE1
066200000000     C                   MoveL     $ALLFUNCT     H3ALLFUNCT
066300000000     C                   MoveL     $KEY2         Z2KE2
066400000000     C                   Write     FMTZ2
066500000000     C*
066600170505     C                   ENDSR
066700170508     C/EJECT
066800000000     C************************************************************
066900000000     C* GESTIONE F24 VIDEO D3
067000000000     C************************************************************
067100170505     C     F24D3         BEGSR
067200000000     C*
067300000000     C                   MoveA     $FX3          $FX
067400000000     C                   MoveA     $FC3          $FC
067500000000     C                   MoveA     $FR3          $FR
067600000000     C                   Z-Add     $ULKD3        $ULK
067700000000     C                   Z-Add     44            $L01
067800000000     C                   Z-Add     0             $L02
067900170505     C                   EXSR      RIEKEY
068000000000     C                   Z-Add     $ULK          $ULKD3            3 0
068100000000     C                   MoveA     $FC           $FC3
068200000000     C                   MoveL     $KEY1         Z3KE1
068300000000     C                   MoveL     $ALLFUNCT     H3ALLFUNCT
068400000000     C                   Write     FMTZ3
068500000000     C*
068600170505     C                   ENDSR
068700170508     C/EJECT
068800000000     C************************************************************
068900000000     C* CONTROLLO VIDEATA
069000000000     C************************************************************
069100170505     C     CtrD1         BEGSR
069200000000     C*
069300000000     C                   SETOFF                                       99
069400130809     C* non imposto pos. del cursore
069500130809     C                   Z-Add     0             H1RIGA
069600130809     C                   Z-Add     0             H1COLO
069700130809     C*
069800170505     C                   EXSR      CtrAnno
069900170505     C                   EXSR      CtrTrim
070000170711     C*
070100170711     C                   EXSR      CtrCF
070200170711     C*
070300170711     C* tipo invio
070400170711     C                   Exsr      CtrTipInv
070500170711     C*
070600170711     C* id file e posizione
070700170711     C                   Exsr      CtrIdFile
070800170515     C*
070900170711     C* Reperimento nome cartella
071000170515     C                   Clear                   D1Cartella
071100170515     C                   if        *In99       = *Off
071200170515     C                   EXSR      RepCartella
071300170515     C                   endif
071400170725     C*
071500170725     C* Controllo presenza dati dichiarante
071600170725     C                   if        *In99       = *Off
071700170725     C                   EXSR      RepDatiDic
071800170725     C                   endif
071900170508     C*
072000170505     C                   ENDSR
072100170508     C/EJECT
072200000000     C***********************************************************
072300000000     C* Controllo anno
072400000000     C***********************************************************
072500170505     C     CtrAnno       BEGSR
072600170505     C*
072700170505     C                   if        D1Anno      = 0
072800000000     C                   Seton                                        5099
072900170505     C                   endif
073000000000     C*
073100170505     C                   ENDSR
073200170508     C/EJECT
073300170505     C***********************************************************
073400170505     C* Controllo trimestre
073500170505     C***********************************************************
073600170505     C     CtrTrim       BEGSR
073700170508     C*
073800170508     C                   Clear                   D1TrimD
073900170508     C*
074000170508     C                   Clear                   X07ValDs
074100170508     C*
074200170508     C                   Move      '1'           X07Ric
074300170508     C                   Move      *On           X07All
074400170711     C                   Movel     'SDGCST000 '  X07TRc
074500170711     C                   Movel     'CSTTRIM   '  X07Cam
074600170508     C                   MoveL     D1Trim        X07VAL
074700170508     C*
074800170508     C                   Callb     'X07VALR'
074900170508     C                   Parm                    X07ValDs
075000170508     C*
075100170508     C     X07ERR        IFEQ      *Off
075200170508     C                   MOVEL     X07DES        D1TrimD
075300170508     C                   else
075400170508     C                   Seton                                        5199
075500170508     C                   endif
075600170505     C*
075700170505     C                   ENDSR
075800170508     C/EJECT
075900170711     C***********************************************************
076000170711     C* Controllo tipo soggetto
076100170711     C***********************************************************
076200170711     C     CtrCF         BEGSR
076300170711     C*
076400170711     C                   Clear                   D1CFD
076500170711     C*
076600170711     C                   Clear                   X07ValDs
076700170711     C*
076800170711     C                   Move      '1'           X07Ric
076900170711     C                   Move      *On           X07All
077000170711     C                   Movel     'ANDIZ000'    X07TRc
077100170711     C                   Movel     'DSPCLIFOR'   X07Cam
077200170711     C                   Movel     D1CF          X07VAL
077300170711     C*
077400170711     C                   Callb     'X07VALR'
077500170711     C                   Parm                    X07ValDs
077600170711     C*
077700170711     C     X07ERR        IFEQ      *Off
077800170711     C                   MOVEL     X07DES        D1CFD
077900170711     C                   else
078000170711     C                   SetOn                                        5499
078100170711     C                   endif
078200170711     C*
078300170711     C                   ENDSR
078400170711     C/EJECT
078500170711     C***********************************************************
078600170711     C* Controllo tipo invio
078700170711     C***********************************************************
078800170711     C     CtrTipInv     BegSr
078900170711     C*
079000170711     C                   Clear                   D1TipInvD
079100170711     C*
079200170711     C                   Clear                   X07ValDs
079300170711     C*
079400170711     C                   Move      '1'           X07Ric
079500170711     C                   Move      *On           X07All
079600170711     C                   Movel     'SDGSI60S'    X07TRc
079700170711     C                   Movel     'TIPINVI60'   X07Cam
079800170711     C                   Movel     D1TIPINV      X07VAL
079900170711     C*
080000170711     C                   Callb     'X07VALR'
080100170711     C                   Parm                    X07ValDs
080200170711     C*
080300170711     C     X07ERR        IFEQ      *Off
080400170711     C                   MOVEL     X07DES        D1TipInvD
080500170711     C                   ElSe
080600170711     C                   SetOn                                        5299
080700170711     C                   EndIf
080800170711     C*
080900170711     C                   EndSr
081000170711     C***********************************************************
081100170711     C* Controllo id file
081200170711     C***********************************************************
081300170711     C     CtrIdFile     BEGSR
081400170711     C*
081500170711     C* ITAAABBB99T99X999W_DF_00001
081600170711     C* Id File se richiesta rettifica o annullamento
081700170801     C                   If        D1TipInv   <> 'O'  and
081800170711     C                             D1IdFile   = *blank
081900170711     C                   SetOn                                        6199
082000170711     C                   EndIf
082100170801     C                   If        D1TipInv   = 'O'  and
082200170711     C                             D1IdFile   <> *blank
082300170711     C                   SetOn                                        6599
082400170711     C                   EndIf
082500170807     C*
082600170807     C* Id File obbligatorio se richiesta rettifica o annullamento
082700170807     C                   If        (D1TipInv   = 'R'   or
082800170807     C                              D1TipInv   = 'A')  and
082900170807     C                             D1IdFile   = *blank
083000170807     C                   SetOn                                        6199
083100170807     C                   EndIf
083200170711     C*
083300170807     C* Posizione obbligatoria se richiesta rettifica
083400170807     C                   If        D1TipInv   = 'R'   and
083500170711     C                             D1Posiz    = 0
083600170807     C                   SetOn                                        6899
083700170711     C                   EndIf
083800170807     C*
083900170801     C                   If        D1TipInv  <> 'R'  and
084000170801     C                             D1TipInv  <> 'A'  and
084100170711     C                             D1Posiz    > 0
084200170711     C                   SetOn                                        6999
084300170711     C                   EndIf
084400170711     C*
084500170801     C*  Controllo assegnazione id file
084600170801     C                   if        *in99 = *off
084700170801     C                   if        D1IdFile <> *blank
084800170807     C                   eval      CsfSocieta = XscSoc
084900170807     C                   eval      CsfIdFile  = D1IdFile
085000170807     C     K02Csf04      chain     SdgCsf04l
085100170807     C                   if        not %found(sdgcsf04l)
085200170801     C                   seton                                        6099
085300170801     C                   endif
085400170801     C                   endif
085500170801     C                   endIf
085600170801     C*
085700170711     C                   EndSr
085800170515     C************************************************************
085900170515     C* REPERIMENTO indirizzario su IFS
086000170515     C************************************************************
086100170515     C     RepCartella   BEGSR
086200170515     C*
086300170515     C                   EXSR      RepIFS
086400170515     C*
086500170515     C                   Clear                   DsIFS
086600170515     C*
086700170515     C     Risultato     IfEq      '0'
086800170515     C*
086900170515     C                   MoveL     DSMPEPAR      DsIFS
087000170515     C*
087100170515     C                   MoveL(P)  $Cartella     D1Cartella
087200170515     C*
087300170515     C                   else
087400170515     C*
087500170515     C                   Eval      D1Cartella  = *All'?'
087600170515     C*
087700170515     C                   endif
087800170515     C*
087900170515     C                   ENDSR
088000170515     C/EJECT
088100170515     C************************************************************
088200170515     C* REPERIMENTO indirizzario su IFS
088300170515     C************************************************************
088400170515     C     RepIFS        BEGSR
088500170515     C*
088600170515     C* preparo i parametri di chiamata al driver
088700170515     C                   Clear                   XMPEDS
088800170515     C                   EVAL      TpChiamata = '01'
088900170515     C                   EVAL      NChiavi = 5
089000170515     C                   EVAL      VLogica = 2
089100170515     C*
089200170515     C* preparo le chiavi per la chiamata al driver
089300170515     C                   Clear                   XMPEDS1
089400170515     C                   MoveL     XSCSOC        DSMPESOC
089500170711     C                   MoveL     'SDGSI60R  '  DSMPEPGM
089600170725     C                   Move      'I'           DSMPEPGM
089700170515     C                   MoveL     *Blanks       DSMPEPRF
089800170515     C*
089900170515     C* Si reperiscono i dati di default a seconda delle modalita con
090000170515     C* cui il pgm e usato
090100170515     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
090200170515     C* MPETIP = 'PV'  pgm richiamato da menu
090300170515     C* MPETIP = 'PB'  x lancio Batch
090400170515     C                   MoveL     'PB'          DSMPETIP
090500170515     C*
090600170515     C                   CALLB     'XMPE'
090700170515     C                   PARM                    XMPEDS
090800170515     C                   PARM                    XMPEDS1
090900170515     C*
091000170515     C                   ENDSR
091100170515     C/EJECT
091200170725     C************************************************************
091300170725     C* Controlla presenza dati dichiarante
091400170725     C******************************************************
091500170725     C     RepDatiDic    BEGSR
091600170725     C*
091700170725     C                   Clear                   XMPEDS
091800170725     C                   EVAL      TpChiamata = '01'
091900170725     C                   EVAL      NChiavi = 5
092000170725     C                   EVAL      VLogica = 2
092100170725     C*
092200170725     C                   Do        4             $Cont
092300170725     C                   Clear                   XMPEDS1
092400170725     C                   Eval      DSMPEPGM = 'SDGSI74R' +
092500170725     C                                        %char($cont)
092600170725     C* preparo le chiavi per la chiamata al driver
092700170725     C                   MoveL     XSCSOC        DSMPESOC
092800170725     C                   MoveL     *Blanks       DSMPEPRF
092900170725     C* Si reperiscono i dati di default a seconda delle modalità con
093000170725     C* cui il pgm è usato
093100170725     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
093200170725     C* MPETIP = 'PV'  pgm richiamato da menù
093300170725     C* MPETIP = 'PB'  x lancio Batch
093400170725     C                   MoveL     'PV'          DSMPETIP
093500170725     C*
093600170725     C                   CALLB     'XMPE'
093700170725     C                   PARM                    XMPEDS
093800170725     C                   PARM                    XMPEDS1
093900170725     C*
094000170725     C     Risultato     IfEq      '0'
094100170725     C*
094200170725     C                   else
094300170725     C*
094400170809     C                   seton                                        6499
094500170725     C                   leave
094600170725     C                   endif
094700170725     C*
094800170725     C                   Enddo
094900170725     C*
095000170725     C                   Endsr
095100170725     C/EJECT
095200000000     C************************************************************
095300000000     C* GESTIONE VALORI ASSUNTI
095400000000     C************************************************************
095500170505     C     GESD3         BEGSR
095600000000     C*
095700000000     C     $INZD3        IfEq      *ON
095800170505     C                   EXSR      INZD3
095900000000     C                   Move      *OFF          $INZD3
096000000000     C                   EndIf
096100000000     C*
096200000000     C     $LastV        IFNE      'D3'
096300170505     C                   EXSR      GESPIE
096400000000     C                   Move      $LastV        $LastG
096500000000     C                   Move      'D3'          $LastV
096600000000     C                   EndIf
096700000000     C*
096800000000     C* se esistono errori sulla videata
096900000000     C* emetto la Write del formato a indicatori spenti per vedere
097000000000     C* le eventuali decodifiche
097100000000     C     *IN99         IfEq      *ON
097200000000     C                   MoveA     *IN           WIN
097300000000     C                   Move      *ALL'0'       IN5098           49
097400000000     C                   MoveA     IN5098        *IN(50)
097500000000     C                   Write     FMTD3
097600000000     C                   MoveA     WIN           *IN
097700000000     C                   EndIf
097800000000     C*
097900000000     C                   EXFMT     FMTD3
098000000000     C*
098100000000     C* controllo abilitazione tasti funzione
098200170505     C                   EXSR      CTRKD3
098300000000     C*
098400000000     C     *IN99         IfEq      *OFF
098500000000     C*
098600000000     C                   Select
098700000000     C* F3=Fine
098800000000     C     $Tasto        WhenEq    F03
098900170505     C                   EXSR      F03D1
099000000000     C* F12=Ritorno
099100000000     C     $Tasto        WhenEq    F12
099200170505     C                   EXSR      F12D3
099300000000     C* F24=Altri tasti
099400000000     C     $Tasto        WhenEq    F24
099500170505     C                   EXSR      F24D3
099600000000     C*
099700000000     C                   Other
099800170505     C                   EXSR      CTRD3
099900000000     C     *IN99         IfEq      *OFF
100000000000     C                   Move      $LastG        $GEST
100100000000     C                   EndIf
100200000000     C*
100300000000     C                   EndSl
100400000000     C*
100500000000     C                   EndIf
100600000000     C*
100700170505     C                   ENDSR
100800170508     C/EJECT
100900000000     C************************************************************
101000000000     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
101100000000     C************************************************************
101200170505     C     INZD3         BEGSR
101300000000     C*
101400000000     C                   Clear                   FMTD3
101500000000     C     $VAL          IfEq      *ON
101600000000     C                   Move      XSCON         D3VAL
101700000000     C                   ElSe
101800000000     C                   Move      XSCOFF        D3VAL
101900000000     C                   EndIf
102000000000     C                   Move      XSCON         D3ON
102100000000     C                   Move      XSCOFF        D3OF
102200000000     C*
102300170505     C                   ENDSR
102400170508     C/EJECT
102500170512     C************************************************************
102600170512     C* GESTIONE VIDEO RECORD  D4
102700170512     C************************************************************
102800170512     C     GESD4         BEGSR
102900170512     C*
103000170512     C* inizializzo videata
103100170512     C     $InzD4        IfEq      *ON
103200170512     C                   EXSR      InzD4
103300170512     C                   Move      *OFF          $InzD4
103400170512     C                   EndIf
103500170512     C*
103600170512     C     $LastV        IFNE      'D4'
103700170512     C                   Write     FMTT1
103800170512     C                   EXSR      GESPIE
103900170512     C                   Move      $LastV        $LastG
104000170512     C                   Move      'D4'          $LastV
104100170512     C                   EndIf
104200170512     C*
104300170512     C* se esistono errori sulla videata
104400170512     C* emetto la Write del formato a indicatori spenti per vedere
104500170512     C* le eventuali decodifiche
104600170512     C     *IN99         IfEq      *ON
104700170512     C     $GEST         ANDEQ     'D4'
104800170512     C                   MoveA     *IN           WIN
104900170512     C                   Move      *ALL'0'       IN5098           49
105000170512     C                   MoveA     IN5098        *IN(50)
105100170512     C                   Write     FMTD4
105200170512     C                   MoveA     WIN           *IN
105300170512     C                   EndIf
105400170512     C*
105500170512     C                   EXFMT     FMTD4
105600170512     C*
105700170512     C* controllo abilitazione tasti funzione
105800170512     C                   EXSR      CTRKD2
105900170512     C*
106000170512     C     *IN99         IfEq      *OFF
106100170512     C*
106200170512     C                   Select
106300170512     C* F3=Fine
106400170512     C     $Tasto        WhenEq    F03
106500170512     C                   EXSR      F03D1
106600170512     C* F4=Ricerca
106700170512     C     $Tasto        WhenEq    F04
106800170512     C                   EXSR      SEARCH
106900170512     C* F5=Refresh
107000170512     C     $Tasto        WhenEq    F05
107100170512     C                   EXSR      F05D4
107200170512     C* F8=Successivo
107300170512     C     $Tasto        WhenEq    F08
107400170512     C                   EXSR      F08D1
107500170512     C* F12=Ritorno
107600170512     C     $Tasto        WhenEq    F12
107700170512     C                   EXSR      F12D2
107800170512     C* F18=Valori assunti
107900170512     C     $Tasto        WhenEq    F18
108000170512     C                   EXSR      F18D1
108100170512     C* F21=Pers. batch
108200170512     C     $Tasto        WhenEq    F21
108300170512     C                   EXSR      F21D1
108400170512     C* F24=Altri tasti
108500170512     C     $Tasto        WhenEq    F24
108600170512     C                   EXSR      F24D1
108700170512     C*
108800170512     C                   Other
108900170512     C                   EXSR      CTRD4
109000170512     C*
109100170512     C     $Tasto        IfEq      F06
109200170512     C     *IN99         ANDEQ     *OFF
109300170512     C*
109400170512     C                   EXSR      AggD4
109500170512     C                   Move      'D1'          $GEST
109600170512     C*
109700170512     C                   EndIf
109800170512     C*
109900170512     C                   EndSl
110000170512     C*
110100170512     C                   EndIf
110200170512     C*
110300170512     C                   ENDSR
110400170512     C/EJECT
110500170512     C************************************************************
110600170512     C* REPERIMENTO dati per Trasferimento dati a IFS
110700170512     C************************************************************
110800170512     C     InzD4         BEGSR
110900170512     C*
111000170512     C                   Clear                   FMTD4
111100170512     C*
111200170515     C                   EXSR      RepIFS
111300170512     C*
111400170515     C                   Clear                   DsIFS
111500170512     C*
111600170512     C     Risultato     IfEq      '0'
111700170512     C*
111800170515     C                   MoveL     DSMPEPAR      DsIFS
111900170512     C*
112000170512     C                   MoveL(P)  $Cartella     D4Cartella
112100170512     C*
112200170512     C                   endif
112300170512     C*
112400170512     C                   ENDSR
112500170512     C/EJECT
112600000000     C************************************************************
112700000000     C* GESTIONE F05 VIDEO D2
112800000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
112900000000     C* e il riposizionamento alla prima
113000000000     C************************************************************
113100170505     C     F05D2         BEGSR
113200000000     C*
113300150204     C                   Move      *ON           $InzD2
113400150204     C                   Move      'D2'          $GEST
113500000000     C*
113600170505     C                   ENDSR
113700170508     C/EJECT
113800000000     C************************************************************
113900000000     C* GESTIONE F12 VIDEO D2
114000000000     C************************************************************
114100170505     C     F12D2         BEGSR
114200000000     C*
114300000000     C                   Move      'D1'          $GEST
114400000000     C*
114500170505     C                   ENDSR
114600170508     C/EJECT
114700000000     C************************************************************
114800000000     C* GESTIONE F12 VIDEO D3
114900000000     C************************************************************
115000170505     C     F12D3         BEGSR
115100000000     C*
115200000000     C                   Move      $LastG        $GEST
115300000000     C*
115400170505     C                   ENDSR
115500170508     C/EJECT
115600170512     C************************************************************
115700170512     C* RIEMPIMENTO VIDEATA D4
115800170512     C************************************************************
115900170512     C     RIED4         BEGSR
116000170512     C*
116100170512     C*
116200170512     C                   ENDSR
116300170512     C/EJECT
116400170512     C************************************************************
116500170512     C* GESTIONE F05 VIDEO D4
116600170512     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
116700170512     C* e il riposizionamento alla prima
116800170512     C************************************************************
116900170512     C     F05D4         BEGSR
117000170512     C*
117100170512     C                   Move      *ON           $InzD4
117200170512     C                   Move      'D4'          $GEST
117300170512     C*
117400170512     C                   ENDSR
117500170512     C/EJECT
117600170508     C************************************************************
117700170508     C* CONTROLLO VIDEATA  D3
117800170508     C************************************************************
117900170508     C     CTRD3         BEGSR
118000170508     C*
118100000000     C                   SETOFF                                       99
118200000000     C* non imposto pos. del cursore
118300000000     C                   Z-Add     0             H1RIGA
118400000000     C                   Z-Add     0             H1COLO
118500000000     C*
118600000000     c* Controllo VALORE
118700000000     C     D3VAL         IFNE      XSCON
118800000000     C     D3VAL         ANDNE     XSCOFF
118900000000     C                   Seton                                        5099
119000000000     C                   EndIf
119100000000     C*
119200000000     C     D3VAL         IfEq      XSCON
119300000000     C                   Move      *ON           $VAL
119400000000     C                   ElSe
119500000000     C                   Move      *OFF          $val
119600000000     c                   EndIf
119700000000     C*
119800170505     C                   ENDSR
119900170508     C/EJECT
120000170512     C************************************************************
120100170512     C* CONTROLLO VIDEATA  D4
120200170512     C************************************************************
120300170512     C     CtrD4         BEGSR
120400170512     C*
120500170512     C                   SETOFF                                       99
120600170512     C* non imposto pos. del cursore
120700170512     C                   Z-Add     0             H1RIGA
120800170512     C                   Z-Add     0             H1COLO
120900170512     C*
121000170512     C                   EXSR      CtrCartella
121100170512     C*
121200170726     C                   EXSR      CtrPath
121300170726     C*
121400170512     C                   ENDSR
121500170512     C/EJECT
121600170512     C************************************************************
121700170512     C* Controllo indirizzo cartella
121800170512     C************************************************************
121900170512     C     CtrCartella   BEGSR
122000170512     C*
122100170512     C     D4Cartella    IFEQ      *BLANK
122200170711     C                   SETON                                        7099
122300170512     C                   ENDIF
122400170512     C*
122500170512     C                   ENDSR
122600170512     C/EJECT
122700170726     C************************************************************
122800170726     C* CONTROLLO ESISTENZA PATH
122900170726     C************************************************************
123000170726     C     CtrPath       BEGSR
123100170726     C*
123200170726     C* controllo che esista il percorso indicato (specifico *DIR
123300170726     C* per indicare che deve essere una directory)
123400170726     C*
123500170726     C                   Eval      IFSObjN = D4Cartella
123600170726     C                   Eval      IFSObjT = '*DIR'
123700170726     C                   Eval      IFSErr = *Off
123800170726     C*
123900170726     C                   call      'PJXX029R'
124000170726     C                   Parm                    IFSObjN         256
124100170726     C                   Parm                    IFSObjT          10
124200170726     C                   Parm                    IFSErr            1
124300170726     C*
124400170726     C                   if        IfsErr = *on
124500170726     C                   seton                                        7199
124600170726     C                   endif
124700170726     C*
124800170726     C                   ENDSR
124900000000     C************************************************************
125000000000     C* ESECUZIONE LANCIO
125100000000     C************************************************************
125200170505     C     ExeLan        BEGSR
125300170508     C*
125400170508     C* aggiorno valori assunti
125500170508     C                   EXSR      AGGMPE
125600130809     C*
125700170711     C                   Reset                   SDGSI61DS
125800130809     C*
125900170711     C                   Move      XscSoc        SocietaI61
126000170711     C                   Eval      AnnoI61     = D1Anno
126100170711     C                   Eval      TrimI61     = D1Trim
126200170713     C                   Eval      CfI61       = D1Cf
126300170711     C                   Eval      IdFileI61   = D1IdFile
126400170711     C                   Eval      PosizI61    = D1Posiz
126500170711     C                   Eval      TipInvI61   = D1TipInv
126600170711     C                   Eval      CartellI61  = D1Cartella
126700170721     C                   Eval      MaxRecI61   = D1MaxRec
126800110421     C*
126900170711     C                   MoveL     SDGSI61DS     KPJBU
127000000000     C*
127100170711     C                   CALL      'SDGSI61R'
127200000000     C                   PARM                    KPJBA
127300000000     C*
127400170711     C                   MoveL     KPJBU         SDGSI61DS
127500000000     C*
127600000000     C* ritorno da PGM gestione
127700170711     C                   EXSR      RTNI61
127800000000     C*
127900170505     C                   ENDSR
128000170508     C/EJECT
128100000000     C************************************************************
128200000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
128300000000     C************************************************************
128400170711     C     RTNI61        BEGSR
128500000000     C*
128600000000     C* funzione non eseguibile per errore :
128700000000     C*
128800000000     C                   Select
128900000000     C* 0 = funzione eseguita correttamente
129000170711     C     ERRI61        WhenEq    '0'
129100000000     C                   Eval      $Fine       = *On
129200000000     C*
129300000000     C*
129400000000     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
129500170711     C     ERRI61        WhenEq    '1'
129600170711     C     ERRI61        OREQ      '2'
129700000000     C                   Seton                                        9399      emiss.SFLMSGID
129800000000     C*
129900000000     C                   EndSl
130000000000     C*
130100170505     C                   ENDSR
130200170508     C/EJECT
130300000000     C************************************************************
130400000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
130500000000     C************************************************************
130600170505     C     CTRKD1        BEGSR
130700000000     C*
130800000000     C                   SETOFF                                       99
130900000000     C                   MoveA     $FC1          $FC
131000170505     C                   EXSR      CTRKEY
131100000000     C     *IN99         IfEq      *ON
131200000000     C                   Seton                                        97
131300000000     C                   EndIf
131400000000     C*
131500170505     C                   ENDSR
131600170508     C/EJECT
131700000000     C************************************************************
131800000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
131900000000     C************************************************************
132000170505     C     CTRKD2        BEGSR
132100000000     C*
132200000000     C                   SETOFF                                       99
132300000000     C                   MoveA     $FC2          $FC
132400170505     C                   EXSR      CTRKEY
132500000000     C     *IN99         IfEq      *ON
132600000000     C                   Seton                                        97
132700000000     C                   EndIf
132800000000     C*
132900170505     C                   ENDSR
133000170508     C/EJECT
133100000000     C************************************************************
133200000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
133300000000     C* INPUT
133400000000     C* $FC3 = Abilitazioni videata D3
133500000000     C* OUTPUT
133600000000     C* *IN97 *IN99 Errori
133700000000     C************************************************************
133800170505     C     CTRKD3        BEGSR
133900000000     C*
134000000000     C                   SETOFF                                       99
134100000000     C                   MoveA     $FC3          $FC
134200170505     C                   EXSR      CTRKEY
134300000000     C     *IN99         IfEq      *ON
134400000000     C                   Seton                                        97
134500000000     C                   EndIf
134600000000     C*
134700170505     C                   ENDSR
134800170508     C/EJECT
134900000000     C************************************************************
135000000000     C* CONTROLLO ABILITAZIONE TASTI
135100000000     C************************************************************
135200170505     C     CTRKEY        BEGSR
135300000000     C*
135400000000     C                   Select
135500000000     C     $Tasto        WhenEq    F01
135600000000     C     $FC(1)        IfEq      '0'
135700000000     C                   Seton                                        99
135800000000     C                   EndIf
135900000000     C     $Tasto        WhenEq    F02
136000000000     C     $FC(2)        IfEq      '0'
136100000000     C                   Seton                                        99
136200000000     C                   EndIf
136300000000     C     $Tasto        WhenEq    F03
136400000000     C     $FC(3)        IfEq      '0'
136500000000     C                   Seton                                        99
136600000000     C                   EndIf
136700000000     C     $Tasto        WhenEq    F04
136800000000     C     $FC(4)        IfEq      '0'
136900000000     C                   Seton                                        99
137000000000     C                   EndIf
137100000000     C     $Tasto        WhenEq    F05
137200000000     C     $FC(5)        IfEq      '0'
137300000000     C                   Seton                                        99
137400000000     C                   EndIf
137500000000     C     $Tasto        WhenEq    F06
137600000000     C     $FC(6)        IfEq      '0'
137700000000     C                   Seton                                        99
137800000000     C                   EndIf
137900000000     C     $Tasto        WhenEq    F07
138000000000     C     $FC(7)        IfEq      '0'
138100000000     C                   Seton                                        99
138200000000     C                   EndIf
138300000000     C     $Tasto        WhenEq    F08
138400000000     C     $FC(8)        IfEq      '0'
138500000000     C                   Seton                                        99
138600000000     C                   EndIf
138700000000     C     $Tasto        WhenEq    F09
138800000000     C     $FC(09)       IfEq      '0'
138900000000     C                   Seton                                        99
139000000000     C                   EndIf
139100000000     C     $Tasto        WhenEq    F10
139200000000     C     $FC(10)       IfEq      '0'
139300000000     C                   Seton                                        99
139400000000     C                   EndIf
139500000000     C     $Tasto        WhenEq    F11
139600000000     C     $FC(11)       IfEq      '0'
139700000000     C                   Seton                                        99
139800000000     C                   EndIf
139900000000     C     $Tasto        WhenEq    F12
140000000000     C     $FC(12)       IfEq      '0'
140100000000     C                   Seton                                        99
140200000000     C                   EndIf
140300000000     C     $Tasto        WhenEq    F13
140400000000     C     $FC(13)       IfEq      '0'
140500000000     C                   Seton                                        99
140600000000     C                   EndIf
140700000000     C     $Tasto        WhenEq    F14
140800000000     C     $FC(14)       IfEq      '0'
140900000000     C                   Seton                                        99
141000000000     C                   EndIf
141100000000     C     $Tasto        WhenEq    F15
141200000000     C     $FC(15)       IfEq      '0'
141300000000     C                   Seton                                        99
141400000000     C                   EndIf
141500000000     C     $Tasto        WhenEq    F16
141600000000     C     $FC(16)       IfEq      '0'
141700000000     C                   Seton                                        99
141800000000     C                   EndIf
141900000000     C     $Tasto        WhenEq    F17
142000000000     C     $FC(17)       IfEq      '0'
142100000000     C                   Seton                                        99
142200000000     C                   EndIf
142300000000     C     $Tasto        WhenEq    F18
142400000000     C     $FC(18)       IfEq      '0'
142500000000     C                   Seton                                        99
142600000000     C                   EndIf
142700000000     C     $Tasto        WhenEq    F19
142800000000     C     $FC(19)       IfEq      '0'
142900000000     C                   Seton                                        99
143000000000     C                   EndIf
143100000000     C     $Tasto        WhenEq    F20
143200000000     C     $FC(20)       IfEq      '0'
143300000000     C                   Seton                                        99
143400000000     C                   EndIf
143500000000     C     $Tasto        WhenEq    F21
143600000000     C     $FC(21)       IfEq      '0'
143700000000     C                   Seton                                        99
143800000000     C                   EndIf
143900000000     C     $Tasto        WhenEq    F22
144000000000     C     $FC(22)       IfEq      '0'
144100000000     C                   Seton                                        99
144200000000     C                   EndIf
144300000000     C     $Tasto        WhenEq    F23
144400000000     C     $FC(23)       IfEq      '0'
144500000000     C                   Seton                                        99
144600000000     C                   EndIf
144700000000     C     $Tasto        WhenEq    F24
144800000000     C     $FC(24)       IfEq      '0'
144900000000     C                   Seton                                        99
145000000000     C                   EndIf
145100000000     C                   EndSl
145200000000     C*
145300170505     C                   ENDSR
145400170508     C/EJECT
145500000000     C************************************************************
145600000000     C* OPERAZIONI INIZIALI
145700000000     C************************************************************
145800170505     C     *INZSR        BEGSR
145900000000     C*
146000000000     C* Reperimento parametri
146100000000     C*
146200000000     C     *ENTRY        PLIST
146300000000     C                   PARM                    KPJBA
146400150210     C*
146500150210     C* reperisco DATFMT del job
146600150210     C                   CallB     'XDATFMT'
146700150210     C                   Parm                    $DatFmt           4
146800000000     C*
146900000000     C* Reperimento tasti di funzione
147000000000     C*
147100170505     C                   EXSR      REPKEY
147200000000     C*
147300170505     C                   ENDSR
147400170508     C/EJECT
147500000000     C************************************************************
147600000000     C* REPERIMENTO TASTI DI FUNZIONE
147700000000     C************************************************************
147800170505     C     REPKEY        BEGSR
147900000000     C*
148000000000     C                   MoveA     $FM1          $FM
148100000000     C                   MoveA     $Fc1          $Fc
148200170505     C                   EXSR      REPKKK
148300000000     C                   MoveA     $FX           $FX1
148400000000     C*
148500000000     C                   MoveA     $FM2          $FM
148600000000     C                   MoveA     $Fc2          $Fc
148700170505     C                   EXSR      REPKKK
148800000000     C                   MoveA     $FX           $FX2
148900000000     C*
149000000000     C                   MoveA     $FM3          $FM
149100000000     C                   MoveA     $Fc3          $Fc
149200170505     C                   EXSR      REPKKK
149300000000     C                   MoveA     $FX           $FX3
149400000000     C*
149500170505     C                   ENDSR
149600170508     C/EJECT
149700000000     C************************************************************
149800000000     C* REPERIMENTO TASTI DI FUNZIONE
149900000000     C************************************************************
150000170505     C     REPKKK        BEGSR
150100000000     C*
150200000000     C                   Clear                   $FX
150300000000     C     1             DO        24            X
150400000000     C*
150500000000     C     $FM(X)        IFNE      *BLANK
150600000000     C     $fc(X)        andne     '0'
150700000000     C*
150800000000     C                   MoveL     $FM(X)        IDMSG
150900000000     C                   CALLB     'XRTVM'
151000000000     C                   PARM                    IDMSG            27
151100000000     C                   PARM                    TXTMSG          100
151200000000     C                   PARM                    ERRMSG            1
151300000000     C*
151400000000     C     ERRMSG        IFNE      *ON
151500000000     C                   MoveL     TXTMSG        $FX(X)
151600000000     C                   ElSe
151700000000     C                   MoveL     *ALL' '       $FX(X)
151800000000     C                   EndIf
151900000000     C*
152000000000     C                   EndIf
152100000000     C*
152200000000     C                   ENDDO
152300000000     C*
152400170505     C                   ENDSR
152500170508     C/EJECT
152600000000     C************************************************************
152700000000     C* INIZIALIZZAZIONE VARIABILI
152800000000     C************************************************************
152900170505     C     INZVAR        BEGSR
153000000000     C*
153100170711     C                   MoveL     KPJBU         SDGSI60DS
153200150204     C*
153300000000     C* se il pgm e stato richiamato imposto i valori ricevuti dalla DS
153400170711     C                   If        SocietaI60 <> *Blanks
153500000000     C                   Eval      CallPgm     = '1'
153600000000     C                   ElSe
153700000000     C                   Eval      CallPgm     = '0'
153800000000     C                   EndIf
153900000000     C*
154000000000     C* Reperimento dati societa
154100000000     C*
154200000000     C                   MoveL     'SOC001'      TIPXSC
154300170711     C                   MoveL     SocietaI60    SOCXSC
154400170505     C                   EXSR      REPSOC
154500000000     C     RTNXSC        IFNE      '1'
154600000000     C                   MoveL     XSOCDS        SOC001
154700000000     C                   ElSe
154800000000     C                   Return
154900000000     C                   EndIf
155000170505     C*
155100170505     C* DATI IDENTIFICATIVI DEL CONTRIBUENTE
155200170505     C*
155300170505     C*
155400000000     C*
155500000000     C* Reperimento valori default utente
155600000000     C*
155700170505     C                   EXSR      REPDFT
155800000000     C*
155900000000     C* Definizione dei tasti funzione utilizzabili a pgm
156000000000     C*
156100170505     C                   EXSR      DEFOPZ
156200000000     C*
156300000000     C* Variabili per gestione videate
156400000000     C*
156500000000     C                   Move      'D1'          $GEST
156600000000     C                   Move      *OFF          $FINE
156700000000     C                   Move      *ON           $InzD1
156800000000     C                   Move      *BLANK        $LastG
156900000000     C                   Move      *BLANK        $LastV
157000000000     C                   Z-Add     0             $ULKD1            3 0
157100000000     C                   Z-Add     0             $ULKD2            3 0
157200000000     C                   Z-Add     0             $ULKD3            3 0
157300000000     C*
157400000000     C* Variabili appoggio
157500000000     C*
157600000000     C                   Z-Add     0             CURR
157700000000     C                   Z-Add     0             CURC
157800000000     C*
157900000000     C* Valorizzazione campi univoci testate
158000000000     C*
158100000000     C                   Clear                   FMTT1
158200000000     C                   MoveL     KNSIf         NOMSIf
158300000000     C                   MoveL     XSCDSI        NOMDIT
158400000000     C                   MOVEL     KCDAZ         NOMAZN
158500000000     C                   MOVEL     DSPGM         NOMPGM           10
158600000000     C* recupero il sottotitolo (se c'e)
158700000000     C                   MoveL     *BLANK        D1TIT
158800000000     C     SOTTITD1      IFNE      *BLANK
158900000000     C                   MoveL     SOTTITD1      IDMSG
159000000000     C                   CALLB     'XRTVM'
159100000000     C                   PARM                    IDMSG            27
159200000000     C                   PARM                    TXTMSG          100
159300000000     C                   PARM                    ERRMSG            1
159400000000     C     ERRMSG        IFNE      *ON
159500000000     C                   MoveL     TXTMSG        D1TIT
159600000000     C                   ElSe
159700000000     C                   MoveL     *ALL'?'       D1TIT
159800000000     C                   EndIf
159900000000     C                   EndIf
160000000000     C*
160100000000     C                   MoveL     *BLANK        T1TIT
160200170515     C                   Eval      LENMSG      = %Len(T1TIT)
160300000000     C     SOTTITD2      IFNE      *BLANK
160400000000     C                   MoveL     SOTTITD2      IDMSG
160500000000     C                   CALLB     'XRTVM'
160600000000     C                   PARM                    IDMSG            27
160700000000     C                   PARM                    TXTMSG          100
160800000000     C                   PARM                    ERRMSG            1
160900000000     C                   PARM      *On           CTRMSG            1            centratura
161000170515     C                   PARM                    LENMSG            3 0          lun output.
161100000000     C     ERRMSG        IFNE      *ON
161200000000     C                   MoveL     TXTMSG        T1TIT
161300000000     C                   ElSe
161400000000     C                   MoveL     *ALL'?'       T1TIT
161500000000     C                   EndIf
161600000000     C                   EndIf
161700000000     C*
161800000000     C* Inizializzazione/Decodifica prima videata
161900000000     C*
162000170505     C                   EXSR      InzD1
162100000000     C                   Move      *OFF          $InzD1
162200170512     C                   EXSR      InzD4
162300170512     C                   Move      *OFF          $InzD4
162400000000     C*
162500170505     C                   ENDSR
162600170508     C/EJECT
162700000000     C************************************************************
162800000000     C* REPERIMENTO DATI SOCIETA'
162900000000     C************************************************************
163000170505     C     REPSOC        BEGSR
163100000000     C*
163200000000     C                   CALL      'SDGSOCDR'
163300000000     C                   PARM                    TIPXSC            6
163400000000     C                   PARM                    SOCXSC            3
163500000000     C                   PARM                    CDSXSC            9 0
163600000000     C                   PARM                    MODXSC            3
163700000000     C                   PARM      *BLANKS       RTNXSC            1
163800000000     C                   PARM                    XSOCDS
163900000000     C                   PARM                    KPJBA
164000000000     C*
164100170505     C                   ENDSR
164200170508     C/EJECT
164300170725     C************************************************************
164400170725     C* Rerimento parametri di setup per Spesometro Trimestrale
164500170725     C************************************************************
164600170725     C     RepSDSPETRI   BEGSR
164700170725     C*
164800170725     C                   Clear                   SDGPA12DS
164900170725     C*
165000170725     C                   CALLB     'XPAR'
165100170725     C                   Parm                    XScSoc
165200170725     C                   Parm      'SDSPETRI'    XPaPar            8
165300170725     C                   Parm                    XPaOut         2000
165400170725     C                   Parm      *OFF          XPaErr            1
165500170725     C                   Parm      ' '           XPaRic            1
165600170725     C*
165700170725     C                   If        XPaErr      = *On
165800170725     C*
165900170725     C* Errore: fallito reperimento recupero parametro modulo &1
166000170725     C* Valorizzo le variabili del msg PRO2184
166100170725     C                   MoveL     XPaPar        P1Param
166200170725     C                   Seton                                        6399
166300170725     C*
166400170725     C                   ElSe
166500170725     C                   MoveL     XPaOut        SDGPA12DS
166600170725     C                   EndIf
166700170725     C*
166800170725     C                   EndSr
166900000000     C************************************************************
167000000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
167100000000     C************************************************************
167200170505     C     REPDFT        BEGSR
167300000000     C*
167400000000     C* preparo i parametri di chiamata al driver
167500000000     C                   Clear                   XMPEDS
167600000000     C                   EVAL      TpChiamata = '01'
167700000000     C                   EVAL      NChiavi = 5
167800000000     C                   EVAL      VLogica = 2
167900000000     C*
168000000000     C* preparo le chiavi per la chiamata al driver
168100000000     C                   Clear                   XMPEDS1
168200000000     C                   MoveL     XSCSOC        DSMPESOC
168300000000     C                   MoveL     DSPGM         DSMPEPGM
168400000000     C                   MoveL     KNMUS         DSMPEPRF
168500000000     C* Si reperiscono i dati di default a seconda delle modalita con
168600000000     C* cui il pgm e usato
168700000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
168800000000     C* MPETIP = 'PV'  pgm richiamato da menu
168900000000     C* MPETIP = 'PB'  x lancio Batch
169000000000     C                   MoveL     'PB'          DSMPETIP
169100000000     C*
169200000000     C                   CALLB     'XMPE'
169300000000     C                   PARM                    XMPEDS
169400000000     C                   PARM                    XMPEDS1
169500000000     C*
169600000000     C     Risultato     IfEq      '0'
169700000000     C                   MoveL     DSMPEPAR      DSMPE
169800000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
169900000000     C* i suoi valori di default vengono decisi a pgm
170000000000     C*
170100000000     C                   ElSe
170200000000     C                   EndIf
170300000000     C*
170400170505     C                   ENDSR
170500170508     C/EJECT
170600000000     C************************************************************
170700000000     C* Aggiornamento valori assunti default
170800000000     C************************************************************
170900170505     C     Aggmpe        BEGSR
171000000000     C*
171100000000     C* preparo i parametri di chiamata al driver
171200000000     C                   Clear                   XMPEDS
171300000000     C                   EVAL      TpChiamata = '03'
171400000000     C                   EVAL      NChiavi = 5
171500000000     C                   EVAL      VLogica = 2
171600000000     C*
171700000000     C* preparo le chiavi per la chiamata al driver
171800000000     C                   Clear                   XMPEDS1
171900000000     C                   MoveL     XscSoc        DSMPESOC
172000000000     C                   MoveL     DSPGM         DSMPEPGM
172100000000     C                   MoveL     KNMUS         DSMPEPRF
172200000000     C*
172300000000     C* Si memorizzano i dati di default a seconda delle modalita con
172400000000     C* cui il pgm e usato
172500000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
172600000000     C* MPETIP = 'PV'  pgm richiamato da menu
172700000000     C* MPETIP = 'PB'  x lancio Batch
172800130809     C*
172900130809     C                   MoveL     'PB'          DSMPETIP
173000130809     C* Dati in recupero
173100130809     C                   Move      D1Anno        $Anno
173200170505     C                   Move      D1Trim        $Trim
173300170721     C                   Z-Add     D1MaxRec      $MaxRec
173400130809     C*
173500130809     C                   MoveL     DSMPE         DSMPEPAR
173600130809     C*
173700130809     C                   CALLB     'XMPE'
173800130809     C                   PARM                    XMPEDS
173900000000     C                   PARM                    XMPEDS1
174000000000     C*
174100170505     C                   ENDSR
174200170508     C/EJECT
174300170512     C************************************************************
174400170512     C* Aggiornamento D4
174500170512     C************************************************************
174600170512     C     AggD4         BEGSR
174700170512     C*
174800170512     C* preparo i parametri di chiamata al driver
174900170512     C                   Clear                   XMPEDS
175000170512     C                   EVAL      TpChiamata = '03'
175100170512     C                   EVAL      NChiavi = 5
175200170512     C                   EVAL      VLogica = 2
175300170512     C*
175400170512     C* preparo le chiavi per la chiamata al driver
175500170512     C                   Clear                   XMPEDS1
175600170512     C                   MoveL     XscSoc        DSMPESOC
175700170711     C                   MoveL     'SDGSI60R  '  DSMPEPGM
175800170515     C                   Move      'I'           DSMPEPGM
175900170512     C                   MoveL     *Blanks       DSMPEPRF
176000170512     C*
176100170512     C* Si memorizzano i dati di default a seconda delle modalita con
176200170512     C* cui il pgm e usato
176300170512     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
176400170512     C* MPETIP = 'PV'  pgm richiamato da menu
176500170512     C* MPETIP = 'PB'  x lancio Batch
176600170512     C*
176700170512     C                   MoveL     'PB'          DSMPETIP
176800170512     C*
176900170515     C                   Clear                   DsIFS
177000170512     C*
177100170512     C                   MoveL(P)  D4Cartella    $Cartella
177200170512     C*
177300170515     C                   MoveL     DsIFS         DSMPEPAR
177400170512     C*
177500170512     C                   CALLB     'XMPE'
177600170512     C                   PARM                    XMPEDS
177700170512     C                   PARM                    XMPEDS1
177800170512     C*
177900170512     C                   ENDSR
178000170512     C/EJECT
178100000000     C************************************************************
178200000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
178300000000     C************************************************************
178400170505     C     DEFOPZ        BEGSR
178500000000     C*
178600000000     C                   Reset                   $FC1
178700000000     C                   Reset                   $FR1
178800000000     C                   Reset                   $FC3
178900000000     C                   Reset                   $FR3
179000000000     C*
179100170505     C                   ENDSR
179200170508     C/EJECT
179300000000     C************************************************************
179400000000     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
179500000000     C************************************************************
179600170505     C     DEFCAM        BEGSR
179700000000     C*
179800000000     C* klist
179900170801     C*
180000170807     C* SDGCSF04L
180100170807     C     K02Csf04      KLIST
180200170807     C                   KFLD                    CsfSocieta
180300170807     C                   KFLD                    CsfIdFile
180400000000     C*
180500000000     C* Variabili per gestione videate
180600000000     C*
180700000000     C                   Move      *BLANK        $GEST             2
180800000000     C                   Move      *BLANK        $FINE             1
180900000000     C                   Move      *BLANK        $InzD1            1
181000000000     C                   Move      *BLANK        $InzD2            1
181100000000     C                   Move      *BLANK        $INZD3            1
181200170512     C                   Move      *BLANK        $INZD4            1
181300000000     C                   Move      *BLANK        $LastG            2
181400000000     C                   Move      *BLANK        $LastV            2
181500000000     C                   Z-Add     0             $PIED1            3 0
181600000000     C*
181700000000     C* Variabili appoggio
181800000000     C*
181900000000     C                   Z-Add     0             CURR              2 0
182000000000     C                   Z-Add     0             CURC              2 0
182100000000     C                   Move      *ZEROS        WIN              99
182200000000     C*
182300000000     C* Indici
182400000000     C                   Z-Add     0             X                 3 0
182500000000     C                   Z-Add     0             Y                 3 0
182600000000     C*
182700170505     C                   ENDSR
182800170508     C/EJECT
182900000000     C************************************************************
183000000000     C* RIEMPIMENTO TASTI FUNZIONE
183100000000     C************************************************************
183200170505     C     RIEKEY        BEGSR
183300000000     C*
183400000000     C* Input :
183500000000     C* - $FX = schiera con tutti i testi disponibili
183600000000     C* - $FC = schiere con i flag di validita associati ai testi
183700000000     C* - $FR = schiere con i flag di REVERSE IMAGE
183800000000     C* - $ULK = ultimo testo caricato
183900000000     C* - $L01 = Lunghezza prima riga
184000000000     C* - $L02 = Lunghezza seconda riga
184100000000     C* Output :
184200000000     C* - $ULK
184300000000     C* - $KEY1= 1ø riga di tasti funzione
184400000000     C* - $KEY2= 2ø riga di tasti funzione
184500000000     C*
184600000000     C                   CALLB     'XKEYMSG'
184700000000     C* input
184800000000     C                   PARM                    $FX
184900000000     C                   PARM                    $FC
185000000000     C                   PARM                    $FR
185100000000     C                   PARM                    $ULK              3 0
185200000000     C* output
185300000000     C                   PARM                    $KEY1            79
185400000000     C                   PARM                    $KEY2            79
185500000000     C* input
185600000000     C                   PARM                    $L01              3 0
185700000000     C                   PARM                    $L02              3 0
185800000000     C*
185900000000     C                   PARM                    $ALLFUNCT       480
186000000000     C*
186100170505     C                   ENDSR
186200170508     C/EJECT
186300000000** TASTI DI FUNZIONE  D1
186400000000PROMSG    *LIBL     KEY0001  01
186500000000PROMSG    *LIBL     KEY0002  02
186600000000PROMSG    *LIBL     KEY0003  03
186700000000PROMSG    *LIBL     KEY0004  04
186800000000PROMSG    *LIBL     KEY0005  05
186900000000PROMSG    *LIBL     KEY0006  06
187000000000PROMSG    *LIBL     KEY0007  07
187100000000PROMSG    *LIBL     KEY0008  08
187200000000PROMSG    *LIBL     KEY0009  09
187300000000PROMSG    *LIBL     KEY0010  10
187400000000PROMSG    *LIBL     KEY0011  11
187500000000PROMSG    *LIBL     KEY0012  12
187600000000PROMSG    *LIBL     KEY0013  13
187700000000PROMSG    *LIBL     KEY0014  14
187800000000PROMSG    *LIBL     KEY0015  15
187900000000PROMSG    *LIBL     KEY0016  16
188000000000PROMSG    *LIBL     KEY0017  17
188100000000PROMSG    *LIBL     KEY0018  18
188200170512PROMSG    *LIBL     KEY0019  19
188300170725SDGMSG    *LIBL     SDK0031  20
188400000000PROMSG    *LIBL     KEY0021  21
188500170512SDGMSG    *LIBL     SDK0027  22
188600000000PROMSG    *LIBL     KEY0023  23
188700000000PROMSG    *LIBL     KEY0024  24
188800170512**  ABILITAZIONE D1
1889000000000N  01
1890000000000N  02
1891000000001N  03
1892000000001N  04
1893000000001N  05
1894000000001N  06
1895000000000N  07
1896000000000N  08
1897000000000N  09
1898000000000N  10
1899000000000N  11
1900000000001N  12
1901000000000N  13
1902000000000N  14
1903000000000N  15
1904000000000N  16
1905000000000N  17
1906000000001N  18
1907001705050N  19
1908001705081S  20
1909000000000N  21
1910001705121S  22
1911000000000N  23   Impostare '1' se si usano le opzioni
1912000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
191300170512** TASTI DI FUNZIONE  D2 / D4
191400000000PROMSG    *LIBL     KEY0001  01
191500000000PROMSG    *LIBL     KEY0002  02
191600000000PROMSG    *LIBL     KEY0003  03
191700000000PROMSG    *LIBL     KEY0004  04
191800000000PROMSG    *LIBL     KEY0005  05
191900000000PROMSG    *LIBL     KEY0006  06
192000000000PROMSG    *LIBL     KEY0007  07
192100000000PROMSG    *LIBL     KEY0008  08
192200000000PROMSG    *LIBL     KEY0009  09
192300000000PROMSG    *LIBL     KEY0010  10
192400000000PROMSG    *LIBL     KEY0011  11
192500000000PROMSG    *LIBL     KEY0012  12
192600000000PROMSG    *LIBL     KEY0013  13
192700000000PROMSG    *LIBL     KEY0014  14
192800000000PROMSG    *LIBL     KEY0015  15
192900000000PROMSG    *LIBL     KEY0016  16
193000000000PROMSG    *LIBL     KEY0017  17
193100000000PROMSG    *LIBL     KEY0018  18
193200000000PROMSG    *LIBL     KEY0019  19
193300000000PROMSG    *LIBL     KEY0020  20
193400000000PROMSG    *LIBL     KEY0021  21
193500000000PROMSG    *LIBL     KEY0022  22
193600000000PROMSG    *LIBL     KEY0023  23
193700000000PROMSG    *LIBL     KEY0024  24
193800170512**  ABILITAZIONE D2 / D4
1939000000000N  01
1940000000000N  02
1941000000001N  03
1942001108111N  04
1943000000001N  05
1944000000001N  06
1945000000000N  07
1946000000000N  08
1947000000000N  09
1948000000000N  10
1949000000000N  11
1950000000001N  12
1951000000000N  13
1952000000000N  14
1953000000000N  15
1954000000000N  16
1955000000000N  17
1956000000000N  18
1957000000000N  19
1958000000000N  20
1959000000000N  21
1960000000000N  22
1961000000000N  23   Impostare '1' se si usano le opzioni
1962000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
196300000000** TASTI DI FUNZIONE  D3
196400000000PROMSG    *LIBL     KEY0001  01
196500000000PROMSG    *LIBL     KEY0002  02
196600000000PROMSG    *LIBL     KEY0003  03
196700000000PROMSG    *LIBL     KEY0004  04
196800000000PROMSG    *LIBL     KEY0005  05
196900000000PROMSG    *LIBL     KEY0006  06
197000000000PROMSG    *LIBL     KEY0007  07
197100000000PROMSG    *LIBL     KEY0008  08
197200000000PROMSG    *LIBL     KEY0009  09
197300000000PROMSG    *LIBL     KEY0010  10
197400000000PROMSG    *LIBL     KEY0011  11
197500000000PROMSG    *LIBL     KEY0012  12
197600000000PROMSG    *LIBL     KEY0013  13
197700000000PROMSG    *LIBL     KEY0014  14
197800000000PROMSG    *LIBL     KEY0015  15
197900000000PROMSG    *LIBL     KEY0016  16
198000000000PROMSG    *LIBL     KEY0017  17
198100000000PROMSG    *LIBL     KEY0018  18
198200000000PROMSG    *LIBL     KEY0019  19
198300000000PROMSG    *LIBL     KEY0020  20
198400000000PROMSG    *LIBL     KEY0021  21
198500000000PROMSG    *LIBL     KEY0022  22
198600000000PROMSG    *LIBL     KEY0023  23
198700000000PROMSG    *LIBL     KEY0024  24
198800000000**  ABILITAZIONE D3
1989000000000N  01
1990000000000N  02
1991000000001N  03
1992000000000N  04
1993000000000N  05
1994000000000N  06
1995000000000N  07
1996000000000N  08
1997000000000N  09
1998000000000N  10
1999000000000N  11
2000000000001N  12
2001000000000N  13
2002000000000N  14
2003000000000N  15
2004000000000N  16
2005000000000N  17
2006000000000N  18
2007000000000N  19
2008000000000N  20
2009000000000N  21
2010000000000N  22
2011000000000N  23   Impostare '1' se si usano le opzioni
2012000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
