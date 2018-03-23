000100000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(QILE)
000200000000     H*PARMS BNDDIR(PJXBND PJCBND) CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500000000      *****************************************************************
000600000000      *
000700000000      *         ------------------------------------------------
000800130809      *                Comunicazione Operazioni Iva  2013
000900130809      *                 Lancio creazione file telematico
001000000000      *         ------------------------------------------------
001100000000      *
001200000000      *****************************************************************
001300000000     F*----------------------------------------------------*
001400130809     FSDGSG60D  CF   E             WORKSTN
001500130809     F                                     RENAME(SG60T1:FMTT1)
001600130809     F                                     RENAME(SG60D1:FMTD1)
001700130809     F                                     RENAME(SG60Z1:FMTZ1)
001800130809     F                                     RENAME(SG60D2:FMTD2)
001900130809     F                                     RENAME(SG60D3:FMTD3)
002000130809     F                                     RENAME(SG60Z3:FMTZ3)
002001160609R224 F                                     RENAME(SK60D4:FMTD4)
002100130809     F                                     RENAME(SG60Z2:FMTZ2)
002200000000     F                                     INFDS(DSFMT)
002300000000     F*------------
002400000000     D*----------------------------------------------------*
002500000000     D* Schiere per la gestione dei tasti di funzione
002600000000     D $FM             S             27    DIM(24)
002700000000     D $FX             S             50    DIM(24)
002800000000     D $FC             S              1    DIM(24)
002900000000     D $FR             S              1    DIM(24)
003000000000     D* Video D1/D2
003100000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003200000000     D $FX1            S             50    DIM(24) ALT($FM1)
003300000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003400000000     D $FR1            S              1    DIM(24) ALT($FC1)
003500000000     D* Video D1/D2
003600000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
003700000000     D $FX2            S             50    DIM(24) ALT($FM2)
003800000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
003900000000     D $FR2            S              1    DIM(24) ALT($FC2)
004000000000     D* Video D3 Valori assunti
004100000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
004200000000     D $FX3            S             50    DIM(24) ALT($FM3)
004300000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
004400000000     D $FR3            S              1    DIM(24) ALT($FC3)
004500000000     D*----------------------------------------------------*
004600000000     D*-------------
004700000000     D* Passaggio Parametri
004800000000     D KPJBA         E DS
004900000000     D*-------------
005000000000     D* DS Esterna gestione messaggi
005100000000     D SDGMSGDS      E DS                  INZ
005200000000     D  STS          E                     EXTFLD(MSGSTS)
005300000000     D                                     DIM(333)
005400000000     D  JBA          E                     EXTFLD(MSGJBA)
005500000000     D                                     DIM(502)
005600000000     D*-------------
005700000000     D*Asteriscare se non presenti
005800000000     D* Reperimento Autorizzazioni
005900000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
006000000000     D*-------------
006100000000     D* Dati per pgm XNOAUT
006200000000     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
006300000000     D*-------------
006400000000     D* Passaggio Autorizzazioni
006500000000     D DSAUT           DS           512
006600110421     D*-------------
006700110421     D* Ricerca legende
006800110421     D Ana964DS      E DS
006801160609R224 D*---------
006802160609 "   D* tabella destinazione dati aggiuntivi
006803160609 "   D SDGHC4DS      E DS                  INZ
006804160609 "   D*-------------
006805160609 "   D* Parametri per ricerca codice ANATB00F
006806160609 "   D XTABDS        E DS                  inz
006807160609 "   D*-------------
006808160609 "   D* Parametri per controllo codice ANATB00F
006809160613 "   D SDGTABDS      E DS                  INZ
006810160613 "   D*----------------
006811160613 "   D* tabelle delle nature giuridiche
006812160613R224 D ANGG16DS      E DS                  INZ
006900101118     D*-------------
007000000000     D* Valori assunti
007100000000     D XMPEDS        E DS                  INZ
007200000000     D XMPEDS1       E DS                  INZ
007300000000     D DSMPE           DS                  INZ
007400000000     D  $VAL                          1
007500000000     D  $Anno                         4S 0
007600110421     D  $TipInv                       1S 0
007700110811     D  $SocInc                       1S 0
007800000000     D  $MaxDim                       2S 0
007900110810     D  $MaxRec                       5S 0
008000000000     D  $File                        10A
008100131115R132 D  $Mese                         2A
008200131115 "   D  $Trim                         2A
008300131115R132 D  $StpL                         1A
008400000000     D*-------------
008500110811     D DSInterm        DS                  INZ
008600110811     D $FisInt                             Like(D2FisInt )
008700110811     D $CafInt                        5A
008800110811     D $ImpTrs                        1A
008900110811     D $ImpDta                       10A
009000130913     D $FISSOG                       16A
009100130913     D $CARSOG                        2A
009200130913     D $DTISOG                       10A
009300130913     D $DTFSOG                       10A
009400130913     D $COGSOG                       24A
009500130913     D $NOMSOG                       20A
009600130913     D $SEXSOG                        1A
009700130913     D $DTNSOG                       10A
009800130913     D $COMSOG                       40A
009900130913     D $PRVSOG                        2A
010000130913     D $RAGSOG                       24A
010100000000     D*-------------
010200110811     D DSD2Interm      DS
010300110811     D  D2FISINT
010400110811     D  D2CAFINT
010500110811     D  D2IMPTRS
010600110811     D  D2IMPDTA
010700130913     D  D2FISSOG
010800130913     D  D2CARSOG
010900130913     D  D2DTISOG
011000130913     D  D2DTFSOG
011100130913     D  D2COGSOG
011200130913     D  D2NOMSOG
011300130913     D  D2SEXSOG
011400130913     D  D2DTNSOG
011500130913     D  D2COMSOG
011600130913     D  D2PRVSOG
011700130913     D  D2RAGSOG
011701160609R224 D*-------------
011702160609 "   D DSContrib       DS                  INZ
011703160609R224 D $DestCon                            Like(D4DestCon)
011800110811     D*-------------
011900000000     D* Reperimento nome PGM
012000000000     D STATUS         SDS           333
012100000000     D  DSPGM            *PROC
012200000000     D  PARMS            *PARMS
012300000000     D*-------------
012400000000     D* Dati di ambiente ottenuti da SDGSOCDR
012500000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
012600000000     D*-------------
012700000000     D* DS Interna per dati di output di SDGSOCDR
012800000000     D XSOCDS          DS          1000
012900000000     D*-------------
013000000000     D* Ds per ricezione parametri
013100130809     D SDGSG60DS     E DS
013200130809     D  ERRG60       E                     INZ('0')
013300000000     D*-------------
013400000000     D* Passaggio Parametri al PGM di estrazione dati
013500130809     D SDGSG61DS     E DS
013600130809     D  ERRG61       E                     INZ('0')
013700131114R132 D*-------------
013800131114 "   D* Passaggio Parametri al PGM di estrazione dati
013900131114 "   D* per Black List
014000131114 "   D SDGSG63DS     E DS
014100131114R132 D  ERRG63       E                     INZ('0')
014200140328R133 D*-------------
014300140328 "   D* Passaggio Parametri al PGM di estrazione dati
014400140328 "   D* per Acquisti di San Marino
014500140328 "   D SDGSG64DS     E DS
014600140328R133 D  ERRG64       E                     INZ('0')
014700000000     D*-------------
014800000000     D* Parametri modulo SCARSUMA
014900000000     D ANP022DS      E DS
015000160202R195 X****-------------
015100160202 "   X**** Catalogo file per scarico su supporto magnetico
015200160202R195 X**** ANA122DS      E DS
015300000000     D*-------------
015400000000     D X07VALDS      E DS
015500000000     D*-------------
015600000000     D DSFMT           DS
015700000000     D  $Tasto               369    369
015800000000     D  NRG                  370    370
015900000000     D  NCL                  371    371
016000000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
016100000000     D*-------------
016200000000     D* posizione cursore
016300000000     D CURSOR          DS
016400000000     D  RIRI                   1      2B 0 INZ
016500000000     D  R$$                    2      2
016600000000     D  COCO                   3      4B 0 INZ
016700000000     D  C$$                    4      4
016800000000     D*-------------
016900000000     D* Tasti di funzione
017000000000     D F01             C                   CONST(X'31')
017100000000     D F02             C                   CONST(X'32')
017200000000     D F03             C                   CONST(X'33')
017300000000     D F04             C                   CONST(X'34')
017400000000     D F05             C                   CONST(X'35')
017500000000     D F06             C                   CONST(X'36')
017600000000     D F07             C                   CONST(X'37')
017700000000     D F08             C                   CONST(X'38')
017800000000     D F09             C                   CONST(X'39')
017900000000     D F10             C                   CONST(X'3A')
018000000000     D F11             C                   CONST(X'3B')
018100000000     D F12             C                   CONST(X'3C')
018200000000     D F13             C                   CONST(X'B1')
018300000000     D F14             C                   CONST(X'B2')
018400000000     D F15             C                   CONST(X'B3')
018500000000     D F16             C                   CONST(X'B4')
018600000000     D F17             C                   CONST(X'B5')
018700000000     D F18             C                   CONST(X'B6')
018800000000     D F19             C                   CONST(X'B7')
018900000000     D F20             C                   CONST(X'B8')
019000000000     D F21             C                   CONST(X'B9')
019100000000     D F22             C                   CONST(X'BA')
019200000000     D F23             C                   CONST(X'BB')
019300000000     D F24             C                   CONST(X'BC')
019400000000     D ENTER           C                   CONST(X'F1')
019500000000     D ROLDWN          C                   CONST(X'F4')
019600000000     D ROLLUP          C                   CONST(X'F5')
019700000000     I*-------------
019800000000     I* Sottotitolo di testata
019900000000     I* se non si hanno sottotitoli, mettere tutta la costante = ' '
020000000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
020100000000     D                                          SDC0056')
020200000000     D SOTTITD2        C                   CONST('SDGMSG    *LIBL-
020300000000     D                                          SDC0054')
020400131114R132 D*-------------
020500160204R132 D* Sottotitolo di testata
020801160204R195 X*ex R132 "Elenchi"
020802160204  "  X*ex R132 SOTTITT1        C                   CONST('SDGMSG    *LIBL-
020803160204R195 X*ex R132                                          SDC0074')
020804160204R195 D* "Spesometro"
020805160204  "  D SOTTITT1        C                   CONST('SDGMSG    *LIBL-
020806160204R195 D                                          SDC0087')
020900160204R132 D* "Black List"
021000131114  "  D SOTTITT3        C                   CONST('SDGMSG    *LIBL-
021100131114R132 D                                          SDC0076')
021200140328R133 D* "Acquisti da San Marino"
021300140328  "  D SOTTITT4        C                   CONST('SDGMSG    *LIBL-
021400140328R133 D                                          SDC0075')
021500000000     D*-----------
021600000000     D* Campi di lavoro
021700000000     D CallPgm         S              1
021800000000     D $OkExeLan       S              1
021900000000     D $AnnoAlfa       S              4
022000000000     D DataISO         S               D
022100110811     D WDtImp          S               D
022200130913     D WDtISog         S               D
022300130913     D WDtFSog         S               D
022400130913     D WDtNSog         S               D
022500000000     D $DT0            C                   CONST('0001-01-01')
022600130904     D*
022700130904     D* Il campo D1File è stato tolto dalla gestione video
022800130904     D* lo tengo in vita nel pgm ma ininfluente
022900130904     D D1File          S                   Like(FileG60    )
023000130904     D*----------------
023100130904     D* ds per nome file
023200130904     D                 DS
023300130904     D  WAnno                  3      6  0
023400130904     D  WPer                   7      8
023500000000     D  WPrgInvio              9     10  0
023600000000     D  WNumero                1     10
023700000000     D*-----------
023800000000     D* Ds contenente i valori del msg PRO1797
023900000000     D §Fil_Lib        DS
024000000000     D  §File                        10A
024100000000     D  §Libreria                    10A
024200000000     D*----------------------------------------------------*
024300000000     D $P1COS2161      s             20A
024400130904     D*---------------------------------------------------------------*
024500131011R135 D*** AgeMaxRec       C                    8250
024600131011R135 D AgeMaxRec       C                   39500
024700130904     C*----------------------------------------------------*
024800130904     C*                MAIN LINE PROGRAM
024900130904     C*----------------------------------------------------*
025000130904     C*
025100130904     C* inizializzazione variabili
025200130904     C                   Exsr      INZVAR
025300130904     C*
025400000000     C     $FINE         DOWEQ     *OFF
025500000000     C     $GEST         CASEQ     'D1'          GESD1
025600000000     C     $GEST         CASEQ     'D2'          GESD2
025700000000     C     $GEST         CASEQ     'D3'          GESD3
025701160609R224 C     $GEST         CASEQ     'D4'          GESD4
025800000000     C                   ENDCS
025900000000     C                   ENDDO
026000000000     C*
026100000000     C* fine programma
026200000000     C                   Exsr      EndPgm
026300000000     C************************************************************
026400000000     C* RICERCHE
026500000000     C************************************************************
026600000000     C     EndPgm        BegSr
026700000000     C*
026800000000     C*
026900000000     C* se ho confermato l'uscita proseguo
027000000000     C     $Fine         IfEq      *ON
027100000000     C*
027200000000     C                   Seton                                            LR
027300000000     C                   Return
027400000000     C*
027500000000     C                   EndIf
027600000000     C*
027700000000     C                   EndSr
027800000000     C************************************************************
027900000000     C* RICERCHE
028000000000     C************************************************************
028100000000     C     Search        BegSr
028200000000     C*
028300000000     C* determino Riga/Colonna del cursore
028400000000     C                   Move      NRG           R$$
028500000000     C                   Move      NCL           C$$
028600000000     C                   Z-Add     RIRI          CURR
028700000000     C                   Z-Add     COCO          CURC
028800000000     C*
028900000000     C* I campi che al momento dell'F4 risultano protetti non devono
029000000000     C* essere abilitati alla funzione di ricerca
029100000000     C*
029200000000     C                   Select
029300000000     C*
029400160202R195 X**** FMTD1  - file su supporto magnetico
029500160202 "   X**** H1NMFL        WhenEq    'D1FILE'
029600160202 "   X****               Reset                   ANA122DS
029700160202 "   X****               Move      '01'          Opz122
029800160202 "   X****               Move      XScSoc        Societa122
029900160202 "   X****               Move      XScSoc        Soc122
030000160202 "   X*ex R133           Select
030100160202 "   X*ex R133           When      TpComG60    = 'E'
030200160202 "   X****               Move      'OP'          TpScar122
030300160202 "   X*ex R133           When      TpComG60    = 'S'
030400160202 "   X*ex R133           Move      'SM'          TpScar122
030500160202 "   X*ex R133           EndSl
030600160202 "   X****               Clear                   KPJBU
030700160202 "   X****               Movel     ANA122DS      KPJBU
030800160202 "   X****               Call      'ANA122R'
030900160202 "   X****               Parm                    KPJBA
031000160202 "   X****               Movel     KPJBU         ANA122DS
031100160202 "   X****
031200160202 "   X****               If        File122  <>   *Blanks
031300160202 "   X****               MoveL     File122       D1File
031400160202R195 X****               endIf
031500130809     C*
031600130809     C     H1NMFL        WHENEQ    'D1TIPINV'
031700130809     C                   EXSR      F4_TIPINV
031800130809     C*
031900130809     C     H1NMFL        WHENEQ    'D2IMPTRS'
032000130809     C                   EXSR      F4_ImpTrs
032100131114R132 C*
032200131114  "  C     H1NMFL        WHENEQ    'D1STPL'
032300131114R132 C                   EXSR      F4_STPL
032301160609R224 C*
032302160609 "   C* FMTD4 - Destinazione dati anagrafici aggiuntivi
032303160609 "   C     H1NMFL        WHENEQ    'D4DESTCON'
032304160609R224 C                   EXSR      F4_DestCon
032400130809     C*
032500130809     C* ricerca non abilitata in questa posizione
032600130809     C                   Other
032700130809     C                   Seton                                        9899
032800130809     C*
032900110421     C                   EndSl
033000110421     C*
033100000000     C* imposto pos. del cursore
033200000000     C                   Z-Add     CURR          H1RIGA
033300000000     C                   Z-Add     CURC          H1COLO
033400000000     C*
033500000000     C                   Move      *HIVAL        $LastV
033600000000     C*
033700000000     C                   EndSr
033800110802     C************************************************************
033900110802     C* Ricerca tipo invio
034000110802     C************************************************************
034100110802     C     F4_TipInv     BEGSR
034200110802     C*
034300110802     C                   Reset                   ANA964DS
034400110802     C*
034500110802     C                   MoveL     '01'          Opz964
034600130809     C                   MoveL     'SDGSG60S'    Trc964
034700130809     C                   MoveL     'TIPINVG60'   Cam964
034800110802     C                   MoveL     *On           All964
034900110802     C*
035000110802     C                   MoveL     ANA964DS      KPJBU
035100130809     C                   CALL      'ANA964R'
035200130809     C                   PARM                    KPJBA
035300130809     C                   MoveL     KPJBU         ANA964DS
035400130809     C*
035500130809     C                   if        Ret964     <> '2'
035600130809     C                   MoveL     Val964        D1TIPINV
035700130809     C                   endif
035800130809     C*
035900130809     C                   ENDSR
036000110811     C************************************************************
036100110811     C* Ricerca tipo impegno alla trasmissione
036200110811     C************************************************************
036300110811     C     F4_ImpTrs     BEGSR
036400110811     C*
036500110811     C                   Reset                   ANA964DS
036600110811     C*
036700110811     C                   MoveL     '01'          Opz964
036800110811     C                   MoveL     'ANDIZ000'    Trc964
036900110811     C                   MoveL     'SDGIMPTRS'   Cam964
037000110811     C                   MoveL     *On           All964
037100110811     C*
037200110811     C                   MoveL     ANA964DS      KPJBU
037300110811     C                   CALL      'ANA964R'
037400110811     C                   PARM                    KPJBA
037500110811     C                   MoveL     KPJBU         ANA964DS
037600110811     C*
037700110811     C                   if        Ret964     <> '2'
037800110811     C                   MoveL     Val964        D2ImpTrs
037900110811     C                   endif
038000110811     C*
038100110811     C                   ENDSR
038200131114R132 C************************************************************
038300131114 "   C* Ricerca tipo stampa
038400131114R132 C************************************************************
038500131114     C     F4_STPL       BEGSR
038600131114     C*
038700131114     C                   Reset                   ANA964DS
038800131114     C*
038900131114     C                   MoveL     '01'          Opz964
038901160202R195 C*
039000131204R132 C                   Select
039001160202R195 C*
039002160202R195 C* Black list ANTE 2016
039100160202R132 C                   When      TpComG60    = 'B'
039101160202R195 C                             or
039102160202 "   C* o Black list annuale da 2016
039103160202 "   C                             TpComG60    = 'L'
039104160202R195 C*
039200160202R132 C                   MoveL     'ANDIZ000'    Trc964
039300160202R132 C                   MoveL     'DSPSTAMBL'   Cam964
039301160202R195 C*
039400131204R132 C                   Other
039401160202R195 C* Spesometro o Acquisti da RSM
039500131114     C                   MoveL     'SDGSG60S'    Trc964
039600131114     C                   MoveL     'STPLG60  '   Cam964
039601160202R195 C*
039700131204R132 C                   endSl
039701160202R195 C*
039800131114     C                   MoveL     *On           All964
039900131114     C*
040000131114     C                   MoveL     ANA964DS      KPJBU
040100131114     C                   CALL      'ANA964R'
040200131114     C                   PARM                    KPJBA
040300131114     C                   MoveL     KPJBU         ANA964DS
040400131114     C*
040500131114     C                   if        Ret964     <> '2'
040600131114     C                   MoveL     Val964        D1STPL
040700131114     C                   endif
040800131114     C*
040900131114R132 C                   ENDSR
040901160609R224 C************************************************************
040902160609 "   C* Ricerca valori per Destinazione dati anagrafici aggiuntivi
040903160609 "   C************************************************************
040904160609R224 C     F4_DestCon    BEGSR
040905160609     C*
040906160609     C*
040907160609     C                   RESET                   XTABDS
040908160609     C*
040909160609     C                   MOVE      '01'          XTAOPZ
040910160609     C                   MOVE      XSCSOC        XTAAZI
040911160609     C                   MOVE      XSCLIN        XTALIN
040912160609     C                   Z-ADD     CURR          XTARIG
040913160609     C                   Z-ADD     CURC          XTACOL
040914160609     C                   CLEAR                   KPJBU
040915160609     C                   MOVEL     XTABDS        KPJBU
040916160609     C                   CALL      'SDGHC4R'
040917160609     C                   PARM                    KPJBA
040918160609     C                   MOVEL     KPJBU         XTABDS
040919160609     C*
040920160609     C                   if        XTaErr     <> '1'  and
040921160609     C                             XTaKey     <> *Blank
040922160609     C                   Move      XTaKey        D4DestCon
040923160609     C                   endif
040924160609     c*
040925160609R224 C                   ENDSR
040926160609R224 C/EJECT
041000110421     C************************************************************
041100110421     C* GESTIONE EMISSIONE TASTI FUNZIONE
041200110421     C************************************************************
041300110421     C     GesPie        BegSr
041400110421     C*
041500110421     C                   Select
041600110421     C*
041700110421     C     $GEST         WhenEq    'D1'
041800110421     C     $ULKD1        IfEq      0
041900110421     C* riempimento opzioni
042000000000     C                   Exsr      F24D1
042100000000     C                   ElSe
042200000000     C                   Write     FMTZ1
042300000000     C                   EndIf
042400000000     C*
042500000000     C     $GEST         WhenEq    'D2'
042501160609R224 C     $GEST         oreq      'D4'
042600000000     C     $ULKD2        IfEq      0
042700000000     C* riempimento opzioni
042800000000     C                   Exsr      F24D2
042900000000     C                   ElSe
043000000000     C                   Write     FMTZ2
043100000000     C                   EndIf
043200000000     c*
043300000000     C* Modifica valori assunti
043400000000     C     $GEST         WhenEq    'D3'
043500000000     C     $ULKD3        IfEq      0
043600000000     C                   Exsr      F24D3
043700000000     C                   ElSe
043800000000     C                   Write     FMTZ3
043900000000     C                   EndIf
044000000000     C                   EndSl
044100000000     C*
044200000000     C                   EndSr
044300000000     C************************************************************
044400000000     C* GESTIONE VIDEO RECORD D1
044500000000     C************************************************************
044600000000     C     GesD1         BegSr
044601160610R224 C*
044602160610 "   C* Ulteriore definizione delle opzioni utilizzabili a pgm
044603160610 "   C                   EXSR      UltOpz
044604160610R224 C*
044700000000     C* inizializzo videata
044800000000     C     $InzD1        IfEq      *ON
044900000000     C                   Exsr      InzD1
045000000000     C                   Move      *OFF          $InzD1
045100000000     C                   EndIf
045200000000     C* emissione piede videata se proveniente da altra
045300000000     C* salvataggio valore formato di provenienza
045400000000     C     $LastV        IFNE      'D1'
045500000000     C                   Write     FMTT1
045600000000     C                   Exsr      GESPIE
045700000000     C                   Move      $LastV        $LastG
045800000000     C                   Move      'D1'          $LastV
045900000000     C                   EndIf
046000000000     C*
046100000000     C* se esistono errori sulla videata
046200000000     C* emetto la Write del formato a indicatori spenti per vedere
046300000000     C* le eventuali decodifiche
046400000000     C     *IN99         IfEq      *ON
046500000000     C     $GEST         ANDEQ     'D1'
046600000000     C                   MoveA     *IN           WIN
046700000000     C                   Move      *ALL'0'       IN5098           49
046800000000     C                   MoveA     IN5098        *IN(50)
046900000000     C                   Write     FMTD1
047000000000     C                   MoveA     WIN           *IN
047100000000     C                   EndIf
047200000000     C*
047300000000     C                   EXFMT     FMTD1
047400000000     C*
047500000000     C* controllo abilitazione tasti funzione
047600000000     C                   Exsr      CTRKD1
047700000000     C*
047800000000     C     *IN99         IfEq      *OFF
047900000000     C*
048000000000     C                   Select
048100000000     C* ROLLUP
048200000000     C     $Tasto        WhenEq    ROLLUP
048300000000     C                   Move      'D2'          $GEST
048400000000     C* F2=Decodifica
048500000000     C     $Tasto        WhenEq    F02
048600000000     C                   Exsr      F02D1
048700000000     C* F3=Fine
048800000000     C     $Tasto        WhenEq    F03
048900000000     C                   Exsr      F03D1
049000000000     C* F4=Ricerca
049100000000     C     $Tasto        WhenEq    F04
049200000000     C                   Exsr      SEARCH
049300000000     C* F5=Refresh
049400000000     C     $Tasto        WhenEq    F05
049500000000     C                   Exsr      F05D1
049600000000     C* F8=Successivo
049700000000     C     $Tasto        WhenEq    F08
049800000000     C                   Exsr      F08D1
049900000000     C* F12=Ritorno
050000000000     C     $Tasto        WhenEq    F12
050100000000     C                   Exsr      F12D1
050200000000     C* F18=Valori assunti
050300000000     C     $Tasto        WhenEq    F18
050400000000     C                   Exsr      F18D1
050401160609R224 C* F19=Dati contribuente
050402160609 "   C     $Tasto        WhenEq    F19
050403160609R244 C                   Exsr      F19D1
050500000000     C* F20=Dati rappresentante
050600000000     C     $Tasto        WhenEq    F20
050700000000     C                   Exsr      F20D1
050800000000     C* F21=Pers. batch
050900000000     C     $Tasto        WhenEq    F21
051000000000     C                   Exsr      F21D1
051100000000     C* F24=Altri tasti
051200000000     C     $Tasto        WhenEq    F24
051300000000     C                   Exsr      F24D1
051400000000     C*
051500000000     C                   Other
051600000000     C                   Exsr      CTRD1
051700000000     C* aggiungere le chiamate ai controlli delle videate supplementari
051800000000     C     *IN99         IfEq      *OFF
051900000000     C                   Exsr      CTRD2
052000000000     C     *IN99         IfEq      *ON
052100000000     C                   Seton                                            96
052200000000     C                   EndIf
052300000000     C                   EndIf
052400000000     C*
052500000000     C     $Tasto        IfEq      F06
052600000000     C     *IN99         ANDEQ     *OFF
052700000000     C*
052800140417R133 C                   Eval      $OkExeLan   = *On
052900000000     C*
053000110811     C* controllo se esiste gia il file con il nome reperito
053100140417R133 C****               EXSR      CtrFileTel
053200000000     C*
053300000000     C* segnalo la richiesta di file a grandezza imposta
053400000000     C                   If        $OkExeLan   = *On
053500110811     C****               Exsr      CtrInvio
053600000000     C                   EndIf
053700000000     C*
053800000000     C                   If        $OkExeLan   = *On
053900000000     C* lancio
054000131114R132 C                   Select
054001160202R195 C*
054002160202R195 C* Black list Polivalente ANTE 2016
054100140328R132 C                   When      TpComG60    = 'B'
054200140328R132 C                   Exsr      ExeLanBL
054201160202R195 C*
054202160202 "   C* Black list annuale da 2016
054203160202 "   C                   When      TpComG60    = 'L'
054204160202 "   C                   Exsr      ExeLan
054205160202 "   C*
054206160202R195 C* Acqusti da San Marino
054300140328R133 C                   When      TpComG60    = 'S'
054400140328R133 C                   Exsr      ExeLanASM
054401160202R195 C*
054500131114R132 C                   Other
054502160202R195 C* Spesometro
054600000000     C                   Exsr      ExeLan
054601160202R195 C*
054700131114R132 C                   endSl
054800131114R132 C*
054900000000     C                   Exsr      EndPgm
055000000000     C*
055100000000     C                   EndIf
055200000000     C*
055300000000     C                   EndIf
055400000000     C*
055500000000     C                   EndSl
055600000000     C*
055700000000     C                   EndIf
055800000000     C*
055900000000     C                   EndSr
055901160610R224 C************************************************************
055902160610 "   C* ULTERIORE DEFINIZIONE OPZIONI UTILIZZABILI A PGM
055903160610 "   C************************************************************
055904160610R224 C     UltOpz        BEGSR
055905160610     C*
055906160610     C*-----------------------------------------------------------
055907160610     C* dico che non ho ancora caricato alcun tasto funzione, così
055908160610     C* li carica tutti di nuovo
055909160610     C                   Z-ADD     0             $ULKD1                         piede da emett. D1
055910160610     C*-----------------------------------------------------------
055911160610     C*
055912160610     C* Questa routine deve eseguire una ulteriore definizione
055913160610     C* dei tasti funzione a seconda che la società contribuente
055914160610     C* abbia ragione sociale di persona fisica o persona giuridica
055915160610     C*
055916160610     C* se persona fisica, mostro il tasto F19=Contribuente
055917160610     C                   if        TpNG16     = '1'
055918160610     C* F19=Contribuente
055919160610     C                   Z-ADD     19            X
055920160610     C                   MOVE      '1'           $FC1(X)
055929160610     C*
055930160610     C                   else
055931160610     C*
055932160610     C* se persona giuridica, nascondo il tasto F19=Contribuente
055933160610     C*
055934160610     C* F19=Contribuente
055935160610     C                   Z-ADD     19            X
055936160610     C                   MOVE      '0'           $FC1(X)
055939160610     C*
055951160610     C                   endif
055952160610     C*
055953160610     C* Reperimento tasti di funzione
055954160610     C*
055955160610     C                   EXSR      REPKEY
055956160610     C*
055957160610R224 C                   ENDSR
055958160610R224 C/EJECT
056000000000     C************************************************************
056100000000     C* INIZIALIZZAZIONE VIDEATA DATI
056200000000     C************************************************************
056300000000     C     InzD1         BegSr
056400000000     C*
056500000000     C                   Clear                   FMTD1
056600000000     C                   Clear                   FMTD2
056700000000     C*
056800131114R132 X****               Eval      D1On1       = XScOn
056900131114R132 X****               Eval      D1Off1      = XScOff
057000131114R132 C
057100160202R132 C                   Select
057101160202R195 C*
057102160202R195 C* Black list Polivalente ANTE 2016
057200140328R132 C                   When      TpComG60    = 'B'
057300140328R132 C                   Eval      *In11       = *On
057301160202R195 C*
057302160202R195 C* Acquisti da RSM
057400140328R133 C                   When      TpComG60    = 'S'
057500140328R133 C                   Eval      *In12       = *On
057501160202R195 C*
057600140328R132 C                   Other
057602160202R195 C* Spesometro
057603160204R195 C* o Black list annuale da 2016
057700140328R132 C                   Eval      *In11       = *Off
057800140328R133 C                   Eval      *In12       = *Off
057801160202R195 C*
057900131114R132 C                   endSl
058000000000     C*
058100000000     C                   Z-Add     1             $PIED1
058200000000     C*
058300110802     C* se il pgm e'stato richiamato imposto i valori ricevuti dalla DS
058400000000     C                   If        CallPgm     = '1'
058500000000     C*
058600130809     C                   Exsr      RieD1G60
058700000000     C*
058800110802     C* se il pgm NON e stato richiamato
058900000000     C                   ElSe
059000000000     C*
059100000000     C* Se vuole impostazione lancio da ultima elaborazione, li imposto da anmpe
059200000000     C     $Val          IfEq      *ON
059300000000     C* riempio la videata da parametri memorizzati
059400000000     C                   Exsr      RIED1
059500000000     C*  Aggiungere le routine di riempimento dei record succcessivi
059600000000     C                   Exsr      RIED2
059700000000     C                   ElSe
059800000000     C* Immettere i valodi di default a video
059900000000     C                   Eval      D1Anno      = *Year
060000150407R178 X*ex R133           If           TpComG60    = *blanks
060001160202R195 C*
060002160202R195 C* Spesometro
060003150407R178 C                   If        TpComG60    = 'E'
060005160202R195 C                             or
060006160202 "   C* o Black list annuale da 2016
060007160202R195 C                             TpComG60    = 'L'
060100110802     C                   Eval      D1Anno      = D1Anno - 1
060200140417R133 C                   EndIf
060201160202R195 C*
060300131114R132 X****               Eval      D1StpL      = XscOff
060400131114R132 C                   Eval      D1StpL      = *Blank
060500130809     C                   Eval      D1MaxDim    = 3
060600130809     C                   Eval      D1MaxRec    = AgeMaxRec
060700130809     C                   EndIf
060800130809     C*
060900130809     C                   EndIf
061000130809     C*
061100130809     C* richiamo routine dei ctrl per decodificarla
061200130809     C                   Exsr      CTRD1
061300130809     C* Aggiungere le routine di controllo dei record succcessivi
061400130809     C                   Exsr      CTRD2
061500130809     C                   Move      *ALL'0'       IN5098
061600130809     C                   MoveA     IN5098        *IN(50)
061700130809     C*
061800130809     C                   EndSr
061900130809     C************************************************************
062000130809     C* Riempimento videata  D1 da richiamo pgm
062100130809     C************************************************************
062200130809     C     RieD1G60      BegSr
062300130809     C*
062400130809     C                   Z-Add     AnnoG60       D1Anno
062500130809     C                   Z-Add     PROTOCG60     D1Protoc
062600130809     C                   Z-Add     PROTODG60     D1Protod
062700130809     C                   Eval      D1TipInv    = TipInvG60
062800130809     C                   Z-Add     MaxDimG60     D1MaxDim
062900130809     C                   Z-Add     MaxRecG60     D1MaxRec
063000130809     C                   Eval      D1File      = FileG60
063100130809     C*
063200131114R132 X****               MoveL     XscOff        D1StpL
063300131114R132 C                   MoveL     *Blank        D1StpL
063400130809     C*
063500130809     C                   EndSr
063600130809     C************************************************************
063700130809     C* RIEMPIMENTO VIDEATA  D1
063800130809     C************************************************************
063900130809     C     RIED1         BegSr
064000130809     C*
064100130809     C* impostare i campi della videata dai parametri memorizzati su anmpe
064200130809     C                   Z-Add     $Anno         D1Anno
064300130809     C                   Z-Add     $MaxDim       D1MaxDim
064400130809     C                   Z-Add     $MaxRec       D1MaxRec
064500000000     C                   Movel     $File         D1File
064600130809     C                   Eval      D1TipInv    = $TipInv
064700131115R132 C                   Move      $Mese         D1Mese
064800131115 "   C                   Move      $Trim         D1Trim
064900131115R132 C                   Move      $StpL         D1StpL
065000130809     C*
065100131114R132 X****               MoveL     XscOff        D1StpL
065200130809     C*
065300130809     C                   EndSr
065400130809     C************************************************************
065500130809     C* GESTIONE F02 VIDEO D1
065600130809     C************************************************************
065700130809     C     F02D1         BegSr
065800130809     C*
065900130809     C* per decodificare la videata richiamo la routine dei ctrl
066000130809     C* e poi disattivo gli eventuali messaggi di errore
066100130809     C                   Exsr      CTRD1
066200130809     C*
066300130809     C                   Move      *ALL'0'       IN5098
066400130809     C                   MoveA     IN5098        *IN(50)
066500130809     C*
066600130809     C                   EndSr
066700000000     C************************************************************
066800000000     C* GESTIONE F03 VIDEO D1
066900000000     C************************************************************
067000000000     C     F03D1         BegSr
067100000000     C*
067200000000     C                   Move      *ON           $FINE
067300000000     C* fine programma
067400000000     C                   Exsr      EndPgm
067500000000     C*
067600000000     C                   EndSr
067700000000     C************************************************************
067800000000     C* GESTIONE F05 VIDEO D1
067900000000     C************************************************************
068000000000     C     F05D1         BegSr
068100000000     C*
068200000000     C                   Move      *ON           $InzD1
068300000000     C*
068400000000     C                   EndSr
068500000000     C************************************************************
068600000000     C* GESTIONE F08 VIDEO D1
068700000000     C************************************************************
068800000000     C     F08D1         BegSr
068900000000     C*
069000000000     C                   Move      *ON           $FINE
069100000000     C*
069200000000     C                   EndSr
069300000000     C************************************************************
069400000000     C* GESTIONE F12 VIDEO D1
069500000000     C************************************************************
069600000000     C     F12D1         BegSr
069700000000     C*
069800000000     C                   Move      *ON           $FINE
069900000000     C*
070000000000     C                   EndSr
070100000000     C************************************************************
070200000000     C* GESTIONE F18 VIDEO D1
070300000000     C************************************************************
070400000000     C     F18D1         BegSr
070500000000     C*
070600000000     C                   Move      'D3'          $GEST
070700000000     C                   Move      *ON           $INZD3
070800000000     C*
070900000000     C                   EndSr
070901160609R224 C************************************************************
070902160609 "   C* GESTIONE F19 VIDEO D1
070903160609 "   C************************************************************
070904160609R224 C     F19D1         BegSr
070905160609     C*
070906160609     C                   Move      'D4'          $GEST
070907160609     C                   Move      *ON           $INZD4
070908160609     C*
070909160609R224 C                   EndSr
071000000000     C************************************************************
071100000000     C* GESTIONE F20 VIDEO D2
071200000000     C************************************************************
071300000000     C     F20D1         BegSr
071400000000     C*
071500000000     C                   Move      'D2'          $GEST
071600000000     C                   Move      *ON           $INZD2
071700000000     C*
071800000000     C                   EndSr
071900000000     C************************************************************
072000000000     C* GESTIONE F21 VIDEO D1
072100000000     C************************************************************
072200000000     C     F21D1         BegSr
072300000000     C*
072400000000     C                   Move      '    '        KCOAZ
072500000000     C                   CALL      'BCH09'
072600000000     C                   PARM                    KPJBA
072700000000     C*
072800000000     C                   EndSr
072900000000     C************************************************************
073000000000     C* GESTIONE F24 VIDEO D1
073100000000     C************************************************************
073200000000     C     F24D1         BegSr
073300000000     C*
073400000000     C                   MoveA     $FX1          $FX
073500000000     C                   MoveA     $FC1          $FC
073600000000     C                   MoveA     $FR1          $FR
073700000000     C                   Z-Add     $ULKD1        $ULK
073800000000     C                   Z-Add     79            $L01
073900000000     C                   Z-Add     79            $L02
074000000000     C                   Exsr      RIEKEY
074100000000     C                   Z-Add     $ULK          $ULKD1            3 0
074200000000     C                   MoveA     $FC           $FC1
074300000000     C                   MoveL     $KEY1         Z1KE1
074400000000     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
074500000000     C                   MoveL     $KEY2         Z1KE2
074600000000     C                   Write     FMTZ1
074700000000     C*
074800000000     C                   EndSr
074900000000     C************************************************************
075000000000     C* GESTIONE F24 VIDEO D3
075100000000     C************************************************************
075200000000     C     F24D2         BegSr
075300000000     C*
075400000000     C                   MoveA     $FX2          $FX
075500000000     C                   MoveA     $FC2          $FC
075600000000     C                   MoveA     $FR2          $FR
075700000000     C                   Z-Add     $ULKD2        $ULK
075800000000     C                   Z-Add     79            $L01
075900000000     C                   Z-Add     79            $L02
076000000000     C                   Exsr      RIEKEY
076100000000     C                   Z-Add     $ULK          $ULKD2            3 0
076200000000     C                   MoveA     $FC           $FC2
076300000000     C                   MoveL     $KEY1         Z2KE1
076400000000     C                   MoveL     $ALLFUNCT     H3ALLFUNCT
076500000000     C                   MoveL     $KEY2         Z2KE2
076600000000     C                   Write     FMTZ2
076700000000     C*
076800000000     C                   EndSr
076900000000     C************************************************************
077000000000     C* GESTIONE F24 VIDEO D3
077100000000     C************************************************************
077200000000     C     F24D3         BegSr
077300000000     C*
077400000000     C                   MoveA     $FX3          $FX
077500000000     C                   MoveA     $FC3          $FC
077600000000     C                   MoveA     $FR3          $FR
077700000000     C                   Z-Add     $ULKD3        $ULK
077800000000     C                   Z-Add     44            $L01
077900000000     C                   Z-Add     0             $L02
078000000000     C                   Exsr      RIEKEY
078100000000     C                   Z-Add     $ULK          $ULKD3            3 0
078200000000     C                   MoveA     $FC           $FC3
078300000000     C                   MoveL     $KEY1         Z3KE1
078400000000     C                   MoveL     $ALLFUNCT     H3ALLFUNCT
078500000000     C                   Write     FMTZ3
078600000000     C*
078700000000     C                   EndSr
078800000000     C************************************************************
078900000000     C* CONTROLLO VIDEATA
079000000000     C************************************************************
079100000000     C     CtrD1         BegSr
079200000000     C*
079300000000     C                   SETOFF                                       99
079400130809     C* non imposto pos. del cursore
079500130809     C                   Z-Add     0             H1RIGA
079600130809     C                   Z-Add     0             H1COLO
079700130809     C*
079800130809     C* anno
079900130809     C                   Exsr      CtrAnno
080000131114R132 C*
080100131114 "   C                   Clear                   D1PerD
080200160202R132 C                   Select
080201160202R195 C* Black List Polivalente ANTE 2016
080300140328R132 C                   When      TpComG60    = 'B'
080400140328R132 C                   If        D1Mese     <> 0        and
080500131114 "   C                             D1Trim     <> 0
080600131114 "   C*  mese e trimestre sono in alternativa
080700131114 "   C                   Seton                                        6099
080800131114 "   C                   ElSe
080900131114 "   C                   If        D1Mese      = 0        and
081000131114 "   C                             D1Trim      = 0
081100131114 "   C*  almeno uno dei due
081200131114 "   C                   Seton                                        5499
081300131114 "   C                   ElSe
081400131114 "   C                   If        D1Mese     <> 0
081500131114 "   C                   Exsr      CtrMese
081600131114 "   C                   EndIf
081700131114 "   C                   If        D1Trim     <> 0
081800131114 "   C                   Exsr      CtrTrim
081900131114 "   C                   EndIf
082000131114 "   C                   EndIf
082100131114 "   C                   EndIf
082200140328R132 C*
082201160202R195 C* Acquisti da San Marino
082300140328R133 C                   When         TpComG60    = 'S'
082400140328 "   C                   If        D1Mese     <> 0
082500140328 "   C                   Exsr      CtrMese
082600140328 "   C                   ElSe
082700140328 "   C                   Seton                                        5999
082800140328 "   C                   EndIf
082900140328R133 C*
083000131114R132 C                   endSl
083100130809     C*
083200130809     C* tipo invio
083300130809     C                   Clear                   D1TipInvD
083400130809     C                   Exsr      CtrTipInv
083500130809     C*
083600130809     C* protocollo  telematico e documento
083700130809     C                   Exsr      CtrProtoco
083800110421     C*
083900000000     C* Reperimento nome file per passaggio a agenzia
084000140417R133 C*****              Exsr      RepNomeF
084100000000     C*
084200000000     C* file da cui recuperare t.rec. A/B
084300140417R133 C*****              EXSR      CtrFile
084400000000     C*
084500110802     C* Tipo stampa
084600110421     C                   Exsr      CtrStpl
084700110421     C*
084800000000     C                   EndSr
084900000000     C***********************************************************
085000000000     C* Controllo anno
085100000000     C***********************************************************
085200000000     C     CtrAnno       BegSr
085300000000     C*
085400000000     C                   If        D1Anno      = 0
085500000000     C                   Seton                                        5099
085600000000     C                   EndIf
085700000000     C*
085800000000     C                   EndSr
085900131114R132 C***********************************************************
086000131114 "   C* Controllo mese
086100131114 "   C***********************************************************
086200131114R132 C     CtrMese       BegSr
086300131114     C*
086400131114     C                   Clear                   X07ValDs
086500131114     C*
086600131114     C                   Move      '1'           X07Ric
086700131114     C                   Move      *On           X07All
086800131114     C                   Movel     'ANDIZ000'    X07TRc
086900131114     C                   Movel     'DSPMESE   '  X07Cam
087000131114     C                   Movel     D1Mese        X07VAL
087100131114     C*
087200131114     C                   Callb     'X07VALR'
087300131114     C                   Parm                    X07ValDs
087400131114     C*
087500131114     C     X07ERR        IfEq      *Off
087600131114     C                   MoveL     X07DES        D1PerD
087700131114     C                   ElSe
087800131114     C                   Seton                                        5799
087900131114     C                   EndIf
088000131114     C*
088100131114R132 C                   EndSr
088200131114R132 C***********************************************************
088300131114 "   C* Controllo trimestre
088400131114 "   C***********************************************************
088500131114R132 C     CtrTrim       BegSr
088600131114     C*
088700131114     C                   Z-Add     D1Trim        WrkTrim           1 0
088800131114     C                   Clear                   X07ValDs
088900131114     C*
089000131114     C                   Move      '1'           X07Ric
089100131114     C                   Move      *On           X07All
089200131114     C                   Movel     'SDGEBL000'   X07TRc
089300131114     C                   Movel     'EBLTRIM   '  X07Cam
089400131114     C                   Movel     WrkTrim       X07VAL
089500131114     C*
089600131114     C                   Callb     'X07VALR'
089700131114     C                   Parm                    X07ValDs
089800131114     C*
089900131114     C     X07ERR        IfEq      *Off
090000131114     C                   MoveL     X07DES        D1PerD
090100131114     C                   ElSe
090200131114     C                   Seton                                        5899
090300131114     C                   EndIf
090400131114     C*
090500131114R132 C                   EndSr
090600110421     C***********************************************************
090700110421     C* Controllo tipo invio
090800110421     C***********************************************************
090900110421     C     CtrTipInv     BegSr
091000110421     C*
091100110421     C                   Clear                   D1TipInvD
091200110421     C*
091300110421     C                   Clear                   X07ValDs
091400110421     C*
091500110421     C                   Move      '1'           X07Ric
091600110421     C                   Move      *On           X07All
091700130809     C                   Movel     'SDGSG60S'    X07TRc
091800130809     C                   Movel     'TIPINVG60'   X07Cam
091900110802     C                   Movel     D1TIPINV      X07VAL
092000110421     C*
092100110421     C                   Callb     'X07VALR'
092200110421     C                   Parm                    X07ValDs
092300110421     C*
092400110421     C     X07ERR        IFEQ      *Off
092500110421     C                   MOVEL     X07DES        D1TipInvD
092600110802     C                   ElSe
092700110421     C                   SetOn                                        5299
092800110802     C                   EndIf
092900110421     C*
093000110421     C                   EndSr
093100130809     C***********************************************************
093200130809     C* Controllo protocollo
093300130809     C***********************************************************
093400130809     C     CtrProtoco    BEGSR
093500130809     C*
093600130809     C*  Telematico
093700130809     C                   If        D1TipInv   > 0    and
093800130809     C                             D1Protoc   = 0
093900130809     C                   SetOn                                        6199
094000130809     C                   EndIf
094100130809     C                   If        D1TipInv   = 0    and
094200130809     C                             D1Protoc   > 0
094300130809     C                   SetOn                                        6599
094400130904     C                   EndIf
094500130904     C*
094600130904     C*  Documento
094700130904     C                   If        D1TipInv   > 0    and
094800130904     C                             D1Protod   = 0
094900130904     C                   SetOn                                        6799
095000130904     C                   EndIf
095100130904     C                   If        D1TipInv   = 0    and
095200130904     C                             D1Protod   > 0
095300130809     C                   SetOn                                        6999
095400130809     C                   EndIf
095500130809     C*
095600130809     C                   EndSr
095700130809     C***********************************************************
095800130809     C* Controllo tipo stampa
095900130809     C***********************************************************
096000130809     C     CtrSTPL       BEGSR
096100130809     C*
096200131114R132 X****               If        D1StpL     <> XScOn     and
096300131114 "   X****                         D1StpL     <> XScOff
096400131114 "   X****               SetOn                                        6899
096500131114R132 X****               EndIf
096600131114R132 C*
096700131114 "   C                   Clear                   D1StpLD
096800131114 "   C*
096900131114 "   C                   Clear                   X07ValDs
097000131114 "   C*
097100131114 "   C                   Move      '1'           X07Ric
097200160202R132 C                   Move      *On           X07All
097201160202R195 C*
097300160202R132 C                   Select
097301160202R195 C*
097302160202R195 C* Black list Polivalente ANTE 2016
097400160202R132 C                   When      TpComG60    = 'B'
097401160202R195 C                             or
097402160202 "   C* o Black list annuale da 2016
097403160202 "   C                             TpComG60    = 'L'
097404160202R195 C*
097500160202R132 C                   MoveL     'ANDIZ000 '   X07TRc
097600160202R132 C                   MoveL     'DSPSTAMBL'   X07Cam
097601160202R195 C*
097700160202R132 C                   Other
097701160202R195 C*
097702160202R195 C* Spesometro o Acquisti da San Marino
097800160202R132 C                   Movel     'SDGSG60S'    X07TRc
097900131114 "   C                   Movel     'STPLG60  '   X07Cam
098000160202R132 C                   endSl
098001160202R195 C*
098100160202R132 C                   Movel     D1StpL        X07VAL
098200131114 "   C*
098300131114 "   C                   Callb     'X07VALR'
098400131114 "   C                   Parm                    X07ValDs
098500131114 "   C*
098600131114 "   C     X07ERR        IFEQ      *Off
098700131114 "   C                   MOVEL     X07DES        D1StpLD
098800131114 "   C                   ElSe
098900131114 "   C                   SetOn                                        5399
099000131114R132 C                   EndIf
099100130809     C*
099200130809     C                   EndSr
099300000000     C***********************************************************
099400000000     C* Controllo parametri per invio multiplo
099500000000     C***********************************************************
099600000000     C     CtrMltInvio   BegSr
099700000000     C*
099800000000     C                   If        D1MaxDim   <> 0   and
099900110810     C                             D1MaxRec   <> 0
100000110511     C                   Seton                                        6499
100100000000     C                   EndIf
100200000000     C*
100300000000     C                   EndSr
100400110811     C***********************************************************
100500110811     C* Controllo tipo impegno alla trasmissione
100600110811     C***********************************************************
100700110811     C     CtrImpTrs     BegSr
100800110811     C*
100900110811     C                   Clear                   D2ImpTrsD
101000110811     C*
101100110811     C                   Clear                   X07ValDs
101200110811     C*
101300110811     C                   Move      '1'           X07Ric
101400110811     C                   Move      *On           X07All
101500110811     C                   Movel     'ANDIZ000'    X07TRc
101600110811     C                   Movel     'SDGIMPTRS'   X07Cam
101700110811     C                   Movel     D2IMPTRS      X07VAL
101800110811     C*
101900110811     C                   Callb     'X07VALR'
102000110811     C                   Parm                    X07ValDs
102100110811     C*
102200110811     C     X07ERR        IFEQ      *Off
102300110811     C                   MOVEL     X07DES        D2ImpTrsD
102400110811     C                   ElSe
102500110811     C                   SetOn                                        6899
102600110811     C                   EndIf
102700110811     C*
102800110811     C                   EndSr
102900000000     C************************************************************
103000000000     C* Reperimento nome file per invio agenzia delle entrate
103100000000     C************************************************************
103200000000     C     RepNomeF      BegSr
103300000000     C*
103400000000     C                   Eval      WNumero     = *All'0'
103500000000     C                   Z-Add     D1Anno        WAnno
103600110811     C                   Z-Add     1             WPrgInvio
103700000000     C                   Move      WNumero       $Numero
103800140401     C*
103900140401R133 C                   Select
103901160202R195 C* Spesometro
104000140401R133 C                   When      TpComG60    = 'E'
104100140401     C                   Move      'OP'          $CdScarico
104101160202R195 C*
104102160202R195 C* Acquisti da San Marino
104200140401R133 C                   When      TpComG60    = 'S'
104300140401R133 C                   Move      'SM'          $CdScarico
104301160202R195 C*
104302160202R195 C* Black List Polivalente ANTE 2016
104400140417R133 C                   When      TpComG60    = 'B'
104500140417R133 C                   Move      'BP'          $CdScarico
104501160208R195 C*
104502160208 "   C* Black list annuale da 2016
104503160208 "   C                   When      TpComG60    = 'L'
104504160208 "   C                   Move      'BA'          $CdScarico
104505160208R195 C*
104600140417R133 C                   EndSl
104700000000     C*
104800000000     C                   CALL      'X35NOMFR'
104900000000     C                   PARM                    XScSoc
105000000000     C                   PARM      *Blank        $Unita            8
105100140401R133 C****               PARM      'OP'          $CdScarico        2
105200140401R133 C                   PARM                    $CdScarico        2
105300000000     C                   PARM                    $Numero           9
105400000000     C                   PARM                    $File            10
105500000000     C                   PARM                    Lib022
105600000000     C                   PARM      *Off          $Errore           1
105700000000     C*
105800000000     C                   If        $Errore    <> '0'
105900110511     C                   Seton                                        5499
106000000000     C                   EndIf
106100000000     C*
106200000000     C                   EndSr
106300000000     C************************************************************
106400000000     C* Verifico esistenza file con stesso nome
106500000000     C************************************************************
106600000000     C     CtrFileTel    BegSr
106700000000     C*
106800000000     C*  CHKOBJ     OBJ($LIBRF/&FILE) OBJTYPE(*FILE)
106900000000     C                   Eval      Cmd         =
107000000000     C                             'CHKOBJ OBJ(' +
107100000000     C                             %Trim(Lib022) + '/' +
107200000000     C                             %Trim($File)  +
107300000000     C                             ') ' +
107400000000     C                             'OBJTYPE(*FILE)'
107500000000     C*
107600000000     C                   Call      'QCMDEXC'                            21
107700000000     C                   Parm                    Cmd             120
107800000000     C                   Parm      120           Len              15 5
107900000000     C*
108000000000     C*
108100000000     C                   Eval      $OkExeLan   = *Off
108200000000     C*
108300110421     C* se il file   presente nella libreria lo segnalo
108400000000     C                   If        *In21       = *Off
108500000000     C*
108600000000     C                   EXSR      MsgErr
108700000000     C*
108800000000     C                   Select
108900000000     C*
109000000000     C* richiesto di continuare
109100000000     C                   When      MsgRtn      = '1'
109200000000     C                   Eval      $OkExeLan   = *On
109300000000     C*
109400000000     C*
109500000000     C* richiesto di non continuare
109600000000     C                   When      MsgRtn      = '2'
109700000000     C                   Eval      $OkExeLan   = *Off
109800000000     C*
109900000000     C                   endsl
110000000000     C*
110100000000     C                   else
110200000000     C* se file non presente nella libreria, eseguo lancio
110300000000     C                   Eval      $OkExeLan   = *On
110400000000     C*
110500000000     C                   endIf
110600000000     C*
110700000000     C                   EndSr
110800000000     C************************************************************
110900000000     C* Controllo file per recupero rec. "A" e "B"
111000000000     C************************************************************
111100000000     C     CtrFile       BegSr
111200000000     C*
111300000000     C                   If        D1File    <> *Blank
111400000000     C*
111500110418     C* il file deve avere nome diverso da quello che andra a generare
111600000000     C                   If        D1File     = $File
111700000000     C                   Seton                                        6299
111800000000     C                   endIf
111900000000     C*
112000000000     C* verifico se esiste, nella libreria
112100000000     C* dei file per lo scarico su supporto magnetico,
112200000000     C* il file indicato a video
112300000000     C                   EXSR      ChkFileBl
112400000000     C*
112500000000     C                   endIf
112600000000     C*
112700000000     C                   EndSr
112800000000     C************************************************************
112900000000     C* Verifico l'esistenza del file nella libreria indicata
113000000000     C* per lo scarico su supporto magnetico
113100000000     C************************************************************
113200000000     C     ChkFileBl     BegSr
113300000000     C*
113400000000     C* CHKOBJ     OBJ(Lib022/D1File) OBJTYPE(*FILE)
113500000000     C                   Eval      Cmd         =
113600000000     C                             'CHKOBJ     OBJ('        +
113700000000     C                             %Trim(Lib022) + '/' +
113800000000     C                             %Trim(D1File) +
113900000000     C                             ') OBJTYPE(*FILE)'
114000000000     C*
114100000000     C                   Call      'QCMDEXC'                            21
114200000000     C                   Parm                    Cmd             120
114300000000     C                   Parm      120           Len              15 5
114400000000     C*
114500000000     C                   If        *In21       = *On
114600000000     C*
114700110418     C* Errore: dico che non il file non e stato trovato nella libreria
114800000000     C* Valorizzo le variabili del msg PRO1797
114900000000     C                   Move      D1File        §File
115000000000     C                   Move      Lib022        §Libreria
115100000000     C                   Seton                                        5499
115200000000     C*
115300000000     C                   endIf
115400000000     C*
115500000000     C                   EndSr
115600000000     C************************************************************
115700000000     C* Messaggio errore
115800000000     C************************************************************
115900000000     C     MsgErr        BegSr
116000000000      *
116100000000     C                   Reset                   SDGMSGDS
116200000000     C*
116300000000     C* Risposte possibili
116400000000     C* "1) Continuare      2) Annulla
116500000000     C                   Eval      IDMSG       =
116600000000     C                             'PROMSG    *LIBL     COS1204'
116700000000     C                   CALLB     'XRTVM'
116800000000     C                   PARM                    IDMSG
116900000000     C                   PARM      *Blank        TXTMSG
117000000000     C                   PARM                    ERRMSG
117100000000     C     ERRMSG        IFNE      *ON
117200000000     C                   MoveL     TXTMSG        XMSGTXT
117300000000     C                   ELSE
117400000000     C                   MoveL     *ALL'?'       XMSGTXT
117500000000     C                   ENDIf
117600000000     C*
117700000000     C                   MoveL     'S'           MSGEMV
117800000000     C                   Z-Add     10            MSGRGP
117900000000     C                   MoveL     'S'           MSGRSP
118000000000     C                   MoveL     '12'          MSGTPR
118100000000      *
118200000000     C                   Movel     $File         $P1COS2161
118300000000     C                   Move      Lib022        $P1COS2161
118400000000      *
118500000000     C                   call      'SDGMSGR'
118600000000     C                   parm                    SDGMSGDS
118700000000     C                   parm                    XMsgTxt         135
118800000000     C*
118900000000     C* Modo di inizializzazione della window:
119000000000     C* ('1'->Close + Open  -->Iø volta dopo l'emissione del video princip.)
119100000000     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
119200000000     C*                       per mantenere il sottofondo del video princip.)
119300000000     C                   parm      '1'           XMsgInz           1
119400000000     C*
119500000000      * COS2161-Il file &1 esiste gia nella libreria &2'
119600000000     C                   parm      'COS2161'     XMsgCd1           7
119700000000     C                   parm                    XMsgCd2           7
119800000000     C                   parm                    XMsgCd3           7
119900000000     C                   parm                    XMsgSn1           7
120000000000     C                   parm                    XMsgCdTxT        63
120100000000     C                   parm                    MsgCdTit         14
120200000000     C                   parm                    MsgClPrtf         1
120300000000     C                   parm      $P1COS2161    VarMsg1         100
120400000000     C                   parm                    VarMsg2         100
120500000000     C                   parm                    VarMsg3         100
120600000000     C*
120700000000     C                   ENDSR
120800000000     C************************************************************
120900000000     C* Verifico esistenza file con stesso nome
121000000000     C************************************************************
121100000000     C     CtrInvio      BegSr
121200000000     C*
121300000000     C                   Eval      $OkExeLan   = *Off
121400000000     C*
121500000000     C* se presenti parametri per creazione file con capacita limitata
121600000000     C* lo segnalo
121700000000     C                   If        D1MaxDim    > 0    or
121800110810     C                             D1MaxRec    > 0
121900000000     C*
122000000000     C                   Exsr      MsgErr3
122100000000     C*
122200000000     C                   Select
122300000000     C*
122400000000     C* richiesto di continuare
122500000000     C                   When      MsgRtn      = '1'
122600000000     C                   Eval      $OkExeLan   = *On
122700000000     C*
122800000000     C*
122900000000     C* richiesto di non continuare
123000000000     C                   When      MsgRtn      = '2'
123100000000     C                   Eval      $OkExeLan   = *Off
123200000000     C*
123300000000     C                   EndSl
123400000000     C*
123500000000     C                   ElSe
123600000000     C* se file non presenti tali parametri, eseguo lancio
123700000000     C                   Eval      $OkExeLan   = *On
123800000000     C*
123900000000     C                   EndIf
124000000000     C*
124100000000     C                   EndSr
124200000000     C************************************************************
124300000000     C* Messaggio errore
124400000000     C************************************************************
124500000000     C     MsgErr3       BegSr
124600000000      *
124700000000     C                   Reset                   SDGMSGDS
124800000000     C*
124900000000     C* Risposte possibili
125000000000     C* "1) Continuare      2) Annulla
125100000000     C                   Eval      IDMSG       =
125200000000     C                             'PROMSG    *LIBL     COS1204'
125300000000     C                   CALLB     'XRTVM'
125400000000     C                   PARM                    IDMSG
125500000000     C                   PARM      *Blank        TXTMSG
125600000000     C                   PARM                    ERRMSG
125700000000     C     ERRMSG        IFNE      *ON
125800000000     C                   MoveL     TXTMSG        XMSGTXT
125900000000     C                   ElSe
126000000000     C                   MoveL     *ALL'?'       XMSGTXT
126100000000     C                   EndIf
126200000000     C*
126300000000     C                   MoveL     'S'           MSGEMV
126400000000     C                   Z-Add     10            MSGRGP
126500000000     C                   MoveL     'S'           MSGRSP
126600000000     C                   MoveL     '12'          MSGTPR
126700000000      *
126800000000     C                   call      'SDGMSGR'
126900000000     C                   parm                    SDGMSGDS
127000000000     C                   parm                    XMsgTxt         135
127100000000     C*
127200000000     C* Modo di inizializzazione della window:
127300000000     C* ('1'->Close + Open  -->Iø volta dopo l'emissione del video princip.)
127400000000     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
127500000000     C*                       per mantenere il sottofondo del video princip.)
127600000000     C                   parm      '1'           XMsgInz           1
127700000000     C*
127800000000     C* COS2670- (F1) E' stato impostato un parametro per la
127900000000     C* eazione di file da trasmettere con capacita di dati limitata.
128000000000     C*  Se il file che verra creato avra capacita (in MegaByte o in
128100000000     C* numero di soggetti) superiore a quanto indicato, verranno
128200000000     C* creati altri file con la capacita richiesta fino a esaurimento
128300000000     C* dati e che dovranno essere traspessi con piu invii.
128400000000     C                   parm      'COS2670'     XMsgCd1           7
128500000000     C                   parm                    XMsgCd2           7
128600000000     C                   parm                    XMsgCd3           7
128700000000     C                   parm                    XMsgSn1           7
128800000000     C                   parm                    XMsgCdTxT        63
128900000000     C                   parm                    MsgCdTit         14
129000000000     C                   parm                    MsgClPrtf         1
129100000000     C                   parm                    VarMsg1         100
129200000000     C                   parm                    VarMsg2         100
129300000000     C                   parm                    VarMsg3         100
129400000000     C*
129500000000     C                   EndSr
129600000000     C************************************************************
129700000000     C* GESTIONE VIDEO RECORD  D2
129800000000     C************************************************************
129900000000     C     GESD2         BegSr
130000000000     C*
130100000000     C* inizializzo videata
130200000000     C     $InzD2        IfEq      *ON
130300000000     C                   Exsr      InzD2
130400000000     C                   Move      *OFF          $InzD2
130500000000     C                   EndIf
130600000000     C*
130700000000     C     $LastV        IFNE      'D2'
130800000000     C                   Write     FMTT1
130900000000     C                   Exsr      GESPIE
131000000000     C                   Move      $LastV        $LastG
131100000000     C                   Move      'D2'          $LastV
131200000000     C                   EndIf
131300000000     C*
131400000000     C* se esistono errori sulla videata
131500000000     C* emetto la Write del formato a indicatori spenti per vedere
131600000000     C* le eventuali decodifiche
131700000000     C     *IN99         IfEq      *ON
131800000000     C     $GEST         ANDEQ     'D2'
131900000000     C                   MoveA     *IN           WIN
132000000000     C                   Move      *ALL'0'       IN5098           49
132100000000     C                   MoveA     IN5098        *IN(50)
132200000000     C                   Write     FMTD2
132300000000     C                   MoveA     WIN           *IN
132400000000     C                   EndIf
132500000000     C*
132600000000     C                   EXFMT     FMTD2
132700000000     C*
132800000000     C* controllo abilitazione tasti funzione
132900000000     C                   Exsr      CTRKD2
133000000000     C*
133100000000     C     *IN99         IfEq      *OFF
133200000000     C*
133300000000     C                   Select
133400000000     C* ROLDOWN
133500000000     C     $Tasto        WhenEq    ROLDWN
133600000000     C                   Move      'D1'          $GEST
133700000000     C* F3=Fine
133800000000     C     $Tasto        WhenEq    F03
133900000000     C                   Exsr      F03D1
134000000000     C* F4=Ricerca
134100000000     C     $Tasto        WhenEq    F04
134200000000     C                   Exsr      SEARCH
134300000000     C* F5=Refresh
134400000000     C     $Tasto        WhenEq    F05
134500000000     C                   Exsr      F05D2
134600000000     C* F8=Successivo
134700000000     C     $Tasto        WhenEq    F08
134800000000     C                   Exsr      F08D1
134900000000     C* F12=Ritorno
135000000000     C     $Tasto        WhenEq    F12
135100000000     C                   Exsr      F12D2
135200000000     C* F18=Valori assunti
135300000000     C     $Tasto        WhenEq    F18
135400000000     C                   Exsr      F18D1
135500000000     C* F21=Pers. batch
135600000000     C     $Tasto        WhenEq    F21
135700000000     C                   Exsr      F21D1
135800000000     C* F24=Altri tasti
135900000000     C     $Tasto        WhenEq    F24
136000000000     C                   Exsr      F24D1
136100000000     C*
136200000000     C                   Other
136300000000     C                   Exsr      CTRD2
136400000000     C     *IN99         IfEq      *OFF
136500000000     C***                Exsr      CTRD1
136600000000     C***  *IN99         IfEq      *ON
136700000000     C***                Seton                                            96
136800000000     C***                EndIf
136900000000     C                   EndIf
137000000000     C*
137100000000     C*
137200000000     C     $Tasto        IfEq      F06
137300000000     C     *IN99         ANDEQ     *OFF
137400000000     C*
137500000000     C                   Exsr      AggD2
137600131108R140 X****               Move      $LastG        $GEST
137700131108R140 C                   Move      'D1'          $GEST
137800000000     C*
137900000000     C                   EndIf
138000000000     C*
138100000000     C*
138200000000     C                   EndSl
138300000000     C*
138400000000     C                   EndIf
138500000000     C*
138600000000     C                   EndSr
138700000000     C************************************************************
138800110811     C* REPERIMENTO dati per intermediario
138900000000     C************************************************************
139000000000     C     InzD2         BegSr
139100000000     C*
139200000000     C* preparo i parametri di chiamata al driver
139300000000     C                   Clear                   XMPEDS
139400000000     C                   EVAL      TpChiamata = '01'
139500000000     C                   EVAL      NChiavi = 5
139600000000     C                   EVAL      VLogica = 2
139700000000     C*
139800000000     C* preparo le chiavi per la chiamata al driver
139900000000     C                   Clear                   XMPEDS1
140000000000     C                   MoveL     XSCSOC        DSMPESOC
140100000000     C                   MoveL     DSPGM         DSMPEPGM
140101160202R195 C*
140200140422R133 C                   Select
140201160202R195 C*
140202160202R195 C* Black list Polivalente ANTE 2016 o da 2016
140300140422R133 C                   When      TpComG60    = 'B'
140301160202R195 C                             or
140302160202 "   C* o Black list annuale da 2016
140303160202R195 C                             TpComG60    = 'L'
140400140422R133 C                   Move      'B'           DSMPEPGM
140401160202R195 C*
140402160202R195 C* Acquisti da San Marino
140500140422R133 C                   When      TpComG60    = 'S'
140600140422R133 C                   Move      'S'           DSMPEPGM
140601160202R195 C*
140700140422R133 C                   Other
140701160202R195 C* Spesometro
140800140422     C                   Move      'R'           DSMPEPGM
140900140422R133 C                   endSl
140901160202R195 C*
141000000000     C                   MoveL     *blanks       DSMPEPRF
141100000000     C* Si reperiscono i dati di default a seconda delle modalita con
141200000000     C* cui il pgm e usato
141300000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
141400000000     C* MPETIP = 'PV'  pgm richiamato da menu
141500000000     C* MPETIP = 'PB'  x lancio Batch
141600000000     C                   MoveL     'PB'          DSMPETIP
141700000000     C*
141800000000     C                   CALLB     'XMPE'
141900000000     C                   PARM                    XMPEDS
142000000000     C                   PARM                    XMPEDS1
142100000000     C*
142200130913     C                   Clear                   DSInterm
142300130913     C                   Clear                   $ImpDta
142400130913     C                   Clear                   $DtISog
142500130913     C                   Clear                   $DtFSog
142600130913     C                   Clear                   $DtNSog
142700000000     C*
142800000000     C     Risultato     IfEq      '0'
142900110811     C                   MoveL     DSMPEPAR      DSInterm
143000130913     C                   If        $ImpDta       = *blanks
143100130913     C                   Move      $DT0          $ImpDta
143200130913     C                   EndIf
143300130913     C                   If        $DtISog       = *blanks
143400130913     C                   Move      $DT0          $DtISog
143500130913     C                   EndIf
143600130913     C                   If        $DtFSog       = *blanks
143700130913     C                   Move      $DT0          $DtFSog
143800130913     C                   EndIf
143900130913     C                   If        $DtNSog       = *blanks
144000130913     C                   Move      $DT0          $DtNSog
144100130913     C                   EndIf
144200000000     C*
144300130913     C* Internediario
144400110811     C                   MoveL(P)  $FisInt       D2FisInt
144500110811     C                   Move      $CafInt       D2CafInt
144600110811     C                   Move      $ImpTrs       D2ImpTrs
144700110811     C* Data impegno
144800110811     C                   MOVEL     $ImpDta       DataISO
144900110811     C                   If        DataISO = *LOVAL
145000110811     C                   Eval      D2ImpDta = *ZEROS
145100110811     C                   Else
145200110811     C                   Select
145300110811     C* formato mm/gg/aaaa
145400110811     C                   When      $DatFmt = '*MDY'
145500110811     C     *USA          Move      DataISO       D2ImpDta
145600110811     C* formato aa/mm/gg
145700110811     C                   When      $DatFmt = '*YMD'
145800110811     C     *ISO          Move      DataISO       D2ImpDta
145900110811     C* formato gg/mm/aaaa
146000110811     C                   When      $DatFmt = '*DMY'
146100110811     C     *eur          MOVEL     DataISO       D2ImpDta
146200110811     C                   EndSl
146300110811     C                   EndIf
146400130913     C*
146500130913     C* Soggetto
146600130913     C                   MoveL(P)  $FisSog       D2FisSog
146700130913     C                   Move      $CarSog       D2CarSog
146800130913     C* Data inizio
146900130913     C                   MOVEL     $DtISog       DataISO
147000130913     C                   If        DataISO = *LOVAL
147100130913     C                   Eval      D2DtISog = *ZEROS
147200130913     C                   Else
147300130913     C                   Select
147400130913     C* formato mm/gg/aaaa
147500130913     C                   When      $DatFmt = '*MDY'
147600130913     C     *USA          Move      DataISO       D2DtISog
147700130913     C* formato aa/mm/gg
147800130913     C                   When      $DatFmt = '*YMD'
147900130913     C     *ISO          Move      DataISO       D2DtISog
148000130913     C* formato gg/mm/aaaa
148100130913     C                   When      $DatFmt = '*DMY'
148200130913     C     *eur          MOVEL     DataISO       D2DtISog
148300130913     C                   EndSl
148400130913     C                   EndIf
148500130913     C* Data fine
148600130913     C                   MOVEL     $DtFSog       DataISO
148700130913     C                   If        DataISO = *LOVAL
148800130913     C                   Eval      D2DtFSog = *ZEROS
148900130913     C                   Else
149000130913     C                   Select
149100130913     C* formato mm/gg/aaaa
149200130913     C                   When      $DatFmt = '*MDY'
149300130913     C     *USA          Move      DataISO       D2DtFSog
149400130913     C* formato aa/mm/gg
149500130913     C                   When      $DatFmt = '*YMD'
149600130913     C     *ISO          Move      DataISO       D2DtFSog
149700130913     C* formato gg/mm/aaaa
149800130913     C                   When      $DatFmt = '*DMY'
149900130913     C     *eur          MOVEL     DataISO       D2DtFSog
150000130913     C                   EndSl
150100130913     C                   EndIf
150200130913     C                   MoveL     $COGSOG       D2COGSOG
150300130913     C                   MoveL     $NOMSOG       D2NOMSOG
150400130913     C                   MoveL     $SEXSOG       D2SEXSOG
150500130913     C* Data nascita
150600130913     C                   MOVEL     $DtNSog       DataISO
150700130913     C                   If        DataISO = *LOVAL
150800130913     C                   Eval      D2DtNSog = *ZEROS
150900130913     C                   Else
151000130913     C                   Select
151100130913     C* formato mm/gg/aaaa
151200130913     C                   When      $DatFmt = '*MDY'
151300130913     C     *USA          Move      DataISO       D2DtNSog
151400130913     C* formato aa/mm/gg
151500130913     C                   When      $DatFmt = '*YMD'
151600130913     C     *ISO          Move      DataISO       D2DtNSog
151700130913     C* formato gg/mm/aaaa
151800130913     C                   When      $DatFmt = '*DMY'
151900130913     C     *eur          MOVEL     DataISO       D2DtNSog
152000130913     C                   EndSl
152100130913     C                   EndIf
152200110811     C*
152300130913     C                   MoveL     $COMSOG       D2COMSOG
152400130913     C                   MoveL     $PRVSOG       D2PRVSOG
152500130913     C                   MoveL     $RAGSOG       D2RAGSOG
152600130913     C*
152700110811     C                   ElSe
152800110811     C                   Exsr      RepDefImp
152900110811     C                   EndIf
153000110811     C*
153100000000     C*
153200000000     C                   EndSr
153300000000     C************************************************************
153400000000     C* GESTIONE VALORI ASSUNTI
153500000000     C************************************************************
153600000000     C     GESD3         BegSr
153700000000     C*
153800000000     C     $INZD3        IfEq      *ON
153900000000     C                   Exsr      INZD3
154000000000     C                   Move      *OFF          $INZD3
154100000000     C                   EndIf
154200000000     C*
154300000000     C     $LastV        IFNE      'D3'
154400000000     C                   Exsr      GESPIE
154500000000     C                   Move      $LastV        $LastG
154600000000     C                   Move      'D3'          $LastV
154700000000     C                   EndIf
154800000000     C*
154900000000     C* se esistono errori sulla videata
155000000000     C* emetto la Write del formato a indicatori spenti per vedere
155100000000     C* le eventuali decodifiche
155200000000     C     *IN99         IfEq      *ON
155300000000     C                   MoveA     *IN           WIN
155400000000     C                   Move      *ALL'0'       IN5098           49
155500000000     C                   MoveA     IN5098        *IN(50)
155600000000     C                   Write     FMTD3
155700000000     C                   MoveA     WIN           *IN
155800000000     C                   EndIf
155900000000     C*
156000000000     C                   EXFMT     FMTD3
156100000000     C*
156200000000     C* controllo abilitazione tasti funzione
156300000000     C                   Exsr      CTRKD3
156400000000     C*
156500000000     C     *IN99         IfEq      *OFF
156600000000     C*
156700000000     C                   Select
156800000000     C* F3=Fine
156900000000     C     $Tasto        WhenEq    F03
157000000000     C                   Exsr      F03D1
157100000000     C* F12=Ritorno
157200000000     C     $Tasto        WhenEq    F12
157300000000     C                   Exsr      F12D3
157400000000     C* F24=Altri tasti
157500000000     C     $Tasto        WhenEq    F24
157600000000     C                   Exsr      F24D3
157700000000     C*
157800000000     C                   Other
157900000000     C                   Exsr      CTRD3
158000000000     C     *IN99         IfEq      *OFF
158100000000     C                   Move      $LastG        $GEST
158200000000     C                   EndIf
158300000000     C*
158400000000     C                   EndSl
158500000000     C*
158600000000     C                   EndIf
158700000000     C*
158800000000     C                   EndSr
158900000000     C************************************************************
159000000000     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
159100000000     C************************************************************
159200000000     C     INZD3         BegSr
159300000000     C*
159400000000     C                   Clear                   FMTD3
159500000000     C     $VAL          IfEq      *ON
159600000000     C                   Move      XSCON         D3VAL
159700000000     C                   ElSe
159800000000     C                   Move      XSCOFF        D3VAL
159900000000     C                   EndIf
160000000000     C                   Move      XSCON         D3ON
160100000000     C                   Move      XSCOFF        D3OF
160200000000     C*
160300000000     C                   EndSr
160400000000     C************************************************************
160500000000     C* RIEMPIMENTO VIDEATA D2
160600000000     C************************************************************
160700000000     C     RIED2         BegSr
160800000000     C*
160900000000     C*
161000000000     C                   EndSr
161001160609R224 C************************************************************
161002160609 "   C* GESTIONE VIDEO RECORD  D4
161003160609 "   C************************************************************
161004160609R224 C     GESD4         BegSr
161005160609     C*
161006160609     C* inizializzo videata
161007160609     C     $InzD4        IfEq      *ON
161008160609     C                   Exsr      InzD4
161009160609     C                   Move      *OFF          $InzD4
161010160609     C                   EndIf
161011160609     C*
161012160609     C     $LastV        IFNE      'D4'
161013160609     C                   Write     FMTT1
161014160609     C                   Exsr      GESPIE
161015160609     C                   Move      $LastV        $LastG
161016160609     C                   Move      'D4'          $LastV
161017160609     C                   EndIf
161018160609     C*
161019160609     C* se esistono errori sulla videata
161020160609     C* emetto la Write del formato a indicatori spenti per vedere
161021160609     C* le eventuali decodifiche
161022160609     C     *IN99         IfEq      *ON
161023160609     C     $GEST         ANDEQ     'D4'
161024160609     C                   MoveA     *IN           WIN
161025160609     C                   Move      *ALL'0'       IN5098           49
161026160609     C                   MoveA     IN5098        *IN(50)
161027160609     C                   Write     FMTD4
161028160609     C                   MoveA     WIN           *IN
161029160609     C                   EndIf
161030160609     C*
161031160609     C                   EXFMT     FMTD4
161032160609     C*
161033160609     C* controllo abilitazione tasti funzione
161034160609     C                   Exsr      CTRKD2
161035160609     C*
161036160609     C     *IN99         IfEq      *OFF
161037160609     C*
161038160609     C                   Select
161039160609     C* F3=Fine
161040160609     C     $Tasto        WhenEq    F03
161041160609     C                   Exsr      F03D1
161042160609     C* F4=Ricerca
161043160609     C     $Tasto        WhenEq    F04
161044160609     C                   Exsr      SEARCH
161045160609     C* F5=Refresh
161046160609     C     $Tasto        WhenEq    F05
161047160609     C                   Exsr      F05D4
161048160609     C* F8=Successivo
161049160609     C     $Tasto        WhenEq    F08
161050160609     C                   Exsr      F08D1
161051160609     C* F12=Ritorno
161052160609     C     $Tasto        WhenEq    F12
161053160609     C                   Exsr      F12D2
161054160609     C* F24=Altri tasti
161055160609     C     $Tasto        WhenEq    F24
161056160609     C                   Exsr      F24D1
161057160609     C*
161058160609     C                   Other
161059160609     C                   Exsr      CTRD4
161060160609     C     *IN99         IfEq      *OFF
161061160609     C                   EndIf
161062160609     C*
161063160609     C*
161064160609     C     $Tasto        IfEq      F06
161065160609     C     *IN99         ANDEQ     *OFF
161066160609     C*
161067160609     C                   Exsr      AggD4
161068160609     C                   Move      'D1'          $GEST
161069160609     C*
161070160609     C                   EndIf
161071160609     C*
161072160609     C*
161073160609     C                   EndSl
161074160609     C*
161075160609     C                   EndIf
161076160609     C*
161077160609R224 C                   EndSr
161078160609R224 C************************************************************
161079160609 "   C* REPERIMENTO dati per contribuente
161080160609 "   C************************************************************
161081160609R224 C     InzD4         BegSr
161082160609     C*
161083160609     C* preparo i parametri di chiamata al driver
161084160609     C                   Clear                   XMPEDS
161085160609     C                   EVAL      TpChiamata = '01'
161086160609     C                   EVAL      NChiavi = 5
161087160609     C                   EVAL      VLogica = 2
161088160609     C*
161089160609     C* preparo le chiavi per la chiamata al driver
161090160609     C                   Clear                   XMPEDS1
161091160609     C                   MoveL     XSCSOC        DSMPESOC
161092160609     C                   MoveL     'SDGSG60R  '  DSMPEPGM
161094160609     C*
161095160609     C                   Select
161096160609     C* Spesometro
161097160609     C                   When      TpComG60    = 'E'
161098160609     C                   Move      'EC'          DSMPEPGM
161099160609     C*
161100160609     C* Acquisti da San Marino
161101160609     C                   When      TpComG60    = 'S'
161102160609     C                   Move      'SC'          DSMPEPGM
161103160609     C*
161104160609     C* Black List
161105160610     C                   When      TpComG60    = 'L'
161106160609     C                   Move      'BC'          DSMPEPGM
161107160609     C*
161108160609     C                   endsl
161109160609     C*
161110160609     C                   MoveL     *blanks       DSMPEPRF
161111160609     C* Si reperiscono i dati di default a seconda delle modalita con
161112160609     C* cui il pgm e usato
161113160609     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
161114160609     C* MPETIP = 'PV'  pgm richiamato da menu
161115160609     C* MPETIP = 'PB'  x lancio Batch
161116160609     C                   MoveL     'PB'          DSMPETIP
161117160609     C*
161118160609     C                   CALLB     'XMPE'
161119160609     C                   PARM                    XMPEDS
161120160609     C                   PARM                    XMPEDS1
161121160609     C*
161122160609     C                   Clear                   DSContrib
161123160609     C*
161124160609     C     Risultato     IfEq      '0'
161125160609     C                   MoveL     DSMPEPAR      DSContrib
161126160609     C*
161127160609     C                   MoveL     $DestCon      D4DestCon
161128160609     C*
161129160609     C                   EndIf
161130160609     C*
161131160609     C*
161132160609R224 C                   EndSr
161133000000     C************************************************************
161200000000     C* GESTIONE F05 VIDEO D2
161300000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
161400000000     C* e il riposizionamento alla prima
161500000000     C************************************************************
161600000000     C     F05D2         BegSr
161700000000     C*
161800000000     C                   Move      *ON           $InzD1
161900000000     C                   Move      'D1'          $GEST
162000000000     C*
162100000000     C                   EndSr
162101160609R224 C************************************************************
162102160609 "   C* GESTIONE F05 VIDEO D4
162103160609 "   C************************************************************
162104160609R224 C     F05D4         BegSr
162105160609     C*
162106160609     C                   Move      *ON           $InzD4
162107160609     C                   Move      'D4'          $GEST
162108160609     C*
162109160609R224 C                   EndSr
162200000000     C************************************************************
162300000000     C* GESTIONE F12 VIDEO D2
162400000000     C************************************************************
162500000000     C     F12D2         BegSr
162600000000     C*
162700000000     C                   Move      'D1'          $GEST
162800000000     C*
162900000000     C                   EndSr
163000000000     C************************************************************
163100000000     C* GESTIONE F12 VIDEO D3
163200000000     C************************************************************
163300000000     C     F12D3         BegSr
163400000000     C*
163500000000     C                   Move      $LastG        $GEST
163600000000     C*
163700000000     C                   EndSr
163800000000     C************************************************************
163900000000     C* CONTROLLO VIDEATA  D2
164000000000     C************************************************************
164100000000     C     CtrD2         BegSr
164200000000     C*
164300000000     C                   SETOFF                                       99
164400000000     C* non imposto pos. del cursore
164500000000     C                   Z-Add     0             H1RIGA
164600130809     C                   Z-Add     0             H1COLO
164700130809     C*
164800130913     C                   Exsr      CtrIntermed
164900130913     C*
165000130913     C                   Exsr      CtrSoggetto
165100110811     C*
165200110811     C                   EndSr
165300110811     C************************************************************
165400130913     C* Controllo dati intermediario
165500110811     C************************************************************
165600130913     C     CtrIntermed   BegSr
165700000000     C*
165800130913     C                   MOVEL     $DT0          WDtImp
165900130913     C     D2ImpDta      IFNE      0
166000130913     C                   Select
166100130913     C* formato mm/gg/aaaa
166200130913     C                   When      $DatFmt = '*MDY'
166300130913     C     *USA          TEST(D)                 D2ImpDta               63
166400130913     C* formato aa/mm/gg
166500130913     C                   When      $DatFmt = '*YMD'
166600130913     C     *ISO          TEST(D)                 D2ImpDta               63
166700130913     C* formato gg/mm/aaaa
166800130913     C                   When      $DatFmt = '*DMY'
166900130913     C     *eur          TEST(D)                 D2ImpDta               63
167000130913     C                   EndSl
167100130913     C* data ok
167200130913     C                   If        *in63 = *off
167300130913     C                   Select
167400130913     C* formato mm/gg/aaaa
167500130913     C                   When      $DatFmt = '*MDY'
167600130913     C     *USA          Move      D2ImpDta      DataISO
167700130913     C* formato aa/mm/gg
167800130913     C                   When      $DatFmt = '*YMD'
167900130913     C     *ISO          Move      D2ImpDta      DataISO
168000130913     C* formato gg/mm/aaaa
168100130913     C                   When      $DatFmt = '*DMY'
168200130913     C     *eur          MoveL     D2ImpDta      DataISO
168300130913     C                   EndSl
168400130913     C                   MoveL     DataISO       WDtImp
168500130913     C                   Else
168600130913     C                   Seton                                          99
168700130913     C                   EndIf
168800130913     C                   EndIf
168900130913     C*
169000130913     C* tipo impegno alla trasmissione
169100130913     C                   Clear                   D2ImpTrsD
169200130913     C                   If        D2ImpTrs      <> *blanks
169300130913     C                   Exsr      CtrImpTrs
169400130913     C                   EndIf
169500130913     C*
169600130913     C                   If        (D2FisInt   =  *blanks or
169700130913     C                              D2ImpDta   =  *zeros  or
169800130913     C                              D2ImpTrs   =  *blanks ) and
169900130913     C                             (D2FisInt  <>  *blanks or
170000130913     C                              D2ImpDta  <>  *zeros  or
170100130913     C                              D2ImpTrs  <>  *blanks )
170200130913     C                   Seton                                        6799
170300130913     C                   EndIf
170400130913     C*
170500130913     C                   EndSr
170600130913     C************************************************************
170700130913     C* Controllo dati Soggetto
170800130913     C************************************************************
170900130913     C     CtrSoggetto   BegSr
171000130913     C*
171100130913     C* Dt.Ini.Soggetto
171200130913     C                   MOVEL     $DT0          WDtISog
171300130913     C     D2DtISog      IFNE      0
171400130913     C                   Select
171500130913     C* formato mm/gg/aaaa
171600130913     C                   When      $DatFmt = '*MDY'
171700130913     C     *USA          TEST(D)                 D2DtISog               64
171800130913     C* formato aa/mm/gg
171900130913     C                   When      $DatFmt = '*YMD'
172000130913     C     *ISO          TEST(D)                 D2DtISog               64
172100130913     C* formato gg/mm/aaaa
172200130913     C                   When      $DatFmt = '*DMY'
172300130913     C     *eur          TEST(D)                 D2DtISog               64
172400130913     C                   EndSl
172500130913     C* data ok
172600130913     C                   If        *in64 = *off
172700130913     C                   Select
172800130913     C* formato mm/gg/aaaa
172900130913     C                   When      $DatFmt = '*MDY'
173000130913     C     *USA          Move      D2DtISog      DataISO
173100130913     C* formato aa/mm/gg
173200130913     C                   When      $DatFmt = '*YMD'
173300130913     C     *ISO          Move      D2DtISog      DataISO
173400130913     C* formato gg/mm/aaaa
173500130913     C                   When      $DatFmt = '*DMY'
173600130913     C     *eur          MoveL     D2DtISog      DataISO
173700130913     C                   EndSl
173800130913     C                   MoveL     DataISO       WDtISog
173900130913     C                   Else
174000130913     C                   Seton                                          99
174100130913     C                   EndIf
174200130913     C                   EndIf
174300130913     C*
174400130913     C* Dt.Fine Soggetto
174500130913     C                   MOVEL     $DT0          WDtFSog
174600130913     C     D2DtFSog      IFNE      0
174700130913     C                   Select
174800130913     C* formato mm/gg/aaaa
174900130913     C                   When      $DatFmt = '*MDY'
175000130913     C     *USA          TEST(D)                 D2DtFSog               65
175100130913     C* formato aa/mm/gg
175200130913     C                   When      $DatFmt = '*YMD'
175300130913     C     *ISO          TEST(D)                 D2DtFSog               65
175400130913     C* formato gg/mm/aaaa
175500130913     C                   When      $DatFmt = '*DMY'
175600130913     C     *eur          TEST(D)                 D2DtFSog               65
175700130913     C                   EndSl
175800130913     C* data ok
175900130913     C                   If        *in65 = *off
176000130913     C                   Select
176100130913     C* formato mm/gg/aaaa
176200130913     C                   When      $DatFmt = '*MDY'
176300130913     C     *USA          Move      D2DtFSog      DataISO
176400130913     C* formato aa/mm/gg
176500130913     C                   When      $DatFmt = '*YMD'
176600130913     C     *ISO          Move      D2DtFSog      DataISO
176700130913     C* formato gg/mm/aaaa
176800130913     C                   When      $DatFmt = '*DMY'
176900130913     C     *eur          MoveL     D2DtFSog      DataISO
177000130913     C                   EndSl
177100130913     C                   MoveL     DataISO       WDtFSog
177200130913     C                   Else
177300130913     C                   Seton                                          99
177400130913     C                   EndIf
177500130913     C                   EndIf
177600130913     C*
177700130913     C* Dt.Nas.Soggetto
177800130913     C                   MOVEL     $DT0          WDtNSog
177900130913     C     D2DtNSog      IFNE      0
178000130913     C                   Select
178100130913     C* formato mm/gg/aaaa
178200130913     C                   When      $DatFmt = '*MDY'
178300130913     C     *USA          TEST(D)                 D2DtNSog               66
178400130913     C* formato aa/mm/gg
178500130913     C                   When      $DatFmt = '*YMD'
178600130913     C     *ISO          TEST(D)                 D2DtNSog               66
178700130913     C* formato gg/mm/aaaa
178800130913     C                   When      $DatFmt = '*DMY'
178900130913     C     *eur          TEST(D)                 D2DtNSog               66
179000130913     C                   EndSl
179100130913     C* data ok
179200130913     C                   If        *in66 = *off
179300130913     C                   Select
179400130913     C* formato mm/gg/aaaa
179500130913     C                   When      $DatFmt = '*MDY'
179600130913     C     *USA          Move      D2DtNSog      DataISO
179700130913     C* formato aa/mm/gg
179800130913     C                   When      $DatFmt = '*YMD'
179900130913     C     *ISO          Move      D2DtNSog      DataISO
180000130913     C* formato gg/mm/aaaa
180100130913     C                   When      $DatFmt = '*DMY'
180200130913     C     *eur          MoveL     D2DtNSog      DataISO
180300130913     C                   EndSl
180400130913     C                   MoveL     DataISO       WDtNSog
180500130913     C                   Else
180600130913     C                   Seton                                          99
180700130913     C                   EndIf
180800130913     C                   EndIf
180900130913     C*
181000130913     C                   If        (D2CogSog   <> *blanks or
181100130913     C                              D2NomSog   <> *blanks or
181200130913     C                              D2SexSog   <> *blanks or
181300130913     C                              D2ComSog   <> *blanks or
181400130913     C                              D2PrvSog   <> *blanks) and
181500130913     C                              D2RagSog   <> *blanks
181600130913     C                   Seton                                        7199
181700130913     C                   EndIf
181800130913     C*
181900130930     C                   If         D2RagSog   =  *blanks
182000130913     C                   If        (D2FisSog   =  *blanks or
182100130913     C                              D2CogSog   =  *blanks or
182200130913     C                              D2NomSog   =  *blanks or
182300130913     C                              D2SexSog   =  *blanks or
182400130913     C                              D2ComSog   =  *blanks or
182500130913     C                              D2PrvSog   =  *blanks) and
182600130913     C                             (D2FisSog  <>  *blanks or
182700130913     C                              D2CogSog  <>  *blanks or
182800130913     C                              D2NomSog  <>  *blanks or
182900130913     C                              D2SexSog  <>  *blanks or
183000130913     C                              D2ComSog  <>  *blanks or
183100130913     C                              D2PrvSog  <>  *blanks)
183200130913     C                   Seton                                        6999
183300130913     C                   EndIf
183400130930     C                   EndIf
183500130913     C*
183600131001     C                   If         D2FisSog   =  *blanks and
183700131001     C****                          D2CarSog   =  *zeros) and
183800130913     C                              D2RagSog  <>  *blanks
183900130913     C                   Seton                                        6999
184000130913     C                   EndIf
184100130913     C*
184200131001     C                   If         D2FisSog   <> *blanks and
184300131001     C                              D2CarSog   =  *zeros
184400131001     C                   Seton                                        7299
184500131001     C                   EndIf
184600131001     C*
184700131001     C                   If         D2FisSog   <> *blanks and
184800131001     C                              D2DtiSog   =  *zeros
184900131001     C                   Seton                                        7399
185000131001     C                   EndIf
185100131001     C*
185200130913     C                   EndSr
185300130913     C************************************************************
185400130913     C* CONTROLLO VIDEATA  D3
185500130913     C************************************************************
185600130913     C     CTRD3         BegSr
185700130913     C*
185800000000     C                   SETOFF                                       99
185900000000     C* non imposto pos. del cursore
186000000000     C                   Z-Add     0             H1RIGA
186100000000     C                   Z-Add     0             H1COLO
186200000000     C*
186300000000     c* Controllo VALORE
186400000000     C     D3VAL         IFNE      XSCON
186500000000     C     D3VAL         ANDNE     XSCOFF
186600000000     C                   Seton                                        5099
186700000000     C                   EndIf
186800000000     C*
186900000000     C     D3VAL         IfEq      XSCON
187000000000     C                   Move      *ON           $VAL
187100000000     C                   ElSe
187200000000     C                   Move      *OFF          $val
187300000000     c                   EndIf
187400000000     C*
187500000000     C                   EndSr
187501160609R224 C************************************************************
187502160609 "   C* CONTROLLO VIDEATA  D4
187503160609 "   C************************************************************
187504160609R224 C     CtrD4         BegSr
187505160609     C*
187506160609     C                   SETOFF                                       99
187507160609     C* non imposto pos. del cursore
187508160609     C                   Z-Add     0             H1RIGA
187509160609     C                   Z-Add     0             H1COLO
187510160609     C*
187511160609     C* Destinazione dati aggiuntivi
187512160609     C                   EXSR      CtrDestCon
187513160609     C*
187514160609R224 C                   EndSr
187515160609R224 C************************************************************
187516160609 "   C* Controllo destinazione contribuente
187517160609 "   C************************************************************
187518160609R224 C     CtrDestCon    BEGSR
187519160609     C*
187520160609     C                   Clear                   D4DestConD
187521160609     C*
187522160609     C                   Clear                   SDGHC4DS
187523160609     C*
187524160609     C                   Clear                   SDGTABDS
187525160609     C*
187526160609     C                   if        D4DestCon  <> *Blank
187527160609     C*
187528160609     C                   Eval      XTbRic      = '1'
187529160609     C                   Eval      XTbAzi      = XScSoc
187530160609     C                   Eval      XTbKey      = *Zero
187531160609     C                   Eval      XTbCod      = 'HC4'
187532160609     C                   Move      D4DestCon     XTbKey
187533160609     C*
187534160609     C                   CALL      'SDGTABDR'
187535160609     C                   PARM                    SDGTABDS
187536160609     C*
187537160609     C                   if        XTbErr     <> '0'
187538160609     C                   SetOn                                        7699
187539160609     C*
187540160609     C                   else
187541160609     C                   MoveL     XTbUni        SDGHC4DS
187542160609     C                   MoveL     DesHC4        D4DestConD
187543160609     C*
187544160609     C                   endif
187545160609     C*
187546160609     C                   else
187547160609     C* "Generica"
187548160609     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS1529'
187549160609     C                   EXSR      PrendiMsg
187550160609     C                   if        ERRMSG     <> *ON
187551160609     C                   MoveL     TXTMSG        D4DestConD
187552160609     C                   else
187553160609     C                   MoveL     *ALL'?'       D4DestConD
187554160609     C                   endif
187555160609     C*
187556160609     C                   endif
187557160609     C*
187558160609R224 C                   ENDSR
187559160609R224 C/EJECT
187560160609R224 C************************************************************
187561160609 "   C* Reperimento testo messaggi
187562160609 "   C************************************************************
187563160609R224 C     PrendiMsg     BEGSR
187564160609     C*
187565160609     C                   CALLB     'XRTVM'
187566160609     C                   PARM                    IDMSG            27
187567160609     C                   PARM                    TXTMSG          100
187568160609     C                   PARM                    ERRMSG            1
187569160609     C*
187570160609R224 C                   ENDSR
187571160609R224 C/EJECT
187600000000     C************************************************************
187700000000     C* ESECUZIONE LANCIO
187800000000     C************************************************************
187900130809     C     ExeLan        BegSr
188000130809     C*
188100130809     C                   Reset                   SDGSG61DS
188200130809     C*
188300130809     C                   Move      XscSoc        SocietaG61
188400130809     C                   Eval      AnnoG61     = D1Anno
188500130809     C                   Eval      ProtocG61   = D1Protoc
188600130809     C                   Eval      ProtoDG61   = D1Protod
188700130809     C                   Eval      TipInvG61   = D1TipInv
188800130809     C                   Eval      MaxDimG61   = D1MaxDim
188900130809     C                   Eval      MaxRecG61   = D1MaxRec
189000130809     C                   Eval      FileTelG61  = $File
189100130809     C                   Eval      FileG61     = D1File
189200130809     C                   Eval      LibG61      = Lib022
189300130809     C*
189400131114R132 X****               If        D1StpL      = XScOn
189500131204 "   X****               MoveL     *On           StpLG61
189600131204 "   X****               ElSe
189700131204 "   X****               MoveL     *Off          StpLG61
189800131204R132 X****               EndIf
189900131204R132 C                   Eval      StpLG61     = D1StpL
190000000000     C*
190001160202R195 C                   Select
190002160202 "   C* Spesometro
190003160202 "   C                   When      TpComG60    = 'E'
190004160202 "   C                   Eval      TpComG61    = 'E'
190005160202 "   C*
190006160202 "   C* Black list annuale da 2016
190007160202 "   C                   When      TpComG60    = 'L'
190008160202 "   C                   Eval      TpComG61    = 'B'
190009160202 "   C                   endSl
190010160202R195 C*
190100110421     C*
190200130809     C                   MoveL     SDGSG61DS     KPJBU
190300000000     C*
190400130809     C                   CALL      'SDGSG61C'
190500000000     C                   PARM                    KPJBA
190600000000     C*
190700130809     C                   MoveL     KPJBU         SDGSG61DS
190800000000     C*
190900000000     C* ritorno da PGM gestione
191000130809     C                   Exsr      RTNG61
191100000000     C*
191200000000     C* aggiorno valori assunti
191300000000     C                   Exsr      AGGMPE
191400000000     C*
191500000000     C                   EndSr
191600000000     C************************************************************
191700000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
191800000000     C************************************************************
191900130809     C     RTNG61        BegSr
192000000000     C*
192100000000     C* funzione non eseguibile per errore :
192200000000     C*
192300000000     C                   Select
192400000000     C* 0 = funzione eseguita correttamente
192500130809     C     ERRG61        WhenEq    '0'
192600000000     C                   Eval      $Fine       = *On
192700000000     C*
192800000000     C*
192900000000     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
193000130809     C     ERRG61        WhenEq    '1'
193100130809     C     ERRG61        OREQ      '2'
193200000000     C                   Seton                                        9399      emiss.SFLMSGID
193300000000     C*
193400000000     C                   EndSl
193500000000     C*
193600000000     C                   EndSr
193700131114R132 C************************************************************
193800131114 "   C* ESECUZIONE LANCIO  Black List
193900131114 "   C************************************************************
194000131114R132 C     ExeLanBL      BegSr
194100131114     C*
194200131115     C                   Clear                   SDGSG63DS
194300131114     C*
194400131114     C                   Move      XscSoc        SocietaG63
194500131114     C                   Eval      AnnoG63     = D1Anno
194600131115     C                   if        D1Mese      > 0
194700131115     C                   Eval      MeseG63     = D1Mese
194800131115     C                   Eval      TrimG63     = 0
194900131115     C                   else
195000131115     C                   Eval      TrimG63     = D1Trim
195100131115     C                   Eval      MeseG63     = 0
195200131115     C                   endif
195300131114     C                   Eval      ProtocG63   = D1Protoc
195400131114     C                   Eval      ProtoDG63   = D1Protod
195500131114     C                   Eval      TipInvG63   = D1TipInv
195600131114     C                   Eval      MaxDimG63   = D1MaxDim
195700131114     C                   Eval      MaxRecG63   = D1MaxRec
195800131114     C                   Eval      FileTelG63  = $File
195900131115     C                   Eval      LibG63      = Lib022
196000131114     C*
196100131115     C                   Eval      StpLG63     = D1StpL
196200131114     C*
196300131114     C*
196400131114     C                   MoveL     SDGSG63DS     KPJBU
196500131114     C*
196600131114     C                   CALL      'SDGSG63R'
196700131114     C                   PARM                    KPJBA
196800131114     C*
196900131114     C                   MoveL     KPJBU         SDGSG63DS
197000131114     C*
197100131114     C* ritorno da PGM gestione
197200131114     C                   Exsr      RTNG63
197300131114     C*
197400131114     C* aggiorno valori assunti
197500131114     C                   Exsr      AGGMPE
197600131114     C*
197700131114R132 C                   EndSr
197800131114R132 C************************************************************
197900131114 "   C* GESTIONE RITORNO DA PGM DI GESTIONE
198000131114 "   C************************************************************
198100131114R132 C     RTNG63        BegSr
198200131114     C*
198300131114     C* funzione non eseguibile per errore :
198400131114     C*
198500131114     C                   Select
198600131114     C* 0 = funzione eseguita correttamente
198700131114     C     ERRG63        WhenEq    '0'
198800131114     C                   Eval      $Fine       = *On
198900131114     C*
199000131114     C*
199100131114     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
199200131114     C     ERRG63        WhenEq    '1'
199300131114     C     ERRG63        OREQ      '2'
199400131114     C                   Seton                                        9399      emiss.SFLMSGID
199500131114     C*
199600131114     C                   EndSl
199700131114     C*
199800131114R132 C                   EndSr
199900140328R133 C************************************************************
200000140328 "   C* ESECUZIONE LANCIO  ACQUISTI DA SAN MARINO
200100140328 "   C************************************************************
200200140328R133 C     ExeLanASM     BegSr
200300140328     C*
200400140328     C                   Clear                   SDGSG64DS
200500140328     C*
200600140328     C                   Move      XscSoc        SocietaG64
200700140328     C                   Eval      AnnoG64     = D1Anno
200800140328     C                   Eval      MeseG64     = D1Mese
200900140328     C                   Eval      ProtocG64   = D1Protoc
201000140328     C                   Eval      ProtoDG64   = D1Protod
201100140328     C                   Eval      TipInvG64   = D1TipInv
201200140328     C                   Eval      MaxDimG64   = D1MaxDim
201300140328     C                   Eval      MaxRecG64   = D1MaxRec
201400140328     C                   Eval      FileTelG64  = $File
201500140328     C                   Eval      LibG64      = Lib022
201600140328     C*
201700140328     C                   Eval      StpLG64     = D1StpL
201800140328     C*
201900140328     C*
202000140328     C                   MoveL     SDGSG64DS     KPJBU
202100140328     C*
202200140328     C                   CALL      'SDGSG64C'
202300140328     C                   PARM                    KPJBA
202400140328     C*
202500140328     C                   MoveL     KPJBU         SDGSG64DS
202600140328     C*
202700140328     C* ritorno da PGM gestione
202800140328     C                   Exsr      RTNG64
202900140328     C*
203000140328     C* aggiorno valori assunti
203100140328     C                   Exsr      AGGMPE
203200140328     C*
203300140328R133 C                   EndSr
203400140328R133 C************************************************************
203500140328 "   C* GESTIONE RITORNO DA PGM DI GESTIONE
203600140328 "   C************************************************************
203700140328R133 C     RTNG64        BegSr
203800140328     C*
203900140328     C* funzione non eseguibile per errore :
204000140328     C*
204100140328     C                   Select
204200140328     C* 0 = funzione eseguita correttamente
204300140328     C     ERRG64        WhenEq    '0'
204400140328     C                   Eval      $Fine       = *On
204500140328     C*
204600140328     C*
204700140328     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
204800140328     C     ERRG64        WhenEq    '1'
204900140328     C     ERRG64        OREQ      '2'
205000140328     C                   Seton                                        9399      emiss.SFLMSGID
205100140328     C*
205200140328     C                   EndSl
205300140328     C*
205400140328R133 C                   EndSr
205500000000     C************************************************************
205600000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
205700000000     C************************************************************
205800000000     C     CTRKD1        BegSr
205900000000     C*
206000000000     C                   SETOFF                                       99
206100000000     C                   MoveA     $FC1          $FC
206200000000     C                   Exsr      CTRKEY
206300000000     C     *IN99         IfEq      *ON
206400000000     C                   Seton                                        97
206500000000     C                   EndIf
206600000000     C*
206700000000     C                   EndSr
206800000000     C************************************************************
206900000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
207000000000     C************************************************************
207100000000     C     CTRKD2        BegSr
207200000000     C*
207300000000     C                   SETOFF                                       99
207400000000     C                   MoveA     $FC2          $FC
207500000000     C                   Exsr      CTRKEY
207600000000     C     *IN99         IfEq      *ON
207700000000     C                   Seton                                        97
207800000000     C                   EndIf
207900000000     C*
208000000000     C                   EndSr
208100000000     C************************************************************
208200000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
208300000000     C* INPUT
208400000000     C* $FC3 = Abilitazioni videata D3
208500000000     C* OUTPUT
208600000000     C* *IN97 *IN99 Errori
208700000000     C************************************************************
208800000000     C     CTRKD3        BegSr
208900000000     C*
209000000000     C                   SETOFF                                       99
209100000000     C                   MoveA     $FC3          $FC
209200000000     C                   Exsr      CTRKEY
209300000000     C     *IN99         IfEq      *ON
209400000000     C                   Seton                                        97
209500000000     C                   EndIf
209600000000     C*
209700000000     C                   EndSr
209800000000     C************************************************************
209900000000     C* CONTROLLO ABILITAZIONE TASTI
210000000000     C************************************************************
210100000000     C     CTRKEY        BegSr
210200000000     C*
210300000000     C                   Select
210400000000     C     $Tasto        WhenEq    F01
210500000000     C     $FC(1)        IfEq      '0'
210600000000     C                   Seton                                        99
210700000000     C                   EndIf
210800000000     C     $Tasto        WhenEq    F02
210900000000     C     $FC(2)        IfEq      '0'
211000000000     C                   Seton                                        99
211100000000     C                   EndIf
211200000000     C     $Tasto        WhenEq    F03
211300000000     C     $FC(3)        IfEq      '0'
211400000000     C                   Seton                                        99
211500000000     C                   EndIf
211600000000     C     $Tasto        WhenEq    F04
211700000000     C     $FC(4)        IfEq      '0'
211800000000     C                   Seton                                        99
211900000000     C                   EndIf
212000000000     C     $Tasto        WhenEq    F05
212100000000     C     $FC(5)        IfEq      '0'
212200000000     C                   Seton                                        99
212300000000     C                   EndIf
212400000000     C     $Tasto        WhenEq    F06
212500000000     C     $FC(6)        IfEq      '0'
212600000000     C                   Seton                                        99
212700000000     C                   EndIf
212800000000     C     $Tasto        WhenEq    F07
212900000000     C     $FC(7)        IfEq      '0'
213000000000     C                   Seton                                        99
213100000000     C                   EndIf
213200000000     C     $Tasto        WhenEq    F08
213300000000     C     $FC(8)        IfEq      '0'
213400000000     C                   Seton                                        99
213500000000     C                   EndIf
213600000000     C     $Tasto        WhenEq    F09
213700000000     C     $FC(09)       IfEq      '0'
213800000000     C                   Seton                                        99
213900000000     C                   EndIf
214000000000     C     $Tasto        WhenEq    F10
214100000000     C     $FC(10)       IfEq      '0'
214200000000     C                   Seton                                        99
214300000000     C                   EndIf
214400000000     C     $Tasto        WhenEq    F11
214500000000     C     $FC(11)       IfEq      '0'
214600000000     C                   Seton                                        99
214700000000     C                   EndIf
214800000000     C     $Tasto        WhenEq    F12
214900000000     C     $FC(12)       IfEq      '0'
215000000000     C                   Seton                                        99
215100000000     C                   EndIf
215200000000     C     $Tasto        WhenEq    F13
215300000000     C     $FC(13)       IfEq      '0'
215400000000     C                   Seton                                        99
215500000000     C                   EndIf
215600000000     C     $Tasto        WhenEq    F14
215700000000     C     $FC(14)       IfEq      '0'
215800000000     C                   Seton                                        99
215900000000     C                   EndIf
216000000000     C     $Tasto        WhenEq    F15
216100000000     C     $FC(15)       IfEq      '0'
216200000000     C                   Seton                                        99
216300000000     C                   EndIf
216400000000     C     $Tasto        WhenEq    F16
216500000000     C     $FC(16)       IfEq      '0'
216600000000     C                   Seton                                        99
216700000000     C                   EndIf
216800000000     C     $Tasto        WhenEq    F17
216900000000     C     $FC(17)       IfEq      '0'
217000000000     C                   Seton                                        99
217100000000     C                   EndIf
217200000000     C     $Tasto        WhenEq    F18
217300000000     C     $FC(18)       IfEq      '0'
217400000000     C                   Seton                                        99
217500000000     C                   EndIf
217600000000     C     $Tasto        WhenEq    F19
217700000000     C     $FC(19)       IfEq      '0'
217800000000     C                   Seton                                        99
217900000000     C                   EndIf
218000000000     C     $Tasto        WhenEq    F20
218100000000     C     $FC(20)       IfEq      '0'
218200000000     C                   Seton                                        99
218300000000     C                   EndIf
218400000000     C     $Tasto        WhenEq    F21
218500000000     C     $FC(21)       IfEq      '0'
218600000000     C                   Seton                                        99
218700000000     C                   EndIf
218800000000     C     $Tasto        WhenEq    F22
218900000000     C     $FC(22)       IfEq      '0'
219000000000     C                   Seton                                        99
219100000000     C                   EndIf
219200000000     C     $Tasto        WhenEq    F23
219300000000     C     $FC(23)       IfEq      '0'
219400000000     C                   Seton                                        99
219500000000     C                   EndIf
219600000000     C     $Tasto        WhenEq    F24
219700000000     C     $FC(24)       IfEq      '0'
219800000000     C                   Seton                                        99
219900000000     C                   EndIf
220000000000     C                   EndSl
220100000000     C*
220200000000     C                   EndSr
220300000000     C************************************************************
220400000000     C* OPERAZIONI INIZIALI
220500000000     C************************************************************
220600000000     C     *INZSR        BegSr
220700000000     C*
220800000000     C* Reperimento parametri
220900000000     C*
221000000000     C     *ENTRY        PLIST
221100000000     C                   PARM                    KPJBA
221200000000     C*
221300000000     C* Reperimento tasti di funzione
221400000000     C*
221500000000     C                   Exsr      REPKEY
221600000000     C*
221700000000     C                   EndSr
221800000000     C************************************************************
221900000000     C* REPERIMENTO TASTI DI FUNZIONE
222000000000     C************************************************************
222100000000     C     REPKEY        BegSr
222200000000     C*
222300000000     C                   MoveA     $FM1          $FM
222400000000     C                   MoveA     $Fc1          $Fc
222500000000     C                   Exsr      REPKKK
222600000000     C                   MoveA     $FX           $FX1
222700000000     C*
222800000000     C                   MoveA     $FM2          $FM
222900000000     C                   MoveA     $Fc2          $Fc
223000000000     C                   Exsr      REPKKK
223100000000     C                   MoveA     $FX           $FX2
223200000000     C*
223300000000     C                   MoveA     $FM3          $FM
223400000000     C                   MoveA     $Fc3          $Fc
223500000000     C                   Exsr      REPKKK
223600000000     C                   MoveA     $FX           $FX3
223700000000     C*
223800000000     C                   EndSr
223900000000     C************************************************************
224000000000     C* REPERIMENTO TASTI DI FUNZIONE
224100000000     C************************************************************
224200000000     C     REPKKK        BegSr
224300000000     C*
224400000000     C                   Clear                   $FX
224500000000     C     1             DO        24            X
224600000000     C*
224700000000     C     $FM(X)        IFNE      *BLANK
224800000000     C     $fc(X)        andne     '0'
224900000000     C*
225000000000     C                   MoveL     $FM(X)        IDMSG
225100000000     C                   CALLB     'XRTVM'
225200000000     C                   PARM                    IDMSG            27
225300000000     C                   PARM                    TXTMSG          100
225400000000     C                   PARM                    ERRMSG            1
225500000000     C*
225600000000     C     ERRMSG        IFNE      *ON
225700000000     C                   MoveL     TXTMSG        $FX(X)
225800000000     C                   ElSe
225900000000     C                   MoveL     *ALL' '       $FX(X)
226000000000     C                   EndIf
226100000000     C*
226200000000     C                   EndIf
226300000000     C*
226400000000     C                   ENDDO
226500000000     C*
226600000000     C                   EndSr
226700000000     C************************************************************
226800000000     C* INIZIALIZZAZIONE VARIABILI
226900000000     C************************************************************
227000000000     C     INZVAR        BegSr
227100000000     C*
227200131114R132 C                   MoveL     KPJBU         SDGSG60DS
227300131114R132 C*
227400000000     C* se il pgm e stato richiamato imposto i valori ricevuti dalla DS
227500131114R132 X****               If        KPJBU      <> *Blanks
227600131114R132 C                   If        SocietaG60 <> *Blanks
227700000000     C                   Eval      CallPgm     = '1'
227800131114R132 X****               MoveL     KPJBU         SDGSG60DS
227900000000     C                   ElSe
228000000000     C                   Eval      CallPgm     = '0'
228100000000     C                   EndIf
228200000000     C*
228300000000     C* Reperimento dati societa
228400000000     C*
228500000000     C                   MoveL     'SOC001'      TIPXSC
228600131114R132 X****               MoveL     *BLANK        SOCXSC
228700131114R132 C                   MoveL     SocietaG60    SOCXSC
228800000000     C                   Exsr      REPSOC
228900000000     C     RTNXSC        IFNE      '1'
229000000000     C                   MoveL     XSOCDS        SOC001
229100000000     C                   ElSe
229200000000     C                   Return
229300000000     C                   EndIf
229301160613R224 C*
229302160613 "   C* DATI IDENTIFICATIVI DEL CONTRIBUENTE
229303160613 "   C*
229304160613 "   C* reperisco la natura giuridica del contribuente
229306160613 "   C                   EXSR      RepNatGiu
229307160613R224 C*
229400000000     C* Asteriscare se non presenti
229500000000     C*
229600000000     C* Reperimento valori default utente
229700000000     C*
229800000000     C                   Exsr      REPDFT
229900000000     C*
230000000000     C* Definizione dei tasti funzione utilizzabili a pgm
230100000000     C*
230200000000     C                   Exsr      DEFOPZ
230300000000     C*
230400000000     C* Variabili per gestione videate
230500000000     C*
230600000000     C                   Move      'D1'          $GEST
230700000000     C                   Move      *OFF          $FINE
230800000000     C                   Move      *ON           $InzD1
230900000000     C                   Move      *BLANK        $LastG
231000000000     C                   Move      *BLANK        $LastV
231100000000     C                   Z-Add     0             $ULKD1            3 0
231200000000     C                   Z-Add     0             $ULKD2            3 0
231300000000     C                   Z-Add     0             $ULKD3            3 0
231400000000     C*
231500000000     C* Variabili appoggio
231600000000     C*
231700000000     C                   Z-Add     0             CURR
231800000000     C                   Z-Add     0             CURC
231900000000     C*
232000000000     C* Valorizzazione campi univoci testate
232100000000     C*
232200000000     C                   Clear                   FMTT1
232300000000     C                   MoveL     KNSIf         NOMSIf
232400000000     C                   MoveL     XSCDSI        NOMDIT
232500000000     C                   MOVEL     KCDAZ         NOMAZN
232600000000     C                   MOVEL     DSPGM         NOMPGM           10
232700000000     C* recupero il sottotitolo (se c'e)
232800000000     C                   MoveL     *BLANK        D1TIT
232900000000     C     SOTTITD1      IFNE      *BLANK
233000000000     C                   MoveL     SOTTITD1      IDMSG
233100000000     C                   CALLB     'XRTVM'
233200000000     C                   PARM                    IDMSG            27
233300000000     C                   PARM                    TXTMSG          100
233400000000     C                   PARM                    ERRMSG            1
233500000000     C     ERRMSG        IFNE      *ON
233600000000     C                   MoveL     TXTMSG        D1TIT
233700000000     C                   ElSe
233800000000     C                   MoveL     *ALL'?'       D1TIT
233900000000     C                   EndIf
234000000000     C                   EndIf
234100000000     C*
234200160204R195 X****               MoveL     *BLANK        T1TIT
234300160204 "   X**** SOTTITD2      IFNE      *BLANK
234400160204 "   X****               MoveL     SOTTITD2      IDMSG
234500160204 "   X****               CALLB     'XRTVM'
234600160204 "   X****               PARM                    IDMSG            27
234700160204 "   X****               PARM                    TXTMSG          100
234800160204 "   X****               PARM                    ERRMSG            1
234900160204 "   X****               PARM      *On           CTRMSG            1            centratura
235000160204 "   X****               PARM      30            LENMSG            3 0          lun output.
235100160204 "   X**** ERRMSG        IFNE      *ON
235200160204 "   X****               MoveL     TXTMSG        T1TIT
235300160204 "   X****               ElSe
235400160204 "   X****               MoveL     *ALL'?'       T1TIT
235500160204 "   X****               EndIf
235600160204R195 X****               EndIf
235700131114R132 C*
235800131114  "  C*
235900131114  "  C* recupero il titolo con il tipo comunicazione
236000131114  "  C                   MOVEL     *BLANK        T1TIT
236100131114  "  C*
236200131114  "  C                   Select
236300160202R132 C* Comunicazione Black List
236301160202R195 C* Polivalente ANTE 2016
236400160202R132 C                   When      TpComG60    = 'B'
236401160202R195 C* o Black list annuale da 2016
236402160202 "   C                             or
236403160202R195 C                             TpComG60    = 'L'
236500160202R132 C*
236600131114  "  C                   if        SOTTITT3   <> *BLANK
236700131114  "  C                   MOVEL     SOTTITT3      IDMSG
236800131114  "  C                   endif
236900140328R132 C*
237000140328R133 C* Comunicazione Acquisti di San Marino
237100140328  "  C                   When      TpComG60    = 'S'
237200140328  "  C*
237300140328  "  C                   if        SOTTITT4   <> *BLANK
237400140328  "  C                   MOVEL     SOTTITT4      IDMSG
237500140328R133 C                   endif
237600140328R132 C*
237700131114  "  C                   Other
237800131114  "  C*
237900131114  "  C* Comunicazione Spesometro Clienti/Fornitori
238000131114  "  C                   if        SOTTITT1   <> *BLANK
238100131114  "  C                   MOVEL     SOTTITT1      IDMSG
238200131114  "  C                   endif
238300131114  "  C*
238400131114  "  C                   endSl
238500131114  "  C*
238600131114  "  C                   CALLB     'XRTVM'
238700131114  "  C                   PARM                    IDMSG            27
238800131114  "  C                   PARM                    TXTMSG          100
238900131114  "  C                   PARM                    ERRMSG            1
239000131114  "  C                   PARM      *ON           CTRMSG            1            centratura
239100131114  "  C                   PARM      30            LENMSG            3 0          lun output.
239200131114  "  C     ERRMSG        IFNE      *ON
239300131114  "  C                   MOVEL     TXTMSG        T1TIT
239400131114  "  C                   ELSE
239500131114  "  C                   MOVEL     *ALL'?'       T1TIT
239600131119R132 C                   ENDIF
239700000000     C*
239800000000     C* Reperisco la libreria in cui vengono memorizzati i file
239900000000     C                   Exsr      RepLibFile
240000000000     C*
240100000000     C* Inizializzazione/Decodifica prima videata
240200000000     C*
240300000000     C                   Exsr      InzD1
240400000000     C                   Move      *OFF          $InzD1
240500000000     C*
240600000000     C* reperisco DATFMT del job
240700000000     C                   CallB     'XDATFMT'
240800000000     C                   Parm                    $DatFmt           4
240900000000     C*
241000000000     C                   EndSr
241100000000     C************************************************************
241200000000     C* REPERIMENTO DATI SOCIETA'
241300000000     C************************************************************
241400000000     C     REPSOC        BegSr
241500000000     C*
241600000000     C                   CALL      'SDGSOCDR'
241700000000     C                   PARM                    TIPXSC            6
241800000000     C                   PARM                    SOCXSC            3
241900000000     C                   PARM                    CDSXSC            9 0
242000000000     C                   PARM                    MODXSC            3
242100000000     C                   PARM      *BLANKS       RTNXSC            1
242200000000     C                   PARM                    XSOCDS
242300000000     C                   PARM                    KPJBA
242400000000     C*
242500000000     C                   EndSr
242501160613R224 C******************************************************
242502160613 "   C* Reperimento della natura giuridica
242503160613 "   C******************************************************
242504160613R224 C     RepNatGiu     BegSr
242505160613     C*
242506160613     C                   Reset                   SDGTABDS
242507160613     C                   Reset                   ANGG16DS
242508160613     C*
242509160613     C                   MOVE      '1'           XTBRIC
242510160613     C                   MOVE      SocietaG60    XTBAZI
242511160613     C                   MOVE      *ZERO         XTBKEY
242512160613     C                   MOVE      'G16'         XTBCOD
242513160613     C                   MOVE      XScNCR        XTBKEY
242514160613     C                   CALL      'SDGTABDR'
242515160613     C                   PARM                    SDGTABDS
242516160613     C                   if        XTBERR      = *Off
242517160613     C                   MoveL     XTBUNI        ANGG16DS
242518160613     C                   else
242519160613     C                   Return
242520160613     C                   endif
242521160613     C*
242522160613R224 C                   ENDSR
242600000000     C************************************************************
242700000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
242800000000     C************************************************************
242900000000     C     REPDFT        BegSr
243000000000     C*
243100000000     C* preparo i parametri di chiamata al driver
243200000000     C                   Clear                   XMPEDS
243300000000     C                   EVAL      TpChiamata = '01'
243400000000     C                   EVAL      NChiavi = 5
243500000000     C                   EVAL      VLogica = 2
243600000000     C*
243700000000     C* preparo le chiavi per la chiamata al driver
243800000000     C                   Clear                   XMPEDS1
243900000000     C                   MoveL     XSCSOC        DSMPESOC
244000000000     C                   MoveL     DSPGM         DSMPEPGM
244001160202R195 C*
244100131114R132 C                   Select
244102160202R195 C* Black list Polivalente ANTE 2016
244200160202R132 C                   When      TpComG60    = 'B'
244201160202R195 C                             or
244202160202 "   C* o Black list annuale da 2016
244203160202R195 C                             TpComG60    = 'L'
244300140328R132 C                   Move      'B'           DSMPEPGM
244301160202R195 C*
244302160202R195 C* Acquisti da RSM
244400140328R133 C                   When      TpComG60    = 'S'
244500140328R133 C                   Move      'S'           DSMPEPGM
244501160202R195 C*
244600140422R133 C                   Other
244602160202R195 C* Spesometro
244700140422R133 C                   Move      'R'           DSMPEPGM
244701160202R195 C*
244800131114R132 C                   endSl
244801160202R195 C*
244900000000     C                   MoveL     KNMUS         DSMPEPRF
245000000000     C* Si reperiscono i dati di default a seconda delle modalita con
245100000000     C* cui il pgm e usato
245200000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
245300000000     C* MPETIP = 'PV'  pgm richiamato da menu
245400000000     C* MPETIP = 'PB'  x lancio Batch
245500000000     C                   MoveL     'PB'          DSMPETIP
245600000000     C*
245700000000     C                   CALLB     'XMPE'
245800000000     C                   PARM                    XMPEDS
245900000000     C                   PARM                    XMPEDS1
246000000000     C*
246100000000     C     Risultato     IfEq      '0'
246200000000     C                   MoveL     DSMPEPAR      DSMPE
246300000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
246400000000     C* i suoi valori di default vengono decisi a pgm
246500000000     C*
246600000000     C                   If        %SubSt(DsMPE: 6: 2)  = *Blank
246700000000     C                   Eval      %SubSt(DsMPE: 6: 2)  = '00'
246800000000     C                   EndIf
246900000000     C*
247000000000     C                   If        %SubSt(DsMPE: 8: 5)  = *Blank
247100000000     C                   Eval      %SubSt(DsMPE: 8: 5)  = '00000'
247200000000     C                   EndIf
247300000000     C*
247400000000     C                   ElSe
247500000000     C                   EndIf
247600000000     C*
247700000000     C                   EndSr
247800000000     C************************************************************
247900130913     C* Reperimento dati default
248000000000     C************************************************************
248100110811     C     RepDefImp     BegSr
248200000000     C*
248300110811     C                   If        $FisInt  = *blanks
248400110811     C                   MoveL     *blanks       D2FisInt
248500110811     C                   Move      *zeros        D2CafInt
248600110811     C                   Move      *blanks       D2ImpTrs
248700110811     C                   MoveL     *blanks       D2ImpDta
248800000000     C                   EndIf
248900000000     C*
249000000000     C                   EndSr
249100000000     C************************************************************
249200000000     C* Reperimento libreria per scarico su supporto magnetico
249300000000     C************************************************************
249400000000     C     RepLibFile    BegSr
249500000000     C*
249600000000     C                   Clear                   ANP022DS
249700000000     C*
249800000000     C                   CALLB     'XPAR'
249900000000     C                   Parm                    XScSoc
250000000000     C                   Parm      'SCARSUMA'    XPaPar            8
250100000000     C                   Parm                    XPaOut         2000
250200000000     C                   Parm      *OFF          XPaErr            1
250300000000     C                   Parm      ' '           XPaRic            1
250400000000     C*
250500000000     C                   If        XPaErr      = *On
250600000000     C*
250700000000     C* Errore: fallito reperimento recupero parametro modulo &1
250800000000     C* Valorizzo le variabili del msg PRO2184
250900000000     C                   MoveL     XPaPar        P1Param
251000000000     C                   Seton                                        6399
251100000000     C*
251200000000     C                   ElSe
251300000000     C                   MoveL     XPaOut        ANP022DS
251400000000     C                   EndIf
251500000000     C*
251600000000     C                   EndSr
251700000000     C************************************************************
251800000000     C* Aggiornamento D2
251900000000     C************************************************************
252000000000     C     AggD2         BegSr
252100000000     C*
252200000000     C*
252300000000     C* preparo i parametri di chiamata al driver
252400000000     C                   Clear                   XMPEDS
252500000000     C                   EVAL      TpChiamata = '03'
252600000000     C                   EVAL      NChiavi = 5
252700000000     C                   EVAL      VLogica = 2
252800000000     C*
252900000000     C* preparo le chiavi per la chiamata al driver
253000000000     C                   Clear                   XMPEDS1
253100000000     C                   MoveL     XscSoc        DSMPESOC
253200000000     C                   MoveL     DSPGM         DSMPEPGM
253201160202R195 C*
253300140422R133 C                   Select
253301160202R195 C*
253302160202R195 C* Black list Polivalente ANTE 2016
253400140422R133 C                   When      TpComG60    = 'B'
253401160202R195 C                             or
253402160202 "   C* o Black list annuale da 2016
253403160202R195 C                             TpComG60    = 'L'
253500140422R133 C                   Move      'B'           DSMPEPGM
253501160202R195 C*
253502160202R195 C* Acquisti da RSM
253600140422R133 C                   When      TpComG60    = 'S'
253700140422R133 C                   Move      'S'           DSMPEPGM
253701160202R195 C*
253800140422R133 C                   Other
253802160202R195 C* Acquisti da RSM
253900140422     C                   Move      'R'           DSMPEPGM
253901160202R195 C*
254000140422R133 C                   endSl
254001160202R195 C*
254100000000     C                   MoveL     *blanks       DSMPEPRF
254200000000     C*
254300000000     C* Si memorizzano i dati di default a seconda delle modalita con
254400000000     C* cui il pgm e usato
254500000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
254600000000     C* MPETIP = 'PV'  pgm richiamato da menu
254700000000     C* MPETIP = 'PB'  x lancio Batch
254800000000     C*
254900000000     C                   MoveL     'PB'          DSMPETIP
255000000000     C* Dati in recupero
255100000000     C*
255200130913     C                   Clear                   DSInterm
255300130913     C                   Clear                   $ImpDta
255400130913     C                   Clear                   $DtISog
255500130913     C                   Clear                   $DtFSog
255600130913     C                   Clear                   $DtNSog
255700130913     C* Intermediario
255800110811     C                   MoveL(P)  D2FisInt      $FisInt
255900110811     C                   Move      D2ImpTrs      $ImpTrs
256000110811     C                   MoveL(P)  D2CafInt      $CafInt
256100110811     C                   MoveL     WDtImp        $ImpDta
256200130913     C* Soggetto
256300130913     C                   MoveL(P)  D2FisSog      $FisSog
256400130913     C                   Move      D2CarSog      $CarSog
256500130913     C*
256600130913     C                   MoveL     WDtISog       $DtISog
256700130913     C                   MoveL     WDtFSog       $DtFSog
256800130913     C*
256900130913     C                   MoveL     D2COGSOG      $COGSOG
257000130913     C                   MoveL     D2NOMSOG      $NOMSOG
257100130913     C                   MoveL     D2SEXSOG      $SEXSOG
257200130913     C*
257300130913     C                   MoveL     WDtNSog       $DtNSog
257400000000     C*
257500130913     C                   MoveL     D2COMSOG      $COMSOG
257600130913     C                   MoveL     D2PRVSOG      $PRVSOG
257700130913     C                   MoveL     D2RAGSOG      $RAGSOG
257800130913     C*
257900110811     C                   MoveL     DSInterm      DSMPEPAR
258000000000     C*
258100000000     C                   CALLB     'XMPE'
258200000000     C                   PARM                    XMPEDS
258300000000     C                   PARM                    XMPEDS1
258400000000     C*
258500000000     C                   EndSr
258501160609R224 C************************************************************
258502160609 "   C* Aggiornamento D4
258503160609 "   C************************************************************
258504160609R224 C     AggD4         BegSr
258505160609     C*
258506160609     C*
258507160609     C* preparo i parametri di chiamata al driver
258508160609     C                   Clear                   XMPEDS
258509160609     C                   EVAL      TpChiamata = '03'
258510160609     C                   EVAL      NChiavi = 5
258511160609     C                   EVAL      VLogica = 2
258512160609     C*
258513160609     C* preparo le chiavi per la chiamata al driver
258514160609     C                   Clear                   XMPEDS1
258515160609     C                   MoveL     XscSoc        DSMPESOC
258516160609     C                   MoveL     'SDGSG60R  '  DSMPEPGM
258518160609     C*
258519160609     C                   Select
258520160609     C* Spesometro
258521160609     C                   When      TpComG60    = 'E'
258522160609     C                   Move      'EC'          DSMPEPGM
258523160609     C*
258524160609     C* Acquisti da San Marino
258525160609     C                   When      TpComG60    = 'S'
258527160609     C                   Move      'SC'          DSMPEPGM
258528160609     C*
258529160609     C* Black List
258530160610     C                   When      TpComG60    = 'L'
258532160609     C                   Move      'BC'          DSMPEPGM
258533160609     C*
258534160609     C                   endsl
258535160609     C*
258536160609     C                   MoveL     *blanks       DSMPEPRF
258537160609     C*
258538160609     C* Si memorizzano i dati di default a seconda delle modalita con
258539160609     C* cui il pgm e usato
258540160609     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
258541160609     C* MPETIP = 'PV'  pgm richiamato da menu
258542160609     C* MPETIP = 'PB'  x lancio Batch
258543160609     C*
258544160609     C                   MoveL     'PB'          DSMPETIP
258545160609     C* Dati in recupero
258546160609     C*
258547160609     C                   Clear                   DSContrib
258548160609     C*
258549160609     C                   MoveL     D4DestCon     $DestCon
258550160609     C*
258551160609     C                   MoveL     DSContrib     DSMPEPAR
258552160609     C*
258553160609     C                   CALLB     'XMPE'
258554160609     C                   PARM                    XMPEDS
258555160609     C                   PARM                    XMPEDS1
258556160609     C*
258557160609R224 C                   EndSr
258600000000     C************************************************************
258700000000     C* Aggiornamento valori assunti default
258800000000     C************************************************************
258900000000     C     Aggmpe        BegSr
259000000000     C*
259100000000     C* preparo i parametri di chiamata al driver
259200000000     C                   Clear                   XMPEDS
259300000000     C                   EVAL      TpChiamata = '03'
259400000000     C                   EVAL      NChiavi = 5
259500000000     C                   EVAL      VLogica = 2
259600000000     C*
259700000000     C* preparo le chiavi per la chiamata al driver
259800000000     C                   Clear                   XMPEDS1
259900000000     C                   MoveL     XscSoc        DSMPESOC
260000000000     C                   MoveL     DSPGM         DSMPEPGM
260001160202R195 C*
260100131114R132 C                   Select
260101160202R195 C*
260102160202R195 C* Black list Polivalente ANTE 2016
260200160202R132 C                   When      TpComG60    = 'B'
260202160202R195 C                             or
260203160202 "   C* o Black list annuale da 2016
260204160202R195 C                             TpComG60    = 'L'
260300140328R132 C                   Move      'B'           DSMPEPGM
260301160202R195 C*
260302160202R195 C* Acqusti da San Marino
260400140328R133 C                   When      TpComG60    = 'S'
260500140328R133 C                   Move      'S'           DSMPEPGM
260501160202R195 C*
260600140422R133 C                   Other
260601160202R195 C* Spesometro
260700140422R133 C                   Move      'R'           DSMPEPGM
260701160202R195 C*
260800140328R132 C                   EndSl
260801160202R195 C*
260900000000     C                   MoveL     KNMUS         DSMPEPRF
261000000000     C*
261100000000     C* Si memorizzano i dati di default a seconda delle modalita con
261200000000     C* cui il pgm e usato
261300000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
261400000000     C* MPETIP = 'PV'  pgm richiamato da menu
261500000000     C* MPETIP = 'PB'  x lancio Batch
261600130809     C*
261700130809     C                   MoveL     'PB'          DSMPETIP
261800130809     C* Dati in recupero
261900130809     C                   Move      D1Anno        $Anno
262000130809     C                   Eval      $TipInv     = D1TipInv
262100130809     C                   Z-Add     D1MaxDim      $MaxDim
262200130809     C                   Z-Add     D1MaxRec      $MaxRec
262300130809     C                   MoveL     D1File        $File
262400131115R132 C                   Move      D1Mese        $Mese
262500131115 "   C                   Move      D1Trim        $Trim
262600131115R132 C                   Move      D1StpL        $StpL
262700130809     C*
262800130809     C                   MoveL     DSMPE         DSMPEPAR
262900130809     C*
263000130809     C                   CALLB     'XMPE'
263100130809     C                   PARM                    XMPEDS
263200000000     C                   PARM                    XMPEDS1
263300000000     C*
263400000000     C                   EndSr
263500000000     C************************************************************
263600000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
263700000000     C************************************************************
263800000000     C     DEFOPZ        BegSr
263900000000     C*
264000000000     C                   Reset                   $FC1
264100000000     C                   Reset                   $FR1
264200000000     C                   Reset                   $FC3
264300000000     C                   Reset                   $FR3
264400000000     C*
264500000000     C                   EndSr
264600000000     C************************************************************
264700000000     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
264800000000     C************************************************************
264900000000     C     DEFCAM        BegSr
265000000000     C*
265100000000     C* klist
265200000000     C*
265300000000     C* Variabili per gestione videate
265400000000     C*
265500000000     C                   Move      *BLANK        $GEST             2
265600000000     C                   Move      *BLANK        $FINE             1
265700000000     C                   Move      *BLANK        $InzD1            1
265800000000     C                   Move      *BLANK        $InzD2            1
265900000000     C                   Move      *BLANK        $INZD3            1
265901160609R224 C                   Move      *BLANK        $INZD4            1
266000000000     C                   Move      *BLANK        $LastG            2
266100000000     C                   Move      *BLANK        $LastV            2
266200000000     C                   Z-Add     0             $PIED1            3 0
266300000000     C*
266400000000     C* Variabili appoggio
266500000000     C*
266600000000     C                   Z-Add     0             CURR              2 0
266700000000     C                   Z-Add     0             CURC              2 0
266800000000     C                   Move      *ZEROS        WIN              99
266900000000     C*
267000000000     C* Indici
267100000000     C                   Z-Add     0             X                 3 0
267200000000     C                   Z-Add     0             Y                 3 0
267300000000     C*
267400000000     C                   EndSr
267500000000     C************************************************************
267600000000     C* RIEMPIMENTO TASTI FUNZIONE
267700000000     C************************************************************
267800000000     C     RIEKEY        BegSr
267900000000     C*
268000000000     C* Input :
268100000000     C* - $FX = schiera con tutti i testi disponibili
268200000000     C* - $FC = schiere con i flag di validita associati ai testi
268300000000     C* - $FR = schiere con i flag di REVERSE IMAGE
268400000000     C* - $ULK = ultimo testo caricato
268500000000     C* - $L01 = Lunghezza prima riga
268600000000     C* - $L02 = Lunghezza seconda riga
268700000000     C* Output :
268800000000     C* - $ULK
268900000000     C* - $KEY1= 1ø riga di tasti funzione
269000000000     C* - $KEY2= 2ø riga di tasti funzione
269100000000     C*
269200000000     C                   CALLB     'XKEYMSG'
269300000000     C* input
269400000000     C                   PARM                    $FX
269500000000     C                   PARM                    $FC
269600000000     C                   PARM                    $FR
269700000000     C                   PARM                    $ULK              3 0
269800000000     C* output
269900000000     C                   PARM                    $KEY1            79
270000000000     C                   PARM                    $KEY2            79
270100000000     C* input
270200000000     C                   PARM                    $L01              3 0
270300000000     C                   PARM                    $L02              3 0
270400000000     C*
270500000000     C                   PARM                    $ALLFUNCT       480
270600000000     C*
270700000000     C                   EndSr
270800000000** TASTI DI FUNZIONE  D1
270900000000PROMSG    *LIBL     KEY0001  01
271000000000PROMSG    *LIBL     KEY0002  02
271100000000PROMSG    *LIBL     KEY0003  03
271200000000PROMSG    *LIBL     KEY0004  04
271300000000PROMSG    *LIBL     KEY0005  05
271400000000PROMSG    *LIBL     KEY0006  06
271500000000PROMSG    *LIBL     KEY0007  07
271600000000PROMSG    *LIBL     KEY0008  08
271700000000PROMSG    *LIBL     KEY0009  09
271800000000PROMSG    *LIBL     KEY0010  10
271900000000PROMSG    *LIBL     KEY0011  11
272000000000PROMSG    *LIBL     KEY0012  12
272100000000PROMSG    *LIBL     KEY0013  13
272200000000PROMSG    *LIBL     KEY0014  14
272300000000PROMSG    *LIBL     KEY0015  15
272400000000PROMSG    *LIBL     KEY0016  16
272500000000PROMSG    *LIBL     KEY0017  17
272600000000PROMSG    *LIBL     KEY0018  18
272601160609SDGMSG    *LIBL     SDK0019  19        ## R224
272800130916SDGMSG    *LIBL     SDK0017  20
272900000000PROMSG    *LIBL     KEY0021  21
273000000000PROMSG    *LIBL     KEY0022  22
273100000000PROMSG    *LIBL     KEY0023  23
273200000000PROMSG    *LIBL     KEY0024  24
273300000000**  ABILITAZIONE D1 /D2
2734000000000N  01
2735000000000N  02
2736000000001N  03
2737000000001N  04
2738000000001N  05
2739000000001N  06
2740000000000N  07
2741000000000N  08
2742000000000N  09
2743000000000N  10
2744000000000N  11
2745000000001N  12
2746000000000N  13
2747000000000N  14
2748000000000N  15
2749000000000N  16
2750000000000N  17
2751000000001N  18
2751011606091N  19         ## R224
2753000000001N  20
2754000000000N  21
2755000000000N  22
2756000000000N  23   Impostare '1' se si usano le opzioni
2757000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
275800000000** TASTI DI FUNZIONE  D2
275900000000PROMSG    *LIBL     KEY0001  01
276000000000PROMSG    *LIBL     KEY0002  02
276100000000PROMSG    *LIBL     KEY0003  03
276200000000PROMSG    *LIBL     KEY0004  04
276300000000PROMSG    *LIBL     KEY0005  05
276400000000PROMSG    *LIBL     KEY0006  06
276500000000PROMSG    *LIBL     KEY0007  07
276600000000PROMSG    *LIBL     KEY0008  08
276700000000PROMSG    *LIBL     KEY0009  09
276800000000PROMSG    *LIBL     KEY0010  10
276900000000PROMSG    *LIBL     KEY0011  11
277000000000PROMSG    *LIBL     KEY0012  12
277100000000PROMSG    *LIBL     KEY0013  13
277200000000PROMSG    *LIBL     KEY0014  14
277300000000PROMSG    *LIBL     KEY0015  15
277400000000PROMSG    *LIBL     KEY0016  16
277500000000PROMSG    *LIBL     KEY0017  17
277600000000PROMSG    *LIBL     KEY0018  18
277700000000PROMSG    *LIBL     KEY0019  19
277800000000PROMSG    *LIBL     KEY0020  20
277900000000PROMSG    *LIBL     KEY0021  21
278000000000PROMSG    *LIBL     KEY0022  22
278100000000PROMSG    *LIBL     KEY0023  23
278200000000PROMSG    *LIBL     KEY0024  24
278300000000**  ABILITAZIONE D2
2784000000000N  01
2785000000000N  02
2786000000001N  03
2787001108111N  04
2788000000001N  05
2789000000001N  06
2790000000000N  07
2791000000000N  08
2792000000000N  09
2793000000000N  10
2794000000000N  11
2795000000001N  12
2796000000000N  13
2797000000000N  14
2798000000000N  15
2799000000000N  16
2800000000000N  17
2801000000000N  18
2802000000000N  19
2803000000000N  20
2804000000000N  21
2805000000000N  22
2806000000000N  23   Impostare '1' se si usano le opzioni
2807000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
280800000000** TASTI DI FUNZIONE  D3
280900000000PROMSG    *LIBL     KEY0001  01
281000000000PROMSG    *LIBL     KEY0002  02
281100000000PROMSG    *LIBL     KEY0003  03
281200000000PROMSG    *LIBL     KEY0004  04
281300000000PROMSG    *LIBL     KEY0005  05
281400000000PROMSG    *LIBL     KEY0006  06
281500000000PROMSG    *LIBL     KEY0007  07
281600000000PROMSG    *LIBL     KEY0008  08
281700000000PROMSG    *LIBL     KEY0009  09
281800000000PROMSG    *LIBL     KEY0010  10
281900000000PROMSG    *LIBL     KEY0011  11
282000000000PROMSG    *LIBL     KEY0012  12
282100000000PROMSG    *LIBL     KEY0013  13
282200000000PROMSG    *LIBL     KEY0014  14
282300000000PROMSG    *LIBL     KEY0015  15
282400000000PROMSG    *LIBL     KEY0016  16
282500000000PROMSG    *LIBL     KEY0017  17
282600000000PROMSG    *LIBL     KEY0018  18
282700000000PROMSG    *LIBL     KEY0019  19
282800000000PROMSG    *LIBL     KEY0020  20
282900000000PROMSG    *LIBL     KEY0021  21
283000000000PROMSG    *LIBL     KEY0022  22
283100000000PROMSG    *LIBL     KEY0023  23
283200000000PROMSG    *LIBL     KEY0024  24
283300000000**  ABILITAZIONE D3
2834000000000N  01
2835000000000N  02
2836000000001N  03
2837000000000N  04
2838000000000N  05
2839000000000N  06
2840000000000N  07
2841000000000N  08
2842000000000N  09
2843000000000N  10
2844000000000N  11
2845000000001N  12
2846000000000N  13
2847000000000N  14
2848000000000N  15
2849000000000N  16
2850000000000N  17
2851000000000N  18
2852000000000N  19
2853000000000N  20
2854000000000N  21
2855000000000N  22
2856000000000N  23   Impostare '1' se si usano le opzioni
2857000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
