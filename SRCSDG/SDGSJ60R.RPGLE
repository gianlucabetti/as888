000100000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(QILE)
000200000000     H*PARMS BNDDIR(PJXBND PJCBND) CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500000000      *****************************************************************
000600000000      *
000700000000      *         ------------------------------------------------
000800141218      *                Comunicazione Dichiarazione d'intento
000900130809      *                 Lancio creazione file telematico
001000000000      *         ------------------------------------------------
001100000000      *
001200000000      *****************************************************************
001300141218     FSDGSJ60D  CF   E             WORKSTN
001400141218     F                                     RENAME(SJ60T1:FMTT1)
001500141218     F                                     RENAME(SJ60D1:FMTD1)
001600141218     F                                     RENAME(SJ60Z1:FMTZ1)
001700141218     F                                     RENAME(SJ60D2:FMTD2)
001800141218     F                                     RENAME(SJ60D3:FMTD3)
001900150119     F                                     RENAME(SJ60D4:FMTD4)
002000141218     F                                     RENAME(SJ60Z3:FMTZ3)
002100141218     F                                     RENAME(SJ60Z2:FMTZ2)
002200000000     F                                     INFDS(DSFMT)
002300000000     F*------------
002400141218     FANSIF01L  IF   E           K DISK
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
003600000000     D* Video D1/D2
003700000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
003800000000     D $FX2            S             50    DIM(24) ALT($FM2)
003900000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
004000000000     D $FR2            S              1    DIM(24) ALT($FC2)
004100000000     D* Video D3 Valori assunti
004200000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
004300000000     D $FX3            S             50    DIM(24) ALT($FM3)
004400000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
004500000000     D $FR3            S              1    DIM(24) ALT($FC3)
004600000000     D*----------------------------------------------------*
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
006801160530R224 D*---------
006802160530 "   D* tabella destinazione dati aggiuntivi
006803160530 "   D SDGHC4DS      E DS                  INZ
006804160530 "   D*-------------
006805160530 "   D* Parametri per ricerca codice ANATB00F
006806160530 "   D XTABDS        E DS                  inz
006807160530 "   D*-------------
006808160613 "   D* Parametri per controllo codice ANATB00F
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
007500141218     D  $TipoPlf                      1
007600141218     D  $DIvaPre                      1
007700141218     D  $Esport                       1
007800141218     D  $CeIntra                      1
007900141218     D  $CeRsm                        1
008000141218     D  $OpeAss                       1
008100141218     D  $OpeStra                      1
008200000000     D  $File                        10A
008300141218     D  $StpL                         1A
008400000000     D*-------------
008500110811     D DSInterm        DS                  INZ
008600110811     D $FisInt                             Like(D2FisInt )
008700110811     D $ImpDta                       10A
008800141219     D $FirInt                       30A
008900130913     D $FISSOG                       16A
009000150113     D $FISDIC                       11A
009100130913     D $CARSOG                        2A
009200130913     D $COGSOG                       24A
009300130913     D $NOMSOG                       20A
009400130913     D $SEXSOG                        1A
009500130913     D $DTNSOG                       10A
009600130913     D $COMSOG                       40A
009700130913     D $PRVSOG                        2A
009800141219     D $TELEFON                      12A
009900141219     D $FIRDIC                       30A
010000141230     D*-------------
010100141230     D DSInterm2       DS                  INZ
010200141230     D $IdProd                       16A
010300141230     D $EMAIL                        69A
010400150119     D*-------------
010500150119     D DSContrib       DS                  INZ
010600150119     D $COGCon                             Like(D4CogCon)
010700150119     D $NOMCon                             Like(D4NomCon)
010800150119     D $DENCon                             Like(D4DenCon )
010900150119     D $PIVACon                            Like(D4PIvaCon)
011000150119     D $COMCon                             Like(D4ComCon)
011100150119     D $PRVCon                             Like(D4PrvCon)
011200150119     D $DTNCon                       10A
011300150119     D $SEXCon                             Like(D4SexCon)
011301160530R224 D $DestCon                            Like(D4DestCon)
011400000000     D*-------------
011500110811     D DSD2Interm      DS
011600110811     D  D2FISINT
011700110811     D  D2IMPDTA
011800141219     D  D2FIRINT
011900130913     D  D2FISSOG
012000141219     D  D2FISDIC
012100130913     D  D2CARSOG
012200130913     D  D2COGSOG
012300130913     D  D2NOMSOG
012400130913     D  D2SEXSOG
012500130913     D  D2DTNSOG
012600130913     D  D2COMSOG
012700130913     D  D2PRVSOG
012800141219     D  D2TELEFON
012900141219     D  D2FIRDIC
013000141230     D*-------------
013100141230     D DSD2Interm2     DS
013200141230     D  D2IDPROD
013300141230     D  D2EMAIL
013400110811     D*-------------
013500000000     D* Reperimento nome PGM
013600000000     D STATUS         SDS           333
013700000000     D  DSPGM            *PROC
013800000000     D  PARMS            *PARMS
013900000000     D*-------------
014000000000     D* Dati di ambiente ottenuti da SDGSOCDR
014100000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
014200000000     D*-------------
014300000000     D* DS Interna per dati di output di SDGSOCDR
014400000000     D XSOCDS          DS          1000
014500000000     D*-------------
014600000000     D* Ds per ricezione parametri
014700141218     D SDGSJ60DS     E DS
014800141218     D  ERRJ60       E                     INZ('0')
014900000000     D*-------------
015000000000     D* Passaggio Parametri al PGM di estrazione dati
015100141218     D SDGSJ61DS     E DS
015200141218     D  ERRJ61       E                     INZ('0')
015300000000     D*-------------
015400000000     D* Parametri modulo SCARSUMA
015500000000     D ANP022DS      E DS
015600000000     D*-------------
015700000000     D* Catalogo file per scarico su supporto magnetico
015800000000     D ANA122DS      E DS
015900000000     D*-------------
016000000000     D X07VALDS      E DS
016100000000     D*-------------
016200000000     D DSFMT           DS
016300000000     D  $Tasto               369    369
016400000000     D  NRG                  370    370
016500000000     D  NCL                  371    371
016600000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
016700000000     D*-------------
016800000000     D* posizione cursore
016900000000     D CURSOR          DS
017000000000     D  RIRI                   1      2B 0 INZ
017100000000     D  R$$                    2      2
017200000000     D  COCO                   3      4B 0 INZ
017300000000     D  C$$                    4      4
017400000000     D*-------------
017500000000     D* Tasti di funzione
017600000000     D F01             C                   CONST(X'31')
017700000000     D F02             C                   CONST(X'32')
017800000000     D F03             C                   CONST(X'33')
017900000000     D F04             C                   CONST(X'34')
018000000000     D F05             C                   CONST(X'35')
018100000000     D F06             C                   CONST(X'36')
018200000000     D F07             C                   CONST(X'37')
018300000000     D F08             C                   CONST(X'38')
018400000000     D F09             C                   CONST(X'39')
018500000000     D F10             C                   CONST(X'3A')
018600000000     D F11             C                   CONST(X'3B')
018700000000     D F12             C                   CONST(X'3C')
018800000000     D F13             C                   CONST(X'B1')
018900000000     D F14             C                   CONST(X'B2')
019000000000     D F15             C                   CONST(X'B3')
019100000000     D F16             C                   CONST(X'B4')
019200000000     D F17             C                   CONST(X'B5')
019300000000     D F18             C                   CONST(X'B6')
019400000000     D F19             C                   CONST(X'B7')
019500000000     D F20             C                   CONST(X'B8')
019600000000     D F21             C                   CONST(X'B9')
019700000000     D F22             C                   CONST(X'BA')
019800000000     D F23             C                   CONST(X'BB')
019900000000     D F24             C                   CONST(X'BC')
020000000000     D ENTER           C                   CONST(X'F1')
020100000000     D ROLDWN          C                   CONST(X'F4')
020200000000     D ROLLUP          C                   CONST(X'F5')
020300000000     I*-------------
020400000000     I* Sottotitolo di testata
020500000000     I* se non si hanno sottotitoli, mettere tutta la costante = ' '
020600000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
020700000000     D                                          SDC0056')
020800000000     D SOTTITD2        C                   CONST('SDGMSG    *LIBL-
020900000000     D                                          SDC0054')
021000000000     D*-----------
021100000000     D* Campi di lavoro
021200000000     D CallPgm         S              1
021300000000     D $OkExeLan       S              1
021400000000     D $AnnoAlfa       S              4
021500000000     D DataISO         S               D
021600110811     D WDtImp          S               D
021700130913     D WDtNSog         S               D
021800150119     D WDtnCon         S               D
021900000000     D $DT0            C                   CONST('0001-01-01')
022000130904     D*
022100130904     D* Il campo D1File è stato tolto dalla gestione video
022200130904     D* lo tengo in vita nel pgm ma ininfluente
022300141218     D D1File          S                   Like(FileJ60    )
022400130904     D*----------------
022500141229     D* Variabili per chiamata a X63CtrChar
022600141229     D X63Codice       S            100A
022700141229     D X63Len          S              2S 0
022800141229     D X63Tipo         S              1A
022900141229     D X63Blank        S              1A
023000141229     D X63Errore       S              1A
023100141229     D*----------------
023200130904     D* ds per nome file
023300130904     D                 DS
023400130904     D  WAnno                  3      6  0
023500130904     D  WPer                   7      8
023600000000     D  WPrgInvio              9     10  0
023700000000     D  WNumero                1     10
023800000000     D*-----------
023900000000     D* Ds contenente i valori del msg PRO1797
024000000000     D §Fil_Lib        DS
024100000000     D  §File                        10A
024200000000     D  §Libreria                    10A
024300000000     D*----------------------------------------------------*
024400000000     D $P1COS2161      s             20A
024500130904     C*----------------------------------------------------*
024600130904     C*                MAIN LINE PROGRAM
024700130904     C*----------------------------------------------------*
024800130904     C*
024900130904     C* inizializzazione variabili
025000130904     C                   Exsr      INZVAR
025100130904     C*
025200000000     C     $FINE         DOWEQ     *OFF
025300000000     C     $GEST         CASEQ     'D1'          GESD1
025400000000     C     $GEST         CASEQ     'D2'          GESD2
025500000000     C     $GEST         CASEQ     'D3'          GESD3
025600150119     C     $GEST         CASEQ     'D4'          GESD4
025700000000     C                   ENDCS
025800000000     C                   ENDDO
025900000000     C*
026000000000     C* fine programma
026100000000     C                   Exsr      EndPgm
026200000000     C************************************************************
026300000000     C* RICERCHE
026400000000     C************************************************************
026500000000     C     EndPgm        BegSr
026600000000     C*
026700000000     C*
026800000000     C* se ho confermato l'uscita proseguo
026900000000     C     $Fine         IfEq      *ON
027000000000     C*
027100000000     C                   Seton                                            LR
027200000000     C                   Return
027300000000     C*
027400000000     C                   EndIf
027500000000     C*
027600000000     C                   EndSr
027700000000     C************************************************************
027800000000     C* RICERCHE
027900000000     C************************************************************
028000000000     C     Search        BegSr
028100000000     C*
028200000000     C* determino Riga/Colonna del cursore
028300000000     C                   Move      NRG           R$$
028400000000     C                   Move      NCL           C$$
028500000000     C                   Z-Add     RIRI          CURR
028600000000     C                   Z-Add     COCO          CURC
028700000000     C*
028800000000     C* I campi che al momento dell'F4 risultano protetti non devono
028900000000     C* essere abilitati alla funzione di ricerca
029000000000     C*
029100000000     C                   Select
029200000000     C*
029300000000     C* FMTD1  - file su supporto magnetico
029400000000     C     H1NMFL        WhenEq    'D1FILE'
029500000000     C                   Reset                   ANA122DS
029600000000     C                   Move      '01'          Opz122
029700000000     C                   Move      XScSoc        Societa122
029800000000     C                   Move      XScSoc        Soc122
029900000000     C                   Clear                   KPJBU
030000000000     C                   Movel     ANA122DS      KPJBU
030100000000     C                   Call      'ANA122R'
030200000000     C                   Parm                    KPJBA
030300000000     C                   Movel     KPJBU         ANA122DS
030400000000     C*
030500130809     C                   If        File122  <>   *Blanks
030600130809     C                   MoveL     File122       D1File
030700130809     C                   endIf
030800130809     C*
030900141218     C     H1NMFL        WHENEQ    'D1TIPOPLF'
031000141218     C                   EXSR      F4_TIPOPLF
031100141218     C*
031200141218     C     H1NMFL        WHENEQ    'D1STPL'
031300141218     C                   EXSR      F4_STPL
031301160530R224 C*
031302160530 "   C* FMTD4 - Destinazione dati anagrafici aggiuntivi
031303160530 "   C     H1NMFL        WHENEQ    'D4DESTCON'
031304160530R224 C                   EXSR      F4_DestCon
031400130809     C*
031500130809     C* ricerca non abilitata in questa posizione
031600130809     C                   Other
031700130809     C                   Seton                                        9899
031800130809     C*
031900110421     C                   EndSl
032000110421     C*
032100000000     C* imposto pos. del cursore
032200000000     C                   Z-Add     CURR          H1RIGA
032300000000     C                   Z-Add     CURC          H1COLO
032400000000     C*
032500000000     C                   Move      *HIVAL        $LastV
032600000000     C*
032700000000     C                   EndSr
032800110802     C************************************************************
032900141219     C* Ricerca tipo plafond
033000110802     C************************************************************
033100141219     C     F4_TipoPlf    BEGSR
033200110802     C*
033300110802     C                   Reset                   ANA964DS
033400110802     C*
033500110802     C                   MoveL     '01'          Opz964
033600141218     C                   MoveL     'SDGSJ60S'    Trc964
033700141219     C                   MoveL     'TIPOPLFJ60'  Cam964
033800110802     C                   MoveL     *On           All964
033900110802     C*
034000110802     C                   MoveL     ANA964DS      KPJBU
034100130809     C                   CALL      'ANA964R'
034200130809     C                   PARM                    KPJBA
034300130809     C                   MoveL     KPJBU         ANA964DS
034400130809     C*
034500130809     C                   if        Ret964     <> '2'
034600141219     C                   MoveL     Val964        D1TIPOPLF
034700130809     C                   endif
034800130809     C*
034900130809     C                   ENDSR
035000141219     C************************************************************
035100141219     C* Ricerca tipo stampa
035200141219     C************************************************************
035300131114     C     F4_STPL       BEGSR
035400131114     C*
035500131114     C                   Reset                   ANA964DS
035600131114     C*
035700131114     C                   MoveL     '01'          Opz964
035800141218     C                   MoveL     'SDGSJ60S'    Trc964
035900141218     C                   MoveL     'STPLJ60  '   Cam964
036000131114     C                   MoveL     *On           All964
036100131114     C*
036200131114     C                   MoveL     ANA964DS      KPJBU
036300131114     C                   CALL      'ANA964R'
036400131114     C                   PARM                    KPJBA
036500131114     C                   MoveL     KPJBU         ANA964DS
036600131114     C*
036700131114     C                   if        Ret964     <> '2'
036800131114     C                   MoveL     Val964        D1STPL
036900131114     C                   endif
037000131114     C*
037100141219     C                   ENDSR
037101160530R224 C************************************************************
037102160530 "   C* Ricerca valori per Destinazione dati anagrafici aggiuntivi
037103160530 "   C************************************************************
037104160530R224 C     F4_DestCon    BEGSR
037105160530     C*
037106160530     C*
037107160530     C                   RESET                   XTABDS
037108160530     C*
037109160530     C                   MOVE      '01'          XTAOPZ
037110160530     C                   MOVE      XSCSOC        XTAAZI
037111160530     C                   MOVE      XSCLIN        XTALIN
037112160530     C                   Z-ADD     CURR          XTARIG
037113160530     C                   Z-ADD     CURC          XTACOL
037114160530     C                   CLEAR                   KPJBU
037115160530     C                   MOVEL     XTABDS        KPJBU
037116160530     C                   CALL      'SDGHC4R'
037117160530     C                   PARM                    KPJBA
037118160530     C                   MOVEL     KPJBU         XTABDS
037119160530     C*
037120160530     C                   if        XTaErr     <> '1'  and
037121160530     C                             XTaKey     <> *Blank
037122160530     C                   Move      XTaKey        D4DestCon
037123160530     C                   endif
037124160530     c*
037125160530R224 C                   ENDSR
037126160530R224 C/EJECT
037200110421     C************************************************************
037300110421     C* GESTIONE EMISSIONE TASTI FUNZIONE
037400110421     C************************************************************
037500110421     C     GesPie        BegSr
037600110421     C*
037700110421     C                   Select
037800110421     C*
037900110421     C     $GEST         WhenEq    'D1'
038000110421     C     $ULKD1        IfEq      0
038100110421     C* riempimento opzioni
038200000000     C                   Exsr      F24D1
038300000000     C                   ElSe
038400000000     C                   Write     FMTZ1
038500000000     C                   EndIf
038600000000     C*
038700000000     C     $GEST         WhenEq    'D2'
038800150119     C     $GEST         oreq      'D4'
038900000000     C     $ULKD2        IfEq      0
039000000000     C* riempimento opzioni
039100000000     C                   Exsr      F24D2
039200000000     C                   ElSe
039300000000     C                   Write     FMTZ2
039400000000     C                   EndIf
039500000000     c*
039600000000     C* Modifica valori assunti
039700000000     C     $GEST         WhenEq    'D3'
039800000000     C     $ULKD3        IfEq      0
039900000000     C                   Exsr      F24D3
040000000000     C                   ElSe
040100000000     C                   Write     FMTZ3
040200000000     C                   EndIf
040300000000     C                   EndSl
040400000000     C*
040500000000     C                   EndSr
040600000000     C************************************************************
040700000000     C* GESTIONE VIDEO RECORD D1
040800000000     C************************************************************
040900000000     C     GesD1         BegSr
041000000000     C* inizializzo videata
041100000000     C     $InzD1        IfEq      *ON
041200000000     C                   Exsr      InzD1
041300000000     C                   Move      *OFF          $InzD1
041400000000     C                   EndIf
041500000000     C* emissione piede videata se proveniente da altra
041600000000     C* salvataggio valore formato di provenienza
041700000000     C     $LastV        IFNE      'D1'
041800000000     C                   Write     FMTT1
041900000000     C                   Exsr      GESPIE
042000000000     C                   Move      $LastV        $LastG
042100000000     C                   Move      'D1'          $LastV
042200000000     C                   EndIf
042300000000     C*
042400000000     C* se esistono errori sulla videata
042500000000     C* emetto la Write del formato a indicatori spenti per vedere
042600000000     C* le eventuali decodifiche
042700000000     C     *IN99         IfEq      *ON
042800000000     C     $GEST         ANDEQ     'D1'
042900000000     C                   MoveA     *IN           WIN
043000000000     C                   Move      *ALL'0'       IN5098           49
043100000000     C                   MoveA     IN5098        *IN(50)
043200000000     C                   Write     FMTD1
043300000000     C                   MoveA     WIN           *IN
043400000000     C                   EndIf
043500000000     C*
043600000000     C                   EXFMT     FMTD1
043700000000     C*
043800000000     C* controllo abilitazione tasti funzione
043900000000     C                   Exsr      CTRKD1
044000000000     C*
044100000000     C     *IN99         IfEq      *OFF
044200000000     C*
044300000000     C                   Select
044400000000     C* ROLLUP
044500000000     C     $Tasto        WhenEq    ROLLUP
044600000000     C                   Move      'D2'          $GEST
044700000000     C* F2=Decodifica
044800000000     C     $Tasto        WhenEq    F02
044900000000     C                   Exsr      F02D1
045000000000     C* F3=Fine
045100000000     C     $Tasto        WhenEq    F03
045200000000     C                   Exsr      F03D1
045300000000     C* F4=Ricerca
045400000000     C     $Tasto        WhenEq    F04
045500000000     C                   Exsr      SEARCH
045600000000     C* F5=Refresh
045700000000     C     $Tasto        WhenEq    F05
045800000000     C                   Exsr      F05D1
045900000000     C* F8=Successivo
046000000000     C     $Tasto        WhenEq    F08
046100000000     C                   Exsr      F08D1
046200000000     C* F12=Ritorno
046300000000     C     $Tasto        WhenEq    F12
046400000000     C                   Exsr      F12D1
046500000000     C* F18=Valori assunti
046600000000     C     $Tasto        WhenEq    F18
046700000000     C                   Exsr      F18D1
046800150119     C* F19=Dati contribuente
046900150119     C     $Tasto        WhenEq    F19
047000150119     C                   Exsr      F19D1
047100000000     C* F20=Dati rappresentante
047200000000     C     $Tasto        WhenEq    F20
047300000000     C                   Exsr      F20D1
047400000000     C* F21=Pers. batch
047500000000     C     $Tasto        WhenEq    F21
047600000000     C                   Exsr      F21D1
047700000000     C* F24=Altri tasti
047800000000     C     $Tasto        WhenEq    F24
047900000000     C                   Exsr      F24D1
048000000000     C*
048100000000     C                   Other
048200000000     C                   Exsr      CTRD1
048300000000     C* aggiungere le chiamate ai controlli delle videate supplementari
048400000000     C     *IN99         IfEq      *OFF
048500000000     C                   Exsr      CTRD2
048600000000     C     *IN99         IfEq      *ON
048700141219     C                   MOVE      *ALL'0'       IN5098
048800141219     C                   MOVEA     IN5098        *IN(50)
048900000000     C                   Seton                                            96
049000000000     C                   EndIf
049100000000     C                   EndIf
049200000000     C*
049300000000     C     $Tasto        IfEq      F06
049400000000     C     *IN99         ANDEQ     *OFF
049500000000     C*
049600141219     C                   Eval      $OkExeLan   = *On
049700000000     C*
049800110811     C* controllo se esiste gia il file con il nome reperito
049900141219     C****               EXSR      CtrFileTel
050000000000     C*
050100000000     C*
050200000000     C                   If        $OkExeLan   = *On
050300000000     C* lancio
050400000000     C                   Exsr      ExeLan
050500000000     C                   Exsr      EndPgm
050600000000     C*
050700000000     C                   EndIf
050800000000     C*
050900000000     C                   EndIf
051000000000     C*
051100000000     C                   EndSl
051200000000     C*
051300000000     C                   EndIf
051400000000     C*
051500000000     C                   EndSr
051600000000     C************************************************************
051700000000     C* INIZIALIZZAZIONE VIDEATA DATI
051800000000     C************************************************************
051900000000     C     InzD1         BegSr
052000000000     C*
052100000000     C                   Clear                   FMTD1
052200000000     C                   Clear                   FMTD2
052300000000     C*
052400141218     C                   Eval      D1On1       = XScOn
052500141218     C                   Eval      D1Of1       = XScOff
052600141218     C                   Eval      D1On2       = XScOn
052700141218     C                   Eval      D1Of2       = XScOff
052800141218     C                   Eval      D1On3       = XScOn
052900141218     C                   Eval      D1Of3       = XScOff
053000141218     C                   Eval      D1On4       = XScOn
053100141218     C                   Eval      D1Of4       = XScOff
053200141218     C                   Eval      D1On5       = XScOn
053300141218     C                   Eval      D1Of5       = XScOff
053400141218     C                   Eval      D1On6       = XScOn
053500141218     C                   Eval      D1Of6       = XScOff
053600141218     C                   Eval      D1On7       = XScOn
053700141218     C                   Eval      D1Of7       = XScOff
053800000000     C*
053900000000     C                   Z-Add     1             $PIED1
054000000000     C*
054100110802     C* se il pgm e'stato richiamato imposto i valori ricevuti dalla DS
054200000000     C                   If        CallPgm     = '1'
054300000000     C*
054400141218     C                   Exsr      RieD1J60
054500000000     C*
054600110802     C* se il pgm NON e stato richiamato
054700000000     C                   ElSe
054800000000     C*
054900000000     C* Se vuole impostazione lancio da ultima elaborazione, li imposto da anmpe
055000000000     C     $Val          IfEq      *ON
055100000000     C* riempio la videata da parametri memorizzati
055200000000     C                   Exsr      RIED1
055300000000     C*  Aggiungere le routine di riempimento dei record succcessivi
055400000000     C                   Exsr      RIED2
055500000000     C                   ElSe
055600000000     C* Immettere i valodi di default a video
055700141218     C                   Eval      D1ComInt    = XscOff
055800141218     C     XscSoc        chain     AnSif01l
055900141218     C                   if        %found(ansif01l)
056000141218     C                   Eval      D1TipoPlf   = %subst(sifflgope:3:1)
056100141218     C                   else
056200141218     C                   Eval      D1TipoPlf   = *blank
056300141218     C                   endif
056400141218     C                   Eval      D1DIvaPre   = XscOff
056500141218     C                   Eval      D1Esport    = XscOff
056600141218     C                   Eval      D1CeIntra   = XscOff
056700141218     C                   Eval      D1CeRsm     = XscOff
056800141218     C                   Eval      D1OpeAss    = XscOff
056900141218     C                   Eval      D1OpeStra   = XscOff
057000141218     C                   Eval      D1StpL      = *Blank
057100130809     C                   EndIf
057200130809     C*
057300130809     C                   EndIf
057400130809     C*
057500130809     C* richiamo routine dei ctrl per decodificarla
057600150121     C**                 Exsr      CTRD1
057700130809     C* Aggiungere le routine di controllo dei record succcessivi
057800141230     C**                 Exsr      CTRD2
057900141230     C**                 Move      *ALL'0'       IN5098
058000141230     C**                 MoveA     IN5098        *IN(50)
058100130809     C*
058200130809     C                   EndSr
058300130809     C************************************************************
058400130809     C* Riempimento videata  D1 da richiamo pgm
058500130809     C************************************************************
058600141218     C     RieD1J60      BegSr
058700130809     C*
058800141222     C                   if        ComIntJ60   = *on
058900141222     C                   Eval      D1ComInt    = 'S'
059000141222     C                   else
059100141222     C                   Eval      D1ComInt    = 'N'
059200141222     C                   endif
059300141218     C                   Z-Add     PROTOCJ60     D1Protoc
059400141218     C                   Z-Add     PROTODJ60     D1Protod
059500141222     C                   Eval      D1TipoPlf   = TipoPlfJ60
059600141222     C                   if        DIvaPreJ60  = *on
059700141222     C                   Eval      D1DIvaPre   = 'S'
059800141222     C                   else
059900141222     C                   Eval      D1DIvaPre   = 'N'
060000141222     C                   endif
060100141222     C                   if        EsportJ60   = *on
060200141222     C                   Eval      D1Esport    = 'S'
060300141222     C                   else
060400141222     C                   Eval      D1Esport    = 'N'
060500141222     C                   endif
060600141222     C                   if        CeIntraJ60  = *on
060700141222     C                   Eval      D1CeIntra   = 'S'
060800141222     C                   else
060900141222     C                   Eval      D1CeIntra   = 'N'
061000141222     C                   endif
061100141222     C                   if        CeRsmJ60    = *on
061200141222     C                   Eval      D1CeRsm     = 'S'
061300141222     C                   else
061400141222     C                   Eval      D1CeRsm     = 'N'
061500141222     C                   endif
061600141222     C                   if        OpeAssJ60   = *on
061700141222     C                   Eval      D1OpeAss    = 'S'
061800141222     C                   else
061900141222     C                   Eval      D1OpeAss    = 'N'
062000141222     C                   endif
062100141222     C                   if        OpeStraJ60  = *on
062200141222     C                   Eval      D1OpeStra   = 'S'
062300141222     C                   else
062400141222     C                   Eval      D1OpeStra   = 'N'
062500141222     C                   endif
062600141218     C                   Eval      D1File      = FileJ60
062700141218     C                   Eval      D1StpL      = StpLJ60
062800130809     C*
062900130809     C                   EndSr
063000130809     C************************************************************
063100130809     C* RIEMPIMENTO VIDEATA  D1
063200130809     C************************************************************
063300130809     C     RIED1         BegSr
063400130809     C*
063500141230     C                   Eval      D1ComInt    = XscOff
063600130809     C* impostare i campi della videata dai parametri memorizzati su anmpe
063700141218     C                   Eval      D1TipoPlf   = $TipoPlf
063800141222     C                   if        $divapre    = *on
063900141222     C                   Eval      D1DIvaPre   = 'S'
064000141222     C                   else
064100141222     C                   Eval      D1DIvaPre   = 'N'
064200141222     C                   endif
064300141222     C                   if        $esport     = *on
064400141222     C                   Eval      D1Esport    = 'S'
064500141222     C                   else
064600141222     C                   Eval      D1Esport    = 'N'
064700141222     C                   endif
064800141222     C                   if        $ceintra    = *on
064900141222     C                   Eval      D1CeIntra   = 'S'
065000141222     C                   else
065100141222     C                   Eval      D1CeIntra   = 'N'
065200141222     C                   endif
065300141222     C                   if        $cersm      = *on
065400141222     C                   Eval      D1CeRsm     = 'S'
065500141222     C                   else
065600141222     C                   Eval      D1CeRsm     = 'N'
065700141222     C                   endif
065800141222     C                   if        $opeass     = *on
065900141222     C                   Eval      D1OpeAss    = 'S'
066000141222     C                   else
066100141222     C                   Eval      D1OpeAss    = 'N'
066200141222     C                   endif
066300141222     C                   if        $opestra    = *on
066400141222     C                   Eval      D1OpeStra   = 'S'
066500141222     C                   else
066600141222     C                   Eval      D1OpeStra   = 'N'
066700141222     C                   endif
066800141218     C*
066900000000     C                   Movel     $File         D1File
067000141218     C                   Move      $StpL         D1StpL
067100130809     C*
067200130809     C                   EndSr
067300130809     C************************************************************
067400130809     C* GESTIONE F02 VIDEO D1
067500130809     C************************************************************
067600130809     C     F02D1         BegSr
067700130809     C*
067800130809     C* per decodificare la videata richiamo la routine dei ctrl
067900130809     C* e poi disattivo gli eventuali messaggi di errore
068000130809     C                   Exsr      CTRD1
068100130809     C*
068200130809     C                   Move      *ALL'0'       IN5098
068300130809     C                   MoveA     IN5098        *IN(50)
068400130809     C*
068500130809     C                   EndSr
068600000000     C************************************************************
068700000000     C* GESTIONE F03 VIDEO D1
068800000000     C************************************************************
068900000000     C     F03D1         BegSr
069000000000     C*
069100000000     C                   Move      *ON           $FINE
069200000000     C* fine programma
069300000000     C                   Exsr      EndPgm
069400000000     C*
069500000000     C                   EndSr
069600000000     C************************************************************
069700000000     C* GESTIONE F05 VIDEO D1
069800000000     C************************************************************
069900000000     C     F05D1         BegSr
070000000000     C*
070100000000     C                   Move      *ON           $InzD1
070200000000     C*
070300000000     C                   EndSr
070400000000     C************************************************************
070500000000     C* GESTIONE F08 VIDEO D1
070600000000     C************************************************************
070700000000     C     F08D1         BegSr
070800000000     C*
070900000000     C                   Move      *ON           $FINE
071000000000     C*
071100000000     C                   EndSr
071200000000     C************************************************************
071300000000     C* GESTIONE F12 VIDEO D1
071400000000     C************************************************************
071500000000     C     F12D1         BegSr
071600000000     C*
071700000000     C                   Move      *ON           $FINE
071800000000     C*
071900000000     C                   EndSr
072000000000     C************************************************************
072100000000     C* GESTIONE F18 VIDEO D1
072200000000     C************************************************************
072300000000     C     F18D1         BegSr
072400000000     C*
072500000000     C                   Move      'D3'          $GEST
072600000000     C                   Move      *ON           $INZD3
072700000000     C*
072800000000     C                   EndSr
072900150119     C************************************************************
073000150119     C* GESTIONE F19 VIDEO D1
073100150119     C************************************************************
073200150119     C     F19D1         BegSr
073300150119     C*
073400150119     C                   Move      'D4'          $GEST
073500150119     C                   Move      *ON           $INZD4
073600150119     C*
073700150119     C                   EndSr
073800000000     C************************************************************
073900000000     C* GESTIONE F20 VIDEO D2
074000000000     C************************************************************
074100000000     C     F20D1         BegSr
074200000000     C*
074300000000     C                   Move      'D2'          $GEST
074400000000     C                   Move      *ON           $INZD2
074500000000     C*
074600000000     C                   EndSr
074700000000     C************************************************************
074800000000     C* GESTIONE F21 VIDEO D1
074900000000     C************************************************************
075000000000     C     F21D1         BegSr
075100000000     C*
075200000000     C                   Move      '    '        KCOAZ
075300000000     C                   CALL      'BCH09'
075400000000     C                   PARM                    KPJBA
075500000000     C*
075600000000     C                   EndSr
075700000000     C************************************************************
075800000000     C* GESTIONE F24 VIDEO D1
075900000000     C************************************************************
076000000000     C     F24D1         BegSr
076100000000     C*
076200000000     C                   MoveA     $FX1          $FX
076300000000     C                   MoveA     $FC1          $FC
076400000000     C                   MoveA     $FR1          $FR
076500000000     C                   Z-Add     $ULKD1        $ULK
076600000000     C                   Z-Add     79            $L01
076700000000     C                   Z-Add     79            $L02
076800000000     C                   Exsr      RIEKEY
076900000000     C                   Z-Add     $ULK          $ULKD1            3 0
077000000000     C                   MoveA     $FC           $FC1
077100000000     C                   MoveL     $KEY1         Z1KE1
077200000000     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
077300000000     C                   MoveL     $KEY2         Z1KE2
077400000000     C                   Write     FMTZ1
077500000000     C*
077600000000     C                   EndSr
077700000000     C************************************************************
077800000000     C* GESTIONE F24 VIDEO D3
077900000000     C************************************************************
078000000000     C     F24D2         BegSr
078100000000     C*
078200000000     C                   MoveA     $FX2          $FX
078300000000     C                   MoveA     $FC2          $FC
078400000000     C                   MoveA     $FR2          $FR
078500000000     C                   Z-Add     $ULKD2        $ULK
078600000000     C                   Z-Add     79            $L01
078700000000     C                   Z-Add     79            $L02
078800000000     C                   Exsr      RIEKEY
078900000000     C                   Z-Add     $ULK          $ULKD2            3 0
079000000000     C                   MoveA     $FC           $FC2
079100000000     C                   MoveL     $KEY1         Z2KE1
079200000000     C                   MoveL     $ALLFUNCT     H3ALLFUNCT
079300000000     C                   MoveL     $KEY2         Z2KE2
079400000000     C                   Write     FMTZ2
079500000000     C*
079600000000     C                   EndSr
079700000000     C************************************************************
079800000000     C* GESTIONE F24 VIDEO D3
079900000000     C************************************************************
080000000000     C     F24D3         BegSr
080100000000     C*
080200000000     C                   MoveA     $FX3          $FX
080300000000     C                   MoveA     $FC3          $FC
080400000000     C                   MoveA     $FR3          $FR
080500000000     C                   Z-Add     $ULKD3        $ULK
080600000000     C                   Z-Add     44            $L01
080700000000     C                   Z-Add     0             $L02
080800000000     C                   Exsr      RIEKEY
080900000000     C                   Z-Add     $ULK          $ULKD3            3 0
081000000000     C                   MoveA     $FC           $FC3
081100000000     C                   MoveL     $KEY1         Z3KE1
081200000000     C                   MoveL     $ALLFUNCT     H3ALLFUNCT
081300000000     C                   Write     FMTZ3
081400000000     C*
081500000000     C                   EndSr
081600000000     C************************************************************
081700000000     C* CONTROLLO VIDEATA
081800000000     C************************************************************
081900000000     C     CtrD1         BegSr
082000000000     C*
082100000000     C                   SETOFF                                       99
082200130809     C* non imposto pos. del cursore
082300130809     C                   Z-Add     0             H1RIGA
082400130809     C                   Z-Add     0             H1COLO
082500130809     C*
082600141218     C* comunicazione integrativa
082700141218     C                   Exsr      CtrComInt
082800130809     C*
082900130809     C* protocollo  telematico e documento
083000130809     C                   Exsr      CtrProtoco
083100141218     C*
083200141218     C* Quadro A - Plafond
083300141218     C*
083400141218     C* tipo plafond
083500141218     C                   Clear                   D1TipoPlfD
083600141218     C                   Exsr      CtrTipPlf
083700141218     C* operazioni che concorrono alla formazione del plafond
083800141218     C                   Exsr      CtrOpe
083900110421     C*
084000000000     C* Reperimento nome file per passaggio a agenzia
084100141219     C*****              Exsr      RepNomeF
084200000000     C*
084300000000     C* file da cui recuperare t.rec. A/B
084400141219     C*****              EXSR      CtrFile
084500000000     C*
084600110802     C* Tipo stampa
084700150121     C*****              Exsr      CtrStpl
084800110421     C*
084900000000     C                   EndSr
085000000000     C***********************************************************
085100141218     C* Controllo comunicazione integrativa
085200000000     C***********************************************************
085300141218     C     CtrComInt     BegSr
085400000000     C*
085500141218     C                   If        D1ComInt   <> XScOn     and
085600141218     C                             D1ComInt   <> XScOff
085700141218     C                   SetOn                                        5299
085800141218     C                   EndIf
085900000000     C*
086000000000     C                   EndSr
086100110421     C***********************************************************
086200141218     C* Controllo tipo plafond
086300110421     C***********************************************************
086400141218     C     CtrTipPlf     BegSr
086500110421     C*
086600141218     C                   Clear                   D1TipoPlfD
086700110421     C*
086800110421     C                   Clear                   X07ValDs
086900110421     C*
087000110421     C                   Move      '1'           X07Ric
087100141218     C                   Move      *Off          X07All
087200141218     C                   Movel     'SDGSJ60S'    X07TRc
087300141218     C                   Movel     'TIPOPLFJ60'  X07Cam
087400141218     C                   Movel     D1TipoPlf     X07VAL
087500110421     C*
087600110421     C                   Callb     'X07VALR'
087700110421     C                   Parm                    X07ValDs
087800110421     C*
087900110421     C     X07ERR        IFEQ      *Off
088000141218     C                   MOVEL     X07DES        D1TipoPlfD
088100110802     C                   ElSe
088200141218     C                   SetOn                                        5099
088300110802     C                   EndIf
088400110421     C*
088500110421     C                   EndSr
088600141218     C***********************************************************
088700141218     C* Controllo operazioni
088800141218     C***********************************************************
088900141218     C     CtrOpe        BegSr
089000141218     C*
089100141218     C* Dich. annuale IVA presentata
089200141218     C                   If        D1DIvapre  <> XScOn     and
089300141218     C                             D1DIvaPre  <> XScOff
089400141218     C                   SetOn                                        5199
089500141218     C                   EndIf
089600141218     C*
089700141218     C* esportazioni
089800141218     C                   If        D1Esport   <> XScOn     and
089900141218     C                             D1Esport   <> XScOff
090000141218     C                   SetOn                                        5499
090100141218     C                   EndIf
090200141218     C*
090300141218     C* cessioni intracomunitarie
090400141218     C                   If        D1CeIntra  <> XScOn     and
090500141218     C                             D1CeIntra  <> XScOff
090600141218     C                   SetOn                                        5599
090700141218     C                   EndIf
090800141218     C*
090900141218     C* cessioni verso San Marino
091000141218     C                   If        D1CeRsm    <> XScOn     and
091100141218     C                             D1CeRsm    <> XScOff
091200141218     C                   SetOn                                        5699
091300141218     C                   EndIf
091400141218     C*
091500141218     C* operazioni assimilate
091600141218     C                   If        D1OpeAss   <> XScOn     and
091700141218     C                             D1OpeAss   <> XScOff
091800141218     C                   SetOn                                        5799
091900141218     C                   EndIf
092000141218     C*
092100141218     C* operazioni straordinarie
092200141218     C                   If        D1OpeStra  <> XScOn     and
092300141218     C                             D1OpeStra  <> XScOff
092400141218     C                   SetOn                                        5899
092500141218     C                   EndIf
092600141218     C*
092700141219     C* Se, alla data di trasmissione della dichiarazione d?intento, la dichiarazione
092800141219     C* annuale IVA è stata già presentata, va impostata la casella Dichiarazione
092900141219     C* annuale IVA presentata e non è necessario indicare quali operazioni
093000141219     C* concorrono alla formazione del plafond
093100141218     C                   If        D1DIvaPre  = XscOn      and
093200141218     C                             (D1Esport   = XScOn      or
093300141218     C                              D1CeIntra  = XScOn      or
093400141218     C                              D1CeRsm    = XScOn      or
093500141218     C                              D1OpeAss   = XScOn)
093600141219     C                   SetOn                                        666799
093700141218     C                   EndIf
093800141218     C*
093900141219     C* Se la dichiarazione annuale IVA non è stata ancora presentata è necessario
094000141219     C* indicare quali operazioni hanno concorso alla formazione del plafond
094100141219     C* Casella operazioni straordinarie obbligatoria se non è barrata nessuna
094200141219     C* casella precedente
094300141218     C                   If        D1DIvaPre  = XscOff     and
094400141219     C                             D1OpeStra  = XScOff     and
094500141218     C                             (D1Esport   = XScOff    and
094600141218     C                              D1CeIntra  = XScOff    and
094700141218     C                              D1CeRsm    = XScOff    and
094800141218     C                              D1OpeAss   = XScOff)
094900141219     C                   SetOn                                        666799
095000141218     C                   EndIf
095100141219     C*
095200141219     C                   EndSr
095300130809     C***********************************************************
095400130809     C* Controllo protocollo
095500130809     C***********************************************************
095600130809     C     CtrProtoco    BEGSR
095700130809     C*
095800130809     C*  Telematico
095900141218     C                   If        D1ComInt   = 'S'  and
096000130809     C                             D1Protoc   = 0
096100130809     C                   SetOn                                        6199
096200130809     C                   EndIf
096300141218     C                   If        D1ComInt   = 'N'  and
096400130809     C                             D1Protoc   > 0
096500130809     C                   SetOn                                        6599
096600130904     C                   EndIf
096700130904     C*
096800130904     C*  Documento
096900141218     C                   If        D1ComInt   = 'S'  and
097000130904     C                             D1Protod   = 0
097100141219     C                   SetOn                                        6299
097200130904     C                   EndIf
097300141218     C                   If        D1ComInt   = 'N'  and
097400130904     C                             D1Protod   > 0
097500130809     C                   SetOn                                        6999
097600130809     C                   EndIf
097700130809     C*
097800130809     C                   EndSr
097900130809     C***********************************************************
098000130809     C* Controllo tipo stampa
098100130809     C***********************************************************
098200130809     C     CtrSTPL       BEGSR
098300130809     C*
098400141218     C                   Clear                   D1StpLD
098500141218     C*
098600141218     C                   Clear                   X07ValDs
098700141218     C*
098800141218     C                   Move      '1'           X07Ric
098900141218     C                   Move      *On           X07All
099000141218     C                   Movel     'SDGSJ60S'    X07TRc
099100141218     C                   Movel     'STPLJ60  '   X07Cam
099200141218     C                   Movel     D1StpL        X07VAL
099300141218     C*
099400141218     C                   Callb     'X07VALR'
099500141218     C                   Parm                    X07ValDs
099600141218     C*
099700141218     C     X07ERR        IFEQ      *Off
099800141218     C                   MOVEL     X07DES        D1StpLD
099900141218     C                   ElSe
100000141218     C                   SetOn                                        5399
100100141218     C                   EndIf
100200130809     C*
100300130809     C                   EndSr
100400000000     C************************************************************
100500000000     C* Reperimento nome file per invio agenzia delle entrate
100600000000     C************************************************************
100700000000     C     RepNomeF      BegSr
100800000000     C*
100900000000     C                   Eval      WNumero     = *All'0'
101000141218     C                   Z-Add     *Year         WAnno
101100110811     C                   Z-Add     1             WPrgInvio
101200000000     C                   Move      WNumero       $Numero
101300140401     C*
101400141218     C                   Move      'DD'          $CdScarico
101500000000     C*
101600000000     C                   CALL      'X35NOMFR'
101700000000     C                   PARM                    XScSoc
101800000000     C                   PARM      *Blank        $Unita            8
101900141218     C                   PARM                    $CdScarico        2
102000000000     C                   PARM                    $Numero           9
102100000000     C                   PARM                    $File            10
102200000000     C                   PARM                    Lib022
102300000000     C                   PARM      *Off          $Errore           1
102400000000     C*
102500000000     C                   If        $Errore    <> '0'
102600110511     C                   Seton                                        5499
102700000000     C                   EndIf
102800000000     C*
102900000000     C                   EndSr
103000000000     C************************************************************
103100000000     C* Verifico esistenza file con stesso nome
103200000000     C************************************************************
103300000000     C     CtrFileTel    BegSr
103400000000     C*
103500000000     C*  CHKOBJ     OBJ($LIBRF/&FILE) OBJTYPE(*FILE)
103600000000     C                   Eval      Cmd         =
103700000000     C                             'CHKOBJ OBJ(' +
103800000000     C                             %Trim(Lib022) + '/' +
103900000000     C                             %Trim($File)  +
104000000000     C                             ') ' +
104100000000     C                             'OBJTYPE(*FILE)'
104200000000     C*
104300000000     C                   Call      'QCMDEXC'                            21
104400000000     C                   Parm                    Cmd             120
104500000000     C                   Parm      120           Len              15 5
104600000000     C*
104700000000     C*
104800000000     C                   Eval      $OkExeLan   = *Off
104900000000     C*
105000110421     C* se il file   presente nella libreria lo segnalo
105100000000     C                   If        *In21       = *Off
105200000000     C*
105300000000     C                   EXSR      MsgErr
105400000000     C*
105500000000     C                   Select
105600000000     C*
105700000000     C* richiesto di continuare
105800000000     C                   When      MsgRtn      = '1'
105900000000     C                   Eval      $OkExeLan   = *On
106000000000     C*
106100000000     C*
106200000000     C* richiesto di non continuare
106300000000     C                   When      MsgRtn      = '2'
106400000000     C                   Eval      $OkExeLan   = *Off
106500000000     C*
106600000000     C                   endsl
106700000000     C*
106800000000     C                   else
106900000000     C* se file non presente nella libreria, eseguo lancio
107000000000     C                   Eval      $OkExeLan   = *On
107100000000     C*
107200000000     C                   endIf
107300000000     C*
107400000000     C                   EndSr
107500000000     C************************************************************
107600000000     C* Controllo file per recupero rec. "A" e "B"
107700000000     C************************************************************
107800000000     C     CtrFile       BegSr
107900000000     C*
108000000000     C                   If        D1File    <> *Blank
108100000000     C*
108200110418     C* il file deve avere nome diverso da quello che andra a generare
108300000000     C                   If        D1File     = $File
108400000000     C                   Seton                                        6299
108500000000     C                   endIf
108600000000     C*
108700000000     C* verifico se esiste, nella libreria
108800000000     C* dei file per lo scarico su supporto magnetico,
108900000000     C* il file indicato a video
109000000000     C                   EXSR      ChkFileBl
109100000000     C*
109200000000     C                   endIf
109300000000     C*
109400000000     C                   EndSr
109500000000     C************************************************************
109600000000     C* Verifico l'esistenza del file nella libreria indicata
109700000000     C* per lo scarico su supporto magnetico
109800000000     C************************************************************
109900000000     C     ChkFileBl     BegSr
110000000000     C*
110100000000     C* CHKOBJ     OBJ(Lib022/D1File) OBJTYPE(*FILE)
110200000000     C                   Eval      Cmd         =
110300000000     C                             'CHKOBJ     OBJ('        +
110400000000     C                             %Trim(Lib022) + '/' +
110500000000     C                             %Trim(D1File) +
110600000000     C                             ') OBJTYPE(*FILE)'
110700000000     C*
110800000000     C                   Call      'QCMDEXC'                            21
110900000000     C                   Parm                    Cmd             120
111000000000     C                   Parm      120           Len              15 5
111100000000     C*
111200000000     C                   If        *In21       = *On
111300000000     C*
111400110418     C* Errore: dico che non il file non e stato trovato nella libreria
111500000000     C* Valorizzo le variabili del msg PRO1797
111600000000     C                   Move      D1File        §File
111700000000     C                   Move      Lib022        §Libreria
111800000000     C                   Seton                                        5499
111900000000     C*
112000000000     C                   endIf
112100000000     C*
112200000000     C                   EndSr
112300000000     C************************************************************
112400000000     C* Messaggio errore
112500000000     C************************************************************
112600000000     C     MsgErr        BegSr
112700000000      *
112800000000     C                   Reset                   SDGMSGDS
112900000000     C*
113000000000     C* Risposte possibili
113100000000     C* "1) Continuare      2) Annulla
113200000000     C                   Eval      IDMSG       =
113300000000     C                             'PROMSG    *LIBL     COS1204'
113400000000     C                   CALLB     'XRTVM'
113500000000     C                   PARM                    IDMSG
113600000000     C                   PARM      *Blank        TXTMSG
113700000000     C                   PARM                    ERRMSG
113800000000     C     ERRMSG        IFNE      *ON
113900000000     C                   MoveL     TXTMSG        XMSGTXT
114000000000     C                   ELSE
114100000000     C                   MoveL     *ALL'?'       XMSGTXT
114200000000     C                   ENDIf
114300000000     C*
114400000000     C                   MoveL     'S'           MSGEMV
114500000000     C                   Z-Add     10            MSGRGP
114600000000     C                   MoveL     'S'           MSGRSP
114700000000     C                   MoveL     '12'          MSGTPR
114800000000      *
114900000000     C                   Movel     $File         $P1COS2161
115000000000     C                   Move      Lib022        $P1COS2161
115100000000      *
115200000000     C                   call      'SDGMSGR'
115300000000     C                   parm                    SDGMSGDS
115400000000     C                   parm                    XMsgTxt         135
115500000000     C*
115600000000     C* Modo di inizializzazione della window:
115700000000     C* ('1'->Close + Open  -->Iø volta dopo l'emissione del video princip.)
115800000000     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
115900000000     C*                       per mantenere il sottofondo del video princip.)
116000000000     C                   parm      '1'           XMsgInz           1
116100000000     C*
116200000000      * COS2161-Il file &1 esiste gia nella libreria &2'
116300000000     C                   parm      'COS2161'     XMsgCd1           7
116400000000     C                   parm                    XMsgCd2           7
116500000000     C                   parm                    XMsgCd3           7
116600000000     C                   parm                    XMsgSn1           7
116700000000     C                   parm                    XMsgCdTxT        63
116800000000     C                   parm                    MsgCdTit         14
116900000000     C                   parm                    MsgClPrtf         1
117000000000     C                   parm      $P1COS2161    VarMsg1         100
117100000000     C                   parm                    VarMsg2         100
117200000000     C                   parm                    VarMsg3         100
117300000000     C*
117400000000     C                   ENDSR
117500000000     C************************************************************
117600000000     C* Messaggio errore
117700000000     C************************************************************
117800000000     C     MsgErr3       BegSr
117900000000      *
118000000000     C                   Reset                   SDGMSGDS
118100000000     C*
118200000000     C* Risposte possibili
118300000000     C* "1) Continuare      2) Annulla
118400000000     C                   Eval      IDMSG       =
118500000000     C                             'PROMSG    *LIBL     COS1204'
118600000000     C                   CALLB     'XRTVM'
118700000000     C                   PARM                    IDMSG
118800000000     C                   PARM      *Blank        TXTMSG
118900000000     C                   PARM                    ERRMSG
119000000000     C     ERRMSG        IFNE      *ON
119100000000     C                   MoveL     TXTMSG        XMSGTXT
119200000000     C                   ElSe
119300000000     C                   MoveL     *ALL'?'       XMSGTXT
119400000000     C                   EndIf
119500000000     C*
119600000000     C                   MoveL     'S'           MSGEMV
119700000000     C                   Z-Add     10            MSGRGP
119800000000     C                   MoveL     'S'           MSGRSP
119900000000     C                   MoveL     '12'          MSGTPR
120000000000      *
120100000000     C                   call      'SDGMSGR'
120200000000     C                   parm                    SDGMSGDS
120300000000     C                   parm                    XMsgTxt         135
120400000000     C*
120500000000     C* Modo di inizializzazione della window:
120600000000     C* ('1'->Close + Open  -->Iø volta dopo l'emissione del video princip.)
120700000000     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
120800000000     C*                       per mantenere il sottofondo del video princip.)
120900000000     C                   parm      '1'           XMsgInz           1
121000000000     C*
121100000000     C* COS2670- (F1) E' stato impostato un parametro per la
121200000000     C* eazione di file da trasmettere con capacita di dati limitata.
121300000000     C*  Se il file che verra creato avra capacita (in MegaByte o in
121400000000     C* numero di soggetti) superiore a quanto indicato, verranno
121500000000     C* creati altri file con la capacita richiesta fino a esaurimento
121600000000     C* dati e che dovranno essere traspessi con piu invii.
121700000000     C                   parm      'COS2670'     XMsgCd1           7
121800000000     C                   parm                    XMsgCd2           7
121900000000     C                   parm                    XMsgCd3           7
122000000000     C                   parm                    XMsgSn1           7
122100000000     C                   parm                    XMsgCdTxT        63
122200000000     C                   parm                    MsgCdTit         14
122300000000     C                   parm                    MsgClPrtf         1
122400000000     C                   parm                    VarMsg1         100
122500000000     C                   parm                    VarMsg2         100
122600000000     C                   parm                    VarMsg3         100
122700000000     C*
122800000000     C                   EndSr
122900000000     C************************************************************
123000000000     C* GESTIONE VIDEO RECORD  D2
123100000000     C************************************************************
123200000000     C     GESD2         BegSr
123300000000     C*
123400000000     C* inizializzo videata
123500000000     C     $InzD2        IfEq      *ON
123600000000     C                   Exsr      InzD2
123700141230     C                   Exsr      InzD2_2
123800000000     C                   Move      *OFF          $InzD2
123900000000     C                   EndIf
124000000000     C*
124100000000     C     $LastV        IFNE      'D2'
124200000000     C                   Write     FMTT1
124300000000     C                   Exsr      GESPIE
124400000000     C                   Move      $LastV        $LastG
124500000000     C                   Move      'D2'          $LastV
124600000000     C                   EndIf
124700000000     C*
124800000000     C* se esistono errori sulla videata
124900000000     C* emetto la Write del formato a indicatori spenti per vedere
125000000000     C* le eventuali decodifiche
125100000000     C     *IN99         IfEq      *ON
125200000000     C     $GEST         ANDEQ     'D2'
125300000000     C                   MoveA     *IN           WIN
125400000000     C                   Move      *ALL'0'       IN5098           49
125500000000     C                   MoveA     IN5098        *IN(50)
125600000000     C                   Write     FMTD2
125700000000     C                   MoveA     WIN           *IN
125800000000     C                   EndIf
125900000000     C*
126000000000     C                   EXFMT     FMTD2
126100000000     C*
126200000000     C* controllo abilitazione tasti funzione
126300000000     C                   Exsr      CTRKD2
126400000000     C*
126500000000     C     *IN99         IfEq      *OFF
126600000000     C*
126700000000     C                   Select
126800000000     C* ROLDOWN
126900000000     C     $Tasto        WhenEq    ROLDWN
127000000000     C                   Move      'D1'          $GEST
127100000000     C* F3=Fine
127200000000     C     $Tasto        WhenEq    F03
127300000000     C                   Exsr      F03D1
127400000000     C* F4=Ricerca
127500000000     C     $Tasto        WhenEq    F04
127600000000     C                   Exsr      SEARCH
127700000000     C* F5=Refresh
127800000000     C     $Tasto        WhenEq    F05
127900000000     C                   Exsr      F05D2
128000000000     C* F8=Successivo
128100000000     C     $Tasto        WhenEq    F08
128200000000     C                   Exsr      F08D1
128300000000     C* F12=Ritorno
128400000000     C     $Tasto        WhenEq    F12
128500000000     C                   Exsr      F12D2
128600000000     C* F18=Valori assunti
128700000000     C     $Tasto        WhenEq    F18
128800000000     C                   Exsr      F18D1
128900000000     C* F21=Pers. batch
129000000000     C     $Tasto        WhenEq    F21
129100000000     C                   Exsr      F21D1
129200000000     C* F24=Altri tasti
129300000000     C     $Tasto        WhenEq    F24
129400000000     C                   Exsr      F24D1
129500000000     C*
129600000000     C                   Other
129700000000     C                   Exsr      CTRD2
129800000000     C     *IN99         IfEq      *OFF
129900000000     C***                Exsr      CTRD1
130000000000     C***  *IN99         IfEq      *ON
130100000000     C***                Seton                                            96
130200000000     C***                EndIf
130300000000     C                   EndIf
130400000000     C*
130500000000     C*
130600000000     C     $Tasto        IfEq      F06
130700000000     C     *IN99         ANDEQ     *OFF
130800000000     C*
130900000000     C                   Exsr      AggD2
131000141230     C                   Exsr      AggD2_2
131100141218     C                   Move      'D1'          $GEST
131200000000     C*
131300000000     C                   EndIf
131400000000     C*
131500000000     C*
131600000000     C                   EndSl
131700000000     C*
131800000000     C                   EndIf
131900000000     C*
132000000000     C                   EndSr
132100000000     C************************************************************
132200110811     C* REPERIMENTO dati per intermediario
132300000000     C************************************************************
132400000000     C     InzD2         BegSr
132500000000     C*
132600000000     C* preparo i parametri di chiamata al driver
132700000000     C                   Clear                   XMPEDS
132800000000     C                   EVAL      TpChiamata = '01'
132900000000     C                   EVAL      NChiavi = 5
133000000000     C                   EVAL      VLogica = 2
133100000000     C*
133200000000     C* preparo le chiavi per la chiamata al driver
133300000000     C                   Clear                   XMPEDS1
133400000000     C                   MoveL     XSCSOC        DSMPESOC
133500000000     C                   MoveL     DSPGM         DSMPEPGM
133600140422     C                   Move      'R'           DSMPEPGM
133700000000     C                   MoveL     *blanks       DSMPEPRF
133800000000     C* Si reperiscono i dati di default a seconda delle modalita con
133900000000     C* cui il pgm e usato
134000000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
134100000000     C* MPETIP = 'PV'  pgm richiamato da menu
134200000000     C* MPETIP = 'PB'  x lancio Batch
134300000000     C                   MoveL     'PB'          DSMPETIP
134400000000     C*
134500000000     C                   CALLB     'XMPE'
134600000000     C                   PARM                    XMPEDS
134700000000     C                   PARM                    XMPEDS1
134800000000     C*
134900130913     C                   Clear                   DSInterm
135000130913     C                   Clear                   $ImpDta
135100130913     C                   Clear                   $DtNSog
135200000000     C*
135300000000     C     Risultato     IfEq      '0'
135400110811     C                   MoveL     DSMPEPAR      DSInterm
135500130913     C                   If        $ImpDta       = *blanks
135600130913     C                   Move      $DT0          $ImpDta
135700130913     C                   EndIf
135800130913     C                   If        $DtNSog       = *blanks
135900130913     C                   Move      $DT0          $DtNSog
136000130913     C                   EndIf
136100000000     C*
136200141219     C* Intermediario
136300110811     C                   MoveL(P)  $FisInt       D2FisInt
136400141219     C                   MoveL(P)  $FirInt       D2FirInt
136500110811     C* Data impegno
136600110811     C                   MOVEL     $ImpDta       DataISO
136700110811     C                   If        DataISO = *LOVAL
136800110811     C                   Eval      D2ImpDta = *ZEROS
136900110811     C                   Else
137000110811     C                   Select
137100110811     C* formato mm/gg/aaaa
137200110811     C                   When      $DatFmt = '*MDY'
137300110811     C     *USA          Move      DataISO       D2ImpDta
137400110811     C* formato aa/mm/gg
137500110811     C                   When      $DatFmt = '*YMD'
137600110811     C     *ISO          Move      DataISO       D2ImpDta
137700110811     C* formato gg/mm/aaaa
137800110811     C                   When      $DatFmt = '*DMY'
137900110811     C     *eur          MOVEL     DataISO       D2ImpDta
138000110811     C                   EndSl
138100110811     C                   EndIf
138200130913     C*
138300130913     C* Soggetto
138400130913     C                   MoveL(P)  $FisSog       D2FisSog
138500141219     C                   MoveL(P)  $FisDic       D2FisDic
138600130913     C                   Move      $CarSog       D2CarSog
138700130913     C                   MoveL     $COGSOG       D2COGSOG
138800130913     C                   MoveL     $NOMSOG       D2NOMSOG
138900130913     C                   MoveL     $SEXSOG       D2SEXSOG
139000130913     C* Data nascita
139100130913     C                   MOVEL     $DtNSog       DataISO
139200130913     C                   If        DataISO = *LOVAL
139300130913     C                   Eval      D2DtNSog = *ZEROS
139400130913     C                   Else
139500130913     C                   Select
139600130913     C* formato mm/gg/aaaa
139700130913     C                   When      $DatFmt = '*MDY'
139800130913     C     *USA          Move      DataISO       D2DtNSog
139900130913     C* formato aa/mm/gg
140000130913     C                   When      $DatFmt = '*YMD'
140100130913     C     *ISO          Move      DataISO       D2DtNSog
140200130913     C* formato gg/mm/aaaa
140300130913     C                   When      $DatFmt = '*DMY'
140400130913     C     *eur          MOVEL     DataISO       D2DtNSog
140500130913     C                   EndSl
140600130913     C                   EndIf
140700110811     C*
140800130913     C                   MoveL     $COMSOG       D2COMSOG
140900130913     C                   MoveL     $PRVSOG       D2PRVSOG
141000141219     C                   MoveL     $TELEFON      D2TELEFON
141100141219     C                   MoveL     $FIRDIC       D2FIRDIC
141200130913     C*
141300110811     C                   ElSe
141400110811     C                   Exsr      RepDefImp
141500110811     C                   EndIf
141600110811     C*
141700000000     C*
141800000000     C                   EndSr
141900141230     C************************************************************
142000141230     C* REPERIMENTO dati per intermediario
142100141230     C************************************************************
142200141230     C     InzD2_2       BegSr
142300141230     C*
142400141230     C* preparo i parametri di chiamata al driver
142500141230     C                   Clear                   XMPEDS
142600141230     C                   EVAL      TpChiamata = '01'
142700141230     C                   EVAL      NChiavi = 5
142800141230     C                   EVAL      VLogica = 2
142900141230     C*
143000141230     C* preparo le chiavi per la chiamata al driver
143100141230     C                   Clear                   XMPEDS1
143200141230     C                   MoveL     XSCSOC        DSMPESOC
143300141230     C                   MoveL     DSPGM         DSMPEPGM
143400141230     C                   Move      'S'           DSMPEPGM
143500141230     C                   MoveL     *blanks       DSMPEPRF
143600141230     C* Si reperiscono i dati di default a seconda delle modalita con
143700141230     C* cui il pgm e usato
143800141230     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
143900141230     C* MPETIP = 'PV'  pgm richiamato da menu
144000141230     C* MPETIP = 'PB'  x lancio Batch
144100141230     C                   MoveL     'PB'          DSMPETIP
144200141230     C*
144300141230     C                   CALLB     'XMPE'
144400141230     C                   PARM                    XMPEDS
144500141230     C                   PARM                    XMPEDS1
144600141230     C*
144700141230     C                   Clear                   DSInterm2
144800141230     C                   Clear                   $ImpDta
144900141230     C                   Clear                   $DtNSog
145000141230     C*
145100141230     C     Risultato     IfEq      '0'
145200141230     C                   MoveL     DSMPEPAR      DSInterm2
145300141230     C*
145400141230     C* Identificativo produttore software
145500141230     C                   MoveL(P)  $IdProd       D2IdProd
145600141230     C                   MoveL     $EMAIL        D2EMAIL
145700141230     C*
145800141230     C                   EndIf
145900141230     C*
146000141230     C                   Exsr      CTRD2
146100141230     C*
146200141230     C                   EndSr
146300000000     C************************************************************
146400000000     C* GESTIONE VALORI ASSUNTI
146500000000     C************************************************************
146600000000     C     GESD3         BegSr
146700000000     C*
146800000000     C     $INZD3        IfEq      *ON
146900000000     C                   Exsr      INZD3
147000000000     C                   Move      *OFF          $INZD3
147100000000     C                   EndIf
147200000000     C*
147300000000     C     $LastV        IFNE      'D3'
147400000000     C                   Exsr      GESPIE
147500000000     C                   Move      $LastV        $LastG
147600000000     C                   Move      'D3'          $LastV
147700000000     C                   EndIf
147800000000     C*
147900000000     C* se esistono errori sulla videata
148000000000     C* emetto la Write del formato a indicatori spenti per vedere
148100000000     C* le eventuali decodifiche
148200000000     C     *IN99         IfEq      *ON
148300000000     C                   MoveA     *IN           WIN
148400000000     C                   Move      *ALL'0'       IN5098           49
148500000000     C                   MoveA     IN5098        *IN(50)
148600000000     C                   Write     FMTD3
148700000000     C                   MoveA     WIN           *IN
148800000000     C                   EndIf
148900000000     C*
149000000000     C                   EXFMT     FMTD3
149100000000     C*
149200000000     C* controllo abilitazione tasti funzione
149300000000     C                   Exsr      CTRKD3
149400000000     C*
149500000000     C     *IN99         IfEq      *OFF
149600000000     C*
149700000000     C                   Select
149800000000     C* F3=Fine
149900000000     C     $Tasto        WhenEq    F03
150000000000     C                   Exsr      F03D1
150100000000     C* F12=Ritorno
150200000000     C     $Tasto        WhenEq    F12
150300000000     C                   Exsr      F12D3
150400000000     C* F24=Altri tasti
150500000000     C     $Tasto        WhenEq    F24
150600000000     C                   Exsr      F24D3
150700000000     C*
150800000000     C                   Other
150900000000     C                   Exsr      CTRD3
151000000000     C     *IN99         IfEq      *OFF
151100000000     C                   Move      $LastG        $GEST
151200000000     C                   EndIf
151300000000     C*
151400000000     C                   EndSl
151500000000     C*
151600000000     C                   EndIf
151700000000     C*
151800000000     C                   EndSr
151900000000     C************************************************************
152000000000     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
152100000000     C************************************************************
152200000000     C     INZD3         BegSr
152300000000     C*
152400000000     C                   Clear                   FMTD3
152500000000     C     $VAL          IfEq      *ON
152600000000     C                   Move      XSCON         D3VAL
152700000000     C                   ElSe
152800000000     C                   Move      XSCOFF        D3VAL
152900000000     C                   EndIf
153000000000     C                   Move      XSCON         D3ON
153100000000     C                   Move      XSCOFF        D3OF
153200000000     C*
153300000000     C                   EndSr
153400000000     C************************************************************
153500000000     C* RIEMPIMENTO VIDEATA D2
153600000000     C************************************************************
153700000000     C     RIED2         BegSr
153800000000     C*
153900000000     C*
154000000000     C                   EndSr
154100150119     C************************************************************
154200150119     C* GESTIONE VIDEO RECORD  D4
154300150119     C************************************************************
154400150119     C     GESD4         BegSr
154500150119     C*
154600150119     C* inizializzo videata
154700150119     C     $InzD4        IfEq      *ON
154800150119     C                   Exsr      InzD4
154900150119     C                   Move      *OFF          $InzD4
155000150119     C                   EndIf
155100150119     C*
155200150119     C     $LastV        IFNE      'D4'
155300150119     C                   Write     FMTT1
155400150119     C                   Exsr      GESPIE
155500150119     C                   Move      $LastV        $LastG
155600150119     C                   Move      'D4'          $LastV
155700150119     C                   EndIf
155800150119     C*
155900150119     C* se esistono errori sulla videata
156000150119     C* emetto la Write del formato a indicatori spenti per vedere
156100150119     C* le eventuali decodifiche
156200150119     C     *IN99         IfEq      *ON
156300150119     C     $GEST         ANDEQ     'D4'
156400150119     C                   MoveA     *IN           WIN
156500150119     C                   Move      *ALL'0'       IN5098           49
156600150119     C                   MoveA     IN5098        *IN(50)
156700150119     C                   Write     FMTD4
156800150119     C                   MoveA     WIN           *IN
156900150119     C                   EndIf
157000150119     C*
157100150119     C                   EXFMT     FMTD4
157200150119     C*
157300150119     C* controllo abilitazione tasti funzione
157400150119     C                   Exsr      CTRKD2
157500150119     C*
157600150119     C     *IN99         IfEq      *OFF
157700150119     C*
157800150119     C                   Select
157900150119     C* F3=Fine
158000150119     C     $Tasto        WhenEq    F03
158100150119     C                   Exsr      F03D1
158200150119     C* F4=Ricerca
158300150119     C     $Tasto        WhenEq    F04
158400150119     C                   Exsr      SEARCH
158500150119     C* F5=Refresh
158600150119     C     $Tasto        WhenEq    F05
158700150119     C                   Exsr      F05D4
158800150119     C* F8=Successivo
158900150119     C     $Tasto        WhenEq    F08
159000150119     C                   Exsr      F08D1
159100150119     C* F12=Ritorno
159200150119     C     $Tasto        WhenEq    F12
159300150119     C                   Exsr      F12D2
159400150119     C* F24=Altri tasti
159500150119     C     $Tasto        WhenEq    F24
159600150119     C                   Exsr      F24D1
159700150119     C*
159800150119     C                   Other
159900150119     C                   Exsr      CTRD4
160000150119     C     *IN99         IfEq      *OFF
160100150119     C                   EndIf
160200150119     C*
160300150119     C*
160400150119     C     $Tasto        IfEq      F06
160500150119     C     *IN99         ANDEQ     *OFF
160600150119     C*
160700150119     C                   Exsr      AggD4
160800150119     C                   Move      'D1'          $GEST
160900150119     C*
161000150119     C                   EndIf
161100150119     C*
161200150119     C*
161300150119     C                   EndSl
161400150119     C*
161500150119     C                   EndIf
161600150119     C*
161700150119     C                   EndSr
161800150119     C************************************************************
161900150119     C* REPERIMENTO dati per intermediario
162000150119     C************************************************************
162100150119     C     InzD4         BegSr
162101160613R224 C*
162102160613 "   C* se persona fisica, mostro la Destinazione anagrafica
162103160613 "   C                   if        TpNG16     = '1'
162104160613 "   C                   Eval      *In13      = *On
162105160613 "   C                   else
162106160613 "   C                   Eval      *In13      = *Off
162107160613R224 C                   endif
162200150119     C*
162300150119     C* preparo i parametri di chiamata al driver
162400150119     C                   Clear                   XMPEDS
162500150119     C                   EVAL      TpChiamata = '01'
162600150119     C                   EVAL      NChiavi = 5
162700150119     C                   EVAL      VLogica = 2
162800150119     C*
162900150119     C* preparo le chiavi per la chiamata al driver
163000150119     C                   Clear                   XMPEDS1
163100150119     C                   MoveL     XSCSOC        DSMPESOC
163200150119     C                   MoveL     DSPGM         DSMPEPGM
163300150119     C                   Move      'C'           DSMPEPGM
163400150119     C                   MoveL     *blanks       DSMPEPRF
163500150119     C* Si reperiscono i dati di default a seconda delle modalita con
163600150119     C* cui il pgm e usato
163700150119     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
163800150119     C* MPETIP = 'PV'  pgm richiamato da menu
163900150119     C* MPETIP = 'PB'  x lancio Batch
164000150119     C                   MoveL     'PB'          DSMPETIP
164100150119     C*
164200150119     C                   CALLB     'XMPE'
164300150119     C                   PARM                    XMPEDS
164400150119     C                   PARM                    XMPEDS1
164500150119     C*
164600150119     C                   Clear                   DSContrib
164700150119     C                   Clear                   $DtnCon
164800150119     C*
164900150119     C     Risultato     IfEq      '0'
165000150119     C                   MoveL     DSMPEPAR      DSContrib
165100150119     C                   If        $DtnCon       = *blanks
165200150119     C                   Move      $DT0          $DtnCon
165300150119     C                   EndIf
165400150119     C*
165500150119     C                   MoveL     $COGCON       D4COGCON
165600150119     C                   MoveL     $NOMCON       D4NOMCON
165700150119     C                   MoveL     $DENCON       D4DENCON
165800150119     C                   MoveL     $PivaCon      D4PivaCon
165900150119     C                   MoveL     $ComCon       D4ComCon
166000150119     C                   MoveL     $PrvCon       D4PrvCon
166100150119     C* Data nascita
166200150119     C                   MOVEL     $DtnCon       DataISO
166300150119     C                   If        DataISO = *LOVAL
166400150119     C                   Eval      D4DtnCon = *ZEROS
166500150119     C                   Else
166600150119     C                   Select
166700150119     C* formato mm/gg/aaaa
166800150119     C                   When      $DatFmt = '*MDY'
166900150119     C     *USA          Move      DataISO       D4DtnCon
167000150119     C* formato aa/mm/gg
167100150119     C                   When      $DatFmt = '*YMD'
167200150119     C     *ISO          Move      DataISO       D4DtnCon
167300150119     C* formato gg/mm/aaaa
167400150119     C                   When      $DatFmt = '*DMY'
167500150119     C     *eur          MOVEL     DataISO       D4DtnCon
167600150119     C                   EndSl
167700150119     C                   EndIf
167800150119     C*
167900150119     C                   MoveL     $SexCon       D4SexCon
168000150119     C*
168001160530R224 C                   MoveL     $DestCon      D4DestCon
168002160530R224 C*
168100150119     C                   EndIf
168200150119     C*
168300150119     C*
168400150119     C                   EndSr
168500000000     C************************************************************
168600000000     C* GESTIONE F05 VIDEO D2
168700000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
168800000000     C* e il riposizionamento alla prima
168900000000     C************************************************************
169000000000     C     F05D2         BegSr
169100000000     C*
169200150119     C                   Move      *ON           $InzD2
169300150119     C                   Move      'D2'          $GEST
169400000000     C*
169500000000     C                   EndSr
169600000000     C************************************************************
169700000000     C* GESTIONE F12 VIDEO D2
169800000000     C************************************************************
169900000000     C     F12D2         BegSr
170000000000     C*
170100000000     C                   Move      'D1'          $GEST
170200000000     C*
170300000000     C                   EndSr
170400000000     C************************************************************
170500000000     C* GESTIONE F12 VIDEO D3
170600000000     C************************************************************
170700000000     C     F12D3         BegSr
170800000000     C*
170900000000     C                   Move      $LastG        $GEST
171000000000     C*
171100000000     C                   EndSr
171200150119     C************************************************************
171300150119     C* GESTIONE F05 VIDEO D4
171400150119     C************************************************************
171500150119     C     F05D4         BegSr
171600150119     C*
171700150119     C                   Move      *ON           $InzD4
171800150119     C                   Move      'D4'          $GEST
171900150119     C*
172000150119     C                   EndSr
172100000000     C************************************************************
172200000000     C* CONTROLLO VIDEATA  D2
172300000000     C************************************************************
172400000000     C     CtrD2         BegSr
172500000000     C*
172600000000     C                   SETOFF                                       99
172700000000     C* non imposto pos. del cursore
172800000000     C                   Z-Add     0             H1RIGA
172900130809     C                   Z-Add     0             H1COLO
173000141230     C*
173100141230     C                   If        D2IdProd   =  *blanks
173200141230     C                   Eval      D2IdProd    = XScCdF
173300141230     C                   EndIf
173400141230     C                   If        D2IdProd   =  *blanks
173500141230     C                   Seton                                        7899
173600141230     C                   EndIf
173700130809     C*
173800130913     C                   Exsr      CtrIntermed
173900130913     C*
174000130913     C                   Exsr      CtrSoggetto
174100141229     C*
174200141229     C* Telefono: solo caratteri numerici
174300141229     C* tra blank e '9'
174400141229     C                   if        D2Telefon <> *blank
174500141229     C                   exsr      CtrlTel
174600141229     C                   if        X63Errore = *On
174700141229     C                   seton                                        7799
174800141229     C                   endif
174900141229     C                   endif
175000110811     C*
175100110811     C                   EndSr
175200110811     C************************************************************
175300130913     C* Controllo dati intermediario
175400110811     C************************************************************
175500130913     C     CtrIntermed   BegSr
175600000000     C*
175700130913     C                   MOVEL     $DT0          WDtImp
175800130913     C     D2ImpDta      IFNE      0
175900130913     C                   Select
176000130913     C* formato mm/gg/aaaa
176100130913     C                   When      $DatFmt = '*MDY'
176200130913     C     *USA          TEST(D)                 D2ImpDta               63
176300130913     C* formato aa/mm/gg
176400130913     C                   When      $DatFmt = '*YMD'
176500130913     C     *ISO          TEST(D)                 D2ImpDta               63
176600130913     C* formato gg/mm/aaaa
176700130913     C                   When      $DatFmt = '*DMY'
176800130913     C     *eur          TEST(D)                 D2ImpDta               63
176900130913     C                   EndSl
177000130913     C* data ok
177100130913     C                   If        *in63 = *off
177200130913     C                   Select
177300130913     C* formato mm/gg/aaaa
177400130913     C                   When      $DatFmt = '*MDY'
177500130913     C     *USA          Move      D2ImpDta      DataISO
177600130913     C* formato aa/mm/gg
177700130913     C                   When      $DatFmt = '*YMD'
177800130913     C     *ISO          Move      D2ImpDta      DataISO
177900130913     C* formato gg/mm/aaaa
178000130913     C                   When      $DatFmt = '*DMY'
178100130913     C     *eur          MoveL     D2ImpDta      DataISO
178200130913     C                   EndSl
178300130913     C                   MoveL     DataISO       WDtImp
178400130913     C                   Else
178500130913     C                   Seton                                          99
178600130913     C                   EndIf
178700130913     C                   EndIf
178800130913     C*
178900150107     C**                 If        D2FisInt  =   *blanks   and
179000150107     C**                           D2FirInt  <>  *blanks
179100150107     C**                 Seton                                        7999
179200150107     C**                 EndIf
179300150107     C**                 If        D2FisInt  <>  *blanks   and
179400150107     C**                           D2FirInt   =  *blanks
179500150107     C**                 Seton                                        8099
179600150107     C**                 EndIf
179700150107     C**                 if        D2ImpDta   =  *zeros
179800150107     C**                 Seton                                        5999
179900150107     C**                 EndIf
180000150107     C                   If        (D2FisInt   =  *blanks or
180100150107     C                              D2FirInt   =  *blanks or
180200150107     C                              D2ImpDta   =  *zeros  ) and
180300150107     C                             (D2FisInt  <>  *blanks or
180400150107     C                              D2FirInt  <>  *blanks or
180500150107     C                              D2ImpDta  <>  *zeros  )
180600150107     C                   Seton                                        8599
180700150107     C                   EndIf
180800130913     C*
180900130913     C                   EndSr
181000130913     C************************************************************
181100130913     C* Controllo dati Soggetto
181200130913     C************************************************************
181300130913     C     CtrSoggetto   BegSr
181400130913     C*
181500130913     C*
181600130913     C*
181700130913     C* Dt.Nas.Soggetto
181800130913     C                   MOVEL     $DT0          WDtNSog
181900130913     C     D2DtNSog      IFNE      0
182000130913     C                   Select
182100130913     C* formato mm/gg/aaaa
182200130913     C                   When      $DatFmt = '*MDY'
182300130913     C     *USA          TEST(D)                 D2DtNSog               66
182400130913     C* formato aa/mm/gg
182500130913     C                   When      $DatFmt = '*YMD'
182600130913     C     *ISO          TEST(D)                 D2DtNSog               66
182700130913     C* formato gg/mm/aaaa
182800130913     C                   When      $DatFmt = '*DMY'
182900130913     C     *eur          TEST(D)                 D2DtNSog               66
183000130913     C                   EndSl
183100130913     C* data ok
183200130913     C                   If        *in66 = *off
183300130913     C                   Select
183400130913     C* formato mm/gg/aaaa
183500130913     C                   When      $DatFmt = '*MDY'
183600130913     C     *USA          Move      D2DtNSog      DataISO
183700130913     C* formato aa/mm/gg
183800130913     C                   When      $DatFmt = '*YMD'
183900130913     C     *ISO          Move      D2DtNSog      DataISO
184000130913     C* formato gg/mm/aaaa
184100130913     C                   When      $DatFmt = '*DMY'
184200130913     C     *eur          MoveL     D2DtNSog      DataISO
184300130913     C                   EndSl
184400130913     C                   MoveL     DataISO       WDtNSog
184500130913     C                   Else
184600130913     C                   Seton                                          99
184700130913     C                   EndIf
184800130913     C                   EndIf
184900130913     C*
185000130913     C                   If        (D2FisSog   =  *blanks or
185100130913     C                              D2CogSog   =  *blanks or
185200130913     C                              D2NomSog   =  *blanks or
185300130913     C                              D2SexSog   =  *blanks or
185400130913     C                              D2ComSog   =  *blanks or
185500130913     C                              D2PrvSog   =  *blanks) and
185600130913     C                             (D2FisSog  <>  *blanks or
185700130913     C                              D2CogSog  <>  *blanks or
185800130913     C                              D2NomSog  <>  *blanks or
185900130913     C                              D2SexSog  <>  *blanks or
186000130913     C                              D2ComSog  <>  *blanks or
186100130913     C                              D2PrvSog  <>  *blanks)
186200130913     C                   Seton                                        6999
186300130913     C                   EndIf
186400130913     C*
186500131001     C                   If         D2FisSog   <> *blanks and
186600131001     C                              D2CarSog   =  *zeros
186700131001     C                   Seton                                        7299
186800131001     C                   EndIf
186900141219     C*
187000141219     C                   If         D2FisDic   <> *blanks and
187100141219     C                              D2CarSog   <> 1       and
187200141219     C                              D2CarSog   <> 6       and
187300141219     C                              D2CarSog   <> 9
187400141219     C                   Seton                                        7399
187500141219     C                   EndIf
187600131001     C*
187700130913     C                   EndSr
187800141229      ************************************************************
187900141229      * Controllo che il numero di telefono sia numerico
188000141229      ************************************************************
188100141229     c     CtrlTel       begsr
188200141229      *
188300141229     C                   eval      X63Codice = D2Telefon
188400141229     C                   eval      X63Len = %len(%trim(D2Telefon))
188500141229     C                   eval      X63Tipo = '2'
188600141229     C                   eval      X63Blank = '0'
188700141229     C                   eval      X63Errore = *Off
188800141229      *
188900141229     C                   call      'X63CTRCHAR'
189000141229     C                   parm                    X63Codice
189100141229     C                   parm                    X63Len
189200141229     C                   parm                    X63Tipo
189300141229     C                   parm                    X63Blank
189400141229     C                   parm                    X63Errore
189500141229      *
189600141229     C                   endsr
189700141229     C/EJECT
189800130913     C************************************************************
189900130913     C* CONTROLLO VIDEATA  D3
190000130913     C************************************************************
190100130913     C     CTRD3         BegSr
190200130913     C*
190300000000     C                   SETOFF                                       99
190400000000     C* non imposto pos. del cursore
190500000000     C                   Z-Add     0             H1RIGA
190600000000     C                   Z-Add     0             H1COLO
190700000000     C*
190800000000     c* Controllo VALORE
190900000000     C     D3VAL         IFNE      XSCON
191000000000     C     D3VAL         ANDNE     XSCOFF
191100000000     C                   Seton                                        5099
191200000000     C                   EndIf
191300000000     C*
191400000000     C     D3VAL         IfEq      XSCON
191500000000     C                   Move      *ON           $VAL
191600000000     C                   ElSe
191700000000     C                   Move      *OFF          $val
191800000000     c                   EndIf
191900000000     C*
192000000000     C                   EndSr
192100150119     C************************************************************
192200150119     C* CONTROLLO VIDEATA  D4
192300150119     C************************************************************
192400150119     C     CtrD4         BegSr
192500150119     C*
192600150119     C                   SETOFF                                       99
192700150119     C* non imposto pos. del cursore
192800150119     C                   Z-Add     0             H1RIGA
192900150119     C                   Z-Add     0             H1COLO
193000150119     C*
193100150119     C* Dt.Nas.Contribuente
193200150119     C                   MOVEL     $DT0          WDtNCon
193300150119     C     D4DtNCon      IFNE      0
193400150119     C                   Select
193500150119     C* formato mm/gg/aaaa
193600150119     C                   When      $DatFmt = '*MDY'
193700150119     C     *USA          TEST(D)                 D4DtNCon               66
193800150119     C* formato aa/mm/gg
193900150119     C                   When      $DatFmt = '*YMD'
194000150119     C     *ISO          TEST(D)                 D4DtNCon               66
194100150119     C* formato gg/mm/aaaa
194200150119     C                   When      $DatFmt = '*DMY'
194300150119     C     *eur          TEST(D)                 D4DtNCon               66
194400150119     C                   EndSl
194500150119     C* data ok
194600150119     C                   If        *in66 = *off
194700150119     C                   Select
194800150119     C* formato mm/gg/aaaa
194900150119     C                   When      $DatFmt = '*MDY'
195000150119     C     *USA          Move      D4DtNCon      DataISO
195100150119     C* formato aa/mm/gg
195200150119     C                   When      $DatFmt = '*YMD'
195300150119     C     *ISO          Move      D4DtNCon      DataISO
195400150119     C* formato gg/mm/aaaa
195500150119     C                   When      $DatFmt = '*DMY'
195600150119     C     *eur          MoveL     D4DtNCon      DataISO
195700150119     C                   EndSl
195800150119     C                   MoveL     DataISO       WDtNCon
195900150119     C                   Else
196000150119     C                   Seton                                          99
196100150119     C                   EndIf
196200150119     C                   EndIf
196300150119     C*
196400150128R166 C                   if        D4CogCon  <>  *blanks or
196500150128 "   C                             D4NomCon  <>  *blanks or
196600150128 "   C                             D4DenCon  <>  *blanks or
196700150128 "   C                             D4PIvaCon <>  *blanks or
196800150128 "   C                             D4SexCon  <>  *blanks or
196900150128 "   C                             D4ComCon  <>  *blanks or
197000150128 "   C                             D4PrvCon  <>  *blanks or
197100150128 "   C                             D4DtnCon  <>  0
197200150128R166 C*
197201160610R224 C* il codice destinazione anagrafica è in alternativa agli altri dati
197202160610 "   C* inseriti
197203160610 "   C                   if        D4DestCon  <> *Blank
197204160610 "   C                   Seton                                        7799
197205160610 "   C                   endif
197206160610R224 C*
197300150119     C                   if        (D4CogCon   =  *blanks and
197400150119     C                              D4NomCon   =  *blanks and
197500150119     C                              D4DenCon   =  *blanks)
197600150119     C                   Seton                                        6999
197700150119     C                   EndIf
197800150119     C*
197900150119     C                   if         D4PIvaCon  =  *blanks
198000150119     C                   Seton                                        7599
198100150119     C                   EndIf
198200150119     C*
198300150119     C                   If         (D4CogCon   <> *blanks or
198400150119     C                               D4NomCon   <> *blanks) and
198500150119     C                              D4DenCon  <>  *blanks
198600150119     C                   Seton                                        7199
198700150119     C                   EndIf
198800150119     C*
198900150119     C                   If         D4CogCon   <> *blanks  and
199000150119     C                              D4NomCon   =  *blanks
199100150119     C                   Seton                                        7399
199200150119     C                   EndIf
199300150119     C*
199400150119     C                   If         D4CogCon   =  *blanks  and
199500150119     C                              D4NomCon   <> *blanks
199600150119     C                   Seton                                        7299
199700150119     C                   EndIf
199800150119     C*
199900150119     C                   If         D4CogCon   <> *blanks or
200000150119     C                              D4NomCon   <> *blanks
200100150119     C                   if         D4SexCon   =  *blanks or
200200150119     C                              D4ComCon   =  *blanks or
200300150119     C                              D4PrvCon   =  *blanks or
200400150119     C                              D4DtnCon   =  0
200500150119     C                   Seton                                        7099
200600150119     C                   EndIf
200700150119     C                   EndIf
200800150119     C*
200900150119     C                   If         D4DenCon   <> *blank
201000150119     C                   if         D4SexCon   <> *blanks or
201100150119     C                              D4ComCon   <> *blanks or
201200150119     C                              D4PrvCon   <> *blanks or
201300150119     C                              D4DtnCon   <> 0
201400150119     C                   Seton                                        7499
201500150119     C                   EndIf
201600150119     C                   EndIf
201700150119     C*
201701160530R224 C                   else
201702160530 "   C*
201703160530 "   C* Destinazione dati aggiuntivi
201704160530 "   C                   EXSR      CtrDestCon
201705160530R224
201800150128R166 C                   EndIf
201900150128R166 C*
202000150119     C                   EndSr
202001160530R224 C************************************************************
202002160530 "   C* Controllo destinazione contribuente
202003160530 "   C************************************************************
202004160530R224 C     CtrDestCon    BEGSR
202005160530     C*
202006160530     C                   Clear                   D4DestConD
202007160530     C*
202008160530     C                   Clear                   SDGHC4DS
202009160530     C*
202010160530     C                   Clear                   SDGTABDS
202011160530     C*
202012160530     C                   if        D4DestCon  <> *Blank
202013160530     C*
202014160530     C                   Eval      XTbRic      = '1'
202015160530     C                   Eval      XTbAzi      = XScSoc
202016160530     C                   Eval      XTbKey      = *Zero
202017160530     C                   Eval      XTbCod      = 'HC4'
202018160530     C                   Move      D4DestCon     XTbKey
202019160530     C*
202020160530     C                   CALL      'SDGTABDR'
202021160530     C                   PARM                    SDGTABDS
202022160530     C*
202023160530     C                   if        XTbErr     <> '0'
202024160530     C                   SetOn                                        7699
202025160530     C*
202026160530     C                   else
202027160530     C                   MoveL     XTbUni        SDGHC4DS
202028160530     C                   MoveL     DesHC4        D4DestConD
202029160530     C*
202030160530     C                   endif
202031160530     C*
202032160530     C                   else
202033160530     C* "Generica"
202034160530     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS1529'
202035160530     C                   EXSR      PrendiMsg
202036160530     C                   if        ERRMSG     <> *ON
202037160530     C                   MoveL     TXTMSG        D4DestConD
202038160530     C                   else
202039160530     C                   MoveL     *ALL'?'       D4DestConD
202040160530     C                   endif
202041160530     C*
202042160530     C                   endif
202043160530     C*
202044160530R224 C                   ENDSR
202045160530R224 C/EJECT
202046160530R224 C************************************************************
202047160530 "   C* Reperimento testo messaggi
202048160530 "   C************************************************************
202049160530R224 C     PrendiMsg     BEGSR
202050160530     C*
202051160530     C                   CALLB     'XRTVM'
202052160530     C                   PARM                    IDMSG            27
202053160530     C                   PARM                    TXTMSG          100
202054160530     C                   PARM                    ERRMSG            1
202055160530     C*
202056160530R224 C                   ENDSR
202057160530R224 C/EJECT
202100000000     C************************************************************
202200000000     C* ESECUZIONE LANCIO
202300000000     C************************************************************
202400130809     C     ExeLan        BegSr
202500130809     C*
202600141218     C                   Reset                   SDGSJ61DS
202700130809     C*
202800141219     C                   Eval      SocietaJ61  = XscSoc
202900141222     C                   if        D1ComInt    = XscOn
203000141222     C                   eval      ComIntJ61   = *on
203100141222     C                   else
203200141222     C                   eval      ComIntJ61   = *off
203300141222     C                   endif
203400141218     C                   Eval      ProtocJ61   = D1Protoc
203500141218     C                   Eval      ProtoDJ61   = D1Protod
203600141219     C                   Eval      TipoPlfJ61  = D1TipoPlf
203700141222     C                   if        D1DIvaPre   = XscOn
203800141222     C                   Eval      DIvaPreJ61  = *on
203900141222     C                   else
204000141222     C                   Eval      DIvaPreJ61  = *off
204100141222     C                   endif
204200141222     C                   if        D1Esport    = XscOn
204300141222     C                   Eval      EsportJ61   = *on
204400141222     C                   else
204500141222     C                   Eval      EsportJ61   = *off
204600141222     C                   endif
204700141222     C                   if        D1CeIntra   = XscOn
204800141222     C                   Eval      CeIntraJ61  = *on
204900141222     C                   else
205000141222     C                   Eval      CeIntraJ61  = *off
205100141222     C                   endif
205200141222     C                   if        D1CeRsm     = XscOn
205300141222     C                   Eval      CeRsmJ61    = *on
205400141222     C                   else
205500141222     C                   Eval      CeRsmJ61    = *off
205600141222     C                   endif
205700141222     C                   if        D1OpeAss    = XscOn
205800141222     C                   Eval      OpeAssJ61   = *on
205900141222     C                   else
206000141222     C                   Eval      OpeAssJ61   = *off
206100141222     C                   endif
206200141222     C                   if        D1OpeStra   = XscOn
206300141222     C                   Eval      OpeStraJ61  = *on
206400141222     C                   else
206500141222     C                   Eval      OpeStraJ61  = *off
206600141222     C                   endif
206700141218     C                   Eval      FileTelJ61  = $File
206800141218     C                   Eval      FileJ61     = D1File
206900141218     C                   Eval      LibJ61      = Lib022
207000130809     C*
207100141218     C                   Eval      StpLJ61     = D1StpL
207200000000     C*
207300110421     C*
207400141218     C                   MoveL     SDGSJ61DS     KPJBU
207500000000     C*
207600141218     C                   CALL      'SDGSJ61C'
207700000000     C                   PARM                    KPJBA
207800000000     C*
207900141218     C                   MoveL     KPJBU         SDGSJ61DS
208000000000     C*
208100000000     C* ritorno da PGM gestione
208200141218     C                   Exsr      RTNJ61
208300000000     C*
208400000000     C* aggiorno valori assunti
208500000000     C                   Exsr      AGGMPE
208600000000     C*
208700000000     C                   EndSr
208800000000     C************************************************************
208900000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
209000000000     C************************************************************
209100141218     C     RTNJ61        BegSr
209200000000     C*
209300000000     C* funzione non eseguibile per errore :
209400000000     C*
209500000000     C                   Select
209600000000     C* 0 = funzione eseguita correttamente
209700141218     C     ERRJ61        WhenEq    '0'
209800000000     C                   Eval      $Fine       = *On
209900000000     C*
210000000000     C*
210100000000     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
210200141218     C     ERRJ61        WhenEq    '1'
210300141218     C     ERRJ61        OREQ      '2'
210400000000     C                   Seton                                        9399      emiss.SFLMSGID
210500000000     C*
210600000000     C                   EndSl
210700000000     C*
210800000000     C                   EndSr
210900000000     C************************************************************
211000000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
211100000000     C************************************************************
211200000000     C     CTRKD1        BegSr
211300000000     C*
211400000000     C                   SETOFF                                       99
211500000000     C                   MoveA     $FC1          $FC
211600000000     C                   Exsr      CTRKEY
211700000000     C     *IN99         IfEq      *ON
211800000000     C                   Seton                                        97
211900000000     C                   EndIf
212000000000     C*
212100000000     C                   EndSr
212200000000     C************************************************************
212300000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
212400000000     C************************************************************
212500000000     C     CTRKD2        BegSr
212600000000     C*
212700000000     C                   SETOFF                                       99
212800000000     C                   MoveA     $FC2          $FC
212900000000     C                   Exsr      CTRKEY
213000000000     C     *IN99         IfEq      *ON
213100000000     C                   Seton                                        97
213200000000     C                   EndIf
213300000000     C*
213400000000     C                   EndSr
213500000000     C************************************************************
213600000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
213700000000     C* INPUT
213800000000     C* $FC3 = Abilitazioni videata D3
213900000000     C* OUTPUT
214000000000     C* *IN97 *IN99 Errori
214100000000     C************************************************************
214200000000     C     CTRKD3        BegSr
214300000000     C*
214400000000     C                   SETOFF                                       99
214500000000     C                   MoveA     $FC3          $FC
214600000000     C                   Exsr      CTRKEY
214700000000     C     *IN99         IfEq      *ON
214800000000     C                   Seton                                        97
214900000000     C                   EndIf
215000000000     C*
215100000000     C                   EndSr
215200000000     C************************************************************
215300000000     C* CONTROLLO ABILITAZIONE TASTI
215400000000     C************************************************************
215500000000     C     CTRKEY        BegSr
215600000000     C*
215700000000     C                   Select
215800000000     C     $Tasto        WhenEq    F01
215900000000     C     $FC(1)        IfEq      '0'
216000000000     C                   Seton                                        99
216100000000     C                   EndIf
216200000000     C     $Tasto        WhenEq    F02
216300000000     C     $FC(2)        IfEq      '0'
216400000000     C                   Seton                                        99
216500000000     C                   EndIf
216600000000     C     $Tasto        WhenEq    F03
216700000000     C     $FC(3)        IfEq      '0'
216800000000     C                   Seton                                        99
216900000000     C                   EndIf
217000000000     C     $Tasto        WhenEq    F04
217100000000     C     $FC(4)        IfEq      '0'
217200000000     C                   Seton                                        99
217300000000     C                   EndIf
217400000000     C     $Tasto        WhenEq    F05
217500000000     C     $FC(5)        IfEq      '0'
217600000000     C                   Seton                                        99
217700000000     C                   EndIf
217800000000     C     $Tasto        WhenEq    F06
217900000000     C     $FC(6)        IfEq      '0'
218000000000     C                   Seton                                        99
218100000000     C                   EndIf
218200000000     C     $Tasto        WhenEq    F07
218300000000     C     $FC(7)        IfEq      '0'
218400000000     C                   Seton                                        99
218500000000     C                   EndIf
218600000000     C     $Tasto        WhenEq    F08
218700000000     C     $FC(8)        IfEq      '0'
218800000000     C                   Seton                                        99
218900000000     C                   EndIf
219000000000     C     $Tasto        WhenEq    F09
219100000000     C     $FC(09)       IfEq      '0'
219200000000     C                   Seton                                        99
219300000000     C                   EndIf
219400000000     C     $Tasto        WhenEq    F10
219500000000     C     $FC(10)       IfEq      '0'
219600000000     C                   Seton                                        99
219700000000     C                   EndIf
219800000000     C     $Tasto        WhenEq    F11
219900000000     C     $FC(11)       IfEq      '0'
220000000000     C                   Seton                                        99
220100000000     C                   EndIf
220200000000     C     $Tasto        WhenEq    F12
220300000000     C     $FC(12)       IfEq      '0'
220400000000     C                   Seton                                        99
220500000000     C                   EndIf
220600000000     C     $Tasto        WhenEq    F13
220700000000     C     $FC(13)       IfEq      '0'
220800000000     C                   Seton                                        99
220900000000     C                   EndIf
221000000000     C     $Tasto        WhenEq    F14
221100000000     C     $FC(14)       IfEq      '0'
221200000000     C                   Seton                                        99
221300000000     C                   EndIf
221400000000     C     $Tasto        WhenEq    F15
221500000000     C     $FC(15)       IfEq      '0'
221600000000     C                   Seton                                        99
221700000000     C                   EndIf
221800000000     C     $Tasto        WhenEq    F16
221900000000     C     $FC(16)       IfEq      '0'
222000000000     C                   Seton                                        99
222100000000     C                   EndIf
222200000000     C     $Tasto        WhenEq    F17
222300000000     C     $FC(17)       IfEq      '0'
222400000000     C                   Seton                                        99
222500000000     C                   EndIf
222600000000     C     $Tasto        WhenEq    F18
222700000000     C     $FC(18)       IfEq      '0'
222800000000     C                   Seton                                        99
222900000000     C                   EndIf
223000000000     C     $Tasto        WhenEq    F19
223100000000     C     $FC(19)       IfEq      '0'
223200000000     C                   Seton                                        99
223300000000     C                   EndIf
223400000000     C     $Tasto        WhenEq    F20
223500000000     C     $FC(20)       IfEq      '0'
223600000000     C                   Seton                                        99
223700000000     C                   EndIf
223800000000     C     $Tasto        WhenEq    F21
223900000000     C     $FC(21)       IfEq      '0'
224000000000     C                   Seton                                        99
224100000000     C                   EndIf
224200000000     C     $Tasto        WhenEq    F22
224300000000     C     $FC(22)       IfEq      '0'
224400000000     C                   Seton                                        99
224500000000     C                   EndIf
224600000000     C     $Tasto        WhenEq    F23
224700000000     C     $FC(23)       IfEq      '0'
224800000000     C                   Seton                                        99
224900000000     C                   EndIf
225000000000     C     $Tasto        WhenEq    F24
225100000000     C     $FC(24)       IfEq      '0'
225200000000     C                   Seton                                        99
225300000000     C                   EndIf
225400000000     C                   EndSl
225500000000     C*
225600000000     C                   EndSr
225700000000     C************************************************************
225800000000     C* OPERAZIONI INIZIALI
225900000000     C************************************************************
226000000000     C     *INZSR        BegSr
226100000000     C*
226200000000     C* Reperimento parametri
226300000000     C*
226400000000     C     *ENTRY        PLIST
226500000000     C                   PARM                    KPJBA
226600000000     C*
226700000000     C* Reperimento tasti di funzione
226800000000     C*
226900000000     C                   Exsr      REPKEY
227000000000     C*
227100000000     C                   EndSr
227200000000     C************************************************************
227300000000     C* REPERIMENTO TASTI DI FUNZIONE
227400000000     C************************************************************
227500000000     C     REPKEY        BegSr
227600000000     C*
227700000000     C                   MoveA     $FM1          $FM
227800000000     C                   MoveA     $Fc1          $Fc
227900000000     C                   Exsr      REPKKK
228000000000     C                   MoveA     $FX           $FX1
228100000000     C*
228200000000     C                   MoveA     $FM2          $FM
228300000000     C                   MoveA     $Fc2          $Fc
228400000000     C                   Exsr      REPKKK
228500000000     C                   MoveA     $FX           $FX2
228600000000     C*
228700000000     C                   MoveA     $FM3          $FM
228800000000     C                   MoveA     $Fc3          $Fc
228900000000     C                   Exsr      REPKKK
229000000000     C                   MoveA     $FX           $FX3
229100000000     C*
229200000000     C                   EndSr
229300000000     C************************************************************
229400000000     C* REPERIMENTO TASTI DI FUNZIONE
229500000000     C************************************************************
229600000000     C     REPKKK        BegSr
229700000000     C*
229800000000     C                   Clear                   $FX
229900000000     C     1             DO        24            X
230000000000     C*
230100000000     C     $FM(X)        IFNE      *BLANK
230200000000     C     $fc(X)        andne     '0'
230300000000     C*
230400000000     C                   MoveL     $FM(X)        IDMSG
230500000000     C                   CALLB     'XRTVM'
230600000000     C                   PARM                    IDMSG            27
230700000000     C                   PARM                    TXTMSG          100
230800000000     C                   PARM                    ERRMSG            1
230900000000     C*
231000000000     C     ERRMSG        IFNE      *ON
231100000000     C                   MoveL     TXTMSG        $FX(X)
231200000000     C                   ElSe
231300000000     C                   MoveL     *ALL' '       $FX(X)
231400000000     C                   EndIf
231500000000     C*
231600000000     C                   EndIf
231700000000     C*
231800000000     C                   ENDDO
231900000000     C*
232000000000     C                   EndSr
232100000000     C************************************************************
232200000000     C* INIZIALIZZAZIONE VARIABILI
232300000000     C************************************************************
232400000000     C     INZVAR        BegSr
232500000000     C*
232600141218     C                   MoveL     KPJBU         SDGSJ60DS
232700141218     C*
232800000000     C* se il pgm e stato richiamato imposto i valori ricevuti dalla DS
232900141218     C                   If        SocietaJ60 <> *Blanks
233000000000     C                   Eval      CallPgm     = '1'
233100000000     C                   ElSe
233200000000     C                   Eval      CallPgm     = '0'
233300000000     C                   EndIf
233400000000     C*
233500000000     C* Reperimento dati societa
233600000000     C*
233700000000     C                   MoveL     'SOC001'      TIPXSC
233800141218     C                   MoveL     SocietaJ60    SOCXSC
233900000000     C                   Exsr      REPSOC
234000000000     C     RTNXSC        IFNE      '1'
234100000000     C                   MoveL     XSOCDS        SOC001
234200000000     C                   ElSe
234300000000     C                   Return
234400000000     C                   EndIf
234401160613R224 C*
234402160613 "   C* DATI IDENTIFICATIVI DEL CONTRIBUENTE
234403160613 "   C*
234404160613 "   C* reperisco la natura giuridica del contribuente
234405160613 "   C                   EXSR      RepNatGiu
234406160613R224 C*
234500000000     C* Asteriscare se non presenti
234600000000     C*
234700000000     C* Reperimento valori default utente
234800000000     C*
234900000000     C                   Exsr      REPDFT
235000000000     C*
235100000000     C* Definizione dei tasti funzione utilizzabili a pgm
235200000000     C*
235300000000     C                   Exsr      DEFOPZ
235400000000     C*
235500000000     C* Variabili per gestione videate
235600000000     C*
235700000000     C                   Move      'D1'          $GEST
235800000000     C                   Move      *OFF          $FINE
235900000000     C                   Move      *ON           $InzD1
236000000000     C                   Move      *BLANK        $LastG
236100000000     C                   Move      *BLANK        $LastV
236200000000     C                   Z-Add     0             $ULKD1            3 0
236300000000     C                   Z-Add     0             $ULKD2            3 0
236400000000     C                   Z-Add     0             $ULKD3            3 0
236500000000     C*
236600000000     C* Variabili appoggio
236700000000     C*
236800000000     C                   Z-Add     0             CURR
236900000000     C                   Z-Add     0             CURC
237000000000     C*
237100000000     C* Valorizzazione campi univoci testate
237200000000     C*
237300000000     C                   Clear                   FMTT1
237400000000     C                   MoveL     KNSIf         NOMSIf
237500000000     C                   MoveL     XSCDSI        NOMDIT
237600000000     C                   MOVEL     KCDAZ         NOMAZN
237700000000     C                   MOVEL     DSPGM         NOMPGM           10
237800000000     C* recupero il sottotitolo (se c'e)
237900000000     C                   MoveL     *BLANK        D1TIT
238000000000     C     SOTTITD1      IFNE      *BLANK
238100000000     C                   MoveL     SOTTITD1      IDMSG
238200000000     C                   CALLB     'XRTVM'
238300000000     C                   PARM                    IDMSG            27
238400000000     C                   PARM                    TXTMSG          100
238500000000     C                   PARM                    ERRMSG            1
238600000000     C     ERRMSG        IFNE      *ON
238700000000     C                   MoveL     TXTMSG        D1TIT
238800000000     C                   ElSe
238900000000     C                   MoveL     *ALL'?'       D1TIT
239000000000     C                   EndIf
239100000000     C                   EndIf
239200000000     C*
239300000000     C                   MoveL     *BLANK        T1TIT
239400000000     C     SOTTITD2      IFNE      *BLANK
239500000000     C                   MoveL     SOTTITD2      IDMSG
239600000000     C                   CALLB     'XRTVM'
239700000000     C                   PARM                    IDMSG            27
239800000000     C                   PARM                    TXTMSG          100
239900000000     C                   PARM                    ERRMSG            1
240000000000     C                   PARM      *On           CTRMSG            1            centratura
240100000000     C                   PARM      30            LENMSG            3 0          lun output.
240200000000     C     ERRMSG        IFNE      *ON
240300000000     C                   MoveL     TXTMSG        T1TIT
240400000000     C                   ElSe
240500000000     C                   MoveL     *ALL'?'       T1TIT
240600000000     C                   EndIf
240700000000     C                   EndIf
240800141218     C*
240900141218     C* recupero il titolo
241000141218     C                   MOVEL     *BLANK        T1TIT
241100141218     C*
241200141218     C                   CALLB     'XRTVM'
241300141218     C                   PARM                    IDMSG            27
241400141218     C                   PARM                    TXTMSG          100
241500141218     C                   PARM                    ERRMSG            1
241600141218     C                   PARM      *ON           CTRMSG            1            centratura
241700141218     C                   PARM      30            LENMSG            3 0          lun output.
241800141218     C     ERRMSG        IFNE      *ON
241900141218     C                   MOVEL     TXTMSG        T1TIT
242000141218     C                   ELSE
242100141218     C                   MOVEL     *ALL'?'       T1TIT
242200141218     C                   ENDIF
242300000000     C*
242400000000     C* Reperisco la libreria in cui vengono memorizzati i file
242500000000     C                   Exsr      RepLibFile
242600141230     C*
242700141230     C* reperisco DATFMT del job
242800141230     C                   CallB     'XDATFMT'
242900141230     C                   Parm                    $DatFmt           4
243000000000     C*
243100000000     C* Inizializzazione/Decodifica prima videata
243200000000     C                   Exsr      InzD1
243300000000     C                   Move      *OFF          $InzD1
243400141230     C                   Exsr      InzD2
243500141230     C                   Exsr      InzD2_2
243600141230     C                   Move      *OFF          $InzD2
243700000000     C*
243800000000     C                   EndSr
243900000000     C************************************************************
244000000000     C* REPERIMENTO DATI SOCIETA'
244100000000     C************************************************************
244200000000     C     REPSOC        BegSr
244300000000     C*
244400000000     C                   CALL      'SDGSOCDR'
244500000000     C                   PARM                    TIPXSC            6
244600000000     C                   PARM                    SOCXSC            3
244700000000     C                   PARM                    CDSXSC            9 0
244800000000     C                   PARM                    MODXSC            3
244900000000     C                   PARM      *BLANKS       RTNXSC            1
245000000000     C                   PARM                    XSOCDS
245100000000     C                   PARM                    KPJBA
245200000000     C*
245300000000     C                   EndSr
245301160613R224 C******************************************************
245302160613 "   C* Reperimento della natura giuridica
245303160613 "   C******************************************************
245304160613R224 C     RepNatGiu     BegSr
245305160613     C*
245306160613     C                   Reset                   SDGTABDS
245307160613     C                   Reset                   ANGG16DS
245308160613     C*
245309160613     C                   MOVE      '1'           XTBRIC
245310160613     C                   MOVE      SocietaJ60    XTBAZI
245311160613     C                   MOVE      *ZERO         XTBKEY
245312160613     C                   MOVE      'G16'         XTBCOD
245313160613     C                   MOVE      XScNCR        XTBKEY
245314160613     C                   CALL      'SDGTABDR'
245315160613     C                   PARM                    SDGTABDS
245316160613     C                   if        XTBERR      = *Off
245317160613     C                   MoveL     XTBUNI        ANGG16DS
245318160613     C                   else
245319160613     C                   Return
245320160613     C                   endif
245321160613     C*
245322160613R224 C                   ENDSR
245400000000     C************************************************************
245500000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
245600000000     C************************************************************
245700000000     C     REPDFT        BegSr
245800000000     C*
245900000000     C* preparo i parametri di chiamata al driver
246000000000     C                   Clear                   XMPEDS
246100000000     C                   EVAL      TpChiamata = '01'
246200000000     C                   EVAL      NChiavi = 5
246300000000     C                   EVAL      VLogica = 2
246400000000     C*
246500000000     C* preparo le chiavi per la chiamata al driver
246600000000     C                   Clear                   XMPEDS1
246700000000     C                   MoveL     XSCSOC        DSMPESOC
246800000000     C                   MoveL     DSPGM         DSMPEPGM
246900141218     C                   Move      'R'           DSMPEPGM
247000000000     C                   MoveL     KNMUS         DSMPEPRF
247100000000     C* Si reperiscono i dati di default a seconda delle modalita con
247200000000     C* cui il pgm e usato
247300000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
247400000000     C* MPETIP = 'PV'  pgm richiamato da menu
247500000000     C* MPETIP = 'PB'  x lancio Batch
247600000000     C                   MoveL     'PB'          DSMPETIP
247700000000     C*
247800000000     C                   CALLB     'XMPE'
247900000000     C                   PARM                    XMPEDS
248000000000     C                   PARM                    XMPEDS1
248100000000     C*
248200000000     C     Risultato     IfEq      '0'
248300000000     C                   MoveL     DSMPEPAR      DSMPE
248400000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
248500000000     C* i suoi valori di default vengono decisi a pgm
248600000000     C*
248700000000     C*
248800000000     C                   ElSe
248900000000     C                   EndIf
249000000000     C*
249100000000     C                   EndSr
249200000000     C************************************************************
249300130913     C* Reperimento dati default
249400000000     C************************************************************
249500110811     C     RepDefImp     BegSr
249600000000     C*
249700110811     C                   If        $FisInt  = *blanks
249800110811     C                   MoveL     *blanks       D2FisInt
249900141230     C                   MoveL     XscCdf        D2FisInt
250000141230     C                   MoveL     XscCdf        D2IdProd
250100110811     C                   MoveL     *blanks       D2ImpDta
250200141219     C                   MoveL     *blanks       D2FirInt
250300000000     C                   EndIf
250400000000     C*
250500000000     C                   EndSr
250600000000     C************************************************************
250700000000     C* Reperimento libreria per scarico su supporto magnetico
250800000000     C************************************************************
250900000000     C     RepLibFile    BegSr
251000000000     C*
251100000000     C                   Clear                   ANP022DS
251200000000     C*
251300000000     C                   CALLB     'XPAR'
251400000000     C                   Parm                    XScSoc
251500000000     C                   Parm      'SCARSUMA'    XPaPar            8
251600000000     C                   Parm                    XPaOut         2000
251700000000     C                   Parm      *OFF          XPaErr            1
251800000000     C                   Parm      ' '           XPaRic            1
251900000000     C*
252000000000     C                   If        XPaErr      = *On
252100000000     C*
252200000000     C* Errore: fallito reperimento recupero parametro modulo &1
252300000000     C* Valorizzo le variabili del msg PRO2184
252400000000     C                   MoveL     XPaPar        P1Param
252500000000     C                   Seton                                        6399
252600000000     C*
252700000000     C                   ElSe
252800000000     C                   MoveL     XPaOut        ANP022DS
252900000000     C                   EndIf
253000000000     C*
253100000000     C                   EndSr
253200000000     C************************************************************
253300000000     C* Aggiornamento D2
253400000000     C************************************************************
253500000000     C     AggD2         BegSr
253600000000     C*
253700000000     C*
253800000000     C* preparo i parametri di chiamata al driver
253900000000     C                   Clear                   XMPEDS
254000000000     C                   EVAL      TpChiamata = '03'
254100000000     C                   EVAL      NChiavi = 5
254200000000     C                   EVAL      VLogica = 2
254300000000     C*
254400000000     C* preparo le chiavi per la chiamata al driver
254500000000     C                   Clear                   XMPEDS1
254600000000     C                   MoveL     XscSoc        DSMPESOC
254700000000     C                   MoveL     DSPGM         DSMPEPGM
254800140422     C                   Move      'R'           DSMPEPGM
254900000000     C                   MoveL     *blanks       DSMPEPRF
255000000000     C*
255100000000     C* Si memorizzano i dati di default a seconda delle modalita con
255200000000     C* cui il pgm e usato
255300000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
255400000000     C* MPETIP = 'PV'  pgm richiamato da menu
255500000000     C* MPETIP = 'PB'  x lancio Batch
255600000000     C*
255700000000     C                   MoveL     'PB'          DSMPETIP
255800000000     C* Dati in recupero
255900000000     C*
256000130913     C                   Clear                   DSInterm
256100130913     C                   Clear                   $ImpDta
256200130913     C                   Clear                   $DtNSog
256300130913     C* Intermediario
256400110811     C                   MoveL(P)  D2FisInt      $FisInt
256500110811     C                   MoveL     WDtImp        $ImpDta
256600141219     C                   MoveL(P)  D2FirInt      $FirInt
256700130913     C* Soggetto
256800130913     C                   MoveL(P)  D2FisSog      $FisSog
256900141219     C                   MoveL(P)  D2FisDic      $FisDic
257000130913     C                   Move      D2CarSog      $CarSog
257100130913     C*
257200130913     C                   MoveL     D2COGSOG      $COGSOG
257300130913     C                   MoveL     D2NOMSOG      $NOMSOG
257400130913     C                   MoveL     D2SEXSOG      $SEXSOG
257500130913     C*
257600130913     C                   MoveL     WDtNSog       $DtNSog
257700000000     C*
257800130913     C                   MoveL     D2COMSOG      $COMSOG
257900130913     C                   MoveL     D2PRVSOG      $PRVSOG
258000141219     C                   MoveL     D2TELEFON     $TELEFON
258100141219     C                   MoveL     D2FIRDIC      $FIRDIC
258200130913     C*
258300110811     C                   MoveL     DSInterm      DSMPEPAR
258400000000     C*
258500000000     C                   CALLB     'XMPE'
258600000000     C                   PARM                    XMPEDS
258700000000     C                   PARM                    XMPEDS1
258800000000     C*
258900000000     C                   EndSr
259000141230     C************************************************************
259100141230     C* Aggiornamento D2
259200141230     C************************************************************
259300141230     C     AggD2_2       BegSr
259400141230     C*
259500141230     C*
259600141230     C* preparo i parametri di chiamata al driver
259700141230     C                   Clear                   XMPEDS
259800141230     C                   EVAL      TpChiamata = '03'
259900141230     C                   EVAL      NChiavi = 5
260000141230     C                   EVAL      VLogica = 2
260100141230     C*
260200141230     C* preparo le chiavi per la chiamata al driver
260300141230     C                   Clear                   XMPEDS1
260400141230     C                   MoveL     XscSoc        DSMPESOC
260500141230     C                   MoveL     DSPGM         DSMPEPGM
260600141230     C                   Move      'S'           DSMPEPGM
260700141230     C                   MoveL     *blanks       DSMPEPRF
260800141230     C*
260900141230     C* Si memorizzano i dati di default a seconda delle modalita con
261000141230     C* cui il pgm e usato
261100141230     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
261200141230     C* MPETIP = 'PV'  pgm richiamato da menu
261300141230     C* MPETIP = 'PB'  x lancio Batch
261400141230     C*
261500141230     C                   MoveL     'PB'          DSMPETIP
261600141230     C* Dati in recupero
261700141230     C*
261800141230     C                   Clear                   DSInterm2
261900141230     C* Identificativo produttore software
262000141230     C                   MoveL(P)  D2IdProd      $IdProd
262100141230     C                   MoveL     D2EMAIL       $EMAIL
262200141230     C*
262300141230     C                   MoveL     DSInterm2     DSMPEPAR
262400141230     C*
262500141230     C                   CALLB     'XMPE'
262600141230     C                   PARM                    XMPEDS
262700141230     C                   PARM                    XMPEDS1
262800141230     C*
262900141230     C                   EndSr
263000150119     C************************************************************
263100150119     C* Aggiornamento D4
263200150119     C************************************************************
263300150119     C     AggD4         BegSr
263400150119     C*
263500150119     C*
263600150119     C* preparo i parametri di chiamata al driver
263700150119     C                   Clear                   XMPEDS
263800150119     C                   EVAL      TpChiamata = '03'
263900150119     C                   EVAL      NChiavi = 5
264000150119     C                   EVAL      VLogica = 2
264100150119     C*
264200150119     C* preparo le chiavi per la chiamata al driver
264300150119     C                   Clear                   XMPEDS1
264400150119     C                   MoveL     XscSoc        DSMPESOC
264500150119     C                   MoveL     DSPGM         DSMPEPGM
264600150119     C                   Move      'C'           DSMPEPGM
264700150119     C                   MoveL     *blanks       DSMPEPRF
264800150119     C*
264900150119     C* Si memorizzano i dati di default a seconda delle modalita con
265000150119     C* cui il pgm e usato
265100150119     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
265200150119     C* MPETIP = 'PV'  pgm richiamato da menu
265300150119     C* MPETIP = 'PB'  x lancio Batch
265400150119     C*
265500150119     C                   MoveL     'PB'          DSMPETIP
265600150119     C* Dati in recupero
265700150119     C*
265800150119     C                   Clear                   DSContrib
265900150119     C                   Clear                   $DtnCon
266000150119     C*
266100150119     C                   MoveL     D4COGCON      $COGCON
266200150119     C                   MoveL     D4NOMCON      $NOMCON
266300150119     C                   MoveL     D4DENCON      $DENCON
266400150119     C                   MoveL(P)  D4PIvaCon     $PIvaCon
266500150119     C                   MoveL     D4COMCON      $COMCON
266600150119     C                   MoveL     D4PRVCON      $PRVCON
266700150119     C                   MoveL     D4SEXCON      $SEXCON
266701160530R224 C                   MoveL     D4DestCon     $DestCon
266800150128R166 C                   if        WDtnCon <> *loval
266900150119     C                   MoveL     WDtnCon       $DtnCon
267000150128R166 C                   endif
267100150119     C*
267200150119     C                   MoveL     DSContrib     DSMPEPAR
267300150119     C*
267400150119     C                   CALLB     'XMPE'
267500150119     C                   PARM                    XMPEDS
267600150119     C                   PARM                    XMPEDS1
267700150119     C*
267800150119     C                   EndSr
267900000000     C************************************************************
268000000000     C* Aggiornamento valori assunti default
268100000000     C************************************************************
268200000000     C     Aggmpe        BegSr
268300000000     C*
268400000000     C* preparo i parametri di chiamata al driver
268500000000     C                   Clear                   XMPEDS
268600000000     C                   EVAL      TpChiamata = '03'
268700000000     C                   EVAL      NChiavi = 5
268800000000     C                   EVAL      VLogica = 2
268900000000     C*
269000000000     C* preparo le chiavi per la chiamata al driver
269100000000     C                   Clear                   XMPEDS1
269200000000     C                   MoveL     XscSoc        DSMPESOC
269300000000     C                   MoveL     DSPGM         DSMPEPGM
269400141218     C                   Move      'R'           DSMPEPGM
269500000000     C                   MoveL     KNMUS         DSMPEPRF
269600000000     C*
269700000000     C* Si memorizzano i dati di default a seconda delle modalita con
269800000000     C* cui il pgm e usato
269900000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
270000000000     C* MPETIP = 'PV'  pgm richiamato da menu
270100000000     C* MPETIP = 'PB'  x lancio Batch
270200130809     C*
270300130809     C                   MoveL     'PB'          DSMPETIP
270400130809     C* Dati in recupero
270500141218     C                   Eval      $TipoPlf    = D1TipoPlf
270600141222     C                   if        D1divapre   = 'S'
270700141222     C                   Eval      $DIvaPre    = *on
270800141222     C                   else
270900141222     C                   Eval      $DIvaPre    = *off
271000141222     C                   endif
271100141222     C                   if        D1esport    = 'S'
271200141222     C                   Eval      $Esport     = *on
271300141222     C                   else
271400141222     C                   Eval      $Esport     = *off
271500141222     C                   endif
271600141222     C                   if        D1ceintra   = 'S'
271700141222     C                   Eval      $CeIntra    = *on
271800141222     C                   else
271900141222     C                   Eval      $CeIntra    = *off
272000141222     C                   endif
272100141222     C                   if        D1cersm     = 'S'
272200141222     C                   Eval      $CeRsm      = *on
272300141222     C                   else
272400141222     C                   Eval      $CeRsm      = *off
272500141222     C                   endif
272600141222     C                   if        D1opeass    = 'S'
272700141222     C                   Eval      $OpeAss     = *on
272800141222     C                   else
272900141222     C                   Eval      $OpeAss     = *off
273000141222     C                   endif
273100141222     C                   if        D1opestra   = 'S'
273200141222     C                   Eval      $OpeStra    = *on
273300141222     C                   else
273400141222     C                   Eval      $OpeStra    = *off
273500141222     C                   endif
273600130809     C                   MoveL     D1File        $File
273700141218     C                   Move      D1StpL        $StpL
273800130809     C*
273900130809     C                   MoveL     DSMPE         DSMPEPAR
274000130809     C*
274100130809     C                   CALLB     'XMPE'
274200130809     C                   PARM                    XMPEDS
274300000000     C                   PARM                    XMPEDS1
274400000000     C*
274500000000     C                   EndSr
274600000000     C************************************************************
274700000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
274800000000     C************************************************************
274900000000     C     DEFOPZ        BegSr
275000000000     C*
275100000000     C                   Reset                   $FC1
275200000000     C                   Reset                   $FR1
275300000000     C                   Reset                   $FC3
275400000000     C                   Reset                   $FR3
275500000000     C*
275600000000     C                   EndSr
275700000000     C************************************************************
275800000000     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
275900000000     C************************************************************
276000000000     C     DEFCAM        BegSr
276100000000     C*
276200000000     C* klist
276300000000     C*
276400000000     C* Variabili per gestione videate
276500000000     C*
276600000000     C                   Move      *BLANK        $GEST             2
276700000000     C                   Move      *BLANK        $FINE             1
276800000000     C                   Move      *BLANK        $InzD1            1
276900000000     C                   Move      *BLANK        $InzD2            1
277000000000     C                   Move      *BLANK        $INZD3            1
277100150119     C                   Move      *BLANK        $INZD4            1
277200000000     C                   Move      *BLANK        $LastG            2
277300000000     C                   Move      *BLANK        $LastV            2
277400000000     C                   Z-Add     0             $PIED1            3 0
277500000000     C*
277600000000     C* Variabili appoggio
277700000000     C*
277800000000     C                   Z-Add     0             CURR              2 0
277900000000     C                   Z-Add     0             CURC              2 0
278000000000     C                   Move      *ZEROS        WIN              99
278100000000     C*
278200000000     C* Indici
278300000000     C                   Z-Add     0             X                 3 0
278400000000     C                   Z-Add     0             Y                 3 0
278500000000     C*
278600000000     C                   EndSr
278700000000     C************************************************************
278800000000     C* RIEMPIMENTO TASTI FUNZIONE
278900000000     C************************************************************
279000000000     C     RIEKEY        BegSr
279100000000     C*
279200000000     C* Input :
279300000000     C* - $FX = schiera con tutti i testi disponibili
279400000000     C* - $FC = schiere con i flag di validita associati ai testi
279500000000     C* - $FR = schiere con i flag di REVERSE IMAGE
279600000000     C* - $ULK = ultimo testo caricato
279700000000     C* - $L01 = Lunghezza prima riga
279800000000     C* - $L02 = Lunghezza seconda riga
279900000000     C* Output :
280000000000     C* - $ULK
280100000000     C* - $KEY1= 1ø riga di tasti funzione
280200000000     C* - $KEY2= 2ø riga di tasti funzione
280300000000     C*
280400000000     C                   CALLB     'XKEYMSG'
280500000000     C* input
280600000000     C                   PARM                    $FX
280700000000     C                   PARM                    $FC
280800000000     C                   PARM                    $FR
280900000000     C                   PARM                    $ULK              3 0
281000000000     C* output
281100000000     C                   PARM                    $KEY1            79
281200000000     C                   PARM                    $KEY2            79
281300000000     C* input
281400000000     C                   PARM                    $L01              3 0
281500000000     C                   PARM                    $L02              3 0
281600000000     C*
281700000000     C                   PARM                    $ALLFUNCT       480
281800000000     C*
281900000000     C                   EndSr
282000000000** TASTI DI FUNZIONE  D1
282100000000PROMSG    *LIBL     KEY0001  01
282200000000PROMSG    *LIBL     KEY0002  02
282300000000PROMSG    *LIBL     KEY0003  03
282400000000PROMSG    *LIBL     KEY0004  04
282500000000PROMSG    *LIBL     KEY0005  05
282600000000PROMSG    *LIBL     KEY0006  06
282700000000PROMSG    *LIBL     KEY0007  07
282800000000PROMSG    *LIBL     KEY0008  08
282900000000PROMSG    *LIBL     KEY0009  09
283000000000PROMSG    *LIBL     KEY0010  10
283100000000PROMSG    *LIBL     KEY0011  11
283200000000PROMSG    *LIBL     KEY0012  12
283300000000PROMSG    *LIBL     KEY0013  13
283400000000PROMSG    *LIBL     KEY0014  14
283500000000PROMSG    *LIBL     KEY0015  15
283600000000PROMSG    *LIBL     KEY0016  16
283700000000PROMSG    *LIBL     KEY0017  17
283800000000PROMSG    *LIBL     KEY0018  18
283900150119SDGMSG    *LIBL     SDK0019  19
284000130916SDGMSG    *LIBL     SDK0017  20
284100000000PROMSG    *LIBL     KEY0021  21
284200000000PROMSG    *LIBL     KEY0022  22
284300000000PROMSG    *LIBL     KEY0023  23
284400000000PROMSG    *LIBL     KEY0024  24
284500000000**  ABILITAZIONE D1 /D2
2846000000000N  01
2847000000000N  02
2848000000001N  03
2849000000001N  04
2850000000001N  05
2851000000001N  06
2852000000000N  07
2853000000000N  08
2854000000000N  09
2855000000000N  10
2856000000000N  11
2857000000001N  12
2858000000000N  13
2859000000000N  14
2860000000000N  15
2861000000000N  16
2862000000000N  17
2863000000001N  18
2864001501191N  19
2865000000001N  20
2866000000000N  21
2867000000000N  22
2868000000000N  23   Impostare '1' se si usano le opzioni
2869000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
287000000000** TASTI DI FUNZIONE  D2
287100000000PROMSG    *LIBL     KEY0001  01
287200000000PROMSG    *LIBL     KEY0002  02
287300000000PROMSG    *LIBL     KEY0003  03
287400000000PROMSG    *LIBL     KEY0004  04
287500000000PROMSG    *LIBL     KEY0005  05
287600000000PROMSG    *LIBL     KEY0006  06
287700000000PROMSG    *LIBL     KEY0007  07
287800000000PROMSG    *LIBL     KEY0008  08
287900000000PROMSG    *LIBL     KEY0009  09
288000000000PROMSG    *LIBL     KEY0010  10
288100000000PROMSG    *LIBL     KEY0011  11
288200000000PROMSG    *LIBL     KEY0012  12
288300000000PROMSG    *LIBL     KEY0013  13
288400000000PROMSG    *LIBL     KEY0014  14
288500000000PROMSG    *LIBL     KEY0015  15
288600000000PROMSG    *LIBL     KEY0016  16
288700000000PROMSG    *LIBL     KEY0017  17
288800000000PROMSG    *LIBL     KEY0018  18
288900000000PROMSG    *LIBL     KEY0019  19
289000000000PROMSG    *LIBL     KEY0020  20
289100000000PROMSG    *LIBL     KEY0021  21
289200000000PROMSG    *LIBL     KEY0022  22
289300000000PROMSG    *LIBL     KEY0023  23
289400000000PROMSG    *LIBL     KEY0024  24
289500000000**  ABILITAZIONE D2
2896000000000N  01
2897000000000N  02
2898000000001N  03
2899001605301N  04         ## R224
2900000000001N  05
2901000000001N  06
2902000000000N  07
2903000000000N  08
2904000000000N  09
2905000000000N  10
2906000000000N  11
2907000000001N  12
2908000000000N  13
2909000000000N  14
2910000000000N  15
2911000000000N  16
2912000000000N  17
2913000000000N  18
2914000000000N  19
2915000000000N  20
2916000000000N  21
2917000000000N  22
2918000000000N  23   Impostare '1' se si usano le opzioni
2919000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
292000000000** TASTI DI FUNZIONE  D3
292100000000PROMSG    *LIBL     KEY0001  01
292200000000PROMSG    *LIBL     KEY0002  02
292300000000PROMSG    *LIBL     KEY0003  03
292400000000PROMSG    *LIBL     KEY0004  04
292500000000PROMSG    *LIBL     KEY0005  05
292600000000PROMSG    *LIBL     KEY0006  06
292700000000PROMSG    *LIBL     KEY0007  07
292800000000PROMSG    *LIBL     KEY0008  08
292900000000PROMSG    *LIBL     KEY0009  09
293000000000PROMSG    *LIBL     KEY0010  10
293100000000PROMSG    *LIBL     KEY0011  11
293200000000PROMSG    *LIBL     KEY0012  12
293300000000PROMSG    *LIBL     KEY0013  13
293400000000PROMSG    *LIBL     KEY0014  14
293500000000PROMSG    *LIBL     KEY0015  15
293600000000PROMSG    *LIBL     KEY0016  16
293700000000PROMSG    *LIBL     KEY0017  17
293800000000PROMSG    *LIBL     KEY0018  18
293900000000PROMSG    *LIBL     KEY0019  19
294000000000PROMSG    *LIBL     KEY0020  20
294100000000PROMSG    *LIBL     KEY0021  21
294200000000PROMSG    *LIBL     KEY0022  22
294300000000PROMSG    *LIBL     KEY0023  23
294400000000PROMSG    *LIBL     KEY0024  24
294500000000**  ABILITAZIONE D3
2946000000000N  01
2947000000000N  02
2948000000001N  03
2949000000000N  04
2950000000000N  05
2951000000000N  06
2952000000000N  07
2953000000000N  08
2954000000000N  09
2955000000000N  10
2956000000000N  11
2957000000001N  12
2958000000000N  13
2959000000000N  14
2960000000000N  15
2961000000000N  16
2962000000000N  17
2963000000000N  18
2964000000000N  19
2965000000000N  20
2966000000000N  21
2967000000000N  22
2968000000000N  23   Impostare '1' se si usano le opzioni
2969000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
