000100000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(QILE)
000200000000     H*PARMS BNDDIR(PJXBND PJCBND) CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500000000      *****************************************************************
000600000000      *
000700000000      *         ------------------------------------------------
000800150128      *                            CU 2015
000900130809      *                 Lancio creazione file telematico
001000000000      *         ------------------------------------------------
001100000000      *
001200000000      *****************************************************************
001300000000     F*----------------------------------------------------*
001400150128     FSDGSK60D  CF   E             WORKSTN
001500150128     F                                     RENAME(SK60T1:FMTT1)
001600150128     F                                     RENAME(SK60D1:FMTD1)
001700150128     F                                     RENAME(SK60Z1:FMTZ1)
001800150128     F                                     RENAME(SK60D2:FMTD2)
001900150128     F                                     RENAME(SK60D3:FMTD3)
001901160603R224 F                                     RENAME(SK60D4:FMTD4)
002000150128     F                                     RENAME(SK60Z3:FMTZ3)
002100150128     F                                     RENAME(SK60Z2:FMTZ2)
002200000000     F                                     INFDS(DSFMT)
002300150306R172 FND77019L  IF   E           K DISK
002400000000     F*------------
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
006700110421     D*-------------
006800110421     D* Ricerca legende
006900110421     D Ana964DS      E DS
006901160603R224 D*---------
006902160603 "   D* tabella destinazione dati aggiuntivi
006903160603 "   D SDGHC4DS      E DS                  INZ
006904160603 "   D*-------------
006905160603 "   D* Parametri per ricerca codice ANATB00F
006906160603 "   D XTABDS        E DS                  inz
006907160603 "   D*-------------
006908160603 "   D* Parametri per controllo codice ANATB00F
006909160613 "   D SDGTABDS      E DS                  INZ
006910160613 "   D*----------------
006911160613 "   D* tabelle delle nature giuridiche
006912160613R224 D ANGG16DS      E DS                  INZ
007000101118     D*-------------
007100000000     D* Valori assunti
007200000000     D XMPEDS        E DS                  INZ
007300000000     D XMPEDS1       E DS                  INZ
007400000000     D DSMPE           DS                  INZ
007500000000     D  $VAL                          1
007600000000     D  $Anno                         4S 0
007700110421     D  $TipInv                       1S 0
007800110811     D  $SocInc                       1S 0
007900000000     D  $MaxDim                       2S 0
008000110810     D  $MaxRec                       5S 0
008100000000     D  $File                        10A
008200000000     D*-------------
008300110811     D DSInterm        DS                  INZ
008400150204     D $IdProd                       16A
008500150204     D $FisInt                       16A
008600110811     D $ImpTrs                        1A
008700150204     D $FirInt                       30A
008800110811     D $ImpDta                       10A
008900150204     D $FisSog                       16A
009000150204     D $CarSog                        2A
009100150204     D $CogSog                       24A
009200150204     D $NomSog                       20A
009300150204     D $FisDic                       11A
009400150204     D $DtFirDic                     10A
009500150204     D $FirDic                       30A
009600150204     D*-------------
009700150204     D DSInterm2       DS                  INZ
009800150204     D $Telefon                      12A
009900150204     D $Email                        69A
009901160603R224 D*-------------
009902160607 "   D DSSostit        DS                  INZ
009911160607R224 D $DestSos                            Like(D4DestSos)
010000110811     D*-------------
010100000000     D* Reperimento nome PGM
010200000000     D STATUS         SDS           333
010300000000     D  DSPGM            *PROC
010400000000     D  PARMS            *PARMS
010500000000     D*-------------
010600000000     D* Dati di ambiente ottenuti da SDGSOCDR
010700000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
010800000000     D*-------------
010900000000     D* DS Interna per dati di output di SDGSOCDR
011000000000     D XSOCDS          DS          1000
011100000000     D*-------------
011200000000     D* Ds per ricezione parametri
011300150128     D SDGSK60DS     E DS
011400150128     D  ERRK60       E                     INZ('0')
011500000000     D*-------------
011600000000     D* Passaggio Parametri al PGM di estrazione dati
011700150203     D SDGSK61DS     E DS
011800150203     D  ERRK61       E                     INZ('0')
011900000000     D*-------------
012000000000     D* Parametri modulo SCARSUMA
012100000000     D ANP022DS      E DS
012200000000     D*-------------
012300000000     D* Catalogo file per scarico su supporto magnetico
012400000000     D ANA122DS      E DS
012500000000     D*-------------
012600000000     D X07VALDS      E DS
012700000000     D*-------------
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
017900000000     D $OkExeLan       S              1
018000000000     D $AnnoAlfa       S              4
018100000000     D DataISO         S               D
018200110811     D WDtImp          S               D
018300150204     D WDtFirDic       S               D
018400000000     D $DT0            C                   CONST('0001-01-01')
018500130904     D*
018600130904     D* Il campo D1File è stato tolto dalla gestione video
018700130904     D* lo tengo in vita nel pgm ma ininfluente
018800150128     D D1File          S                   Like(FileK60    )
018900150204     D*----------------
019000150204     D* Variabili per chiamata a X63CtrChar
019100150204     D X63Codice       S            100A
019200150204     D X63Len          S              2S 0
019300150204     D X63Tipo         S              1A
019400150204     D X63Blank        S              1A
019500150204     D X63Errore       S              1A
019600130904     D*----------------
019700130904     D* ds per nome file
019800130904     D                 DS
019900130904     D  WAnno                  3      6  0
020000130904     D  WPer                   7      8
020100000000     D  WPrgInvio              9     10  0
020200000000     D  WNumero                1     10
020300000000     D*-----------
020400000000     D* Ds contenente i valori del msg PRO1797
020500000000     D §Fil_Lib        DS
020600000000     D  §File                        10A
020700000000     D  §Libreria                    10A
020800000000     D*----------------------------------------------------*
020900000000     D $P1COS2161      s             20A
021000130904     D*---------------------------------------------------------------*
021100150204     D AgeMaxRec       C                   39500
021200130904     C*----------------------------------------------------*
021300130904     C*                MAIN LINE PROGRAM
021400130904     C*----------------------------------------------------*
021500130904     C*
021600130904     C* inizializzazione variabili
021700130904     C                   Exsr      INZVAR
021800130904     C*
021900000000     C     $FINE         DOWEQ     *OFF
022000000000     C     $GEST         CASEQ     'D1'          GESD1
022100000000     C     $GEST         CASEQ     'D2'          GESD2
022200000000     C     $GEST         CASEQ     'D3'          GESD3
022201160603R224 C     $GEST         CASEQ     'D4'          GESD4
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
024700000000     C*
024800000000     C* determino Riga/Colonna del cursore
024900000000     C                   Move      NRG           R$$
025000000000     C                   Move      NCL           C$$
025100000000     C                   Z-Add     RIRI          CURR
025200000000     C                   Z-Add     COCO          CURC
025300000000     C*
025400000000     C* I campi che al momento dell'F4 risultano protetti non devono
025500000000     C* essere abilitati alla funzione di ricerca
025600000000     C*
025700000000     C                   Select
025800000000     C*
025900000000     C* FMTD1  - file su supporto magnetico
026000000000     C     H1NMFL        WhenEq    'D1FILE'
026100000000     C                   Reset                   ANA122DS
026200000000     C                   Move      '01'          Opz122
026300000000     C                   Move      XScSoc        Societa122
026400000000     C                   Move      XScSoc        Soc122
026500150203     C                   Move      'CU'          TpScar122
026600000000     C                   Clear                   KPJBU
026700000000     C                   Movel     ANA122DS      KPJBU
026800000000     C                   Call      'ANA122R'
026900000000     C                   Parm                    KPJBA
027000000000     C                   Movel     KPJBU         ANA122DS
027100000000     C*
027200130809     C                   If        File122  <>   *Blanks
027300130809     C                   MoveL     File122       D1File
027400130809     C                   endIf
027500130809     C*
027600130809     C     H1NMFL        WHENEQ    'D1TIPINV'
027700130809     C                   EXSR      F4_TIPINV
027800130809     C*
027900130809     C     H1NMFL        WHENEQ    'D2IMPTRS'
028000130809     C                   EXSR      F4_ImpTrs
028001160603R224 C*
028002160603 "   C* FMTD4 - Destinazione dati anagrafici aggiuntivi
028003160607 "   C     H1NMFL        WHENEQ    'D4DESTSOS'
028004160607R224 C                   EXSR      F4_DestSos
028100130809     C*
028200130809     C* ricerca non abilitata in questa posizione
028300130809     C                   Other
028400130809     C                   Seton                                        9899
028500130809     C*
028600110421     C                   EndSl
028700110421     C*
028800000000     C* imposto pos. del cursore
028900000000     C                   Z-Add     CURR          H1RIGA
029000000000     C                   Z-Add     CURC          H1COLO
029100000000     C*
029200000000     C                   Move      *HIVAL        $LastV
029300000000     C*
029400000000     C                   EndSr
029500110802     C************************************************************
029600110802     C* Ricerca tipo invio
029700110802     C************************************************************
029800110802     C     F4_TipInv     BEGSR
029900110802     C*
030000110802     C                   Reset                   ANA964DS
030100110802     C*
030200110802     C                   MoveL     '01'          Opz964
030300150128     C                   MoveL     'SDGSK60S'    Trc964
030400150128     C                   MoveL     'TIPINVK60'   Cam964
030500110802     C                   MoveL     *On           All964
030600110802     C*
030700110802     C                   MoveL     ANA964DS      KPJBU
030800130809     C                   CALL      'ANA964R'
030900130809     C                   PARM                    KPJBA
031000130809     C                   MoveL     KPJBU         ANA964DS
031100130809     C*
031200130809     C                   if        Ret964     <> '2'
031300130809     C                   MoveL     Val964        D1TIPINV
031400130809     C                   endif
031500130809     C*
031600130809     C                   ENDSR
031700110811     C************************************************************
031800110811     C* Ricerca tipo impegno alla trasmissione
031900110811     C************************************************************
032000110811     C     F4_ImpTrs     BEGSR
032100110811     C*
032200110811     C                   Reset                   ANA964DS
032300110811     C*
032400110811     C                   MoveL     '01'          Opz964
032500110811     C                   MoveL     'ANDIZ000'    Trc964
032600110811     C                   MoveL     'SDGIMPTRS'   Cam964
032700110811     C                   MoveL     *On           All964
032800110811     C*
032900110811     C                   MoveL     ANA964DS      KPJBU
033000110811     C                   CALL      'ANA964R'
033100110811     C                   PARM                    KPJBA
033200110811     C                   MoveL     KPJBU         ANA964DS
033300110811     C*
033400110811     C                   if        Ret964     <> '2'
033500110811     C                   MoveL     Val964        D2ImpTrs
033600110811     C                   endif
033700110811     C*
033800110811     C                   ENDSR
033801160603R224 C************************************************************
033802160603 "   C* Ricerca valori per Destinazione dati anagrafici aggiuntivi
033803160603 "   C************************************************************
033804160607R224 C     F4_DestSos    BEGSR
033805160603     C*
033806160603     C*
033807160603     C                   RESET                   XTABDS
033808160603     C*
033809160603     C                   MOVE      '01'          XTAOPZ
033810160603     C                   MOVE      XSCSOC        XTAAZI
033811160603     C                   MOVE      XSCLIN        XTALIN
033812160603     C                   Z-ADD     CURR          XTARIG
033813160603     C                   Z-ADD     CURC          XTACOL
033814160603     C                   CLEAR                   KPJBU
033815160603     C                   MOVEL     XTABDS        KPJBU
033816160603     C                   CALL      'SDGHC4R'
033817160603     C                   PARM                    KPJBA
033818160603     C                   MOVEL     KPJBU         XTABDS
033819160603     C*
033820160603     C                   if        XTaErr     <> '1'  and
033821160603     C                             XTaKey     <> *Blank
033822160607     C                   Move      XTaKey        D4DestSos
033823160603     C                   endif
033824160603     c*
033825160603R224 C                   ENDSR
033826160603R224 C/EJECT
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
035300000000     C*
035400000000     C     $GEST         WhenEq    'D2'
035401160603R224 C     $GEST         oreq      'D4'
035500000000     C     $ULKD2        IfEq      0
035600000000     C* riempimento opzioni
035700000000     C                   Exsr      F24D2
035800000000     C                   ElSe
035900000000     C                   Write     FMTZ2
036000000000     C                   EndIf
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
037501160613R224 C*
037502160613 "   C* Ulteriore definizione delle opzioni utilizzabili a pgm
037503160613 "   C                   EXSR      UltOpz
037504160613R224 C*
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
041100000000     C     $Tasto        WhenEq    ROLLUP
041200000000     C                   Move      'D2'          $GEST
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
043301160607R224 C* F19=Dati sostituto d'imposta
043302160603 "   C     $Tasto        WhenEq    F19
043303160603R244 C                   Exsr      F19D1
043400000000     C* F20=Dati rappresentante
043500000000     C     $Tasto        WhenEq    F20
043600000000     C                   Exsr      F20D1
043700000000     C* F21=Pers. batch
043800000000     C     $Tasto        WhenEq    F21
043900000000     C                   Exsr      F21D1
044000000000     C* F24=Altri tasti
044100000000     C     $Tasto        WhenEq    F24
044200000000     C                   Exsr      F24D1
044300000000     C*
044400000000     C                   Other
044500000000     C                   Exsr      CTRD1
044600000000     C* aggiungere le chiamate ai controlli delle videate supplementari
044700000000     C     *IN99         IfEq      *OFF
044800000000     C                   Exsr      CTRD2
044900000000     C     *IN99         IfEq      *ON
045000000000     C                   Seton                                            96
045100000000     C                   EndIf
045200000000     C                   EndIf
045300000000     C*
045400000000     C     $Tasto        IfEq      F06
045500000000     C     *IN99         ANDEQ     *OFF
045600000000     C*
045700150203     C                   Eval      $OkExeLan   = *On
045800000000     C*
045900110811     C* controllo se esiste gia il file con il nome reperito
046000150203     C****               EXSR      CtrFileTel
046100000000     C*
046200000000     C* segnalo la richiesta di file a grandezza imposta
046300000000     C                   If        $OkExeLan   = *On
046400110811     C****               Exsr      CtrInvio
046500000000     C                   EndIf
046600000000     C*
046700000000     C                   If        $OkExeLan   = *On
046800000000     C* lancio
046900000000     C                   Exsr      ExeLan
047000000000     C                   Exsr      EndPgm
047100000000     C*
047200000000     C                   EndIf
047300000000     C*
047400000000     C                   EndIf
047500000000     C*
047600000000     C                   EndSl
047700000000     C*
047800000000     C                   EndIf
047900000000     C*
048000000000     C                   EndSr
048001160613R224 C************************************************************
048002160613 "   C* ULTERIORE DEFINIZIONE OPZIONI UTILIZZABILI A PGM
048003160613 "   C************************************************************
048004160613R224 C     UltOpz        BEGSR
048005160613     C*
048006160613     C*-----------------------------------------------------------
048007160613     C* dico che non ho ancora caricato alcun tasto funzione, così
048008160613     C* li carica tutti di nuovo
048009160613     C                   Z-ADD     0             $ULKD1                         piede da emett. D1
048010160613     C*-----------------------------------------------------------
048011160613     C*
048012160613     C* Questa routine deve eseguire una ulteriore definizione
048013160613     C* dei tasti funzione a seconda che la società contribuente
048014160613     C* abbia ragione sociale di persona fisica o persona giuridica
048015160613     C*
048016160613     C* se persona fisica, mostro il tasto F19=Contribuente
048017160613     C                   if        TpNG16     = '1'
048018160613     C* F19=Contribuente
048019160613     C                   Z-ADD     19            X
048020160613     C                   MOVE      '1'           $FC1(X)
048021160613     C*
048022160613     C                   else
048023160613     C*
048024160613     C* se persona giuridica, nascondo il tasto F19=Contribuente
048025160613     C*
048026160613     C* F19=Contribuente
048027160613     C                   Z-ADD     19            X
048028160613     C                   MOVE      '0'           $FC1(X)
048029160613     C*
048030160613     C                   endif
048031160613     C*
048032160613     C* Reperimento tasti di funzione
048033160613     C*
048034160613     C                   EXSR      REPKEY
048035160613     C*
048036160613R224 C                   ENDSR
048037160613R224 C/EJECT
048100000000     C************************************************************
048200000000     C* INIZIALIZZAZIONE VIDEATA DATI
048300000000     C************************************************************
048400000000     C     InzD1         BegSr
048500000000     C*
048600000000     C                   Clear                   FMTD1
048700000000     C                   Clear                   FMTD2
048800000000     C*
048900000000     C                   Z-Add     1             $PIED1
049000000000     C*
049100110802     C* se il pgm e'stato richiamato imposto i valori ricevuti dalla DS
049200000000     C                   If        CallPgm     = '1'
049300000000     C*
049400150128     C                   Exsr      RieD1K60
049500000000     C*
049600110802     C* se il pgm NON e stato richiamato
049700000000     C                   ElSe
049800000000     C*
049900000000     C* Se vuole impostazione lancio da ultima elaborazione, li imposto da anmpe
050000000000     C     $Val          IfEq      *ON
050100000000     C* riempio la videata da parametri memorizzati
050200000000     C                   Exsr      RIED1
050300000000     C*  Aggiungere le routine di riempimento dei record succcessivi
050400000000     C                   Exsr      RIED2
050500000000     C                   ElSe
050600000000     C* Immettere i valodi di default a video
050700000000     C                   Eval      D1Anno      = *Year
050800110802     C                   Eval      D1Anno      = D1Anno - 1
050900130809     C                   Eval      D1MaxDim    = 3
051000130809     C                   Eval      D1MaxRec    = AgeMaxRec
051100130809     C                   EndIf
051200130809     C*
051300130809     C                   EndIf
051400130809     C*
051500130809     C* richiamo routine dei ctrl per decodificarla
051600150210     C**                 Exsr      CTRD1
051700130809     C* Aggiungere le routine di controllo dei record succcessivi
051800150210     C**                 Exsr      CTRD2
051900150210     C**                 Move      *ALL'0'       IN5098
052000150210     C**                 MoveA     IN5098        *IN(50)
052100130809     C*
052200130809     C                   EndSr
052300130809     C************************************************************
052400130809     C* Riempimento videata  D1 da richiamo pgm
052500130809     C************************************************************
052600150128     C     RieD1K60      BegSr
052700130809     C*
052800150128     C                   Z-Add     AnnoK60       D1Anno
052900150128     C                   Z-Add     PROTOCK60     D1Protoc
053000150128     C                   Z-Add     PROTODK60     D1Protod
053100150128     C                   Eval      D1TipInv    = TipInvK60
053200150128     C                   Z-Add     MaxDimK60     D1MaxDim
053300150128     C                   Z-Add     MaxRecK60     D1MaxRec
053400150128     C                   Eval      D1File      = FileK60
053500130809     C*
053600130809     C                   EndSr
053700130809     C************************************************************
053800130809     C* RIEMPIMENTO VIDEATA  D1
053900130809     C************************************************************
054000130809     C     RIED1         BegSr
054100130809     C*
054200130809     C* impostare i campi della videata dai parametri memorizzati su anmpe
054300130809     C                   Z-Add     $Anno         D1Anno
054400130809     C                   Z-Add     $MaxDim       D1MaxDim
054500130809     C                   Z-Add     $MaxRec       D1MaxRec
054600000000     C                   Movel     $File         D1File
054700130809     C                   Eval      D1TipInv    = $TipInv
054800130809     C*
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
060501160603R224 C************************************************************
060502160603 "   C* GESTIONE F19 VIDEO D1
060503160603 "   C************************************************************
060504160603R224 C     F19D1         BegSr
060505160603     C*
060506160603     C                   Move      'D4'          $GEST
060507160603     C                   Move      *ON           $INZD4
060508160603     C*
060509160603R224 C                   EndSr
060600000000     C************************************************************
060700000000     C* GESTIONE F20 VIDEO D2
060800000000     C************************************************************
060900000000     C     F20D1         BegSr
061000000000     C*
061100000000     C                   Move      'D2'          $GEST
061200000000     C                   Move      *ON           $INZD2
061300000000     C*
061400000000     C                   EndSr
061500000000     C************************************************************
061600000000     C* GESTIONE F21 VIDEO D1
061700000000     C************************************************************
061800000000     C     F21D1         BegSr
061900000000     C*
062000000000     C                   Move      '    '        KCOAZ
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
064500000000     C************************************************************
064600000000     C* GESTIONE F24 VIDEO D3
064700000000     C************************************************************
064800000000     C     F24D2         BegSr
064900000000     C*
065000000000     C                   MoveA     $FX2          $FX
065100000000     C                   MoveA     $FC2          $FC
065200000000     C                   MoveA     $FR2          $FR
065300000000     C                   Z-Add     $ULKD2        $ULK
065400000000     C                   Z-Add     79            $L01
065500000000     C                   Z-Add     79            $L02
065600000000     C                   Exsr      RIEKEY
065700000000     C                   Z-Add     $ULK          $ULKD2            3 0
065800000000     C                   MoveA     $FC           $FC2
065900000000     C                   MoveL     $KEY1         Z2KE1
066000000000     C                   MoveL     $ALLFUNCT     H3ALLFUNCT
066100000000     C                   MoveL     $KEY2         Z2KE2
066200000000     C                   Write     FMTZ2
066300000000     C*
066400000000     C                   EndSr
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
069600130809     C*
069700130809     C* tipo invio
069800130809     C                   Clear                   D1TipInvD
069900130809     C                   Exsr      CtrTipInv
070000130809     C*
070100130809     C* protocollo  telematico e documento
070200130809     C                   Exsr      CtrProtoco
070300110421     C*
070400000000     C* Reperimento nome file per passaggio a agenzia
070500150203     C*****              Exsr      RepNomeF
070600000000     C*
070700000000     C* file da cui recuperare t.rec. A/B
070800150203     C*****              EXSR      CtrFile
070900110421     C*
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
072100110421     C***********************************************************
072200110421     C* Controllo tipo invio
072300110421     C***********************************************************
072400110421     C     CtrTipInv     BegSr
072500110421     C*
072600110421     C                   Clear                   D1TipInvD
072700110421     C*
072800110421     C                   Clear                   X07ValDs
072900110421     C*
073000110421     C                   Move      '1'           X07Ric
073100110421     C                   Move      *On           X07All
073200150128     C                   Movel     'SDGSK60S'    X07TRc
073300150128     C                   Movel     'TIPINVK60'   X07Cam
073400110802     C                   Movel     D1TIPINV      X07VAL
073500110421     C*
073600110421     C                   Callb     'X07VALR'
073700110421     C                   Parm                    X07ValDs
073800110421     C*
073900110421     C     X07ERR        IFEQ      *Off
074000110421     C                   MOVEL     X07DES        D1TipInvD
074100110802     C                   ElSe
074200110421     C                   SetOn                                        5299
074300110802     C                   EndIf
074400110421     C*
074500110421     C                   EndSr
074600130809     C***********************************************************
074700130809     C* Controllo protocollo
074800130809     C***********************************************************
074900130809     C     CtrProtoco    BEGSR
075000130809     C*
075100130809     C*  Telematico
075200130809     C                   If        D1TipInv   > 0    and
075300130809     C                             D1Protoc   = 0
075400130809     C                   SetOn                                        6199
075500130809     C                   EndIf
075600130809     C                   If        D1TipInv   = 0    and
075700130809     C                             D1Protoc   > 0
075800130809     C                   SetOn                                        6599
075900130904     C                   EndIf
076000130904     C*
076100130904     C*  Documento
076200130904     C                   If        D1TipInv   > 0    and
076300130904     C                             D1Protod   = 0
076400150306R172 X****               SetOn                                        7099
076500130904     C                   EndIf
076600130904     C                   If        D1TipInv   = 0    and
076700130904     C                             D1Protod   > 0
076800130809     C                   SetOn                                        6999
076900130809     C                   EndIf
077000150306R172 C*
077100150306 "   C*  Controllo esistenza protocollo telematico
077200150306 "   C                   if        D1TipInv   > 0    and
077300150306 "   C                             D1Protoc   > 0
077400150306 "   C                   eval      N77Societa = XscSoc
077500150306 "   C                   eval      N77Acf     = D1Anno
077600150306 "   C                   eval      N77Protoc  = D1Protoc
077700150306 "   C     K0377019      chain     Nd77019l
077800150306 "   C                   if        not %found(nd77019l)
077900150306 "   C                   seton                                        6099
078000150306 "   C                   endif
078100150306 "   C                   endIf
078200150306 "   C*
078300150306 "   C*  Controllo esistenza protocollo documento
078400150306 "   C                   If        D1TipInv   > 0    and
078500150306 "   C                             D1Protoc   > 0    and
078600150306 "   C                             D1Protod   > 0
078700150306 "   C                   eval      N77Societa = XscSoc
078800150306 "   C                   eval      N77Acf     = D1Anno
078900150306 "   C                   eval      N77Protoc  = D1Protoc
079000150306 "   C                   eval      N77ProtCea = D1Protod
079100150306 "   C     K0477019      chain     Nd77019l
079200150306 "   C                   if        not %found(nd77019l)
079300150306 "   C                   seton                                        6699
079400150306 "   C                   endif
079500150306R172 C                   endIf
079600130809     C*
079700130809     C                   EndSr
079800000000     C***********************************************************
079900000000     C* Controllo parametri per invio multiplo
080000000000     C***********************************************************
080100000000     C     CtrMltInvio   BegSr
080200000000     C*
080300000000     C                   If        D1MaxDim   <> 0   and
080400110810     C                             D1MaxRec   <> 0
080500110511     C                   Seton                                        6499
080600000000     C                   EndIf
080700000000     C*
080800000000     C                   EndSr
080900110811     C***********************************************************
081000110811     C* Controllo tipo impegno alla trasmissione
081100110811     C***********************************************************
081200110811     C     CtrImpTrs     BegSr
081300110811     C*
081400110811     C                   Clear                   D2ImpTrsD
081500110811     C*
081600110811     C                   Clear                   X07ValDs
081700110811     C*
081800110811     C                   Move      '1'           X07Ric
081900110811     C                   Move      *On           X07All
082000110811     C                   Movel     'ANDIZ000'    X07TRc
082100110811     C                   Movel     'SDGIMPTRS'   X07Cam
082200110811     C                   Movel     D2IMPTRS      X07VAL
082300110811     C*
082400110811     C                   Callb     'X07VALR'
082500110811     C                   Parm                    X07ValDs
082600110811     C*
082700110811     C     X07ERR        IFEQ      *Off
082800110811     C                   MOVEL     X07DES        D2ImpTrsD
082900110811     C                   ElSe
083000110811     C                   SetOn                                        6899
083100110811     C                   EndIf
083200110811     C*
083300110811     C                   EndSr
083400000000     C************************************************************
083500000000     C* Reperimento nome file per invio agenzia delle entrate
083600000000     C************************************************************
083700000000     C     RepNomeF      BegSr
083800000000     C*
083900000000     C                   Eval      WNumero     = *All'0'
084000000000     C                   Z-Add     D1Anno        WAnno
084100110811     C                   Z-Add     1             WPrgInvio
084200000000     C                   Move      WNumero       $Numero
084300140401     C*
084400150203     C                   Move      'CU'          $CdScarico
084500000000     C*
084600000000     C                   CALL      'X35NOMFR'
084700000000     C                   PARM                    XScSoc
084800000000     C                   PARM      *Blank        $Unita            8
084900150203     C                   PARM                    $CdScarico        2
085000000000     C                   PARM                    $Numero           9
085100000000     C                   PARM                    $File            10
085200000000     C                   PARM                    Lib022
085300000000     C                   PARM      *Off          $Errore           1
085400000000     C*
085500000000     C                   If        $Errore    <> '0'
085600110511     C                   Seton                                        5499
085700000000     C                   EndIf
085800000000     C*
085900000000     C                   EndSr
086000000000     C************************************************************
086100000000     C* Verifico esistenza file con stesso nome
086200000000     C************************************************************
086300000000     C     CtrFileTel    BegSr
086400000000     C*
086500000000     C*  CHKOBJ     OBJ($LIBRF/&FILE) OBJTYPE(*FILE)
086600000000     C                   Eval      Cmd         =
086700000000     C                             'CHKOBJ OBJ(' +
086800000000     C                             %Trim(Lib022) + '/' +
086900000000     C                             %Trim($File)  +
087000000000     C                             ') ' +
087100000000     C                             'OBJTYPE(*FILE)'
087200000000     C*
087300000000     C                   Call      'QCMDEXC'                            21
087400000000     C                   Parm                    Cmd             120
087500000000     C                   Parm      120           Len              15 5
087600000000     C*
087700000000     C*
087800000000     C                   Eval      $OkExeLan   = *Off
087900000000     C*
088000110421     C* se il file   presente nella libreria lo segnalo
088100000000     C                   If        *In21       = *Off
088200000000     C*
088300000000     C                   EXSR      MsgErr
088400000000     C*
088500000000     C                   Select
088600000000     C*
088700000000     C* richiesto di continuare
088800000000     C                   When      MsgRtn      = '1'
088900000000     C                   Eval      $OkExeLan   = *On
089000000000     C*
089100000000     C*
089200000000     C* richiesto di non continuare
089300000000     C                   When      MsgRtn      = '2'
089400000000     C                   Eval      $OkExeLan   = *Off
089500000000     C*
089600000000     C                   endsl
089700000000     C*
089800000000     C                   else
089900000000     C* se file non presente nella libreria, eseguo lancio
090000000000     C                   Eval      $OkExeLan   = *On
090100000000     C*
090200000000     C                   endIf
090300000000     C*
090400000000     C                   EndSr
090500000000     C************************************************************
090600000000     C* Controllo file per recupero rec. "A" e "B"
090700000000     C************************************************************
090800000000     C     CtrFile       BegSr
090900000000     C*
091000000000     C                   If        D1File    <> *Blank
091100000000     C*
091200110418     C* il file deve avere nome diverso da quello che andra a generare
091300000000     C                   If        D1File     = $File
091400000000     C                   Seton                                        6299
091500000000     C                   endIf
091600000000     C*
091700000000     C* verifico se esiste, nella libreria
091800000000     C* dei file per lo scarico su supporto magnetico,
091900000000     C* il file indicato a video
092000000000     C                   EXSR      ChkFileBl
092100000000     C*
092200000000     C                   endIf
092300000000     C*
092400000000     C                   EndSr
092500000000     C************************************************************
092600000000     C* Verifico l'esistenza del file nella libreria indicata
092700000000     C* per lo scarico su supporto magnetico
092800000000     C************************************************************
092900000000     C     ChkFileBl     BegSr
093000000000     C*
093100000000     C* CHKOBJ     OBJ(Lib022/D1File) OBJTYPE(*FILE)
093200000000     C                   Eval      Cmd         =
093300000000     C                             'CHKOBJ     OBJ('        +
093400000000     C                             %Trim(Lib022) + '/' +
093500000000     C                             %Trim(D1File) +
093600000000     C                             ') OBJTYPE(*FILE)'
093700000000     C*
093800000000     C                   Call      'QCMDEXC'                            21
093900000000     C                   Parm                    Cmd             120
094000000000     C                   Parm      120           Len              15 5
094100000000     C*
094200000000     C                   If        *In21       = *On
094300000000     C*
094400110418     C* Errore: dico che non il file non e stato trovato nella libreria
094500000000     C* Valorizzo le variabili del msg PRO1797
094600000000     C                   Move      D1File        §File
094700000000     C                   Move      Lib022        §Libreria
094800000000     C                   Seton                                        5499
094900000000     C*
095000000000     C                   endIf
095100000000     C*
095200000000     C                   EndSr
095300000000     C************************************************************
095400000000     C* Messaggio errore
095500000000     C************************************************************
095600000000     C     MsgErr        BegSr
095700000000      *
095800000000     C                   Reset                   SDGMSGDS
095900000000     C*
096000000000     C* Risposte possibili
096100000000     C* "1) Continuare      2) Annulla
096200000000     C                   Eval      IDMSG       =
096300000000     C                             'PROMSG    *LIBL     COS1204'
096400000000     C                   CALLB     'XRTVM'
096500000000     C                   PARM                    IDMSG
096600000000     C                   PARM      *Blank        TXTMSG
096700000000     C                   PARM                    ERRMSG
096800000000     C     ERRMSG        IFNE      *ON
096900000000     C                   MoveL     TXTMSG        XMSGTXT
097000000000     C                   ELSE
097100000000     C                   MoveL     *ALL'?'       XMSGTXT
097200000000     C                   ENDIf
097300000000     C*
097400000000     C                   MoveL     'S'           MSGEMV
097500000000     C                   Z-Add     10            MSGRGP
097600000000     C                   MoveL     'S'           MSGRSP
097700000000     C                   MoveL     '12'          MSGTPR
097800000000      *
097900000000     C                   Movel     $File         $P1COS2161
098000000000     C                   Move      Lib022        $P1COS2161
098100000000      *
098200000000     C                   call      'SDGMSGR'
098300000000     C                   parm                    SDGMSGDS
098400000000     C                   parm                    XMsgTxt         135
098500000000     C*
098600000000     C* Modo di inizializzazione della window:
098700000000     C* ('1'->Close + Open  -->Iø volta dopo l'emissione del video princip.)
098800000000     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
098900000000     C*                       per mantenere il sottofondo del video princip.)
099000000000     C                   parm      '1'           XMsgInz           1
099100000000     C*
099200000000      * COS2161-Il file &1 esiste gia nella libreria &2'
099300000000     C                   parm      'COS2161'     XMsgCd1           7
099400000000     C                   parm                    XMsgCd2           7
099500000000     C                   parm                    XMsgCd3           7
099600000000     C                   parm                    XMsgSn1           7
099700000000     C                   parm                    XMsgCdTxT        63
099800000000     C                   parm                    MsgCdTit         14
099900000000     C                   parm                    MsgClPrtf         1
100000000000     C                   parm      $P1COS2161    VarMsg1         100
100100000000     C                   parm                    VarMsg2         100
100200000000     C                   parm                    VarMsg3         100
100300000000     C*
100400000000     C                   ENDSR
100500000000     C************************************************************
100600000000     C* Verifico esistenza file con stesso nome
100700000000     C************************************************************
100800000000     C     CtrInvio      BegSr
100900000000     C*
101000000000     C                   Eval      $OkExeLan   = *Off
101100000000     C*
101200000000     C* se presenti parametri per creazione file con capacita limitata
101300000000     C* lo segnalo
101400000000     C                   If        D1MaxDim    > 0    or
101500110810     C                             D1MaxRec    > 0
101600000000     C*
101700000000     C                   Exsr      MsgErr3
101800000000     C*
101900000000     C                   Select
102000000000     C*
102100000000     C* richiesto di continuare
102200000000     C                   When      MsgRtn      = '1'
102300000000     C                   Eval      $OkExeLan   = *On
102400000000     C*
102500000000     C*
102600000000     C* richiesto di non continuare
102700000000     C                   When      MsgRtn      = '2'
102800000000     C                   Eval      $OkExeLan   = *Off
102900000000     C*
103000000000     C                   EndSl
103100000000     C*
103200000000     C                   ElSe
103300000000     C* se file non presenti tali parametri, eseguo lancio
103400000000     C                   Eval      $OkExeLan   = *On
103500000000     C*
103600000000     C                   EndIf
103700000000     C*
103800000000     C                   EndSr
103900000000     C************************************************************
104000000000     C* Messaggio errore
104100000000     C************************************************************
104200000000     C     MsgErr3       BegSr
104300000000      *
104400000000     C                   Reset                   SDGMSGDS
104500000000     C*
104600000000     C* Risposte possibili
104700000000     C* "1) Continuare      2) Annulla
104800000000     C                   Eval      IDMSG       =
104900000000     C                             'PROMSG    *LIBL     COS1204'
105000000000     C                   CALLB     'XRTVM'
105100000000     C                   PARM                    IDMSG
105200000000     C                   PARM      *Blank        TXTMSG
105300000000     C                   PARM                    ERRMSG
105400000000     C     ERRMSG        IFNE      *ON
105500000000     C                   MoveL     TXTMSG        XMSGTXT
105600000000     C                   ElSe
105700000000     C                   MoveL     *ALL'?'       XMSGTXT
105800000000     C                   EndIf
105900000000     C*
106000000000     C                   MoveL     'S'           MSGEMV
106100000000     C                   Z-Add     10            MSGRGP
106200000000     C                   MoveL     'S'           MSGRSP
106300000000     C                   MoveL     '12'          MSGTPR
106400000000      *
106500000000     C                   call      'SDGMSGR'
106600000000     C                   parm                    SDGMSGDS
106700000000     C                   parm                    XMsgTxt         135
106800000000     C*
106900000000     C* Modo di inizializzazione della window:
107000000000     C* ('1'->Close + Open  -->Iø volta dopo l'emissione del video princip.)
107100000000     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
107200000000     C*                       per mantenere il sottofondo del video princip.)
107300000000     C                   parm      '1'           XMsgInz           1
107400000000     C*
107500150204     C* COS2670- (F1) E' stato impostato un parametro per la
107600000000     C* eazione di file da trasmettere con capacita di dati limitata.
107700000000     C*  Se il file che verra creato avra capacita (in MegaByte o in
107800000000     C* numero di soggetti) superiore a quanto indicato, verranno
107900000000     C* creati altri file con la capacita richiesta fino a esaurimento
108000000000     C* dati e che dovranno essere traspessi con piu invii.
108100000000     C                   parm      'COS2670'     XMsgCd1           7
108200000000     C                   parm                    XMsgCd2           7
108300000000     C                   parm                    XMsgCd3           7
108400000000     C                   parm                    XMsgSn1           7
108500000000     C                   parm                    XMsgCdTxT        63
108600000000     C                   parm                    MsgCdTit         14
108700000000     C                   parm                    MsgClPrtf         1
108800000000     C                   parm                    VarMsg1         100
108900000000     C                   parm                    VarMsg2         100
109000000000     C                   parm                    VarMsg3         100
109100000000     C*
109200000000     C                   EndSr
109300000000     C************************************************************
109400000000     C* GESTIONE VIDEO RECORD  D2
109500000000     C************************************************************
109600000000     C     GESD2         BegSr
109700000000     C*
109800000000     C* inizializzo videata
109900000000     C     $InzD2        IfEq      *ON
110000000000     C                   Exsr      InzD2
110100150204     C                   Exsr      InzD2_2
110200000000     C                   Move      *OFF          $InzD2
110300000000     C                   EndIf
110400000000     C*
110500000000     C     $LastV        IFNE      'D2'
110600000000     C                   Write     FMTT1
110700000000     C                   Exsr      GESPIE
110800000000     C                   Move      $LastV        $LastG
110900000000     C                   Move      'D2'          $LastV
111000000000     C                   EndIf
111100000000     C*
111200000000     C* se esistono errori sulla videata
111300000000     C* emetto la Write del formato a indicatori spenti per vedere
111400000000     C* le eventuali decodifiche
111500000000     C     *IN99         IfEq      *ON
111600000000     C     $GEST         ANDEQ     'D2'
111700000000     C                   MoveA     *IN           WIN
111800000000     C                   Move      *ALL'0'       IN5098           49
111900000000     C                   MoveA     IN5098        *IN(50)
112000000000     C                   Write     FMTD2
112100000000     C                   MoveA     WIN           *IN
112200000000     C                   EndIf
112300000000     C*
112400000000     C                   EXFMT     FMTD2
112500000000     C*
112600000000     C* controllo abilitazione tasti funzione
112700000000     C                   Exsr      CTRKD2
112800000000     C*
112900000000     C     *IN99         IfEq      *OFF
113000000000     C*
113100000000     C                   Select
113200000000     C* ROLDOWN
113300000000     C     $Tasto        WhenEq    ROLDWN
113400000000     C                   Move      'D1'          $GEST
113500000000     C* F3=Fine
113600000000     C     $Tasto        WhenEq    F03
113700000000     C                   Exsr      F03D1
113800000000     C* F4=Ricerca
113900000000     C     $Tasto        WhenEq    F04
114000000000     C                   Exsr      SEARCH
114100000000     C* F5=Refresh
114200000000     C     $Tasto        WhenEq    F05
114300000000     C                   Exsr      F05D2
114400000000     C* F8=Successivo
114500000000     C     $Tasto        WhenEq    F08
114600000000     C                   Exsr      F08D1
114700000000     C* F12=Ritorno
114800000000     C     $Tasto        WhenEq    F12
114900000000     C                   Exsr      F12D2
115000000000     C* F18=Valori assunti
115100000000     C     $Tasto        WhenEq    F18
115200000000     C                   Exsr      F18D1
115300000000     C* F21=Pers. batch
115400000000     C     $Tasto        WhenEq    F21
115500000000     C                   Exsr      F21D1
115600000000     C* F24=Altri tasti
115700000000     C     $Tasto        WhenEq    F24
115800000000     C                   Exsr      F24D1
115900000000     C*
116000000000     C                   Other
116100000000     C                   Exsr      CTRD2
116200000000     C     *IN99         IfEq      *OFF
116300000000     C***                Exsr      CTRD1
116400000000     C***  *IN99         IfEq      *ON
116500000000     C***                Seton                                            96
116600000000     C***                EndIf
116700000000     C                   EndIf
116800000000     C*
116900000000     C*
117000000000     C     $Tasto        IfEq      F06
117100000000     C     *IN99         ANDEQ     *OFF
117200000000     C*
117300000000     C                   Exsr      AggD2
117400150204     C                   Exsr      AggD2_2
117500150204     C                   Move      'D1'          $GEST
117600000000     C*
117700000000     C                   EndIf
117800000000     C*
117900000000     C*
118000000000     C                   EndSl
118100000000     C*
118200000000     C                   EndIf
118300000000     C*
118400000000     C                   EndSr
118500000000     C************************************************************
118600110811     C* REPERIMENTO dati per intermediario
118700000000     C************************************************************
118800000000     C     InzD2         BegSr
118900000000     C*
119000000000     C* preparo i parametri di chiamata al driver
119100000000     C                   Clear                   XMPEDS
119200000000     C                   EVAL      TpChiamata = '01'
119300000000     C                   EVAL      NChiavi = 5
119400000000     C                   EVAL      VLogica = 2
119500000000     C*
119600000000     C* preparo le chiavi per la chiamata al driver
119700000000     C                   Clear                   XMPEDS1
119800000000     C                   MoveL     XSCSOC        DSMPESOC
119900000000     C                   MoveL     DSPGM         DSMPEPGM
120000140422     C                   Move      'R'           DSMPEPGM
120100000000     C                   MoveL     *blanks       DSMPEPRF
120200000000     C* Si reperiscono i dati di default a seconda delle modalita con
120300000000     C* cui il pgm e usato
120400000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
120500000000     C* MPETIP = 'PV'  pgm richiamato da menu
120600000000     C* MPETIP = 'PB'  x lancio Batch
120700000000     C                   MoveL     'PB'          DSMPETIP
120800000000     C*
120900000000     C                   CALLB     'XMPE'
121000000000     C                   PARM                    XMPEDS
121100000000     C                   PARM                    XMPEDS1
121200000000     C*
121300130913     C                   Clear                   DSInterm
121400150204     C*
121500130913     C                   Clear                   $ImpDta
121600150204     C                   Clear                   $DtFirDic
121700000000     C*
121800000000     C     Risultato     IfEq      '0'
121900110811     C                   MoveL     DSMPEPAR      DSInterm
122000130913     C                   If        $ImpDta       = *blanks
122100130913     C                   Move      $DT0          $ImpDta
122200130913     C                   EndIf
122300150204     C                   If        $DtFirDic     = *blanks
122400150204     C                   Move      $DT0          $DtFirDic
122500130913     C                   EndIf
122600150204     C*
122700150204     C* Identificativo produttore software
122800150204     C                   MoveL(P)  $IdProd       D2IdProd
122900000000     C*
123000150204     C* Intermediario
123100110811     C                   MoveL(P)  $FisInt       D2FisInt
123200150204     C* Impegno a trasmettere
123300110811     C                   Move      $ImpTrs       D2ImpTrs
123400150204     C* Firma Intermediario
123500150204     C                   MoveL(P)  $FirInt       D2FirInt
123600110811     C* Data impegno
123700110811     C                   MOVEL     $ImpDta       DataISO
123800110811     C                   If        DataISO = *LOVAL
123900110811     C                   Eval      D2ImpDta = *ZEROS
124000110811     C                   Else
124100110811     C                   Select
124200110811     C* formato mm/gg/aaaa
124300110811     C                   When      $DatFmt = '*MDY'
124400110811     C     *USA          Move      DataISO       D2ImpDta
124500110811     C* formato aa/mm/gg
124600110811     C                   When      $DatFmt = '*YMD'
124700110811     C     *ISO          Move      DataISO       D2ImpDta
124800110811     C* formato gg/mm/aaaa
124900110811     C                   When      $DatFmt = '*DMY'
125000110811     C     *eur          MOVEL     DataISO       D2ImpDta
125100110811     C                   EndSl
125200110811     C                   EndIf
125300130913     C*
125400130913     C* Soggetto
125500130913     C                   MoveL(P)  $FisSog       D2FisSog
125600130913     C                   Move      $CarSog       D2CarSog
125700150204     C                   MoveL     $CogSog       D2CogSog
125800150204     C                   MoveL     $NomSog       D2NomSog
125900150204     C                   MoveL(P)  $FisDic       D2FisDic
126000150204     C* Data firma
126100150204     C                   MOVEL     $DtFirDic     DataISO
126200130913     C                   If        DataISO = *LOVAL
126300150204     C                   Eval      D2DtFirDic = *ZEROS
126400130913     C                   Else
126500130913     C                   Select
126600130913     C* formato mm/gg/aaaa
126700130913     C                   When      $DatFmt = '*MDY'
126800150204     C     *USA          Move      DataISO       D2DtFirDic
126900130913     C* formato aa/mm/gg
127000130913     C                   When      $DatFmt = '*YMD'
127100150204     C     *ISO          Move      DataISO       D2DtFirDic
127200130913     C* formato gg/mm/aaaa
127300130913     C                   When      $DatFmt = '*DMY'
127400150204     C     *eur          MOVEL     DataISO       D2DtFirDic
127500130913     C                   EndSl
127600130913     C                   EndIf
127700150204     C* Firma del dichiarante
127800150204     C                   MoveL     $FIRDIC       D2FIRDIC
127900130913     C*
128000110811     C                   ElSe
128100110811     C                   Exsr      RepDefImp
128200110811     C                   EndIf
128300000000     C*
128400000000     C                   EndSr
128500150204     C************************************************************
128600150204     C* REPERIMENTO dati per intermediario
128700150204     C************************************************************
128800150204     C     InzD2_2       BegSr
128900150204     C*
129000150204     C* preparo i parametri di chiamata al driver
129100150204     C                   Clear                   XMPEDS
129200150204     C                   EVAL      TpChiamata = '01'
129300150204     C                   EVAL      NChiavi = 5
129400150204     C                   EVAL      VLogica = 2
129500150204     C*
129600150204     C* preparo le chiavi per la chiamata al driver
129700150204     C                   Clear                   XMPEDS1
129800150204     C                   MoveL     XSCSOC        DSMPESOC
129900150204     C                   MoveL     DSPGM         DSMPEPGM
130000150204     C                   Move      'S'           DSMPEPGM
130100150204     C                   MoveL     *blanks       DSMPEPRF
130200150204     C* Si reperiscono i dati di default a seconda delle modalita con
130300150204     C* cui il pgm e usato
130400150204     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
130500150204     C* MPETIP = 'PV'  pgm richiamato da menu
130600150204     C* MPETIP = 'PB'  x lancio Batch
130700150204     C                   MoveL     'PB'          DSMPETIP
130800150204     C*
130900150204     C                   CALLB     'XMPE'
131000150204     C                   PARM                    XMPEDS
131100150204     C                   PARM                    XMPEDS1
131200150204     C*
131300150204     C                   Clear                   DSInterm2
131400150204     C*
131500150204     C     Risultato     IfEq      '0'
131600150204     C                   MoveL     DSMPEPAR      DSInterm2
131700150204     C*
131800150204     C                   MoveL     $TELEFON      D2TELEFON
131900150204     C                   MoveL     $EMAIL        D2EMAIL
132000150204     C*
132100150204     C                   EndIf
132200150204     C*
132300150204     C                   Exsr      CTRD2
132400150204     C*
132500150204     C                   EndSr
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
139700000000     C************************************************************
139800000000     C* RIEMPIMENTO VIDEATA D2
139900000000     C************************************************************
140000000000     C     RIED2         BegSr
140100000000     C*
140200000000     C*
140300000000     C                   EndSr
140301160603R224 C************************************************************
140302160603 "   C* GESTIONE VIDEO RECORD  D4
140303160603 "   C************************************************************
140304160603R224 C     GESD4         BegSr
140305160603     C*
140306160603     C* inizializzo videata
140307160603     C     $InzD4        IfEq      *ON
140308160603     C                   Exsr      InzD4
140309160603     C                   Move      *OFF          $InzD4
140310160603     C                   EndIf
140311160603     C*
140312160603     C     $LastV        IFNE      'D4'
140313160603     C                   Write     FMTT1
140314160603     C                   Exsr      GESPIE
140315160603     C                   Move      $LastV        $LastG
140316160603     C                   Move      'D4'          $LastV
140317160603     C                   EndIf
140318160603     C*
140319160603     C* se esistono errori sulla videata
140320160603     C* emetto la Write del formato a indicatori spenti per vedere
140321160603     C* le eventuali decodifiche
140322160603     C     *IN99         IfEq      *ON
140323160603     C     $GEST         ANDEQ     'D4'
140324160603     C                   MoveA     *IN           WIN
140325160603     C                   Move      *ALL'0'       IN5098           49
140326160603     C                   MoveA     IN5098        *IN(50)
140327160603     C                   Write     FMTD4
140328160603     C                   MoveA     WIN           *IN
140329160603     C                   EndIf
140330160603     C*
140331160603     C                   EXFMT     FMTD4
140332160603     C*
140333160603     C* controllo abilitazione tasti funzione
140334160603     C                   Exsr      CTRKD2
140335160603     C*
140336160603     C     *IN99         IfEq      *OFF
140337160603     C*
140338160603     C                   Select
140339160603     C* F3=Fine
140340160603     C     $Tasto        WhenEq    F03
140341160603     C                   Exsr      F03D1
140342160603     C* F4=Ricerca
140343160603     C     $Tasto        WhenEq    F04
140344160603     C                   Exsr      SEARCH
140345160603     C* F5=Refresh
140346160603     C     $Tasto        WhenEq    F05
140347160603     C                   Exsr      F05D4
140348160603     C* F8=Successivo
140349160603     C     $Tasto        WhenEq    F08
140350160603     C                   Exsr      F08D1
140351160603     C* F12=Ritorno
140352160603     C     $Tasto        WhenEq    F12
140353160603     C                   Exsr      F12D2
140354160603     C* F24=Altri tasti
140355160603     C     $Tasto        WhenEq    F24
140356160603     C                   Exsr      F24D1
140357160603     C*
140358160603     C                   Other
140359160603     C                   Exsr      CTRD4
140360160603     C     *IN99         IfEq      *OFF
140361160603     C                   EndIf
140362160603     C*
140363160603     C*
140364160603     C     $Tasto        IfEq      F06
140365160603     C     *IN99         ANDEQ     *OFF
140366160603     C*
140367160603     C                   Exsr      AggD4
140368160603     C                   Move      'D1'          $GEST
140369160603     C*
140370160603     C                   EndIf
140371160603     C*
140372160603     C*
140373160603     C                   EndSl
140374160603     C*
140375160603     C                   EndIf
140376160603     C*
140377160603R224 C                   EndSr
140378160603R224 C************************************************************
140379160607 "   C* REPERIMENTO dati per sostituto d'imposta
140380160603 "   C************************************************************
140381160603R224 C     InzD4         BegSr
140382160603     C*
140383160603     C* preparo i parametri di chiamata al driver
140384160603     C                   Clear                   XMPEDS
140385160603     C                   EVAL      TpChiamata = '01'
140386160603     C                   EVAL      NChiavi = 5
140387160603     C                   EVAL      VLogica = 2
140388160603     C*
140389160603     C* preparo le chiavi per la chiamata al driver
140390160603     C                   Clear                   XMPEDS1
140391160603     C                   MoveL     XSCSOC        DSMPESOC
140392160609     C                   MoveL     'SDGSK60R C'  DSMPEPGM
140394160603     C                   MoveL     *blanks       DSMPEPRF
140395160603     C* Si reperiscono i dati di default a seconda delle modalita con
140396160603     C* cui il pgm e usato
140397160603     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
140398160603     C* MPETIP = 'PV'  pgm richiamato da menu
140399160603     C* MPETIP = 'PB'  x lancio Batch
140400160603     C                   MoveL     'PB'          DSMPETIP
140401160603     C*
140402160603     C                   CALLB     'XMPE'
140403160603     C                   PARM                    XMPEDS
140404160603     C                   PARM                    XMPEDS1
140405160603     C*
140406160607     C                   Clear                   DSSostit
140408160603     C*
140409160603     C     Risultato     IfEq      '0'
140410160607     C                   MoveL     DSMPEPAR      DSSostit
140414160603     C*
140441160607     C                   MoveL     $DestSos      D4DestSos
140442160603     C*
140443160603     C                   EndIf
140444160603     C*
140445160603     C*
140446160603R224 C                   EndSr
140447000000     C************************************************************
140500000000     C* GESTIONE F05 VIDEO D2
140600000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
140700000000     C* e il riposizionamento alla prima
140800000000     C************************************************************
140900000000     C     F05D2         BegSr
141000000000     C*
141100150204     C                   Move      *ON           $InzD2
141200150204     C                   Move      'D2'          $GEST
141300000000     C*
141400000000     C                   EndSr
141401160603R224 C************************************************************
141402160603 "   C* GESTIONE F05 VIDEO D4
141403160603 "   C************************************************************
141404160603R224 C     F05D4         BegSr
141405160603     C*
141406160603     C                   Move      *ON           $InzD4
141407160603     C                   Move      'D4'          $GEST
141408160603     C*
141409160603R224 C                   EndSr
141500000000     C************************************************************
141600000000     C* GESTIONE F12 VIDEO D2
141700000000     C************************************************************
141800000000     C     F12D2         BegSr
141900000000     C*
142000000000     C                   Move      'D1'          $GEST
142100000000     C*
142200000000     C                   EndSr
142300000000     C************************************************************
142400000000     C* GESTIONE F12 VIDEO D3
142500000000     C************************************************************
142600000000     C     F12D3         BegSr
142700000000     C*
142800000000     C                   Move      $LastG        $GEST
142900000000     C*
143000000000     C                   EndSr
143100000000     C************************************************************
143200000000     C* CONTROLLO VIDEATA  D2
143300000000     C************************************************************
143400000000     C     CtrD2         BegSr
143500000000     C*
143600000000     C                   SETOFF                                       99
143700000000     C* non imposto pos. del cursore
143800000000     C                   Z-Add     0             H1RIGA
143900130809     C                   Z-Add     0             H1COLO
144000150204     C*
144100150204     C                   If        D2IdProd   =  *blanks
144200150204     C                   Eval      D2IdProd    = XScCdF
144300150204     C                   EndIf
144400150204     C                   If        D2IdProd   =  *blanks
144500150204     C                   Seton                                        7899
144600150204     C                   EndIf
144700130809     C*
144800130913     C                   Exsr      CtrIntermed
144900130913     C*
145000130913     C                   Exsr      CtrSoggetto
145100150204     C*
145200150204     C* Data firma dichiarante
145300150204     C                   MOVEL     $DT0          WDtFirDic
145400150204     C     D2DtFirDic    IFNE      0
145500150204     C                   Select
145600150204     C* formato mm/gg/aaaa
145700150204     C                   When      $DatFmt = '*MDY'
145800150204     C     *USA          TEST(D)                 D2DtFirDic             73
145900150204     C* formato aa/mm/gg
146000150204     C                   When      $DatFmt = '*YMD'
146100150204     C     *ISO          TEST(D)                 D2DtFirDic             73
146200150204     C* formato gg/mm/aaaa
146300150204     C                   When      $DatFmt = '*DMY'
146400150204     C     *eur          TEST(D)                 D2DtFirDic             73
146500150204     C                   EndSl
146600150204     C* data ok
146700150204     C                   If        *in73 = *off
146800150204     C                   Select
146900150204     C* formato mm/gg/aaaa
147000150204     C                   When      $DatFmt = '*MDY'
147100150204     C     *USA          Move      D2DtFirDic    DataISO
147200150204     C* formato aa/mm/gg
147300150204     C                   When      $DatFmt = '*YMD'
147400150204     C     *ISO          Move      D2DtFirDic    DataISO
147500150204     C* formato gg/mm/aaaa
147600150204     C                   When      $DatFmt = '*DMY'
147700150204     C     *eur          MoveL     D2DtFirDic    DataISO
147800150204     C                   EndSl
147900150204     C                   MoveL     DataISO       WDtFirDic
148000150204     C                   Else
148100150204     C                   Seton                                          99
148200150204     C                   EndIf
148300150204     C                   Else
148400150204     C                   Seton                                          74
148500150204     C                   EndIf
148600150204     C*
148700150204     C* Firma dichiarante
148800150204     C                   if        D2FirDic  = *blank
148900150204     C                   seton                                        7599
149000150204     C                   endif
149100150204     C*
149200150204     C* Telefono: solo caratteri numerici
149300150204     C* tra blank e '9'
149400150204     C                   if        D2Telefon <> *blank
149500150204     C                   exsr      CtrlTel
149600150204     C                   if        X63Errore = *On
149700150204     C                   seton                                        7799
149800150204     C                   endif
149900150204     C                   endif
150000110811     C*
150100110811     C                   EndSr
150200110811     C************************************************************
150300130913     C* Controllo dati intermediario
150400110811     C************************************************************
150500130913     C     CtrIntermed   BegSr
150600000000     C*
150700130913     C                   MOVEL     $DT0          WDtImp
150800130913     C     D2ImpDta      IFNE      0
150900130913     C                   Select
151000130913     C* formato mm/gg/aaaa
151100130913     C                   When      $DatFmt = '*MDY'
151200130913     C     *USA          TEST(D)                 D2ImpDta               63
151300130913     C* formato aa/mm/gg
151400130913     C                   When      $DatFmt = '*YMD'
151500130913     C     *ISO          TEST(D)                 D2ImpDta               63
151600130913     C* formato gg/mm/aaaa
151700130913     C                   When      $DatFmt = '*DMY'
151800130913     C     *eur          TEST(D)                 D2ImpDta               63
151900130913     C                   EndSl
152000130913     C* data ok
152100130913     C                   If        *in63 = *off
152200130913     C                   Select
152300130913     C* formato mm/gg/aaaa
152400130913     C                   When      $DatFmt = '*MDY'
152500130913     C     *USA          Move      D2ImpDta      DataISO
152600130913     C* formato aa/mm/gg
152700130913     C                   When      $DatFmt = '*YMD'
152800130913     C     *ISO          Move      D2ImpDta      DataISO
152900130913     C* formato gg/mm/aaaa
153000130913     C                   When      $DatFmt = '*DMY'
153100130913     C     *eur          MoveL     D2ImpDta      DataISO
153200130913     C                   EndSl
153300130913     C                   MoveL     DataISO       WDtImp
153400130913     C                   Else
153500130913     C                   Seton                                          99
153600130913     C                   EndIf
153700130913     C                   EndIf
153800130913     C*
153900130913     C* tipo impegno alla trasmissione
154000130913     C                   Clear                   D2ImpTrsD
154100130913     C                   If        D2ImpTrs      <> *blanks
154200130913     C                   Exsr      CtrImpTrs
154300130913     C                   EndIf
154400130913     C*
154500130913     C                   If        (D2FisInt   =  *blanks or
154600130913     C                              D2ImpDta   =  *zeros  or
154700150204     C                              D2FirInt   =  *blank  or
154800130913     C                              D2ImpTrs   =  *blanks ) and
154900130913     C                             (D2FisInt  <>  *blanks or
155000130913     C                              D2ImpDta  <>  *zeros  or
155100150204     C                              D2FirInt  <>  *blank  or
155200130913     C                              D2ImpTrs  <>  *blanks )
155300130913     C                   Seton                                        6799
155400130913     C                   EndIf
155500130913     C*
155600130913     C                   EndSr
155700130913     C************************************************************
155800130913     C* Controllo dati Soggetto
155900130913     C************************************************************
156000130913     C     CtrSoggetto   BegSr
156100130913     C*
156200130913     C                   If        (D2FisSog   =  *blanks or
156300130913     C                              D2CogSog   =  *blanks or
156400150204     C                              D2NomSog   =  *blanks) and
156500130913     C                             (D2FisSog  <>  *blanks or
156600130913     C                              D2CogSog  <>  *blanks or
156700150204     C                              D2NomSog  <>  *blanks)
156800130913     C                   Seton                                        6999
156900130913     C                   EndIf
157000130913     C*
157100130913     C*
157200131001     C                   If         D2FisSog   <> *blanks and
157300131001     C                              D2CarSog   =  *zeros
157400131001     C                   Seton                                        7299
157500131001     C                   EndIf
157600131001     C*
157700130913     C                   EndSr
157800150204      ************************************************************
157900150204      * Controllo che il numero di telefono sia numerico
158000150204      ************************************************************
158100150204     c     CtrlTel       begsr
158200150204      *
158300150204     C                   eval      X63Codice = D2Telefon
158400150204     C                   eval      X63Len = %len(%trim(D2Telefon))
158500150204     C                   eval      X63Tipo = '2'
158600150204     C                   eval      X63Blank = '0'
158700150204     C                   eval      X63Errore = *Off
158800150204      *
158900150204     C                   call      'X63CTRCHAR'
159000150204     C                   parm                    X63Codice
159100150204     C                   parm                    X63Len
159200150204     C                   parm                    X63Tipo
159300150204     C                   parm                    X63Blank
159400150204     C                   parm                    X63Errore
159500150204      *
159600150204     C                   endsr
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
162001160603R224 C************************************************************
162002160603 "   C* CONTROLLO VIDEATA  D4
162003160603 "   C************************************************************
162004160603R224 C     CtrD4         BegSr
162005160603     C*
162006160603     C                   SETOFF                                       99
162007160603     C* non imposto pos. del cursore
162008160603     C                   Z-Add     0             H1RIGA
162009160603     C                   Z-Add     0             H1COLO
162099160603     C*
162100160603     C* Destinazione dati aggiuntivi
162101160607     C                   EXSR      CtrDestSos
162102160603     C*
162105160609R224 C                   EndSr
162106160603R224 C************************************************************
162107160607 "   C* Controllo destinazione sostituto d'imposta
162108160603 "   C************************************************************
162109160607R224 C     CtrDestSos    BEGSR
162110160603     C*
162111160607     C                   Clear                   D4DestSosD
162112160603     C*
162113160603     C                   Clear                   SDGHC4DS
162114160603     C*
162115160603     C                   Clear                   SDGTABDS
162116160603     C*
162117160607     C                   if        D4DestSos  <> *Blank
162118160603     C*
162119160603     C                   Eval      XTbRic      = '1'
162120160603     C                   Eval      XTbAzi      = XScSoc
162121160603     C                   Eval      XTbKey      = *Zero
162122160603     C                   Eval      XTbCod      = 'HC4'
162123160607     C                   Move      D4DestSos     XTbKey
162124160603     C*
162125160603     C                   CALL      'SDGTABDR'
162126160603     C                   PARM                    SDGTABDS
162127160603     C*
162128160603     C                   if        XTbErr     <> '0'
162129160603     C                   SetOn                                        7699
162130160603     C*
162131160603     C                   else
162132160603     C                   MoveL     XTbUni        SDGHC4DS
162133160607     C                   MoveL     DesHC4        D4DestSosD
162134160603     C*
162135160603     C                   endif
162136160603     C*
162137160603     C                   else
162138160603     C* "Generica"
162139160603     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS1529'
162140160603     C                   EXSR      PrendiMsg
162141160603     C                   if        ERRMSG     <> *ON
162142160607     C                   MoveL     TXTMSG        D4DestSosD
162143160603     C                   else
162144160607     C                   MoveL     *ALL'?'       D4DestSosD
162145160603     C                   endif
162146160603     C*
162147160603     C                   endif
162148160603     C*
162149160603R224 C                   ENDSR
162150160603R224 C/EJECT
162151160603R224 C************************************************************
162152160603 "   C* Reperimento testo messaggi
162153160603 "   C************************************************************
162154160603R224 C     PrendiMsg     BEGSR
162155160603     C*
162156160603     C                   CALLB     'XRTVM'
162157160603     C                   PARM                    IDMSG            27
162158160603     C                   PARM                    TXTMSG          100
162159160603     C                   PARM                    ERRMSG            1
162160160603     C*
162161160603R224 C                   ENDSR
162162160603R224 C/EJECT
162163000000     C************************************************************
162200000000     C* ESECUZIONE LANCIO
162300000000     C************************************************************
162400130809     C     ExeLan        BegSr
162500130809     C*
162600150203     C                   Reset                   SDGSK61DS
162700130809     C*
162800150203     C                   Move      XscSoc        SocietaK61
162900150203     C                   Eval      AnnoK61     = D1Anno
163000150203     C                   Eval      ProtocK61   = D1Protoc
163100150203     C                   Eval      ProtoDK61   = D1Protod
163200150203     C                   Eval      TipInvK61   = D1TipInv
163300150203     C                   Eval      MaxDimK61   = D1MaxDim
163400150203     C                   Eval      MaxRecK61   = D1MaxRec
163500150203     C                   Eval      FileTelK61  = $File
163600150203     C                   Eval      FileK61     = D1File
163700150203     C                   Eval      LibK61      = Lib022
163800110421     C*
163900150203     C                   MoveL     SDGSK61DS     KPJBU
164000000000     C*
164100150203     C                   CALL      'SDGSK61C'
164200000000     C                   PARM                    KPJBA
164300000000     C*
164400150203     C                   MoveL     KPJBU         SDGSK61DS
164500000000     C*
164600000000     C* ritorno da PGM gestione
164700150203     C                   Exsr      RTNK61
164800000000     C*
164900000000     C* aggiorno valori assunti
165000000000     C                   Exsr      AGGMPE
165100000000     C*
165200000000     C                   EndSr
165300000000     C************************************************************
165400000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
165500000000     C************************************************************
165600150203     C     RTNK61        BegSr
165700000000     C*
165800000000     C* funzione non eseguibile per errore :
165900000000     C*
166000000000     C                   Select
166100000000     C* 0 = funzione eseguita correttamente
166200150203     C     ERRK61        WhenEq    '0'
166300000000     C                   Eval      $Fine       = *On
166400000000     C*
166500000000     C*
166600000000     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
166700150203     C     ERRK61        WhenEq    '1'
166800150203     C     ERRK61        OREQ      '2'
166900000000     C                   Seton                                        9399      emiss.SFLMSGID
167000000000     C*
167100000000     C                   EndSl
167200000000     C*
167300000000     C                   EndSr
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
168700000000     C************************************************************
168800000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
168900000000     C************************************************************
169000000000     C     CTRKD2        BegSr
169100000000     C*
169200000000     C                   SETOFF                                       99
169300000000     C                   MoveA     $FC2          $FC
169400000000     C                   Exsr      CTRKEY
169500000000     C     *IN99         IfEq      *ON
169600000000     C                   Seton                                        97
169700000000     C                   EndIf
169800000000     C*
169900000000     C                   EndSr
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
185000000000     C*
185100000000     C                   MoveA     $FM2          $FM
185200000000     C                   MoveA     $Fc2          $Fc
185300000000     C                   Exsr      REPKKK
185400000000     C                   MoveA     $FX           $FX2
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
189500150204     C                   MoveL     KPJBU         SDGSK60DS
189600150204     C*
189700000000     C* se il pgm e stato richiamato imposto i valori ricevuti dalla DS
189800150204     C                   If        SocietaK60 <> *Blanks
189900000000     C                   Eval      CallPgm     = '1'
190000000000     C                   ElSe
190100000000     C                   Eval      CallPgm     = '0'
190200000000     C                   EndIf
190300000000     C*
190400000000     C* Reperimento dati societa
190500000000     C*
190600000000     C                   MoveL     'SOC001'      TIPXSC
190700150204     C                   MoveL     SocietaK60    SOCXSC
190800000000     C                   Exsr      REPSOC
190900000000     C     RTNXSC        IFNE      '1'
191000000000     C                   MoveL     XSOCDS        SOC001
191100000000     C                   ElSe
191200000000     C                   Return
191300000000     C                   EndIf
191301160613R224 C*
191302160613 "   C* DATI IDENTIFICATIVI DEL CONTRIBUENTE
191303160613 "   C*
191304160613 "   C* reperisco la natura giuridica del contribuente
191305160613 "   C                   EXSR      RepNatGiu
191306160613R224 C*
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
194700000000     C                   MoveL     *BLANK        D1TIT
194800000000     C     SOTTITD1      IFNE      *BLANK
194900000000     C                   MoveL     SOTTITD1      IDMSG
195000000000     C                   CALLB     'XRTVM'
195100000000     C                   PARM                    IDMSG            27
195200000000     C                   PARM                    TXTMSG          100
195300000000     C                   PARM                    ERRMSG            1
195400000000     C     ERRMSG        IFNE      *ON
195500000000     C                   MoveL     TXTMSG        D1TIT
195600000000     C                   ElSe
195700000000     C                   MoveL     *ALL'?'       D1TIT
195800000000     C                   EndIf
195900000000     C                   EndIf
196000000000     C*
196100000000     C                   MoveL     *BLANK        T1TIT
196200000000     C     SOTTITD2      IFNE      *BLANK
196300000000     C                   MoveL     SOTTITD2      IDMSG
196400000000     C                   CALLB     'XRTVM'
196500000000     C                   PARM                    IDMSG            27
196600000000     C                   PARM                    TXTMSG          100
196700000000     C                   PARM                    ERRMSG            1
196800000000     C                   PARM      *On           CTRMSG            1            centratura
196900000000     C                   PARM      30            LENMSG            3 0          lun output.
197000000000     C     ERRMSG        IFNE      *ON
197100000000     C                   MoveL     TXTMSG        T1TIT
197200000000     C                   ElSe
197300000000     C                   MoveL     *ALL'?'       T1TIT
197400000000     C                   EndIf
197500000000     C                   EndIf
197600000000     C*
197700000000     C* Reperisco la libreria in cui vengono memorizzati i file
197800000000     C                   Exsr      RepLibFile
197900000000     C*
198000000000     C* Inizializzazione/Decodifica prima videata
198100000000     C*
198200000000     C                   Exsr      InzD1
198300000000     C                   Move      *OFF          $InzD1
198400150205     C                   Exsr      InzD2
198500150205     C                   Exsr      InzD2_2
198600150205     C                   Move      *OFF          $InzD2
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
200301160613R224 C******************************************************
200302160613 "   C* Reperimento della natura giuridica
200303160613 "   C******************************************************
200304160613R224 C     RepNatGiu     BegSr
200305160613     C*
200306160613     C                   Reset                   SDGTABDS
200307160613     C                   Reset                   ANGG16DS
200308160613     C*
200309160613     C                   MOVE      '1'           XTBRIC
200310160613     C                   MOVE      SocietaK60    XTBAZI
200311160613     C                   MOVE      *ZERO         XTBKEY
200312160613     C                   MOVE      'G16'         XTBCOD
200313160613     C                   MOVE      XScNCR        XTBKEY
200314160613     C                   CALL      'SDGTABDR'
200315160613     C                   PARM                    SDGTABDS
200316160613     C                   if        XTBERR      = *Off
200317160613     C                   MoveL     XTBUNI        ANGG16DS
200318160613     C                   else
200319160613     C                   Return
200320160613     C                   endif
200321160613     C*
200322160613R224 C                   ENDSR
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
204100000000     C************************************************************
204200130913     C* Reperimento dati default
204300000000     C************************************************************
204400110811     C     RepDefImp     BegSr
204500000000     C*
204600110811     C                   If        $FisInt  = *blanks
204700150204     C                   MoveL     XscCdf        D2IdProd
204800110811     C                   MoveL     *blanks       D2FisInt
204900110811     C                   Move      *blanks       D2ImpTrs
205000110811     C                   MoveL     *blanks       D2ImpDta
205100150204     C                   MoveL     *blanks       D2FirInt
205200150204     C                   MoveL     *blanks       D2DtFirDic
205300000000     C                   EndIf
205400000000     C*
205500000000     C                   EndSr
205600000000     C************************************************************
205700000000     C* Reperimento libreria per scarico su supporto magnetico
205800000000     C************************************************************
205900000000     C     RepLibFile    BegSr
206000000000     C*
206100000000     C                   Clear                   ANP022DS
206200000000     C*
206300000000     C                   CALLB     'XPAR'
206400000000     C                   Parm                    XScSoc
206500000000     C                   Parm      'SCARSUMA'    XPaPar            8
206600000000     C                   Parm                    XPaOut         2000
206700000000     C                   Parm      *OFF          XPaErr            1
206800000000     C                   Parm      ' '           XPaRic            1
206900000000     C*
207000000000     C                   If        XPaErr      = *On
207100000000     C*
207200000000     C* Errore: fallito reperimento recupero parametro modulo &1
207300000000     C* Valorizzo le variabili del msg PRO2184
207400000000     C                   MoveL     XPaPar        P1Param
207500000000     C                   Seton                                        6399
207600000000     C*
207700000000     C                   ElSe
207800000000     C                   MoveL     XPaOut        ANP022DS
207900000000     C                   EndIf
208000000000     C*
208100000000     C                   EndSr
208200000000     C************************************************************
208300000000     C* Aggiornamento D2
208400000000     C************************************************************
208500000000     C     AggD2         BegSr
208600000000     C*
208700000000     C*
208800000000     C* preparo i parametri di chiamata al driver
208900000000     C                   Clear                   XMPEDS
209000000000     C                   EVAL      TpChiamata = '03'
209100000000     C                   EVAL      NChiavi = 5
209200000000     C                   EVAL      VLogica = 2
209300000000     C*
209400000000     C* preparo le chiavi per la chiamata al driver
209500000000     C                   Clear                   XMPEDS1
209600000000     C                   MoveL     XscSoc        DSMPESOC
209700000000     C                   MoveL     DSPGM         DSMPEPGM
209800140422     C                   Move      'R'           DSMPEPGM
209900000000     C                   MoveL     *blanks       DSMPEPRF
210000000000     C*
210100000000     C* Si memorizzano i dati di default a seconda delle modalita con
210200000000     C* cui il pgm e usato
210300000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
210400000000     C* MPETIP = 'PV'  pgm richiamato da menu
210500000000     C* MPETIP = 'PB'  x lancio Batch
210600000000     C*
210700000000     C                   MoveL     'PB'          DSMPETIP
210800000000     C*
210900130913     C                   Clear                   DSInterm
211000150204     C*
211100130913     C                   Clear                   $ImpDta
211200150204     C                   Clear                   $DtFirDic
211300150205     C* Identificativo produttore software
211400150205     C                   MoveL(P)  D2IdProd      $IdProd
211500130913     C* Intermediario
211600110811     C                   MoveL(P)  D2FisInt      $FisInt
211700110811     C                   Move      D2ImpTrs      $ImpTrs
211800150204     C                   MoveL(P)  D2FirInt      $FirInt
211900110811     C                   MoveL     WDtImp        $ImpDta
212000130913     C* Soggetto
212100130913     C                   MoveL(P)  D2FisSog      $FisSog
212200130913     C                   Move      D2CarSog      $CarSog
212300150204     C                   MoveL     D2CogSog      $CogSog
212400150204     C                   MoveL     D2NomSog      $NomSog
212500150204     C                   MoveL(P)  D2FisDic      $FisDic
212600130913     C*
212700150204     C                   MoveL     WDtFirDic     $DtFirDic
212800150204     C                   MoveL(P)  D2FirDic      $FirDic
212900130913     C*
213000110811     C                   MoveL     DSInterm      DSMPEPAR
213100000000     C*
213200000000     C                   CALLB     'XMPE'
213300000000     C                   PARM                    XMPEDS
213400000000     C                   PARM                    XMPEDS1
213500000000     C*
213600000000     C                   EndSr
213700150204     C************************************************************
213800150204     C* Aggiornamento D2
213900150204     C************************************************************
214000150204     C     AggD2_2       BegSr
214100150204     C*
214200150204     C*
214300150204     C* preparo i parametri di chiamata al driver
214400150204     C                   Clear                   XMPEDS
214500150204     C                   EVAL      TpChiamata = '03'
214600150204     C                   EVAL      NChiavi = 5
214700150204     C                   EVAL      VLogica = 2
214800150204     C*
214900150204     C* preparo le chiavi per la chiamata al driver
215000150204     C                   Clear                   XMPEDS1
215100150204     C                   MoveL     XscSoc        DSMPESOC
215200150204     C                   MoveL     DSPGM         DSMPEPGM
215300150204     C                   Move      'S'           DSMPEPGM
215400150204     C                   MoveL     *blanks       DSMPEPRF
215500150204     C*
215600150204     C* Si memorizzano i dati di default a seconda delle modalita con
215700150204     C* cui il pgm e usato
215800150204     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
215900150204     C* MPETIP = 'PV'  pgm richiamato da menu
216000150204     C* MPETIP = 'PB'  x lancio Batch
216100150204     C*
216200150204     C                   MoveL     'PB'          DSMPETIP
216300150204     C* Dati in recupero
216400150204     C*
216500150204     C                   Clear                   DSInterm2
216600150204     C*
216700150204     C                   MoveL     D2TELEFON     $TELEFON
216800150204     C                   MoveL     D2EMAIL       $EMAIL
216900150204     C*
217000150204     C                   MoveL     DSInterm2     DSMPEPAR
217100150204     C*
217200150204     C                   CALLB     'XMPE'
217300150204     C                   PARM                    XMPEDS
217400150204     C                   PARM                    XMPEDS1
217500150204     C*
217600150204     C                   EndSr
217601160603R224 C************************************************************
217602160603 "   C* Aggiornamento D4
217603160603 "   C************************************************************
217604160603R224 C     AggD4         BegSr
217605160603     C*
217606160603     C*
217607160603     C* preparo i parametri di chiamata al driver
217608160603     C                   Clear                   XMPEDS
217609160603     C                   EVAL      TpChiamata = '03'
217610160603     C                   EVAL      NChiavi = 5
217611160603     C                   EVAL      VLogica = 2
217612160603     C*
217613160603     C* preparo le chiavi per la chiamata al driver
217614160603     C                   Clear                   XMPEDS1
217615160603     C                   MoveL     XscSoc        DSMPESOC
217616160609     C                   MoveL     'SDGSK60R C'  DSMPEPGM
217618160603     C                   MoveL     *blanks       DSMPEPRF
217619160603     C*
217620160603     C* Si memorizzano i dati di default a seconda delle modalita con
217621160603     C* cui il pgm e usato
217622160603     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
217623160603     C* MPETIP = 'PV'  pgm richiamato da menu
217624160603     C* MPETIP = 'PB'  x lancio Batch
217625160603     C*
217626160603     C                   MoveL     'PB'          DSMPETIP
217627160603     C* Dati in recupero
217628160603     C*
217629160607     C                   Clear                   DSSostit
217630160603     C*
217631160607     C                   MoveL     D4DestSos     $DestSos
217643160603     C*
217644160607     C                   MoveL     DSSostit      DSMPEPAR
217645160603     C*
217646160603     C                   CALLB     'XMPE'
217647160603     C                   PARM                    XMPEDS
217648160603     C                   PARM                    XMPEDS1
217649160603     C*
217650160603R224 C                   EndSr
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
220400130809     C                   Eval      $TipInv     = D1TipInv
220500130809     C                   Z-Add     D1MaxDim      $MaxDim
220600130809     C                   Z-Add     D1MaxRec      $MaxRec
220700130809     C                   MoveL     D1File        $File
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
223100000000     C*
223200000000     C* klist
223300150306     C*
223400150306     C* ND77019L
223500150306     C     K0377019      KLIST
223600150306     C                   KFLD                    N77Societa
223700150306     C                   KFLD                    N77Acf
223800150306     C                   KFLD                    N77Protoc
223900150306     C     K0477019      KLIST
224000150306     C                   KFLD                    N77Societa
224100150306     C                   KFLD                    N77Acf
224200150306     C                   KFLD                    N77Protoc
224300150306     C                   KFLD                    N77ProtCea
224400000000     C*
224500000000     C* Variabili per gestione videate
224600000000     C*
224700000000     C                   Move      *BLANK        $GEST             2
224800000000     C                   Move      *BLANK        $FINE             1
224900000000     C                   Move      *BLANK        $InzD1            1
225000000000     C                   Move      *BLANK        $InzD2            1
225100000000     C                   Move      *BLANK        $INZD3            1
225101160603R224 C                   Move      *BLANK        $INZD4            1
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
231801160607SDGMSG    *LIBL     SDK0023  19        ## R224
232000130916SDGMSG    *LIBL     SDK0017  20
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
2344001606031N  19         ## R224
2345000000001N  20
2346000000000N  21
2347000000000N  22
2348000000000N  23   Impostare '1' se si usano le opzioni
2349000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
235000000000** TASTI DI FUNZIONE  D2
235100000000PROMSG    *LIBL     KEY0001  01
235200000000PROMSG    *LIBL     KEY0002  02
235300000000PROMSG    *LIBL     KEY0003  03
235400000000PROMSG    *LIBL     KEY0004  04
235500000000PROMSG    *LIBL     KEY0005  05
235600000000PROMSG    *LIBL     KEY0006  06
235700000000PROMSG    *LIBL     KEY0007  07
235800000000PROMSG    *LIBL     KEY0008  08
235900000000PROMSG    *LIBL     KEY0009  09
236000000000PROMSG    *LIBL     KEY0010  10
236100000000PROMSG    *LIBL     KEY0011  11
236200000000PROMSG    *LIBL     KEY0012  12
236300000000PROMSG    *LIBL     KEY0013  13
236400000000PROMSG    *LIBL     KEY0014  14
236500000000PROMSG    *LIBL     KEY0015  15
236600000000PROMSG    *LIBL     KEY0016  16
236700000000PROMSG    *LIBL     KEY0017  17
236800000000PROMSG    *LIBL     KEY0018  18
236900000000PROMSG    *LIBL     KEY0019  19
237000000000PROMSG    *LIBL     KEY0020  20
237100000000PROMSG    *LIBL     KEY0021  21
237200000000PROMSG    *LIBL     KEY0022  22
237300000000PROMSG    *LIBL     KEY0023  23
237400000000PROMSG    *LIBL     KEY0024  24
237500000000**  ABILITAZIONE D2
2376000000000N  01
2377000000000N  02
2378000000001N  03
2379001108111N  04
2380000000001N  05
2381000000001N  06
2382000000000N  07
2383000000000N  08
2384000000000N  09
2385000000000N  10
2386000000000N  11
2387000000001N  12
2388000000000N  13
2389000000000N  14
2390000000000N  15
2391000000000N  16
2392000000000N  17
2393000000000N  18
2394000000000N  19
2395000000000N  20
2396000000000N  21
2397000000000N  22
2398000000000N  23   Impostare '1' se si usano le opzioni
2399000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
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
