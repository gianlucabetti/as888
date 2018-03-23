000100000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(QILE)
000200000000     H*PARMS BNDDIR(PJXBND PJCBND) CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500000000      *****************************************************************
000600000000      *
000700000000      *         ------------------------------------------------
000800170711      *                      Spesometro Trimestrale
000900170717      *                 Lancio aggiornamento stato record
001000000000      *         ------------------------------------------------
001100170508      *
001200170711R258  * Questa PGM è stato creato con il task R258
001300000000      *
001400000000      *****************************************************************
001500000000     F*----------------------------------------------------*
001600170717     FSDGSI80D  CF   E             WORKSTN
001700170717     F                                     RENAME(SI80T1:FMTT1)
001800170717     F                                     RENAME(SI80D1:FMTD1)
001900170717     F                                     RENAME(SI80Z1:FMTZ1)
002000170717     F                                     RENAME(SI80Z2:FMTZ2)
002100000000     F                                     INFDS(DSFMT)
002200170717     F*----------------------------------------------------------------
002300170717     F* associazione libri/riferim-aliquote
002400170717     FSDGCSF01L IF   E           K DISK
002500000000     F*------------
002600000000     D*----------------------------------------------------*
002700000000     D* Schiere per la gestione dei tasti di funzione
002800000000     D $FM             S             27    DIM(24)
002900000000     D $FX             S             50    DIM(24)
003000000000     D $FC             S              1    DIM(24)
003100000000     D $FR             S              1    DIM(24)
003200170512     D* Video D1/D2/D4
003300000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003400000000     D $FX1            S             50    DIM(24) ALT($FM1)
003500000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003600000000     D $FR1            S              1    DIM(24) ALT($FC1)
003700170512     D* Video D1/D2/D4
003800000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
003900000000     D $FX2            S             50    DIM(24) ALT($FM2)
004000000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
004100000000     D $FR2            S              1    DIM(24) ALT($FC2)
004200000000     D* Video D3 Valori assunti
004300000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
004400000000     D $FX3            S             50    DIM(24) ALT($FM3)
004500000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
004600000000     D $FR3            S              1    DIM(24) ALT($FC3)
004700000000     D*----------------------------------------------------*
004800000000     D*-------------
004900000000     D* Passaggio Parametri
005000000000     D KPJBA         E DS
005100170508     D*-------------
005200170508     D* Reperimento nome PGM
005300170508     D STATUS         SDS           333
005400170508     D  DSPGM            *PROC
005500170508     D  PARMS            *PARMS
005600170508     D*-------------
005700170508     D* Dati di ambiente ottenuti da SDGSOCDR
005800170508     D SOC001        E DS                  EXTNAME(SDGSOCDS )
005900170508     D*-------------
006000170508     D* DS Interna per dati di output di SDGSOCDR
006100170508     D XSOCDS          DS          1000
006200000000     D*-------------
006300000000     D* DS Esterna gestione messaggi
006400000000     D SDGMSGDS      E DS                  INZ
006500000000     D  STS          E                     EXTFLD(MSGSTS)
006600000000     D                                     DIM(333)
006700000000     D  JBA          E                     EXTFLD(MSGJBA)
006800000000     D                                     DIM(502)
006900110421     D*-------------
007000110421     D* Ricerca legende
007100110421     D Ana964DS      E DS
007200170508     D*-------------
007300170508     D X07VALDS      E DS
007400170505     D*-------------
007500000000     D* Valori assunti
007600000000     D XMPEDS        E DS                  INZ
007700000000     D XMPEDS1       E DS                  INZ
007800000000     D DSMPE           DS                  INZ
007900000000     D  $VAL                          1
008000170717     D  $Anno                         4S 0
008100000000     D*-------------
008200000000     D*-------------
008300000000     D* Ds per ricezione parametri
008400170717     D SDGSI80DS     E DS
008500170717     D  ERRI80       E                     INZ('0')
008600000000     D*-------------
008700000000     D* Passaggio Parametri al PGM di estrazione dati
008800170717     D SDGSI81DS     E DS
008900170717     D  ERRI81       E                     INZ('0')
009000170719     D*-------------
009100170719     D* Passaggio Parametri al PGM di estrazione dati
009200170719     D SDGSI82DS     E DS
009300170719     D  ERRI82       E                     INZ('0')
009400000000     D*-------------
009500000000     D DSFMT           DS
009600000000     D  $Tasto               369    369
009700000000     D  NRG                  370    370
009800000000     D  NCL                  371    371
009900000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
010000000000     D*-------------
010100000000     D* posizione cursore
010200000000     D CURSOR          DS
010300000000     D  RIRI                   1      2B 0 INZ
010400000000     D  R$$                    2      2
010500000000     D  COCO                   3      4B 0 INZ
010600000000     D  C$$                    4      4
010700000000     D*-------------
010800000000     D* Tasti di funzione
010900000000     D F01             C                   CONST(X'31')
011000000000     D F02             C                   CONST(X'32')
011100000000     D F03             C                   CONST(X'33')
011200000000     D F04             C                   CONST(X'34')
011300000000     D F05             C                   CONST(X'35')
011400000000     D F06             C                   CONST(X'36')
011500000000     D F07             C                   CONST(X'37')
011600000000     D F08             C                   CONST(X'38')
011700000000     D F09             C                   CONST(X'39')
011800000000     D F10             C                   CONST(X'3A')
011900000000     D F11             C                   CONST(X'3B')
012000000000     D F12             C                   CONST(X'3C')
012100000000     D F13             C                   CONST(X'B1')
012200000000     D F14             C                   CONST(X'B2')
012300000000     D F15             C                   CONST(X'B3')
012400000000     D F16             C                   CONST(X'B4')
012500000000     D F17             C                   CONST(X'B5')
012600000000     D F18             C                   CONST(X'B6')
012700000000     D F19             C                   CONST(X'B7')
012800000000     D F20             C                   CONST(X'B8')
012900000000     D F21             C                   CONST(X'B9')
013000000000     D F22             C                   CONST(X'BA')
013100000000     D F23             C                   CONST(X'BB')
013200000000     D F24             C                   CONST(X'BC')
013300000000     D ENTER           C                   CONST(X'F1')
013400000000     D ROLDWN          C                   CONST(X'F4')
013500000000     D ROLLUP          C                   CONST(X'F5')
013600000000     I*-------------
013700000000     I* Sottotitolo di testata
013800000000     I* se non si hanno sottotitoli, mettere tutta la costante = ' '
013900170505     D* Creazione
014000170804     D SOTTITD1        C                   CONST('               -
014100170804     D                                                 ')
014200170505     D* File per invio telematico
014300000000     D SOTTITD2        C                   CONST('SDGMSG    *LIBL-
014400170804     D                                          SDC0116')
014500000000     D*-----------
014600000000     D* Campi di lavoro
014700000000     D CallPgm         S              1
014800170508     D WDtImp4         S               D
014900170508     D WDtIso          S               D
015000130904     C*----------------------------------------------------*
015100130904     C*                MAIN LINE PROGRAM
015200130904     C*----------------------------------------------------*
015300130904     C*
015400170505     C                   EXSR      INZVAR
015500130904     C*
015600000000     C     $FINE         DOWEQ     *OFF
015700000000     C     $GEST         CASEQ     'D1'          GESD1
015800000000     C                   ENDCS
015900000000     C                   ENDDO
016000000000     C*
016100170505     C                   EXSR      ENDPGM
016200170508     C/EJECT
016300000000     C************************************************************
016400000000     C* RICERCHE
016500000000     C************************************************************
016600170505     C     ENDPGM        BEGSR
016700000000     C*
016800000000     C* se ho confermato l'uscita proseguo
016900000000     C     $Fine         IfEq      *ON
017000000000     C*
017100000000     C                   Seton                                            LR
017200000000     C                   Return
017300000000     C*
017400000000     C                   EndIf
017500000000     C*
017600170505     C                   ENDSR
017700170508     C/EJECT
017800000000     C************************************************************
017900000000     C* RICERCHE
018000000000     C************************************************************
018100170505     C     Search        BEGSR
018200000000     C*
018300000000     C* determino Riga/Colonna del cursore
018400000000     C                   Move      NRG           R$$
018500000000     C                   Move      NCL           C$$
018600000000     C                   Z-Add     RIRI          CURR
018700000000     C                   Z-Add     COCO          CURC
018800000000     C*
018900000000     C* I campi che al momento dell'F4 risultano protetti non devono
019000000000     C* essere abilitati alla funzione di ricerca
019100000000     C*
019200000000     C                   Select
019300170508     C*
019400170717     C     H1NMFL        WHENEQ    'D1FILE'
019500170807     C     H1NMFL        OREQ      'D1IDFILE'
019600170717     C                   EXSR      F4_File
019700170711     C*
019800170719     C                   Clear                   KPJBU
019900170719     C                   Movel     SDGSI82DS     KPJBU
020000170719     C                   Call      'SDGSI82R'
020100170719     C                   Parm                    KPJBA
020200170719     C                   Movel     KPJBU         SDGSI82DS
020300170719     C*
020400170719     C                   if        FileI82  <>   *Blanks
020500170719     C                   MoveL     FileI82       D1File
020600170719     C                   endif
020700170807     C*
020800170807     C                   if        IdFileI82  <> *Blanks
020900170807     C                   MoveL     IdFileI82     D1IdFile
021000170807     C                   endif
021100170719     C*
021200130809     C* ricerca non abilitata in questa posizione
021300130809     C                   Other
021400130809     C                   Seton                                        9899
021500130809     C*
021600110421     C                   EndSl
021700110421     C*
021800000000     C* imposto pos. del cursore
021900000000     C                   Z-Add     CURR          H1RIGA
022000000000     C                   Z-Add     CURC          H1COLO
022100000000     C*
022200000000     C                   Move      *HIVAL        $LastV
022300000000     C*
022400170505     C                   ENDSR
022500170508     C/EJECT
022600170503     C************************************************************
022700170717     C* Ricerca file telematico
022800170503     C************************************************************
022900170717     C     F4_File       BEGSR
023000170503     C*
023100170719     C* FMTD1  - file su supporto magnetico
023200170719     C                   Reset                   SDGSI82DS
023300170719     C                   Z-ADD     0             TrimI82
023400170719     C                   Z-ADD     0             AnnoI82
023500170719     C                   Move      '01'          OpzI82
023600170719     C                   Move      XScSoc        SocietaI82
023700170807     C                   Movel     D1File        FileI82
023800170807     C                   Movel     D1IdFile      IdFileI82
023900170503     C*
024000170505     C                   ENDSR
024100170508     C/EJECT
024200110421     C************************************************************
024300110421     C* GESTIONE EMISSIONE TASTI FUNZIONE
024400110421     C************************************************************
024500170505     C     GesPie        BEGSR
024600110421     C*
024700110421     C                   Select
024800110421     C*
024900110421     C     $GEST         WhenEq    'D1'
025000110421     C     $ULKD1        IfEq      0
025100110421     C* riempimento opzioni
025200170505     C                   EXSR      F24D1
025300000000     C                   ElSe
025400000000     C                   Write     FMTZ1
025500000000     C                   EndIf
025600000000     C*
025700000000     C                   EndSl
025800000000     C*
025900170505     C                   ENDSR
026000170508     C/EJECT
026100000000     C************************************************************
026200000000     C* GESTIONE VIDEO RECORD D1
026300000000     C************************************************************
026400170505     C     GesD1         BEGSR
026500170505     C*
026600000000     C* inizializzo videata
026700000000     C     $InzD1        IfEq      *ON
026800170505     C                   EXSR      InzD1
026900000000     C                   Move      *OFF          $InzD1
027000000000     C                   EndIf
027100000000     C* emissione piede videata se proveniente da altra
027200000000     C* salvataggio valore formato di provenienza
027300000000     C     $LastV        IFNE      'D1'
027400000000     C                   Write     FMTT1
027500170505     C                   EXSR      GESPIE
027600000000     C                   Move      $LastV        $LastG
027700000000     C                   Move      'D1'          $LastV
027800000000     C                   EndIf
027900000000     C*
028000000000     C* se esistono errori sulla videata
028100000000     C* emetto la Write del formato a indicatori spenti per vedere
028200000000     C* le eventuali decodifiche
028300000000     C     *IN99         IfEq      *ON
028400000000     C     $GEST         ANDEQ     'D1'
028500000000     C                   MoveA     *IN           WIN
028600000000     C                   Move      *ALL'0'       IN5098           49
028700000000     C                   MoveA     IN5098        *IN(50)
028800000000     C                   Write     FMTD1
028900000000     C                   MoveA     WIN           *IN
029000000000     C                   EndIf
029100000000     C*
029200000000     C                   EXFMT     FMTD1
029300000000     C*
029400000000     C* controllo abilitazione tasti funzione
029500170505     C                   EXSR      CTRKD1
029600000000     C*
029700000000     C     *IN99         IfEq      *OFF
029800000000     C*
029900000000     C                   Select
030000000000     C* F2=Decodifica
030100000000     C     $Tasto        WhenEq    F02
030200170505     C                   EXSR      F02D1
030300000000     C* F3=Fine
030400000000     C     $Tasto        WhenEq    F03
030500170505     C                   EXSR      F03D1
030600000000     C* F4=Ricerca
030700000000     C     $Tasto        WhenEq    F04
030800170505     C                   EXSR      SEARCH
030900000000     C* F5=Refresh
031000000000     C     $Tasto        WhenEq    F05
031100170505     C                   EXSR      F05D1
031200000000     C* F8=Successivo
031300000000     C     $Tasto        WhenEq    F08
031400170505     C                   EXSR      F08D1
031500000000     C* F12=Ritorno
031600000000     C     $Tasto        WhenEq    F12
031700170505     C                   EXSR      F12D1
031800000000     C* F21=Pers. batch
031900000000     C     $Tasto        WhenEq    F21
032000170505     C                   EXSR      F21D1
032100000000     C* F24=Altri tasti
032200000000     C     $Tasto        WhenEq    F24
032300170505     C                   EXSR      F24D1
032400000000     C*
032500000000     C                   Other
032600170505     C                   EXSR      CTRD1
032700000000     C*
032800000000     C     $Tasto        IfEq      F06
032900000000     C     *IN99         ANDEQ     *OFF
033000000000     C*
033100170505     C                   EXSR      ExeLan
033200170505     C                   EXSR      ENDPGM
033300000000     C*
033400000000     C                   EndIf
033500000000     C*
033600000000     C                   EndSl
033700000000     C*
033800000000     C                   EndIf
033900000000     C*
034000170505     C                   ENDSR
034100170508     C/EJECT
034200000000     C************************************************************
034300000000     C* INIZIALIZZAZIONE VIDEATA DATI
034400000000     C************************************************************
034500170505     C     InzD1         BEGSR
034600000000     C*
034700000000     C                   Clear                   FMTD1
034800000000     C*
034900000000     C                   Z-Add     1             $PIED1
035000000000     C*
035100130809     C*
035200170505     C                   ENDSR
035300170508     C/EJECT
035400130809     C************************************************************
035500130809     C* RIEMPIMENTO VIDEATA  D1
035600130809     C************************************************************
035700170505     C     RIED1         BEGSR
035800130809     C*
035900130809     C* impostare i campi della videata dai parametri memorizzati su anmpe
036000130809     C*
036100170505     C                   ENDSR
036200170508     C/EJECT
036300130809     C************************************************************
036400130809     C* GESTIONE F02 VIDEO D1
036500130809     C************************************************************
036600170505     C     F02D1         BEGSR
036700130809     C*
036800130809     C* per decodificare la videata richiamo la routine dei ctrl
036900130809     C* e poi disattivo gli eventuali messaggi di errore
037000170505     C                   EXSR      CTRD1
037100130809     C*
037200130809     C                   Move      *ALL'0'       IN5098
037300130809     C                   MoveA     IN5098        *IN(50)
037400130809     C*
037500170505     C                   ENDSR
037600170508     C/EJECT
037700000000     C************************************************************
037800000000     C* GESTIONE F03 VIDEO D1
037900000000     C************************************************************
038000170505     C     F03D1         BEGSR
038100000000     C*
038200000000     C                   Move      *ON           $FINE
038300000000     C* fine programma
038400170505     C                   EXSR      ENDPGM
038500000000     C*
038600170505     C                   ENDSR
038700170508     C/EJECT
038800000000     C************************************************************
038900000000     C* GESTIONE F05 VIDEO D1
039000000000     C************************************************************
039100170505     C     F05D1         BEGSR
039200000000     C*
039300000000     C                   Move      *ON           $InzD1
039400000000     C*
039500170505     C                   ENDSR
039600170508     C/EJECT
039700000000     C************************************************************
039800000000     C* GESTIONE F08 VIDEO D1
039900000000     C************************************************************
040000170505     C     F08D1         BEGSR
040100000000     C*
040200000000     C                   Move      *ON           $FINE
040300000000     C*
040400170505     C                   ENDSR
040500170508     C/EJECT
040600000000     C************************************************************
040700000000     C* GESTIONE F12 VIDEO D1
040800000000     C************************************************************
040900170505     C     F12D1         BEGSR
041000000000     C*
041100000000     C                   Move      *ON           $FINE
041200000000     C*
041300170505     C                   ENDSR
041400170508     C/EJECT
041500000000     C************************************************************
041600000000     C* GESTIONE F21 VIDEO D1
041700000000     C************************************************************
041800170505     C     F21D1         BEGSR
041900000000     C*
042000000000     C                   Move      '    '        KCOAZ
042100000000     C                   CALL      'BCH09'
042200000000     C                   PARM                    KPJBA
042300000000     C*
042400170505     C                   ENDSR
042500170508     C/EJECT
042600000000     C************************************************************
042700000000     C* GESTIONE F24 VIDEO D1
042800000000     C************************************************************
042900170505     C     F24D1         BEGSR
043000000000     C*
043100000000     C                   MoveA     $FX1          $FX
043200000000     C                   MoveA     $FC1          $FC
043300000000     C                   MoveA     $FR1          $FR
043400000000     C                   Z-Add     $ULKD1        $ULK
043500000000     C                   Z-Add     79            $L01
043600000000     C                   Z-Add     79            $L02
043700170505     C                   EXSR      RIEKEY
043800000000     C                   Z-Add     $ULK          $ULKD1            3 0
043900000000     C                   MoveA     $FC           $FC1
044000000000     C                   MoveL     $KEY1         Z1KE1
044100000000     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
044200000000     C                   MoveL     $KEY2         Z1KE2
044300000000     C                   Write     FMTZ1
044400000000     C*
044500170505     C                   ENDSR
044600170508     C/EJECT
044700000000     C************************************************************
044800000000     C* CONTROLLO VIDEATA
044900000000     C************************************************************
045000170505     C     CtrD1         BEGSR
045100000000     C*
045200000000     C                   SETOFF                                       99
045300130809     C* non imposto pos. del cursore
045400130809     C                   Z-Add     0             H1RIGA
045500130809     C                   Z-Add     0             H1COLO
045600130809     C*
045700170717     C                   EXSR      CtrFile
045800170711     C*
045900170711     C* id file e posizione
046000170711     C                   Exsr      CtrIdFile
046100170515     C*
046200170505     C                   ENDSR
046300170508     C/EJECT
046400000000     C***********************************************************
046500170717     C* Controllo file
046600000000     C***********************************************************
046700170717     C     CtrFile       BEGSR
046800170505     C*
046900170717     C                   If        D1File      = *blanks
047000000000     C                   Seton                                        5099
047100170717     C*
047200170717     C                   ElSe
047300170717     C*
047400170717     C                   Eval      CSFSocieta  = XscSoc
047500170717     C                   Eval      CSFFile     = D1File
047600170717     C     K02CSF01      CHAIN     SDGCSF01L                          21
047700170717     C                   If        *In21       = *On
047800170717     C                   SetOn                                        5199
047900170719     C                   ElSe
048000170719     C                   Eval      D1DescrF    = CSFNote
048100170719     C                   Exsr      F4_TpRec
048200170807     C*
048300170807     C* Id File già assegnato
048400170807     C                   if        CSFIdFile <> *blank
048500170807     C                   seton                                        6299
048600170807     C                   endif
048700170807     C*
048800170719     C                   EndIf
048900170717     C                   EndIf
049000000000     C*
049100170505     C                   ENDSR
049200170508     C/EJECT
049300170711     C***********************************************************
049400170711     C* Controllo id file
049500170711     C***********************************************************
049600170711     C     CtrIdFile     BEGSR
049700170711     C*
049800170711     C* ITAAABBB99T99X999W_DF_00001
049900170711     C* Id File se richiesta rettifica o annullamento
050000170717     C                   If        D1IdFile   = *blank
050100170711     C                   SetOn                                        6199
050200170711     C                   EndIf
050300170711     C*
050400170711     C                   EndSr
050500000000     C************************************************************
050600000000     C* ESECUZIONE LANCIO
050700000000     C************************************************************
050800170505     C     ExeLan        BEGSR
050900170508     C*
051000170508     C* aggiorno valori assunti
051100170717     C****               EXSR      AGGMPE
051200130809     C*
051300170717     C                   Reset                   SDGSI81DS
051400130809     C*
051500170717     C                   Move      XscSoc        SocietaI81
051600170717     C                   Eval      TpComI81    = CSFTPCOM
051700170717     C                   Eval      TpRecI81    = CSFTPREC
051800170717     C                   Eval      AnnoI81     = CSFAnno
051900170717     C                   Eval      TrimI81     = CSFTrim
052000170717     C                   Eval      MeseI81     = CSFMese
052100170717     C                   Eval      FileI81     = D1File
052200170717     C                   Eval      IdFileI81   = D1IdFile
052300110421     C*
052400170717     C                   MoveL     SDGSI81DS     KPJBU
052500000000     C*
052600170717     C                   CALL      'SDGSI81R'
052700000000     C                   PARM                    KPJBA
052800000000     C*
052900170717     C                   MoveL     KPJBU         SDGSI81DS
053000000000     C*
053100000000     C* ritorno da PGM gestione
053200170717     C                   EXSR      RTNI81
053300000000     C*
053400170505     C                   ENDSR
053500170508     C/EJECT
053600000000     C************************************************************
053700000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
053800000000     C************************************************************
053900170717     C     RTNI81        BEGSR
054000000000     C*
054100000000     C* funzione non eseguibile per errore :
054200000000     C*
054300000000     C                   Select
054400000000     C* 0 = funzione eseguita correttamente
054500170717     C     ERRI81        WhenEq    '0'
054600000000     C                   Eval      $Fine       = *On
054700000000     C*
054800000000     C*
054900000000     C* 1 = funzione richiamata chiusa in errore o con avvertimenti
055000170717     C     ERRI81        WhenEq    '1'
055100170717     C     ERRI81        OREQ      '2'
055200000000     C                   Seton                                        9399      emiss.SFLMSGID
055300000000     C*
055400000000     C                   EndSl
055500000000     C*
055600170505     C                   ENDSR
055700170508     C/EJECT
055800000000     C************************************************************
055900000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
056000000000     C************************************************************
056100170505     C     CTRKD1        BEGSR
056200000000     C*
056300000000     C                   SETOFF                                       99
056400000000     C                   MoveA     $FC1          $FC
056500170505     C                   EXSR      CTRKEY
056600000000     C     *IN99         IfEq      *ON
056700000000     C                   Seton                                        97
056800000000     C                   EndIf
056900000000     C*
057000170505     C                   ENDSR
057100170508     C/EJECT
057200000000     C************************************************************
057300000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
057400000000     C************************************************************
057500170505     C     CTRKD2        BEGSR
057600000000     C*
057700000000     C                   SETOFF                                       99
057800000000     C                   MoveA     $FC2          $FC
057900170505     C                   EXSR      CTRKEY
058000000000     C     *IN99         IfEq      *ON
058100000000     C                   Seton                                        97
058200000000     C                   EndIf
058300000000     C*
058400170505     C                   ENDSR
058500170508     C/EJECT
058600000000     C************************************************************
058700000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D3
058800000000     C* INPUT
058900000000     C* $FC3 = Abilitazioni videata D3
059000000000     C* OUTPUT
059100000000     C* *IN97 *IN99 Errori
059200000000     C************************************************************
059300170505     C     CTRKD3        BEGSR
059400000000     C*
059500000000     C                   SETOFF                                       99
059600000000     C                   MoveA     $FC3          $FC
059700170505     C                   EXSR      CTRKEY
059800000000     C     *IN99         IfEq      *ON
059900000000     C                   Seton                                        97
060000000000     C                   EndIf
060100000000     C*
060200170505     C                   ENDSR
060300170508     C/EJECT
060400000000     C************************************************************
060500000000     C* CONTROLLO ABILITAZIONE TASTI
060600000000     C************************************************************
060700170505     C     CTRKEY        BEGSR
060800000000     C*
060900000000     C                   Select
061000000000     C     $Tasto        WhenEq    F01
061100000000     C     $FC(1)        IfEq      '0'
061200000000     C                   Seton                                        99
061300000000     C                   EndIf
061400000000     C     $Tasto        WhenEq    F02
061500000000     C     $FC(2)        IfEq      '0'
061600000000     C                   Seton                                        99
061700000000     C                   EndIf
061800000000     C     $Tasto        WhenEq    F03
061900000000     C     $FC(3)        IfEq      '0'
062000000000     C                   Seton                                        99
062100000000     C                   EndIf
062200000000     C     $Tasto        WhenEq    F04
062300000000     C     $FC(4)        IfEq      '0'
062400000000     C                   Seton                                        99
062500000000     C                   EndIf
062600000000     C     $Tasto        WhenEq    F05
062700000000     C     $FC(5)        IfEq      '0'
062800000000     C                   Seton                                        99
062900000000     C                   EndIf
063000000000     C     $Tasto        WhenEq    F06
063100000000     C     $FC(6)        IfEq      '0'
063200000000     C                   Seton                                        99
063300000000     C                   EndIf
063400000000     C     $Tasto        WhenEq    F07
063500000000     C     $FC(7)        IfEq      '0'
063600000000     C                   Seton                                        99
063700000000     C                   EndIf
063800000000     C     $Tasto        WhenEq    F08
063900000000     C     $FC(8)        IfEq      '0'
064000000000     C                   Seton                                        99
064100000000     C                   EndIf
064200000000     C     $Tasto        WhenEq    F09
064300000000     C     $FC(09)       IfEq      '0'
064400000000     C                   Seton                                        99
064500000000     C                   EndIf
064600000000     C     $Tasto        WhenEq    F10
064700000000     C     $FC(10)       IfEq      '0'
064800000000     C                   Seton                                        99
064900000000     C                   EndIf
065000000000     C     $Tasto        WhenEq    F11
065100000000     C     $FC(11)       IfEq      '0'
065200000000     C                   Seton                                        99
065300000000     C                   EndIf
065400000000     C     $Tasto        WhenEq    F12
065500000000     C     $FC(12)       IfEq      '0'
065600000000     C                   Seton                                        99
065700000000     C                   EndIf
065800000000     C     $Tasto        WhenEq    F13
065900000000     C     $FC(13)       IfEq      '0'
066000000000     C                   Seton                                        99
066100000000     C                   EndIf
066200000000     C     $Tasto        WhenEq    F14
066300000000     C     $FC(14)       IfEq      '0'
066400000000     C                   Seton                                        99
066500000000     C                   EndIf
066600000000     C     $Tasto        WhenEq    F15
066700000000     C     $FC(15)       IfEq      '0'
066800000000     C                   Seton                                        99
066900000000     C                   EndIf
067000000000     C     $Tasto        WhenEq    F16
067100000000     C     $FC(16)       IfEq      '0'
067200000000     C                   Seton                                        99
067300000000     C                   EndIf
067400000000     C     $Tasto        WhenEq    F17
067500000000     C     $FC(17)       IfEq      '0'
067600000000     C                   Seton                                        99
067700000000     C                   EndIf
067800000000     C     $Tasto        WhenEq    F18
067900000000     C     $FC(18)       IfEq      '0'
068000000000     C                   Seton                                        99
068100000000     C                   EndIf
068200000000     C     $Tasto        WhenEq    F19
068300000000     C     $FC(19)       IfEq      '0'
068400000000     C                   Seton                                        99
068500000000     C                   EndIf
068600000000     C     $Tasto        WhenEq    F20
068700000000     C     $FC(20)       IfEq      '0'
068800000000     C                   Seton                                        99
068900000000     C                   EndIf
069000000000     C     $Tasto        WhenEq    F21
069100000000     C     $FC(21)       IfEq      '0'
069200000000     C                   Seton                                        99
069300000000     C                   EndIf
069400000000     C     $Tasto        WhenEq    F22
069500000000     C     $FC(22)       IfEq      '0'
069600000000     C                   Seton                                        99
069700000000     C                   EndIf
069800000000     C     $Tasto        WhenEq    F23
069900000000     C     $FC(23)       IfEq      '0'
070000000000     C                   Seton                                        99
070100000000     C                   EndIf
070200000000     C     $Tasto        WhenEq    F24
070300000000     C     $FC(24)       IfEq      '0'
070400000000     C                   Seton                                        99
070500000000     C                   EndIf
070600000000     C                   EndSl
070700000000     C*
070800170505     C                   ENDSR
070900170508     C/EJECT
071000000000     C************************************************************
071100000000     C* OPERAZIONI INIZIALI
071200000000     C************************************************************
071300170505     C     *INZSR        BEGSR
071400000000     C*
071500000000     C* Reperimento parametri
071600000000     C*
071700000000     C     *ENTRY        PLIST
071800000000     C                   PARM                    KPJBA
071900150210     C*
072000150210     C* reperisco DATFMT del job
072100150210     C                   CallB     'XDATFMT'
072200150210     C                   Parm                    $DatFmt           4
072300000000     C*
072400000000     C* Reperimento tasti di funzione
072500000000     C*
072600170505     C                   EXSR      REPKEY
072700000000     C*
072800170505     C                   ENDSR
072900170508     C/EJECT
073000000000     C************************************************************
073100000000     C* REPERIMENTO TASTI DI FUNZIONE
073200000000     C************************************************************
073300170505     C     REPKEY        BEGSR
073400000000     C*
073500000000     C                   MoveA     $FM1          $FM
073600000000     C                   MoveA     $Fc1          $Fc
073700170505     C                   EXSR      REPKKK
073800000000     C                   MoveA     $FX           $FX1
073900000000     C*
074000000000     C                   MoveA     $FM2          $FM
074100000000     C                   MoveA     $Fc2          $Fc
074200170505     C                   EXSR      REPKKK
074300000000     C                   MoveA     $FX           $FX2
074400000000     C*
074500000000     C                   MoveA     $FM3          $FM
074600000000     C                   MoveA     $Fc3          $Fc
074700170505     C                   EXSR      REPKKK
074800000000     C                   MoveA     $FX           $FX3
074900000000     C*
075000170505     C                   ENDSR
075100170508     C/EJECT
075200000000     C************************************************************
075300000000     C* REPERIMENTO TASTI DI FUNZIONE
075400000000     C************************************************************
075500170505     C     REPKKK        BEGSR
075600000000     C*
075700000000     C                   Clear                   $FX
075800000000     C     1             DO        24            X
075900000000     C*
076000000000     C     $FM(X)        IFNE      *BLANK
076100000000     C     $fc(X)        andne     '0'
076200000000     C*
076300000000     C                   MoveL     $FM(X)        IDMSG
076400000000     C                   CALLB     'XRTVM'
076500000000     C                   PARM                    IDMSG            27
076600000000     C                   PARM                    TXTMSG          100
076700000000     C                   PARM                    ERRMSG            1
076800000000     C*
076900000000     C     ERRMSG        IFNE      *ON
077000000000     C                   MoveL     TXTMSG        $FX(X)
077100000000     C                   ElSe
077200000000     C                   MoveL     *ALL' '       $FX(X)
077300000000     C                   EndIf
077400000000     C*
077500000000     C                   EndIf
077600000000     C*
077700000000     C                   ENDDO
077800000000     C*
077900170505     C                   ENDSR
078000170508     C/EJECT
078100170719     C************************************************************
078200170719     C* Ricerca Tipo Record
078300170719     C************************************************************
078400170719     C     F4_TpRec      BEGSR
078500170719     C*
078600170719     C                   Clear                   D1TpRecD
078700170719     C*
078800170719     C                   Clear                   X07ValDs
078900170719     C*
079000170719     C                   Move      '1'           X07Ric
079100170719     C                   Move      *On           X07All
079200170719     C                   Movel     'SDGCST000'   X07TRc
079300170719     C                   Movel     'CSTTPREC '   X07Cam
079400170719     C                   Movel     CSFTpRec      X07VAL
079500170719     C*
079600170719     C                   Callb     'X07VALR'
079700170719     C                   Parm                    X07ValDs
079800170719     C*
079900170719     C     X07ERR        IFEQ      *Off
080000170719     C                   MOVEL     X07DES        D1TpRecD
080100170719     C                   else
080200170719     C                   SetOn                                        5599
080300170719     C                   endif
080400170719     C*
080500170719     C                   EndSr
080600170719      /EJECT
080700000000     C************************************************************
080800000000     C* INIZIALIZZAZIONE VARIABILI
080900000000     C************************************************************
081000170505     C     INZVAR        BEGSR
081100000000     C*
081200170717     C                   MoveL     KPJBU         SDGSI80DS
081300150204     C*
081400000000     C* se il pgm e stato richiamato imposto i valori ricevuti dalla DS
081500170717     C                   If        SocietaI80 <> *Blanks
081600000000     C                   Eval      CallPgm     = '1'
081700000000     C                   ElSe
081800000000     C                   Eval      CallPgm     = '0'
081900000000     C                   EndIf
082000000000     C*
082100000000     C* Reperimento dati societa
082200000000     C*
082300000000     C                   MoveL     'SOC001'      TIPXSC
082400170717     C                   MoveL     SocietaI80    SOCXSC
082500170505     C                   EXSR      REPSOC
082600000000     C     RTNXSC        IFNE      '1'
082700000000     C                   MoveL     XSOCDS        SOC001
082800000000     C                   ElSe
082900000000     C                   Return
083000000000     C                   EndIf
083100000000     C*
083200000000     C* Reperimento valori default utente
083300000000     C*
083400170717     C*****              EXSR      REPDFT
083500000000     C*
083600000000     C* Definizione dei tasti funzione utilizzabili a pgm
083700000000     C*
083800170505     C                   EXSR      DEFOPZ
083900000000     C*
084000000000     C* Variabili per gestione videate
084100000000     C*
084200000000     C                   Move      'D1'          $GEST
084300000000     C                   Move      *OFF          $FINE
084400000000     C                   Move      *ON           $InzD1
084500000000     C                   Move      *BLANK        $LastG
084600000000     C                   Move      *BLANK        $LastV
084700000000     C                   Z-Add     0             $ULKD1            3 0
084800000000     C                   Z-Add     0             $ULKD2            3 0
084900000000     C                   Z-Add     0             $ULKD3            3 0
085000000000     C*
085100000000     C* Variabili appoggio
085200000000     C*
085300000000     C                   Z-Add     0             CURR
085400000000     C                   Z-Add     0             CURC
085500000000     C*
085600000000     C* Valorizzazione campi univoci testate
085700000000     C*
085800000000     C                   Clear                   FMTT1
085900000000     C                   MoveL     KNSIf         NOMSIf
086000000000     C                   MoveL     XSCDSI        NOMDIT
086100000000     C                   MOVEL     KCDAZ         NOMAZN
086200000000     C                   MOVEL     DSPGM         NOMPGM           10
086300000000     C* recupero il sottotitolo (se c'e)
086400000000     C                   MoveL     *BLANK        D1TIT
086500000000     C     SOTTITD1      IFNE      *BLANK
086600000000     C                   MoveL     SOTTITD1      IDMSG
086700000000     C                   CALLB     'XRTVM'
086800000000     C                   PARM                    IDMSG            27
086900000000     C                   PARM                    TXTMSG          100
087000000000     C                   PARM                    ERRMSG            1
087100000000     C     ERRMSG        IFNE      *ON
087200000000     C                   MoveL     TXTMSG        D1TIT
087300000000     C                   ElSe
087400000000     C                   MoveL     *ALL'?'       D1TIT
087500000000     C                   EndIf
087600000000     C                   EndIf
087700000000     C*
087800000000     C                   MoveL     *BLANK        T1TIT
087900170515     C                   Eval      LENMSG      = %Len(T1TIT)
088000000000     C     SOTTITD2      IFNE      *BLANK
088100000000     C                   MoveL     SOTTITD2      IDMSG
088200000000     C                   CALLB     'XRTVM'
088300000000     C                   PARM                    IDMSG            27
088400000000     C                   PARM                    TXTMSG          100
088500000000     C                   PARM                    ERRMSG            1
088600000000     C                   PARM      *On           CTRMSG            1            centratura
088700170515     C                   PARM                    LENMSG            3 0          lun output.
088800000000     C     ERRMSG        IFNE      *ON
088900000000     C                   MoveL     TXTMSG        T1TIT
089000000000     C                   ElSe
089100000000     C                   MoveL     *ALL'?'       T1TIT
089200000000     C                   EndIf
089300000000     C                   EndIf
089400000000     C*
089500000000     C* Inizializzazione/Decodifica prima videata
089600000000     C*
089700170505     C                   EXSR      InzD1
089800000000     C                   Move      *OFF          $InzD1
089900000000     C*
090000170505     C                   ENDSR
090100170508     C/EJECT
090200000000     C************************************************************
090300000000     C* REPERIMENTO DATI SOCIETA'
090400000000     C************************************************************
090500170505     C     REPSOC        BEGSR
090600000000     C*
090700000000     C                   CALL      'SDGSOCDR'
090800000000     C                   PARM                    TIPXSC            6
090900000000     C                   PARM                    SOCXSC            3
091000000000     C                   PARM                    CDSXSC            9 0
091100000000     C                   PARM                    MODXSC            3
091200000000     C                   PARM      *BLANKS       RTNXSC            1
091300000000     C                   PARM                    XSOCDS
091400000000     C                   PARM                    KPJBA
091500000000     C*
091600170505     C                   ENDSR
091700170508     C/EJECT
091800000000     C************************************************************
091900000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
092000000000     C************************************************************
092100170505     C     REPDFT        BEGSR
092200000000     C*
092300000000     C* preparo i parametri di chiamata al driver
092400000000     C                   Clear                   XMPEDS
092500000000     C                   EVAL      TpChiamata = '01'
092600000000     C                   EVAL      NChiavi = 5
092700000000     C                   EVAL      VLogica = 2
092800000000     C*
092900000000     C* preparo le chiavi per la chiamata al driver
093000000000     C                   Clear                   XMPEDS1
093100000000     C                   MoveL     XSCSOC        DSMPESOC
093200000000     C                   MoveL     DSPGM         DSMPEPGM
093300000000     C                   MoveL     KNMUS         DSMPEPRF
093400000000     C* Si reperiscono i dati di default a seconda delle modalita con
093500000000     C* cui il pgm e usato
093600000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
093700000000     C* MPETIP = 'PV'  pgm richiamato da menu
093800000000     C* MPETIP = 'PB'  x lancio Batch
093900000000     C                   MoveL     'PB'          DSMPETIP
094000000000     C*
094100000000     C                   CALLB     'XMPE'
094200000000     C                   PARM                    XMPEDS
094300000000     C                   PARM                    XMPEDS1
094400000000     C*
094500000000     C     Risultato     IfEq      '0'
094600000000     C                   MoveL     DSMPEPAR      DSMPE
094700000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
094800000000     C* i suoi valori di default vengono decisi a pgm
094900000000     C*
095000000000     C                   ElSe
095100000000     C                   EndIf
095200000000     C*
095300170505     C                   ENDSR
095400170508     C/EJECT
095500000000     C************************************************************
095600000000     C* Aggiornamento valori assunti default
095700000000     C************************************************************
095800170505     C     Aggmpe        BEGSR
095900000000     C*
096000000000     C* preparo i parametri di chiamata al driver
096100000000     C                   Clear                   XMPEDS
096200000000     C                   EVAL      TpChiamata = '03'
096300000000     C                   EVAL      NChiavi = 5
096400000000     C                   EVAL      VLogica = 2
096500000000     C*
096600000000     C* preparo le chiavi per la chiamata al driver
096700000000     C                   Clear                   XMPEDS1
096800000000     C                   MoveL     XscSoc        DSMPESOC
096900000000     C                   MoveL     DSPGM         DSMPEPGM
097000000000     C                   MoveL     KNMUS         DSMPEPRF
097100000000     C*
097200000000     C* Si memorizzano i dati di default a seconda delle modalita con
097300000000     C* cui il pgm e usato
097400000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
097500000000     C* MPETIP = 'PV'  pgm richiamato da menu
097600000000     C* MPETIP = 'PB'  x lancio Batch
097700130809     C*
097800130809     C                   MoveL     'PB'          DSMPETIP
097900130809     C* Dati in recupero
098000170717     C***                Move      D1Anno        $Anno
098100170717     C**                 Move      D1Trim        $Trim
098200130809     C*
098300130809     C                   MoveL     DSMPE         DSMPEPAR
098400130809     C*
098500130809     C                   CALLB     'XMPE'
098600130809     C                   PARM                    XMPEDS
098700000000     C                   PARM                    XMPEDS1
098800000000     C*
098900170505     C                   ENDSR
099000170508     C/EJECT
099100000000     C************************************************************
099200000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
099300000000     C************************************************************
099400170505     C     DEFOPZ        BEGSR
099500000000     C*
099600000000     C                   Reset                   $FC1
099700000000     C                   Reset                   $FR1
099800000000     C                   Reset                   $FC3
099900000000     C                   Reset                   $FR3
100000000000     C*
100100170505     C                   ENDSR
100200170508     C/EJECT
100300000000     C************************************************************
100400000000     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
100500000000     C************************************************************
100600170505     C     DEFCAM        BEGSR
100700000000     C*
100800000000     C* klist
100900170717     C*
101000170717     C* SDGCSF01L
101100170717     C     K02CSF01      KLIST
101200170717     C                   KFLD                    CSFSocieta
101300170717     C                   KFLD                    CSFFile
101400000000     C*
101500000000     C* Variabili per gestione videate
101600000000     C*
101700000000     C                   Move      *BLANK        $GEST             2
101800000000     C                   Move      *BLANK        $FINE             1
101900000000     C                   Move      *BLANK        $InzD1            1
102000000000     C                   Move      *BLANK        $LastG            2
102100000000     C                   Move      *BLANK        $LastV            2
102200000000     C                   Z-Add     0             $PIED1            3 0
102300000000     C*
102400000000     C* Variabili appoggio
102500000000     C*
102600000000     C                   Z-Add     0             CURR              2 0
102700000000     C                   Z-Add     0             CURC              2 0
102800000000     C                   Move      *ZEROS        WIN              99
102900000000     C*
103000000000     C* Indici
103100000000     C                   Z-Add     0             X                 3 0
103200000000     C                   Z-Add     0             Y                 3 0
103300000000     C*
103400170505     C                   ENDSR
103500170508     C/EJECT
103600000000     C************************************************************
103700000000     C* RIEMPIMENTO TASTI FUNZIONE
103800000000     C************************************************************
103900170505     C     RIEKEY        BEGSR
104000000000     C*
104100000000     C* Input :
104200000000     C* - $FX = schiera con tutti i testi disponibili
104300000000     C* - $FC = schiere con i flag di validita associati ai testi
104400000000     C* - $FR = schiere con i flag di REVERSE IMAGE
104500000000     C* - $ULK = ultimo testo caricato
104600000000     C* - $L01 = Lunghezza prima riga
104700000000     C* - $L02 = Lunghezza seconda riga
104800000000     C* Output :
104900000000     C* - $ULK
105000000000     C* - $KEY1= 1ø riga di tasti funzione
105100000000     C* - $KEY2= 2ø riga di tasti funzione
105200000000     C*
105300000000     C                   CALLB     'XKEYMSG'
105400000000     C* input
105500000000     C                   PARM                    $FX
105600000000     C                   PARM                    $FC
105700000000     C                   PARM                    $FR
105800000000     C                   PARM                    $ULK              3 0
105900000000     C* output
106000000000     C                   PARM                    $KEY1            79
106100000000     C                   PARM                    $KEY2            79
106200000000     C* input
106300000000     C                   PARM                    $L01              3 0
106400000000     C                   PARM                    $L02              3 0
106500000000     C*
106600000000     C                   PARM                    $ALLFUNCT       480
106700000000     C*
106800170505     C                   ENDSR
106900170508     C/EJECT
107000000000** TASTI DI FUNZIONE  D1
107100000000PROMSG    *LIBL     KEY0001  01
107200000000PROMSG    *LIBL     KEY0002  02
107300000000PROMSG    *LIBL     KEY0003  03
107400000000PROMSG    *LIBL     KEY0004  04
107500000000PROMSG    *LIBL     KEY0005  05
107600000000PROMSG    *LIBL     KEY0006  06
107700000000PROMSG    *LIBL     KEY0007  07
107800000000PROMSG    *LIBL     KEY0008  08
107900000000PROMSG    *LIBL     KEY0009  09
108000000000PROMSG    *LIBL     KEY0010  10
108100000000PROMSG    *LIBL     KEY0011  11
108200000000PROMSG    *LIBL     KEY0012  12
108300000000PROMSG    *LIBL     KEY0013  13
108400000000PROMSG    *LIBL     KEY0014  14
108500000000PROMSG    *LIBL     KEY0015  15
108600000000PROMSG    *LIBL     KEY0016  16
108700000000PROMSG    *LIBL     KEY0017  17
108800000000PROMSG    *LIBL     KEY0018  18
108900170512PROMSG    *LIBL     KEY0019  19
109000170508SDGMSG    *LIBL     SDK0026  20
109100000000PROMSG    *LIBL     KEY0021  21
109200170512SDGMSG    *LIBL     SDK0027  22
109300000000PROMSG    *LIBL     KEY0023  23
109400000000PROMSG    *LIBL     KEY0024  24
109500170512**  ABILITAZIONE D1
1096000000000N  01
1097000000000N  02
1098000000001N  03
1099000000001N  04
1100000000001N  05
1101000000001N  06
1102000000000N  07
1103000000000N  08
1104000000000N  09
1105000000000N  10
1106000000000N  11
1107000000001N  12
1108000000000N  13
1109000000000N  14
1110000000000N  15
1111000000000N  16
1112000000000N  17
1113001707170N  18
1114001705050N  19
1115001707170N  20
1116000000000N  21
1117001707170N  22
1118000000000N  23   Impostare '1' se si usano le opzioni
1119000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
112000170512** TASTI DI FUNZIONE  D2 / D4
112100000000PROMSG    *LIBL     KEY0001  01
112200000000PROMSG    *LIBL     KEY0002  02
112300000000PROMSG    *LIBL     KEY0003  03
112400000000PROMSG    *LIBL     KEY0004  04
112500000000PROMSG    *LIBL     KEY0005  05
112600000000PROMSG    *LIBL     KEY0006  06
112700000000PROMSG    *LIBL     KEY0007  07
112800000000PROMSG    *LIBL     KEY0008  08
112900000000PROMSG    *LIBL     KEY0009  09
113000000000PROMSG    *LIBL     KEY0010  10
113100000000PROMSG    *LIBL     KEY0011  11
113200000000PROMSG    *LIBL     KEY0012  12
113300000000PROMSG    *LIBL     KEY0013  13
113400000000PROMSG    *LIBL     KEY0014  14
113500000000PROMSG    *LIBL     KEY0015  15
113600000000PROMSG    *LIBL     KEY0016  16
113700000000PROMSG    *LIBL     KEY0017  17
113800000000PROMSG    *LIBL     KEY0018  18
113900000000PROMSG    *LIBL     KEY0019  19
114000000000PROMSG    *LIBL     KEY0020  20
114100000000PROMSG    *LIBL     KEY0021  21
114200000000PROMSG    *LIBL     KEY0022  22
114300000000PROMSG    *LIBL     KEY0023  23
114400000000PROMSG    *LIBL     KEY0024  24
114500170512**  ABILITAZIONE D2 / D4
1146000000000N  01
1147000000000N  02
1148000000001N  03
1149001108111N  04
1150000000001N  05
1151000000001N  06
1152000000000N  07
1153000000000N  08
1154000000000N  09
1155000000000N  10
1156000000000N  11
1157000000001N  12
1158000000000N  13
1159000000000N  14
1160000000000N  15
1161000000000N  16
1162000000000N  17
1163000000000N  18
1164000000000N  19
1165000000000N  20
1166000000000N  21
1167000000000N  22
1168000000000N  23   Impostare '1' se si usano le opzioni
1169000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
117000000000** TASTI DI FUNZIONE  D3
117100000000PROMSG    *LIBL     KEY0001  01
117200000000PROMSG    *LIBL     KEY0002  02
117300000000PROMSG    *LIBL     KEY0003  03
117400000000PROMSG    *LIBL     KEY0004  04
117500000000PROMSG    *LIBL     KEY0005  05
117600000000PROMSG    *LIBL     KEY0006  06
117700000000PROMSG    *LIBL     KEY0007  07
117800000000PROMSG    *LIBL     KEY0008  08
117900000000PROMSG    *LIBL     KEY0009  09
118000000000PROMSG    *LIBL     KEY0010  10
118100000000PROMSG    *LIBL     KEY0011  11
118200000000PROMSG    *LIBL     KEY0012  12
118300000000PROMSG    *LIBL     KEY0013  13
118400000000PROMSG    *LIBL     KEY0014  14
118500000000PROMSG    *LIBL     KEY0015  15
118600000000PROMSG    *LIBL     KEY0016  16
118700000000PROMSG    *LIBL     KEY0017  17
118800000000PROMSG    *LIBL     KEY0018  18
118900000000PROMSG    *LIBL     KEY0019  19
119000000000PROMSG    *LIBL     KEY0020  20
119100000000PROMSG    *LIBL     KEY0021  21
119200000000PROMSG    *LIBL     KEY0022  22
119300000000PROMSG    *LIBL     KEY0023  23
119400000000PROMSG    *LIBL     KEY0024  24
119500000000**  ABILITAZIONE D3
1196000000000N  01
1197000000000N  02
1198000000001N  03
1199000000000N  04
1200000000000N  05
1201000000000N  06
1202000000000N  07
1203000000000N  08
1204000000000N  09
1205000000000N  10
1206000000000N  11
1207000000001N  12
1208000000000N  13
1209000000000N  14
1210000000000N  15
1211000000000N  16
1212000000000N  17
1213000000000N  18
1214000000000N  19
1215000000000N  20
1216000000000N  21
1217000000000N  22
1218000000000N  23   Impostare '1' se si usano le opzioni
1219000000001N  24   Lasciare sempre impostata a '0' il valore della riga 24
