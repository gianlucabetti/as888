000100171218     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200171218     H*PARMS CVTOPT(*NONE)
000300171218     H DECEDIT('0,') DATEDIT(*DMY.)
000400171218      *
000500171218      *
000600171218      *          Regole per detrazione IVA a Credito
000700171218      *               Manutenzione
000800171218      *        -----------------------------------------
000900171218      *
001000171218R275  * Questo pgm è stato creato con il task R275 Slitt. data liquidazion IVA
001100171218R275  * Decreto Legge 50/2017
001200171218      *
001300171218     F*----------------------------------------------------*
001400171218     F* Regole per detrazione IVA a Credito
001500171219     F* Key: Soc/Reg/Libro/DtInVal Unique
001600171218     FSDGRIC01L UF A E           K DISK
001700171218     F*------------
001800171218     F* Anagrafico libri IVA
001900171218     F* Key: FSC/TRI/LIB
002000171218     FANDLI01L  IF   E           K DISK
002100171218     F*------------
002200171218     FSDGSI31D  CF   E             WORKSTN
002300171218     F                                     RENAME(SI31D1:FMTD1)
002400171218     F                                     RENAME(SI31T1:FMTT1)
002500171218     F                                     RENAME(SI31Z1:FMTZ1)
002600171218     F                                     INFDS(DSFMT)
002700171218     D*----------------------------------------------------*
002800171218     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
002900171218     D $ID             S             27    DIM(5) ALT($OZ)
003000171218     D* Schiere per la gestione dei tasti di funzione
003100171218     D $FM             S             27    DIM(24)
003200171218     D $FX             S             50    DIM(24)
003300171218     D $FC             S              1    DIM(24)
003400171218     D $FR             S              1    DIM(24)
003500171218     D* Video D1
003600171218     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003700171218     D $FX1            S             50    DIM(24) ALT($FM1)
003800171218     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003900171218     D $FR1            S              1    DIM(24) ALT($FC1)
004000171218     D*-------------
004100171218     D* Passaggio Parametri
004200171218     D KPJBA         E DS
004300171218     D*-------------
004400171218     D* DS Esterna gestione messaggi per COMIT
004500171218     D XMSGDS        E DS
004600171218     D  STS          E                     EXTFLD(MSGSTS)
004700171218     D                                     DIM(333)
004800171218     D  JBA          E                     EXTFLD(MSGJBA)
004900171218     D                                     DIM(502)
005000171218     D*-------------
005100171218     D* Reperimento Autorizzazioni
005200171218     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
005300171218     D*-------------
005400171218     D* Passaggio Autorizzazioni
005500171218     D DSAUT           DS           512
005600171218     D*-------------
005700171218     D* Maschera dati autorizzazioni
005800171218     D ANGIVADS      E DS
005900171218     D*-------------
006000171218     D* DS gestione gruppo  o azienda
006100171218     D XPAOUT          DS          2000
006200171218     D*-------------
006300171218     D* Reperimento nome PGM
006400171218     D STATUS         SDS           333
006500171218     D  DSPGM            *PROC
006600171218     D  PARMS            *PARMS
006700171218     D*-------------
006800171218     D* Dati di ambiente ottenuti da XSOC
006900171218    >D SOC001        E DS                  EXTNAME(XSOC001DS)
007000171218     D*-------------
007100171218     D* DS Interna per dati di output di XSOC
007200171218     D XSOCDS          DS          1000
007300171218     D*-------------
007400171218     D* Ricerca legende
007500171218     D ANA964DS      E DS                  INZ
007600171218     D* Controlli validità legende
007700171218     D X07VALDS      E DS                  INZ
007800171218     D*-------------
007900171218     D* Passaggio Parametri al PGM di manutenzione
008000171218     D SDGSI31DS     E DS
008100171221     D*-------------
008200171221     D* Ricerca causale
008300171221     D ANA070DS      E DS                  INZ
008400171221     D  RET070       E                     INZ('0')
008500171221     D  OPR070       E                     INZ('0')
008600171221     D  CMT070       E                     INZ('0')
008700171221     D  ERR070       E                     INZ('0')
008800171221     D  FSC070       E                     INZ('S')
008900171221     D*---------
009000171221     D* Definizioni controllo causale
009100171221     D Anope00f      E ds                  inz
009200171221     D GesMsg          S              1
009300171221     D Struttura       S             10
009400171221     D Esito           S              1
009500171221     D LenOut          S              9B 0
009600171221     D Refresh         S              1
009700171221     D Uso             S              1
009800171221     D Causale         S              4
009900171221     D WDtAlfa         S             10A
010000171218     D*-------------
010100171218     D DSFMT           DS
010200171218     D  $TASTO               369    369
010300171218     D  NRG                  370    370
010400171218     D  NCL                  371    371
010500171218     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
010600171218     D*-------------
010700171218     D* posizione cursore
010800171218     D CURSOR          DS
010900171218     D  RIRI                   1      2B 0 INZ
011000171218     D  R$$                    2      2
011100171218     D  COCO                   3      4B 0 INZ
011200171218     D  C$$                    4      4
011300171218     D*-------------
011400171218     D* Tasti di funzione
011500171218     D F01             C                   CONST(X'31')
011600171218     D F02             C                   CONST(X'32')
011700171218     D F03             C                   CONST(X'33')
011800171218     D F04             C                   CONST(X'34')
011900171218     D F05             C                   CONST(X'35')
012000171218     D F06             C                   CONST(X'36')
012100171218     D F07             C                   CONST(X'37')
012200171218     D F08             C                   CONST(X'38')
012300171218     D F09             C                   CONST(X'39')
012400171218     D F10             C                   CONST(X'3A')
012500171218     D F11             C                   CONST(X'3B')
012600171218     D F12             C                   CONST(X'3C')
012700171218     D F13             C                   CONST(X'B1')
012800171218     D F14             C                   CONST(X'B2')
012900171218     D F15             C                   CONST(X'B3')
013000171218     D F16             C                   CONST(X'B4')
013100171218     D F17             C                   CONST(X'B5')
013200171218     D F18             C                   CONST(X'B6')
013300171218     D F19             C                   CONST(X'B7')
013400171218     D F20             C                   CONST(X'B8')
013500171218     D F21             C                   CONST(X'B9')
013600171218     D F22             C                   CONST(X'BA')
013700171218     D F23             C                   CONST(X'BB')
013800171218     D F24             C                   CONST(X'BC')
013900171218     D ENTER           C                   CONST(X'F1')
014000171218     D ROLDWN          C                   CONST(X'F4')
014100171218     D ROLLUP          C                   CONST(X'F5')
014200171218     D*-------------
014300171218     D ZDateISO        S               D
014400171218     C*----------------------------------------------------*
014500171218     C*                MAIN LINE PROGRAM
014600171218     C*----------------------------------------------------*
014700171218     C*
014800171218     C* inizializzazione variabili
014900171218     C                   EXSR      INZVAR
015000171218     C*
015100171218     C     $FINE         DOWEQ     *OFF
015200171218     C     $GEST         CASEQ     'D1'          GESD1
015300171218     C                   ENDCS
015400171218     C                   ENDDO
015500171218     C*
015600171218     C* fine programma
015700171218     C                   EXSR      ENDPGM
015800171218     C/EJECT
015900171218     C************************************************************
016000171218     C* RICERCHE
016100171218     C************************************************************
016200171218     C     ENDPGM        BEGSR
016300171218     C*
016400171218     C* se ho confermato l'uscita proseguo
016500171218     C     $FINE         IFEQ      *ON
016600171218     C*
016700171218     C* disallocazione rcd nel caso di modifica/annullamento
016800171218     C     OPZI31        IFEQ      '02'
016900171218     C     OPZI31        OREQ      '04'
017000171218     C                   UNLOCK    SDGRIC01L                            22
017100171218     C                   ENDIF
017200171218     C*
017300171218     C* passaggio dati al pgm chiamante
017400171218     C                   MOVEL     SDGSI31DS     KPJBU
017500171218     C*
017600171218     C                   SETON                                            RT
017700171218     C                   RETURN
017800171218     C*
017900171218     C                   ENDIF
018000171218     C*
018100171218     C                   ENDSR
018200171218     C/EJECT
018300171218     C************************************************************
018400171218     C* RICERCHE
018500171218     C************************************************************
018600171218     C     SEARCH        BEGSR
018700171218     C*
018800171218     C* determino Riga/Colonna del cursore
018900171218     C                   MOVE      NRG           R$$
019000171218     C                   MOVE      NCL           C$$
019100171218     C                   Z-ADD     RIRI          CURR
019200171218     C                   Z-ADD     COCO          CURC
019300171218     C*
019400171218     C* I campi che al momento dell'F4 risultano protetti non devono
019500171218     C* essere abilitati alla funzione di ricerca
019600171218     C*
019700171218     C                   SELECT
019800171218     C*
019900171218     C** FMTD1  - TIPO REGISTRO
020000171218     C     H1NMFL        WHENEQ    'D1TPREG'
020100171218     C                   EXSR      F4_TpReg
020200171218     C*
020300171218     C** FMTD1  - Tipo regola/periodo
020400171218     C     H1NMFL        WHENEQ    'D1TPRIC'
020500171218     C                   EXSR      F4_TpRIC
020600171221     C*
020700171221     C** FMTD1  - Causale di indetraibilità
020800171221     C     H1NMFL        WHENEQ    'D1CAUSIND'
020900171221     C                   EXSR      F4_CausInd
021000171218     C*
021100171218     C* ricerca non abilitata in questa posizione
021200171218     C                   OTHER
021300171218     C                   SETON                                        9899
021400171218     C                   ENDSL
021500171218     C*
021600171218     C* imposto pos. del cursore
021700171218     C                   Z-ADD     CURR          H1RIGA
021800171218     C                   Z-ADD     CURC          H1COLO
021900171218     C                   MOVE      *HIVAL        $LASTV
022000171218     C                   ENDSR
022100171218     C/EJECT
022200171218     C************************************************************
022300171218     C* Ricerca tipo registro IVA
022400171218     C************************************************************
022500171218     C     F4_TpReg      BEGSR
022600171218     C*
022700171218     C                   Reset                   ANA964DS
022800171218     C*
022900171221     C                   if        OPZI31      = '05' or
023000171221     C                             OPZI31      = '04'
023100171221     C                   Move      '05'          Opz964
023200171221     C                   else
023300171221     C                   MOVEL     '01'          OPZ964
023400171221     C                   endif
023500171218     C                   MOVEL     'ANDLI000'    TRC964
023600171218     C                   MOVEL     'DLITPREG'    CAM964
023700171218     C                   MoveL     *On           All964
023800171218     C*
023900171218     C                   MoveL     ANA964DS      KPJBU
024000171218     C                   CALL      'ANA964R'
024100171218     C                   PARM                    KPJBA
024200171218     C                   MoveL     KPJBU         ANA964DS
024300171218     C*
024400171218     C                   if        Ret964     <> '2'
024500171218     C                   MoveL     Val964        D1TpReg
024600171218     C                   endif
024700171218     C*
024800171218     C                   ENDSR
024900171218     C/EJECT
025000171218     C************************************************************
025100171218     C* Ricerca tipo regola/periodo
025200171218     C************************************************************
025300171218     C     F4_TpRIC      BEGSR
025400171218     C*
025500171218     C                   Reset                   ANA964DS
025600171218     C*
025700171221     C                   if        OPZI31      = '05' or
025800171221     C                             OPZI31      = '04'
025900171221     C                   Move      '05'          Opz964
026000171221     C                   else
026100171218     C                   MOVEL     '01'          OPZ964
026200171221     C                   endif
026300171218     C                   MOVEL     'SDGRIC000'   TRC964
026400171218     C                   MOVEL     'RICTPRIC'    CAM964
026500171218     C                   MoveL     *On           All964
026600171218     C*
026700171218     C                   MoveL     ANA964DS      KPJBU
026800171218     C                   CALL      'ANA964R'
026900171218     C                   PARM                    KPJBA
027000171218     C                   MoveL     KPJBU         ANA964DS
027100171218     C*
027200171218     C                   if        Ret964     <> '2'
027300171218     C                   MoveL     Val964        D1TpRIC
027400171218     C                   endif
027500171218     C*
027600171218     C                   ENDSR
027700171218     C/EJECT
027800171221     C************************************************************
027900171221     C* Ricerca causale di indetraibilità
028000171221     C************************************************************
028100171221     C     F4_CausInd    BEGSR
028200171221     C*
028300171221     C                   Clear                   D1CausIndD
028400171221     C*
028500171221     C                   Reset                   ANA070DS
028600171221     C*
028700171221     C                   if        OPZI31      = '05' or
028800171221     C                             OPZI31      = '04'
028900171221     C                   Move      '02'          Opz070
029000171221     C                   else
029100171221     C                   Move      '01'          Opz070
029200171221     C                   endif
029300171221     C                   MOVEL     D1CausInd     Cau070
029400171221     C                   MOVE      '2'           Tpc070
029500171221     C                   MOVE      FSCI31        FSC070
029600171221     C*
029700171221     C                   CLEAR                   KPJBU
029800171221     C                   MoveL     ANA070DS      KPJBU
029900171221     C*
030000171221     C                   CALL      'ANA070R'
030100171221     C                   PARM                    KPJBA
030200171221     C*
030300171221     C                   MoveL     KPJBU         ANA070DS
030400171221     C*
030500171221     C     Cau070        IFNE      *BLANK
030600171221     C                   MoveL     Cau070        D1CausInd
030700171221     C                   MoveL     De1070        D1CausIndD
030800171221     C                   ENDIF
030900171221     C*
031000171221     C                   ENDSR
031100171221     C/EJECT
031200171218     C************************************************************
031300171218     C* GESTIONE EMISSIONE TASTI FUNZIONE
031400171218     C************************************************************
031500171218     C     GESPIE        BEGSR
031600171218     C*
031700171218     C                   SELECT
031800171218     C*
031900171218     C     $GEST         WHENEQ    'D1'
032000171218     C     $ULKD1        IFEQ      0
032100171218     C* riempimento opzioni
032200171218     C                   EXSR      F24D1
032300171218     C                   ELSE
032400171218     C                   WRITE     FMTZ1
032500171218     C                   ENDIF
032600171218     C                   ENDSL
032700171218     C*
032800171218     C                   ENDSR
032900171218     C/EJECT
033000171218     C************************************************************
033100171218     C* GESTIONE VIDEO RECORD D1
033200171218     C************************************************************
033300171218     C     GESD1         BEGSR
033400171218     C* inizializzazione videata
033500171218   B1C     $INZD1        IFEQ      *ON
033600171218     C                   EXSR      INZD1
033700171218     C                   MOVE      *OFF          $INZD1
033800171218   E1C                   ENDIF
033900171218     C* emissione piede videata se proveniente da altra
034000171218     C* salvataggio valore formato di provenienza
034100171218   B1C     $LASTV        IFNE      'D1'
034200171218     C                   WRITE     FMTT1
034300171218     C                   EXSR      GESPIE
034400171218     C                   MOVE      $LASTV        $LASTG
034500171218     C                   MOVE      'D1'          $LASTV
034600171218   E1C                   ENDIF
034700171218     C*
034800171218     C* se esistono errori sulla videata
034900171218     C* emetto la write del formato a indicatori spenti per vedere
035000171218     C* le eventuali decodifiche
035100171218   B1C     *IN99         IFEQ      *ON
035200171218     C     $GEST         ANDEQ     'D1'
035300171218     C                   MOVEA     *IN           WIN
035400171218     C                   MOVE      *ALL'0'       IN5098           49
035500171218     C                   MOVEA     IN5098        *IN(50)
035600171218     C                   WRITE     FMTD1
035700171218     C                   MOVEA     WIN           *IN
035800171218   E1C                   ENDIF
035900171218     C*
036000171218   B1C                   SELECT
036100171218     C* Annullamento
036200171218    >C     OPZI31        WHENEQ    '04'
036300171218     C* Visualizzazione
036400171218    >C     OPZI31        OREQ      '05'
036500171218     C                   WRITE     FMTD1
036600171218     C                   EXFMT     PROTECT
036700171218     C* Immissione/Modifica/Copia
036800171218   X1C                   OTHER
036900171218     C                   EXFMT     FMTD1
037000171218   E1C                   ENDSL
037100171218     C* controllo abilitazione tasti funzione
037200171218     C*
037300171218     C                   EXSR      CTRKD1
037400171218     C*
037500171218   B1C     *IN99         IFEQ      *OFF
037600171218     C*
037700171218   B2C                   SELECT
037800171218     C* F2=Decodifica
037900171218     C     $TASTO        WHENEQ    F02
038000171218     C                   EXSR      F02D1
038100171218     C* F3=Fine
038200171218     C     $TASTO        WHENEQ    F03
038300171218     C                   EXSR      F03D1
038400171218     C* F4=Ricerca
038500171218     C     $TASTO        WHENEQ    F04
038600171218     C                   EXSR      SEARCH
038700171218     C* F5=Refresh
038800171218     C     $TASTO        WHENEQ    F05
038900171218     C                   EXSR      F05D1
039000171218     C* F8=Successivo
039100171218     C     $TASTO        WHENEQ    F08
039200171218     C                   EXSR      F08D1
039300171218     C* F12=Ritorno
039400171218     C     $TASTO        WHENEQ    F12
039500171218     C                   EXSR      F12D1
039600171218     C* F24=Altri tasti
039700171218     C     $TASTO        WHENEQ    F24
039800171218     C                   EXSR      F24D1
039900171218     C*
040000171218   X2C                   OTHER
040100171218     C                   EXSR      CTRD1
040200171218     C*
040300171218   B3C     $TASTO        IFEQ      F06
040400171218     C     *IN99         ANDEQ     *OFF
040500171218     C* eseguo aggiornamento
040600171218     C                   EXSR      AGGANA
040700171218     C* eseguo operazioni del dopo-aggiornamento
040800171218   B4C     *IN99         IFEQ      *OFF
040900171218     C                   EXSR      GESAGG
041000171218   E4C                   ENDIF
041100171218   E3C                   ENDIF
041200171218     C*
041300171218   E2C                   ENDSL
041400171218     C*
041500171218   E1C                   ENDIF
041600171218     C*
041700171218     C                   ENDSR
041800171218     C/EJECT
041900171218     C************************************************************
042000171218     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
042100171218     C************************************************************
042200171218     C     CTRKD1        BEGSR
042300171218     C*
042400171218     C                   SETOFF                                       99
042500171218     C                   MOVEA     $FC1          $FC
042600171218     C                   EXSR      CTRKEY
042700171218     C     *IN99         IFEQ      *ON
042800171218     C                   SETON                                        97
042900171218     C                   ENDIF
043000171218     C*
043100171218     C                   ENDSR
043200171218     C/EJECT
043300171218     C************************************************************
043400171218     C* INIZIALIZZAZIONE VIDEATA DATI
043500171218     C************************************************************
043600171218     C     INZD1         BEGSR
043700171218     C*
043800171218     C                   CLEAR                   FMTD1
043900171218     C*
044000171218     C                   Move      XScOn         D1On1
044100171218     C                   Move      XScOn         D1On2
044200171218     C                   Move      XScOn         D1On3
044201171228     C                   Move      XScOn         D1On4
044300171218     C                   Move      XScOff        D1Off1
044400171218     C                   Move      XScOff        D1Off2
044500171218     C                   Move      XScOff        D1Off3
044501171228     C                   Move      XScOff        D1Off4
044600171218     C*
044700171218     C                   Z-ADD     1             $PIED1
044800171218     C*
044900171218   B>C     OPZI31        IFNE      '01'
045000171218    >C                   MOVEL     WWWSOC        RICSocieta
045100171218    >C                   MOVEL     TpRegI31      RICTpReg
045200171218    >C                   MOVEL     LibroI31      RICLibro
045300171218    >C                   MOVE      DtInValI31    RICDtInVal
045400171218     C*
045500171218   B2C     *IN22         DOUEQ     *OFF
045600171218     C     MSGRTN        ORNE      'R'
045700171218     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
045800171218   B>C     OPZI31        IFEQ      '02'
045900171218    >C     OPZI31        OREQ      '04'
046000171218    >C     K04RIC01      CHAIN     SDGRIC01L                          2122
046100171218     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
046200171218   X3C                   ELSE
046300171218    >C     K04RIC01      CHAIN(N)  SDGRIC01L                          2122
046400171218   E3C                   ENDIF
046500171218     C*
046600171218     C* Gestione RCD allocato
046700171218   B3C     *IN22         IFEQ      *ON
046800171218    >C                   MOVEL     'SDGRIC00F'   MSGFIL
046900171218     C                   EXSR      MSGPGM
047000171218   E3C                   ENDIF
047100171218   E2C                   ENDDO
047200171218     C*
047300171218     C* se il rcd da gestire non viene trovato
047400171218     C* oppure è allocato da altro pgm
047500171218     C* ritorno al pgm guida avvertendolo dell'errore
047600171218   B2C     *IN21         IFEQ      *ON
047700171218     C     *IN22         OREQ      *ON
047800171218     C                   MOVE      *ON           $FINE
047900171218    >C                   MOVE      '1'           ERRI31
048000171218     C*
048100171218     C* altrimenti
048200171218   X2C                   ELSE
048300171218     C* riempio la videata
048400171218     C                   EXSR      RIED1
048500171218     C* richiamo routine dei ctrl per decodificarla
048600171218     C                   EXSR      CTRD1
048700171218     C                   MOVE      *ALL'0'       IN5098
048800171218     C                   MOVEA     IN5098        *IN(50)
048900171218     C* definizione indicatori su video
049000171219    >C                   if        OPZI31      = '02'
049100171219     C                   Eval      *In02       = *On
049200171219     C                   else
049300171219     C                   Eval      *In02       = *Off
049400171219     C                   endif
049500171218     C*
049600171218   E2C                   ENDIF
049700171218     C*
049800171218   X1C                   ELSE
049900171218     C* attivo tutti i campi
050000171218     C                   SETON                                        11
050100171218     C*
050200171218     C* riempio con i default di immissione
050300171218     C                   EXSR      RIEDEF
050400171218     C*
050500171218     C* imposto i campi chiave ricevuti anche se si è in immissione
050600171218     C* perchè potrebbero essere stati passati dei default di chiave
050700171218     C                   MOVEL     TpRegI31      D1TpReg
050800171218     C                   MOVEL     LibroI31      D1Libro
050900171218     C                   MOVEL     TpRICI31      D1TpRIC
051000171218     C                   MOVEL     DescrI31      D1Descr
051100171218   E1C                   ENDIF
051200171218     C*
051300171218     C                   ENDSR
051400171218     C/EJECT
051500171218     C************************************************************
051600171218     C* RIEMPIMENTO VIDEATA  D1
051700171218     C************************************************************
051800171218     C     RIED1         BEGSR
051900171218     C*
052000171218     C* impostare i campi della videata dai campi del file
052100171218     C*
052200171218     C                   MOVE      RICTpReg      D1TpReg
052300171218     C                   MOVE      RICLibro      D1Libro
052400171218     C                   MOVE      RICTpRIC      D1TpRIC
052500171219     C                   MOVE      RICDescr      D1Descr
052600171219     C*
052700171218     C                   IF        RICDtInVal <> *Loval
052800171218     C     *JobRun       MOVE      RICDtInVal    D1DtInVal
052900171218     C                   ENDIF
053000171218     C                   IF        RICDtFiVal <> *Hival
053100171218     C     *JobRun       MOVE      RICDtFiVal    D1DtFiVal
053200171218     C                   ENDIF
053201171228     C*
053202171228     C                   IF        RICFatt     = *On
053203171228     C                   Move      XScOn         D1Fatt
053204171228     C                   else
053205171228     C                   Move      XScOff        D1Fatt
053206171228     C                   ENDIF
053300171219     C*
053400171219     C                   IF        RICNAcc     = *On
053500171219     C                   Move      XScOn         D1NAcc
053600171219     C                   else
053700171219     C                   Move      XScOff        D1NAcc
053800171219     C                   ENDIF
053900171219     C*
054000171219     C                   IF        RICDtLiq   <> *Loval
054100171219     C     *JobRun       MOVE      RICDtLiq      D1DtLiq
054200171219     C                   ENDIF
054300171219     C*
054400171219     C                   IF        RICMsg1     = *On
054500171219     C                   Move      XScOn         D1Msg1
054600171219     C                   else
054700171219     C                   Move      XScOff        D1Msg1
054800171219     C                   ENDIF
054900171219     C*
055000171219     C                   IF        RICMsg2     = *On
055100171219     C                   Move      XScOn         D1Msg2
055200171219     C                   else
055300171219     C                   Move      XScOff        D1Msg2
055400171219     C                   ENDIF
055500171221     C*
055600171221     C                   Move      RicCausInd    D1CausInd
055700171218     C*
055800171218     C* salvataggio campi chiave univoca per opzione di modifica
055801171228     C* ed anche visualizzazione
055900171228     C                   IF        OPZI31='02' Or OPZI31='05'
056000171218     C                   MOVE      RICSocieta    SavSocieta
056100171218     C                   MOVE      RICTpReg      SavTpReg
056200171218     C                   MOVE      RICLibro      SavLibro
056300171218     C                   MOVE      RICDtInVal    SavDtInVal
056400171218     C                   ENDIF
056500171218     C*
056600171218     C                   ENDSR
056700171218     C/EJECT
056800171218     C************************************************************
056900171218     C* RIEMPIMENTO CAMPI DAI DEFAULT
057000171218     C************************************************************
057100171218     C     RIEDEF        BEGSR
057200171218     C*
057300171218     C                   IF        DEFDtInVal <> *Loval
057400171218     C     *JobRun       MOVE      DEFDtInVal    D1DtInVal
057500171218     C                   ENDIF
057600171218     C                   IF        DefDtFiVal <> *Hival
057700171218     C     *JobRun       MOVE      DefDtFiVal    D1DtFiVal
057800171218     C                   ENDIF
057900171218     C*
058000171218     C                   ENDSR
058100171218     C/EJECT
058200171218     C************************************************************
058300171218     C* GESTIONE F02 VIDEO D1
058400171218     C************************************************************
058500171218     C     F02D1         BEGSR
058600171218     C*
058700171218     C* per decodificare la videata richiamo la routine dei ctrl
058800171218     C* e poi disattivo gli eventuali messaggi di errore
058900171218     C                   EXSR      CTRD1
059000171218     C*
059100171218     C                   MOVE      *ALL'0'       IN5098
059200171218     C                   MOVEA     IN5098        *IN(50)
059300171218     C*
059400171218     C                   ENDSR
059500171218     C/EJECT
059600171218     C************************************************************
059700171218     C* GESTIONE F03 VIDEO D1
059800171218     C************************************************************
059900171218     C     F03D1         BEGSR
060000171218     C*
060100171218     C                   MOVE      *ON           $FINE
060200171218    >C                   MOVE      '1'           RETI31
060300171218     C* fine programma
060400171218     C                   EXSR      ENDPGM
060500171218     C*
060600171218     C                   ENDSR
060700171218     C/EJECT
060800171218     C************************************************************
060900171218     C* GESTIONE F05 VIDEO D1
061000171218     C************************************************************
061100171218     C     F05D1         BEGSR
061200171218     C*
061300171218     C                   MOVE      *ON           $INZD1
061400171218     C*
061500171218     C                   ENDSR
061600171218     C/EJECT
061700171218     C************************************************************
061800171218     C* GESTIONE F08 VIDEO D1
061900171218     C************************************************************
062000171218     C     F08D1         BEGSR
062100171218     C*
062200171218     C                   MOVE      *ON           $FINE
062300171218    >C                   MOVE      '0'           RETI31
062400171218     C*
062500171218     C                   ENDSR
062600171218     C/EJECT
062700171218     C************************************************************
062800171218     C* GESTIONE F12 VIDEO D1
062900171218     C************************************************************
063000171218     C     F12D1         BEGSR
063100171218     C*
063200171218     C                   MOVE      *ON           $FINE
063300171218    >C                   MOVE      '2'           RETI31
063400171218     C*
063500171218     C                   ENDSR
063600171218     C/EJECT
063700171218     C************************************************************
063800171218     C* GESTIONE F24 VIDEO D1
063900171218     C************************************************************
064000171218     C     F24D1         BEGSR
064100171218     C*
064200171218     C                   MOVEA     $FX1          $FX
064300171218     C                   MOVEA     $FC1          $FC
064400171218     C                   MOVEA     $FR1          $FR
064500171218     C                   Z-ADD     $ULKD1        $ULK
064600171218     C                   EXSR      RIEKEY
064700171218     C                   Z-ADD     $ULK          $ULKD1            3 0
064800171218     C                   MOVEA     $FC           $FC1
064900171218     C                   MOVEL     $KEY1         Z1KE1
065000171218     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
065100171218     C                   MOVEL     $KEY2         Z1KE2
065200171218     C                   WRITE     FMTZ1
065300171218     C*
065400171218     C                   ENDSR
065500171218     C/EJECT
065600171218     C************************************************************
065700171218     C* CONTROLLO VIDEATA
065800171218     C************************************************************
065900171218     C     CTRD1         BEGSR
066000171218     C*
066100171218     C                   SETOFF                                       99
066200171218     C* non imposto pos. del cursore
066300171218     C                   Z-ADD     0             H1RIGA
066400171218     C                   Z-ADD     0             H1COLO
066500171218     C*
066600171218     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
066700171218     C*
066800171218     C     OPZI31        IFNE      '04'
066900171218     C*
067000171218     C                   EXSR      CtrTpReg
067100171218     C                   EXSR      CtrLibro
067200171219     C                   EXSR      CtrDtVal
067300171218     C*
067400171218     C                   EXSR      CtrTpRIC
067500171219     C                   EXSR      CtrDescr
067501171228     C                   EXSR      CtrFatt
067600171228     C                   EXSR      CtrNAcc
067601171228     C                   EXSR      CtrFAtNac
067700171219     C                   EXSR      CtrDtLiq
067800171219     C                   EXSR      CtrMsg1
067900171219     C                   EXSR      CtrMsg2
068000171221     C                   EXSR      CtrCausInd
068100171218     C*
068200171218     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
068300171218    >C     OPZI31        IFEQ      '01'
068400171218    >C     OPZI31        OREQ      '03'
068500171218     C*
068600171218     C                   IF        *IN59=*OFF
068700171218     C                   MOVEL     WWWSOC        RICSocieta
068800171218     C                   MOVEL     D1TpReg       RICTpReg
068900171218     C                   MOVEL     D1Libro       RICLibro
069000171218     C                   IF        D1DtInVal  <> 0
069100171218     C     *JobRun       MOVE      D1DtInVal     RICDtInVal
069200171218     C                   ENDIF
069300171218     C*
069400171218     C     K04RIC01      SETLL     SDGRIC01L                              21
069500171218     C     *IN21         IFEQ      '1'
069600171218     C                   SETON                                        5199
069700171218     C                   ENDIF
069800171218     C                   ENDIF
069900171218     c*
070000171218     C                   ENDIF
070100171218     C*
070200171218     C                   ENDIF
070300171218     C*
070400171218     C                   ENDSR
070500171218     C/EJECT
070600171218     C************************************************************
070700171218     C* CONTROLLO tipo registro IVA
070800171218     C************************************************************
070900171218     C     CtrTpReg      BEGSR
071000171218     C*
071100171218     C                   Clear                   D1TpRegD
071200171218     C*
071300171218     C*  CTRL OBBLIGATORIETA' TIPO REGISTRO
071400171218     C     D1TpReg       IFEQ      *BLANK
071500171218     C                   SETON                                        5099
071600171218     C                   ENDIF
071700171218     C*
071800171218     C     D1TpReg       IFNE      *BLANK
071900171218     C*
072000171218     C                   RESET                   X07VALDS
072100171218     C                   MOVE      '1'           X07RIC
072200171218     C                   MOVEL     'ANDLI000'    X07TRC
072300171218     C                   MOVEL     'DLITPREG'    X07CAM
072400171218     C                   MOVEL     D1TpReg       X07VAL
072500171218     C                   CALLb     'X07VALR'
072600171218     C                   PARM                    X07VALDS
072700171218     C*
072800171218     C     X07ERR        IFEQ      '0'
072900171218     C                   MOVEL     X07DES        D1TpRegD
073000171218   X3C                   ELSE
073100171218     C                   SETON                                          5299
073200171218   E3C                   ENDIF
073300171218     C*
073400171218     C                   ENDIF
073500171218     C*
073600171218     C                   ENDSR
073700171218     C/EJECT
073800171218     C************************************************************
073900171218     C* CONTROLLO libro IVA
074000171218     C************************************************************
074100171218     C     CtrLibro      BEGSR
074200171218     C*
074300171218     C                   Clear                   D1LibroD
074400171218     C*
074500171218     C     D1Libro       IFEQ      *BLANK
074600171218     C                   SETON                                        5399
074700171218     C                   ENDIF
074800171218     C*
074900171218     C* CTRL ESISTENZA registro/LIBRO IN ANAGRAFICA LIBRI IVA
075000171218     C     D1Libro       IFNE      *BLANK
075100171218     C                   MOVE      WWWSOC        DLISOCIETA
075200171218     C                   MOVE      D1TpReg       DLITpReg
075300171218     C                   MOVE      D1Libro       DLILibro
075400171218     C     K03DLI01      CHAIN     ANDLI01L                           21
075500171218     C                   IF        *IN21=*OFF
075600171218     C                   MOVEL     DLIDESBREV    D1LibroD
075601171228     C* Verifico eventuali errori di incoerenza con il tipo regola
075602171228     C                   If        D1TpRic = '2' and DLISospens <> '9'
075603171228     C                   Seton                                        7899
075604171228     C                   Endif
075606171228     C                   If        D1TpRic <>'2' and DLISospens = '9'
075607171228     C                   Seton                                        7999
075608171228     C                   Endif
075700171218     c                   ELSE
075800171218     c                   SETON                                        5499
075900171218     C                   ENDIF
076000171218     C                   ENDIF
076100171218     C*
076200171218     C                   ENDSR
076300171218     C/EJECT
076400171218     C************************************************************
076500171218     C* CONTROLLO periodo di validità
076600171218     C************************************************************
076700171218     C     CtrDtVal      BEGSR
076800171218     C*
076900171222     C                   Clear                   WDtInVal
077000171222     C                   Clear                   WDtFiVal
077100171222     C                   Z-Add     0             WAnnoIn
077200171222     C                   Z-Add     0             WAnnoFi
077300171222     C*
077400171218     C     D1DtInVal     IFEQ      0
077500171218     C                   SETON                                        6099
077600171218     C                   ENDIF
077700171222     C*
077800171222     C     D1DtFiVal     IFEQ      0
077900171222     C                   SETON                                        7499
078000171222     C                   ENDIF
078100171218     C*
078200171218     C                   IF        D1DtInVal  <> 0
078300171218     C     *JobRun       TEST(D)                 D1DtInVal              23
078400171218     C                   IF        *IN23=*ON
078500171218     C                   SETON                                        5999
078600171218     C                   ENDIF
078700171218     C                   ENDIF
078800171218     C*
078900171218     C*
079000171218     C                   IF        D1DtFiVal  <> 0
079100171218     C     *JobRun       TEST(D)                 D1DtFiVal              23
079200171218     C                   IF        *IN23=*ON
079300171218     C                   SETON                                        6199
079400171218     C                   ENDIF
079500171218     C                   ENDIF
079600171218     C*
079700171218     C* CTRL DATA FINALE > DATA INIZIALE
079800171218     C                   IF        *IN59=*OFF and *IN61=*OFF
079900171218     C                   IF        D1DtInVal  <> 0
080000171218     C     *JobRun       MOVE      D1DtInVal     WDtInVal
080100171222     C*
080200171222     C                   Extrct    WDtInVal:*Y   WAnnoIn           4 0
080300171222     C*
080400171218     C                   ELSE
080500171218     C                   MOVE      *Loval        WDtInVal
080600171218     C                   ENDIF
080700171218     C                   IF        D1DtFiVal  <> 0
080800171218     C     *JobRun       MOVE      D1DtFiVal     WDtFiVal
080900171222     C*
081000171222     C                   Extrct    WDtFiVal:*Y   WAnnoFi           4 0
081100171222     C*
081200171218     C                   ELSE
081300171218     C                   MOVE      *Hival        WDtFiVal
081400171218     C                   ENDIF
081500171222     C*
081600171218     C                   IF        WDtInVal    > WDtFiVal
081700171218     C                   SETON                                        6299
081800171218     C                   ENDIF
081900171222     C*
082000171222     C                   IF        WAnnoIn    <> WAnnoFi  and
082100171222     C                             WAnnoIn     > 0        and
082200171222     C                             WAnnoFi     > 0
082300171222     C* devono appartenere allo stesso anno
082400171222     C                   SETON                                        7599
082500171222     C                   ENDIF
082600171222     C*
082700171218     C                   ENDIF
082800171218     C*
082900171218     C*
083000171218     C* CTRL SOVRAPPOSIZIONE PERIODI DI validità
083100171218     C                   IF        *IN59=*OFF and *IN61=*OFF
083200171218     C                   MOVE      WWWSoc        RICSocieta
083300171218     C                   MOVE      D1TpReg       RICTpReg
083400171218     C                   MOVE      D1Libro       RICLibro
083500171218     C     K03RIC01      SETLL     SDGRIC01L
083600171218     C*
083700171218     C                   DO        *HIVAL
083800171228     C     K03RIC01      READE(N)  SDGRIC01L                              22
083900171218     C                   IF        *IN22=*ON OR
084000171218     C                             *IN63=*ON
084100171218     C                   LEAVE
084200171218     C                   ENDIF
084300171228     C                   IF        OPZI31<> '01'      AND
084301171228     C                             OPZI31<> '03'      AND
084400171218     C* escludo controllo su record che viene modificato
084500171218     C                             SavSocieta = RICSocieta AND
084600171218     C                             SAVTpReg   = RICTpReg   AND
084700171218     C                             SAVLibro   = RICLibro   AND
084800171218     C                             SAVDtInVal = RICDtInVal
084900171218     C                   ITER
085000171218     C                   ELSE
085100171218     C* Il periodo inserito inizia in uno ancora valido
085200171218     C                   IF        WDtInVal >= RICDtInVal and
085300171218     C                             WDtInVal <= RICDtFiVal
085400171218     C                   SETON                                        6399
085500171218     C                   ENDIF
085600171218     C* Il periodo inserito finisce in uno ancora valido
085700171218     C                   IF        WDtFiVal   >= RICDtInVal and
085800171222     C                             WDtFiVal   <= RICDtFiVal and
085900171222     C                             WDtFiVal   <> *Loval     and
086000171222     C                             WDtFiVal   <> *Hival
086100171218     C                   SETON                                        6399
086200171218     C                   ENDIF
086300171218     C* Il periodo inserito è compreso in un altro
086400171218     C                   IF        WDtInVal   <= RICDtInVal AND
086500171222     C                             WDtFiVal   >= RICDtFiVal and
086600171222     C                             WDtFiVal   <> *Loval     and
086700171222     C                             WDtFiVal   <> *Hival
086800171218     C                   SETON                                        6399
086900171218     C                   ENDIF
087000171218     C                   ENDIF
087100171218     C                   ENDDO
087200171218     C*
087300171218     C* A fine lettura riposizionamento su record in modifica
087400171218     C                   IF        OPZI31='02'
087500171218     C                   MOVE      SAVSocieta    RICSocieta
087600171218     C                   MOVE      SAVTpReg      RICTpReg
087700171218     C                   MOVE      SAVLibro      RICLibro
087800171218     C                   MOVE      SAVDtInVal    RICDtInVal
087900171218     C     K04RIC01      CHAIN     SDGRIC01L                          21
088000171218     C                   ENDIF
088100171218     C                   ENDIF
088200171218     C*
088300171218     C                   ENDSR
088400171218     C/EJECT
088500171219     C************************************************************
088600171219     C* CONTROLLO tipo regola / periodo
088700171219     C************************************************************
088800171219     C     CtrTpRIC      BEGSR
088900171219     C*
089000171219     C                   Clear                   D1TpRICD
089100171219     C*
089200171219     C     D1TpRIC       IFEQ      *BLANK
089300171219     C                   SETON                                        5599
089400171219     C                   ENDIF
089500171219     C*
089600171219     C* CTRL VALIDITA' TIPO regola/periodo
089700171219     C     D1TpRIC       IFNE      *BLANK
089800171219     C                   RESET                   X07ValDS
089900171219     C                   MOVE      '1'           X07RIC
090000171219     C                   MOVEL     'SDGRIC000'   X07TRC
090100171219     C                   MOVEL     'RICTPRIC'    X07CAM
090200171219     C                   MOVEL     D1TpRIC       X07VAL
090300171219     C                   CALLb     'X07VALR'
090400171219     C                   PARM                    X07ValDS
090500171219     C     X07ERR        IFEQ      '0'
090600171219     C                   MOVEL     X07DES        D1TpRICD
090700171219     C                   ELSE
090800171219     C                   SETON                                          5699
090900171219     C                   ENDIF
091000171219     C                   ENDIF
091100171219     C*
091200171219     C                   ENDSR
091300171219     C/EJECT
091400171219     C************************************************************
091500171219     C* CONTROLLO descrizione regola
091600171219     C************************************************************
091700171219     C     CtrDescr      BEGSR
091800171219     C*
091900171219     C     D1Descr       IFEQ      *BLANK
092000171219     C                   Eval      D1Descr     = D1TpRICD
092100171219     C                   ENDIF
092200171219     C*
092300171219     C                   ENDSR
092400171219     C/EJECT
092401171228     C************************************************************
092402171228     C* CONTROLLO applicazione a fatture
092403171228     C************************************************************
092404171228     C     CtrFatt       BEGSR
092405171228     C*
092406171228     C                   if        D1Fatt     <> XScOn    and
092407171228     C                             D1Fatt     <> XScOff
092408171228     C                   SetOn                                          7699
092409171228     C                   endif
092410171228     C*
092411171228     C                   ENDSR
092412171228     C/EJECT
092500171219     C************************************************************
092600171219     C* CONTROLLO applicazione a note di accredito
092700171219     C************************************************************
092800171219     C     CtrNAcc       BEGSR
092900171219     C*
093000171219     C                   if        D1NAcc     <> XScOn    and
093100171219     C                             D1NAcc     <> XScOff
093200171222     C                   SetOn                                          7399
093300171219     C                   endif
093400171219     C*
093500171219     C                   ENDSR
093600171219     C/EJECT
093602171228     C************************************************************
093603171228     C* CONTROLLO che la regola sia almeno redito
093604171228     C************************************************************
093605171228     C     CtrFatNAc     BEGSR
093606171228     C*
093607171228     C                   if        D1Fatt     <> XScOn    and
093608171228     C                             D1NAcc     <> XScOn
093609171228     C                   SetOn                                          7799
093610171228     C                   endif
093611171228     C*
093612171228     C                   ENDSR
093613171228     C/EJECT
093700171219     C************************************************************
093800171219     C* CONTROLLO data di liquidazione proposta
093900171219     C************************************************************
094000171219     C     CtrDtLiq      BEGSR
094100171221     C*
094200171221     C                   MOVE      *Loval        WDtLiq
094300171219     C*
094400171219     C                   IF        D1DtLiq    <> 0
094500171219     C     *JobRun       TEST(D)                 D1DtLiq                23
094600171219     C                   IF        *IN23=*ON
094700171219     C                   SETON                                        6499
094800171221     C                   else
094900171221     C     *JobRun       MOVE      D1DtLiq       WDtLiq
095000171219     C                   ENDIF
095100171219     C                   ENDIF
095200171219     C*
095300171221     C                   if        D1TpRIC     = '1'     and
095400171221     C                             WDtLiq      = *Loval
095500171221     C                   SetOn                                        6699
095600171221     C                   endif
095700171221     C*
095800171221     C                   if        (D1TpRIC     = '2'   or
095900171221     C                              D1TpRIC     = '3'      ) and
096000171221     C                             WDtLiq     <> *Loval
096100171221     C                   SetOn                                        6999
096200171221     C                   endif
096300171221     C*
096400171219     C                   ENDSR
096500171219     C/EJECT
096600171219     C************************************************************
096700171219     C* CONTROLLO messaggio 1 incoerenza data documento
096800171219     C************************************************************
096900171219     C     CtrMsg1       BEGSR
097000171219     C*
097100171219     C                   if        D1Msg1     <> XScOn    and
097200171219     C                             D1Msg1     <> XScOff
097300171219     C                   SetOn                                          5799
097400171219     C                   endif
097500171221     C*
097600171221     C                   if        (D1TpRIC     = '1'   or
097700171221     C                              D1TpRIC     = '3'      ) and
097800171221     C                             D1Msg1      = XScOn
097900171221     C                   SetOn                                        7299
098000171221     C                   endif
098100171219     C*
098200171219     C                   ENDSR
098300171219     C/EJECT
098400171219     C************************************************************
098500171219     C* CONTROLLO messaggio 2 mancanza causale indetraibilità
098600171219     C************************************************************
098700171219     C     CtrMsg2       BEGSR
098800171219     C*
098900171219     C                   if        D1Msg2     <> XScOn    and
099000171219     C                             D1Msg2     <> XScOff
099100171219     C                   SetOn                                          5899
099200171219     C                   endif
099300171221     C*
099400171221     C                   if        (D1TpRIC     = '1'   or
099500171221     C                              D1TpRIC     = '2'      ) and
099600171221     C                             D1Msg2      = XScOn
099700171221     C                   SetOn                                        7099
099800171221     C                   endif
099900171219     C*
100000171219     C                   ENDSR
100100171221     C/EJECT
100200171221     C************************************************************
100300171221     C* CONTROLLO causale di indetraibilità
100400171221     C************************************************************
100500171221     C     CtrCausInd    BEGSR
100600171221     C*
100700171221     C                   Clear                   D1CausIndD
100800171221     C*
100900171221     C                   if        WDtFiVal   <> *Loval    and
101000171221     C                             WDtFiVal   <> *Hival
101100171221     C                   movel     WDtFiVal      WdtAlfa
101200171221     C                   Else
101300171221     C                   movel     ZdateIso      WdtAlfa
101400171221     C                   Endif
101500171221     C*
101600171221     C                   Clear                   ANOPE00F
101700171221     C*
101800171221     C                   if        (D1TpRIC     = '1'   or
101900171221     C                              D1TpRIC     = '2'      ) and
102000171221     C                             D1CausInd  <> *Blank
102100171221     C                   SetOn                                        7199
102200171221     C                   endif
102300171221     C*
102400171221     C                   if        D1CausInd  <> *Blank   and
102500171221     C                             D1TpRIC     = '3'
102600171221     C*
102700171221     C                   Eval      Lenout = %Size(ANOPE00F)
102800171221     C                   CALLB     'NDMVC003'
102900171221     C                   PARM                    WWWSoc
103000171221     C                   PARM                    D1CausInd
103100171221     C                   PARM                    WDtAlfa
103200171221     C                   PARM                    Uso
103300171221     C                   PARM      *Off          GesMsg
103400171221     C                   PARM      *Off          Esito
103500171221     C                   PARM      'ANOPE     '  Struttura
103600171221     C                   PARM                    ANOPE00F
103700171221     C                   PARM                    LenOut
103800171221     C                   PARM      *Off          Refresh
103900171221     C*
104000171221     C                   if        Esito     = *on
104100171221     C                   SetOn                                        6799
104200171221     C                   else
104300171221     C*
104400171221     C                   Eval      D1CausIndD  = OPEDesBrev
104500171221     C*
104600171221     C                   If        OpeTPC <>'2' OR
104700171221     C                             OPEInded <> *on or
104800171221     C                             OPEIndedP <> 100
104900171221     C                   Seton                                        6899
105000171221     C                   Endif
105100171221     C                   Endif
105200171221     C*
105300171221     C                   Endif
105400171221     C*
105500171221     C                   ENDSR
105600171219     C/EJECT
105700171218     C************************************************************
105800171218     C* AGGIORNAMENTO ANAGRAFICA
105900171218     C************************************************************
106000171218     C     AGGANA        BEGSR
106100171218     C*
106200171218     C                   SELECT
106300171218    >C     OPZI31        WHENEQ    '01'
106400171218    >C     OPZI31        OREQ      '03'
106500171218     C                   EXSR      RIEANA
106600171218    >C                   WRITE     SDGRIC000                            22
106700171218     C* se rcd non scrivibile attivo errore
106800171218     C     *IN22         IFEQ      *ON
106900171218     C                   SETON                                        9599
107000171218     C                   ENDIF
107100171218    >C     OPZI31        WHENEQ    '02'
107200171218     C                   EXSR      RIEANA
107300171218    >C                   UPDATE    SDGRIC000
107400171218    >C     OPZI31        WHENEQ    '04'
107500171218    >C                   DELETE    SDGRIC000
107600171218     C                   ENDSL
107700171218     C*
107800171218     C* richiama pgm User Exit
107900171218     c*
108000171218     C     USRERR        IFEQ      *BLANK
108100171218     C* imposta chiave univoca  per user exit
108200171218     C                   MOVE      XSCSOC        XUKSOC
108300171218     C                   MOVE      XSCDUN        XUKDUN
108400171218     C                   MOVEL     DSPGM         XUKPGM
108500171218     C                   MOVEL     RICTpReg      XUKFL1
108600171218     C                   MOVEL     RICLibro      XUKFL2
108700171218     C                   MOVEL     RICTpRIC      XUKFL3
108800171218     C                   MOVEL     XUKDS         KPJBU
108900171218     C     DSPGM         CAT       'U':0         USREXT           10
109000171218     C                   CALL      USREXT                               21
109100171218     C                   PARM                    KPJBA
109200171218     C     *IN21         IFEQ      *ON
109300171218     C                   MOVE      *ON           USRERR            1
109400171218     C                   ENDIF
109500171218     C                   ENDIF
109600171218     C                   ENDSR
109700171218     C/EJECT
109800171218     C************************************************************
109900171218     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
110000171218     C************************************************************
110100171218     C     RIEANA        BEGSR
110200171218     C*
110300171221     C*
110400171221     C                   if        OPZI31      = '01'   or
110500171221     C                             OPZI31      = '03'
110600171221     C*
110700171221     C                   CLEAR                   SDGRIC000
110800171221     C*
110900171221     C* imposto la data e l'utente di immissione
111000171221     C                   Eval      RICDtImm    = ZDateIso
111100171221     C                   Eval      RICUteImm   = XScCUt
111200171221     C*
111300171221     C                   endif
111400171221     C*
111500171221     C* imposto la data e l'utente di ultima modifica
111600171221     C                   Eval      RICDtUlMo   = ZDateIso
111700171221     C                   Eval      RICUteUlMo  = XScCUt
111800171221     C*
111900171218     C*
112000171218     C* imposto i campi del file da quelli video
112100171218     C*
112200171218     C                   MOVEL     WWWSoc        RICSocieta
112300171218     C                   MOVEL     D1TpReg       RICTpReg
112400171218     C                   MOVEL     D1Libro       RICLibro
112500171218     C                   MOVEL     D1TpRIC       RICTpRIC
112600171219     C                   MOVEL     D1Descr       RICDescr
112700171219     C*
112800171218     C                   IF        D1DtInVal  <> 0
112900171218     C     *JobRun       MOVE      D1DtInVal     RICDtInVal
113000171218     C                   ENDIF
113100171218     C                   IF        D1DtFiVal  <> 0
113200171218     C     *JobRun       MOVE      D1DtFiVal     RICDtFiVal
113300171218     C                   ELSE
113400171218     C                   MOVE      *HIVAL        RICDtFiVal
113500171218     C                   ENDIF
113501171228     C*
113502171228     C                   if        D1Fatt      = XScOn
113503171228     C                   MOVEL     *On           RICFatt
113504171228     C                   else
113505171228     C                   MOVEL     *Off          RICFatt
113506171228     C                   endif
113600171219     C*
113700171219     C                   if        D1NAcc      = XScOn
113800171219     C                   MOVEL     *On           RICNAcc
113900171219     C                   else
114000171219     C                   MOVEL     *Off          RICNAcc
114100171219     C                   endif
114200171219     C*
114300171219     C*
114400171219     C                   IF        D1DtLiq    <> 0
114500171219     C     *JobRun       MOVE      D1DtLiq       RICDtLiq
114600171222     C                   else
114700171222     C                   MOVE      *Loval        RICDtLiq
114800171219     C                   ENDIF
114900171219     C*
115000171219     C                   if        D1Msg1      = XScOn
115100171219     C                   MOVEL     *On           RICMsg1
115200171219     C                   else
115300171219     C                   MOVEL     *Off          RICMsg1
115400171219     C                   endif
115500171219     C*
115600171219     C                   if        D1Msg2      = XScOn
115700171219     C                   MOVEL     *On           RICMsg2
115800171219     C                   else
115900171219     C                   MOVEL     *Off          RICMsg2
116000171219     C                   endif
116100171218     C*
116200171221     C                   Movel     D1CausInd     RicCausInd
116300171221     C
116400171218     C                   ENDSR
116500171218     C/EJECT
116600171218     C************************************************************
116700171218     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
116800171218     C************************************************************
116900171218     C     GESAGG        BEGSR
117000171218     C*
117100171218     C* segnalo al pgm chiamante l'avvenuta conferma
117200171218    >C                   MOVE      *ON           OPRI31
117300171218     C* segnalo al pgm chiamante l'aver premuto l'F6
117400171218    >C                   MOVE      '0'           RETI31
117500171218     C*
117600171218     C                   SELECT
117700171218     C*
117800171218     C* nel caso di immissione
117900171218    >C     OPZI31        WHENEQ    '01'
118000171218     C* ripristino la videata iniziale
118100171218     C                   MOVE      *ON           $INZD1
118200171218     C                   MOVE      'D1'          $GEST
118300171218     C*
118400171218     C* nel caso di modifica
118500171218    >C     OPZI31        WHENEQ    '02'
118600171218     C* ripasso i campi che devono essere reimpostati sul sfl
118700171218     C                   MOVE      RICTpRIC      TpRICI31
118800171219     C                   MOVEL     RICDescr      DescrI31
118900171218     C                   MOVE      RICDtInVal    DtInValI31
119000171218     C                   MOVE      RICDtFiVal    DtFiValI31
119001171228     C                   MOVE      RICFatt       FattI31
119100171219     C                   MOVE      RICNAcc       NAccI31
119200171218     C* e ritorno al pgm chiamante
119300171218     C                   MOVE      *ON           $FINE
119400171218     C*
119500171218     C* per tutte le altre opzioni
119600171218     C                   OTHER
119700171218     C* ritorno al pgm chiamante
119800171218     C                   MOVE      *ON           $FINE
119900171218     C                   ENDSL
120000171218     C*
120100171218     C                   ENDSR
120200171218     C/EJECT
120300171218     C************************************************************
120400171218     C* CONTROLLO ABILITAZIONE TASTI
120500171218     C************************************************************
120600171218     C     CTRKEY        BEGSR
120700171218     C*
120800171218     C                   SELECT
120900171218     C     $TASTO        WHENEQ    F01
121000171218     C     $FC(1)        IFEQ      '0'
121100171218     C                   SETON                                        99
121200171218     C                   ENDIF
121300171218     C     $TASTO        WHENEQ    F02
121400171218     C     $FC(2)        IFEQ      '0'
121500171218     C                   SETON                                        99
121600171218     C                   ENDIF
121700171218     C     $TASTO        WHENEQ    F03
121800171218     C     $FC(3)        IFEQ      '0'
121900171218     C                   SETON                                        99
122000171218     C                   ENDIF
122100171218     C     $TASTO        WHENEQ    F04
122200171218     C     $FC(4)        IFEQ      '0'
122300171218     C                   SETON                                        99
122400171218     C                   ENDIF
122500171218     C     $TASTO        WHENEQ    F05
122600171218     C     $FC(5)        IFEQ      '0'
122700171218     C                   SETON                                        99
122800171218     C                   ENDIF
122900171218     C     $TASTO        WHENEQ    F06
123000171218     C     $FC(6)        IFEQ      '0'
123100171218     C                   SETON                                        99
123200171218     C                   ENDIF
123300171218     C     $TASTO        WHENEQ    F07
123400171218     C     $FC(7)        IFEQ      '0'
123500171218     C                   SETON                                        99
123600171218     C                   ENDIF
123700171218     C     $TASTO        WHENEQ    F08
123800171218     C     $FC(8)        IFEQ      '0'
123900171218     C                   SETON                                        99
124000171218     C                   ENDIF
124100171218     C     $TASTO        WHENEQ    F09
124200171218     C     $FC(09)       IFEQ      '0'
124300171218     C                   SETON                                        99
124400171218     C                   ENDIF
124500171218     C     $TASTO        WHENEQ    F10
124600171218     C     $FC(10)       IFEQ      '0'
124700171218     C                   SETON                                        99
124800171218     C                   ENDIF
124900171218     C     $TASTO        WHENEQ    F11
125000171218     C     $FC(11)       IFEQ      '0'
125100171218     C                   SETON                                        99
125200171218     C                   ENDIF
125300171218     C     $TASTO        WHENEQ    F12
125400171218     C     $FC(12)       IFEQ      '0'
125500171218     C                   SETON                                        99
125600171218     C                   ENDIF
125700171218     C     $TASTO        WHENEQ    F13
125800171218     C     $FC(13)       IFEQ      '0'
125900171218     C                   SETON                                        99
126000171218     C                   ENDIF
126100171218     C     $TASTO        WHENEQ    F14
126200171218     C     $FC(14)       IFEQ      '0'
126300171218     C                   SETON                                        99
126400171218     C                   ENDIF
126500171218     C     $TASTO        WHENEQ    F15
126600171218     C     $FC(15)       IFEQ      '0'
126700171218     C                   SETON                                        99
126800171218     C                   ENDIF
126900171218     C     $TASTO        WHENEQ    F16
127000171218     C     $FC(16)       IFEQ      '0'
127100171218     C                   SETON                                        99
127200171218     C                   ENDIF
127300171218     C     $TASTO        WHENEQ    F17
127400171218     C     $FC(17)       IFEQ      '0'
127500171218     C                   SETON                                        99
127600171218     C                   ENDIF
127700171218     C     $TASTO        WHENEQ    F18
127800171218     C     $FC(18)       IFEQ      '0'
127900171218     C                   SETON                                        99
128000171218     C                   ENDIF
128100171218     C     $TASTO        WHENEQ    F19
128200171218     C     $FC(19)       IFEQ      '0'
128300171218     C                   SETON                                        99
128400171218     C                   ENDIF
128500171218     C     $TASTO        WHENEQ    F20
128600171218     C     $FC(20)       IFEQ      '0'
128700171218     C                   SETON                                        99
128800171218     C                   ENDIF
128900171218     C     $TASTO        WHENEQ    F21
129000171218     C     $FC(21)       IFEQ      '0'
129100171218     C                   SETON                                        99
129200171218     C                   ENDIF
129300171218     C     $TASTO        WHENEQ    F22
129400171218     C     $FC(22)       IFEQ      '0'
129500171218     C                   SETON                                        99
129600171218     C                   ENDIF
129700171218     C     $TASTO        WHENEQ    F23
129800171218     C     $FC(23)       IFEQ      '0'
129900171218     C                   SETON                                        99
130000171218     C                   ENDIF
130100171218     C     $TASTO        WHENEQ    F24
130200171218     C     $FC(24)       IFEQ      '0'
130300171218     C                   SETON                                        99
130400171218     C                   ENDIF
130500171218     C                   ENDSL
130600171218     C*
130700171218     C                   ENDSR
130800171218     C/EJECT
130900171218     C******************************************************
131000171218     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
131100171218     C******************************************************
131200171218     C     MSGPGM        BEGSR
131300171218     C*
131400171218     C                   Z-ADD     2             MSGMSG
131500171218     C                   MOVEL     'S'           MSGRSP
131600171218     C                   MOVEL     'CR'          MSGTPR
131700171218     C                   MOVEL     'S'           MSGEMV
131800171218     C                   Z-ADD     10            MSGRGP
131900171218     C                   MOVEL     'S'           MSGLCK
132000171218     C                   MOVEA     STATUS        STS
132100171218     C                   MOVEL     'N'           MSGCNL
132200171218     C                   MOVEL     'N'           MSGVID
132300171218     C                   MOVEL     'N'           MSGOPE
132400171218     C                   MOVEL     'N'           MSGSTP
132500171218     C                   CALL      'XMSGR'
132600171218     C                   PARM                    XMSGDS
132700171218     C*
132800171218     C                   ENDSR
132900171218     C/EJECT
133000171218     C************************************************************
133100171218     C* OPERAZIONI INIZIALI
133200171218     C************************************************************
133300171218     C     *INZSR        BEGSR
133400171218     C*
133500171218     C     *DMY          Move      Udate         ZDateISO
133600171218     C     *JobRun       Move      ZDateISO      ZDate             6 0
133700171218     C*
133800171218     C* Reperimento parametri
133900171218     C*
134000171218     C     *ENTRY        PLIST
134100171218     C                   PARM                    KPJBA
134200171218     C                   PARM                    DSAUT
134300171218     C*
134400171218     C     PARMS         IFLT      2
134500171218     C                   MOVE      *BLANK        AUTDS
134600171218     C                   ELSE
134700171218     C                   MOVEL     DSAUT         AUTDS
134800171218     C                   ENDIF
134900171218     C*
135000171218     C* Reperimento tasti di funzione
135100171218     C                   EXSR      REPKEY
135200171218     C*
135300171218     C                   ENDSR
135400171218     C/EJECT
135500171218     C************************************************************
135600171218     C* REPERIMENTO TASTI DI FUNZIONE
135700171218     C************************************************************
135800171218     C     REPKEY        BEGSR
135900171218     C                   MOVEA     $FM1          $FM
136000171218     C                   MOVEA     $Fc1          $Fc
136100171218     C                   EXSR      REPKKK
136200171218     C                   MOVEA     $FX           $FX1
136300171218     C*
136400171218     C                   ENDSR
136500171218     C/EJECT
136600171218     C************************************************************
136700171218     C* REPERIMENTO TASTI DI FUNZIONE
136800171218     C************************************************************
136900171218     C     REPKKK        BEGSR
137000171218     C*
137100171218     C                   CLEAR                   $FX
137200171218     C     1             DO        24            X
137300171218     C*
137400171218     C     $FM(X)        IFNE      *BLANK
137500171218     C     $FM(X)        andne     '0'
137600171218     C*
137700171218     C                   MOVEL     $FM(X)        IDMSG
137800171218     C                   CALLb     'XRTVM'
137900171218     C                   PARM                    IDMSG            27
138000171218     C                   PARM                    TXTMSG          100
138100171218     C                   PARM                    ERRMSG            1
138200171218     C*
138300171218     C     ERRMSG        IFNE      *ON
138400171218     C                   MOVEL     TXTMSG        $FX(X)
138500171218     C                   ELSE
138600171218     C                   MOVEL     *ALL' '       $FX(X)
138700171218     C                   ENDIF
138800171218     C*
138900171218     C                   ENDIF
139000171218     C*
139100171218     C                   ENDDO
139200171218     C*
139300171218     C                   ENDSR
139400171218     C/EJECT
139500171218     C************************************************************
139600171218     C* INIZIALIZZAZIONE VARIABILI
139700171218     C************************************************************
139800171218     C     INZVAR        BEGSR
139900171218     C*
140000171218     C* Dati da PGM guida
140100171218     C*
140200171218    >C                   MOVEL     KPJBU         SDGSI31DS
140300171218     C*
140400171218     C* Reperimento dati società
140500171218     C*
140600171218    >C                   MOVEL     'SOC001'      TIPXSC
140700171218     C                   MOVEL     *BLANK        SOCXSC
140800171218     C                   EXSR      REPSOC
140900171218     C     RTNXSC        IFNE      '1'
141000171218     C                   MOVEL     XSOCDS        SOC001
141100171218     C                   ELSE
141200171218     C                   RETURN
141300171218     C                   ENDIF
141400171218     C*
141500171218     C* Reperimento autorizzazioni
141600171218     C*
141700171218     C*  nel caso l'anagrafica non gestisse le autorizzazioni
141800171218     C*   asteriscare la chiamata alla subroutine REPAUT
141900171218     C* le autorizzazioni potrebbero essere già state passate,
142000171218     C* nel qual caso non vanno reperite
142100171218     C     AUTDS         IFEQ      *BLANK
142200171218     C                   EXSR      REPAUT
142300171218     C                   ELSE
142400171218     C                   MOVEL     XCADDS        ANGIVADS
142500171218     C                   ENDIF
142600171218     C*
142700171218     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
142800171218     C* le autorizzazione
142900171218     C*
143000171218    >C*  nel caso l'anagrafica non gestisse le autorizzazioni
143100171218    >C*   asteriscare la chiamata alla subroutine TSTAUT
143200171218    >C                   EXSR      TSTAUT
143300171218     C*
143400171218     C* Definizione dei tasti funzione utilizzabili a pgm
143500171218     C*
143600171218     C                   EXSR      DEFOPZ
143700171218     C*
143800171218     C* Variabili per gestione videate
143900171218     C*
144000171218     C                   MOVE      'D1'          $GEST                          fmt in gestione
144100171218     C                   MOVE      *OFF          $FINE                          fine pgm
144200171218     C                   MOVE      *ON           $INZD1                         inizializzione D1
144300171218     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
144400171218     C                   MOVE      *BLANK        $LASTV                         ultima emissione
144500171218     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
144600171218     C                   MOVE      *Loval        DefDtInVal
144700171218     C                   MOVE      *Hival        DefDtFiVal
144800171218     C*
144900171218     C* Variabili appoggio
145000171218     C*
145100171218     C                   Z-ADD     0             CURR
145200171218     C                   Z-ADD     0             CURC
145300171218     C*
145400171218     C* Valorizzazione campi univoci testate
145500171218     C*
145600171218     C                   CLEAR                   FMTT1
145700171218     C                   MOVEL     KNSIF         NOMSIF
145800171218     C                   MOVEL     XSCDSI        NOMDIT
145900171219     C                   MOVEL     KCDAZ         NOMAZN
146000171218     C*****              MOVEL     DSPGM         NOMPGM
146100171218     C* recupero il sottotitolo (se c'è)
146200171218     C                   MOVEL     *BLANK        T1TIT
146300171218     C                   MOVEL     *BLANK        IDMSG
146400171218     C                   CALLb     'XRTVM'
146500171218     C                   PARM                    IDMSG            27
146600171218     C                   PARM                    TXTMSG          100
146700171218     C                   PARM                    ERRMSG            1
146800171218     C                   PARM      *ON           CTRMSG            1            centratura
146900171218     C                   PARM      30            LENMSG            3 0          lun output.
147000171218     C     ERRMSG        IFNE      *ON
147100171218     C                   MOVEL     TXTMSG        T1TIT
147200171218     C                   ELSE
147300171218     C                   ENDIF
147400171218     C* recupero il tipo operazione
147500171218     C                   Z-ADD     1             X
147600171218     C     OPZI31        LOOKUP    $OZ(X)                                 21
147700171218     C     *IN21         IFEQ      *ON
147800171218     C                   MOVEL     $ID(X)        IDMSG
147900171218     C                   CALLb     'XRTVM'
148000171218     C                   PARM                    IDMSG            27
148100171218     C                   PARM                    TXTMSG          100
148200171218     C                   PARM                    ERRMSG            1
148300171219     C                   PARM      *Off          CTRMSG            1            centratura
148400171218     C                   PARM      30            LENMSG            3 0          lun output.
148500171218     C     ERRMSG        IFNE      *ON
148600171218     C                   MOVEL     TXTMSG        T1OPE
148700171218     C                   ELSE
148800171218     C                   MOVEL     *ALL'?'       T1OPE
148900171218     C                   ENDIF
149000171218     C                   ENDIF
149100171218     C*
149200171218     C* Imposto campo WWWSOC
149300171218     C*
149400171218     C     FSCI31        IFEQ      'S'
149500171218     C                   MOVE      XSCSOC        WWWSOC
149600171218     C                   ELSE
149700171218     C                   MOVE      XSCCPA        WWWSOC
149800171218     C                   END
149900171218     C*
150000171218     C* Reperisco e memorizzo default in caso di immissione
150100171218     C*
150200171218    >C* azzero tutti i campi di appoggio default
150300171218     C     OPZI31        IFEQ      '01'
150400171218     C* costruisco la chiave univoca per il reperimento dei default
150500171218     C                   MOVE      WWWSOC        RICSocieta
150600171218     C                   MOVE      DF1I31        RICTpReg
150700171218     C                   MOVE      DF2I31        RICLibro
150800171218     C                   MOVE      DtInValI31    RICDtInVal
150900171228     C     K04RIC01      CHAIN(N)  SDGRIC01L                          21
151000171218     C     *IN21         IFEQ      *OFF
151100171218     C                   EXSR      REPDEF
151200171218     C                   ENDIF
151300171218     C                   ENDIF
151400171218     C*
151500171218     C* Inizializzazione/Decodifica prima videata
151600171218     C*
151700171218     C                   EXSR      INZD1
151800171218     C                   MOVE      *OFF          $INZD1
151900171218     C*
152000171218     C                   ENDSR
152100171218     C/EJECT
152200171218     C************************************************************
152300171218     C* REPERIMENTO DATI SOCIETA'
152400171218     C************************************************************
152500171218     C     REPSOC        BEGSR
152600171218     C*
152700171218     C                   CALLb     'XSOC'
152800171218     C                   PARM                    TIPXSC            6
152900171218     C                   PARM                    SOCXSC            3
153000171218     C                   PARM                    CDSXSC            9 0
153100171218     C                   PARM                    MODXSC            3
153200171218     C                   PARM      *BLANKS       RTNXSC            1
153300171218     C                   PARM                    XSOCDS
153400171218     C                   PARM                    KPJBA
153500171218     C*
153600171218     C                   ENDSR
153700171218     C/EJECT
153800171218     C************************************************************
153900171218     C* REPERIMENTO AUTORIZZAZIONI UTENTE
154000171218     C************************************************************
154100171218     C     REPAUT        BEGSR
154200171218     C*
154300171218     C                   CLEAR                   AUTDS
154400171218     C*
154500171218     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
154600171218     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
154700171218     C                   Z-ADD     0             XCAGRP                         GRUPPO
154800171218     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
154900171218     C                   MOVEL     'ANGIVA'      XCAFNC                         FUNZIONE
155000171218    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
155100171218    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
155200171218    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
155300171218     C*
155400171218     C                   CALLb     'XCHKAUT'
155500171218     C                   PARM                    AUTDS
155600171218     C*
155700171218     C* errori/non abilitazione
155800171218     C     XCARTN        IFNE      0
155900171218     C                   EXSR      ENDPGM
156000171218     C                   ELSE
156100171218     C                   MOVEL     XCADDS        ANGIVADS
156200171218     C                   ENDIF
156300171218     C*
156400171218     C                   ENDSR
156500171218     C/EJECT
156600171218     C************************************************************
156700171218     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
156800171218     C************************************************************
156900171218     C     TSTAUT        BEGSR
157000171218     C*
157100171218     C                   SELECT
157200171218     C*
157300171218     C* se richiamato in immissione
157400171218     C     OPZI31        WHENEQ    '01'
157500171218     C* ma l'utente non è abilitato
157600171218     C     IMMDLI        ANDEQ     *OFF
157700171218     C* esco da pgm avvertendolo
157800171218     C                   MOVE      '2'           ERRI31
157900171218     C                   MOVE      *ON           $FINE
158000171218     C                   EXSR      ENDPGM
158100171218     C*
158200171218     C* se richiamato in modifica
158300171218    >C     OPZI31        WHENEQ    '02'
158400171218     C* ma l'utente non è abilitato
158500171218     C     MANDLI        ANDEQ     *OFF
158600171218     C* testo se l'utente è abilitato almeno alla visualizzazione
158700171218     C     VISDLI        IFEQ      *ON
158800171218    >C                   MOVE      '05'          OPZI31
158900171218     C                   ELSE
159000171218     C* altrimenti esco da pgm avvertendolo
159100171218    >C                   MOVE      '2'           ERRI31
159200171218     C                   MOVE      *ON           $FINE
159300171218     C                   EXSR      ENDPGM
159400171218     C                   ENDIF
159500171218     C*
159600171218     C* se richiamato in copia
159700171218    >C     OPZI31        WHENEQ    '03'
159800171218     C* ma l'utente non è abilitato
159900171218     C     IMMDLI        ANDEQ     *OFF
160000171218     C* esco da pgm avvertendolo
160100171218    >C                   MOVE      '2'           ERRI31
160200171218     C                   MOVE      *ON           $FINE
160300171218     C                   EXSR      ENDPGM
160400171218     C*
160500171218     C* se richiamato in annullamento
160600171218    >C     OPZI31        WHENEQ    '04'
160700171218     C* ma l'utente non è abilitato
160800171218     C     ANNDLI        ANDEQ     *OFF
160900171218     C* esco da pgm avvertendolo
161000171218    >C                   MOVE      '2'           ERRI31
161100171218     C                   MOVE      *ON           $FINE
161200171218     C                   EXSR      ENDPGM
161300171218     C*
161400171218     C* se richiamato in visualizzazione
161500171218    >C     OPZI31        WHENEQ    '05'
161600171218     C* ma l'utente non è abilitato
161700171218     C     VISDLI        ANDEQ     *OFF
161800171218     C* esco da pgm avvertendolo
161900171218    >C                   MOVE      '2'           ERRI31
162000171218     C                   MOVE      *ON           $FINE
162100171218     C                   EXSR      ENDPGM
162200171218     C*
162300171218     C                   ENDSL
162400171218     C*
162500171218     C                   ENDSR
162600171218     C/EJECT
162700171218     C************************************************************
162800171218     C* REPERIMENTO PARAMETRI GESTIONE
162900171218     C************************************************************
163000171218     C     REPPAR        BEGSR
163100171218     C*
163200171218     C                   CALLb     'XPAR'
163300171218     C                   PARM                    XPASOC            3
163400171218     C                   PARM                    XPAPAR            8
163500171218     C                   PARM                    XPAOUT
163600171218     C                   PARM                    XPAERR            1
163700171218     C                   PARM                    XPARIC            1
163800171218     C*
163900171218     C                   ENDSR
164000171218     C/EJECT
164100171218     C************************************************************
164200171218     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
164300171218     C************************************************************
164400171218     C     DEFOPZ        BEGSR
164500171218     C*
164600171218     C* Questa routine deve valorizzare la schiera $FC1/$FR1
164700171218     C* con S o N a seconda della utilizzabilità o meno
164800171218     C* del tasto funzione correlato
164900171218     C*
165000171218     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
165100171218     C* di compilazione
165200171218     C                   RESET                   $FC1
165300171218     C                   RESET                   $FR1
165400171218     C*
165500171218     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
165600171218     C* invalido alcune opzioni
165700171218     C                   SELECT
165800171218     C* Se pgm richiamato in interrogazione disattivo :
165900171218    >C     OPZI31        WHENEQ    '05'
166000171218     C* F4=Ricerca
166100171218     C                   Z-ADD     4             X
166200171218     C                   MOVE      '0'           $FC1(X)
166300171218     C* F5=Ripristino
166400171218     C                   Z-ADD     5             X
166500171218     C                   MOVE      '0'           $FC1(X)
166600171218     C* F6=Conferma
166700171218     C                   Z-ADD     06            X
166800171218     C                   MOVE      '0'           $FC1(X)
166900171218     C* Se pgm richiamato in annullamento disattivo :
167000171218    >C     OPZI31        WHENEQ    '04'
167100171218     C* F4=Ricerca
167200171218     C                   Z-ADD     4             X
167300171218     C                   MOVE      '0'           $FC1(X)
167400171218     C* F5=Ripristino
167500171218     C                   Z-ADD     5             X
167600171218     C                   MOVE      '0'           $FC1(X)
167700171218     C*
167800171218     C                   OTHER
167900171218     C*
168000171218     C                   ENDSL
168100171218     C*
168200171218     C                   ENDSR
168300171218     C/EJECT
168400171218     C************************************************************
168500171218     C* REPERIMENTO CAMPI DI DEFAULT
168600171218     C************************************************************
168700171218     C     REPDEF        BEGSR
168800171218     C*
168900171218     C                   MOVE      RICDtInVal    DefDtInVal
169000171218     C                   MOVE      RICDtFiVal    DefDtFiVal
169100171218     C*
169200171218     C                   ENDSR
169300171218     C************************************************************
169400171218     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
169500171218     C************************************************************
169600171218     C     DEFCAM        BEGSR
169700171218     C*
169800171218     C* klist
169900171218     C*
170000171218     C* SDGRIC01L
170100171218     C     K04RIC01      KLIST
170200171218     C                   KFLD                    RICSocieta
170300171218     C                   KFLD                    RICTpReg
170400171218     C                   KFLD                    RICLibro
170500171219     C                   KFLD                    RICDtInVal
170600171218     C*
170700171218     C     K03RIC01      KLIST
170800171218     C                   KFLD                    RICSocieta
170900171218     C                   KFLD                    RICTpReg
171000171218     C                   KFLD                    RICLibro
171100171218     C*
171200171218     C* ANDLI01L
171300171218     C     K03DLI01      KLIST
171400171218     C                   KFLD                    DLISocieta
171500171218     C                   KFLD                    DLITpReg
171600171218     C                   KFLD                    DLILibro
171700171218     C*
171800171218     C* Variabili per gestione videate
171900171218     C*
172000171218     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
172100171218     C                   MOVE      *BLANK        $FINE             1            fine pgm
172200171218     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
172300171218     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
172400171218     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
172500171218     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
172600171218     C*
172700171218     C* Variabili appoggio
172800171218     C*
172900171218     C                   Z-ADD     0             CURR              2 0
173000171218     C                   Z-ADD     0             CURC              2 0
173100171218     C                   MOVE      *ZEROS        WIN              99
173200171218    >C* elenco tutti i campi di appoggio default
173300171218     C     *LIKE         DEFINE    XSCSOC        WWWSOC
173400171218     C     *LIKE         DEFINE    RICTpReg      WWTpReg
173500171219     C     *LIKE         DEFINE    RICLibro      WWLibro
173600171218     C     *LIKE         DEFINE    RICDtInVal    WDtInVal
173700171218     C     *LIKE         DEFINE    RICDtFiVal    WDtFiVal
173800171218     C     *LIKE         DEFINE    RICDtInVal    DefDtInVal
173900171218     C     *LIKE         DEFINE    RICDtFiVal    DefDtFiVal
174000171218     C     *LIKE         DEFINE    RICSocieta    SAVSocieta
174100171218     C     *LIKE         DEFINE    RICTpReg      SAVTpReg
174200171218     C     *LIKE         DEFINE    RICLibro      SAVLibro
174300171218     C     *LIKE         DEFINE    RICDtInVal    SAVDtInVal
174400171221     C     *LIKE         DEFINE    RICDtLiq      WDtLiq
174500171218     C*
174600171218     C* Indici
174700171218     C*
174800171218     C                   Z-ADD     0             X                 3 0
174900171218     C                   Z-ADD     0             Y                 3 0
175000171218     C*
175100171218     C                   ENDSR
175200171218     C/EJECT
175300171218     C************************************************************
175400171218     C* RIEMPIMENTO TASTI FUNZIONE
175500171218     C************************************************************
175600171218     C     RIEKEY        BEGSR
175700171218     C*
175800171218     C* Input :
175900171218     C* - $FX = schiera con tutti i testi disponibili
176000171218     C* - $FC = schiere con i flag di validità associati ai testi
176100171218     C* - $FR = schiere con i flag di REVERSE IMAGE
176200171218     C* - $ULK = ultimo testo caricato
176300171218     C* - $L01 = Lunghezza prima riga
176400171218     C* - $L02 = Lunghezza seconda riga
176500171218     C* Output :
176600171218     C* - $ULK
176700171218     C* - $KEY1= 1° riga di tasti funzione
176800171218     C* - $KEY2= 2° riga di tasti funzione
176900171218     C*
177000171218     C                   CALLb     'XKEYMSG'
177100171218     C* input
177200171218     C                   PARM                    $FX
177300171218     C                   PARM                    $FC
177400171218     C                   PARM                    $FR
177500171218     C                   PARM                    $ULK              3 0
177600171218     C* output
177700171218     C                   PARM                    $KEY1            79
177800171218     C                   PARM                    $KEY2            79
177900171218    >C* input
178000171218    >C                   PARM      79            $L01              3 0
178100171218    >C                   PARM      79            $L02              3 0
178200171218     C*
178300171218     C                   PARM                    $ALLFUNCT       480
178400171218     C*
178500171218     C                   ENDSR
178600171218**
17870017121801PROMSG    *LIBL     COS0001
17880017121802PROMSG    *LIBL     COS0002
17890017121803PROMSG    *LIBL     COS0003
17900017121804PROMSG    *LIBL     COS0004
17910017121805PROMSG    *LIBL     COS0005
179200171218** TASTI DI FUNZIONE  D1
179300171218PROMSG    *LIBL     KEY0001  01
179400171218PROMSG    *LIBL     KEY0002  02
179500171218PROMSG    *LIBL     KEY0003  03
179600171218PROMSG    *LIBL     KEY0004  04
179700171218PROMSG    *LIBL     KEY0005  05
179800171218PROMSG    *LIBL     KEY0006  06
179900171218PROMSG    *LIBL     KEY0007  07
180000171218PROMSG    *LIBL     KEY0008  08
180100171218PROMSG    *LIBL     KEY0009  09
180200171218PROMSG    *LIBL     KEY0010  10
180300171218PROMSG    *LIBL     KEY0011  11
180400171218PROMSG    *LIBL     KEY0012  12
180500171218PROMSG    *LIBL     KEY0013  13
180600171218PROMSG    *LIBL     KEY0014  14
180700171218PROMSG    *LIBL     KEY0015  15
180800171218PROMSG    *LIBL     KEY0016  16
180900171218PROMSG    *LIBL     KEY0017  17
181000171218PROMSG    *LIBL     KEY0018  18
181100171218PROMSG    *LIBL     KEY0019  19
181200171218PROMSG    *LIBL     KEY0020  20
181300171218PROMSG    *LIBL     KEY0021  21
181400171218PROMSG    *LIBL     KEY0022  22
181500171218PROMSG    *LIBL     KEY0023  23
181600171218PROMSG    *LIBL     KEY0024  24
181700171218**  ABILITAZIONE D1
1818001712180N  01
1819001712180N  02
1820001712181N  03
1821001712181N  04
1822001712181N  05
1823001712181N  06
1824001712180N  07
1825001712181N  08
1826001712180N  09
1827001712180N  10
1828001712180N  11
1829001712181N  12
1830001712180N  13
1831001712180N  14
1832001712180N  15
1833001712180N  16
1834001712180N  17
1835001712180N  18
1836001712180N  19
1837001712180N  20
1838001712180N  21
1839001712180N  22
1840001712180N  23   Impostare a '1' se si utilizzano opzioni
1841001712181N  24   Lasciare sempre impostata a '1' il valore della riga 24
