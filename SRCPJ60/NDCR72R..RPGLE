000100981211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300951122     H*PARMS CVTOPT(*DATETIME)
000400951122     H DECEDIT('0,') DATEDIT(*DMY.)
000500951122     F*---------------------------------------------------------------*
000600971023     F*         ESTRAZIONE RIGHE PERCIPIENTE CON TOTALI PER:          *
000700971023     F*         - TRIBUTO/PROGRESSIVO TRIBUTO                         *
000800971023     F*          OPPURE                                               *
000900971023     F*         - CODICE DI RAGGRUPPAMENTO TRIBUTO                    *
001000000000     F*---------------------------------------------------------------*
001100951123     F*    INDICATORI                                                 *
001200000000     F*                                                               *
001300951122     F* 90 FINE FILE                                                  *
001400971009     F* 91 CHAIN ANTRI00F                                             *
001500951123     F* 50 LOOKUP SU TABELLA RAGGRUPPAMENTI TRIBUTI                   *
001600951123     F*    50 = OFF  INSERIMENTO NELLA TABELLA RAGGR. TRIBUTI         *
001700951123     F*    50 = ON   TOTALIZZAZIONE IMPORTI TABELLA RAGGR. TRIBUTI    *
001800951123     F* 51 LOOKUP CON *BLANK PER REPERIRE PRIMO LIBERO                *
001900951123     F* 60 INDICATORE DI OCCUR SU DS MULTIPLA RAGGRUPPAMENTI          *
002000951123     F*---------------------------------------------------------------*
002100951123     F*  --- FLUSSO PRINCIPALE ---                                    *
002200000000     F*                                                               *
002300000000     F*  - CANCELLAZIONE RECORD ESISTENTI PER L'ANNO DI COMPETENZA    *
002400951122     F*    SELEZIONATO NEL FILE ND77001L                              *
002500000000     F*                                                               *
002600000000     F*  - LETTURA ANAGRAFICO PERCIPIENTI                             *
002700000000     F*                                                               *
002800000000     F*  - LETTURA RITENUTE DEL PERCIPIENTE                           *
002900000000     F*                                                               *
003000000000     F*  - SELEZIONE DELLE RITENUTE CHE HANNO:                        *
003100000000     F*               1 - ANNO DI COMPETENZA SELEZIONATO A VIDEO      *
003200000000     F*               2 - STATO=3 (QUIETANZATI)                       *
003300000000     F*               3 - ALMENO UN IMPORTO DIVERSO DA 0              *
003400990422     F*               4 - QUADRO = SC - SE                            *
003500000000     F*                                                               *
003600000000     F*  - INSERIMENTO DEI MOVIMENTI ELABORATI IN UNA DS MULTIPLA     *
003700971023     F*    CHE LI RACCOGLIE PER TRIBUTO/PROGR. O COD. RAGGRUPPAMENTO  *
003800000000     F*                                                               *
003900000000     F*  - TOTALIZZAZIONE DEGLI IMPORTI NEL CASO IL MOVIMENTO APPAR-  *
004000971023     F*    TENGA AD UN TRIBUTO/PROGR. O COD. RAGGR. GIA' INSERITO     *
004100000000     F*                                                               *
004200000000     F*  - LETTURA NUOVO RECORD RITENUTE                              *
004300000000     F*                                                               *
004400000000     F*  - PER FINE MOVIMENTI DEL PERCIPIENTE MEMORIZZO DS MULTIPLA   *
004500951122     F*    NEL FILE ND77001L.                                         *
004600000000     F*                                                               *
004700000000     F*  - LETTURA NUOVO RECORD PERCIPIENTI                           *
004800000000     F*                                                               *
004900000000     F*  - FINE FILE PERCIPIENTI VAI A FINE                           *
005000000000     F*---------------------------------------------------------------*
005100951122     FANAPE01L  IF   E           K DISK
005200951124     F*------------
005300951123     FNDMRA01L  IF   E           K DISK
005400951124     F*------------
005500951122     FANTRI01L  IF   E           K DISK
005600951124     F*------------
005700951122     FND77001L  UF A E           K DISK
005800951122     F*---------------------------------------------------------------*
005900951122     D*------------
006000951122     D KPJBA         E DS
006100951122     D*-------------
006200951122     D ndcr70ds      E DS
006300951122     D*------------
006400951122     D* Richiamo a XATB
006500951122     D XATBDS        E DS
006600951123     D TABG16        E DS                  EXTNAME(ANGG16DS) INZ
006700040930     D*------------
006800040930C1788D* Tabella esclusione tributi 2%
006900040930C1788D TAB022        E DS                  EXTNAME(ANG022DS) INZ
007000951122     D*------------
007100951122     D* Definizioni parametri driver soggetti
007200951122     D DVARIC          S              1
007300951122     D DVASOCIETA      S              3
007400951122     D DVAUNITA        S              8
007500951122     D DVADTRIF        S               D
007600951122     D DVASTRUTT       S             10
007700951122     D DVALENOUT       S              9B 0
007800951122     D DVASNATURA      S              1
007900951122     D DVACODICE       S              8
008000951122     D DVALINEAV       S              3
008100951122     D DVAFILIALE      S              3  0
008200951122     D DVASOGG         S              8
008300951122     D DVATPIND        S              2
008400951122     D DVACDIND        S              3
008500951122     D DVAOUTPUT       S           4000
008600951122     D DVAERRORE       S              1
008700951122     D*------------
008800951123     D* DS esterna per dati soggetto
008900951123    >D DVASOG        E DS
009000951123     D*-------------
009100951123     D* DS esterna per decodifica conto
009200951122     D ND002DS       E DS
009300951123     D*-------------
009400951123     D* Definizioni parametri standard chiamate moduli
009500951122     D Esito           S              1
009600951122     D GesMsg          S              1
009700951122     D LenOut          S              9B 0
009800951122     D Struttura       S             10
009900020415     D Data            S             10
010000951122     D*------------
010100000323D0495D* Definizioni parametri driver estremi fiscali
010200000323  "  D dvcRic          S              1
010300000323  "  D dvcSocieta      S              3
010400000323  "  D dvcUnita        S              8
010500000323  "  D dvcStrutt       S             10
010600000323  "  D dvcDtRif        S               D
010700000323  "  D dvcLenOut       S              9B 0
010800000323  "  D dvcSogg         S              8
010900000323  "  D dvcErrore       S              1
011000000323  "  D dvcOutput       S           4000
011100000323  "  D*-------------
011200000323  "  D* Ds per output variazioni estremi fiscali
011300000323  "  D ANCPI         E DS                  EXTNAME(ANCPI00F)
011400000323D0495D*-------------
011500000323D0495D*** RGPT            S              6    DIM(50)                           SCHIERA RAGGRUPP
011600000323D0495D RGPT            S             26    DIM(50)                              SCHIERA RAGGRUPP
011700971009     D $C              S              4    DIM(30)
011800951122     D*------------
011900971009     D* Parametro RABASE
012000971009     D ANP012DS      E DS                  INZ
012100971009     D XPAOUT          DS          2000
012200971009     D*------------
012300951122     D ND770           DS                  OCCURS(50)
012400951122     D  SOC                    1      3
012500951122     D  ACF                    4      6P 0
012600951122     D  QUA                    7      8
012700951122     D  KCC                    9     14
012800951122     D  KSC                   15     22
012900951122     D  RGT                   23     26
013000951122     D  CDF                   27     46
013100951122     D  COG                   47     94
013200951122     D  NOM                   95    142
013300951122     D  LNC                  143    176
013400951123     D  DNC                  177    186
013500951122     D  PNC                  187    188
013600951122     D  SES                  189    189
013700951122     D  CIT                  190    223
013800951122     D  VIA                  224    267
013900951122     D  NUM                  268    270
014000951122     D  PRV                  271    272
014100951122     D  C77                  273    292
014200951122     D  ITC                  293    302P 3
014300951122     D  IMN                  303    312P 3
014400951122     D  SNS                  313    322P 3
014500951122     D  IMS                  323    332P 3
014600951122     D  PRA                  333    335P 2
014700951122     D  RAC                  336    345P 3
014800951122     D  NET                  346    355P 3
014900951122     D  PFG                  356    356
015000951122     D  CSE                  357    391
015100951122     D  SER                  392    400
015200951122     D  CDE                  401    425
015300951130     D  PRG                  426    427
015400951122     D*------------
015500951122     D TBTTRI          DS
015600951123     D  TRITRIBUTO             1      4
015700951123     D  TRITRIPROG             5      6
015800951122     D*------------
015900951123     D NETTO           S                   like(N77NET)
016000951123     D IMT             S                   like(N77ITC)
016100951123     D*------------
016200951123     D DatMin          C                   CONST('0001-01-01')
016300951123     D*------------
016400951123     C*---------------------------------------------------------------*
016500951122     C*
016600951122     C                   EXSR      INZVAR                                       *
016700951122     C*
016800951122     C* Cancellazione eventuali record per l'anno selezionato
016900951123     C                   EXSR      DEL770                                       *
017000951122     C*
017100951123     C* Ciclo lettura percipienti
017200970924     C     socR70        SETLL     ANAPE000                                     *
017300951122     C*
017400000000     C                   DO        *HIVAL                                       *  -1-
017500970924     C     socR70        READE     ANAPE000                               90    *
017600000000     C   90              GOTO      FINE                                         *
017700951122     C*
017800000323D0495C* Reperisco dati conto
017900000323  "  C                   EXSR      READANPDC
018000000323  "  C     Esito         IfEq      *off
018100000323  "  C* Recupero dati soggetto
018200000323  "  C                   EXSR      CTRSOG
018300000323  "  C     DVAERRORE     IfEq      *ON
018400000323  "  C                   Clear                   DVASOG
018500000323  "  C                   EndIf
018600000323  "  C                   EndIf
018700000323D0495C*
018800951123     C                   move      apekcc        mrakcc
018900951123     C                   move      apeksc        mraksc
019000951123     C     KEYMRA        SETLL     NDMRA01L                                     *
019100951122     C*
019200000000     C                   DO        *HIVAL                                       *   -2-
019300951123     C     KEYMRA        READE     NDMRA01L                               90    *
019400951122     C*
019500951122     C  N90MRAACF        IFEQ      annoR70                                      *  -3-
019600000000     C     MRASTA        ANDEQ     '3'                                          *
019700951122     C*
019800951123     C     MRAIMN        IFEQ      0                                            *
019900000000     C     MRASNS        ANDEQ     0                                            *
020000000000     C     MRAIMS        ANDEQ     0                                            *
020100000000     C     MRARAC        ANDEQ     0                                            *
020200000000     C                   GOTO      LAB100                                       *
020300951122     C                   ENDIF                                                  *  -4-
020400951122     C*
020500000323D0495C* Reperisco eventuali dati variazioni fisali
020600000323  "  C                   Clear                   VarCPIva         20
020700000323  "  C                   Exsr      CallDVACPI
020800000323  "  C     DVCErrore     IfEq      *off
020900000323  "  C                   Movel     DVCOutput     AnCPI
021000000323  "  C     CPICdFisc     IFNE      *BLANK
021100000323  "  C                   MOVEL     CPICdFisc     VarCPIva
021200000323  "  C                   ELSE
021300000323  "  C                   MOVEL     CpIPartIva    VarCPIva
021400000323  "  C                   ENDIF
021500000323  "  C                   EndIf
021600000323D0495C*
021700951123     C                   MOVE      MRATRI        TRITRIBUTO
021800951123     C                   MOVE      MRAPTR        TRITRIPROG
021900971009     C     KEYTRI        CHAIN     ANTRI01L                           91
022000971009     C   91              CLEAR                   ANTRI000
022100951122     C*
022200020415     C  N91TRIQUA770     IFEQ      'AU'
022300951122     C                   EXSR      CARDS
022400951122     C                   ENDIF
022500951122     C*
022600000000     C     LAB100        TAG
022700951122     C                   ENDIF                                                  *  -3-
022800951122     C  N90              ENDDO                                                  *  -2-
022900951122     C*
023000000000     C     RGPT(1)       IFNE      *BLANK                                       *  -6-
023100000000     C                   EXSR      SCRIVI                                       *
023200000000     C                   EXSR      INZDS                                        *
023300951122     C                   ENDIF                                                  *  -6-
023400951122     C*
023500951122     C                   ENDDO                                                  *  -1-
023600951122     C*
023700000000     C     FINE          TAG                                                    *
023800000000     C                   SETON                                        LR        *
023900951122     C*
024000000000     C*----------------------------------------------------*
024100951122     C* SUBROUTINE INIZIALE                                *
024200000000     C*----------------------------------------------------*
024300951122     C     INZVAR        BEGSR                                                  *
024400951122     C*
024500000000     C     *ENTRY        PLIST                                                  *
024600000000     C                   PARM                    KPJBA                          *
024700951122     C*
024800951123     C                   movel     kpjbu         ndcr70ds
024900951123     C*
025000951123     C     KEY770        KLIST                                                  *
025100951127     C                   KFLD                    socR70
025200951123     C                   KFLD                    N77ACF
025300951122     C*
025400000000     C     KEYMRA        KLIST                                                  *
025500951127     C                   KFLD                    socR70
025600951122     C                   KFLD                    MRAKCC
025700951122     C                   KFLD                    MRAKSC
025800951122     C*
025900951122     C     KEYTRI        KLIST
026000951127     C                   KFLD                    socR70
026100951122     C                   KFLD                    TRITRIBUTO
026200951122     C                   KFLD                    TRITRIPROG
026300000323D0495C*
026400000323  "  C     PDVACPI       PLIST
026500000323  "  C                   PARM                    DVCRIC
026600000323  "  C                   PARM                    DVCSOCIETA
026700000323  "  C                   PARM                    DVCUNITA
026800000323  "  C                   PARM                    DVCSTRUTT
026900000323  "  C                   PARM                    DVCDTRIF
027000000323  "  C                   PARM                    DVCLENOUT
027100000323  "  C                   PARM                    DVCSOGG
027200000323  "  C                   PARM                    DVCERRORE
027300000323D0495C                   PARM                    DVCOUTPUT
027400971009     C*
027500971009     C* Reperimento parametro RABASE
027600971009     C                   CallB     'XPAR'
027700971009     C                   Parm                    SocR70
027800971009     C                   Parm      'RABASE  '    XPaPar            8
027900971009     C                   Parm                    XPaOut
028000971009     C                   Parm      *Off          XPaErr            1
028100971009     C                   Parm      ' '           XPaRic            1
028200971009     C     XPaErr        IfEq      *On
028300971009     C                   Goto      Fine
028400971009     C                   Else
028500971009     C                   MoveL     XPaOut        ANP012DS
028600971009     C                   EndIf
028700951122     C*
028800951122     C* Inizializzo DS multipla
028900951122     C                   EXSR      INZDS
029000951122     C*
029100000000     C                   ENDSR                                                  *
029200951122     C*
029300000000     C*----------------------------------------------------*
029400000000     C* SUBROUTINE CANCELLAZIONE RECORD X ANNO COMPETENZA  *
029500000000     C*----------------------------------------------------*
029600951123     C     DEL770        BEGSR                                                  *
029700951122     C*
029800951123     C                   z-add     annoR70       N77ACF
029900951123     C     KEY770        SETLL     ND77001L                                     *
030000000000     C                   DO        *HIVAL                                       *
030100951123     C     KEY770        READE     ND77001L                               90    *
030200951122     C   90              LEAVE                                                  *
030300951122     C                   DELETE    ND770000                                     *
030400951122     C                   ENDDO                                                  *
030500951122     C*
030600951122     C                   ENDSR                                                  *
030700000000     C*----------------------------------------------------*
030800000000     C*   INIZIALIZZAZIONE DS MULTIPLA                     *
030900000000     C*----------------------------------------------------*
031000000000     C     INZDS         BEGSR                                                  *
031100951122     C*
031200000000     C                   MOVEA     *BLANK        RGPT                           *
031300951122     C*                                                                         *
031400000000     C                   DO        50            X                 2 0          *
031500951122     C     X             OCCUR     ND770                                        *
031600951122     C                   MOVE      *BLANK        SOC
031700951122     C                   Z-ADD     0             ACF
031800951122     C                   MOVE      *BLANK        QUA
031900951122     C                   MOVEL     0             KCC
032000951122     C                   MOVEL     0             KSC
032100951122     C                   MOVE      *BLANK        RGT
032200951122     C                   MOVE      *BLANK        CDF
032300951122     C                   MOVE      *BLANK        COG
032400951122     C                   MOVE      *BLANK        NOM
032500951122     C                   MOVE      *BLANK        LNC
032600951123     C                   MOVE      *BLANK        DNC
032700951122     C                   MOVE      *BLANK        PNC
032800951122     C                   MOVE      *BLANK        SES
032900951122     C                   MOVE      *BLANK        CIT
033000951122     C                   MOVE      *BLANK        VIA
033100951122     C                   MOVE      *BLANK        NUM
033200951122     C                   MOVE      *BLANK        PRV
033300951122     C                   MOVE      *BLANK        C77
033400951122     C                   MOVE      *BLANK        CSE
033500951122     C                   MOVE      *BLANK        SER
033600951122     C                   MOVE      *BLANK        CDE
033700951130     C                   MOVE      *BLANK        PRG
033800951122     C                   Z-ADD     0             ITC
033900951122     C                   Z-ADD     0             IMN
034000951122     C                   Z-ADD     0             SNS
034100951122     C                   Z-ADD     0             IMS
034200951122     C                   Z-ADD     0             PRA
034300951122     C                   Z-ADD     0             RAC
034400951122     C                   Z-ADD     0             NET
034500951122     C                   MOVE      *BLANK        PFG
034600951122     C                   ENDDO
034700951122     C*
034800000000     C                   ENDSR                                                  *
034900000000     C*----------------------------------------------------*
035000000000     C* SUBROUTINE CARICAMENTO DS MULTIPLA                 *
035100000000     C*----------------------------------------------------*
035200000000     C     CARDS         BEGSR                                                  *
035300951122     C*
035400951122     C                   Z-ADD     1             X                              *
035500951122     C*
035600951122     C* Reperimento tributo su schiera tributi
035700971023     C*
035800971023     C                   If        RaggRGTR70 = *On
035900971023     C* codice raggr. tributo
036000971023     C                   Move      *Blank        RACTRI
036100150626R186 X***                MoveL     TRITriRagg    RACTRI
036101150626R186 C                   Eval      RacTri = TRITriRagg +
036102150626R186 C                                      %Subst(TRIDES770: 1:2)
036200971023     C                   Else
036300971023     C* tributo/progressivo
036400951123     C                   MOVEL     TBTTRI        RACTRI            6
036500971023     C                   EndIf
036600000323D0495C                   MOVEL     VarCPIva      KeyRGPT          26
036700000323D0495C                   MOVE      RacTri        KeyRGPT
036800000323D0495C***  RACTRI        LOOKUP    RGPT(X)                                50    *
036900000323D0495C     KeyRGPT       LOOKUP    RGPT(X)                                50    *
037000951123     C*
037100951123     C* Inserimento tributo su schiera tributi
037200951123     C     *in50         ifeq      *off                                         *
037300000000     C                   Z-ADD     1             J                 3 0          *
037400000000     C     *BLANK        LOOKUP    RGPT(J)                                51    *
037500000323D0495C***51              MOVE      RACTRI        RGPT(J)                        *
037600000323D0495C   51              MOVE      KeyRGPT       RGPT(J)                        *
037700951123     C   51              Z-ADD     J             X                              *
037800951123     C                   endif                                                  *
037900951122     C*
038000951122     C* Inserimento o aggiornamento importi DS multipla
038100951122     C     X             OCCUR     ND770                                60      *
038200000000     C   60              GOTO      ENDSU1                                       *
038300951123     C     MRAIMS        SUB       MRARAC        NETTO                          *
038400951122     C*
038500000323D0495C***  *in50         ifeq      *off
038600000323 "   C* Reperisco dati conto
038700000323 "   C***                EXSR      READANPDC
038800000323 "   C***  Esito         ifeq      *off
038900000323 "   C* Recupero dati soggetto
039000000323 "   C***                EXSR      CTRSOG
039100000323 "   C***  DVAERRORE     IFEQ      *ON
039200000323 "   C***                CLEAR                   DVASOG
039300000323 "   C***                ENDIF
039400000323 "   C***                endif
039500000323D0495C***                endif
039600951123     C*
039700951123     C* Scrittura o aggiornamento record DS multipla
039800951123     C*
039900951123     C     *in50         ifeq      *off
040000951123     C*
040100951123     C                   MOVE      MRASOCIETA    SOC
040200951123     C                   Z-ADD     MRAACF        ACF
040300951123     C                   MOVE      TRIQUA770     QUA
040400951123     C                   MOVEL     MRAKCC        KCC
040500951123     C                   MOVEL     MRAKSC        KSC
040600971023     C                   If        RaggRGTR70 = *On
040700971023     C* codice raggr. tributo
040800971023     C                   Move      TRITriRagg    RGT
040900971023     C                   Move      *Blank        PRG
041000971023     C                   Else
041100971023     C* tributo/progressivo
041200951123     C                   MOVEL     TRITRIBUTO    RGT
041300951130     C                   MOVEL     TRITRIPROG    PRG
041400971023     C                   EndIf
041500000323D0495C     VarCPIva      IFNE      *BLANK
041600000323D0495C                   Movel     VarCpIva      CDF
041700000323D0495C                   Else
041800951123     C     DVSCDFISC     IFNE      *BLANK
041900951123     C                   MOVEL     DVSCDFISC     CDF
042000951123     C                   ELSE
042100951123     C                   MOVEL     DVSPARTIVA    CDF
042200000323     C                   EndIf
042300000323D0495C                   EndIf
042400951123     C                   MOVE      '1'           XTBRIC
042500951127     C                   MOVE      socR70        XTBAZI
042600951123     C                   MOVE      *ZERO         XTBKEY
042700951123     C                   MOVE      'G16'         XTBCOD
042800951123     C                   MOVE      DVSNATGIUR    XTBKEY
042900951123     C                   CALLb     'XATB'
043000951123     C                   PARM                    XATBDS
043100971009     C     XTBERR        IFEQ      '0'
043200951123     C                   MOVEL     XTBUNI        TABG16
043300971009     C                   ENDIF
043400951123     C                   if        TPNG16 = '1'
043500951123     C                   MOVEL     APECOGNOME    COG
043600951123     C                   MOVEL     APENOME       NOM
043700951123     C                   MOVEL     APELOCALIT    LNC
043800951123     C                   MOVEL     DVSDTNASC     DNC
043900951123     C                   MOVEL     APEPROV       PNC
044000971022     C                   IF        DVSSEX = '1'
044100971022     C                   MOVEL     'M'           SES
044200971022     C                   ENDIF
044300971022     C                   IF        DVSSEX = '2'
044400971022     C                   MOVEL     'F'           SES
044500971022     C                   ENDIF
044600951123     C                   MOVEL     'F'           PFG
044700951123     C                   Else
044800951123     C                   MOVEL     DVSDES        COG
044900971009     C                   MOVEL     *BLANK        NOM
045000971009     C                   MOVEL     *BLANK        LNC
045100951123     C                   MOVEL     DatMin        DNC
045200971009     C                   MOVEL     *BLANK        PNC
045300971009     C                   MOVEL     *BLANK        SES
045400951123     C                   MOVEL     'G'           PFG
045500951123     C                   Endif
045600951123     C                   MOVEL     DVSLOCALIT    CIT
045700951123     C                   MOVEL     DVSINDRIZ     VIA
045800951123     C                   MOVEL     DVSPROV       PRV
045900951123     C                   MOVEL     TRIDES770     C77
046000971009     C*
046100951123     C                   MOVEL     APECDFISC     CDE
046200971009     C     CDE           IFNE      *BLANK
046300971009     C                   MOVEL     *BLANK        CDF
046400971009     C                   ENDIF
046500951123     C                   MOVEL     APECDESMIN    CSE
046600951123     C     CSE           IFNE      *BLANK
046700971009     C* Qui ci andrebbe la descrizione dello stato estero
046800971009     C* (ex tab.70 di Orion), ma per ora non ci metto niente !!!
046900951124     C                   MOVEL     *BLANK        SER
047000951123     C                   ENDIF
047100041001C1788C*
047200030625  "  C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
047300030625  "  C* se il tributo in elaborazione è quello da escludere; in caso
047400030625  "  C* affermativo lo elimino dalla/e caselle indicate
047500040930  "  x*** ex c1667       SetOn                                        3839
047600040930  "  x***      "         If        EscItcR70 = *On And MraTri = EscTriR70
047700040930  "  x***      "                   And MraPtr = EscPrgR70
047800040930  "  x***      "         SetOff                                       38
047900040930  "  x***      "         EndIf
048000040930  "  x***      "         If        EscImnR70 = *On And MraTri = EscTriR70
048100040930  "  x***      "                   And MraPtr = EscPrgR70
048200040930  "  x***      "         SetOff                                       39
048300040930  "  x***      "         EndIf
048400040930  "  C*
048500040930  "  C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
048600040930  "  C* se il tributo in elaborazione è quello da escludere; in caso
048700040930  "  C* affermativo lo elimino dalla/e caselle indicate
048800040930  "  C                   SetOn                                        3839
048900041001  "  C                   If        EscItcR70 = *On or
049000041001  "  C                             EscImnR70 = *On
049100040930  "  c* vedo se esiste nella tabella 022
049200040930  "  C                   MOVE      '1'           XTBRIC
049300040930  "  C                   MOVE      socR70        XTBAZI
049400040930  "  C                   MOVE      *ZERO         XTBKEY
049500040930  "  C                   MOVE      '022'         XTBCOD
049600040930  "  C                   MOVEl     MraTri        wrk6              6
049700040930  "  C                   MOVE      MraPtr        wrk6
049800040930  "  C                   MOVE      wrk6          XTBKEY
049900040930  "  C                   CALLb     'XATB'
050000040930  "  C                   PARM                    XATBDS
050100040930  "  C     XTBERR        IFEQ      '0'
050200040930  "  C                   If        EscItcR70 = *On
050300040930  "  C                   SetOff                                       38
050400040930  "  C                   EndIf
050500040930  "  c*
050600040930  "  C                   If        EscImnR70 = *On
050700040930  "  C                   SetOff                                       39
050800040930  "  C                   EndIf
050900041001  "  c*
051000041001  "  C                   EndIf
051100041001C1788c                   EndIf
051200990426     C*
051300990422     C     MRAIMS        ADD       MRASNS        ITC
051400030625C1667C   38              ADD       MRAIMN        ITC
051500030625  "  C  N39              Z-ADD     0             IMN
051600030625C1667C   39              Z-ADD     MRAIMN        IMN
051700951122     C                   Z-ADD     MRASNS        SNS
051800951122     C                   Z-ADD     MRAIMS        IMS
051900951122     C                   Z-ADD     MRAPRA        PRA
052000951122     C                   Z-ADD     MRARAC        RAC
052100951122     C                   Z-ADD     NETTO         NET
052200951123     C*
052300951123     C                   else
052400041001C1788x*
052500040930  "  x* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
052600040930  "  x* se il tributo in elaborazione è quello da escludere; in caso
052700040930  "  x* affermativo lo elimino dalla/e caselle indicate
052800040930  "  x*** ex c1667       SetOn                                        3839
052900040930  "  x***      "         If        EscItcR70 = *On And MraTri = EscTriR70
053000040930  "  x***      "                   And MraPtr = EscPrgR70
053100040930  "  x***      "         SetOff                                       38
053200040930  "  x***      "         EndIf
053300040930  "  x***      "         If        EscImnR70 = *On And MraTri = EscTriR70
053400040930  "  x***      "                   And MraPtr = EscPrgR70
053500040930  "  x***      "         SetOff                                       39
053600040930  "  x***      "         EndIf
053700040930  "  C*
053800040930  "  C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
053900040930  "  C* se il tributo in elaborazione è quello da escludere; in caso
054000040930  "  C* affermativo lo elimino dalla/e caselle indicate
054100040930  "  C                   SetOn                                        3839
054200041001  "  C                   If        EscItcR70 = *On or
054300041001  "  C                             EscImnR70 = *On
054400040930  "  c* vedo se esiste nella tabella 022
054500040930  "  C                   MOVE      '1'           XTBRIC
054600040930  "  C                   MOVE      socR70        XTBAZI
054700040930  "  C                   MOVE      *ZERO         XTBKEY
054800040930  "  C                   MOVE      '022'         XTBCOD
054900040930  "  C                   MOVEl     MraTri        wrk6              6
055000040930  "  C                   MOVE      MraPtr        wrk6
055100040930  "  C                   MOVE      wrk6          XTBKEY
055200040930  "  C                   CALLb     'XATB'
055300040930  "  C                   PARM                    XATBDS
055400040930  "  C     XTBERR        IFEQ      '0'
055500040930  "  C                   If        EscItcR70 = *On
055600040930  "  C                   SetOff                                       38
055700040930  "  C                   EndIf
055800040930  "  c*
055900040930  "  C                   If        EscImnR70 = *On
056000040930  "  C                   SetOff                                       39
056100040930  "  C                   EndIf
056200041001  "  c*
056300041001  "  C                   EndIf
056400041001C1788c                   EndIf
056500990924     C*
056600971009     C                   Z-ADD     0             IMT
056700990422     C     MRAIMS        ADD       MRASNS        IMT
056800030625C1667C   38              ADD       MRAIMN        IMT
056900971009     C                   ADD       IMT           ITC
057000030625C1667C   39              ADD       MRAIMN        IMN
057100030625     C                   ADD       MRASNS        SNS
057200951122     C                   ADD       MRAIMS        IMS
057300951122     C                   ADD       MRARAC        RAC
057400951122     C                   ADD       NETTO         NET
057500951123     C*
057600951123     C                   endif
057700951122     C*
057800000000     C     ENDSU1        ENDSR                                                  *
057900000000     C*----------------------------------------------------*
058000000000     C* SUBROUTINE SCRITTURA FILE DA DS MULTIPLA           *
058100000000     C*----------------------------------------------------*
058200000000     C     SCRIVI        BEGSR                                                  *
058300951123     C*
058400951123     C     1             DO        50            X                              *
058500951123     C*
058600000000     C     RGPT(X)       IFEQ      *BLANK                                       *
058700000000     C                   GOTO      ENDSUB                                       *
058800000000     C                   END                                                    *
058900951123     C*
059000951123     C     X             OCCUR     ND770                                60      *
059100000000     C   60              GOTO      ENDSUB                                       *
059200951123     C*
059300951123     C* Scrittura nuovo record
059400010404C1478C                   Clear                   ND770000                       *
059500951123     C                   MOVE      SOC           N77SOCIETA                     *
059600000000     C                   Z-ADD     ACF           N77ACF                         *
059700000000     C                   MOVE      QUA           N77QUA                         *
059800951123     C                   MOVE      KCC           N77KCC                         *
059900951123     C                   MOVE      KSC           N77KSC                         *
060000000000     C                   MOVE      RGT           N77RGT                         *
060100951130     C                   MOVE      PRG           N77PRG                         *
060200000000     C                   MOVE      CDF           N77CDF                         *
060300000000     C                   MOVE      COG           N77COG                         *
060400000000     C                   MOVE      NOM           N77NOM                         *
060500000000     C                   MOVE      LNC           N77LNC                         *
060600951123     C                   MOVE      DNC           N77DNC                         *
060700000000     C                   MOVE      PNC           N77PNC                         *
060800000000     C                   MOVE      SES           N77SES                         *
060900020807D1183C     CDE           IFEQ      *BLANK
061000000000     C                   MOVE      CIT           N77CIT                         *
061100000000     C                   MOVE      VIA           N77VIA                         *
061200020807D1183C                   ELSE
061300020807  "  C                   MOVE      *BLANKS       N77CIT                         *
061400020807  "  C                   MOVE      *BLANKS       N77VIA                         *
061500020807D1183C                   ENDIF
061600000000     C                   MOVEL     C77           N77C77                         *
061700000000     C                   Z-ADD     ITC           N77ITC                         *
061800000000     C                   Z-ADD     IMN           N77IMN                         *
061900000000     C                   Z-ADD     SNS           N77SNS                         *
062000000000     C                   Z-ADD     IMS           N77IMS                         *
062100000000     C                   Z-ADD     PRA           N77PRA                         *
062200000000     C                   Z-ADD     RAC           N77RAC                         *
062300981027     C                   Z-ADD     0             N77NET                         *
062400000000     C                   MOVE      PFG           N77PFG                         *
062500951123     C                   MOVEL     SER           N77SER
062600971009     C                   MOVEL     CSE           N77CSE
062700940316     C                   MOVEL     CDE           N77CDE
062800951123     C     CDE           IFNE      *BLANK
062900951123     C                   MOVE      '*'           NUM
063000951123     C                   MOVE      *BLANK        PRV
063100951123     C                   ENDIF
063200971009     C                   MOVE      NUM           N77NUM
063300971009     C                   MOVE      PRV           N77PRV
063400010404C1478C                   MOVEL     *blanks       N77Tit
063500010404C1478C                   Z-add     0             N77Anno
063600010404C1478C                   Z-add     0             N77Reg
063700010404C1478C                   Z-add     0             N77ARG
063800010404C1478C                   Z-add     0             N77ARS
063900010404C1478C                   Z-add     0             N77IMSAP
064000010404C1478C                   Z-add     0             N77RACAP
064100010404C1478C                   MOVEL     *blanks       N77PIn
064200010404C1478C                   Z-add     0             N77DtIn
064300010404C1478C                   Z-add     0             N77DtFi
064400010404C1478C                   MOVEL     *blanks       N77COM
064500010404C1478C                   MOVEL     *blanks       N77RCDF
064600010404C1478C                   MOVEL     *blanks       N77RCOG
064700010404C1478C                   MOVEL     *blanks       N77RNOM
064800010404C1478C                   MOVEL     *blanks       N77RSES
064900010404C1478C                   MOVEL     *blanks       N77RLNC
065000010404C1478C                   MOVEL     DatMin        N77RDNC
065100010404C1478C                   MOVEL     *blanks       N77RPNC
065200040622C1770C*
065300040622  "  C* ritenuta a titolo d'imposta
065400040622  "  C                   Z-ADD     0             N77RIM
065500040622  "  C* addizionale a titolo d'imposta
065600040622  "  C                   Z-ADD     0             N77ARI
065700040622C1770C*
065800020807D1183C* i campi N77RVIA e N77RCIT indicano dal 2002 i dati degli esteri
065900020807  "  C     CDE           IFNE      *BLANK
066000020807  "  C                   MOVEL     CIT           N77RCIT                        *
066100020807  "  C                   MOVEL     VIA           N77RVIA                        *
066200020807D1183C                   ELSE
066300010404C1478C                   MOVEL     *blanks       N77RCIT
066400010404C1478C                   MOVEL     *blanks       N77RVIA
066500020807D1183C                   ENDIF
066600010404C1478C                   MOVEL     *blanks       N77RPRV
066700010404C1478C                   MOVEL     *blanks       N77RCSE
066800120516R102 C                   MOVEL     *blanks       N77CAUAS
066900120516R102 C                   MOVEL     *blanks       N77ASCDF
067000120516R102 C                   Z-Add     0             N77ASIMS
067100120516R102 C                   Z-Add     0             N77ASRAC
067200120516R102 C                   Z-Add     0             N77ASRIM
067300120516R102 C                   Z-Add     0             N77ASRSO
067400120516R102 C                   Z-Add     0             N77ASARA
067500120516R102 C                   Z-Add     0             N77ASARI
067600120516R102 C                   Z-Add     0             N77ASARS
067700971009     C*
067800130611R122 C* Campo CODICE (770 / 2013)
067900130611R122 C                   Clear                   N77Codice
068000130611R122 C                   If        N77NUM        = *blanks
068100130611R122 C                   If        N77IMN        > 0       or
068200130611R122 C                             N77SNS        > 0
068300130611R122 C                   Eval      N77Codice     = '3'
068400130611R122 C                   EndIf                                                  *
068500130611R122 C                   EndIf                                                  *
068501160203     C*
068502160204R211 C                   Clear                   N77NRess
068503160203  "  C                   Clear                   N77DpCdf
068504160203  "  C                   Clear                   N77DpDen
068505160203  "  C                   Clear                   N77DpCenp
068506160203  "  C                   Clear                   N77DpCazi
068507160203  "  C                   Clear                   N77DpCate
068508160203  "  C                   Clear                   N77DpAcon
068509160203  "  C                   Clear                   N77DpImpa
068510160203  "  C                   Clear                   N77DpCodu
068511160203  "  C                   Clear                   N77DpCove
068512160203  "  C                   Clear                   N77OsCdf1
068513160203  "  C                   Clear                   N77OsCdf2
068514160203  "  C                   Clear                   N77OsCdf3
068515160203  "  C                   Clear                   N77F2Cdf
068516160203  "  C                   Clear                   N77F2Ime
068517160203  "  C                   Clear                   N77F2Rio
068518160203  "  C                   Clear                   N77F2Rino
068519160203  "  C                   Clear                   N77F2Cdf2
068520160203  "  C                   Clear                   N77F2Ime2
068521160203  "  C                   Clear                   N77F2Rio2
068522160203  "  C                   Clear                   N77F2Rino2
068523160203  "  C                   Clear                   N77F3Ine
068524160203  "  C                   Clear                   N77F3Rie
068525160203  "  C                   Clear                   N77F3Ain
068526160203  "  C                   Clear                   N77F3Ari
068527160203  "  C                   Clear                   N77F3Ine2
068528160203  "  C                   Clear                   N77F3Rie2
068529160203  "  C                   Clear                   N77F3Ain2
068530160203  "  C                   Clear                   N77F3Ari2
068531160203  "  C                   Eval      N77CitAP = N77Cit
068532160203  "  C                   Eval      N77PrvAP = N77Prv
068533160203R211 C                   Eval      N77Comap = N77Com
068600010404     C*
068700951123     C                   WRITE     ND770000                                     *
068800951123     C*
068900951123     C                   ENDDO                                                  *
069000951123     C*
069100000000     C     ENDSUB        ENDSR                                                  *
069200951123     C************************************************************
069300951123     C* REPERIMENTO DATI da ANPDC
069400951123     C************************************************************
069500951123     C     READANPDC     BEGSR
069600951123     C*
069700951123     C                   CLEAR                   ND002DS
069800951123     C                   EVAL      LenOut = %Size(ND002ds)
069900951123     C                   CallB     'NDMVC002'
070000951127     C                   PARM                    socR70
070100000323D0495C***                PARM                    MRAKCC
070200000323  "  C***                PARM                    MRAKSC
070300000323  "  C                   PARM                    APEKcc
070400000323D0495C                   PARM                    APEKsc
070500020415     C                   PARM      DatMin        Data
070600951123     C                   PARM      *OFF          GesMsg
070700951123     C                   PARM      *ON           Esito
070800951123     C                   PARM      'ND002DS'     Struttura
070900951123     C                   PARM                    ND002DS
071000951123     C                   PARM                    LenOut
071100951123     C*
071200951123     C                   ENDSR
071300951123     C************************************************************
071400951123     C* CONTROLLO SOGGETTI
071500971027     C************************************************************
071600951123     C     CTRSOG        BEGSR
071700951127     C*
071800951123     C                   MOVE      '1'           DVARIC
071900951127     C                   MOVE      socR70        DVASOCIETA
072000951123     C                   move      *blank        DVASTRUTT
072100951123     C                   MOVEL     'DVASOG'      DVASTRUTT
072200951123     C                   EVAL      DVALenOut = %Size(DVASOG)
072300951123     C                   MOVE      sogg002       DVASOGG
072400990915     C                   Clear                   DvaTpInd
072500990915     C                   Clear                   DvaCdInd
072600000328     C                   If        IndirR70      = '1'
072700990915     C                   If        APETpInd      <> *blanks and
072800000328     C                             APECdInd      <> *blanks
072900990915     C                   MOVE      APETpInd      DvaTpInd
073000990915     C                   MOVE      APECdInd      DvaCdInd
073100990915     C                   EndIf
073200000328     C                   EndIf
073300951123     C                   CALLB     'NDDVASOG'
073400951123     C                   PARM                    DVARIC
073500951123     C                   PARM                    DVASOCIETA
073600951123     C                   PARM                    DVAUNITA
073700951123     C                   PARM                    DVASTRUTT
073800951123     C                   PARM                    DVADTRIF
073900951123     C                   PARM                    DVALENOUT
074000951123     C                   PARM                    DVASNATURA
074100951123     C                   PARM                    *omit
074200951123     C                   PARM                    DVALINEAV
074300951123     C                   PARM                    DVAFILIALE
074400951123     C                   PARM                    DVASOGG
074500951123     C                   PARM                    DVATPIND
074600951123     C                   PARM                    DVACDIND
074700951123     C                   PARM                    DVAERRORE
074800951123     C                   PARM                    DVAOUTPUT
074900971009     C     DVAERRORE     IFEQ      *OFF
075000951123     C                   EVAL      %SUBST(DVASOG:1:DVALENOUT)
075100951123     C                              = %SUBST(DVAOUTPUT:1:DVALENOUT)
075200951123     C                   ENDIF
075300951123     C*
075400951123     C                   ENDSR
075500000323D0495C************************************************************
075600000323 "   C* DATI ANAGRAFICI VARIAZIONE ESTREMI FISCALI
075700000323 "   C************************************************************
075800000323 "   C     CALLDVACPI    BEGSR
075900000323 "   C*
076000000323 "   C                   move      '2'           DVCRIC
076100000323 "   C                   move      MRASocieta    DVCSOCIETA
076200000323 "   C                   move      *blank        DVCUNITA
076300000323 "   C                   movel     'ANCPI'       DVCSTRUTT
076400000323 "   C                   move      MRADDC        DVCDTRIF
076500000323 "   C                   eval      DVCLENOUT = %Size(ANCPI)
076600000323 "   C                   move      sogg002       DVCSOGG
076700000323 "   C*
076800000323 "   C                   CALLB     'NDDVACPI'    PDVACPI
076900000323 "   C*
077000000323 "   C                   ENDSR
077100000323 "   C/EJECT
077200000323D0495 ************************************************************
