000100160616     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200160616     H*PARMS BNDDIR(PJXBND PJCBND)
000300160616     H*PARMS CVTOPT(*DATETIME)
000400160616     H DECEDIT('0,') DATEDIT(*DMY.)
000500160616     F*---------------------------------------------------------------*
000600160616     F*         ESTRAZIONE RIGHE PERCIPIENTE CON TOTALI PER:          *
000700160616     F*         - TRIBUTO/PROGRESSIVO TRIBUTO                         *
000800160616     F*          OPPURE                                               *
000900160616     F*         - CODICE DI RAGGRUPPAMENTO TRIBUTO                    *
001000160616     F*---------------------------------------------------------------*
001100160616     F*    INDICATORI                                                 *
001200160616     F*                                                               *
001300160616     F* 90 FINE FILE                                                  *
001400160616     F* 91 CHAIN ANTRI00F                                             *
001500160616     F* 50 LOOKUP SU TABELLA RAGGRUPPAMENTI TRIBUTI                   *
001600160616     F*    50 = OFF  INSERIMENTO NELLA TABELLA RAGGR. TRIBUTI         *
001700160616     F*    50 = ON   TOTALIZZAZIONE IMPORTI TABELLA RAGGR. TRIBUTI    *
001800160616     F* 51 LOOKUP CON *BLANK PER REPERIRE PRIMO LIBERO                *
001900160616     F* 60 INDICATORE DI OCCUR SU DS MULTIPLA RAGGRUPPAMENTI          *
002000160616     F*---------------------------------------------------------------*
002100160616     F*  --- FLUSSO PRINCIPALE ---                                    *
002200160616     F*                                                               *
002300160616     F*  - CANCELLAZIONE RECORD ESISTENTI PER L'ANNO DI COMPETENZA    *
002400160616     F*    SELEZIONATO NEL FILE SDGN7701L                             *
002500160616     F*                                                               *
002600160616     F*  - LETTURA ANAGRAFICO PERCIPIENTI                             *
002700160616     F*                                                               *
002800160616     F*  - LETTURA RITENUTE DEL PERCIPIENTE                           *
002900160616     F*                                                               *
003000160616     F*  - SELEZIONE DELLE RITENUTE CHE HANNO:                        *
003100160616     F*               1 - ANNO DI COMPETENZA SELEZIONATO A VIDEO      *
003200160616     F*               2 - STATO=3 (QUIETANZATI)                       *
003300160616     F*               3 - ALMENO UN IMPORTO DIVERSO DA 0              *
003400160616     F*               4 - QUADRO = SY                                 *
003500160616     F*                                                               *
003600160616     F*  - INSERIMENTO DEI MOVIMENTI ELABORATI IN UNA DS MULTIPLA     *
003700160616     F*    CHE LI RACCOGLIE PER TRIBUTO/PROGR. O COD. RAGGRUPPAMENTO  *
003800160616     F*                                                               *
003900160616     F*  - TOTALIZZAZIONE DEGLI IMPORTI NEL CASO IL MOVIMENTO APPAR-  *
004000160616     F*    TENGA AD UN TRIBUTO/PROGR. O COD. RAGGR. GIA' INSERITO     *
004100160616     F*                                                               *
004200160616     F*  - LETTURA NUOVO RECORD RITENUTE                              *
004300160616     F*                                                               *
004400160616     F*  - PER FINE MOVIMENTI DEL PERCIPIENTE MEMORIZZO DS MULTIPLA   *
004500160616     F*    NEL FILE SDGN7701l                                         *
004600160616     F*                                                               *
004700160616     F*  - LETTURA NUOVO RECORD PERCIPIENTI                           *
004800160616     F*                                                               *
004900160616     F*  - FINE FILE PERCIPIENTI VAI A FINE                           *
005000160616     F*---------------------------------------------------------------*
005100160616     FANAPE01L  IF   E           K DISK
005200160616     F*------------
005300160616     FNDMRA01L  IF   E           K DISK
005400160616     F*------------
005500160616     FANTRI01L  IF   E           K DISK
005600160616     F*------------
005700160616     FSDGN7701L UF A E           K DISK
005800160616     F*---------------------------------------------------------------*
005900160616     D*------------
006000160616     D KPJBA         E DS
006100160616     D*-------------
006200160617     D ndcr70ds      E DS
006300160616     D*-------------
006400160616     D* Richiamo a SDGTABDR
006500160616     D SDGTABDS      E DS
006600160616     D*------------
006700160616     D* Tabella Natura Giuridica
006800160616     D TABG16        E DS                  EXTNAME(ANGG16DS) INZ
006900160616     D*------------
007000160616     D* Tabella esclusione tributi 2%
007100160616     D TAB022        E DS                  EXTNAME(ANG022DS) INZ
007200160616     D*------------
007300160616     D* Tabella Stati
007400160616     D TABG12        E DS                  EXTNAME(ANGG12DS) INZ
007500160616     D*------------
007600160616     D* Definizioni parametri driver soggetti
007700160616     D DVARIC          S              1
007800160616     D DVASOCIETA      S              3
007900160616     D DVAUNITA        S              8
008000160616     D DVADTRIF        S               D
008100160616     D DVASTRUTT       S             10
008200160616     D DVALENOUT       S              9B 0
008300160616     D DVASNATURA      S              1
008400160616     D DVACODICE       S              8
008500160616     D DVALINEAV       S              3
008600160616     D DVAFILIALE      S              3  0
008700160616     D DVASOGG         S              8
008800160616     D DVATPIND        S              2
008900160616     D DVACDIND        S              3
009000160616     D DVAOUTPUT       S           4000
009100160616     D DVAERRORE       S              1
009200160616     D*------------
009300160616     D* DS esterna per dati soggetto
009400160616    >D DVASOG        E DS
009500160616     D*-------------
009600160616     D* DS esterna per decodifica conto
009700160616     D ND002DS       E DS
009800160616     D*-------------
009900160616     D* Definizioni parametri standard chiamate moduli
010000160616     D Esito           S              1
010100160616     D GesMsg          S              1
010200160616     D LenOut          S              9B 0
010300160616     D Struttura       S             10
010400160616     D Data            S             10
010500160616     D*------------
010600160616     D* Definizioni parametri driver estremi fiscali
010700160616     D dvcRic          S              1
010800160616     D dvcSocieta      S              3
010900160616     D dvcUnita        S              8
011000160616     D dvcStrutt       S             10
011100160616     D dvcDtRif        S               D
011200160616     D dvcLenOut       S              9B 0
011300160616     D dvcSogg         S              8
011400160616     D dvcErrore       S              1
011500160616     D dvcOutput       S           4000
011600160616     D*-------------
011700160616     D* Ds per output variazioni estremi fiscali
011800160616     D ANCPI         E DS                  EXTNAME(ANCPI00F)
011900160616     D*-------------
012000160616     D RGPT            S             26    DIM(50)                              SCHIERA RAGGRUPP
012100160616     D $C              S              4    DIM(30)
012200160616     D*------------
012300160616     D* Parametro RABASE
012400160616     D ANP012DS      E DS                  INZ
012500160616     D XPAOUT          DS          2000
012600160616     D*------------
012700160616     D ND770           DS                  OCCURS(50)
012800160616     D  SOC                    1      3
012900160616     D  ACF                    4      6P 0
013000160616     D  QUA                    7      8
013100160616     D  KCC                    9     14
013200160616     D  KSC                   15     22
013300160616     D  RGT                   23     26
013400160616     D  CDF                   27     46
013500160616     D  COG                   47     94
013600160616     D  NOM                   95    142
013700160616     D  LNC                  143    176
013800160616     D  DNC                  177    186
013900160616     D  PNC                  187    188
014000160616     D  SES                  189    189
014100160616     D  CIT                  190    223
014200160616     D  VIA                  224    267
014300160616     D  NUM                  268    270
014400160616     D  PRV                  271    272
014500160616     D  C77                  273    292
014600160616     D  ITC                  293    302P 3
014700160616     D  IMN                  303    312P 3
014800160616     D  SNS                  313    322P 3
014900160616     D  IMS                  323    332P 3
015000160616     D  PRA                  333    335P 2
015100160616     D  RAC                  336    345P 3
015200160616     D  NET                  346    355P 3
015300160616     D  PFG                  356    356
015400160616     D  CSE                  357    391
015500160616     D  CDE                  392    416
015600160616     D  PRG                  417    418
015700160616     D*------------
015800160616     D TBTTRI          DS
015900160616     D  TRITRIBUTO             1      4
016000160616     D  TRITRIPROG             5      6
016100160616     D*------------
016200160616     D NETTO           S                   like(N77NET)
016300160616     D IMT             S                   like(N77ITC)
016400160616     D $SOGGEST        S              1
016500160616     D*------------
016600160616     D DatMin          C                   CONST('0001-01-01')
016700160616     D*------------
016800160616     C*---------------------------------------------------------------*
016900160616     C*
017000160616     C                   EXSR      INZVAR                                       *
017100160616     C*
017200160616     C* Cancellazione eventuali record per l'anno selezionato
017300160616     C                   EXSR      DEL770                                       *
017400160616     C*
017500160616     C* Ciclo lettura percipienti
017600160617     C     socR70        SETLL     ANAPE000                                     *
017700160616     C*
017800160616     C                   DO        *HIVAL                                       *  -1-
017900160617     C     socR70        READE     ANAPE000                               90    *
018000160616     C   90              LEAVE                                                  *
018100160616     C*
018200160616     C                   Eval      $SoggEst   = *off
018300160616     C* Reperisco dati conto
018400160616     C                   EXSR      READANPDC
018500160616     C     Esito         IfEq      *Off
018600160616     C* Recupero dati soggetto
018700160616     C                   EXSR      CTRSOG
018800160616     C     DVAERRORE     IfEq      *ON
018900160616     C                   Clear                   DVASOG
019000160616     C                   Else
019100160616     C                   Exsr      CTRStato
019200160616     C                   Endif
019300160616     C                   EndIf
019400160616     C* Sono percipienti esteri se il soggetto è estero oppure se
019500160616     C* nell'anagrafica percipiente è valorizzato l'identificativo
019600160616     C* fiscale estero.
019700160616     C                   If        $Soggest = *on  or
019800160616     C                             ApeCdFisc <> *blanks
019900160616     C*
020000160616     C                   move      apekcc        mrakcc
020100160616     C                   move      apeksc        mraksc
020200160616     C     KEYMRA        SETLL     NDMRA01L                                     *
020300160616     C*
020400160616     C                   DO        *HIVAL                                       *   -2-
020500160616     C     KEYMRA        READE     NDMRA01L                               90    *
020600160616     C*
020700160616     C                   If        *IN90
020800160616     C                   Leave
020900160616     C                   Endif
021000160616     C*
021100160617     C     MRAACF        IFEQ      annoR70                                      *  -3-
021200160616     C     MRASTA        ANDEQ     '3'                                          *
021300160616     C*
021400160616     C     MRAIMN        IFEQ      0                                            *
021500160616     C     MRASNS        ANDEQ     0                                            *
021600160616     C     MRAIMS        ANDEQ     0                                            *
021700160616     C     MRARAC        ANDEQ     0                                            *
021800160616     C                   ITER                                                   *
021900160616     C                   ENDIF                                                  *  -4-
022000160616     C*
022100160616     C* Reperisco eventuali dati variazioni fisali
022200160616     C                   Clear                   VarCPIva         20
022300160616     C                   Exsr      CallDVACPI
022400160616     C     DVCErrore     IfEq      *off
022500160616     C                   Movel     DVCOutput     AnCPI
022600160616     C     CPICdFisc     IFNE      *BLANK
022700160616     C                   MOVEL     CPICdFisc     VarCPIva
022800160616     C                   ELSE
022900160616     C                   MOVEL     CpIPartIva    VarCPIva
023000160616     C                   ENDIF
023100160616     C                   EndIf
023200160616     C*
023300160616     C                   MOVE      MRATRI        TRITRIBUTO
023400160616     C                   MOVE      MRAPTR        TRITRIPROG
023500160616     C     KEYTRI        CHAIN     ANTRI01L                           91
023600160616     C   91              CLEAR                   ANTRI000
023700160616     C*
023800160616     C  N91TRIQUA770     IFEQ      'AU'
023900160616     C                   EXSR      CARDS
024000160616     C                   ENDIF
024100160616     C*
024200160616     C                   ENDIF                                                  *  -3-
024300160616     C                   ENDDO                                                  *  -2-
024400160616     C*
024500160616     C     RGPT(1)       IFNE      *BLANK                                       *  -6-
024600160616     C                   EXSR      SCRIVI                                       *
024700160616     C                   EXSR      INZDS                                        *
024800160616     C                   ENDIF                                                  *  -6-
024900160616     C*
025000160616     C                   ENDIF
025100160616     C*
025200160616     C                   ENDDO                                                  *  -1-
025300160616     C*
025400160616     C*
025500160616     C                   EXSR      ENDPGM
025600160616     C*
025700160616     C*----------------------------------------------------*
025800160616     C* FINE PROGRAMMA                                     *
025900160616     C*----------------------------------------------------*
026000160616     C     ENDPGM        BEGSR                                                  *
026100160616     C*
026200160617     C                   MoveL     Ndcr70ds      KPJBU
026300160616     C                   SETON                                        LR
026400160616     C*
026500160616     C                   RETURN
026600160616     C*
026700160616     C                   ENDSR                                                  *
026800160616     C*
026900160616     C*----------------------------------------------------*
027000160616     C* SUBROUTINE INIZIALE                                *
027100160616     C*----------------------------------------------------*
027200160616     C     INZVAR        BEGSR                                                  *
027300160616     C*
027400160616     C     *ENTRY        PLIST                                                  *
027500160616     C                   PARM                    KPJBA                          *
027600160616     C*
027700160617     C                   movel     kpjbu         ndcr70ds
027800160616     C*
027900160616     C     KEYN77        KLIST                                                  *
028000160617     C                   KFLD                    SocR70
028100160616     C                   KFLD                    N77ACF
028200160616     C*
028300160616     C     KEYMRA        KLIST                                                  *
028400160617     C                   KFLD                    SocR70
028500160616     C                   KFLD                    MRAKCC
028600160616     C                   KFLD                    MRAKSC
028700160616     C*
028800160616     C     KEYTRI        KLIST
028900160617     C                   KFLD                    SocR70
029000160616     C                   KFLD                    TRITRIBUTO
029100160616     C                   KFLD                    TRITRIPROG
029200160616     C*
029300160616     C     PDVACPI       PLIST
029400160616     C                   PARM                    DVCRIC
029500160616     C                   PARM                    DVCSOCIETA
029600160616     C                   PARM                    DVCUNITA
029700160616     C                   PARM                    DVCSTRUTT
029800160616     C                   PARM                    DVCDTRIF
029900160616     C                   PARM                    DVCLENOUT
030000160616     C                   PARM                    DVCSOGG
030100160616     C                   PARM                    DVCERRORE
030200160616     C                   PARM                    DVCOUTPUT
030300160616     C*
030400160616     C* Reperimento parametro RABASE
030500160616     C                   CallB     'XPAR'
030600160617     C                   Parm                    SocR70
030700160616     C                   Parm      'RABASE  '    XPaPar            8
030800160616     C                   Parm                    XPaOut
030900160616     C                   Parm      *Off          XPaErr            1
031000160616     C                   Parm      ' '           XPaRic            1
031100160616     C     XPaErr        IfEq      *On
031200160616     C                   EXSR      EndPgM
031300160616     C                   Else
031400160616     C                   MoveL     XPaOut        ANP012DS
031500160616     C                   EndIf
031600160616     C*
031700160616     C* Inizializzo DS multipla
031800160616     C                   EXSR      INZDS
031900160616     C*
032000160616     C                   ENDSR                                                  *
032100160616     C*
032200160616     C*----------------------------------------------------*
032300160616     C* SUBROUTINE CANCELLAZIONE RECORD X ANNO COMPETENZA  *
032400160616     C*----------------------------------------------------*
032500160616     C     DEL770        BEGSR                                                  *
032600160616     C*
032700160617     C                   z-add     annoR70       N77ACF
032800160616     C     KEYN77        SETLL     SDGN7701L                                    *
032900160616     C                   DO        *HIVAL                                       *
033000160616     C     KEYN77        READE     SDGN7701L                              90    *
033100160616     C   90              LEAVE                                                  *
033200160616     C                   DELETE    SDGN77000                                    *
033300160616     C                   ENDDO                                                  *
033400160616     C*
033500160616     C                   ENDSR                                                  *
033600160616     C*----------------------------------------------------*
033700160616     C*   INIZIALIZZAZIONE DS MULTIPLA                     *
033800160616     C*----------------------------------------------------*
033900160616     C     INZDS         BEGSR                                                  *
034000160616     C*
034100160616     C                   MOVEA     *BLANK        RGPT                           *
034200160616     C*                                                                         *
034300160616     C                   DO        50            X                 2 0          *
034400160616     C     X             OCCUR     ND770                                        *
034500160616     C                   MOVE      *BLANK        SOC
034600160616     C                   Z-ADD     0             ACF
034700160616     C                   MOVE      *BLANK        QUA
034800160616     C                   MOVEL     0             KCC
034900160616     C                   MOVEL     0             KSC
035000160616     C                   MOVE      *BLANK        RGT
035100160616     C                   MOVE      *BLANK        CDF
035200160616     C                   MOVE      *BLANK        COG
035300160616     C                   MOVE      *BLANK        NOM
035400160616     C                   MOVE      *BLANK        LNC
035500160616     C                   MOVE      *BLANK        DNC
035600160616     C                   MOVE      *BLANK        PNC
035700160616     C                   MOVE      *BLANK        SES
035800160616     C                   MOVE      *BLANK        CIT
035900160616     C                   MOVE      *BLANK        VIA
036000160616     C                   MOVE      *BLANK        NUM
036100160616     C                   MOVE      *BLANK        PRV
036200160616     C                   MOVE      *BLANK        C77
036300160616     C                   MOVE      *BLANK        CSE
036400160616     C                   MOVE      *BLANK        CDE
036500160616     C                   MOVE      *BLANK        PRG
036600160616     C                   Z-ADD     0             ITC
036700160616     C                   Z-ADD     0             IMN
036800160616     C                   Z-ADD     0             SNS
036900160616     C                   Z-ADD     0             IMS
037000160616     C                   Z-ADD     0             PRA
037100160616     C                   Z-ADD     0             RAC
037200160616     C                   Z-ADD     0             NET
037300160616     C                   MOVE      *BLANK        PFG
037400160616     C                   ENDDO
037500160616     C*
037600160616     C                   ENDSR                                                  *
037700160616     C*----------------------------------------------------*
037800160616     C* SUBROUTINE CARICAMENTO DS MULTIPLA                 *
037900160616     C*----------------------------------------------------*
038000160616     C     CARDS         BEGSR                                                  *
038100160616     C*
038200160616     C                   Z-ADD     1             X                              *
038300160616     C*
038400160616     C* Reperimento tributo su schiera tributi
038500160616     C*
038600160617     C                   If        RaggRGTR70 = *On
038700160616     C* codice raggr. tributo
038800160616     C                   Move      *Blank        RACTRI
038900160616     C                   Eval      RacTri = TRITriRagg +
039000160616     C                                      %Subst(TRIDES770: 1:2)
039100160616     C                   Else
039200160616     C* tributo/progressivo
039300160616     C                   MOVEL     TBTTRI        RACTRI            6
039400160616     C                   EndIf
039500160616     C                   MOVEL     VarCPIva      KeyRGPT          26
039600160616     C                   MOVE      RacTri        KeyRGPT
039700160616     C     KeyRGPT       LOOKUP    RGPT(X)                                50    *
039800160616     C*
039900160616     C* Inserimento tributo su schiera tributi
040000160616     C     *in50         ifeq      *off                                         *
040100160616     C                   Z-ADD     1             J                 3 0          *
040200160616     C     *BLANK        LOOKUP    RGPT(J)                                51    *
040300160616     C   51              MOVE      KeyRGPT       RGPT(J)                        *
040400160616     C   51              Z-ADD     J             X                              *
040500160616     C                   endif                                                  *
040600160616     C*
040700160616     C* Inserimento o aggiornamento importi DS multipla
040800160616     C     X             OCCUR     ND770                                60      *
040900160616     C   60              GOTO      ENDSU1                                       *
041000160616     C     MRAIMS        SUB       MRARAC        NETTO                          *
041100160616     C*
041200160616     C*
041300160616     C* Scrittura o aggiornamento record DS multipla
041400160616     C*
041500160616     C     *in50         ifeq      *off
041600160616     C*
041700160616     C                   MOVE      MRASOCIETA    SOC
041800160616     C                   Z-ADD     MRAACF        ACF
041900160617     C                   MOVE      'SY'          QUA
042000160616     C                   MOVEL     MRAKCC        KCC
042100160616     C                   MOVEL     MRAKSC        KSC
042200160617     C                   If        RaggRGTR70 = *On
042300160616     C* codice raggr. tributo
042400160616     C                   Move      TRITriRagg    RGT
042500160616     C                   Move      *Blank        PRG
042600160616     C                   Else
042700160616     C* tributo/progressivo
042800160616     C                   MOVEL     TRITRIBUTO    RGT
042900160616     C                   MOVEL     TRITRIPROG    PRG
043000160616     C                   EndIf
043100160616     C     VarCPIva      IFNE      *BLANK
043200160616     C                   Movel     VarCpIva      CDF
043300160616     C                   Else
043400160616     C     DVSCDFISC     IFNE      *BLANK
043500160616     C                   MOVEL     DVSCDFISC     CDF
043600160616     C                   ELSE
043700160616     C                   MOVEL     DVSPARTIVA    CDF
043800160616     C                   EndIf
043900160616     C                   EndIf
044000160616     C                   MOVE      '1'           XTBRIC
044100160617     C                   MOVE      SocR70        XTBAZI
044200160616     C                   MOVE      *ZERO         XTBKEY
044300160616     C                   MOVE      'G16'         XTBCOD
044400160616     C                   MOVE      DVSNATGIUR    XTBKEY
044500160616     C                   CALL      'SDGTABDR'
044600160616     C                   PARM                    SDGTABDS
044700160616     C     XTBERR        IFEQ      '0'
044800160616     C                   MOVEL     XTBUNI        TABG16
044900160616     C                   ENDIF
045000160616     C                   if        TPNG16 = '1'
045100160616     C                   MOVEL     APECOGNOME    COG
045200160616     C                   MOVEL     APENOME       NOM
045300160616     C                   MOVEL     APELOCALIT    LNC
045400160616     C                   MOVEL     DVSDTNASC     DNC
045500160616     C                   MOVEL     APEPROV       PNC
045600160616     C                   IF        DVSSEX = '1'
045700160616     C                   MOVEL     'M'           SES
045800160616     C                   ENDIF
045900160616     C                   IF        DVSSEX = '2'
046000160616     C                   MOVEL     'F'           SES
046100160616     C                   ENDIF
046200160616     C                   MOVEL     'F'           PFG
046300160616     C                   Else
046400160616     C                   MOVEL     DVSDES        COG
046500160616     C                   MOVEL     *BLANK        NOM
046600160616     C                   MOVEL     *BLANK        LNC
046700160616     C                   MOVEL     DatMin        DNC
046800160616     C                   MOVEL     *BLANK        PNC
046900160616     C                   MOVEL     *BLANK        SES
047000160616     C                   MOVEL     'G'           PFG
047100160616     C                   Endif
047200160616     C                   MOVEL     DVSLOCALIT    CIT
047300160616     C                   MOVEL     DVSINDRIZ     VIA
047400160616     C                   MOVEL     DVSPROV       PRV
047500160616     C                   MOVEL     TRIDES770     C77
047600160616     C*
047700160616     C                   MOVEL     APECDFISC     CDE
047800160616     C     CDE           IFNE      *BLANK
047900160616     C                   MOVEL     *BLANK        CDF
048000160616     C                   ENDIF
048100160616     C                   MOVEL     APECDESMIN    CSE
048200160616     C*
048300160616     C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
048400160616     C* se il tributo in elaborazione è quello da escludere; in caso
048500160616     C* affermativo lo elimino dalla/e caselle indicate
048600160616     C                   SetOn                                        3839
048700160617     C                   If        EscItcR70 = *On or
048800160617     C                             EscImnR70 = *On
048900160616     c* vedo se esiste nella tabella 022
049000160616     C                   MOVE      '1'           XTBRIC
049100160617     C                   MOVE      SocR70        XTBAZI
049200160616     C                   MOVE      *ZERO         XTBKEY
049300160616     C                   MOVE      '022'         XTBCOD
049400160616     C                   MOVEl     MraTri        wrk6              6
049500160616     C                   MOVE      MraPtr        wrk6
049600160616     C                   MOVE      wrk6          XTBKEY
049700160616     C                   CALL      'SDGTABDR'
049800160616     C                   PARM                    SDGTABDS
049900160616     C     XTBERR        IFEQ      '0'
050000160617     C                   If        EscItcR70 = *On
050100160616     C                   SetOff                                       38
050200160616     C                   EndIf
050300160616     c*
050400160617     C                   If        EscImnR70 = *On
050500160616     C                   SetOff                                       39
050600160616     C                   EndIf
050700160616     c*
050800160616     C                   EndIf
050900160616     c                   EndIf
051000160616     C*
051100160616     C     MRAIMS        ADD       MRASNS        ITC
051200160616     C   38              ADD       MRAIMN        ITC
051300160616     C  N39              Z-ADD     0             IMN
051400160616     C   39              Z-ADD     MRAIMN        IMN
051500160616     C                   Z-ADD     MRASNS        SNS
051600160616     C                   Z-ADD     MRAIMS        IMS
051700160616     C                   Z-ADD     MRAPRA        PRA
051800160616     C                   Z-ADD     MRARAC        RAC
051900160616     C                   Z-ADD     NETTO         NET
052000160616     C*
052100160616     C                   else
052200160616     C*
052300160616     C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
052400160616     C* se il tributo in elaborazione è quello da escludere; in caso
052500160616     C* affermativo lo elimino dalla/e caselle indicate
052600160616     C                   SetOn                                        3839
052700160617     C                   If        EscItcR70 = *On or
052800160617     C                             EscImnR70 = *On
052900160616     c* vedo se esiste nella tabella 022
053000160616     C                   MOVE      '1'           XTBRIC
053100160617     C                   MOVE      SocR70        XTBAZI
053200160616     C                   MOVE      *ZERO         XTBKEY
053300160616     C                   MOVE      '022'         XTBCOD
053400160616     C                   MOVEl     MraTri        wrk6              6
053500160616     C                   MOVE      MraPtr        wrk6
053600160616     C                   MOVE      wrk6          XTBKEY
053700160616     C                   CALL      'SDGTABDR'
053800160616     C                   PARM                    SDGTABDS
053900160616     C     XTBERR        IFEQ      '0'
054000160617     C                   If        EscItcR70 = *On
054100160616     C                   SetOff                                       38
054200160616     C                   EndIf
054300160616     c*
054400160617     C                   If        EscImnR70 = *On
054500160616     C                   SetOff                                       39
054600160616     C                   EndIf
054700160616     c*
054800160616     C                   EndIf
054900160616     c                   EndIf
055000160616     C*
055100160616     C                   Z-ADD     0             IMT
055200160616     C     MRAIMS        ADD       MRASNS        IMT
055300160616     C   38              ADD       MRAIMN        IMT
055400160616     C                   ADD       IMT           ITC
055500160616     C   39              ADD       MRAIMN        IMN
055600160616     C                   ADD       MRASNS        SNS
055700160616     C                   ADD       MRAIMS        IMS
055800160616     C                   ADD       MRARAC        RAC
055900160616     C                   ADD       NETTO         NET
056000160616     C*
056100160616     C                   endif
056200160616     C*
056300160616     C     ENDSU1        ENDSR                                                  *
056400160616     C*----------------------------------------------------*
056500160616     C* SUBROUTINE SCRITTURA FILE DA DS MULTIPLA           *
056600160616     C*----------------------------------------------------*
056700160616     C     SCRIVI        BEGSR                                                  *
056800160616     C*
056900160616     C     1             DO        50            X                              *
057000160616     C*
057100160616     C     RGPT(X)       IFEQ      *BLANK                                       *
057200160616     C                   GOTO      ENDSUB                                       *
057300160616     C                   END                                                    *
057400160616     C*
057500160616     C     X             OCCUR     ND770                                60      *
057600160616     C   60              GOTO      ENDSUB                                       *
057700160616     C*
057800160616     C* Scrittura nuovo record
057900160616     C                   Clear                   SDGN77000                      *
058000160616     C                   MOVE      SOC           N77SOCIETA                     *
058100160616     C                   Z-ADD     ACF           N77ACF                         *
058200160616     C                   MOVE      QUA           N77QUA                         *
058300160616     C                   MOVE      KCC           N77KCC                         *
058400160616     C                   MOVE      KSC           N77KSC                         *
058500160616     C                   MOVE      RGT           N77RGT                         *
058600160616     C                   MOVE      PRG           N77PRG                         *
058700160616     C                   MOVE      CDF           N77CDF                         *
058800160616     C                   MOVE      COG           N77COG                         *
058900160616     C                   MOVE      NOM           N77NOM                         *
059000160616     C                   MOVE      DNC           N77DNC                         *
059100160616     C                   MOVE      SES           N77SES                         *
059200160616     C     CDE           IFEQ      *BLANK
059300160616     C                   MOVE      CIT           N77CIT                         *
059400160616     C                   MOVE      VIA           N77VIA                         *
059500160616     C                   ELSE
059600160616     C                   MOVE      *BLANKS       N77CIT                         *
059700160616     C                   MOVE      *BLANKS       N77VIA                         *
059800160616     C                   ENDIF
059900160616     C                   MOVEL     C77           N77C77                         *
060000160616     C                   Z-ADD     ITC           N77ITC                         *
060100160616     C                   Z-ADD     IMN           N77IMN                         *
060200160616     C                   Z-ADD     SNS           N77SNS                         *
060300160616     C                   Z-ADD     IMS           N77IMS                         *
060400160616     C                   Z-ADD     PRA           N77PRA                         *
060500160616     C                   Z-ADD     RAC           N77RAC                         *
060600160616     C                   Z-ADD     0             N77NET                         *
060700160616     C                   MOVE      PFG           N77PFG                         *
060800160616     C                   MOVEL     CSE           N77CSE
060900160616     C                   MOVEL     CDE           N77CDE
061000160616     C*
061100160616     C* ritenuta a titolo d'imposta
061200160616     C                   Z-ADD     0             N77RIM
061300160616     C*
061400160616     C                   WRITE     SDGN77000                                    *
061500160616     C*
061600160616     C                   ENDDO                                                  *
061700160616     C*
061800160616     C     ENDSUB        ENDSR                                                  *
061900160616     C************************************************************
062000160616     C* REPERIMENTO DATI da ANPDC
062100160616     C************************************************************
062200160616     C     READANPDC     BEGSR
062300160616     C*
062400160616     C                   CLEAR                   ND002DS
062500160616     C                   EVAL      LenOut = %Size(ND002ds)
062600160616     C                   CallB     'NDMVC002'
062700160617     C                   PARM                    SocR70
062800160616     C                   PARM                    APEKcc
062900160616     C                   PARM                    APEKsc
063000160616     C                   PARM      DatMin        Data
063100160616     C                   PARM      *OFF          GesMsg
063200160616     C                   PARM      *ON           Esito
063300160616     C                   PARM      'ND002DS'     Struttura
063400160616     C                   PARM                    ND002DS
063500160616     C                   PARM                    LenOut
063600160616     C*
063700160616     C                   ENDSR
063800160616     C************************************************************
063900160616     C* CONTROLLO SOGGETTI
064000160616     C************************************************************
064100160616     C     CTRSOG        BEGSR
064200160616     C*
064300160616     C                   MOVE      '1'           DVARIC
064400160617     C                   MOVE      SocR70        DVASOCIETA
064500160616     C                   move      *blank        DVASTRUTT
064600160616     C                   MOVEL     'DVASOG'      DVASTRUTT
064700160616     C                   EVAL      DVALenOut = %Size(DVASOG)
064800160616     C                   MOVE      sogg002       DVASOGG
064900160616     C                   Clear                   DvaTpInd
065000160616     C                   Clear                   DvaCdInd
065100160617     C                   If        IndirR70      = '1'
065200160616     C                   If        APETpInd      <> *blanks and
065300160616     C                             APECdInd      <> *blanks
065400160616     C                   MOVE      APETpInd      DvaTpInd
065500160616     C                   MOVE      APECdInd      DvaCdInd
065600160616     C                   EndIf
065700160616     C                   EndIf
065800160616     C                   CALLB     'NDDVASOG'
065900160616     C                   PARM                    DVARIC
066000160616     C                   PARM                    DVASOCIETA
066100160616     C                   PARM                    DVAUNITA
066200160616     C                   PARM                    DVASTRUTT
066300160616     C                   PARM                    DVADTRIF
066400160616     C                   PARM                    DVALENOUT
066500160616     C                   PARM                    DVASNATURA
066600160616     C                   PARM                    *omit
066700160616     C                   PARM                    DVALINEAV
066800160616     C                   PARM                    DVAFILIALE
066900160616     C                   PARM                    DVASOGG
067000160616     C                   PARM                    DVATPIND
067100160616     C                   PARM                    DVACDIND
067200160616     C                   PARM                    DVAERRORE
067300160616     C                   PARM                    DVAOUTPUT
067400160616     C     DVAERRORE     IFEQ      *OFF
067500160616     C                   EVAL      %SUBST(DVASOG:1:DVALENOUT)
067600160616     C                              = %SUBST(DVAOUTPUT:1:DVALENOUT)
067700160616     C                   ENDIF
067800160616     C*
067900160616     C                   ENDSR
068000160616     C************************************************************
068100160616     C* DATI ANAGRAFICI VARIAZIONE ESTREMI FISCALI
068200160616     C************************************************************
068300160616     C     CALLDVACPI    BEGSR
068400160616     C*
068500160616     C                   move      '2'           DVCRIC
068600160616     C                   move      MRASocieta    DVCSOCIETA
068700160616     C                   move      *blank        DVCUNITA
068800160616     C                   movel     'ANCPI'       DVCSTRUTT
068900160616     C                   move      MRADDC        DVCDTRIF
069000160616     C                   eval      DVCLENOUT = %Size(ANCPI)
069100160616     C                   move      sogg002       DVCSOGG
069200160616     C*
069300160616     C                   CALLB     'NDDVACPI'    PDVACPI
069400160616     C*
069500160616     C                   ENDSR
069600160616     C/EJECT
069700160616     C************************************************************
069800160616     C* Controllo stato
069900160616     C************************************************************
070000160616     C     CTRSTATO      BEGSR
070100160616     C*
070200160616     C*
070300160616     C                   Clear                   SDGTABDS
070400160616     C*
070500160616     C                   Eval      XTbRic      = '1'
070600160617     C                   Eval      XTbAzi      = SocR70
070700160616     C                   Eval      XTbKey      = *Zero
070800160616     C                   Eval      XTbCod      = 'G12'
070900160616     C                   Move      DVSSTATO      XTbkey
071000160616     C*
071100160616     C                   CALL      'SDGTABDR'
071200160616     C                   PARM                    SDGTABDS
071300160616     C*
071400160616     C                   if        XTbErr      = '0'
071500160616     C                   MoveL     XTbUni        TABG12
071600160616     C*
071700160616     C                   if        NazG12 = 'IT' or
071800160616     C                             CodUicG12 = 086
071900160620     C                   Movel     *Off          $SoggEst
072000160616     C                   Else
072100160616     C                   Movel     *On           $SoggEst
072200160616     C                   Endif
072300160616     C*
072400160616     C                   Endif
072500160616     C*
072600160616     C                   ENDSR
072700160616     C/EJECT
072800160616      ************************************************************
