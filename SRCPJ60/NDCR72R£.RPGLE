000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200000000     H*PARMS BNDDIR(PJXBND PJCBND)
000300000000     H*PARMS CVTOPT(*DATETIME)
000400170220     H DECEDIT('0,') DATEDIT(*DMY.) BNDDIR('PJXBND':'PJCBND')
000500000000     F*---------------------------------------------------------------*
000600000000     F*         ESTRAZIONE RIGHE PERCIPIENTE CON TOTALI PER:          *
000700000000     F*         - TRIBUTO/PROGRESSIVO TRIBUTO                         *
000800000000     F*          OPPURE                                               *
000900000000     F*         - CODICE DI RAGGRUPPAMENTO TRIBUTO                    *
001000000000     F*---------------------------------------------------------------*
001100000000     F*    INDICATORI                                                 *
001200000000     F*                                                               *
001300000000     F* 90 FINE FILE                                                  *
001400000000     F* 91 CHAIN ANTRI00F                                             *
001500000000     F* 50 LOOKUP SU TABELLA RAGGRUPPAMENTI TRIBUTI                   *
001600000000     F*    50 = OFF  INSERIMENTO NELLA TABELLA RAGGR. TRIBUTI         *
001700000000     F*    50 = ON   TOTALIZZAZIONE IMPORTI TABELLA RAGGR. TRIBUTI    *
001800000000     F* 51 LOOKUP CON *BLANK PER REPERIRE PRIMO LIBERO                *
001900000000     F* 60 INDICATORE DI OCCUR SU DS MULTIPLA RAGGRUPPAMENTI          *
002000000000     F*---------------------------------------------------------------*
002100000000     F*  --- FLUSSO PRINCIPALE ---                                    *
002200000000     F*                                                               *
002300000000     F*  - CANCELLAZIONE RECORD ESISTENTI PER L'ANNO DI COMPETENZA    *
002400000000     F*    SELEZIONATO NEL FILE ND77001L                              *
002500000000     F*                                                               *
002600000000     F*  - LETTURA ANAGRAFICO PERCIPIENTI                             *
002700000000     F*                                                               *
002800000000     F*  - LETTURA RITENUTE DEL PERCIPIENTE                           *
002900000000     F*                                                               *
003000000000     F*  - SELEZIONE DELLE RITENUTE CHE HANNO:                        *
003100000000     F*               1 - ANNO DI COMPETENZA SELEZIONATO A VIDEO      *
003200000000     F*               2 - STATO=3 (QUIETANZATI)                       *
003300000000     F*               3 - ALMENO UN IMPORTO DIVERSO DA 0              *
003400000000     F*               4 - QUADRO = SC - SE                            *
003500000000     F*                                                               *
003600000000     F*  - INSERIMENTO DEI MOVIMENTI ELABORATI IN UNA DS MULTIPLA     *
003700000000     F*    CHE LI RACCOGLIE PER TRIBUTO/PROGR. O COD. RAGGRUPPAMENTO  *
003800000000     F*                                                               *
003900000000     F*  - TOTALIZZAZIONE DEGLI IMPORTI NEL CASO IL MOVIMENTO APPAR-  *
004000000000     F*    TENGA AD UN TRIBUTO/PROGR. O COD. RAGGR. GIA' INSERITO     *
004100000000     F*                                                               *
004200000000     F*  - LETTURA NUOVO RECORD RITENUTE                              *
004300000000     F*                                                               *
004400000000     F*  - PER FINE MOVIMENTI DEL PERCIPIENTE MEMORIZZO DS MULTIPLA   *
004500000000     F*    NEL FILE ND77001L.                                         *
004600000000     F*                                                               *
004700000000     F*  - LETTURA NUOVO RECORD PERCIPIENTI                           *
004800000000     F*                                                               *
004900000000     F*  - FINE FILE PERCIPIENTI VAI A FINE                           *
005000000000R235 F*
005100000000R235 F* Modificata dicitura pgm,
005200000000R235 F*  da Estrazione righe percipienti mod.770
005300000000R235 F*   a Estrazione percipienti per CU  Batch
005400000000     F*---------------------------------------------------------------*
005500000000     FANAPE01L  IF   E           K DISK
005600000000R235 F*------------
005700000000 "   F* Storico percipienti in regime agevolato
005800000000 "   F* key: Soc/Kcc/Ksc/DtIni(desc)    UNIQUE
005900000000 "   FSDGSPA01L IF   E           K DISK
006000000000 "   F*------------
006100000000 "   F* NDPAS/NDMOV/NDREG, soc/ctb/kcc/ksc/dtp/n
006200000000 "   FNDPAS02J  IF   E           K DISK
006300000000 "   F*------------
006400000000 "   F* Movimenti IVA
006500000000 "   F* key: Sys/NrAsReg  UNIQUE
006600000000R235 FNDIVA01L  IF   E           K DISK
006700000000     F*------------
006800000000     FNDMRA01L  IF   E           K DISK
006900000000     F*------------
007000000000     FANTRI01L  IF   E           K DISK
007100000000     F*------------
007200000000     FND77001L  UF A E           K DISK
007300000000     F*---------------------------------------------------------------*
007400000000R235 D* Prototipazione procedure EURO
007500000000R235 D/COPY *LIBL/SRCCPY,PJX002PR
007600000000     D*------------
007700000000     D KPJBA         E DS
007800000000     D*-------------
007900000000     D ndcr70ds      E DS
008000000000     D*------------
008100000000     D* Richiamo a XATB
008200000000     D XATBDS        E DS
008300000000     D TABG16        E DS                  EXTNAME(ANGG16DS) INZ
008400000000     D*------------
008500000000C1788D* Tabella esclusione tributi 2%
008600000000C1788D TAB022        E DS                  EXTNAME(ANG022DS) INZ
008700000000R235 D*------------
008800000000 "   D* Tabella causali di pagamento percipienti in regime
008900000000 "   D* agevolato
009000000000 "   D SDGHK1DS      E DS                  INZ
009100000000 "   D*------------
009200000000 "   D* Tabella Stati
009300000000R235 D TABG12        E DS                  EXTNAME(ANGG12DS) INZ
009400000000     D*------------
009500000000     D* Definizioni parametri driver soggetti
009600000000     D DVARIC          S              1
009700000000     D DVASOCIETA      S              3
009800000000     D DVAUNITA        S              8
009900000000     D DVADTRIF        S               D
010000000000     D DVASTRUTT       S             10
010100000000     D DVALENOUT       S              9B 0
010200000000     D DVASNATURA      S              1
010300000000     D DVACODICE       S              8
010400000000     D DVALINEAV       S              3
010500000000     D DVAFILIALE      S              3  0
010600000000     D DVASOGG         S              8
010700000000     D DVATPIND        S              2
010800000000     D DVACDIND        S              3
010900000000     D DVAOUTPUT       S           4000
011000000000     D DVAERRORE       S              1
011100000000     D*------------
011200000000     D* DS esterna per dati soggetto
011300000000    >D DVASOG        E DS
011400000000     D*-------------
011500000000     D* DS esterna per decodifica conto
011600000000     D ND002DS       E DS
011700000000     D*-------------
011800000000     D* Definizioni parametri standard chiamate moduli
011900000000     D Esito           S              1
012000000000     D GesMsg          S              1
012100000000     D LenOut          S              9B 0
012200000000     D Struttura       S             10
012300000000     D Data            S             10
012400000000     D*------------
012500000000D0495D* Definizioni parametri driver estremi fiscali
012600000000  "  D dvcRic          S              1
012700000000  "  D dvcSocieta      S              3
012800000000  "  D dvcUnita        S              8
012900000000  "  D dvcStrutt       S             10
013000000000  "  D dvcDtRif        S               D
013100000000  "  D dvcLenOut       S              9B 0
013200000000  "  D dvcSogg         S              8
013300000000  "  D dvcErrore       S              1
013400000000  "  D dvcOutput       S           4000
013500000000  "  D*-------------
013600000000  "  D* Ds per output variazioni estremi fiscali
013700000000  "  D ANCPI         E DS                  EXTNAME(ANCPI00F)
013800000000D0495D*-------------
013900000000D0495D*** RGPT            S              6    DIM(50)                           SCHIERA RAGGRUPP
014000000000R235 X*ex D0495  RGPT  S             26    DIM(50)                              SCHIERA RAGGRUPP
014100000000R235 D* Aumento di 20 caratteri la schiera di raggruppamento per inserire
014200000000 "   D* il codice fiscale/partita IVA come indice di scrittura
014300000000 "   D* dell'elemento di schiera
014400000000R235 D RGPT            S             46    DIM(50)                              SCHIERA RAGGRUPP
014500000000     D $C              S              4    DIM(30)
014600000000R235 D*------------
014700000000R235 D KeyRGPT         S             46
014800000000     D*------------
014900000000     D* Parametro RABASE
015000000000     D ANP012DS      E DS                  INZ
015100000000     D XPAOUT          DS          2000
015200000000     D*------------
015300000000     D ND770           DS                  OCCURS(50)
015400000000     D  SOC                    1      3
015500000000     D  ACF                    4      6P 0
015600000000     D  QUA                    7      8
015700000000     D  KCC                    9     14
015800000000     D  KSC                   15     22
015900000000     D  RGT                   23     26
016000000000     D  CDF                   27     46
016100000000     D  COG                   47     94
016200000000     D  NOM                   95    142
016300000000     D  LNC                  143    176
016400000000     D  DNC                  177    186
016500000000     D  PNC                  187    188
016600000000     D  SES                  189    189
016700000000     D  CIT                  190    223
016800000000     D  VIA                  224    267
016900000000     D  NUM                  268    270
017000000000     D  PRV                  271    272
017100000000     D  C77                  273    292
017200000000     D  ITC                  293    302P 3
017300000000     D  IMN                  303    312P 3
017400000000     D  SNS                  313    322P 3
017500000000     D  IMS                  323    332P 3
017600000000     D  PRA                  333    335P 2
017700000000     D  RAC                  336    345P 3
017800000000     D  NET                  346    355P 3
017900000000     D  PFG                  356    356
018000000000     D  CSE                  357    391
018100000000     D  SER                  392    400
018200000000     D  CDE                  401    425
018300000000     D  PRG                  426    427
018400000000     D*------------
018500000000     D TBTTRI          DS
018600000000     D  TRITRIBUTO             1      4
018700000000     D  TRITRIPROG             5      6
018800000000     D*------------
018900000000     D NETTO           S                   like(N77NET)
019000000000     D IMT             S                   like(N77ITC)
019100000000     D*------------
019200000000     D DatMin          C                   CONST('0001-01-01')
019300000000     D*------------
019400000000R235 D* Dati di ambiente ottenuti da SDGSOCDR
019500000000 "   D SOC001        E DS                  EXTNAME(SDGSOCDS )
019600000000 "   D*  DS per reperire il valore di XScSKP nella posizione 30
019700000000 "   D  StaSKP                30     30
019800000000 "   D*-------------
019900000000 "   D* DS Interna per dati di output di SDGSOCDR
020000000000 "   D XSOCDS          DS          1000
020100000000 "   D*----------------
020200000000 "   D* numeri decimali della m.c.
020300000000 "   D NrDecMC         S              1  0
020400000000 "   D WNrDecMC        S              1
020500000000 "   D*------------
020600000000 "   D* data di lavoro per "costruzione" date
020700000000 "   D  DataAlfa       DS            10
020800000000 "   D  AAAlfa                 1      4
020900000000 "   D  MMAlfa                 6      7
021000000000 "   D  GGAlfa                 9     10
021100000000 "   D*----------------
021200000000 "   D $RegAgev        S              1
021300000000 "   D WData           S               D
021400000000 "   D WDtAlfa         S             10A
021500000000 "   D $Causale        S                   Like(MOVCausTes)
021600000000 "   D $FineSPA        S              1    Inz(*Off)
021700000000 "   D $FinePart       S              1    Inz(*Off)
021800000000 "   D $FineFatt       S              1    Inz(*Off)
021900000000 "   D $FinePag        S              1    Inz(*Off)
022000000000 "   D $FineIVA        S              1    Inz(*Off)
022100000000 "   D $Fattura        S              1    Inz(*Off)
022200000000 "   D $NotaAccr       S              1    Inz(*Off)
022300000000 "   D $OkPart         S              1    Inz(*On )
022400000000 "   D $OkFatt         S              1    Inz(*On )
022500000000 "   D $OkPag          S              1    Inz(*On )
022600000000 "   D $TotFatt        S                   Like(MOVImporto)
022700000000 "   D $TotIVA         S                   Like(IVAImporto)
022800000000 "   D $TotPag         S                   Like(PASImporto)
022900000000 "   D WImpInp         S                   Like(ImporD0202)
023000000000 "   D WImpOut         S                   Like(ImporD0202)
023100000000 "   D*---------
023200000000 "   D* Definizioni parametri standard chiamate moduli
023300000000 "   D $Esito          S              1
023400000000 "   D $GesMsg         S              1
023500000000 "   D $LenOut         S              9B 0
023600000000 "   D $Struttura      S             10
023700000000 "   D $CodOper        S              1
023800000000 "   D $Output         S           4000
023900000000 "   D $Refresh        S              1
024000000000 "   D $Uso            S              1
024100000000 "   D*-------------
024200000000 "   D* Ds per reperimento dati su causali operative contabili
024300000000 "   D ANOPE         E DS                  ExtName(ANOPE00F) Inz
024400000000 "   D*-------------
024500000000 "   D SavKcc          S                   Like(PASKcc)
024600000000 "   D SavKsc          S                   Like(PASKsc)
024700000000 "   D SavDtPar        S                   Like(PASDtPar)
024800000000 "   D SavNrPar        S                   Like(PASNrPar)
024900000000 "   D SavSerPar       S                   Like(PASSerPar)
025000000000 "   D*-------------
025100000000 "   D $SOGGEST        S              1
025200000000 "   D $CDFIVA         S             20A
025300000000 "   D $NATGIUR        S                   like(CPINatgiur)
025400000000 "   D XCodFisc        S                   like($CDFIva)
025500000000R235 D ErrFisc         S              1  0
025600000000     C*---------------------------------------------------------------*
025700000000     C*
025800000000     C                   EXSR      INZVAR                                       *
025900000000     C*
026000000000     C* Cancellazione eventuali record per l'anno selezionato
026100000000     C                   EXSR      DEL770                                       *
026200000000     C*
026300000000R235 C                   Eval      $RegAgev    = *Off
026400000000R235 C*
026500000000     C* Ciclo lettura percipienti
026600000000     C     socR70        SETLL     ANAPE000                                     *
026700000000     C*
026800000000     C                   DO        *HIVAL                                       *  -1-
026900000000     C     socR70        READE     ANAPE000                               90    *
027000000000R235 X***90              GOTO      FINE                                         *
027100000000R235 C   90              GOTO      PERCMIN                                      *
027200000000 "   C*
027300000000R235 C                   Eval      $SoggEst   = *on
027400000000     C*
027500000000D0495C* Reperisco dati conto
027600000000  "  C                   EXSR      READANPDC
027700000000  "  C     Esito         IfEq      *off
027800000000  "  C* Recupero dati soggetto
027900000000  "  C                   EXSR      CTRSOG
028000000000  "  C     DVAERRORE     IfEq      *ON
028100000000  "  C                   Clear                   DVASOG
028200000000  "  C                   EndIf
028300000000  "  C                   EndIf
028400000000D0495C*
028500000000     C                   move      apekcc        mrakcc
028600000000     C                   move      apeksc        mraksc
028700000000     C     KEYMRA        SETLL     NDMRA01L                                     *
028800000000     C*
028900000000     C                   DO        *HIVAL                                       *   -2-
029000000000     C     KEYMRA        READE     NDMRA01L                               90    *
029100000000     C*
029200000000     C  N90MRAACF        IFEQ      annoR70                                      *  -3-
029300000000     C     MRASTA        ANDEQ     '3'                                          *
029400000000     C*
029500000000     C     MRAIMN        IFEQ      0                                            *
029600000000     C     MRASNS        ANDEQ     0                                            *
029700000000     C     MRAIMS        ANDEQ     0                                            *
029800000000     C     MRARAC        ANDEQ     0                                            *
029900000000     C                   GOTO      LAB100                                       *
030000000000     C                   ENDIF                                                  *  -4-
030100000000R235 C*
030200000000 "   C* controllo se il percipiente è nel periodo agevolato alla data
030300000000 "   C* del documento, se lo è salto il movimento perchè altrimenti
030400000000 "   C* lo estrarrei 2 volte
030500000000 "   C                   EXSR      CtrRegAgev
030600000000 "   C                   if        $RegAgev    = *On
030700000000 "   C                   GOTO      LAB100
030800000000R235 C                   endif
030900000000     C*
031000000000D0495C* Reperisco eventuali dati variazioni fisali
031100000000  "  C                   Clear                   VarCPIva         20
031200000000  "  C                   Exsr      CallDVACPI
031300000000  "  C     DVCErrore     IfEq      *off
031400000000  "  C                   Movel     DVCOutput     AnCPI
031500000000  "  C     CPICdFisc     IFNE      *BLANK
031600000000  "  C                   MOVEL     CPICdFisc     VarCPIva
031700000000  "  C                   ELSE
031800000000  "  C                   MOVEL     CpIPartIva    VarCPIva
031900000000D0495C                   ENDIF
032000000000R235 C*
032100000000 "   C                   Eval      $CDFIva = VarCPIva
032200000000 "   C                   EVAL      $NatGiur = CPINatGiur
032300000000R235 C*
032400000000D0495C                   EndIf
032500000000D0495C*
032600000000R235 C* e controlla il codice fiscale e la partita iva
032700000000  "  C                   Exsr      CTRStato
032800000000  "  C                   Exsr      CTRFiscIva
032900000000  "  C*
033000000000  "  C* Sono sicuramente percipienti esteri se
033100000000  "  C* nell'anagrafica percipiente è valorizzato l'identificativo
033200000000  "  C* fiscale estero e se lo stato è estero e il cod.fisc o la
033300000000  "  C* partita iva non sono italiani.
033400000000  "  C* Se estero, in CU non deve entrare
033500000000  "  C                   If        $SoggEst <> *on and
033600000000  "  C                             ApeCdFisc = *blanks
033700000000R235 C*
033800000000     C                   MOVE      MRATRI        TRITRIBUTO
033900000000     C                   MOVE      MRAPTR        TRITRIPROG
034000000000     C     KEYTRI        CHAIN     ANTRI01L                           91
034100000000     C   91              CLEAR                   ANTRI000
034200000000     C*
034300000000     C  N91TRIQUA770     IFEQ      'AU'
034400000000     C                   EXSR      CARDS
034500000000     C                   ENDIF
034600000000     C*
034700000000R235 C                   Endif
034800000000R235 C*
034900000000     C     LAB100        TAG
035000000000     C                   ENDIF                                                  *  -3-
035100000000     C  N90              ENDDO                                                  *  -2-
035200000000     C*
035300000000     C     RGPT(1)       IFNE      *BLANK                                       *  -6-
035400000000     C                   EXSR      SCRIVI                                       *
035500000000     C                   EXSR      INZDS                                        *
035600000000     C                   ENDIF                                                  *  -6-
035700000000     C*
035800000000     C                   ENDDO                                                  *  -1-
035900000000R235 C*
036000000000 "   C*
036100000000 "   C     PERCMIN       TAG
036200000000 "   C*
036300000000 "   C* Reperisco gli importi relativi ai percipienti in regime
036400000000 "   C* agevolato
036500000000R235 C                   EXSR      RepRegAgev
036600000000     C*
036700000000     C     FINE          TAG                                                    *
036800000000     C                   SETON                                        LR        *
036900000000     C*
037000000000     C*----------------------------------------------------*
037100000000     C* SUBROUTINE INIZIALE                                *
037200000000     C*----------------------------------------------------*
037300000000     C     INZVAR        BEGSR                                                  *
037400000000     C*
037500000000     C     *ENTRY        PLIST                                                  *
037600000000     C                   PARM                    KPJBA                          *
037700000000     C*
037800000000     C                   movel     kpjbu         ndcr70ds
037900000000     C*
038000000000     C     KEY770        KLIST                                                  *
038100000000     C                   KFLD                    socR70
038200000000     C                   KFLD                    N77ACF
038300000000     C*
038400000000     C     KEYMRA        KLIST                                                  *
038500000000     C                   KFLD                    socR70
038600000000     C                   KFLD                    MRAKCC
038700000000     C                   KFLD                    MRAKSC
038800000000     C*
038900000000     C     KEYTRI        KLIST
039000000000     C                   KFLD                    socR70
039100000000     C                   KFLD                    TRITRIBUTO
039200000000     C                   KFLD                    TRITRIPROG
039300000000R235 C*
039400000000 "   C* SDGSPA01L
039500000000 "   C     K03SPA01      KLIST
039600000000 "   C                   KFLD                    SPASocieta
039700000000 "   C                   KFLD                    SPAKcc
039800000000 "   C                   KFLD                    SPAKsc
039900000000 "   C*
040000000000 "   C* NDIVA01L
040100000000 "   C     K02IVA01      KLIST
040200000000 "   C                   KFLD                    IVASys
040300000000 "   C                   KFLD                    IVANrAsReg
040400000000 "   C*
040500000000 "   C* ANAPE01L
040600000000 "   C     K03APE01      KLIST
040700000000 "   C                   KFLD                    APESocieta
040800000000 "   C                   KFLD                    APEKcc
040900000000 "   C                   KFLD                    APEKsc
041000000000 "   C*
041100000000 "   C* NDPAS02J
041200000000 "   C     K04PAS02      KLIST
041300000000 "   C                   KFLD                    PASSocieta
041400000000 "   C                   KFLD                    PASCtb
041500000000 "   C                   KFLD                    PASKcc
041600000000 "   C                   KFLD                    PASKsc
041700000000 "   C*
041800000000 "   C     K07PAS02      KLIST
041900000000 "   C                   KFLD                    PASSocieta
042000000000 "   C                   KFLD                    PASCtb
042100000000 "   C                   KFLD                    PASKcc
042200000000 "   C                   KFLD                    PASKsc
042300000000 "   C                   KFLD                    PASDtPar
042400000000 "   C                   KFLD                    PASNrPar
042500000000 "   C                   KFLD                    PASSerPar
042600000000 "   C*
042700000000 "   C     K10PAS02      KLIST
042800000000 "   C                   KFLD                    PASSocieta
042900000000 "   C                   KFLD                    PASCtb
043000000000 "   C                   KFLD                    PASKcc
043100000000 "   C                   KFLD                    PASKsc
043200000000 "   C                   KFLD                    PASDtPar
043300000000 "   C                   KFLD                    PASNrPar
043400000000 "   C                   KFLD                    PASSerPar
043500000000 "   C                   KFLD                    PASDtReg
043600000000 "   C                   KFLD                    PASSys
043700000000 "   C                   KFLD                    PASNrAsReg
043800000000 "   C*
043900000000 "   C     K11PAS02      KLIST
044000000000 "   C                   KFLD                    PASSocieta
044100000000 "   C                   KFLD                    PASCtb
044200000000 "   C                   KFLD                    PASKcc
044300000000 "   C                   KFLD                    PASKsc
044400000000 "   C                   KFLD                    PASDtPar
044500000000 "   C                   KFLD                    PASNrPar
044600000000 "   C                   KFLD                    PASSerPar
044700000000 "   C                   KFLD                    PASDtReg
044800000000 "   C                   KFLD                    PASSys
044900000000 "   C                   KFLD                    PASNrAsReg
045000000000R235 C                   KFLD                    PASNrRigaM
045100000000D0495C*
045200000000  "  C     PDVACPI       PLIST
045300000000  "  C                   PARM                    DVCRIC
045400000000  "  C                   PARM                    DVCSOCIETA
045500000000  "  C                   PARM                    DVCUNITA
045600000000  "  C                   PARM                    DVCSTRUTT
045700000000  "  C                   PARM                    DVCDTRIF
045800000000  "  C                   PARM                    DVCLENOUT
045900000000  "  C                   PARM                    DVCSOGG
046000000000  "  C                   PARM                    DVCERRORE
046100000000D0495C                   PARM                    DVCOUTPUT
046200000000     C*
046300000000     C* Reperimento parametro RABASE
046400000000     C                   CallB     'XPAR'
046500000000     C                   Parm                    SocR70
046600000000     C                   Parm      'RABASE  '    XPaPar            8
046700000000     C                   Parm                    XPaOut
046800000000     C                   Parm      *Off          XPaErr            1
046900000000     C                   Parm      ' '           XPaRic            1
047000000000     C     XPaErr        IfEq      *On
047100000000     C                   Goto      Fine
047200000000     C                   Else
047300000000     C                   MoveL     XPaOut        ANP012DS
047400000000     C                   EndIf
047500000000R235 C*
047600000000 "   C                   EXSR      REPSOC
047700000000 "   C*
047800000000 "   C* reperisco dai dati di società i numeri dei decimali della moneta di
047900000000 "   C* conto
048000000000 "   C                   Eval      WNrDecMC   =  %SubSt(XScSKp:6:1)
048100000000R235 C                   Move      WNrDecMC      NrDecMC
048200000000     C*
048300000000     C* Inizializzo DS multipla
048400000000     C                   EXSR      INZDS
048500000000     C*
048600000000     C                   ENDSR                                                  *
048700000000     C*
048800000000     C*----------------------------------------------------*
048900000000     C* SUBROUTINE CANCELLAZIONE RECORD X ANNO COMPETENZA  *
049000000000     C*----------------------------------------------------*
049100000000     C     DEL770        BEGSR                                                  *
049200000000     C*
049300000000     C                   z-add     annoR70       N77ACF
049400000000     C     KEY770        SETLL     ND77001L                                     *
049500000000     C                   DO        *HIVAL                                       *
049600000000     C     KEY770        READE     ND77001L                               90    *
049700000000     C   90              LEAVE                                                  *
049800000000     C                   DELETE    ND770000                                     *
049900000000     C                   ENDDO                                                  *
050000000000     C*
050100000000     C                   ENDSR                                                  *
050200000000R235 C************************************************************
050300000000 "   C* Reperimento dati società
050400000000 "   C************************************************************
050500000000R235 C     REPSOC        BEGSR
050600000000     C*
050700000000     C                   CALL      'SDGSOCDR'
050800000000     C                   PARM      'SOC001'      TIPXSC            6
050900000000     C                   PARM      SocR70        SOCXSC            3
051000000000     C                   PARM                    CDSXSC            9 0
051100000000     C                   PARM                    MODXSC            3
051200000000     C                   PARM      *BLANKS       RTNXSC            1
051300000000     C                   PARM                    XSOCDS
051400000000     C                   PARM                    KPJBA
051500000000     C*
051600000000     C                   if        RtnXsc     <> '1'
051700000000     C                   MoveL     XSOCDS        SOC001
051800000000     C                   else
051900000000     C                   GOTO      FINE
052000000000     C                   endif
052100000000     C*
052200000000R235 C                   ENDSR
052300000000R235 C/EJECT
052400000000     C*----------------------------------------------------*
052500000000     C*   INIZIALIZZAZIONE DS MULTIPLA                     *
052600000000     C*----------------------------------------------------*
052700000000     C     INZDS         BEGSR                                                  *
052800000000     C*
052900000000     C                   MOVEA     *BLANK        RGPT                           *
053000000000     C*                                                                         *
053100000000     C                   DO        50            X                 2 0          *
053200000000     C     X             OCCUR     ND770                                        *
053300000000     C                   MOVE      *BLANK        SOC
053400000000     C                   Z-ADD     0             ACF
053500000000     C                   MOVE      *BLANK        QUA
053600000000     C                   MOVEL     0             KCC
053700000000     C                   MOVEL     0             KSC
053800000000     C                   MOVE      *BLANK        RGT
053900000000     C                   MOVE      *BLANK        CDF
054000000000     C                   MOVE      *BLANK        COG
054100000000     C                   MOVE      *BLANK        NOM
054200000000     C                   MOVE      *BLANK        LNC
054300000000     C                   MOVE      *BLANK        DNC
054400000000     C                   MOVE      *BLANK        PNC
054500000000     C                   MOVE      *BLANK        SES
054600000000     C                   MOVE      *BLANK        CIT
054700000000     C                   MOVE      *BLANK        VIA
054800000000     C                   MOVE      *BLANK        NUM
054900000000     C                   MOVE      *BLANK        PRV
055000000000     C                   MOVE      *BLANK        C77
055100000000     C                   MOVE      *BLANK        CSE
055200000000     C                   MOVE      *BLANK        SER
055300000000     C                   MOVE      *BLANK        CDE
055400000000     C                   MOVE      *BLANK        PRG
055500000000     C                   Z-ADD     0             ITC
055600000000     C                   Z-ADD     0             IMN
055700000000     C                   Z-ADD     0             SNS
055800000000     C                   Z-ADD     0             IMS
055900000000     C                   Z-ADD     0             PRA
056000000000     C                   Z-ADD     0             RAC
056100000000     C                   Z-ADD     0             NET
056200000000     C                   MOVE      *BLANK        PFG
056300000000     C                   ENDDO
056400000000     C*
056500000000     C                   ENDSR                                                  *
056600000000     C*----------------------------------------------------*
056700000000     C* SUBROUTINE CARICAMENTO DS MULTIPLA                 *
056800000000     C*----------------------------------------------------*
056900000000     C     CARDS         BEGSR                                                  *
057000000000     C*
057100000000     C                   Z-ADD     1             X                              *
057200000000     C*
057300000000     C* Reperimento tributo su schiera tributi
057400000000     C*
057500000000     C                   If        RaggRGTR70 = *On
057600000000     C* codice raggr. tributo
057700000000     C                   Move      *Blank        RACTRI
057800000000R186 X***                MoveL     TRITriRagg    RACTRI
057900000000R186 C                   Eval      RacTri = TRITriRagg +
058000000000R186 C                                      %Subst(TRIDES770: 1:2)
058100000000     C                   Else
058200000000     C* tributo/progressivo
058300000000     C                   MOVEL     TBTTRI        RACTRI            6
058400000000     C                   EndIf
058500000000R235 X*ex D0495          MOVEL     VarCPIva      KeyRGPT          26
058600000000R235 X*ex D0495          MOVE      RacTri        KeyRGPT
058700000000R235 C*
058800000000 "   C     $CdFIVA       CAT       VarCPIva:0    KeyRGPT
058900000000 "   C                   CAT       RacTri:0      KeyRGPT
059000000000 "   C*
059100000000 "   C                   Eval      KeyRGPT     = $CdFIVA + VarCPIva +
059200000000R235 C                                           RacTri
059300000000D0495C***  RACTRI        LOOKUP    RGPT(X)                                50    *
059400000000D0495C     KeyRGPT       LOOKUP    RGPT(X)                                50    *
059500000000     C*
059600000000     C* Inserimento tributo su schiera tributi
059700000000     C     *in50         ifeq      *off                                         *
059800000000     C                   Z-ADD     1             J                 3 0          *
059900000000     C     *BLANK        LOOKUP    RGPT(J)                                51    *
060000000000D0495C***51              MOVE      RACTRI        RGPT(J)                        *
060100000000D0495C   51              MOVE      KeyRGPT       RGPT(J)                        *
060200000000     C   51              Z-ADD     J             X                              *
060300000000     C                   endif                                                  *
060400000000     C*
060500000000     C* Inserimento o aggiornamento importi DS multipla
060600000000     C     X             OCCUR     ND770                                60      *
060700000000     C   60              GOTO      ENDSU1                                       *
060800000000R235 C                   if        $RegAgev    = *On
060900000000 "   C                   Z-ADD     $TotPag       NETTO
061000000000R235 C                   else
061100000000     C     MRAIMS        SUB       MRARAC        NETTO                          *
061200000000R235 C                   endif
061300000000     C*
061400000000D0495C***  *in50         ifeq      *off
061500000000 "   C* Reperisco dati conto
061600000000 "   C***                EXSR      READANPDC
061700000000 "   C***  Esito         ifeq      *off
061800000000 "   C* Recupero dati soggetto
061900000000 "   C***                EXSR      CTRSOG
062000000000 "   C***  DVAERRORE     IFEQ      *ON
062100000000 "   C***                CLEAR                   DVASOG
062200000000 "   C***                ENDIF
062300000000 "   C***                endif
062400000000D0495C***                endif
062500000000     C*
062600000000     C* Scrittura o aggiornamento record DS multipla
062700000000     C*
062800000000     C     *in50         ifeq      *off
062900000000     C*
063000000000R235 C                   if        $RegAgev    = *On
063100000000 "   C*
063200000000 "   C                   MOVE      SPASocieta    SOC
063300000000 "   C                   Z-ADD     AnnoR70       ACF
063400000000 "   C                   MOVE      TRIQua770     QUA
063500000000 "   C                   MOVEL     SPAKCC        KCC
063600000000 "   C                   MOVEL     SPAKSC        KSC
063700000000 "   C*
063800000000R235 C                   else
063900000000     C                   MOVE      MRASOCIETA    SOC
064000000000     C                   Z-ADD     MRAACF        ACF
064100000000     C                   MOVE      TRIQUA770     QUA
064200000000     C                   MOVEL     MRAKCC        KCC
064300000000     C                   MOVEL     MRAKSC        KSC
064400000000R235 C                   endif
064500000000R235 C*
064600000000     C                   If        RaggRGTR70 = *On
064700000000     C* codice raggr. tributo
064800000000     C                   Move      TRITriRagg    RGT
064900000000     C                   Move      *Blank        PRG
065000000000     C                   Else
065100000000     C* tributo/progressivo
065200000000     C                   MOVEL     TRITRIBUTO    RGT
065300000000     C                   MOVEL     TRITRIPROG    PRG
065400000000     C                   EndIf
065500000000D0495C     VarCPIva      IFNE      *BLANK
065600000000D0495C                   Movel     VarCpIva      CDF
065700000000D0495C                   Else
065800000000     C     DVSCDFISC     IFNE      *BLANK
065900000000     C                   MOVEL     DVSCDFISC     CDF
066000000000     C                   ELSE
066100000000     C                   MOVEL     DVSPARTIVA    CDF
066200000000     C                   EndIf
066300000000D0495C                   EndIf
066400000000     C                   MOVE      '1'           XTBRIC
066500000000     C                   MOVE      socR70        XTBAZI
066600000000     C                   MOVE      *ZERO         XTBKEY
066700000000     C                   MOVE      'G16'         XTBCOD
066800000000     C                   MOVE      DVSNATGIUR    XTBKEY
066900000000     C                   CALLb     'XATB'
067000000000     C                   PARM                    XATBDS
067100000000     C     XTBERR        IFEQ      '0'
067200000000     C                   MOVEL     XTBUNI        TABG16
067300000000     C                   ENDIF
067400000000     C                   if        TPNG16 = '1'
067500000000     C                   MOVEL     APECOGNOME    COG
067600000000     C                   MOVEL     APENOME       NOM
067700000000     C                   MOVEL     APELOCALIT    LNC
067800000000     C                   MOVEL     DVSDTNASC     DNC
067900000000     C                   MOVEL     APEPROV       PNC
068000000000     C                   IF        DVSSEX = '1'
068100000000     C                   MOVEL     'M'           SES
068200000000     C                   ENDIF
068300000000     C                   IF        DVSSEX = '2'
068400000000     C                   MOVEL     'F'           SES
068500000000     C                   ENDIF
068600000000     C                   MOVEL     'F'           PFG
068700000000     C                   Else
068800000000     C                   MOVEL     DVSDES        COG
068900000000     C                   MOVEL     *BLANK        NOM
069000000000     C                   MOVEL     *BLANK        LNC
069100000000     C                   MOVEL     DatMin        DNC
069200000000     C                   MOVEL     *BLANK        PNC
069300000000     C                   MOVEL     *BLANK        SES
069400000000     C                   MOVEL     'G'           PFG
069500000000     C                   Endif
069600000000     C                   MOVEL     DVSLOCALIT    CIT
069700000000     C                   MOVEL     DVSINDRIZ     VIA
069800000000     C                   MOVEL     DVSPROV       PRV
069900000000     C                   MOVEL     TRIDES770     C77
070000000000     C*
070100000000     C                   MOVEL     APECDFISC     CDE
070200000000     C     CDE           IFNE      *BLANK
070300000000     C                   MOVEL     *BLANK        CDF
070400000000     C                   ENDIF
070500000000     C                   MOVEL     APECDESMIN    CSE
070600000000     C     CSE           IFNE      *BLANK
070700000000     C* Qui ci andrebbe la descrizione dello stato estero
070800000000     C* (ex tab.70 di Orion), ma per ora non ci metto niente !!!
070900000000     C                   MOVEL     *BLANK        SER
071000000000     C                   ENDIF
071100000000C1788C*
071200000000  "  C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
071300000000  "  C* se il tributo in elaborazione è quello da escludere; in caso
071400000000  "  C* affermativo lo elimino dalla/e caselle indicate
071500000000  "  x*** ex c1667       SetOn                                        3839
071600000000  "  x***      "         If        EscItcR70 = *On And MraTri = EscTriR70
071700000000  "  x***      "                   And MraPtr = EscPrgR70
071800000000  "  x***      "         SetOff                                       38
071900000000  "  x***      "         EndIf
072000000000  "  x***      "         If        EscImnR70 = *On And MraTri = EscTriR70
072100000000  "  x***      "                   And MraPtr = EscPrgR70
072200000000  "  x***      "         SetOff                                       39
072300000000  "  x***      "         EndIf
072400000000  "  C*
072500000000  "  C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
072600000000  "  C* se il tributo in elaborazione è quello da escludere; in caso
072700000000  "  C* affermativo lo elimino dalla/e caselle indicate
072800000000  "  C                   SetOn                                        3839
072900000000  "  C                   If        EscItcR70 = *On or
073000000000  "  C                             EscImnR70 = *On
073100000000  "  c* vedo se esiste nella tabella 022
073200000000  "  C                   MOVE      '1'           XTBRIC
073300000000  "  C                   MOVE      socR70        XTBAZI
073400000000  "  C                   MOVE      *ZERO         XTBKEY
073500000000C1788C                   MOVE      '022'         XTBCOD
073600000000R235 C                   if        $RegAgev    = *On
073700000000  "  C                   MOVEl     SPATributo    wrk6              6
073800000000  "  C                   MOVE      SPATriProg    wrk6
073900000000R235 C                   else
074000000000C1788C                   MOVEl     MraTri        wrk6              6
074100000000C1788C                   MOVE      MraPtr        wrk6
074200000000R235 C                   endif
074300000000C1788C                   MOVE      wrk6          XTBKEY
074400000000  "  C                   CALLb     'XATB'
074500000000  "  C                   PARM                    XATBDS
074600000000  "  C     XTBERR        IFEQ      '0'
074700000000  "  C                   If        EscItcR70 = *On
074800000000  "  C                   SetOff                                       38
074900000000  "  C                   EndIf
075000000000  "  c*
075100000000  "  C                   If        EscImnR70 = *On
075200000000  "  C                   SetOff                                       39
075300000000  "  C                   EndIf
075400000000  "  c*
075500000000  "  C                   EndIf
075600000000C1788c                   EndIf
075700000000     C*
075800000000R235 C                   if        $RegAgev    = *On
075900000000 "   C   38              Z-ADD     $TotPag       ITC
076000000000 "   C   39              Z-ADD     $TotPag       IMN
076100000000 "   C                   Z-ADD     0             SNS
076200000000 "   C                   Z-ADD     0             IMS
076300000000 "   C                   Z-ADD     0             PRA
076400000000 "   C                   Z-ADD     0             RAC
076500000000R235 C                   else
076600000000     C     MRAIMS        ADD       MRASNS        ITC
076700000000C1667C   38              ADD       MRAIMN        ITC
076800000000  "  C  N39              Z-ADD     0             IMN
076900000000C1667C   39              Z-ADD     MRAIMN        IMN
077000000000     C                   Z-ADD     MRASNS        SNS
077100000000     C                   Z-ADD     MRAIMS        IMS
077200000000     C                   Z-ADD     MRAPRA        PRA
077300000000     C                   Z-ADD     MRARAC        RAC
077400000000R235 C                   endif
077500000000     C                   Z-ADD     NETTO         NET
077600000000     C*
077700000000     C                   else
077800000000C1788x*
077900000000  "  x* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
078000000000  "  x* se il tributo in elaborazione è quello da escludere; in caso
078100000000  "  x* affermativo lo elimino dalla/e caselle indicate
078200000000  "  x*** ex c1667       SetOn                                        3839
078300000000  "  x***      "         If        EscItcR70 = *On And MraTri = EscTriR70
078400000000  "  x***      "                   And MraPtr = EscPrgR70
078500000000  "  x***      "         SetOff                                       38
078600000000  "  x***      "         EndIf
078700000000  "  x***      "         If        EscImnR70 = *On And MraTri = EscTriR70
078800000000  "  x***      "                   And MraPtr = EscPrgR70
078900000000  "  x***      "         SetOff                                       39
079000000000  "  x***      "         EndIf
079100000000  "  C*
079200000000  "  C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
079300000000  "  C* se il tributo in elaborazione è quello da escludere; in caso
079400000000  "  C* affermativo lo elimino dalla/e caselle indicate
079500000000  "  C                   SetOn                                        3839
079600000000  "  C                   If        EscItcR70 = *On or
079700000000  "  C                             EscImnR70 = *On
079800000000  "  c* vedo se esiste nella tabella 022
079900000000  "  C                   MOVE      '1'           XTBRIC
080000000000  "  C                   MOVE      socR70        XTBAZI
080100000000  "  C                   MOVE      *ZERO         XTBKEY
080200000000C1788C                   MOVE      '022'         XTBCOD
080300000000R235 C                   if        $RegAgev    = *On
080400000000  "  C                   MOVEl     SPATributo    wrk6              6
080500000000  "  C                   MOVE      SPATriProg    wrk6
080600000000R235 C                   else
080700000000C1788C                   MOVEl     MraTri        wrk6              6
080800000000C1788C                   MOVE      MraPtr        wrk6
080900000000R235 C                   endif
081000000000C1788C                   MOVE      wrk6          XTBKEY
081100000000  "  C                   CALLb     'XATB'
081200000000  "  C                   PARM                    XATBDS
081300000000  "  C     XTBERR        IFEQ      '0'
081400000000  "  C                   If        EscItcR70 = *On
081500000000  "  C                   SetOff                                       38
081600000000  "  C                   EndIf
081700000000  "  c*
081800000000  "  C                   If        EscImnR70 = *On
081900000000  "  C                   SetOff                                       39
082000000000  "  C                   EndIf
082100000000  "  c*
082200000000  "  C                   EndIf
082300000000C1788c                   EndIf
082400000000     C*
082500000000R235 C                   if        $RegAgev    = *On
082600000000 "   C   38              ADD       $TotPag       ITC
082700000000 "   C   39              ADD       $TotPag       IMN
082800000000 "   C                   ADD       0             SNS
082900000000 "   C                   ADD       0             IMS
083000000000 "   C                   ADD       0             RAC
083100000000R235 C                   else
083200000000     C                   Z-ADD     0             IMT
083300000000     C     MRAIMS        ADD       MRASNS        IMT
083400000000C1667C   38              ADD       MRAIMN        IMT
083500000000     C                   ADD       IMT           ITC
083600000000C1667C   39              ADD       MRAIMN        IMN
083700000000     C                   ADD       MRASNS        SNS
083800000000     C                   ADD       MRAIMS        IMS
083900000000     C                   ADD       MRARAC        RAC
084000000000R235 C                   endif
084100000000R235 C*
084200000000     C                   ADD       NETTO         NET
084300000000     C*
084400000000     C                   endif
084500000000     C*
084600000000     C     ENDSU1        ENDSR                                                  *
084700000000     C*----------------------------------------------------*
084800000000     C* SUBROUTINE SCRITTURA FILE DA DS MULTIPLA           *
084900000000     C*----------------------------------------------------*
085000000000     C     SCRIVI        BEGSR                                                  *
085100000000     C*
085200000000     C     1             DO        50            X                              *
085300000000     C*
085400000000     C     RGPT(X)       IFEQ      *BLANK                                       *
085500000000     C                   GOTO      ENDSUB                                       *
085600000000     C                   END                                                    *
085700000000     C*
085800000000     C     X             OCCUR     ND770                                60      *
085900000000     C   60              GOTO      ENDSUB                                       *
086000000000     C*
086100000000     C* Scrittura nuovo record
086200000000C1478C                   Clear                   ND770000                       *
086300000000     C                   MOVE      SOC           N77SOCIETA                     *
086400000000     C                   Z-ADD     ACF           N77ACF                         *
086500000000     C                   MOVE      QUA           N77QUA                         *
086600000000     C                   MOVE      KCC           N77KCC                         *
086700000000     C                   MOVE      KSC           N77KSC                         *
086800000000     C                   MOVE      RGT           N77RGT                         *
086900000000     C                   MOVE      PRG           N77PRG                         *
087000000000     C                   MOVE      CDF           N77CDF                         *
087100000000     C                   MOVE      COG           N77COG                         *
087200000000     C                   MOVE      NOM           N77NOM                         *
087300000000     C                   MOVE      LNC           N77LNC                         *
087400000000     C                   MOVE      DNC           N77DNC                         *
087500000000     C                   MOVE      PNC           N77PNC                         *
087600000000     C                   MOVE      SES           N77SES                         *
087700000000D1183C     CDE           IFEQ      *BLANK
087800000000     C                   MOVE      CIT           N77CIT                         *
087900000000     C                   MOVE      VIA           N77VIA                         *
088000000000D1183C                   ELSE
088100000000  "  C                   MOVE      *BLANKS       N77CIT                         *
088200000000  "  C                   MOVE      *BLANKS       N77VIA                         *
088300000000D1183C                   ENDIF
088400000000     C                   MOVEL     C77           N77C77                         *
088500000000     C                   Z-ADD     ITC           N77ITC                         *
088600000000     C                   Z-ADD     IMN           N77IMN                         *
088700000000     C                   Z-ADD     SNS           N77SNS                         *
088800000000     C                   Z-ADD     IMS           N77IMS                         *
088900000000     C                   Z-ADD     PRA           N77PRA                         *
089000000000     C                   Z-ADD     RAC           N77RAC                         *
089100000000     C                   Z-ADD     0             N77NET                         *
089200000000     C                   MOVE      PFG           N77PFG                         *
089300000000     C                   MOVEL     SER           N77SER
089400000000     C                   MOVEL     CSE           N77CSE
089500000000     C                   MOVEL     CDE           N77CDE
089600000000     C     CDE           IFNE      *BLANK
089700000000     C                   MOVE      '*'           NUM
089800000000     C                   MOVE      *BLANK        PRV
089900000000     C                   ENDIF
090000000000     C                   MOVE      NUM           N77NUM
090100000000     C                   MOVE      PRV           N77PRV
090200000000C1478C                   MOVEL     *blanks       N77Tit
090300000000C1478C                   Z-add     0             N77Anno
090400000000C1478C                   Z-add     0             N77Reg
090500000000C1478C                   Z-add     0             N77ARG
090600000000C1478C                   Z-add     0             N77ARS
090700000000C1478C                   Z-add     0             N77IMSAP
090800000000C1478C                   Z-add     0             N77RACAP
090900000000C1478C                   MOVEL     *blanks       N77PIn
091000000000C1478C                   Z-add     0             N77DtIn
091100000000C1478C                   Z-add     0             N77DtFi
091200000000C1478C                   MOVEL     *blanks       N77COM
091300000000C1478C                   MOVEL     *blanks       N77RCDF
091400000000C1478C                   MOVEL     *blanks       N77RCOG
091500000000C1478C                   MOVEL     *blanks       N77RNOM
091600000000C1478C                   MOVEL     *blanks       N77RSES
091700000000C1478C                   MOVEL     *blanks       N77RLNC
091800000000C1478C                   MOVEL     DatMin        N77RDNC
091900000000C1478C                   MOVEL     *blanks       N77RPNC
092000000000C1770C*
092100000000  "  C* ritenuta a titolo d'imposta
092200000000  "  C                   Z-ADD     0             N77RIM
092300000000  "  C* addizionale a titolo d'imposta
092400000000  "  C                   Z-ADD     0             N77ARI
092500000000C1770C*
092600000000D1183C* i campi N77RVIA e N77RCIT indicano dal 2002 i dati degli esteri
092700000000  "  C     CDE           IFNE      *BLANK
092800000000  "  C                   MOVEL     CIT           N77RCIT                        *
092900000000  "  C                   MOVEL     VIA           N77RVIA                        *
093000000000D1183C                   ELSE
093100000000C1478C                   MOVEL     *blanks       N77RCIT
093200000000C1478C                   MOVEL     *blanks       N77RVIA
093300000000D1183C                   ENDIF
093400000000C1478C                   MOVEL     *blanks       N77RPRV
093500000000C1478C                   MOVEL     *blanks       N77RCSE
093600000000R102 C                   MOVEL     *blanks       N77CAUAS
093700000000R102 C                   MOVEL     *blanks       N77ASCDF
093800000000R102 C                   Z-Add     0             N77ASIMS
093900000000R102 C                   Z-Add     0             N77ASRAC
094000000000R102 C                   Z-Add     0             N77ASRIM
094100000000R102 C                   Z-Add     0             N77ASRSO
094200000000R102 C                   Z-Add     0             N77ASARA
094300000000R102 C                   Z-Add     0             N77ASARI
094400000000R102 C                   Z-Add     0             N77ASARS
094500000000     C*
094600000000R122 C* Campo CODICE (770 / 2013)
094700000000R122 C                   Clear                   N77Codice
094800000000R122 C                   If        N77NUM        = *blanks
094900000000R122 C                   If        N77IMN        > 0       or
095000000000R122 C                             N77SNS        > 0
095100000000R250 X***                Eval      N77Codice     = '3'
095200000000R250 C                   Eval      N77Codice     = '6'
095300000000R122 C                   EndIf                                                  *
095400000000R122 C                   EndIf                                                  *
095500000000     C*
095600000000R211 C                   Clear                   N77NRess
095700000000  "  C                   Clear                   N77DpCdf
095800000000  "  C                   Clear                   N77DpDen
095900000000  "  C                   Clear                   N77DpCenp
096000000000  "  C                   Clear                   N77DpCazi
096100000000  "  C                   Clear                   N77DpCate
096200000000  "  C                   Clear                   N77DpAcon
096300000000  "  C                   Clear                   N77DpImpa
096400000000  "  C                   Clear                   N77DpCodu
096500000000  "  C                   Clear                   N77DpCove
096600000000  "  C                   Clear                   N77OsCdf1
096700000000  "  C                   Clear                   N77OsCdf2
096800000000  "  C                   Clear                   N77OsCdf3
096900000000  "  C                   Clear                   N77F2Cdf
097000000000  "  C                   Clear                   N77F2Ime
097100000000  "  C                   Clear                   N77F2Rio
097200000000  "  C                   Clear                   N77F2Rino
097300000000  "  C                   Clear                   N77F2Cdf2
097400000000  "  C                   Clear                   N77F2Ime2
097500000000  "  C                   Clear                   N77F2Rio2
097600000000  "  C                   Clear                   N77F2Rino2
097700000000  "  C                   Clear                   N77F3Ine
097800000000  "  C                   Clear                   N77F3Rie
097900000000  "  C                   Clear                   N77F3Ain
098000000000  "  C                   Clear                   N77F3Ari
098100000000  "  C                   Clear                   N77F3Ine2
098200000000  "  C                   Clear                   N77F3Rie2
098300000000  "  C                   Clear                   N77F3Ain2
098400000000  "  C                   Clear                   N77F3Ari2
098500000000  "  C                   Eval      N77CitAP = N77Cit
098600000000  "  C                   Eval      N77PrvAP = N77Prv
098700000000R211 C                   Eval      N77Comap = N77Com
098800000000     C*
098900000000     C                   WRITE     ND770000                                     *
099000000000     C*
099100000000     C                   ENDDO                                                  *
099200000000     C*
099300000000     C     ENDSUB        ENDSR                                                  *
099400000000     C************************************************************
099500000000     C* REPERIMENTO DATI da ANPDC
099600000000     C************************************************************
099700000000     C     READANPDC     BEGSR
099800000000     C*
099900000000     C                   CLEAR                   ND002DS
100000000000     C                   EVAL      LenOut = %Size(ND002ds)
100100000000     C                   CallB     'NDMVC002'
100200000000     C                   PARM                    socR70
100300000000D0495C***                PARM                    MRAKCC
100400000000  "  C***                PARM                    MRAKSC
100500000000  "  C                   PARM                    APEKcc
100600000000D0495C                   PARM                    APEKsc
100700000000     C                   PARM      DatMin        Data
100800000000     C                   PARM      *OFF          GesMsg
100900000000     C                   PARM      *ON           Esito
101000000000     C                   PARM      'ND002DS'     Struttura
101100000000     C                   PARM                    ND002DS
101200000000     C                   PARM                    LenOut
101300000000     C*
101400000000     C                   ENDSR
101500000000     C************************************************************
101600000000     C* CONTROLLO SOGGETTI
101700000000     C************************************************************
101800000000     C     CTRSOG        BEGSR
101900000000     C*
102000000000     C                   MOVE      '1'           DVARIC
102100000000     C                   MOVE      socR70        DVASOCIETA
102200000000     C                   move      *blank        DVASTRUTT
102300000000     C                   MOVEL     'DVASOG'      DVASTRUTT
102400000000     C                   EVAL      DVALenOut = %Size(DVASOG)
102500000000     C                   MOVE      sogg002       DVASOGG
102600000000     C                   Clear                   DvaTpInd
102700000000     C                   Clear                   DvaCdInd
102800000000     C                   If        IndirR70      = '1'
102900000000     C                   If        APETpInd      <> *blanks and
103000000000     C                             APECdInd      <> *blanks
103100000000     C                   MOVE      APETpInd      DvaTpInd
103200000000     C                   MOVE      APECdInd      DvaCdInd
103300000000     C                   EndIf
103400000000     C                   EndIf
103500000000     C                   CALLB     'NDDVASOG'
103600000000     C                   PARM                    DVARIC
103700000000     C                   PARM                    DVASOCIETA
103800000000     C                   PARM                    DVAUNITA
103900000000     C                   PARM                    DVASTRUTT
104000000000     C                   PARM                    DVADTRIF
104100000000     C                   PARM                    DVALENOUT
104200000000     C                   PARM                    DVASNATURA
104300000000     C                   PARM                    *omit
104400000000     C                   PARM                    DVALINEAV
104500000000     C                   PARM                    DVAFILIALE
104600000000     C                   PARM                    DVASOGG
104700000000     C                   PARM                    DVATPIND
104800000000     C                   PARM                    DVACDIND
104900000000     C                   PARM                    DVAERRORE
105000000000     C                   PARM                    DVAOUTPUT
105100000000     C     DVAERRORE     IFEQ      *OFF
105200000000     C                   EVAL      %SUBST(DVASOG:1:DVALENOUT)
105300000000     C                              = %SUBST(DVAOUTPUT:1:DVALENOUT)
105400000000R235 C                   If        DVSCDFisc <> *blanks
105500000000  "  C                   Eval      $CDFIva = DVSCDFisc
105600000000  "  C                   Else
105700000000  "  C                   Eval      $CDFIva = DVSPartIVA
105800000000  "  C                   Endif
105900000000  "  C                   Eval      $NatGiur = DVSNatGiur
106000000000R235 C*
106100000000     C                   ENDIF
106200000000     C*
106300000000     C                   ENDSR
106400000000D0495C************************************************************
106500000000 "   C* DATI ANAGRAFICI VARIAZIONE ESTREMI FISCALI
106600000000 "   C************************************************************
106700000000 "   C     CALLDVACPI    BEGSR
106800000000 "   C*
106900000000 "   C                   move      '2'           DVCRIC
107000000000 "   C                   move      MRASocieta    DVCSOCIETA
107100000000 "   C                   move      *blank        DVCUNITA
107200000000 "   C                   movel     'ANCPI'       DVCSTRUTT
107300000000 "   C                   move      MRADDC        DVCDTRIF
107400000000 "   C                   eval      DVCLENOUT = %Size(ANCPI)
107500000000 "   C                   move      sogg002       DVCSOGG
107600000000 "   C*
107700000000 "   C                   CALLB     'NDDVACPI'    PDVACPI
107800000000 "   C*
107900000000 "   C                   ENDSR
108000000000 "   C/EJECT
108100000000D0495 ************************************************************
108200000000R235 C************************************************************
108300000000 "   C* DATI ANAGRAFICI VARIAZIONE ESTREMI FISCALI
108400000000 "   C************************************************************
108500000000 "   C     CallDVACPI2   BEGSR
108600000000 "   C*
108700000000 "   C                   Move      '2'           DVCRIC
108800000000 "   C                   Move      MOVSocieta    DVCSOCIETA
108900000000 "   C                   Move      *Blank        DVCUNITA
109000000000 "   C                   MoveL     'ANCPI'       DVCSTRUTT
109100000000 "   C                   Move      MOVDtDoc      DVCDTRIF
109200000000 "   C                   Eval      DVCLENOUT = %Size(ANCPI)
109300000000 "   C                   move      Sogg002       DVCSOGG
109400000000 "   C*
109500000000 "   C                   CALLB     'NDDVACPI'    PDVACPI
109600000000 "   C*
109700000000 "   C                   ENDSR
109800000000 "   C/EJECT
109900000000R235  ************************************************************
110000000000R235 C*----------------------------------------------------*
110100000000 "   C* Controllo se percipiente in regime agevolato       *
110200000000 "   C*----------------------------------------------------*
110300000000R235 C     CtrRegAgev    BEGSR
110400000000     C*
110500000000     C                   Eval      $RegAgev    = *Off
110600000000     C*
110700000000     C                   Eval      SPASocieta  = APESocieta
110800000000     C                   Eval      SPAKcc      = APEKcc
110900000000     C                   Eval      SPAKsc      = APEKsc
111000000000     C     K03SPA01      SETLL     SDGSPA01L
111100000000     C     K03SPA01      READE     SDGSPA01L                              21
111200000000     C                   Eval      $FineSPA    = *In21
111300000000     C*
111400000000     C                   DoW       $FineSPA    = *Off
111500000000     C*
111600000000     C* data documento compresa nel periodo in cui il percipiente
111700000000     C* è in regime agevolato
111800000000     C                   if        SPADtFin   >= MRADDc
111900000000     C                             and
112000000000     C                             SPADtIni   <= MRADDc
112100000000     C                   Eval      $RegAgev    = *On
112200000000     C                   LEAVE
112300000000     C                   endif
112400000000     C*
112500000000     C     K03SPA01      READE     SDGSPA01L                              21
112600000000     C                   Eval      $FineSPA    = *In21
112700000000     C                   ENDDO
112800000000     C*
112900000000R235 C                   ENDSR
113000000000R235 C*----------------------------------------------------*
113100000000 "   C* Caricamento importi per percipienti minimi         *
113200000000 "   C*----------------------------------------------------*
113300000000R235 C     RepRegAgev    BEGSR
113400000000     C*
113500000000     C                   Eval      $RegAgev    = *On
113600000000     C*
113700000000     C* Ciclo lettura percipienti in regime agevolato
113800000000     C     SocR70        SETLL     SDGSPA01L
113900000000     C     SocR70        READE     SDGSPA01L                              21
114000000000     C                   Eval      $FineSPA    = *In21
114100000000     C*
114200000000     C                   DoW       $FineSPA    = *Off
114300000000     C*
114400000000     C                   EXSR      RepDatiAna
114500000000     C*
114600000000     C* cerco le partite con fatture emesse
114700000000     C* dal percipiente nel periodo in cui era nel regime agevolato
114800000000     C* e per ogni partita cerco eventuali pagamenti da cui
114900000000     C* scorporare eventuale imposta IVA in proporzione
115000000000     C                   EXSR      RepPart
115100000000     C*
115200000000     C     RGPT(1)       IFNE      *BLANK                                       *  -6-
115300000000     C                   EXSR      SCRIVI
115400000000     C                   EXSR      INZDS
115500000000     C                   ENDIF
115600000000     C*
115700000000     C     SocR70        READE     SDGSPA01L                              21
115800000000     C                   Eval      $FineSPA    = *In21
115900000000     C                   ENDDO
116000000000     C*
116100000000R235 C                   ENDSR
116200000000R235 C*----------------------------------------------------*
116300000000 "   C* Reperimento dati anagrafici                        *
116400000000 "   C*----------------------------------------------------*
116500000000R235 C     RepDatiAna    BEGSR
116600000000     C*
116700000000     C                   Move      SPASocieta    APESocieta
116800000000     C                   Move      SPAKcc        APEKcc
116900000000     C                   Move      SPAKsc        APEKsc
117000000000     C     K03APE01      CHAIN     ANAPE01L                           21
117100000000     C                   if        *In21       = *Off
117200000000R235 C*
117300000000R235 C                   Eval      $SoggEst   = *on
117400000000     C* Reperisco dati conto
117500000000     C                   EXSR      READANPDC
117600000000     C     Esito         IfEq      *off
117700000000     C* Recupero dati soggetto
117800000000     C                   EXSR      CTRSOG
117900000000     C     DVAERRORE     IfEq      *ON
118000000000     C                   Clear                   DVASOG
118100000000     C                   EndIf
118200000000     C                   EndIf
118300000000     C                   endif
118400000000     C*
118500000000R235 C                   ENDSR
118600000000R235 C*----------------------------------------------------*
118700000000 "   C* Reperimento partite con fatture                    *
118800000000 "   C*----------------------------------------------------*
118900000000R235 C     RepPart       BEGSR
119000000000     C*
119100000000     C                   Move      *Off          $FinePart
119200000000     C                   Eval      $Fattura    = *Off
119300000000     C                   Eval      $NotaAccr   = *Off
119400000000     C                   Eval      $TotIVA     = 0
119500000000     C                   Eval      $TotFatt    = 0
119600000000     C                   Eval      $TotPag     = 0
119700000000     C*
119800000000     C                   Move      XScSoc        PASSocieta
119900000000     C                   Move      'CG'          PASCtb
120000000000     C                   Move      SPAKcc        PASKcc
120100000000     C                   Move      SPAKsc        PASKsc
120200000000     C     K04PAS02      SETLL     NDPAS02J
120300000000     C     K04PAS02      READE     NDPAS02J                               21
120400000000     C                   Eval      $FinePart   = *In21
120500000000     C*
120600000000     C                   if        $FinePart   = *Off
120700000000     C                   Eval      SavKcc      = PASKcc
120800000000     C                   Eval      SavKsc      = PASKsc
120900000000     C                   Eval      SavDtPar    = PASDtPar
121000000000     C                   Eval      SavNrPar    = PASNrPar
121100000000     C                   Eval      SavSerPar   = PASSerPar
121200000000     C                   endif
121300000000     C*
121400000000     C                   DoW       $FinePart   = *Off
121500000000     C*
121600000000     C* al cambio partita, eventualmente elaboro i dati della
121700000000     C* partita precedente
121800000000     C                   if        SavKcc     <> PASKcc   or
121900000000     C                             SavKsc     <> PASKsc   or
122000000000     C                             SavDtPar   <> PASDtPar or
122100000000     C                             SavNrPar   <> PASNrPar or
122200000000     C                             SavSerPar  <> PASSerPar
122300000000     C                   EXSR      ElabPart
122400000000     C                   Eval      SavKcc      = PASKcc
122500000000     C                   Eval      SavKsc      = PASKsc
122600000000     C                   Eval      SavDtPar    = PASDtPar
122700000000     C                   Eval      SavNrPar    = PASNrPar
122800000000     C                   Eval      SavSerPar   = PASSerPar
122900000000     C                   endif
123000000000     C*
123100000000     C* cerco la fattura e l'eventuale IVA
123200000000     C                   EXSR      RepFatt
123300000000     C*
123400000000R252 C                   if        $OkFatt     = *On
123500000000     C                   if        MOVDtDoc    < SPADtIni  or
123600000000     C                             MOVDtDoc    > SPADtFin
123700000000     C* salto tutti i movimemti di PAS relativi alla stessa partita
123800000000     C* questo perchè non può essere che nella stessa partita siano
123900000000     C* presenti documenti a regime agevolato e altri non a
124000000000     C* regime agevolato. Se il documento in esame non è a regime
124100000000     C* si assume che non ve ne siano altri nella partita
124200000000     C     K07PAS02      SETGT     NDPAS02J
124300000000     C*
124400000000     C                   else
124500000000R252 C* salto tutti i movimemti di PAS relativi alla stessa registrazione
124600000000 "   C     K10PAS02      SETGT     NDPAS02J
124700000000 "   C                   endif
124800000000 "   C*
124900000000R252 C                   endif
125000000000     C* cerco eventuali pagamenti
125100000000R252 C                   if        $OkFatt    <> *On
125200000000     C                   EXSR      RepPagam
125300000000     C                   endif
125400000000     C*
125500000000R252 X****               if        $OkFatt     = *On
125600000000 "   X**** salto tutti i movimemti di PAS relativi alla stessa registrazione
125700000000 "   X**** K10PAS02      SETGT     NDPAS02J
125800000000R252 X****               endif
125900000000     C*
126000000000     C* leggo la scadenza successiva
126100000000     C     K04PAS02      READE     NDPAS02J                               21
126200000000     C                   Eval      $FinePart   = *In21
126300000000     C                   ENDDO
126400000000     C*
126500000000     C* elaboro i dati dell'ultima partita esaminata
126600000000     C                   EXSR      ElabPart
126700000000     C*
126800000000R235 C                   ENDSR
126900000000R235 C*----------------------------------------------------*
127000000000 "   C* Selezione delle partite con fattura                *
127100000000 "   C*----------------------------------------------------*
127200000000R235 C     SelPart       BEGSR
127300000000     C*
127400000000     C                   Eval      $OkPart     = *On
127500000000     C*
127600000000     C                   Eval      $Causale    = MOVCausTes
127700000000     C                   EXSR      CallMVC003
127800000000     C                   if        $Esito      = *On
127900000000     C* la causale non esiste
128000000000     C                   Eval      $OkPart     = *Off
128100000000     C                   endif
128200000000     C*
128300000000     C*
128400000000     C                   if        $OkPart     = *On
128500000000     C*
128600000000     C                   Select
128700000000     C* la sottotipologia deve essere per Fornitori
128800000000     C                   When      OPESTP     <> 'F'
128900000000     C                   Eval      $OkPart     = *Off
129000000000     C*
129100000000     C* la tipologia deve essere per registrazioni IVA
129200000000     C                   When      OPETPC     <> '1'
129300000000     C                   Eval      $OkPart     = *Off
129400000000     C*
129500000000     C                   When      PASTpRegZ  <> 'D'
129600000000     C                   Eval      $OkPart     = *Off
129700000000     C*
129800000000     C                   endSl
129900000000     C*
130000000000     C                   endif
130100000000     C*
130200000000     C                   if        $OkPart     = *Off
130300000000     C* salto tutti i movimemti di PAS relativi alla stessa registrazione
130400000000     C     K10PAS02      SETGT     NDPAS02J
130500000000     C                   endif
130600000000     C
130700000000     C                   if        $OkPart     = *On
130800000000     C*
130900000000     C                   if        MOVDtDoc    < SPADtIni  or
131000000000     C                             MOVDtDoc    > SPADtFin
131100000000     C                   Eval      $OkPart     = *Off
131200000000     C* salto tutti i movimemti di PAS relativi alla stessa partita
131300000000     C* questo perchè non può essere che nella stessa partita siano
131400000000     C* presenti documenti a regime agevolato e altri non a
131500000000     C* regime agevolato. Se il documento in esame non è a regime
131600000000     C* si assume che non ve ne siano altri nella partita
131700000000     C     K07PAS02      SETGT     NDPAS02J
131800000000     C*
131900000000     C                   endif
132000000000     C*
132100000000     C                   endif
132200000000     C*
132300000000R235 C                   ENDSR
132400000000R235 C************************************************************
132500000000 "   C* Reperimento dati di causale operativa
132600000000 "   C************************************************************
132700000000R235 C     CallMVC003    BEGSR
132800000000     C*
132900000000     C                   Clear                   ANOPE
133000000000     C*
133100000000     C                   Move      PASDtReg      WData
133200000000     C                   Move      WData         WDtAlfa
133300000000     C*
133400000000     C* la causale deve essere valida alla data di registrazione
133500000000     C* passo quindi data di registrazione + un giorno
133600000000     C
133700000000     C*
133800000000     C                   Eval      $LenOut     = %Size(ANOPE)
133900000000     C                   CallB     'NDMVC003'
134000000000     C                   Parm                    XScSoc
134100000000     C                   Parm                    $Causale
134200000000     C                   Parm                    WDtAlfa
134300000000     C                   Parm      '0'           $Uso              1
134400000000     C                   Parm      *OFF          $GesMsg
134500000000     C                   Parm      *ON           $Esito
134600000000     C                   Parm      'ANOPE     '  $Struttura
134700000000     C                   Parm                    ANOPE
134800000000     C                   Parm                    $LenOut
134900000000     C*
135000000000R235 C                   ENDSR
135100000000R235 C/EJECT
135200000000R235 C*----------------------------------------------------*
135300000000 "   C* Reperimento dati anagrafici da variazioni fiscali  *
135400000000 "   C*----------------------------------------------------*
135500000000R235 C     RepVarFisc    BEGSR
135600000000     C*
135700000000     C                   Clear                   VarCPIva         20
135800000000     C                   EXSR      CallDVACPI2
135900000000     C                   if        DVCErrore   = *Off
136000000000     C                   MoveL     DVCOutput     AnCPI
136100000000     C                   if        CPICdFisc  <> *Blank
136200000000     C                   MoveL     CPICdFisc     VarCPIva
136300000000     C                   else
136400000000     C                   MoveL     CpIPartIva    VarCPIva
136500000000     C                   endif
136600000000     C*
136700000000R235 C                   Eval      $CDFIva = VarCPIva
136800000000R235 C                   EVAL      $NatGiur = CPINatGiur
136900000000     C                   endif
137000000000     C*
137100000000R235 C                   ENDSR
137200000000R235 C*----------------------------------------------------*
137300000000 "   C* Reperimento fatture                                *
137400000000 "   C*----------------------------------------------------*
137500000000R235 C     RepFatt       BEGSR
137600000000     C*
137700000000     C                   Move      *Off          $OkFatt
137800000000     C*
137900000000     C                   EXSR      SelFatt
138000000000     C*
138100000000     C                   if        $OkFatt     = *On
138200000000     C*
138300000000     C* Reperisco eventuali dati da variazioni fiscali
138400000000     C* alla data del documento (se ho più fatture, prendo
138500000000     C* i riferimenti relativi all'ultima fattura)
138600000000     C                   EXSR      RepVarFisc
138700000000     C*
138800000000     C                   Eval      $TotFatt    = $TotFatt + MOVImporto
138900000000     C*
139000000000R252 C* se non Autofattura acquisto,
139100000000R252 C                   if        OPEAFtAcq  <> '1'
139200000000     C                   EXSR      RepIVA
139300000000R252 C                   endif
139400000000     C*
139500000000     C                   endif
139600000000     C*
139700000000R235 C                   ENDSR
139800000000R235 C*----------------------------------------------------*
139900000000 "   C* Selezione delle fatture                            *
140000000000 "   C*----------------------------------------------------*
140100000000R235 C     SelFatt       BEGSR
140200000000     C*
140300000000     C                   Eval      $OkFatt     = *On
140400000000     C*
140500000000     C                   Eval      $Causale    = MOVCausTes
140600000000     C                   EXSR      CallMVC003
140700000000     C                   if        $Esito      = *On
140800000000     C* la causale non esiste
140900000000     C                   Eval      $OkFatt     = *Off
141000000000     C                   endif
141100000000     C*
141200000000     C*
141300000000     C                   if        $OkFatt     = *On
141400000000     C*
141500000000     C                   Select
141600000000R252 C* scarto le autofatture
141700000000 "   C                   When      OPETpDocI   = '8'
141800000000 "   C                   Eval      $OkFatt     = *Off
141900000000R252 C*
142000000000     C* scarto le note di accredito
142100000000     C                   When      PASDare     = 1
142200000000     C                   Eval      $OkFatt     = *Off
142300000000     C*
142400000000     C* la sottotipologia deve essere per Fornitori
142500000000     C                   When      OPESTP     <> 'F'
142600000000     C                   Eval      $OkFatt     = *Off
142700000000     C*
142800000000     C* la tipologia deve essere per registrazioni IVA
142900000000     C                   When      OPETPC     <> '1'
143000000000     C                   Eval      $OkFatt     = *Off
143100000000     C*
143200000000     C                   When      PASTpRegZ  <> 'D'
143300000000     C                   Eval      $OkFatt     = *Off
143400000000     C*
143500000000     C                   endSl
143600000000     C*
143700000000     C                   endif
143800000000     C*
143900000000     C*
144000000000     C                   if        $OkFatt     = *On
144100000000     C*
144200000000     C                   if        MOVDtDoc    < SPADtIni  or
144300000000     C                             MOVDtDoc    > SPADtFin
144400000000     C                   Eval      $OkFatt     = *Off
144500000000     C* salto tutti i movimemti di PAS relativi alla stessa partita
144600000000     C* questo perchè non può essere che nella stessa partita siano
144700000000     C* presenti documenti a regime agevolato e altri non a
144800000000     C* regime agevolato. Se il documento in esame non è a regime
144900000000     C* si assume che non ve ne siano altri nella partita
145000000000     C**** K07PAS02      SETGT     NDPAS02J
145100000000     C*
145200000000     C                   endif
145300000000     C*
145400000000     C                   endif
145500000000     C*
145600000000R235 C                   ENDSR
145700000000R235 C*----------------------------------------------------*
145800000000 "   C* Reperimento imposta                                *
145900000000 "   C*----------------------------------------------------*
146000000000R235 C     RepIVA        BEGSR
146100000000     C*
146200000000     C                   Eval      IVASys      = PASSys
146300000000     C                   Eval      IVANrAsReg  = PASNrAsReg
146400000000     C     K02IVA01      SETLL     NDIVA01L
146500000000     C     K02IVA01      READE     NDIVA01L                               21
146600000000     C                   Eval      $FineIVA    = *In21
146700000000     C*
146800000000     C                   DoW       $FineIVA    = *Off
146900000000     C*
147000000000     C                   Eval      $TotIVA     = $TotIVA +
147100000000     C                             (IVAImporto * (IVADare - IVAAvere))
147200000000     C*
147300000000     C     K02IVA01      READE     NDIVA01L                               21
147400000000     C                   Eval      $FineIVA    = *In21
147500000000     C                   ENDDO
147600000000     C*
147700000000R235 C                   ENDSR
147800000000R235 C*----------------------------------------------------*
147900000000 "   C* Reperimento pagamenti                              *
148000000000 "   C*----------------------------------------------------*
148100000000R235 C     RepPagam      BEGSR
148200000000     C*
148300000000     C                   Move      *Off          $OkPag
148400000000     C*
148500000000     C                   EXSR      SelPag
148600000000     C*
148700000000     C                   if        $OkPag      = *On
148800000000     C                   Eval      $TotPag     = $TotPag + (PASImporto *
148900000000     C                                           (PASDare - PASAvere)   )
149000000000     C                   endif
149100000000     C*
149200000000R235 C                   ENDSR
149300000000R235 C*----------------------------------------------------*
149400000000 "   C* Selezione dei pagamenti                            *
149500000000 "   C*----------------------------------------------------*
149600000000R235 C     SelPag        BEGSR
149700000000     C*
149800000000     C                   Eval      $OkPag      = *On
149900000000     C*
150000000000     C                   Reset                   XATBDS
150100000000     C                   Reset                   SDGHK1DS
150200000000     C*
150300000000     C                   MOVE      '1'           XTBRIC
150400000000     C                   MOVE      SocR70        XTBAZI
150500000000     C                   MOVE      *ZERO         XTBKEY
150600000000     C                   MOVE      'HK1'         XTBCOD
150700000000     C                   MOVE      PASCausRig    XTBKEY
150800000000     C*
150900000000     C                   CALLB     'XATB2'
151000000000     C                   PARM                    XATBDS
151100000000     C*
151200000000     C* causale non in tabella, scarto il movimento
151300000000     C                   if        XTBERR      = '1'
151400000000     C                   Eval      $OkPag      = *Off
151500000000     C                   endif
151600000000     C*
151700000000     C*
151800000000     C                   if        $OkPag      = *On
151900000000     C*
152000000000     C                   if        PASTpRegZ  <> 'D'
152100000000     C                   Eval      $OkPag      = *Off
152200000000     C                   endif
152300000000     C*
152400000000     C                   endif
152500000000     C*
152600000000     C                   if        $OkPag      = *On
152700000000     C* la data di registrazione del pagamento deve essere dell'anno
152800000000     C* di competenza della CU
152900000000     C                   Move(P)   PASDtReg      DataAlfa
153000000000     C                   Move      AAAlfa        WAnno             4 0
153100000000     C*
153200000000     C                   if        WAnno      <> AnnoR70
153300000000     C                   Eval      $OkPag      = *Off
153400000000     C                   endif
153500000000     C*
153600000000     C                   endif
153700000000     C*
153800000000R235 C                   ENDSR
153900000000R235 C*----------------------------------------------------*
154000000000 "   C* Reperisco i dati dal tributo                       *
154100000000 "   C*----------------------------------------------------*
154200000000R235 C     RepANTRI      BEGSR
154300000000     C*
154400000000     C                   Move      SPATributo    TRITributo
154500000000     C                   Move      SPATriProg    TRITriProg
154600000000     C     KEYTRI        CHAIN     ANTRI01L                           21
154700000000     C* se non l'ho trovato, imposto i valori di default per i percipienti
154800000000     C* in regime agevolato
154900000000     C                   if        *In21       = *On
155000000000     C                   Eval      TRIQua770   = 'AU'
155100000000     C                   Eval      TRIDes770   = 'R'
155200000000     C                   Eval      TRITriRagg  = *Blank
155300000000     C                   Eval      TRITributo  = *Blank
155400000000     C                   Eval      TRITriProg  = *Blank
155500000000     C                   endif
155600000000     C*
155700000000R235 C                   ENDSR
155800000000R235 C*----------------------------------------------------*
155900000000 "   C* Gestisco l'importo dell'IVA da decurtare dal pagam.*
156000000000 "   C*----------------------------------------------------*
156100000000R235 C     GestIVA       BEGSR
156200000000     C*
156300000000     C* calcolo l'IVA come fa la procedura dei bonifici, ovvero come
156400000000     C* l'imposta fosse in proporzione all'importo pagato, senza
156500000000     C* tenere conto del dettaglio della condizione di pagamento
156600000000     C                   Z-Add     $TotPag       WImpInp
156700000000     C                   CallP     X0201MultC(WImpInp: ($TotIVA / $TotFatt):
156800000000     C                                        1:
156900000000     C                                        'H': '0': NrDecMC: WImpOut)
157000000000     C                   Eval      $TotPag     = $TotPag - WImpOut
157100000000     C*
157200000000     C*
157300000000R235 C                   ENDSR
157400000000R235 C************************************************************
157500000000 "   C* Controllo stato
157600000000 "   C************************************************************
157700000000R235 C     CTRSTATO      BEGSR
157800000000     C*
157900000000     C*
158000000000     C                   Clear                   XATBDS
158100000000     C*
158200000000     C                   Eval      XTbRic      = '1'
158300000000     C                   Eval      XTbAzi      = SocR70
158400000000     C                   Eval      XTbKey      = *Zero
158500000000     C                   Eval      XTbCod      = 'G12'
158600000000     C                   Move      DVSSTATO      XTbkey
158700000000     C*
158800000000     C                   CALLB     'XATB2'
158900000000     C                   PARM                    XATBDS
159000000000     C*
159100000000     C                   if        XTbErr      = '0'
159200000000     C                   MoveL     XTbUni        TABG12
159300000000     C*
159400000000     C                   if        NazG12 = 'IT' and
159500000000     C                             CodUicG12 = 086
159600000000     C                   Movel     *Off          $SoggEst
159700000000     C                   Else
159800000000     C                   Movel     *On           $SoggEst
159900000000     C                   Endif
160000000000     C*
160100000000     C                   Else
160200000000     C                   Movel     *On           $SoggEst
160300000000     C*
160400000000     C                   Endif
160500000000     C*
160600000000R235 C                   ENDSR
160700000000R235 C/EJECT
160800000000R235 C************************************************************
160900000000  "  C* Controllo codice fiscale /partita iva, se italiani
161000000000  "  C************************************************************
161100000000R235 C     CTRFISCIVA    BEGSR
161200000000     C*
161300000000     C* soggetti e dell'anagrafica variazioni dati fiscali
161400000000     C*
161500000000 b2  C                   if        $cdfiva   <> *blank
161600000000     C*   e se la legislazione è italiana
161700000000     C                             and StaSKP = '1'
161800000000     C*
161900000000     C                   eval      XCodFisc = $Cdfiva
162000000000     C                   callb     'XCDFIS'
162100000000     C                   parm                    XCodFisc
162200000000     C                   parm                    Errfisc
162300000000     C*
162400000000     C* Se codice fiscale non valido
162500000000 b3  C                   if        Errfisc < 0
162600000000     C*
162700000000     C* Se TIPO natura giuridica "società di persone" o
162800000000     C* "società di capitale" si controlla il codice
162900000000     C* fiscale anche come partita IVA perchè in questi casi
163000000000     C* le due cose possono coincidere
163100000000     C                   MOVE      '1'           XTBRIC
163200000000     C                   MOVE      SocR70        XTBAZI
163300000000     C                   MOVE      *ZERO         XTBKEY
163400000000     C                   MOVE      'G16'         XTBCOD
163500000000     C                   MOVE      $NatGiur      XTBKEY
163600000000     C                   CALLB     'XATB2'
163700000000     C                   PARM                    XATBDS
163800000000     C     XTBERR        IFEQ      '0'
163900000000     C                   MOVEL     XTBUNI        TABG16
164000000000     C*
164100000000 b4  C                   if        TpNG16 = '2' OR TpNG16 = '3'
164200000000     C                   callb     'XPARIVA'
164300000000     C                   parm                    $Cdfiva
164400000000     C                   parm      *Zero         Errfisc
164500000000     C                   parm                    XScSoc
164600000000 e4  C                   endif
164700000000     C*
164800000000     C                   else
164900000000     C                   Movel     *On           $Soggest
165000000000     C                   ENDIF
165100000000     C*
165200000000     C* a questo punto se Esito < 0 significa che per i tipi
165300000000     C* natura giuridica '2' e '3' il codice fiscale è errato
165400000000     C* anche come partita IVA e che per il tipo natura
165500000000     C* giuridica '1' il codice fiscale è formalmente errato
165600000000 b4  C                   if        Errfisc < 0
165700000000     C* "Codice fiscale non valido"
165800000000     C                   Movel     *On           $Soggest
165900000000     C*
166000000000 x4  C                   else
166100000000     C                   Movel     *Off          $Soggest
166200000000 e4  C                   Endif
166300000000     C*
166400000000 x3  C                   else
166500000000     C                   Movel     *Off          $Soggest
166600000000 e3  C                   endif
166700000000     C*
166800000000 e2  C                   endif
166900000000     C*
167000000000R235 C                   Endsr
167100000000R235 C*
167200000000R235 C*----------------------------------------------------*
167300000000 "   C* Scrivo i dati di partita su DS multipla            *
167400000000 "   C*----------------------------------------------------*
167500000000R235 C     ElabPart      BEGSR
167600000000     C*
167700000000     C*
167800000000     C                   if        $TotFatt   <> 0        and
167900000000     C                             $TotPag    <> 0
168000000000     C*
168100000000     C* controlla il codice fiscale e la partita iva
168200000000     C                   Exsr      CTRStato
168300000000     C                   Exsr      CTRFiscIva
168400000000     C*
168500000000     C* Sono sicuramente percipienti esteri se
168600000000     C* nell'anagrafica percipiente è valorizzato l'identificativo
168700000000     C* fiscale estero e se lo stato è estero e il cod.fisc o la
168800000000     C* partita iva non sono italiani.
168900000000     C* Se estero, in CU non deve entrare
169000000000     C                   If        $SoggEst <> *on and
169100000000     C                             ApeCdFisc = *blanks
169200000000     C*
169300000000     C*
169400000000     C* calcolo l'importo del pagamento al netto dell'IVA
169500000000     C* Importo fattura : IVA fattura = Importo pagamento : IVA su pagamento
169600000000     C* ovvero:
169700000000     C* IVA su pagamento = (IVA fattura / Importo fattura ) * pagamento
169800000000     C                   if        $TotIVA    <> 0
169900000000     C                   EXSR      GestIVA
170000000000     C                   endif
170100000000     C*
170200000000     C                   EXSR      RepANTRI
170300000000     C*
170400000000     C                   EXSR      CARDS
170500000000     C*
170600000000     C                   Endif
170700000000     C*
170800000000     C*
170900000000     C                   Eval      $TotIVA     = 0
171000000000     C                   Eval      $TotFatt    = 0
171100000000     C                   Eval      $TotPag     = 0
171200000000     C*
171300000000     C                   endif
171400000000     C*
171500000000R235 C                   ENDSR
