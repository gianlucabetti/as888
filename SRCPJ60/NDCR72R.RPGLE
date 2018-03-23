000001180220     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000002180220     H*PARMS BNDDIR(PJXBND PJCBND)
000003180220     H*PARMS CVTOPT(*DATETIME)
000004180220     H DECEDIT('0,') DATEDIT(*DMY.)
000005180220     F*---------------------------------------------------------------*
000006180220     F*         ESTRAZIONE RIGHE PERCIPIENTE CON TOTALI PER:          *
000007180220     F*         - TRIBUTO/PROGRESSIVO TRIBUTO                         *
000008180220     F*          OPPURE                                               *
000009180220     F*         - CODICE DI RAGGRUPPAMENTO TRIBUTO                    *
000010180220     F*---------------------------------------------------------------*
000011180220     F*    INDICATORI                                                 *
000012180220     F*                                                               *
000013180220     F* 90 FINE FILE                                                  *
000014180220     F* 91 CHAIN ANTRI00F                                             *
000015180220     F* 50 LOOKUP SU TABELLA RAGGRUPPAMENTI TRIBUTI                   *
000016180220     F*    50 = OFF  INSERIMENTO NELLA TABELLA RAGGR. TRIBUTI         *
000017180220     F*    50 = ON   TOTALIZZAZIONE IMPORTI TABELLA RAGGR. TRIBUTI    *
000018180220     F* 51 LOOKUP CON *BLANK PER REPERIRE PRIMO LIBERO                *
000019180220     F* 60 INDICATORE DI OCCUR SU DS MULTIPLA RAGGRUPPAMENTI          *
000020180220     F*---------------------------------------------------------------*
000021180220     F*  --- FLUSSO PRINCIPALE ---                                    *
000022180220     F*                                                               *
000023180220     F*  - CANCELLAZIONE RECORD ESISTENTI PER L'ANNO DI COMPETENZA    *
000024180220     F*    SELEZIONATO NEL FILE ND77001L                              *
000025180220     F*                                                               *
000026180220     F*  - LETTURA ANAGRAFICO PERCIPIENTI                             *
000027180220     F*                                                               *
000028180220     F*  - LETTURA RITENUTE DEL PERCIPIENTE                           *
000029180220     F*                                                               *
000030180220     F*  - SELEZIONE DELLE RITENUTE CHE HANNO:                        *
000031180220     F*               1 - ANNO DI COMPETENZA SELEZIONATO A VIDEO      *
000032180220     F*               2 - STATO=3 (QUIETANZATI)                       *
000033180220     F*               3 - ALMENO UN IMPORTO DIVERSO DA 0              *
000034180220     F*               4 - QUADRO = SC - SE                            *
000035180220     F*                                                               *
000036180220     F*  - INSERIMENTO DEI MOVIMENTI ELABORATI IN UNA DS MULTIPLA     *
000037180220     F*    CHE LI RACCOGLIE PER TRIBUTO/PROGR. O COD. RAGGRUPPAMENTO  *
000038180220     F*                                                               *
000039180220     F*  - TOTALIZZAZIONE DEGLI IMPORTI NEL CASO IL MOVIMENTO APPAR-  *
000040180220     F*    TENGA AD UN TRIBUTO/PROGR. O COD. RAGGR. GIA' INSERITO     *
000041180220     F*                                                               *
000042180220     F*  - LETTURA NUOVO RECORD RITENUTE                              *
000043180220     F*                                                               *
000044180220     F*  - PER FINE MOVIMENTI DEL PERCIPIENTE MEMORIZZO DS MULTIPLA   *
000045180220     F*    NEL FILE ND77001L.                                         *
000046180220     F*                                                               *
000047180220     F*  - LETTURA NUOVO RECORD PERCIPIENTI                           *
000048180220     F*                                                               *
000049180220     F*  - FINE FILE PERCIPIENTI VAI A FINE                           *
000050180220R235 F*
000051180220R235 F* Modificata dicitura pgm,
000052180220R235 F*  da Estrazione righe percipienti mod.770
000053180220R235 F*   a Estrazione percipienti per CU  Batch
000054180220     F*---------------------------------------------------------------*
000055180220     FANAPE01L  IF   E           K DISK
000056180220R235 F*------------
000057180220 "   F* Storico percipienti in regime agevolato
000058180220 "   F* key: Soc/Kcc/Ksc/DtIni(desc)    UNIQUE
000059180220 "   FSDGSPA01L IF   E           K DISK
000060180220 "   F*------------
000061180220 "   F* NDPAS/NDMOV/NDREG, soc/ctb/kcc/ksc/dtp/n
000062180220 "   FNDPAS02J  IF   E           K DISK
000063180220 "   F*------------
000064180220 "   F* Movimenti IVA
000065180220 "   F* key: Sys/NrAsReg  UNIQUE
000066180220R235 FNDIVA01L  IF   E           K DISK
000067180220     F*------------
000068180220     FNDMRA01L  IF   E           K DISK
000069180220     F*------------
000070180220     FANTRI01L  IF   E           K DISK
000071180220     F*------------
000072180220     FND77001L  UF A E           K DISK
000073180220     F*---------------------------------------------------------------*
000074180220R235 D* Prototipazione procedure EURO
000075180220R235 D/COPY *LIBL/SRCCPY,PJX002PR
000076180220     D*------------
000077180220     D KPJBA         E DS
000078180220     D*-------------
000079180220     D ndcr70ds      E DS
000080180220     D*------------
000081180220     D* Richiamo a XATB
000082180220     D XATBDS        E DS
000083180220     D TABG16        E DS                  EXTNAME(ANGG16DS) INZ
000084180220     D*------------
000085180220C1788D* Tabella esclusione tributi 2%
000086180220C1788D TAB022        E DS                  EXTNAME(ANG022DS) INZ
000087180220R235 D*------------
000088180220 "   D* Tabella causali di pagamento percipienti in regime
000089180220 "   D* agevolato
000090180220 "   D SDGHK1DS      E DS                  INZ
000091180220 "   D*------------
000092180220 "   D* Tabella Stati
000093180220R235 D TABG12        E DS                  EXTNAME(ANGG12DS) INZ
000094180220     D*------------
000095180220     D* Definizioni parametri driver soggetti
000096180220     D DVARIC          S              1
000097180220     D DVASOCIETA      S              3
000098180220     D DVAUNITA        S              8
000099180220     D DVADTRIF        S               D
000100180220     D DVASTRUTT       S             10
000101180220     D DVALENOUT       S              9B 0
000102180220     D DVASNATURA      S              1
000103180220     D DVACODICE       S              8
000104180220     D DVALINEAV       S              3
000105180220     D DVAFILIALE      S              3  0
000106180220     D DVASOGG         S              8
000107180220     D DVATPIND        S              2
000108180220     D DVACDIND        S              3
000109180220     D DVAOUTPUT       S           4000
000110180220     D DVAERRORE       S              1
000111180220     D*------------
000112180220     D* DS esterna per dati soggetto
000113180220    >D DVASOG        E DS
000114180220     D*-------------
000115180220     D* DS esterna per decodifica conto
000116180220     D ND002DS       E DS
000117180220     D*-------------
000118180220     D* Definizioni parametri standard chiamate moduli
000119180220     D Esito           S              1
000120180220     D GesMsg          S              1
000121180220     D LenOut          S              9B 0
000122180220     D Struttura       S             10
000123180220     D Data            S             10
000124180220     D*------------
000125180220D0495D* Definizioni parametri driver estremi fiscali
000126180220  "  D dvcRic          S              1
000127180220  "  D dvcSocieta      S              3
000128180220  "  D dvcUnita        S              8
000129180220  "  D dvcStrutt       S             10
000130180220  "  D dvcDtRif        S               D
000131180220  "  D dvcLenOut       S              9B 0
000132180220  "  D dvcSogg         S              8
000133180220  "  D dvcErrore       S              1
000134180220  "  D dvcOutput       S           4000
000135180220  "  D*-------------
000136180220  "  D* Ds per output variazioni estremi fiscali
000137180220  "  D ANCPI         E DS                  EXTNAME(ANCPI00F)
000138180220D0495D*-------------
000139180220D0495D*** RGPT            S              6    DIM(50)                           SCHIERA RAGGRUPP
000140180220R235 X*ex D0495  RGPT  S             26    DIM(50)                              SCHIERA RAGGRUPP
000141180220R235 D* Aumento di 20 caratteri la schiera di raggruppamento per inserire
000142180220 "   D* il codice fiscale/partita IVA come indice di scrittura
000143180220 "   D* dell'elemento di schiera
000144180220R235 D RGPT            S             46    DIM(50)                              SCHIERA RAGGRUPP
000145180220     D $C              S              4    DIM(30)
000146180220R235 D*------------
000147180220R235 D KeyRGPT         S             46
000148180220     D*------------
000149180220     D* Parametro RABASE
000150180220     D ANP012DS      E DS                  INZ
000151180220     D XPAOUT          DS          2000
000152180220     D*------------
000153180220     D ND770           DS                  OCCURS(50)
000154180220     D  SOC                    1      3
000155180220     D  ACF                    4      6P 0
000156180220     D  QUA                    7      8
000157180220     D  KCC                    9     14
000158180220     D  KSC                   15     22
000159180220     D  RGT                   23     26
000160180220     D  CDF                   27     46
000161180220     D  COG                   47     94
000162180220     D  NOM                   95    142
000163180220     D  LNC                  143    176
000164180220     D  DNC                  177    186
000165180220     D  PNC                  187    188
000166180220     D  SES                  189    189
000167180220     D  CIT                  190    223
000168180220     D  VIA                  224    267
000169180220     D  NUM                  268    270
000170180220     D  PRV                  271    272
000171180220     D  C77                  273    292
000172180220     D  ITC                  293    302P 3
000173180220     D  IMN                  303    312P 3
000174180220     D  SNS                  313    322P 3
000175180220     D  IMS                  323    332P 3
000176180220     D  PRA                  333    335P 2
000177180220     D  RAC                  336    345P 3
000178180220     D  NET                  346    355P 3
000179180220     D  PFG                  356    356
000180180220     D  CSE                  357    391
000181180220     D  SER                  392    400
000182180220     D  CDE                  401    425
000183180220     D  PRG                  426    427
000184180220     D*------------
000185180220     D TBTTRI          DS
000186180220     D  TRITRIBUTO             1      4
000187180220     D  TRITRIPROG             5      6
000188180220     D*------------
000189180220     D NETTO           S                   like(N77NET)
000190180220     D IMT             S                   like(N77ITC)
000191180220     D*------------
000192180220     D DatMin          C                   CONST('0001-01-01')
000193180220     D*------------
000194180220R235 D* Dati di ambiente ottenuti da SDGSOCDR
000195180220 "   D SOC001        E DS                  EXTNAME(SDGSOCDS )
000196180220 "   D*  DS per reperire il valore di XScSKP nella posizione 30
000197180220 "   D  StaSKP                30     30
000198180220 "   D*-------------
000199180220 "   D* DS Interna per dati di output di SDGSOCDR
000200180220 "   D XSOCDS          DS          1000
000201180220 "   D*----------------
000202180220 "   D* numeri decimali della m.c.
000203180220 "   D NrDecMC         S              1  0
000204180220 "   D WNrDecMC        S              1
000205180220 "   D*------------
000206180220 "   D* data di lavoro per "costruzione" date
000207180220 "   D  DataAlfa       DS            10
000208180220 "   D  AAAlfa                 1      4
000209180220 "   D  MMAlfa                 6      7
000210180220 "   D  GGAlfa                 9     10
000211180220 "   D*----------------
000212180220 "   D $RegAgev        S              1
000213180220 "   D WData           S               D
000214180220 "   D WDtAlfa         S             10A
000215180220 "   D $Causale        S                   Like(MOVCausTes)
000216180220 "   D $FineSPA        S              1    Inz(*Off)
000217180220 "   D $FinePart       S              1    Inz(*Off)
000218180220 "   D $FineFatt       S              1    Inz(*Off)
000219180220 "   D $FinePag        S              1    Inz(*Off)
000220180220 "   D $FineIVA        S              1    Inz(*Off)
000221180220 "   D $Fattura        S              1    Inz(*Off)
000222180220 "   D $NotaAccr       S              1    Inz(*Off)
000223180220 "   D $OkPart         S              1    Inz(*On )
000224180220 "   D $OkFatt         S              1    Inz(*On )
000225180220 "   D $OkPag          S              1    Inz(*On )
000226180220 "   D $TotFatt        S                   Like(MOVImporto)
000227180220 "   D $TotIVA         S                   Like(IVAImporto)
000228180220 "   D $TotPag         S                   Like(PASImporto)
000229180220 "   D WImpInp         S                   Like(ImporD0202)
000230180220 "   D WImpOut         S                   Like(ImporD0202)
000231180220 "   D*---------
000232180220 "   D* Definizioni parametri standard chiamate moduli
000233180220 "   D $Esito          S              1
000234180220 "   D $GesMsg         S              1
000235180220 "   D $LenOut         S              9B 0
000236180220 "   D $Struttura      S             10
000237180220 "   D $CodOper        S              1
000238180220 "   D $Output         S           4000
000239180220 "   D $Refresh        S              1
000240180220 "   D $Uso            S              1
000241180220 "   D*-------------
000242180220 "   D* Ds per reperimento dati su causali operative contabili
000243180220 "   D ANOPE         E DS                  ExtName(ANOPE00F) Inz
000244180220 "   D*-------------
000245180220 "   D SavKcc          S                   Like(PASKcc)
000246180220 "   D SavKsc          S                   Like(PASKsc)
000247180220 "   D SavDtPar        S                   Like(PASDtPar)
000248180220 "   D SavNrPar        S                   Like(PASNrPar)
000249180220 "   D SavSerPar       S                   Like(PASSerPar)
000250180220 "   D*-------------
000251180220 "   D $SOGGEST        S              1
000252180220 "   D $CDFIVA         S             20A
000253180220 "   D $NATGIUR        S                   like(CPINatgiur)
000254180220 "   D XCodFisc        S                   like($CDFIva)
000255180220R235 D ErrFisc         S              1  0
000256180220     C*---------------------------------------------------------------*
000257180220     C*
000258180220     C                   EXSR      INZVAR                                       *
000259180220     C*
000260180220     C* Cancellazione eventuali record per l'anno selezionato
000261180220     C                   EXSR      DEL770                                       *
000262180220     C*
000263180220R235 C                   Eval      $RegAgev    = *Off
000264180220R235 C*
000265180220     C* Ciclo lettura percipienti
000266180220     C     socR70        SETLL     ANAPE000                                     *
000267180220     C*
000268180220     C                   DO        *HIVAL                                       *  -1-
000269180220     C     socR70        READE     ANAPE000                               90    *
000270180220R235 X***90              GOTO      FINE                                         *
000271180220R235 C   90              GOTO      PERCMIN                                      *
000272180220 "   C*
000273180220R235 C                   Eval      $SoggEst   = *on
000274180220     C*
000275180220D0495C* Reperisco dati conto
000276180220  "  C                   EXSR      READANPDC
000277180220  "  C     Esito         IfEq      *off
000278180220  "  C* Recupero dati soggetto
000279180220  "  C                   EXSR      CTRSOG
000280180220  "  C     DVAERRORE     IfEq      *ON
000281180220  "  C                   Clear                   DVASOG
000282180220  "  C                   EndIf
000283180220  "  C                   EndIf
000284180220D0495C*
000285180220     C                   move      apekcc        mrakcc
000286180220     C                   move      apeksc        mraksc
000287180220     C     KEYMRA        SETLL     NDMRA01L                                     *
000288180220     C*
000289180220     C                   DO        *HIVAL                                       *   -2-
000290180220     C     KEYMRA        READE     NDMRA01L                               90    *
000291180220     C*
000292180220     C  N90MRAACF        IFEQ      annoR70                                      *  -3-
000293180220     C     MRASTA        ANDEQ     '3'                                          *
000294180220     C*
000295180220     C     MRAIMN        IFEQ      0                                            *
000296180220     C     MRASNS        ANDEQ     0                                            *
000297180220     C     MRAIMS        ANDEQ     0                                            *
000298180220     C     MRARAC        ANDEQ     0                                            *
000299180220     C                   GOTO      LAB100                                       *
000300180220     C                   ENDIF                                                  *  -4-
000301180220R235 C*
000302180220 "   C* controllo se il percipiente è nel periodo agevolato alla data
000303180220 "   C* del documento, se lo è salto il movimento perchè altrimenti
000304180220 "   C* lo estrarrei 2 volte
000305180220 "   C                   EXSR      CtrRegAgev
000306180220 "   C                   if        $RegAgev    = *On
000307180220 "   C                   GOTO      LAB100
000308180220R235 C                   endif
000309180220     C*
000310180220D0495C* Reperisco eventuali dati variazioni fisali
000311180220  "  C                   Clear                   VarCPIva         20
000312180220  "  C                   Exsr      CallDVACPI
000313180220  "  C     DVCErrore     IfEq      *off
000314180220  "  C                   Movel     DVCOutput     AnCPI
000315180220  "  C     CPICdFisc     IFNE      *BLANK
000316180220  "  C                   MOVEL     CPICdFisc     VarCPIva
000317180220  "  C                   ELSE
000318180220  "  C                   MOVEL     CpIPartIva    VarCPIva
000319180220D0495C                   ENDIF
000320180220R235 C*
000321180220 "   C                   Eval      $CDFIva = VarCPIva
000322180220 "   C                   EVAL      $NatGiur = CPINatGiur
000323180220R235 C*
000324180220D0495C                   EndIf
000325180220D0495C*
000326180220R235 C* e controlla il codice fiscale e la partita iva
000327180220  "  C                   Exsr      CTRStato
000328180220  "  C                   Exsr      CTRFiscIva
000329180220  "  C*
000330180220  "  C* Sono sicuramente percipienti esteri se
000331180220  "  C* nell'anagrafica percipiente è valorizzato l'identificativo
000332180220  "  C* fiscale estero e se lo stato è estero e il cod.fisc o la
000333180220  "  C* partita iva non sono italiani.
000334180220  "  C* Se estero, in CU non deve entrare
000335180220  "  C                   If        $SoggEst <> *on and
000336180220  "  C                             ApeCdFisc = *blanks
000337180220R235 C*
000338180220     C                   MOVE      MRATRI        TRITRIBUTO
000339180220     C                   MOVE      MRAPTR        TRITRIPROG
000340180220     C     KEYTRI        CHAIN     ANTRI01L                           91
000341180220     C   91              CLEAR                   ANTRI000
000342180220     C*
000343180220     C  N91TRIQUA770     IFEQ      'AU'
000344180220     C                   EXSR      CARDS
000345180220     C                   ENDIF
000346180220     C*
000347180220R235 C                   Endif
000348180220R235 C*
000349180220     C     LAB100        TAG
000350180220     C                   ENDIF                                                  *  -3-
000351180220     C  N90              ENDDO                                                  *  -2-
000352180220     C*
000353180220     C     RGPT(1)       IFNE      *BLANK                                       *  -6-
000354180220     C                   EXSR      SCRIVI                                       *
000355180220     C                   EXSR      INZDS                                        *
000356180220     C                   ENDIF                                                  *  -6-
000357180220     C*
000358180220     C                   ENDDO                                                  *  -1-
000359180220R235 C*
000360180220 "   C*
000361180220 "   C     PERCMIN       TAG
000362180220 "   C*
000363180220 "   C* Reperisco gli importi relativi ai percipienti in regime
000364180220 "   C* agevolato
000365180220R235 C                   EXSR      RepRegAgev
000366180220     C*
000367180220     C     FINE          TAG                                                    *
000368180220     C                   SETON                                        LR        *
000369180220     C*
000370180220     C*----------------------------------------------------*
000371180220     C* SUBROUTINE INIZIALE                                *
000372180220     C*----------------------------------------------------*
000373180220     C     INZVAR        BEGSR                                                  *
000374180220     C*
000375180220     C     *ENTRY        PLIST                                                  *
000376180220     C                   PARM                    KPJBA                          *
000377180220     C*
000378180220     C                   movel     kpjbu         ndcr70ds
000379180220     C*
000380180220     C     KEY770        KLIST                                                  *
000381180220     C                   KFLD                    socR70
000382180220     C                   KFLD                    N77ACF
000383180220     C*
000384180220     C     KEYMRA        KLIST                                                  *
000385180220     C                   KFLD                    socR70
000386180220     C                   KFLD                    MRAKCC
000387180220     C                   KFLD                    MRAKSC
000388180220     C*
000389180220     C     KEYTRI        KLIST
000390180220     C                   KFLD                    socR70
000391180220     C                   KFLD                    TRITRIBUTO
000392180220     C                   KFLD                    TRITRIPROG
000393180220R235 C*
000394180220 "   C* SDGSPA01L
000395180220 "   C     K03SPA01      KLIST
000396180220 "   C                   KFLD                    SPASocieta
000397180220 "   C                   KFLD                    SPAKcc
000398180220 "   C                   KFLD                    SPAKsc
000399180220 "   C*
000400180220 "   C* NDIVA01L
000401180220 "   C     K02IVA01      KLIST
000402180220 "   C                   KFLD                    IVASys
000403180220 "   C                   KFLD                    IVANrAsReg
000404180220 "   C*
000405180220 "   C* ANAPE01L
000406180220 "   C     K03APE01      KLIST
000407180220 "   C                   KFLD                    APESocieta
000408180220 "   C                   KFLD                    APEKcc
000409180220 "   C                   KFLD                    APEKsc
000410180220 "   C*
000411180220 "   C* NDPAS02J
000412180220 "   C     K04PAS02      KLIST
000413180220 "   C                   KFLD                    PASSocieta
000414180220 "   C                   KFLD                    PASCtb
000415180220 "   C                   KFLD                    PASKcc
000416180220 "   C                   KFLD                    PASKsc
000417180220 "   C*
000418180220 "   C     K07PAS02      KLIST
000419180220 "   C                   KFLD                    PASSocieta
000420180220 "   C                   KFLD                    PASCtb
000421180220 "   C                   KFLD                    PASKcc
000422180220 "   C                   KFLD                    PASKsc
000423180220 "   C                   KFLD                    PASDtPar
000424180220 "   C                   KFLD                    PASNrPar
000425180220 "   C                   KFLD                    PASSerPar
000426180220 "   C*
000427180220 "   C     K10PAS02      KLIST
000428180220 "   C                   KFLD                    PASSocieta
000429180220 "   C                   KFLD                    PASCtb
000430180220 "   C                   KFLD                    PASKcc
000431180220 "   C                   KFLD                    PASKsc
000432180220 "   C                   KFLD                    PASDtPar
000433180220 "   C                   KFLD                    PASNrPar
000434180220 "   C                   KFLD                    PASSerPar
000435180220 "   C                   KFLD                    PASDtReg
000436180220 "   C                   KFLD                    PASSys
000437180220 "   C                   KFLD                    PASNrAsReg
000438180220 "   C*
000439180220 "   C     K11PAS02      KLIST
000440180220 "   C                   KFLD                    PASSocieta
000441180220 "   C                   KFLD                    PASCtb
000442180220 "   C                   KFLD                    PASKcc
000443180220 "   C                   KFLD                    PASKsc
000444180220 "   C                   KFLD                    PASDtPar
000445180220 "   C                   KFLD                    PASNrPar
000446180220 "   C                   KFLD                    PASSerPar
000447180220 "   C                   KFLD                    PASDtReg
000448180220 "   C                   KFLD                    PASSys
000449180220 "   C                   KFLD                    PASNrAsReg
000450180220R235 C                   KFLD                    PASNrRigaM
000451180220D0495C*
000452180220  "  C     PDVACPI       PLIST
000453180220  "  C                   PARM                    DVCRIC
000454180220  "  C                   PARM                    DVCSOCIETA
000455180220  "  C                   PARM                    DVCUNITA
000456180220  "  C                   PARM                    DVCSTRUTT
000457180220  "  C                   PARM                    DVCDTRIF
000458180220  "  C                   PARM                    DVCLENOUT
000459180220  "  C                   PARM                    DVCSOGG
000460180220  "  C                   PARM                    DVCERRORE
000461180220D0495C                   PARM                    DVCOUTPUT
000462180220     C*
000463180220     C* Reperimento parametro RABASE
000464180220     C                   CallB     'XPAR'
000465180220     C                   Parm                    SocR70
000466180220     C                   Parm      'RABASE  '    XPaPar            8
000467180220     C                   Parm                    XPaOut
000468180220     C                   Parm      *Off          XPaErr            1
000469180220     C                   Parm      ' '           XPaRic            1
000470180220     C     XPaErr        IfEq      *On
000471180220     C                   Goto      Fine
000472180220     C                   Else
000473180220     C                   MoveL     XPaOut        ANP012DS
000474180220     C                   EndIf
000475180220R235 C*
000476180220 "   C                   EXSR      REPSOC
000477180220 "   C*
000478180220 "   C* reperisco dai dati di società i numeri dei decimali della moneta di
000479180220 "   C* conto
000480180220 "   C                   Eval      WNrDecMC   =  %SubSt(XScSKp:6:1)
000481180220R235 C                   Move      WNrDecMC      NrDecMC
000482180220     C*
000483180220     C* Inizializzo DS multipla
000484180220     C                   EXSR      INZDS
000485180220     C*
000486180220     C                   ENDSR                                                  *
000487180220     C*
000488180220     C*----------------------------------------------------*
000489180220     C* SUBROUTINE CANCELLAZIONE RECORD X ANNO COMPETENZA  *
000490180220     C*----------------------------------------------------*
000491180220     C     DEL770        BEGSR                                                  *
000492180220     C*
000493180220     C                   z-add     annoR70       N77ACF
000494180220     C     KEY770        SETLL     ND77001L                                     *
000495180220     C                   DO        *HIVAL                                       *
000496180220     C     KEY770        READE     ND77001L                               90    *
000497180220     C   90              LEAVE                                                  *
000498180220     C                   DELETE    ND770000                                     *
000499180220     C                   ENDDO                                                  *
000500180220     C*
000501180220     C                   ENDSR                                                  *
000502180220R235 C************************************************************
000503180220 "   C* Reperimento dati società
000504180220 "   C************************************************************
000505180220R235 C     REPSOC        BEGSR
000506180220     C*
000507180220     C                   CALL      'SDGSOCDR'
000508180220     C                   PARM      'SOC001'      TIPXSC            6
000509180220     C                   PARM      SocR70        SOCXSC            3
000510180220     C                   PARM                    CDSXSC            9 0
000511180220     C                   PARM                    MODXSC            3
000512180220     C                   PARM      *BLANKS       RTNXSC            1
000513180220     C                   PARM                    XSOCDS
000514180220     C                   PARM                    KPJBA
000515180220     C*
000516180220     C                   if        RtnXsc     <> '1'
000517180220     C                   MoveL     XSOCDS        SOC001
000518180220     C                   else
000519180220     C                   GOTO      FINE
000520180220     C                   endif
000521180220     C*
000522180220R235 C                   ENDSR
000523180220R235 C/EJECT
000524180220     C*----------------------------------------------------*
000525180220     C*   INIZIALIZZAZIONE DS MULTIPLA                     *
000526180220     C*----------------------------------------------------*
000527180220     C     INZDS         BEGSR                                                  *
000528180220     C*
000529180220     C                   MOVEA     *BLANK        RGPT                           *
000530180220     C*                                                                         *
000531180220     C                   DO        50            X                 2 0          *
000532180220     C     X             OCCUR     ND770                                        *
000533180220     C                   MOVE      *BLANK        SOC
000534180220     C                   Z-ADD     0             ACF
000535180220     C                   MOVE      *BLANK        QUA
000536180220     C                   MOVEL     0             KCC
000537180220     C                   MOVEL     0             KSC
000538180220     C                   MOVE      *BLANK        RGT
000539180220     C                   MOVE      *BLANK        CDF
000540180220     C                   MOVE      *BLANK        COG
000541180220     C                   MOVE      *BLANK        NOM
000542180220     C                   MOVE      *BLANK        LNC
000543180220     C                   MOVE      *BLANK        DNC
000544180220     C                   MOVE      *BLANK        PNC
000545180220     C                   MOVE      *BLANK        SES
000546180220     C                   MOVE      *BLANK        CIT
000547180220     C                   MOVE      *BLANK        VIA
000548180220     C                   MOVE      *BLANK        NUM
000549180220     C                   MOVE      *BLANK        PRV
000550180220     C                   MOVE      *BLANK        C77
000551180220     C                   MOVE      *BLANK        CSE
000552180220     C                   MOVE      *BLANK        SER
000553180220     C                   MOVE      *BLANK        CDE
000554180220     C                   MOVE      *BLANK        PRG
000555180220     C                   Z-ADD     0             ITC
000556180220     C                   Z-ADD     0             IMN
000557180220     C                   Z-ADD     0             SNS
000558180220     C                   Z-ADD     0             IMS
000559180220     C                   Z-ADD     0             PRA
000560180220     C                   Z-ADD     0             RAC
000561180220     C                   Z-ADD     0             NET
000562180220     C                   MOVE      *BLANK        PFG
000563180220     C                   ENDDO
000564180220     C*
000565180220     C                   ENDSR                                                  *
000566180220     C*----------------------------------------------------*
000567180220     C* SUBROUTINE CARICAMENTO DS MULTIPLA                 *
000568180220     C*----------------------------------------------------*
000569180220     C     CARDS         BEGSR                                                  *
000570180220     C*
000571180220     C                   Z-ADD     1             X                              *
000572180220     C*
000573180220     C* Reperimento tributo su schiera tributi
000574180220     C*
000575180220     C                   If        RaggRGTR70 = *On
000576180220     C* codice raggr. tributo
000577180220     C                   Move      *Blank        RACTRI
000578180220R186 X***                MoveL     TRITriRagg    RACTRI
000579180220R186 C                   Eval      RacTri = TRITriRagg +
000580180220R186 C                                      %Subst(TRIDES770: 1:2)
000581180220     C                   Else
000582180220     C* tributo/progressivo
000583180220     C                   MOVEL     TBTTRI        RACTRI            6
000584180220     C                   EndIf
000585180220R235 X*ex D0495          MOVEL     VarCPIva      KeyRGPT          26
000586180220R235 X*ex D0495          MOVE      RacTri        KeyRGPT
000587180220R235 C*
000588180220 "   C     $CdFIVA       CAT       VarCPIva:0    KeyRGPT
000589180220 "   C                   CAT       RacTri:0      KeyRGPT
000590180220 "   C*
000591180220 "   C                   Eval      KeyRGPT     = $CdFIVA + VarCPIva +
000592180220R235 C                                           RacTri
000593180220D0495C***  RACTRI        LOOKUP    RGPT(X)                                50    *
000594180220D0495C     KeyRGPT       LOOKUP    RGPT(X)                                50    *
000595180220     C*
000596180220     C* Inserimento tributo su schiera tributi
000597180220     C     *in50         ifeq      *off                                         *
000598180220     C                   Z-ADD     1             J                 3 0          *
000599180220     C     *BLANK        LOOKUP    RGPT(J)                                51    *
000600180220D0495C***51              MOVE      RACTRI        RGPT(J)                        *
000601180220D0495C   51              MOVE      KeyRGPT       RGPT(J)                        *
000602180220     C   51              Z-ADD     J             X                              *
000603180220     C                   endif                                                  *
000604180220     C*
000605180220     C* Inserimento o aggiornamento importi DS multipla
000606180220     C     X             OCCUR     ND770                                60      *
000607180220     C   60              GOTO      ENDSU1                                       *
000608180220R235 C                   if        $RegAgev    = *On
000609180220 "   C                   Z-ADD     $TotPag       NETTO
000610180220R235 C                   else
000611180220     C     MRAIMS        SUB       MRARAC        NETTO                          *
000612180220R235 C                   endif
000613180220     C*
000614180220D0495C***  *in50         ifeq      *off
000615180220 "   C* Reperisco dati conto
000616180220 "   C***                EXSR      READANPDC
000617180220 "   C***  Esito         ifeq      *off
000618180220 "   C* Recupero dati soggetto
000619180220 "   C***                EXSR      CTRSOG
000620180220 "   C***  DVAERRORE     IFEQ      *ON
000621180220 "   C***                CLEAR                   DVASOG
000622180220 "   C***                ENDIF
000623180220 "   C***                endif
000624180220D0495C***                endif
000625180220     C*
000626180220     C* Scrittura o aggiornamento record DS multipla
000627180220     C*
000628180220     C     *in50         ifeq      *off
000629180220     C*
000630180220R235 C                   if        $RegAgev    = *On
000631180220 "   C*
000632180220 "   C                   MOVE      SPASocieta    SOC
000633180220 "   C                   Z-ADD     AnnoR70       ACF
000634180220 "   C                   MOVE      TRIQua770     QUA
000635180220 "   C                   MOVEL     SPAKCC        KCC
000636180220 "   C                   MOVEL     SPAKSC        KSC
000637180220 "   C*
000638180220R235 C                   else
000639180220     C                   MOVE      MRASOCIETA    SOC
000640180220     C                   Z-ADD     MRAACF        ACF
000641180220     C                   MOVE      TRIQUA770     QUA
000642180220     C                   MOVEL     MRAKCC        KCC
000643180220     C                   MOVEL     MRAKSC        KSC
000644180220R235 C                   endif
000645180220R235 C*
000646180220     C                   If        RaggRGTR70 = *On
000647180220     C* codice raggr. tributo
000648180220     C                   Move      TRITriRagg    RGT
000649180220     C                   Move      *Blank        PRG
000650180220     C                   Else
000651180220     C* tributo/progressivo
000652180220     C                   MOVEL     TRITRIBUTO    RGT
000653180220     C                   MOVEL     TRITRIPROG    PRG
000654180220     C                   EndIf
000655180220D0495C     VarCPIva      IFNE      *BLANK
000656180220D0495C                   Movel     VarCpIva      CDF
000657180220D0495C                   Else
000658180220     C     DVSCDFISC     IFNE      *BLANK
000659180220     C                   MOVEL     DVSCDFISC     CDF
000660180220     C                   ELSE
000661180220     C                   MOVEL     DVSPARTIVA    CDF
000662180220     C                   EndIf
000663180220D0495C                   EndIf
000664180220     C                   MOVE      '1'           XTBRIC
000665180220     C                   MOVE      socR70        XTBAZI
000666180220     C                   MOVE      *ZERO         XTBKEY
000667180220     C                   MOVE      'G16'         XTBCOD
000668180220     C                   MOVE      DVSNATGIUR    XTBKEY
000669180220     C                   CALLb     'XATB'
000670180220     C                   PARM                    XATBDS
000671180220     C     XTBERR        IFEQ      '0'
000672180220     C                   MOVEL     XTBUNI        TABG16
000673180220     C                   ENDIF
000674180220     C                   if        TPNG16 = '1'
000675180220     C                   MOVEL     APECOGNOME    COG
000676180220     C                   MOVEL     APENOME       NOM
000677180220     C                   MOVEL     APELOCALIT    LNC
000678180220     C                   MOVEL     DVSDTNASC     DNC
000679180220     C                   MOVEL     APEPROV       PNC
000680180220     C                   IF        DVSSEX = '1'
000681180220     C                   MOVEL     'M'           SES
000682180220     C                   ENDIF
000683180220     C                   IF        DVSSEX = '2'
000684180220     C                   MOVEL     'F'           SES
000685180220     C                   ENDIF
000686180220     C                   MOVEL     'F'           PFG
000687180220     C                   Else
000688180220     C                   MOVEL     DVSDES        COG
000689180220     C                   MOVEL     *BLANK        NOM
000690180220     C                   MOVEL     *BLANK        LNC
000691180220     C                   MOVEL     DatMin        DNC
000692180220     C                   MOVEL     *BLANK        PNC
000693180220     C                   MOVEL     *BLANK        SES
000694180220     C                   MOVEL     'G'           PFG
000695180220     C                   Endif
000696180220     C                   MOVEL     DVSLOCALIT    CIT
000697180220     C                   MOVEL     DVSINDRIZ     VIA
000698180220     C                   MOVEL     DVSPROV       PRV
000699180220     C                   MOVEL     TRIDES770     C77
000700180220     C*
000701180220     C                   MOVEL     APECDFISC     CDE
000702180220     C     CDE           IFNE      *BLANK
000703180220     C                   MOVEL     *BLANK        CDF
000704180220     C                   ENDIF
000705180220     C                   MOVEL     APECDESMIN    CSE
000706180220     C     CSE           IFNE      *BLANK
000707180220     C* Qui ci andrebbe la descrizione dello stato estero
000708180220     C* (ex tab.70 di Orion), ma per ora non ci metto niente !!!
000709180220     C                   MOVEL     *BLANK        SER
000710180220R2527C                   MOVEL     *BLANK        CSE
000711180220     C                   ENDIF
000712180220C1788C*
000713180220  "  C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
000714180220  "  C* se il tributo in elaborazione è quello da escludere; in caso
000715180220  "  C* affermativo lo elimino dalla/e caselle indicate
000716180220  "  x*** ex c1667       SetOn                                        3839
000717180220  "  x***      "         If        EscItcR70 = *On And MraTri = EscTriR70
000718180220  "  x***      "                   And MraPtr = EscPrgR70
000719180220  "  x***      "         SetOff                                       38
000720180220  "  x***      "         EndIf
000721180220  "  x***      "         If        EscImnR70 = *On And MraTri = EscTriR70
000722180220  "  x***      "                   And MraPtr = EscPrgR70
000723180220  "  x***      "         SetOff                                       39
000724180220  "  x***      "         EndIf
000725180220  "  C*
000726180220  "  C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
000727180220  "  C* se il tributo in elaborazione è quello da escludere; in caso
000728180220  "  C* affermativo lo elimino dalla/e caselle indicate
000729180220  "  C                   SetOn                                        3839
000730180220  "  C                   If        EscItcR70 = *On or
000731180220  "  C                             EscImnR70 = *On
000732180220  "  c* vedo se esiste nella tabella 022
000733180220  "  C                   MOVE      '1'           XTBRIC
000734180220  "  C                   MOVE      socR70        XTBAZI
000735180220  "  C                   MOVE      *ZERO         XTBKEY
000736180220C1788C                   MOVE      '022'         XTBCOD
000737180220R235 C                   if        $RegAgev    = *On
000738180220  "  C                   MOVEl     SPATributo    wrk6              6
000739180220  "  C                   MOVE      SPATriProg    wrk6
000740180220R235 C                   else
000741180220C1788C                   MOVEl     MraTri        wrk6              6
000742180220C1788C                   MOVE      MraPtr        wrk6
000743180220R235 C                   endif
000744180220C1788C                   MOVE      wrk6          XTBKEY
000745180220  "  C                   CALLb     'XATB'
000746180220  "  C                   PARM                    XATBDS
000747180220  "  C     XTBERR        IFEQ      '0'
000748180220  "  C                   If        EscItcR70 = *On
000749180220  "  C                   SetOff                                       38
000750180220  "  C                   EndIf
000751180220  "  c*
000752180220  "  C                   If        EscImnR70 = *On
000753180220  "  C                   SetOff                                       39
000754180220  "  C                   EndIf
000755180220  "  c*
000756180220  "  C                   EndIf
000757180220C1788c                   EndIf
000758180220     C*
000759180220R235 C                   if        $RegAgev    = *On
000760180220 "   C   38              Z-ADD     $TotPag       ITC
000761180220 "   C   39              Z-ADD     $TotPag       IMN
000762180220 "   C                   Z-ADD     0             SNS
000763180220 "   C                   Z-ADD     0             IMS
000764180220 "   C                   Z-ADD     0             PRA
000765180220 "   C                   Z-ADD     0             RAC
000766180220R235 C                   else
000767180220     C     MRAIMS        ADD       MRASNS        ITC
000768180220C1667C   38              ADD       MRAIMN        ITC
000769180220  "  C  N39              Z-ADD     0             IMN
000770180220C1667C   39              Z-ADD     MRAIMN        IMN
000771180220     C                   Z-ADD     MRASNS        SNS
000772180220     C                   Z-ADD     MRAIMS        IMS
000773180220     C                   Z-ADD     MRAPRA        PRA
000774180220     C                   Z-ADD     MRARAC        RAC
000775180220R235 C                   endif
000776180220     C                   Z-ADD     NETTO         NET
000777180220     C*
000778180220     C                   else
000779180220C1788x*
000780180220  "  x* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
000781180220  "  x* se il tributo in elaborazione è quello da escludere; in caso
000782180220  "  x* affermativo lo elimino dalla/e caselle indicate
000783180220  "  x*** ex c1667       SetOn                                        3839
000784180220  "  x***      "         If        EscItcR70 = *On And MraTri = EscTriR70
000785180220  "  x***      "                   And MraPtr = EscPrgR70
000786180220  "  x***      "         SetOff                                       38
000787180220  "  x***      "         EndIf
000788180220  "  x***      "         If        EscImnR70 = *On And MraTri = EscTriR70
000789180220  "  x***      "                   And MraPtr = EscPrgR70
000790180220  "  x***      "         SetOff                                       39
000791180220  "  x***      "         EndIf
000792180220  "  C*
000793180220  "  C* Se nel lancio è stata richiesta l'esclusione del 2%, verifico
000794180220  "  C* se il tributo in elaborazione è quello da escludere; in caso
000795180220  "  C* affermativo lo elimino dalla/e caselle indicate
000796180220  "  C                   SetOn                                        3839
000797180220  "  C                   If        EscItcR70 = *On or
000798180220  "  C                             EscImnR70 = *On
000799180220  "  c* vedo se esiste nella tabella 022
000800180220  "  C                   MOVE      '1'           XTBRIC
000801180220  "  C                   MOVE      socR70        XTBAZI
000802180220  "  C                   MOVE      *ZERO         XTBKEY
000803180220C1788C                   MOVE      '022'         XTBCOD
000804180220R235 C                   if        $RegAgev    = *On
000805180220  "  C                   MOVEl     SPATributo    wrk6              6
000806180220  "  C                   MOVE      SPATriProg    wrk6
000807180220R235 C                   else
000808180220C1788C                   MOVEl     MraTri        wrk6              6
000809180220C1788C                   MOVE      MraPtr        wrk6
000810180220R235 C                   endif
000811180220C1788C                   MOVE      wrk6          XTBKEY
000812180220  "  C                   CALLb     'XATB'
000813180220  "  C                   PARM                    XATBDS
000814180220  "  C     XTBERR        IFEQ      '0'
000815180220  "  C                   If        EscItcR70 = *On
000816180220  "  C                   SetOff                                       38
000817180220  "  C                   EndIf
000818180220  "  c*
000819180220  "  C                   If        EscImnR70 = *On
000820180220  "  C                   SetOff                                       39
000821180220  "  C                   EndIf
000822180220  "  c*
000823180220  "  C                   EndIf
000824180220C1788c                   EndIf
000825180220     C*
000826180220R235 C                   if        $RegAgev    = *On
000827180220 "   C   38              ADD       $TotPag       ITC
000828180220 "   C   39              ADD       $TotPag       IMN
000829180220 "   C                   ADD       0             SNS
000830180220 "   C                   ADD       0             IMS
000831180220 "   C                   ADD       0             RAC
000832180220R235 C                   else
000833180220     C                   Z-ADD     0             IMT
000834180220     C     MRAIMS        ADD       MRASNS        IMT
000835180220C1667C   38              ADD       MRAIMN        IMT
000836180220     C                   ADD       IMT           ITC
000837180220C1667C   39              ADD       MRAIMN        IMN
000838180220     C                   ADD       MRASNS        SNS
000839180220     C                   ADD       MRAIMS        IMS
000840180220     C                   ADD       MRARAC        RAC
000841180220R235 C                   endif
000842180220R235 C*
000843180220     C                   ADD       NETTO         NET
000844180220     C*
000845180220     C                   endif
000846180220     C*
000847180220     C     ENDSU1        ENDSR                                                  *
000848180220     C*----------------------------------------------------*
000849180220     C* SUBROUTINE SCRITTURA FILE DA DS MULTIPLA           *
000850180220     C*----------------------------------------------------*
000851180220     C     SCRIVI        BEGSR                                                  *
000852180220     C*
000853180220     C     1             DO        50            X                              *
000854180220     C*
000855180220     C     RGPT(X)       IFEQ      *BLANK                                       *
000856180220     C                   GOTO      ENDSUB                                       *
000857180220     C                   END                                                    *
000858180220     C*
000859180220     C     X             OCCUR     ND770                                60      *
000860180220     C   60              GOTO      ENDSUB                                       *
000861180220     C*
000862180220     C* Scrittura nuovo record
000863180220C1478C                   Clear                   ND770000                       *
000864180220     C                   MOVE      SOC           N77SOCIETA                     *
000865180220     C                   Z-ADD     ACF           N77ACF                         *
000866180220     C                   MOVE      QUA           N77QUA                         *
000867180220     C                   MOVE      KCC           N77KCC                         *
000868180220     C                   MOVE      KSC           N77KSC                         *
000869180220     C                   MOVE      RGT           N77RGT                         *
000870180220     C                   MOVE      PRG           N77PRG                         *
000871180220     C                   MOVE      CDF           N77CDF                         *
000872180220     C                   MOVE      COG           N77COG                         *
000873180220     C                   MOVE      NOM           N77NOM                         *
000874180220     C                   MOVE      LNC           N77LNC                         *
000875180220     C                   MOVE      DNC           N77DNC                         *
000876180220     C                   MOVE      PNC           N77PNC                         *
000877180220     C                   MOVE      SES           N77SES                         *
000878180220D1183C     CDE           IFEQ      *BLANK
000879180220     C                   MOVE      CIT           N77CIT                         *
000880180220     C                   MOVE      VIA           N77VIA                         *
000881180220D1183C                   ELSE
000882180220  "  C                   MOVE      *BLANKS       N77CIT                         *
000883180220  "  C                   MOVE      *BLANKS       N77VIA                         *
000884180220D1183C                   ENDIF
000885180220     C                   MOVEL     C77           N77C77                         *
000886180220     C                   Z-ADD     ITC           N77ITC                         *
000887180220     C                   Z-ADD     IMN           N77IMN                         *
000888180220     C                   Z-ADD     SNS           N77SNS                         *
000889180220     C                   Z-ADD     IMS           N77IMS                         *
000890180220     C                   Z-ADD     PRA           N77PRA                         *
000891180220     C                   Z-ADD     RAC           N77RAC                         *
000892180220     C                   Z-ADD     0             N77NET                         *
000893180220     C                   MOVE      PFG           N77PFG                         *
000894180220     C                   MOVEL     SER           N77SER
000895180220     C                   MOVEL     CSE           N77CSE
000896180220     C                   MOVEL     CDE           N77CDE
000897180220     C     CDE           IFNE      *BLANK
000898180220     C                   MOVE      '*'           NUM
000899180220     C                   MOVE      *BLANK        PRV
000900180220     C                   ENDIF
000901180220     C                   MOVE      NUM           N77NUM
000902180220     C                   MOVE      PRV           N77PRV
000903180220C1478C                   MOVEL     *blanks       N77Tit
000904180220C1478C                   Z-add     0             N77Anno
000905180220C1478C                   Z-add     0             N77Reg
000906180220C1478C                   Z-add     0             N77ARG
000907180220C1478C                   Z-add     0             N77ARS
000908180220C1478C                   Z-add     0             N77IMSAP
000909180220C1478C                   Z-add     0             N77RACAP
000910180220C1478C                   MOVEL     *blanks       N77PIn
000911180220C1478C                   Z-add     0             N77DtIn
000912180220C1478C                   Z-add     0             N77DtFi
000913180220C1478C                   MOVEL     *blanks       N77COM
000914180220C1478C                   MOVEL     *blanks       N77RCDF
000915180220C1478C                   MOVEL     *blanks       N77RCOG
000916180220C1478C                   MOVEL     *blanks       N77RNOM
000917180220C1478C                   MOVEL     *blanks       N77RSES
000918180220C1478C                   MOVEL     *blanks       N77RLNC
000919180220C1478C                   MOVEL     DatMin        N77RDNC
000920180220C1478C                   MOVEL     *blanks       N77RPNC
000921180220C1770C*
000922180220  "  C* ritenuta a titolo d'imposta
000923180220  "  C                   Z-ADD     0             N77RIM
000924180220  "  C* addizionale a titolo d'imposta
000925180220  "  C                   Z-ADD     0             N77ARI
000926180220C1770C*
000927180220D1183C* i campi N77RVIA e N77RCIT indicano dal 2002 i dati degli esteri
000928180220  "  C     CDE           IFNE      *BLANK
000929180220  "  C                   MOVEL     CIT           N77RCIT                        *
000930180220  "  C                   MOVEL     VIA           N77RVIA                        *
000931180220D1183C                   ELSE
000932180220C1478C                   MOVEL     *blanks       N77RCIT
000933180220C1478C                   MOVEL     *blanks       N77RVIA
000934180220D1183C                   ENDIF
000935180220C1478C                   MOVEL     *blanks       N77RPRV
000936180220C1478C                   MOVEL     *blanks       N77RCSE
000937180220R102 C                   MOVEL     *blanks       N77CAUAS
000938180220R102 C                   MOVEL     *blanks       N77ASCDF
000939180220R102 C                   Z-Add     0             N77ASIMS
000940180220R102 C                   Z-Add     0             N77ASRAC
000941180220R102 C                   Z-Add     0             N77ASRIM
000942180220R102 C                   Z-Add     0             N77ASRSO
000943180220R102 C                   Z-Add     0             N77ASARA
000944180220R102 C                   Z-Add     0             N77ASARI
000945180220R102 C                   Z-Add     0             N77ASARS
000946180220     C*
000947180220R122 C* Campo CODICE (770 / 2013)
000948180220R122 C                   Clear                   N77Codice
000949180220R122 C                   If        N77NUM        = *blanks
000950180220R122 C                   If        N77IMN        > 0       or
000951180220R122 C                             N77SNS        > 0
000952180220R250 X***                Eval      N77Codice     = '3'
000953180220R282 X*** ex R250        Eval      N77Codice     = '6'
000954180220R282 C                   Eval      N77Codice     = '7'
000955180220R122 C                   EndIf                                                  *
000956180220R122 C                   EndIf                                                  *
000957180220     C*
000958180220R211 C                   Clear                   N77NRess
000959180220  "  C                   Clear                   N77DpCdf
000960180220  "  C                   Clear                   N77DpDen
000961180220  "  C                   Clear                   N77DpCenp
000962180220  "  C                   Clear                   N77DpCazi
000963180220  "  C                   Clear                   N77DpCate
000964180220  "  C                   Clear                   N77DpAcon
000965180220  "  C                   Clear                   N77DpImpa
000966180220  "  C                   Clear                   N77DpCodu
000967180220  "  C                   Clear                   N77DpCove
000968180220  "  C                   Clear                   N77OsCdf1
000969180220  "  C                   Clear                   N77OsCdf2
000970180220  "  C                   Clear                   N77OsCdf3
000971180220  "  C                   Clear                   N77F2Cdf
000972180220  "  C                   Clear                   N77F2Ime
000973180220  "  C                   Clear                   N77F2Rio
000974180220  "  C                   Clear                   N77F2Rino
000975180220  "  C                   Clear                   N77F2Cdf2
000976180220  "  C                   Clear                   N77F2Ime2
000977180220  "  C                   Clear                   N77F2Rio2
000978180220  "  C                   Clear                   N77F2Rino2
000979180220  "  C                   Clear                   N77F3Ine
000980180220  "  C                   Clear                   N77F3Rie
000981180220  "  C                   Clear                   N77F3Ain
000982180220  "  C                   Clear                   N77F3Ari
000983180220  "  C                   Clear                   N77F3Ine2
000984180220  "  C                   Clear                   N77F3Rie2
000985180220  "  C                   Clear                   N77F3Ain2
000986180220  "  C                   Clear                   N77F3Ari2
000987180220  "  C                   Eval      N77CitAP = N77Cit
000988180220  "  C                   Eval      N77PrvAP = N77Prv
000989180220R211 C                   Eval      N77Comap = N77Com
000990180220     C*
000991180220     C                   WRITE     ND770000                                     *
000992180220     C*
000993180220     C                   ENDDO                                                  *
000994180220     C*
000995180220     C     ENDSUB        ENDSR                                                  *
000996180220     C************************************************************
000997180220     C* REPERIMENTO DATI da ANPDC
000998180220     C************************************************************
000999180220     C     READANPDC     BEGSR
001000180220     C*
001001180220     C                   CLEAR                   ND002DS
001002180220     C                   EVAL      LenOut = %Size(ND002ds)
001003180220     C                   CallB     'NDMVC002'
001004180220     C                   PARM                    socR70
001005180220D0495C***                PARM                    MRAKCC
001006180220  "  C***                PARM                    MRAKSC
001007180220  "  C                   PARM                    APEKcc
001008180220D0495C                   PARM                    APEKsc
001009180220     C                   PARM      DatMin        Data
001010180220     C                   PARM      *OFF          GesMsg
001011180220     C                   PARM      *ON           Esito
001012180220     C                   PARM      'ND002DS'     Struttura
001013180220     C                   PARM                    ND002DS
001014180220     C                   PARM                    LenOut
001015180220     C*
001016180220     C                   ENDSR
001017180220     C************************************************************
001018180220     C* CONTROLLO SOGGETTI
001019180220     C************************************************************
001020180220     C     CTRSOG        BEGSR
001021180220     C*
001022180220     C                   MOVE      '1'           DVARIC
001023180220     C                   MOVE      socR70        DVASOCIETA
001024180220     C                   move      *blank        DVASTRUTT
001025180220     C                   MOVEL     'DVASOG'      DVASTRUTT
001026180220     C                   EVAL      DVALenOut = %Size(DVASOG)
001027180220     C                   MOVE      sogg002       DVASOGG
001028180220     C                   Clear                   DvaTpInd
001029180220     C                   Clear                   DvaCdInd
001030180220     C                   If        IndirR70      = '1'
001031180220     C                   If        APETpInd      <> *blanks and
001032180220     C                             APECdInd      <> *blanks
001033180220     C                   MOVE      APETpInd      DvaTpInd
001034180220     C                   MOVE      APECdInd      DvaCdInd
001035180220     C                   EndIf
001036180220     C                   EndIf
001037180220     C                   CALLB     'NDDVASOG'
001038180220     C                   PARM                    DVARIC
001039180220     C                   PARM                    DVASOCIETA
001040180220     C                   PARM                    DVAUNITA
001041180220     C                   PARM                    DVASTRUTT
001042180220     C                   PARM                    DVADTRIF
001043180220     C                   PARM                    DVALENOUT
001044180220     C                   PARM                    DVASNATURA
001045180220     C                   PARM                    *omit
001046180220     C                   PARM                    DVALINEAV
001047180220     C                   PARM                    DVAFILIALE
001048180220     C                   PARM                    DVASOGG
001049180220     C                   PARM                    DVATPIND
001050180220     C                   PARM                    DVACDIND
001051180220     C                   PARM                    DVAERRORE
001052180220     C                   PARM                    DVAOUTPUT
001053180220     C     DVAERRORE     IFEQ      *OFF
001054180220     C                   EVAL      %SUBST(DVASOG:1:DVALENOUT)
001055180220     C                              = %SUBST(DVAOUTPUT:1:DVALENOUT)
001056180220R235 C                   If        DVSCDFisc <> *blanks
001057180220  "  C                   Eval      $CDFIva = DVSCDFisc
001058180220  "  C                   Else
001059180220  "  C                   Eval      $CDFIva = DVSPartIVA
001060180220  "  C                   Endif
001061180220  "  C                   Eval      $NatGiur = DVSNatGiur
001062180220R235 C*
001063180220     C                   ENDIF
001064180220     C*
001065180220     C                   ENDSR
001066180220D0495C************************************************************
001067180220 "   C* DATI ANAGRAFICI VARIAZIONE ESTREMI FISCALI
001068180220 "   C************************************************************
001069180220 "   C     CALLDVACPI    BEGSR
001070180220 "   C*
001071180220 "   C                   move      '2'           DVCRIC
001072180220 "   C                   move      MRASocieta    DVCSOCIETA
001073180220 "   C                   move      *blank        DVCUNITA
001074180220 "   C                   movel     'ANCPI'       DVCSTRUTT
001075180220 "   C                   move      MRADDC        DVCDTRIF
001076180220 "   C                   eval      DVCLENOUT = %Size(ANCPI)
001077180220 "   C                   move      sogg002       DVCSOGG
001078180220 "   C*
001079180220 "   C                   CALLB     'NDDVACPI'    PDVACPI
001080180220 "   C*
001081180220 "   C                   ENDSR
001082180220 "   C/EJECT
001083180220D0495 ************************************************************
001084180220R235 C************************************************************
001085180220 "   C* DATI ANAGRAFICI VARIAZIONE ESTREMI FISCALI
001086180220 "   C************************************************************
001087180220 "   C     CallDVACPI2   BEGSR
001088180220 "   C*
001089180220 "   C                   Move      '2'           DVCRIC
001090180220 "   C                   Move      MOVSocieta    DVCSOCIETA
001091180220 "   C                   Move      *Blank        DVCUNITA
001092180220 "   C                   MoveL     'ANCPI'       DVCSTRUTT
001093180220 "   C                   Move      MOVDtDoc      DVCDTRIF
001094180220 "   C                   Eval      DVCLENOUT = %Size(ANCPI)
001095180220 "   C                   move      Sogg002       DVCSOGG
001096180220 "   C*
001097180220 "   C                   CALLB     'NDDVACPI'    PDVACPI
001098180220 "   C*
001099180220 "   C                   ENDSR
001100180220 "   C/EJECT
001101180220R235  ************************************************************
001102180220R235 C*----------------------------------------------------*
001103180220 "   C* Controllo se percipiente in regime agevolato       *
001104180220 "   C*----------------------------------------------------*
001105180220R235 C     CtrRegAgev    BEGSR
001106180220     C*
001107180220     C                   Eval      $RegAgev    = *Off
001108180220     C*
001109180220     C                   Eval      SPASocieta  = APESocieta
001110180220     C                   Eval      SPAKcc      = APEKcc
001111180220     C                   Eval      SPAKsc      = APEKsc
001112180220     C     K03SPA01      SETLL     SDGSPA01L
001113180220     C     K03SPA01      READE     SDGSPA01L                              21
001114180220     C                   Eval      $FineSPA    = *In21
001115180220     C*
001116180220     C                   DoW       $FineSPA    = *Off
001117180220     C*
001118180220     C* data documento compresa nel periodo in cui il percipiente
001119180220     C* è in regime agevolato
001120180220     C                   if        SPADtFin   >= MRADDc
001121180220     C                             and
001122180220     C                             SPADtIni   <= MRADDc
001123180220     C                   Eval      $RegAgev    = *On
001124180220     C                   LEAVE
001125180220     C                   endif
001126180220     C*
001127180220     C     K03SPA01      READE     SDGSPA01L                              21
001128180220     C                   Eval      $FineSPA    = *In21
001129180220     C                   ENDDO
001130180220     C*
001131180220R235 C                   ENDSR
001132180220R235 C*----------------------------------------------------*
001133180220 "   C* Caricamento importi per percipienti minimi         *
001134180220 "   C*----------------------------------------------------*
001135180220R235 C     RepRegAgev    BEGSR
001136180220     C*
001137180220     C                   Eval      $RegAgev    = *On
001138180220     C*
001139180220     C* Ciclo lettura percipienti in regime agevolato
001140180220     C     SocR70        SETLL     SDGSPA01L
001141180220     C     SocR70        READE     SDGSPA01L                              21
001142180220     C                   Eval      $FineSPA    = *In21
001143180220     C*
001144180220     C                   DoW       $FineSPA    = *Off
001145180220     C*
001146180220     C                   EXSR      RepDatiAna
001147180220     C*
001148180220     C* cerco le partite con fatture emesse
001149180220     C* dal percipiente nel periodo in cui era nel regime agevolato
001150180220     C* e per ogni partita cerco eventuali pagamenti da cui
001151180220     C* scorporare eventuale imposta IVA in proporzione
001152180220     C                   EXSR      RepPart
001153180220     C*
001154180220     C     RGPT(1)       IFNE      *BLANK                                       *  -6-
001155180220     C                   EXSR      SCRIVI
001156180220     C                   EXSR      INZDS
001157180220     C                   ENDIF
001158180220     C*
001159180220     C     SocR70        READE     SDGSPA01L                              21
001160180220     C                   Eval      $FineSPA    = *In21
001161180220     C                   ENDDO
001162180220     C*
001163180220R235 C                   ENDSR
001164180220R235 C*----------------------------------------------------*
001165180220 "   C* Reperimento dati anagrafici                        *
001166180220 "   C*----------------------------------------------------*
001167180220R235 C     RepDatiAna    BEGSR
001168180220     C*
001169180220     C                   Move      SPASocieta    APESocieta
001170180220     C                   Move      SPAKcc        APEKcc
001171180220     C                   Move      SPAKsc        APEKsc
001172180220     C     K03APE01      CHAIN     ANAPE01L                           21
001173180220     C                   if        *In21       = *Off
001174180220R235 C*
001175180220R235 C                   Eval      $SoggEst   = *on
001176180220     C* Reperisco dati conto
001177180220     C                   EXSR      READANPDC
001178180220     C     Esito         IfEq      *off
001179180220     C* Recupero dati soggetto
001180180220     C                   EXSR      CTRSOG
001181180220     C     DVAERRORE     IfEq      *ON
001182180220     C                   Clear                   DVASOG
001183180220     C                   EndIf
001184180220     C                   EndIf
001185180220     C                   endif
001186180220     C*
001187180220R235 C                   ENDSR
001188180220R235 C*----------------------------------------------------*
001189180220 "   C* Reperimento partite con fatture                    *
001190180220 "   C*----------------------------------------------------*
001191180220R235 C     RepPart       BEGSR
001192180220     C*
001193180220     C                   Move      *Off          $FinePart
001194180220     C                   Eval      $Fattura    = *Off
001195180220     C                   Eval      $NotaAccr   = *Off
001196180220     C                   Eval      $TotIVA     = 0
001197180220     C                   Eval      $TotFatt    = 0
001198180220     C                   Eval      $TotPag     = 0
001199180220     C*
001200180220     C                   Move      XScSoc        PASSocieta
001201180220     C                   Move      'CG'          PASCtb
001202180220     C                   Move      SPAKcc        PASKcc
001203180220     C                   Move      SPAKsc        PASKsc
001204180220     C     K04PAS02      SETLL     NDPAS02J
001205180220     C     K04PAS02      READE     NDPAS02J                               21
001206180220     C                   Eval      $FinePart   = *In21
001207180220     C*
001208180220     C                   if        $FinePart   = *Off
001209180220     C                   Eval      SavKcc      = PASKcc
001210180220     C                   Eval      SavKsc      = PASKsc
001211180220     C                   Eval      SavDtPar    = PASDtPar
001212180220     C                   Eval      SavNrPar    = PASNrPar
001213180220     C                   Eval      SavSerPar   = PASSerPar
001214180220     C                   endif
001215180220     C*
001216180220     C                   DoW       $FinePart   = *Off
001217180220     C*
001218180220     C* al cambio partita, eventualmente elaboro i dati della
001219180220     C* partita precedente
001220180220     C                   if        SavKcc     <> PASKcc   or
001221180220     C                             SavKsc     <> PASKsc   or
001222180220     C                             SavDtPar   <> PASDtPar or
001223180220     C                             SavNrPar   <> PASNrPar or
001224180220     C                             SavSerPar  <> PASSerPar
001225180220     C                   EXSR      ElabPart
001226180220     C                   Eval      SavKcc      = PASKcc
001227180220     C                   Eval      SavKsc      = PASKsc
001228180220     C                   Eval      SavDtPar    = PASDtPar
001229180220     C                   Eval      SavNrPar    = PASNrPar
001230180220     C                   Eval      SavSerPar   = PASSerPar
001231180220     C                   endif
001232180220     C*
001233180220     C* cerco la fattura e l'eventuale IVA
001234180220     C                   EXSR      RepFatt
001235180220     C*
001236180220R252 C                   if        $OkFatt     = *On
001237180220     C                   if        MOVDtDoc    < SPADtIni  or
001238180220     C                             MOVDtDoc    > SPADtFin
001239180220     C* salto tutti i movimemti di PAS relativi alla stessa partita
001240180220     C* questo perchè non può essere che nella stessa partita siano
001241180220     C* presenti documenti a regime agevolato e altri non a
001242180220     C* regime agevolato. Se il documento in esame non è a regime
001243180220     C* si assume che non ve ne siano altri nella partita
001244180220     C     K07PAS02      SETGT     NDPAS02J
001245180220     C*
001246180220     C                   else
001247180220R2524X*R252  salto tutti i movimemti di PAS relativi alla stessa registrazione
001248180220R2524X*R252 K10PAS02      SETGT     NDPAS02J
001249180220R2524C* salto tutti i movimemti di PAS relativi allo stesso movimento
001250180220R2524C     K11PAS02      SETGT     NDPAS02J
001251180220R252 C                   endif
001252180220 "   C*
001253180220R252 C                   endif
001254180220     C* cerco eventuali pagamenti
001255180220R252 C                   if        $OkFatt    <> *On
001256180220     C                   EXSR      RepPagam
001257180220     C                   endif
001258180220     C*
001259180220R252 X****               if        $OkFatt     = *On
001260180220 "   X**** salto tutti i movimemti di PAS relativi alla stessa registrazione
001261180220 "   X**** K10PAS02      SETGT     NDPAS02J
001262180220R252 X****               endif
001263180220     C*
001264180220     C* leggo la scadenza successiva
001265180220     C     K04PAS02      READE     NDPAS02J                               21
001266180220     C                   Eval      $FinePart   = *In21
001267180220     C                   ENDDO
001268180220     C*
001269180220     C* elaboro i dati dell'ultima partita esaminata
001270180220     C                   EXSR      ElabPart
001271180220     C*
001272180220R235 C                   ENDSR
001273180220R235 C*----------------------------------------------------*
001274180220 "   C* Selezione delle partite con fattura                *
001275180220 "   C*----------------------------------------------------*
001276180220R235 C     SelPart       BEGSR
001277180220     C*
001278180220     C                   Eval      $OkPart     = *On
001279180220     C*
001280180220     C                   Eval      $Causale    = MOVCausTes
001281180220     C                   EXSR      CallMVC003
001282180220     C                   if        $Esito      = *On
001283180220     C* la causale non esiste
001284180220     C                   Eval      $OkPart     = *Off
001285180220     C                   endif
001286180220     C*
001287180220     C*
001288180220     C                   if        $OkPart     = *On
001289180220     C*
001290180220     C                   Select
001291180220     C* la sottotipologia deve essere per Fornitori
001292180220     C                   When      OPESTP     <> 'F'
001293180220     C                   Eval      $OkPart     = *Off
001294180220     C*
001295180220     C* la tipologia deve essere per registrazioni IVA
001296180220     C                   When      OPETPC     <> '1'
001297180220     C                   Eval      $OkPart     = *Off
001298180220     C*
001299180220     C                   When      PASTpRegZ  <> 'D'
001300180220     C                   Eval      $OkPart     = *Off
001301180220     C*
001302180220     C                   endSl
001303180220     C*
001304180220     C                   endif
001305180220     C*
001306180220     C                   if        $OkPart     = *Off
001307180220R2524X**** salto tutti i movimemti di PAS relativi alla stessa registrazione
001308180220R2524X**** K10PAS02      SETGT     NDPAS02J
001309180220R2524C* salto tutti i movimemti di PAS relativi allo stesso movimento
001310180220R2524C     K11PAS02      SETGT     NDPAS02J
001311180220     C                   endif
001312180220     C
001313180220     C                   if        $OkPart     = *On
001314180220     C*
001315180220     C                   if        MOVDtDoc    < SPADtIni  or
001316180220     C                             MOVDtDoc    > SPADtFin
001317180220     C                   Eval      $OkPart     = *Off
001318180220     C* salto tutti i movimemti di PAS relativi alla stessa partita
001319180220     C* questo perchè non può essere che nella stessa partita siano
001320180220     C* presenti documenti a regime agevolato e altri non a
001321180220     C* regime agevolato. Se il documento in esame non è a regime
001322180220     C* si assume che non ve ne siano altri nella partita
001323180220     C     K07PAS02      SETGT     NDPAS02J
001324180220     C*
001325180220     C                   endif
001326180220     C*
001327180220     C                   endif
001328180220     C*
001329180220R235 C                   ENDSR
001330180220R235 C************************************************************
001331180220 "   C* Reperimento dati di causale operativa
001332180220 "   C************************************************************
001333180220R235 C     CallMVC003    BEGSR
001334180220     C*
001335180220     C                   Clear                   ANOPE
001336180220     C*
001337180220     C                   Move      PASDtReg      WData
001338180220     C                   Move      WData         WDtAlfa
001339180220     C*
001340180220     C* la causale deve essere valida alla data di registrazione
001341180220     C* passo quindi data di registrazione + un giorno
001342180220     C
001343180220     C*
001344180220     C                   Eval      $LenOut     = %Size(ANOPE)
001345180220     C                   CallB     'NDMVC003'
001346180220     C                   Parm                    XScSoc
001347180220     C                   Parm                    $Causale
001348180220     C                   Parm                    WDtAlfa
001349180220     C                   Parm      '0'           $Uso              1
001350180220     C                   Parm      *OFF          $GesMsg
001351180220     C                   Parm      *ON           $Esito
001352180220     C                   Parm      'ANOPE     '  $Struttura
001353180220     C                   Parm                    ANOPE
001354180220     C                   Parm                    $LenOut
001355180220     C*
001356180220R235 C                   ENDSR
001357180220R235 C/EJECT
001358180220R235 C*----------------------------------------------------*
001359180220 "   C* Reperimento dati anagrafici da variazioni fiscali  *
001360180220 "   C*----------------------------------------------------*
001361180220R235 C     RepVarFisc    BEGSR
001362180220     C*
001363180220     C                   Clear                   VarCPIva         20
001364180220     C                   EXSR      CallDVACPI2
001365180220     C                   if        DVCErrore   = *Off
001366180220     C                   MoveL     DVCOutput     AnCPI
001367180220     C                   if        CPICdFisc  <> *Blank
001368180220     C                   MoveL     CPICdFisc     VarCPIva
001369180220     C                   else
001370180220     C                   MoveL     CpIPartIva    VarCPIva
001371180220     C                   endif
001372180220     C*
001373180220R235 C                   Eval      $CDFIva = VarCPIva
001374180220R235 C                   EVAL      $NatGiur = CPINatGiur
001375180220     C                   endif
001376180220     C*
001377180220R235 C                   ENDSR
001378180220R235 C*----------------------------------------------------*
001379180220 "   C* Reperimento fatture                                *
001380180220 "   C*----------------------------------------------------*
001381180220R235 C     RepFatt       BEGSR
001382180220     C*
001383180220     C                   Move      *Off          $OkFatt
001384180220     C*
001385180220     C                   EXSR      SelFatt
001386180220     C*
001387180220     C                   if        $OkFatt     = *On
001388180220     C*
001389180220     C* Reperisco eventuali dati da variazioni fiscali
001390180220     C* alla data del documento (se ho più fatture, prendo
001391180220     C* i riferimenti relativi all'ultima fattura)
001392180220     C                   EXSR      RepVarFisc
001393180220     C*
001394180220     C                   Eval      $TotFatt    = $TotFatt + MOVImporto
001395180220     C*
001396180220R252 C* se non Autofattura acquisto,
001397180220R252 C                   if        OPEAFtAcq  <> '1'
001398180220     C                   EXSR      RepIVA
001399180220R252 C                   endif
001400180220     C*
001401180220     C                   endif
001402180220     C*
001403180220R235 C                   ENDSR
001404180220R235 C*----------------------------------------------------*
001405180220 "   C* Selezione delle fatture                            *
001406180220 "   C*----------------------------------------------------*
001407180220R235 C     SelFatt       BEGSR
001408180220     C*
001409180220     C                   Eval      $OkFatt     = *On
001410180220     C*
001411180220     C                   Eval      $Causale    = MOVCausTes
001412180220     C                   EXSR      CallMVC003
001413180220     C                   if        $Esito      = *On
001414180220     C* la causale non esiste
001415180220     C                   Eval      $OkFatt     = *Off
001416180220     C                   endif
001417180220     C*
001418180220     C*
001419180220     C                   if        $OkFatt     = *On
001420180220     C*
001421180220     C                   Select
001422180220R252 C* scarto le autofatture
001423180220 "   C                   When      OPETpDocI   = '8'
001424180220 "   C                   Eval      $OkFatt     = *Off
001425180220R252 C*
001426180220     C* scarto le note di accredito
001427180220     C                   When      PASDare     = 1
001428180220     C                   Eval      $OkFatt     = *Off
001429180220     C*
001430180220     C* la sottotipologia deve essere per Fornitori
001431180220     C                   When      OPESTP     <> 'F'
001432180220     C                   Eval      $OkFatt     = *Off
001433180220     C*
001434180220     C* la tipologia deve essere per registrazioni IVA
001435180220     C                   When      OPETPC     <> '1'
001436180220     C                   Eval      $OkFatt     = *Off
001437180220     C*
001438180220     C                   When      PASTpRegZ  <> 'D'
001439180220     C                   Eval      $OkFatt     = *Off
001440180220     C*
001441180220     C                   endSl
001442180220     C*
001443180220     C                   endif
001444180220     C*
001445180220     C*
001446180220     C                   if        $OkFatt     = *On
001447180220     C*
001448180220     C                   if        MOVDtDoc    < SPADtIni  or
001449180220     C                             MOVDtDoc    > SPADtFin
001450180220     C                   Eval      $OkFatt     = *Off
001451180220     C* salto tutti i movimemti di PAS relativi alla stessa partita
001452180220     C* questo perchè non può essere che nella stessa partita siano
001453180220     C* presenti documenti a regime agevolato e altri non a
001454180220     C* regime agevolato. Se il documento in esame non è a regime
001455180220     C* si assume che non ve ne siano altri nella partita
001456180220     C**** K07PAS02      SETGT     NDPAS02J
001457180220     C*
001458180220     C                   endif
001459180220     C*
001460180220     C                   endif
001461180220     C*
001462180220R235 C                   ENDSR
001463180220R235 C*----------------------------------------------------*
001464180220 "   C* Reperimento imposta                                *
001465180220 "   C*----------------------------------------------------*
001466180220R235 C     RepIVA        BEGSR
001467180220     C*
001468180220     C                   Eval      IVASys      = PASSys
001469180220     C                   Eval      IVANrAsReg  = PASNrAsReg
001470180220     C     K02IVA01      SETLL     NDIVA01L
001471180220     C     K02IVA01      READE     NDIVA01L                               21
001472180220     C                   Eval      $FineIVA    = *In21
001473180220     C*
001474180220     C                   DoW       $FineIVA    = *Off
001475180220     C*
001476180220     C                   Eval      $TotIVA     = $TotIVA +
001477180220     C                             (IVAImporto * (IVADare - IVAAvere))
001478180220     C*
001479180220     C     K02IVA01      READE     NDIVA01L                               21
001480180220     C                   Eval      $FineIVA    = *In21
001481180220     C                   ENDDO
001482180220     C*
001483180220R235 C                   ENDSR
001484180220R235 C*----------------------------------------------------*
001485180220 "   C* Reperimento pagamenti                              *
001486180220 "   C*----------------------------------------------------*
001487180220R235 C     RepPagam      BEGSR
001488180220     C*
001489180220     C                   Move      *Off          $OkPag
001490180220     C*
001491180220     C                   EXSR      SelPag
001492180220     C*
001493180220     C                   if        $OkPag      = *On
001494180220     C                   Eval      $TotPag     = $TotPag + (PASImporto *
001495180220     C                                           (PASDare - PASAvere)   )
001496180220     C                   endif
001497180220     C*
001498180220R235 C                   ENDSR
001499180220R235 C*----------------------------------------------------*
001500180220 "   C* Selezione dei pagamenti                            *
001501180220 "   C*----------------------------------------------------*
001502180220R235 C     SelPag        BEGSR
001503180220     C*
001504180220     C                   Eval      $OkPag      = *On
001505180220     C*
001506180220     C                   Reset                   XATBDS
001507180220     C                   Reset                   SDGHK1DS
001508180220     C*
001509180220     C                   MOVE      '1'           XTBRIC
001510180220     C                   MOVE      SocR70        XTBAZI
001511180220     C                   MOVE      *ZERO         XTBKEY
001512180220     C                   MOVE      'HK1'         XTBCOD
001513180220     C                   MOVE      PASCausRig    XTBKEY
001514180220     C*
001515180220     C                   CALLB     'XATB2'
001516180220     C                   PARM                    XATBDS
001517180220     C*
001518180220     C* causale non in tabella, scarto il movimento
001519180220     C                   if        XTBERR      = '1'
001520180220     C                   Eval      $OkPag      = *Off
001521180220     C                   endif
001522180220     C*
001523180220     C*
001524180220     C                   if        $OkPag      = *On
001525180220     C*
001526180220     C                   if        PASTpRegZ  <> 'D'
001527180220     C                   Eval      $OkPag      = *Off
001528180220     C                   endif
001529180220     C*
001530180220     C                   endif
001531180220     C*
001532180220     C                   if        $OkPag      = *On
001533180220     C* la data di registrazione del pagamento deve essere dell'anno
001534180220     C* di competenza della CU
001535180220     C                   Move(P)   PASDtReg      DataAlfa
001536180220     C                   Move      AAAlfa        WAnno             4 0
001537180220     C*
001538180220     C                   if        WAnno      <> AnnoR70
001539180220     C                   Eval      $OkPag      = *Off
001540180220     C                   endif
001541180220     C*
001542180220     C                   endif
001543180220     C*
001544180220R235 C                   ENDSR
001545180220R235 C*----------------------------------------------------*
001546180220 "   C* Reperisco i dati dal tributo                       *
001547180220 "   C*----------------------------------------------------*
001548180220R235 C     RepANTRI      BEGSR
001549180220     C*
001550180220     C                   Move      SPATributo    TRITributo
001551180220     C                   Move      SPATriProg    TRITriProg
001552180220     C     KEYTRI        CHAIN     ANTRI01L                           21
001553180220     C* se non l'ho trovato, imposto i valori di default per i percipienti
001554180220     C* in regime agevolato
001555180220     C                   if        *In21       = *On
001556180220     C                   Eval      TRIQua770   = 'AU'
001557180220     C                   Eval      TRIDes770   = 'R'
001558180220     C                   Eval      TRITriRagg  = *Blank
001559180220     C                   Eval      TRITributo  = *Blank
001560180220     C                   Eval      TRITriProg  = *Blank
001561180220     C                   endif
001562180220     C*
001563180220R235 C                   ENDSR
001564180220R235 C*----------------------------------------------------*
001565180220 "   C* Gestisco l'importo dell'IVA da decurtare dal pagam.*
001566180220 "   C*----------------------------------------------------*
001567180220R235 C     GestIVA       BEGSR
001568180220     C*
001569180220     C* calcolo l'IVA come fa la procedura dei bonifici, ovvero come
001570180220     C* l'imposta fosse in proporzione all'importo pagato, senza
001571180220     C* tenere conto del dettaglio della condizione di pagamento
001572180220     C                   Z-Add     $TotPag       WImpInp
001573180220     C                   CallP     X0201MultC(WImpInp: ($TotIVA / $TotFatt):
001574180220     C                                        1:
001575180220     C                                        'H': '0': NrDecMC: WImpOut)
001576180220     C                   Eval      $TotPag     = $TotPag - WImpOut
001577180220     C*
001578180220     C*
001579180220R235 C                   ENDSR
001580180220R235 C************************************************************
001581180220 "   C* Controllo stato
001582180220 "   C************************************************************
001583180220R235 C     CTRSTATO      BEGSR
001584180220     C*
001585180220     C*
001586180220     C                   Clear                   XATBDS
001587180220     C*
001588180220     C                   Eval      XTbRic      = '1'
001589180220     C                   Eval      XTbAzi      = SocR70
001590180220     C                   Eval      XTbKey      = *Zero
001591180220     C                   Eval      XTbCod      = 'G12'
001592180220     C                   Move      DVSSTATO      XTbkey
001593180220     C*
001594180220     C                   CALLB     'XATB2'
001595180220     C                   PARM                    XATBDS
001596180220     C*
001597180220     C                   if        XTbErr      = '0'
001598180220     C                   MoveL     XTbUni        TABG12
001599180220     C*
001600180220     C                   if        NazG12 = 'IT' and
001601180220     C                             CodUicG12 = 086
001602180220     C                   Movel     *Off          $SoggEst
001603180220     C                   Else
001604180220     C                   Movel     *On           $SoggEst
001605180220     C                   Endif
001606180220     C*
001607180220     C                   Else
001608180220     C                   Movel     *On           $SoggEst
001609180220     C*
001610180220     C                   Endif
001611180220     C*
001612180220R235 C                   ENDSR
001613180220R235 C/EJECT
001614180220R235 C************************************************************
001615180220  "  C* Controllo codice fiscale /partita iva, se italiani
001616180220  "  C************************************************************
001617180220R235 C     CTRFISCIVA    BEGSR
001618180220     C*
001619180220     C* soggetti e dell'anagrafica variazioni dati fiscali
001620180220     C*
001621180220 b2  C                   if        $cdfiva   <> *blank
001622180220     C*   e se la legislazione è italiana
001623180220     C                             and StaSKP = '1'
001624180220     C*
001625180220     C                   eval      XCodFisc = $Cdfiva
001626180220     C                   callb     'XCDFIS'
001627180220     C                   parm                    XCodFisc
001628180220     C                   parm                    Errfisc
001629180220     C*
001630180220     C* Se codice fiscale non valido
001631180220 b3  C                   if        Errfisc < 0
001632180220     C*
001633180220     C* Se TIPO natura giuridica "società di persone" o
001634180220     C* "società di capitale" si controlla il codice
001635180220     C* fiscale anche come partita IVA perchè in questi casi
001636180220     C* le due cose possono coincidere
001637180220     C                   MOVE      '1'           XTBRIC
001638180220     C                   MOVE      SocR70        XTBAZI
001639180220     C                   MOVE      *ZERO         XTBKEY
001640180220     C                   MOVE      'G16'         XTBCOD
001641180220     C                   MOVE      $NatGiur      XTBKEY
001642180220     C                   CALLB     'XATB2'
001643180220     C                   PARM                    XATBDS
001644180220     C     XTBERR        IFEQ      '0'
001645180220     C                   MOVEL     XTBUNI        TABG16
001646180220     C*
001647180220 b4  C                   if        TpNG16 = '2' OR TpNG16 = '3'
001648180220     C                   callb     'XPARIVA'
001649180220     C                   parm                    $Cdfiva
001650180220     C                   parm      *Zero         Errfisc
001651180220     C                   parm                    XScSoc
001652180220 e4  C                   endif
001653180220     C*
001654180220     C                   else
001655180220     C                   Movel     *On           $Soggest
001656180220     C                   ENDIF
001657180220     C*
001658180220     C* a questo punto se Esito < 0 significa che per i tipi
001659180220     C* natura giuridica '2' e '3' il codice fiscale è errato
001660180220     C* anche come partita IVA e che per il tipo natura
001661180220     C* giuridica '1' il codice fiscale è formalmente errato
001662180220 b4  C                   if        Errfisc < 0
001663180220     C* "Codice fiscale non valido"
001664180220     C                   Movel     *On           $Soggest
001665180220     C*
001666180220 x4  C                   else
001667180220     C                   Movel     *Off          $Soggest
001668180220 e4  C                   Endif
001669180220     C*
001670180220 x3  C                   else
001671180220     C                   Movel     *Off          $Soggest
001672180220 e3  C                   endif
001673180220     C*
001674180220 e2  C                   endif
001675180220     C*
001676180220R235 C                   Endsr
001677180220R235 C*
001678180220R235 C*----------------------------------------------------*
001679180220 "   C* Scrivo i dati di partita su DS multipla            *
001680180220 "   C*----------------------------------------------------*
001681180220R235 C     ElabPart      BEGSR
001682180220     C*
001683180220     C*
001684180220     C                   if        $TotFatt   <> 0        and
001685180220     C                             $TotPag    <> 0
001686180220     C*
001687180220     C* controlla il codice fiscale e la partita iva
001688180220     C                   Exsr      CTRStato
001689180220     C                   Exsr      CTRFiscIva
001690180220     C*
001691180220     C* Sono sicuramente percipienti esteri se
001692180220     C* nell'anagrafica percipiente è valorizzato l'identificativo
001693180220     C* fiscale estero e se lo stato è estero e il cod.fisc o la
001694180220     C* partita iva non sono italiani.
001695180220     C* Se estero, in CU non deve entrare
001696180220     C                   If        $SoggEst <> *on and
001697180220     C                             ApeCdFisc = *blanks
001698180220     C*
001699180220     C*
001700180220     C* calcolo l'importo del pagamento al netto dell'IVA
001701180220     C* Importo fattura : IVA fattura = Importo pagamento : IVA su pagamento
001702180220     C* ovvero:
001703180220     C* IVA su pagamento = (IVA fattura / Importo fattura ) * pagamento
001704180220     C                   if        $TotIVA    <> 0
001705180220     C                   EXSR      GestIVA
001706180220     C                   endif
001707180220     C*
001708180220     C                   EXSR      RepANTRI
001709180220     C*
001710180220     C                   EXSR      CARDS
001711180220     C*
001712180220     C                   Endif
001713180220     C*
001714180220R2523C*
001715180220R2523C                   endif
001716180220     C*
001717180220     C                   Eval      $TotIVA     = 0
001718180220     C                   Eval      $TotFatt    = 0
001719180220     C                   Eval      $TotPag     = 0
001720180220     C*
001721180220R2523X****               endif
001722180220     C*
001723180220R235 C                   ENDSR
