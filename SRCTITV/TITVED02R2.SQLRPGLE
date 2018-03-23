000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131023     FTABEL00F  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600131022     FTITA730C  IF   E           K DISK
000700131022     FNDREG04L  IF   E           K DISK
000800131022     FNDIVA01L  IF   E           K DISK
000900131022     FNDPAS01L  IF   E           K DISK
001000160217     FFIRAB01L  IF   E           K DISK
001100131019     Ftivgdtmp  UF   E             DISK
001200090323     FTIVAWWWT  UF A E             DISK    commit
001300031103
001400030924
001500041015     D*-----------
001600041015     D* VARIABILI D WRK
001700041015     D*-----------
001800131019     D prmppt          S             50
001900030924     D prmesito        S              1
002000030924     D wrkesito        S                   like(prmesito)
002100131004     D campo_alfa      S            512
002200131004     D currDate6       S              6
002300131004     D currDate6inv    S              6
002400131018     D currDate8       S              8
002500131004     D currHHMM        S              4
002600131019     D Conta_msg       S             10i 0 inz
002700131019     D Conta_item      S             10i 0 inz
002800131019     D Conta_segm      S             10i 0 inz
002900131022     D wPagamGG        S              3  0 inz
003000131023     D wCurrIVA        S              5  2 inz
003100131022     D wTotIMV         S             30  3 inz
003200131022     D wTotIVA         S             30  3 inz
003300131023     D wTotFATT        S             30  3 inz
003400131019     D savNFT          S                   inz like(vafNFT)
003500140107     D savFIV          S                   inz like(vafFIV)
003600131021     D wNAD_CA_2       S            256    inz
003700131021     D wNAD_CA_3       S            256    inz
003800131021     D wRFF_VA_1_sgm   S            256    inz
003900131030     D wSVN            S              1    inz
004000131023     D wVAN            S             13  3 inz
004100131030     D wDES            S             40    inz
004200131205     D wBOLLO          S             19  3 inz
004300160217     D wDepRMN         S                   like(vafRMN) inz
004400160217     D wDepRMA         S                   like(vafRMA) inz
004500160217     D vawDTA_Acc      S                   like(vawDTA) inz
004600131004
004700131004
004800131004     D*-----------
004900131004     D* DEFINIZIONE DS ESTERNE
005000131004     D*-----------
005100131004     D trul33ds      e ds                  inz
005200131004     D kpjba         e ds                  inz
005300131019     D tivafds       e ds                  inz extname(tivaf00t)
005400131023     D dscc          e ds                  inz
005500160208     D DTA4A         e ds
005600131022
005700131022
005800131022     D*------------
005900131022     D* SCHIERE DETTAGLI REGISTRAZIONE CONTABILE
006000131022     D*------------
006100131023     D skIVAIMP        S             19  3 dim(100)
006200131023     D skIVAALQ        S              5  2 dim(100)
006300131022     D skIMPONB        S             19  3 dim(100)
006400131023     D j1              S              3  0 inz
006500131030     D skSVN           S              1    DIM(100)
006600131030     D skSVNDES        S             40    DIM(100)
006700131030     D skSVNnoIVA      S              1    DIM(100)
006800131023     D j2              S              3  0 inz
006900131030     D skALC           S              5    DIM(100)
007000131030     D skALC_DES       S             40    DIM(100)
007100131023     D skMOA_8         S             19  3 DIM(100)
007200131023     D skTAX_7         S              5  2 DIM(100)
007300131023     D j3              S              3  0 inz
007400131030     D skCHRG_BRT      S              1    DIM(100)
007500131030     D skCHRG_CLI      S              5    DIM(100)
007600131030     D skCHRG_DES      S             40    DIM(100)
007700131030     D j4              S              3  0 inz
007800160217     D skCpyAccorpa    S                   LIKE(vawDTA) DIM(5000) INZ
007900160217     D jCpyAcc         S              5  0 INZ(*zeros)
008000131022
008100131004
008200131004     D*------------------
008300131004     D* LINKING A DEFINIZIONI ESTERNE
008400131004     D*------------------
008500131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
008600131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
008700160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
008800131021     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
008900131021      /DEFINE DFTACTGRP_YES
009000131021     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
009100131021      /UNDEFINE DFTACTGRP_YES
009200131021
009300131021     ***************************************************************************
009400131021     **
009500131021     ** Definizione strutture dati.
009600131021     **
009700131021      **************************************************************************
009800131021     D tibsSocI0...
009900131021     D               E DS                  QUALIFIED
010000131021     D                                     INZ
010100131021     D tibsSocO0...
010200131021     D               E DS                  QUALIFIED
010300131021     D                                     INZ
010400131021     ***************************************************************************
010500131021     **
010600131021     ** Definizione variabili modulo/programma.
010700131021     **
010800131021     ***************************************************************************
010900131021     D prmRqsOpCode...
011000131021     D                 S             10A
011100131021     D prmRpyOpCode...
011200131021     D                 S             10A
011300131021     D prmRpyIdMsg...
011400131021     D                 S             10I 0
011500131021     D prmRqsFormato...
011600131021     D                 S             10A
011700131021     D prmRqsDataSize...
011800131021     D                 S             10I 0
011900131021     D prmRpyFormato...
012000131021     D                 S             10A
012100131021     D prmRpyDataSize...
012200131021     D                 S             10I 0
012300110705
012400031103
012500031103     D*-----------
012600031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
012700031103     D*-----------
012800131019     D DSPPT           DS
012900131030     D  w§PPTKSU                      7  0
013000131030     D  w§PPTCVT                      1
013100131018     D  w§UNB_ID_1                    5
013200131018     D  w§UNB_ID_2                   25
013300131004
013400131030
013500131030     D*-----------
013600131030     D* SCHIERE A PROGRAMMA
013700131030     D*-----------
013800160128     D CHRGCODES       S             60    DIM(48) CTDATA PERRCD(1)
013900131030
014000131030
014100131030     D*-----------
014200131030     D* RIDEFINIZIONE SCHIERA A PROGRAMMA CHRGCODES
014300131030     D*-----------
014400131030     D DS_CHRGCODE     DS
014500131030     D  CHRGCODE_BRT                  5
014600131030     D  CHRGCODE_CLI                  5
014700131030     D  CHRGCODE_DES                 40
014800131004
014900131030
015000131004     D*-----------
015100131018     D* COSTANTI / DEFAULT
015200131004     D*-----------
015300131018     D UNB_sgm         C                   CONST('UNB+UNOA:1+')
015400131007     D UNH_sgm_1       C                   CONST('UNH+')
015500131018     D UNH_sgm_2       C                   CONST('+INVOIC:D:96A:UN:EAN008')
015600131018     D BGM_sgm_1       C                   CONST('BGM+380+')
015700131018     D BGM_sgm_2       C                   CONST('+9')
015800131018     D DTM_3_1         C                   CONST('DTM+3:')
015900131018     D DTM_3_2         C                   CONST(':102')
016000131018     D DTM_137_1       C                   CONST('DTM+137:')
016100131018     D DTM_137_2       C                   CONST(':102')
016200131018     D RFF_IT_sgm      C                   CONST('RFF+IT:')
016300131021     D NAD_CA_1        C                   CONST('NAD+CA+')
016400131021     D NAD_CA_2        C                   CONST('BRT S.P.A.+PIAZZA DIAZ, 7+')
016500131021     D NAD_CA_3        C                   CONST('MILANO++20123+IT')
016600150309     D RFF_VA_1_sgm    C                   CONST('RFF+VA:IT04507990150')
016700131018     D NAD_SF_1        C                   CONST('NAD+SF+5450534001540::9++')
016800131128     D NAD_SF_2        C                   CONST('AMAZON EU SARL:')
016900131128     D NAD_SF_3        C                   CONST('CO AMAZON LOGISTICA SRL+')
017000131128     D NAD_SF_4        C                   CONST('STRADA DOGANA PO 2 T')
017100131128     D NAD_SF_5        C                   CONST('CASTEL SAN GIOVANNI++')
017200131128     D NAD_SF_6        C                   CONST('29015+IT')
017300140505     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000345::9++')
017400140505     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
017500140505     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
017600140505     D*NAD_IV_4        C                   CONST('AMAZON IT LOGISTICA SRL:')
017700140505     D*NAD_IV_5        C                   CONST('VIA FERR. APORTI 8 - ')
017800140505     D*NAD_IV_6        C                   CONST('IT07231660965+')
017900140505     D*NAD_IV_7        C                   CONST('MILANO++20125+IT')
018000140508     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000345::9++')
018100140508     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
018200140508     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
018300140508     D*NAD_IV_4        C                   CONST('PI IT05257010966:')
018400140508     D*NAD_IV_5        C                   CONST('RAPPR FISCALE AMAZON ITALIA:')
018500140508     D*NAD_IV_6        C                   CONST('LOGISTICA SRL VIA FERRANTE')
018600140508     D*NAD_IV_7        C                   CONST('APORTI 8+MILANO++20125+IT')
018700140516     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000055::9++')
018800140516     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
018900140516     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
019000140516     D*NAD_IV_4        C                   CONST('CO ACCOUNTING SERVICE CENTER')
019100140516     D*NAD_IV_5        C                   CONST(':1-9 THE GROVE+SLOUGH++')
019200140516     D*NAD_IV_6        C                   CONST('SL1 1QP+GB')
019300140516     D*NAD_IV_7        C                   CONST('')
019400150309     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000055::9++')
019500150309     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:5 RUE PLAETIS')
019600150309     D*NAD_IV_3        C                   CONST(' L-2338 LUXEMBOURG+C/O AP')
019700150309     D*NAD_IV_4        C                   CONST(' NON-INVENTORY FINANCIAL')
019800150309     D*NAD_IV_5        C                   CONST(' OPS:1-9 THE GROVE+')
019900150309     D*NAD_IV_6        C                   CONST('SLOUGH++SL1 1QP+GB')
020000150309     D*NAD_IV_7        C                   CONST('')
020100150309     D NAD_IV_1        C                   CONST('NAD+IV+5450534005845::9++')
020200150309     D NAD_IV_2        C                   CONST('AMAZON EU SARL, ')
020300150309     D NAD_IV_3        C                   CONST('SUCCURSALE ITALIANA+')
020400150309     D NAD_IV_4        C                   CONST('VIA FERRANTE APORTI 8+')
020500150309     D NAD_IV_5        C                   CONST('MILANO++20125+IT')
020600150309     D NAD_IV_6        C                   CONST('')
020700140516     D NAD_IV_7        C                   CONST('')
020800150309     D*RFF_VA_2_sgm    C                   CONST('RFF+VA:LU20260743')
020900150309     D RFF_VA_2_sgm    C                   CONST('RFF+VA:IT08973230967')
021000131018     D CUX_sgm         C                   CONST('CUX+2:EUR:4')
021100131022     D PAT_sgm         C                   CONST('PAT+1++5::D:')
021200131019     D PCD_sgm         C                   CONST('PCD+7:0:13')
021300131019     D LIN_sgm         C                   CONST('LIN+')
021400131023     D IMD_sgm         C                   CONST('IMD+F+8+:::BRT_IT')
021500140407     D IMD_sgm_Bollo   C                   CONST('IMD+F+8+:::STAMP DUTY')
021600131019     D MOA_203_sgm     C                   CONST('MOA+203:')
021700131019     D PRI_sgm         C                   CONST('PRI+CAL:')
021800131022     D TAX_sgm         C                   CONST('TAX+7+VAT+++:::')
021900131019     D NAD_CN          C                   CONST('NAD+CN++')
022000131019     D RFF_AAM_sgm     C                   CONST('RFF+AAM:')
022100131019     D RFF_CR_sgm      C                   CONST('RFF+CR:')
022200131019     D RFF_ACC_sgm     C                   CONST('RFF+ACC:')
022300131019     D DTM_200_1       C                   CONST('DTM+200:')
022400131019     D DTM_200_2       C                   CONST(':102')
022500131023     D ALC_sgm         C                   CONST('ALC+C++++')
022600140407     D ALC_sgm_Bollo   C                   CONST('ALC+C++++SDU:::STAMP DUTY')
022700131023     D MOA_8_sgm       C                   CONST('MOA+8:')
022800131023     D UNS_sgm         C                   CONST('UNS+S')
022900131019     D CNT_2_sgm       C                   CONST('CNT+2:')
023000131019     D MOA_79_sgm      C                   CONST('MOA+79:')
023100131019     D MOA_58_sgm      C                   CONST('MOA+58:')
023200131019     D MOA_77_sgm      C                   CONST('MOA+77:')
023300131205     D MOA_124_sgm     C                   CONST('MOA+124:')
023400131205     D MOA_125_sgm     C                   CONST('MOA+125:')
023500131004     D UNT_sgm         C                   CONST('UNT+')
023600131004     D UNZ_sgm         C                   CONST('UNZ+')
023700131004     D piu             C                   CONST('+')
023800131004     D diviso          C                   CONST(':')
023900131004     D apice           C                   CONST('''')
024000131022     D societa         C                   CONST('201')
024100131021     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
024200131021     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
024300131004
024400030924
024500030325
024600030924     C* MAIN/
024700170123     C*
024800170123     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
024900170123     C
025000170123     C/EXEC SQL
025100170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
025200170123     C/END-EXEC
025300131021     C*
025400131021     C* Inizializzo il TIBSSOCR
025500131025     C                   SETOFF                                       2021
025600131021     C                   SETON                                        20
025700131021     C                   EXSR      EXESOCR
025800041015     C*
025900041015     C* Carica i dati tabellati
026000041015     C                   EXSR      cartab
026100041015     C*
026200041015     C* Esegue traduzione
026300131004     C                   setoff                                       31
026400131004     C  N31              EXSR      chkRECFILMBR
026500131004     C  N31              EXSR      rtvID
026600131004     C  N31              EXSR      wriHeader
026700131004     C  N31              EXSR      traduci
026800131004     C  N31              EXSR      wriFooter
026900131004 xxx C***31              EXSR      sndAlert
027000131021     C*
027100131021     C* Chiudo il TIBSSOCR
027200131025     C                   SETOFF                                       2021
027300131021     C                   SETON                                        21
027400131021     C                   EXSR      EXESOCR
027500160208     C*
027600160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
027700160208     C                   CALL      'UBTA400R'
027800160208     C                   PARM                    UBTA4IOPZ
027900160208     C                   PARM      'C'           UBTA4ITLA
028000160208     C                   PARM                    UBTA4IAAS
028100160208     C                   PARM                    UBTA4ILNP
028200160208     C                   PARM                    UBTA4INRS
028300160208     C                   PARM                    UBTA4INSP
028400160208     C                   PARM                    UBTA4ITRC
028500160208     C                   PARM                    UBTA4OERR
028600160208     C                   PARM                    UBTA4ODS
028700160208     C                   PARM                    UBTA4OLEN
028800160208     C                   PARM                    UBTA4ODATI
028900110705     C*
029000921023     C                   SETON                                        LR
029100131004
029200131004
029300131004
029400131004     C*----------------------------------------------------*
029500131004     C*  STACCO ID UNIVOCO
029600131004     C*----------------------------------------------------*
029700131004     C     RTVID         BEGSR
029800131004     C*
029900131004     C                   z-add     *zeros        currUNIQ_ID      15 0
030000131004     C*
030100131004     C* NSP => Stacco un numeratore da AZNUM
030200131004     C                   clear                   TRUL33DS
030300131004     C                   eval      I33OPE = *zeros
030400131004     C                   eval      I33CNU = 302
030500131004     C                   eval      I33NUM = 1
030600131004     C                   movel     TRUL33DS      KPJBU
030700131004     C                   call      'TRUL33R'
030800131004     C                   parm                    KPJBA
030900131004     C                   movel     KPJBU         TRUL33DS
031000131004     C                   if        O33ERR = *zeros
031100131004     C                   z-add     O33NRF        currUNIQ_ID
031200131004     C                   else
031300131004     C                   seton                                        31
031400131004     C                   endif
031500131004     C*
031600131004     C                   ENDSR
031700131021
031800131021
031900131021     C*----------------------------------------------------*
032000131021     C*  INIZIALIZZO / CHIUDO IL TIBSSOCR
032100131021     C*----------------------------------------------------*
032200131021     C     EXESOCR       BEGSR
032300131021     C*
032400131021     C* Inizializzo il programma.
032500131021     C                   IF        *IN20
032600131021     C                   CALL      'TIBSSOCR'
032700131021     C                   PARM      'INIT'        prmRqsOpCode
032800131021     C                   PARM                    prmRpyOpCode
032900131021     C                   PARM                    prmRpyIdMsg
033000131021     C                   ENDIF
033100131021     C*
033200131021     C* Chiudo il programma.
033300131021     C                   IF        *IN21
033400131021     C                   CALL      'TIBSSOCR'
033500131021     C                   PARM      'FINALIZE'    prmRqsOpCode
033600131021     C                   PARM                    prmRpyOpCode
033700131021     C                   PARM                    prmRpyIdMsg
033800131021     C                   ENDIF
033900131021     C*
034000131021     C                   ENDSR
034100131004
034200131021
034300131004     C     chkRECFILMBR  BEGSR
034400131004     C*
034500131004     C     *start        setll     TIVAWWWT
034600131004     C                   if        %found(TIVAWWWT)
034700131004     C                   seton                                        31
034800131004     C                   endif
034900131004     C*
035000131004     C                   ENDSR
035100131021
035200131021
035300131021     C     Decod_societa BEGSR
035400131021     C*
035500131021     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
035600131021     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
035700131021     C                   RESET                   tibsSocI0
035800131022     C                   EVAL      tibsSocI0.idSocieta  = societa
035900131021     C                   EVAL      tibsSocI0.dtValidita = %date(vafDFT)
036000131021     C
036100131021     C                   CALL      'TIBSSOCR'
036200131021     C                   PARM      'GETANAGRAF'  prmRqsOpCode
036300131021     C                   PARM      *BLANK        prmRpyOpCode
036400131021     C                   PARM      *ZERO         prmRpyIdMsg
036500131021     C                   PARM      'TIBSSOCI0'   prmRqsFormato
036600131021     C                   PARM                    tibsSocI0
036700131021     C                   PARM                    prmRqsDataSize
036800131021     C                   PARM      'TIBSSOCO0'   prmRpyFormato
036900131021     C                   PARM                    tibsSocO0
037000131021     C                   PARM                    prmRpyDataSize
037100131021     C                   if         PRMRPYIDMSG >= 0
037200131021     C                              and TIBSSOCO0.IDSOCIETA <> *blank
037300131021     C     minu:maiu     xlate     TIBSSOCO0     TIBSSOCO0
037400131021     C                   if        TIBSSOCO0.SEDLEGNAZ = *blanks
037500131021     C                   eval      TIBSSOCO0.SEDLEGNAZ = 'IT'
037600131025     C                   endif
037700131025     C*
037800131025     C* Verifico eventuali caratteri non consentiti dal EDIFACT
037900131025     C                   movel     *blanks       wAlfaSOCRAG     512
038000131025     C                   movel     *blanks       wAlfaSOCIND     512
038100131025     C                   movel     *blanks       wAlfaSOCLOC     512
038200131025     C* SOC RAG
038300131025     C                   eval      campo_alfa = TIBSSOCO0.RAGSOCIALE
038400131025     C                   exsr      Char_Speciali
038500131025     C                   eval      wAlfaSOCRAG = campo_alfa
038600131025     C* SOC IND
038700131025     C                   eval      campo_alfa = TIBSSOCO0.SEDLEGIND
038800131025     C                   exsr      Char_Speciali
038900131025     C                   eval      wAlfaSOCIND = campo_alfa
039000131025     C* SOC LOC
039100131025     C                   eval      campo_alfa = TIBSSOCO0.SEDLEGLOC
039200131025     C                   exsr      Char_Speciali
039300131025     C                   eval      wAlfaSOCLOC = campo_alfa
039400131025     C*
039500131025     C                   eval      wNAD_CA_2     = %trim(wAlfaSOCRAG)          +
039600131021     C                                             piu                         +
039700131025     C                                             %trim(wAlfaSOCIND)          +
039800131021     C                                             piu
039900131025     C                   eval      wNAD_CA_3     = %trim(wAlfaSOCLOC)          +
040000131021     C                                             piu                         +
040100131021     C                                             piu                         +
040200131021     C                                             %trim(TIBSSOCO0.SEDLEGCAP)  +
040300131021     C                                             piu                         +
040400131021     C                                             %trim(TIBSSOCO0.SEDLEGNAZ)
040500131021     C                   eval      wRFF_VA_1_sgm = 'RFF+VA:'                   +
040600131021     C                                             %trim(TIBSSOCO0.PARTITAIVA)
040700131021     C                   else
040800131021     C                   eval      wNAD_CA_2     = NAD_CA_2
040900131021     C                   eval      wNAD_CA_3     = NAD_CA_3
041000131021     C                   eval      wRFF_VA_1_sgm = RFF_VA_1_sgm
041100131021     C                   endif
041200131021     C*
041300131021     C                   ENDSR
041400131004
041500131004
041600131004     C     wriOUTPUT     BEGSR
041700131004     C*
041800131004     C                   write     TIVAW000
041900131004     C*
042000131004     C                   ENDSR
042100130807
042200041015
042300131023     C     cartab        BEGSR
042400041015     C*
042500131023     C* Reperisco le descrizioni delle varie
042600131023     C                   z-add     *zeros        j2
042700131023     C                   z-add     1             tblKUT
042800131023     C                   movel(P)  'CC'          tblCOD
042900131023     C     KEYtab00_P1   setll     TABEL00F
043000131023     C                   if        %equal(TABEL00F)
043100131023     C     KEYtab00_P1   reade     TABEL00F
043200131023     C                   dow       not %eof(TABEL00F)
043300131030     C                   if        %subst(tblKEY:1:5) = 'VARIE'
043400131023     C                   add       1             j2
043500131030     C                   eval      skSVN(j2) = %subst(tblKEY:8:1)
043600131030     C                   movel(P)  tblUNI        DSCC
043700131023     C                   movel(P)  §CCDES        skSVNDES(j2)
043800131023     C                   if        §CCJEI <> *blanks
043900131023     C                   eval      skSVNnoIVA(j2) = 'Y'
044000131023     C                   endif
044100131030     C                   endif
044200131023     C     KEYtab00_P1   reade     TABEL00F
044300131023     C                   enddo
044400131023     C                   endif
044500131030     C*
044600131030     C* Carico i codici varie personalizzati per cliente
044700131030     C                   z-add     1             j4
044800131030     C                   dow       j4 <= %elem(CHRGCODES) and
044900131030     C                                   CHRGCODES(j4) <> *blanks
045000131030     C                   eval      DS_CHRGCODE = CHRGCODES(j4)
045100131030     C                   eval      skCHRG_BRT(j4) = CHRGCODE_BRT
045200131030     C                   eval      skCHRG_CLI(j4) = CHRGCODE_CLI
045300131030     C                   eval      skCHRG_DES(j4) = CHRGCODE_DES
045400131030     C                   add       1             j4
045500131030     C                   enddo
045600041015     C*
045700041015     C                   ENDSR
045800131004
045900131004
046000131004     C     Char_Speciali BEGSR
046100131111     C*
046200131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
046300131111     C*
046400131111     C/EXEC SQL
046500131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
046600131111     C/END-EXEC
046700131111     C*
046800131111     C/EXEC SQL
046900131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
047000131111     C/END-EXEC
047100131111     C*
047200131111     C/EXEC SQL
047300131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
047400131111     C/END-EXEC
047500131111     C*
047600131111     C/EXEC SQL
047700131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
047800131111     C/END-EXEC
047900131111     C*
048000131111     C/EXEC SQL
048100131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
048200131111     C/END-EXEC
048300131111     C*
048400131004     C                   ENDSR
048500131004
048600131004
048700131004     C     wriHeader     BEGSR
048800131010     C*
048900131019     C*  UNB -
049000131004     C                   eval      vawDTA = UNB_sgm
049100131018     C                              + %trim(w§UNB_ID_1)+%trim(w§UNB_ID_2)
049200131004     C                              +   piu
049300131004     C                              + %trim(currDate6inv)
049400131004     C                              +   diviso
049500131004     C                              + %trim(currHHMM)
049600131004     C                              +   piu
049700131004     C                              + %trim(
049800131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
049900131004     C                              +   apice
050000131004     C                   exsr      wriOUTPUT
050100131004     C*
050200131004     C                   ENDSR
050300131019
050400131019
050500131019     C     wriSingle     BEGSR
050600131021     C*
050700131021     C* Reperisco i dati societari di fatturazione
050800131021     C                   exsr      Decod_societa
050900131022     C*
051000131022     C* Reperisco i dati della registrazione contabile
051100131022     C                   exsr      rtvContab
051200131019     C*
051300131019     C*  UNH -
051400131019     C                   eval      Conta_segm  = Conta_segm  + 1
051500131019     C                   eval      vawDTA = UNH_sgm_1
051600131022     C                              + %trim(%editc(Conta_msg:'3'))
051700131019     C                              + UNH_sgm_2
051800131019     C                              +   apice
051900131019     C                   exsr      wriOUTPUT
052000131019     C*
052100131019     C*  BGM -
052200131019     C                   eval      Conta_segm  = Conta_segm  + 1
052300131019     C                   eval      vawDTA = BGM_sgm_1
052400140506     C                              + %trim(%editc(vafNFT:'X'))
052500140506     C                              + %trim(%editc(vafFIV:'X'))
052600131019     C                              + BGM_sgm_2
052700131019     C                              +   apice
052800131019     C                   exsr      wriOUTPUT
052900131019     C*
053000131019     C*  DTM 3 -
053100131019     C                   eval      Conta_segm  = Conta_segm  + 1
053200131019     C                   eval      vawDTA = DTM_3_1
053300131022     C                              + %trim(%editc(vafDFT:'3'))
053400131019     C                              + DTM_3_2
053500131019     C                              +   apice
053600131019     C                   exsr      wriOUTPUT
053700131019     C*
053800131023     C*  DTM 137 -
053900131019     C                   eval      Conta_segm  = Conta_segm  + 1
054000131019     C                   eval      vawDTA = DTM_137_1
054100131023     C                              + %trim(currDate8)
054200131019     C                              + DTM_137_2
054300131019     C                              +   apice
054400131019     C                   exsr      wriOUTPUT
054500131019     C*
054600131019     C*  RFF IT -
054700131019     C                   eval      Conta_segm  = Conta_segm  + 1
054800131019     C                   eval      vawDTA = RFF_IT_sgm
054900131030     C                              + %trim(%editc(w§PPTKSU:'3'))
055000131019     C                              +   apice
055100131019     C                   exsr      wriOUTPUT
055200131019     C*
055300131019     C*  NAD CA -
055400131019     C                   eval      Conta_segm  = Conta_segm  + 1
055500131021     C                   eval      vawDTA = NAD_CA_1
055600131019     C                              + %trim(w§UNB_ID_1)
055700131019     C                              +   piu
055800131019     C                              +   piu
055900131021     C                              + %trim(wNAD_CA_2)
056000131021     C                              + %trim(wNAD_CA_3)
056100131019     C                              +   apice
056200131019     C                   exsr      wriOUTPUT
056300131019     C*
056400131019     C*  RFF VA 1 -
056500131019     C                   eval      Conta_segm  = Conta_segm  + 1
056600131025     C                   eval      vawDTA = %trim(wRFF_VA_1_sgm)
056700131019     C                              +   apice
056800131019     C                   exsr      wriOUTPUT
056900131019     C*
057000131019     C*  NAD SF -
057100131019     C                   eval      Conta_segm  = Conta_segm  + 1
057200131019     C                   eval      vawDTA = NAD_SF_1
057300131019     C                              + NAD_SF_2
057400131019     C                              + NAD_SF_3
057500131019     C                              + NAD_SF_4
057600131019     C                              + NAD_SF_5
057700131128     C                              + NAD_SF_6
057800131019     C                              +   apice
057900131019     C                   exsr      wriOUTPUT
058000131019     C*
058100131019     C*  NAD IV -
058200131019     C                   eval      Conta_segm  = Conta_segm  + 1
058300131019     C                   eval      vawDTA = NAD_IV_1
058400131019     C                              + NAD_IV_2
058500131019     C                              + NAD_IV_3
058600131019     C                              + NAD_IV_4
058700131019     C                              + NAD_IV_5
058800131019     C                              + NAD_IV_6
058900131128     C                              + NAD_IV_7
059000131019     C                              +   apice
059100131019     C                   exsr      wriOUTPUT
059200131019     C*
059300131019     C*  RFF VA 2 -
059400131019     C                   eval      Conta_segm  = Conta_segm  + 1
059500131019     C                   eval      vawDTA = RFF_VA_2_sgm
059600131019     C                              +   apice
059700131019     C                   exsr      wriOUTPUT
059800131019     C*
059900131019     C*  CUX -
060000131019     C                   eval      Conta_segm  = Conta_segm  + 1
060100131019     C                   eval      vawDTA = CUX_sgm
060200131019     C                              +   apice
060300131019     C                   exsr      wriOUTPUT
060400131019     C*
060500131019     C*  PAT -
060600131019     C                   eval      Conta_segm  = Conta_segm  + 1
060700131019     C                   eval      vawDTA = PAT_sgm
060800131022     C                              + %trim(%editc(wPagamGG:'3'))
060900131019     C                              +   apice
061000131019     C                   exsr      wriOUTPUT
061100131019     C*
061200131019     C*  PCD -
061300131019     C                   eval      Conta_segm  = Conta_segm  + 1
061400131019     C                   eval      vawDTA = PCD_sgm
061500131019     C                              +   apice
061600131019     C                   exsr      wriOUTPUT
061700131019     C*
061800131019     C                   ENDSR
061900131019
062000131019
062100140407     C     wriSummary    BEGSR
062200140407     C*
062300140407     C* Come ultima spedizione e prima dei dati di sommario della fattura corrente
062400140407     C* si inserisce una bolla fake relativa al bollo fattura in esenzione IVA
062500150401     C* (solo se effettivamente presente il bollo)
062600150401     C                   if        wBOLLO > *zeros
062700140407     C                   exsr      exeBollaBollo
062800150401     C                   endif
062900131019     C*
063000131019     C*  UNS -
063100131019     C                   eval      Conta_segm  = Conta_segm  + 1
063200131019     C                   eval      vawDTA = UNS_sgm
063300131019     C                              +   apice
063400131019     C                   exsr      wriOUTPUT
063500131019     C*
063600131019     C*  CNT 2 -
063700131019     C                   eval      Conta_segm  = Conta_segm  + 1
063800131019     C                   eval      vawDTA = CNT_2_sgm
063900131022     C                              + %trim(%editc(Conta_item:'3'))
064000131019     C                              +   apice
064100131019     C                   exsr      wriOUTPUT
064200131019     C*
064300131019     C*  MOA 79 -
064400131019     C                   eval      Conta_segm  = Conta_segm  + 1
064500131019     C                   eval      vawDTA = MOA_79_sgm
064600131219     C                              + %trim(%editc(wTotIMV:'3'))
064700131019     C                              +   apice
064800131019     C                   exsr      wriOUTPUT
064900131019     C*
065000131205     C*  MOA 58 -
065100140407     C                   eval      Conta_segm  = Conta_segm  + 1
065200140407     C                   eval      vawDTA = MOA_58_sgm
065300140407     C                              + %trim(%editc(wBOLLO:'3'))
065400140407     C                              +   apice
065500140407     C                   exsr      wriOUTPUT
065600131019     C*
065700131019     C*  MOA 77 -
065800131019     C                   eval      Conta_segm  = Conta_segm  + 1
065900131019     C                   eval      vawDTA = MOA_77_sgm
066000131022     C                              + %trim(%editc(wTotFATT:'3'))
066100131019     C                              +   apice
066200131019     C                   exsr      wriOUTPUT
066300131022     C*
066400131022     C* Ciclo per tutti gli imponibili/aliquote presenti in fattura
066500131023     C                   dow       j1 > *zeros
066600131022     C*
066700131022     C*  TAX -
066800131022     C                   eval      Conta_segm  = Conta_segm  + 1
066900131022     C                   eval      vawDTA = TAX_sgm
067000131023     C                              + %trim(%editc(skIVAALQ(j1):'3'))
067100131019     C                              +   apice
067200131019     C                   exsr      wriOUTPUT
067300131019     C*
067400131205     C*  MOA 124 -
067500131019     C                   eval      Conta_segm  = Conta_segm  + 1
067600131019     C                   eval      vawDTA = MOA_124_sgm
067700131023     C                              + %trim(%editc(skIVAIMP(j1):'3'))
067800131019     C                              +   apice
067900131019     C                   exsr      wriOUTPUT
068000131019     C*
068100131205     C*  MOA 125 -
068200131019     C                   eval      Conta_segm  = Conta_segm  + 1
068300131019     C                   eval      vawDTA = MOA_125_sgm
068400131023     C                              + %trim(%editc(skIMPONB(j1):'3'))
068500131022     C                              +   apice
068600131022     C                   exsr      wriOUTPUT
068700131022     C*
068800131023     C                   sub       1             j1
068900131022     C                   enddo
069000131019     C*
069100131019     C*  UNT -
069200131019     C                   eval      Conta_segm  = Conta_segm  + 1
069300131019     C                   eval      vawDTA = UNT_sgm
069400131022     C                              + %trim(%editc(Conta_segm:'3'))
069500131019     C                              +   piu
069600131022     C                              + %trim(%editc(Conta_msg:'3'))
069700131019     C                              +   apice
069800131019     C                   exsr      wriOUTPUT
069900131019     C*
070000131019     C                   ENDSR
070100131019
070200131004
070300131004     C     wriFooter     BEGSR
070400131004     C*
070500131004     C*  UNZ -
070600131004     C                   eval      vawDTA = UNZ_sgm
070700131004     C                              +   piu
070800131022     C                              + %trim(%editc(Conta_msg:'3'))
070900131004     C                              +   piu
071000131004     C                              + %trim(
071100131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
071200131004     C                              +   apice
071300131004     C                   exsr      wriOUTPUT
071400131004     C*
071500131004     C                   ENDSR
071600131019
071700131019
071800131019     C     exeInz        BEGSR
071900131019     C*
072000131019     C                   clear                   wTotIMV
072100131019     C                   clear                   wTotIVA
072200131019     C                   clear                   wTotFATT
072300131019     C                   clear                   Conta_msg
072400131019     C                   clear                   Conta_item
072500131019     C                   clear                   Conta_segm
072600131019     C*
072700131019     C                   ENDSR
072800030924
072900030924
073000991027     C     traduci       BEGSR
073100131004     C*
073200131019     C* Inizializzo i contatori a livello di transazione/messaggio
073300131019     C                   exsr      exeInz
073400131019     C*
073500131019     C                   READ      tivgdtmp
073600131019     C                   dow       not %eof(tivgdtmp)
073700131019     C                   movel     vgddta        tivafds
073800131022     C*
073900131022     C* Innanzitutto chaino la bolla direttamente su TITAS
074000131022     C     KEYtas30P     chain     titas30c
074100131022     C                   if        %found(titas30c)
074200160208     C*
074300160208     C* Reprrisco NATURA MERCE
074400160208     C                   EXSR      RTVNAS
074500131019     C*
074600140107     C* Effettuo rottura di codice per numero fattura/registro IVA
074700140107     C                   if        savNFT <> vafNFT AND savFIV <> vafFIV
074800140107     C                   if        savNFT  = *zeros AND savFIV  = *zeros
074900131019     C* Scarico i dati di testata fattura
075000131019     C                   exsr      wriSingle
075100131022     C* Elaboro la bolla corrente
075200131022     C                   exsr      exeBolla
075300131019     C*
075400131019     C                   else
075500131019     C* Elaboro la bolla corrente
075600131022     C                   exsr      exeBolla
075700131022     C* Scarico i dati di sommario a livello di fattura
075800131022     C                   exsr      wriSummary
075900131019     C* Inizializzo i contatori a livello di transazione/messaggio
076000131019     C                   exsr      exeInz
076100131019     C* Scarico i dati di testata fattura
076200131019     C                   exsr      wriSingle
076300131019     C                   endif
076400131019     C*
076500131019     C                   else
076600131019     C* Elaboro la bolla corrente
076700131022     C                   exsr      exeBolla
076800131019     C                   endif
076900131019     C
077000131019     C* Salvo i nuovi valori della rottuta di codice
077100131019     C                   eval      savNFT = vafNFT
077200140107     C                   eval      savFIV = vafFIV
077300131022     C*
077400131022     C                   endif
077500131019     C*
077600131019     C                   delete    tivgd000
077700160217     C*
077800131019     C                   read      tivgdtmp
077900131019     C                   enddo
078000131022     C*
078100131022     C* Scarico i dati di sommario a livello di fattura
078200131022     C                   exsr      wriSummary
078300991027     C*
078400030325     C                   EVAL      wrkesito = '0'
078500991027     C*
078600910830     C                   ENDSR
078700131019
078800131019
078900140407     C     exeBolla      BEGSR
079000131019     C*
079100131019     C* Incremento il contatore di item (ovvero spedizione)
079200131019     C                   add       1             Conta_item
079300131023     C*
079400131023     C* Reperisco i dati di dettaglio fatturazione spedizione corrente
079500131023     C                   exsr      rtvSvn
079600131019     C*
079700131019     C* Scarico il buffer d output
079800131019     C                   exsr      wriDettagli
079900160217     C*
080000160217     C* Salvo la nuova chiave di rottura
080100160217     C                   eval      wDepRMN = vafRMN
080200160217     C                   eval      wDepRMA = vafRMA
080300160217     C*
080400160217     C* Eseguo routine di De-Accorpamento in uscita dal ciclo di principale
080500160217     C                   exsr      DeAccorpa
080600160217     C*
080700160217     C* Inizializzazioni per nuova chiave di rottura
080800160217     C                   clear                   skCpyAccorpa
080900160217     C                   clear                   jCpyAcc
081000131019     C*
081100131019     C                   ENDSR
081200140407
081300140407
081400140407     C     exeBollaBollo BEGSR
081500140407     C*
081600140407     C* Incremento il contatore di item (ovvero spedizione)
081700140407     C                   add       1             Conta_item
081800140407     C*
081900140407     C* Scarico il buffer d output
082000140407     C                   exsr      wriBollaFake
082100140407     C*
082200140407     C                   ENDSR
082300140407
082400140407
082500140407     C     wriBollaFake  BEGSR
082600140407     C*
082700140407     C* Incremento il contatore di messaggio (al interno della transazione)
082800140407     C                   eval      Conta_msg = Conta_msg + 1
082900140407     C*
083000140407     C* Compongo segmenti per dati di dettaglio spedizione corrente
083100140407     C*
083200140407     C*  LIN -
083300140407     C                   eval      Conta_segm  = Conta_segm  + 1
083400140407     C                   eval      vawDTA = LIN_sgm
083500140407     C                              + %trim(%editc(Conta_item:'3'))
083600140407     C                              +   apice
083700140407     C                   exsr      wriOUTPUT
083800140407     C*
083900140407     C*  IMD -
084000140407     C                   eval      Conta_segm  = Conta_segm  + 1
084100140407     C                   eval      vawDTA = IMD_sgm_Bollo
084200140407     C                              +   apice
084300140407     C                   exsr      wriOUTPUT
084400140407     C*
084500140407     C*  MOA 203 -
084600140407     C                   eval      Conta_segm  = Conta_segm  + 1
084700140407     C                   eval      vawDTA = MOA_203_sgm
084800140407     C                              + '0'
084900140407     C                              +   apice
085000140407     C                   exsr      wriOUTPUT
085100140407     C*
085200140407     C*  PRI -
085300140407     C                   eval      Conta_segm  = Conta_segm  + 1
085400140407     C                   eval      vawDTA = PRI_sgm
085500140407     C                              + '0'
085600140407     C                              +   apice
085700140407     C                   exsr      wriOUTPUT
085800140407     C*
085900140407     C*  TAX -
086000140407     C                   eval      Conta_segm  = Conta_segm  + 1
086100140407     C                   eval      vawDTA = TAX_sgm
086200140407     C                              + '0'
086300140407     C                              +   apice
086400140407     C                   exsr      wriOUTPUT
086500140407     C*
086600140407     C*  NAD CN -
086700140407     C                   eval      Conta_segm  = Conta_segm  + 1
086800140407     C                   eval      vawDTA = NAD_CN
086900140407     C                              + 'DUMMY DATA'
087000140407     C                              +   piu
087100140407     C                              +   piu
087200140407     C                              + 'DUMMY DATA'
087300140407     C                              +   piu
087400140407     C                              + 'BOLOGNA'
087500140407     C                              +   piu
087600140407     C                              + 'BO'
087700140407     C                              +   piu
087800140407     C                              + '40138'
087900140407     C                              +   piu
088000140407     C                              + 'IT'
088100140407     C                              +   apice
088200140407     C                   exsr      wriOUTPUT
088300140407     C*
088400140407     C*  RFF AAM -
088500140407     C                   eval      Conta_segm  = Conta_segm  + 1
088600140407     C                   eval      vawDTA = RFF_AAM_sgm
088700140407     C                              + '00000'
088800140407     C                              +   apice
088900140407     C                   exsr      wriOUTPUT
089000140407     C*
089100140407     C*  RFF CR -
089200140407     C                   eval      Conta_segm  = Conta_segm  + 1
089300140407     C                   eval      vawDTA = RFF_CR_sgm
089400140407     C                              + '00000'
089500140407     C                              +   apice
089600140407     C                   exsr      wriOUTPUT
089700140407     C*
089800140407     C*  RFF ACC -
089900140407     C                   eval      Conta_segm  = Conta_segm  + 1
090000140407     C                   eval      vawDTA = RFF_ACC_sgm
090100140407     C                              + '00000'
090200140407     C                              +   apice
090300140407     C                   exsr      wriOUTPUT
090400140407     C*
090500140407     C*  DTM 200 -
090600140407     C                   eval      Conta_segm  = Conta_segm  + 1
090700140407     C                   eval      vawDTA = DTM_200_1
090800140407     C                              + %trim(%editc(vafDFT:'X'))
090900140407     C                              + DTM_200_2
091000140407     C                              +   apice
091100140407     C                   exsr      wriOUTPUT
091200140407     C*
091300140407     C*  ALC -
091400140407     C                   eval      Conta_segm  = Conta_segm  + 1
091500140407     C                   eval      vawDTA = ALC_sgm_Bollo
091600140407     C                              + %trim(skALC(j3))
091700140407     C                              +   apice
091800140407     C                   exsr      wriOUTPUT
091900140407     C*
092000140407     C*  MOA 8 -
092100140407     C                   eval      Conta_segm  = Conta_segm  + 1
092200140407     C                   eval      vawDTA = MOA_8_sgm
092300140407     C                              + %trim(%editc(wBOLLO:'3'))
092400140407     C                              +   apice
092500140407     C                   exsr      wriOUTPUT
092600140407     C*
092700140407     C*  TAX 7 -
092800140407     C                   eval      Conta_segm  = Conta_segm  + 1
092900140407     C                   eval      vawDTA = TAX_sgm
093000140407     C                              + '0'
093100140407     C                              +   apice
093200140407     C                   exsr      wriOUTPUT
093300140407     C*
093400140407     C                   ENDSR
093500131022
093600131022
093700131022     C     rtvContab     BEGSR
093800131022     C*
093900131205     C                   clear                   wBOLLO
094000131023     C                   z-add     *zeros        j1
094100131022     C                   clear                   NDREG000
094200131022     C*
094300131022     C                   eval      REGSOCIETA = societa
094400131022     C                   eval      REGCTB     = 'CG'
094500131022     C                   eval      REGDTREG   = %date(vafDFT)
094600131022     C                   eval      REGNRREG   = vafNFT
094700131022     C                   eval      REGSERREG  = '2'+ %trim(%editc(vafFIV:'X'))
094800131022     C*
094900131022     C* Aggancio la testata della registrazione contabile
095000131022     C     KEYreg04C     chain     ndreg04l
095100131022     C                   if        %found(ndreg04l)
095200131022     C*
095300131022     C* Filtro solamente le caussali 'NFT0'
095400131022     C                   if        REGCAUSTES = 'NFT0'
095500131023     C*
095600131023     C* Reperisco subito importo totale fattura
095700131023     C                   eval      wTotFATT = wTotFATT + REGIMPORTO
095800131022     C*
095900131022     C* Aggancio il dettaglio della registrazione contabile
096000131022     C     KEYiva01P     setll     ndiva01l
096100131022     C                   if        %found(ndiva01l)
096200131022     C     KEYiva01P     reade     ndiva01l
096300131022     C                   dow       not %eof(ndiva01l)
096400131022     C*
096500131022     C* Filtro solamente le caussali 'NFT0'
096600131022     C                   if        IVACAUSTES = 'NFT0'
096700131022     C*
096800131022     C* Reperisco i dati relativi a aliquota IVA, imponibile, importo IVA
096900131023     C                   if        IVAALIQ > wCurrIVA
097000131022     C                   eval      wCurrIVA = IVAALIQ
097100131022     C                   endif
097200131022     C*
097300131022     C                   eval      wTotIVA  = wTotIVA  + IVAIMPORTO
097400131219 xxx C***                eval      wTotIMV  = wTotIMV  + IVAIMPONIB
097500131022     C*
097600131205     C* Nei righi IVA tengo separato il bollo
097700131205     C                   if        IVARIFIVA = '150'
097800131205     C                   eval      wBOLLO = IVAIMPONIB
097900131205     C                   else
098000131023     C                   add       1             j1
098100131023     C                   eval      skIVAIMP(j1) = IVAIMPORTO
098200131023     C                   eval      skIVAALQ(j1) = IVAALIQ
098300131205     C                   eval      skIMPONB(j1) = IVAIMPONIB
098400131205     C                   endif
098500131022     C*
098600131022     C                   endif
098700131022     C*
098800131022     C     KEYiva01P     reade     ndiva01l
098900131022     C                   enddo
099000131022     C*
099100131022     C* Aggancio il partitario scadenze
099200131022     C                   eval      PASNRRIGAM = 1
099300131023     C     KEYpas01P     chain     ndpas01l
099400131022     C                   if        %found(ndpas01l)
099500131022     C*
099600131022     C* Filtro solamente le caussali 'NFT0'
099700131022     C                   if        PASCAUSRIG = 'NFT0'
099800131022     C*
099900131022     C* Reperisco il numero di giorni pagamento data fattura
100000131022     C                   eval      wPagamGG =  %diff(PASDTSCA:REGDTREG:*DAYS)
100100131022     C*
100200131022     C                   endif
100300131022     C*
100400131022     C                   endif
100500131022     C*
100600131022     C                   endif
100700131022     C                   endif
100800131022     C                   endif
100900131022     C*
101000131022     C                   ENDSR
101100131023
101200131023
101300131023     C*------------------------------------------------------------------------*
101400131023     C* RTVSVN - Routine di reperimento VARIE fatturate
101500131023     C*------------------------------------------------------------------------*
101600131023     C     rtvSvn        BEGSR
101700131023     C*
101800131023     C* Inizializzo i dettagli di fatturazione
101900131023     C                   clear                   skALC
102000131023     C                   clear                   skALC_DES
102100131023     C                   clear                   skMOA_8
102200131023     C                   clear                   skTAX_7
102300131023     C                   z-add     *zeros        j3
102400131023     C*
102500131023     C* Verifica ed eventuale scrittura del PORTO su TITAS
102600131023     C                   IF        tasPOR <> *zeros
102700131023     C                   EVAL      wSVN = '|'
102800131023     C                   EVAL      wVAN = tasPOR
102900131023     C                   EVAL      wDES = 'PORTO'
103000131023     C                   EXSR      WRISVN
103100131023     C                   ENDIF
103200131023     C*
103300131023     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
103400131023     C                   IF        tasSV1 <> *blanks
103500131023     C                   EVAL      wSVN = tasSV1
103600131023     C                   EVAL      wVAN = tasVA1
103700131023     C                   EVAL      wDES = *blanks
103800131023     C                   EXSR      WRISVN
103900131023     C                   ENDIF
104000131023     C*
104100131023     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
104200131023     C                   IF        tasSV2 <> *blanks
104300131023     C                   EVAL      wSVN = tasSV2
104400131023     C                   EVAL      wVAN = tasVA2
104500131023     C                   EVAL      wDES = *blanks
104600131023     C                   EXSR      WRISVN
104700131023     C                   ENDIF
104800131023     C*
104900131023     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
105000131023     C                   IF        tasSV3 <> *blanks
105100131023     C                   EVAL      wSVN = tasSV3
105200131023     C                   EVAL      wVAN = tasVA3
105300131023     C                   EVAL      wDES = *blanks
105400131023     C                   EXSR      WRISVN
105500131023     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
105600131023     C     KEYta730P     SETLL     tita730c
105700131023     C                   IF        %equal(tita730c)
105800131023     C     KEYta730P     READE     tita730c
105900131023     C                   DOW       not %eof(tita730c)
106000131023     C                   if        ta7VAN > *zeros
106100131023     C                   EVAL      wSVN = ta7SVN
106200131023     C                   EVAL      wVAN = ta7VAN
106300131023     C                   EVAL      wDES = *blanks
106400131023     C                   EXSR      WRISVN
106500131023     C                   endif
106600131023     C     KEYta730P     READE     tita730c
106700131023     C                   ENDDO
106800131023     C                   ENDIF
106900131023     C                   ENDIF
107000131023     C*
107100131023     C                   ENDSR
107200131023
107300131023
107400131023     C*------------------------------------------------------------------------*
107500131023     C* WRISVN - Routine di valorizzazione wrk-file VARIE fatturate
107600131023     C*------------------------------------------------------------------------*
107700131023     C     wriSvn        BEGSR
107800131023     C*
107900131023     C* Elaboro tutto tranne PORTO e la varia 8 (IVA)
108000131023     C                   if        wSVN <> '8' and wSVN <> '|'
108100131023     C                   add       1             j3
108200131023     C                   eval      skALC(j3)   = wSVN
108300131023     C                   eval      skMOA_8(j3) = wVAN
108400131023     C*
108500131023     C* Reperisco la descrizione della VARIA
108600131023     C                   if        wDES = *blanks
108700131023     C                   z-add     1             j2
108800131023     C     wSVN          lookup    skSVN(j2)                              55
108900131023     C                   if        %found
109000131025     C                   eval      skALC_DES(j3) = skSVNDES(j2)
109100131023     C                   else
109200131023     C                   eval      skALC_DES(j3) = *all'*'
109300131023     C                   endif
109400131023     C                   else
109500131025     C                   eval      skALC_DES(j3) = wDES
109600131025     C                   endif
109700131030     C*
109800131030     C* Se richiesto effettuo mapping tra charge codes BRT e quelli CLIENTE
109900131030     C                   if        w§PPTCVT = 'Y'
110000160224     C                   setoff                                       56
110100131030     C                   z-add     1             j4
110200131030     C     wSVN          lookup    skCHRG_BRT(j4)                         55
110300131030     C                   if        %found
110400160224     C                   eval      skALC_DES(j3) = skCHRG_DES(j4)
110500131030     C                   if        skCHRG_CLI(j4) <> *blanks
110600160224     C                   seton                                        56
110700131030     C                   eval      skALC(j3)     = skCHRG_CLI(j4)
110800131030     C                   endif
110900131030     C                   endif
111000160224     C*
111100160224     C* Se richiesta mappatira ma varia non contemplata "forzo" codice varia cliente "generico"
111200160224     C                   if        not *in56
111300160224     C                   eval      skALC(j3)     = 'SC'
111400160224     C                   endif
111500131030     C                   endif
111600131025     C*
111700131025     C* Verifico eventuali caratteri non consentiti dal EDIFACT
111800131025     C* ALC DES
111900131025     C                   eval      campo_alfa = skALC_DES(j3)
112000131025     C                   exsr      Char_Speciali
112100131030     C                   eval      skALC_DES(j3) = campo_alfa
112200131023     C*
112300131023     C* Reperisco aliquota IVA della VARIA corrente
112400131023     C                   z-add     1             j2
112500131023     C     wSVN          lookup    skSVNnoIVA(j2)                         55
112600131023     C                   if        %found
112700131023     C                   eval      skTAX_7(j3) = *zeros
112800131023     C                   else
112900131023     C                   eval      skTAX_7(j3) = wCurrIVA
113000131023     C                   endif
113100131023     C*
113200131023     C                   endif
113300131023     C*
113400131023     C                   clear                   wSVN
113500131023     C                   clear                   wVAN
113600131023     C                   clear                   wDES
113700131023     C*
113800131023     C                   ENDSR
113900031103
114000031103
114100131019     C     wriDettagli   BEGSR
114200131023     C*
114300131004     C* Incremento il contatore di messaggio (al interno della transazione)
114400131019     C                   eval      Conta_msg = Conta_msg + 1
114500131004     C*
114600131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
114700131025     C                   movel     *blanks       wAlfaRMA        512
114800131025     C                   movel     *blanks       wAlfaRSD        512
114900131004     C                   movel     *blanks       wAlfaLOD        512
115000131004     C                   movel     *blanks       wAlfaIND        512
115100131025     C                   movel     *blanks       wAlfaNAS        512
115200131004     C* RMA
115300131019     C                   eval      campo_alfa = vafRMA
115400131004     C                   exsr      Char_Speciali
115500131004     C                   eval      wAlfaRMA = campo_alfa
115600131025     C* RSD
115700131025     C                   eval      campo_alfa = tasRSD
115800131025     C                   exsr      Char_Speciali
115900131025     C                   eval      wAlfaRSD = campo_alfa
116000131004     C* LOD
116100131004     C                   eval      campo_alfa = tasLOD
116200131004     C                   exsr      Char_Speciali
116300131004     C                   eval      wAlfaLOD = campo_alfa
116400131004     C* IND
116500131004     C                   eval      campo_alfa = tasIND
116600131004     C                   exsr      Char_Speciali
116700131004     C                   eval      wAlfaIND = campo_alfa
116800131025     C* NAS
116900160208     C                   if        §TA4ANAS <> *blanks
117000160208     C                   eval      campo_alfa = §TA4ANAS
117100131025     C                   exsr      Char_Speciali
117200131025     C                   eval      wAlfaNAS = campo_alfa
117300131025     C                   else
117400131025     C                   eval      wAlfaNAS = 'VOID'
117500131025     C                   endif
117600131004     C*
117700131004     C*
117800131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
117900131019     C*
118000131019     C*  LIN -
118100131019     C                   eval      Conta_segm  = Conta_segm  + 1
118200131019     C                   eval      vawDTA = LIN_sgm
118300131022     C                              + %trim(%editc(Conta_item:'3'))
118400131019     C                              +   apice
118500131019     C                   exsr      wriOUTPUT
118600160217     C                   eval      vawDTA_Acc = vawDTA
118700160217     C                   exsr      CpyAccorpa
118800131019     C*
118900131019     C*  IMD -
119000131019     C                   eval      Conta_segm  = Conta_segm  + 1
119100131019     C                   eval      vawDTA = IMD_sgm
119200131019     C                              +   apice
119300131019     C                   exsr      wriOUTPUT
119400160217     C                   eval      vawDTA_Acc = vawDTA
119500160217     C                   exsr      CpyAccorpa
119600131019     C*
119700131019     C*  MOA 203 -
119800131019     C                   eval      Conta_segm  = Conta_segm  + 1
119900131019     C                   eval      vawDTA = MOA_203_sgm
120000131022     C                              + %trim(%editc(vafIMV:'3'))
120100131219     C                              +   apice
120200131019     C                   exsr      wriOUTPUT
120300160217     C                   eval      vawDTA_Acc = vawDTA
120400160217     C                   exsr      CpyAccorpa
120500131219     C                   eval      wTotIMV  = wTotIMV + vafIMV                  * Tot MOA+203 per 79
120600131019     C*
120700131019     C*  PRI -
120800160204     C*
120900160204     C* Ritorno il valore reale del PORTO solo se spedizione NON di "reso mittente"
121000160201     C                   if        tasFBR = *blanks
121100131019     C                   eval      Conta_segm  = Conta_segm  + 1
121200131019     C                   eval      vawDTA = PRI_sgm
121300131022     C                              + %trim(%editc(vafPOR:'3'))
121400131019     C                              +   apice
121500131019     C                   exsr      wriOUTPUT
121600160217     C                   eval      vawDTA_Acc = vawDTA
121700160217     C                   exsr      CpyAccorpa
121800160204     C*
121900160204     C* Se trattasi di spedizione di "reso mittente" invece ritorno il porto a zero
122000160204     C                   else
122100160204     C*
122200160204     C                   eval      Conta_segm  = Conta_segm  + 1
122300160204     C                   eval      vawDTA = PRI_sgm
122400160204     C                              + '0'
122500160204     C                              +   apice
122600160204     C                   exsr      wriOUTPUT
122700160217     C                   eval      vawDTA_Acc = vawDTA
122800160217     C                   exsr      CpyAccorpa
122900160204     C*
123000160201     C                   endif
123100131019     C*
123200131019     C*  TAX -
123300131019     C                   eval      Conta_segm  = Conta_segm  + 1
123400131019     C                   eval      vawDTA = TAX_sgm
123500131022     C                              + %trim(%editc(wCurrIVA:'3'))
123600131019     C                              +   apice
123700131019     C                   exsr      wriOUTPUT
123800160217     C                   eval      vawDTA_Acc = vawDTA
123900160217     C                   exsr      CpyAccorpa
124000131018     C*
124100131019     C*  NAD CN -
124200131004     C                   eval      Conta_segm  = Conta_segm  + 1
124300131018     C                   eval      vawDTA = NAD_CN
124400131025     C                              + %trim(wAlfaRSD)
124500131004     C                              +   piu
124600131004     C                              +   piu
124700131004     C                              + %trim(wAlfaIND)
124800131004     C                              +   piu
124900131004     C                              + %trim(wAlfaLOD)
125000131004     C                              +   piu
125100131004     C                              + %trim(tasPRD)
125200131004     C                              +   piu
125300131004     C                              + %trim(tasCAD)
125400131004     C                              +   piu
125500131004     C                              + 'IT'
125600131004     C                              +   apice
125700131019     C                   exsr      wriOUTPUT
125800160217     C                   eval      vawDTA_Acc = vawDTA
125900160217     C                   exsr      CpyAccorpa
126000131019     C*
126100131022     C*  RFF AAM -
126200131019     C                   eval      Conta_segm  = Conta_segm  + 1
126300131019     C                   eval      vawDTA = RFF_AAM_sgm
126400131025     C                              + %trim(wAlfaNAS)
126500131019     C                              +   apice
126600131019     C                   exsr      wriOUTPUT
126700160217     C                   eval      vawDTA_Acc = vawDTA
126800160217     C                   exsr      CpyAccorpa
126900131019     C*
127000131019     C*  RFF CR -
127100131019     C                   eval      Conta_segm  = Conta_segm  + 1
127200131019     C                   eval      vawDTA = RFF_CR_sgm
127300131022     C                              + %trim(%editc(vafRMN:'3'))
127400131019     C                              +   apice
127500131019     C                   exsr      wriOUTPUT
127600160217     C                   eval      vawDTA_Acc = vawDTA
127700160217     C                   exsr      CpyAccorpa
127800131019     C*
127900131019     C*  RFF ACC -
128000131019     C                   eval      Conta_segm  = Conta_segm  + 1
128100131019     C                   eval      vawDTA = RFF_ACC_sgm
128200131025     C                              + %trim(wAlfaRMA)
128300131019     C                              +   apice
128400131019     C                   exsr      wriOUTPUT
128500160217     C                   eval      vawDTA_Acc = vawDTA
128600160217     C                   exsr      CpyAccorpa
128700131019     C*
128800131019     C*  DTM 200 -
128900131019     C                   eval      Conta_segm  = Conta_segm  + 1
129000131019     C                   eval      vawDTA = DTM_200_1
129100131019     C                              + %trim(%editc(tasDRT:'X'))
129200131019     C                              + DTM_200_2
129300131019     C                              +   apice
129400131019     C                   exsr      wriOUTPUT
129500160217     C                   eval      vawDTA_Acc = vawDTA
129600160217     C                   exsr      CpyAccorpa
129700131023     C*
129800131023     C* Sviluppo tutti i dettagli di fatturazione
129900160202     C* Ritorno le VARIE solo se spedizione NON di "reso mittente"
130000160201     C                   if        tasFBR = *blanks
130100131023     C                   eval      j3 = 1
130200131023     C                   dow       j3 <= %elem(skALC) and skALC(j3) <> *blanks
130300131023     C*
130400131023     C* Restituisco solamente le varie con importo
130500131023     C                   if        skMOA_8(j3) > *zeros
130600131023     C*
130700131023     C*  ALC -
130800131023     C                   eval      Conta_segm  = Conta_segm  + 1
130900131023     C                   eval      vawDTA = ALC_sgm
131000131023     C                              + %trim(skALC(j3))
131100131023     C                              +   diviso
131200131023     C                              +   diviso
131300131023     C                              +   diviso
131400131030     C                              + %trim(skALC_DES(j3))
131500131023     C                              +   apice
131600131023     C                   exsr      wriOUTPUT
131700160217     C                   eval      vawDTA_Acc = vawDTA
131800160217     C                   exsr      CpyAccorpa
131900131023     C*
132000131023     C*  MOA 8 -
132100131023     C                   eval      Conta_segm  = Conta_segm  + 1
132200131023     C                   eval      vawDTA = MOA_8_sgm
132300131023     C                              + %trim(%editc(skMOA_8(j3):'3'))
132400131023     C                              +   apice
132500131023     C                   exsr      wriOUTPUT
132600160217     C                   eval      vawDTA_Acc = vawDTA
132700160217     C                   exsr      CpyAccorpa
132800131023     C*
132900131023     C*  TAX 7 -
133000131023     C                   eval      Conta_segm  = Conta_segm  + 1
133100131023     C                   eval      vawDTA = TAX_sgm
133200131023     C                              + %trim(%editc(skTAX_7(j3):'3'))
133300131023     C                              +   apice
133400131023     C                   exsr      wriOUTPUT
133500160217     C                   eval      vawDTA_Acc = vawDTA
133600160217     C                   exsr      CpyAccorpa
133700131023     C*
133800131023     C                   endif
133900131023     C                   add       1             j3
134000131023     C                   enddo
134100160202     C*
134200160202     C* Se trattasi di spedizione di "reso mittente"
134300160202     C                   else
134400160202     C*
134500160202     C*  ALC -
134600160202     C                   eval      Conta_segm  = Conta_segm  + 1
134700160202     C                   eval      vawDTA = ALC_sgm
134800160202     C                              + 'UD'
134900160202     C                              +   diviso
135000160202     C                              +   diviso
135100160202     C                              +   diviso
135200160202     C                              + 'RETURN TO SENDER'
135300160202     C                              +   apice
135400160202     C                   exsr      wriOUTPUT
135500160217     C                   eval      vawDTA_Acc = vawDTA
135600160217     C                   exsr      CpyAccorpa
135700160202     C*
135800160202     C*  MOA 8 -
135900160202     C                   eval      Conta_segm  = Conta_segm  + 1
136000160202     C                   eval      vawDTA = MOA_8_sgm
136100160202     C                              + %trim(%editc(vafIMV:'3'))
136200160202     C                              +   apice
136300160202     C                   exsr      wriOUTPUT
136400160217     C                   eval      vawDTA_Acc = vawDTA
136500160217     C                   exsr      CpyAccorpa
136600160202     C*
136700160202     C*  TAX 7 -
136800160202     C                   eval      Conta_segm  = Conta_segm  + 1
136900160202     C                   eval      vawDTA = TAX_sgm
137000160202     C                              + %trim(%editc(wCurrIVA:'3'))
137100160202     C                              +   apice
137200160202     C                   exsr      wriOUTPUT
137300160217     C                   eval      vawDTA_Acc = vawDTA
137400160217     C                   exsr      CpyAccorpa
137500160202     C*
137600160201     C                   endif
137700031103     C*
137800031103     C                   ENDSR
137900160208
138000160208
138100160208
138200160208     C     RTVNAS        BEGSR
138300160208     C*
138400160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
138500160208     C                   CLEAR                   DTA4A
138600160208     C*
138700160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
138800160208     C                   CALL      'UBTA400R'
138900160208     C                   PARM      *blanks       UBTA4IOPZ
139000160208     C                   PARM      *blanks       UBTA4ITLA
139100160208     C                   PARM      tasAAS        UBTA4IAAS
139200160208     C                   PARM      tasLNP        UBTA4ILNP
139300160208     C                   PARM      tasNRS        UBTA4INRS
139400160208     C                   PARM      tasNSP        UBTA4INSP
139500160208     C                   PARM      'A'           UBTA4ITRC
139600160208     C                   PARM                    UBTA4OERR
139700160208     C                   PARM                    UBTA4ODS
139800160208     C                   PARM                    UBTA4OLEN
139900160208     C                   PARM                    UBTA4ODATI
140000160208     C*
140100160208     C                   IF        UBTA4OERR = *zeros
140200160208     C                   SELECT
140300160208     C* Gestione output tipo record 'A'
140400160208     C                   WHEN      UBTA4ODS = 'DTA4A'
140500160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
140600160208     C                   ENDSL
140700160208     C*
140800160208     C                   ENDIF
140900160208     C*
141000160208     C                   ENDSR
141100160217
141200160217
141300160217
141400160217     C     CpyAccorpa    BEGSR
141500160217     C*
141600160217     C                   ADD       1             jCpyAcc
141700160217     C                   EVAL      skCpyAccorpa(jCpyAcc) = vawDTA_Acc
141800160217     C*
141900160217     C                   ENDSR
142000160217
142100160217
142200160217
142300160218     C     DeAccorpa     BEGSR
142400160218     C*
142500160217     C* Scorro tutte le spedizioni accorpate sulla spedizone corrente
142600160217     C     KEYrab01P     SETLL     firab01l
142700160217     C                   IF        %equal(firab01l)
142800160217     C     KEYrab01P     READE     firab01l
142900160217     C                   DOW       not %eof(firab01l)
143000160217     C*
143100160217     C* Controllo (ridondante) su riferimenti spedizione accorpante
143200160217     C                   IF        wDepRMN = rabRMN AND
143300160217     C                             wDepRMA = rabRMA
143400160217     C*
143500160217     C* Considero solamente spedizioni ACCORPATE
143600160217     C                   IF        rabRMN = rabRMNR AND
143700160217     C                             rabRMA = rabRMAR
143800160217     C                   ELSE
143900160217     C*
144000160217     C* Per ogni spedizione ACCORPATA "duplico" la spedizione ACCORPANTE
144100160217     C                   Z-ADD     1             jCpyAcc
144200160217     C                   DOW       jCpyAcc <= %elem(skCpyAccorpa) AND
144300160217     C                             skCpyAccorpa(jCpyAcc) <> *blanks
144400160217     C*
144500160217     C* Incremento il contatore dei segmenti
144600160217     C                   eval      Conta_segm  = Conta_segm  + 1
144700160217     C*
144800160217     C* Effettuo sostuituzione di valori rispetto bolla ACCORPANTE e sue ACCORPATE
144900160217     C                   SELECT
145000160217     C*
145100160218     C* LIN (per contatori item (ovvero spedizione)
145200160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(LIN_sgm))
145300160218     C                             = LIN_sgm
145400160218     C                   add       1             Conta_item
145500160218     C                   eval      vawDTA = LIN_sgm
145600160218     C                              + %trim(%editc(Conta_item:'3'))
145700160218     C                              +   apice
145800160218     C*
145900160218     C* RFF CR - (per RMN)
146000160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
146100160218     C                             RFF_CR_sgm)) = RFF_CR_sgm
146200160218     C                   eval      vawDTA = RFF_CR_sgm
146300160217     C                              + %trim(%editc(rabRMNR:'4'))
146400160217     C                              +   apice
146500160218     C*
146600160218     C* RFF ACC - (per RMA)
146700160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
146800160218     C                             RFF_ACC_sgm)) = RFF_ACC_sgm
146900160218     C                   eval      campo_alfa = rabRMAR
147000160218     C                   exsr      Char_Speciali
147100160218     C                   eval      wAlfaRMA = campo_alfa
147200160218     C                   eval      vawDTA = RFF_ACC_sgm
147300160218     C                              + %trim(wAlfaRMA)
147400160218     C                              +   apice
147500160218     C*
147600160218     C*  MOA 203 -
147700160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
147800160218     C                             MOA_203_sgm)) = MOA_203_sgm
147900160218     C                   eval      vawDTA = MOA_203_sgm
148000160218     C                              + '0'
148100160218     C                              +   apice
148200160218     C*
148300160218     C*  PRI -
148400160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(PRI_sgm))
148500160218     C                             = PRI_sgm
148600160218     C                   eval      vawDTA = PRI_sgm
148700160218     C                              + '0'
148800160218     C                              +   apice
148900160218     C*
149000160218     C*  MOA 8 -
149100160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
149200160218     C                             MOA_8_sgm)) = MOA_8_sgm
149300160218     C                   eval      vawDTA = MOA_8_sgm
149400160218     C                              + '0'
149500160218     C                              +   apice
149600160218     C*
149700160217     C                   OTHER
149800160217     C*
149900160217     C* Tutti gli altri segmenti "AS IS"
150000160217     C                   eval      vawDTA = skCpyAccorpa(jCpyAcc)
150100160217     C                   ENDSL
150200160217     C*
150300160217     C* Scarico l'ouput di "duplicazione"
150400160217     C                   exsr      wriOUTPUT
150500160217     C*
150600160217     C                   ADD       1             jCpyAcc
150700160217     C                   ENDDO
150800160217     C*
150900160217     C                   ENDIF
151000160217     C                   ENDIF
151100160217     C*
151200160217     C     KEYrab01P     READE     firab01l
151300160217     C                   ENDDO
151400160217     C                   ENDIF
151500160217     C*
151600160217     C                   ENDSR
151700031103
151800160208
151900991027
152000991027      /TITLE Operazioni iniziali.
152100991027     C     *inzsr        BEGSR
152200991027     C*
152300991027     C     *ENTRY        PLIST
152400131019     C                   parm                    prmppt
152500991027     C     wrkesito      parm      wrkesito      prmesito
152600031103     C*
152700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
152800131019     C                   EVAL      DSPPT = prmppt
152900131004     C*
153000131004     C* Reperisco i valori variabili ma a livello di transazione corrente
153100131004     C* (tipicamente data e ora relativ al file generato)
153200131004     C                   eval      currDate6    =
153300131004     C                              UBFMTDATE_Convert(
153400131004     C                                  %editc(%dec(%date():*ISO):'X') :
153500131004     C                                  'YYYYMMDD' : 'YYMMDD')
153600131004     C                   eval      currDate6inv =
153700131004     C                              UBFMTDATE_Convert(
153800131004     C                                  %editc(%dec(%date():*ISO):'X') :
153900131004     C                                  'YYYYMMDD' : 'DDMMYY')
154000131018     C                   eval      currDate8 =
154100131018     C                              UBFMTDATE_Convert(
154200131018     C                                  %editc(%dec(%date():*ISO):'X') :
154300131023     C                                  'YYYYMMDD' : 'YYYYMMDD')
154400131004     C                   eval      currHHMM =
154500131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
154600030709     C*
154700030709     C* Definizione chiavi
154800131023     C*
154900131023     C* LETTURA tabel00f - PARZIALE
155000131023     C     KEYtab00_P1   KLIST
155100131023     C                   KFLD                    tblKUT
155200131023     C                   KFLD                    tblCOD
155300031103     C*
155400031103     C* Chiave su TITAS30C - Parziale
155500031103     C     KEYtas30P     KLIST
155600131019     C                   KFLD                    vafAAS
155700131019     C                   KFLD                    vafLNP
155800131019     C                   KFLD                    vafNRS
155900131019     C                   KFLD                    vafNSP
156000131023     C*
156100131023     C* LETTURA tita730c - PARZIALE
156200131023     C     KEYta730P     KLIST
156300131023     C                   KFLD                    tasAAS
156400131023     C                   KFLD                    tasLNP
156500131023     C                   KFLD                    tasNRS
156600131023     C                   KFLD                    tasNSP
156700131023     C                   KFLD                    tasTBL
156800131022     C*
156900131022     C* Chiave su NDREG04L - Completa
157000131022     C     KEYreg04C     KLIST
157100131022     C                   KFLD                    REGSOCIETA
157200131022     C                   KFLD                    REGCTB
157300131022     C                   KFLD                    REGDTREG
157400131022     C                   KFLD                    REGNRREG
157500131022     C                   KFLD                    REGSERREG
157600131022     C*
157700131022     C* Chiave su NDIVA01L - Parziale
157800131022     C     KEYiva01P     KLIST
157900131023     C                   KFLD                    REGSYS
158000131023     C                   KFLD                    REGNRASREG
158100131022     C*
158200131022     C* Chiave su NDPAS01L - Parziale
158300131022     C     KEYpas01P     KLIST
158400131023     C                   KFLD                    REGSYS
158500131023     C                   KFLD                    REGNRASREG
158600131022     C                   KFLD                    PASNRRIGAM
158700160217     C*
158800160217     C* Chiave su FIRAB01L - Parziale
158900160217     C     KEYrab01P     KLIST
159000160218     C                   KFLD                    tasAAS
159100160218     C                   KFLD                    tasLNP
159200160218     C                   KFLD                    tasNRS
159300160218     C                   KFLD                    tasNSP
159400991027     C*
159500991027     C                   ENDSR
159600131030** CHRGCODES - CHARGE CODES BRT / CLIENTE
159700160127+         SUPPL.CARBURANTE                                                                  01
159800160127&    SC   ANN.P/ASSEGNATO                                                                   02
159900160127*    SC   DIROTTAM.TO                                                                       03
160000160127=    SC   RESO BANCALI                                                                      04
160100160127a    DCC  POD IMAGE                                                                         05
160200160127b    SC   AD.GEST.C.                                                                        06
160300160127c    SC   L.AVVISO                                                                          07
160400160127d    INS  A.C. BASE                                                                         08
160500160127e    SC   PRIORITY                                                                          09
160600160127f    FSC  FUEL SURCH                                                                        10
160700160127h    SC   H 10:30                                                                           11
160800160127k         PACKING L.                                                                        11
160900160127m    SC   AV.AFFID.                                                                         12
161000160127p         PINcode                                                                           11
161100160127t         PRENOT.RITIRO                                                                     11
161200160127z         EXPO                                                                              11
161300160127A    EDC  APPUNTAMENTO                                                                      13
161400160127B    DCC  CONS.DDT                                                                          14
161500160127C    SC   FACCH.ARR.                                                                        15
161600160127D    SC   INV/INC FT                                                                        16
161700160127E         R.A.RESP.VETT.                                                                    17
161800160127F    OSC  FUORI MISURA AZ                                                                   18
161900160127G    COD  COMP.ASSEGNO                                                                      19
162000160127H    SC   DIR.FISSO                                                                         20
162100160127I    HLD  SP.GIACENZA                                                                       21
162200160127J    RAS  ISOLA                                                                             22
162300160127K    RAS  LOC.DISAG.                                                                        23
162400160127L    SC   I.S.T.A.T.                                                                        24
162500160127M    SC   SPESE INSOLUTO                                                                    25
162600160127N    SC   ANTEPORTO                                                                         26
162700160127O    SC   RIC.DOCUM.                                                                        27
162800160127P    AHC  AI PIANI                                                                          28
162900160127Q    DAS  ZTL                                                                               29
163000160127R    INS  A.C. PLUS                                                                         30
163100160127S    SC   SUPER./GDO                                                                        31
163200160127T    SC   RIT.COMMISSIONATO                                                                 32
163300160127U    SC   RITIRO                                                                            33
163400160127W    COD  REC.C/ASSEGNI                                                                     34
163500160127X    DAS  CONS.DISAG.                                                                       35
163600160127Y    SC   RIT.ANNULL.                                                                       36
163700160127Z    FSC  AD.GEST.AP                                                                        37
1638001601272    SC   INOLTRO                                                                           38
1639001601273         A.R.                                                                              39
1640001601274         FRAGILE                                                                           40
1641001601275    SC   DIR.PESAT.                                                                        41
1642001601276    SDU  BOLLO                                                                             42
1643001601277         T.C.P.                                                                            43
1644001601278    65   I.V.A.                                                                            44
