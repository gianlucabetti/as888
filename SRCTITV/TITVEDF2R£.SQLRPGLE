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
004100170906     D wIVA            S             13  4 inz
004200131030     D wDES            S             40    inz
004300131205     D wBOLLO          S             19  3 inz
004400160217     D wDepRMN         S                   like(vafRMN) inz
004500160217     D wDepRMA         S                   like(vafRMA) inz
004600160217     D vawDTA_Acc      S                   like(vawDTA) inz
004700170915     D wVAL2D          S             19  2 inz
004800171019     D wVAL3D          S             19  3 inz
004900131004
005000131004
005100131004     D*-----------
005200131004     D* DEFINIZIONE DS ESTERNE
005300131004     D*-----------
005400131004     D trul33ds      e ds                  inz
005500131004     D kpjba         e ds                  inz
005600131019     D tivafds       e ds                  inz extname(tivaf00t)
005700131023     D dscc          e ds                  inz
005800160208     D DTA4A         e ds
005900131022
006000131022
006100131022     D*------------
006200131022     D* SCHIERE DETTAGLI REGISTRAZIONE CONTABILE
006300131022     D*------------
006400131023     D skIVAIMP        S             19  3 dim(100)
006500131023     D skIVAALQ        S              5  2 dim(100)
006600131022     D skIMPONB        S             19  3 dim(100)
006700131023     D j1              S              3  0 inz
006800131030     D skSVN           S              1    DIM(100)
006900131030     D skSVNDES        S             40    DIM(100)
007000131030     D skSVNnoIVA      S              1    DIM(100)
007100131023     D j2              S              3  0 inz
007200131030     D skALC           S              5    DIM(100)
007300131030     D skALC_DES       S             40    DIM(100)
007400131023     D skMOA_8         S             19  3 DIM(100)
007500131023     D skTAX_7         S              5  2 DIM(100)
007600170906     D skMOA_124       S             19  4 DIM(100)
007700131023     D j3              S              3  0 inz
007800131030     D skCHRG_BRT      S              1    DIM(100)
007900131030     D skCHRG_CLI      S              5    DIM(100)
008000131030     D skCHRG_DES      S             40    DIM(100)
008100131030     D j4              S              3  0 inz
008200160217     D skCpyAccorpa    S                   LIKE(vawDTA) DIM(5000) INZ
008300160217     D jCpyAcc         S              5  0 INZ(*zeros)
008400131022
008500131004
008600131004     D*------------------
008700131004     D* LINKING A DEFINIZIONI ESTERNE
008800131004     D*------------------
008900131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
009000131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
009100160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
009200131021     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
009300131021      /DEFINE DFTACTGRP_YES
009400131021     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
009500131021      /UNDEFINE DFTACTGRP_YES
009600131021
009700131021     ***************************************************************************
009800131021     **
009900131021     ** Definizione strutture dati.
010000131021     **
010100131021      **************************************************************************
010200131021     D tibsSocI0...
010300131021     D               E DS                  QUALIFIED
010400131021     D                                     INZ
010500131021     D tibsSocO0...
010600131021     D               E DS                  QUALIFIED
010700131021     D                                     INZ
010800131021     ***************************************************************************
010900131021     **
011000131021     ** Definizione variabili modulo/programma.
011100131021     **
011200131021     ***************************************************************************
011300131021     D prmRqsOpCode...
011400131021     D                 S             10A
011500131021     D prmRpyOpCode...
011600131021     D                 S             10A
011700131021     D prmRpyIdMsg...
011800131021     D                 S             10I 0
011900131021     D prmRqsFormato...
012000131021     D                 S             10A
012100131021     D prmRqsDataSize...
012200131021     D                 S             10I 0
012300131021     D prmRpyFormato...
012400131021     D                 S             10A
012500131021     D prmRpyDataSize...
012600131021     D                 S             10I 0
012700110705
012800031103
012900031103     D*-----------
013000031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
013100031103     D*-----------
013200131019     D DSPPT           DS
013300131030     D  w§PPTKSU                      7  0
013400131030     D  w§PPTCVT                      1
013500131018     D  w§UNB_ID_1                    5
013600131018     D  w§UNB_ID_2                   25
013700131004
013800131030
013900131030     D*-----------
014000131030     D* SCHIERE A PROGRAMMA
014100131030     D*-----------
014200160128     D CHRGCODES       S             60    DIM(48) CTDATA PERRCD(1)
014300131030
014400131030
014500131030     D*-----------
014600131030     D* RIDEFINIZIONE SCHIERA A PROGRAMMA CHRGCODES
014700131030     D*-----------
014800131030     D DS_CHRGCODE     DS
014900131030     D  CHRGCODE_BRT                  5
015000131030     D  CHRGCODE_CLI                  5
015100131030     D  CHRGCODE_DES                 40
015200131004
015300131030
015400131004     D*-----------
015500131018     D* COSTANTI / DEFAULT
015600131004     D*-----------
015700131018     D UNB_sgm         C                   CONST('UNB+UNOA:1+')
015800131007     D UNH_sgm_1       C                   CONST('UNH+')
015900131018     D UNH_sgm_2       C                   CONST('+INVOIC:D:96A:UN:EAN008')
016000131018     D BGM_sgm_1       C                   CONST('BGM+380+')
016100131018     D BGM_sgm_2       C                   CONST('+9')
016200131018     D DTM_3_1         C                   CONST('DTM+3:')
016300131018     D DTM_3_2         C                   CONST(':102')
016400131018     D DTM_137_1       C                   CONST('DTM+137:')
016500131018     D DTM_137_2       C                   CONST(':102')
016600131018     D RFF_IT_sgm      C                   CONST('RFF+IT:')
016700131021     D NAD_CA_1        C                   CONST('NAD+CA+')
016800131021     D NAD_CA_2        C                   CONST('BRT S.P.A.+PIAZZA DIAZ, 7+')
016900131021     D NAD_CA_3        C                   CONST('MILANO++20123+IT')
017000150309     D RFF_VA_1_sgm    C                   CONST('RFF+VA:IT04507990150')
017100131018     D NAD_SF_1        C                   CONST('NAD+SF+5450534001540::9++')
017200131128     D NAD_SF_2        C                   CONST('AMAZON EU SARL:')
017300131128     D NAD_SF_3        C                   CONST('CO AMAZON LOGISTICA SRL+')
017400131128     D NAD_SF_4        C                   CONST('STRADA DOGANA PO 2 T')
017500131128     D NAD_SF_5        C                   CONST('CASTEL SAN GIOVANNI++')
017600131128     D NAD_SF_6        C                   CONST('29015+IT')
017700140505     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000345::9++')
017800140505     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
017900140505     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
018000140505     D*NAD_IV_4        C                   CONST('AMAZON IT LOGISTICA SRL:')
018100140505     D*NAD_IV_5        C                   CONST('VIA FERR. APORTI 8 - ')
018200140505     D*NAD_IV_6        C                   CONST('IT07231660965+')
018300140505     D*NAD_IV_7        C                   CONST('MILANO++20125+IT')
018400140508     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000345::9++')
018500140508     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
018600140508     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
018700140508     D*NAD_IV_4        C                   CONST('PI IT05257010966:')
018800140508     D*NAD_IV_5        C                   CONST('RAPPR FISCALE AMAZON ITALIA:')
018900140508     D*NAD_IV_6        C                   CONST('LOGISTICA SRL VIA FERRANTE')
019000140508     D*NAD_IV_7        C                   CONST('APORTI 8+MILANO++20125+IT')
019100140516     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000055::9++')
019200140516     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
019300140516     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
019400140516     D*NAD_IV_4        C                   CONST('CO ACCOUNTING SERVICE CENTER')
019500140516     D*NAD_IV_5        C                   CONST(':1-9 THE GROVE+SLOUGH++')
019600140516     D*NAD_IV_6        C                   CONST('SL1 1QP+GB')
019700140516     D*NAD_IV_7        C                   CONST('')
019800150309     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000055::9++')
019900150309     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:5 RUE PLAETIS')
020000150309     D*NAD_IV_3        C                   CONST(' L-2338 LUXEMBOURG+C/O AP')
020100150309     D*NAD_IV_4        C                   CONST(' NON-INVENTORY FINANCIAL')
020200150309     D*NAD_IV_5        C                   CONST(' OPS:1-9 THE GROVE+')
020300150309     D*NAD_IV_6        C                   CONST('SLOUGH++SL1 1QP+GB')
020400150309     D*NAD_IV_7        C                   CONST('')
020500150309     D NAD_IV_1        C                   CONST('NAD+IV+5450534005845::9++')
020600150309     D NAD_IV_2        C                   CONST('AMAZON EU SARL, ')
020700150309     D NAD_IV_3        C                   CONST('SUCCURSALE ITALIANA+')
020800150309     D NAD_IV_4        C                   CONST('VIA FERRANTE APORTI 8+')
020900150309     D NAD_IV_5        C                   CONST('MILANO++20125+IT')
021000150309     D NAD_IV_6        C                   CONST('')
021100140516     D NAD_IV_7        C                   CONST('')
021200171005     D NAD_IV_1_2      C                   CONST('NAD+IV+5450534006941::9++')
021300171005     D NAD_IV_2_2      C                   CONST('AMAZON EU SARL, ')
021400171005     D NAD_IV_3_2      C                   CONST('SUCCURSALE ITALIANA+')
021500171005     D NAD_IV_4_2      C                   CONST('VIALE MONTE GRAPPA 3/5+')
021600171005     D NAD_IV_5_2      C                   CONST('MILANO++20124+IT')
021700171005     D NAD_IV_6_2      C                   CONST('')
021800171005     D NAD_IV_7_2      C                   CONST('')
021900150309     D*RFF_VA_2_sgm    C                   CONST('RFF+VA:LU20260743')
022000150309     D RFF_VA_2_sgm    C                   CONST('RFF+VA:IT08973230967')
022100131018     D CUX_sgm         C                   CONST('CUX+2:EUR:4')
022200131022     D PAT_sgm         C                   CONST('PAT+1++5::D:')
022300131019     D PCD_sgm         C                   CONST('PCD+7:0:13')
022400131019     D LIN_sgm         C                   CONST('LIN+')
022500131023     D IMD_sgm         C                   CONST('IMD+F+8+:::BRT_IT')
022600140407     D IMD_sgm_Bollo   C                   CONST('IMD+F+8+:::STAMP DUTY')
022700170411     D MEA_WT_sgm      C                   CONST('MEA+WT+G+KG:')
022800131019     D MOA_203_sgm     C                   CONST('MOA+203:')
022900131019     D PRI_sgm         C                   CONST('PRI+CAL:')
023000131022     D TAX_sgm         C                   CONST('TAX+7+VAT+++:::')
023100131019     D NAD_CN          C                   CONST('NAD+CN++')
023200131019     D RFF_AAM_sgm     C                   CONST('RFF+AAM:')
023300131019     D RFF_CR_sgm      C                   CONST('RFF+CR:')
023400131019     D RFF_ACC_sgm     C                   CONST('RFF+ACC:')
023500131019     D DTM_200_1       C                   CONST('DTM+200:')
023600131019     D DTM_200_2       C                   CONST(':102')
023700131023     D ALC_sgm         C                   CONST('ALC+C++++')
023800140407     D ALC_sgm_Bollo   C                   CONST('ALC+C++++SDU:::STAMP DUTY')
023900131023     D MOA_8_sgm       C                   CONST('MOA+8:')
024000131023     D UNS_sgm         C                   CONST('UNS+S')
024100131019     D CNT_2_sgm       C                   CONST('CNT+2:')
024200131019     D MOA_79_sgm      C                   CONST('MOA+79:')
024300131019     D MOA_58_sgm      C                   CONST('MOA+58:')
024400131019     D MOA_77_sgm      C                   CONST('MOA+77:')
024500131205     D MOA_124_sgm     C                   CONST('MOA+124:')
024600131205     D MOA_125_sgm     C                   CONST('MOA+125:')
024700131004     D UNT_sgm         C                   CONST('UNT+')
024800131004     D UNZ_sgm         C                   CONST('UNZ+')
024900131004     D piu             C                   CONST('+')
025000131004     D diviso          C                   CONST(':')
025100131004     D apice           C                   CONST('''')
025200131022     D societa         C                   CONST('201')
025300131021     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
025400131021     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
025500131004
025600030924
025700030325
025800030924     C* MAIN/
025900170123     C*
026000170123     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
026100170123     C
026200170123     C/EXEC SQL
026300170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
026400170123     C/END-EXEC
026500131021     C*
026600131021     C* Inizializzo il TIBSSOCR
026700131025     C                   SETOFF                                       2021
026800131021     C                   SETON                                        20
026900131021     C                   EXSR      EXESOCR
027000041015     C*
027100041015     C* Carica i dati tabellati
027200041015     C                   EXSR      cartab
027300041015     C*
027400041015     C* Esegue traduzione
027500131004     C                   setoff                                       31
027600131004     C  N31              EXSR      chkRECFILMBR
027700131004     C  N31              EXSR      rtvID
027800131004     C  N31              EXSR      wriHeader
027900131004     C  N31              EXSR      traduci
028000131004     C  N31              EXSR      wriFooter
028100131004 xxx C***31              EXSR      sndAlert
028200131021     C*
028300131021     C* Chiudo il TIBSSOCR
028400131025     C                   SETOFF                                       2021
028500131021     C                   SETON                                        21
028600131021     C                   EXSR      EXESOCR
028700160208     C*
028800160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
028900160208     C                   CALL      'UBTA400R'
029000160208     C                   PARM                    UBTA4IOPZ
029100160208     C                   PARM      'C'           UBTA4ITLA
029200160208     C                   PARM                    UBTA4IAAS
029300160208     C                   PARM                    UBTA4ILNP
029400160208     C                   PARM                    UBTA4INRS
029500160208     C                   PARM                    UBTA4INSP
029600160208     C                   PARM                    UBTA4ITRC
029700160208     C                   PARM                    UBTA4OERR
029800160208     C                   PARM                    UBTA4ODS
029900160208     C                   PARM                    UBTA4OLEN
030000160208     C                   PARM                    UBTA4ODATI
030100110705     C*
030200921023     C                   SETON                                        LR
030300131004
030400131004
030500131004
030600131004     C*----------------------------------------------------*
030700131004     C*  STACCO ID UNIVOCO
030800131004     C*----------------------------------------------------*
030900131004     C     RTVID         BEGSR
031000131004     C*
031100131004     C                   z-add     *zeros        currUNIQ_ID      15 0
031200131004     C*
031300131004     C* NSP => Stacco un numeratore da AZNUM
031400131004     C                   clear                   TRUL33DS
031500131004     C                   eval      I33OPE = *zeros
031600131004     C                   eval      I33CNU = 302
031700131004     C                   eval      I33NUM = 1
031800131004     C                   movel     TRUL33DS      KPJBU
031900131004     C                   call      'TRUL33R'
032000131004     C                   parm                    KPJBA
032100131004     C                   movel     KPJBU         TRUL33DS
032200131004     C                   if        O33ERR = *zeros
032300131004     C                   z-add     O33NRF        currUNIQ_ID
032400131004     C                   else
032500131004     C                   seton                                        31
032600131004     C                   endif
032700131004     C*
032800131004     C                   ENDSR
032900131021
033000131021
033100131021     C*----------------------------------------------------*
033200131021     C*  INIZIALIZZO / CHIUDO IL TIBSSOCR
033300131021     C*----------------------------------------------------*
033400131021     C     EXESOCR       BEGSR
033500131021     C*
033600131021     C* Inizializzo il programma.
033700131021     C                   IF        *IN20
033800131021     C                   CALL      'TIBSSOCR'
033900131021     C                   PARM      'INIT'        prmRqsOpCode
034000131021     C                   PARM                    prmRpyOpCode
034100131021     C                   PARM                    prmRpyIdMsg
034200131021     C                   ENDIF
034300131021     C*
034400131021     C* Chiudo il programma.
034500131021     C                   IF        *IN21
034600131021     C                   CALL      'TIBSSOCR'
034700131021     C                   PARM      'FINALIZE'    prmRqsOpCode
034800131021     C                   PARM                    prmRpyOpCode
034900131021     C                   PARM                    prmRpyIdMsg
035000131021     C                   ENDIF
035100131021     C*
035200131021     C                   ENDSR
035300131004
035400131021
035500131004     C     chkRECFILMBR  BEGSR
035600131004     C*
035700131004     C     *start        setll     TIVAWWWT
035800131004     C                   if        %found(TIVAWWWT)
035900131004     C                   seton                                        31
036000131004     C                   endif
036100131004     C*
036200131004     C                   ENDSR
036300131021
036400131021
036500131021     C     Decod_societa BEGSR
036600131021     C*
036700131021     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
036800131021     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
036900131021     C                   RESET                   tibsSocI0
037000131022     C                   EVAL      tibsSocI0.idSocieta  = societa
037100131021     C                   EVAL      tibsSocI0.dtValidita = %date(vafDFT)
037200131021     C
037300131021     C                   CALL      'TIBSSOCR'
037400131021     C                   PARM      'GETANAGRAF'  prmRqsOpCode
037500131021     C                   PARM      *BLANK        prmRpyOpCode
037600131021     C                   PARM      *ZERO         prmRpyIdMsg
037700131021     C                   PARM      'TIBSSOCI0'   prmRqsFormato
037800131021     C                   PARM                    tibsSocI0
037900131021     C                   PARM                    prmRqsDataSize
038000131021     C                   PARM      'TIBSSOCO0'   prmRpyFormato
038100131021     C                   PARM                    tibsSocO0
038200131021     C                   PARM                    prmRpyDataSize
038300131021     C                   if         PRMRPYIDMSG >= 0
038400131021     C                              and TIBSSOCO0.IDSOCIETA <> *blank
038500131021     C     minu:maiu     xlate     TIBSSOCO0     TIBSSOCO0
038600131021     C                   if        TIBSSOCO0.SEDLEGNAZ = *blanks
038700131021     C                   eval      TIBSSOCO0.SEDLEGNAZ = 'IT'
038800131025     C                   endif
038900131025     C*
039000131025     C* Verifico eventuali caratteri non consentiti dal EDIFACT
039100131025     C                   movel     *blanks       wAlfaSOCRAG     512
039200131025     C                   movel     *blanks       wAlfaSOCIND     512
039300131025     C                   movel     *blanks       wAlfaSOCLOC     512
039400131025     C* SOC RAG
039500131025     C                   eval      campo_alfa = TIBSSOCO0.RAGSOCIALE
039600131025     C                   exsr      Char_Speciali
039700131025     C                   eval      wAlfaSOCRAG = campo_alfa
039800131025     C* SOC IND
039900131025     C                   eval      campo_alfa = TIBSSOCO0.SEDLEGIND
040000131025     C                   exsr      Char_Speciali
040100131025     C                   eval      wAlfaSOCIND = campo_alfa
040200131025     C* SOC LOC
040300131025     C                   eval      campo_alfa = TIBSSOCO0.SEDLEGLOC
040400131025     C                   exsr      Char_Speciali
040500131025     C                   eval      wAlfaSOCLOC = campo_alfa
040600131025     C*
040700131025     C                   eval      wNAD_CA_2     = %trim(wAlfaSOCRAG)          +
040800131021     C                                             piu                         +
040900131025     C                                             %trim(wAlfaSOCIND)          +
041000131021     C                                             piu
041100131025     C                   eval      wNAD_CA_3     = %trim(wAlfaSOCLOC)          +
041200131021     C                                             piu                         +
041300131021     C                                             piu                         +
041400131021     C                                             %trim(TIBSSOCO0.SEDLEGCAP)  +
041500131021     C                                             piu                         +
041600131021     C                                             %trim(TIBSSOCO0.SEDLEGNAZ)
041700131021     C                   eval      wRFF_VA_1_sgm = 'RFF+VA:'                   +
041800131021     C                                             %trim(TIBSSOCO0.PARTITAIVA)
041900131021     C                   else
042000131021     C                   eval      wNAD_CA_2     = NAD_CA_2
042100131021     C                   eval      wNAD_CA_3     = NAD_CA_3
042200131021     C                   eval      wRFF_VA_1_sgm = RFF_VA_1_sgm
042300131021     C                   endif
042400131021     C*
042500131021     C                   ENDSR
042600131004
042700131004
042800131004     C     wriOUTPUT     BEGSR
042900131004     C*
043000131004     C                   write     TIVAW000
043100131004     C*
043200131004     C                   ENDSR
043300130807
043400041015
043500131023     C     cartab        BEGSR
043600041015     C*
043700131023     C* Reperisco le descrizioni delle varie
043800131023     C                   z-add     *zeros        j2
043900131023     C                   z-add     1             tblKUT
044000131023     C                   movel(P)  'CC'          tblCOD
044100131023     C     KEYtab00_P1   setll     TABEL00F
044200131023     C                   if        %equal(TABEL00F)
044300131023     C     KEYtab00_P1   reade     TABEL00F
044400131023     C                   dow       not %eof(TABEL00F)
044500131030     C                   if        %subst(tblKEY:1:5) = 'VARIE'
044600131023     C                   add       1             j2
044700131030     C                   eval      skSVN(j2) = %subst(tblKEY:8:1)
044800131030     C                   movel(P)  tblUNI        DSCC
044900131023     C                   movel(P)  §CCDES        skSVNDES(j2)
045000131023     C                   if        §CCJEI <> *blanks
045100131023     C                   eval      skSVNnoIVA(j2) = 'Y'
045200131023     C                   endif
045300131030     C                   endif
045400131023     C     KEYtab00_P1   reade     TABEL00F
045500131023     C                   enddo
045600131023     C                   endif
045700131030     C*
045800131030     C* Carico i codici varie personalizzati per cliente
045900131030     C                   z-add     1             j4
046000131030     C                   dow       j4 <= %elem(CHRGCODES) and
046100131030     C                                   CHRGCODES(j4) <> *blanks
046200131030     C                   eval      DS_CHRGCODE = CHRGCODES(j4)
046300131030     C                   eval      skCHRG_BRT(j4) = CHRGCODE_BRT
046400131030     C                   eval      skCHRG_CLI(j4) = CHRGCODE_CLI
046500131030     C                   eval      skCHRG_DES(j4) = CHRGCODE_DES
046600131030     C                   add       1             j4
046700131030     C                   enddo
046800041015     C*
046900041015     C                   ENDSR
047000131004
047100131004
047200131004     C     Char_Speciali BEGSR
047300131111     C*
047400131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
047500131111     C*
047600131111     C/EXEC SQL
047700131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
047800131111     C/END-EXEC
047900131111     C*
048000131111     C/EXEC SQL
048100131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
048200131111     C/END-EXEC
048300131111     C*
048400131111     C/EXEC SQL
048500131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
048600131111     C/END-EXEC
048700131111     C*
048800131111     C/EXEC SQL
048900131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
049000131111     C/END-EXEC
049100131111     C*
049200131111     C/EXEC SQL
049300131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
049400131111     C/END-EXEC
049500131111     C*
049600131004     C                   ENDSR
049700131004
049800131004
049900131004     C     wriHeader     BEGSR
050000131010     C*
050100131019     C*  UNB -
050200131004     C                   eval      vawDTA = UNB_sgm
050300131018     C                              + %trim(w§UNB_ID_1)+%trim(w§UNB_ID_2)
050400131004     C                              +   piu
050500131004     C                              + %trim(currDate6inv)
050600131004     C                              +   diviso
050700131004     C                              + %trim(currHHMM)
050800131004     C                              +   piu
050900131004     C                              + %trim(
051000131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
051100131004     C                              +   apice
051200131004     C                   exsr      wriOUTPUT
051300131004     C*
051400131004     C                   ENDSR
051500131019
051600131019
051700131019     C     wriSingle     BEGSR
051800131021     C*
051900131021     C* Reperisco i dati societari di fatturazione
052000131021     C                   exsr      Decod_societa
052100131022     C*
052200131022     C* Reperisco i dati della registrazione contabile
052300131022     C                   exsr      rtvContab
052400171005     C*
052500171005     C* Gestione dati sociali in relazione alla data fattura
052600171005     C                   if        VAFDFT >= 20171030
052700171005     C                   seton                                        43
052800171005     C                   else
052900171005     C                   setoff                                       43
053000171005     C                   endif
053100131019     C*
053200131019     C*  UNH -
053300131019     C                   eval      Conta_segm  = Conta_segm  + 1
053400131019     C                   eval      vawDTA = UNH_sgm_1
053500131022     C                              + %trim(%editc(Conta_msg:'3'))
053600131019     C                              + UNH_sgm_2
053700131019     C                              +   apice
053800131019     C                   exsr      wriOUTPUT
053900131019     C*
054000131019     C*  BGM -
054100131019     C                   eval      Conta_segm  = Conta_segm  + 1
054200131019     C                   eval      vawDTA = BGM_sgm_1
054300140506     C                              + %trim(%editc(vafNFT:'X'))
054400140506     C                              + %trim(%editc(vafFIV:'X'))
054500131019     C                              + BGM_sgm_2
054600131019     C                              +   apice
054700131019     C                   exsr      wriOUTPUT
054800131019     C*
054900131019     C*  DTM 3 -
055000131019     C                   eval      Conta_segm  = Conta_segm  + 1
055100131019     C                   eval      vawDTA = DTM_3_1
055200131022     C                              + %trim(%editc(vafDFT:'3'))
055300131019     C                              + DTM_3_2
055400131019     C                              +   apice
055500131019     C                   exsr      wriOUTPUT
055600131019     C*
055700131023     C*  DTM 137 -
055800131019     C                   eval      Conta_segm  = Conta_segm  + 1
055900131019     C                   eval      vawDTA = DTM_137_1
056000131023     C                              + %trim(currDate8)
056100131019     C                              + DTM_137_2
056200131019     C                              +   apice
056300131019     C                   exsr      wriOUTPUT
056400131019     C*
056500131019     C*  RFF IT -
056600131019     C                   eval      Conta_segm  = Conta_segm  + 1
056700131019     C                   eval      vawDTA = RFF_IT_sgm
056800131030     C                              + %trim(%editc(w§PPTKSU:'3'))
056900131019     C                              +   apice
057000131019     C                   exsr      wriOUTPUT
057100131019     C*
057200131019     C*  NAD CA -
057300131019     C                   eval      Conta_segm  = Conta_segm  + 1
057400131021     C                   eval      vawDTA = NAD_CA_1
057500131019     C                              + %trim(w§UNB_ID_1)
057600131019     C                              +   piu
057700131019     C                              +   piu
057800131021     C                              + %trim(wNAD_CA_2)
057900131021     C                              + %trim(wNAD_CA_3)
058000131019     C                              +   apice
058100131019     C                   exsr      wriOUTPUT
058200131019     C*
058300131019     C*  RFF VA 1 -
058400131019     C                   eval      Conta_segm  = Conta_segm  + 1
058500131025     C                   eval      vawDTA = %trim(wRFF_VA_1_sgm)
058600131019     C                              +   apice
058700131019     C                   exsr      wriOUTPUT
058800131019     C*
058900131019     C*  NAD SF -
059000131019     C                   eval      Conta_segm  = Conta_segm  + 1
059100131019     C                   eval      vawDTA = NAD_SF_1
059200131019     C                              + NAD_SF_2
059300131019     C                              + NAD_SF_3
059400131019     C                              + NAD_SF_4
059500131019     C                              + NAD_SF_5
059600131128     C                              + NAD_SF_6
059700131019     C                              +   apice
059800131019     C                   exsr      wriOUTPUT
059900131019     C*
060000171005     C*  NAD IV -
060100131019     C                   eval      Conta_segm  = Conta_segm  + 1
060200171005     C  N43              eval      vawDTA = NAD_IV_1
060300131019     C                              + NAD_IV_2
060400131019     C                              + NAD_IV_3
060500131019     C                              + NAD_IV_4
060600131019     C                              + NAD_IV_5
060700131019     C                              + NAD_IV_6
060800131128     C                              + NAD_IV_7
060900131019     C                              +   apice
061000171005     C   43              eval      vawDTA = NAD_IV_1_2
061100171005     C                              + NAD_IV_2_2
061200171005     C                              + NAD_IV_3_2
061300171005     C                              + NAD_IV_4_2
061400171005     C                              + NAD_IV_5_2
061500171005     C                              + NAD_IV_6_2
061600171005     C                              + NAD_IV_7_2
061700171005     C                              +   apice
061800131019     C                   exsr      wriOUTPUT
061900131019     C*
062000131019     C*  RFF VA 2 -
062100131019     C                   eval      Conta_segm  = Conta_segm  + 1
062200131019     C                   eval      vawDTA = RFF_VA_2_sgm
062300131019     C                              +   apice
062400131019     C                   exsr      wriOUTPUT
062500131019     C*
062600131019     C*  CUX -
062700131019     C                   eval      Conta_segm  = Conta_segm  + 1
062800131019     C                   eval      vawDTA = CUX_sgm
062900131019     C                              +   apice
063000131019     C                   exsr      wriOUTPUT
063100131019     C*
063200131019     C*  PAT -
063300131019     C                   eval      Conta_segm  = Conta_segm  + 1
063400131019     C                   eval      vawDTA = PAT_sgm
063500131022     C                              + %trim(%editc(wPagamGG:'3'))
063600131019     C                              +   apice
063700131019     C                   exsr      wriOUTPUT
063800131019     C*
063900131019     C*  PCD -
064000131019     C                   eval      Conta_segm  = Conta_segm  + 1
064100131019     C                   eval      vawDTA = PCD_sgm
064200131019     C                              +   apice
064300131019     C                   exsr      wriOUTPUT
064400131019     C*
064500131019     C                   ENDSR
064600131019
064700131019
064800140407     C     wriSummary    BEGSR
064900140407     C*
065000140407     C* Come ultima spedizione e prima dei dati di sommario della fattura corrente
065100140407     C* si inserisce una bolla fake relativa al bollo fattura in esenzione IVA
065200150401     C* (solo se effettivamente presente il bollo)
065300150401     C                   if        wBOLLO > *zeros
065400140407     C                   exsr      exeBollaBollo
065500150401     C                   endif
065600131019     C*
065700131019     C*  UNS -
065800131019     C                   eval      Conta_segm  = Conta_segm  + 1
065900131019     C                   eval      vawDTA = UNS_sgm
066000131019     C                              +   apice
066100131019     C                   exsr      wriOUTPUT
066200131019     C*
066300131019     C*  CNT 2 -
066400131019     C                   eval      Conta_segm  = Conta_segm  + 1
066500131019     C                   eval      vawDTA = CNT_2_sgm
066600131022     C                              + %trim(%editc(Conta_item:'3'))
066700131019     C                              +   apice
066800131019     C                   exsr      wriOUTPUT
066900131019     C*
067000131019     C*  MOA 79 -
067100131019     C                   eval      Conta_segm  = Conta_segm  + 1
067200131019     C                   eval      vawDTA = MOA_79_sgm
067300131219     C                              + %trim(%editc(wTotIMV:'3'))
067400131019     C                              +   apice
067500131019     C                   exsr      wriOUTPUT
067600131019     C*
067700131205     C*  MOA 58 -
067800140407     C                   eval      Conta_segm  = Conta_segm  + 1
067900140407     C                   eval      vawDTA = MOA_58_sgm
068000140407     C                              + %trim(%editc(wBOLLO:'3'))
068100140407     C                              +   apice
068200140407     C                   exsr      wriOUTPUT
068300131019     C*
068400131019     C*  MOA 77 -
068500131019     C                   eval      Conta_segm  = Conta_segm  + 1
068600131019     C                   eval      vawDTA = MOA_77_sgm
068700131022     C                              + %trim(%editc(wTotFATT:'3'))
068800131019     C                              +   apice
068900131019     C                   exsr      wriOUTPUT
069000131022     C*
069100131022     C* Ciclo per tutti gli imponibili/aliquote presenti in fattura
069200131023     C                   dow       j1 > *zeros
069300131022     C*
069400131022     C*  TAX -
069500131022     C                   eval      Conta_segm  = Conta_segm  + 1
069600131022     C                   eval      vawDTA = TAX_sgm
069700131023     C                              + %trim(%editc(skIVAALQ(j1):'3'))
069800131019     C                              +   apice
069900131019     C                   exsr      wriOUTPUT
070000131019     C*
070100131205     C*  MOA 124 -
070200131019     C                   eval      Conta_segm  = Conta_segm  + 1
070300131019     C                   eval      vawDTA = MOA_124_sgm
070400131023     C                              + %trim(%editc(skIVAIMP(j1):'3'))
070500131019     C                              +   apice
070600131019     C                   exsr      wriOUTPUT
070700131019     C*
070800131205     C*  MOA 125 -
070900131019     C                   eval      Conta_segm  = Conta_segm  + 1
071000131019     C                   eval      vawDTA = MOA_125_sgm
071100131023     C                              + %trim(%editc(skIMPONB(j1):'3'))
071200131022     C                              +   apice
071300131022     C                   exsr      wriOUTPUT
071400131022     C*
071500131023     C                   sub       1             j1
071600131022     C                   enddo
071700131019     C*
071800131019     C*  UNT -
071900131019     C                   eval      Conta_segm  = Conta_segm  + 1
072000131019     C                   eval      vawDTA = UNT_sgm
072100131022     C                              + %trim(%editc(Conta_segm:'3'))
072200131019     C                              +   piu
072300131022     C                              + %trim(%editc(Conta_msg:'3'))
072400131019     C                              +   apice
072500131019     C                   exsr      wriOUTPUT
072600131019     C*
072700131019     C                   ENDSR
072800131019
072900131004
073000131004     C     wriFooter     BEGSR
073100131004     C*
073200131004     C*  UNZ -
073300131004     C                   eval      vawDTA = UNZ_sgm
073400131004     C                              +   piu
073500131022     C                              + %trim(%editc(Conta_msg:'3'))
073600131004     C                              +   piu
073700131004     C                              + %trim(
073800131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
073900131004     C                              +   apice
074000131004     C                   exsr      wriOUTPUT
074100131004     C*
074200131004     C                   ENDSR
074300131019
074400131019
074500131019     C     exeInz        BEGSR
074600131019     C*
074700131019     C                   clear                   wTotIMV
074800131019     C                   clear                   wTotIVA
074900131019     C                   clear                   wTotFATT
075000131019     C                   clear                   Conta_msg
075100131019     C                   clear                   Conta_item
075200131019     C                   clear                   Conta_segm
075300131019     C*
075400131019     C                   ENDSR
075500030924
075600030924
075700991027     C     traduci       BEGSR
075800131004     C*
075900131019     C* Inizializzo i contatori a livello di transazione/messaggio
076000131019     C                   exsr      exeInz
076100131019     C*
076200131019     C                   READ      tivgdtmp
076300131019     C                   dow       not %eof(tivgdtmp)
076400131019     C                   movel     vgddta        tivafds
076500131022     C*
076600131022     C* Innanzitutto chaino la bolla direttamente su TITAS
076700131022     C     KEYtas30P     chain     titas30c
076800131022     C                   if        %found(titas30c)
076900160208     C*
077000170112     C* Reperisco NATURA MERCE
077100160208     C                   EXSR      RTVNAS
077200131019     C*
077300140107     C* Effettuo rottura di codice per numero fattura/registro IVA
077400140107     C                   if        savNFT <> vafNFT AND savFIV <> vafFIV
077500140107     C                   if        savNFT  = *zeros AND savFIV  = *zeros
077600131019     C* Scarico i dati di testata fattura
077700131019     C                   exsr      wriSingle
077800131022     C* Elaboro la bolla corrente
077900131022     C                   exsr      exeBolla
078000131019     C*
078100131019     C                   else
078200131019     C* Elaboro la bolla corrente
078300131022     C                   exsr      exeBolla
078400131022     C* Scarico i dati di sommario a livello di fattura
078500131022     C                   exsr      wriSummary
078600131019     C* Inizializzo i contatori a livello di transazione/messaggio
078700131019     C                   exsr      exeInz
078800131019     C* Scarico i dati di testata fattura
078900131019     C                   exsr      wriSingle
079000131019     C                   endif
079100131019     C*
079200131019     C                   else
079300131019     C* Elaboro la bolla corrente
079400131022     C                   exsr      exeBolla
079500131019     C                   endif
079600131019     C
079700131019     C* Salvo i nuovi valori della rottuta di codice
079800131019     C                   eval      savNFT = vafNFT
079900140107     C                   eval      savFIV = vafFIV
080000131022     C*
080100131022     C                   endif
080200131019     C*
080300131019     C                   delete    tivgd000
080400160217     C*
080500131019     C                   read      tivgdtmp
080600131019     C                   enddo
080700131022     C*
080800131022     C* Scarico i dati di sommario a livello di fattura
080900131022     C                   exsr      wriSummary
081000991027     C*
081100030325     C                   EVAL      wrkesito = '0'
081200991027     C*
081300910830     C                   ENDSR
081400131019
081500131019
081600140407     C     exeBolla      BEGSR
081700131019     C*
081800131019     C* Incremento il contatore di item (ovvero spedizione)
081900131019     C                   add       1             Conta_item
082000131023     C*
082100131023     C* Reperisco i dati di dettaglio fatturazione spedizione corrente
082200131023     C                   exsr      rtvSvn
082300131019     C*
082400131019     C* Scarico il buffer d output
082500131019     C                   exsr      wriDettagli
082600160217     C*
082700160217     C* Salvo la nuova chiave di rottura
082800160217     C                   eval      wDepRMN = vafRMN
082900160217     C                   eval      wDepRMA = vafRMA
083000160217     C*
083100160217     C* Eseguo routine di De-Accorpamento in uscita dal ciclo di principale
083200160217     C                   exsr      DeAccorpa
083300160217     C*
083400160217     C* Inizializzazioni per nuova chiave di rottura
083500160217     C                   clear                   skCpyAccorpa
083600160217     C                   clear                   jCpyAcc
083700131019     C*
083800131019     C                   ENDSR
083900140407
084000140407
084100140407     C     exeBollaBollo BEGSR
084200140407     C*
084300140407     C* Incremento il contatore di item (ovvero spedizione)
084400140407     C                   add       1             Conta_item
084500140407     C*
084600140407     C* Scarico il buffer d output
084700140407     C                   exsr      wriBollaFake
084800140407     C*
084900140407     C                   ENDSR
085000140407
085100140407
085200140407     C     wriBollaFake  BEGSR
085300140407     C*
085400140407     C* Incremento il contatore di messaggio (al interno della transazione)
085500140407     C                   eval      Conta_msg = Conta_msg + 1
085600140407     C*
085700140407     C* Compongo segmenti per dati di dettaglio spedizione corrente
085800140407     C*
085900140407     C*  LIN -
086000140407     C                   eval      Conta_segm  = Conta_segm  + 1
086100140407     C                   eval      vawDTA = LIN_sgm
086200140407     C                              + %trim(%editc(Conta_item:'3'))
086300140407     C                              +   apice
086400140407     C                   exsr      wriOUTPUT
086500140407     C*
086600140407     C*  IMD -
086700140407     C                   eval      Conta_segm  = Conta_segm  + 1
086800140407     C                   eval      vawDTA = IMD_sgm_Bollo
086900140407     C                              +   apice
087000140407     C                   exsr      wriOUTPUT
087100170411     C*
087200170411     C*  MEA WT -
087300170411     C                   eval      Conta_segm  = Conta_segm  + 1
087400170411     C                   eval      vawDTA = MEA_WT_sgm
087500170411     C                              + '0'
087600170411     C                              +   apice
087700170411     C                   exsr      wriOUTPUT
087800140407     C*
087900140407     C*  MOA 203 -
088000140407     C                   eval      Conta_segm  = Conta_segm  + 1
088100140407     C                   eval      vawDTA = MOA_203_sgm
088200140407     C                              + '0'
088300140407     C                              +   apice
088400140407     C                   exsr      wriOUTPUT
088500140407     C*
088600140407     C*  PRI -
088700140407     C                   eval      Conta_segm  = Conta_segm  + 1
088800140407     C                   eval      vawDTA = PRI_sgm
088900140407     C                              + '0'
089000140407     C                              +   apice
089100140407     C                   exsr      wriOUTPUT
089200140407     C*
089300140407     C*  TAX -
089400140407     C                   eval      Conta_segm  = Conta_segm  + 1
089500140407     C                   eval      vawDTA = TAX_sgm
089600140407     C                              + '0'
089700140407     C                              +   apice
089800140407     C                   exsr      wriOUTPUT
089900170904     C*
090000170904     C*  MOA 124 -
090100170904     C                   eval      Conta_segm  = Conta_segm  + 1
090200170904     C                   eval      vawDTA = MOA_124_sgm
090300170904     C                              + '0'
090400170904     C                              +   apice
090500170904     C                   exsr      wriOUTPUT
090600140407     C*
090700140407     C*  NAD CN -
090800140407     C                   eval      Conta_segm  = Conta_segm  + 1
090900140407     C                   eval      vawDTA = NAD_CN
091000140407     C                              + 'DUMMY DATA'
091100140407     C                              +   piu
091200140407     C                              +   piu
091300140407     C                              + 'DUMMY DATA'
091400140407     C                              +   piu
091500140407     C                              + 'BOLOGNA'
091600140407     C                              +   piu
091700140407     C                              + 'BO'
091800140407     C                              +   piu
091900140407     C                              + '40138'
092000140407     C                              +   piu
092100140407     C                              + 'IT'
092200140407     C                              +   apice
092300140407     C                   exsr      wriOUTPUT
092400140407     C*
092500140407     C*  RFF AAM -
092600140407     C                   eval      Conta_segm  = Conta_segm  + 1
092700140407     C                   eval      vawDTA = RFF_AAM_sgm
092800140407     C                              + '00000'
092900140407     C                              +   apice
093000140407     C                   exsr      wriOUTPUT
093100140407     C*
093200140407     C*  RFF CR -
093300140407     C                   eval      Conta_segm  = Conta_segm  + 1
093400140407     C                   eval      vawDTA = RFF_CR_sgm
093500140407     C                              + '00000'
093600140407     C                              +   apice
093700140407     C                   exsr      wriOUTPUT
093800140407     C*
093900140407     C*  RFF ACC -
094000140407     C                   eval      Conta_segm  = Conta_segm  + 1
094100140407     C                   eval      vawDTA = RFF_ACC_sgm
094200140407     C                              + '00000'
094300140407     C                              +   apice
094400140407     C                   exsr      wriOUTPUT
094500140407     C*
094600140407     C*  DTM 200 -
094700140407     C                   eval      Conta_segm  = Conta_segm  + 1
094800140407     C                   eval      vawDTA = DTM_200_1
094900140407     C                              + %trim(%editc(vafDFT:'X'))
095000140407     C                              + DTM_200_2
095100140407     C                              +   apice
095200140407     C                   exsr      wriOUTPUT
095300140407     C*
095400140407     C*  ALC -
095500140407     C                   eval      Conta_segm  = Conta_segm  + 1
095600140407     C                   eval      vawDTA = ALC_sgm_Bollo
095700140407     C                              + %trim(skALC(j3))
095800140407     C                              +   apice
095900140407     C                   exsr      wriOUTPUT
096000140407     C*
096100140407     C*  MOA 8 -
096200140407     C                   eval      Conta_segm  = Conta_segm  + 1
096300171019     C                   eval(h)   wVAL3D = wBOLLO
096400140407     C                   eval      vawDTA = MOA_8_sgm
096500171019     C                              + %trim(%editc(wVAL3D:'3'))
096600140407     C                              +   apice
096700140407     C                   exsr      wriOUTPUT
096800140407     C*
096900140407     C*  TAX 7 -
097000140407     C                   eval      Conta_segm  = Conta_segm  + 1
097100140407     C                   eval      vawDTA = TAX_sgm
097200140407     C                              + '0'
097300140407     C                              +   apice
097400140407     C                   exsr      wriOUTPUT
097500170906     C*
097600170906     C*  MOA 124 -
097700170906     C                   eval      Conta_segm  = Conta_segm  + 1
097800170906     C                   eval      vawDTA = MOA_124_sgm
097900170906     C                              + '0'
098000170906     C                              +   apice
098100170906     C                   exsr      wriOUTPUT
098200140407     C*
098300140407     C                   ENDSR
098400131022
098500131022
098600131022     C     rtvContab     BEGSR
098700131022     C*
098800131205     C                   clear                   wBOLLO
098900131023     C                   z-add     *zeros        j1
099000131022     C                   clear                   NDREG000
099100131022     C*
099200131022     C                   eval      REGSOCIETA = societa
099300131022     C                   eval      REGCTB     = 'CG'
099400131022     C                   eval      REGDTREG   = %date(vafDFT)
099500131022     C                   eval      REGNRREG   = vafNFT
099600131022     C                   eval      REGSERREG  = '2'+ %trim(%editc(vafFIV:'X'))
099700131022     C*
099800131022     C* Aggancio la testata della registrazione contabile
099900131022     C     KEYreg04C     chain     ndreg04l
100000131022     C                   if        %found(ndreg04l)
100100131022     C*
100200131022     C* Filtro solamente le caussali 'NFT0'
100300131022     C                   if        REGCAUSTES = 'NFT0'
100400131023     C*
100500131023     C* Reperisco subito importo totale fattura
100600131023     C                   eval      wTotFATT = wTotFATT + REGIMPORTO
100700131022     C*
100800131022     C* Aggancio il dettaglio della registrazione contabile
100900131022     C     KEYiva01P     setll     ndiva01l
101000131022     C                   if        %found(ndiva01l)
101100131022     C     KEYiva01P     reade     ndiva01l
101200131022     C                   dow       not %eof(ndiva01l)
101300131022     C*
101400131022     C* Filtro solamente le caussali 'NFT0'
101500131022     C                   if        IVACAUSTES = 'NFT0'
101600131022     C*
101700131022     C* Reperisco i dati relativi a aliquota IVA, imponibile, importo IVA
101800131023     C                   if        IVAALIQ > wCurrIVA
101900131022     C                   eval      wCurrIVA = IVAALIQ
102000131022     C                   endif
102100131022     C*
102200131022     C                   eval      wTotIVA  = wTotIVA  + IVAIMPORTO
102300131219 xxx C***                eval      wTotIMV  = wTotIMV  + IVAIMPONIB
102400131022     C*
102500131205     C* Nei righi IVA tengo separato il bollo
102600131205     C                   if        IVARIFIVA = '150'
102700131205     C                   eval      wBOLLO = IVAIMPONIB
102800131205     C                   else
102900131023     C                   add       1             j1
103000131023     C                   eval      skIVAIMP(j1) = IVAIMPORTO
103100131023     C                   eval      skIVAALQ(j1) = IVAALIQ
103200131205     C                   eval      skIMPONB(j1) = IVAIMPONIB
103300131205     C                   endif
103400131022     C*
103500131022     C                   endif
103600131022     C*
103700131022     C     KEYiva01P     reade     ndiva01l
103800131022     C                   enddo
103900131022     C*
104000131022     C* Aggancio il partitario scadenze
104100131022     C                   eval      PASNRRIGAM = 1
104200131023     C     KEYpas01P     chain     ndpas01l
104300131022     C                   if        %found(ndpas01l)
104400131022     C*
104500131022     C* Filtro solamente le caussali 'NFT0'
104600131022     C                   if        PASCAUSRIG = 'NFT0'
104700131022     C*
104800131022     C* Reperisco il numero di giorni pagamento data fattura
104900131022     C                   eval      wPagamGG =  %diff(PASDTSCA:REGDTREG:*DAYS)
105000131022     C*
105100131022     C                   endif
105200131022     C*
105300131022     C                   endif
105400131022     C*
105500131022     C                   endif
105600131022     C                   endif
105700131022     C                   endif
105800131022     C*
105900131022     C                   ENDSR
106000131023
106100131023
106200131023     C*------------------------------------------------------------------------*
106300131023     C* RTVSVN - Routine di reperimento VARIE fatturate
106400131023     C*------------------------------------------------------------------------*
106500131023     C     rtvSvn        BEGSR
106600131023     C*
106700131023     C* Inizializzo i dettagli di fatturazione
106800131023     C                   clear                   skALC
106900131023     C                   clear                   skALC_DES
107000131023     C                   clear                   skMOA_8
107100131023     C                   clear                   skTAX_7
107200170906     C                   clear                   skMOA_124
107300131023     C                   z-add     *zeros        j3
107400131023     C*
107500131023     C* Verifica ed eventuale scrittura del PORTO su TITAS
107600131023     C                   IF        tasPOR <> *zeros
107700131023     C                   EVAL      wSVN = '|'
107800131023     C                   EVAL      wVAN = tasPOR
107900131023     C                   EVAL      wDES = 'PORTO'
108000131023     C                   EXSR      WRISVN
108100131023     C                   ENDIF
108200131023     C*
108300131023     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
108400131023     C                   IF        tasSV1 <> *blanks
108500131023     C                   EVAL      wSVN = tasSV1
108600131023     C                   EVAL      wVAN = tasVA1
108700131023     C                   EVAL      wDES = *blanks
108800131023     C                   EXSR      WRISVN
108900131023     C                   ENDIF
109000131023     C*
109100131023     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
109200131023     C                   IF        tasSV2 <> *blanks
109300131023     C                   EVAL      wSVN = tasSV2
109400131023     C                   EVAL      wVAN = tasVA2
109500131023     C                   EVAL      wDES = *blanks
109600131023     C                   EXSR      WRISVN
109700131023     C                   ENDIF
109800131023     C*
109900131023     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
110000131023     C                   IF        tasSV3 <> *blanks
110100131023     C                   EVAL      wSVN = tasSV3
110200131023     C                   EVAL      wVAN = tasVA3
110300131023     C                   EVAL      wDES = *blanks
110400131023     C                   EXSR      WRISVN
110500131023     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
110600131023     C     KEYta730P     SETLL     tita730c
110700131023     C                   IF        %equal(tita730c)
110800131023     C     KEYta730P     READE     tita730c
110900131023     C                   DOW       not %eof(tita730c)
111000131023     C                   if        ta7VAN > *zeros
111100131023     C                   EVAL      wSVN = ta7SVN
111200131023     C                   EVAL      wVAN = ta7VAN
111300131023     C                   EVAL      wDES = *blanks
111400131023     C                   EXSR      WRISVN
111500131023     C                   endif
111600131023     C     KEYta730P     READE     tita730c
111700131023     C                   ENDDO
111800131023     C                   ENDIF
111900131023     C                   ENDIF
112000131023     C*
112100131023     C                   ENDSR
112200131023
112300131023
112400131023     C*------------------------------------------------------------------------*
112500131023     C* WRISVN - Routine di valorizzazione wrk-file VARIE fatturate
112600131023     C*------------------------------------------------------------------------*
112700131023     C     wriSvn        BEGSR
112800131023     C*
112900131023     C* Elaboro tutto tranne PORTO e la varia 8 (IVA)
113000131023     C                   if        wSVN <> '8' and wSVN <> '|'
113100131023     C                   add       1             j3
113200170906     C                   eval      skALC(j3)     = wSVN
113300170906     C                   eval      skMOA_8(j3)   = wVAN
113400131023     C*
113500131023     C* Reperisco la descrizione della VARIA
113600131023     C                   if        wDES = *blanks
113700131023     C                   z-add     1             j2
113800131023     C     wSVN          lookup    skSVN(j2)                              55
113900131023     C                   if        %found
114000131025     C                   eval      skALC_DES(j3) = skSVNDES(j2)
114100131023     C                   else
114200131023     C                   eval      skALC_DES(j3) = *all'*'
114300131023     C                   endif
114400131023     C                   else
114500131025     C                   eval      skALC_DES(j3) = wDES
114600131025     C                   endif
114700131030     C*
114800131030     C* Se richiesto effettuo mapping tra charge codes BRT e quelli CLIENTE
114900131030     C                   if        w§PPTCVT = 'Y'
115000160224     C                   setoff                                       56
115100131030     C                   z-add     1             j4
115200131030     C     wSVN          lookup    skCHRG_BRT(j4)                         55
115300131030     C                   if        %found
115400160224     C                   eval      skALC_DES(j3) = skCHRG_DES(j4)
115500131030     C                   if        skCHRG_CLI(j4) <> *blanks
115600160224     C                   seton                                        56
115700131030     C                   eval      skALC(j3)     = skCHRG_CLI(j4)
115800131030     C                   endif
115900131030     C                   endif
116000160224     C*
116100170112     C* Se richiesta mappatura ma varia non contemplata "forzo" codice varia cliente "generico"
116200160224     C                   if        not *in56
116300160224     C                   eval      skALC(j3)     = 'SC'
116400160224     C                   endif
116500131030     C                   endif
116600131025     C*
116700131025     C* Verifico eventuali caratteri non consentiti dal EDIFACT
116800131025     C* ALC DES
116900131025     C                   eval      campo_alfa = skALC_DES(j3)
117000131025     C                   exsr      Char_Speciali
117100131030     C                   eval      skALC_DES(j3) = campo_alfa
117200131023     C*
117300170906     C* Reperisco aliquota IVA della VARIA corrente e lego relativo importo IVA
117400131023     C                   z-add     1             j2
117500131023     C     wSVN          lookup    skSVNnoIVA(j2)                         55
117600131023     C                   if        %found
117700170906     C                   eval      skTAX_7(j3)   = *zeros
117800170906     C                   eval      skMOA_124(j3) = *zeros
117900131023     C                   else
118000170906     C                   eval      skTAX_7(j3)   = wCurrIVA
118100170906     C                   eval(h)   skMOA_124(j3) = skMOA_8(j3)*wCurrIVA/100
118200131023     C                   endif
118300131023     C*
118400131023     C                   endif
118500131023     C*
118600131023     C                   clear                   wSVN
118700131023     C                   clear                   wVAN
118800131023     C                   clear                   wDES
118900131023     C*
119000131023     C                   ENDSR
119100031103
119200031103
119300131019     C     wriDettagli   BEGSR
119400131023     C*
119500131004     C* Incremento il contatore di messaggio (al interno della transazione)
119600131019     C                   eval      Conta_msg = Conta_msg + 1
119700131004     C*
119800131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
119900131025     C                   movel     *blanks       wAlfaRMA        512
120000131025     C                   movel     *blanks       wAlfaRSD        512
120100131004     C                   movel     *blanks       wAlfaLOD        512
120200131004     C                   movel     *blanks       wAlfaIND        512
120300131025     C                   movel     *blanks       wAlfaNAS        512
120400131004     C* RMA
120500131019     C                   eval      campo_alfa = vafRMA
120600131004     C                   exsr      Char_Speciali
120700131004     C                   eval      wAlfaRMA = campo_alfa
120800131025     C* RSD
120900131025     C                   eval      campo_alfa = tasRSD
121000131025     C                   exsr      Char_Speciali
121100131025     C                   eval      wAlfaRSD = campo_alfa
121200131004     C* LOD
121300131004     C                   eval      campo_alfa = tasLOD
121400131004     C                   exsr      Char_Speciali
121500131004     C                   eval      wAlfaLOD = campo_alfa
121600131004     C* IND
121700131004     C                   eval      campo_alfa = tasIND
121800131004     C                   exsr      Char_Speciali
121900131004     C                   eval      wAlfaIND = campo_alfa
122000131025     C* NAS
122100160208     C                   if        §TA4ANAS <> *blanks
122200160208     C                   eval      campo_alfa = §TA4ANAS
122300131025     C                   exsr      Char_Speciali
122400131025     C                   eval      wAlfaNAS = campo_alfa
122500131025     C                   else
122600131025     C                   eval      wAlfaNAS = 'VOID'
122700131025     C                   endif
122800131004     C*
122900131004     C*
123000131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
123100131019     C*
123200131019     C*  LIN -
123300131019     C                   eval      Conta_segm  = Conta_segm  + 1
123400131019     C                   eval      vawDTA = LIN_sgm
123500131022     C                              + %trim(%editc(Conta_item:'3'))
123600131019     C                              +   apice
123700131019     C                   exsr      wriOUTPUT
123800160217     C                   eval      vawDTA_Acc = vawDTA
123900160217     C                   exsr      CpyAccorpa
124000131019     C*
124100131019     C*  IMD -
124200131019     C                   eval      Conta_segm  = Conta_segm  + 1
124300131019     C                   eval      vawDTA = IMD_sgm
124400131019     C                              +   apice
124500131019     C                   exsr      wriOUTPUT
124600160217     C                   eval      vawDTA_Acc = vawDTA
124700160217     C                   exsr      CpyAccorpa
124800170411     C*
124900170411     C*  MEA WT -
125000170411     C                   eval      Conta_segm  = Conta_segm  + 1
125100170411     C                   if        vafPKC > vafPKB
125200170411     C                   z-add     vafPKC        wPeso             7 1
125300170411     C                   else
125400170411     C                   z-add     vafPKB        wPeso
125500170411     C                   endif
125600170411     C                   eval      vawDTA = MEA_WT_sgm
125700170411     C                              + %trim(%editc(wPeso:'3'))
125800170411     C                              +   apice
125900170411     C                   exsr      wriOUTPUT
126000170411     C                   eval      vawDTA_Acc = vawDTA
126100170411     C                   exsr      CpyAccorpa
126200131019     C*
126300131019     C*  MOA 203 -
126400131019     C                   eval      Conta_segm  = Conta_segm  + 1
126500171019     C                   eval(h)   wVAL3D = vafIMV
126600131019     C                   eval      vawDTA = MOA_203_sgm
126700171019     C                              + %trim(%editc(wVAL3D:'3'))
126800131219     C                              +   apice
126900131019     C                   exsr      wriOUTPUT
127000160217     C                   eval      vawDTA_Acc = vawDTA
127100160217     C                   exsr      CpyAccorpa
127200131219     C                   eval      wTotIMV  = wTotIMV + vafIMV                  * Tot MOA+203 per 79
127300131019     C*
127400131019     C*  PRI -
127500160204     C*
127600160204     C* Ritorno il valore reale del PORTO solo se spedizione NON di "reso mittente"
127700160201     C                   if        tasFBR = *blanks
127800170904     C                   z-add     vafPOR        wPOR             11 3
127900170904     C*
128000170904     C* Se trattasi di spedizione di "reso mittente" invece ritorno il porto a zero
128100170904     C                   else
128200170904     C                   z-add     *zeros        wPOR
128300170904     C                   endif
128400170904     C*
128500131019     C                   eval      Conta_segm  = Conta_segm  + 1
128600171019     C                   eval(h)   wVAL3D = wPOR
128700131019     C                   eval      vawDTA = PRI_sgm
128800171019     C                              + %trim(%editc(wVAL3D:'3'))
128900131019     C                              +   apice
129000131019     C                   exsr      wriOUTPUT
129100160217     C                   eval      vawDTA_Acc = vawDTA
129200160217     C                   exsr      CpyAccorpa
129300131019     C*
129400131019     C*  TAX -
129500131019     C                   eval      Conta_segm  = Conta_segm  + 1
129600131019     C                   eval      vawDTA = TAX_sgm
129700131022     C                              + %trim(%editc(wCurrIVA:'3'))
129800131019     C                              +   apice
129900131019     C                   exsr      wriOUTPUT
130000160217     C                   eval      vawDTA_Acc = vawDTA
130100160217     C                   exsr      CpyAccorpa
130200170904     C*
130300170904     C*  MOA 124 -
130400170906     C***                eval(h)   wIVA = vafIMV*wCurrIVA/100
130500170906     C                   eval(h)   wIVA = wPOR*wCurrIVA/100
130600170904     C                   eval      Conta_segm  = Conta_segm  + 1
130700170904     C                   eval      vawDTA = MOA_124_sgm
130800170904     C                              + %trim(%editc(wIVA:'3'))
130900170904     C                              +   apice
131000170904     C                   exsr      wriOUTPUT
131100170904     C                   eval      vawDTA_Acc = vawDTA
131200170904     C                   exsr      CpyAccorpa
131300131018     C*
131400131019     C*  NAD CN -
131500131004     C                   eval      Conta_segm  = Conta_segm  + 1
131600131018     C                   eval      vawDTA = NAD_CN
131700131025     C                              + %trim(wAlfaRSD)
131800131004     C                              +   piu
131900131004     C                              +   piu
132000131004     C                              + %trim(wAlfaIND)
132100131004     C                              +   piu
132200131004     C                              + %trim(wAlfaLOD)
132300131004     C                              +   piu
132400131004     C                              + %trim(tasPRD)
132500131004     C                              +   piu
132600131004     C                              + %trim(tasCAD)
132700131004     C                              +   piu
132800131004     C                              + 'IT'
132900131004     C                              +   apice
133000131019     C                   exsr      wriOUTPUT
133100160217     C                   eval      vawDTA_Acc = vawDTA
133200160217     C                   exsr      CpyAccorpa
133300131019     C*
133400131022     C*  RFF AAM -
133500131019     C                   eval      Conta_segm  = Conta_segm  + 1
133600131019     C                   eval      vawDTA = RFF_AAM_sgm
133700131025     C                              + %trim(wAlfaNAS)
133800131019     C                              +   apice
133900131019     C                   exsr      wriOUTPUT
134000160217     C                   eval      vawDTA_Acc = vawDTA
134100160217     C                   exsr      CpyAccorpa
134200131019     C*
134300131019     C*  RFF CR -
134400131019     C                   eval      Conta_segm  = Conta_segm  + 1
134500131019     C                   eval      vawDTA = RFF_CR_sgm
134600131022     C                              + %trim(%editc(vafRMN:'3'))
134700131019     C                              +   apice
134800131019     C                   exsr      wriOUTPUT
134900160217     C                   eval      vawDTA_Acc = vawDTA
135000160217     C                   exsr      CpyAccorpa
135100131019     C*
135200131019     C*  RFF ACC -
135300131019     C                   eval      Conta_segm  = Conta_segm  + 1
135400131019     C                   eval      vawDTA = RFF_ACC_sgm
135500131025     C                              + %trim(wAlfaRMA)
135600131019     C                              +   apice
135700131019     C                   exsr      wriOUTPUT
135800160217     C                   eval      vawDTA_Acc = vawDTA
135900160217     C                   exsr      CpyAccorpa
136000131019     C*
136100131019     C*  DTM 200 -
136200131019     C                   eval      Conta_segm  = Conta_segm  + 1
136300131019     C                   eval      vawDTA = DTM_200_1
136400131019     C                              + %trim(%editc(tasDRT:'X'))
136500131019     C                              + DTM_200_2
136600131019     C                              +   apice
136700131019     C                   exsr      wriOUTPUT
136800160217     C                   eval      vawDTA_Acc = vawDTA
136900160217     C                   exsr      CpyAccorpa
137000131023     C*
137100131023     C* Sviluppo tutti i dettagli di fatturazione
137200160202     C* Ritorno le VARIE solo se spedizione NON di "reso mittente"
137300160201     C                   if        tasFBR = *blanks
137400131023     C                   eval      j3 = 1
137500131023     C                   dow       j3 <= %elem(skALC) and skALC(j3) <> *blanks
137600131023     C*
137700131023     C* Restituisco solamente le varie con importo
137800131023     C                   if        skMOA_8(j3) > *zeros
137900131023     C*
138000131023     C*  ALC -
138100131023     C                   eval      Conta_segm  = Conta_segm  + 1
138200131023     C                   eval      vawDTA = ALC_sgm
138300131023     C                              + %trim(skALC(j3))
138400131023     C                              +   diviso
138500131023     C                              +   diviso
138600131023     C                              +   diviso
138700131030     C                              + %trim(skALC_DES(j3))
138800131023     C                              +   apice
138900131023     C                   exsr      wriOUTPUT
139000160217     C                   eval      vawDTA_Acc = vawDTA
139100160217     C                   exsr      CpyAccorpa
139200131023     C*
139300131023     C*  MOA 8 -
139400131023     C                   eval      Conta_segm  = Conta_segm  + 1
139500171019     C                   eval(h)   wVAL3D = skMOA_8(j3)
139600131023     C                   eval      vawDTA = MOA_8_sgm
139700171019     C                              + %trim(%editc(wVAL3D:'3'))
139800131023     C                              +   apice
139900131023     C                   exsr      wriOUTPUT
140000160217     C                   eval      vawDTA_Acc = vawDTA
140100160217     C                   exsr      CpyAccorpa
140200131023     C*
140300131023     C*  TAX 7 -
140400131023     C                   eval      Conta_segm  = Conta_segm  + 1
140500131023     C                   eval      vawDTA = TAX_sgm
140600170915     C                              + %trim(%editc(skTAX_7(j3):'3'))
140700131023     C                              +   apice
140800131023     C                   exsr      wriOUTPUT
140900160217     C                   eval      vawDTA_Acc = vawDTA
141000160217     C                   exsr      CpyAccorpa
141100170906     C*
141200170906     C*  MOA 124 -
141300170906     C                   eval      Conta_segm  = Conta_segm  + 1
141400170906     C                   eval      vawDTA = MOA_124_sgm
141500170906     C                              + %trim(%editc(skMOA_124(j3):'3'))
141600170906     C                              +   apice
141700170906     C                   exsr      wriOUTPUT
141800170906     C                   eval      vawDTA_Acc = vawDTA
141900170906     C                   exsr      CpyAccorpa
142000131023     C*
142100131023     C                   endif
142200131023     C                   add       1             j3
142300131023     C                   enddo
142400160202     C*
142500160202     C* Se trattasi di spedizione di "reso mittente"
142600160202     C                   else
142700160202     C*
142800160202     C*  ALC -
142900160202     C                   eval      Conta_segm  = Conta_segm  + 1
143000160202     C                   eval      vawDTA = ALC_sgm
143100160202     C                              + 'UD'
143200160202     C                              +   diviso
143300160202     C                              +   diviso
143400160202     C                              +   diviso
143500160202     C                              + 'RETURN TO SENDER'
143600160202     C                              +   apice
143700160202     C                   exsr      wriOUTPUT
143800160217     C                   eval      vawDTA_Acc = vawDTA
143900160217     C                   exsr      CpyAccorpa
144000160202     C*
144100160202     C*  MOA 8 -
144200160202     C                   eval      Conta_segm  = Conta_segm  + 1
144300171019     C                   eval(h)   wVAL3D = vafIMV
144400160202     C                   eval      vawDTA = MOA_8_sgm
144500171019     C                              + %trim(%editc(wVAL3D:'3'))
144600160202     C                              +   apice
144700160202     C                   exsr      wriOUTPUT
144800160217     C                   eval      vawDTA_Acc = vawDTA
144900160217     C                   exsr      CpyAccorpa
145000160202     C*
145100160202     C*  TAX 7 -
145200160202     C                   eval      Conta_segm  = Conta_segm  + 1
145300160202     C                   eval      vawDTA = TAX_sgm
145400160202     C                              + %trim(%editc(wCurrIVA:'3'))
145500160202     C                              +   apice
145600160202     C                   exsr      wriOUTPUT
145700160217     C                   eval      vawDTA_Acc = vawDTA
145800160217     C                   exsr      CpyAccorpa
145900170906     C*
146000170906     C*  MOA 124 -
146100170906     C                   eval      Conta_segm  = Conta_segm  + 1
146200170906     C                   eval(h)   wIVA = vafIMV*wCurrIVA/100
146300170906     C                   eval      vawDTA = MOA_124_sgm
146400170906     C                              + %trim(%editc(wIVA:'3'))
146500170906     C                              +   apice
146600170906     C                   exsr      wriOUTPUT
146700170906     C                   eval      vawDTA_Acc = vawDTA
146800170906     C                   exsr      CpyAccorpa
146900160202     C*
147000160201     C                   endif
147100031103     C*
147200031103     C                   ENDSR
147300160208
147400160208
147500160208
147600160208     C     RTVNAS        BEGSR
147700160208     C*
147800160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
147900160208     C                   CLEAR                   DTA4A
148000160208     C*
148100160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
148200160208     C                   CALL      'UBTA400R'
148300160208     C                   PARM      *blanks       UBTA4IOPZ
148400160208     C                   PARM      *blanks       UBTA4ITLA
148500160208     C                   PARM      tasAAS        UBTA4IAAS
148600160208     C                   PARM      tasLNP        UBTA4ILNP
148700160208     C                   PARM      tasNRS        UBTA4INRS
148800160208     C                   PARM      tasNSP        UBTA4INSP
148900160208     C                   PARM      'A'           UBTA4ITRC
149000160208     C                   PARM                    UBTA4OERR
149100160208     C                   PARM                    UBTA4ODS
149200160208     C                   PARM                    UBTA4OLEN
149300160208     C                   PARM                    UBTA4ODATI
149400160208     C*
149500160208     C                   IF        UBTA4OERR = *zeros
149600160208     C                   SELECT
149700160208     C* Gestione output tipo record 'A'
149800160208     C                   WHEN      UBTA4ODS = 'DTA4A'
149900160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
150000160208     C                   ENDSL
150100160208     C*
150200160208     C                   ENDIF
150300160208     C*
150400160208     C                   ENDSR
150500160217
150600160217
150700160217
150800160217     C     CpyAccorpa    BEGSR
150900160217     C*
151000160217     C                   ADD       1             jCpyAcc
151100160217     C                   EVAL      skCpyAccorpa(jCpyAcc) = vawDTA_Acc
151200160217     C*
151300160217     C                   ENDSR
151400160217
151500160217
151600160217
151700160218     C     DeAccorpa     BEGSR
151800160218     C*
151900160217     C* Scorro tutte le spedizioni accorpate sulla spedizone corrente
152000160217     C     KEYrab01P     SETLL     firab01l
152100160217     C                   IF        %equal(firab01l)
152200160217     C     KEYrab01P     READE     firab01l
152300160217     C                   DOW       not %eof(firab01l)
152400160217     C*
152500160217     C* Controllo (ridondante) su riferimenti spedizione accorpante
152600160217     C                   IF        wDepRMN = rabRMN AND
152700160217     C                             wDepRMA = rabRMA
152800160217     C*
152900160217     C* Considero solamente spedizioni ACCORPATE
153000160217     C                   IF        rabRMN = rabRMNR AND
153100160217     C                             rabRMA = rabRMAR
153200160217     C                   ELSE
153300160217     C*
153400160217     C* Per ogni spedizione ACCORPATA "duplico" la spedizione ACCORPANTE
153500160217     C                   Z-ADD     1             jCpyAcc
153600160217     C                   DOW       jCpyAcc <= %elem(skCpyAccorpa) AND
153700160217     C                             skCpyAccorpa(jCpyAcc) <> *blanks
153800160217     C*
153900160217     C* Incremento il contatore dei segmenti
154000160217     C                   eval      Conta_segm  = Conta_segm  + 1
154100160217     C*
154200160217     C* Effettuo sostuituzione di valori rispetto bolla ACCORPANTE e sue ACCORPATE
154300160217     C                   SELECT
154400160217     C*
154500160218     C* LIN (per contatori item (ovvero spedizione)
154600160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(LIN_sgm))
154700160218     C                             = LIN_sgm
154800160218     C                   add       1             Conta_item
154900160218     C                   eval      vawDTA = LIN_sgm
155000160218     C                              + %trim(%editc(Conta_item:'3'))
155100160218     C                              +   apice
155200160218     C*
155300160218     C* RFF CR - (per RMN)
155400160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
155500160218     C                             RFF_CR_sgm)) = RFF_CR_sgm
155600160218     C                   eval      vawDTA = RFF_CR_sgm
155700160217     C                              + %trim(%editc(rabRMNR:'4'))
155800160217     C                              +   apice
155900160218     C*
156000160218     C* RFF ACC - (per RMA)
156100160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
156200160218     C                             RFF_ACC_sgm)) = RFF_ACC_sgm
156300160218     C                   eval      campo_alfa = rabRMAR
156400160218     C                   exsr      Char_Speciali
156500160218     C                   eval      wAlfaRMA = campo_alfa
156600160218     C                   eval      vawDTA = RFF_ACC_sgm
156700160218     C                              + %trim(wAlfaRMA)
156800160218     C                              +   apice
156900170411     C*
157000170411     C*  MEA WT -
157100170411     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
157200170411     C                             MEA_WT_sgm)) = MEA_WT_sgm
157300170411     C                   eval      vawDTA = MEA_WT_sgm
157400170411     C                              + '0'
157500170411     C                              +   apice
157600160218     C*
157700160218     C*  MOA 203 -
157800160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
157900160218     C                             MOA_203_sgm)) = MOA_203_sgm
158000160218     C                   eval      vawDTA = MOA_203_sgm
158100160218     C                              + '0'
158200160218     C                              +   apice
158300160218     C*
158400160218     C*  PRI -
158500170112     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
158600170112     C                             PRI_sgm)) = PRI_sgm
158700160218     C                   eval      vawDTA = PRI_sgm
158800160218     C                              + '0'
158900160218     C                              +   apice
159000170904     C*
159100170904     C*  MOA 124 -
159200170904     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
159300170904     C                             MOA_124_sgm)) = MOA_124_sgm
159400170904     C                   eval      vawDTA = MOA_124_sgm
159500170904     C                              + '0'
159600170904     C                              +   apice
159700160218     C*
159800160218     C*  MOA 8 -
159900160218     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(
160000160218     C                             MOA_8_sgm)) = MOA_8_sgm
160100160218     C                   eval      vawDTA = MOA_8_sgm
160200160218     C                              + '0'
160300160218     C                              +   apice
160400160218     C*
160500160217     C                   OTHER
160600160217     C*
160700160217     C* Tutti gli altri segmenti "AS IS"
160800160217     C                   eval      vawDTA = skCpyAccorpa(jCpyAcc)
160900160217     C                   ENDSL
161000160217     C*
161100160217     C* Scarico l'ouput di "duplicazione"
161200160217     C                   exsr      wriOUTPUT
161300160217     C*
161400160217     C                   ADD       1             jCpyAcc
161500160217     C                   ENDDO
161600160217     C*
161700160217     C                   ENDIF
161800160217     C                   ENDIF
161900160217     C*
162000160217     C     KEYrab01P     READE     firab01l
162100160217     C                   ENDDO
162200160217     C                   ENDIF
162300160217     C*
162400160217     C                   ENDSR
162500031103
162600160208
162700991027
162800991027      /TITLE Operazioni iniziali.
162900991027     C     *inzsr        BEGSR
163000991027     C*
163100991027     C     *ENTRY        PLIST
163200131019     C                   parm                    prmppt
163300991027     C     wrkesito      parm      wrkesito      prmesito
163400031103     C*
163500031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
163600131019     C                   EVAL      DSPPT = prmppt
163700131004     C*
163800131004     C* Reperisco i valori variabili ma a livello di transazione corrente
163900131004     C* (tipicamente data e ora relativ al file generato)
164000131004     C                   eval      currDate6    =
164100131004     C                              UBFMTDATE_Convert(
164200131004     C                                  %editc(%dec(%date():*ISO):'X') :
164300131004     C                                  'YYYYMMDD' : 'YYMMDD')
164400131004     C                   eval      currDate6inv =
164500131004     C                              UBFMTDATE_Convert(
164600131004     C                                  %editc(%dec(%date():*ISO):'X') :
164700131004     C                                  'YYYYMMDD' : 'DDMMYY')
164800131018     C                   eval      currDate8 =
164900131018     C                              UBFMTDATE_Convert(
165000131018     C                                  %editc(%dec(%date():*ISO):'X') :
165100131023     C                                  'YYYYMMDD' : 'YYYYMMDD')
165200131004     C                   eval      currHHMM =
165300131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
165400030709     C*
165500030709     C* Definizione chiavi
165600131023     C*
165700131023     C* LETTURA tabel00f - PARZIALE
165800131023     C     KEYtab00_P1   KLIST
165900131023     C                   KFLD                    tblKUT
166000131023     C                   KFLD                    tblCOD
166100031103     C*
166200031103     C* Chiave su TITAS30C - Parziale
166300031103     C     KEYtas30P     KLIST
166400131019     C                   KFLD                    vafAAS
166500131019     C                   KFLD                    vafLNP
166600131019     C                   KFLD                    vafNRS
166700131019     C                   KFLD                    vafNSP
166800131023     C*
166900131023     C* LETTURA tita730c - PARZIALE
167000131023     C     KEYta730P     KLIST
167100131023     C                   KFLD                    tasAAS
167200131023     C                   KFLD                    tasLNP
167300131023     C                   KFLD                    tasNRS
167400131023     C                   KFLD                    tasNSP
167500131023     C                   KFLD                    tasTBL
167600131022     C*
167700131022     C* Chiave su NDREG04L - Completa
167800131022     C     KEYreg04C     KLIST
167900131022     C                   KFLD                    REGSOCIETA
168000131022     C                   KFLD                    REGCTB
168100131022     C                   KFLD                    REGDTREG
168200131022     C                   KFLD                    REGNRREG
168300131022     C                   KFLD                    REGSERREG
168400131022     C*
168500131022     C* Chiave su NDIVA01L - Parziale
168600131022     C     KEYiva01P     KLIST
168700131023     C                   KFLD                    REGSYS
168800131023     C                   KFLD                    REGNRASREG
168900131022     C*
169000131022     C* Chiave su NDPAS01L - Parziale
169100131022     C     KEYpas01P     KLIST
169200131023     C                   KFLD                    REGSYS
169300131023     C                   KFLD                    REGNRASREG
169400131022     C                   KFLD                    PASNRRIGAM
169500160217     C*
169600160217     C* Chiave su FIRAB01L - Parziale
169700160217     C     KEYrab01P     KLIST
169800160218     C                   KFLD                    tasAAS
169900160218     C                   KFLD                    tasLNP
170000160218     C                   KFLD                    tasNRS
170100160218     C                   KFLD                    tasNSP
170200991027     C*
170300991027     C                   ENDSR
170400131030** CHRGCODES - CHARGE CODES BRT / CLIENTE
170500160127+         SUPPL.CARBURANTE                                                                  01
170600160127&    SC   ANN.P/ASSEGNATO                                                                   02
170700160127*    SC   DIROTTAM.TO                                                                       03
170800160127=    SC   RESO BANCALI                                                                      04
170900160127a    DCC  POD IMAGE                                                                         05
171000160127b    SC   AD.GEST.C.                                                                        06
171100160127c    SC   L.AVVISO                                                                          07
171200160127d    INS  A.C. BASE                                                                         08
171300160127e    SC   PRIORITY                                                                          09
171400160127f    FSC  FUEL SURCH                                                                        10
171500160127h    SC   H 10:30                                                                           11
171600160127k         PACKING L.                                                                        11
171700160127m    SC   AV.AFFID.                                                                         12
171800160127p         PINcode                                                                           11
171900160127t         PRENOT.RITIRO                                                                     11
172000160127z         EXPO                                                                              11
172100160127A    EDC  APPUNTAMENTO                                                                      13
172200160127B    DCC  CONS.DDT                                                                          14
172300160127C    SC   FACCH.ARR.                                                                        15
172400160127D    SC   INV/INC FT                                                                        16
172500160127E         R.A.RESP.VETT.                                                                    17
172600160127F    OSC  FUORI MISURA AZ                                                                   18
172700160127G    COD  COMP.ASSEGNO                                                                      19
172800160127H    SC   DIR.FISSO                                                                         20
172900160127I    HLD  SP.GIACENZA                                                                       21
173000160127J    RAS  ISOLA                                                                             22
173100160127K    RAS  LOC.DISAG.                                                                        23
173200160127L    SC   I.S.T.A.T.                                                                        24
173300160127M    SC   SPESE INSOLUTO                                                                    25
173400160127N    SC   ANTEPORTO                                                                         26
173500160127O    SC   RIC.DOCUM.                                                                        27
173600160127P    AHC  AI PIANI                                                                          28
173700160127Q    DAS  ZTL                                                                               29
173800160127R    INS  A.C. PLUS                                                                         30
173900160127S    SC   SUPER./GDO                                                                        31
174000160127T    SC   RIT.COMMISSIONATO                                                                 32
174100160127U    SC   RITIRO                                                                            33
174200160127W    COD  REC.C/ASSEGNI                                                                     34
174300160127X    DAS  CONS.DISAG.                                                                       35
174400160127Y    SC   RIT.ANNULL.                                                                       36
174500160127Z    FSC  AD.GEST.AP                                                                        37
1746001601272    SC   INOLTRO                                                                           38
1747001601273         A.R.                                                                              39
1748001601274         FRAGILE                                                                           40
1749001601275    SC   DIR.PESAT.                                                                        41
1750001601276    SDU  BOLLO                                                                             42
1751001601277         T.C.P.                                                                            43
1752001601278    65   I.V.A.                                                                            44
