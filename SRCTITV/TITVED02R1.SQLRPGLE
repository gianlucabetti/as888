000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131023     FTABEL00F  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600131022     FTITA730C  IF   E           K DISK
000700131022     FNDREG04L  IF   E           K DISK
000800131022     FNDIVA01L  IF   E           K DISK
000900131022     FNDPAS01L  IF   E           K DISK
001000131019     Ftivgdtmp  UF   E             DISK
001100090323     FTIVAWWWT  UF A E             DISK    commit
001200031103
001300030924
001400041015     D*-----------
001500041015     D* VARIABILI D WRK
001600041015     D*-----------
001700131019     D prmppt          S             50
001800030924     D prmesito        S              1
001900030924     D wrkesito        S                   like(prmesito)
002000131004     D campo_alfa      S            512
002100131004     D currDate6       S              6
002200131004     D currDate6inv    S              6
002300131018     D currDate8       S              8
002400131004     D currHHMM        S              4
002500131019     D Conta_msg       S             10i 0 inz
002600131019     D Conta_item      S             10i 0 inz
002700131019     D Conta_segm      S             10i 0 inz
002800131022     D wPagamGG        S              3  0 inz
002900131023     D wCurrIVA        S              5  2 inz
003000131022     D wTotIMV         S             30  3 inz
003100131022     D wTotIVA         S             30  3 inz
003200131023     D wTotFATT        S             30  3 inz
003300131019     D savNFT          S                   inz like(vafNFT)
003400140107     D savFIV          S                   inz like(vafFIV)
003500131021     D wNAD_CA_2       S            256    inz
003600131021     D wNAD_CA_3       S            256    inz
003700131021     D wRFF_VA_1_sgm   S            256    inz
003800131030     D wSVN            S              1    inz
003900131023     D wVAN            S             13  3 inz
004000170906     D wIVA            S             13  4 inz
004100131030     D wDES            S             40    inz
004200131205     D wBOLLO          S             19  3 inz
004300170915     D wVAL2D          S             19  2 inz
004400171019     D wVAL3D          S             19  3 inz
004500131004
004600131004
004700131004     D*-----------
004800131004     D* DEFINIZIONE DS ESTERNE
004900131004     D*-----------
005000131004     D trul33ds      e ds                  inz
005100131004     D kpjba         e ds                  inz
005200131019     D tivafds       e ds                  inz extname(tivaf00t)
005300131023     D dscc          e ds                  inz
005400160208     D DTA4A         e ds
005500131022
005600131022
005700131022     D*------------
005800131022     D* SCHIERE DETTAGLI REGISTRAZIONE CONTABILE
005900131022     D*------------
006000131023     D skIVAIMP        S             19  3 dim(100)
006100131023     D skIVAALQ        S              5  2 dim(100)
006200131022     D skIMPONB        S             19  3 dim(100)
006300131023     D j1              S              3  0 inz
006400131030     D skSVN           S              1    DIM(100)
006500131030     D skSVNDES        S             40    DIM(100)
006600131030     D skSVNnoIVA      S              1    DIM(100)
006700131023     D j2              S              3  0 inz
006800131030     D skALC           S              5    DIM(100)
006900131030     D skALC_DES       S             40    DIM(100)
007000131023     D skMOA_8         S             19  3 DIM(100)
007100131023     D skTAX_7         S              5  2 DIM(100)
007200170906     D skMOA_124       S             19  4 DIM(100)
007300131023     D j3              S              3  0 inz
007400131030     D skCHRG_BRT      S              1    DIM(100)
007500131030     D skCHRG_CLI      S              5    DIM(100)
007600131030     D skCHRG_DES      S             40    DIM(100)
007700131030     D j4              S              3  0 inz
007800131022
007900131004
008000131004     D*------------------
008100131004     D* LINKING A DEFINIZIONI ESTERNE
008200131004     D*------------------
008300131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
008400131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
008500160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
008600131021     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
008700131021      /DEFINE DFTACTGRP_YES
008800131021     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
008900131021      /UNDEFINE DFTACTGRP_YES
009000131021
009100131021     ***************************************************************************
009200131021     **
009300131021     ** Definizione strutture dati.
009400131021     **
009500131021      **************************************************************************
009600131021     D tibsSocI0...
009700131021     D               E DS                  QUALIFIED
009800131021     D                                     INZ
009900131021     D tibsSocO0...
010000131021     D               E DS                  QUALIFIED
010100131021     D                                     INZ
010200131021     ***************************************************************************
010300131021     **
010400131021     ** Definizione variabili modulo/programma.
010500131021     **
010600131021     ***************************************************************************
010700131021     D prmRqsOpCode...
010800131021     D                 S             10A
010900131021     D prmRpyOpCode...
011000131021     D                 S             10A
011100131021     D prmRpyIdMsg...
011200131021     D                 S             10I 0
011300131021     D prmRqsFormato...
011400131021     D                 S             10A
011500131021     D prmRqsDataSize...
011600131021     D                 S             10I 0
011700131021     D prmRpyFormato...
011800131021     D                 S             10A
011900131021     D prmRpyDataSize...
012000131021     D                 S             10I 0
012100110705
012200031103
012300031103     D*-----------
012400031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
012500031103     D*-----------
012600131019     D DSPPT           DS
012700131030     D  w§PPTKSU                      7  0
012800131030     D  w§PPTCVT                      1
012900131018     D  w§UNB_ID_1                    5
013000131018     D  w§UNB_ID_2                   25
013100131004
013200131030
013300131030     D*-----------
013400131030     D* SCHIERE A PROGRAMMA
013500131030     D*-----------
013600160128     D CHRGCODES       S             60    DIM(48) CTDATA PERRCD(1)
013700131030
013800131030
013900131030     D*-----------
014000131030     D* RIDEFINIZIONE SCHIERA A PROGRAMMA CHRGCODES
014100131030     D*-----------
014200131030     D DS_CHRGCODE     DS
014300131030     D  CHRGCODE_BRT                  5
014400131030     D  CHRGCODE_CLI                  5
014500131030     D  CHRGCODE_DES                 40
014600131004
014700131030
014800131004     D*-----------
014900131018     D* COSTANTI / DEFAULT
015000131004     D*-----------
015100131018     D UNB_sgm         C                   CONST('UNB+UNOA:1+')
015200131007     D UNH_sgm_1       C                   CONST('UNH+')
015300131018     D UNH_sgm_2       C                   CONST('+INVOIC:D:96A:UN:EAN008')
015400131018     D BGM_sgm_1       C                   CONST('BGM+380+')
015500131018     D BGM_sgm_2       C                   CONST('+9')
015600131018     D DTM_3_1         C                   CONST('DTM+3:')
015700131018     D DTM_3_2         C                   CONST(':102')
015800131018     D DTM_137_1       C                   CONST('DTM+137:')
015900131018     D DTM_137_2       C                   CONST(':102')
016000131018     D RFF_IT_sgm      C                   CONST('RFF+IT:')
016100131021     D NAD_CA_1        C                   CONST('NAD+CA+')
016200131021     D NAD_CA_2        C                   CONST('BRT S.P.A.+PIAZZA DIAZ, 7+')
016300131021     D NAD_CA_3        C                   CONST('MILANO++20123+IT')
016400150309     D RFF_VA_1_sgm    C                   CONST('RFF+VA:IT04507990150')
016500131018     D NAD_SF_1        C                   CONST('NAD+SF+5450534001540::9++')
016600131128     D NAD_SF_2        C                   CONST('AMAZON EU SARL:')
016700131128     D NAD_SF_3        C                   CONST('CO AMAZON LOGISTICA SRL+')
016800131128     D NAD_SF_4        C                   CONST('STRADA DOGANA PO 2 T')
016900131128     D NAD_SF_5        C                   CONST('CASTEL SAN GIOVANNI++')
017000131128     D NAD_SF_6        C                   CONST('29015+IT')
017100140505     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000345::9++')
017200140505     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
017300140505     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
017400140505     D*NAD_IV_4        C                   CONST('AMAZON IT LOGISTICA SRL:')
017500140505     D*NAD_IV_5        C                   CONST('VIA FERR. APORTI 8 - ')
017600140505     D*NAD_IV_6        C                   CONST('IT07231660965+')
017700140505     D*NAD_IV_7        C                   CONST('MILANO++20125+IT')
017800140508     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000345::9++')
017900140508     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
018000140508     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
018100140508     D*NAD_IV_4        C                   CONST('PI IT05257010966:')
018200140508     D*NAD_IV_5        C                   CONST('RAPPR FISCALE AMAZON ITALIA:')
018300140508     D*NAD_IV_6        C                   CONST('LOGISTICA SRL VIA FERRANTE')
018400140508     D*NAD_IV_7        C                   CONST('APORTI 8+MILANO++20125+IT')
018500140516     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000055::9++')
018600140516     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
018700140516     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
018800140516     D*NAD_IV_4        C                   CONST('CO ACCOUNTING SERVICE CENTER')
018900140516     D*NAD_IV_5        C                   CONST(':1-9 THE GROVE+SLOUGH++')
019000140516     D*NAD_IV_6        C                   CONST('SL1 1QP+GB')
019100140516     D*NAD_IV_7        C                   CONST('')
019200150309     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000055::9++')
019300150309     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:5 RUE PLAETIS')
019400150309     D*NAD_IV_3        C                   CONST(' L-2338 LUXEMBOURG+C/O AP')
019500150309     D*NAD_IV_4        C                   CONST(' NON-INVENTORY FINANCIAL')
019600150309     D*NAD_IV_5        C                   CONST(' OPS:1-9 THE GROVE+')
019700150309     D*NAD_IV_6        C                   CONST('SLOUGH++SL1 1QP+GB')
019800150309     D*NAD_IV_7        C                   CONST('')
019900150309     D NAD_IV_1        C                   CONST('NAD+IV+5450534005845::9++')
020000150309     D NAD_IV_2        C                   CONST('AMAZON EU SARL, ')
020100150309     D NAD_IV_3        C                   CONST('SUCCURSALE ITALIANA+')
020200150309     D NAD_IV_4        C                   CONST('VIA FERRANTE APORTI 8+')
020300150309     D NAD_IV_5        C                   CONST('MILANO++20125+IT')
020400150309     D NAD_IV_6        C                   CONST('')
020500140516     D NAD_IV_7        C                   CONST('')
020600171005     D NAD_IV_1_2      C                   CONST('NAD+IV+5450534006941::9++')
020700171005     D NAD_IV_2_2      C                   CONST('AMAZON EU SARL, ')
020800171005     D NAD_IV_3_2      C                   CONST('SUCCURSALE ITALIANA+')
020900171005     D NAD_IV_4_2      C                   CONST('VIALE MONTE GRAPPA 3/5+')
021000171005     D NAD_IV_5_2      C                   CONST('MILANO++20124+IT')
021100171005     D NAD_IV_6_2      C                   CONST('')
021200171005     D NAD_IV_7_2      C                   CONST('')
021300150309     D*RFF_VA_2_sgm    C                   CONST('RFF+VA:LU20260743')
021400150309     D RFF_VA_2_sgm    C                   CONST('RFF+VA:IT08973230967')
021500131018     D CUX_sgm         C                   CONST('CUX+2:EUR:4')
021600131022     D PAT_sgm         C                   CONST('PAT+1++5::D:')
021700131019     D PCD_sgm         C                   CONST('PCD+7:0:13')
021800131019     D LIN_sgm         C                   CONST('LIN+')
021900131023     D IMD_sgm         C                   CONST('IMD+F+8+:::BRT_IT')
022000140407     D IMD_sgm_Bollo   C                   CONST('IMD+F+8+:::STAMP DUTY')
022100170411     D MEA_WT_sgm      C                   CONST('MEA+WT+G+KG:')
022200131019     D MOA_203_sgm     C                   CONST('MOA+203:')
022300131019     D PRI_sgm         C                   CONST('PRI+CAL:')
022400131022     D TAX_sgm         C                   CONST('TAX+7+VAT+++:::')
022500131019     D NAD_CN          C                   CONST('NAD+CN++')
022600131019     D RFF_AAM_sgm     C                   CONST('RFF+AAM:')
022700131019     D RFF_CR_sgm      C                   CONST('RFF+CR:')
022800131019     D RFF_ACC_sgm     C                   CONST('RFF+ACC:')
022900131019     D DTM_200_1       C                   CONST('DTM+200:')
023000131019     D DTM_200_2       C                   CONST(':102')
023100131023     D ALC_sgm         C                   CONST('ALC+C++++')
023200140407     D ALC_sgm_Bollo   C                   CONST('ALC+C++++SDU:::STAMP DUTY')
023300131023     D MOA_8_sgm       C                   CONST('MOA+8:')
023400131023     D UNS_sgm         C                   CONST('UNS+S')
023500131019     D CNT_2_sgm       C                   CONST('CNT+2:')
023600131019     D MOA_79_sgm      C                   CONST('MOA+79:')
023700131019     D MOA_58_sgm      C                   CONST('MOA+58:')
023800131019     D MOA_77_sgm      C                   CONST('MOA+77:')
023900131205     D MOA_124_sgm     C                   CONST('MOA+124:')
024000131205     D MOA_125_sgm     C                   CONST('MOA+125:')
024100131004     D UNT_sgm         C                   CONST('UNT+')
024200131004     D UNZ_sgm         C                   CONST('UNZ+')
024300131004     D piu             C                   CONST('+')
024400131004     D diviso          C                   CONST(':')
024500131004     D apice           C                   CONST('''')
024600131022     D societa         C                   CONST('201')
024700131021     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
024800131021     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
024900131004
025000030924
025100030325
025200030924     C* MAIN/
025300170123     C*
025400170123     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
025500170123     C
025600170123     C/EXEC SQL
025700170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
025800170123     C/END-EXEC
025900131021     C*
026000131021     C* Inizializzo il TIBSSOCR
026100131025     C                   SETOFF                                       2021
026200131021     C                   SETON                                        20
026300131021     C                   EXSR      EXESOCR
026400041015     C*
026500041015     C* Carica i dati tabellati
026600041015     C                   EXSR      cartab
026700041015     C*
026800041015     C* Esegue traduzione
026900131004     C                   setoff                                       31
027000131004     C  N31              EXSR      chkRECFILMBR
027100131004     C  N31              EXSR      rtvID
027200131004     C  N31              EXSR      wriHeader
027300131004     C  N31              EXSR      traduci
027400131004     C  N31              EXSR      wriFooter
027500131004 xxx C***31              EXSR      sndAlert
027600131021     C*
027700131021     C* Chiudo il TIBSSOCR
027800131025     C                   SETOFF                                       2021
027900131021     C                   SETON                                        21
028000131021     C                   EXSR      EXESOCR
028100160208     C*
028200160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
028300160208     C                   CALL      'UBTA400R'
028400160208     C                   PARM                    UBTA4IOPZ
028500160208     C                   PARM      'C'           UBTA4ITLA
028600160208     C                   PARM                    UBTA4IAAS
028700160208     C                   PARM                    UBTA4ILNP
028800160208     C                   PARM                    UBTA4INRS
028900160208     C                   PARM                    UBTA4INSP
029000160208     C                   PARM                    UBTA4ITRC
029100160208     C                   PARM                    UBTA4OERR
029200160208     C                   PARM                    UBTA4ODS
029300160208     C                   PARM                    UBTA4OLEN
029400160208     C                   PARM                    UBTA4ODATI
029500110705     C*
029600921023     C                   SETON                                        LR
029700131004
029800131004
029900131004
030000131004     C*----------------------------------------------------*
030100131004     C*  STACCO ID UNIVOCO
030200131004     C*----------------------------------------------------*
030300131004     C     RTVID         BEGSR
030400131004     C*
030500131004     C                   z-add     *zeros        currUNIQ_ID      15 0
030600131004     C*
030700131004     C* NSP => Stacco un numeratore da AZNUM
030800131004     C                   clear                   TRUL33DS
030900131004     C                   eval      I33OPE = *zeros
031000131004     C                   eval      I33CNU = 302
031100131004     C                   eval      I33NUM = 1
031200131004     C                   movel     TRUL33DS      KPJBU
031300131004     C                   call      'TRUL33R'
031400131004     C                   parm                    KPJBA
031500131004     C                   movel     KPJBU         TRUL33DS
031600131004     C                   if        O33ERR = *zeros
031700131004     C                   z-add     O33NRF        currUNIQ_ID
031800131004     C                   else
031900131004     C                   seton                                        31
032000131004     C                   endif
032100131004     C*
032200131004     C                   ENDSR
032300131021
032400131021
032500131021     C*----------------------------------------------------*
032600131021     C*  INIZIALIZZO / CHIUDO IL TIBSSOCR
032700131021     C*----------------------------------------------------*
032800131021     C     EXESOCR       BEGSR
032900131021     C*
033000131021     C* Inizializzo il programma.
033100131021     C                   IF        *IN20
033200131021     C                   CALL      'TIBSSOCR'
033300131021     C                   PARM      'INIT'        prmRqsOpCode
033400131021     C                   PARM                    prmRpyOpCode
033500131021     C                   PARM                    prmRpyIdMsg
033600131021     C                   ENDIF
033700131021     C*
033800131021     C* Chiudo il programma.
033900131021     C                   IF        *IN21
034000131021     C                   CALL      'TIBSSOCR'
034100131021     C                   PARM      'FINALIZE'    prmRqsOpCode
034200131021     C                   PARM                    prmRpyOpCode
034300131021     C                   PARM                    prmRpyIdMsg
034400131021     C                   ENDIF
034500131021     C*
034600131021     C                   ENDSR
034700131004
034800131021
034900131004     C     chkRECFILMBR  BEGSR
035000131004     C*
035100131004     C     *start        setll     TIVAWWWT
035200131004     C                   if        %found(TIVAWWWT)
035300131004     C                   seton                                        31
035400131004     C                   endif
035500131004     C*
035600131004     C                   ENDSR
035700131021
035800131021
035900131021     C     Decod_societa BEGSR
036000131021     C*
036100131021     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
036200131021     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
036300131021     C                   RESET                   tibsSocI0
036400131022     C                   EVAL      tibsSocI0.idSocieta  = societa
036500131021     C                   EVAL      tibsSocI0.dtValidita = %date(vafDFT)
036600131021     C
036700131021     C                   CALL      'TIBSSOCR'
036800131021     C                   PARM      'GETANAGRAF'  prmRqsOpCode
036900131021     C                   PARM      *BLANK        prmRpyOpCode
037000131021     C                   PARM      *ZERO         prmRpyIdMsg
037100131021     C                   PARM      'TIBSSOCI0'   prmRqsFormato
037200131021     C                   PARM                    tibsSocI0
037300131021     C                   PARM                    prmRqsDataSize
037400131021     C                   PARM      'TIBSSOCO0'   prmRpyFormato
037500131021     C                   PARM                    tibsSocO0
037600131021     C                   PARM                    prmRpyDataSize
037700131021     C                   if         PRMRPYIDMSG >= 0
037800131021     C                              and TIBSSOCO0.IDSOCIETA <> *blank
037900131021     C     minu:maiu     xlate     TIBSSOCO0     TIBSSOCO0
038000131021     C                   if        TIBSSOCO0.SEDLEGNAZ = *blanks
038100131021     C                   eval      TIBSSOCO0.SEDLEGNAZ = 'IT'
038200131025     C                   endif
038300131025     C*
038400131025     C* Verifico eventuali caratteri non consentiti dal EDIFACT
038500131025     C                   movel     *blanks       wAlfaSOCRAG     512
038600131025     C                   movel     *blanks       wAlfaSOCIND     512
038700131025     C                   movel     *blanks       wAlfaSOCLOC     512
038800131025     C* SOC RAG
038900131025     C                   eval      campo_alfa = TIBSSOCO0.RAGSOCIALE
039000131025     C                   exsr      Char_Speciali
039100131025     C                   eval      wAlfaSOCRAG = campo_alfa
039200131025     C* SOC IND
039300131025     C                   eval      campo_alfa = TIBSSOCO0.SEDLEGIND
039400131025     C                   exsr      Char_Speciali
039500131025     C                   eval      wAlfaSOCIND = campo_alfa
039600131025     C* SOC LOC
039700131025     C                   eval      campo_alfa = TIBSSOCO0.SEDLEGLOC
039800131025     C                   exsr      Char_Speciali
039900131025     C                   eval      wAlfaSOCLOC = campo_alfa
040000131025     C*
040100131025     C                   eval      wNAD_CA_2     = %trim(wAlfaSOCRAG)          +
040200131021     C                                             piu                         +
040300131025     C                                             %trim(wAlfaSOCIND)          +
040400131021     C                                             piu
040500131025     C                   eval      wNAD_CA_3     = %trim(wAlfaSOCLOC)          +
040600131021     C                                             piu                         +
040700131021     C                                             piu                         +
040800131021     C                                             %trim(TIBSSOCO0.SEDLEGCAP)  +
040900131021     C                                             piu                         +
041000131021     C                                             %trim(TIBSSOCO0.SEDLEGNAZ)
041100131021     C                   eval      wRFF_VA_1_sgm = 'RFF+VA:'                   +
041200131021     C                                             %trim(TIBSSOCO0.PARTITAIVA)
041300131021     C                   else
041400131021     C                   eval      wNAD_CA_2     = NAD_CA_2
041500131021     C                   eval      wNAD_CA_3     = NAD_CA_3
041600131021     C                   eval      wRFF_VA_1_sgm = RFF_VA_1_sgm
041700131021     C                   endif
041800131021     C*
041900131021     C                   ENDSR
042000131004
042100131004
042200131004     C     wriOUTPUT     BEGSR
042300131004     C*
042400131004     C                   write     TIVAW000
042500131004     C*
042600131004     C                   ENDSR
042700130807
042800041015
042900131023     C     cartab        BEGSR
043000041015     C*
043100131023     C* Reperisco le descrizioni delle varie
043200131023     C                   z-add     *zeros        j2
043300131023     C                   z-add     1             tblKUT
043400131023     C                   movel(P)  'CC'          tblCOD
043500131023     C     KEYtab00_P1   setll     TABEL00F
043600131023     C                   if        %equal(TABEL00F)
043700131023     C     KEYtab00_P1   reade     TABEL00F
043800131023     C                   dow       not %eof(TABEL00F)
043900131030     C                   if        %subst(tblKEY:1:5) = 'VARIE'
044000131023     C                   add       1             j2
044100131030     C                   eval      skSVN(j2) = %subst(tblKEY:8:1)
044200131030     C                   movel(P)  tblUNI        DSCC
044300131023     C                   movel(P)  §CCDES        skSVNDES(j2)
044400131023     C                   if        §CCJEI <> *blanks
044500131023     C                   eval      skSVNnoIVA(j2) = 'Y'
044600131023     C                   endif
044700131030     C                   endif
044800131023     C     KEYtab00_P1   reade     TABEL00F
044900131023     C                   enddo
045000131023     C                   endif
045100131030     C*
045200131030     C* Carico i codici varie personalizzati per cliente
045300131030     C                   z-add     1             j4
045400131030     C                   dow       j4 <= %elem(CHRGCODES) and
045500131030     C                                   CHRGCODES(j4) <> *blanks
045600131030     C                   eval      DS_CHRGCODE = CHRGCODES(j4)
045700131030     C                   eval      skCHRG_BRT(j4) = CHRGCODE_BRT
045800131030     C                   eval      skCHRG_CLI(j4) = CHRGCODE_CLI
045900131030     C                   eval      skCHRG_DES(j4) = CHRGCODE_DES
046000131030     C                   add       1             j4
046100131030     C                   enddo
046200041015     C*
046300041015     C                   ENDSR
046400131004
046500131004
046600131004     C     Char_Speciali BEGSR
046700131111     C*
046800131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
046900131111     C*
047000131111     C/EXEC SQL
047100131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
047200131111     C/END-EXEC
047300131111     C*
047400131111     C/EXEC SQL
047500131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
047600131111     C/END-EXEC
047700131111     C*
047800131111     C/EXEC SQL
047900131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
048000131111     C/END-EXEC
048100131111     C*
048200131111     C/EXEC SQL
048300131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
048400131111     C/END-EXEC
048500131111     C*
048600131111     C/EXEC SQL
048700131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
048800131111     C/END-EXEC
048900131111     C*
049000131004     C                   ENDSR
049100131004
049200131004
049300131004     C     wriHeader     BEGSR
049400131010     C*
049500131019     C*  UNB -
049600131004     C                   eval      vawDTA = UNB_sgm
049700131018     C                              + %trim(w§UNB_ID_1)+%trim(w§UNB_ID_2)
049800131004     C                              +   piu
049900131004     C                              + %trim(currDate6inv)
050000131004     C                              +   diviso
050100131004     C                              + %trim(currHHMM)
050200131004     C                              +   piu
050300131004     C                              + %trim(
050400131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
050500131004     C                              +   apice
050600131004     C                   exsr      wriOUTPUT
050700131004     C*
050800131004     C                   ENDSR
050900131019
051000131019
051100131019     C     wriSingle     BEGSR
051200131021     C*
051300131021     C* Reperisco i dati societari di fatturazione
051400131021     C                   exsr      Decod_societa
051500131022     C*
051600131022     C* Reperisco i dati della registrazione contabile
051700131022     C                   exsr      rtvContab
051800171005     C*
051900171005     C* Gestione dati sociali in relazione alla data fattura
052000171113     C                   if        VAFDFT >= 20171030
052100171005     C                   seton                                        43
052200171005     C                   else
052300171005     C                   setoff                                       43
052400171005     C                   endif
052500131019     C*
052600131019     C*  UNH -
052700131019     C                   eval      Conta_segm  = Conta_segm  + 1
052800131019     C                   eval      vawDTA = UNH_sgm_1
052900131022     C                              + %trim(%editc(Conta_msg:'3'))
053000131019     C                              + UNH_sgm_2
053100131019     C                              +   apice
053200131019     C                   exsr      wriOUTPUT
053300131019     C*
053400131019     C*  BGM -
053500131019     C                   eval      Conta_segm  = Conta_segm  + 1
053600131019     C                   eval      vawDTA = BGM_sgm_1
053700140506     C                              + %trim(%editc(vafNFT:'X'))
053800140506     C                              + %trim(%editc(vafFIV:'X'))
053900131019     C                              + BGM_sgm_2
054000131019     C                              +   apice
054100131019     C                   exsr      wriOUTPUT
054200131019     C*
054300131019     C*  DTM 3 -
054400131019     C                   eval      Conta_segm  = Conta_segm  + 1
054500131019     C                   eval      vawDTA = DTM_3_1
054600131022     C                              + %trim(%editc(vafDFT:'3'))
054700131019     C                              + DTM_3_2
054800131019     C                              +   apice
054900131019     C                   exsr      wriOUTPUT
055000131019     C*
055100131023     C*  DTM 137 -
055200131019     C                   eval      Conta_segm  = Conta_segm  + 1
055300131019     C                   eval      vawDTA = DTM_137_1
055400131023     C                              + %trim(currDate8)
055500131019     C                              + DTM_137_2
055600131019     C                              +   apice
055700131019     C                   exsr      wriOUTPUT
055800131019     C*
055900131019     C*  RFF IT -
056000131019     C                   eval      Conta_segm  = Conta_segm  + 1
056100131019     C                   eval      vawDTA = RFF_IT_sgm
056200131030     C                              + %trim(%editc(w§PPTKSU:'3'))
056300131019     C                              +   apice
056400131019     C                   exsr      wriOUTPUT
056500131019     C*
056600131019     C*  NAD CA -
056700131019     C                   eval      Conta_segm  = Conta_segm  + 1
056800131021     C                   eval      vawDTA = NAD_CA_1
056900131019     C                              + %trim(w§UNB_ID_1)
057000131019     C                              +   piu
057100131019     C                              +   piu
057200131021     C                              + %trim(wNAD_CA_2)
057300131021     C                              + %trim(wNAD_CA_3)
057400131019     C                              +   apice
057500131019     C                   exsr      wriOUTPUT
057600131019     C*
057700131019     C*  RFF VA 1 -
057800131019     C                   eval      Conta_segm  = Conta_segm  + 1
057900131025     C                   eval      vawDTA = %trim(wRFF_VA_1_sgm)
058000131019     C                              +   apice
058100131019     C                   exsr      wriOUTPUT
058200131019     C*
058300131019     C*  NAD SF -
058400131019     C                   eval      Conta_segm  = Conta_segm  + 1
058500131019     C                   eval      vawDTA = NAD_SF_1
058600131019     C                              + NAD_SF_2
058700131019     C                              + NAD_SF_3
058800131019     C                              + NAD_SF_4
058900131019     C                              + NAD_SF_5
059000131128     C                              + NAD_SF_6
059100131019     C                              +   apice
059200131019     C                   exsr      wriOUTPUT
059300131019     C*
059400131019     C*  NAD IV -
059500131019     C                   eval      Conta_segm  = Conta_segm  + 1
059600171005     C  N43              eval      vawDTA = NAD_IV_1
059700131019     C                              + NAD_IV_2
059800131019     C                              + NAD_IV_3
059900131019     C                              + NAD_IV_4
060000131019     C                              + NAD_IV_5
060100131019     C                              + NAD_IV_6
060200131128     C                              + NAD_IV_7
060300131019     C                              +   apice
060400171005     C   43              eval      vawDTA = NAD_IV_1_2
060500171005     C                              + NAD_IV_2_2
060600171005     C                              + NAD_IV_3_2
060700171005     C                              + NAD_IV_4_2
060800171005     C                              + NAD_IV_5_2
060900171005     C                              + NAD_IV_6_2
061000171005     C                              + NAD_IV_7_2
061100171005     C                              +   apice
061200131019     C                   exsr      wriOUTPUT
061300131019     C*
061400131019     C*  RFF VA 2 -
061500131019     C                   eval      Conta_segm  = Conta_segm  + 1
061600131019     C                   eval      vawDTA = RFF_VA_2_sgm
061700131019     C                              +   apice
061800131019     C                   exsr      wriOUTPUT
061900131019     C*
062000131019     C*  CUX -
062100131019     C                   eval      Conta_segm  = Conta_segm  + 1
062200131019     C                   eval      vawDTA = CUX_sgm
062300131019     C                              +   apice
062400131019     C                   exsr      wriOUTPUT
062500131019     C*
062600131019     C*  PAT -
062700131019     C                   eval      Conta_segm  = Conta_segm  + 1
062800131019     C                   eval      vawDTA = PAT_sgm
062900131022     C                              + %trim(%editc(wPagamGG:'3'))
063000131019     C                              +   apice
063100131019     C                   exsr      wriOUTPUT
063200131019     C*
063300131019     C*  PCD -
063400131019     C                   eval      Conta_segm  = Conta_segm  + 1
063500131019     C                   eval      vawDTA = PCD_sgm
063600131019     C                              +   apice
063700131019     C                   exsr      wriOUTPUT
063800131019     C*
063900131019     C                   ENDSR
064000131019
064100131019
064200140407     C     wriSummary    BEGSR
064300140407     C*
064400140407     C* Come ultima spedizione e prima dei dati di sommario della fattura corrente
064500140407     C* si inserisce una bolla fake relativa al bollo fattura in esenzione IVA
064600150401     C* (solo se effettivamente presente il bollo)
064700150401     C                   if        wBOLLO > *zeros
064800140407     C                   exsr      exeBollaBollo
064900150401     C                   endif
065000131019     C*
065100131019     C*  UNS -
065200131019     C                   eval      Conta_segm  = Conta_segm  + 1
065300131019     C                   eval      vawDTA = UNS_sgm
065400131019     C                              +   apice
065500131019     C                   exsr      wriOUTPUT
065600131019     C*
065700131019     C*  CNT 2 -
065800131019     C                   eval      Conta_segm  = Conta_segm  + 1
065900131019     C                   eval      vawDTA = CNT_2_sgm
066000131022     C                              + %trim(%editc(Conta_item:'3'))
066100131019     C                              +   apice
066200131019     C                   exsr      wriOUTPUT
066300131019     C*
066400131019     C*  MOA 79 -
066500131019     C                   eval      Conta_segm  = Conta_segm  + 1
066600131019     C                   eval      vawDTA = MOA_79_sgm
066700131219     C                              + %trim(%editc(wTotIMV:'3'))
066800131019     C                              +   apice
066900131019     C                   exsr      wriOUTPUT
067000131019     C*
067100131205     C*  MOA 58 -
067200140407     C                   eval      Conta_segm  = Conta_segm  + 1
067300140407     C                   eval      vawDTA = MOA_58_sgm
067400140407     C                              + %trim(%editc(wBOLLO:'3'))
067500140407     C                              +   apice
067600140407     C                   exsr      wriOUTPUT
067700131019     C*
067800131019     C*  MOA 77 -
067900131019     C                   eval      Conta_segm  = Conta_segm  + 1
068000131019     C                   eval      vawDTA = MOA_77_sgm
068100131022     C                              + %trim(%editc(wTotFATT:'3'))
068200131019     C                              +   apice
068300131019     C                   exsr      wriOUTPUT
068400131022     C*
068500131022     C* Ciclo per tutti gli imponibili/aliquote presenti in fattura
068600131023     C                   dow       j1 > *zeros
068700131022     C*
068800131022     C*  TAX -
068900131022     C                   eval      Conta_segm  = Conta_segm  + 1
069000131022     C                   eval      vawDTA = TAX_sgm
069100131023     C                              + %trim(%editc(skIVAALQ(j1):'3'))
069200131019     C                              +   apice
069300131019     C                   exsr      wriOUTPUT
069400131019     C*
069500171116     C*** Amazon  vuole il segmento MOA+124 solo a livello di riepilogo
069600131205     C*  MOA 124 -
069700171116     C                   eval      Conta_segm  = Conta_segm  + 1
069800171116     C                   eval      vawDTA = MOA_124_sgm
069900171116     C                              + %trim(%editc(skIVAIMP(j1):'3'))
070000171116     C                              +   apice
070100171116     C                   exsr      wriOUTPUT
070200131019     C*
070300131205     C*  MOA 125 -
070400131019     C                   eval      Conta_segm  = Conta_segm  + 1
070500131019     C                   eval      vawDTA = MOA_125_sgm
070600131023     C                              + %trim(%editc(skIMPONB(j1):'3'))
070700131022     C                              +   apice
070800131022     C                   exsr      wriOUTPUT
070900131022     C*
071000131023     C                   sub       1             j1
071100131022     C                   enddo
071200131019     C*
071300131019     C*  UNT -
071400131019     C                   eval      Conta_segm  = Conta_segm  + 1
071500131019     C                   eval      vawDTA = UNT_sgm
071600131022     C                              + %trim(%editc(Conta_segm:'3'))
071700131019     C                              +   piu
071800131022     C                              + %trim(%editc(Conta_msg:'3'))
071900131019     C                              +   apice
072000131019     C                   exsr      wriOUTPUT
072100131019     C*
072200131019     C                   ENDSR
072300131019
072400131004
072500131004     C     wriFooter     BEGSR
072600131004     C*
072700131004     C*  UNZ -
072800131004     C                   eval      vawDTA = UNZ_sgm
072900131004     C                              +   piu
073000131022     C                              + %trim(%editc(Conta_msg:'3'))
073100131004     C                              +   piu
073200131004     C                              + %trim(
073300131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
073400131004     C                              +   apice
073500131004     C                   exsr      wriOUTPUT
073600131004     C*
073700131004     C                   ENDSR
073800131019
073900131019
074000131019     C     exeInz        BEGSR
074100131019     C*
074200131019     C                   clear                   wTotIMV
074300131019     C                   clear                   wTotIVA
074400131019     C                   clear                   wTotFATT
074500131019     C                   clear                   Conta_msg
074600131019     C                   clear                   Conta_item
074700131019     C                   clear                   Conta_segm
074800131019     C*
074900131019     C                   ENDSR
075000030924
075100030924
075200991027     C     traduci       BEGSR
075300131004     C*
075400131019     C* Inizializzo i contatori a livello di transazione/messaggio
075500131019     C                   exsr      exeInz
075600131019     C*
075700131019     C                   READ      tivgdtmp
075800131019     C                   dow       not %eof(tivgdtmp)
075900131019     C                   movel     vgddta        tivafds
076000131022     C*
076100131022     C* Innanzitutto chaino la bolla direttamente su TITAS
076200131022     C     KEYtas30P     chain     titas30c
076300131022     C                   if        %found(titas30c)
076400160208     C*
076500160208     C* Reprrisco NATURA MERCE
076600160208     C                   EXSR      RTVNAS
076700131019     C*
076800140107     C* Effettuo rottura di codice per numero fattura/registro IVA
076900140107     C                   if        savNFT <> vafNFT AND savFIV <> vafFIV
077000140107     C                   if        savNFT  = *zeros AND savFIV  = *zeros
077100131019     C* Scarico i dati di testata fattura
077200131019     C                   exsr      wriSingle
077300131022     C* Elaboro la bolla corrente
077400131022     C                   exsr      exeBolla
077500131019     C*
077600131019     C                   else
077700131019     C* Elaboro la bolla corrente
077800131022     C                   exsr      exeBolla
077900131022     C* Scarico i dati di sommario a livello di fattura
078000131022     C                   exsr      wriSummary
078100131019     C* Inizializzo i contatori a livello di transazione/messaggio
078200131019     C                   exsr      exeInz
078300131019     C* Scarico i dati di testata fattura
078400131019     C                   exsr      wriSingle
078500131019     C                   endif
078600131019     C*
078700131019     C                   else
078800131019     C* Elaboro la bolla corrente
078900131022     C                   exsr      exeBolla
079000131019     C                   endif
079100131019     C
079200131019     C* Salvo i nuovi valori della rottuta di codice
079300131019     C                   eval      savNFT = vafNFT
079400140107     C                   eval      savFIV = vafFIV
079500131022     C*
079600131022     C                   endif
079700131019     C*
079800131019     C                   delete    tivgd000
079900131019     C*                                                               50
080000131019     C                   read      tivgdtmp
080100131019     C                   enddo
080200131022     C*
080300131022     C* Scarico i dati di sommario a livello di fattura
080400131022     C                   exsr      wriSummary
080500991027     C*
080600030325     C                   EVAL      wrkesito = '0'
080700991027     C*
080800910830     C                   ENDSR
080900131019
081000131019
081100140407     C     exeBolla      BEGSR
081200131019     C*
081300131019     C* Incremento il contatore di item (ovvero spedizione)
081400131019     C                   add       1             Conta_item
081500131023     C*
081600131023     C* Reperisco i dati di dettaglio fatturazione spedizione corrente
081700131023     C                   exsr      rtvSvn
081800131019     C*
081900131019     C* Scarico il buffer d output
082000131019     C                   exsr      wriDettagli
082100131019     C*
082200131019     C                   ENDSR
082300140407
082400140407
082500140407     C     exeBollaBollo BEGSR
082600140407     C*
082700140407     C* Incremento il contatore di item (ovvero spedizione)
082800140407     C                   add       1             Conta_item
082900140407     C*
083000140407     C* Scarico il buffer d output
083100140407     C                   exsr      wriBollaFake
083200140407     C*
083300140407     C                   ENDSR
083400140407
083500140407
083600140407     C     wriBollaFake  BEGSR
083700140407     C*
083800140407     C* Incremento il contatore di messaggio (al interno della transazione)
083900140407     C                   eval      Conta_msg = Conta_msg + 1
084000140407     C*
084100140407     C* Compongo segmenti per dati di dettaglio spedizione corrente
084200140407     C*
084300140407     C*  LIN -
084400140407     C                   eval      Conta_segm  = Conta_segm  + 1
084500140407     C                   eval      vawDTA = LIN_sgm
084600140407     C                              + %trim(%editc(Conta_item:'3'))
084700140407     C                              +   apice
084800140407     C                   exsr      wriOUTPUT
084900140407     C*
085000140407     C*  IMD -
085100140407     C                   eval      Conta_segm  = Conta_segm  + 1
085200140407     C                   eval      vawDTA = IMD_sgm_Bollo
085300140407     C                              +   apice
085400140407     C                   exsr      wriOUTPUT
085500170411     C*
085600170411     C*  MEA WT -
085700170411     C                   eval      Conta_segm  = Conta_segm  + 1
085800170411     C                   eval      vawDTA = MEA_WT_sgm
085900170411     C                              + '0'
086000170411     C                              +   apice
086100170411     C                   exsr      wriOUTPUT
086200140407     C*
086300140407     C*  MOA 203 -
086400140407     C                   eval      Conta_segm  = Conta_segm  + 1
086500140407     C                   eval      vawDTA = MOA_203_sgm
086600140407     C                              + '0'
086700140407     C                              +   apice
086800140407     C                   exsr      wriOUTPUT
086900140407     C*
087000140407     C*  PRI -
087100140407     C                   eval      Conta_segm  = Conta_segm  + 1
087200140407     C                   eval      vawDTA = PRI_sgm
087300140407     C                              + '0'
087400140407     C                              +   apice
087500140407     C                   exsr      wriOUTPUT
087600140407     C*
087700140407     C*  TAX -
087800140407     C                   eval      Conta_segm  = Conta_segm  + 1
087900140407     C                   eval      vawDTA = TAX_sgm
088000140407     C                              + '0'
088100140407     C                              +   apice
088200140407     C                   exsr      wriOUTPUT
088300170904     C*
088400171113     C*** Amazon NON vuole questo segmento
088500171113     C*** per cui si disattiva la modifica e poi se ne riparla
088600170904     C*  MOA 124 -
088700171113     C***                eval      Conta_segm  = Conta_segm  + 1
088800171113     C***                eval      vawDTA = MOA_124_sgm
088900171113     C***                           + '0'
089000171113     C***                           +   apice
089100171113     C***                exsr      wriOUTPUT
089200140407     C*
089300140407     C*  NAD CN -
089400140407     C                   eval      Conta_segm  = Conta_segm  + 1
089500140407     C                   eval      vawDTA = NAD_CN
089600140407     C                              + 'DUMMY DATA'
089700140407     C                              +   piu
089800140407     C                              +   piu
089900140407     C                              + 'DUMMY DATA'
090000140407     C                              +   piu
090100140407     C                              + 'BOLOGNA'
090200140407     C                              +   piu
090300140407     C                              + 'BO'
090400140407     C                              +   piu
090500140407     C                              + '40138'
090600140407     C                              +   piu
090700140407     C                              + 'IT'
090800140407     C                              +   apice
090900140407     C                   exsr      wriOUTPUT
091000140407     C*
091100140407     C*  RFF AAM -
091200140407     C                   eval      Conta_segm  = Conta_segm  + 1
091300140407     C                   eval      vawDTA = RFF_AAM_sgm
091400140407     C                              + '00000'
091500140407     C                              +   apice
091600140407     C                   exsr      wriOUTPUT
091700140407     C*
091800140407     C*  RFF CR -
091900140407     C                   eval      Conta_segm  = Conta_segm  + 1
092000140407     C                   eval      vawDTA = RFF_CR_sgm
092100140407     C                              + '00000'
092200140407     C                              +   apice
092300140407     C                   exsr      wriOUTPUT
092400140407     C*
092500140407     C*  RFF ACC -
092600140407     C                   eval      Conta_segm  = Conta_segm  + 1
092700140407     C                   eval      vawDTA = RFF_ACC_sgm
092800140407     C                              + '00000'
092900140407     C                              +   apice
093000140407     C                   exsr      wriOUTPUT
093100140407     C*
093200140407     C*  DTM 200 -
093300140407     C                   eval      Conta_segm  = Conta_segm  + 1
093400140407     C                   eval      vawDTA = DTM_200_1
093500140407     C                              + %trim(%editc(vafDFT:'X'))
093600140407     C                              + DTM_200_2
093700140407     C                              +   apice
093800140407     C                   exsr      wriOUTPUT
093900140407     C*
094000140407     C*  ALC -
094100140407     C                   eval      Conta_segm  = Conta_segm  + 1
094200140407     C                   eval      vawDTA = ALC_sgm_Bollo
094300140407     C                              + %trim(skALC(j3))
094400140407     C                              +   apice
094500140407     C                   exsr      wriOUTPUT
094600140407     C*
094700140407     C*  MOA 8 -
094800140407     C                   eval      Conta_segm  = Conta_segm  + 1
094900171019     C                   eval(h)   wVAL3D = wBOLLO
095000140407     C                   eval      vawDTA = MOA_8_sgm
095100171019     C                              + %trim(%editc(wVAL3D:'3'))
095200140407     C                              +   apice
095300140407     C                   exsr      wriOUTPUT
095400140407     C*
095500140407     C*  TAX 7 -
095600140407     C                   eval      Conta_segm  = Conta_segm  + 1
095700140407     C                   eval      vawDTA = TAX_sgm
095800140407     C                              + '0'
095900140407     C                              +   apice
096000140407     C                   exsr      wriOUTPUT
096100170906     C*
096200171113     C*** Amazon NON vuole questo segmento
096300171113     C*** per cui si disattiva la modifica e poi se ne riparla
096400170906     C*  MOA 124 -
096500171113     C***                eval      Conta_segm  = Conta_segm  + 1
096600171113     C***                eval      vawDTA = MOA_124_sgm
096700171113     C***                           + '0'
096800171113     C***                           +   apice
096900171113     C***                exsr      wriOUTPUT
097000140407     C*
097100140407     C                   ENDSR
097200131022
097300131022
097400131022     C     rtvContab     BEGSR
097500131022     C*
097600131205     C                   clear                   wBOLLO
097700131023     C                   z-add     *zeros        j1
097800131022     C                   clear                   NDREG000
097900131022     C*
098000131022     C                   eval      REGSOCIETA = societa
098100131022     C                   eval      REGCTB     = 'CG'
098200131022     C                   eval      REGDTREG   = %date(vafDFT)
098300131022     C                   eval      REGNRREG   = vafNFT
098400131022     C                   eval      REGSERREG  = '2'+ %trim(%editc(vafFIV:'X'))
098500131022     C*
098600131022     C* Aggancio la testata della registrazione contabile
098700131022     C     KEYreg04C     chain     ndreg04l
098800131022     C                   if        %found(ndreg04l)
098900131022     C*
099000131022     C* Filtro solamente le caussali 'NFT0'
099100131022     C                   if        REGCAUSTES = 'NFT0'
099200131023     C*
099300131023     C* Reperisco subito importo totale fattura
099400131023     C                   eval      wTotFATT = wTotFATT + REGIMPORTO
099500131022     C*
099600131022     C* Aggancio il dettaglio della registrazione contabile
099700131022     C     KEYiva01P     setll     ndiva01l
099800131022     C                   if        %found(ndiva01l)
099900131022     C     KEYiva01P     reade     ndiva01l
100000131022     C                   dow       not %eof(ndiva01l)
100100131022     C*
100200131022     C* Filtro solamente le caussali 'NFT0'
100300131022     C                   if        IVACAUSTES = 'NFT0'
100400131022     C*
100500131022     C* Reperisco i dati relativi a aliquota IVA, imponibile, importo IVA
100600131023     C                   if        IVAALIQ > wCurrIVA
100700131022     C                   eval      wCurrIVA = IVAALIQ
100800131022     C                   endif
100900131022     C*
101000131022     C                   eval      wTotIVA  = wTotIVA  + IVAIMPORTO
101100131219 xxx C***                eval      wTotIMV  = wTotIMV  + IVAIMPONIB
101200131022     C*
101300131205     C* Nei righi IVA tengo separato il bollo
101400131205     C                   if        IVARIFIVA = '150'
101500131205     C                   eval      wBOLLO = IVAIMPONIB
101600131205     C                   else
101700131023     C                   add       1             j1
101800131023     C                   eval      skIVAIMP(j1) = IVAIMPORTO
101900131023     C                   eval      skIVAALQ(j1) = IVAALIQ
102000131205     C                   eval      skIMPONB(j1) = IVAIMPONIB
102100131205     C                   endif
102200131022     C*
102300131022     C                   endif
102400131022     C*
102500131022     C     KEYiva01P     reade     ndiva01l
102600131022     C                   enddo
102700131022     C*
102800131022     C* Aggancio il partitario scadenze
102900131022     C                   eval      PASNRRIGAM = 1
103000131023     C     KEYpas01P     chain     ndpas01l
103100131022     C                   if        %found(ndpas01l)
103200131022     C*
103300131022     C* Filtro solamente le caussali 'NFT0'
103400131022     C                   if        PASCAUSRIG = 'NFT0'
103500131022     C*
103600131022     C* Reperisco il numero di giorni pagamento data fattura
103700131022     C                   eval      wPagamGG =  %diff(PASDTSCA:REGDTREG:*DAYS)
103800131022     C*
103900131022     C                   endif
104000131022     C*
104100131022     C                   endif
104200131022     C*
104300131022     C                   endif
104400131022     C                   endif
104500131022     C                   endif
104600131022     C*
104700131022     C                   ENDSR
104800131023
104900131023
105000131023     C*------------------------------------------------------------------------*
105100131023     C* RTVSVN - Routine di reperimento VARIE fatturate
105200131023     C*------------------------------------------------------------------------*
105300131023     C     rtvSvn        BEGSR
105400131023     C*
105500131023     C* Inizializzo i dettagli di fatturazione
105600131023     C                   clear                   skALC
105700131023     C                   clear                   skALC_DES
105800131023     C                   clear                   skMOA_8
105900131023     C                   clear                   skTAX_7
106000170906     C                   clear                   skMOA_124
106100131023     C                   z-add     *zeros        j3
106200131023     C*
106300131023     C* Verifica ed eventuale scrittura del PORTO su TITAS
106400131023     C                   IF        tasPOR <> *zeros
106500131023     C                   EVAL      wSVN = '|'
106600131023     C                   EVAL      wVAN = tasPOR
106700131023     C                   EVAL      wDES = 'PORTO'
106800131023     C                   EXSR      WRISVN
106900131023     C                   ENDIF
107000131023     C*
107100131023     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
107200131023     C                   IF        tasSV1 <> *blanks
107300131023     C                   EVAL      wSVN = tasSV1
107400131023     C                   EVAL      wVAN = tasVA1
107500131023     C                   EVAL      wDES = *blanks
107600131023     C                   EXSR      WRISVN
107700131023     C                   ENDIF
107800131023     C*
107900131023     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
108000131023     C                   IF        tasSV2 <> *blanks
108100131023     C                   EVAL      wSVN = tasSV2
108200131023     C                   EVAL      wVAN = tasVA2
108300131023     C                   EVAL      wDES = *blanks
108400131023     C                   EXSR      WRISVN
108500131023     C                   ENDIF
108600131023     C*
108700131023     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
108800131023     C                   IF        tasSV3 <> *blanks
108900131023     C                   EVAL      wSVN = tasSV3
109000131023     C                   EVAL      wVAN = tasVA3
109100131023     C                   EVAL      wDES = *blanks
109200131023     C                   EXSR      WRISVN
109300131023     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
109400131023     C     KEYta730P     SETLL     tita730c
109500131023     C                   IF        %equal(tita730c)
109600131023     C     KEYta730P     READE     tita730c
109700131023     C                   DOW       not %eof(tita730c)
109800131023     C                   if        ta7VAN > *zeros
109900131023     C                   EVAL      wSVN = ta7SVN
110000131023     C                   EVAL      wVAN = ta7VAN
110100131023     C                   EVAL      wDES = *blanks
110200131023     C                   EXSR      WRISVN
110300131023     C                   endif
110400131023     C     KEYta730P     READE     tita730c
110500131023     C                   ENDDO
110600131023     C                   ENDIF
110700131023     C                   ENDIF
110800131023     C*
110900131023     C                   ENDSR
111000131023
111100131023
111200131023     C*------------------------------------------------------------------------*
111300131023     C* WRISVN - Routine di valorizzazione wrk-file VARIE fatturate
111400131023     C*------------------------------------------------------------------------*
111500131023     C     wriSvn        BEGSR
111600131023     C*
111700131023     C* Elaboro tutto tranne PORTO e la varia 8 (IVA)
111800131023     C                   if        wSVN <> '8' and wSVN <> '|'
111900131023     C                   add       1             j3
112000170906     C                   eval      skALC(j3)     = wSVN
112100170906     C                   eval      skMOA_8(j3)   = wVAN
112200131023     C*
112300131023     C* Reperisco la descrizione della VARIA
112400131023     C                   if        wDES = *blanks
112500131023     C                   z-add     1             j2
112600131023     C     wSVN          lookup    skSVN(j2)                              55
112700131023     C                   if        %found
112800131025     C                   eval      skALC_DES(j3) = skSVNDES(j2)
112900131023     C                   else
113000131023     C                   eval      skALC_DES(j3) = *all'*'
113100131023     C                   endif
113200131023     C                   else
113300131025     C                   eval      skALC_DES(j3) = wDES
113400131025     C                   endif
113500131030     C*
113600131030     C* Se richiesto effettuo mapping tra charge codes BRT e quelli CLIENTE
113700131030     C                   if        w§PPTCVT = 'Y'
113800160224     C                   setoff                                       56
113900131030     C                   z-add     1             j4
114000131030     C     wSVN          lookup    skCHRG_BRT(j4)                         55
114100131030     C                   if        %found
114200160224     C                   eval      skALC_DES(j3) = skCHRG_DES(j4)
114300131030     C                   if        skCHRG_CLI(j4) <> *blanks
114400160224     C                   seton                                        56
114500131030     C                   eval      skALC(j3)     = skCHRG_CLI(j4)
114600131030     C                   endif
114700131030     C                   endif
114800160224     C*
114900160224     C* Se richiesta mappatira ma varia non contemplata "forzo" codice varia cliente "generico"
115000160224     C                   if        not *in56
115100160224     C                   eval      skALC(j3)     = 'SC'
115200160224     C                   endif
115300131030     C                   endif
115400131025     C*
115500131025     C* Verifico eventuali caratteri non consentiti dal EDIFACT
115600131025     C* ALC DES
115700131025     C                   eval      campo_alfa = skALC_DES(j3)
115800131025     C                   exsr      Char_Speciali
115900131030     C                   eval      skALC_DES(j3) = campo_alfa
116000131023     C*
116100170906     C* Reperisco aliquota IVA della VARIA corrente e lego relativo importo IVA
116200131023     C                   z-add     1             j2
116300131023     C     wSVN          lookup    skSVNnoIVA(j2)                         55
116400131023     C                   if        %found
116500170906     C                   eval      skTAX_7(j3)   = *zeros
116600170906     C                   eval      skMOA_124(j3) = *zeros
116700131023     C                   else
116800170906     C                   eval      skTAX_7(j3)   = wCurrIVA
116900170906     C                   eval(h)   skMOA_124(j3) = skMOA_8(j3)*wCurrIVA/100
117000131023     C                   endif
117100131023     C*
117200131023     C                   endif
117300131023     C*
117400131023     C                   clear                   wSVN
117500131023     C                   clear                   wVAN
117600131023     C                   clear                   wDES
117700131023     C*
117800131023     C                   ENDSR
117900031103
118000031103
118100131019     C     wriDettagli   BEGSR
118200131023     C*
118300131004     C* Incremento il contatore di messaggio (al interno della transazione)
118400131019     C                   eval      Conta_msg = Conta_msg + 1
118500131004     C*
118600131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
118700131025     C                   movel     *blanks       wAlfaRMA        512
118800131025     C                   movel     *blanks       wAlfaRSD        512
118900131004     C                   movel     *blanks       wAlfaLOD        512
119000131004     C                   movel     *blanks       wAlfaIND        512
119100131025     C                   movel     *blanks       wAlfaNAS        512
119200131004     C* RMA
119300131019     C                   eval      campo_alfa = vafRMA
119400131004     C                   exsr      Char_Speciali
119500131004     C                   eval      wAlfaRMA = campo_alfa
119600131025     C* RSD
119700131025     C                   eval      campo_alfa = tasRSD
119800131025     C                   exsr      Char_Speciali
119900131025     C                   eval      wAlfaRSD = campo_alfa
120000131004     C* LOD
120100131004     C                   eval      campo_alfa = tasLOD
120200131004     C                   exsr      Char_Speciali
120300131004     C                   eval      wAlfaLOD = campo_alfa
120400131004     C* IND
120500131004     C                   eval      campo_alfa = tasIND
120600131004     C                   exsr      Char_Speciali
120700131004     C                   eval      wAlfaIND = campo_alfa
120800131025     C* NAS
120900160208     C                   if        §TA4ANAS <> *blanks
121000160208     C                   eval      campo_alfa = §TA4ANAS
121100131025     C                   exsr      Char_Speciali
121200131025     C                   eval      wAlfaNAS = campo_alfa
121300131025     C                   else
121400131025     C                   eval      wAlfaNAS = 'VOID'
121500131025     C                   endif
121600131004     C*
121700131004     C*
121800131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
121900131019     C*
122000131019     C*  LIN -
122100131019     C                   eval      Conta_segm  = Conta_segm  + 1
122200131019     C                   eval      vawDTA = LIN_sgm
122300131022     C                              + %trim(%editc(Conta_item:'3'))
122400131019     C                              +   apice
122500131019     C                   exsr      wriOUTPUT
122600131019     C*
122700131019     C*  IMD -
122800131019     C                   eval      Conta_segm  = Conta_segm  + 1
122900131019     C                   eval      vawDTA = IMD_sgm
123000131019     C                              +   apice
123100131019     C                   exsr      wriOUTPUT
123200170411     C*
123300170411     C*  MEA WT -
123400170411     C                   eval      Conta_segm  = Conta_segm  + 1
123500170411     C                   if        vafPKC > vafPKB
123600170411     C                   z-add     vafPKC        wPeso             7 1
123700170411     C                   else
123800170411     C                   z-add     vafPKB        wPeso
123900170411     C                   endif
124000170411     C                   eval      vawDTA = MEA_WT_sgm
124100170411     C                              + %trim(%editc(wPeso:'3'))
124200170411     C                              +   apice
124300170411     C                   exsr      wriOUTPUT
124400131019     C*
124500131019     C*  MOA 203 -
124600131019     C                   eval      Conta_segm  = Conta_segm  + 1
124700171019     C                   eval(h)   wVAL3D = vafIMV
124800170915     C                   eval      vawDTA = MOA_203_sgm
124900171019     C                              + %trim(%editc(wVAL3D:'3'))
125000131219     C                              +   apice
125100131019     C                   exsr      wriOUTPUT
125200131219     C                   eval      wTotIMV  = wTotIMV + vafIMV                  * Tot MOA+203 per 79
125300131019     C*
125400131019     C*  PRI -
125500170904     C*
125600170904     C* Ritorno il valore reale del PORTO solo se spedizione NON di "reso mittente"
125700170904     C                   if        tasFBR = *blanks
125800170904     C                   z-add     vafPOR        wPOR             11 3
125900170904     C*
126000170904     C* Se trattasi di spedizione di "reso mittente" invece ritorno il porto a zero
126100170904     C                   else
126200170904     C                   z-add     *zeros        wPOR
126300170904     C                   endif
126400170904     C*
126500170904     C                   eval      Conta_segm  = Conta_segm  + 1
126600171019     C                   eval(h)   wVAL3D = wPOR
126700170904     C                   eval      vawDTA = PRI_sgm
126800171019     C                              + %trim(%editc(wVAL3D:'3'))
126900170904     C                              +   apice
127000170904     C                   exsr      wriOUTPUT
127100131019     C*
127200131019     C*  TAX -
127300131019     C                   eval      Conta_segm  = Conta_segm  + 1
127400131019     C                   eval      vawDTA = TAX_sgm
127500131022     C                              + %trim(%editc(wCurrIVA:'3'))
127600131019     C                              +   apice
127700131019     C                   exsr      wriOUTPUT
127800170904     C*
127900171113     C*** Amazon NON vuole questo segmento
128000171113     C*** per cui si disattiva la modifica e poi se ne riparla
128100170904     C*  MOA 124 -
128200171113     C******             eval(h)   wIVA = vafIMV*wCurrIVA/100
128300171113     C***                eval(h)   wIVA = wPOR*wCurrIVA/100
128400171113     C***                eval      Conta_segm  = Conta_segm  + 1
128500171113     C***                eval      vawDTA = MOA_124_sgm
128600171113     C***                           + %trim(%editc(wIVA:'3'))
128700171113     C***                           +   apice
128800171113     C***                exsr      wriOUTPUT
128900131018     C*
129000131019     C*  NAD CN -
129100131004     C                   eval      Conta_segm  = Conta_segm  + 1
129200131018     C                   eval      vawDTA = NAD_CN
129300131025     C                              + %trim(wAlfaRSD)
129400131004     C                              +   piu
129500131004     C                              +   piu
129600131004     C                              + %trim(wAlfaIND)
129700131004     C                              +   piu
129800131004     C                              + %trim(wAlfaLOD)
129900131004     C                              +   piu
130000131004     C                              + %trim(tasPRD)
130100131004     C                              +   piu
130200131004     C                              + %trim(tasCAD)
130300131004     C                              +   piu
130400131004     C                              + 'IT'
130500131004     C                              +   apice
130600131019     C                   exsr      wriOUTPUT
130700131019     C*
130800131022     C*  RFF AAM -
130900131019     C                   eval      Conta_segm  = Conta_segm  + 1
131000131019     C                   eval      vawDTA = RFF_AAM_sgm
131100131025     C                              + %trim(wAlfaNAS)
131200131019     C                              +   apice
131300131019     C                   exsr      wriOUTPUT
131400131019     C*
131500131019     C*  RFF CR -
131600131019     C                   eval      Conta_segm  = Conta_segm  + 1
131700131019     C                   eval      vawDTA = RFF_CR_sgm
131800131022     C                              + %trim(%editc(vafRMN:'3'))
131900131019     C                              +   apice
132000131019     C                   exsr      wriOUTPUT
132100131019     C*
132200131019     C*  RFF ACC -
132300131019     C                   eval      Conta_segm  = Conta_segm  + 1
132400131019     C                   eval      vawDTA = RFF_ACC_sgm
132500131025     C                              + %trim(wAlfaRMA)
132600131019     C                              +   apice
132700131019     C                   exsr      wriOUTPUT
132800131019     C*
132900131019     C*  DTM 200 -
133000131019     C                   eval      Conta_segm  = Conta_segm  + 1
133100131019     C                   eval      vawDTA = DTM_200_1
133200131019     C                              + %trim(%editc(tasDRT:'X'))
133300131019     C                              + DTM_200_2
133400131019     C                              +   apice
133500131019     C                   exsr      wriOUTPUT
133600131023     C*
133700131023     C* Sviluppo tutti i dettagli di fatturazione
133800160202     C* Ritorno le VARIE solo se spedizione NON di "reso mittente"
133900160201     C                   if        tasFBR = *blanks
134000131023     C                   eval      j3 = 1
134100131023     C                   dow       j3 <= %elem(skALC) and skALC(j3) <> *blanks
134200131023     C*
134300131023     C* Restituisco solamente le varie con importo
134400131023     C                   if        skMOA_8(j3) > *zeros
134500131023     C*
134600131023     C*  ALC -
134700131023     C                   eval      Conta_segm  = Conta_segm  + 1
134800131023     C                   eval      vawDTA = ALC_sgm
134900131023     C                              + %trim(skALC(j3))
135000131023     C                              +   diviso
135100131023     C                              +   diviso
135200131023     C                              +   diviso
135300131030     C                              + %trim(skALC_DES(j3))
135400131023     C                              +   apice
135500131023     C                   exsr      wriOUTPUT
135600131023     C*
135700131023     C*  MOA 8 -
135800131023     C                   eval      Conta_segm  = Conta_segm  + 1
135900171019     C                   eval(h)   wVAL3D = skMOA_8(j3)
136000131023     C                   eval      vawDTA = MOA_8_sgm
136100171019     C                              + %trim(%editc(wVAL3D:'3'))
136200131023     C                              +   apice
136300131023     C                   exsr      wriOUTPUT
136400131023     C*
136500131023     C*  TAX 7 -
136600131023     C                   eval      Conta_segm  = Conta_segm  + 1
136700131023     C                   eval      vawDTA = TAX_sgm
136800131023     C                              + %trim(%editc(skTAX_7(j3):'3'))
136900131023     C                              +   apice
137000131023     C                   exsr      wriOUTPUT
137100170906     C*
137200171113     C*** Amazon NON vuole questo segmento
137300171113     C*** per cui si disattiva la modifica e poi se ne riparla
137400170906     C*  MOA 124 -
137500171113     C***                eval      Conta_segm  = Conta_segm  + 1
137600171113     C***                eval      vawDTA = MOA_124_sgm
137700171113     C***                           + %trim(%editc(skMOA_124(j3):'3'))
137800171113     C***                           +   apice
137900171113     C***                exsr      wriOUTPUT
138000131023     C*
138100131023     C                   endif
138200131023     C                   add       1             j3
138300131023     C                   enddo
138400160202     C*
138500160202     C* Se trattasi di spedizione di "reso mittente"
138600160202     C                   else
138700160202     C*
138800160202     C*  ALC -
138900160202     C                   eval      Conta_segm  = Conta_segm  + 1
139000160202     C                   eval      vawDTA = ALC_sgm
139100160202     C                              + 'UD'
139200160202     C                              +   diviso
139300160202     C                              +   diviso
139400160202     C                              +   diviso
139500160202     C                              + 'RETURN TO SENDER'
139600160202     C                              +   apice
139700160202     C                   exsr      wriOUTPUT
139800160202     C*
139900160202     C*  MOA 8 -
140000160202     C                   eval      Conta_segm  = Conta_segm  + 1
140100171019     C                   eval(h)   wVAL3D = vafIMV
140200160202     C                   eval      vawDTA = MOA_8_sgm
140300171019     C                              + %trim(%editc(wVAL3D:'3'))
140400160202     C                              +   apice
140500160202     C                   exsr      wriOUTPUT
140600160202     C*
140700160202     C*  TAX 7 -
140800160202     C                   eval      Conta_segm  = Conta_segm  + 1
140900160202     C                   eval      vawDTA = TAX_sgm
141000160202     C                              + %trim(%editc(wCurrIVA:'3'))
141100160202     C                              +   apice
141200160202     C                   exsr      wriOUTPUT
141300170906     C*
141400171113     C*** Amazon NON vuole questo segmento
141500171113     C*** per cui si disattiva la modifica e poi se ne riparla
141600170906     C*  MOA 124 -
141700171113     C***                eval      Conta_segm  = Conta_segm  + 1
141800171113     C***                eval(h)   wIVA = vafIMV*wCurrIVA/100
141900171113     C***                eval      vawDTA = MOA_124_sgm
142000171113     C***                           + %trim(%editc(wIVA:'3'))
142100171113     C***                           +   apice
142200171113     C***                exsr      wriOUTPUT
142300160202     C*
142400160201     C                   endif
142500031103     C*
142600031103     C                   ENDSR
142700160208
142800160208
142900160208
143000160208     C     RTVNAS        BEGSR
143100160208     C*
143200160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
143300160208     C                   CLEAR                   DTA4A
143400160208     C*
143500160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
143600160208     C                   CALL      'UBTA400R'
143700160208     C                   PARM      *blanks       UBTA4IOPZ
143800160208     C                   PARM      *blanks       UBTA4ITLA
143900160208     C                   PARM      tasAAS        UBTA4IAAS
144000160208     C                   PARM      tasLNP        UBTA4ILNP
144100160208     C                   PARM      tasNRS        UBTA4INRS
144200160208     C                   PARM      tasNSP        UBTA4INSP
144300160208     C                   PARM      'A'           UBTA4ITRC
144400160208     C                   PARM                    UBTA4OERR
144500160208     C                   PARM                    UBTA4ODS
144600160208     C                   PARM                    UBTA4OLEN
144700160208     C                   PARM                    UBTA4ODATI
144800160208     C*
144900160208     C                   IF        UBTA4OERR = *zeros
145000160208     C                   SELECT
145100160208     C* Gestione output tipo record 'A'
145200160208     C                   WHEN      UBTA4ODS = 'DTA4A'
145300160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
145400160208     C                   ENDSL
145500160208     C*
145600160208     C                   ENDIF
145700160208     C*
145800160208     C                   ENDSR
145900031103
146000160208
146100991027
146200991027      /TITLE Operazioni iniziali.
146300991027     C     *inzsr        BEGSR
146400991027     C*
146500991027     C     *ENTRY        PLIST
146600131019     C                   parm                    prmppt
146700991027     C     wrkesito      parm      wrkesito      prmesito
146800031103     C*
146900031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
147000131019     C                   EVAL      DSPPT = prmppt
147100131004     C*
147200131004     C* Reperisco i valori variabili ma a livello di transazione corrente
147300131004     C* (tipicamente data e ora relativ al file generato)
147400131004     C                   eval      currDate6    =
147500131004     C                              UBFMTDATE_Convert(
147600131004     C                                  %editc(%dec(%date():*ISO):'X') :
147700131004     C                                  'YYYYMMDD' : 'YYMMDD')
147800131004     C                   eval      currDate6inv =
147900131004     C                              UBFMTDATE_Convert(
148000131004     C                                  %editc(%dec(%date():*ISO):'X') :
148100131004     C                                  'YYYYMMDD' : 'DDMMYY')
148200131018     C                   eval      currDate8 =
148300131018     C                              UBFMTDATE_Convert(
148400131018     C                                  %editc(%dec(%date():*ISO):'X') :
148500131023     C                                  'YYYYMMDD' : 'YYYYMMDD')
148600131004     C                   eval      currHHMM =
148700131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
148800030709     C*
148900030709     C* Definizione chiavi
149000131023     C*
149100131023     C* LETTURA tabel00f - PARZIALE
149200131023     C     KEYtab00_P1   KLIST
149300131023     C                   KFLD                    tblKUT
149400131023     C                   KFLD                    tblCOD
149500031103     C*
149600031103     C* Chiave su TITAS30C - Parziale
149700031103     C     KEYtas30P     KLIST
149800131019     C                   KFLD                    vafAAS
149900131019     C                   KFLD                    vafLNP
150000131019     C                   KFLD                    vafNRS
150100131019     C                   KFLD                    vafNSP
150200131023     C*
150300131023     C* LETTURA tita730c - PARZIALE
150400131023     C     KEYta730P     KLIST
150500131023     C                   KFLD                    tasAAS
150600131023     C                   KFLD                    tasLNP
150700131023     C                   KFLD                    tasNRS
150800131023     C                   KFLD                    tasNSP
150900131023     C                   KFLD                    tasTBL
151000131022     C*
151100131022     C* Chiave su NDREG04L - Completa
151200131022     C     KEYreg04C     KLIST
151300131022     C                   KFLD                    REGSOCIETA
151400131022     C                   KFLD                    REGCTB
151500131022     C                   KFLD                    REGDTREG
151600131022     C                   KFLD                    REGNRREG
151700131022     C                   KFLD                    REGSERREG
151800131022     C*
151900131022     C* Chiave su NDIVA01L - Parziale
152000131022     C     KEYiva01P     KLIST
152100131023     C                   KFLD                    REGSYS
152200131023     C                   KFLD                    REGNRASREG
152300131022     C*
152400131022     C* Chiave su NDPAS01L - Parziale
152500131022     C     KEYpas01P     KLIST
152600131023     C                   KFLD                    REGSYS
152700131023     C                   KFLD                    REGNRASREG
152800131022     C                   KFLD                    PASNRRIGAM
152900991027     C*
153000991027     C                   ENDSR
153100131030** CHRGCODES - CHARGE CODES BRT / CLIENTE
153200160127+         SUPPL.CARBURANTE                                                                  01
153300160127&    SC   ANN.P/ASSEGNATO                                                                   02
153400160127*    SC   DIROTTAM.TO                                                                       03
153500160127=    SC   RESO BANCALI                                                                      04
153600160127a    DCC  POD IMAGE                                                                         05
153700160127b    SC   AD.GEST.C.                                                                        06
153800160127c    SC   L.AVVISO                                                                          07
153900160127d    INS  A.C. BASE                                                                         08
154000160127e    SC   PRIORITY                                                                          09
154100160127f    FSC  FUEL SURCH                                                                        10
154200160127h    SC   H 10:30                                                                           11
154300160127k         PACKING L.                                                                        11
154400160127m    SC   AV.AFFID.                                                                         12
154500160127p         PINcode                                                                           11
154600160127t         PRENOT.RITIRO                                                                     11
154700160127z         EXPO                                                                              11
154800160127A    EDC  APPUNTAMENTO                                                                      13
154900160127B    DCC  CONS.DDT                                                                          14
155000160127C    SC   FACCH.ARR.                                                                        15
155100160127D    SC   INV/INC FT                                                                        16
155200160127E         R.A.RESP.VETT.                                                                    17
155300160127F    OSC  FUORI MISURA AZ                                                                   18
155400160127G    COD  COMP.ASSEGNO                                                                      19
155500160127H    SC   DIR.FISSO                                                                         20
155600160127I    HLD  SP.GIACENZA                                                                       21
155700160127J    RAS  ISOLA                                                                             22
155800160127K    RAS  LOC.DISAG.                                                                        23
155900160127L    SC   I.S.T.A.T.                                                                        24
156000160127M    SC   SPESE INSOLUTO                                                                    25
156100160127N    SC   ANTEPORTO                                                                         26
156200160127O    SC   RIC.DOCUM.                                                                        27
156300160127P    AHC  AI PIANI                                                                          28
156400180305Q    RAS  ZTL                                                                               29
156500160127R    INS  A.C. PLUS                                                                         30
156600160127S    SC   SUPER./GDO                                                                        31
156700160127T    SC   RIT.COMMISSIONATO                                                                 32
156800160127U    SC   RITIRO                                                                            33
156900160127W    COD  REC.C/ASSEGNI                                                                     34
157000180305X    RAS  CONS.DISAG.                                                                       35
157100160127Y    SC   RIT.ANNULL.                                                                       36
157200160127Z    FSC  AD.GEST.AP                                                                        37
1573001601272    SC   INOLTRO                                                                           38
1574001601273         A.R.                                                                              39
1575001601274         FRAGILE                                                                           40
1576001601275    SC   DIR.PESAT.                                                                        41
1577001601276    SDU  BOLLO                                                                             42
1578001601277         T.C.P.                                                                            43
1579001601278    65   I.V.A.                                                                            44
