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
004400131004
004500131004
004600131004     D*-----------
004700131004     D* DEFINIZIONE DS ESTERNE
004800131004     D*-----------
004900131004     D trul33ds      e ds                  inz
005000131004     D kpjba         e ds                  inz
005100131019     D tivafds       e ds                  inz extname(tivaf00t)
005200131023     D dscc          e ds                  inz
005300160208     D DTA4A         e ds
005400131022
005500131022
005600131022     D*------------
005700131022     D* SCHIERE DETTAGLI REGISTRAZIONE CONTABILE
005800131022     D*------------
005900131023     D skIVAIMP        S             19  3 dim(100)
006000131023     D skIVAALQ        S              5  2 dim(100)
006100131022     D skIMPONB        S             19  3 dim(100)
006200131023     D j1              S              3  0 inz
006300131030     D skSVN           S              1    DIM(100)
006400131030     D skSVNDES        S             40    DIM(100)
006500131030     D skSVNnoIVA      S              1    DIM(100)
006600131023     D j2              S              3  0 inz
006700131030     D skALC           S              5    DIM(100)
006800131030     D skALC_DES       S             40    DIM(100)
006900131023     D skMOA_8         S             19  3 DIM(100)
007000131023     D skTAX_7         S              5  2 DIM(100)
007100170906     D skMOA_124       S             19  4 DIM(100)
007200131023     D j3              S              3  0 inz
007300131030     D skCHRG_BRT      S              1    DIM(100)
007400131030     D skCHRG_CLI      S              5    DIM(100)
007500131030     D skCHRG_DES      S             40    DIM(100)
007600131030     D j4              S              3  0 inz
007700131022
007800131004
007900131004     D*------------------
008000131004     D* LINKING A DEFINIZIONI ESTERNE
008100131004     D*------------------
008200131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
008300131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
008400160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
008500131021     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
008600131021      /DEFINE DFTACTGRP_YES
008700131021     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
008800131021      /UNDEFINE DFTACTGRP_YES
008900131021
009000131021     ***************************************************************************
009100131021     **
009200131021     ** Definizione strutture dati.
009300131021     **
009400131021      **************************************************************************
009500131021     D tibsSocI0...
009600131021     D               E DS                  QUALIFIED
009700131021     D                                     INZ
009800131021     D tibsSocO0...
009900131021     D               E DS                  QUALIFIED
010000131021     D                                     INZ
010100131021     ***************************************************************************
010200131021     **
010300131021     ** Definizione variabili modulo/programma.
010400131021     **
010500131021     ***************************************************************************
010600131021     D prmRqsOpCode...
010700131021     D                 S             10A
010800131021     D prmRpyOpCode...
010900131021     D                 S             10A
011000131021     D prmRpyIdMsg...
011100131021     D                 S             10I 0
011200131021     D prmRqsFormato...
011300131021     D                 S             10A
011400131021     D prmRqsDataSize...
011500131021     D                 S             10I 0
011600131021     D prmRpyFormato...
011700131021     D                 S             10A
011800131021     D prmRpyDataSize...
011900131021     D                 S             10I 0
012000110705
012100031103
012200031103     D*-----------
012300031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
012400031103     D*-----------
012500131019     D DSPPT           DS
012600131030     D  w§PPTKSU                      7  0
012700131030     D  w§PPTCVT                      1
012800131018     D  w§UNB_ID_1                    5
012900131018     D  w§UNB_ID_2                   25
013000131004
013100131030
013200131030     D*-----------
013300131030     D* SCHIERE A PROGRAMMA
013400131030     D*-----------
013500160128     D CHRGCODES       S             60    DIM(48) CTDATA PERRCD(1)
013600131030
013700131030
013800131030     D*-----------
013900131030     D* RIDEFINIZIONE SCHIERA A PROGRAMMA CHRGCODES
014000131030     D*-----------
014100131030     D DS_CHRGCODE     DS
014200131030     D  CHRGCODE_BRT                  5
014300131030     D  CHRGCODE_CLI                  5
014400131030     D  CHRGCODE_DES                 40
014500131004
014600131030
014700131004     D*-----------
014800131018     D* COSTANTI / DEFAULT
014900131004     D*-----------
015000131018     D UNB_sgm         C                   CONST('UNB+UNOA:1+')
015100131007     D UNH_sgm_1       C                   CONST('UNH+')
015200131018     D UNH_sgm_2       C                   CONST('+INVOIC:D:96A:UN:EAN008')
015300131018     D BGM_sgm_1       C                   CONST('BGM+380+')
015400131018     D BGM_sgm_2       C                   CONST('+9')
015500131018     D DTM_3_1         C                   CONST('DTM+3:')
015600131018     D DTM_3_2         C                   CONST(':102')
015700131018     D DTM_137_1       C                   CONST('DTM+137:')
015800131018     D DTM_137_2       C                   CONST(':102')
015900131018     D RFF_IT_sgm      C                   CONST('RFF+IT:')
016000131021     D NAD_CA_1        C                   CONST('NAD+CA+')
016100131021     D NAD_CA_2        C                   CONST('BRT S.P.A.+PIAZZA DIAZ, 7+')
016200131021     D NAD_CA_3        C                   CONST('MILANO++20123+IT')
016300150309     D RFF_VA_1_sgm    C                   CONST('RFF+VA:IT04507990150')
016400131018     D NAD_SF_1        C                   CONST('NAD+SF+5450534001540::9++')
016500131128     D NAD_SF_2        C                   CONST('AMAZON EU SARL:')
016600131128     D NAD_SF_3        C                   CONST('CO AMAZON LOGISTICA SRL+')
016700131128     D NAD_SF_4        C                   CONST('STRADA DOGANA PO 2 T')
016800131128     D NAD_SF_5        C                   CONST('CASTEL SAN GIOVANNI++')
016900131128     D NAD_SF_6        C                   CONST('29015+IT')
017000140505     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000345::9++')
017100140505     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
017200140505     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
017300140505     D*NAD_IV_4        C                   CONST('AMAZON IT LOGISTICA SRL:')
017400140505     D*NAD_IV_5        C                   CONST('VIA FERR. APORTI 8 - ')
017500140505     D*NAD_IV_6        C                   CONST('IT07231660965+')
017600140505     D*NAD_IV_7        C                   CONST('MILANO++20125+IT')
017700140508     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000345::9++')
017800140508     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
017900140508     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
018000140508     D*NAD_IV_4        C                   CONST('PI IT05257010966:')
018100140508     D*NAD_IV_5        C                   CONST('RAPPR FISCALE AMAZON ITALIA:')
018200140508     D*NAD_IV_6        C                   CONST('LOGISTICA SRL VIA FERRANTE')
018300140508     D*NAD_IV_7        C                   CONST('APORTI 8+MILANO++20125+IT')
018400140516     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000055::9++')
018500140516     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:')
018600140516     D*NAD_IV_3        C                   CONST('5 RUE PLAETIS LUXEMBOURG+')
018700140516     D*NAD_IV_4        C                   CONST('CO ACCOUNTING SERVICE CENTER')
018800140516     D*NAD_IV_5        C                   CONST(':1-9 THE GROVE+SLOUGH++')
018900140516     D*NAD_IV_6        C                   CONST('SL1 1QP+GB')
019000140516     D*NAD_IV_7        C                   CONST('')
019100150309     D*NAD_IV_1        C                   CONST('NAD+IV+5450534000055::9++')
019200150309     D*NAD_IV_2        C                   CONST('AMAZON EU SARL:5 RUE PLAETIS')
019300150309     D*NAD_IV_3        C                   CONST(' L-2338 LUXEMBOURG+C/O AP')
019400150309     D*NAD_IV_4        C                   CONST(' NON-INVENTORY FINANCIAL')
019500150309     D*NAD_IV_5        C                   CONST(' OPS:1-9 THE GROVE+')
019600150309     D*NAD_IV_6        C                   CONST('SLOUGH++SL1 1QP+GB')
019700150309     D*NAD_IV_7        C                   CONST('')
019800150309     D NAD_IV_1        C                   CONST('NAD+IV+5450534005845::9++')
019900150309     D NAD_IV_2        C                   CONST('AMAZON EU SARL, ')
020000150309     D NAD_IV_3        C                   CONST('SUCCURSALE ITALIANA+')
020100150309     D NAD_IV_4        C                   CONST('VIA FERRANTE APORTI 8+')
020200150309     D NAD_IV_5        C                   CONST('MILANO++20125+IT')
020300150309     D NAD_IV_6        C                   CONST('')
020400140516     D NAD_IV_7        C                   CONST('')
020500150309     D*RFF_VA_2_sgm    C                   CONST('RFF+VA:LU20260743')
020600150309     D RFF_VA_2_sgm    C                   CONST('RFF+VA:IT08973230967')
020700131018     D CUX_sgm         C                   CONST('CUX+2:EUR:4')
020800131022     D PAT_sgm         C                   CONST('PAT+1++5::D:')
020900131019     D PCD_sgm         C                   CONST('PCD+7:0:13')
021000131019     D LIN_sgm         C                   CONST('LIN+')
021100131023     D IMD_sgm         C                   CONST('IMD+F+8+:::BRT_IT')
021200140407     D IMD_sgm_Bollo   C                   CONST('IMD+F+8+:::STAMP DUTY')
021300170411     D MEA_WT_sgm      C                   CONST('MEA+WT+G+KG:')
021400131019     D MOA_203_sgm     C                   CONST('MOA+203:')
021500131019     D PRI_sgm         C                   CONST('PRI+CAL:')
021600131022     D TAX_sgm         C                   CONST('TAX+7+VAT+++:::')
021700131019     D NAD_CN          C                   CONST('NAD+CN++')
021800131019     D RFF_AAM_sgm     C                   CONST('RFF+AAM:')
021900131019     D RFF_CR_sgm      C                   CONST('RFF+CR:')
022000131019     D RFF_ACC_sgm     C                   CONST('RFF+ACC:')
022100131019     D DTM_200_1       C                   CONST('DTM+200:')
022200131019     D DTM_200_2       C                   CONST(':102')
022300131023     D ALC_sgm         C                   CONST('ALC+C++++')
022400140407     D ALC_sgm_Bollo   C                   CONST('ALC+C++++SDU:::STAMP DUTY')
022500131023     D MOA_8_sgm       C                   CONST('MOA+8:')
022600131023     D UNS_sgm         C                   CONST('UNS+S')
022700131019     D CNT_2_sgm       C                   CONST('CNT+2:')
022800131019     D MOA_79_sgm      C                   CONST('MOA+79:')
022900131019     D MOA_58_sgm      C                   CONST('MOA+58:')
023000131019     D MOA_77_sgm      C                   CONST('MOA+77:')
023100131205     D MOA_124_sgm     C                   CONST('MOA+124:')
023200131205     D MOA_125_sgm     C                   CONST('MOA+125:')
023300131004     D UNT_sgm         C                   CONST('UNT+')
023400131004     D UNZ_sgm         C                   CONST('UNZ+')
023500131004     D piu             C                   CONST('+')
023600131004     D diviso          C                   CONST(':')
023700131004     D apice           C                   CONST('''')
023800131022     D societa         C                   CONST('201')
023900131021     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
024000131021     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
024100131004
024200030924
024300030325
024400030924     C* MAIN/
024500170123     C*
024600170123     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
024700170123     C
024800170123     C/EXEC SQL
024900170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
025000170123     C/END-EXEC
025100131021     C*
025200131021     C* Inizializzo il TIBSSOCR
025300131025     C                   SETOFF                                       2021
025400131021     C                   SETON                                        20
025500131021     C                   EXSR      EXESOCR
025600041015     C*
025700041015     C* Carica i dati tabellati
025800041015     C                   EXSR      cartab
025900041015     C*
026000041015     C* Esegue traduzione
026100131004     C                   setoff                                       31
026200131004     C  N31              EXSR      chkRECFILMBR
026300131004     C  N31              EXSR      rtvID
026400131004     C  N31              EXSR      wriHeader
026500131004     C  N31              EXSR      traduci
026600131004     C  N31              EXSR      wriFooter
026700131004 xxx C***31              EXSR      sndAlert
026800131021     C*
026900131021     C* Chiudo il TIBSSOCR
027000131025     C                   SETOFF                                       2021
027100131021     C                   SETON                                        21
027200131021     C                   EXSR      EXESOCR
027300160208     C*
027400160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
027500160208     C                   CALL      'UBTA400R'
027600160208     C                   PARM                    UBTA4IOPZ
027700160208     C                   PARM      'C'           UBTA4ITLA
027800160208     C                   PARM                    UBTA4IAAS
027900160208     C                   PARM                    UBTA4ILNP
028000160208     C                   PARM                    UBTA4INRS
028100160208     C                   PARM                    UBTA4INSP
028200160208     C                   PARM                    UBTA4ITRC
028300160208     C                   PARM                    UBTA4OERR
028400160208     C                   PARM                    UBTA4ODS
028500160208     C                   PARM                    UBTA4OLEN
028600160208     C                   PARM                    UBTA4ODATI
028700110705     C*
028800921023     C                   SETON                                        LR
028900131004
029000131004
029100131004
029200131004     C*----------------------------------------------------*
029300131004     C*  STACCO ID UNIVOCO
029400131004     C*----------------------------------------------------*
029500131004     C     RTVID         BEGSR
029600131004     C*
029700131004     C                   z-add     *zeros        currUNIQ_ID      15 0
029800131004     C*
029900131004     C* NSP => Stacco un numeratore da AZNUM
030000131004     C                   clear                   TRUL33DS
030100131004     C                   eval      I33OPE = *zeros
030200131004     C                   eval      I33CNU = 302
030300131004     C                   eval      I33NUM = 1
030400131004     C                   movel     TRUL33DS      KPJBU
030500131004     C                   call      'TRUL33R'
030600131004     C                   parm                    KPJBA
030700131004     C                   movel     KPJBU         TRUL33DS
030800131004     C                   if        O33ERR = *zeros
030900131004     C                   z-add     O33NRF        currUNIQ_ID
031000131004     C                   else
031100131004     C                   seton                                        31
031200131004     C                   endif
031300131004     C*
031400131004     C                   ENDSR
031500131021
031600131021
031700131021     C*----------------------------------------------------*
031800131021     C*  INIZIALIZZO / CHIUDO IL TIBSSOCR
031900131021     C*----------------------------------------------------*
032000131021     C     EXESOCR       BEGSR
032100131021     C*
032200131021     C* Inizializzo il programma.
032300131021     C                   IF        *IN20
032400131021     C                   CALL      'TIBSSOCR'
032500131021     C                   PARM      'INIT'        prmRqsOpCode
032600131021     C                   PARM                    prmRpyOpCode
032700131021     C                   PARM                    prmRpyIdMsg
032800131021     C                   ENDIF
032900131021     C*
033000131021     C* Chiudo il programma.
033100131021     C                   IF        *IN21
033200131021     C                   CALL      'TIBSSOCR'
033300131021     C                   PARM      'FINALIZE'    prmRqsOpCode
033400131021     C                   PARM                    prmRpyOpCode
033500131021     C                   PARM                    prmRpyIdMsg
033600131021     C                   ENDIF
033700131021     C*
033800131021     C                   ENDSR
033900131004
034000131021
034100131004     C     chkRECFILMBR  BEGSR
034200131004     C*
034300131004     C     *start        setll     TIVAWWWT
034400131004     C                   if        %found(TIVAWWWT)
034500131004     C                   seton                                        31
034600131004     C                   endif
034700131004     C*
034800131004     C                   ENDSR
034900131021
035000131021
035100131021     C     Decod_societa BEGSR
035200131021     C*
035300131021     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
035400131021     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
035500131021     C                   RESET                   tibsSocI0
035600131022     C                   EVAL      tibsSocI0.idSocieta  = societa
035700131021     C                   EVAL      tibsSocI0.dtValidita = %date(vafDFT)
035800131021     C
035900131021     C                   CALL      'TIBSSOCR'
036000131021     C                   PARM      'GETANAGRAF'  prmRqsOpCode
036100131021     C                   PARM      *BLANK        prmRpyOpCode
036200131021     C                   PARM      *ZERO         prmRpyIdMsg
036300131021     C                   PARM      'TIBSSOCI0'   prmRqsFormato
036400131021     C                   PARM                    tibsSocI0
036500131021     C                   PARM                    prmRqsDataSize
036600131021     C                   PARM      'TIBSSOCO0'   prmRpyFormato
036700131021     C                   PARM                    tibsSocO0
036800131021     C                   PARM                    prmRpyDataSize
036900131021     C                   if         PRMRPYIDMSG >= 0
037000131021     C                              and TIBSSOCO0.IDSOCIETA <> *blank
037100131021     C     minu:maiu     xlate     TIBSSOCO0     TIBSSOCO0
037200131021     C                   if        TIBSSOCO0.SEDLEGNAZ = *blanks
037300131021     C                   eval      TIBSSOCO0.SEDLEGNAZ = 'IT'
037400131025     C                   endif
037500131025     C*
037600131025     C* Verifico eventuali caratteri non consentiti dal EDIFACT
037700131025     C                   movel     *blanks       wAlfaSOCRAG     512
037800131025     C                   movel     *blanks       wAlfaSOCIND     512
037900131025     C                   movel     *blanks       wAlfaSOCLOC     512
038000131025     C* SOC RAG
038100131025     C                   eval      campo_alfa = TIBSSOCO0.RAGSOCIALE
038200131025     C                   exsr      Char_Speciali
038300131025     C                   eval      wAlfaSOCRAG = campo_alfa
038400131025     C* SOC IND
038500131025     C                   eval      campo_alfa = TIBSSOCO0.SEDLEGIND
038600131025     C                   exsr      Char_Speciali
038700131025     C                   eval      wAlfaSOCIND = campo_alfa
038800131025     C* SOC LOC
038900131025     C                   eval      campo_alfa = TIBSSOCO0.SEDLEGLOC
039000131025     C                   exsr      Char_Speciali
039100131025     C                   eval      wAlfaSOCLOC = campo_alfa
039200131025     C*
039300131025     C                   eval      wNAD_CA_2     = %trim(wAlfaSOCRAG)          +
039400131021     C                                             piu                         +
039500131025     C                                             %trim(wAlfaSOCIND)          +
039600131021     C                                             piu
039700131025     C                   eval      wNAD_CA_3     = %trim(wAlfaSOCLOC)          +
039800131021     C                                             piu                         +
039900131021     C                                             piu                         +
040000131021     C                                             %trim(TIBSSOCO0.SEDLEGCAP)  +
040100131021     C                                             piu                         +
040200131021     C                                             %trim(TIBSSOCO0.SEDLEGNAZ)
040300131021     C                   eval      wRFF_VA_1_sgm = 'RFF+VA:'                   +
040400131021     C                                             %trim(TIBSSOCO0.PARTITAIVA)
040500131021     C                   else
040600131021     C                   eval      wNAD_CA_2     = NAD_CA_2
040700131021     C                   eval      wNAD_CA_3     = NAD_CA_3
040800131021     C                   eval      wRFF_VA_1_sgm = RFF_VA_1_sgm
040900131021     C                   endif
041000131021     C*
041100131021     C                   ENDSR
041200131004
041300131004
041400131004     C     wriOUTPUT     BEGSR
041500131004     C*
041600131004     C                   write     TIVAW000
041700131004     C*
041800131004     C                   ENDSR
041900130807
042000041015
042100131023     C     cartab        BEGSR
042200041015     C*
042300131023     C* Reperisco le descrizioni delle varie
042400131023     C                   z-add     *zeros        j2
042500131023     C                   z-add     1             tblKUT
042600131023     C                   movel(P)  'CC'          tblCOD
042700131023     C     KEYtab00_P1   setll     TABEL00F
042800131023     C                   if        %equal(TABEL00F)
042900131023     C     KEYtab00_P1   reade     TABEL00F
043000131023     C                   dow       not %eof(TABEL00F)
043100131030     C                   if        %subst(tblKEY:1:5) = 'VARIE'
043200131023     C                   add       1             j2
043300131030     C                   eval      skSVN(j2) = %subst(tblKEY:8:1)
043400131030     C                   movel(P)  tblUNI        DSCC
043500131023     C                   movel(P)  §CCDES        skSVNDES(j2)
043600131023     C                   if        §CCJEI <> *blanks
043700131023     C                   eval      skSVNnoIVA(j2) = 'Y'
043800131023     C                   endif
043900131030     C                   endif
044000131023     C     KEYtab00_P1   reade     TABEL00F
044100131023     C                   enddo
044200131023     C                   endif
044300131030     C*
044400131030     C* Carico i codici varie personalizzati per cliente
044500131030     C                   z-add     1             j4
044600131030     C                   dow       j4 <= %elem(CHRGCODES) and
044700131030     C                                   CHRGCODES(j4) <> *blanks
044800131030     C                   eval      DS_CHRGCODE = CHRGCODES(j4)
044900131030     C                   eval      skCHRG_BRT(j4) = CHRGCODE_BRT
045000131030     C                   eval      skCHRG_CLI(j4) = CHRGCODE_CLI
045100131030     C                   eval      skCHRG_DES(j4) = CHRGCODE_DES
045200131030     C                   add       1             j4
045300131030     C                   enddo
045400041015     C*
045500041015     C                   ENDSR
045600131004
045700131004
045800131004     C     Char_Speciali BEGSR
045900131111     C*
046000131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
046100131111     C*
046200131111     C/EXEC SQL
046300131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
046400131111     C/END-EXEC
046500131111     C*
046600131111     C/EXEC SQL
046700131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
046800131111     C/END-EXEC
046900131111     C*
047000131111     C/EXEC SQL
047100131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
047200131111     C/END-EXEC
047300131111     C*
047400131111     C/EXEC SQL
047500131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
047600131111     C/END-EXEC
047700131111     C*
047800131111     C/EXEC SQL
047900131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
048000131111     C/END-EXEC
048100131111     C*
048200131004     C                   ENDSR
048300131004
048400131004
048500131004     C     wriHeader     BEGSR
048600131010     C*
048700131019     C*  UNB -
048800131004     C                   eval      vawDTA = UNB_sgm
048900131018     C                              + %trim(w§UNB_ID_1)+%trim(w§UNB_ID_2)
049000131004     C                              +   piu
049100131004     C                              + %trim(currDate6inv)
049200131004     C                              +   diviso
049300131004     C                              + %trim(currHHMM)
049400131004     C                              +   piu
049500131004     C                              + %trim(
049600131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
049700131004     C                              +   apice
049800131004     C                   exsr      wriOUTPUT
049900131004     C*
050000131004     C                   ENDSR
050100131019
050200131019
050300131019     C     wriSingle     BEGSR
050400131021     C*
050500131021     C* Reperisco i dati societari di fatturazione
050600131021     C                   exsr      Decod_societa
050700131022     C*
050800131022     C* Reperisco i dati della registrazione contabile
050900131022     C                   exsr      rtvContab
051000131019     C*
051100131019     C*  UNH -
051200131019     C                   eval      Conta_segm  = Conta_segm  + 1
051300131019     C                   eval      vawDTA = UNH_sgm_1
051400131022     C                              + %trim(%editc(Conta_msg:'3'))
051500131019     C                              + UNH_sgm_2
051600131019     C                              +   apice
051700131019     C                   exsr      wriOUTPUT
051800131019     C*
051900131019     C*  BGM -
052000131019     C                   eval      Conta_segm  = Conta_segm  + 1
052100131019     C                   eval      vawDTA = BGM_sgm_1
052200140506     C                              + %trim(%editc(vafNFT:'X'))
052300140506     C                              + %trim(%editc(vafFIV:'X'))
052400131019     C                              + BGM_sgm_2
052500131019     C                              +   apice
052600131019     C                   exsr      wriOUTPUT
052700131019     C*
052800131019     C*  DTM 3 -
052900131019     C                   eval      Conta_segm  = Conta_segm  + 1
053000131019     C                   eval      vawDTA = DTM_3_1
053100131022     C                              + %trim(%editc(vafDFT:'3'))
053200131019     C                              + DTM_3_2
053300131019     C                              +   apice
053400131019     C                   exsr      wriOUTPUT
053500131019     C*
053600131023     C*  DTM 137 -
053700131019     C                   eval      Conta_segm  = Conta_segm  + 1
053800131019     C                   eval      vawDTA = DTM_137_1
053900131023     C                              + %trim(currDate8)
054000131019     C                              + DTM_137_2
054100131019     C                              +   apice
054200131019     C                   exsr      wriOUTPUT
054300131019     C*
054400131019     C*  RFF IT -
054500131019     C                   eval      Conta_segm  = Conta_segm  + 1
054600131019     C                   eval      vawDTA = RFF_IT_sgm
054700131030     C                              + %trim(%editc(w§PPTKSU:'3'))
054800131019     C                              +   apice
054900131019     C                   exsr      wriOUTPUT
055000131019     C*
055100131019     C*  NAD CA -
055200131019     C                   eval      Conta_segm  = Conta_segm  + 1
055300131021     C                   eval      vawDTA = NAD_CA_1
055400131019     C                              + %trim(w§UNB_ID_1)
055500131019     C                              +   piu
055600131019     C                              +   piu
055700131021     C                              + %trim(wNAD_CA_2)
055800131021     C                              + %trim(wNAD_CA_3)
055900131019     C                              +   apice
056000131019     C                   exsr      wriOUTPUT
056100131019     C*
056200131019     C*  RFF VA 1 -
056300131019     C                   eval      Conta_segm  = Conta_segm  + 1
056400131025     C                   eval      vawDTA = %trim(wRFF_VA_1_sgm)
056500131019     C                              +   apice
056600131019     C                   exsr      wriOUTPUT
056700131019     C*
056800131019     C*  NAD SF -
056900131019     C                   eval      Conta_segm  = Conta_segm  + 1
057000131019     C                   eval      vawDTA = NAD_SF_1
057100131019     C                              + NAD_SF_2
057200131019     C                              + NAD_SF_3
057300131019     C                              + NAD_SF_4
057400131019     C                              + NAD_SF_5
057500131128     C                              + NAD_SF_6
057600131019     C                              +   apice
057700131019     C                   exsr      wriOUTPUT
057800131019     C*
057900131019     C*  NAD IV -
058000131019     C                   eval      Conta_segm  = Conta_segm  + 1
058100131019     C                   eval      vawDTA = NAD_IV_1
058200131019     C                              + NAD_IV_2
058300131019     C                              + NAD_IV_3
058400131019     C                              + NAD_IV_4
058500131019     C                              + NAD_IV_5
058600131019     C                              + NAD_IV_6
058700131128     C                              + NAD_IV_7
058800131019     C                              +   apice
058900131019     C                   exsr      wriOUTPUT
059000131019     C*
059100131019     C*  RFF VA 2 -
059200131019     C                   eval      Conta_segm  = Conta_segm  + 1
059300131019     C                   eval      vawDTA = RFF_VA_2_sgm
059400131019     C                              +   apice
059500131019     C                   exsr      wriOUTPUT
059600131019     C*
059700131019     C*  CUX -
059800131019     C                   eval      Conta_segm  = Conta_segm  + 1
059900131019     C                   eval      vawDTA = CUX_sgm
060000131019     C                              +   apice
060100131019     C                   exsr      wriOUTPUT
060200131019     C*
060300131019     C*  PAT -
060400131019     C                   eval      Conta_segm  = Conta_segm  + 1
060500131019     C                   eval      vawDTA = PAT_sgm
060600131022     C                              + %trim(%editc(wPagamGG:'3'))
060700131019     C                              +   apice
060800131019     C                   exsr      wriOUTPUT
060900131019     C*
061000131019     C*  PCD -
061100131019     C                   eval      Conta_segm  = Conta_segm  + 1
061200131019     C                   eval      vawDTA = PCD_sgm
061300131019     C                              +   apice
061400131019     C                   exsr      wriOUTPUT
061500131019     C*
061600131019     C                   ENDSR
061700131019
061800131019
061900140407     C     wriSummary    BEGSR
062000140407     C*
062100140407     C* Come ultima spedizione e prima dei dati di sommario della fattura corrente
062200140407     C* si inserisce una bolla fake relativa al bollo fattura in esenzione IVA
062300150401     C* (solo se effettivamente presente il bollo)
062400150401     C                   if        wBOLLO > *zeros
062500140407     C                   exsr      exeBollaBollo
062600150401     C                   endif
062700131019     C*
062800131019     C*  UNS -
062900131019     C                   eval      Conta_segm  = Conta_segm  + 1
063000131019     C                   eval      vawDTA = UNS_sgm
063100131019     C                              +   apice
063200131019     C                   exsr      wriOUTPUT
063300131019     C*
063400131019     C*  CNT 2 -
063500131019     C                   eval      Conta_segm  = Conta_segm  + 1
063600131019     C                   eval      vawDTA = CNT_2_sgm
063700131022     C                              + %trim(%editc(Conta_item:'3'))
063800131019     C                              +   apice
063900131019     C                   exsr      wriOUTPUT
064000131019     C*
064100131019     C*  MOA 79 -
064200131019     C                   eval      Conta_segm  = Conta_segm  + 1
064300131019     C                   eval      vawDTA = MOA_79_sgm
064400131219     C                              + %trim(%editc(wTotIMV:'3'))
064500131019     C                              +   apice
064600131019     C                   exsr      wriOUTPUT
064700131019     C*
064800131205     C*  MOA 58 -
064900140407     C                   eval      Conta_segm  = Conta_segm  + 1
065000140407     C                   eval      vawDTA = MOA_58_sgm
065100140407     C                              + %trim(%editc(wBOLLO:'3'))
065200140407     C                              +   apice
065300140407     C                   exsr      wriOUTPUT
065400131019     C*
065500131019     C*  MOA 77 -
065600131019     C                   eval      Conta_segm  = Conta_segm  + 1
065700131019     C                   eval      vawDTA = MOA_77_sgm
065800131022     C                              + %trim(%editc(wTotFATT:'3'))
065900131019     C                              +   apice
066000131019     C                   exsr      wriOUTPUT
066100131022     C*
066200131022     C* Ciclo per tutti gli imponibili/aliquote presenti in fattura
066300131023     C                   dow       j1 > *zeros
066400131022     C*
066500131022     C*  TAX -
066600131022     C                   eval      Conta_segm  = Conta_segm  + 1
066700131022     C                   eval      vawDTA = TAX_sgm
066800131023     C                              + %trim(%editc(skIVAALQ(j1):'3'))
066900131019     C                              +   apice
067000131019     C                   exsr      wriOUTPUT
067100131019     C*
067200131205     C*  MOA 124 -
067300131019     C                   eval      Conta_segm  = Conta_segm  + 1
067400131019     C                   eval      vawDTA = MOA_124_sgm
067500131023     C                              + %trim(%editc(skIVAIMP(j1):'3'))
067600131019     C                              +   apice
067700131019     C                   exsr      wriOUTPUT
067800131019     C*
067900131205     C*  MOA 125 -
068000131019     C                   eval      Conta_segm  = Conta_segm  + 1
068100131019     C                   eval      vawDTA = MOA_125_sgm
068200131023     C                              + %trim(%editc(skIMPONB(j1):'3'))
068300131022     C                              +   apice
068400131022     C                   exsr      wriOUTPUT
068500131022     C*
068600131023     C                   sub       1             j1
068700131022     C                   enddo
068800131019     C*
068900131019     C*  UNT -
069000131019     C                   eval      Conta_segm  = Conta_segm  + 1
069100131019     C                   eval      vawDTA = UNT_sgm
069200131022     C                              + %trim(%editc(Conta_segm:'3'))
069300131019     C                              +   piu
069400131022     C                              + %trim(%editc(Conta_msg:'3'))
069500131019     C                              +   apice
069600131019     C                   exsr      wriOUTPUT
069700131019     C*
069800131019     C                   ENDSR
069900131019
070000131004
070100131004     C     wriFooter     BEGSR
070200131004     C*
070300131004     C*  UNZ -
070400131004     C                   eval      vawDTA = UNZ_sgm
070500131004     C                              +   piu
070600131022     C                              + %trim(%editc(Conta_msg:'3'))
070700131004     C                              +   piu
070800131004     C                              + %trim(
070900131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
071000131004     C                              +   apice
071100131004     C                   exsr      wriOUTPUT
071200131004     C*
071300131004     C                   ENDSR
071400131019
071500131019
071600131019     C     exeInz        BEGSR
071700131019     C*
071800131019     C                   clear                   wTotIMV
071900131019     C                   clear                   wTotIVA
072000131019     C                   clear                   wTotFATT
072100131019     C                   clear                   Conta_msg
072200131019     C                   clear                   Conta_item
072300131019     C                   clear                   Conta_segm
072400131019     C*
072500131019     C                   ENDSR
072600030924
072700030924
072800991027     C     traduci       BEGSR
072900131004     C*
073000131019     C* Inizializzo i contatori a livello di transazione/messaggio
073100131019     C                   exsr      exeInz
073200131019     C*
073300131019     C                   READ      tivgdtmp
073400131019     C                   dow       not %eof(tivgdtmp)
073500131019     C                   movel     vgddta        tivafds
073600131022     C*
073700131022     C* Innanzitutto chaino la bolla direttamente su TITAS
073800131022     C     KEYtas30P     chain     titas30c
073900131022     C                   if        %found(titas30c)
074000160208     C*
074100160208     C* Reprrisco NATURA MERCE
074200160208     C                   EXSR      RTVNAS
074300131019     C*
074400140107     C* Effettuo rottura di codice per numero fattura/registro IVA
074500140107     C                   if        savNFT <> vafNFT AND savFIV <> vafFIV
074600140107     C                   if        savNFT  = *zeros AND savFIV  = *zeros
074700131019     C* Scarico i dati di testata fattura
074800131019     C                   exsr      wriSingle
074900131022     C* Elaboro la bolla corrente
075000131022     C                   exsr      exeBolla
075100131019     C*
075200131019     C                   else
075300131019     C* Elaboro la bolla corrente
075400131022     C                   exsr      exeBolla
075500131022     C* Scarico i dati di sommario a livello di fattura
075600131022     C                   exsr      wriSummary
075700131019     C* Inizializzo i contatori a livello di transazione/messaggio
075800131019     C                   exsr      exeInz
075900131019     C* Scarico i dati di testata fattura
076000131019     C                   exsr      wriSingle
076100131019     C                   endif
076200131019     C*
076300131019     C                   else
076400131019     C* Elaboro la bolla corrente
076500131022     C                   exsr      exeBolla
076600131019     C                   endif
076700131019     C
076800131019     C* Salvo i nuovi valori della rottuta di codice
076900131019     C                   eval      savNFT = vafNFT
077000140107     C                   eval      savFIV = vafFIV
077100131022     C*
077200131022     C                   endif
077300131019     C*
077400131019     C                   delete    tivgd000
077500131019     C*                                                               50
077600131019     C                   read      tivgdtmp
077700131019     C                   enddo
077800131022     C*
077900131022     C* Scarico i dati di sommario a livello di fattura
078000131022     C                   exsr      wriSummary
078100991027     C*
078200030325     C                   EVAL      wrkesito = '0'
078300991027     C*
078400910830     C                   ENDSR
078500131019
078600131019
078700140407     C     exeBolla      BEGSR
078800131019     C*
078900131019     C* Incremento il contatore di item (ovvero spedizione)
079000131019     C                   add       1             Conta_item
079100131023     C*
079200131023     C* Reperisco i dati di dettaglio fatturazione spedizione corrente
079300131023     C                   exsr      rtvSvn
079400131019     C*
079500131019     C* Scarico il buffer d output
079600131019     C                   exsr      wriDettagli
079700131019     C*
079800131019     C                   ENDSR
079900140407
080000140407
080100140407     C     exeBollaBollo BEGSR
080200140407     C*
080300140407     C* Incremento il contatore di item (ovvero spedizione)
080400140407     C                   add       1             Conta_item
080500140407     C*
080600140407     C* Scarico il buffer d output
080700140407     C                   exsr      wriBollaFake
080800140407     C*
080900140407     C                   ENDSR
081000140407
081100140407
081200140407     C     wriBollaFake  BEGSR
081300140407     C*
081400140407     C* Incremento il contatore di messaggio (al interno della transazione)
081500140407     C                   eval      Conta_msg = Conta_msg + 1
081600140407     C*
081700140407     C* Compongo segmenti per dati di dettaglio spedizione corrente
081800140407     C*
081900140407     C*  LIN -
082000140407     C                   eval      Conta_segm  = Conta_segm  + 1
082100140407     C                   eval      vawDTA = LIN_sgm
082200140407     C                              + %trim(%editc(Conta_item:'3'))
082300140407     C                              +   apice
082400140407     C                   exsr      wriOUTPUT
082500140407     C*
082600140407     C*  IMD -
082700140407     C                   eval      Conta_segm  = Conta_segm  + 1
082800140407     C                   eval      vawDTA = IMD_sgm_Bollo
082900140407     C                              +   apice
083000140407     C                   exsr      wriOUTPUT
083100170411     C*
083200170411     C*  MEA WT -
083300170411     C                   eval      Conta_segm  = Conta_segm  + 1
083400170411     C                   eval      vawDTA = MEA_WT_sgm
083500170411     C                              + '0'
083600170411     C                              +   apice
083700170411     C                   exsr      wriOUTPUT
083800140407     C*
083900140407     C*  MOA 203 -
084000140407     C                   eval      Conta_segm  = Conta_segm  + 1
084100140407     C                   eval      vawDTA = MOA_203_sgm
084200140407     C                              + '0'
084300140407     C                              +   apice
084400140407     C                   exsr      wriOUTPUT
084500140407     C*
084600140407     C*  PRI -
084700140407     C                   eval      Conta_segm  = Conta_segm  + 1
084800140407     C                   eval      vawDTA = PRI_sgm
084900140407     C                              + '0'
085000140407     C                              +   apice
085100140407     C                   exsr      wriOUTPUT
085200140407     C*
085300140407     C*  TAX -
085400140407     C                   eval      Conta_segm  = Conta_segm  + 1
085500140407     C                   eval      vawDTA = TAX_sgm
085600140407     C                              + '0'
085700140407     C                              +   apice
085800140407     C                   exsr      wriOUTPUT
085900170904     C*
086000170904     C*  MOA 124 -
086100170904     C                   eval      Conta_segm  = Conta_segm  + 1
086200170904     C                   eval      vawDTA = MOA_124_sgm
086300170904     C                              + '0'
086400170904     C                              +   apice
086500170904     C                   exsr      wriOUTPUT
086600140407     C*
086700140407     C*  NAD CN -
086800140407     C                   eval      Conta_segm  = Conta_segm  + 1
086900140407     C                   eval      vawDTA = NAD_CN
087000140407     C                              + 'DUMMY DATA'
087100140407     C                              +   piu
087200140407     C                              +   piu
087300140407     C                              + 'DUMMY DATA'
087400140407     C                              +   piu
087500140407     C                              + 'BOLOGNA'
087600140407     C                              +   piu
087700140407     C                              + 'BO'
087800140407     C                              +   piu
087900140407     C                              + '40138'
088000140407     C                              +   piu
088100140407     C                              + 'IT'
088200140407     C                              +   apice
088300140407     C                   exsr      wriOUTPUT
088400140407     C*
088500140407     C*  RFF AAM -
088600140407     C                   eval      Conta_segm  = Conta_segm  + 1
088700140407     C                   eval      vawDTA = RFF_AAM_sgm
088800140407     C                              + '00000'
088900140407     C                              +   apice
089000140407     C                   exsr      wriOUTPUT
089100140407     C*
089200140407     C*  RFF CR -
089300140407     C                   eval      Conta_segm  = Conta_segm  + 1
089400140407     C                   eval      vawDTA = RFF_CR_sgm
089500140407     C                              + '00000'
089600140407     C                              +   apice
089700140407     C                   exsr      wriOUTPUT
089800140407     C*
089900140407     C*  RFF ACC -
090000140407     C                   eval      Conta_segm  = Conta_segm  + 1
090100140407     C                   eval      vawDTA = RFF_ACC_sgm
090200140407     C                              + '00000'
090300140407     C                              +   apice
090400140407     C                   exsr      wriOUTPUT
090500140407     C*
090600140407     C*  DTM 200 -
090700140407     C                   eval      Conta_segm  = Conta_segm  + 1
090800140407     C                   eval      vawDTA = DTM_200_1
090900140407     C                              + %trim(%editc(vafDFT:'X'))
091000140407     C                              + DTM_200_2
091100140407     C                              +   apice
091200140407     C                   exsr      wriOUTPUT
091300140407     C*
091400140407     C*  ALC -
091500140407     C                   eval      Conta_segm  = Conta_segm  + 1
091600140407     C                   eval      vawDTA = ALC_sgm_Bollo
091700140407     C                              + %trim(skALC(j3))
091800140407     C                              +   apice
091900140407     C                   exsr      wriOUTPUT
092000140407     C*
092100140407     C*  MOA 8 -
092200140407     C                   eval      Conta_segm  = Conta_segm  + 1
092300170915     C                   eval(h)   wVAL2D = wBOLLO
092400140407     C                   eval      vawDTA = MOA_8_sgm
092500170915     C                              + %trim(%editc(wVAL2D:'3'))
092600140407     C                              +   apice
092700140407     C                   exsr      wriOUTPUT
092800140407     C*
092900140407     C*  TAX 7 -
093000140407     C                   eval      Conta_segm  = Conta_segm  + 1
093100140407     C                   eval      vawDTA = TAX_sgm
093200140407     C                              + '0'
093300140407     C                              +   apice
093400140407     C                   exsr      wriOUTPUT
093500170906     C*
093600170906     C*  MOA 124 -
093700170906     C                   eval      Conta_segm  = Conta_segm  + 1
093800170906     C                   eval      vawDTA = MOA_124_sgm
093900170906     C                              + '0'
094000170906     C                              +   apice
094100170906     C                   exsr      wriOUTPUT
094200140407     C*
094300140407     C                   ENDSR
094400131022
094500131022
094600131022     C     rtvContab     BEGSR
094700131022     C*
094800131205     C                   clear                   wBOLLO
094900131023     C                   z-add     *zeros        j1
095000131022     C                   clear                   NDREG000
095100131022     C*
095200131022     C                   eval      REGSOCIETA = societa
095300131022     C                   eval      REGCTB     = 'CG'
095400131022     C                   eval      REGDTREG   = %date(vafDFT)
095500131022     C                   eval      REGNRREG   = vafNFT
095600131022     C                   eval      REGSERREG  = '2'+ %trim(%editc(vafFIV:'X'))
095700131022     C*
095800131022     C* Aggancio la testata della registrazione contabile
095900131022     C     KEYreg04C     chain     ndreg04l
096000131022     C                   if        %found(ndreg04l)
096100131022     C*
096200131022     C* Filtro solamente le caussali 'NFT0'
096300131022     C                   if        REGCAUSTES = 'NFT0'
096400131023     C*
096500131023     C* Reperisco subito importo totale fattura
096600131023     C                   eval      wTotFATT = wTotFATT + REGIMPORTO
096700131022     C*
096800131022     C* Aggancio il dettaglio della registrazione contabile
096900131022     C     KEYiva01P     setll     ndiva01l
097000131022     C                   if        %found(ndiva01l)
097100131022     C     KEYiva01P     reade     ndiva01l
097200131022     C                   dow       not %eof(ndiva01l)
097300131022     C*
097400131022     C* Filtro solamente le caussali 'NFT0'
097500131022     C                   if        IVACAUSTES = 'NFT0'
097600131022     C*
097700131022     C* Reperisco i dati relativi a aliquota IVA, imponibile, importo IVA
097800131023     C                   if        IVAALIQ > wCurrIVA
097900131022     C                   eval      wCurrIVA = IVAALIQ
098000131022     C                   endif
098100131022     C*
098200131022     C                   eval      wTotIVA  = wTotIVA  + IVAIMPORTO
098300131219 xxx C***                eval      wTotIMV  = wTotIMV  + IVAIMPONIB
098400131022     C*
098500131205     C* Nei righi IVA tengo separato il bollo
098600131205     C                   if        IVARIFIVA = '150'
098700131205     C                   eval      wBOLLO = IVAIMPONIB
098800131205     C                   else
098900131023     C                   add       1             j1
099000131023     C                   eval      skIVAIMP(j1) = IVAIMPORTO
099100131023     C                   eval      skIVAALQ(j1) = IVAALIQ
099200131205     C                   eval      skIMPONB(j1) = IVAIMPONIB
099300131205     C                   endif
099400131022     C*
099500131022     C                   endif
099600131022     C*
099700131022     C     KEYiva01P     reade     ndiva01l
099800131022     C                   enddo
099900131022     C*
100000131022     C* Aggancio il partitario scadenze
100100131022     C                   eval      PASNRRIGAM = 1
100200131023     C     KEYpas01P     chain     ndpas01l
100300131022     C                   if        %found(ndpas01l)
100400131022     C*
100500131022     C* Filtro solamente le caussali 'NFT0'
100600131022     C                   if        PASCAUSRIG = 'NFT0'
100700131022     C*
100800131022     C* Reperisco il numero di giorni pagamento data fattura
100900131022     C                   eval      wPagamGG =  %diff(PASDTSCA:REGDTREG:*DAYS)
101000131022     C*
101100131022     C                   endif
101200131022     C*
101300131022     C                   endif
101400131022     C*
101500131022     C                   endif
101600131022     C                   endif
101700131022     C                   endif
101800131022     C*
101900131022     C                   ENDSR
102000131023
102100131023
102200131023     C*------------------------------------------------------------------------*
102300131023     C* RTVSVN - Routine di reperimento VARIE fatturate
102400131023     C*------------------------------------------------------------------------*
102500131023     C     rtvSvn        BEGSR
102600131023     C*
102700131023     C* Inizializzo i dettagli di fatturazione
102800131023     C                   clear                   skALC
102900131023     C                   clear                   skALC_DES
103000131023     C                   clear                   skMOA_8
103100131023     C                   clear                   skTAX_7
103200170906     C                   clear                   skMOA_124
103300131023     C                   z-add     *zeros        j3
103400131023     C*
103500131023     C* Verifica ed eventuale scrittura del PORTO su TITAS
103600131023     C                   IF        tasPOR <> *zeros
103700131023     C                   EVAL      wSVN = '|'
103800131023     C                   EVAL      wVAN = tasPOR
103900131023     C                   EVAL      wDES = 'PORTO'
104000131023     C                   EXSR      WRISVN
104100131023     C                   ENDIF
104200131023     C*
104300131023     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
104400131023     C                   IF        tasSV1 <> *blanks
104500131023     C                   EVAL      wSVN = tasSV1
104600131023     C                   EVAL      wVAN = tasVA1
104700131023     C                   EVAL      wDES = *blanks
104800131023     C                   EXSR      WRISVN
104900131023     C                   ENDIF
105000131023     C*
105100131023     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
105200131023     C                   IF        tasSV2 <> *blanks
105300131023     C                   EVAL      wSVN = tasSV2
105400131023     C                   EVAL      wVAN = tasVA2
105500131023     C                   EVAL      wDES = *blanks
105600131023     C                   EXSR      WRISVN
105700131023     C                   ENDIF
105800131023     C*
105900131023     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
106000131023     C                   IF        tasSV3 <> *blanks
106100131023     C                   EVAL      wSVN = tasSV3
106200131023     C                   EVAL      wVAN = tasVA3
106300131023     C                   EVAL      wDES = *blanks
106400131023     C                   EXSR      WRISVN
106500131023     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
106600131023     C     KEYta730P     SETLL     tita730c
106700131023     C                   IF        %equal(tita730c)
106800131023     C     KEYta730P     READE     tita730c
106900131023     C                   DOW       not %eof(tita730c)
107000131023     C                   if        ta7VAN > *zeros
107100131023     C                   EVAL      wSVN = ta7SVN
107200131023     C                   EVAL      wVAN = ta7VAN
107300131023     C                   EVAL      wDES = *blanks
107400131023     C                   EXSR      WRISVN
107500131023     C                   endif
107600131023     C     KEYta730P     READE     tita730c
107700131023     C                   ENDDO
107800131023     C                   ENDIF
107900131023     C                   ENDIF
108000131023     C*
108100131023     C                   ENDSR
108200131023
108300131023
108400131023     C*------------------------------------------------------------------------*
108500131023     C* WRISVN - Routine di valorizzazione wrk-file VARIE fatturate
108600131023     C*------------------------------------------------------------------------*
108700131023     C     wriSvn        BEGSR
108800131023     C*
108900131023     C* Elaboro tutto tranne PORTO e la varia 8 (IVA)
109000131023     C                   if        wSVN <> '8' and wSVN <> '|'
109100131023     C                   add       1             j3
109200170906     C                   eval      skALC(j3)     = wSVN
109300170906     C                   eval      skMOA_8(j3)   = wVAN
109400131023     C*
109500131023     C* Reperisco la descrizione della VARIA
109600131023     C                   if        wDES = *blanks
109700131023     C                   z-add     1             j2
109800131023     C     wSVN          lookup    skSVN(j2)                              55
109900131023     C                   if        %found
110000131025     C                   eval      skALC_DES(j3) = skSVNDES(j2)
110100131023     C                   else
110200131023     C                   eval      skALC_DES(j3) = *all'*'
110300131023     C                   endif
110400131023     C                   else
110500131025     C                   eval      skALC_DES(j3) = wDES
110600131025     C                   endif
110700131030     C*
110800131030     C* Se richiesto effettuo mapping tra charge codes BRT e quelli CLIENTE
110900131030     C                   if        w§PPTCVT = 'Y'
111000160224     C                   setoff                                       56
111100131030     C                   z-add     1             j4
111200131030     C     wSVN          lookup    skCHRG_BRT(j4)                         55
111300131030     C                   if        %found
111400160224     C                   eval      skALC_DES(j3) = skCHRG_DES(j4)
111500131030     C                   if        skCHRG_CLI(j4) <> *blanks
111600160224     C                   seton                                        56
111700131030     C                   eval      skALC(j3)     = skCHRG_CLI(j4)
111800131030     C                   endif
111900131030     C                   endif
112000160224     C*
112100160224     C* Se richiesta mappatira ma varia non contemplata "forzo" codice varia cliente "generico"
112200160224     C                   if        not *in56
112300160224     C                   eval      skALC(j3)     = 'SC'
112400160224     C                   endif
112500131030     C                   endif
112600131025     C*
112700131025     C* Verifico eventuali caratteri non consentiti dal EDIFACT
112800131025     C* ALC DES
112900131025     C                   eval      campo_alfa = skALC_DES(j3)
113000131025     C                   exsr      Char_Speciali
113100131030     C                   eval      skALC_DES(j3) = campo_alfa
113200131023     C*
113300170906     C* Reperisco aliquota IVA della VARIA corrente e lego relativo importo IVA
113400131023     C                   z-add     1             j2
113500131023     C     wSVN          lookup    skSVNnoIVA(j2)                         55
113600131023     C                   if        %found
113700170906     C                   eval      skTAX_7(j3)   = *zeros
113800170906     C                   eval      skMOA_124(j3) = *zeros
113900131023     C                   else
114000170906     C                   eval      skTAX_7(j3)   = wCurrIVA
114100170906     C                   eval(h)   skMOA_124(j3) = skMOA_8(j3)*wCurrIVA/100
114200131023     C                   endif
114300131023     C*
114400131023     C                   endif
114500131023     C*
114600131023     C                   clear                   wSVN
114700131023     C                   clear                   wVAN
114800131023     C                   clear                   wDES
114900131023     C*
115000131023     C                   ENDSR
115100031103
115200031103
115300131019     C     wriDettagli   BEGSR
115400131023     C*
115500131004     C* Incremento il contatore di messaggio (al interno della transazione)
115600131019     C                   eval      Conta_msg = Conta_msg + 1
115700131004     C*
115800131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
115900131025     C                   movel     *blanks       wAlfaRMA        512
116000131025     C                   movel     *blanks       wAlfaRSD        512
116100131004     C                   movel     *blanks       wAlfaLOD        512
116200131004     C                   movel     *blanks       wAlfaIND        512
116300131025     C                   movel     *blanks       wAlfaNAS        512
116400131004     C* RMA
116500131019     C                   eval      campo_alfa = vafRMA
116600131004     C                   exsr      Char_Speciali
116700131004     C                   eval      wAlfaRMA = campo_alfa
116800131025     C* RSD
116900131025     C                   eval      campo_alfa = tasRSD
117000131025     C                   exsr      Char_Speciali
117100131025     C                   eval      wAlfaRSD = campo_alfa
117200131004     C* LOD
117300131004     C                   eval      campo_alfa = tasLOD
117400131004     C                   exsr      Char_Speciali
117500131004     C                   eval      wAlfaLOD = campo_alfa
117600131004     C* IND
117700131004     C                   eval      campo_alfa = tasIND
117800131004     C                   exsr      Char_Speciali
117900131004     C                   eval      wAlfaIND = campo_alfa
118000131025     C* NAS
118100160208     C                   if        §TA4ANAS <> *blanks
118200160208     C                   eval      campo_alfa = §TA4ANAS
118300131025     C                   exsr      Char_Speciali
118400131025     C                   eval      wAlfaNAS = campo_alfa
118500131025     C                   else
118600131025     C                   eval      wAlfaNAS = 'VOID'
118700131025     C                   endif
118800131004     C*
118900131004     C*
119000131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
119100131019     C*
119200131019     C*  LIN -
119300131019     C                   eval      Conta_segm  = Conta_segm  + 1
119400131019     C                   eval      vawDTA = LIN_sgm
119500131022     C                              + %trim(%editc(Conta_item:'3'))
119600131019     C                              +   apice
119700131019     C                   exsr      wriOUTPUT
119800131019     C*
119900131019     C*  IMD -
120000131019     C                   eval      Conta_segm  = Conta_segm  + 1
120100131019     C                   eval      vawDTA = IMD_sgm
120200131019     C                              +   apice
120300131019     C                   exsr      wriOUTPUT
120400170411     C*
120500170411     C*  MEA WT -
120600170411     C                   eval      Conta_segm  = Conta_segm  + 1
120700170411     C                   if        vafPKC > vafPKB
120800170411     C                   z-add     vafPKC        wPeso             7 1
120900170411     C                   else
121000170411     C                   z-add     vafPKB        wPeso
121100170411     C                   endif
121200170411     C                   eval      vawDTA = MEA_WT_sgm
121300170411     C                              + %trim(%editc(wPeso:'3'))
121400170411     C                              +   apice
121500170411     C                   exsr      wriOUTPUT
121600131019     C*
121700131019     C*  MOA 203 -
121800131019     C                   eval      Conta_segm  = Conta_segm  + 1
121900170915     C                   eval(h)   wVAL2D = vafIMV
122000170915     C                   eval      vawDTA = MOA_203_sgm
122100170915     C                              + %trim(%editc(wVAL2D:'3'))
122200131219     C                              +   apice
122300131019     C                   exsr      wriOUTPUT
122400131219     C                   eval      wTotIMV  = wTotIMV + vafIMV                  * Tot MOA+203 per 79
122500131019     C*
122600131019     C*  PRI -
122700170904     C*
122800170904     C* Ritorno il valore reale del PORTO solo se spedizione NON di "reso mittente"
122900170904     C                   if        tasFBR = *blanks
123000170904     C                   z-add     vafPOR        wPOR             11 3
123100170904     C*
123200170904     C* Se trattasi di spedizione di "reso mittente" invece ritorno il porto a zero
123300170904     C                   else
123400170904     C                   z-add     *zeros        wPOR
123500170904     C                   endif
123600170904     C*
123700170904     C                   eval      Conta_segm  = Conta_segm  + 1
123800170915     C                   eval(h)   wVAL2D = wPOR
123900170904     C                   eval      vawDTA = PRI_sgm
124000170915     C                              + %trim(%editc(wVAL2D:'3'))
124100170904     C                              +   apice
124200170904     C                   exsr      wriOUTPUT
124300131019     C*
124400131019     C*  TAX -
124500131019     C                   eval      Conta_segm  = Conta_segm  + 1
124600131019     C                   eval      vawDTA = TAX_sgm
124700131022     C                              + %trim(%editc(wCurrIVA:'3'))
124800131019     C                              +   apice
124900131019     C                   exsr      wriOUTPUT
125000170904     C*
125100170904     C*  MOA 124 -
125200170906     C***                eval(h)   wIVA = vafIMV*wCurrIVA/100
125300170906     C                   eval(h)   wIVA = wPOR*wCurrIVA/100
125400170904     C                   eval      Conta_segm  = Conta_segm  + 1
125500170904     C                   eval      vawDTA = MOA_124_sgm
125600170904     C                              + %trim(%editc(wIVA:'3'))
125700170904     C                              +   apice
125800170904     C                   exsr      wriOUTPUT
125900131018     C*
126000131019     C*  NAD CN -
126100131004     C                   eval      Conta_segm  = Conta_segm  + 1
126200131018     C                   eval      vawDTA = NAD_CN
126300131025     C                              + %trim(wAlfaRSD)
126400131004     C                              +   piu
126500131004     C                              +   piu
126600131004     C                              + %trim(wAlfaIND)
126700131004     C                              +   piu
126800131004     C                              + %trim(wAlfaLOD)
126900131004     C                              +   piu
127000131004     C                              + %trim(tasPRD)
127100131004     C                              +   piu
127200131004     C                              + %trim(tasCAD)
127300131004     C                              +   piu
127400131004     C                              + 'IT'
127500131004     C                              +   apice
127600131019     C                   exsr      wriOUTPUT
127700131019     C*
127800131022     C*  RFF AAM -
127900131019     C                   eval      Conta_segm  = Conta_segm  + 1
128000131019     C                   eval      vawDTA = RFF_AAM_sgm
128100131025     C                              + %trim(wAlfaNAS)
128200131019     C                              +   apice
128300131019     C                   exsr      wriOUTPUT
128400131019     C*
128500131019     C*  RFF CR -
128600131019     C                   eval      Conta_segm  = Conta_segm  + 1
128700131019     C                   eval      vawDTA = RFF_CR_sgm
128800131022     C                              + %trim(%editc(vafRMN:'3'))
128900131019     C                              +   apice
129000131019     C                   exsr      wriOUTPUT
129100131019     C*
129200131019     C*  RFF ACC -
129300131019     C                   eval      Conta_segm  = Conta_segm  + 1
129400131019     C                   eval      vawDTA = RFF_ACC_sgm
129500131025     C                              + %trim(wAlfaRMA)
129600131019     C                              +   apice
129700131019     C                   exsr      wriOUTPUT
129800131019     C*
129900131019     C*  DTM 200 -
130000131019     C                   eval      Conta_segm  = Conta_segm  + 1
130100131019     C                   eval      vawDTA = DTM_200_1
130200131019     C                              + %trim(%editc(tasDRT:'X'))
130300131019     C                              + DTM_200_2
130400131019     C                              +   apice
130500131019     C                   exsr      wriOUTPUT
130600131023     C*
130700131023     C* Sviluppo tutti i dettagli di fatturazione
130800160202     C* Ritorno le VARIE solo se spedizione NON di "reso mittente"
130900160201     C                   if        tasFBR = *blanks
131000131023     C                   eval      j3 = 1
131100131023     C                   dow       j3 <= %elem(skALC) and skALC(j3) <> *blanks
131200131023     C*
131300131023     C* Restituisco solamente le varie con importo
131400131023     C                   if        skMOA_8(j3) > *zeros
131500131023     C*
131600131023     C*  ALC -
131700131023     C                   eval      Conta_segm  = Conta_segm  + 1
131800131023     C                   eval      vawDTA = ALC_sgm
131900131023     C                              + %trim(skALC(j3))
132000131023     C                              +   diviso
132100131023     C                              +   diviso
132200131023     C                              +   diviso
132300131030     C                              + %trim(skALC_DES(j3))
132400131023     C                              +   apice
132500131023     C                   exsr      wriOUTPUT
132600131023     C*
132700131023     C*  MOA 8 -
132800131023     C                   eval      Conta_segm  = Conta_segm  + 1
132900170915     C                   eval(h)   wVAL2D = skMOA_8(j3)
133000131023     C                   eval      vawDTA = MOA_8_sgm
133100170915     C                              + %trim(%editc(wVAL2D:'3'))
133200131023     C                              +   apice
133300131023     C                   exsr      wriOUTPUT
133400131023     C*
133500131023     C*  TAX 7 -
133600131023     C                   eval      Conta_segm  = Conta_segm  + 1
133700131023     C                   eval      vawDTA = TAX_sgm
133800131023     C                              + %trim(%editc(skTAX_7(j3):'3'))
133900131023     C                              +   apice
134000131023     C                   exsr      wriOUTPUT
134100170906     C*
134200170906     C*  MOA 124 -
134300170906     C                   eval      Conta_segm  = Conta_segm  + 1
134400170906     C                   eval      vawDTA = MOA_124_sgm
134500170906     C                              + %trim(%editc(skMOA_124(j3):'3'))
134600170906     C                              +   apice
134700170906     C                   exsr      wriOUTPUT
134800131023     C*
134900131023     C                   endif
135000131023     C                   add       1             j3
135100131023     C                   enddo
135200160202     C*
135300160202     C* Se trattasi di spedizione di "reso mittente"
135400160202     C                   else
135500160202     C*
135600160202     C*  ALC -
135700160202     C                   eval      Conta_segm  = Conta_segm  + 1
135800160202     C                   eval      vawDTA = ALC_sgm
135900160202     C                              + 'UD'
136000160202     C                              +   diviso
136100160202     C                              +   diviso
136200160202     C                              +   diviso
136300160202     C                              + 'RETURN TO SENDER'
136400160202     C                              +   apice
136500160202     C                   exsr      wriOUTPUT
136600160202     C*
136700160202     C*  MOA 8 -
136800160202     C                   eval      Conta_segm  = Conta_segm  + 1
136900170915     C                   eval(h)   wVAL2D = vafIMV
137000160202     C                   eval      vawDTA = MOA_8_sgm
137100170915     C                              + %trim(%editc(wVAL2D:'3'))
137200160202     C                              +   apice
137300160202     C                   exsr      wriOUTPUT
137400160202     C*
137500160202     C*  TAX 7 -
137600160202     C                   eval      Conta_segm  = Conta_segm  + 1
137700160202     C                   eval      vawDTA = TAX_sgm
137800160202     C                              + %trim(%editc(wCurrIVA:'3'))
137900160202     C                              +   apice
138000160202     C                   exsr      wriOUTPUT
138100170906     C*
138200170906     C*  MOA 124 -
138300170906     C                   eval      Conta_segm  = Conta_segm  + 1
138400170906     C                   eval(h)   wIVA = vafIMV*wCurrIVA/100
138500170906     C                   eval      vawDTA = MOA_124_sgm
138600170906     C                              + %trim(%editc(wIVA:'3'))
138700170906     C                              +   apice
138800170906     C                   exsr      wriOUTPUT
138900160202     C*
139000160201     C                   endif
139100031103     C*
139200031103     C                   ENDSR
139300160208
139400160208
139500160208
139600160208     C     RTVNAS        BEGSR
139700160208     C*
139800160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
139900160208     C                   CLEAR                   DTA4A
140000160208     C*
140100160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
140200160208     C                   CALL      'UBTA400R'
140300160208     C                   PARM      *blanks       UBTA4IOPZ
140400160208     C                   PARM      *blanks       UBTA4ITLA
140500160208     C                   PARM      tasAAS        UBTA4IAAS
140600160208     C                   PARM      tasLNP        UBTA4ILNP
140700160208     C                   PARM      tasNRS        UBTA4INRS
140800160208     C                   PARM      tasNSP        UBTA4INSP
140900160208     C                   PARM      'A'           UBTA4ITRC
141000160208     C                   PARM                    UBTA4OERR
141100160208     C                   PARM                    UBTA4ODS
141200160208     C                   PARM                    UBTA4OLEN
141300160208     C                   PARM                    UBTA4ODATI
141400160208     C*
141500160208     C                   IF        UBTA4OERR = *zeros
141600160208     C                   SELECT
141700160208     C* Gestione output tipo record 'A'
141800160208     C                   WHEN      UBTA4ODS = 'DTA4A'
141900160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
142000160208     C                   ENDSL
142100160208     C*
142200160208     C                   ENDIF
142300160208     C*
142400160208     C                   ENDSR
142500031103
142600160208
142700991027
142800991027      /TITLE Operazioni iniziali.
142900991027     C     *inzsr        BEGSR
143000991027     C*
143100991027     C     *ENTRY        PLIST
143200131019     C                   parm                    prmppt
143300991027     C     wrkesito      parm      wrkesito      prmesito
143400031103     C*
143500031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
143600131019     C                   EVAL      DSPPT = prmppt
143700131004     C*
143800131004     C* Reperisco i valori variabili ma a livello di transazione corrente
143900131004     C* (tipicamente data e ora relativ al file generato)
144000131004     C                   eval      currDate6    =
144100131004     C                              UBFMTDATE_Convert(
144200131004     C                                  %editc(%dec(%date():*ISO):'X') :
144300131004     C                                  'YYYYMMDD' : 'YYMMDD')
144400131004     C                   eval      currDate6inv =
144500131004     C                              UBFMTDATE_Convert(
144600131004     C                                  %editc(%dec(%date():*ISO):'X') :
144700131004     C                                  'YYYYMMDD' : 'DDMMYY')
144800131018     C                   eval      currDate8 =
144900131018     C                              UBFMTDATE_Convert(
145000131018     C                                  %editc(%dec(%date():*ISO):'X') :
145100131023     C                                  'YYYYMMDD' : 'YYYYMMDD')
145200131004     C                   eval      currHHMM =
145300131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
145400030709     C*
145500030709     C* Definizione chiavi
145600131023     C*
145700131023     C* LETTURA tabel00f - PARZIALE
145800131023     C     KEYtab00_P1   KLIST
145900131023     C                   KFLD                    tblKUT
146000131023     C                   KFLD                    tblCOD
146100031103     C*
146200031103     C* Chiave su TITAS30C - Parziale
146300031103     C     KEYtas30P     KLIST
146400131019     C                   KFLD                    vafAAS
146500131019     C                   KFLD                    vafLNP
146600131019     C                   KFLD                    vafNRS
146700131019     C                   KFLD                    vafNSP
146800131023     C*
146900131023     C* LETTURA tita730c - PARZIALE
147000131023     C     KEYta730P     KLIST
147100131023     C                   KFLD                    tasAAS
147200131023     C                   KFLD                    tasLNP
147300131023     C                   KFLD                    tasNRS
147400131023     C                   KFLD                    tasNSP
147500131023     C                   KFLD                    tasTBL
147600131022     C*
147700131022     C* Chiave su NDREG04L - Completa
147800131022     C     KEYreg04C     KLIST
147900131022     C                   KFLD                    REGSOCIETA
148000131022     C                   KFLD                    REGCTB
148100131022     C                   KFLD                    REGDTREG
148200131022     C                   KFLD                    REGNRREG
148300131022     C                   KFLD                    REGSERREG
148400131022     C*
148500131022     C* Chiave su NDIVA01L - Parziale
148600131022     C     KEYiva01P     KLIST
148700131023     C                   KFLD                    REGSYS
148800131023     C                   KFLD                    REGNRASREG
148900131022     C*
149000131022     C* Chiave su NDPAS01L - Parziale
149100131022     C     KEYpas01P     KLIST
149200131023     C                   KFLD                    REGSYS
149300131023     C                   KFLD                    REGNRASREG
149400131022     C                   KFLD                    PASNRRIGAM
149500991027     C*
149600991027     C                   ENDSR
149700131030** CHRGCODES - CHARGE CODES BRT / CLIENTE
149800160127+         SUPPL.CARBURANTE                                                                  01
149900160127&    SC   ANN.P/ASSEGNATO                                                                   02
150000160127*    SC   DIROTTAM.TO                                                                       03
150100160127=    SC   RESO BANCALI                                                                      04
150200160127a    DCC  POD IMAGE                                                                         05
150300160127b    SC   AD.GEST.C.                                                                        06
150400160127c    SC   L.AVVISO                                                                          07
150500160127d    INS  A.C. BASE                                                                         08
150600160127e    SC   PRIORITY                                                                          09
150700160127f    FSC  FUEL SURCH                                                                        10
150800160127h    SC   H 10:30                                                                           11
150900160127k         PACKING L.                                                                        11
151000160127m    SC   AV.AFFID.                                                                         12
151100160127p         PINcode                                                                           11
151200160127t         PRENOT.RITIRO                                                                     11
151300160127z         EXPO                                                                              11
151400160127A    EDC  APPUNTAMENTO                                                                      13
151500160127B    DCC  CONS.DDT                                                                          14
151600160127C    SC   FACCH.ARR.                                                                        15
151700160127D    SC   INV/INC FT                                                                        16
151800160127E         R.A.RESP.VETT.                                                                    17
151900160127F    OSC  FUORI MISURA AZ                                                                   18
152000160127G    COD  COMP.ASSEGNO                                                                      19
152100160127H    SC   DIR.FISSO                                                                         20
152200160127I    HLD  SP.GIACENZA                                                                       21
152300160127J    RAS  ISOLA                                                                             22
152400160127K    RAS  LOC.DISAG.                                                                        23
152500160127L    SC   I.S.T.A.T.                                                                        24
152600160127M    SC   SPESE INSOLUTO                                                                    25
152700160127N    SC   ANTEPORTO                                                                         26
152800160127O    SC   RIC.DOCUM.                                                                        27
152900160127P    AHC  AI PIANI                                                                          28
153000160127Q    DAS  ZTL                                                                               29
153100160127R    INS  A.C. PLUS                                                                         30
153200160127S    SC   SUPER./GDO                                                                        31
153300160127T    SC   RIT.COMMISSIONATO                                                                 32
153400160127U    SC   RITIRO                                                                            33
153500160127W    COD  REC.C/ASSEGNI                                                                     34
153600160127X    DAS  CONS.DISAG.                                                                       35
153700160127Y    SC   RIT.ANNULL.                                                                       36
153800160127Z    FSC  AD.GEST.AP                                                                        37
1539001601272    SC   INOLTRO                                                                           38
1540001601273         A.R.                                                                              39
1541001601274         FRAGILE                                                                           40
1542001601275    SC   DIR.PESAT.                                                                        41
1543001601276    SDU  BOLLO                                                                             42
1544001601277         T.C.P.                                                                            43
1545001601278    65   I.V.A.                                                                            44
