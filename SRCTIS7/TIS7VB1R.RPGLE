000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200120314     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300890921     H*----------------------------------------------------*
000400890921     F*
000500120314
000600120314     D*------------------
000700120314     D* LINKING A DEFINIZIONI ESTERNE
000800120314     D*------------------
000900120314     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
001000120314     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
001100120314
001200100512     D*--------------------
001300100512     D* DS PASSAGGIO PARAMETRI IN/OUT
001400100512     D*--------------------
001500100512     D PARAM_IN        DS
001600100512     D  iPARDATADA                    8S 0                                      data DA
001700100512     D  iPARDATAA                     8S 0                                      data A
001800100512     D  iPARCLI                       7S 0                                      Cod. cliente
001900100719     D  iPARFLGIMI                    1A                                        S=solo ingombranti
002000100512     D  iPARTIPCLI                    1A                                        ' '=CCM K=KSC E=both
002100100512     D  iPARFLGUNI                    1A                                        U=unific S=singolo
002200100512     D  iPARCLIVGD                    7S 0                                      Cliente out VAS
002300100512     D  iPARTIPFIL                    2A                                        Tipo file out VAS
002400120314     D  iPARGGBACK                    3A                                        n° gg "back"
002500120314     D  iPARGGDAELAB                  3A                                        n° gg "da elab"
002600140702     D  iPARFLGSICS                   1A                                        S=Si Conti/Servizio
002700100512     D*
002800100512     D PARAM_OUT       DS
002900100705     D  oDATADA                       8  0
003000100705     D  oDATAA                        8  0
003100100705     D  oCODCLI                       7  0
003200100719     D  oFLGIMI                       1
003300100705     D  oFLGUNI                       1
003400100705     D  oFLGOPZ                       1
003500100705     D  oCODCLIVAS                    7  0
003600100705     D  oTIPFILE                      2
003700100705     D  oFLGEXE                       1
003800100705     D  oTIPCLI                       1
003900140702     D  oFLGSICS                      1
004000100512
004100100512     D*------------
004200100512     D* ARCHITETTURA
004300100512     D*------------
004400100512     D KPJBA         E DS
004500010402
004600010402
004700100512     C*
004800100512     C* Calcola la data corrente
004900100512     C                   z-add     *zeros        datcor            8 0
005000100512     C                   eval      datcor = %dec(%date() : *ISO)
005100060127     C*
005200100512     C* Verifico e completo i parametri ricevuti
005300100512     C                   exsr      CHKPAR
005400100512     C*
005500100512     C* Qunidi richiamo il *pgm standard
005600100512     C                   exsr      EXEELAB
005700991110     C*
005800100512     C                   seton                                        LR
005900100512
006000100512
006100100512
006200100512     C*----------------------------------------------------*
006300100512     C     CHKPAR        BEGSR
006400100512     C*
006500100512     C* Se nn già ricevute nei parametri d lancio calcolo le date DA/A
006600100512     C                   z-add     *hival        wDataDa           8 0
006700100512     C                   z-add     *hival        wDataA            8 0
006800100512     C                   if        iPARDATADA > *zeros AND
006900100512     C                             iPARDATAA  > *zeros
007000100512     C                   z-add     iPARDATADA    wDataDa
007100100512     C                   z-add     iPARDATAA     wDataA
007200100512     C                   else
007300120314     C*
007400120314     C                   z-add     *zeros        iPARGGBACKn       3 0
007500120314     C                   z-add     *zeros        iPARGGDAELABn     3 0
007600100513     C*
007700120314     C* Verifico se i parametri relativi ai giorni sono numerici
007800120314     C                   eval      PiStr=iPARGGBACK
007900120314     C                   exsr      CHKNUM
008000120314     C                   if        PiInt=*on
008100120314     C                   z-add     PiVal         iPARGGBACKn
008200120314     C*
008300100513     C                   movel     'd'           wTypeRangeBAK     1
008400120314     C                   z-add     iPARGGBACKn   wRangeBAK         3 0
008500100513     C*
008600100513     C* Verifico "valenza" parametri che pilotano il range d date elaborazione
008700100513     C                   select
008800120314     C                   when      iPARGGBACKn >  29 and
008900120314     C                             %rem(iPARGGBACKn :  30) = 0
009000100513     C                   eval      wTypeRangeBAK = 'm'
009100120314     C                   eval      wRangeBAK = %div(iPARGGBACKn :  30)
009200120314     C                   when      iPARGGBACKn > 364 and
009300120314     C                             %rem(iPARGGBACKn : 365) = 0
009400100513     C                   eval      wTypeRangeBAK = 'y'
009500120314     C                   eval      wRangeBAK = %div(iPARGGBACKn : 365)
009600100513     C                   endsl
009700120314     C*
009800120314     C* Se no numerico => gestisco costanti figurative previste
009900120314     C                   else
010000120314     C                   select
010100120314     C                   when      iPARGGBACK = 'MES'
010200120314     C                   eval      wTypeRangeBAK = '1'
010300120314     C                   eval      wRangeBAK = *zeros
010400120314     C                   endsl
010500120314     C                   endif
010600120314     C*
010700120314     C* Verifico se i parametri relativi ai giorni sono numerici
010800120314     C                   eval      PiStr=iPARGGDAELAB
010900120314     C                   exsr      CHKNUM
011000120314     C                   if        PiInt=*on
011100120314     C                   z-add     PiVal         iPARGGDAELABn
011200120314     C*
011300120314     C                   movel     'd'           wTypeRangeELA     1
011400120314     C                   z-add     iPARGGDAELABn wRangeELA         3 0
011500120314     C*
011600120314     C* Verifico "valenza" parametri che pilotano il range d date elaborazione
011700100513     C                   select
011800120314     C                   when      iPARGGDAELABn >  29 and
011900120314     C                             %rem(iPARGGDAELABn :  30) = 0
012000100513     C                   eval      wTypeRangeELA = 'm'
012100120314     C                   eval      wRangeELA = %div(iPARGGDAELABn :  30)
012200120314     C                   when      iPARGGDAELABn > 364 and
012300120314     C                             %rem(iPARGGDAELABn : 365) = 0
012400100513     C                   eval      wTypeRangeELA = 'y'
012500120314     C                   eval      wRangeELA = %div(iPARGGDAELABn : 365)
012600100513     C                   endsl
012700120314     C*
012800120314     C* Se no numerico => gestisco costanti figurative previste
012900120314     C                   else
013000120314     C                   select
013100120314     C                   when      iPARGGDAELAB = 'MES'
013200120314     C                   eval      wTypeRangeELA = '1'
013300120314     C                   eval      wRangeELA = *zeros
013400120314     C                   endsl
013500120314     C                   endif
013600100513     C*
013700120314     C* Infine stabilisco i range in termini di date (DA)
013800100513     C                   select
013900120314     C                   when      wTypeRangeBAK = '1'                          * 1° mese corr
014000120314     C                   eval      wDataDa=%dec(%date(datcor))
014100120314     C                   move      '01'          wDataDa
014200100513     C                   when      wTypeRangeBAK = 'd'                          * in giorni
014300100512     C                   eval      wDataDa=%dec(%date(datcor)-
014400100513     C                                     %days(wRangeBAK))
014500100513     C                   when      wTypeRangeBAK = 'm'                          * in mesi
014600100513     C                   eval      wDataDa=%dec(%date(datcor)-
014700100513     C                                     %months(wRangeBAK))
014800100517     C                   move      '01'          wDataDa
014900100513     C                   when      wTypeRangeBAK = 'y'                          * in anni
015000100513     C                   eval      wDataDa=%dec(%date(datcor)-
015100100706     C                                     %years(wRangeBAK) +
015200100706     C                                     %years(1))
015300100517     C                   move      '0101'        wDataDa
015400100513     C                   endsl
015500100513     C*
015600120314     C* Infine stabilisco i range in termini di date (A)
015700100513     C                   select
015800120314     C                   when      wTypeRangeELA = '1'                          * 1° mese corr
015900120314     C                   eval      wDataA =%dec(%date(datcor))
016000120314     C                   move      '01'          wDataA
016100100513     C                   when      wTypeRangeELA = 'd'                          * in giorni
016200100512     C                   eval      wDataA =%dec(%date(wDataDa)+
016300100513     C                                     %days(wRangeELA))
016400100513     C                   when      wTypeRangeELA = 'm'                          * in mesi
016500100513     C                   eval      wDataA =%dec(%date(wDataDa)+
016600100513     C                                     %months(wRangeELA))
016700100517     C                   eval      wDataA =%dec(%date(wDataA)-
016800100517     C                                     %days(1))
016900100513     C                   when      wTypeRangeELA = 'y'                          * in anni
017000100513     C                   eval      wDataA =%dec(%date(wDataDa)+
017100100513     C                                     %years(wRangeELA))
017200100517     C                   eval      wDataA =%dec(%date(wDataA)-
017300100517     C                                     %days(1))
017400100513     C                   endsl
017500100513     C*
017600100513     C                   endif
017700100512     C*
017800100512     C* Verifico i parametri ricevuti in ingresso
017900100512     C                   if        iPARCLIVGD = *zeros
018000100512     C                   move      iPARCLI       iPARCLIVGD
018100100512     C                   endif
018200100512     C*
018300100512     C                   ENDSR
018400100512     C*----------------------------------------------------*
018500100512
018600100512
018700100512
018800100512     C*----------------------------------------------------*
018900100512     C     EXEELAB       BEGSR
019000100512     C*
019100100512     C* Valorizzo la DS d passaggio parametri
019200100512     C                   clear                   PARAM_OUT
019300100705     C*
019400100512     C                   eval      oDATADA     = wDataDa
019500100512     C                   eval      oDATAA      = wDataA
019600100512     C                   eval      oCODCLI     = iPARCLI
019700100719     C                   eval      oFLGIMI     = iPARFLGIMI
019800100512     C                   eval      oFLGUNI     = iPARFLGUNI
019900100512     C                   eval      oCODCLIVAS  = iPARCLIVGD
020000100512     C                   eval      oTIPFILE    = iPARTIPFIL
020100100512     C                   eval      oTIPCLI     = iPARTIPCLI
020200100705     C                   eval      oFLGOPZ     = '2'
020300140702     C                   eval      oFLGSICS    = iPARFLGSICS
020400100512     C*
020500100512     C* Quindi elabora
020600100512     C                   MOVEL     PARAM_OUT     KPJBU
020700100719     C                   CALL      'TIS7VBC'
020800100512     C                   PARM                    KPJBA
020900100512     C*
021000100512     C                   ENDSR
021100100512     C*----------------------------------------------------*
021200100512
021300120314
021400120314
021500120314     C*----------------------------------------------------*
021600120314     C*  CONTROLLO NUMERICITA' CAMPI
021700120314     C*----------------------------------------------------*
021800120314     C     CHKNUM        BEGSR
021900120314     C*
022000120314     C                   IF        PiDecChr = *blanks
022100120314     C                   EVAL      PiDecChr = '.'
022200120314     C                   ENDIF
022300120314     C*
022400120314     C                   callp(e)  UBISNUM_Check(PiStr
022500120314     C                                          :PiDecChr
022600120314     C                                          :PiVal
022700120314     C                                          :PiNum
022800120314     C                                          :PiInt)
022900120314     C*
023000120314     C                   IF        %error
023100120314     C                   EVAL      PiInt=*off
023200120314     C                   ENDIF
023300120314     C*
023400120314     C                   ENDSR
023500120314     C***
023600120314
023700100512
023800100512
023900090424     C*----------------------------------------------------*
024000090424     C     *INZSR        BEGSR
024100090424     C*
024200090424     C     *ENTRY        PLIST
024300090424     C                   PARM                    KPJBA
024400100512     C                   MOVEL     KPJBU         PARAM_IN
024500100512     C*
024600100512     C                   ENDSR
