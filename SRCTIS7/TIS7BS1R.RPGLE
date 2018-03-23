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
001900120326     D  iPARFLGDBR                    1A                                        S=sì brd/N=no brd/T
002000120323     D  iPARFLGNRS                    1A                                        S=con nrs/N=no nrs/T
002100100512     D  iPARFLGUNI                    1A                                        U=unific S=singolo
002200100512     D  iPARCLIVGD                    7S 0                                      Cliente out VAS
002300100512     D  iPARTIPFIL                    2A                                        Tipo file out VAS
002400120314     D  iPARGGBACK                    3A                                        n° gg "back"
002500120314     D  iPARGGDAELAB                  3A                                        n° gg "da elab"
002600100512     D*
002700100512     D PARAM_OUT       DS
002800100705     D  oDATADA                       8  0
002900100705     D  oDATAA                        8  0
003000100705     D  oCODCLI                       7  0
003100120323     D  oFLGDBR                       1
003200120323     D  oFLGNRS                       1
003300100705     D  oFLGUNI                       1
003400120323     D  oFLGOPZ                       1
003500100705     D  oCODCLIVAS                    7  0
003600100705     D  oTIPFILE                      2
003700120323     D  oFLGEXE                       1
003800100512
003900100512     D*------------
004000100512     D* ARCHITETTURA
004100100512     D*------------
004200100512     D KPJBA         E DS
004300010402
004400010402
004500100512     C*
004600100512     C* Calcola la data corrente
004700100512     C                   z-add     *zeros        datcor            8 0
004800100512     C                   eval      datcor = %dec(%date() : *ISO)
004900060127     C*
005000100512     C* Verifico e completo i parametri ricevuti
005100100512     C                   exsr      CHKPAR
005200100512     C*
005300100512     C* Qunidi richiamo il *pgm standard
005400100512     C                   exsr      EXEELAB
005500991110     C*
005600100512     C                   seton                                        LR
005700100512
005800100512
005900100512
006000100512     C*----------------------------------------------------*
006100100512     C     CHKPAR        BEGSR
006200100512     C*
006300100512     C* Se nn già ricevute nei parametri d lancio calcolo le date DA/A
006400100512     C                   z-add     *hival        wDataDa           8 0
006500100512     C                   z-add     *hival        wDataA            8 0
006600100512     C                   if        iPARDATADA > *zeros AND
006700100512     C                             iPARDATAA  > *zeros
006800100512     C                   z-add     iPARDATADA    wDataDa
006900100512     C                   z-add     iPARDATAA     wDataA
007000100512     C                   else
007100120314     C*
007200120314     C                   z-add     *zeros        iPARGGBACKn       3 0
007300120314     C                   z-add     *zeros        iPARGGDAELABn     3 0
007400100513     C*
007500120314     C* Verifico se i parametri relativi ai giorni sono numerici
007600120314     C                   eval      PiStr=iPARGGBACK
007700120314     C                   exsr      CHKNUM
007800120314     C                   if        PiInt=*on
007900120314     C                   z-add     PiVal         iPARGGBACKn
008000120314     C*
008100100513     C                   movel     'd'           wTypeRangeBAK     1
008200120314     C                   z-add     iPARGGBACKn   wRangeBAK         3 0
008300100513     C*
008400100513     C* Verifico "valenza" parametri che pilotano il range d date elaborazione
008500100513     C                   select
008600120314     C                   when      iPARGGBACKn >  29 and
008700120314     C                             %rem(iPARGGBACKn :  30) = 0
008800100513     C                   eval      wTypeRangeBAK = 'm'
008900120314     C                   eval      wRangeBAK = %div(iPARGGBACKn :  30)
009000120314     C                   when      iPARGGBACKn > 364 and
009100120314     C                             %rem(iPARGGBACKn : 365) = 0
009200100513     C                   eval      wTypeRangeBAK = 'y'
009300120314     C                   eval      wRangeBAK = %div(iPARGGBACKn : 365)
009400100513     C                   endsl
009500120314     C*
009600120314     C* Se no numerico => gestisco costanti figurative previste
009700120314     C                   else
009800120314     C                   select
009900120314     C                   when      iPARGGBACK = 'MES'
010000120314     C                   eval      wTypeRangeBAK = '1'
010100120314     C                   eval      wRangeBAK = *zeros
010200120314     C                   endsl
010300120314     C                   endif
010400120314     C*
010500120314     C* Verifico se i parametri relativi ai giorni sono numerici
010600120314     C                   eval      PiStr=iPARGGDAELAB
010700120314     C                   exsr      CHKNUM
010800120314     C                   if        PiInt=*on
010900120314     C                   z-add     PiVal         iPARGGDAELABn
011000120314     C*
011100120314     C                   movel     'd'           wTypeRangeELA     1
011200120314     C                   z-add     iPARGGDAELABn wRangeELA         3 0
011300120314     C*
011400120314     C* Verifico "valenza" parametri che pilotano il range d date elaborazione
011500100513     C                   select
011600120314     C                   when      iPARGGDAELABn >  29 and
011700120314     C                             %rem(iPARGGDAELABn :  30) = 0
011800100513     C                   eval      wTypeRangeELA = 'm'
011900120314     C                   eval      wRangeELA = %div(iPARGGDAELABn :  30)
012000120314     C                   when      iPARGGDAELABn > 364 and
012100120314     C                             %rem(iPARGGDAELABn : 365) = 0
012200100513     C                   eval      wTypeRangeELA = 'y'
012300120314     C                   eval      wRangeELA = %div(iPARGGDAELABn : 365)
012400100513     C                   endsl
012500120314     C*
012600120314     C* Se no numerico => gestisco costanti figurative previste
012700120314     C                   else
012800120314     C                   select
012900120314     C                   when      iPARGGDAELAB = 'MES'
013000120314     C                   eval      wTypeRangeELA = '1'
013100120314     C                   eval      wRangeELA = *zeros
013200120314     C                   endsl
013300120314     C                   endif
013400100513     C*
013500120314     C* Infine stabilisco i range in termini di date (DA)
013600100513     C                   select
013700120314     C                   when      wTypeRangeBAK = '1'                          * 1° mese corr
013800120314     C                   eval      wDataDa=%dec(%date(datcor))
013900120314     C                   move      '01'          wDataDa
014000100513     C                   when      wTypeRangeBAK = 'd'                          * in giorni
014100100512     C                   eval      wDataDa=%dec(%date(datcor)-
014200100513     C                                     %days(wRangeBAK))
014300100513     C                   when      wTypeRangeBAK = 'm'                          * in mesi
014400100513     C                   eval      wDataDa=%dec(%date(datcor)-
014500100513     C                                     %months(wRangeBAK))
014600100517     C                   move      '01'          wDataDa
014700100513     C                   when      wTypeRangeBAK = 'y'                          * in anni
014800100513     C                   eval      wDataDa=%dec(%date(datcor)-
014900100706     C                                     %years(wRangeBAK) +
015000100706     C                                     %years(1))
015100100517     C                   move      '0101'        wDataDa
015200100513     C                   endsl
015300100513     C*
015400120314     C* Infine stabilisco i range in termini di date (A)
015500100513     C                   select
015600120314     C                   when      wTypeRangeELA = '1'                          * 1° mese corr
015700120314     C                   eval      wDataA =%dec(%date(datcor))
015800120314     C                   move      '01'          wDataA
015900100513     C                   when      wTypeRangeELA = 'd'                          * in giorni
016000100512     C                   eval      wDataA =%dec(%date(wDataDa)+
016100100513     C                                     %days(wRangeELA))
016200100513     C                   when      wTypeRangeELA = 'm'                          * in mesi
016300100513     C                   eval      wDataA =%dec(%date(wDataDa)+
016400100513     C                                     %months(wRangeELA))
016500100517     C                   eval      wDataA =%dec(%date(wDataA)-
016600100517     C                                     %days(1))
016700100513     C                   when      wTypeRangeELA = 'y'                          * in anni
016800100513     C                   eval      wDataA =%dec(%date(wDataDa)+
016900100513     C                                     %years(wRangeELA))
017000100517     C                   eval      wDataA =%dec(%date(wDataA)-
017100100517     C                                     %days(1))
017200100513     C                   endsl
017300100513     C*
017400100513     C                   endif
017500100512     C*
017600100512     C* Verifico i parametri ricevuti in ingresso
017700100512     C                   if        iPARCLIVGD = *zeros
017800100512     C                   move      iPARCLI       iPARCLIVGD
017900100512     C                   endif
018000100512     C*
018100100512     C                   ENDSR
018200100512     C*----------------------------------------------------*
018300100512
018400100512
018500100512
018600100512     C*----------------------------------------------------*
018700100512     C     EXEELAB       BEGSR
018800100512     C*
018900100512     C* Valorizzo la DS d passaggio parametri
019000100512     C                   clear                   PARAM_OUT
019100100705     C*
019200100512     C                   eval      oDATADA     = wDataDa
019300100512     C                   eval      oDATAA      = wDataA
019400100512     C                   eval      oCODCLI     = iPARCLI
019500120323     C                   eval      oFLGDBR     = iPARFLGDBR
019600120323     C                   eval      oFLGNRS     = iPARFLGNRS
019700100512     C                   eval      oFLGUNI     = iPARFLGUNI
019800100512     C                   eval      oCODCLIVAS  = iPARCLIVGD
019900100512     C                   eval      oTIPFILE    = iPARTIPFIL
020000100705     C                   eval      oFLGOPZ     = '2'
020100100512     C*
020200100512     C* Quindi elabora
020300100512     C                   MOVEL     PARAM_OUT     KPJBU
020400120323     C                   CALL      'TIS7BSC'
020500100512     C                   PARM                    KPJBA
020600100512     C*
020700100512     C                   ENDSR
020800100512     C*----------------------------------------------------*
020900100512
021000120314
021100120314
021200120314     C*----------------------------------------------------*
021300120314     C*  CONTROLLO NUMERICITA' CAMPI
021400120314     C*----------------------------------------------------*
021500120314     C     CHKNUM        BEGSR
021600120314     C*
021700120314     C                   IF        PiDecChr = *blanks
021800120314     C                   EVAL      PiDecChr = '.'
021900120314     C                   ENDIF
022000120314     C*
022100120314     C                   callp(e)  UBISNUM_Check(PiStr
022200120314     C                                          :PiDecChr
022300120314     C                                          :PiVal
022400120314     C                                          :PiNum
022500120314     C                                          :PiInt)
022600120314     C*
022700120314     C                   IF        %error
022800120314     C                   EVAL      PiInt=*off
022900120314     C                   ENDIF
023000120314     C*
023100120314     C                   ENDSR
023200120314     C***
023300120314
023400100512
023500100512
023600090424     C*----------------------------------------------------*
023700090424     C     *INZSR        BEGSR
023800090424     C*
023900090424     C     *ENTRY        PLIST
024000090424     C                   PARM                    KPJBA
024100100512     C                   MOVEL     KPJBU         PARAM_IN
024200100512     C*
024300100512     C                   ENDSR
