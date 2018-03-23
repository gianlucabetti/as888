000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300890921     F*
000400100512     D*--------------------
000500100512     D* DS PASSAGGIO PARAMETRI IN/OUT
000600100512     D*--------------------
000700100512     D PARAM_IN        DS
000800100512     D  iPARDATADA                    8S 0                                      data DA
000900100512     D  iPARDATAA                     8S 0                                      data A
001000151110     D  iPARTIPDATA                   1A                                        S=sped G=giac X=giac
001100100512     D  iPARCLI                       7S 0                                      Cod. cliente
001200100512     D  iPARTIPCLI                    1A                                        ' '=CCM K=KSC E=both
001300100512     D  iPARFLGUNI                    1A                                        U=unific S=singolo
001400100705     D  iPARSTATGIAC                  1A                                        A=aper C=chiu E=all
001500100512     D  iPARCLIVGD                    7S 0                                      Cliente out VAS
001600100512     D  iPARTIPFIL                    2A                                        Tipo file out VAS
001700100512     D  iPARGGBACK                    3S 0                                      n° gg "back"
001800100512     D  iPARGGDAELAB                  3S 0                                      n° gg "da elab"
001900100512     D*
002000100512     D PARAM_OUT       DS
002100100705     D  oDATADA                       8  0
002200100705     D  oDATAA                        8  0
002300100705     D  oCODCLI                       7  0
002400100705     D  oFLGgia                       1
002500100705     D  oFLGUNI                       1
002600100705     D  oFLGOPZ                       1
002700100705     D  oCODCLIVAS                    7  0
002800100705     D  oTIPFILE                      2
002900100705     D  oFLGEXE                       1
003000100705     D  oTIPCLI                       1
003100151110     D  oTIPDATA                      1
003200100512
003300100512     D*------------
003400100512     D* ARCHITETTURA
003500100512     D*------------
003600100512     D KPJBA         E DS
003700010402
003800010402
003900100512     C*
004000100512     C* Calcola la data corrente
004100100512     C                   z-add     *zeros        datcor            8 0
004200100512     C                   eval      datcor = %dec(%date() : *ISO)
004300060127     C*
004400100512     C* Verifico e completo i parametri ricevuti
004500100512     C                   exsr      CHKPAR
004600100512     C*
004700100512     C* Qunidi richiamo il *pgm standard
004800100512     C                   exsr      EXEELAB
004900991110     C*
005000100512     C                   seton                                        LR
005100100512
005200100512
005300100512
005400100512     C*----------------------------------------------------*
005500100512     C     CHKPAR        BEGSR
005600100512     C*
005700100512     C* Se nn già ricevute nei parametri d lancio calcolo le date DA/A
005800100512     C                   z-add     *hival        wDataDa           8 0
005900100512     C                   z-add     *hival        wDataA            8 0
006000100512     C                   if        iPARDATADA > *zeros AND
006100100512     C                             iPARDATAA  > *zeros
006200100512     C                   z-add     iPARDATADA    wDataDa
006300100512     C                   z-add     iPARDATAA     wDataA
006400100512     C                   else
006500100513     C*
006600100513     C                   movel     'd'           wTypeRangeBAK     1
006700100513     C                   z-add     iPARGGBACK    wRangeBAK         3 0
006800100513     C                   movel     'd'           wTypeRangeELA     1
006900100513     C                   z-add     iPARGGDAELAB  wRangeELA         3 0
007000100513     C*
007100100513     C* Verifico "valenza" parametri che pilotano il range d date elaborazione
007200100513     C                   select
007300100513     C                   when      iPARGGBACK >  29 and
007400100513     C                             %rem(iPARGGBACK :  30) = 0
007500100513     C                   eval      wTypeRangeBAK = 'm'
007600100513     C                   eval      wRangeBAK = %div(iPARGGBACK :  30)
007700100513     C                   when      iPARGGBACK > 364 and
007800100513     C                             %rem(iPARGGBACK : 365) = 0
007900100513     C                   eval      wTypeRangeBAK = 'y'
008000100513     C                   eval      wRangeBAK = %div(iPARGGBACK : 365)
008100100513     C                   endsl
008200100513     C*
008300100513     C                   select
008400100513     C                   when      iPARGGDAELAB >  29 and
008500100513     C                             %rem(iPARGGDAELAB :  30) = 0
008600100513     C                   eval      wTypeRangeELA = 'm'
008700100513     C                   eval      wRangeELA = %div(iPARGGDAELAB :  30)
008800100513     C                   when      iPARGGDAELAB > 364 and
008900100513     C                             %rem(iPARGGDAELAB : 365) = 0
009000100513     C                   eval      wTypeRangeELA = 'y'
009100100513     C                   eval      wRangeELA = %div(iPARGGDAELAB : 365)
009200100513     C                   endsl
009300100513     C*
009400100513     C                   select
009500100513     C                   when      wTypeRangeBAK = 'd'                          * in giorni
009600100512     C                   eval      wDataDa=%dec(%date(datcor)-
009700100513     C                                     %days(wRangeBAK))
009800100513     C                   when      wTypeRangeBAK = 'm'                          * in mesi
009900100513     C                   eval      wDataDa=%dec(%date(datcor)-
010000100513     C                                     %months(wRangeBAK))
010100100517     C                   move      '01'          wDataDa
010200100513     C                   when      wTypeRangeBAK = 'y'                          * in anni
010300100513     C                   eval      wDataDa=%dec(%date(datcor)-
010400100706     C                                     %years(wRangeBAK) +
010500100706     C                                     %years(1))
010600100517     C                   move      '0101'        wDataDa
010700100513     C                   endsl
010800100513     C*
010900100513     C                   select
011000100513     C                   when      wTypeRangeELA = 'd'                          * in giorni
011100100512     C                   eval      wDataA =%dec(%date(wDataDa)+
011200100513     C                                     %days(wRangeELA))
011300100513     C                   when      wTypeRangeELA = 'm'                          * in mesi
011400100513     C                   eval      wDataA =%dec(%date(wDataDa)+
011500100513     C                                     %months(wRangeELA))
011600100517     C                   eval      wDataA =%dec(%date(wDataA)-
011700100517     C                                     %days(1))
011800100513     C                   when      wTypeRangeELA = 'y'                          * in anni
011900100513     C                   eval      wDataA =%dec(%date(wDataDa)+
012000100513     C                                     %years(wRangeELA))
012100100517     C                   eval      wDataA =%dec(%date(wDataA)-
012200100517     C                                     %days(1))
012300100513     C                   endsl
012400100513     C*
012500100513     C                   endif
012600100512     C*
012700100512     C* Verifico i parametri ricevuti in ingresso
012800100512     C                   if        iPARCLIVGD = *zeros
012900100512     C                   move      iPARCLI       iPARCLIVGD
013000100512     C                   endif
013100100512     C*
013200100512     C                   ENDSR
013300100512     C*----------------------------------------------------*
013400100512
013500100512
013600100512
013700100512     C*----------------------------------------------------*
013800100512     C     EXEELAB       BEGSR
013900100512     C*
014000100512     C* Valorizzo la DS d passaggio parametri
014100100512     C                   clear                   PARAM_OUT
014200100705     C*
014300100705     C                   select
014400100512     C                   when      iPARTIPDATA = 'S'
014500100512     C                   eval      oDATADA     = wDataDa
014600100512     C                   eval      oDATAA      = wDataA
014700151110     C                   when      iPARTIPDATA = 'X'
014800151110     C                   eval      oDATADA     = wDataDa
014900151110     C                   eval      oDATAA      = wDataA
015000100705     C                   when      iPARTIPDATA = 'G'
015100100512     C                   endsl
015200100512     C*
015300100512     C                   eval      oCODCLI     = iPARCLI
015400100512     C                   eval      oFLGUNI     = iPARFLGUNI
015500100705     C                   eval      oFLGGIA     = iPARSTATGIAC
015600100512     C                   eval      oCODCLIVAS  = iPARCLIVGD
015700100512     C                   eval      oTIPFILE    = iPARTIPFIL
015800100512     C                   eval      oTIPCLI     = iPARTIPCLI
015900100705     C                   eval      oFLGOPZ     = '2'
016000151110     C                   eval      oTIPDATA    = iPARTIPDATA
016100100512     C*
016200100512     C* Quindi elabora
016300100512     C                   MOVEL     PARAM_OUT     KPJBU
016400100705     C                   CALL      'TIS7VGC'
016500100512     C                   PARM                    KPJBA
016600100512     C*
016700100512     C                   ENDSR
016800100512     C*----------------------------------------------------*
016900100512
017000100512
017100100512
017200090424     C*----------------------------------------------------*
017300090424     C     *INZSR        BEGSR
017400090424     C*
017500090424     C     *ENTRY        PLIST
017600090424     C                   PARM                    KPJBA
017700100512     C                   MOVEL     KPJBU         PARAM_IN
017800100512     C*
017900100512     C                   ENDSR
