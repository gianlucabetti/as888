000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300890921     F*
000400100512     D*--------------------
000500100512     D* DS PASSAGGIO PARAMETRI IN/OUT
000600100512     D*--------------------
000700100512     D PARAM_IN        DS
000800100512     D  iPARDATADA                    8S 0                                      data DA
000900100512     D  iPARDATAA                     8S 0                                      data A
001000100512     D  iPARCLI                       7S 0                                      Cod. cliente
001100100719     D  iPARFLGIMI                    1A                                        S=solo ingombranti
001200100512     D  iPARTIPCLI                    1A                                        ' '=CCM K=KSC E=both
001300100512     D  iPARFLGUNI                    1A                                        U=unific S=singolo
001400100512     D  iPARCLIVGD                    7S 0                                      Cliente out VAS
001500100512     D  iPARTIPFIL                    2A                                        Tipo file out VAS
001600100512     D  iPARGGBACK                    3S 0                                      n° gg "back"
001700100512     D  iPARGGDAELAB                  3S 0                                      n° gg "da elab"
001800100512     D*
001900100512     D PARAM_OUT       DS
002000100705     D  oDATADA                       8  0
002100100705     D  oDATAA                        8  0
002200100705     D  oCODCLI                       7  0
002300100719     D  oFLGIMI                       1
002400100705     D  oFLGUNI                       1
002500100705     D  oFLGOPZ                       1
002600100705     D  oCODCLIVAS                    7  0
002700100705     D  oTIPFILE                      2
002800100705     D  oFLGEXE                       1
002900100705     D  oTIPCLI                       1
003000100512
003100100512     D*------------
003200100512     D* ARCHITETTURA
003300100512     D*------------
003400100512     D KPJBA         E DS
003500010402
003600010402
003700100512     C*
003800100512     C* Calcola la data corrente
003900100512     C                   z-add     *zeros        datcor            8 0
004000100512     C                   eval      datcor = %dec(%date() : *ISO)
004100060127     C*
004200100512     C* Verifico e completo i parametri ricevuti
004300100512     C                   exsr      CHKPAR
004400100512     C*
004500100512     C* Qunidi richiamo il *pgm standard
004600100512     C                   exsr      EXEELAB
004700991110     C*
004800100512     C                   seton                                        LR
004900100512
005000100512
005100100512
005200100512     C*----------------------------------------------------*
005300100512     C     CHKPAR        BEGSR
005400100512     C*
005500100512     C* Se nn già ricevute nei parametri d lancio calcolo le date DA/A
005600100512     C                   z-add     *hival        wDataDa           8 0
005700100512     C                   z-add     *hival        wDataA            8 0
005800100512     C                   if        iPARDATADA > *zeros AND
005900100512     C                             iPARDATAA  > *zeros
006000100512     C                   z-add     iPARDATADA    wDataDa
006100100512     C                   z-add     iPARDATAA     wDataA
006200100512     C                   else
006300100513     C*
006400100513     C                   movel     'd'           wTypeRangeBAK     1
006500100513     C                   z-add     iPARGGBACK    wRangeBAK         3 0
006600100513     C                   movel     'd'           wTypeRangeELA     1
006700100513     C                   z-add     iPARGGDAELAB  wRangeELA         3 0
006800100513     C*
006900100513     C* Verifico "valenza" parametri che pilotano il range d date elaborazione
007000100513     C                   select
007100100513     C                   when      iPARGGBACK >  29 and
007200100513     C                             %rem(iPARGGBACK :  30) = 0
007300100513     C                   eval      wTypeRangeBAK = 'm'
007400100513     C                   eval      wRangeBAK = %div(iPARGGBACK :  30)
007500100513     C                   when      iPARGGBACK > 364 and
007600100513     C                             %rem(iPARGGBACK : 365) = 0
007700100513     C                   eval      wTypeRangeBAK = 'y'
007800100513     C                   eval      wRangeBAK = %div(iPARGGBACK : 365)
007900100513     C                   endsl
008000100513     C*
008100100513     C                   select
008200100513     C                   when      iPARGGDAELAB >  29 and
008300100513     C                             %rem(iPARGGDAELAB :  30) = 0
008400100513     C                   eval      wTypeRangeELA = 'm'
008500100513     C                   eval      wRangeELA = %div(iPARGGDAELAB :  30)
008600100513     C                   when      iPARGGDAELAB > 364 and
008700100513     C                             %rem(iPARGGDAELAB : 365) = 0
008800100513     C                   eval      wTypeRangeELA = 'y'
008900100513     C                   eval      wRangeELA = %div(iPARGGDAELAB : 365)
009000100513     C                   endsl
009100100513     C*
009200100513     C                   select
009300100513     C                   when      wTypeRangeBAK = 'd'                          * in giorni
009400100512     C                   eval      wDataDa=%dec(%date(datcor)-
009500100513     C                                     %days(wRangeBAK))
009600100513     C                   when      wTypeRangeBAK = 'm'                          * in mesi
009700100513     C                   eval      wDataDa=%dec(%date(datcor)-
009800100513     C                                     %months(wRangeBAK))
009900100517     C                   move      '01'          wDataDa
010000100513     C                   when      wTypeRangeBAK = 'y'                          * in anni
010100100513     C                   eval      wDataDa=%dec(%date(datcor)-
010200100706     C                                     %years(wRangeBAK) +
010300100706     C                                     %years(1))
010400100517     C                   move      '0101'        wDataDa
010500100513     C                   endsl
010600100513     C*
010700100513     C                   select
010800100513     C                   when      wTypeRangeELA = 'd'                          * in giorni
010900100512     C                   eval      wDataA =%dec(%date(wDataDa)+
011000100513     C                                     %days(wRangeELA))
011100100513     C                   when      wTypeRangeELA = 'm'                          * in mesi
011200100513     C                   eval      wDataA =%dec(%date(wDataDa)+
011300100513     C                                     %months(wRangeELA))
011400100517     C                   eval      wDataA =%dec(%date(wDataA)-
011500100517     C                                     %days(1))
011600100513     C                   when      wTypeRangeELA = 'y'                          * in anni
011700100513     C                   eval      wDataA =%dec(%date(wDataDa)+
011800100513     C                                     %years(wRangeELA))
011900100517     C                   eval      wDataA =%dec(%date(wDataA)-
012000100517     C                                     %days(1))
012100100513     C                   endsl
012200100513     C*
012300100513     C                   endif
012400100512     C*
012500100512     C* Verifico i parametri ricevuti in ingresso
012600100512     C                   if        iPARCLIVGD = *zeros
012700100512     C                   move      iPARCLI       iPARCLIVGD
012800100512     C                   endif
012900100512     C*
013000100512     C                   ENDSR
013100100512     C*----------------------------------------------------*
013200100512
013300100512
013400100512
013500100512     C*----------------------------------------------------*
013600100512     C     EXEELAB       BEGSR
013700100512     C*
013800100512     C* Valorizzo la DS d passaggio parametri
013900100512     C                   clear                   PARAM_OUT
014000100705     C*
014100100512     C                   eval      oDATADA     = wDataDa
014200100512     C                   eval      oDATAA      = wDataA
014300100512     C                   eval      oCODCLI     = iPARCLI
014400100719     C                   eval      oFLGIMI     = iPARFLGIMI
014500100512     C                   eval      oFLGUNI     = iPARFLGUNI
014600100512     C                   eval      oCODCLIVAS  = iPARCLIVGD
014700100512     C                   eval      oTIPFILE    = iPARTIPFIL
014800100512     C                   eval      oTIPCLI     = iPARTIPCLI
014900100705     C                   eval      oFLGOPZ     = '2'
015000100512     C*
015100100512     C* Quindi elabora
015200100512     C                   MOVEL     PARAM_OUT     KPJBU
015300100719     C                   CALL      'TIS7VBC'
015400100512     C                   PARM                    KPJBA
015500100512     C*
015600100512     C                   ENDSR
015700100512     C*----------------------------------------------------*
015800100512
015900100512
016000100512
016100090424     C*----------------------------------------------------*
016200090424     C     *INZSR        BEGSR
016300090424     C*
016400090424     C     *ENTRY        PLIST
016500090424     C                   PARM                    KPJBA
016600100512     C                   MOVEL     KPJBU         PARAM_IN
016700100512     C*
016800100512     C                   ENDSR
