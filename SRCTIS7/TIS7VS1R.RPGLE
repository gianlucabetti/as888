000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300890921     F*
000400100512     D*--------------------
000500100512     D* DS PASSAGGIO PARAMETRI IN/OUT
000600100512     D*--------------------
000700100512     D PARAM_IN        DS
000800141009     D  iPARDATAPDA                   8  0                                      Data pagamento DA
000900141009     D  iPARDATAPA                    8  0                                      Data pagamento A
001000141009     D  iPARDATASDA                   8  0                                      Data spedizione DA
001100141009     D  iPARDATASA                    8  0                                      Data spedizione A
001200141009     D  iPARCLI                       7  0                                      Cod. cliente
001300141009     D  iPARFLGUNI                    1                                         U=unific S=singolo
001400141009     D  iPARCLIVGD                    7  0                                      Cliente out VAS
001500141009     D  iPARTIPFIL                    2                                         Tipo file out VAS
001600141009     D  iPARTIPDATA                   1                                         P=pagam S=sped
001700141009     D  iPARGGBACK                    3S 0                                      n° gg "back"
001800141009     D  iPARGGDAELAB                  3S 0                                      n° gg "da elab"
001900100512     D*
002000100512     D PARAM_OUT       DS
002100141009     D  oDATAPDA                      8  0
002200141009     D  oDATAPA                       8  0
002300141009     D  oDATASDA                      8  0
002400141009     D  oDATASA                       8  0
002500141009     D  oCODCLI                       7  0
002600141009     D  oFLGUNI                       1
002700141009     D  oFLGOPZ                       1
002800141009     D  oCODCLIVAS                    7  0
002900141009     D  oTIPFILE                      2
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
005800141009     C                   select
005900141009     C                   when      iPARDATAPDA > *zeros AND
006000141009     C                             iPARDATAPA  > *zeros
006100141009     C                   z-add     iPARDATAPDA   wDataDa
006200141009     C                   z-add     iPARDATAPA    wDataA
006300141009     C*
006400141009     C                   when      iPARDATASDA > *zeros AND
006500141009     C                             iPARDATASA  > *zeros
006600141009     C                   z-add     iPARDATASDA   wDataDa
006700141009     C                   z-add     iPARDATASA    wDataA
006800141009     C*
006900141009     C                   other
007000100513     C                   movel     'd'           wTypeRangeBAK     1
007100100513     C                   z-add     iPARGGBACK    wRangeBAK         3 0
007200100513     C                   movel     'd'           wTypeRangeELA     1
007300100513     C                   z-add     iPARGGDAELAB  wRangeELA         3 0
007400100513     C*
007500100513     C* Verifico "valenza" parametri che pilotano il range d date elaborazione
007600100513     C                   select
007700100513     C                   when      iPARGGBACK >  29 and
007800100513     C                             %rem(iPARGGBACK :  30) = 0
007900100513     C                   eval      wTypeRangeBAK = 'm'
008000100513     C                   eval      wRangeBAK = %div(iPARGGBACK :  30)
008100100513     C                   when      iPARGGBACK > 364 and
008200100513     C                             %rem(iPARGGBACK : 365) = 0
008300100513     C                   eval      wTypeRangeBAK = 'y'
008400100513     C                   eval      wRangeBAK = %div(iPARGGBACK : 365)
008500100513     C                   endsl
008600100513     C*
008700100513     C                   select
008800100513     C                   when      iPARGGDAELAB >  29 and
008900100513     C                             %rem(iPARGGDAELAB :  30) = 0
009000100513     C                   eval      wTypeRangeELA = 'm'
009100100513     C                   eval      wRangeELA = %div(iPARGGDAELAB :  30)
009200100513     C                   when      iPARGGDAELAB > 364 and
009300100513     C                             %rem(iPARGGDAELAB : 365) = 0
009400100513     C                   eval      wTypeRangeELA = 'y'
009500100513     C                   eval      wRangeELA = %div(iPARGGDAELAB : 365)
009600100513     C                   endsl
009700100513     C*
009800100513     C                   select
009900100513     C                   when      wTypeRangeBAK = 'd'                          * in giorni
010000100512     C                   eval      wDataDa=%dec(%date(datcor)-
010100100513     C                                     %days(wRangeBAK))
010200100513     C                   when      wTypeRangeBAK = 'm'                          * in mesi
010300100513     C                   eval      wDataDa=%dec(%date(datcor)-
010400100513     C                                     %months(wRangeBAK))
010500100517     C                   move      '01'          wDataDa
010600100513     C                   when      wTypeRangeBAK = 'y'                          * in anni
010700100513     C                   eval      wDataDa=%dec(%date(datcor)-
010800100706     C                                     %years(wRangeBAK) +
010900100706     C                                     %years(1))
011000100517     C                   move      '0101'        wDataDa
011100100513     C                   endsl
011200100513     C*
011300100513     C                   select
011400100513     C                   when      wTypeRangeELA = 'd'                          * in giorni
011500100512     C                   eval      wDataA =%dec(%date(wDataDa)+
011600100513     C                                     %days(wRangeELA))
011700100513     C                   when      wTypeRangeELA = 'm'                          * in mesi
011800100513     C                   eval      wDataA =%dec(%date(wDataDa)+
011900100513     C                                     %months(wRangeELA))
012000100517     C                   eval      wDataA =%dec(%date(wDataA)-
012100100517     C                                     %days(1))
012200100513     C                   when      wTypeRangeELA = 'y'                          * in anni
012300100513     C                   eval      wDataA =%dec(%date(wDataDa)+
012400100513     C                                     %years(wRangeELA))
012500100517     C                   eval      wDataA =%dec(%date(wDataA)-
012600100517     C                                     %days(1))
012700100513     C                   endsl
012800100513     C*
012900141009     C                   endsl
013000100512     C*
013100100512     C* Verifico i parametri ricevuti in ingresso
013200100512     C                   if        iPARCLIVGD = *zeros
013300100512     C                   move      iPARCLI       iPARCLIVGD
013400100512     C                   endif
013500100512     C*
013600100512     C                   ENDSR
013700100512     C*----------------------------------------------------*
013800100512
013900100512
014000100512
014100100512     C*----------------------------------------------------*
014200100512     C     EXEELAB       BEGSR
014300100512     C*
014400100512     C* Valorizzo la DS d passaggio parametri
014500100512     C                   clear                   PARAM_OUT
014600100512     C*
014700141009     C                   select
014800141009     C                   when      iPARTIPDATA = 'P'
014900141009     C                   eval      oDATAPDA    = wDataDa
015000141009     C                   eval      oDATAPA     = wDataA
015100141009     C                   when      iPARTIPDATA = 'S'
015200141009     C                   eval      oDATASDA    = wDataDa
015300141009     C                   eval      oDATASA     = wDataA
015400141009     C                   endsl
015500141009     C*
015600141009     C                   eval      oCODCLI     = iPARCLI
015700141009     C                   eval      oFLGUNI     = iPARFLGUNI
015800141009     C                   eval      oFLGOPZ     = '2'
015900141009     C                   eval      oCODCLIVAS  = iPARCLIVGD
016000141009     C                   eval      oTIPFILE    = iPARTIPFIL
016100100512     C*
016200100512     C* Quindi elabora
016300100512     C                   MOVEL     PARAM_OUT     KPJBU
016400141009     C                   CALL      'TIS7VSC'
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
