000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300890921     F*
000400100512     D*--------------------
000500100512     D* DS PASSAGGIO PARAMETRI IN/OUT
000600100512     D*--------------------
000700100512     D PARAM_IN        DS
000800100512     D  iPARDATADA                    8S 0                                      data DA
000900100512     D  iPARDATAA                     8S 0                                      data A
001000100512     D  iPARTIPDATA                   1A                                        S=sped C=cons
001100100512     D  iPARCLI                       7S 0                                      Cod. cliente
001200100512     D  iPARTIPCLI                    1A                                        ' '=CCM K=KSC E=both
001300130411     D  iPARFLGUNI                    1A                                        U=uni 3K S=sing T=ST
001400110304     D  iPARTIPCONS                   1A                                        S=solocons O=consOK
001500100512     D  iPARCLIVGD                    7S 0                                      Cliente out VAS
001600100512     D  iPARTIPFIL                    2A                                        Tipo file out VAS
001700100512     D  iPARGGBACK                    3S 0                                      n° gg "back"
001800100512     D  iPARGGDAELAB                  3S 0                                      n° gg "da elab"
001900110304     D  iPARTIPBOL                    2A                                        Tipo bolla
002000110307     D  iPARNRS                       2A                                        Numero serie
002100110304     D  iPARTIPDET                    1A                                        'C'=x CAP 'S'=x SPED
002200130411     D  iPARTIPAFFID                  1A                                        ' '=cli 'C'=cons
002300130411     D*
002400100512     D PARAM_OUT       DS
002500100512     D  oDATADA                       8  0
002600100512     D  oDATAA                        8  0
002700100512     D  oCODCLI                       7  0
002800100512     D  oFLGUNI                       1
002900100512     D  oFLGCONS                      1
003000100512     D  oCODCLIVAS                    7  0
003100100512     D  oTIPFILE                      2
003200100512     D  oFLGEXE                       1
003300100512     D  oTIPCLI                       1
003400110304     D  oDACMDA                       8  0
003500110304     D  oDACMA                        8  0
003600110304     D  oTIPBOL                       2
003700110307     D  oNUMSER                       2
003800110304     D  oTIPDET                       1
003900130411     D  oTIPAFFID                     1
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
007300100513     C*
007400100513     C                   movel     'd'           wTypeRangeBAK     1
007500100513     C                   z-add     iPARGGBACK    wRangeBAK         3 0
007600100513     C                   movel     'd'           wTypeRangeELA     1
007700100513     C                   z-add     iPARGGDAELAB  wRangeELA         3 0
007800100513     C*
007900100513     C* Verifico "valenza" parametri che pilotano il range d date elaborazione
008000100513     C                   select
008100100513     C                   when      iPARGGBACK >  29 and
008200100513     C                             %rem(iPARGGBACK :  30) = 0
008300100513     C                   eval      wTypeRangeBAK = 'm'
008400100513     C                   eval      wRangeBAK = %div(iPARGGBACK :  30)
008500100513     C                   when      iPARGGBACK > 364 and
008600100513     C                             %rem(iPARGGBACK : 365) = 0
008700100513     C                   eval      wTypeRangeBAK = 'y'
008800100513     C                   eval      wRangeBAK = %div(iPARGGBACK : 365)
008900100513     C                   endsl
009000100513     C*
009100100513     C                   select
009200100513     C                   when      iPARGGDAELAB >  29 and
009300100513     C                             %rem(iPARGGDAELAB :  30) = 0
009400100513     C                   eval      wTypeRangeELA = 'm'
009500100513     C                   eval      wRangeELA = %div(iPARGGDAELAB :  30)
009600100513     C                   when      iPARGGDAELAB > 364 and
009700100513     C                             %rem(iPARGGDAELAB : 365) = 0
009800100513     C                   eval      wTypeRangeELA = 'y'
009900100513     C                   eval      wRangeELA = %div(iPARGGDAELAB : 365)
010000100513     C                   endsl
010100100513     C*
010200100513     C                   select
010300100513     C                   when      wTypeRangeBAK = 'd'                          * in giorni
010400100512     C                   eval      wDataDa=%dec(%date(datcor)-
010500100513     C                                     %days(wRangeBAK))
010600100513     C                   when      wTypeRangeBAK = 'm'                          * in mesi
010700100513     C                   eval      wDataDa=%dec(%date(datcor)-
010800100513     C                                     %months(wRangeBAK))
010900100517     C                   move      '01'          wDataDa
011000100513     C                   when      wTypeRangeBAK = 'y'                          * in anni
011100100513     C                   eval      wDataDa=%dec(%date(datcor)-
011200100706     C                                     %years(wRangeBAK) +
011300100706     C                                     %years(1))
011400100517     C                   move      '0101'        wDataDa
011500100513     C                   endsl
011600100513     C*
011700100513     C                   select
011800100513     C                   when      wTypeRangeELA = 'd'                          * in giorni
011900100512     C                   eval      wDataA =%dec(%date(wDataDa)+
012000100513     C                                     %days(wRangeELA))
012100100513     C                   when      wTypeRangeELA = 'm'                          * in mesi
012200100513     C                   eval      wDataA =%dec(%date(wDataDa)+
012300100513     C                                     %months(wRangeELA))
012400100517     C                   eval      wDataA =%dec(%date(wDataA)-
012500100517     C                                     %days(1))
012600100513     C                   when      wTypeRangeELA = 'y'                          * in anni
012700100513     C                   eval      wDataA =%dec(%date(wDataDa)+
012800100513     C                                     %years(wRangeELA))
012900100517     C                   eval      wDataA =%dec(%date(wDataA)-
013000100517     C                                     %days(1))
013100100513     C                   endsl
013200100513     C*
013300100513     C                   endif
013400100512     C*
013500100512     C* Verifico i parametri ricevuti in ingresso
013600100512     C                   if        iPARCLIVGD = *zeros
013700100512     C                   move      iPARCLI       iPARCLIVGD
013800100512     C                   endif
013900100512     C*
014000100512     C                   ENDSR
014100100512     C*----------------------------------------------------*
014200100512
014300100512
014400100512
014500100512     C*----------------------------------------------------*
014600100512     C     EXEELAB       BEGSR
014700100512     C*
014800100512     C* Valorizzo la DS d passaggio parametri
014900100512     C                   clear                   PARAM_OUT
015000100512     C*
015100100512     C                   select
015200100512     C                   when      iPARTIPDATA = 'S'
015300100512     C                   eval      oDATADA     = wDataDa
015400100512     C                   eval      oDATAA      = wDataA
015500100512     C                   when      iPARTIPDATA = 'C'
015600100512     C                   eval      oDACMDA     = wDataDa
015700100512     C                   eval      oDACMA      = wDataA
015800100512     C                   endsl
015900100512     C*
016000100512     C                   eval      oCODCLI     = iPARCLI
016100100512     C                   eval      oFLGUNI     = iPARFLGUNI
016200100512     C                   eval      oFLGCONS    = iPARTIPCONS
016300100512     C                   eval      oCODCLIVAS  = iPARCLIVGD
016400100512     C                   eval      oTIPFILE    = iPARTIPFIL
016500100512     C                   eval      oTIPCLI     = iPARTIPCLI
016600110304     C                   eval      oNUMSER     = iPARNRS
016700110304     C                   eval      oTIPDET     = iPARTIPDET
016800130411     C                   eval      oTIPAFFID   = iPARTIPAFFID
016900110304     C*
017000110304     C                   if        iPARTIPBOL = *blanks                         * 99 = tutte
017100140326     C                   eval      oTIPBOL = 'F1'                               * 98 = F1 + A2
017200110304     C                   else
017300110304     C                   eval      oTIPBOL = iPARTIPBOL
017400110304     C                   endif
017500100512     C*
017600100512     C* Quindi elabora
017700100512     C                   MOVEL     PARAM_OUT     KPJBU
017800110304     C                   CALL      'TIS7FFC'
017900100512     C                   PARM                    KPJBA
018000100512     C*
018100100512     C                   ENDSR
018200100512     C*----------------------------------------------------*
018300100512
018400100512
018500100512
018600090424     C*----------------------------------------------------*
018700090424     C     *INZSR        BEGSR
018800090424     C*
018900090424     C     *ENTRY        PLIST
019000090424     C                   PARM                    KPJBA
019100100512     C                   MOVEL     KPJBU         PARAM_IN
019200100512     C*
019300100512     C                   ENDSR
