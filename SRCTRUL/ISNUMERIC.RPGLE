000100000414
000200100518     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300100518
000400100518     D*------------------
000500100518     D* LINKING A DEFINIZIONI ESTERNE
000600100518     D*------------------
000700100518     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
000800100518     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
000900100518
001000000411
001100000414     C*------------------------------------------------------------------------*
001200000414     C* MAIN LINE
001300000414     C*------------------------------------------------------------------------*
001400100518     C*
001500100518     C                   eval      PiStr    = iPiStr
001600100518     C                   eval      PiDecChr = iPiDecChr
001700100518     C                   eval      PiVal    = *zeros
001800100518     C                   eval      PiNum    = *off
001900100518     C                   eval      PiInt    = *off
002000000414     C*
002100100518     C                   if        PiDecChr = *blanks
002200100518     C                   eval      PiDecChr = '.'
002300100518     C                   endif
002400100518     C*
002500100518     C                   callp(e)  UBISNUM_Check(PiStr
002600100518     C                                          :PiDecChr
002700100518     C                                          :PiVal
002800100518     C                                          :PiNum
002900100518     C                                          :PiInt)
003000100518     C*
003100100518     C                   if        %error
003200100518     C                   eval      oPiNum = *off
003300100518     C                   eval      oPiInt = *off
003400100518     C                   else
003500100518     C                   eval      oPiVal = PiVal
003600100518     C                   eval      oPiNum = PiNum
003700100518     C                   eval      oPiInt = PiInt
003800100518     C                   endif
003900000414     C*
004000000414     C                   SETON                                        LR
004100000411
004200000414     C*------------------------------------------------------------------------*
004300000414     C* *INZSR - OPERAZIONI INIZIALI
004400000414     C*------------------------------------------------------------------------*
004500000414     C     *INZSR        BEGSR
004600100518     C*
004700000414     C     *ENTRY        PLIST
004800100518     C                   PARM                    iPiStr           30
004900100518     C                   PARM                    iPiDecChr         1
005000100518     C                   PARM                    oPiVal           30 9
005100100518     C                   PARM                    oPiInt            1
005200100518     C                   PARM                    oPiNum            1
005300011213     C*
005400000414     C                   ENDSR
