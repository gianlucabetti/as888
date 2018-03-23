000100170726     H DECEDIT('0,') DATEDIT(*DMY.)
000200170726
000300170728     D/COPY GAITRASRC/SRCPROTOPI,UBB2ORMR
000400090521
000500090520
000600170511     C*
000700170728     C                   eval      iB2ORMSUN   = '901020001'
000800170728     C                   eval      iB2ORMKSU   = '1020001'
000900170728     C                   eval      iB2ORMISV   = 'IW'
001000170728     C                   eval      iB2ORMAAS   = 2017
001100170728     C                   eval      iB2ORMSPE   = 001770729164
001200170728     C                   eval      iB2ORMCRC   = *zeros
001300170728     C                   eval      iB2ORMDPM   = 20170725
001400170728     C                   eval      iB2ORMORARD = 1530
001500170728     C                   eval      iB2ORMORARA = 1830
001600170728     C                   eval      iB2ORMRFA   = 'DTYgjHz2b_1'
001700170728     C                   eval      iB2ORMEMLR  = 'fabrizio.gurrieri@brt.it'
001800170728     C                   eval      iB2ORMSMSR  = '347 3441969'
001900170728     C                   eval      iB2ORMNOTE  = 'TEST CED => ELIMINARE'
002000170728     C                   eval      iB2ORMIP    = '10.68.0.217'
002100170509     C*
002200170728     C                   Call      'UBB2ORMR'
002300170728     C                   Parm                    iB2ORMSUN
002400170728     C                   Parm                    iB2ORMKSU
002500170728     C                   Parm                    iB2ORMISV
002600170728     C                   Parm                    iB2ORMAAS
002700170728     C                   Parm                    iB2ORMSPE
002800170728     C                   Parm                    iB2ORMCRC
002900170728     C                   Parm                    iB2ORMDPM
003000170728     C                   Parm                    iB2ORMORARD
003100170728     C                   Parm                    iB2ORMORARA
003200170728     C                   Parm                    iB2ORMRFA
003300170728     C                   Parm                    iB2ORMEMLR
003400170728     C                   Parm                    iB2ORMSMSR
003500170728     C                   Parm                    iB2ORMNOTE
003600170728     C                   Parm                    iB2ORMIP
003700170728     C                   Parm                    oB2ORMEsito
003800170728     C                   Parm                    oB2ORMNPR
003900160804     **
004000170728     C                   movel     oB2ORMNPR     wDiplay          52
004100160804     **
004200160804     C     wDiplay       DSPLY
004300000000     **
004400090520     C                   Seton                                        LR
004500000000     **
