000100000000     H Option( *SrcStmt )
000200090811     D*
000300100503     D iTYPE           S              1A
000400100503     D iLANG           S              3A
000500100503     D iDATA           S              8S 0
000600100503     D iNTW            S              1A
000700100503     D iLNP            S              3S 0
000800100503     D iNZD            S              3A
000900100503     D iPRD            S              2A
001000100503     D iCAD            S              9A
001100100503     D iLOD            S             35A
001200100503     D iIND            S             35A
001300100503     D iRSD            S             35A
001400100503     D iPKB            S              7S 1
001500100503     D iNCL            S              5S 0
001600100503     D iVLB            S              5S 3
001700100503     D iFFD            S              1A
001800100503     D iTSP            S              1A
001900100503     D iTC1            S              1A
002000100503     D iTC2            S              1A
002100100504     D iCBO            S              2A
002200090811     D*
002300100503     D oTFA            S              3S 0
002400100503     D oLNA            S              3S 0
002500100503     D oZNC            S              2S 0
002600100503     D oLNPD           S             20A
002700100503     D oLNAD           S             20A
002800100503     D oERR            S              1A
002900100504     D oMSG            S            256A
003000100504     D oTAG            S             80A
003100090811     D*
003200090811     D RTNesito        S             10I 0
003300100503     D RTNopcode       S             10A
003400090811     D RTNstatus       S             10I 0
003500100407     D*
003600100407     D wMSG            S             52
003700100504     D wTAG            S             52
003800090521
003900090520
004000090520     C*
004100100401     C                   Eval      iTYPE  = 'C'
004200100504     C                   Eval      iLANG  = 'IT '
004300100504     C                   Eval      iDATA  = 20100427
004400100504     C                   Eval      iNTW   = 'S'
004500100504     C                   Eval      iLNP   = 001
004600100504     C                   Eval      iNZD   = 'GR '
004700100504     C                   Eval      iPRD   = '0 '
004800100504     C                   Eval      iCAD   = '00055  '
004900100504     C                   Eval      iLOD   = 'ROMA   '
005000100330     C                   Eval      iIND   = 'VIA MERLONI, 20'
005100100330     C                   Eval      iRSD   = 'A & G CALZATURIFICIO SPA'
005200100504     C                   Eval      iNCL   = 5
005300100504     C                   Eval      iPKB   = 42
005400100330     C                   Eval      iVLB   = 0
005500090811     C                   Eval      iFFD   = ' '
005600100407     C                   Eval      iTSP   = 'C'
005700100504     C                   Eval      iTC1   = ' '
005800100504     C                   Eval      iTC2   = ' '
005900100504     C                   Eval      iCBO   = '1'
006000090520     C*
006100090811     C                   Call      'FISPE02R'
006200100330     C                   Parm                    iTYPE
006300090811     C                   Parm                    iLANG
006400090811     C                   Parm                    iDATA
006500090811     C                   Parm                    iNTW
006600090811     C                   Parm                    iLNP
006700090811     C                   Parm                    iNZD
006800090811     C                   Parm                    iPRD
006900090811     C                   Parm                    iCAD
007000090811     C                   Parm                    iLOD
007100090811     C                   Parm                    iIND
007200090811     C                   Parm                    iRSD
007300100330     C                   Parm                    iNCL
007400090811     C                   Parm                    iPKB
007500090811     C                   Parm                    iVLB
007600090811     C                   Parm                    iFFD
007700090811     C                   Parm                    iTSP
007800090811     C                   Parm                    iTC1
007900090811     C                   Parm                    iTC2
008000100504     C                   Parm                    iCBO
008100090811     C                   Parm                    oTFA
008200090811     C                   Parm                    oLNA
008300090811     C                   Parm                    oZNC
008400090811     C                   Parm                    oLNPD
008500090811     C                   Parm                    oLNAD
008600090811     C                   Parm                    oERR
008700090811     C                   Parm                    oMSG
008800100504     C                   Parm                    oTAG
008900090811     C                   Parm                    rtnesito
009000090811     C                   Parm                    rtnopcode
009100090811     C                   Parm                    rtnstatus
009200000000     **
009300100407     C                   eval      wMSG = oMSG
009400100504     C                   eval      wTAG = oTAG
009500100407     **
009600100407     C     oERR          Dsply
009700100407     C     wMSG          Dsply
009800100504     C     wTAG          Dsply
009900100407     C     rtnesito      Dsply
010000100407     C     rtnopcode     Dsply
010100100407     C     rtnstatus     Dsply
010200100407     **
010300090520     C                   Seton                                        LR
010400000000     **
