000100921013     FCMDST01L  IF   E           K DISK
000200921013     D*
000300921013     D JOBDS           DS
000400921013     D  JOBNAM                 1     10
000500921013     D  JOBDST                 7      9  0
000600921013     D  JOBUSR                11     20
000700921013     D  JOBNBR                21     26
000800921013     C                   MOVE      '0'           OPZ
000900921013     C                   CALL      'XJOB'
001000921013     C                   PARM      'INVIO_*'     PARJOB           10
001100921013     C                   PARM                    OPZ               1
001200921013     C                   PARM                    JOBL             26
001300921013     C     OPZ           DOWEQ     '1'
001400921013     C                   MOVE      JOBL          JOBDS
001500921013     C     JOBDST        CHAIN     CMDST000                           21
001600000909     C                   MOVE      DSTCPN        DSTLCX
001700001108      *
001800001108     c                   eval      DSTLCR = %trim(dstlcx) + 'R'
001900001108     c                   eval      DSTLCI = %trim(dstlcx) + 'I'
002000001108     c                   eval      DSTLCF = %trim(dstlcx) + 'F'
002100001108     c                   eval      DSTLC0 = %trim(dstlcx) + '00'
002200001108      *
002300921013     C  N21              CALL      'CMZ032C4'
002400921013     C                   PARM                    JOBNAM
002500921013     C                   PARM                    JOBUSR
002600921013     C                   PARM                    JOBNBR
002700921013     C                   PARM                    DSTLCX            8
002800921013     C                   PARM                    DSTLCR            8
002900000909     C                   PARM                    DSTLCI            8
003000000909     C                   PARM                    DSTLCF            8
003100001103     C                   PARM                    DSTLC0            8
003200921013     C*
003300921013     C                   CALL      'XJOB'
003400921013     C                   PARM      'INVIO*'      PARJOB           10
003500921013     C                   PARM                    OPZ               1
003600921013     C                   PARM                    JOBL             26
003700921013     C                   END
003800921013     C                   SETON                                        LR
