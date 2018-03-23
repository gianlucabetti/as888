000100000000     H Option( *SrcStmt )
000200160804
000300170518     D/COPY GAITRASRC/SRCPROTOPI,UBGETIFSR
000400090521
000500090520
000600170511     C*
000700170518     C                   eval      iGETIFSfile    =
000800170518     C***                             '/home/EDPFG/S0495029.TXT  '
000900170518     C***                             '/home/EDPFG/UBBRTETIP_846113_1.pdf  '
001000170518     C                                '/home/EDPFG/UBBRTETIP_580234_1.pdf  '
001100170518     C                   eval      iGETIFSbinTxt  = 'BIN'
001200170518     C                   eval      iGETIFSbase64  = 'Y'
001300170509     C*
001400170518     C                   Call      'UBGETIFSR'
001500170518     C                   Parm                    iGETIFSfile
001600170518     C                   Parm                    iGETIFSbinTxt
001700170518     C                   Parm                    iGETIFSbase64
001800170518     C                   Parm                    iGETIFSmaxLen
001900170518     C                   Parm                    oGETIFSesito
002000170518     C                   Parm                    oGETIFScodPage
002100170518     C                   Parm                    oGETIFS_CCSID
002200170518     C                   Parm                    oGETIFSlen
002300170518     C                   Parm                    oGETIFSdati4MB
002400170515     C
002500160804     **
002600170518     C                   movel     oGETIFSdati4MBwDiplay          52
002700160804     **
002800160804     C     wDiplay       DSPLY
002900000000     **
003000090520     C                   Seton                                        LR
003100000000     **
