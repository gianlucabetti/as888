000100170505     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000200000313     F*
000300170807     FFIVABWWR  O    E             DISK
000400170807     FFIVADWWR  O    E             DISK
000500170807     FFIVATWWR  O    E             DISK
000600170807     FFIVAXWWR  O    E             DISK
000700170807     F
000800170807     FEDIVABWR  O    E             DISK
000900170807     FEDIVADWR  O    E             DISK
001000170807     FEDIVATWR  O    E             DISK
001100170807     FEDIVAXWR  O    E             DISK
001200170807     F
001300170807     FFNVAOWWR  O    E             DISK
001400170807     FFNVAOEWR  O    E             DISK
001500170807     F
001600170807     FWWRCE00F  O    E             DISK
001700170524
001800170505
001900000313     D*
002000170607     D fileWW          ds            10
002100170731     D wError          s              1
002200170505
002300010330
002400170505     C*
002500170505     C     *entry        plist
002600170607     C                   parm                    fileWW
002700170505     C*
002800170731     C                   clear                   wError
002900170505     C                   monitor
003000170505     C
003100170505     C                   select
003200170505     C*
003300170607     C                   when      fileWW = 'FIVABWWR'
003400170505     C                   feod      FIVABWWR
003500170802     C*
003600170607     C                   when      fileWW = 'FIVADWWR'
003700170505     C                   feod      FIVADWWR
003800170802     C*
003900170607     C                   when      fileWW = 'FIVATWWR'
004000170505     C                   feod      FIVATWWR
004100170802     C*
004200170607     C                   when      fileWW = 'FIVAXWWR'
004300170505     C                   feod      FIVAXWWR
004400170802     C*
004500170505     C*
004600170607     C                   when      fileWW = 'EDIVABWR'
004700170505     C                   feod      EDIVABWR
004800170802     C*
004900170607     C                   when      fileWW = 'EDIVADWR'
005000170505     C                   feod      EDIVADWR
005100170802     C*
005200170607     C                   when      fileWW = 'EDIVATWR'
005300170505     C                   feod      EDIVATWR
005400170802     C*
005500170607     C                   when      fileWW = 'EDIVAXWR'
005600170505     C                   feod      EDIVAXWR
005700170802     C*
005800170505     C*
005900170607     C                   when      fileWW = 'FNVAOWWR'
006000170505     C                   feod      FNVAOWWR
006100170802     C*
006200170607     C                   when      fileWW = 'FNVAOEWR'
006300170505     C                   feod      FNVAOEWR
006400170802     C*
006500170621     C*
006600170621     C                   when      fileWW = 'WWRCE00F'
006700170621     C                   feod      WWRCE00F
006800170802     C*
006900170505     C*
007000170505     C                   endsl
007100170505     C*
007200170505     C                   on-error
007300170731     C                   eval      wError = '2'
007400170505     C                   endmon
007500010601     C*
007600010601     C                   seton                                        lr
007700170505
