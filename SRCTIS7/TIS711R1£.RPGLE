000100170505     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000200000313     F*
000300170802     FFIVABWWR  O    E             DISK    usropn
000400170802     FFIVADWWR  O    E             DISK    usropn
000500170802     FFIVATWWR  O    E             DISK    usropn
000600170802     FFIVAXWWR  O    E             DISK    usropn
000700170524     F
000800170802     FEDIVABWR  O    E             DISK    usropn
000900170802     FEDIVADWR  O    E             DISK    usropn
001000170802     FEDIVATWR  O    E             DISK    usropn
001100170802     FEDIVAXWR  O    E             DISK    usropn
001200170524     F
001300170802     FFNVAOWWR  O    E             DISK    usropn
001400170802     FFNVAOEWR  O    E             DISK    usropn
001500170621     F
001600170802     FWWRCE00F  O    E             DISK    usropn
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
003400170802     C                   if        not %open(FIVABWWR)
003500170802     C                   open      FIVABWWR
003600170802     C                   endif
003700170505     C                   feod      FIVABWWR
003800170802     C*
003900170607     C                   when      fileWW = 'FIVADWWR'
004000170802     C                   if        not %open(FIVADWWR)
004100170802     C                   open      FIVADWWR
004200170802     C                   endif
004300170505     C                   feod      FIVADWWR
004400170802     C*
004500170607     C                   when      fileWW = 'FIVATWWR'
004600170802     C                   if        not %open(FIVATWWR)
004700170802     C                   open      FIVATWWR
004800170802     C                   endif
004900170505     C                   feod      FIVATWWR
005000170802     C*
005100170607     C                   when      fileWW = 'FIVAXWWR'
005200170802     C                   if        not %open(FIVAXWWR)
005300170802     C                   open      FIVAXWWR
005400170802     C                   endif
005500170505     C                   feod      FIVAXWWR
005600170802     C*
005700170505     C*
005800170607     C                   when      fileWW = 'EDIVABWR'
005900170802     C                   if        not %open(EDIVABWR)
006000170802     C                   open      EDIVABWR
006100170802     C                   endif
006200170505     C                   feod      EDIVABWR
006300170802     C*
006400170607     C                   when      fileWW = 'EDIVADWR'
006500170802     C                   if        not %open(EDIVADWR)
006600170802     C                   open      EDIVADWR
006700170802     C                   endif
006800170505     C                   feod      EDIVADWR
006900170802     C*
007000170607     C                   when      fileWW = 'EDIVATWR'
007100170802     C                   if        not %open(EDIVATWR)
007200170802     C                   open      EDIVATWR
007300170802     C                   endif
007400170505     C                   feod      EDIVATWR
007500170802     C*
007600170607     C                   when      fileWW = 'EDIVAXWR'
007700170802     C                   if        not %open(EDIVAXWR)
007800170802     C                   open      EDIVAXWR
007900170802     C                   endif
008000170505     C                   feod      EDIVAXWR
008100170802     C*
008200170505     C*
008300170607     C                   when      fileWW = 'FNVAOWWR'
008400170802     C                   if        not %open(FNVAOWWR)
008500170802     C                   open      FNVAOWWR
008600170802     C                   endif
008700170505     C                   feod      FNVAOWWR
008800170802     C*
008900170607     C                   when      fileWW = 'FNVAOEWR'
009000170802     C                   if        not %open(FNVAOEWR)
009100170802     C                   open      FNVAOEWR
009200170802     C                   endif
009300170505     C                   feod      FNVAOEWR
009400170802     C*
009500170621     C*
009600170621     C                   when      fileWW = 'WWRCE00F'
009700170802     C                   if        not %open(WWRCE00F)
009800170802     C                   open      WWRCE00F
009900170802     C                   endif
010000170621     C                   feod      WWRCE00F
010100170802     C*
010200170505     C*
010300170505     C                   endsl
010400170505     C*
010500170505     C                   on-error
010600170731     C                   eval      wError = '2'
010700170505     C                   endmon
010800010601     C*
010900010601     C                   seton                                        lr
011000170505
