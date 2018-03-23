000100080603     h dftactgrp(*no) actgrp(*caller)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700080630     D codicepc        S              1
000800080630     D filiale         S              3
000900080702     D inizio          S              1
001000080703     D catfoglio       S              1
001100080703     D numfoglio       S              7
001200080626     D Ticket          S             17
001300080702     D RTNrcdcnt       S             10I 0
001400061017     D RTNesito        S             10I 0
001500061017     D RTNopcode       S             10
001600061017     D RTNstatus       S             10I 0
001700080702
001800060918
001900060918      //********************************************************************************************
002000060918      //
002100060918      // Definizione parametri procedura.
002200060918      //
002300060918      //********************************************************************************************
002400170307     C                   EVAL      codicepc  = '1'
002500170307     C                   EVAL      filiale   = '048'
002600170307     C                   EVAL      inizio    = 'S'
002700170307     c                   EVAL      catfoglio = '5'
002800170307     c                   EVAL      numfoglio = '0000001'
002900170307     C                   EVAL      ticket    = *blanks
003000170307     C                   EVAL      rtnrcdcnt = *zeros
003100170307     C                   EVAL      rtnesito  = *zeros
003200170307     C                   EVAL      rtnopcode = *blanks
003300170307     C                   EVAL      rtnstatus = *zeros
003400170123
003500170123     c                   call      'FISPP03R1'
003600170123     C                   PARM                    codicepc
003700170123     C                   PARM                    filiale
003800170123     C                   PARM                    inizio
003900170123     c                   parm                    catfoglio
004000170123     c                   parm                    numfoglio
004100170123     C                   PARM                    ticket
004200170123     C                   PARM                    rtnrcdcnt
004300170123     C                   PARM                    rtnesito
004400170123     C                   PARM                    rtnopcode
004500170123     C                   PARM                    rtnstatus
004600170307
004700170307     C                   EVAL      codicepc  = '1'
004800170307     C                   EVAL      filiale   = '048'
004900170307     C                   EVAL      inizio    = *blanks
005000170307     c                   EVAL      catfoglio = '5'
005100170307     c                   EVAL      numfoglio = '0000001'
005200170307     C                   EVAL      rtnrcdcnt = *zeros
005300170307     C                   EVAL      rtnesito  = *zeros
005400170307     C                   EVAL      rtnopcode = *blanks
005500170307     C                   EVAL      rtnstatus = *zeros
005600170307
005700170307     c                   call      'FISPP03R1'
005800170307     C                   PARM                    codicepc
005900170307     C                   PARM                    filiale
006000170307     C                   PARM                    inizio
006100170307     c                   parm                    catfoglio
006200170307     c                   parm                    numfoglio
006300170307     C                   PARM                    ticket
006400170307     C                   PARM                    rtnrcdcnt
006500170307     C                   PARM                    rtnesito
006600170307     C                   PARM                    rtnopcode
006700170307     C                   PARM                    rtnstatus
006800170307
006900170307     c                   eval      *inlr=*on
007000060918
