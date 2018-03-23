000100080603     h dftactgrp(*no) actgrp(*caller)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000800080630     D codicepc        S              1
000900080630     D filiale         S              3
001000080702     D inizio          S              1
001100080703     D catfoglio       S              1
001200080703     D numfoglio       S              7
001300080626     D Ticket          S             17
001400080702     D RTNrcdcnt       S             10I 0
001500061017     D RTNesito        S             10I 0
001600061017     D RTNopcode       S             10
001700061017     D RTNstatus       S             10I 0
001800080702
003400060918
003500060918      //********************************************************************************************
003600060918      //
003700060918      // Definizione parametri procedura.
003800060918      //
003900060918      //********************************************************************************************
004000060918     C     *ENTRY        PLIST
004100080626     C                   PARM                    codicepc
004200080626     C                   PARM                    filiale
004300080626     C                   PARM                    inizio
004400080702     c                   parm                    catfoglio
004500080702     c                   parm                    numfoglio
004600080701     C                   PARM                    ticket
004700080702     C                   PARM                    rtnrcdcnt
004800061017     C                   PARM                    rtnesito
004900061017     C                   PARM                    rtnopcode
005000061017     C                   PARM                    rtnstatus
005100080702
005200080702     c                   eval      *inlr=*on
005201170123
005202170123     c                   call      'FISPP03R1'
005203170123     C                   PARM                    codicepc
005204170123     C                   PARM                    filiale
005205170123     C                   PARM                    inizio
005206170123     c                   parm                    catfoglio
005207170123     c                   parm                    numfoglio
005208170123     C                   PARM                    ticket
005209170123     C                   PARM                    rtnrcdcnt
005210170123     C                   PARM                    rtnesito
005211170123     C                   PARM                    rtnopcode
005212170123     C                   PARM                    rtnstatus
005213170123
005214170123     c                   return
005300080630      *
018100060918
