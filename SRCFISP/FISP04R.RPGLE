000100061030     H DATEDIT(*YMD.)
000200080605     h dftactgrp(*no) actgrp(*caller)
000300060918      //********************************************************************************************
000400060918      //
000500060918      // Definizione variabili.
000600060918      //
000700060918      //********************************************************************************************
000800070209     FFIpsp00F  o  a E             DISK
000900061030
001000061017     D Profilo         S             10
001100061031     D IDdispositivo   S             35
001200061031     D Spunta          S            100
001300061017     D RTNesito        S             10I 0
001400061017     D RTNopcode       S             10
001500061017     D RTNstatus       S             10I 0
001600061030     D Wdata           S              8  0
001700061030     D Wtime           S              6  0
001800070116     d dataiso         s               d   datfmt(*iso)
001900060918
002000060918      //********************************************************************************************
002100060918      //
002200060918      // Definizione parametri procedura.
002300060918      //
002400060918      //********************************************************************************************
002500060918     C     *ENTRY        PLIST
002600061017     C                   PARM                    Profilo
002700061031     C                   PARM                    IDdispositivo
002800061030     C                   PARM                    Spunta
002900061017     C                   PARM                    rtnesito
003000061017     C                   PARM                    rtnopcode
003100061017     C                   PARM                    rtnstatus
003200060918
003300060918       //*******************************************************************************************
003400061030
003500061030     c                   eval      *inlr = *on
003600061030
003700061030     C                   clear                   rtnesito
003800061030     C                   clear                   rtnopcode
003900061030     C                   clear                   rtnstatus
004000061030     C                   clear                   wtime
004100061030     C                   clear                   wdata
004200070116     C                   clear                   dataiso
004300070209     C                   clear                   FIpsp000
004400061030
004500070209     c                   movel     profilo       pspPRFC
004600070209     c                   movel     IDdispositivo pspIDDISP
004700070209     c                   movel     Spunta        pspDATI
004800070116     c                   time                    dataiso
004900070116     c                   move      dataiso       wdata
005000061030     c                   TIME                    Wtime
005100070209     c                   movel     Wdata         pspdatora
005200070209     c                   move      Wtime         pspdatora
005300061030
005400070209     c                   WRITE(e)  FIpsp000
005500060918
005600061030     C                   IF        %error
005700061030     c                   eval      RTNesito  = -1
005800061030     c                   eval      RTNstatus = %status
005900061030     c                   eval      RTNopcode = 'STATUS'
006000061030     c                   RETURN
006100061030     c                   ENDIF
006200061030
006300061030     c                   RETURN
