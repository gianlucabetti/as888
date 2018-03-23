000100061030     H DATEDIT(*YMD.)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700070209     FFIpsp00F  o  a E             DISK
000800061030
000900061017     D Profilo         S             10
001000061031     D IDdispositivo   S             35
001100061031     D Spunta          S            100
001200061017     D RTNesito        S             10I 0
001300061017     D RTNopcode       S             10
001400061017     D RTNstatus       S             10I 0
001500061030     D Wdata           S              8  0
001600061030     D Wtime           S              6  0
001700070116     d dataiso         s               d   datfmt(*iso)
001800060918
001900060918      //********************************************************************************************
002000060918      //
002100060918      // Definizione parametri procedura.
002200060918      //
002300060918      //********************************************************************************************
002400060918     C     *ENTRY        PLIST
002500061017     C                   PARM                    Profilo
002600061031     C                   PARM                    IDdispositivo
002700061030     C                   PARM                    Spunta
002800061017     C                   PARM                    rtnesito
002900061017     C                   PARM                    rtnopcode
003000061017     C                   PARM                    rtnstatus
003100060918
003200060918       //*******************************************************************************************
003300061030
003400061030     c                   eval      *inlr = *on
003500061030
003600061030     C                   clear                   rtnesito
003700061030     C                   clear                   rtnopcode
003800061030     C                   clear                   rtnstatus
003900061030     C                   clear                   wtime
004000061030     C                   clear                   wdata
004100070116     C                   clear                   dataiso
004200070209     C                   clear                   FIpsp000
004300061030
004400070209     c                   movel     profilo       pspPRFC
004500070209     c                   movel     IDdispositivo pspIDDISP
004600070209     c                   movel     Spunta        pspDATI
004700070116     c                   time                    dataiso
004800070116     c                   move      dataiso       wdata
004900061030     c                   TIME                    Wtime
005000070209     c                   movel     Wdata         pspdatora
005100070209     c                   move      Wtime         pspdatora
005200061030
005300070209     c                   WRITE(e)  FIpsp000
005400060918
005500061030     C                   IF        %error
005600061030     c                   eval      RTNesito  = -1
005700061030     c                   eval      RTNstatus = %status
005800061030     c                   eval      RTNopcode = 'STATUS'
005900061030     c                   RETURN
006000061030     c                   ENDIF
006100061030
006200061030     c                   RETURN
