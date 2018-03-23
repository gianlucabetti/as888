000100061030     H DATEDIT(*YMD.)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700070111     FFIPRC00R  o  a E             DISK
000800061030
000900061017     D Profilo         S             10
001000061031     D IDdispositivo   S             35
001100061228     D Dati            S            250
001200061017     D RTNesito        S             10I 0
001300061017     D RTNopcode       S             10
001400061017     D RTNstatus       S             10I 0
001500061030     D Wdata           S              8  0
001600061030     D Wtime           S              6  0
001700060918
001800060918      //********************************************************************************************
001900060918      //
002000060918      // Definizione parametri procedura.
002100060918      //
002200060918      //********************************************************************************************
002300060918     C     *ENTRY        PLIST
002400061017     C                   PARM                    Profilo
002500061031     C                   PARM                    IDdispositivo
002600061228     C                   PARM                    Dati
002700061017     C                   PARM                    rtnesito
002800061017     C                   PARM                    rtnopcode
002900061017     C                   PARM                    rtnstatus
003000060918
003100061228      //******************************************************************* ************************
003200061030
003300061030     c                   eval      *inlr = *on
003400061030
003500061030     C                   clear                   rtnesito
003600061030     C                   clear                   rtnopcode
003700061030     C                   clear                   rtnstatus
003800061030     C                   clear                   wtime
003900061030     C                   clear                   wdata
004000070111     C                   clear                   FIPRC000
004100061030
004200070111     c                   eval      PRCtiporcd  = 'RCH'
004300070111     c                   movel     profilo       PRCPRFC
004400070111     c                   movel     IDdispositivo PRCIDDISP
004500070111     c                   movel     Dati          PRCDATI
004600061030     c                   movel     *date         Wdata
004700061030     c                   TIME                    Wtime
004800070111     c                   movel     Wdata         PRCdatora
004900070111     c                   move      Wtime         PRCdatora
005000061030
005100070111     c                   WRITE(e)  FIPRC000
005200060918
005300061030     C                   IF        %error
005400061030     c                   eval      RTNesito  = -1
005500061030     c                   eval      RTNstatus = %status
005600061030     c                   eval      RTNopcode = 'STATUS'
005700061030     c                   RETURN
005800061030     c                   ENDIF
005900061030
006000061030     c                   RETURN
