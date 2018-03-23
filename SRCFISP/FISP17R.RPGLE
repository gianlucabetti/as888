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
001500090713
001600090713     d wtxt            S           2048
001700090713     d TxtInOut        S           2048
001800090713     d ElencoChar      S            256
001900090713     d TipoElenco      S              1
002000090713     d CharSost        S              1
002100090713     d UpperCase       S              1
002200090713     d ChkNull         S              1    Inz('1')
002300090713     d CharNull        S              1
002400090713     d Esito           S              1
002500070117
002600070117     d Wdata           ds
002700070117     d  waa1                   1      4  0
002800070117     d  wmm1                   5      6  0
002900070117     d  wgg1                   7      8  0
003000070117
003100070117     d                 ds
003200070117     d wtime                   1      6  0
003300070117     d wgg                     7      8  0
003400070117     d wmm                     9     10  0
003500070117     d waa                    11     14  0
003600070117     d w0014                   1     14  0
003700060918
003800060918      //********************************************************************************************
003900060918      //
004000060918      // Definizione parametri procedura.
004100060918      //
004200060918      //********************************************************************************************
004300060918     C     *ENTRY        PLIST
004400061017     C                   PARM                    Profilo
004500061031     C                   PARM                    IDdispositivo
004600061228     C                   PARM                    Dati
004700061017     C                   PARM                    rtnesito
004800061017     C                   PARM                    rtnopcode
004900061017     C                   PARM                    rtnstatus
005000060918
005100061228      //******************************************************************* ************************
005200061030
005300061030     c                   eval      *inlr = *on
005400061030
005500061030     C                   clear                   rtnesito
005600061030     C                   clear                   rtnopcode
005700061030     C                   clear                   rtnstatus
005800070117     C                   clear                   w0014
005900070117     C                   clear                   wdata
006000070111     C                   clear                   FIPRC000
006100090713
006200090713     c                   eval      wtxt = dati
006300090713     c                   eval      TxtInOut = wtxt
006400090713     c                   call      'XCHKCHAR'
006500090713     c                   Parm                    TxtInOut
006600090713     c                   Parm                    ElencoChar
006700090713     c                   Parm                    TipoElenco
006800090713     c                   Parm                    CharSost
006900090713     c                   Parm                    UpperCase
007000090713     c                   Parm                    ChkNull
007100090713     c                   Parm                    CharNull
007200090713     c                   Parm                    Esito
007300090713     c                   If        Esito = '1'
007400090713     c                   Move      TxtInOut      wtxt
007500090713     c                   EndIf
007600090713     c                   eval      dati = wtxt
007700061030
007800070111     c                   eval      PRCtiporcd  = 'RES'
007900070111     c                   movel     profilo       PRCPRFC
008000070111     c                   movel     IDdispositivo PRCIDDISP
008100070111     c                   movel     Dati          PRCDATI
008200070117     c                   TIME                    w0014
008300070117     c                   move      waa           waa1
008400070117     c                   move      wmm           wmm1
008500070117     c                   move      wgg           wgg1
008600070117     c                   movel     wdata         PRCdatora
008700070117     c                   move      wtime         PRCdatora
008800061030
008900090713     c                   WRITE(e)  FIPRC000
009000060918
009100090713     C                   IF        %error
009200090713     c                   eval      RTNesito  = -1
009300090713     c                   eval      RTNstatus = %status
009400090713     c                   eval      RTNopcode = 'STATUS'
009500090713     c                   RETURN
009600090713     c                   ENDIF
009700061030
009800061030     c                   RETURN
