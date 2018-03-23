000100061030     H DATEDIT(*YMD.)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700070111     FFIPRC00R  o  a E             DISK
000800080219     FFIPRD00R  o  a E             DISK
000900061030
001000061017     D Profilo         S             10
001100061031     D IDdispositivo   S             35
001200061228     D Dati            S            250
001300061017     D RTNesito        S             10I 0
001400061017     D RTNopcode       S             10
001500061017     D RTNstatus       S             10I 0
001600070117
001700070117     d Wdata           ds
001800070117     d  waa1                   1      4  0
001900070117     d  wmm1                   5      6  0
002000070117     d  wgg1                   7      8  0
002100070117
002200070117     d                 ds
002300070117     d wtime                   1      6  0
002400070117     d wgg                     7      8  0
002500070117     d wmm                     9     10  0
002600070117     d waa                    11     14  0
002700070117     d w0014                   1     14  0
002800060918
002900060918      //********************************************************************************************
003000060918      //
003100060918      // Definizione parametri procedura.
003200060918      //
003300060918      //********************************************************************************************
003400060918     C     *ENTRY        PLIST
003500061017     C                   PARM                    Profilo
003600061031     C                   PARM                    IDdispositivo
003700061228     C                   PARM                    Dati
003800061017     C                   PARM                    rtnesito
003900061017     C                   PARM                    rtnopcode
004000061017     C                   PARM                    rtnstatus
004100060918
004200061228      //******************************************************************* ************************
004300061030
004400061030     c                   eval      *inlr = *on
004500061030
004600061030     C                   clear                   rtnesito
004700061030     C                   clear                   rtnopcode
004800061030     C                   clear                   rtnstatus
004900070117     C                   clear                   w0014
005000070117     C                   clear                   wdata
005100070111     C                   clear                   FIPRC000
005200080219     C                   clear                   FIPRD000
005300080219
005400080219      * controllo se Consegna o Ritiro
005500080227      * per consegna scrivo fiprd00r
005600080227     c                   if        %subst(dati:40:1) = 'C'
005700080227     c                   eval      PRDtiporcd  = 'OK '
005800080227     c                   movel     profilo       PRDPRFC
005900080227     c                   movel     IDdispositivo PRDIDDISP
006000080227     c                   movel     Dati          PRDDATI
006100080227     c                   TIME                    w0014
006200080227     c                   move      waa           waa1
006300080227     c                   move      wmm           wmm1
006400080227     c                   move      wgg           wgg1
006500080227     c                   movel     wdata         PRDdatora
006600080227     c                   move      wtime         PRDdatora
006700080227
006800080227     c                   WRITE(e)  FIPRD000
006900080227     c                   else
007000080227      * per ritiro o no 'C' scrivo fiprc00r
007100070111     c                   eval      PRCtiporcd  = 'OK '
007200070111     c                   movel     profilo       PRCPRFC
007300070111     c                   movel     IDdispositivo PRCIDDISP
007400070111     c                   movel     Dati          PRCDATI
007500070117     c                   TIME                    w0014
007600070117     c                   move      waa           waa1
007700070117     c                   move      wmm           wmm1
007800070117     c                   move      wgg           wgg1
007900070117     c                   movel     wdata         PRCdatora
008000070117     c                   move      wtime         PRCdatora
008100061030
008200070111     c                   WRITE(e)  FIPRC000
008300080219     c                   endif
008400060918
008500061030     C                   IF        %error
008600061030     c                   eval      RTNesito  = -1
008700061030     c                   eval      RTNstatus = %status
008800061030     c                   eval      RTNopcode = 'STATUS'
008900061030     c                   RETURN
009000061030     c                   ENDIF
009100061030
009200061030     c                   RETURN
