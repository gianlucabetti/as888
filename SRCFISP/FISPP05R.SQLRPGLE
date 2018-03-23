000100080620     h dftactgrp(*no) actgrp(*caller)
000200080620      //********************************************************************************************
000300080620      //
000400080620      // Definizione variabili.
000500080620      //
000600080620      //********************************************************************************************
000700080620     D Filiale         S              3
000800080620     D RTNesito        S             10I 0
000900080620     D RTNopcode       S             10
001000080620     D RTNstatus       S             10I 0
001100080702      *   C O S T A N T I
001200080702     d digitn          c                   const('1234567890')
001300080620
001400080620      //********************************************************************************************
001500080620      //
001600080620      // Definizione parametri procedura.
001700080620      //
001800080620      //********************************************************************************************
001900080620     C     *ENTRY        PLIST
002000080620     C                   PARM                    Filiale
002100080620     C                   PARM                    rtnesito
002200080620     C                   PARM                    rtnopcode
002300080620     C                   PARM                    rtnstatus
002400080620     c*
002500080620      /FREE
002600080620
002700080620       //*******************************************************************************************
002800080620       //
002900080620       // Main.
003000080620       //
003100080620       //*******************************************************************************************
003200080620       CLEAR rtnesito;
003300080620       CLEAR rtnopcode;
003400080620       CLEAR rtnstatus;
003500080620       *INLR = *ON;
003600080620
003700080620       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
003800080620
003900080620      /END-FREE
004000080620
004100080702     c                   IF        Filiale = *blanks or
004200080702     c                             %check(digitn:filiale)>0
004300080620     c                   eval      RTNstatus = 99
004400080620     c                   eval      RTNopcode = 'ERRORE '
004500080620     c                   eval      RTNesito  = -1
004600080620     c                   return
004700080620     c                   ENDIF
004800080620     c*
004900080620     C/EXEC SQL
005000160705     C+ DECLARE FLTR8 CURSOR FOR SELECT
005100160705     C+   TR8fil, TR8tip, TR8orl, TR8brl, TR8pnc, TR8bnc,
005200160705     C+   TR8psp, TR8bsp, TR8ncl, TR8bcl,
005300160705     C+   TR8m01, TR8b01,
005400160705     C+   TR8m02, TR8b02,
005500160705     C+   TR8m03, TR8b03,
005600160705     C+   TR8m04, TR8b04,
005700160705     C+   TR8m05, TR8b05,
005800160705     C+   TR8m06, TR8b06,
005900160705     C+   TR8m07, TR8b07,
006000160705     C+   TR8m08, TR8b08,
006100160705     C+   TR8m09, TR8b09,
006200160705     C+   TR8m10, TR8b10
006300160705     C+ from fltr800f where
006400080701     C+ TR8fil=:filiale
006500080620     C+ FOR READ ONLY
006600080620     C/END-EXEC
006700080620     C
006800080620     C/EXEC SQL
006900080701     C+ OPEN FLTR8
007000080620     C/END-EXEC
007100080620
007200080620      /FREE
007300080620
007400080620       select;
007500080620       When sqlCode = 100;
007600080620         RTNstatus = sqlCode;
007700080620         RTNopcode = 'SQLCODE';
007800080620         RTNesito  = -1;
007900080620         RETURN;
008000080620       When sqlCode < 0;
008100080620         RTNstatus = sqlCode;
008200080620         RTNopcode = 'SQLCODE';
008300080620         RTNesito  = -1;
008400080620         RETURN;
008500080620       Endsl;
008600080620
008700080620      /END-FREE
008800080620
008900080620     C/EXEC SQL
009000080620     C+ SET RESULT SETS FOR RETURN TO CLIENT
009100080701     C+                 cursor fltr8
009200080620     C/END-EXEC
009300080620      /FREE
009400080620
009500080620       IF sqlCode < 0;
009600080620         RTNstatus = sqlCode;
009700080620         RTNopcode = 'SQLCODE';
009800080620         RTNesito  = -1;
009900080620         RETURN;
010000080620       ENDIF;
010100080620
010200080620       RETURN;
010300080620
010400080620      /END-FREE
