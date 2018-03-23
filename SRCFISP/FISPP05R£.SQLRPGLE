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
005000080701     C+ DECLARE FLTR8 CURSOR FOR SELECT * from fltr800f where
005100080701     C+ TR8fil=:filiale
005200080620     C+ FOR READ ONLY
005300080620     C/END-EXEC
005400080620     C
005500080620     C/EXEC SQL
005600080701     C+ OPEN FLTR8
005700080620     C/END-EXEC
005800080620
005900080620      /FREE
006000080620
006100080620       select;
006200080620       When sqlCode = 100;
006300080620         RTNstatus = sqlCode;
006400080620         RTNopcode = 'SQLCODE';
006500080620         RTNesito  = -1;
006600080620         RETURN;
006700080620       When sqlCode < 0;
006800080620         RTNstatus = sqlCode;
006900080620         RTNopcode = 'SQLCODE';
007000080620         RTNesito  = -1;
007100080620         RETURN;
007200080620       Endsl;
007300080620
007400080620      /END-FREE
007500080620
007600080620     C/EXEC SQL
007700080620     C+ SET RESULT SETS FOR RETURN TO CLIENT
007800080701     C+                 cursor fltr8
007900080620     C/END-EXEC
008000080620      /FREE
008100080620
008200080620       IF sqlCode < 0;
008300080620         RTNstatus = sqlCode;
008400080620         RTNopcode = 'SQLCODE';
008500080620         RTNesito  = -1;
008600080620         RETURN;
008700080620       ENDIF;
008800080620
008900080620       RETURN;
009000080620
009100080620      /END-FREE
