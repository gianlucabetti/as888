000100080605     h dftactgrp(*no) actgrp(*caller)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700061017     D Profilo         S             10
000800061031     D IDdispositivo   S             35
000900061017     D RTNesito        S             10I 0
001000061017     D RTNopcode       S             10
001100061017     D RTNstatus       S             10I 0
001200060918
001300060918      //********************************************************************************************
001400060918      //
001500060918      // Definizione parametri procedura.
001600060918      //
001700060918      //********************************************************************************************
001800060918     C     *ENTRY        PLIST
001900061017     C                   PARM                    Profilo
002000061031     C                   PARM                    IDdispositivo
002100061017     C                   PARM                    rtnesito
002200061017     C                   PARM                    rtnopcode
002300061017     C                   PARM                    rtnstatus
002400060918      /FREE
002500060918
002600060918       //*******************************************************************************************
002700060918       //
002800060918       // Main.
002900060918       //
003000060918       //*******************************************************************************************
003100061017       CLEAR rtnesito;
003200061017       CLEAR rtnopcode;
003300061017       CLEAR rtnstatus;
003400061030       *INLR = *ON;
003500080605
003600080605       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
003700060918
003800060918      /END-FREE
003900060918     C/EXEC SQL
004000070116     C+ DECLARE TABEL CURSOR FOR
004100070220     C+ SELECT SUBSTR(TBLKEY, 1, 1), substr(TBLUNI, 1, 20),
004200070220     C+ substr(TBLUNI, 38, 1)
004300070220     C+ FROM TABEL00F
004400070215     C+ WHERE tblcod = '3E' and substr(tbluni, 37, 1) <> 'N'
004500060918     C+ FOR READ ONLY
004600060918     C/END-EXEC
004700061016     C
004800060918     C/EXEC SQL
004900060926     C+ OPEN TABEL
005000060918     C/END-EXEC
005100060918      /FREE
005200060918
005300061017       select;
005400061017       When sqlCode = 100;
005500061017         RTNstatus = sqlCode;
005600061017         RTNopcode = 'SQLCODE';
005700061017         RTNesito  = -1;
005800061017         RETURN;
005900061017       When sqlCode < 0;
006000061017         RTNstatus = sqlCode;
006100061017         RTNopcode = 'SQLCODE';
006200061017         RTNesito  = -1;
006300060918         RETURN;
006400061017       Endsl;
006500060918
006501081105       EXEC SQL
006502081105        DECLARE TBLPARAM CURSOR FOR SELECT 'A' as idparam,
006503081105        substr(a.tbeuni, 1, 242) concat substr(b.tbeuni, 1, 242) as des
006504081105        FROM TNTBE00F as a join (select * from tntbe00f where tbecod
006505081105        = 'PSP' and tbeke1 = substr(:profilo, 3, 3) and tbeke2 = '2')
006506081105        as b on a.tbecod = b.tbecod and a.tbeke1 = b.tbeke1
006507081105        where a.tbecod = 'PSP' and a.tbeke1 = substr(:profilo, 3, 3)
006508081105        and a.tbeke2 = '1' FOR READ ONLY;
006509081105
006510081105       EXEC SQL
006511081105        OPEN Tblparam;
006512081105
006513081105       select;
006514081105       When sqlCode < 0;
006515081105         RTNstatus = sqlCode;
006516081105         RTNopcode = 'SQLCODE';
006517081105         RTNesito  = -1;
006518081105         RETURN;
006519081105       When sqlCode = 100;
006520081105       When sqlCode > 0 and RTNopcode = ' ';
006521081105         RTNstatus = sqlCode;
006522081105         RTNopcode = 'TBLPARAM';
006523081105       Endsl;
006600060918      /END-FREE
006700060918     C/EXEC SQL
006800081105     C+ SET RESULT SETS for return to client
006801081105     c+                 cursor TABEL,
006802081105     c+                 cursor TBLPARAM
006900060918     C/END-EXEC
007000060918      /FREE
007100061017
007200061016       IF sqlCode < 0;
007300061017         RTNstatus = sqlCode;
007400061017         RTNopcode = 'SQLCODE';
007500061017         RTNesito  = -1;
007600060918         RETURN;
007700060918       ENDIF;
007800060918
007900060918       RETURN;
008000060918
008100060918      /END-FREE
