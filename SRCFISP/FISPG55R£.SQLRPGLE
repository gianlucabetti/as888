000100080530     h dftactgrp(*no) actgrp(*caller)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
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
003500060918      /END-FREE
003600080530     c* x chiudere i cursori
003700080530     C/EXEC SQL
003800080530     C+ SET OPTION CLOSQLCSR = *ENDMOD
003900080530     C/END-EXEC
007700170522      *___ scarico tabella ORG ora stimata x filiale e tipo servizio     ___
007800080428     C/EXEC SQL
007900170522     C+ DECLARE TBEORG CURSOR FOR
008000170522     C+ SELECT SUBSTR(TBEKE1, 1, 3) as idfil, SUBSTR(TBEKE2, 1, 1) as idtsr,
008100170522     C+ substr(TBEUNI, 1, 4) as orada,
008200170522     C+ substr(TBEUNI, 5, 4) as oraa,
008300170522     C+ substr(TBEUNI, 9, 2) as range
008400080428     C+ FROM TNTBE00F
008500170522     C+ WHERE tbecod = 'ORG' and tbelin = ' '
008600080428     C+ FOR READ ONLY
008700080428     C/END-EXEC
008800080428     C
008900080428     C/EXEC SQL
009000170522     C+ OPEN tbeorg
009100080428     C/END-EXEC
009200080428      /FREE
009300080428
009400080428       select;
009500080428       When sqlCode < 0;
009600080428         RTNstatus = sqlCode;
009700080428         RTNopcode = 'SQLCODE';
009800080428         RTNesito  = -1;
009900080428         RETURN;
010000080428       When sqlCode = 100;
010100080428       When sqlCode > 0 and RTNopcode = ' ';
010200080428         RTNstatus = sqlCode;
010300170522         RTNopcode = 'TBEORG';
010400080428       Endsl;
053900131127      /END-FREE
054000070118     C/EXEC SQL
054100070118     C+ SET RESULT SETS for return to client
054300170522     c+                 cursor tbeorg
055900070118     C/END-EXEC
056000070118      /FREE
056100070118
056200070118       IF sqlCode < 0;
056300070118         RTNstatus = sqlCode;
056400070118         RTNopcode = 'SQLCODE';
056500070118         RTNesito  = -1;
056600070118         RETURN;
056700070118       ENDIF;
056800070116
056900060918       RETURN;
057000060918
057100060918      /END-FREE
