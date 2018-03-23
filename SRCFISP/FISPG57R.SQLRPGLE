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
007700171019      *scarico tabella ACT motivi assistenza consegne
007800080428     C/EXEC SQL
007900171019     C+ DECLARE TBEACT CURSOR FOR
008000171019     C+ SELECT SUBSTR(TBEKE1, 1, 1) as idact,
008100171019     C+ substr(TBEUNI, 1, 35) as desact,
008200171019     C+ substr(TBEUNI, 36, 1) as tipoact,
008300171019     C+ substr(TBEUNI, 37, 1) as defaultact,
008301171019     C+ substr(TBEUNI, 38, 1) as noteact
008400080428     C+ FROM TNTBE00F
008500171019     C+ WHERE tbecod = 'ACT' and tbelin = ' '
008600080428     C+ FOR READ ONLY
008700080428     C/END-EXEC
008800080428     C
008900080428     C/EXEC SQL
009000171019     C+ OPEN tbeact
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
010300171019         RTNopcode = 'TBEACT';
010400080428       Endsl;
053900131127      /END-FREE
053901171019      *scarico tabella ART motivi assistenza ritiri
053902171019     C/EXEC SQL
053903171019     C+ DECLARE TBEART CURSOR FOR
053904171019     C+ SELECT SUBSTR(TBEKE1, 1, 1) as idart,
053905171019     C+ substr(TBEUNI, 1, 35) as desart,
053906171019     C+ substr(TBEUNI, 36, 1) as tipoart,
053907171019     C+ substr(TBEUNI, 37, 1) as defaultart,
053908171019     C+ substr(TBEUNI, 38, 1) as noteart
053909171019     C+ FROM TNTBE00F
053910171019     C+ WHERE tbecod = 'ART' and tbelin = ' '
053911171019     C+ FOR READ ONLY
053912171019     C/END-EXEC
053913171019     C
053914171019     C/EXEC SQL
053915171019     C+ OPEN tbeart
053916171019     C/END-EXEC
053917171019      /FREE
053918171019
053919171019       select;
053920171019       When sqlCode < 0;
053921171019         RTNstatus = sqlCode;
053922171019         RTNopcode = 'SQLCODE';
053923171019         RTNesito  = -1;
053924171019         RETURN;
053925171019       When sqlCode = 100;
053926171019       When sqlCode > 0 and RTNopcode = ' ';
053927171019         RTNstatus = sqlCode;
053928171019         RTNopcode = 'TBEART';
053929171019       Endsl;
053930171019      /END-FREE
054000070118     C/EXEC SQL
054100070118     C+ SET RESULT SETS for return to client
054300171019     c+                 cursor tbeact,
054301171019     c+                 cursor tbeart
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
