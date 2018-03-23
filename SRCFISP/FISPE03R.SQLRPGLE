000100090910     h datedit(*ymd/) dftactgrp(*no) actgrp(*caller)
000200090910      //********************************************************************************************
000300090910      //
000400090910      // Definizione variabili.
000500090910      //
000600090910      //********************************************************************************************
000700090910     D RTNesito        S             10I 0
000800090910     D RTNopcode       S             10
000900090910     D RTNstatus       S             10I 0
001000090910
001100090910      //********************************************************************************************
001200090910      //
001300090910      // Definizione parametri procedura.
001400090910      //
001500090910      //********************************************************************************************
001600090910     C     *ENTRY        PLIST
001700090910     C                   PARM                    rtnesito
001800090910     C                   PARM                    rtnopcode
001900090910     C                   PARM                    rtnstatus
002000090910
002100090910      /FREE
002200090910
002300090910       //*******************************************************************************************
002400090910       //
002500090910       // Main.
002600090910       //
002700090910       //*******************************************************************************************
002800090910       CLEAR rtnesito;
002900090910       CLEAR rtnopcode;
003000090910       CLEAR rtnstatus;
003100090910       *INLR = *ON;
003200090910
003300090910       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
003400090910
003500090910       // ritorno le tabelle in un unico recordset
003600090910       exec sql
003700090910         declare tabelle cursor for
003800090910
003900090910          select tbeatb,
004000090910                 case tbelin when ' ' then '1' else tbelin end as tbelin,
004100090911                 tbecod, tbeke1, tbeke2, tbeuni
004200090910          from tntbe00f
004300090910          where tbecod = 'TBA' and substr(tbeuni, 33, 1) = 'S'
004400090910
004500090910          union
004600090910
004700090910          select tbeatb,
004800090910                 case tbelin when ' ' then '1' else tbelin end as tbelin,
004900090910                 tbecod, tbeke1, tbeke2, tbeuni
005000090910          from tntbe00f
005100090910          where tbecod = 'NTW'
005200100326
005300100326          union
005400100326
005500100326          select tbeatb,
005600100326                 case tbelin when ' ' then '1' else tbelin end as tbelin,
005700100326                 tbecod, tbeke1, tbeke2, tbeuni
005800100326          from tntbe00f
005900100326          where tbecod = 'STA'
006000090910
006100090910          union
006200090910
006300090910          select tblflg as tbeatb, digits(tblkut) as tbelin, tblcod as tbecod,
006400090911                 tblkey as tbeke1, ' ' as tbeke2, tbluni as tbeuni
006500090910          from tabel00f
006600090910          where tblcod = '15' and substr(tbluni, 71, 1) = 'S'
006700090910
006800090910          union
006900090910
007000090910          select tblflg as tbeatb, digits(tblkut) as tbelin, tblcod as tbecod,
007100090910                 tblkey as tbeke1, ' ' as tbeke2, tbluni as tbeuni
007200090910          from tabel00f
007300090910          where tblcod = '1A' and substr(tbluni, 64, 1) = 'S'
007400090910
007500090910          union
007600090910
007700090910          select tblflg as tbeatb, digits(tblkut) as tbelin, tblcod as tbecod,
007800090910                 tblkey as tbeke1, ' ' as tbeke2, tbluni as tbeuni
007900090910          from tabel00f
008000090910          where tblcod = '1B' and substr(tbluni, 69, 1) = 'S'
008100090910
008200090910          union
008300090910
008400090910          select tblflg as tbeatb, digits(tblkut) as tbelin, tblcod as tbecod,
008500090910                 tblkey as tbeke1, ' ' as tbeke2, tbluni as tbeuni
008600090910          from tabel00f
008700090910          where tblcod = '1P' and substr(tbluni, 50, 1) = 'S'
008800090910
008900090910          union
009000090910
009100090910          select tblflg as tbeatb, digits(tblkut) as tbelin, tblcod as tbecod,
009200090910                 tblkey as tbeke1, ' ' as tbeke2, tbluni as tbeuni
009300090910          from tabel00f
009400090910          where tblcod = '3A' and substr(tbluni, 80, 1) = 'S'
009500090910
009600090910          union
009700090910
009800090910          select tblflg as tbeatb, digits(tblkut) as tbelin, tblcod as tbecod,
009900090910                 tblkey as tbeke1, ' ' as tbeke2, tbluni as tbeuni
010000090910          from tabel00f
010100100121          where tblcod = '5E' and substr(tbluni, 58, 1) = 'S'
010200090910
010300090910          union
010400090910
010500090910          select tblflg as tbeatb, digits(tblkut) as tbelin, tblcod as tbecod,
010600090910                 tblkey as tbeke1, ' ' as tbeke2, tbluni as tbeuni
010700090910          from tabel00f
010800090910          where tblcod = 'CV' and substr(tbluni, 10, 1) = 'S'
010900090910
011000090910          union
011100090910
011200090910          select tblflg as tbeatb, digits(tblkut) as tbelin, tblcod as tbecod,
011300090910                 tblkey as tbeke1, ' ' as tbeke2, tbluni as tbeuni
011400090910          from tabel00f
011500090922          where tblcod = 'TR' and substr(tbluni, 24, 1) = 'S'
011600090922
011700090922          order by 3;
011800090922
011900090922
012000090910
012100090910         exec sql OPEN tabelle;
012200090910
012300090910         select;
012400090910           When sqlCode = 100;
012500090910             RTNstatus = sqlCode;
012600090910             RTNopcode = 'SQLCODE';
012700090910             RTNesito  = -1;
012800090910             RETURN;
012900090910           When sqlCode < 0;
013000090910             RTNstatus = sqlCode;
013100090910             RTNopcode = 'SQLCODE';
013200090910             RTNesito  = -1;
013300090910             RETURN;
013400090910         Endsl;
013500090910
013600090910         exec sql SET RESULT SETS CURSOR tabelle;
013700090910
013800090910         IF  sqlCode < 0;
013900090910           RTNstatus = sqlCode;
014000090910           RTNopcode = 'SQLCODE';
014100090910           RTNesito  = -1;
014200090910           RETURN;
014300090910         ENDIF;
014400090910
014500090910       RETURN;
014600090910
