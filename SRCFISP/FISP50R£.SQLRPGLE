000001080530     h dftactgrp(*no) actgrp(*caller)
000100060918      //********************************************************************************************
000200060918      //
000300070115      // Definizione variabili.
000400060918      //
000500060918      //********************************************************************************************
000501080716     F*ctrtic   o    E             DISK    rename(wctrtic: wctr000)
000502080716     f*                                    prefix(£)
000600061017     D Profilo         S             10
000700061031     D IDdispositivo   S             35
000800080305     D Ticket          S             17
000900061017     D RTNesito        S             10I 0
001000061017     D RTNopcode       S             10
001100061017     D RTNstatus       S             10I 0
001200061219
001300080305     d wtime           s             17
001400060918      //********************************************************************************************
001500060918      //
001600060918      // Definizione parametri procedura.
001700060918      //
001800060918      //********************************************************************************************
001900060918     C     *ENTRY        PLIST
002000061017     C                   PARM                    Profilo
002100061031     C                   PARM                    IDdispositivo
002200061218     C                   PARM                    ticket
002300061017     C                   PARM                    rtnesito
002400061017     C                   PARM                    rtnopcode
002500061017     C                   PARM                    rtnstatus
002600061219
002700060918      /FREE
002800060918
002900060918       //*******************************************************************************************
003000060918       //
003100060918       // Main.
003200060918       //
003300060918       //*******************************************************************************************
003400061017       CLEAR rtnesito;
003500061017       CLEAR rtnopcode;
003600061017       CLEAR rtnstatus;
003700061026       *INLR = *ON;
003800061219
003900061219       // imposto la data e ora del lavoro
004001080409       wtime = %char(%timestamp:*iso0);
004100060918
004200060918      /END-FREE
004201080530     c* x chiudere i cursori
004202080530     C/EXEC SQL
004203080530     C+ SET OPTION CLOSQLCSR = *ENDMOD
004204080530     C/END-EXEC
004205080716     c*                  eval      £WCTRTIC = profilo + IDdispositivo +
004206080716     c*                                       ticket + wtime
004207080716     c*                  write     wctr000
004208080409      *__________________ RITIRI __________________________________
004209080409     C/EXEC SQL
004210080409     C+ DECLARE ORM CURSOR FOR SELECT
004211080409     C+ digits(pdofgs)concat(digits(pdondc)) as pdoiddist,
004212080409     C+ digits(pdopoe)concat(digits(pdonsr))concat(digits(pdonor))
004213080409     C+ concat(digits(pdonrv)) as pdoid, pdoatb, pdotdc,
004214080409     C+ pdorso, pdorsr, pdoinr,
004215080409     C+ pdolor, pdoprr, pdonar, pdorer, pdoter, pdoorr, pdonam, pdoncl,
004216080409     C+ pdopkg, pdovlm, pdobnc, pdono1, pdono2, pdorfa, pdosto,
004217080409     C+ pdoricrit, pdocodfisc, pdocommis, pdoimpprep, pdodivprep,
004218080409     C+ pdoflginfo, pdoinfosub, pdobollarc, pdonote, pdoiconasx,
004219080409     C+ digits(pdoidrow) as pdoidrow
004220080409     C+ FROM
004221080409     C+ fipdo00t WHERE digits(pdocodaut) = substr(:profilo, 3, 7) and
004222080409     C+ pdodatora >= :ticket FOR READ ONLY
004223080409     C/END-EXEC
004224080409     C
004225080409     C/EXEC SQL
004226080409     C+ OPEN orm
004227080409     C/END-EXEC
004228080409
004229080409      /FREE
004230080409       select;
004231080409       When sqlCode < 0;
004232080409         RTNstatus = sqlCode;
004233080409         RTNopcode = 'SQLCODE';
004234080409         RTNesito  = -1;
004235080409         RETURN;
004236080409       When sqlCode = 100;
004237080409       When sqlCode > 0 and RTNopcode = ' ';
004238080409         RTNstatus = sqlCode;
004239080409         RTNopcode = 'ORM';
004240080409       Endsl;
004241080409
004242080409      /END-FREE
004300061025
004302080409      *__________________ CONSEGNE  spedizioni ____________________
004400061026     C/EXEC SQL
004500070115     C+ DECLARE DISTINTE CURSOR FOR SELECT
004600070112     C+ digits(pdcfgs)concat(digits(pdcndc)) as pdciddist,
004700070112     C+ digits(pdcaas)concat(digits(pdclnp))concat(digits(pdcnrs))
004800070112     C+ concat(digits(pdcnsp)) as pdcid, pdcatb, pdctdc,
004900070112     C+ pdcrsm, pdclom, pdcprm,
005000070112     C+ pdcnzm, pdcrmn, pdcrma, pdcrmo, pdcrsd, pdcrs2, pdcind, pdclod,
005100070112     C+ pdcprd, pdcnzd, pdcgc1, pdcgc2, pdctcr, pdcdcr, pdchcr,
005200080219     C+ pdctc1, pdctc2, pdcno1, pdcno2, pdctelef, pdcnomin, pdcgga,
005300070112     C+ pdcgma, pdcgva, pdcgca, pdctic, pdccas, pdcvca, pdcift,
005400080219     C+ pdcdiv, pdcsipiva, pdcsicfis, pdcdocum, pdcncl, pdcnclori, pdcncldac,
005500080219     C+ pdcpkcdac, pdcvlcdac, pdcspesing, pdcsinoavv, pdcserviz, pdctassat,
005600080219     C+ pdcflginfo, pdcflgsx, pdcinfosub, pdcnrbandr, pdcrnomfir, pdcnote,
005700080219     C+ pdcnotaut, digits(pdcpoe)concat(digits(pdcnsr))concat(digits(pdcnor))
005800100330     C+ concat(digits(pdcnrv)) as pdcidorm, pdcidrow,
005801100330     C+ pdcragcdi, pdcnotpxa, pdcnotfir, pdcflgfir
005900061221     C+ FROM
006000070115     C+ fipdc00t WHERE digits(pdccodaut) = substr(:profilo, 3, 7) and
006100070112     C+ pdcdatora >= :ticket FOR READ ONLY
006200061026     C/END-EXEC
006300061026     C
006400061026     C/EXEC SQL
006500070112     C+ OPEN distinte
006600061026     C/END-EXEC
006700061027
006800061027      /FREE
006900061027
007000061027       select;
007600061027       When sqlCode < 0;
007700061027         RTNstatus = sqlCode;
007800070115         RTNopcode = 'SQLCODE';
007900061027         RTNesito  = -1;
008000061027         RETURN;
008001080409       When sqlCode = 100;
008002080409       When sqlCode > 0 and RTNopcode = ' ';
008003080409         RTNstatus = sqlCode;
008004080409         RTNopcode = 'DISTINTE';
008100061027       Endsl;
008200061027
008300061027      /END-FREE
008400080409      *__________________ CONSEGNE dettaglio colli ____________________
008500070119     C/EXEC SQL
008600070119     C+ DECLARE DETTDIS CURSOR FOR SELECT
008700070119     C+ digits(pddfgs)concat(digits(pddndc)) as pddiddist,
008800070119     C+ digits(pddaas)concat(digits(pddlnp))concat(digits(pddnrs))
008900080219     C+ concat(digits(pddnsp)) as pddid, pddatb,
009000070119     C+ digits(pddfls)concat(digits(pddlna))concat(digits(pddnrs))
009100080219     C+ concat(digits(pddnsc)) as pddidcollo, pddcan, pddspumag
009200070119     C+ FROM
009300070119     C+ fipdd00t WHERE digits(pddcodaut) = substr(:profilo, 3, 7) and
009400070119     C+ pdddatora >= :ticket FOR READ ONLY
009500070119     C/END-EXEC
009600070119     C
009700070119     C/EXEC SQL
009800070119     C+ OPEN dettdis
009900070119     C/END-EXEC
010000070119
010100070119      /FREE
010200070119
010300070119       select;
010900070119       When sqlCode < 0;
011000070119         RTNstatus = sqlCode;
011100070119         RTNopcode = 'SQLCODE';
011200070119         RTNesito  = -1;
011300070119         RETURN;
011301080409       When sqlCode = 100;
011302080409       When sqlCode > 0 and RTNopcode = ' ';
011303080409         RTNstatus = sqlCode;
011304080409         RTNopcode = 'DETTDIS';
011400070119       Endsl;
011500070119
011600070119      /END-FREE
011700061219     C/EXEC SQL
011800070119     C+ SET RESULT SETS for return to client
011900080409     c+                 cursor orm,
011901080409     c+                 cursor distinte,
012000070119     c+                 cursor dettdis
012100061219     C/END-EXEC
012200061219      /FREE
012300061219
012400061219       IF sqlCode < 0;
012500061219         RTNstatus = sqlCode;
012600061219         RTNopcode = 'SQLCODE';
012700061219         RTNesito  = -1;
012800061219         RETURN;
012900061219       ENDIF;
013000061219
013100061219       ticket = wtime;
013200061025
013300060918       RETURN;
013400060918
013500060918      /END-FREE
