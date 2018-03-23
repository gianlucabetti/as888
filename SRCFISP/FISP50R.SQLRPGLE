000100080530     h dftactgrp(*no) actgrp(*caller)
000200060918      //********************************************************************************************
000300060918      //
000400070115      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700080716     F*ctrtic   o    E             DISK    rename(wctrtic: wctr000)
000800080716     f*                                    prefix(£)
000900061017     D Profilo         S             10
001000061031     D IDdispositivo   S             35
001100080305     D Ticket          S             17
001200061017     D RTNesito        S             10I 0
001300061017     D RTNopcode       S             10
001400061017     D RTNstatus       S             10I 0
001500061219
001600080305     d wtime           s             17
001700060918      //********************************************************************************************
001800060918      //
001900060918      // Definizione parametri procedura.
002000060918      //
002100060918      //********************************************************************************************
002200060918     C     *ENTRY        PLIST
002300061017     C                   PARM                    Profilo
002400061031     C                   PARM                    IDdispositivo
002500061218     C                   PARM                    ticket
002600061017     C                   PARM                    rtnesito
002700061017     C                   PARM                    rtnopcode
002800061017     C                   PARM                    rtnstatus
002900061219
003000060918      /FREE
003100060918
003200060918       //*******************************************************************************************
003300060918       //
003400060918       // Main.
003500060918       //
003600060918       //*******************************************************************************************
003700061017       CLEAR rtnesito;
003800061017       CLEAR rtnopcode;
003900061017       CLEAR rtnstatus;
004000061026       *INLR = *ON;
004100061219
004200061219       // imposto la data e ora del lavoro
004300080409       wtime = %char(%timestamp:*iso0);
004400060918
004500060918      /END-FREE
004600080530     c* x chiudere i cursori
004700080530     C/EXEC SQL
004800080530     C+ SET OPTION CLOSQLCSR = *ENDMOD
004900080530     C/END-EXEC
005000080716     c*                  eval      £WCTRTIC = profilo + IDdispositivo +
005100080716     c*                                       ticket + wtime
005200080716     c*                  write     wctr000
005300080409      *__________________ RITIRI __________________________________
005400080409     C/EXEC SQL
005500080409     C+ DECLARE ORM CURSOR FOR SELECT
005600080409     C+ digits(pdofgs)concat(digits(pdondc)) as pdoiddist,
005700080409     C+ digits(pdopoe)concat(digits(pdonsr))concat(digits(pdonor))
005800080409     C+ concat(digits(pdonrv)) as pdoid, pdoatb, pdotdc,
005900080409     C+ pdorso, pdorsr, pdoinr,
006000080409     C+ pdolor, pdoprr, pdonar, pdorer, pdoter, pdoorr, pdonam, pdoncl,
006100080409     C+ pdopkg, pdovlm, pdobnc, pdono1, pdono2, pdorfa, pdosto,
006200080409     C+ pdoricrit, pdocodfisc, pdocommis, pdoimpprep, pdodivprep,
006300080409     C+ pdoflginfo, pdoinfosub, pdobollarc, pdonote, pdoiconasx,
006400101108     C+ digits(pdoidrow) as pdoidrow, pdogiro, pdoordina
006500080409     C+ FROM
006600080409     C+ fipdo00t WHERE digits(pdocodaut) = substr(:profilo, 3, 7) and
006700080409     C+ pdodatora >= :ticket FOR READ ONLY
006800080409     C/END-EXEC
006900080409     C
007000080409     C/EXEC SQL
007100080409     C+ OPEN orm
007200080409     C/END-EXEC
007300080409
007400080409      /FREE
007500080409       select;
007600080409       When sqlCode < 0;
007700080409         RTNstatus = sqlCode;
007800080409         RTNopcode = 'SQLCODE';
007900080409         RTNesito  = -1;
008000080409         RETURN;
008100080409       When sqlCode = 100;
008200080409       When sqlCode > 0 and RTNopcode = ' ';
008300080409         RTNstatus = sqlCode;
008400080409         RTNopcode = 'ORM';
008500080409       Endsl;
008600080409
008700080409      /END-FREE
008800061025
008900080409      *__________________ CONSEGNE  spedizioni ____________________
009000061026     C/EXEC SQL
009100070115     C+ DECLARE DISTINTE CURSOR FOR SELECT
009200070112     C+ digits(pdcfgs)concat(digits(pdcndc)) as pdciddist,
009300070112     C+ digits(pdcaas)concat(digits(pdclnp))concat(digits(pdcnrs))
009400070112     C+ concat(digits(pdcnsp)) as pdcid, pdcatb, pdctdc,
009500070112     C+ pdcrsm, pdclom, pdcprm,
009600070112     C+ pdcnzm, pdcrmn, pdcrma, pdcrmo, pdcrsd, pdcrs2, pdcind, pdclod,
009700070112     C+ pdcprd, pdcnzd, pdcgc1, pdcgc2, pdctcr, pdcdcr, pdchcr,
009800080219     C+ pdctc1, pdctc2, pdcno1, pdcno2, pdctelef, pdcnomin, pdcgga,
009900070112     C+ pdcgma, pdcgva, pdcgca, pdctic, pdccas, pdcvca, pdcift,
010000080219     C+ pdcdiv, pdcsipiva, pdcsicfis, pdcdocum, pdcncl, pdcnclori, pdcncldac,
010100080219     C+ pdcpkcdac, pdcvlcdac, pdcspesing, pdcsinoavv, pdcserviz, pdctassat,
010200080219     C+ pdcflginfo, pdcflgsx, pdcinfosub, pdcnrbandr, pdcrnomfir, pdcnote,
010300080219     C+ pdcnotaut, digits(pdcpoe)concat(digits(pdcnsr))concat(digits(pdcnor))
010400100330     C+ concat(digits(pdcnrv)) as pdcidorm, pdcidrow,
010500101108     C+ pdcragcdi, pdcnotpxa, pdcnotfir, pdcflgfir, pdcldvfir, pdcgiro,
010600101108     C+ pdcordina, pdcznc
010700061221     C+ FROM
010800070115     C+ fipdc00t WHERE digits(pdccodaut) = substr(:profilo, 3, 7) and
010900070112     C+ pdcdatora >= :ticket FOR READ ONLY
011000061026     C/END-EXEC
011100061026     C
011200061026     C/EXEC SQL
011300070112     C+ OPEN distinte
011400061026     C/END-EXEC
011500061027
011600061027      /FREE
011700061027
011800061027       select;
011900061027       When sqlCode < 0;
012000061027         RTNstatus = sqlCode;
012100070115         RTNopcode = 'SQLCODE';
012200061027         RTNesito  = -1;
012300061027         RETURN;
012400080409       When sqlCode = 100;
012500080409       When sqlCode > 0 and RTNopcode = ' ';
012600080409         RTNstatus = sqlCode;
012700080409         RTNopcode = 'DISTINTE';
012800061027       Endsl;
012900061027
013000061027      /END-FREE
013100080409      *__________________ CONSEGNE dettaglio colli ____________________
013200070119     C/EXEC SQL
013300070119     C+ DECLARE DETTDIS CURSOR FOR SELECT
013400070119     C+ digits(pddfgs)concat(digits(pddndc)) as pddiddist,
013500070119     C+ digits(pddaas)concat(digits(pddlnp))concat(digits(pddnrs))
013600080219     C+ concat(digits(pddnsp)) as pddid, pddatb,
013700070119     C+ digits(pddfls)concat(digits(pddlna))concat(digits(pddnrs))
013800080219     C+ concat(digits(pddnsc)) as pddidcollo, pddcan, pddspumag
013900070119     C+ FROM
014000070119     C+ fipdd00t WHERE digits(pddcodaut) = substr(:profilo, 3, 7) and
014100070119     C+ pdddatora >= :ticket FOR READ ONLY
014200070119     C/END-EXEC
014300070119     C
014400070119     C/EXEC SQL
014500070119     C+ OPEN dettdis
014600070119     C/END-EXEC
014700070119
014800070119      /FREE
014900070119
015000070119       select;
015100070119       When sqlCode < 0;
015200070119         RTNstatus = sqlCode;
015300070119         RTNopcode = 'SQLCODE';
015400070119         RTNesito  = -1;
015500070119         RETURN;
015600080409       When sqlCode = 100;
015700080409       When sqlCode > 0 and RTNopcode = ' ';
015800080409         RTNstatus = sqlCode;
015900080409         RTNopcode = 'DETTDIS';
016000070119       Endsl;
016100070119
016200070119      /END-FREE
016300061219     C/EXEC SQL
016400070119     C+ SET RESULT SETS for return to client
016500080409     c+                 cursor orm,
016600080409     c+                 cursor distinte,
016700070119     c+                 cursor dettdis
016800061219     C/END-EXEC
016900061219      /FREE
017000061219
017100061219       IF sqlCode < 0;
017200061219         RTNstatus = sqlCode;
017300061219         RTNopcode = 'SQLCODE';
017400061219         RTNesito  = -1;
017500061219         RETURN;
017600061219       ENDIF;
017700061219
017800061219       ticket = wtime;
017900061025
018000060918       RETURN;
018100060918
018200060918      /END-FREE
