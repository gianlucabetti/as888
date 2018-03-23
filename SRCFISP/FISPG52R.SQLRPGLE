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
005500170718     C+ DECLARE ORM CURSOR FOR SELECT
005600080409     C+ digits(pdofgs)concat(digits(pdondc)) as pdoiddist,
005700080409     C+ digits(pdopoe)concat(digits(pdonsr))concat(digits(pdonor))
005800131127     C+ concat(digits(pdonrv)) as pdoid, pdoatb, pdotdc, pdorso, pdorsr,
005900131127     C+ pdoinr, pdolor, pdoprr, pdonar, pdorer, pdoter, pdoorr, pdonam,
006000131127     C+ pdoncl, pdopkg, pdovlm, pdobnc, pdono1, pdono2, pdorfa, pdosto,
006100131127     C+ pdoricrit, pdocodfisc, pdocommis, pdoimpprep, pdodivprep,
006200131127     C+ pdoflginfo, pdoinfosub, pdobollarc, pdonote, pdoiconasx,
006300131127     C+ digits(pdoidrow) as pdoidrow, pdogiro, pdoordina, PDORITENTA,
006400131127     C+ PDONOTAUT, PDOSEPARA, PDOVAR, PDOALERT, PDOORASTD, PDOORARIAP,
006500170718     C+ PDONOTE1, 0 as PDOORAPRORD, pdocodaut
006501170725     C+ FROM gullitst/fipdo00t WHERE digits(pdofgs) =
006600170718     C+ substr(:profilo, 1, 3) and pdodatora >= :ticket FOR READ ONLY
006700080409     C/END-EXEC
006800080409     C
006900080409     C/EXEC SQL
007000080409     C+ OPEN orm
007100080409     C/END-EXEC
007200080409
007300080409      /FREE
007400080409       select;
007500080409       When sqlCode < 0;
007600080409         RTNstatus = sqlCode;
007700080409         RTNopcode = 'SQLCODE';
007800080409         RTNesito  = -1;
007900080409         RETURN;
008000080409       When sqlCode = 100;
008100080409       When sqlCode > 0 and RTNopcode = ' ';
008200080409         RTNstatus = sqlCode;
008300080409         RTNopcode = 'ORM';
008400080409       Endsl;
008500080409
008600080409      /END-FREE
008700061025
008800080409      *__________________ CONSEGNE  spedizioni ____________________
008900061026     C/EXEC SQL
009000170718     C+ DECLARE DISTINTE CURSOR FOR SELECT
009100070112     C+ digits(pdcfgs)concat(digits(pdcndc)) as pdciddist,
009200070112     C+ digits(pdcaas)concat(digits(pdclnp))concat(digits(pdcnrs))
009300070112     C+ concat(digits(pdcnsp)) as pdcid, pdcatb, pdctdc,
009400070112     C+ pdcrsm, pdclom, pdcprm,
009500070112     C+ pdcnzm, pdcrmn, pdcrma, pdcrmo, pdcrsd, pdcrs2, pdcind, pdclod,
009600070112     C+ pdcprd, pdcnzd, pdcgc1, pdcgc2, pdctcr, pdcdcr, pdchcr,
009700080219     C+ pdctc1, pdctc2, pdcno1, pdcno2, pdctelef, pdcnomin, pdcgga,
009800070112     C+ pdcgma, pdcgva, pdcgca, pdctic, pdccas, pdcvca, pdcift,
009900080219     C+ pdcdiv, pdcsipiva, pdcsicfis, pdcdocum, pdcncl, pdcnclori, pdcncldac,
010000080219     C+ pdcpkcdac, pdcvlcdac, pdcspesing, pdcsinoavv, pdcserviz, pdctassat,
010100080219     C+ pdcflginfo, pdcflgsx, pdcinfosub, pdcnrbandr, pdcrnomfir, pdcnote,
010200080219     C+ pdcnotaut, digits(pdcpoe)concat(digits(pdcnsr))concat(digits(pdcnor))
010300100330     C+ concat(digits(pdcnrv)) as pdcidorm, pdcidrow,
010400101108     C+ pdcragcdi, pdcnotpxa, pdcnotfir, pdcflgfir, pdcldvfir, pdcgiro,
010500131127     C+ pdcordina, pdcznc, pdcric, pdcpin, pdccmd, pdcvar, pdcalert,
010600170728     C+ pdcorastd, case when pdc6ora is null
010602170718     C+ then 0    else
010603170728     C+ hour(pdc6ora) *100+ minute(pdc6ora) end as PDCORAPRORD, pdccodaut
010700170728     C+ FROM gullitst/fipdc00t left outer join fipdc60t on
010701170728     C+ pdccodaut = pdc6pdr and
010702170524     C+ case when pdcserviz in ('C', 'D') then ' ' else pdcserviz end =
010703170728     C+ pdc6tsp and pdcfgs = pdc6fgsg and pdcgiro = pdc6cgi
010800170718     C+ WHERE digits(pdcfgs) = substr(:profilo, 1, 3) and
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
013300170718     C+ DECLARE DETTDIS CURSOR FOR SELECT
013400070119     C+ digits(pddfgs)concat(digits(pddndc)) as pddiddist,
013500070119     C+ digits(pddaas)concat(digits(pddlnp))concat(digits(pddnrs))
013600080219     C+ concat(digits(pddnsp)) as pddid, pddatb,
013700070119     C+ digits(pddfls)concat(digits(pddlna))concat(digits(pddnrs))
013800170718     C+ concat(digits(pddnsc)) as pddidcollo, pddcan, pddspumag, pddcodaut
013900070119     C+ FROM
014000170725     C+ gullitst/fipdd00t WHERE digits(pddfgs) = substr(:profilo, 1, 3) and
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
