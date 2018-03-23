000100080603     h dftactgrp(*no) actgrp(*caller)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700080630     D codicepc        S              1
000800080630     D filiale         S              3
000900080704     D*inizio          S              1
001000080704     D*catfoglio       S              1
001100080704     D*numfoglio       S              7
001200080704     D*Ticket          S             17
001300080704     D RTNrcdcnt       S             10I 0
001400061017     D RTNesito        S             10I 0
001500061017     D RTNopcode       S             10
001600061017     D RTNstatus       S             10I 0
001700060918
001800060918      //********************************************************************************************
001900060918      //
002000060918      // Definizione parametri procedura.
002100060918      //
002200060918      //********************************************************************************************
002300060918     C     *ENTRY        PLIST
002400080626     C                   PARM                    codicepc
002500080626     C                   PARM                    filiale
002600080704     C*                  PARM                    inizio
002700080704     C*                  PARM                    catfoglio
002800080704     C*                  PARM                    numfoglio
002900080704     C*                  PARM                    ticket
003000080704     C                   PARM                    rtnrcdcnt
003100061017     C                   PARM                    rtnesito
003200061017     C                   PARM                    rtnopcode
003300061017     C                   PARM                    rtnstatus
003400080630      *
003500080603
003600060918      /FREE
003700060918
003800060918       //*******************************************************************************************
003900060918       //
004000060918       // Main.
004100060918       //
004200060918       //*******************************************************************************************
004300061017       CLEAR rtnesito;
004400061017       CLEAR rtnopcode;
004500061017       CLEAR rtnstatus;
004600061030       *INLR = *ON;
004700080626
004800080618       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
004900080603
005000080618      /END-FREE
005100080618
005200080618     C/EXEC SQL
005300080704     C+ DECLARE FITR6 insensitive CURSOR
005400080704     C+ FOR SELECT TR6ATB, TR6WHO, TR6SEA, TR6ST1,
005500080704     C+ TR6ST2, TR6SQU, TR6FL1, TR6FL2, TR6FL3, TR6FL4, TR6FLX, TR6KSC,
005600080704     C+ TR6FLS, TR6DLP, TR6CDP, TR6NRS, TR6NCD, TR6LNA, TR6DLA, TR6TNA,
005700080704     C+ TR6TSP, TR6ZSC, TR6NC1, TR6NC2, TR6RSM, TR6PRM, TR6RSD, TR6CAD,
005800080704     C+ TR6IND, TR6LOD, TR6PRD, TR6CPD, TR6NCP, TR6NCU, TR6RST, TR6VOL,
005900080704     C+ TR6PKG, TR6FVR, TR6AAS, TR6MGS, TR6TST, TR6TSE, TR6SSE, TR6SIP,
006000080704     C+ TR6SIA, TR6AWB, TR6ISA, TR6FCA, TR6EUR, TR6XX1, TR6XX2, TR6BAI,
006100080704     C+ TR6POC, TR6NPG, TR6NFV, TR6DFV, TR6FPP, TR6PDC, TR6DPD, TR6DS1,
006200080704     C+ TR6DS2, TR6BAU FROM FITR600F
006300080704     C+ WHERE tr6fil=:filiale and tr6cpc=:codicepc
006400080618     C/END-EXEC
006500080630     C/EXEC SQL
006600080704     C+ OPEN FITR6
006700080630     C/END-EXEC
006800080618
006900080618      /FREE
007000080618
007100061017       select;
007200061017       When sqlCode = 100;
007300061017         RTNstatus = sqlCode;
007400061017         RTNopcode = 'SQLCODE';
007500061017         RTNesito  = -1;
007600061017         RETURN;
007700061017       When sqlCode < 0;
007800061017         RTNstatus = sqlCode;
007900061017         RTNopcode = 'SQLCODE';
008000061017         RTNesito  = -1;
008100060918         RETURN;
008200061017       Endsl;
008300080704
008400080704       RTNrcdcnt = sqlErrD(2);
008500080618
008600080618      /END-FREE
008700060918
008800080630     C/EXEC SQL
008900080704     C+ SET RESULT SETS for return to client CURSOR FITR6
009000080630     C/END-EXEC
009100061017
009200080618      /FREE
009300061016       IF sqlCode < 0;
009400061017         RTNstatus = sqlCode;
009500061017         RTNopcode = 'SQLCODE';
009600061017         RTNesito  = -1;
009700060918         RETURN;
009800060918       ENDIF;
009900060918
010000060918       RETURN;
010100060918
010200060918      /END-FREE
