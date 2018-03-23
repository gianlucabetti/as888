000100170320     h dftactgrp(*no) actgrp(*caller)
000200170320      //********************************************************************************************
000300170320      //
000400170320      // Definizione variabili.
000500170320      //
000600170320      //********************************************************************************************
000601170320     Fazlin03l  IF   E           k DISK
000602170320     d wlintntbe       s                   like(lintntbe)
000603170320     d Ilang           s              2a
000900170320     D RTNesito        S             10I 0
001000170320     D RTNopcode       S             10
001100170320     D RTNstatus       S             10I 0
001200170320
001300170320      //********************************************************************************************
001400170320      //
001500170320      // Definizione parametri procedura.
001600170320      //
001700170320      //********************************************************************************************
001800170320     C     *ENTRY        PLIST
002100170320     C                   PARM                    ilang
002101170320     C                   PARM                    rtnesito
002200170320     C                   PARM                    rtnopcode
002300170320     C                   PARM                    rtnstatus
002400170320      /FREE
002500170320
002600170320       //*******************************************************************************************
002700170320       //
002800170320       // Main.
002900170320       //
003000170320       //*******************************************************************************************
003100170320       CLEAR rtnesito;
003200170320       CLEAR rtnopcode;
003300170320       CLEAR rtnstatus;
003400170320       *inLR = *ON;
003401170320     c                   if        ilang=*blanks
003402170320     c                   eval      ilang='it'
003403170320     c                   endif
003404170320     c* cerco codice lingua per chain tntbe
003405170320     c                   clear                   wlintntbe
003406170320     c     ilang         chain     azlin03l
003407170320     c                   if        %found(azlin03l)
003408170320     c                   eval      wlintntbe=lintntbe
003409170320     c                   endif
003500170320
003600170320       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
003700170320
003800170320      /END-FREE
003900170320     C/EXEC SQL
004000170320     C+ DECLARE AZORG CURSOR FOR
004100170320     C+ SELECT ORGFIL, ORGDES, orgind, orgcpf, orgloc, orgpro, tbeuni
004200170320     C+ FROM AZORG00F, tntbe00f
004300170321     C+ WHERE orgfil=tbeke1 and 'UFI'=tbecod and :wlintntbe=tbelin
004301170320     C+ and   ORGFVA <> '*' and ORGFAG <> 'V'
004400170320     C+ ORDER BY ORGFIL
004500170320     C+ FOR READ ONLY
004600170320     C/END-EXEC
004700170320     C
004800170320     C/EXEC SQL
004900170320     C+ OPEN AZORG
005000170320     C/END-EXEC
005100170320      /FREE
005200170320
005300170320       select;
005400170320       When sqlCode = 100;
005500170320         RTNstatus = sqlCode;
005600170320         RTNopcode = 'SQLCODE';
005700170320         RTNesito  = -1;
005800170320         RETURN;
005900170320       When sqlCode < 0;
006000170320         RTNstatus = sqlCode;
006100170320         RTNopcode = 'SQLCODE';
006200170320         RTNesito  = -1;
006300170320         RETURN;
006400170320       Endsl;
006500170320
006600170320      /END-FREE
006700170320     C/EXEC SQL
006800170320     C+ SET RESULT SETS CURSOR AZORG
006900170320     C/END-EXEC
007000170320      /FREE
007100170320
007200170320       IF sqlCode < 0;
007300170320         RTNstatus = sqlCode;
007400170320         RTNopcode = 'SQLCODE';
007500170320         RTNesito  = -1;
007600170320         RETURN;
007700170320       ENDIF;
007800170320
007900170320       RETURN;
008000170320
008100170320      /END-FREE
