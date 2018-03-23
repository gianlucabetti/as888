000100020520      *PARMS DFTACTGRP(*NO) ACTGRP(*CALLER)
000200020520      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300020520      *PARMS DYNUSRPRF(*OWNER) CLOSQLCSR(*ENDMOD)
000400020520
000500020520     D NullPtr         S               *
000600020520     D CnDizion      E DS
000700020520     D                                     BASED(NullPtr)
000800020520     D AzOrg           DS
000900020520     D                                     INZ
001000020520     D  OrgFel
001100020520     D                                     LIKE($Fil)
001200020520     D QCmd            S            256
001300020520     D QCmdLen         S             15  5
001400020520     D                                     INZ(%SIZE(QCmd))
001500020520
001600020520     C/EXEC SQL
001700020520     C+ DECLARE AZORG CURSOR FOR SELECT DISTINCT ORGFEL FROM AZORG00F
001800020520     C+ WHERE ORGFEL NOT IN (0, 46) FOR READ ONLY
001900020520     C/END-EXEC
002000020520
002100020520     C/EXEC SQL
002200020520     C+ OPEN AZORG
002300020520     C/END-EXEC
002400020520
002500020520     C                   DOU       *INLR
002600020520
002700020520     C/EXEC SQL
002800020520     C+ FETCH NEXT FROM AZORG INTO :AZORG
002900020520     C/END-EXEC
003000020520
003100020520     C                   SELECT
003200020520     C                   WHEN      SqlCod = 100
003300020520     C                   SETON                                        LR
003400020520     C                   WHEN      SqlCod = 0
003500020520     C                   EXSR      RunRmtCmd
003600020520     C                   OTHER
003700020520     C                   ENDSL
003800020520
003900020520     C                   ENDDO
004000020520
004100020520     C/EXEC SQL
004200020520     C+ CLOSE AZORG
004300020520     C/END-EXEC
004400020520
004500020520     ***********************************************************************
004600020520     **
004700020520     ** Esegue RUNRMTCMD.
004800020520     **
004900020520     ***********************************************************************
005000020520     C     RunRmtCmd     BEGSR
005100020520
005200020520     C                   EVAL      QCmd = 'RUNRMTCMD'
005300020520     C                             +
005400020520     C                             ' CMD(''CALL PGM(EDPWK/TNVRCHKRT)'')'
005500020520     C                             +
005600020520     C                             ' RMTLOCNAME(AS' + %EDITC(OrgFel:'X') + ')'
005700020520
005800020520     C                   CALL      'QCMDEXC'
005900020520     C                   PARM                    QCmd
006000020520     C                   PARM                    QCmdLen
006100020520
006200020520     C                   ENDSR
