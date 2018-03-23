000100010925      *PARMS DYNUSRPRF(*OWNER)
000200000920     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     C*------------------------------------------------------------------------*
000400991214     C* MAIN LINES
000500970526     C*------------------------------------------------------------------------*
000600970526     C*
000700051026     C* Gestione x download
000800051026     C                   if        wUD = 'D'
000900011206     C*
001000051026     C/EXEC SQL
001100051026     C+ declare C1 cursor for
001200051026     C+ select count(*) from tivlt00f where
001300051026     C+ VLTKSU = :wKSU and
001400051026     C+ VLTTIP = :wTIP and
001500051026     C+ VLTSTS = '0'
001600051026     C+ for read only
001700051026     C/END-EXEC
001800051026     C
001900051026     C/EXEC SQL
002000051026     C+ open C1
002100051026     C/END-EXEC
002200051026     C
002300051026     C/EXEC SQL
002400051026     C+ Fetch C1 into :wRecZero
002500051026     C/END-EXEC
002600051026     C
002700000929     C/EXEC SQL
002800000929     C+ close C1
002900000929     C/END-EXEC
003000051026     C                   endif
003100051026     C*
003200051026     C* Gestione x upload
003300051026     C                   if        wUD = 'U'
003400051026     C*
003500051026     C/EXEC SQL
003600051026     C+ declare C2 cursor for
003700051026     C+ select count(*) from tivlr00f where
003800051026     C+ VLRKSC = :wKSU and
003900051026     C+ VLRTIP = :wTIP and
004000051026     C+ VLRFLG = '0'
004100051026     C+ for read only
004200051026     C/END-EXEC
004300051026     C
004400051026     C/EXEC SQL
004500051026     C+ open C2
004600051026     C/END-EXEC
004700051026     C
004800051026     C/EXEC SQL
004900051026     C+ Fetch C2 into :wRecZero
005000051026     C/END-EXEC
005100051026     C
005200051026     C/EXEC SQL
005300051026     C+ close C2
005400051026     C/END-EXEC
005500051026     C                   endif
005600000929     C*
005700051026     C                   SETON                                        LR
005800970526     C*------------------------------------------------------------------------*
005900970526     C* *INZSR - OPERAZIONI INIZIALI
006000970526     C*------------------------------------------------------------------------*
006100970526     C     *INZSR        BEGSR
006200970526     C*---
006300970526     C* RICEVIMENTO PARAMETRI
006400970526     C*---
006500970526     C     *ENTRY        PLIST
006600051026     C                   PARM                    wKSU              8
006700051026     C                   PARM                    wTIP              2
006800051026     C                   PARM                    wUD               1
006900051026     C                   PARM                    wRecZero          9 0
007000991214     C*
007100970526     C                   ENDSR
