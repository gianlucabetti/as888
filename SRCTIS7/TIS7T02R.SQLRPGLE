000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200111230     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400030325
000500030924     C* MAIN/
000600120105     C
000700120105     C/EXEC SQL
000800120105     C+ SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED, READ WRITE
000900120105     C/END-EXEC
001000120105     C
001100120105     C/EXEC SQL
001200120105     C+ UPDATE TIVGD00F SET VGDSTO = ' '
001300120105     C+ WHERE VGDSTO = '�'
001400120105     C/END-EXEC
001500120105     C
001600120105     C*
001700120105     C* A questo punto la singola transazione � logicamente conclusa x cui sancisco il commit
001800120105     C                   commit
001900071121     C*
002000921023     C                   SETON                                        LR
002100071121
002200071121
002300991027
002400991027      /TITLE Operazioni iniziali.
002500991027     C     *inzsr        BEGSR
002600991027     C*
002700991027     C*
002800991027     C                   ENDSR
