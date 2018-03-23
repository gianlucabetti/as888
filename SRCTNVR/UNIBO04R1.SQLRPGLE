000100940912     H*------------------------------------------------------------------------*
000200121102     D SBMJ            S             90    DIM(1) CTDATA PERRCD(1)
000300121026     C*
000400040304     C/EXEC SQL
000500121026     C+ delete  from fiart00f
000600040304     C/END-EXEC
000700040304     c*
000800121026     c                   call      'UNIBO04R'
000900121026     c
001000040304     C                   SETON                                        LR
001100121026**
001200121102SBMJOB CMD(CALL PGM(UNIBO04R1)) JOB(FIART) JOBQ(K018EDI) SCDDATE(04112012) SCDTIME(100000)
