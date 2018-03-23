000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*--------------------------------------------------------------*
000300020802     D
000400120229     d Trul21ds      e ds
000500980528     I***************************************************************
000600980528     c**
000700120229      * NUOVO CONTROLLO C/ASSEGNO
000800120229     c                   clear                   trul21ds
000900120229     c                   eval      i21tla = 'L'
001000120229     c                   eval      i21cbo = '4 '
001100120229     c                   eval      i21tsp = 'C'
001200120229     c                   eval      i21lnp = 001
001300120229     c                   eval      i21nzm = '   '
001400120229     c                   eval      i21lna = 006
001500120229     c                   eval      i21nzd ='   '
001600120229     c                   eval      i21tic = '  '
001700120229     c                   eval      i21imp = 1000
001800120229     c                   eval      i21div = 'EUR'
001900120229     c                   eval      i21ute = 'EDPES  '
002000120229     c                   eval      i21pgm = 'TNVR '
002100120229     c                   call      'TRUL21R'
002200120229     c                   parm                    trul21ds
002300020322     C*
002400020322     C                   SETON                                        LR
