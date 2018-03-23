000100021127      *PARMS DFTACTGRP(*NO) ACTGRP(*CALLER)
000200021127      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300021127      *PARMS DYNUSRPRF(*OWNER)
000400021127     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500131206
000600131206     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
000700131206     D KPJBA         E DS
000800131206C2226D  numrec         S              9s 0
000900131206     D retcod          s              1
001000131206     C     *ENTRY        PLIST
001100131206     C                   PARM                    KPJBA
001200131206     C                   PARM                    retcod
001300131206
001400131206     c                   movel     kpjbu         xukds
001500131206     c                   movel     XUKFL1        numrec
001600131206
001700021127     C/EXEC SQL
001800131206     c+  update ndrca00f set rcalibero = (
001900131206     c+  SELECT cmmdes
002000131206     c+  from cnclp00f  join azcmm00f on clpage =cmmcod
002100131206     c+   where
002200131206     c+   1 = clpkut and clpkcc=dec(rcakcc)
002300131206     c+  and clpksc=dec(rcaksc) and clpage<>0)
002400131206     c+  where rrn(ndrca00f) = :numrec
002500021127     C/END-EXEC
002600140403      *
002700131206     C                   eval      *inlr = *on
002800131206     C                   return
