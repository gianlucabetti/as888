000100021127      *PARMS DFTACTGRP(*NO) ACTGRP(*CALLER)
000200021127      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300021127      *PARMS DYNUSRPRF(*OWNER)
000400021127     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500131206
000600131206     D KPJBA         E DS
000700131206     D retcod          s              1
000800131206     C     *ENTRY        PLIST
000900131206     C                   PARM                    KPJBA
001000131206     C                   PARM                    retcod
001100131206
001200021127     C/EXEC SQL
001300131206     c+  update ndrca00f set rcalibero = (
001400131206     c+  SELECT cmmdes
001500131206     c+  from cnclp00f  join azcmm00f on clpage =cmmcod
001600131206     c+   where
001700131206     c+   1 = clpkut and clpkcc=dec(rcakcc)
001800131206     c+  and clpksc=dec(rcaksc) and clpage<>0)
001900131206     c+  where exists
002000131206     c+  (SELECT cmmdes
002100131206     c+  from cnclp00f  join azcmm00f on clpage =cmmcod
002200131206     c+   where
002300131206     c+   1 = clpkut and clpkcc=dec(rcakcc)
002400131206     c+  and clpksc=dec(rcaksc) and clpage<>0
002500131206     c+   and rcalibero <> cmmdes)
002600131209     c+   skip locked data
002700021127     C/END-EXEC
002800140411      *
002900140411      /free
003000140411        exec sql
003100140411
003200140411        update ndrca00f set rcaunita=
003300140411        (select  ppaunita from ndppa00f
003400140411        where ppasocieta=rcasocieta
003500140411        and ppactb='CG' and ppakcc=rcakcc  and ppaksc=rcaksc
003600140411        and ppadtpar=rcadtpar and ppanrpar=rcanrpar
003700140411        and ppaserpar=rcaserpar and ppaunita<>rcaunita)
003800140411        where exists (select * from ndppa00f
003900140411        where ppasocieta=rcasocieta
004000140411        and ppactb='CG' and ppakcc=rcakcc and ppaksc=rcaksc
004100140411        and ppadtpar=rcadtpar and ppanrpar=rcanrpar
004200140411        and ppaserpar=rcaserpar and ppaunita<>rcaunita) ;
004300140411
004400140411        exec sql
004500140411
004600140411          update ndatt00f set attunita=
004700140411          (select  ppaunita from ndppa00f
004800140411          where ppasocieta=attsocieta
004900140411          and ppactb='CG' and ppakcc=attkcc and ppaksc=attksc
005000140411          and ppadtpar=attdtpar and ppanrpar=attnrpar
005100140411          and ppaserpar=attserpar and ppaunita<>attunita)
005200140411           where exists (select * from ndppa00f
005300140411          where ppasocieta=attsocieta
005400140411          and ppactb='CG' and ppakcc=attkcc and ppaksc=attksc
005500140411          and ppadtpar=attdtpar and ppanrpar=attnrpar
005600140411          and ppaserpar=attserpar and ppaunita<>attunita) ;
005700140411
005800140411        exec sql
005900140411
006000140411          update ndimp00f set impunita=
006100140411          (select  ppaunita from ndppa00f
006200140411          where ppasocieta=impsocieta
006300140411          and ppactb='CG' and ppakcc=impkcc and ppaksc=impksc
006400140411          and ppadtpar=impdtpar and ppanrpar=impnrpar
006500140411          and ppaserpar=impserpar and ppaunita<>impunita)
006600140411           where exists (select * from ndppa00f
006700140411          where ppasocieta=impsocieta
006800140411          and ppactb='CG' and ppakcc=impkcc and ppaksc=impksc
006900140411          and ppadtpar=impdtpar and ppanrpar=impnrpar
007000140411          and ppaserpar=impserpar and ppaunita<>impunita) ;
007100140411
007200140411       /end-free
007300140411      *
007400131206     C                   eval      *inlr = *on
007500140411      *
007600131206     C                   return
