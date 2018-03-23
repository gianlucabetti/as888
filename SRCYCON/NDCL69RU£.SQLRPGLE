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
002600021127      *
002601140403      /free
002602140403        exec sql
002603140403
002604140403        update ndrca00f set rcaunita=
002605140403        (select  ppaunita from ndppa00f
002606140403        where ppasocieta=rcasocieta
002607140403        and ppactb='CG' and ppakcc=rcakcc  and ppaksc=rcaksc
002610140403        and ppadtpar=rcadtpar and ppanrpar=rcanrpar
002612140403        and ppaserpar=rcaserpar and ppaunita<>rcaunita)
002613140403        where exists (select * from ndppa00f
002614140403        where ppasocieta=rcasocieta
002615140403        and ppactb='CG' and ppakcc=rcakcc and ppaksc=rcaksc
002618140403        and ppadtpar=rcadtpar and ppanrpar=rcanrpar
002620140403        and ppaserpar=rcaserpar and ppaunita<>rcaunita) ;
002621140403
002622140403        exec sql
002623140403
002626140403          update ndatt00f set attunita=
002646140403          (select  ppaunita from ndppa00f
002647140403          where ppasocieta=attsocieta
002648140403          and ppactb='CG' and ppakcc=attkcc and ppaksc=attksc
002650140403          and ppadtpar=attdtpar and ppanrpar=attnrpar
002653140403          and ppaserpar=attserpar and ppaunita<>attunita)
002654140403           where exists (select * from ndppa00f
002655140403          where ppasocieta=attsocieta
002656140403          and ppactb='CG' and ppakcc=attkcc and ppaksc=attksc
002659140403          and ppadtpar=attdtpar and ppanrpar=attnrpar
002661140403          and ppaserpar=attserpar and ppaunita<>attunita) ;
002716140403
002717140403        exec sql
002718140403
002719140403          update ndimp00f set impunita=
002720140403          (select  ppaunita from ndppa00f
002721140403          where ppasocieta=impsocieta
002722140403          and ppactb='CG' and ppakcc=impkcc and ppaksc=impksc
002723140403          and ppadtpar=impdtpar and ppanrpar=impnrpar
002724140403          and ppaserpar=impserpar and ppaunita<>impunita)
002725140403           where exists (select * from ndppa00f
002726140403          where ppasocieta=impsocieta
002727140403          and ppactb='CG' and ppakcc=impkcc and ppaksc=impksc
002728140403          and ppadtpar=impdtpar and ppanrpar=impnrpar
002729140403          and ppaserpar=impserpar and ppaunita<>impunita) ;
002730140403
002731140403       /end-free
002732140403      *
002733131206     C                   eval      *inlr = *on
002800131206     C                   return
