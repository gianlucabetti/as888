000100990520      *PARMS DFTACTGRP(*YES)
000200990521      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300010928      *PARMS DYNUSRPRF(*OWNER)
000400990531      /TITLE Scansione membri origine tipo PF.
000500990524     Fqrpglesrc if   e             disk    usropn
000600990531     Fxrdbr01l  if a e           k disk
000700990524
000800990525     D srcdta          s            100
000900990531     D formatoa        ds           100
001000990531     D afunzioni              45     80
001100990524     D i               s              3  0
001200990524     D f               s              3  0
001300990526     D wfine           s              1    inz('0')
001400990521
001500990524     C                   open      qrpglesrc
001600990524
001700990520     C/EXEC SQL
001800990525     C+ DECLARE A1 CURSOR FOR SELECT translate(SRCDTA)
001900990524     C+ FROM qrpglesrc
002000990524     C+ WHERE (substr(srcdta, 7, 1) <> '*')
002100990531     C+ AND   (translate(substr(srcdta, 45, 6)) = 'FORMAT')
002200990524     C+ FOR READ ONLY
002300990520     C/END-EXEC
002400990521
002500990521     C/EXEC SQL
002600990521     C+ OPEN A1
002700990521     C/END-EXEC
002800990520
002900990526     C                   reset                   wfine
003000990525     C     wfine         doueq     *on
003100990521
003200990520     C/EXEC SQL
003300990525     C+ FETCH NEXT FROM A1 INTO :srcdta
003400990520     C/END-EXEC
003500990520
003600990520     C     sqlcod        ifne      0
003700990525     C                   eval      wfine = *on
003800990520     C                   else
003900990531     C                   exsr      scana
004000990520     C                   endif
004100990521
004200990521     C                   enddo
004300990521
004400990525     C                   close     qrpglesrc
004500990525
004600990521     C/EXEC SQL
004700990521     C+ CLOSE A1
004800990521     C/END-EXEC
004900990524
005000990525     C                   return
005100990525
005200990531      /TITLE Scansione riga 'A'.
005300990531     C     scana         begsr
005400990524
005500990531     C                   eval      formatoa = srcdta
005600990531     C     'FORMAT'      scan(e)   afunzioni     i
005700990524     C                   if        %found
005800990531     C                   clear                   qwhdrdbr
005900990531     C     '('           scan      afunzioni:i   i
006000990524     C                   add       1             i
006100990531     C     ')'           scan      afunzioni:i   f
006200990524     C                   sub       i             f
006300990531     C                   eval      whrfi = %subst(afunzioni:i:f)
006400990531     C                   exsr      wrtxrdbr
006500990524     C                   endif
006600990524
006700990524     C                   endsr
006800990531
006900990531      /TITLE Scrittura XRDBR00F.
007000990531     C     wrtxrdbr      begsr
007100990524
007200990531     C                   eval      whrefi = srcmembro
007300990531     C                   eval      whreli = srclibreria
007400990531     C                   eval      whrli  = srclibreria
007500990531     C     k04dbr01      chain     xrdbr01l
007600990524     C                   if        not %found
007700990531     C                   eval      whrtyp = 'P'
007800990531     C                   write     qwhdrdbr
007900990524     C                   endif
008000990524
008100990524     C                   endsr
008200990524      /TITLE Operazioni iniziali.
008300990524     C     *inzsr        begsr
008400990524
008500990524     C     *entry        plist
008600990524     C                   parm                    srclibreria      10
008700990524     C                   parm                    srcfile          10
008800990524     C                   parm                    srcmembro        10
008900990525     C                   parm                    srctipo          10
009000990524     C                   parm                    srctesto         50
009100990524
009200990531     C     k04dbr01      klist
009300990531     C                   kfld                    whrli
009400990531     C                   kfld                    whrfi
009500990531     C                   kfld                    whrefi
009600990531     C                   kfld                    whreli
009700990524
009800990524     C                   endsr
