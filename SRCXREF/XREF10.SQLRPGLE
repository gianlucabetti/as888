000100990520      *PARMS DFTACTGRP(*YES)
000200990521      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300010928      *PARMS DYNUSRPRF(*OWNER)
000400000505      /TITLE Scansione membri origine tipo RPGIII.
000500990525     Fqrpglesrc if   e             disk    usropn
000600990524     Fxrpgm01l  if a e           k disk
000700990524
000800990525     D srcdta          s            100
000900990524     D formato         s              1
001000990525     D formatoI        ds           100
001100990525     D Dnome                   7     12
001200990525     D De                     17     17
001300990525     D Dds                    19     20
001400990525     D Dnomeext               21     30
001500000505     D i               S              5U 0
001600000505     D f               S              5U 0
001700990526     D wfine           s              1    inz('0')
001800000505     D X15Lib          S             10
001900000505     D X15Fil          S             10
002000000505     D X15Mbr          S             10
002100000505     D X15Esito        S              1
002200000505     D Eseguito        C                   '0'
002300000505     D Errore          C                   '1'
002400990521
002500990525     C                   open      qrpglesrc
002600990524
002700990520     C/EXEC SQL
002800990525     C+ DECLARE A1 CURSOR FOR SELECT translate(SRCDTA)
002900990525     C+ FROM qrpglesrc
003000990524     C+ WHERE (substr(srcdta, 7, 1) <> '*')
003100990525     C+ AND   ((translate(substr(srcdta, 6, 1)) = 'I'
003200990526     C+        and translate(substr(srcdta, 17, 1)) = 'E'
003300990526     C+        and translate(substr(srcdta, 19, 2)) = 'DS'))
003400000505     C+        or
003500000505     C+        (translate(substr(srcdta, 7, 5)) = '/COPY')
003600990524     C+ FOR READ ONLY
003700990520     C/END-EXEC
003800990521
003900990521     C/EXEC SQL
004000990521     C+ OPEN A1
004100990521     C/END-EXEC
004200990520
004300990526     C                   reset                   wfine
004400990525     C     wfine         doueq     *on
004500990521
004600990520     C/EXEC SQL
004700990525     C+ FETCH NEXT FROM A1 INTO :srcdta
004800990520     C/END-EXEC
004900990520
005000990520     C     sqlcod        ifne      0
005100990525     C                   eval      wfine = *on
005200990520     C                   else
005300990524     C                   clear                   qwhdrppr
005400990524     C                   eval      formato = %subst(srcdta:6:1)
005500990524     C                   select
005600990525     C     formato       wheneq    'I'
005700990525     C                   exsr      scani
005800990524     C                   endsl
005900000505      *
006000000505     C                   EXSR      ScanCopy
006100000505      *
006200990520     C                   endif
006300990521
006400990521     C                   enddo
006500990521
006600990525     C                   close     qrpglesrc
006700990525
006800990521     C/EXEC SQL
006900990521     C+ CLOSE A1
007000990521     C/END-EXEC
007100990524
007200990525     C                   return
007300990525
007400990525      /TITLE Scansione riga 'I'.
007500990525     C     scani         begsr
007600990524
007700990525     C                   eval      formatoi = srcdta
007800990525     C     dnomeext      ifne      *blanks
007900990525     C                   eval      whfnam = dnomeext
008000990525     C                   else
008100990525     C                   eval      whfnam = dnome
008200990524     C                   endif
008300990524
008400000505     C                   eval      whlnam = '*LIBL'
008500990524     C                   eval      whobjt = 'F'
008600990524     C                   eval      whotyp = '*FILE'
008700990524     C                   exsr      wrtxrpgm
008800990524
008900990524     C                   endsr
009000000505
009100000505     C     ScanCopy      BEGSR
009200000505      *
009300000505     C                   IF        %SUBST(SrcDta:7:5) = '/COPY'
009400000505      *
009500000505     C                   CALL      'XREF15'
009600000505     C                   PARM                    SrcDta
009700000505     C                   PARM      *BLANK        X15Lib
009800000505     C                   PARM      *BLANK        X15Fil
009900000505     C                   PARM      *BLANK        X15Mbr
010000000505     C                   PARM      *BLANK        X15Esito
010100000505      *
010200000505     C                   IF        X15Esito = Eseguito
010300000505     C                   EVAL      whfnam = X15Mbr
010400000505     C                   EVAL      whlnam = X15Lib
010500000505     C                   EVAL      whobjt = 'S'
010600000505     C                   EVAL      whotyp = '*COPY'
010700000505     C                   EXSR      WrtXrPgm
010800000505     C                   ENDIF
010900000505      *
011000000505     C                   ENDIF
011100000505      *
011200000505     C                   ENDSR
011300000505
011400990524      /TITLE Scrittura XRPGM00F.
011500990524     C     wrtxrpgm      begsr
011600990524
011700990524     C                   eval      whpnam = srcmembro
011800990524     C                   eval      whlib  = srclibreria
011900990524     C     k04pgm01      chain     xrpgm01l
012000990524     C                   if        not %found
012100990524     C                   eval      whtext = srctesto
012200990526     C                   eval      whsnam = srcfile
012300990524     C                   write     qwhdrppr
012400990524     C                   endif
012500990524
012600990524     C                   endsr
012700990524      /TITLE Operazioni iniziali.
012800990524     C     *inzsr        begsr
012900990524
013000990524     C     *entry        plist
013100990524     C                   parm                    srclibreria      10
013200990524     C                   parm                    srcfile          10
013300990524     C                   parm                    srcmembro        10
013400990525     C                   parm                    srctipo          10
013500990524     C                   parm                    srctesto         50
013600990524
013700990524     C     k04pgm01      klist
013800990524     C                   kfld                    whfnam
013900990524     C                   kfld                    whlnam
014000990525     C                   kfld                    whpnam
014100990525     C                   kfld                    whlib
014200990524
014300990524     C                   endsr
