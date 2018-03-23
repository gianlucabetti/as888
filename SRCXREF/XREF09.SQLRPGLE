000100990520      *PARMS DFTACTGRP(*YES)
000200990521      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300010928      *PARMS DYNUSRPRF(*OWNER)
000400990525      /TITLE Scansione membri origine tipo RPGIV.
000500090716     F*qrpglesrc if   e             disk    usropn
000600990524     Fxrpgm01l  if a e           k disk
000700990524
000800990525     D srcdta          s            100
000900990524     D formato         s              1
001000990524     D formatoD        ds           100
001100990524     D Dnome                   7     21
001200990524     D De                     22     22
001300990524     D Dds                    24     25
001400990524     D Dparole                44     80
001500990524     D formatoC        ds           100
001600990524     D Coperaz                26     35
001700990524     D Cfattore2              36     49
001800990524     D nomeds          s             10
001900990521     D nomemod         s             10
002000000505     D i               S              5U 0
002100000505     D f               S              5U 0
002200990526     D wfine           s              1    inz('0')
002300000505     D X15Lib          S             10
002400000505     D X15Fil          S             10
002500000505     D X15Mbr          S             10
002600000505     D X15Esito        S              1
002700000505     D Eseguito        C                   '0'
002800000505     D Errore          C                   '1'
002900990521
003000090716     C*                  open      qrpglesrc
003100990524
003200990520     C/EXEC SQL
003300090717     C+ DECLARE A1 CURSOR FOR
003400090717     C+ WITH MEMBRO (SRCDTA) AS (
003500090717     C+ SELECT UCASE(SRCDTA)
003600090717     C+ FROM QRPGLESRC
003700090717     C+ WHERE SUBSTR(SRCDTA, 7, 1) <> '*'
003800090717     C+   AND (UCASE(SUBSTR(SRCDTA, 6, 1)) IN ('D','C')
003900090717     C+        OR UCASE(SUBSTR(SRCDTA, 7, 5)) = '/COPY')
004000090717     C+ )
004100090717     C+ SELECT SRCDTA
004200090717     C+ FROM MEMBRO
004300090717     C+ WHERE ((SUBSTR(SRCDTA, 6, 1) = 'D'
004400090717     C+         AND SUBSTR(SRCDTA, 22, 1) = 'E'
004500090717     C+         AND SUBSTR(SRCDTA, 24, 2) = 'DS')
004600090717     C+        OR
004700090717     C+        (SUBSTR(SRCDTA, 6, 1) = 'D'
004800090717     C+         AND SUBSTR(SRCDTA, 44, 37) LIKE '%EXTNAME%')
004900090717     C+        OR
005000090717     C+        (SUBSTR(SRCDTA, 6, 1) = 'D'
005100090717     C+        AND SUBSTR(SRCDTA, 9, 72) LIKE '%...%')
005200090717     C+        OR
005300090717     C+        (SUBSTR(SRCDTA, 6, 1) = 'C'
005400090717     C+        AND SUBSTR(SRCDTA, 26, 5) in ('CALLB','CALLP')))
005500090717     C+        OR
005600090717     C+        (SUBSTR(SRCDTA, 7, 5) = '/COPY')
005700990524     C+ FOR READ ONLY
005800990520     C/END-EXEC
005900990521
006000990521     C/EXEC SQL
006100990521     C+ OPEN A1
006200990521     C/END-EXEC
006300990520
006400990526     C                   reset                   wfine
006500990525     C     wfine         doueq     *on
006600990521
006700990520     C/EXEC SQL
006800990525     C+ FETCH NEXT FROM A1 INTO :srcdta
006900990520     C/END-EXEC
007000990520
007100990520     C     sqlcod        ifne      0
007200000505      *
007300990525     C                   eval      wfine = *on
007400000505      *
007500990520     C                   else
007600000505      *
007700990524     C                   eval      formato = %subst(srcdta:6:1)
007800000505      *
007900990524     C                   select
008000990524     C     formato       wheneq    'D'
008100990524     C                   exsr      scand
008200990524     C     formato       wheneq    'C'
008300990524     C                   exsr      scanc
008400990524     C                   endsl
008500000505      *
008600000505     C                   EXSR      ScanCopy
008700000505      *
008800990520     C                   endif
008900990521
009000990521     C                   enddo
009100990521
009200090716     C*                  close     qrpglesrc
009300990525
009400990521     C/EXEC SQL
009500990521     C+ CLOSE A1
009600990521     C/END-EXEC
009700990524
009800990525     C                   return
009900990525
010000990524      /TITLE Scansione riga 'D'.
010100990524     C     scand         begsr
010200990524
010300090717     C                   IF        %SUBST(SrcDta:7:5) = '/COPY'
010400090717     C                   LEAVESR
010500090717     C                   ENDIF
010600000505      *
010700090717     C     '...'         scan(e)   srcdta:9      f
010800090717     C                   if        %found
010900090717     C     ' '           check     srcdta:7      i
011000090717     C                   eval      f = f - i
011100090717     C                   if        i > 0 and f > 0 and f < 11
011200090717     C                   eval      whfnam = %subst(srcdta:i:f)
011300090717     C                   ENDIF
011400090717     C                   LEAVESR
011500090717     C                   ENDIF
011600090717      *
011700990524     C                   eval      formatod = srcdta
011800990524     C     'EXTNAME'     scan(e)   dparole       i
011900990524     C                   if        %found
012000990524     C     '('           scan      dparole:i     i
012100990524     C                   add       1             i
012200990524     C     ')'           scan      dparole:i     f
012300990524     C                   sub       i             f
012400990524     C                   eval      whfnam = %subst(dparole:i:f)
012500990524     C                   else
012600090717     C                   if        dnome <> *blank
012700090717     C                   if        %subst(dnome:1:1) = *blank
012800990524     C     ' '           check     dnome         i
012900990524     C                   else
013000990524     C                   eval      i = 1
013100990524     C                   endif
013200990524     C     ' '           checkr    dnome         f
013300990524     C                   eval      f = f - i + 1
013400090716     C                   if        i > 0 and f > 0
013500090716     C                   eval      whfnam = %subst(dnome:i:f)
013600090716     C                   ENDIF
013700090717     C                   ENDIF
013800990524     C                   endif
013900990524
014000990524     C                   eval      whobjt = 'F'
014100990524     C                   eval      whotyp = '*FILE'
014200000505     C                   eval      whlnam = '*LIBL'
014300990524     C                   exsr      wrtxrpgm
014400990524
014500990524     C                   endsr
014600090717
014700990524      /TITLE Scansione riga 'C'.
014800990524     C     scanc         begsr
014900990524
015000990524     C                   eval      formatoc = srcdta
015100990525     C                   if        (coperaz = 'CALLB')
015200990524     C                             and (%subst(cfattore2:1:1) = '''')
015300990524     C     '''':' '      xlate     cfattore2     cfattore2
015400990524     C                   eval      whfnam = %subst(cfattore2:2:10)
015500990524     C                   eval      whobjt = 'P'
015600990524     C                   eval      whotyp = '*MODULE'
015700990524     C                   eval      whspkg = 'M'
015800000505     C                   eval      whlnam = '*LIBL'
015900990524     C                   exsr      wrtxrpgm
016000990521     C                   endif
016100990524
016200990520     C                   endsr
016300000505
016400000505     C     ScanCopy      BEGSR
016500000505      *
016600000505     C                   IF        %SUBST(SrcDta:7:5) = '/COPY'
016700000505      *
016800000505     C                   CALL      'XREF15'
016900000505     C                   PARM                    SrcDta
017000000505     C                   PARM      *BLANK        X15Lib
017100000505     C                   PARM      *BLANK        X15Fil
017200000505     C                   PARM      *BLANK        X15Mbr
017300000505     C                   PARM      *BLANK        X15Esito
017400000505      *
017500000505     C                   IF        X15Esito = Eseguito
017600000505     C                   EVAL      whfnam = X15Mbr
017700000505     C                   EVAL      whlnam = X15Lib
017800000505     C                   EVAL      whobjt = 'S'
017900000505     C                   EVAL      whotyp = '*COPY'
018000000505     C                   EXSR      WrtXrPgm
018100000505     C                   ENDIF
018200000505      *
018300000505     C                   ENDIF
018400000505      *
018500000505     C                   ENDSR
018600000505
018700990524      /TITLE Scrittura XRPGM00F.
018800990524     C     wrtxrpgm      begsr
018900990524
019000090716     C                   if        whfnam = *blank
019100090716     C                   leavesr
019200090716     C                   ENDIF
019300990524     C                   eval      whpnam = srcmembro
019400990524     C                   eval      whlib  = srclibreria
019500990524     C     k04pgm01      chain     xrpgm01l
019600990524     C                   if        not %found
019700990524     C                   eval      whtext = srctesto
019800990526     C                   eval      whsnam = srcfile
019900990524     C                   write     qwhdrppr
020000990524     C                   endif
020100090717     C                   clear                   qwhdrppr
020200990524
020300990524     C                   endsr
020400990524      /TITLE Operazioni iniziali.
020500990524     C     *inzsr        begsr
020600990524
020700990524     C     *entry        plist
020800990524     C                   parm                    srclibreria      10
020900990524     C                   parm                    srcfile          10
021000990524     C                   parm                    srcmembro        10
021100990525     C                   parm                    srctipo          10
021200990524     C                   parm                    srctesto         50
021300990524
021400990524     C     k04pgm01      klist
021500990524     C                   kfld                    whfnam
021600990524     C                   kfld                    whlnam
021700990525     C                   kfld                    whpnam
021800990525     C                   kfld                    whlib
021900990524
022000990524     C                   endsr
