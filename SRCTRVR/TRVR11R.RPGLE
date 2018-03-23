000100160129     H DFTACTGRP(*NO) ACTGRP('QILE')
000200030625
000300160129     FTRVR100F  UF A E             DISK
000400160129     F                                     RENAME(TRVR100F:TRVR1000)
000500030625     F                                     PREFIX(Fld)
000600030625     F                                     USROPN
000700030625
000800030625     D Alfabeto        C                   'QAZXSWEDCVFRTGBNHYUJMKIOLP'
000900030626     D Elementi        C                   26
001000030625
001100160129     D Risultato       S             15
001200160129
001300030626     D l               S             10I 0
001400030626     D m               S             10I 0
001500030626     D s               S             10I 0
001600030625     D Let             S              1
001700030625     D                                     DIM(Elementi)
001800030626     D WrkTime         S               Z
001900030626     D WrkEstrai       S              1
002000030626     D                                     INZ(*OFF)
002100030625
002200030625     D WrkRfa          DS            15
002300030625     D  WrkRfaChr                     3
002400030625     D  WrkRfaNum                    12S 0
002500030626     D  WrkDa0a9              15     15S 0
002600030625
002700160129     C     *entry        PLIST
002800160129     C                   PARM                    Risultato
002900030625
003000030625     ** Operazioni eseguite solo la prima volta.
003100160129     C                   IF        NOT %OPEN(TRVR100F)
003200160129     C                   OPEN      TRVR100F
003300030626     C                   MOVEA     Alfabeto      Let(1)
003400030626     C                   EVAL      WrkEstrai = *ON
003500030625     C                   ELSE
003600160129     C     *START        SETLL     TRVR100F
003700030625     C                   ENDIF
003800030626
003900030626     ** Reperisco la parte numerica del codice.
004000160129     C                   READ      TRVR100F
004100160129     C*
004200160129     C                   MOVEL     FldTRVR100F   WrkRFA
004300030626     C*
004400030626     C                   IF        NOT %EOF
004500030626     C                   ADD       1             WrkRfaNum
004600030626     C                   ELSE
004700030626     C                   Z-ADD     1             WrkRfaNum
004800030626     C                   ENDIF
004900030626
005000030626     ** Reperisco valori casuali.
005100030626     C                   IF        WrkEstrai = *ON
005200030626     C*
005300030626     C                   RESET                   WrkEstrai
005400030626     C*
005500030626     C                   TIME                    WrkTime
005600030626     C*
005700030626     C                   EXTRCT    WrkTime:*MS   l
005800030626     C                   EXTRCT    WrkTime:*S    s
005900030626     C                   EXTRCT    WrkTime:*M    m
006000030626     C*
006100030626     C                   EVAL      l = l + (WrkDa0a9 + 1)
006200030626     C                   EVAL      s = s + l
006300030626     C                   EVAL      m = m + s
006400030626     C*
006500030626     C                   IF        l > Elementi
006600030626     C     l             DIV       Elementi      l
006700030626     C                   MVR                     l
006800030626     C                   ENDIF
006900030626     C*
007000030626     C                   IF        s > Elementi
007100030626     C     s             DIV       Elementi      s
007200030626     C                   MVR                     s
007300030626     C                   ENDIF
007400030626     C*
007500030626     C                   SELECT
007600030626     C                   WHEN      m > Elementi
007700030626     C     m             DIV       Elementi      m
007800030626     C                   MVR                     m
007900030626     C                   WHEN      m = 0
008000030626     C                   EVAL      m = 1
008100030626     C                   ENDSL
008200030626     C*
008300030626     C                   ENDIF
008400030625
008500030625     ** Calcolo la 1a lettera.
008600030626     C                   EVAL      l = l + 1
008700030626     C*
008800030626     C                   IF        l > Elementi
008900030626     C                   EVAL      l = l - Elementi
009000030626     C                   EVAL      WrkEstrai = *ON
009100030626     C                   ENDIF
009200030626     C*
009300030626     C                   EVAL      %SUBST(WrkRfaChr:1:1) = Let(l)
009400030625
009500030626     ** Calcolo la 2a lettera.
009600030626     C                   EVAL      s = s - (WrkDa0a9 + 1)
009700030626     C*
009800030626     C                   IF        s <= 0
009900030626     C                   EVAL      s = s + Elementi
010000030625     C                   ENDIF
010100030626     C*
010200030626     C                   EVAL      %SUBST(WrkRfaChr:2:1) = Let(s)
010300030626
010400030626     ** Calcolo la 3a lettera.
010500030626     C                   EVAL      m = m + (WrkDa0a9 + 1)
010600030626     C*
010700030626     C                   IF        m > Elementi
010800030626     C                   EVAL      m = m - Elementi
010900030626     C                   ENDIF
011000030626     C*
011100030626     C                   EVAL      %SUBST(WrkRfaChr:3:1) = Let(m)
011200030625
011300030625     ** Aggiorno numeratore.
011400160129     C                   EVAL      FldTRVR100F = WrkRFA
011500030625     C                   IF        NOT %EOF
011600160129     C                   UPDATE    TRVR1000
011700030625     C                   ELSE
011800160129     C                   WRITE     TRVR1000
011900030625     C                   ENDIF
012000030625
012100160129     C                   EVAL      Risultato = WrkRfa
012200030625
012300160129     C                   SETON                                        LR
