000100030625     ***********************************************************************
000200030625     ** Modulo......: TIO711R
000300030625     ** Descrizione.: Calcola e restituisce il riferimento ORM.
000400030625     **
000500030625     ** Compilazione: CRTRPGMOD  MODULE(GAITRAFIL/TIO711R)
000600030625     **                          SRCFILE(GAITRASRC/SRCTIO7)
000700030625     **                          SRCMBR(TIO711R)
000800030625     **                          REPLACE(*YES)
000900030625     **
001000030625     **               CRTSRVPGM  SRVPGM(GAITRAFIL/TIO711R)
001100030625     **                          EXPORT(*ALL)
001200030625     **
001300030625     **               DLTMOD     MODULE(GAITRAFIL/TIO711R)
001400030625     **
001500030625     **               ADDBNDDIRE BNDDIR(GAITRAFIL/TIO7)
001600030625     **                          OBJ((*LIBL/TIO711R *SRVPGM))
001700030625     **
001800030625     ***********************************************************************
001900030625     H NOMAIN
002000030625
002100030625     FTio7100F  UF A E             DISK
002200030625     F                                     RENAME(Tio7100F:Tio71000)
002300030625     F                                     PREFIX(Fld)
002400030625     F                                     USROPN
002500030625
002600030625     D Alfabeto        C                   'QAZXSWEDCVFRTGBNHYUJMKIOLP'
002700030626     D Elementi        C                   26
002800030625
002900030626     D l               S             10I 0
003000030626     D m               S             10I 0
003100030626     D s               S             10I 0
003200030625     D Let             S              1
003300030625     D                                     DIM(Elementi)
003400030626     D WrkTime         S               Z
003500030626     D WrkEstrai       S              1
003600030626     D                                     INZ(*OFF)
003700030625
003800030625     D PtrFldTio7100F  S               *
003900030625     D                                     INZ(%ADDR(FldTio7100F))
004000030625
004100030625     D WrkRfa          DS            15
004200030625     D                                     BASED(PtrFldTio7100F)
004300030625     D  WrkRfaChr                     3
004400030625     D  WrkRfaNum                    12S 0
004500030626     D  WrkDa0a9              15     15S 0
004600030625
004700030625     D GetRifORM       PR            15
004800030625     D  ParametroU                   15
004900030625
005000030625     ***********************************************************************
005100030625     **
005200030625     ** Funzione GetRifORM.
005300030625     **
005400030625     ***********************************************************************
005500030625     P GetRifORM       B                   EXPORT
005600030625
005700030625     D GetRifORM       PI            15
005800030625     D  PrmRfa
005900030625     D                                     LIKE(FldTio7100F)
006000030625
006100030625     ** Operazioni eseguite solo la prima volta.
006200030625     C                   IF        NOT %OPEN(Tio7100F)
006300030625     C                   OPEN      Tio7100F
006400030626     C                   MOVEA     Alfabeto      Let(1)
006500030626     C                   EVAL      WrkEstrai = *ON
006600030625     C                   ELSE
006700030625     C     *START        SETLL     Tio7100F
006800030625     C                   ENDIF
006900030626
007000030626     ** Reperisco la parte numerica del codice.
007100030626     C                   READ      Tio7100F
007200030626     C*
007300030626     C                   IF        NOT %EOF
007400030626     C                   ADD       1             WrkRfaNum
007500030626     C                   ELSE
007600030626     C                   Z-ADD     1             WrkRfaNum
007700030626     C                   ENDIF
007800030626
007900030626     ** Reperisco valori casuali.
008000030626     C                   IF        WrkEstrai = *ON
008100030626     C*
008200030626     C                   RESET                   WrkEstrai
008300030626     C*
008400030626     C                   TIME                    WrkTime
008500030626     C*
008600030626     C                   EXTRCT    WrkTime:*MS   l
008700030626     C                   EXTRCT    WrkTime:*S    s
008800030626     C                   EXTRCT    WrkTime:*M    m
008900030626     C*
009000030626     C                   EVAL      l = l + (WrkDa0a9 + 1)
009100030626     C                   EVAL      s = s + l
009200030626     C                   EVAL      m = m + s
009300030626     C*
009400030626     C                   IF        l > Elementi
009500030626     C     l             DIV       Elementi      l
009600030626     C                   MVR                     l
009700030626     C                   ENDIF
009800030626     C*
009900030626     C                   IF        s > Elementi
010000030626     C     s             DIV       Elementi      s
010100030626     C                   MVR                     s
010200030626     C                   ENDIF
010300030626     C*
010400030626     C                   SELECT
010500030626     C                   WHEN      m > Elementi
010600030626     C     m             DIV       Elementi      m
010700030626     C                   MVR                     m
010800030626     C                   WHEN      m = 0
010900030626     C                   EVAL      m = 1
011000030626     C                   ENDSL
011100030626     C*
011200030626     C                   ENDIF
011300030625
011400030625     ** Calcolo la 1a lettera.
011500030626     C                   EVAL      l = l + 1
011600030626     C*
011700030626     C                   IF        l > Elementi
011800030626     C                   EVAL      l = l - Elementi
011900030626     C                   EVAL      WrkEstrai = *ON
012000030626     C                   ENDIF
012100030626     C*
012200030626     C                   EVAL      %SUBST(WrkRfaChr:1:1) = Let(l)
012300030625
012400030626     ** Calcolo la 2a lettera.
012500030626     C                   EVAL      s = s - (WrkDa0a9 + 1)
012600030626     C*
012700030626     C                   IF        s <= 0
012800030626     C                   EVAL      s = s + Elementi
012900030625     C                   ENDIF
013000030626     C*
013100030626     C                   EVAL      %SUBST(WrkRfaChr:2:1) = Let(s)
013200030626
013300030626     ** Calcolo la 3a lettera.
013400030626     C                   EVAL      m = m + (WrkDa0a9 + 1)
013500030626     C*
013600030626     C                   IF        m > Elementi
013700030626     C                   EVAL      m = m - Elementi
013800030626     C                   ENDIF
013900030626     C*
014000030626     C                   EVAL      %SUBST(WrkRfaChr:3:1) = Let(m)
014100030625
014200030625     ** Aggiorno numeratore.
014300030625     C                   IF        NOT %EOF
014400030625     C                   UPDATE    Tio71000
014500030625     C                   ELSE
014600030625     C                   WRITE     Tio71000
014700030625     C                   ENDIF
014800030625
014900030625     C                   EVAL      PrmRfa = WrkRfa
015000030625     C                   RETURN    PrmRfa
015100030625
015200030625     P GetRifORM       E
