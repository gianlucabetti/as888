000100160210     ***********************************************************************
000200160210     ** Modulo......: TIO730R
000300160210     ** Descrizione.: Calcola e restituisce il Numero Prenotazione Ritiro NPR
000400160210     **
000500160210     ** Compilazione: CRTRPGMOD  MODULE(GAITRAFIL/TIO730R)
000600160210     **                          SRCFILE(GAITRASRC/SRCTIO7)
000700160210     **                          SRCMBR(TIO730R)
000800160210     **                          REPLACE(*YES)
000900160210     **
001000160210     **               CRTSRVPGM  SRVPGM(GAITRAFIL/TIO730R)
001100160210     **                          EXPORT(*ALL)
001200160210     **
001300160210     **               DLTMOD     MODULE(GAITRAFIL/TIO730R)
001400160210     **
001500160210     **               ADDBNDDIRE BNDDIR(GAITRAFIL/TIO7)
001600160210     **                          OBJ((*LIBL/TIO730R *SRVPGM))
001700160210     **
001800160210     ***********************************************************************
001900160210     H NOMAIN
002000160210
002100160210     FTio7300F  UF A E             DISK
002200160210     F                                     RENAME(Tio7300F:Tio73000)
002300160210     F                                     PREFIX(Fld)
002400160210     F                                     USROPN
002500160210
002600160210     D Alfabeto        C                   'QAZXSWEDCVFRTGBNHYUJMKIOLP'
002700160210     D Elementi        C                   26
002800160210
002900160210     D l               S             10I 0
003000160210     D m               S             10I 0
003100160210     D s               S             10I 0
003200160210     D Let             S              1
003300160210     D                                     DIM(Elementi)
003400160210     D WrkTime         S               Z
003500160210     D WrkEstrai       S              1
003600160210     D                                     INZ(*OFF)
003700160210
003800160210     D PtrFldTio7300F  S               *
003900160210     D                                     INZ(%ADDR(FldTio7300F))
004000160210
004100160224     D WrkNPR          DS            10
004200160210     D                                     BASED(PtrFldTio7300F)
004300160210     D  WrkNPRChr                     3
004400160210     D  WrkNPRNum                     7S 0
004500160210     D  WrkDa0a9              10     10S 0
004600160210
004700160211     D GetNPR          PR            10
004800160210     D  ParametroU                   10
004900160210
005000160210     ***********************************************************************
005100160210     **
005200160211     ** Funzione GetNPR.
005300160210     **
005400160210     ***********************************************************************
005500160211     P GetNPR          B                   EXPORT
005600160210
005700160211     D GetNPR          PI            10
005800160210     D  PrmNPR
005900160210     D                                     LIKE(FldTio7300F)
006000160210
006100160210     ** Operazioni eseguite solo la prima volta.
006200160210     C                   IF        NOT %OPEN(Tio7300F)
006300160210     C                   OPEN      Tio7300F
006400160210     C                   MOVEA     Alfabeto      Let(1)
006500160210     C                   EVAL      WrkEstrai = *ON
006600160210     C                   ELSE
006700160210     C     *START        SETLL     Tio7300F
006800160210     C                   ENDIF
006900160210
007000160210     ** Reperisco la parte numerica del codice.
007100160210     C                   READ      Tio7300F
007200160210     C*
007300160210     C                   IF        NOT %EOF
007400160210     C                   ADD       1             WrkNPRNum
007500160210     C                   ELSE
007600160210     C                   Z-ADD     1             WrkNPRNum
007700160210     C                   ENDIF
007800160210
007900160210     ** Reperisco valori casuali.
008000160210     C                   IF        WrkEstrai = *ON
008100160210     C*
008200160210     C                   RESET                   WrkEstrai
008300160210     C*
008400160210     C                   TIME                    WrkTime
008500160210     C*
008600160210     C                   EXTRCT    WrkTime:*MS   l
008700160210     C                   EXTRCT    WrkTime:*S    s
008800160210     C                   EXTRCT    WrkTime:*M    m
008900160210     C*
009000160210     C                   EVAL      l = l + (WrkDa0a9 + 1)
009100160210     C                   EVAL      s = s + l
009200160210     C                   EVAL      m = m + s
009300160210     C*
009400160210     C                   IF        l > Elementi
009500160210     C     l             DIV       Elementi      l
009600160210     C                   MVR                     l
009700160210     C                   ENDIF
009800160210     C*
009900160210     C                   IF        s > Elementi
010000160210     C     s             DIV       Elementi      s
010100160210     C                   MVR                     s
010200160210     C                   ENDIF
010300160210     C*
010400160210     C                   SELECT
010500160210     C                   WHEN      m > Elementi
010600160210     C     m             DIV       Elementi      m
010700160210     C                   MVR                     m
010800160210     C                   WHEN      m = 0
010900160210     C                   EVAL      m = 1
011000160210     C                   ENDSL
011100160210     C*
011200160210     C                   ENDIF
011300160210
011400160210     ** Calcolo la 1a lettera.
011500160210     C                   EVAL      l = l + 1
011600160210     C*
011700160210     C                   IF        l > Elementi
011800160210     C                   EVAL      l = l - Elementi
011900160210     C                   EVAL      WrkEstrai = *ON
012000160210     C                   ENDIF
012100160210     C*
012200160210     C                   EVAL      %SUBST(WrkNPRChr:1:1) = Let(l)
012300160210
012400160210     ** Calcolo la 2a lettera.
012500160210     C                   EVAL      s = s - (WrkDa0a9 + 1)
012600160210     C*
012700160210     C                   IF        s <= 0
012800160210     C                   EVAL      s = s + Elementi
012900160210     C                   ENDIF
013000160210     C*
013100160210     C                   EVAL      %SUBST(WrkNPRChr:2:1) = Let(s)
013200160210
013300160210     ** Calcolo la 3a lettera.
013400160210     C                   EVAL      m = m + (WrkDa0a9 + 1)
013500160210     C*
013600160210     C                   IF        m > Elementi
013700160210     C                   EVAL      m = m - Elementi
013800160210     C                   ENDIF
013900160210     C*
014000160210     C                   EVAL      %SUBST(WrkNPRChr:3:1) = Let(m)
014100160210
014200160210     ** Aggiorno numeratore.
014300160210     C                   IF        NOT %EOF
014400160210     C                   UPDATE    Tio73000
014500160210     C                   ELSE
014600160210     C                   WRITE     Tio73000
014700160210     C                   ENDIF
014800160210
014900160210     C                   EVAL      PrmNPR = WrkNPR
015000160210     C                   RETURN    PrmNPR
015100160210
015200160211     P GetNPR          E
