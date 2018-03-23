000100011011      *
000200011019      *  Routine calcolo "J" per spedizioni posta
000300011019      * -----------------------------------------
000400011011      *
000500011019      *  Il programma riceve le ore di non conformità e se utilizzare
000600011019      *  le tabelle per sede o p.o..
000700011019      *  Restituisce la chiave della tabella JPT corrispondente, se non
000800011019      *  trovasse il record corrispondente la ritorna a blank.
000900011011
001000011011     H DECEDIT('0,') DATEDIT(*DMY.)
001100011011
001200011019      * Archivi
001300011019     fTNTBE01L  IF   E           K DISK    usropn
001400011019
001500011019      *
001600011019     D DJPT          e ds
001700011019     D TRUL20DS      e ds
001800011011
001900011019      * Schiere
002000011019     D JPTp            S             15    DIM(50)                              JPT p.o. CODICI
002100011019     D HHDp            S              4  0 DIM(50)                              JPT p.o. HH DA
002200011019     D HHAp            S              4  0 DIM(50)                              JPT p.o. HH A
002300011019     D JPTs            S             15    DIM(50)                              JPT sede CODICI
002400011019     D HHDs            S              4  0 DIM(50)                              JPT sede HH DA
002500011019     D HHAs            S              4  0 DIM(50)                              JPT sede HH A
002600011019
002700011019      * Campi
002800011019     D xx              s              2  0
002900011019     D pp              s              2  0
003000011019     D ss              s              2  0
003100011019     D Whhd            s              4  0
003200011019     D Whha            s              4  0
003300011019
003400011019      *----------------------------------------------------------------
003500011019
003600011019     C     *ENTRY        PLIST
003700011019     C                   PARM                    TRUL20DS
003800011019
003900011019     C                   clear                   O20KE1
004000011019
004100011019      * Controllo validità tipo lancio
004200011030     c                   IF        I20TLA <> 'C' and
004300011030     c                             I20TLA <> 'L' and
004400011030     c                             I20TLA <> *blanks
004500011019     c                   clear                   I20TLA
004600011019     c                   ENDIF
004700011019
004800011019      * Se Tipo lancio 'C' --> solo chiusura file
004900011126      * Se Tipo Calcolo non impostato vado a fine
005000011126     c                   IF        I20TLA <> 'C'  and  I20TPC <> *blanks
005100011019     C                   Exsr      Elabora
005200011019     c                   ENDIF
005300011011
005400020726     c                   IF        I20TLA = 'L'
005500020726     c                   eval      *inRT = *on
005600011019     c                   ELSE
005700020726     c                   eval      *inLR = *on
005800011019     c                   ENDIF
005900011011
006000011011      *----------------------------------------------------
006100011019      * ELABORAZIONE
006200011011      *----------------------------------------------------
006300011019     C     Elabora       Begsr
006400011019      *
006500011019      * Carico tabelle in schiera se mai fatto
006600011019     C                   IF        JPTs(1) = *blanks  and  JPTp(1) = *blanks
006700011019     c                   exsr      CaricaJPT
006800011019     c                   ENDIF
006900011019      *
007000011019     c                   clear                   xx
007100011019      *
007200011019      * Cerco JPT per p.o.
007300011019     c                   IF        I20TPC = 'P'
007400011019      *
007500011019     C                   DOW       %elem(JPTp) > xx
007600011019     c                   add       1             xx
007700011019     c                   SELECT
007800011030     c                   WHEN      JPTp(xx) = *blanks
007900011019     c                   leave
008000011030     C                   WHEN      I20ORE >= HHDp(xx) and
008100011030     C                             I20ORE <= HHAp(xx)
008200011030     c                   eval      O20KE1 = JPTp(xx)
008300011019     C                   leave
008400011019     C                   ENDSL
008500011019     C                   ENDDO
008600011019      *
008700011019      * Cerco JPT per sede
008800011019     c                   ELSE
008900011019      *
009000011019     C                   DOW       %elem(JPTs) > xx
009100011019     c                   add       1             xx
009200011019     c                   SELECT
009300011030     c                   WHEN      JPTs(xx) = *blanks
009400011019     c                   leave
009500011030     C                   WHEN      I20ORE >= HHDs(xx) and
009600011030     C                             I20ORE <= HHAs(xx)
009700011030     c                   eval      O20KE1 = JPTs(xx)
009800011019     C                   leave
009900011019     C                   ENDSL
010000011019     C                   ENDDO
010100011019      *
010200011019     C                   ENDIF
010300011019      *
010400011011     C                   Endsr
010500011011      *----------------------------------------------------
010600011019      * Carico schiere
010700011011      *----------------------------------------------------
010800011019     C     CaricaJPT     Begsr
010900011019      *
011000011019     C                   open      TNTBE01L
011100011019     C                   clear                   ss
011200011019     C                   clear                   pp
011300011019      *
011400011019     C     'JPT'         SETLL     TNTBE01L
011500011019     C     'JPT'         READE     TNTBE01L
011600011019      *
011700011019     C                   DOW       not %EOF(tntbe01l)
011800011019      *
011900011019     c                   IF        TBEATB = *blanks  and
012000011019     c                             TBELIN = *blanks  and
012100011019     c                             TBESIF = *blanks
012200011019     C                   movel     TBEUNI        DJPT
012300011019      *
012400011019     c                   IF        §JPTupo = 'S'  or  §JPTuse = 'S'
012500011019      *  calcolo ora da
012600011019     C                   IF        §JPTSGD = '-'
012700011019     C                   eval      Whhd = §JPTHHD * -1
012800011019     C                   ELSE
012900011019     C                   eval      Whhd = §JPTHHD
013000011019     C                   ENDIF
013100011019      *  calcolo ora a
013200011019     C                   IF        §JPTSGA = '-'
013300011030     C                   eval      Whha = §JPTHHA * -1
013400011019     C                   ELSE
013500011030     C                   eval      Whha = §JPTHHA
013600011019     C                   ENDIF
013700011019      *  riempio schiera p.o.
013800011019     c                   IF        §JPTupo = 'S'
013900011019     c                   add       1             pp
014000011019     C                   eval      JPTp(pp) = TBEKE1
014100011019     C                   eval      HHDp(pp) = Whhd
014200011019     C                   eval      HHAp(pp) = Whha
014300011019     C                   ENDIF
014400011019      *  riempio schiera sede
014500011019     c                   IF        §JPTuse = 'S'
014600011019     c                   add       1             ss
014700011019     C                   eval      JPTs(ss) = TBEKE1
014800011019     C                   eval      HHDs(ss) = Whhd
014900011019     C                   eval      HHAs(ss) = Whha
015000011019     C                   ENDIF
015100011019      *
015200011019     c                   ENDIF
015300011019     c                   ENDIF
015400011019      *
015500011019     C     'JPT'         READE     TNTBE01L
015600011019     C                   ENDDO
015700011019      *
015800011019     C                   close     TNTBE01L
015900011019      *
016000011011     C                   Endsr
