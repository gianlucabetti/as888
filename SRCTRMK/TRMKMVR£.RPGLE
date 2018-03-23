000100030723     H DECEDIT('0,') DATEDIT(*DMY/)
000200090203     H* trmkvr *---------------------------------------------------*
000300090203     H*   Prepara dati per mappatura
000400930126     H*---------------------------------------------------------------*
000500090203     Ftrmkmvd   CF   E             WORKSTN
000600080319     d
000700040604     D CNCR80        E DS
000800040604     D KPJBA         E DS
000900030722     D UT§DSE0F      E DS
001000090203     d
001100090203     D PARAM           DS
001200090203     D  v1ccpo                 1      1
001300090203     D  v1cwadel               2      2
001400090204     D  v1cdempc               3      3
001500090204     D  v1cdempd               4      4
001600090209     D  v1cdemuc               5      5
001700090209     D  v1cinfo                6      6
001800090205     dwelab            s              1
001900090114     D*
002000000000     C*---------------------------------------------------------------*
002100000000     C     *ENTRY        PLIST
002200000000     C                   PARM                    KPJBA
002300941221     C*
002400000000     C                   Z-ADD     1             CODUT
002500911010     C                   CALL      'X§PARUT'
002600030722     C                   PARM                    UT§DSE0F
002700920901     C                   MOVEL     REC80         CNCR80
002800941221     C                   MOVEL     RAGUT         RSUT
002900081121     C*---------------------------------------------------------------*
003000090210     c                   eval      v1ccpo=' '
003100090210     c                   eval      v1cwadel=' '
003200090204     c                   eval      v1cdempc=' '
003300090204     c                   eval      v1cdempd=' '
003400090205     c                   eval      v1cUNI=' '
003500090209     c                   eval      v1cTOP=' '
003600090210     c                   eval      v1cTOU=' '
003700090212     c                   eval      v1cTOR=' '
003800090209     c                   eval      v1cDEMUc=' '
003900090209     c                   eval      v1cinfo  =' '
004000090211     c                   eval      v1cSPP   =' '
004100971223     C*
004200921123     C     FOR01         TAG
004300911008     C*
004400090203     C                   EXFMT     mkmvd1
004500040607     C*
004600030722     C** F3 - FINE LAVORO
004700040604     C   KC              GOTO      FINE
004800921117     C*
004900081121     C*
005000081121     C  nkf              GOTO      FOR01
005100040528     c
005200030722     C* F6  - ELABORAZIONE
005300090205     c                   if        *inkf
005400090205
005500090205     c                   if        v1cUNI='S'
005600090205     C                   CALL      'TRMKM4R'
005700090205     C                   PARM                    KPJBA
005800090205     C                   EVAL      WELAB='S'
005900090205     C                   ENDIF
006000090205     C
006100090204     c                   if        v1ccpo='S' or v1cwadel='S' or
006200090210     c                             v1cdempc='S' or v1cdempd='S'  or
006300090210     c                             v1cdemuc='S'  or v1cinfo='S'
006400090203     c                   movel     param         kpjbu
006500090203     C                   CALL      'TRMKM3R'
006600040607     C                   PARM                    KPJBA
006700090205     C                   EVAL      WELAB='S'
006800090205     c                   endif
006900090209
007000090209     c                   if        v1ctop='S'
007100090209     C                   CALL      'TRMKM5R'
007200090209     C                   PARM                    KPJBA
007300090209     C                   EVAL      WELAB='S'
007400090209     C                   ENDIF
007500090210
007600090210     c                   if        v1ctoU='S'
007700090210     C                   CALL      'TRMKM6R'
007800090210     C                   PARM                    KPJBA
007900090210     C                   EVAL      WELAB='S'
008000090210     C                   ENDIF
008100090211
008200090211     c                   if        v1cSPP='S'
008300090211     C                   CALL      'TRMKM7R'
008400090211     C                   PARM                    KPJBA
008500090211     C                   EVAL      WELAB='S'
008600090211     C                   ENDIF
008700090212
008800090212     c                   if        v1cTOR='S'
008900090212     C                   CALL      'TRMKM8R'
009000090212     C                   PARM                    KPJBA
009100090212     C                   EVAL      WELAB='S'
009200090212     C                   ENDIF
009300090205
009400090205     c* se non ho fatto nulla non esco dalla videata
009500090205     c                   if        welab=' '
009600090203     c                   goto      for01
009700040604     c                   endif
009800090203     c                   endif
009900040604     c*
010000000000     C     FINE          TAG
010100000000     C                   SETON                                        LR
