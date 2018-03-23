000100060925      *---------------------------------------------------------------*
000200070426      *?CAPPARIO DPD - richiamo interrogazioni                       ?*
000300060925      *---------------------------------------------------------------*
000400060925
000500060925     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060925
000700060925      *---------------------------------------------------------------*
000800060925
000900060925      * Versioni
001000060925      *---------------------------------------------------------------*
001100060925      * - Architettura
001200060925     d KPJBA         e ds
001300060925     d TIBS34ds      e ds                                                       *Profili utente
001400060925     d dDatiUte      e ds                                                       *Dati utente
001500060925     d AZUTEds       e ds                  extname(AZUTE00F)                    *Utenti
001600060926      *
001800170704     d TISID8ds      e ds                  inz
001900070426     D wrkoggiiso      s               d
002000060925      *---------------------------------------------------------------*
002100060925     c     *Entry        plist
002200060925     c                   parm                    KPJBA
002300070426     c
002400170704     C                   CLEAR                   tisiD8ds
002500070426     C                   time                    wrkoggiiso
002600170704     C     *iso          move      wrkoggiiso    isiD8dri
002700170704     c                   movel     'D01'         isiD8op0
002800170704     C                   MOVEL     'A  '         isiD8nar
002900170704     C                   CALL      'TISID8R'
003000070426     C                   PARM                    kpjba
003100170704     C                   PARM                    tisiD8ds
003200060925      *
003300060925      * Operazioni Finali
003400060925     c                   eval      *inLR   = *on
