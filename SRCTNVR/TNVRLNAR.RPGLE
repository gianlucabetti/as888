000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300111018     ffnvabw    uf   E             DISK
000400980611      * ---------------------------------------------------------
000500061012     D tisi95ds      E DS
000600980625      * ---------------------------------------------------------
000700111018     c*
000800111018     c     *start        setll     fnvabw
000900111018     c                   read      fnvabw
001000111018     c                   dow       not %eof(fnvabw)
001100111018     c*
001200111018     c                   CLEAR                   TISI95DS                       *azzera i parametri
001300111018     c                   MOVEL     *blanks       I95TLA                         *tipo lancio
001400111018     c                   MOVEL     '7'           I95TCN                         *tipo controllo
001500111018     c                   MOVEL     vabnzd        I95NAR                         *nazione
001600111018     c                   MOVEL     vabcad        I95CAP                         *cap
001700111018     c                   MOVEL     vablod        I95LOC                         *località
001800111018     c                   MOVEL     vabprd        I95PRV                         *provincia
001900111018     c                   MOVEL     'C'           I95TSP                         *tipo servizio
002000111018     c                   Z-ADD     vabpkb        I95LKG                         *peso
002100111018     c                   EVAL      I95DAT = %dec(%date() : *ISO)
002200111018     c*
002300111018     c                   CALL      'TISI95R'
002400111018     c                   PARM                    TISI95DS
002500111018     c*
002600111018     c                   eval      vablna = O95LNA
002700111018     c                   eval      vabznc = O95ZNC
002800111018     c*
002900111018     c                   update    fnvab000
003000111018     c*
003100111018     c                   read      fnvabw
003200111018     c                   enddo
003300111018     c*
003400061012     c                   SETON                                        lr
