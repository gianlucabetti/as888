000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400160411      *  Descrizione   :  filtro conferma ricezione stampa tariffe autisti AFF/DEF
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700160411     Fficna7D0  CF   E             WORKSTN
000800000000      *
000900160411     D KPJBA         E DS
001000160411     D CNCR80        E DS
001100160411     D tibs42ds      E DS
001200160411     D uteautds      E DS
001300160411     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400940926     D*
001500081008      *__________________________________________________________________
001600000000     C     *ENTRY        PLIST
001700000000     C                   PARM                    KPJBA
001800081008
001900000000      *
002000000000      *--------------------------------------------------------------*
002100000000      *             M A I N      L I N E
002200000000      *--------------------------------------------------------------*
002300000000     C                   Z-ADD     1             CODUT
002400000000     C                   CALL      'X§PARUT'
002500000000     C                   PARM                    UT§DSE
002600000000     C                   MOVEL     RAGUT         RSUT             20
002700000000     C                   MOVEL     RAGUT         RSUT2            20
002800000000     C                   MOVEL     REC80         CNCR80
002900000000      *
003000160411     c                   clear                   tibs42ds
003100160411     c                   movel     001           i42pge
003200160411     c                   call      'TIBS42R'
003300160411     c                   parm                    tibs42ds
003400160411     c                   movel     o42uni        uteautds
003500160411     c                   if        §affcrs <> 'S'
003600160411     c                   goto      fine
003700160411     c                   endif
003800000000      *
003900000000      * Emetto il formato video
004000000000     C     EMETTO        TAG
004100020308     C                   SETOFF                                       90
004200090914      *
004300160411     c                   do        *hival
004400081008     C                   EXFMT     cna7D01
004500000000      *
004600160411     C   KC              LEAVE
004700000000      *
004800081008      * Richiamo programma di conferma
004900160411     c                   select
005000160411     c                   when      vtipo = 'C'
005100160411     c                   call      'FICNA7R1'
005200160411     c                   parm                    kpjba
005300160411     c                   when      vtipo = 'A'
005400160511     c                   movel(p)  vtipo         kpjbu
005500160512     c                   call      'FICN16R'
005600160411     c                   parm                    kpjba
005700160511     c                   clear                   kpjbu
005800160411     c                   when      vtipo = 'D'
005900160411     c                   call      'FICNA7R'
006000160411     c                   parm                    kpjba
006100160411     c                   when      vtipo = *blank
006200160411     c                   iter
006300160411      *
006400160411     c                   endsl
006500160411     c                   enddo
006600000000      *
006700160411     c     fine          tag
006800000000     C                   SETON                                        LR
