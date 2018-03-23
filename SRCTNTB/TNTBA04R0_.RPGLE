000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400160411      *  Descrizione   :  filtro conferma ricezione stampa tariffe autisti AFF/DEF
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700160928     Ftntba03d0 CF   E             WORKSTN
000800160928     fazorg01l  if   e           k disk
000900000000      *
001000160411     D KPJBA         E DS
001100160411     D CNCR80        E DS
001200160928     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300940926     D*
001400081008      *__________________________________________________________________
001500000000     C     *ENTRY        PLIST
001600000000     C                   PARM                    KPJBA
001700000000      *
001800000000      *--------------------------------------------------------------*
001900000000      *             M A I N      L I N E
002000000000      *--------------------------------------------------------------*
002100000000     C                   Z-ADD     1             CODUT
002200000000     C                   CALL      'X§PARUT'
002300000000     C                   PARM                    UT§DSE
002400000000     C                   MOVEL     RAGUT         RSUT             20
002500000000     C                   MOVEL     RAGUT         RSUT2            20
002600000000     C                   MOVEL     REC80         CNCR80
002700000000      *
002800000000      *
002900000000      * Emetto il formato video
003000090914      *
003100160411     c                   do        *hival
003200081008     C                   EXFMT     cna7D01
003300160928     c                   if        vfil > 0
003400160928     c     vfil          chain     azorg01l
003500160928     c                   if        %found(azorg01l)
003600160928     c                   movel     orgdes        vfild
003700160928     c                   else
003800160928     c                   seton                                        90
003900160928     c                   iter
004000160928     c                   endif
004100160928     c                   endif
004200000000      *
004300160411     C   KC              LEAVE
004400000000      *
004500081008      * Richiamo programma di conferma
004600160928     c                   if        vfil > 0
004700160928     c                   movel     vfil          kpjbu
004800160928     c                   call      'TNTBA03R'
004900160411     c                   parm                    kpjba
005000160928     c                   else
005100160928     c                   call      'TNTBA03R1'
005200160411     c                   parm                    kpjba
005300160928     c                   endif
005400160411     c                   enddo
005500000000      *
005600000000     C                   SETON                                        LR
