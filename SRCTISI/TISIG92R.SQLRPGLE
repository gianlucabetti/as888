000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200170424      * ?------------------------------------------------------------------ */
000300170503      * Il pgm definisce i movimenti di magazzino richiesti
000400170503      *     IL DETTAGLIO DEI MOVIMENTI GABBIE e BANCALI
000500170424      * ?------------------------------------------------------------------ */
000600170503     Fwfmgb91f  O    E             disk
000700000607     D kpjba         e ds
000800170503     D azMGB00F      e ds
000900170424      **
001000170503     d fldUTE          s                   like(mgbUTE)
001100170503     d fldCAU          s                   like(mgbCAU)
001200170503     d fldSOG          s                   like(mgbSOG)
001300170503     d fldCOD          s                   like(mgbCOD)
001400170503     d fldQTA          s                   like(mgbQTA)
001500170503     d fldDAT          s              8  0
001600170420      **
001700170503     D*------------------
001800170503     D* PASSAGGIO PARAMETRI
001900170503     D*------------------
002000170503     D PARAM           DS                  INZ
002100170503     D  PARDAL                        8  0
002200170503     D  PARAL                         8  0
002300170503     D  PARCAU                        4
002400170503     D  PARCOD                       15
002500170510     D  PARSOG                        8
002600170428      **
002700170424      * ?------------------------------------------------------------------ */
002800170424     c     *entry        plist
002900170424     c                   parm                    KPJBA
003000170503     C*
003100170503     c                   eval      param = kpjbu
003200170424     c                   seton                                        LR
003300170503     C*---
003400161026     C/EXEC SQL
003500170424     C+ DECLARE a1 CURSOR FOR
003600170503     C+ SELECT mgbUTE, mgbCAU, mgbSOG, mgbCOD, mgbQTA, mgbDAT
003700170503     C+  from azmgb00f
003800170503     C+  where mgbdat between :pardal and :paral and
003900170503     C+  mgbCAU = :parCAU and (:parcod = mgbCOD or :parCOD = ' ')
004000170510     C+  and (:parsog = mgbSOG or :parSOG = ' ')
004100161026     C/END-EXEC
004200170424      *
004300170424     C/EXEC SQL
004400170424     C+ OPEN a1
004500170424     C/END-EXEC
004600170424      *
004700170424     C                   do        *hival
004800170424
004900170424     C/EXEC SQL
005000170424     C+ FETCH NEXT FROM a1 INTO
005100170503     C+ :fldUTE, :fldCAU, :fldSOG, :fldCOD, :fldQTA, :fldDAT
005200170424     C/END-EXEC
005300170428      **
005400170424     C                   select
005500170424     C                   when      sqlcod = 100
005600170424     c                   leave
005700170424     C                   WHEN      SqlCod < 0
005800170424     c                   if        sqlcod <> -913
005900170424     C                   seton                                        H1
006000170424     c                   end
006100170424     c                   seton                                        lr
006200170424     c                   LEAVE
006300170424     C                   other
006400170503      **--
006500170503     c                   clear                   wfmgb000
006600170424     c* per ritiro non ho il giro
006700170503     c                   eval      WFMDAL    = parDAL
006800170503     c                   eval      WFMAL     = parAL
006900170503     c                   eval      WFMUTE    = fldUTE
007000170503     c                   eval      WFMCAU    = fldCAU
007100170503     c                   eval      WFMSOG    = fldSOG
007200170503     c                   eval      WFMCOD    = fldCOD
007300170503     c                   eval      WFMQTA    = fldQTA
007400170503     c                   eval      WFMDAT    = fldDAT
007500170503     c                   write     wfmgb000
007600170424     C                   endsl
007700170424     C                   enddo
007800170424     C/EXEC SQL
007900170424     C+ CLOSE a1
008000170424     C/END-EXEC
008100030429      *
008200170424     c                   RETURN
008300161025      * ?___________________________________________________________________
