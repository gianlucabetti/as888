000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040902      *  aggiorna gruppi generici 999 sistemati in tabella TNCFP
000400000000      *****************************************************************
000500040902     Ftnefr00f  uF   E             disk
000600040902     Ftncfp02l  iF   E           k disk
000700030116      *****************************************************************
000800040902     d nrRecord        S             10I 0
000900040902      *
001000030116     d KPJBA         E DS
001100030120     D Tibs36ds      E DS
001200000000     D CNCR80        E DS
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400940926     D*
001500000000     D WLBDAT          DS
001600940926     D  G02DAT                 1      8  0
001700940926     D  G02INV                 9     16  0
001800940926     D  G02ERR                17     17
001900940926     D  G02TGI                18     22  0
002000040902      *
002100040902     D  data_Oggi      s                   like(G02INV) INZ(0)
002200030205      *
002300030116     D digits          C                   CONST('0123456789')
002400030116      *****************************************************************
002500040902     c     Kcfp_1        klist
002600040902     c                   kfld                    efrntw
002700040902     c                   kfld                    savcau           15
002800040902      *
002900000000     C     *ENTRY        PLIST
003000000000     C                   PARM                    KPJBA
003100030120      *
003200030825     C* Imposta la UDATE  da TIME
003300040902     C                   move      *date         G02DAT
003400040902     C                   MOVE      *ZEROS        G02INV
003500040902     C                   MOVE      *BLANKS       G02ERR
003600040902     C                   CALL      'XSRDA8'
003700040902     C                   PARM                    WLBDAT
003800040902     C                   Z-ADD     G02INV        data_oggi
003900030825     C                   TIME                    W0140            14 0
004000030825     C                   MOVE      W0140         UDATE8            8 0
004100030825     C*
004200030120      *  In base all'utente prende il P.O.
004300030120     C                   CLEAR                   Tibs36Ds
004400030120     c                   EVAL      I36ute = Knmus
004500030120     c                   EVAL      I36Tla = 'L'
004600030120     C                   CALL      'TIBS36R'
004700030120     C                   PARM                    Tibs36Ds
004800030827     c                   movel     KNMUS         ALFA3             3
004900040902     c                   setoff                                       01
005000030829      * solo per EDP....:
005100030829     c                   if        ALFA3 ='EDP'
005200030829     c                   seton                                        01
005300030829     c                   end
005400000000      *--------------------------------------------------------------*
005500000000      *             M A I N      L I N E
005600000000      *--------------------------------------------------------------*
005700000000     C                   Z-ADD     1             CODUT
005800000000     C                   CALL      'X§PARUT'
005900000000     C                   PARM                    UT§DSE
006000000000     C                   MOVEL     RAGUT         RSUT             20
006100000000     C                   MOVEL     RAGUT         RSUT2            20
006200000000     C                   MOVEL     REC80         CNCR80
006300030904     C/EXEC SQL
006400030904     C+ Declare A1 Cursor for
006500040902     c+ SELECT  RRN(TNEFR00F) from TNEFR00F
006600040902     c+ WHERE efrgrp = '999'
006700030904     C/END-EXEC
006800030904      *          apertura cursore
006900030904     C/EXEC SQL
007000030904     C+ OPEN A1
007100030904     C/END-EXEC
007200040902     C                   DOU       SqlCod = 100 OR SqlCod < 0
007300040902      *          lettura cursore
007400040902     C/EXEC SQL
007500040902     C+ Fetch Next From A1 Into :nrrecord
007600040902     C/END-EXEC
007700040902     C                   SELECT
007800040902     C                   WHEN      SqlCod = 100
007900040902      *
008000040902     C                   WHEN      SqlCod >= 0
008100040902     C                   Exsr      Update_grp
008200040902      *
008300040902     C                   OTHER
008400040902     C                   EXSR      SqlError
008500040902     C                   ENDSL
008600040902      *
008700040902     C                   ENDDO
008800030904     C/EXEC SQL
008900030904     C+ Close A1
009000030904     C/END-EXEC
009100040902      *
009200040902      *
009300040902     C     FINE          TAG
009400040902     C                   SETON                                        LR
009500040902      *--------------------------------------------------------------*
009600040902     C* Aggiorna il Gruppo eliminando il gruppo generico 999
009700040902     C*---------------------------------------------------------------*
009800040902     C     Update_grp    BEGSR
009900040902      *
010000040902     c     NrRecord      chain     tnefr00f
010100040902      *  solo se c'è lo
010200040902      *    aggiorna
010300040902     c                   if        %found(tnefr00f)
010400040902      *
010500040902     c                   clear                   savcau
010600040902     c                   movel     efrcau        savcau
010700040902     c     Kcfp_1        setll     tncfp02l
010800040902     c     Kcfp_1        reade     tncfp02l
010900040902     c                   dow       not %Eof(tncfp02l)
011000040902      *
011100040902     c                   if        cfpdda <= data_oggi and
011200040902     c                             cfpada >= data_oggi
011300040902     c                   eval      efrgrp = cfpgrp
011400040902     c                   leave
011500040902     c                   end
011600040902      *
011700040902     c     Kcfp_1        reade     tncfp02l
011800040902     c                   enddo
011900040902      *                           ----------
012000040902     c                   update    tnefr000
012100040902      *                           ----------
012200040902     c                   end
012300040902      *
012400040902     C                   ENDSR
012500040902     C*---------------------------------------------------------------*
012600040902     C* per emissione errore su SQL
012700040902     C*---------------------------------------------------------------*
012800040902     C     SqlError      BEGSR
012900040902      *
013000040902      * Forzo la stampa del JOBLOG.
013100040902      *   e chiude il pgm forzatamente
013200040902     C                   CALL      'X66CHGJOB'
013300040902     C                   EVAL      *INLR = *ON
013400040902     C                   RETURN
013500040902      *
013600040902     C                   ENDSR
013700040902     C*---------------------------------------------------------------*
