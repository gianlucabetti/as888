000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600170622     Ftrec95D   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100170622     D DATA_iso        S               D   DATFMT(*iso)
001200170622     D DATA_eur        S               D   DATFMT(*eur)
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400170622     D trec96ds      e DS                  inz
001500940926     D*
001600000000     D WLBDAT          DS
001700940926     D  G02DAT                 1      8  0
001800940926     D  G02INV                 9     16  0
001900940926     D  G02ERR                17     17
002000940926     D  G02TGI                18     22  0
002100030205      *
002200030116     D digits          C                   CONST('0123456789')
002300030116      *****************************************************************
002400000000     C     *ENTRY        PLIST
002500000000     C                   PARM                    KPJBA
002600030120      *
002700030825     C* Imposta la UDATE  da TIME
002800030825     C                   TIME                    W0140            14 0
002900030825     C                   MOVE      W0140         UDATE8            8 0
003000170622     C                   Z-ADD     UDATE8        G02DAT
003100170622     C                   MOVEL     *BLANK        G02ERR
003200170622     C                   CALL      'XSRDA8'
003300170622     C                   PARM                    WLBDAT
003400170622     C     G02ERR        IFNE      '1'
003500170622     C                   Z-ADD     G02INV        udtymg            8 0
003600170622     C                   ENDIF
003700030825     C*
003800030120      *  In base all'utente prende il P.O.
003900030120     C                   CLEAR                   Tibs36Ds
004000030120     c                   EVAL      I36ute = Knmus
004100030120     c                   EVAL      I36Tla = 'L'
004200030120     C                   CALL      'TIBS36R'
004300030120     C                   PARM                    Tibs36Ds
004400030827     c                   movel     KNMUS         ALFA3             3
004500000000      *--------------------------------------------------------------*
004600000000      *             M A I N      L I N E
004700000000      *--------------------------------------------------------------*
004800000000     C                   Z-ADD     1             CODUT
004900000000     C                   CALL      'X§PARUT'
005000000000     C                   PARM                    UT§DSE
005100000000     C                   MOVEL     RAGUT         RSUT             20
005200000000     C                   MOVEL     RAGUT         RSUT2            20
005300000000     C                   MOVEL     REC80         CNCR80
005400170622     C                   SETOFF                                       02
005500040819      *
005600040819     C     INIZIO        tag
005700030825      *
005800170622     c                   move      '01'          udtymg
005900170622     c                   movel     udtymg        data_iso
006000170622     c                   subdur    6:*m          data_iso
006100170622     c                   move      data_iso      data_eur
006200170622     C                   move      data_eur      D1Dsped
006300170622      *
006400170622     c                   movel     udtymg        data_iso
006500170622     c                   subdur    2:*m          data_iso
006600170622     c                   move      data_iso      data_eur
006700170622     C                   move      data_eur      D1Ddec
006800170622      *
006900170622     C                   move      'S'           D1stp             1
007000170622     C                   move      'N'           D1agg
007100030915      *
007200000000      * Emetto il formato video
007300030116     C     Video1        TAG
007400020308     C                   SETOFF                                       90
007500170622     C                   EXFMT     ec95d01
007600000000      *
007700000000     C   KC              GOTO      FINE
007800170622     C   KB              SETON                                        02
007900000000      *
008000000000      * Eseguo i  controlli sui campi immessi a video
008100000000     C                   EXSR      CONTR
008200040830      *
008300040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008400040830     C   90
008500040830     CorNkf              GOTO      Video1
008600030905      *
008700170622     c                   clear                   TREC96ds
008800030905      * compone il codice numerico
008900170622     c                   z-add     DaSped        C96DASPED
009000170622     c                   z-add     DecTar        C96DADEC
009100170622     c                   move      d1stp         C96STP
009200170622     C                   move      d1agg         C96AGG
009300040819      *
009400040819      *  richiama il pgm che esegue il consolidamento
009500170622     c                   movel     trec96ds      kpjbu
009600170622     c                   movel     'EC96'        kcoaz
009700040826     c     kcoaz         chain     kfazn11l
009800040826     c                   if        not %Found(kfazn11l)
009900170622     c                   call      'TREC96R'
010000040826     c                   parm                    kpjba
010100040826     c                   else
010200040819     c                   call      'BCH09'
010300040819     c                   parm                    kpjba
010400040819     c                   call      'BCH10'
010500040819     c                   parm                    kpjba
010600040826     c                   end
010700000000      *
010800170622     C*****              GOTO      INIZIO
010900030116      *
011000000000     C     FINE          TAG
011100000000     C                   SETON                                        LR
011200000000      *--------------------------------------------------------------*
011300000000      * Routine per i controlli sui campi del formato video          *
011400000000      *--------------------------------------------------------------*
011500000000     C     CONTR         BEGSR
011600000000      *
011700030116     c                   movea     '0000000000'  *in(41)
011800040819     c                   movea     '0000000000'  *in(51)
011900030116      * --------------
012000170622      * controllo data decorrenza tariffa
012100170622     C                   Z-ADD     d1Ddec        G02DAT
012200000000     C                   MOVEL     *BLANK        G02ERR
012300940926     C                   CALL      'XSRDA8'
012400000000     C                   PARM                    WLBDAT
012500000000     C     G02ERR        IFEQ      '1'
012600020308     C                   SETON                                        44  90
012700030116     C                   GOTO      endct1
012800000000     C                   ENDIF
012900170622     C                   Z-ADD     G02INV        DecTar            8 0
013000170622     C                   Z-ADD     G02dat        D1Ddec
013100030825      *
013200170622      * Controllo data spedizioni
013300170622     C                   Z-ADD     d1Dsped       G02DAT
013400020624     C                   MOVEL     *BLANK        G02ERR
013500020624     C                   CALL      'XSRDA8'
013600020624     C                   PARM                    WLBDAT
013700020624     C     G02ERR        IFEQ      '1'
013800020624     C                   SETON                                        42  90
013900030116     C                   GOTO      endct1
014000020624     C                   ENDIF
014100170622     C                   Z-ADD     G02INV        DaSped            8 0
014200170622     C                   Z-ADD     G02dat        d1Dsped
014300040819      * --------------
014400030116     C     endct1        ENDSR
014500030116      *--------------------------------------------------------------*
