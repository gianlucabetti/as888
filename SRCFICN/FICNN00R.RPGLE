000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500130822     FAZORG01L  IF   E           K DISK
000600040826     fkfazn11l  if   E           k disk
000700170706     Fficnn00D  CF   E             WORKSTN
000800030116      *****************************************************************
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300170706     D ficnn01ds     e DS                  inz
001400940926     D*
001500000000     D WLBDAT          DS
001600940926     D  G02DAT                 1      8  0
001700940926     D  G02INV                 9     16  0
001800940926     D  G02ERR                17     17
001900940926     D  G02TGI                18     22  0
002000030205      *
002100030116     D digits          C                   CONST('0123456789')
002200030116      *****************************************************************
002300000000     C     *ENTRY        PLIST
002400000000     C                   PARM                    KPJBA
002500030120      *
002600030825     C* Imposta la UDATE  da TIME
002700030825     C                   TIME                    W0140            14 0
002800030825     C                   MOVE      W0140         UDATE8            8 0
002900030825     C*
003000030120      *  In base all'utente prende il P.O.
003100030120     C                   CLEAR                   Tibs36Ds
003200030120     c                   EVAL      I36ute = Knmus
003300030120     c                   EVAL      I36Tla = 'L'
003400030120     C                   CALL      'TIBS36R'
003500030120     C                   PARM                    Tibs36Ds
003600030827     c                   movel     KNMUS         ALFA3             3
003700030829     c                   setoff                                       0102
003800000000      *--------------------------------------------------------------*
003900000000      *             M A I N      L I N E
004000000000      *--------------------------------------------------------------*
004100000000     C                   Z-ADD     1             CODUT
004200000000     C                   CALL      'X§PARUT'
004300000000     C                   PARM                    UT§DSE
004400000000     C                   MOVEL     RAGUT         RSUT             20
004500000000     C                   MOVEL     RAGUT         RSUT2            20
004600000000     C                   MOVEL     REC80         CNCR80
004700040819      *
004800040819     C     INIZIO        tag
004900030825      *
005000170706     c                   clear                   D1fil
005100170706     c                   clear                   D1fild
005200030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
005300030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
005400030915      *
005500000000      * Emetto il formato video
005600030116     C     Video1        TAG
005700020308     C                   SETOFF                                       90
005800170706     C                   EXFMT     NN00D01
005900000000      *
006000000000     C   KC              GOTO      FINE                                         F3=USCITA
006100000000      *
006200000000      * Eseguo i  controlli sui campi immessi a video
006300000000     C                   EXSR      CONTR
006400040830      *
006500040830      * Se non vi sono errori ma non e' premuto F6 emetto video
006600040830     C   90
006700040830     CorNkf              GOTO      Video1
006800040830      *
006900170706     c                   clear                   ficnn01ds
007000170706      *
007100030905      * compone il codice numerico
007200170706     c                   move      d1fil         n01FGS
007300170706     c                   z-add     DalAmg        n01DADATA
007400170706     c                   z-add     AlAmg         n01ADATA
007500040819      *
007600040819      *  richiama il pgm che esegue il consolidamento
007700170706     c                   movel     ficnn01ds     kpjbu
007800170706     c                   movel     'FNN1'        kcoaz
007900040826     c     kcoaz         chain     kfazn11l
008000040826     c                   if        not %Found(kfazn11l)
008100170706     c                   call      'FICNN01R'
008200040826     c                   parm                    kpjba
008300040826     c                   else
008400041103     c                   eval      KJOST ='H'
008500041103     c                   if        ALFA3 ='EDP'
008600040819     c                   call      'BCH09'
008700040819     c                   parm                    kpjba
008800041103     c                   end
008900040819     c                   call      'BCH10'
009000040819     c                   parm                    kpjba
009100040826     c                   end
009200000000      *
009300030116     C                   GOTO      INIZIO
009400030116      *
009500000000     C     FINE          TAG
009600000000     C                   SETON                                        LR
009700000000      *--------------------------------------------------------------*
009800000000      * Routine per i controlli sui campi del formato video          *
009900000000      *--------------------------------------------------------------*
010000000000     C     CONTR         BEGSR
010100000000      *
010200030116     c                   movea     '0000000000'  *in(41)
010300040819     c                   movea     '0000000000'  *in(51)
010400170706      *
010500170706     C     d1fil         ifNE      999
010600170706     C     d1fil         ifeq      0
010700040819     C                   SETON                                        4190
010800040819     C                   GOTO      endct1
010900130822     c                   else
011000170706     c     d1fil         chain     azorg01l
011100130822     c                   if        not %Found(azorg01l)
011200130822     C                   SETON                                        4190
011300130822     C                   GOTO      endct1
011400170706     c                   else
011500170706     c                   eval      d1fild = orgDES
011600170706     c                   end
011700170706     C                   Endif
011800170706     c                   end
011900030116      * --------------
012000020624      * controllo immissione delle date
012100030116     C                   Z-ADD     d1DDAL        G02DAT
012200000000     C                   MOVEL     *BLANK        G02ERR
012300940926     C                   CALL      'XSRDA8'
012400000000     C                   PARM                    WLBDAT
012500000000     C     G02ERR        IFEQ      '1'
012600020308     C                   SETON                                        44  90
012700030116     C                   GOTO      endct1
012800000000     C                   ENDIF
012900170706      *
013000030116     C                   Z-ADD     G02INV        DalAmg            8 0
013100030116     C                   Z-ADD     G02dat        D1ddal
013200030825      *
013300020624      * Controllo data finale
013400030116     C     d1Dal         ifeq      0
013500030127     C                   Z-ADD     d1DDal        d1Dal
013600030127     C                   End
013700030825      *
013800030116     C                   Z-ADD     d1Dal         G02DAT
013900020624     C                   MOVEL     *BLANK        G02ERR
014000020624     C                   CALL      'XSRDA8'
014100020624     C                   PARM                    WLBDAT
014200020624     C     G02ERR        IFEQ      '1'
014300020624     C                   SETON                                        42  90
014400030116     C                   GOTO      endct1
014500020624     C                   ENDIF
014600030825     c*
014700030116     C                   Z-ADD     G02INV        AlAmG             8 0
014800030116     C                   Z-ADD     G02dat        d1dal
014900030127     c*
015000030116     C     DalAmg        ifgt      AlAmg
015100020624     C                   SETON                                        4590
015200030116     C                   GOTO      endct1
015300020624     C                   END
015400030129      *
015500040819      * --------------
015600030116     C     endct1        ENDSR
015700030116      *--------------------------------------------------------------*
