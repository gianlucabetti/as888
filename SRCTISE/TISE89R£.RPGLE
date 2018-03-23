000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600111114     Ftise89D   CF   E             WORKSTN
000700030116      *****************************************************************
000800111114     D tise89ds      e DS                  inz
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300040830     d  xgrp                  54     83    dim(10) descend
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
003800030829      * solo per EDP....:
003900030829     c                   if        ALFA3 ='EDP'
004000030829     c                   seton                                        01
004100030829     c                   end
004200030829      * per utenti di filiale:
004300030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
004400030829     c                   seton                                        02
004500030829     c                   end
004600000000      *--------------------------------------------------------------*
004700000000      *             M A I N      L I N E
004800000000      *--------------------------------------------------------------*
004900000000     C                   Z-ADD     1             CODUT
005000000000     C                   CALL      'X§PARUT'
005100000000     C                   PARM                    UT§DSE
005200000000     C                   MOVEL     RAGUT         RSUT             20
005300000000     C                   MOVEL     RAGUT         RSUT2            20
005400000000     C                   MOVEL     REC80         CNCR80
005500040819      *
005600040819     C     INIZIO        tag
005700030825      *
005800030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
005900030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
006000030915      *
006100000000      * Emetto il formato video
006200030116     C     Video1        TAG
006300020308     C                   SETOFF                                       90
006400111114     C                   EXFMT     eu89d01
006500000000      *
006600000000     C   KC              GOTO      FINE                                         F3=USCITA
006700000000      *
006800000000      * Eseguo i  controlli sui campi immessi a video
006900000000     C                   EXSR      CONTR
007000040830      *
007100040830      * Se non vi sono errori ma non e' premuto F6 emetto video
007200040830     C   90
007300040830     CorNkf              GOTO      Video1
007400040830      *
007500111115      *  Pulisce il file di work
007600111115     c                   call      'TISE89C'
007700111115      *
007800111114     c                   clear                   tise89ds
007900030905      * compone il codice numerico
008000111114     c                   z-add     DalAmg        e89dti
008100111114     c                   z-add     AlAmg         e89dtf
008200111114     c                   movel     tise89ds      kpjbu
008300111114     c                   movel     'SE8A'        kcoaz
008400040826     c     kcoaz         chain     kfazn11l
008500040826     c                   if        not %Found(kfazn11l)
008600111115      *  Sottomette la richiesta
008700111114     c                   call      'TISE89R1'
008800040826     c                   parm                    kpjba
008900040826     c                   else
009000041103     c                   eval      KJOST ='H'
009100041103     c                   if        ALFA3 ='EDP'
009200040819     c                   call      'BCH09'
009300040819     c                   parm                    kpjba
009400041103     c                   end
009500040819     c                   call      'BCH10'
009600040819     c                   parm                    kpjba
009700040826     c                   end
009800000000      *
009900030116     C                   GOTO      INIZIO
010000030116      *
010100000000     C     FINE          TAG
010200000000     C                   SETON                                        LR
010300000000      *--------------------------------------------------------------*
010400000000      * Routine per i controlli sui campi del formato video          *
010500000000      *--------------------------------------------------------------*
010600000000     C     CONTR         BEGSR
010700000000      *
010800030116     c                   movea     '0000000000'  *in(41)
010900040819     c                   movea     '0000000000'  *in(51)
011000030116      * --------------
011100020624      * controllo immissione delle date
011200030116     C                   Z-ADD     d1DDAL        G02DAT
011300000000     C                   MOVEL     *BLANK        G02ERR
011400940926     C                   CALL      'XSRDA8'
011500000000     C                   PARM                    WLBDAT
011600000000     C     G02ERR        IFEQ      '1'
011700020308     C                   SETON                                        44  90
011800030116     C                   GOTO      endct1
011900000000     C                   ENDIF
012000030116     C                   Z-ADD     G02INV        DalAmg            8 0
012100030116     C                   Z-ADD     G02dat        D1ddal
012200030825      *
012300020624      * Controllo data finale
012400030116     C     d1Dal         ifeq      0
012500030127     C                   Z-ADD     d1DDal        d1Dal
012600030127     C                   End
012700030825      *
012800030116     C                   Z-ADD     d1Dal         G02DAT
012900020624     C                   MOVEL     *BLANK        G02ERR
013000020624     C                   CALL      'XSRDA8'
013100020624     C                   PARM                    WLBDAT
013200020624     C     G02ERR        IFEQ      '1'
013300020624     C                   SETON                                        42  90
013400030116     C                   GOTO      endct1
013500020624     C                   ENDIF
013600030825     c*
013700030116     C                   Z-ADD     G02INV        AlAmG             8 0
013800030116     C                   Z-ADD     G02dat        d1dal
013900030127     c*
014000030116     C     DalAmg        ifgt      AlAmg
014100020624     C                   SETON                                        4590
014200030116     C                   GOTO      endct1
014300020624     C                   END
014400030129      *
014500040819      *
014600040819      * --------------
014700040819      *
014800030116     C     endct1        ENDSR
014900030116      *--------------------------------------------------------------*
