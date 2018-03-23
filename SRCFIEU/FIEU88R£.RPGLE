000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030904      *       prompt di controllo dati ricevuti dal partner
000400000000      *****************************************************************
000500040831     fkfazn11l  if   E           k disk
000600040819     Ffieu88D   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200040819     D*
001300040819     D fieu88ds      e DS                  inz
001400940926     D*
001500000000     D WLBDAT          DS
001600940926     D  G02DAT                 1      8  0
001700940926     D  G02INV                 9     16  0
001800940926     D  G02ERR                17     17
001900940926     D  G02TGI                18     22  0
002000030205      *
002100041008     d datiso          s               d   Datfmt(*iso)
002200041008     d data30          s               d   Datfmt(*iso)
002300041008      *
002400030116     D digits          C                   CONST('0123456789')
002500030116      *****************************************************************
002600000000     C     *ENTRY        PLIST
002700000000     C                   PARM                    KPJBA
002800030120      *
002900030825     C* Imposta la UDATE  da TIME
003000030825     C                   TIME                    W0140            14 0
003100030825     C                   MOVE      W0140         UDATE8            8 0
003200030825     C*
003300030120      *  In base all'utente prende il P.O.
003400030120     C                   CLEAR                   Tibs36Ds
003500030120     c                   EVAL      I36ute = Knmus
003600030120     c                   EVAL      I36Tla = 'L'
003700030120     C                   CALL      'TIBS36R'
003800030120     C                   PARM                    Tibs36Ds
003900030827     c                   movel     KNMUS         ALFA3             3
004000030829     c                   setoff                                       0102
004100030829      * solo per EDP....:
004200030829     c                   if        ALFA3 ='EDP'
004300030829     c                   seton                                        01
004400030829     c                   end
004500030829      * per utenti di filiale:
004600030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
004700030829     c                   seton                                        02
004800030829     c                   end
004900000000      *--------------------------------------------------------------*
005000000000      *             M A I N      L I N E
005100000000      *--------------------------------------------------------------*
005200000000     C                   Z-ADD     1             CODUT
005300000000     C                   CALL      'X§PARUT'
005400000000     C                   PARM                    UT§DSE
005500000000     C                   MOVEL     RAGUT         RSUT             20
005600000000     C                   MOVEL     RAGUT         RSUT2            20
005700000000     C                   MOVEL     REC80         CNCR80
005800040819      *
005900040819     C     INIZIO        tag
006000030825      *
006100030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
006200040819     c                   clear                   D1NTW
006300041012     c                   clear                   D1DSP
006400030915      *
006500000000      * Emetto il formato video
006600030116     C     Video1        TAG
006700020308     C                   SETOFF                                       90
006800040819     C                   EXFMT     eu88d01
006900000000      *
007000000000     C   KC              GOTO      FINE                                         F3=USCITA
007100000000      *
007200000000      * Eseguo i  controlli sui campi immessi a video
007300000000     C                   EXSR      CONTR
007400000000      *
007500020308      * Se non vi sono errori ma non e' premuto F6 emetto video
007600030915     C   90
007700040819     CorNkf              GOTO      Video1
007800030905      *
007900040819     c                   clear                   fieu88ds
008000030905      * compone il codice numerico
008100040819     c                   z-add     AlAmg         e88dtf
008200040819     c                   move      d1ntw         e88ntw
008300041008     c                   z-add     AlDsp         e88dsp
008400000000      *
008500040819      *  richiama il pgm che esegue il consolidamento
008600040819     c                   movel     fieu88ds      kpjbu
008700040819     c                   movel     'EU89'        kcoaz
008800040831     c     kcoaz         chain     kfazn11l
008900040831     c                   if        not %Found(kfazn11l)
009000040831     c                   call      'FIEU89R'
009100040831     c                   parm                    kpjba
009200040831     c                   else
009300041012     c                   if        ALFA3 ='EDP'
009400040819     c                   call      'BCH09'
009500040819     c                   parm                    kpjba
009600041026     c                   end
009700040819     c                   call      'BCH10'
009800040819     c                   parm                    kpjba
009900040831     c                   end
010000040819      *
010100030116     C                   GOTO      INIZIO
010200030116      *
010300000000     C     FINE          TAG
010400000000     C                   SETON                                        LR
010500000000      *--------------------------------------------------------------*
010600000000      * Routine per i controlli sui campi del formato video          *
010700000000      *--------------------------------------------------------------*
010800000000     C     CONTR         BEGSR
010900000000      *
011000030116     c                   movea     '0000000000'  *in(41)
011100030825      *
011200030116     C                   Z-ADD     d1Dal         G02DAT
011300020624     C                   MOVEL     *BLANK        G02ERR
011400020624     C                   CALL      'XSRDA8'
011500020624     C                   PARM                    WLBDAT
011600020624     C     G02ERR        IFEQ      '1'
011700020624     C                   SETON                                        42  90
011800030116     C                   GOTO      endct1
011900020624     C                   ENDIF
012000030825     c*
012100030116     C                   Z-ADD     G02INV        AlAmG             8 0
012200041008     C                   move      G02INV        datiso
012300030116     C                   Z-ADD     G02dat        d1dal
012400040819     c*
012500040819     C     d1ntw         IFEQ      *blank
012600040819     C                   SETON                                        41  90
012700040819     C                   GOTO      endct1
012800040819     C                   ENDIF
012900041008      *
013000041008     c                   if        d1dsp = 0
013100041008     c     datiso        subdur    1:*m          data30
013200041008     C     *eur          move      data30        d1dSp
013300041008     c                   end
013400041008      *
013500041008     C                   clear                   AldSp             8 0
013600041008      *
013700041008     c                   if        d1dsp > 0
013800041008     C                   Z-ADD     d1Dsp         G02DAT
013900041008     C                   MOVEL     *BLANK        G02ERR
014000041008     C                   CALL      'XSRDA8'
014100041008     C                   PARM                    WLBDAT
014200041008     C     G02ERR        IFEQ      '1'
014300041008     C                   SETON                                        48  90
014400041008     C                   GOTO      endct1
014500041008     C                   ENDIF
014600041008     c*
014700041008     C                   Z-ADD     G02INV        AldSp
014800041008     C                   Z-ADD     G02dat        d1dsp
014900041008     c                   end
015000030129      *
015100030116     C     endct1        ENDSR
015200030904      *--------------------------------------------------------------*
