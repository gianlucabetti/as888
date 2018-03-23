000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030904      *       prompt di controllo dati ricevuti dal partner
000400000000      *****************************************************************
000500040831     fkfazn11l  if   E           k disk
000600060802     Ffieu91D   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200040819     D*
001300060802     D fieu91ds      e DS                  inz
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
006800060802     C                   EXFMT     eu91d01
006900060802      *
007000060802     c                   clear                   inKq              1
007100060802     C   KQ              do
007200060802     C                   EXFMT     eu91w01
007300060802     c   KF              move      '1'           inKq              1
007400060802     c                   end
007500000000      *
007600000000     C   KC              GOTO      FINE                                         F3=USCITA
007700000000      *
007800000000      * Eseguo i  controlli sui campi immessi a video
007900000000     C                   EXSR      CONTR
008000000000      *
008100020308      * Se non vi sono errori ma non e' premuto F6 emetto video
008200030915     C   90
008300060802     CorNkf              GOTO      Video1
008400030905      *
008500060802     c                   clear                   fieu91ds
008600030905      * compone il codice numerico
008700060802     c                   z-add     AlAmg         e91dtf
008800060802     c                   move      d1ntw         e91ntw
008900060802     c                   z-add     AlDsp         e91dsp
009000060802     c                   movel     fieu91ds      kpjbu
009100000000      *
009200060802      *  solo x F16 - Annullamento dati consolidati
009300060802     c                   if        inKQ = *on
009400060802      *
009500040819      *  richiama il pgm che esegue il consolidamento
009600060802     c                   movel     'EU92'        kcoaz
009700040831     c     kcoaz         chain     kfazn11l
009800040831     c                   if        not %Found(kfazn11l)
009900060802     c                   call      'FIEU92R'
010000040831     c                   parm                    kpjba
010100040831     c                   else
010200041012     c                   if        ALFA3 ='EDP'
010300040819     c                   call      'BCH09'
010400040819     c                   parm                    kpjba
010500041026     c                   end
010600040819     c                   call      'BCH10'
010700040819     c                   parm                    kpjba
010800040831     c                   end
010900060802      *
011000060802     c                   end
011100040819      *
011200030116     C                   GOTO      INIZIO
011300030116      *
011400000000     C     FINE          TAG
011500000000     C                   SETON                                        LR
011600000000      *--------------------------------------------------------------*
011700000000      * Routine per i controlli sui campi del formato video          *
011800000000      *--------------------------------------------------------------*
011900000000     C     CONTR         BEGSR
012000000000      *
012100030116     c                   movea     '0000000000'  *in(41)
012200030825      *
012300030116     C                   Z-ADD     d1Dal         G02DAT
012400020624     C                   MOVEL     *BLANK        G02ERR
012500020624     C                   CALL      'XSRDA8'
012600020624     C                   PARM                    WLBDAT
012700020624     C     G02ERR        IFEQ      '1'
012800020624     C                   SETON                                        42  90
012900030116     C                   GOTO      endct1
013000020624     C                   ENDIF
013100030825     c*
013200030116     C                   Z-ADD     G02INV        AlAmG             8 0
013300041008     C                   move      G02INV        datiso
013400030116     C                   Z-ADD     G02dat        d1dal
013500040819     c*
013600040819     C     d1ntw         IFEQ      *blank
013700040819     C                   SETON                                        41  90
013800040819     C                   GOTO      endct1
013900040819     C                   ENDIF
014000041008      *
014100041008     c                   if        d1dsp = 0
014200041008     c     datiso        subdur    1:*m          data30
014300041008     C     *eur          move      data30        d1dSp
014400041008     c                   end
014500041008      *
014600041008     C                   clear                   AldSp             8 0
014700041008      *
014800041008     c                   if        d1dsp > 0
014900041008     C                   Z-ADD     d1Dsp         G02DAT
015000041008     C                   MOVEL     *BLANK        G02ERR
015100041008     C                   CALL      'XSRDA8'
015200041008     C                   PARM                    WLBDAT
015300041008     C     G02ERR        IFEQ      '1'
015400041008     C                   SETON                                        48  90
015500041008     C                   GOTO      endct1
015600041008     C                   ENDIF
015700041008     c*
015800041008     C                   Z-ADD     G02INV        AldSp
015900041008     C                   Z-ADD     G02dat        d1dsp
016000041008     c                   end
016100030129      *
016200030116     C     endct1        ENDSR
016300030904      *--------------------------------------------------------------*
