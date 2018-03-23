000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030904      *       prompt di controllo dati ricevuti dal partner
000400000000      *****************************************************************
000500130823     FAZORG01L  IF   E           K DISK
000600040831     fkfazn11l  if   E           k disk
000700060802     Ffieu91D   CF   E             WORKSTN
000800030116      *****************************************************************
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200130823     D OG143         E DS
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400040819     D*
001500060802     D fieu91ds      e DS                  inz
001600940926     D*
001700000000     D WLBDAT          DS
001800940926     D  G02DAT                 1      8  0
001900940926     D  G02INV                 9     16  0
002000940926     D  G02ERR                17     17
002100940926     D  G02TGI                18     22  0
002200030205      *
002300041008     d datiso          s               d   Datfmt(*iso)
002400041008     d data30          s               d   Datfmt(*iso)
002500041008      *
002600030116     D digits          C                   CONST('0123456789')
002700030116      *****************************************************************
002800000000     C     *ENTRY        PLIST
002900000000     C                   PARM                    KPJBA
003000030120      *
003100030825     C* Imposta la UDATE  da TIME
003200030825     C                   TIME                    W0140            14 0
003300030825     C                   MOVE      W0140         UDATE8            8 0
003400030825     C*
003500030120      *  In base all'utente prende il P.O.
003600030120     C                   CLEAR                   Tibs36Ds
003700030120     c                   EVAL      I36ute = Knmus
003800030120     c                   EVAL      I36Tla = 'L'
003900030120     C                   CALL      'TIBS36R'
004000030120     C                   PARM                    Tibs36Ds
004100030827     c                   movel     KNMUS         ALFA3             3
004200030829     c                   setoff                                       0102
004300030829      * solo per EDP....:
004400030829     c                   if        ALFA3 ='EDP'
004500030829     c                   seton                                        01
004600030829     c                   end
004700030829      * per utenti di filiale:
004800030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
004900030829     c                   seton                                        02
005000030829     c                   end
005100000000      *--------------------------------------------------------------*
005200000000      *             M A I N      L I N E
005300000000      *--------------------------------------------------------------*
005400000000     C                   Z-ADD     1             CODUT
005500000000     C                   CALL      'X§PARUT'
005600000000     C                   PARM                    UT§DSE
005700000000     C                   MOVEL     RAGUT         RSUT             20
005800000000     C                   MOVEL     RAGUT         RSUT2            20
005900000000     C                   MOVEL     REC80         CNCR80
006000040819      *
006100040819     C     INIZIO        tag
006200030825      *
006300030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
006400040819     c                   clear                   D1NTW
006500041012     c                   clear                   D1DSP
006600030915      *
006700000000      * Emetto il formato video
006800030116     C     Video1        TAG
006900020308     C                   SETOFF                                       90
007000060802     C                   EXFMT     eu91d01
007100060802      *
007200060802     c                   clear                   inKq              1
007300060802     C   KQ              do
007400060802     C                   EXFMT     eu91w01
007500060802     c   KF              move      '1'           inKq              1
007600060802     c                   end
007700000000      *
007800000000     C   KC              GOTO      FINE                                         F3=USCITA
007900000000      *
008000000000      * Eseguo i  controlli sui campi immessi a video
008100000000     C                   EXSR      CONTR
008200000000      *
008300020308      * Se non vi sono errori ma non e' premuto F6 emetto video
008400030915     C   90
008500060802     CorNkf              GOTO      Video1
008600030905      *
008700060802     c                   clear                   fieu91ds
008800030905      * compone il codice numerico
008900060802     c                   z-add     AlAmg         e91dtf
009000060802     c                   move      d1ntw         e91ntw
009100060802     c                   z-add     AlDsp         e91dsp
009200060802     c                   movel     fieu91ds      kpjbu
009300000000      *
009400060802      *  solo x F16 - Annullamento dati consolidati
009500060802     c                   if        inKQ = *on
009600060802      *
009700040819      *  richiama il pgm che esegue il consolidamento
009800060802     c                   movel     'EU92'        kcoaz
009900040831     c     kcoaz         chain     kfazn11l
010000040831     c                   if        not %Found(kfazn11l)
010100060802     c                   call      'FIEU92R'
010200040831     c                   parm                    kpjba
010300040831     c                   else
010400041012     c                   if        ALFA3 ='EDP'
010500040819     c                   call      'BCH09'
010600040819     c                   parm                    kpjba
010700041026     c                   end
010800040819     c                   call      'BCH10'
010900040819     c                   parm                    kpjba
011000040831     c                   end
011100060802      *
011200060802     c                   end
011300040819      *
011400030116     C                   GOTO      INIZIO
011500030116      *
011600000000     C     FINE          TAG
011700000000     C                   SETON                                        LR
011800000000      *--------------------------------------------------------------*
011900000000      * Routine per i controlli sui campi del formato video          *
012000000000      *--------------------------------------------------------------*
012100000000     C     CONTR         BEGSR
012200000000      *
012300030116     c                   movea     '0000000000'  *in(41)
012400030825      *
012500030116     C                   Z-ADD     d1Dal         G02DAT
012600020624     C                   MOVEL     *BLANK        G02ERR
012700020624     C                   CALL      'XSRDA8'
012800020624     C                   PARM                    WLBDAT
012900020624     C     G02ERR        IFEQ      '1'
013000020624     C                   SETON                                        42  90
013100030116     C                   GOTO      endct1
013200020624     C                   ENDIF
013300030825     c*
013400030116     C                   Z-ADD     G02INV        AlAmG             8 0
013500041008     C                   move      G02INV        datiso
013600030116     C                   Z-ADD     G02dat        d1dal
013700040819     c*
013800040819     C     d1ntw         IFEQ      *blank
013900040819     C                   SETON                                        41  90
014000040819     C                   GOTO      endct1
014100130823
014200130823     c                   else
014300130823      *
014400130823     c                   if        d1ntw <>'DPD' and d1ntw <>'FED'
014500130823      * controlla la LNP se network EDI estero
014600130823     c                   z-add     0             dove              3 0
014700130823     c     digits        check     d1ntw         dove
014800130823     c                   if        dove > 0
014900130823      * ci sono alfanumerici
015000130823     C                   SETON                                        4190
015100130823     C                   GOTO      endct1
015200130823     C                   Else
015300130823     c                   move      d1ntw         ntw_num3          3 0
015400130823     c     ntw_num3      chain     azorg01l
015500130823     c                   if        not %Found(azorg01l)
015600130823     C                   SETON                                        4190
015700130823     C                   GOTO      endct1
015800130823     C                   Else
015900130823     c                   eval      og143 = orgde3
016000130823     c                   if        §ogNTW <> 'EEX'
016100130823     C                   SETON                                        4190
016200130823     C                   GOTO      endct1
016300130823     c                   end
016400130823     c                   end
016500130823     C                   End
016600130823      *
016700130823     C                   End
016800040819     C                   ENDIF
016900041008      *
017000041008     c                   if        d1dsp = 0
017100041008     c     datiso        subdur    1:*m          data30
017200041008     C     *eur          move      data30        d1dSp
017300041008     c                   end
017400041008      *
017500041008     C                   clear                   AldSp             8 0
017600041008      *
017700041008     c                   if        d1dsp > 0
017800041008     C                   Z-ADD     d1Dsp         G02DAT
017900041008     C                   MOVEL     *BLANK        G02ERR
018000041008     C                   CALL      'XSRDA8'
018100041008     C                   PARM                    WLBDAT
018200041008     C     G02ERR        IFEQ      '1'
018300041008     C                   SETON                                        48  90
018400041008     C                   GOTO      endct1
018500041008     C                   ENDIF
018600041008     c*
018700041008     C                   Z-ADD     G02INV        AldSp
018800041008     C                   Z-ADD     G02dat        d1dsp
018900041008     c                   end
019000030129      *
019100030116     C     endct1        ENDSR
019200030904      *--------------------------------------------------------------*
