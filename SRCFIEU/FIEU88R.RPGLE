000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030904      *       prompt di controllo dati ricevuti dal partner
000400000000      *****************************************************************
000500130822     FAZORG01L  IF   E           K DISK
000600040831     fkfazn11l  if   E           k disk
000700040819     Ffieu88D   CF   E             WORKSTN
000800030116      *****************************************************************
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200130822     D OG143         E DS
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400040819     D*
001500040819     D fieu88ds      e DS                  inz
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
007000040819     C                   EXFMT     eu88d01
007100000000      *
007200000000     C   KC              GOTO      FINE                                         F3=USCITA
007300000000      *
007400000000      * Eseguo i  controlli sui campi immessi a video
007500000000     C                   EXSR      CONTR
007600000000      *
007700020308      * Se non vi sono errori ma non e' premuto F6 emetto video
007800030915     C   90
007900040819     CorNkf              GOTO      Video1
008000030905      *
008100040819     c                   clear                   fieu88ds
008200030905      * compone il codice numerico
008300040819     c                   z-add     AlAmg         e88dtf
008400040819     c                   move      d1ntw         e88ntw
008500041008     c                   z-add     AlDsp         e88dsp
008600000000      *
008700040819      *  richiama il pgm che esegue il consolidamento
008800040819     c                   movel     fieu88ds      kpjbu
008900040819     c                   movel     'EU89'        kcoaz
009000040831     c     kcoaz         chain     kfazn11l
009100040831     c                   if        not %Found(kfazn11l)
009200040831     c                   call      'FIEU89R'
009300040831     c                   parm                    kpjba
009400040831     c                   else
009500041012     c                   if        ALFA3 ='EDP'
009600040819     c                   call      'BCH09'
009700040819     c                   parm                    kpjba
009800041026     c                   end
009900040819     c                   call      'BCH10'
010000040819     c                   parm                    kpjba
010100040831     c                   end
010200040819      *
010300030116     C                   GOTO      INIZIO
010400030116      *
010500000000     C     FINE          TAG
010600000000     C                   SETON                                        LR
010700000000      *--------------------------------------------------------------*
010800000000      * Routine per i controlli sui campi del formato video          *
010900000000      *--------------------------------------------------------------*
011000000000     C     CONTR         BEGSR
011100000000      *
011200030116     c                   movea     '0000000000'  *in(41)
011300030825      *
011400030116     C                   Z-ADD     d1Dal         G02DAT
011500020624     C                   MOVEL     *BLANK        G02ERR
011600020624     C                   CALL      'XSRDA8'
011700020624     C                   PARM                    WLBDAT
011800020624     C     G02ERR        IFEQ      '1'
011900020624     C                   SETON                                        42  90
012000030116     C                   GOTO      endct1
012100020624     C                   ENDIF
012200030825     c*
012300030116     C                   Z-ADD     G02INV        AlAmG             8 0
012400041008     C                   move      G02INV        datiso
012500030116     C                   Z-ADD     G02dat        d1dal
012600040819     c*
012700040819     C     d1ntw         IFEQ      *blank
012800040819     C                   SETON                                        41  90
012900040819     C                   GOTO      endct1
013000130822     c                   else
013100130822      ***
013200130822     c                   if        d1ntw <>'DPD' and d1ntw <>'FED'
013300130822      * controlla la LNP se network EDI estero
013400130822     c                   z-add     0             dove              3 0
013500130822     c     digits        check     d1ntw         dove
013600130822     c                   if        dove > 0
013700130822      * ci sono alfanumerici
013800130822     C                   SETON                                        4190
013900130822     C                   GOTO      endct1
014000130822     C                   Else
014100130822     c                   move      d1ntw         ntw_num3          3 0
014200130822     c     ntw_num3      chain     azorg01l
014300130822     c                   if        not %Found(azorg01l)
014400130822     C                   SETON                                        4190
014500130822     C                   GOTO      endct1
014600130822     C                   Else
014700130822     c                   eval      og143 = orgde3
014800130822     c                   if        §ogNTW <> 'EEX'
014900130822     C                   SETON                                        4190
015000130822     C                   GOTO      endct1
015100130822     c                   end
015200130822     c                   end
015300130822     C                   End
015400130822      *
015500130822     C                   End
015600130822      ***
015700040819     C                   ENDIF
015800041008      *
015900041008     c                   if        d1dsp = 0
016000041008     c     datiso        subdur    1:*m          data30
016100041008     C     *eur          move      data30        d1dSp
016200041008     c                   end
016300041008      *
016400041008     C                   clear                   AldSp             8 0
016500041008      *
016600041008     c                   if        d1dsp > 0
016700041008     C                   Z-ADD     d1Dsp         G02DAT
016800041008     C                   MOVEL     *BLANK        G02ERR
016900041008     C                   CALL      'XSRDA8'
017000041008     C                   PARM                    WLBDAT
017100041008     C     G02ERR        IFEQ      '1'
017200041008     C                   SETON                                        48  90
017300041008     C                   GOTO      endct1
017400041008     C                   ENDIF
017500041008     c*
017600041008     C                   Z-ADD     G02INV        AldSp
017700041008     C                   Z-ADD     G02dat        d1dsp
017800041008     c                   end
017900030129      *
018000030116     C     endct1        ENDSR
018100030904      *--------------------------------------------------------------*
