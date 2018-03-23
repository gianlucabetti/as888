000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500130822     FAZORG01L  IF   E           K DISK
000600130906     Ffieu93D   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200130906     D fieu94ds      e DS                  inz
001300130822     D OG143         E DS
001400040830     d  xgrp                  54     83    dim(10) descend
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
003000030825     C*
003100030120      *  In base all'utente prende il P.O.
003200030120     C                   CLEAR                   Tibs36Ds
003300030120     c                   EVAL      I36ute = Knmus
003400030120     c                   EVAL      I36Tla = 'L'
003500030120     C                   CALL      'TIBS36R'
003600030120     C                   PARM                    Tibs36Ds
003700030827     c                   movel     KNMUS         ALFA3             3
003800030829     c                   setoff                                       0102
003900030829      * solo per EDP....:
004000030829     c                   if        ALFA3 ='EDP'
004100030829     c                   seton                                        01
004200030829     c                   end
004300030829      * per utenti di filiale:
004400030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
004500030829     c                   seton                                        02
004600030829     c                   end
004700000000      *--------------------------------------------------------------*
004800000000      *             M A I N      L I N E
004900000000      *--------------------------------------------------------------*
005000000000     C                   Z-ADD     1             CODUT
005100000000     C                   CALL      'X§PARUT'
005200000000     C                   PARM                    UT§DSE
005300000000     C                   MOVEL     RAGUT         RSUT             20
005400000000     C                   MOVEL     RAGUT         RSUT2            20
005500000000     C                   MOVEL     REC80         CNCR80
005600040819      *
005700040819     C     INIZIO        tag
005800030825      *
005900040819     c                   clear                   D1NTW
006000030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
006100030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
006200130906     C                   Z-ADD     UDATE8        D1DWRT
006300040819     c                   clear                   D1Nfp
006400130910     C                   move      'S'           D1TSN                          S/N
006500040831     C                   move      'I'           w1grie
006600040831     C                   move      '001'         w1grp1
006700030915      *
006800000000      * Emetto il formato video
006900030116     C     Video1        TAG
007000020308     C                   SETOFF                                       90
007100130906     C                   EXFMT     eu93d01
007200000000      *
007300000000     C   KC              GOTO      FINE                                         F3=USCITA
007400000000      *
007500000000      * Eseguo i  controlli sui campi immessi a video
007600000000     C                   EXSR      CONTR
007700040830      *
007800040830      * Se non vi sono errori ma non e' premuto F6 emetto video
007900040830     C   90
008000040830     CorNkf              GOTO      Video1
008100040830      *
008200040830     C     Video2        TAG
008300040830     C                   SETOFF                                       90
008400000000      *
008500130906     C                   EXFMT     eu93w01
008600040830     C   KC              GOTO      FINE                                         F3=USCITA
008700040830     C   KL              GOTO      Video1                                       F3=USCITA
008800040830      *
008900020308      * Se non vi sono errori ma non e' premuto F6 emetto video
009000030915     C   90
009100040830     CorNkf              GOTO      Video2
009200030905      *
009300130906     c                   clear                   fieu94ds
009400030905      * compone il codice numerico
009500130906     c                   move      d1ntw         e94ntw
009600130906     c                   z-add     DalAmg        e94dti
009700130906     c                   z-add     AlAmg         e94dtf
009800130906     c                   z-add     d1DEL         e94del
009900130906     c                   move      d1nfp         e94nfp
010000130906     C                   move      d1tsn         e94Tsn                         S/N
010100130906     C                   move      w1grie        e94gie
010200130906     C                   move      w1grp1        e94gr1
010300130906     C                   move      w1grp2        e94gr2
010400130906     C                   move      w1grp3        e94gr3
010500130906     C                   move      w1grp4        e94gr4
010600130906     C                   move      w1grp5        e94gr5
010700130906     C                   move      w1grp6        e94gr6
010800130906     C                   move      w1grp7        e94gr7
010900130906     C                   move      w1grp8        e94gr8
011000130906     C                   move      w1grp9        e94gr9
011100130906     C                   move      w1grp0        e94gr0
011200040830     c                   sorta     xgrp
011300040819      *
011400040819      *  richiama il pgm che esegue il consolidamento
011500130906     c                   movel     fieu94ds      kpjbu
011600130906     c                   call      'FIEU94R'
011700040826     c                   parm                    kpjba
011800000000      *
011900130910     C***********        GOTO      INIZIO
012000030116      *
012100000000     C     FINE          TAG
012200000000     C                   SETON                                        LR
012300000000      *--------------------------------------------------------------*
012400000000      * Routine per i controlli sui campi del formato video          *
012500000000      *--------------------------------------------------------------*
012600000000     C     CONTR         BEGSR
012700000000      *
012800030116     c                   movea     '0000000000'  *in(41)
012900040819     c                   movea     '0000000000'  *in(51)
013000040819     C     d1ntw         ifeq      *blank
013100040819     C                   SETON                                        4190
013200040819     C                   GOTO      endct1
013300130822
013400130822     c                   else
013500130822      *
013600130822     c                   if        d1ntw <>'DPD' and d1ntw <>'FED'
013700130822      * controlla la LNP se network EDI estero
013800130822     c                   z-add     0             dove              3 0
013900130822     c     digits        check     d1ntw         dove
014000130822     c                   if        dove > 0
014100130822      * ci sono alfanumerici
014200130822     C                   SETON                                        4190
014300130822     C                   GOTO      endct1
014400130822     C                   Else
014500130822     c                   move      d1ntw         ntw_num3          3 0
014600130822     c     ntw_num3      chain     azorg01l
014700130822     c                   if        not %Found(azorg01l)
014800130822     C                   SETON                                        4190
014900130822     C                   GOTO      endct1
015000130822     C                   Else
015100130822     c                   eval      og143 = orgde3
015200130822     c                   if        §ogNTW <> 'EEX'
015300130822     C                   SETON                                        4190
015400130822     C                   GOTO      endct1
015500130822     c                   end
015600130822     c                   end
015700130822     C                   End
015800130822      *
015900130822     C                   End
016000040819     C                   End
016100030116      * --------------
016200020624      * controllo immissione delle date
016300030116     C                   Z-ADD     d1DDAL        G02DAT
016400000000     C                   MOVEL     *BLANK        G02ERR
016500940926     C                   CALL      'XSRDA8'
016600000000     C                   PARM                    WLBDAT
016700000000     C     G02ERR        IFEQ      '1'
016800020308     C                   SETON                                        44  90
016900030116     C                   GOTO      endct1
017000000000     C                   ENDIF
017100030116     C                   Z-ADD     G02INV        DalAmg            8 0
017200030116     C                   Z-ADD     G02dat        D1ddal
017300030825      *
017400020624      * Controllo data finale
017500030116     C     d1Dal         ifeq      0
017600030127     C                   Z-ADD     d1DDal        d1Dal
017700030127     C                   End
017800030825      *
017900030116     C                   Z-ADD     d1Dal         G02DAT
018000020624     C                   MOVEL     *BLANK        G02ERR
018100020624     C                   CALL      'XSRDA8'
018200020624     C                   PARM                    WLBDAT
018300020624     C     G02ERR        IFEQ      '1'
018400020624     C                   SETON                                        42  90
018500030116     C                   GOTO      endct1
018600020624     C                   ENDIF
018700030825     c*
018800030116     C                   Z-ADD     G02INV        AlAmG             8 0
018900030116     C                   Z-ADD     G02dat        d1dal
019000030127     c*
019100030116     C     DalAmg        ifgt      AlAmg
019200020624     C                   SETON                                        4590
019300030116     C                   GOTO      endct1
019400020624     C                   END
019500030129      *
019600130906      * --------------
019700130906      * controllo Data Immissione dei dati scritti sul sistema
019800130910     C     d1Dwrt        ifne      0
019900130906     C                   Z-ADD     d1Dwrt        G02DAT
020000130906     C                   MOVEL     *BLANK        G02ERR
020100130906     C                   CALL      'XSRDA8'
020200130906     C                   PARM                    WLBDAT
020300130906     C     G02ERR        IFEQ      '1'
020400130906     C                   SETON                                        49  90
020500130906     C                   GOTO      endct1
020600130906     C                   ENDIF
020700130906     C                   Z-ADD     G02INV        D1Del             8 0
020800130906     C                   Z-ADD     G02dat        D1dwrt
020900130910     c                   endif
021000040819      *
021100040819     c                   if        d1nfp = *all'0'
021200040819     c                   clear                   d1nfp
021300040819     c                   endif
021400040819      *
021500030116     C     endct1        ENDSR
021600030116      *--------------------------------------------------------------*
