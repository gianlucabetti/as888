000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500130822     FAZORG01L  IF   E           K DISK
000600040826     fkfazn11l  if   E           k disk
000700130906     Ffieu93D   CF   E             WORKSTN
000800030116      *****************************************************************
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300130906     D fieu94ds      e DS                  inz
001400130822     D OG143         E DS
001500040830     d  xgrp                  54     83    dim(10) descend
001600940926     D*
001700000000     D WLBDAT          DS
001800940926     D  G02DAT                 1      8  0
001900940926     D  G02INV                 9     16  0
002000940926     D  G02ERR                17     17
002100940926     D  G02TGI                18     22  0
002200030205      *
002300030116     D digits          C                   CONST('0123456789')
002400030116      *****************************************************************
002500000000     C     *ENTRY        PLIST
002600000000     C                   PARM                    KPJBA
002700030120      *
002800030825     C* Imposta la UDATE  da TIME
002900030825     C                   TIME                    W0140            14 0
003000030825     C                   MOVE      W0140         UDATE8            8 0
003100030825     C*
003200030120      *  In base all'utente prende il P.O.
003300030120     C                   CLEAR                   Tibs36Ds
003400030120     c                   EVAL      I36ute = Knmus
003500030120     c                   EVAL      I36Tla = 'L'
003600030120     C                   CALL      'TIBS36R'
003700030120     C                   PARM                    Tibs36Ds
003800030827     c                   movel     KNMUS         ALFA3             3
003900030829     c                   setoff                                       0102
004000030829      * solo per EDP....:
004100030829     c                   if        ALFA3 ='EDP'
004200030829     c                   seton                                        01
004300030829     c                   end
004400030829      * per utenti di filiale:
004500030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
004600030829     c                   seton                                        02
004700030829     c                   end
004800000000      *--------------------------------------------------------------*
004900000000      *             M A I N      L I N E
005000000000      *--------------------------------------------------------------*
005100000000     C                   Z-ADD     1             CODUT
005200000000     C                   CALL      'X§PARUT'
005300000000     C                   PARM                    UT§DSE
005400000000     C                   MOVEL     RAGUT         RSUT             20
005500000000     C                   MOVEL     RAGUT         RSUT2            20
005600000000     C                   MOVEL     REC80         CNCR80
005700040819      *
005800040819     C     INIZIO        tag
005900030825      *
006000040819     c                   clear                   D1NTW
006100030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
006200030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
006300130906     C                   Z-ADD     UDATE8        D1DWRT
006400040819     c                   clear                   D1Nfp
006500041103     C                   move      'N'           D1TSN                          S/N
006600040831     C                   move      'I'           w1grie
006700040831     C                   move      '001'         w1grp1
006800030915      *
006900000000      * Emetto il formato video
007000030116     C     Video1        TAG
007100020308     C                   SETOFF                                       90
007200130906     C                   EXFMT     eu93d01
007300000000      *
007400000000     C   KC              GOTO      FINE                                         F3=USCITA
007500000000      *
007600000000      * Eseguo i  controlli sui campi immessi a video
007700000000     C                   EXSR      CONTR
007800040830      *
007900040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008000040830     C   90
008100040830     CorNkf              GOTO      Video1
008200040830      *
008300040830     C     Video2        TAG
008400040830     C                   SETOFF                                       90
008500000000      *
008600130906     C                   EXFMT     eu93w01
008700040830     C   KC              GOTO      FINE                                         F3=USCITA
008800040830     C   KL              GOTO      Video1                                       F3=USCITA
008900040830      *
009000020308      * Se non vi sono errori ma non e' premuto F6 emetto video
009100030915     C   90
009200040830     CorNkf              GOTO      Video2
009300030905      *
009400130906     c                   clear                   fieu94ds
009500030905      * compone il codice numerico
009600130906     c                   move      d1ntw         e94ntw
009700130906     c                   z-add     DalAmg        e94dti
009800130906     c                   z-add     AlAmg         e94dtf
009900130906     c                   z-add     d1DEL         e94del
010000130906     c                   move      d1nfp         e94nfp
010100130906     C                   move      d1tsn         e94Tsn                         S/N
010200130906     C                   move      w1grie        e94gie
010300130906     C                   move      w1grp1        e94gr1
010400130906     C                   move      w1grp2        e94gr2
010500130906     C                   move      w1grp3        e94gr3
010600130906     C                   move      w1grp4        e94gr4
010700130906     C                   move      w1grp5        e94gr5
010800130906     C                   move      w1grp6        e94gr6
010900130906     C                   move      w1grp7        e94gr7
011000130906     C                   move      w1grp8        e94gr8
011100130906     C                   move      w1grp9        e94gr9
011200130906     C                   move      w1grp0        e94gr0
011300040830     c                   sorta     xgrp
011400040819      *
011500040819      *  richiama il pgm che esegue il consolidamento
011600130906     c                   movel     fieu94ds      kpjbu
011700130906     c                   movel     'EU94'        kcoaz
011800040826     c     kcoaz         chain     kfazn11l
011900040826     c                   if        not %Found(kfazn11l)
012000130906     c                   call      'FIEU94R'
012100040826     c                   parm                    kpjba
012200040826     c                   else
012300041103     c                   eval      KJOST ='H'
012400041103     c                   if        ALFA3 ='EDP'
012500040819     c                   call      'BCH09'
012600040819     c                   parm                    kpjba
012700041103     c                   end
012800040819     c                   call      'BCH10'
012900040819     c                   parm                    kpjba
013000040826     c                   end
013100000000      *
013200030116     C                   GOTO      INIZIO
013300030116      *
013400000000     C     FINE          TAG
013500000000     C                   SETON                                        LR
013600000000      *--------------------------------------------------------------*
013700000000      * Routine per i controlli sui campi del formato video          *
013800000000      *--------------------------------------------------------------*
013900000000     C     CONTR         BEGSR
014000000000      *
014100030116     c                   movea     '0000000000'  *in(41)
014200040819     c                   movea     '0000000000'  *in(51)
014300040819     C     d1ntw         ifeq      *blank
014400040819     C                   SETON                                        4190
014500040819     C                   GOTO      endct1
014600130822
014700130822     c                   else
014800130822      *
014900130822     c                   if        d1ntw <>'DPD' and d1ntw <>'FED'
015000130822      * controlla la LNP se network EDI estero
015100130822     c                   z-add     0             dove              3 0
015200130822     c     digits        check     d1ntw         dove
015300130822     c                   if        dove > 0
015400130822      * ci sono alfanumerici
015500130822     C                   SETON                                        4190
015600130822     C                   GOTO      endct1
015700130822     C                   Else
015800130822     c                   move      d1ntw         ntw_num3          3 0
015900130822     c     ntw_num3      chain     azorg01l
016000130822     c                   if        not %Found(azorg01l)
016100130822     C                   SETON                                        4190
016200130822     C                   GOTO      endct1
016300130822     C                   Else
016400130822     c                   eval      og143 = orgde3
016500130822     c                   if        §ogNTW <> 'EEX'
016600130822     C                   SETON                                        4190
016700130822     C                   GOTO      endct1
016800130822     c                   end
016900130822     c                   end
017000130822     C                   End
017100130822      *
017200130822     C                   End
017300040819     C                   End
017400030116      * --------------
017500020624      * controllo immissione delle date
017600030116     C                   Z-ADD     d1DDAL        G02DAT
017700000000     C                   MOVEL     *BLANK        G02ERR
017800940926     C                   CALL      'XSRDA8'
017900000000     C                   PARM                    WLBDAT
018000000000     C     G02ERR        IFEQ      '1'
018100020308     C                   SETON                                        44  90
018200030116     C                   GOTO      endct1
018300000000     C                   ENDIF
018400030116     C                   Z-ADD     G02INV        DalAmg            8 0
018500030116     C                   Z-ADD     G02dat        D1ddal
018600030825      *
018700020624      * Controllo data finale
018800030116     C     d1Dal         ifeq      0
018900030127     C                   Z-ADD     d1DDal        d1Dal
019000030127     C                   End
019100030825      *
019200030116     C                   Z-ADD     d1Dal         G02DAT
019300020624     C                   MOVEL     *BLANK        G02ERR
019400020624     C                   CALL      'XSRDA8'
019500020624     C                   PARM                    WLBDAT
019600020624     C     G02ERR        IFEQ      '1'
019700020624     C                   SETON                                        42  90
019800030116     C                   GOTO      endct1
019900020624     C                   ENDIF
020000030825     c*
020100030116     C                   Z-ADD     G02INV        AlAmG             8 0
020200030116     C                   Z-ADD     G02dat        d1dal
020300030127     c*
020400030116     C     DalAmg        ifgt      AlAmg
020500020624     C                   SETON                                        4590
020600030116     C                   GOTO      endct1
020700020624     C                   END
020800030129      *
020900130906      * --------------
021000130906      * controllo Data Immissione dei dati scritti sul sistema
021100130906     C                   Z-ADD     d1Dwrt        G02DAT
021200130906     C                   MOVEL     *BLANK        G02ERR
021300130906     C                   CALL      'XSRDA8'
021400130906     C                   PARM                    WLBDAT
021500130906     C     G02ERR        IFEQ      '1'
021600130906     C                   SETON                                        49  90
021700130906     C                   GOTO      endct1
021800130906     C                   ENDIF
021900130906     C                   Z-ADD     G02INV        D1Del             8 0
022000130906     C                   Z-ADD     G02dat        D1dwrt
022100130906      *
022200040819      *
022300040819     c                   if        d1nfp = *all'0'
022400040819     c                   clear                   d1nfp
022500040819     c                   endif
022600040819      *
022700040819      *
022800030116     C     endct1        ENDSR
022900030116      *--------------------------------------------------------------*
