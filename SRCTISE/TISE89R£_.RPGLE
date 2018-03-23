000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600040819     Ffieu82D   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200040819     D fieu82ds      e DS                  inz
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
005800040819     c                   clear                   D1NTW
005900030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
006000030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
006100040819     c                   clear                   D1Nfp
006200040819     C                   Z-ADD     *ZERO         D1Dini                         DATA INIZIALE
006300040819     C                   Z-ADD     *ZERO         D1Dinf                         DATA FINALE
006400041103     C                   move      'N'           D1TSN                          S/N
006500040819     C                   move      'N'           D1Dnin                         S/N
006600040819     C                   move      'N'           D1Dsin                         S/N
006700040831     C                   move      'I'           w1grie
006800040831     C                   move      '001'         w1grp1
006900030915      *
007000000000      * Emetto il formato video
007100030116     C     Video1        TAG
007200020308     C                   SETOFF                                       90
007300040819     C                   EXFMT     eu82d01
007400000000      *
007500000000     C   KC              GOTO      FINE                                         F3=USCITA
007600000000      *
007700000000      * Eseguo i  controlli sui campi immessi a video
007800000000     C                   EXSR      CONTR
007900040830      *
008000040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008100040830     C   90
008200040830     CorNkf              GOTO      Video1
008300040830      *
008400040830     C     Video2        TAG
008500040830     C                   SETOFF                                       90
008600000000      *
008700040830     C                   EXFMT     eu82w01
008800040830     C   KC              GOTO      FINE                                         F3=USCITA
008900040830     C   KL              GOTO      Video1                                       F3=USCITA
009000040830      *
009100020308      * Se non vi sono errori ma non e' premuto F6 emetto video
009200030915     C   90
009300040830     CorNkf              GOTO      Video2
009400030905      *
009500040819     c                   clear                   fieu82ds
009600030905      * compone il codice numerico
009700040819     c                   move      d1ntw         e82ntw
009800040819     c                   z-add     DalAmg        e82dti
009900040819     c                   z-add     AlAmg         e82dtf
010000040819     c                   move      d1nfp         e82nfp
010100040819     c                   z-add     DalAmg1       e82din
010200040819     c                   z-add     AlAmg1        e82dfi
010300040819     C                   move      d1tsn         e82Tsn                         S/N
010400040819     C                   move      D1Dnin        e82nin                         S/N
010500040819     C                   move      D1Dsin        e82sin                         S/N
010600040830     C                   move      w1grie        e82gie
010700040830     C                   move      w1grp1        e82gr1
010800040830     C                   move      w1grp2        e82gr2
010900040830     C                   move      w1grp3        e82gr3
011000040830     C                   move      w1grp4        e82gr4
011100040830     C                   move      w1grp5        e82gr5
011200040830     C                   move      w1grp6        e82gr6
011300040830     C                   move      w1grp7        e82gr7
011400040830     C                   move      w1grp8        e82gr8
011500040830     C                   move      w1grp9        e82gr9
011600040830     C                   move      w1grp0        e82gr0
011700040830     c                   sorta     xgrp
011800040819      *
011900040819      *  richiama il pgm che esegue il consolidamento
012000040819     c                   movel     fieu82ds      kpjbu
012100040826     c                   movel     'EU83'        kcoaz
012200040826     c     kcoaz         chain     kfazn11l
012300040826     c                   if        not %Found(kfazn11l)
012400040826     c                   call      'FIEU83R'
012500040826     c                   parm                    kpjba
012600040826     c                   else
012700041103     c                   eval      KJOST ='H'
012800041103     c                   if        ALFA3 ='EDP'
012900040819     c                   call      'BCH09'
013000040819     c                   parm                    kpjba
013100041103     c                   end
013200040819     c                   call      'BCH10'
013300040819     c                   parm                    kpjba
013400040826     c                   end
013500000000      *
013600030116     C                   GOTO      INIZIO
013700030116      *
013800000000     C     FINE          TAG
013900000000     C                   SETON                                        LR
014000000000      *--------------------------------------------------------------*
014100000000      * Routine per i controlli sui campi del formato video          *
014200000000      *--------------------------------------------------------------*
014300000000     C     CONTR         BEGSR
014400000000      *
014500030116     c                   movea     '0000000000'  *in(41)
014600040819     c                   movea     '0000000000'  *in(51)
014700040819     C     d1ntw         ifeq      *blank
014800040819     C                   SETON                                        4190
014900040819     C                   GOTO      endct1
015000040819     C                   End
015100030116      * --------------
015200020624      * controllo immissione delle date
015300030116     C                   Z-ADD     d1DDAL        G02DAT
015400000000     C                   MOVEL     *BLANK        G02ERR
015500940926     C                   CALL      'XSRDA8'
015600000000     C                   PARM                    WLBDAT
015700000000     C     G02ERR        IFEQ      '1'
015800020308     C                   SETON                                        44  90
015900030116     C                   GOTO      endct1
016000000000     C                   ENDIF
016100030116     C                   Z-ADD     G02INV        DalAmg            8 0
016200030116     C                   Z-ADD     G02dat        D1ddal
016300030825      *
016400020624      * Controllo data finale
016500030116     C     d1Dal         ifeq      0
016600030127     C                   Z-ADD     d1DDal        d1Dal
016700030127     C                   End
016800030825      *
016900030116     C                   Z-ADD     d1Dal         G02DAT
017000020624     C                   MOVEL     *BLANK        G02ERR
017100020624     C                   CALL      'XSRDA8'
017200020624     C                   PARM                    WLBDAT
017300020624     C     G02ERR        IFEQ      '1'
017400020624     C                   SETON                                        42  90
017500030116     C                   GOTO      endct1
017600020624     C                   ENDIF
017700030825     c*
017800030116     C                   Z-ADD     G02INV        AlAmG             8 0
017900030116     C                   Z-ADD     G02dat        d1dal
018000030127     c*
018100030116     C     DalAmg        ifgt      AlAmg
018200020624     C                   SETON                                        4590
018300030116     C                   GOTO      endct1
018400020624     C                   END
018500030129      *
018600040819      *
018700040819      * --------------
018800040819      * controllo immissione delle date
018900040819     C     d1Dini        ifne      0
019000040819     C     d1Dinf        orne      0
019100040819     C                   Z-ADD     d1Dini        G02DAT
019200040819     C                   MOVEL     *BLANK        G02ERR
019300040819     C                   CALL      'XSRDA8'
019400040819     C                   PARM                    WLBDAT
019500040819     C     G02ERR        IFEQ      '1'
019600040819     C                   SETON                                        54  90
019700040819     C                   GOTO      endct1
019800040819     C                   ENDIF
019900040819     C                   Z-ADD     G02INV        DalAmg1           8 0
020000040819     C                   Z-ADD     G02dat        D1dini
020100040819      *
020200040819      * Controllo data finale
020300040819     C     d1Dinf        ifeq      0
020400040819     C                   Z-ADD     d1Dini        d1dinf
020500040819     C                   End
020600040819      *
020700040819     C                   Z-ADD     d1Dinf        G02DAT
020800040819     C                   MOVEL     *BLANK        G02ERR
020900040819     C                   CALL      'XSRDA8'
021000040819     C                   PARM                    WLBDAT
021100040819     C     G02ERR        IFEQ      '1'
021200040819     C                   SETON                                        52  90
021300040819     C                   GOTO      endct1
021400040819     C                   ENDIF
021500040819     c*
021600040819     C                   Z-ADD     G02INV        AlAmG1            8 0
021700040819     C                   Z-ADD     G02dat        d1dinf
021800040819     c*
021900040819     C     DalAmg1       ifgt      AlAmg1
022000040819     C                   SETON                                        5590
022100040819     C                   GOTO      endct1
022200040819     C                   END
022300040819      *
022400040819     c                   endif
022500040819      *
022600040819     c                   if        d1nfp = *all'0'
022700040819     c                   clear                   d1nfp
022800040819     c                   endif
022900040819      *
023000040819      *
023100030116     C     endct1        ENDSR
023200030116      *--------------------------------------------------------------*
