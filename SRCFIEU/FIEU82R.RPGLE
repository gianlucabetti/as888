000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500130822     FAZORG01L  IF   E           K DISK
000600040826     fkfazn11l  if   E           k disk
000700040819     Ffieu82D   CF   E             WORKSTN
000800030116      *****************************************************************
000900030116     d KPJBA         E DS
001000030120     D Tibs36ds      E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300040819     D fieu82ds      e DS                  inz
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
006300040819     c                   clear                   D1Nfp
006400040819     C                   Z-ADD     *ZERO         D1Dini                         DATA INIZIALE
006500040819     C                   Z-ADD     *ZERO         D1Dinf                         DATA FINALE
006600041103     C                   move      'N'           D1TSN                          S/N
006700040819     C                   move      'N'           D1Dnin                         S/N
006800040819     C                   move      'N'           D1Dsin                         S/N
006900040831     C                   move      'I'           w1grie
007000040831     C                   move      '001'         w1grp1
007100030915      *
007200000000      * Emetto il formato video
007300030116     C     Video1        TAG
007400020308     C                   SETOFF                                       90
007500040819     C                   EXFMT     eu82d01
007600000000      *
007700000000     C   KC              GOTO      FINE                                         F3=USCITA
007800000000      *
007900000000      * Eseguo i  controlli sui campi immessi a video
008000000000     C                   EXSR      CONTR
008100040830      *
008200040830      * Se non vi sono errori ma non e' premuto F6 emetto video
008300040830     C   90
008400040830     CorNkf              GOTO      Video1
008500040830      *
008600040830     C     Video2        TAG
008700040830     C                   SETOFF                                       90
008800000000      *
008900040830     C                   EXFMT     eu82w01
009000040830     C   KC              GOTO      FINE                                         F3=USCITA
009100040830     C   KL              GOTO      Video1                                       F3=USCITA
009200040830      *
009300020308      * Se non vi sono errori ma non e' premuto F6 emetto video
009400030915     C   90
009500040830     CorNkf              GOTO      Video2
009600030905      *
009700040819     c                   clear                   fieu82ds
009800030905      * compone il codice numerico
009900040819     c                   move      d1ntw         e82ntw
010000040819     c                   z-add     DalAmg        e82dti
010100040819     c                   z-add     AlAmg         e82dtf
010200040819     c                   move      d1nfp         e82nfp
010300040819     c                   z-add     DalAmg1       e82din
010400040819     c                   z-add     AlAmg1        e82dfi
010500040819     C                   move      d1tsn         e82Tsn                         S/N
010600040819     C                   move      D1Dnin        e82nin                         S/N
010700040819     C                   move      D1Dsin        e82sin                         S/N
010800040830     C                   move      w1grie        e82gie
010900040830     C                   move      w1grp1        e82gr1
011000040830     C                   move      w1grp2        e82gr2
011100040830     C                   move      w1grp3        e82gr3
011200040830     C                   move      w1grp4        e82gr4
011300040830     C                   move      w1grp5        e82gr5
011400040830     C                   move      w1grp6        e82gr6
011500040830     C                   move      w1grp7        e82gr7
011600040830     C                   move      w1grp8        e82gr8
011700040830     C                   move      w1grp9        e82gr9
011800040830     C                   move      w1grp0        e82gr0
011900040830     c                   sorta     xgrp
012000040819      *
012100040819      *  richiama il pgm che esegue il consolidamento
012200040819     c                   movel     fieu82ds      kpjbu
012300040826     c                   movel     'EU83'        kcoaz
012400040826     c     kcoaz         chain     kfazn11l
012500040826     c                   if        not %Found(kfazn11l)
012600040826     c                   call      'FIEU83R'
012700040826     c                   parm                    kpjba
012800040826     c                   else
012900041103     c                   eval      KJOST ='H'
013000041103     c                   if        ALFA3 ='EDP'
013100040819     c                   call      'BCH09'
013200040819     c                   parm                    kpjba
013300041103     c                   end
013400040819     c                   call      'BCH10'
013500040819     c                   parm                    kpjba
013600040826     c                   end
013700000000      *
013800030116     C                   GOTO      INIZIO
013900030116      *
014000000000     C     FINE          TAG
014100000000     C                   SETON                                        LR
014200000000      *--------------------------------------------------------------*
014300000000      * Routine per i controlli sui campi del formato video          *
014400000000      *--------------------------------------------------------------*
014500000000     C     CONTR         BEGSR
014600000000      *
014700030116     c                   movea     '0000000000'  *in(41)
014800040819     c                   movea     '0000000000'  *in(51)
014900040819     C     d1ntw         ifeq      *blank
015000040819     C                   SETON                                        4190
015100040819     C                   GOTO      endct1
015200130822
015300130822     c                   else
015400130822      *
015500130822     c                   if        d1ntw <>'DPD' and d1ntw <>'FED'
015600130822      * controlla la LNP se network EDI estero
015700130822     c                   z-add     0             dove              3 0
015800130822     c     digits        check     d1ntw         dove
015900130822     c                   if        dove > 0
016000130822      * ci sono alfanumerici
016100130822     C                   SETON                                        4190
016200130822     C                   GOTO      endct1
016300130822     C                   Else
016400130822     c                   move      d1ntw         ntw_num3          3 0
016500130822     c     ntw_num3      chain     azorg01l
016600130822     c                   if        not %Found(azorg01l)
016700130822     C                   SETON                                        4190
016800130822     C                   GOTO      endct1
016900130822     C                   Else
017000130822     c                   eval      og143 = orgde3
017100130822     c                   if        §ogNTW <> 'EEX'
017200130822     C                   SETON                                        4190
017300130822     C                   GOTO      endct1
017400130822     c                   end
017500130822     c                   end
017600130822     C                   End
017700130822      *
017800130822     C                   End
017900040819     C                   End
018000030116      * --------------
018100020624      * controllo immissione delle date
018200030116     C                   Z-ADD     d1DDAL        G02DAT
018300000000     C                   MOVEL     *BLANK        G02ERR
018400940926     C                   CALL      'XSRDA8'
018500000000     C                   PARM                    WLBDAT
018600000000     C     G02ERR        IFEQ      '1'
018700020308     C                   SETON                                        44  90
018800030116     C                   GOTO      endct1
018900000000     C                   ENDIF
019000030116     C                   Z-ADD     G02INV        DalAmg            8 0
019100030116     C                   Z-ADD     G02dat        D1ddal
019200030825      *
019300020624      * Controllo data finale
019400030116     C     d1Dal         ifeq      0
019500030127     C                   Z-ADD     d1DDal        d1Dal
019600030127     C                   End
019700030825      *
019800030116     C                   Z-ADD     d1Dal         G02DAT
019900020624     C                   MOVEL     *BLANK        G02ERR
020000020624     C                   CALL      'XSRDA8'
020100020624     C                   PARM                    WLBDAT
020200020624     C     G02ERR        IFEQ      '1'
020300020624     C                   SETON                                        42  90
020400030116     C                   GOTO      endct1
020500020624     C                   ENDIF
020600030825     c*
020700030116     C                   Z-ADD     G02INV        AlAmG             8 0
020800030116     C                   Z-ADD     G02dat        d1dal
020900030127     c*
021000030116     C     DalAmg        ifgt      AlAmg
021100020624     C                   SETON                                        4590
021200030116     C                   GOTO      endct1
021300020624     C                   END
021400030129      *
021500040819      *
021600040819      * --------------
021700040819      * controllo immissione delle date
021800040819     C     d1Dini        ifne      0
021900040819     C     d1Dinf        orne      0
022000040819     C                   Z-ADD     d1Dini        G02DAT
022100040819     C                   MOVEL     *BLANK        G02ERR
022200040819     C                   CALL      'XSRDA8'
022300040819     C                   PARM                    WLBDAT
022400040819     C     G02ERR        IFEQ      '1'
022500040819     C                   SETON                                        54  90
022600040819     C                   GOTO      endct1
022700040819     C                   ENDIF
022800040819     C                   Z-ADD     G02INV        DalAmg1           8 0
022900040819     C                   Z-ADD     G02dat        D1dini
023000040819      *
023100040819      * Controllo data finale
023200040819     C     d1Dinf        ifeq      0
023300040819     C                   Z-ADD     d1Dini        d1dinf
023400040819     C                   End
023500040819      *
023600040819     C                   Z-ADD     d1Dinf        G02DAT
023700040819     C                   MOVEL     *BLANK        G02ERR
023800040819     C                   CALL      'XSRDA8'
023900040819     C                   PARM                    WLBDAT
024000040819     C     G02ERR        IFEQ      '1'
024100040819     C                   SETON                                        52  90
024200040819     C                   GOTO      endct1
024300040819     C                   ENDIF
024400040819     c*
024500040819     C                   Z-ADD     G02INV        AlAmG1            8 0
024600040819     C                   Z-ADD     G02dat        d1dinf
024700040819     c*
024800040819     C     DalAmg1       ifgt      AlAmg1
024900040819     C                   SETON                                        5590
025000040819     C                   GOTO      endct1
025100040819     C                   END
025200040819      *
025300040819     c                   endif
025400040819      *
025500040819     c                   if        d1nfp = *all'0'
025600040819     c                   clear                   d1nfp
025700040819     c                   endif
025800040819      *
025900040819      *
026000030116     C     endct1        ENDSR
026100030116      *--------------------------------------------------------------*
