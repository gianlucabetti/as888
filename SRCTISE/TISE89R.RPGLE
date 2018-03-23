000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300120105      * prompt di lancio CONTROLLO su chi sta eseguendo ATR in Filiale
000400120105      *   si analizza un periodo di max 2 mesi indietro.
000500120105      *   Ci si basa sul confronto fatto fra TITAS e TNEVB.
000600120105      *   si confrontano le bolle export delle enne LINEE e per giornata
000700120105      *   si comparano gli eventi di ATR registrati sul nostro sistema
000800120105      *   CHI NON FA l'ATR non scrive gli eventi ATR quindi si nota subito
000900120105      * < tante bolle........ nessun evento registrato su quelle bolle.>
001000000000      *****************************************************************
001100120105     ftabel00F  if   E           k disk
001200120105     fkfazn11l  if   E           k disk
001300111114     Ftise89D   CF   E             WORKSTN
001400030116      *****************************************************************
001500111114     D tise89ds      e DS                  inz
001600030116     d KPJBA         E DS
001700030120     D Tibs36ds      E DS
001800000000     D CNCR80        E DS
001900000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002000040830     d  xgrp                  54     83    dim(10) descend
002100120105     D*
002200120105     D DS$3          E DS
002300940926     D*
002400000000     D WLBDAT          DS
002500940926     D  G02DAT                 1      8  0
002600940926     D  G02INV                 9     16  0
002700940926     D  G02ERR                17     17
002800940926     D  G02TGI                18     22  0
002900030205      *
003000030116     D digits          C                   CONST('0123456789')
003100030116      *****************************************************************
003200000000     C     *ENTRY        PLIST
003300000000     C                   PARM                    KPJBA
003400120105     C*
003500120105     C     KTAB          KLIST
003600120105     C                   KFLD                    TBLKUT
003700120105     C                   KFLD                    TBLCOD
003800120105     C                   KFLD                    TBLKEY
003900120105      *
004000030120      *
004100030825     C* Imposta la UDATE  da TIME
004200030825     C                   TIME                    W0140            14 0
004300030825     C                   MOVE      W0140         UDATE8            8 0
004400030825     C*
004500030120      *  In base all'utente prende il P.O.
004600030120     C                   CLEAR                   Tibs36Ds
004700030120     c                   EVAL      I36ute = Knmus
004800030120     c                   EVAL      I36Tla = 'L'
004900030120     C                   CALL      'TIBS36R'
005000030120     C                   PARM                    Tibs36Ds
005100030827     c                   movel     KNMUS         ALFA3             3
005200030829     c                   setoff                                       0102
005300030829      * solo per EDP....:
005400030829     c                   if        ALFA3 ='EDP'
005500030829     c                   seton                                        01
005600030829     c                   end
005700030829      * per utenti di filiale:
005800030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
005900030829     c                   seton                                        02
006000030829     c                   end
006100000000      *--------------------------------------------------------------*
006200000000      *             M A I N      L I N E
006300000000      *--------------------------------------------------------------*
006400000000     C                   Z-ADD     1             CODUT
006500000000     C                   CALL      'X§PARUT'
006600000000     C                   PARM                    UT§DSE
006700000000     C                   MOVEL     RAGUT         RSUT             20
006800000000     C                   MOVEL     RAGUT         RSUT2            20
006900000000     C                   MOVEL     REC80         CNCR80
007000120105     C                   Z-ADD     CODUT         tblkut
007100120105     C                   MOVEL     '$3'          tblCOD
007200120105     C                   eval       tblKEY = '1'
007300120105     C     KTAB          CHAIN     TABEL00F                           30
007400120105     c                   clear                   ds$3
007500120105     c                   if        %Found(TABEL00F)
007600120105     c                   eval      ds$3 = tblUNI
007700120105     c                   move      §$3D00        Data_limit        8 0
007800120105     c                   move      '01'          Data_limit
007900120105     c                   end
008000040819      *
008100040819     C     INIZIO        tag
008200030825      *
008300030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
008400030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
008500030915      *
008600000000      * Emetto il formato video
008700030116     C     Video1        TAG
008800020308     C                   SETOFF                                       90
008900111114     C                   EXFMT     eu89d01
009000000000      *
009100000000     C   KC              GOTO      FINE                                         F3=USCITA
009200000000      *
009300000000      * Eseguo i  controlli sui campi immessi a video
009400000000     C                   EXSR      CONTR
009500040830      *
009600040830      * Se non vi sono errori ma non e' premuto F6 emetto video
009700040830     C   90
009800040830     CorNkf              GOTO      Video1
009900040830      *
010000111115      *  Pulisce il file di work
010100111115     c                   call      'TISE89C'
010200111115      *
010300111114     c                   clear                   tise89ds
010400030905      * compone il codice numerico
010500111114     c                   z-add     DalAmg        e89dti
010600111114     c                   z-add     AlAmg         e89dtf
010700111114     c                   movel     tise89ds      kpjbu
010800111114     c                   movel     'SE8A'        kcoaz
010900040826     c     kcoaz         chain     kfazn11l
011000040826     c                   if        not %Found(kfazn11l)
011100111115      *  Sottomette la richiesta
011200111114     c                   call      'TISE89R1'
011300040826     c                   parm                    kpjba
011400040826     c                   else
011500041103     c                   eval      KJOST ='H'
011600041103     c                   if        ALFA3 ='EDP'
011700040819     c                   call      'BCH09'
011800040819     c                   parm                    kpjba
011900041103     c                   end
012000040819     c                   call      'BCH10'
012100040819     c                   parm                    kpjba
012200040826     c                   end
012300000000      *
012400030116     C                   GOTO      INIZIO
012500030116      *
012600000000     C     FINE          TAG
012700000000     C                   SETON                                        LR
012800000000      *--------------------------------------------------------------*
012900000000      * Routine per i controlli sui campi del formato video          *
013000000000      *--------------------------------------------------------------*
013100000000     C     CONTR         BEGSR
013200000000      *
013300030116     c                   movea     '0000000000'  *in(41)
013400040819     c                   movea     '0000000000'  *in(51)
013500030116      * --------------
013600020624      * controllo immissione delle date
013700030116     C                   Z-ADD     d1DDAL        G02DAT
013800000000     C                   MOVEL     *BLANK        G02ERR
013900940926     C                   CALL      'XSRDA8'
014000000000     C                   PARM                    WLBDAT
014100000000     C     G02ERR        IFEQ      '1'
014200020308     C                   SETON                                        44  90
014300030116     C                   GOTO      endct1
014400000000     C                   ENDIF
014500030116     C                   Z-ADD     G02INV        DalAmg            8 0
014600030116     C                   Z-ADD     G02dat        D1ddal
014700030825      *
014800020624      * Controllo data finale
014900030116     C     d1Dal         ifeq      0
015000030127     C                   Z-ADD     d1DDal        d1Dal
015100030127     C                   End
015200030825      *
015300030116     C                   Z-ADD     d1Dal         G02DAT
015400020624     C                   MOVEL     *BLANK        G02ERR
015500020624     C                   CALL      'XSRDA8'
015600020624     C                   PARM                    WLBDAT
015700020624     C     G02ERR        IFEQ      '1'
015800020624     C                   SETON                                        42  90
015900030116     C                   GOTO      endct1
016000020624     C                   ENDIF
016100030825     c*
016200030116     C                   Z-ADD     G02INV        AlAmG             8 0
016300030116     C                   Z-ADD     G02dat        d1dal
016400030127     c*
016500120105     C     DalAmg        ifgt      AlAmg
016600020624     C                   SETON                                        4590
016700030116     C                   GOTO      endct1
016800020624     C                   END
016900030129      *
017000120105     c*
017100120105     C     DalAmg        iflt      data_limit
017200120105     C                   SETON                                        4690
017300120105     C                   GOTO      endct1
017400120105     C                   END
017500040819      *
017600040819      * --------------
017700040819      *
017800030116     C     endct1        ENDSR
017900030116      *--------------------------------------------------------------*
