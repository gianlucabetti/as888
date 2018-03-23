000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030904      *       prompt di controllo dati ricevuti dal partner
000400000000      *****************************************************************
000500130822     FAZORG01L  IF   E           K DISK
000600040901     fkfazn11l  if   E           k disk
000700130717     Ftnefr00F  iF   E             disk
000800040901     Ffieu80D   CF   E             WORKSTN
000900030116      *****************************************************************
001000030116     d KPJBA         E DS
001100030120     D Tibs36ds      E DS
001200000000     D CNCR80        E DS
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400040901     D fieu80ds      e DS                  inz
001500130822     D OG143         E DS
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
005700030825      *
005800030915     C     INIZIO        tag
005900040901      *
006000040901     c                   move      '   '         D1NTW
006100041104     C                   MOVE      UDATE8        d1alfp
006200030915      *
006300000000      * Emetto il formato video
006400030116     C     Video1        TAG
006500040901     C                   SETOFF                                       99
006600040901     C                   EXFMT     eu80d01
006700000000      *
006800000000     C   KC              GOTO      FINE                                         F3=USCITA
006900000000      *
007000000000      * Eseguo i  controlli sui campi immessi a video
007100000000     C                   EXSR      CONTR
007200000000      *
007300020308      * Se non vi sono errori ma non e' premuto F6 emetto video
007400040901     C   99
007500040901     CorNkf              GOTO      Video1
007600000000      *
007700040901      *  richiama il pgm che esegue il consolidamento
007800040901     c                   movel     d1ntw         kpjbu
007900041104     c                   eval      %subst(kpjbu:4:8)=%char(alAMG)
008000040901     c                   movel     'EU79'        kcoaz
008100040901     c     kcoaz         chain     kfazn11l
008200040901     c                   if        not %Found(kfazn11l)
008300040901     c                   call      'FIEU79R'
008400040901     c                   parm                    kpjba
008500040901     c                   else
008600041103     c                   if        ALFA3 ='EDP'
008700040901     c                   call      'BCH09'
008800040901     c                   parm                    kpjba
008900041103     c                   end
009000040901     c                   call      'BCH10'
009100040901     c                   parm                    kpjba
009200040901     c                   end
009300040901      *
009400030116     C                   GOTO      INIZIO
009500030116      *
009600000000     C     FINE          TAG
009700000000     C                   SETON                                        LR
009800000000      *--------------------------------------------------------------*
009900000000      * Routine per i controlli sui campi del formato video          *
010000000000      *--------------------------------------------------------------*
010100000000     C     CONTR         BEGSR
010200000000      *
010300030116     c                   movea     '0000000000'  *in(41)
010400040901     c                   setoff                                       33
010500040901     c                   if        d1ntw = *blank
010600040901     c                   seton                                        9941
010700130822     C                   GOTO      endct1
010800130822     c                   else
010900130822      *
011000130822     c                   if        d1ntw <>'DPD' and d1ntw <>'FED'
011100130822      * controlla la LNP se network EDI estero
011200130822     c                   z-add     0             dove              3 0
011300130822     c     digits        check     d1ntw         dove
011400130822     c                   if        dove > 0
011500130822      * ci sono alfanumerici
011600130822     C                   SETON                                        4199
011700130822     C                   GOTO      endct1
011800130822     C                   Else
011900130822     c                   move      d1ntw         ntw_num3          3 0
012000130822     c     ntw_num3      chain     azorg01l
012100130822     c                   if        not %Found(azorg01l)
012200130822     C                   SETON                                        4199
012300130822     C                   GOTO      endct1
012400130822     C                   Else
012500130822     c                   eval      og143 = orgde3
012600130822     c                   if        §ogNTW <> 'EEX'
012700130822     C                   SETON                                        4199
012800130822     C                   GOTO      endct1
012900130822     c                   end
013000130822     c                   end
013100130822     C                   End
013200130822      *
013300130822     C                   End
013400130822      *
013500130822     c                   end
013600040901      *
013700041104     C                   Z-ADD     d1alfp        G02DAT
013800041104     C                   MOVEL     *BLANK        G02ERR
013900041104     C                   CALL      'XSRDA8'
014000041104     C                   PARM                    WLBDAT
014100041104     C     G02ERR        IFEQ      '1'
014200041104     C                   SETON                                        9942
014300041104     C                   GOTO      endct1
014400041104     C                   ENDIF
014500041104     c*
014600041104     C                   Z-ADD     G02INV        AlAmG             8 0
014700041104     C                   Z-ADD     G02dat        d1alfp
014800040901      *
014900040901     c                   exsr      conta
015000030116      * --------------
015100030129      *
015200030116     C     endct1        ENDSR
015300030116      *--------------------------------------------------------------*
015400030904      * conta
015500030904      *--------------------------------------------------------------*
015600030904     C     Conta         BEGSR
015700030904      *
015800030904     c                   z-add     0             cnt               7 0
015900030904     C/EXEC SQL
016000030904     C+ Declare A1 Cursor for
016100130717     c+ SELECT efrgrp, count(*) from tnefr00f
016200040901     c+ WHERE EFRNTW = :d1ntw  and efrgrp = '999'
016300040901     c+ GROUP BY EFRgrp
016400030904     C/END-EXEC
016500030904      *          apertura cursore
016600030904     C/EXEC SQL
016700030904     C+ OPEN A1
016800030904     C/END-EXEC
016900030904     C                   if        SqlCod <> 100 OR SqlCod = 0
017000030904      *          lettura cursore
017100030904     C/EXEC SQL
017200040901     C+ Fetch Next From A1 Into :efrgrp, :cnt
017300030904     C/END-EXEC
017400040901     C                   if        cnt > 0
017500040901     C                   seton                                        33  99
017600040901     C                   End
017700040901     C                   End
017800030904     C/EXEC SQL
017900030904     C+ Close A1
018000030904     C/END-EXEC
018100030915     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
018200030904     C                   ENDSR
018300030904      *--------------------------------------------------------------*
