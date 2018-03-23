000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300040819      *       prompt di Controllo fatture
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600160304     Ffieu64D   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200160304     D fieu64ds      e DS                  inz
001300940926     D*
001400000000     D WLBDAT          DS
001500940926     D  G02DAT                 1      8  0
001600940926     D  G02INV                 9     16  0
001700940926     D  G02ERR                17     17
001800940926     D  G02TGI                18     22  0
001900030205      *
002000030116     D digits          C                   CONST('0123456789')
002100030116      *****************************************************************
002200000000     C     *ENTRY        PLIST
002300000000     C                   PARM                    KPJBA
002400030120      *
002500030825     C* Imposta la UDATE  da TIME
002600030825     C                   TIME                    W0140            14 0
002700030825     C                   MOVE      W0140         UDATE8            8 0
002800030825     C*
002900030120      *  In base all'utente prende il P.O.
003000030120     C                   CLEAR                   Tibs36Ds
003100030120     c                   EVAL      I36ute = Knmus
003200030120     c                   EVAL      I36Tla = 'L'
003300030120     C                   CALL      'TIBS36R'
003400030120     C                   PARM                    Tibs36Ds
003500030827     c                   movel     KNMUS         ALFA3             3
003600030829     c                   setoff                                       0102
003700000000      *--------------------------------------------------------------*
003800000000      *             M A I N      L I N E
003900000000      *--------------------------------------------------------------*
004000000000     C                   Z-ADD     1             CODUT
004100000000     C                   CALL      'X§PARUT'
004200000000     C                   PARM                    UT§DSE
004300000000     C                   MOVEL     RAGUT         RSUT             20
004400000000     C                   MOVEL     REC80         CNCR80
004500040819      *
004600040819     C     INIZIO        tag
004700030825      *
004800160304     c                   movel     'DPD'         D1NTW
004900160923     c                   movel     'N'           D1BREC
005000160308     C                   Z-ADD     0             D1ANNO                         DATA INIZIALE
005100160308     C                   Z-ADD     0             D1MESE                         DATA FINALE
005200030915      *
005300000000      * Emetto il formato video
005400030116     C     Video1        TAG
005500160304     C                   SETOFF                                       99
005600160304     C                   EXFMT     eu64d01
005700000000      *
005800000000     C   KC              GOTO      FINE                                         F3=USCITA
005900000000      *
006000000000      * Eseguo i  controlli sui campi immessi a video
006100000000     C                   EXSR      CONTR
006200040830      *
006300040830      * Se non vi sono errori ma non e' premuto F6 emetto video
006400160304     C   99
006500040830     CorNkf              GOTO      Video1
006600030905      *
006700160304     c                   clear                   fieu64ds
006800030905      * compone il codice numerico
006900160304     c                   move      d1ntw         e64ntw
007000160304     c                   z-add     D1anno        e64anno
007100160304     c                   z-add     D1mese        e64mese
007200160923     c                   move      d1brec        e64brec
007300040819      *
007400040819      *  richiama il pgm che esegue il consolidamento
007500160304     c                   movel     fieu64ds      kpjbu
007600160304     c                   movel     'EU65'        kcoaz
007700040826     c     kcoaz         chain     kfazn11l
007800040826     c                   if        not %Found(kfazn11l)
007900160304     c                   call      'FIEU65R'
008000040826     c                   parm                    kpjba
008100040826     c                   else
008200041103     c                   eval      KJOST ='H'
008300041103     c                   if        ALFA3 ='EDP'
008400040819     c                   call      'BCH09'
008500040819     c                   parm                    kpjba
008600041103     c                   end
008700040819     c                   call      'BCH10'
008800040819     c                   parm                    kpjba
008900040826     c                   end
009000000000      *
009100030116     C                   GOTO      INIZIO
009200030116      *
009300000000     C     FINE          TAG
009400000000     C                   SETON                                        LR
009500000000      *--------------------------------------------------------------*
009600000000      * Routine per i controlli sui campi del formato video          *
009700000000      *--------------------------------------------------------------*
009800000000     C     CONTR         BEGSR
009900000000      *
010000040819      *
010100030116     C     endct1        ENDSR
010200030116      *--------------------------------------------------------------*
