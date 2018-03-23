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
004900161207     c                   movel     'S'           sn001
005000161207     c                   movel     'S'           sn002
005100161207     c                   movel     'S'           sn003
005200161207     c                   movel     'S'           sn004
005300161207     c                   movel     'S'           sn005
005400161207     c                   movel     'S'           sn006
005500161207     c                   movel     'S'           sn007
005600161207     c                   movel     'S'           sn008
005700161207     c                   movel     'N'           sn009
005800161207     c                   movel     'N'           sn010
005900160308     C                   Z-ADD     0             D1ANNO                         DATA INIZIALE
006000160308     C                   Z-ADD     0             D1MESE                         DATA FINALE
006100030915      *
006200000000      * Emetto il formato video
006300030116     C     Video1        TAG
006400160304     C                   SETOFF                                       99
006500160304     C                   EXFMT     eu64d01
006600000000      *
006700000000     C   KC              GOTO      FINE                                         F3=USCITA
006800000000      *
006900000000      * Eseguo i  controlli sui campi immessi a video
007000000000     C                   EXSR      CONTR
007100040830      *
007200040830      * Se non vi sono errori ma non e' premuto F6 emetto video
007300160304     C   99
007400040830     CorNkf              GOTO      Video1
007500030905      *
007600160304     c                   clear                   fieu64ds
007700030905      * compone il codice numerico
007800160304     c                   move      d1ntw         e64ntw
007900160304     c                   z-add     D1anno        e64anno
008000160304     c                   z-add     D1mese        e64mese
008100161207     c                   move      sn001         e64sn01
008200161207     c                   move      sn002         e64sn02
008300161207     c                   move      sn003         e64sn03
008400161207     c                   move      sn004         e64sn04
008500161207     c                   move      sn005         e64sn05
008600161207     c                   move      sn006         e64sn06
008700161207     c                   move      sn007         e64sn07
008800161207     c                   move      sn008         e64sn08
008900161207     c                   move      sn009         e64sn09
009000161207     c                   move      sn010         e64sn10
009100040819      *
009200040819      *  richiama il pgm che esegue il consolidamento
009300160304     c                   movel     fieu64ds      kpjbu
009400160304     c                   movel     'EU65'        kcoaz
009500040826     c     kcoaz         chain     kfazn11l
009600040826     c                   if        not %Found(kfazn11l)
009700160304     c                   call      'FIEU65R'
009800040826     c                   parm                    kpjba
009900040826     c                   else
010000041103     c                   eval      KJOST ='H'
010100041103     c                   if        ALFA3 ='EDP'
010200040819     c                   call      'BCH09'
010300040819     c                   parm                    kpjba
010400041103     c                   end
010500040819     c                   call      'BCH10'
010600040819     c                   parm                    kpjba
010700040826     c                   end
010800000000      *
010900030116     C                   GOTO      INIZIO
011000030116      *
011100000000     C     FINE          TAG
011200000000     C                   SETON                                        LR
011300000000      *--------------------------------------------------------------*
011400000000      * Routine per i controlli sui campi del formato video          *
011500000000      *--------------------------------------------------------------*
011600000000     C     CONTR         BEGSR
011700000000      *
011800040819      *
011900030116     C     endct1        ENDSR
012000030116      *--------------------------------------------------------------*
