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
001900170919      *
002000170919     D DATA_eur        S               D   DATFMT(*eur)
002100170919     D DATA_iso        S               D   DATFMT(*iso)
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
004000000000      *--------------------------------------------------------------*
004100000000      *             M A I N      L I N E
004200000000      *--------------------------------------------------------------*
004300000000     C                   Z-ADD     1             CODUT
004400000000     C                   CALL      'X§PARUT'
004500000000     C                   PARM                    UT§DSE
004600000000     C                   MOVEL     RAGUT         RSUT             20
004700000000     C                   MOVEL     REC80         CNCR80
004800040819      *
004900040819     C     INIZIO        tag
005000030825      *
005100160304     c                   movel     'DPD'         D1NTW
005200161207     c                   movel     'S'           sn001
005300161207     c                   movel     'S'           sn002
005400161207     c                   movel     'S'           sn003
005500161207     c                   movel     'S'           sn004
005600161207     c                   movel     'S'           sn005
005700161207     c                   movel     'S'           sn006
005800161207     c                   movel     'S'           sn007
005900161207     c                   movel     'S'           sn008
006000161207     c                   movel     'N'           sn009
006100161207     c                   movel     'N'           sn010
006200160308     C                   Z-ADD     0             D1ANNO                         DATA INIZIALE
006300160308     C                   Z-ADD     0             D1MESE                         DATA FINALE
006400030915      *
006500000000      * Emetto il formato video
006600030116     C     Video1        TAG
006700160304     C                   SETOFF                                       99
006800160304     C                   EXFMT     eu64d01
006900000000      *
007000000000     C   KC              GOTO      FINE                                         F3=USCITA
007100000000      *
007200000000      * Eseguo i  controlli sui campi immessi a video
007300000000     C                   EXSR      CONTR
007400040830      *
007500040830      * Se non vi sono errori ma non e' premuto F6 emetto video
007600160304     C   99
007700040830     CorNkf              GOTO      Video1
007800030905      *
007900160304     c                   clear                   fieu64ds
008000030905      * compone il codice numerico
008100160304     c                   move      d1ntw         e64ntw
008200160304     c                   z-add     D1anno        e64anno
008300160304     c                   z-add     D1mese        e64mese
008400161207     c                   move      sn001         e64sn01
008500161207     c                   move      sn002         e64sn02
008600161207     c                   move      sn003         e64sn03
008700161207     c                   move      sn004         e64sn04
008800161207     c                   move      sn005         e64sn05
008900161207     c                   move      sn006         e64sn06
009000161207     c                   move      sn007         e64sn07
009100161207     c                   move      sn008         e64sn08
009200161207     c                   move      sn009         e64sn09
009300161207     c                   move      sn010         e64sn10
009400040819      *
009500040819      *  richiama il pgm che esegue il consolidamento
009600160304     c                   movel     fieu64ds      kpjbu
009700160304     c                   movel     'EU65'        kcoaz
009800040826     c     kcoaz         chain     kfazn11l
009900040826     c                   if        not %Found(kfazn11l)
010000160304     c                   call      'FIEU65R'
010100040826     c                   parm                    kpjba
010200040826     c                   else
010300041103     c                   eval      KJOST ='H'
010400041103     c                   if        ALFA3 ='EDP'
010500040819     c                   call      'BCH09'
010600040819     c                   parm                    kpjba
010700041103     c                   end
010800040819     c                   call      'BCH10'
010900040819     c                   parm                    kpjba
011000040826     c                   end
011100000000      *
011200030116     C                   GOTO      INIZIO
011300030116      *
011400000000     C     FINE          TAG
011500000000     C                   SETON                                        LR
011600000000      *--------------------------------------------------------------*
011700000000      * Routine per i controlli sui campi del formato video          *
011800000000      *--------------------------------------------------------------*
011900000000     C     CONTR         BEGSR
012000170919      *
012100170920     c                   movel     *all'-'       anme7             7
012200170920     c                   movel     d1anno        anme7
012300170920     c                   move      d1mese        anme7
012400170920      *
012500170919     c                   movel     d1anno        annomese          6
012600170919     c                   move      d1mese        annomese
012700170919     C                   movel     annomese      dalAMG            8 0          DATA INIZIALE
012800170919     C                   move      '01'          dalAMG
012900170919     c     *iso          movel     DalAMG        DATA_iso
013000170919     c                   exsr      FINEmese
013100170919     C                   movel     DATA_iso      alAMG             8 0          DATA FINALE
013200000000      *
013300170919      **  Controlla se sta caricando lo stesso Clearing precedentemente caricato
013400170919     c                   clear                   gia_CARICATO      9 0
013500170919     C/EXEC SQL
013600170919     c+ SELECT count(*) into :GIA_CARICATO
013700170919     c+ FROM tnefr00f WHERE EFRNTW = 'DPD'
013800170919     c+ and EFRDFT between :dalAMG and :alAMG
013900170919     c+ and substr(EFRCAU, 1, 1) = 'N'
014000170919     c+ and substr( efrrif, 1, 14) in
014100170919     c+ ( SELECT substr( WFDPD1PC , 1, 14) FROM wfdpd1pc )
014200170919     C/END-EXEC
014300170920     c                   if        gia_CARICATO = 0
014400170920     C/EXEC SQL
014500170920     c+ SELECT count(*) into :GIA_CARICATO
014600170920     c+ FROM wfdpdba0f WHERE DBAYMO = :anme7
014700170920     C/END-EXEC
014800170920     c                   end
014900170919      *
015000170919      **   Controlla che ci sia già caricato il mese
015100170919     c                   if        gia_CARICATO > 0
015200170919     c                   seton                                        5599
015300170919     c                   goto      endCT1
015400170919     c                   end
015500040819      *
015600030116     C     endct1        ENDSR
015700170919      * ?------------------------------------------------------------------ */
015800170919      *    calcola il fine mese
015900170919      * ?------------------------------------------------------------------ */
016000170919     c     FINEMESE      BEGSR
016100170919      *
016200170919     C                   MOVEL     DATA_ISO      data08            8 0
016300170919     C                   MOVEL     Data08        aaaa04            4 0
016400170919     C                   move      '31'          data08
016500170919     C                   move      data08        mmgg04            4 0
016600170919     C                   movel     mmgg04        mm02              2 0
016700170919     c                   if        mm02=4 or mm02=6 or mm02=9 or mm02=11
016800170919     C                   move      '30'          data08
016900170919     c                   end
017000170919     c     aaaa04        div       4             risulta           3 0
017100170919     c                   mvr                     resto             3 0
017200170919     c                   if        mm02=2
017300170919     C                   move      '28'          data08
017400170919     c                   if        resto = 0
017500170919     C                   move      '29'          data08
017600170919     c                   end
017700170919     c                   end
017800170919     C     *iso          MOVEL     data08        DATA_ISO
017900170919      *
018000170919     c                   ENDSR
018100170919      * ?------------------------------------------------------------------ */
