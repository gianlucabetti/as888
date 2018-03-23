000100000000      *****************************************************************
000200040819      *  Consolida TNECF
000300000000      *****************************************************************
000400040819     H DECEDIT('0,') DATEDIT(*DMY.)
000500040819      *****************************************************************
000600040819     Ftnecf03l  uF   E           k disk
000700040903     Ftnefr03l  uF   E           k disk
000800030116      *****************************************************************
000900030116     d KPJBA         E DS
001000040819     D*
001100040819     D fieu88ds      e DS                  inz
001200940926     D*
001300000000     D WLBDAT          DS
001400940926     D  G02DAT                 1      8  0
001500940926     D  G02INV                 9     16  0
001600940926     D  G02ERR                17     17
001700940926     D  G02TGI                18     22  0
001800040819      *
001900040819     D  data_Oggi      s                   like(G02INV) INZ(0)
002000040923     D  data_0         s                   like(G02INV) INZ(0)
002100030205      *
002200030116     D digits          C                   CONST('0123456789')
002300030116      *****************************************************************
002400000000     C     *ENTRY        PLIST
002500000000     C                   PARM                    KPJBA
002600040819     C                   movel     Kpjbu         Fieu88ds
002700040819      *
002800040819      *  imposta UDATE di elaborazione
002900040819     C                   move      *date         G02DAT
003000040819     C                   MOVE      *ZEROS        G02INV
003100040819     C                   MOVE      *BLANKS       G02ERR
003200040819     C                   CALL      'XSRDA8'
003300040819     C                   PARM                    WLBDAT
003400040819     C                   Z-ADD     G02INV        data_oggi
003500040923      *
003600040923     C     x_iniziare    klist
003700040923     C                   kfld                    e88ntw
003800040923     C                   kfld                    data_0
003900041008      *
004000000000      *--------------------------------------------------------------*
004100000000      *             M A I N      L I N E
004200000000      *--------------------------------------------------------------*
004300030825      *
004400040923     C     x_iniziare    setgt     tnecf03l
004500040819     c     e88ntw        reade     tnecf03l
004600030116      *
004700040819      *  Fino alla data
004800040819     c                   doW       not %Eof(tnecf03l) and
004900040923     c                             ecfdfp <= e88dtf
005000041008      *  Data incasellamento
005100041012     c*******            if        ecfdta > 0
005200040819      *  Data consolidamento
005300040819     c                   eval      ecfdtg = data_oggi
005400040819     c                   update    tnecf000
005500041012     c*******            end
005600041008      *
005700040819     c     e88ntw        reade     tnecf03l
005800040819     c                   enddo
005900040819      *
006000041008      * x Spedizione non addebitate dal Partner
006100041008     C     x_iniziare    setll     tnecf03l
006200041008     c     x_iniziare    reade     tnecf03l
006300041008      *
006400041008      *  Fino alla data spedizione
006500041008     c                   doW       not %Eof(tnecf03l)
006600041008     c                   movel     ecfaas        datsped           8 0
006700041008     c                   move      ecfmgs        datsped
006800041013     c                   if        datsped <= e88dsp
006900041008      *  Data consolidamento
007000041008     c                   eval      ecfdtg = data_oggi
007100041008     c                   update    tnecf000
007200041013     c                   end
007300041008      *
007400041008     c     x_iniziare    reade     tnecf03l
007500041008     c                   enddo
007600041008      *
007700041013     C     x_iniziare    setgt     tnefr03l
007800040903     c     e88ntw        reade     tnefr03l
007900040903      *
008000040903      *  Fino alla data
008100040903     c                   doW       not %Eof(tnefr03l) and
008200040923     c                             efrdft <= e88dtf
008300040903      *  Data consolidamento
008400040903     c                   eval      efrdtc = data_oggi
008500040903     c                   update    tnefr000
008600040903     c     e88ntw        reade     tnefr03l
008700040903     c                   enddo
008800040903      *
008900000000     C     FINE          TAG
009000000000     C                   SETON                                        LR
009100000000      *--------------------------------------------------------------*
