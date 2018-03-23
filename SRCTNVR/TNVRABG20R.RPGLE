000100940224     H*--------------------------------------------------------------*
000200150331     FTITAS30C  iF   E           k DISK
000300150331     FFIAR531C  iF   E           k DISK
000400150331     d Fiar5ds       e ds                  extname(FIAR531C)
000500150331     d TISIG20DS     e ds
000600150331     d KPJBA         e ds
000700900131     C*---------------------------------------------------------------*
000800150331     C     *ENTRY        PLIST
000900150331     C                   PARM                    KPJBA
001000150331     c
001100150331     c     kTAS          klist
001200150331     c                   kfld                    TASaas
001300150331     c                   kfld                    TASlnp
001400150331     c                   kfld                    TASnrs
001500150331     c                   kfld                    TASnsp
001600030205      *
001700150331     c     kar5          klist
001800150331     c                   kfld                    ar5aas
001900150331     c                   kfld                    ar5lnp
002000150331     c                   kfld                    ar5nrs
002100150331     c                   kfld                    ar5nsp
002200150331     c                   kfld                    ar5trd
002300150331      *
002400150331     c                   eval        TASaas = 2015
002500150331     c                   eval        TASlnp = 275
002600150331     c                   eval        TASnrs = 0
002700150331     c                   eval        TASnsp = 132618
002800150331      *
002900150331     c                   eval        AR5aas = TASaas
003000150331     c                   eval        AR5lnp = TASlnp
003100150331     c                   eval        AR5nrs = TASnrs
003200150331     c                   eval        AR5nsp = TASnsp
003300150331     c                   eval        ar5trd = 'BAN'
003400150331      *
003500150331     c     kTAS          chain     titas30C
003600150331     c                   if        %Found(Titas30c)
003700150331     C*
003800150331     c     kAR5          chain     FIAR531C
003900150331     c                   if        %Found(FIAR531C)
004000150331     C*
004100150331     c                   clear                   tisig20ds
004200150331     c                   eval      i20aas = ar5aas
004300150331     c                   eval      i20lnp = ar5lnp
004400150331     c                   eval      i20nrs = ar5nrs
004500150331     c                   eval      i20nsp = ar5nsp
004600150331     c                   eval      i20trd = ar5trd
004700150331     c                   eval      i20DAT = tasAAS * 10000 + tasMGS
004800150331     c                   move      *all'0'       i20SOG
004900150331     c                   move      TASccm        i20SOG
005000150331     c                   move      TAStbl        i20TBL
005100150331     c                   move      TAScbo        i20CBO
005200150331      *
005300150331     c                   eval        kpjbu = tisig20ds
005400150331     c                   call      'TISIG20R'                           77
005500150331     c                   parm                    kpjba
005600150331     c                   parm                    Fiar5ds
005700150331     C*
005800030515     c                   end
005900150331     c                   end
006000030205     C*
006100150331     C                   CLEAR                   TISIG20DS
006200150331     C                   MOVEL     'C'           i20TLA
006300150331     C                   eval       KPJBU = TISIG20DS
006400150331     C                   CALL      'TISIG20R'                           77
006500150331     C                   PARM                    KPJBA
006600150331     C*
006700000616     C*
006800000000     C                   SETON                                        LR
