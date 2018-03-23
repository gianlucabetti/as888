000100990426     H DECEDIT('0,') DATEDIT(*DMY.)
000200990426      * ---------------------------------------------------------
000401050207     fFIGCPSPIA IP   E             DISK    rename(figcpspia:figcps)
000500030507
001500990426     D KPJBA         E DS
002900020130      *
003000030508     d PARAM           DS
003100050203     d  PARKSC                        7s 0
003200050203     d  PARDDS                        8s 0
003300050203     d  PARADS                        8s 0
003400050203     d  PARLNP                        3s 0
003500050203     d  PARLNA                        3s 0
003600050203     d  PARPUL                        2
003700050203     d  PARINV                        2
003701050203     d  PARaass                       4s 0
003702050203     d  PARlnps                       3s 0
003703050203     d  PARnrss                       2s 0
003704050203     d  PARnsps                       7s 0
003705050207     d  PARTPL                        1
003800030508      *
003805050207      *
003806050207     C                   CLEAR                   PARAM
003811050207     C                   EVAL      PARaass= GCPaas
003812050207     C                   EVAL      PARlnps= GCPlnp
003813050207     C                   EVAL      PARnrss= GCPnrs
003814050207     C                   EVAL      PARnsps= GCpnsp
003815050207     C                   EVAL      PARTPL = 'R'
006500050207     C                   MOVEL(P)  PARAM         KPJBU
006600050207     C                   CALL      'TNVRPF4R'
006700030508     C                   PARM                    KPJBA
006701050210     clr                 exsr      srfine
023200030910      *
023300050207     c     *inzsr        begsr
023301050210     c**********************************************************************
023400050207     C     *ENTRY        PLIST
023500050207     C                   PARM                    KPJBA
023600050207     c                   endsr
023601050210     c**********************************************************************
023700050210     c     srfine        begsr
023702050210     C                   CLEAR                   PARAM
023707050210     C                   EVAL      PARTPL = 'C'
023708050210     C                   MOVEL(P)  PARAM         KPJBU
023709050210     C                   CALL      'TNVRPF4R'
023710050210     C                   PARM                    KPJBA
024000050210     c                   endsr
