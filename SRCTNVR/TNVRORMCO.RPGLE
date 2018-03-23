000100041130     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200041130
000300041130     fFnorm99L  iF   e           K disk
000400041201     fFnorf01l  if   e           k disk
000500041130     fQsysprt   o    f  132        printer oflind(*InOf)
000600041130
000700041130     d conta           s              7  0 inz
000800041130     d contasc         s              7  0 inz
000900041130     d contaagg        s              7  0 inz
001000041130     d wcreaft         s              1    inz(*Off)
001100041130
001200041130     d                 ds
001300041130     d  ormcor1                1      3  0
001400041130     d  ormcor2                4      7  0
001500041130     d  ormcor3                8     10  0
001600041130     d  OrmCor                 1     10  0
001700041130     d                 ds
001800041130     d  filler                 1      1  0
001900041130     d  OrmPoe                 2      4  0
002000041130     d  OrmNsr                 5      6  0
002100041130     d  OrmNor                 7     13  0
002200041130     d  OrmNrv                14     15  0
002300041130     d  KTASRMN                1     15  0
002400041130     d                 ds
002500041130     d  anno_oggi              1      4  0
002600041130     d  mgs_oggi               5      8  0
002700041130     d  data_oggi              1      8  0 inz
002800041130
002900041130
003000041130     d dorm01        e ds
003100041130     d Kpjba         e ds
003200041130     d Trul34ds      e ds
003300041130     d Fnlv55ds      e ds
003400041130     d dsbl4a        e ds
003500041130
003600041130      * dalla data - alla data
003700041130     c     pdatada       setll     fnorm000
003800041130     c                   read      fnorm000
003900041130    1C                   DOW       NOT %EOF
004000041201    2c                   If        Ormdao > pdataal
004100041130     c                   leave
004200041130    2c                   ENDIF
004300041130
004400041130     c                   Eval      dorm01 = OrmFlo
004500041130
004600041130      * Orm Commissionato
004700041201    2c                   If        §OrCom = 'S' and OrmCor > *Zeros and
004800041130     c                             ormcor2 <> 8888 and ormcor2 <> 9999 and
004900041130     c                             OrmTor <> 'P'
005000041201     c                             and ormfao > 50  and   ormnsr = 00 and
005100041201     c                             ormfao = 999
005200041201      * cerco se la fase precedente è una 50
005300041201     c     korf          Setll     fnorf01l
005400041201     C     korf1         readpe    fnorf01l
005500041201     C                   if        orffar = 50
005600041201     c                   add       1             conta
005700041201     c                   Except    riga
005800041201     c                   endif
005900041201    2c                   endif
006000041201      *
006100041201     c                   read      fnorm000
006200041201e   1c                   EndDo
006300041201
006400041201     c                   eval      *inlr = *on
006500041130
006600041130      *-------------------------------------------------------------------------------*
006700041130      * ROUTINE INIZIALE
006800041130      *-------------------------------------------------------------------------------*
006900041130     c     *Inzsr        BegSr
007000041130
007100041130     c     *Entry        Plist
007200041130     c                   Parm                    xdatada           8
007300041130     c                   Parm                    xdataal           8
007400041130     c                   Parm                    xstampa           1
007500041130
007600041130     c                   movel     xdatada       pdatada           8 0
007700041130     c                   movel     xdataal       pdataal           8 0
007800041130     c                   movel     xstampa       pstampa           1
007900041130
008000041130     c                   eval      data_oggi = *date
008100041130
008200041130     c     kOrf          klist
008300041130     c                   kfld                    OrmPoe
008400041130     c                   kfld                    OrmNsr
008500041130     c                   kfld                    OrmNor
008600041130     c                   kfld                    OrmNrv
008700041201     c                   kfld                    OrmDfo
008800041201     C                   kfld                    OrmOfo
008900041201     C                   kfld                    OrmFao
009000041201
009100041201     c     kOrf1         klist
009200041201     c                   kfld                    OrmPoe
009300041201     c                   kfld                    OrmNsr
009400041201     c                   kfld                    OrmNor
009500041201     c                   kfld                    OrmNrv
009600041130
009700041130     c                   EndSr
009800041130
009900041130     oQsysprt   e            riga        1
010000041130     o                       Ormpoe               3
010100041130     o                       Ormnsr           +   1
010200041130     o                       Ormnor           +   1
010300041130     o                       Ormnrv           +   1
010400041130     o                       Ormpor           +   3
010500041130     o                       Ormcor           +   3
010600041201     o                       Ormfao           +   3
010700041201     o                       conta         z  +   1
