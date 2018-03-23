000100041130     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200041130
000300071023     ffnorm01l  if   e           k disk
000400071023     ffnorf01l  if   e           k disk
000500071023     ffnorg01l  uf   e           k disk
000600041130     fQsysprt   o    f  132        printer oflind(*InOf)
000700071023
000800071023     d fnorgds       e ds                  extname(fnorg00f)
000900071023     d fnormds       e ds                  extname(fnorm00f)
001000041130
001100071023     C/EXEC SQL
001200071023     C+ DECLARE B1 CURSOR FOR SELECT fnorm01l.*, fnorg01l.*
001300071023     C+ FROM fnorm01l join fnorg01l on
001400071023     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
001500071023     C+ ormnrv=orgnrv
001600080205     C+ WHERE ormndc>0 and ormddc = 20080205 and
001700080205     C+ ormpor =005 and orgfao = 400
001800071023     C/END-EXEC
001900071023
002000071023     C/EXEC SQL
002100071023     C+ OPEN B1
002200071023     C/END-EXEC
002300071023
002400071023     c                   do        *hival
002500071023
002600071023     C/EXEC SQL
002700071023     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorgds
002800071023     C/END-EXEC
002900071023
003000071023     c                   if        sqlcod = 100 or
003100071023     c                             sqlcod < 0
003200071023     c                   goto      fine
003300071023     c                   endif
003400071023
003500041201     c     korf          Setll     fnorf01l
003600041201     C     korf1         readpe    fnorf01l
003700071023     c                   if        not %eof(fnorf01l)
003800071023     c     korf1         chain     fnorg01l
003900071023     c                   eval      orgfao = orffar
004000071023     c                   eval      orgdfo = orfdae
004100071023     c                   eval      orgofo = orfore
004200071023     c                   update    fnorg000
004300041201     c                   Except    riga
004400071023     c                   endif
004500071023
004600041201e   1c                   EndDo
004700071023
004800071023     C/EXEC SQL
004900071023     C+ CLOSE B1
005000071023     C/END-EXEC
005100041201
005200071023     c     fine          tag
005300041201     c                   eval      *inlr = *on
005400041130
005500041130      *-------------------------------------------------------------------------------*
005600041130      * ROUTINE INIZIALE
005700041130      *-------------------------------------------------------------------------------*
005800041130     c     *Inzsr        BegSr
005900041130
006000041130     c     kOrf          klist
006100041130     c                   kfld                    OrmPoe
006200041130     c                   kfld                    OrmNsr
006300041130     c                   kfld                    OrmNor
006400041130     c                   kfld                    OrmNrv
006500041201     c                   kfld                    OrmDfo
006600041201     C                   kfld                    OrmOfo
006700041201     C                   kfld                    OrmFao
006800041201
006900041201     c     kOrf1         klist
007000041201     c                   kfld                    OrmPoe
007100041201     c                   kfld                    OrmNsr
007200041201     c                   kfld                    OrmNor
007300041201     c                   kfld                    OrmNrv
007400041130
007500041130     c                   EndSr
007600041130
007700041130     oQsysprt   e            riga        1
007800041130     o                       Ormpoe               3
007900041130     o                       Ormnsr           +   1
008000041130     o                       Ormnor           +   1
008100041130     o                       Ormnrv           +   1
008200041130     o                       Ormpor           +   3
008300041130     o                       Ormcor           +   3
008400041201     o                       Ormfao           +   3
