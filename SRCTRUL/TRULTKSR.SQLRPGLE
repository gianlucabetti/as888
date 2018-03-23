000100031205      *------------------------------------------------------------------------*
000200031205      *------------------------------------------------------------------------*
000300071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000400031205      *------------------------------------------------------------------------*
000500120928      *------------------------------------------------------------------------*
000600121001     ftitas02l  Uf   e           k Disk
000800121001     D wmgs            s              4  0
001101121001     d totstop         s             15  0
001102121001     d kksc            s              7  0
001103121001     c     ktas          klist
001104121001     c                   kfld                    kksc
001105121001     c                   kfld                    ktbl              2
001106121001     c                   kfld                    kaas              4 0
001107121001     c                   kfld                    kmgs              4 0
001108121001     c                   eval      kksc=0435730
001109121001     c                   eval      ktbl='F1'
001110121001     c                   eval      kaas=2012
001111121001     c                   do        2
001112121001
001601121001      /free
001602121001       exec sql   DECLARE a1 cursor FOR
001700121001        Select tasmgs, count(*) from gaitragrps/titas00f
001800121001          where tasksc=:kksc
001900121001          group by tasmgs  ;
001901121001       exec sql OPEN a1   ;
001902121001
001903121001       clear sqlcod   ;
001904121001
001905121001       dow sqlcod=0   ;
001906121001
001907121001       exec sql FETCH NEXT  FROM a1 INTO: wmgs, :totstop   ;
001908121001         select ;
001909121001         when sqlcod=100 ;
001923121001
001924121001         when sqlcod<0   ;
001925121001         *inh1=*on   ;
001927121001
001928121001         other   ;
001929121001         kmgs=wmgs  ;
001930121001
001931121001      /end-free
001932121001
002301121001     C     ktas          setll     titas02l
002302121001     C     ktas          reade     titas02l
002303121001     c                   dow       not %eof(titas02l)
002305121001
002400121001      * se qui : ha trovato
002401121001     c                   if        TOTSTOP>99998
002402121001     c                   eval      tasstr=99998
002403121001     c                   else
002500121001     c                   EVAL      tasstr=totstop
002501121001     c                   endif
003000121001     c                   update    titas000
003001121001     c
003002121001     C     ktas          reade     titas02l
003100121001     c                   enddo
003101121001     c                   endsl
003102121001     c                   enddo
003103121001     C/exec sql
003104121001     c+ close a1
003105121001     C/end-exec
003200121001     c                   eval      kksc=0435731
003300121001     c                   enddo
003400121001     c                   seton                                        lr
