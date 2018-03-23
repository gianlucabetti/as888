000100031205      *------------------------------------------------------------------------*
000200031205      *------------------------------------------------------------------------*
000300071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000400031205      *------------------------------------------------------------------------*
000500120928      *------------------------------------------------------------------------*
000600121001     ftitas31c  Uf   e           k Disk
000700121001     fqsysprt   o    f  132        printer
000800121001     D ggmmx           s              5
000900121001     D ggmm            DS             5
001000121001     D  gg                     1      2
001100121001     D  mm                     4      5
001101121001     d  rmn            s             15  0
001200121001     c* leggo titas31c per cliente siemens per aggiornare tasmgs
001300121001     C     0435730       setll     titas31c
001400121001     C     0435730       reade     titas31c
001500121001     c                   dow       not %eof(titas31c)
001601121001      /free
001602121001       exec sql   DECLARE a1 cursor FOR
001700121001        Select distinct(tak_rmn), substr(tak_rec, 51, 5)
001800121001          from wftaksmf
001900121001          where tak_rmn=:tasrmn  ;
001901121001       exec sql OPEN a1   ;
001902121001
001903121001       clear sqlcod   ;
001904121001
001905121001       exec sql FETCH NEXT  FROM a1 INTO: rmn, :ggmmx      ;
001906121001       if  sqlcod<>0  ;
002200121001                        except    riga   ;
002300121001                         else     ;
002301121001      /end-free
002302121001
002400121001      * se qui : ha trovato
002500121001     c                   movel     ggmmx         ggmm
002600121001     c                   movel     mm            tasmgs
002700121001     c                   move      gg            tasmgs
002800121001     c                   z-add     tasmgs        tasdrt
002900121001     c                   movel     2012          tasdrt
003000121001     c                   update    titas000
003100121001     c                   end
003101121001     C/exec sql
003102121001     c+ close a1
003103121001     C/end-exec
003200121001     C     0435730       reade     titas31c
003300121001     c                   enddo
003304121001     c
003400121001     c*            ripeto per l'altro codice
003500121001     C     0435731       setll     titas31c
003600121001     C     0435731       reade     titas31c
003700121001     c                   dow       not %eof(titas31c)
003701121001
004201121001      /free
004202121001       exec sql   DECLARE b1 cursor FOR
004203121001        Select distinct(tak_rmn), substr(tak_rec, 51, 5)
004204121001          from wftaksmf
004205121001          where tak_rmn=:tasrmn  ;
004206121001       exec sql OPEN b1   ;
004207121001
004208121001       clear sqlcod   ;
004209121001
004211121001       exec sql FETCH NEXT  FROM b1 INTO: rmn, :ggmmx      ;
004212121001       if  sqlcod<>0  ;
004213121001                        except    riga   ;
004214121001                         else     ;
004215121001      /end-free
004216121001
004600121001      * se qui : ha trovato
004700121001     c                   movel     ggmmx         ggmm
004800121001     c                   movel     mm            tasmgs
004900121001     c                   move      gg            tasmgs
005000121001     c                   z-add     tasmgs        tasdrt
005100121001     c                   movel     2012          tasdrt
005200121001     c                   update    titas000
005300121001     c                   end
005301121001     C/exec sql
005302121001     c+ close b1
005303121001     C/end-exec
005400121001     C     0435731       reade     titas31c
005500121001     c                   enddo
005600121001     c                   seton                                        lr
005700071122
005800121001     OQSYSPRT   E            riga        1
005900121001     O                                              'sped.:'
006000121001     O                       tasnsp           +   1
006100121001     O                       tasrmn           +   2
