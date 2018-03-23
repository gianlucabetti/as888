000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300170526     FFnart01L  IF   E           K DISK
000400170526     FFNARB01L  IF   E           K DISK
000500170526     Fwfarw01L  IF a E           K DISK
000600170526     FQsysprt   O    F  132        printer oflind(*inof)
000700000602      *--------------------------------------------------
000800150126
000900170529     d waas            s              4  0 inz
001000170529     d w88             s              2    inz
001100150126      *--------------------------------------------------
001200170529     c     karb          klist
001300170529     c                   kfld                    arbaas
001400170529     c                   kfld                    arblnp
001500170529     c                   kfld                    arbnrs
001600170529     c                   kfld                    arbnsp
001700150127
001800120111     c                   clear                   conta             7 0
001900170529     c                   eval      waas=2017
002000170529     c     waas          setll     fnarb01l
002100170529     c     waas          reade     fnarb01l
002200170529     c                   dow       not %eof(fnarb01l)
002300170529     c                   if        arbaas=2017 and arbmgs>=0524
002400170529     c                             and (arbnrs=40 or arbnrs=41)
002500021216      *
002600170529     c     karb          setll     fnart01l
002700170529     c     karb          reade     fnart01l
002800170529     c                   dow       not %eof(fnart01l)
002900170530     c                   if        (artnpp=88 or artnpp=90)
003000170530     c                                  and (artdam=0 or
003100170526     c                             artdam>0 and artnap=85)
003200170530
003300170530     c                   if        arbncl>9
003400170529     c                   eval      w88='B8'
003500170530     c                   else
003600170530     c                   eval      w88='<8'
003700170530     c                   endif
003800170526     c* Scrivo il file
003900170526     c                   clear                   wfarw000
004000170529     c                   eval      arwfile='A'
004100170526     c                   eval      arwaas =arbaas
004200170526     c                   eval      arwlnp =arblnp
004300170526     c                   eval      arwnrs =arbnrs
004400170526     c                   eval      arwnsp =arbnsp
004500170526     c                   eval      arwrmn =arbrmn
004600170526     c                   eval      arwrma =arbrma
004700170526     c                   eval      arwmgs =arbmgs
004800170526     c                   eval      arwlna =arblna
004900170526     c                   eval      arwncl =arbncl
005000170526     c                   eval      arwdcp =arbdcp
005100170526     c                   eval      arwdcm =arbdcm
005200170526     c                   eval      arwddc =arbddc
005300170526     c                   eval      arwndc =arbndc
005400170526     c                   eval      arww88 =w88
005500170529     c                   eval      arw_encd= artnsc
005600170526     c                   write     wfarw000
005700170526     c                   endif
005800170529     c     karb          reade     fnart01l
005900170529     c                   enddo
006000021213      *
006100170529     c                   endif
006200170529     c     waas          reade     fnarb01l
006300170529     c                   enddo
006400150127
006500021213      *
006600021213     C                   eval       *inlr = *on
