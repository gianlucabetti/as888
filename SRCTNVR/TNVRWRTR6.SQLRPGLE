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
001900170531     c                   clear                   wbook             1
002000170531     c
002100170529     c                   eval      waas=2017
002200170529     c     waas          setll     fnarb01l
002300170529     c     waas          reade     fnarb01l
002400170529     c                   dow       not %eof(fnarb01l)
002500170601     c                   if        arbaas=2017 and arbmgs>=0522
002600170529     c                             and (arbnrs=40 or arbnrs=41)
002700170531     c                   if        arbdcp>0 and arbdcm=0
002800021216      *
002900170531     c                   eval      wbook=' '
003000170531     c                   clear                   savdcm            8 0
003100170529     c     karb          setll     fnart01l
003200170529     c     karb          reade     fnart01l
003300170529     c                   dow       not %eof(fnart01l)
003400170531     c                   if        artdcm=0
003500170531     c                   eval      wbook='S'
003600170531     c                   else
003700170531     c                   if        artdcm>savdcm
003800170531     c                   eval      savdcm=artdcm
003900170531     c                   endif
004000170531     c                   endif
004100170531
004200170531     c     karb          reade     fnart01l
004300170531     c                   enddo
004400170526     c* Scrivo il file
004500170531     c                   if        wbook=' '
004600170526     c                   clear                   wfarw000
004700170531     c                   eval      arwfile='C'
004800170526     c                   eval      arwaas =arbaas
004900170526     c                   eval      arwlnp =arblnp
005000170526     c                   eval      arwnrs =arbnrs
005100170526     c                   eval      arwnsp =arbnsp
005200170526     c                   eval      arwrmn =arbrmn
005300170526     c                   eval      arwrma =arbrma
005400170526     c                   eval      arwmgs =arbmgs
005500170526     c                   eval      arwlna =arblna
005600170526     c                   eval      arwncl =arbncl
005700170526     c                   eval      arwdcp =arbdcp
005800170531     c                   eval      arwdcm =savdcm
005900170531     c                   eval      arww88 ='CO'
006000170529     c                   eval      arw_encd= artnsc
006100170526     c                   write     wfarw000
006200170526     c                   endif
006300021213      *
006400170529     c                   endif
006500170531     c                   endif
006600170529     c     waas          reade     fnarb01l
006700170529     c                   enddo
006800150127
006900021213      *
007000021213     C                   eval       *inlr = *on
