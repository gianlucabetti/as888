000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400120518     fywcpa01l  uf   e           k Disk
000500120518     fTita430c  if   e           k Disk    ignore(tita4p00)
000600120518     Fndppa01L  IF   E           K DISK
000700120518     FNdcpa01L  uF   E           K DISK
000800040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900111107      *
001000111108     d kpjba         e ds
001100120518     D** DS codici fiscali su tita4
001200120518     D Dta4q         e ds
001300120518     dwsoc             s                   like(ppasocieta)
001400120518     dwctb             s                   like(ppactb) inz('CG')
001500120518     dwkcc             s                   like(ppakcc) inz('000151')
001600120518     dwksc             s                   like(ppaksc) inz('00000000')
001700120518     dwdft             s                   like(ppadtpar)
001800120518     dwnft             s                   like(ppanrpar)
001900120518     dwfiv             s                   like(ppaserpar) inz
002000120518     d datajis         s               d   datfmt(*jis)
002100120518     d dataiso         s               d   datfmt(*iso)
002200001006      *------------------------------------------------------------------------*
002300120518     C     Kar4          KLIST
002400120518     C                   KFLD                    wpaaas
002500120518     C                   KFLD                    wpalnp
002600120518     C                   KFLD                    wpanrs
002700120518     C                   KFLD                    wpansp
002800120518     C                   KFLD                    ktrc              1
002900120518     C     Kppa          KLIST
003000120518     C                   KFLD                    wsoc
003100120518     C                   KFLD                    wctb
003200120518     C                   KFLD                    wkcc
003300120518     C                   KFLD                    wksc
003400120518     C                   KFLD                    wdft
003500120518     C                   KFLD                    wnft
003600120518     C                   KFLD                    wfiv
003700120518     C     Kycpa         KLIST
003800120518     C                   KFLD                    ppasys
003900120518     C                   KFLD                    ppanrasint
004000111107     c
004100111108     c
004200120518     c                   setoff                                       10
004300120518     c                   clear                   totagg
004400120518     c     *loval        setll     ywcpa01l
004500120518     c                   read      ywcpa01l
004600111108     c
004700120518    1c                   dow       not %eof(ywcpa01l)
004800120518   1ac                   if        WPACDFISC=*blanks
004900120518     C                   MOVE      wpaKSC        KSCCOD            4 0
005000120518     c
005100120518     c                   clear                   dta4q
005200120518     c                   eval      ktrc='Q'
005300120518     c     kar4          chain     tita430c
005400120518    2c                   if        %found(tita430c)
005500120518     c                   eval      dta4q = ta4not
005600120518    3c                   if        (ksccod=9999 and §TA4QCFD<>*blanks ) or
005700120518     c
005800120518     c                             (ksccod=8888 and §TA4QCFM<>*blanks )
005900120518     c
006000120518     c                   eval      wsoc=WPASOCIETA
006100120518     c                   eval      wksc=WPAKSC
006200120518     c                   eval      wdft   =     WPADTPAR
006300120518     c                   eval      wnft=WPANRPAR
006400120518     c
006500120518     C     Kppa          CHAIN     ndppa01l                           26
006600120518    4C     *IN26         IFEQ      *OFF
006700120518     c     kycpa         chain     ndcpa01l                           26
006800120518    5C                   if        not *in26 and cpacdfisc=*blanks
006900120518     c
007000120518    6c                   if        ksccod=8888
007100120518     C                   MOVEL     §TA4QCFM      cpacdfisc
007200120518     c                   else
007300120518     C                   MOVEL     §TA4QCFD      cpacdfisc
007400120518    6c                   endif
007500120518     c
007600120518     c                   update    ndcpa000
007700120518     c
007800120518     c                   add       1             totagg            7 0
007900120518     c
008000120518     c                   eval      WPACDFISC=cpacdfisc
008100120518     c                   update    ywcpa000
008200120518     c
008300120518    5c                   endif
008400120518    4c                   endif
008500120518    3c                   endif
008600120518    2c                   endif
008700120518   1ac                   endif
008800111107     c
008900120518     c                   read      ywcpa01l
009000120518    1c                   EndDo
009100120518     c
009200111108     c
009300111107     c                   except    totale
009400010202
009500111107     c
009600040225     c                   Eval      *Inlr = *On
009700111110     c*--------------------------------------------
009800120518     OQSYSPRT   E            totale      1
009900120518     o                       totagg        2   +  1
