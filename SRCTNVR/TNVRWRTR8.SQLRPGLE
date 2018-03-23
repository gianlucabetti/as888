000100170605     H DECEDIT('0,') DATEDIT(*DMY.)
000500170605     Fwfarw10f  iF A E           K DISK
000600170605     Fwfarw02L  iF   E           K DISK
000601170605     FFNARB01L  iF   E           K DISK
000900000602      *--------------------------------------------------
005300170529     c     kbolla        klist
005400170529     c                   kfld                    arwaas
005500170529     c                   kfld                    arwlnp
005600170529     c                   kfld                    arwnrs
005700170529     c                   kfld                    arwnsp
005800170605
007300170530     c                   clear                   savlnp            3 0
007400170530     c                   clear                   savnrs            2 0
007500170530     c                   clear                   savnsp            7 0
007700021216      *
007800170605     c     *loval        setll     wfarw02l
007900170605     c                   read      wfarw02l
008000170605    1c                   DOW       not %EOF(wfarw02l)
008100170605   1ac                   if        arwdata=20170529
008300170530     c*
008400170530   1bc                   if        savlnp=arwlnp   and
008500170530     c                             savnrs=arwnrs   and
008600170605     c                             savnsp=arwnsp
008900170530     c                   else
009000170530
009100170530     c                   eval      savlnp=arwlnp
009200170530     c                   eval      savnrs=arwnrs
009300170530     c                   eval      savnsp=arwnsp
012900170601     c     kbolla        chain     fnarb01l
013000170601    2c                   if        %found(fnarb01l)
013001170605     c                   clear                   wfarw010
013002170605     c                   eval      orwaas=arwaas
013003170605     c                   eval      orwlnp=arwlnp
013004170605     c                   eval      orwnrs=arwnrs
013005170605     c                   eval      orwnsp=arwnsp
013006170605     c                   eval      orwrmn=arwrmn
013007170605     c                   eval      orwrma=arwrma
013008170605     c                   eval      orwmgs=arwmgs
013009170605     c                   eval      orwlna=arwlna
013100170605     c                   eval      orwncl=arwncl
013101170605     c                   eval      orwnclnew=arbncl
017400170605     c                   write     wfarw010
017500170605    2c                   endif
017501170605   1bc                   endif
017502170605   1ac                   endif
017600170601     c
017700170605     c                   read      wfarw02l
017800170601    1c                   ENDDO
017900021213      *
018000021213     C                   eval       *inlr = *on
