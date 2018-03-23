000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300170601     FFnevb04L  iF   E           K DISK
000400170601     FFnart01L  iF   E           K DISK
000500170529     FFNARB01L  uF   E           K DISK
000600170526     Fwfarw01L  uF   E           K DISK
000700170601     Ftitas30c  iF   E           K DISK    extfile(wtitas)  usropn
000800170526     FQsysprt   O    F  132        printer oflind(*inof)
000900000602      *--------------------------------------------------
001000170601     D KPJBA         E DS
001100170601     d devb01        e ds
001200170601     d FIDN12DS      e ds
001300170526     d  skKey                 26    305    dim(20)
001400170526     d  skAnn                306    325    dim(20)
001500170526     d  skDca                326    485  0 dim(20)
001600170526     d  skFca                486    545  0 dim(20)
001700170526     d  skDch                546    705  0 dim(20)
001800170526     d  skCch                706    745    dim(20)
001900170526     d*
002000170526     d dsKey           ds
002100170526     d  dsaac                         4  0
002200170526     d  dsfil                         3  0
002300170526     d  dsnca                         7  0
002400170601
002500170601     D PARAM7a         DS
002600170601     D  nmi7a                  1      7
002700170601     D  fgs7a                  8     10
002800170601     d
002900170601     D PARAM7b         DS
003000170601     D  pa7nsped               1     16
003100170601     D  pa7rich               17     17
003200170601     D  pa7err                18     18
003300170601
003400150126
003500170526     d segnac          s              7  0
003600170601     d kcev            s                   like(evbcev) inz('P  ')
003700170529     d WTITAS          s             21    inz('GAITRAGRU /TITAS30C')
003800150126      *--------------------------------------------------
003900170529     c     *entry        plist
004000170601     c                   parm                    kpjba
004100170529     c
004200170529     c     kdet          klist
004300170526     c                   kfld                    arwaas
004400170526     c                   kfld                    arwlnp
004500170526     c                   kfld                    arwnrs
004600170526     c                   kfld                    arwnsp
004700170526     c                   kfld                    arw_encd
004800170529     c     ksegnac       klist
004900170526     c                   kfld                    artfls
005000170526     c                   kfld                    artlna
005100170529     c                   kfld                    artnrs
005200170526     c                   kfld                    artnsc
005300170529     c     kbolla        klist
005400170529     c                   kfld                    arwaas
005500170529     c                   kfld                    arwlnp
005600170529     c                   kfld                    arwnrs
005700170529     c                   kfld                    arwnsp
005800170601     c     kevb          klist
005900170601     c                   kfld                    arwaas
006000170601     c                   kfld                    arwlnp
006100170601     c                   kfld                    arwnrs
006200170601     c                   kfld                    arwnsp
006300170601     c                   kfld                    kcev
006400170601     c                   open      titas30c
006500170529     c
006600170529     c
006700170601     c                   if        not %open(titas30c)
006800170529     c                   eval      %subst(Wtitas:7:4)='GRPS'
006900170601     C                   OPEN      titas30c
007000170529     c                   endif
007100120111     c                   clear                   conta             7 0
007200170530     c
007300170530     c                   clear                   savlnp            3 0
007400170530     c                   clear                   savnrs            2 0
007500170530     c                   clear                   savnsp            7 0
007600170530     c                   clear                   savncd            7 0
007700021216      *
007800170526     c     *loval        setll     wfarw01l
007900170526     c                   read      wfarw01l
008000170526    1c                   DOW       not %EOF(wfarw01l)
008100170601   1ac                   if        arww88='CO'  and arwdata=0
008200170526     c                   clear                   salta             1
008300170530     c*
008400170530   1bc                   if        savlnp=arwlnp   and
008500170530     c                             savnrs=arwnrs   and
008600170530     c                             savnsp=arwnsp   and
008700170530     c                             savncd=arw_encd
008800170530     c                   eval      arwdata=8888888
008900170530     c                   else
009000170530
009100170530     c                   eval      savlnp=arwlnp
009200170530     c                   eval      savnrs=arwnrs
009300170530     c                   eval      savnsp=arwnsp
009400170530     c                   eval      savncd=arw_encd
009500170526     c* verifico se ha c.a.
009600170526     c                   clear                   fidn12ds
009700170526     c                   eval      i12aas = arwAAS
009800170526     c                   eval      i12lnp = arwLNP
009900170526     c                   eval      i12nrs = arwNRS
010000170526     c                   eval      i12nsp = arwNSP
010100170526     c                   eval      i12fel = 'E'
010200170526     c*
010300170526     c                   call      'FIDN12R'
010400170526     c                   parm                    fidn12ds
010500170526     c*
010600170526     c* se non ci sono errori
010700170526    2c                   if        o12err = ' '
010800170526     c* se numero di CA trovate maggiore di zero
010900170526    3c                   if        o12nca > 0
011000170526     c
011100170526     c                   z-add     1             aa                2 0
011200170526    4c                   if        aa <= o12nca
011300170526     c                   eval      arwca  =skkey(1)
011400170526    4c                   endif
011500170526     c* se ha c.a. updato senza elaborare altro
011600170529     c                   eval      arwdata=99999999
011700170526     c                   goto      leggi
011800170526     c*
011900170526    3c                   endif
012000170526    2c                   endif
012100170601     c     kevb          chain     fnevb04l
012200170601     c                   if        %found(fnevb04l)
012300170601     c                   movel     evbnot        devb01
012400170601     c                   else
012500170601     c                   clear                   devb01
012600170601     c                   endif
012700170530
012800170601     c
012900170601     c     kbolla        chain     fnarb01l
013000170601    2c                   if        %found(fnarb01l)
013100170601     c                   eval      arbddc=arbdcp
013200170601     c                   eval      arbDCM=arbdcp
013300170601     c                   eval      arbhmc=evbhev
013400170601    3c                   if        %found(fnevb04l)
013500170601     c                   movel     §NOTPDC       arbpdc
013600170601     c                   movel     §NOTNDC       arbndc
013700170601     c                   else
013800170601     c                   movel     0000998       arbpdc
013900170601     c                   movel     arbifp        arbpdc
014000170601     c                   movel     9999999       arbndc
014100170601    3c                   endif
014200170601     c
014300170601     c                   clear                   arbdcp
014400170601     c                   movel     'S'           arbfdc
014500170601     c                   sub       1             arbntc
014600170601    3c                   if        arbipp>0
014700170601     c                   movel     arbipp        arbifp
014800170601     c                   else
014900170601     c                   movel     arbpdc        arbipp
015000170601     c                   movel     arbpdc        arbifp
015100170601    3c                   endif
015200170601     c                   movel     *all'9'       arbnmi
015300170601     c                   update    fnarb000
015400170601
015500170601     c* Per aggiornare fnarb richiamo fnlr90r
015600170601     c                   clear                   param7a
015700170601     c                   clear                   param7b
015800170601     c                   eval      pa7rich=' '
015900170601     c                   eval      %subst(pa7nsped:1:4)=%editc(arbaas:'X')
016000170601     c                   eval      %subst(pa7nsped:5:3)=%editc(arblnp:'X')
016100170601     c                   eval      %subst(pa7nsped:8:2)=%editc(arbnrs:'X')
016200170601     c                   eval      %subst(pa7nsped:10:7)=%editc(arbnsp:'X')
016300170601     c                   movel     param7a       kpjbu
016400170601     c                   call      'FNLR90R'
016500170601     c                   parm                    kpjba
016600170601     c                   parm                    param7b
016700170601
016800170601     c                   eval      arwdata=20170601
016900170601     c                   endif
017000170601     c
017100170601     c     leggi         tag
017200170601   1bc                   endif
017300170601     c
017400170601     c                   update    wfarw000
017500170601   1ac                   endif
017600170601     c
017700170601     c                   read      wfarw01l
017800170601    1c                   ENDDO
017900021213      *
018000021213     C                   eval       *inlr = *on
