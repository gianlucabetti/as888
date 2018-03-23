000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300170529     FFiars01L  uF   E           K DISK
000400170529     FFnanm02l  uF   E           K DISK
000500170529     FFnbrv07l  uF   E           K DISK
000600170529     FFnblt01L  uF   E           K DISK
000700170529     FFnart01L  uF   E           K DISK
000800170529     FFnbtt01L  uF   E           K DISK
000900170529     FFnbtp11L  uF   E           K DISK
001000170529     FFiart01L  uF   E           K DISK    prefix(i_)
001100170529     FFNARB01L  uF   E           K DISK
001200170529     FFNBLP01L  uF   E           K DISK
001300170526     Fwfarw01L  uF   E           K DISK
001400170529     Ftitah30c  uF   E           K DISK    extfile(wtitah)  usropn
001500170529     Ftitat30c  uF   E           K DISK    extfile(wtitat)  usropn
001600170529     Ftitas30c  uF   E           K DISK    extfile(wtitas)  usropn
001700170526     FQsysprt   O    F  132        printer oflind(*inof)
001800000602      *--------------------------------------------------
001900150126     D WLBDAT          DS                  INZ
002000150126     D  G02DAT                 1      8  0
002100150126     D  G02INV                 9     16  0
002200150126     D  G02ERR                17     17
002300150126     D  G02TGI                18     22  0
002400150126     D WDAT8           DS
002500150126     D  DATADA                 1      8  0
002600150126     D  DATAA                  9     16  0
002700150126     D  GGL                   17     21  0
002800170526     d FIDN12DS      e ds
002900170526     d  skKey                 26    305    dim(20)
003000170526     d  skAnn                306    325    dim(20)
003100170526     d  skDca                326    485  0 dim(20)
003200170526     d  skFca                486    545  0 dim(20)
003300170526     d  skDch                546    705  0 dim(20)
003400170526     d  skCch                706    745    dim(20)
003500170526     d*
003600170526     d dsKey           ds
003700170526     d  dsaac                         4  0
003800170526     d  dsfil                         3  0
003900170526     d  dsnca                         7  0
004000150126
004100170526     D dsart         E DS                  EXTNAME(fnart00f)
004200170526     d segnac          s              7  0
004300170529     d WTITAH          s             21    inz('GAITRAGRU /TITAH30C')
004400170529     d WTITAT          s             21    inz('GAITRAGRU /TITAT30C')
004500170529     d WTITAS          s             21    inz('GAITRAGRU /TITAS30C')
004600150126      *--------------------------------------------------
004700170529     c     *entry        plist
004800170529     c                   parm                    waggio            1
004900170529     c
005000170529     c     kdet          klist
005100170526     c                   kfld                    arwaas
005200170526     c                   kfld                    arwlnp
005300170526     c                   kfld                    arwnrs
005400170526     c                   kfld                    arwnsp
005500170526     c                   kfld                    arw_encd
005600170526     c     kbtp          klist
005700170529     c                   kfld                    bttflp
005800170526     c                   kfld                    arwaas
005900170526     c                   kfld                    arwlnp
006000170526     c                   kfld                    arwnrs
006100170526     c                   kfld                    arwnsp
006200170529     c     ksegnac       klist
006300170526     c                   kfld                    artfls
006400170526     c                   kfld                    artlna
006500170529     c                   kfld                    artnrs
006600170526     c                   kfld                    artnsc
006700170529     c     kbolla        klist
006800170529     c                   kfld                    arwaas
006900170529     c                   kfld                    arwlnp
007000170529     c                   kfld                    arwnrs
007100170529     c                   kfld                    arwnsp
007200170529     c                   open      titah30c
007300170529     c
007400170529     c
007500170529     c                   if        not %open(titah30c)
007600170529     c                   eval      %subst(Wtitah:7:4)='GRPS'
007700170529     c                   eval      %subst(Wtitat:7:4)='GRPS'
007800170529     c                   eval      %subst(Wtitas:7:4)='GRPS'
007900170529     C                   OPEN      titah30c
008000170529     c                   endif
008100170529     C                   OPEN      titat30c
008200170529     C                   OPEN      titas30c
008300120111     c                   clear                   conta             7 0
008400170526     c                   except    testa
008500021216      *
008600170526     c     *loval        setll     wfarw01l
008700170526     c                   read      wfarw01l
008800170526    1c                   DOW       not %EOF(wfarw01l)
008900170530   1ac                   if        (arww88='88'  and arwdata=0)
009000170530     c                             or (arww88='90' and arwdata=0)
009100170526     c                   clear                   salta             1
009200170529     c                   if        arw_rart<>*blanks
009300170529     c***                movel     arw_rart      dsart
009400170529     c                   endif
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
012100170526     c*
012200170526     c* se non ha c.a. aggiorno
012300170526     c* FNART --> verifico che tutte le date siano a 0
012400170526     c     kdet          chain     fnart01l
012500170526    2c                   if        not %found(fnart01l)
012600170526     c                   except    err1
012700170529     c                   eval      arwdata=99999999
012800170526     c                   goto      leggi
012900170526    2c                   endif
013000170526     c
013100170530     c                   if        (artdfv>0 and artnpp<>88
013200170530     c                              and artnpp<>90)        or
013300170526     c                             (artdet>0 and artpet<>89)  or
013400170526     c                             (artdut>0 and artput<>89)  or
013500170526     c                             (artdam>0 and artnap<>85)
013600170526     c                   except    err2
013700170529     c                   unlock    fnart01l
013800170529     c                   eval      arwdata=99999999
013900170526     c                   goto      leggi
014000170526    2c                   endif
014100170526     c*
014200170529     c                   movel     dsart         arw_rart
014300170529     c                   eval      arw_art='S'
014400170529     c                   add       1             conta
014500170526     c* BTT
014600170529     c     kdet          setll     fnbtt01l
014700170529     c     kdet          reade     fnbtt01l
014800170529     c                   dow       not %eof(fnbtt01l)
014900170526     c                   eval      arw_btt='S'
015000170529
015100170526     c                   if        waggio='S'
015200170526     c                   delete    fnbtt000
015300170526     c* chaino BTP per diminuire il numero colli
015400170529     c     kbtp          chain     fnbtp11l
015500170529     c                   sub       1             btpncl
015600170529     c                   update    fnbtp000
015700170529     c                   else
015800170529     c                   unlock    fnbtt01l
015900170529     c                   endif
016000170526
016100170529     c     kdet          reade     fnbtt01l
016200170526     c                   enddo
016300170529     c* fnanm
016400170529     c     ksegnac       setll     fnanm02l
016500170529     c     ksegnac       reade     fnanm02l
016600170529     c                   dow       not %eof(fnanm02l)
016700170529     c                   eval      arw_anm='S'
016800170529     c                   if        waggio='S'
016900170529     c                   delete    fnanm000
017000170529     c                   else
017100170529     c                   unlock    fnanm02l
017200170529     c                   endif
017300170529     c     ksegnac       reade     fnanm02l
017400170529     c                   enddo
017500170529     c* fnbrv
017600170529     c     ksegnac       setll     fnbrv07l
017700170529     c     ksegnac       reade     fnbrv07l
017800170529     c                   dow       not %eof(fnbrv07l)
017900170529     c                   eval      arw_brv='S'
018000170529     c                   if        waggio='S'
018100170529     c                   delete    fnbrv000
018200170529     c                   else
018300170529     c                   unlock    fnbrv07l
018400170529     c                   endif
018500170529     c     ksegnac       reade     fnbrv07l
018600170529     c                   enddo
018700170529     c
018800170529     c* fiars
018900170529     c     ksegnac       setll     fiars01l
019000170529     c     ksegnac       reade     fiars01l
019100170529     c                   dow       not %eof(fiars01l)
019200170529     c                   eval      arw_ars='S'
019300170529     c                   if        waggio='S'
019400170529     c                   delete    fiars000
019500170529     c                   else
019600170529     c                   unlock    fiars01l
019700170529     c                   endif
019800170529     c     ksegnac       reade     fiars01l
019900170529     c                   enddo
020000170529     c* titat
020100170529     c     kdet          setll     titat30c
020200170529     c     kdet          reade     titat30c
020300170529     c                   dow       not %eof(titat30c)
020400170529     c                   eval      arw_tat='S'
020500170529     c                   if        waggio='S'
020600170529     c                   delete    titat000
020700170529     c                   else
020800170529     c                   unlock    titat30c
020900170529     c                   endif
021000170529     c     kdet          reade     titat30c
021100170529     c                   enddo
021200170529     c* titah
021300170529     c     kbolla        setll     titah30c
021400170529     c     kbolla        reade     titah30c
021500170529     c                   dow       not %eof(titah30c)
021600170529     c                   if        tahnsc=arw_encd
021700170529     c                   eval      arw_tah='S'
021800170529     c                   if        waggio='S'
021900170529     c                   delete    titah000
022000170529     c                   else
022100170529     c                   unlock    titah30c
022200170529     c                   endif
022300170529     c                   endif
022400170529     c     kbolla        reade     titah30c
022500170529     c                   enddo
022600170529     c* titas
022700170529     c     kbolla        setll     titas30c
022800170529     c     kbolla        reade     titas30c
022900170529     c                   dow       not %eof(titas30c)
023000170529     c                   eval      arw_tas='S'
023100170529     c                   if        waggio='S'
023200170529     c                   sub       1             tasncl
023300170529     c                   update    titas000
023400170529     c                   else
023500170529     c                   unlock    titas30c
023600170529     c                   endif
023700170529     c     kbolla        reade     titas30c
023800170529     c                   enddo
023900170529     c
024000170529     c* fnblt
024100170529     c     kdet          chain     fnblt01l
024200170529     c                   if        %found(fnblt01l)
024300170529     c                   eval      arw_blt='S'
024400170529     c                   if        waggio='S'
024500170529     c                   delete    fnblt000
024600170529     c* chaino BLP per diminuire il numero colli
024700170529     c     kbolla        chain     fnblp01l
024800170529     c                   sub       1             blpncl
024900170529     c                   update    fnblp000
025000170529     c                   else
025100170529     c                   unlock    fnblt01l
025200170529     c                   endif
025300170529     c                   endif
025400170529     c
025500170529     c* fnart
025600170529     c                   if        waggio='S'
025700170529     c     kdet          delete    fiart01l
025800170529     c* chaino ARB per diminuire il numero colli
025900170529     c     kbolla        chain     fnARB01l
026000170529     c                   sub       1             arbncl
026100170529     c                   if        arbvlc>0
026200170529     c                   eval      arbfbs='V'
026300170529     c                   endif
026400170529     c                   update    fnarb000
026500170529     c
026600170529     c                   delete    fnart000
026700170529     c                   endif
026800170529     c
026900170529     c                   if        waggio='S'
027000170529     c                   eval      arwdata=20170529
027100170529     C                   endif
027200170529     C*
027300170526     c     leggi         tag
027400170529     c                   update    wfarw000
027500170529   1ac                   endif
027600170529     c
027700170526     c                   read      wfarw01l
027800120111    1c                   ENDDO
027900170529     c                   except    tot
028000021213      *
028100021213     C                   eval       *inlr = *on
028200170526     OQsysPrt   E            Testa       1
028300170526     o                                        +   0 'Bolle WURTH con segnac.err'
028400170526     O          E            err1        1
028500170526     o                       arwaas           +   1
028600170526     o                       arwlnp           +   1
028700170526     o                       arwnrs           +   1
028800170526     o                       arwnsp        z  +   1
028900170529     o                       arw_encd      z  +   1
029000170526     o                                        +   1 'collo non trovato in ART'
029100170529     O          E            err2        1
029200170526     o                       arwaas           +   1
029300170526     o                       arwlnp           +   1
029400170526     o                       arwnrs           +   1
029500170526     o                       arwnsp        z  +   1
029600170529     o                       arw_encd      z  +   1
029700170526     o                                        +   1 'collo spuntato'
029800170526     o                       artnpp           +   1
029900170526     o                       artpet           +   1
030000170526     o                       artput           +   1
030100170526     o                       artnap           +   1
030200170529     O          E            tot         1
030300170529     o                                        +   1 'totale aggiornamenti'
030400170529     o                       conta         z  +   1
