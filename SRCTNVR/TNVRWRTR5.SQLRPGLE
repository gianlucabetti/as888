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
008500170530     c
008600170530     c                   clear                   savlnp            3 0
008700170530     c                   clear                   savnrs            2 0
008800170530     c                   clear                   savnsp            7 0
008900170530     c                   clear                   savncd            7 0
009000021216      *
009100170526     c     *loval        setll     wfarw01l
009200170526     c                   read      wfarw01l
009300170526    1c                   DOW       not %EOF(wfarw01l)
009400170530   1ac                   if        arww88='E8'  and arwdata=0
009500170526     c                   clear                   salta             1
009600170530     c*
009700170530   1bc                   if        savlnp=arwlnp   and
009800170530     c                             savnrs=arwnrs   and
009900170530     c                             savnsp=arwnsp   and
010000170530     c                             savncd=arw_encd
010100170530     c                   eval      arwdata=8888888
010200170530     c                   else
010300170530
010400170530     c                   eval      savlnp=arwlnp
010500170530     c                   eval      savnrs=arwnrs
010600170530     c                   eval      savnsp=arwnsp
010700170530     c                   eval      savncd=arw_encd
010800170526     c* verifico se ha c.a.
010900170526     c                   clear                   fidn12ds
011000170526     c                   eval      i12aas = arwAAS
011100170526     c                   eval      i12lnp = arwLNP
011200170526     c                   eval      i12nrs = arwNRS
011300170526     c                   eval      i12nsp = arwNSP
011400170526     c                   eval      i12fel = 'E'
011500170526     c*
011600170526     c                   call      'FIDN12R'
011700170526     c                   parm                    fidn12ds
011800170526     c*
011900170526     c* se non ci sono errori
012000170526    2c                   if        o12err = ' '
012100170526     c* se numero di CA trovate maggiore di zero
012200170526    3c                   if        o12nca > 0
012300170526     c
012400170526     c                   z-add     1             aa                2 0
012500170526    4c                   if        aa <= o12nca
012600170526     c                   eval      arwca  =skkey(1)
012700170526    4c                   endif
012800170526     c* se ha c.a. updato senza elaborare altro
012900170529     c                   eval      arwdata=99999999
013000170526     c                   goto      leggi
013100170526     c*
013200170526    3c                   endif
013300170526    2c                   endif
013400170530
013500170530     c* Nella bolla ci deve essere almeno un collo partito: altrimenti errore e salto
013600170530     c*  la bolla
013700170530     c                   clear                   wboOK             1
013800170530     c     kbolla        setll     fnart01l
013900170530     c     kbolla        reade(n)  fnart01l
014000170530     c                   dow       not %eof(fnart01l)
014100170530     c                   if        (artdfv>0 and artnpp<>88) or
014200170530     c                             (artdet>0 and artpet<>89)  or
014300170530     c                             (artdut>0 and artput<>89)  or
014400170530     c                             (artdam>0 and artnap<>85)
014500170530     c                   eval      wbook='S'
014600170530     c                   leave
014700170530     c                   endif
014800170530     c
014900170530     c     kbolla        reade(n)  fnart01l
015000170530     c                   enddo
015100170530     c
015200170530     c* se tutti colli in 88 --> errore
015300170530     c                   if        wbook=' '
015400170530     c                   except    err3
015500170530     c                   eval      arwdata=33333333
015600170530     c                   goto      leggi
015700170530    2c                   endif
015800170526     c*
015900170526     c* FNART --> verifico che tutte le date siano a 0
016000170526     c     kdet          chain     fnart01l
016100170526    2c                   if        not %found(fnart01l)
016200170526     c                   except    err1
016300170530     c                   eval      arwdata=11111111
016400170526     c                   goto      leggi
016500170526    2c                   endif
016600170526     c
016700170529     c                   if        (artdfv>0 and artnpp<>88) or
016800170526     c                             (artdet>0 and artpet<>89)  or
016900170526     c                             (artdut>0 and artput<>89)  or
017000170526     c                             (artdam>0 and artnap<>85)
017100170526     c                   except    err2
017200170529     c                   unlock    fnart01l
017300170530     c                   eval      arwdata=22222222
017400170526     c                   goto      leggi
017500170526    2c                   endif
017600170526     c*
017700170529     c                   movel     dsart         arw_rart
017800170529     c                   eval      arw_art='S'
017900170529     c                   add       1             conta
018000170526     c* BTT
018100170529     c     kdet          setll     fnbtt01l
018200170529     c     kdet          reade     fnbtt01l
018300170529     c                   dow       not %eof(fnbtt01l)
018400170526     c                   eval      arw_btt='S'
018500170529
018600170526     c                   if        waggio='S'
018700170526     c                   delete    fnbtt000
018800170526     c* chaino BTP per diminuire il numero colli
018900170529     c     kbtp          chain     fnbtp11l
019000170529     c                   sub       1             btpncl
019100170529     c                   update    fnbtp000
019200170529     c                   else
019300170529     c                   unlock    fnbtt01l
019400170529     c                   endif
019500170526
019600170529     c     kdet          reade     fnbtt01l
019700170526     c                   enddo
019800170529     c* fnanm
019900170529     c     ksegnac       setll     fnanm02l
020000170529     c     ksegnac       reade     fnanm02l
020100170529     c                   dow       not %eof(fnanm02l)
020200170529     c                   eval      arw_anm='S'
020300170529     c                   if        waggio='S'
020400170529     c                   delete    fnanm000
020500170529     c                   else
020600170529     c                   unlock    fnanm02l
020700170529     c                   endif
020800170529     c     ksegnac       reade     fnanm02l
020900170529     c                   enddo
021000170529     c* fnbrv
021100170529     c     ksegnac       setll     fnbrv07l
021200170529     c     ksegnac       reade     fnbrv07l
021300170529     c                   dow       not %eof(fnbrv07l)
021400170529     c                   eval      arw_brv='S'
021500170529     c                   if        waggio='S'
021600170529     c                   delete    fnbrv000
021700170529     c                   else
021800170529     c                   unlock    fnbrv07l
021900170529     c                   endif
022000170529     c     ksegnac       reade     fnbrv07l
022100170529     c                   enddo
022200170529     c
022300170529     c* fiars
022400170529     c     ksegnac       setll     fiars01l
022500170529     c     ksegnac       reade     fiars01l
022600170529     c                   dow       not %eof(fiars01l)
022700170529     c                   eval      arw_ars='S'
022800170529     c                   if        waggio='S'
022900170529     c                   delete    fiars000
023000170529     c                   else
023100170529     c                   unlock    fiars01l
023200170529     c                   endif
023300170529     c     ksegnac       reade     fiars01l
023400170529     c                   enddo
023500170529     c* titat
023600170529     c     kdet          setll     titat30c
023700170529     c     kdet          reade     titat30c
023800170529     c                   dow       not %eof(titat30c)
023900170529     c                   eval      arw_tat='S'
024000170529     c                   if        waggio='S'
024100170529     c                   delete    titat000
024200170529     c                   else
024300170529     c                   unlock    titat30c
024400170529     c                   endif
024500170529     c     kdet          reade     titat30c
024600170529     c                   enddo
024700170529     c* titah
024800170529     c     kbolla        setll     titah30c
024900170529     c     kbolla        reade     titah30c
025000170529     c                   dow       not %eof(titah30c)
025100170529     c                   if        tahnsc=arw_encd
025200170529     c                   eval      arw_tah='S'
025300170529     c                   if        waggio='S'
025400170529     c                   delete    titah000
025500170529     c                   else
025600170529     c                   unlock    titah30c
025700170529     c                   endif
025800170529     c                   endif
025900170529     c     kbolla        reade     titah30c
026000170529     c                   enddo
026100170529     c* titas
026200170529     c     kbolla        setll     titas30c
026300170529     c     kbolla        reade     titas30c
026400170529     c                   dow       not %eof(titas30c)
026500170529     c                   eval      arw_tas='S'
026600170529     c                   if        waggio='S'
026700170529     c                   sub       1             tasncl
026800170529     c                   update    titas000
026900170529     c                   else
027000170529     c                   unlock    titas30c
027100170529     c                   endif
027200170529     c     kbolla        reade     titas30c
027300170529     c                   enddo
027400170529     c
027500170529     c* fnblt
027600170529     c     kdet          chain     fnblt01l
027700170529     c                   if        %found(fnblt01l)
027800170529     c                   eval      arw_blt='S'
027900170529     c                   if        waggio='S'
028000170529     c                   delete    fnblt000
028100170529     c* chaino BLP per diminuire il numero colli
028200170529     c     kbolla        chain     fnblp01l
028300170529     c                   sub       1             blpncl
028400170529     c                   update    fnblp000
028500170529     c                   else
028600170529     c                   unlock    fnblt01l
028700170529     c                   endif
028800170529     c                   endif
028900170529     c
029000170529     c* fnart
029100170529     c                   if        waggio='S'
029200170529     c     kdet          delete    fiart01l
029300170529     c* chaino ARB per diminuire il numero colli
029400170529     c     kbolla        chain     fnARB01l
029500170529     c                   sub       1             arbncl
029600170529     c                   if        arbvlc>0
029700170529     c                   eval      arbfbs='V'
029800170529     c                   endif
029900170529     c                   update    fnarb000
030000170529     c
030100170529     c                   delete    fnart000
030200170529     c                   endif
030300170529     c
030400170529     c                   if        waggio='S'
030500170529     c                   eval      arwdata=20170529
030600170529     C                   endif
030700170529     C*
030800170526     c     leggi         tag
030900170530   1bc                   endif
031000170530     c
031100170529     c                   update    wfarw000
031200170529   1ac                   endif
031300170529     c
031400170526     c                   read      wfarw01l
031500120111    1c                   ENDDO
031600170529     c                   except    tot
031700021213      *
031800021213     C                   eval       *inlr = *on
031900170526     OQsysPrt   E            Testa       1
032000170530     o                                        +   0 'Bolle WURTH con segnac.ERR'
032100170530     o                                        +   3 'Aggiornamento:'
032200170530     o                       waggio           +   2
032300170526     O          E            err1        1
032400170526     o                       arwaas           +   1
032500170526     o                       arwlnp           +   1
032600170526     o                       arwnrs           +   1
032700170526     o                       arwnsp        z  +   1
032800170529     o                       arw_encd      z  +   1
032900170526     o                                        +   1 'collo non trovato in ART'
033000170529     O          E            err2        1
033100170526     o                       arwaas           +   1
033200170526     o                       arwlnp           +   1
033300170526     o                       arwnrs           +   1
033400170526     o                       arwnsp        z  +   1
033500170529     o                       arw_encd      z  +   1
033600170526     o                                        +   1 'collo spuntato'
033700170530     O          E            err3        1
033800170530     o                       arwaas           +   1
033900170530     o                       arwlnp           +   1
034000170530     o                       arwnrs           +   1
034100170530     o                       arwnsp        z  +   1
034200170530     o                       arw_encd      z  +   1
034300170530     o                                        +   1 'tutti i colli in 88'
034400170526     o                       artnpp           +   1
034500170526     o                       artpet           +   1
034600170526     o                       artput           +   1
034700170526     o                       artnap           +   1
034800170529     O          E            tot         1
034900170529     o                                        +   1 'totale aggiornamenti'
035000170529     o                       conta         z  +   1
