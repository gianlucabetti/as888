000100060315      *parms dynusrprf(*owner)
000200060315
000300060316     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000400060315
000500060315      *------------------------------------------------------------------------*
000600060315      * VERIFICA/AGGIORNA CODICI IMPORTANZA CLIENTI
000700060315      *------------------------------------------------------------------------*
000800060315
000900060315     fazorg01l  if   e           k disk
001000060315     ftabel00f  if   e           k disk
001100060316     fybacl01l  if   e           k disk
001200060404     fcnclp00f  uf   e           k disk
001300060327     fancln10l  uf   e           k disk
001400060316     fwfcic01l  uf a e           k disk    usropn
001500060316     fwfcic00f  uf   e           k disk    usropn rename(wfcic000:wfcicfff)
001600060316     f                                     prefix(f_)
001700060316     fwfcic02l  uf   e           k disk    usropn rename(wfcic000:wfcic002)
001800060403     fwfcic03l  if   e           k disk    usropn rename(wfcic000:wfcic003)
001900060316     fwfcic05l  if   e           k disk    usropn rename(wfcic000:wfcic005)
002000060331     f                                     prefix(i_)
002100060324     ftist51p   o    e             printer oflind(*in90)
002200060316
002300060316      *------------------------------------------------------------------------*
002400060316      *  RIEPILOGO INDICATORI
002500060316      *------------------------------------------------------------------------*
002600060316      * 30 - comodo
002700060327      * 44 - categoria variata
002800060324      * 90 - salto pagina
002900090421      * 91 - non stampo riga vuota
003000060316      *------------------------------------------------------------------------*
003100060316
003200060316      *   V A R I A B I L I
003300060317     d codut           s              1  0 inz(1)
003400060316     d dataiso         s               d   datfmt(*iso)
003500060316     d depare          s                   like(wfcare)
003600060614     d depcmm          s                   like(wfccmm)
003700060316     d depdis          s                   like(wfcdis)
003800060316     d depfil          s                   like(wfcfil)
003900060316     d desare          s             20
004000060614     d descmm          s             25
004100060316     d desdis          s             20
004200060316     d desfil          s             20
004300060316     d finl00          s              1    inz(*off)
004400060316     d kcod            s                   like(tblcod)
004500060327     d kkcc            s                   like(clpkcc)
004600060327     d kkcca           s                   like(aclkcc)
004700060316     d kkey            s                   like(tblkey)
004800060316     d kksc            s                   like(wfcksc)
004900060316     d kksca           s                   like(aclksc)
005000060316     d ksoc            s                   like(aclsocieta)
005100060316     d lengh           s             15  5 inz(80)
005200060316     d sqlline         s           5000    varying
005300060316     d waamm           s                   like(i50ami)
005400060316     d wdata           s              8  0
005500060331     d wok             s              1    inz(*off)
005600060317     d wora            s                   like(wfcora)
005700060316     d wrkfat          s             12  3
005800060316     d w0030           s              3  0
005900060323     d w0040           s              4  0
006000060316     d w0070           s              7  0
006100060316     d xx              s              3  0
006200060317     d yy              s              3  0
006300060329     d zz              s              3  0
006400060316
006500060316      *   S C H I E R E
006600060316     d cmd             s             80    dim(1) ctdata perrcd(1)
006700060316     d skfat           s             10  3 dim(12)
006800060316
006900060316      *   D S   I N T E R N E / E S T E R N E
007000060316     d dspjana         ds
007100060316     d  §pjsoc                 1      3
007200060316     d  §pjsog                 4     11
007300060316     d  §pjksc                12     19
007400060316     d  §pjdes                20     63
007500060316     d  §pjiva                64     83
007600060316     d  §pjcdf                84    103
007700060316     d  §pjind               104    147
007800060316     d  §pjloc               148    181
007900060316     d  §pjcap               182    190
008000060316     d  §pjprv               191    192
008100060316     d  §pjnaz               193    196
008200060316     d  §pjava               197    199
008300060316     d  §pjabc               200    200
008400060316
008500060316     d                 ds
008600060316     d  wmmi                   1      2  0
008700060316     d  waai                   3      6  0
008800060316     d  wmmaai                 1      6  0
008900060316
009000060316     d                 ds
009100060316     d  wmmf                   1      2  0
009200060316     d  waaf                   3      6  0
009300060316     d  wmmaaf                 1      6  0
009400060316
009500060316     d azuteds       e ds                  extname(azute00f)
009600060918     d cnacods       e ds                  extname(cnaco00f)
009700060918     d cnclpds       e ds                  extname(cnclp00f)
009800060918     d cnindds       e ds                  extname(cnind00f)
009900060316     d ddatiute      e ds
010000060614     d ds01          e ds
010100060918     d fnclsds       e ds                  extname(fncls00f)
010200060316     d kpjba         e ds
010300060323     d og143         e ds
010400060316     d tibs10ds      e ds
010500060316     d tibs34ds      e ds
010600060919     d tibs69ds      e ds
010700060918     d tibs73ds      e ds
010800060316     d tise70ds      e ds
010900060316     d tist50ds      e ds
011000060316
011100060316      *------------------------------------------------------------------------*
011200060316
011300060316      * clrpfm del file di work
011400060316     c                   call(e)   'QCMDEXC'
011500060316     c                   parm                    cmd(1)
011600060316     c                   parm                    lengh
011700060316
011800060316      * se riuscito comando vado avanti con l'elaborazione
011900060316     c                   if        not %error
012000060316     c                   exsr      sr_pjana
012100060316     c                   exsr      sr_saldi
012200060316     c                   exsr      sr_kun
012300060316     c                   exsr      sr_fat
012400060614     c                   exsr      sr_com
012500060316     c                   exsr      sr_cat
012600060316     c                   exsr      sr_stampa
012700060327     c                   if        i50agg = 'S'
012800060327     c                   exsr      sr_aggiorna
012900060327     c                   endif
013000060316     c                   endif
013100060316
013200060316     c                   eval      *inlr = *on
013300060316
013400060316      *------------------------------------------------------------------------*
013500060316      * elaborazione anagrafica clienti di proj
013600060316      *------------------------------------------------------------------------*
013700060316     c     sr_pjana      begsr
013800060316
013900060316      * apro il file di work
014000060316     c                   open      wfcic01l
014100060316
014200060316     c                   eval      sqlline =
014300060316     c                             'select clnsocieta, clnsogg, clnksc, -
014400060316     c                             sogdes, sogpartiva, sogcdfisc, indindriz, -
014500060316     c                             indlocalit, indcap, indprov, indstato, -
014600060316     c                             rcosocaval, clnclasabc -
014700060316     c                             from (((ancln00f join ansog00f on  -
014800060316     c                             clnsogg=sogsogg) join anind00f on clnsogg  -
014900060316     c                             =indsogg) join anrco00f on clnsocieta=  -
015000060316     c                             rcosocieta and clnkcc=rcokcc and  -
015100060316     c                             clnksc=rcoksc) -
015200060316     c                             where clnsocieta = ''201'' and  -
015300060316     c                             rcosocaval=''   '' -
015400060316     c                             order by sogpartiva, sogcdfisc'
015500060316
015600060316     C/EXEC SQL
015700060316     C+ prepare s1 from :sqlline
015800060316     C/END-EXEC
015900060316
016000060316     C/EXEC SQL
016100060316     C+ declare c1 cursor for s1
016200060316     C/END-EXEC
016300060316
016400060316     C/EXEC SQL
016500060316     C+ open c1
016600060316     C/END-EXEC
016700060316
016800060316     C/EXEC SQL
016900060316     C+ fetch c1 into :dspjana
017000060316     C/END-EXEC
017100060316
017200060316     c                   dow       sqlcod = *zeros
017300060316     c                   exsr      sr_elaba
017400060316
017500060316     C/EXEC SQL
017600060316     C+ Fetch c1 into :dspjana
017700060316     C/END-EXEC
017800060316
017900060316     C                   enddo
018000060316
018100060316     C/EXEC SQL
018200060316     C+ close c1
018300060316     C/END-EXEC
018400060316
018500060316      * chiudo il file di work
018600060316     c                   close     wfcic01l
018700060316
018800060316     c                   endsr
018900060316      *------------------------------------------------------------------------*
019000060316      * elaboro i rcd selezionati dall'anagrafica
019100060316      *------------------------------------------------------------------------*
019200060316     c     sr_elaba      begsr
019300060316
019400060317     c                   move      §pjksc        kksc
019500060331
019600060331      * se codice 8888 o 9999 non devo scrivere il file
019700060331     c                   move      kksc          w0040
019800060331     c                   if        w0040 = 8888 or w0040 = 9999
019900060331     c                   leavesr
020000060331     c                   endif
020100060331
020200060331      * aggiorno file
020300060316     c     kksc          chain     wfcic01l
020400060316     c                   if        %found(wfcic01l)
020500060316     c                   exsr      sr_vala
020600060316     c                   update    wfcic000
020700060331
020800060331      * scrivo file
020900060316     c                   else
021000060316     c                   clear                   wfcic000
021100060331      * se cliente non è di un p.o. Bartolini i SDI non devo scrivere il file
021200060331     c                   movel     kksc          w0030
021300060331     c     w0030         chain     azorg01l
021400060331     c                   if        not %found(azorg01l)
021500060331     c                   leavesr
021600060331     c                   endif
021700060331     c                   eval      og143 = orgde3
021800060331     c                   if        §ogntw <> 'COR' and §ogntw <> 'MES'
021900060331     c                   leavesr
022000060331     c                   endif
022100060331      * scrivo se rcd ok
022200060316     c                   exsr      sr_vala
022300060316     c                   write     wfcic000
022400060316     c                   endif
022500060331
022600060316     c                   endsr
022700060316
022800060316      *------------------------------------------------------------------------*
022900060316      * valorizzo i campi del file di work con i dati dell'anagrafica
023000060316      *------------------------------------------------------------------------*
023100060316     c     sr_vala       begsr
023200060316
023300060316     c                   z-add     *date         wfcdata
023400060317     c                   eval      wfcora = wora
023500060317     c                   eval      wfcpru = knmus
023600060316     c                   eval      wfcksc = kksc
023700060316
023800060316     c                   eval      ksoc = §pjsoc
023900060327     c                   eval      kkcca = *all'0'
024000060327     c                   move      dutkci        kkcca
024100060316     c                   movel     §pjksc        kksca
024200060316     c     kybacl        chain     ybacl01l
024300060316     c                   if        %found(ybacl01l) and acltbc <> *blanks
024400060331     c                   eval      wfcblk = 'B'
024500060316     c                   endif
024600060316
024700060316     c                   if        §pjiva <> *blanks
024800060316     c                   eval      wfcioc = §pjiva
024900060316     c                   else
025000060316     c                   eval      wfcioc = §pjcdf
025100060316     c                   endif
025200060316
025300060316     c                   eval      wfcrag = §pjdes
025400060316     c                   eval      wfcind = §pjind
025500060317     c                   eval      wfcloc = §pjloc
025600060316     c                   eval      wfccap = §pjcap
025700060316     c                   eval      wfcpro = §pjprv
025800060316     c                   if        §pjnaz='IT '
025900060316     c                   clear                   wfcnaz
026000060316     c                   else
026100060316     c                   eval      wfcnaz = §pjnaz
026200060316     c                   endif
026300060316
026400060316     c                   eval      wfccca = §pjabc
026500060327     c                   eval      wfcccp = §pjabc
026600060316
026700060317     c                   eval      wfcumes = i50umes
026800060329
026900060329     c                   eval      wfcchk = 'N'
027000060331
027100060614     c**!!!              z-add     orgfil        wfcfil
027200060614     c**!!!              z-add     orgcar        wfcare
027300060614     c**!!!              movel     orgfl3        wfcdis
027400060316
027500060316     c                   endsr
027600060316
027700060316      *------------------------------------------------------------------------*
027800060316      * elaborazione saldi fatturato per cliente
027900060316      *------------------------------------------------------------------------*
028000060316     c     sr_saldi      begsr
028100060316
028200060316      * apro i file di work
028300060316     c                   open      wfcic01l
028400060316
028500060316      * leggo il file di work per cliente
028600060316     c     *loval        setll     wfcic01l
028700060316     c                   do        *hival
028800060316     c                   read      wfcic01l
028900060316     c                   if        %eof(wfcic01l)
029000060316     c                   leave
029100060316     c                   endif
029200060316
029300060316      * per ogni cliente devo richiamare il pgm 1 volta per ogni mese di elaborazione
029400060316     c                   clear                   xx
029500060316     c                   clear                   skfat
029600060316     c                   eval      waamm = i50ami
029700060316     c                   dow       waamm <= i50amf
029800060316     c                   add       1             xx
029900060316     c                   clear                   tise70ds
030000060316     c                   eval      i70tla = 'S'
030100060316     c                   eval      i70tle = 'ST'
030200060316     c                   eval      i70ksc = wfcksc
030300060316     c                   eval      i70daf = waamm
030400060316     c                   eval      i70dai = waamm
030500060316     c                   call      'TISE70R'
030600060316     c                   parm                    tise70ds
030700060316
030800060316      * memorizzo il fatturato
030900060316     c                   eval      skfat(xx) = o70ric
031000060316
031100060316      * aumento di 1 mese
031200060316     c                   movel     waamm         wdata
031300060316     c                   move      '01'          wdata
031400060316     c                   move      wdata         dataiso
031500060316     c                   adddur    1:*m          dataiso
031600060316     c                   move      dataiso       wdata
031700060317     c                   movel     wdata         waamm
031800060316
031900060316     c                   enddo
032000060316
032100060316      * aggiorno il file
032200060316     c                   exsr      sr_vals
032300060316     c                   update    wfcic000
032400060316
032500060316     c                   enddo
032600060316
032700060316      * chiudo il file di work
032800060316     c                   close     wfcic01l
032900060316
033000060316     c                   endsr
033100060316
033200060316      *------------------------------------------------------------------------*
033300060316      * valorizzo il file di work con i dati relativi ai saldi
033400060316      *------------------------------------------------------------------------*
033500060316     c     sr_vals       begsr
033600060331
033700060331      * controllo quanti mesi ha lavorato
033800060331     c                   eval      xx = 1
033900060331     c                   for       xx by 1 to 12
034000060331     c                   if        skfat(xx) > *zeros
034100060331     c                   add       1             wfcmesi
034200060331     c                   endif
034300060331     c                   endfor
034400060316
034500060316     c                   eval      wfcft01 = skfat(01)
034600060316     c                   eval      wfcft02 = skfat(02)
034700060316     c                   eval      wfcft03 = skfat(03)
034800060316     c                   eval      wfcft04 = skfat(04)
034900060316     c                   eval      wfcft05 = skfat(05)
035000060316     c                   eval      wfcft06 = skfat(06)
035100060316     c                   eval      wfcft07 = skfat(07)
035200060316     c                   eval      wfcft08 = skfat(08)
035300060316     c                   eval      wfcft09 = skfat(09)
035400060316     c                   eval      wfcft10 = skfat(10)
035500060316     c                   eval      wfcft11 = skfat(11)
035600060316     c                   eval      wfcft12 = skfat(12)
035700060316
035800060316     c                   xfoot     skfat         wfcfat
035900060316
036000060316     c                   endsr
036100060316
036200060316      *------------------------------------------------------------------------*
036300060316      * elaborazione unificante
036400060316      *------------------------------------------------------------------------*
036500060316     c     sr_kun        begsr
036600060316
036700060316      * apro i file di work
036800060316     c                   open      wfcic00f
036900060316     c                   open      wfcic01l
037000060316
037100060316     c                   do        *hival
037200060316     c                   read      wfcic00f
037300060316     c                   if        %eof(wfcic00f)
037400060316     c                   leave
037500060316     c                   endif
037600060316
037700060316      * controlla se il cliente è padre (unificante)
037800060316     c                   clear                   tibs10ds
037900060316     c                   eval      d10drf = *date
038000060316     c                   eval      d10tle = 'ST'
038100060316     c                   eval      d10paf = 'F'
038200060316     c                   eval      d10cod = f_wfcksc
038300060316     c                   call      'TIBS10R'
038400060316     c                   parm                    tibs10ds
038500060316    1c                   if        d10err = *blanks
038600060316     c                   z-add     d10cod        f_wfcksu
038700060316     c                   eval      f_wfcflu = 'X'
038800060316     c                   eval      f_wfcrau = f_wfcrag
038900060316     c                   update    wfcicfff
039000060316
039100060316   x1c                   else
039200060316      * non è padre quindi
039300060316      * controlla se il cliente è figlio (unificato)
039400060316     c                   clear                   tibs10ds
039500060316     c                   eval      d10drf = *date
039600060316     c                   eval      d10tle = 'ST'
039700060316     c                   eval      d10paf = 'P'
039800060316     c                   eval      d10cod = f_wfcksc
039900060316     c                   call      'TIBS10R'
040000060316     c                   parm                    tibs10ds
040100060316    2c                   if        d10err = *blanks
040200060316     c                   z-add     d10cop        w0070
040300060323     c     w0070         chain(n)  wfcic01l
040400060316    3c                   if        %found(wfcic01l)
040500060316     c                   z-add     d10cop        f_wfcksu
040600060316     c                   eval      f_wfcrau = wfcrag
040700060316     c                   update    wfcicfff
040800060316    3c                   endif
040900060316    2c                   endif
041000060316    1c                   endif
041100060316
041200060316     c                   enddo
041300060316
041400060316      * chiudo i file di work
041500060316     c                   close     wfcic00f
041600060316     c                   close     wfcic01l
041700060316
041800060316     c                   endsr
041900060316
042000060316      *------------------------------------------------------------------------*
042100060329      * memorizzo il fatturato
042200060316      *------------------------------------------------------------------------*
042300060316     c     sr_fat        begsr
042400060329
042500060329      * --> calcolo il fatturato
042600060316
042700060316      * apro il file di work
042800060316     c                   open      wfcic02l
042900060316
043000060316     c     *loval        setll     wfcic02l
043100060316     c                   do        *hival
043200060316     c                   read      wfcic02l
043300060316     c                   if        %eof(wfcic02l)
043400060316     c                   leave
043500060316     c                   endif
043600060316
043700060316     c                   select
043800060316      * se cliente non unificato
043900060317     c                   when      wfcflu = *blanks and wfcksu = *zeros
044000060316     c                   clear                   skfat
044100060328     c                   clear                   wfcmedia
044200060328     c                   clear                   wfcfatp
044300060316      * carico il fatturato in schiera
044400060317     c                   eval      skfat(01) = wfcft01
044500060317     c                   eval      skfat(02) = wfcft02
044600060317     c                   eval      skfat(03) = wfcft03
044700060317     c                   eval      skfat(04) = wfcft04
044800060317     c                   eval      skfat(05) = wfcft05
044900060317     c                   eval      skfat(06) = wfcft06
045000060317     c                   eval      skfat(07) = wfcft07
045100060317     c                   eval      skfat(08) = wfcft08
045200060317     c                   eval      skfat(09) = wfcft09
045300060317     c                   eval      skfat(10) = wfcft10
045400060317     c                   eval      skfat(11) = wfcft11
045500060317     c                   eval      skfat(12) = wfcft12
045600060316      * se non ha lavorato 12 mesi calcolo la media e il totale proporzionato
045700060327     c                   if        wfcmesi < 12 and wfcmesi > 0
045800060327     c                   eval      wfcmedia = wfcfat / wfcmesi
045900060316     c                   eval      wfcfatp = wfcmedia * 12
046000060316     c                   endif
046100060331      * se ha lavorato 12 mesi imposto il proporzionato = al fatturato reale
046200060331     c                   if        wfcmesi = 12
046300060331     c                   eval      wfcfatp = wfcfat
046400060331     c                   endif
046500060316      * controllo se ha lavorato negli ultimi mesi richiesti
046600060316     c                   clear                   xx
046700080509     c                   clear                   zz
046800060327     c                   eval      yy = 12
046900060316     c                   eval      xx = 12 - i50umes
047000060327     c                   for       xx = xx + 1 by 1 to yy
047100060328     c                   if        skfat(xx) <> *zeros
047200060329     c                   add       1             zz
047300060316     c                   endif
047400060316     c                   endfor
047500070110     c                   if        zz = i50umes
047600060329     c                   clear                   wfcchk
047700060329     c                   endif
047800060316      * aggiorno il rcd per il cliente non unificato
047900060316     c                   update    wfcic002
048000060316     c                   clear                   skfat
048100060316
048200060316      * se cliente unificato
048300060316     c                   when      wfcksu <> *zeros
048400060316      * carico il fatturato in schiera
048500060317     c                   add       wfcft01       skfat(01)
048600060317     c                   add       wfcft02       skfat(02)
048700060317     c                   add       wfcft03       skfat(03)
048800060317     c                   add       wfcft04       skfat(04)
048900060317     c                   add       wfcft05       skfat(05)
049000060317     c                   add       wfcft06       skfat(06)
049100060317     c                   add       wfcft07       skfat(07)
049200060317     c                   add       wfcft08       skfat(08)
049300060317     c                   add       wfcft09       skfat(09)
049400060317     c                   add       wfcft10       skfat(10)
049500060317     c                   add       wfcft11       skfat(11)
049600060317     c                   add       wfcft12       skfat(12)
049700060316      * quando sto leggendo il codice padre
049800060317     c                   if        wfcflu = 'X'
049900060331     c                   clear                   wfcmesiu
050000060331     c                   clear                   wfcmedia
050100060331     c                   clear                   wfcfatp
050200060316     c                   xfoot     skfat         wfcfatu
050300060331      * controllo quanti mesi ha lavorato l'unificante
050400060316     c                   eval      xx = 1
050500060316     c                   for       xx by 1 to 12
050600060328     c                   if        skfat(xx) <> *zeros
050700060331     c                   add       1             wfcmesiu
050800060316     c                   endif
050900060316     c                   endfor
051000060316      * se non ha lavorato 12 mesi calcolo la media e il totale proporzionato
051100060331     c                   if        wfcmesiu < 12 and wfcmesiu > 0
051200060331     c                   eval      wfcmedia = wfcfatu / wfcmesiu
051300060316     c                   eval      wfcfatp = wfcmedia * 12
051400060316     c                   endif
051500060331      * se ha lavorato 12 mesi imposto il proporzionato = al fatturato reale
051600060331     c                   if        wfcmesiu = 12
051700060331     c                   eval      wfcfatp = wfcfatu
051800060331     c                   endif
051900060316      * controllo se ha lavorato negli ultimi mesi richiesti
052000060316     c                   clear                   xx
052100060329     c                   clear                   zz
052200060327     c                   eval      yy = 12
052300060327     c                   eval      xx = 12 - i50umes
052400060327     c                   for       xx = xx + 1 by 1 to yy
052500060328     c                   if        skfat(xx) <> *zeros
052600060329     c                   add       1             zz
052700060316     c                   endif
052800060316     c                   endfor
052900070110     c                   if        zz = i50umes
053000060329     c                   clear                   wfcchk
053100060329     c                   endif
053200060316      * aggiorno il rcd del codice padre
053300060316     c                   update    wfcic002
053400060316     c                   clear                   skfat
053500060316     c                   endif
053600060316
053700060316     c                   endsl
053800060316
053900060316     c                   enddo
054000060316
054100060316      * chiudo il file di work
054200060316     c                   close     wfcic02l
054300060316
054400060316     c                   endsr
054500060614
054600060614      *------------------------------------------------------------------------*
054700060614      * imposto il commerciale unificante del cliente unificante
054800060614      *------------------------------------------------------------------------*
054900060614     c     sr_com        begsr
055000060614
055100060614      * apro i file di work
055200060614     c                   open      wfcic02l
055300060614
055400060614     c     *loval        setll     wfcic02l
055500060614     c                   do        *hival
055600060614     c                   read      wfcic02l
055700060614     c                   if        %eof(wfcic02l)
055800060614     c                   leave
055900060614     c                   endif
056000060614
056100060614     c                   eval      kkcc = dutkci
056200060614
056300060614     c                   select
056400060614      * se cliente non unificato
056500060614     c                   when      wfcflu = *blanks and wfcksu = *zeros
056600060614      * aggancio l'anagrafica per recuperare il codice commerciale
056700060614      * del cliente non unificato
056800060614     c                   eval      kksc = wfcksc
056900060614      * se cliente unificato
057000060614     c                   when      wfcksu <> *zeros
057100060614      * aggancio l'anagrafica per recuperare il codice commerciale
057200060614      * del cliente unificante
057300060614     c                   eval      kksc = wfcksu
057400060614     c                   endsl
057500060614
057600060614      * cerco il codice commerciale
057700060614     c     kcnclp        chain(n)  cnclp00f
057800060614     c                   if        not %found(cnclp00f)
057900060614     c                   iter
058000060614     c                   endif
058100060614      * cerco il commerciale unificante
058200060614     c                   clear                   ds01
058300060614     c                   eval      kcod = '01'
058400060614     c                   movel     clpage        kkey
058500060614     c     ktabel        chain     tabel00f
058600060614     c                   if        not %found(tabel00f) or tblflg <> *blanks
058700060614     c                   iter
058800060614     c                   endif
058900060614     c                   eval      ds01 = tbluni
059000060614     c                   movel     §01rgf        w0030
059100060614      * cerco distretto/area/p.o. del commerciale
059200060614     c     w0030         chain     azorg01l
059300060614     c                   if        not %found(azorg01l)
059400060614     c                   iter
059500060614     c                   endif
059600060614
059700060614      * aggiorno il record
059800060614     c                   movel     §01rgf        wfccmm
059900060614     c                   z-add     orgfil        wfcfil
060000060614     c                   z-add     orgcar        wfcare
060100060614     c                   movel     orgfl3        wfcdis
060200060614     c                   update    wfcic002
060300060614
060400060614     c                   enddo
060500060614
060600060614      * chiudo i file di work
060700060614     c                   close     wfcic02l
060800060614
060900060614     c                   endsr
061000060316
061100060316      *------------------------------------------------------------------------*
061200060316      * controllo la categoria attuale con il fatturato e memorizzo la nuova
061300060316      *------------------------------------------------------------------------*
061400060316     c     sr_cat        begsr
061500060316
061600060316      * apro i file di work
061700060316     c                   open      wfcic00f
061800060316     c                   open      wfcic02l
061900060316
062000060316     c                   do        *hival
062100060316     c                   read      wfcic00f
062200060316     c                   if        %eof(wfcic00f)
062300060316     c                   leave
062400060316     c                   endif
062500060316
062600060316      * se non ha fatturato torno a leggere
062700060403     c                   if        f_wfcfatp = *zeros
062800060316     c                   iter
062900060316     c                   endif
063000060316
063100060316      * se non ha lavorato negli ultimi mesi richiesti torno a leggere
063200060329     c                   if        f_wfcchk <> *blanks
063300060316     c                   iter
063400060316     c                   endif
063500060316
063600060316      * elaboro solo i codici unificanti e i non unificati
063700060316    1c                   if        f_wfcflu = 'X'    or
063800060316     c                             (f_wfcflu = *blanks and
063900060316     c                              f_wfcksu = *zeros)
064000060316
064100060316     c                   select
064200060316     c                   when      f_wfcfatp > *zeros
064300060316     c                   eval      wrkfat = f_wfcfatp/1000
064400060316     c                   when      f_wfcflu <> 'X'
064500060316     c                   eval      wrkfat = f_wfcfat/1000
064600060316     c                   when      f_wfcflu = 'X'
064700060316     c                   eval      wrkfat = f_wfcfatu/1000
064800060316     c                   endsl
064900060316
065000060316    2c                   if        wrkfat >= i50icd  and
065100060316     c                             wrkfat <  i50ica
065200060316     c                   eval      f_wfcccp = 'C'
065300060316   x2c                   else
065400060316    3c                   if        wrkfat >= i50ibd  and
065500060316     c                             wrkfat <  i50iba
065600060316     c                   eval      f_wfcccp = 'B'
065700060316   x3c                   else
065800060316    4c                   if        wrkfat >= i50iad  and
065900060316     c                             wrkfat <  i50iaa
066000060316     c                   eval      f_wfcccp = 'A'
066100060316   x4c                   else
066200060316    5c                   if        wrkfat >= i50itd  and
066300060316     c                             wrkfat <  i50ita
066400060316     c                   eval      f_wfcccp = 'T'
066500060316   x5c                   else
066600060316    6c                   if        wrkfat >= i50idd
066700060316     c                   eval      f_wfcccp = 'D'
066800060316    6c                   endif
066900060316    5c                   endif
067000060316    4c                   endif
067100060316    3c                   endif
067200060316    2c                   endif
067300060316      * aggiorno il rcd con la nuova categoria calcolata
067400060316     c                   update    wfcicfff
067500060316      * aggiorno i codici legati all'unificante
067600060316     c                   if        f_wfcflu = 'X'
067700060316     c     f_wfcksu      setll     wfcic02l
067800060316     c                   do        *hival
067900060316     c     f_wfcksu      reade     wfcic02l
068000060316     c                   if        %eof(wfcic02l)
068100060316     c                   leave
068200060316     c                   endif
068300060316     c                   eval      wfcccp = f_wfcccp
068400060403     c                   eval      wfcchk = f_wfcchk
068500060316     c                   update    wfcic002
068600060316     c                   enddo
068700060316     c                   endif
068800060316
068900060316    1c                   endif
069000060316
069100060316     c                   enddo
069200060316
069300060316      * chiudo i file di work
069400060316     c                   close     wfcic00f
069500060316     c                   close     wfcic02l
069600060316
069700060316     c                   endsr
069800060614
069900060316      *------------------------------------------------------------------------*
070000060316      * stampa
070100060316      *------------------------------------------------------------------------*
070200060316     c     sr_stampa     begsr
070300060331
070400060331      * apro i file di work
070500060403     c                   open      wfcic03l
070600060331     c                   open      wfcic05l
070700060316
070800060316     c                   eval      stpdai = wmmaai
070900060316     c                   eval      stpdaf = wmmaaf
071000060316     c                   eval      stpumes = i50umes
071100060331
071200060331     c                   if        i50agg = 'S'
071300060331     c                   eval      stpagg = 'SI'
071400060331     c                   else
071500060331     c                   eval      stpagg = 'NO'
071600060331     c                   endif
071700060331
071800060331     c                   if        i50var = 'S'
071900060331     c                   eval      stpvar = 'SI'
072000060331     c                   else
072100060331     c                   eval      stpvar = 'NO'
072200060331     c                   endif
072300060614
072400060614     c                   if        i50blc = 'S'
072500060614     c                   eval      stpblc = 'SI'
072600060614     c                   else
072700060614     c                   eval      stpblc = 'NO'
072800060614     c                   endif
072900060316
073000060316     c                   eval      stpitd = i50itd
073100060316     c                   eval      stpita = i50ita
073200060316     c                   eval      stpiad = i50iad
073300060316     c                   eval      stpiaa = i50iaa
073400060316     c                   eval      stpibd = i50ibd
073500060316     c                   eval      stpiba = i50iba
073600060316     c                   eval      stpicd = i50icd
073700060316     c                   eval      stpica = i50ica
073800060316     c                   eval      stpidd = i50idd
073900060316     c                   eval      stpida = i50ida
074000060331
074100060331      * stampo la pagina dei parametri
074200060331     c                   write     st51p01
074300060316
074400060616      *------
074500060616
074600060324      * leggo il file di work
074700060331     c     *loval        setll     wfcic05l
074800060324     c                   do        *hival
074900060331     c                   read      wfcic05l
075000060324      * fine file
075100060331     c                   if        %eof(wfcic05l)
075200060324     c                   leave
075300060324     c                   endif
075400060331
075500060331      * elaboro solo i codici unificanti e i non unificati
075600060331     c                   if        i_wfcflu = *blanks and i_wfcksu > 0
075700060331     c                   iter
075800060331     c                   endif
075900060331      * se non ha lavorato nei mesi richiesti
076000060331     c                   if        i_wfcchk <> *blanks
076100060331     c                   iter
076200060331     c                   endif
076300060331
076400060331      * per prima cosa devo verificare nel caso di unificante se variato lui o uno dei figli
076500060331      * perchè devo stampare sempre tutto il gruppo se anche solo 1 figlio è variato
076600060331     c                   if        i_wfcflu = 'X'
076700060331     c                   exsr      sr_ctrrcd
076800060331     c                   if        wok = *off
076900060331     c                   iter
077000060331     c                   endif
077100060331     c                   endif
077200090421
077300090421      * spegno indicatore di comodo che pilota la stampa della riga vuota
077400090421     c                   eval      *in91 = *off
077500060316
077600060331      * stampa riga di dettaglio se unificante
077700060331     c                   if        i_wfcflu  = 'X'
077800060331     c                   exsr      sr_stadetu
077900060331     c                   else
078000060331      * stampa riga di dettaglio se singolo
078100060331     c                   exsr      sr_stadet
078200060331     c                   endif
078300060404      * salto riga
078400090421     c   91              write     st51d02
078500060316
078600060324     c                   enddo
078700060316
078800060316      * stampo le righe di chiusura prospetto
078900060316     c                   write     st51ri
079000060316     c                   write     st51fi
079100060316
079200060316      * chiudo i file di work
079300060403     c                   close     wfcic03l
079400060331     c                   close     wfcic05l
079500060316
079600060316     c                   endsr
079700060331
079800060331      *------------------------------------------------------------------------*
079900060331      * controllo se il rcd è da stampare
080000060331      *------------------------------------------------------------------------*
080100060331     c     sr_ctrrcd     begsr
080200060331
080300060331     c                   eval      wok = *off
080400060331
080500060331      * se già l'unificante è diverso sono a posto
080600060331     c                   if        i_wfccca <> i_wfcccp
080700060331     c                   eval      wok = *on
080800060331     c                   leavesr
080900060331     c                   endif
081000060331
081100060403      * altrimenti controllo i figli
081200060403     c     i_wfcksu      setll     wfcic03l
081300060331     c                   do        *hival
081400060403     c     i_wfcksu      reade     wfcic03l
081500060403     c                   if        %eof(wfcic03l)
081600060331     c                   leave
081700060331     c                   endif
081800060331     c                   if        wfcccp <> wfccca
081900060331     c                   eval      wok = *on
082000060331     c                   leave
082100060331     c                   endif
082200060331     c                   enddo
082300060331
082400060331     c                   endsr
082500060616
082600060616      *------------------------------------------------------------------------*
082700060616      * stampa il dettaglio riga per unificante
082800060616      *------------------------------------------------------------------------*
082900060616     c     sr_stadetu    begsr
083000060616
083100060616      * imposto altri dati della stampa
083200060616     c                   clear                   st51d01
083300060616
083400060616     c                   eval      stpksu  = i_wfcksu
083500060616     c                   eval      stprau  = %trim(i_wfcrau)
083600060616     c                   eval      stpfatu = i_wfcfatu
083700060616     c                   eval      stpmesu = i_wfcmesiu
083800060616     c                   eval      stpfatp = i_wfcfatp
083900060616     c                   eval      stpblk  = i_wfcblk
084000060616     c                   eval      stpksc  = i_wfcksc
084100060616     c                   eval      stprag  = %trim(i_wfcrag)
084200060616     c                   eval      stpfat  = i_wfcfat
084300060616     c                   eval      stpmesi = i_wfcmesi
084400060616     c                   eval      stpcca  = i_wfccca
084500060616     c                   if        i_wfccca <> i_wfcccp
084600060616     c                   eval      stpccp  = i_wfcccp
084700060616     c                   endif
084800060616
084900060616      * stampo testata a rottura
085000060616     c                   exsr      sr_testa
085100060616      * se fine pagina salto pagina
085200060616     c                   if        *in90
085300060616     c                   write     st51t01
085400060616     c                   eval      *in90 = *off
085500060616     c                   endif
085600060616      * stampo il dettaglio dell'unificante
085700060616     c                   write     st51d01
085800090421      * accendo indicatore per ok stampa riga vuota
085900090421     c                   eval      *in91 = *on
086000060616
086100060616      * reperisco i dati dei figli
086200060616     c                   clear                   st51d01
086300060616     c     i_wfcksu      setll     wfcic03l
086400060616     c                   do        *hival
086500060616     c     i_wfcksu      reade     wfcic03l
086600060616     c                   if        %eof(wfcic03l)
086700060616     c                   leave
086800060616     c                   endif
086900060616      * se stesso codice dell'unificante torno a leggere
087000060616     c                   if        wfcksu = wfcksc
087100060616     c                   iter
087200060616     c                   endif
087300060616     c                   eval      stpblk  = wfcblk
087400060616     c                   eval      stpksc  = wfcksc
087500060616     c                   eval      stprag  = %trim(wfcrag)
087600060616     c                   eval      stpfat  = wfcfat
087700060616     c                   eval      stpmesi = wfcmesi
087800060616     c                   eval      stpcca  = wfccca
087900060616     c                   clear                   stpccp
088000060616     c                   if        wfccca <> wfcccp
088100060616     c                   eval      stpccp  = wfcccp
088200060616     c                   endif
088300060616      * se richiesto non stampo se bloccato senza fatturato
088400060616     c                   if        i50blc = 'S' and stpblk = 'B' and stpfat = 0
088500060616     c                   iter
088600060616     c                   endif
088700060616      * se fine pagina salto pagina
088800060616     c                   if        *in90
088900060616     c                   write     st51t01
089000060616     c                   eval      *in90 = *off
089100060616     c                   endif
089200060616      * stampo il dettaglio dell'unificante
089300060616     c                   write     st51d01
089400060616     c                   enddo
089500060616
089600060616     c                   endsr
089700060616
089800060616      *------------------------------------------------------------------------*
089900060616      * stampa il dettaglio riga del singolo
090000060616      *------------------------------------------------------------------------*
090100060616     c     sr_stadet     begsr
090200060616
090300060616      * imposto altri dati della stampa
090400060616     c                   clear                   st51d01
090500060616     c                   eval      *in44 = *off
090600060616
090700060616     c                   eval      stpksu  = i_wfcksc
090800060616     c                   eval      stprau  = %trim(i_wfcrag)
090900060616     c                   eval      stpfatu = i_wfcfat
091000060616     c                   eval      stpmesu = i_wfcmesi
091100060616     c                   eval      stpfatp = i_wfcfatp
091200060616     c                   eval      stpblk  = i_wfcblk
091300060616     c                   eval      stpksc  = i_wfcksc
091400060616     c                   eval      stprag  = %trim(i_wfcrag)
091500060616     c                   eval      stpfat  = i_wfcfat
091600060616     c                   eval      stpmesi = i_wfcmesi
091700060616     c                   eval      stpcca  = i_wfccca
091800060616     c                   clear                   stpccp
091900060616     c                   if        i_wfccca <> i_wfcccp
092000060616     c                   eval      stpccp  = i_wfcccp
092100060616     c                   eval      *in44 = *on
092200060616     c                   endif
092300060616      * se richiesto non stampo se bloccato senza fatturato
092400060616     c                   if        i50blc = 'S' and stpblk = 'B' and stpfat = 0
092500060616     c                   leavesr
092600060616     c                   endif
092700060616
092800060616      * se richiesto stampo solo se categoria attuale <> dalla calcolata
092900060616     c                   if        i50var = 'N' or *in44
093000060616      * stampo testata a rottura
093100060616     c                   exsr      sr_testa
093200060616      * se fine pagina salto pagina
093300060616     c                   if        *in90
093400060616     c                   write     st51t01
093500060616     c                   eval      *in90 = *off
093600060616     c                   endif
093700090421      * stampo il dettaglio del cliente
093800090421     c                   write     st51d01
093900090421      * accendo indicatore per ok stampa riga vuota
094000090421     c                   eval      *in91 = *on
094100060616     c                   endif
094200060616
094300060616     c                   endsr
094400060316
094500060316      *------------------------------------------------------------------------*
094600060616      * stampo testata
094700060316      *------------------------------------------------------------------------*
094800060616     c     sr_testa      begsr
094900060616
095000060616      * cambio distretto
095100060616     c                   if        i_wfcdis <> depdis
095200060616     c                   eval      depdis = i_wfcdis
095300060616     c                   exsr      sr_decdis
095400060616      * imposto il distretto in testata
095500060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
095600060616     c                   eval      depare = i_wfcare
095700060616     c                   exsr      sr_decare
095800060616      * imposto l'area in testata
095900060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
096000060616     c                                    '  '+desare
096100060616     c                   eval      depfil = i_wfcfil
096200060616     c                   exsr      sr_decfil
096300060616      * imposto il p.o. in testata
096400060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
096500060616     c                                    '  '+desfil
096600060616     c                   eval      depcmm = i_wfccmm
096700060616     c                   exsr      sr_deccmm
096800060616      * imposto il commerciale in testata
096900060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
097000060627     c                                     %editc(depcmm:'Z')+
097100060627     c                                    '  '+descmm
097200060616      * stampo la testata
097300060616     c                   write     st51t01
097400060616     c                   endif
097500060616
097600060616      * cambio area
097700060616     c                   if        i_wfcare <> depare
097800060616     c                   eval      depdis = i_wfcdis
097900060616     c                   exsr      sr_decdis
098000060616      * imposto il distretto in testata
098100060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
098200060616     c                   eval      depare = i_wfcare
098300060616     c                   exsr      sr_decare
098400060616      * imposto l'area in testata
098500060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
098600060616     c                                    '  '+desare
098700060616     c                   eval      depfil = i_wfcfil
098800060616     c                   exsr      sr_decfil
098900060616      * imposto il p.o. in testata
099000060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
099100060616     c                                    '  '+desfil
099200060616     c                   eval      depcmm = i_wfccmm
099300060616     c                   exsr      sr_deccmm
099400060616      * imposto il commerciale in testata
099500060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
099600060627     c                                     %editc(depcmm:'Z')+
099700060627     c                                    '  '+descmm
099800060616      * stampo la testata
099900060616     c                   write     st51t01
100000060616     c                   endif
100100060616
100200060616      * cambio p.o.
100300060616     c                   if        i_wfcfil <> depfil
100400060616     c                   eval      depdis = i_wfcdis
100500060616     c                   exsr      sr_decdis
100600060616      * imposto il distretto in testata
100700060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
100800060616     c                   eval      depare = i_wfcare
100900060616     c                   exsr      sr_decare
101000060616      * imposto l'area in testata
101100060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
101200060616     c                                    '  '+desare
101300060616     c                   eval      depfil = i_wfcfil
101400060616     c                   exsr      sr_decfil
101500060616      * imposto il p.o. in testata
101600060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
101700060616     c                                    '  '+desfil
101800060616     c                   eval      depcmm = i_wfccmm
101900060616     c                   exsr      sr_deccmm
102000060616      * imposto il commerciale in testata
102100060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
102200060627     c                                     %editc(depcmm:'Z')+
102300060627     c                                    '  '+descmm
102400060616      * stampo la testata
102500060616     c                   write     st51t01
102600060616     c                   endif
102700060616
102800060616      * cambio commerciale
102900060616     c                   if        i_wfccmm <> depcmm
103000060616     c                   eval      depdis = i_wfcdis
103100060616     c                   exsr      sr_decdis
103200060616      * imposto il distretto in testata
103300060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
103400060616     c                   eval      depare = i_wfcare
103500060616     c                   exsr      sr_decare
103600060616      * imposto l'area in testata
103700060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
103800060616     c                                    '  '+desare
103900060616     c                   eval      depfil = i_wfcfil
104000060616     c                   exsr      sr_decfil
104100060616      * imposto il p.o. in testata
104200060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
104300060616     c                                    '  '+desfil
104400060616     c                   eval      depcmm = i_wfccmm
104500060616     c                   exsr      sr_deccmm
104600060616      * imposto il commerciale in testata
104700060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
104800060627     c                                     %editc(depcmm:'Z')+
104900060627     c                                    '  '+descmm
105000060616      * stampo la testata
105100060616     c                   write     st51t01
105200060616     c                   endif
105300060616
105400060616     c                   endsr
105500060616      *------------------------------------------------------------------------*
105600060616      * decodifico il distretto
105700060616      *------------------------------------------------------------------------*
105800060616     c     sr_decdis     begsr
105900060316
106000060316      * decodifico il distretto
106100060324     c                   eval      kcod = '17'
106200060324     c                   eval      kkey = depdis
106300060324     c     ktabel        chain     tabel00f
106400060324     c                   if        not %found(tabel00f) or tblflg <> *blanks
106500060324     c                   eval      desdis = *all'*'
106600060324     c                   else
106700060324     c                   eval      desdis = tbluni
106800060324     c                   endif
106900060316
107000060316     c                   endsr
107100060324
107200060316      *------------------------------------------------------------------------*
107300060316      * decodifco area
107400060316      *------------------------------------------------------------------------*
107500060316     c     sr_decare     begsr
107600060316
107700060316     c                   eval      kcod = '05'
107800060317     c                   movel     depare        kkey
107900060316     c     ktabel        chain     tabel00f
108000060316     c                   if        not %found(tabel00f) or tblflg <> *blanks
108100060316     c                   eval      desare = *all'*'
108200060316     c                   else
108300060316     c                   eval      desare = tbluni
108400060316     c                   endif
108500060316
108600060316     c                   endsr
108700060316
108800060316      *------------------------------------------------------------------------*
108900060316      * decodifico p.o.
109000060316      *------------------------------------------------------------------------*
109100060316     c     sr_decfil     begsr
109200060316
109300060316     c     depfil        chain     azorg01l
109400060316     c                   if        not %found(azorg01l) or orgfva <> *blanks
109500060316     c                   eval      desfil = *all'*'
109600060316     c                   else
109700060316     c                   eval      desfil = orgdes
109800060316     c                   endif
109900060316
110000060316     c                   endsr
110100060614
110200060614      *------------------------------------------------------------------------*
110300060614      * decodifco commerciale
110400060614      *------------------------------------------------------------------------*
110500060614     c     sr_deccmm     begsr
110600060614
110700060614     c                   eval      kcod = '01'
110800060614     c                   movel     depcmm        kkey
110900060614     c     ktabel        chain     tabel00f
111000060614     c                   if        not %found(tabel00f) or tblflg <> *blanks
111100060614     c                   eval      descmm = *all'*'
111200060614     c                   else
111300060614     c                   eval      ds01 = tbluni
111400060614     c                   eval      descmm = §01age
111500060614     c                   endif
111600060614
111700060614     c                   endsr
111800060327
111900060327      *------------------------------------------------------------------------*
112000060327      * aggiorna la categoria del cliente
112100060327      *------------------------------------------------------------------------*
112200060327     c     sr_aggiorna   begsr
112300060327
112400060327      * apro i file di work
112500060327     c                   open      wfcic01l
112600060327
112700060327      * leggo il file di work per cliente
112800060327     c     *loval        setll     wfcic01l
112900060327     c                   do        *hival
113000060327     c                   read      wfcic01l
113100060327     c                   if        %eof(wfcic01l)
113200060327     c                   leave
113300060327     c                   endif
113400060327      * se ok il rcd e categoria variata aggiorno
113500060331     c                   if        wfccca <> wfcccp
113600060918      * prima di aggiornare devo memorizzare sullo storico variazioni l'immagine prima della
113700060918      * variazione
113800060919     c                   clear                   tibs69ds
113900060919     c                   eval      i69kac = wfcksc
114000060919     c                   eval      i69kin = wfcksc
114100060919     c                   eval      i69kcp = wfcksc
114200060919     c                   eval      i69kcs = wfcksc
114300060919     c                   call      'TIBS69R'
114400060919     c                   parm                    tibs69ds
114500060919     c                   parm                    cnacods
114600060919     c                   parm                    cnindds
114700060919     c                   parm                    cnclpds
114800060919     c                   parm                    fnclsds
114900060919
115000060919     c                   clear                   tibs73ds
115100060919     c                   eval      ibs73pru = knmus
115200060919     c                   eval      ibs73noj = 'TIST51R'
115300060918     c                   eval      ibs73pgm = 'TIST51R'
115400060918     c                   eval      ibs73immag = 'P'
115500060918     c                   eval      ibs73cta = 'M'
115600060918     c                   eval      ibs73yda = 'C'
115700060919     c                   call      'TIBS73R'
115800060919     c                   parm                    tibs73ds
115900060919     c                   parm                    cnacods
116000060919     c                   parm                    cnindds
116100060919     c                   parm                    cnclpds
116200060919     c                   parm                    fnclsds
116300060918
116400060327      * aggiorno direttamente cnclp
116500060404     c                   eval      kkcc = dutkci
116600060404     c                   eval      kksc = wfcksc
116700060404     c     kcnclp        chain     cnclp00f
116800060404     c                   if        %found(cnclp00f)
116900060404     c                   eval      clpclv = wfcccp
117000060404     c                   update    cnclp000
117100060404     c                   endif
117200060918      * ora devo memorizzare sullo storico variazioni l'immagine dopo la variazione
117300060919     c                   clear                   tibs73ds
117400060919     c                   eval      ibs73pru = knmus
117500060919     c                   eval      ibs73noj = 'TIST51R'
117600060918     c                   eval      ibs73pgm = 'TIST51R'
117700060918     c                   eval      ibs73immag = 'D'
117800060918     c                   eval      ibs73cta = 'M'
117900060918     c                   eval      ibs73yda = 'C'
118000060919     c                   call      'TIBS73R'
118100060919     c                   parm                    tibs73ds
118200060919     c                   parm                    cnacods
118300060919     c                   parm                    cnindds
118400060919     c                   parm                    cnclpds
118500060919     c                   parm                    fnclsds
118600060403      * aggiorno ancln
118700060404      * bloccato il trigger che aggiorna cnclp
118800060330     c                   eval      ksoc = '201'
118900060330     c                   eval      kkcca = *all'0'
119000060330     c                   move      dutkci        kkcca
119100060330     c                   eval      kksca = *all'0'
119200060330     c                   move      wfcksc        kksca
119300060327     c     kybacl        chain     ancln10l
119400060327     c                   if        %found(ancln10l)
119500060327     c                   eval      clnclasabc = wfcccp
119600060327     c                   update    ancln000
119700060327     c                   endif
119800060327     c                   endif
119900060327
120000060327     c                   enddo
120100060327
120200060327      * chiudo i file di work
120300060327     c                   close     wfcic01l
120400060327
120500060327     c                   endsr
120600060316
120700060316      *------------------------------------------------------------------------*
120800060316      * routine iniziale
120900060316      *------------------------------------------------------------------------*
121000060316     c     *inzsr        begsr
121100060316
121200060316     c     *entry        plist
121300060316     c                   parm                    kpjba
121400060316     c                   eval      tist50ds = kpjbu
121500060316
121600060316     c     *dtaara       define    §azute        azuteds
121700060316     c     *dtaara       define    §datiute      ddatiute
121800060316     c                   in(e)     *dtaara
121900060316     c                   if        %error  or rsut = *blanks
122000060316     c                   clear                   tibs34ds
122100060316     c                   call      'TIBS34R'
122200060316     c                   parm                    tibs34ds
122300060316     c                   in        *dtaara
122400060316     c                   endif
122500060316
122600060317     c                   movel(p)  rsut          dsfirs
122700060316
122800060316     c                   movel     i50ami        waai
122900060316     c                   move      i50ami        wmmi
123000060316     c                   movel     i50amf        waaf
123100060316     c                   move      i50amf        wmmf
123200060317
123300060317     c                   time                    wora
123400060316
123500060316     c     kybacl        klist
123600060316     c                   kfld                    ksoc
123700060327     c                   kfld                    kkcca
123800060316     c                   kfld                    kksca
123900060316
124000060316     c     ktabel        klist
124100060316     c                   kfld                    codut
124200060316     c                   kfld                    kcod
124300060316     c                   kfld                    kkey
124400060327
124500060404     c     kcnclp        klist
124600060404     c                   kfld                    codut
124700060404     c                   kfld                    kkcc
124800060404     c                   kfld                    kksc
124900060316
125000060316     c                   endsr
125100000914
125200060316** cmd
125300001124CLRPFM FILE(WFCIC00F)
