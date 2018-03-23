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
001100130902     fAZCMM01L  if   e           k disk
001200060316     fybacl01l  if   e           k disk
001300060404     fcnclp00f  uf   e           k disk
001400060327     fancln10l  uf   e           k disk
001500060316     fwfcic01l  uf a e           k disk    usropn
001600060316     fwfcic00f  uf   e           k disk    usropn rename(wfcic000:wfcicfff)
001700060316     f                                     prefix(f_)
001800060316     fwfcic02l  uf   e           k disk    usropn rename(wfcic000:wfcic002)
001900060403     fwfcic03l  if   e           k disk    usropn rename(wfcic000:wfcic003)
002000060316     fwfcic05l  if   e           k disk    usropn rename(wfcic000:wfcic005)
002100060331     f                                     prefix(i_)
002200060324     ftist51p   o    e             printer oflind(*in90)
002300060316
002400060316      *------------------------------------------------------------------------*
002500060316      *  RIEPILOGO INDICATORI
002600060316      *------------------------------------------------------------------------*
002700060316      * 30 - comodo
002800060327      * 44 - categoria variata
002900060324      * 90 - salto pagina
003000090421      * 91 - non stampo riga vuota
003100060316      *------------------------------------------------------------------------*
003200060316
003300060316      *   V A R I A B I L I
003400060317     d codut           s              1  0 inz(1)
003500060316     d dataiso         s               d   datfmt(*iso)
003600060316     d depare          s                   like(wfcare)
003700060614     d depcmm          s                   like(wfccmm)
003800060316     d depdis          s                   like(wfcdis)
003900060316     d depfil          s                   like(wfcfil)
004000060316     d desare          s             20
004100060614     d descmm          s             25
004200060316     d desdis          s             20
004300060316     d desfil          s             20
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
010000060918     d fnclsds       e ds                  extname(fncls00f)
010100060316     d kpjba         e ds
010200060323     d og143         e ds
010300060316     d tibs10ds      e ds
010400060316     d tibs34ds      e ds
010500060919     d tibs69ds      e ds
010600060918     d tibs73ds      e ds
010700060316     d tise70ds      e ds
010800060316     d tist50ds      e ds
010900060316
011000060316      *------------------------------------------------------------------------*
011100130902
011200130902     c/EXEC SQL
011300130902     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
011400130902     c/END-EXEC
011500060316
011600060316      * clrpfm del file di work
011700060316     c                   call(e)   'QCMDEXC'
011800060316     c                   parm                    cmd(1)
011900060316     c                   parm                    lengh
012000060316
012100060316      * se riuscito comando vado avanti con l'elaborazione
012200060316     c                   if        not %error
012300060316     c                   exsr      sr_pjana
012400060316     c                   exsr      sr_saldi
012500060316     c                   exsr      sr_kun
012600060316     c                   exsr      sr_fat
012700060614     c                   exsr      sr_com
012800060316     c                   exsr      sr_cat
012900060316     c                   exsr      sr_stampa
013000060327     c                   if        i50agg = 'S'
013100060327     c                   exsr      sr_aggiorna
013200060327     c                   endif
013300060316     c                   endif
013400060316
013500060316     c                   eval      *inlr = *on
013600060316
013700060316      *------------------------------------------------------------------------*
013800060316      * elaborazione anagrafica clienti di proj
013900060316      *------------------------------------------------------------------------*
014000060316     c     sr_pjana      begsr
014100060316
014200060316      * apro il file di work
014300060316     c                   open      wfcic01l
014400060316
014500060316     c                   eval      sqlline =
014600060316     c                             'select clnsocieta, clnsogg, clnksc, -
014700060316     c                             sogdes, sogpartiva, sogcdfisc, indindriz, -
014800060316     c                             indlocalit, indcap, indprov, indstato, -
014900060316     c                             rcosocaval, clnclasabc -
015000060316     c                             from (((ancln00f join ansog00f on  -
015100060316     c                             clnsogg=sogsogg) join anind00f on clnsogg  -
015200060316     c                             =indsogg) join anrco00f on clnsocieta=  -
015300060316     c                             rcosocieta and clnkcc=rcokcc and  -
015400060316     c                             clnksc=rcoksc) -
015500060316     c                             where clnsocieta = ''201'' and  -
015600060316     c                             rcosocaval=''   '' -
015700060316     c                             order by sogpartiva, sogcdfisc'
015800060316
015900060316     C/EXEC SQL
016000060316     C+ prepare s1 from :sqlline
016100060316     C/END-EXEC
016200060316
016300060316     C/EXEC SQL
016400060316     C+ declare c1 cursor for s1
016500060316     C/END-EXEC
016600060316
016700060316     C/EXEC SQL
016800060316     C+ open c1
016900060316     C/END-EXEC
017000060316
017100060316     C/EXEC SQL
017200060316     C+ fetch c1 into :dspjana
017300060316     C/END-EXEC
017400060316
017500060316     c                   dow       sqlcod = *zeros
017600060316     c                   exsr      sr_elaba
017700060316
017800060316     C/EXEC SQL
017900060316     C+ Fetch c1 into :dspjana
018000060316     C/END-EXEC
018100060316
018200060316     C                   enddo
018300060316
018400060316     C/EXEC SQL
018500060316     C+ close c1
018600060316     C/END-EXEC
018700060316
018800060316      * chiudo il file di work
018900060316     c                   close     wfcic01l
019000060316
019100060316     c                   endsr
019200060316      *------------------------------------------------------------------------*
019300060316      * elaboro i rcd selezionati dall'anagrafica
019400060316      *------------------------------------------------------------------------*
019500060316     c     sr_elaba      begsr
019600060316
019700060317     c                   move      §pjksc        kksc
019800060331
019900060331      * se codice 8888 o 9999 non devo scrivere il file
020000060331     c                   move      kksc          w0040
020100060331     c                   if        w0040 = 8888 or w0040 = 9999
020200060331     c                   leavesr
020300060331     c                   endif
020400060331
020500060331      * aggiorno file
020600060316     c     kksc          chain     wfcic01l
020700060316     c                   if        %found(wfcic01l)
020800060316     c                   exsr      sr_vala
020900060316     c                   update    wfcic000
021000060331
021100060331      * scrivo file
021200060316     c                   else
021300060316     c                   clear                   wfcic000
021400060331      * se cliente non è di un p.o. Bartolini i SDI non devo scrivere il file
021500060331     c                   movel     kksc          w0030
021600060331     c     w0030         chain     azorg01l
021700060331     c                   if        not %found(azorg01l)
021800060331     c                   leavesr
021900060331     c                   endif
022000060331     c                   eval      og143 = orgde3
022100060331     c                   if        §ogntw <> 'COR' and §ogntw <> 'MES'
022200060331     c                   leavesr
022300060331     c                   endif
022400060331      * scrivo se rcd ok
022500060316     c                   exsr      sr_vala
022600060316     c                   write     wfcic000
022700060316     c                   endif
022800060331
022900060316     c                   endsr
023000060316
023100060316      *------------------------------------------------------------------------*
023200060316      * valorizzo i campi del file di work con i dati dell'anagrafica
023300060316      *------------------------------------------------------------------------*
023400060316     c     sr_vala       begsr
023500060316
023600060316     c                   z-add     *date         wfcdata
023700060317     c                   eval      wfcora = wora
023800060317     c                   eval      wfcpru = knmus
023900060316     c                   eval      wfcksc = kksc
024000060316
024100060316     c                   eval      ksoc = §pjsoc
024200060327     c                   eval      kkcca = *all'0'
024300060327     c                   move      dutkci        kkcca
024400060316     c                   movel     §pjksc        kksca
024500060316     c     kybacl        chain     ybacl01l
024600060316     c                   if        %found(ybacl01l) and acltbc <> *blanks
024700060331     c                   eval      wfcblk = 'B'
024800060316     c                   endif
024900060316
025000060316     c                   if        §pjiva <> *blanks
025100060316     c                   eval      wfcioc = §pjiva
025200060316     c                   else
025300060316     c                   eval      wfcioc = §pjcdf
025400060316     c                   endif
025500060316
025600060316     c                   eval      wfcrag = §pjdes
025700060316     c                   eval      wfcind = §pjind
025800060317     c                   eval      wfcloc = §pjloc
025900060316     c                   eval      wfccap = §pjcap
026000060316     c                   eval      wfcpro = §pjprv
026100060316     c                   if        §pjnaz='IT '
026200060316     c                   clear                   wfcnaz
026300060316     c                   else
026400060316     c                   eval      wfcnaz = §pjnaz
026500060316     c                   endif
026600060316
026700060316     c                   eval      wfccca = §pjabc
026800060327     c                   eval      wfcccp = §pjabc
026900060316
027000060317     c                   eval      wfcumes = i50umes
027100060329
027200060329     c                   eval      wfcchk = 'N'
027300060331
027400060614     c**!!!              z-add     orgfil        wfcfil
027500060614     c**!!!              z-add     orgcar        wfcare
027600060614     c**!!!              movel     orgfl3        wfcdis
027700060316
027800060316     c                   endsr
027900060316
028000060316      *------------------------------------------------------------------------*
028100060316      * elaborazione saldi fatturato per cliente
028200060316      *------------------------------------------------------------------------*
028300060316     c     sr_saldi      begsr
028400060316
028500060316      * apro i file di work
028600060316     c                   open      wfcic01l
028700060316
028800060316      * leggo il file di work per cliente
028900060316     c     *loval        setll     wfcic01l
029000060316     c                   do        *hival
029100060316     c                   read      wfcic01l
029200060316     c                   if        %eof(wfcic01l)
029300060316     c                   leave
029400060316     c                   endif
029500060316
029600060316      * per ogni cliente devo richiamare il pgm 1 volta per ogni mese di elaborazione
029700060316     c                   clear                   xx
029800060316     c                   clear                   skfat
029900060316     c                   eval      waamm = i50ami
030000060316     c                   dow       waamm <= i50amf
030100060316     c                   add       1             xx
030200060316     c                   clear                   tise70ds
030300060316     c                   eval      i70tla = 'S'
030400060316     c                   eval      i70tle = 'ST'
030500060316     c                   eval      i70ksc = wfcksc
030600060316     c                   eval      i70daf = waamm
030700060316     c                   eval      i70dai = waamm
030800060316     c                   call      'TISE70R'
030900060316     c                   parm                    tise70ds
031000060316
031100060316      * memorizzo il fatturato
031200060316     c                   eval      skfat(xx) = o70ric
031300060316
031400060316      * aumento di 1 mese
031500060316     c                   movel     waamm         wdata
031600060316     c                   move      '01'          wdata
031700060316     c                   move      wdata         dataiso
031800060316     c                   adddur    1:*m          dataiso
031900060316     c                   move      dataiso       wdata
032000060317     c                   movel     wdata         waamm
032100060316
032200060316     c                   enddo
032300060316
032400060316      * aggiorno il file
032500060316     c                   exsr      sr_vals
032600060316     c                   update    wfcic000
032700060316
032800060316     c                   enddo
032900060316
033000060316      * chiudo il file di work
033100060316     c                   close     wfcic01l
033200060316
033300060316     c                   endsr
033400060316
033500060316      *------------------------------------------------------------------------*
033600060316      * valorizzo il file di work con i dati relativi ai saldi
033700060316      *------------------------------------------------------------------------*
033800060316     c     sr_vals       begsr
033900060331
034000060331      * controllo quanti mesi ha lavorato
034100060331     c                   eval      xx = 1
034200060331     c                   for       xx by 1 to 12
034300060331     c                   if        skfat(xx) > *zeros
034400060331     c                   add       1             wfcmesi
034500060331     c                   endif
034600060331     c                   endfor
034700060316
034800060316     c                   eval      wfcft01 = skfat(01)
034900060316     c                   eval      wfcft02 = skfat(02)
035000060316     c                   eval      wfcft03 = skfat(03)
035100060316     c                   eval      wfcft04 = skfat(04)
035200060316     c                   eval      wfcft05 = skfat(05)
035300060316     c                   eval      wfcft06 = skfat(06)
035400060316     c                   eval      wfcft07 = skfat(07)
035500060316     c                   eval      wfcft08 = skfat(08)
035600060316     c                   eval      wfcft09 = skfat(09)
035700060316     c                   eval      wfcft10 = skfat(10)
035800060316     c                   eval      wfcft11 = skfat(11)
035900060316     c                   eval      wfcft12 = skfat(12)
036000060316
036100060316     c                   xfoot     skfat         wfcfat
036200060316
036300060316     c                   endsr
036400060316
036500060316      *------------------------------------------------------------------------*
036600060316      * elaborazione unificante
036700060316      *------------------------------------------------------------------------*
036800060316     c     sr_kun        begsr
036900060316
037000060316      * apro i file di work
037100060316     c                   open      wfcic00f
037200060316     c                   open      wfcic01l
037300060316
037400060316     c                   do        *hival
037500060316     c                   read      wfcic00f
037600060316     c                   if        %eof(wfcic00f)
037700060316     c                   leave
037800060316     c                   endif
037900060316
038000060316      * controlla se il cliente è padre (unificante)
038100060316     c                   clear                   tibs10ds
038200060316     c                   eval      d10drf = *date
038300060316     c                   eval      d10tle = 'ST'
038400060316     c                   eval      d10paf = 'F'
038500060316     c                   eval      d10cod = f_wfcksc
038600060316     c                   call      'TIBS10R'
038700060316     c                   parm                    tibs10ds
038800060316    1c                   if        d10err = *blanks
038900060316     c                   z-add     d10cod        f_wfcksu
039000060316     c                   eval      f_wfcflu = 'X'
039100060316     c                   eval      f_wfcrau = f_wfcrag
039200060316     c                   update    wfcicfff
039300060316
039400060316   x1c                   else
039500060316      * non è padre quindi
039600060316      * controlla se il cliente è figlio (unificato)
039700060316     c                   clear                   tibs10ds
039800060316     c                   eval      d10drf = *date
039900060316     c                   eval      d10tle = 'ST'
040000060316     c                   eval      d10paf = 'P'
040100060316     c                   eval      d10cod = f_wfcksc
040200060316     c                   call      'TIBS10R'
040300060316     c                   parm                    tibs10ds
040400060316    2c                   if        d10err = *blanks
040500060316     c                   z-add     d10cop        w0070
040600060323     c     w0070         chain(n)  wfcic01l
040700060316    3c                   if        %found(wfcic01l)
040800060316     c                   z-add     d10cop        f_wfcksu
040900060316     c                   eval      f_wfcrau = wfcrag
041000060316     c                   update    wfcicfff
041100060316    3c                   endif
041200060316    2c                   endif
041300060316    1c                   endif
041400060316
041500060316     c                   enddo
041600060316
041700060316      * chiudo i file di work
041800060316     c                   close     wfcic00f
041900060316     c                   close     wfcic01l
042000060316
042100060316     c                   endsr
042200060316
042300060316      *------------------------------------------------------------------------*
042400060329      * memorizzo il fatturato
042500060316      *------------------------------------------------------------------------*
042600060316     c     sr_fat        begsr
042700060329
042800060329      * --> calcolo il fatturato
042900060316
043000060316      * apro il file di work
043100060316     c                   open      wfcic02l
043200060316
043300060316     c     *loval        setll     wfcic02l
043400060316     c                   do        *hival
043500060316     c                   read      wfcic02l
043600060316     c                   if        %eof(wfcic02l)
043700060316     c                   leave
043800060316     c                   endif
043900060316
044000060316     c                   select
044100060316      * se cliente non unificato
044200060317     c                   when      wfcflu = *blanks and wfcksu = *zeros
044300060316     c                   clear                   skfat
044400060328     c                   clear                   wfcmedia
044500060328     c                   clear                   wfcfatp
044600060316      * carico il fatturato in schiera
044700060317     c                   eval      skfat(01) = wfcft01
044800060317     c                   eval      skfat(02) = wfcft02
044900060317     c                   eval      skfat(03) = wfcft03
045000060317     c                   eval      skfat(04) = wfcft04
045100060317     c                   eval      skfat(05) = wfcft05
045200060317     c                   eval      skfat(06) = wfcft06
045300060317     c                   eval      skfat(07) = wfcft07
045400060317     c                   eval      skfat(08) = wfcft08
045500060317     c                   eval      skfat(09) = wfcft09
045600060317     c                   eval      skfat(10) = wfcft10
045700060317     c                   eval      skfat(11) = wfcft11
045800060317     c                   eval      skfat(12) = wfcft12
045900060316      * se non ha lavorato 12 mesi calcolo la media e il totale proporzionato
046000060327     c                   if        wfcmesi < 12 and wfcmesi > 0
046100060327     c                   eval      wfcmedia = wfcfat / wfcmesi
046200060316     c                   eval      wfcfatp = wfcmedia * 12
046300060316     c                   endif
046400060331      * se ha lavorato 12 mesi imposto il proporzionato = al fatturato reale
046500060331     c                   if        wfcmesi = 12
046600060331     c                   eval      wfcfatp = wfcfat
046700060331     c                   endif
046800060316      * controllo se ha lavorato negli ultimi mesi richiesti
046900060316     c                   clear                   xx
047000080509     c                   clear                   zz
047100060327     c                   eval      yy = 12
047200060316     c                   eval      xx = 12 - i50umes
047300060327     c                   for       xx = xx + 1 by 1 to yy
047400060328     c                   if        skfat(xx) <> *zeros
047500060329     c                   add       1             zz
047600060316     c                   endif
047700060316     c                   endfor
047800070110     c                   if        zz = i50umes
047900060329     c                   clear                   wfcchk
048000060329     c                   endif
048100060316      * aggiorno il rcd per il cliente non unificato
048200060316     c                   update    wfcic002
048300060316     c                   clear                   skfat
048400060316
048500060316      * se cliente unificato
048600060316     c                   when      wfcksu <> *zeros
048700060316      * carico il fatturato in schiera
048800060317     c                   add       wfcft01       skfat(01)
048900060317     c                   add       wfcft02       skfat(02)
049000060317     c                   add       wfcft03       skfat(03)
049100060317     c                   add       wfcft04       skfat(04)
049200060317     c                   add       wfcft05       skfat(05)
049300060317     c                   add       wfcft06       skfat(06)
049400060317     c                   add       wfcft07       skfat(07)
049500060317     c                   add       wfcft08       skfat(08)
049600060317     c                   add       wfcft09       skfat(09)
049700060317     c                   add       wfcft10       skfat(10)
049800060317     c                   add       wfcft11       skfat(11)
049900060317     c                   add       wfcft12       skfat(12)
050000060316      * quando sto leggendo il codice padre
050100060317     c                   if        wfcflu = 'X'
050200060331     c                   clear                   wfcmesiu
050300060331     c                   clear                   wfcmedia
050400060331     c                   clear                   wfcfatp
050500060316     c                   xfoot     skfat         wfcfatu
050600060331      * controllo quanti mesi ha lavorato l'unificante
050700060316     c                   eval      xx = 1
050800060316     c                   for       xx by 1 to 12
050900060328     c                   if        skfat(xx) <> *zeros
051000060331     c                   add       1             wfcmesiu
051100060316     c                   endif
051200060316     c                   endfor
051300060316      * se non ha lavorato 12 mesi calcolo la media e il totale proporzionato
051400060331     c                   if        wfcmesiu < 12 and wfcmesiu > 0
051500060331     c                   eval      wfcmedia = wfcfatu / wfcmesiu
051600060316     c                   eval      wfcfatp = wfcmedia * 12
051700060316     c                   endif
051800060331      * se ha lavorato 12 mesi imposto il proporzionato = al fatturato reale
051900060331     c                   if        wfcmesiu = 12
052000060331     c                   eval      wfcfatp = wfcfatu
052100060331     c                   endif
052200060316      * controllo se ha lavorato negli ultimi mesi richiesti
052300060316     c                   clear                   xx
052400060329     c                   clear                   zz
052500060327     c                   eval      yy = 12
052600060327     c                   eval      xx = 12 - i50umes
052700060327     c                   for       xx = xx + 1 by 1 to yy
052800060328     c                   if        skfat(xx) <> *zeros
052900060329     c                   add       1             zz
053000060316     c                   endif
053100060316     c                   endfor
053200070110     c                   if        zz = i50umes
053300060329     c                   clear                   wfcchk
053400060329     c                   endif
053500060316      * aggiorno il rcd del codice padre
053600060316     c                   update    wfcic002
053700060316     c                   clear                   skfat
053800060316     c                   endif
053900060316
054000060316     c                   endsl
054100060316
054200060316     c                   enddo
054300060316
054400060316      * chiudo il file di work
054500060316     c                   close     wfcic02l
054600060316
054700060316     c                   endsr
054800060614
054900060614      *------------------------------------------------------------------------*
055000060614      * imposto il commerciale unificante del cliente unificante
055100060614      *------------------------------------------------------------------------*
055200060614     c     sr_com        begsr
055300060614
055400060614      * apro i file di work
055500060614     c                   open      wfcic02l
055600060614
055700060614     c     *loval        setll     wfcic02l
055800060614     c                   do        *hival
055900060614     c                   read      wfcic02l
056000060614     c                   if        %eof(wfcic02l)
056100060614     c                   leave
056200060614     c                   endif
056300060614
056400060614     c                   eval      kkcc = dutkci
056500060614
056600060614     c                   select
056700060614      * se cliente non unificato
056800060614     c                   when      wfcflu = *blanks and wfcksu = *zeros
056900060614      * aggancio l'anagrafica per recuperare il codice commerciale
057000060614      * del cliente non unificato
057100060614     c                   eval      kksc = wfcksc
057200060614      * se cliente unificato
057300060614     c                   when      wfcksu <> *zeros
057400060614      * aggancio l'anagrafica per recuperare il codice commerciale
057500060614      * del cliente unificante
057600060614     c                   eval      kksc = wfcksu
057700060614     c                   endsl
057800060614
057900060614      * cerco il codice commerciale
058000060614     c     kcnclp        chain(n)  cnclp00f
058100060614     c                   if        not %found(cnclp00f)
058200060614     c                   iter
058300060614     c                   endif
058400060614      * cerco il commerciale unificante
058500130902     c     CLPage        chain     AZCMM000
058600130902     c                   if        not %found(AZCMM01L)
058700060614     c                   iter
058800060614     c                   endif
058900130902     c                   movel     CMMuni        w0030
059000060614      * cerco distretto/area/p.o. del commerciale
059100060614     c     w0030         chain     azorg01l
059200060614     c                   if        not %found(azorg01l)
059300060614     c                   iter
059400060614     c                   endif
059500060614
059600060614      * aggiorno il record
059700130902     c                   z-add     CMMuni        wfccmm
059800060614     c                   z-add     orgfil        wfcfil
059900060614     c                   z-add     orgcar        wfcare
060000060614     c                   movel     orgfl3        wfcdis
060100060614     c                   update    wfcic002
060200060614
060300060614     c                   enddo
060400060614
060500060614      * chiudo i file di work
060600060614     c                   close     wfcic02l
060700060614
060800060614     c                   endsr
060900060316
061000060316      *------------------------------------------------------------------------*
061100060316      * controllo la categoria attuale con il fatturato e memorizzo la nuova
061200060316      *------------------------------------------------------------------------*
061300060316     c     sr_cat        begsr
061400060316
061500060316      * apro i file di work
061600060316     c                   open      wfcic00f
061700060316     c                   open      wfcic02l
061800060316
061900060316     c                   do        *hival
062000060316     c                   read      wfcic00f
062100060316     c                   if        %eof(wfcic00f)
062200060316     c                   leave
062300060316     c                   endif
062400060316
062500060316      * se non ha fatturato torno a leggere
062600060403     c                   if        f_wfcfatp = *zeros
062700060316     c                   iter
062800060316     c                   endif
062900060316
063000060316      * se non ha lavorato negli ultimi mesi richiesti torno a leggere
063100060329     c                   if        f_wfcchk <> *blanks
063200060316     c                   iter
063300060316     c                   endif
063400060316
063500060316      * elaboro solo i codici unificanti e i non unificati
063600060316    1c                   if        f_wfcflu = 'X'    or
063700060316     c                             (f_wfcflu = *blanks and
063800060316     c                              f_wfcksu = *zeros)
063900060316
064000060316     c                   select
064100060316     c                   when      f_wfcfatp > *zeros
064200060316     c                   eval      wrkfat = f_wfcfatp/1000
064300060316     c                   when      f_wfcflu <> 'X'
064400060316     c                   eval      wrkfat = f_wfcfat/1000
064500060316     c                   when      f_wfcflu = 'X'
064600060316     c                   eval      wrkfat = f_wfcfatu/1000
064700060316     c                   endsl
064800060316
064900060316    2c                   if        wrkfat >= i50icd  and
065000060316     c                             wrkfat <  i50ica
065100060316     c                   eval      f_wfcccp = 'C'
065200060316   x2c                   else
065300060316    3c                   if        wrkfat >= i50ibd  and
065400060316     c                             wrkfat <  i50iba
065500060316     c                   eval      f_wfcccp = 'B'
065600060316   x3c                   else
065700060316    4c                   if        wrkfat >= i50iad  and
065800060316     c                             wrkfat <  i50iaa
065900060316     c                   eval      f_wfcccp = 'A'
066000060316   x4c                   else
066100060316    5c                   if        wrkfat >= i50itd  and
066200060316     c                             wrkfat <  i50ita
066300060316     c                   eval      f_wfcccp = 'T'
066400060316   x5c                   else
066500060316    6c                   if        wrkfat >= i50idd
066600060316     c                   eval      f_wfcccp = 'D'
066700060316    6c                   endif
066800060316    5c                   endif
066900060316    4c                   endif
067000060316    3c                   endif
067100060316    2c                   endif
067200060316      * aggiorno il rcd con la nuova categoria calcolata
067300060316     c                   update    wfcicfff
067400060316      * aggiorno i codici legati all'unificante
067500060316     c                   if        f_wfcflu = 'X'
067600060316     c     f_wfcksu      setll     wfcic02l
067700060316     c                   do        *hival
067800060316     c     f_wfcksu      reade     wfcic02l
067900060316     c                   if        %eof(wfcic02l)
068000060316     c                   leave
068100060316     c                   endif
068200060316     c                   eval      wfcccp = f_wfcccp
068300060403     c                   eval      wfcchk = f_wfcchk
068400060316     c                   update    wfcic002
068500060316     c                   enddo
068600060316     c                   endif
068700060316
068800060316    1c                   endif
068900060316
069000060316     c                   enddo
069100060316
069200060316      * chiudo i file di work
069300060316     c                   close     wfcic00f
069400060316     c                   close     wfcic02l
069500060316
069600060316     c                   endsr
069700060614
069800060316      *------------------------------------------------------------------------*
069900060316      * stampa
070000060316      *------------------------------------------------------------------------*
070100060316     c     sr_stampa     begsr
070200060331
070300060331      * apro i file di work
070400060403     c                   open      wfcic03l
070500060331     c                   open      wfcic05l
070600060316
070700060316     c                   eval      stpdai = wmmaai
070800060316     c                   eval      stpdaf = wmmaaf
070900060316     c                   eval      stpumes = i50umes
071000060331
071100060331     c                   if        i50agg = 'S'
071200060331     c                   eval      stpagg = 'SI'
071300060331     c                   else
071400060331     c                   eval      stpagg = 'NO'
071500060331     c                   endif
071600060331
071700060331     c                   if        i50var = 'S'
071800060331     c                   eval      stpvar = 'SI'
071900060331     c                   else
072000060331     c                   eval      stpvar = 'NO'
072100060331     c                   endif
072200060614
072300060614     c                   if        i50blc = 'S'
072400060614     c                   eval      stpblc = 'SI'
072500060614     c                   else
072600060614     c                   eval      stpblc = 'NO'
072700060614     c                   endif
072800060316
072900060316     c                   eval      stpitd = i50itd
073000060316     c                   eval      stpita = i50ita
073100060316     c                   eval      stpiad = i50iad
073200060316     c                   eval      stpiaa = i50iaa
073300060316     c                   eval      stpibd = i50ibd
073400060316     c                   eval      stpiba = i50iba
073500060316     c                   eval      stpicd = i50icd
073600060316     c                   eval      stpica = i50ica
073700060316     c                   eval      stpidd = i50idd
073800060316     c                   eval      stpida = i50ida
073900060331
074000060331      * stampo la pagina dei parametri
074100060331     c                   write     st51p01
074200060316
074300060616      *------
074400060616
074500060324      * leggo il file di work
074600060331     c     *loval        setll     wfcic05l
074700060324     c                   do        *hival
074800060331     c                   read      wfcic05l
074900060324      * fine file
075000060331     c                   if        %eof(wfcic05l)
075100060324     c                   leave
075200060324     c                   endif
075300060331
075400060331      * elaboro solo i codici unificanti e i non unificati
075500060331     c                   if        i_wfcflu = *blanks and i_wfcksu > 0
075600060331     c                   iter
075700060331     c                   endif
075800060331      * se non ha lavorato nei mesi richiesti
075900060331     c                   if        i_wfcchk <> *blanks
076000060331     c                   iter
076100060331     c                   endif
076200060331
076300060331      * per prima cosa devo verificare nel caso di unificante se variato lui o uno dei figli
076400060331      * perchè devo stampare sempre tutto il gruppo se anche solo 1 figlio è variato
076500060331     c                   if        i_wfcflu = 'X'
076600060331     c                   exsr      sr_ctrrcd
076700060331     c                   if        wok = *off
076800060331     c                   iter
076900060331     c                   endif
077000060331     c                   endif
077100090421
077200090421      * spegno indicatore di comodo che pilota la stampa della riga vuota
077300090421     c                   eval      *in91 = *off
077400060316
077500060331      * stampa riga di dettaglio se unificante
077600060331     c                   if        i_wfcflu  = 'X'
077700060331     c                   exsr      sr_stadetu
077800060331     c                   else
077900060331      * stampa riga di dettaglio se singolo
078000060331     c                   exsr      sr_stadet
078100060331     c                   endif
078200060404      * salto riga
078300090421     c   91              write     st51d02
078400060316
078500060324     c                   enddo
078600060316
078700060316      * stampo le righe di chiusura prospetto
078800060316     c                   write     st51ri
078900060316     c                   write     st51fi
079000060316
079100060316      * chiudo i file di work
079200060403     c                   close     wfcic03l
079300060331     c                   close     wfcic05l
079400060316
079500060316     c                   endsr
079600060331
079700060331      *------------------------------------------------------------------------*
079800060331      * controllo se il rcd è da stampare
079900060331      *------------------------------------------------------------------------*
080000060331     c     sr_ctrrcd     begsr
080100060331
080200060331     c                   eval      wok = *off
080300060331
080400060331      * se già l'unificante è diverso sono a posto
080500060331     c                   if        i_wfccca <> i_wfcccp
080600060331     c                   eval      wok = *on
080700060331     c                   leavesr
080800060331     c                   endif
080900060331
081000060403      * altrimenti controllo i figli
081100060403     c     i_wfcksu      setll     wfcic03l
081200060331     c                   do        *hival
081300060403     c     i_wfcksu      reade     wfcic03l
081400060403     c                   if        %eof(wfcic03l)
081500060331     c                   leave
081600060331     c                   endif
081700060331     c                   if        wfcccp <> wfccca
081800060331     c                   eval      wok = *on
081900060331     c                   leave
082000060331     c                   endif
082100060331     c                   enddo
082200060331
082300060331     c                   endsr
082400060616
082500060616      *------------------------------------------------------------------------*
082600060616      * stampa il dettaglio riga per unificante
082700060616      *------------------------------------------------------------------------*
082800060616     c     sr_stadetu    begsr
082900060616
083000060616      * imposto altri dati della stampa
083100060616     c                   clear                   st51d01
083200060616
083300060616     c                   eval      stpksu  = i_wfcksu
083400060616     c                   eval      stprau  = %trim(i_wfcrau)
083500060616     c                   eval      stpfatu = i_wfcfatu
083600060616     c                   eval      stpmesu = i_wfcmesiu
083700060616     c                   eval      stpfatp = i_wfcfatp
083800060616     c                   eval      stpblk  = i_wfcblk
083900060616     c                   eval      stpksc  = i_wfcksc
084000060616     c                   eval      stprag  = %trim(i_wfcrag)
084100060616     c                   eval      stpfat  = i_wfcfat
084200060616     c                   eval      stpmesi = i_wfcmesi
084300060616     c                   eval      stpcca  = i_wfccca
084400060616     c                   if        i_wfccca <> i_wfcccp
084500060616     c                   eval      stpccp  = i_wfcccp
084600060616     c                   endif
084700060616
084800060616      * stampo testata a rottura
084900060616     c                   exsr      sr_testa
085000060616      * se fine pagina salto pagina
085100060616     c                   if        *in90
085200060616     c                   write     st51t01
085300060616     c                   eval      *in90 = *off
085400060616     c                   endif
085500060616      * stampo il dettaglio dell'unificante
085600060616     c                   write     st51d01
085700090421      * accendo indicatore per ok stampa riga vuota
085800090421     c                   eval      *in91 = *on
085900060616
086000060616      * reperisco i dati dei figli
086100060616     c                   clear                   st51d01
086200060616     c     i_wfcksu      setll     wfcic03l
086300060616     c                   do        *hival
086400060616     c     i_wfcksu      reade     wfcic03l
086500060616     c                   if        %eof(wfcic03l)
086600060616     c                   leave
086700060616     c                   endif
086800060616      * se stesso codice dell'unificante torno a leggere
086900060616     c                   if        wfcksu = wfcksc
087000060616     c                   iter
087100060616     c                   endif
087200060616     c                   eval      stpblk  = wfcblk
087300060616     c                   eval      stpksc  = wfcksc
087400060616     c                   eval      stprag  = %trim(wfcrag)
087500060616     c                   eval      stpfat  = wfcfat
087600060616     c                   eval      stpmesi = wfcmesi
087700060616     c                   eval      stpcca  = wfccca
087800060616     c                   clear                   stpccp
087900060616     c                   if        wfccca <> wfcccp
088000060616     c                   eval      stpccp  = wfcccp
088100060616     c                   endif
088200060616      * se richiesto non stampo se bloccato senza fatturato
088300060616     c                   if        i50blc = 'S' and stpblk = 'B' and stpfat = 0
088400060616     c                   iter
088500060616     c                   endif
088600060616      * se fine pagina salto pagina
088700060616     c                   if        *in90
088800060616     c                   write     st51t01
088900060616     c                   eval      *in90 = *off
089000060616     c                   endif
089100060616      * stampo il dettaglio dell'unificante
089200060616     c                   write     st51d01
089300060616     c                   enddo
089400060616
089500060616     c                   endsr
089600060616
089700060616      *------------------------------------------------------------------------*
089800060616      * stampa il dettaglio riga del singolo
089900060616      *------------------------------------------------------------------------*
090000060616     c     sr_stadet     begsr
090100060616
090200060616      * imposto altri dati della stampa
090300060616     c                   clear                   st51d01
090400060616     c                   eval      *in44 = *off
090500060616
090600060616     c                   eval      stpksu  = i_wfcksc
090700060616     c                   eval      stprau  = %trim(i_wfcrag)
090800060616     c                   eval      stpfatu = i_wfcfat
090900060616     c                   eval      stpmesu = i_wfcmesi
091000060616     c                   eval      stpfatp = i_wfcfatp
091100060616     c                   eval      stpblk  = i_wfcblk
091200060616     c                   eval      stpksc  = i_wfcksc
091300060616     c                   eval      stprag  = %trim(i_wfcrag)
091400060616     c                   eval      stpfat  = i_wfcfat
091500060616     c                   eval      stpmesi = i_wfcmesi
091600060616     c                   eval      stpcca  = i_wfccca
091700060616     c                   clear                   stpccp
091800060616     c                   if        i_wfccca <> i_wfcccp
091900060616     c                   eval      stpccp  = i_wfcccp
092000060616     c                   eval      *in44 = *on
092100060616     c                   endif
092200060616      * se richiesto non stampo se bloccato senza fatturato
092300060616     c                   if        i50blc = 'S' and stpblk = 'B' and stpfat = 0
092400060616     c                   leavesr
092500060616     c                   endif
092600060616
092700060616      * se richiesto stampo solo se categoria attuale <> dalla calcolata
092800060616     c                   if        i50var = 'N' or *in44
092900060616      * stampo testata a rottura
093000060616     c                   exsr      sr_testa
093100060616      * se fine pagina salto pagina
093200060616     c                   if        *in90
093300060616     c                   write     st51t01
093400060616     c                   eval      *in90 = *off
093500060616     c                   endif
093600090421      * stampo il dettaglio del cliente
093700090421     c                   write     st51d01
093800090421      * accendo indicatore per ok stampa riga vuota
093900090421     c                   eval      *in91 = *on
094000060616     c                   endif
094100060616
094200060616     c                   endsr
094300060316
094400060316      *------------------------------------------------------------------------*
094500060616      * stampo testata
094600060316      *------------------------------------------------------------------------*
094700060616     c     sr_testa      begsr
094800060616
094900060616      * cambio distretto
095000060616     c                   if        i_wfcdis <> depdis
095100060616     c                   eval      depdis = i_wfcdis
095200060616     c                   exsr      sr_decdis
095300060616      * imposto il distretto in testata
095400060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
095500060616     c                   eval      depare = i_wfcare
095600060616     c                   exsr      sr_decare
095700060616      * imposto l'area in testata
095800060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
095900060616     c                                    '  '+desare
096000060616     c                   eval      depfil = i_wfcfil
096100060616     c                   exsr      sr_decfil
096200060616      * imposto il p.o. in testata
096300060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
096400060616     c                                    '  '+desfil
096500060616     c                   eval      depcmm = i_wfccmm
096600060616     c                   exsr      sr_deccmm
096700060616      * imposto il commerciale in testata
096800060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
096900060627     c                                     %editc(depcmm:'Z')+
097000060627     c                                    '  '+descmm
097100060616      * stampo la testata
097200060616     c                   write     st51t01
097300060616     c                   endif
097400060616
097500060616      * cambio area
097600060616     c                   if        i_wfcare <> depare
097700060616     c                   eval      depdis = i_wfcdis
097800060616     c                   exsr      sr_decdis
097900060616      * imposto il distretto in testata
098000060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
098100060616     c                   eval      depare = i_wfcare
098200060616     c                   exsr      sr_decare
098300060616      * imposto l'area in testata
098400060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
098500060616     c                                    '  '+desare
098600060616     c                   eval      depfil = i_wfcfil
098700060616     c                   exsr      sr_decfil
098800060616      * imposto il p.o. in testata
098900060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
099000060616     c                                    '  '+desfil
099100060616     c                   eval      depcmm = i_wfccmm
099200060616     c                   exsr      sr_deccmm
099300060616      * imposto il commerciale in testata
099400060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
099500060627     c                                     %editc(depcmm:'Z')+
099600060627     c                                    '  '+descmm
099700060616      * stampo la testata
099800060616     c                   write     st51t01
099900060616     c                   endif
100000060616
100100060616      * cambio p.o.
100200060616     c                   if        i_wfcfil <> depfil
100300060616     c                   eval      depdis = i_wfcdis
100400060616     c                   exsr      sr_decdis
100500060616      * imposto il distretto in testata
100600060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
100700060616     c                   eval      depare = i_wfcare
100800060616     c                   exsr      sr_decare
100900060616      * imposto l'area in testata
101000060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
101100060616     c                                    '  '+desare
101200060616     c                   eval      depfil = i_wfcfil
101300060616     c                   exsr      sr_decfil
101400060616      * imposto il p.o. in testata
101500060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
101600060616     c                                    '  '+desfil
101700060616     c                   eval      depcmm = i_wfccmm
101800060616     c                   exsr      sr_deccmm
101900060616      * imposto il commerciale in testata
102000060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
102100060627     c                                     %editc(depcmm:'Z')+
102200060627     c                                    '  '+descmm
102300060616      * stampo la testata
102400060616     c                   write     st51t01
102500060616     c                   endif
102600060616
102700060616      * cambio commerciale
102800060616     c                   if        i_wfccmm <> depcmm
102900060616     c                   eval      depdis = i_wfcdis
103000060616     c                   exsr      sr_decdis
103100060616      * imposto il distretto in testata
103200060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
103300060616     c                   eval      depare = i_wfcare
103400060616     c                   exsr      sr_decare
103500060616      * imposto l'area in testata
103600060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
103700060616     c                                    '  '+desare
103800060616     c                   eval      depfil = i_wfcfil
103900060616     c                   exsr      sr_decfil
104000060616      * imposto il p.o. in testata
104100060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
104200060616     c                                    '  '+desfil
104300060616     c                   eval      depcmm = i_wfccmm
104400060616     c                   exsr      sr_deccmm
104500060616      * imposto il commerciale in testata
104600060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
104700060627     c                                     %editc(depcmm:'Z')+
104800060627     c                                    '  '+descmm
104900060616      * stampo la testata
105000060616     c                   write     st51t01
105100060616     c                   endif
105200060616
105300060616     c                   endsr
105400060616      *------------------------------------------------------------------------*
105500060616      * decodifico il distretto
105600060616      *------------------------------------------------------------------------*
105700060616     c     sr_decdis     begsr
105800060316
105900060316      * decodifico il distretto
106000060324     c                   eval      kcod = '17'
106100060324     c                   eval      kkey = depdis
106200060324     c     ktabel        chain     tabel00f
106300060324     c                   if        not %found(tabel00f) or tblflg <> *blanks
106400060324     c                   eval      desdis = *all'*'
106500060324     c                   else
106600060324     c                   eval      desdis = tbluni
106700060324     c                   endif
106800060316
106900060316     c                   endsr
107000060324
107100060316      *------------------------------------------------------------------------*
107200060316      * decodifco area
107300060316      *------------------------------------------------------------------------*
107400060316     c     sr_decare     begsr
107500060316
107600060316     c                   eval      kcod = '05'
107700060317     c                   movel     depare        kkey
107800060316     c     ktabel        chain     tabel00f
107900060316     c                   if        not %found(tabel00f) or tblflg <> *blanks
108000060316     c                   eval      desare = *all'*'
108100060316     c                   else
108200060316     c                   eval      desare = tbluni
108300060316     c                   endif
108400060316
108500060316     c                   endsr
108600060316
108700060316      *------------------------------------------------------------------------*
108800060316      * decodifico p.o.
108900060316      *------------------------------------------------------------------------*
109000060316     c     sr_decfil     begsr
109100060316
109200060316     c     depfil        chain     azorg01l
109300060316     c                   if        not %found(azorg01l) or orgfva <> *blanks
109400060316     c                   eval      desfil = *all'*'
109500060316     c                   else
109600060316     c                   eval      desfil = orgdes
109700060316     c                   endif
109800060316
109900060316     c                   endsr
110000130902      *
110100060614      *------------------------------------------------------------------------*
110200060614      * decodifco commerciale
110300060614      *------------------------------------------------------------------------*
110400060614     c     sr_deccmm     begsr
110500130902      *
110600130902     c     DEPcmm        chain     AZCMM000
110700130902     c                   if        not %found(AZCMM01L)
110800060614     c                   eval      descmm = *all'*'
110900060614     c                   else
111000130902     c                   eval      descmm = CMMdes
111100060614     c                   endif
111200130902      *
111300060614     c                   endsr
111400130902      *
111500060327      *------------------------------------------------------------------------*
111600060327      * aggiorna la categoria del cliente
111700060327      *------------------------------------------------------------------------*
111800060327     c     sr_aggiorna   begsr
111900060327
112000060327      * apro i file di work
112100060327     c                   open      wfcic01l
112200060327
112300060327      * leggo il file di work per cliente
112400060327     c     *loval        setll     wfcic01l
112500060327     c                   do        *hival
112600060327     c                   read      wfcic01l
112700060327     c                   if        %eof(wfcic01l)
112800060327     c                   leave
112900060327     c                   endif
113000060327      * se ok il rcd e categoria variata aggiorno
113100060331     c                   if        wfccca <> wfcccp
113200060918      * prima di aggiornare devo memorizzare sullo storico variazioni l'immagine prima della
113300060918      * variazione
113400060919     c                   clear                   tibs69ds
113500060919     c                   eval      i69kac = wfcksc
113600060919     c                   eval      i69kin = wfcksc
113700060919     c                   eval      i69kcp = wfcksc
113800060919     c                   eval      i69kcs = wfcksc
113900060919     c                   call      'TIBS69R'
114000060919     c                   parm                    tibs69ds
114100060919     c                   parm                    cnacods
114200060919     c                   parm                    cnindds
114300060919     c                   parm                    cnclpds
114400060919     c                   parm                    fnclsds
114500060919
114600060919     c                   clear                   tibs73ds
114700060919     c                   eval      ibs73pru = knmus
114800060919     c                   eval      ibs73noj = 'TIST51R'
114900060918     c                   eval      ibs73pgm = 'TIST51R'
115000060918     c                   eval      ibs73immag = 'P'
115100060918     c                   eval      ibs73cta = 'M'
115200060918     c                   eval      ibs73yda = 'C'
115300060919     c                   call      'TIBS73R'
115400060919     c                   parm                    tibs73ds
115500060919     c                   parm                    cnacods
115600060919     c                   parm                    cnindds
115700060919     c                   parm                    cnclpds
115800060919     c                   parm                    fnclsds
115900060918
116000060327      * aggiorno direttamente cnclp
116100060404     c                   eval      kkcc = dutkci
116200060404     c                   eval      kksc = wfcksc
116300060404     c     kcnclp        chain     cnclp00f
116400060404     c                   if        %found(cnclp00f)
116500060404     c                   eval      clpclv = wfcccp
116600060404     c                   update    cnclp000
116700060404     c                   endif
116800060918      * ora devo memorizzare sullo storico variazioni l'immagine dopo la variazione
116900060919     c                   clear                   tibs73ds
117000060919     c                   eval      ibs73pru = knmus
117100060919     c                   eval      ibs73noj = 'TIST51R'
117200060918     c                   eval      ibs73pgm = 'TIST51R'
117300060918     c                   eval      ibs73immag = 'D'
117400060918     c                   eval      ibs73cta = 'M'
117500060918     c                   eval      ibs73yda = 'C'
117600060919     c                   call      'TIBS73R'
117700060919     c                   parm                    tibs73ds
117800060919     c                   parm                    cnacods
117900060919     c                   parm                    cnindds
118000060919     c                   parm                    cnclpds
118100060919     c                   parm                    fnclsds
118200060403      * aggiorno ancln
118300060404      * bloccato il trigger che aggiorna cnclp
118400060330     c                   eval      ksoc = '201'
118500060330     c                   eval      kkcca = *all'0'
118600060330     c                   move      dutkci        kkcca
118700060330     c                   eval      kksca = *all'0'
118800060330     c                   move      wfcksc        kksca
118900060327     c     kybacl        chain     ancln10l
119000060327     c                   if        %found(ancln10l)
119100060327     c                   eval      clnclasabc = wfcccp
119200060327     c                   update    ancln000
119300060327     c                   endif
119400060327     c                   endif
119500060327
119600060327     c                   enddo
119700060327
119800060327      * chiudo i file di work
119900060327     c                   close     wfcic01l
120000060327
120100060327     c                   endsr
120200060316
120300060316      *------------------------------------------------------------------------*
120400060316      * routine iniziale
120500060316      *------------------------------------------------------------------------*
120600060316     c     *inzsr        begsr
120700060316
120800060316     c     *entry        plist
120900060316     c                   parm                    kpjba
121000060316     c                   eval      tist50ds = kpjbu
121100060316
121200060316     c     *dtaara       define    §azute        azuteds
121300060316     c     *dtaara       define    §datiute      ddatiute
121400060316     c                   in(e)     *dtaara
121500060316     c                   if        %error  or rsut = *blanks
121600060316     c                   clear                   tibs34ds
121700060316     c                   call      'TIBS34R'
121800060316     c                   parm                    tibs34ds
121900060316     c                   in        *dtaara
122000060316     c                   endif
122100060316
122200060317     c                   movel(p)  rsut          dsfirs
122300060316
122400060316     c                   movel     i50ami        waai
122500060316     c                   move      i50ami        wmmi
122600060316     c                   movel     i50amf        waaf
122700060316     c                   move      i50amf        wmmf
122800060317
122900060317     c                   time                    wora
123000060316
123100060316     c     kybacl        klist
123200060316     c                   kfld                    ksoc
123300060327     c                   kfld                    kkcca
123400060316     c                   kfld                    kksca
123500060316
123600060316     c     ktabel        klist
123700060316     c                   kfld                    codut
123800060316     c                   kfld                    kcod
123900060316     c                   kfld                    kkey
124000060327
124100060404     c     kcnclp        klist
124200060404     c                   kfld                    codut
124300060404     c                   kfld                    kkcc
124400060404     c                   kfld                    kksc
124500060316
124600060316     c                   endsr
124700000914
124800060316** cmd
124900001124CLRPFM FILE(WFCIC00F)
