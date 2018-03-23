000100070716     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070716     h dftactgrp(*no) actgrp(*caller)
000300070716
000400070716      *------------------------------------------------------------------------*
000500070716      *
000600070716      *         Assegnazione distinta automatica
000700070716      *         Aggiorno riferimenti distinta su ORM partendo da ORG
000800070716      *
000900070716      *------------------------------------------------------------------------*
001000070802     ffnorm01l  uf   e           k disk    commit infds(ormds)
001100070717     ffnorf01l  uf a e           k disk    commit
001200070716     ffnorg01l  uf   e           k disk    commit
001300070716     fazorg01l  if   e           k disk
001400080730     ffidst01l  if   e           k disk
001500070913     ffior63p   o    e             printer infds(prterr)
001600080715     ffior61p   o    e             printer
001700070716
001800070716      *------------------------------------------------------------------------*
001900070716      *  RIEPILOGO INDICATORI
002000070716      *------------------------------------------------------------------------*
002100070716      *  80 - of printer file
002200070716      *------------------------------------------------------------------------*
002300070716
002400070716      *------------------------------------------------------------------------*
002500070716      *   V A R I A B I L I
002600070716      *------------------------------------------------------------------------*
002700070716     d comddao         s              8  0
002800070716     d dateu           s              8  0
002900070716     d dtaeur          s               d   datfmt(*eur)
003000070716     d dtaiso          s               d
003100070717     d savormdfo       s                   like(ormdfo)
003200070716     d savormfao       s                   like(ormfao)
003300070717     d savormofo       s                   like(ormofo)
003400080317     d wmetti          s               n
003500140212     d wndc            s                   like(dstnfv)
003600070731     d wnpg            s                   like(ormnpg) inz(4)
003700070716     d w0140           s             14  0
003800070716
003900070716      *------------------------------------------------------------------------*
004000070716      *   S C H I E R E
004100070716      *------------------------------------------------------------------------*
004200070716     d msg             s             78    dim(01) ctdata perrcd(1)
004300070716
004400070716      *------------------------------------------------------------------------*
004500070716      *   D S   I N T E R N E / E S T E R N E
004600070716      *------------------------------------------------------------------------*
004700070802     d ormds           ds
004800070716     d  ormnrr               397    400i 0
004900070913
005000070913     d prterr          ds
005100070913     d  riga                 367    368i 0
005200070716
005300070716     d wlbdat          ds
005400070716     d  g08dat                 1      8  0
005500070716     d  g08inv                 9     16  0
005600070716     d  g08err                17     17
005700070716     d  g08tgi                18     22  0
005800080303
005900080303     d fnorfds         ds
006000080303     d orffgs                  1      3  0
006100080303     d orfndc                  4      9  0
006200080303     d orfddc                 10     17  0
006300170111     d worfass                18     18
006400070716
006500070716     d azuteds       e ds                  extname(azute00f)
006600070716     d ddatiute      e ds
006700070716     d dfar          e ds
006800170111     d dORF01        e ds
006900070716     d fior61ds      e ds
007000140212     d fior82ds      e ds
007100170111     d FIOR90DS      e ds
007200070716     d fnorgds       e ds                  extname(fnorg00f)
007300070717     d fnormds       e ds                  extname(fnorm00f)
007400070716     d kpjba         e ds
007500070716     d og147         e ds
007600070717     d og148         e ds
007700070716     d tibs02ds      e ds
007800070716     d tibs34ds      e ds                  inz
007900070716     d trul82ds      e ds
008000070716
008100140212      /copy gaitrasrc/srcprotopr,fior82r
008200170111
008300170111      // - Pgm controllo fasi
008400170111     d FIOR90R         pr                  extpgm('FIOR90R')
008500170111     d  kpjba                              likeds(kpjba)
008600170111     d  fior90ds                           likeds(fior90ds)
008700070716      *------------------------------------------------------------------------*
008800071030
008900071030     c/exec sql
009000071030     c+ set option dynusrprf = *owner, closqlcsr = *endmod
009100071030     c/end-exec
009200070716
009300070716      * metto in distinta
009400070716
009500070716     c                   if        or61mtdi = 'M'
009600070716     c                   exsr      sr_metti
009700070716     c                   endif
009800070716
009900070716      * tolgo dalla distinta
010000070716     c                   if        or61mtdi = 'T'
010100070716     c                   exsr      sr_togli
010200070716     c                   endif
010300070905
010400070905      * trasferisco da una distinta all'altra
010500070905     c                   if        or61mtdi = 'X'
010600070905     c                   exsr      sr_trasferisci
010700070905     c                   endif
010800070716
010900070716     c                   if        or61cmti= 'S'
011000070716     c                   commit
011100070716     c                   endif
011200070716
011300070716     c                   eval      or61pag = pages
011400070716     c                   eval      or61riga= riga
011500070716
011600070716     c                   eval      *inlr = *on
011700070716
011800070716      *------------------------------------------------------------------------*
011900070716      * Leggo i ritiri messi in distinta su ORG e li inserisco su ORM
012000070716      *------------------------------------------------------------------------*
012100070716     c     sr_metti      begsr
012200070716
012300070716     C/EXEC SQL
012400070716     C+ DECLARE A1 CURSOR FOR SELECT fnorm01l.*, fnorg03l.*
012500070904     C+ FROM fnorm01l join fnorg03l on
012600070716     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
012700070716     C+ ormnrv=orgnrv
012800070717     C+ WHERE orgndc=:or61nfvi and orgddc=:or61dfvi and
012900071107     C+ orgfgs =:or61fgsi and ormndc=0 and ormeti <> ' '
013000070716     C+ ORDER BY fnorg03l.orgfgs, fnorg03l.orgndc, fnorg03l.orgddc
013100070716     C/END-EXEC
013200070716
013300070716     C/EXEC SQL
013400070716     C+ OPEN A1
013500070716     C/END-EXEC
013600070716
013700070716     c                   do        *hival
013800070716
013900070716     C/EXEC SQL
014000070716     C+ FETCH NEXT FROM A1 INTO :fnormds, :fnorgds
014100070716     C/END-EXEC
014200070716
014300070716     c                   select
014400070716     c                   when      sqlcod = 100
014500070716     c                   leave
014600070716     c                   when      sqlcod < 0
014700080715      * record non allocato
014800070716     c                   if        sqlcod <> -913
014900080715     c                   exsr      sr_stampaman
015000080715     c                   exsr      sr_fine
015100070716     c                   endif
015200080715      * record allocato
015300070716     c                   exsr      sr_stampa
015400070716     c                   exsr      sr_fine
015500070716
015600070716     c                   other
015700071001      * solo se non chiamato per bolle RC
015800071001     c                   if        or61rc = *blanks
015900070807      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
016000070807      * solo automatiche (or61nftl = 0)
016100070716     c                   if        or61nftl = 99999 and orgnftl <> 99999
016200070716     c                   iter
016300070716     c                   endif
016400070716     c                   if        or61nftl <> 99999 and orgnftl = 99999
016500070716     c                   iter
016600070716     c                   endif
016700071001     c                   endif
016800070716
016900070716     c                   exsr      sr_mettiorm
017000070716     c                   endsl
017100070716
017200070716     c                   enddo
017300070716
017400070716     C/EXEC SQL
017500070716     C+ CLOSE A1
017600070716     C/END-EXEC
017700070717
017800070717     c                   endsr
017900070716
018000070717      *------------------------------------------------------------------------*
018100070717      * Leggo i ritiri che sono ancora in distinta su ORM e che sono da togliere
018200070717      *------------------------------------------------------------------------*
018300070717     c     sr_togli      begsr
018400070717
018500070717     C/EXEC SQL
018600080303     C+ DECLARE B1 CURSOR FOR SELECT distinct  fnorm01l.*,
018700080303     C+ fnorf01l.orffgs, fnorf01l.orfndc,
018800080314     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as §orfass,
018900070717     C+ fnorg01l.*
019000070717     C+ FROM (fnorm01l join fnorf01l on
019100070717     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
019200070717     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
019300070717     C+ ormofo=orfore) join fnorg01l on
019400070717     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
019500070717     C+ ormnrv=orgnrv
019600070717     C+ WHERE orfndc=:or61nfvi and orfddc=:or61dfvi and
019700080221     C+ orffgs =:or61fgsi and orgndc=0 and ormndc > 0
019800070717     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
019900070717     C/END-EXEC
020000070717
020100070717     C/EXEC SQL
020200070717     C+ OPEN B1
020300070717     C/END-EXEC
020400070717
020500070717     c                   do        *hival
020600070717
020700070717     C/EXEC SQL
020800070717     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorfds, :fnorgds
020900070717     C/END-EXEC
021000070717
021100070717     c                   select
021200070717     c                   when      sqlcod = 100
021300070717     c                   leave
021400070717     c                   when      sqlcod < 0
021500080715      * record non allocato
021600070717     c                   if        sqlcod <> -913
021700080715     c                   exsr      sr_stampaman
021800080715     c                   exsr      sr_fine
021900070717     c                   endif
022000080715      * record allocato
022100070717     c                   exsr      sr_stampa
022200070717     c                   exsr      sr_fine
022300070717
022400070717     c                   other
022500071001      * solo se non chiamato per bolle RC
022600071001     c                   if        or61rc = *blanks
022700070717      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
022800070717      * solo automatiche (or61nftl = 0)
022900070921     c                   if        or61nftl = 99999 and §orfass <> 'M'
023000070717     c                   iter
023100070717     c                   endif
023200070921     c                   if        or61nftl = 0      and §orfass = 'M'
023300070717     c                   iter
023400070717     c                   endif
023500071001     c                   endif
023600070717
023700070717     c                   exsr      sr_togliorm
023800070717     c                   endsl
023900070717
024000070717     c                   enddo
024100070717
024200070717     C/EXEC SQL
024300070717     C+ CLOSE B1
024400070717     C/END-EXEC
024500070717
024600070717     c                   endsr
024700070905
024800070905      *------------------------------------------------------------------------*
024900070905      * Leggo i ritiri che sono ancora in distinta su ORM e che sono da trasferire
025000070905      *------------------------------------------------------------------------*
025100070905     c     sr_trasferiscibegsr
025200070905
025300070905     C/EXEC SQL
025400080303     C+ DECLARE C1 CURSOR FOR SELECT distinct  fnorm01l.*,
025500080303     C+ fnorf01l.orffgs, fnorf01l.orfndc,
025600080314     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as §orfass,
025700070905     C+ fnorg01l.*
025800070905     C+ FROM (fnorm01l join fnorf01l on
025900070905     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
026000070905     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
026100070905     C+ ormofo=orfore) join fnorg01l on
026200070905     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
026300070905     C+ ormnrv=orgnrv
026400070905     C+ WHERE orfndc=:or61nfvi and orfddc=:or61dfvi and
026500070905     C+ orffgs =:or61fgsi and orgndc=:or61nfvin
026600070905     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
026700070905     C/END-EXEC
026800070905
026900070905     C/EXEC SQL
027000070905     C+ OPEN C1
027100070905     C/END-EXEC
027200070905
027300070905     c                   do        *hival
027400070905
027500070905     C/EXEC SQL
027600070905     C+ FETCH NEXT FROM C1 INTO :fnormds, :fnorfds, :fnorgds
027700070905     C/END-EXEC
027800070905
027900070905     c                   select
028000070905     c                   when      sqlcod = 100
028100070905     c                   leave
028200070905     c                   when      sqlcod < 0
028300080715      * record non allocato
028400070905     c                   if        sqlcod <> -913
028500080715     c                   exsr      sr_stampaman
028600080715     c                   exsr      sr_fine
028700070905     c                   endif
028800080715      * record allocato
028900070905     c                   exsr      sr_stampa
029000070905     c                   exsr      sr_fine
029100070905
029200070905     c                   other
029300071001      * solo se non chiamato per bolle RC
029400071001     c                   if        or61rc = *blanks
029500070905      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
029600070905      * solo automatiche (or61nftl = 0)
029700070921     c                   if        or61nftl = 99999 and §orfass <> 'M'
029800070905     c                   iter
029900070905     c                   endif
030000070921     c                   if        or61nftl = 0      and §orfass = 'M'
030100070905     c                   iter
030200070905     c                   endif
030300071001     c                   endif
030400070905
030500070905     c                   exsr      sr_togliorm
030600070905     c                   exsr      sr_mettiorm
030700070905     c                   endsl
030800070905
030900070905     c                   enddo
031000070905
031100070905     C/EXEC SQL
031200070905     C+ CLOSE C1
031300070905     C/END-EXEC
031400070905
031500070905     c                   endsr
031600070717
031700070716      *------------------------------------------------------------------------*
031800070716      * Aggiorna ORM con riferimenti distinta
031900070716      *------------------------------------------------------------------------*
032000070716     c     sr_mettiorm   begsr
032100070716
032200070716      * controllo che l'ORM non sia allocato
032300070716     c     korm          chain(e)  fnorm01l
032400070206     c                   if        %error
032500070716      * se allocato mando msg a chi alloca e reperisco il RRN del file
032600070716     c     korm          chain(n)  fnorm01l
032700070716     c                   if        %found(fnorm01l)
032800070716     c                   exsr      sr_geslck
032900070716     c                   if        ul82§sts = 'A'
033000070716     c                   eval      *in85 = *on
033100070716     c                   exsr      sr_stampa
033200071030     c                   exsr      sr_fine
033300070716     c                   endif
033400070716      * non allocato impegno l'ORM
033500070716     c     korm          chain     fnorm01l
033600070716     c                   endif
033700070716     c                   endif
033800070716      * non trovo ORM errore
033900070716     c                   if        not %found(fnorm01l)
034000070716     c                   eval      *in84 = *on
034100070716     c                   exsr      sr_stampa
034200071030     c                   exsr      sr_fine
034300070716     c                   endif
034400070716      * controllo orm
034500070716     c                   if        ormndc <> *zeros
034600070716     c                   eval      *in85 = *on
034700070716     c                   exsr      sr_stampa
034800071030     c                   exsr      sr_fine
034900070716     c                   endif
035000090717      * l'orm deve essere ancora in fase assegnabil
035100090717     c                   if        ormeti = ''
035200090717     c                   eval      *in85 = *on
035300090717     c                   exsr      sr_stampa
035400090717     c                   exsr      sr_fine
035500090717     c                   endif
035600070716
035700070716      * mi salvo i dati della fase presente sulla testa ORM per salvarla su ORG
035800070716     c                   eval      savormfao = ormfao
035900070717     c                   eval      savormdfo = ormdfo
036000070717     c                   eval      savormofo = ormofo
036100070716
036200070716      * imposto la distinta su ORM e scrivo ORF
036300070716     c                   eval      ormnpg = wnpg
036400070716     c                   eval      ormndc = orgndc
036500070716     c                   eval      ormddc = orgddc
036600070716     c                   eval      ormfao = 400
036700070716     c                   time                    w0140
036800070716     c                   clear                   wlbdat
036900070716     c                   move      w0140         g08dat
037000070716     c                   call      'XSRDA8'
037100070716     c                   parm                    wlbdat
037200070716     c                   z-add     g08inv        ormdfo
037300070716     c                   movel     w0140         ormofo
037400070716     c                   z-add     g08inv        ormdtt
037500070716     c                   exsr      sr_fase
037600070716     c                   eval      ormeti = d§farass
037700070716
037800080317     c                   eval      wmetti = *on
037900070716     c                   exsr      sr_wrtorf
038000070716
038100070716     c     korm          chain     fnorg01l
038200070716     c                   if        %found(fnorg01l)
038300070716     c                   eval      orgfao = savormfao
038400070717     c                   eval      orgdfo = savormdfo
038500070717     c                   eval      orgofo = savormofo
038600070716     c                   update    fnorg000
038700070716     c                   endif
038800070716
038900070716     c                   update    fnorm000
039000070716
039100070716     c                   endsr
039200070717
039300070717      *------------------------------------------------------------------------*
039400070717      * Aggiorna ORM con fase precedente
039500070717      *------------------------------------------------------------------------*
039600070717     c     sr_togliorm   begsr
039700070717
039800070717      * controllo che l'ORM non sia allocato
039900070717     c     korm          chain(e)  fnorm01l
040000070717     c                   if        %error
040100070717      * se allocato mando msg a chi alloca e reperisco il RRN del file
040200070717     c     korm          chain(n)  fnorm01l
040300070717     c                   if        %found(fnorm01l)
040400070717     c                   exsr      sr_geslck
040500070717     c                   if        ul82§sts = 'A'
040600070717     c                   eval      *in85 = *on
040700070717     c                   exsr      sr_stampa
040800071030     c                   exsr      sr_fine
040900070717     c                   endif
041000070717      * non allocato impegno l'ORM
041100070717     c     korm          chain     fnorm01l
041200070717     c                   endif
041300070717     c                   endif
041400070717      * non trovo ORM errore
041500070717     c                   if        not %found(fnorm01l)
041600070717     c                   eval      *in84 = *on
041700070717     c                   exsr      sr_stampa
041800071030     c                   exsr      sr_fine
041900070717     c                   endif
042000070717      * controllo orm
042100070717     c                   if        ormndc = *zeros
042200070717     c                   eval      *in85 = *on
042300070717     c                   exsr      sr_stampa
042400071030     c                   exsr      sr_fine
042500070717     c                   endif
042600080312
042700080312      * se ORM con esito da PDA non devo impostare la fase precedente
042800080312      * ma creare la fase 390
042900080312     c                   if        ormfao = 410 or ormfao = 420
043000080312      * tolgo la distinta su ORM e scrivo ORF fase 390
043100080312     c                   clear                   ormnpg
043200080312     c                   clear                   ormndc
043300080312     c                   clear                   ormddc
043400080312     c                   eval      ormfao = 390
043500080312     c                   time                    w0140
043600080312     c                   clear                   wlbdat
043700080312     c                   move      w0140         g08dat
043800080312     c                   call      'XSRDA8'
043900080312     c                   parm                    wlbdat
044000080312     c                   z-add     g08inv        ormdfo
044100080312     c                   movel     w0140         ormofo
044200080312     c                   z-add     g08inv        ormdtt
044300080312     c                   exsr      sr_fase
044400080312     c                   eval      ormeti = d§farass
044500080312      * cancello la data di stampa (ORM Comunicato)
044600080312     c                   clear                   ormdst
044700080312      * scrivo la fase 390
044800080317     c                   eval      wmetti = *off
044900080312     c                   exsr      sr_wrtorf
045000080312      * tolgo fase precedente da ORG
045100080312     c     korm          chain     fnorg01l
045200080312     c                   if        %found(fnorg01l)
045300080312     c                   clear                   orgfao
045400080312     c                   clear                   orgdfo
045500080312     c                   clear                   orgofo
045600080312     c                   update    fnorg000
045700080312     c                   endif
045800080312
045900080312     c                   update    fnorm000
046000080312
046100080312      * se non ha avuto esito da PDA
046200080312     c                   else
046300070717
046400070717      * aggancio ORG per recuperare la fase precedente
046500070717     c     korm          chain     fnorg01l
046600070717     c                   if        %found(fnorg01l)
046700070717     c                   eval      savormfao = orgfao
046800070717     c                   eval      savormdfo = orgdfo
046900070717     c                   eval      savormofo = orgofo
047000070717     c                   clear                   orgfao
047100070717     c                   clear                   orgdfo
047200070717     c                   clear                   orgofo
047300070717     c                   update    fnorg000
047400070717     c                   endif
047500070717
047600070717      * cancello la fase da ORF
047700070717     c     korm1         chain     fnorf01l
047800070717     c                   if        %found(fnorg01l)
047900070717     c                   delete    fnorf000
048000070717     c                   endif
048100070717
048200070717      * cancello la distinta su ORM e imposto la fase precedente
048300070717     c                   clear                   ormnpg
048400070717     c                   clear                   ormndc
048500070717     c                   clear                   ormddc
048600070717     c                   eval      ormfao = savormfao
048700070717     c                   eval      ormdfo = savormdfo
048800070717     c                   eval      ormofo = savormofo
048900070717     c                   exsr      sr_fase
049000070717     c                   eval      ormeti = d§farass
049100070921      * cancello la data di stampa (ORM Comunicato)
049200070921     c                   clear                   ormdst
049300070717     c                   update    fnorm000
049400080312
049500080312     c                   endif
049600080911
049700080911      * aggancio la distinta per recupera l'AUT
049800080911     c                   eval      dstnpg = 4
049900080911     c     kfidst        chain     fidst01l
050000080911     c                   if        not %found(fidst01l)
050100080911     c                   clear                   dstpdr
050200080911     c                   endif
050300080911      * cancello la richiesta info
050400140212      /free
050500140212         clear fior82ds;
050600140212         c82fgs = or61fgsi;
050700140212         c82ndc = wndc ;
050800140212         c82codaut = dstpdr;
050900140212         c82POE = ormpoe;
051000140212         c82NSR = ormnsr;
051100140212         c82NOR = ormnor;
051200140212         c82NRV = ormnrv;
051300140212         c82aggvis = 'S';
051400140212         kpjbu = fior82ds;
051500140212         fior82r(kpjba);
051600140212         fior82ds = kpjbu;
051700070717
051800140212      /end-free
051900070717     c                   endsr
052000070716
052100070716      *------------------------------------------------------------------------*
052200070716      * Stampa errore
052300070716      *------------------------------------------------------------------------*
052400070716     c     sr_stampa     begsr
052500070716
052600070716     c                   move      or61dfvi      dtaiso
052700070227     c                   move      dtaiso        dtaeur
052800070227     c                   move      dtaeur        prtdfv
052900070716     c                   move      orgndc        prtnfv
053000070716
053100070704     c                   if        *in85
053200070716     c                   eval      prtpoe = orgpoe
053300070716     c                   eval      prtnsr = orgnsr
053400070716     c                   eval      prtnor = orgnor
053500070716     c                   eval      prtnrv = orgnrv
053600070716     c                   eval      prtmsg = 'L''ORM non è stato aggiornato corr-
053700070716     c                             ettamente. VERIFICARLO'
053800070716     c                   endif
053900070716
054000070716      * pagina in stampa
054100070913     c                   if        riga >= 60
054200070716     c                   eval      *in80 = *on
054300070716     c                   add       1             pages
054400070716     c                   eval      riga = 3
054500070716     c                   endif
054600070716
054700070913     c   80              write     testa
054800070716     c                   eval      *in80 = *off
054900070716
055000070319     c   82              write     riga2
055100070716     c   82              add       1             riga
055200070716
055300070227     c   84              write     riga4
055400070716     c   84              add       1             riga
055500070716
055600070704     c   85              write     riga5
055700070716     c   85              add       1             riga
055800070716
055900070704     c                   setoff                                       828485
056000070716
056100070227     c                   endsr
056200080715
056300080715      *------------------------------------------------------------------------*
056400080715      * Stampa errore SQL
056500080715      *------------------------------------------------------------------------*
056600080715     c     sr_stampaman  begsr
056700080715
056800080715     c                   if        or61nftl = *all'9'
056900080715     c                   eval      nompgm = 'FIOR60R'
057000080715     c                   else
057100080715     c                   eval      nompgm = 'FIOR61R'
057200080715     c                   endif
057300080715
057400080715     c                   move      or61dfvi      dtaiso
057500080715     c                   move      dtaiso        dtaeur
057600080715     c                   move      dtaeur        prtdfv
057700080715     c                   move      or61nfvi      prtnfv
057800080715     c                   eval      errore = sqlcod
057900080715
058000080715      * pagina in stampa
058100080715     c                   if        *inof or not *in99
058200080715     c                   write     or61t
058300080715     c                   eval      *inof = *off
058400080715     c                   eval      *in99 = *on
058500080715     c                   endif
058600080715
058700080715     c                   write     or61r
058800080715
058900080715     c                   endsr
059000070716
059100070716      *------------------------------------------------------------------------*
059200070716      * Esco con codice d'errore
059300070716      *------------------------------------------------------------------------*
059400070716     c     sr_fine       begsr
059500070716
059600070716     c                   if        or61cmti= 'S'
059700070206     c                   rolbk
059800070716     c                   endif
059900070716
060000070716     c                   eval      or61erro= '1'
060100070206     c                   return
060200070716
060300070206     c                   endsr
060400070716
060500070716      *------------------------------------------------------------------------*
060600070716      * Messaggio per ORM allocato
060700070716      *------------------------------------------------------------------------*
060800070716     c     sr_geslck     begsr
060900070716
061000070716     c                   clear                   trul82ds
061100070716     c                   eval      ul82§rrn = ormnrr
061200070716     c                   eval      ul82§fil = 'FNORM00F'
061300070716     c                   eval      ul82§win = 'S'
061400070716     c                   eval      ul82§num = 2
061500070716     c                   eval      ul82§att = 2
061600070716     c                   eval      ul82§mss = msg(01)
061700070716     c                   eval      ul82§msw = 'L''ORM '
061800070716     c                             + %editc(ormpoe:'X') +  ' '
061900070717     c                             + %editc(ormnsr:'Z') +  ' '
062000070716     c                             + %editc(ormnor:'Z') +  ' '
062100070716     c                             + %editc(ormnrv:'Z') +  ' è già utilizzato -
062200070716     c                             da un altro lavoro e non è manutenzionabile'
062300070716     c                   call(e)   'TRUL82R'
062400070716     c                   parm                    trul82ds
062500070716
062600070716     c                   endsr
062700070716
062800070716      *---------------------------------------------------------------*
062900070716      *  Tabella fase                                                 *
063000070716      *---------------------------------------------------------------*
063100070716     c     sr_fase       begsr
063200070716
063300070716     c                   clear                   tibs02ds
063400070716     c                   eval      t02mod = 'C'
063500070716     c                   eval      t02sif = knsif
063600070716     c                   eval      t02cod = 'FAR'
063700070716     c                   movel(p)  ormfao        t02ke1
063800070716     c                   call      'TIBS02R'
063900070716     c                   parm                    kpjba
064000070716     c                   parm                    tibs02ds
064100070716     c                   eval      dfar = t02uni
064200070716
064300070716     c                   endsr
064400070716      *------------------------------------------------------------------------*
064500070716      * Scrivo la fase
064600070716      *------------------------------------------------------------------------*
064700070716     c     sr_wrtorf     begsr
064800070716
064900070716     c                   clear                   fnorf000
065000070716     c                   eval      orfpoe = ormpoe
065100070716     c                   eval      orfnsr = ormnsr
065200070716     c                   eval      orfnor = ormnor
065300070716     c                   eval      orfnrv = ormnrv
065400070716     c                   eval      orfpog = or61fgsi
065500070716     c                   eval      orfdae = ormdfo
065600070716     c                   eval      orfore = ormofo
065700070716     c                   eval      orffar = ormfao
065800070716     c                   eval      orfcar = *blanks
065900070716     c                   eval      orfpue = knmus
066000070716     c                   eval      orffgs = orgfgs
066100070716     c                   eval      orfndc = orgndc
066200070716     c                   eval      orfddc = orgddc
066300161207      * se fase 390 non devo memorizzare la distinta
066400161207     c                   IF        ORFfar = 390
066500161207     c                   clear                   orffgs
066600161207     c                   clear                   orfndc
066700161207     c                   clear                   orfddc
066800161207     c                   ENDIF
066900070905     c                   if        (or61mtdi = 'M' or or61mtdi = 'X')
067000071106     c                              and or61nftl <> 0
067100070716     c                   eval      §orfass = 'M'
067200070716     c                   endif
067300070905     c                   if        (or61mtdi = 'M' or or61mtdi = 'X')
067400071106     c                              and or61nftl = 0
067500070716     c                   eval      §orfass = 'A'
067600070716     c                   endif
067700080314     c                   movel     §orfass       orfflo
067800070716     c                   write     fnorf000
067900070716
068000070716     c                   endsr
068100070716
068200070716      *------------------------------------------------------------------------*
068300070716      * ROUTINE INIZIALE
068400070716      *------------------------------------------------------------------------*
068500070716     c     *inzsr        begsr
068600070716
068700070716     c     *entry        plist
068800070716     c                   parm                    kpjba
068900070720     c                   parm                    fior61ds
069000070716
069100070717     c                   eval      pages = or61pag
069200070717     c                   eval      riga  = or61riga
069300070717     c                   clear                   or61erro
069400080616     c                   eval      wndc = or61nfvi
069500070716
069600070716      * inverto la data del giorno
069700070716     c                   time                    w0140
069800070716     c                   clear                   wlbdat
069900070716     c                   move      w0140         g08dat
070000070716     c                   call      'XSRDA8'
070100070716     c                   parm                    wlbdat
070200070716     c                   z-add     g08inv        dateu
070300070716
070400070716      * controllo se la filiale è partita
070500070716     c                   clear                   og147
070600070717     c                   clear                   og148
070700070716     c                   clear                   comddao
070800070716     c     or61fgsi      chain     azorg01l
070900070323     c                   if        %found(azorg01l)
071000070716     c                   eval      og147 = orgde7
071100070717     c                   eval      og148 = orgde8
071200080124      * se filiale non partita esco (partita anche solo con fase 1)
071300080124     c                   if        §ogcgio = *blanks
071400070323     c                   return
071500070716     c                   endif
071600080124     c                   endif
071700170111
071800170111      /free
071900170111      //?Se richiamato per aggiornare ORM RC devo controllare se
072000170111      //?è stata fatta la fase 'RCH' o 'RQE' sulla distinta
072100170111      //?perchè è l'unico caso in cui il chiamante non fa questo controllo
072200170111        IF  OR61rc = 'S';
072300170111        //?provo prima con la fase 'RCH'
072400170111          clear FIOR90DS;
072500170111          OR90tla = 'C';
072600170111          OR90fas = 'RCH';
072700170111          OR90fgs = OR61fgsi;
072800170111          OR90ndc = OR61nfvi;
072900170111          fior90r (kpjba:fior90ds);
073000170111        //?poi provo con la fase 'RQE'
073100170111          IF  OR90ndcok = *blanks;
073200170111            clear FIOR90DS;
073300170111            OR90tla = 'C';
073400170111            OR90fas = 'RQE';
073500170111            OR90fgs = OR61fgsi;
073600170111            OR90ndc = OR61nfvi;
073700170111            fior90r (kpjba:fior90ds);
073800170111          ENDIF;
073900170111        //?Ho trovato una delle due fasi esco dal pgm
074000170111          IF  OR90ndcok <> *blanks;
074100170111            return;
074200170111          ENDIF;
074300170111        ENDIF;
074400170111      /end-free
074500070716
074600070716      * dati utente
074700030718     c     *dtaara       define    §azute        azuteds
074800030718     c     *dtaara       define    §datiute      ddatiute
074900070716     c                   if        %error or rsut = *blanks
075000070716     c                   clear                   tibs34ds
075100070716     c                   call      'TIBS34R'
075200070716     c                   parm                    tibs34ds
075300070716     c                   in        *dtaara
075400070716     c                   endif
075500070716
075600070717      *  klist
075700070717     c     korm          klist
075800070716     c                   kfld                    orgpoe
075900070716     c                   kfld                    orgnsr
076000070716     c                   kfld                    orgnor
076100070716     c                   kfld                    orgnrv
076200070716
076300070717     c     korm1         klist
076400070717     c                   kfld                    ormpoe
076500070717     c                   kfld                    ormnsr
076600070717     c                   kfld                    ormnor
076700070717     c                   kfld                    ormnrv
076800070717     c                   kfld                    ormdfo
076900070717     c                   kfld                    ormofo
077000070717     c                   kfld                    ormfao
077100080616
077200080730     c     kfidst        klist
077300080730     c                   kfld                    dstnpg
077400080730     c                   kfld                    wndc
077500080730     c                   kfld                    or61fgsi
077600070717
077700070716     c                   endsr
077800070716** msg
077900070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
