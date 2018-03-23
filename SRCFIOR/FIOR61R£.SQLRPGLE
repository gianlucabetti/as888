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
006300080314     d §orfass                18     18
006400070716
006500070716     d azuteds       e ds                  extname(azute00f)
006600070716     d ddatiute      e ds
006700070716     d dfar          e ds
006800070716     d fior61ds      e ds
006900140212     d fior82ds      e ds
007000070716     d fnorgds       e ds                  extname(fnorg00f)
007100070717     d fnormds       e ds                  extname(fnorm00f)
007200070716     d kpjba         e ds
007300070716     d og147         e ds
007400070717     d og148         e ds
007500070716     d tibs02ds      e ds
007600070716     d tibs34ds      e ds                  inz
007700070716     d trul82ds      e ds
007800070716
007900140212      /copy gaitrasrc/srcprotopr,fior82r
008000070716      *------------------------------------------------------------------------*
008100071030
008200071030     c/exec sql
008300071030     c+ set option dynusrprf = *owner, closqlcsr = *endmod
008400071030     c/end-exec
008500070716
008600070716      * metto in distinta
008700070716
008800070716     c                   if        or61mtdi = 'M'
008900070716     c                   exsr      sr_metti
009000070716     c                   endif
009100070716
009200070716      * tolgo dalla distinta
009300070716     c                   if        or61mtdi = 'T'
009400070716     c                   exsr      sr_togli
009500070716     c                   endif
009600070905
009700070905      * trasferisco da una distinta all'altra
009800070905     c                   if        or61mtdi = 'X'
009900070905     c                   exsr      sr_trasferisci
010000070905     c                   endif
010100070716
010200070716     c                   if        or61cmti= 'S'
010300070716     c                   commit
010400070716     c                   endif
010500070716
010600070716     c                   eval      or61pag = pages
010700070716     c                   eval      or61riga= riga
010800070716
010900070716     c                   eval      *inlr = *on
011000070716
011100070716      *------------------------------------------------------------------------*
011200070716      * Leggo i ritiri messi in distinta su ORG e li inserisco su ORM
011300070716      *------------------------------------------------------------------------*
011400070716     c     sr_metti      begsr
011500070716
011600070716     C/EXEC SQL
011700070716     C+ DECLARE A1 CURSOR FOR SELECT fnorm01l.*, fnorg03l.*
011800070904     C+ FROM fnorm01l join fnorg03l on
011900070716     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
012000070716     C+ ormnrv=orgnrv
012100070717     C+ WHERE orgndc=:or61nfvi and orgddc=:or61dfvi and
012200071107     C+ orgfgs =:or61fgsi and ormndc=0 and ormeti <> ' '
012300070716     C+ ORDER BY fnorg03l.orgfgs, fnorg03l.orgndc, fnorg03l.orgddc
012400070716     C/END-EXEC
012500070716
012600070716     C/EXEC SQL
012700070716     C+ OPEN A1
012800070716     C/END-EXEC
012900070716
013000070716     c                   do        *hival
013100070716
013200070716     C/EXEC SQL
013300070716     C+ FETCH NEXT FROM A1 INTO :fnormds, :fnorgds
013400070716     C/END-EXEC
013500070716
013600070716     c                   select
013700070716     c                   when      sqlcod = 100
013800070716     c                   leave
013900070716     c                   when      sqlcod < 0
014000080715      * record non allocato
014100070716     c                   if        sqlcod <> -913
014200080715     c                   exsr      sr_stampaman
014300080715     c                   exsr      sr_fine
014400070716     c                   endif
014500080715      * record allocato
014600070716     c                   exsr      sr_stampa
014700070716     c                   exsr      sr_fine
014800070716
014900070716     c                   other
015000071001      * solo se non chiamato per bolle RC
015100071001     c                   if        or61rc = *blanks
015200070807      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
015300070807      * solo automatiche (or61nftl = 0)
015400070716     c                   if        or61nftl = 99999 and orgnftl <> 99999
015500070716     c                   iter
015600070716     c                   endif
015700070716     c                   if        or61nftl <> 99999 and orgnftl = 99999
015800070716     c                   iter
015900070716     c                   endif
016000071001     c                   endif
016100070716
016200070716     c                   exsr      sr_mettiorm
016300070716     c                   endsl
016400070716
016500070716     c                   enddo
016600070716
016700070716     C/EXEC SQL
016800070716     C+ CLOSE A1
016900070716     C/END-EXEC
017000070717
017100070717     c                   endsr
017200070716
017300070717      *------------------------------------------------------------------------*
017400070717      * Leggo i ritiri che sono ancora in distinta su ORM e che sono da togliere
017500070717      *------------------------------------------------------------------------*
017600070717     c     sr_togli      begsr
017700070717
017800070717     C/EXEC SQL
017900080303     C+ DECLARE B1 CURSOR FOR SELECT distinct  fnorm01l.*,
018000080303     C+ fnorf01l.orffgs, fnorf01l.orfndc,
018100080314     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as §orfass,
018200070717     C+ fnorg01l.*
018300070717     C+ FROM (fnorm01l join fnorf01l on
018400070717     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
018500070717     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
018600070717     C+ ormofo=orfore) join fnorg01l on
018700070717     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
018800070717     C+ ormnrv=orgnrv
018900070717     C+ WHERE orfndc=:or61nfvi and orfddc=:or61dfvi and
019000080221     C+ orffgs =:or61fgsi and orgndc=0 and ormndc > 0
019100070717     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
019200070717     C/END-EXEC
019300070717
019400070717     C/EXEC SQL
019500070717     C+ OPEN B1
019600070717     C/END-EXEC
019700070717
019800070717     c                   do        *hival
019900070717
020000070717     C/EXEC SQL
020100070717     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorfds, :fnorgds
020200070717     C/END-EXEC
020300070717
020400070717     c                   select
020500070717     c                   when      sqlcod = 100
020600070717     c                   leave
020700070717     c                   when      sqlcod < 0
020800080715      * record non allocato
020900070717     c                   if        sqlcod <> -913
021000080715     c                   exsr      sr_stampaman
021100080715     c                   exsr      sr_fine
021200070717     c                   endif
021300080715      * record allocato
021400070717     c                   exsr      sr_stampa
021500070717     c                   exsr      sr_fine
021600070717
021700070717     c                   other
021800071001      * solo se non chiamato per bolle RC
021900071001     c                   if        or61rc = *blanks
022000070717      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
022100070717      * solo automatiche (or61nftl = 0)
022200070921     c                   if        or61nftl = 99999 and §orfass <> 'M'
022300070717     c                   iter
022400070717     c                   endif
022500070921     c                   if        or61nftl = 0      and §orfass = 'M'
022600070717     c                   iter
022700070717     c                   endif
022800071001     c                   endif
022900070717
023000070717     c                   exsr      sr_togliorm
023100070717     c                   endsl
023200070717
023300070717     c                   enddo
023400070717
023500070717     C/EXEC SQL
023600070717     C+ CLOSE B1
023700070717     C/END-EXEC
023800070717
023900070717     c                   endsr
024000070905
024100070905      *------------------------------------------------------------------------*
024200070905      * Leggo i ritiri che sono ancora in distinta su ORM e che sono da trasferire
024300070905      *------------------------------------------------------------------------*
024400070905     c     sr_trasferiscibegsr
024500070905
024600070905     C/EXEC SQL
024700080303     C+ DECLARE C1 CURSOR FOR SELECT distinct  fnorm01l.*,
024800080303     C+ fnorf01l.orffgs, fnorf01l.orfndc,
024900080314     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as §orfass,
025000070905     C+ fnorg01l.*
025100070905     C+ FROM (fnorm01l join fnorf01l on
025200070905     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
025300070905     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
025400070905     C+ ormofo=orfore) join fnorg01l on
025500070905     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
025600070905     C+ ormnrv=orgnrv
025700070905     C+ WHERE orfndc=:or61nfvi and orfddc=:or61dfvi and
025800070905     C+ orffgs =:or61fgsi and orgndc=:or61nfvin
025900070905     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
026000070905     C/END-EXEC
026100070905
026200070905     C/EXEC SQL
026300070905     C+ OPEN C1
026400070905     C/END-EXEC
026500070905
026600070905     c                   do        *hival
026700070905
026800070905     C/EXEC SQL
026900070905     C+ FETCH NEXT FROM C1 INTO :fnormds, :fnorfds, :fnorgds
027000070905     C/END-EXEC
027100070905
027200070905     c                   select
027300070905     c                   when      sqlcod = 100
027400070905     c                   leave
027500070905     c                   when      sqlcod < 0
027600080715      * record non allocato
027700070905     c                   if        sqlcod <> -913
027800080715     c                   exsr      sr_stampaman
027900080715     c                   exsr      sr_fine
028000070905     c                   endif
028100080715      * record allocato
028200070905     c                   exsr      sr_stampa
028300070905     c                   exsr      sr_fine
028400070905
028500070905     c                   other
028600071001      * solo se non chiamato per bolle RC
028700071001     c                   if        or61rc = *blanks
028800070905      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
028900070905      * solo automatiche (or61nftl = 0)
029000070921     c                   if        or61nftl = 99999 and §orfass <> 'M'
029100070905     c                   iter
029200070905     c                   endif
029300070921     c                   if        or61nftl = 0      and §orfass = 'M'
029400070905     c                   iter
029500070905     c                   endif
029600071001     c                   endif
029700070905
029800070905     c                   exsr      sr_togliorm
029900070905     c                   exsr      sr_mettiorm
030000070905     c                   endsl
030100070905
030200070905     c                   enddo
030300070905
030400070905     C/EXEC SQL
030500070905     C+ CLOSE C1
030600070905     C/END-EXEC
030700070905
030800070905     c                   endsr
030900070717
031000070716      *------------------------------------------------------------------------*
031100070716      * Aggiorna ORM con riferimenti distinta
031200070716      *------------------------------------------------------------------------*
031300070716     c     sr_mettiorm   begsr
031400070716
031500070716      * controllo che l'ORM non sia allocato
031600070716     c     korm          chain(e)  fnorm01l
031700070206     c                   if        %error
031800070716      * se allocato mando msg a chi alloca e reperisco il RRN del file
031900070716     c     korm          chain(n)  fnorm01l
032000070716     c                   if        %found(fnorm01l)
032100070716     c                   exsr      sr_geslck
032200070716     c                   if        ul82§sts = 'A'
032300070716     c                   eval      *in85 = *on
032400070716     c                   exsr      sr_stampa
032500071030     c                   exsr      sr_fine
032600070716     c                   endif
032700070716      * non allocato impegno l'ORM
032800070716     c     korm          chain     fnorm01l
032900070716     c                   endif
033000070716     c                   endif
033100070716      * non trovo ORM errore
033200070716     c                   if        not %found(fnorm01l)
033300070716     c                   eval      *in84 = *on
033400070716     c                   exsr      sr_stampa
033500071030     c                   exsr      sr_fine
033600070716     c                   endif
033700070716      * controllo orm
033800070716     c                   if        ormndc <> *zeros
033900070716     c                   eval      *in85 = *on
034000070716     c                   exsr      sr_stampa
034100071030     c                   exsr      sr_fine
034200070716     c                   endif
034300090717      * l'orm deve essere ancora in fase assegnabil
034400090717     c                   if        ormeti = ''
034500090717     c                   eval      *in85 = *on
034600090717     c                   exsr      sr_stampa
034700090717     c                   exsr      sr_fine
034800090717     c                   endif
034900070716
035000070716      * mi salvo i dati della fase presente sulla testa ORM per salvarla su ORG
035100070716     c                   eval      savormfao = ormfao
035200070717     c                   eval      savormdfo = ormdfo
035300070717     c                   eval      savormofo = ormofo
035400070716
035500070716      * imposto la distinta su ORM e scrivo ORF
035600070716     c                   eval      ormnpg = wnpg
035700070716     c                   eval      ormndc = orgndc
035800070716     c                   eval      ormddc = orgddc
035900070716     c                   eval      ormfao = 400
036000070716     c                   time                    w0140
036100070716     c                   clear                   wlbdat
036200070716     c                   move      w0140         g08dat
036300070716     c                   call      'XSRDA8'
036400070716     c                   parm                    wlbdat
036500070716     c                   z-add     g08inv        ormdfo
036600070716     c                   movel     w0140         ormofo
036700070716     c                   z-add     g08inv        ormdtt
036800070716     c                   exsr      sr_fase
036900070716     c                   eval      ormeti = d§farass
037000070716
037100080317     c                   eval      wmetti = *on
037200070716     c                   exsr      sr_wrtorf
037300070716
037400070716     c     korm          chain     fnorg01l
037500070716     c                   if        %found(fnorg01l)
037600070716     c                   eval      orgfao = savormfao
037700070717     c                   eval      orgdfo = savormdfo
037800070717     c                   eval      orgofo = savormofo
037900070716     c                   update    fnorg000
038000070716     c                   endif
038100070716
038200070716     c                   update    fnorm000
038300070716
038400070716     c                   endsr
038500070717
038600070717      *------------------------------------------------------------------------*
038700070717      * Aggiorna ORM con fase precedente
038800070717      *------------------------------------------------------------------------*
038900070717     c     sr_togliorm   begsr
039000070717
039100070717      * controllo che l'ORM non sia allocato
039200070717     c     korm          chain(e)  fnorm01l
039300070717     c                   if        %error
039400070717      * se allocato mando msg a chi alloca e reperisco il RRN del file
039500070717     c     korm          chain(n)  fnorm01l
039600070717     c                   if        %found(fnorm01l)
039700070717     c                   exsr      sr_geslck
039800070717     c                   if        ul82§sts = 'A'
039900070717     c                   eval      *in85 = *on
040000070717     c                   exsr      sr_stampa
040100071030     c                   exsr      sr_fine
040200070717     c                   endif
040300070717      * non allocato impegno l'ORM
040400070717     c     korm          chain     fnorm01l
040500070717     c                   endif
040600070717     c                   endif
040700070717      * non trovo ORM errore
040800070717     c                   if        not %found(fnorm01l)
040900070717     c                   eval      *in84 = *on
041000070717     c                   exsr      sr_stampa
041100071030     c                   exsr      sr_fine
041200070717     c                   endif
041300070717      * controllo orm
041400070717     c                   if        ormndc = *zeros
041500070717     c                   eval      *in85 = *on
041600070717     c                   exsr      sr_stampa
041700071030     c                   exsr      sr_fine
041800070717     c                   endif
041900080312
042000080312      * se ORM con esito da PDA non devo impostare la fase precedente
042100080312      * ma creare la fase 390
042200080312     c                   if        ormfao = 410 or ormfao = 420
042300080312      * tolgo la distinta su ORM e scrivo ORF fase 390
042400080312     c                   clear                   ormnpg
042500080312     c                   clear                   ormndc
042600080312     c                   clear                   ormddc
042700080312     c                   eval      ormfao = 390
042800080312     c                   time                    w0140
042900080312     c                   clear                   wlbdat
043000080312     c                   move      w0140         g08dat
043100080312     c                   call      'XSRDA8'
043200080312     c                   parm                    wlbdat
043300080312     c                   z-add     g08inv        ormdfo
043400080312     c                   movel     w0140         ormofo
043500080312     c                   z-add     g08inv        ormdtt
043600080312     c                   exsr      sr_fase
043700080312     c                   eval      ormeti = d§farass
043800080312      * cancello la data di stampa (ORM Comunicato)
043900080312     c                   clear                   ormdst
044000080312      * scrivo la fase 390
044100080317     c                   eval      wmetti = *off
044200080312     c                   exsr      sr_wrtorf
044300080312      * tolgo fase precedente da ORG
044400080312     c     korm          chain     fnorg01l
044500080312     c                   if        %found(fnorg01l)
044600080312     c                   clear                   orgfao
044700080312     c                   clear                   orgdfo
044800080312     c                   clear                   orgofo
044900080312     c                   update    fnorg000
045000080312     c                   endif
045100080312
045200080312     c                   update    fnorm000
045300080312
045400080312      * se non ha avuto esito da PDA
045500080312     c                   else
045600070717
045700070717      * aggancio ORG per recuperare la fase precedente
045800070717     c     korm          chain     fnorg01l
045900070717     c                   if        %found(fnorg01l)
046000070717     c                   eval      savormfao = orgfao
046100070717     c                   eval      savormdfo = orgdfo
046200070717     c                   eval      savormofo = orgofo
046300070717     c                   clear                   orgfao
046400070717     c                   clear                   orgdfo
046500070717     c                   clear                   orgofo
046600070717     c                   update    fnorg000
046700070717     c                   endif
046800070717
046900070717      * cancello la fase da ORF
047000070717     c     korm1         chain     fnorf01l
047100070717     c                   if        %found(fnorg01l)
047200070717     c                   delete    fnorf000
047300070717     c                   endif
047400070717
047500070717      * cancello la distinta su ORM e imposto la fase precedente
047600070717     c                   clear                   ormnpg
047700070717     c                   clear                   ormndc
047800070717     c                   clear                   ormddc
047900070717     c                   eval      ormfao = savormfao
048000070717     c                   eval      ormdfo = savormdfo
048100070717     c                   eval      ormofo = savormofo
048200070717     c                   exsr      sr_fase
048300070717     c                   eval      ormeti = d§farass
048400070921      * cancello la data di stampa (ORM Comunicato)
048500070921     c                   clear                   ormdst
048600070717     c                   update    fnorm000
048700080312
048800080312     c                   endif
048900080911
049000080911      * aggancio la distinta per recupera l'AUT
049100080911     c                   eval      dstnpg = 4
049200080911     c     kfidst        chain     fidst01l
049300080911     c                   if        not %found(fidst01l)
049400080911     c                   clear                   dstpdr
049500080911     c                   endif
049600080911      * cancello la richiesta info
049700140212      /free
049800140212         clear fior82ds;
049900140212         c82fgs = or61fgsi;
050000140212         c82ndc = wndc ;
050100140212         c82codaut = dstpdr;
050200140212         c82POE = ormpoe;
050300140212         c82NSR = ormnsr;
050400140212         c82NOR = ormnor;
050500140212         c82NRV = ormnrv;
050600140212         c82aggvis = 'S';
050700140212         kpjbu = fior82ds;
050800140212         fior82r(kpjba);
050900140212         fior82ds = kpjbu;
051000070717
051100140212      /end-free
051200070717     c                   endsr
051300070716
051400070716      *------------------------------------------------------------------------*
051500070716      * Stampa errore
051600070716      *------------------------------------------------------------------------*
051700070716     c     sr_stampa     begsr
051800070716
051900070716     c                   move      or61dfvi      dtaiso
052000070227     c                   move      dtaiso        dtaeur
052100070227     c                   move      dtaeur        prtdfv
052200070716     c                   move      orgndc        prtnfv
052300070716
052400070704     c                   if        *in85
052500070716     c                   eval      prtpoe = orgpoe
052600070716     c                   eval      prtnsr = orgnsr
052700070716     c                   eval      prtnor = orgnor
052800070716     c                   eval      prtnrv = orgnrv
052900070716     c                   eval      prtmsg = 'L''ORM non è stato aggiornato corr-
053000070716     c                             ettamente. VERIFICARLO'
053100070716     c                   endif
053200070716
053300070716      * pagina in stampa
053400070913     c                   if        riga >= 60
053500070716     c                   eval      *in80 = *on
053600070716     c                   add       1             pages
053700070716     c                   eval      riga = 3
053800070716     c                   endif
053900070716
054000070913     c   80              write     testa
054100070716     c                   eval      *in80 = *off
054200070716
054300070319     c   82              write     riga2
054400070716     c   82              add       1             riga
054500070716
054600070227     c   84              write     riga4
054700070716     c   84              add       1             riga
054800070716
054900070704     c   85              write     riga5
055000070716     c   85              add       1             riga
055100070716
055200070704     c                   setoff                                       828485
055300070716
055400070227     c                   endsr
055500080715
055600080715      *------------------------------------------------------------------------*
055700080715      * Stampa errore SQL
055800080715      *------------------------------------------------------------------------*
055900080715     c     sr_stampaman  begsr
056000080715
056100080715     c                   if        or61nftl = *all'9'
056200080715     c                   eval      nompgm = 'FIOR60R'
056300080715     c                   else
056400080715     c                   eval      nompgm = 'FIOR61R'
056500080715     c                   endif
056600080715
056700080715     c                   move      or61dfvi      dtaiso
056800080715     c                   move      dtaiso        dtaeur
056900080715     c                   move      dtaeur        prtdfv
057000080715     c                   move      or61nfvi      prtnfv
057100080715     c                   eval      errore = sqlcod
057200080715
057300080715      * pagina in stampa
057400080715     c                   if        *inof or not *in99
057500080715     c                   write     or61t
057600080715     c                   eval      *inof = *off
057700080715     c                   eval      *in99 = *on
057800080715     c                   endif
057900080715
058000080715     c                   write     or61r
058100080715
058200080715     c                   endsr
058300070716
058400070716      *------------------------------------------------------------------------*
058500070716      * Esco con codice d'errore
058600070716      *------------------------------------------------------------------------*
058700070716     c     sr_fine       begsr
058800070716
058900070716     c                   if        or61cmti= 'S'
059000070206     c                   rolbk
059100070716     c                   endif
059200070716
059300070716     c                   eval      or61erro= '1'
059400070716     c                   eval      *inlr = *on
059500070206     c                   return
059600070716
059700070206     c                   endsr
059800070716
059900070716      *------------------------------------------------------------------------*
060000070716      * Messaggio per ORM allocato
060100070716      *------------------------------------------------------------------------*
060200070716     c     sr_geslck     begsr
060300070716
060400070716     c                   clear                   trul82ds
060500070716     c                   eval      ul82§rrn = ormnrr
060600070716     c                   eval      ul82§fil = 'FNORM00F'
060700070716     c                   eval      ul82§win = 'S'
060800070716     c                   eval      ul82§num = 2
060900070716     c                   eval      ul82§att = 2
061000070716     c                   eval      ul82§mss = msg(01)
061100070716     c                   eval      ul82§msw = 'L''ORM '
061200070716     c                             + %editc(ormpoe:'X') +  ' '
061300070717     c                             + %editc(ormnsr:'Z') +  ' '
061400070716     c                             + %editc(ormnor:'Z') +  ' '
061500070716     c                             + %editc(ormnrv:'Z') +  ' è già utilizzato -
061600070716     c                             da un altro lavoro e non è manutenzionabile'
061700070716     c                   call(e)   'TRUL82R'
061800070716     c                   parm                    trul82ds
061900070716
062000070716     c                   endsr
062100070716
062200070716      *---------------------------------------------------------------*
062300070716      *  Tabella fase                                                 *
062400070716      *---------------------------------------------------------------*
062500070716     c     sr_fase       begsr
062600070716
062700070716     c                   clear                   tibs02ds
062800070716     c                   eval      t02mod = 'C'
062900070716     c                   eval      t02sif = knsif
063000070716     c                   eval      t02cod = 'FAR'
063100070716     c                   movel(p)  ormfao        t02ke1
063200070716     c                   call      'TIBS02R'
063300070716     c                   parm                    kpjba
063400070716     c                   parm                    tibs02ds
063500070716     c                   eval      dfar = t02uni
063600070716
063700070716     c                   endsr
063800070716      *------------------------------------------------------------------------*
063900070716      * Scrivo la fase
064000070716      *------------------------------------------------------------------------*
064100070716     c     sr_wrtorf     begsr
064200070716
064300070716     c                   clear                   fnorf000
064400070716     c                   eval      orfpoe = ormpoe
064500070716     c                   eval      orfnsr = ormnsr
064600070716     c                   eval      orfnor = ormnor
064700070716     c                   eval      orfnrv = ormnrv
064800070716     c                   eval      orfpog = or61fgsi
064900070716     c                   eval      orfdae = ormdfo
065000070716     c                   eval      orfore = ormofo
065100070716     c                   eval      orffar = ormfao
065200070716     c                   eval      orfcar = *blanks
065300070716     c                   eval      orfpue = knmus
065400070716     c                   eval      orffgs = orgfgs
065500070716     c                   eval      orfndc = orgndc
065600070716     c                   eval      orfddc = orgddc
065700161207      * se fase 390 non devo memorizzare la distinta
065800161207     c                   IF        ORFfar = 390
065900161207     c                   clear                   orffgs
066000161207     c                   clear                   orfndc
066100161207     c                   clear                   orfddc
066200161207     c                   ENDIF
066300070905     c                   if        (or61mtdi = 'M' or or61mtdi = 'X')
066400071106     c                              and or61nftl <> 0
066500070716     c                   eval      §orfass = 'M'
066600070716     c                   endif
066700070905     c                   if        (or61mtdi = 'M' or or61mtdi = 'X')
066800071106     c                              and or61nftl = 0
066900070716     c                   eval      §orfass = 'A'
067000070716     c                   endif
067100080314     c                   movel     §orfass       orfflo
067200070716     c                   write     fnorf000
067300070716
067400070716     c                   endsr
067500070716
067600070716      *------------------------------------------------------------------------*
067700070716      * ROUTINE INIZIALE
067800070716      *------------------------------------------------------------------------*
067900070716     c     *inzsr        begsr
068000070716
068100070716     c     *entry        plist
068200070716     c                   parm                    kpjba
068300070720     c                   parm                    fior61ds
068400070716
068500070717     c                   eval      pages = or61pag
068600070717     c                   eval      riga  = or61riga
068700070717     c                   clear                   or61erro
068800080616     c                   eval      wndc = or61nfvi
068900070716
069000070716      * inverto la data del giorno
069100070716     c                   time                    w0140
069200070716     c                   clear                   wlbdat
069300070716     c                   move      w0140         g08dat
069400070716     c                   call      'XSRDA8'
069500070716     c                   parm                    wlbdat
069600070716     c                   z-add     g08inv        dateu
069700070716
069800070716      * controllo se la filiale è partita
069900070716     c                   clear                   og147
070000070717     c                   clear                   og148
070100070716     c                   clear                   comddao
070200070716     c     or61fgsi      chain     azorg01l
070300070323     c                   if        %found(azorg01l)
070400070716     c                   eval      og147 = orgde7
070500070717     c                   eval      og148 = orgde8
070600080124      * se filiale non partita esco (partita anche solo con fase 1)
070700080124     c                   if        §ogcgio = *blanks
070800070716     c                   eval      *inlr = *on
070900070323     c                   return
071000070716     c                   endif
071100070717
071200080124     c                   endif
071300070716
071400070716      * dati utente
071500030718     c     *dtaara       define    §azute        azuteds
071600030718     c     *dtaara       define    §datiute      ddatiute
071700070716     c                   if        %error or rsut = *blanks
071800070716     c                   clear                   tibs34ds
071900070716     c                   call      'TIBS34R'
072000070716     c                   parm                    tibs34ds
072100070716     c                   in        *dtaara
072200070716     c                   endif
072300070716
072400070717      *  klist
072500070717     c     korm          klist
072600070716     c                   kfld                    orgpoe
072700070716     c                   kfld                    orgnsr
072800070716     c                   kfld                    orgnor
072900070716     c                   kfld                    orgnrv
073000070716
073100070717     c     korm1         klist
073200070717     c                   kfld                    ormpoe
073300070717     c                   kfld                    ormnsr
073400070717     c                   kfld                    ormnor
073500070717     c                   kfld                    ormnrv
073600070717     c                   kfld                    ormdfo
073700070717     c                   kfld                    ormofo
073800070717     c                   kfld                    ormfao
073900080616
074000080730     c     kfidst        klist
074100080730     c                   kfld                    dstnpg
074200080730     c                   kfld                    wndc
074300080730     c                   kfld                    or61fgsi
074400070717
074500070716     c                   endsr
074600070716** msg
074700070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
