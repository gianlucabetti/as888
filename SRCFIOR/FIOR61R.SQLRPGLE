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
003400170111     d wfatto          s               n   inz
003500080317     d wmetti          s               n
003600140212     d wndc            s                   like(dstnfv)
003700070731     d wnpg            s                   like(ormnpg) inz(4)
003800070716     d w0140           s             14  0
003900070716
004000070716      *------------------------------------------------------------------------*
004100070716      *   S C H I E R E
004200070716      *------------------------------------------------------------------------*
004300070716     d msg             s             78    dim(01) ctdata perrcd(1)
004400070716
004500070716      *------------------------------------------------------------------------*
004600070716      *   D S   I N T E R N E / E S T E R N E
004700070716      *------------------------------------------------------------------------*
004800070802     d ormds           ds
004900070716     d  ormnrr               397    400i 0
005000070913
005100070913     d prterr          ds
005200070913     d  riga                 367    368i 0
005300070716
005400070716     d wlbdat          ds
005500070716     d  g08dat                 1      8  0
005600070716     d  g08inv                 9     16  0
005700070716     d  g08err                17     17
005800070716     d  g08tgi                18     22  0
005900080303
006000080303     d fnorfds         ds
006100080303     d orffgs                  1      3  0
006200080303     d orfndc                  4      9  0
006300080303     d orfddc                 10     17  0
006400170111     d worfass                18     18
006500070716
006600070716     d azuteds       e ds                  extname(azute00f)
006700070716     d ddatiute      e ds
006800070716     d dfar          e ds
006900170111     d dORF01        e ds
007000070716     d fior61ds      e ds
007100140212     d fior82ds      e ds
007200170111     d FIOR90DS      e ds
007300070716     d fnorgds       e ds                  extname(fnorg00f)
007400070717     d fnormds       e ds                  extname(fnorm00f)
007500070716     d kpjba         e ds
007600070716     d og147         e ds
007700070717     d og148         e ds
007800070716     d tibs02ds      e ds
007900070716     d tibs34ds      e ds                  inz
008000070716     d trul82ds      e ds
008100070716
008200140212      /copy gaitrasrc/srcprotopr,fior82r
008300170111
008400170111      // - Pgm controllo fasi
008500170111     d FIOR90R         pr                  extpgm('FIOR90R')
008600170111     d  kpjba                              likeds(kpjba)
008700170111     d  fior90ds                           likeds(fior90ds)
008800070716      *------------------------------------------------------------------------*
008900071030
009000071030     c/exec sql
009100071030     c+ set option dynusrprf = *owner, closqlcsr = *endmod
009200071030     c/end-exec
009300070716
009400070716      * metto in distinta
009500070716
009600070716     c                   if        or61mtdi = 'M'
009700070716     c                   exsr      sr_metti
009800070716     c                   endif
009900070716
010000070716      * tolgo dalla distinta
010100070716     c                   if        or61mtdi = 'T'
010200070716     c                   exsr      sr_togli
010300070716     c                   endif
010400070905
010500070905      * trasferisco da una distinta all'altra
010600070905     c                   if        or61mtdi = 'X'
010700070905     c                   exsr      sr_trasferisci
010800070905     c                   endif
010900070716
011000070716     c                   if        or61cmti= 'S'
011100070716     c                   commit
011200070716     c                   endif
011300070716
011400070716     c                   eval      or61pag = pages
011500070716     c                   eval      or61riga= riga
011600070716
011700070716     c                   eval      *inlr = *on
011800070716
011900070716      *------------------------------------------------------------------------*
012000070716      * Leggo i ritiri messi in distinta su ORG e li inserisco su ORM
012100070716      *------------------------------------------------------------------------*
012200070716     c     sr_metti      begsr
012300070716
012400070716     C/EXEC SQL
012500070716     C+ DECLARE A1 CURSOR FOR SELECT fnorm01l.*, fnorg03l.*
012600070904     C+ FROM fnorm01l join fnorg03l on
012700070716     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
012800070716     C+ ormnrv=orgnrv
012900070717     C+ WHERE orgndc=:or61nfvi and orgddc=:or61dfvi and
013000071107     C+ orgfgs =:or61fgsi and ormndc=0 and ormeti <> ' '
013100070716     C+ ORDER BY fnorg03l.orgfgs, fnorg03l.orgndc, fnorg03l.orgddc
013200070716     C/END-EXEC
013300070716
013400070716     C/EXEC SQL
013500070716     C+ OPEN A1
013600070716     C/END-EXEC
013700070716
013800070716     c                   do        *hival
013900070716
014000070716     C/EXEC SQL
014100070716     C+ FETCH NEXT FROM A1 INTO :fnormds, :fnorgds
014200070716     C/END-EXEC
014300070716
014400070716     c                   select
014500070716     c                   when      sqlcod = 100
014600070716     c                   leave
014700070716     c                   when      sqlcod < 0
014800080715      * record non allocato
014900070716     c                   if        sqlcod <> -913
015000080715     c                   exsr      sr_stampaman
015100080715     c                   exsr      sr_fine
015200070716     c                   endif
015300080715      * record allocato
015400070716     c                   exsr      sr_stampa
015500070716     c                   exsr      sr_fine
015600070716
015700070716     c                   other
015800071001      * solo se non chiamato per bolle RC
015900071001     c                   if        or61rc = *blanks
016000070807      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
016100070807      * solo automatiche (or61nftl = 0)
016200070716     c                   if        or61nftl = 99999 and orgnftl <> 99999
016300070716     c                   iter
016400070716     c                   endif
016500070716     c                   if        or61nftl <> 99999 and orgnftl = 99999
016600070716     c                   iter
016700070716     c                   endif
016800071001     c                   endif
016900070716
017000070716     c                   exsr      sr_mettiorm
017100070716     c                   endsl
017200070716
017300070716     c                   enddo
017400070716
017500070716     C/EXEC SQL
017600070716     C+ CLOSE A1
017700070716     C/END-EXEC
017800070717
017900070717     c                   endsr
018000070716
018100070717      *------------------------------------------------------------------------*
018200070717      * Leggo i ritiri che sono ancora in distinta su ORM e che sono da togliere
018300070717      *------------------------------------------------------------------------*
018400070717     c     sr_togli      begsr
018500070717
018600070717     C/EXEC SQL
018700080303     C+ DECLARE B1 CURSOR FOR SELECT distinct  fnorm01l.*,
018800080303     C+ fnorf01l.orffgs, fnorf01l.orfndc,
018900170111     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as worfass,
019000070717     C+ fnorg01l.*
019100070717     C+ FROM (fnorm01l join fnorf01l on
019200070717     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
019300070717     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
019400070717     C+ ormofo=orfore) join fnorg01l on
019500070717     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
019600070717     C+ ormnrv=orgnrv
019700070717     C+ WHERE orfndc=:or61nfvi and orfddc=:or61dfvi and
019800080221     C+ orffgs =:or61fgsi and orgndc=0 and ormndc > 0
019900070717     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
020000070717     C/END-EXEC
020100070717
020200070717     C/EXEC SQL
020300070717     C+ OPEN B1
020400070717     C/END-EXEC
020500070717
020600070717     c                   do        *hival
020700070717
020800070717     C/EXEC SQL
020900070717     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorfds, :fnorgds
021000070717     C/END-EXEC
021100070717
021200070717     c                   select
021300070717     c                   when      sqlcod = 100
021400070717     c                   leave
021500070717     c                   when      sqlcod < 0
021600080715      * record non allocato
021700070717     c                   if        sqlcod <> -913
021800080715     c                   exsr      sr_stampaman
021900080715     c                   exsr      sr_fine
022000070717     c                   endif
022100080715      * record allocato
022200070717     c                   exsr      sr_stampa
022300070717     c                   exsr      sr_fine
022400070717
022500070717     c                   other
022600071001      * solo se non chiamato per bolle RC
022700071001     c                   if        or61rc = *blanks
022800070717      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
022900070717      * solo automatiche (or61nftl = 0)
023000170111     c                   if        or61nftl = 99999 and worfass <> 'M'
023100070717     c                   iter
023200070717     c                   endif
023300170111     c                   if        or61nftl = 0      and worfass = 'M'
023400070717     c                   iter
023500070717     c                   endif
023600071001     c                   endif
023700070717
023800070717     c                   exsr      sr_togliorm
023900070717     c                   endsl
024000070717
024100070717     c                   enddo
024200070717
024300070717     C/EXEC SQL
024400070717     C+ CLOSE B1
024500070717     C/END-EXEC
024600070717
024700070717     c                   endsr
024800070905
024900070905      *------------------------------------------------------------------------*
025000070905      * Leggo i ritiri che sono ancora in distinta su ORM e che sono da trasferire
025100070905      *------------------------------------------------------------------------*
025200070905     c     sr_trasferiscibegsr
025300070905
025400070905     C/EXEC SQL
025500080303     C+ DECLARE C1 CURSOR FOR SELECT distinct  fnorm01l.*,
025600080303     C+ fnorf01l.orffgs, fnorf01l.orfndc,
025700170111     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as worfass,
025800070905     C+ fnorg01l.*
025900070905     C+ FROM (fnorm01l join fnorf01l on
026000070905     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
026100070905     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
026200070905     C+ ormofo=orfore) join fnorg01l on
026300070905     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
026400070905     C+ ormnrv=orgnrv
026500070905     C+ WHERE orfndc=:or61nfvi and orfddc=:or61dfvi and
026600070905     C+ orffgs =:or61fgsi and orgndc=:or61nfvin
026700070905     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
026800070905     C/END-EXEC
026900070905
027000070905     C/EXEC SQL
027100070905     C+ OPEN C1
027200070905     C/END-EXEC
027300070905
027400070905     c                   do        *hival
027500070905
027600070905     C/EXEC SQL
027700070905     C+ FETCH NEXT FROM C1 INTO :fnormds, :fnorfds, :fnorgds
027800070905     C/END-EXEC
027900070905
028000070905     c                   select
028100070905     c                   when      sqlcod = 100
028200070905     c                   leave
028300070905     c                   when      sqlcod < 0
028400080715      * record non allocato
028500070905     c                   if        sqlcod <> -913
028600080715     c                   exsr      sr_stampaman
028700080715     c                   exsr      sr_fine
028800070905     c                   endif
028900080715      * record allocato
029000070905     c                   exsr      sr_stampa
029100070905     c                   exsr      sr_fine
029200070905
029300070905     c                   other
029400071001      * solo se non chiamato per bolle RC
029500071001     c                   if        or61rc = *blanks
029600070905      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
029700070905      * solo automatiche (or61nftl = 0)
029800170111     c                   if        or61nftl = 99999 and worfass <> 'M'
029900070905     c                   iter
030000070905     c                   endif
030100170111     c                   if        or61nftl = 0      and worfass = 'M'
030200070905     c                   iter
030300070905     c                   endif
030400071001     c                   endif
030500070905
030600070905     c                   exsr      sr_togliorm
030700070905     c                   exsr      sr_mettiorm
030800070905     c                   endsl
030900070905
031000070905     c                   enddo
031100070905
031200070905     C/EXEC SQL
031300070905     C+ CLOSE C1
031400070905     C/END-EXEC
031500070905
031600070905     c                   endsr
031700070717
031800070716      *------------------------------------------------------------------------*
031900070716      * Aggiorna ORM con riferimenti distinta
032000070716      *------------------------------------------------------------------------*
032100070716     c     sr_mettiorm   begsr
032200070716
032300070716      * controllo che l'ORM non sia allocato
032400070716     c     korm          chain(e)  fnorm01l
032500070206     c                   if        %error
032600070716      * se allocato mando msg a chi alloca e reperisco il RRN del file
032700070716     c     korm          chain(n)  fnorm01l
032800070716     c                   if        %found(fnorm01l)
032900070716     c                   exsr      sr_geslck
033000070716     c                   if        ul82§sts = 'A'
033100070716     c                   eval      *in85 = *on
033200070716     c                   exsr      sr_stampa
033300071030     c                   exsr      sr_fine
033400070716     c                   endif
033500070716      * non allocato impegno l'ORM
033600070716     c     korm          chain     fnorm01l
033700070716     c                   endif
033800070716     c                   endif
033900070716      * non trovo ORM errore
034000070716     c                   if        not %found(fnorm01l)
034100070716     c                   eval      *in84 = *on
034200070716     c                   exsr      sr_stampa
034300071030     c                   exsr      sr_fine
034400070716     c                   endif
034500070716      * controllo orm
034600070716     c                   if        ormndc <> *zeros
034700070716     c                   eval      *in85 = *on
034800070716     c                   exsr      sr_stampa
034900071030     c                   exsr      sr_fine
035000070716     c                   endif
035100090717      * l'orm deve essere ancora in fase assegnabil
035200090717     c                   if        ormeti = ''
035300090717     c                   eval      *in85 = *on
035400090717     c                   exsr      sr_stampa
035500090717     c                   exsr      sr_fine
035600090717     c                   endif
035700070716
035800070716      * mi salvo i dati della fase presente sulla testa ORM per salvarla su ORG
035900070716     c                   eval      savormfao = ormfao
036000070717     c                   eval      savormdfo = ormdfo
036100070717     c                   eval      savormofo = ormofo
036200070716
036300070716      * imposto la distinta su ORM e scrivo ORF
036400070716     c                   eval      ormnpg = wnpg
036500070716     c                   eval      ormndc = orgndc
036600070716     c                   eval      ormddc = orgddc
036700070716     c                   eval      ormfao = 400
036800070716     c                   time                    w0140
036900070716     c                   clear                   wlbdat
037000070716     c                   move      w0140         g08dat
037100070716     c                   call      'XSRDA8'
037200070716     c                   parm                    wlbdat
037300070716     c                   z-add     g08inv        ormdfo
037400070716     c                   movel     w0140         ormofo
037500070716     c                   z-add     g08inv        ormdtt
037600070716     c                   exsr      sr_fase
037700070716     c                   eval      ormeti = d§farass
037800070716
037900080317     c                   eval      wmetti = *on
038000070716     c                   exsr      sr_wrtorf
038100070716
038200070716     c     korm          chain     fnorg01l
038300070716     c                   if        %found(fnorg01l)
038400070716     c                   eval      orgfao = savormfao
038500070717     c                   eval      orgdfo = savormdfo
038600070717     c                   eval      orgofo = savormofo
038700070716     c                   update    fnorg000
038800070716     c                   endif
038900070716
039000070716     c                   update    fnorm000
039100070716
039200070716     c                   endsr
039300070717
039400070717      *------------------------------------------------------------------------*
039500070717      * Aggiorna ORM con fase precedente
039600070717      *------------------------------------------------------------------------*
039700070717     c     sr_togliorm   begsr
039800170111
039900170111     c                   eval      wfatto = *off
040000070717
040100070717      * controllo che l'ORM non sia allocato
040200070717     c     korm          chain(e)  fnorm01l
040300070717     c                   if        %error
040400070717      * se allocato mando msg a chi alloca e reperisco il RRN del file
040500070717     c     korm          chain(n)  fnorm01l
040600070717     c                   if        %found(fnorm01l)
040700070717     c                   exsr      sr_geslck
040800070717     c                   if        ul82§sts = 'A'
040900070717     c                   eval      *in85 = *on
041000070717     c                   exsr      sr_stampa
041100071030     c                   exsr      sr_fine
041200070717     c                   endif
041300070717      * non allocato impegno l'ORM
041400070717     c     korm          chain     fnorm01l
041500070717     c                   endif
041600070717     c                   endif
041700070717      * non trovo ORM errore
041800070717     c                   if        not %found(fnorm01l)
041900070717     c                   eval      *in84 = *on
042000070717     c                   exsr      sr_stampa
042100071030     c                   exsr      sr_fine
042200070717     c                   endif
042300070717      * controllo orm
042400070717     c                   if        ormndc = *zeros
042500070717     c                   eval      *in85 = *on
042600070717     c                   exsr      sr_stampa
042700071030     c                   exsr      sr_fine
042800070717     c                   endif
042900080312
043000080312      * se ORM con esito da PDA non devo impostare la fase precedente
043100080312      * ma creare la fase 390
043200080312     c                   if        ormfao = 410 or ormfao = 420
043300170111     c                   eval      wfatto = (ORMfao = 420)
043400080312      * tolgo la distinta su ORM e scrivo ORF fase 390
043500080312     c                   clear                   ormnpg
043600080312     c                   clear                   ormndc
043700080312     c                   clear                   ormddc
043800080312     c                   eval      ormfao = 390
043900080312     c                   time                    w0140
044000080312     c                   clear                   wlbdat
044100080312     c                   move      w0140         g08dat
044200080312     c                   call      'XSRDA8'
044300080312     c                   parm                    wlbdat
044400080312     c                   z-add     g08inv        ormdfo
044500080312     c                   movel     w0140         ormofo
044600080312     c                   z-add     g08inv        ormdtt
044700080312     c                   exsr      sr_fase
044800080312     c                   eval      ormeti = d§farass
044900080312      * cancello la data di stampa (ORM Comunicato)
045000080312     c                   clear                   ormdst
045100080312      * scrivo la fase 390
045200080317     c                   eval      wmetti = *off
045300080312     c                   exsr      sr_wrtorf
045400080312      * tolgo fase precedente da ORG
045500080312     c     korm          chain     fnorg01l
045600080312     c                   if        %found(fnorg01l)
045700080312     c                   clear                   orgfao
045800080312     c                   clear                   orgdfo
045900080312     c                   clear                   orgofo
046000080312     c                   update    fnorg000
046100080312     c                   endif
046200080312
046300080312     c                   update    fnorm000
046400080312
046500080312      * se non ha avuto esito da PDA
046600080312     c                   else
046700070717
046800070717      * aggancio ORG per recuperare la fase precedente
046900070717     c     korm          chain     fnorg01l
047000070717     c                   if        %found(fnorg01l)
047100070717     c                   eval      savormfao = orgfao
047200070717     c                   eval      savormdfo = orgdfo
047300070717     c                   eval      savormofo = orgofo
047400070717     c                   clear                   orgfao
047500070717     c                   clear                   orgdfo
047600070717     c                   clear                   orgofo
047700070717     c                   update    fnorg000
047800070717     c                   endif
047900070717
048000070717      * cancello la fase da ORF
048100070717     c     korm1         chain     fnorf01l
048200070717     c                   if        %found(fnorg01l)
048300070717     c                   delete    fnorf000
048400070717     c                   endif
048500070717
048600070717      * cancello la distinta su ORM e imposto la fase precedente
048700070717     c                   clear                   ormnpg
048800070717     c                   clear                   ormndc
048900070717     c                   clear                   ormddc
049000070717     c                   eval      ormfao = savormfao
049100070717     c                   eval      ormdfo = savormdfo
049200070717     c                   eval      ormofo = savormofo
049300070717     c                   exsr      sr_fase
049400070717     c                   eval      ormeti = d§farass
049500070921      * cancello la data di stampa (ORM Comunicato)
049600070921     c                   clear                   ormdst
049700070717     c                   update    fnorm000
049800080312
049900080312     c                   endif
050000080911
050100080911      * aggancio la distinta per recupera l'AUT
050200080911     c                   eval      dstnpg = 4
050300080911     c     kfidst        chain     fidst01l
050400080911     c                   if        not %found(fidst01l)
050500080911     c                   clear                   dstpdr
050600080911     c                   endif
050700080911      * cancello la richiesta info
050800140212      /free
050900140212         clear fior82ds;
051000140212         c82fgs = or61fgsi;
051100140212         c82ndc = wndc ;
051200140212         c82codaut = dstpdr;
051300140212         c82POE = ormpoe;
051400140212         c82NSR = ormnsr;
051500140212         c82NOR = ormnor;
051600140212         c82NRV = ormnrv;
051700140212         c82aggvis = 'S';
051800140212         kpjbu = fior82ds;
051900140212         fior82r(kpjba);
052000140212         fior82ds = kpjbu;
052100070717
052200140212      /end-free
052300070717     c                   endsr
052400070716
052500070716      *------------------------------------------------------------------------*
052600070716      * Stampa errore
052700070716      *------------------------------------------------------------------------*
052800070716     c     sr_stampa     begsr
052900070716
053000070716     c                   move      or61dfvi      dtaiso
053100070227     c                   move      dtaiso        dtaeur
053200070227     c                   move      dtaeur        prtdfv
053300070716     c                   move      orgndc        prtnfv
053400070716
053500070704     c                   if        *in85
053600070716     c                   eval      prtpoe = orgpoe
053700070716     c                   eval      prtnsr = orgnsr
053800070716     c                   eval      prtnor = orgnor
053900070716     c                   eval      prtnrv = orgnrv
054000070716     c                   eval      prtmsg = 'L''ORM non è stato aggiornato corr-
054100070716     c                             ettamente. VERIFICARLO'
054200070716     c                   endif
054300070716
054400070716      * pagina in stampa
054500070913     c                   if        riga >= 60
054600070716     c                   eval      *in80 = *on
054700070716     c                   add       1             pages
054800070716     c                   eval      riga = 3
054900070716     c                   endif
055000070716
055100070913     c   80              write     testa
055200070716     c                   eval      *in80 = *off
055300070716
055400070319     c   82              write     riga2
055500070716     c   82              add       1             riga
055600070716
055700070227     c   84              write     riga4
055800070716     c   84              add       1             riga
055900070716
056000070704     c   85              write     riga5
056100070716     c   85              add       1             riga
056200070716
056300070704     c                   setoff                                       828485
056400070716
056500070227     c                   endsr
056600080715
056700080715      *------------------------------------------------------------------------*
056800080715      * Stampa errore SQL
056900080715      *------------------------------------------------------------------------*
057000080715     c     sr_stampaman  begsr
057100080715
057200080715     c                   if        or61nftl = *all'9'
057300080715     c                   eval      nompgm = 'FIOR60R'
057400080715     c                   else
057500080715     c                   eval      nompgm = 'FIOR61R'
057600080715     c                   endif
057700080715
057800080715     c                   move      or61dfvi      dtaiso
057900080715     c                   move      dtaiso        dtaeur
058000080715     c                   move      dtaeur        prtdfv
058100080715     c                   move      or61nfvi      prtnfv
058200080715     c                   eval      errore = sqlcod
058300080715
058400080715      * pagina in stampa
058500080715     c                   if        *inof or not *in99
058600080715     c                   write     or61t
058700080715     c                   eval      *inof = *off
058800080715     c                   eval      *in99 = *on
058900080715     c                   endif
059000080715
059100080715     c                   write     or61r
059200080715
059300080715     c                   endsr
059400070716
059500070716      *------------------------------------------------------------------------*
059600070716      * Esco con codice d'errore
059700070716      *------------------------------------------------------------------------*
059800070716     c     sr_fine       begsr
059900070716
060000070716     c                   if        or61cmti= 'S'
060100070206     c                   rolbk
060200070716     c                   endif
060300070716
060400070716     c                   eval      or61erro= '1'
060500070716     c                   eval      *inlr = *on
060600070206     c                   return
060700070716
060800070206     c                   endsr
060900070716
061000070716      *------------------------------------------------------------------------*
061100070716      * Messaggio per ORM allocato
061200070716      *------------------------------------------------------------------------*
061300070716     c     sr_geslck     begsr
061400070716
061500070716     c                   clear                   trul82ds
061600070716     c                   eval      ul82§rrn = ormnrr
061700070716     c                   eval      ul82§fil = 'FNORM00F'
061800070716     c                   eval      ul82§win = 'S'
061900070716     c                   eval      ul82§num = 2
062000070716     c                   eval      ul82§att = 2
062100070716     c                   eval      ul82§mss = msg(01)
062200070716     c                   eval      ul82§msw = 'L''ORM '
062300070716     c                             + %editc(ormpoe:'X') +  ' '
062400070717     c                             + %editc(ormnsr:'Z') +  ' '
062500070716     c                             + %editc(ormnor:'Z') +  ' '
062600070716     c                             + %editc(ormnrv:'Z') +  ' è già utilizzato -
062700070716     c                             da un altro lavoro e non è manutenzionabile'
062800070716     c                   call(e)   'TRUL82R'
062900070716     c                   parm                    trul82ds
063000070716
063100070716     c                   endsr
063200070716
063300070716      *---------------------------------------------------------------*
063400070716      *  Tabella fase                                                 *
063500070716      *---------------------------------------------------------------*
063600070716     c     sr_fase       begsr
063700070716
063800070716     c                   clear                   tibs02ds
063900070716     c                   eval      t02mod = 'C'
064000070716     c                   eval      t02sif = knsif
064100070716     c                   eval      t02cod = 'FAR'
064200070716     c                   movel(p)  ormfao        t02ke1
064300070716     c                   call      'TIBS02R'
064400070716     c                   parm                    kpjba
064500070716     c                   parm                    tibs02ds
064600070716     c                   eval      dfar = t02uni
064700070716
064800070716     c                   endsr
064900070716      *------------------------------------------------------------------------*
065000070716      * Scrivo la fase
065100070716      *------------------------------------------------------------------------*
065200070716     c     sr_wrtorf     begsr
065300070716
065400170111     c                   clear                   dORF01
065500070716     c                   clear                   fnorf000
065600070716     c                   eval      orfpoe = ormpoe
065700070716     c                   eval      orfnsr = ormnsr
065800070716     c                   eval      orfnor = ormnor
065900070716     c                   eval      orfnrv = ormnrv
066000070716     c                   eval      orfpog = or61fgsi
066100070716     c                   eval      orfdae = ormdfo
066200070716     c                   eval      orfore = ormofo
066300070716     c                   eval      orffar = ormfao
066400070716     c                   eval      orfcar = *blanks
066500070716     c                   eval      orfpue = knmus
066600070716     c                   eval      orffgs = orgfgs
066700070716     c                   eval      orfndc = orgndc
066800070716     c                   eval      orfddc = orgddc
066900170111      * se fase 390 devo memorizzare la distinta da cui sto togliendo l'ORM
067000161207     c                   IF        ORFfar = 390
067100170111     c                   eval      ORFfgs = OR61fgsi
067200170111     c                   eval      ORFndc = OR61nfvi
067300170111     c                   eval      ORFddc = OR61dfvi
067400170111      * inoltre memorizzo se la fase precedente era un fatto o non fatto
067500170111     c                   IF        wfatto
067600170111     c                   eval      §ORFfatto = 'S'
067700170111     c                   ELSE
067800170111     c                   eval      §ORFfatto = 'N'
067900170111     c                   ENDIF
068000161207     c                   ENDIF
068100070905     c                   if        (or61mtdi = 'M' or or61mtdi = 'X')
068200071106     c                              and or61nftl <> 0
068300070716     c                   eval      §orfass = 'M'
068400070716     c                   endif
068500070905     c                   if        (or61mtdi = 'M' or or61mtdi = 'X')
068600071106     c                              and or61nftl = 0
068700070716     c                   eval      §orfass = 'A'
068800070716     c                   endif
068900170111     c                   eval      ORFflo = dORF01
069000070716     c                   write     fnorf000
069100070716
069200070716     c                   endsr
069300070716
069400070716      *------------------------------------------------------------------------*
069500070716      * ROUTINE INIZIALE
069600070716      *------------------------------------------------------------------------*
069700070716     c     *inzsr        begsr
069800070716
069900070716     c     *entry        plist
070000070716     c                   parm                    kpjba
070100070720     c                   parm                    fior61ds
070200070716
070300070717     c                   eval      pages = or61pag
070400070717     c                   eval      riga  = or61riga
070500070717     c                   clear                   or61erro
070600080616     c                   eval      wndc = or61nfvi
070700070716
070800070716      * inverto la data del giorno
070900070716     c                   time                    w0140
071000070716     c                   clear                   wlbdat
071100070716     c                   move      w0140         g08dat
071200070716     c                   call      'XSRDA8'
071300070716     c                   parm                    wlbdat
071400070716     c                   z-add     g08inv        dateu
071500070716
071600070716      * controllo se la filiale è partita
071700070716     c                   clear                   og147
071800070717     c                   clear                   og148
071900070716     c                   clear                   comddao
072000070716     c     or61fgsi      chain     azorg01l
072100070323     c                   if        %found(azorg01l)
072200070716     c                   eval      og147 = orgde7
072300070717     c                   eval      og148 = orgde8
072400080124      * se filiale non partita esco (partita anche solo con fase 1)
072500080124     c                   if        §ogcgio = *blanks
072600070716     c                   eval      *inlr = *on
072700070323     c                   return
072800070716     c                   endif
072900080124     c                   endif
073000170111
073100170111      /free
073200170111      //?Se richiamato per aggiornare ORM RC devo controllare se
073300170111      //?è stata fatta la fase 'RCH' o 'RQE' sulla distinta
073400170111      //?perchè è l'unico caso in cui il chiamante non fa questo controllo
073500170111        IF  OR61rc = 'S';
073600170111        //?provo prima con la fase 'RCH'
073700170111          clear FIOR90DS;
073800170111          OR90tla = 'C';
073900170111          OR90fas = 'RCH';
074000170111          OR90fgs = OR61fgsi;
074100170111          OR90ndc = OR61nfvi;
074200170111          fior90r (kpjba:fior90ds);
074300170111        //?poi provo con la fase 'RQE'
074400170111          IF  OR90ndcok = *blanks;
074500170111            clear FIOR90DS;
074600170111            OR90tla = 'C';
074700170111            OR90fas = 'RQE';
074800170111            OR90fgs = OR61fgsi;
074900170111            OR90ndc = OR61nfvi;
075000170111            fior90r (kpjba:fior90ds);
075100170111          ENDIF;
075200170111        //?Ho trovato una delle due fasi esco dal pgm
075300170111          IF  OR90ndcok <> *blanks;
075400170111            *inlr = *on;
075500170111            return;
075600170111          ENDIF;
075700170111        ENDIF;
075800170111      /end-free
075900070716
076000070716      * dati utente
076100030718     c     *dtaara       define    §azute        azuteds
076200030718     c     *dtaara       define    §datiute      ddatiute
076300070716     c                   if        %error or rsut = *blanks
076400070716     c                   clear                   tibs34ds
076500070716     c                   call      'TIBS34R'
076600070716     c                   parm                    tibs34ds
076700070716     c                   in        *dtaara
076800070716     c                   endif
076900070716
077000070717      *  klist
077100070717     c     korm          klist
077200070716     c                   kfld                    orgpoe
077300070716     c                   kfld                    orgnsr
077400070716     c                   kfld                    orgnor
077500070716     c                   kfld                    orgnrv
077600070716
077700070717     c     korm1         klist
077800070717     c                   kfld                    ormpoe
077900070717     c                   kfld                    ormnsr
078000070717     c                   kfld                    ormnor
078100070717     c                   kfld                    ormnrv
078200070717     c                   kfld                    ormdfo
078300070717     c                   kfld                    ormofo
078400070717     c                   kfld                    ormfao
078500080616
078600080730     c     kfidst        klist
078700080730     c                   kfld                    dstnpg
078800080730     c                   kfld                    wndc
078900080730     c                   kfld                    or61fgsi
079000070717
079100070716     c                   endsr
079200070716** msg
079300070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
