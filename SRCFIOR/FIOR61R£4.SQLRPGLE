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
001400070716     fazorg01l  if   e           k disk
001500080730     ffidst01l  if   e           k disk
001600070913     ffior63p   o    e             printer infds(prterr)
001700080715     ffior61p   o    e             printer
001800070716
001900070716      *------------------------------------------------------------------------*
002000070716      *  RIEPILOGO INDICATORI
002100070716      *------------------------------------------------------------------------*
002200070716      *  80 - of printer file
002300070716      *------------------------------------------------------------------------*
002400070716
002500070716      *------------------------------------------------------------------------*
002600070716      *   V A R I A B I L I
002700070716      *------------------------------------------------------------------------*
002800070716     d comddao         s              8  0
002900070716     d dateu           s              8  0
003000070716     d dtaeur          s               d   datfmt(*eur)
003100070716     d dtaiso          s               d
003200070717     d savormdfo       s                   like(ormdfo)
003300070716     d savormfao       s                   like(ormfao)
003400070717     d savormofo       s                   like(ormofo)
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
006400080314     d §orfass                18     18
006500070716
006600070716     d azuteds       e ds                  extname(azute00f)
006700070716     d ddatiute      e ds
006800070716     d dfar          e ds
006900070716     d fior61ds      e ds
006901140212     d fior82ds      e ds
007000070716     d fnorgds       e ds                  extname(fnorg00f)
007100070717     d fnormds       e ds                  extname(fnorm00f)
007200070716     d kpjba         e ds
007300070716     d og147         e ds
007400070717     d og148         e ds
007500070716     d tibs02ds      e ds
007600070716     d tibs34ds      e ds                  inz
007700070716     d trul82ds      e ds
007800070716
007801140212      /copy gaitrasrc/srcprotopr,fior82r
007900070716      *------------------------------------------------------------------------*
008000071030
008100071030     c/exec sql
008200071030     c+ set option dynusrprf = *owner, closqlcsr = *endmod
008300071030     c/end-exec
008400070716
008500070716      * metto in distinta
008600070716
008700070716     c                   if        or61mtdi = 'M'
008800070716     c                   exsr      sr_metti
008900070716     c                   endif
009000070716
009100070716      * tolgo dalla distinta
009200070716     c                   if        or61mtdi = 'T'
009300070716     c                   exsr      sr_togli
009400070716     c                   endif
009500070905
009600070905      * trasferisco da una distinta all'altra
009700070905     c                   if        or61mtdi = 'X'
009800070905     c                   exsr      sr_trasferisci
009900070905     c                   endif
010000070716
010100070716     c                   if        or61cmti= 'S'
010200070716     c                   commit
010300070716     c                   endif
010400070716
010500070716     c                   eval      or61pag = pages
010600070716     c                   eval      or61riga= riga
010700070716
010800070716     c                   eval      *inlr = *on
010900070716
011000070716      *------------------------------------------------------------------------*
011100070716      * Leggo i ritiri messi in distinta su ORG e li inserisco su ORM
011200070716      *------------------------------------------------------------------------*
011300070716     c     sr_metti      begsr
011400070716
011500070716     C/EXEC SQL
011600070716     C+ DECLARE A1 CURSOR FOR SELECT fnorm01l.*, fnorg03l.*
011700070904     C+ FROM fnorm01l join fnorg03l on
011800070716     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
011900070716     C+ ormnrv=orgnrv
012000070717     C+ WHERE orgndc=:or61nfvi and orgddc=:or61dfvi and
012100071107     C+ orgfgs =:or61fgsi and ormndc=0 and ormeti <> ' '
012200070716     C+ ORDER BY fnorg03l.orgfgs, fnorg03l.orgndc, fnorg03l.orgddc
012300070716     C/END-EXEC
012400070716
012500070716     C/EXEC SQL
012600070716     C+ OPEN A1
012700070716     C/END-EXEC
012800070716
012900070716     c                   do        *hival
013000070716
013100070716     C/EXEC SQL
013200070716     C+ FETCH NEXT FROM A1 INTO :fnormds, :fnorgds
013300070716     C/END-EXEC
013400070716
013500070716     c                   select
013600070716     c                   when      sqlcod = 100
013700070716     c                   leave
013800070716     c                   when      sqlcod < 0
013900080715      * record non allocato
014000070716     c                   if        sqlcod <> -913
014100080715     c                   exsr      sr_stampaman
014200080715     c                   exsr      sr_fine
014300070716     c                   endif
014400080715      * record allocato
014500070716     c                   exsr      sr_stampa
014600070716     c                   exsr      sr_fine
014700070716
014800070716     c                   other
014900071001      * solo se non chiamato per bolle RC
015000071001     c                   if        or61rc = *blanks
015100070807      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
015200070807      * solo automatiche (or61nftl = 0)
015300070716     c                   if        or61nftl = 99999 and orgnftl <> 99999
015400070716     c                   iter
015500070716     c                   endif
015600070716     c                   if        or61nftl <> 99999 and orgnftl = 99999
015700070716     c                   iter
015800070716     c                   endif
015900071001     c                   endif
016000070716
016100070716     c                   exsr      sr_mettiorm
016200070716     c                   endsl
016300070716
016400070716     c                   enddo
016500070716
016600070716     C/EXEC SQL
016700070716     C+ CLOSE A1
016800070716     C/END-EXEC
016900070717
017000070717     c                   endsr
017100070716
017200070717      *------------------------------------------------------------------------*
017300070717      * Leggo i ritiri che sono ancora in distinta su ORM e che sono da togliere
017400070717      *------------------------------------------------------------------------*
017500070717     c     sr_togli      begsr
017600070717
017700070717     C/EXEC SQL
017800080303     C+ DECLARE B1 CURSOR FOR SELECT distinct  fnorm01l.*,
017900080303     C+ fnorf01l.orffgs, fnorf01l.orfndc,
018000080314     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as §orfass,
018100070717     C+ fnorg01l.*
018200070717     C+ FROM (fnorm01l join fnorf01l on
018300070717     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
018400070717     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
018500070717     C+ ormofo=orfore) join fnorg01l on
018600070717     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
018700070717     C+ ormnrv=orgnrv
018800070717     C+ WHERE orfndc=:or61nfvi and orfddc=:or61dfvi and
018900080221     C+ orffgs =:or61fgsi and orgndc=0 and ormndc > 0
019000070717     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
019100070717     C/END-EXEC
019200070717
019300070717     C/EXEC SQL
019400070717     C+ OPEN B1
019500070717     C/END-EXEC
019600070717
019700070717     c                   do        *hival
019800070717
019900070717     C/EXEC SQL
020000070717     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorfds, :fnorgds
020100070717     C/END-EXEC
020200070717
020300070717     c                   select
020400070717     c                   when      sqlcod = 100
020500070717     c                   leave
020600070717     c                   when      sqlcod < 0
020700080715      * record non allocato
020800070717     c                   if        sqlcod <> -913
020900080715     c                   exsr      sr_stampaman
021000080715     c                   exsr      sr_fine
021100070717     c                   endif
021200080715      * record allocato
021300070717     c                   exsr      sr_stampa
021400070717     c                   exsr      sr_fine
021500070717
021600070717     c                   other
021700071001      * solo se non chiamato per bolle RC
021800071001     c                   if        or61rc = *blanks
021900070717      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
022000070717      * solo automatiche (or61nftl = 0)
022100070921     c                   if        or61nftl = 99999 and §orfass <> 'M'
022200070717     c                   iter
022300070717     c                   endif
022400070921     c                   if        or61nftl = 0      and §orfass = 'M'
022500070717     c                   iter
022600070717     c                   endif
022700071001     c                   endif
022800070717
022900070717     c                   exsr      sr_togliorm
023000070717     c                   endsl
023100070717
023200070717     c                   enddo
023300070717
023400070717     C/EXEC SQL
023500070717     C+ CLOSE B1
023600070717     C/END-EXEC
023700070717
023800070717     c                   endsr
023900070905
024000070905      *------------------------------------------------------------------------*
024100070905      * Leggo i ritiri che sono ancora in distinta su ORM e che sono da trasferire
024200070905      *------------------------------------------------------------------------*
024300070905     c     sr_trasferiscibegsr
024400070905
024500070905     C/EXEC SQL
024600080303     C+ DECLARE C1 CURSOR FOR SELECT distinct  fnorm01l.*,
024700080303     C+ fnorf01l.orffgs, fnorf01l.orfndc,
024800080314     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as §orfass,
024900070905     C+ fnorg01l.*
025000070905     C+ FROM (fnorm01l join fnorf01l on
025100070905     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
025200070905     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
025300070905     C+ ormofo=orfore) join fnorg01l on
025400070905     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
025500070905     C+ ormnrv=orgnrv
025600070905     C+ WHERE orfndc=:or61nfvi and orfddc=:or61dfvi and
025700070905     C+ orffgs =:or61fgsi and orgndc=:or61nfvin
025800070905     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
025900070905     C/END-EXEC
026000070905
026100070905     C/EXEC SQL
026200070905     C+ OPEN C1
026300070905     C/END-EXEC
026400070905
026500070905     c                   do        *hival
026600070905
026700070905     C/EXEC SQL
026800070905     C+ FETCH NEXT FROM C1 INTO :fnormds, :fnorfds, :fnorgds
026900070905     C/END-EXEC
027000070905
027100070905     c                   select
027200070905     c                   when      sqlcod = 100
027300070905     c                   leave
027400070905     c                   when      sqlcod < 0
027500080715      * record non allocato
027600070905     c                   if        sqlcod <> -913
027700080715     c                   exsr      sr_stampaman
027800080715     c                   exsr      sr_fine
027900070905     c                   endif
028000080715      * record allocato
028100070905     c                   exsr      sr_stampa
028200070905     c                   exsr      sr_fine
028300070905
028400070905     c                   other
028500071001      * solo se non chiamato per bolle RC
028600071001     c                   if        or61rc = *blanks
028700070905      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
028800070905      * solo automatiche (or61nftl = 0)
028900070921     c                   if        or61nftl = 99999 and §orfass <> 'M'
029000070905     c                   iter
029100070905     c                   endif
029200070921     c                   if        or61nftl = 0      and §orfass = 'M'
029300070905     c                   iter
029400070905     c                   endif
029500071001     c                   endif
029600070905
029700070905     c                   exsr      sr_togliorm
029800070905     c                   exsr      sr_mettiorm
029900070905     c                   endsl
030000070905
030100070905     c                   enddo
030200070905
030300070905     C/EXEC SQL
030400070905     C+ CLOSE C1
030500070905     C/END-EXEC
030600070905
030700070905     c                   endsr
030800070717
030900070716      *------------------------------------------------------------------------*
031000070716      * Aggiorna ORM con riferimenti distinta
031100070716      *------------------------------------------------------------------------*
031200070716     c     sr_mettiorm   begsr
031300070716
031400070716      * controllo che l'ORM non sia allocato
031500070716     c     korm          chain(e)  fnorm01l
031600070206     c                   if        %error
031700070716      * se allocato mando msg a chi alloca e reperisco il RRN del file
031800070716     c     korm          chain(n)  fnorm01l
031900070716     c                   if        %found(fnorm01l)
032000070716     c                   exsr      sr_geslck
032100070716     c                   if        ul82§sts = 'A'
032200070716     c                   eval      *in85 = *on
032300070716     c                   exsr      sr_stampa
032400071030     c                   exsr      sr_fine
032500070716     c                   endif
032600070716      * non allocato impegno l'ORM
032700070716     c     korm          chain     fnorm01l
032800070716     c                   endif
032900070716     c                   endif
033000070716      * non trovo ORM errore
033100070716     c                   if        not %found(fnorm01l)
033200070716     c                   eval      *in84 = *on
033300070716     c                   exsr      sr_stampa
033400071030     c                   exsr      sr_fine
033500070716     c                   endif
033600070716      * controllo orm
033700070716     c                   if        ormndc <> *zeros
033800070716     c                   eval      *in85 = *on
033900070716     c                   exsr      sr_stampa
034000071030     c                   exsr      sr_fine
034100070716     c                   endif
034200090717      * l'orm deve essere ancora in fase assegnabil
034300090717     c                   if        ormeti = ''
034400090717     c                   eval      *in85 = *on
034500090717     c                   exsr      sr_stampa
034600090717     c                   exsr      sr_fine
034700090717     c                   endif
034800070716
034900070716      * mi salvo i dati della fase presente sulla testa ORM per salvarla su ORG
035000070716     c                   eval      savormfao = ormfao
035100070717     c                   eval      savormdfo = ormdfo
035200070717     c                   eval      savormofo = ormofo
035300070716
035400070716      * imposto la distinta su ORM e scrivo ORF
035500070716     c                   eval      ormnpg = wnpg
035600070716     c                   eval      ormndc = orgndc
035700070716     c                   eval      ormddc = orgddc
035800070716     c                   eval      ormfao = 400
035900070716     c                   time                    w0140
036000070716     c                   clear                   wlbdat
036100070716     c                   move      w0140         g08dat
036200070716     c                   call      'XSRDA8'
036300070716     c                   parm                    wlbdat
036400070716     c                   z-add     g08inv        ormdfo
036500070716     c                   movel     w0140         ormofo
036600070716     c                   z-add     g08inv        ormdtt
036700070716     c                   exsr      sr_fase
036800070716     c                   eval      ormeti = d§farass
036900070716
037000080317     c                   eval      wmetti = *on
037100070716     c                   exsr      sr_wrtorf
037200070716
037300070716     c     korm          chain     fnorg01l
037400070716     c                   if        %found(fnorg01l)
037500070716     c                   eval      orgfao = savormfao
037600070717     c                   eval      orgdfo = savormdfo
037700070717     c                   eval      orgofo = savormofo
037800070716     c                   update    fnorg000
037900070716     c                   endif
038000070716
038100070716     c                   update    fnorm000
038200070716
038300070716     c                   endsr
038400070717
038500070717      *------------------------------------------------------------------------*
038600070717      * Aggiorna ORM con fase precedente
038700070717      *------------------------------------------------------------------------*
038800070717     c     sr_togliorm   begsr
038900070717
039000070717      * controllo che l'ORM non sia allocato
039100070717     c     korm          chain(e)  fnorm01l
039200070717     c                   if        %error
039300070717      * se allocato mando msg a chi alloca e reperisco il RRN del file
039400070717     c     korm          chain(n)  fnorm01l
039500070717     c                   if        %found(fnorm01l)
039600070717     c                   exsr      sr_geslck
039700070717     c                   if        ul82§sts = 'A'
039800070717     c                   eval      *in85 = *on
039900070717     c                   exsr      sr_stampa
040000071030     c                   exsr      sr_fine
040100070717     c                   endif
040200070717      * non allocato impegno l'ORM
040300070717     c     korm          chain     fnorm01l
040400070717     c                   endif
040500070717     c                   endif
040600070717      * non trovo ORM errore
040700070717     c                   if        not %found(fnorm01l)
040800070717     c                   eval      *in84 = *on
040900070717     c                   exsr      sr_stampa
041000071030     c                   exsr      sr_fine
041100070717     c                   endif
041200070717      * controllo orm
041300070717     c                   if        ormndc = *zeros
041400070717     c                   eval      *in85 = *on
041500070717     c                   exsr      sr_stampa
041600071030     c                   exsr      sr_fine
041700070717     c                   endif
041800080312
041900080312      * se ORM con esito da PDA non devo impostare la fase precedente
042000080312      * ma creare la fase 390
042100080312     c                   if        ormfao = 410 or ormfao = 420
042200080312      * tolgo la distinta su ORM e scrivo ORF fase 390
042300080312     c                   clear                   ormnpg
042400080312     c                   clear                   ormndc
042500080312     c                   clear                   ormddc
042600080312     c                   eval      ormfao = 390
042700080312     c                   time                    w0140
042800080312     c                   clear                   wlbdat
042900080312     c                   move      w0140         g08dat
043000080312     c                   call      'XSRDA8'
043100080312     c                   parm                    wlbdat
043200080312     c                   z-add     g08inv        ormdfo
043300080312     c                   movel     w0140         ormofo
043400080312     c                   z-add     g08inv        ormdtt
043500080312     c                   exsr      sr_fase
043600080312     c                   eval      ormeti = d§farass
043700080312      * cancello la data di stampa (ORM Comunicato)
043800080312     c                   clear                   ormdst
043900080312      * scrivo la fase 390
044000080317     c                   eval      wmetti = *off
044100080312     c                   exsr      sr_wrtorf
044200080312      * tolgo fase precedente da ORG
044300080312     c     korm          chain     fnorg01l
044400080312     c                   if        %found(fnorg01l)
044500080312     c                   clear                   orgfao
044600080312     c                   clear                   orgdfo
044700080312     c                   clear                   orgofo
044800080312     c                   update    fnorg000
044900080312     c                   endif
045000080312
045100080312     c                   update    fnorm000
045200080312
045300080312      * se non ha avuto esito da PDA
045400080312     c                   else
045500070717
045600070717      * aggancio ORG per recuperare la fase precedente
045700070717     c     korm          chain     fnorg01l
045800070717     c                   if        %found(fnorg01l)
045900070717     c                   eval      savormfao = orgfao
046000070717     c                   eval      savormdfo = orgdfo
046100070717     c                   eval      savormofo = orgofo
046200070717     c                   clear                   orgfao
046300070717     c                   clear                   orgdfo
046400070717     c                   clear                   orgofo
046500070717     c                   update    fnorg000
046600070717     c                   endif
046700070717
046800070717      * cancello la fase da ORF
046900070717     c     korm1         chain     fnorf01l
047000070717     c                   if        %found(fnorg01l)
047100070717     c                   delete    fnorf000
047200070717     c                   endif
047300070717
047400070717      * cancello la distinta su ORM e imposto la fase precedente
047500070717     c                   clear                   ormnpg
047600070717     c                   clear                   ormndc
047700070717     c                   clear                   ormddc
047800070717     c                   eval      ormfao = savormfao
047900070717     c                   eval      ormdfo = savormdfo
048000070717     c                   eval      ormofo = savormofo
048100070717     c                   exsr      sr_fase
048200070717     c                   eval      ormeti = d§farass
048300070921      * cancello la data di stampa (ORM Comunicato)
048400070921     c                   clear                   ormdst
048500070717     c                   update    fnorm000
048600080312
048700080312     c                   endif
048800080911
048900080911      * aggancio la distinta per recupera l'AUT
049000080911     c                   eval      dstnpg = 4
049100080911     c     kfidst        chain     fidst01l
049200080911     c                   if        not %found(fidst01l)
049300080911     c                   clear                   dstpdr
049400080911     c                   endif
049500080911      * cancello la richiesta info
050001140212      /free
050002140212         clear fior82ds;
050003140212         c82fgs = or61fgsi;
050004140212         c82ndc = wndc ;
050005140212         c82codaut = dstpdr;
050006140212         c82POE = ormpoe;
050007140212         c82NSR = ormnsr;
050008140212         c82NOR = ormnor;
050009140212         c82NRV = ormnrv;
050010140212         c82aggvis = 'S';
050011140212         kpjbu = fior82ds;
050012140212         fior82r(kpjba);
050013140212         fior82ds = kpjbu;
050100070717
050101140212      /end-free
050200070717     c                   endsr
050300070716
050400070716      *------------------------------------------------------------------------*
050500070716      * Stampa errore
050600070716      *------------------------------------------------------------------------*
050700070716     c     sr_stampa     begsr
050800070716
050900070716     c                   move      or61dfvi      dtaiso
051000070227     c                   move      dtaiso        dtaeur
051100070227     c                   move      dtaeur        prtdfv
051200070716     c                   move      orgndc        prtnfv
051300070716
051400070704     c                   if        *in85
051500070716     c                   eval      prtpoe = orgpoe
051600070716     c                   eval      prtnsr = orgnsr
051700070716     c                   eval      prtnor = orgnor
051800070716     c                   eval      prtnrv = orgnrv
051900070716     c                   eval      prtmsg = 'L''ORM non è stato aggiornato corr-
052000070716     c                             ettamente. VERIFICARLO'
052100070716     c                   endif
052200070716
052300070716      * pagina in stampa
052400070913     c                   if        riga >= 60
052500070716     c                   eval      *in80 = *on
052600070716     c                   add       1             pages
052700070716     c                   eval      riga = 3
052800070716     c                   endif
052900070716
053000070913     c   80              write     testa
053100070716     c                   eval      *in80 = *off
053200070716
053300070319     c   82              write     riga2
053400070716     c   82              add       1             riga
053500070716
053600070227     c   84              write     riga4
053700070716     c   84              add       1             riga
053800070716
053900070704     c   85              write     riga5
054000070716     c   85              add       1             riga
054100070716
054200070704     c                   setoff                                       828485
054300070716
054400070227     c                   endsr
054500080715
054600080715      *------------------------------------------------------------------------*
054700080715      * Stampa errore SQL
054800080715      *------------------------------------------------------------------------*
054900080715     c     sr_stampaman  begsr
055000080715
055100080715     c                   if        or61nftl = *all'9'
055200080715     c                   eval      nompgm = 'FIOR60R'
055300080715     c                   else
055400080715     c                   eval      nompgm = 'FIOR61R'
055500080715     c                   endif
055600080715
055700080715     c                   move      or61dfvi      dtaiso
055800080715     c                   move      dtaiso        dtaeur
055900080715     c                   move      dtaeur        prtdfv
056000080715     c                   move      or61nfvi      prtnfv
056100080715     c                   eval      errore = sqlcod
056200080715
056300080715      * pagina in stampa
056400080715     c                   if        *inof or not *in99
056500080715     c                   write     or61t
056600080715     c                   eval      *inof = *off
056700080715     c                   eval      *in99 = *on
056800080715     c                   endif
056900080715
057000080715     c                   write     or61r
057100080715
057200080715     c                   endsr
057300070716
057400070716      *------------------------------------------------------------------------*
057500070716      * Esco con codice d'errore
057600070716      *------------------------------------------------------------------------*
057700070716     c     sr_fine       begsr
057800070716
057900070716     c                   if        or61cmti= 'S'
058000070206     c                   rolbk
058100070716     c                   endif
058200070716
058300070716     c                   eval      or61erro= '1'
058400070716     c                   eval      *inlr = *on
058500070206     c                   return
058600070716
058700070206     c                   endsr
058800070716
058900070716      *------------------------------------------------------------------------*
059000070716      * Messaggio per ORM allocato
059100070716      *------------------------------------------------------------------------*
059200070716     c     sr_geslck     begsr
059300070716
059400070716     c                   clear                   trul82ds
059500070716     c                   eval      ul82§rrn = ormnrr
059600070716     c                   eval      ul82§fil = 'FNORM00F'
059700070716     c                   eval      ul82§win = 'S'
059800070716     c                   eval      ul82§num = 2
059900070716     c                   eval      ul82§att = 2
060000070716     c                   eval      ul82§mss = msg(01)
060100070716     c                   eval      ul82§msw = 'L''ORM '
060200070716     c                             + %editc(ormpoe:'X') +  ' '
060300070717     c                             + %editc(ormnsr:'Z') +  ' '
060400070716     c                             + %editc(ormnor:'Z') +  ' '
060500070716     c                             + %editc(ormnrv:'Z') +  ' è già utilizzato -
060600070716     c                             da un altro lavoro e non è manutenzionabile'
060700070716     c                   call(e)   'TRUL82R'
060800070716     c                   parm                    trul82ds
060900070716
061000070716     c                   endsr
061100070716
061200070716      *---------------------------------------------------------------*
061300070716      *  Tabella fase                                                 *
061400070716      *---------------------------------------------------------------*
061500070716     c     sr_fase       begsr
061600070716
061700070716     c                   clear                   tibs02ds
061800070716     c                   eval      t02mod = 'C'
061900070716     c                   eval      t02sif = knsif
062000070716     c                   eval      t02cod = 'FAR'
062100070716     c                   movel(p)  ormfao        t02ke1
062200070716     c                   call      'TIBS02R'
062300070716     c                   parm                    kpjba
062400070716     c                   parm                    tibs02ds
062500070716     c                   eval      dfar = t02uni
062600070716
062700070716     c                   endsr
062800070716      *------------------------------------------------------------------------*
062900070716      * Scrivo la fase
063000070716      *------------------------------------------------------------------------*
063100070716     c     sr_wrtorf     begsr
063200070716
063300070716     c                   clear                   fnorf000
063400070716     c                   eval      orfpoe = ormpoe
063500070716     c                   eval      orfnsr = ormnsr
063600070716     c                   eval      orfnor = ormnor
063700070716     c                   eval      orfnrv = ormnrv
063800070716     c                   eval      orfpog = or61fgsi
063900070716     c                   eval      orfdae = ormdfo
064000070716     c                   eval      orfore = ormofo
064100070716     c                   eval      orffar = ormfao
064200070716     c                   eval      orfcar = *blanks
064300070716     c                   eval      orfpue = knmus
064400070716     c                   eval      orffgs = orgfgs
064500070716     c                   eval      orfndc = orgndc
064600070716     c                   eval      orfddc = orgddc
064700070905     c                   if        (or61mtdi = 'M' or or61mtdi = 'X')
064800071106     c                              and or61nftl <> 0
064900070716     c                   eval      §orfass = 'M'
065000070716     c                   endif
065100070905     c                   if        (or61mtdi = 'M' or or61mtdi = 'X')
065200071106     c                              and or61nftl = 0
065300070716     c                   eval      §orfass = 'A'
065400070716     c                   endif
065500080314     c                   movel     §orfass       orfflo
065600070716     c                   write     fnorf000
065700070716
065800070716     c                   endsr
065900070716
066000070716      *------------------------------------------------------------------------*
066100070716      * ROUTINE INIZIALE
066200070716      *------------------------------------------------------------------------*
066300070716     c     *inzsr        begsr
066400070716
066500070716     c     *entry        plist
066600070716     c                   parm                    kpjba
066700070720     c                   parm                    fior61ds
066800070716
066900070717     c                   eval      pages = or61pag
067000070717     c                   eval      riga  = or61riga
067100070717     c                   clear                   or61erro
067200080616     c                   eval      wndc = or61nfvi
067300070716
067400070716      * inverto la data del giorno
067500070716     c                   time                    w0140
067600070716     c                   clear                   wlbdat
067700070716     c                   move      w0140         g08dat
067800070716     c                   call      'XSRDA8'
067900070716     c                   parm                    wlbdat
068000070716     c                   z-add     g08inv        dateu
068100070716
068200070716      * controllo se la filiale è partita
068300070716     c                   clear                   og147
068400070717     c                   clear                   og148
068500070716     c                   clear                   comddao
068600070716     c     or61fgsi      chain     azorg01l
068700070323     c                   if        %found(azorg01l)
068800070716     c                   eval      og147 = orgde7
068900070717     c                   eval      og148 = orgde8
069000080124      * se filiale non partita esco (partita anche solo con fase 1)
069100080124     c                   if        §ogcgio = *blanks
069200070716     c                   eval      *inlr = *on
069300070323     c                   return
069400070716     c                   endif
069500070717
069600080124     c                   endif
069700070716
069800070716      * dati utente
069900030718     c     *dtaara       define    §azute        azuteds
070000030718     c     *dtaara       define    §datiute      ddatiute
070100070716     c                   if        %error or rsut = *blanks
070200070716     c                   clear                   tibs34ds
070300070716     c                   call      'TIBS34R'
070400070716     c                   parm                    tibs34ds
070500070716     c                   in        *dtaara
070600070716     c                   endif
070700070716
070800070717      *  klist
070900070717     c     korm          klist
071000070716     c                   kfld                    orgpoe
071100070716     c                   kfld                    orgnsr
071200070716     c                   kfld                    orgnor
071300070716     c                   kfld                    orgnrv
071400070716
071500070717     c     korm1         klist
071600070717     c                   kfld                    ormpoe
071700070717     c                   kfld                    ormnsr
071800070717     c                   kfld                    ormnor
071900070717     c                   kfld                    ormnrv
072000070717     c                   kfld                    ormdfo
072100070717     c                   kfld                    ormofo
072200070717     c                   kfld                    ormfao
072300080616
073300080730     c     kfidst        klist
073400080730     c                   kfld                    dstnpg
073500080730     c                   kfld                    wndc
073600080730     c                   kfld                    or61fgsi
073700070717
073800070716     c                   endsr
073900070716** msg
074000070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
