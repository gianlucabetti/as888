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
001300080616     ffipro01l  uf   e           k disk    commit
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
003600080616     d wndc            s                   like(prondc)
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
006900070717     d fior56ds      e ds
007000070716     d fior61ds      e ds
007100070716     d fnorgds       e ds                  extname(fnorg00f)
007200070717     d fnormds       e ds                  extname(fnorm00f)
007300070716     d kpjba         e ds
007400070716     d og147         e ds
007500070717     d og148         e ds
007600070716     d tibs02ds      e ds
007700070716     d tibs34ds      e ds                  inz
007800070716     d trul82ds      e ds
007900070716
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
014400080715     c**!!!              eval      *inlr = *on
014500080715     c**!!!              return
014600070716     c                   endif
014700080715      * record allocato
014800070716     c                   exsr      sr_stampa
014900070716     c                   exsr      sr_fine
015000070716
015100070716     c                   other
015200071001      * solo se non chiamato per bolle RC
015300071001     c                   if        or61rc = *blanks
015400070807      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
015500070807      * solo automatiche (or61nftl = 0)
015600070716     c                   if        or61nftl = 99999 and orgnftl <> 99999
015700070716     c                   iter
015800070716     c                   endif
015900070716     c                   if        or61nftl <> 99999 and orgnftl = 99999
016000070716     c                   iter
016100070716     c                   endif
016200071001     c                   endif
016300070716
016400070716     c                   exsr      sr_mettiorm
016500070716     c                   endsl
016600070716
016700070716     c                   enddo
016800070716
016900070716     C/EXEC SQL
017000070716     C+ CLOSE A1
017100070716     C/END-EXEC
017200070717
017300070717     c                   endsr
017400070716
017500070717      *------------------------------------------------------------------------*
017600070717      * Leggo i ritiri che sono ancora in distinta su ORM e che sono da togliere
017700070717      *------------------------------------------------------------------------*
017800070717     c     sr_togli      begsr
017900070717
018000070717     C/EXEC SQL
018100080303     C+ DECLARE B1 CURSOR FOR SELECT distinct  fnorm01l.*,
018200080303     C+ fnorf01l.orffgs, fnorf01l.orfndc,
018300080314     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as §orfass,
018400070717     C+ fnorg01l.*
018500070717     C+ FROM (fnorm01l join fnorf01l on
018600070717     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
018700070717     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
018800070717     C+ ormofo=orfore) join fnorg01l on
018900070717     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
019000070717     C+ ormnrv=orgnrv
019100070717     C+ WHERE orfndc=:or61nfvi and orfddc=:or61dfvi and
019200080221     C+ orffgs =:or61fgsi and orgndc=0 and ormndc > 0
019300070717     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
019400070717     C/END-EXEC
019500070717
019600070717     C/EXEC SQL
019700070717     C+ OPEN B1
019800070717     C/END-EXEC
019900070717
020000070717     c                   do        *hival
020100070717
020200070717     C/EXEC SQL
020300070717     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorfds, :fnorgds
020400070717     C/END-EXEC
020500070717
020600070717     c                   select
020700070717     c                   when      sqlcod = 100
020800070717     c                   leave
020900070717     c                   when      sqlcod < 0
021000080715      * record non allocato
021100070717     c                   if        sqlcod <> -913
021200080715     c                   exsr      sr_stampaman
021300080715     c                   exsr      sr_fine
021400080715     c**!!!              eval      *inlr = *on
021500080715     c**!!!              return
021600070717     c                   endif
021700080715      * record allocato
021800070717     c                   exsr      sr_stampa
021900070717     c                   exsr      sr_fine
022000070717
022100070717     c                   other
022200071001      * solo se non chiamato per bolle RC
022300071001     c                   if        or61rc = *blanks
022400070717      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
022500070717      * solo automatiche (or61nftl = 0)
022600070921     c                   if        or61nftl = 99999 and §orfass <> 'M'
022700070717     c                   iter
022800070717     c                   endif
022900070921     c                   if        or61nftl = 0      and §orfass = 'M'
023000070717     c                   iter
023100070717     c                   endif
023200071001     c                   endif
023300070717
023400070717     c                   exsr      sr_togliorm
023500070717     c                   endsl
023600070717
023700070717     c                   enddo
023800070717
023900070717     C/EXEC SQL
024000070717     C+ CLOSE B1
024100070717     C/END-EXEC
024200070717
024300070717     c                   endsr
024400070905
024500070905      *------------------------------------------------------------------------*
024600070905      * Leggo i ritiri che sono ancora in distinta su ORM e che sono da trasferire
024700070905      *------------------------------------------------------------------------*
024800070905     c     sr_trasferiscibegsr
024900070905
025000070905     C/EXEC SQL
025100080303     C+ DECLARE C1 CURSOR FOR SELECT distinct  fnorm01l.*,
025200080303     C+ fnorf01l.orffgs, fnorf01l.orfndc,
025300080314     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as §orfass,
025400070905     C+ fnorg01l.*
025500070905     C+ FROM (fnorm01l join fnorf01l on
025600070905     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
025700070905     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
025800070905     C+ ormofo=orfore) join fnorg01l on
025900070905     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
026000070905     C+ ormnrv=orgnrv
026100070905     C+ WHERE orfndc=:or61nfvi and orfddc=:or61dfvi and
026200070905     C+ orffgs =:or61fgsi and orgndc=:or61nfvin
026300070905     C+ ORDER BY fnorf01l.orffgs, fnorf01l.orfndc, fnorf01l.orfddc
026400070905     C/END-EXEC
026500070905
026600070905     C/EXEC SQL
026700070905     C+ OPEN C1
026800070905     C/END-EXEC
026900070905
027000070905     c                   do        *hival
027100070905
027200070905     C/EXEC SQL
027300070905     C+ FETCH NEXT FROM C1 INTO :fnormds, :fnorfds, :fnorgds
027400070905     C/END-EXEC
027500070905
027600070905     c                   select
027700070905     c                   when      sqlcod = 100
027800070905     c                   leave
027900070905     c                   when      sqlcod < 0
028000080715      * record non allocato
028100070905     c                   if        sqlcod <> -913
028200080715     c                   exsr      sr_stampaman
028300080715     c                   exsr      sr_fine
028400080715     c**!!!              eval      *inlr = *on
028500080715     c**!!!              return
028600070905     c                   endif
028700080715      * record allocato
028800070905     c                   exsr      sr_stampa
028900070905     c                   exsr      sr_fine
029000070905
029100070905     c                   other
029200071001      * solo se non chiamato per bolle RC
029300071001     c                   if        or61rc = *blanks
029400070905      * controllo se richiesto passaggio solo le manuali (or61nftl = 99999) o
029500070905      * solo automatiche (or61nftl = 0)
029600070921     c                   if        or61nftl = 99999 and §orfass <> 'M'
029700070905     c                   iter
029800070905     c                   endif
029900070921     c                   if        or61nftl = 0      and §orfass = 'M'
030000070905     c                   iter
030100070905     c                   endif
030200071001     c                   endif
030300070905
030400070905     c                   exsr      sr_togliorm
030500070905     c                   exsr      sr_mettiorm
030600070905     c                   endsl
030700070905
030800070905     c                   enddo
030900070905
031000070905     C/EXEC SQL
031100070905     C+ CLOSE C1
031200070905     C/END-EXEC
031300070905
031400070905     c                   endsr
031500070717
031600070716      *------------------------------------------------------------------------*
031700070716      * Aggiorna ORM con riferimenti distinta
031800070716      *------------------------------------------------------------------------*
031900070716     c     sr_mettiorm   begsr
032000070716
032100070716      * controllo che l'ORM non sia allocato
032200070716     c     korm          chain(e)  fnorm01l
032300070206     c                   if        %error
032400070716      * se allocato mando msg a chi alloca e reperisco il RRN del file
032500070716     c     korm          chain(n)  fnorm01l
032600070716     c                   if        %found(fnorm01l)
032700070716     c                   exsr      sr_geslck
032800070716     c                   if        ul82§sts = 'A'
032900070716     c                   eval      *in85 = *on
033000070716     c                   exsr      sr_stampa
033100071030     c                   exsr      sr_fine
033200070716     c                   endif
033300070716      * non allocato impegno l'ORM
033400070716     c     korm          chain     fnorm01l
033500070716     c                   endif
033600070716     c                   endif
033700070716      * non trovo ORM errore
033800070716     c                   if        not %found(fnorm01l)
033900070716     c                   eval      *in84 = *on
034000070716     c                   exsr      sr_stampa
034100071030     c                   exsr      sr_fine
034200070716     c                   endif
034300070716      * controllo orm
034400070716     c                   if        ormndc <> *zeros
034500070716     c                   eval      *in85 = *on
034600070716     c                   exsr      sr_stampa
034700071030     c                   exsr      sr_fine
034800070716     c                   endif
034900090717      * la filiale ritiro deve essere la stessa della distinta
035000090803      * non controllo per ORM RC
035100090803     c*****              if        ormpor <> or61fgsi
035200090803     c******                       and or61rc = *blanks
035300090803     c********           eval      *in85 = *on
035400090803     c*********          exsr      sr_stampa
035500090803     c********           exsr      sr_fine
035600090803     c********           endif
035700090717      * l'orm deve essere ancora in fase assegnabil
035800090717     c                   if        ormeti = ''
035900090717     c                   eval      *in85 = *on
036000090717     c                   exsr      sr_stampa
036100090717     c                   exsr      sr_fine
036200090717     c                   endif
036300070716
036400070716      * mi salvo i dati della fase presente sulla testa ORM per salvarla su ORG
036500070716     c                   eval      savormfao = ormfao
036600070717     c                   eval      savormdfo = ormdfo
036700070717     c                   eval      savormofo = ormofo
036800070716
036900070716      * imposto la distinta su ORM e scrivo ORF
037000070716     c                   eval      ormnpg = wnpg
037100070716     c                   eval      ormndc = orgndc
037200070716     c                   eval      ormddc = orgddc
037300070716     c                   eval      ormfao = 400
037400070716     c                   time                    w0140
037500070716     c                   clear                   wlbdat
037600070716     c                   move      w0140         g08dat
037700070716     c                   call      'XSRDA8'
037800070716     c                   parm                    wlbdat
037900070716     c                   z-add     g08inv        ormdfo
038000070716     c                   movel     w0140         ormofo
038100070716     c                   z-add     g08inv        ormdtt
038200070716     c                   exsr      sr_fase
038300070716     c                   eval      ormeti = d§farass
038400070716
038500080317     c                   eval      wmetti = *on
038600070716     c                   exsr      sr_wrtorf
038700070716
038800070716     c     korm          chain     fnorg01l
038900070716     c                   if        %found(fnorg01l)
039000070716     c                   eval      orgfao = savormfao
039100070717     c                   eval      orgdfo = savormdfo
039200070717     c                   eval      orgofo = savormofo
039300070716     c                   update    fnorg000
039400070716     c                   endif
039500070716
039600070716     c                   update    fnorm000
039700070716
039800070716     c                   endsr
039900070717
040000070717      *------------------------------------------------------------------------*
040100070717      * Aggiorna ORM con fase precedente
040200070717      *------------------------------------------------------------------------*
040300070717     c     sr_togliorm   begsr
040400070717
040500070717      * controllo che l'ORM non sia allocato
040600070717     c     korm          chain(e)  fnorm01l
040700070717     c                   if        %error
040800070717      * se allocato mando msg a chi alloca e reperisco il RRN del file
040900070717     c     korm          chain(n)  fnorm01l
041000070717     c                   if        %found(fnorm01l)
041100070717     c                   exsr      sr_geslck
041200070717     c                   if        ul82§sts = 'A'
041300070717     c                   eval      *in85 = *on
041400070717     c                   exsr      sr_stampa
041500071030     c                   exsr      sr_fine
041600070717     c                   endif
041700070717      * non allocato impegno l'ORM
041800070717     c     korm          chain     fnorm01l
041900070717     c                   endif
042000070717     c                   endif
042100070717      * non trovo ORM errore
042200070717     c                   if        not %found(fnorm01l)
042300070717     c                   eval      *in84 = *on
042400070717     c                   exsr      sr_stampa
042500071030     c                   exsr      sr_fine
042600070717     c                   endif
042700070717      * controllo orm
042800070717     c                   if        ormndc = *zeros
042900070717     c                   eval      *in85 = *on
043000070717     c                   exsr      sr_stampa
043100071030     c                   exsr      sr_fine
043200070717     c                   endif
043300080312
043400080312      * se ORM con esito da PDA non devo impostare la fase precedente
043500080312      * ma creare la fase 390
043600080312     c                   if        ormfao = 410 or ormfao = 420
043700080312      * tolgo la distinta su ORM e scrivo ORF fase 390
043800080312     c                   clear                   ormnpg
043900080312     c                   clear                   ormndc
044000080312     c                   clear                   ormddc
044100080312     c                   eval      ormfao = 390
044200080312     c                   time                    w0140
044300080312     c                   clear                   wlbdat
044400080312     c                   move      w0140         g08dat
044500080312     c                   call      'XSRDA8'
044600080312     c                   parm                    wlbdat
044700080312     c                   z-add     g08inv        ormdfo
044800080312     c                   movel     w0140         ormofo
044900080312     c                   z-add     g08inv        ormdtt
045000080312     c                   exsr      sr_fase
045100080312     c                   eval      ormeti = d§farass
045200080312      * cancello la data di stampa (ORM Comunicato)
045300080312     c                   clear                   ormdst
045400080312      * scrivo la fase 390
045500080317     c                   eval      wmetti = *off
045600080312     c                   exsr      sr_wrtorf
045700080312      * tolgo fase precedente da ORG
045800080312     c     korm          chain     fnorg01l
045900080312     c                   if        %found(fnorg01l)
046000080312     c                   clear                   orgfao
046100080312     c                   clear                   orgdfo
046200080312     c                   clear                   orgofo
046300080312     c                   update    fnorg000
046400080312     c                   endif
046500080312
046600080312     c                   update    fnorm000
046700080312
046800080312      * se non ha avuto esito da PDA
046900080312     c                   else
047000070717
047100070717      * aggancio ORG per recuperare la fase precedente
047200070717     c     korm          chain     fnorg01l
047300070717     c                   if        %found(fnorg01l)
047400070717     c                   eval      savormfao = orgfao
047500070717     c                   eval      savormdfo = orgdfo
047600070717     c                   eval      savormofo = orgofo
047700070717     c                   clear                   orgfao
047800070717     c                   clear                   orgdfo
047900070717     c                   clear                   orgofo
048000070717     c                   update    fnorg000
048100070717     c                   endif
048200070717
048300070717      * cancello la fase da ORF
048400070717     c     korm1         chain     fnorf01l
048500070717     c                   if        %found(fnorg01l)
048600070717     c                   delete    fnorf000
048700070717     c                   endif
048800070717
048900070717      * cancello la distinta su ORM e imposto la fase precedente
049000070717     c                   clear                   ormnpg
049100070717     c                   clear                   ormndc
049200070717     c                   clear                   ormddc
049300070717     c                   eval      ormfao = savormfao
049400070717     c                   eval      ormdfo = savormdfo
049500070717     c                   eval      ormofo = savormofo
049600070717     c                   exsr      sr_fase
049700070717     c                   eval      ormeti = d§farass
049800070921      * cancello la data di stampa (ORM Comunicato)
049900070921     c                   clear                   ormdst
050000070717     c                   update    fnorm000
050100080312
050200080312     c                   endif
050300080911
050400080911      * aggancio la distinta per recupera l'AUT
050500080911     c                   eval      dstnpg = 4
050600080911     c     kfidst        chain     fidst01l
050700080911     c                   if        not %found(fidst01l)
050800080911     c                   clear                   dstpdr
050900080911     c                   endif
051000080911      * cancello la richiesta info
051100080911     c     kfipro        chain     fipro01l
051200080911     c                   if        %found(fipro01l)
051300080911     c                   clear                   proflgass
051400080911     c                   update    fipro000
051500080911     c                   endif
051600070717
051700070717      * se la filiale ritiro è attiva con il PDA aggiorno i dati
051800080707      * ma non lo faccio se aggiornamento relativo a bolla RC
051900080714      * NON LO FACCIO MAI....CI PENSA L'ASSEGNAZIONE MANUALE
052000080213     c                   if        §ogpdaorm <> *blanks
052100080707     c                             and or61rc = *blanks
052200070717      * aggiorno i dati inviati al PDA
052300070717     c                   clear                   fior56ds
052400080317     c                   eval      or56comit = '1'
052500070717     c                   eval      or56tla = 'A'
052600080714     c**!!!              exsr      sr_aggpdo
052700070717     c                   endif
052800070717
052900070717     c                   endsr
053000070716
053100070716      *------------------------------------------------------------------------*
053200070716      * Stampa errore
053300070716      *------------------------------------------------------------------------*
053400070716     c     sr_stampa     begsr
053500070716
053600070716     c                   move      or61dfvi      dtaiso
053700070227     c                   move      dtaiso        dtaeur
053800070227     c                   move      dtaeur        prtdfv
053900070716     c                   move      orgndc        prtnfv
054000070716
054100070704     c                   if        *in85
054200070716     c                   eval      prtpoe = orgpoe
054300070716     c                   eval      prtnsr = orgnsr
054400070716     c                   eval      prtnor = orgnor
054500070716     c                   eval      prtnrv = orgnrv
054600070716     c                   eval      prtmsg = 'L''ORM non è stato aggiornato corr-
054700070716     c                             ettamente. VERIFICARLO'
054800070716     c                   endif
054900070716
055000070716      * pagina in stampa
055100070913     c                   if        riga >= 60
055200070716     c                   eval      *in80 = *on
055300070716     c                   add       1             pages
055400070716     c                   eval      riga = 3
055500070716     c                   endif
055600070716
055700070913     c   80              write     testa
055800070716     c                   eval      *in80 = *off
055900070716
056000070319     c   82              write     riga2
056100070716     c   82              add       1             riga
056200070716
056300070227     c   84              write     riga4
056400070716     c   84              add       1             riga
056500070716
056600070704     c   85              write     riga5
056700070716     c   85              add       1             riga
056800070716
056900070704     c                   setoff                                       828485
057000070716
057100070227     c                   endsr
057200080715
057300080715      *------------------------------------------------------------------------*
057400080715      * Stampa errore SQL
057500080715      *------------------------------------------------------------------------*
057600080715     c     sr_stampaman  begsr
057700080715
057800080715     c                   if        or61nftl = *all'9'
057900080715     c                   eval      nompgm = 'FIOR60R'
058000080715     c                   else
058100080715     c                   eval      nompgm = 'FIOR61R'
058200080715     c                   endif
058300080715
058400080715     c                   move      or61dfvi      dtaiso
058500080715     c                   move      dtaiso        dtaeur
058600080715     c                   move      dtaeur        prtdfv
058700080715     c                   move      or61nfvi      prtnfv
058800080715     c                   eval      errore = sqlcod
058900080715
059000080715      * pagina in stampa
059100080715     c                   if        *inof or not *in99
059200080715     c                   write     or61t
059300080715     c                   eval      *inof = *off
059400080715     c                   eval      *in99 = *on
059500080715     c                   endif
059600080715
059700080715     c                   write     or61r
059800080715
059900080715     c                   endsr
060000070716
060100070716      *------------------------------------------------------------------------*
060200070716      * Esco con codice d'errore
060300070716      *------------------------------------------------------------------------*
060400070716     c     sr_fine       begsr
060500070716
060600070716     c                   if        or61cmti= 'S'
060700070206     c                   rolbk
060800070716     c                   endif
060900070716
061000070716     c                   eval      or61erro= '1'
061100070716     c                   eval      *inlr = *on
061200070206     c                   return
061300070716
061400070206     c                   endsr
061500070716
061600070716      *------------------------------------------------------------------------*
061700070716      * Messaggio per ORM allocato
061800070716      *------------------------------------------------------------------------*
061900070716     c     sr_geslck     begsr
062000070716
062100070716     c                   clear                   trul82ds
062200070716     c                   eval      ul82§rrn = ormnrr
062300070716     c                   eval      ul82§fil = 'FNORM00F'
062400070716     c                   eval      ul82§win = 'S'
062500070716     c                   eval      ul82§num = 2
062600070716     c                   eval      ul82§att = 2
062700070716     c                   eval      ul82§mss = msg(01)
062800070716     c                   eval      ul82§msw = 'L''ORM '
062900070716     c                             + %editc(ormpoe:'X') +  ' '
063000070717     c                             + %editc(ormnsr:'Z') +  ' '
063100070716     c                             + %editc(ormnor:'Z') +  ' '
063200070716     c                             + %editc(ormnrv:'Z') +  ' è già utilizzato -
063300070716     c                             da un altro lavoro e non è manutenzionabile'
063400070716     c                   call(e)   'TRUL82R'
063500070716     c                   parm                    trul82ds
063600070716
063700070716     c                   endsr
063800070716
063900070716      *---------------------------------------------------------------*
064000070716      *  Tabella fase                                                 *
064100070716      *---------------------------------------------------------------*
064200070716     c     sr_fase       begsr
064300070716
064400070716     c                   clear                   tibs02ds
064500070716     c                   eval      t02mod = 'C'
064600070716     c                   eval      t02sif = knsif
064700070716     c                   eval      t02cod = 'FAR'
064800070716     c                   movel(p)  ormfao        t02ke1
064900070716     c                   call      'TIBS02R'
065000070716     c                   parm                    kpjba
065100070716     c                   parm                    tibs02ds
065200070716     c                   eval      dfar = t02uni
065300070716
065400070716     c                   endsr
065500070716      *------------------------------------------------------------------------*
065600070716      * Scrivo la fase
065700070716      *------------------------------------------------------------------------*
065800070716     c     sr_wrtorf     begsr
065900070716
066000070716     c                   clear                   fnorf000
066100070716     c                   eval      orfpoe = ormpoe
066200070716     c                   eval      orfnsr = ormnsr
066300070716     c                   eval      orfnor = ormnor
066400070716     c                   eval      orfnrv = ormnrv
066500070716     c                   eval      orfpog = or61fgsi
066600070716     c                   eval      orfdae = ormdfo
066700070716     c                   eval      orfore = ormofo
066800070716     c                   eval      orffar = ormfao
066900070716     c                   eval      orfcar = *blanks
067000070716     c                   eval      orfpue = knmus
067100080317      * se metto ORM in distinta imposto i dati di ORG
067200080319     c**********         if        wmetti = *on
067300070716     c                   eval      orffgs = orgfgs
067400070716     c                   eval      orfndc = orgndc
067500070716     c                   eval      orfddc = orgddc
067600080319     c**********         endif
067700080317      * se tolgo dalla distinta e sono in questa routine vuol dire che devo scrivere
067800080317      * la fase 390 quindi imposto la distinta che sto togliendo
067900080319     c**********         if        wmetti = *off
068000080319     c**********         eval      orffgs = or61fgsi
068100080319     c**********         eval      orfndc = or61nfvi
068200080319     c**********         eval      orfddc = or61dfvi
068300080319     c**********         endif
068400070905     c                   if        (or61mtdi = 'M' or or61mtdi = 'X')
068500071106     c                              and or61nftl <> 0
068600070716     c                   eval      §orfass = 'M'
068700070716     c                   endif
068800070905     c                   if        (or61mtdi = 'M' or or61mtdi = 'X')
068900071106     c                              and or61nftl = 0
069000070716     c                   eval      §orfass = 'A'
069100070716     c                   endif
069200080314     c                   movel     §orfass       orfflo
069300070716     c                   write     fnorf000
069400070716
069500070716     c                   endsr
069600070717
069700070717      *--------------------------------------------------------------------*
069800070717      * Aggiorno file PDO
069900070717      *--------------------------------------------------------------------*
070000070717     c     sr_aggpdo     begsr
070100070717
070200080221      * se l'ORM non è presente su FIPDO mi torna errore il fior56r
070300071221     c                   eval      or56fgs = or61fgsi
070400071221     c                   eval      or56ndcd = or61nfvi
070500071221     c                   eval      or56ndca = or61nfvi
070600070717     c                   eval      or56poe = ormpoe
070700070717     c                   eval      or56nsr = ormnsr
070800070717     c                   eval      or56nor = ormnor
070900070717     c                   eval      or56nrv = ormnrv
071000070717     c                   call      'FIOR56R'
071100070717     c                   parm                    kpjbu
071200070717     c                   parm                    fior56ds
071300070717      * il pgm torna degli errori, ma dato che ho già fatto qua tutti i controlli
071400070717      * del caso, non me ne preoccupo
071500070717
071600070717     c                   endsr
071700070716
071800070716      *------------------------------------------------------------------------*
071900070716      * ROUTINE INIZIALE
072000070716      *------------------------------------------------------------------------*
072100070716     c     *inzsr        begsr
072200070716
072300070716     c     *entry        plist
072400070716     c                   parm                    kpjba
072500070720     c                   parm                    fior61ds
072600070716
072700070717     c                   eval      pages = or61pag
072800070717     c                   eval      riga  = or61riga
072900070717     c                   clear                   or61erro
073000080616     c                   eval      wndc = or61nfvi
073100070716
073200070716      * inverto la data del giorno
073300070716     c                   time                    w0140
073400070716     c                   clear                   wlbdat
073500070716     c                   move      w0140         g08dat
073600070716     c                   call      'XSRDA8'
073700070716     c                   parm                    wlbdat
073800070716     c                   z-add     g08inv        dateu
073900070716
074000070716      * controllo se la filiale è partita
074100070716     c                   clear                   og147
074200070717     c                   clear                   og148
074300070716     c                   clear                   comddao
074400070716     c     or61fgsi      chain     azorg01l
074500070323     c                   if        %found(azorg01l)
074600070716     c                   eval      og147 = orgde7
074700070717     c                   eval      og148 = orgde8
074800080124      * se filiale non partita esco (partita anche solo con fase 1)
074900080124     c                   if        §ogcgio = *blanks
075000070716     c                   eval      *inlr = *on
075100070323     c                   return
075200070716     c                   endif
075300070717
075400080124     c                   endif
075500070716
075600070716      * dati utente
075700030718     c     *dtaara       define    §azute        azuteds
075800030718     c     *dtaara       define    §datiute      ddatiute
075900070716     c                   if        %error or rsut = *blanks
076000070716     c                   clear                   tibs34ds
076100070716     c                   call      'TIBS34R'
076200070716     c                   parm                    tibs34ds
076300070716     c                   in        *dtaara
076400070716     c                   endif
076500070716
076600070717      *  klist
076700070717     c     korm          klist
076800070716     c                   kfld                    orgpoe
076900070716     c                   kfld                    orgnsr
077000070716     c                   kfld                    orgnor
077100070716     c                   kfld                    orgnrv
077200070716
077300070717     c     korm1         klist
077400070717     c                   kfld                    ormpoe
077500070717     c                   kfld                    ormnsr
077600070717     c                   kfld                    ormnor
077700070717     c                   kfld                    ormnrv
077800070717     c                   kfld                    ormdfo
077900070717     c                   kfld                    ormofo
078000070717     c                   kfld                    ormfao
078100080616
078200080616     c     kfipro        klist
078300080616     c                   kfld                    or61fgsi
078400080616     c                   kfld                    wndc
078500080730     c                   kfld                    dstpdr
078600080616     c                   kfld                    ormpoe
078700080616     c                   kfld                    ormnsr
078800080616     c                   kfld                    ormnor
078900080616     c                   kfld                    ormnrv
079000080730
079100080730     c     kfidst        klist
079200080730     c                   kfld                    dstnpg
079300080730     c                   kfld                    wndc
079400080730     c                   kfld                    or61fgsi
079500070717
079600070716     c                   endsr
079700070716** msg
079800070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
