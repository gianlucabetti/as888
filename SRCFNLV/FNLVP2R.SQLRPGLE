000100110324     /*PRM tgtrls(*current)
000200110324     /*END
000300110119     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000400110119     h dftactgrp(*no) actgrp(*new)
000500110119     h BndDir('UBBNDDIR')
000600070209      *------------------------------------------------------------------------*
000700070209      *
000800070504      *                     INTERROGAZIONE LOG PDA  ?
000900070209      *
001000070209      *------------------------------------------------------------------------*
001100070209
001200070209     fazorg01l  if   e           k disk
001300110119     ftabel00f  if   e           k disk
001400140217     fTNTBE01L  if   e           k disk
001500070504     ffiapd01l  if   e           k disk
001600110119     ffidst01l  if   e           k disk    extfile(wLibFIDST)  usropn
001700110324     f                                     extdesc('FILTRAPRD/FIDST01L')
001800070508     ffnlvp2d   cf   e             workstn sfile(lvp2s01:nrr)
001900070209
002000070209      *------------------------------------------------------------------------*
002100070216      * ? RIEPILOGO INDICATORI ?
002200070209      *------------------------------------------------------------------------*
002300070508      * 20 - gestione subfile
002400070508      * 21 - gestione subfile
002500070508      * 28 - errore generico
002600070508      * 30 - comodo
002700070504      * 31 - fine file x subfile
002800070504      * 32 - fine subfile
002900070504      * 40 - errore data ricezione
003000070504      * 41 - errore ora ricezione
003100070504      * 42 - errore filiale distinta
003200070504      * 43 - errore numero distinta
003300070504      * 44 - errore autotrasportatore
003400070514      * 45 - errore tipo record
003500080328      * 46 - errore id documento
003600070508      * 50 - tipo record OK
003700070508      * 51 - tipo record RES
003800070508      * 52 - tipo record RCH
003900070514      * 53 - altro tipo record
004000070508      * 60 - id documento ORM
004100070514      * 62 - ORM manuale
004200070514      * 73 - posizionamento cursore subfile
004300070209      *------------------------------------------------------------------------*
004400110308
004500110308      * ? C O S T A N T I ?
004600110324       // -?Attributi di visualizzazione campi di output a video?
004700110308     d c_ColorGrn      c                   const(x'20')
004800110308     d c_ColorWht      c                   const(x'22')
004900110308     d c_ColorTrq      c                   const(x'30')
005000110308     d c_ColorBlu      c                   const(x'3A')
005100110324
005200110324       // -?Numero massimo di record gestibili nel subfile?
005300110324     d c_MaxRec        c                   const(9999)
005400070209
005500070216      * ? V A R I A B I L I ?
005600081117     d dataiso         s               d   datfmt(*iso)
005700070508     d nrr             s                   like(recsf)
005800070214     d stringasql      s           4096    varying
005900070508     d v1cdatainv      s              8    inz
006000070507     d wconta          s              2  0 inz
006100070508     d wfgs            s              3
006200070508     d wora            s              6
006300070504     d w0030           s              3  0
006400081117     d xx              s              3  0
006500110119       // -?Nome specifico libreria/file del file di filiale?
006600110119     d wLibFIDST       s             21a   inz
006700070209
006800070216      * ? S C H I E R E ?
006900080327     d msg             s             78    dim(20) ctdata perrcd(1)
007000081117     d $fv6            s              6p 0 dim(600)
007100070209
007200070216      * ? D S   I N T E R N E / E S T E R N E ?
007300070514     d idbolds         ds
007400070514     d v3aas                   1      4
007500070514     d v3lnp                   5      7
007600070514     d v3nrs                   8      9
007700070514     d v3nsp                  10     16
007800070511
007900070514     d idormds         ds
008000070514     d v3poe                   1      3
008100070514     d v3nsr                   4      5
008200070514     d v3nor                   6     12
008300070514     d v3nrv                  13     14
008400070220
008500070220     d wdata           ds
008600070504     d  wgg                    1      2  0
008700070504     d  wmm                    3      4  0
008800070504     d  waa                    5      8  0
008900070504
009000070504     d wdatainv        ds
009100070508     d  waai                   1      4  0
009200070508     d  wmmi                   5      6  0
009300070508     d  wggi                   7      8  0
009400070511
009500070511     d wlbdat          ds                  inz
009600070511     d  g08dat                 1      8  0
009700070511     d  g08inv                 9     16  0
009800070511     d  g08err                17     17
009900070511     d  g08tgi                18     22  0
010000070209
010100070209     d azuteds       e ds                  extname(azute00f)
010200070508     d dcmr          e ds
010300070209     d ddatiute      e ds
010400070508     d dprc          e ds
010500070531     d ds5a2         e ds
010600070504     d fiplgds       e ds                  extname(fiplg00f)
010700080331     d fnlvp2ds      e ds
010800070215     d fiprcokds     e ds
010900140212     d fiprcrords    e ds
011000070504     d fiprcrchds    e ds
011100070504     d fiprcresds    e ds
011200070209     d fnlv24ds      e ds
011300081117     d fidg31ds      e ds
011400070209     d kpjba         e ds
011500070209     d og148         e ds
011600070508     d tibs02ds      e ds
011700070209     d tibs34ds      e ds                  inz
011800070504     d trul31ds      e ds                  inz
011900070504     d  pog                   10    759  0 dim(250)
012000070209
012100070209     d                sds
012200070209     d  vtcpgm                 1     10
012300110119
012400110119      * ? P R O C E D U R E   U S A T E ?
012500110119       // -?Reperimento NETA sistema AS/400 corrente?
012600110119     d currSysNeta     s              8a   inz
012700110119      /copy gaitrasrc/srcProtoPr,UBRTVNETA
012800110308
012900110308      * ? D E F I N I Z I O N E   K E Y - L I S T ?
013000110308       // -?File TNTBE01L?
013100110308     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
013200110308     d                                     prefix(k_)   inz
013300070209
013400070209      *------------------------------------------------------------------------*
013500080130
013600080130     c/exec sql
013700080130     c+ set option dynusrprf = *owner, closqlcsr = *endmod
013800080130     c/end-exec
013900070209
014000070216      *? prima videata ?
014100070216     c     emid01        tag
014200070216      * pulisco la videata
014300070209     c                   if        not *in28 and not *in90
014400070216     c                   exsr      sr_puld01
014500070209     c                   endif
014600080331      * emetto la videata solo se non sono richiamato
014700140328     c     emid02        tag
014800110311     c                   if        Richiamato = *blanks
014900070504     c                   write     lvp2t01
015000070504     c                   exfmt     lvp2d01
015100110311     c                   endif
015200070216      * spengo indicatori di comodo
015300070209     c                   eval      *in28 = *off
015400070209     c                   eval      *in90 = *off
015500070216      * pulisco campo messaggi
015600070216     c                   clear                   v1cmsg
015700070214      * f3=fine
015800070209     c                   if        *inkc
015900070216     c                   goto      fine
016000070209     c                   endif
016100140328      * f5=ripristino video
016200140328     c                   if        *inke
016300140328     c                   goto      emid01
016400140328     c                   endif
016500070215      * controllo i dati della videata
016600070219     c                   exsr      sr_contrd01
016700080331     c* se richiamato
016800110311     c                   IF        Richiamato <> *blanks
016900110311     c                   If        *in28 or *in90 or *inkl
017000080331     c                   if        *in28 or *in90
017100080331     c                   eval      ds2esito = 'E'
017200110311     c                   endif
017300080331     c                   if        *inkl
017400080331     c                   eval      ds2esito = '2'
017500110311     c                   endif
017600080331     c                   goto      fine
017700110311     c                   EndIf
017800110311     c                   ENDIF
017900080331     c*
018000070209     c                   if        *in28 or *in90
018100070216     c                   goto      emid01
018200070209     c                   endif
018300070504
018400070504      * con sql elaboro il file in base a quanto richiesto in prima
018500080331      * videata o passato dal chiamante nella ds FNLVP2DS
018600070504     c                   exsr      sr_carsql
018700070216
018800070504      *? subfile ?
018900070504     c                   exsr      sr_pulsfl
019000070504     c                   exsr      sr_carsfl
019100070507      * se errore nel carico dati da sql ritorno alla prima videata con msg
019200070507      * di errore
019300070507     c                   if        *in28
019400070507     c                   goto      emid01
019500070507     c                   endif
019600070511
019700070511     c     emisfl        tag
019800070223      * emetto videata vuota se non ho caricato niente
019900070508     c                   if        nrr = *zeros
020000070223     c                   exsr      sr_emid02
020100070223     c                   else
020200070504     c                   exsr      sr_emisfl
020300070223     c                   endif
020400070214
020500070216      *? fine programma ?
020600070216     c     fine          tag
020700070216
020800080331     c* se richiamato passo l'esito
020900110311     c                   If        Richiamato <> *blanks
021000080520     c                   eval      ds2esito = esitol
021100080331     c                   if        *inkc
021200080331     c                   eval      ds2esito = '1'
021300110311     c                   endif
021400080331     c                   movel(p)  fnlvp2ds      kpjbu
021500110311     c                   EndIf
021600080331     c*
021700070214     c                   eval      *inlr = *on
021800070209
021900070209      *------------------------------------------------------------------------*
022000070216      *? PULIZIA DELLA PRIMA VIDEATA  ?
022100070209      *------------------------------------------------------------------------*
022200070216     c     sr_puld01     begsr
022300070209
022400070508     c                   clear                   v1cdata
022500070508     c                   clear                   v1cdatainv
022600070504     c                   clear                   v1cora
022700070514     c                   clear                   v1ctiporcd
022800070514     c                   clear                   v1dtiporcd
022900070508     c                   clear                   v1cfgs
023000070508     c                   clear                   v1dfgs
023100070504     c                   clear                   v1cndc
023200070504     c                   clear                   v1dndc
023300070508     c                   clear                   v1ccodaut
023400070209     c                   clear                   v1dpdr
023500070504     c                   clear                   v1cerr
023600080331     c                   clear                   v1ciddoc
023700110311     c                   clear                   V1CidDisp
023800080331     c* se richiamato imposto i dati nella prima videata e non emetto il
023900080331     c* video di richiesta parametri ma carico direttamente il sfl
024000110311     c                   if        Richiamato <> *blanks
024100080331     c                   eval      v1ctiporcd = ds2tiporcd
024200080331     c                   eval      v1cfgs = ds2fgs
024300080331     c                   eval      v1cndc = ds2ndc
024400080331     c                   eval      v1ccodaut = ds2codaut
024500080331     c                   eval      v1ciddoc = ds2iddoc
024600110311     c                   endif
024700070209
024800070209     c                   endsr
024900070214
025000070209      *------------------------------------------------------------------------*
025100070216      *? CONTROLLO I DATI DELLA PRIMA VIDEATA   ?
025200070209      *------------------------------------------------------------------------*
025300070209     c     sr_contrd01   begsr
025400070214
025500070504      * indicatori di posizionamento cursore
025600070504     c                   eval      *in40 = *off
025700070504     c                   eval      *in41 = *off
025800070504     c                   eval      *in42 = *off
025900070504     c                   eval      *in43 = *off
026000070504     c                   eval      *in44 = *off
026100070514     c                   eval      *in45 = *off
026200080328     c                   eval      *in46 = *off
026300070504
026400070504      * almeno una scelta è da fare
026500070510     c                   if        v1cdata = *zeros and v1cora = *zeros and
026600110311     c                             V1CtipoRcd = *blanks  and
026700110311     c                             V1CidDisp  = *blanks  and
026800110311     c                             V1CidDoc   = *blanks  and
026900080328     c                             v1cfgs = *zeros and v1cndc = *blanks and
027000070510     c                             v1ccodaut = *blanks and v1cerr = *blanks
027100070504     c                   eval      *in28 = *on
027200070504     c                   eval      *in40 = *on
027300070504     c                   eval      v1cmsg = msg(03)
027400070504     c                   leavesr
027500070504     c                   endif
027600070504
027700070504      *? DATA RICEZIONE
027800070508     c                   if        v1cdata > *zeros
027900070511     c                   z-add     v1cdata       g08dat
028000070511     c                   call      'XSRDA8'
028100070511     c                   parm                    wlbdat
028200070511     c                   if        g08err = '1'
028300070511     c                   eval      *in40 = *on
028400070511     c                   eval      *in28 = *on
028500070511     c                   eval      v1cmsg = msg(04)
028600070511     c                   leavesr
028700070511     c                   endif
028800070511     c                   z-add     g08dat        v1cdata
028900070511     c                   move      g08inv        v1cdatainv
029000070504     c                   endif
029100070504
029200070504      *? ORA RICEZIONE
029300070504     c                   if        v1cora > *zeros
029400070504     c     *eur          test(te)                v1cora
029500070504     c                   if        %error
029600070504     c                   eval      *in41 = *on
029700070504     c                   eval      *in28 = *on
029800070504     c                   eval      v1cmsg = msg(05)
029900070504     c                   leavesr
030000070504     c                   endif
030100070504     c                   endif
030200070514
030300070514      *? TIPO RECORD
030400080331     c                   clear                   dprc
030500110307if  1c                   if        v1ctiporcd <> *blanks
030600070514     c                   clear                   v1dtiporcd
030700070514      *  ricerca
030800110307if  2c                   if        %scan('?':v1ctiporcd) > *zeros
030900070514     c                   eval      *in45 = *on
031000070514     c                   eval      *in90 = *on
031100070514     c                   clear                   tibs02ds
031200070514     c                   eval      t02mod = 'R'
031300070514     c                   eval      t02sif = knsif
031400070514     c                   eval      t02cod = 'PRC'
031500070514     c                   call      'TIBS02R'
031600070514     c                   parm                    kpjba
031700070514     c                   parm                    tibs02ds
031800070514     c                   eval      v1ctiporcd = t02ke1
031900070514     c                   eval      dprc = t02uni
032000070514     c                   eval      v1dtiporcd = d§prcdes
032100070514     c                   leavesr
032200110307e   2c                   endif
032300070514      * controllo
032400070514     c                   clear                   tibs02ds
032500070514     c                   eval      t02mod = 'C'
032600070514     c                   eval      t02sif = knsif
032700070514     c                   eval      t02cod = 'PRC'
032800070514     c                   eval      t02ke1 = v1ctiporcd
032900070514     c                   call      'TIBS02R'
033000070514     c                   parm                    kpjba
033100070514     c                   parm                    tibs02ds
033200110307if  2c                   if        t02err <> *blanks
033300070514     c                   eval      *in28 = *on
033400070514     c                   eval      *in45 = *on
033500070514     c                   eval      v1cmsg = msg(13)
033600070514     c                   leavesr
033700110307e   2c                   endif
033800070514     c                   eval      v1ctiporcd = t02ke1
033900070514     c                   eval      dprc = t02uni
034000070514     c                   eval      v1dtiporcd = d§prcdes
034100110307e   1c                   endif
034200110307
034300080328      *? id documento (non faccio controllo esistenza bolla xchè non è
034400080328      *? detto che si voglia ricercare una spedizione esistente)
034500110307     c                   if        v1ciddoc   <> *blanks  and
034600110307     c                             V1CtipoRcd <> *blanks  and
034700080328     c                             (d§prcipd = *blanks or d§prcipd ='000')
034800080328     c                   eval      *in28 = *on
034900080328     c                   eval      *in46 = *on
035000080328     c                   eval      v1cmsg = msg(15)
035100080328     c                   leavesr
035200080328     c                   endif
035300070504
035400070504      *? FILIALE DISTINTA
035500070508     c                   if        v1cfgs > *zeros
035600070508     c                   clear                   v1dfgs
035700070504     c                   clear                   og148
035800070504      *  valida
035900110308     c     v1cfgs        chain     azorg
036000070504     c                   if        not %found(azorg01l) or orgfva <> *blanks
036100070504     c                   eval      *in28 = *on
036200070504     c                   eval      *in42 = *on
036300070504     c                   eval      v1cmsg = msg(06)
036400070504     c                   leavesr
036500070504     c                   endif
036600070504     c                   eval      v1dfgs = orgdes
036700070504     c                   eval      og148 = orgde8
036800070504      *  PDA attivo
036900080213     c                   if        §ogpdaorm = *blanks
037000070504     c                   eval      *in28 = *on
037100070504     c                   eval      *in42 = *on
037200070504     c                   eval      v1cmsg = msg(02)
037300070504     c                   leavesr
037400070504     c                   endif
037500070504      *  deve essere una filiale gestita dall'utente
037600070508     c     v1cfgs        lookup    pog                                    30
037700070504     c                   if        not *in30
037800070504     c                   eval      *in28 = *on
037900070504     c                   eval      *in42 = *on
038000070504     c                   eval      v1cmsg = msg(07)
038100070504     c                   leavesr
038200070504     c                   endif
038300070504     c                   endif
038400070504
038500070504      *? DISTINTA
038600070504     c                   if        v1cndc <> *blanks
038700070511     c                   if        v1cfgs = *zeros
038800070511     c                   eval      *in28 = *on
038900070511     c                   eval      *in42 = *on
039000070511     c                   eval      v1cmsg = msg(06)
039100070511     c                   leavesr
039200070511     c                   endif
039300070504     c                   clear                   v1dndc
039400070504      *  ricerca
039500070504     c                   if        %scan('?':v1cndc) > *zeros
039600081117     c                   clear                   v1cndc
039700081117     c                   clear                   fidg31ds
039800070504     c                   eval      *in43 = *on
039900070504     c                   eval      *in90 = *on
040000081117     c                   eval      fdgfgs = v1cfgs
040100081117     c                   move      dataiso       fdggda
040200081117     c                   eval      fdggal = *date
040300081117     c                   eval      fdgtfv = '2'
040400081117     c                   eval      fdgnpg = 4
040500081117     c                   eval      fdgric = 'S'
040600081117     c                   eval      fdgtpv = 'T'
040700081117     c                   clear                   $fv6
040800081117      * impostato a tutti 99999
040900081117     c                   do        600           xx
041000081117     c                   z-add     999999        $fv6(xx)
041100081117     c                   enddo
041200081117     c                   eval      fdgtpop = 'R'
041300081117     c                   eval      kpjbu = fidg31ds
041400081117     c                   call      'FIDG31R'
041500081117     c                   parm                    kpjba
041600081117     c                   parm                    $fv6
041700081117     c                   eval      fidg31ds = kpjbu
041800081117      * se torno dal pgm senza aver selezionato niente pulisco i dati della DS
041900081117     c                   if        fdgnfv = *zeros
042000081117     c                   clear                   fidg31ds
042100081117     c                   else
042200081117     c                   eval      v1cndc = *all'0'
042300081117     c                   move      fdgnfv        v1cndc
042400081117     c                   endif
042500081117     c                   if        v1cndc = *zeros
042600081117     c                   clear                   v1cndc
042700081117     c                   leavesr
042800081117     c                   endif
042900081117     c                   endif
043000070504      *  valida
043100070504     c                   eval      dstnpg = 4
043200070508     c                   eval      dstfgs = v1cfgs
043300070504     c                   move      v1cndc        dstnfv
043400110308     c     kfidst        chain     fidst000
043500070504     c                   if        not %found(fidst01l)
043600070504     c                   eval      *in28 = *on
043700070504     c                   eval      *in43 = *on
043800070504     c                   eval      v1cmsg = msg(08)
043900070504     c                   leavesr
044000070504     c                   endif
044100070504      *  abilitata allo scarico PDA
044200080424     c                   if        dstpda ='N'
044300070504     c                   eval      *in28 = *on
044400070504     c                   eval      *in43 = *on
044500070504     c                   eval      v1cmsg = msg(09)
044600070504     c                   leavesr
044700070504     c                   endif
044800070504      *  decodifico
044900070504     c                   eval      apdtip = 'A'
045000070504     c                   eval      apdpdr = dstpdr
045100070504     c                   clear                   apdrsc
045200110308     c     kfiapd        chain     fiapd000
045300070504     c                   if        %found(fiapd01l)
045400110308     c                   eval      v1dndc = 'Del' +
045500110308     c                             c_ColorTrq +
045600070504     c                             %subst(%editc(%dec(dstdfv):'X'):7:2) + '/' +
045700070504     c                             %subst(%editc(%dec(dstdfv):'X'):5:2) + '/' +
045800070504     c                             %subst(%editc(%dec(dstdfv):'X'):1:4) + ' ' +
045900110308     c                             %editc(%dec(dstpdr):'X') +
046000110308     c                             c_ColorBlu +
046100070504     c                             %subst(apdrsc:1:17)
046200070504     c                   endif
046300070504     c                   endif
046400070504
046500070504      *? AUTOSTRASPORTATORE
046600070508     c                   if        v1ccodaut <> *blanks
046700070504      *  pulisco il campo se tutti zeri
046800070508     c                   if        v1ccodaut = *all'0'
046900070508     c                   clear                   v1ccodaut
047000070504     c                   endif
047100070504      *  ricerca
047200070508     c                   if        %scan('?':v1ccodaut) > *zeros
047300070514     c                   eval      *in44 = *on
047400070504     c                   eval      *in90 = *on
047500070504     c                   clear                   fnlv24ds
047600070504     c                   eval      d24fil = dutpou
047700070504     c                   eval      d24tip = 'A'
047800070504     c                   eval      d24flg = 'R'
047900070504     c                   eval      kpjbu = fnlv24ds
048000070504     c                   call      'FNLV24R'
048100070504     c                   parm                    kpjba
048200070504      *  è stato scelto un codice lo imposto a video
048300070504     c                   eval      fnlv24ds = kpjbu
048400070504     c                   if        d24pdr <> *zeros
048500070508     c                   move      d24pdr        v1ccodaut
048600070504     c                   eval      v1dpdr = d24rsc
048700070504     c                   leavesr
048800070504     c                   endif
048900070504      *  non è stato scelto nessun codice pulisco i campi a video
049000070508     c                   clear                   v1ccodaut
049100070504     c                   clear                   v1dpdr
049200070504     c                   leavesr
049300070504     c                   endif
049400070504      *  deve essere numerico
049500070504     c                   eval      *in30 = *off
049600070508     c                   testn                   v1ccodaut            30
049700070504      *  controllo validità
049800070508     c                   if        *in30 and %subst(v1ccodaut:7:1) >= *zeros
049900070504     c                   eval      apdtip = 'A'
050000070508     c                   move      v1ccodaut     apdpdr
050100110308     c     kfiapd        chain     fiapd000
050200070504     c                   if        not %found(fiapd01l) or apdatb <> *blanks
050300070504     c                   eval      *in28 = *on
050400070504     c                   eval      *in45 = *on
050500070504     c                   eval      v1cmsg = msg(10)
050600070504     c                   leavesr
050700070504     c                   endif
050800070504     c                   eval      v1dpdr = apdrsc
050900070504     c                   endif
051000070504      *  deve essere in gestione all'utente
051100070504     c                   movel     apdpdr        w0030
051200070508     c     w0030         lookup    pog                                    30
051300070504     c                   if        not *in30
051400070504     c                   eval      *in28 = *on
051500070504     c                   eval      *in45 = *on
051600070504     c                   eval      v1cmsg = msg(11)
051700070504     c                   leavesr
051800070504     c                   endif
051900070215     c                   endif
052000070209
052100070209     c                   endsr
052200070504
052300070504      *------------------------------------------------------------------------*
052400070504      *? CARICO SQL
052500070504      *------------------------------------------------------------------------*
052600070504     c     sr_carsql     begsr
052700070507
052800070507     c                   clear                   wconta
052900070507
053000080327     c                   eval      stringasql = 'select fiplg00f.*, ' +
053100080327     c                                          'rrn(fiplg00f) ' +
053200110308     c                                          'from fiplg00f where'
053300070510      * selezionata la data
053400070508     c                   if        v1cdata <> *zeros
053500110308     c                   eval      stringasql += ' substr(plgdatora, 1, 8) +
053600110308     c                                           = ''' +
053700110308     c                                           v1cdatainv + ''''
053800070507     c                   eval      wconta = 1
053900070507     c                   endif
054000070507
054100070510      * selezionata l'ora
054200070507     c                   if        v1cora <> *zeros
054300070508     c                   if        wconta > *zeros
054400110308     c                   eval      stringasql += ' and'
054500070508     c                   endif
054600070508     c                   move      v1cora        wora
054700110308     c                   eval      stringasql += ' substr(plgdatora, 9, 6) +
054800110308     c                                           >= ' + wora
054900070507     c                   eval      wconta = 1
055000070507     c                   endif
055100070514
055200070514      * selezionato il tipo record
055300070514     c                   if        v1ctiporcd <> *blanks
055400070514     c                   if        wconta > *zeros
055500110308     c                   eval      stringasql += ' and'
055600070514     c                   endif
055700110308     c                   eval      stringasql += ' plgtiporcd = ''' +
055800110308     c                                           v1ctiporcd + ''''
055900070514     c                   eval      wconta = 1
056000070514     c                   endif
056100070507
056200070510      * selezionata la filiale
056300070508     c                   if        v1cfgs <> *zeros
056400070508     c                   if        wconta > *zeros
056500110308     c                   eval      stringasql += ' and'
056600070508     c                   endif
056700070508     c                   move      v1cfgs        wfgs
056800110308     c                   eval      stringasql += ' plgfgs = ' + wfgs
056900070507     c                   eval      wconta = 1
057000070507     c                   endif
057100070507
057200070510      * selezionata la distinta
057300070507     c                   if        v1cndc <> *blanks
057400070508     c                   if        wconta > *zeros
057500110308     c                   eval      stringasql += ' and'
057600070508     c                   endif
057700110308     c                   eval      stringasql += ' digits(plgndc) = ' +
057800110308     c                                           v1cndc
057900070507     c                   eval      wconta = 1
058000070507     c                   endif
058100070507
058200070510      * selezionato l'autotrasportatore
058300070508     c                   if        v1ccodaut <> *blanks
058400070508     c                   if        wconta > *zeros
058500110308     c                   eval      stringasql += ' and'
058600070508     c                   endif
058700110308     c                   eval      stringasql += ' digits(plgcodaut) = ' +
058800070508     c                                           v1ccodaut
058900070507     c                   eval      wconta = 1
059000070507     c                   endif
059100070507
059200070510      * selezionato solo errori
059300070507     c                   if        v1cerr = 'S'
059400070508     c                   if        wconta > *zeros
059500110308     c                   eval      stringasql += ' and'
059600070508     c                   endif
059700110308     c                   eval      stringasql += ' plgerr <> '' '''
059800110308     c                   eval      wconta = 1
059900070507     c                   endif
060000110311
060100110311      /free
060200110311
060300110311         // -?Selezionato Id Dispositivo?
060400110311         if  V1CidDisp <> *blank;
060500110311           if  wConta > *zero;
060600110311             StringaSql += ' and';
060700110311           endif;
060800170718            // -?Se Id Dispositivo? per nuovo PDA cellulare (*)
060900170718            //  i records ID Dispositivo sono vuoti sul FIPLG
061000170718            if  V1CidDisp = '*';
061100170718           StringaSql += ' plgIdDisp = ''  '' ';
061200170907            elseif V1CidDisp = '#';
061300170907           StringaSql += ' SUBSTR(plgIdDisp, 1, 3) = ''#G#'' ';
061400170907            else;
061500110311           StringaSql += ' plgIdDisp = ''' + V1CidDisp + '''';
061600170718            endif;
061700110311         endif;
061800110311
061900110311      /end-free
062000110307
062100080328      * selezionato id documento
062200110307if  1c                   if        v1ciddoc   <> *blanks
062300110307if  Ac                   IF        V1CtipoRcd <> *blanks
062400080328     c                   clear                   pini
062500080328     c                   monitor
062600080328     c                   move      d§prcipd      pini              3 0
062700080328     c                   on-error
062800080328     c                   endmon
062900080328     c                   clear                   lun
063000080328     c                   monitor
063100080328     c                   move      d§prcld       lun               2 0
063200080328     c                   on-error
063300080328     c                   endmon
063400110307if  2c                   if        pini <> 0  and lun <> 0
063500110307if  3c                   if        wconta > *zeros
063600110308     c                   eval      stringasql += ' and'
063700110307e   3c                   endif
063800110308     c                   eval      stringasql += ' substr(plgdati, ' +
063900110308     c                                           d§prcipd + ', ' +
064000110308     c                                           d§prcld + ') = ''' +
064100110308     c                                           v1ciddoc + ''''
064200110307e   2c                   endif
064300110307x   Ac                   ELSE
064400110308     c                   exsr      sr_SelectIdDoc
064500110307e   Ac                   ENDIF
064600110308     c                   eval      wConta = 1
064700110307e   1c                   endif
064800070507
064900110308     c                   eval      stringasql += ' order by plgdatora +
065000110308     c                                             for read only'
065100070507
065200070507      * preparo il cursore
065300070507     c/exec sql
065400070507     c+ prepare s1 from :stringasql
065500070507     c/end-exec
065600070507
065700070507      * dichiaro il cursore
065800070507     c/exec sql
065900070507     c+ declare plg cursor for s1
066000070507     c/end-exec
066100070504
066200070504     c                   endsr
066300110308      /free
066400110308
066500110308       //--------------------------------------------------------------
066600110308       //?Selezione (generica) per "ID documento"                      ?
066700110308       //--------------------------------------------------------------
066800110308       BEGSR  sr_SelectIdDoc;
066900110308
067000110308         if  wConta > *zero;
067100110308           StringaSql += ' and';
067200110308         endif;
067300110308
067400110308         clear  xx;
067500110308         clear  k05tntbe01;
067600110308         k_TBEcod = 'PRC';
067700110308         setll  %kds(k05tntbe01 : 1)  TNTBE000;
067800110308         reade  %kds(k05tntbe01 : 1)  TNTBE000;
067900110308
068000110308         DoW  Not %eof(TNTBE01L);
068100110308
068200110308           dPRC = TBEuni;
068300110308
068400110308           clear  PIni;
068500110308           clear  Lun;
068600110308           monitor;
068700110308             PIni = %int(d§PRCipd);
068800110308           on-error;
068900110308           endmon;
069000110308           monitor;
069100110308             Lun  = %int(d§PRCld);
069200110308           on-error;
069300110308           endmon;
069400110308
069500110308           if  PIni <> *zero  and  Lun <> *zero;
069600110308
069700110308             if  xx = *zero;
069800110308               StringaSql += ' (';
069900110308             else;
070000110308               StringaSql += ' or';
070100110308             endif;
070200110308
070300110308             StringaSql += ' (PLGtipoRcd = ''' + %trimr(TBEke1) +
070400110308                           ''' and substr(PLGdati, ' + d§PRCipd +
070500110308                                                ', ' + d§PRCld +
070600110308                           ') = ''' + V1CidDoc + ''')';
070700110308
070800110308             xx += 1;
070900110308
071000110308           endif;
071100110308
071200110308           reade  %kds(k05tntbe01 : 1)  TNTBE000;
071300110308
071400110308         EndDo;
071500110308
071600110309         if  xx > *zero;
071700110309           StringaSql += ' )';
071800110309         endif;
071900110308
072000110308       ENDSR;
072100110308
072200110308      /end-free
072300070215
072400070215      *------------------------------------------------------------------------*
072500070507      *? PULISCO IL SUBFILE
072600070215      *------------------------------------------------------------------------*
072700070508     c     sr_pulsfl     begsr
072800070215
072900070504     c                   clear                   nrr
073000070216
073100070216     c                   eval      *in20 = *off
073200070216     c                   eval      *in21 = *off
073300070504     c                   write     lvp2c01
073400070216     c                   eval      *in21 = *on
073500070216     c                   eval      *in20 = *on
073600070216
073700070504     c                   eval      recsf = 1
073800070216     c                   eval      *in31 = *off
073900070215
074000070215     c                   endsr
074100070216
074200070216      *------------------------------------------------------------------------*
074300070507      *? CARICO IL SUBFILE --> LEGGO SQL
074400070216      *------------------------------------------------------------------------*
074500070504     c     sr_carsfl     begsr
074600070216
074700070507      * apertura cursore
074800070507     c/exec sql
074900070507     c+ open plg
075000070507     c/end-exec
075100070507
075200070507      * lettura cursore
075300070507     c                   do        *hival
075400070507     c/exec sql
075500080327     c+ fetch next from plg into :fiplgds, :nrrplg
075600070507     c/end-exec
075700070507     c                   select
075800070507      * fine file
075900070507     c                   when      sqlcod = 100
076000070507     c                   leave
076100070507      * errore
076200070507     c                   when      sqlcod < 0
076300070507     c                   eval      *in28 = *on
076400070507     c                   eval      v1cmsg = msg(12)
076500070507     c                   leave
076600070507     c                   other
076700110324     c                   if        nrr < c_MaxRec
076800070508     c                   exsr      sr_wrtsfl
076900110324     c                   else
077000110324     c                   eval      *in28 = *on
077100110324     c                   eval      v1cmsg = msg(16)
077200110324     c                   leave
077300110324     c                   endif
077400070507     c                   endsl
077500070507     c                   enddo
077600110324     c                   if        sqlcod = 100
077700110324     c                   eval      *in31 = *on
077800110324     c                   endif
077900070508
078000070507      * chisura cursore
078100070507     c/exec sql
078200070508     c+ close plg
078300070507     c/end-exec
078400070507
078500070507     c                   endsr
078600070508
078700070508      *------------------------------------------------------------------------*
078800070508      *? SCRIVO LA RIGA DEL SUBFILE A TOTALE
078900070508      *------------------------------------------------------------------------*
079000070508     c     sr_wrtsfl     begsr
079100070219
079200070508     c                   clear                   v1sopz
079300070514     c                   eval      v1stiporcd = plgtiporcd
079400070514     c                   eval      v1stipappl= plgtipappl
079500070511     c                   if        plgdatora <> *blanks
079600070508     c                   eval      wdatainv = %subst(plgdatora:1:8)
079700070508     c                   move      waai          waa
079800070508     c                   move      wmmi          wmm
079900070508     c                   move      wggi          wgg
080000070508     c                   move      wdata         v1sdata
080100070508     c                   eval      v1sora = %dec(%subst(plgdatora:9:6):6:0)
080200070511     c                   else
080300070511     c                   clear                   v1sdata
080400070511     c                   clear                   v1sora
080500070511     c                   endif
080600070508     c                   eval      v1scodaut = plgcodaut
080700070508     c                   eval      v1sfgs = plgfgs
080800070508     c                   eval      v1sndc = plgndc
080900070508     c                   eval      v1siddoc = plgiddoc
081000070508     c                   eval      v1serr = plgerr
081100070220      * imposto i campi hidden
081200070508     c                   eval      v1sprfc = plgprfc
081300070508     c                   eval      v1siddisp = plgiddisp
081400070508     c                   eval      v1sdati= plgdati
081500070508     c                   eval      v1smsg= plgmsg
081600070219      * scrivo il subfile a totale
081700070508     c                   add       1             nrr
081800070223      * se primo rcd che scrivo accendo indicatore di PC
081900070508     c                   if        nrr = 1
082000070514     c                   eval      *in73 = *on
082100070223     c                   else
082200070514     c                   eval      *in73 = *off
082300070223     c                   endif
082400070223
082500070508     c                   write     lvp2s01
082600070219
082700070219     c                   endsr
082800070223
082900070223      *------------------------------------------------------------------------*
083000070223      *? EMETTO LA VIDEATA VUOTA
083100070223      *------------------------------------------------------------------------*
083200070223     c     sr_emid02     begsr
083300070223
083400070223     c                   do        *hival
083500070223
083600070508     c                   write     lvp2t01
083700070508     c                   exfmt     lvp2d02
083800070223
083900070223     c                   eval      *in28 = *off
084000070223
084100070223      * f3=fine
084200070223     c                   if        *inkc
084300070223     c                   goto      fine
084400070223     c                   endif
084500070223
084600070223      * f12=ritorno
084700070223     c                   if        *inkl
084800140328     c                   goto      emid02
084900070223     c                   endif
085000070223
085100070223     c                   enddo
085200070223
085300070223     c                   endsr
085400070216
085500070216      *------------------------------------------------------------------------*
085600070216      *? EMETTO IL SUBFILE A TOTALE
085700070216      *------------------------------------------------------------------------*
085800070508     c     sr_emisfl     begsr
085900070219
086000070219     c                   do        *hival
086100070219
086200070508     c                   if        recsf > nrr
086300070508     c                   eval      recsf = 1
086400070216     c                   endif
086500070216
086600070508     c                   write     lvp2t01
086700070508     c                   write     lvp2z01
086800070508     c                   exfmt     lvp2c01
086900070216
087000070216     c                   eval      *in28 = *off
087100070508     c                   clear                   v1cmsg
087200070216
087300070216      * f3=fine
087400070216     c                   if        *inkc
087500070216     c                   goto      fine
087600070216     c                   endif
087700070216
087800070216      * f12=ritorno
087900070216     c                   if        *inkl
088000140328     c                   goto      emid02
088100070216     c                   endif
088200070216
088300070216      * controllo i dati immessi nel subfile
088400070219      * solo se ho caricato il subfile
088500070508     c                   if        nrr > *zeros
088600070508     c                   exsr      sr_contrsfl
088700070216     c                   if        *in28
088800070216     c                   iter
088900070216     c                   endif
089000070219     c                   endif
089100070216
089200070216     c                   enddo
089300070216
089400070216     c                   endsr
089500070216
089600070216      *------------------------------------------------------------------------*
089700070508      *? CONTROLLA I DATI DEL SUBFILE
089800070216      *------------------------------------------------------------------------*
089900070508     c     sr_contrsfl   begsr
090000070216
090100070508     c                   clear                   nrr
090200070216     c                   do        *hival
090300070508     c                   add       1             nrr
090400070508     c     nrr           chain     lvp2s01                            32
090500070216      * esco se fine sfl
090600070216     c                   if        *in32
090700070216     c                   leave
090800070216     c                   endif
090900070216
091000070220      * opzione V --> visualizza
091100070511     c                   if        v1sopz = '5'
091200070220     c                   exsr      sr_visriga
091300070508     c                   eval      recsf = nrr
091400070510     c                   eval      v1sopz = *blanks
091500070508     c                   clear                   v1sopz
091600070514     c                   eval      *in73 = *on
091700070508     c                   update    lvp2s01
091800070511     c                   if        *inkl
091900070511     c                   goto      emisfl
092000070511     c                   endif
092100070508     c                   else
092200070223
092300070223      * spengo l'indicatore di PC
092400070514     c                   eval      *in73 = *off
092500070510     c                   clear                   v1sopz
092600070508     c                   update    lvp2s01
092700070508     c                   endif
092800070223
092900070216     c                   enddo
093000070216
093100070216     c                   endsr
093200070508
093300070508      *------------------------------------------------------------------------*
093400070508      *? VISUALIZZO RIGA LOG
093500070508      *------------------------------------------------------------------------*
093600070508     c     sr_visriga    begsr
093700070510
093800070510     c                   exsr      sr_pulvid03
093900070508      * tipo record
094000070514     c                   eval      v3ctiporcd = v1stiporcd
094100070514     c                   clear                   v3dtiporcd
094200070508     c                   clear                   tibs02ds
094300070508     c                   eval      t02mod = 'C'
094400070508     c                   eval      t02sif = knsif
094500070508     c                   eval      t02cod = 'PRC'
094600070514     c                   eval      t02ke1 = v3ctiporcd
094700070508     c                   call      'TIBS02R'
094800070508     c                   parm                    kpjba
094900070508     c                   parm                    tibs02ds
095000070508     c                   if        t02err = *blanks
095100070508     c                   eval      dprc = t02uni
095200080327     c                   endif
095300080327     c* per consegne chiamo pgm esterni per visualizzare il record
095400080327     c                   if        D§PRCCR = 'C'
095500080327     c                   exsr      sr_call
095600080327      * f3=fine
095700080327     c                   if        esitol = '1'
095800080327     c                   goto      fine
095900080327     c                   endif
096000080327
096100080327      * f12=ritorno
096200080327     c                   if        esitol ='2'
096300080327     c                   leavesr
096400080327     c                   endif
096500080327      * f12=ritorno
096600080327     c                   if        esitol <>' '
096700080327     c                   seton                                        28
096800080327     c                   movel     msg(14)       v1cmsg
096900080327     c                   leavesr
097000080327     c                   endif
097100080327     c                   end
097200070514     c                   eval      v3dtiporcd = d§prcdes
097300070508      * tipo applicazione
097400070514     c                   eval      v3ctipappl = v1stipappl
097500070514     c                   clear                   v3dtipappl
097600070514     c                   if        v3ctipappl = 'R'
097700070514     c                   eval      v3dtipappl = 'Ritiri'
097800070508     c                   endif
097900070508      * profilo
098000070508     c                   eval      v3cprfc = v1sprfc
098100070508      * id dispositivo
098200070508     c                   eval      v3ciddisp = v1siddisp
098300070508      * data/ora ricezione
098400070508     c                   eval      v3cdata = v1sdata
098500070508     c                   eval      v3cora = v1sora
098600070508      * id documento
098700070508     c                   eval      v3ciddoc = v1siddoc
098800070508      * autotrasportatore
098900070508     c                   eval      v3ccodaut = v1scodaut
099000070508     c                   clear                   v3dcodaut
099100070508     c                   eval      apdtip = 'A'
099200070508     c                   eval      apdpdr = v3ccodaut
099300110308     c     kfiapd        chain     fiapd000
099400070508     c                   if        %found(fiapd01l) and apdatb = *blanks
099500070508     c                   eval      v3dcodaut = apdrsc
099600070508     c                   endif
099700070508      * filiale distinta
099800070508     c                   eval      v3cfgs = v1sfgs
099900070508     c                   clear                   v3dfgs
100000110308     c     v3cfgs        chain     azorg
100100070508     c                   if        %found(azorg01l) and orgfva = *blanks
100200070508     c                   eval      v3dfgs = orgdes
100300070508     c                   endif
100400070508      * numero distinta
100500070508     c                   eval      v3cndc = v1sndc
100600070508     c                   clear                   v3dndc
100700070508     c                   eval      dstnpg = 4
100800070508     c                   eval      dstfgs = v3cfgs
100900070508     c                   eval      dstnfv = v3cndc
101000110308     c     kfidst        chain     fidst000
101100070508     c                   if        %found(fidst01l)
101200070508     c                   eval      apdtip = 'A'
101300070508     c                   eval      apdpdr = dstpdr
101400070508     c                   clear                   apdrsc
101500110308     c     kfiapd        chain     fiapd000
101600070508     c                   if        %found(fiapd01l)
101700110308     c                   eval      v3dndc = 'Del' +
101800110308     c                             c_ColorTrq +
101900070508     c                             %subst(%editc(%dec(dstdfv):'X'):7:2) + '/' +
102000070508     c                             %subst(%editc(%dec(dstdfv):'X'):5:2) + '/' +
102100070508     c                             %subst(%editc(%dec(dstdfv):'X'):1:4) + ' ' +
102200110308     c                             %editc(%dec(dstpdr):'X') +
102300110308     c                             c_ColorBlu +
102400070508     c                             %subst(apdrsc:1:17)
102500070508     c                   endif
102600070508     c                   endif
102700070508      * messaggio errore
102800070508     c                   eval      v3cmsg = v1smsg
102900070508      * spengo indicatori per visualizzazione dei vari tipi record
103000070508     c                   eval      *in50 = *off
103100070508     c                   eval      *in51 = *off
103200070508     c                   eval      *in52 = *off
103300070514     c                   eval      *in53 = *off
103400140212     c                   eval      *in54 = *off
103500070508      * spengo indicatori per visualizzazione i due diversi documenti
103600070508     c                   eval      *in60 = *off
103700070514      * spengo indicatori per visualizzazione ORM manuale
103800070514     c                   eval      *in62 = *off
103900070508      * devo visualizzare cose diverse per i vari tipi record
104000070508     c                   select
104100070508
104200070508      * tipo record OK  --> conferma dati
104300070514     c                   when      v1stiporcd = 'OK '
104400070508     c                   eval      *in50 = *on
104500070508     c                   eval      fiprcokds = v1sdati
104600070508      * filiale distinta
104700070514     c                   eval      v3fgs = §okfgs
104800070508      * numero distinta
104900070514     c                   eval      v3ndc = §okndc
105000070514      * data/ora evento
105100070531     c                   eval      v3data = %subst(§okdatora:1:8)
105200070514     c                   eval      v3ora = %subst(§okdatora:9:6)
105300070508      * tipo iddocumento e iddocumento
105400070514     c                   select
105500070514     c                   when      §oktpiddoc = 'R'
105600070508     c                   eval      v3doktpdoc = 'ORM'
105700070514     c                   eval      idormds = §okiddoc
105800070508     c                   eval      *in60 = *on
105900070514     c                   other
106000070514     c                   eval      *in60 = *off
106100070514     c                   eval      v3doktpdoc = §oktpiddoc
106200070514     c                   endsl
106300070508      * richiesta assistenza
106400140217     c                   clear                   v3resnote
106500070514     c                   select
106600070514     c                   when      §okflgass = 'S'
106700070508     c                   eval      v3okflgass = 'SI'
106800070514     c                   when      §okflgass = *blanks
106900070508     c                   eval      v3okflgass = 'NO'
107000070514     c                   other
107100070514     c                   eval      v3okflgass = §okflgass
107200140217     c                   movel     'RAS'         tbecod
107300140217     c                   movel(p)  §okflgass     tbeke1
107400140217     c     ktbe          chain     tntbe01l
107500140217     c                   if        %found(tntbe01l)
107600140217     c                   movel     tbeuni        v3resnote
107700140217     c                   else
107800140217     c                   move      *all'?'       v3resnote
107900140217     c                   endif
108000070514     c                   endsl
108100080221      * id riga
108200080221     c                   eval      v3okidrow = §okidrow
108300140217     c                   eval      v3drescmr = §oknotass
108400070508
108500140212      * tipo record ROR --> ora stimata ritiro
108600140212     c                   when      v1stiporcd = 'ROR'
108700140212     c                   eval      *in50 = *on
108800140212     c                   eval      *in54 = *on
108900140212     c                   eval      fiprcrords = v1sdati
109000140212      * filiale distinta
109100140212     c                   eval      v3fgs = §rorfgs
109200140212      * numero distinta
109300140212     c                   eval      v3ndc = §rorndc
109400140212      * data/ora evento
109500140212     c                   eval      v3data = %subst(§rordatora:1:8)
109600140212     c                   eval      v3ora = %subst(§rordatora:9:6)
109700140212      * tipo iddocumento e iddocumento
109800140217     c                   eval      v3doktpdoc = 'ORM'
109900140212     c                   eval      idormds = §roriddoc
110000140212      * ora stimata
110100140212     c                   movel     §rororasti    v3rororast
110200140212     c                   movel     §rorsort      v3rorsort
110300140212
110400070508      * tipo record RES --> esiti ritiri
110500070514     c                   when      v1stiporcd = 'RES'
110600070508     c                   eval      *in51 = *on
110700070508     c                   eval      fiprcresds = v1sdati
110800070508      * filiale distinta
110900070514     c                   eval      v3fgs = §resfgs
111000070508      * numero distinta
111100070514     c                   eval      v3ndc = §resndc
111200070514      * data/ora evento
111300070531     c                   eval      v3data = %subst(§resdatora:1:8)
111400070514     c                   eval      v3ora = %subst(§resdatora:9:6)
111500070514      * numero ORM
111600070514     c                   move      §respoe       v3respoe
111700070514     c                   move      §resnsr       v3resnsr
111800070514     c                   move      §resnor       v3resnor
111900070514     c                   move      §resnrv       v3resnrv
112000070508      * colli/bancali
112100070514     c                   eval      v3resncl = §resncl
112200070514     c                   eval      v3resbnc = §resbnc
112300070508      * note
112400070508     c                   eval      v3resnote = §resnote
112500070508      * causale
112600081106      * no AAA (orm manuale annullato)
112700081106     c                   if        §rescmr <> 'AAA'
112800070508     c                   eval      v3rescmr = §rescmr
112900070518     c                   clear                   v3drescmr
113000070518     c                   clear                   dcmr
113100070508     c                   clear                   tibs02ds
113200070508     c                   eval      t02mod = 'C'
113300070508     c                   eval      t02sif = knsif
113400070508     c                   eval      t02cod = 'CMR'
113500070508     c                   eval      t02ke1 = v3rescmr
113600070508     c                   call      'TIBS02R'
113700070508     c                   parm                    kpjba
113800070508     c                   parm                    tibs02ds
113900070508     c                   if        t02err = *blanks
114000070508     c                   eval      dcmr = t02uni
114100070508     c                   endif
114200070508     c                   eval      v3drescmr = d§cmrdes
114300081106     c                   else
114400081106     c                   eval      v3cmsg = 'ORM MANUALE ANNULLATO'
114500081106     c                   endif
114600070508      * data/ora inserimento
114700070531     c                   eval      v3resdatai = %subst(§resdtorin:1:8)
114800070518     c                   eval      v3resorai = %subst(§resdtorin:9:6)
114900070508      * inserito da spc
115000070514     c                   select
115100070514     c                   when      §resflgspc = 'S'
115200070514     c                   eval      v3resflspc = 'SPC'
115300070514     c                   when      §resflgspc = *blanks
115400070514     c                   eval      v3resflspc = 'AUT'
115500070514     c                   other
115600070514     c                   eval      v3resflspc = §resflgspc
115700070514     c                   endsl
115800070508      * ORM manuale
115900070514     c                   eval      v3resrsr = §resrsr
116000070514     c                   eval      v3resinr = §resinr
116100070514     c                   eval      v3resrfa = §resrfa
116200070514      * visualizzo ORM manuale o n. ORM valido
116300070514     c                   if        §resrsr <> *blanks and
116400080407     c                             %subst(v1sdati:10:8) = *all'0'
116500070514     c                   eval      *in62 = *on
116600070508     c                   else
116700070514     c                   eval      *in62 = *off
116800070508     c                   endif
116900080214      * codice fiscale
117000080214     c                   eval      v3rescdf = §rescodfis
117100080214      * partita iva
117200080214     c                   eval      v3resiso = §resisoiva
117300080214     c                   eval      v3resiva = §respiva
117400120515      * Coordinate GEO
117500120515     c                   IF        §RESxlongi > *zeros
117600120515     c                   eval      v3reslongi = %dec(§resxlongi:8:0)/1000000
117700120515     c                   ELSE
117800120515     c                   clear                   v3reslongi
117900120515     c                   ENDIF
118000120515     c                   IF        §RESylati > *zeros
118100120515     c                   eval      v3reslati  = %dec(§resylati:8:0)/1000000
118200120515     c                   ELSE
118300120515     c                   clear                   v3reslati
118400120515     c                   ENDIF
118500070508
118600070508      * tipo record RCH --> chiusura distinta
118700070514     c                   when      v1stiporcd = 'RCH'
118800070508     c                   eval      *in52 = *on
118900070508     c                   eval      fiprcrchds = v1sdati
119000070508      * filiale distinta
119100070514     c                   eval      v3fgs = §rchfgs
119200070508      * numero distinta
119300070514     c                   eval      v3ndc = §rchndc
119400070514      * data/ora chiusura (evento)
119500070531     c                   eval      v3data = %subst(§rchdatora:1:8)
119600070514     c                   eval      v3ora = %subst(§rchdatora:9:6)
119700070508      * colli/bancali
119800070514     c                   eval      v3rchncl = §rchncl
119900070514     c                   eval      v3rchbnc = §rchbnc
120000080707      * codice SPC
120100080707     c                   eval      v3rchspc = §rchcodspc
120200070514
120300070514      * tipo record     --> altri tipi record
120400070514     c                   other
120500070514     c                   eval      *in53 = *on
120600070508     c                   endsl
120700070508
120800070508     c                   write     lvp2t01
120900070508     c                   exfmt     lvp2d03
121000070508
121100070508      * f3=fine
121200070508     c                   if        *inkc
121300070508     c                   goto      fine
121400070508     c                   endif
121500070508
121600070508      * f12=ritorno
121700070508     c                   if        *inkl
121800070511     c                   leavesr
121900070508     c                   endif
122000070508
122100070508     c                   endsr
122200070510
122300070510      *------------------------------------------------------------------------*
122400080327      *? chiamata a pgm esterni per visualizzare record log consegne
122500070510      *------------------------------------------------------------------------*
122600080327     c     sr_call       begsr
122700080520     c                   clear                   esitol
122800070510
122900080327     c* lancio
123000080327     c                   call      'FNLVP3C'
123100080327     c                   parm                    kpjba
123200080327     c                   parm                    nrrplg
123300080327     c                   parm                    esitol            1
123400080327
123500080327     c                   endsr
123600080327
123700080327      *------------------------------------------------------------------------*
123800080327      *? PULIZIA DELLA VIDEATA  ?
123900080327      *------------------------------------------------------------------------*
124000080327     c     sr_pulvid03   begsr
124100080327
124200070514     c                   clear                   v3ctiporcd
124300070514     c                   clear                   v3dtiporcd
124400070514     c                   clear                   v3ctipappl
124500070514     c                   clear                   v3dtipappl
124600070510     c                   clear                   v3cprfc
124700070510     c                   clear                   v3ciddisp
124800070510     c                   clear                   v3cdata
124900070510     c                   clear                   v3cora
125000070510     c                   clear                   v3ciddoc
125100070510     c                   clear                   v3ccodaut
125200070510     c                   clear                   v3dcodaut
125300070510     c                   clear                   v3cfgs
125400070510     c                   clear                   v3dfgs
125500070510     c                   clear                   v3cndc
125600070510     c                   clear                   v3dndc
125700070510     c                   clear                   v3cmsg
125800070510
125900070514     c                   clear                   v3fgs
126000070514     c                   clear                   v3ndc
126100070514     c                   clear                   v3data
126200070514     c                   clear                   v3ora
126300070514
126400070510     c                   clear                   v3doktpdoc
126500070514     c                   clear                   idormds
126600070514     c                   clear                   idbolds
126700070510     c                   clear                   v3okflgass
126800070510
126900070510     c                   clear                   v3respoe
127000070510     c                   clear                   v3resnsr
127100070510     c                   clear                   v3resnor
127200070510     c                   clear                   v3resnrv
127300070510     c                   clear                   v3resncl
127400070510     c                   clear                   v3resbnc
127500070510     c                   clear                   v3resnote
127600070510     c                   clear                   v3rescmr
127700070510     c                   clear                   v3drescmr
127800070510     c                   clear                   v3resdatai
127900070510     c                   clear                   v3resorai
128000070510     c                   clear                   v3resflspc
128100070510     c                   clear                   v3resrsr
128200070510     c                   clear                   v3resrfa
128300070510     c                   clear                   v3resinr
128400070510
128500080707     c                   clear                   v3rchspc
128600070510     c                   clear                   v3rchncl
128700070510     c                   clear                   v3rchbnc
128800070510
128900070510     c                   endsr
129000061227
129100070504      *------------------------------------------------------------------------*
129200070504      *? ROUTINE INIZIALE
129300070504      *------------------------------------------------------------------------*
129400070504     c     *inzsr        begsr
129500070504
129600070504     c     *entry        plist
129700070504     c                   parm                    kpjba
129800110311     c                   clear                   Richiamato        1
129900110311     c                   if        kpjbu <> *blanks
130000080331     c                   movel     kpjbu         fnlvp2ds
130100110311     c                   eval      Richiamato = *on
130200110311     c                   endif
130300110119      * -?Verifica del sistema AS/400 corrente?
130400110119     c                   if        UBRTVNETA_Rtv(currSysNeta) <> *zero
130500110119     c                   eval      *inlr = *on
130600110119     c                   return
130700110119     c                   endif
130800110119      * -?Impostazione libreria archivi di filiale?
130900110119     c                   if        %subst ( currSysNeta : 1 : 6 ) = 'SETRAS'
131000110119     c                   eval      wLibFIDST = 'FILTRA201/FIDST01L'
131100110119     c                   else
131200110119     c                   eval      wLibFIDST = 'FILTRAPRD/FIDST01L'
131300110119     c                   endif
131400110119      * -?Apertura archivi di filiale?
131500110119     c                   open      FIDST01L
131600110119      *
131700070504     c     *dtaara       define    §azute        azuteds
131800070504     c     *dtaara       define    §datiute      ddatiute
131900070504     c                   in(e)     *dtaara
132000070504     c                   if        %error  or rsut = *blanks
132100070504     c                   clear                   tibs34ds
132200070504     c                   call      'TIBS34R'
132300070504     c                   parm                    tibs34ds
132400070504     c                   in        *dtaara
132500070504     c                   endif
132600070504
132700070504      * carico schiera filiali gestite
132800070504     c                   clear                   trul31ds
132900070504     c                   eval      i31abi = uteaut
133000070504     c                   eval      i31cdi = dutdis
133100070504     c                   eval      i31car = dutare
133200070504     c                   eval      i31cpo = dutpou
133300070504     c                   call      'TRUL31R'
133400070504     c                   parm                    kpjba
133500070504     c                   parm                    trul31ds
133600070504      * se non carica nessuna filiale errore utente non abilitato
133700070504     c                   if        o31pog <= *zeros
133800070504     c                   eval      *in28 = *on
133900070504     c                   eval      v1cmsg = msg(01)
134000070504     c                   endif
134100070504
134200070504      * controllo se PDA attivo sulla filiale utente
134300070504     c                   clear                   og148
134400110308     c     dutpou        chain     azorg
134500070504     c                   if        %found(azorg01l) and orgfva = *blanks
134600070504     c                   eval      og148 = orgde8
134700070504     c                   endif
134800080213     c                   if        §ogpdaorm = *blanks
134900110119     c                             and  DUTpou <> 046
135000070504     c                   eval      *in28 = *on
135100070504     c                   eval      *in40 = *on
135200070504     c                   eval      v1cmsg = msg(02)
135300070504     c                   endif
135400070531
135500070531      * cerco i gg. di pulizia del prospetto ORM
135600070531     c                   clear                   ds5a2
135700070531     c                   eval      tblkut = 1
135800070531     c                   eval      tblcod = '5A'
135900070531     c                   eval      tblkey = '2       '
136000110308     c     ktabel        chain     tabel
136100070531     c                   if        %found(tabel00f) and tblflg = *blanks
136200070531     c                   eval      ds5a2 = tbluni
136300070531     c                   endif
136400070531      * calcolo data partenza per ricerca distinte
136500081117     c                   move      *date         dataiso
136600081117     c                   subdur    §5aporm:*d    dataiso
136700070531
136800070531     c     ktabel        klist
136900070531     c                   kfld                    tblkut
137000070531     c                   kfld                    tblcod
137100070531     c                   kfld                    tblkey
137200070504
137300070504     c     kfidst        klist
137400070504     c                   kfld                    dstnpg
137500070504     c                   kfld                    dstnfv
137600070508     c                   kfld                    dstfgs
137700070504
137800070209     c     kfiapd        klist
137900070214     c                   kfld                    apdtip
138000070214     c                   kfld                    apdpdr
138100070504
138200140217     c     ktbe          klist
138300140217     c                   kfld                    tbecod
138400140217     c                   kfld                    tbeke1
138500070504     c                   endsr
138600070504
138700070504** MSG  Lungh. 78                                                            *
138800070504Utente non abilitato alla funzione                                            01
138900070504Procedura PDA non attiva per la filiale distinta                              02
139000070504Effettuare almeno una scelta                                                  03
139100070504Data ricezione errata                                                         04
139200070504Ora ricezione errata                                                          05
139300070504Filiale distinta errata                                                       06
139400070504Filiale distinta non in gestione all'utente                                   07
139500070504Distinta errata                                                               08
139600070504Distinta non abilitata allo scarico dati su PDA                               09
139700070504Autotrasportatore errato                                                      10
139800070504Autotrasportatore non in gestione all'utente                                  11
139900070507Errore nella ricerca dei dati inviati da PDA                                  12
140000070514Tipo record errato                                                            13
140100080327Record errato                                                                 14
140200080328Id documento non previsto x il tipo record scelto                             15
140300110324SUPERATO IL NUMERO MASSIMO DI RECORD VISUALIZZABILI NEL SUBFILE (9999)        16
