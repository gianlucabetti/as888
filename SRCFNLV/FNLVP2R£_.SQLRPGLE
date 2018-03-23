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
010801140212     d fiprcrords    e ds
010900070504     d fiprcrchds    e ds
011000070504     d fiprcresds    e ds
011100070209     d fnlv24ds      e ds
011200081117     d fidg31ds      e ds
011300070209     d kpjba         e ds
011400070209     d og148         e ds
011500070508     d tibs02ds      e ds
011600070209     d tibs34ds      e ds                  inz
011700070504     d trul31ds      e ds                  inz
011800070504     d  pog                   10    759  0 dim(250)
011900070209
012000070209     d                sds
012100070209     d  vtcpgm                 1     10
012200110119
012300110119      * ? P R O C E D U R E   U S A T E ?
012400110119       // -?Reperimento NETA sistema AS/400 corrente?
012500110119     d currSysNeta     s              8a   inz
012600110119      /copy gaitrasrc/srcProtoPr,UBRTVNETA
012700110308
012800110308      * ? D E F I N I Z I O N E   K E Y - L I S T ?
012900110308       // -?File TNTBE01L?
013000110308     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
013100110308     d                                     prefix(k_)   inz
013200070209
013300070209      *------------------------------------------------------------------------*
013400080130
013500080130     c/exec sql
013600080130     c+ set option dynusrprf = *owner, closqlcsr = *endmod
013700080130     c/end-exec
013800070209
013900070216      *? prima videata ?
014000070216     c     emid01        tag
014100070216      * pulisco la videata
014200070209     c                   if        not *in28 and not *in90
014300070216     c                   exsr      sr_puld01
014400070209     c                   endif
014500080331      * emetto la videata solo se non sono richiamato
014600110311     c                   if        Richiamato = *blanks
014700070504     c                   write     lvp2t01
014800070504     c                   exfmt     lvp2d01
014900110311     c                   endif
015000070216      * spengo indicatori di comodo
015100070209     c                   eval      *in28 = *off
015200070209     c                   eval      *in90 = *off
015300070216      * pulisco campo messaggi
015400070216     c                   clear                   v1cmsg
015500070214      * f3=fine
015600070209     c                   if        *inkc
015700070216     c                   goto      fine
015800070209     c                   endif
015900070215      * controllo i dati della videata
016000070219     c                   exsr      sr_contrd01
016100080331     c* se richiamato
016200110311     c                   IF        Richiamato <> *blanks
016300110311     c                   If        *in28 or *in90 or *inkl
016400080331     c                   if        *in28 or *in90
016500080331     c                   eval      ds2esito = 'E'
016600110311     c                   endif
016700080331     c                   if        *inkl
016800080331     c                   eval      ds2esito = '2'
016900110311     c                   endif
017000080331     c                   goto      fine
017100110311     c                   EndIf
017200110311     c                   ENDIF
017300080331     c*
017400070209     c                   if        *in28 or *in90
017500070216     c                   goto      emid01
017600070209     c                   endif
017700070504
017800070504      * con sql elaboro il file in base a quanto richiesto in prima
017900080331      * videata o passato dal chiamante nella ds FNLVP2DS
018000070504     c                   exsr      sr_carsql
018100070216
018200070504      *? subfile ?
018300070504     c                   exsr      sr_pulsfl
018400070504     c                   exsr      sr_carsfl
018500070507      * se errore nel carico dati da sql ritorno alla prima videata con msg
018600070507      * di errore
018700070507     c                   if        *in28
018800070507     c                   goto      emid01
018900070507     c                   endif
019000070511
019100070511     c     emisfl        tag
019200070223      * emetto videata vuota se non ho caricato niente
019300070508     c                   if        nrr = *zeros
019400070223     c                   exsr      sr_emid02
019500070223     c                   else
019600070504     c                   exsr      sr_emisfl
019700070223     c                   endif
019800070214
019900070216      *? fine programma ?
020000070216     c     fine          tag
020100070216
020200080331     c* se richiamato passo l'esito
020300110311     c                   If        Richiamato <> *blanks
020400080520     c                   eval      ds2esito = esitol
020500080331     c                   if        *inkc
020600080331     c                   eval      ds2esito = '1'
020700110311     c                   endif
020800080331     c                   movel(p)  fnlvp2ds      kpjbu
020900110311     c                   EndIf
021000080331     c*
021100070214     c                   eval      *inlr = *on
021200070209
021300070209      *------------------------------------------------------------------------*
021400070216      *? PULIZIA DELLA PRIMA VIDEATA  ?
021500070209      *------------------------------------------------------------------------*
021600070216     c     sr_puld01     begsr
021700070209
021800070508     c                   clear                   v1cdata
021900070508     c                   clear                   v1cdatainv
022000070504     c                   clear                   v1cora
022100070514     c                   clear                   v1ctiporcd
022200070514     c                   clear                   v1dtiporcd
022300070508     c                   clear                   v1cfgs
022400070508     c                   clear                   v1dfgs
022500070504     c                   clear                   v1cndc
022600070504     c                   clear                   v1dndc
022700070508     c                   clear                   v1ccodaut
022800070209     c                   clear                   v1dpdr
022900070504     c                   clear                   v1cerr
023000080331     c                   clear                   v1ciddoc
023100110311     c                   clear                   V1CidDisp
023200080331     c* se richiamato imposto i dati nella prima videata e non emetto il
023300080331     c* video di richiesta parametri ma carico direttamente il sfl
023400110311     c                   if        Richiamato <> *blanks
023500080331     c                   eval      v1ctiporcd = ds2tiporcd
023600080331     c                   eval      v1cfgs = ds2fgs
023700080331     c                   eval      v1cndc = ds2ndc
023800080331     c                   eval      v1ccodaut = ds2codaut
023900080331     c                   eval      v1ciddoc = ds2iddoc
024000110311     c                   endif
024100070209
024200070209     c                   endsr
024300070214
024400070209      *------------------------------------------------------------------------*
024500070216      *? CONTROLLO I DATI DELLA PRIMA VIDEATA   ?
024600070209      *------------------------------------------------------------------------*
024700070209     c     sr_contrd01   begsr
024800070214
024900070504      * indicatori di posizionamento cursore
025000070504     c                   eval      *in40 = *off
025100070504     c                   eval      *in41 = *off
025200070504     c                   eval      *in42 = *off
025300070504     c                   eval      *in43 = *off
025400070504     c                   eval      *in44 = *off
025500070514     c                   eval      *in45 = *off
025600080328     c                   eval      *in46 = *off
025700070504
025800070504      * almeno una scelta è da fare
025900070510     c                   if        v1cdata = *zeros and v1cora = *zeros and
026000110311     c                             V1CtipoRcd = *blanks  and
026100110311     c                             V1CidDisp  = *blanks  and
026200110311     c                             V1CidDoc   = *blanks  and
026300080328     c                             v1cfgs = *zeros and v1cndc = *blanks and
026400070510     c                             v1ccodaut = *blanks and v1cerr = *blanks
026500070504     c                   eval      *in28 = *on
026600070504     c                   eval      *in40 = *on
026700070504     c                   eval      v1cmsg = msg(03)
026800070504     c                   leavesr
026900070504     c                   endif
027000070504
027100070504      *? DATA RICEZIONE
027200070508     c                   if        v1cdata > *zeros
027300070511     c                   z-add     v1cdata       g08dat
027400070511     c                   call      'XSRDA8'
027500070511     c                   parm                    wlbdat
027600070511     c                   if        g08err = '1'
027700070511     c                   eval      *in40 = *on
027800070511     c                   eval      *in28 = *on
027900070511     c                   eval      v1cmsg = msg(04)
028000070511     c                   leavesr
028100070511     c                   endif
028200070511     c                   z-add     g08dat        v1cdata
028300070511     c                   move      g08inv        v1cdatainv
028400070504     c                   endif
028500070504
028600070504      *? ORA RICEZIONE
028700070504     c                   if        v1cora > *zeros
028800070504     c     *eur          test(te)                v1cora
028900070504     c                   if        %error
029000070504     c                   eval      *in41 = *on
029100070504     c                   eval      *in28 = *on
029200070504     c                   eval      v1cmsg = msg(05)
029300070504     c                   leavesr
029400070504     c                   endif
029500070504     c                   endif
029600070514
029700070514      *? TIPO RECORD
029800080331     c                   clear                   dprc
029900110307if  1c                   if        v1ctiporcd <> *blanks
030000070514     c                   clear                   v1dtiporcd
030100070514      *  ricerca
030200110307if  2c                   if        %scan('?':v1ctiporcd) > *zeros
030300070514     c                   eval      *in45 = *on
030400070514     c                   eval      *in90 = *on
030500070514     c                   clear                   tibs02ds
030600070514     c                   eval      t02mod = 'R'
030700070514     c                   eval      t02sif = knsif
030800070514     c                   eval      t02cod = 'PRC'
030900070514     c                   call      'TIBS02R'
031000070514     c                   parm                    kpjba
031100070514     c                   parm                    tibs02ds
031200070514     c                   eval      v1ctiporcd = t02ke1
031300070514     c                   eval      dprc = t02uni
031400070514     c                   eval      v1dtiporcd = d§prcdes
031500070514     c                   leavesr
031600110307e   2c                   endif
031700070514      * controllo
031800070514     c                   clear                   tibs02ds
031900070514     c                   eval      t02mod = 'C'
032000070514     c                   eval      t02sif = knsif
032100070514     c                   eval      t02cod = 'PRC'
032200070514     c                   eval      t02ke1 = v1ctiporcd
032300070514     c                   call      'TIBS02R'
032400070514     c                   parm                    kpjba
032500070514     c                   parm                    tibs02ds
032600110307if  2c                   if        t02err <> *blanks
032700070514     c                   eval      *in28 = *on
032800070514     c                   eval      *in45 = *on
032900070514     c                   eval      v1cmsg = msg(13)
033000070514     c                   leavesr
033100110307e   2c                   endif
033200070514     c                   eval      v1ctiporcd = t02ke1
033300070514     c                   eval      dprc = t02uni
033400070514     c                   eval      v1dtiporcd = d§prcdes
033500110307e   1c                   endif
033600110307
033700080328      *? id documento (non faccio controllo esistenza bolla xchè non è
033800080328      *? detto che si voglia ricercare una spedizione esistente)
033900110307     c                   if        v1ciddoc   <> *blanks  and
034000110307     c                             V1CtipoRcd <> *blanks  and
034100080328     c                             (d§prcipd = *blanks or d§prcipd ='000')
034200080328     c                   eval      *in28 = *on
034300080328     c                   eval      *in46 = *on
034400080328     c                   eval      v1cmsg = msg(15)
034500080328     c                   leavesr
034600080328     c                   endif
034700070504
034800070504      *? FILIALE DISTINTA
034900070508     c                   if        v1cfgs > *zeros
035000070508     c                   clear                   v1dfgs
035100070504     c                   clear                   og148
035200070504      *  valida
035300110308     c     v1cfgs        chain     azorg
035400070504     c                   if        not %found(azorg01l) or orgfva <> *blanks
035500070504     c                   eval      *in28 = *on
035600070504     c                   eval      *in42 = *on
035700070504     c                   eval      v1cmsg = msg(06)
035800070504     c                   leavesr
035900070504     c                   endif
036000070504     c                   eval      v1dfgs = orgdes
036100070504     c                   eval      og148 = orgde8
036200070504      *  PDA attivo
036300080213     c                   if        §ogpdaorm = *blanks
036400070504     c                   eval      *in28 = *on
036500070504     c                   eval      *in42 = *on
036600070504     c                   eval      v1cmsg = msg(02)
036700070504     c                   leavesr
036800070504     c                   endif
036900070504      *  deve essere una filiale gestita dall'utente
037000070508     c     v1cfgs        lookup    pog                                    30
037100070504     c                   if        not *in30
037200070504     c                   eval      *in28 = *on
037300070504     c                   eval      *in42 = *on
037400070504     c                   eval      v1cmsg = msg(07)
037500070504     c                   leavesr
037600070504     c                   endif
037700070504     c                   endif
037800070504
037900070504      *? DISTINTA
038000070504     c                   if        v1cndc <> *blanks
038100070511     c                   if        v1cfgs = *zeros
038200070511     c                   eval      *in28 = *on
038300070511     c                   eval      *in42 = *on
038400070511     c                   eval      v1cmsg = msg(06)
038500070511     c                   leavesr
038600070511     c                   endif
038700070504     c                   clear                   v1dndc
038800070504      *  ricerca
038900070504     c                   if        %scan('?':v1cndc) > *zeros
039000081117     c                   clear                   v1cndc
039100081117     c                   clear                   fidg31ds
039200070504     c                   eval      *in43 = *on
039300070504     c                   eval      *in90 = *on
039400081117     c                   eval      fdgfgs = v1cfgs
039500081117     c                   move      dataiso       fdggda
039600081117     c                   eval      fdggal = *date
039700081117     c                   eval      fdgtfv = '2'
039800081117     c                   eval      fdgnpg = 4
039900081117     c                   eval      fdgric = 'S'
040000081117     c                   eval      fdgtpv = 'T'
040100081117     c                   clear                   $fv6
040200081117      * impostato a tutti 99999
040300081117     c                   do        600           xx
040400081117     c                   z-add     999999        $fv6(xx)
040500081117     c                   enddo
040600081117     c                   eval      fdgtpop = 'R'
040700081117     c                   eval      kpjbu = fidg31ds
040800081117     c                   call      'FIDG31R'
040900081117     c                   parm                    kpjba
041000081117     c                   parm                    $fv6
041100081117     c                   eval      fidg31ds = kpjbu
041200081117      * se torno dal pgm senza aver selezionato niente pulisco i dati della DS
041300081117     c                   if        fdgnfv = *zeros
041400081117     c                   clear                   fidg31ds
041500081117     c                   else
041600081117     c                   eval      v1cndc = *all'0'
041700081117     c                   move      fdgnfv        v1cndc
041800081117     c                   endif
041900081117     c                   if        v1cndc = *zeros
042000081117     c                   clear                   v1cndc
042100081117     c                   leavesr
042200081117     c                   endif
042300081117     c                   endif
042400070504      *  valida
042500070504     c                   eval      dstnpg = 4
042600070508     c                   eval      dstfgs = v1cfgs
042700070504     c                   move      v1cndc        dstnfv
042800110308     c     kfidst        chain     fidst000
042900070504     c                   if        not %found(fidst01l)
043000070504     c                   eval      *in28 = *on
043100070504     c                   eval      *in43 = *on
043200070504     c                   eval      v1cmsg = msg(08)
043300070504     c                   leavesr
043400070504     c                   endif
043500070504      *  abilitata allo scarico PDA
043600080424     c                   if        dstpda ='N'
043700070504     c                   eval      *in28 = *on
043800070504     c                   eval      *in43 = *on
043900070504     c                   eval      v1cmsg = msg(09)
044000070504     c                   leavesr
044100070504     c                   endif
044200070504      *  decodifico
044300070504     c                   eval      apdtip = 'A'
044400070504     c                   eval      apdpdr = dstpdr
044500070504     c                   clear                   apdrsc
044600110308     c     kfiapd        chain     fiapd000
044700070504     c                   if        %found(fiapd01l)
044800110308     c                   eval      v1dndc = 'Del' +
044900110308     c                             c_ColorTrq +
045000070504     c                             %subst(%editc(%dec(dstdfv):'X'):7:2) + '/' +
045100070504     c                             %subst(%editc(%dec(dstdfv):'X'):5:2) + '/' +
045200070504     c                             %subst(%editc(%dec(dstdfv):'X'):1:4) + ' ' +
045300110308     c                             %editc(%dec(dstpdr):'X') +
045400110308     c                             c_ColorBlu +
045500070504     c                             %subst(apdrsc:1:17)
045600070504     c                   endif
045700070504     c                   endif
045800070504
045900070504      *? AUTOSTRASPORTATORE
046000070508     c                   if        v1ccodaut <> *blanks
046100070504      *  pulisco il campo se tutti zeri
046200070508     c                   if        v1ccodaut = *all'0'
046300070508     c                   clear                   v1ccodaut
046400070504     c                   endif
046500070504      *  ricerca
046600070508     c                   if        %scan('?':v1ccodaut) > *zeros
046700070514     c                   eval      *in44 = *on
046800070504     c                   eval      *in90 = *on
046900070504     c                   clear                   fnlv24ds
047000070504     c                   eval      d24fil = dutpou
047100070504     c                   eval      d24tip = 'A'
047200070504     c                   eval      d24flg = 'R'
047300070504     c                   eval      kpjbu = fnlv24ds
047400070504     c                   call      'FNLV24R'
047500070504     c                   parm                    kpjba
047600070504      *  è stato scelto un codice lo imposto a video
047700070504     c                   eval      fnlv24ds = kpjbu
047800070504     c                   if        d24pdr <> *zeros
047900070508     c                   move      d24pdr        v1ccodaut
048000070504     c                   eval      v1dpdr = d24rsc
048100070504     c                   leavesr
048200070504     c                   endif
048300070504      *  non è stato scelto nessun codice pulisco i campi a video
048400070508     c                   clear                   v1ccodaut
048500070504     c                   clear                   v1dpdr
048600070504     c                   leavesr
048700070504     c                   endif
048800070504      *  deve essere numerico
048900070504     c                   eval      *in30 = *off
049000070508     c                   testn                   v1ccodaut            30
049100070504      *  controllo validità
049200070508     c                   if        *in30 and %subst(v1ccodaut:7:1) >= *zeros
049300070504     c                   eval      apdtip = 'A'
049400070508     c                   move      v1ccodaut     apdpdr
049500110308     c     kfiapd        chain     fiapd000
049600070504     c                   if        not %found(fiapd01l) or apdatb <> *blanks
049700070504     c                   eval      *in28 = *on
049800070504     c                   eval      *in45 = *on
049900070504     c                   eval      v1cmsg = msg(10)
050000070504     c                   leavesr
050100070504     c                   endif
050200070504     c                   eval      v1dpdr = apdrsc
050300070504     c                   endif
050400070504      *  deve essere in gestione all'utente
050500070504     c                   movel     apdpdr        w0030
050600070508     c     w0030         lookup    pog                                    30
050700070504     c                   if        not *in30
050800070504     c                   eval      *in28 = *on
050900070504     c                   eval      *in45 = *on
051000070504     c                   eval      v1cmsg = msg(11)
051100070504     c                   leavesr
051200070504     c                   endif
051300070215     c                   endif
051400070209
051500070209     c                   endsr
051600070504
051700070504      *------------------------------------------------------------------------*
051800070504      *? CARICO SQL
051900070504      *------------------------------------------------------------------------*
052000070504     c     sr_carsql     begsr
052100070507
052200070507     c                   clear                   wconta
052300070507
052400080327     c                   eval      stringasql = 'select fiplg00f.*, ' +
052500080327     c                                          'rrn(fiplg00f) ' +
052600110308     c                                          'from fiplg00f where'
052700070510      * selezionata la data
052800070508     c                   if        v1cdata <> *zeros
052900110308     c                   eval      stringasql += ' substr(plgdatora, 1, 8) +
053000110308     c                                           = ''' +
053100110308     c                                           v1cdatainv + ''''
053200070507     c                   eval      wconta = 1
053300070507     c                   endif
053400070507
053500070510      * selezionata l'ora
053600070507     c                   if        v1cora <> *zeros
053700070508     c                   if        wconta > *zeros
053800110308     c                   eval      stringasql += ' and'
053900070508     c                   endif
054000070508     c                   move      v1cora        wora
054100110308     c                   eval      stringasql += ' substr(plgdatora, 9, 6) +
054200110308     c                                           >= ' + wora
054300070507     c                   eval      wconta = 1
054400070507     c                   endif
054500070514
054600070514      * selezionato il tipo record
054700070514     c                   if        v1ctiporcd <> *blanks
054800070514     c                   if        wconta > *zeros
054900110308     c                   eval      stringasql += ' and'
055000070514     c                   endif
055100110308     c                   eval      stringasql += ' plgtiporcd = ''' +
055200110308     c                                           v1ctiporcd + ''''
055300070514     c                   eval      wconta = 1
055400070514     c                   endif
055500070507
055600070510      * selezionata la filiale
055700070508     c                   if        v1cfgs <> *zeros
055800070508     c                   if        wconta > *zeros
055900110308     c                   eval      stringasql += ' and'
056000070508     c                   endif
056100070508     c                   move      v1cfgs        wfgs
056200110308     c                   eval      stringasql += ' plgfgs = ' + wfgs
056300070507     c                   eval      wconta = 1
056400070507     c                   endif
056500070507
056600070510      * selezionata la distinta
056700070507     c                   if        v1cndc <> *blanks
056800070508     c                   if        wconta > *zeros
056900110308     c                   eval      stringasql += ' and'
057000070508     c                   endif
057100110308     c                   eval      stringasql += ' digits(plgndc) = ' +
057200110308     c                                           v1cndc
057300070507     c                   eval      wconta = 1
057400070507     c                   endif
057500070507
057600070510      * selezionato l'autotrasportatore
057700070508     c                   if        v1ccodaut <> *blanks
057800070508     c                   if        wconta > *zeros
057900110308     c                   eval      stringasql += ' and'
058000070508     c                   endif
058100110308     c                   eval      stringasql += ' digits(plgcodaut) = ' +
058200070508     c                                           v1ccodaut
058300070507     c                   eval      wconta = 1
058400070507     c                   endif
058500070507
058600070510      * selezionato solo errori
058700070507     c                   if        v1cerr = 'S'
058800070508     c                   if        wconta > *zeros
058900110308     c                   eval      stringasql += ' and'
059000070508     c                   endif
059100110308     c                   eval      stringasql += ' plgerr <> '' '''
059200110308     c                   eval      wconta = 1
059300070507     c                   endif
059400110311
059500110311      /free
059600110311
059700110311         // -?Selezionato Id Dispositivo?
059800110311         if  V1CidDisp <> *blank;
059900110311           if  wConta > *zero;
060000110311             StringaSql += ' and';
060100110311           endif;
060200110311           StringaSql += ' plgIdDisp = ''' + V1CidDisp + '''';
060300110311         endif;
060400110311
060500110311      /end-free
060600110307
060700080328      * selezionato id documento
060800110307if  1c                   if        v1ciddoc   <> *blanks
060900110307if  Ac                   IF        V1CtipoRcd <> *blanks
061000080328     c                   clear                   pini
061100080328     c                   monitor
061200080328     c                   move      d§prcipd      pini              3 0
061300080328     c                   on-error
061400080328     c                   endmon
061500080328     c                   clear                   lun
061600080328     c                   monitor
061700080328     c                   move      d§prcld       lun               2 0
061800080328     c                   on-error
061900080328     c                   endmon
062000110307if  2c                   if        pini <> 0  and lun <> 0
062100110307if  3c                   if        wconta > *zeros
062200110308     c                   eval      stringasql += ' and'
062300110307e   3c                   endif
062400110308     c                   eval      stringasql += ' substr(plgdati, ' +
062500110308     c                                           d§prcipd + ', ' +
062600110308     c                                           d§prcld + ') = ''' +
062700110308     c                                           v1ciddoc + ''''
062800110307e   2c                   endif
062900110307x   Ac                   ELSE
063000110308     c                   exsr      sr_SelectIdDoc
063100110307e   Ac                   ENDIF
063200110308     c                   eval      wConta = 1
063300110307e   1c                   endif
063400070507
063500110308     c                   eval      stringasql += ' order by plgdatora +
063600110308     c                                             for read only'
063700070507
063800070507      * preparo il cursore
063900070507     c/exec sql
064000070507     c+ prepare s1 from :stringasql
064100070507     c/end-exec
064200070507
064300070507      * dichiaro il cursore
064400070507     c/exec sql
064500070507     c+ declare plg cursor for s1
064600070507     c/end-exec
064700070504
064800070504     c                   endsr
064900110308      /free
065000110308
065100110308       //--------------------------------------------------------------
065200110308       //?Selezione (generica) per "ID documento"                      ?
065300110308       //--------------------------------------------------------------
065400110308       BEGSR  sr_SelectIdDoc;
065900110308
066000110308         if  wConta > *zero;
066100110308           StringaSql += ' and';
066200110308         endif;
066300110308
066400110308         clear  xx;
066500110308         clear  k05tntbe01;
066600110308         k_TBEcod = 'PRC';
066700110308         setll  %kds(k05tntbe01 : 1)  TNTBE000;
066800110308         reade  %kds(k05tntbe01 : 1)  TNTBE000;
066900110308
067000110308         DoW  Not %eof(TNTBE01L);
067100110308
067200110308           dPRC = TBEuni;
067300110308
067400110308           clear  PIni;
067500110308           clear  Lun;
067600110308           monitor;
067700110308             PIni = %int(d§PRCipd);
067800110308           on-error;
067900110308           endmon;
068000110308           monitor;
068100110308             Lun  = %int(d§PRCld);
068200110308           on-error;
068300110308           endmon;
068400110308
068500110308           if  PIni <> *zero  and  Lun <> *zero;
068600110308
068700110308             if  xx = *zero;
068800110308               StringaSql += ' (';
068900110308             else;
069000110308               StringaSql += ' or';
069100110308             endif;
069200110308
069300110308             StringaSql += ' (PLGtipoRcd = ''' + %trimr(TBEke1) +
069400110308                           ''' and substr(PLGdati, ' + d§PRCipd +
069500110308                                                ', ' + d§PRCld +
069600110308                           ') = ''' + V1CidDoc + ''')';
069700110308
069800110308             xx += 1;
069900110308
070000110308           endif;
070100110308
070200110308           reade  %kds(k05tntbe01 : 1)  TNTBE000;
070300110308
070400110308         EndDo;
070500110308
070600110309         if  xx > *zero;
070700110309           StringaSql += ' )';
070800110309         endif;
070900110308
071000110308       ENDSR;
071100110308
071200110308      /end-free
071300070215
071400070215      *------------------------------------------------------------------------*
071500070507      *? PULISCO IL SUBFILE
071600070215      *------------------------------------------------------------------------*
071700070508     c     sr_pulsfl     begsr
071800070215
071900070504     c                   clear                   nrr
072000070216
072100070216     c                   eval      *in20 = *off
072200070216     c                   eval      *in21 = *off
072300070504     c                   write     lvp2c01
072400070216     c                   eval      *in21 = *on
072500070216     c                   eval      *in20 = *on
072600070216
072700070504     c                   eval      recsf = 1
072800070216     c                   eval      *in31 = *off
072900070215
073000070215     c                   endsr
073100070216
073200070216      *------------------------------------------------------------------------*
073300070507      *? CARICO IL SUBFILE --> LEGGO SQL
073400070216      *------------------------------------------------------------------------*
073500070504     c     sr_carsfl     begsr
073600070216
073700070507      * apertura cursore
073800070507     c/exec sql
073900070507     c+ open plg
074000070507     c/end-exec
074100070507
074200070507      * lettura cursore
074300070507     c                   do        *hival
074400070507     c/exec sql
074500080327     c+ fetch next from plg into :fiplgds, :nrrplg
074600070507     c/end-exec
074700070507     c                   select
074800070507      * fine file
074900070507     c                   when      sqlcod = 100
075000070507     c                   leave
075100070507      * errore
075200070507     c                   when      sqlcod < 0
075300070507     c                   eval      *in28 = *on
075400070507     c                   eval      v1cmsg = msg(12)
075500070507     c                   leave
075600070507     c                   other
075700110324     c                   if        nrr < c_MaxRec
075800070508     c                   exsr      sr_wrtsfl
075900110324     c                   else
076000110324     c                   eval      *in28 = *on
076100110324     c                   eval      v1cmsg = msg(16)
076200110324     c                   leave
076300110324     c                   endif
076400070507     c                   endsl
076500070507     c                   enddo
076600110324     c                   if        sqlcod = 100
076700110324     c                   eval      *in31 = *on
076800110324     c                   endif
076900070508
077000070507      * chisura cursore
077100070507     c/exec sql
077200070508     c+ close plg
077300070507     c/end-exec
077400070507
077500070507     c                   endsr
077600070508
077700070508      *------------------------------------------------------------------------*
077800070508      *? SCRIVO LA RIGA DEL SUBFILE A TOTALE
077900070508      *------------------------------------------------------------------------*
078000070508     c     sr_wrtsfl     begsr
078100070219
078200070508     c                   clear                   v1sopz
078300070514     c                   eval      v1stiporcd = plgtiporcd
078400070514     c                   eval      v1stipappl= plgtipappl
078500070511     c                   if        plgdatora <> *blanks
078600070508     c                   eval      wdatainv = %subst(plgdatora:1:8)
078700070508     c                   move      waai          waa
078800070508     c                   move      wmmi          wmm
078900070508     c                   move      wggi          wgg
079000070508     c                   move      wdata         v1sdata
079100070508     c                   eval      v1sora = %dec(%subst(plgdatora:9:6):6:0)
079200070511     c                   else
079300070511     c                   clear                   v1sdata
079400070511     c                   clear                   v1sora
079500070511     c                   endif
079600070508     c                   eval      v1scodaut = plgcodaut
079700070508     c                   eval      v1sfgs = plgfgs
079800070508     c                   eval      v1sndc = plgndc
079900070508     c                   eval      v1siddoc = plgiddoc
080000070508     c                   eval      v1serr = plgerr
080100070220      * imposto i campi hidden
080200070508     c                   eval      v1sprfc = plgprfc
080300070508     c                   eval      v1siddisp = plgiddisp
080400070508     c                   eval      v1sdati= plgdati
080500070508     c                   eval      v1smsg= plgmsg
080600070219      * scrivo il subfile a totale
080700070508     c                   add       1             nrr
080800070223      * se primo rcd che scrivo accendo indicatore di PC
080900070508     c                   if        nrr = 1
081000070514     c                   eval      *in73 = *on
081100070223     c                   else
081200070514     c                   eval      *in73 = *off
081300070223     c                   endif
081400070223
081500070508     c                   write     lvp2s01
081600070219
081700070219     c                   endsr
081800070223
081900070223      *------------------------------------------------------------------------*
082000070223      *? EMETTO LA VIDEATA VUOTA
082100070223      *------------------------------------------------------------------------*
082200070223     c     sr_emid02     begsr
082300070223
082400070223     c                   do        *hival
082500070223
082600070508     c                   write     lvp2t01
082700070508     c                   exfmt     lvp2d02
082800070223
082900070223     c                   eval      *in28 = *off
083000070223
083100070223      * f3=fine
083200070223     c                   if        *inkc
083300070223     c                   goto      fine
083400070223     c                   endif
083500070223
083600070223      * f12=ritorno
083700070223     c                   if        *inkl
083800070223     c                   goto      emid01
083900070223     c                   endif
084000070223
084100070223     c                   enddo
084200070223
084300070223     c                   endsr
084400070216
084500070216      *------------------------------------------------------------------------*
084600070216      *? EMETTO IL SUBFILE A TOTALE
084700070216      *------------------------------------------------------------------------*
084800070508     c     sr_emisfl     begsr
084900070219
085000070219     c                   do        *hival
085100070219
085200070508     c                   if        recsf > nrr
085300070508     c                   eval      recsf = 1
085400070216     c                   endif
085500070216
085600070508     c                   write     lvp2t01
085700070508     c                   write     lvp2z01
085800070508     c                   exfmt     lvp2c01
085900070216
086000070216     c                   eval      *in28 = *off
086100070508     c                   clear                   v1cmsg
086200070216
086300070216      * f3=fine
086400070216     c                   if        *inkc
086500070216     c                   goto      fine
086600070216     c                   endif
086700070216
086800070216      * f12=ritorno
086900070216     c                   if        *inkl
087000070216     c                   goto      emid01
087100070216     c                   endif
087200070216
087300070216      * controllo i dati immessi nel subfile
087400070219      * solo se ho caricato il subfile
087500070508     c                   if        nrr > *zeros
087600070508     c                   exsr      sr_contrsfl
087700070216     c                   if        *in28
087800070216     c                   iter
087900070216     c                   endif
088000070219     c                   endif
088100070216
088200070216     c                   enddo
088300070216
088400070216     c                   endsr
088500070216
088600070216      *------------------------------------------------------------------------*
088700070508      *? CONTROLLA I DATI DEL SUBFILE
088800070216      *------------------------------------------------------------------------*
088900070508     c     sr_contrsfl   begsr
089000070216
089100070508     c                   clear                   nrr
089200070216     c                   do        *hival
089300070508     c                   add       1             nrr
089400070508     c     nrr           chain     lvp2s01                            32
089500070216      * esco se fine sfl
089600070216     c                   if        *in32
089700070216     c                   leave
089800070216     c                   endif
089900070216
090000070220      * opzione V --> visualizza
090100070511     c                   if        v1sopz = '5'
090200070220     c                   exsr      sr_visriga
090300070508     c                   eval      recsf = nrr
090400070510     c                   eval      v1sopz = *blanks
090500070508     c                   clear                   v1sopz
090600070514     c                   eval      *in73 = *on
090700070508     c                   update    lvp2s01
090800070511     c                   if        *inkl
090900070511     c                   goto      emisfl
091000070511     c                   endif
091100070508     c                   else
091200070223
091300070223      * spengo l'indicatore di PC
091400070514     c                   eval      *in73 = *off
091500070510     c                   clear                   v1sopz
091600070508     c                   update    lvp2s01
091700070508     c                   endif
091800070223
091900070216     c                   enddo
092000070216
092100070216     c                   endsr
092200070508
092300070508      *------------------------------------------------------------------------*
092400070508      *? VISUALIZZO RIGA LOG
092500070508      *------------------------------------------------------------------------*
092600070508     c     sr_visriga    begsr
092700070510
092800070510     c                   exsr      sr_pulvid03
092900070508      * tipo record
093000070514     c                   eval      v3ctiporcd = v1stiporcd
093100070514     c                   clear                   v3dtiporcd
093200070508     c                   clear                   tibs02ds
093300070508     c                   eval      t02mod = 'C'
093400070508     c                   eval      t02sif = knsif
093500070508     c                   eval      t02cod = 'PRC'
093600070514     c                   eval      t02ke1 = v3ctiporcd
093700070508     c                   call      'TIBS02R'
093800070508     c                   parm                    kpjba
093900070508     c                   parm                    tibs02ds
094000070508     c                   if        t02err = *blanks
094100070508     c                   eval      dprc = t02uni
094200080327     c                   endif
094300080327     c* per consegne chiamo pgm esterni per visualizzare il record
094400080327     c                   if        D§PRCCR = 'C'
094500080327     c                   exsr      sr_call
094600080327      * f3=fine
094700080327     c                   if        esitol = '1'
094800080327     c                   goto      fine
094900080327     c                   endif
095000080327
095100080327      * f12=ritorno
095200080327     c                   if        esitol ='2'
095300080327     c                   leavesr
095400080327     c                   endif
095500080327      * f12=ritorno
095600080327     c                   if        esitol <>' '
095700080327     c                   seton                                        28
095800080327     c                   movel     msg(14)       v1cmsg
095900080327     c                   leavesr
096000080327     c                   endif
096100080327     c                   end
096200070514     c                   eval      v3dtiporcd = d§prcdes
096300070508      * tipo applicazione
096400070514     c                   eval      v3ctipappl = v1stipappl
096500070514     c                   clear                   v3dtipappl
096600070514     c                   if        v3ctipappl = 'R'
096700070514     c                   eval      v3dtipappl = 'Ritiri'
096800070508     c                   endif
096900070508      * profilo
097000070508     c                   eval      v3cprfc = v1sprfc
097100070508      * id dispositivo
097200070508     c                   eval      v3ciddisp = v1siddisp
097300070508      * data/ora ricezione
097400070508     c                   eval      v3cdata = v1sdata
097500070508     c                   eval      v3cora = v1sora
097600070508      * id documento
097700070508     c                   eval      v3ciddoc = v1siddoc
097800070508      * autotrasportatore
097900070508     c                   eval      v3ccodaut = v1scodaut
098000070508     c                   clear                   v3dcodaut
098100070508     c                   eval      apdtip = 'A'
098200070508     c                   eval      apdpdr = v3ccodaut
098300110308     c     kfiapd        chain     fiapd000
098400070508     c                   if        %found(fiapd01l) and apdatb = *blanks
098500070508     c                   eval      v3dcodaut = apdrsc
098600070508     c                   endif
098700070508      * filiale distinta
098800070508     c                   eval      v3cfgs = v1sfgs
098900070508     c                   clear                   v3dfgs
099000110308     c     v3cfgs        chain     azorg
099100070508     c                   if        %found(azorg01l) and orgfva = *blanks
099200070508     c                   eval      v3dfgs = orgdes
099300070508     c                   endif
099400070508      * numero distinta
099500070508     c                   eval      v3cndc = v1sndc
099600070508     c                   clear                   v3dndc
099700070508     c                   eval      dstnpg = 4
099800070508     c                   eval      dstfgs = v3cfgs
099900070508     c                   eval      dstnfv = v3cndc
100000110308     c     kfidst        chain     fidst000
100100070508     c                   if        %found(fidst01l)
100200070508     c                   eval      apdtip = 'A'
100300070508     c                   eval      apdpdr = dstpdr
100400070508     c                   clear                   apdrsc
100500110308     c     kfiapd        chain     fiapd000
100600070508     c                   if        %found(fiapd01l)
100700110308     c                   eval      v3dndc = 'Del' +
100800110308     c                             c_ColorTrq +
100900070508     c                             %subst(%editc(%dec(dstdfv):'X'):7:2) + '/' +
101000070508     c                             %subst(%editc(%dec(dstdfv):'X'):5:2) + '/' +
101100070508     c                             %subst(%editc(%dec(dstdfv):'X'):1:4) + ' ' +
101200110308     c                             %editc(%dec(dstpdr):'X') +
101300110308     c                             c_ColorBlu +
101400070508     c                             %subst(apdrsc:1:17)
101500070508     c                   endif
101600070508     c                   endif
101700070508      * messaggio errore
101800070508     c                   eval      v3cmsg = v1smsg
101900070508      * spengo indicatori per visualizzazione dei vari tipi record
102000070508     c                   eval      *in50 = *off
102100070508     c                   eval      *in51 = *off
102200070508     c                   eval      *in52 = *off
102300070514     c                   eval      *in53 = *off
102301140212     c                   eval      *in54 = *off
102400070508      * spengo indicatori per visualizzazione i due diversi documenti
102500070508     c                   eval      *in60 = *off
102600070514      * spengo indicatori per visualizzazione ORM manuale
102700070514     c                   eval      *in62 = *off
102800070508      * devo visualizzare cose diverse per i vari tipi record
102900070508     c                   select
103000070508
103100070508      * tipo record OK  --> conferma dati
103200070514     c                   when      v1stiporcd = 'OK '
103300070508     c                   eval      *in50 = *on
103400070508     c                   eval      fiprcokds = v1sdati
103500070508      * filiale distinta
103600070514     c                   eval      v3fgs = §okfgs
103700070508      * numero distinta
103800070514     c                   eval      v3ndc = §okndc
103900070514      * data/ora evento
104000070531     c                   eval      v3data = %subst(§okdatora:1:8)
104100070514     c                   eval      v3ora = %subst(§okdatora:9:6)
104200070508      * tipo iddocumento e iddocumento
104300070514     c                   select
104400070514     c                   when      §oktpiddoc = 'R'
104500070508     c                   eval      v3doktpdoc = 'ORM'
104600070514     c                   eval      idormds = §okiddoc
104700070508     c                   eval      *in60 = *on
104800070514     c                   other
104900070514     c                   eval      *in60 = *off
105000070514     c                   eval      v3doktpdoc = §oktpiddoc
105100070514     c                   endsl
105200070508      * richiesta assistenza
105201140217     c                   clear                   v3resnote
105300070514     c                   select
105400070514     c                   when      §okflgass = 'S'
105500070508     c                   eval      v3okflgass = 'SI'
105600070514     c                   when      §okflgass = *blanks
105700070508     c                   eval      v3okflgass = 'NO'
105800070514     c                   other
105900070514     c                   eval      v3okflgass = §okflgass
105901140217     c                   movel     'RAS'         tbecod
105902140217     c                   movel(p)  §okflgass     tbeke1
105903140217     c     ktbe          chain     tntbe01l
105904140217     c                   if        %found(tntbe01l)
105905140217     c                   movel     tbeuni        v3resnote
105906140217     c                   else
105907140217     c                   move      *all'?'       v3resnote
105908140217     c                   endif
106000070514     c                   endsl
106100080221      * id riga
106200080221     c                   eval      v3okidrow = §okidrow
106201140217     c                   eval      v3drescmr = §oknotass
106300070508
106301140212      * tipo record ROR --> ora stimata ritiro
106302140212     c                   when      v1stiporcd = 'ROR'
106303140212     c                   eval      *in50 = *on
106304140212     c                   eval      *in54 = *on
106305140212     c                   eval      fiprcrords = v1sdati
106306140212      * filiale distinta
106307140212     c                   eval      v3fgs = §rorfgs
106308140212      * numero distinta
106309140212     c                   eval      v3ndc = §rorndc
106310140212      * data/ora evento
106311140212     c                   eval      v3data = %subst(§rordatora:1:8)
106312140212     c                   eval      v3ora = %subst(§rordatora:9:6)
106313140212      * tipo iddocumento e iddocumento
106314140217     c                   eval      v3doktpdoc = 'ORM'
106316140212     c                   eval      idormds = §roriddoc
106331140212      * ora stimata
106332140212     c                   movel     §rororasti    v3rororast
106333140212     c                   movel     §rorsort      v3rorsort
106334140212
106400070508      * tipo record RES --> esiti ritiri
106500070514     c                   when      v1stiporcd = 'RES'
106600070508     c                   eval      *in51 = *on
106700070508     c                   eval      fiprcresds = v1sdati
106800070508      * filiale distinta
106900070514     c                   eval      v3fgs = §resfgs
107000070508      * numero distinta
107100070514     c                   eval      v3ndc = §resndc
107200070514      * data/ora evento
107300070531     c                   eval      v3data = %subst(§resdatora:1:8)
107400070514     c                   eval      v3ora = %subst(§resdatora:9:6)
107500070514      * numero ORM
107600070514     c                   move      §respoe       v3respoe
107700070514     c                   move      §resnsr       v3resnsr
107800070514     c                   move      §resnor       v3resnor
107900070514     c                   move      §resnrv       v3resnrv
108000070508      * colli/bancali
108100070514     c                   eval      v3resncl = §resncl
108200070514     c                   eval      v3resbnc = §resbnc
108300070508      * note
108400070508     c                   eval      v3resnote = §resnote
108500070508      * causale
108600081106      * no AAA (orm manuale annullato)
108700081106     c                   if        §rescmr <> 'AAA'
108800070508     c                   eval      v3rescmr = §rescmr
108900070518     c                   clear                   v3drescmr
109000070518     c                   clear                   dcmr
109100070508     c                   clear                   tibs02ds
109200070508     c                   eval      t02mod = 'C'
109300070508     c                   eval      t02sif = knsif
109400070508     c                   eval      t02cod = 'CMR'
109500070508     c                   eval      t02ke1 = v3rescmr
109600070508     c                   call      'TIBS02R'
109700070508     c                   parm                    kpjba
109800070508     c                   parm                    tibs02ds
109900070508     c                   if        t02err = *blanks
110000070508     c                   eval      dcmr = t02uni
110100070508     c                   endif
110200070508     c                   eval      v3drescmr = d§cmrdes
110300081106     c                   else
110400081106     c                   eval      v3cmsg = 'ORM MANUALE ANNULLATO'
110500081106     c                   endif
110600070508      * data/ora inserimento
110700070531     c                   eval      v3resdatai = %subst(§resdtorin:1:8)
110800070518     c                   eval      v3resorai = %subst(§resdtorin:9:6)
110900070508      * inserito da spc
111000070514     c                   select
111100070514     c                   when      §resflgspc = 'S'
111200070514     c                   eval      v3resflspc = 'SPC'
111300070514     c                   when      §resflgspc = *blanks
111400070514     c                   eval      v3resflspc = 'AUT'
111500070514     c                   other
111600070514     c                   eval      v3resflspc = §resflgspc
111700070514     c                   endsl
111800070508      * ORM manuale
111900070514     c                   eval      v3resrsr = §resrsr
112000070514     c                   eval      v3resinr = §resinr
112100070514     c                   eval      v3resrfa = §resrfa
112200070514      * visualizzo ORM manuale o n. ORM valido
112300070514     c                   if        §resrsr <> *blanks and
112400080407     c                             %subst(v1sdati:10:8) = *all'0'
112500070514     c                   eval      *in62 = *on
112600070508     c                   else
112700070514     c                   eval      *in62 = *off
112800070508     c                   endif
112900080214      * codice fiscale
113000080214     c                   eval      v3rescdf = §rescodfis
113100080214      * partita iva
113200080214     c                   eval      v3resiso = §resisoiva
113300080214     c                   eval      v3resiva = §respiva
113400120515      * Coordinate GEO
113500120515     c                   IF        §RESxlongi > *zeros
113600120515     c                   eval      v3reslongi = %dec(§resxlongi:8:0)/1000000
113700120515     c                   ELSE
113800120515     c                   clear                   v3reslongi
113900120515     c                   ENDIF
114000120515     c                   IF        §RESylati > *zeros
114100120515     c                   eval      v3reslati  = %dec(§resylati:8:0)/1000000
114200120515     c                   ELSE
114300120515     c                   clear                   v3reslati
114400120515     c                   ENDIF
114500070508
114600070508      * tipo record RCH --> chiusura distinta
114700070514     c                   when      v1stiporcd = 'RCH'
114800070508     c                   eval      *in52 = *on
114900070508     c                   eval      fiprcrchds = v1sdati
115000070508      * filiale distinta
115100070514     c                   eval      v3fgs = §rchfgs
115200070508      * numero distinta
115300070514     c                   eval      v3ndc = §rchndc
115400070514      * data/ora chiusura (evento)
115500070531     c                   eval      v3data = %subst(§rchdatora:1:8)
115600070514     c                   eval      v3ora = %subst(§rchdatora:9:6)
115700070508      * colli/bancali
115800070514     c                   eval      v3rchncl = §rchncl
115900070514     c                   eval      v3rchbnc = §rchbnc
116000080707      * codice SPC
116100080707     c                   eval      v3rchspc = §rchcodspc
116200070514
116300070514      * tipo record     --> altri tipi record
116400070514     c                   other
116500070514     c                   eval      *in53 = *on
116600070508     c                   endsl
116700070508
116800070508     c                   write     lvp2t01
116900070508     c                   exfmt     lvp2d03
117000070508
117100070508      * f3=fine
117200070508     c                   if        *inkc
117300070508     c                   goto      fine
117400070508     c                   endif
117500070508
117600070508      * f12=ritorno
117700070508     c                   if        *inkl
117800070511     c                   leavesr
117900070508     c                   endif
118000070508
118100070508     c                   endsr
118200070510
118300070510      *------------------------------------------------------------------------*
118400080327      *? chiamata a pgm esterni per visualizzare record log consegne
118500070510      *------------------------------------------------------------------------*
118600080327     c     sr_call       begsr
118700080520     c                   clear                   esitol
118800070510
118900080327     c* lancio
119000080327     c                   call      'FNLVP3C'
119100080327     c                   parm                    kpjba
119200080327     c                   parm                    nrrplg
119300080327     c                   parm                    esitol            1
119400080327
119500080327     c                   endsr
119600080327
119700080327      *------------------------------------------------------------------------*
119800080327      *? PULIZIA DELLA VIDEATA  ?
119900080327      *------------------------------------------------------------------------*
120000080327     c     sr_pulvid03   begsr
120100080327
120200070514     c                   clear                   v3ctiporcd
120300070514     c                   clear                   v3dtiporcd
120400070514     c                   clear                   v3ctipappl
120500070514     c                   clear                   v3dtipappl
120600070510     c                   clear                   v3cprfc
120700070510     c                   clear                   v3ciddisp
120800070510     c                   clear                   v3cdata
120900070510     c                   clear                   v3cora
121000070510     c                   clear                   v3ciddoc
121100070510     c                   clear                   v3ccodaut
121200070510     c                   clear                   v3dcodaut
121300070510     c                   clear                   v3cfgs
121400070510     c                   clear                   v3dfgs
121500070510     c                   clear                   v3cndc
121600070510     c                   clear                   v3dndc
121700070510     c                   clear                   v3cmsg
121800070510
121900070514     c                   clear                   v3fgs
122000070514     c                   clear                   v3ndc
122100070514     c                   clear                   v3data
122200070514     c                   clear                   v3ora
122300070514
122400070510     c                   clear                   v3doktpdoc
122500070514     c                   clear                   idormds
122600070514     c                   clear                   idbolds
122700070510     c                   clear                   v3okflgass
122800070510
122900070510     c                   clear                   v3respoe
123000070510     c                   clear                   v3resnsr
123100070510     c                   clear                   v3resnor
123200070510     c                   clear                   v3resnrv
123300070510     c                   clear                   v3resncl
123400070510     c                   clear                   v3resbnc
123500070510     c                   clear                   v3resnote
123600070510     c                   clear                   v3rescmr
123700070510     c                   clear                   v3drescmr
123800070510     c                   clear                   v3resdatai
123900070510     c                   clear                   v3resorai
124000070510     c                   clear                   v3resflspc
124100070510     c                   clear                   v3resrsr
124200070510     c                   clear                   v3resrfa
124300070510     c                   clear                   v3resinr
124400070510
124500080707     c                   clear                   v3rchspc
124600070510     c                   clear                   v3rchncl
124700070510     c                   clear                   v3rchbnc
124800070510
124900070510     c                   endsr
125000061227
125100070504      *------------------------------------------------------------------------*
125200070504      *? ROUTINE INIZIALE
125300070504      *------------------------------------------------------------------------*
125400070504     c     *inzsr        begsr
125500070504
125600070504     c     *entry        plist
125700070504     c                   parm                    kpjba
125800110311     c                   clear                   Richiamato        1
125900110311     c                   if        kpjbu <> *blanks
126000080331     c                   movel     kpjbu         fnlvp2ds
126100110311     c                   eval      Richiamato = *on
126200110311     c                   endif
126300110119      * -?Verifica del sistema AS/400 corrente?
126400110119     c                   if        UBRTVNETA_Rtv(currSysNeta) <> *zero
126500110119     c                   eval      *inlr = *on
126600110119     c                   return
126700110119     c                   endif
126800110119      * -?Impostazione libreria archivi di filiale?
126900110119     c                   if        %subst ( currSysNeta : 1 : 6 ) = 'SETRAS'
127000110119     c                   eval      wLibFIDST = 'FILTRA201/FIDST01L'
127100110119     c                   else
127200110119     c                   eval      wLibFIDST = 'FILTRAPRD/FIDST01L'
127300110119     c                   endif
127400110119      * -?Apertura archivi di filiale?
127500110119     c                   open      FIDST01L
127600110119      *
127700070504     c     *dtaara       define    §azute        azuteds
127800070504     c     *dtaara       define    §datiute      ddatiute
127900070504     c                   in(e)     *dtaara
128000070504     c                   if        %error  or rsut = *blanks
128100070504     c                   clear                   tibs34ds
128200070504     c                   call      'TIBS34R'
128300070504     c                   parm                    tibs34ds
128400070504     c                   in        *dtaara
128500070504     c                   endif
128600070504
128700070504      * carico schiera filiali gestite
128800070504     c                   clear                   trul31ds
128900070504     c                   eval      i31abi = uteaut
129000070504     c                   eval      i31cdi = dutdis
129100070504     c                   eval      i31car = dutare
129200070504     c                   eval      i31cpo = dutpou
129300070504     c                   call      'TRUL31R'
129400070504     c                   parm                    kpjba
129500070504     c                   parm                    trul31ds
129600070504      * se non carica nessuna filiale errore utente non abilitato
129700070504     c                   if        o31pog <= *zeros
129800070504     c                   eval      *in28 = *on
129900070504     c                   eval      v1cmsg = msg(01)
130000070504     c                   endif
130100070504
130200070504      * controllo se PDA attivo sulla filiale utente
130300070504     c                   clear                   og148
130400110308     c     dutpou        chain     azorg
130500070504     c                   if        %found(azorg01l) and orgfva = *blanks
130600070504     c                   eval      og148 = orgde8
130700070504     c                   endif
130800080213     c                   if        §ogpdaorm = *blanks
130900110119     c                             and  DUTpou <> 046
131000070504     c                   eval      *in28 = *on
131100070504     c                   eval      *in40 = *on
131200070504     c                   eval      v1cmsg = msg(02)
131300070504     c                   endif
131400070531
131500070531      * cerco i gg. di pulizia del prospetto ORM
131600070531     c                   clear                   ds5a2
131700070531     c                   eval      tblkut = 1
131800070531     c                   eval      tblcod = '5A'
131900070531     c                   eval      tblkey = '2       '
132000110308     c     ktabel        chain     tabel
132100070531     c                   if        %found(tabel00f) and tblflg = *blanks
132200070531     c                   eval      ds5a2 = tbluni
132300070531     c                   endif
132400070531      * calcolo data partenza per ricerca distinte
132500081117     c                   move      *date         dataiso
132600081117     c                   subdur    §5aporm:*d    dataiso
132700070531
132800070531     c     ktabel        klist
132900070531     c                   kfld                    tblkut
133000070531     c                   kfld                    tblcod
133100070531     c                   kfld                    tblkey
133200070504
133300070504     c     kfidst        klist
133400070504     c                   kfld                    dstnpg
133500070504     c                   kfld                    dstnfv
133600070508     c                   kfld                    dstfgs
133700070504
133800070209     c     kfiapd        klist
133900070214     c                   kfld                    apdtip
134000070214     c                   kfld                    apdpdr
134100070504
134101140217     c     ktbe          klist
134102140217     c                   kfld                    tbecod
134103140217     c                   kfld                    tbeke1
134200070504     c                   endsr
134300070504
134400070504** MSG  Lungh. 78                                                            *
134500070504Utente non abilitato alla funzione                                            01
134600070504Procedura PDA non attiva per la filiale distinta                              02
134700070504Effettuare almeno una scelta                                                  03
134800070504Data ricezione errata                                                         04
134900070504Ora ricezione errata                                                          05
135000070504Filiale distinta errata                                                       06
135100070504Filiale distinta non in gestione all'utente                                   07
135200070504Distinta errata                                                               08
135300070504Distinta non abilitata allo scarico dati su PDA                               09
135400070504Autotrasportatore errato                                                      10
135500070504Autotrasportatore non in gestione all'utente                                  11
135600070507Errore nella ricerca dei dati inviati da PDA                                  12
135700070514Tipo record errato                                                            13
135800080327Record errato                                                                 14
135900080328Id documento non previsto x il tipo record scelto                             15
136000110324SUPERATO IL NUMERO MASSIMO DI RECORD VISUALIZZABILI NEL SUBFILE (9999)        16
