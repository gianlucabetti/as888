000100080108     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200080108
000300080108      *---------------------------------------------------------------*
000400080108      *                                                               *
000500080108      *                 PARTENZA GEO ORM                              *
000600080108      *                                                               *
000700080108      *---------------------------------------------------------------*
000800080108
000900080108     ffior98d   cf   e             workstn sfile(or98s01:nrr1)
001000080108     fazorg01l  uf   e           k disk    usropn
001100080108     ftntbe01l  uf a e           k disk
001200080108
001300080108      *------------------------------------------------------------------------*
001400080108      * riepilogo indicatori
001500080108      *------------------------------------------------------------------------*
001600080108      * 01 - ambiente di prova
001700080129      * 03 - proteggo data partenza presunta
001800080108      * 04 - proteggo data partenza reale
001900080108      * 20 - gestione subfile
002000080108      * 21 - gestione subfile
002100080108      * 28 - messaggio errore
002200080108      * 31 - fine subfile
002300080108      * 40 - filiale
002400080108
002500080108      *------------------------------------------------------------------------*
002600080108      * campi di work
002700080108      *------------------------------------------------------------------------*
002800080108     d command         s             80
002900080108     d conta           s              1  0 inz
003000080108     d ktbecod         s                   like(tbecod)
003100080108     d ktbeke1         s                   like(tbeke1)
003200080108     d lung            s             15  5 inz(80)
003300080108     d nrr1            s                   like(recsf1)
003400080108     d sav_w1cfil      s                   like(w1cfil)
003500080108     d wdatageo        s              8  0 inz
003600080108     d wdataiso        s               d   datfmt(*ISO)
003700080108     d wfil            s              3
003800080129     d wora            s              6  0 inz
003900080108     d w0030           s                   like(orgfil)
004000080108     d w0080           s              8  0 inz
004100080108     d xx              s              2  0 inz
004200080108
004300080108      *------------------------------------------------------------------------*
004400080108      * schiere
004500080108      *------------------------------------------------------------------------*
004600080108     d cmd             s             80    dim(5)  ctdata perrcd(1)
004700080129     d skpog           s              3    dim(250) inz(*zeros)
004800080108
004900080108      *------------------------------------------------------------------------*
005000080108      * ds interne/esterne
005100080108      *------------------------------------------------------------------------*
005200080108     d wdatainv        ds
005300080108     d waainv                  1      4
005400080108     d wmminv                  5      6
005500080108     d wgginv                  7      8
005600080108
005700080108     d wdata           ds
005800080108     d wgg                     1      2
005900080108     d wmm                     3      4
006000080108     d waa                     5      8
006100080108
006200080108     d wlbdat          ds                  inz
006300080108     d  g08dat                 1      8  0
006400080108     d  g08inv                 9     16  0
006500080108     d  g08err                17     17
006600080108     d  g08tgi                18     22  0
006700080108
006800080108     d azuteds       e ds                  extname(azute00f)
006900080108     d ddatiute      e ds
007000080108     d dorm          e ds
007100080108     d fnlv55ds      e ds
007200080108     d kpjba         e ds
007300080108     d og147         e ds
007400080108     d tibs02ds      e ds
007500080108     d tibs34ds      e ds
007600080108     d trul06ds      e ds                  inz
007700080108     d  lin                    1     90    dim(30)
007800080129     d trul31ds      e ds
007900080108
008000080108      *------------------------------------------------------------------------*
008100080108
008200080108     c     inizio        tag
008300080129
008400080129     c                   exsr      sr_video0
008500080108
008600080108     c                   exsr      sr_carvideo
008700080108
008800080108     c                   exsr      sr_video1
008900080108
009000080108     c                   exsr      sr_video2
009100080108
009200080108     c     fine          tag
009300080108
009400080108     c                   eval      *inlr = *on
009500080129
009600080129      *------------------------------------------------------------------------*
009700080129      * gestione videata iniziale
009800080129      *------------------------------------------------------------------------*
009900080129     c     sr_video0     begsr
010000080129
010100080129     c                   do        *hival
010200080129     c                   exfmt     or98d00
010300080129     c                   eval      *in28 = *off
010400080129     c                   clear                   w1cmsg
010500080129
010600080129      * f3=fine
010700080129     c                   if        *inkc
010800080129     c                   goto      fine
010900080129     c                   endif
011000080129
011100080129      * controllo
011200080129     c                   exsr      sr_ctrd00
011300080129
011400080129     c                   if        *in28
011500080129     c                   iter
011600080129     c                   endif
011700080129
011800080129     c                   leave
011900080129
012000080129     c                   enddo
012100080129
012200080129     c                   endsr
012300080108
012400080108      *------------------------------------------------------------------------*
012500080108      * carico i dati nella prima videata
012600080108      *------------------------------------------------------------------------*
012700080108     c     sr_carvideo   begsr
012800080108
012900080108     c                   clear                   w1fase1
013000080108     c                   clear                   w1dataf1
013100080108     c                   clear                   w1data1
013200080108     c                   clear                   w1dup
013300080108     c                   clear                   w1fase2
013400080108     c                   clear                   w1dataf2
013500080108     c                   clear                   w1data2
013600080129     c                   setoff                                       0304
013700080129     c                   setoff                                       0506
013800080108
013900080108      * data fase 1 una tantum
014000080108     c                   if        d§ormfase1 > *zeros
014100080108     c                   eval      wdatainv = d§ormfase1
014200080108     c                   eval      waa = waainv
014300080108     c                   eval      wmm = wmminv
014400080108     c                   eval      wgg = wgginv
014500080129     c                   move      wdata         w1dataf1
014600080108     c                   endif
014700080108      * data fase 2 giornaliera
014800080108     c                   if        d§ormfase2 > *zeros
014900080108     c                   eval      wdatainv = d§ormfase2
015000080108     c                   eval      waa = waainv
015100080108     c                   eval      wmm = wmminv
015200080108     c                   eval      wgg = wgginv
015300080129     c                   move      wdata         w1dataf2
015400080108     c                   endif
015500080108
015600080129      * proteggo sempre la data partenza presunta
015700080114     c                   eval      *in03 = *on
015800080129      * proteggo sempre la data partenza reale
015900080129     c                   eval      *in04 = *on
016000080129
016100080129      * se già attivata la fase 2
016200080108     c                   if        d§ormfase2 > *zeros
016300080129      * imposto la data partenza presunta = data partenza reale + 1 gg
016400080108     c                   move      wdatageo      wdataiso
016500080108     c                   adddur    1:*d          wdataiso
016600080108     c                   move      wdataiso      w0080
016700080108     c                   move      w0080         wdatainv
016800080108     c                   eval      waa = waainv
016900080108     c                   eval      wmm = wmminv
017000080108     c                   eval      wgg = wgginv
017100080108     c                   move      wdata         w1data1
017200080108      * imposto la data partenza reale
017300080108     c                   move      wdatageo      wdatainv
017400080108     c                   eval      waa = waainv
017500080108     c                   eval      wmm = wmminv
017600080108     c                   eval      wgg = wgginv
017700080108     c                   move      wdata         w1data2
017800080129      * proteggo i campi delle scelte tanto la filiale è già attiva
017900080129     c                   eval      *in05 = *on
018000080129     c                   eval      *in06 = *on
018100080108     c                   endif
018200080129
018300080108      * se attivata solo la fase 1
018400080129     c                   if        d§ormfase1 > *zeros and
018500080129     c                             (d§ormfase2 = *zeros or d§ormfase2 = *blanks)
018600080129      * imposto la data partenza presunta
018700080108     c                   move      wdatageo      wdatainv
018800080108     c                   eval      waa = waainv
018900080108     c                   eval      wmm = wmminv
019000080108     c                   eval      wgg = wgginv
019100080108     c                   move      wdata         w1data1
019200080129      * proteggo i campi della scelte perchè si può attivare solo la fase 2
019300080129     c                   eval      *in05 = *on
019400080129      * imposto la data partenza reale
019500080129     c                   move      *date         wdatainv
019600080129     c                   eval      waa = waainv
019700080129     c                   eval      wmm = wmminv
019800080129     c                   eval      wgg = wgginv
019900080129     c                   move      wdata         w1data2
020000080108     c                   endif
020100080114
020200080129      * se non è stata attivata la fase 1 imposto oggi + 6 mesi
020300080129     c                   if        d§ormfase1 = *zeros or d§ormfase1 = *blanks
020400080114     c                   move      *date         wdataiso
020500080114     c                   adddur    6:*m          wdataiso
020600080114     c                   move      wdataiso      w0080
020700080114     c                   move      w0080         wdatainv
020800080114     c                   eval      waa = waainv
020900080114     c                   eval      wmm = wmminv
021000080114     c                   eval      wgg = wgginv
021100080114     c                   move      wdata         w1data1
021200080114     c                   endif
021300080129
021400080129      * imposto di default la duplica della pianificazione
021500080129     c                   eval      w1dup = 'S'
021600080108
021700080108     c                   eval      sav_w1cfil = w1cfil
021800080108
021900080108     c                   endsr
022000080108
022100080108      *------------------------------------------------------------------------*
022200080108      * gestione prima videata
022300080108      *------------------------------------------------------------------------*
022400080108     c     sr_video1     begsr
022500080108
022600080108     c                   do        *hival
022700080108     c                   exfmt     or98d01
022800080108     c                   eval      *in28 = *off
022900080108     c                   clear                   w1cmsg
023000080108
023100080108      * f3=fine
023200080108     c                   if        *inkc
023300080108     c                   goto      fine
023400080108     c                   endif
023500080129
023600080129      * f12=ritorno
023700080129     c                   if        *inkl
023800080129     c                   goto      inizio
023900080129     c                   endif
024000080108
024100080108      * controllo
024200080108     c                   exsr      sr_ctrd01
024300080108
024400080108     c                   if        *in28
024500080108     c                   iter
024600080108     c                   endif
024700080108
024800080108      * f6=conferma
024900080108     c                   if        *inkf
025000080108     c                   leave
025100080108     c                   endif
025200080108
025300080108     c                   enddo
025400080108
025500080108     c                   endsr
025600080108
025700080108      *------------------------------------------------------------------------*
025800080108      * gestione seconda videata
025900080108      *------------------------------------------------------------------------*
026000080108     c     sr_video2     begsr
026100080108
026200080108      * visualizzo le linee di arrivo da abilitare
026300080108     c                   exsr      sr_pulsfl
026400080108     c                   exsr      sr_carsfl
026500080108     c                   exsr      sr_emisfl
026600080108
026700080108     c                   endsr
026800080129
026900080129      *------------------------------------------------------------------------*
027000080129      * controllo la prima iniziale
027100080129      *------------------------------------------------------------------------*
027200080129     c     sr_ctrd00     begsr
027300080129
027400080129     c                   setoff                                       40
027500080129
027600080129      * la filiale inserita deve essere una filiale valida
027700080129     c                   clear                   w1dfil
027800080129     c                   clear                   og147
027900080129     c     w1cfil        chain(n)  azorg01l
028000080129     c                   if        not %found(azorg01l) or orgfva <> *blanks or
028100080129     c                             (orgfag <> 'A' and orgfag <> 'F')
028200080129     c                   eval      w1cmsg = 'Filiale errata'
028300080129     c                   eval      *in28 = *on
028400080129     c                   eval      *in40 = *on
028500080129     c                   leavesr
028600080129     c                   endif
028700080129
028800080129      * non deve essere una filiale gestita da altra filiale
028900080129     c                   clear                   fnlv55ds
029000080129     c                   movel     '6'           d55tpt
029100080129     c                   z-add     w1cfil        d55lin
029200080129     c                   z-add     *date         d55drf
029300080129     c                   call      'FNLV55R'
029400080129     c                   parm                    fnlv55ds
029500080129     c                   if        d55lin <> d55tfa
029600080129     c                   eval      w1cmsg = 'Attenzione!! Filiale gestita'
029700080129     c                   eval      *in28 = *on
029800080129     c                   eval      *in40 = *on
029900080129     c                   leavesr
030000080129     c                   endif
030100080129
030200080129      * per utente no EDP controllo che la filiale sia gestibile dall'utente
030300080129     c                   if        %subst(knmus:1:3) <> 'EDP'
030400080129     c                   movel     w1cfil        wfil
030500080129     c                   if        %lookup(wfil:skpog) = *zeros
030600080129     c                   eval      w1cmsg = 'Filiale non in gestione -
030700080129     c                             all''utente'
030800080129     c                   eval      *in28 = *on
030900080129     c                   eval      *in40 = *on
031000080129     c                   leavesr
031100080129     c                   endif
031200080129     c                   endif
031300080129
031400080129     c                   eval      w1dfil= orgdes
031500080129     c                   eval      og147 = orgde7
031600080129
031700080129      * data partenza
031800080129     c                   if        §ogddao > *zeros
031900080129     c                   move      §ogddao       wdatageo
032000080129     c                   else
032100080129     c                   clear                   wdatageo
032200080129     c                   endif
032300080129
032400080129      * aggancio la tabella ORM della filiale utente
032500080129     c                   clear                   dorm
032600080129     c                   clear                   tibs02ds
032700080129     c                   eval      t02mod = 'C'
032800080129     c                   eval      t02sif = knsif
032900080129     c                   eval      t02cod = 'ORM'
033000080129     c                   movel     w1cfil        t02ke1
033100080129     c                   call      'TIBS02R'
033200080129     c                   parm                    kpjba
033300080129     c                   parm                    tibs02ds
033400080129     c                   if        t02err = *blanks
033500080129     c                   eval      dorm = t02uni
033600080129     c                   endif
033700080129
033800080129      * controllo se filiale già attivata (tabella ORM)
033900080129     c                   if        d§ormfase1 <> *blanks and
034000080129     c                             d§ormfase2 <> *blanks
034100080129     c                   eval      w1cmsg = 'Filiale già attivata'
034200080129     c                   eval      *in28 = *on
034300080129     c                   eval      *in40 = *on
034400080129     c                   leavesr
034500080129     c                   endif
034600080129
034700080129      * controllo se filiale già attivata (organigramma)
034800080129     c                   if        §ogcgio = 'S' and
034900080129     c                             wdatageo > *zeros and wdatageo <= *date
035000080129     c                   eval      w1cmsg = 'Filiale già attivata'
035100080129     c                   eval      *in28 = *on
035200080129     c                   eval      *in40 = *on
035300080129     c                   leavesr
035400080129     c                   endif
035500080129
035600080129      * cerco le filiali gestite dalla filiale da elaborare
035700080129     c                   clear                   trul06ds
035800080129     c                   eval      d06cod = '£6'
035900080129     c                   movel     w1cfil        d06key
036000080129     c                   eval      d06tla = 'L'
036100080129     c                   eval      kpjbu = trul06ds
036200080129     c                   call      'TRUL06R'
036300080129     c                   parm                    kpjba
036400080129     c                   eval      trul06ds = kpjbu
036500080129
036600080129     c                   endsr
036700080108
036800080108      *------------------------------------------------------------------------*
036900080108      * controllo la prima videata
037000080108      *------------------------------------------------------------------------*
037100080108     c     sr_ctrd01     begsr
037200080129
037300080129     c                   setoff                                       4142
037400080108
037500080108      * posso richiedere una fase alla volta
037600080108     c                   if        w1fase1 = 'S' and w1fase2 = 'S'
037700080108     c                   eval      w1cmsg = 'Attivare una FASE alla volta'
037800080108     c                   eval      *in28 = *on
037900080129     c                   eval      *in41 = *on
038000080108     c                   leavesr
038100080108     c                   endif
038200080108
038300080108      * devo richiedere almeno una FASE
038400080108     c                   if        w1fase1 = *blanks and w1fase2 = *blanks
038500080108     c                   eval      w1cmsg = 'Scegliere la FASE da attivare'
038600080108     c                   eval      *in28 = *on
038700080129     c  n05              eval      *in41 = *on
038800080129     c   05              eval      *in42 = *on
038900080108     c                   leavesr
039000080108     c                   endif
039100080108
039200080108      * FASE 1 Una-Tantum
039300080108     c                   if        w1fase1 = 'S'
039400080108      * errore se già attivato (tabella ORM)
039500080108     c                   if        d§ormfase1 <> *blanks
039600080108     c                   eval      w1cmsg = 'FASE 1 Una-Tantum già attivata'
039700080108     c                   eval      *in28 = *on
039800080129     c                   eval      *in41 = *on
039900080108     c                   leavesr
040000080108     c                   endif
040100080108      * errore se già attivato (organigramma)
040200080108     c                   if        §ogcgio = 'S'
040300080108     c                   eval      w1cmsg = 'FASE 1 Una-Tantum già attivata'
040400080108     c                   eval      *in28 = *on
040500080129     c                   eval      *in41 = *on
040600080108     c                   leavesr
040700080108     c                   endif
040800080129     c                   endif
040900080108
041000080108      * FASE 2 Giornaliera
041100080108     c                   if        w1fase2 = 'S'
041200080108      * errore se non ho attivato già la fase 1
041300080108     c                   if        d§ormfase1 = *blanks
041400080108     c                   eval      w1cmsg =
041500080108     c                             'Prima attivare la FASE 1 Una-Tantum'
041600080108     c                   eval      *in28 = *on
041700080129     c                   eval      *in42 = *on
041800080108     c                   leavesr
041900080108     c                   endif
042000080108      * errore se già attivato (tabella ORM)
042100080108     c                   if        d§ormfase2 <> *blanks
042200080108     c                   eval      w1cmsg = 'FASE 2 giornaliera già attivata'
042300080108     c                   eval      *in28 = *on
042400080129     c                   eval      *in42 = *on
042500080108     c                   leavesr
042600080108     c                   endif
042700080108      * errore se già attivato (organigramma)
042800080108     c                   if        wdatageo <= *date
042900080108     c                   eval      w1cmsg = 'FASE 2 giornaliera già attivata'
043000080108     c                   eval      *in28 = *on
043100080129     c                   eval      *in42 = *on
043200080108     c                   leavesr
043300080108     c                   endif
043400080129      * cerco l'orario di sistema
043500080129     c                   time                    wora
043600080129      * errore se orario di attivazione non tra le 9:30 e le 11:30
043700080129     c                   if        wora < 093000 or wora >= 113100
043800080129     c                   eval      w1cmsg = 'FASE 2 da attivare tra le 9:30 -
043900080129     c                             e le 11:30'
044000080129     c                   eval      *in28 = *on
044100080129     c                   eval      *in42 = *on
044200080129     c                   leavesr
044300080129     c                   endif
044400080108     c                   endif
044500080108
044600080108     c                   endsr
044700080108
044800080108      *------------------------------------------------------------------------*
044900080108      * pulizia del subfile
045000080108      *------------------------------------------------------------------------*
045100080108     c     sr_pulsfl     begsr
045200080108
045300080108     c                   clear                   nrr1
045400080108     c                   eval      *in20 = *off
045500080108     c                   eval      *in21 = *off
045600080108     c                   write     or98c01
045700080108     c                   eval      *in21 = *on
045800080108     c                   eval      *in20 = *on
045900080108
046000080108     c                   eval      recsf1 = 1
046100080108     c                   eval      *in31 = *off
046200080108
046300080108     c                   endsr
046400080108
046500080108      *------------------------------------------------------------------------*
046600080108      * carico subfile con le filiali
046700080108      *------------------------------------------------------------------------*
046800080108     c     sr_carsfl     begsr
046900080108
047000080108      * leggo la schiera delle filiali
047100080108     c                   do        30            xx
047200080108     c                   if        lin(xx) = *blanks or lin(xx) = *zeros
047300080108     c                   iter
047400080108     c                   endif
047500080108     c                   move      lin(xx)       w0030
047600080108     c     w0030         chain(n)  azorg01l
047700080108     c                   eval      w1sfil = w0030
047800080108     c                   eval      w1sdfil = orgdes
047900080108     c                   eval      w1ssce = '1'
048000080108     c                   eval      nrr1 = xx
048100080108     c                   write     or98s01
048200080108     c                   enddo
048300080108
048400080108     c                   eval      *in31 = *on
048500080108
048600080108      * imposto la fase da attivare
048700080108     c                   if        w1fase1 = 'S'
048800080108     c                   eval      w1fase = 'FASE 1 Una-Tantum'
048900080108     c                   endif
049000080108     c                   if        w1fase2 = 'S'
049100080108     c                   eval      w1fase = 'FASE 2 Giornaliera'
049200080108     c                   endif
049300080108
049400080108     c                   endsr
049500080108
049600080108      *------------------------------------------------------------------------*
049700080108      * emetto il subfile
049800080108      *------------------------------------------------------------------------*
049900080108     c     sr_emisfl     begsr
050000080108
050100080108     c                   do        *hival
050200080108
050300080108     c                   if        recsf1 > nrr1
050400080108     c                   eval      recsf1 = 1
050500080108     c                   endif
050600080108
050700080108     c                   write     or98z01
050800080108     c                   exfmt     or98c01
050900080108
051000080108     c                   eval      *in28 = *off
051100080108     c                   clear                   w1cmsg
051200080108
051300080108      * f3=fine
051400080108     c                   if        *inkc
051500080108     c                   goto      fine
051600080108     c                   endif
051700080108
051800080108      * f6=conferma
051900080108     c                   if        *inkf
052000080108     c                   exsr      sr_contrsfl
052100080108     c                   leave
052200080108     c                   endif
052300080108
052400080108     c                   enddo
052500080108
052600080108     c                   endsr
052700080108
052800080108      *------------------------------------------------------------------------*
052900080108      *  controllo i dati del subfile
053000080108      *------------------------------------------------------------------------*
053100080108     c     sr_contrsfl   begsr
053200080108
053300080108     c                   clear                   conta
053400080108     c                   clear                   nrr1
053500080108     c                   do        *hival
053600080108     c                   add       1             nrr1
053700080108     c     nrr1          chain     or98s01                            32
053800080108      * esco se fine sfl
053900080108     c                   if        *in32
054000080108     c                   leave
054100080108     c                   endif
054200080108      * scelta = 1
054300080108     c                   if        w1ssce = '1'
054400080108      * aggiorno i dati in base alla fase scelta
054500080108     c                   exsr      sr_aggiorna
054600080108     c                   eval      recsf1 = nrr1
054700080108     c                   clear                   w1ssce
054800080108     c                   endif
054900080108     c                   update    or98s01
055000080108     c                   enddo
055100080108
055200080108     c                   endsr
055300080108
055400080108      *------------------------------------------------------------------------*
055500080108      *  aggiono i dati in base alla fase scelta
055600080108      *------------------------------------------------------------------------*
055700080108     c     sr_aggiorna   begsr
055800080108
055900080108     c                   add       1             conta
056000080108     c                   select
056100080108      * fase 1 UNA TANTUM
056200080108     c                   when      w1fase1 = 'S'
056300080108      * imposta il flag su organigramma + la data partenza + 1
056400080108     c                   if        conta = 1
056500080108     c                   exsr      sr_flag
056600080108     c                   endif
056700080108      * imposto la tabella per tutte le filiali
056800080108     c                   exsr      sr_taborm
056900080108      * duplica pianificazione se richiesta
057000080108     c                   if        w1dup = 'S' and conta = 1
057100080108     c                   exsr      sr_dup
057200080108     c                   endif
057300080108      * fase 2 GIORNALIERA
057400080108     c                   when      w1fase2 = 'S'
057500080108      * imposta su organigramma la data partenza effettiva
057600080108     c                   if        conta = 1
057700080108     c                   exsr      sr_flag
057800080108     c                   endif
057900080108      * imposto la tabella per tutte le filiali
058000080108     c                   exsr      sr_taborm
058100080108      * sistema i dati degli ORM
058200080108     c                   if        conta = 1
058300080108     c                   exsr      sr_orm
058400080108     c                   endif
058500080108     c                   endsl
058600080108
058700080108     c                   endsr
058800080108
058900080108      *------------------------------------------------------------------------*
059000080108      *  imposta flag in organigramma
059100080108      *------------------------------------------------------------------------*
059200080108     c     sr_flag       begsr
059300080108
059400080108      * chiudo il file aperto
059500080108     c                   if        %open(azorg01l)
059600080108     c                   close     azorg01l
059700080108     c                   endif
059800080108
059900080108      * ovrdbf azorg01l di filiale
060000080108     c                   clear                   command
060100080108     c  n01              movea     cmd(2)        command
060200080108     c   01              movea     cmd(4)        command
060300080108     c                   call      'QCMDEXC'
060400080108     c                   parm                    command
060500080108     c                   parm                    lung
060600080108     c                   open      azorg01l
060700080108
060800080108      * imposto subito il flag e la data in organigramma di filiale
060900080108     c                   clear                   og147
061000080108     c     w1cfil        chain     azorg01l
061100080108     c                   if        %found(azorg01l)
061200080108     c                   eval      og147 = orgde7
061300080108      * fase 1
061400080108     c                   if        w1fase1 = 'S'
061500080108     c                   eval      §ogcgio = 'S'
061600080108     c                   move      w1data1       wdata
061700080108     c                   eval      waainv = waa
061800080108     c                   eval      wmminv = wmm
061900080108     c                   eval      wgginv = wgg
062000080108     c                   move      wdatainv      wdatageo
062100080108     c                   endif
062200080108      * fase 2
062300080108     c                   if        w1fase2 = 'S'
062400080108     c                   move      w1data2       wdata
062500080108     c                   eval      waainv = waa
062600080108     c                   eval      wmminv = wmm
062700080108     c                   eval      wgginv = wgg
062800080108     c                   move      wdatainv      wdatageo
062900080108     c                   endif
063000080108
063100080108     c                   move      wdatageo      §ogddao
063200080108     c                   eval      orgde7 = og147
063300080108     c                   update    azorg
063400080108     c                   endif
063500080108
063600080108      * chiudo il file aperto
063700080108     c                   if        %open(azorg01l)
063800080108     c                   close     azorg01l
063900080108     c                   endif
064000080108
064100080108      * ovrdbf azorg01l di sede
064200080108     c                   clear                   command
064300080108     c  n01              movea     cmd(3)        command
064400080108     c   01              movea     cmd(5)        command
064500080108     c                   call      'QCMDEXC'
064600080108     c                   parm                    command
064700080108     c                   parm                    lung
064800080108     c                   open      azorg01l
064900080108
065000080108      * imposto subito il flag e la data in organigramma di sede
065100080108     c                   clear                   og147
065200080108     c     w1cfil        chain     azorg01l
065300080108     c                   if        %found(azorg01l)
065400080108     c                   eval      og147 = orgde7
065500080108      * fase 1
065600080108     c                   if        w1fase1 = 'S'
065700080108     c                   eval      §ogcgio = 'S'
065800080108     c                   move      w1data1       wdata
065900080108     c                   eval      waainv = waa
066000080108     c                   eval      wmminv = wmm
066100080108     c                   eval      wgginv = wgg
066200080108     c                   move      wdatainv      wdatageo
066300080108     c                   endif
066400080108      * fase 2
066500080108     c                   if        w1fase2 = 'S'
066600080108     c                   move      w1data2       wdata
066700080108     c                   eval      waainv = waa
066800080108     c                   eval      wmminv = wmm
066900080108     c                   eval      wgginv = wgg
067000080108     c                   move      wdatainv      wdatageo
067100080108     c                   endif
067200080108
067300080108     c                   move      wdatageo      §ogddao
067400080108     c                   eval      orgde7 = og147
067500080108     c                   update    azorg
067600080108     c                   endif
067700080108
067800080108     c                   endsr
067900080108
068000080108      *------------------------------------------------------------------------*
068100080108      *  aggiorna/scrive la tebella ORM
068200080108      *------------------------------------------------------------------------*
068300080108     c     sr_taborm     begsr
068400080108
068500080108      * aggiorno la tabella con tutte le filiali attivate
068600080108     c                   eval      ktbecod = 'ORM'
068700080108     c                   movel(p)  w1sfil        ktbeke1
068800080108     c     ktntbe        chain     tntbe01l
068900080108     c                   z-add     *date         w0080
069000080108      * scrivo
069100080108     c                   if        not %found(tntbe01l)
069200080108     c                   clear                   tntbe000
069300080108     c                   clear                   dorm
069400080108     c                   eval      tbecod = ktbecod
069500080108     c                   eval      tbeke1 = ktbeke1
069600080108      * fase 1
069700080108     c                   if        w1fase1 = 'S'
069800080108     c                   movel     w0080         d§ormfase1
069900080108     c                   endif
070000080108      * fase 2
070100080108     c                   if        w1fase2 = 'S'
070200080108     c                   movel     w0080         d§ormfase2
070300080108     c                   endif
070400080108     c                   eval      tbeuni = dorm
070500080108     c                   write     tntbe000
070600080108      * aggiorno
070700080108     c                   else
070800080108     c                   eval      dorm = tbeuni
070900080108      * fase 1
071000080108     c                   if        w1fase1 = 'S'
071100080108     c                   movel     w0080         d§ormfase1
071200080108     c                   endif
071300080121      * fase 2
071400080121     c                   if        w1fase2 = 'S'
071500080108     c                   movel     w0080         d§ormfase2
071600080108     c                   endif
071700080108     c                   eval      tbeuni = dorm
071800080108     c                   update    tntbe000
071900080108     c                   endif
072000080108
072100080108     c                   endsr
072200080108
072300080108      *------------------------------------------------------------------------*
072400080108      *  duplica pianificazione
072500080108      *------------------------------------------------------------------------*
072600080108     c     sr_dup        begsr
072700080108
072800080108     c                   move      w1cfil        wfil
072900080108     c                   call      'FIDG94R'
073000080108     c                   parm                    wfil
073100080108
073200080108     c                   endsr
073300080108
073400080108      *------------------------------------------------------------------------*
073500080108      *  sistema i dati degli ORM
073600080108      *------------------------------------------------------------------------*
073700080108     c     sr_orm        begsr
073800080108
073900080108     c                   move      w1cfil        wfil
074000080108     c                   call      'TNSY01C'
074100080108     c                   parm                    wfil
074200080108
074300080108     c                   endsr
074400080108
074500080108      *------------------------------------------------------------------------*
074600080108      * routine iniziale
074700080108      *------------------------------------------------------------------------*
074800080108     c     *inzsr        begsr
074900080108
075000080108     c     *entry        plist
075100080108     c                   parm                    kpjba
075200080108
075300080108      * ambiente di prova
075400080108     c                   eval      *in01 = (%subst(knsif:7:1) = 'P')
075500080108
075600080108     c     *dtaara       define    §azute        azuteds
075700080108     c     *dtaara       define    §datiute      ddatiute
075800080108     c                   in(e)     *dtaara
075900080108     c                   if        %error or rsut = *blanks
076000080108     c                   clear                   tibs34ds
076100080108     c                   call      'TIBS34R'
076200080108     c                   parm                    tibs34ds
076300080108     c                   in        *dtaara
076400080108     c                   endif
076500080129
076600080129      * carico le filiali con autorizzazione 'AP' area poc
076700080129     c                   clear                   trul31ds
076800080129     c                   eval      i31abi = 'AP'
076900080129     c                   eval      i31cdi = dutdis
077000080129     c                   eval      i31car = dutare
077100080129     c                   eval      i31cpo = dutpou
077200080129     c                   call      'TRUL31R'
077300080129     c                   parm                    kpjba
077400080129     c                   parm                    trul31ds
077500080129     c                   if        o31pog > *zeros
077600080129     c                   movea     o31pog        skpog
077700080129     c                   endif
077800080108
077900080108      * imposto la filiale utente a video
078000080108     c                   eval      w1cfil = dutpou
078100080108     c                   eval      w1dfil = dutdpo
078200080108
078300080108     c     ktntbe        klist
078400080108     c                   kfld                    ktbecod
078500080108     c                   kfld                    ktbeke1
078600080108
078700080108      * ovrdbf azorg01l di filiale
078800080108     c                   clear                   command
078900080108     c  n01              movea     cmd(2)        command
079000080108     c   01              movea     cmd(4)        command
079100080108     c                   call      'QCMDEXC'
079200080108     c                   parm                    command
079300080108     c                   parm                    lung
079400080108     c                   open      azorg01l
079500080108
079600080108     c                   endsr
079700080108** CMD   (Lunga 80)                                                            *
079800080108
079900080108OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRU/AZORG01L)                                 2
080000080108OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRU/AZORG01L)                                 3
080100080108OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRPF/AZORG01L)                                4
080200080108OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRPS/AZORG01L)                                5
