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
002400080207      * 41 - fase 1
002500080207      * 42 - fase 2
002600080207      * 43 - password
002700080108
002800080108      *------------------------------------------------------------------------*
002900080108      * campi di work
003000080108      *------------------------------------------------------------------------*
003100080108     d command         s             80
003200080108     d conta           s              1  0 inz
003300080108     d ktbecod         s                   like(tbecod)
003400080108     d ktbeke1         s                   like(tbeke1)
003500080108     d lung            s             15  5 inz(80)
003600080108     d nrr1            s                   like(recsf1)
003700080108     d sav_w1cfil      s                   like(w1cfil)
003800080108     d wdatageo        s              8  0 inz
003900080108     d wdataiso        s               d   datfmt(*ISO)
004000080108     d wfil            s              3
004100080129     d wora            s              6  0 inz
004200080108     d w0030           s                   like(orgfil)
004300080108     d w0080           s              8  0 inz
004400080108     d xx              s              2  0 inz
004500080108
004600080108      *------------------------------------------------------------------------*
004700080108      * schiere
004800080108      *------------------------------------------------------------------------*
004900080108     d cmd             s             80    dim(5)  ctdata perrcd(1)
005000080129     d skpog           s              3    dim(250) inz(*zeros)
005100080108
005200080108      *------------------------------------------------------------------------*
005300080108      * ds interne/esterne
005400080108      *------------------------------------------------------------------------*
005500080108     d wdatainv        ds
005600080108     d waainv                  1      4
005700080108     d wmminv                  5      6
005800080108     d wgginv                  7      8
005900080108
006000080108     d wdata           ds
006100080108     d wgg                     1      2
006200080108     d wmm                     3      4
006300080108     d waa                     5      8
006400080108
006500080108     d wlbdat          ds                  inz
006600080108     d  g08dat                 1      8  0
006700080108     d  g08inv                 9     16  0
006800080108     d  g08err                17     17
006900080108     d  g08tgi                18     22  0
007000080108
007100080108     d azuteds       e ds                  extname(azute00f)
007200080108     d ddatiute      e ds
007300080108     d dorm          e ds
007400080108     d fnlv55ds      e ds
007500080108     d kpjba         e ds
007600080108     d og147         e ds
007700080108     d tibs02ds      e ds
007800080108     d tibs34ds      e ds
007900080108     d trul06ds      e ds                  inz
008000080108     d  lin                    1     90    dim(30)
008100080129     d trul31ds      e ds
008200080207
008300080207      *------------------------------------------------------------------------*
008400080207      * costanti
008500080207      *------------------------------------------------------------------------*
008600080207     d password        c                   const('GEOORM    ')
008700080108
008800080108      *------------------------------------------------------------------------*
008900080108
009000080108     c     inizio        tag
009100080129
009200080129     c                   exsr      sr_video0
009300080108
009400080108     c                   exsr      sr_carvideo
009500080108
009600080108     c                   exsr      sr_video1
009700080108
009800080108     c                   exsr      sr_video2
009900080108
010000080108     c     fine          tag
010100080108
010200080108     c                   eval      *inlr = *on
010300080129
010400080129      *------------------------------------------------------------------------*
010500080129      * gestione videata iniziale
010600080129      *------------------------------------------------------------------------*
010700080129     c     sr_video0     begsr
010800080129
010900080129     c                   do        *hival
011000080129     c                   exfmt     or98d00
011100080129     c                   eval      *in28 = *off
011200080129     c                   clear                   w1cmsg
011300080129
011400080129      * f3=fine
011500080129     c                   if        *inkc
011600080129     c                   goto      fine
011700080129     c                   endif
011800080129
011900080129      * controllo
012000080129     c                   exsr      sr_ctrd00
012100080129
012200080129     c                   if        *in28
012300080129     c                   iter
012400080129     c                   endif
012500080129
012600080129     c                   leave
012700080129
012800080129     c                   enddo
012900080129
013000080129     c                   endsr
013100080108
013200080108      *------------------------------------------------------------------------*
013300080108      * carico i dati nella prima videata
013400080108      *------------------------------------------------------------------------*
013500080108     c     sr_carvideo   begsr
013600080108
013700080108     c                   clear                   w1fase1
013800080108     c                   clear                   w1dataf1
013900080108     c                   clear                   w1data1
014000080108     c                   clear                   w1dup
014100080108     c                   clear                   w1fase2
014200080108     c                   clear                   w1dataf2
014300080108     c                   clear                   w1data2
014400080129     c                   setoff                                       0304
014500080129     c                   setoff                                       0506
014600080108
014700080108      * data fase 1 una tantum
014800080108     c                   if        d§ormfase1 > *zeros
014900080108     c                   eval      wdatainv = d§ormfase1
015000080108     c                   eval      waa = waainv
015100080108     c                   eval      wmm = wmminv
015200080108     c                   eval      wgg = wgginv
015300080129     c                   move      wdata         w1dataf1
015400080108     c                   endif
015500080108      * data fase 2 giornaliera
015600080108     c                   if        d§ormfase2 > *zeros
015700080108     c                   eval      wdatainv = d§ormfase2
015800080108     c                   eval      waa = waainv
015900080108     c                   eval      wmm = wmminv
016000080108     c                   eval      wgg = wgginv
016100080129     c                   move      wdata         w1dataf2
016200080108     c                   endif
016300080108
016400080129      * proteggo sempre la data partenza presunta
016500080114     c                   eval      *in03 = *on
016600080129      * proteggo sempre la data partenza reale
016700080129     c                   eval      *in04 = *on
016800080129
016900080129      * se già attivata la fase 2
017000080108     c                   if        d§ormfase2 > *zeros
017100080129      * imposto la data partenza presunta = data partenza reale + 1 gg
017200080108     c                   move      wdatageo      wdataiso
017300080108     c                   adddur    1:*d          wdataiso
017400080108     c                   move      wdataiso      w0080
017500080108     c                   move      w0080         wdatainv
017600080108     c                   eval      waa = waainv
017700080108     c                   eval      wmm = wmminv
017800080108     c                   eval      wgg = wgginv
017900080108     c                   move      wdata         w1data1
018000080108      * imposto la data partenza reale
018100080108     c                   move      wdatageo      wdatainv
018200080108     c                   eval      waa = waainv
018300080108     c                   eval      wmm = wmminv
018400080108     c                   eval      wgg = wgginv
018500080108     c                   move      wdata         w1data2
018600080129      * proteggo i campi delle scelte tanto la filiale è già attiva
018700080129     c                   eval      *in05 = *on
018800080129     c                   eval      *in06 = *on
018900080108     c                   endif
019000080129
019100080108      * se attivata solo la fase 1
019200080129     c                   if        d§ormfase1 > *zeros and
019300080129     c                             (d§ormfase2 = *zeros or d§ormfase2 = *blanks)
019400080129      * imposto la data partenza presunta
019500080108     c                   move      wdatageo      wdatainv
019600080108     c                   eval      waa = waainv
019700080108     c                   eval      wmm = wmminv
019800080108     c                   eval      wgg = wgginv
019900080108     c                   move      wdata         w1data1
020000080129      * proteggo i campi della scelte perchè si può attivare solo la fase 2
020100080129     c                   eval      *in05 = *on
020200080129      * imposto la data partenza reale
020300080129     c                   move      *date         wdatainv
020400080129     c                   eval      waa = waainv
020500080129     c                   eval      wmm = wmminv
020600080129     c                   eval      wgg = wgginv
020700080129     c                   move      wdata         w1data2
020800080108     c                   endif
020900080114
021000080129      * se non è stata attivata la fase 1 imposto oggi + 6 mesi
021100080129     c                   if        d§ormfase1 = *zeros or d§ormfase1 = *blanks
021200080114     c                   move      *date         wdataiso
021300080114     c                   adddur    6:*m          wdataiso
021400080114     c                   move      wdataiso      w0080
021500080114     c                   move      w0080         wdatainv
021600080114     c                   eval      waa = waainv
021700080114     c                   eval      wmm = wmminv
021800080114     c                   eval      wgg = wgginv
021900080114     c                   move      wdata         w1data1
022000080114     c                   endif
022100080129
022200080129      * imposto di default la duplica della pianificazione
022300080129     c                   eval      w1dup = 'S'
022400080108
022500080108     c                   eval      sav_w1cfil = w1cfil
022600080108
022700080108     c                   endsr
022800080108
022900080108      *------------------------------------------------------------------------*
023000080108      * gestione prima videata
023100080108      *------------------------------------------------------------------------*
023200080108     c     sr_video1     begsr
023300080108
023400080108     c                   do        *hival
023500080108     c                   exfmt     or98d01
023600080108     c                   eval      *in28 = *off
023700080108     c                   clear                   w1cmsg
023800080108
023900080108      * f3=fine
024000080108     c                   if        *inkc
024100080108     c                   goto      fine
024200080108     c                   endif
024300080129
024400080129      * f12=ritorno
024500080129     c                   if        *inkl
024600080129     c                   goto      inizio
024700080129     c                   endif
024800080108
024900080108      * controllo
025000080108     c                   exsr      sr_ctrd01
025100080108
025200080108     c                   if        *in28
025300080108     c                   iter
025400080108     c                   endif
025500080108
025600080108      * f6=conferma
025700080108     c                   if        *inkf
025800080108     c                   leave
025900080108     c                   endif
026000080108
026100080108     c                   enddo
026200080108
026300080108     c                   endsr
026400080108
026500080108      *------------------------------------------------------------------------*
026600080108      * gestione seconda videata
026700080108      *------------------------------------------------------------------------*
026800080108     c     sr_video2     begsr
026900080108
027000080108      * visualizzo le linee di arrivo da abilitare
027100080108     c                   exsr      sr_pulsfl
027200080108     c                   exsr      sr_carsfl
027300080108     c                   exsr      sr_emisfl
027400080108
027500080108     c                   endsr
027600080129
027700080129      *------------------------------------------------------------------------*
027800080129      * controllo la prima iniziale
027900080129      *------------------------------------------------------------------------*
028000080129     c     sr_ctrd00     begsr
028100080129
028200080207     c                   setoff                                       4043
028300080129
028400080129      * la filiale inserita deve essere una filiale valida
028500080129     c                   clear                   w1dfil
028600080129     c                   clear                   og147
028700080129     c     w1cfil        chain(n)  azorg01l
028800080129     c                   if        not %found(azorg01l) or orgfva <> *blanks or
028900080129     c                             (orgfag <> 'A' and orgfag <> 'F')
029000080129     c                   eval      w1cmsg = 'Filiale errata'
029100080129     c                   eval      *in28 = *on
029200080129     c                   eval      *in40 = *on
029300080129     c                   leavesr
029400080129     c                   endif
029500080129
029600080129      * non deve essere una filiale gestita da altra filiale
029700080129     c                   clear                   fnlv55ds
029800080129     c                   movel     '6'           d55tpt
029900080129     c                   z-add     w1cfil        d55lin
030000080129     c                   z-add     *date         d55drf
030100080129     c                   call      'FNLV55R'
030200080129     c                   parm                    fnlv55ds
030300080129     c                   if        d55lin <> d55tfa
030400080129     c                   eval      w1cmsg = 'Attenzione!! Filiale gestita'
030500080129     c                   eval      *in28 = *on
030600080129     c                   eval      *in40 = *on
030700080129     c                   leavesr
030800080129     c                   endif
030900080129
031000080129      * per utente no EDP controllo che la filiale sia gestibile dall'utente
031100080129     c                   if        %subst(knmus:1:3) <> 'EDP'
031200080129     c                   movel     w1cfil        wfil
031300080129     c                   if        %lookup(wfil:skpog) = *zeros
031400080129     c                   eval      w1cmsg = 'Filiale non in gestione -
031500080129     c                             all''utente'
031600080129     c                   eval      *in28 = *on
031700080129     c                   eval      *in40 = *on
031800080129     c                   leavesr
031900080129     c                   endif
032000080129     c                   endif
032100080129
032200080129     c                   eval      w1dfil= orgdes
032300080129     c                   eval      og147 = orgde7
032400080129
032500080129      * data partenza
032600080129     c                   if        §ogddao > *zeros
032700080129     c                   move      §ogddao       wdatageo
032800080129     c                   else
032900080129     c                   clear                   wdatageo
033000080129     c                   endif
033100080129
033200080129      * aggancio la tabella ORM della filiale utente
033300080129     c                   clear                   dorm
033400080129     c                   clear                   tibs02ds
033500080129     c                   eval      t02mod = 'C'
033600080129     c                   eval      t02sif = knsif
033700080129     c                   eval      t02cod = 'ORM'
033800080129     c                   movel     w1cfil        t02ke1
033900080129     c                   call      'TIBS02R'
034000080129     c                   parm                    kpjba
034100080129     c                   parm                    tibs02ds
034200080129     c                   if        t02err = *blanks
034300080129     c                   eval      dorm = t02uni
034400080129     c                   endif
034500080129
034600080129      * controllo se filiale già attivata (tabella ORM)
034700080129     c                   if        d§ormfase1 <> *blanks and
034800080129     c                             d§ormfase2 <> *blanks
034900080129     c                   eval      w1cmsg = 'Filiale già attivata'
035000080129     c                   eval      *in28 = *on
035100080129     c                   eval      *in40 = *on
035200080129     c                   leavesr
035300080129     c                   endif
035400080129
035500080129      * controllo se filiale già attivata (organigramma)
035600080129     c                   if        §ogcgio = 'S' and
035700080129     c                             wdatageo > *zeros and wdatageo <= *date
035800080129     c                   eval      w1cmsg = 'Filiale già attivata'
035900080129     c                   eval      *in28 = *on
036000080129     c                   eval      *in40 = *on
036100080129     c                   leavesr
036200080129     c                   endif
036300080129
036400080129      * cerco le filiali gestite dalla filiale da elaborare
036500080129     c                   clear                   trul06ds
036600080129     c                   eval      d06cod = '£6'
036700080129     c                   movel     w1cfil        d06key
036800080129     c                   eval      d06tla = 'L'
036900080129     c                   eval      kpjbu = trul06ds
037000080129     c                   call      'TRUL06R'
037100080129     c                   parm                    kpjba
037200080129     c                   eval      trul06ds = kpjbu
037300080207
037400080207      * controllo la password
037500080207     c                   if        w1cpsw = *blanks
037600080207     c                   eval      w1cmsg = 'Immettere la password'
037700080207     c                   eval      *in28 = *on
037800080207     c                   eval      *in43 = *on
037900080207     c                   leavesr
038000080207     c                   endif
038100080207      * controllo se esatta
038200080207     c                   if        w1cpsw <> password
038300080207     c                   clear                   w1cpsw
038400080207     c                   eval      w1cmsg = 'Password errata'
038500080207     c                   eval      *in28 = *on
038600080207     c                   eval      *in43 = *on
038700080207     c                   leavesr
038800080207     c                   endif
038900080129
039000080129     c                   endsr
039100080108
039200080108      *------------------------------------------------------------------------*
039300080108      * controllo la prima videata
039400080108      *------------------------------------------------------------------------*
039500080108     c     sr_ctrd01     begsr
039600080129
039700080129     c                   setoff                                       4142
039800080108
039900080108      * posso richiedere una fase alla volta
040000080108     c                   if        w1fase1 = 'S' and w1fase2 = 'S'
040100080108     c                   eval      w1cmsg = 'Attivare una FASE alla volta'
040200080108     c                   eval      *in28 = *on
040300080129     c                   eval      *in41 = *on
040400080108     c                   leavesr
040500080108     c                   endif
040600080108
040700080108      * devo richiedere almeno una FASE
040800080108     c                   if        w1fase1 = *blanks and w1fase2 = *blanks
040900080108     c                   eval      w1cmsg = 'Scegliere la FASE da attivare'
041000080108     c                   eval      *in28 = *on
041100080129     c  n05              eval      *in41 = *on
041200080129     c   05              eval      *in42 = *on
041300080108     c                   leavesr
041400080108     c                   endif
041500080108
041600080108      * FASE 1 Una-Tantum
041700080108     c                   if        w1fase1 = 'S'
041800080108      * errore se già attivato (tabella ORM)
041900080108     c                   if        d§ormfase1 <> *blanks
042000080108     c                   eval      w1cmsg = 'FASE 1 Una-Tantum già attivata'
042100080108     c                   eval      *in28 = *on
042200080129     c                   eval      *in41 = *on
042300080108     c                   leavesr
042400080108     c                   endif
042500080108      * errore se già attivato (organigramma)
042600080108     c                   if        §ogcgio = 'S'
042700080108     c                   eval      w1cmsg = 'FASE 1 Una-Tantum già attivata'
042800080108     c                   eval      *in28 = *on
042900080129     c                   eval      *in41 = *on
043000080108     c                   leavesr
043100080108     c                   endif
043200080129     c                   endif
043300080108
043400080108      * FASE 2 Giornaliera
043500080108     c                   if        w1fase2 = 'S'
043600080108      * errore se non ho attivato già la fase 1
043700080108     c                   if        d§ormfase1 = *blanks
043800080108     c                   eval      w1cmsg =
043900080108     c                             'Prima attivare la FASE 1 Una-Tantum'
044000080108     c                   eval      *in28 = *on
044100080129     c                   eval      *in42 = *on
044200080108     c                   leavesr
044300080108     c                   endif
044400080108      * errore se già attivato (tabella ORM)
044500080108     c                   if        d§ormfase2 <> *blanks
044600080108     c                   eval      w1cmsg = 'FASE 2 giornaliera già attivata'
044700080108     c                   eval      *in28 = *on
044800080129     c                   eval      *in42 = *on
044900080108     c                   leavesr
045000080108     c                   endif
045100080108      * errore se già attivato (organigramma)
045200080108     c                   if        wdatageo <= *date
045300080108     c                   eval      w1cmsg = 'FASE 2 giornaliera già attivata'
045400080108     c                   eval      *in28 = *on
045500080129     c                   eval      *in42 = *on
045600080108     c                   leavesr
045700080108     c                   endif
045800080129      * cerco l'orario di sistema
045900080129     c                   time                    wora
046000080129      * errore se orario di attivazione non tra le 9:30 e le 11:30
046100080213      * non controllo se utente EDP
046200080129     c                   if        wora < 093000 or wora >= 113100
046300080211     c                             and %subst(knmus:1:3) <> 'EDP'
046400080129     c                   eval      w1cmsg = 'FASE 2 da attivare tra le 9:30 -
046500080129     c                             e le 11:30'
046600080129     c                   eval      *in28 = *on
046700080129     c                   eval      *in42 = *on
046800080129     c                   leavesr
046900080129     c                   endif
047000080108     c                   endif
047100080108
047200080108     c                   endsr
047300080108
047400080108      *------------------------------------------------------------------------*
047500080108      * pulizia del subfile
047600080108      *------------------------------------------------------------------------*
047700080108     c     sr_pulsfl     begsr
047800080108
047900080108     c                   clear                   nrr1
048000080108     c                   eval      *in20 = *off
048100080108     c                   eval      *in21 = *off
048200080108     c                   write     or98c01
048300080108     c                   eval      *in21 = *on
048400080108     c                   eval      *in20 = *on
048500080108
048600080108     c                   eval      recsf1 = 1
048700080108     c                   eval      *in31 = *off
048800080108
048900080108     c                   endsr
049000080108
049100080108      *------------------------------------------------------------------------*
049200080108      * carico subfile con le filiali
049300080108      *------------------------------------------------------------------------*
049400080108     c     sr_carsfl     begsr
049500080108
049600080108      * leggo la schiera delle filiali
049700080108     c                   do        30            xx
049800080108     c                   if        lin(xx) = *blanks or lin(xx) = *zeros
049900080108     c                   iter
050000080108     c                   endif
050100080108     c                   move      lin(xx)       w0030
050200080108     c     w0030         chain(n)  azorg01l
050300080108     c                   eval      w1sfil = w0030
050400080108     c                   eval      w1sdfil = orgdes
050500080108     c                   eval      w1ssce = '1'
050600080108     c                   eval      nrr1 = xx
050700080108     c                   write     or98s01
050800080108     c                   enddo
050900080108
051000080108     c                   eval      *in31 = *on
051100080108
051200080108      * imposto la fase da attivare
051300080108     c                   if        w1fase1 = 'S'
051400080108     c                   eval      w1fase = 'FASE 1 Una-Tantum'
051500080108     c                   endif
051600080108     c                   if        w1fase2 = 'S'
051700080108     c                   eval      w1fase = 'FASE 2 Giornaliera'
051800080108     c                   endif
051900080108
052000080108     c                   endsr
052100080108
052200080108      *------------------------------------------------------------------------*
052300080108      * emetto il subfile
052400080108      *------------------------------------------------------------------------*
052500080108     c     sr_emisfl     begsr
052600080108
052700080108     c                   do        *hival
052800080108
052900080108     c                   if        recsf1 > nrr1
053000080108     c                   eval      recsf1 = 1
053100080108     c                   endif
053200080108
053300080108     c                   write     or98z01
053400080108     c                   exfmt     or98c01
053500080108
053600080108     c                   eval      *in28 = *off
053700080108     c                   clear                   w1cmsg
053800080108
053900080108      * f3=fine
054000080108     c                   if        *inkc
054100080108     c                   goto      fine
054200080108     c                   endif
054300080108
054400080108      * f6=conferma
054500080108     c                   if        *inkf
054600080108     c                   exsr      sr_contrsfl
054700080108     c                   leave
054800080108     c                   endif
054900080108
055000080108     c                   enddo
055100080108
055200080108     c                   endsr
055300080108
055400080108      *------------------------------------------------------------------------*
055500080108      *  controllo i dati del subfile
055600080108      *------------------------------------------------------------------------*
055700080108     c     sr_contrsfl   begsr
055800080108
055900080108     c                   clear                   conta
056000080108     c                   clear                   nrr1
056100080108     c                   do        *hival
056200080108     c                   add       1             nrr1
056300080108     c     nrr1          chain     or98s01                            32
056400080108      * esco se fine sfl
056500080108     c                   if        *in32
056600080108     c                   leave
056700080108     c                   endif
056800080108      * scelta = 1
056900080108     c                   if        w1ssce = '1'
057000080108      * aggiorno i dati in base alla fase scelta
057100080108     c                   exsr      sr_aggiorna
057200080108     c                   eval      recsf1 = nrr1
057300080108     c                   clear                   w1ssce
057400080108     c                   endif
057500080108     c                   update    or98s01
057600080108     c                   enddo
057700080108
057800080108     c                   endsr
057900080108
058000080108      *------------------------------------------------------------------------*
058100080108      *  aggiono i dati in base alla fase scelta
058200080108      *------------------------------------------------------------------------*
058300080108     c     sr_aggiorna   begsr
058400080108
058500080108     c                   add       1             conta
058600080108     c                   select
058700080108      * fase 1 UNA TANTUM
058800080108     c                   when      w1fase1 = 'S'
058900080108      * imposta il flag su organigramma + la data partenza + 1
059000080108     c                   if        conta = 1
059100080108     c                   exsr      sr_flag
059200080108     c                   endif
059300080108      * imposto la tabella per tutte le filiali
059400080108     c                   exsr      sr_taborm
059500080108      * duplica pianificazione se richiesta
059600080108     c                   if        w1dup = 'S' and conta = 1
059700080108     c                   exsr      sr_dup
059800080108     c                   endif
059900080108      * fase 2 GIORNALIERA
060000080108     c                   when      w1fase2 = 'S'
060100080108      * imposta su organigramma la data partenza effettiva
060200080108     c                   if        conta = 1
060300080108     c                   exsr      sr_flag
060400080108     c                   endif
060500080108      * imposto la tabella per tutte le filiali
060600080108     c                   exsr      sr_taborm
060700080108      * sistema i dati degli ORM
060800080108     c                   if        conta = 1
060900080108     c                   exsr      sr_orm
061000080108     c                   endif
061100080108     c                   endsl
061200080108
061300080108     c                   endsr
061400080108
061500080108      *------------------------------------------------------------------------*
061600080108      *  imposta flag in organigramma
061700080108      *------------------------------------------------------------------------*
061800080108     c     sr_flag       begsr
061900080108
062000080108      * chiudo il file aperto
062100080108     c                   if        %open(azorg01l)
062200080108     c                   close     azorg01l
062300080108     c                   endif
062400080108
062500080108      * ovrdbf azorg01l di filiale
062600080108     c                   clear                   command
062700080108     c  n01              movea     cmd(2)        command
062800080108     c   01              movea     cmd(4)        command
062900080108     c                   call      'QCMDEXC'
063000080108     c                   parm                    command
063100080108     c                   parm                    lung
063200080108     c                   open      azorg01l
063300080108
063400080108      * imposto subito il flag e la data in organigramma di filiale
063500080108     c                   clear                   og147
063600080108     c     w1cfil        chain     azorg01l
063700080108     c                   if        %found(azorg01l)
063800080108     c                   eval      og147 = orgde7
063900080108      * fase 1
064000080108     c                   if        w1fase1 = 'S'
064100080108     c                   eval      §ogcgio = 'S'
064200080108     c                   move      w1data1       wdata
064300080108     c                   eval      waainv = waa
064400080108     c                   eval      wmminv = wmm
064500080108     c                   eval      wgginv = wgg
064600080108     c                   move      wdatainv      wdatageo
064700080108     c                   endif
064800080108      * fase 2
064900080108     c                   if        w1fase2 = 'S'
065000080108     c                   move      w1data2       wdata
065100080108     c                   eval      waainv = waa
065200080108     c                   eval      wmminv = wmm
065300080108     c                   eval      wgginv = wgg
065400080108     c                   move      wdatainv      wdatageo
065500080108     c                   endif
065600080108
065700080108     c                   move      wdatageo      §ogddao
065800080108     c                   eval      orgde7 = og147
065900080108     c                   update    azorg
066000080108     c                   endif
066100080108
066200080108      * chiudo il file aperto
066300080108     c                   if        %open(azorg01l)
066400080108     c                   close     azorg01l
066500080108     c                   endif
066600080108
066700080108      * ovrdbf azorg01l di sede
066800080108     c                   clear                   command
066900080108     c  n01              movea     cmd(3)        command
067000080108     c   01              movea     cmd(5)        command
067100080108     c                   call      'QCMDEXC'
067200080108     c                   parm                    command
067300080108     c                   parm                    lung
067400080108     c                   open      azorg01l
067500080108
067600080108      * imposto subito il flag e la data in organigramma di sede
067700080108     c                   clear                   og147
067800080108     c     w1cfil        chain     azorg01l
067900080108     c                   if        %found(azorg01l)
068000080108     c                   eval      og147 = orgde7
068100080108      * fase 1
068200080108     c                   if        w1fase1 = 'S'
068300080108     c                   eval      §ogcgio = 'S'
068400080108     c                   move      w1data1       wdata
068500080108     c                   eval      waainv = waa
068600080108     c                   eval      wmminv = wmm
068700080108     c                   eval      wgginv = wgg
068800080108     c                   move      wdatainv      wdatageo
068900080108     c                   endif
069000080108      * fase 2
069100080108     c                   if        w1fase2 = 'S'
069200080108     c                   move      w1data2       wdata
069300080108     c                   eval      waainv = waa
069400080108     c                   eval      wmminv = wmm
069500080108     c                   eval      wgginv = wgg
069600080108     c                   move      wdatainv      wdatageo
069700080108     c                   endif
069800080108
069900080108     c                   move      wdatageo      §ogddao
070000080108     c                   eval      orgde7 = og147
070100080108     c                   update    azorg
070200080108     c                   endif
070300080108
070400080108     c                   endsr
070500080108
070600080108      *------------------------------------------------------------------------*
070700080108      *  aggiorna/scrive la tebella ORM
070800080108      *------------------------------------------------------------------------*
070900080108     c     sr_taborm     begsr
071000080108
071100080108      * aggiorno la tabella con tutte le filiali attivate
071200080108     c                   eval      ktbecod = 'ORM'
071300080108     c                   movel(p)  w1sfil        ktbeke1
071400080108     c     ktntbe        chain     tntbe01l
071500080108     c                   z-add     *date         w0080
071600080108      * scrivo
071700080108     c                   if        not %found(tntbe01l)
071800080108     c                   clear                   tntbe000
071900080108     c                   clear                   dorm
072000080108     c                   eval      tbecod = ktbecod
072100080108     c                   eval      tbeke1 = ktbeke1
072200080108      * fase 1
072300080108     c                   if        w1fase1 = 'S'
072400080108     c                   movel     w0080         d§ormfase1
072500080108     c                   endif
072600080108      * fase 2
072700080108     c                   if        w1fase2 = 'S'
072800080108     c                   movel     w0080         d§ormfase2
072900080108     c                   endif
073000080108     c                   eval      tbeuni = dorm
073100080108     c                   write     tntbe000
073200080108      * aggiorno
073300080108     c                   else
073400080108     c                   eval      dorm = tbeuni
073500080108      * fase 1
073600080108     c                   if        w1fase1 = 'S'
073700080108     c                   movel     w0080         d§ormfase1
073800080108     c                   endif
073900080121      * fase 2
074000080121     c                   if        w1fase2 = 'S'
074100080108     c                   movel     w0080         d§ormfase2
074200080108     c                   endif
074300080108     c                   eval      tbeuni = dorm
074400080108     c                   update    tntbe000
074500080108     c                   endif
074600080108
074700080108     c                   endsr
074800080108
074900080108      *------------------------------------------------------------------------*
075000080108      *  duplica pianificazione
075100080108      *------------------------------------------------------------------------*
075200080108     c     sr_dup        begsr
075300080108
075400080108     c                   move      w1cfil        wfil
075500080108     c                   call      'FIDG94R'
075600080108     c                   parm                    wfil
075700080108
075800080108     c                   endsr
075900080108
076000080108      *------------------------------------------------------------------------*
076100080108      *  sistema i dati degli ORM
076200080108      *------------------------------------------------------------------------*
076300080108     c     sr_orm        begsr
076400080108
076500080108     c                   move      w1cfil        wfil
076600080108     c                   call      'TNSY01C'
076700080108     c                   parm                    wfil
076800080108
076900080108     c                   endsr
077000080108
077100080108      *------------------------------------------------------------------------*
077200080108      * routine iniziale
077300080108      *------------------------------------------------------------------------*
077400080108     c     *inzsr        begsr
077500080108
077600080108     c     *entry        plist
077700080108     c                   parm                    kpjba
077800080108
077900080108      * ambiente di prova
078000080108     c                   eval      *in01 = (%subst(knsif:7:1) = 'P')
078100080108
078200080108     c     *dtaara       define    §azute        azuteds
078300080108     c     *dtaara       define    §datiute      ddatiute
078400080108     c                   in(e)     *dtaara
078500080108     c                   if        %error or rsut = *blanks
078600080108     c                   clear                   tibs34ds
078700080108     c                   call      'TIBS34R'
078800080108     c                   parm                    tibs34ds
078900080108     c                   in        *dtaara
079000080108     c                   endif
079100080129
079200080129      * carico le filiali con autorizzazione 'AP' area poc
079300080129     c                   clear                   trul31ds
079400080129     c                   eval      i31abi = 'AP'
079500080129     c                   eval      i31cdi = dutdis
079600080129     c                   eval      i31car = dutare
079700080129     c                   eval      i31cpo = dutpou
079800080129     c                   call      'TRUL31R'
079900080129     c                   parm                    kpjba
080000080129     c                   parm                    trul31ds
080100080129     c                   if        o31pog > *zeros
080200080129     c                   movea     o31pog        skpog
080300080129     c                   endif
080400080108
080500080108      * imposto la filiale utente a video
080600080108     c                   eval      w1cfil = dutpou
080700080108     c                   eval      w1dfil = dutdpo
080800080108
080900080108     c     ktntbe        klist
081000080108     c                   kfld                    ktbecod
081100080108     c                   kfld                    ktbeke1
081200080108
081300080108      * ovrdbf azorg01l di filiale
081400080108     c                   clear                   command
081500080108     c  n01              movea     cmd(2)        command
081600080108     c   01              movea     cmd(4)        command
081700080108     c                   call      'QCMDEXC'
081800080108     c                   parm                    command
081900080108     c                   parm                    lung
082000080108     c                   open      azorg01l
082100080108
082200080108     c                   endsr
082300080108** CMD   (Lunga 80)                                                            *
082400080108
082500080108OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRU/AZORG01L)                                 2
082600080108OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRU/AZORG01L)                                 3
082700080108OVRDBF FILE(AZORG01L) TOFILE(FILTRAGRPF/AZORG01L)                                4
082800080108OVRDBF FILE(AZORG01L) TOFILE(GAITRAGRPS/AZORG01L)                                5
