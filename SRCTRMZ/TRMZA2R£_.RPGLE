000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300100323     Ftrmza2d   CF   E             WORKSTN
000400070605     Faitra04l  if   e           k disk
000500050623     Fazorg01l  if   e           k disk    rename(azorg:azorg1)
000600050622     Fazorg02l  if   e           k disk
000700100323     Fwaitra1f  uf a e           k disk    usropn
000800050801     ftabel00f  if   e           k disk
000900050805     Fqsysprt   O    F  132        PRINTER prtctl(prtds)
001000020304     dkpjba          e ds
001100030630     d Tibs36Ds      e ds
001200070529     d Tibs02Ds      e ds
001300020404     d prtds           ds
001400020404     d   spab                         3  0
001500020404     d   spaa                         3  0
001600020404     d   skab                         3  0
001700020404     d   skaa                         3  0
001800020404     d   line                         3  0
001900050623     d totmzfil        s              5  0
002000050623     d totmzarea       s              5  0
002100050623     d totmzdis        s              5  0
002200050623     d totmzaz         s              5  0
002300050623     d primo           s              1
002400050801     d exaread         s             14
002500051121     d exaread20       s             20
002600051121     d exdistd20       s             20
002700050623     d excar           s                   like(orgcar)
002800050802     d warea           s                   like(orgcar)
002900050623     d kfil            s                   like(orgfil)
003000050803     d exfil           s                   like(orgfil)
003100050623     d exfl3           s                   like(orgfl3)
003200050802     d wdist           s                   like(orgfl3)
003300070608     d coman           s             99
003400050802     d luncmd          s             15  5
003500070605     d dataiso         s               d   datfmt(*iso)
003600070605     d dataeur         s               d   datfmt(*eur)
003700070605     D WLBDA8          DS
003800070605     D  G02DAT                 1      8  0
003900070605     D  G02INV                 9     16  0
004000070605     D  G02ERR                17     17
004100070605     D  G02TGI                18     22  0
004200080502      *
004300070608     d file            s             10
004400070608     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
004500070608     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
004600070608     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
004700070608     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
004800070608     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
004900091216
005000091216
005100000000     C*---------------------------------------------------------------*
005200020304      *
005300031105     c                   do        *hival
005400031105     c                   exfmt     video1
005500031105     c                   clear                   $msg
005600091217     c   kc              leave
005700031105     c                   exsr      controv1
005800031105     c   96              iter
005900050623     c                   if        *inkf
006000091216      *
006100091111     c                   if        vstampa = 'S'
006200070531     c                   except    legenda
006300070601     c                   z-add     99            line
006400091216      *
006500091111     c                   endif
006600091216      *
006700050805      * verifico indicatore per salto pagina forzato
006800050805     c     saltp         comp      'S'                                    66
006900050802      *clear file
007000050802     c                   movea     cmd1          coman
007100050802     c                   exsr      comandi
007200050802
007300050623     c                   exsr      elabora
007400050805     c                   leave
007500050623     c                   endif
007600050805     c                   enddo
007700050802      * se richiesta stampa per area o distretto la eseguo dopo aver
007800050802      * completato il W.F con i dati selezionati
007900070531     c*                  if        lisarea = 'S' and *in80 or
008000070531     c*                            lisdist = 'S' and *in81
008100091111      * se si vuole la stampa
008200100114     c                   if        vstampa = 'S'
008300050802     c                   exsr      waitra
008400091111     c                   endif
008500070531     c*                  endif
008600160115      ***
008700160115      *    CANCELLA sempre il file per poi ricrearlo
008800070608     c                   movea     cmd4          coman
008900070608     c                   eval      %subst(coman:22:10) = file
009000070608     c                   exsr      comandi2
009100160115      ***
009200160115      *     RICREA il file sempre
009300070608     c                   movea     cmd5          coman
009400070608     c                   eval      %subst(coman:78:10) = file
009500070608     c                   exsr      comandi2
009600160115      ***
009700091217      ** se cambiato il tracciato del file lo deve segnalare
009800091217     c                   if        not *inKC
009900091217     c   34              eval      w1nomf = file
010000091217     c   34              exfmt     w1Error
010100091217     c                   end
010200091217      **
010300050802      *elimina file in qtemp x la filiale
010400051017     c                   movea     cmd3          coman
010500051017     c                   exsr      comandi
010600091217      *
010700091217     c     fine          tag
010800020308     c                   seton                                        lr
010900050622     c*-------------------------------------------------
011000050622     c     elabora       begsr
011100050622     c*-------------------------------------------------
011200050622     c                   clear                   v1dfil
011300070529 1   c                   if        v1cfil > 0 and lisarea = *blank and
011400050802     c                             lisdist = *blank
011500050623     c                   move      v1cfil        kfil
011600050622     c                   exsr      aitra
011700070529 x1  c                   else
011800050622     c     *loval        setll     azorg02l
011900070529 2   c                   do        *hival
012000050622     c                   read      azorg02l
012100070529 3   c                   if        %eof(azorg02l) or orgfl3 > '5'
012200050622     c                   leave
012300070529 e3  c                   endif
012400051122      * SEDE solo le filiali di un'area se selezionata a video
012500051122     c                   move      orgcar        orgcara           3
012600070529 3   c                   if        orgcara <> varea and varea <> *blank
012700051122     c                   iter
012800070529 e3  c                   endif
012900051122      * SEDE solo le filiali di un distretto se selezionato a video
013000051122     c                   if        orgfl3 <> vdist and vdist <> *blank
013100051122     c                   iter
013200051122     c                   endif
013300050623      * solo le filiali
013400050805     c                   if        orgfag <>'F' or %subst(orgde8:1:1) = *blank
013500050623     c                   iter
013600050623     c                   endif
013700050802      * scarto se profilo di area o di distretto e voglio sapere solo i miei
013800070529     c                   if        lisarea = 'S' and *in80
013900070529     c                   if        orgcar <> warea
014000050802     c                   iter
014100050802     c                   endif
014200050802     c                   endif
014300070619     c                   if        lisdist = 'S' and *in81
014400070619     c                   if        orgfl3 <> wdist
014500050802     c                   iter
014600050802     c                   endif
014700050802     c                   endif
014800070531     c                   move      orgfil        kfil
014900050622     c                   exsr      aitra
015000070619 e2  c                   enddo
015100070619 e1  c                   endif
015200050622
015300050622     c                   endsr
015400990113     c*-------------------------------------------------
015500030630     c     aitra         begsr
015600990113     c*-------------------------------------------------
015700070605     c     kfil          setll     aitra04l
015800981029     c                   do        *hival
015900070605     c     kfil          reade     aitra04l                               98
016000031105     c   98              leave
016100070530      * esclude annullati e corrispondenti
016200091006     c                   if        traann <> *blank
016300040212     c                   iter
016400040212     c                   endif
016500081204      * esclude se selezionati autisti di città o aff/def
016600081204     c                   if        vtpaut <> *blank
016700081204     c                   if        vtpaut = 'C' and trakau = 0
016800081204     c                   iter
016900081204     c                   endif
017000081204     c                   if        vtpaut = 'A' and trabpt = 0
017100081204     c                   iter
017200081204     c                   endif
017300081204     c                   endif
017400100324      * esclude disaccreditati se non selezionati a video
017500100323     c                   if        Vtradfi <> 'S' and tradfi > 0
017600091111     c                   iter
017700091111     c                   endif
017800070530      * analizza filtri a video
017900070530      * Autista di riserva
018000070530     c                   if        vtraris <> *blank
018100070530     c                   if        vtraris = 'S' and traris = *blank or
018200070530     c                             vtraris = 'N' and traris <> *blank
018300070530     c                   iter
018400070530     c                   endif
018500070531     c                   endif
018600070530      * Mezzo di scorta
018700070530     c                   if        vtrasco <> *blank
018800070530     c                   if        vtrasco = 'S' and trasco = *blank or
018900070530     c                             vtrasco = 'N' and trasco <> *blank
019000070530     c                   iter
019100070530     c                   endif
019200070531     c                   endif
019300070530      * sponda idraulica
019400070530     c                   if        vtraspi <> *blank
019500070530     c                   if        vtraspi = 'S' and traspi <> 'S' or
019600070530     c                             vtraspi = 'N' and traspi = 'S'
019700070530     c                   iter
019800070530     c                   endif
019900070531     c                   endif
020000100323      * periferica satellitare
020100100323     c                   if        vtraper = 'S' and traper = *blank
020200070606     c                   iter
020300070606     c                   endif
020400100323     c                   if        vtraper = 'N' and traper <> *blank
020500100323     c                   iter
020600100323     c                   endif
020700100323      * centrale controllo satellitare
020800100323     c                   if        vtracen <> *blank
020900100323     c                   if        vtracen  <> tracen
021000100323     c                   iter
021100100323     c                   endif
021200100323     c                   endif
021300070530      * allestimento
021400070530     c                   if        vtrafur <> *blank
021500070530     c                   if        vtrafur  <> trafur
021600070530     c                   iter
021700070530     c                   endif
021800070531     c                   endif
021900100402      * con targa inserita
022000100402     c                   if        vtrataa <> *blank
022100100402     c                   if        trataa  =  *blank
022200100402     c                   iter
022300100402     c                   endif
022400100402     c                   endif
022500070530      * antifurto blocca porte
022600070530     c                   if        vtrabpo <> *blank
022700070530     c                   if        vtrabpo  <> trabpo
022800070530     c                   iter
022900070530     c                   endif
023000070531     c                   endif
023100070530      * antifurto blocco motore
023200070530     c                   if        vtraant <> *blank
023300070530     c                   if        vtraant  <> traant
023400070530     c                   iter
023500070530     c                   endif
023600070531     c                   endif
023700160113      *
023800160113     c                   clear                   tibs02ds
023900160113     c                   movel     'C'           t02mod
024000160113     c                   movel     knsif         t02sif
024100160113     c                   movel     'CEN'         t02cod
024200160113     c                   movel     tracen        t02ke1
024300160113     c                   call      'TIBS02R'
024400160113     c                   parm                    KPJBA
024500160113     c                   parm                    TIBS02DS
024600160113     c                   if        t02err =  *blanks
024700160113     c                   movel     t02uni        tracend
024800160113     c                   else
024900160113     c                   clear                   tracend
025000160113     c                   endif
025100160113      *decodifica CCNL
025200160113     c                   clear                   traCNLd
025300160113     c                   if        traCNL <> *blank
025400160113     c                   clear                   tibs02ds
025500160113     c                   movel     'C'           t02mod
025600160113     c                   movel     knsif         t02sif
025700160113     c                   movel     'CNL'         t02cod
025800160113     c                   movel     traCNL        t02ke1
025900160113     c                   call      'TIBS02R'
026000160113     c                   parm                    KPJBA
026100160113     c                   parm                    TIBS02DS
026200160113     c                   if        t02err =  *blanks
026300160113     c                   movel     t02uni        traCNLd
026400160113     c                   endif
026500160113     c                   endif
026600160113      *decodifica QUALIFICA
026700160113     c                   clear                   traQUAd
026800160113     c                   if        traQUA <> *blank
026900160113     c                   clear                   tibs02ds
027000160113     c                   movel     'C'           t02mod
027100160113     c                   movel     knsif         t02sif
027200160113     c                   movel     'QUA'         t02cod
027300160113     c                   movel     traQUA        t02ke1
027400160113     c                   call      'TIBS02R'
027500160113     c                   parm                    KPJBA
027600160113     c                   parm                    TIBS02DS
027700160113     c                   if        t02err =  *blanks
027800160113     c                   movel     t02uni        traQUAd
027900160113     c                   endif
028000160113     c                   endif
028100160113      *decodifica LIVELLO
028200160113     c                   clear                   traLIQd
028300160113     c                   if        traLIQ <> *blank
028400160113     c                   clear                   tibs02ds
028500160113     c                   movel     'C'           t02mod
028600160113     c                   movel     knsif         t02sif
028700160113     c                   movel     'LIQ'         t02cod
028800160113     c                   movel     traLIQ        t02ke1
028900160113     c                   call      'TIBS02R'
029000160113     c                   parm                    KPJBA
029100160113     c                   parm                    TIBS02DS
029200160113     c                   if        t02err =  *blanks
029300160113     c                   movel     t02uni        traLIQd
029400160113     c                   endif
029500160113     c                   endif
029600050801      * scrive W.F
029700050801     c                   move      orgcar        traarea
029800050801     c                   move      orgfl3        tradist
029900100331     c                   movel     orgdes        trafild
030000100331      *
030100100331     c                   clear                   traaread
030200100331     c                   movel     orgcar        kkey
030300100331     c                   move      '05'          kcod
030400100331     c     ktab          chain     tabel00f
030500100331     c                   if        %found(tabel00f)
030600100331     c                   movel     tbluni        traaread
030700100331     c                   endif
030800050801     c                   write     waitra00
030900050623     c                   enddo
031000020304
031100020304     c                   endsr
031200981029      *
031300050802     c*-------------------------------------------------
031400050802     c     waitra        begsr
031500050802     c*-------------------------------------------------
031600070530     c                   clear                   totmzfil
031700070530     c                   clear                   totmzarea
031800070530     c                   clear                   totmzdis
031900070530     c                   clear                   totmzaz
032000070530     c                   clear                   primo
032100100323     c     *loval        setll     waitra1f
032200070530     c                   do        *hival
032300100323     c                   read      waitra1f                               98
032400070530     c   98              leave
032500070530      * filtro area (solo x sede)
032600070530     c                   if        varea <> *blank
032700070530     c                   move      traarea       traareaa          3
032800070530     c                   if        traareaa <> varea
032900070530     c                   iter
033000070530     c                   endif
033100070530     c                   endif
033200070530      * filtro distretto (solo x sede)
033300070530     c                   if        vdist <> *blank and tradist <> vdist
033400070530     c                   iter
033500070530     c                   endif
033600070530      *
033700070530     c                   if        primo = *blank
033800070530     c                   move      trafil        exfil
033900070530     c                   move      traarea       excar
034000070530     c                   move      tradist       exfl3
034100070530     c                   move      'X'           primo
034200070530     c                   endif
034300070530      * rottura filiale
034400070530     c                   if        trafil  <> exfil
034500091216     c                   if        vstampa = 'S'
034600070530     c                   if        line > 60
034700070530     c                   except    testafil
034800070530     c                   endif
034900091216     c                   endif
035000100114      *
035100070530     c     exfil         chain     azorg01l
035200091216     c                   if        vstampa = 'S'
035300070530     c                   except    totfil
035400091216     c                   endif
035500070530     c                   clear                   totmzfil
035600070530     c                   move      trafil        exfil
035700070530     c                   endif
035800070530      *
035900070530      * rottura area
036000070530     c                   if        traarea <> excar
036100091216     c                   if        vstampa = 'S'
036200070530     c                   if        line > 60
036300070530     c                   except    testafil
036400070530     c                   endif
036500091216     c                   endif
036600091216      *
036700070530      * descrizione area
036800070530     c                   clear                   exaread20
036900070530     c                   movel     excar         kkey
037000070530     c                   move      '05'          kcod
037100070530     c     ktab          chain     tabel00f
037200070530     c                   if        %found(tabel00f)
037300070530     c                   movel     tbluni        exaread20
037400070530     c                   endif
037500091216      *
037600091216     c                   if        vstampa = 'S'
037700070530     c                   except    detarea
037800091216     c                   endif
037900070530     c                   clear                   totmzarea
038000070530     c                   move      traarea       excar
038100070530     c   66              z-add     99            line
038200070530     c                   endif
038300070530      *
038400070530      * rottura distretto
038500070530     c                   if        tradist <> exfl3
038600091216     c                   if        vstampa = 'S'
038700070530     c                   if        line > 60
038800070530     c                   except    testafil
038900070530     c                   endif
039000091216     c                   endif
039100091216      *
039200070530     c                   clear                   exdistd20
039300070530     c                   movel(p)  exfl3         kkey
039400070530     c                   move      '17'          kcod
039500070530     c     ktab          chain     tabel00f
039600070530     c                   if        %found(tabel00f)
039700070530     c                   movel     tbluni        exdistd20
039800070530     c                   endif
039900091216      *
040000091216     c                   if        vstampa = 'S'
040100091216      *
040200070530     c                   except    detdist
040300091216      *
040400091216     c                   endif
040500091216      *
040600070530     c                   clear                   totmzdis
040700070530     c                   move      tradist       exfl3
040800070530     c   66              z-add     99            line
040900070530     c                   endif
041000070530      * dettaglio  p.o
041100091216      *
041200091216     c                   if        vstampa = 'S'
041300091216      *
041400070530     c                   if        line > 60
041500070530     c                   except    testafil
041600070530     c                   endif
041700091216      *
041800091216     c                   endif
041900091216      *
042000070530     c     exfil         chain     azorg01l
042100070606     c                   clear                   tradfis
042200070606     c                   setoff                                       99
042300070606      * data disaccreditamento da stampare per turnover
042400100323     c                   if        tradfi > 0  and vtradfi = 'S'
042500070606     c                   seton                                        99
042600070606     c                   move      tradfi        dataiso
042700070606     c                   move      dataiso       dataeur
042800070606     c                   move      dataeur       tradfis           8 0
042900070606     c                   endif
043000091216      *
043100091216     c                   if        vstampa = 'S'
043200091216      *
043300070530     c                   except    detfil
043400091216      *
043500091216     c                   end
043600091216      *
043700070530      * incremento totale mezzi per area e filiale
043800070530     c                   add       1             totmzfil
043900070530     c                   add       1             totmzarea
044000070530     c                   add       1             totmzdis
044100070530     c                   add       1             totmzaz
044200070530
044300070530     c                   enddo
044400070530
044500070530      * fine file controlli finali x stampa totali
044600091216      *
044700091216     c                   if        vstampa = 'S'
044800091216      *
044900070531     c*                  if        lisarea = 'S' and *in80 or
045000070531     c*                            lisdist = 'S' and *in81
045100070530     c                   if        line > 60
045200070530     c                   except    testafil
045300070530     c                   endif
045400091216      *
045500091216     c                   endif
045600091216      *
045700070530      * descrizione area
045800070530     c                   clear                   exaread20
045900070530     c                   movel     excar         kkey
046000070530     c                   move      '05'          kcod
046100070530     c     ktab          chain     tabel00f
046200070530     c                   if        %found(tabel00f)
046300070530     c                   movel     tbluni        exaread20
046400070530     c                   endif
046500091216      *
046600091216     c                   if        vstampa = 'S'
046700091216      *
046800070530     c                   except    totfil
046900091216      *
047000091216     c                   end
047100091216      *
047200070530      * stampo totale area finale
047300091216      *
047400091216     c                   if        vstampa = 'S'
047500091216      *
047600070530     c                   if        *in80 or *in81 or *in82 and v1cfil = 0
047700070530     c                   except    detarea
047800070530     c                   endif
047900091216      *
048000091216     c                   endif
048100091216      *
048200070530      * stampo totale distretto finale
048300070530     c                   if        *in81 or *in82 and v1cfil = 0
048400070530     c                   clear                   exdistd20
048500070530     c                   movel(p)  exfl3         kkey
048600070530     c                   move      '17'          kcod
048700070530     c     ktab          chain     tabel00f
048800070530     c                   if        %found(tabel00f)
048900070530     c                   movel     tbluni        exdistd20
049000070530     c                   endif
049100091216      *
049200091216     c                   if        vstampa = 'S'
049300091216      *
049400070530     c                   except    detdist
049500091216      *
049600091216     c                   endif
049700091216      *
049800070530     c                   endif
049900070530      * stampo totale aziendale solo se richiesta globale di sede
050000091216      *
050100091216     c                   if        vstampa = 'S'
050200091216      *
050300070530     c                   if        *in82 and v1cfil = 0
050400070530     c                   except    detaz
050500070530     c                   endif
050600091216      *
050700091216     c                   endif
050800091216      *
050900070530
051000070531     c*                  endif
051100070530     c                   endsr
051200070530      *
051300050802      *
051400031105     c*-------------------------------------------------
051500031105     c     controv1      begsr
051600031105     c*-------------------------------------------------
051700031105     c                   clear                   v1dfil
051800070606     c                   setoff                                       9697
051900051122     c                   setoff                                       959493
052000051122     c                   setoff                                       92
052100031105     c                   if        v1cfil > 0
052200051122
052300050623     c     v1cfil        chain     azorg01l
052400050623     c                   if        %found(azorg01l)
052500031105     c                   movel     orgdes        v1dfil
052600031105     c                   else
052700031105     c                   seton                                        9796
052800051122     c                   goto      endcontr
052900031105     c                   endif
053000051122      * se profilo di area verifico se area di appartenenza
053100051122     c                   if        *in80
053200051122     c                   if        orgcar <> o36car
053300051122     c                   seton                                        9396
053400051122     c                   goto      endcontr
053500051122     c                   endif
053600051122     c                   endif
053700051122      * se profilo di distretto verifico se di appartenenza
053800051122     c                   if        *in81
053900051122     c                   if        orgfl3 <> o36dis
054000051122     c                   seton                                        9296
054100051122     c                   goto      endcontr
054200051122     c                   endif
054300051122     c                   endif
054400051122      *
054500031105     c                   endif
054600051122      * controllo congruenza selezioni a video
054700051122     c                   if        v1cfil > 0 and
054800051122     c                             (varea <> *blank or vdist <> *blank)
054900051122     c                   seton                                        9596
055000051122     c                   goto      endcontr
055100051122     c                   endif
055200051122      *
055300051122     c                   if        varea <> *blank and vdist <> *blank
055400051122     c                   seton                                        9496
055500051122     c                   goto      endcontr
055600051122     c                   endif
055700051121      * x sede parzializza area o distretto
055800051121     C     varea         ifeq      '?'
055900051121     C                   movel     '05'          kcod
056000100324     C                   clear                   kkey
056100051121     C                   call      'X§TABER'
056200051121     C                   parm                    kkut
056300051121     C                   parm                    kcod
056400051121     C                   parm                    kkey
056500051121     C                   parm                    §des             30
056600051121     c                   movel     §des          varead
056700051121     c                   movel     kkey          varea
056800051121     C*
056900051121     C                   else
057000051121     C*
057100051121     C                   if        varea <> *blank and varea <> *zero
057200051121     C                   movel     '05'          kcod
057300051121     C                   movel(p)  varea         kkey
057400051121     C     ktab          chain     tabel00f
057500051121     c                   if        %found(tabel00f) and tblflg = *blank
057600051121     C                   movel     tbluni        varead
057700051121     c                   else
057800051121     c                   movel     *all'?'       varead
057900051121     C                   end
058000051121     C                   end
058100051121     C                   end
058200051121      * distretto
058300051121     C     vdist         ifeq      '?'
058400051121     C                   movel     '17'          kcod
058500051121     C                   clear                   kkey
058600051121     C                   call      'X§TABER'
058700051121     C                   parm                    kkut
058800051121     C                   parm                    kcod
058900051121     C                   parm                    kkey
059000051121     C                   parm                    §des
059100051121     c                   movel     §des          vdistd
059200051121     c                   movel     kkey          vdist
059300051121     C*
059400051121     C                   else
059500051121     C*
059600051121     C                   if        vdist <> *blank and vdist <> *zero
059700051121     C                   movel     '17'          kcod
059800051121     C                   movel(p)  vdist         kkey
059900051121     C     ktab          chain     tabel00f
060000051121     c                   if        %found(tabel00f) and tblflg = *blank
060100051121     C                   movel     tbluni        vdistd
060200051121     c                   else
060300051121     c                   movel     *all'?'       vdistd
060400051121     C                   end
060500051121     C                   end
060600051121     C                   end
060700091216      *
060800070529      *allestimento (tabella)
060900070529     c                   setoff                                       45
061000070529     c                   if        vtrafur = '?'
061100070529     c                   clear                   tibs02ds
061200070529     c                   movel     'R'           t02mod
061300070529     c                   movel     knsif         t02sif
061400070529     c                   movel     'ALL'         t02cod
061500070529     c                   call      'TIBS02R'
061600070529     c                   parm                    KPJBA
061700070529     c                   parm                    TIBS02DS
061800070529     c                   if        t02err =  *blanks
061900070529     c                   movel     t02uni        vtrafurd
062000070529     c                   movel     t02ke1        vtrafur
062100070529     c                   goto      endcontr
062200070529     c                   endif
062300070529     c                   else
062400070529     c                   if        vtrafur <> *blank
062500070529     c                   clear                   tibs02ds
062600070529     c                   movel     'C'           t02mod
062700070529     c                   movel     knsif         t02sif
062800070529     c                   movel     'ALL'         t02cod
062900070529     c                   movel     vtrafur       t02ke1
063000070529     c                   call      'TIBS02R'
063100070529     c                   parm                    KPJBA
063200070529     c                   parm                    TIBS02DS
063300070529     c                   if        t02err =  *blanks
063400070529     c                   movel     t02uni        vtrafurd
063500070529     c                   else
063600070529     c                   seton                                        4596
063700070529     c                   goto      endcontr
063800070529     c                   endif
063900070529     c                   endif
064000070529     c                   endif
064100100323      *allestimento (tabella)
064200100323     c                   setoff                                       41
064300100323     c                   if        vtracen = '?'
064400100323     c                   clear                   tibs02ds
064500100323     c                   movel     'R'           t02mod
064600100323     c                   movel     knsif         t02sif
064700100323     c                   movel     'CEN'         t02cod
064800100323     c                   call      'TIBS02R'
064900100323     c                   parm                    KPJBA
065000100323     c                   parm                    TIBS02DS
065100100323     c                   if        t02err =  *blanks
065200100323     c                   movel     t02uni        vtracend
065300100323     c                   movel     t02ke1        vtracen
065400100323     c                   goto      endcontr
065500100323     c                   endif
065600100323     c                   else
065700100323     c                   if        vtracen <> *blank
065800100323     c                   clear                   tibs02ds
065900100323     c                   movel     'C'           t02mod
066000100323     c                   movel     knsif         t02sif
066100100323     c                   movel     'CEN'         t02cod
066200100323     c                   movel     vtracen       t02ke1
066300100323     c                   call      'TIBS02R'
066400100323     c                   parm                    KPJBA
066500100323     c                   parm                    TIBS02DS
066600100323     c                   if        t02err =  *blanks
066700100323     c                   movel     t02uni        vtracend
066800100323     c                   else
066900100323     c                   seton                                        4196
067000100323     c                   goto      endcontr
067100100323     c                   endif
067200100323     c                   endif
067300100323     c                   endif
067400070529      * antifurto vano carico
067500070529     c                   setoff                                       46
067600070529     c                   if        vtrabpo = '?  '
067700070529     c                   clear                   tibs02ds
067800070529     c                   movel     'R'           t02mod
067900070529     c                   movel     knsif         t02sif
068000070529     c                   movel     'ANT'         t02cod
068100070529     c                   movel(p)  'VAN'         t02ke1
068200070529     c                   call      'TIBS02R'
068300070529     c                   parm                    KPJBA
068400070529     c                   parm                    TIBS02DS
068500070529     c                   if        t02err =  *blanks
068600070529     c                   movel     t02uni        vtrabpod
068700070529     c                   movel     t02ke2        vtrabpo
068800070529     c                   endif
068900070529     c                   else
069000070529     c                   if        vtrabpo <> *blank
069100070529     c                   clear                   tibs02ds
069200070529     c                   movel     'C'           t02mod
069300070529     c                   movel     knsif         t02sif
069400070529     c                   movel     'ANT'         t02cod
069500070529     c                   movel     'VAN'         t02ke1
069600070529     c                   movel     vtrabpo       t02ke2
069700070529     c                   call      'TIBS02R'
069800070529     c                   parm                    KPJBA
069900070529     c                   parm                    TIBS02DS
070000070529     c                   if        t02err =  *blanks
070100070529     c                   movel     t02uni        vtrabpod
070200070529     c                   else
070300070529     c                   seton                                        4696
070400070529     c                   goto      endcontr
070500070529     c                   endif
070600070529     c                   endif
070700070529     c                   endif
070800070529      * antifurto blocco motore
070900070529     c                   setoff                                       47
071000070529     c                   if        vtraant = '?  '
071100070529     c                   clear                   tibs02ds
071200070529     c                   movel     'R'           t02mod
071300070529     c                   movel     knsif         t02sif
071400070529     c                   movel     'ANT'         t02cod
071500070529     c                   movel(p)  'MOT'         t02ke1
071600070529     c                   call      'TIBS02R'
071700070529     c                   parm                    KPJBA
071800070529     c                   parm                    TIBS02DS
071900070529     c                   if        t02err =  *blanks
072000070529     c                   movel     t02uni        vtraantd
072100070529     c                   movel     t02ke2        vtraant
072200070529     c                   endif
072300070529     c                   else
072400070529     c                   if        vtraant <> *blank
072500070529     c                   clear                   tibs02ds
072600070529     c                   movel     'C'           t02mod
072700070529     c                   movel     knsif         t02sif
072800070529     c                   movel     'ANT'         t02cod
072900070529     c                   movel     'MOT'         t02ke1
073000070529     c                   movel     vtraant       t02ke2
073100070529     c                   call      'TIBS02R'
073200070529     c                   parm                    KPJBA
073300070529     c                   parm                    TIBS02DS
073400070529     c                   if        t02err =  *blanks
073500070529     c                   movel     t02uni        vtraantd
073600070529     c                   else
073700070529     c                   seton                                        4796
073800070529     c                   goto      endcontr
073900070529     c                   endif
074000070529     c                   endif
074100070529     c                   endif
074200051121
074300051122     c     endcontr      endsr
074400050802     c*-------------------------------------------------
074500050802     c     comandi       begsr
074600050802     c*-------------------------------------------------
074700100323     C                   close     waitra1f
074800050802      *
074900070608     C                   z-add     99            luncmd
075000050802     C                   CALL      'QCMDEXC'
075100050802     C                   parm                    coman
075200050802     C                   parm                    luncmd
075300050802      *
075400100323     C  nkc              OPEN      waitra1f
075500050802     c                   endsr
075600070608     c*-------------------------------------------------
075700070608     c     comandi2      begsr
075800070608     c*-------------------------------------------------
075900070608      *
076000070608     C                   z-add     99            luncmd
076100070608     C                   CALL      'QCMDEXC'                            34
076200070608     C                   parm                    coman
076300070608     C                   parm                    luncmd
076400070608      *
076500070608     c                   endsr
076600011026     c*-------------------------------------------------
076700011026     c     *inzsr        begsr
076800011026     c*-------------------------------------------------
076900020304     c     *entry        plist
077000020304     c                   parm                    kpjba
077100050801
077200050801     c     ktab          klist
077300050801     c                   kfld                    kkut              1 0
077400050801     c                   kfld                    kcod              2
077500050801     c                   kfld                    kkey              8
077600050801     c                   z-add     1             kkut
077700050801     c                   move      '05'          kcod
077800050801     c
077900050801
078000020218     C                   time                    w0120            14 0
078100020218     C                   move      w0120         wdat              8 0
078200020404     c                   z-add     99            line
078300030630      *profilo di filiale estraggo solo quelli relativi al P.O.
078400030630     C                   CLEAR                   Tibs36Ds
078500030630     C                   EVAL      I36ute = knmus
078600030630     C                   CALL      'TIBS36R'
078700030630     C                   PARM                    tibs36ds
078800051017      *se profilo di filiale per pilotare le letture solo sul P.O
078900050802     c                   clear                   lisarea
079000050802     c                   clear                   lisdist
079100080502     c                   setoff                                       84
079200051017     c                   eval      saltp = 'N'
079300080502      *
079400051017      * x filiale duplica file di work in qtemp
079500051017     c                   movea     cmd2          coman
079600051017     c                   exsr      comandi
079700080502      *
079800030630     c                   if        o36pou <> 046
079900050805     c                   move      o36pou        v1cfil
080000080502      *
080100050802      * verifica se profili di filiale con autorizzazione a area o distretto
080200050802     c                   if        %subst(knmus:1:3) ='REA'
080300050802     c                   seton                                        80
080400050802     c                   move      'S'           lisarea
080500050802     c                   endif
080600050802     c
080700050802     c                   if        %subst(knmus:1:3) ='RED'
080800050802     c                   seton                                        81
080900050802     c                   move      'S'           lisdist
081000050802     c                   endif
081100080502     c
081200080502     c                   if        %subst(knmus:1:3) ='COA'
081300080502     c                   seton                                        84
081400080502     c                   endif
081500080502      *
081600051018     c                   else
081700051018      *profilo di sede
081800051018     c                   seton                                        82
081900030630     c                   endif
082000050802
082100050805     c                   if        v1cfil <> 0
082200031105     c     v1cfil        chain     azorg01l
082300031105     c                   if        %found(azorg01l)
082400031105     c                   movel     orgdes        v1dfil
082500050802     c                   move      orgcar        warea
082600050802     c                   move      orgfl3        wdist
082700031105     c                   endif
082800050805     c                   endif
082900070608      * creo nome file per dati utente in libreria di wrk
083000100325     c                   eval      file = 'AS' + knmus
083100011026     c                   endsr
083200050802      *---------------------------------------------------------
083300070531     Oqsysprt   E            legenda        1 02
083400100324     o                                           10 'TRMZA2R'
083500070531     o                       knmus               22
083600070606     o                                        +   1 'Lista Automezzi/Autisti -
083700070606     o                                              di città  PARAMETRI'
083800070531     o                       udate         y  +   3
083900070531     O                                        +   1 'Pag.'
084000070531     O                       page          z  +   1
084100070531     O          E            legenda     2
084200070531     o                                            8 'Filiale'
084300070531     o                       v1cfil           +   2
084400070531     o                       v1dfil           +   1
084500070531     o                                        +   2 'Tutta l''area'
084600070531     o                       lisarea          +   1
084700070531     o                                        +   2 'Tutto distretto'
084800070531     o                       lisdist          +   1
084900070531     o                                        +   2 'Solo area'
085000070531     o                       varea            +   1
085100070531     o                       varead           +   1
085200070531     o                                        +   2 'Solo distr.'
085300070531     o                       vdist            +   1
085400070531     o                       vdistd           +   1
085500070531     O          E            legenda     0
085600070531     o                                            8 'Filiale'
085700070531     o                       v1cfil           +   2
085800070531     o                       v1dfil           +   1
085900070531     o                                        +   2 'Tutta l''area'
086000070531     o                       lisarea          +   1
086100070531     o                                        +   2 'Tutto distretto'
086200070531     o                       lisdist          +   1
086300070531     o                                        +   2 'Solo area'
086400070531     o                       varea            +   1
086500070531     o                       varead           +   1
086600070531     o                                        +   2 'Solo distr.'
086700070531     o                       vdist            +   1
086800070531     o                       vdistd           +   1
086900070531     O          E            legenda     0
087000070531     o                                            8 'Filiale'
087100070531     o                       v1cfil           +   2
087200070531     o                       v1dfil           +   1
087300070531     o                                        +   2 'Tutta l''area'
087400070531     o                       lisarea          +   1
087500070531     o                                        +   2 'Tutto distretto'
087600070531     o                       lisdist          +   1
087700070531     o                                        +   2 'Solo area'
087800070531     o                       varea            +   1
087900070531     o                       varead           +   1
088000070531     o                                        +   2 'Solo distr.'
088100070531     o                       vdist            +   1
088200070531     o                       vdistd           +   1
088300081204     O          E            legenda     2
088400081204     o                                           50 'Tipo Autista ..........'
088500081204     o                       vtpaut           +   2
088600070531     O          E            legenda     2
088700070531     o                                           50 'Autista di riserva ....'
088800070531     o                       vtraris          +   2
088900100406     O          E            legenda     2
089000100406     o                                           50 'Anche disaccreditati...'
089100100406     o                       vtradfi          +   2
089200070531     O          E            legenda     2
089300070531     o                                           50 'Mezzo di scorta........'
089400070531     o                       vtrasco          +   2
089500070531     O          E            legenda     2
089600070531     o                                           50 'Sponda idraulica.......'
089700070531     o                       vtraspi          +   2
089800100406     O          E            legenda     2
089900100406     o                                           50 'Con periferica satel...'
090000100406     o                       vtraper          +   2
090100100406     O          E            legenda     2
090200100406     o                                           50 'Con targa inserita.....'
090300100406     o                       vtrataa          +   2
090400100406     O          E            legenda     2
090500100406     o                                           50 'Centrale contr.satel...'
090600100406     o                       vtracen          +   2
090700070531     O          E            legenda     2
090800070531     o                                           50 'Allestimento ..........'
090900070531     o                       vtrafur          +   2
091000070531     o                       vtrafurd         +   2
091100070531     O          E            legenda     2
091200070531     o                                           50 'Antifurto blocca porte.'
091300070531     o                       vtrabpo          +   2
091400070531     o                       vtrabpod         +   2
091500070531     O          E            legenda     2
091600070531     o                                           50 'Antifurto blocco motore'
091700070531     o                       vtraant          +   2
091800070531     o                       vtraantd         +   2
091900070531      *----------------------------------------------------------
092000070531     O          E            testafil       1 02
092100100324     o                                           10 'TRMZA2R'
092200050802     o                       knmus               22
092300070606     o                                        +   1 'Lista Automezzi/Autisti -
092400070606     o                                              di città ELENCO  '
092500050802     o                       udate         y  +   3
092600050802     O                                        +   1 'Pag.'
092700050802     O                       page          z  +   1
092800050803     O          E            testafil    2  1
092900050803     o                                           16 'Societa'
093000050803     o                                           42 'Autista'
093100050803     o                                           66 'Mezzo'
093200050803     o                                           84 'Targa'
093300070605     o                                          102 'Data Disaccr.'
093400050802     O          E            detfil      1
093500051122     o                       TRASOC           +   9
093600050802     o                       TRAAUT           +   1
093700050802     o                       TRATIA           +   1
093800050802     o                       TRASCO           +   1
093900050802     o                       TRATAA           +   1
094000070605     o               99      TRAdfis            100 '  /  /    '
094100050803     O          E            totfil      1  1
094200050803     o                                              'Totale P.O '
094300050803     o                       exfil            +   1
094400051121     o                       orgdes           +   1
094500051122     o                       totmzfil      z  +   4
094600050803     O          E            detarea     1  1
094700050803     o                                              'Totale area'
094800050803     o                       excar            +   1
094900051121     o                       exaread20        +   1
095000051122     o                       totmzarea     z  +   4
095100050803     O          E            detdist     1
095200050803     o                                              'Totale distretto'
095300050803     o                       exfl3            +   1
095400051121     o                       exdistd20        +   1
095500051121     o                       totmzdis      z  +   1
095600051018     O          E            detaz       1
095700051122     o                                              'Totale azienda'
095800051122     o                       totmzaz       z     45
095900050802**  CMD1
096000100323CLRPFM    FILE(WAITRA1F)
096100050802**  CMD2
096200100323CRTDUPOBJ OBJ(WAITRA1F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
096300050802**  CMD3
096400100323DLTF FILE(QTEMP/WAITRA1F)
096500070608**  CMD4
096600160115DLTF   FILE(WRKQUERY/xxxxxxxxxx)
096700070608**  CMD5
096800100323CRTDUPOBJ OBJ(WAITRA1F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xxxxxxxxxx) DATA(*YES)
