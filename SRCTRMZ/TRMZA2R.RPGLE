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
017900170221      * Senza Automezzo   (ex aut.di Riserva)
018000070530     c                   if        vtraris <> *blank
018100170221     c                   if        vtraris = 'S' and traris = *blank or
018200170221     c                             vtraris = 'N' and traris <> *blank
018300070530     c                   iter
018400070530     c                   endif
018500070531     c                   endif
018600170626      * Picco Lavorativo
018700170626     c                   if        vtraatp <> *blank
018800170626     c                   if        vtraatp = 'S' and traatp = *blank or
018900170626     c                             vtraatp = 'N' and traatp <> *blank
019000170626     c                   iter
019100170626     c                   endif
019200170626     c                   endif
019300070530      * Mezzo di scorta
019400070530     c                   if        vtrasco <> *blank
019500070530     c                   if        vtrasco = 'S' and trasco = *blank or
019600070530     c                             vtrasco = 'N' and trasco <> *blank
019700070530     c                   iter
019800070530     c                   endif
019900070531     c                   endif
020000070530      * sponda idraulica
020100070530     c                   if        vtraspi <> *blank
020200070530     c                   if        vtraspi = 'S' and traspi <> 'S' or
020300070530     c                             vtraspi = 'N' and traspi = 'S'
020400070530     c                   iter
020500070530     c                   endif
020600070531     c                   endif
020700100323      * periferica satellitare
020800100323     c                   if        vtraper = 'S' and traper = *blank
020900070606     c                   iter
021000070606     c                   endif
021100100323     c                   if        vtraper = 'N' and traper <> *blank
021200100323     c                   iter
021300100323     c                   endif
021400100323      * centrale controllo satellitare
021500100323     c                   if        vtracen <> *blank
021600100323     c                   if        vtracen  <> tracen
021700100323     c                   iter
021800100323     c                   endif
021900100323     c                   endif
022000070530      * allestimento
022100070530     c                   if        vtrafur <> *blank
022200070530     c                   if        vtrafur  <> trafur
022300070530     c                   iter
022400070530     c                   endif
022500070531     c                   endif
022600100402      * con targa inserita
022700100402     c                   if        vtrataa <> *blank
022800100402     c                   if        trataa  =  *blank
022900100402     c                   iter
023000100402     c                   endif
023100100402     c                   endif
023200070530      * antifurto blocca porte
023300070530     c                   if        vtrabpo <> *blank
023400070530     c                   if        vtrabpo  <> trabpo
023500070530     c                   iter
023600070530     c                   endif
023700070531     c                   endif
023800070530      * antifurto blocco motore
023900070530     c                   if        vtraant <> *blank
024000070530     c                   if        vtraant  <> traant
024100070530     c                   iter
024200070530     c                   endif
024300070531     c                   endif
024400160113      *
024500160113     c                   clear                   tibs02ds
024600160113     c                   movel     'C'           t02mod
024700160113     c                   movel     knsif         t02sif
024800160113     c                   movel     'CEN'         t02cod
024900160113     c                   movel     tracen        t02ke1
025000160113     c                   call      'TIBS02R'
025100160113     c                   parm                    KPJBA
025200160113     c                   parm                    TIBS02DS
025300160113     c                   if        t02err =  *blanks
025400160113     c                   movel     t02uni        tracend
025500160113     c                   else
025600160113     c                   clear                   tracend
025700160113     c                   endif
025800160113      *decodifica CCNL
025900160113     c                   clear                   traCNLd
026000160113     c                   if        traCNL <> *blank
026100160113     c                   clear                   tibs02ds
026200160113     c                   movel     'C'           t02mod
026300160113     c                   movel     knsif         t02sif
026400160113     c                   movel     'CNL'         t02cod
026500160113     c                   movel     traCNL        t02ke1
026600160113     c                   call      'TIBS02R'
026700160113     c                   parm                    KPJBA
026800160113     c                   parm                    TIBS02DS
026900160113     c                   if        t02err =  *blanks
027000160113     c                   movel     t02uni        traCNLd
027100160113     c                   endif
027200160113     c                   endif
027300160113      *decodifica QUALIFICA
027400160113     c                   clear                   traQUAd
027500160113     c                   if        traQUA <> *blank
027600160113     c                   clear                   tibs02ds
027700160113     c                   movel     'C'           t02mod
027800160113     c                   movel     knsif         t02sif
027900160113     c                   movel     'QUA'         t02cod
028000160113     c                   movel     traQUA        t02ke1
028100160113     c                   call      'TIBS02R'
028200160113     c                   parm                    KPJBA
028300160113     c                   parm                    TIBS02DS
028400160113     c                   if        t02err =  *blanks
028500160113     c                   movel     t02uni        traQUAd
028600160113     c                   endif
028700160113     c                   endif
028800160113      *decodifica LIVELLO
028900160113     c                   clear                   traLIQd
029000160113     c                   if        traLIQ <> *blank
029100160113     c                   clear                   tibs02ds
029200160113     c                   movel     'C'           t02mod
029300160113     c                   movel     knsif         t02sif
029400160113     c                   movel     'LIQ'         t02cod
029500160113     c                   movel     traLIQ        t02ke1
029600160113     c                   call      'TIBS02R'
029700160113     c                   parm                    KPJBA
029800160113     c                   parm                    TIBS02DS
029900160113     c                   if        t02err =  *blanks
030000160113     c                   movel     t02uni        traLIQd
030100160113     c                   endif
030200160113     c                   endif
030300050801      * scrive W.F
030400050801     c                   move      orgcar        traarea
030500050801     c                   move      orgfl3        tradist
030600100331     c                   movel     orgdes        trafild
030700100331      *
030800100331     c                   clear                   traaread
030900100331     c                   movel     orgcar        kkey
031000100331     c                   move      '05'          kcod
031100100331     c     ktab          chain     tabel00f
031200100331     c                   if        %found(tabel00f)
031300100331     c                   movel     tbluni        traaread
031400100331     c                   endif
031500050801     c                   write     waitra00
031600050623     c                   enddo
031700020304
031800020304     c                   endsr
031900981029      *
032000050802     c*-------------------------------------------------
032100050802     c     waitra        begsr
032200050802     c*-------------------------------------------------
032300070530     c                   clear                   totmzfil
032400070530     c                   clear                   totmzarea
032500070530     c                   clear                   totmzdis
032600070530     c                   clear                   totmzaz
032700070530     c                   clear                   primo
032800100323     c     *loval        setll     waitra1f
032900070530     c                   do        *hival
033000100323     c                   read      waitra1f                               98
033100070530     c   98              leave
033200070530      * filtro area (solo x sede)
033300070530     c                   if        varea <> *blank
033400070530     c                   move      traarea       traareaa          3
033500070530     c                   if        traareaa <> varea
033600070530     c                   iter
033700070530     c                   endif
033800070530     c                   endif
033900070530      * filtro distretto (solo x sede)
034000070530     c                   if        vdist <> *blank and tradist <> vdist
034100070530     c                   iter
034200070530     c                   endif
034300070530      *
034400070530     c                   if        primo = *blank
034500070530     c                   move      trafil        exfil
034600070530     c                   move      traarea       excar
034700070530     c                   move      tradist       exfl3
034800070530     c                   move      'X'           primo
034900070530     c                   endif
035000070530      * rottura filiale
035100070530     c                   if        trafil  <> exfil
035200091216     c                   if        vstampa = 'S'
035300070530     c                   if        line > 60
035400070530     c                   except    testafil
035500070530     c                   endif
035600091216     c                   endif
035700100114      *
035800070530     c     exfil         chain     azorg01l
035900091216     c                   if        vstampa = 'S'
036000070530     c                   except    totfil
036100091216     c                   endif
036200070530     c                   clear                   totmzfil
036300070530     c                   move      trafil        exfil
036400070530     c                   endif
036500070530      *
036600070530      * rottura area
036700070530     c                   if        traarea <> excar
036800091216     c                   if        vstampa = 'S'
036900070530     c                   if        line > 60
037000070530     c                   except    testafil
037100070530     c                   endif
037200091216     c                   endif
037300091216      *
037400070530      * descrizione area
037500070530     c                   clear                   exaread20
037600070530     c                   movel     excar         kkey
037700070530     c                   move      '05'          kcod
037800070530     c     ktab          chain     tabel00f
037900070530     c                   if        %found(tabel00f)
038000070530     c                   movel     tbluni        exaread20
038100070530     c                   endif
038200091216      *
038300091216     c                   if        vstampa = 'S'
038400070530     c                   except    detarea
038500091216     c                   endif
038600070530     c                   clear                   totmzarea
038700070530     c                   move      traarea       excar
038800070530     c   66              z-add     99            line
038900070530     c                   endif
039000070530      *
039100070530      * rottura distretto
039200070530     c                   if        tradist <> exfl3
039300091216     c                   if        vstampa = 'S'
039400070530     c                   if        line > 60
039500070530     c                   except    testafil
039600070530     c                   endif
039700091216     c                   endif
039800091216      *
039900070530     c                   clear                   exdistd20
040000070530     c                   movel(p)  exfl3         kkey
040100070530     c                   move      '17'          kcod
040200070530     c     ktab          chain     tabel00f
040300070530     c                   if        %found(tabel00f)
040400070530     c                   movel     tbluni        exdistd20
040500070530     c                   endif
040600091216      *
040700091216     c                   if        vstampa = 'S'
040800091216      *
040900070530     c                   except    detdist
041000091216      *
041100091216     c                   endif
041200091216      *
041300070530     c                   clear                   totmzdis
041400070530     c                   move      tradist       exfl3
041500070530     c   66              z-add     99            line
041600070530     c                   endif
041700070530      * dettaglio  p.o
041800091216      *
041900091216     c                   if        vstampa = 'S'
042000091216      *
042100070530     c                   if        line > 60
042200070530     c                   except    testafil
042300070530     c                   endif
042400091216      *
042500091216     c                   endif
042600091216      *
042700070530     c     exfil         chain     azorg01l
042800070606     c                   clear                   tradfis
042900070606     c                   setoff                                       99
043000070606      * data disaccreditamento da stampare per turnover
043100100323     c                   if        tradfi > 0  and vtradfi = 'S'
043200070606     c                   seton                                        99
043300070606     c                   move      tradfi        dataiso
043400070606     c                   move      dataiso       dataeur
043500070606     c                   move      dataeur       tradfis           8 0
043600070606     c                   endif
043700091216      *
043800091216     c                   if        vstampa = 'S'
043900091216      *
044000070530     c                   except    detfil
044100091216      *
044200091216     c                   end
044300091216      *
044400070530      * incremento totale mezzi per area e filiale
044500070530     c                   add       1             totmzfil
044600070530     c                   add       1             totmzarea
044700070530     c                   add       1             totmzdis
044800070530     c                   add       1             totmzaz
044900070530
045000070530     c                   enddo
045100070530
045200070530      * fine file controlli finali x stampa totali
045300091216      *
045400091216     c                   if        vstampa = 'S'
045500091216      *
045600070531     c*                  if        lisarea = 'S' and *in80 or
045700070531     c*                            lisdist = 'S' and *in81
045800070530     c                   if        line > 60
045900070530     c                   except    testafil
046000070530     c                   endif
046100091216      *
046200091216     c                   endif
046300091216      *
046400070530      * descrizione area
046500070530     c                   clear                   exaread20
046600070530     c                   movel     excar         kkey
046700070530     c                   move      '05'          kcod
046800070530     c     ktab          chain     tabel00f
046900070530     c                   if        %found(tabel00f)
047000070530     c                   movel     tbluni        exaread20
047100070530     c                   endif
047200091216      *
047300091216     c                   if        vstampa = 'S'
047400091216      *
047500070530     c                   except    totfil
047600091216      *
047700091216     c                   end
047800091216      *
047900070530      * stampo totale area finale
048000091216      *
048100091216     c                   if        vstampa = 'S'
048200091216      *
048300070530     c                   if        *in80 or *in81 or *in82 and v1cfil = 0
048400070530     c                   except    detarea
048500070530     c                   endif
048600091216      *
048700091216     c                   endif
048800091216      *
048900070530      * stampo totale distretto finale
049000070530     c                   if        *in81 or *in82 and v1cfil = 0
049100070530     c                   clear                   exdistd20
049200070530     c                   movel(p)  exfl3         kkey
049300070530     c                   move      '17'          kcod
049400070530     c     ktab          chain     tabel00f
049500070530     c                   if        %found(tabel00f)
049600070530     c                   movel     tbluni        exdistd20
049700070530     c                   endif
049800091216      *
049900091216     c                   if        vstampa = 'S'
050000091216      *
050100070530     c                   except    detdist
050200091216      *
050300091216     c                   endif
050400091216      *
050500070530     c                   endif
050600070530      * stampo totale aziendale solo se richiesta globale di sede
050700091216      *
050800091216     c                   if        vstampa = 'S'
050900091216      *
051000070530     c                   if        *in82 and v1cfil = 0
051100070530     c                   except    detaz
051200070530     c                   endif
051300091216      *
051400091216     c                   endif
051500091216      *
051600070530
051700070531     c*                  endif
051800070530     c                   endsr
051900070530      *
052000050802      *
052100031105     c*-------------------------------------------------
052200031105     c     controv1      begsr
052300031105     c*-------------------------------------------------
052400031105     c                   clear                   v1dfil
052500070606     c                   setoff                                       9697
052600051122     c                   setoff                                       959493
052700051122     c                   setoff                                       92
052800031105     c                   if        v1cfil > 0
052900051122
053000050623     c     v1cfil        chain     azorg01l
053100050623     c                   if        %found(azorg01l)
053200031105     c                   movel     orgdes        v1dfil
053300031105     c                   else
053400031105     c                   seton                                        9796
053500051122     c                   goto      endcontr
053600031105     c                   endif
053700051122      * se profilo di area verifico se area di appartenenza
053800051122     c                   if        *in80
053900051122     c                   if        orgcar <> o36car
054000051122     c                   seton                                        9396
054100051122     c                   goto      endcontr
054200051122     c                   endif
054300051122     c                   endif
054400051122      * se profilo di distretto verifico se di appartenenza
054500051122     c                   if        *in81
054600051122     c                   if        orgfl3 <> o36dis
054700051122     c                   seton                                        9296
054800051122     c                   goto      endcontr
054900051122     c                   endif
055000051122     c                   endif
055100051122      *
055200031105     c                   endif
055300051122      * controllo congruenza selezioni a video
055400051122     c                   if        v1cfil > 0 and
055500051122     c                             (varea <> *blank or vdist <> *blank)
055600051122     c                   seton                                        9596
055700051122     c                   goto      endcontr
055800051122     c                   endif
055900051122      *
056000051122     c                   if        varea <> *blank and vdist <> *blank
056100051122     c                   seton                                        9496
056200051122     c                   goto      endcontr
056300051122     c                   endif
056400051121      * x sede parzializza area o distretto
056500051121     C     varea         ifeq      '?'
056600051121     C                   movel     '05'          kcod
056700100324     C                   clear                   kkey
056800051121     C                   call      'X§TABER'
056900051121     C                   parm                    kkut
057000051121     C                   parm                    kcod
057100051121     C                   parm                    kkey
057200051121     C                   parm                    §des             30
057300051121     c                   movel     §des          varead
057400051121     c                   movel     kkey          varea
057500051121     C*
057600051121     C                   else
057700051121     C*
057800051121     C                   if        varea <> *blank and varea <> *zero
057900051121     C                   movel     '05'          kcod
058000051121     C                   movel(p)  varea         kkey
058100051121     C     ktab          chain     tabel00f
058200051121     c                   if        %found(tabel00f) and tblflg = *blank
058300051121     C                   movel     tbluni        varead
058400051121     c                   else
058500051121     c                   movel     *all'?'       varead
058600051121     C                   end
058700051121     C                   end
058800051121     C                   end
058900051121      * distretto
059000051121     C     vdist         ifeq      '?'
059100051121     C                   movel     '17'          kcod
059200051121     C                   clear                   kkey
059300051121     C                   call      'X§TABER'
059400051121     C                   parm                    kkut
059500051121     C                   parm                    kcod
059600051121     C                   parm                    kkey
059700051121     C                   parm                    §des
059800051121     c                   movel     §des          vdistd
059900051121     c                   movel     kkey          vdist
060000051121     C*
060100051121     C                   else
060200051121     C*
060300051121     C                   if        vdist <> *blank and vdist <> *zero
060400051121     C                   movel     '17'          kcod
060500051121     C                   movel(p)  vdist         kkey
060600051121     C     ktab          chain     tabel00f
060700051121     c                   if        %found(tabel00f) and tblflg = *blank
060800051121     C                   movel     tbluni        vdistd
060900051121     c                   else
061000051121     c                   movel     *all'?'       vdistd
061100051121     C                   end
061200051121     C                   end
061300051121     C                   end
061400091216      *
061500070529      *allestimento (tabella)
061600070529     c                   setoff                                       45
061700070529     c                   if        vtrafur = '?'
061800070529     c                   clear                   tibs02ds
061900070529     c                   movel     'R'           t02mod
062000070529     c                   movel     knsif         t02sif
062100070529     c                   movel     'ALL'         t02cod
062200070529     c                   call      'TIBS02R'
062300070529     c                   parm                    KPJBA
062400070529     c                   parm                    TIBS02DS
062500070529     c                   if        t02err =  *blanks
062600070529     c                   movel     t02uni        vtrafurd
062700070529     c                   movel     t02ke1        vtrafur
062800070529     c                   goto      endcontr
062900070529     c                   endif
063000070529     c                   else
063100070529     c                   if        vtrafur <> *blank
063200070529     c                   clear                   tibs02ds
063300070529     c                   movel     'C'           t02mod
063400070529     c                   movel     knsif         t02sif
063500070529     c                   movel     'ALL'         t02cod
063600070529     c                   movel     vtrafur       t02ke1
063700070529     c                   call      'TIBS02R'
063800070529     c                   parm                    KPJBA
063900070529     c                   parm                    TIBS02DS
064000070529     c                   if        t02err =  *blanks
064100070529     c                   movel     t02uni        vtrafurd
064200070529     c                   else
064300070529     c                   seton                                        4596
064400070529     c                   goto      endcontr
064500070529     c                   endif
064600070529     c                   endif
064700070529     c                   endif
064800100323      *allestimento (tabella)
064900100323     c                   setoff                                       41
065000100323     c                   if        vtracen = '?'
065100100323     c                   clear                   tibs02ds
065200100323     c                   movel     'R'           t02mod
065300100323     c                   movel     knsif         t02sif
065400100323     c                   movel     'CEN'         t02cod
065500100323     c                   call      'TIBS02R'
065600100323     c                   parm                    KPJBA
065700100323     c                   parm                    TIBS02DS
065800100323     c                   if        t02err =  *blanks
065900100323     c                   movel     t02uni        vtracend
066000100323     c                   movel     t02ke1        vtracen
066100100323     c                   goto      endcontr
066200100323     c                   endif
066300100323     c                   else
066400100323     c                   if        vtracen <> *blank
066500100323     c                   clear                   tibs02ds
066600100323     c                   movel     'C'           t02mod
066700100323     c                   movel     knsif         t02sif
066800100323     c                   movel     'CEN'         t02cod
066900100323     c                   movel     vtracen       t02ke1
067000100323     c                   call      'TIBS02R'
067100100323     c                   parm                    KPJBA
067200100323     c                   parm                    TIBS02DS
067300100323     c                   if        t02err =  *blanks
067400100323     c                   movel     t02uni        vtracend
067500100323     c                   else
067600100323     c                   seton                                        4196
067700100323     c                   goto      endcontr
067800100323     c                   endif
067900100323     c                   endif
068000100323     c                   endif
068100070529      * antifurto vano carico
068200070529     c                   setoff                                       46
068300070529     c                   if        vtrabpo = '?  '
068400070529     c                   clear                   tibs02ds
068500070529     c                   movel     'R'           t02mod
068600070529     c                   movel     knsif         t02sif
068700070529     c                   movel     'ANT'         t02cod
068800070529     c                   movel(p)  'VAN'         t02ke1
068900070529     c                   call      'TIBS02R'
069000070529     c                   parm                    KPJBA
069100070529     c                   parm                    TIBS02DS
069200070529     c                   if        t02err =  *blanks
069300070529     c                   movel     t02uni        vtrabpod
069400070529     c                   movel     t02ke2        vtrabpo
069500070529     c                   endif
069600070529     c                   else
069700070529     c                   if        vtrabpo <> *blank
069800070529     c                   clear                   tibs02ds
069900070529     c                   movel     'C'           t02mod
070000070529     c                   movel     knsif         t02sif
070100070529     c                   movel     'ANT'         t02cod
070200070529     c                   movel     'VAN'         t02ke1
070300070529     c                   movel     vtrabpo       t02ke2
070400070529     c                   call      'TIBS02R'
070500070529     c                   parm                    KPJBA
070600070529     c                   parm                    TIBS02DS
070700070529     c                   if        t02err =  *blanks
070800070529     c                   movel     t02uni        vtrabpod
070900070529     c                   else
071000070529     c                   seton                                        4696
071100070529     c                   goto      endcontr
071200070529     c                   endif
071300070529     c                   endif
071400070529     c                   endif
071500070529      * antifurto blocco motore
071600070529     c                   setoff                                       47
071700070529     c                   if        vtraant = '?  '
071800070529     c                   clear                   tibs02ds
071900070529     c                   movel     'R'           t02mod
072000070529     c                   movel     knsif         t02sif
072100070529     c                   movel     'ANT'         t02cod
072200070529     c                   movel(p)  'MOT'         t02ke1
072300070529     c                   call      'TIBS02R'
072400070529     c                   parm                    KPJBA
072500070529     c                   parm                    TIBS02DS
072600070529     c                   if        t02err =  *blanks
072700070529     c                   movel     t02uni        vtraantd
072800070529     c                   movel     t02ke2        vtraant
072900070529     c                   endif
073000070529     c                   else
073100070529     c                   if        vtraant <> *blank
073200070529     c                   clear                   tibs02ds
073300070529     c                   movel     'C'           t02mod
073400070529     c                   movel     knsif         t02sif
073500070529     c                   movel     'ANT'         t02cod
073600070529     c                   movel     'MOT'         t02ke1
073700070529     c                   movel     vtraant       t02ke2
073800070529     c                   call      'TIBS02R'
073900070529     c                   parm                    KPJBA
074000070529     c                   parm                    TIBS02DS
074100070529     c                   if        t02err =  *blanks
074200070529     c                   movel     t02uni        vtraantd
074300070529     c                   else
074400070529     c                   seton                                        4796
074500070529     c                   goto      endcontr
074600070529     c                   endif
074700070529     c                   endif
074800070529     c                   endif
074900051121
075000051122     c     endcontr      endsr
075100050802     c*-------------------------------------------------
075200050802     c     comandi       begsr
075300050802     c*-------------------------------------------------
075400100323     C                   close     waitra1f
075500050802      *
075600070608     C                   z-add     99            luncmd
075700050802     C                   CALL      'QCMDEXC'
075800050802     C                   parm                    coman
075900050802     C                   parm                    luncmd
076000050802      *
076100100323     C  nkc              OPEN      waitra1f
076200050802     c                   endsr
076300070608     c*-------------------------------------------------
076400070608     c     comandi2      begsr
076500070608     c*-------------------------------------------------
076600070608      *
076700070608     C                   z-add     99            luncmd
076800070608     C                   CALL      'QCMDEXC'                            34
076900070608     C                   parm                    coman
077000070608     C                   parm                    luncmd
077100070608      *
077200070608     c                   endsr
077300011026     c*-------------------------------------------------
077400011026     c     *inzsr        begsr
077500011026     c*-------------------------------------------------
077600020304     c     *entry        plist
077700020304     c                   parm                    kpjba
077800050801
077900050801     c     ktab          klist
078000050801     c                   kfld                    kkut              1 0
078100050801     c                   kfld                    kcod              2
078200050801     c                   kfld                    kkey              8
078300050801     c                   z-add     1             kkut
078400050801     c                   move      '05'          kcod
078500050801     c
078600050801
078700020218     C                   time                    w0120            14 0
078800020218     C                   move      w0120         wdat              8 0
078900020404     c                   z-add     99            line
079000030630      *profilo di filiale estraggo solo quelli relativi al P.O.
079100030630     C                   CLEAR                   Tibs36Ds
079200030630     C                   EVAL      I36ute = knmus
079300030630     C                   CALL      'TIBS36R'
079400030630     C                   PARM                    tibs36ds
079500051017      *se profilo di filiale per pilotare le letture solo sul P.O
079600050802     c                   clear                   lisarea
079700050802     c                   clear                   lisdist
079800080502     c                   setoff                                       84
079900051017     c                   eval      saltp = 'N'
080000080502      *
080100051017      * x filiale duplica file di work in qtemp
080200051017     c                   movea     cmd2          coman
080300051017     c                   exsr      comandi
080400080502      *
080500030630     c                   if        o36pou <> 046
080600050805     c                   move      o36pou        v1cfil
080700080502      *
080800050802      * verifica se profili di filiale con autorizzazione a area o distretto
080900050802     c                   if        %subst(knmus:1:3) ='REA'
081000050802     c                   seton                                        80
081100050802     c                   move      'S'           lisarea
081200050802     c                   endif
081300050802     c
081400050802     c                   if        %subst(knmus:1:3) ='RED'
081500050802     c                   seton                                        81
081600050802     c                   move      'S'           lisdist
081700050802     c                   endif
081800080502     c
081900080502     c                   if        %subst(knmus:1:3) ='COA'
082000080502     c                   seton                                        84
082100080502     c                   endif
082200080502      *
082300051018     c                   else
082400051018      *profilo di sede
082500051018     c                   seton                                        82
082600030630     c                   endif
082700050802
082800050805     c                   if        v1cfil <> 0
082900031105     c     v1cfil        chain     azorg01l
083000031105     c                   if        %found(azorg01l)
083100031105     c                   movel     orgdes        v1dfil
083200050802     c                   move      orgcar        warea
083300050802     c                   move      orgfl3        wdist
083400031105     c                   endif
083500050805     c                   endif
083600070608      * creo nome file per dati utente in libreria di wrk
083700100325     c                   eval      file = 'AS' + knmus
083800011026     c                   endsr
083900050802      *---------------------------------------------------------
084000070531     Oqsysprt   E            legenda        1 02
084100100324     o                                           10 'TRMZA2R'
084200070531     o                       knmus               22
084300070606     o                                        +   1 'Lista Automezzi/Autisti -
084400070606     o                                              di città  PARAMETRI'
084500070531     o                       udate         y  +   3
084600070531     O                                        +   1 'Pag.'
084700070531     O                       page          z  +   1
084800070531     O          E            legenda     2
084900070531     o                                            8 'Filiale'
085000070531     o                       v1cfil           +   2
085100070531     o                       v1dfil           +   1
085200070531     o                                        +   2 'Tutta l''area'
085300070531     o                       lisarea          +   1
085400070531     o                                        +   2 'Tutto distretto'
085500070531     o                       lisdist          +   1
085600070531     o                                        +   2 'Solo area'
085700070531     o                       varea            +   1
085800070531     o                       varead           +   1
085900070531     o                                        +   2 'Solo distr.'
086000070531     o                       vdist            +   1
086100070531     o                       vdistd           +   1
086200070531     O          E            legenda     0
086300070531     o                                            8 'Filiale'
086400070531     o                       v1cfil           +   2
086500070531     o                       v1dfil           +   1
086600070531     o                                        +   2 'Tutta l''area'
086700070531     o                       lisarea          +   1
086800070531     o                                        +   2 'Tutto distretto'
086900070531     o                       lisdist          +   1
087000070531     o                                        +   2 'Solo area'
087100070531     o                       varea            +   1
087200070531     o                       varead           +   1
087300070531     o                                        +   2 'Solo distr.'
087400070531     o                       vdist            +   1
087500070531     o                       vdistd           +   1
087600070531     O          E            legenda     0
087700070531     o                                            8 'Filiale'
087800070531     o                       v1cfil           +   2
087900070531     o                       v1dfil           +   1
088000070531     o                                        +   2 'Tutta l''area'
088100070531     o                       lisarea          +   1
088200070531     o                                        +   2 'Tutto distretto'
088300070531     o                       lisdist          +   1
088400070531     o                                        +   2 'Solo area'
088500070531     o                       varea            +   1
088600070531     o                       varead           +   1
088700070531     o                                        +   2 'Solo distr.'
088800070531     o                       vdist            +   1
088900070531     o                       vdistd           +   1
089000081204     O          E            legenda     2
089100081204     o                                           50 'Tipo Autista ..........'
089200081204     o                       vtpaut           +   2
089300070531     O          E            legenda     2
089400170221     o                                           50 'Senza Automezzo .......'
089500070531     o                       vtraris          +   2
089600100406     O          E            legenda     2
089700100406     o                                           50 'Anche disaccreditati...'
089800100406     o                       vtradfi          +   2
089900170626     O          E            legenda     2
090000170626     o                                           50 'Picco Lavorativo.......'
090100170626     o                       vtraatp          +   2
090200070531     O          E            legenda     2
090300070531     o                                           50 'Mezzo di scorta........'
090400070531     o                       vtrasco          +   2
090500070531     O          E            legenda     2
090600070531     o                                           50 'Sponda idraulica.......'
090700070531     o                       vtraspi          +   2
090800100406     O          E            legenda     2
090900100406     o                                           50 'Con periferica satel...'
091000100406     o                       vtraper          +   2
091100100406     O          E            legenda     2
091200100406     o                                           50 'Con targa inserita.....'
091300100406     o                       vtrataa          +   2
091400100406     O          E            legenda     2
091500100406     o                                           50 'Centrale contr.satel...'
091600100406     o                       vtracen          +   2
091700070531     O          E            legenda     2
091800070531     o                                           50 'Allestimento ..........'
091900070531     o                       vtrafur          +   2
092000070531     o                       vtrafurd         +   2
092100070531     O          E            legenda     2
092200070531     o                                           50 'Antifurto blocca porte.'
092300070531     o                       vtrabpo          +   2
092400070531     o                       vtrabpod         +   2
092500070531     O          E            legenda     2
092600070531     o                                           50 'Antifurto blocco motore'
092700070531     o                       vtraant          +   2
092800070531     o                       vtraantd         +   2
092900070531      *----------------------------------------------------------
093000070531     O          E            testafil       1 02
093100100324     o                                           10 'TRMZA2R'
093200050802     o                       knmus               22
093300070606     o                                        +   1 'Lista Automezzi/Autisti -
093400070606     o                                              di città ELENCO  '
093500050802     o                       udate         y  +   3
093600050802     O                                        +   1 'Pag.'
093700050802     O                       page          z  +   1
093800050803     O          E            testafil    2  1
093900050803     o                                           16 'Societa'
094000050803     o                                           42 'Autista'
094100050803     o                                           66 'Mezzo'
094200050803     o                                           84 'Targa'
094300070605     o                                          102 'Data Disaccr.'
094400050802     O          E            detfil      1
094500051122     o                       TRASOC           +   9
094600050802     o                       TRAAUT           +   1
094700050802     o                       TRATIA           +   1
094800050802     o                       TRASCO           +   1
094900050802     o                       TRATAA           +   1
095000070605     o               99      TRAdfis            100 '  /  /    '
095100050803     O          E            totfil      1  1
095200050803     o                                              'Totale P.O '
095300050803     o                       exfil            +   1
095400051121     o                       orgdes           +   1
095500051122     o                       totmzfil      z  +   4
095600050803     O          E            detarea     1  1
095700050803     o                                              'Totale area'
095800050803     o                       excar            +   1
095900051121     o                       exaread20        +   1
096000051122     o                       totmzarea     z  +   4
096100050803     O          E            detdist     1
096200050803     o                                              'Totale distretto'
096300050803     o                       exfl3            +   1
096400051121     o                       exdistd20        +   1
096500051121     o                       totmzdis      z  +   1
096600051018     O          E            detaz       1
096700051122     o                                              'Totale azienda'
096800051122     o                       totmzaz       z     45
096900050802**  CMD1
097000100323CLRPFM    FILE(WAITRA1F)
097100050802**  CMD2
097200100323CRTDUPOBJ OBJ(WAITRA1F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
097300050802**  CMD3
097400100323DLTF FILE(QTEMP/WAITRA1F)
097500070608**  CMD4
097600160115DLTF   FILE(WRKQUERY/xxxxxxxxxx)
097700070608**  CMD5
097800100323CRTDUPOBJ OBJ(WAITRA1F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xxxxxxxxxx) DATA(*YES)
